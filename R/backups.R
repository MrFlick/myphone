
path_expand <- function(x) {
	m <- gregexpr("%[^%]+%", x)
	if (any(sapply(m, max)!=-1)) {
		vals <- regmatches(x,m)
		vars <- Map(function(x) substring(x, 2, nchar(x)-1), vals)
		expanded <- Map(Sys.getenv, vars)
		regmatches(x,m) <- expanded
	}
	path.expand(x)
}

list_backups <- function(dir = ifelse(.Platform$OS.type=="windows","%APPDATA%\\Apple Computer\\MobileSync\\Backup","~/Library/Application\ Support/MobileSync/Backup")) {
	list.dirs(path_expand(dir), recursive=FALSE)
}

get_backups <- function() {
	lapply(list_backups(), function(x) {
		z <- list(path=x);
		manifest_path <- file.path(x, "Manifest.db");
		if(file.exists(manifest_path)) {
			z$manifest = manifest_path
		}
		class(z) <- "ios_backup";
		z
	})
}

ios_hash <- function(x) {
	sapply(x, digest::digest, algo="sha1", serialize=FALSE)
}

get_backup_file_path <- function(backup, file, domain="") {
	if (!is.null(backup$manifest)) {
		# newer files have a manifest.db to find the file hashes
		con <- RSQLite::dbConnect(drv=RSQLite::SQLite(), dbname=backup$manifest)
		hashes <- apply(cbind(file, domain), 1, function(x) {
			f <- x[1]
			d <- x[2]
			sql <- "select fileID from Files where relativePath='%s'"
			if (nchar(d)>0) {
				sql <- sprintf(paste0(sql, " and domain='%s'"))
				sql <- sprintf(sql, f, d)
			} else {
				sql <- sprintf(sql, f)
			}
			dd <- RSQLite::dbGetQuery(conn=con, statement=sql)
			if (nrow(dd)<1) {
				NA
			} else if (nrow(dd)==1) {
				dd$fileID[1]
			} else {
				warning(sprintf("multiple files (%d) found for %s (%s)", nrow(dd), f, d))
				dd$fileID[1]
			}
		})
		RSQLite::dbConnect(con)
		# newer backups have folders for the first two characters of the hash
		file.path(backup$path, substr(hashes,1,2), hashes)
	} else {
		file <- paste0(ifelse(nchar(domain)>0, paste0(domain, "-"), ""), file)
		hashes <- ios_hash(file)
		file.path(backup$path, hashes)
	}
}

read_sms_data <- function(dir = list_backups()[1], db = "3d0d7e5fb2ce288813306e4d4636395e047a3d28") {
    con <- RSQLite::dbConnect(drv=RSQLite::SQLite(), dbname=file.path(dir, db))
    sql <- paste0("SELECT m.rowid as message_id, DATETIME(date + 978307200, 'unixepoch', 'localtime') as message_date, ", 
    	"h.id as contact, m.service as service, ",
    	"CASE is_from_me WHEN 0 THEN 'received' WHEN 1 THEN 'sent' ELSE 'unknown' END as type, ",
    	"CASE WHEN date_read > 0 THEN DATETIME(date_read + 978307200, 'unixepoch') WHEN date_delivered > 0 THEN DATETIME(date_delivered + 978307200, 'unixepoch') ELSE NULL END as 'read_deliver_date', ",
    	"text as text, ma.attachment_id, a.filename, ", 
    	"CASE a.is_outgoing WHEN 0 THEN 'incoming' WHEN 1 THEN 'outgoing' ELSE NULL END as direction, ",
    	"a.total_bytes, cm.chat_id as chat FROM message m ",
    	"INNER JOIN handle h ON h.rowid = m.handle_id ",
    	"LEFT OUTER JOIN message_attachment_join ma ON ma.message_id=m.rowid ",
    	"LEFT OUTER JOIN attachment a ON ma.attachment_id=a.rowid ",
    	"LEFT OUTER JOIN chat_message_join cm ON cm.message_id = m.rowid ",
    	"ORDER BY m.rowid ASC;")
    dd<-RSQLite::dbGetQuery(conn=con, statement=sql)
    dd$message_date <- as.POSIXct(dd$message_date)
    dd$read_deliver_date <- as.POSIXct(dd$read_deliver_date)
    dplyr::tbl_df(dd)
}

get_sms_attachment_path <- function(dir = list_backups()[1], filename) {
	file.path(dir, sapply(gsub("~/","MediaDomain-", filename), digest::digest, algo="sha1", serialize=FALSE))
}

get_emoji <- function(x) {
	emoji <- gregexpr("[\U0001F300-\U0001F64F]", x)
	regmatches(x, emoji)
}

sanitize_phone_number <- function(x) {
	pn<-gsub("\\D","", x)
	pn[nchar(pn)==10] <- paste0("1",pn[nchar(pn)==10])
	pn[nchar(pn)==11] <- paste0("+",pn[nchar(pn)==11])
	pn
}

read_contacts <- function(dir = list_backups()[1], db = "31bb7ba8914766d4ba40d6dfb6113c8b614be442") {
    con <- RSQLite::dbConnect(drv=RSQLite::SQLite(), dbname=file.path(dir, db))
    sql <- paste0("select ABPerson.ROWID as contact_id, ABPerson.first,ABPerson.last, ",
    	"v.value as email_phone, ",
    	"CASE v.property WHEN 3 THEN 'phone' WHEN 4 THEN 'email' END as type ",
		"from ABPerson left outer join ABMultiValue c on c.record_id = ABPerson.ROWID and c.label = 1 and c.property= 3 ",
		"left outer join ABMultiValue v on v.record_id = ABPerson.ROWID ",
		"where v.property in (3,4)")
    dd <- RSQLite::dbGetQuery(conn=con, statement=sql)
    dd$contact <- dd$email_phone;
    dd$contact[dd$type=="phone"] <- sanitize_phone_number(dd$contact[dd$type=="phone"])
    dplyr::tbl_df(dd)
}

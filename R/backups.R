
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
	lapply(list_backups(), get_backup)
}

is_directory <- function(x) {
	x <- as.character(x)
	r <- file.exists(x)
	r[r] <- file.info(x[r])$isdir
	r
}

sort_mtime <- function(x) {
	fi <- file.info(x)
	x[order(fi$mtime, decreasing=TRUE)]
}

get_backup <- function(x=1) {
	if (is.factor(x)) {x <- as.character(x)}
	if ("ios_backup" %in% class(x)) {
		return(x)
	}
	stopifnot(length(x)==1)
	if (is.character(x) && is_directory(x)) {
		path <- x
	} else {
		paths <- sort_mtime(list_backups())
		bu_names <- basename(paths)
		if (is.numeric(x)) {
			path <- paths[x]
		} else if (x %in% bu_names) {
			path <- paths[x==bu_names]
		} else {
			matches <- grep(x, bu_names, value=TRUE)
			if (length(matches)==1) {
				path <- matches
			} else {
				stop(paste("multiple matches for", x, ":", 
					paste(matches, collapse=",")))
			}
		} 
	}
	z <- list(path=path, name=basename(path))
	manifest_path <- file.path(path, "Manifest.db");
	if(file.exists(manifest_path)) {
		z$manifest = manifest_path
	}
	class(z) <- "ios_backup";
	z
}

ios_hash <- function(x) {
	sapply(x, digest::digest, algo="sha1", serialize=FALSE)
}

manifest_contents <- function(backup, table="Files") {
	backup <- get_backup(backup)
	if (!"manifest" %in% names(backup)) {
		stop("cannot find manifest contents")
	}
	db <- dplyr::src_sqlite(backup$manifest)
	dplyr::tbl(db, table)
}

get_backup_file_path <- function(backup, file, domain="") {
	backup <- get_backup(backup)
	if (!is.null(backup$manifest)) {
		# newer files have a manifest.db to find the file hashes
		manifest <- manifest_contents(backup)
		hashes <- apply(cbind(file, domain), 1, function(x) {
			f <- unname(x[1])
			d <- unname(x[2])
			where <- list(~relativePath==f)
			if (nchar(d)>0) {
				where <- c(where, ~domain == d)
			}
			dd <- dplyr::collect(dplyr::filter_(manifest, .dots=where))
			if (nrow(dd)<1) {
				NA
			} else if (nrow(dd)==1) {
				dd$fileID[1]
			} else {
				warning(sprintf("multiple files (%d) found for %s (%s)", nrow(dd), f, d))
				dd$fileID[1]
			}
		})
		# newer backups have folders for the first two characters of the hash
		file.path(backup$path, substr(hashes,1,2), hashes)
	} else {
		file <- paste0(ifelse(nchar(domain)>0, paste0(domain, "-"), ""), file)
		hashes <- ios_hash(file)
		file.path(backup$path, hashes)
	}
}

read_sms_data <- function(x, collect=TRUE) {
	if (is.character(x) && length(x)==1 && file.exists(x)) {
		path <- x
	} else {
		path <- get_backup_file_path(x, file="Library/SMS/sms.db", domain="HomeDomain")
	}
    sql <- paste0("SELECT m.rowid as 'message_id', ",
		"DATETIME(date + 978307200, 'unixepoch', 'localtime') as 'message_date', ", 
    	"h.id as 'contact', m.service as 'service', ",
    	"CASE is_from_me WHEN 0 THEN 'received' WHEN 1 THEN 'sent' ELSE 'unknown' END as 'type', ",
    	"CASE WHEN date_read > 0 THEN DATETIME(date_read + 978307200, 'unixepoch') WHEN date_delivered > 0 THEN DATETIME(date_delivered + 978307200, 'unixepoch') ELSE NULL END as 'read_deliver_date', ",
    	"text as 'text', ma.attachment_id, a.filename, ", 
    	"CASE a.is_outgoing WHEN 0 THEN 'incoming' WHEN 1 THEN 'outgoing' ELSE NULL END as 'direction', ",
    	"a.total_bytes, cm.chat_id as 'chat' FROM message m ",
    	"INNER JOIN handle h ON h.rowid = m.handle_id ",
    	"LEFT OUTER JOIN message_attachment_join ma ON ma.message_id=m.rowid ",
    	"LEFT OUTER JOIN attachment a ON ma.attachment_id=a.rowid ",
    	"LEFT OUTER JOIN chat_message_join cm ON cm.message_id = m.rowid ",
    	"ORDER BY m.rowid ASC")
    dd <- dplyr::tbl(dplyr::src_sqlite(path), dplyr::sql(sql))
	if (collect) {
    	dd <- dplyr::mutate(dplyr::collect(dd), message_date = as.POSIXct(message_date),
		read_deliver_date = as.POSIXct(read_deliver_date))
	}
	dd
}

get_sms_attachment_path <- function(dir = list_backups()[1], filename) {
	file.path(dir, sapply(gsub("~/","MediaDomain-", filename), ios_hash))
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

read_contacts <- function(x, collect=TRUE) {
	if (is.character(x) && length(x)==1 && file.exists(x)) {
		path <- x
	} else {
		path <- get_backup_file_path(x, file="Library/AddressBook/AddressBook.sqlitedb", domain="HomeDomain")
	}
    sql <- paste0("select ABPerson.ROWID as contact_id, ABPerson.first,ABPerson.last, ",
    	"v.value as email_phone, ",
    	"CASE v.property WHEN 3 THEN 'phone' WHEN 4 THEN 'email' END as type ",
		"from ABPerson left outer join ABMultiValue c on c.record_id = ABPerson.ROWID and c.label = 1 and c.property= 3 ",
		"left outer join ABMultiValue v on v.record_id = ABPerson.ROWID ",
		"where v.property in (3,4)")
    dd <- dplyr::tbl(dplyr::src_sqlite(path), dplyr::sql(sql))
	if (collect) {
		dd <- dplyr::mutate(dplyr::collect(dd), contact = email_phone)
		dd$contact[dd$type=="phone"] <- sanitize_phone_number(dd$contact[dd$type=="phone"])
	}
	dd
}

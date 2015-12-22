

list_backups <- function(dir = "~/Library/Application\ Support/MobileSync/Backup") {
	list.dirs(dir, recursive=FALSE)
}

read_sms_data <- function(dir = list_backups()[1], db = "3d0d7e5fb2ce288813306e4d4636395e047a3d28") {
    con <- RSQLite::dbConnect(drv=RSQLite::SQLite(), dbname=file.path(dir, db))
    sql <- paste0("SELECT m.rowid as RowID, DATETIME(date + 978307200, 'unixepoch', 'localtime') as Date, ", 
    	"h.id as 'Phone Number', m.service as Service, ",
    	"CASE is_from_me WHEN 0 THEN 'Received' WHEN 1 THEN 'Sent' ELSE 'Unknown' END as Type, ",
    	"CASE WHEN date_read > 0 THEN DATETIME(date_read + 978307200, 'unixepoch') WHEN date_delivered > 0 THEN DATETIME(date_delivered + 978307200, 'unixepoch') ELSE NULL END as 'Date Read/Sent', ",
    	"text as Text, ma.attachment_id, a.filename, ", 
    	"CASE a.is_outgoing WHEN 0 THEN 'Incoming' WHEN 1 THEN 'Outgoing' ELSE NULL END as AType, ",
    	"a.total_bytes FROM message m ",
    	"INNER JOIN handle h ON h.rowid = m.handle_id ",
    	"LEFT OUTER JOIN message_attachment_join ma ON ma.message_id=m.rowid ",
    	"LEFT OUTER JOIN attachment a ON ma.attachment_id=a.rowid ",
    	"ORDER BY m.rowid ASC;")
    RSQLite::dbGetQuery(conn=con, statement=sql)
}

get_sms_attachment_path <- function(dir = list_backups()[1], filename) {
	file.path(dir, sapply(gsub("~/","MediaDomain-", filename), digest::digest, algo="sha1", serialize=FALSE))
}

read_contacts <- function(dir = list_backups()[1], db = "31bb7ba8914766d4ba40d6dfb6113c8b614be442") {
    con <- RSQLite::dbConnect(drv=RSQLite::SQLite(), dbname=file.path(dir, db))
    sql <- paste0("select ABPerson.prefix, ABPerson.first,ABPerson.last, c.value as MobilePhone, ",
    	"h.value as HomePhone, he.value as HomeEmail, w.value as WorkPhone, we.value as WorkEmail ",
		"from ABPerson left outer join ABMultiValue c on c.record_id = ABPerson.ROWID and c.label = 1 and c.property= 3 ",
		"left outer join ABMultiValue h on h.record_id = ABPerson.ROWID and h.label = 2 and h.property = 3 ",
		"left outer join ABMultiValue he on he.record_id = ABPerson.ROWID and he.label = 2 and he.property = 4 ",
		"left outer join ABMultiValue w on w.record_id = ABPerson.ROWID and w.label = 4 and w.property = 3 ",
		"left outer join ABMultiValue we on we.record_id = ABPerson.ROWID and we.label = 4 and we.property = 4")
    RSQLite::dbGetQuery(conn=con, statement=sql)
}

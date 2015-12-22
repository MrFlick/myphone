
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
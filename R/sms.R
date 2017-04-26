

read_sms_data <- function(x, collect=TRUE) {
	if (is.character(x) && length(x)==1 && file.exists(x)) {
		path <- x
	} else {
		path <- backup_file_path(x, file="Library/SMS/sms.db", domain="HomeDomain")
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

sms_attachment_path <- function(dir = list_backups()[1], filename) {
	file.path(dir, sapply(gsub("~/","MediaDomain-", filename), ios_hash))
}

grep_emoji <- function(x) {
	emoji <- gregexpr("[\U0001F300-\U0001F64F]", x)
	regmatches(x, emoji)
}

sanitize_phone_number <- function(x) {
	pn <- gsub("\\D","", x)
	pn[nchar(pn)==10] <- paste0("1",pn[nchar(pn)==10])
	pn[nchar(pn)==11] <- paste0("+",pn[nchar(pn)==11])
	pn
}

#' SMS data path
#'
#' Returns the full path to the SMS database
#'
#' @param x An \code{ios_backup} object (or something that can be passed
#'   to \code{get_backup})
#' @return The full path to the SMS database file
#' @export

path_sms <- function(x) {
	if (is.character(x) && length(x)==1 && file.exists(x)) {
		path <- x
	} else {
		path <- backup_file_path(x, file="Library/SMS/sms.db", domain="HomeDomain")
	}
}

#' Read SMS Data
#'
#' Return SMS messages stored in backup
#'
#' @param x An \code{ios_backup} object (or something that can be passed
#' to \code{get_backup}) or the path to a sqlite SMS database.
#' @param collect Should dplyr results be collected before being returned.
#' @return A tibble containing SMS data
#' @export
read_sms_data <- function(x, collect=TRUE) {
	path <- path_sms(x)
    sql <- paste0("SELECT m.rowid as 'message_id', ",
		"DATETIME(date + 978307200, 'unixepoch', 'localtime') as 'message_date', ",
    	"h.id as 'contact', m.service as 'service', ",
    	"CASE is_from_me WHEN 0 THEN 'received' WHEN 1 THEN 'sent' ELSE 'unknown' END as 'type', ",
    	"CASE WHEN date_read > 0 THEN DATETIME(date_read + 978307200, 'unixepoch') WHEN date_delivered > 0 THEN DATETIME(date_delivered + 978307200, 'unixepoch') ELSE NULL END as 'read_deliver_date', ",
    	"text as 'text', ma.attachment_id, a.filename as `attachment_filename`, ",
    	"CASE a.is_outgoing WHEN 0 THEN 'incoming' WHEN 1 THEN 'outgoing' ELSE NULL END as 'direction', ",
    	"a.total_bytes, cm.chat_id as 'chat' FROM message m ",
    	"INNER JOIN handle h ON h.rowid = m.handle_id ",
    	"LEFT OUTER JOIN message_attachment_join ma ON ma.message_id=m.rowid ",
    	"LEFT OUTER JOIN attachment a ON ma.attachment_id=a.rowid ",
    	"LEFT OUTER JOIN chat_message_join cm ON cm.message_id = m.rowid ",
    	"ORDER BY m.rowid ASC")
    dd <- dplyr::tbl(dplyr::src_sqlite(path), dplyr::sql(sql))
	if (collect) {
    	dd <- dplyr::mutate_(dplyr::collect(dd), .dots=list(message_date = ~as.POSIXct(message_date),
		read_deliver_date = ~as.POSIXct(read_deliver_date)))
	}
	dd
}


#' Find SMS attachment path
#'
#' Return the file path to an SMS attachment given the relative path
#'  from the SMS database
#' @param backup An \code{ios_backup} object 
#' @param attachment_filename The attachment filename from the SMS database
#' @return The full path to the attachment file in the backup
#' @export

sms_attachment_path <- function(backup, attachment_filename) {
	backup <- get_backup(backup)
	filename <- gsub("^(~/|/var/mobile/)", "", attachment_filename)
	backup_file_path(backup, filename, "MediaDomain")
}

#' Find emojis in text
#'
#' Extract emojis from text

#' @param x A character vector 
#' @return A list the same length as \code{x} with a vector
#'  of emoji-like symbols found in the text
#' @export

grep_emoji <- function(x) {
	emoji <- gregexpr("[\U0001F300-\U0001F64F]", as.character(x))
	regmatches(x, emoji)
}

sanitize_phone_number <- function(x) {
	pn <- gsub("\\D","", x)
	pn[nchar(pn)==10] <- paste0("1",pn[nchar(pn)==10])
	pn[nchar(pn)==11] <- paste0("+",pn[nchar(pn)==11])
	pn
}

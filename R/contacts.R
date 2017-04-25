#' Read contacts database
#'
#' Extract data from the Address Book in an iOS Backup.
#'
#' @param x An \code{ios_backup} object (oe somethin that can be passed
#' to \code{get_backup}) or the path to a sqlite AddressBook database.
#' @param collect Should dplyr results be collected before being returned.
#' @return This will return a tibble with the contact data.
#'   If \code{collect==FALSE}, it will be a lazy tibble.
#'   The following columns will be included
#' \itemize{
#' \item{contact_id} A unique ID for each contact
#' \item{first} Contact first name
#' \item{last} Contact last name
#' \item{emain_phone} Contact e-mail or phone number
#'}
#' @export

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

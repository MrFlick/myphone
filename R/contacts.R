#' Address book path
#'
#' Returns the full path to the Address Book database
#'
#' @param x An \code{ios_backup} object (or something that can be passed
#'   to \code{get_backup})
#' @return The full path to the address book database file
#'@examples
#'\dontrun{
#' backup <- get_backup()
#' path_contacts(backup)
#'}
#' @export

path_contacts <- function(x) {
	if (is.character(x) && length(x)==1 && file.exists(x)) {
		path <- x
	} else {
		path <- backup_file_path(x, file="Library/AddressBook/AddressBook.sqlitedb", domain="HomeDomain")
	}
}

#' Read contacts database
#'
#' Extract data from the Address Book in an iOS Backup.
#'
#' @param x An \code{ios_backup} object (or something that can be passed
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
#'@examples
#'\dontrun{
#' backup <- get_backup()
#' read_contacts(backup)
#'
#' #Read a copy
#' orig <- path_contacts(backup)
#' mycopy <- file.path(getwd(), "contacts.sqlitedb")
#' file.copt(orig, mycopy)
#' read_contacts(mycopy)
#'
#' #Join with SMS
#' read_sms_data(backup) %>% left_join(read_contacts(backup))
#'}
#' @export

read_contacts <- function(x, collect=TRUE) {
	path <- path_contacts(x)
    sql <- paste0("select ABPerson.ROWID as contact_id, ABPerson.first,ABPerson.last, ",
    	"v.value as email_phone, ",
    	"CASE v.property WHEN 3 THEN 'phone' WHEN 4 THEN 'email' END as type ",
		"from ABPerson left outer join ABMultiValue c on c.record_id = ABPerson.ROWID and c.label = 1 and c.property= 3 ",
		"left outer join ABMultiValue v on v.record_id = ABPerson.ROWID ",
		"where v.property in (3,4)")
    dd <- dplyr::tbl(dplyr::src_sqlite(path), dplyr::sql(sql))
	if (collect) {
		dd <- dplyr::mutate_(dplyr::collect(dd), .dots=list(contact = ~email_phone))
		dd$contact[dd$type=="phone"] <- sanitize_phone_number(dd$contact[dd$type=="phone"])
	}
	dd
}

#' Read contacts
#'
#' Extract contacts from the Address Book in an iOS Backup.
#'
#' @param x An \code{ios_backup} object (or something that can be passed
#'   to \code{get_backup}) or the path to a sqlite AddressBook database.
#' @param collect Should dplyr results be collected before being returned.
#' @return This will return a tibble with the contact data.
#'   If \code{collect==FALSE}, it will be a lazy tibble.
#'   The following columns will be included
#' \itemize{
#' \item{contact_id} A unique ID for each contact
#' \item{first_name} Contact first name
#' \item{middle_name} Contact middle name
#' \item{last_name} Contact last name
#' \item{organization} Contact organization
#' \item{department} Contact department
#' \item{jobtitle} Contact job title
#' \item{birthday} Contact birthday
#' \item{creation_date} Date contact created
#'}
#' @seealso \code{\link{read_contacts_phone_numbers}}, \code{\link{read_contacts_email_addresses}}
#' @export

read_contact_entities <- function(x, collect=TRUE) {
	path <- path_contacts(x)
	sql = paste0("select ABPerson.ROWID as contact_id, ",
    	"ABPerson.first as first_name, ABPerson.last as last_name, ABPerson.middle as middle_name, ",
     	"ABPerson.Organization as organization, ABPerson.Department as department, ",
		"ABPerson.JobTitle as jobtitle, ",
		"DATETIME(cast(ABPerson.Birthday as decimal) + 978307200, 'unixepoch', 'localtime') as birthday, ",
		"DATETIME(ABPerson.CreationDate + 978307200, 'unixepoch', 'localtime') as creation_date ",
		"FROM ABPerson")
	dd <- dplyr::tbl(dplyr::src_sqlite(path), dplyr::sql(sql))
	if (collect) {
		dd <- dplyr::mutate_(dplyr::collect(dd), .dots=list(
			creation_date = ~as.POSIXct(creation_date),
			birthday = ~as.POSIXct(birthday)
		))
	}
	dd
}

#' Read contact phone numbers
#'
#' Extract phone numbers from the Address Book in an iOS Backup.
#'
#' @param x An \code{ios_backup} object (or something that can be passed
#' to \code{get_backup}) or the path to a sqlite AddressBook database.
#' @param collect Should dplyr results be collected before being returned.
#' @return This will return a tibble with the contact data.
#'   If \code{collect==FALSE}, it will be a lazy tibble.
#'   The following columns will be included
#' \itemize{
#' \item{contact_id} A unique ID for each contact
#' \item{identifier} A unique number assigned to each address 
#'    for each contact (0 indicates "primary" address)
#' \item{phone} Contact phone number
#' \item{phone_type} Phone number label
#'}
#' @seealso \code{\link{read_contact_entities}}, \code{\link{read_contacts_email_addresses}}
#' @export

read_contacts_phone_numbers <- function(x, collect=TRUE) {
	path <- path_contacts(x)
	sql <- paste0("select mv.record_id as contact_id, ",
		"mv.identifier as identifier ",
		"mv.value as phone, ",
		"mvl.value as phone_type ",
		"from ABMultiValue as mv ",
		"left join ABMultiValueLabel as mvl on mvl.ROWID = mv.label ",
		"where mv.property=3 ",
		"order by mv.record_id, mv.identifier")
	dd <- dplyr::tbl(dplyr::src_sqlite(path), dplyr::sql(sql))
	if (collect) {
		dd <- dplyr::mutate_(dplyr::collect(dd), .dots=list(
			phone_type = ~gsub("_\\$!<(.*)>!\\$_", "\\1", phone_type)
		))
	}
	dd
}

#' Read contact email addresses
#'
#' Extract email addresses from the Address Book in an iOS Backup.
#'
#' @param x An \code{ios_backup} object (or something that can be passed
#' to \code{get_backup}) or the path to a sqlite AddressBook database.
#' @param collect Should dplyr results be collected before being returned.
#' @return This will return a tibble with the contact data.
#'   If \code{collect==FALSE}, it will be a lazy tibble.
#'   The following columns will be included
#' \itemize{
#' \item{contact_id} A unique ID for each contact
#' \item{identifier} A unique number assigned to each address 
#'    for each contact (0 indicates "primary" address)
#' \item{email} Contact email address
#' \item{email_type} Email address label
#'}
#' @seealso \code{\link{read_contact_entities}}, \code{\link{read_contacts_phone_numbers}}
#' @export

read_contacts_email_addresses <- function(x, collect=TRUE) {
	path <- path_contacts(x)
	sql <- paste0("select mv.record_id as contact_id, ",
		"mv.identifier as identifier ",
		"mv.value as email, ",
		"mvl.value as email_type ",
		"from ABMultiValue as mv ",
		"left join ABMultiValueLabel as mvl on mvl.ROWID = mv.label ",
		"where mv.property=4 ",
		"order by mv.record_id, mv.identifier")
	dd <- dplyr::tbl(dplyr::src_sqlite(path), dplyr::sql(sql))
	if (collect) {
		dd <- dplyr::mutate_(dplyr::collect(dd), .dots=list(
			email_type = ~gsub("_\\$!<(.*)>!\\$_", "\\1", email_type)
		))
	}
	dd
}


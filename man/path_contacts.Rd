% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contacts.R
\name{path_contacts}
\alias{path_contacts}
\title{Address book path}
\usage{
path_contacts(x)
}
\arguments{
\item{x}{An \code{ios_backup} object (or something that can be passed
to \code{get_backup})}
}
\value{
The full path to the address book database file
}
\description{
Returns the full path to the Address Book database
}
\examples{
\dontrun{
backup <- get_backup()
path_contacts(backup)
}
}

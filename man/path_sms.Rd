% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sms.R
\name{path_sms}
\alias{path_sms}
\title{SMS data path}
\usage{
path_sms(x)
}
\arguments{
\item{x}{An \code{ios_backup} object (or something that can be passed
to \code{get_backup})}
}
\value{
The full path to the SMS database file
}
\description{
Returns the full path to the SMS database
}
\examples{
\dontrun{
backup <- get_backup()
path_sms(backup)
}
}

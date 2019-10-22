
path_expand <- function(x) {
	m <- gregexpr("%[^%]+%", x)
	if (any(vapply(m, max, numeric(1))!=-1)) {
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

is_directory <- function(x) {
	x <- as.character(x)
	r <- file.exists(x)
	r[r] <- file.info(x[r])$isdir
	r
}

ios_hash <- function(x) {
	vapply(x, digest::digest, character(1), algo="sha1", serialize=FALSE)
}

sort_mtime <- function(x) {
	fi <- file.info(x)
	x[order(fi$mtime, decreasing=TRUE)]
}

#' Get iOS Backup
#'
#' Find iOS backup path
#'
#' @param x If unspecified, returns the newest back-up. You can also
#'   specify a full path to a backup or a partial name that will be
#'   matched against known backup names
#' @return An object of class \code{ios_backup}
#'@examples
#'\dontrun{
#' backup <- get_backup()
#'}	
#' @export

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
			} else if (length(matches)==0) {
				stop(paste("no matches for", x))				
			} else {
				stop(paste("multiple matches for", x, ":",
					paste(matches, collapse=",")))
			}
		}
	}
	z <- list(path=path, name=basename(path))
	manifest_path <- file.path(path, "Manifest.db")
	if(file.exists(manifest_path)) {
		z$manifest <- manifest_path
	}
	class(z) <- "ios_backup"
	z
}

#' Get iOS Backups
#'
#' Find all iOS backup paths
#'
#' @param ... Parameters passed to \code{list_backups}
#' @return A list of all backups in backup folder
#'@examples
#'\dontrun{
#' backups <- get_backups()
#' if (length(backups)) {
#'   Map(function(x) {x$path}, backups)
#' }
#'}
#' @export

get_backups <- function(...) {
	lapply(list_backups(...), get_backup)
}

#' Return manifest contents
#'
#' List all files in a backup from the manifest
#'
#' A manifest file is a database that tracks all of the files in a backup.
#' The fileID has traditioanally been a hash of the combination of the
#' domain and the relative bath but by looking it up in the manifest, we
#' can be sure to extract the correct file.
#'
#' @param backup An \code{ios_backup} object (or something that can be passed
#' to \code{get_backup}).
#' @param table Which table should be returned from the manifest database.
#' @param collect Should dplyr results be collected before being returned.
#' @return This will return a tibble with the contact data.
#'   If \code{collect==FALSE}, it will be a lazy tibble.
#'   The following columns will be included
#' \itemize{
#' \item{fileID} A unique ID for each file in the backup
#' \item{domain} The domain where the file is used
#' \item{flags} Flags set on the file
#'}
#'@examples
#'\dontrun{
#' backup <- get_backup()
#' files <- manifest_contents(backup)
#' nrow(files)
#'}
#' @export
manifest_contents <- function(backup, table="Files", collect=TRUE) {
	backup <- get_backup(backup)
	if (!"manifest" %in% names(backup)) {
		stop("manifest database not found in backup")
	}
	db <- dplyr::src_sqlite(backup$manifest)
	dd <- dplyr::tbl(db, table)
	if (collect) {
		dd <- dplyr::collect(dd)
	}
	dd
}

#' Find file in backup
#'
#' Return the path for a specific file in the backup
#'
#' This function translates the human-readable relative file
#' paths and domains into the fileID necessary to extract the file.
#' If present, the manifest data for the backup is used; if not
#' present, the path and domain are hashed as in older iOS versions.

#' @param backup An \code{ios_backup} object (or something that can be passed
#' to \code{get_backup}).
#' @param file The relative name of the file you want to find.
#' @param domain The domain in which to search for the file.
#' @return For each input, will return a character value with the
#'  true path to the file on disk or NA if the location cannot be
#'  determined (no entry in the manifest)
#'@examples
#'\dontrun{
#' backup <- get_backup()
#' backup_file_path(backup, file="Library/AddressBook/AddressBook.sqlitedb", 
#'   domain="HomeDomain")
#'}
#' @importFrom rlang .data
#' @export

backup_file_path <- function(backup, file, domain="") {
	backup <- get_backup(backup)
	if (!is.null(backup$manifest)) {
		# newer files have a manifest.db to find the file hashes
		manifest <- manifest_contents(backup)
		hashes <- apply(cbind(file, domain), 1, function(x) {
			f <- unname(x[1])
			d <- unname(x[2])
			if (nchar(d)>0) {
				where <- rlang::quos(.data$relativePath==f, .data$domain == d)
			} else {
			  where <- rlang::quos(.data$relativePath==f)
			}
			dd <- dplyr::collect(dplyr::filter(manifest, !!!where))
			if (nrow(dd)<1) {
				NA
			} else if (nrow(dd)==1) {
				dd$fileID[1]
			} else {
				warning(sprintf("multiple files (%d) found for %s (%s)", 
					nrow(dd), f, d))
				dd$fileID[1]
			}
		})
		# newer backups have folders for the first two characters of the hash
		paths <- hashes
		paths[!is.na(paths)] <- file.path(backup$path, 
			substr(paths[!is.na(paths)],1,2), paths[!is.na(paths)])
		paths
	} else {
		file <- paste0(ifelse(nchar(domain)>0, paste0(domain, "-"), ""), file)
		hashes <- ios_hash(file)
		file.path(backup$path, hashes)
	}
}

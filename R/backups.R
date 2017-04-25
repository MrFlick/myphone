
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

is_directory <- function(x) {
	x <- as.character(x)
	r <- file.exists(x)
	r[r] <- file.info(x[r])$isdir
	r
}

ios_hash <- function(x) {
	sapply(x, digest::digest, algo="sha1", serialize=FALSE)
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

get_backups <- function() {
	lapply(list_backups(), get_backup)
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

#' Decode binary plist data
#'
#' Decode binary plist data
#'
#' Some of the databases in the iOS backups contain columns with
#' binary data that appear to be binary plist objects. This function
#' can be used to decode the raw data into an proper R object.
#'
#' @param x A raw vector
#' @return The data stored in the plist. Dictionaries are converted
#'   to named lists, arrays are converted to lists, and other objects
#'   are returned as their equivalent atomic types
#' @examples
#' example <- system.file("extdata", "example.bplist", package = "myphone")
#' bytes <- readBin(example, "raw", n=file.size(example))
#' decode_binary_plist(bytes)

#' @references \url{https://en.wikipedia.org/wiki/Property_list}
#' @references \url{http://fileformats.archiveteam.org/wiki/Property_List/Binary}
#' @references \url{https://github.com/google/binplist/}
#' @export

decode_binary_plist <- function(x) {
	stopifnot(length(x)>=8)
	rcon <- rawConnection(x)
	on.exit(close(rcon))
	pltype <- rawToChar(readBin(rcon, raw(), 8))
	stopifnot(pltype %in% c("bplist00","bplist01"))
	tinfo <- read_plist_trailer(rcon)
	toffset <- read_plist_offset_table(rcon, tinfo)
	vv <- lapply(toffset, function(i) {
		x <- read_plist_object(rcon, i, tinfo)
		x})
	vv
	expand_object_from_values(vv, tinfo$root_number)
}

expand_object_size <- function(size, rcon) {
	if (size==15) {
		size_byte_size <- readBin(rcon, integer(), 1, 1, signed=FALSE)
		size <- readBin(rcon, integer(), 1, 2^bitwAnd(size_byte_size, 15),
			signed=FALSE, endian="big")
	}
	size
}

read_plist_object <- function(rcon, offset, tinfo) {
	if(!is.na(offset)) seek(rcon, offset, "start")
	type_size <- readBin(rcon, integer(), 1, 1, signed=FALSE)
	type <- bitwAnd(bitwShiftR(type_size,4), 15)
	size <- bitwAnd(type_size, 15)
	object <- if(type==0) {
		read_plist_singleton(rcon, size, tinfo)
	} else if (type==1) {
		read_plist_integer(rcon, size, tinfo)
	} else if (type==2) {
		read_plist_float(rcon, size, tinfo)
	} else if (type==3) {
		read_plist_date(rcon, size, tinfo)
	} else if (type==4) {
		read_plist_binary(rcon, size, tinfo)
	} else if (type==5) {
		read_plist_sbstring(rcon, size, tinfo)
	} else if (type==6) {
		read_plist_dbstring(rcon, size, tinfo)
	} else if (type==8) {
		read_plist_uid(rcon, size, tinfo)
	} else if (type==10) {
		read_plist_array(rcon, size, tinfo)
	} else if (type==13) {
		read_plist_dictionary(rcon, size, tinfo)
	} else {
		stop(paste("Unrecognied type", type))
	}
	object
}

read_plist_singleton <- function(rcon, size, tinfo) {
	value <- if(size==0) {
		"NULL"
	} else if (size==8) {
		"FALSE"
	} else if(size==9) {
		"TRUE"
	} else if (size==15) {
		"FILL"
	} else {
		stop("invalid singleton value")
	}
	return(value)
}

read_plist_integer <- function(rcon, size, tinfo) {
	value <- readBin(rcon, integer(), 1, 2^size, signed=TRUE, endian="big")
	return(value)
}

read_plist_float <- function(rcon, size, tinfo) {
	value <- readBin(rcon, numeric(), 1, 2^size, signed=TRUE, endian="big")
	return(value)
}

read_plist_date <- function(rcon, size, tinfo) {
	stopifnot(size==3)
	value <- readBin(rcon, double(), 1, 8, signed=TRUE, endian="big")
	value <- as.POSIXct(value, origin="2001-01-01 0:00:00 GMT")
	return(value)
}

read_plist_binary <- function(rcon, size, tinfo) {
	size <- expand_object_size(size, rcon)
	value <- readBin(rcon, raw(), size)
	return(value)
}

read_plist_sbstring <- function(rcon, size, tinfo) {
	size <- expand_object_size(size, rcon)
	value <- rawToChar(readBin(rcon, raw(), size))
	return(value)
}

read_plist_dbstring <- function(rcon, size, tinfo) {
	size <- expand_object_size(size, rcon)
	size <- size *2
	value <- iconv(list(readBin(rcon, raw(), size)), "UTF-16")
	return(value)
}

read_plist_uid <- function(rcon, size, tinfo) {
	value <- readBin(rcon, integer(), 1, 2^size, 
		signed=FALSE, endian="big")
	return(value)
}

read_plist_array <- function(rcon, size, tinfo) {
	size <- expand_object_size(size, rcon)
	values <- readBin(rcon, integer(), size, tinfo$reference_size,
		signed=FALSE, endian="big")
	return(list(values=values))
}

read_plist_dictionary <- function(rcon, size, tinfo) {
	size <- expand_object_size(size, rcon)
	keys <- readBin(rcon, integer(), size, tinfo$reference_size,
		signed=FALSE, endian="big")
	values <- readBin(rcon, integer(), size, tinfo$reference_size,
		signed=FALSE, endian="big")
	return(list(keys=keys, values=values))
}

expand_object_from_values <- function(values, root=0) {
	expanded_values <- vector("list", length(values))
	get_expanded_value <- function(i) {
		ev <- expanded_values[[i]]
		if(!is.null(ev)) {
			return(ev)
		}
		rv <- values[[i]]
		if(is.list(rv) && "values" %in% names(rv)) {
			idxs <- rv$values + 1
			ev <- lapply(idxs, get_expanded_value)
			if ("keys" %in% names(rv)) {
				idxs <- rv$keys + 1
				kv <- sapply(idxs, get_expanded_value)
				names(ev) <- kv
			}
			expanded_values[[i]] <<- ev
			return(ev)
		} else {
			expanded_values[[i]] <<- rv
			return(rv)
		}
	}
	get_expanded_value(root+1)
}

read_plist_offset_table <- function(rcon, tinfo) {
	seek(rcon, tinfo$table_offset, "start")
	size <- tinfo$offset_size
	count <- tinfo$object_count
	value <- readBin(rcon,integer(), count, size, signed=FALSE, endian="big")
	return(value)
}

read_plist_trailer <- function(rcon, auto_seek=TRUE) {
	if (auto_seek) seek(rcon, -32, "end")
	readBin(rcon, raw(), 6, 1)
	offset_size <- readBin(rcon, integer(), 1, 1, signed=FALSE)
	reference_size <- readBin(rcon, integer(), 1, 1, signed=FALSE)
	readBin(rcon, raw(), 4, 1)
	object_count <- readBin(rcon, integer(), 1, 4, signed=TRUE, endian="big")
	readBin(rcon, raw(), 4, 1)
	root_number <- readBin(rcon, integer(), 1, 4, signed=TRUE, endian="big")
	readBin(rcon, raw(), 4, 1)
	table_offset <- readBin(rcon, integer(), 1, 4, signed=TRUE, endian="big")
	list(offset_size=offset_size,
		reference_size=reference_size,
		object_count=object_count,
		root_number=root_number,
		table_offset=table_offset)
}

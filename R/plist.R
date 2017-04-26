decode_plist <- function(x) {
	stopifnot(length(x)>=8)
	rcon <- rawConnection(x)
	pltype <- rawToChar(readBin(rcon, raw(), 8))
	stopifnot(pltype %in% c("bplist00","bplist01"))
	seek(rcon, -32, "end")
	tinfo <- read_plist_trailer(rcon)
	seek(rcon, 8, "start")
	while(seek(rcon,NA) < tinfo$table_offset)
	vv <- lapply(1:tinfo$object_count, function(i) {
		x<-read_plist_object(rcon, tinfo)
		print(paste(i, "------"))
		print(x)
		x})
	close(rcon)
	vv
}

read_plist_object <- function(rcon, tinfo) {
	type_size <- readBin(rcon, integer(), 1, 1, signed=FALSE)
	type <- bitwAnd(bitwShiftR(type_size,4), 15)
	print(type)
	size <- bitwAnd(type_size, 15)
	#this isn't quite right yet
	if (size==15 & type!=0) {
		size <- readBin(rcon, integer(), 1, 2, signed=FALSE, endian="big")
	}
	print(size)
	object <- if(type==0) {
		read_plist_singleton(rcon, size, tinfo)
	} else if (type==1) {
		read_plist_integer(rcon, size, tinfo)
	} else if (type==2) {
		read_plist_float(rcon, size, tinfo)
	} else if (type==5) {
		read_plist_sbstring(rcon, size, tinfo)
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

read_plist_sbstring <- function(rcon, size, tinfo) {
	value <- rawToChar(readBin(rcon, raw(), size))
	return(value)
}


read_plist_array <- function(rcon, size, tinfo) {
	len <- size * tinfo$reference_size
	values <- readBin(rcon, integer(), len, tinfo$reference_size, signed=FALSE, endian="big")
	return(list(values=values))
}

read_plist_dictionary <- function(rcon, size, tinfo) {
	len <- size * tinfo$reference_size
	keys <- readBin(rcon, integer(), len, tinfo$reference_size, signed=FALSE, endian="big")
	values <- readBin(rcon, integer(), len, tinfo$reference_size, signed=FALSE, endian="big")
	return(list(keys=keys, values=values))
}

read_plist_trailer <- function(rcon) {
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

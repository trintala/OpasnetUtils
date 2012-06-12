opasnet <- function(x, ...)
UseMethod("opasnet")

# Get data from Opasnet
#
# filename - Name of the file
# wiki - Source Wiki: opasnet_en (default), opasnet_fi
#
# Returns file contents (loaded using curl)

opasnet.data <- function(filename,wiki='') {

	# Parse arguments
	targs <- strsplit(commandArgs(trailingOnly = TRUE),",")
	args = list()
	for(i in targs[[1]])
	{
		tmp = strsplit(i,"=")
		key <- tmp[[1]][1]
		value <- tmp[[1]][2]
		args[[key]] <- value
	}
	
	if (wiki == '')
	{
		wiki = args$user
	}

	if (wiki == 'opasnet_en' || wiki == 'op_en')
	{
		file <- paste("http://en.opasnet.org/en-opwiki/images/",filename,sep='')
	}
	if (wiki == 'opasnet_fi' || wiki == 'op_fi')
	{
		file <- paste("http://fi.opasnet.org/fi_wiki/images/",filename,sep='')
	}
	if (wiki == 'heande')
	{
		file <- paste("http://",args$ht_username,":",args$ht_password,"@heande.opasnet.org/heande/images/",filename,sep='')
	}	
	return(getURL(file))
}

opasnet.csv <- function(filename, header = TRUE, sep = ",", quote = "\"",dec = ".", row.names, col.names,as.is = !stringsAsFactors,na.strings = "NA", colClasses = NA, nrows = -1,skip = 0, check.names = TRUE, fill = TRUE,strip.white = FALSE, blank.lines.skip = TRUE,comment.char = "",allowEscapes = FALSE, flush = FALSE,stringsAsFactors = default.stringsAsFactors(),fileEncoding = "", encoding = "unknown",wiki='') {

	# Parse arguments
	targs <- strsplit(commandArgs(trailingOnly = TRUE),",")
	args = list()
	for(i in targs[[1]])
	{
		tmp = strsplit(i,"=")
		key <- tmp[[1]][1]
		value <- tmp[[1]][2]
		args[[key]] <- value
	}
	
	if (wiki == '')
	{
		wiki = args$user
	}
	
	if (wiki == 'opasnet_en' || wiki == 'op_en')
	{
		file <- paste("http://en.opasnet.org/en-opwiki/images/",filename,sep='')
	}
	if (wiki == 'opasnet_fi' || wiki == 'op_fi')
	{
		file <- paste("http://fi.opasnet.org/fi_wiki/images/",filename,sep='')
	}
	if (wiki == 'heande')
	{
		file <- paste("http://",args$ht_username,":",args$ht_password,"@heande.opasnet.org/heande/images/",filename,sep='')
	}	
	
	csv <- getURL(file)
	
	return(read.table(file = textConnection(csv), header = header, sep = sep, quote = quote,
           dec = dec, row.names = row.names, col.names = col.names,
           as.is = as.is,
           na.strings = na.strings, colClasses = colClasses, nrows = nrows,
           skip = skip, check.names = check.names, fill = fill,
           strip.white = strip.white, blank.lines.skip = blank.lines.skip,
           comment.char = comment.char,
           allowEscapes = allowEscapes, flush = flush,
           stringsAsFactors = stringsAsFactors,
           fileEncoding = fileEncoding, encoding = encoding))
}
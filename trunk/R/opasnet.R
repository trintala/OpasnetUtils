opasnet <- function(x, ...)
UseMethod("opasnet")

# Get data from Opasnet
#
# filename - Name of the file
# wiki - Source Wiki: opasnet_en (default), opasnet_fi
#
# Returns file contents (loaded using curl)

opasnet.data <- function(filename,wiki='', unzip='') {

	now <- Sys.time()
	
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
	
	if (unzip != '')
	{
		f <- paste('/tmp/rtools_',as.numeric(now),'.zip',sep='')
		bin <- getBinaryURL(file)
		con <- file(f, open = "wb")
		writeBin(bin, con)
		close(con)
		con <- unz(f, unzip, "r")
		return(paste(readLines(con),collapse="\n"))
	}
	else
	{	
		return(getURL(file))
	}
}

opasnet.csv <- function(filename, wiki='', unzip = '', ...) {

	now <- Sys.time()
	
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

	if (unzip != '')
	{
		f <- paste('/tmp/rtools_',as.numeric(now),'.zip',sep='')
		bin <- getBinaryURL(file)
		con <- file(f, open = "wb")
		writeBin(bin, con)
		close(con)
		return(read.table(unz(f, unzip), ...))
	}
	else
	{	
		csv <- getURL(file)
		return(read.table(file = textConnection(csv), ...))
	}
	
	
}

# OPASNET.DATA #####################################
## opasnet.data downloads a file from Finnish Opasnet wiki, English Opasnet wiki, or Opasnet File.
## Parameters: filename is the URL without the first part (see below), wiki is "opasnet_en", "opasnet_fi", or "M-files".
## If table is TRUE then a table file for read.table function is assumed; all other parameters are for this read.table function.
#
#opasnet.data <- function(filename, wiki = "opasnet_en", table = FALSE, ...)
#{
#if (wiki == "opasnet_en") {
#file <- paste("http://en.opasnet.org/en-opwiki/images/", filename, sep = "")
#}
#if (wiki == "opasnet_fi") {
#file <- paste("http://fi.opasnet.org/fi_wiki/images/", filename, sep = "")
#}
#if (wiki == "M-files") {
#file <- paste("http://http://fi.opasnet.org/fi_wiki/extensions/mfiles/", filename, sep = "")
#}
#
#if(table == TRUE) {
#file <- re#ad.table(file, header = FALSE, sep = "", quote = "\"'",
#           dec = ".", row.names, col.names,
#           as.is = !stringsAsFactors,
#           na.strings = "NA", colClasses = NA, nrows = -1,
#           skip = 0, check.names = TRUE, fill = !blank.lines.skip,
#           strip.white = FALSE, blank.lines.skip = TRUE,
#           comment.char = "#",
#           allowEscapes = FALSE, flush = FALSE,
#           stringsAsFactors = default.stringsAsFactors(),
#           fileEncoding = "", encoding = "unknown")
#return(file)
#}
#else {return(ge#tURL(file))}
#}
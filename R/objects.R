objects <- function(x, ...)
UseMethod("objects")

# Wrapper for save-method, writes desired objects to run path as rdata

objects.put <- function(..., list = character()){
	
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
	
	fname <- paste(args$token,'_objs.RData.gz',sep='')
	
	save(..., list = list,
			file = fname,
			ascii = FALSE, version = NULL, envir = parent.frame(),
			compress = 'gzip', compression_level = 6,
			eval.promises = TRUE, precheck = TRUE)
}

# Wrapper for load-method, reads object for given run token

objects.get <- function(token){
	fname <- paste(token,'_objs.RData.gz',sep='')	
	load(fname, .GlobalEnv)
}
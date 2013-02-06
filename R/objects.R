objects <- function(x, ...)
UseMethod("objects")

# Wrapper for save-method, writes desired objects to run path as rdata

objects.put <- function(..., list = character()){
	
	# Parse arguments
	targs <- strsplit(commandArgs(trailingOnly = TRUE),",")
	args = list()
	
	if (length(targs) == 0) stop('This function can be used within Opasnet only!!!')
	
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

# New method for storing objects, writes key to the opasnet base as well
# Returns the key

objects.store <- function(..., list = character(), verbose = FALSE){
	
	# Parse arguments
	targs <- strsplit(commandArgs(trailingOnly = TRUE),",")
	args = list()
	
	if (length(targs) == 0) stop('This function can be used within Opasnet only!!!')
	
	for(i in targs[[1]])
	{
		tmp = strsplit(i,"=")
		key <- tmp[[1]][1]
		value <- tmp[[1]][2]
		args[[key]] <- value
	}
	
	now <- Sys.time()
	okey <- gsub("\\.","",as.character(as.numeric(now)))
	okey <- substr(okey,0,12)
	
	if (is.null(args$code_name)) stop('R-code block must have NAME to save objects!')
	
	# Write to base
	data <- matrix(c(args$wiki_page_id, args$code_name, format(now,"%Y-%m-%dT%I:%M:%OS2Z",tz='GMT'), okey), ncol=4, byrow=TRUE)
	colnames(data) <- c("Page ident","Code name","Time","result")
	data <- as.data.frame(data)
	
	index_types <- c("entity","entity","time")
	
	obj_name <- "Saved R objects"
	unit <- "#"
	who <- 'RTools'
	ident <- objects.page_ident(args$user)
	
	if (verbose) paste('Objects page ident:',print(ident),sep=' ')
	if (verbose) paste('Data to insert:',print(data),sep=' ')
	
	if (opbase.obj.exists(ident)){
		opbase.upload(input = data, ident = ident, name = obj_name, act_type = 'append', unit = unit, who = who, verbose = verbose)
	} else {
		opbase.upload(input = data, ident = ident, name = obj_name, act_type = 'replace', unit = unit, who = who, index_types = index_types, verbose = verbose)
	}
	
	# Now finally write objects
	fname <- paste(okey,'_objs.RData.gz',sep='')
	
	save(..., list = list,
			file = fname,
			ascii = FALSE, version = NULL, envir = parent.frame(),
			compress = 'gzip', compression_level = 6,
			eval.promises = TRUE, precheck = TRUE)	
	
	return(okey)
}


objects.latest <- function(page_ident, code_name, verbose = FALSE){
	
	# Parse arguments
	targs <- strsplit(commandArgs(trailingOnly = TRUE),",") 
	args = list()
	
	if (length(targs) == 0) stop('This function can be used within Opasnet only!!!') 
	
	for(i in targs[[1]])
	{
		tmp = strsplit(i,"=")
		key <- tmp[[1]][1]
		value <- tmp[[1]][2]
		args[[key]] <- value
	}
	
	ident <- objects.page_ident(args$user)
	
	if (verbose) print(paste('Saved R objects page ident is ', ident, sep=''))
	
	res <- opbase.data(ident, include = list('Page ident' = page_ident, 'Code name' = code_name), verbose = verbose)
	
	if (verbose) print(res)
	
	k <- max(res$Result)
	
	if (verbose) print(paste('Object key is ', k, sep=''))
	
	objects.get(k)
}

# Private function for getting the ident for page holding the key data
objects.page_ident <- function(base_user){
	if (base_user == 'heande') return('Heande3827')
	if (base_user == 'opasnet_en') return('Op_en5897')
	if (base_user == 'opasnet_fi') return('Op_fi3382')
	if (base_user == 'eractest') return('test4228')	
}


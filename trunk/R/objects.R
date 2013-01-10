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


objects.put2 <- function(..., list = character(), verbose = FALSE){
	
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
	
	now <- Sys.time()
	okey <- gsub("\\.","",as.character(as.numeric(now)))
	
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
	
	ret <- tryCatch(opbase.upload(input = data, ident = ident, name = obj_name, act_type = 'append', unit = unit, who = who), error = function(e) return(NULL))
	# If appending failed, then try replacing (object might not exist yet)
	if (is.null(ret))
	{
		opbase.upload(input = data, ident = ident, name = obj_name, act_type = 'replace', unit = unit, who = who, index_types = index_types)
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


objects.get_latest <- function(page_ident, code_name){
	
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
	
	ident <- objects.page_ident(args$user)
	
	res <- opbase.data(ident, include = list('Page ident' = page_ident, 'Code name' = code_name))
	objects.get(max(res$Key))
}

# Private function for getting the ident for page holding the key data
objects.page_ident <- function(base_user){
	if (base_user == 'heande') return('heande')
	if (base_user == 'opasnet_en') return('op_en')
	if (base_user == 'opasnet_fi') return('op_fi')
	if (base_user == 'eractest') return('test4228')	
}


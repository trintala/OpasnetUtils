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


objects.put2 <- function(..., list = character()){
	
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
	
	# Write to base
	if (args$user == 'heande') prefix <- 'heande'
	if (args$user == 'opasnet_en') prefix <- 'op_en'
	if (args$user == 'opasnet_fi') prefix <- 'op_fi'
	if (args$user == 'eractest') prefix <- 'test'

	data <- matrix(c(args$wiki_page_id,args$wiki_page_id,format(now,"%Y-%m-%dT%I:%M:%OS2Z",tz='GMT'),as.numeric(now)),ncol=4,byrow=TRUE)
	colnames(data) <- c("Page ident","Code name","Time","result")
	data <- as.data.frame(data)
	
	index_types <- c("entity","entity","time")
	
	obj_name <- "Saved R objects"
	unit <- "#"
	who <- 'RTools'
	ident <- paste(prefix,"_r_objects",sep='')
	
	ret <- tryCatch(opbase.upload(input = data, ident = ident, name = obj_name, act_type = 'append', unit = unit, who = who), error = function(e) return(NULL))
	# If appending failed, then try replacing (object might not exist yet)
	if (is.null(ret))
	{
		opbase.upload(input = data, ident = ident, name = obj_name, act_type = 'replace', unit = unit, who = who, index_types = index_types)
	}
	
	# Now finally write objects
	fname <- paste(as.numeric(now),'_objs.RData.gz',sep='')
	
	save(..., list = list,
			file = fname,
			ascii = FALSE, version = NULL, envir = parent.frame(),
			compress = 'gzip', compression_level = 6,
			eval.promises = TRUE, precheck = TRUE)	
	
	return(as.numeric(now))
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
	
	if (args$user == 'heande') prefix <- 'heande'
	if (args$user == 'opasnet_en') prefix <- 'op_en'
	if (args$user == 'opasnet_fi') prefix <- 'op_fi'
	if (args$user == 'eractest') prefix <- 'test'
	
	res <- opbase.data(paste(prefix,'_r_objects',sep=''), include = list('Page ident' = page_ident, 'Code name' = code_name))
	objects.get(max(res$Key))
}		
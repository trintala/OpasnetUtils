op_baseGetData <- function(dsn, ident, include = NULL, exclude = NULL, series_id = NULL, iterations = NULL, use.utf8 = TRUE, apply.utf8 = TRUE) {

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
	
	if (dsn == 'heande_base' && args$user != 'heande')
	{
		stop("Heande base is accessible only for the Heande Wiki!")
	}
		
	if (use.utf8) db <- odbcConnect(dsn, DBMSencoding = "UTF-8") else db <- odbcConnect(dsn)
	obj_id <- sqlQuery(db, paste('SELECT id FROM obj WHERE ident = "', ident, '"', sep=''))[1,1]
	if (length(series_id) == 0) {series_id <- sqlQuery(db, paste('SELECT series_id FROM actobj WHERE obj_id = ', obj_id, 
		' ORDER BY series_id DESC LIMIT 1', sep = ''))[1,1]}
	sliced <- FALSE
	locations <- NULL
	x <- 1
	basequery <- paste('SELECT loccell.cell_id FROM actobj LEFT JOIN cell ON actobj.id = cell.actobj_id LEFT JOIN', 
			' loccell ON cell.id = loccell.cell_id WHERE actobj.obj_id = ', obj_id, ' AND actobj.series_id = ', 
			series_id, ' AND loccell.loc_id IN(', sep = '')
	if (length(include) != 0) {
		sliced <- TRUE
		locations[x] <- paste("IN(", basequery, paste(include, collapse = ","), ")", sep = "")
		x <- x + 1
	}
	if (length(exclude) != 0) {
		sliced <- TRUE
		locations[x] <- paste("NOT IN(", basequery, paste(exclude, collapse = ","), ")", sep = "")
	}
	if (sliced == FALSE) {
		Data <- sqlQuery(db, paste('SELECT cell.id, res.obs, obj.ident AS ind, loc.location AS loc, res.result,', 
			' res.restext FROM actobj LEFT JOIN cell ON actobj.id = cell.actobj_id LEFT JOIN res ON cell.id =', 
			' res.cell_id LEFT JOIN loccell ON cell.id = loccell.cell_id LEFT JOIN loc ON loccell.loc_id',
			' = loc.id LEFT JOIN obj ON loc.obj_id_i = obj.id WHERE actobj.obj_id = ', obj_id, 
			' AND actobj.series_id = ', series_id, if(length(iterations)==1){paste(" AND obs <= ", iterations, 
			sep = "")}, sep = '')) } else {
		Data <- sqlQuery(db, paste('SELECT cell.id, res.obs, obj.ident AS ind, loc.location AS loc, res.result,', 
			' res.restext FROM actobj LEFT JOIN cell ON actobj.id = cell.actobj_id LEFT JOIN res ON cell.id =', 
			' res.cell_id LEFT JOIN loccell ON cell.id = loccell.cell_id LEFT JOIN loc ON loccell.loc_id',
			' = loc.id LEFT JOIN obj ON loc.obj_id_i = obj.id WHERE actobj.obj_id = ', obj_id, 
			' AND actobj.series_id = ', series_id, if(length(iterations)==1){paste(" AND obs <= ", iterations, 
			sep = "")}, ' AND (cell.id ', paste(locations, collapse = ') AND cell.id '), 
			'))', sep = ''))
	}
	odbcClose(db)
	Data <- Data[order(Data[,1], Data[,2], Data[,3]),]
	nind <- length(levels(Data[,3]))
	nres <- nrow(Data)/nind
	dataframe <- Data[1:nres*nind, c(1,2)]
	for (i in 1:nind) {
		dataframe[,2 + i] <- factor(Data[1:nres*nind - (nind - i), 4])
		levels(dataframe[,2 + i]) <- gsub(" *$", "",gsub("^ *", "", levels(dataframe[,2 + i])))
		colnames(dataframe)[2 + i] <- as.character(Data[i, 3])
		if(apply.utf8) Encoding(levels(dataframe[,2 + i])) <- "UTF-8"
	}
	dataframe[,1:2 + 2 + nind] <- Data[1:nres*nind, 5:6]
	colnames(dataframe)[1:2 + 2 + nind] <- c("Result", "Result.Text")
	if(apply.utf8) {if(is.factor(dataframe[,"Result.Text"])) {Encoding(levels(dataframe[,"Result.Text"])) <- "UTF-8"} else if(is.character(dataframe[,"Result.Text"])) {
		Encoding(dataframe[,"Result.Text"]) <- "UTF-8"}}
	rownames(dataframe) <- 1:nres
	return(dataframe)
}

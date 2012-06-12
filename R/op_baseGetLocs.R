op_baseGetLocs <- function(dsn, ident, series_id = NULL, use.utf8 = TRUE, apply.utf8 = TRUE) {

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
		return("Heande base is accessible only for the Heande Wiki!")
	}	
	
	if (use.utf8) db <- odbcConnect(dsn, DBMSencoding = "UTF-8") else db <- odbcConnect(dsn)
	obj_id <- sqlQuery(db, paste('SELECT id FROM obj WHERE ident = "', ident, '"', sep=''))[1,1]
	if (length(series_id) == 0) {series_id <- sqlQuery(db, paste('SELECT series_id FROM actobj WHERE obj_id = ', obj_id, 
		' ORDER BY series_id DESC LIMIT 1', sep = ''))[1,1]}
	Locs <- sqlQuery(db, paste("SELECT DISTINCT obj.ident AS ind, loc.location AS loc, loc.id AS loc_id", 
		" FROM actobj LEFT JOIN actloc ON actobj.id = actloc.actobj_id LEFT JOIN loc ON actloc.loc_id = loc.id", 
		" LEFT JOIN obj ON loc.obj_id_i = obj.id WHERE actobj.obj_id = ", obj_id, " AND actobj.series_id = ", 
		series_id, sep = ""))
	odbcClose(db)
	Locs <- Locs[order(Locs$ind, Locs$loc_id),]
	rownames(Locs) <- 1:nrow(Locs)
	if(apply.utf8) {Encoding(levels(Locs$ind)) <- "UTF-8"; Encoding(levels(Locs$loc)) <- "UTF-8"}
	return(Locs)
}

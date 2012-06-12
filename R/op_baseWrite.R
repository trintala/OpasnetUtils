op_baseWrite <- function(dsn, input, ident = NULL, name = NULL, unit = NULL, objtype_id = NULL, who = NULL, acttype = NULL, 		
	rescol = NULL, n.obs.const = FALSE, maxrows = 50000, use.utf8 = TRUE, use.utf8.read = TRUE, latin1.2.utf8.conv.write = TRUE, utf8.2.latin1.conv.read = TRUE) {
	
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

	# Coerce input into a data frame if it isn't one already; get rid of empty cells
	if (is.array(input)) dataframe <- as.data.frame(as.table(input)) else dataframe <- input
	if (is.null(rescol)) {
	rescol <- colnames(dataframe) == "Freq"
	if (sum(rescol) == 1) rescol <- "Freq" else {
		rescol <- colnames(dataframe) == "Result"
		if (sum(rescol) == 1) rescol <- "Result" else {
			rescol <- colnames(dataframe) == "result"
			if (sum(rescol) == 1) rescol <- "result"
		}
	}}
	dataframe <- dataframe[is.na(dataframe[,rescol]) == FALSE,]
	ColNames <- colnames(dataframe)[!(colnames(dataframe)%in%c(rescol, "id", "obs"))]
	for (i in ColNames) {
		dataframe[,i] <- factor(dataframe[,i])
		levels(dataframe[,i]) <- gsub(" *$", "",gsub("^ *", "", levels(dataframe[,i])))
		if(latin1.2.utf8.conv.write) if(sum(Encoding(levels(dataframe[,i]))=="latin1")!=0) levels(dataframe[,i]) <- iconv(levels(dataframe[,i]), "latin1", "UTF-8")
	}
	
	#if(!is.numeric(dataframe[,rescol])) 
	
	# Open database connection
	if(use.utf8) db <- odbcConnect(dsn, DBMSencoding = "UTF-8") else db <- odbcConnect(dsn)
	if(!use.utf8.read) db2 <- odbcConnect(dsn)
	
	# Add page to database (if it doesn't already exist)
	if (is.null(ident)) if (interactive()) ident <- readline(paste("What is the identifier of this object?", 
		"\n", sep = "")) else stop("indentifier of object no given")
	obj_id <- sqlQuery(db, paste('SELECT id FROM obj WHERE ident = "', ident, '"', sep = ''))[1,1]
	if (is.na(obj_id)) {
		
		# Wiki id
		if (substr(ident, 1,5)=="Op_en") {wiki_id <- 1; page <- substr(ident, 6, nchar(ident))} else {
		if (substr(ident, 1,5)=="Op_fi") {wiki_id <- 2; page <- substr(ident, 6, nchar(ident))} else {
		if (substr(ident, 1,6)=="Heande") {wiki_id <- 6; page <- substr(ident, 7, nchar(ident))} else {
		if (substr(ident, 1,4)=="Erac") {wiki_id <- 6; page <- substr(ident, 5, nchar(ident))} else {
		wiki_id <- 0; page <- 0; warning("No wiki id found in ident, writing zero.")}}}}
		page <- as.numeric(page)
		if (is.na(page)) stop("could not convert characters following the wiki ident into a page number")
		
		# Name etc.
		if (is.null(name)) if (interactive()) name <- readline(paste("What is the name of this object?", 
			"\n", sep = "")) else stop("object name not given")
		if (is.null(objtype_id)) if (interactive()) objtype_id <- readline(paste("What type of object is", 
			" this (id)?", paste(paste(sqlQuery(db, "SELECT id FROM objtype")[,1], sqlQuery(db, paste("SELECT objtype",
			" FROM objtype", sep = ""))[,1], sep = " - "), collapse = ", "), "\n", collapse = " ")) else {
			stop("object type not given")}
		sqlQuery(db, paste('INSERT INTO obj (ident, name, objtype_id, page, wiki_id) VALUES ("', paste(ident, 
			name, sep = '","'), '",', paste(objtype_id, page, wiki_id, sep = ','), ')', sep = ''))
		obj_id <- sqlQuery(db, paste('SELECT id FROM obj WHERE ident = "', ident, '"', sep = ''))[1,1]
	}
	
	# Write act and actobj
	if (is.null(who)==TRUE) if (interactive()) {who <- readline(paste("What is the name of the uploader?", "\n", sep = "")) 
		} else stop("uploader name not given")
	series_id <- sqlQuery(db, paste("SELECT series_id FROM actobj WHERE obj_id = ", obj_id, " ORDER BY series_id DESC LIMIT 1", 
		sep = ""))[1,1]
	if (is.na(series_id)==FALSE) {if (is.null(acttype)==TRUE) {if (interactive()) {acttype <- readline(paste("What type of upload", 
		" is this? 4 - new data to replace any existing, 5 - new data to be appended to existing data (must have the same", 
		" indices).", "\n", sep = "")) 
		} else acttype <- 4 
		}} else acttype <- 4
	if (!(acttype%in%c(4,5))) stop ("proper acttype not given")
	
	sqlQuery(db, paste('INSERT INTO act (acttype_id, who, comments) VALUES (', acttype, ',"', who, '","R upload")', sep = ''))
	act_id <- sqlQuery(db, paste('SELECT id FROM act WHERE who = "', who,'" AND comments = "R upload" ORDER BY id DESC LIMIT 1', 
		sep = ''))[1,1]
	if (acttype == 4) series_id <- act_id
	if (is.null(unit)) if (interactive()) unit <- readline(paste("What is the unit of this object?", 
		"\n", sep = "")) else stop("unit not given")
	sqlQuery(db, paste('INSERT INTO actobj (act_id, obj_id, series_id, unit) VALUES (', paste(act_id, obj_id, series_id, sep = ','), 
		',"', unit, '")', sep = ''))
	actobj_id <- sqlQuery(db, paste('SELECT id FROM actobj WHERE act_id = ', act_id, sep = ''))[1,1]
	
	#Write indexes
	for (i in ColNames) {
		sqlQuery(db, paste('INSERT IGNORE INTO obj (ident, name, objtype_id) VALUES ("', gsub(' ', '_', i), '","', 
			i, '", 6)', sep = ''))
	}
	IndIds <- sqlQuery((if(use.utf8.read) db else db2), paste('SELECT id, ident FROM obj WHERE ident IN("', paste(gsub(" ", "_", ColNames), 
		collapse = '","'), '")', sep = ''))
	if(utf8.2.latin1.conv.read) levels(IndIds$ident) <- iconv(levels(IndIds$ident), "UTF-8", "latin1")
	IndIdMap <- IndIds$id
	names(IndIdMap) <- tolower(IndIds$ident)
	ColIds <- as.character(IndIdMap[tolower(gsub(" ", "_", ColNames))])
	colnames(dataframe)[colnames(dataframe)%in%ColNames] <- ColIds
	
	#Write locations
	for (i in ColIds) {
		for (j in levels(dataframe[, i])) {
			sqlQuery(db, paste('INSERT IGNORE INTO loc (obj_id_i, location) VALUES (', i, ',"', j, '")', 
				sep = ''))
		}
	}
	LocIds <- sqlQuery((if(use.utf8.read) db else db2), paste('SELECT id, obj_id_i, location FROM loc WHERE obj_id_i IN("', paste(ColIds, collapse = '","'), 
		'")', sep = ''))
	if(utf8.2.latin1.conv.read) levels(LocIds$location) <- iconv(levels(LocIds$location), "UTF-8", "latin1")
	
	for (i in ColIds) {
		LocIdMap <- LocIds[LocIds$obj_id_i == i, 1]
		names(LocIdMap) <- gsub(" *$", "",gsub("^ *", "", tolower(LocIds[LocIds$obj_id_i == i, 3])))
		levels(dataframe[, i]) <- LocIdMap[tolower(levels(dataframe[, i]))]
		if (sum(is.na(levels(dataframe[, i]))) != 0) stop("Faulty location matching. Usually caused by special characters.")
		
		#Writing actloc
		sqlQuery(db, paste("INSERT INTO actloc (actobj_id, loc_id) VALUES (", paste(actobj_id, levels(dataframe[, i]), 
		sep = ",", collapse = "),("), ")", sep = ""))
	}
	
	#Writing cell
	n <- tapply(dataframe[,rescol], dataframe[,ColIds], length)
	ncell <- sum(n, na.rm = TRUE)
	if (is.numeric(dataframe[,rescol])) means <- tapply(dataframe[,rescol], dataframe[,ColIds], mean) else means <- rep(0, ncell)
	if (is.numeric(dataframe[,rescol])) {
		sds <- tapply(dataframe[,rescol], dataframe[,ColIds], sd); sds[] <- ifelse(n == 1, 0, sds)} else sds <- rep(0, ncell)
	
	cellQuery <- paste(actobj_id, means[!is.na(means)], sds[!is.na(sds)], n[!is.na(n)], sep = ",")
	i <- 1
	while (length(cellQuery) >= (i + maxrows - 1)) {
		sqlQuery(db, paste('INSERT INTO cell (actobj_id, mean, sd, n) VALUES (', paste(cellQuery[i:(i + maxrows - 1)], 
			collapse = '),('), ')', sep = ''))
		i <- i + maxrows
	}
	if (length(cellQuery) %% maxrows != 0) {
		sqlQuery(db, paste('INSERT INTO cell (actobj_id, mean, sd, n) VALUES (', 
			paste(cellQuery[i:length(cellQuery)], collapse = '),('), ')', sep = ''))
	}
	
	#Writing res
	cell_id <- sqlQuery(db, paste("SELECT id FROM cell WHERE actobj_id = ", actobj_id, " ORDER BY ID", sep = ""))[,1]
	if (length(cell_id) != ncell) stop("number of written cells differs from given data")
	if (is.numeric(dataframe[,rescol])) ids <- means else ids <- n
	ids[!is.na(ids)] <- cell_id
	dataframe[, ncol(dataframe) + 1] <- ids[as.matrix(dataframe[,ColIds])]
	colnames(dataframe)[ncol(dataframe)] <- "cell_id"
	
	resQuery <- paste(dataframe[,"cell_id"], ',', if(sum(colnames(dataframe) == "obs") == 0) 1 else dataframe[,"obs"], ',', 
		if (!is.numeric(dataframe[,rescol])) '"', dataframe[,rescol], if (!is.numeric(dataframe[,rescol])) '"', sep = "")
	i <- 1
	while (length(resQuery) >= (i + maxrows - 1)) {
		sqlQuery(db, paste('INSERT INTO res (cell_id, obs, ', ifelse(is.numeric(dataframe[,rescol]), "result", "restext"), ') VALUES (', 
			paste(resQuery[i:(i + maxrows - 1)], collapse = '),('), ')', sep = ''))
		i <- i + maxrows
	}
	if (length(resQuery) %% maxrows != 0) {
		sqlQuery(db, paste('INSERT INTO res (cell_id, obs, ', ifelse(is.numeric(dataframe[,rescol]), "result", "restext"), ') VALUES (', 
			paste(resQuery[i:length(resQuery)], collapse = '),('), ')', sep = ''))
	}
	
	#Writing loccell
	ids <- as.data.frame(as.table(ids))
	ids <- ids[!is.na(ids$Freq),]
	loccellQuery <- paste(ids$Freq, unlist(ids[,-ncol(ids)]), sep = ",")
	i <- 1
	while (length(loccellQuery) >= (i + maxrows - 1)) {
		sqlQuery(db, paste('INSERT INTO loccell (cell_id, loc_id) VALUES (', paste(loccellQuery[i:(i + maxrows - 1)], collapse = '),('), ')', 
			sep = ''))
		i <- i + maxrows
	}
	if (length(loccellQuery) %% maxrows != 0) {
		sqlQuery(db, paste('INSERT INTO loccell (cell_id, loc_id) VALUES (', paste(loccellQuery[i:length(loccellQuery)], collapse = '),('), ')', 
			sep = ''))
	}
	
	#Close database connection
	odbcClose(db)
	cat("Successful\n")
	return(character())
}

#opbase <- function(x, ...)
#UseMethod("opbase")

# Read data from opasnet base 2
opbase.data <- function(ident, series_id = NULL) {
	
	# Then aim for the data itself
	# act == 0 gets the most recent series of data!
	if (is.null(series_id))
	{
		url <- paste("ident=", ident, "&act=0", sep = "")
	}
	else
	{
		url <- paste("ident=", ident, "&series=", series_id, sep = "")
	}
	
	object <- opbase.query(url)
	
	if(is.null(object$key) || object$key == '')
	{
		stop(paste("Invalid download key retrieved! Query:", url, sep=''))
	}
	
	out <- data.frame()
	data <- NULL
	first <- TRUE
	
	while ((!is.null(data) && data != '') | first) {
		first <- FALSE
		
		temp <- opbase.query(paste("key=", object$key, sep = ""))
		
		data <- temp$data
		
		if (!is.null(data) && data != '')
		{
			temp <- fromJSON(data)
			temp <- lapply(temp, list.to.data.frame)		
			lengths <- lapply(temp, nrow)	
			temp <- do.call("rbind", temp)
			
			if (sum(unlist(lengths) > 1) > 0) {
				iterations <- lapply(lengths, f.iter)
				temp$Iter <- unlist(iterations)
			}	
			out <- rbind(out, temp)
		}
		
	}
	
	#indices <- object[[2]][[acts$slot[acts$id == act]]][[2]]
	#indices <- lapply(indices, as.data.frame)
	
	temp <- data.frame()
	for (i in  object$indices) {
		temp <- rbind(temp, data.frame(Name = i$name, id = i$ident))
	}
	indices <- temp
	
	colnames(out) <- gsub("^\"|\"$", "", colnames(out))
	out <- out[,!colnames(out) %in% c("sid", "aid", "mean", "sd")]
	for(i in 1:ncol(out)) {
		out[[i]] <- gsub("^\"|\"$", "", out[[i]])
		out[[i]] <- gsub("^ *| *$", "", out[[i]])
		temp <- indices[indices$id == colnames(out)[i], "Name"]
		if (length(temp) == 1) colnames(out)[i] <- as.character(temp)
		temp <- suppressWarnings(as.numeric(out[[i]]))
		if (sum(is.na(temp)) == 0) out[[i]] <- temp 
	}
	colnames(out)[colnames(out) == "res"] <- "Result"
	return(out)
}

# Write data to the new opasnet database
opbase.upload <- function(input, ident = NULL, name = NULL, obj_type = 'variable', act_type = 'replace', language = 'eng', unit = '', who = NULL, rescol = NULL, chunk_size = NULL, verbose = FALSE) {
	
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
	
	if (is.null(ident) == TRUE)
	{
		ident <- args$wiki_page_id
	}
	
	server <- 'cl1.opasnet.org'
	path <- '/opasnet_base_2/index.php'
	
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
	
	# Wiki id
	if (substr(ident, 1,5)=="Op_en") {wiki_id <- 1; page <- substr(ident, 6, nchar(ident))} else {
		if (substr(ident, 1,5)=="Op_fi") {wiki_id <- 2; page <- substr(ident, 6, nchar(ident))} else {
			if (substr(ident, 1,6)=="Heande") {wiki_id <- 3; page <- substr(ident, 7, nchar(ident))} else {
				if (substr(ident, 1,4)=="test") {wiki_id <- 4; page <- substr(ident, 5, nchar(ident))} else {
					stop(paste("No wiki id found in ident ",ident,sep=''))}}}}
	
	n <- length(dataframe[1,rescol])
	page <- as.numeric(page)
	if (is.na(page)) stop("Could not convert characters following the wiki ident into a page number!\n")
	if (is.null(who)==TRUE) stop("uploader name not given")
	if (is.null(name)==TRUE && act_type == 'replace') stop("object name not given")
	
	# Build index list
	indices = list()

	for (i in 1:length(ColNames)) {
		indices[[i]] = list(type='entity',name=ColNames[[i]],page=page,wiki_id=wiki_id,order_index=i,hidden=0,unit='') 
	}
	
	header <- list(
			object = list(
					name = name,
					ident = ident,
					#subset_name = 'Kaavi',
					type = obj_type,
					page = page,
					wiki_id = wiki_id
			),
			act = list(
					unit = unit,
					who = who,
					samples = n,
					comments = "R upload",
					language = language
			),
			indices = indices
	)
		
	if (act_type == 'replace')
	{
		method <- 'POST';
	}
	if (act_type == 'append')
	{
		method <- 'PUT';
	}
	
	if (verbose) print(header)
	
	data <- list('_method' = method, 'json' = toJSON(header))
	
	response <- postToHost(server, path, data)
	
	if (is.null(response)) stop('Server is not responding!!!')

	if (verbose) print(response)
	
	# Parse JSON data from the server response
	response <- fromJSON(regmatches(response, regexpr('\\{.+\\}',response)))
	if (! is.null(response$error)) stop(response$error)
	if (is.null(response$key) || response$key == '') stop(paste("Invalid upload key retrieved! Query:", url, sep=''))
	
	data_rows <- nrow(dataframe)
	
	# Automatic chunksize?
	if (is.null(chunk_size))
	{
		chunk_size <- 1000
	
		if (n > 1)
		{
			div <-  n/ 30
			if (div < 1) div <- 1
			chunk_size = round(chunk_size / div)	
			if (chunk_size < 1) chunk_size <- 1
		}
	}
	
	if (chunk_size > data_rows) chunk_size <- data_rows
	
	start <- 1
	end <- chunk_size
	
	rows <- 0
	
	# Write the data
	repeat
	{
		data_chunk = dataframe[start:end,]
		data_rows = list()
		r = 1
		# Create data list for JSON
		for (row in data_chunk)
		{
			data_rows[[r]] <- list('res' = row[[rescol]])
			i = 1
			for (col in ColNames)
			{
				data_rows[[r]][[i]] <- row[[col]]
				i <- i + 1
			}
			r <- r + 1
		}
		
		json <- toJSON(list('key' = response$key, 'indices' =  indices,  'data' = data_rows))
		
		if (verbose) print(json)
		
		data <- list('json' = json)
		response <- postToHost(server, path, data)
		
		if (is.null(response)) stop('Server is not responding!!!')
		
		if (verbose) print(response)
		
		# Parse JSON data from the server response
		response <- fromJSON(regmatches(response, regexpr('\\{.+\\}',response)))
		if (! is.null(response$error)) stop(response$error)
			
		if (response$rows !=  (end-start+1)) stop(paste('Invalid inserted rows count! ',response$rows, ' vs ', (end-start+1), sep=''))
	
		rows <- rows + response$rows
		
		if (end >= data_rows) break
		
		start <- end + 1
		end <- end + chunk_size
		if (end > data_rows) end <- data_rows
	}
	
	return(rows)
	
}

# Private function to make queries to server
opbase.query <- function(query) {
	url <- paste("http://cl1.opasnet.org/opasnet_base_2/index.php?", query, sep = "")
	
	response <- fromJSON(
			paste(
					readLines(url),  
					collapse = ""
			)
	)
	
	if (is.null(response))
	{
		stop("Server is not responding! Unable to retrieve data download key!")
	}
	
	if (! is.null(response$error))
	{
		stop(paste("Query: ",query,", Error: ",response$error, sep= ''))
	}
	
	return(response)
}

list.to.data.frame <- function(x) { # As.data.frame equivalent, but allows one list as longer than others. 
	do.call("data.frame", x) # Which is the case with opb2's probabilistic results. 
}

f.iter <- function(x) {
	1:x
}

objlist <- fromJSON(
	paste(
		readLines(
			"http://cl1.opasnet.org/opasnet_base_2/index.php", 
		), 
		collapse = ""
	)
)



# Read data from the old opasnet database
opbase.old.data <- function(dsn, ident, ...) {

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
	
	return(opbase.old.read(dsn, ident, ...))
}


# Write data to the old opasnet database
opbase.old.upload <- function(dsn, input, ...) {
	
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
	
	return(opbase.old.write(dsn, input, ...))
}

# Locations of an object in the old opasnet database
opbase.old.locations <- function(dsn, ident, ...) {
	
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
	
	return(opbase.old.locations.read(dsn, ident, ...))
}

# Use-restricted equivalents for cross wiki reads

opbase.old.read <- function(
	dsn, 
	ident, 
	include = NULL, 
	exclude = NULL, 
	series_id = NULL, 
	iterations = NULL, 
	use.utf8 = TRUE, 
	apply.utf8 = TRUE
) {
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
	if(apply.utf8) {
		if(is.factor(dataframe[,"Result.Text"])) {
			Encoding(levels(dataframe[,"Result.Text"])) <- "UTF-8"
		} else if(is.character(dataframe[,"Result.Text"])) {
			Encoding(dataframe[,"Result.Text"]) <- "UTF-8"
		}
	}
	rownames(dataframe) <- 1:nres
	return(dataframe)
}

opbase.old.locations.read <- function(dsn, ident, series_id = NULL, use.utf8 = TRUE, apply.utf8 = TRUE, ...) {
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

opbase.old.write <- function(
	dsn, 
	input, 
	ident = NULL, 
	name = NULL, 
	unit = NULL, 
	objtype_id = NULL, 
	who = NULL, 
	acttype = NULL, 		
	rescol = NULL, 
	n.obs.const = FALSE, 
	maxrows = 50000, # limits number of rows passed in a single query
	use.utf8 = TRUE, 
	use.utf8.read = TRUE,
	latin1.2.utf8.conv.write = TRUE, 
	utf8.2.latin1.conv.read = TRUE
) {
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
		if(latin1.2.utf8.conv.write) {
			if(sum(Encoding(levels(dataframe[,i]))=="latin1")!=0) {
				levels(dataframe[,i]) <- iconv(levels(dataframe[,i]), "latin1", "UTF-8")
			}
		}
	}
	
	#if(!is.numeric(dataframe[,rescol])) 
	
	# Open database connection
	if(use.utf8) db <- odbcConnect(dsn, DBMSencoding = "UTF-8") else db <- odbcConnect(dsn)
	if(!use.utf8.read) db2 <- odbcConnect(dsn)
	
	# Add page to database (if it doesn't already exist)
	if (is.null(ident)) if (interactive()) ident <- readline(paste("What is the identifier of this object?", 
		"\n", sep = "")) else stop("Identifier of object not given!\n")
	obj_id <- sqlQuery(db, paste('SELECT id FROM obj WHERE ident = "', ident, '"', sep = ''))[1,1]
	if (is.na(obj_id)) {
		
		# Wiki id
		if (substr(ident, 1,5)=="Op_en") {wiki_id <- 1; page <- substr(ident, 6, nchar(ident))} else {
		if (substr(ident, 1,5)=="Op_fi") {wiki_id <- 2; page <- substr(ident, 6, nchar(ident))} else {
		if (substr(ident, 1,6)=="Heande") {wiki_id <- 6; page <- substr(ident, 7, nchar(ident))} else {
		if (substr(ident, 1,4)=="Erac") {wiki_id <- 6; page <- substr(ident, 5, nchar(ident))} else {
		wiki_id <- 0; page <- 0; warning("No wiki id found in ident, writing zero.\n")}}}}
		page <- as.numeric(page)
		if (is.na(page)) stop("Could not convert characters following the wiki ident into a page number!\n")
		
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
	if (nrow(series_id)) {if (is.null(acttype)==TRUE) {if (interactive()) {acttype <- readline(paste("What type of upload", 
		" is this? 4 - new data to replace any existing, 5 - new data to be appended to existing data (must have the same", 
		" indices).", "\n", sep = "")) 
		} else acttype <- 4 
		}} else acttype <- 4
	if (!(acttype%in%c(4,5))) stop ("Proper acttype not given!\n")
	
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
	LocIds <- sqlQuery(
		(if(use.utf8.read) db else db2), 
		paste('SELECT id, obj_id_i, location FROM loc WHERE obj_id_i IN("', paste(ColIds, collapse = '","'), 
		'")', sep = '')
	)
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

# Wrapper function to create interface between old op_baseWrite and new opbase.upload
opbase.write <- function(
		dsn,
		input, 
		ident = NULL,
		name = NULL, 
		unit = NULL, 
		objtype_id = NULL, 
		who = NULL, 
		acttype = NULL, 		
		rescol = NULL, 
		n.obs.const = FALSE,
		maxrows = 50000,
		use.utf8 = TRUE, 
		use.utf8.read = TRUE,
		latin1.2.utf8.conv.write = TRUE, 
		utf8.2.latin1.conv.read = TRUE
) { 
	
	if (! is.null(objtype_id))
	{
		obj_types <- list(o1 = 'variable', o2 = 'study', o3 = 'method', o4 = 'assessment', o5 = 'class', o7 = 'nugget', o8 = 'encyclopedia')
		obj_type <- obj_types[[paste('o',objtype_id,sep='')]]
	}
	else
	{
		obj_type <- NULL
	}
	
	if (! is.null(acttype))
	{
		act_types <- list(a4 = 'replace', a5 = 'append')
		act_type <- act_types[[paste('a',acttype,sep='')]]
	}
	else
	{
		act_type <- NULL
	}

	return(opbase.upload(input, ident = ident, name = name, obj_type = obj_type, act_type = act_type, unit = unit, who = who, rescol = rescol))
	
}

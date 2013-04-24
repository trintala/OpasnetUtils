 #opbase <- function(x, ...)
#UseMethod("opbase")

# Returns TRUE if object with given ident exists in Opasnet Base
opbase.obj.exists <- function(ident, username = NULL, password = NULL)
{
	q <- list('ident' = ident)
	ret <- tryCatch(opbase.query(q, username, password), error = function(e) return(NULL))
	if (is.null(ret)){
		return(FALSE)
	} else {
		return(TRUE)
	}
}

opbase.locations <- function(ident, index_name, series_id = NULL, username = NULL, password = NULL)
{
	query = list()
	query[['ident']] <- ident
	query[['index_name']] <- URLencode(index_name, reserved = TRUE)
	if (! is.null(series_id)) query[['series_id']] <- series_id
	ret <- opbase.query(query, username, password)
	return(ret$locations)
}

# Fetch all series ids
opbase.series <- function(ident, username = NULL, password = NULL, verbose = FALSE)
{
	query = list()
	query[['ident']] <- ident
	query[['username']] <- username
	query[['password']] <- password
	object <- opbase.query(query, username, password)

	if(is.null(object$acts)) stop("Acts not found!")
	
	ret = c()
	
	if (verbose) print(object$acts)
	
	for (a in object$acts)
	{
		ret <- append(ret, as.numeric(a$act$series_id))
	}
	
	return(unique(ret))
}

# Read data from opasnet base 2
opbase.data <- function(ident, series_id = NULL, subset = NULL, verbose = FALSE, username = NULL, password = NULL, samples = NULL, exclude = NULL, include = NULL, optim_test = TRUE, ...) {
	
	query = list()

	query[['ident']] <- ident
	if (! is.null(subset)) query[['ident']] <- paste(query[['ident']], opbase.sanitize_subset_name(subset), sep='.')
	
	if (is.null(series_id))
	{
		query[['act']] <- 0
	}
	else
	{
		query[['series']] <- series_id
	}
	
	if (! is.null(samples)) query[['samples']] <- samples
	if (! is.null(exclude)) query[['exclude']] <- opbase.parse_locations(exclude, query[['ident']], series_id, username, password)
	if (! is.null(include)) query[['include']] <- opbase.parse_locations(include, query[['ident']], series_id, username, password)
	
	query[['username']] <- username
	query[['password']] <- password
	
	#if (verbose) print(query)
	
	# Run query to get KEY for downloading the actual data
	object <- opbase.query(query, username, password)
	
	#if (verbose) print(object)
	if (verbose) print("Object info and download key loaded!")
	
	if(is.null(object$key) || object$key == '') stop("Invalid download key retrieved!")
	
	out <- data.frame()
	data <- NULL
	first <- TRUE
	
	query = list()
	query[['key']] <- object$key
	
	while ((!is.null(data) && data != '') | first) {
		first <- FALSE

		if (verbose) print(paste('Loading data chunk from server... ',format(Sys.time(), "%H:%M:%OS3"),sep=''))
		temp <- opbase.query(query, username, password)
		data <- temp$data
		if (verbose) print('Data loaded ok!')
		if (verbose) print(paste('Processing data... ',format(Sys.time(), "%H:%M:%OS3"),sep=''))
		if (!is.null(data) && data != '')
		{
			temp <- fromJSON(data)
			if (optim_test){
				tmp <- list()
				iterator <- list()
				for (rivi in temp) {
					for (sarake in names(rivi)) {
						tmp[[sarake]] <- c(tmp[[sarake]], rivi[[sarake]])
						iterator[[sarake]] <- c(iterator[[sarake]], length(rivi[[sarake]]))
					}
				}
				# Using iterator it would be possible to implement multiple results on any column, but since this is not possible
				# at the moment only res will be checked.
				iterate = FALSE
				if (is.null(samples) || (! is.null(samples) && samples > 0)) {
					if (prod(iterator[["res"]])>1) {
						for (sarake in names(tmp[names(tmp)!="res"])) {
							tmp[[sarake]] <- rep(tmp[[sarake]], times = iterator[["res"]])
						}
						iterate = TRUE
					}
				}
				tmp <- data.frame(tmp)
				if (iterate) {
					iterations <- lapply(iterator[["res"]], f.iter)
					tmp$Iteration <- unlist(iterations)
				}
				# This method appears to be slower than the original with heavily iterated data (~15% difference with 5000 samples).
				# As the number of rows per chunk gets smaller, the difference between per-row and per-column approaches 
				# diminishes while this method wastes more resources calculating the cell data lengths.
				out <- rbind(out, tmp)
			} else {
				if (verbose) print(paste('JSON parsed',format(Sys.time(), "%H:%M:%OS3"),sep=''))
				temp <- lapply(temp, data.frame)#list.to.data.frame)	# THIS FUNCTION IS RELATIVELY SLOW!!!! Could this be done in any other way?
				if (verbose) print(paste('Converted to data frame',format(Sys.time(), "%H:%M:%OS3"),sep=''))
				lengths <- lapply(temp, nrow)	
				if (verbose) print(paste('Row lengths resolved',format(Sys.time(), "%H:%M:%OS3"),sep=''))
				temp <- do.call("rbind", temp)
				if (verbose) print(paste('Rbind done',format(Sys.time(), "%H:%M:%OS3"),sep=''))
				
				if (is.null(samples) || (! is.null(samples) && samples > 0)) {
					if  (sum(unlist(lengths) > 1) > 0) {
						iterations <- lapply(lengths, f.iter)
						temp$Iteration <- unlist(iterations)
					}
				}	
				out <- rbind(out, temp)
				if (verbose) print(paste('Concatenated chunk to output',format(Sys.time(), "%H:%M:%OS3"),sep=''))
			}
		}
		if (verbose) print('Data processed ok!')
	}
	
	if (nrow(out) == 0) stop('Empty result set!')
	
	#if (verbose) print(out)


	if (is.null(samples) || samples > 0) {
		out <- out[,!colnames(out) %in% c("sid", "aid", "mean", "sd")]
		colnames(out)[colnames(out) == "res"] <- "Result"	
		a <- suppressWarnings(as.numeric(as.character(out$Result)))
		if (sum(is.na(a)) == 0) out$Result <- a
	} else {
		out <- out[,!colnames(out) %in% c("sid", "aid")]
		colnames(out)[colnames(out) == "mean"] <- "Mean"	
		colnames(out)[colnames(out) == "sd"] <- "Sd"	
	}

	for(i in 1:length(object$indices)) {
		ind <- object$indices[[i]]
		temp <- as.character(ind$name)
		if (verbose) print(paste("Index ",i," name is ",temp,sep=''))
		colnames(out)[i] <- temp
	}
	
	
	
	return(out)
}

# Write data to the new opasnet database
opbase.upload <- function(input, ident = NULL, name = NULL, subset = NULL, obj_type = 'variable', act_type = 'replace', language = 'eng', unit = '', who = NULL, rescol = NULL, chunk_size = NULL, verbose = FALSE, username = NULL, password = NULL, index_units = NULL, index_types = NULL ) {
	
	args <- opbase.parse_args()
	
	if (is.null(ident))
	{
		if (! is.null(args$wiki_page_id)){
			ident <- args$wiki_page_id
		} else {
			stop('Ident missing!')
		}
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

	if (is.null(rescol)) stop('No result column could be defined!')
	if (verbose) print(paste('Rescol:',rescol, sep= ' '))
	
	dataframe <- dataframe[is.na(dataframe[,rescol]) == FALSE,]
	ColNames <- colnames(dataframe)[!(colnames(dataframe)%in%c(rescol, "id", "obs"))]
	
	ident <- tolower(ident)
	
	# Wiki id
	if (substr(ident, 1,5)=="op_en") {wiki_id <- 1; page <- substr(ident, 6, nchar(ident))} else {
		if (substr(ident, 1,5)=="op_fi") {wiki_id <- 2; page <- substr(ident, 6, nchar(ident))} else {
			if (substr(ident, 1,6)=="heande") {wiki_id <- 3; page <- substr(ident, 7, nchar(ident))} else {
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
		if (is.null(index_types))
		{
			t = 'entity'; 
		} else {
			t = index_types[i]
		}
		if (is.null(index_units))
		{
			u = ''; 
		} else {
			u = index_units[i]
		}	
		indices[[i]] = list(type=t,name=ColNames[i],page=page,wiki_id=wiki_id,order_index=i,hidden=0,unit=u) 
	}
	
	header <- list(
			object = list(
					name = opbase.ensure_utf8(name),
					ident = ident,
					type = obj_type,
					page = page,
					wiki_id = wiki_id
			),
			act = list(
					unit = opbase.ensure_utf8(unit),
					who = opbase.ensure_utf8(who),
					samples = n,
					comments = "R upload",
					language = language
			),
			indices = indices
	)
	
	if (! is.null(subset))
	{
		header[['object']][['subset_name']] <- opbase.ensure_utf8(subset)
		header[['object']][['ident']] <- paste(ident, opbase.sanitize_subset_name(subset), sep='.')
	}
		
	if (act_type == 'replace')
	{
		method <- 'POST';
	}
	if (act_type == 'append')
	{
		method <- 'PUT';
	}
	
	# Do some authentication!!!
	if (is.null(username))
	{
		if (! is.null(args$user))
		{	
			header[['username']] <- args$user
			header[['password']] <- opbase.hashed_password(opbase.read_auth(args$user),  ident=header[['object']][['ident']])
		}
	}
	else
	{
		if (! is.null(password))
		{	
			header[['username']] <- username
			header[['password']] <- opbase.hashed_password(password,  ident=header[['object']][['ident']])
		}
	}	
	
	json <- toJSON(header)
	
	if (verbose) print(json)
	
	data <- list('_method' = method, 'json' = json)
	
	response <- postToHost(server, path, data)
	
	if (is.null(response)) stop('Server is not responding!!!')

	if (verbose) print(response)
	
	# Parse JSON data from the server response
	response <- fromJSON(regmatches(response, regexpr('\\{.+\\}',response)))
	if (! is.null(response$error)) stop(response$error)
	if (is.null(response$key) || response$key == '') stop("Invalid upload key retrieved!")
	
	total_rows <- nrow(dataframe)
	
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
	
	if (chunk_size > total_rows) chunk_size <- total_rows
	
	start <- 1
	end <- chunk_size
	
	rows <- 0
	
	raw_data <- list('key' = response$key, 'indices' =  indices)

	# Do some authentication!!!
	if (is.null(username))
	{
		if (! is.null(args$user))
		{	
			raw_data[['username']] <- args$user
			raw_data[['password']] <- opbase.hashed_password(opbase.read_auth(args$user),  key=response$key)
		}
	}
	else
	{
		if (! is.null(password))
		{	
			raw_data[['username']] <- username
			raw_data[['password']] <- opbase.hashed_password(password,  key=response$key)
		}
	}	
	
	
	# Write the data
	repeat
	{
		data_chunk <- data.frame(lapply(dataframe[start:end,], as.character), stringsAsFactors=FALSE, check.names=FALSE)
		chunk_rows <- nrow(data_chunk)
		
		if (verbose) print(data_chunk)
		
		data_rows = list()
		# Create data list for JSON
		for (r in 1:chunk_rows)
		{
			row <- data_chunk[r,]
			
			v <- as.character(row[rescol])
			record <- list('res' = v)
			
			for (i in 1:length(ColNames))
			{
				v <- as.character(row[ColNames[i]])
				record[[toString(i)]] <- v
			}
			
			if (verbose) print(record)

			data_rows[[r]] <- record
		}
		
		#if (verbose) print(data_rows)
	
		raw_data[['data']] <- data_rows
		json <- toJSON(raw_data)
		
		if (verbose) print(json)
		
		data <- list('json' = json)
		response <- postToHost(server, path, data)
		
		if (is.null(response)) stop('Server is not responding!!!')
		
		if (verbose) print(response)
		
		# Parse JSON data from the server response
		response <- fromJSON(regmatches(response, regexpr('\\{.+\\}',response)))
		if (! is.null(response$error)) stop(response$error)
		
		if (verbose) print(response)
		
		rr <- as.integer(response$rows)
		
		if (rr != chunk_rows) stop(paste('Invalid inserted rows count! ', rr, ' vs ', chunk_rows, sep=''))
	
		rows <- (rows + rr)
		
		if (end >= total_rows) break
		
		start <- (end + 1)
		end <- (end + chunk_size)
		if (end > total_rows) end <- total_rows
	}
	
	return(rows)
	
}

# Private function to parse include or exclude list and return vector containing corresponding index idents and location ids
opbase.parse_locations <- function(locs, ident, series_id = NULL, username = NULL, password = NULL) {
	ret = c()
	
	#print(locs)
	
	for(i in names(locs))
	{
		query = list()
		query[['ident']] <- ident
		query[['index_name']] <- URLencode(i, reserved = TRUE)
		if (! is.null(series_id)) query[['series']] <- series_id
		object <- opbase.query(query, username, password)
		#ind <- object$index
		loc_ids = c()
		for (loc in locs[[i]])
		{
			# Seek thru all locations of an index
			for (lid in names(object$locations))
			{
				if (object$locations[[lid]] == loc)
				{
					loc_ids <- c(loc_ids, lid) 
					break
				}
			}
		}
		ret <- c(ret, paste(object$index$ident,paste(loc_ids,collapse=','),sep=','))
	}
	#print(ret)
	return(ret)
}

# Private function to get authentication
opbase.read_auth <- function(user) {
	auth <- fromJSON(paste(readLines("/var/www/html/rtools_server/offline/opasnet.json"),  collapse = ""))
	return(auth[[user]])
}

# Private function to create hashed password
opbase.hashed_password <- function(password, index = NULL, ident = NULL, key = NULL) {
	str <- ''
	if (! is.null(index)) str <- paste(str, index, sep='')
	if (! is.null(ident)) str <- paste(str, ident, sep='')
	if (! is.null(key)) str <- paste(str, key, sep='')
	str <- paste(str, password, sep='')
	return(digest(str, algo="md5", serialize=FALSE))
}

# Private function to make queries to server
opbase.query <- function(data, username = NULL, password = NULL) {

	args <- opbase.parse_args() 
	
	index <- NULL
	ident <- NULL
	key <- NULL
	
	if (! is.null(data[['index']])) index <- data[['index']]
	if (! is.null(data[['ident']])) ident <- data[['ident']]
	if (! is.null(data[['key']])) key <- data[['key']]
	
	# Do some authentication!!!
	if (is.null(username))
	{
		if (! is.null(args$user))
		{	
			data[['username']] <- args$user
			data[['password']] <- opbase.hashed_password(opbase.read_auth(args$user), index = index, ident = ident, key = key)
		}
	}
	else
	{
		if (! is.null(password))
		{	
			data[['username']] <- username
			data[['password']] <- opbase.hashed_password(password, index = index, ident = ident, key = key)
		}
	}
	
	# Build http-query key / value pairs
	tmp = c(1:length(data))
	i <-1
	for (k in names(data)){
		if (length(data[[k]]) > 1)
		{
			sepi <- paste(k,'[]=',sep='')
			tmp[i] <- paste(sepi, paste(data[[k]], collapse=paste("&", sepi, sep='')), sep='')
		} else {
			tmp[i] <- paste(k, '=', data[[k]], sep= '')
		}
		i <- i + 1
	}
	
	#print( paste(tmp, collapse='&') )
		
	url <- paste("http://cl1.opasnet.org/opasnet_base_2/index.php?", paste(tmp, collapse='&'), sep = "")
	
	response <- fromJSON(
			paste(
					readLines(url),  
					collapse = ""
			)
	)
	
	if (is.null(response))
	{
		stop("Opasnet server is not responding! Unable to query!")
	}
	
	if (! is.null(response$error))
	{
		stop(paste("Query response error: ",response$error, sep= ''))
	}
	
	return(response)
}

list.to.data.frame <- function(x) { # As.data.frame equivalent, but allows variable length lists, 
	do.call("data.frame", x) # which is the case with opb2's probabilistic results. 
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

	args <- opbase.parse_args()
	
	if (dsn == 'heande_base' && args$user != 'heande')
	{
		stop("Heande base is accessible only for the Heande Wiki!")
	}
	
	return(opbase.old.read(dsn, ident, ...))
}


# Write data to the old opasnet database
opbase.old.upload <- function(dsn, input, ...) {
	
	args <- opbase.parse_args()
	
	if (dsn == 'heande_base' && args$user != 'heande')
	{
		stop("Heande base is accessible only for the Heande Wiki!")
	}
	
	return(opbase.old.write(dsn, input, ...))
}

# Locations of an object in the old opasnet database
opbase.old.locations <- function(dsn, ident, ...) {
	
	args <- opbase.parse_args()
	
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

# Private function to parse arguments
opbase.parse_args <- function()
{
	# Parse arguments
	targs <- strsplit(commandArgs(trailingOnly = TRUE),",")
	args = list()
	
	if (length(targs) > 0)
		for(i in targs[[1]])
		{
			tmp = strsplit(i,"=")
			key <- tmp[[1]][1]
			value <- tmp[[1]][2]
			args[[key]] <- value
		}
	return(args)
}

# Private function to sanitize object subset data name 
opbase.sanitize_subset_name <- function(name)
{
	enc <- Encoding(name)
	# Make lowercase
	name <- tolower(name)
	# Remove all punctuation marks: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~.
	name <- gsub('[[:punct:] ]','_',name)
	# Convert to ASCII?
	if (enc != 'unknown') name <- iconv(name, enc, "ASCII//TRANSLIT","_")
	# Truncate multiple underscores
	name <- gsub('_+','_',name)
	# Remove trailing or leading underscores
	name <- gsub('^_|_$','',name)
	return(name)
}

opbase.ensure_utf8 <- function(str)
{
	enc <- Encoding(str)
	if (enc == 'UTF8' || enc == 'unknown') return(str)
	return(iconv(str, enc, "UTF8","?"))
}

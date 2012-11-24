############################################################3
# TIDY ########### 
#################################################
# Tidy is a function that cleans the tables from Opasnet Base. Intended to be used with table2base-uploaded
# tables, since they have the required Obs/Row - column.
# Inputs:
#   data - is a table from the base
#   objname - is the prefix given to the 'new' columns, defaults to empty
#   idvar - is a combination of the colnames that uniquely define a new row in the resulting dataframe. By default
#     'obs' to which 'Row' and 'Obs' are converted. 'Obs' is a column used by the table2base wiki table data upload
#     system to store the original row number. This can be used as such in reshape to bring out the original table.
#   direction - reshape paramater that tells that we are 'widening' the dataframe
#   widecol - the column which contains 'new' columns
tidy.old <- function (data, objname = "", idvar = "obs", direction = "wide", widecol = NULL) {
	data$Result <- ifelse(is.na(data$Result.Text), data$Result, as.character(data$Result.Text))
	#data <- data[
	#	ifelse("Observation" %in% colnames(data), 
	#		data$Observation != "Description",
	#		TRUE
	#	), 
	#	!colnames(data) %in% c("id", "Result.Text")
	#]
	data <- data[, !colnames(data) %in% c("id", "Result.Text")]
	if("obs.1" %in% colnames(data)) { # this line is temporarily needed until the obs.1 bug is fixed.
		data[, "obs"] <- data[, "obs.1"]
		data <- data[, colnames(data) != "obs.1"]
	} else if("Obs" %in% colnames(data)) {
		data[, "obs"] <- data[, "Obs"]
		data <- data[, colnames(data) != "Obs"]
	} else if("Row" %in% colnames(data)) { # If user has given Row, it is used instead of automatic obs.
		data <- data[, colnames(data) != "obs"]
		colnames(data)[colnames(data) == "Row"] <- "obs"
	}
	#if (objname != "") objname <- paste(objname, "", sep = "")
	if (direction == "wide") { 
		if(is.null(widecol)) {
			if("Observation" %in% colnames(data)) {
				widecol <- "Observation"
			} else if("Parameter" %in% colnames(data)) {
				widecol <- "Parameter"
			} else if("Havainto" %in% colnames(data)) {
				widecol <- "Havainto"
			} else widecol <- NA
		}
		if(!is.na(widecol)) {
			cols <- levels(data$Observation)
			data <- reshape(data, idvar = idvar, timevar = widecol, v.names = "Result", direction = "wide")
			data <- data[colnames(data) != "obs"]
			colnames(data) <- gsub("^Result.", objname, colnames(data))
			for (i in paste(objname, cols, sep = "")) {
				a <- suppressWarnings(as.numeric(as.character(data[, i])))
				if (sum(is.na(a)) == 0) data[, i] <- a else data[, i] <- factor(data[, i])
			}
			colnames(data)[grepl(paste("^", objname, "result", sep = ""), colnames(data))] <- paste(objname, "Result", sep = "")
			colnames(data)[grepl(paste("^", objname, "Amount", sep = ""), colnames(data))] <- paste(objname, "Result", sep = "")
			return(data)
		}
	}
	data <- data[,colnames(data) != "obs"]
	colnames(data)[colnames(data)=="Result"] <- paste(objname, "Result", sep = "")
	return(data)
}




tidy <- function (data, objname = "", idvar = "Obs", direction = "wide", widecol = NULL) {
	#data$Result <- ifelse(is.na(data$Result.Text), data$Result, as.character(data$Result.Text))
	#data <- data[
	#	ifelse("Observation" %in% colnames(data), 
	#		data$Observation != "Description",
	#		TRUE
	#	), 
	#	!colnames(data) %in% c("id", "Result.Text")
	#]
	#data <- data[, !colnames(data) %in% c("id", "Result.Text")]
	#if("obs.1" %in% colnames(data)) { # this line is temporarily needed until the obs.1 bug is fixed.
	#	data[, "obs"] <- data[, "obs.1"]
	#	data <- data[, colnames(data) != "obs.1"]
	#} else if("Obs" %in% colnames(data)) { # this line
	#	data[, "obs"] <- data[, "Obs"]
	#	data <- data[, colnames(data) != "Obs"]
	#} else if("Row" %in% colnames(data)) { # If user has given Row, it is used instead of automatic obs.
	#	data <- data[, colnames(data) != "obs"]
	#	colnames(data)[colnames(data) == "Row"] <- "obs"
	#}
	#if (objname != "") objname <- paste(objname, "", sep = "")
	if (direction == "wide") { 
		if(is.null(widecol)) {
			if("Observation" %in% colnames(data)) {
				widecol <- "Observation"
			} else if("Parameter" %in% colnames(data)) {
				widecol <- "Parameter"
			} else if("Havainto" %in% colnames(data)) {
				widecol <- "Havainto"
			} else widecol <- NA
		}
		if(!is.na(widecol)) {
			cols <- levels(data$Observation)
			data <- reshape(data, idvar = idvar, timevar = widecol, v.names = "Result", direction = "wide")
			data <- data[colnames(data) != "Obs"]
			colnames(data) <- gsub("^Result.", objname, colnames(data))
			for (i in paste(objname, cols, sep = "")) {
				a <- suppressWarnings(as.numeric(as.character(data[, i])))
				if (sum(is.na(a)) == 0) data[, i] <- a else data[, i] <- factor(data[, i])
			}
			colnames(data)[grepl(paste("^", objname, "result", sep = ""), colnames(data))] <- paste(objname, "Result", sep = "")
			colnames(data)[grepl(paste("^", objname, "Amount", sep = ""), colnames(data))] <- paste(objname, "Result", sep = "")
			return(data)
		}
	}
	data <- data[,colnames(data) != "Obs"]
	colnames(data)[colnames(data)=="Result"] <- paste(objname, "Result", sep = "")
	return(data)
}
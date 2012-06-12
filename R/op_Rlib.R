# APPLY.DECISIONS ############## apply.decisions takes a decision table and applies that to an assessment.
## dec is a decision data.frame that must have columns Decision, Option, Variable, Cell, Change, Result. It can have several variables.
apply.decisions <- function(assessment) {
	dec <- merge(assessment@decisions, assessment@dependencies, by.x = "Variable", by.y = "Name")
	colnames(dec)[colnames(dec) == "Result.x"] <- "Result"
	colnames(dec)[colnames(dec) == "Result.y"] <- "Name"

	for(variables in unique(dec$Name)) { # Take one variable at a time.
		dec.var <- dec[dec$Name == variables, ] # dec.var = variable-specific decisions
		scenarios <- data.frame(temp = 1)

		for(decisions in unique(dec.var$Decision)) { # Add BAU option to each decision and merge decisions.
			temp <- as.character(dec.var[dec.var$Decision == decisions, "Option"])
			temp <- data.frame(Options = c(temp, "BAU"))
			colnames(temp) <- decisions
			scenarios <- merge(scenarios, temp)
		}
		var <- assessment@vars[[variables]]# as.character(dec$Name[1])]]
		var@output <- merge(scenarios[colnames(scenarios) != "temp"], var@output)
		for(s in 1:nrow(dec.var)) { # Each option row handled separately
			cell <- gsub("^[ \t]+|[ \t]+$", "", as.character(dec.var$Cell[s])) # Leading and trailing whitespaces removed.
			cell <- gsub(":[ \t]+", ":", cell) # Whitespaces removed after :
			cell <- gsub(";[ \t]+", ";", cell) # Whitespaces removed after ;
			cell <- gsub("[ \t]+:", ":", cell) # Whitespaces removed before :
			cell <- gsub("[ \t]+;", ";", cell) # Whitespaces removed before ;
			cell <- strsplit(cell, split = ";") # Separate cell identifiers (indices and locations)
			cell <- strsplit(cell[[1]], split = ":")
			cond <- as.character(dec.var$Decision[s])
			cond <- var@output[, cond] == as.character(dec.var$Option[s]) # Only the rows with the relevant option.
			for(r in 1:length(cell)) { # All Cell conditions extracted and combined with AND.
				cond <- cond * (var@output[, cell[[r]][1]] == cell[[r]][2])
			}
			cond <- as.logical(cond)
			if(dec.var$Change[s] == "Replace" ) 
				{var@output[cond, "Result"] <- dec.var$Result[s]}
			if(dec.var$Change[s] == "Add"     ) 
				{var@output[cond, "Result"] <- as.numeric(dec.var$Result[s]) + var@output[cond, "Result"]}
			if(dec.var$Change[s] == "Multiply") 
				{var@output[cond, "Result"] <- as.numeric(dec.var$Result[s]) * var@output[cond, "Result"]}
		}
		assessment@vars[[variables]] <- var
		}
	return(assessment)
}

# DECISIONS.APPLY ############## decisions.apply takes a decision table and applies that to an assessment.
## dec is a decision data.frame that must have columns Decision, Option, Variable, Cell, Change, Result. It can have several variables.
decisions.apply <- function(dec, assessment = NULL) {
	out <- as.list(unique(dec$Variable))
	names(out) <- as.character(unique(dec$Variable))

	for(variables in unique(dec$Variable)) { # Take one variable at a time.
		dec.var <- dec[dec$Variable == variables, ] # dec.var = variable-specific decisions
		scenarios <- data.frame(temp = 1)

		for(decisions in unique(dec.var$Decision)) { # Add BAU option to each decision and merge decisions.
			temp <- as.character(dec.var[dec.var$Decision == decisions, "Option"])
			temp <- data.frame(Options = c(temp, "BAU"))
			colnames(temp) <- decisions
			scenarios <- merge(scenarios, temp)
		}

		if(!is.null(assessment)) {
			var <- assessment@vars[[as.character(dec.var$Variable[1])]]
		} else {
			if(exists(as.character(dec.var$Variable[1]))) 
				{var <- get(as.character(dec.var$Variable[1]))
			} else {
				stop()
			}
		}
		var <- merge(scenarios[colnames(scenarios) != "temp"], var)

		for(s in 1:nrow(dec.var)) { # Each row in decision handled separately
			cell <- gsub("^[ \t]+|[ \t]+$", "", as.character(dec$Cell[s])) # Leading and trailing whitespaces removed.
			cell <- gsub(":[ \t]+", ":", cell) # Whitespaces removed after :
			cell <- gsub(";[ \t]+", ";", cell) # Whitespaces removed after ;
			cell <- gsub("[ \t]+:", ":", cell) # Whitespaces removed before :
			cell <- gsub("[ \t]+;", ";", cell) # Whitespaces removed before ;
			cell <- strsplit(cell, split = ";") # Separate cell identifiers (indices and locations)
			cell <- strsplit(cell[[1]], split = ":")
			cond <- as.character(dec.var$Decision[s])
			cond <- var[, cond] == as.character(dec.var$Option[s]) # Only the rows with the relevant option.
			for(r in 1:length(cell)) { # All Cell conditions extracted and combined with AND.
				cond <- cond * (var[, cell[[r]][1]] == cell[[r]][2])
			}
			cond <- as.logical(cond)
			if(dec.var$Change[s] == "Replace" ) {var[cond, "Result"] <- dec.var$Result[s]}
			if(dec.var$Change[s] == "Add"     ) {var[cond, "Result"] <- dec.var$Result[s] + var[cond, "Result"]}
			if(dec.var$Change[s] == "Multiply") {var[cond, "Result"] <- dec.var$Result[s] * var[cond, "Result"]}
		}

		out[[variables]] <- var
		}
	return(out)
}

# DROPALL #####################################
## dropall pudottaa data.framesta pois kaikki faktorien sellaiset levelit, joita ei käytetä.
## parametrit: x = data.frame 

dropall <- function(x){
	isFac <- NULL
	for (i in 1:dim(x)[2]){
		isFac[i] = is.factor(x[ , i])
	}
	for (i in 1:length(isFac)){
		x[, i] <- x[, i][ , drop = TRUE]
	}
	return(x)
}

# FETCH ##################### fetch downloads a variable.

fetch <- function(x, direction = "wide") { # Could think of a version where dependencies table is given as a parameter, and based on Name, Identifier is downloaded from the Base.
	x <- as.character(x)
	if(exists(x)) {
		out <- get(x)
	} else {
		out <- tidy(op_baseGetData("opasnet_base", x), direction = "wide")
	}
	return(out)
}


# INIT.ASSESSMENT ########## init.assessment creates S4 assessment from dependencies data.frame, including decisions, stakeholders, probabilities, and variables. 
########### NOTE! You must include the formula code from each variable page, otherwise formulas and dependencies are not updated. 
########### Parameters:
## dependencies: a data.frame that has the structure of oassessment@name (Columns: Name, Identifier, Direction, Result)
init.assessment <- function(dependencies) 
{
	dependencies  <- fetch(dependencies)
	decisions     <- fetch(dependencies[dependencies$Result == "decisions", "Identifier"])
	stakeholders  <- fetch(dependencies[dependencies$Result == "stakeholders", "Identifier"])
	probabilities <- fetch(dependencies[dependencies$Result == "probabilities", "Identifier"])
	dependencies  <- dependencies[!dependencies$Result %in% c("decisions", "stakeholders", "probabilities"), ]
	vars          <- list()
	for(x in 1:nrow(dependencies)) { # Objects with names as aliases are created and filled with data from Opasnet Base.
		cat("Initialising variable ", as.character(dependencies$Name[x]), ".\n", sep = "")
		ident <- as.character(dependencies$Identifier[x])
		temp <- tidy(op_baseGetData("opasnet_base", ident), direction = as.character(dependencies$Direction[x]))
		temp <- make.ovariable(temp)
		if(exists(paste("formula.", ident, sep = ""))) 
			{temp@formula <- get(paste("formula.", ident, sep = ""))}
		if(exists(paste("dependencies.", ident, sep = "")))
			{temp@dependencies <- get(paste("dependencies.", ident, sep = ""))}
		vars[as.character(dependencies$Result[x])] <- temp
	}
	assessment <- new("oassessment", 
		dependencies  = dependencies, 
		decisions     = decisions,
		stakeholders  = stakeholders,
		probabilities = probabilities,
		vars          = vars
	)

	return(assessment)
# Does it cause problems to use the wide direction? Anyway, there should be exactly one Result column.
## No it doesn't if the Observation columns are such as Unit, Result, Description.
}

# INTERPRET ################### interpret takes a vector and makes a data.frame out of it (to be used in e.g. make.ovariable).
### It also changes abbreviations into probability samples.
interpret <- function(data) {
	sample <- NULL
	if(is.vector(data)) {data <- data.frame(Result = data)}
	if("Iter" %in% colnames(data)) {
		out <- data}
	else {
		if(!"Result" %in% colnames(data)) {cat("There MUST be an observation column named 'Result'.\n")}
		test <- !is.na(as.numeric(as.character(data$Result)))
	
		for(i in 1:nrow(data)) {
			if(test[i]) {
				sample <- c(sample, rep(as.numeric(as.character(data[i, "Result"])), n))
			} else {
				samplingguide <- as.numeric(strsplit(gsub(" ", "", data[i, "Result"]), "-")[[1]])
				if(is.na(samplingguide[1]) | is.na(samplingguide[2])) {
					sample <- c(sample, rep(data[i, "Result"], n))
				} else {
					sample <- c(sample, runif(n, samplingguide[1], samplingguide[2]))
				}
			}
		}
	
		out <- as.data.frame(array(1:(n*nrow(data)*(ncol(data)+1)), dim = c(n*nrow(data), ncol(data) + 1)))
		colnames(out) <- c("Iter", colnames(data))
	
		for(i in colnames(data)) {
			out[i] <- rep(data[, i], each = n)
		}
		out$Iter <- 1:n
		out$Result <- sample
		
	}
	return(out)
}

# MAKE.OASSESSMENT ########## make.oassessment creates S4 assessment from dependencies data.frame, including decisions, stakeholders, probabilities, and variables. 
########### NOTE! You must include the formula code from each variable page, otherwise formulas and dependencies are not updated. 
########### Parameters:
## dependencies: a data.frame that has the structure of oassessment@name (Columns: Name, Identifier, Direction, Result)
make.oassessment <- function(x) {
	x  <- fetch(x)
	decisions     <- fetch(x[x$Result == "decisions",     "Identifier"])
	stakeholders  <- fetch(x[x$Result == "stakeholders",  "Identifier"])
	probabilities <- fetch(x[x$Result == "probabilities", "Identifier"])
	dependencies  <- x[!x$Result %in% c("decisions", "stakeholders", "probabilities"), ]
	vars          <- list()
	for(i in 1:nrow(dependencies)) { # Objects with names as aliases are created and filled with data from Opasnet Base.
		cat("Initialising variable ", as.character(dependencies$Name[i]), ".\n", sep = "")
		ident <- as.character(dependencies$Identifier[i])
		data <- fetch(ident, direction = as.character(dependencies$Direction[i]))
		if(exists(paste("formula.", ident, sep = ""))) 
			{formula <- get(paste("formula.", ident, sep = ""))
		} else {
			formula <- function(dependencies) {return(0)}
		}
		if(exists(paste("dependencies.", ident, sep = "")))
			{depend <- get(paste("dependencies.", ident, sep = ""))
		} else {
			depend <- data.frame()
		}
		vars[[i]] <- make.ovariable(
			name = as.character(dependencies$Result[i]),
			data = data,
			formula = formula,
			dependencies = depend
		)
	}
	names(vars) <- dependencies$Result
	assessment <- new("oassessment", 
		dependencies  = dependencies, 
		decisions     = decisions,
		stakeholders  = stakeholders,
		probabilities = probabilities,
		vars          = vars
	)

	return(assessment)
}

# MOVARIABLE ########## movariable takes a data.frame, a function, and a list and makes an ovariable out of them. It is a 
#####subfunction of make.ovariable and prevents infinite recursion of S4 methods.
movariable <- function(
	data, 
	formula, 
	dependencies,
	name
) {
	output <- interpret(data)
	out <- new("ovariable", 
		name         = name,
		output       = output, 
		data         = data,
		marginal     = ifelse(colnames(output) %in% c("Result", "Unit"), FALSE, TRUE), 
		formula      = formula, 
		dependencies = dependencies
	)
	out <- update(out)
	return(out)
}

# OPASNET.DATA #####################################
## opasnet.data downloads a file from Finnish Opasnet wiki, English Opasnet wiki, or Opasnet File.
## Parameters: filename is the URL without the first part (see below), wiki is "opasnet_en", "opasnet_fi", or "M-files".
## If table is TRUE then a table file for read.table function is assumed; all other parameters are for this read.table function.

opasnet.data <- function(filename, wiki = "opasnet_en", table = FALSE, ...)
{
if (wiki == "opasnet_en") {
file <- paste("http://en.opasnet.org/en-opwiki/images/", filename, sep = "")
}
if (wiki == "opasnet_fi") {
file <- paste("http://fi.opasnet.org/fi_wiki/images/", filename, sep = "")
}
if (wiki == "M-files") {
file <- paste("http://http://fi.opasnet.org/fi_wiki/extensions/mfiles/", filename, sep = "")
}

#if(table == TRUE) {
#file <- re#ad.table(file, header = FALSE, sep = "", quote = "\"'",
#           dec = ".", row.names, col.names,
#           as.is = !stringsAsFactors,
#           na.strings = "NA", colClasses = NA, nrows = -1,
#           skip = 0, check.names = TRUE, fill = !blank.lines.skip,
#           strip.white = FALSE, blank.lines.skip = TRUE,
#           comment.char = "#",
#           allowEscapes = FALSE, flush = FALSE,
#           stringsAsFactors = default.stringsAsFactors(),
#           fileEncoding = "", encoding = "unknown")
#return(file)
#}
#else {return(ge#tURL(file))}
}

# ORBIND ########################### orbind combines two ovariables using clever rbind

orbind <- function(x, y) {
		if(class(x) == "ovariable") {xoutput <- x@output} else {xoutput <- x}
		if(class(y) == "ovariable") {youtput <- y@output} else {youtput <- y}
		cols <- setdiff(colnames(youtput), colnames(xoutput)) # Take all columns that do not exist in x and add them.
		col <- as.data.frame(array("NA", dim = c(1, length(cols))))
		colnames(col) <- cols
		if("Unit" %in% cols) {col[, "Unit"] <- "?"}
		temp <- cbind(xoutput, col)

		cols <- setdiff(colnames(xoutput), colnames(youtput)) # Take all columns that do not exist in y and add them.
		col <- as.data.frame(array("NA", dim = c(1, length(cols))))
		colnames(col) <- cols
		if("Unit" %in% cols) {col[, "Unit"] <- "?"}
		xoutput <- rbind(temp, cbind(youtput, col)) # Combine x and y with rbind.
		return(xoutput)
#Should this be made S4 function for ovariables? Then it could be named simply rbind.
	}

# PTABLE ########################################
## PTable muuntaa arvioinnin todennäköisyystaulun sopivaan muotoon arviointia varten.
## Parametrit: P = todennäköisyystaulu Opasnet-kannasta kaivettuna.
##             n = iteraatioiden lukumäärä Monte Carlossa
## Todennäköisyystaulun sarakkeiden on oltava: Muuttuja, Selite, Lokaatio, P
## Tuotteena on Monte Carloa varten tehty taulu, jonka sarakkeina ovat
## n (iteraatio) ja kaikki todennäköisyystaulussa olleet selitteet, joiden riveille on arvottu
## lokaatiot niiden todennäköisyyksien mukaisesti, jotka todennäköisyystaulussa oli annettu.

PTable <- function(P, n) {
Pt <- unique(P[,c("Muuttuja", "Selite")])
Pt <- data.frame(Muuttuja = rep(Pt$Muuttuja, n), Selite = rep(Pt$Selite, n), obs = rep(1:n, each = nrow(Pt)), P = runif(n*nrow(Pt), 0, 1))
for(i in 2:nrow(P)){P$Result[i] <- P$Result[i] + ifelse(P$Muuttuja[i] == P$Muuttuja[i-1] & P$Selite[i] == P$Selite[i-1], P$Result[i-1], 0)}
P <- merge(P, Pt)
P <- P[P$P <= P$Result, ]
Pt <- as.data.frame(as.table(tapply(P$Result, as.list(P[, c("Muuttuja", "Selite", "obs")]), min)))
colnames(Pt) <- c("Muuttuja", "Selite", "obs", "Result")
Pt <- Pt[!is.na(P$Result), ]
P <- merge(P, Pt)
P <- P[, !colnames(P) %in% c("Result", "P", "Muuttuja")]
P <- reshape(P, idvar = "obs", timevar = "Selite", v.names = "Lokaatio", direction = "wide")
colnames(P) <- ifelse(substr(colnames(P), 1, 9) == "Lokaatio.", substr(colnames(P), 10,30), colnames(P))
return(P)
}

# SETCLASS OASSESSMENT ################### Defines the S4 class "oassessment" which is the object type for open assessments.
setClass(
	"oassessment", 
	representation(
		dependencies  = "data.frame", 
		decisions     = "data.frame",
		probabilities = "data.frame",
		stakeholders  = "data.frame",
		vars          = "list"
	)
)

# SETCLASS OVARIABLE ################### Defines the S4 class "ovariable" which is the basic building block in open assessments.
setClass(
	"ovariable", 
	representation(
		name         = "character",
		output       = "data.frame", 
		data         = "data.frame", 
		marginal     = "vector", 
		formula      = "function", 
		dependencies = "data.frame"
	)
)

# SETMETHOD CONVERT.UNITS####################################################3
############# convert.units: a function that converts units from one to another, if possible.
#    x         = a numeric vector with values to be converted
#    tounit    = a character vector of the new units to be used. Must be found from the To column from the table in [[Unit conversions]].
#    fromunit  = a character vector or factor with the current units
convert.units <- function(x, tounit = c("kg", "s", "m", "m3", "J", "W", "A", "V", "C", "N", "Pa", "Hz", "mol"), fromunit = NULL) {
	if(is.null(tounit)) {tounit <- c("kg", "s", "m", "m3", "J", "W", "A", "V", "C", "N", "Pa", "Hz", "mol")}
	if(is.null(fromunit)) { # Do nothing if from-units are not defined.
		out <- data.frame(Unit = fromunit, Result = x)
	} else {
		conversions <- fetch("Op_en5475") # Set up a full unit conversion table.
		conversions$Result <- as.numeric(conversions$Result)
		colnames(conversions)[colnames(conversions) == "type"] <- "Type"
		prefixes <- conversions[conversions$Type == "Prefix", ] # Combine all prefixes with all units in From column.
		colnames(prefixes) <- paste("Prefix.", colnames(prefixes), sep = "")
		conversions <- merge(prefixes, conversions[conversions$Type == "Unit", ])
		conversions$From <- paste(conversions$Prefix.From, conversions$From, sep = "")
		conversions$Result <- conversions$Result * conversions$Prefix.Result
		conversions <- conversions[c("From", "To", "Result")]
		conversions <- merge(conversions, conversions, by = "To") # Create all possible from-to pairs.
		conversions$Result <- conversions$Result.x / conversions$Result.y
		conversions <- unique(conversions[c("From.x", "From.y", "Result")])
		colnames(conversions) <- c("From", "To", "Result")
		coefficients <- data.frame()
		for(i in levels(as.factor(fromunit))) { # Look through each different "From" unit in the data.
			coefficient <- 1
			outto <- ""
			for(j in strsplit(i, split = " ")[[1]]) { # Look though each part of a composite unit.
				if(gsub("^/", "", j) != j) { # If unit is in denominator, use inverse.
					j <- gsub("^/", "", j)
					exponent <- -1
					slash <- "/"
				} else {
					exponent <- 1
					slash <- ""
				}
				conversions.j <- merge(conversions, j, by.x = "From", by.y = "y")
				conversions.j <- conversions.j[conversions.j$To %in% tounit, ]
				if(nrow(conversions.j) > 0) {
					coefficient <- coefficient * conversions.j$Result^exponent
					outto <- paste(outto, " ", slash, conversions.j$To, sep = "")
				}
			}
			coefficients <- rbind(coefficients, data.frame(
				From = i,
				To = gsub("^ ", "", outto), 
				Result = coefficient
			))
		}
		# Combine coefficients with data.
		out <- merge(fromunit, coefficients, by.x = "x", by.y = "From", sort = FALSE)
		out <- data.frame(Unit = out$To, Result = x * out$Result)
	}
	return(out)
}

setGeneric("convert.units")

setMethod(
	f = "convert.units",
	signature = signature(x = "ovariable"),
	definition = function(x, tounit)
	{
		if("Unit" %in% colnames(x@output) & !is.null(tounit)) {
			x@output[c("Unit", "Result")] <- convert.units(x = x@output$Result, tounit = tounit, fromunit = x@output$Unit, expo = expo)
		}
		return(x)
	}
)

setMethod(
	f = "convert.units",
	signature = signature(x = "data.frame"),
	definition = function(x, tounit = NULL)
	{
		if("Unit" %in% colnames(x) & "Result" %in% colnames(x) & !is.null(tounit)) {
			x[c("Unit", "Result")] <- convert.units(x = x$Result, tounit = tounit, fromunit = x$Unit, expo = expo)
		}
		return(x)
	}
)


# SETMETHOD MAKE.OVARIABLE ################################################################################
########### make.ovariable takes a vector or data.frame and makes an ovariable out of it.
make.ovariable <- function(
	data, 
	formula = function(dependencies){return(0)}, 
	dependencies = list(x = 0),
	name = ""
) {
	return(movariable(data, formula, dependencies, name))
}

setGeneric("make.ovariable") # Makes make.ovariable a generic S4 function.

setMethod(
	f = "make.ovariable",
	signature = signature(data = "data.frame"),
	definition = function(
		data, 
		formula = function(dependencies){return(0)}, 
		dependencies = data.frame(),
		name = ""
	) {
cat("Data frame\n")
		return(movariable(data, formula, dependencies, name))
	}
)

setMethod(
	f = "make.ovariable",
	signature = signature(data = "vector"),
	definition = function(
		data, 
		formula = function(dependencies){return(0)}, 
		dependencies = data.frame(),
		name = ""
	) {
cat("Vector\n")
		data <- data.frame(Result = data)
		return(make.ovariable(data, formula, dependencies, name))
	}
)

setMethod(
	f = "make.ovariable",
	signature = signature(data = "list"),
	definition = function(
		data, 
		formula = function(dependencies){return(0)}, 
		dependencies = data.frame()
	) {
		for(i in 1:length(data)) {
cat("List", i, "\n")
			data[[i]] <- make.ovariable(data[[i]], formula, dependencies, name = names(data)[[i]])
		}
		return(data)
	}
)

setMethod(
	f = "make.ovariable",
	signature = signature(data = "ovariable"),
	definition = function(
		data, 
		formula = NULL, 
		dependencies = NULL,
		name = NULL
	) {
cat("ovariable\n")
		if(is.null(formula))      {formula      <- data@formula}
		if(is.null(dependencies)) {dependencies <- data@dependencies}
		if(is.null(formula))      {name         <- data@name}
		return(movariable(data@data, formula, dependencies, name))
	}
)

# SETMETHOD MATH ################### Math defines basic mathematical operations (log, exp, abs, ...) for ovariables
setMethod(
	f = "Math", 
	signature = signature(x = "ovariable"), 
	definition = function(x) {
		x@output$Result <- callGeneric(x@output$Result)
		return(x)
	}
)

# SETMETHOD MERGE ########### merge of ovariables merges the 'output' slot by index columns except 'Unit'.
setMethod(f = "merge", 
	signature = signature(x = "ovariable", y = "ovariable"),
	definition = function(x, y) {
		test <- intersect(colnames(x@output[x@marginal]), colnames(y@output[y@marginal]))
		x@output <- merge(x@output, y@output, by = test, all = TRUE)
		return(x)
	}
)

setMethod(f = "merge", 
	signature = signature(x = "ovariable", y = "numeric"),
	definition = function(x, y) {
		y <- make.ovariable(y)
		return(merge(x, y))
	}
)

setMethod(f = "merge", 
	signature = signature(x = "numeric", y = "ovariable"),
	definition = function(x, y) {
		x <- make.ovariable(x)
		return(merge(y, x))
	}
)

# SETMETHOD OPS ######### Arithmetic operations of ovariables: first they are merged by index columns,
### then the operation is performed for the Result.x and Result.y columns.
### If one of the expressions is numeric, it is first transformed to ovariable.
setMethod(
	f = "Ops", 
	signature = signature(e1 = "ovariable", e2 = "ovariable"), 
	definition = function(e1, e2) {
		out <- merge(e1, e2)@output
		colnames(out) <- gsub(".x", "", colnames(out))
		out$Result <- callGeneric(out$Result, out$Result.y)
		if(!is.null(out$Unit.y)) {out$Unit <- paste(out$Unit, "|(", out$Unit.y, ")", sep= "")}
		e1@output <- out[, !colnames(out) %in% c("Result.y", "Unit.y")]
		return(e1)
	}
)

setMethod(
	f = "Ops", 
	signature = signature(e1 = "ovariable", e2 = "numeric"), 
	definition = function(e1, e2) {
		e2 <- make.ovariable(e2)
		e1 <- callGeneric(e1, e2)
		return(e1)
	}
)

setMethod(
	f = "Ops", 
	signature = signature(e1 = "numeric", e2 = "ovariable"), 
	definition = function(e1, e2) {
		e1 <- make.ovariable(e1)
		e1 <- callGeneric(e2, e1)
		return(e1)
	}
)

# SETMETHOD PLOT ################ plot diagrams about ovariable data

setMethod(
	f = "plot",
	signature = signature(x = "ovariable"),
	definition = function(x) {
		plot(
			x    = x@output[, paste("Source", x@name, sep = ".")], 
			y    = x@output$Result, 
			xlab = paste("Source", x@name, sep = "."), 
			ylab = x@output[x@output[, paste("Source", x@name, sep = ".")] == "Data", "Unit"][1], 
			main = x@name
		)
	}
)

setMethod(
	f = "plot",
	signature = signature(x = "oassessment"),
	definition = function(x) {
		for(i in names(x@vars)) {
			plot(x@vars[[i]])
		}
	}
)

# SETMETHOD PRINT ######################## print ovariable contents

setMethod(
	f = "print",
	signature = signature(x = "oassessment"),
	definition = function(x) {
		cat("Dependencies\n")
		print(xtable(x@dependencies), type = 'html')
		cat("Decisions\n")
		print(xtable(x@decisions), type = 'html')
		cat("Stakeholders\n")
		print(xtable(x@stakeholders), type = 'html')
		cat("Probabilities\n")
		print(xtable(x@probabilities), type = 'html')

		cat("\n\nThe list of variables in this assessment.\n")
		for(k in 1:length(x@vars)) {
			cat("<b>Variable", names(x@vars)[k], "</b> <p>\n\n")
			print(x@vars[[k]])
		}
	}
)

setMethod(
	f = "print",
	signature = signature(x = "ovariable"),
	definition = function(x, iter = 1) {
		cat("<b>Ovariable", x@name, "</b>\n")
		cat("Output\n")
		print(xtable(x@output[x@output$Iter %in% iter, ]), type = 'html')
		cat("Formula\n")
		print(x@formula)
		cat("Dependencies\n")
		print(x@dependencies)
	}
)

# SETMETHOD UPDATE ########## update updates the output of an ovariable based on data and function.
####### The function assumes that object@data is always available, but object@dependencies may be empty.
setMethod(
	f = "update", 
	signature = "ovariable", 
	definition = function(object) {
		dat  <- data.frame(Source = "Data", interpret(object@data))
		if(ncol(object@dependencies) == 0 & nrow(object@dependencies) == 0) {
			object@output <- dat
		} else {
			form <- object@formula(object@dependencies)
			if(is.vector(form)) {form <- data.frame(Result = form)}
			if(class(form) == "ovariable") {form <- form@output}
			form <- data.frame(Source = "Formula", form)
			object@output <- orbind(dat, form)
		}
		colnames(object@output)[colnames(object@output) == "Source"] <- paste("Source", object@name, sep = ".")
		object@marginal <- c(TRUE, object@marginal) # This alone is not enough; orbind must operate with marginals as well.
		return(object)
	}
)


#### This code was taken out of update. It is still needed somewhere but not here.
#		dep <- object@dependencies
#		for(i in 1:length(dep)) {
#			if(class(dep[[i]]) == "ovariable") {
#				dep[[i]] <- dep[[i]]@output
#			} else {
#				if(length(grep("Op_(en|fi)", dep[[i]])) > 0) {
#					dep[[i]] <- op_baseGetData("opasnet_base", dep[[i]])}
#				else {
#					if(class(dep[[i]]) != "data.frame" & !is.numeric(dep[[i]])) {
#						dep[[i]] <- get(dep[[i]])
#					}
#				}
#			}
#		}


# SUMMARY.BRING ############## summary.bring: Bring parts of summary table
#   page is the page identifier for the summary table.
summary.bring <- function(page, base = "opasnet_base"){
	data <- tidy(op_baseGetData(base, page))
	pages <- levels(data$Page)

	## temp contains the additional information that is not on the actual data table.
	temp <- data[, !colnames(data) == "Observation"]
	temp <- reshape(temp, idvar = "Page", timevar = "Index", direction = "wide")
	colnames(temp) <- ifelse(substr(colnames(temp), 1, 7) == "Result.", substr(colnames(temp), 8, 50), colnames(temp))

	## Get all data tables one at a time and combine them.
	for(i in 1:length(pages)){
		out <- op_baseGetData("opasnet_base", pages[i])
		out <- tidy(out)
		cols <- colnames(out)[!colnames(out) %in% c("Observation", "Result")]
		out <- reshape(out, timevar = "Observation", idvar = cols, direction = "wide")
		colnames(out) <- ifelse(substr(colnames(out), 1, 7) == "Result.", substr(colnames(out), 8, 50), colnames(out))
		out <- merge(temp[temp$Page == pages[i], ][colnames(temp) != "Page"], out)

		## Check that all data tables have all the same columns before you combine them with rbind.
		if(i == 1){out2 <- out} else {
			addcol <- colnames(out2)[!colnames(out2) %in% colnames(out)]
			if(length(addcol) > 0) {
				temp <- as.data.frame(array("*", dim = c(1,length(addcol))))
				colnames(temp) <- addcol
				out <- merge(out, temp)}
			addcol <- colnames(out)[!colnames(out) %in% colnames(out2)]
			if(length(addcol) > 0) {
				temp <- as.data.frame(array("*", dim = c(1,length(addcol))))
				colnames(temp) <- addcol
				out2 <- merge(out2, temp)}

			## Combine data tables.
			out2 <- rbind(out2, out)}
		}
		return(out2)
	}
##########

# TAPPLY ########### tapply of ovariables applies a function to each cell of a ragged array, that is to each (non-empty) group of
############ values given by a unique combination of the levels of certain factors.
### parameters (other parameters are as in generic tapply):
### X an ovariable

setMethod(f = "tapply", 
	signature = signature(X = "ovariable"),
	definition = function(X, INDEX, FUN = NULL, ..., simplify = TRUE) {
		out <- as.data.frame(as.table(tapply(X@output$Result, INDEX, FUN, ..., simplify = TRUE)))
		colnames(out)[colnames(out) == "Freq"] <- "Result"
		X@output <- out
		return(X)
	}
)

# TIDY ########### tidy: a function that cleans the tables from Opasnet Base
# data is a table from op_baseGetData function
tidy <- function (data, idvar = "obs", direction = "wide") {

	data$Result <- ifelse(!is.na(data$Result.Text), as.character(data$Result.Text), data$Result)
	if("Observation" %in% colnames(data)){test <- data$Observation != "Description"} else {test <- TRUE}
	data <- data[test, !colnames(data) %in% c("id", "Result.Text")]
	if("obs.1" %in% colnames(data)) {data[, "obs"] <- data[, "obs.1"]} # this line is temporarily needed until the obs.1 bug is fixed.
	data <- data[colnames(data) != "obs.1"]
	if("Row" %in% colnames(data)) { # If user has given Row, it is used instead of automatic obs.
		data <- data[, colnames(data) != "obs"]
		colnames(data)[colnames(data) == "Row"] <- "obs"
	}
	if(direction == "wide" & "Observation" %in% colnames(data)) 
	{
		data <- reshape(data, idvar = idvar, timevar = "Observation", v.names = "Result", direction = "wide")
		data <- data[colnames(data) != "obs"]
		colnames(data) <- gsub("^Result.", "", colnames(data))
		colnames(data)[colnames(data) == "result"] <- "Result"
		colnames(data)[colnames(data) == "Amount"] <- "Result"
	}
	else
	{
		data <- data[colnames(data) != "obs"]
	}
	return(data)
}

# FETCH ##################### fetch downloads a variable.

fetch <- function(x, direction = "wide") { # Could think of a version where dependencies table is given as a parameter, and based on Name, Identifier is downloaded from the Base.
	x <- as.character(x)
	if(exists(x)) {
		out <- get(x)
	} else {
		out <- tidy(op_baseGetData("opasnet_base", x), direction = "wide")
	}
	return(out)
}


# Fetch2 #################### loads all given dependencies to global memory

Fetch2 <- function(dependencies, evaluate = FALSE) {
	for (i in 1:nrow(dependencies)) {
		if(!exists(dependencies$Name[i])) {
			objects.get(dependencies$Key[i]) # Key is the R-tools session identifier (seen at the end of the url)
			if (evaluate) get(dependencies$Name[i])@output <- EvalOutput(get(dependencies$Name[i])) 
			# Eval not necessarily needed at this point
		}
	}
}

#library(reshape2)
#library(reshape) # for older version

# EvalOutput #################### evaluates the output slot of ovariables

EvalOutput <- function(variable, ...) {
	#if (nrow(variable@data) > 0) { # if interpret can handle zero rows, no problem
	a <- interpret(variable@data, ...)
	colnames(a)[colnames(a) == "Result"] <- "Data"
	#}
	b <- variable@formula(variable@dependencies)
	colnames(b)[colnames(b) == "Result"] <- "Formula"
	if (b == 0) return(a)
	if (nrow(variable@data)==0) return(b)
	return(
		melt(
			merge(a, b, all = TRUE, ...), 
			measure.vars = c("Data", "Formula"), 
			variable.name = "Source", 
			value.name = "Result",
			...
		)
	)
}


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
# Lognormal distribution parametrization functions
lmean <- function(parmean, parsd) {return(log(parmean)-log(1+(parsd^2)/(parmean^2))/2)}
lsd <- function(parmean, parsd) {return(log(1+(parsd^2)/(parmean^2)))}

# Actual interpretation function. Takes already pre-processed information and returns a distribution.
interpf <- function(
	n, 
	res.char, 
	brackets.pos, 
	brackets.length, 
	minus, 
	minus.length, 
	minus.exists, 
	plusminus, 
	plusminus.length, 
	plusminus.exists,
	doublePoint
	) {

	if(doublePoint[1] > 0) {
		tempArgs <- sort(as.numeric(unlist(strsplit(res.char, "\\:"))))
		return(rtriangle(n,tempArgs[1],tempArgs[3],tempArgs[2]))
	}
	if(brackets.pos >= 0) {
		minus.relevant <- unlist(minus)[(cumsum(c(0, minus.length)) + 1):cumsum(minus.length)]
		n.minus.inside.brackets <- sum(minus.relevant > brackets.pos & minus.relevant < brackets.pos + brackets.length)
		imean <- as.numeric(substr(res.char, 1, brackets.pos - 1))
		if(n.minus.inside.brackets == 1) {
			ici <- c(as.numeric(substr(res.char, brackets.pos + 1, minus.relevant[minus.relevant > brackets.pos] - 1)), as.numeric(substr(res.char, 
				minus.relevant[minus.relevant > brackets.pos] + 1, brackets.pos + brackets.length - 2)))
			isd <- sum(abs(ici - imean) / 2) / qnorm(0.975)
			if((ici[2] - imean) / (ici[1] - imean) < 1.5) {
				return(rnorm(n, imean, isd))
			} else {
				return(out[[i]] <- rlnorm(n, lmean(imean, isd), lsd(imean, isd))) # menee vaarin koska isd on laskettu normaalijakaumalle
			}
		} else 
		if(n.minus.inside.brackets %in% c(2,3)) {
			ici <- c(as.numeric(substr(res.char, brackets.pos + 1, minus.relevant[minus.relevant > brackets.pos][2] - 1)), as.numeric(substr(res.char, 
				minus.relevant[minus.relevant > brackets.pos][2] + 1, brackets.pos + brackets.length - 2)))
			isd <- sum(abs(ici - imean) / 2) / qnorm(0.975)
			return(rnorm(n, imean, isd))
		}
		warning(paste("Unable to interpret \"", res.char, "\"", sep = ""))
		return(NA)
	}
	if(minus.exists) {
		minus.relevant <- unlist(minus)[(cumsum(c(0, minus.length)) + 1):cumsum(minus.length)]
		if(length(minus.relevant) == 1) {
			if(as.numeric(substr(res.char, 1, minus.relevant - 1)) / as.numeric(substr(res.char, minus.relevant + 1, nchar(res.char))) >= 1/100) {
				return(runif(n, as.numeric(substr(res.char, 1, minus.relevant - 1)), as.numeric(substr(res.char, minus.relevant + 1, nchar(res.char[i])))))
			} else {
				return(exp(runif(n, log(as.numeric(substr(res.char, 1, minus.relevant - 1))), log(as.numeric(substr(res.char, minus.relevant + 1, nchar(res.char)))))))
			}
		}
		if(length(minus.relevant) %in% c(2,3)) {
			return(runif(n, as.numeric(substr(res.char, 1, minus.relevant[2] - 1)), as.numeric(substr(res.char, minus.relevant[2] + 1, nchar(res.char)))))
		}
	}
	if(plusminus.exists) {
		return(rnorm(n, as.numeric(substr(res.char, 1, plusminus[1] - 1)), as.numeric(substr(res.char, plusminus[1] + 1, nchar(res.char)))))
	}
	if(sum(unlist(strsplit(res.char, ""))==";") > 0) {
		return(sample(sapply(strsplit(res.char, ";"), as.numeric), N, replace = TRUE))
	}
	warning(paste("Unable to interpret \"", res.char, "\"", sep = ""))
	return(NA)
}

# The next function processes character strings and loops the interpretation function.
input.interp <- function(res.char, n = 1000) {
	res.char <- gsub(" ", "", res.char)
	res.char <- gsub(",", ".", res.char)
	plusminus <- gregexpr(paste("\\+-|", rawToChar(as.raw(177)), sep = ""), res.char) # saattaa osoittautua ongelmaksi enkoodauksen vuoksi
	plusminus.length <- sapply(plusminus, length)
	plusminus.exists <- unlist(plusminus)[cumsum(c(0, plusminus.length[-length(plusminus.length)])) + 1] > 0
	minus <- gregexpr("-", res.char)
	minus.length <- sapply(minus, length)
	minus.exists <- unlist(minus)[cumsum(c(0, minus.length[-length(minus.length)])) + 1] > 0
	brackets <- gregexpr("\\(.*\\)", res.char) # matches for brackets "(...)"
	brackets.length <- as.numeric(unlist(sapply(brackets, attributes)[1,]))
	brackets.pos <- unlist(brackets)
	doublePoint <- gregexpr(":", res.char)
	out <- list()
	for(i in 1:length(res.char)) {
		out[[i]] <- interpf(n, res.char[i], brackets.pos[i], brackets.length[i], minus[i], minus.length[i], minus.exists[i], plusminus[[i]], 
	plusminus.length[i], plusminus.exists[i],doublePoint[[i]])
	}
	out
}

# Assisting function for data.frame wrapper.
iter.f <- function(x) {
	1:x
}

# Data.frame wrapper for the functions.
interpret <- function(idata, rescol = "Result", N = 1000) {

	temp <- input.interp(idata[, rescol], N)
	temp.lengths <- sapply(temp, length)
	out <- idata[rep(1:nrow(idata), times = temp.lengths),]
	out$Interp.Result <- unlist(temp)
	dim(temp.lengths) <- length(temp.lengths)
	out$Iter<- c(apply(temp.lengths, 1, iter.f))
	out
}

setGeneric("interpret")

setMethod(
	f = "interpret",
	signature = signature(idata = "character"),
	definition = function(idata) {
		if(!is.data.frame){
			callGeneric(data.frame(Result = idata))
			}
			callGeneric(idata)
	}
)

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
		name			= "character",
		output			= "data.frame", 
		data			= "data.frame", 
		marginal		= "logical", 
		formula			= "function", 
		dependencies	= "data.frame"
	),
	prototype = prototype(
		name = character(),
		output = data.frame(),
		data = data.frame(),
		marginal = logical(),
		formula = function(...){0},
		dependencies = data.frame()
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
# TIDY ########### tidy: a function that cleans the tables from Opasnet Base
# data is a table from op_baseGetData function
tidy <- function (data, objname = "", idvar = "obs", direction = "wide") {
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
	}
	if("Row" %in% colnames(data)) { # If user has given Row, it is used instead of automatic obs.
		data <- data[, colnames(data) != "obs"]
		colnames(data)[colnames(data) == "Row"] <- "obs"
	}
	if (objname != "") objname <- paste(objname, ":", sep = "")
	if (direction == "wide") { 
		if("Observation" %in% colnames(data)) {
			cols <- levels(data$Observation)
			data <- reshape(data, idvar = idvar, timevar = "Observation", v.names = "Result", direction = "wide")
			data <- data[colnames(data) != "obs"]
			colnames(data) <- gsub("^Result.", objname, colnames(data))
			for (i in paste(objname, cols, sep = "")) {
				a <- suppressWarnings(as.numeric(data[, i]))
				if (sum(is.na(a)) == 0) data[, i] <- a else data[, i] <- factor(data[, i])
			}
			colnames(data)[grepl(paste("^", objname, "result", sep = ""), colnames(data))] <- paste(objname, "Result", sep = "")
			colnames(data)[grepl(paste("^", objname, "Amount", sep = ""), colnames(data))] <- paste(objname, "Result", sep = "")
			return(data)
		}
		if("Parameter" %in% colnames(data)) {
			cols <- levels(data$Parameter)
			data <- reshape(data, idvar = idvar, timevar = "Parameter", v.names = "Result", direction = "wide")
			data <- data[colnames(data) != "obs"]
			colnames(data) <- gsub("^Result.", objname, colnames(data))
			for (i in paste(objname, cols, sep = "")) {
				a <- suppressWarnings(as.numeric(data[, i]))
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
	if (nrow(dependencies) > 0) {
		for (i in 1:nrow(dependencies)) {
			if(!exists(as.character(dependencies$Name[i]))) {
				objects.get(dependencies$Key[i]) # Key is the R-tools session identifier (shown at the end of the url)
				if (evaluate) get(as.character(dependencies$Name[i]))@output <- EvalOutput(get(as.character(dependencies$Name[i]))) 
				# Eval not necessarily needed at this point
				cat(as.character(dependencies$Name[i]), "fetched successfully!\n")
			}
		}
	}
} # no need to return anything since variables are already written in global memory

#library(reshape2)
#library(reshape) # for older version

# EvalOutput #################### evaluates the output slot of ovariables
##### Marginals should be also checked and updated here or elsewhere

EvalOutput <- function(variable, ...) { # ... for e.g na.rm 
	if (nrow(variable@data) > 0) {
		rescol <- ifelse(
			"Result" %in% colnames(variable@data), 
			"Result", 
			paste(variable@name, "Result", sep = ":")
		)
		if (!is.numeric(variable@data[[rescol]])) {
			a <- interpret(variable@data, rescol = rescol, ...) 
		} else a <- variable@data
	} else a <- variable@data
	b <- variable@formula(variable@dependencies)
	if (is.numeric(b) & nrow(variable@data) == 0) {
		stop(paste("No proper data nor formula defined for ", variable@name, "!\n", sep = ""))
	}
	if (is.numeric(b)) {
		a[,paste(variable@name, "Source", sep = ":")] <- "Data"
		variable@output <- a
		return(variable)
	}
	if (nrow(variable@data) == 0) {
		b[,paste(variable@name, "Source", sep = ":")] <- "Formula"
		variable@output <- b
		return(variable)
	}
	colnames(a)[colnames(a) == rescol] <- "FromData"
	colnames(b)[colnames(b) %in% c(paste(variable@name, "Result", sep = ":"), "Result")] <- "FromFormula" # *
	# <variablename>: prefix not necessitated for "Result" column of formula output
	temp <- melt(
		merge(a, b, all = TRUE, ...), # Will cause problems if dependencies contain non-marginal indices that match with -
		# marginal indeces in data. Or maybe not.
		measure.vars = c("FromData", "FromFormula"),
		variable.name = paste(variable@name, "Source", sep = ":"),
		value.name = paste(variable@name, "Result", sep = ":"),
		...
	)
	levels(
		temp[[paste(variable@name, "Source", sep = ":")]]
	) <- gsub("^From", "", 
		levels(
			temp[[paste(variable@name, "Source", sep = ":")]]
		)
	)
	variable@output <- temp
	return(variable)
}

# CheckMarginals ############# Assumes that all depended upon variables are in memory, as should be the case.
##################
# Returns an avariable with a marginal devised from the data and upstream variable marginals. 
# Marginal values for data should be stored into the database somehow

CheckMarginals <- function(variable) {
	varmar <- colnames(variable@data)[
		!grepl(paste("^", variable@name, ":", sep=""), colnames(variable@data))&
		!colnames(variable@data) %in% c("Result", "Unit")
	]
	# all locs under observation/parameter index should be excluded
	varmar <- c(varmar, paste(variable@name, "Source", sep = "_")) # Source is usually added 
	# by EvalOutput so it should be in the initial list by default. 
	novarmar <- colnames(variable@data)[!colnames(variable@data) %in% varmar]
	for (i in as.character(variable@dependencies$Name)){
		varmar <- unique(varmar, colnames(get(i)@output)[get(i)@marginal])
		novarmar <- unique(novarmar, colnames(get(i)@output)[!get(i)@marginal])
	}
	varmar <- varmar[!varmar %in% novarmar]
	variable@marginal <- colnames(variable@output) %in% varmar
	return(variable)
}

#marginal <- ifelse(colnames(output) %in% c("Result", "Unit"), FALSE, TRUE)

# CheckInput ################# checks and uses outside input (user inputs in models or decision variables)
# takes an ovariable as argument
# returns an ovariable

CheckInput <- function(variable, substitute = FALSE, ...) { # ... e.g for na.rm
	if (nrow(variable@output) == 0) stop(paste(variable@name, "output not evaluated yet!"))
	if (exists(paste("Inp", variable@name, sep = ""))) {
		inputvar <- get(paste("Inp", variable@name, sep = ""))
		if (substitute) {
			colnames(inputvar@output)[colnames(inputvar@output) == paste(inputvar@name, "Result", sep = ":")] <- "InpVarRes"
			colnames(variable@output)[colnames(variable@output) == paste(variable@name, "Result", sep = ":")] <- "VarRes"
			finalvar <- merge(variable, inputvar)
			finalvar@output[[paste(variable@name, "Result", sep = ":")]] <- ifelse(
				is.na(finalvar@output$InpVarRes), 
				finalvar@output$VarRes, 
				finalvar@output$InpVarRes
			)
			finalvar@output[[paste(variable@name, "Source", sep = ":")]] <- ifelse(
				is.na(finalvar@output$InpVarRes), 
				finalvar@output[[paste(variable@name, "Source", sep = ":")]], 
				"Input"
			)
			finalvar@output <- finalvar@output[!colnames(finalvar) %in% c("InpVarRes", "VarRes")]
			return(finalvar)
		}
		#variable@output[variable@output$Source,]
		j <- levels(variable@output[[paste(variable@name, "Source", sep = ":")]])
		temp <- variable@output[
			variable@output[,paste(variable@name, "Source", sep = ":")] == j[1], 
			!colnames(variable@output) %in% paste(variable@name, "Source", sep = ":")
		]
		colnames(temp)[colnames(temp) %in% "Result"] <- j[1]
		for (i in j[!j == j[1]]) {
			temp <- merge(
				temp, 
				variable@output[
					variable@output[,paste(variable@name, "Source", sep = ":")] == i, 
					!colnames(variable@output) %in% paste(variable@name, "Source", sep = ":")
				]
			)
			colnames(temp)[colnames(temp) %in% "Result"] <- i
		}
		variable@output <- melt(
			temp, 
			measure.vars = levels(variable@output[,paste(variable@name, "Source", sep = ":")]), 
			variable.name = paste(variable@name, "Source", sep = ":"), 
			value.name = paste(variable@name, "Result", sep = ":"), 
			...
		)
		return(variable)
	}
	#cat("No input found for ", variable@name, ". Continuing...\n")
	return(variable)
}

# ComputeDependencies ############ uses Fetch2, EvalOutput, CheckMarginals and CheckInput to load and pre-process
# upstream variables. Typically seen on the first line of ovariable formula code. 
# '...' can be used for input substitution, na.rm, number of iterations (N) and others

ComputeDependencies <- function(dependencies, ...) { 
	Fetch2(dependencies)
	for (i in as.character(dependencies$Name)) {
		assign(i, EvalOutput(get(i), ...), envir = .GlobalEnv)
		assign(i, CheckMarginals(get(i), ...), envir = .GlobalEnv)
		assign(i, CheckInput(get(i), ...), envir = .GlobalEnv)
	}
}

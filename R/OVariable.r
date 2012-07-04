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
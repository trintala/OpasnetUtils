# SETCLASS OVARIABLE ################### Defines the S4 class "ovariable" which is the basic building block in open assessments.
setClass(
	"ovariable", 
	representation(
		name			= "character",
		output			= "data.frame", 
		data			= "data.frame", 
		marginal		= "logical", 
		formula			= "function", 
		dependencies	= "data.frame",
		ddata			= "character"
	),
	prototype = prototype(
		name			= character(),
		output			= data.frame(),
		data			= data.frame(),
		marginal		= logical(),
		formula			= function(...){0},
		dependencies	= data.frame(),
		ddata			= character()
	)
)


### result returns a vector that contains the result column of the
### output of a given ovariable. The vector contains the original column
### name as the attribute comment.
### e1 is the ovariable to operate with.

result <- function(e1) { # e1 must be an ovariable or a data.frame.
	
# Should we allow people to use this for data.frames as well?
#	if(class(e1) == "data.frame") e1 <- new("ovariable", name = character(), output = e1)
	
	# First check presence of name specific Result-columns
	
	test1 <- "Result" %in% colnames(e1@output)
	
	test3 <- paste(e1@name, "Result", sep = "") %in% colnames(e1@output)
	
	# If found take note
	
	rescol1 <- ifelse(test1, "Result", paste(e1@name, "Result", sep = ""))
	
	if(!(test1 | test3)) stop("No result column found while operating mathematically with ovariables!\n")
	
	out <- e1@output[[rescol1]]
	comment(out) <- rescol1 # Save the column name for later use
	
	return(out)
}

## "result<-" is a function that tells what is done if content is assigned into Getrescol(ovariable).
## e1 is the ovariable into which something is assigned.
## value is the thing to assign into the ovariable.

assign("result<-", function(e1, value) {
			e1@output[[comment(result(e1))]] <- value
			return(e1)}
)

####################
# ddata_apply
############################
# This function will download newest available data in the base according to the defined ddata link (page identifier).
# Normally if data already exists it is left alone. Replacement can be forced with a parameter.
# Remember to use ddata_tidy = FALSE for old data with "obs" as Iteration column.
#########################
ddata_apply <- function(
		ovariable, 
		ddata_tidy = TRUE, 
		force_ddata = FALSE, 
		...
) { 
	if (length(attributes(ovariable)) < 8) return(ovariable) # line for compatibility with old ovariable definitions
	if ((identical(ovariable@data, data.frame()) | force_ddata) & !identical(ovariable@ddata, character())) {
		ovariable@data <- opbase.data(ovariable@ddata)
		if (ddata_tidy) ovariable@data <- tidy(ovariable@data, ovariable@name, direction='long')
	}
	return(ovariable)
}

####################
# Ovariable (constructor)
#################
# Constructs an ovariable and optionally downloads ddata and saves the variable for use in other codes on the server. 
# The point of this constructor is to simplify variable creation: where before many different functions would have to
# be used to get data into a usable format now a simple call Ovariable(<variable_name>, ddata = <page_ident>, save = TRUE)
# will do the trick.
#####################
Ovariable <- function(
		name = character(), 
		data = data.frame(), 
		formula = function(...){0}, 
		dependencies = data.frame(), 
		ddata = character(),
		subset = NULL,
		getddata = TRUE, # will dynamic data be immediately be downloaded, as opposed waiting until variable output is Evaluated
		save = FALSE, # will the variable be saved on the server using objects.put
		public = TRUE, # will the saved variable be public or private
		...
) {
	if (! is.null(subset)) ddata <- paste(ddata, opbase.sanitize_subset_name(subset), sep='.')
	
	out <- new(
			"ovariable",
			name = name,
			data = data,
			formula = formula,
			dependencies = dependencies,
			ddata = ddata
	)
	if (getddata) out <- ddata_apply(out)
	if (save){
		assign(name, out)
		if (public) objects.store(list = name, ...) else objects.put(list = name, ...)
	}
	return(out)
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
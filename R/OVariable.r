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
		output = data.frame(),
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
			ddata = ddata,
			output = output
	)
	if (getddata) out <- ddata_apply(out)
	if (save){
		assign(name, out)
		if (public) objects.store(list = name, ...) else objects.put(list = name, ...)
	}
	return(out)
}

# SETMETHOD MATH ################### Math defines basic mathematical operations (log, exp, abs, ...) for ovariables
setMethod(
		f = "Math", 
		signature = signature(x = "ovariable"), 
		definition = function(x) {
			test <- paste(x@name, "Result", sep = "") %in% colnames(x@output)
			rescol <- ifelse(test, paste(x@name, "Result", sep = ""), "Result")
			x@output[[rescol]] <- callGeneric(x@output[[rescol]])
			return(x)
		}
)

# SETMETHOD OPS ##################
#########################################################################################
# Arithmetic operations of ovariables: first they are merged by index columns,
# then the operation is performed for the respective Result columns.
# If one of the expressions is numeric, it is first converted to an ovariable.
#########################################################################################

setMethod(
		f = "Ops", 
		signature = signature(e1 = "ovariable", e2 = "ovariable"), 
		definition = function(e1, e2) {
			# First check presence of name specific Result-columns
			
			test1 <- "Result" %in% colnames(e1@output)
			test2 <- "Result" %in% colnames(e2@output)
			
			test3 <- paste(e1@name, "Result", sep = "") %in% colnames(e1@output)
			test4 <- paste(e2@name, "Result", sep = "") %in% colnames(e2@output)
			
			# If found take note
			
			rescol1 <- ifelse(!test1, paste(e1@name, "Result", sep = ""), "Result")
			rescol2 <- ifelse(!test2, paste(e2@name, "Result", sep = ""), "Result")
			
			if(!(test1 | test3) | !(test2 | test4)) stop("No result column found while operating mathematically with ovariables!\n")
			
			#if (!(test1 & test2)) {
			#	rescol1 <- "Result.x"
			#	rescol2 <- "Result.y"
			#}
			
			# If not change prefixless Result to Result.x/y
			
			if (test1) {
				colnames(e1@output)[colnames(e1@output)=="Result"] <- "Result.x"
				rescol1 <- "Result.x"
			}
			
			if (test2) {
				colnames(e2@output)[colnames(e2@output)=="Result"] <- "Result.y"
				rescol2 <- "Result.y"
			}
			
			# Now merging should be possible without any confusion
			
			out <- merge(e1, e2)@output
			
			# Call generic function on the two Result-columns
			
			out$Result <- callGeneric(out[[rescol1]], out[[rescol2]])
			
			out <- new(
					"ovariable",
					#	dependencies = data.frame(Name = c(e1@name, e2@name)),
					output = out[
							!colnames(out) %in% c(
									ifelse(test1, rescol1, character()), 
									ifelse(test2, rescol2, character())
							) | colnames(out) == "Result"
					]
			)
			out <- CheckMarginals(out, deps = list(e1, e2), verbose = FALSE)
			return(out)
		}
)

setMethod(
		f = "Ops", 
		signature = signature(e1 = "ovariable", e2 = "numeric"), 
		definition = function(e1, e2) {
			e2 <- new("ovariable", output = data.frame(Result = e2))
			out <- callGeneric(e1, e2) # Call above definition
			return(out)
		}
)

setMethod(
		f = "Ops", 
		signature = signature(e1 = "numeric", e2 = "ovariable"), 
		definition = function(e1, e2) {
			e1 <- new("ovariable", output = data.frame(Result = e1))
			out <- callGeneric(e1, e2) # Call above definition
			return(out)
		}
)

# SETMETHOD MERGE ########### merge of ovariables merges the 'output' slot by index columns except 'Unit'.
setMethod(f = "merge", 
		signature = signature(x = "ovariable", y = "ovariable"),
		definition = function(x, y, all = FALSE, ...) {
			if (nrow(x@output) == 0) stop("X output missing!")
			if (nrow(y@output) == 0) stop("Y output missing!")
			#test <- intersect(c(colnames(x@output[x@marginal]), colnames(y@output[y@marginal])))
			temp <- merge(x@output, y@output, all = all, ...)#, by = test)
			temp <- new("ovariable", output = temp)
			#temp <- CheckMarginals(temp, deps = list(x,y))
			return(temp)
		}
)

setMethod(f = "merge", 
		signature = signature(x = "ovariable", y = "numeric"),
		definition = function(x, y, ...) {
			y <- new("ovariable", output = data.frame(Result = y))
			return(callGeneric(x, y, ...))
		}
)

setMethod(f = "merge", 
		signature = signature(x = "numeric", y = "ovariable"),
		definition = function(x, y, ...) {
			x <- new("ovariable", output = data.frame(Result = x))
			return(callGeneric(x, y, ...))
		}
)

setMethod(f = "merge", 
		signature = signature(x = "ovariable", y = "data.frame"),
		definition = function(x, y, ...) {
			y <- new("ovariable", output = y)
			return(callGeneric(x, y, ...))
		}
)

setMethod(f = "merge", 
		signature = signature(x = "data.frame", y = "ovariable"),
		definition = function(x, y, ...) {
			y <- new("ovariable", output = x)
			return(callGeneric(x, y, ...))
		}
)

# SETMETHOD PLOT ################ plot diagrams about ovariable data

setMethod(
		f = "plot",
		signature = signature(x = "ovariable"),
		definition = function(x) {
			plot(
					x    = x@output[, paste("Source", x@name, sep = "")], 
					y    = x@output$Result, 
					xlab = paste("Source", x@name, sep = ""), 
					ylab = x@output[x@output[, paste("Source", x@name, sep = "")] == "Data", "Unit"][1], 
					main = x@name
			)
		}
)

# SETMETHOD summary ################### Summary defines how summaries of ovariables are shown.
setMethod(
		f = "summary", 
		signature = signature(object = "ovariable"), 
		definition = function(object, function_names = character(), marginals = character(), ...) {
			test <- paste(object@name, "Result", sep = "") %in% colnames(object@output)
			rescol <- ifelse(test, paste(object@name, "Result", sep = ""), "Result")
			#object@output <- object@output[ , -grep("Description|Source", colnames(object@output))] # not a necessary line
			
			# If no function names are defined then use defaults which depend on whether the data is probabilistic or not
			if("Iter" %in% colnames(object@output) && !"Iter" %in% marginals) {
				if (length(function_names)==0) function_names <- c("mean", "sd", "min", "Q0.025", "median", "Q0.975", "max")
				#object@output <- object@output[object@output$Iter == 1, ]
			}
			else {
				if (length(function_names)==0) function_names <- c("mean")
			}
			function_names <- unique(function_names)
			
			functions <- list()
			for(fname in function_names) {
				functions <- c(functions, get(fname))
			}
			
			# If marginals are not defined the data is assumed probabilistic and the summary to be about the distribution
			if(length(marginals)==0) {
				marginals <- colnames(object@output)[object@marginal & colnames(object@output) != "Iter"]
			}
			
			# Apply the selected functions
			temp <- list()
			for(fun in functions){
				temp[[length(temp)+1]] <- tapply(object@output[[rescol]], object@output[marginals], fun)
			}
			#out <- data.frame()
			
			# Convert results to data.frames and remove useless rows
			for(i in 1:length(temp)){
				temp[[i]] <- as.data.frame(as.table(temp[[i]]))
				temp[[i]] <- temp[[i]][!is.na(temp[[i]][["Freq"]]),]
				colnames(temp[[i]])[colnames(temp[[i]])=="Freq"] <- function_names[i]
			}
			
			# Merging
			if(length(temp)>1) {
				out <- merge(temp[[1]], temp[[2]], all = TRUE)
				if(length(temp)>2) {
					for(i in 3:length(temp)) {
						out <- merge(out, temp[[i]], all = TRUE)
					}
				}
			}
			else {
				out <- temp[[1]]
			}
			#if(nrow(object@output) > 200) {
			#	object@output <- object@output[1:200, ]
			#}
			#return(object@output)
			return(out)
		}
)

Q0.025 <- function(x){
	return(quantile(x, probs = 0.025))
}

Q0.975 <- function(x){
	return(quantile(x, probs = 0.975))
}



####################
# result
######################
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
		if (ddata_tidy) ovariable@data <- tidy(ovariable@data, ovariable@name, direction='long') # data from base should
		# always be taken as is (ddata_tidy = FALSE) or to direction 'wide'
	}
	return(ovariable)
}



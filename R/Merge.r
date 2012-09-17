# SETMETHOD MERGE ########### merge of ovariables merges the 'output' slot by index columns except 'Unit'.
setMethod(f = "merge", 
	signature = signature(x = "ovariable", y = "ovariable"),
	definition = function(x, y, ...) {
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
	definition = function(x, y) {
		y <- new("ovariable", output = data.frame(Result = y))
		return(callGeneric(x, y))
	}
)

setMethod(f = "merge", 
	signature = signature(x = "numeric", y = "ovariable"),
	definition = function(x, y) {
		x <- new("ovariable", output = data.frame(Result = x))
		return(callGeneric(x, y))
	}
)

setMethod(f = "merge", 
	signature = signature(x = "ovariable", y = "data.frame"),
	definition = function(x, y) {
		y <- new("ovariable", output = y)
		return(callGeneric(x, y))
	}
)

setMethod(f = "merge", 
	signature = signature(x = "data.frame", y = "ovariable"),
	definition = function(x, y) {
		y <- new("ovariable", output = x)
		return(callGeneric(x, y))
	}
)
# SETMETHOD MERGE ########### merge of ovariables merges the 'output' slot by index columns except 'Unit'.
setMethod(f = "merge", 
	signature = signature(x = "ovariable", y = "ovariable"),
	definition = function(x, y) {
		if (nrow(x@output) == 0) stop("X output missing!")
		if (nrow(y@output) == 0) stop("Y output missing!")
		test <- intersect(colnames(x@output[x@marginal]), colnames(y@output[y@marginal]))
		x@output <- merge(x@output, y@output, by = test, all = TRUE)
		return(x)
	}
)

setMethod(f = "merge", 
	signature = signature(x = "ovariable", y = "numeric"),
	definition = function(x, y) {
		y <- new("ovariable", output = data.frame(Result = y))
		return(merge(x, y))
	}
)

setMethod(f = "merge", 
	signature = signature(x = "numeric", y = "ovariable"),
	definition = function(x, y) {
		x <- new("ovariable", output = data.frame(Result = x))
		return(merge(x, y))
	}
)
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
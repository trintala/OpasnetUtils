# SETMETHOD MATH ################### Math defines basic mathematical operations (log, exp, abs, ...) for ovariables
setMethod(
	f = "Math", 
	signature = signature(x = "ovariable"), 
	definition = function(x) {
		test <- paste(x@name, "Result", sep = "") %in% colnames(x)
		rescol <- ifelse(test, paste(x@name, "Result", sep = ""), "Result")
		x@output[[rescol]] <- callGeneric(x@output[[rescol]])
		return(x)
	}
)
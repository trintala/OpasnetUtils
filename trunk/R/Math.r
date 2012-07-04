# SETMETHOD MATH ################### Math defines basic mathematical operations (log, exp, abs, ...) for ovariables
setMethod(
	f = "Math", 
	signature = signature(x = "ovariable"), 
	definition = function(x) {
		test <- paste(x@name, "Result", sep = "") %in% colnames(out)
		rescol <- ifelse(test, paste(e1@name, "Result", sep = ""), "Result")
		x@output[[rescol]] <- callGeneric(x@output[[rescol]])
		return(x)
	}
)
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
		
		test1 <- paste(e1@name, "Result", sep = "") %in% colnames(out)
		test2 <- paste(e2@name, "Result", sep = "") %in% colnames(out)
		
		# If found take note
		
		rescol1 <- ifelse(test1, paste(e1@name, "Result", sep = ""), "Result")
		rescol2 <- ifelse(test2, paste(e2@name, "Result", sep = ""), "Result")
		
		#if (!(test1 & test2)) {
		#	rescol1 <- "Result.x"
		#	rescol2 <- "Result.y"
		#}
		
		# If not change prefixless Result to Result.x/y
		
		if (!test1) {
			colnames(e1@output)[colnames(e1@output)=="Result"] <- "Result.x"
			rescol1 <- "Result.x"
		}
		
		if (!test2) {
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
			output = out[, (!colnames(out) %in% c(rescol1, rescol2)) | colnames(out) == "Result"]
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
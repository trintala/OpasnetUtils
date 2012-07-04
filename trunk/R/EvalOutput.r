# EvalOutput #################### evaluates the output slot of ovariables
##### Marginals should be also checked and updated here or elsewhere

EvalOutput <- function(variable, ...) { # ... for e.g na.rm 
	if (nrow(variable@data) > 0) {
		rescol <- ifelse(
			"Result" %in% colnames(variable@data), 
			"Result", 
			paste(variable@name, "Result", sep = "")
		)
		if (!is.numeric(variable@data[[rescol]]) & !is.null(variable@data[[rescol]])) {
			a <- interpret(variable@data, rescol = rescol, ...) 
		} else a <- variable@data
	} else a <- variable@data
	b <- variable@formula(variable@dependencies, ...)
	if (is.numeric(b) & nrow(variable@data) == 0) {
		stop(paste("No proper data nor formula defined for ", variable@name, "!\n", sep = ""))
	}
	if (is.numeric(b)) {
		colnames(a)[colnames(a) == rescol] <- paste(variable@name, "Result", sep = "")
		a[,paste(variable@name, "Source", sep = "")] <- "Data"
		variable@output <- a
		return(variable)
	}
	if (nrow(variable@data) == 0) {
		colnames(b)[
			colnames(b) %in% paste(c("", variable@name), "Result", sep = "")
		] <- paste(variable@name, "Result", sep = "")
		b[,paste(variable@name, "Source", sep = "")] <- "Formula"
		variable@output <- b
		return(variable)
	}
	colnames(a)[colnames(a) == rescol] <- "FromData"
	colnames(b)[colnames(b) %in% c(paste(variable@name, "Result", sep = ""), "Result")] <- "FromFormula" # *
	# <variablename> prefix not necessitated for "Result" column of formula output
	temp <- melt(
		merge(a, b, all = TRUE, ...), # Will cause problems if dependencies contain non-marginal indices that match with -
		# marginal indeces in data. Or maybe not.
		measure.vars = c("FromData", "FromFormula"),
		variable.name = paste(variable@name, "Source", sep = ""),
		value.name = paste(variable@name, "Result", sep = ""),
		...
	)
	levels(
		temp[[paste(variable@name, "Source", sep = "")]]
	) <- gsub("^From", "", 
		levels(
			temp[[paste(variable@name, "Source", sep = "")]]
		)
	)
	variable@output <- temp
	return(variable)
}
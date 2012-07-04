# ORBIND ########################### orbind combines two ovariables using clever rbind

orbind <- function(x, y) {
		if(class(x) == "ovariable") {xoutput <- x@output} else {xoutput <- x}
		if(class(y) == "ovariable") {youtput <- y@output} else {youtput <- y}
		cols <- setdiff(colnames(youtput), colnames(xoutput)) # Take all columns that do not exist in x and add them.
		col <- as.data.frame(array("NA", dim = c(1, length(cols))))
		colnames(col) <- cols
		if("Unit" %in% cols) {col[, "Unit"] <- "?"}
		temp <- cbind(xoutput, col)

		cols <- setdiff(colnames(xoutput), colnames(youtput)) # Take all columns that do not exist in y and add them.
		col <- as.data.frame(array("NA", dim = c(1, length(cols))))
		colnames(col) <- cols
		if("Unit" %in% cols) {col[, "Unit"] <- "?"}
		xoutput <- rbind(temp, cbind(youtput, col)) # Combine x and y with rbind.
		return(xoutput)
#Should this be made S4 function for ovariables? Then it could be named simply rbind.
}
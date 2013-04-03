# ORBIND ########################### orbind combines two ovariables using clever rbind

orbind <- function(x, y) {
	if(class(x) == "ovariable") x <- x@output
	if(class(y) == "ovariable") y <- y@output
	
	addmissingcol <- function(e1, e2) { #Adds all missing columns. Merges Iter if that is missing.
		cols <- setdiff(colnames(e2), colnames(e1)) # Take all columns that do not exist in e1.
		
		if("Iter" %in% cols) {
			e1 <- merge(unique(e2["Iter"]), e1) # Add Iter with all locations existing in e2.
			cols <- cols[cols != "Iter"] # Remove Iter from the list of columns to add.
		}
		col <- as.data.frame(array(NA, dim = c(1, length(cols))))
		colnames(col) <- cols
		if("Unit" %in% cols) {col[, "Unit"] <- "?"}
		
		return(cbind(e1, col))
	}
	
	temp1 <- addmissingcol(x, y)
	temp2 <- addmissingcol(y, x)
	
	return(rbind(temp1, temp2))
	
#Should this be made S4 function for ovariables? Then it could be named simply rbind.
	
}
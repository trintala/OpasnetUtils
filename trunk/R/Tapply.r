# TAPPLY ########### tapply of ovariables applies a function to each cell of a ragged array, that is to each (non-empty) group of
############ values given by a unique combination of the levels of certain factors.
### parameters (other parameters are as in generic tapply):
### X an ovariable

setMethod(f = "tapply", 
	signature = signature(X = "ovariable"),
	definition = function(X, INDEX, FUN = NULL, ..., simplify = TRUE) {
		out <- as.data.frame(as.table(tapply(X@output$Result, INDEX, FUN, ..., simplify = TRUE)))
		colnames(out)[colnames(out) == "Freq"] <- "Result"
		X@output <- out
		return(X)
	}
)
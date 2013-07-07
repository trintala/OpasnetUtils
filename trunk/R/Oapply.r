# OAPPLY ########### tapply of ovariables applies a function to each cell of a ragged array, that is to each (non-empty) group of
############ values given by a unique combination of the levels of certain factors.
### parameters (other parameters are as in generic tapply):
### X an ovariable
### cols overrides INDEX by choosing INDEX as all marginals NOT given in cols (character vector) argument

oapply = function(X, INDEX = NULL, FUN = NULL, cols = NULL, ..., simplify = TRUE) {
	out <- X@output
	marginals <- colnames(out)[X@marginal]
	if (is.null(INDEX) & is.null(cols)) stop("No INDEX nor cols defined!\n")
	if (!is.null(cols)) INDEX <- out[marginals[!marginals %in% cols]]
	out <- tapply(
		X = out[[paste(X@name, "Result", sep = "")]], 
		INDEX = INDEX,
		FUN = FUN,
		...,
		simplify = simplify
	)
	out <- as.data.frame(as.table(out))
	colnames(out)[colnames(out) == "Freq"] <- paste(X@name, "Result", sep = "")
	X@output <- out
	X@marginal <- colnames(out) %in% marginals # Marginals can be easily corrected here disrequiring CheckMarginals
	return(X)
}

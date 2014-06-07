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
	if (length(INDEX) == 0) stop("No marginals!\n")
	out <- tapply(
		X = out[[paste(X@name, "Result", sep = "")]], 
		INDEX = INDEX,
		FUN = FUN,
		...,
		simplify = simplify
	)
	if (length(out) == 0) stop("0 length return from tapply!\n")
	if (is.list(out[1])) { # function returned array
		rows <- tapply(1:nrow(X@output), INDEX, I)
		if (length(dim(out[[1]])) == 2) {
			out <- lapply(out, t)
		}
		out <- lapply(lapply(out, as.table), as.data.frame)
		for (i in 1:length(out)) {
			out[[i]]$Row <- rows[[i]]
		}
		out <- do.call(rbind, out)
		
		temp <- X@output[!colnames(X@output) %in% paste(X@name, "Result", sep = "")]
		temp$Row <- 1:nrow(temp)
		
		out <- merge(temp, out)
		out <- out[colnames(out) != "Row"]
	}
	else { # function returned single value
		out <- as.data.frame(as.table(out))
	}
	colnames(out)[colnames(out) == "Freq"] <- paste(X@name, "Result", sep = "")
	X@output <- out
	X@marginal <- colnames(out) %in% marginals # Marginals can be easily corrected here disrequiring CheckMarginals
	return(X)
}

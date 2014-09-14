# OAPPLY ########### tapply of ovariables applies a function to each cell of a ragged array, that is to each (non-empty) group of
############ values given by a unique combination of the levels of certain factors.
### parameters (other parameters are as in generic tapply):
### X an ovariable
### cols overrides INDEX by choosing INDEX as all marginals NOT given in cols (character vector) argument

oapply = function(X, INDEX = NULL, FUN = NULL, cols = NULL, use_plyr = TRUE, ..., simplify = TRUE) {
	out <- X@output
	marginals <- colnames(out)[X@marginal]
	if (is.null(INDEX) & is.null(cols)) stop("No INDEX nor cols defined!\n")
	if (!is.null(cols)) INDEX <- out[marginals[!marginals %in% cols]]
	if (length(INDEX) == 0) stop("No marginals!\n")
	if (use_plyr == TRUE) {
		if (is.character(INDEX)) vars <- INDEX else vars <- colnames(INDEX)
		if (is.null(vars)) stop("Unable to determine index name, please use character input.")
		out <- ddply(
			out, 
			vars,
			oapplyf(FUN),
			rescol = paste(X@name, "Result", sep = ""),
			datvars = vars, 
			.drop = TRUE
		)
	} else {
		# Old implementation
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
	}
	X@output <- out
	X@marginal <- colnames(out) %in% marginals # Marginals can be easily corrected here disrequiring CheckMarginals
	return(X)
}

oapplyf <- function(fun) {
	if (is.character(fun)) fun <- get(fun)
	out <- function(dat, rescol, datvars) {
		# Take first entry of each index (since they should contain only one distinct value)
		out <- data.frame(dat[[datvars[1]]][1])
		if (length(datvars > 1)) {
			for (i in 2:length(datvars)) {
				out[[i]] <- dat[[datvars[i]]][1]
			}
		}
		out <- data.frame(out, fun(dat[[rescol]]))
		colnames(out) <- c(datvars, rescol)
		return(out)
	}
	return(out)
}

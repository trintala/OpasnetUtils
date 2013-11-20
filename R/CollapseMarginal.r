#######################################
# CollapseMarginal
##########################################
# Collapses marginals by applying sums, means or samples
# Also loses all non-marginal columns except the relevant Result
# Parse-able table should have columns Index, Function and Probs
# Probs can be left out for equal weights sampling
# If Function is not given mean is assumed
#####################################

CollapseTableParser <- function(CTable, env = .GlobalEnv){ # CTable is a data.frame
	for (i in unique(as.character(CTable$Variable))) {
		temp <- CTable[CTable$Variable == i,]
		cols <- temp[["Index"]]
		probs <- strsplit(as.character(temp[["Probs"]]), ",")
		probs <- lapply(probs, as.numeric)
		fun <- temp[["Function"]]
		out <- list(cols = cols, probs = probs, fun = fun)
		assign(paste("Col", i, sep = ""), out, envir = env)
	}
}

CheckCollapse <- function(variable, indent = 0, verbose = TRUE, ...) {
	if (exists(paste("Col", variable@name, sep = ""))) {
		if (verbose) cat(rep("-", indent), "Processing", variable@name, "marginal collapses", "...")
		Col <- get(paste("Col", variable@name, sep = ""))
		variable <- CollapseMarginal(variable, Col$cols, Col$fun, Col$probs, ...)
		if (verbose) cat(" done!\n")
	}
	return(variable)
}

CollapseMarginal <- function(variable, cols, fun = "mean", probs = NULL, ...) { # cols is a character vector, while probs is a list
	if (length(fun) == 0) fun <- "mean"
	# If no probabilities given use function
	# Also if given funtion is sample then equal weights are used and this section is skipped
	if (length(probs) == 0 & fun != "sample") {
		fun <- get(fun)
		out <- oapply(variable, FUN = fun, cols = cols, na.rm = TRUE)
		return(out)
	}
	
	# Else use sample with option of given probabilities
	out <- variable@output
	marginals <- colnames(out)[variable@marginal]
	
	if ("Iter" %in% colnames(out)) {
		if (!is.list(probs) & is.numeric(probs)) probs <- list(probs)
		if (!is.null(probs) & length(probs) != length(cols)) stop("Length of cols and probs do not match!\n")
		N <- max(out$Iter)
		lengths <- list()
		a <- 0
		for (i in cols) {
			a <- a + 1
			b <- probs[[a]]
			if(is.null(b)) b <- rep(1, length(unique(as.character(out[[i]]))))
			if(is.na(b)) b <- rep(1, length(unique(as.character(out[[i]]))))
			if(b == 1) b <- rep(1, length(unique(as.character(out[[i]]))))
			#selection <- tapply( # For each iteration, sample from available locations. All locations should exist for
			#	out[[i]], # all iterations, but tapply is the most efficient way of doing the sampling and it adds
			#	out[["Iter"]], # extra robustness. Not going to work with given probabilities, since the input would 
			#	sample, # vary in length depending on the number of other marginals etc.
			#	size = 1, 
			#	prob = b
			#)
			#temp <- rownames(selection)
			#selection <- data.frame(selection, Iter = temp)
			selection <- data.frame(
				sample(
					unique(
						as.character(
							out[[i]]
						)
					), 
					size = N, 
					replace = TRUE, 
					prob = b
				), 
				Iter = 1:N
			)
			colnames(selection)[1] <- i
			out <- merge(out, selection)
		}
		variable@output <- out
		variable@marginal <- colnames(out) %in% marginals & ! colnames(out) %in% cols
	} else warning(
		paste(
			"Unable to collapse columns \"", 
			paste(cols, collapse = "\",\""), 
			"\" in ", 
			variable@name, 
			" using sampling.\n", 
			"Reason: non-probabilistic data!\n", 
			sep = ""
		)
	)
	return(variable)
}
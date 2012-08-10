#######################################
# CollapseMarginal
##########################################
# Collapses marginals by applying sums, means or samples
# Also loses all non-marginal columns except the relevant Result
#####################################

CollapseMarginal <- function(variable, cols, method = "Sample", ...) { # ... prob = <vector> for sampling weights
	out <- variable@output
	func <- CollapsePreset(method) # get the selected function
	marginals <- colnames(out)[variable@marginal]
	out <- tapply(
		out[[paste(variable@name, "Result", sep = "")]], 
		out[[marginals[!marginals %in% cols]]],
		func,
		...
	)
	out <- as.data.frame(as.table(out))
	colnames(out)[colnames(out) == "Freq"] <- paste(variable@name, "Result", sep = "")
	#for(i in colnames(out)[colnames(out) %in% cols])
	variable@output <- out
	variable@marginal <- colnames(out) %in% marginals # Marginals can be easily corrected here disrequiring CheckMarginals
	return(variable)
}



CollapsePreset <- function(name) {
	if (name == "Sum") return(function(x, ...) sum(x)) # ... needs to be allowed
	if (name == "Mean") return(function(x, ...) mean(x))
	if (name == "Sample") return(function(x, ...) sample(x, size = 1, ...))
}

CollapseTableParser <- function(ctable) {
	for (i in rownames(ctable)) {
		
	}
}

CollapseMarginal <- function(variable, cols, probs = NULL, ...) { # cols is a character vector, while probs is a list
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
			if(is.null(b)) b <- rep(1, N)
			selection <- tapply( # For each iteration, sample from available locations. All locations should exist for
				out[[i]], # all iterations, but tapply is the most efficient way of doing the sampling and it adds
				out[["Iter"]], # extra robustness.
				sample,
				size = 1,
				prob = b
			)
			temp <- rownames(selection)
			selection <- data.frame(selection, Iter = temp)
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
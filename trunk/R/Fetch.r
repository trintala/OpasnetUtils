# FETCH ##################### fetch downloads a variable.

fetch <- function(x, direction = "wide") { # Could think of a version where dependencies table is given as a parameter, and based on Name, Identifier is downloaded from the Base.
	x <- as.character(x)
	if(exists(x)) {
		out <- get(x)
	} else {
		out <- tidy(op_baseGetData("opasnet_base", x), direction = "wide")
	}
	return(out)
}


# Fetch2 #################### loads all given dependencies to global memory

Fetch2 <- function(dependencies, evaluate = FALSE, indent = 0, ...) {
	if (nrow(dependencies) > 0) {
		for (i in 1:nrow(dependencies)) {
			if(!exists(as.character(dependencies$Name[i]))) {
				if (is.na(dependencies$Key[i]) | dependencies$Key[i] == "") stop(paste("No key given for dependent variable", dependencies$Name))
				objects.get(dependencies$Key[i]) # Key is the R-tools session identifier (shown at the end of the url)
				if (evaluate) assign(
					as.character(dependencies$Name[i]), 
					EvalOutput(get(as.character(dependencies$Name[i])), ...), 
					envir = .GlobalEnv
				) 
				# Eval not necessarily needed at this point
				cat("\n", rep("-", indent), as.character(dependencies$Name[i]), "fetched successfully!\n")
			}
		}
	}
} # no need to return anything since variables are already written in global memory
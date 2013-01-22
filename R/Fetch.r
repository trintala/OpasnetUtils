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

Fetch2 <- function(dependencies, evaluate = FALSE, indent = 0, verbose = TRUE, ...) {
	if (nrow(dependencies) > 0) {
		for (i in 1:nrow(dependencies)) {
			if(!exists(as.character(dependencies$Name[i]))) {
				testkey <- if (is.null(dependencies$Key[i])) TRUE else is.na(dependencies$Key[i]) | dependencies$Key[i] == ""
				testid <- if (is.null(dependencies$Ident[i])) TRUE else is.na(dependencies$Ident[i]) | dependencies$Ident[i] == ""
				if (testkey & testid) {
					stop(paste("No key nor ident given for dependent variable: ", dependencies$Name, "!", sep = ""))
				}
				if (!testkey) {
					objects.get(dependencies$Key[i]) # Key is the R-tools session identifier (shown at the end of the url)
				}
				if (testkey & !testid) {
					ident <- strsplit(dependencies$Ident[i], "/")[[1]] # Ident should be in format <page_id>/<code_name>
					objects.latest(ident[1], ident[2])
				}
				if (evaluate) assign(
						as.character(dependencies$Name[i]), 
						EvalOutput(get(as.character(dependencies$Name[i])), ...), 
						envir = .GlobalEnv
				)
				if (verbose) cat("\n", rep("-", indent), as.character(dependencies$Name[i]), "fetched successfully!\n")
			}
		}
	}
} # no need to return anything since variables are written in global memory by objects.get
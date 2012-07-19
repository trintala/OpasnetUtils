# ComputeDependencies ############ uses Fetch2, EvalOutput, CheckMarginals and CheckInput to load and pre-process
# upstream variables. Typically seen on the first line of ovariable formula code. 
# '...' can be used for input substitution, na.rm, number of iterations (N) and others

ComputeDependencies <- function(dependencies, forceEval = FALSE, indent = 0, ...) { 
	cat("\n")
	Fetch2(dependencies, indent = indent, ...)
	for (i in as.character(dependencies$Name)) {
		if (nrow(get(i)@output) == 0 | forceEval) assign(i, EvalOutput(get(i), indent = indent, ...), envir = .GlobalEnv)
		assign(i, CheckMarginals(get(i), indent = indent, ...), envir = .GlobalEnv)
		assign(i, CheckInput(get(i), indent = indent, ...), envir = .GlobalEnv)
	}
	cat("\n")
}
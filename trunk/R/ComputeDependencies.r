# ComputeDependencies ############ uses Fetch2, EvalOutput, CheckMarginals and CheckInput to load and pre-process
# upstream variables. Is used automatically by EvalOutput, but can be seen on the first lines of old ovariable formula code, 
# to avoid applying decisions, inputs and optimizations twice in old code the function does nothing by default. This is no 
# problem since users should not be calling this function at all.
# '...' can be used for input substitution, na.rm, number of iterations (N) and others
# new_code is a parameter for old codes where ComputeDependencies was not automated

ComputeDependencies <- function(dependencies, forceEval = FALSE, indent = 0, new_code = FALSE, ...) { 
	if (new_code) {
		cat("\n")
		Fetch2(dependencies, indent = indent, ...)
		for (i in as.character(dependencies$Name)) {
			# First check if dependency exists at all
			ret <- tryCatch(get(i), error = function(e) return(NULL))
			if (is.null(ret)) stop(paste("Ovariable depends on missing variable named '",i,"'",sep='' ))
			# If dependency is ovariable
			if (class(get(i)) == "ovariable") {
				if (nrow(get(i)@output) == 0 | forceEval) {
					#assign(i, EvalOutput(get(i), indent = indent, ...), envir = .GlobalEnv)
					ret1 <- tryCatch(
						assign(i, EvalOutput(get(i), indent = indent, ...), envir = .GlobalEnv), 
						error = function(e) return(NULL)
					)
					if (is.null(ret1)) stop(paste("Evaluating", get(i)@name, "failed!"))
				}
				#assign(i, CheckMarginals(get(i), indent = indent, ...), envir = .GlobalEnv) # moved to EvalOutput
				ret2 <- tryCatch(
					assign(i, CheckInput(get(i), indent = indent, ...), envir = .GlobalEnv), 
					error = function(e) return(NULL)
				)
				if (is.null(ret2)) stop(paste("Input checking", get(i)@name, "failed!"))
				ret3 <- tryCatch(assign(i, CheckDecisions(get(i), indent = indent, ...), envir = .GlobalEnv), 
						error = function(e) return(NULL)
				)
				if (is.null(ret3)) stop(paste("Decision checking", get(i)@name, "failed!"))
				ret4 <- tryCatch(assign(i, CheckCollapse(get(i), indent = indent, ...), envir = .GlobalEnv), 
						error = function(e) return(NULL)
				)
				if (is.null(ret4)) stop(paste("Collapse checking", get(i)@name, "failed!"))
			}
		}
		cat("\n")
	}
}
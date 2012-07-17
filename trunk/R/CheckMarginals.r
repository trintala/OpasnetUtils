# CheckMarginals ############# Assumes that all depended upon variables are in memory, as should be the case.
##################
# Returns an ovariable with a marginal devised from the data and upstream variable marginals. 
# Marginal values for data should be stored into the database somehow

CheckMarginals <- function(variable, deps = NULL, priormarg = TRUE, ...) { # extra inputs necessary for ops functionality
	cat("Checking", variable@name, "marginals", "...")
	varmar <- colnames(variable@data)[
		!grepl(paste("^", variable@name, "", sep=""), colnames(variable@data))&
		!colnames(variable@data) %in% c("Result", "Unit")
	]
	# all locs under observation/parameter index should be excluded
	varmar <- c(varmar, paste(variable@name, "Source", sep = "")) # Source is added 
	# by EvalOutput so it should be in the initial list by default. 
	novarmar <- colnames(variable@data)[!colnames(variable@data) %in% varmar]
	if (priormarg & length(variable@marginal) > 0) {
		varmar <- unique(varmar, colnames(variable@output)[variable@marginal])
		novarmar <- unique(c(novarmar, colnames(variable@data)[!colnames(variable@output) %in% varmar]))
	}
	if (length(deps) > 0) {
		for (i in deps) {
			varmar <- unique(c(varmar, colnames(i@output)[i@marginal]))
			novarmar <- unique(c(novarmar, colnames(i@output)[!i@marginal]))
		}
	} else {
		for (i in as.character(variable@dependencies$Name)){
			varmar <- unique(c(varmar, colnames(get(i)@output)[get(i)@marginal]))
			novarmar <- unique(c(novarmar, colnames(get(i)@output)[!get(i)@marginal]))
		}
	}
	varmar <- varmar[!varmar %in% novarmar]
	variable@marginal <- colnames(variable@output) %in% varmar
	cat("done!\n")
	return(variable)
}
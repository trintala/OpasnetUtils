# SETMETHOD summary ################### Summary defines how summaries of ovariables are shown.
setMethod(
		f = "summary", 
		signature = signature(object = "ovariable"), 
		definition = function(object, function_names = character(), marginals = character(), ...) {
			test <- paste(object@name, "Result", sep = "") %in% colnames(object@output)
			rescol <- ifelse(test, paste(object@name, "Result", sep = ""), "Result")
			#object@output <- object@output[ , -grep("Description|Source", colnames(object@output))] # not a necessary line
			
			# If no function names are defined then use defaults which depend on whether the data is probabilistic or not
			if("Iteration" %in% colnames(object@output) && !"Iteration" %in% marginals) {
				if (length(function_names)==0) function_names <- c("mean", "sd", "min", "Q0.025", "median", "Q0.975", "max")
				#object@output <- object@output[object@output$Iter == 1, ]
			}
			else {
				if (length(function_names)==0) function_names <- c("mean")
			}
			function_names <- unique(function_names)
			
			functions <- list()
			for(fname in function_names) {
				functions <- c(functions, get(fname))
			}
			
			# If marginals are not defined the data is assumed probabilistic and the summary to be about the distribution
			if(length(marginals)==0) {
				marginals <- colnames(object@output)[object@marginal & colnames(object@output) != "Iteration"]
			}
			
			# Apply the selected functions
			temp <- list()
			for(fun in functions){
				temp[[length(temp)+1]] <- tapply(object@output[[rescol]], object@output[marginals], fun)
			}
			#out <- data.frame()
			
			# Convert results to data.frames and remove useless rows
			for(i in 1:length(temp)){
				temp[[i]] <- as.data.frame(as.table(temp[[i]]))
				temp[[i]] <- temp[[i]][!is.na(temp[[i]][["Freq"]]),]
				colnames(temp[[i]])[colnames(temp[[i]])=="Freq"] <- function_names[i]
			}
			
			# Merging
			if(length(temp)>1) {
				out <- merge(temp[[1]], temp[[2]], all = TRUE)
				if(length(temp)>2) {
					for(i in 3:length(temp)) {
						out <- merge(out, temp[[i]], all = TRUE)
					}
				}
			}
			else {
				out <- temp[[1]]
			}
			#if(nrow(object@output) > 200) {
			#	object@output <- object@output[1:200, ]
			#}
			#return(object@output)
			return(out)
		}
)

Q0.025 <- function(x){
	return(quantile(x, probs = 0.025))
}

Q0.975 <- function(x){
	return(quantile(x, probs = 0.975))
}


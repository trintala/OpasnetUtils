# CheckInput ################# checks and uses outside input (user inputs in models or decision variables)
# takes an ovariable as argument
# returns an ovariable

CheckInput <- function(variable, substitute = FALSE, ...) { # ... e.g for na.rm
	cat("Checking", variable@name, "inputs", "...")
	if (nrow(variable@output) == 0) stop(paste(variable@name, "output not evaluated yet!"))
	if (exists(paste("Inp", variable@name, sep = ""))) {
		inputvar <- get(paste("Inp", variable@name, sep = ""))
		if (substitute) {
			colnames(inputvar@output)[colnames(inputvar@output) == paste(variable@name, "Result", sep = "")] <- "InpVarRes"
			colnames(variable@output)[colnames(variable@output) == paste(variable@name, "Result", sep = "")] <- "VarRes"
			finalvar <- merge(variable, inputvar)
			finalvar@output[[paste(variable@name, "Result", sep = "")]] <- ifelse(
				is.na(finalvar@output$InpVarRes), 
				finalvar@output$VarRes, 
				finalvar@output$InpVarRes
			)
			finalvar@output[[paste(variable@name, "Source", sep = "")]] <- ifelse(
				is.na(finalvar@output$InpVarRes), 
				finalvar@output[[paste(variable@name, "Source", sep = "")]], 
				"Input"
			)
			finalvar@output <- finalvar@output[!colnames(finalvar) %in% c("InpVarRes", "VarRes")]
			cat("done!\n")
			return(finalvar)
		}
		#variable@output[variable@output$Source,]
		j <- unique(as.character(variable@output[[paste(variable@name, "Source", sep = "")]]))
		temp <- variable@output[
			variable@output[,paste(variable@name, "Source", sep = "")] == j[1], 
		]
		temp <- temp[!colnames(variable@output) %in% paste(variable@name, "Source", sep = "")]
		colnames(temp)[colnames(temp) %in% paste(c(variable@name, ""), "Result", sep = "")] <- j[1]
		for (i in j[!j == j[1]]) {
			temp2 <- variable@output[
				variable@output[,paste(variable@name, "Source", sep = "")] == i, 
			]
			temp2 <- temp2[!colnames(variable@output) %in% paste(variable@name, "Source", sep = "")]
			temp <- merge(
				temp, 
				temp2
			)
			colnames(temp)[colnames(temp) %in% "Result"] <- i
		}
		colnames(inputvar@output)[colnames(inputvar@output) == paste(variable@name, "Result", sep = "")] <- "Input"
		temp <- merge(temp, inputvar@output)
		variable@output <- melt(
			temp, 
			measure.vars = c(j, "Input"), 
			variable.name = paste(variable@name, "Source", sep = ""), 
			value.name = paste(variable@name, "Result", sep = ""), 
			...
		)
		cat("done!\n")
		return(variable)
	}
	#cat("No input found for ", variable@name, ". Continuing...\n")
	cat("done!\n")
	return(variable)
}
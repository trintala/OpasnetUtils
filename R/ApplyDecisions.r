# APPLY.DECISIONS ############## apply.decisions takes a decision table and applies that to an assessment.
## dec is a decision data.frame that must have columns Decision, Option, Variable, Cell, Change, Result. It can have several variables.
apply.decisions <- function(assessment) {
	dec <- merge(assessment@decisions, assessment@dependencies, by.x = "Variable", by.y = "Name")
	colnames(dec)[colnames(dec) == "Result.x"] <- "Result"
	colnames(dec)[colnames(dec) == "Result.y"] <- "Name"

	for(variables in unique(dec$Name)) { # Take one variable at a time.
		dec.var <- dec[dec$Name == variables, ] # dec.var = variable-specific decisions
		scenarios <- data.frame(temp = 1)

		for(decisions in unique(dec.var$Decision)) { # Add BAU option to each decision and merge decisions.
			temp <- as.character(dec.var[dec.var$Decision == decisions, "Option"])
			temp <- data.frame(Options = c(temp, "BAU"))
			colnames(temp) <- decisions
			scenarios <- merge(scenarios, temp)
		}
		var <- assessment@vars[[variables]]# as.character(dec$Name[1])]]
		var@output <- merge(scenarios[colnames(scenarios) != "temp"], var@output)
		for(s in 1:nrow(dec.var)) { # Each option row handled separately
			cell <- gsub("^[ \t]+|[ \t]+$", "", as.character(dec.var$Cell[s])) # Leading and trailing whitespaces removed.
			cell <- gsub(":[ \t]+", ":", cell) # Whitespaces removed after :
			cell <- gsub(";[ \t]+", ";", cell) # Whitespaces removed after ;
			cell <- gsub("[ \t]+:", ":", cell) # Whitespaces removed before :
			cell <- gsub("[ \t]+;", ";", cell) # Whitespaces removed before ;
			cell <- strsplit(cell, split = ";") # Separate cell identifiers (indices and locations)
			cell <- strsplit(cell[[1]], split = ":")
			cond <- as.character(dec.var$Decision[s])
			cond <- var@output[, cond] == as.character(dec.var$Option[s]) # Only the rows with the relevant option.
			for(r in 1:length(cell)) { # All Cell conditions extracted and combined with AND.
				cond <- cond * (var@output[, cell[[r]][1]] == cell[[r]][2])
			}
			cond <- as.logical(cond)
			if(dec.var$Change[s] == "Replace" ) 
				{var@output[cond, "Result"] <- dec.var$Result[s]}
			if(dec.var$Change[s] == "Add"     ) 
				{var@output[cond, "Result"] <- as.numeric(dec.var$Result[s]) + var@output[cond, "Result"]}
			if(dec.var$Change[s] == "Multiply") 
				{var@output[cond, "Result"] <- as.numeric(dec.var$Result[s]) * var@output[cond, "Result"]}
		}
		assessment@vars[[variables]] <- var
		}
	return(assessment)
}

# DECISIONS.APPLY ############## decisions.apply takes a decision table and applies that to an assessment.
## dec is a decision data.frame that must have columns Decision, Option, Variable, Cell, Change, Result. It can have several variables.
decisions.apply <- function(dec, assessment = NULL) {
	out <- as.list(unique(dec$Variable))
	names(out) <- as.character(unique(dec$Variable))

	for(variables in unique(dec$Variable)) { # Take one variable at a time.
		dec.var <- dec[dec$Variable == variables, ] # dec.var = variable-specific decisions
		scenarios <- data.frame(temp = 1)

		for(decisions in unique(dec.var$Decision)) { # Add BAU option to each decision and merge decisions.
			temp <- as.character(dec.var[dec.var$Decision == decisions, "Option"])
			temp <- data.frame(Options = c(temp, "BAU"))
			colnames(temp) <- decisions
			scenarios <- merge(scenarios, temp)
		}

		if(!is.null(assessment)) {
			var <- assessment@vars[[as.character(dec.var$Variable[1])]]
		} else {
			if(exists(as.character(dec.var$Variable[1]))) 
				{var <- get(as.character(dec.var$Variable[1]))
			} else {
				stop(as.character(dec.var$Variable[1]), " not defined!")
			}
		}
		var <- merge(scenarios[colnames(scenarios) != "temp"], var)

		for(s in 1:nrow(dec.var)) { # Each row in decision handled separately
			cell <- gsub("^[ \t]+|[ \t]+$", "", as.character(dec$Cell[s])) # Leading and trailing whitespaces removed.
			cell <- gsub(":[ \t]+", ":", cell) # Whitespaces removed after :
			cell <- gsub(";[ \t]+", ";", cell) # Whitespaces removed after ;
			cell <- gsub("[ \t]+:", ":", cell) # Whitespaces removed before :
			cell <- gsub("[ \t]+;", ";", cell) # Whitespaces removed before ;
			cell <- strsplit(cell, split = ";") # Separate cell identifiers (indices and locations)
			cell <- strsplit(cell[[1]], split = ":")
			cond <- as.character(dec.var$Decision[s])
			cond <- var[, cond] == as.character(dec.var$Option[s]) # Only the rows with the relevant option.
			for(r in 1:length(cell)) { # All Cell conditions extracted and combined with AND.
				cond <- cond * (var[, cell[[r]][1]] == cell[[r]][2])
			}
			cond <- as.logical(cond)
			if(dec.var$Change[s] == "Replace" ) {var[cond, "Result"] <- dec.var$Result[s]}
			if(dec.var$Change[s] == "Add"     ) {var[cond, "Result"] <- dec.var$Result[s] + var[cond, "Result"]}
			if(dec.var$Change[s] == "Multiply") {var[cond, "Result"] <- dec.var$Result[s] * var[cond, "Result"]}
		}

		out[[variables]] <- var
		}
	return(out)
}
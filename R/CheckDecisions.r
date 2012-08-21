######################
# CheckDecisions
#####################
# Function that checks for and applies decisions for a variable if such exist. 
# Inputs:
#   * variable - ovariable to be checked
#
# For the function to actually do anything, an odecision variable that matches the name DecVariable (where "Variable" is the
# name of the variable of interest) has to be defined before running CheckDecisions. Odecision class definition can be found 
# below. The function also makes use of a preset of decision effects which match a given string constant (i.e. "Multiply"). 
#######################

CheckDecisions <- function(variable) {
	if(exists(get(paste("Dec", variable@name, sep = "")))) {
		
		# Initialization: Setting up a data.frame upon which to apply desired decision - option specific effect. 
		
		dec <- get(paste("Dec", variable@name, sep = "")) # Get decision variable
		dectable <- dec@dectable # Decision input table in format described on http://en.opasnet.org/w/Decision
		temp2 <- data.frame(ignoremeiamadummy = NA) # A mergeable dummy for loop initial value
		for (i in unique(as.character(dectable$Decision))) { # Decisions form new indices. Here the decision indices are merged together.
			tempdec[[i]] <- dectable$Option[dectable$Decision == i]
			temp2 <- merge(temp2, tempdec)
		}
		temp2 <- temp2[!colnames(temp2) %in% "ignoremeiamadummy"] # remove dummy column
		out <- merge(variable@output, temp2) # Merge decisions with output. 
		
		# Conditions: Constructing a list of logical vectors which correspond to those rows of our new data.frame that we want to apply 
		# effects on. This will be done either by the user (as functions that take the data.frame as input and return a logical vector) 
		# while defining a custom decision or by parsing the decision table. 
		
		cond <- list()
		
		# First check if condition functions have been given. The default for the condition slot of an ovariable is a function that returns 0.
		
		if (length(dec@condition) == 1 & dec@condition[[1]](variable@output) == 0) { 
			
			# Build multiple condition vectors that correspond to a unique decision - option combination
			
			for (j in 1:nrow(dectable)) { 
				
				# In the decision table format conditions are given in the "Cell"-column separated by ";".
				
				sel1 <- strsplit(dectable[j, "Cell"], split = ";")[[1]] 
				
				# ":" defines index - location matches as a condition.
				
				sel2 <- strsplit(sel1, split = ":") # No need for lapply, since strsplit is a vectorized function and current list depth is 1.
				
				# Create a list of conditions which the decision and option specific condition vector consists of. 
				
				selection <- list() 
				for (k in 1:length(sel1)) { # For each condition separated by ";"
					if (length(sel2[[k]]) > 1) { # If ":" has been used for condition k
						locs <- strsplit(sel2[[k]][2], split = ",") # Split by "," for multiple locs per given index
						selection[[k]] <- out[, sel2[[k]][1]] %in% locs # Match our data.frame to the condition
					} #else { # Unimplemented code for (in)equality checks for numeric variables
						#if(grepl(">=")) {
							#a <- strsplit(sel1[[k]])
							#b <-  <- out[, sel2[[k]][1]]
							#selection[[k]]
						#}
					#}
				}
				
				# Match all conditions given for this decision - option combination.
				
				selection <- as.data.frame(selection)
				selection$optslice <- out[[as.character(dec[j,1])]] == dec[j,2] # We only want rows where the relevant option is in use to be affected
				selection <- as.matrix(selection)
				cond[[j]] <- apply(
					selection,
					1,
					all
				)
			}
		} else { # Otherwise use given condition functions.
			for (j in 1:length(dec@condition)) {
				cond[[j]] <- dec@condition[[j]](variable@output)
			}
		}
		
		# Effects
		
		if (length(dec@effect) == 1 & dec@effect[[1]](variable@output) == 0) {
			eff <- list()
			for (j in 1:nrow(dectable)) { 
				eff[[j]] <- EffectPreset(dectable$Change) # Returns a standard function from presets defined below.
			}
		} else {
			eff <- dec@effect
		}
		
		# Applying effects
		

		for (j in 1:nrow(dectable)) {
			out[
				cond[[j]], 
				paste(variable@name, "Result", split = "")
			] <- eff[[j]](
				out[
					cond[[j]], 
					paste(variable@name, "Result", split = "")
				], 
				dectable$Value[j]
			)
		}
		
		variable@marginal <- colnames(out) %in% c(colnames(variable@output)[variable@marginal], colnames(temp2))
		variable@output <- out
	}
	return(variable)
}

EffectPreset <- function(name) {
	if (name == "Add") return(function(x, y) {x + y})
	if (name == "Multiply") return(function(x, y) {x * y})
	if (name == "Replace") return(function(x, y) {y})
	if (name == "Remove") return(function(x, y) {NA})
}

setClass(
	"odecision", 
	representation(
		dectable	= "data.frame",
		condition	= "list",
		effect		= "list"
	),
	prototype = prototype(
		dectable	= data.frame(),
		condition	= list(function(variable){0}),
		effect		= list(function(variable){0})
	)
)

DecisionTableParser <- function(DTable){ # DTable is a data.frame
	for (i in unique(as.character(DTable$Variable))) {
		temp <- DTable[DTable$Variable == i,] # c("Decision", "Option")]
		out <- new("odecision", temp)
		assign(paste("Dec", i), out, envir = .GlobalEnv)
	}
}







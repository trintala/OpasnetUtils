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


######################
# CheckDecision
#####################
# It's a function that checks for and applies decisions if they exists.
#######################

DecisionTableParser <- function(DTable){ # DTable is a data.frame
	for (i in unique(as.character(DTable$Variable))) {
		temp <- DTable[DTable$Variable == i,] # c("Decision", "Option")]
		out <- new("odecision", temp)
		#out <- temp
		assign(paste("Dec", i), out, envir = .GlobalEnv)
	}
}

CheckDecisions <- function(variable) {
	if(exists(get(paste("Dec", variable@name, sep = "")))) {
		
		# Initialization
		
		dec <- get(paste("Dec", variable@name, sep = ""))
		temp <- dec@dectable
		temp2 <- data.frame(ignoremeiamadummy = NA)
		for (i in unique(as.character(temp$Decision))) {
			tempdec <- list()
			tempdec[[i]] <- temp$Option[temp$Decision == i]
			temp2 <- merge(temp2, tempdec)
		}
		temp2 <- temp2[!colnames(temp2) %in% "ignoremeiamadummy"]
		out <- merge(variable@output, temp2)
		
		# Conditions
		
		cond <- list()
		#cond <- dec@condition(variable@output)
		if (length(dec@condition) == 1 & dec@condition[[1]](variable@output) == 0) { # There are two options 
			# in using decisions: either define an odecision with a list of condition and effect functions or without 
			for (j in 1:nrow(temp)) { 	# We'll build multiple condition vectors that correspond to a 
				# unique decision - option combination
			#cond[[j]] <- function(input) { # input is an ovariable
				#temp2 <- input@output
				sel1 <- strsplit(temp[j, "Cell"], split = ";")[[1]]
				sel2 <- strsplit(sel1, split = ":")
				selection <- list() # a list of conditions which the condition vector consists of
				for (k in 1:length(sel1)) {
					if (length(sel2[[k]]) > 1) {
						selection[[k]] <- out[, sel2[[k]][1]] %in% sel2[[k]][-1] # select locations after :
					} #else {
						#if(grepl(">=")) {
							#a <- strsplit(sel1[[k]])
							#b <-  <- out[, sel2[[k]][1]]
							#selection[[k]]
						#}
					#}
				}
				#gregexpr(":", selection)
				selection <- as.data.frame(selection)
				selection$decslice <- out[[as.character(dec[j,1])]] == dec[j,2]
				selection <- as.matrix(selection)
				#selection <- gregexpr()
				cond[[j]] <- apply(
					selection,
					1,
					all
				)
			#}
			#cond[[j]] <- out
			}
		} else {
			for (j in 1:length(dec@condition)) {
				cond[[j]] <- dec@condition[[j]](variable@output)
			}
		}
		
		# Effects
		
		if (length(dec@effect) == 1 & dec@effect[[1]](variable@output) == 0) {
			eff <- list()
			for (j in 1:nrow(temp)) { # temp is the decision table from the odecision variable
				eff[[j]] <- EffectPreset(temp$Change) # returns a standard function
			}
		} else {
			eff <- dec@effect
		}
		
		# Applying effects
		
		#temp3 <- data.frame()
		for (j in 1:nrow(temp)) {
			out[
				cond[[j]], 
				paste(variable@name, "Result", split = "")
			] <- eff[[j]](
				out[
					cond[[j]], 
					paste(variable@name, "Result", split = "")
				], 
				temp$Value[j]
			)
			#temp3 <- rbind(temp3, eff[[j]](out[cond[[j]], ], temp$Value[j])) # Inputs for functions assumed to be 
			# data.frame and numeric
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






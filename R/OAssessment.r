# SETCLASS OASSESSMENT ################### Defines the S4 class "oassessment" which is the object type for open assessments.
setClass(
	"oassessment", 
	representation(
		dependencies  = "data.frame", 
		decisions     = "data.frame",
		probabilities = "data.frame",
		stakeholders  = "data.frame",
		vars          = "list"
	)
)

# MAKE.OASSESSMENT ########## make.oassessment creates S4 assessment from dependencies data.frame, including decisions, stakeholders, probabilities, and variables. 
########### NOTE! You must include the formula code from each variable page, otherwise formulas and dependencies are not updated. 
########### Parameters:
## dependencies: a data.frame that has the structure of oassessment@name (Columns: Name, Identifier, Direction, Result)
make.oassessment <- function(x) {
	x  <- fetch(x)
	decisions     <- fetch(x[x$Result == "decisions",     "Identifier"])
	stakeholders  <- fetch(x[x$Result == "stakeholders",  "Identifier"])
	probabilities <- fetch(x[x$Result == "probabilities", "Identifier"])
	dependencies  <- x[!x$Result %in% c("decisions", "stakeholders", "probabilities"), ]
	vars          <- list()
	for(i in 1:nrow(dependencies)) { # Objects with names as aliases are created and filled with data from Opasnet Base.
		cat("Initialising variable ", as.character(dependencies$Name[i]), ".\n", sep = "")
		ident <- as.character(dependencies$Identifier[i])
		data <- fetch(ident, direction = as.character(dependencies$Direction[i]))
		if(exists(paste("formula.", ident, sep = ""))) 
			{formula <- get(paste("formula.", ident, sep = ""))
		} else {
			formula <- function(dependencies) {return(0)}
		}
		if(exists(paste("dependencies.", ident, sep = "")))
			{depend <- get(paste("dependencies.", ident, sep = ""))
		} else {
			depend <- data.frame()
		}
		vars[[i]] <- make.ovariable(
			name = as.character(dependencies$Result[i]),
			data = data,
			formula = formula,
			dependencies = depend
		)
	}
	names(vars) <- dependencies$Result
	assessment <- new("oassessment", 
		dependencies  = dependencies, 
		decisions     = decisions,
		stakeholders  = stakeholders,
		probabilities = probabilities,
		vars          = vars
	)

	return(assessment)
}
# INIT.ASSESSMENT ########## init.assessment creates S4 assessment from dependencies data.frame, including decisions, stakeholders, probabilities, and variables. 
########### NOTE! You must include the formula code from each variable page, otherwise formulas and dependencies are not updated. 
########### Parameters:
## dependencies: a data.frame that has the structure of oassessment@name (Columns: Name, Identifier, Direction, Result)
init.assessment <- function(dependencies) 
{
	dependencies  <- fetch(dependencies)
	decisions     <- fetch(dependencies[dependencies$Result == "decisions", "Identifier"])
	stakeholders  <- fetch(dependencies[dependencies$Result == "stakeholders", "Identifier"])
	probabilities <- fetch(dependencies[dependencies$Result == "probabilities", "Identifier"])
	dependencies  <- dependencies[!dependencies$Result %in% c("decisions", "stakeholders", "probabilities"), ]
	vars          <- list()
	for(x in 1:nrow(dependencies)) { # Objects with names as aliases are created and filled with data from Opasnet Base.
		cat("Initialising variable ", as.character(dependencies$Name[x]), ".\n", sep = "")
		ident <- as.character(dependencies$Identifier[x])
		temp <- tidy(op_baseGetData("opasnet_base", ident), direction = as.character(dependencies$Direction[x]))
		temp <- make.ovariable(temp)
		if(exists(paste("formula.", ident, sep = ""))) 
			{temp@formula <- get(paste("formula.", ident, sep = ""))}
		if(exists(paste("dependencies.", ident, sep = "")))
			{temp@dependencies <- get(paste("dependencies.", ident, sep = ""))}
		vars[as.character(dependencies$Result[x])] <- temp
	}
	assessment <- new("oassessment", 
		dependencies  = dependencies, 
		decisions     = decisions,
		stakeholders  = stakeholders,
		probabilities = probabilities,
		vars          = vars
	)

	return(assessment)
# Does it cause problems to use the wide direction? Anyway, there should be exactly one Result column.
## No it doesn't if the Observation columns are such as Unit, Result, Description.
}
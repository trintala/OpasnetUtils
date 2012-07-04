# SETMETHOD PRINT ######################## print ovariable contents

setMethod(
	f = "print",
	signature = signature(x = "oassessment"),
	definition = function(x) {
		cat("Dependencies\n")
		print(xtable(x@dependencies), type = 'html')
		cat("Decisions\n")
		print(xtable(x@decisions), type = 'html')
		cat("Stakeholders\n")
		print(xtable(x@stakeholders), type = 'html')
		cat("Probabilities\n")
		print(xtable(x@probabilities), type = 'html')

		cat("\n\nThe list of variables in this assessment.\n")
		for(k in 1:length(x@vars)) {
			cat("<b>Variable", names(x@vars)[k], "</b> <p>\n\n")
			print(x@vars[[k]])
		}
	}
)

setMethod(
	f = "print",
	signature = signature(x = "ovariable"),
	definition = function(x, iter = 1) {
		cat("<b>Ovariable", x@name, "</b>\n")
		cat("Output\n")
		print(xtable(x@output[x@output$Iter %in% iter, ]), type = 'html')
		cat("Formula\n")
		print(x@formula)
		cat("Dependencies\n")
		print(x@dependencies)
	}
)
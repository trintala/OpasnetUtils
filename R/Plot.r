# SETMETHOD PLOT ################ plot diagrams about ovariable data

setMethod(
	f = "plot",
	signature = signature(x = "ovariable"),
	definition = function(x) {
		plot(
			x    = x@output[, paste("Source", x@name, sep = "")], 
			y    = x@output$Result, 
			xlab = paste("Source", x@name, sep = ""), 
			ylab = x@output[x@output[, paste("Source", x@name, sep = "")] == "Data", "Unit"][1], 
			main = x@name
		)
	}
)

setMethod(
	f = "plot",
	signature = signature(x = "oassessment"),
	definition = function(x) {
		for(i in names(x@vars)) {
			plot(x@vars[[i]])
		}
	}
)
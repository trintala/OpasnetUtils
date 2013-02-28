# SETMETHOD summary ################### Summary defines how summaries of ovariables are shown.
setMethod(
		f = "summary", 
		signature = signature(object = "ovariable"), 
		definition = function(object, ...) {
			test <- paste(object@name, "Result", sep = "") %in% colnames(object@output)
			rescol <- ifelse(test, paste(object@name, "Result", sep = ""), "Result")
			object@output <- object@output[ , -grep("Description|Source", colnames(object@output))]
			if("Iter" %in% colnames(object@output)) {
				object@output <- object@output[object@output$Iter == 1, ]
			}
			if(nrow(object@output) > 200) {
				object@output <- object@output[1:200, ]
			}
			return(object@output)
# Kehityskohteita:
			## Lasketaan mean, median, min, max, 95%CI, SD resultista selittävien sarakkeiden suhteen.
		}
)
# SETMETHOD UPDATE ########## update updates the output of an ovariable based on data and function.
####### The function assumes that object@data is always available, but object@dependencies may be empty.
setMethod(
	f = "update", 
	signature = "ovariable", 
	definition = function(object) {
		dat  <- data.frame(Source = "Data", interpret(object@data))
		if(ncol(object@dependencies) == 0 & nrow(object@dependencies) == 0) {
			object@output <- dat
		} else {
			form <- object@formula(object@dependencies)
			if(is.vector(form)) {form <- data.frame(Result = form)}
			if(class(form) == "ovariable") {form <- form@output}
			form <- data.frame(Source = "Formula", form)
			object@output <- orbind(dat, form)
		}
		colnames(object@output)[colnames(object@output) == "Source"] <- paste("Source", object@name, sep = "")
		object@marginal <- c(TRUE, object@marginal) # This alone is not enough; orbind must operate with marginals as well.
		return(object)
	}
)

#### This code was taken out of update. It is still needed somewhere but not here.
#		dep <- object@dependencies
#		for(i in 1:length(dep)) {
#			if(class(dep[[i]]) == "ovariable") {
#				dep[[i]] <- dep[[i]]@output
#			} else {
#				if(length(grep("Op_(en|fi)", dep[[i]])) > 0) {
#					dep[[i]] <- op_baseGetData("opasnet_base", dep[[i]])}
#				else {
#					if(class(dep[[i]]) != "data.frame" & !is.numeric(dep[[i]])) {
#						dep[[i]] <- get(dep[[i]])
#					}
#				}
#			}
#		}
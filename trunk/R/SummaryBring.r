# SUMMARY.BRING ############## summary.bring: Bring parts of summary table
#   page is the page identifier for the summary table.
summary.bring <- function(page, base = "opasnet_base"){
	data <- tidy(op_baseGetData(base, page))
	pages <- levels(data$Page)

	## temp contains the additional information that is not on the actual data table.
	temp <- data[, !colnames(data) == "Observation"]
	temp <- reshape(temp, idvar = "Page", timevar = "Index", direction = "wide")
	colnames(temp) <- ifelse(substr(colnames(temp), 1, 7) == "Result.", substr(colnames(temp), 8, 50), colnames(temp))

	## Get all data tables one at a time and combine them.
	for(i in 1:length(pages)){
		out <- op_baseGetData("opasnet_base", pages[i])
		out <- tidy(out)
		cols <- colnames(out)[!colnames(out) %in% c("Observation", "Result")]
		out <- reshape(out, timevar = "Observation", idvar = cols, direction = "wide")
		colnames(out) <- ifelse(substr(colnames(out), 1, 7) == "Result.", substr(colnames(out), 8, 50), colnames(out))
		out <- merge(temp[temp$Page == pages[i], ][colnames(temp) != "Page"], out)

		## Check that all data tables have all the same columns before you combine them with rbind.
		if(i == 1){out2 <- out} else {
			addcol <- colnames(out2)[!colnames(out2) %in% colnames(out)]
			if(length(addcol) > 0) {
				temp <- as.data.frame(array("*", dim = c(1,length(addcol))))
				colnames(temp) <- addcol
				out <- merge(out, temp)}
			addcol <- colnames(out)[!colnames(out) %in% colnames(out2)]
			if(length(addcol) > 0) {
				temp <- as.data.frame(array("*", dim = c(1,length(addcol))))
				colnames(temp) <- addcol
				out2 <- merge(out2, temp)}

			## Combine data tables.
			out2 <- rbind(out2, out)
		}
	}
	return(out2)
}
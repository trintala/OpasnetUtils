GetData <- function(ident, act = NULL) {
	object <- fromJSON(
		paste(
			readLines(
				paste(
					"http://cl1.opasnet.org/opasnet_base_2/index.php?ident=", 
					ident, 
					sep = ""
				)
			), 
			collapse = ""
		)
	)
	
	acts <- data.frame()
	a <- 0
	for (i in object[[2]]) {
		a <- a + 1
		acts <- rbind(acts, data.frame(id = i[[1]]$id, slot = a))
	}
	
	if(is.null(act)) {
		act <- sort(acts$id, decreasing = TRUE)[1] # choose the latest
	}
	
	key <- fromJSON(
		paste(
			readLines(
				paste(
					"http://cl1.opasnet.org/opasnet_base_2/index.php?ident=", 
					ident, 
					"&act=", 
					act, 
					sep = ""
				)
			), 
			collapse = ""
		)
	)
	
	key <- key$key
	
	out <- data.frame()
	temp <- NULL
	first <- TRUE
	while (!is.null(temp) | first) {
		first <- FALSE
		
		temp <- fromJSON(
			paste(
				readLines(
					paste(
						"http://cl1.opasnet.org/opasnet_base_2/index.php?key=", 
						key, 
						sep = ""
					)
				), 
				collapse = ""
			)
		)
		
		#gregexpr("^\\[|\\]$", temp$data)
		temp <- gsub("^\\[|\\]$", "", temp$data)
		#gregexpr("\\},\\{", temp)
		temp <- gsub("^\\{|\\}$", "", temp)
		temp <- strsplit(temp, split = "\\},\\{")
		#gregexpr("\\(.*\\)",temp[[1]][1]) # check for iterated cells
		temp <- lapply(temp, strsplit, split = ",")
		temp <- temp[[1]]
		temp <- lapply(temp, strsplit, split = ":")
		#for (i in temp) {
		#	a <- as.data.frame(i)
		#	colnames(a) <- unlist(a[1,])
		#}
		#temp <- lapply(temp, as.data.frame)
		#temp <- lapply(temp, as.matrix)
		temp <- lapply(temp, dataframefier)
		temp <- do.call("rbind", temp)
		
		out <- rbind(out, temp)
	}
	
	indices <- object[[2]][[acts$slot[acts$id == act]]][[2]]
	#indices <- lapply(indices, as.data.frame)
	temp <- data.frame()
	for (i in indices) {
		temp <- rbind(temp, data.frame(Name = i$name, id = i$ident))
	}
	indices <- temp
	
	colnames(out) <- gsub("^\"|\"$", "", colnames(out))
	out <- out[,!colnames(out) %in% c("sid", "aid", "mean", "sd")]
	for(i in 1:ncol(out)) {
		out[[i]] <- gsub("^\"|\"$", "", out[[i]])
		out[[i]] <- gsub("^ *| *$", "", out[[i]])
		temp <- indices[indices$id == colnames(out)[i], "Name"]
		if (length(temp) == 1) colnames(out)[i] <- as.character(temp)
		temp <- suppressWarnings(as.numeric(out[[i]]))
		if (sum(is.na(temp)) == 0) out[[i]] <- temp 
	}
	colnames(out)[colnames(out) == "res"] <- "Result"
	return(out)
}

dataframefier <- function(x) {
	x <- as.data.frame(x)
	colnames(x) <- unlist(x[1,])
	x <- x[-1,]
}

#objlist <- fromJSON(
#	paste(
#		readLines(
#			paste(
#				"http://cl1.opasnet.org/opasnet_base_2/index.php", 
#				#ident, 
#				sep = ""
#			)
#		), 
#		collapse = ""
#	)
#)

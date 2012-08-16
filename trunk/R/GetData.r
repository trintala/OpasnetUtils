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
		
		temp <- fromJSON(temp$data)
		
		temp <- lapply(temp, list.to.data.frame)
		
		lengths <- lapply(temp, nrow)
		
		temp <- do.call("rbind", temp)
		
		if (sum(unlist(lengths) > 1) > 0) {
			iterations <- lapply(lengths, f.iter)
			temp$Iter <- unlist(iterations)
		}
		
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

list.to.data.frame <- function(x) { # As.data.frame equivalent, but allows one list as longer than others. 
	do.call("data.frame", x) # Which is the case with opb2's probabilistic results. 
}

f.iter <- function(x) {
	1:x
}

objlist <- fromJSON(
	paste(
		readLines(
			paste(
				"http://cl1.opasnet.org/opasnet_base_2/index.php", 
				#ident, 
				sep = ""
			)
		), 
		collapse = ""
	)
)


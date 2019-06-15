#######################################

#' CollapseMarginal Collapses marginals by applying sums, means or samples
#' 
#' Also loses all non-marginal columns except the relevant Result
#' Parse-able table should have columns Index, Function and Probs
#' @param variable ovariable to be collapsed
#' @param cols list of column name vectors to collapse. If fun is "sample", only one column name in vector.
#' @param fun function name to used in collapsing. If function is not given "mean" is assumed
#' @param probs list of probability vectors for each column. Can be left out for equal weights sampling
#' @return ovariable without columns in cols
#####################################

CollapseTableParser <- function (CTable, env = .GlobalEnv) # There is a bug in code. This is adjusted for non-sample functions.
{ 
  for (i in unique(as.character(CTable$Variable))) {
    temp <- CTable[CTable$Variable == i, ]
    cols <- strsplit(as.character(temp[["Index"]]), ",") # Assumes one row per index for sample and one row per ovariable for other functions
    cols <- lapply(cols, trimws)
    probs <- strsplit(as.character(temp[["Probs"]]), ",") 
    probs <- lapply(probs, as.numeric) 
    fun <- temp[["Function"]] 
    out <- list(cols = cols, probs = probs, fun = fun) 
    assign(paste("Col", i, sep = ""), out, envir = env)
  } 
}

CheckCollapse <- function(variable, indent = 0, verbose = TRUE, ...) {
	if (exists(paste("Col", variable@name, sep = ""))) {
		if (verbose) cat(rep("-", indent), "Processing", variable@name, "marginal collapses", "...")
		Col <- get(paste("Col", variable@name, sep = ""))
		variable <- CollapseMarginal(variable, Col$cols, Col$fun, Col$probs, ...)
		if (verbose) cat(" done!\n")
	}
	return(variable)
}

CollapseMarginal <- function(variable, cols, fun = "mean", probs = NULL, ...) { # cols and probs are lists. If cols is not a list, it is converted to a list of leangth 1.
  if (length(fun) == 0) fun <- "mean"
  if (is.na(fun)) stop("No function given to collapse with!\n")
  if(length(fun)==1) fun <- rep(fun,length(cols)) # Make sure that function works if fun is omitted.
  if(is.null(probs)) probs <- rep(list(NA),length(cols)) # Make sure probs is a list with same length as cols
  probs[sapply(probs,length)==0] <- NA # Make sure empty probs are NA
  if(!is.list(cols)) cols <- list(cols)
  
	# If no probabilities given use function
	# Also if given function is sample then equal weights are used and this section is skipped
#	if (fun == "unkeep") {
#	  out <- unkeep(variable, cols)
#	  return(out)
#	}
  out <- variable@output
  marginals <- colnames(out)[variable@marginal]
  for(i in 1:length(cols)) {
  	if (is.na(probs[[i]][1]) & fun[[i]] != "sample") {
  		out <- oapply(variable, FUN = fun[[i]], cols = cols[[i]], na.rm = TRUE)
  		return(out)
  	}
  	
  	# Else use sample with option of given probabilities
  	
#  	if (!is.list(probs) & is.numeric(probs)) probs <- list(probs)
#  	if (!is.null(probs) & length(probs) != length(cols)) stop("Number of columns does not match number of probability vectors given!\n")
  	if ("Iter" %in% colnames(out)) {
  		N <- max(as.numeric(as.character(out$Iter)))
  	} else {
  		N <- get("N", openv)
  	}
#  	for (i in 1:length(cols)) { # Not needed because with sample, length(cols[[i]]) is always 1 (if not, others are ignored)
  		b <- probs[[i]]
  		locs <- sort(unique(out[[cols[[i]][1]]])) # This is sorted so that the order is always known.
  		if (is.null(b)) b <- rep(1, length(locs)) # dont see why NA would turn up here, but hey lets just be sure
  		if (any(is.na(b))) b <- rep(1, length(locs))
  		if (length(b) != length(locs)) {
  			stop(paste("Number of locations does not match number of probabilities given for ", cols[i], "!\n", sep = ""))
  		}
  		selection <- data.frame( # This does NOT follow oapply logic, i.e. ignores all other marginals than Iter.
  			Iter = 1:N, 
  			sample(
  				locs, 
  				size = N, 
  				replace = TRUE, 
  				prob = b
  			)
  		)
  		colnames(selection)[2] <- cols[i]
  		out <- merge(selection, out)
#  	}
  }
	variable@output <- out
	variable@marginal <- colnames(out) %in% c(marginals, "Iter") & ! colnames(out) %in% cols
	return(variable)
}

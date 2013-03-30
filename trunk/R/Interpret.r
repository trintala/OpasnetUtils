# INTERPRET ################### interpret takes a vector and makes a data.frame out of it (to be used in e.g. make.ovariable).
### It also changes abbreviations into probability samples.
# Lognormal distribution parametrization functions
lmean <- function(parmean, parsd) {return(log(parmean)-log(1+(parsd^2)/(parmean^2))/2)}
lsd <- function(parmean, parsd) {return(log(1+(parsd^2)/(parmean^2)))}

# Actual interpretation function. Takes already pre-processed information and returns a distribution. This is a separate function because
# it isn't vectorizable (easily anyway).
interpf <- function(
	n, 
	res.char, 
	brackets.pos, 
	brackets.length, 
	minus, 
	minus.length, 
	minus.exists, 
	plusminus, 
	plusminus.length, 
	plusminus.pos,
	doublePoint, 
	minus.relevant, 
	fromzero, 
	dbug = FALSE
	) {

	if(doublePoint[1] > 0) {
		tempArgs <- sort(as.numeric(unlist(strsplit(res.char, "\\:"))))
		if(dbug) cat("Triangular distribution. \n")
		return(rtriangle(n,tempArgs[1],tempArgs[3],tempArgs[2]))
	}
	if(brackets.pos > 0) {
		n.minus.inside.brackets <- sum(minus.relevant > brackets.pos & minus.relevant < brackets.pos + brackets.length)
		imean <- as.numeric(substr(res.char, 1, brackets.pos - 1))
		if(n.minus.inside.brackets == 1) {
			ici <- c(as.numeric(substr(res.char, brackets.pos + 1, minus.relevant[minus.relevant > brackets.pos] - 1)), as.numeric(substr(res.char, 
				minus.relevant[minus.relevant > brackets.pos] + 1, brackets.pos + brackets.length - 2)))
			if((ici[2] - imean) / (imean - ici[1]) < 1.5) {
				if(dbug) cat("Normal distribution. \n")
				isd <- sum(abs(ici - imean) / 2) / qnorm(0.975)
				return(rnorm(n, imean, isd))
			} else {
				if(dbug) cat("Lognormal distribution. \n")
				isd <- sum(abs(log(ici) - log(imean)) / 2) / qnorm(0.975)
				return(exp(rnorm(n, log(imean), isd)))
			}
		} else 
		if(n.minus.inside.brackets %in% c(2,3)) {
			ici <- c(as.numeric(substr(res.char, brackets.pos + 1, minus.relevant[minus.relevant > brackets.pos][2] - 1)), as.numeric(substr(res.char, 
				minus.relevant[minus.relevant > brackets.pos][2] + 1, brackets.pos + brackets.length - 2)))
			isd <- sum(abs(ici - imean) / 2) / qnorm(0.975)
			if(dbug) cat("Normal distribution. \n")
			return(rnorm(n, imean, isd))
		}
		warning(paste("Unable to interpret \"", res.char, "\"", sep = ""))
		return(rep(NA, n))
	}
	if(plusminus.pos > 0) {
		if(dbug) cat("Normal distribution. \n")
		return(rnorm(n, as.numeric(substr(res.char, 1, plusminus.pos - 1)), as.numeric(substr(res.char, plusminus.pos + plusminus.length, nchar(res.char)))))
	}
	if(minus.exists) {
		if(length(minus.relevant) == 1) {
			a <- as.numeric(substr(res.char, 1, minus.relevant - 1))
			b <- as.numeric(substr(res.char, minus.relevant + 1, nchar(res.char)))
			if(a / b >= 1/100 | a == 0) {
				if(dbug) cat("Uniform distribution. \n")
				return(runif(n, a, b))
			} else {
				if(dbug) cat("Loguniform distribution. \n")
				return(exp(runif(n, log(a), log(b))))
			}
		}
		if(length(minus.relevant) %in% c(2,3)) { # If there is more than one '-' we're porbably dealing with negative boundaries. (More than 3 will produce NAs.)
			if(dbug) cat("Uniform distribution. \n")
			# Assume that negative number is always first.
			return(runif(n, as.numeric(substr(res.char, 1, minus.relevant[2] - 1)), as.numeric(substr(res.char, minus.relevant[2] + 1, nchar(res.char)))))
		}
	}
	if(sum(unlist(strsplit(res.char, ""))==";") > 0) {
		if(dbug) cat("Discrete random samples. \n")
		return(sample(as.numeric(unlist(strsplit(res.char, ";"))), n, replace = TRUE))
	}
	if(fromzero[[1]][1] == 1) {
		temp <- interpret(
			paste("0-", substr(res.char, 2, nchar(res.char)), sep = ""), n, dbug = dbug
		)
		return(
			temp$Result
		)
	}
	warning(paste("Unable to interpret \"", res.char, "\"", sep = ""))
	return(rep(NA, n))
}

# The following function processes character strings and loops the interpretation function.
input.interp <- function(res.char, n = 1000, dbug = FALSE) {
	res.char <- gsub(" ", "", res.char)
	res.char <- gsub(",", ".", res.char)
	plusminus <- gregexpr(paste("\\+-|", rawToChar(as.raw(177)), sep = ""), res.char) # saattaa osoittautua ongelmaksi enkoodauksen vuoksi
	plusminus.length <- as.numeric(unlist(sapply(plusminus, attributes)))
	plusminus.pos <- unlist(plusminus)
	minus <- gregexpr("-", res.char)
	minus.length <- sapply(minus, length)
	minus.exists <- unlist(minus)[cumsum(c(0, minus.length[-length(minus.length)])) + 1] > 0
	brackets <- gregexpr("\\(.*\\)", res.char) # matches for brackets "(...)"
	brackets.length <- as.numeric(unlist(sapply(brackets, attributes)[1,]))
	brackets.pos <- unlist(brackets)
	doublePoint <- gregexpr(":", res.char)
	fromzero <- gregexpr("<", res.char)
	out <- list()
	for(i in 1:length(res.char)) {
		val <- suppressWarnings(as.numeric(res.char[i]))
		if(is.na(val)) {
			minus.relevant <- unlist(minus)[(cumsum(c(0, minus.length)) + 1)[i]:cumsum(minus.length)[i]]
			out[[i]] <- interpf(n, res.char[i], brackets.pos[i], brackets.length[i], minus[i], minus.length[i], minus.exists[i], plusminus[[i]], 
				plusminus.length[i], plusminus.pos[i], doublePoint[[i]], minus.relevant, fromzero[i], dbug
			)
		} else out[[i]] <- rep(val, n)
	}
	out
}

# Assisting function for data.frame wrapper.
f.iter <- function(x) {
	1:x
}

# Data.frame wrapper for the functions.
interpret <- function(idata, N = 1000, rescol = "Result", dbug = FALSE, ...) {
	if (!is.data.frame(idata)) idata <- as.data.frame(idata)
	if (ncol(idata) == 0) stop("Empty data.frame!")
	if (!rescol %in% colnames(idata)) stop(paste("No \"", rescol, "\" column found", sep = ""))
	temp <- input.interp(idata[[rescol]], N, dbug)
	temp.lengths <- sapply(temp, length)
	if (ncol(idata) == 1) {
		out <- list()
		out[[rescol]] <- unlist(temp)
		out <- as.data.frame(out)
	} else {
		out <- data.frame(idata[rep(1:nrow(idata), times = temp.lengths),])
		out[[rescol]] <- unlist(temp)
	}
	dim(temp.lengths) <- length(temp.lengths)
	out$Iter<- c(apply(temp.lengths, 1, f.iter))
	out
}

setGeneric("interpret")

setMethod(
	f = "interpret",
	signature = signature(idata = "character"),
	definition = function(idata, N = 1000, dbug = FALSE) {
		callGeneric(data.frame(Result = idata), N = N, dbug = dbug)
	}
)

#setMethod(
#		f = "interpret",
#		signature = signature(idata = "interpret"),
#		definition = function(idata, N = 1000, dbug = FALSE) {
#			callGeneric(data.frame(Result = idata), N = N, dbug = dbug)
#		}
#)

# Interpreting empty locations in indices

fillna <- function(object, marginals) {
	a <- dropall(object)
	for(i in marginals) {
		a[[i]] <- as.factor(a[[i]])
		a1 <- a[!is.na(a[[i]]), ]
		a2 <- a[is.na(a[[i]]), ][-i]
		addition <- data.frame(A = levels(a[[i]]))
		colnames(addition) <- colnames(a)[i]
		a2 <- merge(addition, a2)
		a <- rbind(a1, a2)
	}
	return(a)
}

#interpret("500(490-5000)", N = 2, dbug= TRUE)

#interpret("1;2;3;4", N = 20, dbug = TRUE)

#interpret("<9", N = 4, dbug = TRUE)

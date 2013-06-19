\name{interpret}
\alias{interpret}
\title{
Parse human readable distribution definitions
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
interpret(idata, N = 1000, rescol = "Result", dbug = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{idata}{
%%     ~~Describe \code{idata} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{rescol}{
%%     ~~Describe \code{rescol} here~~
}
  \item{dbug}{
%%     ~~Describe \code{dbug} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (idata, N = 1000, rescol = "Result", dbug = FALSE, 
    ...) 
standardGeneric("interpret"), generic = structure("interpret", package = "OpasnetUtils"), package = "OpasnetUtils", group = list(), valueClass = character(0), signature = c("idata", 
"N", "rescol", "dbug"), default = structure(function (idata, 
    N = 1000, rescol = "Result", dbug = FALSE, ...) 
{
    if (!is.data.frame(idata)) 
        idata <- as.data.frame(idata)
    if (ncol(idata) == 0) 
        stop("Empty data.frame!")
    if (!rescol \%in\% colnames(idata)) 
        stop(paste("No \"", rescol, "\" column found", sep = ""))
    temp <- input.interp(idata[[rescol]], N, dbug)
    temp.lengths <- sapply(temp, length)
    if (ncol(idata) == 1) {
        out <- list()
        out[[rescol]] <- unlist(temp)
        out <- as.data.frame(out)
    }
    else {
        out <- data.frame(idata[rep(1:nrow(idata), times = temp.lengths), 
            ])
        out[[rescol]] <- unlist(temp)
    }
    dim(temp.lengths) <- length(temp.lengths)
    out$Iter <- c(apply(temp.lengths, 1, f.iter))
    out
  }, target = structure("ANY", class = structure("signature", package = "methods"), .Names = "idata", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "idata", package = "methods"), generic = structure("interpret", package = "OpasnetUtils"), class = structure("derivedDefaultMethod", package = "methods")), skeleton = (structure(function (idata, 
    N = 1000, rescol = "Result", dbug = FALSE, ...) 
{
    if (!is.data.frame(idata)) 
        idata <- as.data.frame(idata)
    if (ncol(idata) == 0) 
        stop("Empty data.frame!")
    if (!rescol \%in\% colnames(idata)) 
        stop(paste("No \"", rescol, "\" column found", sep = ""))
    temp <- input.interp(idata[[rescol]], N, dbug)
    temp.lengths <- sapply(temp, length)
    if (ncol(idata) == 1) {
        out <- list()
        out[[rescol]] <- unlist(temp)
        out <- as.data.frame(out)
    }
    else {
        out <- data.frame(idata[rep(1:nrow(idata), times = temp.lengths), 
            ])
        out[[rescol]] <- unlist(temp)
    }
    dim(temp.lengths) <- length(temp.lengths)
    out$Iter <- c(apply(temp.lengths, 1, f.iter))
    out
  }, target = structure("ANY", class = structure("signature", package = "methods"), .Names = "idata", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "idata", package = "methods"), generic = structure("interpret", package = "OpasnetUtils"), class = structure("derivedDefaultMethod", package = "methods")))(idata, 
    N, rescol, dbug, ...), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

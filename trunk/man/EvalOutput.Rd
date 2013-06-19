\name{EvalOutput}
\alias{EvalOutput}
\title{
Evalaute ovariable output
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EvalOutput(variable, fillna = FALSE, indent = 0, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variable}{
%%     ~~Describe \code{variable} here~~
}
  \item{fillna}{
%%     ~~Describe \code{fillna} here~~
}
  \item{indent}{
%%     ~~Describe \code{indent} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
function (variable, fillna = FALSE, indent = 0, verbose = FALSE, 
    ...) 
{
    if (verbose) 
        cat(rep("-", indent), "Evaluating", variable@name, "...")
    ComputeDependencies(variable@dependencies, fillna = fillna, 
        indent = indent + 1, verbose = verbose, new_code = TRUE, 
        ...)
    variable <- ddata_apply(variable, ...)
    if (nrow(variable@data) > 0) {
        colnames(variable@data)[colnames(variable@data) \%in\% 
            "Result"] <- paste(variable@name, "Result", sep = "")
        rescol <- paste(variable@name, "Result", sep = "")
        if (!is.numeric(variable@data[[rescol]]) & !is.null(variable@data[[rescol]])) {
            a <- interpret(variable@data, rescol = rescol, ...)
        }
        else a <- variable@data
    }
    else a <- variable@data
    b <- variable@formula(variable@dependencies, indent = indent, 
        verbose = verbose, ...)
    tempmarginals <- character()
    if (class(b)[1] == "ovariable") {
        if (length(b@marginal) > 0) {
            tempmarginals <- c(tempmarginals, colnames(b@output)[b@marginal], 
                paste(variable@name, "Source", sep = ""))
        }
        b <- b@output
    }
    if (is.numeric(b) & nrow(a) == 0) {
        if (verbose) 
            cat("\n")
        stop(paste("No proper data nor formula defined for ", 
            variable@name, "! (Numeric formula return and 0 rows data)\n", 
            sep = ""))
    }
    if (is.numeric(b)) {
        colnames(a)[colnames(a) == rescol] <- paste(variable@name, 
            "Result", sep = "")
        a[, paste(variable@name, "Source", sep = "")] <- "Data"
        variable@output <- a
        if (verbose) 
            cat(" done!\n")
    }
    else if (nrow(a) == 0) {
        colnames(b)[colnames(b) \%in\% "Result"] <- paste(variable@name, 
            "Result", sep = "")
        b[, paste(variable@name, "Source", sep = "")] <- "Formula"
        variable@output <- b
        if (length(tempmarginals) > 1) 
            variable@marginal <- colnames(variable@output) \%in\% 
                tempmarginals
        if (verbose) 
            cat(rep("-", indent), " done!\n")
    }
    else {
        colnames(a)[colnames(a) == rescol] <- "FromData"
        colnames(b)[colnames(b) \%in\% c(paste(variable@name, "Result", 
            sep = ""), "Result")] <- "FromFormula"
        temp <- melt(merge(a, b, all = TRUE, ...), measure.vars = c("FromData", 
            "FromFormula"), variable.name = paste(variable@name, 
            "Source", sep = ""), value.name = paste(variable@name, 
            "Result", sep = ""), ...)
        levels(temp[[paste(variable@name, "Source", sep = "")]]) <- gsub("^From", 
            "", levels(temp[[paste(variable@name, "Source", sep = "")]]))
        variable@output <- temp
        if (length(tempmarginals) > 1) 
            variable@marginal <- colnames(variable@output) \%in\% 
                tempmarginals
        if (verbose) 
            cat(rep("-", indent), " done!\n")
    }
    variable <- CheckMarginals(variable, indent = indent, verbose = verbose, 
        ...)
    if (fillna) {
        ret <- tryCatch(variable@output <- fillna(variable@output, 
            1:ncol(variable@output)[variable@marginal]), error = function(e) return(NULL))
        if (is.null(ret)) 
            warning("Unable to FillNA.")
    }
    return(variable)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

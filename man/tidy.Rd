\name{tidy}
\alias{tidy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Format database output for use in ovariables
}
\description{
Various standard operations applied on ovariable data from the OpasnetBase.
}
\usage{
tidy(data, objname = "", idvar = "Obs", direction = "wide", widecol = NULL, base1 = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{objname}{
%%     ~~Describe \code{objname} here~~
}
  \item{idvar}{
%%     ~~Describe \code{idvar} here~~
}
  \item{direction}{
%%     ~~Describe \code{direction} here~~
}
  \item{widecol}{
%%     ~~Describe \code{widecol} here~~
}
  \item{base1}{
%%     ~~Describe \code{base1} here~~
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
%%  ~~who you are~~
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
function (data, objname = "", idvar = "Obs", direction = "wide", 
    widecol = NULL, base1 = FALSE) 
{
    if (base1) {
        data$Result <- ifelse(is.na(data$Result.Text), data$Result, 
            as.character(data$Result.Text))
    }
    data <- data[, !colnames(data) \%in\% c("id", "Result.Text")]
    if ("obs.1" \%in\% colnames(data)) {
        data[, "Obs"] <- data[, "obs.1"]
        data <- data[, colnames(data) != "obs.1"]
    }
    else if ("obs" \%in\% colnames(data)) {
        data[, "Obs"] <- data[, "obs"]
        data <- data[, colnames(data) != "Obs"]
    }
    else if ("Row" \%in\% colnames(data)) {
        data[, "Obs"] <- data[, "Row"]
        data <- data[, colnames(data) != "Row"]
    }
    if (direction == "wide") {
        if (is.null(widecol)) {
            if ("Observation" \%in\% colnames(data)) {
                widecol <- "Observation"
            }
            else if ("Parameter" \%in\% colnames(data)) {
                widecol <- "Parameter"
            }
            else if ("Havainto" \%in\% colnames(data)) {
                widecol <- "Havainto"
            }
            else widecol <- NA
        }
        if (!is.na(widecol)) {
            cols <- levels(data[[widecol]])
            data <- reshape(data, idvar = idvar, timevar = widecol, 
                v.names = "Result", direction = "wide")
            data <- data[colnames(data) != "Obs"]
            colnames(data) <- gsub("^Result.", objname, colnames(data))
            for (i in paste(objname, cols, sep = "")) {
                a <- suppressWarnings(as.numeric(as.character(data[, 
                  i])))
                if (sum(is.na(a)) == 0) 
                  data[, i] <- a
                else data[, i] <- factor(data[, i])
            }
            colnames(data)[grepl(paste("^", objname, "result", 
                sep = ""), colnames(data))] <- paste(objname, 
                "Result", sep = "")
            colnames(data)[grepl(paste("^", objname, "Amount", 
                sep = ""), colnames(data))] <- paste(objname, 
                "Result", sep = "")
            return(data)
        }
    }
    data <- data[, colnames(data) != "Obs"]
    colnames(data)[colnames(data) == "Result"] <- paste(objname, 
        "Result", sep = "")
    colnames(data)[colnames(data) == "Unit"] <- paste(objname, 
        "Unit", sep = "")
    colnames(data)[colnames(data) == "Yksikk�"] <- paste(objname, 
        "Yksikk�", sep = "")
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

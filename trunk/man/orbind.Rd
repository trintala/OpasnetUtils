\name{orbind}
\alias{orbind}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rowbinding ovariables
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
orbind(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
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
function (x, y) 
{
    if (class(x) == "ovariable") 
        x <- x@output
    if (class(y) == "ovariable") 
        y <- y@output
    addmissingcol <- function(e1, e2) {
        cols <- setdiff(colnames(e2), colnames(e1))
        if ("Iter" \%in\% cols) {
            e1 <- merge(unique(e2["Iter"]), e1)
            cols <- cols[cols != "Iter"]
        }
        col <- as.data.frame(array(NA, dim = c(1, length(cols))))
        colnames(col) <- cols
        if ("Unit" \%in\% cols) {
            col[, "Unit"] <- "?"
        }
        return(cbind(e1, col))
    }
    temp1 <- addmissingcol(x, y)
    temp2 <- addmissingcol(y, x)
    return(rbind(temp1, temp2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

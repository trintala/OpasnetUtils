\name{result}
\alias{result}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Access result vector of an ovariable
}
\description{
A shortcut to the Result column of the data.frame in the output slot of an ovariable.
}
\usage{
result(e1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e1}{
%%     ~~Describe \code{e1} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~

See also:
\url{http://en.opasnet.org/}
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
function (e1) 
{
    test1 <- "Result" \%in\% colnames(e1@output)
    test3 <- paste(e1@name, "Result", sep = "") \%in\% colnames(e1@output)
    rescol1 <- ifelse(test1, "Result", paste(e1@name, "Result", 
        sep = ""))
    if (!(test1 | test3)) 
        stop("No result column found while operating mathematically with ovariables!\n")
    out <- e1@output[[rescol1]]
    comment(out) <- rescol1
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

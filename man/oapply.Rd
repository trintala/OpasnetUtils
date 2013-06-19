\name{oapply}
\alias{oapply}
\title{
Apply for ovariables
}
\description{
Apply for ovariables, uses \code{tapply} on the output slot.
}
\usage{
oapply(X, INDEX = NULL, FUN = NULL, cols = NULL, ..., simplify = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{INDEX}{
%%     ~~Describe \code{INDEX} here~~
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{simplify}{
%%     ~~Describe \code{simplify} here~~
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
function (X, INDEX = NULL, FUN = NULL, cols = NULL, ..., simplify = TRUE) 
{
    out <- X@output
    marginals <- colnames(out)[X@marginal]
    if (is.null(INDEX) & is.null(cols)) 
        stop("No INDEX nor cols defined!\n")
    if (!is.null(cols)) 
        INDEX <- out[[marginals[!marginals \%in\% cols]]]
    out <- tapply(X = out[[paste(X@name, "Result", sep = "")]], 
        INDEX = INDEX, FUN = FUN, ..., simplify = simplify)
    out <- as.data.frame(as.table(out))
    colnames(out)[colnames(out) == "Freq"] <- paste(X@name, "Result", 
        sep = "")
    X@output <- out
    X@marginal <- colnames(out) \%in\% marginals
    return(X)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

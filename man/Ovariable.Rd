\name{Ovariable}
\alias{Ovariable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
\code{\link{ovariable}} constructor
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Ovariable(name = character(), data = data.frame(), formula = function(...) {
    0
}, dependencies = data.frame(), ddata = character(), subset = NULL, getddata = TRUE, save = FALSE, public = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{dependencies}{
%%     ~~Describe \code{dependencies} here~~
}
  \item{ddata}{
%%     ~~Describe \code{ddata} here~~
}
  \item{subset}{
%%     ~~Describe \code{subset} here~~
}
  \item{getddata}{
%%     ~~Describe \code{getddata} here~~
}
  \item{save}{
%%     ~~Describe \code{save} here~~
}
  \item{public}{
%%     ~~Describe \code{public} here~~
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
function (name = character(), data = data.frame(), formula = function(...) {
    0
  }, dependencies = data.frame(), ddata = character(), subset = NULL, 
    getddata = TRUE, save = FALSE, public = TRUE, ...) 
{
    if (!is.null(subset)) 
        ddata <- paste(ddata, opbase.sanitize_subset_name(subset), 
            sep = ".")
    out <- new("ovariable", name = name, data = data, formula = formula, 
        dependencies = dependencies, ddata = ddata)
    if (getddata) 
        out <- ddata_apply(out)
    if (save) {
        assign(name, out)
        if (public) 
            objects.store(list = name, ...)
        else objects.put(list = name, ...)
    }
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{objects}
\alias{objects}
\alias{objects.decode}
\alias{objects.encode}
\alias{objects.get}
\alias{objects.latest}
\alias{objects.put}
\alias{objects.store}
\title{
Server side shared R objects
}
\description{
Fetches R objects from Opasnet. Including most \code{\link{ovariable}}s.
}
\usage{
objects.encode(obj, key)
objects.decode(eobj, key)
objects.get(token)
objects.latest(page_ident, code_name, verbose = FALSE)
objects.put(..., list = character())
objects.store(..., list = character(), verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{
%%     ~~Describe \code{obj} here~~
}
  \item{eobj}{
%%     ~~Describe \code{eobj} here~~
}
  \item{key}{
%%     ~~Describe \code{key} here~~
}
  \item{token}{
%%     ~~Describe \code{token} here~~
}
  \item{page_ident}{
%%     ~~Describe \code{page_ident} here~~
}
  \item{code_name}{
%%     ~~Describe \code{code_name} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{list}{
%%     ~~Describe \code{list} here~~
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
E. Happonen 
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
function (eobj, key) 
{
    key <- charToRaw(key)
    if (length(key)\%\%16 != 0) 
        stop("Invalid key length! Must be 16, 32 or 64 ASCII chars!")
    xtra <- as.integer(eobj[[1]])
    aes <- AES(key, mode = "ECB")
    sobj <- aes$decrypt(eobj[-1], raw = TRUE)
    if (xtra > 0) {
        l <- length(sobj)
        tmp <- c((l - xtra + 1):l)
        sobj <- sobj[-(tmp)]
    }
    obj <- unserialize(sobj)
    return(obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

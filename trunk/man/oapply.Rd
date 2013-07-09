\name{oapply}
\alias{oapply}
\title{
Apply for ovariables
}
\description{
Use \code{tapply} on the output slot
}
\usage{
oapply(X, INDEX = NULL, FUN = NULL, cols = NULL, ..., simplify = TRUE)
}

\arguments{
  \item{X}{
  an \code{\link{ovariable}}
}
  \item{INDEX}{
  list of factors, like \code{\link{tapply}}
}
  \item{FUN}{
  function to apply
}
  \item{cols}{
  names of columns to be removed (reverse INDEX)
}
  \item{\dots}{
  optional arguments to FUN
}
  \item{simplify}{
  like \code{tapply}
}
}
\details{
See also:
\url{http://en.opasnet.org/}
}
\value{
Returns an \code{ovariable}, with output slot tapplied and marginal adjusted accordingly. 
}
\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}

\examples{
a <- new("ovariable", name = "a", output = data.frame(A = c("a", "a", "b", "b"), 
    B = c("1", "2", "1", "2"), aResult = 1:4), marginal = c(TRUE, TRUE, FALSE))
oapply(a, FUN = sum, cols = "A")
oapply(a, a@output[c("A")], sum)
}

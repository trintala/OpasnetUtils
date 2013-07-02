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

\arguments{
  \item{X}{
  an \code{\link{ovariable}}
}
  \item{INDEX}{
  a vector of column names from the output slot \code{data.frame} to tapply over
}
  \item{FUN}{
  function to apply
}
  \item{cols}{
  columns to be removed (reverse INDEX)
}
  \item{\dots}{
  optional arguments to FUN
}
  \item{simplify}{
  like \code{\link{tapply}}
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
#
}

\name{oapply}
\alias{oapply}
\alias{ooapply}
\title{
Apply for ovariables
}
\description{
Use \code{tapply} on the output slot
}
\usage{
oapply(X, INDEX = NULL, FUN = NULL, cols = NULL, drop_na = TRUE, 
    use_aggregate = TRUE, ..., simplify = TRUE)
ooapply(X, cols, FUN = "sum", ...)
}

\arguments{
  \item{X}{
  an \code{\link{ovariable}}
}
  \item{INDEX}{
  list of factors, like \code{\link{tapply}}
}
  \item{FUN}{
  function to apply, \code{ooapply} takes character input (Only "sum", "mean", "min", "max" and "prod" allowed.)
}
  \item{cols}{
  names of columns to be removed (reverse INDEX)
}
  \item{drop_na}{
    if \code{TRUE} removes NA from the result automatically
}
  \item{use_aggregate}{
    if \code{TRUE} uses \code{\link{aggregate}} to perform apply, which is considerably faster with sparse variables
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
Returns an \code{ovariable}, with output slot tapplied and marginal adjusted accordingly. \code{ooapply} is a 
memory-saving variant of oapply when there is exactly one row for each unique combination. \code{oapply} with 
\code{use_aggregate} is fastest in most cases. Any extraneous columns are lost, as with 
\code{\link{tapply}} and \code{\link{aggregate}}.
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

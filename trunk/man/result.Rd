\name{result}
\alias{result}
\alias{result<-}
\title{
Access result vector of an \code{ovariable}
}
\description{
A shortcut to the Result column of the \code{data.frame} in the output slot of an \code{ovariable}.
}
\usage{
result(e1)
}
\arguments{
  \item{e1}{
  an \code{\link{ovariable}}
}
}
\details{
See also:
\url{http://en.opasnet.org/}
}
\value{
Returns a \code{numeric} vector
}

\author{
J. Tuomisto \email{jouni.tuomisto@thl.fi}
}

\examples{
a <- Ovariable("a", output = data.frame(Result = 1))
result(a)
result(a) <- 10 * result(a)
a@output
}
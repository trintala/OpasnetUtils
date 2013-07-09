\name{fillna}
\alias{fillna}
\title{
Interpreting empty locations in indices
}
\description{
Copies result rows that have \code{NA}s as index values and replaces the index value with all available values of that index.
}
\usage{
fillna(object, marginals)
}
\arguments{
  \item{object}{
  a \code{data.frame} to be filled
}
  \item{marginals}{
  \code{integer}, positions of columns whose locations contain NAs that should be duplicated
}
}
\details{
Runs \code{\link{dropall}} before duplication to avoid unnecessary levels. 

See also:
\url{http://en.opasnet.org/}
}
\value{
Returns a \code{data.frame}
}

\author{
J. Tuomisto \email{jouni.tuomisto@thl.fi}
}

\examples{
a <- data.frame(A = c("a", "b", NA), B = c(1, 2, 3))
fillna(a, 1)
}

\name{orbind}
\alias{orbind}
\alias{combine}
\title{
Rowbinding ovariables
}
\description{
Combine two \code{ovariable}s or \code{data.frame}s using rbind even if columns differ
}
\usage{
orbind(x, y)
combine(..., name = character())
}
\arguments{
  \item{x}{
  first object
}
  \item{y}{
  second object
}
  \item{\dots}{
   any number of \code{\link{ovariable}}s separated by commas
}
  \item{name}{
  name of resulting \code{ovariable}, affects Source and Result column names
}
}
\details{
Missing columns from each \code{\link{ovariable}} are added to the other and filled with \code{NA}. 
Combine is similar to \code{orbind}, but takes multiple arguments and tracks Sources under a single index.

See also:
\url{http://en.opasnet.org/}
}
\value{
Returns a \code{data.frame}
}

\author{
J. Tuomisto \email{jouni.tuomisto@thl.fi}
}

%\examples{
%%s#
%}

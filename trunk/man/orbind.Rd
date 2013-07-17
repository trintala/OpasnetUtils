\name{orbind}
\alias{orbind}
\title{
Rowbinding ovariables
}
\description{
Combine two \code{ovariable}s or \code{data.frame}s using rbind even if columns differ
}
\usage{
orbind(x, y)
}
\arguments{
  \item{x}{
  first object
}
  \item{y}{
  second object
}
}
\details{
Missing columns from each \code{\link{ovariable}} are added to the other and filled with \code{NA}. 

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

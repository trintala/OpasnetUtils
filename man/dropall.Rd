\name{dropall}
\alias{dropall}
\title{
Dropall
}
\description{
Drops unused factor levels in data.frames
}
\usage{
dropall(x)
}
\arguments{
  \item{x}{
  a \code{data.frame}
}
}
\details{
This function makes sure that the \code{factor} \code{levels} in a \code{data.frame} do not contain entries that have already been removed from the \code{factor} 
itself. 

See also:
\url{http://en.opasnet.org/}
}
\value{
\code{data.frame}
}

\author{
J. Tuomisto \email{jouni.tuomisto@thl.fi}
}

\examples{
a <- data.frame(A = c("a", "b"), B = c(1,1))
levels(a[[1]])

a <- a[-2 ,]
levels(a[[1]])
a[[1]]

a <- dropall(a)
levels(a[[1]])
a[[1]]
}

\name{DecisionTableParser}
\alias{DecisionTableParser}
\title{
Parse data.frame for decisions
}
\description{
Parses a data.frame of suitable format into \code{\link{odecision}}s.
}
\usage{
DecisionTableParser(DTable)
}
\arguments{
  \item{DTable}{
data.frame
}
}
\details{
Decisions consist of conditions and effects, target a certain variable and may have multiple options. 
Saves \code{odecision} class objects into \code{.GlobalEnv}.

Input format is described on \url{http://en.opasnet.org/w/Decision}. Currently usable decision effects are Add, Multiply, Replace, 
Remove and Identity.

See also:
\url{http://en.opasnet.org/}
}
\value{
No return value. 
}
\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}
\seealso{
\code{\link{CheckDecisions}}
}
\examples{
#
}

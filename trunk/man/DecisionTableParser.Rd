\name{DecisionTableParser}
\alias{DecisionTableParser}
\title{
Parse data.frame for decisions
}
\description{
Parses a \code{data.frame}  into \code{\link{odecision}}s.
}
\usage{
DecisionTableParser(DTable, env = .GlobalEnv)
}
\arguments{
  \item{DTable}{
  \code{data.frame}
}
  \item{env}{
  target \code{environment}, default is \code{.GlobalEnv}
}
}
\details{
Decisions consist of conditions and effects, target a certain variable and may have multiple options. 

Input format is described on \url{http://en.opasnet.org/w/Decision}. Currently usable decision effects are Add, Multiply, Replace, 
Remove and Identity.

See also:
\url{http://en.opasnet.org/}
}
\value{
No return value. Saves \code{odecision} class objects into specified \code{environment}.
}
\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}
\seealso{
\code{\link{CheckDecisions}}
}
\examples{
modeldecisions <- data.frame(Stakeholder = "Group A", Decision = "More wind power", 
  Option = "A few more turbines", Variable = "PowerGeneration", Cell = "Type:Wind", 
  Change = "Add", Result = "5")
DecisionTableParser(modeldecisions)
ls()
}

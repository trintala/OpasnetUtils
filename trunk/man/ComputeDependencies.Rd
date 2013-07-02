\name{ComputeDependencies}
\alias{ComputeDependencies}
\title{
Evaluate ovariable dependencies
}
\description{
Fetches, evaluates and \code{\link{Check}}s ovariable dependencies given in an appropriate data.frame. 
}
\usage{
ComputeDependencies(dependencies, forceEval = FALSE, indent = 0, new_code = FALSE, ...)
}
\arguments{
  \item{dependencies}{
  data.frame that defines \code{\link{Fetch}} targets, usually taken from an ovariables dependencies slot
}
  \item{forceEval}{
  forcibly re-evaluate existing instances of listed dependencies
}
  \item{indent}{
  verbose print assist for the Check family, used internally
}
  \item{new_code}{
  a flag for compatibility with older code, default FALSE nullifies ComputeDependencies usage in \code{\link{ovariable}} formulas
}
  \item{\dots}{
  arguments to pass on to the various recursive checks and evaluations
}
}
\details{
\code{ComputeDependencies} uses \code{Fetch2}, \code{EvalOutput}, \code{CheckDecisions}, \code{CheckCollapse} and \code{CheckInput} to load and pre-process
upstream variables. It is automatically called by \code{\link{EvalOutput}}, but can be seen on the first lines of old \code{ovariable} formula code, 
to avoid applying decisions, inputs and optimizations twice in old code the function does nothing by default. This is no 
problem since users should not be calling this function at all. 
\code{ComputeDependencies} also does most of the exception handling in the recursive ovariable model.

See also:
\url{http://en.opasnet.org/}
}
\value{
No return value
}
\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}
\examples{
#
}
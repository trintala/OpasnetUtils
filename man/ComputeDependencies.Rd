\name{ComputeDependencies}
\alias{ComputeDependencies}
\title{
Evaluate \code{ovariable} dependencies
}
\description{
Fetches, evaluates and \code{\link{Check}}s \code{\link{ovariable}} dependencies given in data.frame format
}
\usage{
ComputeDependencies(dependencies, forceEval = FALSE, indent = 0, 
    verbose = FALSE, ...)
}
\arguments{
  \item{dependencies}{
  \code{data.frame} that defines \code{\link{Fetch}} targets, usually taken from an \code{ovariable}'s dependencies slot
}
  \item{forceEval}{
  if \code{TRUE}, forcibly re-evaluates existing instances of listed dependencies
}
  \item{indent}{
  verbose print assist for the \code{Check} family, used internally
}
  \item{verbose}{
  TRUE to enable status message printing assists (line breaks) and status messages for other function calls
}
  \item{\dots}{
  arguments to pass on to the various recursive checks and evaluations
}
}
\details{
\code{ComputeDependencies} uses \code{Fetch}, \code{EvalOutput}, \code{CheckDecisions}, \code{CheckCollapse} and \code{CheckInput} to load and pre-process
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
%\examples{
%%
%}
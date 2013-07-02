\name{EvalOutput}
\alias{EvalOutput}
\title{
Evalaute ovariable output
}
\description{
Evaluate the output slot of an \code{\link{ovariable}}, this usually means recursively evaluating all dependent variables as well.
}
\usage{
EvalOutput(variable, fillna = FALSE, indent = 0, verbose = FALSE, ...)
}
\arguments{
  \item{variable}{
  an \code{\link{ovariable}}
}
  \item{fillna}{
  TRUE to attempt \code{\link{fillna}} at the end
}
  \item{indent}{
  internal integer argument used in verbose printing
}
  \item{verbose}{
  TRUE to print status messages while processing outputs and various checks
}
  \item{\dots}{
  arguments are passed on to ovariable formulas and to dependent EvalOutput calls (recursivity), number of iterations (N) is commonly set here
}
}
\details{
\code{EvalOutput} automates most of the other features related to \code{ovariable} handling. It runs \code{\link{ComputeDependencies}} first, 
produces a \code{data.frame} by combining the return values from \code{\link{interpret}}ing the data slot and running the formula slot function, 
makes a "Source" -column to distinguish between the two "Results", 
and lastly \code{\link{CheckMarginals}} is run on the variable (optionally also \code{fillna}). 

Since \code{EvalOutput} is usually run on the end node of a model, there should not be inputs or decisions hence they are not checked for. 
In contrast \code{ComputeDependencies} runs all \code{Check}s besides \code{CheckMarginals}.

See also:
\url{http://en.opasnet.org/}
}
\value{
Returns the input ovariable with the output slot (re)defined.
}
\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}

\examples{
a <- Ovariable("a", data.frame(A = c("a", "b"), Result = c("1-2", "1-4")))
a <- EvalOutput(a, N = 10)
a@output
}

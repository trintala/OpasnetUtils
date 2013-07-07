\name{Check}
\alias{Check}
\alias{CheckCollapse}
\alias{CheckDecisions}
\alias{CheckInput}
\alias{CheckMarginals}
\title{
Ovariable Checks
}
\description{
The Check functions are used to introduce common model specific alterations to variables without changing their definition directly. 
}
\usage{
CheckCollapse(variable, indent = 0, verbose = TRUE, ...)
CheckDecisions(variable, indent = 0, verbose = TRUE, ...)
CheckInput(variable, substitute = FALSE, indent = 0, verbose = TRUE, ...)
CheckMarginals(variable, deps = list(), priormarg = TRUE, indent = 0, verbose = TRUE, ...)
}

\arguments{
  \item{variable}{
  An ovariable to run Check on.
}
  \item{deps}{
  The dependency list of a latent ovariable, used by CheckMarginals to keep track of index columns. 
}
  \item{priormarg}{
  Flag determining whether columns are assumed to be indices by default when Checking marginals.
}
  \item{substitute}{
  Flag determining whether model inputs should replace or append to current variable values.
}
  \item{indent}{
  Used by verbose to structure status messages by using indentation.
}
  \item{verbose}{
  Flag status message printing.
}
  \item{\dots}{
  Excess arguments are ignored. 
}
}
\details{
The Check functions are mainly used internally. They check for orders of external (from a variables perspective) changes to it. 
Specifically they check objects in \code{.GlobalEnv} with prefixes ("Col", "Dec", "Inp"). These checks are automated in the normal 
\code{\link{ovariable}} evaluation routine \code{\link{EvalOutput}}. 

\code{CheckCollapse}
\code{\link{CollapseMarginal}} which collapses marginals by applying sums, means or samples. Also loses all non-marginal columns except 
the relevant Result. It is mainly used to streamline models by reducing rows in data. 

\code{CheckDecisions}
checks for and applies decisions on variables. 
The function makes use of the \code{\link{odecision-class}}, which specifies the target cells as well as the effect. Odecisions are 
most often produced by \code{\link{DecisionTableParser}}.

\code{CheckInput}
checks and uses outside input (user inputs in models or decision variables). Input should be in ovariable format. 

\code{CheckMarginals}
returns an ovariable with a marginal derived from the data and upstream variable marginals. 
Assumes that all depended upon variables are in memory, as should be the case.

See also:
\url{http://en.opasnet.org/}
}
\value{
  Original \code{\link{ovariable}} with possible adjustments.
}
\author{
  T. Rintala \email{teemu.rintala.a@gmail.com}
}

\examples{
#
}
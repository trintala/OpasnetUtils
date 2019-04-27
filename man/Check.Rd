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
CheckMarginals(variable, deps = list(), dep_margs = list(), dep_nomargs = list(), 
    priormarg = TRUE, indent = 0, verbose = TRUE, ...)
}

\arguments{
  \item{variable}{
  an ovariable to run Check on.
}
  \item{deps}{
  the dependency list of a latent ovariable, used by CheckMarginals to keep track of index columns. Ignored if length 0.
}
  \item{dep_margs}{list of marginals, main use is in ovarible merge method. Ignored if length 0.}
  \item{dep_nomargs}{list of non-marginals, main use is in ovarible merge method. Ignored if length 0.}
  \item{priormarg}{
  flag determining whether columns are assumed to be indices by default when checking marginals.
}
  \item{substitute}{
  flag determining whether model inputs should replace or append to current variable values.
}
  \item{indent}{
  used by verbose to structure status messages by using indentation.
}
  \item{verbose}{
  flag status message printing.
}
  \item{\dots}{
  excess arguments are ignored. 
}
}
\details{
The Check functions are mainly used internally. They check for external instructions (model specific changes); 
specifically objects in \code{.GlobalEnv} with prefixes ("Col", "Dec", "Inp"). they are automated in the normal 
\code{\link{ovariable}} evaluation routine (\code{\link{EvalOutput}}). 

\code{CheckCollapse} uses \code{\link{CollapseMarginal}} which collapses marginals by applying sums, means or 
samples. Also loses all non-marginal columns except the relevant "Result". It is mainly used to streamline models 
by reducing rows in data. 

\code{CheckDecisions} checks for and applies decisions on variables. The function makes use of the \code{\link{odecision-class}}, 
which specifies the target cells as well as the effect. Odecisions are most often produced by \code{\link{DecisionTableParser}}.

\code{CheckInput} checks and uses outside input (run specific user inputs in models). Input should be in \code{ovariable} format. 

\code{CheckMarginals} fills the marginal slot of an \code{ovariable} using information from variable data and upstream variable marginals. 
Assumes that all depended upon variables are loaded, as should be the case.

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
#library(OpasnetUtils)
#
Deca <- new(
		"odecision",
		dectable = data.frame(
				Decision = "Dec1",
				Option = c("do it", "BAU"),
				Cell = c("",""),
				Change = c("Multiply", "Identity"),
				Result = c("2", "1")
		)
)
openv.setN(10)
a <- Ovariable("a", data = data.frame(A = c("A", "B"), Result = c("1-2","2-4")))

b <- Ovariable("b", formula = function(...){2*a}, dependencies = data.frame(Name = "a"))

b <- EvalOutput(b)

b
oapply(a, a@output[c("A", "Dec1")], mean)

}
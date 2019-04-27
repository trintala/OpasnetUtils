\name{openv}
\alias{openv}
\alias{openv.setN}
\title{
OpasnetUtils environment for mutable function parameters
}
\description{
Container for model-wide parameters such as number of iterations used in random sampling. 
}
\usage{
openv.setN(x)
}

\arguments{
  \item{x}{
  \code{numeric}, new default number of iterations
}
}
\details{
The environment is also automatically populated by \code{CheckDecisions} to communicate evaluation to other branches of the recursion tree. 
The value N in this environment is used by \code{\link{interpret}} when N is not given in an \code{EvalOutput(...)} call.
}
\value{
No return value.
}

\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}

\examples{
openv$N
openv.setN(1)
openv$N
openv$N <- 2
openv$N
}


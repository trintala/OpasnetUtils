\name{odebug}
\alias{odebug}
\title{
Debugging ovariables
}
\description{
Check shared indices, dropped locations and some statistical metrics
}
\usage{
odebug(x, variance = FALSE)
}

\arguments{
  \item{x}{
  \code{ovariable} to be debugged
}
  \item{variance}{
  If \code{TRUE} runs variance analysis between indices of the given ovariable. 
}
}
\details{
Checks output lengths, number of NAs, common and matching marginals, missing locations in common marginals and performs variance analysis if required. 
Variance analysis is only performed up to second order, the intended use is to quantify overlap between indices.
}
\value{
List of outputs.
}

\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}

\examples{
test <- data.frame(
  A = c("x","y","z"), 
  B = rep(c("a","b","c"), each = 3), 
  C = rep(c("1","2","3"), each = 3*3), 
  testResult = runif(1*3*3*3)
)
test <- Ovariable(
  name = "test", 
  output = test, 
  marginal = c(TRUE, TRUE, TRUE, FALSE)
)
odebug(test, variance = TRUE)
}


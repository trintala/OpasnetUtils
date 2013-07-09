\name{interpret}
\alias{interpret}
\alias{interpret,character-method}
\title{
Parse human readable distribution definitions
}
\description{
Interpret textual data into probability distributions using regular expressions.
}
\usage{
interpret(idata, N = 1000, rescol = "Result", dbug = FALSE, ...)
}

\arguments{
  \item{idata}{
  input, \code{character} or \code{data.frame}
}
  \item{N}{
  number of iterations
}
  \item{rescol}{
  name of result column, defaults to "Result"
}
  \item{dbug}{
  use \code{TRUE} to turn on debug prints
}
  \item{\dots}{
  excess arguments are ignored
}
}
\details{
Interpretation rules are as follows:
Empty space is stripped away.
"X-Y" defines a uniform distribution between X and Y, if Y/X is greater than 100 then logarithmic uniform distribution is assumed. \cr
Negative X and Y are determined by the number of "-": if 2, X is negative; if 3, both are. \cr
"<X" defines a uniform distribution between 0 and X. \cr
"X+-Y" defines a normal distribution with mean X and sd Y. \cr
"X(Y-Z)" defines a normal distribution where Y-Z is assumed the 95-percent confidence interval, from which sd is determined. \cr
If distance from mean to the higher boundary is 50-percent higher than to lower boundary log normality is assumed. \cr
"X:Y:Z" defines a triangular distribution with min, mode and max (can be given in any order). \cr
"X1;X2;...;Xn" defines a random unbiased sample (with replacement) between the given elements. \cr
See \url{http://en.opasnet.org/w/Interpret} for a table. 

See also:
\url{http://en.opasnet.org/}
}
\value{
Returns a \code{data.frame} with an "Iter" column added. Uninterpretable values are converted to \code{NA}s.
}

\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}

\examples{
interpret(c("1-4", "1-1000"), N = 5)
}

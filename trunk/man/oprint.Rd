\name{oprint}
\alias{oprint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print ovariables or data frames in html format.
}
\description{
This function uses xtable library to output ovariables or data frames as html formatted tables.
}
\usage{
oprint(x, show_all = FALSE, sortable = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  An ovariable or data frame.
}
  \item{show_all}{
  Flag determining whether all data rows should be printed. If FALSE, only thousand first rows get printed (default). 
}
  \item{sortable}{
  Flag determining whether output table should be sortable.
}
  \item{\dots}{
  Excess arguments will be passed straight to xtable-function.
}
}
\details{
If argument x is \code{\link{ovariable}} type, its output-slot gets printed.
If output-slot is empty, \code{\link{EvalOutput}}-function will be automatically executed to generate output.
This function is aimed for being used within Opasnet only!
}
\value{
  Input data as html formatted table string
}
\references{
  \url{http://en.opasnet.org/}
}
\author{
  E. Happonen \email{einari.happonen@thl.fi}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link[xtable]{xtable}}
}
\examples{
library(OpasnetUtils)
x <- data.frame(c(1,2),c(2,4))
oprint(x)
}

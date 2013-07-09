\name{oprint}
\alias{oprint}
\alias{oprint,ovariable-method}
\title{
Print ovariables or data frames in html format.
}
\description{
This function uses \code{\link[xtable]{xtable}} to output \code{\link{ovariable}}s or \code{data.frame}s as html formatted tables.
}
\usage{
oprint(x, show_all = FALSE, sortable = TRUE, ...)
}

\arguments{
  \item{x}{
  \code{ovariable} or \code{data frame}.
}
  \item{show_all}{
  if \code{TRUE} all data rows are printed, else only first thousand rows get printed (default). 
}
  \item{sortable}{
  if \code{TRUE} output table is made sortable.
}
  \item{\dots}{
  arguments can be passed to \code{xtable}
}
}
\details{
If argument x is an \code{\link{ovariable}}, its output-slot gets printed.
If output-slot is empty, \code{\link{EvalOutput}} will be automatically executed to generate output.
This function is aimed for being used within Opasnet only! R console will print out html markup. 

See also:
\url{http://en.opasnet.org/}
}
\value{
  Input data as html formatted table string.
}
\author{
  E. Happonen \email{einari.happonen@thl.fi}
}

\seealso{
  \code{\link[xtable]{xtable}}
}
\examples{
library(OpasnetUtils)
x <- data.frame(c(1,2),c(2,4))
oprint(x)
}

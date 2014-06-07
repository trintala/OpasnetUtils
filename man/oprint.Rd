\name{oprint}
\alias{oprint}
\alias{oprint,ovariable-method}
\alias{oprint,data.frame-method}
\alias{oprint,matrix-method}
\alias{oprint.table}
\title{
Print ovariables or data frames in html format.
}
\description{
Oprint uses \code{\link[xtable]{xtable}} to output \code{\link{ovariable}}s or \code{data.frame}s or \code{matrix} as html formatted tables. 
In other cases uses regular print but adds formatting. 
}
\usage{
oprint(x, pre = TRUE, ...)
oprint.table(x, show_all = FALSE, sortable = TRUE, ...)
}

\arguments{
  \item{x}{
  Any object with a print or oprint method.
}
  \item{pre}{
  \code{TRUE} for \code{<pre>} formatting.
}
  \item{show_all}{
  if \code{TRUE} all data rows are printed, else only first thousand rows get printed (default). 
}
  \item{sortable}{
  if \code{TRUE} output table is made sortable.
}
  \item{\dots}{
  extra arguments are passed to \code{oprint.table} and/or \code{xtable}
}
}
\details{
If argument x is an \code{\link{ovariable}}, its output-slot gets printed.
If output-slot is empty, \code{\link{EvalOutput}} will be automatically executed to generate output.
This function is aimed for being used within Opasnet only! R console will print out html markup. 
oprint.table is not exported. Use oprint instead and \code{...} to pass arguments \code{show_all} and \code{sortable}.

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

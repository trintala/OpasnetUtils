\name{CollapseMarginal}
\alias{CollapseMarginal}
\title{
Collapse marginals
}
\description{
Apply functions (only \code{sample} at the moment) over ovaribale indices
}
\usage{
CollapseMarginal(variable, cols, fun = "mean", probs = NULL, ...)
}
\arguments{
  \item{variable}{
  an ovariable
}
  \item{cols}{
  vector of column names or indices to collapse
}
  \item{fun}{
  \code{character} name of function to be used to summarize 
}
  \item{probs}{
  list of vectors defining the distribution of values in a column index
}
  \item{\dots}{
  excess arguments are ignored
}
}
\details{
Samples over a fully defined column index (marginal) treating it as a nuisance parameter. 
Increases the joint distribution uncertainty (loses information). Weighted sampling is also possible.

Used to streamline heavy models (output has fewer rows of data). 

See also:
\url{http://en.opasnet.org/}
}
\value{
  Input \code{ovariable} with possibly lighter output.
}
\author{
  T. Rintala \email{teemu.rintala.a@gmail.com}
}
\examples{
test <- Ovariable("test", output = data.frame(City = c("Helsinki", "Espoo"), 
    Iter = 1, testResult = 1:2), marginal = c(TRUE, TRUE, FALSE))
CollapseMarginal(test, "City", "mean")
}

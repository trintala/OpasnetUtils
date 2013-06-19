\name{CollapseMarginal}
\alias{CollapseMarginal}
\title{
Collapse marginals
}
\description{
Apply functions (only sample at the moment) over ovaribale indices
}
\usage{
CollapseMarginal(variable, cols, probs = NULL, ...)
}
\arguments{
  \item{variable}{
  an ovariable
}
  \item{cols}{
  vector of column names or indices to collapse
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
Increases the joint distribution uncertainty (loses information). Used to streamline heavy 
models (output has fewer rows of data). 
}
\value{
  ovariable
}
\author{
  T. Rintala \email{teemu.rintala.a@gmail.com}
}
\seealso{
  \url{http://en.opasnet.org/}
}
\examples{
#
}

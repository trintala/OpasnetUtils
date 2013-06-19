\name{CollapseTableParser}
\alias{CollapseTableParser}
\title{
Parsing Collapse orders from a table
}
\description{
Parses data.frames of specific format to produce "Col" prefixed lists as per \code{\link{CollapseMarginal}}
}
\usage{
CollapseTableParser(CTable)
}
\arguments{
  \item{CTable}{
  a data.frame with columns "Index" (column names) and "Probs" (propabilities of column levels in joint distribution, comma separated)
}
}
\details{
Used in the Opasnet modeling environment an endpoint node to produce multiple model relevant Collapse orders. 
}
\value{
No return value
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

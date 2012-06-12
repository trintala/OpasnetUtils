\name{IntArray}
\alias{IntArray}
\title{Intelligent Arrays}
\description{
  Matches the cells of two arrays with any number of matching dimensions to a single data.frame.
  If matching dimensions exist, non-matching locations will be dropped. All non-matching dimensions
  are included in the result, results from the input that is missing the non-matching dimension 
  are duplicated over that dimension.
}
\usage{IntArray(input1, input2, rescol2name = "input2result")}
\arguments{
  \item{input1}{a data.frame, array input will be converted to a data.frame using \code{as.data.frame(as.table())}}
  \item{input2}{array}
  \item{rescol2name}{name of the column with the matched values from input2}
}
\value{A data.frame with columns like "dim1", "dim2",...,"dimn", "input1result", "input2result")}
\author{Teemu Rintala, \email{teemu.rintala@thl.fi}}
\seealso{\url{http://en.opasnet.org/w/Operating_intelligently_with_multidimensional_arrays_in_R}}
\examples{
  IntArray(data.frame(Country=rep(c("Finland", "Sweden"),each=4),
  Age = rep(c("<4", "5-14", "15-59", ">60"),2), Result=runif(2*4, 2, 11)),
  array(runif(8, 132, 234), dim=c(2, 4), dimnames=list(Country = c("Finland", "Sweden"),
  Year = c("2000", "2010", "2020", "2040"))), "test")
}
\keyword{methods}
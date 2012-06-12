\name{DataframeToArray}
\alias{DataframeToArray}
\title{Transform data.frames of certain structure to arrays}
\description{
  Transfrom data.frames, which are of similar form as what is returned by the \code{op_baseGetData} or 
  \code{as.data.frame(as.table())} (columns represent dimensions), to arrays.
}
\usage{
  DataframeToArray(dataframe, rescol = NULL)
}
\arguments{
  \item{dataframe}{input data.frame}
  \item{rescol}{a valid column subscript, either numeric or character vector of length 1, 
  if not given assumed to be either "Freq", "Result" or "result" in that order}
}
\value{an array with dimensions corresponding to the columns of the data.frame}
\author{Teemu Rintala, \email{teemu.rintala@thl.fi}}
\seealso{\url{http://en.opasnet.org/w/Opasnet_Base_Connection_for_R}}
\examples{
  DataframeToArray(data.frame(Country=rep(c("Finland", "Sweden"),each=4),
  Age=rep(c("<4","5-14", "15-59", ">60"),2), Result=runif(2*4, 2, 11)))
}
\keyword{methods}
\name{ddata_apply}
\alias{ddata_apply}
\title{
Dynamic data link activation
}
\description{
Fetches the latest data associated with the ovariable from the OpasnetBase
}
\usage{
ddata_apply(ovariable, ddata_tidy = TRUE, force_ddata = FALSE, ...)
}
\arguments{
  \item{ovariable}{
  an ovariable with the ddata slot defined as page_id i.e. "Op_en1000"
}
  \item{ddata_tidy}{
  flag whether \code{\link{tidy}} should be run on downloaded data
}
  \item{force_ddata}{
  use dynamic data links even if the data slot of an ovariable is already defined
}
  \item{\dots}{
  excess parameters are ignored
}
}
\details{
  This function is mostly used internally
}
\value{
Returns the input ovariable, with the data slot defined (if ddata is available or it is already defined).
}
\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}
\seealso{
\code{\link{ovariable}}
}
\examples{
#
}

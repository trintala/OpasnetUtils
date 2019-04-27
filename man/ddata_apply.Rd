\name{ddata_apply}
\alias{ddata_apply}
\title{
Dynamic data link activation
}
\description{
Fetches the latest data associated with an \code{\link{ovariable}} from the OpasnetBase if available
}
\usage{
ddata_apply(ovariable, ddata_tidy = TRUE, force_ddata = FALSE, ...)
}
\arguments{
  \item{ovariable}{
  an \code{ovariable} with the ddata slot defined as page_id i.e. "Op_en1000"
}
  \item{ddata_tidy}{
  \code{TRUE} to run \code{\link{tidy}} on downloaded data
}
  \item{force_ddata}{
  if \code{TRUE}, dynamic data links are used even if the data slot of an \code{ovariable} is already defined
}
  \item{\dots}{
  excess parameters are ignored
}
}
\details{
This function is mostly used internally
  
See also:
\url{http://en.opasnet.org/}
}
\value{
Returns the input \code{ovariable}. (Re)defines the data slot if it is not already defined ands ddata is available.
}
\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}
\seealso{
\code{\link{ovariable}}
}
%\examples{
%%
%}

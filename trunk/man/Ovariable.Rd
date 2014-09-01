\name{Ovariable}
\alias{Ovariable}
\title{
Ovariable constructor
}
\description{
Create \code{\link{ovariable}}s more conveniently
}
\usage{
Ovariable(name = character(), data = data.frame(), 
    formula = function(...) {0}, dependencies = data.frame(), 
    ddata = character(), output = data.frame(), marginal = logical(), 
    subset = character(), getddata = TRUE, save = FALSE, public = TRUE, ...)
}
\arguments{
  \item{name}{
  \code{character} string for the name slot, should match object name
}
  \item{data}{
  \code{data.frame} for the data slot
}
  \item{formula}{
  \code{function} for the formula slot
}
  \item{dependencies}{
  \code{data.frame} for the dependencies slot
}
  \item{ddata}{
  \code{character} string specifying an Opasnet page identifier (Op_enXXXX) for the ddata slot
}
  \item{output}{
  \code{data.frame} for the output slot
}
  \item{marginal}{
  \code{logical} for the marginal slot
}
  \item{subset}{
  \code{character} string specifying an Opasnet Base subset (See \code{\link{opbase.data}} for details)
}
  \item{getddata}{
  if TRUE dynamic data link will be activated immediately, which means that by default data will not be refreshed at model runtime
}
  \item{save}{
  if TRUE resulting \code{ovariable} will be saved on the server 
}
  \item{public}{
  if TRUE \code{\link{objects.store}} is used instead of \code{objects.put} (the former stores the run key in a public database)
}
  \item{\dots}{
  more arguments can be passed onto \code{objects.store} and \code{objects.put} in case \code{save == TRUE}.
}
}
\details{
Just a regular constructor with integrated dynamic data link activation and storing options.

See also:
\url{http://en.opasnet.org/}
}
\value{
Returns an \code{ovariable}.
}
\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}
\seealso{
\code{\link{ovariable-class}}
}
\examples{
\dontrun{Ovariable("A", ddata = "Op_en5674", getddata = TRUE)}
\dontrun{k <- Ovariable("k", output = data.frame(B = "a", Result = 1))}
}

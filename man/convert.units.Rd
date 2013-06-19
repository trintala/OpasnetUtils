\name{convert.units}
\alias{convert.units}
\title{
Converting units
}
\description{
Converts units (to SI equivalents by default)
}
\usage{
convert.units(x, tounit = c("kg", "s", "m", "m3", "J", "W", "A", "V", "C", "N", "Pa", "Hz", "mol"), fromunit = NULL)
}
\arguments{
  \item{x}{
  a numeric vector with values to be converted
}
  \item{tounit}{
  a character vector of the new units to be used
}
  \item{fromunit}{
  a character vector or factor with the current units
}
}
\details{
Units use the table in \url{en.opasnet.org/w/Unit_conversions} for their conversions, so the units used have to be specified there.
}
\value{
data.frame
}
\author{
J. Tuomisto
}

\seealso{
  \url{http://en.opasnet.org/}
}
\examples{
#
}

\name{convert.units}
\alias{convert.units}
\alias{convert.units,ovariable-method}
\alias{convert.units,data.frame-method}
\title{
Converting units
}
\description{
Converts units (to SI equivalents by default)
}
\usage{
convert.units(x, tounit = c("kg", "s", "m", "m3", "J", "W", "A", 
    "V", "C", "N", "Pa", "Hz", "mol"), fromunit = NULL)
}
\arguments{
  \item{x}{
  \code{numeric} vector with values to be converted
}
  \item{tounit}{
  \code{character} vector of the new units to be used
}
  \item{fromunit}{
  \code{character} vector or factor with the current units
}
}
\details{
Uses the table in \url{en.opasnet.org/w/Unit_conversions} for the conversions, which can be edited to contain relevant units.

See also:
\url{http://en.opasnet.org/}
}
\value{
Returns a \code{data.frame}
}
\author{
J. Tuomisto \email{jouni.tuomisto@thl.fi}
}

\examples{
\dontrun{
convert.units(1, tounit = c("pg", "l"), fromunit = "ug /m3")
}
}

\name{op_base}
\alias{op_base}
\alias{op_baseGetData}
\alias{op_baseGetLocs}
\title{Functions for Interaction with the Opasnet Base (obsolete)}
\description{
  A collection of functions used in Opasnet for database interaction. Includes functions for fetching
  datasets, exploring the dimensions of Opasnet variables and writing objects into the database.
  
  This function family has been replaced by the \code{\link{opbase}} family
}
\usage{
op_baseGetData(dsn, ident, ...)
op_baseGetLocs(dsn, ident, ...)
}
\arguments{
  \item{dsn}{a defined Data Service Name (in ODBC) to use}
  \item{ident}{object identifier in Opasnet (or other)}
  \item{...}{arguments for \code{opbase}}
}
\details{
  Obsolete.
  
  See also:
  \url{http://en.opasnet.org/w/Opasnet_Base_Connection_for_R}
}
\value{
  \item{op_baseGetData}{Returns data as a data.frame.}
  \item{op_baseGetLocs}{Returns dimension information as a data.frame.}
  \item{op_baseWrite}{Returns 0 if successful.}
}
\author{Teemu Rintala, \email{teemu.rintala@thl.fi}}
\examples{
  \dontrun{op_baseGetLocs("opasnet_base", "Op_en4723")}
  \dontrun{asthma <- op_baseGetData("opasnet_base", "Op_en4723", exclude = 48823)}
}
\keyword{database}
\name{op_base}
\alias{op_base}
\alias{op_baseGetData}
\alias{op_baseGetLocs}
\alias{op_baseWrite}
\title{Functions for Interaction with the Opasnet Base}
\description{
  A collection of functions used in Opasnet for database interaction. Includes functions for fetching
  datasets, exploring the dimensions of Opasnet variables and writing objects into the database.
}
\usage{
op_baseGetData(dsn, ident, include = NULL, exclude = NULL, series_id = NULL, iterations = NULL)
op_baseGetLocs(dsn, ident, series_id = NULL)
op_baseWrite(dsn, input, ident = NULL, name = NULL, unit = NULL, objtype_id = NULL, who = NULL, acttype = NULL, sort = TRUE)
}
\arguments{
  \item{dsn}{a defined Data Service Name (in ODBC) to use}
  \item{ident}{object identifier in Opasnet (or other)}
  \item{include}{
    a vector of numerical id:s for locations in indexes which are desired;
    if defined, cells in locations not mentioned will be removed from the query;
    if locations in different indexes are defined the result will be the union of
    data picked separately for each index from the whole data.
  }
  \item{exclude}{
    a vector of numerical id:s for locations in indexes which are not desired;
    if defined, all cells which are in the defined locations are removed from the query
  }
  \item{series_id}{
    numerical id; if defined, defines the series from which data is fetched;
    otherwise the most recent one is fetched
  }
  \item{iterations}{number of iterations to be fetched from the database}
  \item{input}{
    data.frame, if array is converted using \code{as.data.frame(as.table())},
    correct structure described elsewhere
  }
  \item{name}{
    if the object to be written doesn't have an earlier entry, name will be required;
    if not given, is prompted for
  }
  \item{unit}{unit of the object; if not given, is prompted for}
  \item{objtype_id}{
    if the object to be written doesn't have an earlier entry, objtype_id will be required;
    if not given, is prompted for; 1 for variable; most recent ids will be printed for prompt
  }
  \item{who}{
    the name of the uploader; if not given, is prompted for
  }
  \item{acttype}{
    4 for creating a new data series, 5 for appending to the most recent one;
    if not given, is prompted for
  }
  \item{sort}{
    is the data sorted properly (indices first, obs last), if yes sort can be set to FALSE
    for a minor performance boost.
  }
}
\details{
  Uses \link{RODBC} for actual database communication.
}
\value{
  \item{op_baseGetData}{returns data as a data.frame}
  \item{op_baseGetLocs}{returns dimension info as a data.frame}
  \item{op_baseWrite}{returns 0 if successful}
}
\author{Teemu Rintala, \email{teemu.rintala@thl.fi}}
\seealso{\url{http://en.opasnet.org/w/Opasnet_Base_Connection_for_R}}
\examples{
  \dontrun{op_baseGetLocs("opasnet_base", "Op_en4723")}
  \dontrun{asthma <- op_baseGetData("opasnet_base", "Op_en4723", exclude = 48823)}
  \dontrun{op_baseWrite("opasnet_base", asthma)}
}
\keyword{database}
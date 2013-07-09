\name{tidy}
\alias{tidy}

\title{
Format database output for use in ovariables
}
\description{
Wrapper for various standard operations applied on \code{ovariable} data from the OpasnetBase.
}
\usage{
tidy(data, objname = "", idvar = "Obs", direction = "wide", 
    widecol = NULL, base1 = FALSE)
}

\arguments{
  \item{data}{
  \code{data.frame} to be formatted
}
  \item{objname}{
  \code{character} prefix to be added to variable specific rows like "Result" and "Unit"
}
  \item{idvar}{
  \code{\link{reshape}} idvar
}
  \item{direction}{
  \code{\link{reshape}} direction
}
  \item{widecol}{
  \code{\link{reshape}} timevar, specifiec column to be expanded
}
  \item{base1}{
  compatibility with obsolete database
}
}
\details{
Uses reshape, renames "Result" and "Unit" columns and gets rid of unwanted columns from old database merged data. 

See also:
\url{http://en.opasnet.org/}
}
\value{
Returns a \code{data.frame}
}

\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}

\examples{
var1 <- opbase.data("Op_en5674")
var1 <- tidy(var1, "var1")
var1
}

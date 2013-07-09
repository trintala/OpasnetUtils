\name{Fetch}
\alias{Fetch}
\alias{Fetch2}
\title{
Fetch R objects described in a data.frame
}
\description{
Download a batch of R objects from Opasnet servers.
}
\usage{
Fetch(dependencies, evaluate = FALSE, indent = 0, verbose = TRUE, ...)
Fetch2(...)
}
\arguments{
  \item{dependencies}{
  \code{data.frame} which defines variable names and "locations"
}
  \item{evaluate}{
  \code{TRUE} to run \code{\link{EvalOutput}} on each variable (non-\code{ovariables} are ignored)
}
  \item{indent}{
  \code{integer} internal argument for verbose printing
}
  \item{verbose}{
  use \code{TRUE} to enable status messages in between fetches
}
  \item{\dots}{
  excess arguments are ignored or passed to \code{EvalOutput} if evaluate is \code{TRUE}
}
}
\details{
The input \code{data.frame} should have columns "Name" and at least one of "Key" and "Ident".

Key is the R-tools session identifier (shown at the end of the url).
Ident should be in format <page_id>/<code_name>.

Fetch first checks if the variable (or something with the same name) is already available, if it is nothing will be done.
If Key is defined (not NA or "") for a variable it takes precedence over Ident. 

Fetch is run as first part of \code{\link{ComputeDependencies}}. 

See also:
\url{http://en.opasnet.org/}
}
\value{
No return value. Fetched variables are written in \code{.GlobalEnv}.
}
\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}
\examples{
deps <- data.frame(Name = "exposure", Key = "6WYTFxiZUIxiY8tw")
#Fetch(deps)
#exposure

# If variable exists
exposure <- 1
Fetch(deps)
exposure # by default nothing is changed
}

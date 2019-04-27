\name{continuousOps}
\alias{continuousOps}
\title{
Simple continuous index merge and Ops
}
\description{
continuousOps merges two ovariables by continuous indices and performs an operation.
}
\usage{
continuousOps(O1, O2, fun, cols = NULL)
}
\arguments{
  \item{O1}{
  ovariable
}
  \item{O2}{
  ovariable
}
  \item{fun}{
  function or function name (\code{character}) that is to be performed. Typically '*', '+' or some other Ops.
}
  \item{cols}{
  character vector of column names that contain continuous indices. Only needed if there are > 1 indices and the order is critical. 
  Otherwise, shared continuous indices are identified automatically.
}
}
\details{
O1 is of main interest, while O2 has information that links to O2 via continuous index or indices.
All locations in these continuous indices of O1 are created for O2 assuming that the value in the previous location of cols applies.
Note that this is asymmetric. Locations in O2 that are missing from O1 are omitted.
continuousOps assumes that all continuous indices are in the same dimension, the first one being the main index.
Additional indices affect the outcome only if there are (approximate) ties. Therefore, avoid using this with several continuous indices.
However, if continuous indices are NOT shared by both O1 and O2, they cause no trouble.

See also:
\url{http://en.opasnet.org/}
}
\value{
Returns an \code{Ovariable}
}

\author{
J. Tuomisto \email{jouni.tuomisto@thl.fi}
}

%\examples{
%%s#
%}

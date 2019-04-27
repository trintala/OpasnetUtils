\name{unkeep}
\alias{unkeep}
\title{
Remove unnecessary columns
}
\description{
Removes columns in ovariable outputs
}
\usage{
unkeep(X, cols = NA, sources = FALSE, prevresults = FALSE) 
}
\arguments{
  \item{X}{
  an \code{ovariable} to be worked on
}
  \item{cols}{
  \code{character} vector of columns to be removed
}
  \item{sources}{
  if TRUE removes "...Source"-columns
}
  \item{prevresults}{
  if TRUE removes Result-columns of other variables
}
}
\value{
\code{ovariable}
}

\author{
J. Tuomisto \email{jouni.tuomisto@thl.fi}
}


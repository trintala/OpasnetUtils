\name{odag}
\alias{odag}
\title{
Directed Acyclic Graph Visualization of Ovariables
}
\description{
Makes a directed acyclic graph (DAG) out of all ovariables and their dependencies in the global environment.
}
\usage{
odag(plotting = TRUE, ...)
}
\arguments{
  \item{plotting}{
  plots the DAG if \code{TRUE}, else returns \code{\link{graph.data.frame}}
}
  \item{...}{
  ignored
}
}
\details{
Finds all \code{\link[=ovariable-class]{ovariables}} in \code{.GlobalEnv}, finds edges (dependencies) between them and uses \code{\link{igraph}} to produce a DAG. 

See also:
\url{http://en.opasnet.org/}
}
\value{
Nothing or a \code{\link{graph.data.frame}}.
}

\author{
J. Tuomisto \email{jouni.tuomisto@thl.fi}
}

%\examples{
%
%}

\name{Optimize}
\alias{Optimize}
\alias{Minimize}
\title{
Simple optimization between discrete index locations
}
\description{
Finds the minimal value of an \code{ovariable} along given indices. Returns sought rows and drops the rest. 
}
\usage{
Optimize(...)
Minimize(data, indices)
}

\arguments{
  \item{\dots}{
  arguments passed to Minimize
}
  \item{data}{
  \code{\link{ovariable}} or \code{data.frame} of suitable format (must contain a "Result" column)
}
  \item{indices}{
  \code{character} vector of index names meant to be retained (\code{tapply} \code{INDEX})
}
}
\details{
Uses \code{\link{tapply}} and \code{\link{which.min}}. 
}
\value{
Returns a \code{data.frame} which is a subset of the original \code{data.frame} or \code{ovariable@output}.
}

\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}

\examples{
a <- Ovariable(output = data.frame(A = letters[c(1,1,2,2)], B = c(1,2,1,2), Result = 1:4))
Minimize(a, "B")
Minimize(a, "A")
}


\name{CollapseTableParser}
\alias{CollapseTableParser}
\title{
Parsing Collapse orders from a table
}
\description{
Parses \code{data.frames} of specific format to produce "Col" prefixed lists for \code{\link{CheckCollapse}}
}
\usage{
CollapseTableParser(CTable, env = .GlobalEnv)
}
\arguments{
  \item{CTable}{
  a data.frame with columns "Variable" (variable names), "Index" (column names) and "Probs" (propabilities of column levels in marginal distribution, comma separated)
}
  \item{env}{
  target \code{environment}, default is \code{.GlobalEnv}
}
}
\details{
Used in the Opasnet assessments/analyses to produce multiple model specific Collapse instructions. Using other distribution values
than 1 requires knowledge about the amount and order of unique index values. \code{Probs} values \code{1} and \code{NA} are considered equal weighting. 

See also:
\url{http://en.opasnet.org/}
}
\value{
No return value, "Col" prefixed variables are written straight into specified \code{environment}.
}
\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}
\examples{
a <- data.frame(Variable = "test", Index = c("City"), Probs = 1)
CollapseTableParser(a)
Coltest
}


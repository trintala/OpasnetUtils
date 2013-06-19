\name{OpasnetUtils-package}
\alias{OpasnetUtils-package}
\alias{OpasnetUtils}
\docType{package}
\title{Open Assessors Network Modeling Utilities}
\description{
  This package contains tools made for building stochastic models within Opasnet (\url{http://www.opasnet.org}). 
}
\details{
\tabular{ll}{
Package: \tab OpasnetUtils\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Imports: \tab methods, rjson, RCurl, xtable, reshape2, triangle, httpRequest, digest\cr
License: \tab gpl\cr
LazyLoad: \tab yes\cr
Date: \tab 2013-06-xx\cr
}

Index:
\preformatted{
OpasnetUtils-package    Open Assessors' Network modeling platform
                        utilities.
}

  The operating principle is maximum modularity. Variables are defined publicly on wiki pages using wiki inputs/tables, our database 
  and R code. Using any predefined variable is very easy: fetch the variable from our servers (or your own) and evaluate it. Actual 
  evaluation of variables is done lazily by default: when the evaluation of a variable is explicitly called, all variables it is dependent 
  on are evaluated recursively. There are also a few impact assessment tools like GIS in the package. 
  To learn more go to \url{http://en.opasnet.org/w/Opasnet_wiki-R_modeling_platform}.
}

\author{
T. Rintala, \email{teemu.rintala.a@gmail.com}\cr
E. Happonen, \email{einari.happonen@thl.fi}

Maintainer: E. Happonen <einari.happonen@thl.fi>
}

\keyword{ package }

\seealso{
  \url{http://en.opasnet.org/}
}
\name{OpasnetUtils-package}
\alias{OpasnetUtils-package}
\alias{OpasnetUtils}
\docType{package}
\title{Open Assessors Network Modelling Utilities}
\description{
  This package contains tools made for building stochastic models within Opasnet (\url{http://www.opasnet.org}). 
}
\details{
\tabular{ll}{
Package: \tab OpasnetUtils\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Imports: \tab methods, rjson, RCurl, xtable, reshape2, triangle, httpRequest, digest\cr
License: \tab GPL-3\cr
LazyLoad: \tab yes\cr
Date: \tab 2013-07-09\cr
}

Index:
\preformatted{
Check                   Ovariable Checks
CollapseMarginal        Collapse marginals
CollapseTableParser     Parsing Collapse orders from a table
ComputeDependencies     Evaluate ovariable dependencies
DecisionTableParser     Parse data.frame for decisions
EvalOutput              Evaluate ovariable output
Fetch                   Fetch R objects described in a data.frame
GIS                     Handling spatially distributed variables
OpasnetUtils-package    Open Assessors Network Modeling Utilities
Ovariable               Ovariable constructor
convert.units           Converting units
ddata_apply             Dynamic data link activation
dropall                 Dropall
fillna                  Interpreting empty locations in indices
interpret               Parse human readable distribution definitions
oapply                  Apply for ovariables
objects                 Server side shared R objects
odecision-class         Class '"odecision"'
op_base                 Functions for Interaction with the Opasnet Base
                        (obsolete)
opasnet                 Importing files from Opasnet
opbase                  OpasnetBase Access
oprint                  Print ovariables or data frames in html format.
orbind                  Rowbinding ovariables
ovariable-class         Class '"ovariable"'
result                  Access result vector of an ovariable
tidy                    Format database output for use in ovariables
}

  The operating principle of this package is maximum modularity. Variables are defined publicly on wiki pages using wiki inputs/tables, our database 
  and R code. Using any predefined variable is very easy: fetch the variable from our servers (or your own) and evaluate it. Actual 
  evaluation of variables is done lazily by default: when the evaluation of a variable is explicitly called, all variables it is dependent 
  on are evaluated recursively. There are also a few impact assessment tools like a few GIS functions in the package. 
  To learn more go to \url{http://en.opasnet.org/w/Modelling_in_Opasnet}.
}

\author{
T. Rintala, \email{teemu.rintala.a@gmail.com}\cr
E. Happonen, \email{einari.happonen@thl.fi}\cr
J. Tuomisto, \email{jouni.tuomisto@thl.fi}

Maintainer: E. Happonen <einari.happonen@thl.fi>
}

\keyword{ package }
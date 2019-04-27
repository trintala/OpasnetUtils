\name{odecision-class}
\Rdversion{1.1}
\docType{class}
\alias{odecision-class}
\alias{odecision}

\title{Class \code{"odecision"}}
\description{
Definition container for \code{\link{CheckDecisions}}
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("odecision", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{dectable}:}{Object of class \code{"data.frame"} describes the decisions and their relevant options. It is merged with the output slot \code{data.frame} of an \code{\link{ovariable}}}
    \item{\code{condition}:}{Object of class \code{"list"} contains functions which return a logical vector that should indicate the relevant rows to be affected by a decision-option combination.}
    \item{\code{effect}:}{Object of class \code{"list"} contains functions which describe the effects of the decision on relevant rows of the output.}
  }
}
\section{Methods}{
No methods defined with class "odecision" in the signature.
}
\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}
\details{
Usually \code{odecision}s are created by \code{\link{DecisionTableParser}} using a full decision table that includes
condition and effect descriptions in standard form. \code{Odecision}s created by \code{DecisionTableParser} do not have
condition or effect defined. Instead \code{\link{CheckDecisions}} does the final parsing into preset effects and conditions. 
For non-standard conditions and effects decisions can be defined using the \code{new("odecision", ...)} call.

See also:
\url{http://en.opasnet.org/}
}

%\examples{
%%showClass("odecision")
%}
\keyword{classes}

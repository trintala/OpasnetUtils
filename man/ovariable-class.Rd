\name{ovariable-class}
\Rdversion{1.1}
\docType{class}
\alias{ovariable-class}
\alias{ovariable}
\alias{Math,ovariable-method}
\alias{merge,data.frame,ovariable-method}
\alias{merge,numeric,ovariable-method}
\alias{merge,ovariable,data.frame-method}
\alias{merge,ovariable,numeric-method}
\alias{merge,ovariable,ovariable-method}
\alias{Ops,numeric,ovariable-method}
\alias{Ops,ovariable,numeric-method}
\alias{Ops,ovariable,ovariable-method}
\alias{plot,ovariable-method}
\alias{plot,ovariable,ANY-method}
\alias{summary,ovariable-method}
\alias{unique,ovariable-method}
\alias{$,ovariable-method}
\alias{$<-,ovariable-method}
\alias{[,ovariable,ANY,ANY-method}
\alias{[,ovariable,ANY,character-method}
\alias{[,ovariable,ANY,missing-method}

\title{Class \code{"ovariable"}}
\description{
Standard modelling variables for the Opasnet modelling framework
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ovariable", ...)}.
Or by using the \code{\link{Ovariable}}-constructor.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{ Object of class \code{"character"} name of variable, should match object name }
    \item{\code{output}:}{ Object of class \code{"data.frame"} output from formula and/or data operations }
    \item{\code{data}:}{ Object of class \code{"data.frame"} data describing the variable, should have a "Result" column}
    \item{\code{marginal}:}{ Object of class \code{"logical"} identifies output columns which are considered indices }
    \item{\code{formula}:}{ Object of class \code{"function"} a function that produces a \code{data.frame} that describes this variable }
    \item{\code{dependencies}:}{ Object of class \code{"data.frame"} list of variables that are used in \code{formula}, format is described in details for \code{\link{Fetch}} }
    \item{\code{ddata}:}{ Object of class \code{"character"} specifies an Opasnet page identifier (Op_enXXXX) which will be used to download most recent data on this variable in the Opasnet database }
    \item{\code{meta}:}{ Object of class \code{"list"} , contains additional information such as the initialization timestamp and the initializers wiki user name when run in Opasnet }
  }
}
\section{Methods}{
  \describe{
    \item{Math}{\code{signature(x = "ovariable")}: \code{\link{Math}} will be applied on Result column of output }
    \item{merge}{\code{signature(x = "data.frame", y = "ovariable")}: \code{data.frame} will be converted to \code{ovariable} (with only output slot defined) and then merged }
    \item{merge}{\code{signature(x = "numeric", y = "ovariable")}: \code{numeric} is converted to \code{data.frame} and then to \code{ovariable} and then merged }
    \item{merge}{\code{signature(x = "ovariable", y = "data.frame")}: same as above }
    \item{merge}{\code{signature(x = "ovariable", y = "numeric")}: same as above }
    \item{merge}{\code{signature(x = "ovariable", y = "ovariable")}: output slots will be merged with all = TRUE, a blank ovariable with only output defined is returned }
    \item{Ops}{\code{signature(e1 = "numeric", e2 = "ovariable")}: \code{numeric} is converted to \code{data.frame} and then to \code{ovariable} and then operated }
    \item{Ops}{\code{signature(e1 = "ovariable", e2 = "numeric")}: same as above }
    \item{Ops}{\code{signature(e1 = "ovariable", e2 = "ovariable")}: the \code{ovariable}s are merged and then the two Result columns are operated unto, the result is saved in another Result column (or the same if they are not named: "Var1Result" vs "Result") }
    \item{plot}{\code{signature(x = "ovariable")}: plots a simple comparison between sources (data vs formula) }
    \item{summary}{
      \code{signature(object = "ovariable")}: returns a \code{data.frame}. Takes \code{function_names} and \code{marginals} as extra arguments. The former matches 
      \code{character vector} elements into functions which will be \code{tapplied} with. The latter matches \code{character vector} elements to output 
      \code{data.frame} columns which define INDEX. The default is to \code{tapply} over iterations using \code{mean}, \code{sd}, \code{min}, \code{quantile(probs=0.025)}, \code{median}, \code{quantile(probs=0.975)} and \code{max}. 
    }
    \item{unique}{ \code{signature(x = "ovariable")}: applies \code{\link{unique}} to the \code{output}-slot \code{data.frame} and returns a copy of the original \code{ovariable} where the output has been replaced with the unique rows. } 
    \item{$}{ \code{signature(x = "ovariable")}: provides direct access to the \code{output}-slot \code{data.frame} columns } 
    \item{$<-}{ \code{signature(x = "ovariable")}: insert into \code{output}-slot \code{data.frame} column } 
    \item{[}{ \code{signature(x = "ovariable", i = "ANY", j = "ANY")}: select subset from \code{output}-slot \code{data.frame}, returning an \code{ovariable} } 
    \item{[}{ \code{signature(x = "ovariable", i = "ANY", j = "character")}: select subset from \code{output}-slot \code{data.frame}, returning an \code{ovariable} } 
    \item{[}{ \code{signature(x = "ovariable", i = "ANY", j = "missing")}: select subset from \code{output}-slot \code{data.frame}, returning an \code{ovariable} } 
    
  }
}

\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}

\seealso{
\code{\link{Ovariable}}
}
%\examples{
%%showClass("ovariable")
%}
\keyword{classes}

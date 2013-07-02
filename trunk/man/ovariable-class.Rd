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
\alias{summary,ovariable-method}

\title{Class \code{"ovariable"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ovariable", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{output}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{data}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{marginal}:}{Object of class \code{"logical"} ~~ }
    \item{\code{formula}:}{Object of class \code{"function"} ~~ }
    \item{\code{dependencies}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{ddata}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{convert.units}{\code{signature(x = "ovariable")}: ... }
    \item{Math}{\code{signature(x = "ovariable")}: ... }
    \item{merge}{\code{signature(x = "data.frame", y = "ovariable")}: ... }
    \item{merge}{\code{signature(x = "numeric", y = "ovariable")}: ... }
    \item{merge}{\code{signature(x = "ovariable", y = "data.frame")}: ... }
    \item{merge}{\code{signature(x = "ovariable", y = "numeric")}: ... }
    \item{merge}{\code{signature(x = "ovariable", y = "ovariable")}: ... }
    \item{oprint}{\code{signature(x = "ovariable")}: ... }
    \item{Ops}{\code{signature(e1 = "numeric", e2 = "ovariable")}: ... }
    \item{Ops}{\code{signature(e1 = "ovariable", e2 = "numeric")}: ... }
    \item{Ops}{\code{signature(e1 = "ovariable", e2 = "ovariable")}: ... }
    \item{plot}{\code{signature(x = "ovariable")}: ... }
    \item{print}{\code{signature(x = "ovariable")}: ... }
    \item{summary}{\code{signature(object = "ovariable")}: ... }
    \item{update}{\code{signature(object = "ovariable")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}
\note{
%%  ~~further notes~~
}

\section{Website}{
\url{http://en.opasnet.org/}
}

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("ovariable")
}
\keyword{classes}

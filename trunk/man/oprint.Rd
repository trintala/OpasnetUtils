\name{oprint}
\alias{oprint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print ovariables in html format.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
oprint(x, show_all = FALSE, sortable = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{show_all}{
%%     ~~Describe \code{show_all} here~~
}
  \item{sortable}{
%%     ~~Describe \code{sortable} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (x, show_all = FALSE, sortable = TRUE, ...) 
standardGeneric("oprint"), generic = structure("oprint", package = "OpasnetUtils"), package = "OpasnetUtils", group = list(), valueClass = character(0), signature = c("x", 
"show_all", "sortable"), default = structure(function (x, show_all = FALSE, 
    sortable = TRUE, ...) 
{
    if (nrow(x) > 1000 && !show_all) {
        print(paste("Showing first 1000 rows out of ", nrow(x), 
            ". Set show_all=TRUE to show all rows.", sep = ""))
        x = x[1:1000, ]
    }
    if (sortable) {
        print(xtable(x, ...), type = "html", html.table.attributes = "class='wikitable sortable'")
    }
    else {
        print(xtable(x, ...), type = "html", html.table.attributes = "class='wikitable'")
    }
  }, target = structure("ANY", class = structure("signature", package = "methods"), .Names = "x", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "x", package = "methods"), generic = structure("oprint", package = "OpasnetUtils"), class = structure("derivedDefaultMethod", package = "methods")), skeleton = (structure(function (x, 
    show_all = FALSE, sortable = TRUE, ...) 
{
    if (nrow(x) > 1000 && !show_all) {
        print(paste("Showing first 1000 rows out of ", nrow(x), 
            ". Set show_all=TRUE to show all rows.", sep = ""))
        x = x[1:1000, ]
    }
    if (sortable) {
        print(xtable(x, ...), type = "html", html.table.attributes = "class='wikitable sortable'")
    }
    else {
        print(xtable(x, ...), type = "html", html.table.attributes = "class='wikitable'")
    }
  }, target = structure("ANY", class = structure("signature", package = "methods"), .Names = "x", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "x", package = "methods"), generic = structure("oprint", package = "OpasnetUtils"), class = structure("derivedDefaultMethod", package = "methods")))(x, 
    show_all, sortable, ...), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{Fetch}
\alias{Fetch}
\alias{Fetch2}
\title{
Fetch R objects described in a data.frame
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Fetch2(dependencies, evaluate = FALSE, indent = 0, verbose = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dependencies}{
%%     ~~Describe \code{dependencies} here~~
}
  \item{evaluate}{
%%     ~~Describe \code{evaluate} here~~
}
  \item{indent}{
%%     ~~Describe \code{indent} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
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
T. Rintala \email{teemu.rintala.a@gmail.com}
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
function (dependencies, evaluate = FALSE, indent = 0, verbose = TRUE, 
    ...) 
{
    if (nrow(dependencies) > 0) {
        for (i in 1:nrow(dependencies)) {
            if (!exists(as.character(dependencies$Name[i]))) {
                testkey <- if (is.null(dependencies$Key[i])) 
                  TRUE
                else is.na(dependencies$Key[i]) | dependencies$Key[i] == 
                  ""
                testid <- if (is.null(dependencies$Ident[i])) 
                  TRUE
                else is.na(dependencies$Ident[i]) | dependencies$Ident[i] == 
                  ""
                if (testkey & testid) {
                  stop(paste("No key nor ident given for dependent variable: ", 
                    dependencies$Name[i], "!", sep = ""))
                }
                if (!testkey) {
                  objects.get(dependencies$Key[i])
                }
                if (testkey & !testid) {
                  ident <- strsplit(as.character(dependencies$Ident[i]), 
                    "/")[[1]]
                  objects.latest(ident[1], ident[2])
                }
                if (evaluate) 
                  assign(as.character(dependencies$Name[i]), 
                    EvalOutput(get(as.character(dependencies$Name[i])), 
                      ...), envir = .GlobalEnv)
                if (verbose) 
                  cat("\n", rep("-", indent), as.character(dependencies$Name[i]), 
                    "fetched successfully!\n")
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

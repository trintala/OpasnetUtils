\name{DecisionTableParser}
\alias{DecisionTableParser}
\title{
Parse data.frame for decisions
}
\description{
Parses a data.frame of suitable format into \code{\link{odecision}}s.
}
\usage{
DecisionTableParser(DTable)
}
\arguments{
  \item{DTable}{
data.frame in format described on \url{http://en.opasnet.org/w/}
}
}
\details{
Decisions consist of conditions and effects, target a certain variable and may have multiple options. 
Saves \code{odecision} class objects into \code{.GlobalEnv}.
}
\value{
No return value. 
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
function (DTable) 
{
    for (i in unique(as.character(DTable$Variable))) {
        temp <- DTable[DTable$Variable == i, ]
        out <- new("odecision", dectable = temp)
        assign(paste("Dec", i, sep = ""), out, envir = .GlobalEnv)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

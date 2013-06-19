\name{fillna}
\alias{fillna}
\title{
Interpreting empty locations in indices
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fillna(object, marginals)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{marginals}{
%%     ~~Describe \code{marginals} here~~
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
function (object, marginals) 
{
    a <- dropall(object)
    for (i in marginals) {
        a[[i]] <- as.factor(a[[i]])
        a1 <- a[!is.na(a[[i]]), ]
        a2 <- a[is.na(a[[i]]), ][-i]
        addition <- data.frame(A = levels(a[[i]]))
        colnames(addition) <- colnames(a)[i]
        a2 <- merge(addition, a2)
        a <- rbind(a1, a2)
    }
    return(a)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

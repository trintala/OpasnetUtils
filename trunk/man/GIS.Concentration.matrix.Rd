\name{GIS.Concentration.matrix}
\alias{GIS.Concentration.matrix}
\title{
Spatial concentration map based on source point emission
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GIS.Concentration.matrix(Emission, LO, LA, distx = 10.5, disty = 10.5, resolution = 1, N = 1000, dbug = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Emission}{
%%     ~~Describe \code{Emission} here~~
}
  \item{LO}{
%%     ~~Describe \code{LO} here~~
}
  \item{LA}{
%%     ~~Describe \code{LA} here~~
}
  \item{distx}{
%%     ~~Describe \code{distx} here~~
}
  \item{disty}{
%%     ~~Describe \code{disty} here~~
}
  \item{resolution}{
%%     ~~Describe \code{resolution} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{dbug}{
%%     ~~Describe \code{dbug} here~~
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
function (Emission, LO, LA, distx = 10.5, disty = 10.5, resolution = 1, 
    N = 1000, dbug = FALSE, ...) 
{
    LaPerKm <- dtheta.dy(earth.radius)
    LoPerKm <- dphi.dx(earth.radius, LA)
    PILTTI.matrix <- tidy(op_baseGetData("opasnet_base", "Op_en5797", 
        ...), objname = "PILTTI.matrix")
    if (N == 0) {
        PILTTI.matrix <- as.data.frame(as.table(tapply(PILTTI.matrix[["PILTTI.matrixResult"]], 
            PILTTI.matrix[, c("dx", "dy")], mean)))
        colnames(PILTTI.matrix)[colnames(PILTTI.matrix) == "Freq"] <- "PILTTI.matrixResult"
    }
    else {
        ID.list <- tapply(1:nrow(PILTTI.matrix), PILTTI.matrix[, 
            c("Kaupunki", "Vuosi", "Tyyppi")], list)
        ID.list.samples <- sample(ID.list, N, replace = TRUE)
        ID.sample.lengths <- sapply(ID.list.samples, length)
        ID.vec <- unlist(ID.list.samples)
        PILTTI.matrix <- PILTTI.matrix[ID.vec, c("dx", "dy", 
            "PILTTI.matrixResult")]
        PILTTI.matrix$Iter <- rep(1:N, times = ID.sample.lengths)
    }
    PILTTI.matrix$dy <- as.numeric(as.character(PILTTI.matrix$dy))
    PILTTI.matrix$dx <- as.numeric(as.character(PILTTI.matrix$dx))
    PILTTI.matrix$LObin <- cut(PILTTI.matrix$dx/1000 * LoPerKm + 
        LO, breaks = LO + seq(-distx, distx, resolution) * LoPerKm)
    PILTTI.matrix$LAbin <- cut(PILTTI.matrix$dy/1000 * LaPerKm + 
        LA, breaks = LA + seq(-disty, disty, resolution) * LaPerKm)
    PILTTI.matrix <- new("ovariable", name = "PILTTI.matrix", 
        output = PILTTI.matrix, marginal = colnames(PILTTI.matrix) \%in\% 
            c("LObin", "LAbin", "Iter"))
    if (dbug) {
        cat(colnames(PILTTI.matrix@output), "\n")
        cat(colnames(Emission@output), "\n")
    }
    out <- PILTTI.matrix * Emission
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\name{GIS.Exposure}
\alias{GIS.Exposure}
\title{
Population exposure to airborne pathogen concentration (spatial)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GIS.Exposure(Concentration.matrix, LO = NULL, LA = NULL, distx = 10.5, disty = 10.5, resolution = 1, dbug = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Concentration.matrix}{
%%     ~~Describe \code{Concentration.matrix} here~~
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
function (Concentration.matrix, LO = NULL, LA = NULL, distx = 10.5, 
    disty = 10.5, resolution = 1, dbug = FALSE, ...) 
{
    if (is.null(LO) | is.null(LA)) {
        bounds = unique(Concentration.matrix@output[c("LAbin", 
            "LObin")])
        LAlower = NA
        LAupper = NA
        LOlower = NA
        LOupper = NA
        PopLocs.la <- opbase.locations("Heande3182", "Latitude", 
            username = "heande", password = opbase.read_auth("heande"))
        PopLocs.la <- unlist(PopLocs.la)
        PopLocs.lo <- opbase.locations("Heande3182", "Longitude", 
            username = "heande", password = opbase.read_auth("heande"))
        PopLocs.lo <- unlist(PopLocs.lo)
        Population <- NULL
        first <- TRUE
        for (i in 1:nrow(bounds)) {
            tmp <- strsplit(as.character(bounds$LAbin[i]), ",")[[1]]
            LAlower[i] <- substring(tmp[1], 2, nchar(tmp[1]))
            LAupper[i] <- substring(tmp[2], 1, nchar(tmp[2]) - 
                1)
            tmp <- strsplit(as.character(bounds$LObin[i]), ",")[[1]]
            LOlower[i] <- substring(tmp[1], 2, nchar(tmp[1]))
            LOupper[i] <- substring(tmp[2], 1, nchar(tmp[2]) - 
                1)
            LAlocs <- PopLocs.la[as.numeric(PopLocs.la) > as.numeric(LAlower) & 
                as.numeric(PopLocs.la) <= as.numeric(LAupper)]
            LOlocs <- PopLocs.lo[as.numeric(PopLocs.lo) > as.numeric(LOlower) & 
                as.numeric(PopLocs.lo) <= as.numeric(LOupper)]
            if (length(LAlocs) == 0 || length(LOlocs) == 0) {
                warning(paste("Population data missing in LA", 
                  LAlower[i], "to", LAupper[i], "LO", LOlower[i], 
                  "to", LOupper[i]))
            }
            else {
                inc = list(Latitude = LAlocs, Longitude = LOlocs)
                pop <- tidy(opbase.data("Heande3182", username = "heande", 
                  password = opbase.read_auth("heande"), include = inc))
                if (first) {
                  Population <- pop
                  first <- FALSE
                }
                else {
                  Population <- rbind(Population, pop)
                }
            }
        }
        if (is.null(Population)) 
            stop("No population data at these coordinates.")
        if (dbug) 
            print(nrow(Population))
        LAcuts <- unique(c(-Inf, as.numeric(LAlower), as.numeric(LAupper), 
            Inf))
        LOcuts <- unique(c(-Inf, as.numeric(LOlower), as.numeric(LOupper), 
            Inf))
        Population$LAbin <- cut(as.numeric(as.character(Population$Latitude)), 
            LAcuts)
        Population$LObin <- cut(as.numeric(as.character(Population$Longitude)), 
            LOcuts)
    }
    else {
        LaPerKm <- dtheta.dy(earth.radius)
        LoPerKm <- dphi.dx(earth.radius, LA)
        if (dbug) {
            cat("LaPerKm = ", LaPerKm, "\n")
            cat("LoPerKm = ", LoPerKm, "\n")
        }
        Population <- function(LO, LA, LaPerKm, LoPerKm, distx = 10.5, 
            disty = 10.5, dbug = FALSE) {
            GetPopLocs <- function(index_name) {
                return(opbase.locations("Heande3182", index_name, 
                  username = "heande", password = opbase.read_auth("heande")))
            }
            GetPopData <- function(...) {
                return(opbase.data("Heande3182", username = "heande", 
                  password = opbase.read_auth("heande"), ...))
            }
            if (dbug) 
                print("Fetching latitudes and longitudes...")
            locs.la <- GetPopLocs("Latitude")
            locs.lo <- GetPopLocs("Longitude")
            if (dbug) 
                print("Done!")
            pop.slice.la <- locs.la[locs.la < LA + disty * LaPerKm & 
                locs.la > LA - disty * LaPerKm]
            pop.slice.lo <- locs.lo[locs.lo < LO + distx * LoPerKm & 
                locs.lo > LO - distx * LoPerKm]
            if (length(pop.slice.lo) == 0 || length(pop.slice.la) == 
                0) 
                stop("No population on selected LA + LO")
            if (dbug) {
                cat("Matching LA locations in population data: ", 
                  paste(pop.slice.la, collapse = ", "), ".\n")
                cat("Matching LO locations in population data: ", 
                  paste(pop.slice.lo, collapse = ", "), ".\n")
            }
            if (dbug) 
                print("Fetching the population data...")
            Population <- tidy(GetPopData(include = list(Latitude = pop.slice.la, 
                Longitude = pop.slice.lo)))
            if (dbug) 
                print("Done!")
            Population$Longitude <- as.numeric(as.character(Population$Longitude))
            Population$Latitude <- as.numeric(as.character(Population$Latitude))
            return(Population)
        }
        Population <- Population(LO, LA, LaPerKm, LoPerKm, dbug = dbug)
        if (dbug) 
            print(nrow(Population))
        Population$LObin <- cut(Population$Longitude, breaks = LO + 
            seq(-distx, distx, resolution) * LoPerKm)
        Population$LAbin <- cut(Population$Latitude, breaks = LA + 
            seq(-disty, disty, resolution) * LaPerKm)
    }
    Population <- new("ovariable", output = Population)
    Population@marginal <- colnames(Population@output) \%in\% c("Iter", 
        "LObin", "LAbin")
    if (dbug) {
        cat(colnames(Concentration.matrix@output), "\n")
        cat(colnames(Population@output), "\n")
    }
    temp <- Population * Concentration.matrix
    if (dbug) 
        cat(colnames(temp@output), "\n")
    out <- tapply(temp@output$Result, temp@output[, colnames(temp@output)[temp@marginal & 
        !colnames(temp@output) \%in\% c("LObin", "LAbin")]], sum, 
        na.rm = TRUE)
    out <- as.data.frame(as.table(out))
    colnames(out)[colnames(out) == "Freq"] <- "Result"
    temp@marginal <- colnames(out) \%in\% colnames(temp@output)[temp@marginal] & 
        !colnames(out) \%in\% c("LObin", "LAbin")
    temp@output <- out
    return(temp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

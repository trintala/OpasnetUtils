\name{GIS}
\alias{GIS}
\alias{GIS.Exposure}
\alias{GIS.Concentration.matrix}
\title{
Opasnet spatially distributed data interfaces
}
\description{
Currently there are only two GIS functions: one for producing spatial concentration maps (\code{GIS.Concentration.matrix}) and one for using spatial 
population data and concentration maps to calculate exposure (\code{GIS.Exposure}). 
}
\usage{
GIS.Concentration.matrix(Emission, LO, LA, distx = 10.5, disty = 10.5, 
    resolution = 1, N = 1000, dbug = FALSE, ...)
GIS.Exposure(Concentration.matrix, dbug = FALSE, ...)
}

\arguments{
  \item{Emission}{
  \code{numeric}, emission in any units; can be an \code{ovariable} as well
}
  \item{Concentration.matrix}{
  \code{\link{ovariable}} concentration matrix, usually produced by \code{GIS.Concentration.matrix}
}
  \item{LO}{
  \code{numeric}, longitude corresponding to the center of the considered area (emission source)
}
  \item{LA}{
  \code{numeric}, latitude corresponding to the center of the considered area (emission source)
}
  \item{distx}{
  \code{numeric}, maximum distance from center on the x axis of the area, 10.5 corresponds to the source-receiver-matrices used
}
  \item{disty}{
  \code{numeric}, maximum distance from center on the y axis of the area, 10.5 corresponds to the source-receiver-matrices used
}
  \item{resolution}{
  \code{numeric}, size of the grid, default 1 is 1km x 1km grid
}
  \item{N}{
  \code{integer}, number of iterations to run
}
  \item{dbug}{
  use \code{TRUE} to turn debug prints on
}
  \item{\dots}{
  excess arguments are ignored or passed to \code{\link{tidy}} on data download
}
}
\details{
The concentration matrix is computed using PILTTI source-receiver-matrices (\url{http://en.opasnet.org/w/Piltti_source-receptor_matrix}). 
They are originally for modeling PM2.5 distributions in a few Finnish cities between the years 2000 and 2003. To produce a rudimentary probability distribution 
these matrices are randomized between iterations. 

Exposure is calculated by matching a concentration matrix to Finnish population data (\url{http://en.opasnet.org/w/Special:Opasnet_Base?id=op_en2949.2012}).

LA and LO are not required arguments for exposure, but speed the computation significantly.

See also:
\url{http://en.opasnet.org/}
}
\value{
\code{GIS.Concentration.matrix} returns an \code{ovariable} whose output is a grid defined as bins for coordinates. 

\code{GIS.Exposure} returns an \code{ovariable} whose output is concentration * population. All population data that matches cells defined by the
concentration matrix is included in the output.
}

\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}

\examples{
\dontrun{
# Excerpt from http://en.opasnet.org/w/Health_impacts_of_fine_particles_in_Rauma 
# (not evaluated)

# Paasto Emissions

Paasto <- new(
	"ovariable", 
	name = "Paasto",
	dependencies = data.frame(Name = "tieliikennepaastot", Key = "0194s0uuucjxq8Wi"),
	formula = function(dependencies, ...) {
		ComputeDependencies(dependencies, ...)
		
		# Muutetaan paivapaasto vuosipaastoksi ja grammat tonneiksi
		out <- tieliikennepaastot * 365 * 1E-6 

		return(out)
	}
)

# Muita tarpeellisia arvoja Other important values

bg.mort <- 45182 / 5203826 # same values as used in PILTTI

## J. T. Tuomisto, A. Wilson, et al. Uncertainty in mortality response to 
## airborne fine particulate matter... 2008
erf <- 0.0097 
# unit: m^3 /ug

# Ovariablet

## Pitoisuudet Concentrations

Pitoisuus <- new(
	"ovariable", 
	name = "Pitoisuus",
	dependencies = data.frame(
		Name = c("Paasto", "LO", "LA")
	),
	formula =  function(dependencies, ...) {
		ComputeDependencies(dependencies, ...)

		temp <- GIS.Concentration.matrix(Paasto, LO, LA, ...)

		return(temp)
	} 
)

## Altistuminen Exposure

Altistuminen <- new(
	"ovariable", 
	name = "Altistuminen", 
	dependencies = data.frame(
		Name = c("Pitoisuus", "LO", "LA")
	),
	formula = function(dependencies, ...) {
		ComputeDependencies(dependencies, ...)

		out <- GIS.Exposure(Pitoisuus, LO, LA, ...)

		return(out)
	}
)

}
}

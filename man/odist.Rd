\name{odist}
\alias{odist}
\alias{odirichlet}
\title{
Random sampling using ovariables as parameters
}
\description{
Currently there are only GIS functions for producing spatial concentration maps (\code{GIS.Concentration.matrix}) and using (closed) spatial 
population data to calculate exposure (\code{GIS.Exposure}). 
}
\usage{
odirichlet(a, n = 0, ...)
}

\arguments{
  \item{a}{
  \code{ovariable} containing distribution parameters
}
  \item{n}{
  \code{numeric}, number of samples. If 0, openv$N is used instead.
}
  \item{\dots}{
  arguments passed to \code{\link{oapply}}
}
}
\details{
Odirichlet is based on functions ddirichlet and rdirichlet from gtools dirichlet.R (originally contributed by Ian Wilson). The "Dirichlet function" 
is the multidimensional generalization of the beta distribution: it's the Bayesian canonical distribution for the parameter estimates of a
multinomial distribution. Odirichlet samples from the dirichlet distribution given parameter vectors that are processed by oapply. 
}
\value{
\code{ovariable}
}

\author{
T. Rintala \email{teemu.rintala.a@gmail.com}
}

\examples{
openv$N <- 5

test <- Ovariable(
		output=data.frame(
				a = 1:4, 
				b = rep(letters[1:4], each = 4), 
				c = rep(toupper(letters[1:4]), each = 4^2), 
				Result = 1:(4^3)), 
		marginal=c(rep(TRUE, 3), FALSE))
out <- odirichlet(test, cols = "a")
oapply(out, FUN = sum, cols = "a")

test <- Ovariable(
		output=data.frame(
				a = 1:4, 
				b = rep(letters[1:4], each = 4), 
				Iter = rep(toupper(letters[1:4]), each = 4^2), 
				Result = 1:(4^3)), 
		marginal=c(rep(TRUE, 3),FALSE))
out <- odirichlet(test, cols = "a")
oapply(out, FUN = sum, cols = "a")
}


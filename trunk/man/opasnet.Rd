\name{opasnet}
\alias{opasnet}
\alias{opasnet.data}
\alias{opasnet.csv}
\title{
Importing files from Opasnet
}
\description{
Functions for downloading files from Opasnet Media Wiki environments.
}
\usage{
opasnet.data(filename, wiki = "", unzip = "")
opasnet.csv(filename, wiki = "", unzip = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
  Path to file in Opasnet after the "images/" part.
}
  \item{wiki}{
  Name of the Opasnet wiki: "opasnet_en" for en.opasnet.org, "opasnet_fi" for fi.opasnet.org or "heande" for heande.opasnet.org (accessible only within Heande wiki).
}
  \item{unzip}{
  Name of the file in the package (if compressed using zip). 
}
  \item{\dots}{
  Excess arguments will be passed to  \code{read.table} function when downloading csv-file.
}
}
\details{
These functions make it easy to download Opasnet files for being used in R. Required path (filename) for file must be resolved
using the corresponding Media Wiki. Big data files should always be compressed before uploading to Opasnet. Using the
\code{unzip}-argument makes it easy to download and directly use any zip-compressed files.
}
\value{
  \code{opasnet.data} returns the file data as is. \code{opasnet.csv} returns \code{data.frame} parsed from csv file.
}
\references{
  \url{http://en.opasnet.org/}
}
\author{
  E. Happonen \email{einari.happonen@thl.fi}
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link[utils]{read.table}}
}
\examples{
library(OpasnetUtils)
opasnet.csv("4/49/Test.zip", wiki = "opasnet_fi", unzip = 'ejpop.csv', sep=';')
opasnet.data("c/cc/Test_bugs_model.txt", wiki = "opasnet_en")
}


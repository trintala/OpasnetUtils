\name{opasnet}
\alias{opasnet}
\alias{opasnet.data}
\alias{opasnet.csv}
\alias{opasnet.page}
\title{
Importing files from Opasnet
}
\description{
Functions for downloading files from Opasnet Media Wiki environments.
}
\usage{
opasnet.data(filename, wiki = "", unzip = "")
opasnet.csv(filename, wiki = "", unzip = "", ...)
opasnet.page(pagename, wiki = "")
}
\arguments{
  \item{filename}{
  Path to file in Opasnet after the "images/" part.
}
  \item{pagename}{
  Name of a page in Opasnet.
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
\code{opasnet.page} downloads wiki pages using \code{getURL} from \code{RCurl}.

See also:
\url{http://en.opasnet.org/}
}
\value{
  \item{opasnet.data}{Returns the file data as is.}
  \item{opasnet.csv}{Returns \code{data.frame} parsed from csv file.}
  \item{opasent.page}{Returns the web page as a single string.}
}
\author{
  E. Happonen \email{einari.happonen@thl.fi}, T. Rintala \email{teemu.rintala.a@gmail.com}
}
\seealso{
  \code{\link[utils]{read.table}}
}
\examples{
opasnet.csv("4/49/Test.zip", wiki = "opasnet_fi", unzip = 'ejpop.csv', sep=';')
opasnet.data("c/cc/Test_bugs_model.txt", wiki = "opasnet_en")
}


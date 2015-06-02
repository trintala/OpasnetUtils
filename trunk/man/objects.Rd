\name{objects}
\alias{objects}
\alias{objects.decode}
\alias{objects.encode}
\alias{objects.get}
\alias{objects.latest}
\alias{objects.put}
\alias{objects.store}
\title{
Server side shared R objects
}
\description{
Library for using R objects (like \code{\link{ovariable}}s.) stored in Opasnet R server. Also includes basic encryption and decryption functionality for R objects.
}
\usage{
objects.encode(obj, key)
objects.decode(eobj, key)
objects.get(token)
objects.latest(page_ident, code_name, verbose = FALSE)
objects.put(..., list = character())
objects.store(..., list = character(), verbose = FALSE)
}

\arguments{
  \item{obj}{
 Any R object.
}
  \item{eobj}{
  An encoded object returned by \code{objects.encode} function.
}
  \item{key}{
  Key string to encode or decode objects. Must be 16,32 or 64 characters in length.
}
  \item{token}{
  R-tools run token string to identify a stored object on Opasnet R server.
}
  \item{page_ident}{
  Opasnet Media Wiki page identifier (e.g. op_en1390).
}
  \item{code_name}{
  Name of the R code block in Opasnet Media Wiki (the \code{name} argument in \code{rcode} tag).
}
  \item{verbose}{
  Flag to set more verbose output (for debug purposes).
}
  \item{\dots}{
  Objects will be passed straight to R core \code{save} function.
}
  \item{list}{
  List will be passed straight to R core \code{save} function.
}
}
\details{
The main purpose of this library is to provide means to store R objects to Opasnet R server for later use. This is specifically useful
and embraced in Opasnet R ecosystem where this library is mainly used for storing and fetching \code{\link{ovariable}}s. Storing objects
is currently only possible within Opasnet Media Wiki environments (using R code inside \code{rcode} tags), but reading objects stored
by running code within public wikis is also possible from local R-installation.

Besides object storing, this library provides basic functionality to encode and decode R objects. This is done by using R base serialization functions
and \code{digest} library. Objects given to \code{objects.encode} will be encoded by using \code{AES} function in "ECB" mode.
Longer key (16, 32 or 64 characters) obviously means more secured encryption too. Same key must be used for both encryption and decryption.

See also:
\url{http://en.opasnet.org/}
}
\value{
  \item{objects.encode}{Returns encoded object to be decoded with \code{objects.decode} and given key.}
  \item{objects.decode}{Returns decoded object, as it was before encoding with \code{objects.encode}.}
  \item{objects.get}{Returns object or objects stored to Opasnet R server.}
  \item{objects.latest}{Returns object or objects stored to Opasnet R server.}
  \item{objects.put}{No return value.}
  \item{objects.store}{Returns token to identify stored objects on R server.}
}
\author{
  E. Happonen \email{einari.happonen@thl.fi} 
}
\seealso{
  \code{\link[base]{load}} \cr
  \code{\link[base]{save}} \cr
  \code{\link[base]{serialize}} \cr
  \code{\link[digest]{AES}}
}
\examples{

# Within Opasnet only! Let's assume that the (en.opasnet.org) page identifier - 
# where to code is - would be "Op_en1390" and code name "objs_save_test".
x <- stats::runif(20)
y <- list(a = 1, b = TRUE, c = "Jeah baby jeah!")
\dontrun{
objects.store(x, y)
}
# Fetching can be done also from local R installation.
\dontrun{
objects.latest("Op_en1390","objs_save_test")
}
print(x)
print(y)

# Object encrypt and decrypt

key <- "1234567890abcdef"

eobj <- objects.encode(y, key)
print(eobj)
obj <- objects.decode(eobj, key)
print(obj)
}

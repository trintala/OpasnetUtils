\name{opbase}
\alias{opbase}
\alias{opbase.data}
\alias{opbase.locations}
\alias{opbase.obj.exists}
\alias{opbase.series}
\alias{opbase.upload}
\title{
OpasnetBase Access
}
\description{
Function family for interacting with the Opasnet database.
}
\usage{
opbase.data(ident, series_id = NULL, subset = NULL, verbose = FALSE, 
    username = NULL, password = NULL, samples = NULL, exclude = NULL, 
    include = NULL,  range = NULL, optim_test = TRUE, ...)
opbase.locations(ident, index_name, series_id = NULL, username = NULL, 
    password = NULL)
opbase.obj.exists(ident, username = NULL, password = NULL)
opbase.series(ident, username = NULL, password = NULL, verbose = FALSE)
opbase.upload(input, ident = NULL, name = NULL, subset = NULL, 
    obj_type = "variable", act_type = "replace", language = "eng", 
    unit = "", who = NULL, rescol = NULL, chunk_size = NULL, verbose = FALSE, 
    username = NULL, password = NULL, index_units = NULL, index_types = NULL)
}

\arguments{
  \item{ident}{
  Object ident as string (e.g. "op_en1390"). Optional when uploading within Opasnet; page ident will be taken from the page where the code is.
}
  \item{series_id}{
  Series identifier as integer.
}
  \item{index_name}{
  Column name (index) whose locations should be returned.
}
  \item{subset}{
  Subset data name. Objects can have subsets of data, identified by subset names.
}
  \item{verbose}{
  Flag to view detailed debug output.
}
  \item{username}{
  Opasnet Base username.
}
  \item{password}{
  Opasnet Base password.
}
  \item{samples}{
  Limit the number of samples in result. Default is to get them all.
}
  \item{exclude}{
  Filter result by excluding rows that contain locations defined here as list. Works only with entity type indices!
}
  \item{include}{
  Filter result by only including rows that contain locations defined here as list. Works only with entity type indices!
}
  \item{range}{
  Filter result by setting ranges for index location values. Works only with number and time type indices!
}
  \item{optim_test}{
  Generally faster download, slower only when downloading large probability distributions from the database.
}
  \item{input}{
  Input data as \code{data.frame}.
}
  \item{name}{
  Object name for upload.
}
  \item{obj_type}{
  Object type string: 'variable', 'study', 'method', 'assessment', 'class', 'nugget' or 'encyclopedia'.
}
  \item{act_type}{
  Act type string: 'replace' or 'append'. Replace type uploads data to new series. Append adds new act to latest series.
}
  \item{language}{
  Data language identifier string in ISO 639.2 standard.
}
  \item{unit}{
  String identifying the result unit(s).
}
  \item{who}{
  Name or alias of the data uploading person.
}
  \item{rescol}{
  Name of the result column index.
}
  \item{chunk_size}{
  Size of upload data chunk in rows.
}
  \item{index_units}{
  Units for indices in vector as strings. E.g. \code{c('cm2','m2','ug/m3')}.
}
  \item{index_types}{
  Types for indices in vector as strings. Possible types are: 'entity' for limited set of locations, 'number' for real numbers and 'time' for date time strings. E.g. \code{c('entity','entity','number')}.  
}
  \item{\dots}{
  Excess arguments are ignored.
}
}
\details{
This family of functions provide access to Opasnet Base -database. Opasnet Base is the database used for storing Opasnet data.
Use the \code{opbase.data} function to read data from the database and the \code{opbase.upload} function to upload data to the database.
Note that uploading data from local R-installation requires Opasnet Base username and password. These can be obtained only by trusted people.

Exclude and include syntax: \code{list = ('<index name 1>' = c('<location value 1>',} 
\code{'<location value 2>',...), '<index name 2>' = c('<location value 1>',} 
\code{'<location value 2>',...), ...)}

Range syntax: \code{list = ('<index name 1>' = c(<range from>|NA, <range to>|NA),} 
\code{'<index name 2>' = c(<range from>|NA, <range to>|NA), ...)}

See also:
\url{http://www.loc.gov/standards/iso639-2/php/code_list.php} \cr
\url{http://en.opasnet.org/}
}
\value{
\item{opbase.data}{Returns \code{data.frame} containing the query result data.}
\item{opbase.locations}{Returns list of locations and their ids (as keys).}
\item{opbase.obj.exists}{Returns TRUE if object exists, FALSE if not.}
\item{opbase.series}{Returns vector of series ids.}
\item{opbase.upload}{Returns total number of data rows uploaded.}
}
\author{
  E. Happonen \email{einari.happonen@thl.fi}
}
\examples{
library(OpasnetUtils)

# Read
opbase.data('op_en1390')
opbase.data('op_en2949', subset='2012', include = list('KUNTA' = 322), 
    range = list('ID_NRO' = c(20000, 30000), 'XKOORD' = c(NA,244000)))

# Write (works only within Opasnet when username nor password is given)
input <- matrix(c('male', 12334435.123, 22, 'female', 234345.23423, 33), 
    ncol=3, byrow=TRUE)
colnames(input) <- c("Sex","Some number","result")
input <- as.data.frame(input)
#res <- opbase.upload(input, ident="op_en1390", name = "Sandbox TEST", 
#    index_types = c('entity','number'), unit = "Age", who='Tester person')
}
\keyword{database}

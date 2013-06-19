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
opbase.data(ident, series_id = NULL, subset = NULL, verbose = FALSE, username = NULL, password = NULL, samples = NULL, exclude = NULL, include = NULL, optim_test = TRUE, ...)
opbase.locations(ident, index_name, series_id = NULL, username = NULL, password = NULL)
opbase.obj.exists(ident, username = NULL, password = NULL)
opbase.series(ident, username = NULL, password = NULL, verbose = FALSE)
opbase.upload(input, ident = NULL, name = NULL, subset = NULL, obj_type = "variable", act_type = "replace", language = "eng", unit = "", who = NULL, rescol = NULL, chunk_size = NULL, verbose = FALSE, username = NULL, password = NULL, index_units = NULL, index_types = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ident}{
%%     ~~Describe \code{ident} here~~
}
  \item{series_id}{
%%     ~~Describe \code{series_id} here~~
}
  \item{index_name}{
Column name (index) whose levels should be returned
}
  \item{subset}{
%%     ~~Describe \code{subset} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{username}{
%%     ~~Describe \code{username} here~~
}
  \item{password}{
%%     ~~Describe \code{password} here~~
}
  \item{samples}{
%%     ~~Describe \code{samples} here~~
}
  \item{exclude}{
%%     ~~Describe \code{exclude} here~~
}
  \item{include}{
%%     ~~Describe \code{include} here~~
}
  \item{optim_test}{
%%     ~~Describe \code{optim_test} here~~
}
  \item{input}{
%%     ~~Describe \code{input} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{obj_type}{
%%     ~~Describe \code{obj_type} here~~
}
  \item{act_type}{
%%     ~~Describe \code{act_type} here~~
}
  \item{language}{
%%     ~~Describe \code{language} here~~
}
  \item{unit}{
%%     ~~Describe \code{unit} here~~
}
  \item{who}{
%%     ~~Describe \code{who} here~~
}
  \item{rescol}{
%%     ~~Describe \code{rescol} here~~
}
  \item{chunk_size}{
%%     ~~Describe \code{chunk_size} here~~
}
  \item{index_units}{
%%     ~~Describe \code{index_units} here~~
}
  \item{index_types}{
%%     ~~Describe \code{index_types} here~~
}
  \item{\dots}{
Excess arguments are ignored
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
\author{
E. Happonen
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#
}

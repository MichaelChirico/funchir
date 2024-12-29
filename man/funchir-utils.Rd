\name{funchir-utils}
\alias{stale_package_check}
\alias{embed.mat}
\alias{quick_year}
\alias{quick_mday}
\alias{quick_yday}
\title{ Miscellaneous utile functions }
\description{
   Several odds-and-ends functions for data manipulation & representation, etc. See details and examples.
}
\usage{
   stale_package_check(con)
   embed.mat(mat, M = nrow(mat), N = ncol(mat), m = 1L, n = 1L, fill = 0L)
   quick_year(dates)
   quick_mday(dates)
   quick_yday(dates)
}
\arguments{
  \item{con}{ A file/connection where output should be written. }
  \item{mat}{ A matrix. }
  \item{M}{ An integer specifying the number of rows in the enclosing matrix. }
  \item{N}{ An integer specifying the number of columns in the enclosing matrix. }
  \item{m}{ An integer specifying the row at which to insert \code{mat}. }
  \item{n}{ An integer specifying the column at which to insert \code{mat}. }
  \item{fill}{ An atomic vector specifying how to fill the enclosing matrix. }
  \item{dates}{ A vector of \code{Date}s. }
}
\value{
  \code{stale_package_check} (DEPRECATED in favor of lintr::unused_import_linter) reads a file (with \code{\link{readLines}}) and checks which functions are actually used from each loaded package. Currently only checks for \code{library} (i.e., not \code{require}) calls.

  \code{embed.mat} inserts a supplied matrix into a (weakly) larger enclosing matrix, typically filled with 0s, at a specified position.

  \code{quick_year} converts a \code{Date} object into its year efficiently; also ignores concerns of leap centuries. \code{quick_mday} returns the day of the month. \code{quick_yday} returns the day of the year. Returns as an \code{integer}.
}
\examples{
  inmat <- matrix(1:9, ncol = 3L)
  embed.mat(inmat, M = 4L, N = 4L)
  embed.mat(inmat, N = 6L, n = 4L, fill = NA)

  d1 = as.Date('1987-05-02')
  d2 = as.Date('2016-02-23')
  quick_year(d1)
  quick_mday(d1)
}

\name{funchir-utils}
\alias{create_quantiles}
\alias{to.pct}
\alias{nx.mlt}
\alias{dol.form}
\alias{ntostr}
\alias{write.packages}
\alias{clean_slate}
\alias{embed.mat}
\alias{get_age}
\alias{quick_year}
\alias{quick_wday}
\alias{quick_mday}
\alias{D}
\title{ Miscellaneous utile functions }
\description{
   Several odds-and-ends functions for data manipulation & representation, etc. See details and examples.
}
\usage{
   create_quantiles(x, num, right = FALSE, include.lowest = TRUE, labels = 1:num)
   to.pct(x, dig = Inf)
   nx.mlt(x, n)
   dol.form(x, dig = 0L, suff = "", tex = FALSE)
   ntostr(n, dig = 2L)
   write.packages(file)
   embed.mat(mat, M = nrow(mat), N = ncol(mat), m = 1L, n = 1L, fill = 0L)
   get_age(birthdays, ref_dates)
   quick_year(dates)
   quick_wday(dates)
   quick_mday(dates)
   D(...)
}
\arguments{
  \item{x}{ A numeric vector. }
  \item{num}{ A number, typically an integer, specifying how many equal-count intervals into which to divide the data.}
  \item{right}{ logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa. }
  \item{include.lowest}{ logical, indicating if an \code{x[i]} equal to the lowest (or highest, for \code{right = FALSE}) \code{breaks} value should be included.}
  \item{labels}{ \code{character} vector of length \code{num}; the labels to be applied to the resulting \code{factor}. }
  \item{dig}{ The number of digits to be included past the decimal in output; sent directly to \code{round}. }
  \item{suff}{ The suffix to appended/unit in which to express \code{x}. Currently one of \code{c("", "k", "m", "b")}, corresponding to plain units, thousands, millions, and billions, respectively. }
  \item{tex}{ Should \code{$} be printed as \code{\\$} for direct copy-pasting to TeX files? }
  \item{n}{ For \code{nx.mlt} and \code{ntostr}, a number; see details. For \code{embed.mat}, an integer specifying the column at which to insert \code{mat}. }
  \item{file}{ A file/connection where output should be written. }
  \item{mat}{ A matrix. }
  \item{M}{ An integer specifying the number of rows in the enclosing matrix. }
  \item{N}{ An integer specifying the number of columns in the enclosing matrix. }
  \item{m}{ An integer specifying the row at which to insert \code{mat}. }
  \item{fill}{ An atomic vector specifying how to fill the enclosing matrix. }
  \item{birthdays}{ A vector of \code{Date}s. }
  \item{ref_dates}{ A vector of \code{Date}s. }
  \item{dates}{ A vector of \code{Date}s. }
  \item{...}{ Items and possibly arguments to (methods of) \code{as.Date}. }
}
\value{
  \code{create_quantiles} is a parsimonious function for generating quantiles of a vector (e.g., quartiles for \code{num=4} or quintiles for \code{num=5}). Basically a wrapper for the \code{cut} function; the type of the output is \code{factor}.

  \code{to.pct} converts a number (probably a proportion, i.e., typically between 0 and 1) to a percentage; also has an argument (\code{dig}) which can be used to round the output inline.

  \code{nx.mlt} returns the least multiple of \code{n} which (weakly) exceeds \code{x}. Convenient for making axes ticks land on pretty numbers.

  \code{dol.form} takes a financial input and converts it to a (American-formatted, American-currency) string for printing--appending a dollar sign (\code{"\$"}) and inserting commas after every third digit from the left of the decimal point.
  
  \code{ntostr} converts \code{n} to a \code{character} vector with each element width \code{dig}. This is particularly nice for converting 99:100 to "99" and "100".
  
  \code{write.packages} captures the current date & time and writes the output to the specified file. This may be essential for tracking across time which package versions were being used.

  \code{embed.mat} inserts a supplied matrix into a (weakly) larger enclosing matrix, typically filled with 0s, at a specified position.
  
  \code{get_age} returns the accurate, fractional age (in years) of each individual, quickly. Accuracy deteriorates when non-leap century years are involved (i.e., any year congruent to 0 mod 100 but not 0 mod 400); designed for use with currently-relevant birthdays and ages.
  
  \code{quick_year} converts a \code{Date} object into its year efficiently; also ignores concerns of leap centuries. \code{quick_wday} returns the day of the week (Sunday is \code{1L} and Saturday is \code{7L}). \code{quick_mday} returns the day of the month. Returns as an \code{integer}.
  
  \code{D} is designed with the simplicity of \code{c} in mind to create vectors of \code{Date}s. 
}

\seealso{ 
  \code{\link{cut}}, \code{\link{prettyNum}}
}
\examples{
  x <- runif(100)

  # Return which multiple of 1/7 least
  #   exceeds each element of x
  create_quantiles(x, 7)

  to.pct(x)
  to.pct(x, dig = 2) #output of the form xxx.xx

  nx.mlt(x, 1/3)

  dol.form(x, dig=2L)
  
  ntostr(999:1000, dig = 3L) # c("999","000")
  ntostr(999:1000, dig = 2L) # c("99","00")
  
  library(stats)
  write.packages(file.path(tempdir(), "test.txt"))
  
  inmat <- matrix(1:9, ncol = 3L)
  embed.mat(inmat, M = 4L, N = 4L)
  embed.mat(inmat, N = 6L, n = 4L, fill = NA)
  
  D("01-01-2016", format = "\%m-\%d-\%Y")
  D("2004-03-02")
  D(1L, 2L, origin = D("1980-01-01"))
  
  get_age(D("1987-05-02"), D("2016-02-23"))
  quick_year(D("1987-05-02"))
  quick_wday(D("1987-05-02"))
  quick_mday(D("1987-05-02"))
}
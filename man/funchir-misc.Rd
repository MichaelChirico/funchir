\name{funchir-misc}
\alias{create_quantiles}
\alias{to.pct}
\alias{nx.mlt}
\alias{dol.form}
\title{ Miscellaneous commonly used functions }
\description{
   Several odds-and-ends functions for data manipulation & representation
}
\usage{
   create_quantiles(x, num, right = FALSE, include.lowest = TRUE)
   to.pct(x, dig = Inf)
   nx.mlt(x, n)
   dol.form(x, dig = 0L)
}
\arguments{
  \item{x}{ A numeric vector. }
  \item{num}{ A number, typically an integer, specifying how many equal-count intervals into which to divide the data.}
  \item{right}{ logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa. }
  \item{include.lowest}{ logical, indicating if an \code{x[i]} equal to the lowest (or highest, for \code{right = FALSE}) \code{breaks} value should be included.}
  \item{dig}{ The number of digits to be included past the decimal in output; sent directly to \code{round}. }
  \item{n}{ A number; see details. }
}
\value{
  \code{create_quantiles} is a parsimonious function for generating quantiles of a vector (e.g., quartiles for \code{num=4} or quintiles for \code{num=5}). Basically a wrapper for the \code{cut} function; the type of the output is \code{factor}.

  \code{to.pct} converts a number (probably a proportion, i.e., typically between 0 and 1) to a percentage; also has an argument (\code{dig}) which can be used to round the output inline.

  \code{nx.mlt} returns the least multiple of \code{n} which (weakly) exceeds \code{x}. Convenient for making axes ticks land on pretty numbers.

  \code{dol.form} takes a financial input and converts it to a (American-formatted, American-currency) string for printing--appending a dollar sign (\code{"\$"}) and inserting commas after every third digit from the left of the decimal point.
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

  nx.mlt(x,1/3)

  dol.form(x, dig=2L)
}
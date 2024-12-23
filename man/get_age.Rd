\name{get_age}
\alias{get_age}
\title{Calculate an exact age in fractional years}
\description{
  For someone born May 1, 1990, what is their age on May 2, 2000? 10 years,
  but what if we want more precision? They are 1 day older, and May 1, 2001
  is in 364 days, so they are 10 + 1/365 years old.

  Things get more complicated when we include consideration of leap years,
  when the next birthday might be 366 days away.

  \code{get_age()} solves this problem.

  Note that it assumes there are no leap centuries (and hence may will be
  incorrect for dates before March 1, 1900 or after February 28, 2100). It
  also takes the stance that leap babies (those born February 29) increment
  their age on March 1 in non-leap years.
}
\usage{
   get_age(birthdays, ref_dates)
}
\arguments{
  \item{birthdays}{ A vector of \code{Date}s (or input coercible with \code{\link{as.Date}()}). Each entry is someone's birthday. }
  \item{ref_dates}{ A vector of \code{Date}s (or input coercible with \code{\link{as.Date}()}). Each entry is a "current date" at which to calculate the corresponding age. }
}
\value{
  Numeric vector of years (including fractional parts) between each \code{ref_dates} and \code{birthdays} entry.
}

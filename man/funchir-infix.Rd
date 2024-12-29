\title{ Convenient Infix Operators }
\name{funchir-infix}
\alias{\%\\\%}
\alias{\%<unescaped bksl>\%}
\description{
   An infix operator as convenient shorthand for set modulation (A\\B)
}
\usage{
   A \%\\\% B
}
\arguments{
  \item{A,B}{ Objects which can be treated as sets. }
}
\value{
  This is just a wrapper for \code{\link{setdiff}}
}
\examples{
  set1 <- 1:5
  set2 <- 4:6

  set1 \%\\\% set2 # c(1,2,3)
}

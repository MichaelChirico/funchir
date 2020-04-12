\title{ Convenient Infix Operators }
\name{funchir-infix}
\alias{\%\\\%}
\alias{\%<unescaped bksl>\%}
\alias{\%u\%}
\alias{\%^\%}
\description{
   Several infix operators which are convenient shorthand for common set operations, namely, \emph{modulation} (A\\B), \emph{union} (AUB) and \emph{intersection} (A & B).
}
\usage{
   A \%\\\% B
   A \%u\% B
   A \%^\% B
}
\arguments{
  \item{A}{ A set \code{A}. }
  \item{B}{ \emph{idem} \code{A}. }
}
\value{
  The above are simply wrappers for the base functions \code{setdiff}, \code{union}, and \code{intersect}, respectively, so output is exactly as for those functions.
}

\seealso{
  \code{\link{setdiff}} , \code{\link{union}}, \code{\link{intersect}}
}
\examples{
  set1 <- 1:5
  set2 <- 4:6

  set1 \%\\\% set2 # c(1,2,3)
  set1 \%u\% set2 # c(1,2,3,4,5,6)
  set1 \%^\% set2 # c(4,5)
}

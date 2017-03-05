\title{ Convenient Infix Operators }
\name{funchir-infix}
\alias{\%+\%}
\alias{\%\\\%}
\alias{\%<unescaped bksl>\%}
\alias{\%u\%}
\alias{\%^\%}
\description{
   Several infix operators which are convenient shorthand for common operations, namely, (paired) string \emph{concatenation} and object naming, set \emph{modulation} (A\\B), set \emph{union} (AUB) and set \emph{intersection} (A & B).
}
\usage{
   s1 \%+\% s2
   A \%\\\% B
   A \%u\% B
   A \%^\% B
}
\arguments{
  \item{s1}{ A string, or anything that can be understood in the first argument of \code{paste0}. }
  \item{s2}{ \emph{idem} \code{s1}. }
  \item{A}{ A set \code{A}. }
  \item{B}{ \emph{idem} \code{A}. }
}
\value{
  The above are simply wrappers for the base functions \code{paste0}, \code{setdiff}, \code{union}, and \code{intersect}, respectively, so output is exactly as for those functions.
}

\seealso{ 
  \code{\link{paste0}}, \code{\link{setdiff}} , \code{\link{union}}, \code{\link{intersect}}
}
\examples{
  directory <- "~/some/directory/"
  fil <- "some_file.ext"
  # very convenient for repeated calls to a directory outside getwd()
  directory \%+\% fil  

  set1 <- 1:5
  set2 <- 4:6

  set1 \%\\\% set2 # c(1,2,3)
  set1 \%u\% set2 # c(1,2,3,4,5,6)
  set1 \%^\% set2 # c(4,5)
}
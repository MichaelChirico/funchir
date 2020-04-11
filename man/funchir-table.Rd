\name{funchir-table}
\alias{sanitize2}
\title{ Convenient Wrappers for creating and printing tables }
\description{
   Here are wrappers for common table creation/manipulation/printing operations.
}
\usage{
   sanitize2(str)
}
\arguments{
  \item{str}{ \code{character} vector. }
}
\details{
  \code{sanitize2} is a replacement to the internal \code{sanitize} function used by default in \code{xtable}. Adds items for fixing left and right square brackets, which are (in the current--2017/03/03--version of \code{print.xtable} ) by default left alone, which can cause errors.
}

\examples{
  sanitize2('$\\\\mathcal{B}$')
}

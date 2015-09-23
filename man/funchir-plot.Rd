\name{funchir-plot}
\alias{pdf2}
\alias{png2}
\alias{dev.off2}
\title{ Simulcast Plot Wrappers }
\description{
   Several wrapper functions which make it convenient / parsimonious to simultaneously write a plot to a file (currently limited to .png and .pdf types) \emph{and} to print the plot to the Rstudio graphics window.
}
\usage{
   pdf2(...)
   png2(...)
   dev.off2(typ = "pdf")
}
\arguments{
  \item{...}{ List of arguments to be passed directly to \code{pdf} or \code{png} }
  \item{typ}{ Either \code{"pdf"} or \code{"png"}, must match the plot type currently open.}
}
\details{
  Instead of the standard \code{pdf("file.pdf") [plot code] dev.off()} approach used to write a plot to file, simply replacing the former and latter with these convenience functions will write the same file, but also print the output in the RStudio plotting window.
}

\seealso{ 
  \code{\link{pdf}}, \code{\link{png}} , \code{\link{dev.off}}
}
\examples{
  smple <- rnorm(100)
  pdf2("test.pdf")
  hist(smpl)
  dev.off2()

  png2("test.png")
  hist(smpl)
  dev.off2(typ="png")
}
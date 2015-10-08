\name{funchir-plot}
\alias{pdf2}
\alias{png2}
\alias{dev.off2}
\alias{tile.axes}
\title{ Simulcast Plot Wrappers }
\description{
   Several wrapper functions which make it convenient / parsimonious to simultaneously write a plot to a file (currently limited to .png and .pdf types) \emph{and} to print the plot to the Rstudio graphics window.
}
\usage{
   pdf2(pin = c(4,3), ...)
   png2(...)
   dev.off2(typ = "pdf")
   tile.axes(n, M, N, ...)
}
\arguments{
  \item{pin}{ Set plot dimensions (passed directly to \code{par("pin")}) since current default is to use the RStudio window size which is in constant flux for my workflow; default is to set a 4" x 3" window. }
  \item{...}{ For \code{pdf2}, \code{png2}: list of arguments to be passed directly to \code{pdf} or \code{png}. For \code{tile.axes}, arguments to be passed to \code{axis}. }
  \item{typ}{ Either \code{"pdf"} or \code{"png"}, must match the plot type currently open.}
  \item{n}{ Integer. Cell in \code{mfrow} to which to apply the axes; fills by \emph{row}, following base functionality.}
  \item{M}{ Integer. Number of rows specified in \code{mfrow}. }
  \item{N}{ Integer. Number of columns specified in \code{mfrow}. }
}
\details{
  Instead of the standard \code{pdf("file.pdf") [plot code] dev.off()} approach used to write a plot to file, simply replacing the former and latter with these convenience functions will write the same file, but also print the output in the RStudio plotting window.

  \code{tile.axes} provides a simple way to incorporate the plotting of axes into a loop which creates the plots in a matrix of plots (e.g., by using \code{par(mfrow=c(2, 2))}) \emph{when the axes are shared by all plots}. x axes are only printed on the bottom row of plots, and y axes are only printed on the first column of plots--this saves potentially wasted / white space by eliminating redundant axes, yet can still be done in a loop.
}

\seealso{ 
  \code{\link{pdf}}, \code{\link{png}} , \code{\link{dev.off}}
}
\examples{
  smpl <- rnorm(100)
  
  \dontrun{
    #Runs in RStudio
    pdf2() 
    hist(smpl)
    dev.off2()

    png2()
    hist(smpl)
    dev.off2(typ="png")
  }

  par(mfrow = c(2, 1), mar = c(0, 0, 0, 0), oma=c(5, 4, 4, 2) + .1)
  for (ii in 1:2){
    hist(smpl[sample(length(smpl), 100, rep = TRUE)], xaxt = "n", yaxt = "n")
    tile.axes(ii, 2, 1)
  }
}
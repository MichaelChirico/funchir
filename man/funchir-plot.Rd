\name{funchir-plot}
\alias{pdf2}
\alias{png2}
\alias{dev.off2}
\alias{tile.axes}
\alias{rel_coord}
\title{ Simulcast Plot Wrappers }
\description{
   Several wrapper functions which make it convenient / parsimonious to simultaneously write a plot to a file (currently limited to .png and .pdf types) \emph{and} to print the plot to the Rstudio graphics window.
}
\usage{
   pdf2(...)
   png2(...)
   dev.off2(typ = "pdf")
   tile.axes(n, M, N, params = list(x = list(), y = list()),
             use.x = TRUE, use.y = TRUE)
   rel_coord(ax, lambda = 0)
}
\arguments{
  \item{...}{ For \code{pdf2}, \code{png2}: list of arguments to be passed directly to \code{pdf} or \code{png}. }
  \item{typ}{ Either \code{"pdf"} or \code{"png"}, must match the plot type currently open.}
  \item{n}{ Integer. Cell in \code{mfrow} to which to apply the axes; fills by \emph{row}, following base functionality.}
  \item{M}{ Integer. Number of rows specified in \code{mfrow}. }
  \item{N}{ Integer. Number of columns specified in \code{mfrow}. }
  \item{ax}{ Either \code{"x"} or \code{"y"}; axis for which we're specifying coordinates. }
  \item{lambda}{ Either a mixing weight, giving the percentage of the way between the lower and higher endpoint of the specified axis (0 : smaller endpoint, 1 : larger endpoint), or one of the following relative position characters: \code{c("top", "bottom", "left", "right")}. }
  \item{params}{ A length-2 \code{list}. \code{params$x} is a \code{list} of parameters to be passed to the x-axis. \code{params$y} is a \code{list} of parameters to be passed to the y-axis. }
  \item{use.x}{ \code{logical}. Should the x-axis be printed? }
  \item{use.y}{ \code{logical}. Should the y-axis be printed? }
}
\details{
  Instead of the standard \code{pdf("file.pdf") [plot code] dev.off()} approach used to write a plot to file, simply replacing the former and latter with these convenience functions will write the same file, but also print the output in the RStudio plotting window.

  \code{tile.axes} provides a simple way to incorporate the plotting of axes into a loop which creates the plots in a matrix of plots (e.g., by using \code{par(mfrow=c(2, 2))}) \emph{when the axes are shared by all plots}. x axes are only printed on the bottom row of plots, and y axes are only printed on the first column of plots--this saves potentially wasted / white space by eliminating redundant axes, yet can still be done in a loop.


  \code{rel_coord} gives a simple way to place text, labels, etc. in a plot by specifying only relative coordinates. The first form mimics the flexibility of \code{\link{legend}}, which accepts such relatively-defined positions as \code{"top"} and \code{"left"}. For example, specifying \code{lambda = "right"} will return an x coordinate close to the right endpoint of the currently defined axes. Alternatively, an exact proportion can be supplied -- if the desired coordinate is 75\% of the way to the right endpoint, for example, specify \code{rel_coord("x", .75)}.
  
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
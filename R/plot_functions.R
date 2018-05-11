# Plotting Convenience Functions ####
## Multiplot axis generator
##   Plot #n in an MxN grid
tile.axes <- function(n, M, N, params = list(x = list(), y = list()),
                      use.x = TRUE, use.y = TRUE){
  #only print x axes on the last row
  if ((n > (M - 1) * N | M == 1) && use.x) do.call("axis", c(side = 1, params$x))
  #only print y axes on the first column
  if ((n %% N == 1 | N == 1) && use.y) do.call("axis", c(side = 2, params$y))
}

## Methods for getting relative position in axes
##  **TODO: more sophistication with handling text input,
#   **perhaps ideally mimicking 'legend'
rel_coord <- function(ax, lambda = 0){
  lmc <- is.character(lambda)
  lamn <- if (lmc){if (lambda %in% c("right", "top")) .95 else .05} else lambda
  usr <- par("usr")
  if (ax == "x"){
    lims <- usr[1:2]
    if (lmc & !lambda %in% c("left", "right"))
        stop('Valid text arguments for `lambda` are "left" and "right" on the x axis')
  } else if (ax == "y"){
    lims <- usr[3:4]
    if (lmc & !lambda %in% c("top", "bottom"))
        stop('Valid text arguments for `lambda` are "top" and "bottom" on the y axis')
  }
  sum(lims * c(1 - lamn, lamn))
}

# Functions to invert graphics::{x,y,xy}inch --
#   Convert from graphics device units
#   into inches (in particular useful for
#   specifying length argument in arrows); inspiration:
#   https://stackoverflow.com/questions/47034898
xdev2in = function(x = 1) {
  x * par('pin')[1L]/diff(par('usr')[1L:2L])
}

ydev2in = function(y = 1) {
  y * par('pin')[2L]/diff(par('usr')[3L:4L])
}

xydev2in = function(xy = 1) {
  u = par('usr')
  xy * par('pin')/c(u[2L] - u[1L], u[4L] - u[3L])
}

# Convenient Infix Operators ####
## Concatenation shorthand
"%+%" <- function(s1, s2) paste0(s1, s2)

## See discussion here for why this exists
## http://stackoverflow.com/questions/32748895/
"%<unescaped bksl>%" <- function(){
  cat("What are you thinking? Don't use this function. See ?\"%\\%\"")
}

## Set operations shorthand
### Set difference
###  *Note: only need one backslash for use
"%\\%" <- function(A, B) setdiff(A, B)

### Set union
"%u%" <- function(A, B) union(A, B)

### Set intersection
"%^%" <- function(A, B) intersect(A, B)

# Plotting Convenience Functions ####
## Implicit simulcast: Write plot to specified
##   file while also printing the output to
##   the RStudio plotting window
pdf2 <- function(...){
  graphics.off()
  dev.new()
  do.call('pdf', list(...))
  invisible(dev.set(which = dev.list()["RStudioGD"]))
}

png2 <- function(...){
  graphics.off()
  dev.new()
  do.call('png', list(...))
  invisible(dev.set(which = dev.list()["RStudioGD"]))
}

dev.off2 <- function(typ = "pdf"){
  #dev.new() creates an RStudio device AND a png device.
  #  So there are multiple png devices when we've
  #  created our own png device. As such we have to
  #  take care to select the most recent png device.
  #  Inspection of the object .Devices after running
  #  graphics.off(); dev.new(); png("test.png")
  #  shows that this is probably the right approach.
  #  SOLVES #2: https://github.com/MichaelChirico/funchir/issues/2
  idx <- which(names(dl <- dev.list()) == typ)
  num <- dl[idx[length(idx)]]
  dev.copy(which = num)
  invisible(dev.off(which = num))
}

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

# Table & regression convenience functions ####
## Extending base table with some common options
table2<-function(..., dig = if (prop) 2L else NULL,
                 prop = FALSE, ord = FALSE, pct = FALSE){
  if (ord == "dec"){ dec <- TRUE; ord <- TRUE} else dec <- FALSE
  dots <- list(...)
  args <- names(dots) %in% names(formals(prop.table))
  tab <- if (prop) {
    do.call('prop.table',c(list(
      do.call('table', if (length(args)) dots[!args] else dots)),
      dots[args])) 
  } else do.call('table', list(...))
  if (ord) tab<-tab[order(tab, decreasing = dec)]
  if (pct) tab <- 100 * tab
  if (is.null(dig)) tab else round(tab, digits = dig)
}

#For pretty copy-pasting into LyX
lyx.xtable <- function(...){
  cat(capture.output(do.call('print.xtable', list(...))), sep = "\n\n")
}

sanitize2 <- function(str) {
  result <- str
  result <- gsub("\\\\", "SANITIZE.BACKSLASH", result)
  result <- gsub("$", "\\$", result, fixed = TRUE)
  result <- gsub(">", "$>$", result, fixed = TRUE)
  result <- gsub("<", "$<$", result, fixed = TRUE)
  result <- gsub("|", "$|$", result, fixed = TRUE)
  result <- gsub("{", "\\{", result, fixed = TRUE)
  result <- gsub("}", "\\}", result, fixed = TRUE)
  result <- gsub("%", "\\%", result, fixed = TRUE)
  result <- gsub("&", "\\&", result, fixed = TRUE)
  result <- gsub("_", "\\_", result, fixed = TRUE)
  result <- gsub("#", "\\#", result, fixed = TRUE)
  result <- gsub("[", "\\lbrack", result, fixed = TRUE)
  result <- gsub("]", "\\rbrack", result, fixed = TRUE)
  result <- gsub("^", "\\verb|^|", result, fixed = TRUE)
  result <- gsub("~", "\\~{}", result, fixed = TRUE)
  result <- gsub("SANITIZE.BACKSLASH", "$\\backslash$", 
                 result, fixed = TRUE)
  return(result)
}

# Data reading convenience functions ####
## read.xlsx2 wrapper to invisibly handle
##   the memory leak problem of read.xlsx2
##   by automatically flushing memory each call
read.xlsx3 <- function(...){
  x <- do.call('read.xlsx2', list(...))
  invisible(gc())
  x
}

## Shorthand for colClasses arguments,
##   particularly useful if the number of fields
##   starts to mushroom considerably
abbr_to_colClass <- function(inits, counts){
  x <- strsplit(inits, split = "")[[1L]]
  lookup <- c(b = "blank", c = "character", f = "factor", 
              l = "logical", i = "integer", n = "numeric",
              #For readxl (Hadley)'s odd read_excel options
              D = "Date", t = "text", d = "date")
  rep(unname(lookup[x]), strsplit(counts, split = "")[[1L]])
}

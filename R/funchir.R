# Convenient Infix Operators ####
## Concatenation shorthand
"%+%" <- function(s1, s2) paste0(s1, s2)

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
  dev.set(which = dev.list()["RStudioGD"])
}

png2 <- function(...){
  graphics.off()
  dev.new()
  do.call('png', list(...))
  dev.set(which = dev.list()["RStudioGD"])
}

dev.off2 <- function(typ = "pdf"){
  num <- dev.list()[typ]
  dev.copy(which = num)
  invisible(dev.off(which = num))
}

## Multiplot axis generator
##   Plot #n in an MxN grid
tile.axes <- function(n, M, N, ...){
  #only print x axes on the last row
  if (n > (M - 1) * N | M == 1) do.call("axis", c(side = 1, list(...)))
  #only print y axes on the first column
  if (n %% N == 1 | N == 1) do.call("axis", c(side = 2, list(...)))
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

#For pretty copy-pasting into Lyx
lyx.texreg <- function(...){
  cat(capture.output(do.call('texreg', list(...))), sep = "\n\n")
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
  types <- character(length(x))
  types[x == "c"] <- "character"
  types[x == "f"] <- "factor"
  types[x == "i"] <- "integer"
  types[x == "n"] <- "numeric"
  types[x == "d"] <- "Date"
  rep(types, strsplit(counts, split = "")[[1L]])
}

# Miscellaneous functions ####
## Specific wrapper of cut to create a factor of quantiles of a vector
create_quantiles <- function(x, num, right = FALSE, include.lowest = TRUE){
  cut(x, breaks = quantile(x, probs = seq(0, 1, by = 1 / num)),
      labels = 1:num, right = right, include.lowest = include.lowest)
}

## Inline conversion to percentage
to.pct <- function(x, dig = Inf) round(100 * x, digits = dig)

## Get the nearest multiple of n weakly larger than x
nx.mlt <- function(x, n) n * ceiling(x / n)

## Convert numbers to strings OF SPECIFIED LENGTH
##   Convenient for getting c("99","00") from 99:100
ntostr <- function(n, dig = 2L){
  #TODO: speed this up significantly
  paste0(ifelse(log10(n) < dig - 1,
                substr(n + 10^dig, 2L, dig + 1L),
                ifelse(log10(n) >= dig,
                       substr(n, nchar(n) - dig + 1L, nchar(n)),n)))
}

## Convert numbers for printing to dollar format
dol.form <- function(x, dig = 0L){
  "$" %+% prettyNum(round(x, digits = dig), big.mark = ",")
}

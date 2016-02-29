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

### Left-to-right naming
"%=%" <- function(nm, obj){names(obj) <- nm; obj}

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
tile.axes <- function(n, M, N, params = list(list(),list()), 
                      use.x = TRUE, use.y = TRUE){
  #only print x axes on the last row
  if ((n > (M - 1) * N | M == 1) & use.x) do.call("axis", c(side = 1, params[[1]]))
  #only print y axes on the first column
  if ((n %% N == 1 | N == 1) & use.y) do.call("axis", c(side = 2, params[[2]]))
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
  lookup <- c(c = "character", f = "factor",
              i = "integer", n = "numeric", D = "Date", 
              #For readxl (Hadley)'s odd read_excel options
              t = "text", d = "date")
  rep(unname(lookup[x]), strsplit(counts, split = "")[[1L]])
}

# Miscellaneous functions ####
## Specific wrapper of cut to create a factor of quantiles of a vector
create_quantiles <- function(x, num, right = FALSE,
                             include.lowest = TRUE, labels = 1:num){
  cut(x, breaks = quantile(x, probs = (1 / num) * 0:num),
      labels = labels, right = right, include.lowest = include.lowest)
}

## Inline conversion to percentage
to.pct <- function(x, dig = Inf) round(100 * x, digits = dig)

## Get the nearest multiple of n weakly larger than x
nx.mlt <- function(x, n) n * ceiling(x / n)

## Convert numbers to strings OF SPECIFIED LENGTH
##   Convenient for getting c("99","00") from 99:100
ntostr <- function(n, dig = 2L){
  sprintf("%0" %+% dig %+% "d", x %% 10^dig)
}

## Convert numbers for printing to dollar format
dol.form <- function(x, dig = 0L){
  "$" %+% prettyNum(round(x, digits = dig), big.mark = ",")
}

## Write the output of sessionInfo() & the date to a file
##   (for tracking package versions over time)
write.packages <- function(file) {
  x<-
    capture.output({
      cat("Package info for code run on " %+% Sys.time() %+% ":\n")
      sessionInfo()})
  writeLines(x, con = file)
}

## Embed the matrix mat in a larger matrix by
##   placing the top-left element of mat at the supplied
##   position (m,n).
embed.mat <- function(mat, M = nrow(mat), N = ncol(mat), 
                      m = 1L, n = 1L, fill = 0L) {
  if (m > M || n > N) 
    stop("Supplied starting position outside supplied enclosing matrix bounds")
  if ((end1 <- m + nrow(mat) - 1L) > M || 
      (end2 <- n + ncol(mat) - 1L) > N){
    stop("Supplied matrix too large for supplied enclosing matrix")
  }
  out <- matrix(fill, nrow = M, ncol = N)
  out[m:end1, n:end2] <- mat
  out
}

## Accurately calculate fractional age, quickly
get_age <- function(birthdays, ref_dates){
  x <- data.table(bday <- unclass(birthdays),
                  rem = ((ref <- unclass(ref_dates)) - bday) %% 1461)
  x[ , cycle_type := 
       foverlaps(data.table(start = bdr <- bday %% 1461, end = bdr),
                 data.table(start = c(0, 59, 424, 790, 1155), 
                            end = c(58, 423, 789, 1154, 1460), 
                            val = c(3L, 2L, 1L, 4L, 3L),
                            key = "start,end"))$val]
  I4 <- diag(4)[ , -4]
  x[ , extra := 
       foverlaps(data.table(start = rem, end = rem),
                 data.table(start = st <- cumsum(c(0, rep(365, 3) +
                                                     I4[.BY[[1]],])),
                            end = c(st[-1L] - 1L, 1461),
                            int_yrs = c(0, 1, 2, 3), key = "start,end")
       )[ , int_yrs + (i.start - start) / (end + 1L - start)], by = cycle_type]
  4 * ((ref - bday) %/% 1461) + x$extra
}

## Quick conversion of a code snippet into
##   a form copy-paste-able on Stack Overflow
SOprint <- function(exp, drop.ref = TRUE){
  #note that `deparse(substitute(x))` will pre-evaluate
  #  certain expressions, e.g., turning 'x := y'
  #  from 'data.table' into `:=`(x, y). This regex
  #  reverses this (unless we don't want it to)
  if (grepl("`:=`", dsx <- deparse(substitute(exp)), fixed = TRUE) && drop.ref)
    dsx <- gsub("`:=`\\(([^,]*)\\,([^)]*)\\)", "\\1 := \\2", dsx)
  cat(dsx, capture.output(exp), sep = "\n# ")
}

## Tired of over-using as.Date everywhere...
D <- function(...){
  if (is.null(names(dl <- list(...)))) 
    return(do.call("as.Date", list(do.call("c", dl))))
  do.call("as.Date", c(list(do.call("c", dl[nm <- names(dl) == ""])), dl[!nm]))
}

## devtools has too many dependencies. Use Jan Tilly's alternative
install_github <- function(repo, branch = "master", dependencies = TRUE, method = "auto"){
  source("http://jtilly.io/install_github/install_github.R", local = TRUE)
  install_github(repo, branch, dependencies, method)
}
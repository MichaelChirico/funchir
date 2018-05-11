## Shorthand for colClasses arguments,
##   particularly useful if the number of fields
##   starts to mushroom considerably
abbr_to_colClass <- function(inits, counts){
  x <- strsplit(inits, split = "")[[1L]]
  lookup <- c(b = "blank", c = "character", f = "factor",
              l = "logical", i = "integer", n = "numeric",
              #For readxl (Hadley)'s odd read_excel options
              D = "Date", t = "text", d = "date", s = 'skip')
  rep(unname(lookup[x]), strsplit(counts, split = "")[[1L]])
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

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

sanitize2 <- function(str) {
  result <- str
  result <- gsub("\\\\", "SANITIZE.BACKSLASH", result, fixed = TRUE)
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

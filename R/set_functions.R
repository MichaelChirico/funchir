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

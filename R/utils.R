# Embed the matrix mat in a larger matrix by
#   placing the top-left element of mat at the supplied
#   position (m,n).
embed.mat <- function(mat,
                      M = nrow(mat), N = ncol(mat),
                      m = 1L, n = 1L, fill = 0L) {
  if (m > M || n > N)
    stop("Supplied starting position outside supplied enclosing matrix bounds")
  end1 <- m + nrow(mat) - 1L
  end2 <- n + ncol(mat) - 1L
  if (end1 > M || end2 > N) {
    stop("Supplied matrix too large for supplied enclosing matrix")
  }
  out <- matrix(fill, nrow = M, ncol = N)
  out[m:end1, n:end2] <- mat
  out
}

stale_package_check = function(con) stop("This function is deprecated. Use lintr::unused_import_linter() instead")

# Accurately calculate fractional age, quickly
get_age <- function(birthdays, ref_dates) {
  if (length(birthdays) != length(ref_dates)) {
    if (length(birthdays) == 1L) {
      birthdays = rep(birthdays, length(ref_dates))
    } else if (length(ref_dates) == 1L) {
      ref_dates = rep(ref_dates, length(birthdays))
    } else {
      stop(sprintf(
        "'birthdays' and 'ref_dates' must have equal length or one be a scalar, but got respective lengths %d and %d",
        length(birthdays), length(ref_dates)
      ))
    }
  }
  if (!length(birthdays)) return(numeric())

  if (!inherits(birthdays, "Date")) birthdays = as.Date(birthdays)
  if (!inherits(ref_dates, "Date")) ref_dates = as.Date(ref_dates)

  # NB: Strips fractional day parts
  birthdays_unix <- as.integer(birthdays)
  # offset by 790 days to center around the Mar. 1 following a Feb. 29.
  #   (.Date(790) is Mar. 1, 1972, i.e. a leap year)
  days_after_feb29 <- (birthdays_unix - 790L) %% 1461L
  ref_dates_unix <- as.integer(ref_dates)
  # days in current quadrennium _of life_, i.e. relative quadrennial date
  rem = (ref_dates_unix - birthdays_unix) %% 1461L
  # Use double-fcase() for #18, though earlier this used double-foverlaps()
  #   and could also easily use double-findInterval().
  # nolint start: indentation_linter, spaces_left_parentheses_linter.
  extra_part = fcase(
    days_after_feb29 < 365L,
      fcase(rem < 365L,  0.0+(rem-   0.0)/365.0,
            rem < 730L,  1.0+(rem- 365.0)/365.0,
            rem < 1095L, 2.0+(rem- 730.0)/365.0,
            rem < 1461L, 3.0+(rem-1095.0)/366.0),
    days_after_feb29 < 730L,
      fcase(rem < 365L,  0.0+(rem-   0.0)/365.0,
            rem < 730L,  1.0+(rem- 365.0)/365.0,
            rem < 1096L, 2.0+(rem- 730.0)/366.0,
            rem < 1461L, 3.0+(rem-1096.0)/365.0),
    days_after_feb29 < 1095L,
      fcase(rem < 365L,  0.0+(rem-   0.0)/365.0,
            rem < 731L,  1.0+(rem- 365.0)/366.0,
            rem < 1096L, 2.0+(rem- 731.0)/365.0,
            rem < 1461L, 3.0+(rem-1096.0)/365.0),
    days_after_feb29 < 1461L, # TODO: use default=. Just let vector default bake longer.
      fcase(rem < 366L,  0.0+(rem-   0.0)/366.0,
            rem < 731L,  1.0+(rem- 366.0)/365.0,
            rem < 1096L, 2.0+(rem- 731.0)/365.0,
            rem < 1461L, 3.0+(rem-1096.0)/365.0)
  )
  # nolint end.
  4.0 * ((ref_dates_unix - birthdays_unix) %/% 1461.0) + extra_part
}

# Quickly get the year of a date
cum_days_by_year = c(0L, 365L, 730L, 1096L, 1461L)
quick_year = function(dates) {
  quadrennia = as.integer(unclass(dates) %/% 1461L)
  day_in_quad = unclass(dates) %% 1461L
  rem_yrs = findInterval(day_in_quad, cum_days_by_year) - 1L
  1970L + 4L * quadrennia + rem_yrs
}

quick_yday = function(dates) {
  day_in_quad = as.integer(dates) %% 1461L
  day_in_quad - cum_days_by_year[findInterval(day_in_quad, cum_days_by_year)] + 1L
}

#Month days in the quadrennial cycle
.mday1461__ = c(
  #  Jan     Feb     Mar     Apr     May     Jun     Jul     Aug     Sep     Oct     Nov     Dec
  1L:31L, 1L:28L, 1L:31L, 1L:30L, 1L:31L, 1L:30L, 1L:31L, 1L:31L, 1L:30L, 1L:31L, 1L:30L, 1L:31L,
  1L:31L, 1L:28L, 1L:31L, 1L:30L, 1L:31L, 1L:30L, 1L:31L, 1L:31L, 1L:30L, 1L:31L, 1L:30L, 1L:31L,
  1L:31L, 1L:29L, 1L:31L, 1L:30L, 1L:31L, 1L:30L, 1L:31L, 1L:31L, 1L:30L, 1L:31L, 1L:30L, 1L:31L,
  1L:31L, 1L:28L, 1L:31L, 1L:30L, 1L:31L, 1L:30L, 1L:31L, 1L:31L, 1L:30L, 1L:31L, 1L:30L, 1L:31L
)

quick_mday = function(dates) .mday1461__[1L + unclass(dates) %% 1461L]

## See discussion here for why this exists
## http://stackoverflow.com/questions/32748895/
`%<unescaped bksl>%` <- function() stop('What are you thinking? Don\'t use this function. See ?"%\\%"') # nocov


## Set operations shorthand
### Set difference
###  *Note: only need one backslash for use
`%\\%` <- function(A, B) setdiff(A, B)

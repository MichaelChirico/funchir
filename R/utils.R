# Specific wrapper of cut to create a factor of quantiles of a vector
create_quantiles <- function(x, num, right = FALSE, na.rm = FALSE,
                             include.lowest = TRUE, labels = 1:num){
  uniq_Qs = unique(quantile(x, probs = 0:num/num, na.rm = na.rm))
  if (length(uniq_Qs) - 1L != length(labels) && !is.null(labels)) {
    stop(
      sprintf(
        ngettext(
          length(uniq_Qs) - 1L, domain="R-funchir",
          'Overlapping quantiles. Please provide %d label.',
          'Overlapping quantiles. Please provide %d labels.'
        ),
        length(uniq_Qs) - 1L
      ),
      domain=NULL
    )
  }
  cut(x, breaks = uniq_Qs, labels = labels, right = right,
      include.lowest = include.lowest)
}

# Inline conversion to percentage
to.pct <- function(x, dig = Inf) round(100 * x, digits = dig)

# Get the nearest multiple of n weakly larger than x
nx.mlt <- function(x, n) n * ceiling(x / n)

# Create a linear progression along the range of x with n points
divide = function(x, n, na.rm = FALSE) {
  r = range(x, na.rm = na.rm)
  seq(r[1L], r[2L], length.out = n)
}

# Convert numbers for printing to dollar format
dol.form <- function(x, dig = 0L, suff = "", tex = FALSE){
  neg <- rep("", length(x))
  neg[x < 0] <- "-"
  div <- c(1, "k"=1e3, "m"=1e6, "b"=1e9)
  idx <- which(names(div) == suff)
  paste0(neg, if (tex) "\\", "$",
         prettyNum(round(abs(x)/div[idx], digits = dig),
                   big.mark = ",", scientific = FALSE), suff)
}

# Convert numbers to strings OF SPECIFIED LENGTH
#   Convenient for getting c("99","00") from 99:100
ntostr <- function(n, dig = 2L){
  sprintf(sprintf("%%0%dd", dig), n %% 10L^dig)
}

# Condensed cleaning of work space at
#   outset of R script
# **has some errors when applied to more general setups**
# clean_slate = function(detach.packages = TRUE, gc = TRUE,
#                        all = TRUE, envir = .GlobalEnv) {
#   rm(list = ls(envir = envir, all.names = all), envir = envir)
#   if (gc) gc(verbose = FALSE)
#   if (detach.packages) {
#     base.pkg = "package:" %+% c("stats", "graphics", "grDevices",
#                                 "utils", "datasets", "methods", "base")
#     invisible(lapply(setdiff(grep("^package:",
#                                   search(), value = TRUE), base.pkg),
#                      detach, character.only = TRUE, unload = TRUE))
#   }
# }

# Write the output of sessionInfo() & the date to a file
#   (for tracking package versions over time)
write.packages <- function(con = stdout()) {
  # nocov start
  if (!requireNamespace('jsonlite', quietly = TRUE)) {
    stop('jsonlite is required for this functionality, please install first')
  }
  # nocov end
  si = sessionInfo()
  desc_fields = c('Version', 'Depends', 'Imports', 'Suggests',
                  'License', 'URL', 'Packaged', 'Built')
  desc_pad = function(desc) `names<-`(desc[desc_fields], desc_fields)
  locale_list = function() {
    # query these individually given proviso in ?Sys.getlocale:
    # > For portability, it is best to query categories individually...
    # this list of categories taken from the output of my Linux Mint machine
    lc_names = c('LC_CTYPE', 'LC_NUMERIC', 'LC_TIME', 'LC_COLLATE',
                 'LC_MONETARY', 'LC_MESSAGES', 'LC_PAPER',
                 'LC_NAME', 'LC_ADDRESS', 'LC_TELEPHONE',
                 'LC_MEASUREMENT', 'LC_IDENTIFICATION')
    as.list(
      sapply(lc_names, function(x) {
        tryCatch(Sys.getlocale(x), error = function(e) {
          if (e$message == "invalid 'category' argument") '' else stop(e$message) # nocov
        })
      })
    )
  }
  out = list(
    r_version = list(platform = si$platform,
                     version.string = si$R.version$version.string),
    locale = locale_list(),
    running = si$running,
    linear_algebra = list(matrix_products = si$matprod,
                          blas = si$BLAS, lapack= si$LAPACK),
    base_packages = si$basePkgs,
    other_packages = lapply(si$otherPkgs, desc_pad),
    loaded_via_namespace = lapply(si$loadedOnly, desc_pad),
    write_package_time = format(Sys.time(), tz = 'UTC', usetz = TRUE)
  )
  writeLines(jsonlite::toJSON(out, pretty = TRUE, auto_unbox = TRUE), con = con)
  return(invisible(out))
}

# Embed the matrix mat in a larger matrix by
#   placing the top-left element of mat at the supplied
#   position (m,n).
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

parse_library_calls = function(e) {
  if (is.call(e)) {
    if (e[[1L]] == 'library' || e[[1L]] == 'require') {
      return(e[[2L]])
    } else return(lapply(e[-1L], parse_library_calls))
  }
  return(NULL)
}
# SKIP base::sample namespace-accessed calls -- if always using ::, it's
#   not necessary to run library()
# TODO: maybe it's there just to signal what will be used though?
get_all_plain_calls = function(e) {
  if (is.call(e) && is.name(e[[1L]]))
    return(c(e[[1L]], lapply(e[-1L], get_all_plain_calls)))
}

# Quick scan of code for whether the
#   packages loaded are actually used
#' @param con A connection
stale_package_check = function(con) {
  code = tryCatch(parse(con), error = identity)
  if (inherits(code, 'error')) {
    cat('Failed to parse R script, please fix syntax errors first\n')
    return(invisible())
  }

  all_packages = unique(as.character(unlist(lapply(code, parse_library_calls))))
  if (!length(all_packages)) {
    cat('No library() or require() calls found\n')
    return(invisible())
  }

  # e.g. := from data.table comes out `:=` but := from getNamespaceExports
  all_plain_calls = setdiff(
    gsub(
      pattern = '`', replacement = '', fixed = TRUE,
      unique(as.character(unlist(lapply(code, get_all_plain_calls))))
    ),
    c('library', 'require')
  )

  for (pkg in all_packages) {
    fns = getNamespaceExports(pkg)

    used = fns %in% all_plain_calls
    if (any(used))
      cat('Functions matched from package ', pkg, ':\n\t',
          paste(fns[used], collapse = ', '), '\n', sep = '')
    else cat('**No exported functions matched from ', pkg, '**\n', sep = '')
  }
  return(invisible())
}

# Accurately calculate fractional age, quickly
## R CMD check appeasement
cycle_type = rem = int_yrs = i.start = start = end = NULL

get_age <- function(birthdays, ref_dates) {
  x <- data.table(
    bday <- unclass(birthdays),
    rem = ((ref <- unclass(ref_dates)) - bday) %% 1461L
  )
  x[ , 'cycle_type' := {
    overlaps = foverlaps(
      data.table(start = bdr <- bday %% 1461L, end = bdr),
      data.table(
        start = c(0L, 59L, 424L, 790L, 1155L),
        end = c(58L, 423L, 789L, 1154L, 1460L),
        val = c(3L, 2L, 1L, 4L, 3L),
        key = c('start', 'end')
      )
    )
    overlaps$val
  }]
  I4 <- diag(4L)[ , -4L]
  x[ , by = cycle_type, 'extra' := {
    overlaps = foverlaps(
      data.table(start = rem, end = rem),
      data.table(
        start = st <- cumsum(c(0L, rep(365L, 3L) + I4[.BY[[1L]], ])),
        end = c(st[-1L] - 1L, 1461L),
        int_yrs = 0:3,
        key = c('start', 'end')
      )
    )
    overlaps[ , int_yrs + (i.start - start) / (end + 1L - start)]
  }]
  4 * ((ref - bday) %/% 1461) + x$extra
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

quick_mday = function(dates)
  .mday1461__[1L + unclass(dates) %% 1461L]

## See discussion here for why this exists
## http://stackoverflow.com/questions/32748895/
"%<unescaped bksl>%" <- function() stop("What are you thinking? Don't use this function. See ?\"%\\%\"") # nocov


## Set operations shorthand
### Set difference
###  *Note: only need one backslash for use
"%\\%" <- function(A, B) setdiff(A, B)

### Set union
"%u%" <- function(A, B) union(A, B)

### Set intersection
"%^%" <- function(A, B) intersect(A, B)

test_that('stale_package_check works', {
  stale_package_path <- function(path) test_path('stale_package_test_scripts', path)

  expect_output(
    stale_package_check(stale_package_path('simple.R')),
    paste(
      c("Functions matched from package stats:",
        paste0("\t", toString(sort(c("density", "rnorm")))),
        "Functions matched from package tools:",
        "\tfile_ext",
        "**No exported functions matched from parallel**"),
      collapse = '\n'
    ),
    fixed = TRUE
  )

  expect_output(
    stale_package_check(stale_package_path('use_namespace_call.R')),
    '**No exported functions matched from stats**',
    fixed = TRUE
  )

  expect_output(
    stale_package_check(stale_package_path('wont_parse.R')),
    'Failed to parse R script, please fix syntax errors first',
    fixed = TRUE
  )

  expect_output(
    stale_package_check(stale_package_path('no_library.R')),
    'No library() or require() calls found',
    fixed = TRUE
  )
})

test_that('one-line utilities work', {
  expect_identical(to.pct(0.8, 2.0), 80.0)
  expect_identical(to.pct(0.8030432, 3.0), 80.304)

  expect_identical(nx.mlt(3.0, 5.0), 5.0)
  expect_identical(nx.mlt(24.0, 17.0), 34.0)

  expect_identical(divide(c(1.0, 4.0, 8.0, 9.0, 11.0, 2.0, 2.0), 3L), c(1.0, 6.0, 11.0))

  expect_identical(dol.form(1.0e6), '$1,000,000')
  expect_identical(dol.form(1.0e6, suff='m'), '$1m')
  expect_identical(dol.form(-1.0e6), '-$1,000,000')
  expect_identical(dol.form(123.456, dig = 0L), '$123')
  expect_identical(dol.form(123.0, tex = TRUE), '\\$123')

  expect_identical(ntostr(1999:2020, 2L), sprintf('%02d', c(99L, 0:20)))
})

test_that('embed.mat works', {
  m = matrix(1:10, 5L, 2L)
  expect_identical(embed.mat(m, 6L, 3L), rbind(cbind(m, 0L), 0L))
  expect_identical(embed.mat(m, M=6L, N=3L, m=2L, n=2L), rbind(0L, cbind(0L, m)))
  expect_identical(embed.mat(m, 6L, 3L, fill = 1L), rbind(cbind(m, 1L), 1L))

  expect_error(embed.mat(m, 1L, 1L), 'Supplied matrix too large for supplied enclosing matrix')
  expect_error(embed.mat(m, m=10L, n=10L), 'Supplied starting position outside supplied enclosing matrix bounds')
})

test_that('set utilities work', {
  A = 1:5
  B = 3:8
  expect_identical(A %u% B, 1:8)
  expect_identical(A %\% B, 1:2)
  expect_identical(A %^% B, 3:5)
})

# works as long as range(dates) doesn't include leap centuries:
#   1900-03-01 = -25508
#   2100-02-28 =  47540
test_that('quick date utils work', {
  dates = .Date(c(-1.0e4, 0.0, 365.0, 730.0, 1096.0, 1461.0, 1.0e4))
  dates_lt = as.POSIXlt(dates)

  expect_identical(quick_year(dates), dates_lt$year + 1900L)
  expect_identical(quick_yday(dates), dates_lt$yday + 1L)
  expect_identical(quick_mday(dates), dates_lt$mday)
})

test_that('get_age works', {
  test_df = data.frame(
    birth_date = .Date(c(
      3285.0, 3286.0, 3287.0, -2559.0, -2558.0, -2557.0, 11124.0, 11125.0,
      11126.0, 13590.0, 13591.0, 13592.0, -672.0, -672.0, -672.0
    )),
    given_date = .Date(c(
      16800.0, 16800.0, 16800.0, 16800.0, 16800.0, 16800.0, 29387.0, 29387.0,
      29387.0, 13957.0, 13957.0, 13957.0, 16494.0, 16495.0, 16496.0
    ))
  )
  expect_identical(
    with(test_df, get_age(birth_date, given_date)), c(
      37.0 + 1.0/366.0, # will be 366 days until 12/31/16
      37.0,
      37.0 - 1.0/365.0,
      53.0 + 1.0/366.0, # ditto here
      53.0,
      53.0 - 1.0/365.0,
      50.0 + 1.0/365.0,
      50.0,
      50.0 - 1.0/365.0,
      1.0 + 1.0/365.0,  # 2/29 already passed, only 365 days until 3/19/2009
      1.0,
      1.0 - 1.0/366.0,
      47.0 - 1.0/365.0, # my judgment: birthday occurs on 3/1 for 2/29 babies, so 364/365 the way there
      47.0,
      47.0 + 1.0/366.0
    )
  )
})

test_that('create_quantiles works', {
  expect_identical(create_quantiles(1:10, 4L), factor(rep(1:4, c(3L, 2L, 2L, 3L))))
  expect_identical(create_quantiles(1:10, 3L), factor(rep(1:3, c(3L, 3L, 4L))))

  expect_error(create_quantiles(rep(1:2, c(100L, 1L)), 2L), 'Overlapping quantiles. Please provide 1 label.')
  expect_error(create_quantiles(rep(1:2, c(2L, 1L)), 3L), 'Overlapping quantiles. Please provide 2 labels.')
})

test_that('write_packages works', {
  invisible(capture.output({
    out <- write.packages()
  }))

  expect_named(out, c(
    "r_version", "locale", "running", "linear_algebra", "base_packages",
    "other_packages", "loaded_via_namespace", "write_package_time"
  ))

  expect_identical(out$r_version$version.string, R.version.string)
})

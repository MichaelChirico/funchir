context('Utility functions')

test_that('stale_package_check works', {
  script_dir = 'stale_package_test_scripts'
  expect_output(
    stale_package_check(file.path(script_dir, 'simple.R')),
    paste(
      c("Functions matched from package stats:", "\tdensity, rnorm",
        "Functions matched from package tools:", "\tfile_ext",
        "**No exported functions matched from tcltk**"),
      collapse = '\n'
    ),
    fixed = TRUE
  )

  expect_output(
    stale_package_check(file.path(script_dir, 'use_namespace_call.R')),
    '**No exported functions matched from stats**',
    fixed = TRUE
  )

  expect_output(
    stale_package_check(file.path(script_dir, 'wont-parse.R')),
    'Failed to parse R script, please fix syntax errors first',
    fixed = TRUE
  )

  expect_output(
    stale_package_check(file.path(script_dir, 'no_library.R')),
    'No library() or require() calls found',
    fixed = TRUE
  )
})

test_that('one-line utilities work', {
  expect_equal(to.pct(.8, 2L), 80)
  expect_equal(to.pct(.8030432, 3L), 80.304)

  expect_equal(nx.mlt(3, 5), 5)
  expect_equal(nx.mlt(24, 17), 34)

  expect_equal(divide(c(1, 4, 8, 9, 11, 2, 2), 3), c(1L, 6L, 11L))

  expect_equal(dol.form(1e6), '$1,000,000')
  expect_equal(dol.form(1e6, suff='m'), '$1m')
  expect_equal(dol.form(-1e6), '-$1,000,000')
  expect_equal(dol.form(123.456, dig = 0L), '$123')
  expect_equal(dol.form(123, tex = TRUE), '\\$123')

  expect_equal(ntostr(1999:2020, 2), sprintf('%02d', c(99, 0:20)))
})

test_that('embed.mat works', {
  m = matrix(1:10, 5L, 2L)
  expect_equal(embed.mat(m, 6, 3), rbind(cbind(m, 0), 0))
  expect_equal(embed.mat(m, M=6, N=3, m=2, n=2), rbind(0, cbind(0, m)))
  expect_equal(embed.mat(m, 6, 3, fill = 1), rbind(cbind(m, 1), 1))

  expect_error(embed.mat(m, 1, 1), 'Supplied matrix too large for supplied enclosing matrix')
  expect_error(embed.mat(m, m=10, n=10), 'Supplied starting position outside supplied enclosing matrix bounds')
})

test_that('set utilities work', {
  A = 1:5
  B = 3:8
  expect_equal(A %u% B, 1:8)
  expect_equal(A %\% B, 1:2)
  expect_equal(A %^% B, 3:5)
})

# works as long as range(dates) doesn't include leap centuries:
#   1900-03-01 = -25508
#   2100-02-28 =  47540
test_that('quick date utils work', {
  dates = .Date(c(-1e4, 0, 365, 730, 1096, 1461, 1e4))
  dates_lt = as.POSIXlt(dates)

  expect_equal(quick_year(dates), dates_lt$year + 1900L)
  expect_equal(quick_yday(dates), dates_lt$yday + 1L)
  expect_equal(quick_mday(dates), dates_lt$mday)
})

test_that('get_age works', {
  test_df = data.frame(
    birth_date = .Date(c(
      3285, 3286, 3287, -2559, -2558, -2557, 11124, 11125,
      11126, 13590, 13591, 13592, -672, -672, -672
    )),
    given_date = .Date(c(
      16800, 16800, 16800, 16800, 16800, 16800, 29387, 29387,
      29387, 13957, 13957, 13957, 16494, 16495, 16496
    ))
  )
  expect_equal(
    with(test_df, get_age(birth_date, given_date)), c(
      37.0027322404372, # will be 366 days until 12/31/16, so fraction is 1/366
      37, 36.9972602739726,
      53.0027322404372, # ditto here
      53, 52.9972602739726, 50.0027397260274, 50,
      49.9972602739726, # fraction should be 364/365
      1.0027397260274,  # 2/29 already passed, only 365 days until 3/19/2009
      1, 0.997267759562842,
      46.9972602739726, # my judgment: birthday occurs on 3/1 for 2/29 babies, so 364/365 the way there
      47, 47.0027322404372
    )
  )
})

test_that('create_quantiles works', {
  expect_equal(create_quantiles(1:10, 4L), factor(rep(1:4, c(3L, 2L, 2L, 3L))))
  expect_equal(create_quantiles(1:10, 3L), factor(rep(1:3, c(3L, 3L, 4L))))

  expect_error(create_quantiles(rep(1:2, c(100L, 1L)), 2L), error='Overlapping quantiles. Please provide 1 label.')
  expect_error(create_quantiles(rep(1:2, c(2L, 1L)), 3L), error='Overlapping quantiles. Please provide 2 labels.')
})

test_that('write_packages works', {
  invisible(capture.output(out <- write.packages()))

  expect_equal(
    names(out), c(
      "r_version", "locale", "running", "linear_algebra", "base_packages",
      "other_packages", "loaded_via_namespace", "write_package_time"
    )
  )

  expect_equal(out$r_version$version.string, R.version.string)
})

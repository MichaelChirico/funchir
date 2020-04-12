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

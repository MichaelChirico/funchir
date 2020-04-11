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
    stale_package_check(file.path(script_dir, 'wont-parse.R')),
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
})

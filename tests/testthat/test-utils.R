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
})

linters = all_linters(
  packages = "lintr",
  implicit_integer_linter(allow_colon=TRUE),
  line_length_linter(120L),
  undesirable_function_linter(modify_defaults(
    default_undesirable_functions,
    library = NULL,
    par = NULL
  )),
  assignment_linter = NULL,
  commas_linter = NULL,
  commented_code_linter = NULL,
  condition_call_linter = NULL,
  infix_spaces_linter = NULL,
  library_call_linter = NULL,
  nonportable_path_linter = NULL,
  object_name_linter = NULL,
  quotes_linter = NULL,
  spaces_inside_linter = NULL,
  todo_comment_linter = NULL,
  unreachable_code_linter = NULL
)
exclusions = list(`tests/testthat/stale_package_test_scripts/wont_parse.R` = Inf)

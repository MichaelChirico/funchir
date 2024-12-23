test_that('sanitize2 works', {
  expect_identical(
    sanitize2('$\\\\text[lw=.5]{hey}|u| % 4 & _#^~$'),
    "\\$$\\backslash$text\\lbracklw=.5\\rbrack\\{hey\\}$|$u$|$ \\% 4 \\& \\_\\#\\verb|^|\\~{}\\$"
  )
})

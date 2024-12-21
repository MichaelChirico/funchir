test_that('abbr_to_colClass works', {
  expect_identical(
    abbr_to_colClass('bcflinDtds', '1234567891'),
    rep(
      c("blank", "character", "factor", "logical", "integer", "numeric", "Date", "text", "date", "skip"),
      c(1:9, 1L)
    )
  )
})

test_that('sanitize2 works', {
  expect_identical(
    sanitize2('$\\\\text[lw=.5]{hey}|u| % 4 & _#^~$'),
    "\\$$\\backslash$text\\lbracklw=.5\\rbrack\\{hey\\}$|$u$|$ \\% 4 \\& \\_\\#\\verb|^|\\~{}\\$"
  )
})

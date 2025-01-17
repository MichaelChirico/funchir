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
  expect_identical(A %\% B, 1:2)
})

# works as long as range(dates) doesn't include leap centuries:
#   1900-03-01 = -25508
#   2100-02-28 =  47540
test_that('quick date utils work', {
  dates = as.Date(c(
    "1942-08-16", "1970-01-01", "1971-01-01", "1972-01-01", "1973-01-01", "1974-01-01", "1997-05-19"
  ))
  dates_lt = as.POSIXlt(dates)

  expect_identical(quick_year(dates), dates_lt$year + 1900L)
  expect_identical(quick_yday(dates), dates_lt$yday + 1L)
  expect_identical(quick_mday(dates), dates_lt$mday)
})

test_that('get_age works', {
  birth_date = as.Date(c(
    "1978-12-30", "1978-12-31", "1979-01-01",
    "1962-12-30", "1962-12-31", "1963-01-01",
    "2000-06-16", "2000-06-17", "2000-06-18",
    "2007-03-18", "2007-03-19", "2007-03-20",
    "1968-02-29", "1968-02-29", "1968-02-29",
    "2024-12-22", "2025-03-01", "2026-03-01",
    "2027-03-01",
    NA_character_, Sys.Date(), NA_character_,
    NULL
  ))
  given_date = as.Date(c(
    "2015-12-31", "2015-12-31", "2015-12-31",
    "2015-12-31", "2015-12-31", "2015-12-31",
    "2050-06-17", "2050-06-17", "2050-06-17",
    "2008-03-19", "2008-03-19", "2008-03-19",
    "2015-02-28", "2015-03-01", "2015-03-02",
    "2031-12-23", "2028-12-22", "2029-03-02",
    "2030-03-02",
    Sys.Date(), NA_character_, NA_character_,
    NULL
  ))
  expect_identical(
    get_age(birth_date, given_date), c(
      37.0 + 1.0/366.0, # will be 366 days until 2016-12-31
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
      47.0 + 1.0/365.0, # 365 days until 2016-02-29, not 366

      7.0 + 1.0/366.0, # 366 days until 2032-12-22, not 367 (#23)
      3.0 + 296.0/365.0, # 365 days until 2029-03-01, not 366 (#26)
      3.0 + 1.0/365.0, # 365 days until 2030-03-01, not 366 (#28)
      3.0 + 1.0/365.0, # 365 days until 2031-03-02, not 366 (#30)

      NA_real_, NA_real_, NA_real_,
      NULL
    )
  )

  # Don't require Date input if it can be coerced thereto
  bday = as.Date('2023-01-01')
  tday = as.Date('2025-01-01')
  ## Y-M-D character
  expect_identical(get_age(format(bday), format(tday)), get_age(bday, tday))
  expect_identical(get_age(format(bday), tday), get_age(bday, tday))
  expect_identical(get_age(bday, format(tday)), get_age(bday, tday))
  ## IDate
  expect_identical(get_age(data.table::as.IDate(bday), data.table::as.IDate(tday)), get_age(bday, tday))
  expect_identical(get_age(data.table::as.IDate(bday), tday), get_age(bday, tday))
  expect_identical(get_age(bday, data.table::as.IDate(tday)), get_age(bday, tday))
  ## POSIXct
  expect_identical(get_age(as.POSIXct(bday), as.POSIXct(tday)), get_age(bday, tday))
  expect_identical(get_age(as.POSIXct(bday), tday), get_age(bday, tday))
  expect_identical(get_age(bday, as.POSIXct(tday)), get_age(bday, tday))

  # Input validation: lengths & recycling
  expect_identical(get_age(c('2023-01-01', '2024-01-01'), '2025-01-01'), c(2.0, 1.0))
  expect_identical(get_age('2023-01-01', c('2024-01-01', '2025-01-01')), c(1.0, 2.0))
  expect_identical(get_age(numeric(), numeric()), numeric())
  expect_error(get_age(numeric(3L), numeric(4L)), "must have equal length")
})

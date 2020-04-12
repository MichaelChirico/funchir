context('Plotting utilities')

test_that('device-units-to-inches functions work', {
  on.exit(dev.off())
  grDevices::png(tempfile())
  plot(NA, xlim=0:1, ylim=0:1)
  expect_equal(xdev2in(), 5.02469135802469, tolerance = 1e-4)
  expect_equal(ydev2in(), 4.46913580246914, tolerance = 1e-4)
  expect_equal(xydev2in(), c(5.02469135802469, 4.46913580246914), tolerance = 1e-4)

  for (ii in 1:4) expect_invisible(tile.axes(ii, 2L, 2L))
})

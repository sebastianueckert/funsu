test_that("default settings are returned as list", {
  fmls <- as.list(formals(settings.gq))
  expect_equal(settings.gq(), fmls)
})

test_that("settings can be changed", {
  expect_equal(settings.gq(quad_points = 3), list(quad_points = 3))
  expect_equal(settings.gq(3), list(quad_points = 3))

})

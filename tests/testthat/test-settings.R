test_that("default settings are returned as list", {
  fmls <- as.list(formals(defaults.gq))
  expect_equal(defaults.gq(), fmls)
})

test_that("settings can be changed", {
  expect_equal(defaults.gq(quad_points = 3), list(quad_points = 3))
  expect_equal(defaults.gq(3), list(quad_points = 3))

})

test_that("expected value calculation using gq works in 1D", {
  f <- function(x) x^2
  expect_equal(expect_norm_gq(f), 1)
})

test_that("expected value calculation using gq works in 4D", {
  f <- function(x) x^2
  expect_equal(expect_norm_gq(f, 4), c(1,1,1,1))
})

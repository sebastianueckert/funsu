test_that("gq expected value calculation in 1D works", {
  f <- function(x) x^2
  # chi-square distribution
  expect_equal(expect_norm_gq(f), 1)

  #non-central chi-square
  expect_equal(expect_norm_gq(f, center = 1), 2)

})

test_that("gq expected value calculation in 4D works", {
  f <- function(x) x^2
  expect_equal(expect_norm_gq(f, 4), c(1,1,1,1))

  #non-central chi-square
  expect_equal(expect_norm_gq(f, 4, center = 1), c(2,2,2,2))

})

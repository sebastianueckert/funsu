test_that("gq expected value calculation in 1D works", {
  f <- function(x) x^2
  # chi-square distribution
  expect_equal(expect_norm_gq(f), 1)

  #non-central chi-square
  expect_equal(expect_norm_gq(f, mu = 1), 2)

})

test_that("gq expected value calculation in 4D works", {
  f <- function(x) x^2
  expect_equal(expect_norm_gq(f, 4), c(1,1,1,1))

  #non-central chi-square
  expect_equal(expect_norm_gq(f, 4, mu = c(1,0,2,3)), c(2,1,5,10))

})

test_that("mc expected value calculation in 4D works", {
  f <- function(x) x^2
  expect_lt(abs(mean(expect_norm_mc(f, 4)-c(1,1,1,1))), 0.1)

  #non-central chi-square
  expect_lt(abs(mean(expect_norm_mc(f, 4, mu = c(1,0,2,3))-c(2,1,5,10))), 0.1)

})

test_that("error message is shown when required settings are missing",{
  expect_error(expect_norm_gq(function(x)x,1,settings = list()))
  expect_error(expect_norm_mc(function(x)x,1,settings = list()))
})

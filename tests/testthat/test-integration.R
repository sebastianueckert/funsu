test_that("expected value calculation using gq works", {
  f <- function(x) x^2
  expect_equal(expect_norm_gq(f), 1)
})

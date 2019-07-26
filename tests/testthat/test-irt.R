test_that("expectation calculation works", {
  expected <- c(Comfort = 2.12020087820744, Work = 1.72405134617488, Future = 1.9887845186995,  Benefit = 1.83671835360009)
  fit <- mirt::mirt(mirt::Science, 1, verbose = F)
  expect_equal(irt_expected_item_response(fit), expected)
})

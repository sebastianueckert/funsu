fit <- mirt::mirt(mirt::Science, 1, verbose = F)


test_that("expectation calculation works", {
  expected <-
    c(
      Comfort = 2.12020087820744,
      Work = 1.72405134617488,
      Future = 1.9887845186995,
      Benefit = 1.83671835360009
    )
  expect_equal(irt_expected_item_response(fit), expected)
})

test_that("covariance calculation works", {
  expected <-
    structure(
      c(
        0.349472186051655,
        0.105930771347902,
        0.140622699245503,
        0.0986103365223237,
        0.105930771347902,
        0.65430318337363,
        0.238568664281186,
        0.166798309159225,
        0.140622699245503,
        0.238568664281186,
        0.575280920775503,
        0.221589654473857,
        0.0986103365223237,
        0.166798309159225,
        0.221589654473857,
        0.648215038247966
      ),
      .Dim = c(4L, 4L),
      .Dimnames = list(
        c("Comfort",  "Work", "Future", "Benefit"),
        c("Comfort", "Work", "Future",  "Benefit")
      )
    )
  expect_equal(irt_expected_cov_matrix(fit), expected)
})

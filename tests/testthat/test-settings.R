test_that("default settings are returned as list", {
  fmls <- as.list(formals(settings.gq))
  expect_equal(settings.gq(), fmls)
})

test_that("settings can be changed", {
  expect_equal(settings.gq(quad_points = 3), list(quad_points = 3))
  expect_equal(settings.gq(3), list(quad_points = 3))

})

test_that("settings function creation works", {
  my_settings <- function(opt_1 = 1) make_settings()
  expect_equal(my_settings(), list(opt_1 = 1))
  expect_equal(my_settings(opt_1 = 2), list(opt_1 = 2))
})

test_that("nesting function works as expected",{
  expect_equal(nest_lst_by_name(list(gq.quad_points = 3, gq.option = TRUE, mc.option = FALSE, option = "test")),
                                list(option = "test", gq = list(quad_points = 3, option = TRUE), mc = list(option = FALSE)))
})

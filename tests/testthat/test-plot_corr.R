# create test objects
dat <- mice::nhanes

# tests
test_that("plot_corr creates ggplot object", {
  expect_s3_class(plot_corr(dat), "ggplot")
  expect_s3_class(plot_corr(dat, label = TRUE, square = FALSE, diagonal = TRUE, rotate = TRUE), "ggplot")
})

test_that("plot_corr takes non-default input arguments", {
  expect_s3_class(plot_corr(dat, c("age", "bmi")), "ggplot")
  expect_s3_class(plot_corr(dat, c(age, bmi)), "ggplot")
})

test_that("plot_corr returns error with incorrect argument(s)", {
  expect_error(plot_corr(data = "test"))
  expect_error(plot_corr(dat, vrb = "test"))
  expect_error(plot_corr(dat, age))
})

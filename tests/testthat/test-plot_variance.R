# create test objects
dat <- mice::nhanes
imp <- mice::mice(dat, printFlag = FALSE)

# tests
test_that("plot_variance produces ggplot object", {
  expect_s3_class(plot_variance(imp), "ggplot")
  expect_s3_class(plot_variance(imp, grid = FALSE), "ggplot")
})

test_that("plot_variance returns error with incorrect arguments", {
  expect_error(plot_variance(dat))
})

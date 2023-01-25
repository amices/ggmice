test_that("plot_variance produces ggplot object", {
  imp <- mice::mice(mice::nhanes, printFlag = FALSE)
  gg <- plot_variance(imp)
  expect_s3_class(gg, "ggplot")
  gg <- plot_variance(imp, grid = FALSE)
  expect_s3_class(gg, "ggplot")
})

test_that("plot_variance returns error with incorrect arguments", {
  dat <- mice::nhanes
  expect_error(plot_variance(dat))
})

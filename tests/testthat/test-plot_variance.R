test_that("plot_variance runs", {
  imp <- mice(nhanes)
  gg <- plot_variance(imp)
  expect_s3_class(gg, "ggplot")
})

test_that("ggmice with incorrect arguments", {
  dat <- mice::nhanes
  expect_error(plot_variance(dat))
})

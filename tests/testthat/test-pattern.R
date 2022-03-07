test_that("plot_pattern produces plot", {
  expect_s3_class(plot_pattern(mice::nhanes), "ggplot")
})

test_that("plot_corr creates ggplot object", {
  gg <- plot_corr(mice::nhanes)
  expect_s3_class(gg, "ggplot")
})

test_that("plot_corr returns error with incorrect variable name(s)", {
  expect_error(plot_corr(mice::nhanes, vrb = "test"))
})

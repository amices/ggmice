test_that("plot_pred creates ggplot object", {
  gg <- plot_pred(mice::quickpred(mice::nhanes))
  expect_s3_class(gg, "ggplot")
})

test_that("plot_corr creates ggplot object", {
  gg <- plot_corr(mice::nhanes)
  expect_s3_class(gg, "ggplot")
})

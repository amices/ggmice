test_that("plot_pred creates ggplot object", {
  pred <- mice::quickpred(mice::nhanes)
  gg <- plot_pred(pred)
  expect_s3_class(gg, "ggplot")
})

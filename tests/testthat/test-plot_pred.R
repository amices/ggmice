test_that("plot_pred creates ggplot object", {
  pred <- mice::quickpred(mice::nhanes)
  gg <- plot_pred(pred)
  expect_s3_class(gg, "ggplot")
  gg <- plot_pred(pred, method = c("pmm"), label = FALSE, square = FALSE, rotate = TRUE)
  expect_s3_class(gg, "ggplot")
  expect_s3_class(plot_pred(pred, vrb = c("age", "bmi")), "ggplot")
  expect_s3_class(plot_pred(pred, vrb = c(age, bmi)), "ggplot")
})

test_that("plot_pred with incorrect argument(s)", {
  expect_error(plot_pred(mice::nhanes))
})

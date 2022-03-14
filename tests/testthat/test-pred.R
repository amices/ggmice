# Test ggmice::plot_* set of functions (alphabetically sorted)

test_that("plot_corr creates ggplot object", {
  gg <- plot_corr(mice::nhanes)
  expect_s3_class(gg, "ggplot")
})

test_that("plot_flux creates ggplot object", {
  gg <- plot_flux(mice::nhanes)
  expect_s3_class(gg, "ggplot")
})

test_that("plot_pattern produces plot", {
  gg <- plot_pattern(mice::nhanes)
  expect_s3_class(gg, "ggplot")
})

test_that("plot_pred creates ggplot object", {
  pred <- mice::quickpred(mice::nhanes)
  gg <- plot_pred(pred)
  expect_s3_class(gg, "ggplot")
})

test_that("plot_trace creates ggplot object", {
  imp <- mice::mice(mice::nhanes, printFlag = FALSE)
  gg <- plot_trace(imp)
  expect_s3_class(gg, "ggplot")
})


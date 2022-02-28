test_that("plot_chains creates ggplot object", {
  imp <- mice::mice(data.frame(a = 1:4, b = c(NA, 2, 2, 1)), printFlag = FALSE)
  gg <- plot_chains(imp)
  expect_s3_class(gg, "ggplot")
})

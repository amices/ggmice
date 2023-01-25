test_that("plot_corr creates ggplot object", {
  gg <- plot_corr(mice::nhanes)
  expect_s3_class(gg, "ggplot")
  gg <- plot_corr(mice::nhanes, label = TRUE, square = FALSE, diagonal = TRUE, rotate = TRUE)
  expect_s3_class(gg, "ggplot")
})

test_that("plot_corr returns error with incorrect argument(s)", {
  expect_error(plot_corr(data = "test"))
  expect_error(plot_corr(mice::nhanes, vrb = "test"))
})

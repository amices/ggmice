test_that("plot_pattern produces plot", {
  gg <- plot_pattern(mice::nhanes)
  expect_s3_class(gg, "ggplot")
})

test_that("plot_pattern returns message with complete data", {
  expect_message(plot_pattern(na.omit(mice::nhanes)))
})

test_that("plot_pattern produces plot", {
  gg <- plot_pattern(mice::nhanes)
  expect_s3_class(gg, "ggplot")
  gg <- plot_pattern(mice::nhanes, square = FALSE, rotate = TRUE, cluster = "age", npat = 2)
  expect_s3_class(gg, "ggplot")
  gg <- plot_pattern(cbind(mice::nhanes, "test var" = NA))
  expect_s3_class(gg, "ggplot")
})

test_that("plot_pattern with incorrect argument(s)", {
  expect_message(plot_pattern(na.omit(mice::nhanes)))
  expect_error(plot_pattern("test"))
  expect_error(plot_pattern(mice::nhanes, vrb = "test"))
  expect_error(plot_pattern(mice::nhanes, cluster = "test"))
  expect_error(plot_pattern(cbind(mice::nhanes, .x = NA)))
  expect_error(plot_pattern(mice::nhanes, npat = "test"))
  expect_message(plot_pattern(na.omit(mice::nhanes)))
})

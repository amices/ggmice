test_that("plot_flux creates ggplot object", {
  gg <- plot_flux(mice::nhanes)
  expect_s3_class(gg, "ggplot")
  gg <- plot_flux(mice::nhanes, label = FALSE, caption = FALSE)
  expect_s3_class(gg, "ggplot")
})

test_that("plot_flux returns error with incorrect argument(s)", {
  expect_error(plot_flux(mice::nhanes, vrb = "test"))
})

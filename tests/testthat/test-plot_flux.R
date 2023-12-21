# create test objects
dat <- mice::nhanes

# tests
test_that("plot_flux creates ggplot object", {
  expect_s3_class(plot_flux(dat), "ggplot")
  expect_s3_class(plot_flux(dat, label = FALSE, caption = FALSE), "ggplot")
})

test_that("plot_flux works with different inputs", {
  expect_s3_class(plot_flux(dat, c("age", "bmi")), "ggplot")
  expect_s3_class(plot_flux(na.omit(dat)), "ggplot")
  expect_s3_class(plot_flux(cbind(dat, "with space" = NA)), "ggplot")
})

test_that("plot_flux returns error with incorrect argument(s)", {
  expect_error(plot_flux(dat, vrb = "test"))
})

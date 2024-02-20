# create test objects
dat <- mice::nhanes

# tests
test_that("plot_pattern produces plot", {
  expect_s3_class(plot_pattern(dat), "ggplot")
  expect_s3_class(plot_pattern(dat, square = FALSE, rotate = TRUE, cluster = "age", grid = FALSE, npat = 2), "ggplot")
  expect_s3_class(plot_pattern(cbind(dat, "testvar" = NA), caption = FALSE), "ggplot")
})

test_that("plot_pattern works with different inputs", {
  expect_s3_class(plot_pattern(dat, c("age", "bmi")), "ggplot")
  expect_s3_class(plot_pattern(dat, c(age, bmi)), "ggplot")
  expect_s3_class(plot_pattern(data.frame(age = dat$age, testvar = NA)), "ggplot")
  expect_s3_class(plot_pattern(cbind(dat, "with space" = NA)), "ggplot")
})


test_that("plot_pattern with incorrect argument(s)", {
  expect_output(plot_pattern(na.omit(dat)))
  expect_error(plot_pattern("test"))
  expect_error(plot_pattern(dat, vrb = "test"))
  expect_error(plot_pattern(dat, cluster = "test"))
  expect_error(plot_pattern(cbind(dat, .x = NA)))
  expect_error(plot_pattern(dat, npat = "test"))
})

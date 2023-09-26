# create test objects
dat <- mice::nhanes

# tests
test_that("plot_miss produces plot", {
  expect_s3_class(plot_miss(dat), "ggplot")
  expect_s3_class(plot_miss(dat), "ggplot")
  expect_s3_class(plot_miss(cbind(dat, "testvar" = NA)), "ggplot")
})

test_that("plot_miss works with different inputs", {
  expect_s3_class(plot_miss(dat, c("age", "bmi")), "ggplot")
  expect_s3_class(plot_miss(dat, c(age, bmi)), "ggplot")
  expect_s3_class(plot_miss(data.frame(age = dat$age, testvar = NA)), "ggplot")
  expect_s3_class(plot_miss(cbind(dat, "with space" = NA)), "ggplot")
})


test_that("plot_miss with incorrect argument(s)", {
  expect_output(plot_miss(na.omit(dat)))
  expect_error(plot_miss("test"))
  expect_error(plot_miss(dat, vrb = "test"))
  expect_error(plot_miss(dat, cluster = "test"))
  expect_error(plot_miss(cbind(dat, .x = NA)))
  expect_error(plot_miss(dat, npat = "test"))
})

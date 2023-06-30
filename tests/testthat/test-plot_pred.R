# create test objects
dat <- mice::nhanes
pred <- mice::quickpred(dat)

# tests
test_that("plot_pred creates ggplot object", {
  expect_s3_class(plot_pred(pred), "ggplot")
  expect_s3_class(plot_pred(pred, method = c("pmm"), label = FALSE, square = FALSE, rotate = TRUE), "ggplot")
  expect_s3_class(plot_pred(pred, vrb = c("age", "bmi")), "ggplot")
  expect_s3_class(plot_pred(pred, vrb = c(age, bmi)), "ggplot")
})

test_that("plot_pred with incorrect argument(s)", {
  expect_error(plot_pred(dat))
})

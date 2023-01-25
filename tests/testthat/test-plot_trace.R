test_that("plot_trace creates ggplot object", {
  imp <- mice::mice(mice::nhanes, printFlag = FALSE)
  gg <- plot_trace(imp)
  expect_s3_class(gg, "ggplot")
  gg <- plot_trace(imp, vrb = "bmi")
  expect_s3_class(gg, "ggplot")
})

test_that("plot_trace returns error with incorrect data argument", {
  dat <- mice::nhanes
  expect_error(plot_trace(dat))
})

test_that("plot_trace returns error with incorrect variable name(s)", {
  imp <- mice::mice(mice::nhanes, printFlag = FALSE)
  expect_error(plot_trace(imp, vrb = "test"))
})

test_that("plot_trace creates ggplot object", {
  imp <- mice::mice(mice::nhanes, printFlag = FALSE)
  expect_s3_class(plot_trace(imp), "ggplot")
  expect_s3_class(plot_trace(imp, vrb = "bmi"), "ggplot")
  expect_s3_class(plot_trace(imp, vrb = c("bmi", "hyp")), "ggplot")
  expect_s3_class(plot_trace(imp, vrb = bmi), "ggplot")
  })

test_that("plot_trace returns error with incorrect argument(s)", {
  imp <- mice::mice(mice::nhanes, printFlag = FALSE)
  expect_error(plot_trace(mice::nhanes))
  expect_error(plot_trace(imp, vrb = "test"))
  expect_error(plot_trace(imp, vrb = "age"))
  expect_message(plot_trace(imp, vrb = c("age", "bmi")))
})

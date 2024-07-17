# create test objects
dat <- mice::nhanes
imp <- mice::mice(dat, printFlag = FALSE)
v   <- c("bmi", "hyp")

# tests
test_that("plot_trace creates ggplot object", {
  expect_s3_class(plot_trace(imp), "ggplot")
  expect_s3_class(plot_trace(imp, vrb = "bmi"), "ggplot")
  expect_s3_class(plot_trace(imp, vrb = c("bmi", "hyp")), "ggplot")
  expect_s3_class(plot_trace(imp, vrb = "all"), "ggplot")
  expect_s3_class(plot_trace(imp, vrb = bmi), "ggplot")
  expect_s3_class(plot_trace(imp, vrb = c("bmi", "hyp")), "ggplot")
  expect_s3_class(plot_trace(imp, vrb = c(bmi, hyp)), "ggplot")
  expect_s3_class(plot_trace(imp, vrb = !!v), "ggplot")
  expect_s3_class(plot_trace(imp, vrb = !!v[1]), "ggplot")
})

test_that("plot_trace returns error with incorrect argument(s)", {
  expect_error(plot_trace(dat))
  expect_error(plot_trace(imp, vrb = "test"))
  expect_error(plot_trace(imp, vrb = "age"))
  expect_message(plot_trace(imp, vrb = c("age", "bmi")))
  expect_error(plot_trace(imp, vrb = v))
  expect_error(plot_trace(imp, vrb = v[1]))
})

# create test objects
dat <- mice::nhanes
imp <- mice::mice(dat, printFlag = FALSE)

# tests
test_that("plot_density creates ggplot object", {
    expect_s3_class(plot_density(imp), "ggplot")
    expect_s3_class(plot_density(imp, vrb = "bmi"), "ggplot")
    expect_s3_class(plot_density(imp, vrb = c("bmi", "hyp")), "ggplot")
    expect_s3_class(plot_density(imp, vrb = "all"), "ggplot")
    expect_s3_class(plot_density(imp, vrb = bmi), "ggplot")
    expect_s3_class(plot_density(imp, vrb = c("bmi", "hyp")), "ggplot")
    expect_s3_class(plot_density(imp, vrb = c(bmi, hyp)), "ggplot")
})

test_that("plot_density returns error with incorrect argument(s)", {
    expect_error(plot_density(dat))
    expect_error(plot_density(imp, vrb = "test"))
    expect_error(plot_density(imp, vrb = "age"))
    expect_message(plot_density(imp, vrb = c("age", "bmi")))
})

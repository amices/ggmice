test_that("bwplot creates ggplot object", {
  imp <- mice::mice(data.frame(a = 1:4, b = c(NA, 2, 2, 1)), printFlag = FALSE)
  gg <- bwplot(imp, "b")
  expect_type(gg, "list")
  expect_s3_class(gg[[1]], "ggplot")
})

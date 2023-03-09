test_that("set of old friends functions generate plot", {
  imp <- mice::mice(mice::nhanes, printFlag = FALSE)
  expect_s3_class(bwplot(imp), "trellis")
  expect_s3_class(densityplot(imp), "trellis")
  expect_s3_class(stripplot(imp), "trellis")
  expect_s3_class(xyplot(imp, bmi ~ age), "trellis")
})

# test_that("set of old friends functions generate message", {
#   imp <- mice::mice(mice::nhanes, printFlag = FALSE)
#   expect_message(bwplot(imp))
#   expect_message(densityplot(imp))
#   expect_message(stripplot(imp))
#   expect_message(xyplot(imp, bmi ~ age))
# })

test_that("ggmice with continuous data", {
  dat <- dplyr::mutate(mice::nhanes,
    age = as.integer(age),
    bmi = bmi,
    .keep = "used"
  )
  gg <- ggmice(dat, ggplot2::aes("bmi"))
  expect_s3_class(gg, "ggplot")
  imp <- mice::mice(dat, printFlag = FALSE)
  gg <- ggmice(imp, ggplot2::aes("bmi"))
  expect_s3_class(gg, "ggplot")
})

test_that("ggmice with categorical data", {
  dat <- dplyr::mutate(mice::nhanes,
    age = as.factor(age),
    hyp = hyp,
    .keep = "used"
  )
  gg <- ggmice(dat, ggplot2::aes("bmi"))
  expect_s3_class(gg, "ggplot")
  imp <- mice::mice(dat, printFlag = FALSE)
  gg <- ggmice(imp, ggplot2::aes("bmi"))
  expect_s3_class(gg, "ggplot")
})

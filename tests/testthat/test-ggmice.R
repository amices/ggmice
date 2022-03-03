test_that("ggmice with continuous data", {
  dat <- dplyr::mutate(mice::nhanes,
    age = as.integer(age),
    bmi = bmi,
    .keep = "used"
  )
  gg <- ggmice(dat, ggplot2::aes(bmi))
  expect_s3_class(gg, "ggplot")
  imp <- mice::mice(dat, printFlag = FALSE)
  gg <- ggmice(imp, ggplot2::aes(bmi))
  expect_s3_class(gg, "ggplot")
})

test_that("ggmice with categorical data", {
  dat <- dplyr::mutate(mice::nhanes,
    age = as.factor(age),
    hyp = hyp,
    .keep = "used"
  )
  gg <- ggmice(dat, ggplot2::aes(age))
  expect_s3_class(gg, "ggplot")
  imp <- mice::mice(dat, printFlag = FALSE)
  gg <- ggmice(imp, ggplot2::aes(age))
  expect_s3_class(gg, "ggplot")
})

test_that("ggmice with complete data", {
  dat <- na.omit(mice::nhanes)
  gg <- ggmice(dat, ggplot2::aes(bmi))
  expect_s3_class(gg, "ggplot")
  imp <- mice::mice(dat, printFlag = FALSE, seed = 1)
  gg <- ggmice(imp, ggplot2::aes(bmi))
  expect_s3_class(gg, "ggplot")
})

test_that("ggmice with incorrect arguments", {
  dat <- mice::nhanes
  expect_error(ggmice(as.matrix(dat), ggplot2::aes(bmi)))
  expect_error(ggmice(dat, ggplot2::aes(shape = bmi)))
  expect_error(ggmice(dat, ggplot2::aes("bmi")))
  expect_warning(ggmice(dat, ggplot2::aes(bmi, color = bmi)))
  expect_error(ggmice(dat, ggplot2::aes(abc)))
})

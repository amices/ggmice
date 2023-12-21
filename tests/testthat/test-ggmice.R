# create test objects
dat <- dplyr::mutate(
  mice::nhanes,
  bmi = as.numeric(bmi),
  hyp = as.factor(hyp),
  age2 = as.character(age)
)
imp <- suppressWarnings(mice::mice(dat, printFlag = FALSE))

# tests
test_that("continuous data plot", {
  expect_s3_class(ggmice(dat, ggplot2::aes(bmi)), "ggplot")
  expect_s3_class(ggmice(imp, ggplot2::aes(bmi)), "ggplot")
  expect_s3_class(ggmice(dat, ggplot2::aes(y = bmi)), "ggplot")
  expect_s3_class(ggmice(imp, ggplot2::aes(y = bmi)), "ggplot")
})

test_that("categorical data plot", {
  expect_s3_class(ggmice(dat, ggplot2::aes(hyp)), "ggplot")
  expect_s3_class(ggmice(imp, ggplot2::aes(hyp)), "ggplot")
  expect_s3_class(ggmice(dat, ggplot2::aes(age2)), "ggplot")
  expect_s3_class(ggmice(imp, ggplot2::aes(age2)), "ggplot")
})

test_that("mixed data plot", {
  expect_s3_class(ggmice(dat, ggplot2::aes(age, bmi)), "ggplot")
  expect_s3_class(ggmice(imp, ggplot2::aes(age, bmi)), "ggplot")
  expect_s3_class(ggmice(dat, ggplot2::aes(age, hyp)), "ggplot")
  expect_s3_class(ggmice(imp, ggplot2::aes(age, hyp)), "ggplot")
  expect_s3_class(ggmice(dat, ggplot2::aes(age, age2)), "ggplot")
  expect_s3_class(ggmice(imp, ggplot2::aes(age, age2)), "ggplot")
})

test_that("complete data plot", {
  expect_s3_class(ggmice(na.omit(dat), ggplot2::aes(bmi)), "ggplot")
  imp2 <-
    suppressWarnings(mice::mice(na.omit(dat), printFlag = FALSE, seed = 1))
  expect_s3_class(ggmice(imp2, ggplot2::aes(bmi)), "ggplot")
})

test_that("incorrect data", {
  expect_error(ggmice(NA))
  expect_error(ggmice("test"))
  expect_error(ggmice(as.matrix(dat)))
  expect_error(ggmice(cbind(dat, dat), ggplot2::aes(bmi)))
})

test_that("advanced mapping", {
  expect_error(ggmice(dat, ggplot2::aes(log(age))))
  expect_error(ggmice(dat, ggplot2::aes(age3)))
  expect_warning(ggmice(dat, ggplot2::aes(bmi, color = bmi)))
})

test_that("incorrect mapping", {
  expect_error(ggmice(dat))
  expect_error(ggmice(dat, ggplot2::aes(x = test)))
  expect_error(ggmice(dat, ggplot2::aes(group = age)))
  expect_error(ggmice(dat, ggplot2::aes("bmi")))
})

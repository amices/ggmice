# create test objects
dat <- mice::nhanes
m <- 3
imp <- mice::mice(dat, m = m, printFlag = FALSE)
p <- sum(colSums(imp$where) > 0)

# tests
test_that("returns correct output", {
    expect_s3_class(reshape_mids(imp), "data.frame")
    expect_equal(
        dim(reshape_mids(imp)),
        c(nrow(dat) * (m + 1) * p, 5)
    )
})

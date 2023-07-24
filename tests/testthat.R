library(testthat)
library(ggmice)

set.seed(1)
if (!interactive()) pdf(NULL)
test_check("ggmice")


<!-- README.md is generated from README.Rmd. Please edit that file and render with devtools::build_readme() -->

# `ggmice` <a href='https://amices.github.io/ggmice/'><img src='logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/amices/ggmice/workflows/R-CMD-check/badge.svg)](https://github.com/amices/ggmice/actions)
[![GitHub R package
version](https://img.shields.io/github/r-package/v/amices/ggmice.svg)](https://github.com/amices/ggmice/blob/main/DESCRIPTION)
[![GitHub](https://img.shields.io/github/license/amices/ggmice.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Plotting package for incomplete and imputed data

`ggmice` is an `R` package which enhances the imputation package `mice`
with `ggplot2` visualizations. See the `ggmice` vignette for an overview
of functionalities.

## Installation

You can install the development version of `ggmice` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("amices/ggmice")
```

## Example

Visualize missing data in an incomplete dataset, or evaluate imputed
data against the observed data.

``` r
library(ggmice)
dat <- mice::boys
ggmice(dat, ggplot2::aes(age, bmi)) + ggplot2::geom_point()
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
imp <- mice::mice(dat, m = 1, printFlag = FALSE)
ggmice(imp, ggplot2::aes(age, bmi)) + ggplot2::geom_point() 
```

<img src="man/figures/README-example-2.png" width="100%" />

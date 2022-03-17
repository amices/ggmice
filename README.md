
<!-- README.md is generated from README.Rmd. Please edit that file and render with devtools::build_readme() -->

# `ggmice` <a href='https://amices.org/ggmice/'><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/amices/ggmice/workflows/R-CMD-check/badge.svg)](https://github.com/amices/ggmice/actions)
[![GitHub R package
version](https://img.shields.io/github/r-package/v/amices/ggmice.svg)](https://github.com/amices/ggmice/blob/main/DESCRIPTION)
[![GitHub](https://img.shields.io/github/license/amices/ggmice.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/amices/ggmice/branch/main/graph/badge.svg)](https://app.codecov.io/gh/amices/ggmice?branch=main)
<!-- badges: end -->

## Visualizations for `mice` with `ggplot2`

Enhance a `mice` imputation workflow with visualizations for incomplete
and/or imputed data. The plotting functions produce `ggplot` objects
which may be easily manipulated or extended. Use `ggmice` to inspect
missing data, develop imputation models, evaluate algorithmic
convergence, or compare observed versus imputed data.

## Installation

You can install the latest `ggmice` release from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("ggmice")
```

Alternatively, you could install the development version of `ggmice`
from [GitHub](https://github.com/amices) with:

``` r
# install.packages("devtools")
devtools::install_github("amices/ggmice")
```

## Example

Visualize missing data in an incomplete dataset, or evaluate imputed
data against the observed data. See the [Get
started](https://amices.org/ggmice/articles/ggmice.html) vignette for an
overview of all `ggmice` functionalities.

``` r
# load the package and some data
library(ggmice)
dat <- mice::boys
# visualize the incomplete data
ggmice(dat, ggplot2::aes(age, bmi)) + ggplot2::geom_point()
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
# impute the data and visualize the imputed data
imp <- mice::mice(dat, m = 1, printFlag = FALSE)
ggmice(imp, ggplot2::aes(age, bmi)) + ggplot2::geom_point() 
```

<img src="man/figures/README-example-2.png" width="100%" />

## Acknowledgements

The `ggmice` package is developed with guidance and feedback from Gerko
Vink, Stef van Buuren, Thomas Debray, Valentijn de Jong, Johanna Muñoz,
Thom Volker, Mingyang Cai and Anaïs Fopma. The `ggmice` hex is based on
designs from the `ggplot2` hex and the `mice` hex (by Jaden Walters).

## Code of Conduct

Please note that the `ggmice` project is released with a [Contributor
Code of Conduct](https://amices.org/ggmice/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

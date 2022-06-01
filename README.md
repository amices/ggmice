
<!-- README.md is generated from README.Rmd. Please edit that file and render with devtools::build_readme() -->

# `ggmice` <a href='https://amices.org/ggmice/'><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ggmice)](https://cran.r-project.org/package=ggmice)
[![Total CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/ggmice)](https://cranlogs.r-pkg.org/badges/grand-total/ggmice)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6532702.svg)](https://doi.org/10.5281/zenodo.6532702)

[![GitHub R package
version](https://img.shields.io/github/r-package/v/amices/ggmice?color=yellow&label=dev)](https://github.com/amices/ggmice/blob/main/DESCRIPTION)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/amices/ggmice/workflows/R-CMD-check/badge.svg)](https://github.com/amices/ggmice/actions)
<!-- badges: end -->

## Visualizations for `mice` with `ggplot2`

Enhance a [`mice`](https://amices.org/mice) imputation workflow with
visualizations for incomplete and/or imputed data. The `ggmice`
functions produce
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot) objects which
may be easily manipulated or extended. Use `ggmice` to inspect missing
data, develop imputation models, evaluate algorithmic convergence, or
compare observed versus imputed data.

## Installation

You can install the latest `ggmice` release from
[CRAN](https://CRAN.R-project.org/package=ggmice) with:

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

Inspect the missing data in an incomplete dataset and subsequently
evaluate the imputed data points against observed data. See the [Get
started](https://amices.org/ggmice/articles/ggmice.html) vignette for an
overview of all functionalities. Example data from
[`mice`](https://amices.org/mice/reference/boys).

``` r
# load packages
library(ggplot2)
library(mice)
library(ggmice)
# load some data
dat <- boys
# visualize the incomplete data
ggmice(dat, aes(age, bmi)) + geom_point()
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
# impute the incomplete data
imp <- mice(dat, m = 1, seed = 1, printFlag = FALSE)
# visualize the imputed data
ggmice(imp, aes(age, bmi)) + geom_point() 
```

<img src="man/figures/README-example-2.png" width="100%" />

## Acknowledgements

The `ggmice` package is developed with guidance and feedback from Gerko
Vink, Stef van Buuren, Thomas Debray, Valentijn de Jong, Johanna Muñoz,
Thom Volker, Mingyang Cai and Anaïs Fopma. The `ggmice` hex is based on
designs from the `ggplot2` hex and the `mice` hex (by Jaden Walters).

This project has received funding from the European Union’s Horizon 2020
research and innovation programme under ReCoDID grant agreement No
825746.

## Code of Conduct

You are invited to join the improvement and development of `ggmice`.
Please note that the project is released with a [Contributor Code of
Conduct](https://amices.org/ggmice/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

[![licence](https://img.shields.io/github/license/amices/ggmice?color=blue)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Codecov test
coverage](https://codecov.io/gh/amices/ggmice/branch/main/graph/badge.svg)](https://app.codecov.io/gh/amices/ggmice?branch=main)
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/6036/badge)](https://bestpractices.coreinfrastructure.org/projects/6036)
[![fair-software.eu](https://img.shields.io/badge/fair--software.eu-%E2%97%8F%20%20%E2%97%8F%20%20%E2%97%8F%20%20%E2%97%8F%20%20%E2%97%8F-green)](https://fair-software.eu)

# Plot incomplete or imputed data

Plot incomplete or imputed data

## Usage

``` r
ggmice(data = NULL, mapping = ggplot2::aes())
```

## Arguments

- data:

  An incomplete dataset (of class `data.frame`), or an object of class
  [`mice::mids`](https://amices.org/mice/reference/mids.html).

- mapping:

  A list of aesthetic mappings created with
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).

## Value

An object of class
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html).
The `ggmice` function returns output equivalent to
[`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
output, with a few important exceptions:

- The theme is set to
  [`theme_mice`](http://amices.org/ggmice/reference/theme_mice.md).

- The color scale is set to the
  [`mice::mdc`](https://amices.org/mice/reference/mdc.html) colors.

- The `colour` aesthetic is set to `.where`, an internally defined
  variable which distinguishes observed data from missing data or
  imputed data (for incomplete and imputed data, respectively).

## See also

See the `ggmice` vignette to use the `ggmice()` function on [incomplete
data](https://amices.org/ggmice/articles/ggmice.html#the-ggmice-function)
or [imputed
data](https://amices.org/ggmice/articles/ggmice.html#the-ggmice-function-1).

## Examples

``` r
dat <- mice::nhanes
ggmice(dat, ggplot2::aes(x = age, y = bmi)) + ggplot2::geom_point()

imp <- mice::mice(dat, print = FALSE)
ggmice(imp, ggplot2::aes(x = age, y = bmi)) + ggplot2::geom_point()
```

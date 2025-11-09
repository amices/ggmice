# Scatterplot of observed and imputed data

Scatterplot of observed and imputed data

## Usage

``` r
xyplot(...)
```

## Arguments

- ...:

  Any arguments passed to the function.

## Value

The output of
[mice::xyplot](https://amices.org/mice/reference/xyplot.mids.html) and a
message about the `ggmice` equivalent.

## Examples

``` r
imp <- mice::mice(mice::nhanes, maxit = 1, printFlag = FALSE)
xyplot(imp, bmi ~ age)
#> Hint: Did you know, an equivalent figure can be created with `ggmice()`?
#> For example, to plot 2 variables named 'my_x' and 'my_y' from a mids object
#> called 'my_mids', run:
#>   ggmice(my_mids, ggplot2::aes(x = my_x, y = my_y)) +
#>   ggplot2::geom_point()
#> ℹ See amices.org/ggmice for more info.
#> This message is displayed once per session.
```

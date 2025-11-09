# Box-and-whisker plot of observed and imputed data

Box-and-whisker plot of observed and imputed data

## Usage

``` r
bwplot(...)
```

## Arguments

- ...:

  Any arguments passed to the function.

## Value

The output of
[mice::bwplot](https://amices.org/mice/reference/bwplot.mids.html) and a
message about the `ggmice` equivalent.

## Examples

``` r
imp <- mice::mice(mice::nhanes, maxit = 1, printFlag = FALSE)
bwplot(imp)
#> Hint: Did you know, an equivalent figure can be created with `ggmice()`?
#> For example, to plot a variable named 'my_vrb' from a mids object called
#> 'my_mids', run:
#>   ggmice(my_mids, ggplot2::aes(x = .imp, y = my_vrb)) +
#>   ggplot2::geom_boxplot()
#> ℹ See amices.org/ggmice for more info.
#> This message is displayed once per session.
```

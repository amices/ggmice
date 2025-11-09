# Plot the influx and outflux of a multivariate missing data pattern

Plot the influx and outflux of a multivariate missing data pattern

## Usage

``` r
plot_flux(data, vrb = "all", label = TRUE, caption = TRUE)
```

## Arguments

- data:

  An incomplete dataset of class `data.frame` or `matrix`.

- vrb:

  String, vector, or unquoted expression with variable name(s), default
  is "all".

- label:

  Logical indicating whether variable names should be displayed within
  the plot (the default) or with colors in the legend.

- caption:

  Logical indicating whether the figure caption should be displayed.

## Value

An object of class
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Examples

``` r
# plot flux for all columns
plot_flux(mice::nhanes)


# plot flux for specific columns by supplying a character vector
plot_flux(mice::nhanes, c("chl", "hyp"))


# plot flux for specific columns by supplying unquoted variable names
plot_flux(mice::nhanes, c(chl, hyp))


# plot flux for specific columns by passing an object with variable names
# from the environment, unquoted with `!!`
my_variables <- c("chl", "hyp")
plot_flux(mice::nhanes, !!my_variables)

# object with variable names must be unquoted with `!!`
try(plot_flux(mice::nhanes, my_variables))
#> Error in match_vrb(vrb, names(data)) : 
#>   ✖ The variable name(s) supplied to `vrb` could not be found in `data`.
#> ℹ If you supply an object with variable names from the environment, use `!!` to
#>   unqote:
#>   `vrb = !!my_variables`
```

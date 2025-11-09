# Plot correlations between (incomplete) variables

Plot correlations between (incomplete) variables

## Usage

``` r
plot_corr(
  data,
  vrb = "all",
  label = FALSE,
  square = TRUE,
  diagonal = FALSE,
  rotate = FALSE,
  caption = TRUE
)
```

## Arguments

- data:

  A dataset of class `data.frame`, `tibble`, or `matrix`.

- vrb:

  String, vector, or unquoted expression with variable name(s), default
  is "all".

- label:

  Logical indicating whether correlation values should be displayed.

- square:

  Logical indicating whether the plot tiles should be squares.

- diagonal:

  Logical indicating whether the correlation of each variable with
  itself should be displayed.

- rotate:

  Logical indicating whether the variable name labels should be rotated
  90 degrees.

- caption:

  Logical indicating whether the figure caption should be displayed.

## Value

An object of class
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Examples

``` r
# plot correlations for all columns
plot_corr(mice::nhanes)


# plot correlations for specific columns by supplying a character vector
plot_corr(mice::nhanes, c("chl", "hyp"))


# plot correlations for specific columns by supplying unquoted variable names
plot_corr(mice::nhanes, c(chl, hyp))


# plot correlations for specific columns by passing an object with variable names
# from the environment, unquoted with `!!`
my_variables <- c("chl", "hyp")
plot_corr(mice::nhanes, !!my_variables)

# object with variable names must be unquoted with `!!`
try(plot_corr(mice::nhanes, my_variables))
#> Error in match_vrb(vrb, names(data)) : 
#>   ✖ The variable name(s) supplied to `vrb` could not be found in `data`.
#> ℹ If you supply an object with variable names from the environment, use `!!` to
#>   unqote:
#>   `vrb = !!my_variables`
```

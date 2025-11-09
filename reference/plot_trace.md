# Plot the trace lines of the imputation algorithm

Plot the trace lines of the imputation algorithm

## Usage

``` r
plot_trace(data, vrb = "all", trend = FALSE, legend = TRUE)
```

## Arguments

- data:

  An object of class
  [mice::mids](https://amices.org/mice/reference/mids.html).

- vrb:

  String, vector, or unquoted expression with variable name(s), default
  is "all".

- trend:

  Logical indicating whether a smoothened trend should be added, default
  is FALSE.

- legend:

  Logical indicating whether the plot legend should be visible, default
  is TRUE.

## Value

An object of class
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Details

The `vrb` argument is "quoted" via
[`rlang::enexpr()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
and evaluated according to [tidy evaluation
principles](https://adv-r.hadley.nz/metaprogramming.html). In practice,
this technical nuance only affects users when passing an object from the
environment (e.g., a vector of variable names) to the `vrb` argument. In
such cases, the object must be "unquoted" via the `!!` prefix operator.

## Examples

``` r
# create [mice::mids] object with [mice::mice()]
imp <- mice::mice(mice::nhanes, print = FALSE)

# plot trace lines for all imputed columns
plot_trace(imp)
#> Trace plot could not be produced for variable(s):
#>   age
#> ℹ No convergence diagnostics found.


# plot trace lines for specific columns by supplying a string or character vector
plot_trace(imp, "chl")

plot_trace(imp, c("chl", "hyp"))

# plot trace lines for specific columns by supplying unquoted variable names
plot_trace(imp, chl)

plot_trace(imp, c(chl, hyp))


# plot trace lines for specific columns by passing an object with variable names
# from the environment, unquoted with `!!`
my_variables <- c("chl", "hyp")
plot_trace(imp, !!my_variables)

# object with variable names must be unquoted with `!!`
try(plot_trace(imp, my_variables))
#> Error in match_vrb(vrb, vrbs_in_data) : 
#>   ✖ The variable name(s) supplied to `vrb` could not be found in `data`.
#> ℹ If you supply an object with variable names from the environment, use `!!` to
#>   unqote:
#>   `vrb = !!my_variables`
```

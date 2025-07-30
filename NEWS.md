# ggmice (development version)

# ggmice 0.1.1

## New features

* New function `plot_miss()` visualizes missing data indicator matrix (#123)
* New `plot_trace()` argument `legend` hides plot legend when set to `FALSE` (#165)
* `plot_pred()` now allows `mice::mids` objects as input data type (#132)
* New `plot_trace()` argument `trend` adds a trend line to plot when set to `TRUE` (#140)

## Bug fixes

* `plot_pred()` now correct labels predictor matrix entries `-3`, `3` and `4` as 'exclusion-restriction', 'fixed effect' and 'random effect' variables respectively (#128, #169)
* All `plot_*()` functions now parse `vrb` argument to recognize variable name(s) from object in global environment using `!!` notation (#157)

## Minor changes

* Miscellaneous documentation and vignette updates (#128, #165, #169)

---

# ggmice 0.1.0

## Breaking changes

* Default plotting behavior `plot_pattern()` creates missing data pattern plot with square tiles (#74)
* Default plotting behavior `plot_pred()` creates predictor matrix plot with colored tiles and shows predictor values (#71, #74)

## New features

* New optional argument `plot_pred()` shows methods vector with predictor matrix plot (#71)
* New optional argument `plot_pattern()` hides less frequent patterns (#77)
* New optional argument `plot_pattern()` hides legend caption (#111)

## Bug fixes 

* The `ggmice()` function now displays all imputed data (incl. over-imputed data; #54)
* The `ggmice()` function now catches errors when `mapping` input contains log-transformation (#80)
* The family of `plot_*` functions now handle vector inputs for `vrb` argument (#80)
* The family of `plot_*` functions now handle `matrix` inputs for `data` argument (#108)
* The family of `plot_*` functions now omit grid lines from tile plots (#35)
* The `plot_pattern()` function now handles any `data.frame` input for `data` argument (#38, #77, #104, #112)

## Minor changes

* Input validation for `data` argument `plot_*` functions (#85)
* Input validation for `vrb` argument `plot_*` functions (#80)
* Input validation for `mapping` argument `ggmice()` (#34, #90)
* Vignette updates (#31, #35, #38) and other documentation (#45, #51)
* The `plot_pattern()` function creates missing data pattern plot with more informative labels (#59, #111)

---

# ggmice 0.0.1

* First release of the package.

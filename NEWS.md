# `ggmice 0.1.0`

## Breaking changes

* breaking changes in defaults `plot_pred()` and `plot_pattern()` #33, #73

## New features

* `plot_pred()` has more arguments and different color scheme (#37)
* new argument `plot_pattern()` to hide less frequent patterns (#75)
* new feature: `plot_variance()` (#48, PR #56)

## Bug fixes 

* input validation for `data` and `vrb` arguments: error message about log transformations (#34), variable selection actually works (#41, #42, #63), input validation (#85)
* Make jitter work with incomplete categorical variables bug #25
* `plot_pattern()` now works with variable called 'y' (#36) and complete data (#44) and completely missing variable (#103).
* `ggmice()` now displays all imputed data, not just the filled in missing data (i.e. also over-imputed data; #39, PR #54) and is able to match variable names more effectively (#89, PR #90)

## Minor changes

* Vignette updates (PRs #31, #35, #38) and other documentation (PRs #45, #51)
* grid lines #32
* renamed axes `plot_pattern()` #57

---

# `ggmice 0.0.1`

* First release of the package.

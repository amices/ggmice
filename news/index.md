# Changelog

## ggmice (development version)

## ggmice 0.1.1

CRAN release: 2025-07-30

### New features

- New function
  [`plot_miss()`](http://amices.org/ggmice/reference/plot_miss.md)
  visualizes missing data indicator matrix
  ([\#123](https://github.com/amices/ggmice/issues/123))
- New [`plot_trace()`](http://amices.org/ggmice/reference/plot_trace.md)
  argument `legend` hides plot legend when set to `FALSE`
  ([\#165](https://github.com/amices/ggmice/issues/165))
- [`plot_pred()`](http://amices.org/ggmice/reference/plot_pred.md) now
  allows [`mice::mids`](https://amices.org/mice/reference/mids.html)
  objects as input data type
  ([\#132](https://github.com/amices/ggmice/issues/132))
- New [`plot_trace()`](http://amices.org/ggmice/reference/plot_trace.md)
  argument `trend` adds a trend line to plot when set to `TRUE`
  ([\#140](https://github.com/amices/ggmice/issues/140))

### Bug fixes

- [`plot_pred()`](http://amices.org/ggmice/reference/plot_pred.md) now
  correct labels predictor matrix entries `-3`, `3` and `4` as
  ‘exclusion-restriction’, ‘fixed effect’ and ‘random effect’ variables
  respectively ([\#128](https://github.com/amices/ggmice/issues/128),
  [\#169](https://github.com/amices/ggmice/issues/169))
- All `plot_*()` functions now parse `vrb` argument to recognize
  variable name(s) from object in global environment using `!!` notation
  ([\#157](https://github.com/amices/ggmice/issues/157))

### Minor changes

- Miscellaneous documentation and vignette updates
  ([\#128](https://github.com/amices/ggmice/issues/128),
  [\#165](https://github.com/amices/ggmice/issues/165),
  [\#169](https://github.com/amices/ggmice/issues/169))

------------------------------------------------------------------------

## ggmice 0.1.0

CRAN release: 2023-08-07

### Breaking changes

- Default plotting behavior
  [`plot_pattern()`](http://amices.org/ggmice/reference/plot_pattern.md)
  creates missing data pattern plot with square tiles
  ([\#74](https://github.com/amices/ggmice/issues/74))
- Default plotting behavior
  [`plot_pred()`](http://amices.org/ggmice/reference/plot_pred.md)
  creates predictor matrix plot with colored tiles and shows predictor
  values ([\#71](https://github.com/amices/ggmice/issues/71),
  [\#74](https://github.com/amices/ggmice/issues/74))

### New features

- New optional argument
  [`plot_pred()`](http://amices.org/ggmice/reference/plot_pred.md) shows
  methods vector with predictor matrix plot
  ([\#71](https://github.com/amices/ggmice/issues/71))
- New optional argument
  [`plot_pattern()`](http://amices.org/ggmice/reference/plot_pattern.md)
  hides less frequent patterns
  ([\#77](https://github.com/amices/ggmice/issues/77))
- New optional argument
  [`plot_pattern()`](http://amices.org/ggmice/reference/plot_pattern.md)
  hides legend caption
  ([\#111](https://github.com/amices/ggmice/issues/111))

### Bug fixes

- The [`ggmice()`](http://amices.org/ggmice/reference/ggmice.md)
  function now displays all imputed data (incl. over-imputed data;
  [\#54](https://github.com/amices/ggmice/issues/54))
- The [`ggmice()`](http://amices.org/ggmice/reference/ggmice.md)
  function now catches errors when `mapping` input contains
  log-transformation
  ([\#80](https://github.com/amices/ggmice/issues/80))
- The family of `plot_*` functions now handle vector inputs for `vrb`
  argument ([\#80](https://github.com/amices/ggmice/issues/80))
- The family of `plot_*` functions now handle `matrix` inputs for `data`
  argument ([\#108](https://github.com/amices/ggmice/issues/108))
- The family of `plot_*` functions now omit grid lines from tile plots
  ([\#35](https://github.com/amices/ggmice/issues/35))
- The
  [`plot_pattern()`](http://amices.org/ggmice/reference/plot_pattern.md)
  function now handles any `data.frame` input for `data` argument
  ([\#38](https://github.com/amices/ggmice/issues/38),
  [\#77](https://github.com/amices/ggmice/issues/77),
  [\#104](https://github.com/amices/ggmice/issues/104),
  [\#112](https://github.com/amices/ggmice/issues/112))

### Minor changes

- Input validation for `data` argument `plot_*` functions
  ([\#85](https://github.com/amices/ggmice/issues/85))
- Input validation for `vrb` argument `plot_*` functions
  ([\#80](https://github.com/amices/ggmice/issues/80))
- Input validation for `mapping` argument
  [`ggmice()`](http://amices.org/ggmice/reference/ggmice.md)
  ([\#34](https://github.com/amices/ggmice/issues/34),
  [\#90](https://github.com/amices/ggmice/issues/90))
- Vignette updates ([\#31](https://github.com/amices/ggmice/issues/31),
  [\#35](https://github.com/amices/ggmice/issues/35),
  [\#38](https://github.com/amices/ggmice/issues/38)) and other
  documentation ([\#45](https://github.com/amices/ggmice/issues/45),
  [\#51](https://github.com/amices/ggmice/issues/51))
- The
  [`plot_pattern()`](http://amices.org/ggmice/reference/plot_pattern.md)
  function creates missing data pattern plot with more informative
  labels ([\#59](https://github.com/amices/ggmice/issues/59),
  [\#111](https://github.com/amices/ggmice/issues/111))

------------------------------------------------------------------------

## ggmice 0.0.1

CRAN release: 2022-03-17

- First release of the package.

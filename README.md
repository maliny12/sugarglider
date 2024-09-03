
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sugarglider

<!-- badges: start -->

[![R-CMD-check](https://github.com/maliny12/glyph/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/maliny12/glyph/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

### Overview

ribbon provides ggplot2 extension to produce glyph plots with
`geom_glyph_ribbon()` and `geom_glyph_segment()`.

These functions create a ribbon or segment geometry designed to display
glyphs based on the combination of `x_major` and `y_major`. For each
`x_minor` value, `geom_glyph_ribbon()` displays a y interval defined by
`ymin_minor` and `ymax_minor`. Meanwhile, `geom_glyph_segment()` draw a
straight line between `y_minor` and `yend_minor` with respect to
`x_minor`.

<a href="https://maliny12.github.io/ribbon/articles/Examples.html#overview"><img src="https://maliny12.github.io/ribbon/articles/Examples_files/figure-html/unnamed-chunk-2-1.png" alt="Global vs. Local Rescale" width="1457"/></img></a>

### Installation

``` r
# Download the development version from GitHub:
devtools::install_github("maliny12/ribbon")
```

### Usage

See the
[examples](https://maliny12.github.io/ribbon/articles/Examples.html)
page to learn more about how to use ribbon in your project.

### Examples

Click one of the images below to go to see the code example:

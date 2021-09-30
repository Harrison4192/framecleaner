
<!-- README.md is generated from README.Rmd. Please edit that file -->

# framecleaner

<!-- badges: start -->

[![R-CMD-check](https://github.com/Harrison4192/framecleaner/workflows/R-CMD-check/badge.svg)](https://github.com/Harrison4192/framecleaner/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/framecleaner)](https://CRAN.R-project.org/package=framecleaner)

[![](http://cranlogs.r-pkg.org/badges/grand-total/framecleaner?color=blue)](https://cran.r-project.org/package=framecleaner)
[![](https://img.shields.io/github/languages/code-size/Harrison4192/framecleaner.svg)](https://github.com/Harrison4192/framecleaner)
[![](https://img.shields.io/github/last-commit/Harrison4192/framecleaner.svg)](https://github.com/Harrison4192/framecleaner/commits/main)
<!-- badges: end -->

framecleaner provides a tidyverse-friendly interface for modifying data
frames with a sequence of piped commands. These commands wrap
[dplyr](https://dplyr.tidyverse.org/index.html) mutate statements in a
convenient way to concisely solve common issues that arise when tidying
small to medium data sets. It also includes smart defaults and allows
selection of columns via
[tidyselect](https://tidyselect.r-lib.org/reference/index.html).

## Installation

You can install the released version of framecleaner from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("framecleaner")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Harrison4192/framecleaner")
```

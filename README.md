
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clessnverse

<!-- badges: start -->

[![R-CMD-check](https://github.com/clessn/clessnverse/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/clessn/clessnverse/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

clessnverse contains functions for data domestication, analysis and
visualization along with functions specific to the research chair’s
projects.

*Note: This package is under construction.*

## Installation

To install the latest stable version of this package, run the following
line in your R console:

``` r
remotes::install_github("clessn/clessnverse")
```

## Usage

`library("clessnverse")` will load the following packages:

- [hublot](https://github.com/clessn/hublotr), to access clHub
- [clessnhub](https://github.com/clessn/hublotr), to access clessn hub

## Issues and suggestions

You can submit bugs or suggestions in the Issues tab of this repo. To
facilitate problem solving, please include a [minimal reproducible
example](https://reprex.tidyverse.org/articles/reprex-dos-and-donts.html)
of the issue.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(clessnverse)
#> Registered S3 method overwritten by 'tictoc':
#>   method     from 
#>   print.List rlist
## basic example code
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

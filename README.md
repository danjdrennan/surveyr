---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# surveyr

<!-- badges: start -->
[![R-CMD-check](https://github.com/danjdrennan/surveyr/workflows/R-CMD-check/badge.svg)](https://github.com/danjdrennan/surveyr/actions)
[![Codecov test coverage](https://codecov.io/gh/danjdrennan/surveyr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/danjdrennan/surveyr?branch=main)
<!-- badges: end -->

The goal of surveyr is to ...

## Installation

You can install the development version of surveyr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("danjdrennan/surveyr")
```

## Example

This is a basic example which shows you how to solve a common problem:


```r
library(surveyr)
# Make a synthetic dataset for now
set.seed(1)
d <- tibble::tibble(
    region = rep(1:5, 10),
    y = rnorm(50, 50*region, 5*sqrt(region))
)
N <- rpois(5, 50)
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:


```r
d %>% make_summary(region, y, N)
#> # A tibble: 5 x 7
#>   region     N     n point    var    se     cv
#>    <int> <dbl> <int> <dbl>  <dbl> <dbl>  <dbl>
#> 1      1    52    10  50.5  14.9   3.86 0.0766
#> 2      2    45    10 101.    5.43  2.33 0.0231
#> 3      3    44    10 151.   37.9   6.15 0.0409
#> 4      4    60    10 200.  131.   11.5  0.0573
#> 5      5    64    10 253.   65.1   8.07 0.0319
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-plot_data-1.png" title="plot of chunk plot_data" alt="plot of chunk plot_data" width="100%" />

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.

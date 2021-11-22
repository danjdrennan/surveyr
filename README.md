# surveyr

[![Codecov test coverage](https://codecov.io/gh/danjdrennan/surveyr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/danjdrennan/surveyr?branch=main)

An R package for working with survey data.

## Intended Use

The package is intended for use in an education setting working with survey data.
It provides tools for computing statistics from surveys using simple random
sampling, stratified, and clustered surveying designs.
Plotting is simplified with a thin wrapper on ggplot2 functions, enabling a user
to easily create aesthetic graphs.
Examples of generating samples are also made available.


## Installation

Installation should be done via `devtools` in R.
Run the following code to install via GitHub.

```{r}
devtools::install_github("danjdrennan/surveyr")
```

## Complete

* Implementations for computing SRS statistics implemented with tests (11/21)

    - 13 functions with 11 test suites and 47 passing tests
    
    - Requires refactoring to simplify error messages in `total` functions
    
* Written tests verifying imports work as expected in the `NAMESPACE`

    - These cover 3 test suites with 7 passing tests

## TODO

* Verify if utils needs to be imported to package or not (to use `combn`)

* Core statistical functions:

    - Stratified sampling
    
    - Cluster sampling
    
    - Sample size calculations
    
* Main functions to expose to users:

    - `make_summary` (see `dev/example-make_summary.R` for example goal)
    
    - `estimate_ci`: a meta function CIs for a stat at a confidence level
    
    - `make_samples` to make sampling distribution for small populations
    (need `utils::combn` for this functionality)
    
* Demos (Law of large numbers and CLT) to produce graphs

* Tests/documentation for exposed functions

* Vignette demonstrating how to use aspects of the package

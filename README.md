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

Install using `devtools`:

```{r}
devtools::install_github("danjdrennan/surveyr")
```

The package is not available through CRAN.


## November 2021 Update

**Last updated:** 2021-11-23

### Overview

**3 functions fully written, tested, documented, and exposed in the package**

* `surveyr::draw_samples` to find all SRS samples from a finite population

* `surveyr::sampling_distribution` to compute sampling distribution

* `mk_stat` to compute a point estimate together with its variance, standard
error, and coefficient of variation

**Reach 100% test coverage on 71 passing tests / 8 scripts / ~20 functions**

Encapsulates 

* ~370 lines of development code and documentation

* ~320 lines of tests

* ~90 lines of experimental code (in dev folder and `.Rbuildignore`)

* 780 lines of code in total

* 52 commits

**`codecov` is tracking code coverage:**

```{url}
https://app.codecov.io/gh/danjdrennan/surveyr/
```

### SRS Function Implementations

`R/SRSfunctions.R` fully implemented with tests (2021-11-23)

* 14 functions, 24 test suites (~2/function for error and correctness checks),
and 55 passing tests

* Expose core functions to user through `mk_stat` function

* Fully documented `mk_stat` with tests
    
**TODO:**

* Rewrite error functions with better error messages (time permitting)

* Rewrite error handlers for variance functions (time permitting)


### Imports / NAMESPACE Requirements

Written tests verifying imports work as expected in the `NAMESPACE`

* Tested with 3 test suites and 7 passing tests

**TODO:**


### Probability Demonstrations

Implemented a function to compute the sampling distribution of small, finite
populations to demonstrate effect of CLT on sampling distributions

* Makes available `draw_samples` and `sampling_distribution` for users
    to experiment with different sampling distributions
    
**TODO:**
    
Write a plotting wrapper for `sampling_distribution` to plot a finite
population together with its sampling distribution

* Unit tests for plotting functions (ref `tidyverse/ggplot2` docs)

* Goal is to demonstrate the CLT with different distributions

* The examples in documentation will be key in this part

Write a function demonstrating the law of large numbers

* Unit tests for plotting functions (ref `tidyverse/ggplot2` docs)

* Essentially will simulate the strong law of large numbers with iid data

* Documentation examples will be key in this part


## TODO

### Probability demos (required)

Implement a LLN demo

Implement plotting functions with tests


### Stratified sampling (required)

Will primarily be exposed through `make_summary` (corresponding script)

* Stratified SRSs combine the results developed in `mk_stat`

* Need to specify `make_summary` in light of current progress

* `dev/example-make_summary.R` has target output to develop to
(numerically wrong due to method of computing statistics, but that's the goal)

* Will need a modification of user input data to handle known population sizes

* Will need a summary function to combine tabulated data


### Inference (required)

Implement a method for computing confidence intervals for point estimates

* Needs to optionally use t intervals or z intervals


### Sample sizes (time permitting)

Implementations for computing minimum sample sizes given a standard error and
target margin of error.


### Ratio/regression estimation (time permitting)

Implement functions to compute ratio/regression estimates for survey data using
simple random sample designs.


### Cluster sampling estimation (time permitting)

Implement functions to manage cluster sampling in one- and two-stage sampling
designs.


### Wrapping Up

Write package vignettes demonstrating basic use and functionality

Maintain current test coverage levels

* Refactor code in SRS functions to make more readable error handlers

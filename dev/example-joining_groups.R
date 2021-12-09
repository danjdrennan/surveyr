## Experimenting with how to match known population sizes to a dataset; the case
## is similar when weights are not part of the dataset. One easy way to do this
## is by creating an auxiliary table and then applying a left- or inner-join on
## the dataset. We can assume the number of categories matches the number of
## subgroups in the stratum as a simplification, which is entirely reasonable for
## the intended use case.
##
## In principle there are two cases with group data to consider:
## (1) the population sizes or weights are included in the dataset;
## (2) the weights are known but are not attached to the data at call time.
## The two alternatives are best handled by two functions most likely.
## The first case is quite simple, but requires quoting and unquoting the variable
## of interest. THe second case is more difficult, but can be solved using the
## code examples shown below. This efficiently matches the population sizes or
## weights to the correct group variables. In either case, it assumes the weights
## or population sizes are uniquely associated with the stratum so that a join
## correctly matches values. If this is not the case, then the user would need to
## pass a dataset where the weights were part of the passed data frame anyhow.
library(dplyr)
library(forcats)
library(ggplot2)
library(tidyr)
library(lazyeval)

set.seed(1)
# First make a synthetic dataset to play with.
# The random variable y can be anything in this case, we just want to know that
# there is an observation variable in a column to use.
d <- tibble(stratum = rep(1:5, 4), y = round(rnorm(4*5, 30, 3), 1))
d

# Now make a synthetic set of population sizes to match with the number of strata
# in the synthetic data. This matches what we'd expect as an input argument.
N <- 10 * rpois(5, 15)
# Create an auxiliary table to use in a join.
tn <- tibble(stratum = 1:5, N)
tn

left_join(d, tn, "stratum") -> d
d

d %>% group_by(stratum) %>%
    summarize(surveyr::mk_stat(y, N=N))

d %>% group_by(stratum) %>% mutate(w = N / n()) %>%
    summarize(
        n=n(),
        N = sum(w),
        surveyr::mk_stat(y, stat="mean", N=N, fpc=TRUE)
    )


d %>% group_by(stratum) %>% summarize(m = mean(y), v = var(y)) %>%
    as.data.frame

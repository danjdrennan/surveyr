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

# First make a synthetic dataset to play with.
# The random variable y can be anything in this case, we just want to know that
# there is an observation variable in a column to use.
d <- tibble(stratum = rep(1:5, 2), y = rnorm(10, 30, 3))
d

# Now make a synthetic set of population sizes to match with the number of strata
# in the synthetic data. This matches what we'd expect as an input argument.
n <- 100 * rpois(5, 15)
# Create an auxiliary table to use in a join.
tn <- tibble(stratum = 1:5, n)
tn

# Now compare the left join with the auxiliary population data.
# The console output shows these matching as we'd hope.
left_join(d, tn, by = "stratum")


# The last example is fine for a factor using numbers because we can match on
# the index. Can it be replicated for letters or strings?

# The main question is if the ordering of the unique factors is permuted when
# finding the unique levels of the factor. If not, then we can simply construct
# the auxiliary table with that factor information and apply the same method.
animals <- c("frog", "cat", "dog", "bird", "fish")
unique(animals)
as_factor(animals)
# From console, this doesn't change the order of the animals in the index.

# Recreate the table now with a factor
d <- tibble(animals = as_factor(rep(animals, 2)), y = d$y)

tn <- tibble(animals=as_factor(animals), n=n)
tn
left_join(d, tn, )

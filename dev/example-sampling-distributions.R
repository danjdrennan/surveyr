# Inspiration demo for visualizing sampling distributions
# This code was originally used as a demo for generating a sampling distribution
# and visualizing the estimators found from it.
# Parts of this code will need to be imported into the package with tests
# to prove correctness. They provide a good demo for the CLT when visualizing
# sampling distributions
library(tidyverse)

N <- 30
n <- 4

mkSamplingDistribution <- function(n, N, random_dist, ...){
  # The function generates all samples of size n from a population of size N
  # using the combn function, which outputs all possible samples to an array.
  # Large combinations can be used in the function, but it does not handle very
  # large populations or combinations with very large sample sizes.
  
  # dist must be one of the random distributions
  # The ellipsis allows for optional arguments to use with dist
  # Create a population to sample from and generate statistics from
  
  # Generate the population data (using size N)
  population_data <- random_dist(N, ...)
  
  # Create all SRS's of size n
  samples <- combn(1:N, n, simplify = T)
  
  matrix(population_data[samples], ncol(samples), nrow(samples), byrow=T) %>%
    rowMeans() -> sampling_dist
  
  return(list(
    population = as.data.frame(population_data),
    sampling_distribution = as.data.frame(sampling_dist)
  ))
}

plot_population <- function(population){
  ggplot(population) + 
    geom_histogram(aes(x = population_data), color='gray', bins=20)
}

plot_samping_distribution <- function(sampling_dist, pop_mean){
  ggplot(sampling_dist) + 
    geom_histogram(aes(x = sampling_dist), color='gray', bins=20) +
    geom_vline(xintercept = pop_mean, color="red")
}

sampling_dist <- mkSamplingDistribution(n, N, runif, min = 0, max = 100)

sampling_dist$population %>% plot_population()

pop_mean <- mean(sampling_dist$population$population_data)

sampling_dist$sampling_distribution %>% 
  plot_samping_distribution(pop_mean = pop_mean)

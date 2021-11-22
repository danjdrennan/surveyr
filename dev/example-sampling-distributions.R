# Inspiration demo for visualizing sampling distributions
# This code was originally used as a demo for generating a sampling distribution
# and visualizing the estimators found from it.
# Parts of this code will need to be imported into the package with tests
# to prove correctness. They provide a good demo for the CLT when visualizing
# sampling distributions.

# Now try the surveyr implementation
library(surveyr)
library(dplyr)
library(ggplot2)
N <- 20
y <- rnorm(N, 30, 3)
sampling_dist <- sampling_distribution(y, 6)
df <- as_tibble(cbind(
    "pop" = c(
        rep("population", N),
        rep("sampling_distribution", length(sampling_dist))
    ),
    "y" = c(y, sampling_dist)
))
df["y"] <- as.double(df$y)
df
ggplot(df, aes(x = y)) +
    geom_histogram(aes(y=..density..), color="gray", fill="4e5c68") +
    geom_density(color = "blue", size=1) +
    facet_wrap(~pop) +
    labs(
        title="Example finite population",
        x = "y",
        y = "density"
    ) +
    theme_bw()

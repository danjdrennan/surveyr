# This script runs an example of what we eventually want to produce in make_summary
# The goal is to simplify the stat args so a user can simply pass the data with
# the desired stats to compute.
# The variances and standard errors would be produced automatically based on
# sample inputs.
# The example below is over simplified to get to what the outputs should look
# like.
# The actual computations would deviate from these values most likely.

# Make synthetic data representing a stratified sample
cbind("stratum" = rep(1:5, 20), "y" = rnorm(100)) %>%
    as_tibble() %>%
    mutate(y = stratum + y * sqrt(stratum)) ->
    d

# Simple plot of the data
ggplot(d, aes(x = as_factor(stratum), y = y, fill = as_factor(stratum))) +
    geom_boxplot() +
    labs(
        x = "Stratum",
        y = "y",
        legend = "Stratum"
    ) + theme_bw()

# Assuming a user called make_summary(d, stats = mean), this would be the kind
# of output the user should expect.
group_by(d, stratum) %>%
    summarize(
        n = n(),
        m = sum(y),
        var = var(y),
        sd=sd(y),
        se = sqrt(var/n)
    )

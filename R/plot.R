#' Simple plotting tools
#'
#' @description
#' A simple interface for plotting data using `ggplot2` as a backend.
#' Provides support for plotting histograms of simple random samples or colorized
#' boxplots for stratified samples. Optionally, the plot for a stratified sample
#' can be changed to a colorized histogram. Much greater flexibility is available
#' through `ggplot2`, but this is a helpful starter tool for using that library.
#'
#' In histograms, bins are automatically computed using Doane's formula [1]. This
#' is an immutable default choice for this implementation. Using `ggplot`
#' directly will provide greater flexibility for specifying bin widths and other
#' options.
#'
#' @param .data
#' A tibble dataset.
#' @param .y
#' A column of numeric observations in .data
#' @param .group
#' Optional. A column of population subgroups in .data
#' @param kind
#' Optional. Either "hist" or "histogram". This argument should only be passed
#' when a .group is passed in the function. It will throw an error when .group
#' is not specified with it.
#'
#' @return
#' A ggplot object (graph)
#'
#' @references
#' [1] \cite{https://en.wikipedia.org/wiki/Histogram#Doane's_formula}
#'
#' @export
#'
#' @examples
#' d <- tibble::tibble(s = rep(1:3, 30), y = rnorm(90, 30*s, 5*sqrt(s)))
#' s_plot(d, y, s, kind = "histogram") + ggplot2::labs(
#' title = "My cool plot",
#' subtitle = "These can be entirely customized as ever you'd like",
#' caption = "See more from ggplot help files",
#' x = "An observed variable (with what units)",
#' y = "Frequencies",
#' legend = "Some group variable"
#' ) + ggplot2::theme_bw()
s_plot <- function(.data, .y, .group=NULL, kind=NULL){
    # Make sure a tibble was passed
    stopifnot(class(.data)[2] == "tbl")
    match.arg(kind, c(NULL, "hist", "histogram"))
    # Quote arguments to .y
    # All optional arguments need to be handled in a conditional tree
    .y <- enquo(.y)
    .y_nm <- as_label(.y)

    # kind is handled in cases. When kind is NULL, we must provide a default
    # behavior that is in some sense desirable for a user.
    if(is.null(kind)){
        # Main decision factor is whether .group was passed or not. When .group
        # is not passed, assume an SRS and plot a histogram

        # To suppress the annoying default from ggplot for bins, compute some
        # optimal number of bins
        nb <- .bins(.data, !!.y)

        if(missing(.group)){
            .data %>%
                ggplot2::ggplot(ggplot2::aes(x = !!.y)) +
                ggplot2::geom_histogram(bins = nb, color = "gray") +
                ggplot2::labs(
                    title = "Histogram of data",
                    x = .y_nm,
                    y = "Observed Frequencies"
                ) -> G
        }else{
            .group <- enquo(.group)
            .group_nm <- as_label(.group)

            # First need to make sure the passed .group variable is a factor
            .data %>% mutate(!!.group_nm := as_factor(!!.group)) %>%
                # now on to plotting
                ggplot2::ggplot(ggplot2::aes(y = !!.y, fill = !!.group)) +
                ggplot2::geom_boxplot() +
                ggplot2::labs(
                    title = "Boxplot of data by group",
                    x = .group_nm,
                    y = .y_nm
                ) -> G
        }
    }else{
        if(missing(.group))
            stop("kind should only be specified when .group is used")

        # Quote the .group variable for use in plotting
        .group <- enquo(.group)
        .group_nm <- as_label(.group)

        # As before, need to compute bins to suppress the default behavior
        nb <- .bins(.data, !!.y)

        # First need to make sure the passed .group variable is a factor
        .data %>% mutate(!!.group_nm := as_factor(!!.group)) %>%
            # now on to plotting
            ggplot2::ggplot(ggplot2::aes(x = !!.y, fill = !!.group)) +
            ggplot2::geom_histogram(bins = nb, color = "gray") +
            ggplot2::labs(
                title = "Boxplot of data by group",
                x = .y_nm,
                y = "Observed Frequencies"
            ) -> G
    }
    return(G)
}

.cubed_diff <- function(.y){
    m <- mean(.y)
    sum((.y - m)^3)
}

# Compute number of bins for a histogram according to Doane's formula:
# https://en.wikipedia.org/wiki/Histogram#Doane's_formula
# The choice of Doane's formula is somewhat arbitrary, but it should be useful
# since it will account for skewness in the plotting data as part of the
# formula and may not default to an extremely small number.
.bins <- function(.data, .y){
    # Quote the variable to use in dplyr functions
    .y <- enquo(.y)
    # get values needed to estimate bin count
    .data %>% summarize(
        n=n(),
        m=mean(!!.y),
        s=sd(!!.y),
        k=.cubed_diff(!!.y)
    ) -> S
    sk <- sqrt(6*(S$n-2) / ((S$n + 1)*(S$n + 3)))
    n_bins <- 1 + log2(S$n) + log2(1 + abs(S$k) / sk) %>% round()
    return(n_bins)
}


## The following documentation provides support for how to handle user arguments
## The preference will be to use weights
## in dplyr functions:
## https://tidyeval.tidyverse.org/dplyr.html


#' Summarize data from a stratified random sample
#'
#' @description
#' Support for computing tables of statistics for stratified variables.
#' Wraps `mk_stat` for each subgroup (stratum).
#' Only supports stratification by one variable at this time.
#'
#' @param .data
#' A dataframe or tibble of data
#'
#' @param .group
#' A stratifying variable as a column in the data
#'
#' @param .y
#' An observation variable in the data
#'
#' @param .group_N
#' An exogeneous vector of population sizes for each subgroup to join to the
#' data when computing stratified effects.
#'
#' @param .stat
#' One of "mean", "total" or "prop"
#'
#' @param .group_weights
#' Optional column of weights in the dataframe.
#' If weights are given in the dataframe, they will be used in place of the
#' population vector (.group_N), although .group_N is still a required argument.
#'
#' @param .fpc
#' Logical variable: indicates whether to compute a finite popoulation correction.
#'
#' @return
#' A tibble (`data.frame`-like structure)
#'
#' @export
#'
#' @examples
#' d <- data.frame("stratum"=rep(1:5, 6), "y"=rnorm(30, 30, 3))
#' N <- 50
#' make_summary(d, stratum, y, N)
make_summary <- function(
    .data,
    .group,
    .y,
    .group_N,
    .stat="mean",
    .group_weights=NULL,
    .fpc=TRUE
){
    # First need to quote all data args to pass to dplyr functions
    .group <- enquo(.group)
    .y <- enquo(.y)
    .group_N <- enquo(.group_N)
    # Need to know if .group_weights was passed
    .gw_missing <- missing(.group_weights)
    .group_weights <- enquo(.group_weights)

    # Also need names to check all args with
    .group_nm <- as_label(.group)
    .y_nm <- as_label(.y)
    .group_N_nm <- as_label(.group_N)
    .group_weights_nm <- as_label(.group_weights)

    # Compatibility checks up to .group_weights are done before handling weights

    # Require a data.frame or child of data.frame arg
    if(!("data.frame" %in% class(.data)))
        stop("data must be of type data.frame")

    # .stat must match one of the calls to mk_stat
    match.arg(.stat, c("mean", "total", "prop"))

    # .fpc must be logical
    is.logical(.fpc)

    # .group_N is assumed to be either a numeric scalar or a vector such that
    # length(.group_N) = length(unique(.data$.group))
    stopifnot(
        is.numeric(lazyeval::f_eval(.group_N)),
        length(lazyeval::f_eval(.group_N)) %in% c(
            1,
            length(unique(.data[[.group_nm]]))
        )
    )
    # .group_weights, when passed, is assumed to be a column in the data
    # dplyr will handle errors for the column being undefined
    if(.gw_missing){
        # Assume proportional allocation to groups when weights aren't passed
        # Impute weights accordingly
        wt <- .make_weights_table(.data, !!.group, !!.group_N)

        left_join(.data, wt, by=.group_nm) %>%
            group_by(!!.group) %>%
            mutate(w = N / n()) -> .data
    } else{
        names(.data[[.group_weights_nm]]) = "w"
    }
    .data %>% group_by(!!.group) %>%
        summarize(
            N = sum(w),
            mk_stat(!!.y, stat=.stat, N=N, fpc=.fpc, weights=w)
        ) -> .data_summary
    return(.data_summary)
}

# See part 8.1.3 (block 4 of code):
# https://tidyeval.tidyverse.org/dplyr.html
.make_weights_table <- function(.data, .group, .N){
    .group <- enquo(.group)
    .N <- enquo(.N)
    .N_nm <- as_label(.N)
    .data %>% select(!!.group) %>% unique %>% mutate(!!.N_nm := !!.N)
}

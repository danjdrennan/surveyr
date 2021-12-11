#' Estimate the stratified statistic from a stratified sample dataset
#'
#' @description
#' Helper to compute stratified estimates for means, totals, and proportions.
#' Composes tables from `make_summary` to estimate the stratified statistic.
#'
#' @param .tbl
#' Output from (or similar to) `make_summary`
#'
#' @param .stat
#' One of "mean", "total", or "prop"
#'
#' @param .fpc
#' Logical to use or exclude a finite population correction
#'
#' @return
#' A tibble table combining components of a stratified estimator and providing
#' estimates for
#' \item{Ntotal}{The total population size}
#' \item{ntotal}{The total sample size}
#' \item{point}{The point estimate (stat) provided}
#' \item{var}{The variance of the point estimator}
#' \item{se}{The standard error}
#' \item{cv}{The coefficient of variation}
#'
#' @references
#' Lohr, Sharon L. Sampling: Design and Analysis. Chapman and Hall/CRC, 2019.
#'
#' @export
#'
#' @examples
#' # First need to get output from make_summary
#' d <- data.frame("stratum"=rep(1:5, 6), "y"=rnorm(30, 30, 3))
#' N <- 50
#' ms <- make_summary(d, stratum, y, N, .stat="total")
#' stratified_stat(ms, .stat="total")
#' # Can equivalently pipe the output from make_summary into stratified_stat
#' # (Note how easily the estimated statistic can be computed)
#' make_summary(d, stratum, y, N, .stat="mean") %>%
#' stratified_stat(.stat="mean")
stratified_stat <- function(.tbl, .stat="mean", .fpc=TRUE){
    # Input type checks
    stopifnot(class(.tbl)[2] == "tbl", is.logical(.fpc))
    match.arg(.stat, c("mean", "total", "prop"))

    # Helper functions deal with the fpc so that two nearly equivalent summaries.
    # The standardized format from make_summary makes computations in the helper
    # functions simple to integrate and report.
    if(.fpc){
        stratified_estimate <- .stratified_fpc(.tbl, .stat)
    } else{
        stratified_estimate <- .stratified_no_fpc(.tbl, .stat)
    }
    return(stratified_estimate)
}

.stratified_fpc <- function(.tbl, .stat){
    if(.stat == "total"){
        .tbl %>% ungroup() %>%
            summarize(
                Ntotal = sum(N),
                ntotal = sum(n),
                point = sum(point),
                var = sum((1 - n/N) * N^2 * var / n),
                se = sqrt(var),
                cv = se / point
            ) -> stratified_stat
    } else{
        .tbl %>% ungroup() %>%
            summarize(
                Ntotal = sum(N),
                ntotal = sum(n),
                point = sum(N / Ntotal * point),
                var = sum((1 - n/N) * (N/Ntotal)^2 * var / n),
                se = sqrt(var),
                cv = se / point
            ) -> stratified_stat
    }
    return(stratified_stat)
}

.stratified_no_fpc <- function(.tbl, .stat){
    if(.stat == "total"){
        .tbl %>% ungroup() %>%
            summarize(
                Ntotal = sum(N),
                ntotal = sum(n),
                point = sum(point),
                var = sum(N^2 * var / n),
                se = sqrt(var),
                cv = se / point
            ) -> stratified_stat
    } else{
        .tbl %>% ungroup() %>%
            summarize(
                Ntotal = sum(N),
                ntotal = sum(n),
                point = sum(N / Ntotal * point),
                var = sum((N/Ntotal)^2 * var / n),
                se = sqrt(var),
                cv = se / point
            ) -> stratified_stat
    }
    return(stratified_stat)
}

#' conf_int
#'
#' @description
#' A helper function for generating symmetric confidence intervals using a `t`
#' or standard normal distribution. The confidence interval is a
#' 100(1 - alpha)% confidence interval.
#'
#' @param .tbl
#' Output from `mk_stat` or `stratified_estimator`
#'
#' @param .alpha
#' The confidence level.
#'
#' @param .interval
#' Either "t" or "z". If "t", degrees of freedom are inferred from the input .tbl
#'
#' @return
#' A list with
#' \item{level}{The percentage of the confidence interval, 100(1-alpha)}
#' \item{df}{`NA` if .interval="z", otherwise the degrees of freedom used in t interval}
#' \item{lower}{The lower bound of the confidence interval}
#' \item{upper}{The upper bound of the confidence interval}
#' @export
#'
#' @examples
#' # Simplified output similar to stratified_sample
#' ds <- tibble::tibble(Ntotal=10, ntotal=4, point=3, se=1)
#' conf_int(ds, 0.05, "t")
#'
#' # Simplified output from mk_stat
#' d <- tibble::tibble(n = 4, point=3, se=1)
#' conf_int(d, 0.05, "t")
conf_int <- function(.tbl, .alpha, .interval="t"){
    # Error checks
    stopifnot(
        class(.tbl)[2] == "tbl",
        nrow(.tbl) == 1,
        is.numeric(.alpha),
        length(.alpha) == 1,
        .alpha > 0 & .alpha < 1,
        .interval %in% c("t", "z")
    )
    # Get a score to scale the confidence interval by
    if(.interval == "z"){
        .df <- NA
        .score <- qnorm(1 - .alpha / 2)
    }else{
        # Need to get degrees of freedom with two possible cases
        # (1) If the estimator is a stratified estimator
        # (2) If the estimator is from a simple random sample
        if("Ntotal" %in% names(.tbl)){
            .df <- as.numeric(.tbl[, 1] - .tbl[, 2])
        }else{
            .df <- as.numeric(.tbl[, 1]) - 1
        }
        .score <- qt(1 - .alpha / 2, df = .df)
    }
    .level <- 100 * (1 - .alpha)
    .lower <- .tbl$point - .score * .tbl$se
    .upper <- .tbl$point + .score * .tbl$se
    .conf_int <- list(
        "level" = .level,
        "interval" = .interval,
        "score" = .score,
        "df" = .df,
        "lower" = .lower,
        "upper" = .upper
    )
    return(.conf_int)
}

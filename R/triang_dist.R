# Triangular distribution functions (tidyverse style)

#' Triangular distribution density function
#'
#' Computes the probability density function of the triangular distribution.
#'
#' @param x numeric vector of quantiles.
#' @param lower lower bound of the distribution.
#' @param upper upper bound of the distribution.
#' @param mode mode of the distribution, must satisfy lower <= mode <= upper.
#'
#' @return numeric vector with density values.
#' @export
dtriang <- function(x, lower, upper, mode) {
  if (lower > upper) {
    stop("`lower` must be <= `upper`")
  }
  if (mode < lower || mode > upper) {
    stop("`mode` must be within [lower, upper]")
  }

  out <- ifelse(
    x < lower | x > upper,
    0,
    ifelse(
      x < mode,
      (2 * (x - lower)) / ((upper - lower) * (mode - lower)),
      ifelse(
        x == mode,
        2 / (upper - lower),
        (2 * (upper - x)) / ((upper - lower) * (upper - mode))
      )
    )
  )

  out
}

#' Triangular distribution cumulative distribution function
#'
#' Computes the cumulative distribution function of the triangular distribution.
#'
#' @param q numeric vector of quantiles.
#' @param lower lower bound of the distribution.
#' @param upper upper bound of the distribution.
#' @param mode mode of the distribution.
#'
#' @return numeric vector with cumulative probabilities.
#' @export
ptriang <- function(q, lower, upper, mode) {
  if (lower > upper) {
    stop("`lower` must be <= `upper`")
  }
  if (mode < lower || mode > upper) {
    stop("`mode` must be within [lower, upper]")
  }

  prob <- ifelse(
    q < lower,
    0,
    ifelse(
      q < mode,
      ((q - lower)^2) / ((upper - lower) * (mode - lower)),
      ifelse(
        q <= upper,
        1 - ((upper - q)^2) / ((upper - lower) * (upper - mode)),
        1
      )
    )
  )

  prob
}

#' Triangular distribution quantile function
#'
#' Computes the quantile function (inverse CDF) of the triangular distribution.
#'
#' @param p numeric vector of probabilities in interval 0, 1".
#' @param lower lower bound of the distribution.
#' @param upper upper bound of the distribution.
#' @param mode mode of the distribution.
#'
#' @return numeric vector of quantiles.
#' @export
qtriang <- function(p, lower, upper, mode) {
  if (lower > upper) {
    stop("`lower` must be <= `upper`")
  }
  if (mode < lower || mode > upper) {
    stop("`mode` must be within [lower, upper]")
  }
  if (any(p < 0 | p > 1)) {
    stop("`p` must be between 0 and 1")
  }

  p_mode <- (mode - lower) / (upper - lower)

  out <- ifelse(
    p < p_mode,
    lower + sqrt(p * (upper - lower) * (mode - lower)),
    upper - sqrt((1 - p) * (upper - lower) * (upper - mode))
  )

  out
}
#' Random generation from triangular distribution
#'
#' Generates random samples from a triangular distrib using inverse transform.
#' @importFrom stats runif
#' @param n number of observations to generate.
#' @param lower lower bound of the distribution.
#' @param upper upper bound of the distribution.
#' @param mode mode of the distribution.
#'
#' @return numeric vector of random samples.
#' @export
rtriang <- function(n, lower, upper, mode) {
  if (lower > upper) {
    stop("`lower` must be <= `upper`")
  }
  if (mode < lower || mode > upper) {
    stop("`mode` must be within [lower, upper]")
  }

  u <- runif(n, 0, 1)
  qtriang(u, lower, upper, mode)
}

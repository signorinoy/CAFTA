#' Simulated Survival Data Set
#'
#' @description  A data frame containing simulated survival times, censoring
#'   indicators, and covariates for use in survival analysis.
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{id}{Observation ID.}
#'   \item{time}{Observed survival time.}
#'   \item{status}{Event indicator (1 = event, 0 = censored).}
#'   \item{x1}{Standard normal covariate.}
#'   \item{x2}{Normal covariate, correlated 0.3 with x1.}
#'   \item{x31}{Dummy for x3 = 1.}
#'   \item{x42}{Dummy for x4 = 2.}
#'   \item{x43}{Dummy for x4 = 3.}
#'   \item{x44}{Dummy for x4 = 4.}
#'   \item{group}{Group (1–6), sample sizes: 500, 300, 100, 50, 50, 50.}
#' }
#'
#' @details This data set was generated with 1050 samples, using regression
#'   coefficients (0.15, -0.15, 0.3, 0.1, -0.1, 0.3). Event times were drawn
#'   from a Weibull distribution (shape = 0.8, scale = 0.3), and censoring times
#'   from a uniform distribution with maximum 3.2.
#'
#' @examples
#' head(sim)
"sim"

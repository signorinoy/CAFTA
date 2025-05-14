#' @title Alias for Survival Function
#' @description This function creates an alias for the `Surv` function from the
#' `survival` package, allowing it to be used without explicitly referencing
#' the package namespace.
#'
#' @param time The follow-up time for the subject.
#' @param time2 The ending time for the interval (if applicable).
#' @param event The status indicator, normally 0=alive, 1=dead.
#'              Other values are allowed.
#' @param type Character string specifying the type of censoring.
#'             Possible values are "right", "left", "interval", or "counting".
#' @param origin The origin for counting time, used when `type = "counting"`.
#'
#' @return A `Surv` object representing the survival data.
#'
#' @importFrom survival Surv
#' @seealso \code{\link[survival]{Surv}}
#' @export
Surv <- survival::Surv # nolint: object_name_linter.

#' Collaborative Accelerated Failure Time Analysis (CAFTA)
#'
#' @description Fits an accelerated failure time (AFT) model using collaborative
#' analysis for survival data.
#'
#' @param formula A formula object specifying the model. The response must be
#'   a survival object created using the `Surv` function from the `survival`
#'   package.
#' @param data A data frame containing the variables in the model.
#' @param dist A character string specifying the distribution to be used.
#'   Choices are \code{"weibull"}, \code{"loglogistic"}, or \code{"lognormal"}.
#'   Default is \code{"weibull"}.
#' @param robust Logical; if \code{TRUE}, computes robust (sandwich) covariance
#'   estimates. Default is \code{FALSE}.
#' @param init Optional numeric vector of initial values for the parameters.
#'   If not supplied, defaults are used.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{"cafta"} containing:
#'   \item{theta}{Estimated parameter vector.}
#'   \item{hess}{Estimated Hessian matrix.}
#'   \item{cov}{Estimated covariance matrix.}
#'   \item{formula}{The model formula.}
#'   \item{dist}{The distribution used.}
#'   \item{robust}{Logical indicating if robust covariance was used.}
#'   \item{call}{The matched call.}
#'
#' @details This function fits an accelerated failure time model using maximum
#'   likelihood estimation. The supported distributions are Weibull,
#'   log-logistic, and log-normal. Robust covariance estimation is available via
#'   the \code{robust} argument.
#'
#' @examples
#' formula <- Surv(time, status) ~ x1 + x2 + x31 + x42 + x43 + x44
#' fit <- cafta(formula, sim[sim$group == 1, ], dist = "weibull")
#' for (k in 2:6) {
#'   fit <- update(fit, sim[sim$group == k, ])
#' }
#' summary(fit)
#'
#' @importFrom stats model.frame model.response model.matrix nlm
#'
#' @export
cafta <- function(
    formula, data, dist = c("weibull", "loglogistic", "lognormal"),
    robust = FALSE, init, ...) {
  if (!inherits(formula, "formula")) stop("formula must be a formula object")
  if (!is.data.frame(data)) stop("data must be a data frame")
  if (!is.logical(robust) || length(robust) != 1) {
    stop("robust must be a logical value")
  }
  dist <- match.arg(dist)

  # Prepare model frame and extract response and predictor variables
  mf <- model.frame(formula, data = data)
  y <- model.response(mf)
  time <- y[, 1, drop = TRUE]
  status <- y[, 2, drop = TRUE]
  x <- model.matrix(formula, data = mf)

  n_parameters <- ncol(x) + 1
  if (missing(init)) {
    init <- numeric(n_parameters)
  } else if (length(init) != n_parameters) {
    stop("init must be a numeric vector of length ", n_parameters)
  }
  # Initialize theta and hess
  theta <- init
  hess <- matrix(0, n_parameters, n_parameters)

  # Optimize using nlm
  opt <- nlm(
    objective, init,
    time = time, status = status, x = x,
    theta = theta, hess = hess, dist = dist, robust = FALSE
  )
  if (!opt$code %in% c(1, 2)) stop("Optimization failed")
  theta <- opt$estimate
  # Recompute objective at optimum to get accurate hessian/gradient
  obj <- objective(
    theta,
    time = time, status = status, x = x,
    theta = theta, hess = hess, dist = dist, robust = robust
  )
  hess <- attr(obj, "hessian")
  if (robust) {
    cov <- attr(obj, "covariance")
  } else {
    cov <- matrix(0, n_parameters, n_parameters)
  }
  structure(
    list(
      theta = theta,
      hess = hess,
      cov = cov,
      formula = formula,
      dist = dist,
      robust = robust,
      call = match.call()
    ),
    class = "cafta"
  )
}

#' Update a CAFTA Model Object
#'
#' @description Updates a fitted CAFTA model object with new data.
#'
#' @param object An object of class \code{cafta}.
#' @param newdata A data frame containing the new data to be used for updating
#'   the model. The data frame must contain the same variables as those used in
#'   the original model.
#' @param ... Additional arguments (currently unused).
#'
#' @return An updated object of class \code{cafta}.
#'
#' @seealso \code{\link{cafta}}, \code{\link[stats]{update}}
#'
#' @importFrom stats update
#' @export
update.cafta <- function(object, newdata, ...) {
  if (!inherits(object, "cafta")) stop("object must be of class 'cafta'")
  if (!is.data.frame(newdata)) stop("newdata must be a data frame")

  # Prepare new model frame and extract response and predictor variables
  mf <- model.frame(object$formula, data = newdata)
  y <- model.response(mf)
  time <- y[, 1, drop = TRUE]
  status <- y[, 2, drop = TRUE]
  x <- model.matrix(object$formula, data = mf)

  # Optimize using nlm
  opt <- nlm(
    objective, object$theta,
    time = time, status = status, x = x,
    theta = object$theta, hess = object$hess,
    dist = object$dist, robust = FALSE
  )
  if (!opt$code %in% c(1, 2)) stop("Optimization failed")
  theta <- opt$estimate

  # Recompute objective at optimum to get accurate hessian/gradient
  obj <- objective(
    theta,
    time = time, status = status, x = x,
    theta = object$theta, hess = object$hess,
    dist = object$dist, robust = object$robust
  )
  hess <- attr(obj, "hessian")
  if (object$robust) {
    cov <- object$cov + attr(obj, "covariance")
  } else {
    cov <- object$cov
  }

  structure(
    list(
      theta = theta,
      hess = hess,
      cov = cov,
      formula = object$formula,
      dist = object$dist,
      robust = object$robust,
      call = match.call()
    ),
    class = "cafta"
  )
}

#' Extract Model Coefficients from a cafta Object
#'
#' @description This method extracts the estimated coefficients from an object
#'   of class \code{cafta}.
#'
#' @param object An object of class \code{cafta}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A numeric vector containing the estimated coefficients.
#'
#' @seealso \code{\link{cafta}}
#'
#' @importFrom stats coef terms
#' @export
coef.cafta <- function(object, ...) {
  if (!inherits(object, "cafta")) stop("object must be of class 'cafta'")
  coef <- object$theta
  names(coef) <- c(
    "(Intercept)", attr(terms(object$formula), "term.labels"), "log(scale)"
  )
  coef
}

#' Calculate Variance-Covariance Matrix for cafta Objects
#'
#' @description  Computes the variance-covariance matrix for objects of
#'   class \code{cafta}. If the \code{robust} slot of the object is \code{TRUE},
#'   a robust variance-covariance matrix is calculated using the sandwich
#'   estimator. Otherwise, the standard variance-covariance matrix is computed
#'   as the inverse of the Hessian matrix.
#'
#' @param object An object of class \code{cafta}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A variance-covariance matrix.
#' @seealso \code{\link{cafta}}, \code{\link[stats]{vcov}}
#' @export
vcov.cafta <- function(object, ...) {
  if (!inherits(object, "cafta")) stop("object must be of class 'cafta'")
  hess_inv <- solve(object$hess)
  if (object$robust) {
    hess_inv %*% object$cov %*% hess_inv
  } else {
    solve(object$hess)
  }
}

#' Summarize a CAFTA Model Object
#'
#' @description Provides a summary for objects of class \code{cafta},
#'   including coefficient estimates, standard errors, z-values, and p-values.
#'
#' @param object An object of class \code{cafta}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return An object of class \code{summary.cafta}, which is a list containing:
#'   \item{coefficients}{A matrix with columns for the estimate, standard error,
#'           z value, and p-value for each coefficient.}
#'   \item{call}{The function call that produced the \code{cafta} object.}
#'
#' @importFrom stats pchisq
#' @export
summary.cafta <- function(object, ...) {
  if (!inherits(object, "cafta")) stop("object must be of class 'cafta'")
  coef <- coef(object)
  vcov <- vcov(object)
  se <- sqrt(diag(vcov))
  z_scores <- coef / se
  p_values <- pchisq(z_scores^2, df = 1, lower.tail = FALSE)
  coef_matrix <- cbind(coef, se, z_scores, p_values)
  dimnames(coef_matrix) <- list(
    names(coef), c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  )
  structure(
    list(
      coefficients = coef_matrix,
      call = object$call
    ),
    class = "summary.cafta"
  )
}

#' Print Method for CAFTA Summary Objects
#'
#' @description This function provides a custom print method for objects of
#'   class `summary.cafta`. It displays the function call and a formatted table
#'   of coefficients, including standard errors, test statistics, and p-values.
#'
#' @param x An object of class `summary.cafta`, typically the result of a call
#'   to `summary()` on a CAFTA model object.
#' @param digits The minimum number of significant digits to be used for
#'   printing numeric values. Defaults to `max(3, getOption("digits") - 3)`.
#' @param signif.stars Logical; if `TRUE`, significant stars are printed
#'   alongside p-values. Defaults to `getOption("show.signif.stars")`.
#' @param ... Additional arguments passed to or from other methods.
#'
#' @importFrom stats printCoefmat
#' @export
print.summary.cafta <- function(
    x, digits = max(3, getOption("digits") - 3),
    signif.stars = getOption("show.signif.stars"), ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  printCoefmat(
    x$coefficients,
    digits = digits, signif.stars = signif.stars,
    cs.ind = 1:2, tst.ind = 3, P.values = TRUE, has.Pvalue = TRUE
  )
  invisible(x)
}

#' Objective Function for CAFTA
#'
#' @description Computes the penalized negative log-likelihood, gradient, and
#'   Hessian for accelerated failure time models with parametric distributions
#'  (Weibull, log-logistic, log-normal). The function also allows for robust
#'  covariance estimation.
#'
#' @param par Numeric vector of parameters.
#' @param time Numeric vector of observed survival or censoring times.
#' @param status Numeric vector indicating event occurrence.
#' @param x Numeric matrix of covariates.
#' @param theta Numeric vector of prior means for parameters (for penalization).
#' @param hess Numeric matrix, prior Hessian matrix for penalization.
#' @param dist Character string specifying the distribution. One of
#'   \code{"weibull"}, \code{"loglogistic"}, or \code{"lognormal"}.
#' @param robust Logical; if \code{TRUE}, computes robust covariance estimate
#'   and attaches it as an attribute.
#' @param eps Numeric; small value to avoid division by zero or log of zero,
#'  default is the machine's double precision epsilon.
#'
#' @details
#'   Computes the penalized negative log-likelihood for the specified
#'   accelerated failure time model. The returned value includes attributes
#'   \code{"gradient"} and \code{"hessian"} for use in optimization routines.
#'   If \code{robust = TRUE}, a robust covariance estimate is attached as the
#'   \code{"covariance"} attribute.
#'
#' @return
#' A numeric value representing the penalized negative log-likelihood.
#' The result has attributes:
#' \describe{
#'   \item{gradient}{Gradient vector of the objective function.}
#'   \item{hessian}{Hessian matrix of the objective function.}
#'   \item{covariance}{(If \code{robust = TRUE}) Robust covariance estimate.}
#' }
#'
#' @importFrom stats dnorm pnorm
objective <- function(
    par, time, status, x, theta, hess, dist,
    robust = FALSE, eps = .Machine$double.eps) {
  beta <- par[-length(par)]
  gamma <- par[length(par)]
  sigma <- exp(gamma)

  loss1 <- as.numeric((par - theta) %*% hess %*% (par - theta) / 2)
  grad1 <- as.vector(hess %*% (par - theta))
  hess1 <- hess

  eta <- as.numeric(x %*% beta)
  eps <- (log(time) - eta) / sigma
  haz <- switch(dist,
    weibull = exp(eps - exp(eps)),
    loglogistic = exp(eps) / (1 + exp(eps))^2,
    lognormal = stats::dnorm(eps)
  )
  surv <- switch(dist,
    weibull = exp(-exp(eps)),
    loglogistic = (1 + exp(eps))^(-1),
    lognormal = stats::pnorm(eps, lower.tail = FALSE)
  )
  loss2 <- -sum(status * (log(haz) - gamma) + (1 - status) * log(surv))

  dhaz <- switch(dist,
    weibull = 1 - exp(eps),
    loglogistic = (1 - exp(eps)) / (1 + exp(eps)),
    lognormal = -eps
  )
  dsurv <- switch(dist,
    weibull = -exp(eps),
    loglogistic = - exp(eps) / (1 + exp(eps)),
    lognormal = -stats::dnorm(eps) / stats::pnorm(eps, lower.tail = FALSE)
  )
  deps <- status *  dhaz + (1 - status) * dsurv

  dbeta <- x / sigma * deps
  dgamma <- status + eps * deps
  dtheta <- cbind(dbeta, dgamma)
  grad2 <- colSums(dtheta)

  d2haz <- switch(dist,
    weibull = -exp(eps),
    loglogistic = -2 * exp(eps) / (1 + exp(eps))^2,
    lognormal = -1
  )
  d2surv <- switch(dist,
    weibull = -exp(eps),
    loglogistic = - exp(eps) / (1 + exp(eps))^2,
    lognormal = - eps * dsurv - dsurv^2
  )
  d2eps <- status * d2haz + (1 - status) * d2surv

  d2beta <- -crossprod(x * d2eps, x) / sigma^2
  dbetadgamma <- -t(x) %*% (deps + eps * d2eps) / sigma
  d2gamma <- -sum(eps * (deps + eps * d2eps))
  hess2 <- rbind(
    cbind(d2beta, dbetadgamma),
    c(dbetadgamma, d2gamma)
  )

  res <- loss1 + loss2
  attr(res, "gradient") <- grad1 + grad2
  attr(res, "hessian") <- hess1 + hess2
  if (robust) {
    attr(res, "covariance") <- crossprod(dtheta)
  }
  res
}

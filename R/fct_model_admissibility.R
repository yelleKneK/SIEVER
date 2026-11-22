#' Check Model Covariances Apart From Saturated Auxiliary Correlates
#'
#' The saturated-correlates parameterization can make the joint residual
#' covariance indefinite when an auxiliary relates strongly to a factor.
#' The item residuals, latent covariance and joint implied covariance are
#' the matrices whose admissibility matters for the substantive model.
#' @param fit Fitted lavaan object.
#' @return Whether those covariance checks pass.
#' @author Ken Kelley
#' @noRd
sieve_model_post_check <- function(fit) {
  options <- lavaan::lavInspect(fit, "options")
  aux <- options$aux
  if (!length(aux)) return(isTRUE(suppressWarnings(lavaan::lavInspect(fit, "post.check"))))
  positive <- sieve_covariance_nonnegative
  groups <- function(x) if (is.matrix(x)) list(x) else x
  est <- lavaan::lavInspect(fit, "est")
  if (is.matrix(est$lambda)) est <- list(est)
  all(vapply(est, function(x) {
    items <- setdiff(rownames(x$theta), aux)
    positive(x$theta[items, items, drop = FALSE]) && positive(x$psi)
  }, logical(1))) &&
    all(vapply(groups(lavaan::lavInspect(fit, "cov.lv")), positive, logical(1))) &&
    all(vapply(groups(lavaan::lavInspect(fit, "cov.ov")), positive, logical(1)))
}


#' Check a Covariance Block Independently of Measurement Units
#' @param x Symmetric covariance matrix.
#' @return Whether the correlation-scaled matrix has no negative eigenvalue.
#' @author Ken Kelley
#' @noRd
sieve_covariance_nonnegative <- function(x) {
  if (!length(x)) return(TRUE)
  if (any(!is.finite(x)) || any(diag(x) < 0)) return(FALSE)
  zero <- diag(x) == 0
  if (any(zero) && any(x[zero, , drop = FALSE] != 0)) return(FALSE)
  x <- x[!zero, !zero, drop = FALSE]
  if (!length(x)) return(TRUE)
  scale <- sqrt(diag(x))
  correlation <- x / outer(scale, scale)
  ev <- eigen(correlation, symmetric = TRUE, only.values = TRUE)$values
  all(ev >= -.Machine$double.eps^(3 / 4) * max(1, abs(ev[1L])))
}

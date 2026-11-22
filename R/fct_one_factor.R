#' One Public Engine Route for a One-Factor Estimate or Residual Fit
#' @param X Aligned item and optional auxiliary data.
#' @param ordered Whether to treat every indicator as ordered.
#' @param missing Missing-data treatment.
#' @param aux Continuous auxiliary names under FIML.
#' @param fast Use the complete-data covariance/N route for exploratory screening.
#' @param output Return the engine's omega table or its residual model fit.
#' @author Ken Kelley
#' @noRd
sieve_one_factor <- function(X, ordered = FALSE, missing = c("listwise", "fiml"),
                             aux = NULL, fast = FALSE, output = c("omega", "fit")) {
  missing <- match.arg(missing)
  output <- match.arg(output)
  items <- setdiff(names(X), aux)
  X <- sieve_reliability_data(X, c(items, aux))
  if (ordered) missing <- "listwise"
  seen <- rowSums(!is.na(as.matrix(X[, items, drop = FALSE])))
  available <- rowSums(!is.na(as.matrix(X)))
  X <- X[if (missing == "fiml") available > 0L else seen == length(items), , drop = FALSE]
  if (output == "omega") {
    if (ordered) {
      args <- list(data = X[, items, drop = FALSE], ci_method = "none", denominator = "observed")
      return(do.call(DMAR::reliability_omega_categorical, args))
    }
    args <- list(ci_method = "none", denominator = "observed")
    if (fast && missing == "listwise" && !length(aux)) {
      args$S <- stats::cov(X)
      args$N <- nrow(X)
    } else {
      args$data <- X
      args$missing <- missing
      if (length(aux)) args$aux <- aux
    }
    return(do.call(DMAR::reliability_omega, args))
  }
  latent <- unname(sieve_allocate_names("F", taken = names(X), prefix = "F"))
  args <- list(model = sprintf("%s =~ %s", latent, paste(items, collapse = " + ")),
    data = X, std.lv = TRUE, se = "none", test = "standard")
  if (ordered) {
    args$ordered <- items
    args$estimator <- "WLSMV"
  } else {
    args$estimator <- "ML"
    args$missing <- if (missing == "fiml") "ml" else "listwise"
    if (missing == "fiml" && length(aux)) {
      args$aux <- aux
      args$check.post <- FALSE
    }
  }
  do.call(lavaan::cfa, args)
}

# Efa tables.

#' Numeric EFA tables without display suppression or rounding
#'
#' The EFA page's solution (`sieve_efa_dmar_solution()`, DMAR's values) or
#' a psych common-factor solution, read into the matrices the page shows.
#' For DMAR's solution the intervals come with them: the pattern
#' coefficients' and factor correlations' limits, the communalities' and
#' uniquenesses' limits, the determinacy, minimum score correlation and H
#' of each factor with theirs, the variance accounted, the fit measures of
#' the unrotated fit (with the RMSEA interval at the call's confidence
#' level, which the page shows and the export's manifest records), and
#' every parameter of DMAR's table in both metrics.
#'
#' @param fit Stored solution.
#' @param meta Frozen item ledger.
#' @return Numeric matrices and tables.
#' @author Ken Kelley
#' @noRd
sieve_efa_numeric_tables <- function(fit, meta) {
  pattern <- unclass(fit$loadings)
  if (!is.matrix(pattern)) stop("Run an EFA with a stored loading matrix first.", call. = FALSE)
  phi <- fit$Phi
  if (is.null(phi)) {
    phi <- diag(ncol(pattern))
    dimnames(phi) <- list(colnames(pattern), colnames(pattern))
  }
  structure <- if (is.null(fit$Structure)) pattern %*% phi else unclass(fit$Structure)
  ids <- rownames(pattern)
  idx <- match(ids, meta$item)
  items <- meta[idx, intersect(c("item", "label", "construct", "facet", "type", "status"), names(meta)), drop = FALSE]
  items$communality <- as.numeric(fit$communality)
  dmar <- inherits(fit, "sieve_efa_solution")
  if (dmar) {
    items$communality_lower <- as.numeric(fit$communality_lower)
    items$communality_upper <- as.numeric(fit$communality_upper)
  }
  items$uniqueness <- if (length(fit$uniquenesses) == nrow(pattern)) as.numeric(fit$uniquenesses) else 1 - items$communality
  if (dmar) {
    items$uniqueness_lower <- 1 - items$communality_upper
    items$uniqueness_upper <- 1 - items$communality_lower
  }
  if (length(fit$complexity) == nrow(pattern)) items$complexity <- as.numeric(fit$complexity)
  if (!dmar) {
    return(list(pattern = pattern, structure = structure, phi = phi, items = items, variance_accounted = fit$Vaccounted))
  }
  parameters <- fit$dmar
  class(parameters) <- "data.frame"
  attributes(parameters) <- attributes(parameters)[c("names", "row.names", "class")]
  fit_measures <- fit$fit_measures
  if (!is.null(fit_measures)) {
    # The RMSEA interval is at the call's rmsea_conf_level (the RMSEA
    # level, 0.90 by default; the coefficients' conf_level for a solution
    # that does not record it), which the export's manifest records
    # (rmsea_conf_level, read by sieve_efa_fit_level()).
    fit_measures <- data.frame(term = fit_measures$term, value = fit_measures$value, stringsAsFactors = FALSE)
  }
  list(
    pattern = pattern, pattern_lower = fit$loadings_lower, pattern_upper = fit$loadings_upper,
    pattern_se = fit$loadings_se, structure = structure, phi = phi,
    phi_lower = fit$Phi_lower, phi_upper = fit$Phi_upper, items = items,
    variance_accounted = fit$variance_accounted, factor_quality = fit$factor_quality,
    fit_measures = fit_measures, parameters = parameters
  )
}

#' The variance accounted for, in the EFA page's layout
#'
#' For DMAR's solution: each factor's variance accounted for, the diagonal
#' of Phi Lambda' Lambda with Lambda the standardized pattern coefficients
#' (the column sum of pattern times structure coefficients; the column sum
#' of the squared loadings when the factors are uncorrelated), which add to
#' the total communality, and its proportion of the items' total variance
#' and of the common variance, as DMAR's `variance_accounted` attribute
#' gives them, one column per factor and the total. No cumulative row is
#' shown, since the order of the factors is arbitrary. The quantity is the
#' one psych labels "SS loadings" under an oblique rotation; it is not
#' SPSS's rotation sums of squared loadings (the column sums of the squared
#' structure coefficients, which do not add to a total when the factors are
#' correlated).
#'
#' For a psych solution: psych's rows, labeled as psych's partition when
#' the factors are correlated.
#'
#' @param f The EFA page's solution, or a psych::fa result.
#' @return A data frame with `Statistic` and one column per factor (and
#'   `total` for DMAR's solution).
#' @author Ken Kelley
#' @noRd
sieve_efa_vaccounted_table <- function(f) {
  if (inherits(f, "sieve_efa_solution")) {
    va <- f$variance_accounted
    if (is.null(va) || !nrow(va)) return(NULL)
    rows <- c(
      sum_sq_loadings = if (isTRUE(f$oblique)) {
        "Variance accounted for (diagonal of Phi Lambda' Lambda; pattern x structure)"
      } else {
        "Variance accounted for (sum of squared loadings)"
      },
      proportion_variance = "Proportion of the total variance of the items",
      proportion_common = "Proportion of the common variance"
    )
    out <- data.frame(Statistic = unname(rows), stringsAsFactors = FALSE)
    for (i in seq_len(nrow(va))) {
      out[[as.character(va$factor[i])]] <- vapply(names(rows), function(r) as.numeric(va[[r]][i]), numeric(1))
    }
    return(out)
  }
  va <- f$Vaccounted
  labels <- rownames(va)
  if (!is.null(f$Phi) && ncol(unclass(f$loadings)) > 1L) {
    labels <- sub("^SS loadings$", "SS loadings (psych's partition: pattern x structure)", labels)
    labels <- sub("^Proportion Var$", "Proportion Var (psych's partition)", labels)
    labels <- sub("^Cumulative Var$", "Cumulative Var (psych's partition)", labels)
  }
  data.frame(Statistic = labels, va, check.names = FALSE, stringsAsFactors = FALSE)
}

#' Each factor's determinacy, minimum score correlation and H
#'
#' DMAR's `factor_quality` attribute in the EFA page's layout: the
#' determinacy (the correlation of the regression factor score with the
#' factor; its square is the factor's maximal reliability), beside it
#' 2 rho^2 - 1, the lowest correlation two sets of factor scores equally
#' consistent with the model can have (Guttman, 1955; DMAR's
#' `min_score_correlation` guardrail), and Hancock and Mueller's H on the
#' factor's own loadings, each with its limits (logit-scale Wald limits,
#' mapped back). No threshold is applied.
#'
#' @param solution The EFA page's solution.
#' @return A data frame, one row per factor, or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_efa_quality_table <- function(solution) {
  q <- solution$factor_quality
  if (is.null(q) || !nrow(q)) return(NULL)
  sieve_dmar_require_columns(q, c("factor", "determinacy", "determinacy_ci_lower", "determinacy_ci_upper",
    "min_score_correlation", "min_score_correlation_ci_lower", "min_score_correlation_ci_upper",
    "H", "H_ci_lower", "H_ci_upper"), "EFA factor quality")
  data.frame(
    Factor = as.character(q$factor),
    Determinacy = q$determinacy, Determinacy_lower = q$determinacy_ci_lower, Determinacy_upper = q$determinacy_ci_upper,
    Squared = q$determinacy^2,
    Min_score_r = q$min_score_correlation, Min_score_r_lower = q$min_score_correlation_ci_lower,
    Min_score_r_upper = q$min_score_correlation_ci_upper,
    H = q$H, H_lower = q$H_ci_lower, H_upper = q$H_ci_upper,
    stringsAsFactors = FALSE
  )
}

#' A value with its interval, for a display cell
#' @param est,lo,hi Numbers.
#' @param digits Decimal places.
#' @return Character: `"est [lo, hi]"`, or the estimate alone without limits.
#' @author Ken Kelley
#' @noRd
sieve_efa_ci_cell <- function(est, lo, hi, digits = 3L) {
  f <- function(v) formatC(v, format = "f", digits = digits)
  out <- ifelse(is.finite(est), f(est), "")
  both <- is.finite(est) & is.finite(lo) & is.finite(hi)
  out[both] <- sprintf("%s [%s, %s]", f(est[both]), f(lo[both]), f(hi[both]))
  out
}

#' An interval alone, for a display cell
#' @param lo,hi Numbers.
#' @param digits Decimal places.
#' @return Character: `"[lo, hi]"`, or empty without limits.
#' @author Ken Kelley
#' @noRd
sieve_efa_interval_cell <- function(lo, hi, digits = 3L) {
  f <- function(v) formatC(v, format = "f", digits = digits)
  both <- is.finite(lo) & is.finite(hi)
  out <- rep("", length(lo))
  out[both] <- sprintf("[%s, %s]", f(lo[both]), f(hi[both]))
  out
}

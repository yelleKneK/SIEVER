# fct_efa_point.R
# The exploratory solutions SIEVER reads for their point estimates alone
# (the Items tab's diagnostics, Very Simple Structure on the Dimensionality
# page, and the Optimizer's simple structure) are DMAR::fa_exploratory(),
# maximum likelihood with direct oblimin rotation (no rotation at one
# factor): on the shared correlation matrix (`S` with its `N`, the matrix
# route) where SIEVER fits a correlation matrix, and on the complete item
# responses (the data route) where it fits raw data. The helpers below
# build that one call, run it while keeping every condition (a refusal by
# its class, with DMAR's `reason`), read the loadings, factor correlations,
# communalities and Hofmann complexities, estimate the wait, and write the
# R that replays it. They reuse the EFA page's call, runner and reader
# (R/fct_efa_dmar.R).

#' The DMAR call of a point-estimate exploratory solution
#'
#' Maximum likelihood (`estimator = "ML"`), direct oblimin rotation with
#' delta = 0 (`rotation = "oblimin"`) for two or more factors and the
#' unrotated solution (`rotation = "none"`) for one, the 95% level of DMAR's
#' intervals (which these readers do not show), and the seed of the
#' rotation's 30 random starts. On the matrix route the matrix is supplied
#' as `S` with `N` (covariance input has no missing data route, so `missing`
#' is not passed); on the data route the complete rows are supplied with
#' `missing = "listwise"`, the analysis these statistics define (the complete
#' cases; with no value missing FIML gives the same estimates at about three
#' times the time). The unrotated solution at the RMSEA's level
#' (`sieve_efa_dmar_call()`'s `rmsea`) is not added: these readers show no
#' fit. The call asks for the point estimates only
#' (`point_estimates_only = TRUE`): the Items tab, the Optimizer and Very
#' Simple Structure read nothing else, and they fit many solutions, so DMAR is
#' spared the standard errors, intervals and fit measures, which make the call
#' 14 to 170 times slower than psych::fa() on 9 to 68 items.
#'
#' @param k The number of factors.
#' @param R The correlation matrix (the matrix route), with `n`.
#' @param n Its sample size.
#' @param data The complete item responses (the data route).
#' @param seed The seed of the rotation's random starts, or `NULL`.
#' @return A call list (`fun`, `args`) for `sieve_efa_dmar_run()`.
#' @author Ken Kelley
#' @noRd
sieve_efa_point_call <- function(k, R = NULL, n = NULL, data = NULL, seed = NULL) {
  k <- as.integer(k)
  route <- if (is.null(data)) {
    list(form = "matrix", estimator = "ML", missing = NA_character_, ordered = NULL)
  } else {
    list(form = "data", estimator = "ML", missing = "listwise", ordered = NULL)
  }
  call <- sieve_efa_dmar_call(route, k,
    rotation = if (k <= 1L) "none" else "oblimin",
    data = data, basis = if (is.null(data)) list(R = R, n = n),
    seed = if (is.null(seed)) NULL else as.integer(seed), conf_level = 0.95
  )
  call$args$point_estimates_only <- TRUE
  call
}

#' The point estimates of a DMAR exploratory record
#'
#' @param record A record from `sieve_efa_dmar_run()`.
#' @return A list with `ok`, `status` (`sieve_efa_dmar_status()`: `code`
#'   `"ok"`, `"not_converged"` or `"failed"`, `message`, `refusal`,
#'   `reason`), `solution` (`sieve_efa_dmar_solution()`, or `NULL` when DMAR
#'   refused), `loadings`, `Phi`, `communality`, `complexity` (`NULL` when
#'   refused), `warnings` and `messages`.
#' @author Ken Kelley
#' @noRd
sieve_efa_point_read <- function(record) {
  status <- sieve_efa_dmar_status(record)
  solution <- if (identical(status$code, "ok")) sieve_efa_dmar_solution(record$value)
  list(
    ok = !is.null(solution), status = status, solution = solution,
    loadings = solution$loadings, Phi = solution$Phi, communality = solution$communality,
    complexity = solution$complexity,
    warnings = record$warnings %||% character(0), messages = record$messages %||% character(0)
  )
}

#' Estimated seconds of a point-estimate DMAR exploratory solution
#'
#' The call asks DMAR::fa_exploratory() for the point estimates only
#' (`sieve_efa_point_call()`). Its time has a fixed part, a part per
#' factor, and a part for the items, the moments, `p (p + 1) / 2`, times
#' the free parameters, about `p (k + 1)`, which the iterations of the fit
#' evaluate; at 68 items that part is most of the time. Measured with DMAR
#' 1.1.0 from CRAN and lavaan 0.7-3 on the matrix route at
#' 9 to 68 items and 1 to 8 factors: see `sieve_efa_point_rate()` for the
#' runs and the fitted constants. A solution DMAR refuses can
#' take far longer; `refused` marks the counts expected to be refused
#' (`sieve_efa_boundary_ks()`), which are estimated by
#' `sieve_efa_refusal_wait()`. The data route adds the raw data's handling.
#' The ratio of each measured run to this estimate is recorded
#' (`sieve_rate_note("efa_point", ...)`) and scales later estimates in the
#' session.
#'
#' @param p The number of items.
#' @param k The number of factors (a vector gives one estimate each).
#' @param form `"matrix"` or `"data"`.
#' @param refused Logical, recycled over `k`: is the count expected to be
#'   refused?
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_efa_point_wait <- function(p, k, form = "matrix", refused = FALSE) {
  p <- as.numeric(p)
  k <- as.numeric(k)
  if (length(p) != 1L || !is.finite(p) || !length(k) || any(!is.finite(k))) return(rep(NA_real_, max(1L, length(k))))
  rate <- sieve_efa_point_rate()
  base <- sieve_efa_point_base(p, k, form)
  refused <- rep_len(as.logical(refused), length(k))
  base <- ifelse(refused, rate[["refusal_fixed"]] + rate[["refusal_multiple"]] * base, base)
  ratio <- sieve_rate("efa_point")
  base * (if (is.finite(ratio) && ratio > 0) ratio else 1)
}

#' The standard seconds of a proper point-estimate DMAR solution
#' @inheritParams sieve_efa_point_wait
#' @return Seconds, before any measured ratio.
#' @author Ken Kelley
#' @noRd
sieve_efa_point_base <- function(p, k, form = "matrix") {
  rate <- sieve_efa_point_rate()
  moments <- p * (p + 1) / 2
  npar <- p * (k + 1)
  rate[["fixed"]] + rate[["per_factor"]] * k +
    rate[["moments_npar"]] * moments * npar / 1e6 +
    if (identical(form, "data")) rate[["data"]] else 0
}

#' Record the measured time of a point-estimate DMAR exploratory solution
#'
#' @param p,k,form As for `sieve_efa_point_wait()`.
#' @param seconds The measured seconds.
#' @param refused Logical, recycled over `k`: which counts DMAR refused (the
#'   estimate the time is compared with is that of the outcome).
#' @return Invisible `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_efa_point_time_note <- function(p, k, form, seconds, refused = FALSE) {
  expected <- sieve_at_default_rates(sum(sieve_efa_point_wait(p, k, form, refused = refused)))
  if (length(expected) == 1L && is.finite(expected) && expected > 0) {
    sieve_rate_note("efa_point", seconds, expected)
  }
  invisible(NULL)
}

#' The R that reads a DMAR exploratory solution's point estimates
#'
#' The lines after the DMAR call (`sieve_efa_dmar_code()`): the pattern
#' loadings (standardized), the factor correlations, the communalities and
#' Hofmann's complexity, read from DMAR's table by base R, so the script
#' needs DMAR and nothing of SIEVER.
#'
#' @param result The name of the DMAR result in the script.
#' @param prefix The prefix of the names the lines define.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_efa_point_read_code <- function(result = "items_efa", prefix = "items") {
  std <- sprintf("%s_std", prefix)
  c(
    sprintf("%s <- as.data.frame(%s)[%s$metric == \"standardized\", ]", std, result, result),
    sprintf("%s_pattern <- %s[%s$op == \"=~\", ]", prefix, std, std),
    sprintf(
      "%s_loadings <- tapply(%s_pattern$estimate, list(factor(%s_pattern$rhs, unique(%s_pattern$rhs)), factor(%s_pattern$lhs, unique(%s_pattern$lhs))), sum)",
      prefix, prefix, prefix, prefix, prefix, prefix
    ),
    sprintf("%s_h2 <- stats::setNames(%s$estimate[%s$op == \"communality\"], %s$lhs[%s$op == \"communality\"])",
      prefix, std, std, std, std),
    sprintf("%s_complexity <- stats::setNames(%s$estimate[%s$op == \"complexity\"], %s$lhs[%s$op == \"complexity\"])",
      prefix, std, std, std, std)
  )
}

#' The numbers of factors whose DMAR solution is likely to be refused
#'
#' `DMAR::fa_exploratory()` fits without bounds and refuses an improper
#' solution (a residual variance below zero), one that did not converge,
#' and a likelihood without a maximum; such a refusal can take far longer
#' than a proper fit, since lavaan's optimizer may iterate to its limit
#' first (as measured: see `sieve_efa_point_rate()`). Which
#' counts will be refused cannot be known before fitting, but the bounded
#' maximum likelihood solution says where the unbounded one runs off: a
#' uniqueness of `stats::factanal()` (base R, a few milliseconds on these
#' sizes) at its lower bound of 0.005, or a `factanal()` that fails, marks
#' the count. The screen decides only the estimate of the wait, never a
#' reported result; on the nine bundled tests it marks the four and five
#' factor solutions DMAR refuses, and the three factor solution after
#' excluding t7_sentence.
#'
#' @param R A correlation matrix.
#' @param ks The numbers of factors (identified ones; others are marked).
#' @return A logical vector, one per `ks`.
#' @author Ken Kelley
#' @noRd
sieve_efa_boundary_ks <- function(R, ks) {
  p <- ncol(R)
  vapply(as.integer(ks), function(k) {
    if (!is.finite(k) || k < 1L || (p - k)^2 < p + k) return(TRUE)
    f <- tryCatch(suppressWarnings(stats::factanal(covmat = R, factors = k, rotation = "none")),
      error = function(e) NULL)
    is.null(f) || min(f$uniquenesses) <= 0.005 + 1e-6
  }, logical(1))
}

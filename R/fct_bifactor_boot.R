# fct_bifactor_boot.R
# The nonparametric bootstrap of the bifactor indices, as plain functions the
# Bifactor module (and anything else) calls.
#
# The bootstrap of independent rows is DMAR's ci_bifactor_indices()
# (sieve_bifactor_boot_dmar_call(), sieve_bifactor_boot_from_dmar()): it
# resamples the rows lavaan stored with the point estimate's fit (the rows
# that fit used, with their missing data patterns), refits the same
# parameter table with the same lavaan options, checks that the refit
# reproduces the point estimates before any draw, counts every
# replication's fate, and takes a progress and cancellation hook. SIEVER
# passes improper = "keep" (an improper solution whose indices are
# proportions is used and counted) and the model-implied denominator.
# SIEVER's own aligned engine below serves a declared cluster design, which
# DMAR refuses (it resamples independent cases only). That engine is
# aligned with the point estimate it accompanies:
#
# * it resamples the rows the point estimate used, with their missing data
#   patterns (under full information maximum likelihood every row with an
#   observed item; under listwise deletion the complete rows), so the two
#   describe the same sample;
# * every resample is fitted by the function that produced the point
#   estimate, and the fitted specification is compared with the point
#   estimate's, so the two describe the same model and estimator;
# * one acceptance rule, stated before any resample is drawn, decides
#   whether a draw is used, and it is applied to the point estimate and to
#   every index alike, so all intervals rest on the same draws;
# * what happened to every draw is kept (fit returned, converged,
#   admissible, used, and the reason when not), so the counts shown are
#   counts of those things and not of rows that happened to hold numbers.
#
# Each draw has its own seed, derived from the run's seed, so a draw can be
# refitted on its own and a run that stops early holds exactly the first
# draws of the full run.

#' The fewest usable draws a percentile interval needs
#'
#' A percentile limit at level \eqn{\alpha / 2} stands for the
#' \eqn{\alpha / 2} quantile of the bootstrap distribution. The smallest of
#' \eqn{B} draws cuts off, on average, a share \eqn{1 / (B + 1)} of that
#' distribution (the \eqn{k}th of the sorted draws cuts off
#' \eqn{k / (B + 1)} on average),
#' so with fewer than \eqn{2 / \alpha - 1} draws (39 for a 95 percent
#' interval) every limit read from them cuts off, on average, more than
#' the tail it stands for. boot's percentile rule, which places the limit
#' at position \eqn{(B + 1) \alpha / 2} of the sorted draws, shows it
#' directly: below this count the position falls before the first order
#' statistic, and boot can only repeat the smallest or the largest draw
#' (`sieve_dmar_min_used()`). SIEVER's own limits (`sieve_percentile_limits()`,
#' and the type 7 quantiles of the IRT, ordinal impact and norm
#' calibration intervals, at position \eqn{(B - 1) \alpha / 2 + 1}) lie
#' within the draws at any count; for them the same count is a common
#' floor, so that every percentile interval SIEVER shows rests on at least
#' this many draws. It is necessary, not sufficient, for a limit to reach
#' its tail, and it is not a judgment about the scale.
#'
#' @param conf_level Confidence level, in (0, 1).
#' @return A single integer.
#' @author Ken Kelley
#' @noRd
sieve_bootstrap_min_draws <- function(conf_level = 0.95) {
  as.integer(ceiling(2 / (1 - conf_level) - 1e-9)) - 1L
}

#' The share of unusable draws at which the interval carries a caution
#'
#' The tail area of the interval, \eqn{(1 - C) / 2}. When at least that
#' share of the draws could not be used, the draws set aside could by
#' themselves make up a whole tail of the bootstrap distribution, so a
#' limit computed from the remaining draws could lie anywhere beyond them.
#' It marks when a caution is shown; it decides nothing about the model.
#'
#' @param conf_level Confidence level, in (0, 1).
#' @return A single number.
#' @author Ken Kelley
#' @noRd
sieve_bootstrap_caution_share <- function(conf_level = 0.95) {
  (1 - conf_level) / 2
}

#' Has the share of unusable replications reached the caution share?
#'
#' A share equal to the tail area counts (25 of 1,000 at 0.95), whatever the
#' floating point of `(1 - conf_level) / 2`.
#'
#' @param failed Replications or draws set aside.
#' @param attempted Replications or draws attempted.
#' @param conf_level The interval's level.
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_bootstrap_caution_reached <- function(failed, attempted, conf_level = 0.95) {
  isTRUE(failed > 0 && failed >= attempted * sieve_bootstrap_caution_share(conf_level) - 1e-9)
}

#' A sample quantile by position
#'
#' The value at (possibly fractional) position `h` of sorted values, by
#' linear interpolation between neighboring order statistics: the rule
#' behind `stats::quantile(type = 7)`, where the position of the `p`
#' quantile of `n` values is `(n - 1) p + 1`. A position before the first
#' value or after the last has no value in the sample.
#'
#' @param sorted Values in increasing order.
#' @param h A position, counted from one.
#' @return The value, or `-Inf` or `Inf` when the position is outside the
#'   sample.
#' @author Ken Kelley
#' @noRd
sieve_quantile_at <- function(sorted, h) {
  n <- length(sorted)
  if (!n || h < 1 - 1e-9) {
    return(-Inf)
  }
  if (h > n + 1e-9) {
    return(Inf)
  }
  h <- min(max(h, 1), n)
  lo <- floor(h)
  hi <- ceiling(h)
  sorted[lo] + (h - lo) * (sorted[hi] - sorted[lo])
}

#' Percentile limits and what the unusable draws could have done to them
#'
#' The percentile limits are quantiles of the draws that were used, so they
#' condition on the fit having succeeded. The draws that could not be used
#' are not missing at random: they are the resamples in which the model
#' failed. Had each of them produced a value, the limits would be quantiles
#' of all the draws attempted. Where those values would have fallen is
#' unknown, but the two extreme cases bound the answer: all of them below
#' every used draw, or all of them above. `lower_min` and `lower_max` (and
#' the same for the upper limit) are the limit under those two cases, cut
#' to the range the index can take. With no unusable draw the range
#' collapses onto the limit.
#'
#' @param x The used bootstrap estimates of one index.
#' @param attempted The number of draws attempted.
#' @param conf_level Confidence level.
#' @param range The smallest and largest value the index can take.
#' @return A list with `lower`, `upper`, `se` (the standard deviation of
#'   the used draws), `used`, `lower_min`, `lower_max`, `upper_min`, and
#'   `upper_max`.
#' @author Ken Kelley
#' @noRd
sieve_percentile_limits <- function(x, attempted, conf_level = 0.95, range = c(0, 1)) {
  x <- sort(x[is.finite(x)])
  used <- length(x)
  out <- list(
    lower = NA_real_, upper = NA_real_, se = NA_real_, used = used,
    lower_min = NA_real_, lower_max = NA_real_,
    upper_min = NA_real_, upper_max = NA_real_
  )
  if (used < 2L) {
    return(out)
  }
  tail_area <- (1 - conf_level) / 2
  out$lower <- sieve_quantile_at(x, (used - 1) * tail_area + 1)
  out$upper <- sieve_quantile_at(x, (used - 1) * (1 - tail_area) + 1)
  out$se <- stats::sd(x)
  set_aside <- attempted - used
  h_lower <- (attempted - 1) * tail_area + 1
  h_upper <- (attempted - 1) * (1 - tail_area) + 1
  clip <- function(v) min(max(v, range[1]), range[2])
  out$lower_min <- clip(sieve_quantile_at(x, h_lower - set_aside))
  out$lower_max <- clip(sieve_quantile_at(x, h_lower))
  out$upper_min <- clip(sieve_quantile_at(x, h_upper - set_aside))
  out$upper_max <- clip(sieve_quantile_at(x, h_upper))
  out
}

#' What a fitted lavaan model was asked to be
#'
#' The choices that define an analysis, read from the fitted object itself:
#' estimator, standard errors, test statistic, missing data treatment,
#' scaling, the ordered variables, and the free loadings. Two fits with the
#' same signature are the same model fitted the same way to (possibly)
#' different data.
#'
#' The test statistics are those that define the fit: Browne's residual
#' based statistics (`"browne.residual.nt.model"`, which lavaan 0.7-3 lists
#' beside the standard test of every ML fit by default, and
#' `"browne.residual.adf"`) are set aside. They are further tests of the
#' model computed from its residuals, which change no estimate, standard
#' error, standard or corrected test statistic, or fit index SIEVER reports
#' (`sieve_lavaan_corrected_tests()`), and SIEVER's own ML fits with FIML
#' ask for the standard test alone (`sieve_spec_default_test()`). So an ML
#' FIML fit made with lavaan's default tests and the same fit made by
#' SIEVER have one signature, as `sieve_lavaan_signature_args()` also
#' treats them.
#'
#' @param fit A fitted lavaan object.
#' @return A list with `estimator`, `se`, `test`, `missing`, `std_lv`,
#'   `orthogonal`, `ordered`, `observed`, `latent`, and `loadings`.
#' @author Ken Kelley
#' @noRd
sieve_lavaan_signature <- function(fit) {
  opt <- lavaan::lavInspect(fit, "options")
  pt <- lavaan::parTable(fit)
  free <- pt$op == "=~" & pt$free > 0L
  test <- as.character(opt$test)
  list(
    estimator = as.character(opt$estimator),
    se = as.character(opt$se),
    test = test[is.na(test) | !startsWith(test, "browne.residual")],
    missing = as.character(opt$missing),
    std_lv = isTRUE(opt$std.lv),
    orthogonal = isTRUE(opt$orthogonal),
    ordered = as.character(lavaan::lavNames(fit, "ov.ord")),
    observed = union(as.character(lavaan::lavNames(fit, "ov")), opt$aux %||%
      attr(fit, "sieve_spec", exact = TRUE)$engine_args$aux),
    aux = opt$aux %||% attr(fit, "sieve_spec", exact = TRUE)$engine_args$aux,
    latent = as.character(lavaan::lavNames(fit, "lv")),
    loadings = paste(pt$lhs[free], pt$op[free], pt$rhs[free])
  )
}

#' The lavaan arguments that reproduce a signature
#'
#' Names the estimator the way a user would write it: lavaan reports MLR as
#' maximum likelihood with Huber-White standard errors and a Yuan-Bentler
#' type scaled test, and WLSMV as diagonally weighted least squares with
#' robust standard errors and a scaled and shifted test. Any other
#' combination is written out in full.
#'
#' @param signature A signature from `sieve_lavaan_signature()`.
#' @return A named list of arguments for [lavaan::cfa()], without `model`
#'   and `data`.
#' @author Ken Kelley
#' @noRd
sieve_lavaan_signature_args <- function(signature) {
  # Browne's normal-theory residual statistic, which lavaan lists by
  # default for ML, does not make an ML fit a robust one.
  scaled <- sieve_lavaan_corrected_tests(signature$test)
  is <- function(estimator, se, test) {
    identical(signature$estimator, estimator) && identical(signature$se, se) &&
      identical(scaled, test)
  }
  fiml <- isTRUE(as.character(signature$missing %||% "")[1L] %in% c("ml", "ml.x", "fiml"))
  how <- if (is("ML", "robust.huber.white", "yuan.bentler.mplus")) {
    list(estimator = "MLR")
  } else if (is("DWLS", "robust.sem", "scaled.shifted")) {
    list(estimator = "WLSMV")
  } else if (is("ML", "standard", character(0))) {
    # Under FIML with continuous items the standard test alone, as SIEVER
    # fits the point estimate (sieve_spec_default_test()): a refit with
    # lavaan's default tests would compute Browne's residual statistic for
    # every draw, which SIEVER never reports and which under FIML takes most
    # of the fit's time.
    c(list(estimator = "ML"), if (fiml && !length(signature$ordered)) list(test = "standard"))
  } else {
    # Every statistic beyond the standard one that defines the fit (the
    # signature sets Browne's residual tests aside), so that the refit has
    # the same signature as the fit.
    extra <- setdiff(signature$test, "standard")
    list(
      estimator = signature$estimator, se = signature$se,
      test = if (length(extra)) extra else "standard"
    )
  }
  c(how, if (length(signature$aux)) list(aux = signature$aux), list(
    ordered = if (length(signature$ordered)) signature$ordered,
    std.lv = signature$std_lv,
    orthogonal = signature$orthogonal,
    missing = if (signature$missing %in% c("ml", "fiml")) "fiml" else signature$missing
  ))
}

#' The comment beside `test = "standard"` in a refit written from a signature
#'
#' The reason of `sieve_default_test_comment()`, in words that stand alone:
#' the bootstrap scripts name no SIEVER function and do not depend on it.
#'
#' @param args A result of `sieve_lavaan_signature_args()`.
#' @return One comment line when the refit asks for the standard test alone
#'   under ML with FIML, otherwise `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_signature_test_comment <- function(args) {
  if (!identical(args$estimator, "ML") || !identical(args$test, "standard")) {
    return(NULL)
  }
  paste0(
    "# test = \"standard\": the normal-theory model test, as the point estimate was fit. lavaan (0.7-3 and later) ",
    "would also compute Browne's residual-based test for every replication, which no index here reads and which under ",
    "FIML takes most of each fit's time; the estimates are the same either way."
  )
}

#' The acceptance rule of the bifactor bootstrap, in words
#'
#' One rule, fixed before any resample is drawn, and the same for every
#' index and for the point estimate.
#'
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_bifactor_boot_rule <- function() {
  paste0(
    "A replication is used only when its fit was returned, converged, has standard errors ",
    "(an identified solution), and yields finite proportion indices between zero and one. ",
    "Defined indices from improper solutions are retained and counted separately. ",
    "Every displayed index, including H, must be finite and between zero and one. The same replications are used for every index."
  )
}

#' Read one fitted bifactor model for the bootstrap
#'
#' Applies the acceptance rule of `sieve_bifactor_boot_rule()` to the
#' result of one call of the fitting function, and collects the indices.
#' The outcome is one of `"used"`, `"failed"` (no fit was returned),
#' `"not_converged"`, `"not_identified"`, `"inadmissible"`, and
#' `"no_indices"` (a usable fit whose indices could not all be computed);
#' the first five follow `sieve_fit_status()`, which is what the other
#' tabs report.
#'
#' @param out A result of the fitting function (see [sieve_fit_bifactor()]):
#'   a list with `fit`, `indices`, `improper`, and `error`.
#' @param keys A data frame with `row` and `index`: the cells of the
#'   indices table to read, or `NULL` to read every finite cell.
#' @param signature The point estimate's signature, or `NULL` when this is
#'   the point estimate.
#' @param error An error raised by the fitting function itself.
#' @return A list with `outcome`, `returned`, `converged`, `admissible`,
#'   `used`, `message`, `status` (from `sieve_fit_status()`), `values`
#'   (the indices read, in `keys` order), and `keys`.
#' @author Ken Kelley
#' @noRd
sieve_bifactor_boot_read <- function(out, keys = NULL, signature = NULL, error = NULL) {
  columns <- c("ECV", "omega", "omega_H", "omega_HS", "PUC", "H")
  fit <- out$fit
  status <- sieve_fit_status(fit, error = error %||% out$error)
  read <- list(
    outcome = "failed", returned = !is.null(fit), converged = FALSE, admissible = FALSE,
    used = FALSE, message = status$message, status = status,
    values = if (is.null(keys)) numeric(0) else rep(NA_real_, nrow(keys)), keys = keys
  )
  if (is.null(fit)) {
    return(read)
  }
  read$converged <- isTRUE(status$converged)
  if (!is.null(signature) && !identical(sieve_lavaan_signature(fit), signature)) {
    # Cannot happen while the specification depends on the ledger alone; if
    # it ever does, the draw is not an estimate of the same thing, and no fit
    # of the model in question was returned.
    read$message <- "The resample was fitted with a different specification from the point estimate."
    read$returned <- FALSE
    read$converged <- FALSE
    return(read)
  }
  indices <- out$indices
  if (!is.null(indices)) {
    cells <- as.matrix(indices[, intersect(columns, names(indices)), drop = FALSE])
    if (is.null(keys)) {
      at <- which(is.finite(t(cells)), arr.ind = TRUE)
      keys <- data.frame(
        row = unname(at[, 2]), index = colnames(cells)[at[, 1]],
        stringsAsFactors = FALSE
      )
      read$keys <- keys
    }
    inside <- keys$row <= nrow(cells) & keys$index %in% colnames(cells)
    read$values <- rep(NA_real_, nrow(keys))
    read$values[inside] <- cells[cbind(keys$row[inside], match(keys$index[inside], colnames(cells)))]
  }
  improper <- isTRUE(out$improper)
  read$admissible <- identical(status$code, "ok") && !improper
  if (!status$code %in% c("ok", "inadmissible")) {
    read$outcome <- status$code
    return(read)
  }
  improper <- improper || identical(status$code, "inadmissible")
  if (improper) {
    read$outcome <- "inadmissible"
    read$message <- paste0(
      "The solution is improper: a standardized loading exceeds one or a residual ",
      "variance is negative."
    )
  }
  proportions <- read$values
  if (is.null(indices) || !length(read$values) || any(!is.finite(proportions)) ||
    any(proportions < 0 | proportions > 1) ||
    (!improper && any(!is.finite(read$values)))) {
    read$outcome <- "no_indices"
    read$message <- "The model was fitted, but not every bifactor index could be computed from it."
    return(read)
  }
  read$outcome <- if (improper) "inadmissible" else "used"
  read$used <- TRUE
  if (!improper) read$message <- ""
  read
}

#' Plan a bootstrap of the bifactor indices
#'
#' Fits the point estimate with `fit_fun`, reads from the fitted object the
#' rows it used and the specification it was fitted with, and fixes
#' everything a draw needs: the pool of rows to resample, the acceptance
#' rule, and one seed per draw. Nothing is resampled here.
#'
#' The pool is the set of rows the point estimate used. Under full
#' information maximum likelihood that is every row with at least one
#' observed item, kept with its pattern of observed and missing items, so
#' a resample has the missing data the sample has. Under listwise deletion
#' (which lavaan applies to ordered or binary items) it is the complete
#' rows. Rows the point estimate did not use hold no information for it
#' and are not resampled, so appending such rows changes nothing.
#'
#' The plan is `"unavailable"`, with the reason, when the point estimate
#' does not pass the acceptance rule itself: an interval from converged,
#' admissible resamples would not describe a solution that is neither.
#'
#' @param data A data frame holding the item columns.
#' @param meta The item ledger.
#' @param B Number of bootstrap draws.
#' @param seed Optional seed (a single number, or `NULL` to have one drawn
#'   from the caller's stream and recorded).
#' @param conf_level Confidence level, in (0, 1).
#' @param fit_fun The function that fits the model: called as
#'   `fit_fun(data, meta)` and returning a list with `fit`, `indices`,
#'   `improper`, `syntax`, and `error`.
#' @param cluster Optional cluster labels aligned to the rows of `data`; a
#'   declared cluster design uses SIEVER's whole-cluster engine.
#' @param original_rows Positions of the rows of `data` in the session's
#'   item frame.
#' @param engine `"dmar"` (DMAR's `ci_bifactor_indices()`, the default) or
#'   `"siever"` (SIEVER's aligned engine, which `fit_fun` drives replication
#'   by replication; used for a declared cluster design whatever is asked).
#' @return A list of class `sieve_bifactor_boot_plan` with `status` (`code`:
#'   `"ready"` or `"unavailable"`; `message`), `point` (the reading of the
#'   point estimate, its `indices` table, `signature`, and `syntax`),
#'   `quantities` (one row per index: `key`, `row`, `factor`, `index`,
#'   `estimate`, `fixed`), `pool`, `n` (`rows`, `observed`, `empty`,
#'   `complete`, `used`), `rows` (`"observed"` or `"complete"`), `B`,
#'   `conf_level`, `rng` (the seed record), `draw_seeds`, `seconds_per_fit`,
#'   `rule`, `min_used`, `caution_share`, `meta`, `fit_fun`, and `key`.
#' @author Ken Kelley
#' @noRd
sieve_bifactor_boot_plan <- function(data, meta, B = 1000L, seed = NULL, conf_level = 0.95,
                                     fit_fun = sieve_fit_bifactor, cluster = NULL, original_rows = seq_len(nrow(data)),
                                     engine = c("dmar", "siever")) {
  engine <- match.arg(engine)
  # DMAR resamples independent cases only: a declared cluster design is
  # resampled in whole clusters by SIEVER's own engine.
  if (!is.null(cluster)) engine <- "siever"
  conf_level <- sieve_check_conf_level(conf_level)
  if (!is.numeric(B) || length(B) != 1L || !is.finite(B) || B < 1 || B != floor(B) || B > .Machine$integer.max) {
    stop("`B` must be a single positive whole number.", call. = FALSE)
  }
  sieve_bootstrap_row_references(original_rows, nrow(data))
  B <- as.integer(B)
  seed <- sieve_seed_input(seed)
  plan <- list(
    kind = "bifactor_bootstrap",
    status = list(code = "unavailable", message = "Not planned."),
    point = NULL, quantities = NULL, pool = NULL,
    n = list(
      rows = NROW(data), observed = NA_integer_, empty = NA_integer_,
      complete = NA_integer_, used = NA_integer_
    ),
    rows = NA_character_, original_rows = original_rows, B = B, conf_level = conf_level, rng = NULL,
    draw_seeds = integer(0), seconds_per_fit = NA_real_,
    engine = engine,
    rule = if (identical(engine, "dmar")) sieve_dmar_boot_rule("bifactor", conf_level) else sieve_bifactor_boot_rule(),
    min_used = sieve_bootstrap_min_draws(conf_level),
    caution_share = sieve_bootstrap_caution_share(conf_level),
    meta = meta, fit_fun = fit_fun, key = NA_character_
  )
  class(plan) <- "sieve_bifactor_boot_plan"
  unavailable <- function(why) {
    plan$status <- list(code = "unavailable", message = why)
    plan
  }

  if (identical(engine, "dmar")) {
    request <- sieve_dmar_boot_request(B, conf_level = conf_level)
    if (!request$ok) return(unavailable(request$message))
  }

  started <- proc.time()[["elapsed"]]
  run <- sieve_capture(fit_fun(data, meta))
  plan$seconds_per_fit <- proc.time()[["elapsed"]] - started
  point <- sieve_bifactor_boot_read(run$value %||% list(), error = run$error)
  plan$point <- c(point, list(indices = run$value$indices, syntax = run$value$syntax %||% ""))
  if (!point$returned) {
    return(unavailable(paste("The bifactor model could not be fitted:", point$message)))
  }
  fit <- run$value$fit
  # DMAR's bootstrap refits this fit; a declared cluster design is
  # resampled in whole clusters by SIEVER's own engine instead.
  plan$fit <- fit
  plan$name_map <- run$value$name_map %||% character(0)
  plan$point$signature <- sieve_lavaan_signature(fit)
  if (identical(point$outcome, "no_indices")) {
    return(unavailable(paste(
      "The bifactor indices could not be computed for the fitted model, so there is",
      "nothing to attach an interval to."
    )))
  }
  if (!point$used) {
    return(unavailable(paste(
      point$message,
      "The rule every resample must pass applies to the estimate too: an interval from",
      "converged, admissible resamples would not describe this solution. Respecify the",
      "model, then bootstrap."
    )))
  }
  # The rows the point estimate used, by the missing data treatment lavaan
  # reports for it, checked against the cases lavaan says it used.
  observed <- plan$point$signature$observed
  X <- sieve_reliability_data(data, observed)
  counts <- sieve_reliability_counts(X[, setdiff(observed, plan$point$signature$aux), drop = FALSE])
  seen <- unname(rowSums(!is.na(as.matrix(X))))
  listwise <- identical(plan$point$signature$missing, "listwise")
  keep <- if (listwise) seen == ncol(X) else seen > 0L
  plan$rows <- if (listwise) "complete" else "observed"
  plan$pool <- X[keep, , drop = FALSE]
  if (!is.null(cluster) && length(cluster) != nrow(data)) return(unavailable("Cluster labels must match the full supplied data before case selection."))
  partition <- sieve_capture(sieve_bootstrap_partition(nrow(plan$pool), if (!is.null(cluster)) cluster[keep], original_rows[keep]))
  if (!partition$ok) return(unavailable(partition$error))
  plan$sampling <- partition$value
  plan$n[c("rows", "observed", "empty", "complete")] <-
    counts[c("rows", "observed", "empty", "complete")]
  plan$n$used <- as.integer(sum(lavaan::lavInspect(fit, "nobs")))
  # lavaan lists the rows it read; under full information maximum likelihood
  # that list still holds the rows with no observed item, which it counts
  # out of the sample, so they are left out of the comparison as well.
  cases <- tryCatch(lavaan::lavInspect(fit, "case.idx"), error = function(e) NULL)
  same_rows <- nrow(plan$pool) == plan$n$used &&
    (!is.numeric(cases) || identical(as.integer(cases[seen[cases] > 0L]), which(keep)))
  if (!same_rows) {
    return(unavailable(sprintf(
      paste0(
        "The rows the bootstrap would resample (%d) are not the rows the point estimate ",
        "used (%d), so the two would not describe the same sample."
      ),
      nrow(plan$pool), plan$n$used
    )))
  }
  indices <- plan$point$indices
  keys <- point$keys
  plan$quantities <- data.frame(
    key = paste(indices$factor[keys$row], keys$index, sep = ": "),
    row = keys$row,
    factor = as.character(indices$factor[keys$row]),
    index = keys$index,
    estimate = point$values,
    # PUC is a function of which item loads on which group factor, and of
    # nothing estimated: resampling respondents cannot move it.
    fixed = keys$index == "PUC",
    stringsAsFactors = FALSE
  )
  seeded <- sieve_with_seed(seed, sample.int(.Machine$integer.max, B))
  plan$rng <- seeded$rng
  plan$draw_seeds <- seeded$value
  plan$key <- sieve_hash(list(
    data = sieve_data_key(plan$pool), meta = sieve_meta_key(meta),
    signature = plan$point$signature, sampling = plan$sampling, B = B, seed = plan$rng$seed, conf_level = conf_level
  ))
  plan$status <- list(code = "ready", message = "Ready.")
  plan
}

#' The rows of one bootstrap draw
#'
#' Draw `b` resamples the pool with replacement on a stream seeded with the
#' draw's own seed, so its rows depend on the run's seed and on `b` only:
#' not on how many draws are run, in what order, or on anything a fit does.
#'
#' @param plan A plan from `sieve_bifactor_boot_plan()`.
#' @param b The draw number, from 1 to `plan$B`.
#' @return An integer vector of row positions in `plan$pool`.
#' @author Ken Kelley
#' @noRd
sieve_bifactor_boot_rows <- function(plan, b) {
  n <- nrow(plan$pool)
  sieve_with_seed(plan$draw_seeds[b], sieve_bootstrap_rows(plan$sampling %||% sieve_bootstrap_partition(n)))$value
}

#' Fit one bootstrap draw and record what happened
#'
#' @param plan A plan from `sieve_bifactor_boot_plan()`.
#' @param b The draw number.
#' @return A list with `draw`, `seed`, `outcome`, `returned`, `converged`,
#'   `admissible`, `used`, `message`, and `values` (the indices, in the
#'   order of `plan$quantities`).
#' @author Ken Kelley
#' @noRd
sieve_bifactor_boot_draw <- function(plan, b) {
  rows <- sieve_bifactor_boot_rows(plan, b)
  resample <- plan$pool[rows, , drop = FALSE]
  rownames(resample) <- NULL
  # The fit runs with the caller's stream held aside, so nothing an engine
  # might draw can move it.
  run <- withr::with_preserve_seed(sieve_capture(plan$fit_fun(resample, plan$meta)))
  read <- sieve_bifactor_boot_read(
    run$value %||% list(),
    keys = plan$quantities[, c("row", "index")],
    signature = plan$point$signature, error = run$error
  )
  list(
    draw = as.integer(b), seed = plan$draw_seeds[b], outcome = read$outcome,
    returned = read$returned, converged = read$converged,
    admissible = read$admissible, used = read$used, message = read$message,
    values = read$values
  )
}

#' Summarize the draws of a bifactor bootstrap
#'
#' Forms the percentile interval of every index from the draws that passed
#' the acceptance rule, and reports what happened to all of them. The
#' counts nest: `attempted >= returned >= converged >= admissible >= used`.
#' The interval is formed only when at least
#' `sieve_bootstrap_min_draws()` draws were used. An index that the model
#' fixes (PUC) has no interval: it is reported as fixed, not as an interval
#' of zero width.
#'
#' Because the limits come from the usable draws alone, they condition on
#' the fit having succeeded. `unusable_share` is the share of attempted
#' draws that could not be used, `caution` is `TRUE` when it reaches
#' `sieve_bootstrap_caution_share()`, and the `*_min` and `*_max` columns
#' of the table bound each limit over where the unusable draws could have
#' fallen (see `sieve_percentile_limits()`).
#'
#' @param plan A plan from `sieve_bifactor_boot_plan()`.
#' @param draws A list of draws from `sieve_bifactor_boot_draw()`: the
#'   first so many draws of the plan.
#' @param seconds Time the draws took.
#' @return A list of class `sieve_bifactor_boot` with `status` (`code`:
#'   `"ok"` or `"failed"`; `message`), `uncertainty` (`status`:
#'   `"completed"`, `"failed"`, or `"unavailable"`; `method`, `label`,
#'   `se_type`, `information`, `transformation`, `reason`, `B`,
#'   `B_attempted`, `B_used`, `rng`, `seconds`), `table` (one row per
#'   index, at full precision: `factor`, `index`, `estimate`, `lower`,
#'   `upper`, `se`, `interval`, `status`, `used`, `mc_se`, `lower_min`,
#'   `lower_max`, `upper_min`, `upper_max`), `counts` (`requested`,
#'   `attempted`, `returned`, `converged`, `admissible`, `used`, and
#'   `outcome`, the number of draws with each outcome), `draws` (one row
#'   per draw: `draw`, `seed`, `outcome`, `returned`, `converged`,
#'   `admissible`, `used`, `message`), `estimates` (a matrix, one row per
#'   draw and one column per index; the row of a draw that was not used
#'   holds whatever its fit returned, kept so the draws can be examined,
#'   and enters no interval), `reasons` (why draws were unusable, with
#'   counts), `unusable_share`, `caution_share`, `caution`, `complete`
#'   (whether every requested draw was attempted), `rule`, `conf_level`,
#'   `n`, `rows`, `point`, `code` (the recorded R), `versions`, and `key`.
#' @author Ken Kelley
#' @noRd
sieve_bifactor_boot_summary <- function(plan, draws = list(), seconds = NA_real_) {
  ready <- identical(plan$status$code, "ready")
  q <- plan$quantities
  attempted <- length(draws)
  outcomes <- c("used", "failed", "not_converged", "not_identified", "inadmissible", "no_indices")
  flag <- function(name) vapply(draws, function(d) isTRUE(d[[name]]), logical(1))
  outcome <- vapply(draws, function(d) d$outcome, character(1))
  frame <- data.frame(
    draw = vapply(draws, function(d) d$draw, integer(1)),
    seed = vapply(draws, function(d) d$seed, integer(1)),
    outcome = outcome,
    returned = flag("returned"), converged = flag("converged"),
    admissible = flag("admissible"), used = flag("used"),
    message = vapply(draws, function(d) d$message, character(1)),
    stringsAsFactors = FALSE
  )
  estimates <- if (ready && attempted) {
    matrix(unlist(lapply(draws, function(d) d$values)),
      nrow = attempted, byrow = TRUE,
      dimnames = list(NULL, q$key)
    )
  } else {
    matrix(NA_real_, nrow = 0L, ncol = NROW(q), dimnames = list(NULL, q$key))
  }
  used <- sum(frame$used)
  counts <- list(
    requested = plan$B, attempted = attempted, returned = sum(frame$returned),
    converged = sum(frame$converged), admissible = sum(frame$admissible), used = used,
    improper_kept = sum(frame$used & !frame$admissible),
    outcome = table(factor(outcome, levels = outcomes))
  )
  unusable <- frame[!frame$used, , drop = FALSE]
  reasons <- if (nrow(unusable)) {
    tally <- as.data.frame(table(outcome = unusable$outcome, message = unusable$message),
      stringsAsFactors = FALSE
    )
    tally <- tally[tally$Freq > 0L, , drop = FALSE]
    names(tally)[3] <- "draws"
    tally <- tally[order(-tally$draws, tally$outcome, tally$message), , drop = FALSE]
    rownames(tally) <- NULL
    tally
  } else {
    data.frame(
      outcome = character(0), message = character(0), draws = integer(0),
      stringsAsFactors = FALSE
    )
  }
  method <- sieve_reliability_interval(
    if (identical(plan$point$signature$missing, "listwise")) "listwise" else "fiml",
    "percentile"
  )
  out <- list(
    kind = "bifactor_bootstrap",
    status = list(code = "ok", message = "Estimated."),
    uncertainty = c(
      list(status = "failed", requested = "percentile"), method,
      list(
        reason = NULL, B = plan$B, B_attempted = attempted, B_used = used,
        rng = plan$rng, seconds = seconds
      )
    ),
    table = NULL, counts = counts, draws = frame, estimates = estimates, reasons = reasons,
    unusable_share = if (attempted) (attempted - used) / attempted else NA_real_,
    caution_share = plan$caution_share, caution = FALSE,
    complete = attempted >= plan$B,
    rule = plan$rule, conf_level = plan$conf_level, n = plan$n, rows = plan$rows,
    point = plan$point, sampling = plan$sampling, code = character(0),
    versions = sieve_reliability_versions(), key = plan$key
  )
  class(out) <- "sieve_bifactor_boot"
  if (!ready) {
    out$status <- list(code = "failed", message = plan$status$message)
    out$uncertainty$status <- "unavailable"
    out$uncertainty$reason <- plan$status$message
    return(out)
  }
  enough <- used >= plan$min_used
  # Compared as counts, so a share that equals the tail area exactly (one
  # replication in forty at 95 percent) is not lost to rounding error.
  affected <- attempted - used + counts$improper_kept
  out$caution <- affected > 0L && affected >= attempted * plan$caution_share - 1e-9
  rows <- lapply(seq_len(nrow(q)), function(j) {
    used_here <- sum(frame$used & is.finite(estimates[, j]))
    base <- data.frame(
      factor = q$factor[j], index = q$index[j], estimate = q$estimate[j],
      lower = NA_real_, upper = NA_real_, se = NA_real_,
      interval = method$label, status = "failed", used = used_here, mc_se = NA_real_,
      lower_min = NA_real_, lower_max = NA_real_, upper_min = NA_real_,
      upper_max = NA_real_, stringsAsFactors = FALSE
    )
    if (!is.finite(q$estimate[j])) {
      base$interval <- "undefined for the fitted solution"
      base$status <- "unavailable"
      return(base)
    }
    if (q$fixed[j]) {
      base$interval <- "fixed by the model"
      base$status <- "fixed"
      return(base)
    }
    if (used_here < plan$min_used) {
      # The word the Reliability tab uses for an interval that was attempted
      # and could not be formed; the reason is in `uncertainty$reason`.
      base$interval <- "failed"
      return(base)
    }
    lim <- sieve_percentile_limits(estimates[frame$used, j], attempted, plan$conf_level)
    base$lower <- lim$lower
    base$upper <- lim$upper
    base$se <- lim$se
    base$mc_se <- sieve_bootstrap_mc_error(used_here, plan$conf_level) * lim$se
    base[c("lower_min", "lower_max", "upper_min", "upper_max")] <-
      lim[c("lower_min", "lower_max", "upper_min", "upper_max")]
    base$status <- "completed"
    base
  })
  out$table <- do.call(rbind, rows)
  rownames(out$table) <- NULL
  if (enough) {
    out$uncertainty$status <- "completed"
  } else {
    out$uncertainty$reason <- paste(
      if (used == attempted) {
        sprintf("Only %d replications were completed.", attempted)
      } else {
        sprintf("Only %d of the %d replications attempted could be used.", used, attempted)
      },
      sprintf(
        paste0(
          "A %s percent percentile interval needs at least %d usable replications; with ",
          "fewer, even the most extreme replication cuts off, on average, more than the tail a ",
          "limit stands for (1 / (B + 1) of the bootstrap distribution, for B replications)."
        ),
        format(plan$conf_level * 100), plan$min_used
      )
    )
  }
  out$code <- sieve_bifactor_boot_code(plan, attempted)
  out
}

#' The DMAR call of a bifactor bootstrap
#'
#' `DMAR::ci_bifactor_indices()` on the point estimate's fit: the general
#' factor named, the model-implied denominator (the indices SIEVER reports,
#' `bifactor_indices()`'s default), the percentile interval, the plan's
#' level, B and seed, DMAR's processes, and `improper = "keep"`, SIEVER's
#' rule (DMAR's own default for these indices is `"drop"`).
#'
#' @param plan A ready plan from `sieve_bifactor_boot_plan()`.
#' @param ncpus Processes DMAR spreads the replications over.
#' @return A list with `fun` and `args`, as `sieve_dmar_boot_run()` takes it.
#' @author Ken Kelley
#' @noRd
sieve_bifactor_boot_dmar_call <- function(plan, ncpus = 1L) {
  list(fun = "ci_bifactor_indices", args = list(
    fit = plan$fit, general = plan$point$signature$latent[1],
    denominator = "model_implied", ci_method = "percentile",
    conf_level = plan$conf_level, B = plan$B, seed = plan$rng$seed,
    ncpus = as.integer(ncpus), improper = "keep"
  ))
}

#' The DMAR key of each of a plan's indices
#'
#' DMAR names a factor by its lavaan name; the plan's indices table names
#' it for display. The key is DMAR's `"factor:index"`.
#'
#' @param plan A plan from `sieve_bifactor_boot_plan()`.
#' @return A character vector, one per row of `plan$quantities`.
#' @author Ken Kelley
#' @noRd
sieve_bifactor_boot_dmar_keys <- function(plan) {
  q <- plan$quantities
  map <- plan$name_map
  latent <- names(map)[match(q$factor, unname(map))]
  latent[is.na(latent)] <- q$factor[is.na(latent)]
  paste(latent, q$index, sep = ":")
}

#' Summarize DMAR's bootstrap of the bifactor indices
#'
#' Reads `DMAR::ci_bifactor_indices()`'s result into the result
#' `sieve_bifactor_boot_summary()` returns, so the Bifactor tab, the report
#' and the project read it in the form SIEVER's own engine returns: one row
#' per index of the point estimate's table with DMAR's limits and standard
#' error at full precision, the replications used for it, the Monte Carlo
#' error of a limit, and how far each limit could move had the replications
#' set aside produced values (by boot's percentile rule, which DMAR's
#' limits follow); every replication's fate in SIEVER's count vocabulary
#' (`"used"`, `"inadmissible"` for an improper solution kept,
#' `"no_indices"` for one whose indices are not all proportions, `"failed"`
#' for a refit that raised an error, with DMAR's message,
#' `"not_converged"`); DMAR's caution; DMAR's rule; and the recorded DMAR
#' call. An interval is shown only from at
#' least `sieve_bootstrap_min_draws()` used replications (DMAR forms one
#' from two). PUC is fixed by the model. A stopped run (class
#' `dmar_bootstrap_cancelled`) and DMAR's refusals (class `dmar_refusal`)
#' leave no interval, with the reason, read by class.
#'
#' @param plan A ready plan from `sieve_bifactor_boot_plan()`.
#' @param run A result of `sieve_dmar_boot_run()` of
#'   `sieve_bifactor_boot_dmar_call(plan)`.
#' @param data_name The name of the item data frame in the recorded R.
#' @return A list of class `sieve_bifactor_boot` (see
#'   `sieve_bifactor_boot_summary()`), with `engine = "dmar"`, `ncpus`,
#'   and `dmar` (DMAR's table with its attributes).
#' @author Ken Kelley
#' @noRd
sieve_bifactor_boot_from_dmar <- function(plan, run, data_name = "items") {
  out <- sieve_bifactor_boot_summary(plan, list(), seconds = run$seconds)
  out$engine <- "dmar"
  out$rule <- sieve_dmar_boot_rule("bifactor", plan$conf_level)
  method <- sieve_reliability_interval(
    if (identical(plan$point$signature$missing, "listwise")) "listwise" else "fiml", "percentile"
  )
  out$uncertainty[c("label", "se_type", "information", "transformation")] <-
    method[c("label", "se_type", "information", "transformation")]
  out$uncertainty$warnings <- run$warnings
  if (!identical(plan$status$code, "ready")) {
    return(out)
  }
  outcome <- sieve_dmar_boot_outcome(run)
  if (!identical(outcome, "ok")) {
    out$status <- list(code = "failed", message = sieve_dmar_boot_failure_text(run))
    out$uncertainty$status <- if (identical(outcome, "refused")) "unavailable" else "failed"
    out$uncertainty$reason <- out$status$message
    out$uncertainty$condition_class <- run$error_class
    out$stopped <- identical(outcome, "cancelled")
    out$complete <- FALSE
    out$table <- NULL
    return(out)
  }
  r <- run$value
  sieve_dmar_require_columns(r, c("factor", "index", "estimate", "se", "ci_lower", "ci_upper"), "Bifactor bootstrap")
  bs <- attr(r, "bootstrap", exact = TRUE)
  out$rule <- sieve_dmar_boot_rule("bifactor", plan$conf_level, half = isTRUE(bs$require_half))
  ncpus <- as.integer(bs$ncpus %||% 1L)
  q <- plan$quantities
  keys <- sieve_bifactor_boot_dmar_keys(plan)
  tt <- bs$t
  status <- as.character(bs$status)
  B <- length(status)
  boot_cols <- intersect(keys, colnames(tt))
  needed <- boot_cols[!grepl(":maximal_reliability$", boot_cols)]
  used <- status %in% c("proper", "improper") &
    rowSums(!is.finite(tt[, needed, drop = FALSE])) == 0L
  estimates <- matrix(NA_real_, nrow = B, ncol = nrow(q), dimnames = list(NULL, q$key))
  for (j in seq_len(nrow(q))) {
    if (keys[j] %in% colnames(tt)) {
      estimates[, j] <- tt[, keys[j]]
    } else if (isTRUE(q$fixed[j])) {
      estimates[, j] <- ifelse(used, q$estimate[j], NA_real_)
    }
  }
  outcome_of <- ifelse(status == "proper" & used, "used",
    ifelse(status == "improper" & used, "inadmissible",
      ifelse(status == "error", "failed",
        ifelse(status == "not_converged", "not_converged", "no_indices")
      )
    )
  )
  errors <- bs$error_messages %||% integer(0)
  error_text <- if (length(errors) == 1L) {
    names(errors)
  } else if (length(errors) > 1L) {
    "The refit raised an error (DMAR's messages are listed with their counts)."
  } else {
    "The refit raised an error."
  }
  message <- unname(c(
    used = "",
    inadmissible = paste0(
      "The solution is improper: a standardized loading exceeds one or a residual ",
      "variance is negative."
    ),
    failed = error_text,
    not_converged = "The refit did not converge.",
    no_indices = paste0(
      "The model was fitted, but not every bifactor index is a proportion (an improper ",
      "solution: an omega above one, or H not defined)."
    )
  )[outcome_of])
  frame <- data.frame(
    draw = seq_len(B), status = status, outcome = outcome_of,
    returned = status != "error", converged = status %in% c("proper", "improper"),
    admissible = status == "proper", used = used, message = message,
    stringsAsFactors = FALSE
  )
  outcomes <- c("used", "failed", "not_converged", "not_identified", "inadmissible", "no_indices")
  counts <- list(
    requested = plan$B, attempted = as.integer(bs$attempted), returned = sum(frame$returned),
    converged = sum(frame$converged), admissible = sum(frame$admissible), used = sum(used),
    improper_kept = sum(used & status == "improper"),
    error_messages = errors,
    outcome = table(factor(outcome_of, levels = outcomes))
  )
  unusable <- frame[!frame$used, , drop = FALSE]
  reasons <- if (nrow(unusable)) {
    tally <- as.data.frame(table(outcome = unusable$outcome, message = unusable$message),
      stringsAsFactors = FALSE
    )
    tally <- tally[tally$Freq > 0L, , drop = FALSE]
    names(tally)[3] <- "draws"
    if (length(errors) > 1L) {
      tally <- rbind(tally[tally$outcome != "failed", , drop = FALSE],
        data.frame(outcome = "failed", message = names(errors), draws = as.integer(errors),
          stringsAsFactors = FALSE))
    }
    tally <- tally[order(-tally$draws, tally$outcome, tally$message), , drop = FALSE]
    rownames(tally) <- NULL
    tally
  } else {
    data.frame(outcome = character(0), message = character(0), draws = integer(0), stringsAsFactors = FALSE)
  }
  attempted <- counts$attempted
  out$counts <- counts
  out$draws <- frame
  out$estimates <- estimates
  out$reasons <- reasons
  out$unusable_share <- if (attempted) (attempted - counts$used) / attempted else NA_real_
  out$caution <- isTRUE(bs$caution)
  out$complete <- TRUE
  out$ncpus <- ncpus
  out$dmar <- r
  out$uncertainty$B_attempted <- attempted
  out$uncertainty$B_used <- counts$used
  out$uncertainty$ncpus <- ncpus
  rk <- paste(r$factor, r$index, sep = ":")
  rows <- lapply(seq_len(nrow(q)), function(j) {
    used_here <- sum(used & is.finite(estimates[, j]))
    base <- data.frame(
      factor = q$factor[j], index = q$index[j], estimate = q$estimate[j],
      lower = NA_real_, upper = NA_real_, se = NA_real_,
      interval = method$label, status = "failed", used = used_here, mc_se = NA_real_,
      lower_min = NA_real_, lower_max = NA_real_, upper_min = NA_real_,
      upper_max = NA_real_, stringsAsFactors = FALSE
    )
    if (!is.finite(q$estimate[j])) {
      base$interval <- "undefined for the fitted solution"
      base$status <- "unavailable"
      return(base)
    }
    if (q$fixed[j]) {
      base$interval <- "fixed by the model"
      base$status <- "fixed"
      return(base)
    }
    at <- match(keys[j], rk)
    if (used_here < plan$min_used || is.na(at) || !is.finite(r$ci_lower[at]) || !is.finite(r$ci_upper[at])) {
      base$interval <- "failed"
      return(base)
    }
    base$lower <- r$ci_lower[at]
    base$upper <- r$ci_upper[at]
    base$se <- r$se[at]
    base$mc_se <- sieve_bootstrap_mc_error(used_here, plan$conf_level) * base$se
    rng <- sieve_dmar_percentile_range(estimates[used, j], attempted, plan$conf_level, range = c(0, 1))
    base[c("lower_min", "lower_max", "upper_min", "upper_max")] <- rng[c("lower_min", "lower_max", "upper_min", "upper_max")]
    base$status <- "completed"
    base
  })
  out$table <- do.call(rbind, rows)
  rownames(out$table) <- NULL
  half_short <- isTRUE(bs$require_half) && counts$used < attempted / 2
  if (counts$used >= plan$min_used && any(out$table$status == "completed")) {
    out$uncertainty$status <- "completed"
    out$uncertainty$reason <- NULL
  } else if (half_short) {
    out$uncertainty$status <- "failed"
    out$uncertainty$reason <- sprintf(paste0(
      "Only %d of the %d replications attempted could be used, fewer than half, so DMAR forms no ",
      "interval (its half rule): the replications that returned values are a minority chosen by having ",
      "returned them, and their spread does not describe the sampling distribution of the indices."
    ), counts$used, attempted)
  } else {
    out$uncertainty$status <- "failed"
    out$uncertainty$reason <- paste(
      sprintf("Only %d of the %d replications attempted could be used.", counts$used, attempted),
      sprintf(
        paste0(
          "A %s percent percentile interval needs at least %d usable replications; with ",
          "fewer, its limits could only repeat the smallest and the largest value (boot's rule ",
          "places them below the first order statistic)."
        ),
        format(plan$conf_level * 100), plan$min_used
      )
    )
  }
  out$code <- sieve_bifactor_boot_dmar_code(plan, ncpus = ncpus, data_name = data_name)
  out
}

#' The R that reproduces DMAR's bootstrap of the bifactor indices
#'
#' Plain lavaan and DMAR: the model fitted to the rows the point estimate
#' used, with the estimator and missing data treatment read from the point
#' estimate, and `DMAR::ci_bifactor_indices()` with the seed, B, level and
#' processes of the run. DMAR resamples the rows lavaan stores with the fit,
#' so the same rows in the same order give the same resamples, counts and
#' limits.
#'
#' @param plan A plan from `sieve_bifactor_boot_plan()`.
#' @param ncpus The processes DMAR used.
#' @param data_name The name of the item data frame in the script.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_bifactor_boot_dmar_code <- function(plan, ncpus = 1L, data_name = "items") {
  sig <- plan$point$signature
  data_name <- sieve_reliability_data_code(list(aux = sig$aux), data_name)
  args <- sieve_lavaan_signature_args(sig)
  chr <- function(x) sprintf("c(%s)", paste(sprintf("\"%s\"", x), collapse = ", "))
  written <- vapply(names(args)[!vapply(args, is.null, logical(1))], function(nm) {
    value <- args[[nm]]
    sprintf("%s = %s", nm, if (length(value) > 1L) chr(value) else sieve_r_literal(value))
  }, character(1))
  call <- sieve_bifactor_boot_dmar_call(plan, ncpus = ncpus)$args
  call$fit <- NULL
  c(
    sprintf(
      paste0(
        "# Bifactor indices: DMAR's counted percentile bootstrap (B = %d, seed = %d, %d process%s), ",
        "%s%% intervals."
      ),
      plan$B, plan$rng$seed, as.integer(ncpus), if (identical(as.integer(ncpus), 1L)) "" else "es",
      format(plan$conf_level * 100)
    ),
    if (identical(plan$rows, "complete")) {
      "# The model is fitted to the complete rows, the rows the point estimate used (listwise deletion);"
    } else {
      "# The model is fitted to the rows with an observed item or auxiliary, the rows the point estimate used;"
    },
    "# DMAR resamples the rows lavaan stores with the fit and refits each with the fit's options.",
    if (length(sig$aux)) "# check.post = FALSE: SIEVER checks the admissibility of a fit with auxiliary variables itself.",
    sieve_signature_test_comment(args),
    paste0("bifactor_boot_model <- ", sieve_r_literal(plan$point$syntax)),
    sprintf("bifactor_boot_data <- %s[%s, %s, drop = FALSE]", data_name,
      if (is.null(plan$original_rows)) "" else sieve_r_literal(plan$original_rows), chr(sig$observed)),
    sieve_numeric_code("bifactor_boot_data"),
    sprintf("bifactor_boot_fit <- lavaan::cfa(bifactor_boot_model, data = bifactor_boot_data, %s%s)",
      paste(written, collapse = ", "), if (length(sig$aux)) ", check.post = FALSE" else ""),
    sieve_dmar_call_code("bifactor_boot", "ci_bifactor_indices", "bifactor_boot_fit", call)
  )
}

#' Bootstrap the Bifactor Indices
#'
#' The percentile bootstrap of the bifactor indices (explained common
#' variance, omega, omega hierarchical, omega hierarchical subscale, and H),
#' aligned with the point estimate: SIEVER fits the model
#' (`sieve_bifactor_boot_plan()` reads the rows it used, its specification
#' and its indices), and DMAR's counted bootstrap,
#' `DMAR::ci_bifactor_indices()`, resamples those rows and refits the same
#' model (see `sieve_bifactor_boot_from_dmar()` for what is reported). With
#' a declared cluster design, which DMAR does not resample, SIEVER's own
#' aligned bootstrap resamples whole clusters (`sieve_bifactor_boot_draw()`,
#' `sieve_bifactor_boot_summary()`).
#'
#' Random numbers. The run's seed (supplied, or drawn from the caller's
#' stream and recorded) is passed to DMAR, which sets it for its call only
#' and draws every resample (through boot) before any replication runs, so
#' the result does not depend on `ncpus`. The caller's stream is left as it
#' was (one draw further on when no seed was supplied).
#'
#' Stopping. `progress` is DMAR's hook, called as `progress(done, total)`
#' after each replication (each block on several processes); returning
#' `FALSE` cancels the run, and DMAR forms no interval from a canceled run,
#' so the result then holds the reason and no table. The Bifactor tab runs
#' the same call in a background process, so its page stays responsive and
#' its Stop is heard at the next replication, or at the end of the block in
#' progress on several processes. (Under a cluster design,
#' `progress` is called with the draw's record as a third argument, and a
#' stop summarizes the draws completed.)
#'
#' @details
#' Independent respondents are resampled with replacement. Under full
#' information maximum likelihood the pool retains rows with at least one
#' observed item and their missing-response patterns; under listwise deletion
#' it retains complete rows. The point estimate must itself converge, be
#' identified, be admissible, and have the requested indices. A draw must meet
#' the same rule. Unusable draws are counted with reasons; an interval is not
#' issued without the required number of usable draws for its confidence level.
#' Declared clusters are resampled as whole blocks. Repeated-observation models
#' require a separately specified longitudinal analysis.
#' @param data Numeric item responses after intended reverse scoring.
#' @param meta Ledger defining active items, constructs, and item types.
#' @param B Number of bootstrap draws.
#' @param seed Optional seed (a single number, or `NULL`).
#' @param conf_level Confidence level, in (0, 1).
#' @param progress Optional function called as the replications finish,
#'   `progress(done, total)`; returning `FALSE` stops the run.
#' @param cluster Optional independent cluster labels aligned to all supplied rows.
#' @param original_rows Positions of supplied data rows in the session item frame,
#'   retained when a preceding design filter excluded rows.
#' @param fit_fun Optional model-fitting function with the arguments and
#'   returned fit/index record of [sieve_fit_bifactor()]. The default follows
#'   ledger item types. A supplied function must use one fixed specification
#'   for the point estimate and all resamples.
#' @param ncpus Processes DMAR spreads the replications over (one by
#'   default; with more, DMAR forks workers, or uses a socket cluster on
#'   Windows). One is used when this R session has loaded processx without
#'   the environment variable `PROCESSX_NOTIFY_OLD_SIGCHLD` set, since its
#'   forked workers could then not be reaped. The result does not depend on
#'   it.
#' @param engine `"dmar"` (the default): `DMAR::ci_bifactor_indices()`
#'   refits the point estimate's model to each resample with that fit's
#'   lavaan options, so `fit_fun` fits the point estimate only. `"siever"`:
#'   SIEVER's aligned engine, which calls `fit_fun` for every replication,
#'   draws each replication's rows from its own seed (so a run stopped by
#'   `progress` holds the first draws of the full run, called then as
#'   `progress(done, total, draw)`), and is used for a declared cluster
#'   design whatever is asked.
#' @return A `sieve_bifactor_boot` list with `status`, `uncertainty` (method,
#'   RNG record, attempted and usable draws, and reasons),
#'   `table` (unrounded point estimates, limits and Monte Carlo diagnostics),
#'   `counts`, per-draw `draws` and `estimates`, `reasons`, `complete`, `rule`,
#'   `conf_level`, sample information `n` and `rows`, the `point` record,
#'   replay `code`, `versions`, and `key`. A refused point fit or insufficient
#'   usable draws retains an unavailable/failed status and its reason.
#' @author Ken Kelley
#' @examples
#' # A constructed example: one general and three independent group factors.
#' # This illustrates the interface; it is not an empirical validation study.
#' items <- withr::with_seed(1, {
#'   n <- 500L
#'   general <- stats::rnorm(n)
#'   specific <- matrix(stats::rnorm(n * 3L), n)
#'   group <- rep(1:3, each = 3L)
#'   as.data.frame(sapply(seq_len(9L), function(j) {
#'     .5 * general + .5 * specific[, group[j]] + sqrt(.5) * stats::rnorm(n)
#'   }))
#' })
#' names(items) <- paste0("item", seq_len(9L))
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("A", "B", "C"), each = 3L), facet = NA_character_,
#'   type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE)
#' # DMAR's minimum request depends on the level: 100 replications for an
#' # 80 percent percentile interval, 400 for a 95 percent one. A smaller
#' # request is refused before anything is run, and the status says why.
#' refused <- sieve_bifactor_bootstrap(items, meta, B = 50L, seed = 1, conf_level = 0.80)
#' refused$status$message
#' # The minimum request refits the model 100 times, which takes a few
#' # seconds, so it runs in an interactive session only. A study reports the
#' # level it stated beforehand.
#' if (interactive()) {
#'   boot <- sieve_bifactor_bootstrap(items, meta, B = 100L, seed = 1, conf_level = 0.80)
#'   boot$status
#'   boot$counts[c("requested", "attempted", "used")]
#'   boot$table[boot$table$factor == "general", c("index", "estimate", "lower", "upper", "interval")]
#' }
#' @export
sieve_bifactor_bootstrap <- function(data, meta, B = 1000L, seed = NULL, conf_level = 0.95,
                                     progress = NULL, fit_fun = sieve_fit_bifactor, cluster = NULL, original_rows = seq_len(nrow(data)),
                                     ncpus = 1L, engine = c("dmar", "siever")) {
  plan <- sieve_bifactor_boot_plan(data, meta,
    B = B, seed = seed, conf_level = conf_level,
    fit_fun = fit_fun, cluster = cluster, original_rows = original_rows,
    engine = match.arg(engine)
  )
  if (!identical(plan$status$code, "ready")) {
    return(sieve_bifactor_boot_summary(plan))
  }
  if (identical(plan$engine, "dmar")) {
    call <- sieve_bifactor_boot_dmar_call(plan, ncpus = sieve_dmar_boot_ncpus(ncpus))
    hook <- if (is.function(progress)) function(done, total) progress(done, total)
    return(sieve_bifactor_boot_from_dmar(plan, sieve_dmar_boot_run(call, progress = hook)))
  }
  started <- proc.time()[["elapsed"]]
  draws <- vector("list", plan$B)
  done <- 0L
  for (b in seq_len(plan$B)) {
    draws[[b]] <- sieve_bifactor_boot_draw(plan, b)
    done <- b
    if (is.function(progress) && identical(progress(b, plan$B, draws[[b]]), FALSE)) {
      break
    }
  }
  sieve_bifactor_boot_summary(plan, draws[seq_len(done)],
    seconds = proc.time()[["elapsed"]] - started
  )
}

#' The time one bootstrap draw should take
#'
#' Estimated from the fit on screen, without refitting: the time lavaan
#' recorded for that fit, plus the time it takes now to read a fitted model
#' the way a draw does (its status, its standardized solution, its
#' indices).
#'
#' @param out A result of [sieve_fit_bifactor()] that holds a fit.
#' @return A number of seconds, or `NA` when it cannot be estimated.
#' @author Ken Kelley
#' @noRd
sieve_bifactor_draw_seconds <- function(out) {
  fit <- out$fit
  if (is.null(fit)) {
    return(NA_real_)
  }
  fitting <- tryCatch(as.numeric(lavaan::lavInspect(fit, "timing")$total),
    error = function(e) NA_real_
  )
  started <- proc.time()[["elapsed"]]
  sieve_capture({
    sieve_fit_status(fit)
    sieve_bifactor_loadings(fit)
    DMAR::bifactor_indices(fit, general = lavaan::lavNames(fit, "lv")[1])
  })
  reading <- proc.time()[["elapsed"]] - started
  if (length(fitting) == 1L && is.finite(fitting)) fitting + reading else NA_real_
}

#' The R that reproduces a bifactor bootstrap
#'
#' Plain lavaan and DMAR, written from the plan the bootstrap ran from: the
#' model, the estimator and missing data treatment read from the point
#' estimate, the rows resampled, the seed, the per draw seeds, and the
#' acceptance rule. Run in a clean session it draws the same resamples and
#' returns the same counts and limits.
#'
#' @param plan A plan from `sieve_bifactor_boot_plan()`.
#' @param attempted The number of draws run (fewer than `plan$B` when the
#'   run was stopped).
#' @param data_name The name of the item data frame in the script.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_bifactor_boot_code <- function(plan, attempted = plan$B, data_name = "items") {
  sig <- plan$point$signature
  data_name <- sieve_reliability_data_code(list(aux = sig$aux), data_name)
  args <- sieve_lavaan_signature_args(sig)
  chr <- function(x) sprintf("c(%s)", paste(sprintf("\"%s\"", x), collapse = ", "))
  written <- vapply(names(args)[!vapply(args, is.null, logical(1))], function(nm) {
    value <- args[[nm]]
    sprintf("%s = %s", nm, if (length(value) > 1L) chr(value) else sieve_r_literal(value))
  }, character(1))
  q <- plan$quantities
  # The indices table has a fixed layout (general factor first, then one row
  # per group factor); when the cells read follow it, the script reads them
  # by that layout, and otherwise cell by cell.
  groups <- setdiff(unique(q$row), 1L)
  layout <- rbind(
    data.frame(
      row = 1L, index = c("ECV", "omega", "omega_H", "PUC", "H"),
      stringsAsFactors = FALSE
    ),
    data.frame(
      row = rep(groups, each = 4L),
      index = rep(c("ECV", "omega", "omega_HS", "H"), length(groups)),
      stringsAsFactors = FALSE
    )
  )
  by_layout <- identical(as.integer(q$row), as.integer(layout$row)) &&
    identical(q$index, layout$index)
  read_values <- if (by_layout) {
    c(
      "  # General factor: ECV, omega, omegaH, PUC, H. Each group factor: ECV and omega",
      "  # over its own items (the general factor's share of the group's common",
      "  # variance, and subscale omega), omegaHS, H.",
      "  values <- c(idx[1, c(\"ECV\", \"omega\", \"omega_H\", \"PUC\", \"H\")],",
      "              t(idx[-1, c(\"ECV\", \"omega\", \"omega_HS\", \"H\"), drop = FALSE]))"
    )
  } else {
    sprintf(
      "  values <- c(%s)",
      paste(sprintf("idx[%d, \"%s\"]", q$row, q$index), collapse = ", ")
    )
  }
  key_names <- sieve_c_call(sieve_chr_lit(q$key))
  conf <- sieve_r_literal(plan$conf_level)
  stopped <- attempted < plan$B
  c(
    sprintf(
      paste0(
        "# Bifactor indices: percentile bootstrap, %d draws%s, %s%% intervals. The rows ",
        "resampled"
      ), attempted,
      if (stopped) sprintf(" (stopped; %d were requested)", plan$B) else "",
      format(plan$conf_level * 100)
    ),
    if (identical(plan$rows, "complete")) {
      "# are the complete rows, which are the rows the point estimate used (listwise deletion)."
    } else {
      c(
        "# are the rows with an observed item or auxiliary, which are the rows the point",
        "# estimate used; each keeps its pattern of observed and missing items."
      )
    },
    if (length(sig$aux)) c(
      paste0("sieve_covariance_nonnegative <- ", paste(deparse(sieve_covariance_nonnegative), collapse = "\n")),
      paste0("siever_post_check <- ", paste(deparse(sieve_model_post_check), collapse = "\n"))),
    paste0("bifactor_boot_model <- ", sieve_r_literal(plan$point$syntax)),
    if (identical(plan$sampling$unit, "clustered")) paste0("bifactor_boot_blocks <- ", sieve_r_literal(plan$sampling$groups)),
    sprintf("bifactor_boot_pool <- %s[%s, %s, drop = FALSE]", data_name, if (is.null(plan$original_rows)) "" else sieve_r_literal(plan$original_rows), chr(sig$observed)),
    sieve_numeric_code("bifactor_boot_pool"),
    if (identical(plan$rows, "complete")) {
      "bifactor_boot_pool <- bifactor_boot_pool[stats::complete.cases(bifactor_boot_pool), , drop = FALSE]"
    } else {
      "bifactor_boot_pool <- bifactor_boot_pool[rowSums(!is.na(bifactor_boot_pool)) > 0, , drop = FALSE]"
    },
    "# Converged, identified replications retain every defined proportion index.",
    "# DMAR's defined H is retained; maximal reliability is not one of these indices.",
    sieve_signature_test_comment(args),
    "bifactor_boot_draw <- function(data) {",
    "  fit <- tryCatch(suppressWarnings(lavaan::cfa(bifactor_boot_model, data = data,",
    sprintf("    %s)), error = function(e) NULL)", paste(written, collapse = ", ")),
    "  if (is.null(fit) || !isTRUE(lavaan::lavInspect(fit, \"converged\"))) return(NULL)",
    "  free <- lavaan::parameterEstimates(fit, ci = FALSE, remove.nonfree = TRUE)",
    "  if (!all(is.finite(free$se))) return(NULL)",
    "  est <- lavaan::parameterEstimates(fit, ci = FALSE)",
    "  improper <- any(est$est[est$op == \"~~\" & est$lhs == est$rhs] < 0, na.rm = TRUE)",
    if (length(sig$aux)) "  improper <- improper || !siever_post_check(fit)" else
      "  improper <- improper || !isTRUE(suppressWarnings(lavaan::lavInspect(fit, \"post.check\")))",
    sprintf("  idx <- suppressWarnings(DMAR::bifactor_indices(fit, general = \"%s\"))", sig$latent[1]),
    "  improper <- improper || isTRUE(attr(idx, \"improper\"))",
    "  idx <- as.matrix(as.data.frame(idx)[, c(\"ECV\", \"omega\", \"omega_H\", \"omega_HS\", \"PUC\", \"H\")])",
    read_values,
    "  proportions <- values",
    "  if (all(is.finite(proportions)) && all(proportions >= 0 & proportions <= 1) && (improper || all(is.finite(values)))) structure(values, improper = improper) else NULL",
    "}",
    "# One seed per draw, from the seed of the run, so any draw can be refitted alone.",
    sieve_seed_code(plan$rng),
    sprintf(
      "bifactor_boot_seeds <- sample.int(.Machine$integer.max, %dL)%s", plan$B,
      if (stopped) sprintf("[seq_len(%dL)]", attempted) else ""
    ),
    "bifactor_boot_draws <- lapply(bifactor_boot_seeds, function(draw_seed) {",
    "  set.seed(draw_seed)",
    if (identical(plan$sampling$unit, "clustered"))
      "  rows <- unlist(bifactor_boot_blocks[sample.int(length(bifactor_boot_blocks), length(bifactor_boot_blocks), replace = TRUE)], use.names = FALSE)" else
      "  rows <- sample.int(nrow(bifactor_boot_pool), nrow(bifactor_boot_pool), replace = TRUE)",
    "  bifactor_boot_draw(bifactor_boot_pool[rows, , drop = FALSE])",
    "})",
    sprintf("bifactor_boot_used <- if (any(lengths(bifactor_boot_draws))) do.call(rbind, bifactor_boot_draws) else matrix(numeric(), 0L, %dL)", nrow(q)),
    "bifactor_boot_improper_kept <- sum(vapply(bifactor_boot_draws, function(x) isTRUE(attr(x, \"improper\")), logical(1)))",
    sprintf("bifactor_boot_caution <- (length(bifactor_boot_draws) - NROW(bifactor_boot_used) + bifactor_boot_improper_kept > 0L) && length(bifactor_boot_draws) - NROW(bifactor_boot_used) + bifactor_boot_improper_kept >= length(bifactor_boot_draws) * (1 - %s) / 2 - 1e-9", conf),
    paste0("colnames(bifactor_boot_used) <- ", key_names[1]),
    key_names[-1],
    "bifactor_boot_counts <- c(attempted = length(bifactor_boot_draws), used = NROW(bifactor_boot_used), improper_kept = bifactor_boot_improper_kept, caution = bifactor_boot_caution)",
    sprintf(paste0(
      "# Percentile limits from the draws used, formed only from at least %d usable draws ",
      "(fewer cannot"
    ), plan$min_used),
    sprintf(
      "# hold a %s%% limit); NULL otherwise. PUC is fixed by the model, so it has none.",
      format(plan$conf_level * 100)
    ),
    sprintf("bifactor_boot_varies <- !colnames(bifactor_boot_used) %%in%% %s", chr(q$key[q$fixed])),
    sprintf("if (NROW(bifactor_boot_used) >= %dL) apply(bifactor_boot_used[, bifactor_boot_varies, drop = FALSE], 2, function(values) {", plan$min_used),
    sprintf("  if (sum(is.finite(values)) < %dL) return(c(NA_real_, NA_real_))", plan$min_used),
    sprintf("  stats::quantile(values[is.finite(values)], probs = c((1 - %s) / 2, 1 - (1 - %s) / 2))", conf, conf),
    "}) else NULL"
  )
}

#' What a bifactor bootstrap needs said about it
#'
#' The statements that accompany the interval table: what was resampled and
#' how it was fitted, what happened to the draws, the rule that decided
#' which were used, the Monte Carlo error the number of draws leaves in a
#' limit, and, when draws could not be used, that the limits condition on
#' the fit having succeeded.
#'
#' @param result A result from `sieve_bifactor_boot_summary()`.
#' @return A list with `method`, `counts`, `rule`, `precision`, `stopped`
#'   (or `NULL`), `caution` (or `NULL`), and `reasons` (a character
#'   vector, possibly empty): plain language, with numbers formatted for
#'   reading.
#' @author Ken Kelley
#' @noRd
sieve_bifactor_boot_notes <- function(result) {
  n <- result$n
  cnt <- result$counts
  u <- result$uncertainty
  sig <- result$point$signature
  args <- sieve_lavaan_signature_args(sig)
  pool <- if (identical(result$rows, "complete")) {
    sprintf(paste0(
      "the %d complete rows, which are the rows the point estimate used (with ordered ",
      "or binary items the model is fitted to complete rows)"
    ), n$used)
  } else if (n$complete < n$used) {
    sprintf(paste0(
      "the %d rows with an observed item or auxiliary, which are the rows the point ",
      "estimate used (%d of them incomplete, each kept with its pattern of observed and ",
      "missing items)"
    ), n$used, n$used - n$complete)
  } else {
    sprintf("the %d rows the point estimate used", n$used)
  }
  dmar <- identical(result$engine, "dmar")
  method <- sprintf(
    paste0(
      "%s. Each replication resamples %s, and refits the model as the point estimate was fitted ",
      "(%s%s). Seed %d%s."
    ),
    if (dmar) {
      ncpus <- as.integer(result$ncpus %||% 1L)
      sprintf(
        "%s of %s, DMAR's counted bootstrap (DMAR::ci_bifactor_indices(), DMAR %s, improper = \"keep\", model-implied denominator, %d process%s)",
        u$label, sieve_bootstrap_unit_text(result$sampling), result$versions$DMAR %||% "",
        ncpus, if (identical(ncpus, 1L)) "" else "es"
      )
    } else {
      paste(u$label, sieve_bootstrap_unit_text(result$sampling))
    }, pool, args$estimator,
    if (identical(args$missing, "fiml")) ", full information maximum likelihood" else "",
    u$rng$seed, if (isTRUE(u$rng$supplied)) "" else " (drawn for this run and recorded)"
  )
  counts <- sprintf(paste0(
    "Replications: %d attempted, %d returned a fit, %d converged, %d converged and admissible, ",
    "%d used; %d improper solutions retained for defined indices."
  ), cnt$attempted, cnt$returned, cnt$converged, cnt$admissible, cnt$used, cnt$improper_kept %||% 0L)
  stopped <- if (!isTRUE(result$complete)) {
    sprintf(
      paste0(
        "The run was canceled after %d of the %d replications requested. Those completed ",
        "are the first %d of the full run, and the intervals use them."
      ),
      cnt$attempted, cnt$requested, cnt$attempted
    )
  }
  mc <- sieve_bootstrap_mc_error(c(max(cnt$used, 1L), 200, 10000), result$conf_level)
  precision <- sprintf(
    paste0(
      "With %d usable replications each percentile limit carries a Monte Carlo standard ",
      "error of about %.2f bootstrap standard errors, against %.2f with 200 and %.2f with ",
      "10,000. The number of replications sets how reproducible the limits are, not what ",
      "they estimate."
    ),
    cnt$used, mc[1], mc[2], mc[3]
  )
  not_used <- cnt$attempted - cnt$used
  caution <- if (isTRUE(result$caution) && (cnt$improper_kept %||% 0L) > 0L) {
    sprintf("%d improper replications were retained for defined indices and %d were set aside. Together they reach the interval's tail area; the limits depend on their treatment. Each interval reports its usable count.",
      cnt$improper_kept, not_used)
  } else if (not_used > 0L) {
    lead <- sprintf(
      paste0(
        "%d of the %d replications attempted (%s%%) could not be used, so the limits ",
        "describe the resamples in which the model could be fitted and may be too narrow: ",
        "the resamples set aside are the ones in which it failed, not a random subset."
      ),
      not_used, cnt$attempted, format(100 * result$unusable_share, digits = 3)
    )
    if (isTRUE(result$caution)) {
      paste(lead, sprintf(
        paste0(
          "That share is at least the tail area of the interval (%s%%), so the replications ",
          "set aside could by themselves make up a whole tail. The range columns show how far ",
          "each limit could move had they produced values. This is a caution about ",
          "the interval, not a rule for deciding anything about the scale; frequent ",
          "failures are themselves evidence that the model is weakly determined in a ",
          "sample of this size."
        ),
        format(100 * result$caution_share, digits = 3)
      ))
    } else {
      paste(lead, sprintf(
        paste0(
          "The share is below the tail area of the interval (%s%%), the point at which a ",
          "caution is raised; the range columns show how far each limit could move."
        ),
        format(100 * result$caution_share, digits = 3)
      ))
    }
  }
  words <- c(
    failed = if (dmar) "refit raised an error" else "no fit returned", not_converged = "did not converge",
    not_identified = "not identified", inadmissible = "improper solution",
    no_indices = if (dmar) "indices not all proportions" else "indices not available"
  )
  set_aside <- table(factor(result$draws$outcome[!result$draws$used], levels = names(words)))
  by_outcome <- set_aside[names(words)]
  reasons <- if (not_used > 0L) {
    sprintf("%s: %d", words[by_outcome > 0L], as.integer(by_outcome[by_outcome > 0L]))
  } else {
    character(0)
  }
  list(
    method = method, counts = counts, rule = result$rule, precision = precision,
    stopped = stopped, caution = caution, reasons = reasons
  )
}

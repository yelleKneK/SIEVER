#' Assess a Measurement Model in Disjoint Calibration and Validation Samples
#'
#' Draws a split of the complete cases on active items, fits the declared
#' measurement model in each half, and compares fit and construct reliability.
#' This is internal assessment of the current data. Dividing a dataset does
#' not erase prior exposure to its validation observations.
#'
#' @param data Item-response data frame. Values must already reflect any
#'   intended reverse scoring. Only active item columns are analyzed.
#' @param meta Item ledger, such as the `meta` element of a saved project.
#'   See the example for the required columns.
#' @param holdout The proportion of the complete rows assigned to
#'   validation, one number strictly between zero and one. Any other value
#'   (30 meant as 30 percent, 1, `NA`, `"0.3"`, two values) is refused with
#'   the reason, and no split is drawn. Unused when `validation_rows` is
#'   given.
#' @param seed `NULL`, or one whole number from -2147483647 to 2147483647
#'   (1.9 is refused, not run as 1). A supplied seed leaves the caller's
#'   random number stream unchanged. With `NULL`, one seed is drawn from that
#'   stream and retained in the result; see Details.
#' @param estimator Estimator choice accepted by [sieve_cfa_spec()].
#' @param design Sampling design, with `unit = "independent"`. Clustered or
#'   repeated observations are refused because this procedure does not supply
#'   design-correct inference or a design-aware split.
#' @param denominator The Reliability tab's omega variant for continuous
#'   items: `"observed"` (robust omega, the default) or
#'   `"model_implied"`.
#' @param validation_rows `NULL` to draw the validation sample, or the row
#'   positions in `data` that form it: rows reserved before development
#'   (see [sieve_holdout_draw()]), given rather than drawn. The remaining
#'   complete rows form calibration; `holdout` and `seed` are then unused
#'   and `rng` is `NULL`. The positions are whole numbers from 1 to
#'   `nrow(data)`, without repeats, and are checked before any coercion:
#'   a logical vector (pass `which()` of it), a factor or text (an
#'   identifier is not a row number; pass `match(ids, id_column)`), and
#'   missing, fractional, out-of-range or repeated values are each refused
#'   with the values at fault named. Respondent identifiers passed as
#'   positions are refused when any exceeds `nrow(data)`; identifiers that
#'   happen to lie within it cannot be told from positions.
#' @param syntax `NULL` for the correlated-factors CFA the ledger implies,
#'   or the lavaan syntax of the measurement model to fit in both samples
#'   (the frozen version of a validation reservation passes the model the
#'   CFA page fits, which may be a higher-order or direct facet model).
#' @param std_lv `TRUE` fixes each latent variance to 1; `FALSE` fixes each
#'   factor's first loading to 1 (marker indicators), as [sieve_cfa_spec()].
#' @details
#' Rows incomplete on any active item are removed before sampling, including
#' when the selected continuous-item model otherwise uses full information
#' maximum likelihood. At least 20 complete rows are required. The validation
#' size is `round(n * holdout)`; the remaining rows form calibration. Each
#' half must hold at least p + 1 complete rows, one more than the p active
#' items: with fewer, the half's sample covariance matrix is singular, so
#' the model can be neither fit nor evaluated there (the covariance
#' discrepancy needs log |S| of the validation half). A split that would
#' leave a half smaller than that, an empty half included, is refused
#' before it is drawn, as are given rows that leave either half that small.
#' An MLR (or ML) model the machine or the smaller half cannot carry is
#' refused before the split is drawn, with the message of the model tabs
#' (`sieve_mlr_guard()`), and nothing is fit.
#'
#' A split drawn here after the full data were examined is internal
#' assessment: the validation rows were exposed when the structure and the
#' items were chosen. With `validation_rows` the evaluation can be more than
#' that, when the rows were reserved before development and the model
#' evaluated on them was frozen before the evaluation (Browne, 2000; Cudeck
#' & Browne, 1983); the Standards (AERA, APA, & NCME, 2014) ask that a
#' validity claim rest on evidence gathered for the interpretation proposed,
#' which a sample that shaped the item set cannot supply on its own.
#'
#' The covariance discrepancy evaluates the calibration model's implied
#' covariance against the validation sample covariance with the maximum
#' likelihood divisor n. Its comparison is twice the validation model's
#' minimized `fmin`, which uses the same divisor; their difference is
#' reported as shrinkage. The covariance discrepancy is unavailable for ordered items.
#' The two halves use the first fit-index family both report. Reliability
#' follows the same targets as the Reliability tab: Robust omega (observed
#' denominator) by default (for ordered items its denominator is built from
#' the unrestricted polychoric correlations), as point estimates for each
#' half, since their intervals are bootstraps that are not run for the
#' split. Beside robust omega, Omega (model-implied denominator) is given
#' in each half with its MLR (sandwich) SE interval, and the difference between the
#' halves with a delta method interval for independent samples; the
#' model-implied denominator assumes the one-factor model holds. Similar values in
#' the two halves are consistent with a reliability that does not depend on
#' which rows were used, not proof of it, and both halves come from the
#' same data.
#'
#' Sampling uses a local random number stream. With `seed = NULL`, the caller's
#' stream advances by only the draw of the recorded seed. `rng` records that
#' seed, whether it was supplied, and the three [RNGkind()] settings. To repeat
#' a run in another session, restore those settings and pass `rng$seed`.
#' Row indices refer to positions in `data`, before complete-case removal.
#' No coefficient or returned table is rounded for display.
#' @references
#' Browne, M. W. (2000). Cross-validation methods. *Journal of Mathematical
#' Psychology, 44*, 108--132.
#'
#' Cudeck, R., & Browne, M. W. (1983). Cross-validation of covariance
#' structures. *Multivariate Behavioral Research, 18*, 147--167.
#'
#' American Educational Research Association, American Psychological
#' Association, & National Council on Measurement in Education. (2014).
#' *Standards for educational and psychological testing*. AERA.
#' @return A list with `table` (fit indices and ECVI intervals), `xval`
#'   (covariance discrepancies), `rel` (construct reliability comparison;
#'   see Details),
#'   `n_cal`, `n_val`, `family`, `family_reason`, `reliability_targets`,
#'   `ordered`, model `status`, `ecvi_notes`, `rng`, `holdout` (the
#'   proportion the split was drawn with; `NA` when `validation_rows` was
#'   given), `n_reserved_incomplete` (the number of given validation rows
#'   left out because they are incomplete on the active items; `NA` when the
#'   split was drawn), `eligible_rows`, `calibration_rows`, and
#'   `validation_rows`. A refused request instead has `error` explaining why
#'   no split was fit; when available, its eligible rows and RNG record are
#'   retained.
#' @author Ken Kelley
#' @examples
#' data <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
#' meta <- data.frame(item = names(data), label = names(data),
#'   construct = rep(c("Visual", "Textual"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' # Rows reserved before development are given, not drawn here, so the
#' # result records no random number state.
#' reserved <- sieve_holdout_draw(seq_len(nrow(data)), proportion = .4, seed = 113)$rows
#' split <- sieve_cross_validate(data, meta, validation_rows = reserved)
#' split$table
#' split$xval
#' split$n_val
#' is.null(split$rng)
#' @export
sieve_cross_validate <- function(data, meta, holdout = .5, seed = NULL,
                                 estimator = "auto", design = list(unit = "independent"),
                                 denominator = "observed", validation_rows = NULL,
                                 syntax = NULL, std_lv = TRUE) {
  problem <- sieve_sampling_problem(list(sampling_design = design))
  if (!is.null(problem)) return(list(error = problem))
  # The arguments are checked as given, before any coercion, and a refusal
  # is returned as the function's other refusals are.
  given <- !is.null(validation_rows)
  if (given) {
    checked <- tryCatch(
      sieve_row_positions(validation_rows, NROW(data), arg = "validation_rows", of = "`data`"),
      sieve_invalid_rows = function(e) e
    )
    if (inherits(checked, "condition")) return(list(error = conditionMessage(checked)))
    validation_rows <- checked
    holdout <- NA_real_
  } else {
    p <- tryCatch(sieve_holdout_proportion(holdout, arg = "holdout"), error = function(e) NULL)
    if (is.null(p)) {
      return(list(error = sprintf(
        paste(
          "`holdout` must be one number strictly between 0 and 1, the proportion of the complete rows",
          "assigned to validation (0.5 by default); %s was given, so no split was drawn."
        ),
        sieve_given_text(holdout)
      )))
    }
    holdout <- p
    # Read here, so an invalid seed (from the argument or from the page's
    # seed box, sieve_seed_input()) is this function's refusal.
    seed <- tryCatch(sieve_seed_value(seed), sieve_seed_invalid = function(e) e)
    if (inherits(seed, "condition")) {
      return(list(error = paste(conditionMessage(seed), "No split was drawn.")))
    }
  }
  active_items <- meta$item[meta$status == "active"]
  read <- sieve_numeric_columns(data, intersect(active_items, names(data)))
  if (nrow(read$problems)) {
    return(list(error = sprintf("The split cannot be fit. %s", paste(read$text, collapse = " "))))
  }
  eligible_rows <- which(stats::complete.cases(read$data))
  d <- read$data[eligible_rows, , drop = FALSE]
  n <- nrow(d)
  if (n < 20L) {
    return(list(
      error = sprintf("Only %d complete cases on the active items; too few to split reliably.", n),
      eligible_rows = eligible_rows
    ))
  }

  # Each half needs p + 1 complete rows (p active items), below which its
  # sample covariance matrix is singular and the model can be neither fit
  # nor evaluated there.
  n_items <- ncol(d)
  minimum <- n_items + 1L
  why_minimum <- sprintf(
    paste(
      "each sample needs at least %d complete rows, one more than the %d active items, because with",
      "fewer its sample covariance matrix is singular and the model can be neither fit nor evaluated there"
    ),
    minimum, n_items
  )
  if (given) {
    # The reserved rows, given rather than drawn: those complete on the
    # active items form validation, the other complete rows calibration.
    val_idx <- which(eligible_rows %in% validation_rows)
    n_val <- length(val_idx)
    if (n_val < minimum || n - n_val < minimum) {
      return(list(
        error = sprintf(
          "Of the %d given validation rows, %d are complete on the active items and %d other complete rows remain for calibration, so no split was fit: %s.",
          length(validation_rows), n_val, n - n_val, why_minimum
        ),
        eligible_rows = eligible_rows
      ))
    }
  } else {
    n_val <- round(n * holdout)
    if (n_val < minimum || n - n_val < minimum) {
      return(list(
        error = sprintf(
          "A validation proportion of %s assigns %d of the %d complete rows to validation and %d to calibration, so no split was drawn: %s.",
          format(holdout, digits = 15L), as.integer(n_val), n, as.integer(n - n_val), why_minimum
        ),
        eligible_rows = eligible_rows
      ))
    }
  }
  # The MLR refusal of the model tabs, before the split is drawn or either
  # half is fit: each half is fit on its own, so the rank check reads the
  # smaller half.
  refusal <- sieve_spec_mlr_guard(
    sieve_cfa_spec(meta, syntax = syntax, std_lv = std_lv, reserved = names(d), estimator = estimator),
    n = min(n_val, n - n_val)
  )
  if (!is.null(refusal)) {
    return(list(error = refusal, eligible_rows = eligible_rows))
  }
  drawn <- if (given) list(value = val_idx, rng = NULL) else sieve_with_seed(seed, sort(sample.int(n, n_val)))
  val_idx <- drawn$value
  rows <- list(
    eligible_rows = eligible_rows,
    calibration_rows = eligible_rows[-val_idx], validation_rows = eligible_rows[val_idx]
  )
  d_val <- d[val_idx, , drop = FALSE]
  d_cal <- d[-val_idx, , drop = FALSE]
  f_cal <- sieve_fit_cfa(d_cal, meta, syntax = syntax, std_lv = std_lv, estimator = estimator)
  f_val <- sieve_fit_cfa(d_val, meta, syntax = syntax, std_lv = std_lv, estimator = estimator)
  if (is.null(f_cal) && is.null(f_val)) {
    return(c(list(error = "Neither split could be fit. Check the active model.", rng = drawn$rng), rows))
  }

  cv_family <- sieve_common_family(list(
    calibration = sieve_fit_families(f_cal), validation = sieve_fit_families(f_val)
  ))
  shared <- if (is.na(cv_family$family)) NULL else cv_family$family
  # The ECVI intervals at DMAR's default level, the level of SIEVER's other
  # intervals; the RMSEA intervals at the RMSEA level (90% by default).
  level <- sieve_default_conf_level()
  rmsea_level <- sieve_default_rmsea_conf_level()
  row_cal <- sieve_cv_fit_row("Calibration (training)", f_cal, family = shared, conf_level = level,
    rmsea_conf_level = rmsea_level)
  row_val <- sieve_cv_fit_row("Validation (own fit)", f_val, family = shared, conf_level = level,
    rmsea_conf_level = rmsea_level)
  ecvi_notes <- unique(c(attr(row_cal, "warnings"), attr(row_val, "warnings")))
  tbl <- rbind(row_cal, row_val)
  attr(tbl, "warnings") <- NULL
  if (is.na(cv_family$family) && all(cv_family$solved)) {
    tbl[, c("CFI", "RMSEA", "RMSEA.lo", "RMSEA.hi")] <- NA_real_
  }

  ordered <- sieve_lavaan_spec(meta, estimator = estimator)$ordered
  tight <- sieve_cross_validation_discrepancy(
    if (sieve_has_solution(f_cal)) f_cal else NULL, d_val, ordered
  )
  loose <- if (!sieve_has_solution(f_val)) NA_real_ else {
    tryCatch(2 * unname(lavaan::fitMeasures(f_val, "fmin")), error = function(e) NA_real_)
  }
  xval_tbl <- data.frame(
    Quantity = c(
      "Tight: training parameters on validation", "Loose: validation re-estimated",
      "Shrinkage (tight - loose)"
    ),
    Discrepancy = c(tight, loose, if (is.finite(tight) && is.finite(loose)) tight - loose else NA_real_),
    stringsAsFactors = FALSE
  )
  om_cal <- sieve_split_omega(d_cal, meta, estimator = estimator, denominator = denominator)
  om_val <- sieve_split_omega(d_val, meta, estimator = estimator, denominator = denominator)
  rel_tbl <- sieve_split_reliability_table(om_cal, om_val)
  c(list(
    table = tbl, n_cal = nrow(d_cal), n_val = nrow(d_val),
    family = cv_family$family, family_reason = cv_family$reason, conf_level = level,
    rmsea_conf_level = rmsea_level,
    xval = xval_tbl, rel = rel_tbl, reliability_targets = attr(om_cal, "targets"),
    ordered = !is.null(ordered),
    status = list(calibration = sieve_status_of(f_cal), validation = sieve_status_of(f_val)),
    ecvi_notes = ecvi_notes, rng = drawn$rng, holdout = holdout,
    n_reserved_incomplete = if (given) sum(!validation_rows %in% eligible_rows) else NA_integer_
  ), rows)
}

#' Evaluate calibration covariance against validation responses
#' @param cal_fit Calibration model, or NULL without an admissible solution.
#' @param d_val Complete validation item responses.
#' @param ordered Ordered item names, or NULL for a continuous model.
#' @return The covariance ML discrepancy, or NA when unavailable.
#' @author Ken Kelley
#' @noRd
sieve_cross_validation_discrepancy <- function(cal_fit, d_val, ordered) {
  if (is.null(cal_fit) || !is.null(ordered)) return(NA_real_)
  Sig <- tryCatch(lavaan::lavInspect(cal_fit, "implied")$cov, error = function(e) NULL)
  if (is.null(Sig)) return(NA_real_)
  cols <- rownames(Sig)
  if (!all(cols %in% names(d_val))) return(NA_real_)
  Sval <- tryCatch(stats::cov(d_val[, cols, drop = FALSE], use = "complete.obs"),
    error = function(e) NULL
  )
  if (is.null(Sval)) return(NA_real_)
  # The maximum likelihood covariance (divisor n, not n - 1), the matrix the
  # loose discrepancy (twice the validation fit's fmin) is computed from, so
  # the two discrepancies and their difference are on one scale.
  n_val <- sum(stats::complete.cases(d_val[, cols, drop = FALSE]))
  Sval <- Sval * (n_val - 1) / n_val
  p <- nrow(Sig)
  val <- tryCatch(
    as.numeric(log(det(Sig)) + sum(diag(Sval %*% solve(Sig))) - log(det(Sval)) - p),
    error = function(e) NA_real_
  )
  if (is.finite(val)) val else NA_real_
}

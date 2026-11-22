# Explicit longitudinal item identity, with the invariance syntax written directly.

#' Resolve a Longitudinal Measurement Model From an Explicit Repeated-Item Map
#'
#' @description
#' Experimental: the arguments and the form of the result may change in
#' a later release of SIEVER without a period of deprecation.
#' Builds the lavaan syntax and the specification of a longitudinal
#' measurement model from a map that says which variable is which item at
#' which wave: one factor per construct and wave, the item parameters held
#' equal across waves up to the rung asked for, and residual autocovariances
#' between an item's repetitions as chosen and justified. The first wave's
#' factor mean is fixed at 0 and its variance at 1, which sets the metric of
#' every later wave.
#'
#' @param data Wide data, one independent participant per row.
#' @param mapping Data frame: construct, wave, item (stable identity), variable.
#' @param level Invariance rung.
#' @param wave_order Character vector declaring waves from reference to last.
#'   Alternatively supply a numeric wave_order column in mapping, or an
#'   ordered-factor wave column with the declared order in its levels.
#' @param residuals Residual autocovariances: none, adjacent, or all.
#' @param justification Researcher justification for the autocovariance choice.
#' @param ordered Declared categorical variable names.
#' @param estimator One of `"auto"`, which follows the item types (ML
#'   with FIML when every item is continuous, WLSMV when any is ordered or
#'   binary), `"mlr"` (MLR with FIML), `"ml_fiml"` (ML with FIML), `"ml"`
#'   (ML on the complete cases) or `"wlsmv"` (WLSMV, every item treated as
#'   ordered). With `ordered` items, a choice that fits them as continuous
#'   (`"mlr"`, `"ml_fiml"` or `"ml"`) is refused.
#' @return Exact syntax/specification, mapping, and identification choices.
#' @author Ken Kelley
#' @examples
#' # The same four indicators of democracy, rated in 1960 (y1 to y4) and in 1965
#' # (y5 to y8).
#' map <- data.frame(construct = "Democracy", wave = rep(c("1960", "1965"), each = 4L),
#'   item = rep(c("press", "opposition", "elections", "legislature"), 2L),
#'   variable = paste0("y", 1:8), wave_order = rep(1:2, each = 4L))
#' spec <- sieve_longitudinal_spec(lavaan::PoliticalDemocracy, map, level = "metric",
#'   justification = "The same four indicators were rated at both waves")
#' spec$estimator
#' spec$latent
#' @export
sieve_longitudinal_spec <- function(
  data, mapping, level = c("configural", "thresholds", "metric", "scalar", "strict"),
  residuals = c("adjacent", "none", "all"), justification = "", ordered = character(0), estimator = "auto",
  wave_order = NULL
) {
  level <- match.arg(level)
  residuals <- match.arg(residuals)
  required <- c("construct", "wave", "item", "variable")
  if (!is.data.frame(mapping) || !all(required %in% names(mapping)) || !nrow(mapping)) stop("Supply a mapping table with construct, wave, item, and variable columns.",
    call. = FALSE
  )
  declared <- wave_order
  if (is.null(declared) && is.ordered(mapping$wave)) declared <- levels(mapping$wave)
  if (is.null(declared) && "wave_order" %in% names(mapping)) {
    order_value <- suppressWarnings(as.numeric(mapping$wave_order))
    pairs <- unique(data.frame(wave = as.character(mapping$wave), order = order_value))
    if (any(!is.finite(pairs$order)) || anyDuplicated(pairs$wave) || anyDuplicated(pairs$order)) {
      stop("Each wave must have one distinct finite wave_order value.", call. = FALSE)
    }
    declared <- pairs$wave[order(pairs$order)]
  }
  mapping <- mapping[, required, drop = FALSE]
  mapping[] <- lapply(mapping, function(x) trimws(as.character(x)))
  if (anyNA(mapping) || any(!nzchar(as.matrix(mapping))) || anyDuplicated(mapping$variable) || !all(mapping$variable %in% names(data))) stop("Each mapped variable must exist, occur once, and have nonempty construct/wave/item identities.",
    call. = FALSE
  )
  if (any(make.names(mapping$variable) != mapping$variable)) stop("Mapped data columns must have valid R/lavaan names; rename them before declaring the map.",
    call. = FALSE
  )
  waves <- trimws(as.character(declared))
  if (length(waves) < 2L || anyNA(waves) || anyDuplicated(waves) ||
    !setequal(waves, mapping$wave)) {
    stop("Declare every wave once, in order, using wave_order, a mapping wave_order column, or an ordered wave factor. CSV row order is not a wave declaration.",
      call. = FALSE
    )
  }
  mapping <- mapping[order(match(mapping$wave, waves), seq_len(nrow(mapping))), , drop = FALSE]
  rownames(mapping) <- NULL
  mapping$wave_order <- match(mapping$wave, waves)
  canonical <- unique(mapping[c("construct", "item")])
  for (cn in unique(mapping$construct)) {
    identities <- canonical$item[canonical$construct == cn]
    if (length(identities) < 3L) stop("Each construct needs at least three repeated item identities at every wave.",
      call. = FALSE
    )
    for (wave in waves) {
      actual <- mapping$item[mapping$construct == cn & mapping$wave == wave]
      if (anyDuplicated(actual) || !setequal(
        actual,
        identities
      )) stop("The wave/item map is incomplete or scrambled: every repeated identity must occur once per construct at every wave.",
        call. = FALSE
      )
    }
  }
  if (!is.character(justification) || length(justification) != 1L || !sieve_has_content(justification)) stop("Record why this residual autocovariance structure is justified.",
    call. = FALSE
  )
  if (!all(ordered %in% mapping$variable)) stop("`ordered` must name only mapped variables.", call. = FALSE)
  cells <- unique(mapping[c("construct", "wave")])
  latent <- unname(sieve_allocate_names(
    paste(cells$construct,
      cells$wave,
      sep = "_"
    ),
    taken = names(data),
    prefix = "Long"
  ))
  long_factors <- lapply(
    unique(mapping$construct),
    function(cn) latent[match(
      paste(
        cn,
        waves
      ),
      paste(
        cells$construct,
        cells$wave
      )
    )]
  )
  names(long_factors) <- unique(mapping$construct)
  long_items <- lapply(seq_len(nrow(canonical)), function(i) {
    m <- mapping[mapping$construct == canonical$construct[i] & mapping$item == canonical$item[i], , drop = FALSE]
    m$variable[match(waves, m$wave)]
  })
  names(long_items) <- paste0("RepeatedItem", seq_len(nrow(canonical)))
  model <- paste(vapply(seq_len(nrow(cells)), function(i) {
    m <- mapping[mapping$construct == cells$construct[i] & mapping$wave == cells$wave[i], , drop = FALSE]
    ids <- canonical$item[canonical$construct == cells$construct[i]]
    paste(latent[i], "=~", paste(m$variable[match(ids, m$item)], collapse = " + "))
  }, character(1)), collapse = "\n")
  auto <- switch(residuals,
    none = 0L,
    adjacent = 1L,
    all = "all"
  )
  types <- ifelse(mapping$variable %in% ordered, "ordered", "continuous")
  resolve <- sieve_estimator_resolve(types, choice = estimator)
  if (length(ordered) && resolve$estimator %in% c(
    "ML",
    "MLR"
  )) stop("Declared ordered longitudinal outcomes require an ordered-indicator estimator.",
    call. = FALSE
  )
  ordered <- mapping$variable[resolve$categorical]
  if (!length(ordered) && identical(
    level,
    "thresholds"
  )) stop("A thresholds rung requires categorical indicators.",
    call. = FALSE
  )
  equal <- switch(level,
    configural = character(0),
    thresholds = "thresholds",
    metric = "loadings",
    scalar = c(
      "loadings",
      "intercepts"
    ),
    strict = c(
      "loadings",
      "intercepts",
      "residuals"
    )
  )
  if (length(ordered) && !identical(level, "configural")) equal <- unique(c("thresholds", equal))
  # The configural structure (thresholds counted from the data) is read
  # from an unfitted template, and the rung's syntax is written from it:
  # fixed-factor identification at the first occasion, Wu and Estabrook
  # (2016) constraints for ordered items under the theta parameterization.
  template <- lavaan::cfa(model,
    data = data, ordered = if (length(ordered)) ordered else NULL,
    parameterization = if (length(ordered)) "theta" else "delta",
    std.lv = TRUE, meanstructure = TRUE, do.fit = FALSE
  )
  syntax <- sieve_measurement_syntax(template,
    factors_over_time = long_factors, items_over_time = long_items,
    time_equal = equal, autocov = auto
  )
  ledger <- data.frame(item = mapping$variable, type = types, status = "active")
  plan <- list(
    model = syntax, observed = mapping$variable, latent = latent, structure = "longitudinal",
    name_map = stats::setNames(paste0(cells$construct, " (", cells$wave, ")"), latent), problems = character(0)
  )
  spec <- sieve_model_spec(plan, ledger, kind = "longitudinal", estimator = estimator)
  spec$longitudinal <- list(
    mapping = mapping, waves = waves, wave_order = waves,
    adjacent_pairs = if (length(waves) > 1L) paste(head(waves, -1L), tail(waves, -1L), sep = " - ") else character(0),
    reference_wave = waves[1L], level = level, residuals = residuals,
    justification = trimws(justification), long_factors = long_factors, long_items = long_items, equal = equal
  )
  spec$parameterization <- if (length(ordered)) "theta" else "delta"
  # Parameterization affects categorical residual invariance and must reach
  # the same resolved engine call and replay as the generated syntax.
  spec <- sieve_spec_engine_args(spec, list(parameterization = spec$parameterization))
  spec$key <- NULL
  spec$key <- sieve_hash(spec)
  spec
}

#' Fit One Explicitly Chosen Longitudinal Invariance Rung
#'
#' @description
#' Experimental: the arguments and the form of the result may change in
#' a later release of SIEVER without a period of deprecation.
#' Fits the one rung of a longitudinal invariance ladder named by `level`
#' (passed through `...`), with the specification
#' [sieve_longitudinal_spec()] builds, and returns the fitted model with its
#' status, the item map and the row flow. A maximum likelihood rung that the
#' machine's memory or the sample cannot carry is refused before it is fit,
#' with the reason.
#'
#' @param data Wide item data.
#' @param mapping Repeated item identity map.
#' @param ... Arguments to sieve_longitudinal_spec.
#' @return A bound-ready model result, exact mapping, and row flow. An MLR
#'   (or ML) rung the machine or the sample cannot carry is refused before
#'   it is fit, with the message of the model tabs (`sieve_mlr_guard()`):
#'   the result then has that `error` and no `fit`.
#' @author Ken Kelley
#' @examples
#' map <- data.frame(construct = "Democracy", wave = rep(c("1960", "1965"), each = 4L),
#'   item = rep(c("press", "opposition", "elections", "legislature"), 2L),
#'   variable = paste0("y", 1:8), wave_order = rep(1:2, each = 4L))
#' metric <- sieve_run_longitudinal(lavaan::PoliticalDemocracy, map, level = "metric",
#'   justification = "The same four indicators were rated at both waves")
#' metric$status$code
#' lavaan::fitMeasures(metric$fit, c("cfi.robust", "rmsea.robust"))
#' @export
sieve_run_longitudinal <- function(data, mapping, ...) {
  spec <- sieve_longitudinal_spec(data, mapping, ...)
  # The MLR refusal of the model tabs (sieve_mlr_guard()), before the rung
  # is fit: every wave's indicators in one model, with its mean structure.
  refusal <- sieve_spec_mlr_guard(spec, n = nrow(data), means = TRUE)
  result <- if (is.null(refusal)) {
    sieve_run_model(spec, data)
  } else {
    structure(list(
      spec = spec, fit = NULL,
      status = sieve_fit_status(NULL, error = refusal, code = "failed"),
      warnings = character(0), messages = character(0), error = refusal,
      console = character(0)
    ), class = "sieve_result")
  }
  result$row_flow <- data.frame(
    input = nrow(data),
    used = if (is.null(result$fit)) NA_integer_ else sum(lavaan::lavInspect(
      result$fit,
      "nobs"
    )),
    missing = spec$missing, unit = "One independent participant per wide row; repeated waves are variables"
  )
  result
}

#' Compare Two Deliberately Fitted Longitudinal Rungs
#'
#' @description
#' Experimental: the arguments and the form of the result may change in
#' a later release of SIEVER without a period of deprecation.
#' Compares two rungs of a longitudinal invariance ladder fit to the same
#' data with the same item map, estimator, residual choice and indicator
#' treatment: the difference test from the less to the more constrained
#' rung, in the family the estimator calls for, and the change in each fit
#' index. Rungs that differ in anything but their constraints, or that have
#' no admissible solution, are refused.
#'
#' @param reference,current Longitudinal results, least then more constrained.
#' @return Same-family fit comparison and exact rung identifiers.
#' @author Ken Kelley
#' @examples
#' map <- data.frame(construct = "Democracy", wave = rep(c("1960", "1965"), each = 4L),
#'   item = rep(c("press", "opposition", "elections", "legislature"), 2L),
#'   variable = paste0("y", 1:8), wave_order = rep(1:2, each = 4L))
#' why <- "The same four indicators were rated at both waves"
#' metric <- sieve_run_longitudinal(lavaan::PoliticalDemocracy, map,
#'   level = "metric", justification = why)
#' scalar <- sieve_run_longitudinal(lavaan::PoliticalDemocracy, map,
#'   level = "scalar", justification = why)
#' compared <- sieve_longitudinal_compare(metric, scalar)
#' compared$table[, c("model", "status", "cfi", "rmsea", "delta_cfi", "delta_rmsea", "delta_p")]
#' @export
sieve_longitudinal_compare <- function(reference, current) {
  a <- reference$spec$longitudinal
  b <- current$spec$longitudinal
  if (is.null(a) || is.null(b) || !identical(a$mapping, b$mapping) ||
    !identical(a$wave_order %||% a$waves, b$wave_order %||% b$waves) ||
    !identical(a$residuals, b$residuals) || !identical(a$justification, b$justification) ||
    !identical(reference$spec$ordered, current$spec$ordered) ||
    !identical(
      reference$spec$estimator,
      current$spec$estimator
    )) stop("Compare rungs with exactly the same map, residual decision, indicator treatment, and estimator.",
    call. = FALSE
  )
  levels <- c("configural", if (length(current$spec$ordered)) "thresholds", "metric", "scalar", "strict")
  if (match(a$level, levels) >= match(b$level, levels)) stop("Choose a less constrained reference rung.", call. = FALSE)
  if (!identical(
    sieve_fit_status(reference$fit)$code,
    "ok"
  ) || !identical(
    sieve_fit_status(current$fit)$code,
    "ok"
  )) stop("Both rungs need admissible solutions.",
    call. = FALSE
  )
  if (!identical(
    lavaan::lavInspect(
      reference$fit,
      "data"
    ),
    lavaan::lavInspect(
      current$fit,
      "data"
    )
  )) stop("The fitted longitudinal rungs used different observed data; a nested comparison is unavailable.",
    call. = FALSE
  )
  cmp <- sieve_fit_compare(stats::setNames(list(reference$fit, current$fit), c(a$level, b$level)))
  list(
    table = cmp$table, family = cmp$family, test_family = cmp$test_family,
    test_label = cmp$test_label, warnings = cmp$warnings,
    reference_key = reference$spec$key, current_key = current$spec$key,
    note = "These changes describe the stated longitudinal constraints. Practical fit-change cutoffs are guidance, not evidence of exact invariance or substantive anchor validity."
  )
}

#' Latent Change From a Generated Longitudinal Scalar or Strict Model
#'
#' @description
#' Experimental: the arguments and the form of the result may change in
#' a later release of SIEVER without a period of deprecation.
#' Estimates how far each construct's latent mean has moved from the first
#' wave to each later wave, from a longitudinal invariance model that
#' [sieve_run_longitudinal()] fit at the scalar or strict rung, with a Wald
#' interval for each change. Earlier rungs do not identify a change in
#' latent means, and for them the function says so instead of estimating
#' one.
#'
#' @details
#' **Estimand.** For wave `j`, the latent mean at wave `j` minus the latent
#' mean at the first wave (the first in `wave_order`). The generated model
#' fixes the first wave's latent mean at 0 and its variance at 1, so each
#' change is in units of the first wave's latent standard deviation; it is
#' not a change standardized by a pooled or a later wave's standard
#' deviation. Every contrast is against the first wave; a change between
#' two later waves is the difference of their two contrasts, and its
#' standard error needs their covariance, which this table does not give.
#'
#' **Interval.** Wald: `estimate +/- z * se`, `z = qnorm((1 + conf_level) / 2)`,
#' with `se` the square root of the later wave's mean's variance in
#' lavaan's covariance matrix of the free parameters (the first wave's mean
#' is fixed). It holds under the model and the estimator's standard errors
#' (robust ones under MLR, for example).
#'
#' **What the model must be.** The scalar or strict rung, with an
#' admissible solution: loadings and item locations (intercepts, or
#' thresholds for ordered items) equal across waves, so that the waves
#' share a latent metric (Meredith, 1993). For the configural or metric
#' rung, or an inadmissible solution, the function returns a one-column
#' data frame whose `reason` says why no change is estimated, rather than an
#' error: in those models the fixed means are identification constraints.
#'
#' **The `reason` column.** On a row with an estimate it holds the caveat
#' that the change is conditional on the generated invariance restrictions
#' and the item identity map; on a row without one it says what could not
#' be verified.
#'
#' A `conf_level` outside (0, 1) is refused with an error, before the
#' fit is read.
#'
#' @param result A result generated by [sieve_run_longitudinal()].
#' @param conf_level Confidence level of the Wald intervals: one number
#'   strictly between 0 and 1 (0.95 for a 95 percent interval). It is named
#'   `conf_level` because `level` names the invariance rung in
#'   [sieve_run_longitudinal()].
#' @return A data frame with one row per construct and later wave:
#'   \describe{
#'     \item{`construct`}{The construct.}
#'     \item{`reference`, `wave`}{The first wave and the later wave.}
#'     \item{`estimate`}{The later wave's latent mean minus the first
#'       wave's, in first-wave latent SD units.}
#'     \item{`se`, `lower`, `upper`, `conf_level`}{The standard error and
#'       the Wald limits at `conf_level`.}
#'     \item{`metric`}{The units, in words.}
#'     \item{`reason`}{The caveat, or why no estimate is given.}
#'   }
#'   For a configural or metric rung, or an inadmissible solution, a data
#'   frame with the one column `reason`.
#' @references
#' Meredith, W. (1993). Measurement invariance, factor analysis and
#' factorial invariance. *Psychometrika, 58*(4), 525--543.
#' \doi{10.1007/BF02294825}
#' @seealso [sieve_run_longitudinal()], [sieve_longitudinal_compare()]
#' @author Ken Kelley
#' @examples
#' map <- data.frame(construct = "Democracy", wave = rep(c("1960", "1965"), each = 4L),
#'   item = rep(c("press", "opposition", "elections", "legislature"), 2L),
#'   variable = paste0("y", 1:8), wave_order = rep(1:2, each = 4L))
#' scalar <- sieve_run_longitudinal(lavaan::PoliticalDemocracy, map, level = "scalar",
#'   justification = "The same four indicators were rated at both waves")
#' sieve_longitudinal_contrasts(scalar)[, c(
#'   "construct", "reference", "wave", "estimate", "lower", "upper"
#' )]
#' @export
sieve_longitudinal_contrasts <- function(result, conf_level = .95) {
  conf_level <- sieve_check_conf_level(conf_level)
  d <- result$spec$longitudinal
  if (is.null(d) || !d$level %in% c(
    "scalar",
    "strict"
  )) return(data.frame(reason = "Loading and location invariance must be fitted before latent change is interpreted. Configural/metric fixed means are identification constraints."))
  if (!identical(
    sieve_fit_status(result$fit)$code,
    "ok"
  )) return(data.frame(reason = "An admissible longitudinal model is required."))
  pt <- lavaan::parTable(result$fit)
  V <- lavaan::lavInspect(result$fit, "vcov")
  z <- stats::qnorm((1 + conf_level) / 2)
  rows <- lapply(names(d$long_factors), function(cn) {
    factors <- d$long_factors[[cn]]
    means <- pt[pt$op == "~1" & pt$lhs %in% factors, , drop = FALSE]
    ref <- means[means$lhs == factors[1L], , drop = FALSE]
    do.call(rbind, lapply(seq.int(2L, length(factors)), function(j) {
      focal <- means[means$lhs == factors[j], , drop = FALSE]
      ok <- nrow(ref) == 1L && nrow(focal) == 1L && ref$free == 0L && abs(ref$est) < 1e-10 && focal$free > 0L
      estimate <- se <- lower <- upper <- NA_real_
      if (ok) {
        contrast <- numeric(nrow(V))
        contrast[focal$free] <- 1
        variance <- as.numeric(crossprod(contrast, V %*% contrast))
        ok <- is.finite(variance) && variance >= 0
        if (ok) {
          estimate <- focal$est - ref$est
          se <- sqrt(variance)
          lower <- estimate - z * se
          upper <- estimate + z * se
        }
      }
      data.frame(
        construct = cn, reference = d$waves[1L], wave = d$waves[j], estimate = estimate,
        se = se, lower = lower, upper = upper, conf_level = conf_level,
        metric = "latent mean difference in reference-wave factor SD units (first-wave variance fixed at 1)",
        reason = if (ok) "Conditional on the generated loading/location invariance restrictions and checked item identities; substantive fit and anchor validity still require review." else "The fitted mean origin/free mean or covariance was not verified.",
        stringsAsFactors = FALSE
      )
    }))
  })
  do.call(rbind, rows)
}

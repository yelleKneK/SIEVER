# fct_fit.R
# Fit indices by family, and comparisons of models within one family.
#
# lavaan reports the model test and the indices built on it (CFI, TLI,
# RMSEA and its confidence interval) in up to three versions, which are
# different statistics and not three roundings of one:
#
# * standard: computed from the uncorrected test statistic;
# * scaled (the .scaled columns): the scaled test statistic put into the
#   usual formulas, which is what many programs print;
# * robust (the .robust columns): corrected so that the index estimates the
#   population value its normal theory counterpart estimates (Brosseau-Liard,
#   Savalei & Li, 2012; Brosseau-Liard & Savalei, 2014; Savalei, 2018, 2021;
#   Zhang & Savalei, 2023).
#
# The three are stored by name, each with whether it is available and, when
# it is not, why. One family is chosen for display by a stated rule and is
# always named where a value is shown. A change in CFI or RMSEA between two
# models is a difference of values from one family that every model in the
# comparison reports: a robust value is never subtracted from a scaled one.

#' The lavaan fit measure names of each family
#'
#' The robust family has no test statistic of its own: its indices are
#' derived from the scaled statistic (or, under full information maximum
#' likelihood, from the standard one) and correction factors.
#'
#' @return A named list (`standard`, `scaled`, `robust`) of named character
#'   vectors mapping `chisq`, `df`, `pvalue`, `cfi`, `tli`, `rmsea`,
#'   `rmsea_lo`, and `rmsea_hi` to lavaan's names (`NA` where the family has
#'   none).
#' @author Ken Kelley
#' @noRd
sieve_fit_columns <- function() {
  list(
    standard = c(
      chisq = "chisq", df = "df", pvalue = "pvalue", cfi = "cfi", tli = "tli",
      rmsea = "rmsea", rmsea_lo = "rmsea.ci.lower", rmsea_hi = "rmsea.ci.upper"
    ),
    scaled = c(
      chisq = "chisq.scaled", df = "df.scaled", pvalue = "pvalue.scaled",
      cfi = "cfi.scaled", tli = "tli.scaled", rmsea = "rmsea.scaled",
      rmsea_lo = "rmsea.ci.lower.scaled", rmsea_hi = "rmsea.ci.upper.scaled"
    ),
    robust = c(
      chisq = NA_character_, df = NA_character_, pvalue = NA_character_,
      cfi = "cfi.robust", tli = "tli.robust", rmsea = "rmsea.robust",
      rmsea_lo = "rmsea.ci.lower.robust", rmsea_hi = "rmsea.ci.upper.robust"
    )
  )
}

#' The Default Confidence Levels of SIEVER's Intervals
#'
#' `sieve_default_conf_level()` is the confidence level of every interval
#' that a page or function forms without a level of its own: 0.95, DMAR's
#' `conf_level` default. `sieve_default_rmsea_conf_level()` is the level of
#' every RMSEA interval SIEVER shows, records or reports: 0.90 unless
#' `options(sieve.rmsea_conf_level = )` sets another level.
#'
#' @details
#' # Intervals Other Than the RMSEA Interval
#'
#' An interval formed where no confidence level is set is at 0.95, DMAR's
#' `conf_level` default, which SIEVER leaves in force. A page with a
#' confidence level input of its
#' own (Reliability, Validity, Bifactor, Content, IRT, the norms, and the
#' missing data and multiple imputation panels among them) forms its
#' intervals at the level of that input, 0.95 until it is changed there, and
#' a function with a `conf_level` argument at the level of that argument.
#' Every interval is labeled with its level.
#'
#' # The Level of the RMSEA Interval
#'
#' Every RMSEA interval is at 0.90 by default: that of the
#' correlated-factors CFA, of the SEM, of the exploratory solutions by
#' number of factors, of each construct's one-factor model on the
#' Reliability page, of
#' the comparisons of models and of structures, of the cross-validation
#' table, of the Network page and of the methods draft
#' ([sieve_methods_draft()]). 0.90 is DMAR's default for the RMSEA interval,
#' and lavaan's. A 100(1 - 2 alpha) percent interval decides
#' the one-sided tests of close fit and of not close fit at level alpha
#' (MacCallum, Browne & Sugawara, 1996, p. 136), so the limits of the 90
#' percent interval are those tests at .05, and its lower limit is 0 exactly
#' when the test of exact fit does not reject at .05. The RMSEA level is
#' separate from every other level: a page's confidence level input, the
#' Reliability tab's included, does not reach the RMSEA interval, as DMAR's
#' `conf_level` does not reach its `rmsea_conf_level`.
#'
#' The interval of the unbiased SRMR (Maydeu-Olivares, 2017), shown beside
#' the SRMR on the CFA and SEM pages and in the methods draft, is at the
#' RMSEA level too, as `DMAR::sem_residual_fit()`'s `conf_level` defaults
#' to 0.90: the 90 percent interval of the unbiased SRMR pairs with a test
#' of close fit at .05 in the same way (Maydeu-Olivares, Shi & Rosseel,
#' 2018), so the fit measures follow one interval policy.
#'
#' # Setting the RMSEA Level
#'
#' `options(sieve.rmsea_conf_level = 0.95)`, set before [run_app()] starts
#' or before a SIEVER function is called, puts every RMSEA interval at 0.95
#' for the rest of the R session. The value must be one number strictly
#' between 0 and 1 (0.95 for a 95 percent interval). A value that is set but
#' is not such a number, for example `95`, `1.5`, `NA` or `"ninety"`, is not
#' used: the RMSEA intervals stay at 0.90, and the first time that value is
#' read in the session `sieve_default_rmsea_conf_level()` warns, naming the
#' value and the rule (a warning of class `sieve_rmsea_level_warning`), so a
#' mistyped level is never dropped without a word. Every label, recorded
#' script and saved record states the RMSEA level in force when it was made,
#' and the recorded R passes that level to `lavaan::fitMeasures()`
#' (`fm.args = list(rmsea.ci.level = )`) and to DMAR (`rmsea_conf_level`).
#'
#' @references MacCallum, R. C., Browne, M. W., & Sugawara, H. M. (1996).
#'   Power analysis and determination of sample size for covariance
#'   structure modeling. *Psychological Methods, 1*(2), 130--149.
#'   \doi{10.1037/1082-989X.1.2.130}
#'
#'   Maydeu-Olivares, A. (2017). Assessing the size of model misfit in
#'   structural equation models. *Psychometrika, 82*(3), 533--558.
#'   \doi{10.1007/s11336-016-9552-7}
#'
#'   Maydeu-Olivares, A., Shi, D., & Rosseel, Y. (2018). Assessing fit in
#'   structural equation models: A Monte-Carlo evaluation of RMSEA versus
#'   SRMR confidence intervals and tests of close fit. *Structural Equation
#'   Modeling, 25*(3), 389--402. \doi{10.1080/10705511.2017.1389611}
#'
#' @return `sieve_default_conf_level()`: 0.95.
#'   `sieve_default_rmsea_conf_level()`: the RMSEA level, the value of
#'   `getOption("sieve.rmsea_conf_level")` as a number when it is one number
#'   strictly between 0 and 1, else 0.90.
#' @author Ken Kelley
#' @examples
#' sieve_default_conf_level()
#' sieve_default_rmsea_conf_level()
#' # Every RMSEA interval at 95 percent for a while, then 90 percent again.
#' old <- options(sieve.rmsea_conf_level = 0.95)
#' sieve_default_rmsea_conf_level()
#' options(old)
#' sieve_default_rmsea_conf_level()
#' @export
sieve_default_conf_level <- function() 0.95

#' The values of `sieve.rmsea_conf_level` already warned about this session
#'
#' One entry per value (its deparsed text), written by
#' `sieve_default_rmsea_conf_level()` the first time it reads that value.
#'
#' @author Ken Kelley
#' @noRd
sieve_rmsea_level_warned <- new.env(parent = emptyenv())

#' @rdname sieve_default_conf_level
#' @export
sieve_default_rmsea_conf_level <- function() {
  set <- getOption("sieve.rmsea_conf_level")
  if (is.null(set)) return(0.90)
  opt <- tryCatch(suppressWarnings(as.numeric(set)), error = function(e) NA_real_)
  if (length(opt) == 1L && is.finite(opt) && opt > 0 && opt < 1) return(opt)
  shown <- paste(deparse(set, width.cutoff = 500L), collapse = " ")
  if (!isTRUE(get0(shown, envir = sieve_rmsea_level_warned, inherits = FALSE))) {
    assign(shown, TRUE, envir = sieve_rmsea_level_warned)
    warning(structure(class = c("sieve_rmsea_level_warning", "warning", "condition"), list(
      message = sprintf(paste0(
        "options(sieve.rmsea_conf_level = %s) is not used: the level must be one number ",
        "strictly between 0 and 1 (0.95 for a 95%% interval). RMSEA intervals are at 0.90."
      ), shown),
      call = NULL
    )))
  }
  0.90
}

#' A confidence level, checked
#'
#' The one check every exported function that takes a confidence level
#' applies before anything else. A level is one number strictly between 0
#' and 1, given as a
#' number: a character value, a logical, a factor, a vector of levels,
#' `NA`, `NULL`, 0, 1, or a percentage (95) is refused, naming the argument
#' and what was received, so a percent-style level never yields `NaN`
#' limits, a zero-width or inverted interval, or one row per level.
#'
#' @param conf_level The value given.
#' @param arg The argument's name, for the refusal (`"conf_level"`, or
#'   `"level"` for [confint()]).
#' @return `conf_level` as a double; an error otherwise.
#' @author Ken Kelley
#' @noRd
sieve_check_conf_level <- function(conf_level, arg = "conf_level") {
  ok <- is.numeric(conf_level) && length(conf_level) == 1L &&
    is.finite(conf_level) && conf_level > 0 && conf_level < 1
  if (!ok) {
    shown <- paste(deparse(conf_level, width.cutoff = 200L, nlines = 2L), collapse = " ")
    if (nchar(shown) > 80L) shown <- paste0(substr(shown, 1L, 77L), "...")
    percent <- is.numeric(conf_level) && length(conf_level) == 1L && is.finite(conf_level) &&
      conf_level >= 50 && conf_level < 100
    stop(sprintf(
      "`%s` must be one number strictly between 0 and 1; received %s.%s", arg, shown,
      if (percent) sprintf(" A level is a proportion (%s for %s%%), not a percentage.", format(conf_level / 100), format(conf_level)) else ""
    ), call. = FALSE)
  }
  as.numeric(conf_level)
}

#' A page's confidence level input, read
#'
#' @param x The value of a confidence level input.
#' @param default The level when `x` is not a number strictly between 0
#'   and 1 (an empty or mistyped input).
#' @return A level.
#' @author Ken Kelley
#' @noRd
sieve_valid_conf_level <- function(x, default = sieve_default_conf_level()) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 1L && is.finite(x) && x > 0 && x < 1) x else default
}

#' A confidence level as a percentage, for a label
#'
#' @param conf_level A confidence level (0.95), or `NA`.
#' @return `"95%"`, `"97.5%"`; `""` when the level is not a finite number.
#' @author Ken Kelley
#' @noRd
sieve_ci_percent <- function(conf_level) {
  conf_level <- suppressWarnings(as.numeric(conf_level))
  vapply(conf_level, function(v) {
    if (!is.finite(v)) "" else paste0(format(100 * v, digits = 6, trim = TRUE), "%")
  }, character(1))
}

#' The level of the RMSEA interval a set of fit measures carries
#'
#' Read from the measures themselves: the `ci_level` of a
#' `sieve_fit_families()` object, of the families a selected vector
#' carries, or the vector's own `ci_level` attribute (the measures of a
#' network model, see `sieve_network_fit()`).
#'
#' @param m A result of `sieve_fit_measures()`, `sieve_fit_select()`,
#'   `sieve_fit_families()`, or `sieve_network_fit()`'s `selected`.
#' @return The level, or `NA` when the measures do not say.
#' @author Ken Kelley
#' @noRd
sieve_fit_ci_level <- function(m) {
  level <- if (inherits(m, "sieve_fit_families")) {
    m$ci_level
  } else {
    attr(m, "ci_level", exact = TRUE) %||% attr(m, "families", exact = TRUE)$ci_level
  }
  level <- suppressWarnings(as.numeric(level))
  if (length(level) == 1L && is.finite(level)) level else NA_real_
}

#' The words of an RMSEA interval's label
#'
#' @param conf_level The level (see `sieve_fit_ci_level()`).
#' @return `"95% CI"`, or `"CI"` when the level is unknown.
#' @author Ken Kelley
#' @noRd
sieve_rmsea_ci_words <- function(conf_level) {
  pct <- sieve_ci_percent(conf_level)
  ifelse(nzchar(pct), paste(pct, "CI"), "CI")
}

#' What lavaan's robust indices of a fitted model describe
#'
#' For a model with ordered indicators lavaan computes the robust CFI, TLI
#' and RMSEA (Savalei, 2021) from a normal theory fit to the polychoric
#' correlations. Thresholds, intercepts, and every equality constraint on
#' them are left out of that fit, while the degrees of freedom those
#' constraints add stay in the formula. When such constraints are part of
#' the model (the thresholds, metric, scalar, and strict rungs of an ordered
#' invariance ladder), the robust indices therefore describe the correlation
#' structure only and cannot register misfit in the constraints on
#' thresholds or intercepts. In a simulation with known populations (two
#' groups, six ordered items, 1000 cases per group, 20 data sets), a shift of
#' 0.5 in every threshold of two items was rejected by the scaled difference
#' test at the scalar rung in all 20 data sets and lowered the scaled CFI by
#' .06 on average, while the robust CFI did not fall in any of them.
#'
#' @param fit A fitted lavaan object.
#' @return `"model"` when the robust indices describe the whole model, or
#'   `"correlation_structure"` when the model has ordered indicators and
#'   equality constraints on thresholds or intercepts.
#' @author Ken Kelley
#' @noRd
sieve_robust_scope <- function(fit) {
  categorical <- isTRUE(tryCatch(lavaan::lavInspect(fit, "categorical"),
    error = function(e) FALSE
  ))
  if (!categorical) {
    return("model")
  }
  pt <- tryCatch(lavaan::parTable(fit), error = function(e) NULL)
  if (is.null(pt)) {
    return("model")
  }
  rows <- pt$op %in% c("|", "~1")
  located <- unique(c(pt$plabel[rows], pt$label[rows]))
  located <- located[!is.na(located) & nzchar(located)]
  equal <- pt[pt$op == "==", , drop = FALSE]
  if (any(equal$lhs %in% located | equal$rhs %in% located)) {
    "correlation_structure"
  } else {
    "model"
  }
}

#' Why lavaan returned no robust index for a fitted model
#'
#' lavaan returns `NA` without a reason. The conditions under which it does
#' so are known (its documentation and its source), so the applicable one is
#' named: a model that mixes ordered and continuous indicators, a polychoric
#' correlation matrix that is not positive definite, or a computation the
#' engine reported as failed.
#'
#' @param fit A fitted lavaan object.
#' @param warnings Warnings raised while the fit measures were computed.
#' @return One sentence.
#' @author Ken Kelley
#' @noRd
sieve_robust_missing_reason <- function(fit, warnings = character(0)) {
  categorical <- isTRUE(tryCatch(lavaan::lavInspect(fit, "categorical"),
    error = function(e) FALSE
  ))
  if (categorical) {
    continuous <- tryCatch(lavaan::lavNames(fit, "ov.num"), error = function(e) character(0))
    if (length(continuous)) {
      return(paste0(
        "lavaan does not compute the robust indices for a model that mixes ordered and ",
        "continuous indicators."
      ))
    }
    stats <- tryCatch(lavaan::lavInspect(fit, "sampstat"), error = function(e) NULL)
    if (!is.null(stats)) {
      if (!is.null(stats$cov)) {
        stats <- list(stats)
      }
      not_pd <- vapply(stats, function(s) {
        ev <- tryCatch(eigen(s$cov, symmetric = TRUE, only.values = TRUE)$values,
          error = function(e) NA_real_
        )
        any(!is.finite(ev)) || any(ev < sqrt(.Machine$double.eps))
      }, logical(1))
      if (any(not_pd)) {
        return(paste0(
          "The polychoric correlation matrix is not positive definite, and lavaan computes ",
          "the robust indices only when it is."
        ))
      }
    }
  }
  said <- grep("robust", gsub("\\s+", " ", warnings), value = TRUE, ignore.case = TRUE)
  if (length(said)) {
    return(sprintf(
      "lavaan could not compute it for this model (%s).",
      sub("[.]$", "", sub("^lavaan->[^:]*: *", "", said[1]))
    ))
  }
  "lavaan computed no value for this model."
}

#' The fit measures of a fitted model, by family
#'
#' Reads every family lavaan reports for a fitted model and stores each by
#' name at full precision, with its availability. Nothing is selected and
#' nothing is renamed here: see `sieve_fit_select()` for the one family a
#' display shows and [sieve_fit_compare()] for comparisons of models.
#'
#' A family is `available` when lavaan defines it for the estimator and test
#' of the fit and at least one of its indices is finite. It is `complete`
#' when both CFI and RMSEA are finite. Its `scope` is `"model"` unless it is
#' the robust family of a model with ordered indicators and constraints on
#' thresholds or intercepts (see `sieve_robust_scope()`). The reason a
#' family, or one of its values, is not reported is kept: not applicable to
#' the estimator, not computed by the engine for this model (with the
#' engine's condition where it is known), or not finite.
#'
#' SRMR, AIC, and BIC do not come in families and are stored separately,
#' each with its own availability. The SRMR is the one
#' `DMAR::sem_fit_measures()` reports (`sieve_fit_srmr()`): of the covariance
#' residuals alone and, for a fit with auxiliary variables, of the model's
#' own variables. lavaan's own `srmr`, which also averages in the mean
#' residuals of a model with a mean structure, is kept beside it.
#'
#' The RMSEA interval of every family is formed at `conf_level`, 0.90 by
#' default (`sieve_default_rmsea_conf_level()`: DMAR's default for the
#' RMSEA interval, and lavaan's, the level whose
#' limits decide the tests of close and not close fit at .05; MacCallum,
#' Browne & Sugawara, 1996): lavaan's `fitMeasures(fit, fm.args =
#' list(rmsea.ci.level = conf_level))`, whose limits equal
#' `DMAR::sem_fit_measures(fit, conf_level = conf_level)` for the standard
#' and robust versions. The interval of the unbiased SRMR (`residual =
#' TRUE`) is at the same level; the page's other intervals are at
#' `sieve_default_conf_level()`, 0.95. The tests of close and not close fit
#' do not depend on the level.
#'
#' @param fit A fitted lavaan object, or `NULL`.
#' @param status The status of the fit (see `sieve_fit_status()`); read from
#'   the fit when `NULL`.
#' @param conf_level The confidence level of the RMSEA interval (see
#'   `sieve_default_rmsea_conf_level()`).
#' @param residual `TRUE` to add the unbiased SRMR with its interval at
#'   `conf_level` (`sieve_fit_usrmr()`, run by the run policy), for the
#'   pages that show it beside the SRMR; `FALSE` (the default) leaves it
#'   out, since its time grows with the cube of the number of residual
#'   covariances.
#' @return A list of class `sieve_fit_families` with `status` (the status
#'   code), `estimator`, `test` (the test statistics lavaan computed),
#'   `categorical`, `n_groups`, `npar`, `df`, `families` (a list with
#'   `standard`, `scaled`, and `robust`, each a list with `family`,
#'   `available`, `complete`, `scope`, `reason`, `values`, `columns`, and
#'   `missing`, the reason for each value that is not reported), `srmr`,
#'   `aic`, `bic` (each a list with `value`, `available`, `reason`; `srmr`
#'   is the list `sieve_fit_srmr()` returns, which also names the measure
#'   read, lavaan's own `srmr`, and the fit's auxiliary variables),
#'   `ci_level` (the level of the RMSEA interval), `warnings`,
#'   `engine_error` (the engine's message when it returned no measures),
#'   and, with `residual = TRUE`, `usrmr` (see `sieve_fit_usrmr()`).
#' @author Ken Kelley
#' @noRd
sieve_fit_families <- function(fit, status = NULL, conf_level = sieve_default_rmsea_conf_level(),
                               residual = FALSE) {
  conf_level <- sieve_check_conf_level(conf_level)
  columns <- sieve_fit_columns()
  indices <- names(columns$standard)
  blank <- stats::setNames(rep(NA_real_, length(indices)), indices)
  no_family <- function(name, reason) {
    list(
      family = name, available = FALSE, complete = FALSE, scope = "model",
      reason = reason, values = blank, columns = columns[[name]],
      missing = stats::setNames(rep(reason, length(indices)), indices)
    )
  }
  nothing <- function(reason, code, npar = NA_real_, df = NA_real_, engine_error = NULL) {
    other <- list(value = NA_real_, available = FALSE, reason = reason)
    out <- list(
      status = code, estimator = NA_character_, test = NA_character_,
      categorical = NA, n_groups = NA_integer_, npar = npar, df = df,
      families = list(
        standard = no_family("standard", reason),
        scaled = no_family("scaled", reason),
        robust = no_family("robust", reason)
      ),
      srmr = other, aic = other, bic = other, ci_level = conf_level,
      warnings = character(0), engine_error = engine_error
    )
    if (isTRUE(residual)) {
      out$usrmr <- list(
        value = NA_real_, se = NA_real_, lower = NA_real_, upper = NA_real_,
        conf_level = conf_level, available = FALSE, computed = FALSE, reason = reason
      )
    }
    structure(out, class = "sieve_fit_families")
  }
  if (is.null(fit)) {
    return(nothing("There is no fitted model.", "failed"))
  }
  status <- status %||% sieve_status_of(fit)
  if (!sieve_has_solution(status)) {
    why <- switch(status$code,
      not_converged = "The model did not converge, so there is no solution for an index to describe.",
      not_identified = "The model is not identified, so there is no solution for an index to describe.",
      "The model could not be fit."
    )
    npar <- tryCatch(as.numeric(lavaan::lavInspect(fit, "npar")), error = function(e) NA_real_)
    return(nothing(why, status$code, npar = npar, df = status$df))
  }
  refusal <- sieve_h1_refusal(fit)
  if (!is.null(refusal)) {
    out <- nothing(refusal, status$code,
      npar = as.numeric(lavaan::lavInspect(fit, "npar")), df = status$df)
    ll <- tryCatch(lavaan::logLik(fit), error = function(e) NULL)
    if (lavaan::lavInspect(fit, "options")$missing %in% c("ml", "ml.x") &&
        !is.null(ll) && is.finite(ll)) {
      k <- attr(ll, "df")
      n <- attr(ll, "nobs")
      out$aic <- list(value = -2 * as.numeric(ll) + 2 * k, available = TRUE, reason = NA_character_)
      out$bic <- list(value = -2 * as.numeric(ll) + log(n) * k, available = TRUE, reason = NA_character_)
    }
    return(out)
  }
  run <- sieve_capture(lavaan::fitMeasures(fit, fm.args = list(rmsea.ci.level = conf_level)))
  if (!run$ok || is.null(run$value)) {
    why <- sprintf(
      "lavaan returned no fit measures for this model (%s).",
      sub("[.]$", "", gsub("\\s+", " ", run$error %||% "no reason given"))
    )
    return(nothing(why, status$code, df = status$df, engine_error = why))
  }
  fm <- run$value
  options <- tryCatch(lavaan::lavInspect(fit, "options"), error = function(e) list())
  estimator <- as.character(options$estimator %||% NA_character_)[1]
  test <- as.character(options$test %||% NA_character_)
  df <- if ("df" %in% names(fm)) unname(fm[["df"]]) else status$df
  nonexact_zero_df <- isTRUE(df == 0) && !isTRUE(sieve_zero_df_exact_fit(fit, df))
  zero_df_indices <- c("tli", "rmsea", "rmsea_lo", "rmsea_hi")
  has_scaled <- "chisq.scaled" %in% names(fm)
  # Why a robust index is absent is worked out once, and only when one is.
  robust_cols <- stats::na.omit(columns$robust)
  robust_cols <- robust_cols[robust_cols %in% names(fm)]
  robust_why <- if (length(robust_cols) && any(!is.finite(fm[robust_cols]))) {
    sieve_robust_missing_reason(fit, run$warnings)
  } else {
    "lavaan computed no value for this model."
  }
  not_applicable <- c(
    standard = "lavaan reports no standard indices for this model.",
    scaled = paste0(
      "Not applicable: the model was fit without a scaled (corrected) test statistic, so lavaan ",
      "defines no scaled statistic and no scaled indices for it."
    ),
    robust = if (has_scaled) {
      # The fit has a corrected statistic, but one lavaan builds no robust
      # index from (a mean and variance adjusted, Satterthwaite type,
      # statistic, for example).
      sprintf(
        paste0(
          "Not applicable: lavaan defines no robust indices for the test statistic of this ",
          "fit (%s)."
        ),
        paste(sprintf("\"%s\"", sieve_lavaan_corrected_tests(test)), collapse = ", ")
      )
    } else {
      paste0(
        "Not applicable: lavaan defines the robust indices for a scaled test statistic or for ",
        "full information maximum likelihood, and this fit has neither."
      )
    }
  )
  build <- function(name) {
    cols <- columns[[name]]
    defined <- !is.na(cols) & cols %in% names(fm)
    values <- blank
    values[defined] <- as.numeric(fm[cols[defined]])
    uls_robust <- identical(name, "robust") && estimator == "ULS" &&
      !length(lavaan::lavNames(fit, "ov.ord"))
    if (uls_robust) values[] <- NA_real_
    if (nonexact_zero_df) values[zero_df_indices] <- NA_real_
    core <- c("cfi", "tli", "rmsea")
    applicable <- any(defined[core])
    why <- function(index) {
      if (uls_robust) return("ML-derived robust indices are withheld for continuous ULS; use eligible scaled indices (Savalei, 2018).")
      if (nonexact_zero_df && index %in% zero_df_indices) {
        return(paste0("The model has zero degrees of freedom without established exact fit; ",
          "the saturated-model TLI and RMSEA conventions do not apply."))
      }
      if (!applicable) {
        return(not_applicable[[name]])
      }
      if (identical(name, "robust") && index %in% c("chisq", "df", "pvalue")) {
        return(sprintf(
          "The robust family has no test statistic of its own; the model test that goes with it is the %s one.",
          if (has_scaled) "scaled" else "standard"
        ))
      }
      if (!defined[[index]]) {
        return("lavaan does not define this value for this estimator.")
      }
      value <- values[[index]]
      if (is.nan(value) || is.infinite(value)) {
        return("The engine returned a value that is not finite.")
      }
      if (identical(index, "pvalue") && is.finite(df) && df == 0) {
        return("The model has zero degrees of freedom, so there is no test.")
      }
      if (identical(name, "standard") && identical(index, "pvalue") &&
        estimator %in% c("DWLS", "ULS")) {
        return(paste0(
          "lavaan reports no p-value for the unscaled statistic of this estimator, which has ",
          "no chi square reference distribution."
        ))
      }
      if (identical(name, "robust")) {
        return(robust_why)
      }
      if (is.finite(df) && df == 0) {
        return("lavaan computed no value: the model has zero degrees of freedom.")
      }
      "lavaan computed no value for this model."
    }
    absent <- indices[!is.finite(values)]
    missing <- stats::setNames(rep(NA_character_, length(indices)), indices)
    missing[absent] <- vapply(absent, why, character(1))
    available <- applicable && any(is.finite(values[core]))
    complete <- is.finite(values[["cfi"]]) && is.finite(values[["rmsea"]])
    scope <- if (identical(name, "robust") && available) sieve_robust_scope(fit) else "model"
    reason <- if (uls_robust) {
      why("cfi")
    } else if (!applicable) {
      not_applicable[[name]]
    } else if (!complete) {
      unname(missing[c("cfi", "rmsea")][!is.na(missing[c("cfi", "rmsea")])][1])
    } else if (!identical(scope, "model")) {
      paste0(
        "With ordered indicators lavaan computes the robust indices from the correlations ",
        "alone: thresholds, intercepts, and the equality constraints on them are left out. ",
        "This model has such constraints, so its robust indices cannot register misfit in them."
      )
    } else {
      NA_character_
    }
    list(
      family = name, available = available, complete = complete, scope = scope,
      reason = reason, values = values, columns = cols, missing = missing
    )
  }
  single <- function(name, absent_reason) {
    if (!name %in% names(fm)) {
      return(list(value = NA_real_, available = FALSE, reason = absent_reason))
    }
    value <- unname(fm[[name]])
    list(
      value = value, available = is.finite(value),
      reason = if (is.finite(value)) NA_character_ else "lavaan computed no value for this model."
    )
  }
  no_likelihood <- paste0(
    "Not applicable: this estimator has no likelihood, so lavaan defines no information ",
    "criterion for the model."
  )
  out <- list(
    status = status$code,
    estimator = estimator,
    test = test,
    categorical = isTRUE(tryCatch(lavaan::lavInspect(fit, "categorical"),
      error = function(e) FALSE
    )),
    n_groups = tryCatch(as.integer(lavaan::lavInspect(fit, "ngroups")),
      error = function(e) NA_integer_
    ),
    npar = if ("npar" %in% names(fm)) unname(fm[["npar"]]) else NA_real_,
    df = df,
    families = list(
      standard = build("standard"), scaled = build("scaled"),
      robust = build("robust")
    ),
    # The SRMR DMAR reports (see sieve_fit_srmr()): of the covariance residuals
    # alone and, with auxiliary variables, of the model's own variables.
    srmr = sieve_fit_srmr(fit, fm, aux = as.character(options$aux %||% character(0)), conf_level = conf_level),
    aic = single("aic", no_likelihood),
    bic = single("bic", no_likelihood),
    ci_level = if ("rmsea.ci.level" %in% names(fm)) unname(fm[["rmsea.ci.level"]]) else conf_level,
    warnings = run$warnings,
    engine_error = NULL
  )
  # The unbiased SRMR with its interval at the RMSEA level, where a page
  # shows it beside the SRMR (see sieve_fit_usrmr()). Without it the
  # families hold no usrmr element at all, as they do once
  # sieve_fit_drop_usrmr() has removed it from measures another page read,
  # so a page that does not show it reads identical measures whichever
  # page fit the model first.
  if (isTRUE(residual)) out$usrmr <- sieve_fit_usrmr(fit, conf_level = conf_level)
  structure(out, class = "sieve_fit_families")
}

#' The Fit Measure SIEVER Reports as SRMR
#'
#' The standardized root mean square residual of the covariances: the square
#' root of the mean, over the variances and covariances of the observed
#' variables, of the squared difference between an observed and a model
#' implied covariance, each divided by the two observed standard deviations
#' (Bentler's standardization). It is lavaan's `srmr_bentler_nomean` and the
#' `srmr` row of `DMAR::sem_fit_measures()`, which reads the same measure, so
#' SIEVER and DMAR report one number for one fit.
#'
#' lavaan's own `srmr` is a different average for a continuous model with a
#' mean structure, which every FIML fit and every multiple group fit has: it
#' also averages in the mean residuals. With free intercepts those residuals
#' are zero, so the same model with the same covariance residuals has a
#' smaller `srmr` under FIML than under complete-data ML (0.060 against 0.065
#' for the three-factor model of the nine Holzinger and Swineford tests), and
#' with intercepts held equal across groups it has a different one again.
#' With ordered indicators lavaan's `srmr` is already the measure without
#' the thresholds, so there the two agree.
#'
#' Auxiliary variables are the other difference. lavaan keeps an auxiliary
#' variable (its `aux`) among the observed variables, so its measures average
#' the auxiliary's variances and covariances in, with residuals of zero or
#' nearly so, and the SRMR of the same model is smaller with them (0.060
#' with one and 0.055 with two, for the model above). DMAR forms the SRMR of
#' such a fit over the model's own variables, and so SIEVER asks DMAR for
#' it: see `sieve_fit_srmr()`.
#'
#' @param available The names of the measures lavaan returned.
#' @return `"srmr_bentler_nomean"`, or `"srmr"` where lavaan returned no such
#'   measure, which is the rule `DMAR::sem_fit_measures()` reads it by.
#' @author Ken Kelley
#' @noRd
sieve_srmr_measure <- function(available = "srmr_bentler_nomean") {
  if ("srmr_bentler_nomean" %in% available) "srmr_bentler_nomean" else "srmr"
}

#' The SRMR of a Fit, as DMAR Reports It
#'
#' Without auxiliary variables the value is read from the measures lavaan
#' already returned (`sieve_srmr_measure()`), which is what
#' `DMAR::sem_fit_measures()` reads. With auxiliary variables lavaan's measure
#' averages them in, and DMAR's SRMR is of the model's own variables, so
#' `DMAR::sem_fit_measures()` is called and its `srmr` row is the value: the
#' two programs then report one SRMR for one fit.
#' Where DMAR returns none for the fit, lavaan's measure stays and the note
#' says the auxiliary variables are averaged in.
#'
#' @param fit A fitted lavaan object with a solution.
#' @param fm The measures `lavaan::fitMeasures()` returned for it.
#' @param aux The fit's auxiliary variables.
#' @param conf_level The level passed to DMAR (the level of its RMSEA
#'   interval; it does not enter the SRMR).
#' @return A list with `value`, `available`, `reason`, `measure` (the lavaan
#'   measure read), `lavaan` (lavaan's own `srmr`), `aux`, `source`
#'   (`"lavaan"` or `"DMAR"`, where the value was read), and `with_aux`
#'   (lavaan's measure with the auxiliary variables averaged in, when the
#'   value is DMAR's; `NA` otherwise).
#' @author Ken Kelley
#' @noRd
sieve_fit_srmr <- function(fit, fm, aux = character(0), conf_level = sieve_default_rmsea_conf_level()) {
  measure <- sieve_srmr_measure(names(fm))
  value <- if (measure %in% names(fm)) unname(fm[[measure]]) else NA_real_
  out <- list(
    value = value, available = is.finite(value),
    reason = if (is.finite(value)) {
      NA_character_
    } else if (measure %in% names(fm)) {
      "lavaan computed no value for this model."
    } else {
      "lavaan reports no SRMR for this model."
    },
    measure = measure,
    lavaan = if ("srmr" %in% names(fm)) unname(fm[["srmr"]]) else NA_real_,
    aux = aux, source = "lavaan", with_aux = NA_real_
  )
  if (!length(aux) || !is.finite(value)) {
    return(out)
  }
  engine <- sieve_capture(DMAR::sem_fit_measures(fit, conf_level = conf_level))
  own <- if (engine$ok && is.data.frame(engine$value) && all(c("term", "value") %in% names(engine$value))) {
    suppressWarnings(as.numeric(engine$value$value[engine$value$term == "srmr"]))
  } else {
    numeric(0)
  }
  if (length(own) == 1L && is.finite(own)) {
    out$with_aux <- value
    out$value <- own
    out$source <- "DMAR"
  }
  out
}

#' The estimated time of the unbiased SRMR and its interval
#'
#' `DMAR::sem_residual_fit()` takes the asymptotic covariance matrix of the
#' `t = p (p + 1) / 2` standardized residual covariances from
#' `lavaan::lavResiduals()`, and its time grows with about the cube of `t`.
#' Measured with DMAR 1.1.0, lavaan 0.7-3, and R 4.6.2, for one-group ML
#' fits with FIML of complete continuous items:
#' 0.025 seconds at 9 items (t = 45), 0.46 at 24 (300), 3.9 at 40 (820) and
#' 270 at 80 (3,240). The estimate is `2.2e-6 t^2 + 7.9e-9 t^3` seconds,
#' which is 0.41, 5.8 and 292 seconds at the last three and so errs high
#' between them; it passes two seconds at about 33 items.
#'
#' @param p The number of observed variables of the model.
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_usrmr_seconds <- function(p) {
  p <- as.numeric(p)
  t <- p * (p + 1) / 2
  2.2e-6 * t^2 + 7.9e-9 * t^3
}

#' The same model fit without its auxiliary variables, when that changes nothing
#'
#' `DMAR::sem_residual_fit()` refuses a fit with auxiliary variables
#' (lavaan's `aux`): lavaan's residual summary averages over them, and their
#' residuals of about zero lower the SRMR. When every respondent the fit
#' used is observed on every model variable, an auxiliary variable changes
#' nothing in the model's fit (its saturated correlates add parameters of
#' their own only), so the model fit to the same respondents without it is
#' the one to summarize, as DMAR's refusal says. The refit is made from the
#' fit's own specification (`attr(fit, "sieve_spec")`) on its own data
#' (`lavaan::lavInspect(fit, "data")`), and it is used only when its
#' estimates equal the fit's to the optimizer's convergence (a relative
#' difference of 1e-4; on the nine Holzinger and Swineford tests with the
#' auxiliary age the largest was 1.5e-6).
#'
#' @param fit A fitted lavaan object.
#' @return `NULL` when there is no such refit (no auxiliary variable, no
#'   specification, several groups, a design column, a missing value on a
#'   model variable, or estimates that differ), otherwise a list with `fit`
#'   (the refit), `aux`, `observed` (the model variables), `fun` (`"cfa"`
#'   or `"sem"`), and `args` (the refit's arguments other than `model` and
#'   `data`).
#' @author Ken Kelley
#' @noRd
sieve_fit_without_aux <- function(fit) {
  aux <- tryCatch(as.character(lavaan::lavInspect(fit, "options")$aux %||% character(0)),
    error = function(e) character(0))
  spec <- attr(fit, "sieve_spec", exact = TRUE)
  if (!length(aux) || is.null(spec) || length(spec$ordered) ||
    !identical(tryCatch(as.integer(lavaan::lavInspect(fit, "ngroups")), error = function(e) NA_integer_), 1L) ||
    length(c(spec$engine_args$cluster, spec$engine_args$sampling.weights))) {
    return(NULL)
  }
  data <- tryCatch(as.data.frame(lavaan::lavInspect(fit, "data")), error = function(e) NULL)
  observed <- setdiff(colnames(data), aux)
  if (is.null(data) || !length(observed) || anyNA(data[, observed, drop = FALSE])) {
    return(NULL)
  }
  spec$engine_args$aux <- NULL
  args <- sieve_spec_args(spec)
  args$check.post <- NULL
  fun <- if (identical(spec$fun, "sem")) "sem" else "cfa"
  run <- sieve_capture(do.call(
    if (identical(fun, "sem")) lavaan::sem else lavaan::cfa,
    c(args, list(data = data[, observed, drop = FALSE]))
  ))
  if (!run$ok || !inherits(run$value, "lavaan")) {
    return(NULL)
  }
  before <- lavaan::coef(fit)
  after <- lavaan::coef(run$value)
  if (!all(names(after) %in% names(before)) ||
    !isTRUE(all.equal(as.numeric(before[names(after)]), as.numeric(after), tolerance = 1e-4))) {
    return(NULL)
  }
  args$model <- NULL
  list(fit = run$value, aux = aux, observed = observed, fun = fun, args = args)
}

#' Does DMAR refuse the unbiased SRMR of this fit before computing it?
#'
#' `DMAR::sem_residual_fit()` checks its scope before it forms any residual
#' covariance: a fit that did not converge, `conditional.x = TRUE`, ordered
#' variables, an estimator other than ML, a likelihood other than lavaan's
#' normal one, several groups, several levels, sampling weights, a missing
#' data treatment other than listwise deletion, FIML or pairwise deletion,
#' and incomplete analysis data (DMAR 1.1.0, the checks at the top of
#' `sem_residual_fit()`). These are read here the way DMAR reads them. A
#' fit with auxiliary variables is refused too, but SIEVER then summarizes
#' the same model without them (`sieve_fit_without_aux()`), which is a full
#' computation, so such a fit counts as refused only when that refit is not
#' possible: no SIEVER specification, a sampling design, or a missing value
#' on a model variable. The admissibility check DMAR also makes is left to
#' DMAR, since SIEVER's own reading of an improper solution need not be
#' DMAR's; a fit DMAR refuses for it waits for the button.
#'
#' @param fit A fitted lavaan object.
#' @return `TRUE` when DMAR will refuse at once, otherwise `FALSE` (also
#'   when the fit cannot be read).
#' @author Ken Kelley
#' @noRd
sieve_usrmr_refused <- function(fit) {
  if (!inherits(fit, "lavaan")) {
    return(TRUE)
  }
  out <- tryCatch({
    opt <- lavaan::lavInspect(fit, "options")
    scope <- !isTRUE(lavaan::lavInspect(fit, "converged")) ||
      isTRUE(opt$conditional.x) ||
      length(lavaan::lavNames(fit, "ov.ord")) > 0L ||
      !identical(opt$estimator, "ML") ||
      !identical(opt$likelihood, "normal") ||
      !identical(as.integer(lavaan::lavInspect(fit, "ngroups")), 1L) ||
      !identical(as.integer(lavaan::lavInspect(fit, "nlevels")), 1L) ||
      isTRUE(opt$.sampling.weights) ||
      !isTRUE(opt$missing %in% c("listwise", "ml", "ml.x", "pairwise"))
    if (scope) {
      return(TRUE)
    }
    aux <- as.character(opt$aux %||% character(0))
    rows <- !is.null(lavaan::lavInspect(fit, "case.idx"))
    data <- lavaan::lavInspect(fit, "data")
    model <- if (length(aux) && !is.null(colnames(data))) {
      data[, setdiff(colnames(data), aux), drop = FALSE]
    } else {
      data
    }
    incomplete <- rows && anyNA(unlist(model))
    if (!length(aux)) {
      return(incomplete)
    }
    spec <- attr(fit, "sieve_spec", exact = TRUE)
    is.null(spec) || incomplete || length(spec$ordered) > 0L ||
      length(c(spec$engine_args$cluster, spec$engine_args$sampling.weights)) > 0L
  }, error = function(e) FALSE)
  isTRUE(out)
}

#' The Unbiased SRMR of a Fit, With Its Interval, as DMAR Reports It
#'
#' `DMAR::sem_residual_fit()` gives the asymptotically unbiased estimate of
#' the population SRMR of Maydeu-Olivares (2017), its standard error and its
#' large sample normal reference interval (Maydeu-Olivares, Shi & Rosseel,
#' 2018, Eqs. 20 and 22), whose limits are reported as lavaan returns them,
#' without truncation at zero. The sample SRMR beside it (`sieve_fit_srmr()`)
#' overestimates the population value, since sampling error adds to every
#' residual. The interval is at `conf_level`, the level of the RMSEA
#' interval (`sieve_default_rmsea_conf_level()`, 0.90 by default, DMAR's
#' default for this interval too): one interval policy for the fit measures.
#' No bound of close fit is imposed (`close_fit = NULL`), since SIEVER
#' applies no cutoff. Where DMAR does not compute it (an estimator other than
#' ML, ordered items, several groups, several levels, sampling weights,
#' incomplete data under FIML or pairwise deletion, a two-stage fit) its
#' refusal is kept verbatim as the reason. A fit with auxiliary variables
#' whose model variables are all observed is summarized, as DMAR's refusal
#' advises, by the same model fit without them (`sieve_fit_without_aux()`),
#' which has the same estimates and is the SRMR's own variables, as
#' `sieve_fit_srmr()` reports the SRMR.
#'
#' The computation grows with the cube of the number of residual
#' covariances (`sieve_usrmr_seconds()`), so with `compute = "auto"` it is
#' made only when the run policy lets a result of that estimated time run
#' by itself (`sieve_runs_by_itself()`: two seconds or less, or every time
#' under `options(sieve.auto_run = "always")`); otherwise the result says
#' it was not computed, with the estimate, and a page offers it on a button.
#' A fit DMAR refuses (`sieve_usrmr_refused()`) is not held to the run
#' policy: DMAR refuses it before it forms any residual covariance, in a
#' few hundredths of a second, so its reason is given at once in place of a
#' wait estimate and a button that would only return it.
#'
#' @param fit A fitted lavaan object.
#' @param conf_level The level of the interval.
#' @param compute `"auto"` (the run policy decides) or `"always"`.
#' @return A list with `value` (the unbiased SRMR), `se`, `lower`, `upper`,
#'   `conf_level`, `srmr` (DMAR's sample SRMR), `n`, `residual` (the
#'   residual covariance and information DMAR used, in words), `available`,
#'   `computed` (`FALSE` when it was left for a button), `seconds` (the
#'   estimated time), `reason` (why no value or no interval is reported, or
#'   `NA`), `warnings`, `call` (the R that computes it, reading the fit
#'   as `fit`), and, when it was computed on the same model without the
#'   fit's auxiliary variables (`sieve_fit_without_aux()`), `without_aux`.
#' @author Ken Kelley
#' @noRd
sieve_fit_usrmr <- function(fit, conf_level = sieve_default_rmsea_conf_level(),
                            compute = c("auto", "always")) {
  compute <- match.arg(compute)
  conf_level <- sieve_check_conf_level(conf_level)
  p <- tryCatch(length(lavaan::lavNames(fit, "ov")), error = function(e) NA_integer_)
  out <- list(
    value = NA_real_, se = NA_real_, lower = NA_real_, upper = NA_real_,
    conf_level = conf_level, srmr = NA_real_, n = NA_real_, residual = NA_character_,
    available = FALSE, computed = FALSE, seconds = sieve_usrmr_seconds(p),
    reason = NA_character_, warnings = character(0),
    call = sprintf("DMAR::sem_residual_fit(fit, conf_level = %s)", sieve_num_lit(conf_level))
  )
  if (!inherits(fit, "lavaan")) {
    out$reason <- "There is no fitted model."
    return(out)
  }
  if (identical(compute, "auto") && !sieve_runs_by_itself(out$seconds) && !sieve_usrmr_refused(fit)) {
    policy <- getOption("sieve.auto_run", "default")
    never <- identical(policy, FALSE) || identical(policy, "never")
    what <- sprintf(
      paste0(
        "DMAR::sem_residual_fit() forms the asymptotic covariance matrix of the %s standardized residual ",
        "covariances of these %d variables, which is estimated to take %s here (the time grows with the cube ",
        "of their number)"
      ),
      format(p * (p + 1) / 2, big.mark = ","), as.integer(p), sieve_duration_label(out$seconds)
    )
    out$reason <- if (never) {
      sprintf("options(sieve.auto_run = \"never\") leaves every computation to its button. %s.", what)
    } else {
      sprintf(
        "%s, longer than the %s seconds the run policy lets a result run by itself.", what,
        format(sieve_auto_seconds())
      )
    }
    return(out)
  }
  out$computed <- TRUE
  run <- sieve_capture(DMAR::sem_residual_fit(fit, conf_level = conf_level))
  if (!run$ok) {
    # DMAR refuses a fit with auxiliary variables and names the remedy: with
    # every model variable observed, the same model without them.
    without <- sieve_fit_without_aux(fit)
    if (!is.null(without)) {
      refit <- sieve_capture(DMAR::sem_residual_fit(without$fit, conf_level = conf_level))
      if (refit$ok) {
        run <- refit
        out$without_aux <- without[c("aux", "observed", "fun", "args")]
      }
    }
  }
  out$warnings <- run$warnings
  if (!run$ok || !is.data.frame(run$value)) {
    out$reason <- gsub("\\s+", " ", run$error %||% "DMAR returned no value for this fit.")
    return(out)
  }
  tbl <- run$value
  pick <- function(term) {
    v <- suppressWarnings(as.numeric(tbl$value[tbl$term == term]))
    if (length(v) == 1L) v else NA_real_
  }
  out$value <- pick("unbiased_srmr")
  out$se <- pick("unbiased_srmr_se")
  out$lower <- pick("unbiased_srmr_ci_lower")
  out$upper <- pick("unbiased_srmr_ci_upper")
  out$srmr <- pick("srmr")
  out$n <- pick("N")
  info <- attr(tbl, "sem_fit")
  if (!is.null(info$residual_acov)) {
    out$residual <- sprintf("%s residual covariance, %s information", info$residual_acov, info$information %||% "the fit's")
  }
  if (!is.null(out$without_aux)) {
    aux <- out$without_aux$aux
    out$residual <- paste0(
      if (is.na(out$residual)) "" else paste0(out$residual, "; "),
      sprintf(
        "of the model's own variables, fit to the same respondents without the auxiliary variable%s %s, which with every model variable observed %s nothing in the model's fit",
        if (length(aux) == 1L) "" else "s", paste(aux, collapse = ", "), if (length(aux) == 1L) "changes" else "change"
      )
    )
  }
  out$available <- is.finite(out$value)
  if (!out$available) {
    out$reason <- "DMAR returned no unbiased SRMR for this fit."
  } else if (!is.finite(out$lower) || !is.finite(out$upper)) {
    out$reason <- paste(c("DMAR returned no interval for the unbiased SRMR of this fit.", out$warnings), collapse = " ")
  }
  out
}

#' Add the unbiased SRMR to a set of fit measures
#'
#' @param m A result of `sieve_fit_measures()` or `sieve_fit_select()`.
#' @param usrmr A result of `sieve_fit_usrmr()`.
#' @return `m` with `usrmr`, `usrmr_lo` and `usrmr_hi` set and the
#'   families it carries holding `usrmr`.
#' @author Ken Kelley
#' @noRd
sieve_fit_set_usrmr <- function(m, usrmr) {
  if (is.null(m) || is.null(usrmr)) {
    return(m)
  }
  keep <- attributes(m)
  values <- c(usrmr = usrmr$value, usrmr_lo = usrmr$lower, usrmr_hi = usrmr$upper)
  out <- c(unclass(m)[setdiff(names(m), names(values))], values)
  for (nm in setdiff(names(keep), "names")) attr(out, nm) <- keep[[nm]]
  families <- attr(out, "families")
  if (!is.null(families)) {
    families$usrmr <- usrmr
    attr(out, "families") <- families
  }
  out
}

#' Fit measures without the unbiased SRMR
#'
#' @param m A result of `sieve_fit_measures()` or `sieve_fit_select()`, or
#'   `NULL`.
#' @return `m` without `usrmr`, `usrmr_lo` and `usrmr_hi` and without the
#'   `usrmr` element of its families: the measures of a page that does not
#'   show the unbiased SRMR. Measures that do not hold it are returned as
#'   they are.
#' @author Ken Kelley
#' @noRd
sieve_fit_drop_usrmr <- function(m) {
  families <- attr(m, "families")
  drop <- intersect(c("usrmr", "usrmr_lo", "usrmr_hi"), names(m))
  if (is.null(m) || (!length(drop) && is.null(families$usrmr))) {
    return(m)
  }
  keep <- attributes(m)
  out <- unclass(m)[setdiff(names(m), drop)]
  for (nm in setdiff(names(keep), "names")) attr(out, nm) <- keep[[nm]]
  if (!is.null(families)) {
    families$usrmr <- NULL
    attr(out, "families") <- families
  }
  out
}

#' Fit measures with the unbiased SRMR, computed once
#'
#' @param m A result of `sieve_fit_measures()`, or `NULL`.
#' @param fit The fit the measures describe.
#' @param compute See `sieve_fit_usrmr()`.
#' @return `m` with the unbiased SRMR at the level of its RMSEA interval;
#'   measures that already hold it are returned as they are.
#' @author Ken Kelley
#' @noRd
sieve_fit_with_usrmr <- function(m, fit, compute = "auto") {
  if (is.null(m) || !is.null(attr(m, "families")$usrmr)) {
    return(m)
  }
  level <- sieve_fit_ci_level(m)
  if (!is.finite(level)) level <- sieve_default_rmsea_conf_level()
  sieve_fit_set_usrmr(m, sieve_fit_usrmr(fit, conf_level = level, compute = compute))
}

#' The unbiased SRMR of a set of fit measures, in words
#'
#' @param m A result of `sieve_fit_measures()` or `sieve_fit_select()`.
#' @param num A function formatting one number for display.
#' @return `"unbiased SRMR = .056, 90% CI [.038, .073]"`, `"unbiased SRMR =
#'   .000 (no interval)"`, `"unbiased SRMR not computed"`, `"unbiased SRMR
#'   n/a"`, or `NULL` when the measures do not hold it.
#' @author Ken Kelley
#' @noRd
sieve_usrmr_words <- function(m, num = function(x) sprintf("%.3f", x)) {
  u <- attr(m, "families")$usrmr
  if (is.null(u)) {
    return(NULL)
  }
  if (!isTRUE(u$computed) && isTRUE(is.finite(u$seconds))) {
    return("unbiased SRMR not computed")
  }
  if (!isTRUE(u$available)) {
    return("unbiased SRMR n/a")
  }
  if (!is.finite(u$lower) || !is.finite(u$upper)) {
    return(sprintf("unbiased SRMR = %s (no interval)", num(u$value)))
  }
  sprintf(
    "unbiased SRMR = %s, %s [%s, %s]", num(u$value), sieve_rmsea_ci_words(u$conf_level),
    num(u$lower), num(u$upper)
  )
}

#' What the unbiased SRMR beside the SRMR is, in words
#'
#' @param usrmr The `usrmr` element of a `sieve_fit_families()` object, or
#'   `NULL`.
#' @return A sentence, or `NULL` when the measures do not hold it.
#' @author Ken Kelley
#' @noRd
sieve_usrmr_note <- function(usrmr) {
  if (is.null(usrmr)) {
    return(NULL)
  }
  if (!isTRUE(usrmr$available)) {
    return(sprintf(
      if (isTRUE(usrmr$computed) || is.null(usrmr$seconds)) {
        "The unbiased SRMR and its interval are not reported for this fit: %s"
      } else {
        "The unbiased SRMR and its interval were not computed with the fit: %s"
      },
      usrmr$reason
    ))
  }
  paste0(
    "The unbiased SRMR (Maydeu-Olivares, 2017) estimates the population SRMR, which the sample SRMR overestimates; ",
    sprintf(
      "its %s confidence interval (Maydeu-Olivares, Shi & Rosseel, 2018) is at the level of the RMSEA interval, ",
      sieve_ci_percent(usrmr$conf_level)
    ),
    "from DMAR::sem_residual_fit()",
    if (!is.na(usrmr$residual)) sprintf(" (%s)", usrmr$residual) else "",
    ". Its limits are as lavaan returns them, not truncated at zero.",
    if (!is.finite(usrmr$lower) || !is.finite(usrmr$upper)) {
      paste0(" ", usrmr$reason)
    } else {
      ""
    }
  )
}

#' The recorded R of the unbiased SRMR beside a fit's SRMR
#'
#' @param usrmr A result of `sieve_fit_usrmr()`, or `NULL`.
#' @param fit_name The name of the fit in the recorded R.
#' @param model_name The name of its model syntax there (read by the refit
#'   without auxiliary variables).
#' @return Code lines: the DMAR call when the value was computed, a comment
#'   with DMAR's reason when it was refused, the call as a comment with the
#'   estimated time when it was left for a button; `NULL` without one.
#' @author Ken Kelley
#' @noRd
sieve_usrmr_code <- function(usrmr, fit_name, model_name = sub("_fit$", "_model", fit_name)) {
  if (is.null(usrmr) || is.null(usrmr$conf_level)) {
    return(NULL)
  }
  call <- sprintf("DMAR::sem_residual_fit(%s, conf_level = %s)", fit_name, sieve_num_lit(usrmr$conf_level))
  if (isTRUE(usrmr$available) && !is.null(usrmr$without_aux)) {
    w <- usrmr$without_aux
    refit <- paste0(fit_name, "_model_only")
    w$args <- w$args[!vapply(w$args, is.null, logical(1))]
    args <- vapply(names(w$args), function(nm) {
      sprintf("%s = %s", nm, paste(deparse(w$args[[nm]], width.cutoff = 500L), collapse = " "))
    }, character(1))
    return(c(
      sprintf(
        "# The unbiased SRMR is of the model's own variables: the same model fit to the same respondents without the auxiliary variable%s %s,",
        if (length(w$aux) == 1L) "" else "s", paste(w$aux, collapse = ", ")
      ),
      "# which with every model variable observed changes nothing in the model's fit (DMAR::sem_residual_fit() refuses a fit with aux).",
      sprintf(
        "%s <- lavaan::%s(%s, data = as.data.frame(lavaan::lavInspect(%s, \"data\"))[, %s, drop = FALSE], %s)",
        refit, w$fun, model_name, fit_name, paste(deparse(w$observed, width.cutoff = 500L), collapse = " "),
        paste(args, collapse = ", ")
      ),
      sprintf(
        "DMAR::sem_residual_fit(%s, conf_level = %s)  # the unbiased SRMR (Maydeu-Olivares, 2017) with its %s interval, shown beside the SRMR",
        refit, sieve_num_lit(usrmr$conf_level), sieve_ci_percent(usrmr$conf_level)
      )
    ))
  }
  if (isTRUE(usrmr$available)) {
    return(sprintf(
      "%s  # the unbiased SRMR (Maydeu-Olivares, 2017) with its %s interval, shown beside the SRMR",
      call, sieve_ci_percent(usrmr$conf_level)
    ))
  }
  if (!isTRUE(usrmr$computed) && isTRUE(is.finite(usrmr$seconds))) {
    return(c(
      sprintf(
        "# The unbiased SRMR with its %s interval was not computed with the fit (estimated time: %s); to compute it:",
        sieve_ci_percent(usrmr$conf_level), sieve_duration_label(usrmr$seconds)
      ),
      paste0("# ", call)
    ))
  }
  sieve_comment_lines("The unbiased SRMR is not reported for this fit:", usrmr$reason)
}

#' The unbiased SRMR a page computes on its button
#'
#' A page that shows the SRMR holds its unbiased estimate with the interval
#' when the run policy lets it be computed with the fit
#' (`sieve_fit_usrmr()`), and otherwise offers it here: the reason and a
#' button that states the estimated time. The value the button computes is
#' kept for the fit it was computed on and recorded in the session's R.
#'
#' @param input,output,session The module's.
#' @param state The shared state store.
#' @param fit A function returning the fit on screen.
#' @param key A function returning the identity of that fit (its analysis
#'   identifier).
#' @param measures A function returning the page's measures, as merged by
#'   the returned function.
#' @param fit_name The fit's name in the recorded R.
#' @param page The page, for the recorded step ("CFA").
#' @param ui_id The output that shows the button; the button is
#'   `paste0(ui_id, "_go")`.
#' @return A function of a set of measures that adds the value computed on
#'   the button for the fit on screen.
#' @author Ken Kelley
#' @noRd
sieve_usrmr_wire <- function(input, output, session, state, fit, key, measures, fit_name, page,
                             ui_id = "usrmr_action") {
  held <- shiny::reactiveVal(NULL)
  go <- paste0(ui_id, "_go")
  merge <- function(m) {
    h <- held()
    if (is.null(m) || is.null(h) || !identical(h$key, tryCatch(key(), error = function(e) NULL))) {
      return(m)
    }
    sieve_fit_set_usrmr(m, h$value)
  }
  output[[ui_id]] <- shiny::renderUI({
    sieve_usrmr_button(attr(measures(), "families")$usrmr, session$ns(go))
  })
  shiny::observeEvent(input[[go]], {
    f <- fit()
    m <- measures()
    shiny::req(f, m)
    level <- attr(m, "families")$usrmr$conf_level %||% sieve_default_rmsea_conf_level()
    value <- shiny::withProgress(
      message = "Computing the unbiased SRMR and its interval", value = 0.5,
      sieve_fit_usrmr(f, conf_level = level, compute = "always")
    )
    id <- key()
    held(list(key = id, value = value))
    # The analysis registry's record of the fit holds it too, so the
    # saved-results report reads what the page shows.
    sieve_registry_set_usrmr(state, id, value)
    sieve_log(
      state, sprintf("%s: the unbiased SRMR with its %s interval, on request", page, sieve_ci_percent(level)),
      sieve_usrmr_code(value, fit_name)
    )
  }, ignoreInit = TRUE)
  merge
}

#' The button that computes an unbiased SRMR left for it
#'
#' @param usrmr The `usrmr` element of a page's measures (a result of
#'   `sieve_fit_usrmr()`), or `NULL`.
#' @param id The button's input identifier.
#' @return A [shiny::div()] with the button, which states the estimated
#'   time, and the reason it was not computed with the fit; `NULL` when
#'   there is nothing to compute (no such element, a value, or DMAR's
#'   refusal).
#' @author Ken Kelley
#' @noRd
sieve_usrmr_button <- function(usrmr, id = "usrmr_action_go") {
  if (is.null(usrmr) || isTRUE(usrmr$computed) || !isTRUE(is.finite(usrmr$seconds))) {
    return(NULL)
  }
  shiny::div(
    class = "small mb-2",
    shiny::actionButton(id, sprintf(
      "Compute the Unbiased SRMR and Its %s Interval (%s)", sieve_ci_percent(usrmr$conf_level),
      sieve_duration_label(usrmr$seconds)
    ), class = "btn-sm btn-outline-primary"),
    shiny::tags$span(class = "text-muted", " ", usrmr$reason)
  )
}

#' Record an unbiased SRMR computed on request with its analysis
#'
#' The fit measures of a recorded analysis (`estimates$fit` of its record in
#' `state$analyses`, see `sieve_register_analysis()`) are those read with
#' the fit; the unbiased SRMR a page computes later on its button is added
#' to them here, and the methods the record cites are read again from them
#' (`sieve_spec_methods()`), so the saved-results report shows the value and
#' its references as the page does.
#'
#' @param state The shared state store.
#' @param id The analysis identifier of the fit (`"A3"`).
#' @param usrmr A result of `sieve_fit_usrmr()`.
#' @return `TRUE` when a record was updated, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_registry_set_usrmr <- function(state, id, usrmr) {
  analyses <- shiny::isolate(state$analyses)
  if (is.null(id) || length(id) != 1L || is.na(id) || is.null(analyses[[id]]) || is.null(usrmr)) {
    return(invisible(FALSE))
  }
  record <- analyses[[id]]
  fit <- record$estimates$fit
  if (!is.numeric(fit) || is.null(attr(fit, "families"))) {
    return(invisible(FALSE))
  }
  record$estimates$fit <- sieve_fit_set_usrmr(fit, usrmr)
  record$methods <- unique(c(
    record$methods,
    sieve_spec_methods(record$spec, record$estimates, fitted = TRUE)
  ))
  analyses[[id]] <- record
  state$analyses <- analyses
  invisible(TRUE)
}

#' Can a family be shown or compared as it stands?
#'
#' @param family One family of a `sieve_fit_families()` object.
#' @return `TRUE` when the family is available, reports both CFI and RMSEA,
#'   and describes the whole model.
#' @author Ken Kelley
#' @noRd
sieve_family_usable <- function(family) {
  isTRUE(family$available) && isTRUE(family$complete) && identical(family$scope, "model")
}

#' The family of fit indices shown for one model
#'
#' The rule, in order: robust, then scaled, then standard, taking the first
#' family that is reported for the model (CFI and RMSEA both finite) and
#' describes the whole model. One family is used for every index shown:
#' CFI is never taken from one family and RMSEA from another.
#'
#' Robust is first for both estimators the package uses by default. With
#' robust maximum likelihood (MLR) the scaled versions put the scaled test
#' statistic into the usual formulas and do not converge to the population
#' values of the indices under nonnormal data, while the robust versions do
#' (Brosseau-Liard, Savalei & Li, 2012; Brosseau-Liard & Savalei, 2014;
#' Savalei, 2018; with incomplete data, Zhang & Savalei, 2023). With WLSMV
#' on ordered indicators the robust versions estimate the values the indices
#' would take under normal theory maximum likelihood on the underlying
#' continuous responses, which is what the usual reference values refer to
#' (Savalei, 2021). lavaan's documentation for `fitMeasures()` recommends
#' the robust versions over the scaled ones whenever they are available.
#' They are not always available: lavaan returns none for a model that mixes
#' ordered and continuous indicators or whose polychoric correlation matrix
#' is not positive definite, and for a model with ordered indicators and
#' constraints on thresholds or intercepts they describe the correlation
#' structure only (see `sieve_robust_scope()`). The scaled family is then
#' shown, under its own name, with the reason.
#'
#' @param x A `sieve_fit_families()` object.
#' @return A list with `family` (`"robust"`, `"scaled"`, `"standard"`, or
#'   `NA` when no family reports anything) and `passed_over` (a named
#'   character vector: the reason each family ahead of the chosen one was
#'   not used).
#' @author Ken Kelley
#' @noRd
sieve_primary_family <- function(x) {
  order <- c("robust", "scaled", "standard")
  why <- function(names) {
    vapply(names, function(f) {
      x$families[[f]]$reason %||% "not reported for this model"
    }, character(1))
  }
  usable <- vapply(order, function(f) sieve_family_usable(x$families[[f]]), logical(1))
  if (any(usable)) {
    first <- which(usable)[1]
    return(list(family = order[first], passed_over = why(order[seq_len(first - 1L)])))
  }
  whole <- vapply(order, function(f) {
    isTRUE(x$families[[f]]$available) && identical(x$families[[f]]$scope, "model")
  }, logical(1))
  some <- vapply(order, function(f) isTRUE(x$families[[f]]$available), logical(1))
  first <- if (any(whole)) which(whole)[1] else if (any(some)) which(some)[1] else NA_integer_
  if (is.na(first)) {
    return(list(family = NA_character_, passed_over = why(order)))
  }
  list(family = order[first], passed_over = why(order[seq_len(first - 1L)]))
}

#' The family of the model test that accompanies a family of indices
#'
#' The scaled test statistic is the model test whenever the fit has one (it
#' is the statistic the estimator's inference rests on, and the one the
#' scaled and the robust indices are built from); otherwise, or when the
#' standard family was asked for, the standard statistic.
#'
#' @param x A `sieve_fit_families()` object.
#' @param family The family of the indices.
#' @return `"scaled"` or `"standard"`.
#' @author Ken Kelley
#' @noRd
sieve_test_family <- function(x, family) {
  if (identical(family, "standard")) {
    return("standard")
  }
  if (is.finite(x$families$scaled$values[["chisq"]])) "scaled" else "standard"
}

#' One family of fit measures, as a flat named vector
#'
#' The single place a family is selected for display. With `family = NULL`
#' the rule of `sieve_primary_family()` is applied; a named family is
#' returned as it is (with `NA` where it reports nothing). The result is
#' named by measure (`chisq`, `df`, `pvalue`, `cfi`, `tli`, `rmsea`,
#' `rmsea_lo`, `rmsea_hi`, `srmr`, `aic`, `bic`) whatever the family, so code
#' reads `m["cfi"]` from any family, and it says which family the values are
#' in through its attributes.
#'
#' @param x A `sieve_fit_families()` object.
#' @param family `NULL` for the rule, or `"robust"`, `"scaled"`, or
#'   `"standard"`.
#' @return A named numeric vector at full precision, with `usrmr`,
#'   `usrmr_lo`, and `usrmr_hi` (the unbiased SRMR and its interval) after
#'   `bic` when the families hold them, and the attributes
#'   `status`, `family` (of CFI, TLI, and RMSEA), `test_family` (of the chi
#'   square and its p-value), `passed_over` (why each family ahead of the
#'   chosen one was not used), and `families` (the full object).
#' @author Ken Kelley
#' @noRd
sieve_fit_select <- function(x, family = NULL) {
  chosen <- if (is.null(family)) {
    sieve_primary_family(x)
  } else {
    list(
      family = match.arg(family, c("robust", "scaled", "standard")),
      passed_over = character(0)
    )
  }
  index <- function(name) {
    if (is.na(chosen$family)) NA_real_ else x$families[[chosen$family]]$values[[name]]
  }
  test_family <- sieve_test_family(x, chosen$family)
  test <- x$families[[test_family]]$values
  out <- c(
    npar = x$npar,
    chisq = test[["chisq"]],
    df = if (is.finite(test[["df"]])) test[["df"]] else x$df,
    pvalue = test[["pvalue"]],
    cfi = index("cfi"), tli = index("tli"), rmsea = index("rmsea"),
    rmsea_lo = index("rmsea_lo"), rmsea_hi = index("rmsea_hi"),
    srmr = x$srmr$value, aic = x$aic$value, bic = x$bic$value,
    if (!is.null(x$usrmr)) c(usrmr = x$usrmr$value, usrmr_lo = x$usrmr$lower, usrmr_hi = x$usrmr$upper)
  )
  attr(out, "status") <- x$status
  attr(out, "family") <- chosen$family
  attr(out, "test_family") <- test_family
  attr(out, "passed_over") <- chosen$passed_over
  attr(out, "families") <- x
  out
}

#' The corrected test statistics among a fit's lavaan tests
#'
#' lavaan lists the standard chi square first and any corrected (scaled,
#' or scaled and shifted) statistic after it, the one its `.scaled` fit
#' measures come from. It also lists Browne's residual-based statistics by
#' default: the normal-theory one, `"browne.residual.nt.model"`, for an ML
#' fit, and the asymptotically
#' distribution-free one, `"browne.residual.adf"`, beside the Yuan-Bentler
#' statistic of a weighted MLR fit (ahead of it in the list). Browne's
#' residual statistics are alternative tests of the model, computed from the
#' residuals, not corrections of the standard statistic, so they are set
#' aside with it; reading one as a scaled test would take an ML fit for a
#' robust one, or name the wrong statistic for a scaled chi square. Since
#' SIEVER reports none of them, its own ML fits with FIML ask lavaan for the
#' standard test alone (`sieve_spec_default_test()`); fits made elsewhere
#' (a fit supplied to a function, a saved one) may still list them.
#'
#' @param test The `test` option of a fit (`lavaan::lavInspect(fit,
#'   "options")$test`).
#' @return The corrected statistics, in lavaan's order (`character(0)` for
#'   a fit without one).
#' @author Ken Kelley
#' @noRd
sieve_lavaan_corrected_tests <- function(test) {
  test <- as.character(test %||% character(0))
  test[!is.na(test) & !test %in% c("standard", "none") & !startsWith(test, "browne.residual")]
}

#' The family a set of selected fit measures is in
#'
#' @param m A result of `sieve_fit_measures()` or `sieve_fit_select()`.
#' @param what `"index"` for CFI, TLI, and RMSEA, or `"test"` for the chi
#'   square and its p-value.
#' @return `"robust"`, `"scaled"`, `"standard"`, or `NA`.
#' @author Ken Kelley
#' @noRd
sieve_fit_family <- function(m, what = c("index", "test")) {
  what <- match.arg(what)
  family <- attr(m, if (identical(what, "index")) "family" else "test_family")
  if (is.null(family)) NA_character_ else family
}

#' The label of a fit measure, with its family
#'
#' Every displayed index says which family it is in: `"CFI (robust)"`,
#' `"Model chi square (scaled)"`. Measures that do not come in families
#' (degrees of freedom, SRMR, AIC, BIC) are returned without one. The RMSEA
#' interval names the level it was computed at, read from the measures
#' (`"RMSEA 90% CI lower (robust)"`; see `sieve_fit_ci_level()`).
#'
#' @param m A result of `sieve_fit_measures()` or `sieve_fit_select()`.
#' @param index One or more of `npar`, `chisq`, `df`, `pvalue`, `cfi`, `tli`,
#'   `rmsea`, `rmsea_lo`, `rmsea_hi`, `rmsea_ci`, `srmr`, `aic`, `bic`,
#'   `usrmr`, `usrmr_lo`, `usrmr_hi` (the unbiased SRMR and its limits,
#'   labeled with the level of their interval).
#' @return A character vector of labels.
#' @author Ken Kelley
#' @noRd
sieve_fit_label <- function(m, index) {
  ci <- sieve_rmsea_ci_words(sieve_fit_ci_level(m))
  base <- c(
    npar = "Free parameters", chisq = "Model chi square", df = "df", pvalue = "p-value",
    cfi = "CFI", tli = "TLI", rmsea = "RMSEA", rmsea_lo = sprintf("RMSEA %s lower", ci),
    rmsea_hi = sprintf("RMSEA %s upper", ci), rmsea_ci = sprintf("RMSEA, %s", ci), srmr = "SRMR",
    aic = "AIC", bic = "BIC", usrmr = "Unbiased SRMR",
    usrmr_lo = sprintf("Unbiased SRMR %s lower", sieve_rmsea_ci_words(attr(m, "families")$usrmr$conf_level %||% NA_real_)),
    usrmr_hi = sprintf("Unbiased SRMR %s upper", sieve_rmsea_ci_words(attr(m, "families")$usrmr$conf_level %||% NA_real_))
  )
  # An RMSEA interval is labeled by its own family when the measures say
  # it differs from the index's (a psychonetrics fit under DWLS has a
  # scaled RMSEA and only the standard interval).
  ci_family <- attr(m, "rmsea_ci_family") %||% sieve_fit_family(m, "index")
  family <- ifelse(
    index %in% c("chisq", "pvalue"), sieve_fit_family(m, "test"),
    ifelse(index %in% c("rmsea_lo", "rmsea_hi", "rmsea_ci"), ci_family,
      ifelse(index %in% c("cfi", "tli", "rmsea"), sieve_fit_family(m, "index"), "")
    )
  )
  label <- unname(base[index])
  ifelse(is.na(family) | !nzchar(family), label, sprintf("%s (%s)", label, family))
}

#' A table of fit measures, each named with its family
#'
#' The rows a fit table shows, built in one place so that every panel
#' labels a value with the family it is in.
#'
#' @param m A result of `sieve_fit_measures()` or `sieve_fit_select()`.
#' @param index The measures to list, in order (see `sieve_fit_label()`).
#'   By default every measure, with the unbiased SRMR and its limits after
#'   the SRMR when the measures hold them.
#' @return A data frame with `Statistic` (the label, with the family) and
#'   `Value` (at full precision).
#' @author Ken Kelley
#' @noRd
sieve_fit_table <- function(m, index = NULL) {
  index <- index %||% c(
    "npar", "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea_lo", "rmsea_hi", "srmr",
    if ("usrmr" %in% names(m)) c("usrmr", "usrmr_lo", "usrmr_hi"), "aic", "bic"
  )
  data.frame(
    Statistic = sieve_fit_label(m, index), Value = unname(m[index]),
    stringsAsFactors = FALSE
  )
}

#' What each family is, in a sentence
#'
#' Under ML with FIML (the default for continuous items) lavaan reports the
#' standard model
#' test and no scaled one, and its robust CFI, TLI, and RMSEA are the
#' incomplete data corrections of Zhang and Savalei (2023), not corrections
#' for nonnormality (with no value missing they equal the standard ones);
#' `normal_theory = TRUE` says so.
#'
#' @param family `"robust"`, `"scaled"`, or `"standard"`.
#' @param categorical `TRUE` for a model with ordered indicators.
#' @param normal_theory `TRUE` for a fit with no corrected (scaled) test
#'   statistic, such as ML with FIML.
#' @return One or two sentences with their references.
#' @author Ken Kelley
#' @noRd
sieve_family_text <- function(family, categorical = FALSE, normal_theory = FALSE) {
  switch(family,
    robust = if (isTRUE(categorical)) {
      paste0(
        "The robust versions (lavaan's .robust values) estimate, from the polychoric ",
        "correlations, the values CFI, TLI, and RMSEA would take under normal theory maximum ",
        "likelihood on the underlying continuous responses, which is what the usual ",
        "reference values refer to (Savalei, 2021)."
      )
    } else if (isTRUE(normal_theory)) {
      paste0(
        "Under ML with FIML the robust versions (lavaan's .robust values) are corrected for the ",
        "incomplete data only (Zhang & Savalei, 2023), not for nonnormality: they estimate the ",
        "population values the complete data CFI, TLI, and RMSEA estimate, and they equal the ",
        "standard ones when no value is missing. For markedly nonnormal items, MLR with FIML (the ",
        "Estimator control) adds the correction for nonnormality."
      )
    } else {
      paste0(
        "The robust versions (lavaan's .robust values) are corrected so that with nonnormal ",
        "or incomplete data they estimate the same population values as the normal theory ",
        "CFI, TLI, and RMSEA (Brosseau-Liard, Savalei & Li, 2012; Brosseau-Liard & Savalei, ",
        "2014; Zhang & Savalei, 2023)."
      )
    },
    scaled = paste0(
      "The scaled versions (lavaan's .scaled values) put the scaled test statistic into the ",
      "usual formulas. Many programs print them, but they do not estimate the population ",
      "values of the indices (Brosseau-Liard & Savalei, 2014; Savalei, 2018, 2021)."
    ),
    standard = "The standard versions are computed from the uncorrected test statistic.",
    ""
  )
}

#' Which family is shown, and why, in words
#'
#' The sentences a display puts beside its fit indices: the family shown,
#' what it is, the reason any family ahead of it in the rule was not used,
#' and which test statistic the model chi square is.
#'
#' @param m A result of `sieve_fit_measures()` or `sieve_fit_select()`.
#' @return A character vector of sentences (empty when `m` carries no
#'   family information).
#' @author Ken Kelley
#' @noRd
sieve_fit_note <- function(m) {
  x <- attr(m, "families")
  family <- sieve_fit_family(m, "index")
  if (is.null(x)) {
    return(character(0))
  }
  if (is.na(family)) {
    return(sprintf("No fit index is reported for this model. %s", x$families$standard$reason))
  }
  passed <- attr(m, "passed_over") %||% character(0)
  test_family <- sieve_fit_family(m, "test")
  scaled_test <- sieve_lavaan_corrected_tests(x$test)
  c(
    sprintf(
      "CFI, TLI, and RMSEA are shown in the %s family. %s", family,
      sieve_family_text(family, x$categorical, normal_theory = !length(scaled_test))
    ),
    if (length(passed)) {
      sprintf(
        "The %s family is not shown: %s", names(passed),
        paste0(tolower(substring(passed, 1L, 1L)), substring(passed, 2L))
      )
    },
    sprintf(
      "The model chi square is the %s statistic%s.", test_family,
      if (identical(test_family, "scaled") && length(scaled_test) && !is.na(scaled_test[1])) {
        sprintf(" (lavaan test \"%s\")", scaled_test[1])
      } else {
        ""
      }
    ),
    sieve_srmr_note(x$srmr),
    sieve_usrmr_note(x$usrmr)
  )
}

#' What the SRMR Beside the Indices Is, in Words
#'
#' @param srmr The `srmr` element of a `sieve_fit_families()` object.
#' @return A sentence, with a second one when the fit has auxiliary
#'   variables (they are left out of the SRMR when the value is DMAR's, and
#'   averaged in otherwise: DMAR returned none, or a value no different from
#'   lavaan's with them averaged in), or `NULL` when no SRMR is reported or
#'   the record does not name the measure `srmr_bentler_nomean`.
#' @author Ken Kelley
#' @noRd
sieve_srmr_note <- function(srmr) {
  if (!isTRUE(srmr$available) || !identical(srmr$measure, "srmr_bentler_nomean")) {
    return(NULL)
  }
  differs <- is.finite(srmr$lavaan) && abs(srmr$lavaan - srmr$value) > 5e-4
  aux <- srmr$aux %||% character(0)
  one <- length(aux) == 1L
  # With auxiliary variables the value is DMAR's own computation, of the
  # model's variables, not the lavaan measure of that name.
  set_aside <- length(aux) > 0L && identical(srmr$source, "DMAR") &&
    is.finite(srmr$with_aux %||% NA_real_) && abs(srmr$with_aux - srmr$value) > 1e-10
  c(
    paste0(
      "The SRMR is of the covariance residuals alone, the SRMR DMAR reports",
      if (set_aside) "" else " (lavaan's srmr_bentler_nomean)",
      if (differs) {
        sprintf(
          "; lavaan's own srmr, which also averages in the mean residuals of a model with a mean structure, is %.3f for this fit.",
          srmr$lavaan
        )
      } else {
        "."
      }
    ),
    if (set_aside) {
      sprintf(
        paste0(
          "This fit has %d auxiliary variable%s, which %s left out of this SRMR, as DMAR leaves %s out: it is of the ",
          "model's own variables. lavaan averages %s variances and covariances in, with residuals of zero or nearly so, ",
          "and its srmr_bentler_nomean is %.3f."
        ),
        length(aux), if (one) "" else "s", if (one) "is" else "are", if (one) "it" else "them",
        if (one) "its" else "their", srmr$with_aux
      )
    } else if (length(aux)) {
      sprintf(
        paste0(
          "This fit has %d auxiliary variable%s, whose variances and covariances are averaged into this SRMR with ",
          "residuals of zero or nearly so: the SRMR of the same model without %s is larger."
        ),
        length(aux), if (one) "" else "s", if (one) "it" else "them"
      )
    }
  )
}

#' Every family of a fitted model, side by side
#'
#' For the display that lets the reader see all three families at once
#' rather than only the one the rule selects.
#'
#' @param m A result of `sieve_fit_measures()`, `sieve_fit_select()`, or
#'   `sieve_fit_families()`.
#' @return A data frame with one row per measure and the columns `measure`,
#'   `standard`, `scaled`, and `robust` at full precision (`NA` where a
#'   family reports no value). The attribute `reasons` is a character vector
#'   that says why a family or one of its values is not reported or
#'   describes less than the whole model, and why SRMR, AIC, or BIC is not
#'   reported when one of them is not.
#' @author Ken Kelley
#' @noRd
sieve_fit_family_table <- function(m) {
  x <- if (inherits(m, "sieve_fit_families")) m else attr(m, "families")
  if (is.null(x)) {
    return(NULL)
  }
  ci <- sieve_rmsea_ci_words(sieve_fit_ci_level(x))
  rows <- c(
    chisq = "Model chi square", df = "df", pvalue = "p-value", cfi = "CFI",
    tli = "TLI", rmsea = "RMSEA", rmsea_lo = sprintf("RMSEA %s lower", ci),
    rmsea_hi = sprintf("RMSEA %s upper", ci)
  )
  out <- data.frame(
    measure = unname(rows),
    standard = unname(x$families$standard$values[names(rows)]),
    scaled = unname(x$families$scaled$values[names(rows)]),
    robust = unname(x$families$robust$values[names(rows)]),
    stringsAsFactors = FALSE
  )
  reasons <- unlist(lapply(c("standard", "scaled", "robust"), function(f) {
    fam <- x$families[[f]]
    if (!isTRUE(fam$available) || !identical(fam$scope, "model")) {
      return(sprintf("%s: %s", f, fam$reason))
    }
    absent <- fam$missing[!is.na(fam$missing)]
    if (!length(absent)) {
      return(NULL)
    }
    by_reason <- split(unname(rows[names(absent)]), unname(absent))
    vapply(names(by_reason), function(r) {
      sprintf("%s, %s: %s", f, paste(by_reason[[r]], collapse = ", "), r)
    }, character(1))
  }))
  other <- c(srmr = "SRMR", aic = "AIC", bic = "BIC", if (!is.null(x$usrmr)) c(usrmr = "Unbiased SRMR"))
  absent <- names(other)[!vapply(names(other), function(n) isTRUE(x[[n]]$available), logical(1))]
  if (length(absent)) {
    by_reason <- split(
      unname(other[absent]),
      vapply(absent, function(n) x[[n]]$reason, character(1))
    )
    reasons <- c(reasons, vapply(names(by_reason), function(r) {
      sprintf("%s: %s", paste(by_reason[[r]], collapse = ", "), r)
    }, character(1)))
  }
  attr(out, "reasons") <- unname(reasons)
  out
}

#' The family of fit indices a comparison of models uses
#'
#' A change in CFI or RMSEA is interpretable only as a difference of two
#' values of the same statistic. The family is therefore chosen once for
#' the whole comparison: the first of robust, scaled, and standard that
#' every model with a solution reports (CFI and RMSEA both finite) and that
#' describes the whole model in every one of them. A family that is missing
#' for one model is not used for any of them; there is no fallback model by
#' model.
#'
#' The second condition matters for ordered indicators. lavaan's robust
#' indices leave out thresholds, intercepts, and the constraints on them
#' (see `sieve_robust_scope()`), so for an invariance ladder on ordered
#' items they cannot register a violation of threshold or scalar invariance,
#' and the scaled family, which is built on the same statistic as the
#' scaled difference test, is used instead.
#'
#' @param measures A list of `sieve_fit_families()` objects, one per model.
#' @param prefer A family to use when it is eligible (for example the
#'   family an earlier comparison of the same models used), or `NULL`.
#' @return A list with `family` (`NA` when no model has a solution or no
#'   family is eligible), `eligible` (a named logical vector over robust,
#'   scaled, standard), `why_not` (a named character vector: the reason
#'   each family that is not eligible was passed over), `solved` (which
#'   models have a solution), and `reason` (a sentence stating the choice).
#' @author Ken Kelley
#' @noRd
sieve_common_family <- function(measures, prefer = NULL) {
  order <- c("robust", "scaled", "standard")
  labels <- names(measures) %||% paste("model", seq_along(measures))
  solved <- vapply(measures, function(m) {
    isTRUE(m$status %in% c("ok", "inadmissible")) && is.null(m$engine_error)
  }, logical(1))
  out <- list(
    family = NA_character_,
    eligible = stats::setNames(rep(FALSE, length(order)), order),
    why_not = character(0), solved = solved, reason = NA_character_
  )
  if (!any(solved)) {
    out$reason <- "No model in the comparison has a solution, so there are no indices to compare."
    return(out)
  }
  # A comparison of many models (the candidates of a search) names the first
  # few and counts the rest.
  quote_names <- function(x) {
    shown <- paste(sprintf("'%s'", utils::head(x, 4L)), collapse = ", ")
    if (length(x) > 4L) sprintf("%s, and %d more", shown, length(x) - 4L) else shown
  }
  check <- function(family) {
    fams <- lapply(measures[solved], function(m) m$families[[family]])
    reported <- vapply(fams, function(f) isTRUE(f$available) && isTRUE(f$complete), logical(1))
    whole <- vapply(fams, function(f) identical(f$scope, "model"), logical(1))
    if (all(reported) && all(whole)) {
      return(NA_character_)
    }
    if (!all(reported)) {
      first <- which(!reported)[1]
      return(sprintf(
        "it is not reported for %s (%s)", quote_names(labels[solved][!reported]),
        sub("[.]$", "", fams[[first]]$reason %||% "no reason recorded")
      ))
    }
    sprintf(
      paste0(
        "for %s it leaves out thresholds, intercepts, and the equality constraints on them, ",
        "so it cannot register a violation of those constraints"
      ),
      quote_names(labels[solved][!whole])
    )
  }
  problems <- vapply(order, check, character(1))
  out$eligible <- stats::setNames(is.na(problems), order)
  out$why_not <- problems[!is.na(problems)]
  if (!any(out$eligible)) {
    out$reason <- paste0(
      "No family of fit indices is reported for every model in the comparison, so no ",
      "change is computed."
    )
    return(out)
  }
  out$family <- if (!is.null(prefer) && isTRUE(out$eligible[prefer])) {
    prefer
  } else {
    order[which(out$eligible)[1]]
  }
  ahead <- out$why_not[names(out$why_not) %in% order[seq_len(match(out$family, order) - 1L)]]
  out$reason <- paste0(
    sprintf(paste0(
      "CFI and RMSEA are in the %s family for every model, so a difference ",
      "between two models compares like with like."
    ), out$family),
    if (length(ahead)) {
      paste0(" ", paste(sprintf("The %s family is not used: %s.", names(ahead), ahead),
        collapse = " "
      ))
    } else {
      ""
    }
  )
  out
}

#' Select an EFA display family without letting zero-df rows veto usable models
#' @param measures Fit families for the requested factor counts.
#' @return A common family, its explanation, and the full solved-row mask.
#' @author Ken Kelley
#' @noRd
sieve_efa_common_family <- function(measures) {
  all <- sieve_common_family(measures)
  positive <- all$solved & vapply(measures, function(m) {
    length(m$df) == 1L && is.finite(m$df) && m$df > 0
  }, logical(1))
  if (any(positive)) {
    chosen <- sieve_common_family(measures[positive])
    reason <- chosen$reason
    if (any(all$solved & !positive)) {
      reason <- paste("For the solved models with known positive degrees of freedom:", reason,
        "Other solved rows are descriptive; they do not choose the family or support a fit-based factor count.")
    }
    return(list(family = chosen$family, reason = reason, solved = all$solved))
  }
  family <- all$family
  if (is.na(family) && any(all$solved)) {
    family <- sieve_primary_family(measures[[which(all$solved)[1L]]])$family
  }
  list(family = family, solved = all$solved, reason = paste(
    "No solved model has known positive degrees of freedom.",
    if (is.na(family)) "No family of indices is available." else
      sprintf("Available indices in the %s family are descriptive; no fit-based factor count is evaluated.", family)))
}

#' The engine's chi square difference test of two nested models
#'
#' The test is lavaan's own (`lavaan::lavTestLRT()`), with the method lavaan
#' selects for the test statistic the models were fit with: the Satorra and
#' Bentler (2001) scaled difference for a scaled statistic (MLR), Satorra's
#' (2000) scaled and shifted difference for a mean and variance adjusted
#' statistic (WLSMV), and the ordinary difference for the standard
#' statistic. A scaled difference is a function of the two standard
#' statistics and their scaling factors; it is never the difference of two
#' scaled statistics.
#'
#' The Satorra and Bentler (2001) scaling factor of a difference can be
#' negative, in which case the statistic is undefined. The strictly positive
#' version of Satorra and Bentler (2010) is then computed instead, and the
#' label and the note say so. When neither can be computed the test is
#' reported as unavailable with the reason.
#'
#' A difference test is defined for nested models. Two conditions that show
#' the models handed in are not nested, or not in the stated order, are
#' checked, and no statistic is reported when either holds: the second model
#' has fewer degrees of freedom than the first (lavaan would reorder the two
#' and test them all the same), or the second model fits better than the
#' first on the standard statistic (which no model nested in another can do
#' once both are at their optimum). The second check uses
#' the standard statistics and lavaan's own tolerance, because a scaled and
#' shifted difference can be positive when the standard statistics are not
#' ordered. A negative difference is never turned into a p-value of one.
#'
#' @param less_restricted,more_restricted Fitted lavaan objects, the second
#'   nested in the first.
#' @return A list with `available`, `statistic`, `df`, `p`, `method` (the
#'   lavaan method name, or `"standard"`), `scaled` (whether the test is a
#'   scaled one), `label`, `note` (a caution, or `NA`), `reason` (why the
#'   test is not available, or `NA`), `nested` (`FALSE` when the two models
#'   were shown not to be nested in the stated order, otherwise `NA`: nesting
#'   is not proved by this function), `standard_difference` (the standard
#'   chi square of the second model minus that of the first), and `warnings`.
#' @author Ken Kelley
#' @noRd
sieve_nested_test <- function(less_restricted, more_restricted) {
  out <- list(
    available = FALSE, statistic = NA_real_, df = NA_real_, p = NA_real_,
    method = NA_character_, scaled = NA, label = NA_character_,
    note = NA_character_, reason = NA_character_, nested = NA,
    standard_difference = NA_real_, warnings = character(0)
  )
  if (is.null(less_restricted) || is.null(more_restricted)) {
    out$reason <- "One of the two models has no fit, so there is nothing to test."
    return(out)
  }
  read_fit <- function(name) {
    vapply(list(less_restricted, more_restricted), function(f) {
      tryCatch(unname(lavaan::fitMeasures(f, name)), error = function(e) NA_real_)
    }, numeric(1))
  }
  standard <- read_fit("chisq")
  freedom <- read_fit("df")
  out$standard_difference <- standard[2L] - standard[1L]
  # lavaan orders the two models by their degrees of freedom itself, so a
  # pair handed in the wrong order is caught here, before it is compared.
  if (all(is.finite(freedom)) && freedom[2L] < freedom[1L]) {
    out$nested <- FALSE
    out$df <- freedom[2L] - freedom[1L]
    out$reason <- paste0(
      "The second model has fewer degrees of freedom than the first, so it is not the more ",
      "restricted of the two and no difference test is reported."
    )
    return(out)
  }
  read <- function(run) {
    tab <- run$value
    if (!run$ok || is.null(tab) || nrow(tab) < 2L) {
      return(NULL)
    }
    heading <- gsub("[\r\n]+", " ", paste(attr(tab, "heading"), collapse = " "))
    pattern <- "method[[:space:]]*=[[:space:]]*[\"\u201c]([^\"\u201d]+)[\"\u201d]"
    scaled <- grepl("Scaled|Adjusted", heading)
    method <- if (grepl(pattern, heading)) {
      sub(paste0(".*", pattern, ".*"), "\\1", heading)
    } else if (scaled) {
      NA_character_
    } else {
      "standard"
    }
    list(
      statistic = unname(tab[2L, "Chisq diff"]), df = unname(tab[2L, "Df diff"]),
      p = unname(tab[2L, "Pr(>Chisq)"]), scaled = scaled, method = method
    )
  }
  label_of <- function(got) {
    if (!isTRUE(got$scaled)) {
      "standard chi square difference test"
    } else if (is.na(got$method)) {
      "scaled chi square difference test"
    } else {
      sprintf("scaled chi square difference test (method = \"%s\")", got$method)
    }
  }
  run <- sieve_capture(lavaan::lavTestLRT(less_restricted, more_restricted))
  out$warnings <- run$warnings
  got <- read(run)
  if (is.null(got)) {
    out$reason <- sprintf(
      "lavaan could not compare the two models (%s).",
      sub("[.]$", "", gsub("\\s+", " ", run$error %||% "no reason given"))
    )
    return(out)
  }
  out$df <- got$df
  if (is.finite(got$df) && got$df <= 0) {
    out$reason <- paste0(
      "The added constraints cost no degrees of freedom, so there is nothing to test ",
      "between these two models."
    )
    return(out)
  }
  if (is.finite(out$standard_difference) &&
    out$standard_difference < -1 * .Machine$double.eps^(1 / 3)) {
    out$nested <- FALSE
    out$reason <- sprintf(
      paste0(
        "The more restricted model fits better than the less restricted one (its standard ",
        "chi square is lower by %.3f), which two nested models at their optimum cannot do. ",
        "Either the models are not nested or the less restricted one stopped short of its ",
        "optimum, so no difference test is reported, and the changes in CFI and RMSEA are ",
        "not the cost of the added constraints."
      ),
      -out$standard_difference
    )
    return(out)
  }
  negative <- any(grepl("scaling factor is negative", run$warnings, fixed = TRUE))
  if (negative || !is.finite(got$statistic)) {
    if (negative) {
      again <- sieve_capture(lavaan::lavTestLRT(less_restricted, more_restricted,
        method = "satorra.bentler.2010"
      ))
      retry <- read(again)
      out$warnings <- unique(c(out$warnings, again$warnings))
      if (!is.null(retry) && is.finite(retry$statistic) && isTRUE(retry$scaled)) {
        out$available <- TRUE
        out$statistic <- retry$statistic
        out$df <- retry$df
        out$p <- retry$p
        out$method <- retry$method
        out$scaled <- TRUE
        out$label <- label_of(retry)
        out$note <- paste0(
          "The scaling factor of the Satorra and Bentler (2001) difference test is negative ",
          "for these two models, so that statistic is undefined; the strictly positive ",
          "version (Satorra & Bentler, 2010) is reported instead."
        )
        return(out)
      }
      out$reason <- paste0(
        "The scaling factor of the scaled difference test is negative for these two models ",
        "and the strictly positive version (Satorra & Bentler, 2010) could not be computed ",
        "either, so no scaled difference test is reported."
      )
      return(out)
    }
    out$reason <- "lavaan returned no difference statistic for these two models."
    return(out)
  }
  out$available <- TRUE
  out$statistic <- got$statistic
  out$p <- got$p
  out$method <- got$method
  out$scaled <- got$scaled
  out$label <- label_of(got)
  out
}

#' Compare a Sequence of Fitted Models Within One Family of Fit Indices
#'
#' Builds the comparison table for a sequence of models ordered from least
#' to most restricted (an invariance ladder, a model and its partially
#' constrained version, a model with and without a method factor). The
#' family of CFI and RMSEA is chosen once for all the models by
#' `sieve_common_family()`, and every change is a difference of two values
#' from that family, computed between a model and the one before it. The
#' chi square difference tests use lavaan's applicable scaled or standard
#' test, with nesting and fit-status checks before any statistic is reported.
#'
#' A model that failed, did not converge, or is not identified has no
#' indices: its row says why, and no change or test is reported for it or
#' for the model that would be compared with it.
#'
#' @param fits A list of fitted lavaan objects (or `NULL` for a model with
#'   no fit), from least to most restricted. Its names label the models.
#' @param status A list of statuses in the same order (see
#'   `sieve_fit_status()`); read from the fits when `NULL`.
#' @param prefer A family to use when it is eligible, or `NULL`.
#' @param nested `FALSE` skips the chi square difference tests.
#' @param conf_level The confidence level of the RMSEA intervals (`rmsea_lo`
#'   and `rmsea_hi`); 0.90 by default (`sieve_default_rmsea_conf_level()`,
#'   DMAR's default for the RMSEA interval and lavaan's). The changes, the
#'   tests, and the indices themselves do not depend on it.
#' @return A list of class `sieve_fit_comparison` with `family` and
#'   `test_family` (the family of the indices and of the chi square shown
#'   for each model), `common` (the full result of
#'   `sieve_common_family()`), `table` (one row per model: `model`,
#'   `status`, `chisq`, `df`, `pvalue`, `cfi`, `rmsea`, `rmsea_lo`,
#'   `rmsea_hi`, `delta_chisq`, `delta_df`, `delta_p`, `delta_cfi`,
#'   `delta_rmsea`, and `note`, the reason any value in the row is not
#'   reported), `by_family` (for each of the three families a data frame
#'   with `model`, `cfi`, `tli`, `rmsea`, `rmsea_lo`, `rmsea_hi`,
#'   `delta_cfi`, `delta_rmsea`, `scope`, and `reason`), `tests` (the
#'   difference test of each model against the one before it), `test_label`
#'   (the tests used), `measures` (the `sieve_fit_families()` object of each
#'   model), `conf_level` (the level of the RMSEA intervals), and
#'   `warnings`. Everything is at full precision. The helper
#'   descriptions refer to internal result structures; callers normally
#'   supply fitted lavaan models and let this function resolve them.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' fits <- list(
#'   correlated = sieve_fit_cfa(items, meta),
#'   orthogonal = sieve_fit_cfa(items, meta, orthogonal = TRUE)
#' )
#' compared <- sieve_fit_compare(fits)
#' compared$family
#' compared$table[, c("model", "chisq", "df", "cfi", "rmsea", "delta_chisq", "delta_df", "delta_p")]
#' @export
sieve_fit_compare <- function(fits, status = NULL, prefer = NULL, nested = TRUE,
                              conf_level = sieve_default_rmsea_conf_level()) {
  conf_level <- sieve_check_conf_level(conf_level)
  k <- length(fits)
  labels <- names(fits) %||% paste("model", seq_len(k))
  measures <- stats::setNames(lapply(seq_len(k), function(i) {
    sieve_fit_families(fits[[i]], status = if (is.null(status)) NULL else status[[i]],
      conf_level = conf_level)
  }), labels)
  common <- sieve_common_family(measures, prefer = prefer)
  solved <- common$solved
  family <- common$family
  # The model test shown for each model is one statistic for all of them:
  # the scaled one when every model with a solution has it.
  has_scaled <- vapply(
    measures, function(m) is.finite(m$families$scaled$values[["chisq"]]),
    logical(1)
  )
  test_family <- if (identical(family, "standard") || !any(solved) || !all(has_scaled[solved])) {
    "standard"
  } else {
    "scaled"
  }
  value <- function(fam, index) {
    vapply(measures, function(m) {
      if (is.na(fam)) NA_real_ else m$families[[fam]]$values[[index]]
    }, numeric(1))
  }
  change <- function(x) {
    out <- rep(NA_real_, length(x))
    if (length(x) > 1L) {
      both <- is.finite(x[-1L]) & is.finite(x[-length(x)])
      out[-1L][both] <- (x[-1L] - x[-length(x)])[both]
    }
    out
  }
  blank_test <- function(reason) {
    list(
      available = FALSE, statistic = NA_real_, df = NA_real_, p = NA_real_,
      method = NA_character_, scaled = NA, label = NA_character_,
      note = NA_character_, reason = reason, nested = NA,
      standard_difference = NA_real_, warnings = character(0)
    )
  }
  tests <- stats::setNames(lapply(seq_len(k), function(i) {
    if (i == 1L) {
      return(blank_test(NA_character_))
    }
    if (!isTRUE(nested)) {
      return(blank_test(NA_character_))
    }
    if (!solved[i] || !solved[i - 1L]) {
      return(blank_test(sprintf(
        "No comparison with '%s': %s", labels[i - 1L],
        if (!solved[i]) "this model has no solution." else "that model has no solution."
      )))
    }
    sieve_nested_test(fits[[i - 1L]], fits[[i]])
  }), labels)
  cfi <- value(family, "cfi")
  rmsea <- value(family, "rmsea")
  note <- vapply(seq_len(k), function(i) {
    parts <- c(
      if (!solved[i]) sprintf("No fit indices: %s", measures[[i]]$families$standard$reason),
      if (i > 1L && solved[i] && !solved[i - 1L]) {
        sprintf("No change from '%s', which has no solution.", labels[i - 1L])
      },
      if (i > 1L && solved[i] && solved[i - 1L] && isTRUE(nested) && !tests[[i]]$available) {
        tests[[i]]$reason
      },
      if (!is.na(tests[[i]]$note)) tests[[i]]$note
    )
    paste(parts, collapse = " ")
  }, character(1))
  table <- data.frame(
    model = labels,
    status = vapply(measures, function(m) m$status, character(1)),
    chisq = value(test_family, "chisq"),
    df = vapply(seq_len(k), function(i) {
      d <- measures[[i]]$families[[test_family]]$values[["df"]]
      if (is.finite(d)) d else measures[[i]]$df
    }, numeric(1)),
    pvalue = value(test_family, "pvalue"),
    cfi = cfi, rmsea = rmsea,
    rmsea_lo = value(family, "rmsea_lo"), rmsea_hi = value(family, "rmsea_hi"),
    delta_chisq = vapply(tests, function(t) t$statistic, numeric(1)),
    delta_df = vapply(tests, function(t) as.numeric(t$df), numeric(1)),
    delta_p = vapply(tests, function(t) t$p, numeric(1)),
    delta_cfi = change(cfi), delta_rmsea = change(rmsea),
    note = note,
    stringsAsFactors = FALSE, row.names = NULL
  )
  by_family <- stats::setNames(lapply(c("standard", "scaled", "robust"), function(fam) {
    f_cfi <- value(fam, "cfi")
    f_rmsea <- value(fam, "rmsea")
    data.frame(
      model = labels, cfi = f_cfi, tli = value(fam, "tli"), rmsea = f_rmsea,
      rmsea_lo = value(fam, "rmsea_lo"), rmsea_hi = value(fam, "rmsea_hi"),
      delta_cfi = change(f_cfi), delta_rmsea = change(f_rmsea),
      scope = vapply(measures, function(m) m$families[[fam]]$scope, character(1)),
      reason = vapply(
        measures, function(m) m$families[[fam]]$reason %||% NA_character_,
        character(1)
      ),
      stringsAsFactors = FALSE, row.names = NULL
    )
  }), c("standard", "scaled", "robust"))
  used <- unique(stats::na.omit(vapply(tests, function(t) t$label, character(1))))
  structure(list(
    family = family, test_family = test_family, common = common, table = table,
    by_family = by_family, tests = tests,
    test_label = if (length(used)) paste(used, collapse = "; ") else NA_character_,
    measures = measures, conf_level = conf_level,
    warnings = unique(c(
      unlist(lapply(measures, function(m) m$warnings)),
      unlist(lapply(tests, function(t) t$warnings))
    ))
  ), class = "sieve_fit_comparison")
}

#' Compare a partial invariance model with the rung it is nested in
#'
#' A partial invariance model is a rung of the ladder with chosen parameters
#' left free across groups. The model it is read against is the rung before
#' it with the same parameters free (Byrne, Shavelson & Muthen, 1989): the
#' two then differ only by the constraints the rung adds, so the partial
#' model is nested in the reference, the difference test is a test of those
#' constraints, and the changes in CFI and RMSEA are their cost.
#'
#' The reference is therefore taken from the same refit as the partial model
#' (`partial_fits`, in which every rung carries the freed parameters), never
#' from the full ladder. The two coincide when no freed parameter is
#' constrained at the earlier rung (an intercept freed at the scalar rung
#' leaves the metric rung as it was). They differ when one is: a loading
#' freed at the scalar rung is still held equal in the full metric model, so
#' the full metric model is not less restricted than the partial scalar one
#' and the two are not nested. In an example with two groups of 500, six
#' items, and one loading and one intercept not invariant, comparing with
#' the full metric model gives a chi square difference of -12.3 on 4
#' degrees of freedom where the nested comparison gives 60.1 on 5.
#'
#' @param partial_fits The fitted rungs of the refit, a named list from least
#'   to most restricted, each with the freed parameters (the `fits` attribute
#'   of the DMAR result).
#' @param level The rung of interest.
#' @param partial_status The statuses of the refit's rungs (a named list), or
#'   `NULL` to read them from the fits.
#' @param ladder_fits The fitted rungs of the full ladder, or `NULL`. Used
#'   only to say whether the freed parameters change the reference rung.
#' @param prefer A family of fit indices to use when it is eligible.
#' @return A list with `error` (a string, or `NULL`), `level`, `prev` (the
#'   rung before it), `fit` and `reference` (the two fitted models),
#'   `reference_partial` (`TRUE` when the freed parameters change the
#'   reference rung, `FALSE` when it is the ladder's own rung refit, `NA`
#'   when `ladder_fits` was not given), `labels` (the names the two models
#'   carry in the comparison), and `compare` (the [sieve_fit_compare()]
#'   result for the pair).
#' @author Ken Kelley
#' @noRd
sieve_partial_compare <- function(partial_fits, level, partial_status = NULL,
                                  ladder_fits = NULL, prefer = NULL) {
  out <- list(
    error = NULL, level = level, prev = NA_character_, fit = NULL,
    reference = NULL, reference_partial = NA, labels = character(0),
    compare = NULL
  )
  rungs <- names(partial_fits)
  pos <- match(level, rungs)
  if (is.na(pos) || pos < 2L) {
    out$error <- "That rung has no less restricted rung to compare against."
    return(out)
  }
  prev <- rungs[pos - 1L]
  out$prev <- prev
  out$fit <- partial_fits[[level]]
  out$reference <- partial_fits[[prev]]
  count <- function(f) {
    if (is.null(f)) {
      return(NA_real_)
    }
    tryCatch(as.numeric(lavaan::fitMeasures(f, "df")), error = function(e) NA_real_)
  }
  if (!is.null(ladder_fits)) {
    # Freeing an equality constraint costs the model a degree of freedom, so
    # the reference rung of the refit differs from the ladder's rung exactly
    # when their degrees of freedom do. (lavaan's count of free parameters
    # does not show it: an equality constraint leaves that count as it is.)
    here <- count(out$reference)
    there <- count(ladder_fits[[prev]])
    out$reference_partial <- if (is.finite(here) && is.finite(there)) here != there else NA
  }
  out$labels <- c(
    if (isTRUE(out$reference_partial)) sprintf("partial %s", prev) else prev,
    sprintf("partial %s", level)
  )
  status <- if (is.null(partial_status)) {
    NULL
  } else {
    list(partial_status[[prev]], partial_status[[level]])
  }
  out$compare <- sieve_fit_compare(
    stats::setNames(list(out$reference, out$fit), out$labels),
    status = status, prefer = prefer
  )
  out
}

#' Fit indices of a set of models, all in the one family they share
#'
#' For models that are set side by side without being nested (the candidate
#' item sets of a search, the halves of a split): CFI, TLI, RMSEA and its
#' interval are taken from one family for every model, chosen by
#' `sieve_common_family()`. Reading each model in its own first available
#' family would put robust values beside scaled ones in one column, and a
#' sort or a ranking on that column would compare different statistics.
#'
#' @param measures A list with one element per model: a result of
#'   `sieve_fit_measures()` (which carries its families), a
#'   `sieve_fit_families()` object, or `NULL` for a model with no fit.
#' @param prefer A family to use when it is eligible, or `NULL`.
#' @return A list with `family` (`NA` when no family is reported for every
#'   model with a solution), `reason` (the sentence that states the choice),
#'   `common` (the full `sieve_common_family()` result), and `values` (a
#'   data frame with one row per model and the columns `cfi`, `tli`,
#'   `rmsea`, `rmsea_lo`, and `rmsea_hi` at full precision, `NA` for a model
#'   with no solution and for every model when no family is common), and
#'   `conf_level` (the level of the RMSEA intervals of the models with a
#'   solution, `NA` when they do not share one).
#' @author Ken Kelley
#' @noRd
sieve_fit_common <- function(measures, prefer = NULL) {
  families <- lapply(measures, function(m) {
    if (inherits(m, "sieve_fit_families")) {
      m
    } else if (!is.null(attr(m, "families"))) {
      attr(m, "families")
    } else {
      sieve_fit_families(NULL)
    }
  })
  if (is.null(names(families))) {
    names(families) <- paste("model", seq_along(families))
  }
  common <- sieve_common_family(families, prefer = prefer)
  indices <- c("cfi", "tli", "rmsea", "rmsea_lo", "rmsea_hi")
  values <- as.data.frame(matrix(NA_real_,
    nrow = length(families), ncol = length(indices),
    dimnames = list(NULL, indices)
  ))
  if (!is.na(common$family)) {
    for (i in which(common$solved)) {
      values[i, ] <- families[[i]]$families[[common$family]]$values[indices]
    }
  }
  levels <- unique(vapply(families[common$solved], sieve_fit_ci_level, numeric(1)))
  conf_level <- if (length(levels) == 1L) levels else NA_real_
  list(family = common$family, reason = common$reason, common = common, values = values,
    conf_level = conf_level)
}

#' The R that reproduces a comparison table from the fitted models
#'
#' Written from the comparison itself, so the recorded script computes what
#' the table shows: the test statistic and the indices of the family that
#' was used, the changes within that family, and the engine's difference
#' test of each model against the one before it (with the method that was
#' actually used when it is not lavaan's default).
#'
#' @param comparison A [sieve_fit_compare()] result.
#' @param fits The name, in the script, of the list of fitted models.
#' @return A character vector of R code lines (empty when no family was
#'   common to the models).
#' @author Ken Kelley
#' @noRd
sieve_compare_code <- function(comparison, fits = "fits_mi") {
  family <- comparison$family
  if (is.na(family)) {
    return(character(0))
  }
  columns <- sieve_fit_columns()
  shown <- c(
    columns[[comparison$test_family]][c("chisq", "df", "pvalue")],
    columns[[family]][c("cfi", "rmsea")]
  )
  models <- comparison$table$model
  tested <- which(vapply(comparison$tests, function(t) isTRUE(t$available), logical(1)))
  c(
    sprintf("# The table on screen is read from the fitted models: CFI and RMSEA in the %s", family),
    "# family for every model, changes within that family, and lavaan's chi square",
    "# difference test of each model against the one before it.",
    sprintf(
      "rung_fit <- t(sapply(%s, function(f) lavaan::fitMeasures(f, c(%s))))", fits,
      paste(sprintf("\"%s\"", shown), collapse = ", ")
    ),
    "rung_fit",
    sprintf(
      "apply(rung_fit[, c(\"%s\", \"%s\"), drop = FALSE], 2, diff)  # change from the model before",
      shown[["cfi"]], shown[["rmsea"]]
    ),
    vapply(tested, function(i) {
      sprintf(
        "lavaan::lavTestLRT(%s[[\"%s\"]], %s[[\"%s\"]]%s)", fits, models[i - 1L], fits,
        models[i],
        if (identical(comparison$tests[[i]]$method, "satorra.bentler.2010")) {
          ", method = \"satorra.bentler.2010\""
        } else {
          ""
        }
      )
    }, character(1))
  )
}

#' The conventions for changes in CFI and RMSEA, with their conditions
#'
#' The reference values in common use are conventions, not tests. They are
#' stated with where they come from, and no verdict is drawn from them.
#'
#' @param family The family the displayed changes are in, or `NA`.
#' @return One paragraph of plain text.
#' @author Ken Kelley
#' @noRd
sieve_invariance_convention <- function(family = NA_character_) {
  paste0(
    "A fall in CFI of no more than .01 (Cheung & Rensvold, 2002) and a rise in RMSEA of no ",
    "more than .015 (Chen, 2007) are widely used reference values for the added ",
    "constraints. They are conventions from simulations under particular conditions ",
    "(maximum likelihood, continuous and normally distributed indicators, and the sample ",
    "sizes and models those studies used), and they were worked out for the standard ",
    "indices. ",
    if (is.na(family)) {
      ""
    } else if (identical(family, "standard")) {
      "The changes shown here are in the standard family. "
    } else if (identical(family, "robust")) {
      paste0(
        "The changes shown here are in the robust family, which estimates the same ",
        "population values as the standard indices; how well the reference values carry ",
        "over to it has been studied far less. "
      )
    } else {
      paste0(
        "The changes shown here are in the scaled family, whose values are not on the scale ",
        "of the standard indices (Brosseau-Liard & Savalei, 2014; Savalei, 2021), so the ",
        "reference values are a rough guide at most. "
      )
    },
    "Weigh them with the difference test, the size of the freed or constrained ",
    "parameters, and the purpose of the comparison; SIEVER reports the changes and draws ",
    "no verdict from them."
  )
}

#' The method-factor comparison, within one family of fit indices
#'
#' Sets the substantive measurement model beside the same model with an
#' unmeasured latent method factor added: CFI and RMSEA of each, the change
#' from the first to the second, and the size of the method loadings. The
#' two models are compared in one family (see `sieve_common_family()`), so
#' each change is a difference of two values of the same statistic.
#'
#' @param substantive The fitted correlated-factors model.
#' @param method The fitted model with the method factor (see
#'   `sieve_run_method_factor()`); its `method_factor` attribute names the
#'   method factor.
#' @return A list with `table` (a data frame with `Statistic`, labeled with
#'   the family, and `Value`, at full precision; `NULL` when either model
#'   has no solution or no family is reported for both), `family`, `reason`
#'   (the sentence that states the family, or why there is no table), and
#'   `comparison` (the [sieve_fit_compare()] result).
#' @author Ken Kelley
#' @noRd
sieve_method_factor_table <- function(substantive, method) {
  cmp <- sieve_fit_compare(list(substantive = substantive, method = method), nested = FALSE)
  out <- list(table = NULL, family = cmp$family, reason = cmp$common$reason, comparison = cmp)
  ss <- if (is.null(method)) NULL else {
    tryCatch(lavaan::standardizedSolution(method), error = function(e) NULL)
  }
  if (is.null(ss) || !all(cmp$common$solved) || is.na(cmp$family)) {
    return(out)
  }
  loading <- ss$est.std[ss$op == "=~" & ss$lhs == attr(method, "method_factor")]
  tag <- sprintf(" (%s)", cmp$family)
  out$table <- data.frame(
    Statistic = c(
      paste0("Substantive CFI", tag), paste0("With method factor CFI", tag),
      paste0("Change in CFI", tag),
      paste0("Substantive RMSEA", tag), paste0("With method factor RMSEA", tag),
      paste0("Change in RMSEA", tag),
      "Average |method loading|", "Variance from method (mean squared loading)"
    ),
    Value = c(
      cmp$table$cfi[1L], cmp$table$cfi[2L], cmp$table$delta_cfi[2L],
      cmp$table$rmsea[1L], cmp$table$rmsea[2L], cmp$table$delta_rmsea[2L],
      mean(abs(loading), na.rm = TRUE), mean(loading^2, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  out
}

#' Read the public DMAR likelihood-eligibility decision for H1 results
#'
#' DMAR refuses the fit measures of a fit whose unrestricted (H1) moments
#' are unavailable, and the two-stage measures of a fit that does not carry
#' the all-row sample. Those refusals arrive as conditions of class
#' `dmar_h1_unavailable` and `dmar_two_stage_rows_unavailable`, both
#' inheriting `dmar_refusal`; they are recognized by class, and by their
#' words when a condition arrives without its class.
#'
#' @param fit Fitted lavaan model.
#' @return A plain refusal, or NULL. Absence of a refusal is not an existence proof.
#' @author Ken Kelley
#' @noRd
sieve_h1_refusal <- function(fit) {
  cached <- attr(fit, "sieve_h1_eligibility", exact = TRUE)
  if (!is.null(cached)) return(cached$reason)
  opt <- lavaan::lavInspect(fit, "options")
  if (!opt$missing %in% c("ml", "ml.x", "two.stage", "robust.two.stage") ||
      length(lavaan::lavNames(fit, "ov.ord"))) return(NULL)
  # Only whether DMAR refuses is read here, which does not depend on the
  # level of the interval, so DMAR's default level is left in force.
  eligible <- sieve_capture(DMAR::sem_fit_measures(fit))
  refused <- function(condition_class, words) {
    !eligible$ok && (isTRUE(condition_class %in% eligible$error_class) ||
      grepl(words, eligible$error, fixed = TRUE))
  }
  if (refused("dmar_h1_unavailable", "unrestricted H1")) {
    return(eligible$error)
  }
  if (opt$missing %in% c("two.stage", "robust.two.stage") &&
      refused("dmar_two_stage_rows_unavailable", "Two-stage fit measures require the same all-row sample")) {
    return(eligible$error)
  }
  if (eligible$ok && opt$missing %in% c("two.stage", "robust.two.stage")) {
    all_n <- eligible$value$value[eligible$value$term == "N"]
    fitted_n <- sum(lavaan::lavInspect(fit, "nobs"))
    if (length(all_n) == 1L && is.finite(all_n) && all_n > fitted_n) return(paste(
      "Two-stage results require a corrected all-row fit for respondents observed only on auxiliaries.",
      "This supplied fit does not carry the same fitted sample. Use FIML or a verified corrected fit;",
      "no uncorrected parameters, residuals, composite reliability or criteria are substituted."))
  }
  NULL
}

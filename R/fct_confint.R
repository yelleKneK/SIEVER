# The confidence limits of a headless analysis: one registry of the tables
# that hold limits, by analysis, and one rule by interval type, read by
# confint.siever_analysis().

#' The interval types whose limits are formed again at another level
#'
#' A Wald interval is the estimate plus and minus a normal (or t) quantile
#' times the standard error, on the scale the interval was formed on, so
#' the stored estimate and standard error give it at any level. Every
#' other type needs what a result does not keep (bootstrap replications, a
#' refit, the ordered scores, the counts at a new level, the noncentral
#' chi-square inversion at a new level).
#'
#' @return A character vector of interval kinds.
#' @author Ken Kelley
#' @noRd
sieve_confint_recomputable <- function() "wald"

#' The rows of one interval table
#'
#' @param term Row names.
#' @param estimate,se Point estimate and standard error (`NA` when the
#'   table has none).
#' @param lower,upper The stored limits (`NA` for the absent limit of a
#'   one-sided bound).
#' @param conf_level The stored level of each row.
#' @param method The interval method, in words.
#' @param kind `"wald"`, `"bootstrap"`, `"profile"`, `"order_statistic"`,
#'   `"exact_binomial"`, `"noncentral"`, `"lavaan_bootstrap"`,
#'   `"wald_without_se"`, `"one_sided"`, or `"other"`.
#' @param scale For a Wald interval, `"identity"`, `"unit"` (clamped into
#'   the unit interval), `"logit"`, or `"log_complement"` (Bonett's
#'   ln(1 - alpha)); `NA` when the scale is not recorded.
#' @param df The t reference degrees of freedom, `Inf` for the normal.
#' @param transformation The recorded transformation, in words.
#' @param side `"two-sided"`, or `"upper"` or `"lower"` for a one-sided
#'   bound.
#' @param n_used The number of cases the estimate uses (respondents, or
#'   experts for content validity), `NA` when the result does not record it.
#' @param B_used Bootstrap replications used, for a bootstrap interval.
#' @param reason Why a row has no interval (`NA` when it has one).
#' @param rerun How to obtain the limits at another level, with `<level>`
#'   standing for the requested level and `<min_B>` for the fewest
#'   bootstrap replications DMAR forms a limit from at that level
#'   (10 / ((1 - level) / 2) for a two-sided percentile interval,
#'   10 / min(level, 1 - level) for a one-sided bound, and never fewer than
#'   100) (`NA` when they are
#'   computed); for a one-sided bound that is stored, the refusal that
#'   names the column to read.
#' @return A data frame with one row per term, in the columns of the
#'   interval descriptor (see `sieve_analysis_intervals()`) after `table`.
#' @author Ken Kelley
#' @noRd
sieve_interval_rows <- function(term, estimate = NA_real_, se = NA_real_, lower, upper, conf_level,
                                method, kind, scale = NA_character_, df = Inf,
                                transformation = NA_character_, side = "two-sided", n_used = NA_integer_,
                                B_used = NA_integer_, reason = NA_character_, rerun = NA_character_) {
  n <- length(term)
  each <- function(x) if (length(x) == 1L) rep(x, n) else x
  rows <- data.frame(
    term = as.character(term), estimate = as.numeric(each(estimate)), se = as.numeric(each(se)),
    lower = as.numeric(each(lower)), upper = as.numeric(each(upper)),
    conf_level = as.numeric(each(conf_level)), side = as.character(each(side)),
    method = as.character(each(method)), kind = as.character(each(kind)),
    scale = as.character(each(scale)), df = as.numeric(each(df)),
    transformation = as.character(each(transformation)),
    n_used = suppressWarnings(as.integer(each(n_used))), B_used = as.integer(each(B_used)),
    recomputed = rep(FALSE, n), reason = as.character(each(reason)), rerun = as.character(each(rerun)),
    stringsAsFactors = FALSE
  )
  # A row without its limits (both for a two-sided interval, the one bound
  # for a one-sided one) has no interval to return.
  has <- ifelse(rows$side %in% "upper", is.finite(rows$upper),
    ifelse(rows$side %in% "lower", is.finite(rows$lower), is.finite(rows$lower) & is.finite(rows$upper))
  )
  missing <- is.na(rows$reason) & !has
  rows$reason[missing] <- "no limits are stored for this row"
  rows
}

#' The stored level of an interval table, read from where it lives
#'
#' The level is read from a `conf_level` (or `level`) column, from DMAR's
#' metadata for a DMAR table (`rmsea_conf_level` for RMSEA limits, through
#' [DMAR::dmar_metadata()]), from the table's `conf_level` attribute, or
#' from the result element that records it. Places that disagree, or a
#' table that records no level, are refused with the reason: limits are
#' never attributed to a level the result does not state.
#'
#' @param tbl The table.
#' @param limits The names of its limit columns, for the refusal.
#' @param rmsea Whether the limits are RMSEA limits.
#' @param sibling A level recorded beside the table, or `NULL`.
#' @param sibling_label Where `sibling` is recorded, in words.
#' @return The level of each row.
#' @author Ken Kelley
#' @noRd
sieve_confint_level_of <- function(tbl, limits, rmsea = FALSE, sibling = NULL, sibling_label = NULL) {
  n <- nrow(tbl)
  found <- list()
  column <- intersect(c("conf_level", "level"), names(tbl))
  if (length(column)) {
    found[[sprintf("its %s column", column[1L])]] <- suppressWarnings(as.numeric(tbl[[column[1L]]]))
  }
  if (inherits(tbl, "dmar_tbl")) {
    meta <- DMAR::dmar_metadata(tbl)
    field <- if (rmsea) "rmsea_conf_level" else "conf_level"
    if (length(meta[[field]])) found[[sprintf("its DMAR metadata (%s)", field)]] <- rep(as.numeric(meta[[field]])[1L], n)
  } else if (length(attr(tbl, "conf_level", exact = TRUE))) {
    found[["its conf_level attribute"]] <- rep(as.numeric(attr(tbl, "conf_level", exact = TRUE))[1L], n)
  }
  if (length(sibling)) found[[sibling_label]] <- rep(suppressWarnings(as.numeric(sibling))[1L], n)
  unrecorded <- sprintf(
    "The table names interval limits (%s) but records no confidence level, so they are not returned as an interval.",
    paste(limits, collapse = ", ")
  )
  if (!length(found)) stop(unrecorded, call. = FALSE)
  for (where in names(found)) {
    v <- found[[where]]
    if (any(!is.finite(v) | v <= 0 | v >= 1)) {
      stop(sprintf(
        "The table names interval limits (%s) but %s holds no confidence level strictly between 0 and 1 for %d of its %d rows, so they are not returned as an interval.",
        paste(limits, collapse = ", "), where, sum(!is.finite(v) | v <= 0 | v >= 1), n
      ), call. = FALSE)
    }
  }
  if (length(found) > 1L) {
    first <- found[[1L]]
    for (where in names(found)[-1L]) {
      differs <- abs(found[[where]] - first) > 1e-12
      if (any(differs)) {
        at <- which(differs)[1L]
        stop(sprintf(
          "The table records level %s in %s and %s in %s; the limits cannot be attributed to one level.",
          sieve_confint_fmt(first[at]), names(found)[1L], sieve_confint_fmt(found[[where]][at]), where
        ), call. = FALSE)
      }
    }
  }
  found[[1L]]
}

#' A number in a message, at full precision without trailing zeros
#'
#' @param x A number.
#' @return A string.
#' @author Ken Kelley
#' @noRd
sieve_confint_fmt <- function(x) format(x, digits = 12)

#' The fewest bootstrap replications DMAR uses for a limit
#'
#' DMAR reports a bootstrap limit only when at least 10 replications lie
#' beyond it: a two-sided percentile interval at level C needs
#' 10 / ((1 - C) / 2) of them, a one-sided bound 10 / min(C, 1 - C), and
#' never fewer than 100 replications (DMAR 1.1.0 refuses a smaller B, at
#' 0.80 for example, where the first rule alone would give 50).
#'
#' @param level The confidence level.
#' @param one_sided Whether the limit is a one-sided bound.
#' @return A single integer.
#' @author Ken Kelley
#' @noRd
sieve_confint_dmar_min_B <- function(level, one_sided = FALSE) {
  tail <- if (one_sided) min(level, 1 - level) else (1 - level) / 2
  max(100L, as.integer(ceiling(10 / tail - 1e-9)))
}

#' The level a DMAR call asked for
#'
#' A DMAR result formed without a bootstrap records no level, so advice on
#' forming its interval is given at the level the call asked for: the
#' `conf_level` it was given, or the engine's default when it was given
#' none.
#'
#' @param given The call's `conf_level`, or `NULL`.
#' @param engine The DMAR function.
#' @return One level, or `NA` when neither is a level.
#' @author Ken Kelley
#' @noRd
sieve_confint_asked_level <- function(given, engine) {
  is_level <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0 && x < 1
  if (is_level(given)) return(as.numeric(given))
  default <- formals(engine)$conf_level
  if (is_level(default)) as.numeric(default) else NA_real_
}

#' The placeholders of a rerun sentence, filled in at a level
#'
#' @param rerun Rerun sentences (see `sieve_interval_rows()`).
#' @param level The requested level.
#' @param one_sided Whether each sentence's limit is a one-sided bound.
#' @return The sentences with `<level>` and `<min_B>` filled in.
#' @author Ken Kelley
#' @noRd
sieve_confint_fill_rerun <- function(rerun, level, one_sided = FALSE) {
  one_sided <- rep_len(one_sided, length(rerun))
  vapply(seq_along(rerun), function(i) {
    if (is.na(rerun[i])) return(NA_character_)
    min_B <- as.character(sieve_confint_dmar_min_B(level, one_sided[i]))
    gsub("<min_B>", min_B, gsub("<level>", sieve_confint_fmt(level), rerun[i], fixed = TRUE), fixed = TRUE)
  }, character(1))
}

#' How to obtain a service's limits at another level
#'
#' Reads the service's own arguments, so the advice names an argument only
#' when the service has it.
#'
#' @param analysis The analysis name.
#' @param service The SIEVER service function's name.
#' @param stored The stored level.
#' @param what What the limits are, in words.
#' @param extra Further arguments of the rerun, as R text.
#' @return A sentence with `<level>` standing for the requested level.
#' @author Ken Kelley
#' @noRd
sieve_confint_rerun <- function(analysis, service, stored, what, extra = "") {
  fun <- get(service, envir = asNamespace("SIEVER"), inherits = FALSE)
  if ("conf_level" %in% names(formals(fun))) {
    sprintf("Run sieve_analyze(\"%s\", ..., conf_level = <level>%s) again for %s at that level.", analysis, extra, what)
  } else {
    sprintf("%s() forms %s at %s only; it has no argument for another level.", service, what, sieve_confint_fmt(stored))
  }
}

#' The RMSEA rows of a fit table
#'
#' @param tbl The table.
#' @param label Row labels (a number of factors, a model).
#' @param estimate,lower,upper Column names.
#' @param level The level of each row (see `sieve_confint_level_of()`).
#' @param family The fit index family, or `NULL`.
#' @param rerun How to obtain them at another level.
#' @param n_used The cases each row rests on.
#' @return Interval rows.
#' @author Ken Kelley
#' @noRd
sieve_confint_rmsea_rows <- function(tbl, label, estimate, lower, upper, level, family = NULL, rerun,
                                     n_used = NA_integer_) {
  sieve_interval_rows(
    term = paste0(label, ": RMSEA"), estimate = tbl[[estimate]],
    lower = tbl[[lower]], upper = tbl[[upper]], conf_level = level,
    method = paste0(
      "RMSEA interval by inversion of the noncentral chi-square distribution of the test statistic",
      if (length(family) && !is.na(family)) sprintf(" (%s family)", family) else ""
    ),
    kind = "noncentral", transformation = "none (noncentrality limits carried to the RMSEA scale)",
    n_used = n_used, rerun = rerun
  )
}

#' The sentence that reruns an RMSEA interval at another level
#'
#' @param analysis The analysis name.
#' @param argument Whether the service also takes `conf_level` for it.
#' @return A sentence with `<level>` standing for the requested level.
#' @author Ken Kelley
#' @noRd
sieve_confint_rmsea_rerun <- function(analysis, argument = FALSE) {
  paste0(
    "Set options(sieve.rmsea_conf_level = <level>) and run sieve_analyze(\"", analysis, "\", ...) again",
    if (argument) " (or pass conf_level = <level>)" else "",
    " for RMSEA limits at that level."
  )
}

#' The parameter rows of a lavaan fit
#'
#' The free and defined parameters, named as lavaan's `coef()` names them
#' (`lhs`, `op`, and `rhs` pasted without spaces, with `.g2` for the second
#' group and `.l2` for the second level), defined parameters by their
#' label. Fixed parameters have no sampling interval and carry the reason.
#' The stored level is lavaan's own (the level of the parameter table the
#' analysis reports, `parameterEstimates()`'s default).
#'
#' @param fit A lavaan fit with a solution.
#' @return Interval rows.
#' @author Ken Kelley
#' @noRd
sieve_confint_lavaan_rows <- function(fit) {
  stored <- eval(formals(lavaan::parameterEstimates)$level)
  se_type <- as.character(lavaan::lavInspect(fit, "options")$se)[1L]
  pe <- lavaan::parameterEstimates(fit, ci = TRUE, level = stored, plabel = TRUE)
  pt <- lavaan::parTable(fit)
  defined <- pe$op == ":="
  free <- pt$free[match(pe$plabel, pt$plabel)]
  free[is.na(free)] <- 0L
  term <- ifelse(defined, pe$lhs, paste0(pe$lhs, pe$op, pe$rhs))
  if (!is.null(pe$group) && length(unique(pe$group[!defined])) > 1L) {
    term <- paste0(term, ifelse(!defined & pe$group > 1L, paste0(".g", pe$group), ""))
  }
  if (!is.null(pe$level) && length(unique(pe$level[!defined])) > 1L) {
    index <- match(pe$level, unique(pe$level[!defined]))
    term <- paste0(term, ifelse(!defined & index > 1L, paste0(".l", index), ""))
  }
  reason <- ifelse(!defined & free == 0, "it is fixed in the model, so it has no interval", NA_character_)
  if (identical(se_type, "none")) reason[] <- "the model was fit without standard errors (se = \"none\")"
  bootstrap <- identical(se_type, "bootstrap")
  # The cases each parameter rests on: its group's, or every group's for a
  # defined parameter and a single group model.
  n_total <- tryCatch(as.integer(lavaan::lavInspect(fit, "ntotal")), error = function(e) NA_integer_)
  n_group <- tryCatch(as.integer(lavaan::lavInspect(fit, "nobs")), error = function(e) NA_integer_)
  n_used <- rep(n_total, nrow(pe))
  if (!is.null(pe$group) && length(n_group) > 1L) {
    in_group <- !defined & pe$group >= 1L & pe$group <= length(n_group)
    n_used[in_group] <- n_group[pe$group[in_group]]
  }
  sieve_interval_rows(
    term = term, estimate = pe$est, se = pe$se, lower = pe$ci.lower, upper = pe$ci.upper,
    conf_level = stored, n_used = n_used,
    method = if (bootstrap) {
      "Bootstrap percentile interval (lavaan, from the draws the fit keeps)"
    } else {
      sprintf("Wald interval, normal reference, standard errors of lavaan's se = \"%s\"", se_type)
    },
    kind = if (bootstrap) "lavaan_bootstrap" else "wald", scale = "identity", transformation = "none",
    reason = reason,
    rerun = if (bootstrap) {
      "lavaan::parameterEstimates(fit, level = <level>) forms them from the bootstrap draws the fit keeps."
    } else NA_character_
  )
}

#' The coefficient rows of a reliability analysis
#'
#' Robust omega, model-implied omega, H, and alpha can sit side by side in
#' one result, each with its own interval, named "construct: coefficient".
#' The interval's method code and recorded transformation say whether it
#' is a Wald interval and on which scale it was formed.
#'
#' @param result A result from [sieve_reliability_construct()].
#' @return Interval rows.
#' @author Ken Kelley
#' @noRd
sieve_confint_reliability_rows <- function(result) {
  construct <- result$omega$construct %||% result$spec$construct %||% "Construct"
  parts <- list(
    list(record = result$omega, label = result$omega$label %||% "Omega"),
    list(record = result$model_implied, label = result$model_implied$label %||% "Omega (model-implied denominator)"),
    list(record = result$H, label = result$H$label %||% "H"),
    list(record = result$alpha, label = "Alpha")
  )
  wald <- c("ml", "mlr", "adf", "ml_logistic", "mlr_logistic", "adf_logistic", "bootstrap_se_logistic", "bonett")
  bootstrap <- c("percentile", "dmar_percentile", "bca", "dmar_bca", "cluster_percentile")
  rows <- lapply(parts, function(part) {
    x <- part$record
    if (is.null(x)) return(NULL)
    u <- x$uncertainty %||% list()
    code <- if (is.character(u$ci_method) && length(u$ci_method) == 1L) u$ci_method else u$method %||% NA_character_
    code <- sub("[ (].*$", "", code)
    kind <- if (code %in% wald) "wald" else if (code %in% bootstrap) "bootstrap" else if (identical(code, "likelihood")) "profile" else "other"
    transformation <- u$transformation %||% NA_character_
    scale <- if (is.na(transformation)) NA_character_ else if (startsWith(transformation, "none (symmetric")) {
      "unit"
    } else if (startsWith(transformation, "logit")) {
      "logit"
    } else if (startsWith(transformation, "log(1 - ")) {
      "log_complement"
    } else NA_character_
    df <- suppressWarnings(as.numeric(u$wald_df %||% Inf))
    if (length(df) != 1L || is.na(df)) df <- Inf
    has <- is.finite(x$lower %||% NA_real_) && is.finite(x$upper %||% NA_real_)
    stored <- x$conf_level %||% NA_real_
    rerun <- paste0(
      "Run sieve_analyze(\"reliability\", data, spec = sieve_reliability_spec(..., conf_level = <level>)) again",
      if (code %in% c("percentile", "dmar_percentile")) {
        "; DMAR forms a percentile interval at that level from at least <min_B> replications"
      } else "",
      "."
    )
    sieve_interval_rows(
      term = paste0(construct, ": ", part$label), estimate = x$estimate %||% NA_real_,
      se = x$se %||% NA_real_, lower = x$lower %||% NA_real_, upper = x$upper %||% NA_real_,
      conf_level = stored, method = u$label %||% code, kind = kind, scale = scale, df = df,
      transformation = transformation, n_used = if (is.list(x$n)) x$n$used %||% NA_integer_ else NA_integer_, B_used = u$B_used %||% NA_integer_,
      reason = if (has) NA_character_ else paste0("no interval was formed", if (length(u$reason)) paste0(" (", u$reason, ")") else ""),
      rerun = if (identical(kind, "wald")) NA_character_ else rerun
    )
  })
  do.call(rbind, Filter(Negate(is.null), rows))
}

#' A table the registry reads, or a refusal that says why it is missing
#'
#' @param tbl The table.
#' @param what What the table is, in words.
#' @param why The result's own reason, or `NULL`.
#' @return `tbl`, when it is a data frame with rows.
#' @author Ken Kelley
#' @noRd
sieve_confint_need <- function(tbl, what, why = NULL) {
  if (!is.data.frame(tbl) || !nrow(tbl)) {
    stop(trimws(paste(sprintf("This analysis has no %s, so it has no limits to return.", what), paste(why, collapse = " "))),
      call. = FALSE
    )
  }
  tbl
}

#' The interval tables of a headless analysis
#'
#' One entry per table that holds confidence limits, the analysis's main
#' interval table first (the default of `confint()`), each with its name
#' (the name `summary()` gives it where it has one) and a function that
#' builds its rows. An analysis with no table of limits gives the reason
#' instead.
#'
#' @param object A `siever_analysis` object with a result.
#' @param args The service's arguments, where the sample size of an
#'   engine's result is read from its input (the fit of an AVE, the data of
#'   an HTMT).
#' @return A list of entries (`name`, `build`), or a single string, the
#'   reason the analysis has no limits.
#' @author Ken Kelley
#' @noRd
sieve_interval_registry <- function(object, args = list()) {
  value <- object$result
  analysis <- object$analysis
  entry <- function(name, build) list(name = name, build = build)
  quietly <- function(expr) tryCatch(expr, error = function(e) NA_integer_)
  n_fit <- function(fit) {
    if (inherits(fit, "lavaan")) quietly(as.integer(lavaan::lavInspect(fit, "ntotal"))) else NA_integer_
  }
  fit_entry <- function(fit) {
    entry("parameters", function() {
      if (is.null(fit) || !inherits(fit, "lavaan") || !sieve_has_solution(fit)) {
        stop(sprintf(
          "The %s model has no solution, so it has no parameter intervals. %s",
          analysis, object$status$message %||% ""
        ), call. = FALSE)
      }
      sieve_confint_lavaan_rows(fit)
    })
  }
  switch(analysis,
    cfa = list(fit_entry(value)),
    sem = list(fit_entry(value$fit)),
    bifactor = ,
    longitudinal = list(fit_entry(value$fit)),
    reliability = list(entry("reliability", function() sieve_confint_reliability_rows(value))),
    factor_scores = list(entry("uncertainty", function() {
      u <- sieve_confint_need(value$uncertainty, "table of score intervals", value$reason)
      # A respondent's score interval is conditional on that respondent's
      # responses and the fitted parameters; it rests on no sample of
      # respondents, so it records none.
      sieve_interval_rows(
        term = paste0("row ", u$original_row, ": ", u$factor), estimate = u$estimate, se = u$se,
        lower = u$lower, upper = u$upper,
        conf_level = sieve_confint_level_of(u, c("lower", "upper"),
          sibling = value$conf_level, sibling_label = "the result's conf_level"
        ),
        method = u$interval_method, kind = "wald", scale = "identity", transformation = "none",
        reason = ifelse(is.finite(u$lower) & is.finite(u$upper), NA_character_,
          ifelse(nzchar(u$reason %||% ""), u$reason, "no score error for this row")
        )
      )
    })),
    norms = list(entry("Results: uncertainty", function() {
      u <- sieve_confint_need(value$uncertainty, "table of percentile limits", value$refused)
      level <- sieve_confint_level_of(u, c("lower", "upper"))
      sieve_interval_rows(
        term = paste0(u$construct, ": p = ", as.character(u$probability)), estimate = u$estimate,
        lower = u$lower, upper = u$upper, conf_level = level, method = u$method,
        kind = "order_statistic", transformation = "none (order statistics of the scores)",
        n_used = u$n %||% NA_integer_,
        rerun = sieve_confint_rerun("norms", "sieve_construct_norms", level[1L], "the percentile limits")
      )
    })),
    efa = list(entry("Results", function() {
      sieve_confint_need(value, "exploratory fit table")
      level <- sieve_confint_level_of(value, c("RMSEA.lo", "RMSEA.hi"), rmsea = TRUE)
      sieve_confint_rmsea_rows(value, paste0("k = ", value$k), "RMSEA", "RMSEA.lo", "RMSEA.hi", level,
        family = attr(value, "family", exact = TRUE), rerun = sieve_confint_rmsea_rerun("efa", argument = TRUE),
        n_used = value$nobs %||% NA_integer_
      )
    })),
    retention = {
      out <- list(entry("Results: fit: table", function() {
        tbl <- sieve_confint_need(value$fit$table, "fit table", value$fit$reason)
        level <- sieve_confint_level_of(tbl, c("rmsea_lo", "rmsea_hi"),
          rmsea = TRUE,
          sibling = value$fit$conf_level, sibling_label = "the fit result's conf_level"
        )
        sieve_confint_rmsea_rows(tbl, paste0("k = ", tbl$k), "rmsea", "rmsea_lo", "rmsea_hi", level,
          family = value$fit$family, rerun = sieve_confint_rmsea_rerun("retention"),
          n_used = tbl$nobs %||% value$fit$n %||% NA_integer_
        )
      }))
      for (ref in names(value$pa$dmar)) {
        out[[length(out) + 1L]] <- local({
          reference <- ref
          entry(paste0("Results: pa: dmar: ", reference), function() {
            tbl <- sieve_confint_need(value$pa$dmar[[reference]], "parallel analysis table", value$pa$reason)
            level <- sieve_confint_level_of(tbl, c("reference_quantile_ci_lower", "reference_quantile_ci_upper"),
              sibling = value$pa$call$calls[[reference]]$args$conf_level,
              sibling_label = "the parallel analysis call's conf_level"
            )
            sieve_interval_rows(
              term = tbl$term, estimate = tbl$reference_quantile,
              lower = tbl$reference_quantile_ci_lower, upper = tbl$reference_quantile_ci_upper,
              conf_level = level,
              method = "Monte Carlo binomial order-statistic limits for the reference percentile (DMAR)",
              kind = "order_statistic", transformation = "none (order statistics of the reference draws)",
              n_used = value$pa$n %||% NA_integer_,
              rerun = sieve_confint_rerun("retention", "sieve_retention_evidence", level[1L], "the reference percentile limits")
            )
          })
        })
      }
      out
    },
    invariance = {
      comp <- value$compare
      by_family <- comp$by_family %||% list()
      names(by_family) <- paste0("by_family: ", names(by_family), recycle0 = TRUE)
      tables <- c(list(table = comp$table), by_family)
      tables <- Filter(function(t) is.data.frame(t) && all(c("rmsea_lo", "rmsea_hi") %in% names(t)), tables)
      fits <- if (is.list(value$mi)) attr(value$mi, "fits", exact = TRUE) else NULL
      lapply(names(tables), function(nm) {
        entry(paste0("Results: compare: ", nm), function() {
          tbl <- sieve_confint_need(tables[[nm]], "fit comparison table")
          level <- sieve_confint_level_of(tbl, c("rmsea_lo", "rmsea_hi"),
            rmsea = TRUE,
            sibling = comp$conf_level, sibling_label = "the comparison's conf_level"
          )
          family <- if (startsWith(nm, "by_family: ")) sub("^by_family: ", "", nm) else comp$family
          n_used <- vapply(as.character(tbl$model), function(m) {
            if (is.list(fits) && m %in% names(fits)) n_fit(fits[[m]]) else NA_integer_
          }, integer(1), USE.NAMES = FALSE)
          sieve_confint_rmsea_rows(tbl, tbl$model, "rmsea", "rmsea_lo", "rmsea_hi", level,
            family = family, rerun = sieve_confint_rmsea_rerun("invariance"), n_used = n_used
          )
        })
      })
    },
    cross_validation = list(
      entry("table", function() {
        tbl <- sieve_confint_need(value$table, "fit table", value$error)
        rmsea_level <- sieve_confint_level_of(tbl, c("RMSEA.lo", "RMSEA.hi"),
          rmsea = TRUE,
          sibling = value$rmsea_conf_level, sibling_label = "the result's rmsea_conf_level"
        )
        ecvi_level <- sieve_confint_level_of(tbl, c("ECVI.lo", "ECVI.hi"),
          sibling = value$conf_level, sibling_label = "the result's conf_level"
        )
        n_used <- tbl$n %||% NA_integer_
        rbind(
          sieve_confint_rmsea_rows(tbl, tbl$Sample, "RMSEA", "RMSEA.lo", "RMSEA.hi", rmsea_level,
            family = value$family, rerun = sieve_confint_rmsea_rerun("cross_validation"), n_used = n_used
          ),
          sieve_interval_rows(
            term = paste0(tbl$Sample, ": ECVI"), estimate = tbl$ECVI, lower = tbl$ECVI.lo,
            upper = tbl$ECVI.hi, conf_level = ecvi_level,
            method = "ECVI interval from the noncentral chi-square limits of the noncentrality (DMAR::ecvi())",
            kind = "noncentral", transformation = "none (noncentrality limits carried to the ECVI scale)",
            n_used = n_used,
            rerun = sprintf(
              "sieve_cross_validate() forms ECVI limits at %s only; DMAR::ecvi(fit, conf_level = <level>) forms them at another level for a fitted half.",
              sieve_confint_fmt(ecvi_level[1L])
            )
          )
        )
      }),
      entry("rel", function() {
        tbl <- sieve_confint_need(value$rel, "split reliability table", value$error)
        level <- sieve_confint_level_of(tbl, c("Calibration.lower", "Validation.upper", "Difference.lower"),
          sibling = value$conf_level, sibling_label = "the result's conf_level"
        )
        halves <- c(Calibration = "calibration", Validation = "validation", Difference = "difference")
        n_half <- c(
          Calibration = value$n_cal %||% NA_integer_, Validation = value$n_val %||% NA_integer_,
          Difference = (value$n_cal %||% NA_integer_) + (value$n_val %||% NA_integer_)
        )
        rerun <- sprintf(
          "sieve_cross_validate() forms the split reliability limits at %s only, and keeps them without their standard errors.",
          sieve_confint_fmt(level[1L])
        )
        do.call(rbind, lapply(names(halves), function(h) {
          lower <- tbl[[paste0(h, ".lower")]]
          upper <- tbl[[paste0(h, ".upper")]]
          sieve_interval_rows(
            term = paste0(tbl$Construct, ": ", tbl$Coefficient, ": ", halves[[h]]), estimate = tbl[[h]],
            lower = lower, upper = upper, conf_level = level,
            method = if (identical(h, "Difference")) {
              "Delta method interval for the difference between independent halves"
            } else tbl$Interval,
            kind = "wald_without_se", n_used = n_half[[h]],
            reason = ifelse(is.finite(lower) & is.finite(upper), NA_character_, paste0("no interval (", tbl$Interval, ")")),
            rerun = rerun
          )
        }))
      })
    ),
    ave = list(entry("Results", function() {
      sieve_confint_need(value, "AVE table")
      # AVE estimated without a bootstrap has empty limit columns and no
      # level; its rows say so rather than refusing the table, and name the
      # level the call asked for (DMAR records none without a bootstrap)
      # and the replications DMAR needs at it.
      bounded <- any(is.finite(value$ci_lower) & is.finite(value$ci_upper))
      level <- if (bounded) sieve_confint_level_of(value, c("ci_lower", "ci_upper")) else NA_real_
      meta <- DMAR::dmar_metadata(value)
      bootstrapped <- bounded || identical(args$ci_method, "percentile")
      asked <- if (bounded) level[1L] else sieve_confint_asked_level(args$conf_level, DMAR::average_variance_extracted)
      missing_reason <- if (bootstrapped) {
        "DMAR formed no percentile interval for this factor; its warnings and bootstrap counts are in DMAR::dmar_metadata(result$result)"
      } else if (is.finite(asked)) {
        sprintf(
          "AVE was estimated without an interval; run sieve_analyze(\"ave\", fit = ..., ci_method = \"percentile\", conf_level = %s, B = %d or more, seed = ...) for a percentile interval at that level",
          sieve_confint_fmt(asked), sieve_confint_dmar_min_B(asked)
        )
      } else {
        "AVE was estimated without an interval; run sieve_analyze(\"ave\", fit = ..., ci_method = \"percentile\", conf_level = C, B = max(100, 10 / ((1 - C) / 2)) or more, seed = ...) for a percentile interval at level C"
      }
      sieve_interval_rows(
        term = value$factor, estimate = value$ave, lower = value$ci_lower, upper = value$ci_upper,
        conf_level = level, method = "Bootstrap percentile interval (DMAR)", kind = "bootstrap",
        transformation = "none (limits are percentiles of the replications used)",
        n_used = n_fit(args$fit), B_used = meta$bootstrap$used %||% NA_integer_,
        reason = ifelse(is.finite(value$ci_lower) & is.finite(value$ci_upper), NA_character_, missing_reason),
        rerun = "Run sieve_analyze(\"ave\", fit = ..., ci_method = \"percentile\", conf_level = <level>, B = <min_B> or more) again; DMAR forms a percentile interval at that level from at least <min_B> replications."
      )
    })),
    content = list(entry("Results", function() {
      sieve_confint_need(value, "content validity table")
      level <- sieve_confint_level_of(value, c("ci_lower", "ci_upper"))
      sieve_interval_rows(
        term = value$item, estimate = value$i_cvi, lower = value$ci_lower, upper = value$ci_upper,
        conf_level = level, method = "Exact (Clopper-Pearson) binomial interval for the item content validity index (DMAR)",
        kind = "exact_binomial", transformation = "none (exact binomial limits)",
        n_used = value$n_experts %||% NA_integer_,
        rerun = "Run sieve_analyze(\"content\", ratings = ..., conf_level = <level>) again for limits at that level."
      )
    })),
    htmt = list(entry("Results", function() {
      sieve_confint_need(value, "HTMT table")
      bounded <- any(is.finite(value$ci_upper))
      level <- if (bounded) sieve_confint_level_of(value, "ci_upper") else NA_real_
      meta <- DMAR::dmar_metadata(value)
      # Without a bootstrap the result records no level, so the advice is
      # given at the level the call asked for (DMAR's default when it named
      # none), with the replications DMAR needs for a one-sided bound
      # there, max(100, 10 / min(C, 1 - C)).
      bootstrapped <- bounded || (is.numeric(args$B) && length(args$B) == 1L && isTRUE(args$B > 0))
      asked <- if (bounded) level[1L] else sieve_confint_asked_level(args$conf_level, DMAR::htmt)
      rerun <- if (bounded) {
        sprintf(
          paste0(
            "HTMT reports a one-sided upper confidence bound at level %s (column ci_upper), not a two-sided interval, ",
            "and confint() returns two-sided intervals only. Read as.data.frame(result)$ci_upper; for a bound at another ",
            "level C, run sieve_analyze(\"htmt\", ..., conf_level = C, B = max(100, 10 / min(C, 1 - C)) or more, seed = ...) again ",
            "(DMAR forms the bound from at least that many replications, %d at %s)."
          ),
          sieve_confint_fmt(level[1L]), sieve_confint_dmar_min_B(level[1L], one_sided = TRUE), sieve_confint_fmt(level[1L])
        )
      } else {
        paste0(
          "Run sieve_analyze(\"htmt\", ..., conf_level = <level>, B = <min_B> or more, seed = ...) again for a one-sided ",
          "upper confidence bound at that level; DMAR forms the bound from at least <min_B> replications."
        )
      }
      missing_reason <- if (bootstrapped) {
        "DMAR formed no bound for this pair; its warnings and bootstrap counts are in DMAR::dmar_metadata(result$result)"
      } else if (is.finite(asked)) {
        sprintf(
          "HTMT was estimated without a bound; run sieve_analyze(\"htmt\", ..., conf_level = %s, B = %d or more, seed = ...) for a one-sided upper bound at that level",
          sieve_confint_fmt(asked), sieve_confint_dmar_min_B(asked, one_sided = TRUE)
        )
      } else {
        "HTMT was estimated without a bound; run sieve_analyze(\"htmt\", ..., conf_level = C, B = max(100, 10 / min(C, 1 - C)) or more, seed = ...) for a one-sided upper bound at level C"
      }
      sieve_interval_rows(
        term = paste(value$construct_1, value$construct_2, sep = " - "), estimate = value$htmt,
        lower = NA_real_, upper = value$ci_upper, conf_level = level, side = "upper",
        method = "One-sided percentile bootstrap upper confidence bound (DMAR)", kind = "one_sided",
        transformation = "none (the bound is a percentile of the replications used)",
        n_used = sieve_htmt_pair_n(args$data, args$blocks, value$construct_1, value$construct_2),
        B_used = meta$bootstrap$used %||% attr(value, "B_used", exact = TRUE) %||% NA_integer_,
        reason = ifelse(is.finite(value$ci_upper), NA_character_, missing_reason),
        rerun = rerun
      )
    })),
    screening = "The screening analysis holds no confidence limits: its tables are counts, flags, and thresholds of the response screens.",
    irt = sprintf(
      paste0(
        "The graded response model's table holds item parameters without standard errors or confidence limits. ",
        "The RMSEA interval of its fit (level %s) is in attr(result$result, \"fit_measures\")."
      ),
      sieve_confint_fmt(if (inherits(value, "dmar_tbl")) DMAR::dmar_metadata(value)$rmsea_conf_level %||% NA_real_ else NA_real_)
    ),
    sprintf("The %s analysis has no table of confidence limits that confint() reads.", analysis)
  )
}

#' The cases behind each HTMT bound
#'
#' HTMT's correlations are Pearson correlations on pairwise complete rows,
#' so a pair's bound rests on the rows observed on at least two of the two
#' constructs' items (the rows that enter at least one of its
#' correlations); with complete data, every row.
#'
#' @param data The data given to [DMAR::htmt()].
#' @param blocks Its named list of item names by construct.
#' @param first,second The constructs of each pair.
#' @return One count per pair, `NA` when the input cannot be read.
#' @author Ken Kelley
#' @noRd
sieve_htmt_pair_n <- function(data, blocks, first, second) {
  if (!is.data.frame(data) || !is.list(blocks) || is.null(names(blocks))) return(rep(NA_integer_, length(first)))
  vapply(seq_along(first), function(i) {
    columns <- unlist(blocks[c(first[[i]], second[[i]])], use.names = FALSE)
    if (!is.character(columns) || !all(columns %in% names(data))) return(NA_integer_)
    sum(rowSums(!is.na(data[, columns, drop = FALSE])) >= 2L)
  }, integer(1))
}

#' Other names a confint() table is known by
#'
#' The structural model's parameter table is also listed by `summary()`
#' under its place in the result.
#'
#' @param analysis The analysis name.
#' @return A named character vector, other name to the interval table.
#' @author Ken Kelley
#' @noRd
sieve_confint_aliases <- function(analysis) {
  if (identical(analysis, "sem")) c("Results: estimates: parameters" = "parameters") else character(0)
}

#' The interval descriptor of a headless analysis
#'
#' Every interval a `siever_analysis` holds, in one table, filled when the
#' object is built and read by `confint()`, `summary()`, and `print()`: one
#' row per interval of every table that holds limits, and one row with an
#' empty `term` and the `reason` for a table (or, with an empty `table`
#' too, the analysis) that has none. A table that cannot be read (a level
#' that is not recorded, two levels that disagree, a model without a
#' solution) is recorded the same way, so building the descriptor never
#' stops an analysis.
#'
#' @param object A `siever_analysis` object.
#' @param args The service's arguments (see `sieve_interval_registry()`).
#' @return A data frame with the columns `table`, `term`, `estimate`,
#'   `se`, `lower`, `upper`, `conf_level`, `side`, `method`, `kind`,
#'   `scale`, `df`, `transformation`, `n_used`, `B_used`, `recomputed`,
#'   `reason`, and `rerun`.
#' @author Ken Kelley
#' @noRd
sieve_analysis_intervals <- function(object, args = list()) {
  none <- function(reason, table = NA_character_) {
    cbind(
      table = table,
      sieve_interval_rows(
        term = NA_character_, lower = NA_real_, upper = NA_real_, conf_level = NA_real_,
        method = NA_character_, kind = NA_character_, side = NA_character_, df = NA_real_,
        reason = trimws(gsub("\\s+", " ", reason))
      ),
      stringsAsFactors = FALSE
    )
  }
  if (is.null(object$result)) {
    return(none(paste("This analysis has no estimates.", object$status$message %||% "")))
  }
  registry <- tryCatch(sieve_interval_registry(object, args), error = function(e) {
    paste("The interval tables could not be read:", conditionMessage(e))
  })
  if (is.character(registry)) return(none(registry))
  if (!length(registry)) {
    return(none(paste(
      sprintf("This %s analysis holds no table with confidence limits.", object$analysis),
      object$status$message %||% ""
    )))
  }
  rows <- lapply(registry, function(e) {
    built <- tryCatch(e$build(), error = function(err) conditionMessage(err))
    if (is.character(built)) return(none(built, e$name))
    if (!is.data.frame(built) || !nrow(built)) return(none("The table has no rows.", e$name))
    cbind(table = e$name, built, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' The interval descriptor an object carries, or one built from it
#'
#' A `siever_analysis` built by [sieve_analyze()] carries its descriptor; a
#' list given the class by hand is read from its result.
#'
#' @param object A `siever_analysis` object.
#' @return The descriptor.
#' @author Ken Kelley
#' @noRd
sieve_intervals_of <- function(object) {
  if (is.data.frame(object$intervals)) object$intervals else sieve_analysis_intervals(object)
}

#' The interval tables of a descriptor, one row each
#'
#' @param intervals An interval descriptor.
#' @return A data frame with `table`, `rows` (rows with limits), `conf_level`
#'   (the levels, as text), `side`, `method` (the methods, as text), and
#'   `reason` (why the table, or the analysis when `table` is `NA`, has no
#'   limits).
#' @author Ken Kelley
#' @noRd
sieve_intervals_overview <- function(intervals) {
  key <- ifelse(is.na(intervals$table), "", intervals$table)
  groups <- split(seq_len(nrow(intervals)), factor(key, levels = unique(key)))
  text <- function(x, sep = "; ") {
    x <- unique(x[!is.na(x)])
    if (length(x)) paste(x, collapse = sep) else NA_character_
  }
  out <- do.call(rbind, lapply(groups, function(i) {
    r <- intervals[i, , drop = FALSE]
    with <- r[!is.na(r$term) & is.na(r$reason), , drop = FALSE]
    data.frame(
      table = r$table[1L], rows = nrow(with),
      conf_level = text(vapply(with$conf_level, sieve_confint_fmt, character(1)), " and "),
      side = text(with$side, " and "), method = text(with$method),
      reason = if (nrow(with)) NA_character_ else text(r$reason),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

#' The interval table a confint() request names
#'
#' @param object A `siever_analysis` object.
#' @param intervals Its interval descriptor.
#' @param table `NULL` (the analysis's main interval table), one table
#'   name, or one table number from `summary()`.
#' @return A list with the table's `name` and its `rows` (the descriptor's
#'   rows of that table).
#' @author Ken Kelley
#' @noRd
sieve_confint_table <- function(object, intervals, table = NULL) {
  named <- intervals[!is.na(intervals$table), , drop = FALSE]
  if (!nrow(named)) {
    stop(intervals$reason[1L] %||% sprintf("This %s analysis holds no table with confidence limits.", object$analysis),
      call. = FALSE
    )
  }
  available <- unique(named$table)
  listing <- paste0("'", available, "'", collapse = ", ")
  name <- if (is.null(table)) {
    available[1L]
  } else {
    if (is.numeric(table)) {
      tables <- names(sieve_analysis_tables(object))
      if (length(table) != 1L || !is.finite(table) || table != round(table) || table < 1 || table > length(tables)) {
        stop(sprintf(
          "`table` must name one of the %d tables summary() lists, by name or by a number from 1 to %d; the tables with limits are %s.",
          length(tables), length(tables), listing
        ), call. = FALSE)
      }
      table <- tables[[table]]
    }
    if (!is.character(table) || length(table) != 1L || is.na(table)) {
      stop(sprintf("`table` must be NULL or one table name; the tables with limits are %s.", listing), call. = FALSE)
    }
    aliases <- sieve_confint_aliases(object$analysis)
    if (table %in% available) {
      table
    } else if (table %in% names(aliases) && aliases[[table]] %in% available) {
      aliases[[table]]
    } else if (table %in% names(sieve_analysis_tables(object))) {
      stop(sprintf("Table '%s' holds no confidence limits that confint() reads; the tables with limits are %s.", table, listing),
        call. = FALSE
      )
    } else {
      stop(sprintf("This %s analysis has no table '%s'; the tables with limits are %s.", object$analysis, table, listing),
        call. = FALSE
      )
    }
  }
  rows <- named[named$table == name, , drop = FALSE]
  if (all(is.na(rows$term))) stop(rows$reason[1L], call. = FALSE)
  rows <- rows[!is.na(rows$term), , drop = FALSE]
  # A one-sided bound is not a two-sided interval at any level; the
  # refusal names the column that holds it.
  one_sided <- !is.na(rows$side) & rows$side != "two-sided" & is.na(rows$reason)
  if (any(one_sided)) stop(unique(rows$rerun[one_sided])[1L], call. = FALSE)
  list(name = name, rows = rows)
}

#' The rows a confint() request selects
#'
#' @param rows Interval rows.
#' @param parm Row names or positions; missing for every row with limits.
#' @return The selected rows, each with limits.
#' @author Ken Kelley
#' @noRd
sieve_confint_select <- function(rows, parm) {
  with_limits <- rows[is.na(rows$reason), , drop = FALSE]
  if (missing(parm) || is.null(parm)) {
    if (!nrow(with_limits)) {
      reasons <- unique(rows$reason)
      stop(sprintf(
        "No row of this table has confidence limits (%s).",
        paste(utils::head(reasons, 3L), collapse = "; ")
      ), call. = FALSE)
    }
    return(with_limits)
  }
  available <- with_limits$term
  if (is.character(parm)) {
    unknown <- setdiff(parm, rows$term)
    if (length(unknown)) {
      stop(sprintf(
        "Unknown parameter(s): %s. Available: %s%s.",
        paste(unknown, collapse = ", "), paste(utils::head(available, 10L), collapse = ", "),
        if (length(available) > 10L) sprintf(" (first 10 of %d)", length(available)) else ""
      ), call. = FALSE)
    }
    picked <- rows[match(parm, rows$term), , drop = FALSE]
    without <- picked[!is.na(picked$reason), , drop = FALSE]
    if (nrow(without)) {
      stop(paste(sprintf("%s has no interval: %s.", without$term, without$reason), collapse = " "), call. = FALSE)
    }
    return(picked)
  }
  if (is.numeric(parm) && length(parm) && all(is.finite(parm)) && all(parm == round(parm))) {
    outside <- parm[parm < 1 | parm > length(available)]
    if (length(outside)) {
      stop(sprintf(
        "Positions %s are outside 1 to %d, the rows with limits in this table.",
        paste(outside, collapse = ", "), length(available)
      ), call. = FALSE)
    }
    return(with_limits[parm, , drop = FALSE])
  }
  stop("`parm` must give row names (as rownames(confint(x)) shows them) or row positions.", call. = FALSE)
}

#' Wald limits at a level, on the scale they are formed on
#'
#' The estimate plus and minus the normal (or t, when `df` is finite)
#' quantile times the standard error: on the scale of the estimate
#' (`"identity"`), clamped into the unit interval (`"unit"`), on the logit
#' scale with the delta method standard error SE / (x (1 - x)) and
#' transformed back (`"logit"`), or on the ln(1 - x) scale with
#' SE / (1 - x) (`"log_complement"`, Bonett's form for alpha).
#'
#' @param estimate,se,df Numeric vectors.
#' @param scale A character vector of scales.
#' @param level The confidence level.
#' @return A two-column matrix (`lower`, `upper`).
#' @author Ken Kelley
#' @noRd
sieve_wald_limits <- function(estimate, se, df, scale, level) {
  q <- ifelse(is.finite(df), stats::qt(1 - (1 - level) / 2, pmax(df, 1e-8)), stats::qnorm(1 - (1 - level) / 2))
  lower <- upper <- rep(NA_real_, length(estimate))
  clamp <- function(x) pmin(pmax(x, 0), 1)
  id <- scale %in% c("identity", "unit")
  lower[id] <- estimate[id] - q[id] * se[id]
  upper[id] <- estimate[id] + q[id] * se[id]
  unit <- scale %in% "unit"
  lower[unit] <- clamp(lower[unit])
  upper[unit] <- clamp(upper[unit])
  lg <- scale %in% "logit" & estimate > 0 & estimate < 1
  half <- q[lg] * se[lg] / (estimate[lg] * (1 - estimate[lg]))
  lower[lg] <- stats::plogis(stats::qlogis(estimate[lg]) - half)
  upper[lg] <- stats::plogis(stats::qlogis(estimate[lg]) + half)
  lc <- scale %in% "log_complement" & estimate < 1
  half <- q[lc] * se[lc] / (1 - estimate[lc])
  lower[lc] <- clamp(1 - exp(log(1 - estimate[lc]) + half))
  upper[lc] <- clamp(1 - exp(log(1 - estimate[lc]) - half))
  cbind(lower = lower, upper = upper)
}

#' Why a row's limits are not formed at another level
#'
#' @param rows Interval rows.
#' @return One sentence fragment per row.
#' @author Ken Kelley
#' @noRd
sieve_confint_why_not <- function(rows) {
  at <- sprintf(
    "%s, stored at level %s%s", rows$method, vapply(rows$conf_level, sieve_confint_fmt, character(1)),
    ifelse(is.finite(rows$B_used), sprintf(" (%d replications used)", rows$B_used), "")
  )
  because <- vapply(rows$kind, function(kind) {
    switch(kind,
      bootstrap = "; another level needs the replications, which are not stored",
      lavaan_bootstrap = "",
      profile = "; another level needs the model refit to invert the likelihood ratio test at that level",
      order_statistic = "; another level selects other order statistics, which the result does not keep",
      exact_binomial = "; DMAR forms it from the expert counts at the level it is asked for",
      noncentral = "; another level needs the noncentral chi-square distribution inverted again at that level",
      wald_without_se = ", without its standard error",
      "; confint() forms only Wald intervals at another level"
    )
  }, character(1), USE.NAMES = FALSE)
  paste0(at, because)
}

#' Row names in a message, the first few and a count
#'
#' @param terms Row names.
#' @return A string.
#' @author Ken Kelley
#' @noRd
sieve_confint_term_list <- function(terms) {
  if (length(terms) <= 6L) return(paste(terms, collapse = ", "))
  sprintf("%s, and %d more", paste(terms[1:5], collapse = ", "), length(terms) - 5L)
}

#' The limits of selected rows at the stored or a requested level
#'
#' With `level = NULL` every row's stored limits at its stored level. At
#' another level, a row stored at that level keeps its stored limits; a
#' Wald row is formed again from its estimate and standard error after its
#' stored limits are reproduced at the stored level; and any other row
#' refuses the whole request, naming each such row, the call that reruns
#' it, and the Wald rows `parm` can select instead.
#'
#' @param rows Selected interval rows.
#' @param level `NULL` or a checked level.
#' @param analysis,table The analysis and table names, kept as attributes.
#' @return A two-column matrix with its attributes.
#' @author Ken Kelley
#' @noRd
sieve_confint_limits <- function(rows, level, analysis, table) {
  n <- nrow(rows)
  limits <- cbind(lower = rows$lower, upper = rows$upper)
  source <- rep("stored", n)
  conf <- rows$conf_level
  if (!is.null(level)) {
    same <- abs(rows$conf_level - level) <= 1e-12
    wald <- rows$kind %in% sieve_confint_recomputable()
    need <- !same
    why <- rep(NA_character_, n)
    blocked <- need & !wald
    why[blocked] <- sieve_confint_why_not(rows[blocked, , drop = FALSE])
    check <- which(need & wald)
    if (length(check)) {
      r <- rows[check, , drop = FALSE]
      at <- sprintf("stored at level %s", vapply(r$conf_level, sieve_confint_fmt, character(1)))
      no_scale <- is.na(r$scale)
      no_se <- !no_scale & !(is.finite(r$estimate) & is.finite(r$se))
      again <- sieve_wald_limits(r$estimate, r$se, r$df, r$scale, r$conf_level)
      size <- pmax(1, abs(r$lower), abs(r$upper))
      gap <- pmax(abs(again[, "lower"] - r$lower), abs(again[, "upper"] - r$upper)) / size
      off <- !no_scale & !no_se & !(is.finite(gap) & gap <= 1e-10)
      why[check[no_scale]] <- sprintf("%s, %s; the scale it was formed on is not recorded", r$method[no_scale], at[no_scale])
      why[check[no_se]] <- sprintf("%s, %s, without a finite estimate and standard error", r$method[no_se], at[no_se])
      why[check[off]] <- sprintf(
        "%s, %s; its stored limits cannot be reproduced from its estimate and standard error (largest relative difference %s), so no other level is computed",
        r$method[off], at[off], format(signif(gap[off], 3))
      )
    }
    refused <- !is.na(why)
    if (any(refused)) {
      groups <- split(rows$term[refused], why[refused])
      said <- vapply(names(groups), function(w) sprintf("%s: %s.", sieve_confint_term_list(groups[[w]]), w), character(1))
      reruns <- unique(stats::na.omit(sieve_confint_fill_rerun(
        rows$rerun[refused], level,
        one_sided = !is.na(rows$side[refused]) & rows$side[refused] != "two-sided"
      )))
      computable <- rows$term[!refused & need]
      stop(paste(c(
        sprintf(
          "No limits are returned at level %s: %d of the %d rows requested are not formed at that level.",
          sieve_confint_fmt(level), sum(refused), n
        ),
        said, reruns,
        "With level = NULL, confint() returns every stored interval at its own level.",
        if (length(computable)) {
          sprintf("Select the rows that can be formed at %s with parm (%s).", sieve_confint_fmt(level), sieve_confint_term_list(computable))
        }
      ), collapse = " "), call. = FALSE)
    }
    redo <- which(need)
    if (length(redo)) {
      limits[redo, ] <- sieve_wald_limits(rows$estimate[redo], rows$se[redo], rows$df[redo], rows$scale[redo], level)
      source[redo] <- "computed from the stored estimate and standard error"
    }
    conf <- rep(level, n)
  }
  rownames(limits) <- rows$term
  one <- function(x) {
    if (length(unique(x)) == 1L) x[[1L]] else stats::setNames(x, rows$term)
  }
  attr(limits, "conf_level") <- one(conf)
  attr(limits, "interval_method") <- one(rows$method)
  attr(limits, "source") <- one(source)
  attr(limits, "recomputed") <- one(source != "stored")
  attr(limits, "transformation") <- one(rows$transformation)
  attr(limits, "analysis") <- analysis
  attr(limits, "table") <- table
  limits
}

#' Reproducible Scoring Syntax With Item-Specific Transformations
#'
#' @description
#' Writes standalone R that reproduces each construct's operational score
#' from item data: the item reversals with their stated bounds, the item
#' weights, the scoring method and the minimum-answered rule of each
#' construct, so that the scores can be computed outside SIEVER. Where a
#' stored policy cannot be followed as saved, the repair applied is named in
#' `issues`.
#'
#' @details
#' The generated code expects a data frame named `dat`. It scores active items
#' in assigned constructs. With raw input, known item-specific bounds are
#' required for each reversal. Supply matched `raw` and `processed` frames to
#' distinguish changes made during the session from values already reversed
#' before loading; without them the ledger's reverse flags define the changes.
#' An inconsistent transformation or unknown required bounds is refused.
#' Scoring preserves missingness rules and numeric precision, and does not
#' estimate reliability or establish score validity.
#' @param meta Ledger with `item`, `construct`, `status`, and `reverse`, plus
#'   `rev_lo` and `rev_hi` for reversals that must be applied.
#' @param values Whether dat holds raw or already processed item values.
#' @param policies Named construct policies: the score `method` (`"mean"`,
#'   the default, `"sum"`, `"bartlett"`, or `"regression"`), `min_count`,
#'   `min_prop`, and optional nonnegative item `weights`, as
#'   [sieve_composite_score()] takes them. A factor score is written as a
#'   read of the correlated-factors model specified from the ledger, which
#'   the syntax fits first.
#' @param raw,processed Optional matched loaded and analytic item values,
#'   used to distinguish data received already reversed from reversals made here.
#' @details A stored policy that cannot be used as saved (a method this
#'   version of SIEVER does not know, or an invalid minimum answered rule) is
#'   scored as the mean of the answered items, with the default minimums, so
#'   that a project is never refused for it; the code then says so. Its
#'   header is marked provisional, each such construct is preceded by a
#'   `NEEDS REVIEW` comment naming the method as stored and the repair in
#'   force, and the repairs are returned in `issues`. A policy that records
#'   no method is the mean of the answered items and is not flagged.
#' @return A list with `code`, `name_map` (construct to score column), and
#'   `issues` (the repairs in force, named by construct; empty when the code
#'   follows every stored policy as saved).
#' @author Ken Kelley
#' @examples
#' meta <- data.frame(item = paste0("x", 1:9), label = paste0("x", 1:9),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   status = "active", reverse = FALSE, stringsAsFactors = FALSE)
#' code <- sieve_scoring_code(meta, values = "processed")
#' cat(code$code, sep = "\n")
#' @export
sieve_scoring_code <- function(meta, values = c("raw", "processed"), policies = list(),
                               raw = NULL, processed = NULL) {
  values <- match.arg(values)
  active <- meta[meta$status == "active", , drop = FALSE]
  cons <- unique(as.character(active$construct))
  cons <- cons[sieve_construct_assigned(cons)]
  allocated <- sieve_allocate_names(paste0("score_", cons), taken = meta$item, prefix = "Score")
  map <- stats::setNames(unname(allocated), cons)
  quote_r <- function(x) encodeString(as.character(x), quote = '"')
  one_line <- function(x) gsub("[\r\n]+", " ", as.character(x))
  # One score specification per construct: a composite is scored in place;
  # a factor score is read from the correlated-factors model specified from
  # the ledger, which is fit first. A stored policy repaired on reading is
  # said in the code itself.
  spec <- sieve_score_spec_build(active, policies)
  # The constructs whose reliability is categorical omega, whose target the
  # header states (every included item ordered or binary).
  categorical <- if ("type" %in% names(active)) {
    Filter(function(cn) all(active$type[active$construct %in% cn] %in% c("ordered", "binary")), cons)
  } else {
    character(0)
  }
  issues <- unlist(lapply(spec, function(s) s$issue))
  issues <- if (length(issues)) issues else stats::setNames(character(0), character(0))
  lines <- c(
    "## SIEVER scoring syntax", sprintf("## Input contract: dat holds %s values.", values),
    if (length(issues)) {
      c(
        "## PROVISIONAL: a stored scoring policy could not be used as saved, and the code below scores it with",
        "## the repair named here until a method is kept. Review before operational use.",
        sprintf("## NEEDS REVIEW, %s: %s", one_line(names(issues)), one_line(issues))
      )
    } else {
      "## Scores follow the declared minimum-response and weighting rules below."
    },
    "## Available-item means can differ in content across respondents.",
    "## Reliability for a complete unit-weighted score does not by itself describe that missing-response policy.",
    if (length(categorical)) {
      c(
        sprintf("## Categorical omega (%s: every item ordered or binary) is the reliability of the sum of category", one_line(paste(categorical, collapse = ", "))),
        "## ranks, each item's observed categories scored 0, 1, ..., C - 1 in the order of its codes (DMAR). The scores",
        "## below use the codes, which have that reliability only when every item's codes step by one shared amount;",
        "## the Reliability tab's coding note names any item whose codes do not and gives the coded composite's value."
      )
    },
    # Written with encodeString(), which escapes every character a construct
    # name can hold; dput() leaves a backslash in a backticked name as is,
    # and the code would not parse.
    sprintf(
      "score_name_map <- stats::setNames(c(%s), c(%s))",
      paste(quote_r(unname(map)), collapse = ", "), paste(quote_r(names(map)), collapse = ", ")
    ), ""
  )
  if (identical(values, "raw")) {
    reverse_now <- active$reverse %in% TRUE
    if (!is.null(raw) && !is.null(processed)) {
      if (nrow(raw) != nrow(processed)) stop("Loaded and processed scoring values must have matched rows.",
        call. = FALSE
      )
      reverse_now <- vapply(active$item, function(it) {
        if (!it %in% names(raw) || !it %in% names(processed)) stop("Scoring inputs must hold every active item.",
          call. = FALSE
        )
        !identical(sieve_values_key(raw[[it]]), sieve_values_key(processed[[it]]))
      }, logical(1))
    }
    reversed <- active[reverse_now, , drop = FALSE]
    for (i in seq_len(nrow(reversed))) {
      row <- reversed[i, , drop = FALSE]
      lo <- row$rev_lo
      hi <- row$rev_hi
      if (length(lo) != 1L || length(hi) != 1L || !is.finite(lo) || !is.finite(hi)) {
        stop(
          sprintf(
            "Response bounds for reverse-scored item %s are unknown. Declare its bounds before exporting raw-input scoring syntax.",
            row$item
          ),
          call. = FALSE
        )
      }
      if (!is.null(raw) && !is.null(processed) &&
        !identical(
          sieve_values_key((lo + hi) - as.numeric(raw[[row$item]])),
          sieve_values_key(processed[[row$item]])
        )) {
        stop(
          sprintf(
            "The processed values of %s are not the reflection of its loaded values within the recorded bounds.",
            row$item
          ),
          call. = FALSE
        )
      }
      lines <- c(lines, sprintf(
        "dat[[%s]] <- (%s + %s) - dat[[%s]]  # %s bounds",
        quote_r(row$item), sieve_r_literal(lo), sieve_r_literal(hi), quote_r(row$item), row$rev_bounds %||% "stored"
      ))
    }
  }
  latent <- names(Filter(function(s) isTRUE(s$latent), spec))
  name_map <- NULL
  if (length(latent)) {
    cfa <- tryCatch(sieve_cfa_spec(meta, reserved = meta$item), error = function(e) e)
    if (inherits(cfa, "error")) {
      stop(sprintf(
        "The factor scores specified for %s need the correlated-factors model, which cannot be specified from the ledger: %s",
        paste(latent, collapse = ", "), conditionMessage(cfa)
      ), call. = FALSE)
    }
    name_map <- cfa$name_map
    missing_latent <- setdiff(latent, unname(name_map))
    if (length(missing_latent)) {
      stop(sprintf(
        "%s specified as a factor score, but the correlated-factors model specified from the ledger does not model %s (a construct needs two or more included items). Choose a composite score for %s, or include more items.",
        paste(missing_latent, collapse = ", "), if (length(missing_latent) == 1L) "it" else "them",
        if (length(missing_latent) == 1L) "it" else "them"
      ), call. = FALSE)
    }
    lines <- c(
      lines,
      "## The correlated-factors measurement model the factor scores are read from, specified from the ledger.",
      sieve_cfa_code(cfa, fit_name = "score_fit", model_name = "score_model", data_name = "dat"), ""
    )
  }
  for (cn in cons) {
    s <- spec[[cn]]
    its <- s$items
    required <- s$required
    weights <- s$weights %||% stats::setNames(rep(1, length(its)), its)
    if (!is.null(names(weights))) weights <- weights[its]
    if (length(weights) != length(its) || any(!is.finite(weights)) || any(weights < 0) || !any(weights > 0)) stop("Invalid scoring weights.",
      call. = FALSE
    )
    if (isTRUE(s$latent)) {
      engine <- if (identical(s$method, "bartlett")) "Bartlett" else "regression"
      lines <- c(
        lines,
        sprintf(
          "# %s (%d items; %s from score_fit; at least %d answered)",
          gsub("[\r\n]", " ", cn), length(its), s$label, required
        ),
        if (!is.null(s$issue)) sprintf("# NEEDS REVIEW: %s", one_line(s$issue)),
        sprintf(".fs <- SIEVER::sieve_factor_score_record(score_fit, method = %s, data = dat)$scores", quote_r(engine)),
        sprintf(".score <- .fs[[%s]]", quote_r(names(name_map)[match(cn, unname(name_map))])),
        sprintf(
          ".score[rowSums(!is.na(dat[, c(%s), drop = FALSE])) < %d | !is.finite(.score)] <- NA_real_",
          paste(quote_r(its), collapse = ", "), required
        ),
        sprintf("dat[[%s]] <- .score", quote_r(map[[cn]])), ""
      )
      next
    }
    lines <- c(
      lines, sprintf(
        "# %s (%d items; %s; at least %d answered)",
        gsub("[\r\n]", " ", cn), length(its), s$label, required
      ),
      if (!is.null(s$issue)) sprintf("# NEEDS REVIEW: %s", one_line(s$issue)),
      sprintf(".x <- as.matrix(dat[, c(%s), drop = FALSE])", paste(quote_r(its), collapse = ", ")),
      paste0(".w <- ", paste(utils::capture.output(dput(as.numeric(weights))), collapse = "\n")),
      ".seen <- !is.na(.x)", ".denom <- as.vector(.seen %*% .w)", ".x[!.seen] <- 0",
      ".score <- as.vector(.x %*% .w) / .denom",
      if (identical(s$method, "sum")) ".score <- .score * sum(.w)  # prorated to the sum over every item",
      sprintf(".score[rowSums(.seen) < %d | !is.finite(.score)] <- NA_real_", required),
      sprintf("dat[[%s]] <- .score", quote_r(map[[cn]])), ""
    )
  }
  list(code = paste(lines, collapse = "\n"), name_map = map, issues = issues)
}

#' Resolved split reliability with the same target as the Reliability tab
#'
#' The coefficient of each construct in one half of a split, resolved as on
#' the Reliability tab: Robust omega (observed denominator), for ordered or
#' binary items built from the unrestricted polychoric correlations, or Omega
#' (model-implied denominator) when the Reliability tab is set to it. Complete
#' cases are used (the split is drawn from complete rows). The target's point
#' estimate is returned; robust and categorical omega have only bootstrap
#' intervals, which are not run for the split. The interval each half carries
#' is DMAR's default interval of model implied omega (the MLR (sandwich) SE
#' with logit-scale limits), which assumes the one-factor model holds: beside
#' robust omega as its companion, or of the target itself when the target is
#' model implied omega. The halves are complete rows, so
#' `missing = "listwise"` is the analysis DMAR's default gives complete data.
#'
#' @param data Sample item data.
#' @param meta Item ledger.
#' @param estimator Resolved study/model estimator choice; auto follows item types.
#' @param denominator The Reliability tab's omega variant, `"observed"`
#'   (robust omega, the default) or `"model_implied"`.
#' @param conf_level Confidence level of the intervals.
#' @return A named vector of the target's estimates, with the attributes
#'   `targets` (the coefficient labels), `residuals` (the largest
#'   single-factor residual correlation and its item pair, as text), and
#'   `intervals` (a data frame, one
#'   row per construct: `construct`, `coefficient`, `estimate`, `se`,
#'   `lower`, `upper`, `interval`, `status`; `NA` where no interval applies).
#' @author Ken Kelley
#' @noRd
sieve_split_omega <- function(data, meta, estimator = "auto", denominator = "observed",
                              conf_level = 0.95) {
  denominator <- match.arg(denominator, c("observed", "model_implied"))
  groups <- sieve_reliability_items(meta, names(data))
  treatment <- switch(estimator,
    mlr = ,
    ml_fiml = ,
    ml = "continuous",
    wlsmv = "ordered",
    "auto"
  )
  groups <- lapply(groups, function(g) {
    g$types <- sieve_reliability_treat(g$types, treatment)
    g
  })
  specs <- lapply(groups, function(g) {
    closed <- identical(denominator, "model_implied") &&
      !identical(sieve_reliability_class(g$types), "categorical")
    sieve_reliability_spec(
      items = g$items,
      types = g$types,
      construct = g$construct,
      missing = "listwise",
      ci_method = if (closed) "recommended" else "none",
      denominator = denominator,
      conf_level = conf_level
    )
  })
  labels <- vapply(specs, function(sp) sp$label, character(1))
  runs <- lapply(specs, function(spec) {
    r <- sieve_reliability_omega(data, spec)
    interval <- if (identical(spec$coefficient, "omega")) r else sieve_reliability_model_implied(data, spec)
    # The largest single-factor residual correlation in this half, beside
    # the coefficient.
    residuals <- sieve_reliability_residuals(data, spec, given = r$engine_residual_cor)
    list(target = r, interval = interval, residuals = residuals)
  })
  out <- vapply(runs, function(x) x$target$estimate, numeric(1))
  names(out) <- names(groups)
  attr(out, "targets") <- stats::setNames(labels, names(groups))
  attr(out, "residuals") <- stats::setNames(
    vapply(runs, function(x) sieve_reliability_residual_text(x$residuals), character(1)),
    names(groups)
  )
  attr(out, "intervals") <- do.call(rbind, lapply(seq_along(runs), function(i) {
    iv <- runs[[i]]$interval
    data.frame(
      construct = names(groups)[i],
      coefficient = if (is.null(iv)) NA_character_ else iv$label,
      estimate = if (is.null(iv)) NA_real_ else iv$estimate,
      se = if (is.null(iv)) NA_real_ else iv$se,
      lower = if (is.null(iv)) NA_real_ else iv$lower,
      upper = if (is.null(iv)) NA_real_ else iv$upper,
      interval = if (is.null(iv)) {
        "not applicable (the model-implied companion is not computed for ordered items; their interval is a bootstrap)"
      } else {
        sieve_reliability_interval_label(iv)
      },
      status = if (is.null(iv)) NA_character_ else iv$status$code,
      stringsAsFactors = FALSE
    )
  }))
  out
}

#' The split reliability comparison table
#'
#' One row per construct and coefficient: the Reliability tab's target in
#' each half (a point estimate when its interval is a bootstrap), and, where
#' it applies, model implied omega with its MLR (sandwich) SE interval in each half
#' and the difference between the halves with its delta method interval.
#' The halves are disjoint samples, so the variance of the difference is the
#' sum of the two variances.
#'
#' @param om_cal,om_val Results of `sieve_split_omega()` for the two halves.
#' @param conf_level Confidence level.
#' @return A data frame with `Construct`, `Coefficient`, `Calibration`,
#'   `Calibration.lower`, `Calibration.upper`, `Validation`,
#'   `Validation.lower`, `Validation.upper`, `Difference`,
#'   `Difference.lower`, `Difference.upper`, `Interval`, and
#'   `Calibration.residual` and `Validation.residual` (the largest
#'   single-factor residual correlation in each half and its item pair, on
#'   the target's row), at full precision.
#' @author Ken Kelley
#' @noRd
sieve_split_reliability_table <- function(om_cal, om_val, conf_level = 0.95) {
  cons <- names(om_cal)
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  targets <- attr(om_cal, "targets")
  ic <- attr(om_cal, "intervals")
  iv <- attr(om_val, "intervals")
  residual_of <- function(om, cn) {
    r <- attr(om, "residuals")
    if (is.null(r) || is.na(r[cn])) NA_character_ else unname(r[cn])
  }
  rows <- lapply(cons, function(cn) {
    a <- ic[ic$construct == cn, , drop = FALSE]
    b <- iv[iv$construct == cn, , drop = FALSE]
    same <- nrow(a) == 1L && !is.na(a$coefficient) && identical(a$coefficient, targets[[cn]])
    target <- data.frame(
      Construct = cn, Coefficient = targets[[cn]],
      Calibration = unname(om_cal[cn]),
      Calibration.lower = if (same) a$lower else NA_real_,
      Calibration.upper = if (same) a$upper else NA_real_,
      Validation = unname(om_val[cn]),
      Validation.lower = if (same) b$lower else NA_real_,
      Validation.upper = if (same) b$upper else NA_real_,
      Difference = unname(om_val[cn] - om_cal[cn]),
      Difference.lower = NA_real_, Difference.upper = NA_real_,
      Interval = if (same) a$interval else "point estimates; its interval is a bootstrap, not run for the split",
      Calibration.residual = residual_of(om_cal, cn),
      Validation.residual = residual_of(om_val, cn),
      stringsAsFactors = FALSE
    )
    companion <- if (!same && nrow(a) == 1L && !is.na(a$coefficient)) {
      data.frame(
        Construct = cn, Coefficient = a$coefficient,
        Calibration = a$estimate, Calibration.lower = a$lower, Calibration.upper = a$upper,
        Validation = b$estimate, Validation.lower = b$lower, Validation.upper = b$upper,
        Difference = b$estimate - a$estimate,
        Difference.lower = NA_real_, Difference.upper = NA_real_,
        Interval = a$interval,
        Calibration.residual = NA_character_, Validation.residual = NA_character_,
        stringsAsFactors = FALSE
      )
    }
    out <- rbind(target, companion)
    # The difference interval of the row that has standard errors in both
    # halves, by the delta method for independent samples.
    with_se <- if (same) 1L else if (!is.null(companion)) 2L else integer(0)
    if (length(with_se) && all(is.finite(c(a$se, b$se)))) {
      half <- z * sqrt(a$se^2 + b$se^2)
      out$Difference.lower[with_se] <- out$Difference[with_se] - half
      out$Difference.upper[with_se] <- out$Difference[with_se] + half
    }
    out
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Sample-Bound Norms for Every Included Construct
#'
#' @description
#' Scores every included construct under its scoring policy and describes
#' the scores of the sample at hand: the raw-score summary, T scores
#' standardized on this sample, percentile ranks with their uncertainty,
#' and, for a construct that cannot be normed, the reason. The norms belong
#' to this sample; to carry them to new respondents, freeze them with
#' [sieve_norm_freeze()] and apply them with [sieve_norm_apply()].
#'
#' @details
#' Each construct is scored by [sieve_composite_score()] under its policy
#' (the mean of the answered items by default, or the prorated sum). A
#' construct whose operational score is a factor score is not a composite
#' and is refused by name: the sample reference table and the frozen
#' references describe composite scores only.
#' @param data Already scored values.
#' @param meta Item ledger.
#' @param policies Named per-construct scoring policies (see
#'   [sieve_scoring_code()]).
#' A stored policy that could not be used as saved (see
#' [sieve_scoring_code()]) is scored as repaired and said: the table then
#' has a `review` column, and the repairs are returned in `review`.
#' @return Table, scores, T scores, refusals, percentile uncertainty, the
#'   scoring `definition` of each construct, and `review` (the repairs in
#'   force, named by construct; empty when every policy is used as saved).
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' norms <- sieve_construct_norms(items, meta)
#' norms$table
#' @export
sieve_construct_norms <- function(data, meta, policies = list()) {
  definition <- sieve_norm_definition(meta, policies)
  parts <- lapply(names(definition), function(cn) {
    def <- definition[[cn]]
    method <- def$method %||% "mean"
    if (sieve_score_is_latent(method)) {
      return(list(refused = sprintf(
        paste0(
          "'%s' is not scored here: its operational score is the %s, read from the fitted measurement ",
          "model (Reliability tab, Factor scores), not a composite. The sample reference table and frozen ",
          "references describe composite scores (the mean of the answered items or the prorated sum); choose ",
          "one of those for it (Reliability tab, Operational scores) to include it."
        ),
        cn, unname(sieve_score_methods()[method])
      )))
    }
    run <- tryCatch(
      sieve_composite_score(
        data,
        def$items,
        def$weights,
        method,
        def$min_count,
        def$min_prop
      ),
      error = function(e) e
    )
    if (inherits(run, "error")) return(list(refused = sprintf("'%s' is not scored. %s", cn, conditionMessage(run))))
    score <- run$scores
    sc <- score[is.finite(score)]
    if (length(sc) < 10L) return(list(refused = sprintf(
      "'%s' has %d eligible scores; the descriptive table needs at least ten. This is a convenience minimum, not evidence of precision.",
      cn,
      length(sc)
    )))
    probs <- c(.05, .10, .25, .50, .75, .90, .95)
    qs <- stats::quantile(sc, probs, names = FALSE, type = 7)
    out <- data.frame(
      construct = cn,
      k = length(def$items),
      n = length(sc),
      mean = mean(sc),
      sd = stats::sd(sc),
      stringsAsFactors = FALSE
    )
    for (j in seq_along(probs)) out[[sprintf("p%02d", round(100 * probs[j]))]] <- qs[j]
    T <- rep(NA_real_, length(score))
    if (is.finite(out$sd) && out$sd > 0) T <- 50 + 10 * (score - out$mean) / out$sd
    ci <- sieve_quantile_limits(sc, probs)
    ci$construct <- cn
    list(row = out, T = T, scores = score, quantiles = ci, construct = cn)
  })
  names(parts) <- names(definition)
  valid <- Filter(function(p) !is.null(p$row), parts)
  # A policy repaired on reading is scored as repaired and said: the table
  # gains a review column, and the repairs are returned in review.
  review <- unlist(lapply(definition, function(d) d$review))
  table <- if (length(valid)) do.call(rbind, lapply(valid, `[[`, "row")) else NULL
  if (!is.null(table) && length(review)) table$review <- unname(review[table$construct])
  list(
    table = table,
    refused = unlist(lapply(parts, `[[`, "refused")),
    T = lapply(valid, `[[`, "T"), scores = lapply(valid, `[[`, "scores"),
    uncertainty = if (length(valid)) do.call(rbind, lapply(valid, `[[`, "quantiles")) else NULL,
    definition = definition,
    review = if (length(review)) review else stats::setNames(character(0), character(0))
  )
}

#' The sample reference table, with its provenance, as it is downloaded
#'
#' The construct means, standard deviations, and percentiles of the current
#' sample describe that sample only. They are a sample reference table, not
#' norms: norms are a frozen reference with a stated population
#' (`sieve_norm_freeze()`). The download therefore carries, on every row,
#' what the table is, the rows it rests on, the data set, when it was
#' computed, and the SIEVER version.
#'
#' @param table The `table` of [sieve_construct_norms()].
#' @param rows The number of rows of the sample.
#' @param dataset The data set's name, or `NA`.
#' @param when The time of computation.
#' @return `table` with the columns `reference`, `sample_rows`, `dataset`,
#'   `computed`, and `siever_version` added.
#' @author Ken Kelley
#' @noRd
sieve_sample_reference_table <- function(table, rows, dataset = NA_character_, when = Sys.time()) {
  out <- table
  out$reference <- paste(
    "Sample reference table: means, SDs, and percentiles of this sample only (n is each",
    "construct's scored respondents); not population norms, which need a frozen reference",
    "with a stated population."
  )
  out$sample_rows <- as.integer(rows)
  out$dataset <- if (length(dataset) == 1L && !is.null(dataset)) as.character(dataset) else NA_character_
  out$computed <- format(when, "%Y-%m-%d %H:%M:%S %Z")
  out$siever_version <- sieve_version()
  out
}

# ---- One operational score per construct ----------------------------------
# One operational score specification per construct (its items, the score,
# and the minimum answered count or proportion) governs the Validity tab's criterion
# correlations and marker adjustment, the sample reference table, frozen
# references, the scoring syntax, and the reproducible code. The
# specification is stored per construct in state$scoring_policies, the
# field saved projects and replay carry: the score `method` beside the
# minimum-response rule. A policy with no method is read as the mean of
# the answered items.

#' The operational scores a construct can be given
#'
#' @return A named character vector: the method codes as names, their
#'   labels as values. The composites (`mean`, `sum`) are formed by
#'   [sieve_composite_score()]; the factor scores (`bartlett`,
#'   `regression`) come from the fitted correlated-factors model.
#' @author Ken Kelley
#' @noRd
sieve_score_methods <- function() {
  c(
    mean = "mean of the answered items",
    sum = "prorated sum of the answered items",
    bartlett = "Bartlett factor score",
    regression = "regression factor score"
  )
}

#' Is an operational score a factor score from the fitted model?
#'
#' @param method A method code from `sieve_score_methods()`.
#' @return `TRUE` for the Bartlett and regression factor scores.
#' @author Ken Kelley
#' @noRd
sieve_score_is_latent <- function(method) {
  method %in% c("bartlett", "regression")
}

#' Composite Scores Under an Operational Missing-Response Policy
#'
#' @description
#' Scores a set of items as an available-response composite: the weighted
#' mean of the items a respondent answered (`method = "mean"`), or that
#' mean prorated to the sum the respondent would have had with every item
#' answered (`method = "sum"`; proration, which is person-mean
#' substitution for the items not answered, Mazza, Enders, & Ruehlman,
#' 2015). A respondent is eligible for a score when the number of items
#' answered is at least `min_count` and at least `min_prop` of the items;
#' otherwise the score is `NA`. Nothing is imputed across respondents, and
#' the values are scored as supplied (a reversal is applied before scoring,
#' not here).
#'
#' @details
#' Writing \eqn{w_i} for the weight of item \eqn{i} and \eqn{A} for the set
#' of items a respondent answered, the mean score is
#' \eqn{\sum_{i \in A} w_i x_i / \sum_{i \in A} w_i} and the prorated sum
#' is that mean times \eqn{\sum_i w_i} over every item. With unit weights
#' and complete responses they are the ordinary mean and sum. The number
#' of answered items required is
#' `max(min_count, ceiling(length(items) * min_prop))`.
#'
#' A response is a finite number or missing. `NaN` (read from `NaN` or
#' `nan` in a text file) is a missing response. An infinite response
#' (`Inf`, `-Inf`) is refused with the item and its rows named, since it
#' is not a missing-data code but the trace of a division by zero or an
#' overflow; set it to missing or correct it before scoring.
#'
#' An available-response composite is an operational score: respondents
#' who answered different items are scored on different content, and a
#' reliability coefficient estimated for the complete composite does not
#' describe this partial-response procedure. Proration is a reasonable
#' approximation when the items are parallel and few are missing, and it
#' is biased when they are not (Mazza et al., 2015); the model-based
#' alternative that uses the observed items without proration is a factor
#' score estimated under full information maximum likelihood (Estabrook &
#' Neale, 2013), which SIEVER's score specification offers beside the
#' composites.
#'
#' @param data A data frame holding the item columns.
#' @param items The item columns to score: distinct names in `data`.
#' @param weights Optional nonnegative weights, one per item (named by item
#'   or in the order of `items`); unit weights by default.
#' @param method `"mean"` (the default) or `"sum"` (prorated).
#' @param min_count The minimum number of answered items for a score.
#' @param min_prop The minimum proportion of the items answered, in
#'   \eqn{[0, 1]}.
#' @return A list with `scores` (one per row of `data`, `NA` when the row
#'   is not eligible), `eligible` (logical: a score was formed, that is, the
#'   row meets the rule and its score is a finite number, so
#'   `eligible` is `is.finite(scores)`), `answered` (the number of items
#'   answered with a finite response), `reason` (`NA` for a scored row;
#'   otherwise why it was not scored: "answered 1 of 3 items; 2 required",
#'   "every item answered has weight 0", or "the weighted sum of the
#'   responses exceeds double precision"), and
#'   `policy` (`items`, `method`, `min_count`, `min_prop`, `required`,
#'   `weights`, and `missing`, a sentence stating the rule), all at full
#'   precision.
#' @references
#' Estabrook, R., & Neale, M. (2013). A comparison of factor score
#'   estimation methods in the presence of missing data: Reliability and an
#'   application to nicotine dependence. *Multivariate Behavioral Research,
#'   48*(1), 1--27.
#'
#' Mazza, G. L., Enders, C. K., & Ruehlman, L. S. (2015). Addressing
#'   item-level missing data: A comparison of proration and full information
#'   maximum likelihood estimation. *Multivariate Behavioral Research,
#'   50*(5), 504--519.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' items$x2[1:5] <- NA
#' items$x3[1:2] <- NA
#' # A Visual score for every respondent who answered at least two of its items.
#' visual <- sieve_composite_score(items, c("x1", "x2", "x3"), min_count = 2L)
#' table(answered = visual$answered, scored = visual$eligible)
#' head(visual$scores)
#' # The prorated sum under the same policy, and a weighted mean.
#' head(sieve_composite_score(items, c("x1", "x2", "x3"), method = "sum", min_count = 2L)$scores)
#' head(sieve_composite_score(items, c("x1", "x2", "x3"), weights = c(x1 = 2, x2 = 1, x3 = 1))$scores)
#' @export
sieve_composite_score <- function(data, items, weights = NULL, method = c("mean", "sum"),
                                  min_count = 1L, min_prop = 0) {
  method <- match.arg(method)
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!length(items) || anyDuplicated(items) || !all(items %in% names(data))) stop("`items` must identify distinct data columns.",
    call. = FALSE
  )
  if (length(min_count) != 1L || !is.finite(min_count) || min_count < 1 || min_count != as.integer(min_count)) stop("`min_count` must be a positive integer.",
    call. = FALSE
  )
  if (length(min_prop) != 1L || !is.finite(min_prop) || min_prop < 0 || min_prop > 1) stop("`min_prop` must lie in [0, 1].",
    call. = FALSE
  )
  read <- sieve_numeric_columns(data, items, purpose = "score")
  invalid <- read$problems$kind %in% c("not_numeric", "not_finite")
  if (any(invalid)) stop(paste(read$text[invalid], collapse = " "), call. = FALSE)
  X <- as.matrix(read$data)
  if (is.null(weights)) weights <- stats::setNames(rep(1, length(items)), items)
  if (!is.null(names(weights))) weights <- weights[items]
  if (length(weights) != length(items) || any(!is.finite(weights)) || any(weights < 0) || !any(weights > 0)) stop("`weights` must be finite, nonnegative, and cover the included items.",
    call. = FALSE
  )
  if (!is.finite(sum(weights))) {
    stop("`weights` must have a finite sum; these sum beyond double precision.", call. = FALSE)
  }
  # NaN is a missing response (the reader returns it as NA); an infinite
  # value was refused above.
  seen <- !is.na(X)
  answered <- rowSums(seen)
  needed <- as.integer(max(as.integer(min_count), ceiling(length(items) * min_prop)))
  denom <- as.vector(seen %*% weights)
  X[!seen] <- 0
  scores <- as.vector(X %*% weights) / denom
  if (identical(method, "sum")) scores <- scores * sum(weights)
  rule <- answered >= needed & denom > 0
  # Eligible means a score was formed: the rule is met and the score is a
  # finite number. Finite responses can still overflow (two responses of
  # 1e308 sum beyond double precision), and such a row is not scored and
  # says why.
  eligible <- rule & is.finite(scores)
  scores[!eligible] <- NA_real_
  reason <- rep(NA_character_, length(scores))
  short <- answered < needed
  reason[short] <- sprintf("answered %d of %d items; %d required", as.integer(answered[short]), length(items), needed)
  reason[!short & !(denom > 0)] <- "every item answered has weight 0"
  reason[rule & !eligible] <- "the weighted sum of the responses exceeds double precision"
  list(
    scores = scores, answered = answered, eligible = eligible, reason = reason,
    policy = list(
      items = items, method = method, min_count = as.integer(min_count), min_prop = min_prop,
      required = needed, weights = stats::setNames(as.numeric(weights), items),
      missing = if (identical(method, "sum")) {
        "available-response weighted mean prorated to the sum over every item; all missing or insufficient response = NA"
      } else {
        "available-response weighted mean; all missing or insufficient response = NA"
      }
    )
  )
}

#' Read a stored construct scoring policy
#'
#' A policy in `state$scoring_policies` that holds only the minimum-response
#' rule (`min_count`, `min_prop`, optional `weights`) is read as the mean of
#' the answered items. A stored method that is unknown or malformed (a typing
#' error in a file, or a method of a later version of SIEVER) and an
#' invalid minimum answered rule are read as the mean and the defaults as
#' well, so a saved project is never refused for its scoring policy alone,
#' but never silently: the repair is returned in `issue`, which the
#' Operational scores card and the Ingest tab (when a project is opened)
#' show for review.
#'
#' The fields are read by exact name. A stored value that is not a list,
#' a field that is not part of a policy (anything but `method`, `min_count`,
#' `min_prop`, and `weights`), a fractional minimum count, and a method that
#' is not a single name are flagged in `issue` as well.
#'
#' @param p The stored policy (a list), or `NULL`.
#' @param items The construct's items, used to order named weights.
#' @return A list with `method` (the method in force), `requested` (the
#'   method as stored, as text, or `NA` when none was stored), `min_count`,
#'   `min_prop`, `weights` (`NULL` for unit weights), and `issue` (one
#'   sentence naming each stored value that was replaced, or `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_score_policy_read <- function(p, items = NULL) {
  shown <- function(x) {
    tryCatch(sprintf("\"%s\"", paste(as.character(unlist(x)), collapse = ", ")),
      error = function(e) "a value that cannot be read"
    )
  }
  issues <- character(0)
  # A policy is a list of at most four fields; they are read by exact name,
  # so that a field such as "methods" is not taken for "method".
  if (!is.null(p) && !is.list(p)) {
    issues <- c(issues, sprintf(
      "the stored policy %s is not a list of scoring rules, so the mean of the answered items with at least one answered is in force",
      shown(p)
    ))
  }
  if (!is.list(p)) p <- list()
  known <- c("method", "min_count", "min_prop", "weights")
  extra <- setdiff(names(p) %||% character(0), known)
  if (length(extra) || (length(p) && is.null(names(p)))) {
    issues <- c(issues, sprintf(
      "the stored %s %s %s not part of a scoring policy and %s not used",
      if (length(extra) == 1L) "field" else "fields",
      if (length(extra)) paste(sprintf("\"%s\"", extra), collapse = ", ") else "without a name",
      if (length(extra) == 1L) "is" else "are",
      if (length(extra) == 1L) "was" else "were"
    ))
  }
  stored <- p[["method"]]
  requested <- if (is.null(stored)) {
    NA_character_
  } else {
    tryCatch(paste(as.character(unlist(stored)), collapse = ", "), error = function(e) "a value that cannot be read")
  }
  method <- stored %||% "mean"
  if (!is.character(method) || length(method) != 1L || is.na(method) ||
    !method %in% names(sieve_score_methods())) {
    held <- tryCatch(as.character(unlist(method)), error = function(e) character(0))
    issues <- c(issues, if (!is.character(method) && length(held) == 1L && held %in% names(sieve_score_methods())) {
      sprintf(
        "the stored scoring method is not a single method name (it holds %s as a %s), so the mean of the answered items is in force until a method is chosen",
        shown(held), class(method)[1L]
      )
    } else {
      sprintf(
        "the stored scoring method %s is not one this version of SIEVER knows, so the mean of the answered items is in force until a method is chosen",
        shown(stored)
      )
    })
    method <- "mean"
  }
  count <- p[["min_count"]] %||% 1L
  if (!is.numeric(count) || length(count) != 1L || !is.finite(count) || count < 1 || count != trunc(count)) {
    issues <- c(issues, sprintf("the stored minimum answered count %s is not valid, so 1 is in force", shown(p[["min_count"]])))
    count <- 1L
  }
  prop <- p[["min_prop"]] %||% 0
  if (!is.numeric(prop) || length(prop) != 1L || !is.finite(prop) || prop < 0 || prop > 1) {
    issues <- c(issues, sprintf("the stored minimum answered proportion %s is not valid, so 0 is in force", shown(p[["min_prop"]])))
    prop <- 0
  }
  weights <- p[["weights"]]
  if (!is.null(weights) && !is.null(items) && !is.null(names(weights))) weights <- weights[items]
  issue <- if (length(issues)) {
    text <- paste(issues, collapse = "; ")
    paste0(toupper(substr(text, 1L, 1L)), substring(text, 2L), ".")
  }
  list(
    method = method, requested = requested, min_count = as.integer(count), min_prop = as.numeric(prop),
    weights = weights, issue = issue
  )
}

#' The stored scoring policies that were repaired on reading
#'
#' @param policies The stored policies (`state$scoring_policies`).
#' @return A named character vector, one repair statement per construct
#'   whose stored policy `sieve_score_policy_read()` had to repair (empty
#'   when none did).
#' @author Ken Kelley
#' @noRd
sieve_score_policy_issues <- function(policies) {
  if (!is.list(policies) || !length(policies)) {
    return(stats::setNames(character(0), character(0)))
  }
  issues <- vapply(names(policies), function(cn) {
    sieve_score_policy_read(policies[[cn]])$issue %||% NA_character_
  }, character(1))
  issues[!is.na(issues)]
}

#' Build the score specification of every construct from a ledger
#'
#' One specification per construct with at least one included item: its
#' items (in ledger order), the score (`method`, with its label), the
#' minimum answered count and proportion, the number of answered items
#' they require, optional weights, whether the score is a factor score
#' from the fitted model, `requested_method` (the method as stored, `NA`
#' when none was), and `issue`, the repair `sieve_score_policy_read()`
#' made to its stored policy (`NULL` when none).
#'
#' @param meta The item ledger.
#' @param policies The stored policies (`state$scoring_policies`).
#' @param available Optional column names: items absent from them are left
#'   out (the item columns actually loaded).
#' @return A named list of specifications, one per construct.
#' @author Ken Kelley
#' @noRd
sieve_score_spec_build <- function(meta, policies = list(), available = NULL) {
  if (is.null(meta) || !nrow(meta)) {
    return(list())
  }
  active <- meta[meta$status == "active" & !is.na(meta$construct), , drop = FALSE]
  cons <- unique(as.character(active$construct))
  cons <- cons[sieve_construct_assigned(cons)]
  out <- lapply(cons, function(cn) {
    its <- as.character(active$item[active$construct == cn])
    if (!is.null(available)) its <- intersect(its, available)
    p <- sieve_score_policy_read(policies[[cn]], its)
    required <- max(p$min_count, ceiling(length(its) * p$min_prop))
    list(
      construct = cn, items = its, method = p$method, requested_method = p$requested,
      label = unname(sieve_score_methods()[p$method]),
      min_count = p$min_count, min_prop = p$min_prop, required = as.integer(required),
      weights = p$weights, latent = sieve_score_is_latent(p$method), issue = p$issue
    )
  })
  names(out) <- cons
  out[vapply(out, function(s) length(s$items) > 0L, logical(1))]
}

#' The operational score specification in force
#'
#' Reads the ledger and the stored policies of the shared state (under
#' [shiny::isolate()]: a caller inside a reactive takes its own
#' dependency on `state$meta` and `state$scoring_policies`) and builds one
#' specification per construct with `sieve_score_spec_build()`.
#'
#' @param state The shared state store.
#' @param available Optional column names (see `sieve_score_spec_build()`).
#' @return A named list of specifications, one per construct.
#' @author Ken Kelley
#' @noRd
sieve_score_specification <- function(state, available = NULL) {
  shiny::isolate(sieve_score_spec_build(state$meta, state$scoring_policies %||% list(), available))
}

#' Keep a construct's operational score specification
#'
#' Writes the construct's policy in `state$scoring_policies`, the field
#' saved projects and replay carry. A part not supplied keeps its stored
#' value (the mean of the answered items with at least one answered when
#' nothing was stored).
#'
#' @param state The shared state store.
#' @param construct The construct's name.
#' @param method A code from `sieve_score_methods()`, or `NULL` to keep.
#' @param min_count A positive integer, or `NULL` to keep.
#' @param min_prop A proportion in \eqn{[0, 1]}, or `NULL` to keep.
#' @param weights Nonnegative weights named by item, `NULL` for unit
#'   weights; left out to keep what is stored.
#' @return The stored policy, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_set_score_specification <- function(state, construct, method = NULL, min_count = NULL,
                                          min_prop = NULL, weights) {
  if (!is.character(construct) || length(construct) != 1L || is.na(construct) || !nzchar(construct)) {
    stop("`construct` must be one construct name.", call. = FALSE)
  }
  held <- shiny::isolate(state$scoring_policies) %||% list()
  p <- sieve_score_policy_read(held[[construct]])
  if (!is.null(method)) {
    if (!is.character(method) || length(method) != 1L || !method %in% names(sieve_score_methods())) {
      stop(sprintf(
        "`method` must be one of %s.", paste(sprintf("\"%s\"", names(sieve_score_methods())), collapse = ", ")
      ), call. = FALSE)
    }
    p$method <- method
  }
  if (!is.null(min_count)) {
    if (length(min_count) != 1L || !is.finite(min_count) || min_count < 1 || min_count != as.integer(min_count)) {
      stop("`min_count` must be a positive integer.", call. = FALSE)
    }
    p$min_count <- as.integer(min_count)
  }
  if (!is.null(min_prop)) {
    if (length(min_prop) != 1L || !is.finite(min_prop) || min_prop < 0 || min_prop > 1) {
      stop("`min_prop` must lie in [0, 1].", call. = FALSE)
    }
    p$min_prop <- as.numeric(min_prop)
  }
  if (!missing(weights)) {
    if (!is.null(weights) && (!is.numeric(weights) || any(!is.finite(weights)) || any(weights < 0) || !any(weights > 0))) {
      stop("`weights` must be finite, nonnegative, and not all zero.", call. = FALSE)
    }
    p$weights <- weights
  }
  stored <- list(method = p$method, min_count = p$min_count, min_prop = p$min_prop)
  if (!is.null(p$weights)) stored$weights <- p$weights
  held[[construct]] <- stored
  state$scoring_policies <- held
  invisible(stored)
}

#' The specification in force, one row per construct, with the eligible n
#'
#' The eligible n is the number of respondents who meet the construct's
#' minimum answered rule on the loaded item data. For a factor score the
#' rule is applied to the model's scores as well, so the number is an
#' upper bound on the respondents the fitted model scores (a model fit on
#' the complete cases scores fewer).
#'
#' A stored policy that could not be used as saved (an unknown method, an
#' invalid minimum) keeps its repair in the table: `requested_method` is
#' the method as stored and `review` the repair in force until a method is
#' kept, so an exported table never presents a substitution as the
#' declared rule.
#'
#' @param data The item data.
#' @param spec A specification from `sieve_score_spec_build()`.
#' @return A data frame with `construct`, `k`, `score` (the score in
#'   force), `requested_method` (as stored; `NA` when none was stored, which
#'   is read as the mean), `required`, `eligible`, `not_scored`, and `review`
#'   (the repair that needs review, `NA` when none); or `NULL` when `spec`
#'   is empty.
#' @author Ken Kelley
#' @noRd
sieve_score_specification_table <- function(data, spec) {
  if (!length(spec)) {
    return(NULL)
  }
  rows <- lapply(spec, function(s) {
    run <- tryCatch(
      sieve_composite_score(data, s$items, s$weights, "mean", s$min_count, s$min_prop),
      error = function(e) NULL
    )
    eligible <- if (is.null(run)) NA_integer_ else as.integer(sum(run$eligible))
    data.frame(
      construct = s$construct, k = length(s$items), score = s$label,
      requested_method = s$requested_method %||% NA_character_,
      required = s$required, eligible = eligible,
      not_scored = if (is.na(eligible)) NA_integer_ else as.integer(nrow(data)) - eligible,
      review = s$issue %||% NA_character_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' The operational score in force, in one sentence
#'
#' The constructs grouped by their score and minimum answered rule, each
#' group saying whether its specification was kept or is the default (the
#' mean of the answered items with at least one answered): "Visual:
#' prorated sum of the answered items, at least 3 answered (kept).
#' Textual and Speed: mean of the answered items, at least 1 answered
#' (the default)." Written from the stored policies, so it is true of a
#' reopened project and of a change made on another page, and never says
#' no specification was kept while one was.
#'
#' @param spec A specification from `sieve_score_spec_build()`.
#' @param policies The stored policies (`state$scoring_policies`).
#' @return One string; `""` when `spec` is empty.
#' @author Ken Kelley
#' @noRd
sieve_score_policy_sentence <- function(spec, policies = list()) {
  if (!length(spec)) {
    return("")
  }
  stored <- names(policies %||% list())
  kept <- vapply(spec, function(s) s$construct %in% stored, logical(1))
  key <- vapply(seq_along(spec), function(i) {
    paste(spec[[i]]$label, spec[[i]]$required, kept[[i]], sep = "\r")
  }, character(1))
  groups <- split(seq_along(spec), factor(key, levels = unique(key)))
  parts <- vapply(groups, function(i) {
    s <- spec[[i[[1L]]]]
    names_i <- vapply(spec[i], `[[`, character(1), "construct")
    who <- if (length(names_i) > 1L) {
      paste(paste(names_i[-length(names_i)], collapse = ", "), "and", names_i[length(names_i)])
    } else names_i
    sprintf("%s: %s, at least %d answered (%s).", who, s$label, s$required,
      if (kept[[i[[1L]]]]) "kept" else "the default")
  }, character(1))
  paste(unname(parts), collapse = " ")
}

#' The R that forms the operational score of every construct
#'
#' One `construct_scores` data frame, a column per construct: a composite
#' through [sieve_composite_score()], or a factor score read from the
#' fitted model named `fit_name` through [sieve_factor_score_record()]
#' with the construct's minimum answered rule applied to it.
#'
#' @param spec A specification from `sieve_score_spec_build()`.
#' @param data_name The name of the item data frame in the script.
#' @param fit_name The name of the fitted correlated-factors model in the
#'   script (needed only by a factor score).
#' @param name_map The fitted model's latent names, named by construct
#'   label as `sieve_show_names()` reads it (a latent name to label map
#'   from a specification's `name_map`); without it the columns of the
#'   score record are taken to carry the construct names.
#' @return A character vector of R code lines. A construct whose stored
#'   policy was repaired on reading is preceded by a `NEEDS REVIEW` comment
#'   naming the repair, and the code opens with a `PROVISIONAL` line.
#' @author Ken Kelley
#' @noRd
sieve_score_specification_code <- function(spec, data_name = "items", fit_name = "validity_fit",
                                           name_map = NULL) {
  quote_r <- function(x) encodeString(as.character(x), quote = '"')
  chr <- function(x) sprintf("c(%s)", paste(quote_r(x), collapse = ", "))
  latent_of <- function(cn) {
    if (is.null(name_map)) {
      return(cn)
    }
    hit <- match(cn, unname(name_map))
    if (is.na(hit)) cn else names(name_map)[hit]
  }
  one_line <- function(x) gsub("[\r\n]+", " ", as.character(x))
  repaired <- Filter(function(s) !is.null(s$issue), spec)
  lines <- c(
    "# The operational score of each construct, as specified (Reliability tab, Operational scores).",
    if (length(repaired)) {
      "# PROVISIONAL: a stored scoring policy could not be used as saved. The repair in force is named before its construct; keep a method on the Reliability tab before operational use."
    },
    sprintf("construct_scores <- data.frame(row.names = seq_len(nrow(%s)))", data_name)
  )
  needs <- character(0)
  for (s in spec) {
    rule <- sprintf("at least %d of %d items answered", s$required, length(s$items))
    if (!is.null(s$issue)) {
      lines <- c(lines, sprintf("# NEEDS REVIEW, %s: %s", one_line(s$construct), one_line(s$issue)))
    }
    if (!s$latent) {
      lines <- c(lines, sprintf(
        "construct_scores[[%s]] <- SIEVER::sieve_composite_score(%s, items = %s, weights = %s, method = %s, min_count = %dL, min_prop = %s)$scores  # %s; %s",
        quote_r(s$construct), data_name, chr(s$items),
        if (is.null(s$weights)) "NULL" else paste(utils::capture.output(dput(s$weights)), collapse = ""),
        quote_r(s$method), s$min_count, sieve_r_literal(s$min_prop), s$label, rule
      ))
    } else {
      engine <- if (identical(s$method, "bartlett")) "Bartlett" else "regression"
      object <- sprintf(".fs_%s", engine)
      if (!engine %in% needs) {
        needs <- c(needs, engine)
        lines <- c(lines, sprintf(
          "%s <- SIEVER::sieve_factor_score_record(%s, method = %s, data = %s)$scores  # rows aligned with %s",
          object, fit_name, quote_r(engine), data_name, data_name
        ))
      }
      lines <- c(
        lines,
        sprintf(
          "construct_scores[[%s]] <- %s[[%s]]  # %s from %s; %s",
          quote_r(s$construct), object, quote_r(latent_of(s$construct)), s$label, fit_name, rule
        ),
        sprintf(
          "construct_scores[[%s]][rowSums(!is.na(%s[, %s, drop = FALSE])) < %dL] <- NA_real_",
          quote_r(s$construct), data_name, chr(s$items), s$required
        )
      )
    }
  }
  lines
}

#' Sensitivity of the Marker-Variable Adjustment to the Marker Correlation
#'
#' @description
#' Lindell and Whitney's (2001) marker-variable technique attributes a
#' marker's correlation with the substantive variables to shared method and
#' partials that amount, \eqn{r_M}, from every substantive correlation:
#' \eqn{r^A_{ij} = (r_{ij} - r_M) / (1 - r_M)} (their Equation 4, computed
#' here by [DMAR::common_method_marker()]). The adjustment assumes a
#' constant method effect: the same \eqn{r_M} is removed from every
#' correlation, whatever the pair. Because \eqn{r_M} is a choice (the
#' smallest positive marker correlation is one conservative choice, and a
#' larger value removes more), the adjustment is read over a range of
#' values, and that is what this function computes.
#'
#' @details
#' With the sample size, each adjusted correlation is tested with Lindell
#' and Whitney's (2001) \eqn{t} statistic of the adjusted correlation on
#' \eqn{N - 3} degrees of freedom (their Equation 5),
#' \eqn{t = r^A / \sqrt{(1 - (r^A)^2) / (N - 3)}}, and the two-sided
#' \eqn{p} value is reported. A correlation whose adjusted value remains
#' different from zero is consistent with a relationship that is not
#' solely a method artifact under the constant method effect assumption;
#' it is not evidence that no method variance is present. An adjusted
#' correlation at or beyond \eqn{\pm 1} (a negative correlation pushed
#' away from zero) has no \eqn{t}. The function does not choose
#' \eqn{r_M}: the primary text is cited for the adjustment and the test
#' only.
#'
#' @param R A symmetric correlation matrix of the substantive variables
#'   (the construct scores), with row and column names.
#' @param marker_r The marker correlations to adjust for, each in
#'   \eqn{(-1, 1)}.
#' @param n The sample size the correlations rest on, for the \eqn{t}
#'   test; `NULL` (the default) reports the adjusted correlations only.
#' @return A data frame with one row per variable pair and value of
#'   `marker_r`: `marker_r`, `variable_1`, `variable_2`, `r` (unadjusted),
#'   `r_adjusted`, `t`, `df`, and `p` (`NA` without `n`), at full precision.
#'   The attribute `adjusted` holds the adjusted matrices, one per value of
#'   `marker_r`, and `n` the sample size used.
#' @references
#' Lindell, M. K., & Whitney, D. J. (2001). Accounting for common method
#'   variance in cross-sectional research designs. *Journal of Applied
#'   Psychology, 86*(1), 114--121.
#' @author Ken Kelley
#' @examples
#' hs <- lavaan::HolzingerSwineford1939
#' scores <- data.frame(
#'   Visual = rowMeans(hs[, c("x1", "x2", "x3")]),
#'   Textual = rowMeans(hs[, c("x4", "x5", "x6")]),
#'   Speed = rowMeans(hs[, c("x7", "x8", "x9")])
#' )
#' R <- cor(scores)
#' sens <- sieve_marker_sensitivity(R, marker_r = c(0.05, 0.10, 0.20), n = nrow(hs))
#' sens
#' attr(sens, "adjusted")[["0.1"]]
#' @export
sieve_marker_sensitivity <- function(R, marker_r, n = NULL) {
  if (!is.matrix(R) || !is.numeric(R) || nrow(R) < 2L || !isSymmetric(unname(R))) {
    stop("`R` must be a symmetric correlation matrix of two or more variables.", call. = FALSE)
  }
  if (!is.numeric(marker_r) || !length(marker_r) || any(!is.finite(marker_r)) ||
    any(marker_r <= -1 | marker_r >= 1)) {
    stop("`marker_r` must hold numbers strictly inside (-1, 1).", call. = FALSE)
  }
  if (!is.null(n) && (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n <= 3)) {
    stop("`n` must be a single sample size above 3, or NULL.", call. = FALSE)
  }
  vars <- rownames(R) %||% colnames(R) %||% paste0("V", seq_len(nrow(R)))
  dimnames(R) <- list(vars, vars)
  pairs <- utils::combn(seq_along(vars), 2L)
  adjusted <- lapply(marker_r, function(rM) attr(DMAR::common_method_marker(R, marker_r = rM), "adjusted"))
  names(adjusted) <- as.character(marker_r)
  rows <- lapply(seq_along(marker_r), function(k) {
    A <- adjusted[[k]]
    r_adj <- A[t(pairs)]
    inside <- is.finite(r_adj) & abs(r_adj) < 1
    t_stat <- rep(NA_real_, length(r_adj))
    p <- rep(NA_real_, length(r_adj))
    if (!is.null(n)) {
      t_stat[inside] <- r_adj[inside] / sqrt((1 - r_adj[inside]^2) / (n - 3))
      p[inside] <- 2 * stats::pt(abs(t_stat[inside]), df = n - 3, lower.tail = FALSE)
    }
    data.frame(
      marker_r = marker_r[k], variable_1 = vars[pairs[1, ]], variable_2 = vars[pairs[2, ]],
      r = R[t(pairs)], r_adjusted = r_adj, t = t_stat,
      df = if (is.null(n)) NA_real_ else n - 3, p = p, stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "adjusted") <- adjusted
  attr(out, "n") <- n
  attr(out, "method") <- paste(
    "Lindell and Whitney (2001) adjustment (r - r_M) / (1 - r_M) under a constant method effect,",
    "computed by DMAR::common_method_marker; t test of the adjusted correlation on n - 3 degrees of freedom"
  )
  out
}

#' An estimate of the wait of the calibration transformation bootstrap
#'
#' The time of one resample of each construct's calibration scores (a
#' draw with replacement, its mean and its standard deviation), measured
#' on `draws` pilot resamples on a preserved random number stream, times
#' the number of resamples requested. [sieve_norm_transform_limits()] runs
#' the resamples as one call, so the estimate is stated before it starts.
#'
#' @param reference A frozen reference from [sieve_norm_freeze()].
#' @param B The number of resamples requested.
#' @param draws Pilot resamples per construct for the measurement.
#' @return A list with `seconds` (the estimate), `per_resample` (the
#'   measured seconds of one resample over every construct), and `draws`.
#' @author Ken Kelley
#' @noRd
sieve_norm_transform_wait <- function(reference, B, draws = 200L) {
  per <- withr::with_preserve_seed(vapply(reference$scores, function(x) {
    n <- length(x)
    started <- proc.time()[["elapsed"]]
    for (b in seq_len(draws)) {
      z <- sample(x, n, replace = TRUE)
      m <- mean(z)
      s <- stats::sd(z)
    }
    (proc.time()[["elapsed"]] - started) / draws
  }, numeric(1)))
  list(seconds = sum(per) * B, per_resample = sum(per), draws = draws)
}

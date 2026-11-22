# Scientific inference adapters. Engine estimates are retained at full precision.

#' Resolve a factor score estimator without changing its indicator scale
#' @param fit A fitted lavaan model.
#' @param method A linear or categorical scoring method.
#' @return The requested and actual method, label, and model treatment.
#' @author Ken Kelley
#' @noRd
sieve_score_method <- function(fit, method = "Bartlett") {
  categorical <- sieve_fit_is_categorical(fit)
  valid <- c("Bartlett", "regression", "ML", "EBM")
  if (length(method) != 1L || !method %in% valid) stop("Unknown factor scoring method.", call. = FALSE)
  actual <- if (categorical) {
    if (method %in% c("Bartlett", "ML")) "ML" else "EBM"
  } else {
    if (method %in% c("Bartlett", "ML")) "Bartlett" else "regression"
  }
  opts <- lavaan::lavInspect(fit, "options")
  list(
    requested = method, method = actual, categorical = categorical,
    label = switch(actual,
      ML = "Categorical ML factor score",
      EBM = "Empirical Bayes modal factor score",
      Bartlett = "Bartlett factor score",
      regression = "Regression factor score (posterior mean)"
    ),
    estimator = opts$estimator, missing = opts$missing
  )
}

#' Align engine score rows with the observations supplied to the model
#' @param fit Fitted lavaan model.
#' @param method Resolved score method.
#' @param uncertainty Request engine score error as well as point estimates.
#' @param n The number of rows supplied to the model, when it has been
#'   verified against them (`sieve_score_data_check()`); otherwise the
#'   number the fit itself records. A grouped model does not record rows
#'   whose group membership is missing, so a verified count can exceed it.
#' @param data Optional verified input data, including excluded rows.
#' @param conf_level Confidence level for DMAR's two-score table.
#' @return Aligned matrices, original data positions, and engine diagnostics.
#' @author Ken Kelley
#' @noRd
sieve_score_rows <- function(fit, method, uncertainty = FALSE, n = NULL,
                             data = NULL, conf_level = .95) {
  run <- sieve_capture(DMAR::factor_scores(fit, data = data, conf_level = conf_level))
  if (!run$ok) return(list(error = run$error, warnings = run$warnings))
  table <- run$value
  latent <- unique(table$factor)
  size <- max(c(n %||% 0L, table$row))
  point <- error <- matrix(NA_real_, size, length(latent), dimnames = list(NULL, latent))
  reasons <- matrix("excluded by the model's missing-data or group rule", size, length(latent))
  column <- if (method %in% c("Bartlett", "ML")) "bartlett" else "regression"
  at <- cbind(table$row, match(table$factor, latent))
  derived <- attr(table, "derived_columns", exact = TRUE)
  point_column <- derived[[column]] %||% column
  se_column <- derived[[paste0(column, "_se")]] %||% paste0(column, "_se")
  point[at] <- table[[point_column]]
  error[at] <- table[[se_column]]
  reasons[at] <- table$reason
  reasons[is.na(reasons)] <- ""
  return(list(scores = point, se = error, reasons = reasons, rows = seq_len(size),
    warnings = run$warnings, error = NULL, engine = table))
}

#' Does a data frame hold the rows a model was fitted to?
#'
#' The identity contract of [sieve_factor_score_record()]: the scores come
#' from the fit, and the answered counts and row identities from `data`, so
#' `data` must hold every observed variable of the model, one row per row
#' supplied to the model, and, in the rows the model used, the values it used.
#' Continuous variables must agree to within 1e-8 (or, when the model
#' standardized them with `std.ov = TRUE`, correlate perfectly); an ordered
#' variable is held by lavaan as category codes, so its supplied values must
#' map one to one, in order, onto the codes. Missing values must fall in the
#' same places.
#'
#' A grouped model keeps no record of the rows whose group membership is
#' missing: lavaan drops them before fitting, so its per-group counts
#' (`norig`) and case indices stop at the last row with a group, and a frame
#' that ends in such rows has more rows than the fit can count. When `data`
#' holds the model's group variable, those rows are identified from it rather
#' than assumed: every row must either carry a group the model was fitted to,
#' in the numbers the fit records and at the positions of its case indices, or
#' have no group at all, and only the latter are admitted beyond the fit's
#' count. No other extra row is accepted.
#'
#' @param fit A fitted lavaan model.
#' @param data The rows offered as those the model was fitted to.
#' A fit restricted to some groups with lavaan's `group.label` excluded the
#' rows of the other groups in the same way, and they are kept in the same
#' way; without it, a group value the fit does not hold is refused.
#'
#' @return Invisibly, a list with `n` (the number of rows supplied to the
#'   model), `unassigned` (the rows with no group membership), and
#'   `unselected` (the rows of groups a `group.label` fit left out), both of
#'   which the model excluded; stops with what differs otherwise.
#' @author Ken Kelley
#' @noRd
sieve_score_data_check <- function(fit, data) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame of the rows supplied to the fitted model.", call. = FALSE)
  }
  ov <- lavaan::lavNames(fit, "ov")
  absent <- setdiff(ov, names(data))
  if (length(absent)) {
    stop(sprintf(
      "`data` does not hold the model's observed variable%s %s, so it is not the data the model was fitted to.",
      if (length(absent) == 1L) "" else "s", paste(absent, collapse = ", ")
    ), call. = FALSE)
  }
  index <- lavaan::lavInspect(fit, "case.idx")
  if (!is.list(index)) index <- list(index)
  # A model fitted to summary statistics has no rows to compare with; its
  # scores are refused by the scoring step with its own reason.
  if (!length(unlist(index))) {
    return(invisible(list(n = NULL, unassigned = integer(0))))
  }
  norig <- as.integer(unlist(lavaan::lavInspect(fit, "norig")))
  n_fit <- max(c(sum(norig), unlist(index)))
  group <- as.character(lavaan::lavInspect(fit, "group"))
  unassigned <- integer(0)
  unselected <- integer(0)
  if (length(group) == 1L && nzchar(group) && group %in% names(data)) {
    labels <- as.character(lavaan::lavInspect(fit, "group.label"))
    held <- as.character(data[[group]])
    # A fit given group.label keeps only those groups: rows of the others
    # were supplied to it and excluded by its group rule. Without it, every
    # group in the data was fitted, and another value is not the model's.
    selected <- length(lavaan::lavInspect(fit, "options")$group.label) > 0L
    outside <- !is.na(held) & !held %in% labels
    foreign <- if (selected) character(0) else unique(held[outside])
    if (length(foreign)) {
      stop(sprintf(
        "`data` holds %s %s of the group variable %s, which the model was not fitted to; supply the rows given to the model, in their order.",
        if (length(foreign) == 1L) "the value" else "the values", paste(foreign, collapse = ", "), group
      ), call. = FALSE)
    }
    sizes <- vapply(labels, function(l) sum(held == l, na.rm = TRUE), integer(1))
    placed <- all(vapply(seq_along(index), function(g) {
      all(held[as.integer(index[[g]])] %in% labels[g])
    }, logical(1)))
    if (!identical(unname(sizes), norig) || !placed) {
      stop(sprintf(
        "`data` does not hold the rows the model was fitted to: the rows of its groups (%s) differ from those the model used. Supply the rows given to the model, in their order.",
        group
      ), call. = FALSE)
    }
    unassigned <- which(is.na(held))
    if (selected) unselected <- which(outside)
    expected <- sum(norig) + length(unassigned) + length(unselected)
  } else {
    expected <- n_fit
  }
  if (nrow(data) != expected) {
    stop(sprintf(
      "`data` has %d rows, but the model was fitted to %d; supply the rows given to the model, in their order%s.",
      nrow(data), expected,
      if (length(group) == 1L && nzchar(group) && !group %in% names(data)) {
        sprintf(", with the group variable %s, so that rows the model excluded for a missing group can be identified", group)
      } else ""
    ), call. = FALSE)
  }
  model_data <- lavaan::lavInspect(fit, "data")
  if (!is.list(model_data)) model_data <- list(model_data)
  ordered <- lavaan::lavNames(fit, "ov.ord")
  standardized <- isTRUE(lavaan::lavInspect(fit, "options")$std.ov)
  for (g in seq_along(index)) {
    ids <- as.integer(index[[g]])
    X <- as.matrix(model_data[[g]])
    if (is.null(colnames(X))) colnames(X) <- ov[seq_len(ncol(X))]
    for (v in colnames(X)) {
      fitted <- as.numeric(X[, v])
      supplied <- data[[v]][ids]
      if (inherits(supplied, "haven_labelled_spss") || !is.null(attr(supplied, "na_values")) ||
          !is.null(attr(supplied, "na_range")) || !is.null(attr(supplied, "spss_missing"))) supplied <- sieve_as_number(supplied)
      if (v %in% ordered) {
        s <- if (is.factor(supplied)) as.integer(supplied) else supplied
        same <- all(is.na(s) == is.na(fitted))
        if (same && any(!is.na(fitted))) {
          seen <- !is.na(fitted)
          pairs <- unique(data.frame(code = fitted[seen], value = s[seen], stringsAsFactors = FALSE))
          same <- !anyDuplicated(pairs$code) && !anyDuplicated(pairs$value) &&
            (!is.numeric(pairs$value) || !is.unsorted(pairs$value[order(pairs$code)], strictly = TRUE))
        }
      } else {
        s <- sieve_as_number(supplied)
        same <- all(is.na(s) == is.na(fitted))
        seen <- !is.na(fitted)
        if (same && sum(seen) > 0L) {
          same <- if (standardized) {
            sum(seen) < 3L || isTRUE(stats::cor(s[seen], fitted[seen]) > 1 - 1e-8)
          } else {
            isTRUE(all(abs(s[seen] - fitted[seen]) <= 1e-8 * pmax(1, abs(fitted[seen]))))
          }
        }
      }
      if (!same) {
        stop(sprintf(
          "`data` does not hold the rows the model was fitted to: the values of %s differ from those the model used. Supply the rows given to the model, in their order.",
          v
        ), call. = FALSE)
      }
    }
  }
  invisible(list(n = nrow(data), unassigned = unassigned, unselected = unselected))
}

#' Factor Scores, Person-Level Uncertainty, and Row Identity
#'
#' Scores and intervals retain every input row. Rows omitted by the model's
#' missing-data rule, rows a grouped model excluded because their group is
#' missing, all-missing rows, and categorical endpoint patterns without a
#' finite ML estimate receive missing values with a stated reason.
#' Regression limits are normal-model posterior intervals. Bartlett limits
#' use conditional score estimation error. Both condition on fitted model
#' parameters; parameter uncertainty is not included.
#'
#' Categorical scores (EBM and ML) require lavaan 0.7-3 or later, the
#' version SIEVER imports: lavaan 0.7-2 added the mean structure twice in
#' its categorical scores (lavaan's release history), so they were wrong in
#' a model with nonzero intercepts for ordered indicators (a multiple group
#' model with equal thresholds, for one). The lavaan version that produced
#' the scores is returned with them.
#' @param fit A fitted lavaan model.
#' @param method Requested factor score method.
#' @param data Optional: the rows supplied to the fitted model, in their
#'   order, before the model's own case filtering (they give the answered
#'   counts). They are checked against the data the model holds, and a
#'   frame that is not those rows (other values, another order, a missing
#'   variable, a different number of rows) is refused; scoring new
#'   observations is not what this function does. For a multiple group
#'   model, include the group variable: rows whose group is missing, which
#'   the model excluded, are then identified and kept without a score.
#' @param conf_level Interval level in (0, 1).
#' @param original_rows Stable original row identifiers, aligned with `data`.
#' @return A list with aligned `scores`, long-form `uncertainty`, `observed`
#'   item counts, `rows`, `original_rows`, scoring `method`, interval method,
#'   the `determinacy` of the requested scores (Bartlett-score determinacy for
#'   Bartlett scores, regression-score determinacy otherwise) with
#'   `determinacy_method`, reasons for unavailable results, and
#'   `engine_version` (the lavaan version that computed the scores).
#'   `determinacy_table` retains group and factor names and the selected
#'   method's DMAR determinacy, including unavailable values. The
#'   `determinacy` field is a named numeric vector for one group and a
#'   named list of vectors for multiple groups.
#' @examples
#' fit <- lavaan::cfa("visual =~ x1 + x2 + x3",
#'   data = lavaan::HolzingerSwineford1939, std.lv = TRUE
#' )
#' scores <- sieve_factor_score_record(fit, conf_level = .90)
#' head(scores$uncertainty)
#' @author Ken Kelley
#' @export
sieve_factor_score_record <- function(fit, method = "Bartlett", data = NULL, conf_level = .95,
                                      original_rows = NULL) {
  conf_level <- sieve_check_conf_level(conf_level)
  treatment <- sieve_score_method(fit, method)
  if (!identical(sieve_fit_status(fit)$code, "ok")) {
    return(list(
      scores = NULL, uncertainty = NULL, method = treatment,
      reason = "Score inference requires an admissible measurement model."
    ))
  }
  # The scores come from the fit and the answered counts and identities from
  # `data`, so the two must be the same rows in the same order: a frame
  # that is not is refused before anything is attached to a score. Rows a
  # grouped model excluded for a missing group are identified from the group
  # variable and kept, without a score.
  if (!is.null(data)) {
    columns <- intersect(lavaan::lavNames(fit, "ov"), names(data))
    for (column in columns) {
      x <- data[[column]]
      if (inherits(x, "haven_labelled_spss") || !is.null(attr(x, "na_values")) ||
          !is.null(attr(x, "na_range")) || !is.null(attr(x, "spss_missing"))) data[[column]] <- sieve_as_number(x)
    }
  }
  verified <- if (!is.null(data)) {
    sieve_score_data_check(fit, data)
  } else {
    list(n = NULL, unassigned = integer(0), unselected = integer(0))
  }
  aligned <- sieve_score_rows(fit, treatment$method, uncertainty = TRUE, n = verified$n,
    data = data, conf_level = conf_level)
  if (!is.null(aligned$error)) {
    aligned <- sieve_score_rows(fit, treatment$method, uncertainty = FALSE, n = verified$n,
      data = data, conf_level = conf_level)
    if (!is.null(aligned$error)) return(list(scores = NULL, reason = aligned$error, method = treatment))
  }
  if (length(verified$unassigned)) {
    aligned$reasons[verified$unassigned, ] <- "no group membership; excluded by the model's group rule"
  }
  if (length(verified$unselected)) {
    aligned$reasons[verified$unselected, ] <- "a group the model was not fitted to (group.label); excluded by the model's group rule"
  }
  if (!identical(treatment$method, "ML")) {
    finite <- is.finite(aligned$scores)
    for (at in which(finite & grepl("no finite ML", aligned$reasons, fixed = TRUE))) {
      parts <- strsplit(aligned$reasons[at], "; ", fixed = TRUE)[[1L]]
      aligned$reasons[at] <- paste(parts[!grepl("no finite ML", parts, fixed = TRUE)], collapse = "; ")
    }
  }
  n <- nrow(aligned$scores)
  if (is.null(original_rows)) original_rows <- seq_len(n)
  if (length(original_rows) != n || anyNA(original_rows) || anyDuplicated(original_rows)) {
    stop("`original_rows` must uniquely identify every input row.", call. = FALSE)
  }
  scores <- as.data.frame(aligned$scores)
  names(scores) <- make.unique(sieve_show_names(names(scores), fit), sep = " ")
  observed <- NULL
  if (!is.null(data)) {
    its <- setdiff(lavaan::lavNames(fit, "ov"), lavaan::lavInspect(fit, "options")$aux)
    observed <- rowSums(!is.na(data[, its, drop = FALSE]))
    # A row with no observed item has no score, whatever the engine gave.
    empty <- observed == 0L
    aligned$reasons[empty, ] <- "no observed items"
    aligned$se[empty, ] <- NA_real_
    scores[empty, ] <- NA_real_
  }
  posterior <- treatment$method %in% c("regression", "EBM")
  interval_method <- if (posterior) {
    if (identical(
      treatment$method,
      "EBM"
    )) "Approximate posterior interval centered on the posterior mode; fitted model parameters treated as known"
    else "Normal-model posterior interval centered on the shrunken posterior mean; fitted model parameters treated as known"
  } else "Conditional Wald interval for the person's factor value; fitted model parameters treated as known"
  z <- stats::qnorm((1 + conf_level) / 2)
  uncertainty <- do.call(rbind, lapply(seq_len(ncol(scores)), function(j) {
    se <- aligned$se[, j]
    reason <- aligned$reasons[, j]
    bad <- !is.finite(se) | se < 0
    reason[bad & !nzchar(reason)] <- "The engine did not supply score error for this row and method."
    se[bad] <- NA_real_
    data.frame(
      row = seq_len(n), original_row = original_rows, factor = names(scores)[j],
      estimate = scores[[j]], se = se, lower = scores[[j]] - z * se,
      upper = scores[[j]] + z * se, level = conf_level, interval_method = interval_method,
      answered = if (is.null(observed)) NA_integer_ else observed, reason = reason,
      stringsAsFactors = FALSE
    )
  }))
  # The determinacy of the scores actually requested: Bartlett scores have
  # their own (lavaan's "fs_determinacy_Bartlett"); every other method is
  # read against the regression-score determinacy.
  bartlett <- identical(treatment$method, "Bartlett")
  engine_determinacy <- attr(aligned$engine, "determinacy", exact = TRUE)
  column <- if (bartlett) "determinacy_bartlett" else "determinacy_regression"
  determinacy_table <- engine_determinacy[c("group", "factor", column)]
  names(determinacy_table)[3] <- "determinacy"
  determinacy_values <- if (all(is.na(engine_determinacy$group))) {
    stats::setNames(engine_determinacy[[column]], engine_determinacy$factor)
  } else lapply(split(engine_determinacy, engine_determinacy$group), function(d) {
    stats::setNames(d[[column]], d$factor)
  })
  if (is.list(determinacy_values) && length(determinacy_values) == 1L) determinacy_values <- determinacy_values[[1L]]
  determinacy_available <- any(is.finite(unlist(determinacy_values)))
  determinacy_label <- if (bartlett) "Bartlett-score" else "Regression-score"
  reason <- if (all(!is.finite(uncertainty$se))) {
    "Score error is unavailable for this estimator/missing-data combination; no zero-error scores are substituted."
  } else NULL
  extreme <- matrix(FALSE, nrow = nrow(aligned$scores), ncol = ncol(aligned$scores))
  if (identical(treatment$method, "ML")) {
    extreme <- !is.finite(aligned$scores) & matrix(
      grepl("no finite ML", aligned$reasons, fixed = TRUE), nrow = nrow(aligned$scores))
  }
  list(
    scores = scores, uncertainty = uncertainty, observed = observed, rows = seq_len(n),
    both_scores = if (inherits(aligned$engine, "dmar_factor_scores")) aligned$engine else NULL,
    pattern_determinacy = attr(aligned$engine, "determinacy", exact = TRUE),
    original_rows = original_rows, method = treatment, conf_level = conf_level,
    interval_method = interval_method, reason = reason, warnings = aligned$warnings,
    engine_version = c(lavaan = as.character(utils::packageVersion("lavaan"))),
    acov = attr(aligned$engine, "acov", exact = TRUE),
    unavailable_scores = sum(!is.finite(as.matrix(scores))),
    extreme_patterns = sum(rowSums(extreme) > 0L),
    extreme_scores = sum(extreme),
    determinacy = determinacy_values, determinacy_table = determinacy_table,
    determinacy_method = determinacy_label,
    determinacy_reason = if (determinacy_available) NULL else sprintf("%s determinacy is unavailable for this fitted model.", determinacy_label),
    determinacy_target = sprintf("Determinacy of the model's %s scores; distinct from person score error.",
                                 if (bartlett) "Bartlett" else "regression")
  )
}

#' Residual Pairs From lavaan's Stated Scaling
#'
#' @description
#' The residual of every pair of observed variables of a fitted lavaan model
#' in the scaling you name (correlation residuals in Bentler's or Bollen's
#' scaling, or raw covariance residuals), with the observed and
#' model-implied values and lavaan's standard errors and z statistics,
#' beside lavaan's full residual object. The scaling is part of the result,
#' because residuals in different scalings are not comparable; a model whose
#' unrestricted model lavaan cannot form is refused with the reason.
#'
#' @param fit A fitted lavaan model.
#' @param type A supported lavaan residual scaling.
#' @return A pair table and the full engine residual object.
#' @author Ken Kelley
#' @examples
#' fit <- lavaan::cfa("visual =~ x1 + x2 + x3
#'                     textual =~ x4 + x5 + x6
#'                     speed =~ x7 + x8 + x9",
#'   data = lavaan::HolzingerSwineford1939, std.lv = TRUE)
#' pairs <- sieve_residual_pairs(fit)
#' head(pairs$table)
#' @export
sieve_residual_pairs <- function(fit, type = c("cor.bentler", "cor.bollen", "raw")) {
  type <- match.arg(type)
  refusal <- sieve_h1_refusal(fit)
  if (!is.null(refusal)) return(list(table = NULL, error = refusal))
  run <- sieve_capture(lavaan::lavResiduals(fit, type = type, se = TRUE, zstat = TRUE))
  if (!run$ok) return(list(table = NULL, error = run$error, warnings = run$warnings))
  r <- run$value
  if (is.null(r$cov)) return(list(table = NULL, error = "Residual pairs require one model group.", engine = r))
  M <- r$cov
  ij <- which(upper.tri(M), arr.ind = TRUE)
  z <- r$cov.z
  obs <- lavaan::lavInspect(fit, "sampstat")$cov
  imp <- lavaan::lavInspect(fit, "implied")$cov
  if (identical(type, "cor.bollen")) {
    obs <- stats::cov2cor(obs)
    imp <- stats::cov2cor(imp)
  } else if (identical(type, "cor.bentler")) {
    scale <- sqrt(diag(obs) %o% diag(obs))
    obs <- obs / scale
    imp <- imp / scale
  }
  tab <- data.frame(
    variable1 = rownames(M)[ij[, 1]], variable2 = colnames(M)[ij[, 2]],
    observed = obs[ij], implied = imp[ij], residual = M[ij],
    scale = switch(type,
      raw = "covariance",
      cor.bollen = "correlation",
      cor.bentler = "covariance divided by observed standard deviations"
    ),
    z = if (is.null(z)) rep(NA_real_, nrow(ij)) else z[ij], stringsAsFactors = FALSE
  )
  tab$item1 <- sieve_show_names(tab$variable1, fit)
  tab$item2 <- sieve_show_names(tab$variable2, fit)
  list(table = tab, engine = r, type = type, warnings = run$warnings, error = NULL)
}

#' Apply an Operational Available-Response Score Policy
#' @description
#' The available-response weighted mean of [sieve_composite_score()] with
#' `method = "mean"`: the two return the same scores, answered counts, and
#' eligibility for the same policy.
#' @param data Already scored item values.
#' @param items Included item columns.
#' @param min_count Minimum answered items.
#' @param min_prop Minimum answered proportion.
#' @param weights Optional nonnegative item weights, named by item.
#' @return Scores, answered counts, eligibility, and the exact policy.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' items$x2[1:5] <- NA
#' items$x3[1:2] <- NA
#' # A Visual score for every respondent who answered at least two of its items.
#' scored <- sieve_score_policy(items, c("x1", "x2", "x3"), min_count = 2L)
#' table(answered = scored$answered, scored = scored$eligible)
#' head(scored$scores)
#' @export
sieve_score_policy <- function(data, items, min_count = 1L, min_prop = 0, weights = NULL) {
  # The composite scorer (fct_scoring.R); the policy it returns names the
  # method, which this function's policy leaves out.
  out <- sieve_composite_score(data, items,
    weights = weights, method = "mean",
    min_count = min_count, min_prop = min_prop
  )
  out$policy$method <- NULL
  out
}

#' Compare Prespecified Operational Response Policies
#'
#' @description
#' Scores the same items under several minimum-answered-count policies
#' stated in advance and shows, for each, how many respondents receive a
#' score and the mean and standard deviation of their scores, with the
#' per-respondent results. It shows what a scoring rule costs in coverage
#' and how much it moves the scores; it makes no claim about reliability.
#'
#' @param data Scored items.
#' @param items Item columns.
#' @param counts Integer minimum-count policies.
#' @param meta Optional ledger to count represented facets.
#' @return Summary and per-case policy results, without a reliability claim.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' items$x2[1:5] <- NA
#' items$x3[1:2] <- NA
#' sens <- sieve_score_policy_sensitivity(items, c("x1", "x2", "x3"))
#' sens$table
#' @export
sieve_score_policy_sensitivity <- function(
  data,
  items,
  counts = c(
    1L,
    ceiling(length(items) / 2),
    length(items)
  ),
  meta = NULL
) {
  if (!is.numeric(counts) || !length(counts) || any(!is.finite(counts)) || any(counts < 1) || any(counts != as.integer(counts))) stop("`counts` must contain positive integers.",
    call. = FALSE
  )
  counts <- sort(unique(as.integer(counts)))
  runs <- lapply(counts, function(k) sieve_score_policy(data, items, min_count = k))
  rows <- lapply(seq_along(runs), function(i) {
    x <- runs[[i]]
    data.frame(
      minimum = counts[i], scored = sum(x$eligible), unscored = sum(!x$eligible),
      mean = if (any(x$eligible)) mean(x$scores[x$eligible]) else NA_real_,
      sd = if (sum(x$eligible) > 1L) stats::sd(x$scores[x$eligible]) else NA_real_
    )
  })
  patterns <- data.frame(row = seq_len(nrow(data)), answered = runs[[1L]]$answered)
  if (!is.null(meta) && "facet" %in% names(meta)) {
    f <- as.character(meta$facet[match(items, meta$item)])
    groups <- split(items[!is.na(f) & nzchar(f)], f[!is.na(f) & nzchar(f)])
    if (length(groups)) patterns$facets_represented <- rowSums(vapply(
      groups,
      function(g) rowSums(!is.na(data[,
        g,
        drop = FALSE
      ])) > 0,
      logical(nrow(data))
    ))
  }
  list(
    table = do.call(rbind, rows), cases = patterns, policies = lapply(runs, `[[`, "policy"),
    scores = lapply(runs, `[[`, "scores"),
    interpretation = "Descriptive sensitivity of operational scores; complete-score omega is not reliability of this partial-response procedure."
  )
}

#' Identify the Response Scale Represented by Each Fitted Model Outcome
#'
#' @description
#' Says, for each named outcome of a fitted lavaan model, on what scale its
#' coefficients are expressed: a latent variable, an observed continuous
#' outcome in its own units, or the latent response underlying an ordered
#' outcome, with its link (probit under the least squares estimators). A
#' coefficient is read on that scale, never on the observed categories.
#'
#' @param fit A fitted lavaan model.
#' @param outcomes Engine variable names, not current UI labels or declarations.
#' @return One row per outcome with kind, link, label, and interpretation.
#' @author Ken Kelley
#' @examples
#' fit <- lavaan::sem("visual =~ x1 + x2 + x3
#'                     textual =~ x4 + x5 + x6
#'                     textual ~ visual",
#'   data = lavaan::HolzingerSwineford1939, std.lv = TRUE)
#' sieve_outcome_scale(fit, "textual")
#' @export
sieve_outcome_scale <- function(fit, outcomes) {
  observed <- lavaan::lavNames(fit, "ov")
  latent <- lavaan::lavNames(fit, "lv")
  ordered <- lavaan::lavNames(fit, "ov.ord")
  if (!is.character(outcomes) || anyNA(outcomes) || any(!outcomes %in% c(
    observed,
    latent
  ))) stop("Outcome scales require variable names from the fitted model.",
    call. = FALSE
  )
  options <- lavaan::lavInspect(fit, "options")
  # In lavaan 0.7-3, options$link is documented as an unused placeholder.
  # The supported categorical WLS/ULS/PML engines instead fit a normal
  # underlying response, hence probit. Do not relabel an ignored link option.
  probit <- options$estimator %in% c("DWLS", "WLS", "ULS", "PML")
  link <- if (probit) "probit" else NA_character_
  kind <- ifelse(outcomes %in% latent,
    "latent",
    ifelse(outcomes %in% ordered,
      "underlying_categorical",
      "observed_continuous"
    )
  )
  label <- ifelse(kind == "latent", "Latent variable", ifelse(kind == "underlying_categorical",
    if (probit) "Underlying categorical response (probit)" else "Underlying categorical response (link unavailable)",
    "Observed continuous outcome"
  ))
  meaning <- ifelse(kind == "underlying_categorical",
    "Standardized paths and R-squared refer to the underlying response; they are not category-probability changes or variance explained in the observed category codes.",
    ifelse(kind == "latent", "Standardized paths and R-squared refer to modeled latent-variable variance.",
      "Standardized paths and R-squared refer to the modeled observed continuous outcome."
    )
  )
  data.frame(
    outcome = outcomes, kind = kind, link = ifelse(kind == "underlying_categorical", link, NA_character_),
    outcome_scale = label, interpretation = meaning, stringsAsFactors = FALSE
  )
}

#' R-Squared of Each Structural Outcome, by Group and Level, With Its Interval
#'
#' The proportion of the model-implied variance of each structural outcome
#' (each variable on the left of a `~` line) that the fitted model
#' explains, in every group and at every level of the model, with a Wald
#' interval formed on the logit scale. Every row carries the same columns,
#' and a row without a value or an interval says why.
#'
#' @details
#' The estimand is lavaan's R-squared, `lavaan::lavInspect(fit, "r2")`: one
#' minus the completely standardized residual (disturbance) variance of the
#' outcome, that is, one minus its residual variance divided by its
#' model-implied variance. It is computed in each block of the model (each
#' group, and within a group each level of a multilevel model) from that
#' block's own implied variances, so a coefficient constrained equal across
#' groups still gives a different R-squared in each group whose predictor
#' and residual variances differ. For a latent outcome the variance is the
#' factor's. For an ordered or binary outcome it is the variance of the
#' continuous latent response underlying the categories, not of the
#' observed category codes; each row carries the outcome's scale from
#' [sieve_outcome_scale()].
#'
#' The standard error is lavaan's delta-method standard error of the
#' standardized residual variance, from the joint covariance matrix of the
#' estimates ([lavaan::standardizedSolution()]); it is the standard error
#' of R-squared on its own scale. The rows of the standardized solution are
#' assigned to groups and levels through `lavaan::parTable(fit)$block`,
#' because for a multilevel fit the standardized solution carries no level
#' column. The interval is formed on the logit scale and transformed back
#' (Browne, 1982), as DMAR forms its intervals for omega and coefficient H:
#' with `z` the standard normal quantile for `conf_level`, the limits are
#' `plogis(qlogis(R2) - z * se / (R2 * (1 - R2)))` and
#' `plogis(qlogis(R2) + z * se / (R2 * (1 - R2)))`, the standard error
#' carried to the logit scale by the delta method. The limits lie strictly
#' inside (0, 1).
#'
#' A symmetric Wald interval on the R-squared scale under-covers and
#' crosses zero when R-squared is small. In a Monte Carlo check at n = 150
#' (two factors with three indicators each, standardized loadings of .70,
#' one structural path; 300 to 400 replications per condition, nominal 95
#' percent), it covered the
#' population R-squared in .870, .900, and .893 of the samples at .04, .15,
#' and .30, nearly every miss with the interval below the parameter, and
#' its lower limit fell below zero in 94 percent of the samples at .04; the
#' logit interval covered .917, .960, and .957. At very small R-squared and
#' small n the logit interval also falls short of its nominal level. There
#' a bootstrap interval is the alternative, for example the percentile
#' limits of `lavaan::bootstrapLavaan(fit, FUN = function(x)
#' unlist(lavaan::lavInspect(x, "r2")))`.
#'
#' An R-squared that the model fixes, at 0 when every path into the
#' outcome is fixed at zero, has no sampling interval, and its row says so.
#' An inadmissible fit gives one row whose `reason` repeats the fit's
#' status, a model with no `~` line gives one row that says it has no
#' structural outcome, and a group or level without a `~` line gives a row
#' that says so.
#'
#' @references
#' Browne, M. W. (1982). Covariance structures. In D. M. Hawkins (Ed.),
#' *Topics in applied multivariate analysis* (pp. 72--141). Cambridge
#' University Press.
#' @param fit A fitted lavaan model.
#' @param conf_level Confidence level.
#' @return A data frame with the same eleven columns in every case, at full
#'   precision:
#'   \describe{
#'     \item{`group`}{The group label (`lavaan::lavInspect(fit,
#'       "group.label")`), or `NA` for a single group model.}
#'     \item{`model_level`}{The level of a multilevel model
#'       (`lavaan::lavInspect(fit, "level.label")`, such as `"within"` and
#'       the cluster variable's name), or `NA` for a single level model.
#'       The name keeps it apart from the confidence level.}
#'     \item{`outcome`}{The outcome's name in the fitted model, or `NA` on a
#'       row that says why the model has no structural R-squared.}
#'     \item{`R2`}{lavaan's R-squared.}
#'     \item{`se`}{Its delta-method standard error, on the R-squared scale.}
#'     \item{`lower`, `upper`}{The limits of the logit-scale interval, or
#'       `NA` where no interval is formed.}
#'     \item{`conf_level`}{The confidence level.}
#'     \item{`method`}{How the interval was formed, or `NA` where none was.}
#'     \item{`reason`}{Why a value or an interval is missing; empty when
#'       the row is complete.}
#'     \item{`outcome_scale`}{What the outcome's variance is the variance
#'       of ([sieve_outcome_scale()]), or `NA` on a row without an outcome.}
#'   }
#' @author Ken Kelley
#' @examples
#' fit <- lavaan::sem("visual =~ x1 + x2 + x3
#'                     textual =~ x4 + x5 + x6
#'                     textual ~ visual",
#'   data = lavaan::HolzingerSwineford1939, std.lv = TRUE)
#' sieve_R2_intervals(fit)
#' # One row per group: the same structural model in each school.
#' by_school <- lavaan::sem("visual =~ x1 + x2 + x3
#'                           textual =~ x4 + x5 + x6
#'                           textual ~ visual",
#'   data = lavaan::HolzingerSwineford1939, std.lv = TRUE, group = "school")
#' sieve_R2_intervals(by_school)
#' @export
sieve_R2_intervals <- function(fit, conf_level = .95) {
  conf_level <- sieve_check_conf_level(conf_level)
  method <- paste(
    "Wald interval on the logit scale (Browne, 1982) from the delta-method standard error of the",
    "standardized residual variance (lavaan's joint covariance of the estimates); limits stay inside (0, 1)"
  )
  # One schema for every row, whatever the fit.
  rows <- function(group = NA_character_, model_level = NA_character_, outcome = NA_character_,
                   R2 = NA_real_, se = NA_real_, lower = NA_real_, upper = NA_real_,
                   method = NA_character_, reason = "") {
    data.frame(
      group = group, model_level = model_level, outcome = outcome, R2 = R2, se = se,
      lower = lower, upper = upper, conf_level = conf_level, method = method, reason = reason,
      outcome_scale = NA_character_, stringsAsFactors = FALSE
    )
  }
  status <- sieve_fit_status(fit)
  if (!identical(status$code, "ok")) {
    return(rows(reason = paste("R-squared intervals need an admissible solution.", status$message)))
  }
  pt <- lavaan::parTable(fit)
  if (!any(pt$op == "~")) {
    return(rows(reason = paste(
      "The model regresses no variable on another (it has no `~` line), so it has no structural",
      "R-squared. The R-squared lavaan lists for an indicator (lavaan::lavInspect(fit, \"r2\")) is that",
      "indicator's communality, not the R-squared of a structural outcome."
    )))
  }
  r2 <- lavaan::lavInspect(fit, "r2", drop.list.single.group = FALSE)
  n_groups <- lavaan::lavInspect(fit, "ngroups")
  n_levels <- lavaan::lavInspect(fit, "nlevels")
  group_labels <- lavaan::lavInspect(fit, "group.label")
  level_labels <- lavaan::lavInspect(fit, "level.label")
  # The standardized solution, row for row with the parameter table, whose
  # block column places each residual variance in its group and level.
  read <- sieve_capture(lavaan::standardizedSolution(fit,
    remove_eq = FALSE, remove_ineq = FALSE, remove_def = FALSE
  ))
  ss <- read$value
  unavailable <- if (!read$ok) {
    sprintf("lavaan's standardized solution could not be computed (%s), so this R-squared has no standard error.", read$error)
  } else if (nrow(ss) != nrow(pt) || !identical(ss$lhs, pt$lhs) || !identical(ss$op, pt$op) || !identical(ss$rhs, pt$rhs)) {
    paste(
      "lavaan's standardized solution does not match the parameter table row for row, so its residual",
      "variances cannot be placed in their groups and levels and this R-squared has no standard error."
    )
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  out <- do.call(rbind, lapply(seq_along(r2), function(b) {
    # lavaan orders the blocks by group, and within a group by level.
    group <- if (n_groups > 1L) group_labels[(b - 1L) %/% n_levels + 1L] else NA_character_
    model_level <- if (n_levels > 1L) level_labels[(b - 1L) %% n_levels + 1L] else NA_character_
    endogenous <- unique(pt$lhs[pt$block == b & pt$op == "~"])
    if (!length(endogenous)) {
      where <- paste(c(
        if (!is.na(group)) sprintf("group %s", group),
        if (!is.na(model_level)) sprintf("level %s", model_level)
      ), collapse = " at ")
      return(rows(group, model_level, reason = sprintf(
        "The model regresses no variable on another in %s, so there is no structural R-squared there.", where
      )))
    }
    values <- r2[[b]]
    outcomes <- c(intersect(names(values), endogenous), setdiff(endogenous, names(values)))
    do.call(rbind, lapply(outcomes, function(nm) {
      if (!nm %in% names(values)) {
        return(rows(group, model_level, nm, reason = "lavaan reports no R-squared for this outcome."))
      }
      R2 <- unname(values[[nm]])
      if (!is.finite(R2)) {
        return(rows(group, model_level, nm, R2, reason = "lavaan's R-squared for this outcome is not a finite number, so no interval is formed."))
      }
      if (!is.null(unavailable)) {
        return(rows(group, model_level, nm, R2, reason = unavailable))
      }
      hit <- which(pt$block == b & ss$op == "~~" & ss$lhs == nm & ss$rhs == nm)
      if (length(hit) != 1L || !isTRUE(all.equal(1 - ss$est.std[hit], R2, tolerance = 1e-7))) {
        return(rows(group, model_level, nm, R2, reason = paste(
          "lavaan's standardized residual variance for this outcome is not one minus its R-squared,",
          "so its standard error does not belong to this R-squared and no interval is formed."
        )))
      }
      se <- ss$se[hit]
      if (!is.finite(se)) {
        return(rows(group, model_level, nm, R2, reason = paste(
          "lavaan gives no standard error for the standardized residual variance of this outcome,",
          "so no interval is formed."
        )))
      }
      if (R2 <= 0 || R2 >= 1 || se <= 0) {
        return(rows(group, model_level, nm, R2, se, reason = sprintf(
          "R-squared is fixed at %s by the model, so it has no sampling interval.", format(R2)
        )))
      }
      # Browne's (1982) logit form: the interval on log(R2 / (1 - R2)),
      # with the standard error carried there by the delta method.
      half <- z * se / (R2 * (1 - R2))
      rows(group, model_level, nm, R2, se,
        lower = stats::plogis(stats::qlogis(R2) - half),
        upper = stats::plogis(stats::qlogis(R2) + half),
        method = method
      )
    }))
  }))
  named <- !is.na(out$outcome)
  if (any(named)) out$outcome_scale[named] <- sieve_outcome_scale(fit, out$outcome[named])$outcome_scale
  rownames(out) <- NULL
  out
}

#' Monte Carlo Intervals for the Fitted Defined Effects
#'
#' The Monte Carlo (parametric) interval for the effects a model defines
#' with `:=`, such as an indirect effect `a*b` (Preacher & Selig, 2012).
#' The completely standardized
#' (`std.all`) parameters that the definitions name are drawn `B` times from
#' the multivariate normal distribution centered at their estimates, with
#' lavaan's delta-method joint covariance of the standardized solution
#' (`lavaan::lavInspect(fit, "vcov.std.all")`); each defined effect is
#' evaluated on every draw from its own `:=` expression, and the interval
#' limits are the percentiles of those values. The point estimate is the
#' expression evaluated at the standardized estimates. The draws are
#' `stats::rnorm()` deviates multiplied by the Cholesky factor of the joint
#' covariance, on a local, recorded random number stream; this is the
#' interval method itself, not a simulation study.
#'
#' @details
#' A label normally names one parameter, and the definition is evaluated
#' once, as written. In a multiple-group model with a label for each group,
#' for example `c(a1, a2) * visual` with `ab1 := a1 * b1` and
#' `ab2 := a2 * b2`, each definition names its own group's parameters and
#' is evaluated that way.
#'
#' A label that names more than one parameter imposes equality, and the
#' equality holds for the unstandardized coefficients only: their
#' standardized values differ wherever the variances differ, so a
#' standardized effect built from the label by name is not unique (lavaan
#' reports its own standardized value of such a definition from the first
#' occurrence and warns). When every label a definition uses is shared
#' across the groups of a multiple-group model and names exactly one
#' parameter in each group (one label for the groups, as in `a * visual`
#' with `group = "school"`), the definition is evaluated in each group from
#' that group's standardized parameters, drawn jointly with the parameters
#' of the other groups, and the table gives one row per group with its
#' `group`. Any other shared label, one used twice within a group or in
#' some groups only, and a definition that mixes such a cross-group label
#' with a label of one group, is refused with the parameters the label
#' names and their standardized values, before any draw. lavaan's warnings
#' from the standardized solution and its covariance are returned in
#' `warnings`.
#'
#' The deviates are multiplied by the Cholesky factor of the joint
#' covariance. A covariance that is singular but positive semidefinite is
#' drawn in its column space, through the square root from its eigen
#' decomposition; a label shared across groups produces one, because with
#' `std.lv = TRUE` an equal path into a factor whose disturbance variance
#' is fixed standardizes to the same value in every group. A covariance with
#' an eigenvalue below zero beyond rounding is refused.
#'
#' @param fit A fitted SEM with defined effects; in a multiple-group model
#'   a definition may use labels of one group or labels shared across the
#'   groups, one parameter in each (see Details).
#' @param B Number of joint parameter draws.
#' @param seed Optional seed.
#' @param conf_level Confidence level.
#' @return A list with `table` (one row per defined effect, and in a
#'   multiple-group model one row per group for an effect evaluated in
#'   each group: `effect`; `group` in a multiple-group model only, the
#'   group's label for an effect evaluated in each group and `NA` for one
#'   evaluated once as written; `est.std`, `ci.lower`, `ci.upper`; the row
#'   names add `.g1`, `.g2`, and so on to an effect evaluated in each
#'   group), `samples` (the parameter draws and the effect values on each
#'   draw, the columns of an effect or a parameter of one group named with
#'   the same suffix), `B`, `counts` (requested, returned, and finite draws
#'   per column), `rng` (the seed actually used), `method`, `warnings`, and
#'   `error`; or a list with an `error` string and the `warnings` lavaan
#'   raised.
#' @author Ken Kelley
#' @examples
#' fit <- lavaan::sem("visual =~ x1 + x2 + x3
#'                     textual =~ x4 + x5 + x6
#'                     speed =~ x7 + x8 + x9
#'                     textual ~ a * visual
#'                     speed ~ b * textual + visual
#'                     ab := a * b",
#'   data = lavaan::HolzingerSwineford1939, std.lv = TRUE)
#' indirect <- sieve_indirect_mc(fit, B = 2000L, seed = 1)
#' indirect$table
#' @export
sieve_indirect_mc <- function(fit, B = 20000L, seed = NULL, conf_level = .95) {
  conf_level <- sieve_check_conf_level(conf_level)
  if (!identical(
    sieve_fit_status(fit)$code,
    "ok"
  )) return(list(error = "An admissible parent model is required for effect intervals."))
  if (length(B) != 1L || !is.finite(B) || B < 100L || B != as.integer(B)) stop("`B` must be an integer of at least 100.",
    call. = FALSE
  )
  B <- as.integer(B)
  pt <- lavaan::parTable(fit)
  defined <- pt$op == ":="
  if (!any(defined)) return(list(error = "The fitted model defines no effects (no := lines), so there is no effect to interval."))
  definitions <- stats::setNames(lapply(pt$rhs[defined], str2lang), pt$lhs[defined])
  # The standardized solution, row for row with the parameter table, and
  # its joint covariance, read with lavaan's warnings kept (its warning
  # that a standardized definition from a shared label is ambiguous among
  # them).
  engine <- sieve_capture(list(
    std = lavaan::standardizedSolution(fit, remove_eq = FALSE, remove_ineq = FALSE, remove_def = FALSE),
    V = lavaan::lavInspect(fit, "vcov.std.all")
  ))
  if (!engine$ok) {
    return(list(
      error = sprintf("lavaan could not compute the standardized solution and its covariance: %s", engine$error),
      warnings = engine$warnings
    ))
  }
  std <- engine$value$std
  V <- engine$value$V
  if (nrow(std) != nrow(pt) || !identical(std$lhs, pt$lhs) || !identical(std$op, pt$op) || !identical(std$rhs, pt$rhs)) {
    return(list(error = paste(
      "lavaan's standardized solution does not match the parameter table row for row, so the standardized",
      "parameters cannot be placed in their groups and no draws are made."
    ), warnings = engine$warnings))
  }
  # Every model parameter, named as lavaan names its parameters (the label
  # when one was given), with its group (lavaan orders the blocks by group,
  # and within a group by level) and its position in the covariance.
  keep <- !std$op %in% c(":=", "==", "<", ">", "<=", ">=")
  labels <- rep(NA_character_, nrow(std))
  labels[keep] <- lavaan::lav_partable_labels(std[keep, , drop = FALSE])
  n_groups <- lavaan::lavInspect(fit, "ngroups")
  n_levels <- lavaan::lavInspect(fit, "nlevels")
  group_labels <- lavaan::lavInspect(fit, "group.label")
  level_labels <- lavaan::lavInspect(fit, "level.label")
  group_of <- (pt$block - 1L) %/% n_levels + 1L
  # The parameter labels each definition rests on, through the definitions
  # it uses.
  leaves <- list()
  for (nm in names(definitions)) {
    leaves[[nm]] <- unique(unlist(lapply(all.vars(definitions[[nm]]), function(v) {
      if (v %in% names(leaves)) leaves[[v]] else v
    }), use.names = FALSE))
  }
  used <- unique(unlist(leaves, use.names = FALSE))
  used <- used[used %in% labels]
  rows_of <- lapply(stats::setNames(used, used), function(l) which(keep & labels %in% l))
  # A label names one parameter, or one in each group (an equality across
  # groups, evaluated group by group), or anything else (refused).
  kind <- vapply(rows_of, function(r) {
    if (length(r) == 1L) {
      return("single")
    }
    if (n_groups > 1L && length(r) == n_groups && setequal(group_of[r], seq_len(n_groups))) "by_group" else "shared"
  }, character(1))
  describe <- function(r) {
    paste(sprintf(
      "%s %s %s%s%s, std.all %.3f", std$lhs[r], std$op[r], std$rhs[r],
      if (n_groups > 1L) sprintf(" in group %s", group_labels[group_of[r]]) else "",
      if (n_levels > 1L) sprintf(" at level %s", level_labels[(pt$block[r] - 1L) %% n_levels + 1L]) else "",
      std$est.std[r]
    ), collapse = "; ")
  }
  shared <- names(kind)[kind == "shared"]
  if (length(shared)) {
    said <- vapply(shared, function(l) {
      first <- names(definitions)[vapply(leaves, function(x) l %in% x, logical(1))][1L]
      sprintf(
        "The defined effect %s uses the label %s, which names %d parameters (%s).",
        first, l, length(rows_of[[l]]), describe(rows_of[[l]])
      )
    }, character(1))
    return(list(error = paste(
      paste(said, collapse = " "),
      "Parameters constrained equal by a shared label are equal only unstandardized; their standardized",
      "(std.all) values differ, so a standardized defined effect built from the label is not unique (lavaan",
      "reports the first occurrence only). A label shared across the groups, naming one parameter in each",
      sprintf("group, is evaluated group by group; %s not.", if (length(shared) == 1L) "this one is" else "these are"),
      "Give each parameter its own label, for example c(a1, a2) * visual in a multiple-group model, and define",
      "one effect for each."
    ), warnings = engine$warnings))
  }
  by_group <- vapply(leaves, function(x) any(kind[intersect(x, used)] == "by_group"), logical(1))
  mixed <- by_group & vapply(leaves, function(x) any(kind[intersect(x, used)] == "single"), logical(1))
  if (any(mixed)) {
    said <- vapply(names(definitions)[mixed], function(nm) {
      x <- intersect(leaves[[nm]], used)
      single <- x[kind[x] == "single"]
      sprintf(
        paste(
          "The defined effect %s combines %s, constrained equal across the groups (one parameter in each",
          "group, so the effect would be evaluated group by group), with %s, which %s of one group only",
          "(%s), so the effect has no value in every group."
        ),
        nm, paste(x[kind[x] == "by_group"], collapse = ", "), paste(single, collapse = ", "),
        if (length(single) == 1L) "names a parameter" else "name parameters",
        describe(unlist(rows_of[single], use.names = FALSE))
      )
    }, character(1))
    return(list(error = paste(
      paste(said, collapse = " "),
      "Give each group its own labels, for example c(a1, a2) * visual, and define one effect for each group."
    ), warnings = engine$warnings))
  }
  # The parameters drawn, in the order of the parameter table; a parameter
  # of a label shared across groups is named with its group's suffix.
  slot_rows <- sort(unlist(rows_of, use.names = FALSE))
  slot_label <- labels[slot_rows]
  slot_group <- ifelse(kind[slot_label] == "by_group", group_of[slot_rows], NA_integer_)
  slot_name <- ifelse(is.na(slot_group), slot_label, sprintf("%s.g%d", slot_label, slot_group))
  position <- pt$free[slot_rows]
  if (!length(slot_rows) || any(position < 1L)) {
    return(list(error = "A parameter named in a defined effect has no sampling covariance (it is not a free parameter), so Monte Carlo draws are not defined for it; the delta intervals remain available.", warnings = engine$warnings))
  }
  center <- stats::setNames(std$est.std[slot_rows], slot_name)
  V <- V[position, position, drop = FALSE]
  root <- tryCatch(chol(V), error = function(e) NULL)
  if (is.null(root)) {
    # A covariance that is singular but positive semidefinite is drawn in
    # its column space, through the square root from its eigen
    # decomposition. It arises with labels shared across groups: with
    # std.lv = TRUE an equal path into a factor whose disturbance variance
    # is fixed standardizes to the same value in every group, so those
    # standardized parameters are one random variable.
    eig <- eigen(V, symmetric = TRUE)
    if (all(eig$values >= -sqrt(.Machine$double.eps) * max(abs(eig$values)))) {
      root <- t(eig$vectors %*% diag(sqrt(pmax(eig$values, 0)), nrow(V)))
    } else {
      return(list(error = sprintf(
        paste(
          "The joint covariance of the parameters in the defined effects is not positive semidefinite (its",
          "smallest eigenvalue is %s), so Monte Carlo draws are not defined; the delta intervals remain available."
        ),
        format(min(eig$values))
      ), warnings = engine$warnings))
    }
  }
  once <- names(definitions)[!by_group]
  each <- names(definitions)[by_group]
  groups <- if (length(each)) seq_len(n_groups) else integer(0)
  # The effects in table order: each definition in turn, and an effect
  # evaluated in each group once for every group.
  effects <- do.call(rbind, lapply(names(definitions), function(nm) {
    if (nm %in% each) {
      data.frame(effect = nm, group = groups, column = sprintf("%s.g%d", nm, groups), stringsAsFactors = FALSE)
    } else {
      data.frame(effect = nm, group = NA_integer_, column = nm, stringsAsFactors = FALSE)
    }
  }))
  # Evaluate the definitions in order, so one definition may use another:
  # those of single parameters once, the others in each group from that
  # group's parameters.
  evaluate <- function(values) {
    plain <- values[slot_name[is.na(slot_group)]]
    for (nm in once) {
      plain[[nm]] <- eval(definitions[[nm]], envir = plain, enclos = asNamespace("stats"))
      values[[nm]] <- plain[[nm]]
    }
    for (g in groups) {
      local <- values[slot_name[slot_group %in% g]]
      names(local) <- slot_label[slot_group %in% g]
      for (nm in each) {
        local[[nm]] <- eval(definitions[[nm]], envir = local, enclos = asNamespace("stats"))
        values[[sprintf("%s.g%d", nm, g)]] <- local[[nm]]
      }
    }
    values
  }
  seeded <- sieve_with_seed(seed, sieve_capture({
    d <- length(center)
    # Deviates fill a d by B matrix column by column; each row of the
    # transposed product is one draw from N(center, V).
    z <- t(matrix(stats::rnorm(B * d), d, B)) %*% root
    draws <- as.data.frame(sweep(z, 2L, center, "+"))
    names(draws) <- slot_name
    evaluate(draws)
  }))
  out <- seeded$value
  warnings <- unique(c(engine$warnings, out$warnings))
  if (!out$ok) return(list(error = out$error, warnings = warnings, rng = seeded$rng))
  samples <- out$value
  point <- evaluate(as.list(center))
  half <- (1 - conf_level) / 2
  limits <- t(vapply(effects$column, function(nm) {
    stats::quantile(samples[[nm]], c(half, 1 - half), na.rm = TRUE, names = FALSE)
  }, numeric(2)))
  table <- data.frame(
    effect = effects$effect,
    est.std = vapply(effects$column, function(nm) as.numeric(point[[nm]]), numeric(1)),
    ci.lower = limits[, 1], ci.upper = limits[, 2],
    row.names = effects$column, stringsAsFactors = FALSE
  )
  if (n_groups > 1L) {
    table <- data.frame(
      table["effect"], group = group_labels[effects$group], table[c("est.std", "ci.lower", "ci.upper")],
      stringsAsFactors = FALSE
    )
  }
  samples <- as.matrix(samples)
  counts <- list(requested = B, returned = nrow(samples), finite = colSums(is.finite(samples)))
  list(
    table = table, samples = samples, B = B, counts = counts, rng = seeded$rng,
    method = paste0(
      sprintf(
        "Monte Carlo percentile interval from %d draws of the standardized (std.all) parameters named in each definition, multivariate normal with lavaan's joint covariance of the standardized solution (vcov.std.all); each defined effect is evaluated on every draw",
        B
      ),
      if (length(each)) {
        paste(
          "; a definition whose labels are each shared across the groups, one parameter in each group, is",
          "evaluated in each group from that group's standardized parameters, drawn jointly (lavaan's own",
          "standardized value of such a definition, and its warning, concern the first group only)"
        )
      } else ""
    ),
    warnings = warnings, error = NULL
  )
}

#' Keep an additional inference under its exact parent specification
#' @param state Shared state.
#' @param kind Analysis kind.
#' @param parent Parent fitted result.
#' @param choices Exact inference choices.
#' @param estimates Plain result values.
#' @param code Replay code.
#' @return Registered analysis ID.
#' @author Ken Kelley
#' @noRd
sieve_register_inference <- function(state, kind, parent, choices, estimates, code) {
  previous <- shiny::isolate(state$analyses) %||% list()
  original <- previous[[parent$analysis_id %||% ""]]
  spec <- list(
    kind = kind,
    parent_id = original$id %||% parent$analysis_id,
    parent_spec = parent$spec,
    choices = choices
  )
  spec$key <- sieve_hash(spec)
  result <- list(
    spec = spec, revision = parent$revision %||% sieve_revision(state),
    estimates = estimates,
    status = list(
      code = "ok",
      message = "Computed from the named parent model."
    ),
    warnings = character(0)
  )
  if (!is.null(original)) result$inputs <- original$inputs
  id <- sieve_register_analysis(state, result, note = kind)
  sieve_log(state, paste(kind, id), code)
  invisible(id)
}

#' Compare a Stated Latent-Correlation Boundary With the Fitted CFA
#'
#' @description
#' Compares a fitted single-group CFA with the same model in which the
#' correlation of two factors is held at a boundary you state from theory,
#' with the engine's test of the constraint. A small, nonsignificant loss of
#' fit says the data are compatible with a correlation at the boundary; the
#' comparison is evidence about that one claim, not a verdict on
#' discriminant validity.
#'
#' @param fit An admissible single-group CFA.
#' @param pair Two engine latent names.
#' @param bound Signed correlation strictly between -1 and 1.
#' @return Constrained fit, engine test, estimates, and exact constraint syntax.
#' @author Ken Kelley
#' @examples
#' fit <- lavaan::cfa("visual =~ x1 + x2 + x3
#'                     textual =~ x4 + x5 + x6",
#'   data = lavaan::HolzingerSwineford1939, std.lv = TRUE)
#' bound <- sieve_latent_bound(fit, c("visual", "textual"), bound = .6)
#' bound$test
#' @export
sieve_latent_bound <- function(fit, pair, bound = .9) {
  if (length(pair) != 2L || anyDuplicated(pair) || !all(pair %in% lavaan::lavNames(
    fit,
    "lv"
  ))) stop("`pair` must name two distinct latent factors.",
    call. = FALSE
  )
  if (length(bound) != 1L || !is.finite(bound) || abs(bound) >= 1) stop("`bound` must lie strictly inside (-1, 1); a merged-factor boundary needs a different reference test.",
    call. = FALSE
  )
  if (lavaan::lavInspect(
    fit,
    "ngroups"
  ) != 1L || any(lavaan::parTable(fit)$op == "~")) stop("This boundary test requires a single-group CFA with exogenous factors.",
    call. = FALSE
  )
  if (!identical(
    sieve_fit_status(fit)$code,
    "ok"
  )) stop("A boundary comparison requires an admissible parent CFA.",
    call. = FALSE
  )
  pt <- lavaan::parTable(fit)
  taken <- unique(c(
    pt$label,
    pt$lhs,
    pt$rhs
  ))
  labels <- unname(sieve_allocate_names(
    c(
      "bound_cov",
      "bound_var1",
      "bound_var2"
    ),
    taken = taken
  ))
  covrow <- which(pt$op == "~~" & ((pt$lhs == pair[1] & pt$rhs == pair[2]) | (pt$lhs == pair[2] & pt$rhs == pair[1])))
  if (length(covrow) != 1L || pt$free[covrow] == 0L) stop("The selected latent covariance is not freely estimated.",
    call. = FALSE
  )
  expression <- character(2)
  for (i in seq_len(2L)) {
    j <- which(pt$op == "~~" & pt$lhs == pair[i] & pt$rhs == pair[i])
    if (length(j) != 1L) stop("A latent variance is unavailable.", call. = FALSE)
    if (pt$free[j] == 0L) expression[i] <- sieve_r_literal(pt$est[j]) else {
      if (!nzchar(pt$label[j])) pt$label[j] <- labels[i + 1L]
      expression[i] <- pt$label[j]
    }
  }
  if (!nzchar(pt$label[covrow])) pt$label[covrow] <- labels[1]
  constraint <- sprintf(
    "%s == %s * sqrt(%s * %s)",
    pt$label[covrow],
    sieve_r_literal(bound),
    expression[1],
    expression[2]
  )
  # Append the explicit equality to the fitted parameter table so custom
  # cross-loadings and identification constraints remain intact.
  extra <- pt[1L, , drop = FALSE]
  for (nm in names(extra)) extra[[nm]] <- if (is.character(extra[[nm]])) "" else if (is.logical(extra[[nm]])) FALSE else 0
  extra$id <- max(pt$id) + 1L
  extra$lhs <- pt$label[covrow]
  extra$op <- "=="
  extra$rhs <- sprintf("%s * sqrt(%s * %s)", sieve_r_literal(bound), expression[1], expression[2])
  extra$user <- 1L
  extra$ustart <- NA_real_
  pt <- rbind(pt, extra)
  opts <- lavaan::lavInspect(fit, "options")
  if (grepl("cluster", opts$se %||% "", fixed = TRUE)) {
    return(list(error = "This boundary refit cannot recover the original cluster identifiers from the fitted model. Use an explicit cluster-aware constrained model; no independent-row refit was substituted."))
  }
  # Use the fitted values/options, not unevaluated symbols in the original
  # model call. A caller's local data variable need not still exist.
  args <- list(
    model = pt, data = as.data.frame(lavaan::lavInspect(fit, "data")),
    estimator = opts$estimator.orig %||% opts$estimator, missing = opts$missing,
    meanstructure = opts$meanstructure, parameterization = opts$parameterization,
    # The standard and any corrected statistic, which the difference test
    # needs; not Browne's residual statistics, which lavaan 0.7-3 lists by
    # default and cannot compute under the nonlinear boundary constraint.
    se = opts$se, test = c("standard", sieve_lavaan_corrected_tests(opts$test)), information = opts$information
  )
  ordered <- lavaan::lavNames(fit, "ov.ord")
  if (length(ordered)) args$ordered <- ordered
  run <- sieve_capture(do.call(lavaan::lavaan, args))
  if (!run$ok) return(list(error = run$error, warnings = run$warnings, constraint = constraint))
  status <- sieve_fit_status(run$value, warnings = run$warnings)
  if (!identical(status$code, "ok")) return(list(error = status$message, status = status, constraint = constraint))
  comparison <- sieve_capture(lavaan::lavTestLRT(fit, run$value))
  ss <- lavaan::standardizedSolution(fit)
  selected <- ss[ss$op == "~~" & ((ss$lhs == pair[1] & ss$rhs == pair[2]) | (ss$lhs == pair[2] & ss$rhs == pair[1])), ,
    drop = FALSE
  ]
  summary <- data.frame(
    factor1 = sieve_show_names(pair[1], fit), factor2 = sieve_show_names(pair[2], fit),
    stated_bound = bound, estimated_correlation = selected$est.std,
    lower = selected$ci.lower, upper = selected$ci.upper,
    direction = ifelse(selected$est.std < bound, "estimate below the bound",
      ifelse(selected$est.std > bound, "estimate above the bound", "estimate at the bound")
    ),
    interval_vs_bound = ifelse(selected$ci.upper < bound, "interval entirely below the bound",
      ifelse(selected$ci.lower > bound, "interval entirely above the bound", "interval includes the bound")
    ),
    stringsAsFactors = FALSE
  )
  test_table <- as.data.frame(comparison$value)
  tested <- which(is.finite(test_table[["Chisq diff"]]))
  k <- if (length(tested)) tail(tested, 1L) else NA_integer_
  summary$chisq_difference <- if (is.na(k)) NA_real_ else test_table[["Chisq diff"]][k]
  summary$df_difference <- if (is.na(k)) NA_real_ else test_table[["Df diff"]][k]
  summary$p <- if (is.na(k)) NA_real_ else test_table[["Pr(>Chisq)"]][k]
  summary$test_method <- paste(attr(comparison$value, "heading"), collapse = " ")
  list(
    fit = run$value, test = comparison$value, estimate = selected, summary = summary, pair = pair, bound = bound,
    constraint = constraint, warnings = c(run$warnings, comparison$warnings), error = comparison$error,
    interpretation = "A comparison with the stated signed equality boundary, not an automatic validity verdict or a correlation-one merge test."
  )
}

#' Gauss-Legendre nodes and weights on the interval from -1 to 1
#'
#' The Golub and Welsch (1969) construction: the nodes are the eigenvalues
#' of the symmetric tridiagonal Jacobi matrix of the Legendre polynomials,
#' and each weight is twice the squared first component of its eigenvector.
#'
#' @param n Number of nodes.
#' @return A list with `x` (nodes) and `w` (weights).
#' @author Ken Kelley
#' @noRd
sieve_gauss_legendre <- function(n) {
  i <- seq_len(n - 1L)
  off <- i / sqrt(4 * i^2 - 1)
  J <- matrix(0, n, n)
  J[cbind(i, i + 1L)] <- off
  J[cbind(i + 1L, i)] <- off
  e <- eigen(J, symmetric = TRUE)
  list(x = e$values, w = 2 * e$vectors[1L, ]^2)
}

#' The bivariate standard normal distribution function
#'
#' `P(Z1 <= h, Z2 <= k)` for standard normal variables with correlation
#' `r`, by the Gauss-Legendre method of Drezner and Wesolowsky (1990) with
#' the refinements of Genz (2004): 6, 12, or 20 nodes as `|r|` grows, an
#' integral over `asin(r)` when `|r| < .925`, and for larger `|r|` a series
#' with a correction integral that stays accurate up to `|r| = 1`, where the
#' limit is exact. Absolute error is near machine precision.
#'
#' @param h,k Upper limits (recycled).
#' @param r Correlations in `[-1, 1]` (recycled).
#' @return The probabilities.
#' @author Ken Kelley
#' @noRd
sieve_pbinorm <- function(h, k, r) {
  n <- max(length(h), length(k), length(r))
  # Genz's form is the upper orthant P(X > a, Y > b), with a = -h, b = -k.
  a <- rep_len(-h, n)
  b <- rep_len(-k, n)
  r <- pmin(pmax(rep_len(r, n), -1), 1)
  out <- numeric(n)
  ab <- a * b
  rules <- list(sieve_gauss_legendre(6L), sieve_gauss_legendre(12L), sieve_gauss_legendre(20L))
  size <- ifelse(abs(r) < .3, 1L, ifelse(abs(r) < .75, 2L, 3L))
  low <- abs(r) < .925
  for (s in 1:3) {
    i <- which(low & size == s)
    if (!length(i)) next
    rule <- rules[[s]]
    half <- (a[i]^2 + b[i]^2) / 2
    asr <- asin(r[i])
    sn <- sin(outer(asr, rule$x + 1) / 2)
    f <- exp((sn * ab[i] - half) / (1 - sn^2))
    out[i] <- drop(f %*% rule$w) * asr / (4 * pi) + stats::pnorm(-a[i]) * stats::pnorm(-b[i])
  }
  i <- which(!low)
  if (length(i)) {
    rule <- rules[[3L]]
    neg <- r[i] < 0
    bb <- ifelse(neg, -b[i], b[i])
    abb <- ifelse(neg, -ab[i], ab[i])
    val <- numeric(length(i))
    inner <- abs(r[i]) < 1
    if (any(inner)) {
      j <- which(inner)
      rr <- r[i][j]
      aa <- a[i][j]
      bj <- bb[j]
      hk <- abb[j]
      as <- (1 - rr) * (1 + rr)
      A <- sqrt(as)
      bs <- (aa - bj)^2
      cc <- (4 - hk) / 8
      dd <- (12 - hk) / 16
      v <- A * exp(-(bs / as + hk) / 2) * (1 - cc * (bs - as) * (1 - dd * bs / 5) / 3 + cc * dd * as * as / 5)
      far <- hk > -160
      if (any(far)) {
        B <- sqrt(bs[far])
        v[far] <- v[far] - exp(-hk[far] / 2) * sqrt(2 * pi) * stats::pnorm(-B / A[far]) * B *
          (1 - cc[far] * bs[far] * (1 - dd[far] * bs[far] / 5) / 3)
      }
      A <- A / 2
      xs <- outer(A, rule$x + 1)^2
      rs <- sqrt(1 - xs)
      g <- exp(-bs / (2 * xs) - hk / (1 + rs)) / rs - exp(-(bs / xs + hk) / 2) * (1 + cc * xs * (1 + dd * xs))
      v <- v + A * drop(g %*% rule$w)
      val[j] <- -v / (2 * pi)
    }
    pos <- !neg
    val[pos] <- val[pos] + stats::pnorm(-pmax(a[i][pos], bb[pos]))
    val[neg] <- -val[neg] + pmax(0, stats::pnorm(-a[i][neg]) - stats::pnorm(-bb[neg]))
    out[i] <- val
  }
  out
}

#' Green and Yang's reliability of an observed categorical composite
#'
#' The reliability of a weighted sum of ordered item scores from a model for
#' their latent responses (Green & Yang, 2009). Item `j` is scored
#' `X_j = s_j0 + sum_c d_jc 1[X*_j > t_jc]`, where `d_jc` is the score
#' increment between adjacent observed categories (its step weight): the
#' difference between adjacent codes for the sum of the codes as given, and
#' 1 at every threshold for the sum of consecutive category ranks (each
#' item's categories scored 0, 1, ..., C - 1 in the order of its codes,
#' Green and Yang's Eq. 11 and the target of
#' `DMAR::reliability_omega_categorical()`). The covariance of two item
#' scores at latent response correlation `rho` is
#' `sum_c sum_c' d_jc d_kc' [Phi2(t_jc, t_kc'; rho) - Phi(t_jc) Phi(t_kc')]`;
#' the coefficient is the weighted sum of these covariances at the model's
#' common-factor correlations over the same sum at the latent response
#' correlations of the denominator. The two scorings give the same
#' coefficient when every item's codes step by one shared amount.
#'
#' @param true_cor Common-factor part of the latent response correlations.
#' @param total_cor Latent response correlations for the denominator.
#' @param thresholds List of threshold vectors on the standardized latent
#'   response scale (mean 0, variance 1), one per item.
#' @param weights Item weights.
#' @param steps Optional list of step weights, one vector per item with one
#'   increment per threshold; `NULL` gives unit steps (consecutive ranks).
#' @return The coefficient.
#' @author Ken Kelley
#' @noRd
sieve_green_yang <- function(true_cor, total_cor, thresholds, weights, steps = NULL) {
  p <- length(thresholds)
  if (is.null(steps)) steps <- lapply(thresholds, function(t) rep(1, length(t)))
  if (length(steps) != p || any(lengths(steps) != lengths(thresholds))) {
    stop("Each item needs one step weight per threshold.", call. = FALSE)
  }
  pairs <- expand.grid(j = seq_len(p), jp = seq_len(p))
  cells <- do.call(rbind, lapply(seq_len(nrow(pairs)), function(q) {
    g <- expand.grid(a = seq_along(thresholds[[pairs$j[q]]]), b = seq_along(thresholds[[pairs$jp[q]]]))
    data.frame(
      pair = q, t1 = thresholds[[pairs$j[q]]][g$a], t2 = thresholds[[pairs$jp[q]]][g$b],
      d = steps[[pairs$j[q]]][g$a] * steps[[pairs$jp[q]]][g$b]
    )
  }))
  at <- cbind(pairs$j, pairs$jp)[cells$pair, , drop = FALSE]
  joint_true <- tapply(cells$d * sieve_pbinorm(cells$t1, cells$t2, true_cor[at]), cells$pair, sum)
  joint_total <- tapply(cells$d * sieve_pbinorm(cells$t1, cells$t2, total_cor[at]), cells$pair, sum)
  margin <- vapply(seq_len(p), function(i) sum(steps[[i]] * stats::pnorm(thresholds[[i]])), numeric(1))
  product <- margin[pairs$j] * margin[pairs$jp]
  ww <- weights[pairs$j] * weights[pairs$jp]
  sum(ww * (joint_true - product)) / sum(ww * (joint_total - product))
}

#' The category codes of the ordered items of a fitted model
#'
#' The codes each ordered item showed in the data the model was fitted to,
#' in the order of its categories: lavaan keeps the level labels of an
#' ordered variable with the fitted data (its `Data` slot; lavaan 0.6 and
#' 0.7 offer no `lavInspect()` accessor for them). Labels that are not all
#' numbers (`"Agree"`) define no codes, so that item's categories can only
#' be scored as consecutive ranks.
#'
#' @param fit A fitted lavaan model.
#' @return A named list, one element per ordered item: its numeric codes,
#'   or `NULL` when its labels are not numbers or cannot be read.
#' @author Ken Kelley
#' @noRd
sieve_fit_category_codes <- function(fit) {
  ov <- tryCatch(fit@Data@ov, error = function(e) NULL)
  if (!is.list(ov) || is.null(ov$name) || is.null(ov$lnam)) return(list())
  ord <- which(ov$type %in% "ordered")
  stats::setNames(lapply(ord, function(i) {
    labels <- strsplit(as.character(ov$lnam[i]), "|", fixed = TRUE)[[1L]]
    codes <- suppressWarnings(as.numeric(labels))
    if (length(codes) < 2L || anyNA(codes) || is.unsorted(codes, strictly = TRUE)) NULL else codes
  }), ov$name[ord])
}

#' Model-linked reliability of weighted composites, in closed form
#'
#' For each factor measured by observed items, and for all modeled items
#' together, the reliability of the weighted composite of its items: the
#' composite's true-score variance implied by the fitted model over the
#' composite's observed variance. The true-score covariance of the items is
#' `Lambda A Psi* A' Lambda'`, with `A = (I - B)^-1`; `Psi*` keeps the
#' variances and covariances of the factors that no other factor explains
#' and sets the disturbances of explained factors to zero, so in a
#' higher-order model the second-order factors carry the true score and the
#' first-order disturbances, like the residual variances and covariances of
#' the items, count as error. The denominator is the observed (unrestricted)
#' covariance matrix of the items (`lavInspect(fit, "sampstat")`, which
#' under full information maximum likelihood is the EM estimate). A
#' composite whose items are all ordered gets Green and Yang's (2009)
#' coefficient for the weighted sum of the item codes as given in the
#' fitted data (`sieve_fit_category_codes()`), each threshold step weighted
#' by the increment between adjacent codes (`sieve_green_yang()`), with the
#' latent response correlations of the fit and the model-implied
#' standardized thresholds of each group,
#' `lavInspect(fit, "th")`, which are `(tau - nu - Lambda (I - B)^-1 alpha)`
#' times the scaling factor, so groups and occasions with nonzero latent
#' means or latent response intercepts are handled. When the codes do not
#' step by one shared amount on every item of the composite, the weighted
#' sum of the codes and the weighted sum of consecutive category ranks
#' (the composite DMAR's categorical omega describes) are different
#' composites: the coefficient of the codes is reported, the rank
#' coefficient is returned beside it in `rank`, and a note names the codes.
#' An item whose categories have no numeric codes (text labels) is scored
#' by its ranks, and a note says so. A composite mixing ordered and
#' continuous items gets none. A factor with a single indicator is not a
#' composite and is skipped.
#'
#' @param fit A fitted single-level lavaan model.
#' @param weights Item weights, named by observed variable.
#' @return A list with `value` (named coefficients), `rank` (the rank
#'   coefficient of each ordered composite whose codes do not step by one
#'   shared amount, named as `value`), and `notes`.
#' @author Ken Kelley
#' @noRd
sieve_composite_coefficients <- function(fit, weights) {
  refusal <- sieve_h1_refusal(fit)
  if (!is.null(refusal)) stop(refusal, call. = FALSE)
  if (lavaan::lavInspect(fit, "nlevels") > 1L) {
    stop("Model-linked composite reliability is computed for single-level models.", call. = FALSE)
  }
  ng <- lavaan::lavInspect(fit, "ngroups")
  group_label <- if (ng > 1L) lavaan::lavInspect(fit, "group.label") else NULL
  est <- lavaan::lavInspect(fit, "est", drop.list.single.group = FALSE)
  observed <- lavaan::lavInspect(fit, "sampstat", drop.list.single.group = FALSE)
  pt <- lavaan::parTable(fit)
  loads <- pt[pt$op == "=~", , drop = FALSE]
  ordered <- lavaan::lavNames(fit, "ov.ord")
  composites <- c(unique(loads$lhs), ".TOTAL.")
  total_name <- "All modeled items (multidimensional common-factor variance share)"
  value <- numeric(0)
  rank <- numeric(0)
  notes <- character(0)
  # The model-implied standardized thresholds of each group and the codes of
  # the ordered items in the fitted data (both read once).
  th_all <- if (length(ordered)) lavaan::lavInspect(fit, "th", drop.list.single.group = FALSE)
  codes <- if (length(ordered)) sieve_fit_category_codes(fit) else list()
  kind <- if (all(weights == 1)) "unit-weighted" else "weighted"
  for (cc in composites) {
    for (g in seq_len(ng)) {
      m <- est[[g]]
      lambda <- m$lambda
      ov <- rownames(lambda)
      if (identical(cc, ".TOTAL.")) {
        items <- intersect(ov, unique(loads$rhs[loads$group == g]))
      } else {
        rows <- loads[loads$lhs == cc & loads$group == g, , drop = FALSE]
        if (nrow(rows) < 2L) next
        items <- intersect(ov, rows$rhs)
      }
      if (!length(items)) next
      w <- weights[items]
      if (all(w == 0)) next
      psi <- m$psi
      L <- lambda
      if (!is.null(m$beta) && any(m$beta != 0)) {
        explained <- rownames(m$beta)[rowSums(m$beta != 0) > 0]
        psi[explained, ] <- 0
        psi[, explained] <- 0
        L <- lambda %*% solve(diag(nrow(m$beta)) - m$beta)
      }
      true <- (L %*% psi %*% t(L))[items, items, drop = FALSE]
      total <- observed[[g]]$cov[items, items, drop = FALSE]
      label <- if (identical(cc, ".TOTAL.")) total_name else cc
      if (ng > 1L) label <- sprintf("%s (%s)", label, group_label[g])
      is_ordered <- items %in% ordered
      if (all(is_ordered)) {
        # The thresholds are lavaan's model-implied standardized thresholds of
        # this group, (tau - nu - Lambda (I - B)^-1 alpha) times the scaling
        # factor, on the latent response scale of mean 0 and variance 1 that
        # Green and Yang's formula needs; tau times the scaling factor alone
        # is wrong wherever a latent mean or intercept is not zero.
        scale <- m$delta[items, 1L]
        th <- th_all[[g]]
        th_item <- sub("[|].*$", "", names(th))
        thresholds <- lapply(items, function(i) as.numeric(th[th_item == i]))
        true_cor <- diag(scale, length(items)) %*% true %*% diag(scale, length(items))
        total_cor <- stats::cov2cor(total)
        # Each item is scored by its codes as given: the step weight of a
        # threshold is the increment between the adjacent codes. An item
        # without numeric codes, or whose codes do not match its thresholds,
        # is scored by its ranks.
        item_codes <- lapply(items, function(i) codes[[i]])
        known <- vapply(seq_along(items), function(i) {
          length(item_codes[[i]]) == length(thresholds[[i]]) + 1L
        }, logical(1))
        steps <- lapply(seq_along(items), function(i) {
          if (known[i]) diff(item_codes[[i]]) else rep(1, length(thresholds[[i]]))
        })
        ranked <- sieve_green_yang(true_cor, total_cor, thresholds, w)
        # An item of weight zero is not in the composite, so its codes do not
        # decide whether the two composites differ.
        weighted <- w != 0
        if (sieve_coding_shared(steps[weighted])) {
          # One shared step: the sum of the codes is a rescaling of the sum of
          # the ranks, so the two have the same reliability.
          estimate <- ranked
        } else {
          estimate <- sieve_green_yang(true_cor, total_cor, thresholds, w, steps)
          rank[label] <- ranked
          flagged <- rep(FALSE, length(items))
          flagged[weighted] <- sieve_coding_flagged(steps[weighted])
          shown <- items[known & flagged]
          if (!length(shown)) shown <- items[known & weighted]
          notes <- c(notes, sprintf(
            paste0(
              "%s: the %s sum of the item codes as given in the fitted data is not the %s sum of consecutive ",
              "category ranks, because the codes do not step by one shared amount on every item (%s). ",
              "Green and Yang's (2009) coefficient is reported for the codes, each threshold step weighted by ",
              "the increment between adjacent codes: %.3f. The coefficient of the rank composite (each item's ",
              "categories scored 0, 1, ..., C - 1 in the order of its codes, the composite DMAR's categorical ",
              "omega describes) is %.3f, shown beside it."
            ),
            label, kind, kind,
            sieve_coding_groups(shown, vapply(item_codes[match(shown, items)], function(x) {
              paste(sieve_num_text(x), collapse = ", ")
            }, character(1)), verb = "coded"),
            estimate, ranked
          ))
        }
        if (any(!known)) {
          notes <- c(notes, sprintf(
            paste0(
              "%s: %s %s no numeric codes in the fitted data that match %s thresholds, so %s categories are ",
              "scored as consecutive ranks in the order of %s levels."
            ),
            label, sieve_and_text(items[!known]), if (sum(!known) == 1L) "has" else "have",
            if (sum(!known) == 1L) "its" else "their", if (sum(!known) == 1L) "its" else "their",
            if (sum(!known) == 1L) "its" else "their"
          ))
        }
      } else if (any(is_ordered)) {
        notes <- c(notes, sprintf(
          "%s mixes ordered and continuous items, so it has no composite reliability here; fit the items under one treatment.",
          label
        ))
        next
      } else {
        estimate <- as.numeric(crossprod(w, true %*% w) / crossprod(w, total %*% w))
      }
      value[label] <- estimate
    }
  }
  list(value = value, rank = rank, notes = notes)
}

#' Identify the Substantive Indicators in Their Modeled Order
#' @param fit Fitted lavaan object.
#' @return Observed indicator names, excluding auxiliary variables.
#' @author Ken Kelley
#' @noRd
sieve_model_composite_items <- function(fit) {
  pt <- lavaan::parTable(fit)
  intersect(lavaan::lavNames(fit, "ov"), unique(pt$rhs[pt$op == "=~"]))
}

#' Reliability of Weighted Composites in the Selected Fitted CFA
#'
#' The model-linked (composite) reliability of each construct's weighted
#' composite and of all modeled items together, from the selected CFA as it
#' was fitted, residual covariances included: the composite's true-score
#' variance implied by the model over its observed variance. For weights
#' `w` the coefficient is `w' T w / w' S w`. `T = Lambda A Psi* A' Lambda'`
#' is the items' true-score covariance, with `A = (I - B)^-1` and `Psi*`
#' keeping the variances and covariances of the factors that no other factor
#' explains, so in a higher-order model the second-order factors carry the
#' true score and the first-order disturbances, like the items' residual
#' variances and covariances, count as error. `S` is the observed
#' (unrestricted) covariance matrix of the items, the EM estimate under full
#' information maximum likelihood. Composites of ordered items get Green
#' and Yang's (2009) coefficient for the weighted sum of the item codes as
#' given in the fitted data, each threshold step weighted by the increment
#' between adjacent codes, with the model-implied standardized thresholds of
#' each group, `(tau - nu - Lambda (I - B)^-1 alpha)` times the scaling
#' factor (`lavaan::lavInspect(fit, "th")`), so groups and occasions with
#' nonzero latent means are handled. When every item's codes step by one
#' shared amount this is also the coefficient of the weighted sum of
#' consecutive category ranks (each item's categories scored 0, 1, ...,
#' C - 1 in the order of its codes), the composite
#' `DMAR::reliability_omega_categorical()` describes. When they do not (an
#' unused category between two used ones, codes such as 1, 2, 5, 10, or
#' steps that differ between items), the two are different composites: the
#' table reports the coded composite and adds `rank_estimate`, the rank
#' coefficient beside it, and the warnings name the codes. An item whose
#' categories carry text labels is scored by its ranks. A factor with one
#' indicator is not a composite and is skipped. An optional percentile
#' bootstrap refits the model on resampled rows; each replication scores
#' the categories it holds by their codes.
#'
#' @param fit The selected CFA.
#' @param weights Optional item weights in modeled item order (or named by
#'   item); `NULL` gives unit weights.
#' @param B Optional counted bootstrap size, zero for the estimate only.
#' @param seed Optional seed.
#' @param conf_level Confidence level.
#' @return Coefficients/intervals and their actual scale, model, and replication counts.
#' @author Ken Kelley
#' @examples
#' fit <- lavaan::cfa("visual =~ x1 + x2 + x3
#'                     textual =~ x4 + x5 + x6",
#'   data = lavaan::HolzingerSwineford1939, std.lv = TRUE)
#' composite <- sieve_model_composite(fit)
#' composite$table
#' composite$interval_reason
#' @export
sieve_model_composite <- function(fit, weights = NULL, B = 0L, seed = NULL, conf_level = .95) {
  if (length(B) != 1L || !is.finite(B) || B < 0 || B != as.integer(B)) stop("`B` must be zero or a positive integer.",
    call. = FALSE
  )
  conf_level <- sieve_check_conf_level(conf_level)
  ov <- sieve_model_composite_items(fit)
  unit <- is.null(weights)
  if (!unit) {
    if (!is.null(names(weights))) weights <- weights[ov]
    if (length(weights) != length(ov) || any(!is.finite(weights))) stop("Weights must cover all modeled items with finite numbers.",
      call. = FALSE
    )
    if (all(weights == 0)) stop("At least one weight must be nonzero.", call. = FALSE)
    unit <- all(weights == 1)
  }
  weights <- stats::setNames(if (is.null(weights)) rep(1, length(ov)) else as.numeric(weights), ov)
  if (!identical(sieve_fit_status(fit)$code, "ok")) return(list(error = "The selected CFA has no admissible solution."))
  compute <- function(object) {
    values <- sieve_composite_coefficients(object, weights)$value
    improper <- !isTRUE(tryCatch(sieve_model_post_check(object), error = function(e) FALSE))
    c(values, .improper = as.numeric(improper))
  }
  point <- sieve_capture(sieve_composite_coefficients(fit, weights))
  if (!point$ok) return(list(error = point$error, warnings = point$warnings))
  value <- point$value$value
  if (!length(value)) return(list(error = paste(c("No composite of two or more items has a reliability here.", point$value$notes), collapse = " ")))
  tab <- data.frame(
    composite = names(value),
    estimate = as.numeric(value),
    lower = NA_real_,
    upper = NA_real_,
    used = NA_integer_
  )
  kind <- if (unit) "unit-weighted" else "weighted"
  # Every value names its denominator:
  # the observed (unrestricted) covariance matrix of the items, or for an
  # ordered composite the unrestricted polychoric correlations with the
  # thresholds. The numerator is the selected multi-factor CFA's, so this is
  # not the robust omega of a construct's own one-factor model, which the
  # Reliability tab reports.
  label <- if (sieve_fit_is_categorical(fit)) {
    "Model-linked composite reliability (observed denominator: unrestricted polychoric correlations)"
  } else {
    "Model-linked composite reliability (observed denominator)"
  }
  tab$coefficient <- label
  # The rank coefficient beside a coded composite whose codes do not step by
  # one shared amount (the target of DMAR's categorical omega).
  ranked <- point$value$rank %||% numeric(0)
  if (length(ranked)) tab$rank_estimate <- unname(ranked[tab$composite])
  tab <- tab[, c("composite", "coefficient", "estimate", "lower", "upper", "used", if (length(ranked)) "rank_estimate")]
  out <- list(
    table = tab, weights = weights, label = label,
    target = if (sieve_fit_is_categorical(fit)) {
      sprintf("Observed ordinal %s score: the %s sum of the item codes as given in the fitted data (threshold-adjusted)", kind, kind)
    } else {
      sprintf("Observed %s item score", kind)
    },
    model = attr(
      fit,
      "sieve_spec"
    ),
    method = paste0(
      label, " in closed form: the composite's true-score variance from the fitted model (its common factors; residual variances and covariances count as error) ",
      "over the composite's observed variance (the observed denominator: the unrestricted covariance matrix of the items, the EM estimate under FIML)",
      if (sieve_fit_is_categorical(fit)) {
        paste0(
          "; ordered composites use Green and Yang's (2009) form for the ", kind, " sum of the item codes, each ",
          "threshold step weighted by the increment between adjacent codes (with codes that step by one shared ",
          "amount on every item this is also the coefficient of the sum of consecutive category ranks, each item's ",
          "categories scored 0, 1, ..., C - 1 in the order of its codes, which DMAR's categorical omega reports), ",
          "with the model-implied standardized thresholds of each group and the denominator built from the ",
          "unrestricted polychoric correlations and the thresholds"
        )
      } else {
        ""
      },
      ". The numerator comes from the selected CFA of all its factors, so a construct's value here is not ",
      "the Robust omega (observed denominator) of its own one-factor model, which the Reliability tab reports."
    ),
    warnings = c(point$warnings, point$value$notes,
      if (any(value < 0 | value > 1)) "A model-linked composite estimate is outside [0, 1]. Review the model and weights before interpreting it."),
    error = NULL,
    interval_reason = "Bootstrap not requested; the point estimate has no closed-form interval."
  )
  if (B > 0L) {
    options <- lavaan::lavInspect(fit, "options")
    aux_request <- options$aux %||% tryCatch(lavaan::lavInspect(fit, "call")$aux, error = function(e) NULL)
    if (options$missing %in% c("two.stage", "robust.two.stage") && length(aux_request)) {
      out$interval_reason <- paste(
        "Two-stage composite intervals with separately stored auxiliaries require validated aligned auxiliary resampling.",
        "That replay route is not supplied here. The eligible point is retained; use FIML or a separately verified bootstrap.")
      return(out)
    }
    if (grepl("cluster",
      lavaan::lavInspect(
        fit,
        "options"
      )$se,
      fixed = TRUE
    )) {
      held <- attr(fit, "sieve_bootstrap_data", exact = TRUE)
      spec <- attr(fit, "sieve_spec", exact = TRUE)
      column <- spec$engine_args$cluster
      if (is.null(held) || is.null(spec) || length(column) != 1L || !column %in% names(held)) {
        out$interval_reason <- "The clustered fit has no verified held data and cluster labels. Refit it in SIEVER before requesting whole-cluster intervals."
        return(out)
      }
    }
    if (length(B) != 1L || !is.finite(B) || B < 100L || B != as.integer(B)) stop("`B` must be zero or an integer of at least 100.",
      call. = FALSE
    )
    clustered <- grepl("cluster", lavaan::lavInspect(fit, "options")$se, fixed = TRUE)
    draw <- sieve_with_seed(seed, sieve_capture(if (clustered) {
      columns <- unique(c(spec$observed, spec$engine_args$aux))
      cases <- lavaan::lavInspect(fit, "case.idx")
      if (!is.numeric(cases)) stop("The clustered bootstrap needs a single-group model with verifiable case indices.", call. = FALSE)
      keep <- seq_len(nrow(held)) %in% cases & rowSums(!is.na(held[, columns, drop = FALSE])) > 0L
      held <- held[keep, , drop = FALSE]
      partition <- sieve_bootstrap_partition(nrow(held), held[[column]], which(keep))
      out$sampling <- partition
      args <- sieve_spec_args(spec)
      args$cluster <- NULL
      args$se <- "none"
      args$test <- "none"
      seeds <- sample.int(.Machine$integer.max, as.integer(B))
      values <- lapply(seeds, function(s) sieve_with_seed(s, {
        rows <- sieve_bootstrap_rows(partition)
        inner <- sieve_capture(do.call(if (identical(spec$fun, "sem")) lavaan::sem else lavaan::cfa,
          c(args, list(data = held[rows, , drop = FALSE]))))
        if (!inner$ok || !isTRUE(lavaan::lavInspect(inner$value, "converged")) ||
            !identical(sieve_information_check(inner$value)$singular, FALSE)) {
          rep(NA_real_, length(value) + 1L)
        } else {
          attempt <- sieve_capture(compute(inner$value))
          if (attempt$ok) attempt$value else rep(NA_real_, length(value) + 1L)
        }
      })$value)
      out$draw_seeds <- seeds
      out$method <- paste(out$method, "Percentile intervals resample", sieve_bootstrap_unit_text(partition), ".")
      out$warnings <- c(out$warnings, partition$caution)
      do.call(rbind, values)
    } else lavaan::bootstrapLavaan(fit, R = as.integer(B), FUN = compute)))
    out$rng <- draw$rng
    out$warnings <- c(out$warnings, draw$value$warnings)
    if (!draw$value$ok) {
      out$interval_reason <- draw$value$error
      return(out)
    }
    raw <- as.matrix(draw$value$value)
    failed <- unique(attr(raw, "error.idx"))
    improper <- raw[, ncol(raw)] == 1
    M <- raw[, -ncol(raw), drop = FALSE]
    if (length(failed)) M[failed, ] <- NA_real_
    out$counts <- list(
      attempted = as.integer(B), engine_errors = if (clustered) sum(!is.finite(raw[, ncol(raw)])) else length(failed),
      nonadmissible = sum(improper, na.rm = TRUE)
    )
    out$draws <- M
    out$replications <- data.frame(
      composite = out$table$composite, attempted = as.integer(B),
      used = 0L, failed = 0L, improper_kept = 0L, caution = FALSE
    )
    for (j in seq_len(ncol(M))) {
      ok <- is.finite(M[, j])
      used <- sum(ok)
      kept <- sum(ok & (improper | M[, j] < 0 | M[, j] > 1), na.rm = TRUE)
      out$table$used[j] <- used
      out$replications$used[j] <- used
      out$replications$failed[j] <- B - used
      out$replications$improper_kept[j] <- kept
      affected <- B - used + kept
      out$replications$caution[j] <- affected > 0L &&
        affected >= B * sieve_bootstrap_caution_share(conf_level) - 1e-9
      if (used >= sieve_bootstrap_min_draws(conf_level)) {
        lim <- stats::quantile(M[ok, j], c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2), names = FALSE)
        out$table$lower[j] <- lim[1]
        out$table$upper[j] <- lim[2]
      }
    }
    out$interval_reason <- if (all(is.finite(out$table$lower))) NULL else "Too few finite converged draws for at least one composite."
    out$method <- paste(out$method, "; counted percentile bootstrap, failed engine fits omitted; finite converged improper estimates retained and counted")
  }
  out
}

#' Congeneric AIPE Planning Through the Existing DMAR Engine
#'
#' @description
#' Plans the sample size a study needs for its confidence interval for
#' population omega to be narrow, in the accuracy in parameter estimation
#' (AIPE) approach of Terry and Kelley (2012): given the loadings and
#' uniquenesses of a congeneric model and a target interval width, it
#' returns the necessary N from `DMAR::ss_aipe_reliability()`, with the
#' same plan for loadings 20 percent weaker and 20 percent stronger.
#'
#' @details
#' **The interval planned for.** DMAR's maximum likelihood delta method
#' interval for model-implied omega under the congeneric (one-factor)
#' model, `(sum(lambda))^2 / ((sum(lambda))^2 + sum(psi))`, at
#' `conf_level`; `width` is the full width, upper limit minus lower limit.
#' Without `assurance` the plan is the N whose expected width is at most
#' `width`. With `assurance` (a proportion such as 0.80) it is the N at
#' which at least that proportion of intervals is no wider than `width`,
#' found by DMAR's Monte Carlo search. This is not the interval the
#' Reliability page reports by default (omega with the observed
#' denominator and its own interval method; Kelley & Pornprasertmanit,
#' 2016), so the plan is for the model-implied omega's interval.
#'
#' **Sensitivity.** The rows of `sensitivity` multiply every loading by
#' 0.8, 1 and 1.2 with the uniquenesses held fixed, so each row also
#' changes population omega, given in `population_omega`; the row with
#' multiplier 1 is the plan itself. Every row answers the same assurance
#' question as the plan.
#'
#' **Missing data.** `fmi` is the anticipated fraction of missing
#' information for omega, not the proportion of missing responses; DMAR
#' maps the complete-data plan to it.
#'
#' **Cost.** Without `assurance` each plan is a closed-form computation.
#' With `assurance` the Monte Carlo search runs once per sensitivity row,
#' three times in all, each with DMAR's replications (G = 5,000 by
#' default); a call with `assurance = 0.8` and `fmi = 0.2` ran for minutes.
#'
#' **Refusals.** An error for `fmi` outside [0, 1), and for fewer than three
#' loadings, loadings and uniquenesses of different lengths, values that
#' are not finite, or a uniqueness that is not positive. A refusal by DMAR
#' (an infeasible width, say) is returned, not thrown: the list then holds
#' `error`, `warnings`, `call` and `rng`.
#'
#' @param lambda Population loadings.
#' @param psi Population uniquenesses.
#' @param width Target full interval width.
#' @param conf_level Confidence level.
#' @param assurance Optional assurance: `NULL` for the expected width, or
#'   the proportion of intervals that must be no wider than `width`.
#' @param seed Optional seed, used for the Monte Carlo search; `NULL` draws
#'   one and records it in `rng`.
#' @param fmi Anticipated fraction of missing information for omega, from zero
#'   to less than one; distinct from the fraction of missing responses.
#' @return A list:
#'   \describe{
#'     \item{`result`}{DMAR's result as it returned it, a table of `term`
#'       and `value` with `necessary_N` (and, with assurance, the
#'       assurance attained and the replications used).}
#'     \item{`sensitivity`}{One row per loading multiplier:
#'       `loading_multiplier`, `assurance`, `fmi`, `N` (the necessary N, or
#'       `NA`), `reason` (`""`, or DMAR's refusal) and `population_omega`.}
#'     \item{`call`}{The arguments passed to `DMAR::ss_aipe_reliability()`.}
#'     \item{`rng`}{The seed used, whether it was supplied, and the
#'       generator.}
#'     \item{`warnings`}{DMAR's warnings.}
#'     \item{`error`}{`NULL`.}
#'     \item{`target`}{The design planned for, in words.}
#'   }
#' @references
#' Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for
#' population reliability coefficients: Evaluation of methods,
#' recommendations, and software for composite measures. *Psychological
#' Methods, 21*(1), 69--92. \doi{10.1037/a0040086}
#'
#' Terry, L., & Kelley, K. (2012). Sample size planning for composite
#' reliability coefficients: Accuracy in parameter estimation via narrow
#' confidence intervals. *British Journal of Mathematical and Statistical
#' Psychology, 65*(3), 371--401.
#' \doi{10.1111/j.2044-8317.2011.02030.x}
#' @seealso [sieve_reliability_construct()] for the omega and the interval
#'   the Reliability page reports.
#' @author Ken Kelley
#' @examples
#' plan <- sieve_plan_congeneric(lambda = c(.8, .7, .6, .7), psi = c(.36, .51, .64, .51),
#'   width = .1, seed = 1)
#' plan$result
#' plan$sensitivity
#' @export
sieve_plan_congeneric <- function(lambda, psi, width = .1, conf_level = .95, assurance = NULL, seed = NULL, fmi = 0) {
  conf_level <- sieve_check_conf_level(conf_level)
  if (length(fmi) != 1L || !is.finite(fmi) || fmi < 0 || fmi >= 1) stop("Planning FMI must be in [0, 1).", call. = FALSE)
  if (!is.numeric(lambda) || !is.numeric(psi) || length(lambda) < 3L || length(lambda) != length(psi) ||
    any(!is.finite(lambda)) || any(!is.finite(psi)) || any(psi <= 0)) stop("Enter at least three finite loadings and positive uniquenesses of the same length.",
    call. = FALSE
  )
  call <- list(
    model = "Congeneric", type = "Factor Analytic", width = width,
    n_items = length(lambda), true_factor_loading = lambda, true_residual_var = psi, conf_level = conf_level, assurance = assurance
  )
  call$fmi <- fmi
  seeded <- sieve_with_seed(seed, sieve_capture(do.call(DMAR::ss_aipe_reliability, call)))
  if (!seeded$value$ok) return(list(
    error = seeded$value$error,
    warnings = seeded$value$warnings,
    call = call,
    rng = seeded$rng
  ))
  grid <- lapply(c(.8, 1, 1.2), function(scale) {
    args <- call
    args$true_factor_loading <- lambda * scale
    # Every row answers the same assurance question as the headline plan.
    r <- if (identical(scale, 1)) seeded$value else sieve_with_seed(
      seeded$rng$seed,
      sieve_capture(do.call(DMAR::ss_aipe_reliability, args))
    )$value
    # Population omega of the row's model (the loadings scaled, the
    # uniquenesses as given), so that each N is read against the omega it
    # plans for.
    scaled <- sum(lambda * scale)^2
    data.frame(
      loading_multiplier = scale,
      assurance = assurance %||% NA_real_, fmi = fmi,
      N = if (r$ok) r$value$value[r$value$term == "necessary_N"] else NA_real_,
      reason = if (r$ok) "" else r$error,
      population_omega = scaled / (scaled + sum(psi))
    )
  })
  list(
    result = seeded$value$value, sensitivity = do.call(rbind, grid), call = call, rng = seeded$rng,
    warnings = seeded$value$warnings, error = NULL,
    target = "Continuous congeneric population model; the interval/design is DMAR's supported ML factor analytic delta method. Planning FMI describes information loss for omega, not the proportion of missing responses; the exact complete-data plan and FMI mapping are retained from DMAR. Categorical and bootstrap targets require a separately validated design."
  )
}

#' Actual SEM row flow, keeping overlapping missingness counts explicit
#' @param data The model data.
#' @param spec Resolved SEM specification.
#' @param fit Optional fitted model.
#' @return A descriptive row-count table.
#' @author Ken Kelley
#' @noRd
sieve_sem_row_flow <- function(data, spec, fit = NULL) {
  x <- intersect(names(spec$pred_map %||% character(0)), names(data))
  y <- setdiff(intersect(spec$observed, names(data)), x)
  xmissing <- if (length(x)) rowSums(is.na(data[, x, drop = FALSE])) > 0 else rep(FALSE, nrow(data))
  yseen <- rowSums(!is.na(data[, y, drop = FALSE]))
  counts <- c(
    input_rows = nrow(data),
    complete_on_all_modeled = sum(stats::complete.cases(data[,
      spec$observed,
      drop = FALSE
    ])),
    missing_observed_covariate = sum(xmissing), partially_observed_response = sum(yseen > 0 & yseen < length(y)),
    no_observed_response = sum(yseen == 0),
    engine_rows_used = if (is.null(fit)) NA_real_ else sum(lavaan::lavInspect(
      fit,
      "nobs"
    ))
  )
  data.frame(
    category = names(counts), n = as.numeric(counts),
    note = c(
      "",
      "",
      "Missingness categories overlap; not sequential deletions.",
      "",
      "",
      paste(
        "Actual fit; policy:",
        spec$exogenous_missing %||% "engine_default",
        "; missing =",
        spec$missing
      )
    ),
    stringsAsFactors = FALSE
  )
}

#' The final-pass Monte Carlo information in a reliability assurance plan
#'
#' Keep the requested assurance separate from the empirical proportion at
#' the selected sample size. The binomial Monte Carlo standard error is
#' descriptive: the sample size was selected by the planner's search, so it
#' is not a confidence guarantee for that search or for the planned study.
#'
#' @param table The unmodified result of `DMAR::ss_aipe_reliability()`.
#' @return A list retaining the target, estimated assurance, final-pass
#'   replication count, count without an interval, descriptive Monte Carlo
#'   standard error, FMI and complete-data sample size when reported.
#' @author Ken Kelley
#' @noRd
sieve_reliability_assurance_summary <- function(table) {
  value <- function(term) {
    x <- table$value[table$term == term]
    if (length(x) == 1L && is.numeric(x) && is.finite(x)) unname(x) else NA_real_
  }
  attained <- value("assurance_attained")
  G <- value("G")
  no_interval <- attr(table, "no_interval", exact = TRUE)
  complete <- attr(table, "necessary_N_complete", exact = TRUE)
  list(
    requested = value("assurance"), attained = attained, G = G,
    no_interval = if (length(no_interval) == 1L && is.finite(no_interval)) no_interval else NA_real_,
    mc_se = if (is.finite(attained) && attained >= 0 && attained <= 1 && is.finite(G) && G > 0) {
      sqrt(attained * (1 - attained) / G)
    } else NA_real_,
    fmi = attr(table, "fmi", exact = TRUE) %||% 0,
    necessary_N_complete = if (length(complete) == 1L && is.finite(complete)) complete else NA_real_
  )
}

#' Describe a reliability planner's final-pass assurance estimate
#' @param summary The result of `sieve_reliability_assurance_summary()`.
#' @return A plain-text disclosure of the Monte Carlo result.
#' @author Ken Kelley
#' @noRd
sieve_reliability_assurance_text <- function(summary) {
  words <- if (is.finite(summary$attained)) {
    sprintf("Final-pass estimated assurance: %s%%", format(100 * summary$attained, digits = 5))
  } else "DMAR did not report an attained-assurance estimate"
  if (is.finite(summary$G)) words <- paste0(words, sprintf(" from %d simulations", as.integer(summary$G)))
  words <- paste0(words, ". ")
  if (is.finite(summary$no_interval)) words <- paste0(words, sprintf(
    "%d final-pass samples returned no interval and were counted as wider than the target. ",
    as.integer(summary$no_interval)))
  if (is.finite(summary$mc_se)) words <- paste0(words, sprintf(
    "Descriptive binomial Monte Carlo SE: %s percentage points; the sample size was selected by a search, so this is not a confidence guarantee for the search. ",
    format(100 * summary$mc_se, digits = 3)))
  if (is.finite(summary$fmi) && summary$fmi > 0 && is.finite(summary$necessary_N_complete)) {
    words <- paste0(words, sprintf(
      "The assurance was simulated at complete-data N = %d, then DMAR mapped that plan to the specified FMI of %s. ",
      as.integer(summary$necessary_N_complete), format(summary$fmi)))
  }
  words
}

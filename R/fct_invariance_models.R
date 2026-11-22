# Headless invariance and cross-validation services.

#' Display labels for the rungs of the invariance ladder
#'
#' The ladder in force depends on the item types: with continuous items it is
#' configural, metric, scalar, strict; with ordered or binary items a
#' thresholds rung is fit between configural and metric, because for an
#' ordered item the thresholds rather than an intercept carry the location
#' information (Millsap & Yun-Tein, 2004; Wu & Estabrook, 2016). The labels
#' name the constraints each rung adds so a reader of the table never has to
#' remember the order.
#'
#' @param ordered `TRUE` when any indicator was declared ordered.
#' @return A named character vector, rung name -> display label.
#' @author Ken Kelley
#' @noRd
sieve_inv_level_labels <- function(ordered = FALSE) {
  if (isTRUE(ordered)) {
    c(
      configural = "Configural (same pattern, all free)",
      thresholds = "Thresholds (equal thresholds)",
      metric = "Metric (thresholds + loadings)",
      scalar = "Scalar (thresholds + loadings + intercepts)",
      strict = "Strict (thresholds + loadings + intercepts + residuals)"
    )
  } else {
    c(
      configural = "Configural (same pattern, all free)",
      metric = "Metric (loadings)",
      scalar = "Scalar (loadings + intercepts)",
      strict = "Strict (loadings + intercepts + residuals)"
    )
  }
}

#' The group labels of a fitted multiple group lavaan model
#'
#' @param fit A fitted lavaan object, or `NULL`.
#' @return A character vector of group labels (empty when unavailable).
#' @author Ken Kelley
#' @noRd
sieve_group_labels <- function(fit) {
  if (is.null(fit)) {
    return(character(0))
  }
  tryCatch(as.character(lavaan::lavInspect(fit, "group.label")),
    error = function(e) character(0)
  )
}

#' Whether a Fitted Model Is Categorical
#'
#' A fit is categorical when lavaan estimated thresholds for it, which happens
#' exactly when indicators were declared ordered or binary. The test is
#' `lavaan::lavInspect(fit, "th")`, but that accessor returns one element per
#' group on a multiple group fit, and a continuous multiple group fit returns a
#' list of empty vectors rather than an empty list, so the elements have to be
#' unlisted before their length is read. Testing `length()` on the list itself
#' would call every multiple group fit categorical.
#'
#' @param fit A fitted lavaan object, or `NULL`.
#' @return `TRUE` when the fit has at least one estimated threshold.
#' @author Ken Kelley
#' @noRd
sieve_fit_is_categorical <- function(fit) {
  if (is.null(fit)) {
    return(FALSE)
  }
  th <- tryCatch(lavaan::lavInspect(fit, "th"), error = function(e) NULL)
  if (is.null(th)) {
    return(FALSE)
  }
  length(unlist(th, use.names = FALSE)) > 0L
}

#' Lavaan syntax for an identified invariance model, written directly
#'
#' Writes every parameter of a configural measurement model, for each group
#' of a multiple-group model or each occasion of a longitudinal one, with
#' its value (a number when fixed, `NA` when free) and its label, so the
#' restrictions of an invariance rung and the constraints that identify it
#' are explicit in the syntax. Parameters that share a label are equal.
#'
#' Factors are identified by fixing their variances at 1 and their means at
#' 0 (the fixed-factor method). In a later group or occasion a factor's
#' variance is freed once one of its loadings is held equal to the
#' reference, and its mean once the intercept of an item that measures that
#' factor alone is held equal.
#'
#' Ordered items follow Wu and Estabrook (2016) under the theta
#' parameterization. In the reference group (or at the first occasion)
#' every latent response has intercept 0 and residual variance 1, and all
#' thresholds are free. Elsewhere an item's latent response intercept is
#' freed once at least one of its thresholds is held equal and its
#' intercept is not, and its residual variance once at least two thresholds
#' are held equal (or one threshold and the intercept), unless residual
#' variances are held equal. A released (partial) parameter is free in
#' every group, and the rules above then count only the thresholds still
#' held equal, so the released item's location and scale stay identified.
#'
#' @param template A lavaan object made with `do.fit = FALSE` from the
#'   configural syntax and the data, with `std.lv = TRUE` (and `group`,
#'   `ordered`, and `parameterization = "theta"` when the model has them).
#' @param group_equal Parameter types held equal across groups: any of
#'   `"thresholds"`, `"loadings"`, `"intercepts"`, `"residuals"`.
#' @param group_partial Released parameters in lavaan syntax, such as
#'   `"F =~ x1"`, `"x1 | t2"`, `"x1 ~1"`, or `"x1 ~~ x1"`.
#' @param factors_over_time For a single-group longitudinal model, a named
#'   list giving each construct's factor at every occasion, in the declared
#'   occasion order.
#' @param items_over_time A named list giving each repeated item's variable
#'   at every occasion, in the same order.
#' @param time_equal Parameter types held equal across occasions.
#' @param autocov Residual covariances between occasions of each repeated
#'   item: `0` for none, a largest lag (1 gives adjacent occasions), or
#'   `"all"`.
#' @return The syntax, a single string.
#' @author Ken Kelley
#' @noRd
sieve_measurement_syntax <- function(template, group_equal = character(0), group_partial = character(0),
                                     factors_over_time = list(), items_over_time = list(),
                                     time_equal = character(0), autocov = 0L) {
  pt <- lavaan::parTable(template)
  n_groups <- lavaan::lavInspect(template, "ngroups")
  longitudinal <- length(factors_over_time) > 0L
  if (n_groups > 1L && longitudinal) stop("Write a multiple-group or a longitudinal model, not both at once.", call. = FALSE)
  ov <- lavaan::lavNames(template, "ov")
  lv <- lavaan::lavNames(template, "lv")
  ordered <- lavaan::lavNames(template, "ov.ord")
  if (any(pt$op == "=~" & pt$rhs %in% lv) || any(pt$op == "~")) {
    stop("Invariance syntax is written for first-order measurement models only.", call. = FALSE)
  }
  if (length(ordered) && !identical(lavaan::lavInspect(template, "options")$parameterization, "theta")) {
    stop("Models with ordered items are written under the theta parameterization.", call. = FALSE)
  }
  ref <- pt[pt$group == 1L & pt$op %in% c("=~", "|", "~1", "~~"), c("lhs", "op", "rhs"), drop = FALSE]
  rownames(ref) <- NULL
  type <- with(ref, ifelse(op == "=~", "loading",
    ifelse(op == "|", "threshold",
      ifelse(op == "~1", ifelse(lhs %in% lv, "mean", "intercept"),
        ifelse(lhs %in% lv, ifelse(lhs == rhs, "lv_variance", "lv_covariance"),
          ifelse(lhs == rhs, "residual", "residual_covariance")
        )
      )
    )
  ))
  # Labels name matrix positions in LISREL notation (lambda, tau, nu, theta,
  # alpha, psi) by the indices of the variables, so no two parameters share
  # a label by accident and no label can coincide with a variable name.
  oi <- function(v) match(v, ov)
  li <- function(v) match(v, lv)
  label_of <- function(type, lhs, rhs) {
    type <- rep_len(type, length(lhs))
    ifelse(type == "loading", sprintf("lambda.%d_%d", oi(rhs), li(lhs)),
      ifelse(type == "threshold", sprintf("tau.%d_%s", oi(lhs), sub("^t", "", rhs)),
        ifelse(type == "intercept", sprintf("nu.%d", oi(lhs)),
          ifelse(type == "mean", sprintf("alpha.%d", li(lhs)),
            ifelse(type %in% c("residual", "residual_covariance"), sprintf("theta.%d_%d", oi(lhs), oi(rhs)),
              sprintf("psi.%d_%d", li(lhs), li(rhs))
            )
          )
        )
      )
    )
  }
  base <- label_of(type, ref$lhs, ref$rhs)
  key <- paste(ref$lhs, ref$op, ref$rhs)
  # Values (NA = free) and labels, one column per group.
  value <- matrix(NA_real_, nrow(ref), n_groups)
  label <- matrix(base, nrow(ref), n_groups)
  for (g in seq_len(n_groups)) {
    rows <- pt[pt$group == g, , drop = FALSE]
    at <- match(key, paste(rows$lhs, rows$op, rows$rhs))
    if (anyNA(at)) stop("The groups of the configural model differ in structure.", call. = FALSE)
    value[, g] <- ifelse(rows$free[at] > 0L, NA_real_, rows$est[at])
    if (n_groups > 1L) label[, g] <- paste0(base, ".g", g)
  }
  find <- function(lhs, op, rhs) {
    i <- which(ref$lhs == lhs & ref$op == op & ref$rhs == rhs)
    if (!length(i) && identical(op, "~~")) i <- which(ref$lhs == rhs & ref$op == op & ref$rhs == lhs)
    i
  }
  measures <- function(item) ref$lhs[type == "loading" & ref$rhs == item]
  later <- if (n_groups > 1L) seq_len(n_groups)[-1L] else integer(0)

  if (n_groups > 1L) {
    released <- if (length(group_partial) && any(nzchar(group_partial))) {
      lavaan::lavParseModelString(group_partial[nzchar(group_partial)], as.data.frame. = TRUE)[, c("lhs", "op", "rhs")]
    } else data.frame(lhs = character(0), op = character(0), rhs = character(0))
    is_released <- function(i) {
      any(released$lhs == ref$lhs[i] & released$op == ref$op[i] & released$rhs == ref$rhs[i]) ||
        (ref$op[i] == "~~" && any(released$lhs == ref$rhs[i] & released$op == "~~" & released$rhs == ref$lhs[i]))
    }
    held <- vapply(seq_len(nrow(ref)), function(i) {
      kind <- switch(type[i],
        loading = "loadings", threshold = "thresholds", intercept = "intercepts", residual = "residuals", ""
      )
      nzchar(kind) && kind %in% group_equal && !is_released(i)
    }, logical(1))
    label[held, ] <- base[held]
    for (i in which(held & type == "loading")) value[find(ref$lhs[i], "~~", ref$lhs[i]), later] <- NA_real_
    for (i in which(held & type == "intercept")) {
      f <- measures(ref$lhs[i])
      if (length(f) == 1L) value[find(f, "~1", ""), later] <- NA_real_
    }
    for (item in ordered) {
      k <- sum(held & type == "threshold" & ref$lhs == item)
      equal_intercept <- any(held & type == "intercept" & ref$lhs == item)
      equal_residual <- any(held & type == "residual" & ref$lhs == item)
      if (k >= 1L && !equal_intercept) value[find(item, "~1", ""), later] <- NA_real_
      if ((k >= 2L || (k >= 1L && equal_intercept)) && !equal_residual) value[find(item, "~~", item), later] <- NA_real_
    }
  }

  if (longitudinal) {
    if (!is.list(items_over_time) || !length(items_over_time)) stop("Name the repeated items over occasions.", call. = FALSE)
    first_item <- stats::setNames(rep(vapply(items_over_time, `[`, character(1), 1L), lengths(items_over_time)), unlist(items_over_time))
    first_factor <- stats::setNames(rep(vapply(factors_over_time, `[`, character(1), 1L), lengths(factors_over_time)), unlist(factors_over_time))
    repeated <- function(item) !is.na(first_item[item])
    for (i in which(type == "loading")) {
      f <- ref$lhs[i]
      item <- ref$rhs[i]
      if (!"loadings" %in% time_equal || is.na(first_factor[f]) || !repeated(item)) next
      label[i, 1L] <- label_of("loading", first_factor[[f]], first_item[[item]])
      if (!identical(first_factor[[f]], f)) value[find(f, "~~", f), 1L] <- NA_real_
    }
    for (i in which(type == "threshold")) {
      item <- ref$lhs[i]
      if ("thresholds" %in% time_equal && repeated(item)) label[i, 1L] <- label_of("threshold", first_item[[item]], ref$rhs[i])
    }
    for (i in which(type == "intercept")) {
      item <- ref$lhs[i]
      if (!"intercepts" %in% time_equal || !repeated(item)) next
      label[i, 1L] <- label_of("intercept", first_item[[item]], "")
      f <- measures(item)
      if (length(f) == 1L && !is.na(first_factor[f]) && !identical(first_factor[[f]], f)) value[find(f, "~1", ""), 1L] <- NA_real_
    }
    for (i in which(type == "residual")) {
      item <- ref$lhs[i]
      if ("residuals" %in% time_equal && repeated(item)) label[i, 1L] <- label_of("residual", first_item[[item]], first_item[[item]])
    }
    for (item in intersect(ordered, names(first_item))) {
      if (identical(first_item[[item]], item)) next
      k <- if ("thresholds" %in% time_equal) sum(type == "threshold" & ref$lhs == item) else 0L
      equal_intercept <- "intercepts" %in% time_equal
      equal_residual <- "residuals" %in% time_equal
      value[find(item, "~1", ""), 1L] <- if (k >= 1L && !equal_intercept) NA_real_ else 0
      value[find(item, "~~", item), 1L] <- if ((k >= 2L || (k >= 1L && equal_intercept)) && !equal_residual) NA_real_ else 1
    }
    lag <- if (identical(autocov, "all")) Inf else as.integer(autocov)
    if (is.na(lag) || lag < 0L) stop("`autocov` must be 0, a positive lag, or \"all\".", call. = FALSE)
    extra <- list()
    for (vars in items_over_time) {
      n_t <- length(vars)
      for (d in seq_len(min(lag, n_t - 1L))) {
        for (t in seq_len(n_t - d)) {
          i <- find(vars[t], "~~", vars[t + d])
          if (length(i)) {
            value[i, ] <- NA_real_
          } else {
            extra[[length(extra) + 1L]] <- c(vars[t], vars[t + d])
          }
        }
      }
    }
    if (length(extra)) {
      add <- do.call(rbind, extra)
      ref <- rbind(ref, data.frame(lhs = add[, 1L], op = "~~", rhs = add[, 2L]))
      type <- c(type, rep("residual_covariance", nrow(add)))
      value <- rbind(value, matrix(NA_real_, nrow(add), n_groups))
      label <- rbind(label, matrix(label_of("residual_covariance", add[, 1L], add[, 2L]), nrow(add), n_groups))
    }
  }

  number <- function(v) ifelse(is.na(v), "NA", sprintf("%.17g", v))
  term <- function(i) {
    rhs <- if (identical(ref$op[i], "~1")) "1" else ref$rhs[i]
    op <- if (identical(ref$op[i], "~1")) "~" else ref$op[i]
    if (n_groups > 1L) {
      sprintf(
        "%s %s c(%s)*%s + c(%s)*%s", ref$lhs[i], op, paste(number(value[i, ]), collapse = ", "), rhs,
        paste(label[i, ], collapse = ", "), rhs
      )
    } else {
      sprintf("%s %s %s*%s + %s*%s", ref$lhs[i], op, number(value[i, 1L]), rhs, label[i, 1L], rhs)
    }
  }
  sections <- c(
    loading = "# Loadings", threshold = "# Thresholds", intercept = "# Intercepts",
    residual = "# Residual variances", residual_covariance = "# Residual covariances",
    mean = "# Latent means", lv_variance = "# Latent variances", lv_covariance = "# Latent covariances"
  )
  lines <- unlist(lapply(names(sections), function(s) {
    i <- which(type == s)
    if (length(i)) c(sections[[s]], vapply(i, term, character(1)))
  }))
  paste(lines, collapse = "\n")
}

#' The reproducible R for a DMAR invariance ladder call
#'
#' Writes exactly the call that ran, including the arguments that were
#' actually supplied (ordered items, a leading subset of rungs, freed
#' parameters), so the logged script reproduces the table on screen.
#'
#' @param syntax The lavaan measurement model syntax that was fitted.
#' @param group The grouping column name.
#' @param estimator,missing The estimator and missing data handling used.
#' @param ordered Character vector of ordered item names, or `NULL`.
#' @param group_partial Parameters left free across groups, or `NULL`.
#' @param levels The rungs fitted, or `NULL` for the whole ladder.
#' @param object The name to give the returned object in the script.
#' @return A character vector of R lines.
#' @author Ken Kelley
#' @noRd
sieve_invariance_code <- function(syntax, group, estimator, missing,
                                  ordered = NULL, group_partial = NULL,
                                  levels = NULL, object = "mi") {
  quoted <- function(x) paste(encodeString(x, quote = '"'), collapse = ", ")
  args <- c(
    sprintf("  inv_data, model = inv_model, group = %s,", sieve_r_literal(group)),
    sprintf("  estimator = \"%s\", missing = \"%s\",", estimator, missing)
  )
  if (!is.null(ordered)) {
    args <- c(args, sprintf("  ordered = c(%s),", quoted(ordered)))
  }
  if (!is.null(levels)) {
    args <- c(args, sprintf("  levels = c(%s),", quoted(levels)))
  }
  if (!is.null(group_partial)) {
    args <- c(args, sprintf("  group_partial = c(%s),", quoted(group_partial)))
  }
  c(
    sieve_invariance_frame_code(syntax, group, missing),
    sprintf("%s <- DMAR::measurement_invariance(", object),
    args,
    "  std.lv = TRUE)",
    sprintf(
      "%s%s# the ladder table, one row per rung", object,
      strrep(" ", max(1L, 28L - nchar(object)))
    ),
    sprintf(
      "fits_%s <- attr(%s, \"fits\")  # the fitted lavaan objects, reused below",
      object, object
    )
  )
}

# The same eligible sample is rebuilt for an independent downstream call.
sieve_invariance_frame_code <- function(syntax, group, missing) {
  parts <- lavaan::lavaanify(syntax)
  c(
    paste0("inv_model <- ", sieve_r_literal(syntax)),
    sprintf("# Eligible pool: %s on model items; observed group required.", missing),
    paste0(".inv_items <- ", paste(utils::capture.output(dput(unique(parts$rhs[parts$op == "=~"]))), collapse = "\n")),
    sprintf(
      ".inv_keep <- !is.na(inv_data[[%s]]) & nzchar(trimws(as.character(inv_data[[%s]])))",
      sieve_r_literal(group),
      sieve_r_literal(group)
    ),
    if (identical(
      missing,
      "fiml"
    )) ".inv_keep <- .inv_keep & rowSums(!is.na(inv_data[, .inv_items, drop = FALSE])) > 0L" else ".inv_keep <- .inv_keep & stats::complete.cases(inv_data[, .inv_items, drop = FALSE])",
    "inv_data <- inv_data[.inv_keep, , drop = FALSE]"
  )
}

#' Fit the Measurement Invariance Ladder Through DMAR
#'
#' The single place SIEVER fits an invariance ladder, used for both the ladder
#' itself and the partial invariance refit so the two differ only by the freed
#' parameters. The measurement model is the app's correlated-factors CFA
#' syntax, identified by fixing the latent variances at one, and the estimator
#' follows the confirmed item types. Full information maximum likelihood is
#' used for continuous items so incomplete cases contribute; it is not
#' available with the categorical estimator (DMAR stops when both are asked
#' for), so ordered models fall back to listwise deletion.
#'
#' DMAR fits the rungs. The fit indices of each rung, their changes, and the
#' difference tests are then read by SIEVER from the fitted objects DMAR
#' returns (`compare`, see [sieve_fit_compare()]) and not from DMAR's own
#' table, so that a ladder is compared as every other sequence of models
#' in SIEVER is: in one family of fit indices at every rung
#' (`sieve_common_family()`), and never in the robust indices of ordered
#' items at a rung that constrains thresholds or intercepts, which cannot
#' register a violation of threshold or intercept invariance (see
#' `sieve_robust_scope()`).
#'
#' @param data The analysis frame: the active items plus the grouping column.
#' @param group The grouping column name.
#' @param meta The item ledger.
#' @param group_partial Parameters to leave free across groups at every rung.
#' @param levels A leading subset of the ladder to fit, or `NULL` for all.
#' @param object The name the logged script gives the returned object.
#' @param compare `FALSE` skips the comparison of the rungs (used by the
#'   partial invariance refit, which compares its rung of interest with the
#'   rung before it in the same refit; see `sieve_partial_compare()`).
#' @param estimator One of `"auto"`, which follows the item types (ML
#'   when every item is continuous, WLSMV when any is ordered or binary),
#'   `"mlr"`, `"ml_fiml"`, `"ml"` or `"wlsmv"`, the choices of the
#'   Estimator control, which the tab passes; the rows used follow
#'   `missing_policy`.
#' @param missing_policy Optional FIML or listwise rule for continuous items.
#' @return A list with the DMAR result (`mi`), the syntax, the estimator and
#'   missing data handling used, any messages DMAR emitted (`notes`), the
#'   resolved specification (`spec`, see [sieve_cfa_spec()]), the status of
#'   each fitted rung (`status`, a named list; see `sieve_fit_status()`),
#'   the comparison of the rungs within one family of fit indices
#'   (`compare`), the warnings raised (`warnings`), and the reproducible
#'   `code`; or a list with an `error` string. An MLR (or ML) ladder the
#'   machine or the sample cannot carry is refused before any rung is fit,
#'   with the message of the model tabs (`sieve_mlr_guard()`), and the list
#'   then also has `refused = TRUE`.
#' @author Ken Kelley
#' @examples
#' hs <- lavaan::HolzingerSwineford1939
#' items <- paste0("x", 1:9)
#' meta <- data.frame(item = items, label = items,
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' mi <- sieve_fit_invariance(hs[, c(items, "school")], "school", meta,
#'   levels = c("configural", "metric"))
#' mi$compare$table[, c("model", "status", "cfi", "rmsea", "delta_cfi", "delta_rmsea", "delta_p")]
#' @export
sieve_fit_invariance <- function(data, group, meta, group_partial = NULL,
                                 levels = NULL, object = "mi", compare = TRUE,
                                 estimator = "auto", missing_policy = NULL) {

  # One resolved specification: the latent names avoid every column of the
  # analysis frame (the grouping column included), and the estimator, the
  # ordered variables, and the missing data treatment follow the indicators
  # that are in the model.
  spec <- sieve_cfa_spec(meta, reserved = names(data), estimator = estimator)
  syntax <- spec$model
  if (!nzchar(syntax)) {
    return(list(precondition = TRUE, error = paste(
      "no construct has two or more active items, so there is no measurement",
      "model to test across groups. Assign items to constructs on the Items",
      "tab first."
    )))
  }
  ordered <- spec$ordered
  missing <- spec$missing
  if (!is.null(missing_policy)) {
    if (!missing_policy %in% c(
      "fiml",
      "listwise",
      "complete"
    )) return(list(
      precondition = TRUE,
      error = "Unknown invariance missing-data rule."
    ))
    missing <- if (length(ordered) || missing_policy %in% c("listwise", "complete")) "listwise" else "fiml"
    spec$missing <- missing
  }
  # The MLR refusal of the model tabs (sieve_mlr_guard()), before DMAR fits
  # any rung: the configural model of every group, with its mean structure,
  # against the respondents in the frame.
  in_group <- if (group %in% names(data)) !is.na(data[[group]]) else logical(0)
  refusal <- sieve_spec_mlr_guard(spec,
    n = if (any(in_group)) sum(in_group) else NA_real_,
    groups = length(unique(data[[group]][in_group])), means = TRUE
  )
  if (!is.null(refusal)) {
    return(list(precondition = TRUE, refused = TRUE, error = refusal, spec = spec))
  }
  # DMAR reports its methodological switches (the estimator swap for ordered
  # items, the theta parameterization a strict ordered rung needs) as
  # messages; they are collected and shown in the panel rather than lost to
  # the console.
  notes <- character(0)
  run <- sieve_capture(if (length(ordered) && length(group_partial)) {
    sieve_ordered_partial_ladder(
      data, syntax, group, ordered, levels,
      spec$estimator, missing, group_partial
    )
  } else DMAR::measurement_invariance(
    data = data, model = syntax, group = group,
    levels = levels, ordered = ordered,
    estimator = spec$estimator, missing = missing,
    group_partial = group_partial, std.lv = TRUE
  ))
  notes <- c(notes, run$messages)
  if (!run$ok) {
    return(list(error = run$error, notes = notes, warnings = run$warnings, spec = spec))
  }
  mi <- run$value
  generated_code <- NULL
  if (!is.null(attr(mi, "generated_syntax"))) {
    fitted_syntax <- attr(mi, "generated_syntax")
    # Retain the identified syntax written for each rung, so replay fits it as written.
    # Bind the same eligible frame and fitted-object name as every other
    # ladder; dependent diagnostics must never read a previous ladder.
    frame_code <- sieve_invariance_code(
      syntax, group, spec$estimator, missing,
      ordered, group_partial, levels, object
    )
    call_at <- match(sprintf("%s <- DMAR::measurement_invariance(", object), frame_code)
    fitted_name <- paste0("fits_", object)
    generated_code <- c(
      frame_code[seq_len(call_at - 1L)],
      sprintf("%s <- list()", fitted_name), unlist(lapply(seq_along(fitted_syntax), function(j) {
        sprintf(
          "%s[[%s]] <- lavaan::cfa(%s, data = inv_data, group = %s, ordered = %s, estimator = %s, missing = %s, parameterization = \"theta\")",
          fitted_name, sieve_r_literal(names(attr(mi, "fits"))[j]), sieve_r_literal(fitted_syntax[[j]]),
          sieve_r_literal(group), paste(utils::capture.output(dput(ordered)), collapse = ""),
          sieve_r_literal(spec$estimator), sieve_r_literal(missing)
        )
      })), sprintf("%s <- SIEVER::sieve_fit_compare(%s)$table", object, fitted_name), object
    )
  }
  # A returned ladder is not a certificate: each rung is a fitted model with
  # its own status (failed, not converged, not identified, inadmissible, or
  # ok), read from the fitted objects DMAR hands back.
  fits <- attr(mi, "fits")
  # Every rung must hold the variables the specification intended.
  changed <- unlist(lapply(fits, function(f) {
    if (is.null(f)) NULL else sieve_membership_problem(f, spec)
  }))
  if (length(changed)) {
    return(list(error = unname(changed[1]), notes = notes, warnings = run$warnings, spec = spec))
  }
  status <- lapply(fits, function(f) sieve_fit_status(f))
  # CFI and RMSEA at every rung in one family, their changes within that
  # family, and lavaan's difference test of each rung against the one before
  # it, all read from the fitted rungs.
  compared <- if (isTRUE(compare)) sieve_fit_compare(fits, status = status) else NULL
  list(
    mi = mi, syntax = syntax, estimator = attr(mi, "estimator"),
    ordered = isTRUE(attr(mi, "ordered")), missing = missing, notes = notes,
    spec = spec, status = status, compare = compared,
    data_code = sieve_invariance_frame_code(syntax, group, missing),
    warnings = unique(c(run$warnings, compared$warnings)),
    code = generated_code %||% sieve_invariance_code(syntax, group, attr(mi, "estimator"),
      missing, ordered, group_partial, levels,
      object = object
    )
  )
}

#' One row of headline fit for a calibration or a validation sample
#'
#' CFI, RMSEA and its interval in the family asked for, SRMR, and ECVI
#' (Browne and Cudeck's expected cross-validation index) with its confidence
#' interval from the noncentral chi square (`DMAR::ecvi()`), in keeping with
#' the emphasis on interval estimates.
#'
#' Every value in the row describes a solution. ECVI is a function of the
#' minimized discrepancy, so like the other indices it is reported only for
#' a model that has one: a model that did not converge or is not identified
#' has no minimum for it to describe, whatever number the formula returns
#' for the values the optimizer stopped at. It is also `NA` when the model
#' has zero degrees of freedom.
#'
#' @param label The sample's label.
#' @param fit A fitted lavaan object, or `NULL`.
#' @param family The family of fit indices to read (see
#'   `sieve_fit_measures()`), or `NULL` for the display rule.
#' @param conf_level The confidence level of the ECVI interval (DMAR's
#'   `conf_level`, 0.95 by default).
#' @param rmsea_conf_level The confidence level of the RMSEA interval
#'   (`sieve_default_rmsea_conf_level()`, 0.90 by default).
#' @return A one-row data frame with `Sample`, `n`, `CFI`, `RMSEA`,
#'   `RMSEA.lo`, `RMSEA.hi`, `SRMR`, `ECVI`, `ECVI.lo`, and `ECVI.hi`, at full
#'   precision. The attribute `warnings` holds what DMAR said while forming
#'   the ECVI interval.
#' @author Ken Kelley
#' @noRd
sieve_cv_fit_row <- function(label, fit, family = NULL, conf_level = sieve_default_conf_level(),
                             rmsea_conf_level = sieve_default_rmsea_conf_level()) {
  m <- sieve_fit_measures(fit, family = family, conf_level = rmsea_conf_level)
  ecvi <- c(est = NA_real_, lo = NA_real_, hi = NA_real_)
  said <- character(0)
  if (!is.null(fit) && sieve_has_solution(fit)) {
    run <- sieve_capture(DMAR::ecvi(fit = fit, conf_level = conf_level))
    said <- run$warnings
    e <- run$value
    if (run$ok && !is.null(e)) {
      sieve_dmar_require_terms(e, c("ecvi", "ci_lower", "ci_upper"), "ECVI")
      pick <- function(nm) {
        v <- e$value[e$term == nm]
        if (length(v) == 1L && is.finite(v)) v else NA_real_
      }
      ecvi <- c(
        est = pick("ecvi"), lo = pick("ci_lower"),
        hi = pick("ci_upper")
      )
    }
  }
  row <- if (is.null(m)) {
    data.frame(
      Sample = label, n = NA_integer_, CFI = NA_real_,
      RMSEA = NA_real_, RMSEA.lo = NA_real_, RMSEA.hi = NA_real_,
      SRMR = NA_real_, ECVI = unname(ecvi["est"]),
      ECVI.lo = unname(ecvi["lo"]), ECVI.hi = unname(ecvi["hi"]),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      Sample = label,
      n = tryCatch(lavaan::lavInspect(fit, "ntotal"), error = function(e) NA_integer_),
      CFI = unname(m["cfi"]),
      RMSEA = unname(m["rmsea"]),
      RMSEA.lo = unname(m["rmsea_lo"]),
      RMSEA.hi = unname(m["rmsea_hi"]),
      SRMR = unname(m["srmr"]),
      ECVI = unname(ecvi["est"]),
      ECVI.lo = unname(ecvi["lo"]),
      ECVI.hi = unname(ecvi["hi"]),
      stringsAsFactors = FALSE
    )
  }
  attr(row, "warnings") <- said
  row
}

#' The number of distinct values a grouping variable may have on this tab
#'
#' The one place the rule is written: a grouping variable is usable for a
#' multiple group model when it has at least `min` and at most `max`
#' distinct non-missing values (enough groups for a comparison, not so
#' many that the per group sample collapses). Every selector and message
#' on the tab reads the rule from here.
#'
#' @return A named integer vector with `min` and `max`.
#' @author Ken Kelley
#' @noRd
sieve_group_levels <- function() {
  c(min = 2L, max = 6L)
}

#' The rule of `sieve_group_levels()`, in words
#'
#' @return A string such as `"2 to 6 distinct values"`.
#' @author Ken Kelley
#' @noRd
sieve_group_levels_text <- function() {
  lim <- sieve_group_levels()
  sprintf("%d to %d distinct values", lim[["min"]], lim[["max"]])
}

#' Does a grouping variable have a usable number of distinct values?
#'
#' @param x The variable's values.
#' @return `TRUE` or `FALSE` under `sieve_group_levels()`.
#' @author Ken Kelley
#' @noRd
sieve_group_usable_levels <- function(x) {
  k <- length(unique(stats::na.omit(x)))
  lim <- sieve_group_levels()
  k >= lim[["min"]] && k <= lim[["max"]]
}

#' Grouping variables excluded by the distinct value ceiling
#'
#' The tab's grouping selector offers the columns that have the group role
#' and a usable number of distinct values (see `sieve_group_levels()`).
#' Alignment is intended for many groups, so a user reaching for it may be
#' hitting exactly that ceiling; this names the columns with the group
#' role that the ceiling is keeping out. Columns with more distinct values
#' than `max_report` are not plausible groupings and are not named.
#'
#' @param state The shared state store.
#' @param max_report The largest number of distinct values still plausible as
#'   groups.
#' @return A character vector of column names (possibly empty).
#' @author Ken Kelley
#' @noRd
sieve_groups_over_ceiling <- function(state, max_report = 40L) {
  raw <- state$raw
  if (is.null(raw)) {
    return(character(0))
  }
  cand <- intersect(sieve_role_vars(state, "group"), names(raw))
  lim <- sieve_group_levels()
  keep <- vapply(cand, function(v) {
    k <- length(unique(stats::na.omit(raw[[v]])))
    k > lim[["max"]] && k <= max_report
  }, logical(1))
  cand[keep]
}

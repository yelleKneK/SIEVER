#' Read DMAR's exact-fit decision for a model with zero degrees of freedom
#' @param fit A lavaan fit.
#' @param df Degrees of freedom, read from the fit when NULL.
#' @return TRUE or FALSE when established, otherwise NA.
#' @author Ken Kelley
#' @noRd
sieve_zero_df_exact_fit <- function(fit, df = NULL) {
  if (!inherits(fit, "lavaan")) return(NA)
  if (is.null(df)) df <- lavaan::lavInspect(fit, "test")[[1L]]$df
  if (!isTRUE(df == 0)) return(NA)
  # DMAR owns the zero-df boundary rule, including active bounds.
  boundary <- sieve_capture(DMAR::sem_fit_measures(fit))
  if (!boundary$ok) return(NA)
  rmsea <- boundary$value$value[boundary$value$term == "rmsea"]
  length(rmsea) == 1L && is.finite(rmsea) && rmsea == 0
}

#' Standalone R Applying the Same Zero-df Display Rule
#' @param fit Name of the fitted object in the generated code.
#' @param values Name of the fit-measure vector.
#' @return R lines, indented for an enclosing function.
#' @author Ken Kelley
#' @noRd
sieve_zero_df_mask_code <- function(fit = "f", values = "m") {
  c(
    "  # Zero df alone does not establish exact fit; use DMAR's public boundary rule.",
    sprintf("  if (isTRUE(lavaan::fitMeasures(%s, \"df\") == 0)) {", fit),
    sprintf("    boundary <- tryCatch(DMAR::sem_fit_measures(%s), error = function(e) NULL)", fit),
    "    exact_rmsea <- if (is.null(boundary)) numeric() else boundary$value[boundary$term == \"rmsea\"]",
    "    exact_fit <- length(exact_rmsea) == 1L && is.finite(exact_rmsea) && exact_rmsea == 0",
    sprintf("    if (!isTRUE(exact_fit)) %s[startsWith(names(%s), \"tli\") | startsWith(names(%s), \"rmsea\")] <- NA_real_", values, values, values),
    "  }"
  )
}

#' Read the uncertainty calculation actually used by a fitted model
#' @param fit A lavaan fit, or NULL.
#' @return Plain method information, without fitting or rounding.
#' @author Ken Kelley
#' @noRd
sieve_fit_inference <- function(fit) {
  if (!inherits(fit, "lavaan")) return(NULL)
  opt <- lavaan::lavInspect(fit, "options")
  test <- lavaan::lavInspect(fit, "test")[[1L]]
  list(estimator = opt$estimator, se = opt$se, test = opt$test,
    missing = opt$missing, information = opt$information,
    h1_refusal = sieve_h1_refusal(fit), df = test$df,
    chisq = test$stat, exact_fit = sieve_zero_df_exact_fit(fit, test$df))
}

#' Describe actual standard errors and tests without inferring them from a label
#' @param method Information from sieve_fit_inference().
#' @return One display sentence, or NULL.
#' @author Ken Kelley
#' @noRd
sieve_inference_text <- function(method) {
  if (is.null(method)) return(NULL)
  se <- paste(method$se %||% "not recorded", collapse = ", ")
  label <- switch(se,
    standard = "model-based standard errors",
    robust.huber.white = "Huber-White sandwich standard errors",
    robust.sem = "robust SEM standard errors",
    bootstrap = "bootstrap standard errors", none = "no standard errors", se)
  paste0(label, " (se = ", se, "); test = ",
    paste(method$test %||% "not recorded", collapse = ", "),
    if (!is.null(method$h1_refusal)) paste0(". ", sub("[.]$", "", method$h1_refusal)),
    if (isTRUE(method$df == 0) && isTRUE(method$exact_fit)) paste0(
      ". Saturated model (df = 0): exact-fit conventions such as RMSEA = 0 with CI [0, 0]",
      " supply no evidence of approximate fit and have no fit-index SE; eligible parameter SEs and intervals are separate"),
    if (isTRUE(method$df == 0) && !isTRUE(method$exact_fit)) paste0(
      ". Zero degrees of freedom with unestablished exact fit: the saturated-model conventions do not apply;",
      " RMSEA and TLI inference is unavailable"))
}

#' Preview an inclusion change without changing the ledger or fitting a model
#' @param state Shared state.
#' @param items Selected item names.
#' @param status The proposed inclusion state.
#' @return Included-item counts, complete-case changes and content coverage.
#' @author Ken Kelley
#' @noRd
sieve_item_change_preview <- function(state, items, status) {
  if (!status %in% c("active", "parked")) stop("Unknown inclusion state.", call. = FALSE)
  meta <- shiny::isolate(state$meta)
  data <- shiny::isolate(state$items)
  if (is.null(meta) || is.null(data)) return(NULL)
  after <- meta
  at <- match(unique(items), meta$item)
  at <- at[!is.na(at)]
  changed <- at[meta$status[at] != status]
  after$status[changed] <- status
  included <- function(m) intersect(m$item[m$status == "active"], names(data))
  before_items <- included(meta)
  after_items <- included(after)
  complete <- function(its) {
    if (!length(its)) return(rep(FALSE, nrow(data)))
    X <- sieve_reliability_data(data, its)
    stats::complete.cases(X)
  }
  old <- complete(before_items)
  new <- complete(after_items)
  list(items = meta$item[changed], before = length(before_items), after = length(after_items),
    complete_before = sum(old), complete_after = sum(new),
    recovered = sum(new & !old), lost = sum(old & !new),
    coverage = sieve_coverage_change(sieve_item_coverage(meta), sieve_item_coverage(after)))
}

#' Display an item-change preview beside its Apply action
#' @param preview Output of sieve_item_change_preview().
#' @return Accessible tags, or NULL.
#' @author Ken Kelley
#' @noRd
sieve_item_change_preview_ui <- function(preview) {
  if (is.null(preview) || !length(preview$items)) return(NULL)
  coverage <- preview$coverage
  coverage <- coverage[coverage$before != coverage$after, , drop = FALSE]
  shiny::div(class = "small border rounded p-2 my-2 sieve-item-preview", role = "status",
    shiny::strong("Preview of the Selected Changes"),
    shiny::p(class = "mb-1", sprintf(
      "Included items: %d to %d. Complete respondents: %d to %d; %d recovered, %d lost.",
      preview$before, preview$after, preview$complete_before, preview$complete_after,
      preview$recovered, preview$lost)),
    shiny::p(class = "mb-1", paste(
      "These complete-case counts use all included items; a construct or model can use a different subset.",
      "FIML continues to use incomplete respondents. Applying this batch updates the item pool once",
      "and refits previously requested analyses; other results are marked for updating.")),
    sieve_plain_table_ui(stats::setNames(coverage[c("level", "construct", "facet", "before", "after")],
      c("Level", "Construct", "Facet", "Before", "After"))),
    shiny::p(class = "mb-0", "Review content coverage as well as statistical diagnostics. No change has been applied yet."))
}

#' Count eligibility under the current and alternative response rules
#' @param data Item data.
#' @param meta The item ledger.
#' @param policies Stored scoring policies.
#' @return Current rules, alternative minimum counts, and named refusals.
#' @author Ken Kelley
#' @noRd
sieve_scoring_eligibility <- function(data, meta, policies = list()) {
  specs <- sieve_score_spec_build(meta, policies, names(data))
  rules <- sensitivity <- list()
  refused <- character()
  for (spec in specs) {
    if (isTRUE(spec$latent)) {
      refused <- c(refused, paste0(spec$construct,
        ": factor scores use the fitted model; minimum-count proration comparisons do not apply."))
      next
    }
    read <- tryCatch(sieve_composite_score(data, spec$items, spec$weights, spec$method,
      spec$min_count, spec$min_prop), error = function(e) e)
    if (inherits(read, "error")) {
      refused <- c(refused, paste0(spec$construct, ": ", conditionMessage(read)))
      next
    }
    k <- length(spec$items)
    usable <- is.finite(read$scores)
    rules[[length(rules) + 1L]] <- data.frame(
      Construct = spec$construct, Method = spec$label, Items = k,
      Required = spec$required, Eligible = sum(usable),
      One_Item = sum(usable & read$answered == 1L),
      Partial = sum(usable & read$answered < k), Complete = sum(usable & read$answered == k),
      check.names = FALSE)
    # Reuse the operational scorer, including its positive-weight rule.
    for (required in seq_len(k)) {
      alt <- sieve_composite_score(data, spec$items, spec$weights, spec$method, required, 0)
      sensitivity[[length(sensitivity) + 1L]] <- data.frame(
        Construct = spec$construct, Required = required, Items = k,
        Eligible = sum(is.finite(alt$scores)), Current = required == spec$required)
    }
  }
  bind <- function(x) if (length(x)) do.call(rbind, x) else data.frame()
  list(rules = bind(rules), sensitivity = bind(sensitivity), refused = refused)
}

#' Verify fitted respondent identities against the saved input snapshot
#' @param fit Fitted model.
#' @param inputs Captured replay inputs.
#' @param input_rows Optional verified positions retained before model fitting.
#' @return Verified original row identities, or an explicit unrecorded reason.
#' @author Ken Kelley
#' @noRd
sieve_fitted_respondents <- function(fit, inputs, input_rows = NULL) {
  unavailable <- function(why) list(verified = FALSE, reason = why)
  if (!inherits(fit, "lavaan") || !is.data.frame(inputs$items)) {
    return(unavailable("Fitted respondent identities were not recorded for this engine."))
  }
  if (!is.null(input_rows)) {
    if (!is.numeric(input_rows) || anyNA(input_rows) || anyDuplicated(input_rows) ||
        any(input_rows != as.integer(input_rows)) || any(input_rows < 1L | input_rows > nrow(inputs$items))) {
      return(unavailable("The prefit respondent row map is invalid."))
    }
    n <- nrow(inputs$items)
    inputs$case_index <- (inputs$case_index %||% seq_len(n))[input_rows]
    inputs$items <- inputs$items[input_rows, , drop = FALSE]
    if (is.data.frame(inputs$raw) && nrow(inputs$raw) == n) inputs$raw <- inputs$raw[input_rows, , drop = FALSE]
  }
  data <- inputs$items
  if (is.data.frame(inputs$raw) && nrow(inputs$raw) == nrow(data)) {
    extra <- setdiff(names(inputs$raw), names(data))
    data[extra] <- inputs$raw[extra]
  }
  spec <- attr(fit, "sieve_spec", exact = TRUE)
  aliases <- c(spec$out_map, spec$pred_map)
  for (name in setdiff(names(aliases), names(data))) {
    source <- aliases[[name]]
    if (source %in% names(data)) data[[name]] <- data[[source]]
  }
  check <- tryCatch(sieve_score_data_check(fit, data), error = function(e) e)
  if (inherits(check, "error") || is.null(check$n)) {
    return(unavailable("The fitted rows could not be verified against the saved input snapshot."))
  }
  rows <- inputs$case_index %||% seq_len(nrow(data))
  if (length(rows) != nrow(data) || anyNA(rows) || anyDuplicated(rows)) {
    return(unavailable("Unique original respondent identities were not recorded."))
  }
  index <- lavaan::lavInspect(fit, "case.idx")
  if (!is.list(index)) index <- list(index)
  empty <- lavaan::lavInspect(fit, "empty.idx")
  if (!is.list(empty)) empty <- list(empty)
  index <- lapply(seq_along(index), function(g) {
    positions <- setdiff(seq_along(index[[g]]), empty[[g]])
    index[[g]][positions]
  })
  if (!identical(as.integer(lengths(index)), as.integer(lavaan::lavInspect(fit, "nobs")))) {
    return(unavailable("Fitted row identities do not agree with the engine's respondent counts."))
  }
  list(verified = TRUE, original_rows = rows[sort(unique(unlist(index)))],
    groups = lapply(index, function(i) rows[i]))
}

#' Compare saved analyses and stored numerical tables without fitting models
#' @param state Shared state.
#' @param ids Ordered analysis identifiers; the first is the reference.
#' @return Specifications, differences, stored result tables, and cautions.
#' @author Ken Kelley
#' @noRd
sieve_analysis_comparison <- function(state, ids) {
  all_records <- shiny::isolate(state$analyses) %||% list()
  ids <- unique(as.character(ids))
  present <- intersect(ids, names(all_records))
  absent <- setdiff(ids, names(all_records))
  records <- all_records[present]
  now <- sieve_revision(state)
  labels <- shiny::isolate(state$analysis_labels) %||% list()
  describe <- function(r) {
    sp <- r$spec %||% list()
    items <- sp$items %||% sp$observed %||% unique(unlist(lapply(sp$groups, function(g) g$items)))
    list(items = as.character(items), model = sp$model %||% sp$syntax,
      estimator = sieve_record_estimator_text(r), missing = sieve_record_missing_text(r),
      uncertainty = sieve_record_uncertainty_text(r), rows = r$sample_size$fitted,
      respondents = r$fitted_respondents, dataset = r$scientific_dataset_id,
      input_key = r$scientific_input_key %||% r$input_key, settings = sp)
  }
  descriptions <- lapply(records, describe)
  rows <- lapply(seq_along(records), function(i) {
    r <- records[[i]]
    d <- descriptions[[i]]
    data.frame(Analysis = present[i], Description = labels[[present[i]]] %||% r$note %||% r$kind %||% "not recorded",
      Items = paste(d$items, collapse = ", "),
      Respondents = sieve_record_n_text(d$rows), Estimator = d$estimator,
      Missing = d$missing, Uncertainty = d$uncertainty,
      Status = r$status$code %||% "not recorded",
      Inputs = sieve_record_currency(r, state, now), stringsAsFactors = FALSE)
  })
  differences <- list()
  cautions <- if (length(absent)) paste("Not recorded:", paste(absent, collapse = ", ")) else character()
  if (length(descriptions) > 1L) {
    base <- descriptions[[1L]]
    for (i in seq.int(2L, length(descriptions))) {
      d <- descriptions[[i]]
      same_input <- !is.null(base$input_key) && !is.null(d$input_key) &&
        identical(base$input_key, d$input_key)
      same_dataset <- !is.null(base$dataset) && !is.null(d$dataset) &&
        identical(base$dataset, d$dataset)
      verified <- (same_input || same_dataset) && isTRUE(base$respondents$verified) &&
        isTRUE(d$respondents$verified)
      same_rows <- verified && setequal(base$respondents$original_rows, d$respondents$original_rows)
      differences[[length(differences) + 1L]] <- data.frame(
        Reference = present[1L], Comparison = present[i],
        Added = paste(setdiff(d$items, base$items), collapse = ", "),
        Excluded = paste(setdiff(base$items, d$items), collapse = ", "),
        "Model Changed" = !identical(base$model, d$model),
        "Settings Changed" = !identical(base$settings, d$settings),
        "Estimator Changed" = !identical(base$estimator, d$estimator),
        "Missing Treatment Changed" = !identical(base$missing, d$missing),
        "Input Identity" = if (same_input) "same recorded inputs" else "different or unverified inputs",
        "Fitted Respondents" = if (!verified) "identity not recorded or not comparable" else
          if (same_rows) "same verified respondents" else "different verified respondents",
        check.names = FALSE, stringsAsFactors = FALSE)
      if (!same_input || !same_rows || !identical(base$estimator, d$estimator) ||
          !setequal(base$items, d$items)) {
        cautions <- c(cautions, sprintf(paste(
          "%s versus %s: inputs, item sets, fitted respondents, or estimators differ or cannot be verified.",
          "Equal respondent counts alone do not establish the same respondents.",
          "This display does not establish that a likelihood-ratio test or an information-criterion comparison is appropriate."),
          present[1L], present[i]))
      }
    }
  }
  bind <- function(x) if (length(x)) do.call(rbind, x) else data.frame()
  list(analyses = bind(rows), differences = bind(differences), cautions = cautions,
    results = stats::setNames(lapply(records, function(r) sieve_readable_result_tables(r$estimates)), present))
}

#' Keep Raw Parameter Estimates and Limits Visible Beside Each Other
#' @param table One saved numerical table.
#' @return Display tags; the complete stored table remains available.
#' @author Ken Kelley
#' @noRd
sieve_comparison_result_ui <- function(table) {
  required <- c("lhs", "op", "rhs", "est", "se", "ci.lower", "ci.upper")
  if (!is.data.frame(table) || !all(required %in% names(table))) return(sieve_plain_table_ui(table))
  parameter <- paste(table$lhs, table$op, table$rhs)
  for (field in intersect(c("group", "block", "level", "label"), names(table))) {
    value <- as.character(table[[field]])
    present <- !is.na(value) & nzchar(value)
    parameter[present] <- paste0(parameter[present], " [", field, " = ", value[present], "]")
  }
  compact <- data.frame(Parameter = parameter, Estimate = table$est, SE = table$se,
    "CI Lower" = table$ci.lower, "CI Upper" = table$ci.upper, check.names = FALSE)
  shiny::tagList(
    shiny::p(class = "small mb-1", "Raw estimates and their confidence limits; the complete stored table is available below."),
    shiny::div(class = "sieve-saved-compact", sieve_plain_table_ui(compact)),
    shiny::tags$details(shiny::tags$summary("Complete Stored Table"), sieve_plain_table_ui(table)))
}

#' Display a comparison using only the selected saved records
#' @param comparison Output of sieve_analysis_comparison().
#' @return Tags.
#' @author Ken Kelley
#' @noRd
sieve_analysis_comparison_ui <- function(comparison) {
  paths <- unique(unlist(lapply(comparison$results, names)))
  shiny::tagList(
    shiny::p(class = "small", paste(
      "The first selected analysis is the reference. These are saved specifications and numerical results;",
      "nothing is fitted and no best-scale ranking is calculated.",
      "Compare the same estimand and scale, including its uncertainty method and interval level.",
      "Raw-parameter confidence limits describe the raw estimate, not a standardized estimate beside it.")),
    sieve_plain_table_ui(comparison$analyses),
    if (nrow(comparison$differences)) sieve_plain_table_ui(comparison$differences),
    lapply(comparison$cautions, function(x) shiny::p(class = "small text-warning-emphasis", x)),
    lapply(paths, function(path) shiny::tags$details(open = NA,
      shiny::tags$summary(path),
      shiny::div(class = "sieve-saved-results",
        lapply(names(comparison$results), function(id) shiny::div(class = "sieve-saved-record",
          shiny::strong(id), sieve_comparison_result_ui(comparison$results[[id]][[path]])))))))
}

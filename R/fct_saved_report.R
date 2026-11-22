# fct_saved_report.R
# The report from recorded analyses: the catalog of the analysis registry
# and the readable report of the selected records. Both read state$analyses
# and run nothing: a record is described as it was recorded (its
# specification, the revision of its inputs, its actual sample, its
# estimator and missing data treatment, its uncertainty method, the
# software versions), a record of earlier inputs is labelled earlier, a
# requested record that is absent or failed is listed as such, and the
# report ends with the bibliography of the methods the selected records
# used (fct_references.R). Nothing here refits a model.

#' Render a plain data frame without interpreting imported text as HTML
#' @param x Data frame.
#' @param digits Display precision only.
#' @return Table tags.
#' @author Ken Kelley
#' @noRd
sieve_plain_table_ui <- function(x, digits = 4L) {
  if (!is.data.frame(x) || !nrow(x)) return(shiny::p(class = "small", "No recorded rows."))
  shown <- lapply(x, function(z) {
    if (is.numeric(z)) ifelse(is.na(z), "unavailable", format(z, digits = digits, trim = TRUE))
    else ifelse(is.na(z), "not recorded", as.character(z))
  })
  shiny::div(style = "overflow-x:auto;", shiny::tags$table(
    class = "table table-sm small",
    shiny::tags$thead(shiny::tags$tr(lapply(names(x), shiny::tags$th))),
    shiny::tags$tbody(lapply(
      seq_len(nrow(x)),
      function(i) shiny::tags$tr(lapply(
        shown,
        function(z) shiny::tags$td(z[i])
      ))
    ))
  ))
}

#' Convert stored compact results to labeled readable tables
#' @param x Stored results only, never an engine or fitted input.
#' @param path Current result label.
#' @param depth Recursion bound.
#' @return Named list of data frames.
#' @author Ken Kelley
#' @noRd
sieve_readable_result_tables <- function(x, path = "Results", depth = 0L) {
  if (is.null(x)) return(stats::setNames(
    list(data.frame(status = "Unavailable; no replacement analysis was run.")),
    path
  ))
  if (is.data.frame(x)) return(stats::setNames(list(x), path))
  if (is.matrix(x)) return(stats::setNames(
    list(data.frame(
      term = rownames(x) %||% as.character(seq_len(nrow(x))),
      x,
      check.names = FALSE
    )),
    path
  ))
  if (is.atomic(x)) return(stats::setNames(
    list(data.frame(
      term = names(x) %||% as.character(seq_along(x)),
      value = unname(x),
      stringsAsFactors = FALSE
    )),
    path
  ))
  if (!is.list(x) || depth >= 6L) return(stats::setNames(
    list(data.frame(status = "See the exact technical appendix for this structure.")),
    path
  ))
  out <- list()
  for (i in seq_along(x)) {
    nm <- names(x)[i] %||% as.character(i)
    if (is.na(nm) || !nzchar(nm)) nm <- as.character(i)
    out <- c(out, sieve_readable_result_tables(x[[i]], paste(path, nm, sep = ": "), depth + 1L))
  }
  out
}

#' The sample sizes of a record, in words
#'
#' @param x A count, a vector of per group counts, or `NULL`.
#' @return `"301"`, `"301 (groups: 145, 156)"`, or `"not recorded"`.
#' @author Ken Kelley
#' @noRd
sieve_record_n_text <- function(x) {
  x <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
  if (!length(x) || any(!is.finite(x))) {
    return("not recorded")
  }
  paste0(
    format(sum(x), big.mark = ","),
    if (length(x) > 1L) paste0(" (groups: ", paste(format(x, big.mark = ","), collapse = ", "), ")") else ""
  )
}

#' The missing data treatment of a record, in words
#'
#' @param r A recorded analysis.
#' @return `"FIML"`, `"complete cases"`, a combination for a reliability
#'   record whose groups differ, or `"not recorded"`.
#' @author Ken Kelley
#' @noRd
sieve_record_missing_text <- function(r) {
  sp <- r$spec %||% list()
  word <- function(m) switch(as.character(m %||% ""),
    fiml = "FIML", listwise = , complete = "complete cases", pairwise = "pairwise", NA_character_
  )
  used <- if (length(sp$groups)) {
    vapply(sp$groups, function(g) word(g$missing), character(1))
  } else {
    word(sp$missing %||% sp$parent_spec$missing)
  }
  used <- unique(used[!is.na(used)])
  if (!length(used)) "not recorded" else paste(used, collapse = " and ")
}

#' The uncertainty method of a record, in words
#'
#' What the record's intervals or standard errors rest on: for a fitted
#' model the standard error and test statistic family its estimator gives;
#' for a reliability record, each group's interval method as used, with the
#' replications, the seed and the replications used of a bootstrap; for a
#' Monte Carlo or bootstrap inference, its draws and seed.
#'
#' @param r A recorded analysis.
#' @return One string.
#' @author Ken Kelley
#' @noRd
sieve_record_uncertainty_text <- function(r) {
  if (!is.null(r$inference)) return(sieve_inference_text(r$inference))
  sp <- r$spec %||% list()
  kind <- as.character(r$kind %||% sp$kind %||% "")
  unrecorded <- paste("Actual standard error and model test methods were not recorded;",
    "the estimator label alone does not establish them")
  if (identical(kind, "reliability")) {
    groups <- sp$groups %||% list()
    estimates <- r$estimates %||% list()
    parts <- vapply(seq_along(groups), function(i) {
      g <- groups[[i]]
      om <- if (length(estimates) >= i && is.list(estimates[[i]])) estimates[[i]]$omega else NULL
      u <- om$uncertainty
      label <- u$label %||% g$interval$label %||% as.character(g$ci_method %||% "none")
      status <- u$status %||% g$uncertainty$status %||% "not recorded"
      used <- as.character(g$ci_method %||% "none")
      if (identical(used, "none") && !identical(status, "completed")) {
        reason <- u$reason %||% g$uncertainty$reason
        return(sprintf("%s: no interval (%s%s)", g$construct, status, if (length(reason) == 1L && nzchar(reason)) paste0("; ", sub("[.]$", "", reason)) else ""))
      }
      boot <- used %in% sieve_reliability_bootstrap_methods() || grepl("bootstrap", tolower(label), fixed = TRUE)
      detail <- if (boot) {
        seed <- u$rng$seed %||% g$seed
        sprintf(
          " (B = %s; seed %s; %s of %s replications used)",
          format(u$B %||% g$B %||% NA), if (length(seed) == 1L && !is.na(seed)) format(seed) else "not recorded",
          format(u$B_used %||% NA), format(u$B_attempted %||% u$B %||% g$B %||% NA)
        )
      } else {
        ""
      }
      sprintf("%s: %s%s, %s", g$construct, label, detail, status)
    }, character(1))
    return(if (length(parts)) paste(parts, collapse = "; ") else "not recorded")
  }
  if (identical(kind, "factor_score_uncertainty")) {
    e <- r$estimates %||% list()
    choices <- sp$choices %||% list()
    method <- e$method %||% choices$method
    label <- if (is.list(method)) method$label %||% method$method else method
    interval <- e$interval_method
    level <- e$conf_level %||% choices$conf_level
    if (is.null(interval)) {
      selected <- if (is.list(method)) method$method else method
      interval <- switch(selected %||% "",
        regression = "Normal-model posterior interval; fitted model parameters treated as known",
        Bartlett = "Conditional Wald interval for the person's factor value; fitted model parameters treated as known",
        "Score interval method not recorded; no model-parameter SE method is substituted")
    }
    return(paste0(label %||% "Factor scores", ": ", interval,
      if (length(level) == 1L && is.finite(level)) sprintf(" (%g%%)", 100 * level)))
  }
  if (kind %in% c("indirect_mc", "model_composite", "latent_boundary", "longitudinal_comparison")) {
    e <- if (is.list(r$estimates)) r$estimates else list()
    choices <- sp$choices %||% list()
    method <- e$method
    if (is.list(method)) method <- method$label %||% method$method
    B <- choices$B %||% e$B
    seed <- choices$rng$seed %||% e$rng$seed
    base <- if (length(method) == 1L && is.character(method) && nzchar(method)) method else unrecorded
    draws <- if (length(B) == 1L && is.finite(suppressWarnings(as.numeric(B))) && as.numeric(B) > 0) {
      sprintf(" (B = %s; seed %s)", format(B), if (length(seed) == 1L && !is.na(seed)) format(seed) else "not recorded")
    } else {
      ""
    }
    return(if (is.na(base)) "not recorded" else paste0(base, draws))
  }
  if (kind %in% c("bifactor_bootstrap", "reliability_bootstrap")) {
    B <- sp$B %||% r$estimates$B
    seed <- sp$rng$seed %||% sp$seed
    return(sprintf("bootstrap (B = %s; seed %s)", format(B %||% NA), if (length(seed) == 1L && !is.na(seed)) format(seed) else "not recorded"))
  }
  unrecorded
}

#' The estimator of a record, in words
#'
#' @param r A recorded analysis.
#' @return The estimator of a fitted model (`"MLR"`), the engine function
#'   of a reliability record (`"DMAR::reliability_omega"`), the parent
#'   model's estimator of an inference drawn from it, or `"not recorded"`.
#' @author Ken Kelley
#' @noRd
sieve_record_estimator_text <- function(r) {
  sp <- r$spec %||% list()
  if (length(sp$groups)) {
    engines <- unique(vapply(sp$groups, function(g) as.character(g$engine %||% NA_character_), character(1)))
    engines <- engines[!is.na(engines) & nzchar(engines)]
    if (length(engines)) {
      return(paste(paste0("DMAR::", engines), collapse = " and "))
    }
  }
  estimator <- sp$estimator %||% sp$parent_spec$estimator
  if (length(estimator) == 1L && !is.na(estimator) && nzchar(estimator)) as.character(estimator) else "not recorded"
}

#' The software versions a record carries, in words
#'
#' @param r A recorded analysis.
#' @return `"SIEVER 0.9.0; DMAR 1.1.0; lavaan 0.7.3; R 4.6.2"`, or
#'   `"not recorded"`.
#' @author Ken Kelley
#' @noRd
sieve_record_versions_text <- function(r) {
  v <- r$versions
  if (!is.list(v) || !length(v)) {
    return("not recorded")
  }
  parts <- vapply(names(v), function(pkg) {
    value <- as.character(v[[pkg]])
    if (length(value) == 1L && !is.na(value) && nzchar(value)) paste(pkg, value) else NA_character_
  }, character(1))
  parts <- parts[!is.na(parts)]
  if (length(parts)) paste(parts, collapse = "; ") else "not recorded"
}

#' The specification of a record, in words
#'
#' @param r A recorded analysis.
#' @return A character vector of one or more lines: the kind, the items or
#'   groups, the model syntax when the record holds one, and the ordered
#'   items.
#' @author Ken Kelley
#' @noRd
sieve_record_spec_text <- function(r) {
  sp <- r$spec %||% list()
  kind <- as.character(r$kind %||% sp$kind %||% "not recorded")
  its <- sp$items %||% sp$observed %||% unique(unlist(lapply(sp$groups %||% list(), function(g) g$items))) %||% character(0)
  lines <- sprintf(
    "Specification: kind %s; %s",
    kind,
    if (length(sp$groups)) {
      sprintf("%d group%s (%s)", length(sp$groups), if (length(sp$groups) == 1L) "" else "s",
        paste(vapply(sp$groups, function(g) sprintf("%s: %d items, %s", g$construct, length(g$items), g$label %||% g$coefficient %||% ""), character(1)), collapse = "; "))
    } else if (length(its)) {
      sprintf("%d observed variable%s (%s)", length(its), if (length(its) == 1L) "" else "s", paste(its, collapse = ", "))
    } else {
      "no observed variables recorded"
    }
  )
  if (length(sp$latent)) {
    lines <- c(lines, sprintf("Latent variables: %s.", paste(sp$latent, collapse = ", ")))
  }
  if (length(sp$ordered %||% character(0))) {
    lines <- c(lines, sprintf("Ordered items: %s.", paste(sp$ordered, collapse = ", ")))
  }
  if (length(sp$model) == 1L && is.character(sp$model) && nzchar(sp$model)) {
    lines <- c(lines, "Model syntax as fit:", paste0("    ", strsplit(trimws(sp$model), "\n", fixed = TRUE)[[1]]))
  }
  lines
}

#' The lines that describe one recorded analysis without refitting it
#'
#' What a report says about a record before its results: its note, its
#' status, its specification, whether its inputs are those now in the
#' analysis or earlier ones (and what changed), its actual sample (input
#' and fitted rows), its estimator and missing data treatment, its
#' uncertainty method, the software versions that made it, and when. A
#' failed record says so and that nothing was refit.
#'
#' @param r A recorded analysis (an element of `state$analyses`).
#' @param state Shared state.
#' @param now The current revision.
#' @return A character vector of lines.
#' @author Ken Kelley
#' @noRd
sieve_record_lines <- function(r, state, now = sieve_revision(state)) {
  id <- r$id %||% "?"
  original_label <- r$note %||% r$kind %||% ""
  labels <- as.list(shiny::isolate(state$analysis_labels) %||% list())
  label <- labels[[id]] %||% original_label
  status <- r$status$code %||% "not recorded"
  solved <- status %in% c("ok", "inadmissible", "partial")
  c(
    paste(id, label),
    if (!identical(label, original_label)) paste("Recorded analysis:", original_label),
    paste0(
      "Status: ", status, if (length(r$status$message) == 1L && nzchar(r$status$message)) paste0(" (", sub("[.]$", "", r$status$message), ")") else "",
      if (!solved) ". No result was stored for it and nothing was refit." else "."
    ),
    sieve_record_spec_text(r),
    sieve_record_currency(r, state, now),
    sprintf(
      "Actual sample: input rows: %s; fitted rows: %s.",
      sieve_record_n_text(r$sample_size$input %||% r$inputs$n),
      sieve_record_n_text(r$sample_size$fitted)
    ),
    sprintf("Estimator: %s; missing data: %s.", sieve_record_estimator_text(r), sieve_record_missing_text(r)),
    paste0("Uncertainty: ", sieve_record_uncertainty_text(r), "."),
    paste0("Software: ", sieve_record_versions_text(r), "."),
    paste("Recorded:", r$time %||% "not recorded"),
    paste("Exact input revision:", paste(unlist(r$revision), collapse = "; "))
  )
}

#' The lines for a requested record that is not in the registry
#'
#' @param id The requested identifier.
#' @return A character vector of lines.
#' @author Ken Kelley
#' @noRd
sieve_record_absent_lines <- function(id) {
  c(
    paste(id, "absent"),
    paste(
      "Status: absent. This analysis is not in the registry (it was not run in this session,",
      "or the project was opened without it). Nothing was refit in its place."
    )
  )
}

#' Catalog recorded analyses without refitting
#'
#' One row per record of the analysis registry: its kind and note, whether
#' its inputs are those now in the analysis (current) or earlier ones, with
#' what changed, its input and fitted rows, its estimator and missing data
#' treatment, its uncertainty method, and the software versions it recorded.
#' Read from the records only; nothing is computed.
#'
#' @param state Shared state.
#' @return A data frame with character columns `id`, `label`, `kind`,
#'   `note`, `scope`, `status`, `revision` (`"current"` or `"earlier"`),
#'   `changed`, `estimator`, `missing`, `uncertainty`, `versions`, `time`,
#'   and integer columns `items`, `rows` (input rows) and `fitted`.
#' @author Ken Kelley
#' @noRd
sieve_analysis_catalog <- function(state) {
  records <- shiny::isolate(state$analyses) %||% list()
  labels <- as.list(shiny::isolate(state$analysis_labels) %||% list())
  if (!length(records)) return(data.frame(
    id = character(),
    label = character(),
    kind = character(),
    note = character(),
    scope = character(),
    status = character(),
    revision = character(),
    changed = character(),
    items = integer(),
    rows = integer(),
    fitted = integer(),
    estimator = character(),
    missing = character(),
    uncertainty = character(),
    versions = character(),
    time = character(),
    stringsAsFactors = FALSE
  ))
  now <- sieve_revision(state)
  one_count <- function(x) {
    x <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
    if (!length(x) || any(!is.finite(x))) NA_integer_ else as.integer(sum(x))
  }
  rows <- lapply(names(records), function(id) {
    r <- records[[id]]
    sp <- r$spec %||% list()
    its <- sp$items %||% sp$observed %||% unique(unlist(lapply(
      sp$groups %||% list(),
      function(g) g$items
    ))) %||% character(0)
    scope <- sp$scope %||% paste(sp$scopes %||% sp$latent %||% names(sp$groups), collapse = ", ")
    if (!nzchar(scope)) scope <- "not recorded"
    why <- sieve_record_stale_reason(r, state, now)
    data.frame(
      id = id, label = labels[[id]] %||% r$note %||% r$kind %||% id,
      kind = r$kind %||% "not recorded", note = r$note %||% "",
      scope = scope, status = r$status$code %||% "not recorded",
      revision = if (is.null(why)) "current" else "earlier",
      changed = if (is.null(why)) "" else why,
      items = if (length(its)) length(its) else NA_integer_,
      rows = if (length(r$n) == 1L && is.numeric(r$n)) as.integer(r$n) else if (is.list(r$estimates) && length(r$estimates$n) == 1L && is.numeric(r$estimates$n)) as.integer(r$estimates$n) else if (length(r$inputs$n) == 1L && is.numeric(r$inputs$n)) as.integer(r$inputs$n) else if (is.data.frame(r$inputs$items)) nrow(r$inputs$items) else NA_integer_,
      fitted = one_count(r$sample_size$fitted),
      estimator = sieve_record_estimator_text(r),
      missing = sieve_record_missing_text(r),
      uncertainty = sieve_record_uncertainty_text(r),
      versions = sieve_record_versions_text(r),
      time = r$time %||% "not recorded", stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Whether a recorded result describes the data now in the analysis
#'
#' A recorded analysis keeps the revision of the inputs it was computed
#' from. A report of it says, in one line, whether those are the inputs now
#' in the analysis or earlier ones (another dataset, respondents removed or
#' restored since, an item reverse scored, the ledger changed), with the
#' input rows then and now, so a record of earlier data is never read as a
#' result for the data now in the analysis.
#'
#' @param r A recorded analysis (an element of `state$analyses`).
#' @param state Shared state.
#' @param now The current revision (see `sieve_revision()`).
#' @return One line of text.
#' @author Ken Kelley
#' @noRd
sieve_record_currency <- function(r, state, now = sieve_revision(state)) {
  why <- sieve_record_stale_reason(r, state, now)
  if (is.null(why)) {
    return("Inputs: the data and the item pool now in the analysis (current).")
  }
  n_then <- r$sample_size$input %||% r$inputs$n
  n_now <- NROW(shiny::isolate(state$items))
  rows <- if (length(n_then) == 1L && is.finite(n_then)) {
    sprintf("; input rows %d then, %d now", as.integer(n_then), as.integer(n_now))
  } else {
    ""
  }
  sprintf(
    "Inputs: earlier than those now in the analysis (%s%s). This is a record of that earlier analysis, not a result for the data now in the analysis.",
    why, rows
  )
}

#' Scientific Input Identity for a Recorded Analysis
#' @param inputs Plain input snapshot.
#' @return A key excluding labels, wording, and presentation metadata.
#' @author Ken Kelley
#' @noRd
sieve_record_input_key <- function(inputs) {
  if (!is.list(inputs) || !all(c("raw", "items", "meta") %in% names(inputs))) return(NULL)
  roles <- sieve_roles_table(inputs$raw, inputs$items, inputs$roles)
  if (is.data.frame(roles)) roles <- roles[, intersect(c("variable", "role", "kind"), names(roles)), drop = FALSE]
  values <- function(data) {
    if (is.data.frame(data)) data[] <- lapply(data, function(x) {
      if (is.numeric(x)) sieve_as_number(x) else sieve_plain_values(x)
    })
    sieve_data_key(data)
  }
  sieve_hash(list(raw = values(inputs$raw), items = values(inputs$items),
    meta = sieve_meta_key(inputs$meta), roles = roles,
    rows = sieve_rows_key(inputs$case_index, NROW(inputs$items)),
    construct_basis = inputs$construct_basis, sampling_design = inputs$sampling_design,
    scoring_policies = inputs$scoring_policies, holdout = inputs$holdout))
}

#' Why a Saved Analysis Has Earlier Scientific Inputs
#' @param r Saved analysis record.
#' @param state Shared study state.
#' @param now Current revision.
#' @return A reason, or NULL for current inputs.
#' @author Ken Kelley
#' @noRd
sieve_record_stale_reason <- function(r, state, now = sieve_revision(state)) {
  why <- sieve_stale_reason(r$revision, now)
  if (!is.null(why)) return(why)
  if (!is.null(r$scientific_input_key) &&
    !identical(r$scientific_input_key, sieve_record_input_key(sieve_replay_inputs(state)))) {
    return("the scientific inputs have changed since (including auxiliary values or variable roles)")
  }
  NULL
}

#' Write the readable report of selected recorded analyses
#'
#' For each requested identifier: the record's description
#' (`sieve_record_lines()`), its fit table when it holds fit measures, and
#' its stored results as readable tables; a requested record that is absent
#' from the registry is listed as absent, and a failed one as failed, and
#' neither is refit. The report ends with the bibliography of the methods
#' the present records used and the software they ran in, with the
#' versions recorded, and then the exact appendix (the specifications and
#' the unrounded stored results). Nothing is computed.
#'
#' @param state Shared state.
#' @param ids Selected analysis identifiers.
#' @return Character report.
#' @author Ken Kelley
#' @noRd
sieve_readable_saved_report <- function(state, ids) {
  records <- shiny::isolate(state$analyses) %||% list()
  ids <- unique(as.character(ids))
  present <- intersect(ids, names(records))
  lines <- c(
    "SIEVER report from recorded analyses",
    paste(
      "Only the selected recorded results appear here. No statistical analysis was run and nothing was refit:",
      "a requested result that is absent or failed is listed as such, and a record of earlier inputs is",
      "labelled earlier, never current."
    ),
    ""
  )
  now <- sieve_revision(state)
  for (id in ids) {
    if (!id %in% names(records)) {
      lines <- c(lines, sieve_record_absent_lines(id), "")
      next
    }
    r <- records[[id]]
    lines <- c(lines, sieve_record_lines(r, state, now))
    estimates <- r$estimates
    fit <- if (is.list(estimates)) estimates$fit else NULL
    if (is.numeric(fit) && length(fit) && !is.null(names(fit))) {
      family <- sieve_fit_family(fit)
      test_family <- sieve_fit_family(fit, "test")
      lines <- c(lines, paste(
        "Fit-index family:", if (is.na(family)) "not recorded" else family,
        "; model-test family:", if (is.na(test_family)) "not recorded" else test_family
      ))
      indices <- intersect(c("npar", "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "usrmr", "aic", "bic"), names(fit))
      shown <- sieve_fit_table(fit, indices)
      shown$Lower <- shown$Upper <- NA_real_
      shown$Interval <- ""
      rmsea <- which(indices == "rmsea")
      if (length(rmsea)) {
        shown$Lower[rmsea] <- unname(fit["rmsea_lo"])
        shown$Upper[rmsea] <- unname(fit["rmsea_hi"])
        # The level the interval was computed at, as the record carries it.
        shown$Interval[rmsea] <- sieve_rmsea_ci_words(sieve_fit_ci_level(fit))
      }
      # The unbiased SRMR, recorded beside the SRMR with its interval at the
      # RMSEA level (see sieve_fit_usrmr()).
      usrmr <- which(indices == "usrmr")
      held <- attr(fit, "families")$usrmr
      if (length(usrmr)) {
        shown$Lower[usrmr] <- unname(fit["usrmr_lo"])
        shown$Upper[usrmr] <- unname(fit["usrmr_hi"])
        shown$Interval[usrmr] <- sieve_rmsea_ci_words(held$conf_level %||% NA_real_)
      }
      lines <- c(lines, utils::capture.output(print(shown, row.names = FALSE, digits = 3L, na.print = "")))
      # An empty Unbiased SRMR row says why: DMAR's reason, or that it was
      # left for the page's button and not computed (see sieve_usrmr_note()).
      if (length(usrmr) && !is.null(held) && (!isTRUE(held$available) ||
        !is.finite(held$lower %||% NA_real_) || !is.finite(held$upper %||% NA_real_))) {
        lines <- c(lines, sieve_usrmr_note(held))
      }
      lines <- c(lines, "")
      estimates$fit <- NULL
    }
    if (is.null(estimates)) {
      lines <- c(lines, "Results: none stored (see the status above); nothing was refit.", "")
      next
    }
    tables <- sieve_readable_result_tables(estimates)
    for (nm in names(tables)) lines <- c(
      lines,
      nm,
      utils::capture.output(print(tables[[nm]],
        row.names = FALSE,
        digits = 3L
      )),
      ""
    )
  }
  used <- sieve_records_methods(records[present])
  paste(
    c(
      lines,
      sieve_reference_lines(used$keys, versions = used$versions, markdown = FALSE),
      "",
      "Exact specifications and unrounded stored results",
      sieve_saved_analyses_report(
        state,
        ids
      )
    ),
    collapse = "\n"
  )
}

#' Create the shared application state
#'
#' A single [shiny::reactiveValues()] store is shared across all modules.
#' The `meta` element is the item ledger that the winnowing engine reads
#' and writes; nothing is ever destroyed, only parked (status "parked"),
#' so any removal is reversible.
#'
#' @return A [shiny::reactiveValues()] object with the fields documented
#'   inline below.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny reactiveValues
sieve_new_state <- function() {
  do.call(shiny::reactiveValues, lapply(sieve_state_fields(), `[[`, "default"))
}

#' State Fields, Their Defaults, and Their Persistence Scope
#' @return A named field registry. Each call creates fresh session objects.
#' @author Ken Kelley
#' @noRd
sieve_state_fields <- function() {
  defaults <- list(
    five_continuous = TRUE, # proposed type rule for newly opened data
    dataset_name = NULL, # display name of the loaded data
    dataset_id = NULL, # identity of this load (see sieve_new_dataset)
    dataset_seq = 0L, # number of datasets loaded this session
    analyses = list(), # the analysis registry (see sieve_register_analysis)
    transforms = list(), # every applied transformation (see sieve_apply_reverse)
    roles = NULL, # what each column is for (see sieve_propose_roles)
    held_rows = list(), # ledger rows of columns that left the item pool (see sieve_set_roles)
    project = NULL, # where an opened project came from (see sieve_project_restore)
    project_control = list(), # session-only save/download acknowledgments
    project_name = "Untitled project",
    metadata_edits = list(), # reversible metadata changes, bound to the loaded data
    analysis_labels = list(), # researcher labels for immutable analysis records
    content_mapping = list(), # reviewed expert-rating to item crosswalk
    score_policy = list(min_count = 1L, min_prop = 0),
    scoring_policies = list(), # construct-specific minimum-response rules
    norm_references = list(),
    norm_transform_records = list(),
    anchor_decisions = list(),
    ordinal_impact_records = list(), # frozen calibration references, retained for later samples
    sampling_design = list(unit = "independent", cluster = NULL),
    screening_spec = list(min_run = NULL, min_answered = 3L, missing = "break", values = "raw"),
    case_actions = list(), # immutable observation inclusion decisions
    item_actions = list(), # immutable item inclusion decisions, including prior reasons
    presentation = list(), # presentation choices, independent of scientific settings
    workflow_history = list(),
    efa_settings = list(),
    cfa_settings = list(),
    sem_settings = list(),
    reliability_settings = list(),
    replay_data = list(), # content-addressed source data and changed columns
    replay_states = list(), # compact input revisions
    replay_steps = list(), # step number -> replay state key
    replay_operations = list(), # explicit pre/post revisions; original code stays verbatim
    replay_versions = NULL, # engine versions when recording first begins
    replay_baseline = NULL, # checked state of a project opened without input revisions
    scientific_dataset_id = NULL,
    construct_records = list(),
    frozen_claims = list(),
    assistant_records = list(),
    validation_events = list(),
    raw = NULL, # full data.frame as read
    items = NULL, # item columns only (active and parked)
    meta = NULL, # the item ledger (see sieve_build_meta)
    theory = NULL, # list(k, label, map): the researcher-specified structure
    construct_basis = "theoretical", # "theoretical" (researcher-specified) or
    #   "empirical" (provisional grouping, EFA driven)
    palette = "nd", # active embedded palette for app visuals
    estimator = NULL, # chosen estimator after type confirmation
    estimator_choice = "auto", # the study's estimator choice, made on the Ingest tab (step 4);
    #   every model tab follows it unless set otherwise there
    #   (see sieve_estimator_effective)
    case_index = NULL, # the original row number of each row of raw and items (set at load);
    #   rows removed from the analysis are held in removed_cases
    removed_cases = list(), # respondents removed from the analysis, with their rows and the reason
    #   (see the Screening tab), restorable
    holdout = list(), # validation reservations: rows reserved before development, their seed,
    #   frozen versions and evaluations (see sieve_reserve_holdout in fct_holdout.R)
    confirmed = FALSE, # has the user confirmed the type grid?
    confirmation = NULL, # one recorded baseline and origin for the type gate
    thresholds = sieve_default_thresholds(), # winnowing flag cutoffs
    decisions = list(), # the researcher's decision records (see sieve_record_decision)
    predictions = NULL, # stated construct to criterion predictions (see sieve_set_prediction)
    cache = list(), # results shared across tabs, keyed by their inputs (see sieve_state_basis), and the
    #   EFA tab's kept solution for the Items tab (cache$efa, a plain environment, see sieve_efa_store);
    #   session only: never saved in a project, emptied with the data
    fit_store = sieve_fit_store(), # fitted models of this session by exact identity (see
    #   sieve_fit_store); emptied by sieve_new_dataset, never saved in a project
    page_results = list(), # session-only result status for the shared page guide; no fitted objects
    item_decision_request = NULL, # session-only: a page's request to open the Exclude / Include dialog
    #   with items filled in (see sieve_item_decision_request)
    codelog = character(0) # accumulating annotated reproducible R
  )
  primary <- c(
    "dataset_name", "dataset_id", "dataset_seq", "raw", "items", "meta", "theory", "roles",
    "confirmed", "estimator", "estimator_choice", "construct_basis", "thresholds",
    "palette", "transforms", "analyses", "codelog", "held_rows", "project"
  )
  session <- c("cache", "fit_store", "page_results", "project_control", "five_continuous", "item_decision_request")
  fingerprint_exclude <- c(session, "project", "dataset_seq", "dataset_id", "presentation",
    "replay_states", "replay_steps", "replay_data", "replay_operations", "replay_versions", "replay_baseline")
  out <- lapply(names(defaults), function(name) list(default = defaults[[name]],
    primary = name %in% primary, persisted = !name %in% session,
    reset = !name %in% c("palette", "five_continuous", "dataset_seq"),
    fingerprint = !name %in% fingerprint_exclude))
  names(out) <- names(defaults)
  out
}

#' Fields Excluded From a Persistent or Scientific State View
#' @param scope Persistence or fingerprint.
#' @return Field names.
#' @author Ken Kelley
#' @noRd
sieve_state_exclusions <- function(scope = c("persisted", "fingerprint")) {
  scope <- match.arg(scope)
  names(Filter(function(field) !isTRUE(field[[scope]]), sieve_state_fields()))
}

#' Default winnowing flag thresholds
#'
#' Cutoffs for the per item removal and reassignment flags. These are
#' screening conventions, not decision rules, and the user can adjust them
#' in the winnowing engine. Salience is set at 0.30 (about 9% of variance,
#' a common floor for a meaningful loading); the low loading flag at 0.40;
#' communality and corrected item total correlation at 0.30; near duplicate
#' redundancy at an inter item correlation of 0.80.
#'
#' @return A named numeric list.
#' @author Ken Kelley
#' @noRd
sieve_default_thresholds <- function() {
  list(
    load_low = 0.40,
    salient = 0.30,
    communality_low = 0.30,
    item_total_low = 0.30,
    redundancy_high = 0.80
  )
}

#' The standard gate message for analysis tabs
#'
#' Every analysis tab is gated until data are loaded and item types are
#' confirmed. This helper returns an actionable message distinguishing the
#' two states (no data yet vs awaiting confirmation), or `NULL` when the
#' pipeline is unlocked, so all tabs guide the user the same way. Types
#' are confirmed on load only for a bundled example whose items are all
#' continuous; a file of the researcher's own is always confirmed with the
#' Ingest tab's button, so the second message is what such a file shows
#' until then.
#'
#' @param state The shared state store.
#' @return A [shiny::div()] alert, or `NULL` when no gate applies.
#' @author Ken Kelley
#' @noRd
sieve_gate <- function(state) {
  message <- sieve_gate_text(state)
  if (is.null(message)) return(NULL)
  shiny::div(class = if (is.null(state$meta)) "alert alert-info" else "alert alert-warning", message)
}

#' The Shared Analysis Gate in Plain Words
#' @param state Shared application state.
#' @return A message or NULL when the study is confirmed.
#' @author Ken Kelley
#' @noRd
sieve_gate_text <- function(state) {
  if (is.null(state$meta)) return("No data loaded yet. Go to Ingest and press Load in the action bar above the panels (steps 1 and 2). The bundled example is already selected.")
  if (!isTRUE(sieve_confirmation_status(state)$confirmed)) return("Item types await confirmation. Review the included items on Ingest, then press Confirm the study in the action bar above the panels (steps 3 and 4). Results from before an analytic edit are not current.")
  NULL
}

#' Record the Single Baseline of a Study Confirmation
#' @param state Shared study state.
#' @param origin Confirmation route.
#' @param record_time Whether this action happened now, rather than being
#'   rebuilt on opening a confirmed project that holds no confirmation record.
#' @return The confirmation record.
#' @author Ken Kelley
#' @noRd
sieve_remember_confirmation <- function(state, origin = "button", record_time = TRUE) {
  shiny::isolate({
    record <- list(dataset_id = state$dataset_id,
      scientific_dataset_id = state$scientific_dataset_id,
      meta = sieve_ledger_analytic(state$meta), data_key = sieve_data_key(state$items),
      rows_key = sieve_rows_key(state$case_index, NROW(state$items)),
      roles_key = sieve_hash(sieve_roles_table(state$raw, state$items, state$roles)[c("variable", "role")]), origin = origin,
      time = if (record_time) format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") else NULL)
    state$confirmation <- record
    record
  })
}

#' Bind an Audit Record to Its Scientific Dataset
#' @param record Plain audit record.
#' @param state Shared study state.
#' @return The record with persistent scientific dataset provenance.
#' @author Ken Kelley
#' @noRd
sieve_audit_stamp <- function(record, state) {
  record$scientific_dataset_id <- shiny::isolate(state$scientific_dataset_id)
  record
}

#' Read Confirmation and the Changes Against Its Recorded Baseline
#' @param state Shared study state.
#' @return Confirmation flag, baseline, change description, and estimator change.
#' @author Ken Kelley
#' @noRd
sieve_confirmation_status <- function(state) {
  record <- state$confirmation
  confirmed <- isTRUE(state$confirmed)
  if (is.null(record) || is.null(state$meta)) return(list(confirmed = confirmed,
    record = record, change = NULL, estimator_changed = FALSE))
  rows <- sieve_rows_key(state$case_index, NROW(state$items))
  roles <- sieve_hash(sieve_roles_table(state$raw, state$items, state$roles)[c("variable", "role")])
  change <- sieve_confirm_change(record, list(meta = sieve_ledger_analytic(state$meta), roles_key = roles,
    data_key = if (identical(rows, record$rows_key)) sieve_data_key(state$items) else record$data_key))
  implied <- function(meta) sieve_estimator_resolve(meta$type[meta$status == "active"],
    n = NROW(state$items), choice = sieve_estimator_effective("study", state))$estimator
  moved <- !identical(implied(record$meta), implied(state$meta))
  allowed <- all(attr(change, "kinds") %in% c("status", "facet"))
  list(confirmed = confirmed && identical(record$dataset_id, state$dataset_id) &&
    allowed && !moved && identical(record$roles_key, roles),
    record = record, change = change, estimator_changed = moved)
}

#' Append an annotated step to the reproducible code log
#'
#' Every consequential action records a plain language note and the
#' equivalent R. The Reproducible Code view renders the accumulated script.
#'
#' @param state The shared state store.
#' @param note A short plain language description of the action.
#' @param code A character vector or string of the equivalent R.
#' @param engine_identity `NULL`, or named identity records of engines that
#'   computed the step in a background R process; they replace this
#'   process's records of those packages in the step's engine identity
#'   (`sieve_recording_engine_identity()`).
#' @return The state store, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_log <- function(state, note, code, inputs = NULL, before = NULL,
                      kind = c("analysis", "mutation", "import"), passive = FALSE,
                      engine_identity = NULL) {
  kind <- match.arg(kind)
  existing <- shiny::isolate(state$codelog) %||% character(0)
  # Notes and source are separate channels. Every line of free text gets
  # its own comment marker, including text after a researcher-entered newline.
  note <- paste(as.character(note), collapse = " ")
  note <- gsub("[\r\n]+", " ", note)
  code <- unlist(strsplit(as.character(code), "\n", fixed = TRUE), use.names = FALSE)
  records <- sieve_script_records(state)
  # Give every logged step an operation record: a step without one (its
  # inputs were not saved) gets a record that marks it, and the session
  # script writes it as a commented record and does not replay it.
  operations <- shiny::isolate(state$replay_operations) %||% list()
  for (record in records) {
    operation <- operations[[record$number]] %||% list(legacy = TRUE)
    operation$note <- record$note
    operation$code <- record$code
    operations[[record$number]] <- operation
  }
  operations <- operations[names(records)]
  state$replay_operations <- operations
  x <- inputs %||% sieve_replay_inputs(state)
  if (length(records)) {
    last <- tail(records, 1L)[[1L]]
    number <- last$number
    old <- shiny::isolate(state$replay_steps[[number]])
    same <- shiny::isolate({
      record <- state$replay_states[[old %||% ""]]
      operation <- state$replay_operations[[number]]
      !is.null(record) && identical(operation$kind, kind) &&
        identical(sieve_engine_keys(operation$engine_identity %||% list()),
          sieve_engine_keys(sieve_recording_engine_identity(engine_identity))) &&
        identical(isTRUE(operation$passive), isTRUE(passive)) &&
        identical(sieve_replay_expand(record, state$replay_data %||% list()), x)
    })
    if (identical(last$note, note) && identical(last$code[nzchar(last$code)], code[nzchar(code)]) && same)
      return(invisible(state))
  }
  step <- if (length(records)) max(as.integer(names(records))) + 1L else 1L
  sieve_replay_capture(state, step, inputs = x, before = before, kind = kind, passive = passive,
    engine_identity = engine_identity)
  operations <- shiny::isolate(state$replay_operations) %||% list()
  operation <- operations[[as.character(step)]]
  if (!is.null(operation)) {
    operation$note <- note
    operation$code <- code
    operation$scientific_dataset_id <- shiny::isolate(state$scientific_dataset_id)
    operations[[as.character(step)]] <- operation
    state$replay_operations <- operations
  }
  state$codelog <- c(existing, sprintf("# [%d] %s", step, note), code, "")
  invisible(state)
}

#' Move a progress bar that may not be open
#'
#' [shiny::setProgress()] warns and does nothing when no progress bar is
#' open (a test that replaces [shiny::withProgress()] with its expression,
#' for one). A computation that reports its steps through this helper runs
#' the same with or without a bar; the one warning muffled here is that
#' plumbing notice, never a condition from an engine.
#'
#' @param value The bar's value.
#' @param detail Text under the message.
#' @return `NULL`, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_progress_set <- function(value, detail = NULL) {
  tryCatch(
    withCallingHandlers(
      shiny::setProgress(value = value, detail = detail),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) NULL
  )
  invisible(NULL)
}

#' Make a project's labelled columns readable in this session
#'
#' A column read from an SPSS or Stata file by haven carries the class
#' `haven_labelled`, whose numeric conversion is a method that lives in
#' haven. A session that opens a saved project has not read such a file and
#' may not have loaded haven; once another package has loaded vctrs (ggplot2
#' does, for the first plot drawn), every `as.numeric()` on such a column
#' dispatches to vctrs and stops with "Can't convert `x` <haven_labelled> to
#' <double>", on every tab. Loading haven's namespace, which wrote the
#' columns, restores the conversion; when haven cannot be loaded the columns
#' are read as their codes, which is what every analysis reads from them.
#'
#' @param snapshot A project snapshot (a list with `raw` and `items`).
#' @param load Whether to try loading haven first.
#' @return The snapshot, with labelled columns made readable.
#' @author Ken Kelley
#' @noRd
sieve_labelled_ready <- function(snapshot, load = TRUE) {
  labelled <- function(df) {
    is.data.frame(df) && any(vapply(df, inherits, logical(1), what = "haven_labelled"))
  }
  if (!labelled(snapshot$raw) && !labelled(snapshot$items)) {
    return(snapshot)
  }
  if (isTRUE(load) && requireNamespace("haven", quietly = TRUE)) {
    return(snapshot)
  }
  codes <- function(df) {
    if (!is.data.frame(df)) {
      return(df)
    }
    df[] <- lapply(df, function(v) {
      if (inherits(v, "haven_labelled")) as.vector(unclass(v)) else v
    })
    df
  }
  snapshot$raw <- codes(snapshot$raw)
  snapshot$items <- codes(snapshot$items)
  snapshot
}

#' Make sure the loaded data have an identity
#'
#' The load sites give every dataset its identity with `sieve_new_dataset()`.
#' Data that reached the state some other way (a state assembled by hand)
#' have none, and a record cannot be bound to a dataset without one, so the
#' first operation that needs the identity assigns it here.
#'
#' @param state The shared state store.
#' @return The dataset identifier.
#' @author Ken Kelley
#' @noRd
sieve_ensure_dataset <- function(state) {
  id <- shiny::isolate(state$dataset_id)
  if (is.null(id) || length(id) != 1L || is.na(id)) {
    sieve_new_dataset(state)
  }
  shiny::isolate(state$dataset_id)
}

#' Why a stored result no longer applies to the current inputs
#'
#' The rule every module-local result follows: a cached result, an undo
#' record, or a search candidate carries the revision it was computed from
#' (see `sieve_revision()`) and is applied only while this function returns
#' `NULL` for it.
#'
#' @param made The revision the result was computed from.
#' @param now The current revision.
#' @param what Which parts must agree: any of `"dataset"`, `"data"`, and
#'   `"meta"`.
#' @return `NULL` when the result still applies, or a short reason, in plain
#'   words, naming the first part that changed.
#' @author Ken Kelley
#' @noRd
sieve_stale_reason <- function(made, now, what = c("dataset", "data", "meta")) {
  what <- match.arg(what, several.ok = TRUE)
  if (is.null(made)) {
    return("the data it was computed from were not recorded")
  }
  for (part in what) {
    if (!sieve_same_revision(made, now, what = part)) {
      return(switch(part,
        dataset = "a different dataset has been loaded since",
        data = paste(
          "the item data have changed since (for example, an item was reverse scored,",
          "or respondents were removed from or restored to the analysis)"
        ),
        meta = paste(
          "the item ledger has changed since (for example, an item was included,",
          "excluded, reassigned, retyped or reverse scored)"
        )
      ))
    }
  }
  NULL
}

#' The analytic part of the item ledger
#'
#' The six columns an analysis depends on, in the ledger's order: which
#' items exist, their construct, facet, measurement level, inclusion, and
#' keying. The display label and the wording are not part of it, so an
#' edit to either leaves every stored result current (see
#' `sieve_meta_key()`, whose hash covers the same columns).
#'
#' @param meta The item ledger, or `NULL`.
#' @return A data frame with `item`, `construct`, `facet`, `type`, `status`
#'   and `reverse` (character and logical columns, no row names), or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_ledger_analytic <- function(meta) {
  if (is.null(meta)) {
    return(NULL)
  }
  pick <- function(name, as) {
    if (name %in% names(meta)) as(meta[[name]]) else rep(as(NA), nrow(meta))
  }
  data.frame(
    item = pick("item", as.character), construct = pick("construct", as.character),
    facet = pick("facet", as.character), type = pick("type", as.character),
    status = pick("status", as.character), reverse = pick("reverse", as.logical),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' What changed since a result was computed, in one sentence
#'
#' Compares the analytic ledger a result was computed from (see
#' `sieve_ledger_analytic()`) with the current one and names what moved,
#' item by item: items reverse scored (or their reversal undone), items
#' excluded or included, items moved to another construct or facet, items
#' retyped, and items that entered or left the ledger. A different dataset,
#' and item values that changed with the ledger unchanged, are named as
#' such. Up to five items are named per change; the rest are counted.
#'
#' @param then The analytic ledger the result was computed from.
#' @param now The current analytic ledger.
#' @param dataset_then,dataset_now The dataset identities (see
#'   `sieve_new_dataset()`), or `NULL`.
#' @param data_then,data_now The content keys of the item data (see
#'   `sieve_data_key()`), or `NULL`.
#' @return `NULL` when nothing analytic changed, otherwise one sentence
#'   without a final period, the parts separated by semicolons, for example
#'   `"item_120 reverse scored; 2 items excluded (item_121, item_122)"`.
#' @author Ken Kelley
#' @noRd
sieve_stale_sentence <- function(then, now, dataset_then = NULL, dataset_now = NULL,
                                 data_then = NULL, data_now = NULL) {
  if (!is.null(dataset_then) && !is.null(dataset_now) && !identical(dataset_then, dataset_now)) {
    return("a different dataset was loaded")
  }
  if (is.null(then) || is.null(now)) {
    return("the item ledger it was computed from was not recorded")
  }
  named <- function(items, what, past) {
    n <- length(items)
    if (!n) {
      return(NULL)
    }
    if (n == 1L) {
      return(sprintf("%s %s%s", items, what, past))
    }
    shown <- paste(utils::head(items, 5L), collapse = ", ")
    if (n > 5L) {
      shown <- sprintf("%s and %d more", shown, n - 5L)
    }
    sprintf("%d items %s%s (%s)", n, what, past, shown)
  }
  parts <- character(0)
  diff <- sieve_ledger_diff(then, now)
  both <- diff$common
  a <- diff$a
  b <- diff$b
  same <- function(x, y) (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
  reversed <- both[!same(a$reverse, b$reverse) & isTRUE_vec(b$reverse)]
  undone <- both[!same(a$reverse, b$reverse) & !isTRUE_vec(b$reverse)]
  excluded <- both[a$status == "active" & b$status != "active"]
  included <- both[a$status != "active" & b$status == "active"]
  moved <- both[!same(a$construct, b$construct)]
  refacetted <- both[same(a$construct, b$construct) & !same(a$facet, b$facet)]
  retyped <- both[!same(a$type, b$type)]
  parts <- c(
    parts,
    named(reversed, "reverse scored", ""),
    named(undone, "reversal undone", ""),
    named(excluded, "excluded", ""),
    named(included, "included", ""),
    if (length(moved) == 1L) {
      sprintf("%s moved to construct %s", moved, b$construct[match(moved, b$item)])
    } else {
      named(moved, "moved to another construct", "")
    },
    if (length(refacetted) == 1L) {
      sprintf(
        "%s moved to facet %s", refacetted,
        b$facet[match(refacetted, b$item)] %||% "none"
      )
    } else {
      named(refacetted, "moved to another facet", "")
    },
    if (length(retyped) == 1L) {
      sprintf("%s retyped as %s", retyped, b$type[match(retyped, b$item)])
    } else {
      named(retyped, "retyped", "")
    },
    named(diff$joined, "added to the ledger", ""),
    named(diff$left, "no longer in the ledger", "")
  )
  if (!length(parts) && !is.null(data_then) && !is.null(data_now) &&
    !identical(data_then, data_now)) {
    parts <- "the item values changed"
  }
  if (!length(parts)) {
    return(NULL)
  }
  paste(parts, collapse = "; ")
}

#' Elementwise `isTRUE()`
#'
#' @param x A logical vector, possibly with missing values.
#' @return A logical vector of the same length with `TRUE` where `x` is
#'   `TRUE` and `FALSE` elsewhere.
#' @author Ken Kelley
#' @noRd
isTRUE_vec <- function(x) {
  !is.na(x) & x
}

#' Why a kept result cannot be applied to the current inputs
#'
#' For a result that changes the ledger when it is applied (an optimizer
#' search). It applies to the revision it was computed from (`revision`) and
#' to the one revision its own last application produced from that
#' (`applied_revision`, when there is one), so a second choice from the same
#' result can replace the first; it applies to nothing else.
#'
#' @param result A list with `revision` and, optionally, `applied_revision`.
#' @param now The current revision.
#' @return `NULL` when the result applies, or the reason it does not.
#' @author Ken Kelley
#' @noRd
sieve_result_stale <- function(result, now) {
  why <- sieve_stale_reason(result$revision, now)
  if (is.null(why) || is.null(result$applied_revision)) {
    return(why)
  }
  sieve_stale_reason(result$applied_revision, now)
}

#' Null-coalescing operator
#'
#' Returns `x` unless it is `NULL`, in which case `y`. Canonical definition for
#' the whole package (used for input defaults before the UI has rendered). Note
#' it does NOT treat `NA` as missing; for a numeric input that the user may have
#' cleared (yielding `NA`), use `sieve_finite_or()`.
#'
#' @param x,y Values; `y` is the fallback.
#' @return `x` if non-`NULL`, else `y`.
#' @author Ken Kelley
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' A finite scalar or a default
#'
#' Guards the classic Shiny crash where a cleared `numericInput` returns `NA`
#' (or `NULL`) and is then passed to a function that errors on `NA` (for
#' example `psych::fa(nfactors = NA)`). Returns `default` whenever `x` is `NULL`,
#' empty, or non-finite.
#'
#' @param x A value, typically a numeric input.
#' @param default The fallback returned when `x` is not a finite scalar.
#' @return A finite scalar.
#' @author Ken Kelley
#' @noRd
sieve_finite_or <- function(x, default) {
  if (is.null(x) || !length(x) || !is.finite(x[1])) default else x[1]
}

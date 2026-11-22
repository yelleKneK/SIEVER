# A compact guide in the same place on every page. Reading the guide never
# requests a fit, a correlation matrix, a bootstrap or a background worker.

#' Input identity for the session's page-status guide
#' @param state Shared application state.
#' @return Plain input identity; no model is evaluated.
#' @author Ken Kelley
#' @noRd
sieve_page_revision <- function(state) {
  items <- state$items
  state$sampling_design
  design <- sieve_sampling_spec(state)
  column <- design$cluster
  if (length(column) == 1L && column %in% names(state$raw)) {
    design$values_key <- sieve_hash(state$raw[[column]])
  }
  list(
    dataset = state$dataset_id,
    data = if (!is.null(items)) sieve_data_key(items) else NULL,
    meta = if (!is.null(state$meta)) sieve_meta_key(state$meta) else NULL,
    rows = sieve_hash(state$case_index %||% seq_len(nrow(items) %||% 0L)),
    sampling = design
  )
}

#' The identity of what the correlation-based analyses read
#'
#' The included items, their values on the rows in the analysis, their
#' confirmed types and keying, the respondents, and the study's estimator
#' setting (which sets the correlations they share): what the
#' Dimensionality tab's criteria, the EFA tab's solution and the Items
#' tab's diagnostics are computed from. A construct, a facet or a label is
#' not part of it, so renaming a construct or editing a facet leaves those
#' results current.
#'
#' @param state Shared application state (read reactively).
#' @return A plain list.
#' @author Ken Kelley
#' @noRd
sieve_pool_revision <- function(state) {
  items <- state$items
  meta <- state$meta
  if (is.null(items) || is.null(meta)) {
    return(list(dataset = state$dataset_id, data = NULL))
  }
  active <- meta[meta$status == "active", , drop = FALSE]
  its <- intersect(as.character(active$item), names(items))
  at <- match(its, active$item)
  list(
    dataset = state$dataset_id,
    data = sieve_data_key(as.data.frame(items)[, its, drop = FALSE]),
    items = its,
    types = as.character(active$type)[at],
    reverse = if ("reverse" %in% names(active)) as.logical(active$reverse)[at] else NULL,
    rows = sieve_hash(state$case_index %||% seq_len(nrow(items))),
    estimator = sieve_estimator_effective("study", state)
  )
}

#' Publish a stored result's status without publishing its fitted object
#'
#' A module that keeps its own currency (the identity of exactly what its
#' computation reads, re-published whenever that identity is evaluated
#' again) passes `own = TRUE`, and the page guide then reports the state
#' the module published. Without it the guide also marks a current record
#' stale whenever the data, the ledger, the respondents or the sampling
#' design changed after it was published, the safety net for a module
#' that publishes once.
#'
#' @param state Shared application state.
#' @param page Navigation value.
#' @param id Unique namespaced control or result identifier.
#' @param title Short name of the result.
#' @param status A list containing state, sentence, key and optionally when.
#' @param action The page's action label, when available.
#' @param own `TRUE` when the module keeps its own currency (see above).
#' @return Invisibly the small status record.
#' @author Ken Kelley
#' @noRd
sieve_page_publish <- function(state, page, id, title, status, action = NULL, own = FALSE) {
  if (!is.list(status)) status <- list(state = as.character(status))
  phase <- status$state %||% "not_run"
  if (!phase %in% c("current", "stale", "not_run", "failed", "running", "blocked")) phase <- "blocked"
  record <- list(
    title = title, state = phase,
    sentence = if (length(status$sentence %||% status$error)) paste(status$sentence %||% status$error, collapse = " ") else NULL,
    key = if (!is.null(status$key)) sieve_hash(status$key) else NULL,
    when = status$when, action = action, own = isTRUE(own),
    revision = if (isTRUE(own)) NULL else shiny::isolate(sieve_page_revision(state))
  )
  shiny::isolate({
    pages <- state$page_results %||% list()
    old <- pages[[page]][[id]]
    if (!identical(old, record)) {
      pages[[page]][[id]] <- record
      state$page_results <- pages
    }
  })
  invisible(record)
}

#' The pages with one Update action
#'
#' Each of these pages has an `update_go` button in its module's namespace
#' that recomputes, in turn, every result on the page an edit made stale;
#' the page guide names that action in its summary and has no button of
#' its own, so the page has one Update.
#'
#' @return Page identifiers (the navigation values, which are the module
#'   identifiers).
#' @author Ken Kelley
#' @noRd
sieve_pages_with_update <- function() {
  names(Filter(function(page) isTRUE(page$update), sieve_pages()))
}

#' The next useful action on each application page
#' @return Named list of page labels and instructions.
#' @author Ken Kelley
#' @noRd
sieve_pages <- function() {
  pages <- list(
    ingest = list(label = "Ingest", next_step = paste(
      "Review roles, item types, inclusion and reverse keying; then confirm the study in step 4."
    )),
    screen = list(label = "Screening", next_step = paste(
      "Review respondent and item flags. Decide which cases or items to retain; a flag alone removes nothing."
    )),
    content = list(label = "Content Validity", next_step = paste(
      "Choose an expert-ratings file and review its item mapping. Respondent data are optional here."
    )),
    retention = list(label = "Dimensionality", next_step = paste(
      "Review the scree plot; request parallel analysis or another method only when it informs your factor-count decision."
    )),
    efa = list(label = "EFA", next_step = paste(
      "Choose the factor count, correlations, extraction and rotation. Use Run selected EFA for a first fit and Update after an edit."
    )),
    winnow = list(label = "Items", next_step = paste(
      "Review item diagnostics and proposed decisions. Apply only changes you can justify, then review affected analyses."
    )),
    cfa = list(label = "CFA", next_step = paste(
      "Review the construct model and estimator. Fit the model when requested; optional comparisons have separate controls."
    )),
    reliab = list(label = "Reliability", next_step = paste(
      "Review the score definition and reliability estimates. Request intervals or factor scores only when needed."
    )),
    # The Scoring and Norms sub-tab of Reliability, a page of its own
    # under Scores (fct_workspaces.R).
    scoring = list(label = "Scoring and Norms", next_step = paste(
      "Review each construct's scoring rule; describe the eligible population before freezing a reference."
    )),
    validity = list(label = "Validity", next_step = paste(
      "State the validity question and predictions, then request the corresponding model or interval."
    )),
    finalize = list(label = "CV and Invariance", next_step = paste(
      "Choose the split or grouping design, estimator and seed before running the selected validation analysis."
    )),
    optimize = list(label = "Optimizer", next_step = paste(
      "Under empirical assignments, state constraints and run a search. Review a current candidate before applying it."
    )),
    bifactor = list(label = "Bifactor", next_step = paste(
      "Specify the general and group factors, then fit the model. Request bootstrap intervals separately if needed."
    )),
    # Psychometric network models, beside the factor models.
    network = list(label = "Network", next_step = paste(
      "Estimate the item network, the residual network beside the CFA, or the latent network of the constructs;",
      "each waits for its button and is read with its standard errors."
    )),
    irt = list(label = "IRT", next_step = paste(
      "Select the construct and confirm ordered responses, then fit its graded response model."
    )),
    sem = list(label = "SEM", next_step = paste(
      "Review measurement and structural paths and their estimator before fitting the structural model."
    )),
    repro = list(label = "Reproducible Code", next_step = paste(
      "Select recorded analyses for Report saved analyses (it runs nothing); Run a new analysis and draft refits under its own name;",
      "download the session script with its verified input. Earlier runs retain their original inputs."
    ))
  )
  attributes <- data.frame(
    id = c("ingest", "screen", "content", "retention", "efa", "winnow", "cfa", "reliab", "scoring", "validity", "finalize", "optimize", "bifactor", "network", "irt", "sem", "repro"),
    workspace = c("study", "study", "study", "structure", "structure", "structure", "structure", "scores", "scores", "evidence", "evidence", "structure", "structure", "structure", "scores", "evidence", "report"),
    workspace_order = c(1, 3, 2, 1, 2, 3, 5, 1, 2, 1, 2, 4, 6, 7, 3, 3, 1),
    core = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    more = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    update = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  for (id in names(pages)) {
    row <- attributes[attributes$id == id, , drop = FALSE]
    module <- if (id == "scoring") "finalize" else id
    pages[[id]] <- c(pages[[id]], as.list(row[setdiff(names(row), "id")]),
      list(data_optional = id %in% c("content", "repro"),
        construct_model = id %in% c("cfa", "reliab", "validity", "bifactor", "sem"),
        module = module,
        ui = if (id == "scoring") "mod_finalize_scoring_ui" else paste0("mod_", module, "_ui"),
        server = if (id == "scoring") NULL else paste0("mod_", module, "_server")))
  }
  pages
}

#' Page Labels and Instructions From the Page Registry
#' @return Named list of page labels and instructions.
#' @author Ken Kelley
#' @noRd
sieve_page_catalog <- function() {
  lapply(sieve_pages(), function(page) page[c("label", "next_step")])
}



#' Whether the Construct Models Wait for a Construct
#'
#' The one test of the page gate (`sieve_page_status()`) for the pages that
#' fit a construct model (CFA, Reliability, Validity, Bifactor and SEM; the
#' `construct_model` pages of `sieve_pages()`), which the Ingest status line
#' reads as well, so the next step it names is one the gate allows: no
#' included item is assigned to a construct (each is missing, blank or
#' Unassigned, `sieve_construct_assigned()`).
#'
#' @param meta The item ledger.
#' @return `TRUE` when no included item has a construct, so those pages are
#'   blocked until one is assigned.
#' @author Ken Kelley
#' @noRd
sieve_constructs_needed <- function(meta) {
  active <- meta$status == "active"
  !any(active & sieve_construct_assigned(meta$construct))
}

#' Read page readiness and stored-result assurance without running analyses
#' @param state Shared application state.
#' @param page Navigation value.
#' @return Label, state, message, next step and recorded results.
#' @author Ken Kelley
#' @noRd
sieve_page_status <- function(state, page = "ingest") {
  catalog <- sieve_page_catalog()
  if (!length(page) || !page %in% names(catalog)) page <- "ingest"
  info <- catalog[[page]]
  out <- list(page = page, label = info$label, state = "ready", message = NULL,
    next_step = info$next_step, results = list())
  loaded <- !is.null(state$meta) && !is.null(state$items)
  if (!loaded && !isTRUE(sieve_pages()[[page]]$data_optional)) {
    out$state <- "blocked"
    out$message <- sieve_gate_text(state)
    return(out)
  }
  if (loaded && !isTRUE(sieve_confirmation_status(state)$confirmed) && !isTRUE(sieve_pages()[[page]]$data_optional)) {
    out$state <- "blocked"
    out$message <- sieve_gate_text(state)
    return(out)
  }
  if (page == "optimize" && !sieve_workflow_policy(state)$optimization) {
    out$state <- "blocked"
    out$message <- "Optimization requires empirical assignments. Deliberately change the construct basis on Ingest to use it."
    return(out)
  }
  if (loaded && isTRUE(sieve_pages()[[page]]$construct_model)) {
    if (sieve_constructs_needed(state$meta)) {
      out$state <- "blocked"
      out$message <- "Still needed: assign the included items to constructs on Ingest before fitting a construct model."
      return(out)
    }
  }
  records <- (state$page_results %||% list())[[page]] %||% list()
  revision <- sieve_page_revision(state)
  # A module that keeps its own currency is read as it published itself;
  # the others are checked against the inputs as a safety net.
  records <- lapply(records, function(r) {
    if (!isTRUE(r$own) && identical(r$state, "current") && !identical(r$revision, revision)) {
      r$state <- "stale"
      r$sentence <- "The data, item decisions, respondent rows or sampling design changed."
    }
    r
  })
  # Unrequested optional analyses are not a checklist of required runs.
  records <- Filter(function(r) !identical(r$state, "not_run"), records)
  out$results <- records
  phases <- vapply(records, function(r) r$state, character(1))
  # A page whose stale results its one Update action recomputes: the
  # guide's summary names that action;
  # the button itself is only the page's own.
  out$update <- page %in% sieve_pages_with_update() && any(phases == "stale")
  names_for <- function(phase) unique(vapply(records[phases %in% phase], function(r) r$title, character(1)))
  if (any(phases %in% c("stale", "failed"))) {
    out$state <- "stale"
    out$message <- paste0("Refresh before using: ", paste(names_for(c("stale", "failed")), collapse = "; "),
      if (page %in% sieve_pages_with_update()) {
        ". Earlier results are from before; Update recomputes them in turn."
      } else {
        ". Earlier results are historical; update only the analyses you need."
      })
  } else if (any(phases == "blocked")) {
    out$state <- "blocked"
    reasons <- unique(vapply(records[phases == "blocked"], function(r) r$sentence %||% r$title, character(1)))
    out$message <- paste0("Still needed: ", paste(reasons, collapse = "; "))
  } else if (any(phases == "running")) {
    out$state <- "running"
    out$message <- paste0("Running: ", paste(names_for("running"), collapse = "; "), ".")
  } else if (any(phases == "current")) {
    out$state <- "current"
    out$message <- paste0("Current inputs verified for: ", paste(names_for("current"), collapse = "; "),
      ". Returning to this page does not require another run.")
  } else {
    out$message <- if (page == "ingest" && isTRUE(sieve_confirmation_status(state)$confirmed)) {
      "Preparation confirmed. Continue to the analysis your question needs; there is no required tour of every page."
    } else if (page == "screen") {
      "Screening summaries follow the current inputs. Review flags and make the inclusion decisions your study requires."
    } else paste0("Next: ", info$next_step)
  }
  out
}

#' Consistent page guide with no computation controls of its own
#' @param status Output of sieve_page_status().
#' @param state Shared state, for project preparation information.
#' @return Compact accessible UI.
#' @author Ken Kelley
#' @noRd
sieve_page_status_ui <- function(status, state) {
  tone <- if (status$state %in% c("blocked", "stale")) "warning" else "info"
  shiny::tags$section(
    id = "sieve-page-guide", class = paste0("sieve-page-guide border-start border-", tone, " border-3 px-3 py-0"),
    role = "status", `aria-live` = "polite",
    shiny::tags$details(
      shiny::tags$summary(
        shiny::strong(paste0(status$label, ": what's needed here")),
        shiny::span(class = "ms-2 small", status$message)
        # The guide names the page's Update (status$update) and offers no
        # second one: the page's status line holds the one Update, with its
        # time estimate (a button in the guide beside it would give the page
        # two Updates).
      ),
      shiny::p(class = "small my-1", status$next_step),
      shiny::p(class = "small my-1", paste(
        "Pages can be visited in any order once their inputs are ready.",
        "Recoding, adding or removing items or respondents, or changing a model setting",
        "requires an updated result. Editing a display label does not require a new fit."
      )),
      shiny::p(class = "small my-1", sieve_run_policy_text()),
      if (status$page %in% c("ingest", "repro")) shiny::p(class = "small my-1", paste(
        "A saved project restores its prepared data and recorded transformations.",
        "Reverse scoring already applied there must not be applied a second time when reproducing its analysis."
      )),
      if (status$page == "efa") shiny::p(class = "small my-1", paste(
        "EFA factor numbers are engine labels. Match loading columns, factor order and signs",
        "before comparing with another program; apply the same mapping to both axes of Phi."
      )),
      if (length(status$results)) shiny::tags$ul(class = "small my-1",
        lapply(status$results, function(r) shiny::tags$li(
          shiny::strong(paste0(r$title, ": ")), r$state,
          if (length(r$sentence) && nzchar(r$sentence)) paste0(". ", r$sentence)
        )))
    )
  )
}

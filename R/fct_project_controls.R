# Project controls.

#' Describe a checked project consistently at either Open entry point
#'
#' With the five-option rule on (`min_options`), the preview says, before
#' the file is opened, which items typed ordered with five or more
#' response options will open as continuous because their types are
#' proposed, why, that the confirmation will be renewed, and that one
#' click keeps them ordered after opening; items whose ordered type the
#' researcher chose are named as kept.
#'
#' @param snapshot Checked project snapshot.
#' @param name Display filename.
#' @param min_options The five-option rule in force, or `NULL` when it is
#'   off.
#' @return UI tags.
#' @author Ken Kelley
#' @noRd
sieve_project_preview_ui <- function(snapshot, name, min_options = NULL) {
  about <- sieve_project_describe(snapshot)
  shown <- c(
    "Dataset" = "dataset", "Rows" = "rows", "Items" = "items", "Variable roles" = "roles",
    "Constructs" = "constructs", "Reverse scored items" = "reverse_scored",
    "Item types confirmed" = "confirmed", "Study's estimator setting" = "estimator_setting",
    "Transformation records" = "records", "Registered analyses" = "analyses",
    "Recorded steps" = "steps", "Saved" = "saved", "SIEVER version" = "siever",
    "Project schema" = "schema", "File" = "file_form"
  )
  types <- sieve_project_open_types(snapshot, min_options)
  shiny::tagList(
    shiny::p(class = "small mb-1", shiny::strong(sprintf("%s holds:", name))),
    shiny::tags$table(class = "table table-sm small mb-2", shiny::tags$tbody(
      lapply(names(shown), function(k) {
        shiny::tags$tr(shiny::tags$th(k), shiny::tags$td(about[[shown[[k]]]]))
      }),
      shiny::tags$tr(shiny::tags$th("Item types on opening"), shiny::tags$td(types))
    ))
  )
}

#' What the five-option rule will do to a project's types on opening
#'
#' @param snapshot Checked project snapshot.
#' @param min_options The rule in force, or `NULL` when it is off.
#' @return One or two sentences.
#' @author Ken Kelley
#' @noRd
sieve_project_open_types <- function(snapshot, min_options = NULL) {
  if (is.null(min_options)) {
    return("Kept as saved (the five-option checkbox in step 1 of the Ingest tab is off).")
  }
  rule <- sieve_option_rule(snapshot$items, snapshot$meta, min_options = min_options)
  n <- length(rule$switched)
  kept <- if (length(rule$kept)) {
    sprintf(
      " Kept ordered as your choice: %s.", sieve_name_some(rule$kept)
    )
  } else {
    ""
  }
  if (!n) {
    return(paste0("Kept as saved: no item with a proposed ordered type has five or more response options.", kept))
  }
  paste0(
    sprintf(
      paste(
        "%s typed ordered with five or more response options will open as continuous, because %s type%s",
        "proposed (detected or read from a specification) and the five-option checkbox in step 1 of the",
        "Ingest tab is on: with five or more categories, estimation that treats items as continuous",
        "performs comparably to the categorical estimator in many conditions, especially with roughly",
        "symmetric thresholds (Rhemtulla, Brosseau-Liard & Savalei, 2012).%s After opening, one click",
        "keeps them ordered."
      ),
      if (n == 1L) sprintf("1 item (%s)", rule$switched) else sprintf("%d items (%s)", n, sieve_name_some(rule$switched)),
      if (n == 1L) "its" else "their", if (n == 1L) " is" else "s are",
      if (isTRUE(snapshot$confirmed)) " The study's confirmation will be renewed with them as continuous." else ""
    ),
    kept
  )
}

#' Prepare a project download and record its exact revision
#' @param state Shared state.
#' @param file Destination supplied by the browser download handler.
#' @param name Researcher-selected project name.
#' @return Prepared-copy record invisibly.
#' @author Ken Kelley
#' @noRd
sieve_prepare_project_copy <- function(state, file, name = shiny::isolate(state$project_name)) {
  state$project_name <- name
  token <- sieve_project_fingerprint(state)
  sieve_project_write(sieve_project_snapshot(state), file)
  control <- shiny::isolate(state$project_control) %||% list()
  control$prepared <- list(
    key = token, name = sieve_project_filename(name),
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
  state$project_control <- control
  invisible(control$prepared)
}

#' Acknowledge only the prepared copy, never later changes
#' @param state Shared state.
#' @return Whether a prepared copy exists.
#' @author Ken Kelley
#' @noRd
sieve_acknowledge_project_copy <- function(state) {
  control <- shiny::isolate(state$project_control) %||% list()
  if (is.null(control$prepared)) return(FALSE)
  control$acknowledged <- control$prepared$key
  state$project_control <- control
  TRUE
}

#' Bind the Shared Open Project Dialog
#' @param input,output,session Shiny module objects.
#' @param state Shared state.
#' @param ids Input and output IDs for the existing entry point.
#' @param min_options Function resolving the five-option rule.
#' @return The dialog, preview reader, and pending file.
#' @author Ken Kelley
#' @noRd
sieve_project_open_wire <- function(input, output, session, state,
                                    ids = list(open = "open", file = "file", preview = "open_preview", confirm = "restore"),
                                    min_options = function() if (isTRUE(shiny::isolate(state$five_continuous) %||% TRUE)) 5L else NULL) {
  pending <- shiny::reactiveVal(NULL)
  preview <- function(path, name) {
    read <- sieve_capture(sieve_project_read(path))
    pending(if (read$ok) list(name = name, snapshot = read$value) else list(name = name, problem = read$error))
    invisible(read)
  }
  dialog <- function(preview = NULL) {
    pending(NULL)
    shiny::showModal(shiny::modalDialog(title = "Open Project", easyClose = TRUE,
      shiny::p("Review this saved project before opening it. Opening replaces the current study and its session script. Save a copy with the project bar first if needed."),
      shiny::fileInput(session$ns(ids$file), "SIEVER Project", accept = c(".rds", ".siever")),
      shiny::uiOutput(session$ns(ids$preview)), footer = shiny::modalButton("Cancel")))
    if (!is.null(preview)) read_preview(preview$path, preview$name)
  }
  read_preview <- preview
  shiny::observeEvent(input[[ids$open]], dialog())
  shiny::observeEvent(input[[ids$file]], {
    file <- input[[ids$file]]
    shiny::req(file$datapath)
    read_preview(file$datapath, file$name)
  })
  output[[ids$preview]] <- shiny::renderUI({
    held <- pending()
    if (is.null(held)) return(NULL)
    if (!is.null(held$problem)) return(shiny::div(class = "alert alert-danger small",
      sprintf("%s was not opened, and the session is unchanged. %s", held$name, held$problem)))
    shiny::tagList(sieve_project_preview_ui(held$snapshot, held$name, min_options = min_options()),
      shiny::actionButton(session$ns(ids$confirm), "Open This Project", class = "btn-primary"))
  })
  shiny::observeEvent(input[[ids$confirm]], {
    held <- pending()
    shiny::req(held$snapshot)
    read <- sieve_capture(sieve_project_open(state, held$snapshot, source = held$name, min_options = min_options()))
    if (!read$ok) {
      pending(list(name = held$name, problem = read$error))
      sieve_notify(read$error, type = "error", duration = NULL)
      return()
    }
    if (identical(state$project_name, "Untitled project")) state$project_name <- sub("[.]siever[.]rds$", "", held$name, ignore.case = TRUE)
    state$project_control <- list(acknowledged = read$value$acknowledged)
    pending(NULL)
    shiny::removeModal()
    switched <- (shiny::isolate(state$project) %||% list())$switched_types
    sieve_notify(paste(c("Project opened. Analyses run only when requested.", switched$text), collapse = " "),
      type = if (is.null(switched)) "message" else "warning")
  })
  list(dialog = dialog, preview = preview, pending = pending)
}

#' A filesystem-safe project download name
#' @param name Project name supplied by the researcher.
#' @return A file name, without a directory.
#' @author Ken Kelley
#' @noRd
sieve_project_filename <- function(name) {
  name <- trimws(as.character(name)[1L])
  if (is.na(name) || !nzchar(name)) name <- "Untitled project"
  name <- sub("[.]siever[.]rds$", "", name, ignore.case = TRUE)
  name <- gsub("[[:cntrl:]/\\\\:*?\"<>|]", "_", name)
  paste0(name, ".siever.rds")
}

#' Identity of the saved project content, without session save acknowledgments
#' @param state Shared state.
#' @return Content hash.
#' @author Ken Kelley
#' @noRd
sieve_project_fingerprint <- function(state) {
  x <- shiny::isolate(shiny::reactiveValuesToList(state))
  log <- x$codelog %||% character(0)
  starts <- which(grepl("^# \\[[0-9]+\\] ", log))
  if (length(starts)) {
    ends <- c(starts[-1L] - 1L, length(log))
    keep <- rep(TRUE, length(log))
    for (j in seq_along(starts)) {
      number <- sub("^# \\[([0-9]+)\\].*", "\\1", log[starts[j]])
      if (isTRUE(x$replay_operations[[number]]$passive)) keep[seq.int(starts[j], ends[j])] <- FALSE
    }
    log <- log[keep]
    at <- which(grepl("^# \\[[0-9]+\\] ", log))
    log[at] <- sub("^# \\[[0-9]+\\] ", "# [action] ", log[at])
  }
  x$codelog <- log
  # Empty settings and their displayed defaults have the same scientific
  # meaning. Opening a control panel may materialize defaults, which is
  # presentation initialization rather than a researcher change.
  defaults <- list(
    efa_settings = list(
      n_factors = x$theory$k %||% 3L, cor_type = "auto", fm = "ml",
      rotation = "oblimin", estimator = "study", fit_scope = "k", fit_from = 1L,
      fit_to = min(8L, sieve_efa_max_factors(sum(x$meta$status == "active"))),
      suppress = .30, sort_loadings = TRUE
    ),
    cfa_settings = list(
      scope = "construct", structure = "first", ident = "varone",
      facets = character(0), all_facets = TRUE
    ),
    sem_settings = list(outcome_types = list(), exogenous_missing = "engine_default"),
    reliability_settings = list(
      scope = "construct", treatment = "study", missing = "study",
      denominator = "observed", ci_method = "recommended", conf_level = .95, B = 10000, show_alpha = FALSE, small_sample_t = FALSE
    )
  )
  normalize <- function(value) {
    if (is.list(value)) return(lapply(value, normalize))
    if (is.numeric(value)) return(as.double(value))
    value
  }
  for (field in names(defaults)) {
    merged <- utils::modifyList(defaults[[field]], x[[field]] %||% list())
    x[[field]] <- normalize(merged)
  }
  x[sieve_state_exclusions("fingerprint")] <- NULL
  sieve_hash(sieve_keep_plain(x))
}

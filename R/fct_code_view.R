# Small, read-only disclosures beside scientific results. A disclosure reads
# the recorded computation; opening it, copying its code or downloading it
# never evaluates the displayed code and never starts an analysis.

#' Keep the code and input identity of a completed, recorded calculation
#'
#' The record carries the code of one recorded step, the input revision it
#' was computed from (its key, respondent rows and item columns), the
#' analytic ledger and revision of that moment (so a disclosure can say what
#' changed since), and the versions and namespace paths executing now.
#'
#' @param state Shared application state, after the calculation was logged.
#' @param context Plain text describing the settings used for that result.
#' @param step Recorded step number; the latest step is used when absent.
#' @param packages Further packages whose versions the calculation ran in
#'   (the Network page names psychonetrics, qgraph and igraph), recorded
#'   beside SIEVER, DMAR, lavaan and psych.
#' @return A plain list, or NULL when no recorded step is available.
#' @author Ken Kelley
#' @noRd
sieve_code_snapshot <- function(state, context = character(), step = NULL, packages = NULL) {
  extra <- unique(as.character(packages %||% character(0)))
  sieve_code_remember_state(state)
  shiny::isolate({
    records <- sieve_script_records(state)
    if (!length(records)) return(NULL)
    numbers <- names(records)
    at <- if (is.null(step)) length(records) else match(as.character(step), numbers)
    if (is.na(at)) return(NULL)
    record <- records[[at]]
    code <- c(sprintf("# [%s] %s", record$number, record$note), record$code)
    while (length(code) && !nzchar(tail(code, 1L))) code <- head(code, -1L)
    input_key <- state$replay_steps[[numbers[[at]]]]
    revision <- state$replay_states[[input_key %||% ""]]
    # Provenance belongs to the recorded step, never the R session that
    # happens to open its disclosure later. A step recorded without its
    # engine identity reads "not recorded".
    packages <- unique(c("SIEVER", "DMAR", "lavaan", "psych", extra))
    operation <- state$replay_operations[[numbers[[at]]]] %||% list()
    identity <- operation$engine_identity %||% list()
    versions <- stats::setNames(vapply(packages, function(package) {
      engine <- identity[[package]]
      engine$version %||% if (is.null(engine)) "not recorded" else engine$status
    }, character(1)), packages)
    namespace_paths <- stats::setNames(vapply(packages, function(package) {
      identity[[package]]$namespace_path %||% NA_character_
    }, character(1)), packages)
    versions <- c(R = operation$r_version %||% "not recorded", versions)
    list(
      code = code, context = as.character(context), step = numbers[[at]],
      note = record$note,
      input_key = input_key, input_rows = length(revision$rows),
      item_names = revision$item_names, versions = versions,
      namespace_paths = namespace_paths, engine_identity = identity,
      initial_session_versions = state$replay_versions,
      # The analytic ledger and the revision the step was recorded under:
      # what a later disclosure compares with the present to say what
      # changed (sieve_code_status()).
      ledger = if (!is.null(revision$meta)) sieve_ledger_analytic(revision$meta),
      revision = revision$revision,
      recorded_at = operation$recorded_at %||% "not recorded"
    )
  })
}

#' The latest recorded step whose note starts with a prefix
#'
#' @param state Shared application state.
#' @param prefix The start of the step's note (matched as plain text), or a
#'   regular expression when `regex = TRUE`.
#' @param regex Whether `prefix` is a regular expression.
#' @return The step number as a string, or NULL when no step matches.
#' @author Ken Kelley
#' @noRd
sieve_code_step <- function(state, prefix, regex = FALSE) {
  records <- sieve_script_records(state)
  if (!length(records) || !length(prefix) || !nzchar(prefix)) return(NULL)
  notes <- vapply(records, `[[`, "", "note")
  hit <- if (isTRUE(regex)) grepl(prefix, notes) else startsWith(notes, prefix)
  if (!any(hit)) return(NULL)
  names(records)[max(which(hit))]
}

#' The record of the latest recorded step whose note starts with a prefix
#'
#' For a result whose module writes its step to the reproducible code
#' without keeping a snapshot with the result: the disclosure reads the
#' step back from the record, with the input revision and the ledger of
#' that moment.
#'
#' @inheritParams sieve_code_step
#' @param context Plain text describing the settings of the result.
#' @return A record as `sieve_code_snapshot()` makes, or NULL.
#' @author Ken Kelley
#' @noRd
sieve_code_record <- function(state, prefix, context = character(), regex = FALSE) {
  step <- sieve_code_step(state, prefix, regex = regex)
  if (is.null(step)) return(NULL)
  sieve_code_snapshot(state, context = context, step = step)
}

#' The state of a disclosure against the present inputs
#'
#' A module that keeps its own currency passes it as `status` (a demand
#' status, or a list with `state` and `sentence`): a stale, failed or
#' not-run state is reported as it is. Otherwise the record's revision is
#' compared with the present one, and a difference is named item by item
#' (`sieve_stale_sentence()`), as a historical result.
#'
#' @param record A record from `sieve_code_snapshot()`, or NULL.
#' @param state Shared application state (read under `isolate()`).
#' @param status The module's own status for the result, if it keeps one.
#' @return A list with `state` (`not_run`, `current`, `stale` or `failed`)
#'   and, for a stale result, `sentence`.
#' @author Ken Kelley
#' @noRd
sieve_code_status <- function(record, state, status = NULL) {
  if (is.character(status)) status <- list(state = status)
  phase <- status$state %||% NULL
  if (identical(phase, "failed")) {
    return(list(state = "failed", sentence = status$sentence %||% status$error))
  }
  if (identical(phase, "stale")) {
    return(list(state = "stale", sentence = status$sentence %||% status$error))
  }
  if (identical(phase, "not_run") || is.null(record)) {
    return(list(state = "not_run"))
  }
  now <- shiny::isolate(sieve_revision(state))
  then <- record$revision
  if (is.null(then) || identical(then, now)) {
    return(list(state = "current"))
  }
  ledger_now <- shiny::isolate(sieve_ledger_analytic(state$meta))
  sentence <- sieve_stale_sentence(
    record$ledger, ledger_now,
    dataset_then = then$dataset_id, dataset_now = now$dataset_id,
    data_then = then$data_key, data_now = now$data_key
  )
  if (is.null(sentence) && !identical(then$rows_key, now$rows_key)) {
    sentence <- "the respondents in the analysis changed"
  }
  list(
    state = "stale",
    sentence = if (!is.null(sentence)) sprintf("What changed: %s.", sentence)
  )
}

#' A disclosure for a result recorded under a note prefix
#'
#' Reads the reproducible code for the latest step whose note starts with
#' `prefix` and shows it with its state against the present inputs. Meant
#' for a `renderUI()`: it reads the recorded code, the ledger, the item
#' data, the respondents and the dataset identity, so the panel follows
#' every change of them; it evaluates no code and starts no analysis.
#'
#' @inheritParams sieve_code_record
#' @inheritParams sieve_code_status
#' @inheritParams sieve_code_view
#' @return An escaped HTML tag.
#' @author Ken Kelley
#' @noRd
sieve_code_disclosure <- function(state, prefix, status = NULL, ns = NULL, title = "Show code",
                                  readout = character(), context = character(), regex = FALSE) {
  # The dependencies of the panel: a new recorded step, and every part of
  # the revision the record is compared with.
  state$codelog
  state$meta
  state$items
  state$case_index
  state$dataset_id
  record <- sieve_code_record(state, prefix, context = context, regex = regex)
  sieve_code_view(record, sieve_code_status(record, state, status), title = title, readout = readout, ns = ns)
}

#' A closed disclosure of the code that produced a displayed result
#'
#' The actual call first (the recorded code), then the expressions that
#' read the fitted result for the display, then the actions (copy the
#' recorded code to the clipboard; download this analysis as a small
#' bundle), and, closed, the recorded inputs and provenance: step, time,
#' input revision, rows, item columns, versions and namespace paths.
#'
#' @param record A saved list with code and optional context, step,
#'   input_key, input_rows, item_names and versions. A code character
#'   vector also works.
#' @param status A status string, or a status list with state and sentence.
#' @param title Disclosure label.
#' @param readout Additional expressions that read the already fitted result.
#' @param ns Optional module namespace for the download action and the
#'   links wired by `sieve_code_navigation()`.
#' @return An escaped HTML tag; no code is evaluated.
#' @author Ken Kelley
#' @noRd
sieve_code_view <- function(record = NULL, status = "not_run", title = "Show code",
                            readout = character(), ns = NULL) {
  if (is.character(record)) record <- list(code = record)
  if (is.character(status)) status <- list(state = status)
  phase <- status$state %||% "not_run"
  available <- is.list(record) && is.character(record$code) && length(record$code) > 0L
  message <- if (!available) {
    if (identical(phase, "failed")) {
      "This calculation did not finish. No code for a successful result is available."
    } else if (identical(phase, "not_run")) {
      "Run this analysis to see the code and input settings for its result."
    } else {
      "This earlier result has no saved code disclosure. Run the analysis again to record one."
    }
  } else if (identical(phase, "stale")) {
    paste0("This is the code for the earlier result on screen. The inputs or analysis settings have changed. ",
      status$sentence %||% "")
  } else if (identical(phase, "failed")) {
    "The latest request failed. The code below belongs to the earlier result on screen."
  } else {
    "This code and these settings belong to the result on screen. Opening this panel does not run an analysis."
  }
  context <- if (available) record$context %||% character() else character()
  provenance <- if (available) c(
    if (length(record$step)) paste("Recorded session step:", record$step),
    if (length(record$recorded_at)) paste("Recorded:", record$recorded_at),
    if (length(record$input_key)) paste("Input revision:", record$input_key),
    if (length(record$input_rows)) paste("Input respondent rows:", record$input_rows),
    if (length(record$item_names)) paste("Item columns in the saved input view:", paste(record$item_names, collapse = ", ")),
    if (length(record$versions)) paste("Execution versions:", paste(names(record$versions), record$versions, collapse = "; ")),
    if (length(record$namespace_paths)) paste("Execution namespace paths:",
      paste(names(record$namespace_paths)[!is.na(record$namespace_paths)],
        record$namespace_paths[!is.na(record$namespace_paths)], collapse = "; "))
  ) else character()
  navigation_link <- function(target, label) {
    # Several cards in one module share these actions. A DOM id would be
    # duplicated; send the namespaced event directly from each native link.
    shiny::tags$a(href = "#", onclick = sprintf(
      'Shiny.setInputValue(%s, Date.now(), {priority: "event"}); return false;',
      encodeString(ns(paste0("code_open_", target)), quote = '"')
    ), label)
  }
  # The record a download of this panel is made from: kept in the session
  # under its step, so the download carries the panel's settings as well
  # as the recorded code (sieve_code_navigation()).
  if (available && is.function(ns) && length(record$step)) sieve_code_register(ns, record)
  actions <- if (available) {
    shiny::div(
      class = "d-flex align-items-center gap-2 flex-wrap mt-2 mb-1 sieve-code-actions",
      shiny::tags$button(
        type = "button", class = "btn btn-outline-secondary btn-sm sieve-code-copy",
        "Copy recorded code"
      ),
      if (is.function(ns) && length(record$step)) shiny::tags$button(
        type = "button", class = "btn btn-outline-secondary btn-sm sieve-code-download",
        `data-sieve-input` = ns("code_download"), `data-sieve-step` = as.character(record$step),
        "Download this analysis"
      ),
      shiny::tags$span(class = "small text-muted sieve-code-copy-note", role = "status", `aria-live` = "polite")
    )
  }
  shiny::tags$details(
    class = "siever-code-view small mt-2 mb-2",
    shiny::tags$summary(title),
    shiny::tags$p(class = "mt-2 mb-1", message),
    if (length(context)) shiny::tags$ul(lapply(context, shiny::tags$li)),
    if (available) shiny::tags$pre(
      class = "sieve-code-source", style = "white-space: pre; overflow-x: auto; max-height: 32rem;",
      shiny::tags$code(paste(record$code, collapse = "\n"))
    ),
    if (available && length(readout)) shiny::tagList(
      shiny::tags$p(class = "mb-1", "Reading the fitted result for this display:"),
      shiny::tags$pre(style = "white-space: pre; overflow-x: auto;", shiny::tags$code(paste(readout, collapse = "\n")))
    ),
    actions,
    if (length(provenance)) shiny::tags$details(
      class = "siever-code-provenance mt-1 mb-2",
      shiny::tags$summary("Recorded inputs and provenance"),
      shiny::tags$ul(class = "text-muted mt-1 mb-0", lapply(provenance, shiny::tags$li))
    ),
    shiny::tags$p(class = "mb-1",
      "The data objects in this code use the recorded input view, after the item and respondent decisions and any reversals. ",
      "Use the complete session script and its verified input file to reproduce that preparation; copying this panel alone does not load the data."
    ),
    if (is.function(ns)) shiny::tags$p(class = "mb-0",
      navigation_link("ingest", "Review data and item preparation"), " \u00b7 ",
      navigation_link("repro", "Open the complete session script")
    ) else shiny::tags$p(class = "mb-0", sprintf(
      "Review data preparation on Ingest; the full script and input download are under %s.",
      sieve_page_path_text("repro")
    ))
  )
}

#' Keep the shared state where a disclosure download can find it
#'
#' A download is served by the module whose panel offered it, through
#' `sieve_code_navigation()`, which some modules call without the state.
#' The state every module shares is kept once in the session's user data
#' when a record is made, so a download of any panel can read the input
#' revision it names.
#'
#' @param state Shared application state.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_code_remember_state <- function(state) {
  domain <- shiny::getDefaultReactiveDomain()
  if (is.null(domain)) return(invisible(NULL))
  user <- tryCatch(domain$userData, error = function(e) NULL)
  if (is.environment(user)) user$sieve_state <- state
  invisible(NULL)
}

#' The shared state kept for disclosure downloads, if any
#' @param session A module session.
#' @return The state, or NULL.
#' @author Ken Kelley
#' @noRd
sieve_code_state <- function(session) {
  user <- tryCatch(session$userData, error = function(e) NULL)
  if (is.environment(user)) user$sieve_state else NULL
}

#' Keep the record a panel shows, under its namespaced step
#' @param ns The module namespace function.
#' @param record The record shown.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_code_register <- function(ns, record) {
  domain <- shiny::getDefaultReactiveDomain()
  if (is.null(domain)) return(invisible(NULL))
  user <- tryCatch(domain$userData, error = function(e) NULL)
  if (!is.environment(user)) return(invisible(NULL))
  records <- user$sieve_code_records %||% list()
  records[[ns(paste0("code_step_", record$step))]] <- record
  user$sieve_code_records <- records
  invisible(NULL)
}

#' The record a panel registered for its step, if any
#' @param session A module session.
#' @param step The step number as a string.
#' @return The record, or NULL.
#' @author Ken Kelley
#' @noRd
sieve_code_registered <- function(session, step) {
  user <- tryCatch(session$userData, error = function(e) NULL)
  if (!is.environment(user)) return(NULL)
  (user$sieve_code_records %||% list())[[session$ns(paste0("code_step_", step))]]
}

#' Does the session's sharing choice release this?
#'
#' The recorded input view (the item columns after the item and respondent
#' decisions and any reversals) is research data. It leaves the session in
#' an analysis download only when the session's sharing choice says so
#' (`state$sharing$input_view`, set with `sieve_sharing_set()`); until
#' then it is withheld and the download names the revision instead.
#'
#' @param state Shared application state, or NULL.
#' @param what The item of the sharing choice (`"input_view"`).
#' @return TRUE or FALSE.
#' @author Ken Kelley
#' @noRd
sieve_sharing_allows <- function(state, what = "input_view") {
  if (is.null(state)) return(FALSE)
  choice <- shiny::isolate(state$sharing)
  isTRUE(choice[[what]])
}

#' Record the session's sharing choice
#' @param state Shared application state.
#' @param input_view Whether an analysis download may carry the recorded
#'   input view.
#' @return The choice, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_sharing_set <- function(state, input_view = FALSE) {
  choice <- shiny::isolate(state$sharing) %||% list()
  choice$input_view <- isTRUE(input_view)
  state$sharing <- choice
  invisible(choice)
}

#' The recorded input view a record names
#'
#' @param state Shared application state.
#' @param record A record with `input_key`.
#' @return The item data frame of that revision, with plain columns, or
#'   NULL when the revision is not held.
#' @author Ken Kelley
#' @noRd
sieve_code_input_view <- function(state, record) {
  key <- record$input_key
  if (is.null(state) || is.null(key)) return(NULL)
  shiny::isolate({
    held <- state$replay_states[[key]]
    if (is.null(held)) return(NULL)
    x <- tryCatch(sieve_replay_expand(held, state$replay_data %||% list()), error = function(e) NULL)
    if (is.null(x) || !is.data.frame(x$items)) return(NULL)
    items <- x$items
    items[] <- lapply(items, sieve_plain_values)
    items
  })
}

#' The README of an analysis download
#'
#' @param record A record from `sieve_code_snapshot()`.
#' @param shared Whether the recorded input view is in the bundle.
#' @param withheld_why One sentence saying why the input view is absent
#'   when it is.
#' @return Character lines.
#' @author Ken Kelley
#' @noRd
sieve_code_readme <- function(record, shared = FALSE, withheld_why = NULL) {
  line <- function(label, value) if (length(value) && any(nzchar(value))) paste0(label, ": ", paste(value, collapse = ", "))
  versions <- if (length(record$versions)) paste(names(record$versions), record$versions, collapse = "; ")
  paths <- record$namespace_paths
  paths <- if (length(paths)) paste(names(paths)[!is.na(paths)], paths[!is.na(paths)], collapse = "; ")
  c(
    "SIEVER analysis download",
    "========================",
    "",
    line("Analysis", record$note),
    line("Recorded session step", record$step),
    line("Recorded", record$recorded_at),
    if (length(record$context)) c("Settings:", paste0("  - ", record$context)),
    line("Input revision", record$input_key),
    line("Input respondent rows", record$input_rows),
    line("Item columns in the recorded input view", record$item_names),
    line("Execution versions", versions),
    line("Execution namespace paths", paths),
    line("Immutable engine build IDs (SHA-256)",
      if (length(record$engine_identity)) paste(names(record$engine_identity), sieve_engine_keys(record$engine_identity), collapse = "; ")),
    "",
    "Files:",
    "  recorded_code.R: the code recorded for this result, exactly as the panel shows it.",
    if (shared) {
      "  input_view.csv: the recorded input view (the item columns after the item and respondent decisions and any reversals), as `items` in the code."
    },
    "",
    if (shared) {
      "Input view: included, under the session's sharing choice. It is research data; distribute it only under the study's sharing arrangements."
    } else {
      c(
        "Input view: withheld.",
        withheld_why %||% paste0(
          "The session's sharing choice does not release the recorded input view with an analysis download. ",
          "Revision ", record$input_key %||% "(not recorded)", " identifies it in the saved project and in the session input file ",
          "(Reproducible Code), which the complete session script verifies before it reads them."
        )
      )
    },
    "",
    "The code reads `items` (the item columns of the recorded input view) and, where it appears, `d` (the data as loaded).",
    "The complete session script, under Reproducible Code, rebuilds both from the verified input file; this bundle alone does not."
  )
}

#' Write an analysis download: the recorded code, a README and, when
#' shared, the recorded input view
#'
#' @param record A record from `sieve_code_snapshot()`.
#' @param file The `.tar.gz` file to write (no default: the caller names it).
#' @param input The recorded input view (a data frame), or NULL.
#' @param shared Whether the sharing choice releases the input view.
#' @return The files written, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_code_bundle_write <- function(record, file, input = NULL, shared = FALSE) {
  dir <- tempfile("siever_analysis_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  has_input <- isTRUE(shared) && is.data.frame(input)
  writeLines(record$code %||% character(), file.path(dir, "recorded_code.R"))
  withheld_why <- if (isTRUE(shared) && !has_input) {
    paste0(
      "The sharing choice releases the recorded input view, but revision ", record$input_key %||% "(not recorded)",
      " is not held in this session (a project opened without its replay store); the saved project and the session input file carry it."
    )
  }
  writeLines(sieve_code_readme(record, shared = has_input, withheld_why = withheld_why), file.path(dir, "README.txt"))
  if (has_input) utils::write.csv(input, file.path(dir, "input_view.csv"), row.names = FALSE)
  files <- list.files(dir)
  withr::with_dir(dir, utils::tar(file, files = files, compression = "gzip", tar = "internal"))
  invisible(files)
}

#' Register the download of one panel's analysis
#'
#' Through the session's own download registry (what `downloadHandler()`
#' uses), under a name that carries the step, so several panels of one
#' module can each offer their own download without a DOM id each.
#'
#' @param session A module session.
#' @param record The record to download.
#' @param state Shared application state, or NULL (the input view is then
#'   withheld).
#' @return The download URL as the session reports it (NULL in a session
#'   that reports none), invisibly.
#' @author Ken Kelley
#' @noRd
sieve_code_download_register <- function(session, record, state = NULL) {
  step <- as.character(record$step %||% "0")
  name <- session$ns(sprintf("code_bundle_%s", gsub("[^0-9A-Za-z]", "_", step)))
  shared <- sieve_sharing_allows(state, "input_view")
  filename <- sprintf("siever_analysis_step_%s.tar.gz", gsub("[^0-9A-Za-z]", "_", step))
  content <- function(file) {
    sieve_code_bundle_write(record, file,
      input = if (shared) sieve_code_input_view(state, record), shared = shared
    )
  }
  url <- session$registerDownload(name, filename, "application/gzip", content)
  invisible(if (is.character(url) && length(url) == 1L) url else NULL)
}

#' Start the download in the browser
#' @param session A module session.
#' @param url The download URL.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_code_download_send <- function(session, url) {
  send <- tryCatch(session$sendCustomMessage, error = function(e) NULL)
  if (is.function(send) && is.character(url)) send("sieve-code-download", list(href = url))
  invisible(NULL)
}

#' Wire disclosure links and downloads to the study's preparation, the
#' session script and the analysis download
#'
#' @param input,session Module input and session.
#' @param state Shared application state; when absent, the state kept in
#'   the session's user data by `sieve_code_snapshot()` is read.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_code_navigation <- function(input, session, state = NULL) {
  shiny::observeEvent(input$code_open_ingest, {
    sieve_go(session, "ingest")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$code_open_repro, {
    sieve_go(session, "repro")
  }, ignoreInit = TRUE)
  # Download this analysis: the panel names its recorded step; the record
  # it showed is read back (never recomputed) and served as a bundle.
  shiny::observeEvent(input$code_download, {
    request <- input$code_download
    step <- as.character(request$step %||% "")
    st <- state %||% sieve_code_state(session)
    record <- sieve_code_registered(session, step)
    if (is.null(record) && !is.null(st) && nzchar(step)) record <- sieve_code_snapshot(st, step = step)
    if (is.null(record)) {
      sieve_notify("No recorded code is available for this panel to download.", type = "warning")
      return(invisible(NULL))
    }
    url <- sieve_code_download_register(session, record, st)
    sieve_code_download_send(session, url)
  }, ignoreInit = TRUE)
  invisible(NULL)
}

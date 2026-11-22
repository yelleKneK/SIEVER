# The results-first contract on the dense first screens: a status line in
# place of the Ingest badge, one method headline with its provenance in a
# labeled disclosure, and the notification policy (one path, short
# messages, no stacking, warnings in the status line, an accessible
# panel).

sn_strip <- function(html) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(html), collapse = " ")))

sn_root <- function() {
  root <- file.path(testthat::test_path(), "..", "..")
  if (dir.exists(file.path(root, "R"))) root else NULL
}

# ---- One path for every notification -----------------------------------------

test_that("no file in R/ calls showNotification() except sieve_notify(), the one path", {
  root <- sn_root()
  skip_if(is.null(root), "the R source tree is not beside the tests")
  files <- list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)
  hits <- unlist(lapply(files, function(f) {
    code <- sub("#.*$", "", readLines(f, warn = FALSE))
    at <- grep("showNotification(", code, fixed = TRUE)
    if (length(at)) paste0(basename(f), ":", at) else character(0)
  }))
  # The one call is inside sieve_notify() in fct_ondemand.R.
  expect_length(hits, 1L)
  expect_match(hits, "^fct_ondemand[.]R:")
  body <- paste(deparse(sieve_notify), collapse = "\n")
  expect_match(body, "shiny::showNotification(", fixed = TRUE)
})

test_that("a message lasts at most five seconds, a warning or an error stays until dismissed, and an id, given or taken from the type and text, replaces rather than stacks", {
  seen <- new.env()
  seen$calls <- list()
  local_mocked_bindings(showNotification = function(ui, action = NULL, duration = 5, closeButton = TRUE,
                                                    id = NULL, type = "default", session = NULL) {
    seen$calls[[length(seen$calls) + 1L]] <- list(ui = ui, duration = duration, id = id, type = type)
    invisible(id)
  }, .package = "shiny")
  session <- shiny::MockShinySession$new()
  shiny::withReactiveDomain(session, {
    sieve_notify("Done.")
    sieve_notify("Done, long.", duration = 12)
    sieve_notify("Kept until dismissed.", duration = NULL)
    sieve_notify("Check this.", type = "warning")
    sieve_notify("Stopped.", type = "error")
    sieve_notify("A warning for a while.", type = "warning", duration = 9)
    sieve_notify("Loaded.", id = "ingest-load_notification")
    sieve_notify("Stopped, for a while.", type = "error", duration = 12)
  })
  d <- lapply(seen$calls, `[[`, "duration")
  expect_identical(d[[1]], 5)
  expect_identical(d[[2]], 5)
  expect_null(d[[3]])
  expect_null(d[[4]])
  expect_null(d[[5]])
  # A duration given for a warning or an error is not applied: it stays
  # until dismissed, as the user guide says, rather than vanishing after the
  # 9 seconds asked for.
  expect_null(d[[6]])
  expect_null(d[[8]])
  expect_identical(seen$calls[[7]]$id, "ingest-load_notification")
  expect_null(seen$calls[[1]]$id)
  expect_null(seen$calls[[2]]$id)
  # A notification that stays until dismissed and is given no id takes one
  # from its type and text, so the same one repeated replaces itself: with
  # an id of its own for each repeat, a refusal raised three times would
  # leave three warnings to dismiss.
  for (i in 3:6) expect_match(seen$calls[[i]]$id, "^sieve-note-[0-9a-f]{32}$")
  expect_match(seen$calls[[8]]$id, "^sieve-note-[0-9a-f]{32}$")
  n <- length(seen$calls)
  shiny::withReactiveDomain(session, {
    sieve_notify("Check this.", type = "warning")
    sieve_notify("Check this.", type = "error")
    sieve_notify(htmltools::tags$span("Check this."), type = "warning")
    sieve_notify("Pick at least one item to exclude.", type = "warning")
    sieve_notify("Pick at least one item to exclude.", type = "warning")
  })
  again <- vapply(seen$calls[n + 1:5], `[[`, "", "id")
  expect_identical(again[1], seen$calls[[4]]$id)
  expect_identical(again[4], again[5])
  # Another type, other text or another tag is another notification.
  expect_false(again[2] %in% c(seen$calls[[4]]$id, seen$calls[[5]]$id))
  expect_false(identical(again[3], again[1]))
  expect_false(identical(seen$calls[[4]]$id, seen$calls[[5]]$id))
  expect_false(identical(seen$calls[[4]]$id, seen$calls[[6]]$id))
  # An id given is kept.
  shiny::withReactiveDomain(session, sieve_notify("Check this.", type = "warning", id = "given"))
  expect_identical(seen$calls[[length(seen$calls)]]$id, "given")
  # Outside a session nothing is shown, and nothing fails.
  n <- length(seen$calls)
  expect_null(sieve_notify("No session."))
  expect_length(seen$calls, n)
  expect_error(sieve_notify("x", type = "info"))
})

test_that("no call in R/ gives a warning or an error notification a duration", {
  # sieve_notify() ignores a duration given for a warning or an error, and
  # the call sites say what happens: none passes one.
  root <- sn_root()
  skip_if(is.null(root), "the R source tree is not beside the tests")
  files <- list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)
  hits <- character(0)
  for (f in files) {
    pd <- utils::getParseData(parse(f, keep.source = TRUE))
    calls <- pd$parent[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "sieve_notify"]
    for (i in calls) {
      call_id <- pd$parent[pd$id == i]
      e <- str2lang(utils::getParseText(pd, call_id))
      args <- as.list(e)[-1]
      nm <- names(args) %||% rep("", length(args))
      type <- if ("type" %in% nm) paste(deparse(args[["type"]]), collapse = " ") else "\"message\""
      if (!"duration" %in% nm || is.null(args[["duration"]])) next
      if (grepl("warning|error", type)) hits <- c(hits, paste0(basename(f), ":", pd$line1[pd$id == call_id]))
    }
  }
  expect_identical(hits, character(0))
})

test_that("the notification panel is narrower, bounded and scrolls, and its notifications are announced and dismissible from the keyboard", {
  css <- paste(readLines(system.file("app", "www", "custom.css", package = "SIEVER"), warn = FALSE), collapse = "\n")
  expect_match(css, "#shiny-notification-panel#shiny-notification-panel { width: min(400px, calc(100vw - 2rem)); max-height: 40vh; overflow-y: auto; }", fixed = TRUE)
  expect_match(css, ".shiny-notification-close:focus-visible", fixed = TRUE)
  js <- paste(readLines(system.file("app", "www", "sieve.js", package = "SIEVER"), warn = FALSE), collapse = "\n")
  for (part in c(
    "function sieveNotificationAccess(", "panel.setAttribute(\"role\", \"status\")", "\"aria-live\"",
    "close.setAttribute(\"tabindex\", \"0\")", "close.setAttribute(\"role\", \"button\")",
    "close.setAttribute(\"aria-label\", \"Dismiss\")", "event.key === \"Escape\"", "event.key === \"Enter\""
  )) {
    expect_match(js, part, fixed = TRUE, info = part)
  }
})

# ---- The Ingest status line -----------------------------------------------------

test_that("the Ingest status line is a sentence with role status, within its budget, and a bundled Load raises no notification", {
  fx <- sieve_hs9_fixture()
  skip_if(is.null(fx), "DMAR data not available")
  shown <- new.env()
  shown$notes <- list()
  local_mocked_bindings(sieve_notify = function(message, ...) {
    shown$notes[[length(shown$notes) + 1L]] <- list(message = message, ...)
    invisible(NULL)
  })
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    html <- as.character(output$status_chip$html)
    expect_match(html, "class=\"sieve-status-line sieve-status-neutral\" role=\"status\"", fixed = TRUE)
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    expect_true(isTRUE(shiny::isolate(state$confirmed)))
    # The confirmation on load is said by the status line and the button,
    # not by a notification as well, so one screen does not repeat it.
    expect_length(shown$notes, 0L)
    html <- as.character(output$status_chip$html)
    expect_match(html, "role=\"status\"", fixed = TRUE)
    expect_match(html, "sieve-status-success", fixed = TRUE)
    expect_false(grepl("badge", html, fixed = TRUE))
    headline <- sn_strip(regmatches(html, regexpr("<span class=\"sieve-status-headline\">[^<]*</span>", html)))
    expect_identical(trimws(headline),
      "9 items loaded; types confirmed (ML with FIML). All tabs unlocked. Open CFA to fit the theoretical structure, or Screening to look at the items first.")
    expect_lte(nchar(trimws(headline)), sieve_status_line_budget())
    text <- sn_strip(html)
    expect_lt(nchar(text), 400L)
    expect_false(grepl("A first result runs by itself", text, fixed = TRUE))
    expect_false(grepl("The core path runs left to right", text, fixed = TRUE))
    expect_match(text, "5 variables are not items: 1 identifier, 3 groups, 1 auxiliary variable.", fixed = TRUE)
    # The confirmed state of the button: opaque, with its own class, and
    # announced as disabled.
    button <- as.character(output$confirm_button$html)
    expect_match(button, "sieve-confirmed-state", fixed = TRUE)
    expect_match(button, "aria-disabled=\"true\"", fixed = TRUE)
    # The roles notice says the counts once.
    roles <- sn_strip(output$roles_notice$html)
    expect_match(roles, "Variable roles: 9 items, 1 identifier, 3 groups, 1 auxiliary variable; not items: ", fixed = TRUE)
    expect_match(roles, "id (identifier)", fixed = TRUE)
    expect_false(grepl("variables are not items", roles, fixed = TRUE))
  })
})

test_that("a data file's Load gives one notification under one id, and what needs review stays in the status line", {
  fx <- sieve_hs9_fixture()
  skip_if(is.null(fx), "DMAR data not available")
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "nine.csv")
  utils::write.csv(fx$data, csv, row.names = FALSE)
  shown <- new.env()
  shown$notes <- list()
  local_mocked_bindings(sieve_notify = function(message, type = "message", duration, id = NULL) {
    shown$notes[[length(shown$notes) + 1L]] <- list(message = message, type = type, id = id)
    invisible(NULL)
  })
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "alone", basis = "theoretical", palette = "nd", file = list(name = "nine.csv", datapath = csv))
    session$setInputs(load = 1L)
    expect_false(isTRUE(shiny::isolate(state$confirmed)))
    # One notification, a message, under the Load's id.
    expect_length(shown$notes, 1L)
    expect_identical(shown$notes[[1]]$type, "message")
    expect_identical(shown$notes[[1]]$id, session$ns("load_notification"))
    expect_match(shown$notes[[1]]$message, "nine.csv loaded (14 columns). What to review is listed in the status line above the grid.", fixed = TRUE)
    # The proposal is in the status line, and stays there.
    text <- sn_strip(output$status_chip$html)
    expect_match(text, "Step 3: 9 items loaded", fixed = TRUE)
    expect_match(text, "A role was proposed for each of the 14 columns of nine.csv", fixed = TRUE)
    expect_identical(lengths(regmatches(text, gregexpr("A role was proposed", text, fixed = TRUE))), 1L)
    # The confirmation settles the proposal: it leaves the status line.
    session$setInputs(confirm_main = 1L)
    expect_true(isTRUE(shiny::isolate(state$confirmed)))
    text <- sn_strip(output$status_chip$html)
    expect_false(grepl("A role was proposed", text, fixed = TRUE))
    # A confirmation the researcher made is confirmed for a moment, as a
    # message under its own id.
    last <- shown$notes[[length(shown$notes)]]
    expect_identical(last$type, "message")
    expect_identical(last$id, session$ns("confirm_notification"))
  })
})

test_that("a Load that returns the study's estimator to the item types says so once, in the status line and the session script", {
  # sieve_load_frame() resets the setting with the rest of the session
  # (sieve_reset_dataset()); reset_estimator_choice() must not read it only
  # after that reset, find "auto", and return, or a Load would set the
  # estimator chosen in step 4 (here MLR with FIML) back to "auto" with no
  # word in the status line or the session script.
  fx <- sieve_hs9_fixture()
  skip_if(is.null(fx), "DMAR data not available")
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "nine.csv")
  utils::write.csv(fx$data, csv, row.names = FALSE)
  shown <- new.env()
  shown$notes <- list()
  local_mocked_bindings(sieve_notify = function(message, type = "message", duration, id = NULL) {
    shown$notes[[length(shown$notes) + 1L]] <- list(message = sn_strip(message), type = type, id = id)
    invisible(NULL)
  })
  said <- paste(
    "The study's estimator follows the item types again for the new data (it was set to MLR with FIML",
    "(items treated as continuous; for markedly non-normal items) for the data loaded before); set it in",
    "step 4 if you want another."
  )
  times <- function(text, part) lengths(regmatches(text, gregexpr(part, text, fixed = TRUE)))
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "alone", basis = "theoretical", palette = "nd", file = list(name = "nine.csv", datapath = csv))
    session$setInputs(load = 1L)
    session$setInputs(estimator_choice = "mlr")
    expect_identical(shiny::isolate(state$estimator_choice), "mlr")
    shown$notes <- list()
    # A data file loaded again in place of the study.
    session$setInputs(load = 2L)
    session$setInputs(load_replace = 1L)
    expect_identical(shiny::isolate(state$estimator_choice), "auto")
    text <- sn_strip(output$status_chip$html)
    expect_identical(times(text, said), 1L)
    # Once: the Load's own notification is the only one, and it points to
    # the status line.
    expect_length(shown$notes, 1L)
    expect_identical(shown$notes[[1]]$id, session$ns("load_notification"))
    expect_match(shown$notes[[1]]$message, "What to review is listed in the status line above the grid.", fixed = TRUE)
    script <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_identical(times(script, "The study's estimator follows the item types for the new data"), 1L)
    expect_identical(times(script, "# The setting \"mlr\" was made for the data loaded before; it is \"auto\" again."), 1L)
    # The note stays until the next Load, past a confirmation.
    session$setInputs(confirm_main = 1L)
    expect_identical(times(sn_strip(output$status_chip$html), said), 1L)
    # A Load with the setting already following the item types says nothing
    # of it.
    session$setInputs(load = 3L)
    session$setInputs(load_replace = 2L)
    expect_false(grepl("follows the item types again", sn_strip(output$status_chip$html), fixed = TRUE))
    expect_false(grepl("was made for the data loaded before", paste(shiny::isolate(state$codelog), collapse = "\n"),
      fixed = TRUE))
    # A bundled example after a setting: said in the status line, which a
    # bundled Load uses in place of a notification.
    session$setInputs(estimator_choice = "ml")
    expect_identical(shiny::isolate(state$estimator_choice), "ml")
    shown$notes <- list()
    session$setInputs(source = "hs9")
    session$setInputs(load = 4L)
    session$setInputs(load_replace = 3L)
    expect_identical(shiny::isolate(state$estimator_choice), "auto")
    expect_true(isTRUE(shiny::isolate(state$confirmed)))
    text <- sn_strip(output$status_chip$html)
    expect_identical(times(text, "The study's estimator follows the item types again for the new data (it was set to ML on the complete cases"), 1L)
    expect_length(shown$notes, 0L)
    expect_identical(times(paste(shiny::isolate(state$codelog), collapse = "\n"), "# The setting \"ml\" was made for the data loaded before"), 1L)
  })
  # The user guide lists it among what the status line holds.
  guide_file <- testthat::test_path("..", "..", "vignettes", "user-guide.Rmd")
  if (!file.exists(guide_file)) guide_file <- system.file("doc", "user-guide.Rmd", package = "SIEVER")
  skip_if(!nzchar(guide_file) || !file.exists(guide_file), "the user guide is neither beside the tests nor installed")
  guide <- gsub("\\s+", " ", paste(readLines(guide_file, warn = FALSE), collapse = " "))
  expect_match(guide, paste(
    "warnings from reading the file, the return of the study's estimator to the item types when step 4 had",
    "set another for the data loaded before"
  ), fixed = TRUE)
})

test_that("a data file Loaded with its specification gives one notification, naming it, and what the specification did stays in the status line", {
  fx <- sieve_hs9_fixture()
  skip_if(is.null(fx), "DMAR data not available")
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "nine.csv")
  utils::write.csv(fx$data, csv, row.names = FALSE)
  spec <- sieve_specification_template(fx$data)
  for (cn in names(fx$theory_map)) spec$construct[spec$variable %in% fx$theory_map[[cn]]] <- cn
  # A reversal with no bounds declared is made within the observed range,
  # which the researcher must check, not a warning of its own.
  spec$reverse[spec$variable == "t2_cubes"] <- "yes"
  spec_file <- file.path(dir, "nine_specification.csv")
  sieve_spec_write(spec, spec_file)
  shown <- new.env()
  shown$notes <- list()
  local_mocked_bindings(sieve_notify = function(message, type = "message", duration, id = NULL) {
    shown$notes[[length(shown$notes) + 1L]] <- list(message = sn_strip(message), type = type, id = id)
    invisible(NULL)
  })
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "pair", basis = "theoretical", palette = "nd", spec_keying = "verify")
    session$setInputs(file = data.frame(
      name = c("nine.csv", "nine_specification.csv"), size = 0, type = "",
      datapath = c(csv, spec_file), stringsAsFactors = FALSE
    ))
    session$setInputs(load = 1L)
    expect_true(isTRUE(shiny::isolate(state$confirmed)))
    expect_length(shown$notes, 1L)
    expect_identical(shown$notes[[1]]$type, "message")
    expect_identical(shown$notes[[1]]$id, session$ns("load_notification"))
    expect_match(shown$notes[[1]]$message, "nine.csv loaded (14 columns) with the specification nine_specification.csv.", fixed = TRUE)
    html <- as.character(output$status_chip$html)
    text <- sn_strip(html)
    expect_match(text, "Confirmed from the specification: 9 continuous items", fixed = TRUE)
    expect_match(text, "Specification restored for 14 of the 14 variables in the data.", fixed = TRUE)
    expect_match(text, "t2_cubes is now reverse scored within its observed range", fixed = TRUE)
    expect_match(html, "sieve-status-warning", fixed = TRUE)
  })
})

test_that("a refused specification is a note of the data a Load read with it, and from step 3 the reason nothing changed", {
  fx <- sieve_hs9_fixture()
  skip_if(is.null(fx), "DMAR data not available")
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "nine.csv")
  utils::write.csv(fx$data, csv, row.names = FALSE)
  good <- sieve_specification_template(fx$data)
  for (cn in names(fx$theory_map)) good$construct[good$variable %in% fx$theory_map[[cn]]] <- cn
  good_file <- file.path(dir, "good_specification.csv")
  sieve_spec_write(good, good_file)
  bad <- good
  bad$kind[bad$variable == "sex"] <- "convergent"
  bad_file <- file.path(dir, "bad_specification.csv")
  sieve_spec_write(bad, bad_file)
  shown <- new.env()
  shown$notes <- list()
  local_mocked_bindings(sieve_notify = function(message, type = "message", duration, id = NULL) {
    shown$notes[[length(shown$notes) + 1L]] <- list(message = sn_strip(message), type = type, id = id)
    invisible(NULL)
  })
  # With its data in one Load: the data are loaded, the refusal is a note.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "pair", basis = "theoretical", palette = "nd", spec_keying = "verify")
    session$setInputs(file = data.frame(
      name = c("nine.csv", "bad_specification.csv"), size = 0, type = "",
      datapath = c(csv, bad_file), stringsAsFactors = FALSE
    ))
    session$setInputs(load = 1L)
    expect_false(is.null(shiny::isolate(state$meta)))
    expect_false(isTRUE(shiny::isolate(state$confirmed)))
    expect_length(shown$notes, 1L)
    expect_identical(shown$notes[[1]]$type, "message")
    expect_match(shown$notes[[1]]$message, "nine.csv loaded (14 columns); the specification bad_specification.csv was not applied.", fixed = TRUE)
    text <- sn_strip(output$status_chip$html)
    expect_match(text, "Step 3: 9 items loaded", fixed = TRUE)
    expect_match(text, "The specification bad_specification.csv was not applied, and it changed nothing; the data are loaded", fixed = TRUE)
    expect_match(text, "belongs to a criterion, and sex is not one", fixed = TRUE)
  })
  # From step 3, onto a loaded study: nothing changed, so it is the reason
  # in the status line until the next attempt, and a specification applied
  # gives one message.
  shown$notes <- list()
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd", spec_keying = "verify")
    session$setInputs(load = 1L)
    session$setInputs(up_spec = list(name = "bad_specification.csv", datapath = bad_file))
    expect_length(shown$notes, 0L)
    html <- as.character(output$status_chip$html)
    expect_match(html, "sieve-status-danger", fixed = TRUE)
    expect_match(sn_strip(html), "The specification bad_specification.csv was not applied, and it changed nothing:", fixed = TRUE)
    session$setInputs(up_spec = list(name = "good_specification.csv", datapath = good_file))
    expect_length(shown$notes, 1L)
    expect_identical(shown$notes[[1]]$type, "message")
    expect_identical(shown$notes[[1]]$id, session$ns("load_notification"))
    text <- sn_strip(output$status_chip$html)
    expect_false(grepl("was not applied", text, fixed = TRUE))
    expect_match(text, "Specification restored for 14 of the 14 variables in the data.", fixed = TRUE)
    # Applied again, it says the same things once.
    session$setInputs(up_spec = list(name = "good_specification.csv", datapath = paste0(good_file, "")))
    text <- sn_strip(output$status_chip$html)
    expect_identical(lengths(regmatches(text, gregexpr("Specification restored for", text, fixed = TRUE))), 1L)
  })
})

test_that("a file that cannot be read is reported in the status line, not in a notification", {
  dir <- withr::local_tempdir()
  bad <- file.path(dir, "broken.csv")
  writeLines("x", bad)
  shown <- new.env()
  shown$n <- 0L
  local_mocked_bindings(
    sieve_notify = function(...) {
      shown$n <- shown$n + 1L
      invisible(NULL)
    },
    sieve_read_data = function(path, name = basename(path), description = TRUE, ...) {
      list(ok = FALSE, error = "the file holds no rows of data")
    }
  )
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "alone", basis = "theoretical", palette = "nd", file = list(name = "broken.csv", datapath = bad))
    session$setInputs(load = 1L)
    expect_null(shiny::isolate(state$meta))
    html <- as.character(output$status_chip$html)
    expect_match(html, "sieve-status-danger", fixed = TRUE)
    expect_match(sn_strip(html), "broken.csv was not loaded: the file holds no rows of data. The session is unchanged.", fixed = TRUE)
    expect_identical(shown$n, 0L)
  })
})

test_that("sieve_status_line() drops empty notes and colors only the border", {
  x <- as.character(sieve_status_line("Ready.", notes = c("", NA, "One note."), tone = "warning"))
  expect_match(x, "<div class=\"sieve-status-line sieve-status-warning\" role=\"status\">", fixed = TRUE)
  expect_identical(lengths(regmatches(x, gregexpr("<li>", x, fixed = TRUE))), 1L)
  expect_false(grepl("<ul", as.character(sieve_status_line("Ready.")), fixed = TRUE))
  expect_error(sieve_status_line("x", tone = "loud"))
})

# ---- The method headline and its provenance ------------------------------------

test_that("the method statement is one headline naming the estimator and n, with the provenance in a labeled disclosure", {
  skip_if_not_installed("lavaan")
  hs <- lavaan::HolzingerSwineford1939
  hs$x1[1:20] <- NA
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6", data = hs, missing = "fiml", std.lv = TRUE)
  parts <- sieve_method_parts(fit, "Correlated-factors CFA")
  expect_identical(parts$headline,
    "Correlated-factors CFA in lavaan: ML with FIML; 6 observed variables, 2 factors; n = 301.")
  expect_lte(nchar(parts$headline), 160L)
  expect_identical(sieve_method_text(fit, "Correlated-factors CFA"), parts$text)
  html <- as.character(sieve_method_line(fit, "Correlated-factors CFA"))
  expect_match(html, "<span class=\"sieve-method-headline\">", fixed = TRUE)
  expect_match(html, "<details class=\"sieve-provenance\" data-sieve-remember=\"method-line\">", fixed = TRUE)
  expect_match(html, "<summary>Method and data behind this result</summary>", fixed = TRUE)
  # Everything the four-line statement said is still there, in the
  # disclosure: the full text and the uncertainty.
  expect_match(html, parts$text, fixed = TRUE)
  expect_match(html, "Uncertainty: ", fixed = TRUE)
  expect_null(sieve_method_line(NULL, "x"))
})

test_that("a reused fit is said once in the status line; why nothing was fit again is a disclosure", {
  run <- list(reused = TRUE, reused_from = "CFA", reused_when = "22:57", seconds = 0.9)
  x <- as.character(sieve_reuse_line(run, list(state = "current")))
  expect_match(x, "<summary>Why nothing was fit again</summary>", fixed = TRUE)
  expect_match(x, "The fit is that of CFA at 22:57: the identical lavaan model on the identical data", fixed = TRUE)
  expect_false(grepl("Reused the fit from", x, fixed = TRUE))
  expect_null(sieve_reuse_line(run, list(state = "stale")))
})

test_that("the CFA page puts the diagram before the item pointer, and its fit line is one sentence", {
  html <- gsub("\\s+", " ", as.character(mod_cfa_ui("cfa")))
  diagram <- regexpr("sieve-item-decision-pointer", html, fixed = TRUE)
  fit <- regexpr("id=\"cfa-overview_fit\"", html, fixed = TRUE)
  panel <- regexpr("id=\"cfa-diagram\"", html, fixed = TRUE)
  expect_gt(diagram, panel)
  expect_gt(panel, fit)
})

# ---- The Update line and the page guide ----------------------------------------

test_that("the Update line of a page with results from before keeps its warning style, apart from the status line", {
  # The Update line does not carry the class sieve-status-line, whose rule
  # in custom.css would draw it on white with a grey border.
  entry <- sieve_update_entry("Correlated-factors model", "stale", sentence = "an item was excluded", seconds = 3, run = function() NULL)
  shiny::testServer(
    function(input, output, session) {
      entries <- shiny::reactiveVal(list(cfa = entry))
      wire <- sieve_update_wire(input, session, function() entries())
    },
    {
      x <- as.character(wire$line())
      expect_match(x, "class=\"alert alert-warning py-2 mb-2 sieve-update-line\"", fixed = TRUE)
      expect_false(grepl("sieve-status-line", x, fixed = TRUE))
    }
  )
  css <- paste(readLines(system.file("app", "www", "custom.css", package = "SIEVER"), warn = FALSE), collapse = "\n")
  rules <- gsub("(?s)/[*].*?[*]/", "", css, perl = TRUE)
  expect_false(grepl("sieve-update-line", rules, fixed = TRUE))
  expect_match(rules, ".sieve-status-line {", fixed = TRUE)
})

test_that("the user guide says which Load raises a notification: a data file's one, a bundled example's none", {
  # The guide must not say that a Load itself raises one short
  # notification: a bundled example's Load raises none (the test of the
  # status line above), a data file's raises one (the test of its Load
  # above), and a file that cannot be read raises none (the test of a
  # refused file above).
  guide_file <- testthat::test_path("..", "..", "vignettes", "user-guide.Rmd")
  if (!file.exists(guide_file)) guide_file <- system.file("doc", "user-guide.Rmd", package = "SIEVER")
  skip_if(!nzchar(guide_file) || !file.exists(guide_file), "the user guide is neither beside the tests nor installed")
  guide <- gsub("\\s+", " ", paste(readLines(guide_file, warn = FALSE), collapse = " "))
  expect_false(grepl("a Load itself raises one short notification", guide, fixed = TRUE))
  expect_match(guide, "A Load that reads a data file raises one short notification, which the next Load replaces", fixed = TRUE)
  expect_match(guide, "a file that cannot be read is reported in the status line alone", fixed = TRUE)
  expect_match(guide, "a Load of a bundled example raises none", fixed = TRUE)
})

test_that("the user guide does not give the page guide an Update button", {
  guide_file <- testthat::test_path("..", "..", "vignettes", "user-guide.Rmd")
  if (!file.exists(guide_file)) guide_file <- system.file("doc", "user-guide.Rmd", package = "SIEVER")
  skip_if(!nzchar(guide_file) || !file.exists(guide_file), "the user guide is neither beside the tests nor installed")
  guide <- gsub("\\s+", " ", paste(readLines(guide_file, warn = FALSE), collapse = " "))
  expect_false(grepl("the guide carries the page's one **Update** button", guide, fixed = TRUE))
  expect_false(grepl("the page guide offers the same button", guide, fixed = TRUE))
  expect_match(guide, "the guide has no button of its own", fixed = TRUE)
  # (That the guide renders no button is checked in test-run-policy-rates.R.)
})

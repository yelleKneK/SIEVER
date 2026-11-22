# Excluding and including items from the pages that show item results, and
# the lifecycle of a result after such a change, so that the CFA page says
# how to remove an item and obtain updated results. Tested here: the CFA
# Estimates table and the EFA Pattern and Structure tables send their
# selected rows to the one shared dialog (mod_drop.R), whose Exclude and
# Include are the only writers of the item status; the dialog names where
# the request came from and fills the picker; a requested result refits by
# itself after the change, exactly once, and its line says Current and what
# it was fitted to; a threshold edit fits nothing; an item excluded
# recovers the respondents missing only on it, and including it again loses
# them; a change whose refit is refused says Not refit with the reason; a
# canceled background computation says Stopped until the next run; the
# decision history and the reasons are recorded once whichever page asked;
# a saved project carries the decision and not the request; and the one
# vocabulary of the lifecycle badge.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

# A tag is rendered to HTML first (unlist() would flatten it into its
# attribute strings); a rendered output (a list with html) is read as is.
id_html <- function(x) {
  if (inherits(x, "shiny.tag") || inherits(x, "shiny.tag.list")) return(paste(as.character(x), collapse = ""))
  paste(as.character(unlist(x)), collapse = "")
}
id_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", id_html(x)))
}

# The bundled nine tests, confirmed, with the variable roles declared (as
# test-ondemand.R builds them); ordered items under WLSMV on request.
id_nine <- function(ordered = FALSE) {
  fx <- sieve_hs9_fixture()
  siever_test_study(fx, confirmed = TRUE, labels = TRUE, theory = TRUE, name = fx$name,
    configure = function(s) {
      s$roles <- sieve_declared_roles(fx$data, fx$item_cols)
      s$roles$role[s$roles$role == "auxiliary"] <- "criterion"
      s$estimator <- "MLR"
      if (ordered) {
        shiny::isolate({
          s$items <- as.data.frame(lapply(s$items, function(v) as.integer(cut(as.numeric(v), breaks = 5))))
          m <- s$meta
          m$type <- "ordered"
          s$meta <- m
          s$estimator <- "WLSMV"
        })
      }
    })
}

# Count every SIEVER fit made while a test runs (every fit goes through
# sieve_run_model()), and keep the notifications. Installed in the caller's
# frame, as the mocks of a testServer block must be.
id_count <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$runs <- 0L
  seen$notes <- character(0)
  real_run <- sieve_run_model
  testthat::local_mocked_bindings(
    sieve_run_model = function(...) {
      seen$runs <- seen$runs + 1L
      real_run(...)
    },
    sieve_notify = function(message, type = "message", duration = 8) {
      seen$notes <- c(seen$notes, message)
      invisible(NULL)
    },
    .env = env
  )
  seen
}

id_cfa_inputs <- function(session) {
  session$setInputs(scope = "construct", structure = "first", ident = "varone", estimator = "study", min_path = 0)
}

# The dialog's modal, captured, and its closing counted.
id_dialog <- function(env = parent.frame()) {
  shown <- new.env(parent = emptyenv())
  shown$ui <- NULL
  shown$removed <- 0L
  shown$notes <- character(0)
  testthat::local_mocked_bindings(
    showModal = function(ui, session = NULL) assign("ui", ui, envir = shown),
    # Every notification goes through sieve_notify().
    sieve_notify = function(message, ...) {
      shown$notes <- c(shown$notes, paste(as.character(message), collapse = " "))
      invisible(NULL)
    },
    .package = "SIEVER", .env = env
  )
  testthat::local_mocked_bindings(
    removeModal = function(session = shiny::getDefaultReactiveDomain()) {
      shown$removed <- shown$removed + 1L
      invisible(NULL)
    },
    .package = "shiny", .env = env
  )
  shown
}

# ---- The state field and the request -----------------------------------------

test_that("the request field is session-only: never saved, never in the fingerprint, reset with the data", {
  fields <- sieve_state_fields()
  expect_true("item_decision_request" %in% names(fields))
  expect_null(fields$item_decision_request$default)
  expect_false(fields$item_decision_request$persisted)
  expect_false(fields$item_decision_request$fingerprint)
  expect_true("item_decision_request" %in% sieve_state_exclusions("persisted"))
  expect_false("item_decision_request" %in% sieve_project_fields())
  expect_null(shiny::isolate(sieve_new_state()$item_decision_request))
})

test_that("a request names the items in the ledger, the side and the origin, and two requests for the same items differ", {
  st <- id_nine()
  it <- shiny::isolate(st$meta)$item
  first <- sieve_item_decision_request(st, c(it[2], "not_an_item", it[2], NA), "parked", "the CFA Estimates table")
  expect_identical(first$items, it[2])
  expect_identical(first$status, "parked")
  expect_identical(first$origin, "the CFA Estimates table")
  expect_identical(shiny::isolate(st$item_decision_request), first)
  second <- sieve_item_decision_request(st, it[2], "parked", "the CFA Estimates table")
  expect_false(identical(first$nonce, second$nonce))
  empty <- sieve_item_decision_request(st, character(0), "active", "the EFA Pattern matrix")
  expect_identical(empty$items, character(0))
  expect_identical(empty$status, "active")
  expect_error(sieve_item_decision_request(st, it[1], "deleted"), "arg")
})

# ---- The shared dialog ------------------------------------------------------------

test_that("the dialog opens from a page's request with the items filled in, names where it came from, excludes through the one writer, records the reason once and closes", {
  test_frame <- environment()
  shown <- id_dialog(test_frame)
  st <- id_nine()
  it <- shiny::isolate(st$meta)$item
  shiny::testServer(mod_drop_server, args = list(state = st), {
    sieve_item_decision_request(state, it[1:2], "parked", "the CFA Estimates table")
    session$flushReact()
    expect_false(is.null(shown$ui))
    expect_match(id_text(shown$ui), "Exclude / Include items", fixed = TRUE)
    html <- id_html(output$body$html)
    body <- id_text(html)
    expect_match(body, "Opened from the CFA Estimates table. 2 selected items are filled in below; add or remove items before deciding.", fixed = TRUE)
    expect_match(body, "An analysis already requested for this study is refit by itself after the change", fixed = TRUE)
    # The exclusion picker holds the two items; the inclusion picker is empty.
    for (item in it[1:2]) expect_match(html, sprintf('<option value="%s" selected>', item), fixed = TRUE)
    expect_false(grepl(sprintf('<option value="%s" selected>', it[3]), html, fixed = TRUE))
    before <- length(shiny::isolate(state$item_actions))
    session$setInputs(drop_pick = it[1:2], drop_reason = "asked from the CFA table", drop_go = 1L)
    m <- shiny::isolate(state$meta)
    expect_identical(m$status[match(it[1:2], m$item)], c("parked", "parked"))
    expect_identical(m$exclude_reason[match(it[1:2], m$item)], rep("asked from the CFA table", 2L))
    expect_identical(m$status[match(it[3:9], m$item)], rep("active", 7L))
    # One history record for the batch, the reason in it and in the code log.
    expect_length(shiny::isolate(state$item_actions), before + 1L)
    record <- shiny::isolate(state$item_actions)[[before + 1L]]
    expect_identical(sort(record$items), sort(it[1:2]))
    expect_identical(record$reason, "asked from the CFA table")
    expect_true(any(grepl("asked from the CFA table", shiny::isolate(state$codelog), fixed = TRUE)))
    # The request is spent and the dialog closed.
    expect_null(shiny::isolate(state$item_decision_request))
    expect_identical(shown$removed, 1L)
    expect_match(shown$notes[length(shown$notes)], "Excluded 2 items", fixed = TRUE)
    # A request with no items opens the dialog unfilled and says so.
    sieve_item_decision_request(state, character(0), "parked", "the EFA Pattern matrix")
    session$flushReact()
    body <- id_text(output$body$html)
    expect_match(body, "Opened from the EFA Pattern matrix. No row was selected there, so pick the items below.", fixed = TRUE)
    # Nothing was changed by opening.
    expect_identical(shiny::isolate(state$meta)$status[match(it[1:2], m$item)], c("parked", "parked"))
    expect_identical(shown$removed, 1L)
    # The inclusion side fills from an "active" request; including records once more.
    sieve_item_decision_request(state, it[1], "active", "the EFA Structure matrix")
    session$flushReact()
    html <- id_html(output$body$html)
    expect_match(html, sprintf('<option value="%s" selected>', it[1]), fixed = TRUE)
    session$setInputs(restore_pick = it[1], restore_reason = "", restore_go = 1L)
    m <- shiny::isolate(state$meta)
    expect_identical(m$status[m$item == it[1]], "active")
    expect_true(is.na(m$exclude_reason[m$item == it[1]]))
    expect_length(shiny::isolate(state$item_actions), before + 2L)
    expect_identical(shown$removed, 2L)
    expect_null(shiny::isolate(state$item_decision_request))
    # The navbar link opens the dialog unfilled, and a decision made from it
    # does not close it (the researcher may make several).
    session$setInputs(open = 1L)
    body <- id_text(output$body$html)
    expect_false(grepl("Opened from", body, fixed = TRUE))
    session$setInputs(drop_pick = it[3], drop_reason = "", drop_go = 2L)
    expect_identical(shown$removed, 2L)
    expect_identical(shiny::isolate(state$meta)$status[match(it[3], it)], "parked")
  })
})

test_that("the dialog's preview says which respondents an exclusion recovers and an inclusion loses, before anything changes", {
  st <- missing_study_state()
  item <- shiny::isolate(st$meta)$item[1L]
  shiny::testServer(mod_drop_server, args = list(state = st), {
    session$setInputs(drop_pick = item)
    preview <- id_text(output$drop_preview$html)
    expect_match(preview, "Included items: 9 to 8. Complete respondents: 276 to 301; 25 recovered, 0 lost.", fixed = TRUE)
    expect_match(preview, "No change has been applied yet.", fixed = TRUE)
    expect_identical(shiny::isolate(state$meta)$status[1L], "active")
    session$setInputs(drop_reason = "missing on 25 respondents", drop_go = 1L)
    expect_identical(shiny::isolate(state$meta)$status[1L], "parked")
    session$setInputs(restore_pick = item)
    preview <- id_text(output$restore_preview$html)
    expect_match(preview, "Included items: 8 to 9. Complete respondents: 301 to 276; 0 recovered, 25 lost.", fixed = TRUE)
  })
})

# ---- The CFA page --------------------------------------------------------------------

test_that("the CFA page puts the button and its sentence beside the Estimates table and a pointer under the diagram", {
  html <- id_html(mod_cfa_ui("cfa"))
  at <- function(x) {
    pos <- regexpr(x, html, fixed = TRUE)[[1]]
    if (pos < 0) stop("not found: ", x, call. = FALSE)
    pos
  }
  expect_lt(at('id="cfa-est_decide"'), at('id="cfa-est_tbl"'))
  expect_match(html, "Exclude or include items", fixed = TRUE)
  expect_match(id_text(html), "An exclusion applies to this whole study, on every page, and deletes nothing", fixed = TRUE)
  # The pointer's text children are rendered on separate lines, so the
  # phrase searched for lies within one of them.
  # Under the diagram, so that the diagram follows the fit line directly.
  expect_lt(at('id="cfa-overview_fit"'), at('id="cfa-diagram"'))
  expect_lt(at('id="cfa-diagram"'), at("Estimates sub-tab and press Exclude or include items; nothing is deleted"))
})

test_that("the CFA Estimates table's button sends the selected rows, as variable names, to the dialog", {
  st <- id_nine()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    id_cfa_inputs(session)
    session$setInputs(fit_go = 1L)
    expect_no_error(output$est_tbl)
    rows <- est_shown$items
    expect_length(rows, 9L)
    expect_setequal(rows, shiny::isolate(state$meta)$item)
    session$setInputs(est_tbl_rows_selected = c(2L, 5L))
    session$setInputs(est_decide = 1L)
    request <- shiny::isolate(state$item_decision_request)
    expect_identical(request$items, rows[c(2L, 5L)])
    expect_identical(request$status, "parked")
    expect_identical(request$origin, "the CFA Estimates table")
    # The request alone changes nothing.
    expect_identical(unique(shiny::isolate(state$meta)$status), "active")
    # No selection: the dialog opens unfilled.
    session$setInputs(est_tbl_rows_selected = integer(0))
    session$setInputs(est_decide = 2L)
    expect_identical(shiny::isolate(state$item_decision_request)$items, character(0))
  })
})

test_that("an exclusion refits the requested CFA once, by itself; the line says Current and what it was fitted to; a threshold edit fits nothing; the inclusion refits once more", {
  test_frame <- environment()
  seen <- id_count(test_frame)
  st <- id_nine()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    id_cfa_inputs(session)
    line <- id_text(output$method_line)
    expect_match(line, "Not fit yet Method:", fixed = TRUE)
    session$setInputs(fit_go = 1L)
    first <- seen$runs
    expect_gt(first, 0L)
    st1 <- demand$status()
    expect_identical(st1$state, "current")
    expect_identical(st1$trigger, "request")
    expect_length(st1$fitted$items, 9L)
    expect_identical(st1$fitted$respondents, 301L)
    choices_before <- st1$key$choices
    line <- id_text(output$method_line)
    expect_match(line, "Current Method:", fixed = TRUE)
    # The headline ends with the time of the fit; what the diagram and the
    # tables belong to is in the provenance under it.
    expect_match(line, "Fit at [0-9]{2}:[0-9]{2} in [0-9.]+ seconds, current\\. Method and data behind this result")
    expect_match(line, "The diagram and tables below are from this fit of the 9 included items (", fixed = TRUE)
    expect_match(line, "and 4 more) and 301 respondents.", fixed = TRUE)
    expect_false(grepl("by itself", line, fixed = TRUE))
    expect_identical(sieve_demand_lifecycle(st1), "current")
    items <- shiny::isolate(state$meta)$item
    sieve_set_item_status(state, items[1L], "parked", "from the test")
    session$flushReact()
    expect_identical(seen$runs, 2L * first)
    st2 <- demand$status()
    expect_identical(st2$state, "current")
    expect_identical(st2$trigger, "inclusion")
    expect_length(st2$fitted$items, 8L)
    expect_false(items[1L] %in% st2$fitted$items)
    expect_false(isTRUE(cfa_run()$reused))
    line <- id_text(output$method_line)
    expect_match(line, "Current Method:", fixed = TRUE)
    expect_match(line, "from this fit of the 8 included items (", fixed = TRUE)
    expect_match(line, "It was refit by itself after your item change.", fixed = TRUE)
    expect_false(grepl("Update needed", id_text(output$gate), fixed = TRUE))
    # A threshold edit that changes a value recolors flags and fits nothing.
    state$thresholds <- utils::modifyList(sieve_default_thresholds(), list(load_low = 0.45))
    session$flushReact()
    expect_identical(seen$runs, 2L * first)
    expect_identical(demand$status()$state, "current")
    # Including the item again runs once more, to the nine: the session's
    # fit store holds the identical first fit, so no engine call is made,
    # and the line says the identical fit was reused.
    sieve_set_item_status(state, items[1L], "active")
    session$flushReact()
    st3 <- demand$status()
    expect_identical(st3$state, "current")
    expect_length(st3$fitted$items, 9L)
    expect_identical(st3$trigger, "inclusion")
    expect_true(isTRUE(cfa_run()$reused))
    expect_identical(seen$runs, 2L * first)
    line <- id_text(output$method_line)
    expect_match(line, "It was updated by itself after your item change, reusing the identical earlier fit.", fixed = TRUE)
    # Two items excluded in one batch, one from Visual and one from Textual:
    # one refit, to the seven on three factors.
    sieve_set_item_status(state, items[c(2L, 5L)], "parked", "two at once")
    session$flushReact()
    expect_identical(seen$runs, 3L * first)
    st4 <- demand$status()
    expect_identical(st4$state, "current")
    expect_identical(st4$trigger, "inclusion")
    expect_length(st4$fitted$items, 7L)
    expect_false(any(items[c(2L, 5L)] %in% st4$fitted$items))
    expect_identical(st4$fitted$included, 7L)
    expect_setequal(st4$fitted$items, lavaan::lavNames(cfa_run()$fit, "ov"))
    line <- id_text(output$method_line)
    expect_match(line, "7 items on 3 factors", fixed = TRUE)
    expect_match(line, "from this fit of the 7 included items (", fixed = TRUE)
    # The page's settings (the choices in the result's identity) are
    # untouched by the refits.
    expect_identical(st4$key$choices, choices_before)
    # Excluding the other Visual item but one leaves that construct with a
    # single included item, so it leaves the model (sieve_cfa_plan(),
    # min_items): the refit uses five of the six included items, the
    # sentence counts them against the six, and the note above the diagram
    # names the construct and its item.
    sieve_set_item_status(state, items[3L], "parked", "one left in Visual")
    session$flushReact()
    expect_identical(seen$runs, 4L * first)
    st5 <- demand$status()
    expect_identical(st5$state, "current")
    expect_identical(st5$trigger, "inclusion")
    expect_length(st5$fitted$items, 5L)
    expect_false(items[1L] %in% st5$fitted$items)
    expect_identical(st5$fitted$included, 6L)
    expect_setequal(st5$fitted$items, lavaan::lavNames(cfa_run()$fit, "ov"))
    line <- id_text(output$method_line)
    expect_match(line, "5 items on 2 factors", fixed = TRUE)
    expect_match(line, "from this fit of 5 of the 6 included items (", fixed = TRUE)
    expect_match(line, "It was refit by itself after your item change.", fixed = TRUE)
    expect_match(id_text(output$gate),
      "Construct 'Visual' is not in the model: the construct has 1 included item; a factor needs at least 2 (t1_visual_perception).",
      fixed = TRUE)
    # The lone Visual item is outside the model, so excluding it changes
    # nothing analytic: no refit, the fit stays current, and the sentence and
    # the note follow the ledger (five included, nothing left out).
    sieve_set_item_status(state, items[1L], "parked", "outside the model")
    session$flushReact()
    expect_identical(seen$runs, 4L * first)
    st5b <- demand$status()
    expect_identical(st5b$state, "current")
    expect_identical(st5b$fitted$included, 5L)
    expect_length(st5b$fitted$items, 5L)
    expect_match(id_text(output$method_line), "from this fit of the 5 included items (", fixed = TRUE)
    expect_false(grepl("Construct 'Visual'", id_text(output$gate), fixed = TRUE))
    # Including it again: still current, and the count and the note return.
    sieve_set_item_status(state, items[1L], "active")
    session$flushReact()
    expect_identical(seen$runs, 4L * first)
    st5c <- demand$status()
    expect_identical(st5c$state, "current")
    expect_identical(st5c$fitted$included, 6L)
    expect_match(id_text(output$method_line), "from this fit of 5 of the 6 included items (", fixed = TRUE)
    expect_match(id_text(output$gate), "Construct 'Visual' is not in the model", fixed = TRUE)
    # Including that item again brings the construct back. The fit of the
    # seven is in the session's store, so no engine call is made, and the
    # sentence counts the seven included items plainly again.
    sieve_set_item_status(state, items[3L], "active")
    session$flushReact()
    expect_identical(seen$runs, 4L * first)
    st6 <- demand$status()
    expect_identical(st6$state, "current")
    expect_identical(st6$trigger, "inclusion")
    expect_true(isTRUE(cfa_run()$reused))
    expect_length(st6$fitted$items, 7L)
    expect_setequal(st6$fitted$items, lavaan::lavNames(cfa_run()$fit, "ov"))
    expect_identical(st6$fitted$included, 7L)
    line <- id_text(output$method_line)
    expect_match(line, "from this fit of the 7 included items (", fixed = TRUE)
    expect_match(line, "It was updated by itself after your item change, reusing the identical earlier fit.", fixed = TRUE)
  })
})

test_that("the respondents the fit used follow an exclusion and an inclusion under listwise deletion, and the line says so", {
  test_frame <- environment()
  seen <- id_count(test_frame)
  st <- missing_study_state()
  st$estimator_choice <- "ml"
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", estimator = "study", min_path = 0)
    session$setInputs(fit_go = 1L)
    first <- seen$runs
    st1 <- demand$status()
    expect_identical(st1$fitted$respondents, 276L)
    expect_match(id_text(output$method_line), "from this fit of the 9 included items (", fixed = TRUE)
    expect_match(id_text(output$method_line), "and 276 respondents.", fixed = TRUE)
    item <- shiny::isolate(state$meta)$item[1L]
    # The item missing on 25 respondents leaves: they are recovered.
    sieve_set_item_status(state, item, "parked", "missing on 25 respondents")
    session$flushReact()
    expect_identical(seen$runs, 2L * first)
    st2 <- demand$status()
    expect_identical(st2$fitted$respondents, 301L)
    expect_length(st2$fitted$items, 8L)
    expect_match(id_text(output$method_line), "and 301 respondents. It was refit by itself after your item change.", fixed = TRUE)
    # It returns: the complete cases are lost again.
    sieve_set_item_status(state, item, "active")
    session$flushReact()
    st3 <- demand$status()
    expect_identical(st3$fitted$respondents, 276L)
    expect_length(st3$fitted$items, 9L)
    expect_match(id_text(output$method_line), "and 276 respondents.", fixed = TRUE)
  })
})

test_that("an exclusion whose refit is refused says Not refit with the reason, Update needed, and keeps the result below as from before", {
  test_frame <- environment()
  seen <- id_count(test_frame)
  st <- id_nine(ordered = TRUE)
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    id_cfa_inputs(session)
    session$setInputs(fit_go = 1L)
    first <- seen$runs
    expect_identical(demand$status()$state, "current")
    expect_setequal(demand$status()$fitted$items, lavaan::lavNames(cfa_run()$fit, "ov"))
    # From now on every WLSMV weight matrix is above the limit (set on the
    # test's own frame, so it does not reach a later test).
    withr::local_options(sieve.wlsmv_limit = 10, .local_envir = test_frame)
    items <- shiny::isolate(state$meta)$item
    sieve_set_item_status(state, items[1L], "parked", "refused refit")
    session$flushReact()
    expect_identical(seen$runs, first)
    st2 <- demand$status()
    expect_identical(st2$state, "stale")
    expect_match(st2$sentence, sprintf("%s excluded", items[1L]), fixed = TRUE)
    expect_length(st2$fitted$items, 9L)
    gate <- id_text(output$gate)
    expect_match(gate, "Not refit: the weight matrix of a WLSMV fit of these 8 items", fixed = TRUE)
    expect_false(grepl("Not fit:", gate, fixed = TRUE))
    expect_match(gate, "Update needed", fixed = TRUE)
    expect_match(gate, sprintf("What changed: %s excluded. The result below is from before (CFA: WLSMV, 9 items at", items[1L]), fixed = TRUE)
    expect_identical(sieve_demand_lifecycle(st2), "stale")
    expect_match(id_text(output$method_line), "Update needed Method:", fixed = TRUE)
    # Pressing Update is refused too, and nothing is fit.
    session$setInputs(fit_go = 2L)
    expect_identical(seen$runs, first)
    expect_match(seen$notes[length(seen$notes)], "Not fit: the weight matrix", fixed = TRUE)
  })
})

test_that("a saved project carries the decision and its reason, and never the request", {
  test_frame <- environment()
  shown <- id_dialog(test_frame)
  st <- id_nine()
  it <- shiny::isolate(st$meta)$item
  shiny::testServer(mod_drop_server, args = list(state = st), {
    sieve_item_decision_request(state, it[4], "parked", "the EFA Pattern matrix")
    session$flushReact()
    session$setInputs(drop_pick = it[4], drop_reason = "saved with the project", drop_go = 1L)
  })
  # A request left unanswered is not saved either.
  sieve_item_decision_request(st, it[5], "parked", "the CFA Estimates table")
  snapshot <- sieve_project_snapshot(st)
  other <- sieve_new_state()
  sieve_project_restore(other, snapshot, source = "decisions.siever.rds")
  m <- shiny::isolate(other$meta)
  expect_identical(m$status[m$item == it[4]], "parked")
  expect_identical(m$exclude_reason[m$item == it[4]], "saved with the project")
  expect_identical(m$status[m$item == it[5]], "active")
  history <- shiny::isolate(other$item_actions)
  expect_true(any(vapply(history, function(r) identical(r$reason, "saved with the project"), logical(1))))
  expect_null(shiny::isolate(other$item_decision_request))
  expect_true(any(grepl("saved with the project", shiny::isolate(other$codelog), fixed = TRUE)))
  # The session script replays the decision with its reason.
  script <- sieve_replay_script(other)
  expect_match(script, sprintf('meta$status[meta$item %%in%% c("%s")] <- "parked"', it[4]), fixed = TRUE)
  expect_match(script, "saved with the project", fixed = TRUE)
})

# ---- The EFA page --------------------------------------------------------------------

test_that("the EFA page puts a button and its sentence beside the Pattern and the Structure tables", {
  html <- id_html(mod_efa_ui("efa"))
  at <- function(x) {
    pos <- regexpr(x, html, fixed = TRUE)[[1]]
    if (pos < 0) stop("not found: ", x, call. = FALSE)
    pos
  }
  expect_lt(at('id="efa-pattern_decide"'), at('id="efa-pattern_tbl"'))
  expect_lt(at('id="efa-structure_decide"'), at('id="efa-structure_tbl"'))
  expect_lt(at('id="efa-pattern_tbl"'), at('id="efa-structure_decide"'))
  expect_identical(lengths(regmatches(html, gregexpr("An exclusion applies to this whole study", html, fixed = TRUE))), 2L)
})

test_that("the EFA tables send their displayed rows to the dialog, and an exclusion updates the loadings by itself, once, with the method line saying so", {
  withr::local_options(sieve.cores = 2L, sieve.auto_run = "never")
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  test_frame <- environment()
  # Every DMAR::fa_exploratory() call is counted: the solution is fit once
  # per request (the EFA page's engine).
  fits <- new.env(parent = emptyenv())
  fits$n <- 0L
  real_fa <- DMAR::fa_exploratory
  # (The method line says the intervals are at the page's 95%, the RMSEA's
  # at the RMSEA level, 90%.)
  local_mocked_bindings(fa_exploratory = function(...) {
    fits$n <- fits$n + 1L
    real_fa(...)
  }, .package = "DMAR", .env = test_frame)
  st <- id_nine()
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(
      n_factors = 3L, rotation = "oblimin", fm = "ml", cor_type = "auto",
      estimator = "study", suppress = 0.3, sort_loadings = TRUE
    )
    expect_match(id_text(output$status_line), "Not fit yet", fixed = TRUE)
    session$setInputs(efa_go = 1L)
    expect_identical(primary_status()$state, "current")
    first <- fits$n
    expect_gt(first, 0L)
    expect_no_error(output$pattern_tbl)
    expect_no_error(output$structure_tbl)
    shown <- shown_items$pattern
    expect_length(shown, 9L)
    expect_setequal(shown, shiny::isolate(state$meta)$item)
    expect_setequal(shown_items$structure, shown)
    method <- id_text(output$efa_method)
    expect_match(method, "Current Model: DMAR::fa_exploratory", fixed = TRUE)
    expect_match(method, "9 items; FIML, N = 301; 95% intervals \\(the RMSEA's at 90%\\)\\. Fit at [0-9]{2}:[0-9]{2}; the tables below are from this fit.")
    expect_false(grepl("after your item change", method, fixed = TRUE))
    expect_null(refit_after_item())
    # The displayed order (sorted by dominant factor) is what the rows mean.
    session$setInputs(pattern_tbl_rows_selected = c(1L, 3L))
    session$setInputs(pattern_decide = 1L)
    request <- shiny::isolate(state$item_decision_request)
    expect_identical(request$items, shown[c(1L, 3L)])
    expect_identical(request$origin, "the EFA Pattern matrix")
    session$setInputs(structure_tbl_rows_selected = 2L)
    session$setInputs(structure_decide = 1L)
    request <- shiny::isolate(state$item_decision_request)
    expect_identical(request$items, shown_items$structure[2L])
    expect_identical(request$origin, "the EFA Structure matrix")
    # The exclusion itself: the loadings follow by themselves. (The item is
    # t2_cubes. DMAR refuses an improper solution, and without the first
    # displayed item (a textual test) the textual factor keeps two items and
    # t6's residual variance goes below zero; without t2_cubes the solution
    # is proper.)
    excluded <- "t2_cubes"
    sieve_set_item_status(state, excluded, "parked", "from the EFA table")
    session$flushReact()
    for (i in seq_len(20L)) {
      if (identical(primary_status()$state, "current")) break
      session$elapse(250L)
    }
    expect_identical(primary_status()$state, "current")
    expect_identical(nrow(unclass(efa()$loadings)), 8L)
    expect_false(excluded %in% rownames(unclass(efa()$loadings)))
    # One solution was fit for the change, no more.
    expect_identical(fits$n, 2L * first)
    expect_identical(refit_after_item(), primary_kept$key)
    method <- id_text(output$efa_method)
    expect_match(method, "Current Model: DMAR::fa_exploratory", fixed = TRUE)
    expect_match(method, "8 items; FIML, N = 301; 95% intervals \\(the RMSEA's at 90%\\)\\. Fit at [0-9]{2}:[0-9]{2}; the tables below are from this fit. It was refit by itself after your item change.")
    expect_no_error(output$pattern_tbl)
    expect_length(shown_items$pattern, 8L)
    # A press of Run clears the note.
    session$setInputs(efa_go = 2L)
    expect_null(refit_after_item())
  })
})

# ---- Stopped, and the one vocabulary ---------------------------------------------------

test_that("a canceled background computation says Stopped until the next run starts, also when a result from before stays on the page", {
  withr::local_options(sieve.auto_run = "never")
  st <- id_nine()
  handles <- new.env(parent = emptyenv())
  handles$made <- 0L
  handles$last <- NULL
  shiny::testServer(function(input, output, session) {
    wire <- sieve_demand_wire(input, session, st, "run", "Slow analysis",
      key = shiny::reactive(sieve_demand_key(st, items = st$meta$item[st$meta$status == "active"])),
      plan = shiny::reactive(list(seconds = 3, estimator = "MLR", p = 9, progress = "Slow analysis: 9 items", label = "3 seconds")),
      compute = function(...) stop("not used"),
      background = list(
        start = function(k, p) {
          handles$made <- handles$made + 1L
          h <- new.env(parent = emptyenv())
          h$alive <- TRUE
          h$is_alive <- function() h$alive
          h$kill <- function() h$alive <- FALSE
          h$get_result <- function() list(estimate = 0.7)
          handles$last <- h
          h
        },
        finish = function(k, p, value, handle, current) value,
        in_process = function() FALSE
      )
    )
  }, {
    session$setInputs(run = 1L)
    st1 <- wire$status()
    expect_identical(st1$state, "running")
    expect_null(st1$stopped)
    expect_identical(sieve_demand_lifecycle(st1), "running")
    expect_match(id_text(wire$line()), "Running The model is being fit in a separate R process.", fixed = TRUE)
    wire$cancel()
    st2 <- wire$status()
    expect_identical(st2$state, "not_run")
    expect_identical(st2$stopped$progress, "Slow analysis: 9 items")
    expect_match(st2$stopped$when, "^[0-9]{2}:[0-9]{2}$")
    expect_identical(sieve_demand_lifecycle(st2), "stopped")
    line <- id_text(wire$line())
    expect_match(line, "Stopped Slow analysis: 9 items was canceled at", fixed = TRUE)
    expect_match(line, "nothing was kept. The model is not fit yet.", fixed = TRUE)
    expect_false(grepl("Stopped Stopped", line, fixed = TRUE))
    # The next run clears it, and runs to completion once its process ends.
    session$setInputs(run = 2L)
    st3 <- wire$status()
    expect_identical(st3$state, "running")
    expect_null(st3$stopped)
    expect_identical(handles$made, 2L)
    expect_false(grepl("Stopped", id_text(wire$line()), fixed = TRUE))
    handles$last$alive <- FALSE
    session$elapse(500L)
    expect_identical(wire$status()$state, "current")
    expect_match(id_text(wire$line()), "Current Slow analysis: 9 items at", fixed = TRUE)
    # An exclusion starts the refit by itself; canceling it leaves the
    # result from before on the page, marked Stopped.
    sieve_set_item_status(st, shiny::isolate(st$meta)$item[1L], "parked", "stop test")
    session$flushReact()
    st4 <- wire$status()
    expect_identical(st4$state, "running")
    expect_identical(st4$before, "stale")
    expect_identical(sieve_demand_lifecycle(st4), "refitting")
    expect_match(id_text(wire$line()), "Refitting The model is being fit in a separate R process.", fixed = TRUE)
    wire$cancel()
    st5 <- wire$status()
    expect_identical(st5$state, "stale")
    expect_identical(sieve_demand_lifecycle(st5), "stopped")
    line <- id_text(wire$line())
    expect_match(line, "Stopped Slow analysis: 9 items was canceled at", fixed = TRUE)
    expect_match(line, "The result below is from before (Slow analysis: 9 items at", fixed = TRUE)
    # The cancel belongs to the data it was made for: other data never read it.
    expect_identical(st5$stopped$key$dataset, st5$key$dataset)
    st$dataset_id <- "other-data"
    session$flushReact()
    st6 <- wire$status()
    expect_null(st6$stopped)
    expect_false(grepl("Stopped", id_text(wire$line()), fixed = TRUE))
  })
})

test_that("the lifecycle badge uses one vocabulary, and the fitted-to phrase names items and respondents", {
  states <- sieve_lifecycle_states("fit")
  expect_identical(unname(states[c("not_run", "current", "running", "refitting", "stale", "stopped", "failed", "blocked")]),
    c("Not fit yet", "Current", "Running", "Refitting", "Update needed", "Stopped", "Failed", "Waiting on inputs"))
  expect_false("refused" %in% names(states))
  expect_identical(sieve_lifecycle_states("computed")[["not_run"]], "Not computed yet")
  badge <- as.character(sieve_lifecycle_badge("stale"))
  expect_match(badge, 'class="sieve-lifecycle sieve-lifecycle-stale"', fixed = TRUE)
  expect_match(badge, ">Update needed<", fixed = TRUE)
  expect_match(as.character(sieve_lifecycle_badge("made_up")), ">Waiting on inputs<", fixed = TRUE)
  expect_match(as.character(sieve_lifecycle_badge(NULL)), "sieve-lifecycle-blocked", fixed = TRUE)
  expect_identical(sieve_demand_lifecycle(list(state = "running", before = "stale")), "refitting")
  expect_identical(sieve_demand_lifecycle(list(state = "running", before = "not_run")), "running")
  expect_identical(sieve_demand_lifecycle(list(state = "stale", stopped = list(when = "10:00"))), "stopped")
  expect_identical(sieve_demand_lifecycle(list(state = "current", stopped = list(when = "10:00"))), "current")
  expect_identical(sieve_demand_lifecycle(NULL), "blocked")
  expect_identical(sieve_fitted_to_text(c("x1", "x2"), 301), "fitted to the 2 included items (x1, x2) and 301 respondents")
  expect_identical(sieve_fitted_to_text("x1", 1), "fitted to the 1 included item (x1) and 1 respondent")
  expect_identical(sieve_fitted_to_text(paste0("x", 1:9), NULL), "fitted to the 9 included items (x1, x2, x3, x4, x5 and 4 more)")
  expect_identical(sieve_fitted_to_text(character(0), 50, verb = "computed"), "computed from the included items and 50 respondents")
  expect_identical(sieve_fitted_scope_text(c("x1", "x2"), 10), "the 2 included items (x1, x2) and 10 respondents")
  expect_identical(sieve_fitted_scope_text(c("x1", "x2"), 10, included = 3), "2 of the 3 included items (x1, x2) and 10 respondents")
  expect_identical(sieve_fitted_scope_text(c("x1", "x2"), 10, included = 2L), "the 2 included items (x1, x2) and 10 respondents")
  expect_identical(sieve_fitted_scope_text(c("x1", "x2"), NULL, included = 1L), "the 2 included items (x1, x2)")
  expect_identical(sieve_fitted_scope_text(c("x1", "x2"), NULL, included = NA), "the 2 included items (x1, x2)")
  expect_identical(sieve_fitted_scope_text(c("x1", "x2"), NULL, included = c(3L, 4L)), "the 2 included items (x1, x2)")
  expect_identical(sieve_fitted_scope_text(c("x1", "x2"), NULL, included = "7"), "2 of the 7 included items (x1, x2)")
  expect_identical(sieve_fitted_to_text(paste0("x", 1:6), 301, included = 9L), "fitted to 6 of the 9 included items (x1, x2, x3, x4, x5 and 1 more) and 301 respondents")
  expect_identical(sieve_fitted_to_text(character(0), 50, verb = "computed", included = 9L), "computed from the included items and 50 respondents")
  expect_null(sieve_clock_text(NULL))
  expect_null(sieve_clock_text("not a time"))
  expect_match(sieve_clock_text("2026-11-04 13:05:17 UTC"), "^[0-9]{2}:[0-9]{2}$")
  # A time recorded in UTC is shown as the session's clock reads, as the
  # other pages' times are.
  withr::with_timezone("America/New_York", expect_identical(sieve_clock_text("2026-11-04 13:05:17 UTC"), "08:05"))
  withr::with_timezone("UTC", expect_identical(sieve_clock_text("2026-11-04 13:05:17 UTC"), "13:05"))
})

test_that("every page's Update line and the Dimensionality status line carry the badge", {
  st <- id_nine()
  shiny::testServer(function(input, output, session) {
    wire <- sieve_update_wire(input, session, function() list(
      a = sieve_update_entry("Reliability table", state = "stale", sentence = "x1 excluded", when = "10:00", seconds = 2, run = function() NULL)
    ))
  }, {
    line <- id_text(wire$line())
    expect_match(line, "Update needed The reliability table is from before. What changed: x1 excluded.", fixed = TRUE)
  })
  # Dimensionality, before anything is computed: its status line opens with
  # the badge of a result that waits for its button.
  withr::local_options(sieve.auto_run = "never", sieve.cores = 1L)
  shiny::testServer(mod_retention_server, args = list(state = id_nine()), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 20, max_k = 4, seed = 7)
    line <- id_text(output$status_line)
    expect_match(line, "^ ?Not computed yet ")
  })
})

test_that("an item decision does not make the CV and Invariance page announce an empty Update", {
  test_frame <- environment()
  notes <- character(0)
  testthat::local_mocked_bindings(
    sieve_notify = function(message, type = "message", duration = 8) {
      notes <<- c(notes, message)
      invisible(NULL)
    },
    .env = test_frame
  )
  st <- id_nine()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$flushReact()
    sieve_set_item_status(state, shiny::isolate(state$meta)$item[1L], "parked", "quiet test")
    session$flushReact()
    for (i in seq_len(4L)) session$elapse(250L)
    expect_false(any(grepl("Nothing on this page is from before", notes, fixed = TRUE)))
  })
})

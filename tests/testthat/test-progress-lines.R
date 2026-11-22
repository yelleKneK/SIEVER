# The line of a computation running in a background R process is drawn once
# per run, with its Cancel button; only its progress text is redrawn while
# it runs (R/fct_progress_line.R).
#
# The pages covered are the EFA page's status line and its factor count
# panel, the Items page's status line, and the Dimensionality page's status
# line, Scree panel, VSS panel and Model fit by k panel. A running line
# drawn in a renderUI() that redraws itself each second (invalidateLater()),
# or as each count of the model fit by k finishes, would replace the Cancel
# button at every redraw: keyboard focus on it would fall to the page body,
# and a press whose release came after a redraw would reach no button. Each
# test below draws a page's running line, lets the clock tick, and checks
# that the line (with its Cancel) is not drawn again while the progress
# text moves, then that Cancel still cancels.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would change which runs are automatic.
sieve_rate_reset()

pl_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
pl_html <- function(x) as.character(x$html)
pl_count <- function(html, pattern) lengths(regmatches(html, gregexpr(pattern, html, fixed = TRUE)))

# Count the draws of each running line: sieve_progress_text() is called
# once per draw of a line, with the id of the line's progress output.
pl_count_draws <- function(test_frame) {
  drawn <- new.env(parent = emptyenv())
  real <- sieve_progress_text
  testthat::local_mocked_bindings(sieve_progress_text = function(id) {
    drawn[[id]] <- pl_draws(drawn, id) + 1L
    real(id)
  }, .env = test_frame)
  drawn
}
pl_draws <- function(drawn, id) if (exists(id, envir = drawn, inherits = FALSE)) get(id, envir = drawn) else 0L

# A running line holds its Cancel and its progress output once each, and
# not the progress itself.
pl_expect_line <- function(html, cancel_id, progress_id) {
  expect_identical(pl_count(html, sprintf('id="%s"', cancel_id)), 1L)
  expect_identical(pl_count(html, sprintf('id="%s"', progress_id)), 1L)
  expect_match(html, sprintf('<span id="%s" class="shiny-text-output" role="timer" aria-live="off"></span>', progress_id),
    fixed = TRUE)
  expect_false(grepl("Elapsed:", html, fixed = TRUE))
}

pl_efa_state <- function() {
  siever_test_study(sieve_hs9_fixture(), confirmed = TRUE, labels = TRUE, theory = FALSE, name = NULL,
    configure = function(s) {
      s$estimator_choice <- "auto"
      s$estimator <- "MLR with FIML"
      s$theory <- list(k = 3L)
    })
}

test_that("a running line's progress text is a timer whose live region is off, and its outputs tick only while a run goes on", {
  expect_identical(as.character(sieve_progress_text("page-run_progress")),
    '<span id="page-run_progress" class="shiny-text-output" role="timer" aria-live="off"></span>')
  shiny::testServer(function(input, output, session) {
    running <- shiny::reactiveVal(FALSE)
    calls <- new.env(parent = emptyenv())
    calls$n <- 0L
    sieve_progress_outputs(output, c("panel_progress", "line_progress"), function() {
      if (!isTRUE(running())) return(NULL)
      calls$n <- calls$n + 1L
      sprintf("Reading %d.", calls$n)
    }, session)
  }, {
    expect_identical(output$panel_progress, "")
    expect_identical(output$line_progress, "")
    session$elapse(3000)
    expect_identical(calls$n, 0L)
    running(TRUE)
    session$flushReact()
    first <- c(output$panel_progress, output$line_progress)
    expect_true(all(grepl("^Reading [0-9]+[.]$", first)))
    # Each output is redrawn every second, with the same sentence function.
    session$elapse(1000)
    second <- c(output$panel_progress, output$line_progress)
    expect_true(all(grepl("^Reading [0-9]+[.]$", second)))
    expect_false(any(second == first))
    # The run ends: the outputs are empty and nothing is polled.
    running(FALSE)
    session$flushReact()
    expect_identical(output$panel_progress, "")
    expect_identical(output$line_progress, "")
    n <- calls$n
    session$elapse(3000)
    expect_identical(calls$n, n)
  })
})

test_that("the EFA page's selected EFA line is drawn once with its Cancel; each second redraws only the elapsed time, and Cancel still cancels", {
  test_frame <- environment()
  log <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    sieve_efa_dmar_wait = function(...) 10,
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE),
    .env = test_frame
  )
  drawn <- pl_count_draws(test_frame)
  shiny::testServer(mod_efa_server, args = list(state = pl_efa_state()), {
    progress_id <- session$ns("efa_progress")
    cancel_id <- session$ns("efa_cancel")
    session$setInputs(n_factors = 3, rotation = "oblimin", fm = "ml", estimator = "study")
    expect_identical(output$efa_progress, "")
    session$setInputs(efa_go = 1)
    expect_true(isTRUE(efa_run()$running))
    html <- pl_html(output$status_line)
    expect_match(pl_text(output$status_line), "The selected EFA (DMAR::fa_exploratory, ", fixed = TRUE)
    pl_expect_line(html, cancel_id, progress_id)
    expect_match(output$efa_progress, "^Elapsed: [0-9]+ seconds")
    at_start <- pl_draws(drawn, progress_id)
    expect_gte(at_start, 1L)
    for (shift in c(100, 200)) {
      # The start is `shift` seconds before now, so only the one flush below
      # adds real time to the elapsed seconds the line reads.
      primary_job$started_at <- Sys.time() - shift
      session$elapse(1000)
      expect_match(output$efa_progress, sprintf("^Elapsed: %d[0-9] seconds", shift %/% 10))
      expect_identical(pl_html(output$status_line), html)
      expect_identical(pl_draws(drawn, progress_id), at_start)
    }
    # Cancel stops the worker; the line says so, without Cancel, and the
    # progress output is empty from then on.
    session$setInputs(efa_cancel = 1)
    expect_identical(log$killed, 1L)
    expect_false(log$process$alive)
    expect_false(sieve_efa_dmar_active(primary_job))
    expect_match(pl_text(output$status_line), "The selected EFA was canceled before it finished.", fixed = TRUE)
    expect_false(grepl(cancel_id, pl_html(output$status_line), fixed = TRUE))
    expect_identical(output$efa_progress, "")
    session$elapse(2000)
    expect_identical(output$efa_progress, "")
  })
})

test_that("the EFA page's comparison lines are drawn once with their Cancel; each second redraws only the elapsed time, and both Cancels still cancel", {
  test_frame <- environment()
  log <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE), .env = test_frame)
  drawn <- pl_count_draws(test_frame)
  shiny::testServer(mod_efa_server, args = list(state = pl_efa_state()), {
    note_progress <- session$ns("count_progress")
    line_progress <- session$ns("count_progress_line")
    session$setInputs(efa_go = 1, n_factors = 3, count_to = 4, count_criterion = "BIC")
    expect_false(is.null(efa()))
    for (cancel in c("count_cancel", "count_cancel_global")) {
      session$setInputs(count_go = if (identical(cancel, "count_cancel")) 1L else 2L)
      expect_identical(count_run()$status, "running")
      process <- log$process
      note <- pl_html(output$count_note)
      line <- pl_html(output$status_line)
      expect_match(pl_text(output$count_note), "Running in a background R process: k = 1 to 4 under", fixed = TRUE)
      expect_match(pl_text(output$status_line), "The number of factors comparison (DMAR::factor_count, k = 1 to 4)", fixed = TRUE)
      # The panel's line and the status line, each with its own Cancel and
      # its own progress output.
      pl_expect_line(note, session$ns("count_cancel"), note_progress)
      pl_expect_line(line, session$ns("count_cancel_global"), line_progress)
      expect_false(grepl(session$ns("count_cancel_global"), note, fixed = TRUE))
      expect_false(grepl(sprintf('id="%s"', session$ns("count_cancel")), line, fixed = TRUE))
      at_start <- c(pl_draws(drawn, note_progress), pl_draws(drawn, line_progress))
      expect_true(all(at_start >= 1L))
      for (shift in c(100, 200)) {
        # The start is `shift` seconds before now, so only the one flush below
        # adds real time to the elapsed seconds the line reads.
        count_job$started_at <- Sys.time() - shift
        session$elapse(1000)
        expect_match(output$count_progress, sprintf("^Elapsed: %d[0-9] seconds", shift %/% 10))
        expect_match(output$count_progress_line, sprintf("^Elapsed: %d[0-9] seconds", shift %/% 10))
        expect_identical(pl_html(output$count_note), note)
        expect_identical(pl_html(output$status_line), line)
        expect_identical(c(pl_draws(drawn, note_progress), pl_draws(drawn, line_progress)), at_start)
      }
      killed <- log$killed
      do.call(session$setInputs, stats::setNames(list(1L), cancel))
      expect_identical(log$killed, killed + 1L)
      expect_false(process$alive)
      expect_false(sieve_efa_dmar_active(count_job))
      expect_match(pl_text(output$count_note), "Canceled.", fixed = TRUE)
      expect_identical(output$count_progress, "")
      expect_identical(output$count_progress_line, "")
    }
  })
})

test_that("the Items page's factor analysis line is drawn once with its Cancel; each second redraws only the elapsed time, and Cancel still cancels", {
  withr::local_options(sieve.cores = 1L)
  test_frame <- environment()
  st <- siever_test_study(sieve_hs9_fixture())
  log <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE),
    sieve_efa_point_wait = function(p, k, form = "matrix", refused = FALSE) rep(5, length(k)),
    withProgress = function(expr, ...) expr,
    .env = test_frame
  )
  drawn <- pl_count_draws(test_frame)
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    progress_id <- session$ns("efa_progress")
    cancel_id <- session$ns("efa_cancel")
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    expect_identical(diag_run()$status, "not run")
    session$setInputs(update_go = 1L)
    expect_identical(diag_run()$status, "running")
    html <- pl_html(output$status_line)
    expect_match(pl_text(output$status_line), "(DMAR::fa_exploratory, maximum likelihood, k = 3, 9 items) is running in a background R process",
      fixed = TRUE)
    pl_expect_line(html, cancel_id, progress_id)
    expect_match(output$efa_progress, "^Elapsed: [0-9]+ seconds")
    at_start <- pl_draws(drawn, progress_id)
    expect_gte(at_start, 1L)
    for (shift in c(100, 200)) {
      # The start is `shift` seconds before now, so only the one flush below
      # adds real time to the elapsed seconds the line reads.
      efa_bg$started_at <- Sys.time() - shift
      session$elapse(1000)
      expect_match(output$efa_progress, sprintf("^Elapsed: %d[0-9] seconds", shift %/% 10))
      expect_identical(pl_html(output$status_line), html)
      expect_identical(pl_draws(drawn, progress_id), at_start)
    }
    session$setInputs(efa_cancel = 1L)
    expect_identical(log$killed, 1L)
    expect_false(log$process$alive)
    expect_true(isTRUE(diag_run()$stopped))
    expect_match(pl_text(output$status_line), "Canceled: the exploratory factor analysis was stopped before it finished", fixed = TRUE)
    expect_false(grepl(cancel_id, pl_html(output$status_line), fixed = TRUE))
    expect_identical(output$efa_progress, "")
  })
})

test_that("the Dimensionality page's parallel analysis lines are drawn once with their Cancel; each second redraws only the elapsed time, and both Cancels still cancel", {
  test_frame <- environment()
  st <- siever_test_study(sieve_hs9_fixture(), labels = FALSE)
  log <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE), .env = test_frame)
  drawn <- pl_count_draws(test_frame)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    panel_progress <- session$ns("pa_progress")
    line_progress <- session$ns("pa_progress_line")
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 3000, max_k = 3, seed = 11)
    for (cancel in c("pa_stop", "update_stop")) {
      session$setInputs(pa_go = if (identical(cancel, "pa_stop")) 1L else 2L)
      expect_true(pa_running())
      process <- log$process
      panel <- pl_html(output$pa_control)
      line <- pl_html(output$status_line)
      expect_match(pl_text(output$pa_control), "Parallel analysis (DMAR::parallel_analysis) is running in a background R process.", fixed = TRUE)
      expect_match(pl_text(output$status_line), "Parallel analysis (DMAR::parallel_analysis) is running in a background R process.", fixed = TRUE)
      pl_expect_line(panel, session$ns("pa_stop"), panel_progress)
      pl_expect_line(line, session$ns("update_stop"), line_progress)
      at_start <- c(pl_draws(drawn, panel_progress), pl_draws(drawn, line_progress))
      expect_true(all(at_start >= 1L))
      for (shift in c(100, 200)) {
        # The start is `shift` seconds before now, so only the one flush below
        # adds real time to the elapsed seconds the line reads.
        pa_bg$started_at <- Sys.time() - shift
        session$elapse(1000)
        expect_match(output$pa_progress, sprintf("^Elapsed: %d[0-9] seconds", shift %/% 10))
        expect_match(output$pa_progress_line, sprintf("^Elapsed: %d[0-9] seconds", shift %/% 10))
        expect_identical(pl_html(output$pa_control), panel)
        expect_identical(pl_html(output$status_line), line)
        expect_identical(c(pl_draws(drawn, panel_progress), pl_draws(drawn, line_progress)), at_start)
      }
      killed <- log$killed
      do.call(session$setInputs, stats::setNames(list(1L), cancel))
      expect_identical(log$killed, killed + 1L)
      expect_false(process$alive)
      expect_false(pa_running())
      expect_match(pa_run()$reason, "^Canceled: the run was stopped before it finished")
      expect_identical(output$pa_progress, "")
      expect_identical(output$pa_progress_line, "")
    }
  })
})

test_that("the Dimensionality page's VSS lines are drawn once, each with a Cancel of its own id; each second redraws only the elapsed time, and both Cancels still cancel", {
  test_frame <- environment()
  st <- siever_test_study(sieve_hs9_fixture(), labels = FALSE)
  log <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE),
    sieve_vss_wait = function(max_k, p, R = NULL) 30,
    .env = test_frame
  )
  drawn <- pl_count_draws(test_frame)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    panel_progress <- session$ns("vss_progress")
    line_progress <- session$ns("vss_progress_line")
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 20, max_k = 3, seed = 11)
    # The status line and the VSS panel each draw a Cancel VSS, under
    # different ids, so the page never holds two inputs of one id.
    for (cancel in c("vss_stop_panel", "vss_stop")) {
      session$setInputs(vss_go = if (identical(cancel, "vss_stop_panel")) 1L else 2L)
      expect_true(vss_running())
      process <- log$process
      panel <- pl_html(output$mapvss_note)
      line <- pl_html(output$status_line)
      expect_match(pl_text(output$mapvss_note), "Very Simple Structure (DMAR::fa_exploratory at each k) is running in a background R process.", fixed = TRUE)
      expect_match(pl_text(output$status_line), "Very Simple Structure (DMAR::fa_exploratory at each k) is running in a background R process.", fixed = TRUE)
      pl_expect_line(panel, session$ns("vss_stop_panel"), panel_progress)
      pl_expect_line(line, session$ns("vss_stop"), line_progress)
      expect_false(grepl(sprintf('id="%s"', session$ns("vss_stop")), panel, fixed = TRUE))
      expect_false(grepl(session$ns("vss_stop_panel"), line, fixed = TRUE))
      at_start <- c(pl_draws(drawn, panel_progress), pl_draws(drawn, line_progress))
      expect_true(all(at_start >= 1L))
      for (shift in c(100, 200)) {
        # The start is `shift` seconds before now, so only the one flush below
        # adds real time to the elapsed seconds the line reads.
        vss_bg$started_at <- Sys.time() - shift
        session$elapse(1000)
        expect_match(output$vss_progress, sprintf("^Elapsed: %d[0-9] seconds", shift %/% 10))
        expect_match(output$vss_progress_line, sprintf("^Elapsed: %d[0-9] seconds", shift %/% 10))
        expect_identical(pl_html(output$mapvss_note), panel)
        expect_identical(pl_html(output$status_line), line)
        expect_identical(c(pl_draws(drawn, panel_progress), pl_draws(drawn, line_progress)), at_start)
      }
      do.call(session$setInputs, stats::setNames(list(1L), cancel))
      expect_false(process$alive)
      expect_false(vss_running())
      expect_match(vss_run()$reason, "^Canceled: Very Simple Structure was stopped before it finished")
      expect_identical(output$vss_progress, "")
      expect_identical(output$vss_progress_line, "")
    }
  })
})

# A worker of the model fit by k that stays alive until the test ends it,
# and then fits its counts with the production engine.
pl_fit_start <- function(log) {
  function(data, key, k) {
    p <- new.env(parent = emptyenv())
    p$alive <- TRUE
    p$killed <- FALSE
    p$is_alive <- function() p$alive
    p$kill <- function() {
      p$alive <- FALSE
      p$killed <- TRUE
      invisible(TRUE)
    }
    p$read_progress <- function() list()
    p$get_result <- function() lapply(k, function(kk) sieve_efa_fit_by_k(data, key$estimator, key$ordered,
      rotation = key$rotation, ks = kk, missing = key$missing))
    log$processes <- c(log$processes, list(p))
    p
  }
}

test_that("the Dimensionality page's model fit by k lines are drawn once with their Cancel; a finished count and each second redraw only the progress, and both Cancels still cancel", {
  withr::local_options(sieve.cores = 2L)
  test_frame <- environment()
  log <- new.env(parent = emptyenv())
  log$processes <- list()
  testthat::local_mocked_bindings(
    sieve_fit_wait = function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L, ...) 20,
    withProgress = function(expr, ...) expr,
    sieve_efa_worker_start = pl_fit_start(log),
    .env = test_frame
  )
  drawn <- pl_count_draws(test_frame)
  st <- siever_test_study(sieve_hs9_fixture(), confirmed = TRUE, labels = TRUE, theory = TRUE,
    configure = function(s) {
      s$estimator_choice <- "auto"
      s$estimator <- "MLR with FIML"
    })
  shiny::testServer(mod_retention_server, args = list(state = st), {
    panel_progress <- session$ns("fit_progress")
    line_progress <- session$ns("fit_progress_line")
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 10, max_k = 4, seed = 7, workers = 2, estimator = "study"
    )
    expect_identical(fit_run()$status, "not run")
    session$setInputs(fit_go = 1)
    expect_true(isTRUE(fit_run()$running))
    expect_length(log$processes, 2L)
    panel <- pl_html(output$fit_control)
    line <- pl_html(output$status_line)
    expect_match(pl_text(output$fit_control), "The model fit by k is running: 4 exploratory models (k = 1 to 4)", fixed = TRUE)
    expect_match(pl_text(output$status_line), "The model fit by k is running in the background:", fixed = TRUE)
    pl_expect_line(panel, session$ns("fit_cancel"), panel_progress)
    pl_expect_line(line, session$ns("update_cancel"), line_progress)
    expect_match(output$fit_progress, "^0 of 4 complete; k = 1, 2, 3, 4 running on 2 workers; 0 queued. Elapsed: [0-9]+ seconds.")
    expect_identical(output$fit_progress_line, "0 of 4 complete; k = 1, 2, 3, 4 running on 2 workers.")
    at_start <- c(pl_draws(drawn, panel_progress), pl_draws(drawn, line_progress))
    expect_true(all(at_start >= 1L))
    # A worker finishes its counts: the progress says so; neither line,
    # nor its Cancel, is drawn again.
    log$processes[[1L]]$alive <- FALSE
    for (i in 1:10) {
      session$elapse(200)
      if (grepl("^2 of 4", output$fit_progress_line)) break
    }
    expect_true(isTRUE(fit_run()$running))
    expect_match(output$fit_progress, "^2 of 4 complete; k = 3, 4 running on 1 worker; 0 queued.")
    expect_identical(output$fit_progress_line, "2 of 4 complete; k = 3, 4 running on 1 worker.")
    session$elapse(1000)
    expect_identical(pl_html(output$fit_control), panel)
    expect_identical(pl_html(output$status_line), line)
    expect_identical(c(pl_draws(drawn, panel_progress), pl_draws(drawn, line_progress)), at_start)
    # Cancel on the panel stops the worker still running.
    session$setInputs(fit_cancel = 1)
    expect_true(log$processes[[2L]]$killed)
    expect_false(isTRUE(fit_run()$running))
    expect_identical(fit_run()$status, "not run")
    expect_false(grepl(session$ns("fit_cancel"), pl_html(output$fit_control), fixed = TRUE))
    expect_identical(output$fit_progress, "")
    expect_identical(output$fit_progress_line, "")
    # The status line's Cancel stops a second run.
    session$setInputs(fit_go = 2)
    expect_true(isTRUE(fit_run()$running))
    second <- log$processes[[length(log$processes)]]
    expect_match(output$fit_progress_line, "^[0-4] of 4 complete; k = [0-9, ]+ running on [12] workers?[.]$")
    session$setInputs(update_cancel = 1)
    expect_true(second$killed)
    expect_false(isTRUE(fit_run()$running))
    expect_identical(output$fit_progress_line, "")
  })
})

test_that("a selected EFA started by Update shows its time and is collected when it ends", {
  # update_current() must wake the page's poll observer and the line's
  # progress text (both read primary_tick()) when it starts the background
  # job; otherwise the time so far stays empty and, when only the loadings
  # are updated, the finished job is never collected.
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  test_frame <- environment()
  st <- siever_test_study(sieve_hs9_fixture(), confirmed = TRUE, labels = TRUE, theory = FALSE, name = NULL,
    configure = function(s) {
      s$estimator_choice <- "auto"
      s$estimator <- "MLR with FIML"
      s$theory <- list(k = 3L)
    })
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(
    sieve_efa_dmar_wait = function(...) 10,
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE),
    .env = test_frame
  )
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(n_factors = 3, rotation = "oblimin", fm = "ml", estimator = "study")
    session$setInputs(efa_go = 1)
    log$process$alive <- FALSE
    efa_dmar_settle(session)
    expect_false(is.null(efa_run()$fit))
    session$setInputs(rotation = "geomin")
    expect_true(isTRUE(update_plan()$primary))
    session$setInputs(update_go = 1)
    expect_true(sieve_efa_dmar_active(primary_job))
    first <- output$efa_progress
    expect_match(first, "^Elapsed")
    primary_job$started_at <- primary_job$started_at - 50
    session$elapse(1000)
    expect_false(identical(output$efa_progress, first))
    log$process$alive <- FALSE
    efa_dmar_settle(session)
    expect_false(sieve_efa_dmar_active(primary_job))
    expect_false(is.null(efa_run()$fit))
    expect_false(isTRUE(efa_run()$running))
  })
})

# Exact selected EFA requests and genuinely separate background workers.
# The EFA page's selected solution is
# DMAR::fa_exploratory() (in a background R process when it is estimated
# above two seconds) and its number of factors panel is DMAR::factor_count()
# (always in a background R process); the lavaan::efa fit-by-k queue these
# tests also cover serves the Dimensionality page.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

bg_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = FALSE, name = NULL, configure = function(s) {
  s$estimator_choice <- "auto"
  s$estimator <- "MLR with FIML"
  s$theory <- list(k = 3L)
  })
  s
}

bg_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))

test_that("unsupported primary counts are refused instead of rounded or capped", {
  expect_null(sieve_efa_k_problem(3, 9))
  for (k in list(0, -1, 1.5, NA_real_, Inf, "3")) {
    expect_match(sieve_efa_k_problem(k, 9), "whole number")
  }
  expect_match(sieve_efa_k_problem(8, 9), "requested k = 8.*At most 5")
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log))
  shiny::testServer(mod_efa_server, args = list(state = bg_state()), {
    session$setInputs(efa_go = 1, n_factors = 3, rotation = "oblimin")
    expect_identical(ncol(unclass(efa()$loadings)), 3L)
    expect_match(paste(shiny::isolate(state$codelog), collapse = "\n"), "nfactors = 3", fixed = TRUE)
    session$setInputs(efa_go = 2, n_factors = 8)
    expect_null(efa())
    # output$status_line is the page's one status line, with the caveat on
    # the displayed solution.
    expect_match(bg_text(output$status_line), "requested k = 8", fixed = TRUE)
    session$setInputs(efa_go = 3, n_factors = 2.5)
    expect_null(efa())
    expect_match(bg_text(output$status_line), "whole number", fixed = TRUE)
    # Nothing was sent to DMAR for an unsupported count.
    expect_length(log$calls, 0L)
  })
})

test_that("a selected EFA estimated above two seconds runs in a background process with progress and its own result", {
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(
    sieve_efa_dmar_wait = function(...) 10,
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE)
  )
  shiny::testServer(mod_efa_server, args = list(state = bg_state()), {
    session$setInputs(n_factors = 3, rotation = "oblimin", fm = "ml", estimator = "study")
    # Ten seconds is above the two that run by themselves: the button waits.
    expect_true(isTRUE(efa_run()$pending))
    expect_match(bg_text(output$status_line), "Run selected EFA (roughly", fixed = TRUE)
    session$setInputs(efa_go = 1)
    first <- efa_run()
    expect_true(isTRUE(first$running))
    expect_null(efa())
    expect_length(log$calls, 1L)
    expect_identical(log$calls[[1L]]$fun, "fa_exploratory")
    line <- bg_text(output$status_line)
    expect_match(line, "is running in a background R process", fixed = TRUE)
    # The elapsed time is the line's own text output (the line is drawn
    # once per run, test-progress-lines.R).
    expect_match(output$efa_progress, "Elapsed:", fixed = TRUE)
    expect_match(line, "Cancel", fixed = TRUE)
    expect_identical(shiny::isolate(state$page_results$efa[[session$ns("primary")]]$state), "running")
    # A display setting neither restarts nor stops it.
    session$setInputs(suppress = 0.45)
    expect_true(isTRUE(efa_run()$running))
    expect_length(log$calls, 1L)
    expect_identical(log$killed, 0L)
    # The process finishes: the page reads its result for this request.
    log$process$alive <- FALSE
    efa_dmar_settle(session)
    fit <- efa()
    expect_false(is.null(fit))
    expect_identical(ncol(unclass(fit$loadings)), 3L)
    expect_identical(shiny::isolate(state$page_results$efa[[session$ns("primary")]]$state), "current")
    # The same call in this process gives the same solution.
    direct <- sieve_efa_dmar_solution(do.call(DMAR::fa_exploratory, log$calls[[1L]]$args))
    expect_equal(fit$loadings, direct$loadings, tolerance = 1e-12)
    expect_equal(fit$Phi, direct$Phi, tolerance = 1e-12)
    expect_length(log$calls, 1L)
  })
})

test_that("cancel and a changed scientific specification kill the selected EFA's worker", {
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(
    sieve_efa_dmar_wait = function(...) 10,
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE)
  )
  shiny::testServer(mod_efa_server, args = list(state = bg_state()), {
    session$setInputs(efa_go = 1, n_factors = 3)
    expect_true(isTRUE(efa_run()$running))
    first <- log$process
    session$setInputs(efa_cancel = 1)
    expect_false(first$alive)
    expect_identical(log$killed, 1L)
    expect_false(sieve_efa_dmar_active(primary_job))
    stopped <- efa_run()
    expect_true(isTRUE(stopped$pending))
    expect_true(isTRUE(stopped$stopped))
    expect_match(bg_text(output$status_line), "canceled before it finished", fixed = TRUE)
    # Waiting does not restart a request the user stopped.
    efa_dmar_settle(session)
    expect_length(log$calls, 1L)
    session$setInputs(efa_go = 2)
    expect_true(isTRUE(efa_run()$running))
    second <- log$process
    expect_length(log$calls, 2L)
    # A changed scientific input stops the process fitting the old request;
    # its result can never be shown for the new one.
    session$setInputs(estimator = "ml")
    efa_dmar_settle(session)
    expect_false(second$alive)
    expect_identical(log$killed, 2L)
    expect_null(efa())
    expect_true(isTRUE(efa_run()$pending))
    session$setInputs(efa_go = 3)
    log$process$alive <- FALSE
    efa_dmar_settle(session)
    expect_identical(efa_run()$route$estimator, "ML")
    expect_false(is.null(efa()))
    expect_length(log$calls, 3L)
  })
})

test_that("a real comparison worker returns immediately and is separate from this R session", {
  skip_on_cran()
  skip_if_not_installed("callr")
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  key <- list(estimator = "MLR", ordered = NULL, rotation = "none", missing = "fiml", ks = 2L)
  started <- proc.time()[["elapsed"]]
  p <- sieve_efa_worker_start(d, key, 2L)
  withr::defer(if (p$is_alive()) p$kill())
  if (siever_timing_checks()) expect_lt(proc.time()[["elapsed"]] - started, 3)
  # The start returns while the worker is still starting R and fitting.
  expect_true(p$is_alive())
  expect_false(identical(p$get_pid(), Sys.getpid()))
  siever_wait_until(function() !p$is_alive(), "The comparison worker")
  out <- p$get_result()
  expect_identical(out$k, 2L)
  expected <- sieve_efa_fit_by_k(d, "MLR", rotation = "none", ks = 2L, missing = "fiml")
  expect_equal(out, expected, tolerance = 1e-8)
})


test_that("unsupported background execution is a failure of the request with its reason", {
  local_mocked_bindings(
    sieve_efa_dmar_wait = function(...) 10,
    sieve_efa_dmar_start = function(...) stop("process spawning is disabled")
  )
  shiny::testServer(mod_efa_server, args = list(state = bg_state()), {
    session$setInputs(efa_go = 1, n_factors = 3)
    r <- efa_run()
    expect_null(r$fit)
    expect_match(r$refused, "Background execution unavailable: process spawning is disabled", fixed = TRUE)
    expect_match(bg_text(output$status_line), "process spawning is disabled", fixed = TRUE)
    expect_identical(shiny::isolate(state$page_results$efa[[session$ns("primary")]]$state), "failed")
  })
})

test_that("exported EFA code keeps unavailable common-family indices unavailable", {
  key <- list(ks = 3L, estimator = "MLR", rotation = "none", ordered = NULL, lavaan_missing = "listwise")
  code <- sieve_efa_fit_code(key, family = NA_character_)
  env <- new.env(parent = baseenv())
  env$active_items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  suppressWarnings(capture.output(eval(parse(text = code), env)))
  expect_true(all(is.na(env$efa_fit_indices[c("cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"), ])))
  expect_true(is.finite(env$efa_fit_indices["srmr_bentler_nomean", ]))
})


test_that("opening a project restores EFA settings without executing engines", {
  source <- bg_state()
  # Saved settings that name principal axis and promax, which have no DMAR
  # route: the page says so.
  source$efa_settings <- list(n_factors = 3L, fm = "pa", rotation = "promax", fit_scope = "range", fit_from = 1L, fit_to = 4L)
  restored <- sieve_new_state()
  sieve_project_restore(restored, sieve_project_snapshot(source))
  counts <- new.env(parent = emptyenv())
  counts$primary <- 0L
  counts$basis <- 0L
  original_fa <- DMAR::fa_exploratory
  original_basis <- sieve_state_basis
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(
    fa_exploratory = function(...) {
      counts$primary <- counts$primary + 1L
      original_fa(...)
    },
    .package = "DMAR"
  )
  local_mocked_bindings(
    sieve_state_basis = function(...) {
      counts$basis <- counts$basis + 1L
      original_basis(...)
    },
    sieve_efa_dmar_start = efa_dmar_fake_start(log)
  )
  shiny::testServer(mod_efa_server, args = list(state = restored), {
    session$flushReact()
    expect_match(bg_text(output$method_note), "principal axis factoring extraction, which DMAR does not offer", fixed = TRUE)
    # (Promax opens as direct oblimin, equamax as varimax, and the line says
    # so.)
    expect_match(bg_text(output$rotation_note), "The saved settings chose promax rotation (oblique), which SIEVER does not offer, since DMAR::fa_exploratory() does not; direct oblimin (delta = 0), the standard oblique rotation, is in its place", fixed = TRUE)
    session$setInputs(n_factors = 3, fm = "ml", rotation = "oblimin")
    expect_null(efa())
    expect_identical(count_run()$status, "not run")
    # The page's one status line (output$status_line) holds the gate and
    # the job status, and carries the Run selected EFA button.
    expect_match(bg_text(output$status_line), "Run selected EFA", fixed = TRUE)
    expect_identical(counts$primary, 0L)
    expect_identical(counts$basis, 0L)
    expect_length(log$calls, 0L)
    session$setInputs(efa_go = 1)
    expect_identical(ncol(unclass(efa()$loadings)), 3L)
    expect_identical(counts$primary, 1L)
    # The factor count comparison waits for its own button.
    expect_identical(count_run()$status, "not run")
    expect_length(log$calls, 0L)
    session$setInputs(n_factors = 2)
    expect_null(efa())
    expect_identical(counts$primary, 1L)
    session$setInputs(efa_go = 2)
    expect_identical(ncol(unclass(efa()$loadings)), 2L)
    expect_identical(counts$primary, 2L)
    # The minres matrix route is the only one that asks for the shared basis.
    expect_identical(counts$basis, 0L)
  })
})

test_that("a cached failed comparison stays failed when another range is requested", {
  started <- integer(0)
  factory <- function(data, key, k) {
    started <<- c(started, k)
    # Controlled engine outcomes test queue bookkeeping independently of
    # numerical estimation, which is covered by the direct-engine tests.
    list(
      is_alive = function() FALSE, kill = function() invisible(TRUE),
      get_result = function() data.frame(
        k = k,
        status = if (k == 2L) "failed" else "ok",
        message = if (k == 2L) "Controlled comparison failure" else "Converged"
      )
    )
  }
  jobs <- sieve_efa_jobs(factory)
  key <- list(data = "same responses", estimator = "MLR", rotation = "none", ks = 2:3, selected_k = 3L)
  sieve_efa_jobs_submit(jobs, key, data.frame(x = 1:3), 2L)
  sieve_efa_jobs_poll(jobs)
  expect_identical(jobs$failed, 2L)
  expect_identical(jobs$completed, integer(0))
  expanded <- key
  expanded$ks <- 1:3
  sieve_efa_jobs_submit(jobs, expanded, data.frame(x = 1:3), 1:2)
  expect_identical(jobs$failed, 2L)
  expect_identical(jobs$completed, integer(0))
  expect_identical(sieve_efa_jobs_running(jobs), 1L)
  expect_match(jobs$errors, "k = 2: Controlled comparison failure", fixed = TRUE)
  sieve_efa_jobs_poll(jobs)
  expect_identical(jobs$completed, 1L)
  expect_identical(jobs$failed, 2L)
  expect_identical(started, c(2L, 1L))
  expect_identical(sieve_efa_job_cache(jobs, expanded, 2L)$status, "failed")
})

test_that("the number of factors comparison runs in the background and a changed request stops it", {
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE))
  shiny::testServer(mod_efa_server, args = list(state = bg_state()), {
    session$setInputs(efa_go = 1, n_factors = 3, count_to = 4, count_criterion = "BIC")
    expect_false(is.null(efa()))
    expect_identical(count_run()$status, "not run")
    expect_match(bg_text(output$count_note), "Compare numbers of factors (", fixed = TRUE)
    session$setInputs(count_go = 1)
    expect_identical(count_run()$status, "running")
    first <- log$process
    expect_identical(log$calls[[1L]]$fun, "factor_count")
    expect_identical(log$calls[[1L]]$args$max_factors, 4L)
    # B = 0: the parallel analysis is the
    # Dimensionality page's, computed once (test-dmar-parallel.R).
    expect_identical(log$calls[[1L]]$args$B, 0L)
    expect_match(bg_text(output$status_line), "Cancel the comparison", fixed = TRUE)
    expect_match(bg_text(output$status_line), "k = 1 to 4", fixed = TRUE)
    # A display setting leaves it running.
    session$setInputs(suppress = .4)
    efa_dmar_settle(session)
    expect_true(first$alive)
    # A changed largest count is a changed request: the old one is stopped
    # and the new one waits for its button.
    session$setInputs(count_to = 3)
    efa_dmar_settle(session)
    expect_false(first$alive)
    expect_identical(count_run()$status, "not run")
    session$setInputs(count_go = 2)
    expect_identical(count_run()$status, "running")
    log$process$alive <- FALSE
    efa_dmar_settle(session)
    done <- count_run()
    expect_identical(done$status, "ran")
    expect_identical(done$table$factors, 1:3)
    expect_identical(shiny::isolate(state$page_results$efa[[session$ns("factor_count")]]$state), "current")
    # The selected count is marked, and no count is chosen.
    expect_match(bg_text(output$count_lines), "No cutoff is applied and no count is chosen", fixed = TRUE)
    expect_length(log$calls, 2L)
  })
})

test_that("stale inputs name their cause and worker transport wrappers expose the parent cause", {
  old <- list(data = "old", p = 9L, estimator = "MLR", missing = "listwise")
  new <- old
  new$estimator <- "ML"
  expect_match(sieve_efa_changed_reason(old, new), "the estimator changed")
  expect_match(sieve_efa_changed_reason(old, new), "reuses cached rows")
  wrapped <- structure(list(
    message = "! in callr subprocess", call = NULL,
    parent = simpleError("A controlled model cause")
  ), class = c("error", "condition"))
  expect_identical(sieve_efa_worker_cause(wrapped), "A controlled model cause")
  expect_match(sieve_efa_worker_cause(simpleError("callr subprocess crashed or was killed")), "out of memory")
})

test_that("a changed estimator makes a computed comparison from before, and Update recomputes it", {
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log))
  shiny::testServer(mod_efa_server, args = list(state = bg_state()), {
    session$setInputs(efa_go = 1, n_factors = 3, count_to = 3, count_criterion = "BIC")
    session$setInputs(count_go = 1)
    efa_dmar_settle(session)
    expect_identical(count_run()$status, "ran")
    session$setInputs(estimator = "ml")
    expect_identical(count_status()$state, "stale")
    expect_match(bg_text(output$status_line), "factor count comparison were computed|number of factors comparison was computed")
    expect_match(bg_text(output$status_line), "Update (", fixed = TRUE)
    session$setInputs(update_go = 1)
    efa_dmar_settle(session)
    expect_identical(count_run()$status, "ran")
    expect_identical(count_run()$route$estimator, "ML")
    expect_identical(efa_run()$route$estimator, "ML")
    expect_length(log$calls, 2L)
  })
})

test_that("primary EFA records a local rotation stream and replays under another RNG kind", {
  withr::local_seed(312)
  withr::defer(suppressWarnings(do.call(RNGkind, as.list(c("Mersenne-Twister", "Inversion", "Rejection")))))
  before_seed <- .Random.seed
  sieve_draw_seed()
  expected_after_draw <- .Random.seed
  assign(".Random.seed", before_seed, envir = globalenv())
  shiny::testServer(mod_efa_server, args = list(state = bg_state()), {
    session$setInputs(n_factors = 3, rotation = "oblimin", fm = "ml", estimator = "mlr", efa_go = 1)
    fit <- efa()
    stopifnot(!is.null(fit))
    recorded <- efa_run()$rng
    # DMAR sets the seed locally and restores the stream: the session's
    # stream moved by the one draw that chose the seed.
    expect_identical(.Random.seed, expected_after_draw)
    original_seed <- recorded$seed
    session$setInputs(suppress = .45)
    expect_identical(efa_run()$rng$seed, original_seed)
    script <- sieve_replay_script(state)
    suppressWarnings(RNGkind("L'Ecuyer-CMRG", "Box-Muller", "Rejection"))
    set.seed(901)
    before_replay <- .Random.seed
    before_kind <- RNGkind()
    env <- new.env(parent = baseenv())
    invisible(capture.output(eval(parse(text = script), env)))
    expect_identical(RNGkind(), before_kind)
    expect_identical(.Random.seed, before_replay)
    expect_equal(env$pattern, unclass(fit$loadings), tolerance = 1e-12)
    expect_match(script, sprintf("seed = %d", original_seed), fixed = TRUE)
    expect_match(script, "Rotation starts: seed", fixed = TRUE)
  })
})

test_that("the imported worker transport is used without an optional-package gate", {
  test_frame <- environment()
  calls <- 0L
  local_mocked_bindings(sieve_have = function(pkg) if (identical(pkg, "callr")) FALSE else requireNamespace(pkg, quietly = TRUE), .env = test_frame)
  local_mocked_bindings(r_bg = function(...) {
    calls <<- calls + 1L
    list(is_alive = function() FALSE, kill = function() invisible(TRUE), get_pid = function() 123L)
  }, .package = "callr", .env = test_frame)
  worker <- sieve_efa_worker_start(data.frame(x = 1:3), list(), 1L)
  withr::defer(worker$kill())
  expect_identical(calls, 1L)
  expect_identical(worker$get_pid(), 123L)
})

test_that("exploratory waits use eligible observations and actual factor counts", {
  local_measured_rates()
  baseline <- sieve_fit_wait(66, "MLR", "efa", n = 500, factors = 1)
  larger <- sieve_fit_wait(66, "MLR", "efa", n = 1000, factors = 5)
  # Independently observed timings are approximately 33.1 and 49.8
  # seconds. Startup is included in the UI prediction, so allow ten percent.
  expect_lt(abs(baseline - 33.1) / 33.1, .10)
  expect_lt(abs(larger - 49.8) / 49.8, .10)
  expect_gt(sieve_fit_wait(66, "MLR", "efa", n = 1000, factors = 1), baseline)
  expect_gt(sieve_fit_wait(66, "MLR", "efa", n = 500, factors = 5), baseline)
  expect_equal(
    sieve_fit_wait(66, "MLR", "efa", n = 500, factors = c(1, 5)),
    sum(vapply(c(1, 5), function(k) sieve_fit_wait(66, "MLR", "efa", n = 500, factors = k), numeric(1)))
  )
  expect_lt(sieve_fit_wait(9, "MLR", "efa", n = 301, factors = 3), 2)
  expect_match(sieve_efa_wait_label(larger), "roughly.*to")
  expect_match(sieve_efa_wait_label(larger), "seconds|minutes")
  key <- list(p = 66L, n = 500L, estimator = "MLR")
  timing <- sieve_efa_time_note(key, 1L, 50)
  expect_equal(timing$observed_seconds, 50)
  expect_equal(sieve_fit_wait(66, "MLR", "efa", n = 500, factors = 1), 50)
  expect_gt(sieve_fit_wait(66, "MLR", "efa", n = 1000, factors = 5), 50)
  # Recording time cannot change the scientific cache identity.
  a <- list(data = "immutable values", p = 9L, n = 301L, estimator = "MLR", ks = 1:3)
  b <- a
  b$n <- NULL
  expect_identical(sieve_efa_job_key(a, 2L), sieve_efa_job_key(b, 2L))
})

test_that("accepted real worker time calibrates the next estimate without changing its table", {
  skip_on_cran()
  skip_if_not_installed("callr")
  local_measured_rates()
  data <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  key <- list(
    data = sieve_data_key(data), p = ncol(data), n = nrow(data),
    estimator = "MLR", ordered = NULL, rotation = "none", missing = "fiml", ks = 2L
  )
  jobs <- sieve_efa_jobs()
  withr::defer(sieve_efa_jobs_cancel(jobs))
  sieve_efa_jobs_submit(jobs, key, data, 2L)
  siever_wait_until(function() is.null(jobs$process), "The EFA worker", step = function() {
    Sys.sleep(.02)
    sieve_efa_jobs_poll(jobs)
  })
  expect_length(jobs$timings, 1L)
  timing <- jobs$timings[[1L]]
  expect_gt(timing$observed_seconds, 0)
  expect_equal(sieve_fit_wait(9, "MLR", "efa", n = nrow(data), factors = 2), timing$observed_seconds)
  table <- sieve_efa_job_cache(jobs, key, 2L)
  expect_identical(table$k, 2L)
  expect_false("timings" %in% names(attributes(table)))
})

test_that("Cancel prevents a queued inclusion refit from restarting EFA workers", {
  for (cancel_id in c("count_cancel", "count_cancel_global")) {
    test_frame <- environment()
    log <- new.env(parent = emptyenv())
    local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE), .env = test_frame)
    withr::local_options(sieve.auto_run = "never")
    state <- bg_state()
    shiny::testServer(mod_efa_server, args = list(state = state), {
      session$setInputs(n_factors = 3L, rotation = "oblimin", count_to = 3L, count_criterion = "BIC", efa_go = 1L, count_go = 1L)
      count_run()
      expect_true(sieve_efa_dmar_active(count_job))
      item <- state$meta$item[1L]
      sieve_set_item_status(state, item, "parked", "Cancel test")
      session$flushReact()
      do.call(session$setInputs, stats::setNames(list(1L), cancel_id))
      before <- length(log$calls)
      for (i in seq_len(5L)) session$elapse(250L)
      expect_false(sieve_efa_dmar_active(count_job))
      expect_identical(length(log$calls), before)
      session$setInputs(count_go = 2L)
      count_run()
      expect_gt(length(log$calls), before)
      do.call(session$setInputs, stats::setNames(list(2L), cancel_id))
    })
  }
})


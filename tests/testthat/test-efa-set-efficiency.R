# The EFA set of models is computed efficiently, and every number is that of
# separate fits with the same settings.
#
# 1. One call that fits several numbers of factors computes the data, the
#    sample statistics, the saturated model and the baseline model once
#    (sieve_efa_solutions); every number and every warning is that of
#    separate fits.
# 2. The EFA tab's requested models run in background workers, several at
#    once, each count reported as it finishes, and Cancel stops them all.
# 3. The EFA and Dimensionality tabs of a session keep their solutions in
#    one store, so a solution either tab has fit is read, not fit again
#    (sieve_efa_store()).
# 4. The Dimensionality tab's requested sequence runs in the background on
#    its Workers, cancellable, instead of holding the session.
# 5. The wait stated before a run is the wait of the plan that runs.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")
withr::local_options(sieve.cores = 2L) # at most two workers while the tests run

rv_hs24 <- function() {
  fx <- sieve_hs_fixture()
  fx$data[, fx$item_cols, drop = FALSE]
}
rv_hs9 <- function() lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
rv_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))

rv_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = TRUE, name = fx$name, configure = function(s) {
  s$estimator_choice <- "auto"
  s$estimator <- "MLR with FIML"
  })
  s
}

# A controllable stand-in for a worker: it fits its counts with the
# production engine when asked for its result, and reports a count early
# when `early` names one.
rv_mock_start <- function(log, early = integer(0)) {
  function(data, key, k) {
    log$started <- c(log$started, list(as.integer(k)))
    p <- new.env(parent = emptyenv())
    p$alive <- TRUE
    p$killed <- FALSE
    p$given <- integer(0)
    p$is_alive <- function() p$alive
    p$kill <- function() {
      p$alive <- FALSE
      p$killed <- TRUE
      invisible(TRUE)
    }
    one <- function(kk) sieve_efa_fit_by_k(data, key$estimator, key$ordered,
      rotation = key$rotation, ks = kk, missing = key$missing
    )
    p$read_progress <- function() {
      out <- lapply(setdiff(intersect(early, k), p$given), one)
      p$given <- c(p$given, intersect(early, k))
      out
    }
    p$get_result <- function() lapply(k, one)
    log$processes <- c(log$processes, list(p))
    p
  }
}
rv_log <- function() {
  log <- new.env(parent = emptyenv())
  log$started <- list()
  log$processes <- list()
  log
}
rv_key <- function(d, estimator = "MLR", ks = 1:4, selected_k = 3L) {
  list(
    dataset_id = "review", rows = "all", sampling = NULL, data = sieve_data_key(d),
    estimator = estimator, ordered = NULL, missing = "fiml",
    lavaan_missing = sieve_efa_missing_arg("fiml", d) %||% "default", n = nrow(d),
    label = estimator, types = stats::setNames(rep("continuous", ncol(d)), names(d)),
    rotation = "none", ks = ks, selected_k = selected_k, p = ncol(d)
  )
}
rv_finish <- function(jobs) {
  while (sieve_efa_jobs_active(jobs)) {
    for (slot in jobs$slots) slot$process$alive <- FALSE
    sieve_efa_jobs_poll(jobs)
  }
}

# ---- 1. Shared computations, identical numbers -------------------------------

test_that("several numbers of factors share the data, sample statistics, h1 and baseline models, with every number and warning of separate fits", {
  d <- rv_hs24()
  big <- d
  big[[1]] <- big[[1]] * 1e4 # a data-stage warning that every k reports
  holes <- d
  holes[c(5, 9), ] <- NA # empty rows: a data-stage warning under FIML
  holes[cbind(1:40, rep(1:8, 5))] <- NA
  ordinal <- as.data.frame(lapply(d[, 1:8], function(v) as.integer(cut(v, stats::quantile(v, 0:4 / 4), include.lowest = TRUE))))
  cases <- list(
    list(d, "MLR", NULL, "none", 1:4, "fiml"),
    list(d, "ML", NULL, "none", c(1L, 3L, 6L), "listwise"),
    list(big, "MLR", NULL, "none", 1:3, "fiml"),
    list(holes, "MLR", NULL, "none", 1:3, "fiml"),
    list(ordinal, "WLSMV", names(ordinal), "none", 1:3, "listwise"),
    list(rv_hs9(), "MLR", NULL, "oblimin", 2:3, "fiml"),
    list(rv_hs9(), "MLR", NULL, "none", 1:7, "fiml") # k = 6 and 7 not identified
  )
  for (x in cases) {
    run <- function(share) {
      withr::local_options(sieve.efa_share = share)
      set.seed(20261021)
      out <- suppressWarnings(sieve_efa_fit_by_k(x[[1]], x[[2]], x[[3]],
        rotation = x[[4]], ks = x[[5]], missing = x[[6]]
      ))
      list(table = out, seed = .Random.seed)
    }
    shared <- run(TRUE)
    separate <- run(FALSE)
    expect_identical(shared$table, separate$table)
    expect_identical(shared$seed, separate$seed)
  }
  # The warnings of the shared computation are each k's, as a separate fit
  # reports them.
  w <- attr(suppressWarnings(sieve_efa_fit_by_k(big, "MLR", rotation = "none", ks = 1:3, missing = "fiml")), "warnings")
  expect_true(all(vapply(w, function(x) any(grepl("factor 1000 times larger", x)), logical(1))))
  # And the numbers are lavaan's own, fit one k at a time.
  out <- sieve_efa_fit_by_k(d, "MLR", rotation = "none", ks = 1:4, missing = "fiml")
  for (k in 1:4) {
    direct <- suppressWarnings(lavaan::efa(d, nfactors = k, rotation = "none", estimator = "MLR", missing = "listwise"))
    if (inherits(direct, "efaList")) direct <- direct[[1]]
    fm <- lavaan::fitMeasures(direct, c("cfi.robust", "rmsea.robust", "srmr_bentler_nomean", "bic", "chisq.scaled"))
    expect_equal(unname(c(out$CFI[k], out$RMSEA[k], out$SRMR[k], out$BIC[k], out$chisq[k])), unname(fm), tolerance = 1e-8)
  }
})

test_that("the sample statistics are computed once for a set of k, not once per k", {
  d <- rv_hs24()
  calls <- new.env(parent = emptyenv())
  calls$full <- 0L
  original <- lavaan::efa
  local_mocked_bindings(efa = function(...) {
    args <- list(...)
    if (is.null(args$slot_sample_stats)) calls$full <- calls$full + 1L
    original(...)
  }, .package = "lavaan")
  sieve_efa_fit_by_k(d, "MLR", rotation = "none", ks = 1:4, missing = "fiml")
  # One call reads the data and computes the sample statistics, h1 and
  # (with the first solution) the baseline model, not once per k.
  expect_identical(calls$full, 1L)
})

test_that("one-row tables kept per k combine into exactly the table of the set", {
  d <- rv_hs24()
  set <- sieve_efa_fit_by_k(d, "MLR", rotation = "none", ks = 1:5, missing = "fiml")
  singles <- lapply(1:5, function(k) sieve_efa_fit_by_k(d, "MLR", rotation = "none", ks = k, missing = "fiml"))
  rebuilt <- sieve_efa_table(structure(lapply(singles, sieve_efa_solution_of), lavaan_missing = "listwise"))
  expect_identical(rebuilt, set)
  # A count fit second on a worker gives the same one-row table as alone.
  chunk <- sieve_efa_solutions(d, "MLR", rotation = "none", ks = c(3L, 1L), missing = "fiml")
  expect_identical(sieve_efa_table(chunk[2], "listwise"), singles[[1]])
  expect_identical(sieve_efa_table(chunk[1], "listwise"), singles[[3]])
})

# ---- 2. Parallel workers, progress per k, cancel --------------------------

test_that("the selected count runs first and alone, then the others are split over the workers and run at once", {
  expect_identical(sieve_efa_chunks(c(1L, 2L, 4L, 5L, 6L, 7L, 8L), 5L), list(1:2, 4:5, 6L, 7L, 8L))
  expect_identical(sieve_efa_chunks(1:3, 8L), list(1L, 2L, 3L))
  expect_identical(sieve_efa_chunks(1:4, 1L), list(1:4))
  d <- rv_hs9()
  log <- rv_log()
  jobs <- sieve_efa_jobs(rv_mock_start(log), workers = 3L)
  key <- rv_key(d, ks = 1:5)
  sieve_efa_jobs_submit(jobs, key, d, c(3L, 1L, 2L, 4L, 5L))
  expect_identical(log$started, list(3L))
  expect_identical(jobs$pending, c(1L, 2L, 4L, 5L))
  log$processes[[1]]$alive <- FALSE
  sieve_efa_jobs_poll(jobs)
  # With the selected row in, three workers start at once.
  expect_identical(jobs$completed, 3L)
  expect_identical(log$started, list(3L, c(1L, 2L), 4L, 5L))
  expect_length(jobs$slots, 3L)
  expect_identical(sieve_efa_jobs_running(jobs), c(1L, 2L, 4L, 5L))
  rv_finish(jobs)
  expect_setequal(jobs$completed, 1:5)
  parallel <- sieve_efa_rows(lapply(1:5, function(k) sieve_efa_job_cache(jobs, key, k)))
  sequential <- sieve_efa_rows(lapply(1:5, function(k) sieve_efa_fit_by_k(d, "MLR", rotation = "none", ks = k, missing = "fiml")))
  expect_identical(parallel, sequential)
})

test_that("each count is shown as it finishes, and cancel stops every worker", {
  # An estimate the stand-in workers cannot exceed on a busy machine.
  local_mocked_bindings(sieve_fit_wait = function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L, ...) 100)
  d <- rv_hs9()
  log <- rv_log()
  jobs <- sieve_efa_jobs(rv_mock_start(log, early = c(3L, 2L)), workers = 3L)
  key <- rv_key(d, ks = 1:4, selected_k = 3L)
  key$selected_k <- NULL # no selected count: the four start at once
  sieve_efa_jobs_submit(jobs, key, d, c(3L, 1L, 2L, 4L))
  expect_identical(log$started, list(c(3L, 1L), 2L, 4L))
  sieve_efa_jobs_poll(jobs)
  # Counts 3 and 2 are complete while their workers are still running.
  expect_setequal(jobs$completed, c(3L, 2L))
  expect_true(log$processes[[1]]$alive)
  expect_identical(sieve_efa_jobs_running(jobs), c(1L, 4L))
  progress <- sieve_efa_jobs_progress(jobs)
  # Two workers have counts left; the one whose count is in is only exiting.
  expect_identical(progress$workers, 2L)
  expect_match(progress$wait, "Estimated time remaining")
  sieve_efa_jobs_cancel(jobs)
  expect_true(all(vapply(log$processes, function(p) p$killed, logical(1))))
  expect_false(sieve_efa_jobs_active(jobs))
  expect_length(jobs$shared$inflight, 0L)
  # The finished counts stay kept.
  expect_false(is.null(sieve_efa_job_cache(jobs, key, 3L)))
  expect_false(is.null(sieve_efa_job_cache(jobs, key, 2L)))
})

test_that("real workers in parallel return the numbers of a separate fit of each count", {
  skip_on_cran()
  skip_if_not_installed("callr")
  d <- rv_hs9()
  key <- rv_key(d, ks = 1:4)
  jobs <- sieve_efa_jobs(workers = 2L)
  withr::defer(sieve_efa_jobs_cancel(jobs))
  sieve_efa_jobs_submit(jobs, key, d, c(3L, 1L, 2L, 4L))
  # The selected count alone first; then two workers at once.
  expect_length(jobs$slots, 1L)
  most <- 1L
  siever_wait_until(function() !sieve_efa_jobs_active(jobs), "An EFA worker", step = function() {
    Sys.sleep(0.05)
    sieve_efa_jobs_poll(jobs)
    # Workers with counts still to fit (a worker whose counts are all in
    # is only exiting).
    busy <- sum(vapply(jobs$slots, function(s) length(setdiff(s$ks, s$done)) > 0L, logical(1)))
    most <<- max(most, busy)
  })
  expect_identical(most, 2L)
  for (k in 1:4) {
    expect_equal(sieve_efa_job_cache(jobs, key, k),
      sieve_efa_fit_by_k(d, "MLR", rotation = "none", ks = k, missing = "fiml"),
      tolerance = 1e-8
    )
  }
})

# ---- 3. The EFA and Dimensionality tabs report the same model the same way ----
# The EFA page's fit is that of its own DMAR solution
# (DMAR::fa_exploratory's fit_measures, from the unrotated fit), so it does
# not read the Dimensionality tab's store of lavaan::efa rows; at the
# same count the two pages fit the same unrotated model and report the same
# test statistic.

test_that("the EFA page's fit is its own DMAR solution's and agrees with the Dimensionality fit by k at the same count", {
  log <- rv_log()
  fits <- new.env(parent = emptyenv())
  fits$n <- 0L
  original <- lavaan::efa
  local_mocked_bindings(efa = function(...) {
    if (is.null(list(...)$slot_sample_stats)) fits$n <- fits$n + 1L
    original(...)
  }, .package = "lavaan")
  local_mocked_bindings(
    # Three solutions of 0.5 seconds: within the run policy's two seconds.
    sieve_fit_wait = function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L, ...) 0.5,
    withProgress = function(expr, ...) expr,
    sieve_efa_worker_start = rv_mock_start(log),
    sieve_efa_dmar_start = function(...) stop("The EFA page's fit must not start a separate worker"),
    # VSS reads DMAR::fa_exploratory() solutions, whose lavaan::efa() calls
    # would be counted here; it waits for its button.
    sieve_vss_wait = function(max_k, p, R = NULL) 30
  )
  state <- rv_state()
  both <- function(input, output, session) {
    mod_retention_server("dim", state)
    mod_efa_server("efa", state)
  }
  shiny::testServer(both, {
    session$setInputs(
      `dim-cor_type` = "auto", `dim-criterion` = "p95", `dim-pa_reference` = "auto",
      `dim-n_iter` = 10, `dim-max_k` = 3, `dim-seed` = 7, `dim-workers` = 2, `dim-estimator` = "study"
    )
    invisible(output$`dim-fit_control`)
    # The Dimensionality tab fit k = 1 to 3 in this process, sharing the
    # sample statistics: one full lavaan call.
    expect_identical(fits$n, 1L)
    store <- get("sieve_efa_shared", envir = session$userData)
    expect_length(store$cache, 3L)
    session$setInputs(`efa-n_factors` = 3, `efa-rotation` = "oblimin", `efa-estimator` = "study", `efa-efa_go` = 1)
    line <- rv_text(output$`efa-efa_fit_line`)
    expect_match(line, "the unrotated solution of the same model", fixed = TRUE)
    # No lavaan::efa worker and no separate fit-index model for the EFA page.
    expect_length(log$started, 0L)
    expect_length(store$cache, 3L)
    # The same unrotated model at k = 3 (MLR with FIML on complete data):
    # DMAR's scaled chi square is the Dimensionality row's.
    efa_fit_measures <- sieve_efa_store(state, create = FALSE)$fit$fit_measures
    dmar_chisq <- sieve_efa_fit_value(efa_fit_measures, "chi_square")
    dim_rows <- Filter(function(x) identical(x$key$k, 3L), store$cache)
    expect_length(dim_rows, 1L)
    expect_equal(dmar_chisq, dim_rows[[1L]]$value$chisq, tolerance = 1e-6)
    expect_equal(sieve_efa_fit_value(efa_fit_measures, "df"), dim_rows[[1L]]$value$df)
  })
})

# ---- 4. The Dimensionality tab's requested sequence in the background ---------

test_that("a requested model fit by k runs in background workers, can be canceled, and gives the in-process numbers", {
  log <- rv_log()
  local_mocked_bindings(
    sieve_fit_wait = function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L, ...) 20,
    withProgress = function(expr, ...) expr,
    sieve_efa_worker_start = rv_mock_start(log)
  )
  state <- rv_state()
  X <- shiny::isolate(state$items)
  shiny::testServer(mod_retention_server, args = list(state = state), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 10, max_k = 4, seed = 7, workers = 2, estimator = "study"
    )
    expect_identical(fit_run()$status, "not run")
    expect_match(fit_run()$reason, "on 2 background workers at once", fixed = TRUE)
    session$setInputs(fit_go = 1)
    # The session is not held: the sequence runs in two workers.
    expect_true(isTRUE(fit_run()$running))
    expect_identical(log$started, list(1:2, 3:4))
    expect_match(rv_text(output$fit_control), "Cancel the model fit by k", fixed = TRUE)
    session$setInputs(fit_cancel = 1)
    expect_true(all(vapply(log$processes, function(p) p$killed, logical(1))))
    expect_identical(fit_run()$status, "not run")
    session$setInputs(fit_go = 2)
    for (i in 1:20) {
      for (slot in fit_jobs$slots) slot$process$alive <- FALSE
      session$elapse(200)
      if (!isTRUE(fit_run()$running)) break
    }
    f <- fit_run()
    expect_identical(f$status, "ran")
    expect_identical(f$table$k, 1:4)
    direct <- sieve_efa_fit_by_k(X, "ML", rotation = "none", ks = 1:4, missing = "fiml") # Auto's estimator
    expect_equal(f$table$cfi, direct$CFI, tolerance = 1e-8)
    expect_equal(f$table$rmsea, direct$RMSEA, tolerance = 1e-8)
    expect_equal(f$table$chisq, direct$chisq, tolerance = 1e-8)
    expect_match(rv_text(output$fit_control), "on request, on 2 background workers", fixed = TRUE)
  })
})

# ---- 5. The stated wait is the plan's ----------------------------------------

test_that("the wait stated before a run is that of the split over the workers, and memory limits the workers", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  ks <- c(3L, 1L, 2L, 4L, 5L, 6L, 7L, 8L)
  one <- vapply(ks, function(k) sieve_fit_wait(68, "MLR", "efa", n = 500, factors = k), numeric(1))
  sequential <- sieve_efa_plan_wait(68, "MLR", 500, ks, 1L)
  parallel <- sieve_efa_plan_wait(68, "MLR", 500, ks, 6L)
  expect_equal(sequential, one[1] + 0.55 * sum(one[-1]))
  expect_equal(parallel, max(one[1] + 0.55 * one[2], one[3] + 0.55 * one[4], one[5:8]))
  expect_lt(parallel, sequential)
  expect_lt(sequential, sum(one))
  # One MLR worker on 68 items is planned at about 1.5 GB (1.3 GB
  # measured), so eight gigabytes allow five; 303 items under MLR
  # need far more than any budget, so one worker runs; under ML they need
  # 2.4 GB, so three run; the option raises the budget.
  expect_identical(sieve_efa_worker_cap(68, "MLR"), 5L)
  expect_identical(sieve_efa_worker_cap(68, "WLSMV"), 5L)
  expect_identical(sieve_efa_worker_cap(303, "MLR"), 1L)
  expect_identical(sieve_efa_worker_cap(303, "ML"), 3L)
  withr::with_options(list(sieve.efa_memory = 16e9), expect_identical(sieve_efa_worker_cap(68, "MLR"), 10L))
  # The EFA page states the wait of what it runs: DMAR's solution, and the
  # factor count comparison (k = 1 to the largest count; its parallel
  # analysis reference is the Dimensionality page's) in one background process.
  # Read before any run, so that no time measured in this session has
  # moved the rates. The option is set on the test's own frame: set inside
  # the testServer block it would outlive the test and keep every later file
  # from running anything by itself.
  withr::local_options(sieve.auto_run = "never")
  shiny::testServer(mod_efa_server, args = list(state = rv_state()), {
    session$setInputs(n_factors = 3, count_to = 5, count_criterion = "BIC", rotation = "oblimin")
    # (The RMSEA interval is DMAR's at the page's level, so the run is the
    # solution alone, with no second unrotated fit for a 90% interval.)
    expect_equal(primary_seconds(), sieve_efa_dmar_wait(9, "MLR", 301, 3, "data"))
    plan <- count_plan()
    # (B = 0: the parallel analysis reference is the Dimensionality page's,
    # so it adds nothing to the wait.)
    expect_equal(plan$seconds, sieve_efa_count_wait(9, "MLR", 301, 5L, "data", B = 0L))
    expect_equal(plan$seconds, 2 + 1.3 * sum(sieve_efa_dmar_wait(9, "MLR", 301, 1:5, "data")))
    expect_match(rv_text(output$count_note), sprintf("is estimated to take %s", plan$label), fixed = TRUE)
  })
})

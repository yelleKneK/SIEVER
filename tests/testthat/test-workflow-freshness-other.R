# Scientific controls invalidate the affected result, while returning to a
# matching cached request never obliges the researcher to run it again.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

freshness_other_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = FALSE, name = NULL, configure = function(s) {
  s$estimator <- "MLR with FIML"
  s$estimator_choice <- "auto"
  s$theory <- list(k = 3L)
  })
  s
}

test_that("validation splits track holdout seed estimator data and sampling without fitting", {
  s <- freshness_other_state()
  local_mocked_bindings(
    sieve_cross_validate = function(...) stop("A status read must not fit a split."),
    sieve_fit_invariance = function(...) stop("A status read must not fit a ladder.")
  )
  shiny::testServer(mod_finalize_server, args = list(state = s), {
    session$setInputs(holdout = 50, seed = 11, estimator = "mlr")
    keep_cv(list(marker = "saved split", n_cal = 150L, n_val = 151L))
    expect_identical(cv_shown()$marker, "saved split")
    session$setInputs(holdout = 40)
    expect_true(cv_shown()$stale)
    expect_match(cv_shown()$error, "Create split", fixed = TRUE)
    session$setInputs(holdout = 50)
    expect_identical(cv_shown()$marker, "saved split")
    session$setInputs(seed = 12)
    expect_true(cv_shown()$stale)
    session$setInputs(seed = 11, estimator = "ml")
    expect_true(cv_shown()$stale)
    session$setInputs(estimator = "mlr")
    expect_identical(cv_shown()$marker, "saved split")
    original <- shiny::isolate(state$items)
    changed <- original
    changed[[1L]] <- -changed[[1L]]
    state$items <- changed
    expect_true(cv_shown()$stale)
    state$items <- original
    expect_identical(cv_shown()$marker, "saved split")
    state$sampling_design <- list(unit = "clustered", cluster = "school")
    expect_true(cv_shown()$stale)
    state$sampling_design <- list(unit = "independent", cluster = NULL)
    expect_identical(cv_shown()$marker, "saved split")
  })
})

test_that("invariance and derived results track only their relevant analysis controls", {
  s <- freshness_other_state()
  local_mocked_bindings(sieve_fit_invariance = function(...) stop("No fit from a freshness check."))
  shiny::testServer(mod_finalize_server, args = list(state = s), {
    session$setInputs(group = "school", inv_missing = "auto", estimator = "mlr", inv_level = "scalar",
      inv_diag_tbl_rows_selected = 1L, dmacs_source = "partial", dmacs_ref = "1", dmacs_foc = "2",
      align_construct = "Visual", align_starts = 10, align_seed = 3, align_std = FALSE)
    snapshots <- lapply(c("inv", "diagnosis", "partial", "anchor", "dmacs", "alignment"), function(section) {
      bind(list(marker = section), section)
    })
    names(snapshots) <- c("inv", "diagnosis", "partial", "anchor", "dmacs", "alignment")
    read <- function(section) current(snapshots[[section]], section, "Run analysis", section)
    expect_false(isTRUE(read("inv")$stale))
    session$setInputs(inv_missing = "fiml")
    expect_false(isTRUE(read("inv")$stale)) # Auto resolves to the same actual missing rule.
    session$setInputs(inv_missing = "complete")
    expect_true(read("inv")$stale)
    expect_true(read("anchor")$stale)
    expect_false(isTRUE(read("alignment")$stale))
    session$setInputs(inv_missing = "auto", group = "sex")
    expect_true(read("inv")$stale)
    expect_true(read("diagnosis")$stale)
    expect_true(read("partial")$stale)
    expect_true(read("dmacs")$stale)
    session$setInputs(group = "school", inv_level = "strict")
    expect_false(isTRUE(read("inv")$stale))
    expect_true(read("diagnosis")$stale)
    session$setInputs(inv_level = "scalar", inv_diag_tbl_rows_selected = 2L)
    expect_false(isTRUE(read("diagnosis")$stale))
    expect_true(read("partial")$stale)
    expect_true(read("dmacs")$stale)
    session$setInputs(inv_diag_tbl_rows_selected = 1L, align_seed = 4)
    expect_true(read("alignment")$stale)
    session$setInputs(align_seed = 3, align_starts = 20)
    expect_true(read("alignment")$stale)
    session$setInputs(align_starts = 10, align_std = TRUE)
    expect_true(read("alignment")$stale)
    session$setInputs(align_std = FALSE)
    expect_false(isTRUE(read("alignment")$stale))
    original <- shiny::isolate(state$raw)
    changed <- original
    changed$school <- rev(changed$school)
    state$raw <- changed
    expect_true(read("inv")$stale)
    expect_true(read("alignment")$stale)
    state$raw <- original
    expect_false(isTRUE(read("inv")$stale))
    session$setInputs(holdout = 20, seed = 99)
    expect_false(isTRUE(read("inv")$stale)) # Changing another analysis does not stale this one.
  })
})

test_that("HTMT keeps its limits with no threshold to read them against, and rejects changed draws or design", {
  s <- freshness_other_state()
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_mocked_bindings(htmt = function(data, blocks, B = 0L, conf_level = .95, ...) {
    if (B > 0) calls$n <- calls$n + 1L
    out <- data.frame(construct_1 = "Visual", construct_2 = "Textual", htmt = .6, ci_upper = .8)
    if (B > 0) {
      attr(out, "B") <- B
      attr(out, "B_used") <- B
      attr(out, "n_dropped") <- 0L
      attr(out, "B_required") <- sieve_dmar_boot_required("percentile", conf_level, "one")
      attr(out, "bootstrap_status") <- "complete"
      attr(out, "conf_level") <- conf_level
    }
    out
  }, .package = "DMAR")
  withr::local_options(sieve.auto_run = "never")
  shiny::testServer(mod_validity_server, args = list(state = s), {
    session$setInputs(htmt_on = TRUE, htmt_B = 200L, htmt_seed = 8L, htmt_thresh = .85, htmt_boot = 1L)
    first <- htmt_ci_now()
    expect_null(first$error)
    expect_null(first$stale)
    expect_identical(calls$n, 1L)
    # No value is read against the bounds: a threshold input changes
    # nothing and draws nothing.
    session$setInputs(htmt_thresh = .9)
    expect_null(htmt_ci_now()$thr)
    expect_identical(htmt_ci_now()$table, first$table)
    expect_identical(calls$n, 1L)
    session$setInputs(htmt_B = 400L)
    expect_match(htmt_ci_now()$stale, "replications or seed", fixed = TRUE)
    session$setInputs(htmt_B = 200L, htmt_seed = 9L)
    expect_false(is.null(htmt_ci_now()$stale))
    session$setInputs(htmt_seed = 8L)
    expect_null(htmt_ci_now()$stale)
    state$sampling_design <- list(unit = "clustered", cluster = "school")
    expect_match(htmt_ci_now()$stale, "sampling design", fixed = TRUE)
    expect_identical(calls$n, 1L)
    state$sampling_design <- list(unit = "independent", cluster = NULL)
    expect_null(htmt_ci_now()$stale)
  })
})

test_that("Items reuses its manual fit when the estimated runtime becomes automatic", {
  s <- freshness_other_state()
  seconds <- shiny::reactiveVal(30)
  local_mocked_bindings(sieve_fit_wait = function(...) seconds())
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    session$setInputs(factor_cov = "correlated", estimator = "study")
    key <- fit_key()
    stored <- list(status = "ran", key = key, marker = "manual fit", revision = sieve_revision(state))
    fit_store(list(key = key, value = stored))
    expect_identical(fit_run()$marker, "manual fit")
    # This is the same numeric request after a display-only metadata edit.
    # The runtime estimate can then be recomputed from newly observed rates.
    seconds(1)
    expect_true(fit_plan()$automatic)
    expect_identical(fit_run()$marker, "manual fit")
    expect_null(fit_kept$fit)
    state$dataset_id <- "a new loaded sample with identical values"
    sieve_confirm_types(state)
    expect_false(identical(fit_key(), key))
    state$dataset_id <- key$dataset
    sieve_confirm_types(state)
    state$case_index <- rev(seq_len(nrow(shiny::isolate(state$items))))
    expect_false(identical(fit_key(), key))
  })
})

test_that("Dimensionality reuses manual results after runtime estimates change", {
  s <- freshness_other_state()
  seconds <- shiny::reactiveVal(30)
  local_mocked_bindings(sieve_fit_wait = function(...) seconds(),
    # (The plan's signature and fields follow DMAR's parallel analysis: no
    # workers; a run on request above two seconds goes to a background
    # process.)
    sieve_pa_plan = function(basis, n_iter, threshold = 2) list(automatic = seconds() <= 2, n_iter = n_iter,
      seconds = seconds(), compute = seconds(), background = seconds() > 2, label = "controlled time",
      cores = 1L, cores_available = 1L, source = "controlled test", families = c("normal", "resample"),
      draws = 2L * as.integer(n_iter), per_dataset = seconds() / (2 * n_iter), route = sieve_pa_route(basis)))
  shiny::testServer(mod_retention_server, args = list(state = s), {
    session$setInputs(cor_type = "pearson", max_k = 3L, n_iter = 10L, seed = 8L, workers = 1L)
    fitkey <- fit_key()
    pakey <- pa_key()
    fit_store(list(key = fitkey, value = list(marker = "manual fit")))
    pa_store(list(key = pakey, value = list(marker = "manual simulation")))
    expect_identical(fit_run()$marker, "manual fit")
    expect_identical(pa_run()$marker, "manual simulation")
    # Three solutions of 0.5 seconds share a worker: about 1.4 seconds,
    # within the run policy's two seconds.
    seconds(0.5)
    expect_true(fit_plan()$automatic)
    expect_true(pa_plan()$automatic)
    expect_identical(fit_run()$marker, "manual fit")
    expect_identical(pa_run()$marker, "manual simulation")
    expect_null(kept$fit)
    expect_null(kept$pa)
    state$dataset_id <- "a new loaded sample with identical values"
    sieve_confirm_types(state)
    expect_false(identical(fit_key(), fitkey))
    expect_false(identical(pa_key(), pakey))
  })
})

test_that("bifactor intervals require matching dataset rows estimator and bootstrap settings", {
  s <- freshness_other_state()
  shiny::isolate(sieve_ensure_dataset(s))
  withr::local_options(sieve.auto_run = "never")
  local_mocked_bindings(sieve_fit_bifactor = function(...) stop("Reading interval freshness must not fit."))
  shiny::testServer(mod_bifactor_server, args = list(state = s), {
    session$setInputs(idx_B = 100, idx_seed = 4, estimator = "mlr")
    saved <- list(marker = "stored interval", revision = sieve_revision(state), request = idx_request())
    boot_result(saved)
    expect_identical(idx_ci()$marker, "stored interval")
    session$setInputs(idx_B = 200)
    expect_true(idx_ci()$stale)
    session$setInputs(idx_B = 100, idx_seed = 5)
    expect_true(idx_ci()$stale)
    session$setInputs(idx_seed = 4, estimator = "ml")
    expect_true(idx_ci()$stale)
    session$setInputs(estimator = "mlr")
    expect_identical(idx_ci()$marker, "stored interval")
    state$case_index <- rev(seq_len(nrow(shiny::isolate(state$items))))
    expect_true(idx_ci()$stale)
    state$case_index <- NULL
    expect_identical(idx_ci()$marker, "stored interval")
    old_id <- shiny::isolate(state$dataset_id)
    state$dataset_id <- "another loaded sample"
    expect_true(idx_ci()$stale)
    state$dataset_id <- old_id
    state$sampling_design <- list(unit = "clustered", cluster = "school")
    expect_true(idx_ci()$stale)
    state$sampling_design <- list(unit = "independent", cluster = NULL)
    expect_identical(idx_ci()$marker, "stored interval")
  })
})

test_that("assistant local results preserve their original context and label changed inputs", {
  s <- freshness_other_state()
  shiny::isolate(sieve_ensure_dataset(s))
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_mocked_bindings(sieve_assistant_execute = function(state, plan) {
    calls$n <- calls$n + 1L
    list(task = plan$task, guidance = "Recorded local guidance")
  })
  shiny::testServer(function(input, output, session) {
    sieve_assistant_wire(input, output, session, s)
  }, {
    session$setInputs(assist_goal = "Review this study", assist_task = "understand", assist_plan = 1L)
    session$setInputs(assist_execute = 1L)
    expect_match(paste(as.character(output$assist_result_summary$html), collapse = " "),
      "Current recorded local result", fixed = TRUE)
    expect_identical(calls$n, 1L)
    original <- shiny::isolate(s$items)
    changed <- original
    changed[[1L]] <- -changed[[1L]]
    s$items <- changed
    session$flushReact()
    expect_match(paste(as.character(output$assist_result_summary$html), collapse = " "),
      "Historical local result", fixed = TRUE)
    expect_match(output$assist_result, "Recorded local guidance", fixed = TRUE)
    expect_identical(calls$n, 1L)
    s$items <- original
    session$flushReact()
    expect_match(paste(as.character(output$assist_result_summary$html), collapse = " "),
      "Current recorded local result", fixed = TRUE)
    session$setInputs(assist_goal = "A different goal")
    expect_match(paste(as.character(output$assist_result_summary$html), collapse = " "),
      "Historical local result", fixed = TRUE)
    expect_match(paste(as.character(output$assist_result_summary$html), collapse = " "),
      "Review this study", fixed = TRUE)
    expect_identical(calls$n, 1L)
    session$setInputs(assist_goal = "Review this study")
    s$sampling_design <- list(unit = "clustered", cluster = "school")
    session$flushReact()
    expect_match(paste(as.character(output$assist_result_summary$html), collapse = " "),
      "Historical local result", fixed = TRUE)
    expect_identical(calls$n, 1L)
  })
})

test_that("bifactor refuses repeated-observation bootstraps before planning or drawing", {
  s <- freshness_other_state()
  calls <- new.env(parent = emptyenv())
  calls$plans <- calls$draws <- calls$fits <- 0L
  local_mocked_bindings(
    sieve_bifactor_boot_plan = function(...) {
      calls$plans <- calls$plans + 1L
      stop("A refused request must not plan a row bootstrap.")
    },
    sieve_bifactor_boot_draw = function(...) {
      calls$draws <- calls$draws + 1L
      stop("A refused request must not draw a row sample.")
    },
    sieve_fit_bifactor = function(...) {
      calls$fits <- calls$fits + 1L
      stop("A refused request must not fit a model.")
    }
  )
  withr::local_options(sieve.auto_run = "never")
  shiny::testServer(mod_bifactor_server, args = list(state = s), {
    state$sampling_design <- list(unit = "repeated", cluster = NULL)
    session$setInputs(idx_boot = 2L)
    expect_true(boot_result()$refused)
    expect_match(boot_result()$error, "sampling unit is repeated", fixed = TRUE)
    expect_false(boot_running())
    expect_identical(calls$plans, 0L)
    expect_identical(calls$draws, 0L)
    expect_identical(calls$fits, 0L)
  })
})

test_that("a real one-candidate optimizer search cannot be applied under changed constraints", {
  skip_if_not_installed("lavaan")
  s <- freshness_other_state()
  s$construct_basis <- "empirical"
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  original <- sieve_optimizer_panel_search
  local_mocked_bindings(sieve_optimizer_panel_search = function(...) {
    calls$n <- calls$n + 1L
    original(...)
  })
  shiny::testServer(mod_optimize_server, args = list(state = s), {
    session$setInputs(max_drop = 0L, min_keep = 3L, min_facet = 0L, must_keep = character(),
      undersized = "hold", cap = 10L, ss = FALSE, order_by = "search", run = 1L)
    expect_null(res()$error)
    expect_identical(res()$evaluated, 1L)
    expect_false(is.na(res()$revision$dataset_id))
    expect_null(stale())
    expect_identical(calls$n, 1L)
    session$setInputs(cap = 1L, order_by = "cfi")
    expect_null(stale())
    expect_identical(calls$n, 1L)
    session$setInputs(must_keep = shiny::isolate(state$meta$item[1L]), results_rows_selected = 1L, apply = 1L)
    expect_match(stale(), "content constraints", fixed = TRUE)
    expect_null(applied())
    session$setInputs(must_keep = character(), apply = 2L)
    expect_null(stale())
    expect_identical(applied()$id, 1L)
    expect_false(is.null(last_applied()))
    session$setInputs(ss = TRUE)
    expect_false(is.null(stale()))
    session$setInputs(ss = FALSE)
    expect_null(stale())
    state$estimator_choice <- "ml"
    expect_false(is.null(stale()))
    state$estimator_choice <- "auto"
    expect_null(stale())
    expect_identical(calls$n, 1L)
  })
})

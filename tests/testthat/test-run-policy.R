# One run policy for every automatic path. The checks below guard that
# every result the policy names runs by itself for every dataset, not only
# the CFA and Reliability results while the store is empty (a second
# dataset gets a first result of its own, and the earlier dataset's fit is
# never shown under a stale line); that a reopened project is checked in
# one place; that a changed range for the EFA tab's fit indices keeps the
# fits in flight and starts only the counts not kept, and that the EFA
# page's fits and its factor count comparison respect the memory guard;
# that a shared correlation matrix waits for the gate rather than being
# computed as soon as any of four tabs reads it (about twenty seconds on 68
# ordered items); and that no Harman screen or planner's Monte Carlo runs
# from a render. They count the engine calls (lavaan's
# lavaan() and DMAR::fa_exploratory, which the EFA tab, the Items tab and
# VSS fit, are mocked; psych::fa, which SIEVER does not call, is counted to
# show it is not) on the bundled nine and 24 tests, continuous and cut into
# five ordered categories.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("psych")
skip_if_not_installed("shiny")

withr::local_options(sieve.cores = 1L)

rp_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}
rp_html <- function(x) paste(as.character(unlist(x)), collapse = "")

# The bundled nine (or 24) tests, loaded and confirmed, continuous or cut
# into five ordered categories (typed ordered, confirmed under WLSMV).
rp_state <- function(full = FALSE, ordered = FALSE) {
  fx <- if (full) sieve_hs_fixture() else sieve_hs9_fixture()
  siever_test_study(fx, configure = function(s) {
    s$estimator_choice <- "auto"
    if (ordered) {
      s$items <- as.data.frame(lapply(s$items, function(v) as.integer(cut(as.numeric(v), breaks = 5))))
      s$raw[names(s$items)] <- s$items
      m <- s$meta
      m$type <- "ordered"
      s$meta <- m
    }
  })
}

# Load other data into a running session, as the Ingest tab does.
rp_load <- function(state, full = TRUE, ordered = FALSE) {
  fresh <- rp_state(full = full, ordered = ordered)
  for (field in c("dataset_name", "raw", "items", "roles", "meta", "theory", "estimator")) {
    state[[field]] <- shiny::isolate(fresh[[field]])
  }
  shiny::isolate(sieve_new_dataset(state))
  state$case_index <- seq_len(nrow(shiny::isolate(state$raw)))
  shiny::isolate(sieve_confirm_types(state))
  invisible(state)
}

# Save a state as a project and open it in a fresh session.
rp_reopen <- function(state) {
  snapshot <- sieve_project_snapshot(state)
  fresh <- sieve_new_state()
  sieve_project_restore(fresh, snapshot, source = "policy.siever.rds")
  fresh
}

# Count every lavaan model and every psych::fa solution, and keep the
# notifications; installed in the caller's frame.
rp_count <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$lavaan <- 0L
  seen$fa <- 0L
  seen$notes <- character(0)
  real_lavaan <- lavaan::lavaan
  testthat::local_mocked_bindings(
    lavaan = function(...) {
      seen$lavaan <- seen$lavaan + 1L
      real_lavaan(...)
    },
    .package = "lavaan", .env = env
  )
  real_fa <- psych::fa
  testthat::local_mocked_bindings(
    fa = function(...) {
      seen$fa <- seen$fa + 1L
      real_fa(...)
    },
    .package = "psych", .env = env
  )
  seen$dmar <- 0L
  real_dmar <- DMAR::fa_exploratory
  testthat::local_mocked_bindings(
    fa_exploratory = function(...) {
      seen$dmar <- seen$dmar + 1L
      real_dmar(...)
    },
    .package = "DMAR", .env = env
  )
  testthat::local_mocked_bindings(
    sieve_notify = function(message, type = "message", duration = 8) {
      seen$notes <- c(seen$notes, message)
      invisible(NULL)
    },
    withProgress = function(expr, ...) expr,
    .env = env
  )
  seen
}

# A background EFA worker that fits in this process and is finished when
# it is polled.
rp_worker <- function(seen = NULL) {
  function(data, key, k) {
    if (!is.null(seen)) seen$workers <- c(seen$workers, list(as.integer(k)))
    p <- new.env(parent = emptyenv())
    # Alive for one poll (so a running queue can be observed), done at the next.
    p$polls <- 0L
    p$is_alive <- function() {
      p$polls <- p$polls + 1L
      p$polls <= 1L
    }
    p$kill <- function() invisible(TRUE)
    p$get_result <- function() lapply(k, function(kk) sieve_efa_fit_by_k(data, key$estimator, key$ordered,
      rotation = key$rotation, ks = kk, missing = key$missing
    ))
    p
  }
}
rp_drain <- function(jobs, tick, session) {
  for (i in seq_len(200)) {
    if (!sieve_efa_jobs_active(jobs)) break
    sieve_efa_jobs_poll(jobs)
  }
  tick(shiny::isolate(tick()) + 1L)
  session$flushReact()
}

# ---- the rule, said once ------------------------------------------------------

test_that("the policy text names every result that runs by itself, every exception, and the dataset and reopened-project rules", {
  text <- sieve_run_policy_text()
  for (phrase in c(
    "CFA and the Reliability table", "EFA tab's selected solution and its fit indices",
    "Items tab's item diagnostics and overall model", "parallel analysis and VSS, and model fit by k",
    "shared correlation matrix", "Harman's single-factor screen", "reliability planner's Monte Carlo",
    "Validity, SEM, Bifactor, Network, IRT, and CV and Invariance models", "every bootstrap",
    "Data loaded after other data get a first result of their own",
    "kept in the analysis registry and the saved-results report",
    "A reopened project waits on every page", "choosing the number of factors or a range of k",
    "depends only on the size of the problem", "two seconds or less"
  )) {
    expect_match(text, phrase, fixed = TRUE)
  }
  # Said once: the About dialog and the page guide read it.
  expect_match(paste(deparse(body(app_server)), collapse = " "), "sieve_run_policy_text()", fixed = TRUE)
  guide <- rp_text(sieve_page_status_ui(list(page = "cfa", label = "CFA", state = "ready",
    message = "", next_step = "", results = list()), NULL))
  expect_match(guide, "Harman's single-factor screen", fixed = TRUE)
  # The one restored-project check.
  s <- rp_state()
  expect_false(shiny::isolate(sieve_restored_project(s)))
  r <- rp_reopen(s)
  expect_true(shiny::isolate(sieve_restored_project(r)))
  rp_load(r)
  expect_false(shiny::isolate(sieve_restored_project(r)))
  expect_match(sieve_restored_sentence("Fit the model"), "nothing is refit until you press Fit the model.", fixed = TRUE)
  # No second copy of the check anywhere in the package (read from the source
  # tree; an installed copy under R CMD check has no R directory).
  root <- file.path(testthat::test_path(), "..", "..", "R")
  if (dir.exists(root)) {
    files <- list.files(root, full.names = TRUE)
    inline <- grep("project\\$dataset_id, state\\$dataset_id", unlist(lapply(files, readLines)), value = TRUE)
    expect_length(inline, 1L)
  }
})

# The policy names every result it runs, not only the CFA and Reliability
# tabs. The policy paragraph reads one list, and the list names the files
# whose code calls the rule. The paragraph in the page guide is the one
# statement: the Ingest status line does not repeat a short form of it.
test_that("the policy paragraph names the results from one list, the page guide states it, and the list names every file that calls the run rule", {
  x <- sieve_run_policy_results()
  text <- sieve_run_policy_text()
  expect_identical(names(x), c("file", "long"))
  for (i in seq_len(nrow(x))) {
    expect_match(text, x$long[[i]], fixed = TRUE)
  }
  expect_false(exists("sieve_run_policy_chip_text", envir = asNamespace("SIEVER"), inherits = FALSE))
  st <- sieve_new_state()
  guide <- as.character(shiny::isolate(sieve_page_status_ui(sieve_page_status(st, "ingest"), st)))
  expect_match(gsub("&#39;", "'", guide, fixed = TRUE), text, fixed = TRUE)
  expect_match(text, "the CFA and the Reliability table; the EFA tab's selected solution", fixed = TRUE)
  expect_match(text, "; and Harman's single-factor screen on the Validity tab.", fixed = TRUE)
  root <- file.path(testthat::test_path(), "..", "..", "R")
  skip_if_not(dir.exists(root), "the R source tree is not beside the tests")
  callers <- vapply(list.files(root, pattern = "[.]R$", full.names = TRUE), function(f) {
    code <- sub("#.*$", "", readLines(f, warn = FALSE))
    hits <- grepl("sieve_runs_by_itself(", code, fixed = TRUE) &
      !grepl("sieve_runs_by_itself <- function", code, fixed = TRUE)
    any(hits)
  }, logical(1))
  expect_setequal(basename(names(callers)[callers]), unique(x$file))
})

# ---- first view of each page ---------------------------------------------------

test_that("on the first view of each page a first result runs by itself under the gate, once, and a return visit refits nothing", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  seen <- rp_count()
  st <- rp_state()
  # CFA: the shared controller, from the visible page only.
  shiny::testServer(function(input, output, session) mod_cfa_server("cfa", st), {
    session$setInputs(nav = "ingest", `cfa-structure` = "first", `cfa-ident` = "varone", `cfa-layout` = "tree2",
      `cfa-what` = "std", `cfa-min_path` = 0, `cfa-resid` = TRUE, `cfa-covs` = TRUE)
    expect_identical(seen$lavaan, 0L)
    session$setInputs(nav = "cfa")
    expect_gt(seen$lavaan, 0L)
    after <- seen$lavaan
    session$setInputs(nav = "ingest")
    session$setInputs(nav = "cfa")
    expect_identical(seen$lavaan, after)
    expect_identical(shiny::isolate(st$page_results$cfa[["cfa-fit_go"]]$state), "current")
  })
  # Reliability: the same.
  before <- seen$lavaan
  shiny::testServer(function(input, output, session) mod_reliab_server("reliab", st), {
    session$setInputs(nav = "reliab", `reliab-denominator` = "observed", `reliab-ci_method` = "recommended",
      `reliab-missing` = "listwise", `reliab-conf_level` = 0.95, `reliab-B` = 50, `reliab-boot_seed` = NA,
      `reliab-show_alpha` = FALSE, `reliab-score_method` = "Bartlett", `reliab-scope` = "construct",
      `reliab-treatment` = "study")
    expect_gt(seen$lavaan, before)
    after <- seen$lavaan
    session$setInputs(nav = "ingest")
    session$setInputs(nav = "reliab")
    expect_identical(seen$lavaan, after)
  })
  # EFA: the selected solution (DMAR::fa_exploratory; its number of
  # factors comparison waits for its button).
  st <- rp_state()
  seen$dmar <- 0L
  local_mocked_bindings(sieve_efa_dmar_start = function(...) stop("A first view must not start a background comparison."))
  shiny::testServer(function(input, output, session) mod_efa_server("efa", st), {
    session$setInputs(nav = "ingest", `efa-n_factors` = 3L, `efa-rotation` = "oblimin", `efa-fm` = "ml",
      `efa-cor_type` = "auto", `efa-estimator` = "study",
      `efa-suppress` = 0.3, `efa-sort_loadings` = TRUE)
    expect_identical(seen$dmar, 0L)
    session$setInputs(nav = "efa")
    expect_false(is.null(output[["efa-efa_method"]]))
    expect_identical(seen$dmar, 1L)
    session$setInputs(nav = "ingest")
    session$setInputs(nav = "efa")
    expect_false(is.null(output[["efa-efa_method"]]))
    expect_identical(seen$dmar, 1L)
  })
  # Items: the diagnostics and the overall model. The diagnostics' factor
  # analysis is one DMAR::fa_exploratory() call, counted in seen$dmar.
  st <- rp_state()
  seen$fa <- 0L
  seen$dmar <- 0L
  before <- seen$lavaan
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    expect_false(is.null(diag()))
    expect_false(is.null(overall_fit()))
    expect_identical(seen$dmar, 1L)
    expect_identical(seen$fa, 0L)
    expect_gt(seen$lavaan, before)
    after <- seen$lavaan
    expect_false(is.null(diag()))
    expect_false(is.null(overall_fit()))
    expect_identical(seen$dmar, 1L)
    expect_identical(seen$lavaan, after)
  })
  # Dimensionality: parallel analysis and VSS and the model fit by k
  # (three solutions, within two seconds) run by themselves on the nine,
  # once; a longer sequence waits for its button.
  st <- rp_state()
  before <- seen$lavaan
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 200L, max_k = 3, seed = 7)
    expect_true(isTRUE(basis()$ok))
    expect_true(isTRUE(pa_run()$ok))
    expect_true(isTRUE(vss_run()$ok))
    expect_true(isTRUE(fit_run()$ok))
    expect_gt(seen$lavaan, before)
    after <- seen$lavaan
    expect_true(isTRUE(fit_run()$ok))
    expect_identical(seen$lavaan, after)
  })
  st <- rp_state()
  # VSS's solutions are DMAR::fa_exploratory()'s, fit
  # through lavaan, so its wait is held above two seconds as well, and the
  # count of lavaan models stays about the model fit by k.
  local_mocked_bindings(sieve_vss_wait = function(max_k, p, R = NULL) 30)
  before <- seen$lavaan
  local_mocked_bindings(sieve_fit_wait = function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L, ...) 30)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 200L, max_k = 3, seed = 7)
    f <- fit_run()
    expect_identical(f$status, "not run")
    expect_match(f$reason, "Press Run the model fit by k to start it.", fixed = TRUE)
    expect_identical(seen$lavaan, before)
  })
})

# ---- a second dataset in the same session ---------------------------------------

test_that("data loaded after other data get a first result of their own, and the earlier data's result is withheld with a line saying where it is kept", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  # The frame a mock made inside the testServer block below is undone from:
  # a block of testServer is not a function frame, so a mock registered on
  # it would outlive the test and reach every later file.
  test_frame <- environment()
  seen <- rp_count()
  st <- rp_state()
  shiny::testServer(function(input, output, session) mod_cfa_server("cfa", st), {
    session$setInputs(nav = "cfa", `cfa-structure` = "first", `cfa-ident` = "varone", `cfa-layout` = "tree2",
      `cfa-what` = "std", `cfa-min_path` = 0, `cfa-resid` = TRUE, `cfa-covs` = TRUE)
    expect_gt(seen$lavaan, 0L)
    nine <- seen$lavaan
    status <- shiny::isolate(st$page_results$cfa[["cfa-fit_go"]])
    expect_identical(status$state, "current")
    # The 24 tests replace the nine, as a load does: the nine-item result is
    # withheld (not run, no fit), and a first result runs for the 24 by
    # itself (the rate the nine's fit measured is cleared first: an MLR
    # rate measured on nine items extrapolates to 24 above the six-second
    # veto, which is the measured rule, not the dataset rule under test).
    sieve_rate_reset()
    rp_load(st, full = TRUE)
    session$flushReact()
    expect_gt(seen$lavaan, nine)
    status <- shiny::isolate(st$page_results$cfa[["cfa-fit_go"]])
    expect_identical(status$state, "current")
    gate <- rp_text(output[["cfa-method_line"]])
    expect_match(gate, "CFA, 24 items", fixed = TRUE)
    twenty_four <- seen$lavaan
    # Other data again (the nine, a new load): the 24-item result is
    # withheld, the line names the data it belongs to and where it is
    # kept, and nothing of it is shown. With the estimate above two
    # seconds the first result waits for the button.
    local_mocked_bindings(
      sieve_fit_wait = function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L, ...) 30,
      .env = test_frame
    )
    rp_load(st, full = FALSE)
    session$flushReact()
    expect_identical(seen$lavaan, twenty_four)
    status <- shiny::isolate(st$page_results$cfa[["cfa-fit_go"]])
    expect_identical(status$state, "not_run")
    expect_match(status$sentence,
      "The result of the data loaded before (Holzinger and Swineford, full (n = 301, 24 tests, both schools): CFA: ML, 24 items at",
      fixed = TRUE
    )
    expect_match(status$sentence, "is kept in the analysis registry and the saved-results report, not shown for these data.", fixed = TRUE)
    gate <- rp_text(output[["cfa-gate"]])
    expect_match(gate, "The model is not fit yet.", fixed = TRUE)
    expect_match(gate, "kept in the analysis registry", fixed = TRUE)
    expect_false(grepl("from before", gate, fixed = TRUE))
    session$setInputs(`cfa-fit_go` = 1L)
    expect_gt(seen$lavaan, twenty_four)
    expect_identical(shiny::isolate(st$page_results$cfa[["cfa-fit_go"]])$state, "current")
  })
})

# ---- a reopened project ---------------------------------------------------------

test_that("a reopened project fits nothing on any page until a button, and each page says why in one sentence", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  seen <- rp_count()
  local_mocked_bindings(sieve_efa_worker_start = rp_worker())
  st <- rp_reopen(rp_state())
  expect_true(shiny::isolate(sieve_restored_project(st)))
  sentence <- "This project was reopened: its recorded results are in the analysis registry and the saved-results report, and nothing is refit until you press"
  # CFA and Reliability: the shared controller.
  shiny::testServer(function(input, output, session) mod_cfa_server("cfa", st), {
    session$setInputs(nav = "cfa", `cfa-structure` = "first", `cfa-ident` = "varone", `cfa-layout` = "tree2",
      `cfa-what` = "std", `cfa-min_path` = 0, `cfa-resid` = TRUE, `cfa-covs` = TRUE)
    expect_identical(seen$lavaan, 0L)
    gate <- rp_text(output[["cfa-gate"]])
    expect_match(gate, paste(sentence, "Fit the model."), fixed = TRUE)
    expect_false(grepl("so it waits for you", gate, fixed = TRUE))
    session$setInputs(`cfa-fit_go` = 1L)
    expect_gt(seen$lavaan, 0L)
  })
  seen$lavaan <- 0L
  shiny::testServer(function(input, output, session) mod_reliab_server("reliab", st), {
    session$setInputs(nav = "reliab", `reliab-denominator` = "observed", `reliab-ci_method` = "recommended",
      `reliab-missing` = "listwise", `reliab-conf_level` = 0.95, `reliab-B` = 50, `reliab-boot_seed` = NA,
      `reliab-show_alpha` = FALSE, `reliab-score_method` = "Bartlett", `reliab-scope` = "construct",
      `reliab-treatment` = "study")
    expect_identical(seen$lavaan, 0L)
    expect_match(rp_text(output[["reliab-gate"]]), paste(sentence, "Compute reliability."), fixed = TRUE)
    session$setInputs(`reliab-run_go` = 1L)
    expect_gt(seen$lavaan, 0L)
  })
  # EFA: the selected solution (DMAR::fa_exploratory).
  seen$dmar <- 0L
  shiny::testServer(function(input, output, session) mod_efa_server("efa", st), {
    session$setInputs(nav = "efa", `efa-n_factors` = 3L, `efa-rotation` = "oblimin", `efa-fm` = "ml",
      `efa-cor_type` = "auto", `efa-estimator` = "study",
      `efa-suppress` = 0.3, `efa-sort_loadings` = TRUE)
    line <- rp_text(output[["efa-status_line"]])
    expect_match(line, "The selected EFA has not been fit for these data and settings.", fixed = TRUE)
    expect_match(line, paste(sentence, "Run selected EFA."), fixed = TRUE)
    expect_error(output[["efa-efa_method"]], class = "shiny.silent.error")
    expect_identical(seen$dmar, 0L)
    session$setInputs(`efa-efa_go` = 1L)
    expect_false(is.null(output[["efa-efa_method"]]))
    expect_identical(seen$dmar, 1L)
  })
  # Items: the diagnostics wait for Update once, then follow edits again.
  seen$fa <- 0L
  seen$dmar <- 0L
  seen$lavaan <- 0L
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    r <- diag_run()
    expect_identical(r$status, "not run")
    expect_true(r$restored)
    expect_null(diag())
    expect_identical(fit_run()$status, "not run")
    expect_true(fit_run()$restored)
    expect_identical(c(seen$dmar, seen$lavaan), c(0L, 0L))
    line <- rp_text(output$status_line)
    expect_match(line, paste(sentence, "Update."), fixed = TRUE)
    expect_match(line, "Update (", fixed = TRUE)
    session$setInputs(update_go = 1L)
    expect_false(is.null(diag()))
    expect_false(is.null(overall_fit()))
    # The EFA tab's solution above (DMAR::fa_exploratory, MLR with FIML on
    # the complete nine tests, k = 3, direct oblimin) is the Items tab's
    # model, so it is read and nothing is fit here (both tabs fit DMAR).
    expect_true(diag()$solution_reused)
    expect_identical(seen$dmar, 0L)
    expect_identical(seen$fa, 0L)
    # Asked for once: the diagnostics are live again after an exclusion
    # (their own fit now, the pool having changed).
    sieve_set_item_status(st, "t2_cubes", "parked", "probe")
    session$flushReact()
    expect_false(is.null(diag()))
    expect_false(diag()$solution_reused)
    expect_identical(seen$dmar, 1L)
  })
  # Dimensionality: parallel analysis and VSS and the fit by k.
  st <- rp_reopen(rp_state())
  seen$fa <- 0L
  seen$lavaan <- 0L
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 200L, max_k = 3, seed = 7)
    expect_true(isTRUE(basis()$ok)) # Pearson correlations cost nothing
    pa <- pa_run()
    expect_identical(pa$status, "not run")
    expect_true(pa$restored)
    expect_match(pa$reason, paste(sentence, "Run parallel analysis."), fixed = TRUE)
    v <- vss_run()
    expect_identical(v$status, "not run")
    expect_true(v$restored)
    expect_match(v$reason, paste(sentence, "Run VSS."), fixed = TRUE)
    f <- fit_run()
    expect_identical(f$status, "not run")
    expect_true(f$restored)
    expect_match(f$reason, paste(sentence, "Run the model fit by k."), fixed = TRUE)
    # No lavaan model; the scree plot's reduced eigenvalues (one-factor
    # communalities from psych::fa) are drawn from the matrix as soon as
    # it is, and are not one of the gated results.
    expect_identical(seen$lavaan, 0L)
    expect_match(rp_text(output$status_line), paste(sentence, "their buttons."), fixed = TRUE)
    expect_false(grepl("runs by itself only when", rp_text(output$status_line), fixed = TRUE))
    # VSS's solutions are DMAR::fa_exploratory()'s.
    dmar_before <- seen$dmar
    session$setInputs(vss_go = 1L)
    expect_true(isTRUE(vss_run()$ok))
    expect_gt(seen$dmar, dmar_before)
  })
  # The correlation basis of a reopened ordered pool waits too, however
  # small; Pearson correlations never do.
  st <- rp_reopen(rp_state(ordered = TRUE))
  cors <- new.env()
  cors$n <- 0L
  real_cor <- sieve_retention_cor
  local_mocked_bindings(sieve_retention_cor = function(...) {
    cors$n <- cors$n + 1L
    real_cor(...)
  })
  shiny::testServer(mod_screen_server, args = list(state = st), {
    b <- basis()
    expect_true(b$waiting)
    expect_true(b$restored)
    expect_identical(cors$n, 0L)
    note <- rp_text(output$cor_note)
    expect_match(note, "this project was reopened", fixed = TRUE)
    expect_match(note, "Compute the correlations (under 2 seconds)", fixed = TRUE)
    session$setInputs(basis_go = 1L)
    expect_true(isTRUE(basis()$ok))
    expect_identical(cors$n, 1L)
  })
  # Harman's screen and the planner's Monte Carlo.
  st <- rp_reopen(rp_state())
  shiny::testServer(mod_validity_server, args = list(state = st), {
    r <- harman_run()
    expect_identical(r$status, "not run")
    expect_true(r$restored)
    expect_match(harman_df()$Value[1], paste(sentence, "Compute Harman's screen."), fixed = TRUE)
    session$setInputs(harman_go = 1L)
    expect_identical(harman_df()$Statistic[2], "Proportion variance, factor 1")
  })
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "observed", ci_method = "recommended", missing = "listwise", conf_level = 0.95,
      B = 50, boot_seed = NA, show_alpha = FALSE, score_method = "Bartlett", scope = "construct",
      treatment = "study", plan_omega = 0.80, plan_k = 6, plan_width = 0.10, plan_assurance = "0.85", plan_seed = 113
    )
    p <- plan_calc()
    expect_true(p$waiting)
    expect_match(rp_text(output$plan_result), "Plan with assurance (", fixed = TRUE)
    session$setInputs(plan_go = 1L)
    expect_match(rp_text(output$plan_result), "85% assurance", fixed = TRUE)
  })
})

# ---- the EFA fit indices for a changed range -----------------------------------

test_that("a changed range keeps the fits in flight without starting new counts (the observer's path), and the extension starts only the counts not kept", {
  withr::local_options(sieve.cores = 2L)
  seen <- new.env()
  seen$workers <- list()
  jobs <- sieve_efa_jobs(start = rp_worker(seen), workers = function() 2L, shared = sieve_efa_shared())
  st <- rp_state()
  X <- shiny::isolate(st$items)
  key <- list(
    dataset_id = "D1", rows = "r", sampling = list(unit = "independent"), data = sieve_data_key(X),
    estimator = "MLR", ordered = NULL, missing = "listwise", lavaan_missing = "listwise", n = nrow(X),
    label = "MLR", types = rep("continuous", 9L), rotation = "none", ks = 3L, selected_k = 3L, p = 9L
  )
  sieve_efa_jobs_submit(jobs, key, X, 3L)
  expect_identical(unlist(seen$workers), 3L)
  # A wider range while k = 3 is in flight: rehomed, nothing new started,
  # the queue holds the count in flight only, the others wait.
  wider <- key
  wider$ks <- 1:5
  expect_true(sieve_efa_jobs_rehome(jobs, wider, c(3L, 1L, 2L, 4L, 5L)))
  expect_identical(jobs$key, wider)
  expect_identical(jobs$requested, 3L)
  expect_length(jobs$pending, 0L)
  expect_identical(unlist(seen$workers), 3L)
  expect_true(sieve_efa_jobs_active(jobs))
  # The same request again, or a changed scientific identity, is not rehomed.
  expect_false(sieve_efa_jobs_rehome(jobs, wider, 1:5))
  other <- wider
  other$estimator <- "ML"
  expect_false(sieve_efa_jobs_rehome(jobs, other, 1:5))
  # The extension queues the counts not kept and not in flight; they start
  # once the selected model's row is in (the selected model runs first and
  # alone), split over the two workers, and k = 3 is neither started again
  # nor dropped.
  expect_true(sieve_efa_jobs_extend(jobs, c(3L, 1L, 2L, 4L, 5L)))
  expect_setequal(jobs$requested, 1:5)
  expect_setequal(jobs$pending, c(1L, 2L, 4L, 5L))
  expect_identical(unlist(seen$workers), 3L)
  sieve_efa_jobs_poll(jobs)
  expect_setequal(unlist(seen$workers), 1:5)
  expect_identical(sum(unlist(seen$workers) == 3L), 1L)
  expect_length(seen$workers, 3L)
  for (i in seq_len(50)) if (sieve_efa_jobs_active(jobs)) sieve_efa_jobs_poll(jobs)
  expect_false(sieve_efa_jobs_active(jobs))
  expect_setequal(jobs$completed, 1:5)
  # Nothing to add: no worker starts.
  before <- length(seen$workers)
  expect_false(sieve_efa_jobs_extend(jobs, 1:5))
  expect_length(seen$workers, before)
  # A narrower range stops the worker whose counts are no longer asked for
  # and keeps the one whose counts still are.
  jobs2 <- sieve_efa_jobs(start = rp_worker(seen), workers = function() 2L, shared = sieve_efa_shared())
  sieve_efa_jobs_submit(jobs2, wider, X, c(3L, 1L, 2L, 4L, 5L))
  sieve_efa_jobs_poll(jobs2)
  sieve_efa_jobs_poll(jobs2)
  expect_setequal(sieve_efa_jobs_running(jobs2), c(1L, 2L, 4L, 5L))
  narrower <- wider
  narrower$ks <- 1:3
  expect_true(sieve_efa_jobs_rehome(jobs2, narrower, c(3L, 1L, 2L)))
  expect_setequal(sieve_efa_jobs_running(jobs2), c(1L, 2L))
  expect_setequal(jobs2$requested, c(3L, 1L, 2L))
  expect_length(jobs2$pending, 0L)
})

test_that("the EFA page's DMAR fits and its factor count comparison respect the memory guard", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  seen <- rp_count()
  log <- new.env(parent = emptyenv())
  guard <- shiny::reactiveVal(FALSE)
  # The selected solution is fit in this process when its estimate is two
  # seconds or less, and the estimate scales with the time measured for the
  # page's last fit. On a busy machine the three-factor fit below can take
  # over twice its standard time, which puts the two-factor fit past two
  # seconds and into a background process, a second start in `log` beside
  # the comparison's. The times are not recorded here, so every
  # estimate is at the standard rates, as this file intends, and the one
  # background start counted is the comparison.
  local_mocked_bindings(
    sieve_efa_dmar_start = efa_dmar_fake_start(log),
    sieve_mlr_guard = function(...) if (isTRUE(shiny::isolate(guard()))) "Refused for the test: not enough memory." else NULL,
    sieve_efa_dmar_time_note = function(...) invisible(NULL)
  )
  shiny::testServer(mod_efa_server, args = list(state = rp_state()), {
    session$setInputs(n_factors = 3L, rotation = "oblimin", fm = "ml", cor_type = "auto", estimator = "study",
      count_to = 3L, count_criterion = "BIC", efa_go = 1L)
    expect_false(is.null(efa()))
    expect_identical(seen$dmar, 1L)
    guard(TRUE)
    session$setInputs(n_factors = 2L, efa_go = 2L)
    r <- efa_run()
    expect_null(r$fit)
    expect_match(r$refused, "Refused for the test", fixed = TRUE)
    expect_identical(seen$dmar, 1L)
    expect_match(rp_text(output$status_line), "Refused for the test", fixed = TRUE)
    # The comparison is refused before its button, and nothing starts.
    session$setInputs(count_to = 2L)
    expect_identical(count_run()$status, "refused")
    expect_match(rp_text(output$count_note), "Refused for the test", fixed = TRUE)
    session$setInputs(count_go = 1L)
    expect_length(log$calls, 0L)
    guard(FALSE)
    session$setInputs(efa_go = 3L)
    expect_identical(ncol(efa()$loadings), 2L)
    expect_identical(seen$dmar, 2L)
    session$setInputs(count_go = 2L)
    efa_dmar_settle(session)
    expect_identical(count_run()$status, "ran")
    expect_length(log$calls, 1L)
    expect_identical(log$calls[[1]]$fun, "factor_count")
  })
})

# ---- the correlation basis under the gate ---------------------------------------

test_that("the basis of an ordered pool estimated above two seconds waits on Screening and Dimensionality with the estimate, and a quick one computes by itself", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  cors <- new.env()
  cors$n <- 0L
  real_cor <- sieve_retention_cor
  local_mocked_bindings(
    sieve_retention_cor = function(...) {
      cors$n <- cors$n + 1L
      real_cor(...)
    },
    withProgress = function(expr, ...) expr
  )
  # The 24 tests in five ordered categories: 276 pairs, estimated at 2.5
  # seconds at the standard polychoric rate.
  st <- rp_state(full = TRUE, ordered = TRUE)
  expect_gt(sieve_basis_wait(24, "polychoric"), 2)
  shiny::testServer(mod_screen_server, args = list(state = st), {
    b <- basis()
    expect_true(b$waiting)
    expect_identical(b$used, "polychoric")
    expect_identical(cors$n, 0L)
    expect_false(isTRUE(cor_mat()$ok))
    note <- rp_html(output$cor_note)
    expect_match(rp_text(note), "The polychoric correlations of the 24 included items have not been computed (estimated at about 3 seconds", fixed = TRUE)
    expect_match(rp_text(note), "Compute the correlations (about 3 seconds)", fixed = TRUE)
    expect_match(note, '-basis_go"', fixed = TRUE)
    session$setInputs(basis_go = 1L)
    b <- basis()
    expect_true(isTRUE(b$ok))
    expect_identical(b$used, "polychoric")
    expect_identical(cors$n, 1L)
    expect_true(isTRUE(cor_mat()$ok))
  })
  # Dimensionality, on a fresh state: every criterion waits with it, the
  # status line carries the one button (the page's Update, relabeled), and
  # Update computes the correlations first. (The rate the Screening run
  # measured is cleared: the decision is made at the standard rates, and
  # the label states the estimate at the rates in force.)
  sieve_rate_reset()
  st <- rp_state(full = TRUE, ordered = TRUE)
  cors$n <- 0L
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 200L, max_k = 3, seed = 7)
    expect_true(basis()$waiting)
    expect_true(pa_run()$basis_waiting)
    expect_match(pa_run()$reason, "Parallel analysis waits for the correlations.", fixed = TRUE)
    expect_true(vss_run()$basis_waiting)
    expect_true(fit_run()$basis_waiting)
    expect_error(criteria(), class = "shiny.silent.error")
    expect_identical(cors$n, 0L)
    line <- rp_html(output$status_line)
    expect_match(rp_text(line), "The correlations have not been computed.", fixed = TRUE)
    expect_match(rp_text(line), "Compute the correlations (about 3 seconds)", fixed = TRUE)
    expect_match(line, '-update_go"', fixed = TRUE)
    expect_match(rp_text(output$pa_control), "The button is in the status line", fixed = TRUE)
    expect_match(rp_text(output$summary_reco), "have not been computed", fixed = TRUE)
    session$setInputs(update_go = 1L)
    expect_true(isTRUE(basis()$ok))
    expect_identical(cors$n, 1L)
    expect_false(isTRUE(pa_run()$basis_waiting))
    expect_false(isTRUE(vss_run()$basis_waiting))
    expect_s3_class(criteria(), "data.frame")
  })
  # The nine in five categories: 36 pairs, well within two seconds, so the
  # matrix is computed by itself.
  st <- rp_state(ordered = TRUE)
  cors$n <- 0L
  shiny::testServer(mod_screen_server, args = list(state = st), {
    b <- basis()
    expect_true(isTRUE(b$ok))
    expect_identical(b$used, "polychoric")
    expect_identical(cors$n, 1L)
  })
  # The EFA tab's matrix route (minres on the shared matrix) and the Items
  # tab read the same gate through their own estimates, which include the
  # matrix, and compute it when their request is made.
  st <- rp_state(full = TRUE, ordered = TRUE)
  cors$n <- 0L
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(
      n_factors = 3L, rotation = "oblimin", fm = "minres_matrix", cor_type = "auto", estimator = "study",
      suppress = 0.3, sort_loadings = TRUE
    )
    expect_true(efa_run()$pending)
    expect_identical(cors$n, 0L)
    expect_gt(primary_standard(), 2)
    session$setInputs(efa_go = 1L)
    expect_false(is.null(efa()))
    expect_identical(cors$n, 1L)
  })
})

# ---- Harman's screen and the planner ----------------------------------------------

test_that("Harman's screen follows the gate: a first result by itself, a changed pool and a long estimate wait for the button, and the note says what DMAR computes", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  calls <- new.env()
  calls$n <- 0L
  real <- DMAR::common_method_single_factor
  local_mocked_bindings(
    common_method_single_factor = function(...) {
      calls$n <- calls$n + 1L
      real(...)
    },
    .package = "DMAR"
  )
  st <- rp_state()
  shiny::testServer(mod_validity_server, args = list(state = st), {
    df <- harman_df()
    expect_identical(df$Statistic[2], "Proportion variance, factor 1")
    expect_identical(calls$n, 1L)
    # What DMAR computes, said in the note and matched by a direct call.
    x <- shiny::isolate(active_x())
    R <- stats::cor(x, use = "pairwise.complete.obs")
    direct <- sum(stats::factanal(covmat = R, factors = 1L)$loadings[, 1L]^2) / ncol(x)
    expect_identical(df$Value[2], sprintf("%.3f", direct))
    note <- rp_text(output$harman_note)
    expect_match(note, "one common factor is fit by maximum likelihood (stats::factanal)", fixed = TRUE)
    expect_match(note, "sum of the squared loadings over the number of items", fixed = TRUE)
    expect_match(note, "Podsakoff, MacKenzie, Lee and Podsakoff (2003) describe the convention", fixed = TRUE)
    expect_match(note, "SIEVER applies no cutoff to the value", fixed = TRUE)
    expect_match(note, "cannot by itself rule method variance in or out", fixed = TRUE)
    expect_false(grepl("largest eigenvalue", note, fixed = TRUE))
    expect_false(grepl("harman_go", rp_html(output$harman_note), fixed = TRUE))
    # An exclusion: the earlier value is from before and withheld; the
    # button computes it again.
    sieve_set_item_status(st, "t2_cubes", "parked", "probe")
    session$flushReact()
    r <- harman_run()
    expect_identical(r$status, "not run")
    expect_true(r$stale)
    expect_identical(calls$n, 1L)
    expect_match(harman_df()$Value[1], "changed since Harman's screen was computed", fixed = TRUE)
    expect_match(rp_html(output$harman_note), '-harman_go"', fixed = TRUE)
    session$setInputs(harman_go = 1L)
    expect_identical(calls$n, 2L)
    expect_identical(harman_df()$Value[1], "8")
  })
  # A long estimate waits for the button, which states it.
  local_mocked_bindings(sieve_harman_wait = function(p) 30)
  st <- rp_state()
  calls$n <- 0L
  shiny::testServer(mod_validity_server, args = list(state = st), {
    r <- harman_run()
    expect_identical(r$status, "not run")
    expect_match(r$reason, "Harman's screen has not been computed yet (estimated at about 30 seconds).", fixed = TRUE)
    expect_identical(calls$n, 0L)
    expect_match(rp_text(output$harman_note), "Compute Harman's screen (about 30 seconds)", fixed = TRUE)
    expect_match(rp_text(output$harman_tbl), "Statistic", fixed = TRUE)
    session$setInputs(harman_go = 1L)
    expect_identical(calls$n, 1L)
    expect_identical(harman_df()$Statistic[2], "Proportion variance, factor 1")
  })
})

test_that("the planner's Monte Carlo waits for Plan with assurance every time, whose label states the estimate, and the closed form stays live", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  calls <- new.env()
  calls$n <- 0L
  real <- DMAR::ss_aipe_reliability
  local_mocked_bindings(
    ss_aipe_reliability = function(...) {
      calls$n <- calls$n + 1L
      real(...)
    },
    .package = "DMAR"
  )
  local_mocked_bindings(withProgress = function(expr, ...) expr)
  st <- rp_state()
  inputs <- list(
    denominator = "observed", ci_method = "recommended", missing = "listwise", conf_level = 0.95,
    B = 50, boot_seed = NA, show_alpha = FALSE, score_method = "Bartlett", scope = "construct",
    treatment = "study", plan_omega = 0.80, plan_k = 6, plan_width = 0.10, plan_assurance = ""
  )
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    do.call(session$setInputs, inputs)
    expect_match(rp_text(output$plan_result), "Required N:", fixed = TRUE)
    closed <- calls$n
    expect_gt(closed, 0L)
    # An assurance plan: the Monte Carlo waits for the button, which states
    # the estimate (a planner reads no data, so it has no first result); the
    # closed form stays live.
    session$setInputs(plan_assurance = "0.85", plan_seed = 113)
    expect_true(isTRUE(plan_calc()$waiting))
    expect_identical(calls$n, closed)
    shown <- rp_html(output$plan_result)
    expect_match(rp_text(shown), "Plan with assurance (", fixed = TRUE)
    expect_match(shown, '-plan_go"', fixed = TRUE)
    session$setInputs(plan_go = 1L)
    expect_false(isTRUE(plan_calc()$waiting))
    expect_match(rp_text(output$plan_result), "85% assurance", fixed = TRUE)
    expect_identical(calls$n, closed + 1L)
    n1 <- plan_calc()$n_req
    # A changed input: the Monte Carlo waits again until the button.
    session$setInputs(plan_width = 0.08)
    p <- plan_calc()
    expect_true(p$waiting)
    expect_identical(calls$n, closed + 1L)
    shown <- rp_html(output$plan_result)
    expect_match(rp_text(shown), "waits for its button", fixed = TRUE)
    expect_match(rp_text(shown), "Plan with assurance (", fixed = TRUE)
    session$setInputs(plan_assurance = "")
    expect_match(rp_text(output$plan_result), "Required N:", fixed = TRUE)
    expect_identical(calls$n, closed + 2L)
    session$setInputs(plan_assurance = "0.85")
    expect_true(plan_calc()$waiting)
    session$setInputs(plan_go = 2L)
    p <- plan_calc()
    expect_false(isTRUE(p$waiting))
    expect_gt(p$n_req, n1)
    expect_identical(calls$n, closed + 3L)
    # Pinned against the direct DMAR call under the same seed.
    rho <- 0.80 / (6 - 0.80 * 5)
    set.seed(113)
    ref <- real(model = "Parallel", type = "Normal Theory", width = 0.08, n_items = 6, true_item_covariance = rho,
      true_residual_var = 1 - rho, conf_level = 0.95, assurance = 0.85)
    expect_identical(as.integer(p$n_req), as.integer(ref$value[ref$term == "necessary_N"]))
  })
  # A long estimate: the button states it.
  local_mocked_bindings(sieve_planner_wait = function() 30)
  st <- rp_state()
  calls$n <- 0L
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    do.call(session$setInputs, utils::modifyList(inputs, list(plan_assurance = "0.85", plan_seed = 113)))
    expect_true(plan_calc()$waiting)
    expect_match(rp_text(output$plan_result), "Plan with assurance (about 30 seconds)", fixed = TRUE)
    expect_identical(calls$n, 0L)
  })
})

# ---- the Items tab and the EFA tab's solution ----------------------------------
# The Items tab reads the EFA tab's solution when it is the same model. Both
# are DMAR::fa_exploratory, and the EFA tab's maximum likelihood solution
# (ML or MLR) on complete continuous items, at the Items tab's k with direct
# oblimin, has the estimates of maximum likelihood on the Pearson
# correlations of those cases: it is read, and every other route is named
# as another model.

test_that("the Items diagnostics read the EFA tab's DMAR solution when it is the same model, and say how it differs when not", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  seen <- rp_count()
  st <- rp_state()
  shiny::testServer(function(input, output, session) mod_efa_server("efa", st), {
    session$setInputs(nav = "efa", `efa-n_factors` = 3L, `efa-rotation` = "oblimin", `efa-fm` = "ml",
      `efa-cor_type` = "auto", `efa-estimator` = "study",
      `efa-suppress` = 0.3, `efa-sort_loadings` = TRUE)
    expect_false(is.null(output[["efa-efa_method"]]))
    expect_identical(seen$dmar, 1L)
    expect_identical(seen$fa, 0L)
  })
  store <- sieve_efa_store(st, create = FALSE)
  expect_true(is.environment(store))
  expect_identical(store$key$k, 3L)
  expect_identical(store$key$engine, "DMAR")
  # Session only: never saved in a project.
  expect_identical(sieve_project_snapshot(st)$dropped, character(0))
  expect_false("efa" %in% names(sieve_project_snapshot(st)$other$cache))
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    d <- diag()
    expect_false(is.null(d))
    expect_true(d$solution_reused)
    expect_identical(seen$dmar, 1L)
    line <- rp_text(output$model_line)
    expect_match(line, "from the EFA tab's solution (fit at", fixed = TRUE)
    expect_match(line, "by DMAR::fa_exploratory, ML with FIML, whose estimates are those of maximum likelihood on these correlations", fixed = TRUE)
    # The loadings read equal the Items tab's own fit of the matrix (the
    # same estimates, to the rotation's convergence).
    own <- sieve_item_diagnostics(shiny::isolate(st$items), shiny::isolate(st$meta), rng = d$rng)
    expect_false(own$solution_reused)
    expect_lt(max(abs(own$items$loading_target - d$items$loading_target)), 1e-4)
    expect_lt(max(abs(own$items$communality - d$items$communality)), 1e-5)
  })
  # Another model on the EFA tab (geomin): named, and the diagnostics fit their own.
  st <- rp_state()
  seen$dmar <- 0L
  shiny::testServer(function(input, output, session) mod_efa_server("efa", st), {
    session$setInputs(nav = "efa", `efa-n_factors` = 3L, `efa-rotation` = "geomin", `efa-fm` = "ml",
      `efa-cor_type` = "auto", `efa-estimator` = "study",
      `efa-suppress` = 0.3, `efa-sort_loadings` = TRUE)
    expect_false(is.null(output[["efa-efa_method"]]))
  })
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    expect_false(diag()$solution_reused)
    expect_identical(seen$dmar, 2L)
    line <- rp_text(output$model_line)
    expect_match(line, "from DMAR::fa_exploratory, fit here", fixed = TRUE)
    expect_match(line, "is a different model (geomin (oblique, epsilon = 0.001) rotation there, direct oblimin here), so the diagnostics fit their own.", fixed = TRUE)
  })
  # No EFA solution at all: fit here, nothing about the EFA tab.
  st <- rp_state()
  seen$dmar <- 0L
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    expect_false(diag()$solution_reused)
    expect_identical(seen$dmar, 1L)
    expect_identical(seen$fa, 0L)
    line <- rp_text(output$model_line)
    expect_match(line, "from DMAR::fa_exploratory, fit here", fixed = TRUE)
    expect_false(grepl("EFA tab", line, fixed = TRUE))
  })
})

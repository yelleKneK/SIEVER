# The layout of the Reliability and Items pages: results first (the method
# line and the result above the explanation); the compact Reliability table
# (display only: the full table keeps every column at full precision); and
# one Update per page that recomputes, in turn and under one progress
# message, every result an edit left from before, and never starts one
# never requested.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

ur_state <- function(fixture = sieve_hs9_fixture()) {
  siever_test_study(fixture)
}
ur_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}
ur_html <- function(x) paste(as.character(unlist(x)), collapse = " ")
ur_ui <- function(x) paste(as.character(x), collapse = " ")
ur_reliab_inputs <- function(session, ...) {
  defaults <- list(
    scope = "construct", treatment = "study", missing = "listwise",
    denominator = "observed", ci_method = "recommended", conf_level = 0.95,
    B = 40, boot_seed = 7, show_alpha = FALSE, show_difference = FALSE,
    score_method = "Bartlett"
  )
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}
# The header of a displayed table (DT serves the rows to the browser; the
# widget holds the header).
ur_header <- function(w) {
  x <- jsonlite::fromJSON(unclass(w)[1])$x
  hdr <- regmatches(x$container, gregexpr("<th>[^<]*</th>", x$container))[[1]]
  gsub("</?th>", "", hdr)
}
ur_count <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$constructs <- 0L
  seen$cfa <- 0L
  seen$notes <- character(0)
  seen$progress <- character(0)
  real_construct <- sieve_reliability_construct
  real_cfa <- sieve_run_cfa
  real_progress <- shiny::withProgress
  testthat::local_mocked_bindings(
    sieve_reliability_construct = function(...) {
      seen$constructs <- seen$constructs + 1L
      real_construct(...)
    },
    sieve_run_cfa = function(...) {
      seen$cfa <- seen$cfa + 1L
      real_cfa(...)
    },
    sieve_notify = function(message, type = "message", duration = 8) {
      seen$notes <- c(seen$notes, message)
      invisible(NULL)
    },
    withProgress = function(expr, min = 0, max = 1, value = NULL, message = NULL, detail = NULL, ...) {
      seen$progress <- c(seen$progress, message %||% "")
      real_progress(expr, min = min, max = max, value = value, message = message, detail = detail, ...)
    },
    .env = env
  )
  seen
}

# ---- The compact Reliability table ------------------------------------------

test_that("the Reliability table shows the compact columns by default; Show all columns adds the rest; the values are those of the full table", {
  st <- ur_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    ur_reliab_inputs(session)
    session$setInputs(run_go = 1)
    full <- reliab()
    header <- ur_header(output$omega_tbl)
    # Omega (model-implied denominator) and its immediate interval are in
    # the compact view, so every row has an interval in view before any
    # bootstrap is run.
    expect_identical(header, c(
      "Construct", "k items", "Coefficient", "Estimate", "95% CI", "Interval method", "N used",
      "Largest one-factor residual r (item pair)", "Omega (model-implied denominator)",
      "Model-implied 95% CI"
    ))
    shown <- table_display()$data
    # Display only: the estimate is the full-precision value, the interval
    # and the residual are written from the full table's values.
    expect_identical(shown$omega, full$omega)
    expect_identical(shown$n_used, full$n_used)
    expect_true(all(full$uncertainty == "pending"))
    # (The cell is short: it does not say where the button is, which would
    # wrap every row.)
    expect_identical(shown$ci, rep("not run", 3L))
    expect_identical(shown$interval, rep("Bootstrap percentile, 40 replications", 3L))
    expect_true(all(grepl("^\\[0\\.[0-9]{3}, 0\\.[0-9]{3}\\]$", shown$model_ci)))
    res <- results()
    expect_identical(
      shown$residual,
      unname(vapply(res, function(r) sieve_reliability_residual_text(r$residuals), character(1)))
    )
    # Show all columns: the rest of the columns the table had, with the
    # same values.
    session$setInputs(show_all = TRUE, show_alpha = TRUE, show_difference = TRUE)
    header <- ur_header(output$omega_tbl)
    for (h in c(
      "Robust minus model-implied omega", "Summed residual covariance, 1'(S - \u03a3\u0302)1",
      "Omega (model-implied denominator)", "Model-implied 95% CI", "Model-implied interval method",
      "One-factor RMSEA [90% CI]", "Missing data", "H coefficient", "H", "H 95% CI",
      "\u03b1 (comparison)", "\u03b1 95% CI", "\u03b1 interval method", "\u03b1 N used", "\u03b1 status"
    )) {
      expect_true(h %in% header, label = h)
    }
    shown <- table_display()$data
    ci <- function(l, u, otherwise) ifelse(is.finite(l) & is.finite(u), sprintf("[%.3f, %.3f]", l, u), otherwise)
    expect_identical(shown$model_omega, full$model_omega)
    expect_identical(shown$model_ci, ci(full$model_lower, full$model_upper, full$model_interval))
    expect_identical(shown$H, full$H)
    expect_identical(shown$H_ci, ci(full$H_lower, full$H_upper, "not available"))
    expect_identical(shown$alpha, full$alpha)
    expect_identical(shown$alpha_ci, ci(full$alpha_lower, full$alpha_upper, full$alpha_interval))
    expect_identical(shown$rmsea_text, full$rmsea_text)
    expect_identical(shown$omega_minus_model, full$omega_minus_model)
    # The toggles recompute nothing and change no stored value.
    expect_identical(reliab(), full)
    # Alpha's estimate and interval are shown in the compact view when
    # alpha is asked for; its method, N and status with Show all columns.
    session$setInputs(show_all = FALSE)
    header <- ur_header(output$omega_tbl)
    expect_true(all(c("\u03b1 (comparison)", "\u03b1 95% CI") %in% header))
    expect_false(any(c("\u03b1 N used", "One-factor RMSEA [90% CI]", "H coefficient") %in% header))
  })
})

test_that("a completed interval is written in one column from the full-precision limits", {
  st <- ur_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    ur_reliab_inputs(session, denominator = "model_implied", ci_method = "mlr")
    session$setInputs(run_go = 1)
    full <- reliab()
    shown <- table_display()$data
    expect_true(all(is.finite(full$ci_lower)))
    expect_identical(shown$ci, sprintf("[%.3f, %.3f]", full$ci_lower, full$ci_upper))
    expect_identical(shown$interval, full$interval)
    expect_identical(table_display()$round, "omega")
    expect_true(all(c("ci", "residual") %in% table_display()$text))
  })
})

# ---- Results first -----------------------------------------------------------

test_that("Reliability page: the method line and the table come before the explanation, which is closed below it", {
  ui <- ur_ui(mod_reliab_ui("reliab"))
  at <- function(x) regexpr(x, ui, fixed = TRUE)[1]
  expect_gt(at("id=\"reliab-method_line\""), 0)
  expect_lt(at("id=\"reliab-gate\""), at("id=\"reliab-method_line\""))
  expect_lt(at("id=\"reliab-method_line\""), at("id=\"reliab-omega_tbl\""))
  expect_lt(at("id=\"reliab-show_all\""), at("id=\"reliab-omega_tbl\""))
  # The explanation is below the table: its first paragraph visible
  # (footnote), the rest in a closed details element.
  expect_lt(at("id=\"reliab-omega_tbl\""), at("id=\"reliab-footnote\""))
  expect_lt(at("id=\"reliab-omega_tbl\""), at("why \u03c9, not \u03b1"))
  expect_lt(at("id=\"reliab-omega_tbl\""), at("Coefficient Omega, \u03c9, is the reliability coefficient here."))
  details <- regmatches(ui, regexpr("<details[^>]*>\\s*<summary>More on how to read this table[^<]*</summary>", ui))
  expect_length(details, 1L)
  expect_true(grepl("open", sub("<summary>.*$", "", details), fixed = TRUE))
  expect_lt(at("More on how to read this table"), at("Coefficient Omega, \u03c9, is the reliability coefficient here."))
  st <- ur_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    ur_reliab_inputs(session)
    session$setInputs(run_go = 1)
    line <- ur_text(output$method_line)
    expect_match(line, paste0(
      "Method: Robust omega (observed denominator) from each construct's one-factor model ",
      "(DMAR::reliability_omega, ML; factor variance fixed to 1), complete cases, N = 301; ",
      "95% interval: Bootstrap percentile, not run yet."
    ), fixed = TRUE)
    # The first paragraph reads the compact columns; the rest of the
    # explanation is below it, in full.
    first <- ur_text(output$footnote)
    expect_match(first, "The 95% CI column gives the limits", fixed = TRUE)
    expect_match(first, "Show all columns adds", fixed = TRUE)
    more <- ur_text(output$footnote_more)
    expect_match(more, "signals redundancy, not better measurement", fixed = TRUE)
    expect_match(more, "H is the construct replicability", fixed = TRUE)
    expect_match(more, "Conventional reference values (Nunnally", fixed = TRUE)
  })
})

test_that("Items page: the model line, the one status line, the overall model and the reliability baseline sit above the grid; the explanation and the tools below it", {
  ui <- ur_ui(mod_winnow_ui("winnow"))
  at <- function(x) regexpr(x, ui, fixed = TRUE)[1]
  pool <- at("id=\"winnow-pool\"")
  expect_gt(pool, 0)
  for (id in c("model_line", "status_line", "overall_fit", "rel_summary", "exclude_selected", "include_selected")) {
    expect_lt(at(sprintf("id=\"winnow-%s\"", id)), pool, label = id)
  }
  for (x in c("id=\"winnow-model_note\"", "More on how these diagnostics are computed", "Column Guide",
    "How to Use the Grid", "Batch assignments, group names, and undo", "Prepare an Expert Review Brief")) {
    expect_gt(at(x), pool, label = x)
  }
  # No banner of its own for the thin-construct warning, and the item
  # actions are named Exclude and Include.
  expect_false(grepl("id=\"winnow-warn\"", ui, fixed = TRUE))
  expect_false(grepl("Include selected / Restore", ui, fixed = TRUE))
  expect_match(ui, ">Include selected<", fixed = TRUE)
  # The details below the grid are closed.
  more <- regmatches(ui, regexpr("<details[^>]*>\\s*<summary>More on how these diagnostics are computed", ui))
  expect_length(more, 1L)
  expect_true(grepl("open", more, fixed = TRUE))
})

test_that("Items page: the overall model is one line without a banner, and what waits is said once, in the status line", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  testthat::local_mocked_bindings(
    sieve_fit_wait = function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L, ...) 100,
    withProgress = function(expr, ...) expr
  )
  st <- ur_state()
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    expect_identical(fit_run()$status, "not run")
    fit <- ur_html(output$overall_fit)
    expect_false(grepl("alert", fit, fixed = TRUE))
    expect_match(ur_text(output$overall_fit), "Overall measurement model: not fit yet.", fixed = TRUE)
    status <- ur_html(output$status_line)
    expect_identical(lengths(regmatches(status, gregexpr("class=\"alert", status, fixed = TRUE))), 1L)
    expect_match(status, "-update_go\"", fixed = TRUE)
    # The caveat on AIC is below the grid, not in the line.
    session$setInputs(update_go = 1)
    expect_false(grepl("AIC is for comparing models", ur_text(output$overall_fit), fixed = TRUE))
    expect_match(ur_text(output$overall_note), "AIC is for comparing models (lower is better)", fixed = TRUE)
    # The reliability baseline above the grid, its explanation below.
    expect_match(ur_text(output$rel_summary), "Current reliability by construct", fixed = TRUE)
    expect_false(grepl("signals redundancy", ur_text(output$rel_summary), fixed = TRUE))
    expect_match(ur_text(output$rel_note), "signals redundancy, not better measurement", fixed = TRUE)
    # A construct with fewer than three included items is named beside the
    # reliability line.
    m <- shiny::isolate(st$meta)
    m$status[m$item == m$item[1]] <- "parked"
    st$meta <- m
    session$flushReact()
    # The note says "items" and counts them.
    expect_match(ur_text(output$rel_summary), "Below three included items: Visual (2 of 3 included)", fixed = TRUE)
  })
})

# ---- One Update per page -----------------------------------------------------

test_that("Reliability: one Update in the status line recomputes the table and the factor scores in turn, and starts no bootstrap", {
  seen <- ur_count()
  st <- ur_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    ur_reliab_inputs(session)
    session$setInputs(run_go = 1, scores_go = 1)
    expect_identical(demand$status()$state, "current")
    expect_identical(demand_scores$status()$state, "current")
    expect_identical(c(seen$constructs, seen$cfa), c(3L, 1L))
    it <- shiny::isolate(state$meta$item[1])
    sieve_apply_reverse(state, it, origin = "test")
    sieve_confirm_types(state)
    session$flushReact()
    expect_identical(demand$status()$state, "stale")
    expect_identical(demand_scores$status()$state, "stale")
    gate <- ur_html(output$gate)
    # One status line, one action: Update with the combined estimate.
    # (testServer's namespace is proxy1, so the ids are read by their suffix.)
    expect_match(gate, "-update_go\"", fixed = TRUE)
    expect_false(grepl("-run_go\"", gate, fixed = TRUE))
    expect_false(grepl("-scores_go\"", gate, fixed = TRUE))
    expect_match(ur_text(output$gate), "Update (", fixed = TRUE)
    expect_match(
      ur_text(output$gate),
      sprintf(
        "The data or the item pool changed since the reliability table and the factor scores were computed (what changed: %s reverse scored).",
        it
      ),
      fixed = TRUE
    )
    seen$progress <- character(0)
    seen$notes <- character(0)
    session$setInputs(update_go = 1)
    expect_identical(demand$status()$state, "current")
    expect_identical(demand_scores$status()$state, "current")
    expect_identical(c(seen$constructs, seen$cfa), c(6L, 2L))
    # In turn, under the page's progress message, then announced once.
    expect_match(seen$progress[1], "^Updating reliability table and factor scores \\(")
    expect_match(seen$notes[length(seen$notes)],
      "^Updated the reliability table and factor scores in [0-9.]+ seconds\\.$")
    # The bootstrap keeps its own button and was not started.
    expect_false(isTRUE(boot_running()))
    expect_true(all(reliab()$uncertainty == "pending"))
    expect_false(grepl("update_go", ur_html(output$gate), fixed = TRUE))
  })
})

test_that("Reliability: Update never starts a result never requested, and a table computed under other settings is updated by it", {
  seen <- ur_count()
  st <- ur_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    ur_reliab_inputs(session)
    session$setInputs(run_go = 1)
    it <- shiny::isolate(state$meta$item[2])
    sieve_apply_reverse(state, it, origin = "test")
    sieve_confirm_types(state)
    session$flushReact()
    expect_identical(demand_scores$status()$state, "not_run")
    session$setInputs(update_go = 1)
    expect_identical(demand$status()$state, "current")
    # The factor scores were never requested: still not computed.
    expect_identical(demand_scores$status()$state, "not_run")
    expect_identical(seen$cfa, 0L)
    expect_match(seen$notes[length(seen$notes)], "^Updated the reliability table in ")
    # Another confidence level: the table computed before is offered again
    # through the same Update.
    session$setInputs(conf_level = 0.9)
    expect_identical(demand$status()$state, "not_run")
    gate <- ur_text(output$gate)
    expect_match(gate, "The settings changed since the reliability table was computed", fixed = TRUE)
    expect_match(gate, "Update (", fixed = TRUE)
    session$setInputs(update_go = 2)
    expect_identical(demand$status()$state, "current")
    expect_identical(reliab()$uncertainty, rep("pending", 3L))
  })
})

test_that("Reliability: the AIPE planner prefilled from a construct follows its estimate after Update", {
  st <- ur_state()
  testthat::local_mocked_bindings(sieve_notify = function(...) invisible(NULL))
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    ur_reliab_inputs(session)
    session$setInputs(run_go = 1)
    invisible(output$omega_tbl)
    session$flushReact()
    session$setInputs(plan_prefill = "Visual")
    before <- reliab()$omega[reliab()$construct == "Visual"]
    expect_identical(prefill_last()$omega, before)
    # Visual's first item reverse scored: its omega changes on Update, and
    # the planner reads the new value.
    it <- shiny::isolate(state$meta$item[1])
    sieve_apply_reverse(state, it, origin = "test")
    sieve_confirm_types(state)
    session$flushReact()
    session$setInputs(update_go = 1)
    invisible(output$omega_tbl)
    session$flushReact()
    after <- reliab()$omega[reliab()$construct == "Visual"]
    expect_false(isTRUE(all.equal(after, before)))
    expect_identical(prefill_last()$omega, after)
  })
})

test_that("Items: Update computes what waits in turn under one progress message and says when it is done", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  seen <- ur_count()
  testthat::local_mocked_bindings(
    sieve_fit_wait = function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L, ...) 100,
    sieve_diagnostics_wait = function(...) 30
  )
  st <- ur_state()
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    up <- update_plan()
    expect_true(up$diag)
    expect_true(up$model)
    expect_match(ur_text(output$status_line), "The item diagnostics are not computed yet, nor is the overall model.", fixed = TRUE)
    seen$progress <- character(0)
    session$setInputs(update_go = 1)
    # One progress message for both computations.
    expect_identical(length(seen$progress), 1L)
    expect_match(seen$progress, "^Updating the Items page \\(")
    expect_false(is.null(diag()))
    expect_false(is.null(overall_fit()))
    expect_match(seen$notes[length(seen$notes)],
      "^Update finished: the item diagnostics \\([0-9.]+ seconds\\); the overall model \\([0-9.]+ seconds\\)\\.$")
    # An exclusion: both from before, one sentence, one Update.
    m <- shiny::isolate(st$meta)
    m$status[m$item == m$item[9]] <- "parked"
    st$meta <- m
    session$flushReact()
    expect_match(
      ur_text(output$status_line),
      "The item pool changed since the item diagnostics and the overall model were computed; they are withheld until they are updated.",
      fixed = TRUE
    )
    seen$progress <- character(0)
    session$setInputs(update_go = 2)
    expect_identical(length(seen$progress), 1L)
    expect_identical(fit_run()$status, "ran")
    # The diagnostics' exploratory fit may run in a background R process
    # (DMAR::fa_exploratory()).
    until <- proc.time()[["elapsed"]] + 180
    while (is.null(diag()) && proc.time()[["elapsed"]] < until) {
      Sys.sleep(0.2)
      session$elapse(500)
    }
    expect_false(is.null(diag()))
    # Another model choice: only the overall model waits, said in one
    # sentence with the one Update.
    session$setInputs(factor_cov = "orthogonal")
    expect_match(ur_text(output$status_line), "The overall model is not fit yet. Update computes the overall model, estimated at", fixed = TRUE)
  })
})

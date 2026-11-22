# The EFA and Dimensionality pages put their result first (the status line,
# a one-line method statement, then the result, with the explanations below
# it) and update what an edit made stale with one Update in their status
# line.
# The values are checked against a first run on the same inputs, so the
# route to a result (the Update) changes no number.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("psych")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")
withr::local_options(sieve.cores = 2L) # at most two workers

ue_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}
ue_html <- function(x) paste(as.character(unlist(x)), collapse = "")
ue_page <- function(x) paste(as.character(x), collapse = "")

# The bundled nine tests, loaded and confirmed, with a dataset identity.
ue_nine <- function() {
  siever_test_study(configure = function(s) s$roles <- NULL)
}

# Exclude the first included item, as the Items tab does.
ue_exclude_first <- function(st) {
  sieve_set_case_status(st, 1L, "excluded", "Update test")
}

# A worker that fits in this process and is finished when it is polled.
ue_worker <- function(data, key, k) {
  p <- new.env(parent = emptyenv())
  p$is_alive <- function() FALSE
  p$kill <- function() invisible(TRUE)
  p$get_result <- function() lapply(k, function(kk) sieve_efa_fit_by_k(data, key$estimator, key$ordered,
    rotation = key$rotation, ks = kk, missing = key$missing
  ))
  p
}

ue_drain <- function(jobs, tick, session) {
  for (i in seq_len(200)) {
    if (!sieve_efa_jobs_active(jobs)) break
    sieve_efa_jobs_poll(jobs)
  }
  tick(shiny::isolate(tick()) + 1L)
  session$flushReact()
}

# ---- EFA: results first ------------------------------------------------------

test_that("the EFA page puts one status line, the method and fit lines and the display row before the pattern matrix, and its explanation below", {
  html <- ue_page(mod_efa_ui("efa"))
  at <- function(x) regexpr(x, html, fixed = TRUE)[[1]]
  ids <- c(
    'id="efa-status_line"', 'id="efa-efa_method"', 'id="efa-efa_fit_line"',
    'id="efa-display_construct"', 'id="efa-display_facet"', 'id="efa-dl_numeric"',
    'id="efa-pattern_tbl"', 'id="efa-efa_overview"', 'id="efa-efa_code"'
  )
  pos <- vapply(ids, at, integer(1))
  expect_true(all(pos > 0))
  expect_identical(order(pos), seq_along(pos))
  # No second line of status above the result, and no standing button: the
  # status line carries the page's one action.
  for (id in c("efa-gate", "efa-job_status", "efa-efa_go", "efa-update_go")) {
    expect_false(grepl(sprintf('id="%s"', id), html, fixed = TRUE), info = id)
  }
  # Each table comes before its explanation.
  expect_lt(at('id="efa-pattern_tbl"'), at("standardized regression weight on a"))
  expect_lt(at('id="efa-structure_tbl"'), at("holds the simple (zero-order) correlation"))
  expect_lt(at('id="efa-pattern_tbl"'), at('id="efa-factor_order_pattern"'))
  expect_lt(at('id="efa-vacc_tbl"'), at('id="efa-vacc_note"'))
  expect_lt(at('id="efa-ss_items"'), at('id="efa-ss_note"'))
  # The display filters and the download share one compact row.
  row_start <- at("sieve-efa-display")
  expect_true(row_start > 0 && row_start < at('id="efa-display_construct"'))
  expect_lt(at('id="efa-dl_numeric"'), at('id="efa-display_pool_note"'))
  # The main area scrolls as a document (at 1440 by 900, a fillable
  # column would squeeze the table card to four rows).
  expect_false(grepl("main bslib-gap-spacing html-fill-container", html, fixed = TRUE))
})

test_that("the EFA page's status line runs a first result, and one Update refits the loadings and the factor count comparison that an exclusion made stale", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  st <- ue_nine()
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log))
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(
      n_factors = 3L, rotation = "oblimin", fm = "ml", cor_type = "auto",
      estimator = "study", suppress = 0.3, sort_loadings = TRUE, count_to = 3L, count_criterion = "BIC"
    )
    # Not visible, so nothing ran: the one action is the result's own
    # button, with its wait.
    expect_null(efa())
    line <- ue_html(output$status_line)
    expect_match(ue_text(line), "The selected EFA has not been fit for these data and settings.", fixed = TRUE)
    expect_match(ue_text(line), "Run selected EFA (", fixed = TRUE)
    expect_match(line, '-efa_go"', fixed = TRUE)
    expect_false(grepl("-update_go", line, fixed = TRUE))
    session$setInputs(efa_go = 1L)
    expect_null(output$status_line)
    # One line each: what produced the loadings, and the fit of the same model.
    method <- ue_text(output$efa_method)
    expect_match(method,
      "Model: DMAR::fa_exploratory, ML with FIML, direct oblimin rotation, k = 3; 9 items; FIML, N = 301; 95% intervals (the RMSEA's at 90%).",
      fixed = TRUE
    )
    X <- shiny::isolate(st$items)[, shiny::isolate(st$meta)$item[shiny::isolate(st$meta)$status == "active"]]
    unrotated <- DMAR::fa_exploratory(X, nfactors = 3, estimator = "ML", missing = "fiml", rotation = "none") # the auto estimator
    fm <- attr(unrotated, "fit_measures")
    v <- function(t) fm$value[fm$term == t]
    # The RMSEA interval is at the RMSEA level, 90%, DMAR's rmsea_conf_level
    # default, for the same unrotated model, computed here independently of
    # the page.
    expect_identical(attr(fm, "conf_level"), 0.90)
    expect_match(ue_text(output$efa_fit_line),
      sprintf(
        "Chi-square = %.3f on %s df, p = %s; RMSEA = %.3f, 90%% CI [%.3f, %.3f]; CFI = %.3f; TLI = %.3f; SRMR = %.3f.",
        v("chi_square"), format(v("df")), format.pval(v("p_chi_square"), digits = 3, eps = 1e-4),
        v("rmsea"), v("rmsea_ci_lower"), v("rmsea_ci_upper"), v("cfi"), v("tli"), v("srmr")
      ),
      fixed = TRUE
    )
    # The overview below the tables: its first paragraph visible, the rest
    # under a closed label; the fit is not repeated there.
    ov <- ue_html(output$efa_overview)
    expect_match(ov, "What this analysis is.", fixed = TRUE)
    expect_match(ov, "<details open", fixed = TRUE)
    expect_match(ov, "More on how to read this", fixed = TRUE)
    expect_true(grepl("<details open", ov, fixed = TRUE))
    expect_false(grepl("Chi-square =", ov, fixed = TRUE))
    session$setInputs(count_go = 1L)
    efa_dmar_settle(session)
    expect_identical(count_run()$status, "ran")

    # An exclusion makes both from before: withheld, and one Update.
    ue_exclude_first(st)
    session$flushReact()
    expect_null(efa())
    line <- ue_html(output$status_line)
    expect_match(ue_text(line),
      "changed since the loadings and the factor count comparison were computed; both are withheld until they are updated.",
      fixed = TRUE
    )
    expect_match(ue_text(line), "Update (", fixed = TRUE)
    expect_match(line, '-update_go"', fixed = TRUE)
    expect_false(grepl("-efa_go", line, fixed = TRUE))
    guide <- shiny::isolate(state$page_results$efa)
    expect_identical(guide[[session$ns("primary")]]$state, "stale")
    expect_identical(guide[[session$ns("primary")]]$action, "Update")
    expect_identical(guide[[session$ns("factor_count")]]$state, "stale")
    expect_identical(guide[[session$ns("factor_count")]]$action, "Update")
    expect_match(guide[[session$ns("primary")]]$sentence, "Update, in the status line, refits", fixed = TRUE)

    session$setInputs(update_go = 1L)
    efa_dmar_settle(session)
    expect_identical(nrow(unclass(efa()$loadings)), 9L)
    expect_null(output$status_line, info = ue_text(output$status_line))
    after <- count_run()
    expect_identical(after$status, "ran")
    expect_identical(after$table$factors, 1:3)
    # The same numbers a first fit of the edited pool gives.
    X <- shiny::isolate(st$items)[, shiny::isolate(st$meta)$item[shiny::isolate(st$meta)$status == "active"]]
    direct <- DMAR::fa_exploratory(X, nfactors = 3, estimator = "ML", missing = "fiml", rotation = "oblimin") # the auto estimator
    expect_equal(unname(efa()$communality), unname(sieve_efa_dmar_solution(direct)$communality), tolerance = 1e-6)
    ref <- DMAR::factor_count(X, max_factors = 3, estimator = "ML", missing = "fiml", B = 0)
    expect_equal(after$table$chi_square, ref$chi_square, tolerance = 1e-10)
    expect_equal(after$table$rmsea, ref$rmsea, tolerance = 1e-10)
    expect_identical(attr(after$table, "method")$N, nrow(X))
  })
})

# ---- Dimensionality: results first --------------------------------------------

test_that("the Dimensionality page puts the evidence summary and the criteria table before the plots, and the explanations last", {
  html <- ue_page(mod_retention_ui("ret"))
  at <- function(x) regexpr(x, html, fixed = TRUE)[[1]]
  marks <- c(
    'id="ret-gate"', 'id="ret-status_line"', 'id="ret-method_line"', 'id="ret-decide_evidence"',
    'id="ret-decide_k"', 'id="ret-summary_tbl"', 'id="ret-summary_reco"',
    'data-value="Scree and Parallel Analysis"', 'id="ret-mapvss_tbl"', 'id="ret-decide_tbl"', 'id="ret-fit_tbl"',
    'id="ret-overview"'
  )
  pos <- vapply(marks, at, integer(1))
  expect_true(all(pos > 0))
  expect_identical(order(pos), seq_along(pos))
  # The VSS table precedes its note; the fit by k's estimator is a
  # closed setting below its table.
  expect_lt(at('id="ret-mapvss_tbl"'), at('id="ret-mapvss_note"'))
  expect_lt(at('id="ret-fit_tbl"'), at("Settings: the estimator for the model fit by k"))
  expect_lt(at("Settings: the estimator for the model fit by k"), at('id="ret-estimator"'))
  expect_false(grepl('data-value="Overview"', html, fixed = TRUE))
  # The main area scrolls as a document (at 1440 by 900, a fillable
  # column would show 32 of the evidence and decision card's 432 pixels).
  expect_false(grepl("main bslib-gap-spacing html-fill-container", html, fixed = TRUE))

  st <- ue_nine()
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 200L, max_k = 4, seed = 7, workers = 2, estimator = "study")
    expect_match(ue_text(output$method_line),
      paste0(
        # Parallel analysis is DMAR's, and so are VSS's solutions.
        "Model: Pearson correlations (study setting), 9 items, n = 301 complete cases; ",
        "DMAR::parallel_analysis for parallel analysis; DMAR::fa_exploratory (ML) for VSS; lavaan::efa (ML with FIML) for the fit by k."
      ),
      fixed = TRUE
    )
    ov <- ue_html(output$overview)
    expect_match(ov, "How many factors? Reading dimensionality before EFA", fixed = TRUE)
    first <- regexpr("is the first inferential", ov, fixed = TRUE)
    closed <- regexpr("<details open", ov, fixed = TRUE)
    expect_true(first > 0 && closed > first)
    # The rest is kept, under the closed label.
    expect_gt(regexpr("Lim and Jahng (2019)", ov, fixed = TRUE), closed)
    expect_gt(regexpr("What was computed:", ov, fixed = TRUE), closed)
  })
})

test_that("after an exclusion, parallel analysis and VSS and the model fit by k wait for the one Update, which computes them in turn to the values of a first run", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  st <- ue_nine()
  ran <- new.env(parent = emptyenv())
  ran$vss <- 0L
  real_vss <- sieve_retention_vss
  # The model fit by k runs in two real background workers here: a worker
  # that finishes inside its own submission does not deliver a sequence
  # requested after an edit, which a real worker does.
  local_mocked_bindings(
    sieve_retention_vss = function(...) {
      ran$vss <- ran$vss + 1L
      real_vss(...)
    }
  )
  # VSS's solutions are DMAR::fa_exploratory()'s, and DMAR refuses the nine
  # tests' four factor solution only after iterating to lavaan's limit, so
  # VSS at four factors does not run by itself; three keeps the subject, a
  # first result that runs by itself and an Update that computes it again.
  inputs <- function(session) {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 200L, max_k = 3, seed = 7, workers = 2, estimator = "study")
  }
  # Drive the parallel analysis job and the background fit to their end; a
  # run still going at the limit fails here, before the checks of its results.
  finish <- function(session, running, pending, vss = function() FALSE) {
    siever_wait_until(
      function() !isTRUE(running()) && is.null(pending()) && !isTRUE(vss()),
      "Parallel analysis, VSS or the model fit by k",
      step = function() {
        Sys.sleep(0.05)
        session$elapse(100)
      }
    )
  }
  shiny::testServer(mod_retention_server, args = list(state = st), {
    inputs(session)
    cr1 <- criteria()
    # Every criterion ran (VSS at three factors, the largest probed, reads
    # "no count": its optimum is at the largest k probed).
    expect_true(all(cr1$status %in% c("ran", "no count")))
    expect_false(any(cr1$status == "not run"))
    expect_identical(ran$vss, 1L)
    expect_null(output$status_line)

    ue_exclude_first(st)
    session$flushReact()
    cr2 <- criteria()
    # From before: withheld, not computed again by itself.
    expect_true(all(cr2$status == "not run"))
    expect_identical(ran$vss, 1L)
    expect_true(isTRUE(vss_run()$stale))
    line <- ue_html(output$status_line)
    expect_match(ue_text(line),
      paste0(
        "The included items, their values, the respondents or the settings changed since parallel analysis, ",
        "VSS and the model fit by k were computed; they are withheld until updated."
      ),
      fixed = TRUE
    )
    expect_match(ue_text(line), "Update (", fixed = TRUE)
    expect_match(line, '-update_go"', fixed = TRUE)
    expect_match(ue_text(output$mapvss_note), "Very Simple Structure are from before", fixed = TRUE)
    session$flushReact()
    guide <- shiny::isolate(state$page_results$retention)
    for (id in c("result_pa", "result_vss", "result_fit")) {
      expect_identical(guide[[session$ns(id)]]$state, "stale", info = id)
      expect_identical(guide[[session$ns(id)]]$action, "Update", info = id)
    }

    session$setInputs(update_go = 1L)
    finish(session, pa_running, fit_pending, vss_running)
    cr3 <- criteria()
    expect_true(all(cr3$status %in% c("ran", "no count")))
    expect_false(any(cr3$status == "not run"))
    expect_identical(ran$vss, 2L)
    expect_false(isTRUE(update_queue$active))
    expect_null(output$status_line)
    held <- list(cr = cr3, pa = pa_run()$empirical, ref = pa_sel()$values, vss = vss_run()$table,
      fit = fit_run()$table)
    assign("ue_updated", held, envir = ran)
  })
  # A first run on the edited pool gives the same values (each criterion
  # started by its own button if the measured rates make it wait).
  st2 <- ue_nine()
  ue_exclude_first(st2)
  shiny::testServer(mod_retention_server, args = list(state = st2), {
    inputs(session)
    if (identical(pa_run()$status, "not run")) session$setInputs(pa_go = 1L)
    finish(session, pa_running, fit_pending, vss_running)
    if (identical(vss_run()$status, "not run")) session$setInputs(vss_go = 1L)
    if (identical(fit_run()$status, "not run")) session$setInputs(fit_go = 1L)
    finish(session, pa_running, fit_pending, vss_running)
    cr <- criteria()
    expect_identical(cr$suggested, ran$ue_updated$cr$suggested)
    expect_identical(pa_run()$empirical, ran$ue_updated$pa)
    expect_identical(pa_sel()$values, ran$ue_updated$ref)
    expect_identical(vss_run()$table, ran$ue_updated$vss)
    expect_equal(fit_run()$table$rmsea, ran$ue_updated$fit$rmsea, tolerance = 1e-12)
    expect_equal(fit_run()$table$cfi, ran$ue_updated$fit$cfi, tolerance = 1e-12)
  })
})

test_that("VSS follow the run rule: a first run estimated above two seconds waits for its own button and states its wait", {
  # VSS's solutions are DMAR::fa_exploratory()'s, and a rank DMAR is
  # expected to refuse (the nine tests' four and five factor solutions) is
  # counted at a refusal's cost; a run on request above two seconds goes to
  # a background R process. The solutions are point estimates only, whose
  # time grows with the items: eight factors on 68 items took 15.2 seconds
  # in all, 1.09, 0.83, 1.32, 1.26, 1.91, 1.82, 2.54 and 4.44 for one to
  # eight (DMAR 1.1.0 from CRAN, n = 600), so they wait for Run VSS; a wait
  # without a term for the items would put them at 1.6 seconds and start
  # them unasked.
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  expect_lt(sieve_vss_wait(3, 9), 2)
  expect_gt(sieve_vss_wait(8, 68), 2)
  expect_equal(sieve_vss_wait(8, 68), 15.21, tolerance = 0.25)
  expect_gt(sieve_vss_wait(20, 68), 6)
  expect_true(is.na(sieve_vss_wait(NA, 9)))
  fx <- sieve_hs9_fixture()
  R9 <- stats::cor(fx$data[, fx$item_cols])
  expect_gt(sieve_vss_wait(8, 9, R = R9), sieve_vss_wait(8, 9))
  expect_gt(sieve_vss_wait(8, 9, R = R9), 2)
  st <- ue_nine()
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(
    sieve_vss_wait = function(max_k, p, R = NULL) 30,
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE)
  )
  shiny::testServer(mod_retention_server, args = list(state = st), {
    # Three factors: the stand-in worker runs VSS in this process, and the
    # four factor solution of the nine tests is refused only after lavaan
    # iterates to its limit.
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 200L, max_k = 3, seed = 7, workers = 2, estimator = "study")
    v <- vss_run()
    expect_identical(v$status, "not run")
    expect_false(isTRUE(v$stale))
    cr <- criteria()
    expect_identical(cr$status[cr$id %in% c("map", "vss1", "vss2")], rep("not run", 2L))
    expect_match(ue_html(output$summary_tbl), '-vss_go_table"', fixed = TRUE)
    expect_match(ue_html(output$mapvss_note), '-vss_go"', fixed = TRUE)
    expect_match(ue_text(output$mapvss_note), "Run VSS (about 30 seconds)", fixed = TRUE)
    expect_match(ue_text(output$status_line), "Not run yet for these data: VSS (about 30 seconds)", fixed = TRUE)
    expect_false(grepl("-update_go", ue_html(output$status_line), fixed = TRUE))
    session$setInputs(vss_go = 1L)
    expect_true(vss_running())
    log$process$alive <- FALSE
    efa_dmar_settle(session)
    expect_false(vss_running())
    expect_true(vss_run()$ok)
    cr <- criteria()
    # Ran; at three factors, the largest probed, the optimum reads "no count".
    expect_true(all(cr$status[cr$id %in% c("vss1", "vss2")] %in% c("ran", "no count")))
  })
})

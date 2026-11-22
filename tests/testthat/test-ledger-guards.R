# Ledger guards, composites, retention, parallel analysis queues,
# navigation, scores, missing information and Update feedback.
# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

test_that("the item-ledger source guard detects nested direct module writes", {
  ledger_target <- function(x) {
    if (!is.call(x) || !as.character(x[[1L]])[1L] %in% c("$", "[", "[[")) return(FALSE)
    parent <- x[[2L]]
    if (identical(parent, as.name("state"))) {
      key <- x[[3L]]
      return(identical(key, as.name("meta")) || identical(key, "meta"))
    }
    ledger_target(parent)
  }
  writes <- function(x) {
    if (!is.recursive(x)) return(character())
    found <- if (is.call(x) && as.character(x[[1L]])[1L] %in% c("<-", "=", "<<-") && ledger_target(x[[2L]])) {
      paste(deparse(x), collapse = " ")
    } else character()
    parts <- as.list(x)
    children <- lapply(seq_along(parts), function(i) {
      if (identical(unname(parts[i]), alist(,)[1L])) return(character())
      y <- parts[[i]]
      if (is.call(y) || is.expression(y)) writes(y) else character()
    })
    c(found, unlist(children, use.names = FALSE))
  }
  expect_length(writes(parse(text = 'state$meta <- m; state$meta$status[i] <- "active"; state[["meta"]]$type <- x')), 3L)
  expect_length(writes(parse(text = 'm <- state$meta; state$items <- x; "state$meta <- m"')), 0L)
  root <- compat_source_root()
  skip_if(is.null(root), "source-tree guard")
  for (file in list.files(file.path(root, "R"), "^mod_.*[.]R$", full.names = TRUE)) {
    expect_equal(length(writes(parse(file))), 0L, info = basename(file))
  }
})

test_that("composite weights cover indicators and omit saturated auxiliaries", {
  x <- lavaan::HolzingerSwineford1939
  x$ageyr[seq(1, nrow(x), 7)] <- NA_real_
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6", data = x,
    missing = "ml", aux = "ageyr", std.lv = TRUE, check.post = FALSE)
  items <- paste0("x", 1:6)
  weights <- stats::setNames(c(2, 1, 1, 1, 1, 1), items)
  expect_identical(sieve_model_composite_items(fit), items)
  result <- sieve_model_composite(fit, weights)
  expect_null(result$error)
  expect_identical(result$weights, weights)
  expect_equal(result$table$estimate, unname(sieve_composite_coefficients(fit, weights)$value))
  expect_error(sieve_model_composite(fit, weights[-1L]), "all modeled items")
})

test_that("model-linked bootstrap reports per-composite retained improper counts and cautions", {
  test_frame <- environment()
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6",
    data = lavaan::HolzingerSwineford1939, std.lv = TRUE)
  local_mocked_bindings(bootstrapLavaan = function(object, R, FUN, ...) {
    point <- FUN(object)
    m <- matrix(rep(point, each = R), nrow = R, dimnames = list(NULL, names(point)))
    m[1:3, ncol(m)] <- 1
    m[1:3, 1L] <- c(1.01, 1.1, 1.2)
    # This draw has a proper model but an out-of-range coefficient.
    m[6L, 1L] <- 1.15
    m[4L, 2L] <- NA_real_
    attr(m, "error.idx") <- 5L
    m
  }, .package = "lavaan", .env = test_frame)
  result <- sieve_model_composite(fit, B = 100L, seed = 1L)
  expect_equal(result$replications$used, c(99L, 98L, 99L))
  expect_equal(result$replications$improper_kept, c(4L, 3L, 3L))
  expect_true(all(result$replications$caution))
  expect_true(all(result$draws[1:3, 1L] > 1))
  expect_equal(result$table$upper[1L], unname(stats::quantile(result$draws[, 1L], .975, na.rm = TRUE)))
  state <- siever_test_study()
  spec <- sieve_cfa_spec(sieve_build_meta(lavaan::HolzingerSwineford1939, paste0("x", 1:6),
    list(visual = paste0("x", 1:3), textual = paste0("x", 4:6))))
  shiny::testServer(function(input, output, session) {
    parent <- shiny::reactive(list(fit = fit, spec = spec))
    wires <- sieve_cfa_evidence_server(input, output, session, state, parent, function() TRUE)
  }, {
    session$setInputs(composite_B = 100L, composite_go = 1L)
    expect_match(as.character(output$composite_note$html), "3 improper retained", fixed = TRUE)
    expect_match(as.character(output$composite_note$html), "Caution:", fixed = TRUE)
    session$setInputs(composite_weights = "1,1", composite_go = 2L)
    expect_match(as.character(output$composite_note$html), "Weights must cover", fixed = TRUE)
  })
})

test_that("case decisions retain the scientific dataset identity", {
  state <- siever_test_study()
  sieve_set_case_status(state, 1L, "excluded", "Review test")
  shiny::isolate(expect_identical(state$case_actions[[1L]]$scientific_dataset_id, state$scientific_dataset_id))
})

test_that("a refitted parent keeps an earlier composite historical", {
  state <- siever_test_study()
  d <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3", data = d, std.lv = TRUE)
  spec <- sieve_cfa_spec(sieve_build_meta(d, paste0("x", 1:3), list(visual = paste0("x", 1:3))))
  shiny::testServer(function(input, output, session) {
    fit_now <- shiny::reactiveVal(fit)
    parent <- shiny::reactive(list(fit = fit_now(), spec = spec))
    sieve_cfa_evidence_server(input, output, session, state, parent, function() TRUE)
  }, {
    session$setInputs(composite_go = 1L)
    expect_false(grepl("historical", as.character(output$composite_note$html), fixed = TRUE))
    changed <- fit
    attr(changed, "review_refit") <- TRUE
    fit_now(changed)
    session$flushReact()
    expect_match(as.character(output$composite_note$html), "historical", fixed = TRUE)
    session$setInputs(composite_go = 2L)
    expect_false(grepl("historical", as.character(output$composite_note$html), fixed = TRUE))
  })
})

test_that("factor-only VSS replay preserves a peak at the probe boundary", {
  x <- withr::with_seed(111, {
    f <- matrix(stats::rnorm(1800), ncol = 3L)
    l <- matrix(0, 12L, 3L)
    l[cbind(1:12, rep(1:3, each = 4))] <- .85
    as.data.frame(f %*% t(l) + matrix(stats::rnorm(7200, sd = .4), 600L))
  })
  basis <- sieve_retention_cor(x, rep("continuous", 12))
  vss <- sieve_retention_vss(basis, max_k = 3L, seed = 1L)
  expect_true(vss$ok)
  criteria <- sieve_retention_criteria(basis, list(ok = FALSE), vss)
  env <- new.env(parent = baseenv())
  env$R_items <- basis$R
  env$n_complete <- basis$n
  expect_no_error(suppressWarnings(eval(parse(text = sieve_vss_code(vss)), env)))
  expect_equal(env$vss1_k, criteria$suggested[criteria$id == "vss1"])
  expect_equal(env$vss2_k, criteria$suggested[criteria$id == "vss2"])
  expect_equal(env$vss1_at_least, criteria$at_least[criteria$id == "vss1"])
  expect_equal(env$vss2_at_least, criteria$at_least[criteria$id == "vss2"])
  expect_true(any(is.finite(c(env$vss1_at_least, env$vss2_at_least))))
  expect_false(any(grepl("psych::vss|principal|map_values", sieve_vss_code(vss))))
})

test_that("longitudinal fitting reads canonical confirmation after a ledger change", {
  state <- siever_test_study()
  shiny::isolate({
    m <- state$meta
    m$construct[1L] <- "Changed construct"
    state$meta <- m
    expect_true(state$confirmed)
    expect_false(sieve_confirmation_status(state)$confirmed)
  })
  shiny::testServer(mod_longitudinal_server, args = list(state = state), {
    run()
    expect_match(result()$error, "Confirm the item pool", fixed = TRUE)
  })
})

test_that("confirmation uses the same normalized roles before and after project restore", {
  # An item value is missing in the data as loaded, so the declared
  # auxiliary enters the fit.
  fixture <- sieve_hs9_fixture()
  fixture$data[1:25, fixture$item_cols[1L]] <- NA_real_
  state <- siever_test_study(fixture, configure = function(s) s$roles <- NULL)
  snapshot <- sieve_project_snapshot(state)
  restored <- sieve_new_state()
  sieve_project_restore(restored, snapshot)
  shiny::isolate({
    expect_true(sieve_confirmation_status(state)$confirmed)
    expect_true(sieve_confirmation_status(restored)$confirmed)
    expect_identical(state$confirmation$roles_key, restored$confirmation$roles_key)
    expect_identical(sieve_record_input_key(sieve_replay_inputs(state)),
      sieve_record_input_key(sieve_replay_inputs(restored)))
    spec <- sieve_cfa_spec(state$meta, reserved = names(state$raw))
    before <- sieve_auxiliary_apply(spec, state, state$items)
    after <- sieve_auxiliary_apply(spec, restored, restored$items)
    expect_identical(before$spec$engine_args$aux, "age_years")
    expect_identical(after$spec$engine_args$aux, before$spec$engine_args$aux)
    expect_identical(after$data, before$data)
  })
})


test_that("role changes have an accurate confirmation description", {
  state <- siever_test_study()
  shiny::testServer(mod_ingest_server, args = list(state = state), {
    session$setInputs(cell_edit = list(item = "grade", field = "role", value = "Criterion"))
    status <- sieve_confirmation_status(state)
    expect_false(status$confirmed)
    expect_match(status$change, "variable roles changed", fixed = TRUE)
    expect_true("role" %in% attr(status$change, "kinds"))
    expect_match(confirm_status()$label, "variable roles changed", fixed = TRUE)
    expect_false(grepl("made and then undone", confirm_status()$label, fixed = TRUE))
    session$setInputs(confirm_main = 1L)
    expect_true(sieve_confirmation_status(state)$confirmed)
  })
})


test_that("a refused model-fit request keeps its reason, and the retention criteria hold no row for it", {
  basis <- sieve_retention_cor(lavaan::HolzingerSwineford1939[, paste0("x", 1:6)])
  fit <- sieve_retention_fit(basis)
  expect_false(fit$ok)
  expect_identical(fit[["n"]], NA_integer_)
  expect_match(fit$reason, "Raw item data", fixed = TRUE)
  # The model fit by k is not a criterion.
  table <- sieve_retention_criteria(basis, list(ok = FALSE), list(ok = FALSE))
  expect_identical(table$id, c("pa", "vss1", "vss2"))
})


test_that("public continuous retention preserves incomplete respondents", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
  d[1:20, 1L] <- NA_real_
  result <- sieve_retention_evidence(d, n_iter = 3L, max_k = 1L, seed = 1L)
  direct <- DMAR::cov_fiml(d)
  expect_true(result$basis$ok)
  expect_identical(result$basis$missing, "fiml")
  expect_equal(result$basis$R, stats::cov2cor(direct$S), tolerance = 1e-9)
  expect_equal(result$n, sum(stats::complete.cases(d)))
  expect_equal(result$fit$n, nrow(d))
})

test_that("an inclusion change during the first parallel analysis reruns the new pool", {
  withr::local_options(sieve.auto_run = "never", sieve.cores = 1L)
  # (The parallel analysis is DMAR's, and a run this small is computed in
  # the session at once; here the run is held in a background worker that
  # stays alive until the test lets it finish, so the inclusion change
  # arrives while it runs. A changed request stops the worker and the page's
  # Update starts the new pool's.)
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE),
    sieve_pa_units = function(...) 1e4
  )
  state <- siever_test_study()
  shiny::testServer(mod_retention_server, args = list(state = state), {
    session$setInputs(n_iter = 200L, max_k = 1L, cores = 1L, seed = 1L, pa_go = 1L)
    expect_true(pa_running())
    first <- log$process
    sieve_set_item_status(state, state$meta$item[1L], "parked", "Review test")
    session$flushReact()
    expect_false(update_current())
    for (i in seq_len(100L)) {
      if (!identical(log$process, first)) log$process$alive <- FALSE
      session$elapse(50L)
      status <- pa_run()
      if (!pa_running() && isTRUE(status$ok) && identical(pa_store()$key, pa_key())) break
    }
    expect_false(pa_running())
    expect_identical(pa_run()$status, "ran")
    expect_identical(pa_store()$key, pa_key())
    expect_equal(pa_run()$p, 8L)
    # The first pool's worker was stopped, never read.
    expect_false(first$alive)
    expect_gte(log$killed, 1L)
  })
})

test_that("Cancel consumes a queued inclusion refit without consuming later edits", {
  for (same_flush in c(FALSE, TRUE)) {
    state <- siever_test_study()
    calls <- 0L
    accepted <- 0L
    control <- new.env(parent = emptyenv())
    control$busy <- TRUE
    shiny::testServer(function(input, output, session) {
      inclusion <- sieve_inclusion_wire(state, function() {
        calls <<- calls + 1L
        if (control$busy) return(FALSE)
        accepted <<- accepted + 1L
        TRUE
      })
    }, {
      session$flushReact()
      item <- state$meta$item[1L]
      sieve_set_item_status(state, item, "parked", "Review test")
      if (!same_flush) session$flushReact()
      inclusion$cancel()
      before <- calls
      control$busy <- FALSE
      session$flushReact()
      for (i in seq_len(5L)) session$elapse(250L)
      expect_identical(calls, before)
      expect_identical(accepted, 0L)
      sieve_set_item_status(state, item, "active", "Review test")
      session$flushReact()
      expect_identical(accepted, 1L)
    })
  }
})

test_that("a non-Pearson retention override uses complete cases and retains raw FIML fits", {
  d <- as.data.frame(lapply(lavaan::HolzingerSwineford1939[, paste0("x", 1:6)], function(x) {
    as.integer(cut(x, stats::quantile(x, seq(0, 1, .2)), include.lowest = TRUE))
  }))
  d[1:15, 1L] <- NA_integer_
  result <- sieve_retention_evidence(d, rep("continuous", 6L), cor = "poly", n_iter = 3L, max_k = 1L, seed = 1L)
  complete <- d[stats::complete.cases(d), , drop = FALSE]
  direct <- psych::polychoric(complete, correct = .5, progress = FALSE)$rho
  expect_true(result$basis$ok)
  expect_equal(result$basis$R, direct)
  expect_equal(result$basis$n, nrow(complete))
  expect_equal(result$pa$n, nrow(complete))
  expect_equal(result$fit$n, nrow(d))
  expect_identical(result$fit$lavaan_missing, "ml")
})

test_that("Validity compares substantive constructs without auxiliary covariances", {
  withr::local_options(sieve.auto_run = "never")
  # An item value is missing, so the declared auxiliary enters the fit.
  state <- siever_test_study(configure = function(s) s$items[1:25, 1] <- NA_real_)
  shiny::testServer(mod_validity_server, args = list(state = state), {
    session$setInputs(fit_go = 1L)
    f <- fit()
    expect_true("age_years" %in% lavaan::lavInspect(f, "options")$aux)
    ss <- lavaan::standardizedSolution(f)
    lv <- lavaan::lavNames(f, "lv")
    phi <- ss[ss$op == "~~" & ss$lhs != ss$rhs & ss$lhs %in% lv & ss$rhs %in% lv, ]
    expected <- vapply(lv, function(x) max(abs(phi$est.std[phi$lhs == x | phi$rhs == x])), numeric(1))
    expect_equal(ave_df()$Max.r.with.other, unname(expected))
    invisible(output$phi_tbl)
    log <- state$codelog
    expect_true(any(grepl('validity_latents <- lavaan::lavNames', log, fixed = TRUE)))
    env <- new.env(parent = globalenv())
    env$validity_fit <- f
    env$validity_ss <- ss
    step <- tail(grep('^validity_ss\\[', log, value = TRUE), 1L)
    env$validity_latents <- lv
    replay <- eval(parse(text = step), env)
    expect_equal(replay, phi)
    expect_equal(nrow(replay), choose(length(lv), 2L))
  })
})

test_that("diagrams omit saturated auxiliaries and retain their full parameter record", {
  d <- lavaan::HolzingerSwineford1939
  f <- lavaan::cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6", data = d,
    missing = "ml", aux = "ageyr", std.lv = TRUE, check.post = FALSE)
  diagram <- sieve_diagram_spec(f, label_mode = "short")
  expect_false(any(diagram$nodes$id %in% c("ageyr", "e.ageyr")))
  expect_identical(diagram$nodes$label[diagram$nodes$kind == "observed"], paste0("x", 1:6))
  expect_true(any(diagram$parameters$lhs == "ageyr"))
  pe <- lavaan::parameterEstimates(f, remove_aux = FALSE)
  want <- pe$est[pe$lhs == "ageyr" & pe$op == "~~" & pe$rhs == "ageyr"]
  got <- diagram$parameters$est[diagram$parameters$lhs == "ageyr" & diagram$parameters$op == "~~" & diagram$parameters$rhs == "ageyr"]
  expect_length(got, 1L)
  expect_true(is.finite(got))
  expect_equal(got, want)
  expect_true(is.finite(diagram$parameters$ci_lower[diagram$parameters$lhs == "ageyr" & diagram$parameters$op == "~~" & diagram$parameters$rhs == "ageyr"]))
  expect_match(diagram$meta$caption, "Saturated auxiliaries (ageyr)", fixed = TRUE)
  expect_match(diagram$meta$caption, "parameters remain in the parameter table", fixed = TRUE)
  expect_true(all(diagram$edges$fixed[diagram$edges$op == "~~" & diagram$edges$lhs == diagram$edges$rhs & diagram$edges$lhs %in% c("visual", "textual")]))
})

test_that("Stop consumes an inclusion queued during the first parallel analysis", {
  withr::local_options(sieve.auto_run = "never", sieve.cores = 1L)
  # (The run is held in a background worker that stays alive until the test
  # lets it finish, as in the test above.)
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE),
    sieve_pa_units = function(...) 1e4
  )
  withr::defer(sieve_rate_reset())
  for (stop_id in c("pa_stop", "update_stop")) {
    # A finished run measures its rate against the mocked units; each pass
    # starts from the standard rates, so its run is long again.
    sieve_rate_reset()
    state <- siever_test_study()
    shiny::testServer(mod_retention_server, args = list(state = state), {
      session$setInputs(n_iter = 200L, max_k = 1L, cores = 1L, seed = 1L, pa_go = 1L)
      expect_true(pa_running())
      sieve_set_item_status(state, state$meta$item[1L], "parked", "Review test")
      session$flushReact()
      do.call(session$setInputs, stats::setNames(list(1L), stop_id))
      for (i in seq_len(50L)) session$elapse(100L)
      expect_false(pa_running())
      expect_false(identical(pa_store()$key, pa_key()))
      session$setInputs(pa_go = 2L)
      expect_true(pa_running())
      for (i in seq_len(100L)) {
        log$process$alive <- FALSE
        session$elapse(50L)
        if (!pa_running() && identical(pa_store()$key, pa_key())) break
      }
      expect_identical(pa_store()$key, pa_key())
    })
  }
})

test_that("page values given to the navigation select their visible workspace and page", {
  test_frame <- environment()
  calls <- list()
  local_mocked_bindings(nav_select = function(id, selected = NULL, session = NULL) {
    calls[[length(calls) + 1L]] <<- c(id = id, selected = selected)
  }, .package = "bslib", .env = test_frame)
  expect_identical(sieve_active_page(list(nav = "scoring")), "scoring")
  shiny::testServer(app_server, {
    session$setInputs(nav = "cfa")
    selected <- Filter(function(x) x[["id"]] %in% c("nav", "ws_structure"), calls)
    expect_identical(tail(selected, 2L), list(c(id = "nav", selected = "structure"), c(id = "ws_structure", selected = "cfa")))
    session$setInputs(nav = "scoring")
    selected <- Filter(function(x) x[["id"]] %in% c("nav", "ws_scores", "reliab-panels"), calls)
    expect_identical(tail(selected, 3L), list(c(id = "reliab-panels", selected = "scoring"),
      c(id = "nav", selected = "scores"), c(id = "ws_scores", selected = "reliab")))
    before <- state$codelog
    session$setInputs(study_goal = "evaluate", study_route_go = 1L)
    expect_identical(tail(calls, 2L), list(c(id = "nav", selected = "structure"), c(id = "ws_structure", selected = "cfa")))
    session$setInputs(open_scoring = 1L)
    expect_identical(tail(calls, 3L), list(c(id = "reliab-panels", selected = "scoring"),
      c(id = "nav", selected = "scores"), c(id = "ws_scores", selected = "reliab")))
    expect_identical(state$codelog, before)
  })
})

test_that("Scoring and Norms has no unrelated reliability request", {
  withr::local_options(sieve.auto_run = "never")
  state <- siever_test_study()
  shiny::testServer(mod_reliab_server, args = list(state = state), {
    session$setInputs(panels = "Reliability table")
    expect_match(paste(as.character(output$gate$html), collapse = " "), "Compute reliability", fixed = TRUE)
    session$setInputs(panels = "scoring")
    expect_null(output$gate)
    expect_identical(demand$status()$state, "not_run")
  })
})

test_that("regression score status does not inherit unavailable categorical ML scores", {
  x <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  x[] <- lapply(x, function(v) as.integer(cut(v, stats::quantile(v, 0:4 / 4), include.lowest = TRUE)))
  fit <- lavaan::cfa("F =~ x4 + x5 + x6", x, ordered = names(x), std.lv = TRUE)
  regression <- sieve_factor_score_record(fit, method = "EBM")
  ml <- sieve_factor_score_record(fit, method = "ML")
  expect_true(all(is.finite(as.matrix(regression$scores))))
  expect_identical(regression$extreme_patterns, 0L)
  expect_identical(regression$extreme_scores, 0L)
  expect_gt(ml$extreme_patterns, 0L)
  expect_equal(ml$extreme_scores, sum(!is.finite(as.matrix(ml$scores))))
  expect_identical(sieve_analysis_status("factor_scores", regression)$code, "ok")
})

test_that("navigation by page and score shortcuts select the visible pages in the browser", {
  skip_if_no_browser()
  siever_with_app(function(app) {
    app$run_js("Shiny.setInputValue('nav', 'cfa', {priority: 'event'});")
    expect_true(siever_wait_for(app, "document.querySelector('nav a[data-value=\"structure\"]').classList.contains('active') && document.querySelector('#ws_structure a[data-value=\"cfa\"]').classList.contains('active')"))
    expect_identical(app$get_value(input = "nav"), "structure")
    expect_identical(app$get_value(input = "ws_structure"), "cfa")
    expect_true(siever_wait_for(app, "Array.from(document.querySelectorAll('.tab-pane[data-value=cfa] details[data-sieve-settings] summary')).some(x=>x.textContent.includes('caption'))"))
    app$run_js("Shiny.setInputValue('nav', 'scoring', {priority: 'event'});")
    expect_true(siever_wait_for(app, "document.querySelector('nav a[data-value=\"scores\"]').classList.contains('active') && document.querySelector('#reliab-panels a[data-value=\"scoring\"]').classList.contains('active')"))
    expect_identical(app$get_value(input = "nav"), "scores")
    expect_true(siever_wait_for(app, "document.querySelector('#reliab-gate').textContent.trim()===''"))
    expect_true(siever_wait_for(app, "document.querySelector('#reliab-scope').getClientRects().length===0"))
    expect_identical(siever_open_tab(app, "ingest"), "ingest")
    app$set_inputs(study_goal = "evaluate")
    app$click("study_route_go")
    expect_true(siever_wait_for(app, "document.querySelector('nav a[data-value=\"structure\"]').classList.contains('active') && document.querySelector('#ws_structure a[data-value=\"cfa\"]').classList.contains('active')"))
    expect_identical(compat_output_errors(app), character())
  }, load_example = TRUE)
})


test_that("CFA displays its held missing-information counts and working-model qualification", {
  st <- missing_study_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", estimator = "study", fit_go = 1L)
    run <- cfa_run()
    expect_true(inherits(run$fit, "lavaan"))
    text <- paste(as.character(output$method_line$html), collapse = " ")
    expect_match(text, "Respondents used: 301; answered every included item: 276 of 301", fixed = TRUE)
    expect_match(text, "normal-theory working-model information", fixed = TRUE)
  })
})

test_that("two-stage auxiliaries with fixed covariates are refused before fitting", {
  d <- lavaan::HolzingerSwineford1939[, c("x4", "x5", "x6", "ageyr")]
  d$aux <- lavaan::HolzingerSwineford1939$grade
  d$x4[1:10] <- NA_real_
  model <- "F =~ x4 + x5 + x6\nF ~ ageyr"
  meta <- sieve_build_meta(d, paste0("x", 4:6), list(F = paste0("x", 4:6)))
  plan <- list(model = model, observed = c(paste0("x", 4:6), "ageyr"), latent = "F")
  spec <- sieve_model_spec(plan, meta, kind = "sem", fun = "sem")
  spec <- sieve_spec_engine_args(sieve_two_stage_spec(spec, TRUE), list(aux = "aux", fixed.x = TRUE))
  result <- sieve_run_model(spec, d)
  expect_null(result$fit)
  expect_match(result$status$message, "fixed observed covariates", fixed = TRUE)
  expect_match(result$status$message, "random-covariate model", fixed = TRUE)
})

test_that("FMI intervals keep request currency without refitting on settings edits", {
  test_frame <- environment()
  st <- missing_study_state()
  data <- shiny::isolate(st$items)
  fit <- sieve_fit_cfa(data, shiny::isolate(st$meta))
  diagnostics <- attr(fit, "sieve_missing_data", exact = TRUE)
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_mocked_bindings(fraction_missing_information = function(...) {
    calls$n <- calls$n + 1L
    tab <- diagnostics$fmi
    tab$fmi_ci_lower <- 0
    tab$fmi_ci_upper <- 1
    tab
  }, .package = "DMAR", .env = test_frame)
  module <- function(id, state) shiny::moduleServer(id, function(input, output, session) {
    sieve_missing_info_server(input, output, session, state,
      parent = shiny::reactive(list(fit = fit, spec = attr(fit, "sieve_spec"))),
      current = shiny::reactive(TRUE))
  })
  shiny::testServer(module, args = list(state = st), {
    session$setInputs(missing_B = 400L, missing_level = .95, missing_seed = 1L, missing_ci = 1L)
    expect_identical(calls$n, 1L)
    for (values in list(list(missing_level = .90), list(missing_B = 600L), list(missing_seed = 2L))) {
      do.call(session$setInputs, values)
      text <- paste(as.character(output$missing_info_note$html), collapse = " ")
      expect_match(text, "FMI interval settings changed", fixed = TRUE)
      expect_identical(calls$n, 1L)
    }
    session$setInputs(missing_ci = 2L)
    expect_identical(calls$n, 2L)
    expect_false(grepl("settings changed", paste(as.character(output$missing_info_note$html), collapse = " ")))
  })
})

test_that("a clustered parent cannot enter an independent-row FMI bootstrap", {
  test_frame <- environment()
  st <- missing_study_state()
  data <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  data$x4[1:25] <- NA_real_
  data$group <- rep(seq_len(101), length.out = nrow(data))
  fit <- suppressWarnings(lavaan::cfa("F =~ x4 + x5 + x6", data, std.lv = TRUE,
    estimator = "MLR", missing = "fiml", cluster = "group"))
  attr(fit, "sieve_missing_data") <- sieve_missing_diagnostics(data, paste0("x", 4:6), fit)
  calls <- new.env(parent = emptyenv()); calls$n <- 0L
  local_mocked_bindings(fraction_missing_information = function(...) {
    calls$n <- calls$n + 1L
    stop("An independent-row bootstrap was incorrectly entered")
  }, .package = "DMAR", .env = test_frame)
  module <- function(id, state) shiny::moduleServer(id, function(input, output, session) {
    sieve_missing_info_server(input, output, session, state,
      parent = shiny::reactive(list(fit = fit)), current = shiny::reactive(TRUE))
  })
  shiny::testServer(module, args = list(state = st), {
    session$setInputs(missing_ci = 1L)
    expect_match(paste(as.character(output$missing_info_note$html), collapse = " "), "whole-cluster FMI bootstrap is not supplied", fixed = TRUE)
    expect_identical(calls$n, 0L)
  })
})

test_that("EM method text distinguishes available-data moments and reference N", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  d$x1[1:20] <- NA_real_
  basis <- sieve_retention_cor(d, missing = "fiml")
  expect_match(sieve_basis_n_text(basis), "301 available respondents; complete-case reference N = 281", fixed = TRUE)
})


test_that("item inclusion recomputes the requested invariance ladder's complete cases", {
  state <- missing_study_state()
  state$estimator_choice <- "ml"
  shiny::isolate(sieve_confirm_types(state))
  shiny::testServer(mod_finalize_server, args = list(state = state), {
    session$setInputs(group = "school", estimator = "study", inv_missing = "complete", run_inv = 1L)
    first <- inv_shown()
    expect_null(first$error)
    expect_identical(first$n, 276L)
    item <- state$meta$item[1L]
    sieve_set_item_status(state, item, "parked", reason = "review")
    session$flushReact()
    expect_null(inv_shown()$error)
    expect_identical(inv_shown()$n, 301L)
    sieve_set_item_status(state, item, "active")
    session$flushReact()
    expect_identical(inv_shown()$n, 276L)
  })
})


test_that("a declared cluster identifier never enters the FIML auxiliary model", {
  fixture <- sieve_hs9_fixture()
  fixture$data$cluster_id <- rep(seq_len(101), length.out = nrow(fixture$data))
  state <- siever_test_study(fixture, configure = function(s) {
    s$roles$role[s$roles$variable == "cluster_id"] <- "auxiliary"
    s$sampling_design <- list(unit = "clustered", cluster = "cluster_id")
  })
  shiny::testServer(mod_cfa_server, args = list(state = state), {
    session$setInputs(fit_go = 1L)
    run <- cfa_run()
    expect_s4_class(run$fit, "lavaan")
    expect_identical(run$spec$engine_args$cluster, "cluster_id")
    expect_false("cluster_id" %in% run$spec$engine_args$aux)
    expect_false("cluster_id" %in% lavaan::lavNames(run$fit, "ov"))
    expect_match(paste(run$warnings, collapse = " "), "omitted from the FIML auxiliary set", fixed = TRUE)
  })
})


test_that("Reliability names the measurement model supplying its missing-information summary", {
  st <- missing_study_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(scope = "construct", treatment = "study", missing = "fiml",
      denominator = "observed", ci_method = "recommended", conf_level = .95,
      B = 40L, run_go = 1L)
    res <- results()
    expect_equal(res[[1L]]$H$missing_diagnostics$counts$rows, 301L)
    expect_equal(res[[1L]]$H$missing_diagnostics$counts$complete, 276L)
    text <- paste(as.character(output$method_line$html), collapse = " ")
    expect_match(text, "Respondents used: 301; answered every included item: 276 of 301", fixed = TRUE)
    expect_match(text, "one-factor measurement model of", fixed = TRUE)
    expect_match(text, "not FMI or interval inflation for omega", fixed = TRUE)
    expect_match(text, "counts or treatment vary across constructs", fixed = TRUE)
  })
})

test_that("shared Update feedback includes the article only once", {
  expect_identical(sieve_update_lower(c("The reliability table", "the factor scores")),
    c("reliability table", "factor scores"))
})

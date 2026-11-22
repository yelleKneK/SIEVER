test_that("an item-change preview recovers cases without applying or refitting", {
  test_frame <- environment()
  state <- missing_study_state()
  before <- shiny::isolate(state$meta)
  local_mocked_bindings(sieve_run_model = function(...) stop("Preview must not fit"), .env = test_frame)
  item <- before$item[1]
  shiny::testServer(mod_drop_server, args = list(state = state), {
    session$setInputs(drop_pick = item)
    preview <- sieve_item_change_preview(state, item, "parked")
    expect_equal(preview$complete_before, 276)
    expect_equal(preview$complete_after, 301)
    expect_equal(preview$recovered, 25)
    expect_identical(state$meta, before)
    expect_match(output$drop_preview$html, "25 recovered", fixed = TRUE)
    state$items[26:35, 1] <- NA_real_
    session$flushReact()
    expect_match(output$drop_preview$html, "35 recovered", fixed = TRUE)
    expect_identical(state$meta, before)
    session$setInputs(drop_go = 1L)
    expect_identical(state$meta$status[1], "parked")
    session$setInputs(restore_pick = item)
    expect_match(output$restore_preview$html, "35 lost", fixed = TRUE)
    session$setInputs(restore_go = 1L)
    expect_identical(state$meta$status[1], "active")
    expect_length(sieve_item_change_preview(state, character(), "parked")$items, 0L)
  })
})

test_that("score eligibility uses the operational rule and exposes partial content", {
  state <- siever_test_study()
  meta <- shiny::isolate(state$meta)
  its <- meta$item[meta$construct == unique(meta$construct)[1]]
  meta <- meta[meta$item %in% its, ]
  x <- shiny::isolate(state$items)[, its]
  x[1, its[-1]] <- NA_real_
  x[2, its[1]] <- NA_real_
  x[3, ] <- NA_real_
  cn <- meta$construct[1]
  policies <- stats::setNames(list(list(method = "mean", min_count = 1L, min_prop = 0)), cn)
  read <- sieve_scoring_eligibility(x, meta, policies)
  expect_equal(read$rules$Eligible, 300L)
  expect_equal(read$rules$One_Item, 1L)
  expect_equal(read$rules$Partial, 2L)
  expect_equal(read$rules$Complete, 298L)
  expect_equal(read$sensitivity$Eligible, c(300L, 299L, 298L))
  policies[[cn]]$min_prop <- 1
  complete <- sieve_scoring_eligibility(x, meta, policies)
  expect_equal(complete$rules$Eligible, 298L)
  expect_equal(complete$rules$Required, 3L)
  expect_equal(complete$rules$One_Item, 0L)
  expect_equal(nrow(sieve_scoring_eligibility(x, meta[FALSE, ])$rules), 0L)
})

test_that("saved comparisons retain exact numerical results and verify respondent identity", {
  state <- missing_study_state()
  shiny::testServer(mod_cfa_server, args = list(state = state), {
    session$setInputs(structure = "first", ident = "varone", estimator = "ml", fit_go = 1L)
    first <- names(state$analyses)[1]
    original <- state$analyses[[first]]
    expect_true(original$fitted_respondents$verified)
    expect_equal(length(original$fitted_respondents$original_rows), 276L)
    expect_match(sieve_record_uncertainty_text(original), "model-based", fixed = TRUE)
    expect_false(grepl("Uncertainty: Uncertainty", paste(sieve_record_lines(original, state), collapse = " "), fixed = TRUE))
    sieve_set_item_status(state, state$meta$item[1], "parked")
    session$flushReact()
    second <- tail(names(state$analyses), 1L)
    comparison <- sieve_analysis_comparison(state, c(first, second, "absent"))
    expect_identical(comparison$results[[first]], sieve_readable_result_tables(original$estimates))
    expect_match(comparison$analyses$Inputs[1], "earlier", fixed = TRUE)
    expect_match(comparison$differences[["Fitted Respondents"]], "different verified", fixed = TRUE)
    expect_match(paste(comparison$cautions, collapse = " "), "Equal respondent counts", fixed = TRUE)
    html <- as.character(sieve_analysis_comparison_ui(comparison))
    expect_match(html, "ci.lower", fixed = TRUE)
    expect_match(html, "Raw-parameter confidence limits", fixed = TRUE)
    expect_match(html, "CI Upper", fixed = TRUE)
    expect_match(html, "Complete Stored Table", fixed = TRUE)
    for (class in c("sieve-saved-results", "sieve-saved-record", "sieve-saved-compact")) {
      expect_match(html, paste0('class="', class, '"'), fixed = TRUE)
    }
    expect_match(html, first, fixed = TRUE)
    # Equal counts with different verified respondents must remain different.
    records <- state$analyses
    records[[second]]$sample_size$fitted <- records[[first]]$sample_size$fitted
    state$analyses <- records
    expect_match(sieve_analysis_comparison(state, c(first, second))$differences[["Fitted Respondents"]],
      "different verified", fixed = TRUE)
    records[[first]]$fitted_respondents <- NULL
    state$analyses <- records
    expect_match(sieve_analysis_comparison(state, c(first, second))$differences[["Fitted Respondents"]],
      "not recorded", fixed = TRUE)
  })
})

test_that("actual SE options survive mixed estimator and SE requests", {
  d <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa("f =~ x4+x5+x6", d, estimator = "MLR", se = "standard", std.lv = TRUE)
  text <- sieve_inference_text(sieve_fit_inference(fit))
  expect_match(text, "model-based", fixed = TRUE)
  expect_match(text, "yuan.bentler", fixed = TRUE)
  expect_false(grepl("sandwich", text, fixed = TRUE))
  inputs <- list(items = d, raw = d, case_index = seq_len(nrow(d)))
  inputs$items$x4[1] <- inputs$items$x4[1] + 1
  expect_false(sieve_fitted_respondents(fit, inputs)$verified)
})

test_that("the running CFA note does not infer exact fit from zero df alone", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  fixture <- list(data = d, item_cols = names(d), theory_map = list(F = names(d)),
    theory_k = 1L, theory_lbl = "One Factor", name = "Three Items", labels = NULL)
  state <- siever_test_study(fixture, confirmed = TRUE, labels = FALSE, theory = TRUE)
  shiny::testServer(mod_cfa_server, args = list(state = state), {
    session$setInputs(structure = "first", ident = "varone", estimator = "ml", fit_go = 1L)
    expect_equal(cfa_run()$status$df, 0)
    expect_match(output$gate$html, "no ordinary chi square test of fit", fixed = TRUE)
    expect_no_match(output$gate$html, "reproduces the data exactly", fixed = TRUE)
    expect_match(output$method_line$html, "Saturated model (df = 0)", fixed = TRUE)
  })
})


test_that("fitted identities omit empty FIML rows within each group", {
  d <- lavaan::HolzingerSwineford1939
  d[c(1, 170), paste0("x", 1:3)] <- NA_real_
  run <- sieve_capture(lavaan::cfa("f =~ x1+x2+x3", d,
    group = "school", missing = "ml", std.lv = TRUE))
  expect_true(run$ok)
  inputs <- list(items = d, raw = d, case_index = 1000L + seq_len(nrow(d)))
  ids <- sieve_fitted_respondents(run$value, inputs)
  expect_true(ids$verified)
  expect_false(any(c(1001L, 1170L) %in% ids$original_rows))
  expect_equal(lengths(ids$groups), as.integer(lavaan::lavInspect(run$value, "nobs")), ignore_attr = TRUE)
})

test_that("saved comparison output follows renamed labels and scientific inputs without a new result", {
  test_frame <- environment()
  state <- siever_test_study()
  run <- sieve_run_cfa(sieve_cfa_spec(shiny::isolate(state$meta)), shiny::isolate(state$items))
  first <- shiny::isolate(sieve_register_analysis(state, run, "Original"))
  records <- shiny::isolate(state$analyses)
  second <- "A2"
  records[[second]] <- records[[first]]
  records[[second]]$id <- second
  state$analyses <- records
  local_mocked_bindings(sieve_run_model = function(...) stop("Comparison must not fit"), .env = test_frame)
  shiny::testServer(mod_repro_server, args = list(state = state), {
    session$setInputs(saved_ids = c(first, second))
    expect_match(output$saved_comparison$html, "Original", fixed = TRUE)
    state$analysis_labels <- stats::setNames(list("Renamed Saved Model"), first)
    session$flushReact()
    expect_match(output$saved_comparison$html, "Renamed Saved Model", fixed = TRUE)
    expect_no_match(output$saved_comparison$html, "earlier than", fixed = TRUE)
    state$scoring_policies <- list(Visual = list(method = "mean", min_count = 2L))
    session$flushReact()
    expect_match(output$saved_comparison$html, "earlier than", fixed = TRUE)
    expect_identical(state$analyses, records)
  })
})


test_that("saved score uncertainty is described by its score interval rather than parameter SEs", {
  state <- siever_test_study()
  model <- sieve_run_cfa(sieve_cfa_spec(shiny::isolate(state$meta)), shiny::isolate(state$items))
  ids <- character()
  for (method in c("Bartlett", "regression")) {
    score <- sieve_factor_score_record(model$fit, method, data = shiny::isolate(state$items), conf_level = .90)
    ids <- c(ids, shiny::isolate(sieve_register_inference(state, "factor_score_uncertainty", model,
      list(method = score$method, conf_level = .90),
      score[c("method", "conf_level", "interval_method", "uncertainty")], character())))
  }
  text <- sieve_analysis_comparison(state, ids)$analyses$Uncertainty
  expect_match(text[1], "Conditional Wald interval", fixed = TRUE)
  expect_match(text[2], "posterior interval", fixed = TRUE)
  expect_true(all(grepl("90%", text, fixed = TRUE)))
  expect_true(all(grepl("parameters treated as known", text, fixed = TRUE)))
  expect_false(any(grepl("sandwich", text, fixed = TRUE)))
  report <- sieve_readable_saved_report(state, ids)
  expect_match(report, "posterior interval", fixed = TRUE)
  expect_match(report, "Conditional Wald interval", fixed = TRUE)
})


test_that("prefit respondent maps retain original identities after a missing cluster label", {
  state <- siever_test_study(configure = function(s) {
    s$raw$site <- rep(seq_len(60), length.out = nrow(s$raw))
    s$raw$site[1] <- NA_integer_
    roles <- sieve_declared_roles(s$raw, names(s$items))
    roles$role[roles$variable == "site"] <- "demographic"
    s$roles <- roles
    s$sampling_design <- list(unit = "clustered", cluster = "site")
  })
  shiny::testServer(mod_cfa_server, args = list(state = state), {
    session$setInputs(structure = "first", ident = "varone", estimator = "study", fit_go = 1L)
    run <- cfa_run()
    expect_true(sieve_has_solution(run))
    record <- state$analyses[[run$analysis_id]]
    expect_true(record$fitted_respondents$verified)
    expect_equal(record$fitted_respondents$original_rows, 2:301)
    expect_equal(record$sample_size$fitted, 300)
  })
})


test_that("renamed observed SEM outcomes retain verified respondent identities", {
  state <- siever_test_study()
  raw <- shiny::isolate(state$raw)
  raw[["External Outcome"]] <- raw$age_years
  state$raw <- raw
  run <- sieve_run_structural(shiny::isolate(state$items), raw, shiny::isolate(state$meta),
    outcomes = "External Outcome", predictors = "Visual")
  expect_true(sieve_has_solution(run))
  expect_false("External Outcome" %in% names(run$spec$out_map))
  id <- shiny::isolate(sieve_register_analysis(state, run))
  record <- shiny::isolate(state$analyses)[[id]]
  expect_true(record$fitted_respondents$verified)
  expect_identical(record$fitted_respondents$original_rows, seq_len(301))
})

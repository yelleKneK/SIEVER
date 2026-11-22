test_that("Reliability follows the study without replacing saved explicit choices", {
  st <- missing_study_state()
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(missing = "study", treatment = "study", ci_method = "mlr",
      denominator = "model_implied")
    expect_identical(missing_treatment(), "fiml")
    expect_identical(settings()$missing, "fiml")
    state$estimator_choice <- "ml"
    session$flushReact()
    expect_identical(settings()$missing, "listwise")
    session$setInputs(missing = "fiml")
    expect_identical(settings()$missing, "fiml")
    state$estimator_choice <- "auto"
    session$setInputs(missing = "listwise")
    expect_identical(settings()$missing, "listwise")
  })
})

test_that("inclusion and exclusion refit CFA and recompute complete cases", {
  st <- missing_study_state()
  st$estimator_choice <- "ml"
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", estimator = "study")
    session$setInputs(fit_go = 1L)
    first <- cfa_run()
    expect_equal(lavaan::lavInspect(first$fit, "nobs"), 276L)
    item <- state$meta$item[1]
    sieve_set_item_status(state, item, "parked", reason = "review")
    session$flushReact()
    second <- cfa_run()
    expect_equal(lavaan::lavInspect(second$fit, "nobs"), 301L)
    expect_false(item %in% lavaan::lavNames(second$fit, "ov"))
    sieve_set_item_status(state, item, "active")
    session$flushReact()
    expect_equal(lavaan::lavInspect(cfa_run()$fit, "nobs"), 276L)
    state$thresholds <- sieve_default_thresholds()
    session$flushReact()
    expect_identical(cfa_run()$fit, first$fit)
  })
})

test_that("small samples keep FIML, with no note pointing to another estimator", {
  st <- missing_study_state()
  st$items <- shiny::isolate(st$items)[1:80, ]
  st$raw <- shiny::isolate(st$raw)[1:80, ]
  st$case_index <- seq_len(80)
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", estimator = "study")
    session$setInputs(fit_go = 1L)
    run <- cfa_run()
    expect_identical(run$spec$missing, "fiml")
    expect_equal(lavaan::lavInspect(run$fit, "nobs"), 80L)
    expect_false(grepl("sample is small|alternative in the Estimator", run$est$reason))
  })
})

test_that("reliability converts SPSS user-missing codes and ranges before fitting", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(c(1, 2, 99, 96, NA), na_values = 99, na_range = c(95, 98))
  expect_equal(sieve_reliability_data(data.frame(x = x), "x")$x, c(1, 2, NA, NA, NA))
})

test_that("declared auxiliaries enter FIML fits and their data enter fit identity", {
  st <- missing_study_state()
  raw <- shiny::isolate(st$raw)
  raw$aux_age <- lavaan::HolzingerSwineford1939$ageyr
  st$raw <- raw
  st$roles <- sieve_declared_roles(raw, names(shiny::isolate(st$items)))
  shiny::isolate(st$roles$role[st$roles$variable == "age_years"] <- "criterion")
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", estimator = "study")
    session$setInputs(fit_go = 1L)
    first <- cfa_run()
    expect_identical(first$spec$engine_args$aux, "aux_age")
    expect_identical(lavaan::lavInspect(first$fit, "options")$aux, "aux_age")
    expect_true("aux_age" %in% lavaan::lavNames(first$fit, "ov"))
    code <- paste(sieve_cfa_code(first$spec), collapse = "\n")
    expect_match(code, 'aux = "aux_age"', fixed = TRUE)
    prepared <- sieve_sampling_apply(first$spec, state, state$items)
    id <- sieve_fit_identity(prepared$spec, prepared$data)
    prepared$data$aux_age[1] <- prepared$data$aux_age[1] + 1
    expect_false(identical(id, sieve_fit_identity(prepared$spec, prepared$data)))
    session$setInputs(estimator = "ml")
    session$setInputs(fit_go = 2L)
    expect_null(cfa_run()$spec$engine_args$aux)
  })
})

test_that("DMAR supplies both scores, intervals and determinacy for incomplete cases", {
  skip_if_not("factor_scores" %in% getNamespaceExports("DMAR"))
  st <- missing_study_state()
  d <- shiny::isolate(st$items)
  fit <- sieve_fit_cfa(d, shiny::isolate(st$meta))
  expected <- DMAR::factor_scores(fit, data = d, conf_level = .9)
  record <- sieve_factor_score_record(fit, data = d, conf_level = .9)
  expect_equal(record$both_scores, expected)
  expect_true(all(c(".derived_bartlett_ci_lower", ".derived_regression_ci_upper", "determinacy_regression") %in% names(record$both_scores)))
  expect_true(all(is.finite(record$both_scores$.derived_regression[record$both_scores$row <= 25])))
  expect_equal(as.matrix(sieve_factor_scores(fit, "regression")),
    as.matrix(lavaan::lavPredict(fit, method = "regression")), tolerance = 1e-8,
    ignore_attr = TRUE)
})

test_that("constant reliability items and composites name the failed variables", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  sp <- sieve_reliability_spec(names(d), rep("continuous", 3), missing = "fiml", ci_method = "mlr")
  d$x5 <- 1
  r <- sieve_reliability_construct(d, sp, alpha = FALSE)$omega
  expect_match(r$status$message, "x5", fixed = TRUE)
  d <- data.frame(a = 1:50, b = -(1:50), c = rep(0, 50))
  expect_error(DMAR::reliability_alpha(data = d), "c")
})

test_that("reliability and factor scores use the declared FIML auxiliaries", {
  st <- missing_study_state()
  raw <- shiny::isolate(st$raw)
  raw$aux_age <- lavaan::HolzingerSwineford1939$ageyr
  st$raw <- raw
  st$roles <- sieve_declared_roles(raw, names(shiny::isolate(st$items)))
  shiny::isolate(st$roles$role[st$roles$variable == "age_years"] <- "criterion")
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(scope = "construct", ci_method = "mlr", denominator = "model_implied", missing = "fiml")
    session$setInputs(run_go = 1)
    res <- immediate()
    expect_true(all(vapply(res, function(r) identical(r$omega$spec$aux, "aux_age"), logical(1))))
    sp <- res[[1]]$omega$spec
    expected <- DMAR::reliability_omega(data = reliability_data()[, c(sp$items, sp$aux)],
      missing = "fiml", aux = "aux_age", denominator = "model_implied", ci_method = "mlr")
    expect_equal(res[[1]]$omega$estimate, sieve_reliability_value(expected, "estimate"), tolerance = 1e-9)
    expect_equal(res[[1]]$omega$lower, sieve_reliability_value(expected, "ci_lower"), tolerance = 1e-9)
    expect_match(paste(res[[1]]$omega$code, collapse = "\n"), 'aux = "aux_age"', fixed = TRUE)
    session$setInputs(scores_go = 1)
    fit <- score_fit()
    expect_identical(lavaan::lavInspect(fit, "options")$aux, "aux_age")
    expect_true("aux_age" %in% names(demand_scores$result()$score_data))
    expect_identical(demand$status()$state, "current")
    d <- state$raw
    d$aux_age[1] <- d$aux_age[1] + 1
    state$raw <- d
    session$flushReact()
    expect_identical(demand$status()$state, "stale")
  })
})

test_that("finite improper reliability replications are retained and flagged", {
  test_frame <- environment()
  local_mocked_bindings(sieve_reliability_boot_estimate = function(...) {
    warning("An improper solution has negative error variance.")
    .98
  }, .env = test_frame)
  task <- list(pool = data.frame(x = 1:8), seeds = 1L)
  r <- sieve_reliability_boot_replicate(task, 1L)
  expect_true(r$converged)
  expect_true(r$used)
  expect_false(r$admissible)
  expect_identical(r$outcome, "inadmissible")
  expect_identical(r$estimate, .98)
})

test_that("a nonvarying reliability composite is named without a Shiny error", {
  d <- withr::with_seed(1L, {
    a <- stats::rnorm(100)
    b <- stats::rnorm(100)
    data.frame(item_first = a, item_second = b, item_third = -a - b)
  })
  spec <- sieve_reliability_spec(names(d), rep("continuous", 3L),
    missing = "fiml", ci_method = "mlr")
  result <- sieve_reliability_construct(d, spec, alpha = FALSE)$omega
  expect_identical(result$status$code, "failed")
  expect_match(result$status$message, "composite", ignore.case = TRUE)
  for (name in names(d)) expect_match(result$status$message, name, fixed = TRUE)
})

test_that("an entirely constant reliability pool names each item before fitting", {
  withr::local_options(sieve.auto_run = "never")
  state <- siever_test_study(configure = function(s) s$items[,] <- 1)
  shiny::testServer(mod_reliab_server, args = list(state = state), {
    session$setInputs(scope = "construct", ci_method = "mlr", run_go = 1L)
    results <- immediate()
    expect_length(results, 3L)
    for (r in results) {
      expect_identical(r$omega$status$code, "failed")
      expect_true(all(vapply(r$omega$spec$items, function(item) grepl(item, r$omega$status$message, fixed = TRUE), logical(1))))
      expect_match(r$omega$status$message, "Review or exclude them in Ingest", fixed = TRUE)
    }
    expect_no_error(output$omega_tbl)
  })
})

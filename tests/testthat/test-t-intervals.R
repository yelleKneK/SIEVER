test_that("calibrated t intervals retain DMAR's degrees of freedom and exact limits", {
  x <- lavaan::HolzingerSwineford1939[1:80, paste0("x", 4:6)]
  x[1:15, 1] <- NA_real_
  normal <- sieve_reliability_spec(names(x), rep("continuous", 3), "Textual",
    missing = "fiml", ci_method = "ml", denominator = "model_implied")
  t_spec <- sieve_reliability_spec(names(x), rep("continuous", 3), "Textual",
    missing = "fiml", ci_method = "ml", denominator = "model_implied",
    wald_df = "missing_information")
  actual <- sieve_reliability_omega(x, t_spec)
  expected <- DMAR::reliability_omega(data = x, missing = "fiml", ci_method = "ml",
    denominator = "model_implied", wald_df = "missing_information")
  expect_identical(actual$status$code, "ok")
  expect_identical(actual$uncertainty$status, "completed")
  expect_identical(actual$uncertainty$wald_df, attr(expected, "wald_df"))
  expect_equal(c(actual$lower, actual$upper), c(sieve_reliability_value(expected, "ci_lower"),
    sieve_reliability_value(expected, "ci_upper")), tolerance = 1e-12)
  z <- sieve_reliability_omega(x, normal)
  expect_gt(actual$upper - actual$lower, z$upper - z$lower)
  expect_match(actual$uncertainty$label, "t reference", fixed = TRUE)
  expect_match(paste(actual$code, collapse = "\n"), 'wald_df = "missing_information"', fixed = TRUE)
})

test_that("the Reliability t option updates the requested fit and leaves the default normal", {
  withr::local_options(sieve.auto_run = "never")
  st <- siever_test_study()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(scope = "construct", treatment = "continuous", missing = "fiml",
      denominator = "model_implied", ci_method = "ml", conf_level = .95, run_go = 1L)
    expect_null(immediate()[[1]]$omega$uncertainty$wald_df)
    session$setInputs(small_sample_t = TRUE)
    expect_false(identical(demand$status()$state, "current"))
    session$setInputs(update_go = 1L)
    expect_identical(demand$status()$state, "current")
    expect_true(is.finite(immediate()[[1]]$omega$uncertainty$wald_df))
    expect_match(immediate()[[1]]$omega$uncertainty$label, "calibrated", fixed = TRUE)
  })
})

test_that("two-stage inference refuses auxiliary-only rows and points to FIML", {
  st <- siever_test_study()
  x <- shiny::isolate(st$items)
  x[1, ] <- NA_real_
  x$aux <- seq_len(nrow(x))
  spec <- sieve_cfa_spec(shiny::isolate(st$meta), reserved = names(x))
  spec <- sieve_two_stage_spec(spec, TRUE)
  spec <- sieve_spec_engine_args(spec, list(aux = "aux"))
  result <- sieve_run_model(spec, x)
  expect_null(result$fit)
  expect_match(result$error, "observed only on auxiliary", fixed = TRUE)
  expect_match(result$error, "FIML", fixed = TRUE)
})

test_that("Reliability settings that do not name the t option open with the normal-reference choice", {
  withr::local_options(sieve.auto_run = "never")
  state <- siever_test_study()
  shiny::testServer(mod_reliab_server, args = list(state = state), {
    session$setInputs(scope = "construct", treatment = "continuous", missing = "fiml",
      denominator = "model_implied", ci_method = "ml", small_sample_t = TRUE)
    legacy <- state$reliability_settings
    legacy$small_sample_t <- NULL
    state$reliability_settings <- legacy
    session$flushReact()
    expect_false(rel_binding$pending()$small_sample_t)
    session$setInputs(small_sample_t = FALSE)
    expect_null(rel_binding$pending())
    expect_identical(settings()$wald_df, Inf)
  })
})

test_that("model replay normalizes SPSS missing codes without changing cluster labels", {
  st <- siever_test_study()
  x <- shiny::isolate(st$items)
  value <- x[[1]]
  value[1:10] <- 99
  attr(value, "spss_missing") <- list(values = 99, range = NULL)
  x[[1]] <- value
  spec <- sieve_cfa_spec(shiny::isolate(st$meta))
  actual <- sieve_run_model(spec, x)
  expect_true(inherits(actual$fit, "lavaan"))
  env <- new.env(parent = globalenv())
  env$items <- x
  expect_no_error(capture.output(eval(parse(text = sieve_cfa_code(spec)), env)))
  expect_equal(lavaan::coef(env$cfa_fit), lavaan::coef(actual$fit), tolerance = 1e-10)
  expect_true(all(is.na(env$cfa_model_data[1:10, 1])))
})

test_that("congeneric planning preserves DMAR's target-specific FMI mapping", {
  skip_if_not("fmi" %in% names(formals(DMAR::ss_aipe_reliability)))
  lambda <- c(.8, .7, .6, .75); psi <- c(.36, .51, .64, .4375)
  base <- sieve_plan_congeneric(lambda, psi, seed = 5L)
  missing <- sieve_plan_congeneric(lambda, psi, fmi = .3, seed = 5L)
  engine <- DMAR::ss_aipe_reliability(model = "Congeneric", type = "Factor Analytic",
    width = .1, n_items = 4L, true_factor_loading = lambda, true_residual_var = psi, conf_level = .95,
    assurance = NULL, fmi = .3)
  expect_null(missing$error)
  expect_identical(missing$result, engine)
  expect_identical(attr(missing$result, "fmi"), .3)
  expect_equal(attr(missing$result, "necessary_N_complete"),
    base$result$value[base$result$term == "necessary_N"])
  expect_true(all(missing$sensitivity$fmi == .3))
  expect_gt(missing$result$value[missing$result$term == "necessary_N"],
    base$result$value[base$result$term == "necessary_N"])
  expect_error(sieve_plan_congeneric(lambda, psi, fmi = 1), "Planning FMI")
})

test_that("Reliability's congeneric plan uses the shared FMI control and replays it", {
  withr::local_options(sieve.auto_run = "never")
  st <- siever_test_study()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(plan_lambda = ".8, .7, .6, .75", plan_psi = ".36, .51, .64, .4375",
      plan_width = .1, conf_level = .95, plan_assurance = "", plan_seed = 5L,
      plan_fmi = .3, plan_congeneric_go = 1L)
    r <- advanced_reliability$congeneric()
    expect_null(r$error)
    expect_identical(r$call$fmi, .3)
    expect_true(all(r$sensitivity$fmi == .3))
    code <- tail(state$codelog, 2L)
    replay <- sieve_capture(eval(parse(text = code), new.env(parent = globalenv())))
    expect_true(replay$ok, info = replay$error)
    expect_identical(replay$value$result, r$result)
    log <- state$codelog
    session$setInputs(plan_fmi = .4)
    expect_match(advanced_reliability$congeneric()$error, "planning inputs changed", fixed = TRUE)
    expect_identical(state$codelog, log)
  })
})

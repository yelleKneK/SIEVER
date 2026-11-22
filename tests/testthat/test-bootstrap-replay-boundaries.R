test_that("an auxiliary-adjusted bifactor bootstrap replays in a clean environment", {
  state <- siever_test_study()
  x <- shiny::isolate(state$items)
  meta <- shiny::isolate(state$meta)
  x$age_aux <- shiny::isolate(state$raw$age_years)
  plan <- sieve_bifactor_boot_plan(x, meta, B = 2L, seed = 2L, engine = "siever",
    fit_fun = function(data, meta) sieve_fit_bifactor(data, meta, aux = "age_aux"))
  expect_identical(plan$status$code, "ready")
  draws <- lapply(1:2, function(b) sieve_bifactor_boot_draw(plan, b))
  actual <- sieve_bifactor_boot_summary(plan, draws)
  env <- new.env(parent = baseenv())
  env$replay_data <- x
  expect_no_error(eval(parse(text = sieve_bifactor_boot_code(plan, data_name = "replay_data")), env))
  expect_equal(unname(env$bifactor_boot_counts[c("attempted", "used", "improper_kept", "caution")]),
    c(actual$counts$attempted, actual$counts$used, actual$counts$improper_kept, actual$caution))
  expect_equal(unname(env$bifactor_boot_used),
    unname(actual$estimates[actual$draws$used, , drop = FALSE]), tolerance = 1e-9)
  empty <- new.env(parent = baseenv())
  empty$replay_data <- x
  empty$d <- x
  expect_no_error(eval(parse(text = sieve_bifactor_boot_code(plan, attempted = 0L, data_name = "replay_data")), empty))
  expect_false(empty$bifactor_boot_caution)
  expect_identical(names(empty$bifactor_boot_pool), plan$point$signature$observed)
  expect_identical(NROW(empty$bifactor_boot_used), 0L)
})

test_that("a stopped zero-replication omega replay has no caution", {
  state <- siever_test_study()
  x <- shiny::isolate(state$items[, 4:6])
  spec <- sieve_reliability_spec(names(x), rep("continuous", 3), "Textual",
    missing = "listwise", ci_method = "percentile", B = 2L, seed = 2L)
  env <- new.env(parent = baseenv())
  env$items <- x
  expect_no_error(eval(parse(text = sieve_reliability_boot_code(spec,
    rng = sieve_draw_seed(2L), attempted = 0L)), env))
  expect_false(env$omega_boot_caution)
  expect_length(env$omega_boot_used, 0L)
})

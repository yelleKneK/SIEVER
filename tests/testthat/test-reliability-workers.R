# The headless reliability bootstrap honors sieve.cores, through a workers
# argument, and reports through a progress argument; the same seed gives the
# same draws whatever the number of workers.

skip_if_not_installed("lavaan")

rw_spec <- function() {
  sieve_reliability_spec(c("x4", "x5", "x6"), rep("continuous", 3), construct = "Textual",
    missing = "listwise", ci_method = "percentile", B = 400L, seed = 1)
}

test_that("the headless bootstrap runs on sieve.cores, reports progress, and gives the same limits on any number of workers", {
  d <- lavaan::HolzingerSwineford1939
  expected <- if (sieve_dmar_fork_safe()) 2L else 1L
  calls <- new.env()
  calls$n <- 0L
  calls$total <- integer(0)
  hook <- function(done, total) {
    calls$n <- calls$n + 1L
    calls$total <- c(calls$total, total)
    TRUE
  }
  many <- withr::with_options(list(sieve.cores = 2L),
    sieve_reliability_construct(d, rw_spec(), model_implied = FALSE, progress = hook))
  expect_identical(many$omega$uncertainty$status, "completed")
  expect_identical(as.integer(many$omega$uncertainty$ncpus), expected)
  expect_match(paste(many$omega$code, collapse = " "), sprintf("ncpus = %d", expected), fixed = TRUE)
  expect_identical(many$alpha$uncertainty$status, "completed")
  expect_identical(as.integer(many$alpha$uncertainty$ncpus), expected)
  # The hook heard both bootstraps, omega's and alpha's, each of B = 400.
  expect_gt(calls$n, 1L)
  expect_identical(unique(calls$total), 400L)
  one <- sieve_reliability_construct(d, rw_spec(), model_implied = FALSE, workers = 1L)
  expect_identical(as.integer(one$omega$uncertainty$ncpus), 1L)
  expect_identical(c(one$omega$lower, one$omega$upper), c(many$omega$lower, many$omega$upper))
  expect_identical(one$omega$uncertainty$draws$estimate, many$omega$uncertainty$draws$estimate)
  expect_identical(c(one$alpha$lower, one$alpha$upper), c(many$alpha$lower, many$alpha$upper))
  # A progress function that returns FALSE stops the run.
  stopped <- sieve_reliability_construct(d, rw_spec(), alpha = FALSE, model_implied = FALSE, workers = 1L,
    progress = function(done, total) done < 50L)
  expect_false(identical(stopped$omega$uncertainty$status, "completed"))
})

test_that("workers and progress are checked, and leave the request's identity alone", {
  d <- lavaan::HolzingerSwineford1939
  expect_error(sieve_reliability_construct(d, rw_spec(), workers = 0), "single whole number", fixed = TRUE)
  expect_error(sieve_reliability_construct(d, rw_spec(), workers = 1.5), "single whole number", fixed = TRUE)
  expect_error(sieve_reliability_construct(d, rw_spec(), progress = "yes"), "must be NULL or a function", fixed = TRUE)
  pending <- sieve_reliability_spec(c("x4", "x5", "x6"), rep("continuous", 3), construct = "Textual",
    run_bootstrap = FALSE)
  a <- sieve_analyze("reliability", data = d, spec = pending)
  b <- sieve_analyze("reliability", data = d, spec = pending, workers = 1L, progress = function(done, total) TRUE)
  expect_identical(a$arguments_hash, b$arguments_hash)
  expect_identical(a$result$omega$estimate, b$result$omega$estimate)
})

test_that("under R CMD check's core limit the default is at most two processes", {
  withr::with_options(list(sieve.cores = NULL), withr::with_envvar(c(SIEVER_CORES = NA, `_R_CHECK_LIMIT_CORES_` = "TRUE"), {
    expect_lte(sieve_cores(), 2L)
  }))
  # An explicit setting is honored.
  withr::with_envvar(c(`_R_CHECK_LIMIT_CORES_` = "TRUE"), withr::with_options(list(sieve.cores = 3L), {
    expect_identical(sieve_cores(), 3L)
  }))
})

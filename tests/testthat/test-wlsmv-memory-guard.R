# The WLSMV memory guard models the fit's peak, with the number of
# respondents and lavaan's working copies of the weight matrix, and compares
# it with the memory limit. One copy of the weight matrix, with no term in
# n, against a fixed 2 GB would estimate 92 MB for a fit measured at 2.5 GB.

wmg_items <- function(n, p, categories = 4L) {
  d <- as.data.frame(matrix(rep_len(seq_len(categories), n * p), n, p))
  names(d) <- sprintf("i%03d", seq_len(p))
  d
}

test_that("the peak estimate reproduces the measured peaks of WLSMV fits, where one copy of the weight matrix does not", {
  # Peak resident memory of one R process running sieve_analyze("cfa") on
  # four-category items (two factors, lavaan 0.7-3, R 4.6.2), measured with
  # /usr/bin/time -l; bytes.
  measured <- data.frame(
    p = c(40, 40, 40, 80, 80),
    n = c(1000, 5000, 20000, 2000, 8000),
    peak = c(586, 635, 1168, 2293, 2570) * 1e6
  )
  estimate <- mapply(function(p, n) sieve_wlsmv_peak_bytes(p, 4, n), measured$p, measured$n)
  ratio <- estimate / measured$peak
  # Never more than 5 percent below a measurement (a guard errs high), and
  # never more than 45 percent above one.
  expect_true(all(ratio > 0.95))
  expect_true(all(ratio < 1.45))
  # The statistic counts behind them: 900 at 40 items, 3,400 at 80.
  expect_identical(sieve_wlsmv_statistics(40, 4), 900)
  expect_identical(sieve_wlsmv_statistics(80, 4), 3400)
  # One copy of the weight matrix alone: 6.5 MB and 92 MB, a twenty-fifth
  # of the peak or less.
  one_copy <- mapply(function(p) sieve_wlsmv_weight_bytes(p, 4), measured$p)
  expect_equal(one_copy[c(1L, 4L)], c(8 * 900^2, 8 * 3400^2))
  expect_true(all(one_copy / measured$peak < 1 / 24))
  # 80 items on 10,000 respondents, measured at 2.52 GB.
  expect_gt(sieve_wlsmv_peak_bytes(80, 4, 10000), 2.52e9)
  expect_lt(sieve_wlsmv_weight_bytes(80, 4), 0.1e9)
  # The estimate grows with n; without an n the casewise term is left out.
  expect_gt(sieve_wlsmv_peak_bytes(40, 4, 20000), sieve_wlsmv_peak_bytes(40, 4, 1000))
  expect_equal(sieve_wlsmv_peak_bytes(40, 4), 4e8 + 20 * 8 * 900^2)
  expect_equal(sieve_wlsmv_peak_bytes(40, 4, 1000) - sieve_wlsmv_peak_bytes(40, 4), 6 * 8 * 1000 * 900)
})

test_that("the guard refuses a fit whose peak exceeds the memory limit, with the numbers, and counts the respondents", {
  withr::local_options(sieve.memory_limit = 32e9, sieve.wlsmv_limit = NULL)
  expect_identical(sieve_wlsmv_limit(), 32e9)
  # 175 four-category items: one copy of the weight matrix is 1.98 GB, which
  # a fixed 2 GB limit would admit; with twenty copies the fit needs about
  # 40 GB at any n, more than a 32 GB limit.
  wide <- wmg_items(500L, 175L)
  expect_lt(sieve_wlsmv_weight_bytes(175, 4), 2e9)
  refused <- sieve_wlsmv_guard(names(wide), wide, estimator = "WLSMV")
  expect_false(is.null(refused))
  expect_match(refused, "Not fit: the weight matrix of a WLSMV fit of these 175 items (175 of them ordered or binary)", fixed = TRUE)
  expect_match(refused, "each of its 15,750 sample statistics (525 thresholds and 15,225 correlations), about 2.0 GB", fixed = TRUE)
  expect_match(refused, "lavaan keeps about twenty working copies of it beside several 500 by 15,750 matrices of casewise scores", fixed = TRUE)
  expect_match(refused, sprintf(
    "so on these 500 respondents the fit would need an estimated %s at its peak, more than the 32.0 GB SIEVER allows (option sieve.memory_limit).",
    sieve_bytes_text(sieve_wlsmv_peak_bytes(175, 4, 500))
  ), fixed = TRUE)
  expect_match(refused, "Analyze one construct at a time", fixed = TRUE)
  expect_match(refused, "That is a different model", fixed = TRUE)
  # Through the CFA service: refused before lavaan starts.
  meta <- data.frame(
    item = names(wide), label = names(wide), construct = "One", facet = NA_character_,
    type = "ordered", status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
  run <- sieve_run_cfa(sieve_cfa_spec(meta), wide)
  expect_identical(run$status$code, "failed")
  expect_null(run$fit)
  expect_match(run$error, "at its peak, more than the 32.0 GB SIEVER allows", fixed = TRUE)
  # Forty items on 1,000 respondents pass.
  expect_null(sieve_wlsmv_guard(names(wide)[1:40], wmg_items(1000L, 40L), estimator = "WLSMV"))

  # The number of respondents decides at a fixed item count: under a 1 GB
  # limit, 40 items pass on 1,000 respondents and are refused on 20,000.
  withr::local_options(sieve.memory_limit = 1e9)
  long <- wmg_items(20000L, 40L)
  expect_null(sieve_wlsmv_guard(names(long), long[seq_len(1000L), ], estimator = "WLSMV"))
  on_all <- sieve_wlsmv_guard(names(long), long, estimator = "WLSMV")
  expect_match(on_all, "so on these 20,000 respondents the fit would need an estimated 1.4 GB at its peak", fixed = TRUE)
  expect_match(on_all, "more than the 1.0 GB SIEVER allows (option sieve.memory_limit)", fixed = TRUE)
  # Rows without a response to any item are not respondents.
  padded <- rbind(long[seq_len(1000L), ], long[seq_len(19000L), ] * NA)
  expect_null(sieve_wlsmv_guard(names(padded), padded, estimator = "WLSMV"))
  # A continuous (MLR) fit is never refused by this guard.
  expect_null(sieve_wlsmv_guard(names(long), long, estimator = "MLR"))

  # sieve.wlsmv_limit, when set, bounds the categorical fit alone, and the
  # refusal names it.
  withr::local_options(sieve.memory_limit = 32e9, sieve.wlsmv_limit = 5e8)
  expect_identical(sieve_wlsmv_limit(), 5e8)
  small <- sieve_wlsmv_guard(names(long), long[seq_len(1000L), ], estimator = "WLSMV")
  expect_match(small, "more than the 500.0 MB SIEVER allows (option sieve.wlsmv_limit)", fixed = TRUE)
})

test_that("the exploratory worker cap counts the respondents of a categorical fit", {
  withr::local_options(sieve.efa_memory = 8e9)
  # Without n the estimate is the peak at seven categories, with no
  # casewise term.
  expect_equal(sieve_efa_worker_bytes(40, "WLSMV"), sieve_wlsmv_peak_bytes(40, 7))
  expect_gt(sieve_efa_worker_bytes(40, "WLSMV", n = 20000), sieve_efa_worker_bytes(40, "WLSMV"))
  expect_identical(sieve_efa_worker_cap(40, "WLSMV"), 14L)
  # On 20,000 respondents each worker needs about 1.5 GB: five at once.
  expect_identical(sieve_efa_worker_cap(40, "WLSMV", n = 20000), 5L)
  # Under maximum likelihood the number of respondents changes nothing.
  expect_identical(sieve_efa_worker_cap(68, "MLR", n = 20000), sieve_efa_worker_cap(68, "MLR"))
})

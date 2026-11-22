test_that("headless reliability retains a point estimate when its requested interval is refused", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  spec <- sieve_reliability_spec(names(d), rep("continuous", 3L),
    construct = "Textual", missing = "listwise", ci_method = "percentile", B = 10L)
  result <- sieve_analyze("reliability", data = d, spec = spec,
    alpha = FALSE, model_implied = FALSE, rng = list(seed = 1L))
  expect_true(is.finite(result$result$omega$estimate))
  expect_identical(result$result$omega$uncertainty$status, "unavailable")
  expect_identical(result$status$code, "partial")
  expect_match(result$status$message, "at least 400", fixed = TRUE)
  parts <- result$diagnostics$components
  interval <- grepl("interval$", parts$component)
  expect_true(any(interval))
  expect_identical(parts$code[interval], "unavailable")
})

test_that("requested interval refusals differ from pending and unrequested uncertainty", {
  for (code in c("unavailable", "failed", "pending", "not_requested", "completed")) {
    value <- list(omega = list(estimate = .8, status = list(code = "ok"),
      uncertainty = list(status = code, reason = "Recorded interval reason.")))
    status <- sieve_analysis_status("reliability", value)
    if (code %in% c("unavailable", "failed")) {
      expect_identical(status$code, "partial")
      expect_match(status$message, "Recorded interval reason", fixed = TRUE)
      expect_identical(tail(status$components$code, 1L), code)
    } else {
      expect_identical(status$code, "ok")
    }
  }
})

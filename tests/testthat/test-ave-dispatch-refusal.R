test_that("headless AVE preserves a nonconverged fit without dispatching the engine", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:3)]
  fit <- suppressWarnings(lavaan::cfa("V =~ x1+x2+x3", data = d,
    std.lv = TRUE, control = list(iter.max = 1)))
  expect_identical(sieve_fit_status(fit)$code, "not_converged")
  local_mocked_bindings(average_variance_extracted = function(...) {
    stop("The engine must not receive a fit without a solution.")
  }, .package = "DMAR")
  result <- sieve_analyze("ave", fit = fit)
  expect_s3_class(result, "siever_analysis")
  expect_identical(result$status$code, "not_converged")
  expect_match(result$status$message, "The fit the AVE is read from", fixed = TRUE)
  expect_null(result$result)
  expect_error(as.data.frame(result), "no estimates", fixed = TRUE)
})

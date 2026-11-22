test_that("headless AVE distinguishes point-only results from withheld requested intervals", {
  tab <- data.frame(factor = c("A", "B"), ave = c(.5, .6),
    ci_lower = c(.4, NA_real_), ci_upper = c(.7, NA_real_))
  point <- sieve_analysis_status("ave", tab, list())
  expect_identical(point$code, "ok")
  requested <- sieve_analysis_status("ave", tab, list(ci_method = "percentile"))
  expect_identical(requested$code, "partial")
  expect_match(requested$message, "point estimates are retained", fixed = TRUE)
  attr(tab, "B") <- 400L
  expect_identical(sieve_analysis_status("ave", tab, list())$code, "partial")
  tab$ci_lower <- c(.4, .5)
  tab$ci_upper <- c(.6, .7)
  expect_identical(sieve_analysis_status("ave", tab, list())$code, "ok")
  tab$ave[] <- NA_real_
  expect_identical(sieve_analysis_status("ave", tab, list())$code, "failed")
})

test_that("optional AVE intervals retain the all-improper refusal without inventing counts", {
  items <- paste0("x", 1:3)
  S <- matrix(c(1, .8, .8, .8, 1, .3, .8, .3, 1), 3L,
    dimnames = list(items, items))
  fit <- suppressWarnings(lavaan::cfa("f =~ x1+x2+x3", sample.cov = S,
    sample.nobs = 500L, std.lv = TRUE))
  expect_identical(sieve_fit_status(fit)$code, "inadmissible")
  result <- sieve_ave_intervals(fit, B = 400L, seed = 1)
  expect_identical(result$status$code, "unavailable")
  expect_match(result$status$message, "did not run", fixed = TRUE)
  expect_null(result$table)
  expect_null(result$counts)
  expect_true(any(grepl("Heywood", result$warnings, fixed = TRUE)))
  expect_true(result$engine$ok)
  expect_true(all(is.na(result$engine$value$ave)))
  expect_null(attr(result$engine$value, "B", exact = TRUE))
})

test_that("optional AVE intervals still refuse incomplete metadata from an actual bootstrap", {
  tab <- data.frame(factor = "f", ave = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_)
  attr(tab, "B") <- 400L
  local_mocked_bindings(sieve_has_solution = function(...) TRUE,
    sieve_dmar_boot_run = function(...) list(ok = TRUE, value = tab, warnings = character(0)),
    .package = "SIEVER")
  expect_error(sieve_ave_intervals(list(), B = 400L, seed = 1), "unsupported bootstrap count schema", fixed = TRUE)
})

test_that("AVE interval page and code disclosures report engine refusal and failure", {
  # Not on the CRAN check's clock (about 2 s); the full suite runs it.
  skip_on_cran()
  withr::local_options(sieve.auto_run = "never")
  fixture <- new.env(parent = emptyenv())
  fixture$phase <- "unavailable"
  local_mocked_bindings(sieve_ave_intervals = function(fit, B, seed, conf_level, sampling_unit) {
    partial <- identical(fixture$phase, "partial")
    factors <- lavaan::lavNames(fit, "lv")
    list(table = if (partial) data.frame(factor = factors, ave = .6,
      ci_lower = NA_real_, ci_upper = NA_real_) else NULL,
      counts = if (partial) list(attempted = B, used = B / 2, dropped = B / 2) else NULL,
      status = list(code = fixture$phase, message = paste("Fixture engine status:", fixture$phase)),
      warnings = character(0), fit_signature = sieve_lavaan_signature(fit))
  }, .package = "SIEVER")
  state <- siever_test_study()
  shiny::testServer(mod_validity_server, args = list(state = state), {
    session$setInputs(fit_go = 1L)
    for (i in seq_along(c("unavailable", "failed", "partial"))) {
      phase <- c("unavailable", "failed", "partial")[[i]]
      fixture$phase <- phase
      session$setInputs(ave_B = 400L, ave_seed = i, ave_level = .95, ave_boot = i)
      expected <- if (identical(phase, "partial")) "current" else "failed"
      published <- state$page_results$validity[[session$ns("ave_intervals")]]
      expect_identical(published$state, expected)
      expect_match(published$sentence, phase, fixed = TRUE)
      expect_identical(ave_interval_status()$state, expected)
      if (!identical(phase, "partial")) {
        expect_match(as.character(output$ave_interval_code$html), "latest request failed", fixed = TRUE)
      } else {
        expect_match(as.character(output$ave_interval_note$html), "partial", fixed = TRUE)
        expect_false(grepl("latest request failed", as.character(output$ave_interval_code$html), fixed = TRUE))
      }
    }
  })
})

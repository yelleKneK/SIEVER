test_that("DMAR request counts are validated without changing the request", {
  expect_identical(sieve_dmar_boot_required("percentile", .95), 400L)
  expect_identical(sieve_dmar_boot_required("percentile", .95, "one"), 200L)
  expect_identical(sieve_dmar_boot_required("bca", .95), 2000L)
  expect_identical(sieve_dmar_boot_required("percentile", .99), 2000L)
  expect_identical(sieve_dmar_boot_required("p", .95), 100L)
  for (method in c("percentile", "bca", "se", "p")) {
    for (level in c(.90, .95, .99)) for (side in c("one", "two")) {
      need <- sieve_dmar_boot_required(method, level, side)
      expect_false(sieve_dmar_boot_request(need - 1L, method, level, side)$ok)
      expect_true(sieve_dmar_boot_request(need, method, level, side)$ok)
      expect_identical(sieve_dmar_boot_request(need - 1L, method, level, side)$B, need - 1L)
    }
  }
  expect_false(sieve_dmar_boot_request(400.5)$ok)
})

test_that("reliability analytic adapters retain DMAR's finite limits", {
  X <- lavaan::HolzingerSwineford1939[, c("x4", "x5", "x6")]
  sp <- sieve_reliability_spec(names(X), rep("continuous", 3), "Textual", missing = "listwise",
    ci_method = "mlr_logistic", denominator = "model_implied")
  point <- sieve_reliability_omega(X, sp)
  direct <- DMAR::reliability_omega(X, ci_method = "mlr_logistic", denominator = "model_implied", missing = "listwise")
  expect_true(all(is.finite(c(point$lower, point$upper))))
  expect_identical(point$uncertainty$status, "completed")
  expect_equal(c(point$lower, point$upper), direct$value[match(c("ci_lower", "ci_upper"), direct$term)])
  h <- sieve_reliability_H(X, sp)
  alpha <- sieve_reliability_alpha(X, sp)
  expect_true(all(is.finite(c(h$lower, h$upper, alpha$lower, alpha$upper))))
  expect_identical(h$uncertainty$status, "completed")
  expect_identical(alpha$uncertainty$status, "completed")
  fit <- lavaan::cfa("f =~ x1+x2+x3; g =~ x4+x5+x6", data = lavaan::HolzingerSwineford1939, std.lv = TRUE)
  row <- sieve_cv_fit_row("all", fit)
  ecvi <- DMAR::ecvi(fit)
  expect_equal(c(row$ECVI.lo, row$ECVI.hi), ecvi$value[match(c("ci_lower", "ci_upper"), ecvi$term)])
})

test_that("production percentile reliability uses the requested 400 replications", {
  X <- lavaan::HolzingerSwineford1939[, c("x4", "x5", "x6")]
  sp <- sieve_reliability_spec(names(X), rep("continuous", 3), "Textual", missing = "listwise",
    ci_method = "percentile", B = 400L, seed = 19)
  result <- sieve_reliability_omega(X, sp)
  expect_identical(result$uncertainty$status, "completed")
  expect_identical(result$uncertainty$B_attempted, 400L)
  expect_true(all(is.finite(c(result$lower, result$upper))))
  direct <- DMAR::reliability_omega(X, missing = "listwise", ci_method = "percentile",
    denominator = "observed", B = 400L, seed = 19)
  expect_equal(c(result$lower, result$upper), direct$value[match(c("ci_lower", "ci_upper"), direct$term)])
})

test_that("production bifactor collection succeeds after 400 engine replications", {
  d <- withr::with_seed(113, {
    n <- 500; G <- rnorm(n); S <- matrix(rnorm(n * 3), n)
    as.data.frame(sapply(1:9, function(j) .5 * G + .5 * S[, rep(1:3, each = 3)[j]] + sqrt(.5) * rnorm(n)))
  })
  names(d) <- paste0("y", 1:9)
  meta <- data.frame(item = names(d), label = names(d), construct = rep(c("A", "B", "C"), each = 3),
    facet = "", type = "continuous", status = "active", reverse = FALSE)
  result <- sieve_bifactor_bootstrap(d, meta, B = 400L, seed = 7)
  expect_identical(result$uncertainty$status, "completed")
  expect_identical(result$counts$attempted, 400L)
  live <- result$table$status == "completed"
  expect_true(any(live))
  expect_true(all(is.finite(result$table$lower[live])))
  expect_true(all(is.finite(result$table$upper[live])))
  dm <- result$dmar
  keys <- paste(dm$factor, dm$index, sep = ":")
  at <- match(paste(result$table$factor[live], result$table$index[live], sep = ":"), keys)
  expect_equal(result$table$lower[live], dm$ci_lower[at])
  expect_equal(result$table$upper[live], dm$ci_upper[at])
})

test_that("the request of the documented bifactor bootstrap example completes at the level it states", {
  # The data, the request, and the seed of the example on the help page of
  # sieve_bifactor_bootstrap(): 100 replications for an 80 percent interval,
  # DMAR's minimum at that level. test-rd_hygiene.R holds the page to it.
  items <- withr::with_seed(1, {
    n <- 500L
    general <- stats::rnorm(n)
    specific <- matrix(stats::rnorm(n * 3L), n)
    group <- rep(1:3, each = 3L)
    as.data.frame(sapply(seq_len(9L), function(j) {
      .5 * general + .5 * specific[, group[j]] + sqrt(.5) * stats::rnorm(n)
    }))
  })
  names(items) <- paste0("item", seq_len(9L))
  meta <- data.frame(item = names(items), label = names(items),
    construct = rep(c("A", "B", "C"), each = 3L), facet = NA_character_,
    type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE)
  boot <- sieve_bifactor_bootstrap(items, meta, B = 100L, seed = 1, conf_level = 0.80)
  expect_identical(boot$uncertainty$status, "completed")
  expect_identical(boot$counts$requested, 100L)
  expect_identical(boot$counts$attempted, 100L)
  expect_equal(boot$conf_level, 0.80)
  general <- boot$table[boot$table$factor == "general" & boot$table$status == "completed", ]
  expect_gt(nrow(general), 0L)
  expect_true(all(is.finite(general$lower) & is.finite(general$upper)))
  expect_true(all(general$lower <= general$estimate & general$estimate <= general$upper))
  # One fewer than the minimum is not a request DMAR forms an interval from.
  short <- sieve_bifactor_bootstrap(items, meta, B = 99L, seed = 1, conf_level = 0.80)
  expect_false(identical(short$uncertainty$status, "completed"))
})

test_that("production parallel analysis retains its quantile uncertainty", {
  X <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
  p <- DMAR::parallel_analysis(X, missing = "listwise", B = 200L, seed = 113)
  out <- sieve_pa_dmar_reference(p, list(ok = TRUE), "normal", list(ordered = FALSE, correlations = "pearson"))
  expect_true(out$available)
  expect_equal(out$mc_lower, p$reference_quantile_ci_lower)
  expect_equal(out$mc_upper, p$reference_quantile_ci_upper)
  expect_length(out$mc_lower, ncol(X))
  low <- sieve_pa_plan(sieve_retention_cor(X, rep("continuous", 6), missing = "listwise"), 199L)
  expect_match(low$refused, "at least 200", fixed = TRUE)
  expect_false(low$automatic)
})

test_that("HTMT records the engine's typed failed-resample counts", {
  d <- as.data.frame(setNames(rep(list(c(1, rep(0, 29))), 4), c("x1", "x2", "y1", "y2")))
  blocks <- list(A = c("x1", "x2"), B = c("y1", "y2"))
  h <- suppressWarnings(DMAR::htmt(d, blocks = blocks, B = 200L, seed = 1))
  counts <- sieve_dmar_boot_result(h, "HTMT")
  expect_identical(counts$used, 123L)
  expect_identical(counts$dropped, 77L)
  expect_identical(counts$attempted, 200L)
  expect_identical(counts$status, "dropped")
  expect_true(all(is.finite(h$ci_upper)))
  display <- sieve_htmt_bounds_table(h)
  expect_equal(display$Upper, h$ci_upper)
  # No reading against a cutoff (SIEVER applies no cutoff in output).
  expect_identical(names(display), c("Pair", "HTMT", "Upper"))
  expect_identical(nrow(display), 1L)
  bad <- h; attr(bad, "B_used") <- NULL
  expect_error(sieve_dmar_boot_result(bad, "HTMT"), "unsupported bootstrap count schema")
  sparse <- d
  sparse$y1 <- sparse$y2 <- c(0, 1, rep(0, 28))
  missing <- suppressWarnings(DMAR::htmt(sparse, blocks = blocks, B = 200L, seed = 1))
  withheld <- sieve_dmar_boot_result(missing, "HTMT")
  expect_gt(withheld$dropped, withheld$attempted / 2)
  expect_identical(withheld$status, "insufficient_share")
  expect_true(is.finite(missing$htmt))
  expect_true(is.na(sieve_htmt_bounds_table(missing)$Upper))
})

test_that("EFA quality needs DMAR's factor quality columns and retains all available intervals", {
  X <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  solution <- sieve_efa_dmar_solution(DMAR::fa_exploratory(X, nfactors = 3L, rotation = "oblimin", seed = 1))
  out <- sieve_efa_quality_table(solution)
  expect_equal(out$Determinacy_lower, solution$factor_quality$determinacy_ci_lower)
  expect_equal(out$H_upper, solution$factor_quality$H_ci_upper)
  expect_true(any(is.finite(out$Determinacy_lower)))
  bad <- solution; bad$factor_quality$determinacy_ci_lower <- NULL
  expect_error(sieve_efa_quality_table(bad), class = "sieve_dmar_schema_error")
})

test_that("optional AVE intervals preserve the fit, factors, counts and supported design", {
  X <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa("visual =~ x1+x2+x3; textual =~ x4+x5+x6", data = X, std.lv = TRUE)
  point <- DMAR::average_variance_extracted(fit)
  low <- sieve_ave_intervals(fit, B = 399L, seed = 5)
  expect_identical(low$status$code, "unavailable")
  expect_null(low$table)
  blocked <- sieve_ave_intervals(fit, B = 400L, seed = 5, sampling_unit = "clustered")
  expect_identical(blocked$status$code, "unavailable")
  expect_null(blocked$table)
  result <- sieve_ave_intervals(fit, B = 400L, seed = 5)
  expect_identical(result$counts$attempted, 400L)
  expect_identical(result$table$factor, point$factor)
  expect_equal(result$table$ave, point$ave)
  expect_true(all(is.finite(result$table$ci_lower)))
  expect_true(all(is.finite(result$table$ci_upper)))
  expect_identical(result$status$code, "ok")
  expect_equal(result$fit_signature, sieve_lavaan_signature(fit))
})

test_that("AVE interval controls preserve exact-request reuse and mark changed settings stale", {
  withr::local_options(sieve.auto_run = "never")
  calls <- 0L
  local_mocked_bindings(sieve_ave_intervals = function(fit, B, seed, conf_level, sampling_unit) {
    calls <<- calls + 1L
    factors <- lavaan::lavNames(fit, "lv")
    list(table = data.frame(factor = factors, ave = rep(.6, length(factors)),
      ci_lower = .5, ci_upper = .7), counts = list(attempted = B, used = B, dropped = 0L),
      status = list(code = "ok", message = "Fixture intervals."), warnings = character(),
      fit_signature = sieve_lavaan_signature(fit))
  })
  state <- siever_test_study()
  shiny::testServer(mod_validity_server, args = list(state = state), {
    session$setInputs(ave_B = 400L, ave_seed = 5L, ave_level = .95, ave_boot = 1L)
    expect_match(ave_intervals_now()$error, "Fit the current measurement model", fixed = TRUE)
    session$setInputs(fit_go = 1L)
    session$setInputs(ave_boot = 2L)
    expect_null(ave_intervals_now()$error)
    expect_identical(ave_intervals_now()$counts$attempted, 400L)
    expect_true(any(grepl("AVE percentile intervals", state$codelog, fixed = TRUE)))
    session$setInputs(ave_level = .9)
    expect_match(ave_intervals_now()$stale, "settings changed", fixed = TRUE)
    session$setInputs(ave_level = .95, ave_boot = 3L)
    expect_null(ave_intervals_now()$stale)
    expect_identical(calls, 1L)
    session$setInputs(ave_B = 399L, ave_boot = 4L)
    expect_match(ave_intervals_now()$error, "at least 400", fixed = TRUE)
    expect_identical(calls, 1L)
  })
})

test_that("Bollen-Stine retains DMAR's group count", {
  X <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
  meta <- data.frame(item = names(X), label = names(X), construct = rep(c("A", "B"), each = 3),
    facet = "", type = "continuous", status = "active", reverse = FALSE)
  spec <- sieve_cfa_spec(meta, reserved = names(X), estimator = "ml")
  result <- sieve_bollen_stine(spec, X, B = 100L, seed = 19)
  expect_null(result$error)
  expect_identical(result$groups, 1L)
  expect_identical(result$B, 100L)
  expect_true(is.finite(result$p_bs))
  expect_identical(result$groups, as.integer(result$table$value[result$table$term == "n_groups"]))
})

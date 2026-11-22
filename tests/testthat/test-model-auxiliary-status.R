test_that("covariance admissibility is invariant to measurement units", {
  invalid <- matrix(c(1, 2, 2, 1), 2L)
  valid <- matrix(c(2, 1, 1, 2), 2L)
  for (multiplier in c(1e-12, 1, 1e12)) {
    expect_false(sieve_covariance_nonnegative(multiplier * invalid))
    expect_true(sieve_covariance_nonnegative(multiplier * valid))
  }
  different_units <- diag(c(1e-6, 1e6))
  expect_false(sieve_covariance_nonnegative(different_units %*% invalid %*% different_units))
  expect_true(sieve_covariance_nonnegative(different_units %*% valid %*% different_units))
  expect_false(sieve_covariance_nonnegative(matrix(c(0, .1, .1, 1), 2)))
})

test_that("saturated auxiliaries do not mask substantive improper solutions", {
  x <- withr::with_seed(149L, {
    factor <- stats::rnorm(400)
    d <- as.data.frame(sapply(1:5, function(i) .7 * factor + stats::rnorm(400, sd = .7)))
    names(d) <- paste0("x", 1:5)
    d$aux <- factor + stats::rnorm(400, sd = .1)
    d
  })
  fit <- lavaan::cfa("F =~ x1 + x2 + x3 + x4 + x5", data = x, std.lv = TRUE,
    missing = "ml", aux = "aux", check.post = FALSE)
  full <- lavaan::lavInspect(fit, "est")$theta
  expect_lt(min(eigen(full, symmetric = TRUE, only.values = TRUE)$values), 0)
  expect_true(sieve_model_post_check(fit))
  expect_identical(sieve_fit_status(fit)$code, "ok")
  bad <- suppressWarnings(lavaan::cfa("F =~ x1 + x2 + x3 + x4 + x5\nx1 ~~ -0.1*x1",
    data = x, std.lv = TRUE, missing = "ml", aux = "aux", check.post = FALSE))
  expect_false(sieve_model_post_check(bad))
  expect_true("x1" %in% sieve_fit_status(bad)$negative_variances)
  expect_false(sieve_fit_status(bad)$admissible)
})

test_that("the Bifactor tab presents overlapping-group refusal as an action", {
  test_frame <- environment()
  local_mocked_bindings(bifactor_indices = function(...) {
    stop("Item t1_visual_perception loads on two group factors.", call. = FALSE)
  }, .package = "DMAR", .env = test_frame)
  withr::local_options(sieve.auto_run = "never")
  state <- siever_test_study()
  shiny::testServer(mod_bifactor_server, args = list(state = state), {
    session$setInputs(fit = 1L)
    expect_null(current()$indices)
    expect_match(current()$indices_reason, "Assign each item to only one group factor", fixed = TRUE)
    expect_match(as.character(output$status$html), "Assign each item to only one group factor", fixed = TRUE)
  })
})


test_that("reliability bootstraps in the background process retain the declared FIML auxiliaries", {
  # Reduced-B serial/worker transport mechanics; production FIML counts are tested separately.
  siever_local_dmar_mechanics()
  # (Every DMAR bootstrap of a run runs in one background process,
  # fct_dmar_boot.R.)
  d <- withr::with_seed(221L, {
    f <- stats::rnorm(120)
    x <- as.data.frame(sapply(1:6, function(i) .7 * f + stats::rnorm(120, sd = .7)))
    names(x) <- paste0("x", 1:6)
    x$aux <- f + stats::rnorm(120, sd = .4)
    x[1:15, 1:6] <- NA_real_
    x
  })
  specs <- lapply(list(paste0("x", 1:3), paste0("x", 4:6)), function(items) {
    sieve_reliability_spec(items, rep("continuous", 3), missing = "fiml",
      ci_method = "dmar_percentile", B = 40L, aux = "aux")
  })
  rng <- sieve_draw_seed(192L)
  serial <- lapply(specs, function(spec) sieve_reliability_omega(d, spec, rng = rng))
  calls <- lapply(specs, function(spec) {
    p <- sieve_reliability_point(d, spec, rng = rng, ci_method = "none")
    call <- sieve_reliability_boot_call(spec, rng, p$X, ncpus = 1L)
    # The auxiliary travels with the items, and the call names it.
    expect_true("aux" %in% names(call$args$data))
    expect_identical(call$args$aux, "aux")
    list(call = call, out = p$out)
  })
  job <- sieve_dmar_boot_start(lapply(calls, `[[`, "call"))
  on.exit(job$kill(), add = TRUE)
  # The limit allows for a busy machine; a process still running at it
  # fails here, saying so, rather than in the comparisons below.
  siever_wait_until(function() !job$poll())
  job$poll()
  expect_null(job$failure())
  for (i in seq_along(specs)) {
    background <- sieve_reliability_boot_complete(calls[[i]]$out, job$results[[i]])
    expect_true(is.finite(serial[[i]]$estimate))
    expect_identical(background$n, serial[[i]]$n)
    expect_identical(background$status$code, serial[[i]]$status$code)
    expect_identical(c(background$estimate, background$lower, background$upper),
      c(serial[[i]]$estimate, serial[[i]]$lower, serial[[i]]$upper))
    expect_match(paste(background$code, collapse = " "), "aux = \"aux\"", fixed = TRUE)
  }
})

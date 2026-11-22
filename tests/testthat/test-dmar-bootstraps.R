# The omega and bifactor bootstraps on DMAR 1.1.0, through DMAR's counted
# bootstrap engine and its progress and cancellation hook, run for the app in a background R
# process (fct_dmar_boot.R).
#
# The checks are oracles, not echoes. The limits SIEVER reports are compared
# with DMAR's own calls written out here, value for value. DMAR's resamples
# are reproduced from boot under DMAR's seed and passed through SIEVER's
# own acceptance rule (the engine SIEVER uses for cluster designs): on the
# same resamples the two rules use the same replications and give the same
# values (the parity anchor). boot::boot.ci() checks the
# percentile rule. The recorded R is run in a fresh R process and has to
# return the limits on screen. The modules are driven with shiny::testServer.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("boot")
skip_if_not_installed("callr")
skip_if(utils::packageVersion("DMAR") < "1.1.0")
skip_if_not("progress" %in% names(formals(DMAR::reliability_omega)))

# ---- fixtures --------------------------------------------------------------

hs9 <- function() {
  fx <- sieve_hs9_fixture()
  skip_if(is.null(fx))
  fx
}

hs9_state <- function() {
  fx <- hs9()
  st <- sieve_new_state()
  st$raw <- fx$data
  st$items <- fx$data[, fx$item_cols, drop = FALSE]
  st$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  st$confirmed <- TRUE
  st$palette <- "nd"
  st$thresholds <- sieve_default_thresholds()
  st
}

# Four items, one dominant, sixty rows: resamples with improper solutions,
# kept and set aside (the counts case of the parity check).
unstable_items <- function() {
  X <- withr::with_seed(4401, {
    f <- stats::rnorm(60)
    as.data.frame(sapply(c(.97, .45, .30, .20), function(l) l * f + sqrt(1 - l^2) * stats::rnorm(60)))
  })
  names(X) <- paste0("u", 1:4)
  X
}

# DMAR's resamples, from boot under DMAR's seed, as DMAR draws them.
dmar_rows <- function(pool, seed, B) {
  withr::with_seed(seed, {
    b <- boot::boot(pool, function(d, i) 0, R = B, stype = "i")
    boot::boot.array(b, indices = TRUE)
  })
}

# A bifactor population (as in test-bifactor-bootstrap.R).
bifactor_items <- function(n, lg, ls, seed) {
  group <- rep(1:3, each = 3)
  Y <- withr::with_seed(seed, {
    G <- stats::rnorm(n)
    S <- matrix(stats::rnorm(n * 3), n)
    E <- matrix(stats::rnorm(n * 9), n)
    sapply(1:9, function(j) lg[j] * G + ls[j] * S[, group[j]] + sqrt(1 - lg[j]^2 - ls[j]^2) * E[, j])
  })
  d <- as.data.frame(Y)
  names(d) <- sprintf("%s%d", c("a", "b", "c")[group], stats::ave(1:9, group, FUN = seq_along))
  meta <- data.frame(item = names(d), label = names(d), construct = c("Alpha", "Beta", "Gamma")[group],
    facet = "", type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE)
  list(data = d, meta = meta)
}

# Waits for a background job to end. The limit allows for a busy machine;
# a job still running at the limit fails here, saying so, rather than in
# the expectations on its results.
wait_for <- function(handle, seconds = 600) {
  until <- proc.time()[["elapsed"]] + seconds
  while (handle$poll() && proc.time()[["elapsed"]] < until) Sys.sleep(0.05)
  if (handle$poll()) {
    testthat::fail(sprintf("The background process was still running after %d seconds.", as.integer(seconds)))
  }
  invisible(handle)
}

# Drive a module's background bootstrap to its end under testServer: the
# page polls the background process every 200 ms of session time. The limit
# allows for a busy machine; a run still going at the limit fails here,
# saying so, rather than in the expectations on its results.
run_background <- function(session, running, seconds = 600) {
  siever_wait_until(function() !isTRUE(running()), what = "The page's background bootstrap",
    seconds = seconds, stop_on_timeout = TRUE, step = function() {
      session$elapse(200)
      Sys.sleep(0.05)
    })
}

# ---- the request resolves to DMAR's counted bootstrap ----------------------

test_that("the bootstrap codes resolve to DMAR's counted bootstrap, and a declared cluster keeps SIEVER's", {
  items <- c("x1", "x2", "x3")
  sp <- sieve_reliability_spec(items, rep("continuous", 3), "F", ci_method = "percentile", B = 100L)
  expect_identical(sp$ci_method, "percentile")
  expect_identical(sp$resampling, "dmar")
  expect_identical(sieve_reliability_engine_ci("percentile"), "percentile")
  # The percentile code dmar_percentile, which a saved study may hold,
  # resolves to the one counted call, with the same request key.
  old <- sieve_reliability_spec(items, rep("continuous", 3), "F", ci_method = "dmar_percentile", B = 100L)
  expect_identical(old$ci_method, "percentile")
  expect_identical(old$key, sp$key)
  bca <- sieve_reliability_spec(items, rep("ordered", 3), "F", ci_method = "recommended", B = 100L)
  expect_identical(c(bca$ci_method, bca$resampling), c("dmar_bca", "dmar"))
  # The call names B, seed, ncpus, and SIEVER's (DMAR's default) improper rule.
  call <- sieve_reliability_call(sp, seed = 9L, ncpus = 3L)
  expect_identical(call$fun, "reliability_omega")
  expect_identical(call$args[c("ci_method", "B", "seed", "ncpus", "improper")],
    list(ci_method = "percentile", B = 100L, seed = 9L, ncpus = 3L, improper = "keep"))
  expect_false("improper" %in% names(sieve_reliability_call(bca, seed = 9L)$args))
  # A declared cluster design is resampled in whole clusters by SIEVER.
  cl <- sieve_reliability_spec(items, rep("continuous", 3), "F", ci_method = "dmar_bca", B = 100L, cluster = rep(1:10, 3))
  expect_identical(c(cl$ci_method, cl$resampling), c("percentile", "siever"))
  expect_identical(cl$interval$label, "Whole-cluster bootstrap percentile")
  # One percentile entry on the menu, and a saved selection of the code
  # dmar_percentile is the percentile bootstrap, not a moved selection.
  m <- sieve_reliability_menu("continuous", selected = "dmar_percentile")
  # (DMAR's bootstrap standard error on the logit scale is on the menu; see
  # test-dmar-defaults.R.)
  expect_identical(unname(m$choices), c("recommended", "percentile", "dmar_bca", "bootstrap_se_logistic"))
  expect_identical(m$selected, "percentile")
  expect_false(m$changed)
  expect_match(names(m$choices)[2], "DMAR, counted replications", fixed = TRUE)
})

# ---- omega -----------------------------------------------------------------

test_that("robust omega's percentile interval is DMAR's call, value for value, with its counts read whole", {
  fx <- hs9()
  X <- fx$data[, fx$item_cols]
  items <- fx$theory_map$Visual
  sp <- sieve_reliability_spec(items, rep("continuous", 3), "Visual", missing = "listwise",
    ci_method = "percentile", B = 400L, seed = 11)
  res <- sieve_reliability_omega(X, sp)
  ref <- DMAR::reliability_omega(data = X[, items], ci_method = "percentile", denominator = "observed",
    missing = "listwise", conf_level = 0.95, B = 400L, seed = 11L, ncpus = 1L, improper = "keep")
  v <- function(term) ref$value[ref$term == term]
  expect_identical(c(res$estimate, res$se, res$lower, res$upper),
    c(v("estimate"), v("se"), v("ci_lower"), v("ci_upper")))
  expect_identical(res$uncertainty$status, "completed")
  expect_identical(res$uncertainty$label, "Bootstrap percentile")
  expect_identical(res$uncertainty$engine_method, "percentile")
  bs <- attr(ref, "bootstrap")
  cnt <- res$uncertainty$counts
  expect_identical(c(cnt$requested, cnt$attempted, cnt$used, cnt$improper_kept, cnt$improper_set_aside),
    c(400L, bs$attempted, bs$returned, bs$improper_kept, bs$improper))
  expect_identical(sum(cnt$outcome), cnt$attempted)
  expect_identical(res$uncertainty$draws$estimate, bs$t)
  expect_identical(res$uncertainty$draws$status, as.character(bs$status))
  expect_identical(res$uncertainty$caution, bs$caution)
  # The Monte Carlo error of a limit, and the bounds on the limits, which
  # collapse onto DMAR's limits when no replication was set aside.
  expect_equal(res$uncertainty$mc_se, sieve_bootstrap_mc_error(bs$returned, 0.95) * v("se"))
  expect_identical(bs$returned, 400L)
  expect_equal(unlist(res$uncertainty$limit_range), c(lower_min = v("ci_lower"), lower_max = v("ci_lower"),
    upper_min = v("ci_upper"), upper_max = v("ci_upper")), tolerance = 1e-12)
  # DMAR's rule, named where the result is read.
  expect_match(res$uncertainty$rule, "Half rule", fixed = TRUE)
  expect_match(res$uncertainty$rule, "improper = \"keep\"", fixed = TRUE)
  # The recorded R is the DMAR call, with seed, B, interval, level, missing
  # data treatment, ncpus and the rule.
  code <- paste(res$code, collapse = "\n")
  expect_match(code, "DMAR::reliability_omega(", fixed = TRUE)
  for (arg in c("ci_method = \"percentile\"", "denominator = \"observed\"", "missing = \"listwise\"",
    "conf_level = 0.95", "B = 400", "seed = 11", "ncpus = 1", "improper = \"keep\"")) {
    expect_match(code, arg, fixed = TRUE)
  }
  para <- sieve_reliability_paragraph(list(spec = sp, omega = res, H = list(estimate = NA_real_, label = "H",
    status = list(message = "not computed"), error = "not computed")))
  expect_match(para, "DMAR's counted bootstrap (DMAR::reliability_omega, B = 400, 1 process", fixed = TRUE)
  expect_match(para, "Replications: 400 attempted, 400 returned an estimate, 400 converged", fixed = TRUE)
})

test_that("for a fixed seed DMAR's limits do not depend on ncpus, and the progress hook does not change them", {
  skip_if_not(sieve_dmar_fork_safe())
  fx <- hs9()
  X <- fx$data[, fx$item_cols]
  items <- fx$theory_map$Textual
  sp <- sieve_reliability_spec(items, rep("continuous", 3), "Textual", ci_method = "percentile", B = 400L, seed = 5)
  one <- sieve_reliability_omega(X, sp, workers = 1L)
  seen <- new.env(parent = emptyenv())
  seen$calls <- 0L
  two <- sieve_reliability_omega(X, sp, workers = 2L, progress = function(done, total) {
    seen$calls <- seen$calls + 1L
    TRUE
  })
  expect_identical(c(one$lower, one$upper, one$se), c(two$lower, two$upper, two$se))
  expect_identical(one$uncertainty$draws, two$uncertainty$draws)
  expect_identical(c(one$uncertainty$ncpus, two$uncertainty$ncpus), c(1L, 2L))
  expect_gt(seen$calls, 0L)
  expect_match(paste(two$code, collapse = " "), "ncpus = 2", fixed = TRUE)
})

test_that("the pages ask for SIEVER's workers whatever this process may do, and the fork check is the forking process's", {
  # The pages do not decide ncpus in the Shiny process, which loads
  # processx at its first background run: decided there, every run after
  # the first would use one process. processx reads the switch
  # when it is loaded, so unsetting it here changes only SIEVER's check.
  skip_if_not("processx" %in% loadedNamespaces())
  skip_if(identical(.Platform$OS.type, "windows"))
  withr::local_envvar(PROCESSX_NOTIFY_OLD_SIGCHLD = NA)
  expect_false(sieve_dmar_fork_safe())
  # In this process (the exported functions): one process.
  expect_identical(sieve_dmar_boot_ncpus(4L), 1L)
  # For the background process (the pages): the workers asked for.
  expect_identical(sieve_dmar_boot_cores(4L), 4L)
  expect_identical(sieve_dmar_boot_cores(1L), 1L)
  expect_identical(sieve_dmar_boot_cores(NA), 1L)
  withr::local_envvar(PROCESSX_NOTIFY_OLD_SIGCHLD = "true")
  expect_true(sieve_dmar_fork_safe())
  expect_identical(sieve_dmar_boot_ncpus(4L), 4L)
  # The processes DMAR used, in words.
  expect_identical(sieve_dmar_process_text(list(8L, c(8L, NA))), "8 processes")
  expect_identical(sieve_dmar_process_text(list(1L)), "1 process")
  expect_identical(sieve_dmar_process_text(list(8L, 1L)), "1 and 8 processes")
  expect_null(sieve_dmar_process_text(list(NULL)))
})

test_that("on several processes the wait is a range, from the even split to one process", {
  # DMAR completes the replications in blocks with the progress hook, each
  # waiting for its slowest (measured: 60.3 s on one process and 52.1 s on
  # eight for a bifactor model with 5 of 400 replications failing).
  sieve_at_default_rates({
    one <- sieve_dmar_boot_wait_range(60, 1L)
    expect_identical(one[["low"]], one[["high"]])
    expect_equal(one[["low"]], sieve_rate("dmar_boot_start") + 60)
    eight <- sieve_dmar_boot_wait_range(60, 8L)
    expect_equal(eight[["low"]], sieve_rate("dmar_boot_start") + 60 / 8 + 50 * sieve_rate("dmar_boot_block"))
    expect_equal(eight[["high"]], sieve_rate("dmar_boot_start") + 60 + 50 * sieve_rate("dmar_boot_block"))
    expect_true(52.1 >= eight[["low"]] && 52.1 <= eight[["high"]])
    expect_identical(sieve_dmar_wait_range_label(one), sieve_duration_label(one[["low"]]))
    expect_identical(sieve_dmar_wait_range_label(eight), "between about 10 seconds and about 65 seconds")
  })
})

test_that("failed and improper replications are counted as DMAR counts them, and SIEVER's rule agrees on the same resamples", {
  X <- unstable_items()
  sp <- sieve_reliability_spec(names(X), rep("continuous", 4), "Unstable", missing = "listwise",
    ci_method = "percentile", B = 400L, seed = 77)
  res <- suppressWarnings(sieve_reliability_omega(X, sp))
  direct <- suppressWarnings(DMAR::reliability_omega(data = X, ci_method = "percentile", denominator = "observed",
    missing = "listwise", conf_level = 0.95, B = 400L, seed = 77L, ncpus = 1L, improper = "keep"))
  bs <- attr(direct, "bootstrap")
  cnt <- res$uncertainty$counts
  # The case has what the parity check needs.
  expect_gt(bs$improper + bs$improper_kept, 0L)
  expect_identical(cnt$used, bs$returned)
  expect_identical(cnt$attempted - cnt$used, bs$failed)
  expect_identical(cnt$improper_kept, bs$improper_kept)
  expect_identical(cnt$improper_set_aside, bs$improper)
  expect_identical(as.integer(cnt$outcome[["failed"]]), as.integer(bs$reasons[["error"]]))
  expect_identical(sum(res$uncertainty$reasons$draws), bs$failed)
  expect_identical(res$uncertainty$caution, bs$caution)
  expect_identical(sum(res$uncertainty$draws$outcome == "inadmissible"), bs$improper + bs$improper_kept)
  # The parity anchor: DMAR's resamples reproduced, each passed through
  # SIEVER's counted rule (the engine SIEVER uses for cluster designs). The two
  # rules use the same replications, and the values used agree.
  plan <- sieve_reliability_boot_plan(X, sp)
  idx <- dmar_rows(plan$pool, 77L, 400L)
  task <- sieve_reliability_boot_task(sp, plan$pool, integer(0), plan$path, plan$sampling)
  siever <- lapply(seq_len(400L), function(b) {
    resample <- plan$pool[idx[b, ], , drop = FALSE]
    rownames(resample) <- NULL
    run <- withr::with_preserve_seed(sieve_capture(sieve_reliability_boot_estimate(task, resample)))
    est <- if (run$ok) {
      if (is.data.frame(run$value)) sieve_reliability_value(run$value, "estimate") else as.numeric(run$value)[1]
    } else {
      NA_real_
    }
    c(used = as.numeric(run$ok && is.finite(est)), estimate = est)
  })
  used_siever <- vapply(siever, function(x) x[["used"]] == 1, logical(1))
  est_siever <- vapply(siever, function(x) x[["estimate"]], numeric(1))
  used_dmar <- res$uncertainty$draws$used
  expect_identical(used_siever, used_dmar)
  expect_lt(max(abs(est_siever[used_dmar] - res$uncertainty$draws$estimate[used_dmar])), 1e-8)
})

test_that("categorical omega's interval is DMAR's call, and alpha's bootstrap is DMAR's in plan and finish", {
  Xo <- withr::with_seed(20260929, {
    f <- stats::rnorm(150)
    as.data.frame(sapply(c(.5, .6, .7, .8), function(l) as.numeric(cut(l * f + sqrt(1 - l * l) * stats::rnorm(150),
      c(-Inf, -.7, 0, .7, Inf)))))
  })
  names(Xo) <- paste0("o", 1:4)
  sp <- sieve_reliability_spec(names(Xo), rep("ordered", 4), "Ordered", ci_method = "percentile", B = 400L, seed = 3)
  expect_identical(sp$engine, "reliability_omega_categorical")
  res <- suppressWarnings(sieve_reliability_omega(Xo, sp))
  ref <- suppressWarnings(do.call(DMAR::reliability_omega_categorical,
    c(list(data = Xo), sieve_reliability_call(sp, seed = 3L, ncpus = 1L)$args)))
  expect_identical(c(res$lower, res$upper), c(ref$value[ref$term == "ci_lower"], ref$value[ref$term == "ci_upper"]))
  expect_identical(res$uncertainty$counts$used, attr(ref, "bootstrap")$returned)
  # Alpha for comparison: the plan writes the DMAR call, the finish reads it,
  # and together they give what the one call gives.
  X <- hs9()$data[, hs9()$theory_map$Speed]
  # Listwise named (the API's default is DMAR's "fiml"),
  # as the direct call below names it.
  spa <- sieve_reliability_spec(names(X), rep("continuous", 3), "Speed", missing = "listwise",
    ci_method = "percentile", B = 400L, seed = 4)
  rng <- sieve_draw_seed(4)
  plan <- sieve_reliability_alpha_plan(X, spa, rng = rng, ncpus = 1L)
  expect_identical(plan$call$fun, "reliability_alpha")
  expect_true(plan$bootstrap)
  alpha <- sieve_reliability_alpha_finish(plan, sieve_dmar_boot_run(plan$call))
  direct <- DMAR::reliability_alpha(data = X, estimator = "analytic", missing = "listwise", ci_method = "percentile",
    conf_level = 0.95, B = 400L, seed = 4L, ncpus = 1L)
  expect_identical(c(alpha$lower, alpha$upper), c(direct$value[direct$term == "ci_lower"], direct$value[direct$term == "ci_upper"]))
  one_call <- sieve_reliability_alpha(X, spa, rng = rng)
  timed <- c("seconds")
  expect_identical(alpha[setdiff(names(alpha), timed)], one_call[setdiff(names(one_call), timed)])
  expect_match(alpha$code, "DMAR::reliability_alpha(", fixed = TRUE)
  expect_match(alpha$code, "ncpus = 1L", fixed = TRUE)
})

test_that("a stopped run, a failing callback and DMAR's refusals are read by class and keep their reasons", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  fx <- hs9()
  X <- fx$data[, fx$item_cols]
  sp <- sieve_reliability_spec(fx$theory_map$Speed, rep("continuous", 3), "Speed", ci_method = "percentile", B = 200L, seed = 2)
  stopped <- sieve_reliability_omega(X, sp, progress = function(done, total) done < 10L)
  expect_identical(stopped$uncertainty$status, "failed")
  expect_true(isTRUE(stopped$uncertainty$stopped))
  expect_true("dmar_bootstrap_cancelled" %in% stopped$uncertainty$condition_class)
  expect_match(stopped$uncertainty$reason, "canceled after 10 of 200 replications", fixed = TRUE)
  expect_true(all(is.na(c(stopped$lower, stopped$upper))))
  expect_true(is.finite(stopped$estimate))
  broken <- sieve_reliability_omega(X, sp, progress = function(done, total) stop("display gone"))
  expect_true("dmar_progress_error" %in% broken$uncertainty$condition_class)
  expect_match(broken$uncertainty$reason, "display gone", fixed = TRUE)
  # A refusal (a misnamed item, class dmar_items_invalid, a dmar_refusal)
  # is recognized by class, not by its words.
  refused <- sieve_dmar_boot_run(list(fun = "fa_exploratory", args = list(data = X, nfactors = 1L, items = "no_such_item")))
  expect_false(refused$ok)
  expect_true("dmar_refusal" %in% refused$error_class)
  expect_identical(sieve_dmar_boot_outcome(refused), "refused")
  expect_match(sieve_dmar_boot_failure_text(refused), "DMAR refused the bootstrap", fixed = TRUE)
  relabeled <- refused
  relabeled$error <- "different words entirely"
  expect_identical(sieve_dmar_boot_outcome(relabeled), "refused")
})

test_that("boot's percentile rule is reproduced, and the bounds on the limits bracket them", {
  x <- withr::with_seed(8, stats::rnorm(173))
  for (conf in c(0.80, 0.95)) {
    ref <- boot:::norm.inter(x, (1 + c(-conf, conf)) / 2)[, 2]
    a <- (1 - conf) / 2
    expect_equal(c(sieve_boot_quantile(sort(x), a), sieve_boot_quantile(sort(x), 1 - a)), ref, tolerance = 1e-14)
  }
  full <- sieve_dmar_percentile_range(x, 173L, 0.95)
  expect_equal(full$lower_min, full$lower_max)
  set_aside <- sieve_dmar_percentile_range(x, 200L, 0.95)
  lim <- boot:::norm.inter(x, c(0.025, 0.975))[, 2]
  expect_true(set_aside$lower_min <= lim[1] && lim[1] <= set_aside$lower_max)
  expect_true(set_aside$upper_min <= lim[2] && lim[2] <= set_aside$upper_max)
  expect_identical(sieve_dmar_min_used(0.95), 39L)
})

test_that("the background process returns what this process returns, reports progress, and stops at the next replication", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  fx <- hs9()
  X <- fx$data[, fx$item_cols]
  calls <- lapply(c("Visual", "Speed"), function(cn) {
    sp <- sieve_reliability_spec(fx$theory_map[[cn]], rep("continuous", 3), cn, ci_method = "percentile", B = 150L, seed = 21)
    p <- sieve_reliability_point(X, sp, rng = sieve_draw_seed(21), ci_method = "none")
    sieve_reliability_boot_call(sp, p$out$uncertainty$rng, p$X, ncpus = 1L)
  })
  job <- sieve_dmar_boot_start(calls)
  on.exit(job$kill(), add = TRUE)
  wait_for(job)
  expect_null(job$failure())
  expect_true(all(vapply(job$results, function(r) isTRUE(r$ok), logical(1))))
  # The background process may fork: processx was not loaded there, so its
  # own check passes whether or not this process has loaded processx, and
  # it ran each call with the processes asked for.
  expect_true(all(vapply(job$results, function(r) isFALSE(r$processx_loaded), logical(1))))
  expect_true(all(vapply(job$results, function(r) isTRUE(r$fork_safe), logical(1))))
  expect_identical(vapply(job$results, function(r) r$ncpus, integer(1)), c(1L, 1L))
  expect_identical(job$progress$call, 2L)
  expect_identical(job$progress$done, 150L)
  here <- sieve_dmar_boot_run(calls[[1]])
  expect_identical(job$results[[1]]$value, here$value)
  job$release()
  # Stop: DMAR cancels at the next replication, with its class, and the
  # calls after it are not run.
  long <- calls
  long[[1]]$args$B <- 5000L
  job2 <- sieve_dmar_boot_start(long)
  on.exit(job2$kill(), add = TRUE)
  # Stop is pressed once replication 5 is done, so DMAR is running when it
  # arrives; a process that has not reached it at the limit fails here.
  siever_wait_until(function() !job2$poll() || job2$progress$done >= 5L,
    what = "The background process, short of replication 5,")
  job2$stop()
  wait_for(job2, 120)
  expect_null(job2$failure())
  first <- job2$results[[1]]
  expect_identical(sieve_dmar_boot_outcome(first), "cancelled")
  expect_true(first$condition$done < 5000L)
  expect_null(job2$results[[2]])
})

test_that("the recorded DMAR call reproduces the limits on screen in a fresh R process", {
  fx <- hs9()
  X <- fx$data[, fx$item_cols]
  sp <- sieve_reliability_spec(fx$theory_map$Visual, rep("continuous", 3), "Visual", missing = "fiml",
    ci_method = "percentile", B = 400L)
  res <- sieve_reliability_omega(X, sp)
  expect_false(res$uncertainty$rng$supplied)
  replay <- callr::r(function(code, items) {
    env <- new.env()
    env$items <- items
    eval(parse(text = code), envir = env)
    v <- env$omega_Visual
    c(v$value[v$term == "ci_lower"], v$value[v$term == "ci_upper"])
  }, args = list(code = paste(res$code, collapse = "\n"), items = X), libpath = .libPaths())
  expect_identical(replay, c(res$lower, res$upper))
})

# ---- the bifactor indices --------------------------------------------------

test_that("the bifactor intervals are DMAR's ci_bifactor_indices(), with SIEVER's denominator, rule and counts", {
  s <- bifactor_items(300, lg = rep(c(.55, .60, .65), 3), ls = rep(c(.50, .55, .45), 3), seed = 2101)
  b <- sieve_bifactor_bootstrap(s$data, s$meta, B = 400L, seed = 7)
  expect_identical(b$engine, "dmar")
  fit <- sieve_fit_bifactor(s$data, s$meta)$fit
  ref <- suppressWarnings(DMAR::ci_bifactor_indices(fit, general = lavaan::lavNames(fit, "lv")[1],
    denominator = "model_implied", ci_method = "percentile", conf_level = 0.95, B = 400L, seed = 7L,
    ncpus = 1L, improper = "keep"))
  keys <- sieve_bifactor_boot_dmar_keys(list(quantities = data.frame(factor = b$table$factor, index = b$table$index),
    name_map = sieve_fit_bifactor(s$data, s$meta)$name_map))
  at <- match(keys, paste(ref$factor, ref$index, sep = ":"))
  shown <- b$table$status == "completed"
  expect_identical(b$table$lower[shown], ref$ci_lower[at][shown])
  expect_identical(b$table$upper[shown], ref$ci_upper[at][shown])
  expect_equal(b$table$estimate[shown], ref$estimate[at][shown], tolerance = 1e-10)
  # PUC is fixed by the model, as SIEVER says and DMAR's NA limits agree.
  puc <- b$table$index == "PUC"
  expect_identical(b$table$interval[puc], "fixed by the model")
  expect_true(all(is.na(ref$ci_lower[ref$index == "PUC"])))
  bs <- attr(ref, "bootstrap")
  expect_identical(c(b$counts$attempted, b$counts$used, b$counts$improper_kept), c(bs$attempted, bs$returned, bs$improper_kept))
  expect_identical(b$caution, bs$caution)
  expect_match(b$rule, "improper = \"keep\"", fixed = TRUE)
  # DMAR 1.1.0 applies its half rule to the bifactor indices too (its
  # bootstrap attribute says so, whatever its help page says); the rule
  # SIEVER states follows the result.
  expect_true(isTRUE(bs$require_half))
  expect_match(b$rule, "Half rule", fixed = TRUE)
  notes <- sieve_bifactor_boot_notes(b)
  expect_match(notes$method, "DMAR::ci_bifactor_indices()", fixed = TRUE)
  expect_match(notes$method, "model-implied denominator", fixed = TRUE)
  # The recorded R: lavaan's fit and DMAR's call reproduce the limits in a
  # fresh R process.
  code <- paste(b$code, collapse = "\n")
  expect_match(code, "DMAR::ci_bifactor_indices(bifactor_boot_fit,", fixed = TRUE)
  replay <- callr::r(function(code, items) {
    env <- new.env()
    env$items <- items
    eval(parse(text = code), envir = env)
    env$bifactor_boot[, c("ci_lower", "ci_upper")]
  }, args = list(code = code, items = s$data), libpath = .libPaths())
  expect_identical(replay$ci_lower[at][shown], b$table$lower[shown])
  # When DMAR withholds its intervals by its half rule (fewer than half of
  # the replications returned values, as on the nine Holzinger and
  # Swineford tests, whose bifactor solution is improper), SIEVER says so.
  half <- ref
  hb <- attr(half, "bootstrap")
  # Keep exactly twenty genuinely successful rows and mark every other
  # requested replication failed; count metadata must describe the same rows.
  kept <- which(hb$status == "proper")[seq_len(20L)]
  kept_values <- hb$t[kept, , drop = FALSE]
  hb$status[] <- "error"
  hb$t[,] <- NA_real_
  hb$status[seq_len(20L)] <- "proper"
  hb$t[seq_len(20L), ] <- kept_values
  hb$returned <- 20L
  hb$failed <- as.integer(length(hb$status) - 20L)
  hb$improper_kept <- 0L
  hb$caution <- TRUE
  half$ci_lower <- NA_real_
  half$ci_upper <- NA_real_
  attr(half, "bootstrap") <- hb
  plan <- sieve_bifactor_boot_plan(s$data, s$meta, B = 400L, seed = 7)
  withheld <- sieve_bifactor_boot_from_dmar(plan, list(ok = TRUE, value = half, warnings = character(0), seconds = 1))
  expect_identical(withheld$uncertainty$status, "failed")
  expect_match(withheld$uncertainty$reason, "fewer than half, so DMAR forms no interval (its half rule)", fixed = TRUE)
  expect_identical(withheld$counts$used, 20L)
  expect_true(all(withheld$table$status %in% c("failed", "fixed")))
})

test_that("on the same resamples SIEVER's bifactor rule and DMAR's use the same replications and give the same indices", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  s <- bifactor_items(150, lg = rep(c(.60, .65, .70), 3), ls = c(.50, .55, .45, .50, .45, .55, .30, .35, .30), seed = 3002)
  plan <- sieve_bifactor_boot_plan(s$data, s$meta, B = 30L, seed = 5)
  # Two processes: several, and within the two that R CMD check allows.
  run <- suppressWarnings(sieve_dmar_boot_run(sieve_bifactor_boot_dmar_call(plan, ncpus = sieve_dmar_boot_ncpus(2L))))
  b <- sieve_bifactor_boot_from_dmar(plan, run)
  idx <- dmar_rows(DMAR:::.bifactor_boot_pool(plan$fit)$data, 5L, 30L)
  q <- plan$quantities
  reads <- lapply(seq_len(30L), function(i) {
    resample <- plan$pool[idx[i, ], , drop = FALSE]
    rownames(resample) <- NULL
    fitted <- withr::with_preserve_seed(sieve_capture(plan$fit_fun(resample, plan$meta)))
    sieve_bifactor_boot_read(fitted$value %||% list(), keys = q[, c("row", "index")],
      signature = plan$point$signature, error = fitted$error)
  })
  used <- vapply(reads, function(r) r$used, logical(1))
  expect_identical(used, b$draws$used)
  values <- do.call(rbind, lapply(reads, function(r) r$values))
  varies <- !q$fixed
  expect_lt(max(abs(values[used, varies] - b$estimates[used, varies])), 1e-8)
  # An unstable model: some replications set aside, some improper kept.
  expect_gt(b$counts$attempted - b$counts$used, 0L)
})

test_that("a declared cluster design keeps SIEVER's whole-cluster bifactor bootstrap", {
  s <- bifactor_items(120, lg = rep(c(.55, .60, .65), 3), ls = rep(c(.50, .55, .45), 3), seed = 2101)
  plan <- sieve_bifactor_boot_plan(s$data, s$meta, B = 3L, seed = 1, cluster = rep(seq_len(40), each = 3))
  expect_identical(plan$engine, "siever")
  expect_match(plan$rule, "A replication is used only when its fit was returned", fixed = TRUE)
  unclustered <- sieve_bifactor_boot_plan(s$data, s$meta, B = 3L, seed = 1)
  expect_identical(unclustered$engine, "dmar")
})

# ---- the pages -------------------------------------------------------------

test_that("the Reliability tab runs DMAR's bootstrap in a background process, with progress, Stop and the function's result", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  st <- hs9_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "observed", ci_method = "percentile",
      missing = "listwise", conf_level = 0.95, B = 120, boot_seed = 31,
      show_alpha = FALSE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    expect_identical(reliab()$uncertainty, rep("pending", 3))
    note <- as.character(output$boot_note$html)
    expect_match(note, "background R process", fixed = TRUE)
    expect_match(note, "half rule", fixed = TRUE)
    # Where Stop is heard on several processes: at the end of DMAR's block,
    # or by ending the process.
    expect_match(note, "at the end of the block of replications in progress", fixed = TRUE)
    expect_match(note, "five seconds after the press", fixed = TRUE)

    session$setInputs(run_boot = 1)
    expect_true(boot_running())
    expect_identical(boot_job$mode, "dmar")
    expect_match(as.character(output$stop_boot_ui$html), "Cancel the bootstrap")
    # Nothing is started inside the click.
    expect_null(boot_job$handle)
    run_background(session, boot_running)
    expect_false(boot_running())
    done <- reliab()
    expect_identical(done$uncertainty, rep("completed", 3))
    expect_identical(done$boot_attempted, rep(120L, 3))
    expect_true(all(done$interval == "Bootstrap percentile"))
    # The page's result is the function's: the same DMAR call, the same seed.
    first <- results()[[1]]$omega
    fn <- sieve_reliability_omega(shiny::isolate(state$items), first$spec, rng = first$uncertainty$rng)
    expect_identical(c(first$lower, first$upper), c(fn$lower, fn$upper))
    expect_identical(first$uncertainty$counts$outcome, fn$uncertainty$counts$outcome)
    expect_match(paste(first$code, collapse = " "), "DMAR::reliability_omega(", fixed = TRUE)
    expect_match(paste(first$code, collapse = " "), sprintf("ncpus = %d", sieve_dmar_boot_ncpus()), fixed = TRUE)
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("DMAR's counted bootstrap", log, fixed = TRUE)))
    expect_match(as.character(output$footnote$html), "half rule", fixed = TRUE)
    # The log names the processes DMAR used, as its results report them.
    expect_true(any(grepl(sprintf("DMAR's counted bootstrap, %s", sieve_dmar_process_text(first$uncertainty$ncpus)),
      log, fixed = TRUE)))

    # Stop: the construct in progress gets no interval and stays not run.
    session$setInputs(B = 4000)
    expect_identical(reliab()$uncertainty, rep("pending", 3))
    session$setInputs(run_boot = 2)
    until <- proc.time()[["elapsed"]] + 60
    while ((is.null(boot_job$handle) || boot_job$handle$progress$done < 5L) && proc.time()[["elapsed"]] < until) {
      session$elapse(200)
      Sys.sleep(0.05)
    }
    expect_true(boot_running())
    session$setInputs(stop_boot = 1)
    run_background(session, boot_running, 60)
    expect_false(boot_running())
    expect_identical(reliab()$uncertainty, rep("pending", 3))
    expect_match(boot_last()$sentence, "canceled after", fixed = TRUE)
    expect_match(boot_last()$sentence, "has no interval", fixed = TRUE)
  })
})

test_that("the Bifactor tab runs DMAR's bootstrap in a background process and records the DMAR call", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  s <- bifactor_items(300, lg = rep(c(.55, .60, .65), 3), ls = rep(c(.50, .55, .45), 3), seed = 2101)
  fixture <- list(data = s$data, item_cols = names(s$data), name = "bifactor scenario", theory_map = NULL,
    labels = NULL, load_code = character())
  st <- siever_test_study(fixture, labels = FALSE, theory = FALSE, configure = function(x) {
    x$meta <- s$meta
    x$roles <- NULL
  })
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    session$setInputs(fit = 1)
    expect_match(as.character(output$idx_plan_note$html), "background R process", fixed = TRUE)
    session$setInputs(idx_B = 60, idx_seed = 7, idx_boot = 1)
    expect_true(boot_running())
    expect_identical(job$mode, "dmar")
    expect_match(as.character(output$idx_stop_ui$html), "Cancel the bootstrap")
    run_background(session, boot_running)
    expect_false(boot_running())
    r <- idx_ci()
    full <- sieve_bifactor_bootstrap(s$data, s$meta, B = 60L, seed = 7)
    expect_identical(r$table, full$table)
    expect_identical(r$counts, full$counts)
    note <- as.character(output$idx_ci_note$html)
    expect_match(note, "DMAR::ci_bifactor_indices()", fixed = TRUE)
    expect_match(note, "Seed 7", fixed = TRUE)
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("DMAR::ci_bifactor_indices(bifactor_boot_fit,", log, fixed = TRUE)))
    # Stop: no interval from a stopped run.
    session$setInputs(idx_B = 5000, idx_boot = 2)
    until <- proc.time()[["elapsed"]] + 60
    while ((is.null(job$handle) || job$handle$progress$done < 1L) && proc.time()[["elapsed"]] < until) {
      session$elapse(200)
      Sys.sleep(0.05)
    }
    session$setInputs(idx_stop = 1)
    run_background(session, boot_running, 60)
    expect_false(boot_running())
    expect_match(idx_ci()$notice, "DMAR forms no interval from a canceled run", fixed = TRUE)
  })
})

test_that("Stop pressed before a second run has started stops it cleanly, without the first run's calls", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  # start_boot() must not keep the previous run's DMAR calls: a Stop before
  # the second run has prepared its own would then give "subscript out of
  # bounds".
  st <- hs9_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "observed", ci_method = "percentile", missing = "listwise",
      conf_level = 0.95, B = 60, boot_seed = 51, show_alpha = FALSE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    session$setInputs(run_boot = 1)
    run_background(session, boot_running)
    expect_identical(reliab()$uncertainty, rep("completed", 3))
    # (Omega's call and alpha's of each construct.)
    expect_length(boot_job$owners, 6L)
    m <- shiny::isolate(st$meta)
    m$status[m$item == m$item[m$construct == "Speed"][1]] <- "excluded"
    st$meta <- m
    session$setInputs(B = 80, run_go = 2)
    session$setInputs(run_boot = 2)
    expect_true(boot_running())
    expect_null(boot_job$owners)
    session$setInputs(stop_boot = 1)
    run_background(session, boot_running, 60)
    expect_false(boot_running())
    expect_identical(boot_last()$state, "failed")
    expect_identical(boot_last()$sentence, "The bootstrap stopped before returning an interval.")
    expect_false(grepl("subscript", boot_last()$sentence, fixed = TRUE))
    # Nothing of the second request is stored: its constructs stay not run.
    expect_identical(reliab()$uncertainty, rep("pending", 3))
  })
})

test_that("in a fresh session that has loaded processx without the switch, both pages run DMAR on several processes and say so", {
  # Small-count process/worker mechanics only, not inferential adequacy.
  # The production condition: the app runs without PROCESSX_NOTIFY_OLD_SIGCHLD, and its first
  # background run loads processx in the Shiny process. The pages ask for
  # SIEVER's workers (two here), the background process decides that it
  # may fork, and the wait, the recorded call, the result and the code log
  # name the processes DMAR used.
  skip_if(identical(.Platform$OS.type, "windows"))
  root <- normalizePath(testthat::test_path("..", ".."))
  installed <- !file.exists(file.path(root, "DESCRIPTION"))
  got <- callr::r(function(root, installed, mechanics) {
    if (installed) {
      suppressMessages(library(SIEVER))
    } else {
      suppressMessages(pkgload::load_all(root, quiet = TRUE, helpers = FALSE))
    }
    mechanics()
    loadNamespace("processx")
    options(sieve.cores = 2L)
    seen <- new.env()
    seen$switch_set <- nzchar(Sys.getenv("PROCESSX_NOTIFY_OLD_SIGCHLD"))
    seen$fork_safe_here <- SIEVER:::sieve_dmar_fork_safe()
    # Returns TRUE when the run is still going at the limit (this process
    # has no testthat reporter, so the caller fails on it).
    poll <- function(session, running, seconds = 600) {
      until <- proc.time()[["elapsed"]] + seconds
      while (isTRUE(running()) && proc.time()[["elapsed"]] < until) {
        session$elapse(200)
        Sys.sleep(0.05)
      }
      isTRUE(running())
    }
    # The Reliability page on the nine tests (as hs9_state() builds it).
    fx <- SIEVER:::sieve_hs9_fixture()
    st <- SIEVER:::sieve_new_state()
    st$raw <- fx$data
    st$items <- fx$data[, fx$item_cols, drop = FALSE]
    st$meta <- SIEVER:::sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
    st$confirmed <- TRUE
    st$palette <- "nd"
    st$thresholds <- SIEVER:::sieve_default_thresholds()
    suppressWarnings(shiny::testServer(SIEVER:::mod_reliab_server, args = list(state = st), {
      session$setInputs(denominator = "observed", ci_method = "percentile", missing = "listwise",
        conf_level = 0.95, B = 60, boot_seed = 31, show_alpha = TRUE, score_method = "Bartlett")
      session$setInputs(run_go = 1)
      seen$reliab_note <- as.character(output$boot_note$html)
      session$setInputs(run_boot = 1)
      seen$reliab_timed_out <- poll(session, boot_running)
      seen$reliab_running <- boot_running()
      seen$reliab_used <- unname(vapply(results(), function(r) as.integer(r$omega$uncertainty$ncpus %||% NA), integer(1)))
      seen$alpha_used <- unname(vapply(results(), function(r) as.integer(r$alpha$uncertainty$ncpus %||% NA), integer(1)))
      seen$reliab_code <- paste(unlist(lapply(results(), function(r) r$omega$code)), collapse = "\n")
      seen$reliab_log <- paste(shiny::isolate(st$codelog), collapse = "\n")
    }))
    # The Bifactor page on a bifactor population (as siever_test_study()
    # builds the study in the Bifactor page tests).
    d <- withr::with_seed(2101, {
      group <- rep(1:3, each = 3)
      lg <- rep(c(.55, .60, .65), 3)
      ls <- rep(c(.50, .55, .45), 3)
      G <- stats::rnorm(300)
      S <- matrix(stats::rnorm(900), 300)
      E <- matrix(stats::rnorm(2700), 300)
      as.data.frame(sapply(1:9, function(j) lg[j] * G + ls[j] * S[, group[j]] + sqrt(1 - lg[j]^2 - ls[j]^2) * E[, j]))
    })
    names(d) <- paste0(rep(c("a", "b", "c"), each = 3), 1:3)
    meta <- data.frame(item = names(d), label = names(d), construct = rep(c("Alpha", "Beta", "Gamma"), each = 3),
      facet = "", type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE)
    fixture <- list(data = d, item_cols = names(d), name = "bifactor scenario", theory_map = NULL,
      labels = NULL, load_code = character())
    st2 <- SIEVER:::sieve_study_state(fixture, confirmed = FALSE, labels = FALSE, theory = FALSE, name = fixture$name)
    shiny::isolate({
      st2$meta <- meta
      st2$roles <- NULL
      SIEVER:::sieve_confirm_types(st2, origin = "auto")
      st2$codelog <- character()
    })
    suppressWarnings(shiny::testServer(SIEVER:::mod_bifactor_server, args = list(state = st2), {
      session$setInputs(fit = 1)
      session$setInputs(idx_B = 40, idx_seed = 7)
      seen$bifactor_note <- as.character(output$idx_plan_note$html)
      session$setInputs(idx_boot = 1)
      seen$bifactor_asked <- job$ncpus
      seen$bifactor_timed_out <- poll(session, boot_running)
      r <- idx_ci()
      seen$bifactor_used <- as.integer(r$ncpus %||% NA)
      seen$bifactor_code <- paste(r$code, collapse = "\n")
      seen$bifactor_log <- paste(shiny::isolate(st2$codelog), collapse = "\n")
    }))
    seen$processx_loaded <- "processx" %in% loadedNamespaces()
    as.list(seen)
  }, args = list(root = root, installed = installed, mechanics = siever_local_dmar_mechanics), libpath = .libPaths(),
  env = c(callr::rcmd_safe_env(), PROCESSX_NOTIFY_OLD_SIGCHLD = ""))
  # A page whose run was still going at the limit fails here, by name,
  # rather than in the process counts below.
  if (isTRUE(got$reliab_timed_out)) {
    testthat::fail("The Reliability page's background bootstrap was still running after 600 seconds.")
  }
  if (isTRUE(got$bifactor_timed_out)) {
    testthat::fail("The Bifactor page's background bootstrap was still running after 600 seconds.")
  }
  # The production condition: no switch, processx loaded, so this process
  # may not fork.
  expect_false(got$switch_set)
  expect_false(got$fork_safe_here)
  expect_true(got$processx_loaded)
  # Reliability: the wait, the call, DMAR's result and the log say two.
  expect_false(got$reliab_running)
  expect_match(got$reliab_note, "spread over 2 R processes", fixed = TRUE)
  expect_identical(got$reliab_used, rep(2L, 3))
  expect_identical(got$alpha_used, rep(2L, 3))
  expect_match(got$reliab_code, "ncpus = 2L?\\b")
  expect_false(grepl("ncpus = 1L?\\b", got$reliab_code))
  expect_match(got$reliab_log, "DMAR's counted bootstrap, 2 processes", fixed = TRUE)
  expect_false(grepl("1 process\\b", got$reliab_log))
  # Bifactor: the same, and the wait is a range on several processes.
  expect_match(got$bifactor_note, "spread\\s+evenly over 2 R processes")
  expect_match(got$bifactor_note, "between about", fixed = TRUE)
  expect_match(got$bifactor_note, "each block waits for its slowest", fixed = TRUE)
  expect_identical(got$bifactor_asked, 2L)
  expect_identical(got$bifactor_used, 2L)
  expect_match(got$bifactor_code, "ncpus = 2", fixed = TRUE)
  expect_match(got$bifactor_log, "DMAR::ci_bifactor_indices(), 2 processes", fixed = TRUE)
})

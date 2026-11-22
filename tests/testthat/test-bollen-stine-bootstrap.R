# The Bollen-Stine bootstrap of the CFA tab is
# DMAR::sem_exact_fit_bootstrap() for complete and incomplete data, and the
# residual statistic beside a robust two-stage fit is DMAR::sem_fit_measures()
# on a normal theory two-stage refit; these tests pin both against
# independent computations.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

fb_one_factor <- function() {
  x <- withr::with_seed(20260929, {
    f <- stats::rnorm(500)
    as.data.frame(sapply(
      c(.5, .6, .7, .8, .65, .75),
      function(l) l * f + sqrt(1 - l * l) * stats::rnorm(500)
    ))
  })
  names(x) <- paste0("x", 1:6)
  x
}
fb_meta <- function(x, construct = "F") {
  data.frame(
    item = names(x), label = names(x), construct = rep(construct, length.out = ncol(x)),
    facet = "", type = "continuous", status = "active", reverse = FALSE,
    stringsAsFactors = FALSE
  )
}
# The one-factor data with values missing at random given x1.
fb_incomplete <- function() {
  x <- fb_one_factor()
  withr::with_seed(2026, {
    drop <- stats::runif(500) < stats::plogis(x$x1) - 0.3
    x[drop, c("x3", "x5")] <- NA
    x[sample(500, 40), "x6"] <- NA
  })
  x
}
fb_value <- function(tbl, term) tbl$value[tbl$term == term]

# ---- The bootstrap engine --------------------------------------------------

test_that("complete data: the bootstrap is DMAR's, which draws lavaan's resamples for the same seed", {
  x <- fb_one_factor()
  spec <- sieve_cfa_spec(fb_meta(x), reserved = names(x))
  withr::local_seed(3)
  before <- .Random.seed
  r <- sieve_bollen_stine(spec, x, B = 100L, seed = 41L)
  # The caller's stream is untouched by a supplied seed.
  expect_identical(.Random.seed, before)
  expect_null(r$error)
  expect_false(r$refused)
  expect_identical(r$missing, "listwise")
  expect_identical(r$transformation, "bollen_stine")
  expect_identical(r$rng$seed, 41L)
  expect_identical(r$n, 500L)
  expect_s3_class(r$table, "dmar_tbl")
  # Independent: lavaan's own Bollen-Stine bootstrap with the seed as iseed
  # draws the same resamples, so the statistics agree draw for draw and the
  # p-values exactly.
  direct <- lavaan::cfa("F =~ x1 + x2 + x3 + x4 + x5 + x6", x,
    estimator = "ML", std.lv = TRUE, test = "bollen.stine", bootstrap = 100L, iseed = 41L
  )
  bs <- lavaan::lavInspect(direct, "test")$bollen.stine
  expect_equal(attr(r$table, "bootstrap")$chi_square, as.numeric(bs$boot.T), tolerance = 1e-8)
  expect_identical(r$p_bs, bs$pvalue)
  expect_equal(r$chisq, unname(lavaan::fitMeasures(direct, "chisq")), tolerance = 1e-10)
  expect_equal(r$p_ml, unname(lavaan::fitMeasures(direct, "pvalue")), tolerance = 1e-10)
  expect_identical(r$df, 9)
  # The counts and the Monte Carlo interval, recomputed from DMAR's record of
  # every resample.
  draws <- attr(r$table, "bootstrap")
  used <- draws$status %in% c("proper", "improper") & is.finite(draws$chi_square)
  expect_identical(r$B, 100L)
  expect_identical(r$B_used, sum(used))
  expect_identical(r$B_used + r$B_failed, 100L)
  expect_identical(r$p_bs, mean(draws$chi_square[used] >= r$chisq))
  expect_equal(r$bootstrap_mean, mean(draws$chi_square[used]))
  mc <- DMAR::ci_proportion(sum(draws$chi_square[used] >= r$chisq), sum(used), conf_level = 0.95)
  expect_equal(r$p_bs_lower, mc$value[mc$term == "ci_lower"])
  expect_equal(r$p_bs_upper, mc$value[mc$term == "ci_upper"])
  # The method sentence names DMAR and the transformation.
  expect_match(r$method, "DMAR::sem_exact_fit_bootstrap()", fixed = TRUE)
  expect_match(r$method, "Bollen and Stine's (1992) transformation", fixed = TRUE)
  # Without a seed, one is drawn from the caller's stream (which moves one
  # draw) and recorded.
  withr::local_seed(9)
  drawn <- sieve_bollen_stine(spec, x, B = 100L)
  expect_false(drawn$rng$supplied)
  expect_identical(drawn$p_bs, sieve_bollen_stine(spec, x, B = 100L, seed = drawn$rng$seed)$p_bs)
})

test_that("incomplete data: FIML on every respondent, Savalei and Yuan's transformation, and DMAR's own call", {
  x <- fb_incomplete()
  spec <- sieve_cfa_spec(fb_meta(x), reserved = names(x))
  r <- sieve_bollen_stine(spec, x, B = 100L, seed = 7L)
  expect_null(r$error)
  expect_identical(r$missing, "fiml")
  expect_identical(r$transformation, "common")
  # Every row with an observed item is used: no available response deleted.
  expect_identical(r$n, sum(rowSums(!is.na(x)) > 0L))
  expect_lt(sum(stats::complete.cases(x)), r$n)
  # The same numbers as DMAR called on the same FIML fit with the same
  # settings: SIEVER adds nothing between the refit and DMAR.
  fit <- lavaan::cfa("F =~ x1 + x2 + x3 + x4 + x5 + x6", x,
    estimator = "ML", std.lv = TRUE, missing = "fiml"
  )
  ref <- suppressWarnings(DMAR::sem_exact_fit_bootstrap(fit, B = 100L, seed = 7L))
  expect_identical(r$table$value, ref$value)
  expect_identical(attr(r$table, "bootstrap"), attr(ref, "bootstrap"))
  # DMAR's check of the transformation: the saturated FIML estimating
  # equations of the transformed data hold, and every pattern's means are the
  # model implied means.
  check <- r$transformation_check[[1L]]
  expect_identical(check$method, "common")
  expect_gt(check$patterns, 1L)
  expect_lt(check$score, 1e-6)
  expect_lt(check$mean_residual, 1e-8)
  expect_match(r$method, "FIML on the 500 respondents with Savalei and Yuan's (2009) transformation", fixed = TRUE)
  expect_match(r$method, "one matrix for every missing data pattern", fixed = TRUE)
  # DMAR finds the common matrix with the items in standard deviation units,
  # so the bootstrap does not depend on the units of the items: an item
  # scored ten times larger gives the same statistics, resample for resample.
  # (semTools' bsBootMiss() solves it in raw units, so its transformed data,
  # though they also reproduce the model moments exactly, depend on them.)
  rescaled <- x
  rescaled$x3 <- 10 * rescaled$x3
  r10 <- sieve_bollen_stine(spec, rescaled, B = 100L, seed = 7L)
  expect_equal(attr(r10$table, "bootstrap")$chi_square, attr(r$table, "bootstrap")$chi_square, tolerance = 1e-6)
  expect_identical(r10$p_bs, r$p_bs)
  # The pattern transformation is passed through.
  p <- sieve_bollen_stine(spec, x, B = 100L, seed = 7L, transformation = "pattern")
  expect_identical(p$transformation, "pattern")
  expect_match(p$method, "each missing data pattern on its own", fixed = TRUE)
})

test_that("every resample is counted, improper ones kept or set aside as asked", {
  # Reduced-B outcome/scope mechanics; production B100 is tested separately.
  siever_local_dmar_mechanics()
  # A small, weakly determined two-factor model with missing values, fitted
  # with an iteration limit that its refits inherit, so that some resamples
  # converge to improper solutions and some do not converge.
  d <- withr::with_seed(2, {
    n <- 70
    f1 <- stats::rnorm(n)
    f2 <- .4 * f1 + sqrt(1 - .16) * stats::rnorm(n)
    x <- cbind(
      sapply(c(.7, .6, .65), function(l) l * f1 + sqrt(1 - l^2) * stats::rnorm(n)),
      sapply(c(.3, .25, .3), function(l) l * f2 + sqrt(1 - l^2) * stats::rnorm(n))
    )
    colnames(x) <- paste0("y", 1:6)
    x <- as.data.frame(x)
    for (j in c(2, 5)) x[stats::runif(n) < .2, j] <- NA
    x
  })
  fit <- lavaan::cfa("A =~ y1 + y2 + y3\nB =~ y4 + y5 + y6", d,
    missing = "ml", std.lv = TRUE, control = list(iter.max = 150)
  )
  expect_true(lavaan::lavInspect(fit, "post.check"))
  plan <- list(out = list(warnings = character(0), improper = "keep", n = 70L, rng = list(seed = 1L)))
  for (improper in c("keep", "drop")) {
    plan$out$improper <- improper
    run <- sieve_bollen_stine_engine(fit, list(B = 40L, improper = improper, seed = 1L, ncpus = 1L))
    r <- sieve_bollen_stine_read(plan, run)
    status <- attr(r$table, "bootstrap")$status
    expect_gt(r$B_improper, 0L)
    expect_gt(r$B_failed, 0L)
    expect_identical(r$B_improper, sum(status == "improper"))
    expect_identical(r$B_failed, sum(status %in% c("not_converged", "error")))
    expect_identical(r$B_used, if (improper == "keep") sum(status %in% c("proper", "improper")) else sum(status == "proper"))
    # DMAR's warning about the resamples set aside reaches the result.
    expect_true(any(grepl("bootstrap samples were set aside", r$warnings, fixed = TRUE)))
    counts <- sieve_bollen_stine_counts(r)
    expect_match(counts, sprintf("%d improper (%s)", r$B_improper, if (improper == "keep") "kept" else "set aside"), fixed = TRUE)
    expect_match(counts, sprintf("%d failed", r$B_failed), fixed = TRUE)
  }
})

test_that("refusals are SIEVER's or DMAR's, recognized by class and shown with the reason", {
  test_frame <- environment()
  x <- fb_one_factor()
  spec <- sieve_cfa_spec(fb_meta(x), reserved = names(x))
  # A classed DMAR refusal: recognized by its class, whatever its words.
  local_mocked_bindings(sem_exact_fit_bootstrap = function(...) stop(structure(
    list(message = "This fit is out of the bootstrap's scope (classed, in new words).", call = NULL,
      code = "bootstrap_scope", reason = "two_stage"),
    class = c("dmar_bootstrap_scope", "dmar_refusal", "error", "condition")
  )), .package = "DMAR", .env = test_frame)
  r <- sieve_bollen_stine(spec, x, B = 100L, seed = 1L)
  expect_true(r$refused)
  expect_true(all(c("dmar_bootstrap_scope", "dmar_refusal") %in% r$error_class))
  # One sentence for every refusal (DMAR classes its refusals); the class is
  # kept on the result.
  expect_match(r$error, "DMAR did not run the bootstrap:", fixed = TRUE)
  expect_match(r$error, "out of the bootstrap's scope (classed, in new words).", fixed = TRUE)
  expect_true(all(is.na(c(r$p_bs, r$B_used))))
  # An unclassed stop from DMAR is still shown with DMAR's own reason.
  local_mocked_bindings(sem_exact_fit_bootstrap = function(...) {
    stop("sem_exact_fit_bootstrap() does not take a two-level model.", call. = FALSE)
  }, .package = "DMAR", .env = test_frame)
  r <- sieve_bollen_stine(spec, x, B = 100L, seed = 1L)
  expect_true(r$refused)
  expect_match(r$error, "DMAR did not run the bootstrap: sem_exact_fit_bootstrap() does not take a two-level model.", fixed = TRUE)
})

test_that("SIEVER refuses what the row bootstrap cannot carry before calling DMAR", {
  x <- fb_one_factor()
  meta <- fb_meta(x)
  ordered <- meta
  ordered$type <- "ordered"
  out <- sieve_bollen_stine(sieve_cfa_spec(ordered), x, B = 100L, seed = 1L)
  expect_true(out$refused)
  expect_match(out$error, "categorical (WLSMV) estimator", fixed = TRUE)
  expect_null(out$rng)
  weighted <- sieve_spec_engine_args(sieve_cfa_spec(meta), list(sampling.weights = "w"))
  out <- sieve_bollen_stine(weighted, cbind(x, w = 1), B = 100L, seed = 1L)
  expect_true(out$refused)
  expect_match(out$error, "sampling weights", fixed = TRUE)
  out <- sieve_bollen_stine(sieve_cfa_spec(meta), x[1:20, ], B = 100L, seed = 1L)
  expect_match(out$error, "too few to bootstrap", fixed = TRUE)
  expect_match(sieve_bollen_stine(sieve_cfa_spec(meta), x, B = 1L)$error, "at least 100", fixed = TRUE)
})

test_that("the recorded R is the refit and one DMAR call, and it reproduces the result", {
  x <- fb_incomplete()
  spec <- sieve_cfa_spec(fb_meta(x), reserved = names(x))
  r <- sieve_bollen_stine(spec, x, B = 100L, seed = 11L, improper = "drop")
  code <- sieve_bollen_stine_code(r)
  expect_true(any(grepl(
    "DMAR::sem_exact_fit_bootstrap(bs_fit, B = 100, transformation = \"common\", improper = \"drop\", conf_level = 0.95, seed = 11, ncpus = 1)",
    code, fixed = TRUE
  )))
  expect_true(any(grepl("missing = \"fiml\"", code, fixed = TRUE)))
  expect_false(any(grepl("SIEVER:", code, fixed = TRUE)))
  expect_false(any(grepl("semTools", code, fixed = TRUE)))
  env <- new.env(parent = globalenv())
  env$items <- x
  eval(parse(text = code), envir = env)
  expect_identical(env$bollen_stine$value, r$table$value)
})

test_that("incomplete data: the bootstrap p agrees with semTools' on the same resamples (stored reference)", {
  # Independent of DMAR at test time: values stored from semTools 0.5-9's
  # bsBootMiss(transformation = 2, nBoot = 150, seed = 113) with lavaan 0.7-3
  # on these data with the rows sorted by missing data pattern (semTools
  # holds its transformed rows in that order, so both packages draw the same
  # rows for the seed). semTools finds the common matrix in raw units and
  # DMAR in standard deviation units, so the resampled statistics differ
  # slightly (by up to 0.024 here); the observed statistic is the same and
  # the p-values agree within one Monte Carlo standard error of the stored p.
  x <- fb_incomplete()
  key <- apply(is.na(x), 1L, function(r) paste(as.integer(r), collapse = ""))
  x <- x[order(match(key, unique(key))), , drop = FALSE]
  rownames(x) <- NULL
  spec <- sieve_cfa_spec(fb_meta(x), reserved = names(x))
  r <- sieve_bollen_stine(spec, x, B = 150L, seed = 113L)
  expect_null(r$error)
  expect_identical(r$transformation, "common")
  stored <- c(chisq = 8.6694432116, p = 0.5266666667)
  expect_equal(r$chisq, unname(stored["chisq"]), tolerance = 1e-9)
  mc_se <- sqrt(stored[["p"]] * (1 - stored[["p"]]) / 150)
  expect_lt(abs(r$p_bs - stored[["p"]]), mc_se)
  expect_identical(r$B_failed, 0L)
})

test_that("the counts agree with refits made independently on the same resamples", {
  # Reduced-B outcome/scope mechanics; production B100 is tested separately.
  siever_local_dmar_mechanics()
  # Complete data, so Bollen and Stine's transformation can be written out
  # here (symmetric square roots), and an iteration limit the refits inherit,
  # so that some resamples converge to improper solutions and some do not
  # converge. Each resample is refit by lavaan::cfa() from the fit's
  # estimates, on the rows DMAR draws for the seed (all drawn before any
  # refit, one sample.int(n, replace = TRUE) per resample).
  d <- withr::with_seed(7, {
    n <- 70
    f1 <- stats::rnorm(n)
    f2 <- .4 * f1 + sqrt(1 - .16) * stats::rnorm(n)
    x <- cbind(
      sapply(c(.7, .6, .65), function(l) l * f1 + sqrt(1 - l^2) * stats::rnorm(n)),
      sapply(c(.3, .25, .3), function(l) l * f2 + sqrt(1 - l^2) * stats::rnorm(n))
    )
    colnames(x) <- paste0("y", 1:6)
    as.data.frame(x)
  })
  model <- "A =~ y1 + y2 + y3\nB =~ y4 + y5 + y6"
  fit <- lavaan::cfa(model, d, std.lv = TRUE, control = list(iter.max = 150))
  expect_true(lavaan::lavInspect(fit, "post.check"))
  B <- 40L
  plan <- list(out = list(warnings = character(0), improper = "keep", n = 70L, rng = list(seed = 1L)),
    args = list(transformation = "common"))
  r <- sieve_bollen_stine_read(plan, sieve_bollen_stine_engine(fit, list(B = B, improper = "keep", seed = 1L, ncpus = 1L)))
  root <- function(M, inverse = FALSE) {
    e <- eigen(M, symmetric = TRUE)
    e$vectors %*% diag(if (inverse) 1 / sqrt(e$values) else sqrt(e$values)) %*% t(e$vectors)
  }
  X <- as.matrix(d)
  centered <- sweep(X, 2L, colMeans(X))
  Sigma <- unclass(lavaan::lavInspect(fit, "cov.ov"))[colnames(X), colnames(X)]
  Y <- centered %*% root(crossprod(centered) / nrow(X), inverse = TRUE) %*% root(Sigma)
  colnames(Y) <- colnames(X)
  rows <- withr::with_seed(1L, lapply(seq_len(B), function(b) sample.int(nrow(Y), replace = TRUE)))
  mine <- lapply(rows, function(i) {
    f <- tryCatch(suppressWarnings(lavaan::cfa(model, as.data.frame(Y[i, ]), std.lv = TRUE,
      control = list(iter.max = 150), se = "none", baseline = FALSE, start = fit)), error = function(e) NULL)
    if (is.null(f)) return(list(status = "error", stat = NA_real_))
    if (!isTRUE(lavaan::lavInspect(f, "converged"))) return(list(status = "not_converged", stat = NA_real_))
    list(
      status = if (isTRUE(suppressWarnings(lavaan::lavInspect(f, "post.check")))) "proper" else "improper",
      stat = unname(lavaan::fitMeasures(f, "chisq"))
    )
  })
  status <- vapply(mine, `[[`, character(1), "status")
  stat <- vapply(mine, `[[`, numeric(1), "stat")
  expect_gt(sum(status == "improper"), 0L)
  expect_gt(sum(status == "not_converged"), 0L)
  expect_identical(r$B_improper, sum(status == "improper"))
  expect_identical(r$B_failed, sum(status %in% c("not_converged", "error")))
  expect_identical(r$B_used, sum(status %in% c("proper", "improper")))
  expect_identical(as.character(attr(r$table, "bootstrap")$status), status)
  used <- status %in% c("proper", "improper")
  expect_equal(attr(r$table, "bootstrap")$chi_square[used], stat[used], tolerance = 1e-6)
  expect_equal(r$p_bs, mean(stat[used] >= r$chisq))
})

# The bundled example's Textual and Speed tests with values missing at
# random, and age as an auxiliary variable.
fb_aux_data <- function() {
  fx <- sieve_hs9_fixture()
  items <- fx$item_cols[4:9]
  x <- fx$data[, items]
  withr::with_seed(20261106, {
    z <- as.numeric(scale(x$t6_paragraph_comprehension))
    drop <- stats::runif(nrow(x)) < stats::plogis(-1.4 + 0.9 * z)
    x[drop, c("t7_sentence", "t12_counting_groups_of_dots")] <- NA
    x[stats::runif(nrow(x)) < 0.15, "t13_straight_and_curved_capitals"] <- NA
  })
  x$age_years <- fx$data$age_years
  meta <- fb_meta(x[, items], construct = rep(c("Textual", "Speed"), each = 3))
  plain <- sieve_cfa_spec(meta, reserved = names(x))
  list(data = x, spec = sieve_spec_engine_args(plain, list(aux = "age_years")), plain = plain)
}

test_that("with auxiliary variables, resamples are flagged rather than called improper, and kept", {
  # Reduced-B outcome/scope mechanics; production B100 is tested separately.
  siever_local_dmar_mechanics()
  a <- fb_aux_data()
  # Set aside would select the resamples by lavaan's check of the auxiliary
  # block, so it is refused before anything runs, naming the control.
  drop <- sieve_bollen_stine(a$spec, a$data, B = 40L, seed = 113L, improper = "drop")
  expect_true(drop$refused)
  expect_null(drop$rng)
  expect_match(drop$error, "Set aside is not available with auxiliary variables (age_years)", fixed = TRUE)
  expect_match(drop$error, "Choose Keep in the p-value under Improper resamples.", fixed = TRUE)
  r <- sieve_bollen_stine(a$spec, a$data, B = 40L, seed = 113L)
  expect_null(r$error)
  expect_identical(r$aux, "age_years")
  expect_identical(r$B_used, r$B - r$B_failed)
  expect_match(r$method, "the auxiliary variable age_years entering as saturated correlates (Graham, 2003)", fixed = TRUE)
  counts <- sieve_bollen_stine_counts(r)
  expect_match(counts, sprintf("%d flagged by lavaan's admissibility check (kept;", r$B_improper), fixed = TRUE)
  expect_false(grepl("improper (", counts, fixed = TRUE))
  rows <- sieve_bollen_stine_rows(r)
  expect_true("Resamples flagged by lavaan's check, auxiliary variables included (kept)" %in% rows$Statistic)
  # With complete data the refit uses the complete cases, the auxiliary
  # variable plays no part, and Set aside is available.
  complete <- a$data[stats::complete.cases(a$data), , drop = FALSE]
  rc <- sieve_bollen_stine(a$spec, complete, B = 20L, seed = 1L, improper = "drop")
  expect_null(rc$error)
  expect_identical(rc$aux, character(0))
})

test_that("a refit proper in its own parameters but flagged by lavaan's check with auxiliaries is refused with the true reason", {
  # Reduced-B outcome/scope mechanics; production B100 is tested separately.
  siever_local_dmar_mechanics()
  a <- fb_aux_data()
  # A resample of the data on which the saturated correlates make the joint
  # covariance matrix of the item residuals and age indefinite.
  rows <- withr::with_seed(1, {
    sample(nrow(a$data), replace = TRUE)
    sample(nrow(a$data), replace = TRUE)
  })
  x <- a$data[rows, , drop = FALSE]
  rownames(x) <- NULL
  fit <- suppressMessages(suppressWarnings(lavaan::cfa(a$spec$model, x, std.lv = TRUE, missing = "ml", aux = "age_years")))
  # Independent of SIEVER's check: no variance is negative, the items'
  # residual covariance matrix and the latent covariance matrix are positive
  # definite, and only the matrix with age included is not.
  theta <- lavaan::lavInspect(fit, "theta")
  items <- setdiff(rownames(theta), "age_years")
  expect_false(isTRUE(suppressWarnings(lavaan::lavInspect(fit, "post.check"))))
  expect_true(all(diag(theta) > 0))
  expect_gt(min(eigen(theta[items, items], only.values = TRUE)$values), 0)
  expect_gt(min(eigen(lavaan::lavInspect(fit, "cov.lv"), only.values = TRUE)$values), 0)
  expect_lt(min(eigen(theta, only.values = TRUE)$values), 0)
  r <- sieve_bollen_stine(a$spec, x, B = 20L, seed = 1L)
  expect_true(r$refused)
  expect_identical(r$status$code, "ok")
  expect_null(r$rng)
  expect_match(r$error, "although the model's own parameters are proper", fixed = TRUE)
  expect_match(r$error, "remove the auxiliary role", fixed = TRUE)
  expect_false(grepl("negative variance estimate", r$error, fixed = TRUE))
  # Without the auxiliary variable the same data are bootstrapped.
  expect_null(sieve_bollen_stine(a$plain, x[, setdiff(names(x), "age_years")], B = 20L, seed = 1L)$error)
})

test_that("a pattern too small for the pattern-wise transformation names the control to change, and FIML has the same floor", {
  # Reduced-B outcome/scope mechanics; production B100 is tested separately.
  siever_local_dmar_mechanics()
  x <- fb_one_factor()
  x[1:3, c("x1", "x2")] <- NA
  x[4:60, "x3"] <- NA
  spec <- sieve_cfa_spec(fb_meta(x), reserved = names(x))
  r <- sieve_bollen_stine(spec, x, B = 20L, seed = 1L, transformation = "pattern")
  expect_true(r$refused)
  expect_match(r$error, "DMAR did not run the bootstrap:", fixed = TRUE)
  expect_match(r$error, "Under Missing responses, choose One matrix for every pattern (Savalei & Yuan)", fixed = TRUE)
  expect_match(r$error, "and 1 pattern here does not.", fixed = TRUE)
  # The common transformation runs on the same data.
  expect_null(sieve_bollen_stine(spec, x, B = 20L, seed = 1L)$error)
  # Another refusal (here a mocked unclassed one) gets no such advice when
  # every pattern is large enough.
  y <- fb_incomplete()
  test_frame <- environment()
  local_mocked_bindings(sem_exact_fit_bootstrap = function(...) {
    stop("sem_exact_fit_bootstrap() does not take a two-level model.", call. = FALSE)
  }, .package = "DMAR", .env = test_frame)
  r <- sieve_bollen_stine(sieve_cfa_spec(fb_meta(y), reserved = names(y)), y, B = 20L, seed = 1L, transformation = "pattern")
  expect_false(grepl("Under Missing responses", r$error, fixed = TRUE))
  # The floor of 30 respondents applies to a FIML refit as to complete cases.
  small <- fb_one_factor()[1:20, ]
  small[1, "x1"] <- NA
  r <- sieve_bollen_stine(sieve_cfa_spec(fb_meta(small), reserved = names(small)), small, B = 20L, seed = 1L)
  expect_true(r$refused)
  expect_identical(r$error, "Only 20 respondents with an observed item; too few to bootstrap.")
  expect_null(r$rng)
})

# ---- The CFA tab -----------------------------------------------------------

fb_state <- function(configure = NULL) {
  siever_test_study(sieve_hs9_fixture(), confirmed = TRUE, labels = TRUE, theory = TRUE, configure = configure)
}
fb_inputs <- function(session, ...) {
  session$setInputs(
    structure = "first", ident = "varone", layout = "tree2",
    what = "std", min_path = 0, resid = TRUE, covs = TRUE, ...
  )
}
fb_text <- function(x) gsub("\\s+", " ", paste(as.character(unlist(x)), collapse = " "))

# Waits for the background run, advancing the mock clock while real time
# passes. A run still going at the limit fails here, and FALSE tells the
# caller to leave its result unchecked.
fb_finished <- function(session, running) {
  siever_wait_until(function() !isTRUE(shiny::isolate(running())), "The Bollen-Stine run",
    step = function() {
      Sys.sleep(0.05)
      session$elapse(250)
    })
}

test_that("the CFA tab runs the bootstrap in the background, says progress is not reported, and records the DMAR call", {
  st <- fb_state(function(s) {
    s$items[1:40, "t2_cubes"] <- NA_real_
    s$items[31:70, "t7_sentence"] <- NA_real_
  })
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    fb_inputs(session, bs_B = 100, bs_seed = 113, bs_transformation = "common", bs_improper = "keep")
    session$setInputs(bs_run = 1)
    # Started, not finished inside the click: the page shows the run, how
    # long it should take, that DMAR reports no progress, and Stop.
    expect_true(bs_running())
    note <- fb_text(output$bs_note)
    expect_match(note, "running in the background (B = 100", fixed = TRUE)
    expect_match(note, "does not report its progress", fixed = TRUE)
    expect_match(fb_text(output$bs_stop_ui), "Cancel the bootstrap", fixed = TRUE)
    expect_error(output$bs_tbl, class = "shiny.silent.error")
    if (fb_finished(session, bs_running)) {
      r <- bs_result()
      expect_null(r$error)
      expect_true(r$solved)
      expect_identical(r$missing, "fiml")
      expect_identical(r$transformation, "common")
      expect_identical(r$seed, 113L)
      # The module's numbers are the function's, at full precision.
      spec <- sieve_sampling_apply(build_spec(), state, state$items)
      direct <- sieve_bollen_stine(spec$spec, spec$data, B = 100L, seed = 113L)
      expect_identical(r$table$value, direct$table$value)
      note <- fb_text(output$bs_note)
      expect_match(note, "DMAR::sem_exact_fit_bootstrap(): FIML on the", fixed = TRUE)
      expect_match(note, "Savalei and Yuan's (2009) transformation", fixed = TRUE)
      # The bundled example declares age as an auxiliary variable, so DMAR's
      # improper count is reported as flagged by lavaan's check (see
      # sieve_bollen_stine()).
      expect_identical(r$aux, "age_years")
      expect_match(note, "the auxiliary variable age_years entering as saturated correlates", fixed = TRUE)
      expect_match(note, sprintf("100 resamples attempted, %d used in the p-value, %d flagged by lavaan's admissibility check (kept;", r$B_used, r$B_improper), fixed = TRUE)
      expect_match(note, sprintf("%d failed", r$B_failed), fixed = TRUE)
      expect_match(note, "Transformation check (DMAR)", fixed = TRUE)
      expect_match(note, "Seed = 113.", fixed = TRUE)
      expect_no_error(output$bs_tbl)
      # The rows the table shows (rounded for display; the result is not).
      rows <- sieve_bollen_stine_rows(r)
      shown <- stats::setNames(rows$Value, rows$Statistic)
      expect_identical(shown[["Resamples attempted"]], "100")
      expect_identical(shown[["Resamples used"]], as.character(r$B_used))
      expect_identical(shown[["Resamples flagged by lavaan's check, auxiliary variables included (kept)"]], as.character(r$B_improper))
      expect_identical(shown[["Failed resamples"]], as.character(r$B_failed))
      expect_identical(shown[["p (Bollen-Stine bootstrap)"]], sprintf("%.4f", r$p_bs))
      expect_identical(shown[["Monte Carlo 95% interval for the bootstrap p"]],
        sprintf("[%.4f, %.4f]", r$p_bs_lower, r$p_bs_upper))
      expect_identical(shown[["Transformation"]], "Savalei-Yuan, one matrix for every pattern")
      expect_identical(shown[["Respondents"]], as.character(r$n))
      log <- paste(shiny::isolate(state$codelog), collapse = "\n")
      expect_match(log, "DMAR::sem_exact_fit_bootstrap(bs_fit, B = 100, transformation = \"common\", improper = \"keep\", conf_level = 0.95, seed = 113, ncpus = 1)", fixed = TRUE)
      expect_false(grepl("SIEVER::sieve_bollen_stine", log, fixed = TRUE))
      expect_false(grepl("SIEVER:::", log, fixed = TRUE))
    }
  })
})

test_that("Stop ends a background run, which then reports and records nothing", {
  st <- fb_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    fb_inputs(session, bs_B = 1000, bs_seed = 5)
    steps <- length(shiny::isolate(state$codelog))
    session$setInputs(bs_run = 1)
    expect_true(bs_running())
    handle <- bs_job$handle
    session$elapse(250)
    session$setInputs(bs_stop = 1)
    expect_false(bs_running())
    # The worker is gone.
    deadline <- Sys.time() + 10
    while (handle$is_alive() && Sys.time() < deadline) Sys.sleep(0.05)
    expect_false(handle$is_alive())
    r <- bs_result()
    expect_true(r$stopped)
    expect_match(r$error, "Stopped: the Bollen-Stine bootstrap was canceled after", fixed = TRUE)
    expect_match(fb_text(output$bs_note), "a canceled run reports nothing", fixed = TRUE)
    expect_error(output$bs_tbl, class = "shiny.silent.error")
    expect_identical(length(shiny::isolate(state$codelog)), steps)
    # A change of the request while it runs stops it too.
    session$setInputs(bs_B = 900, bs_run = 2)
    expect_true(bs_running())
    session$setInputs(bs_improper = "drop")
    session$flushReact()
    expect_false(bs_running())
    expect_true(isTRUE(bs_result()$stale))
  })
})

test_that("Missing responses is not part of a complete-data request: changing it neither stops a run nor stales its result", {
  st <- fb_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    fb_inputs(session, bs_B = 100, bs_seed = 3, bs_transformation = "common")
    session$setInputs(bs_run = 1)
    expect_true(bs_running())
    handle <- bs_job$handle
    session$setInputs(bs_transformation = "pattern")
    session$flushReact()
    # DMAR ignores the transformation with complete data, so the run goes on.
    expect_true(bs_running())
    expect_identical(bs_job$handle, handle)
    if (fb_finished(session, bs_running)) {
      r <- bs_result()
      expect_null(r$error)
      expect_identical(r$transformation, "bollen_stine")
      expect_false(bs_scope()$incomplete)
      session$setInputs(bs_transformation = "common")
      session$flushReact()
      expect_null(bs_result()$error)
      expect_false(isTRUE(bs_result()$stale))
      # A change of Improper resamples still makes it stale.
      session$setInputs(bs_improper = "drop")
      session$flushReact()
      expect_true(isTRUE(bs_result()$stale))
    }
  })
  # With a missing value it is part of the request: a change stops the run.
  st <- fb_state(function(s) s$items[1:40, "t2_cubes"] <- NA_real_)
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    fb_inputs(session, bs_B = 1000, bs_seed = 3, bs_transformation = "common")
    session$setInputs(bs_run = 1)
    expect_true(bs_running())
    session$setInputs(bs_transformation = "pattern")
    session$flushReact()
    expect_false(bs_running())
    expect_true(isTRUE(bs_result()$stale))
  })
})

# ---- The residual statistic beside a two-stage fit -------------------------

fb_two_stage_data <- function(aux = FALSE) {
  d <- lavaan::HolzingerSwineford1939[, c(paste0("x", 1:9), "ageyr")]
  withr::with_seed(113, {
    drop <- stats::runif(nrow(d)) < stats::plogis((d$ageyr - 13) * 1.2) - 0.25
    d[drop, c("x2", "x5")] <- NA
    d[sample(301, 30), "x9"] <- NA
  })
  if (aux) d else d[, 1:9]
}

test_that("the residual statistic is DMAR's for a normal theory two-stage refit and matches semTools' value", {
  meta <- data.frame(
    item = paste0("x", 1:9), label = paste0("x", 1:9),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L), facet = NA_character_,
    type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
  model <- "Visual =~ x1 + x2 + x3\nTextual =~ x4 + x5 + x6\nSpeed =~ x7 + x8 + x9"
  # Values stored from semTools 0.5-9 (cfa.2stage(se = "standard"), then its
  # anova method, which forms Browne's residual based statistic from the
  # saturated model's normal theory covariance; semTools prints it only for
  # se = "standard"), with lavaan 0.7-3, on these data. semTools estimates
  # the saturated moments by direct maximization and lavaan by EM, so the two
  # agree to their convergence tolerances (relative 4e-8 and 7e-9 here; at
  # converged first stages, 9e-12 and 1e-9).
  stored <- c(no_aux = 69.5812940260, aux = 69.8184435363)
  for (aux in c(FALSE, TRUE)) {
    d <- fb_two_stage_data(aux)
    spec <- sieve_two_stage_spec(sieve_cfa_spec(meta, reserved = names(d)), TRUE)
    if (aux) spec <- sieve_spec_engine_args(spec, list(se = "robust.two.stage", test = "satorra.bentler", aux = "ageyr"))
    res <- sieve_two_stage_residual(spec, d)
    expect_null(res$error)
    expect_identical(res$value$df, 24)
    # DMAR on a lavaan fit made by hand.
    args <- list(model, data = d, estimator = "ML", std.lv = TRUE, missing = "two.stage", fixed.x = FALSE)
    if (aux) args$aux <- "ageyr"
    ref <- DMAR::sem_fit_measures(do.call(lavaan::cfa, args))
    expect_equal(res$value$chisq, fb_value(ref, "chi_square"), tolerance = 1e-12)
    expect_equal(res$value$pvalue, fb_value(ref, "p_chi_square"), tolerance = 1e-12)
    expect_equal(res$value$chisq, unname(stored[if (aux) "aux" else "no_aux"]), tolerance = 1e-7)
    expect_match(res$test, "two-stage residual based statistic (Savalei & Bentler, 2009)", fixed = TRUE)
    # The recorded R reproduces it from the objects the recorded fit defines.
    env <- new.env(parent = globalenv())
    env$cfa_model <- spec$model
    env$cfa_model_data <- d
    eval(parse(text = res$code), envir = env)
    expect_identical(fb_value(env$two_stage_residual, "chi_square"), res$value$chisq)
    expect_false(any(grepl("semTools", res$code, fixed = TRUE)))
  }
})

test_that("a refusal of the residual statistic is recognized by class and shown with its reason", {
  test_frame <- environment()
  d <- fb_two_stage_data()
  meta <- data.frame(
    item = paste0("x", 1:9), label = paste0("x", 1:9),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L), facet = NA_character_,
    type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
  spec <- sieve_two_stage_spec(sieve_cfa_spec(meta, reserved = names(d)), TRUE)
  rows <- "The fitted sample is not the all-row sample (classed, in new words)."
  local_mocked_bindings(sem_fit_measures = function(...) stop(structure(
    list(message = rows, call = NULL, code = "two_stage_rows_unavailable"),
    class = c("dmar_two_stage_rows_unavailable", "dmar_refusal", "error", "condition")
  )), .package = "DMAR", .env = test_frame)
  res <- sieve_two_stage_residual(spec, d)
  expect_null(res$value)
  expect_true(res$refused)
  expect_true("dmar_two_stage_rows_unavailable" %in% res$error_class)
  expect_identical(res$error, paste("DMAR refused the residual statistic for this fit:", rows))
  ui <- fb_text(sieve_two_stage_residual_ui(res))
  expect_match(ui, rows, fixed = TRUE)
})

test_that("the CFA tab shows the residual statistic beside a robust two-stage fit and records its DMAR call", {
  st <- fb_state(function(s) {
    s$items[1:40, "t2_cubes"] <- NA_real_
    s$items[31:70, "t7_sentence"] <- NA_real_
  })
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    fb_inputs(session, two_stage = TRUE)
    session$setInputs(fit_go = 1)
    run <- cfa_run()
    expect_identical(run$spec$missing, "robust.two.stage")
    test <- run$two_stage_residual
    expect_null(test$error)
    expect_true(is.finite(test$value$chisq))
    shown <- fb_text(output$two_stage_test)
    expect_match(shown, "Savalei-Bentler Residual Test", fixed = TRUE)
    expect_match(shown, sprintf("%.3f", test$value$chisq), fixed = TRUE)
    expect_match(shown, "computed by DMAR's sem_fit_measures()", fixed = TRUE)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "two_stage_residual <- DMAR::sem_fit_measures(cfa_fit_ts)", fixed = TRUE)
    expect_false(grepl("semTools", log, fixed = TRUE))
  })
})

test_that("the SEM tab's residual statistic describes the fit on screen: the respondents it used and its estimates", {
  # An antecedent with missing values: the robust two-stage fit holds it
  # fixed (lavaan's default), so lavaan sets aside the 20 respondents missing
  # it; a refit with every variable random on all 301 would estimate from
  # other data (its estimates would differ by up to 1.01 and its statistic
  # would be 23.93).
  st <- fb_state(function(s) {
    r <- s$roles
    r$role[r$variable == "age_years"] <- "antecedent"
    s$roles <- r
    raw <- s$raw
    withr::with_seed(5, {
      s$items[sample(301, 60), "t2_cubes"] <- NA_real_
      s$items[sample(301, 50), "t7_sentence"] <- NA_real_
      raw[sample(301, 20), "age_years"] <- NA
    })
    s$raw <- raw
  })
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(outcomes = "Textual", predictors = c("Visual", "antecedent:age_years"),
      layout = "tree2", min_path = 0, two_stage = TRUE)
    session$setInputs(fit_go = 1L)
    run <- path_run()
    expect_identical(run$spec$missing, "robust.two.stage")
    expect_identical(as.integer(lavaan::lavInspect(run$fit, "nobs")), 281L)
    test <- run$two_stage_residual
    expect_null(test$error)
    # Independent: semTools 0.5-9's sem.2stage(se = "standard") and its anova
    # residual statistic on the 281 respondents with age observed (lavaan
    # 0.7-3), stored; the two first stages converge differently (relative
    # 2e-8 here).
    expect_equal(test$value$chisq, 24.6872281674, tolerance = 1e-6)
    expect_identical(test$value$df, 13)
    expect_identical(test$rows$set_aside, 20L)
    expect_identical(test$rows$covariates, "age_years")
    expect_identical(length(test$rows$used), 281L)
    expect_true(test$agreement$same)
    expect_identical(test$agreement$n_refit, 281L)
    expect_lt(test$agreement$max_rel, 1e-3)
    shown <- fb_text(output$two_stage_test)
    expect_match(shown, "on the 281 respondents the robust two-stage fit used", fixed = TRUE)
    expect_match(shown, "holds the observed covariate age_years fixed, so lavaan set aside the 20 respondents missing it", fixed = TRUE)
    expect_match(shown, "Its estimates are those of the robust two-stage fit", fixed = TRUE)
    # The recorded R takes the rows the recorded fit used and reproduces it.
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "sem_data_ts <- sem_data[sort(unlist(lavaan::lavInspect(sem_fit, \"case.idx\"))), , drop = FALSE]", fixed = TRUE)
    env <- new.env(parent = globalenv())
    env$items <- shiny::isolate(state$items)
    env$d <- shiny::isolate(state$raw)
    # (lavaan warns again that it set aside the 20 respondents.)
    suppressWarnings(eval(parse(text = c(sieve_sem_code(run$spec), test$code)), envir = env))
    expect_identical(fb_value(env$two_stage_residual, "chi_square"), test$value$chisq)
  })
})

test_that("the residual statistic claims the robust fit's estimates only when they agree", {
  meta <- data.frame(
    item = paste0("x", 1:9), label = paste0("x", 1:9),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L), facet = NA_character_,
    type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
  d <- fb_two_stage_data()
  spec <- sieve_two_stage_spec(sieve_cfa_spec(meta, reserved = names(d)), TRUE)
  args <- c(sieve_spec_args(spec), list(data = d))
  same <- sieve_two_stage_residual(spec, d, fit = do.call(lavaan::cfa, args))
  expect_true(same$agreement$same)
  expect_null(same$rows)
  expect_match(sieve_two_stage_residual_sentence(same), "Its estimates are those of the robust two-stage fit", fixed = TRUE)
  expect_true(any(grepl("its estimates are the robust fit's", same$code, fixed = TRUE)))
  # A fit with other estimates (x1 rescored): the statistic is shown, said
  # to describe the refit, and the claim is dropped from the sentence and
  # the recorded R.
  other <- d
  other$x1 <- 2 * other$x1
  args$data <- other
  differ <- sieve_two_stage_residual(spec, d, fit = do.call(lavaan::cfa, args))
  expect_false(differ$agreement$same)
  expect_true(is.finite(differ$value$chisq))
  expect_identical(differ$value$chisq, same$value$chisq)
  sentence <- sieve_two_stage_residual_sentence(differ)
  expect_match(sentence, "Its estimates differ from those of the robust two-stage fit by up to", fixed = TRUE)
  expect_match(sentence, "so the statistic describes the refit, not the fit on screen.", fixed = TRUE)
  expect_false(grepl("are those of the robust", sentence, fixed = TRUE))
  expect_false(any(grepl("its estimates are the robust fit's", differ$code, fixed = TRUE)))
})

# Factor retention evidence. The tab reports each criterion on its own,
# with the correlations it was actually computed from, and never chooses a number of factors. The
# checks below are independent of the code under test: quantiles are
# recomputed by hand from the eigenvalue draws psych returns, the criteria
# are recomputed directly with psych, Velicer's MAP is recomputed from its
# definition, and the recorded R is replayed in a clean R process.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("psych")
skip_if_not_installed("shiny")
withr::local_options(sieve.cores = 2L) # at most two workers
# The tests below read the content of the model fit by k as a first result.
# Under the run policy a first sequence runs by itself only at two seconds
# or less, which the default estimate for the nine tests at eight factors
# exceeds on a loaded machine; the estimate is held under it here, so the
# content is checked. The policy
# itself is tested in test-items-efa-dimensionality.R.
local_mocked_bindings(sieve_efa_plan_wait = function(...) 0.5)

# Two correlated factors (r = .30), four items each, n = 400. The number of
# factors is known because the data are generated from it.
ret_two_factor <- function(n = 400L) {
  withr::with_seed(20261012, {
    f1 <- stats::rnorm(n)
    f2 <- 0.3 * f1 + sqrt(1 - 0.09) * stats::rnorm(n)
    it <- function(f, l) l * f + sqrt(1 - l^2) * stats::rnorm(n)
    data.frame(
      a1 = it(f1, .80), a2 = it(f1, .75), a3 = it(f1, .70), a4 = it(f1, .65),
      b1 = it(f2, .80), b2 = it(f2, .75), b3 = it(f2, .70), b4 = it(f2, .65)
    )
  })
}

# Six of those items cut into five ordered categories, n = 300.
ret_ordinal <- function() {
  X <- ret_two_factor()[1:300, 1:6]
  as.data.frame(lapply(X, function(v) as.integer(cut(v, c(-Inf, -1, -0.2, 0.5, 1.3, Inf)))))
}

# A loaded, confirmed state around a data frame of items.
ret_state <- function(d, types = "continuous", map = NULL, theory_k = NULL) {
  map <- map %||% list(F = names(d))
  fixture <- list(data = d, item_cols = names(d), theory_map = map,
    theory_k = theory_k, theory_lbl = "theory", name = "retention test",
    labels = NULL, load_code = character())
  siever_test_study(fixture, labels = FALSE, theory = !is.null(theory_k),
    configure = function(s) {
      m <- s$meta
      m$type <- types
      s$meta <- m
    })
}

ret_text <- function(x) {
  if (is.null(x)) return("")
  paste(as.character(unlist(x)), collapse = " ")
}

# The type 7 sample quantile written out, so the reference percentile is
# checked against arithmetic and not against another call to quantile().
ret_q7 <- function(x, p) {
  x <- sort(x)
  h <- (length(x) - 1) * p + 1
  lo <- floor(h)
  hi <- ceiling(h)
  x[lo] + (h - lo) * (x[hi] - x[lo])
}

# Independent common-factor reconstruction of the complexity criterion,
# from DMAR::fa_exploratory() called directly (maximum likelihood on R,
# direct oblimin from the seed, none at one factor), the engine of VSS
# (`engine = "psych"` gives the reconstruction from psych::fa, which agrees
# within the optimizers' tolerance).
ret_vss <- function(R, max_k, n, seed = NULL, engine = "DMAR") {
  by_k <- lapply(seq_len(max_k), function(k) {
    if (identical(engine, "DMAR")) {
      fit <- DMAR::fa_exploratory(S = R, N = n, nfactors = k, estimator = "ML",
        rotation = if (k == 1L) "none" else "oblimin", conf_level = 0.95, seed = seed)
      sol <- sieve_efa_dmar_solution(fit)
      loadings <- sol$loadings
      phi <- sol$Phi
    } else {
      fit <- suppressWarnings(suppressMessages(psych::fa(R, k, fm = "ml", n.obs = n,
        rotate = if (k == 1L) "none" else "oblimin", scores = "none", warnings = FALSE)))
      loadings <- unclass(fit$loadings)
      phi <- fit$Phi %||% diag(k)
    }
    vapply(1:2, function(c) {
      if (k < c) return(NA_real_)
      simplified <- loadings
      for (j in seq_len(nrow(simplified))) {
        selected <- rank(-abs(loadings[j, ]), ties.method = "first") <= c
        simplified[j, !selected] <- 0
      }
      predicted <- simplified %*% phi %*% t(simplified)
      1 - sum((R - predicted)^2) / sum(R^2)
    }, numeric(1))
  })
  values <- do.call(rbind, by_k)
  list(cfit.1 = values[, 1L], cfit.2 = values[, 2L])
}

# The first browser test of an R session is the one that starts Chrome, and
# on a machine busy with other jobs that first start does not always open
# its debugging port; a later start in the same session then succeeds.
# shinytest2 skips a test whose Chrome did not start, which is not a pass,
# so Chrome is started here first, with up to three attempts.
ret_chrome_ready <- function(attempts = 3L) {
  for (i in seq_len(attempts)) {
    started <- tryCatch(
      {
        tab <- chromote::default_chromote_object()$new_session()
        tab$close()
        TRUE
      },
      error = function(e) FALSE
    )
    if (started) {
      return(TRUE)
    }
  }
  FALSE
}

# A criteria table built by hand, for the summary logic: three families,
# one of them (Very Simple Structure) with two criteria. (The model fit by k
# is not a criterion, so its family is not in the table; MAP is the second
# single-criterion family.)
ret_criteria <- function(pa, map, vss1, vss2, reason = NA_character_) {
  k <- c(pa, map, vss1, vss2)
  data.frame(
    id = c("pa", "map", "vss1", "vss2"),
    criterion = c("PA", "MAP", "VSS 1", "VSS 2"),
    family = c("Parallel analysis", "MAP", "VSS", "VSS"),
    suggested = as.integer(k),
    basis_requested = "pearson", basis_used = "pearson", basis_changed = FALSE,
    status = ifelse(is.na(k), "failed", "ran"),
    reason = ifelse(is.na(k), reason, NA_character_),
    stringsAsFactors = FALSE
  )
}

# Very Simple Structure runs DMAR::fa_exploratory() at each k. Its estimate
# on the small pools here can be above the two seconds at which a first result runs by
# itself; this file reads VSS results, not its wait (test-efa-layout.R
# and test-dmar-items-efa.R test the wait), so the estimate is pinned.
local_mocked_bindings(sieve_vss_wait = function(max_k, p, R = NULL) 1)

test_that("the requested correlations follow the confirmed item types", {
  expect_identical(sieve_retention_request("auto", rep("continuous", 4)), "pearson")
  expect_identical(sieve_retention_request("auto", rep("ordered", 4)), "polychoric")
  expect_identical(sieve_retention_request("auto", c("ordered", "binary")), "polychoric")
  expect_identical(sieve_retention_request("auto", rep("binary", 4)), "tetrachoric")
  expect_identical(sieve_retention_request("auto", c("continuous", "ordered")), "mixed")
  expect_identical(sieve_retention_request("auto", c("continuous", "binary")), "mixed")
  # An explicit request overrides the types.
  expect_identical(sieve_retention_request("pearson", rep("ordered", 4)), "pearson")
  expect_identical(sieve_retention_request("poly", rep("continuous", 4)), "polychoric")
  expect_identical(sieve_retention_request("poly", rep("binary", 4)), "tetrachoric")
  expect_identical(sieve_retention_basis_label(c("pearson", "mixed", NA)), c("Pearson", "mixed", "none"))
})

test_that("the parallel analysis reference is the 95th percentile of the eigenvalues DMAR simulated", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  # (The engine is DMAR::parallel_analysis(), one call per kind of reference
  # data from the recorded seed; the independent computation is DMAR's own
  # call made here and the quantile written out by hand.)
  X <- ret_two_factor()
  p <- ncol(X)
  ev <- sieve_retention_evidence(X, rep("continuous", p), n_iter = 40, max_k = 4, seed = 20261012)
  expect_true(ev$pa$ok)
  expect_identical(ev$pa$path, "data")
  expect_identical(ev$pa$rng$seed, 20261012L)
  expect_true(ev$pa$rng$supplied)

  normal <- DMAR::parallel_analysis(X, missing = "listwise", reference = "normal", B = 40,
    percentile = 0.95, conf_level = 0.95, seed = 20261012)
  resample <- DMAR::parallel_analysis(X, missing = "listwise", reference = "resample", B = 40,
    percentile = 0.95, conf_level = 0.95, seed = 20261012)
  sim <- attr(normal, "reference_draws")
  res <- attr(resample, "reference_draws")
  expect_identical(dim(sim), c(40L, p))
  expect_equal(ev$pa$reference$simulated$draws, unname(sim), tolerance = 1e-14)
  expect_equal(ev$pa$reference$resampled$draws, unname(res), tolerance = 1e-14)

  # The reference is the 95th percentile of each column, computed by hand.
  by_hand <- apply(sim, 2, ret_q7, p = 0.95)
  expect_equal(ev$pa$reference$simulated$p95, unname(by_hand), tolerance = 1e-12)
  expect_equal(ev$pa$reference$simulated$mean, unname(colMeans(sim)), tolerance = 1e-14)
  expect_equal(ev$pa$reference$resampled$p95, unname(apply(res, 2, ret_q7, p = 0.95)), tolerance = 1e-12)
  expect_equal(ev$pa$reference$resampled$mean, unname(colMeans(res)), tolerance = 1e-14)
  expect_equal(ev$pa$empirical, normal$estimate, tolerance = 1e-14)
  # DMAR's Monte Carlo limits of the percentile are kept beside it.
  expect_equal(ev$pa$reference$simulated$mc_lower, normal$reference_quantile_ci_lower, tolerance = 1e-14)
  # A percentile is not a mean.
  expect_false(isTRUE(all.equal(ev$pa$reference$simulated$p95, ev$pa$reference$simulated$mean)))

  # The count is the sequential rule applied to that reference, and it
  # recovers the two factors the data were generated from.
  sel <- sieve_pa_select(ev$pa, "p95", "auto", categorical = FALSE)
  expect_identical(sel$family_used, "simulated")
  expect_equal(sel$values, unname(by_hand), tolerance = 1e-12)
  above <- normal$estimate > by_hand
  expect_identical(sel$suggested, as.integer(which(!above)[1] - 1L))
  expect_identical(sel$suggested, 2L)
  expect_identical(sel$suggested, attr(normal, "suggested_factors"))
  expect_identical(sel$code, "quantile_95")
  expect_identical(sieve_pa_select(ev$pa, "mean", "simulated")$values, unname(normal$reference_mean))
  expect_identical(sieve_pa_select(ev$pa, "mean", "resampled")$code, "fa.simr")
  expect_identical(sieve_pa_select(ev$pa, "p95", "resampled")$code, "resampled_quantile_95")
})

test_that("the sequential count stops at the first failure, allows zero, and admits an undefined comparison", {
  expect_identical(
    sieve_pa_count(
      c(
        3,
        2,
        0.1,
        0.5
      ),
      c(
        1,
        1,
        0.2,
        0.1
      )
    ),
    2L
  ) # not 3: the later exceedance does not count
  expect_identical(sieve_pa_count(c(0.5, 2), c(1, 1)), 0L) # no factor is a legitimate answer
  expect_identical(sieve_pa_count(c(3, 2), c(1, 1)), 2L)
  expect_identical(sieve_pa_count(c(3, 2, 1), c(1, 1)), 2L) # only the ranks both series have
  expect_identical(sieve_pa_count(c(3, NA, 1), c(1, 1, 1)), NA_integer_)
  expect_identical(sieve_pa_count(c(0.5, NA), c(1, 1)), 0L) # stopped before the undefined rank
  expect_identical(sieve_pa_count(numeric(0), numeric(0)), NA_integer_)
})

test_that("with a known factor structure the criterion rows match independent factor analyses", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  X <- ret_two_factor()
  p <- ncol(X)
  n <- nrow(X)
  ev <- sieve_retention_evidence(X, rep("continuous", p), n_iter = 40, max_k = 4, seed = 113)
  cr <- sieve_retention_criteria(ev$basis, ev$pa, ev$vss, "p95", "auto", ev$categorical)

  # The model fit by k is not a criterion: it gives no count.
  expect_identical(cr$id, c("pa", "vss1", "vss2"))
  expect_identical(cr$family, c("Parallel analysis", "VSS", "VSS"))
  expect_true(all(cr$status == "ran"))
  expect_true(all(cr$basis_requested == "pearson" & cr$basis_used == "pearson" & !cr$basis_changed))
  expect_true(all(is.na(cr$reason)))
  # The random number record: the stochastic criteria carry the seed, the
  # deterministic ones do not.
  expect_identical(cr$stochastic, c(TRUE, TRUE, TRUE))
  expect_identical(cr$seed, c(113L, 113L, 113L))

  R <- stats::cor(X)
  expect_equal(ev$basis$R, R, tolerance = 1e-14)

  # VSS follows the common-factor law, including oblique factor correlations.
  # (The solutions are DMAR::fa_exploratory()'s, so the reference is DMAR
  # called directly with the seed; psych::fa's reconstruction agrees within
  # the optimizers' tolerance.)
  v <- ret_vss(R, 4L, n, seed = 113L)
  expect_null(ev$vss$map)
  expect_equal(ev$vss$vss1, as.numeric(v$cfit.1), tolerance = 1e-13)
  expect_equal(ev$vss$vss2, as.numeric(v$cfit.2), tolerance = 1e-13)
  vp <- withr::with_seed(113, ret_vss(R, 4L, n, engine = "psych"))
  # psych agrees through k = 3 (to 3e-5); at k = 4, two factors beyond the
  # structure, the overfactored solutions settle at different rotation
  # optima (DMAR's 30 random starts against psych's one), 0.018 apart.
  expect_lt(max(abs(ev$vss$vss1[1:3] - as.numeric(vp$cfit.1)[1:3]), na.rm = TRUE), 1e-4)
  expect_identical(cr$suggested[cr$id == "vss1"], as.integer(which.max(v$cfit.1)))
  expect_identical(cr$suggested[cr$id == "vss2"], as.integer(which.max(v$cfit.2)))

  # The fit sequence comes from the public lavaan EFA route, with the RMSEA
  # interval at the RMSEA level (90%, DMAR's default for it and lavaan's;
  # lavaan is asked for that level here).
  expect_identical(ev$fit$conf_level, 0.90)
  suffix <- if (ev$fit$family == "standard") "" else paste0(".", ev$fit$family)
  test_suffix <- if (ev$fit$test_family == "scaled") ".scaled" else ""
  direct_fit <- do.call(rbind, lapply(1:4, function(k) {
    f <- suppressWarnings(lavaan::efa(data = X, nfactors = k, rotation = "none",
      estimator = ev$fit$estimator, missing = "listwise"))
    if (inherits(f, "efaList")) f <- f[[1L]]
    m <- suppressWarnings(lavaan::fitMeasures(f, c(paste0(c("chisq", "df"), test_suffix),
      paste0(c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "cfi", "tli"), suffix)),
      fm.args = list(rmsea.ci.level = ev$fit$conf_level)))
    data.frame(k = k, chisq = m[1L], df = m[2L], rmsea = m[3L], lo = m[4L],
      hi = m[5L], cfi = m[6L], tli = m[7L])
  }))
  expect_equal(ev$fit$table$chisq, direct_fit$chisq, tolerance = 1e-12)
  expect_equal(ev$fit$table$df, ((p - 1:4)^2 - (p + 1:4)) / 2)
  expect_equal(ev$fit$table$rmsea, direct_fit$rmsea, tolerance = 1e-12)
  expect_equal(ev$fit$table$rmsea_lo, direct_fit$lo, tolerance = 1e-12)
  expect_equal(ev$fit$table$rmsea_hi, direct_fit$hi, tolerance = 1e-12)
  expect_equal(ev$fit$table$cfi, direct_fit$cfi, tolerance = 1e-12)
  expect_equal(ev$fit$table$tli, direct_fit$tli, tolerance = 1e-12)
  # Rotation preserves model fit.
  rotated <- suppressWarnings(lavaan::efa(data = X, nfactors = 2L, rotation = "varimax",
    estimator = ev$fit$estimator, missing = "listwise"))
  if (inherits(rotated, "efaList")) rotated <- rotated[[1L]]
  rotated_measures <- suppressWarnings(lavaan::fitMeasures(rotated,
    c(paste0("chisq", test_suffix), paste0(c("cfi", "tli"), suffix))))
  expect_equal(unname(rotated_measures),
    c(direct_fit$chisq[2L], direct_fit$cfi[2L], direct_fit$tli[2L]), tolerance = 1e-8)

  # The known generating structure does not force agreement among criteria.
  expect_identical(cr$suggested, c(2L, 2L, 4L))
  sm <- sieve_retention_summary(cr, theory_k = 2)
  expect_false(sm$agree)
  expect_identical(sm$range, c(2L, 4L))
  # Parallel analysis suggests 2 and Very Simple Structure is divided (2
  # and 4), so 2 is behind one family and 4 behind none.
  expect_identical(sm$modal, 2L)
  expect_identical(sm$n_families, 2L)
  expect_identical(sm$by_k$n_families, c(1L, 0L))
  expect_true(sm$theory$in_suggested)
})

test_that("a seeded run replays, an unseeded run records its seed, and the caller's stream is left alone", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  X <- ret_two_factor()[1:200, 1:6]
  ty <- rep("continuous", 6)
  set.seed(77)
  before <- get(".Random.seed", envir = globalenv())
  a <- sieve_retention_evidence(X, ty, n_iter = 20, max_k = 3, seed = 5)
  # A supplied seed leaves the caller's stream exactly as it was, although
  # both the simulation and psych's rotation starts drew random numbers.
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  b <- sieve_retention_evidence(X, ty, n_iter = 20, max_k = 3, seed = 5)
  expect_identical(a$pa$reference$simulated$draws, b$pa$reference$simulated$draws)
  expect_identical(a$pa$reference$resampled$draws, b$pa$reference$resampled$draws)
  expect_identical(a$vss$vss1, b$vss$vss1)
  expect_identical(a$vss$vss2, b$vss$vss2)
  other <- sieve_retention_evidence(X, ty, n_iter = 20, max_k = 3, seed = 6)
  expect_false(identical(a$pa$reference$simulated$draws, other$pa$reference$simulated$draws))
  expect_identical(a$rng$kind, RNGkind())

  # The session's mc.cores option changes nothing and is left as it was.
  old <- options(mc.cores = 2L)
  on.exit(options(old), add = TRUE)
  two <- sieve_retention_evidence(X, ty, n_iter = 20, max_k = 3, seed = 5)
  expect_identical(getOption("mc.cores"), 2L)
  expect_identical(two$pa$reference$simulated$draws, a$pa$reference$simulated$draws)
  options(old)

  # No seed: one is drawn from the caller's stream (exactly one draw),
  # recorded as drawn, and replays the run.
  set.seed(77)
  u <- sieve_retention_evidence(X, ty, n_iter = 20, max_k = 3)
  after <- get(".Random.seed", envir = globalenv())
  set.seed(77)
  drawn <- sample.int(.Machine$integer.max, 1L)
  expect_identical(get(".Random.seed", envir = globalenv()), after)
  expect_identical(u$rng$seed, drawn)
  expect_false(u$rng$supplied)
  expect_identical(u$pa$rng, u$rng)
  expect_identical(u$vss$rng, u$rng)
  replay <- sieve_retention_evidence(X, ty, n_iter = 20, max_k = 3, seed = u$rng$seed)
  expect_identical(replay$pa$reference$resampled$draws, u$pa$reference$resampled$draws)
  expect_identical(replay$vss$vss2, u$vss$vss2)
  u2 <- sieve_retention_evidence(X, ty, n_iter = 20, max_k = 3)
  expect_false(identical(u2$rng$seed, u$rng$seed))
  expect_false(identical(u2$pa$reference$simulated$draws, u$pa$reference$simulated$draws))
})

test_that("ordered items use polychoric correlations and a reference built from the items themselves", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  O <- ret_ordinal()
  ev <- sieve_retention_evidence(O, rep("ordered", 6), n_iter = 20, max_k = 3, seed = 113)
  expect_identical(ev$basis$requested, "polychoric")
  expect_identical(ev$basis$used, "polychoric")
  expect_false(ev$basis$changed)
  expect_null(ev$basis$reason)
  direct <- suppressWarnings(suppressMessages(psych::polychoric(O, correct = 0.5, progress = FALSE)$rho))
  expect_equal(unname(ev$basis$R), unname(direct), tolerance = 1e-12)
  # Polychoric and Pearson correlations of these items differ, so the basis
  # is not a label: it changes the matrix every criterion is computed from.
  expect_gt(max(abs(ev$basis$R - stats::cor(O))), 0.02)

  expect_identical(ev$pa$path, "data")
  expect_true(ev$pa$reference$resampled$available)
  sel <- sieve_pa_select(ev$pa, "p95", "auto", categorical = TRUE)
  expect_identical(sel$family_requested, "resampled")
  expect_identical(sel$family_used, "resampled")
  expect_false(sel$family_changed)
  expect_match(sel$label, "resampled independently (polychoric correlations)", fixed = TRUE)
  # (DMAR's normal reference for ordered items is categorized at each item's
  # observed category proportions and analyzed with polychoric correlations,
  # as the items are, so it carries no Pearson caution; the caution stays for
  # the mixed basis, below, whose matrix DMAR can only take as S.) It is a
  # different series from the resampled one.
  sim <- sieve_pa_select(ev$pa, "p95", "simulated", categorical = TRUE)
  expect_match(sim$label, "categorized at each item's observed category proportions (polychoric correlations)", fixed = TRUE)
  expect_false(isTRUE(all.equal(sim$values, sel$values)))
  expect_null(sel$caution)
  expect_null(sim$caution)
  sim_rows <- sieve_retention_criteria(ev$basis, ev$pa, ev$vss, "p95", "simulated", TRUE)
  expect_false(grepl("can suggest too many factors", sim_rows$reason[sim_rows$id == "pa"], fixed = TRUE))
  # For continuous items analyzed with Pearson correlations there is no mismatch.
  cont <- sieve_retention_evidence(ret_two_factor()[1:150, 1:6], rep("continuous", 6),
    n_iter = 10, max_k = 2, seed = 1
  )
  expect_null(sieve_pa_select(cont$pa, "p95", "simulated")$caution)

  cr <- sieve_retention_criteria(ev$basis, ev$pa, ev$vss, "p95", "auto", TRUE)
  expect_true(all(cr$basis_used == "polychoric" & !cr$basis_changed))
  expect_identical(cr$suggested[cr$id == "pa"], 2L)
  # The fit by k of ordered items is WLSMV's, beside the criteria.
  expect_identical(ev$fit$estimator, "WLSMV")
  code <- c(sieve_retention_cor_code(ev$basis), sieve_pa_code(ev$pa, sel))
  expect_true(any(grepl("psych::polychoric(", code, fixed = TRUE)))
  expect_true(any(grepl("DMAR::parallel_analysis(", code, fixed = TRUE)))
  expect_true(any(grepl("ordered = TRUE,", code, fixed = TRUE)))
  expect_true(any(grepl('reference = "resample",', code, fixed = TRUE)))
  expect_true(any(grepl("reference <- pa_resample$reference_quantile", code, fixed = TRUE)))

  # A sparse category that does not stop psych: four category items in
  # which one response carries a fifth code. The correlations are computed,
  # and what the engine reported about the empty cells is kept.
  S <- as.data.frame(lapply(
    ret_two_factor()[1:300, 1:6],
    function(v) as.integer(cut(v, c(-Inf, -1, 0, 1, Inf)))
  ))
  S$a1[which.max(ret_two_factor()$a1[1:300])] <- 5L
  expect_identical(unname(table(S$a1)[["5"]]), 1L)
  sparse <- sieve_retention_cor(S, rep("ordered", 6), "polychoric")
  expect_identical(sparse$used, "polychoric")
  expect_false(sparse$changed)
  expect_true(any(grepl("adjusted for 0 values", sparse$warnings, fixed = TRUE)))

  # Binary items: tetrachoric.
  B <- as.data.frame(lapply(ret_two_factor()[1:300, 1:6], function(v) as.integer(v > 0.3)))
  tet <- sieve_retention_cor(B, rep("binary", 6), sieve_retention_request("auto", rep("binary", 6)))
  expect_identical(tet$used, "tetrachoric")
  expect_equal(unname(tet$R),
    unname(suppressWarnings(suppressMessages(psych::tetrachoric(B, correct = 0.5)$rho))),
    tolerance = 1e-12
  )
})

test_that("when the polychoric step fails the result says requested polychoric, used Pearson, and why", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  # Five category items in which a single response carries a sixth code: a
  # sparse category. psych's polychoric step stops on these data.
  O <- ret_ordinal()
  O$a1[which.max(O$a1)] <- 6L
  expect_identical(unname(table(O$a1)[["6"]]), 1L)
  # The direct call is made on one worker, as the function makes it. psych
  # computes the pairs inside parallel::mcmapply(); with forked workers it
  # stops with a different message ("supply both 'x' and 'y' or a matrix-like
  # 'x'") than with one ("missing value where TRUE/FALSE needed"), and a
  # notice raised in a forked worker is lost. The comparison is against
  # psych's own message from a direct call; it has to be the call that is
  # made.
  engine_says <- withr::with_options(list(mc.cores = 1L), tryCatch(
    suppressWarnings(suppressMessages(psych::polychoric(O, correct = 0.5, progress = FALSE))),
    error = function(e) conditionMessage(e)
  ))
  expect_type(engine_says, "character")

  ev <- sieve_retention_evidence(O, rep("ordered", 6), n_iter = 20, max_k = 3, seed = 113)
  expect_identical(ev$basis$requested, "polychoric")
  expect_identical(ev$basis$used, "pearson")
  expect_true(ev$basis$changed)
  expect_true(ev$basis$ok)
  # The reason is the engine's own message, kept verbatim.
  expect_match(ev$basis$reason, "Polychoric correlations could not be computed", fixed = TRUE)
  expect_match(ev$basis$reason, engine_says, fixed = TRUE)
  # What was used really is the Pearson matrix, and every criterion was
  # computed from it.
  R <- stats::cor(O)
  expect_equal(ev$basis$R, R, tolerance = 1e-14)
  # The fixed one-factor reduction of the Pearson matrix (stats::factanal,
  # DMAR's reduction).
  fa1 <- stats::factanal(covmat = R, factors = 1, rotation = "none")
  reduced <- R
  diag(reduced) <- 1 - fa1$uniquenesses
  expect_equal(ev$pa$empirical, eigen(reduced, symmetric = TRUE, only.values = TRUE)$values, tolerance = 1e-10)
  expect_identical(ev$pa$basis_requested, "polychoric")
  expect_identical(ev$pa$basis_used, "pearson")
  expect_false("map" %in% names(ev$vss))

  cr <- sieve_retention_criteria(ev$basis, ev$pa, ev$vss, "p95", "auto", TRUE)
  from_basis <- cr
  expect_true(all(from_basis$basis_requested == "polychoric"))
  expect_true(all(from_basis$basis_used == "pearson"))
  expect_true(all(from_basis$basis_changed))
  expect_true(all(grepl(engine_says, from_basis$reason, fixed = TRUE)))
  expect_true(all(grepl("Pearson correlation", from_basis$produced_by, fixed = TRUE)))
  lines <- sieve_retention_evidence_lines(from_basis)
  expect_true(all(grepl("Pearson correlations, polychoric requested", lines, fixed = TRUE)))
  # The fit by k reads the item data under WLSMV, not the basis.
  expect_identical(ev$fit$basis_used, "polychoric")
  expect_identical(ev$fit$estimator, "WLSMV")

  # The recorded R is the call that ran, and says what was requested.
  code <- c(
    sieve_retention_cor_code(ev$basis),
    sieve_pa_code(ev$pa, sieve_pa_select(ev$pa, "p95", "auto", TRUE))
  )
  joined <- paste(code, collapse = "\n")
  unwrapped <- gsub("\n# +", " ", joined) # comment lines are wrapped
  expect_match(joined, "R_items <- stats::cor(", fixed = TRUE)
  expect_match(joined, 'missing = "listwise",', fixed = TRUE)
  expect_false(grepl("ordered = TRUE", joined, fixed = TRUE))
  expect_false(grepl("psych::polychoric(", joined, fixed = TRUE))
  expect_match(joined, "# Requested:  polychoric correlations.", fixed = TRUE)
  expect_match(joined, "# Used:       Pearson correlations instead. This changes the analysis.", fixed = TRUE)
  expect_match(unwrapped, engine_says, fixed = TRUE)
  # The reason is written as comments, so the script still parses and runs.
  expect_true(all(startsWith(sieve_retention_cor_code(ev$basis)[4:6], "#")))
  expect_silent(parse(text = code))
  env <- new.env(parent = globalenv())
  env$active_items <- O
  grDevices::pdf(NULL) # the code draws the parallel analysis figure; no file is written
  utils::capture.output(suppressWarnings(suppressMessages(eval(parse(text = code), envir = env))))
  grDevices::dev.off()
  expect_equal(env$R_items, R, tolerance = 1e-14)
  expect_equal(unname(env$reference), sieve_pa_select(ev$pa, "p95", "auto", TRUE)$values, tolerance = 1e-12)

  # A code outside the range psych accepts fails the same way, with psych's
  # own explanation.
  O10 <- ret_ordinal()
  O10$a1[which.max(O10$a1)] <- 10L
  wide <- sieve_retention_cor(O10, rep("ordered", 6), "polychoric")
  expect_identical(wide$used, "pearson")
  expect_match(wide$reason, "polychoric is probably not needed", fixed = TRUE)
})

test_that("a pool of continuous and ordered items uses mixed correlations from the confirmed types", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  M <- ret_two_factor()[1:300, 1:6]
  M[4:6] <- lapply(M[4:6], function(v) as.integer(cut(v, c(-Inf, -1, -0.2, 0.5, 1.3, Inf))))
  # A continuous item recorded coarsely: five distinct values. psych's own
  # detection would treat it as ordinal; the confirmed type says continuous.
  M$a3 <- pmax(-2, pmin(2, round(M$a3)))
  ty <- c("continuous", "continuous", "continuous", "ordered", "ordered", "ordered")
  ev <- sieve_retention_evidence(M, ty, n_iter = 20, max_k = 3, seed = 113)
  expect_identical(ev$basis$requested, "mixed")
  expect_identical(ev$basis$used, "mixed")
  direct <- suppressWarnings(suppressMessages({
    utils::capture.output(out <- psych::mixedCor(M,
      c = c("a1", "a2", "a3"), p = c("a4", "b1", "b2"),
      d = NULL, correct = 0.5
    )$rho)
    out[names(M), names(M)]
  }))
  expect_equal(unname(ev$basis$R), unname(direct), tolerance = 1e-12)
  # Among the confirmed continuous items the correlations are Pearson's,
  # including the coarsely recorded one.
  expect_equal(ev$basis$R[c("a1", "a2", "a3"), c("a1", "a2", "a3")],
    stats::cor(M[c("a1", "a2", "a3")]),
    tolerance = 1e-10
  )
  auto <- suppressWarnings(suppressMessages({
    utils::capture.output(out <- psych::mixedCor(M)$rho)
    out[names(M), names(M)]
  }))
  expect_gt(abs(auto["a1", "a3"] - ev$basis$R["a1", "a3"]), 0.01)

  # DMAR is given the matrix (S with N), so only the normal reference
  # exists, and the request for a resampled one is answered with a reason.
  expect_identical(ev$pa$path, "matrix")
  expect_true(ev$pa$reference$simulated$available)
  expect_false(ev$pa$reference$resampled$available)
  sel <- sieve_pa_select(ev$pa, "p95", "auto", categorical = TRUE)
  expect_identical(sel$family_requested, "resampled")
  expect_identical(sel$family_used, "simulated")
  expect_true(sel$family_changed)
  expect_match(sel$reason, "reads ordered items only when every item is ordered", fixed = TRUE)
  expect_match(sel$caution, "can suggest too many factors", fixed = TRUE)
  cr <- sieve_retention_criteria(ev$basis, ev$pa, ev$vss, "p95", "auto", TRUE)
  expect_identical(cr$status[cr$id == "pa"], "ran")
  expect_match(cr$reason[cr$id == "pa"], "reads ordered items only when every item is ordered", fixed = TRUE)
  expect_match(cr$reason[cr$id == "pa"], "can suggest too many factors", fixed = TRUE)

  # The recorded R passes the confirmed types and replays the reference.
  code <- c(sieve_retention_cor_code(ev$basis), sieve_pa_code(ev$pa, sel))
  expect_true(any(grepl('c = c("a1", "a2", "a3"),', code, fixed = TRUE)))
  expect_true(any(grepl('p = c("a4", "b1", "b2"),', code, fixed = TRUE)))
  env <- new.env(parent = globalenv())
  env$active_items <- M
  grDevices::pdf(NULL) # the code draws the parallel analysis figure; no file is written
  utils::capture.output(suppressWarnings(suppressMessages(eval(parse(text = code), envir = env))))
  grDevices::dev.off()
  expect_equal(unname(env$R_items), unname(ev$basis$R), tolerance = 1e-12)
  expect_equal(unname(env$reference), sel$values, tolerance = 1e-12)
  expect_identical(as.integer(env$n_factors), sel$suggested)
})

test_that("the model fit by k reads no count from cutoffs, and a zero-df solution keeps its masks", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  # (No count is read from a cutoff rule such as the smallest k with RMSEA
  # at most .06 and CFI and TLI at least .95 after Hu and Bentler, 1999: no
  # value of an index is applied as a cutoff, and the model fit by k is not
  # a criterion. No function applies such a rule, and nothing reads a count
  # off the table.)
  expect_false(exists("sieve_fit_conventions", envir = asNamespace("SIEVER"), inherits = FALSE))
  expect_false(exists("sieve_fit_cutoff_count", envir = asNamespace("SIEVER"), inherits = FALSE))

  # With six ordered items the three-factor solution has zero degrees of
  # freedom and is improper. Its RMSEA and TLI are withheld, while valid
  # lower-k results remain. It is not counted as fitting, which a cutoff
  # rule would report as the smallest close fitting solution.
  O <- ret_ordinal()
  ev <- sieve_retention_evidence(O, rep("ordered", 6), n_iter = 20, max_k = 3, seed = 113)
  ft <- ev$fit$table
  expect_identical(ft$df, c(9, 4, 0))
  expect_equal(ft$df[3L], 0)
  expect_true(is.finite(ft$cfi[3L]))
  # The criteria hold no row for the fit by k.
  cr <- sieve_retention_criteria(ev$basis, ev$pa, ev$vss, "p95", "auto", TRUE)
  expect_false("fit" %in% cr$id)
  expect_false(any(grepl("conventional|Hu and Bentler|Sequential fit", unlist(cr), ignore.case = TRUE)))
  # A zero-df row cannot veto the family for the valid positive-df models.
  # Cached rows and both standalone scripts retain the same values and masks.
  solutions <- sieve_efa_solutions(O, ev$fit$estimator, ev$fit$ordered,
    rotation = "none", ks = 1:3, missing = ev$fit$missing)
  combined <- sieve_efa_table(solutions)
  cached <- sieve_efa_rows(lapply(solutions, function(s) sieve_efa_table(list(s))))
  columns <- c("CFI", "TLI", "RMSEA", "RMSEA.lo", "RMSEA.hi", "SRMR", "BIC", "df")
  expect_identical(attr(combined, "family"), "robust")
  expect_equal(cached[, columns], combined[, columns], tolerance = 1e-8)
  expect_true(all(is.na(combined[3L, c("TLI", "RMSEA", "RMSEA.lo", "RMSEA.hi")])))
  measures <- attr(combined, "measures")
  expect_true(is.na(sieve_common_family(measures)$family)) # generic comparisons remain strict
  expect_equal(combined$CFI[3L], measures[[3L]]$families$robust$values[["cfi"]])
  single <- sieve_efa_table(solutions[3L])
  expect_true(is.finite(single$CFI))
  expect_equal(single$df, 0)
  expect_true(is.na(single$RMSEA) && is.na(single$TLI))
  expect_match(attr(single, "family_reason"), "descriptive", fixed = TRUE)

  env <- new.env(parent = globalenv())
  env$active_items <- O
  fit_code <- sieve_fit_lavaan_code(ev$fit)
  # The recorded R reads no count and applies no cutoff.
  expect_false(any(grepl("fit_k|meets|0[.]06|0[.]95|Conventional", fit_code)))
  expect_match(fit_code[length(fit_code)], "fit_by_k", fixed = TRUE)
  expect_true(any(grepl("no number of factors is read from them", fit_code, fixed = TRUE)))
  utils::capture.output(suppressWarnings(eval(parse(text = fit_code), env)))
  replay_columns <- c("chisq", "df", "rmsea", "rmsea_lo", "rmsea_hi", "cfi", "tli", "srmr", "bic")
  expect_equal(env$fit_by_k[, replay_columns], ft[, replay_columns], tolerance = 1e-7)
  expect_false(exists("fit_k", envir = env, inherits = FALSE))
  key <- list(ks = 1:3, estimator = ev$fit$estimator, ordered = names(O),
    rotation = "none", lavaan_missing = ev$fit$lavaan_missing, label = ev$fit$label)
  utils::capture.output(suppressWarnings(eval(parse(text = sieve_efa_fit_code(key, ev$fit$family)), env)))
  expect_equal(unname(t(env$efa_fit_indices)),
    unname(as.matrix(ft[, c("cfi", "tli", "rmsea", "rmsea_lo", "rmsea_hi", "srmr", "bic")])),
    tolerance = 1e-7)
  unavailable <- ev$fit
  unavailable$family <- NA_character_
  utils::capture.output(suppressWarnings(eval(parse(text = sieve_fit_lavaan_code(unavailable)), env)))
  expect_true(all(is.na(env$fit_by_k[, c("cfi", "tli", "rmsea", "rmsea_lo", "rmsea_hi")])))

  # A model with negative degrees of freedom is not fit at all: with nine
  # items that is six factors and more.
  fx <- sieve_hs9_fixture()
  H <- fx$data[, fx$item_cols]
  hb <- sieve_retention_cor(H, rep("continuous", 9), "pearson")
  hf <- sieve_retention_fit(hb, max_k = 8, data = H,
    spec = sieve_lavaan_spec(data.frame(item = names(H), type = "continuous", status = "active")))
  expect_identical(hf$table$k, 1:5)
  expect_identical(hf$table$df, c(27, 19, 12, 6, 1))
  expect_identical(hf$not_identified, 6:8)
  expect_true(any(grepl("k = 6, 7, 8 (negative degrees of freedom", sieve_fit_code(hf), fixed = TRUE)))
  # The summary reads the criteria only. (VSS on these six ordered items may
  # be unavailable: DMAR refuses the improper solutions, and fewer than two
  # k are left to compare.)
  sm <- sieve_retention_summary(cr)
  expect_true(all(sm$unavailable %in% c("vss1", "vss2")))
  expect_false("fit" %in% unlist(strsplit(sm$by_k$criteria, ", ", fixed = TRUE)))
})

test_that("disagreement is reported, VSS counts once, and an unavailable criterion counts for nothing", {
  # PA 3, MAP 3, both VSS complexities 1. Counting VSS twice would tie 1
  # with 3; by family, 3 leads.
  cr <- ret_criteria(pa = 3, map = 3, vss1 = 1, vss2 = 1)
  sm <- sieve_retention_summary(cr, theory_k = 3)
  expect_false(sm$agree)
  expect_identical(sm$range, c(1L, 3L))
  expect_identical(sm$by_k$k, c(1L, 3L))
  expect_identical(sm$by_k$criteria, c("vss1, vss2", "pa, map"))
  expect_identical(sm$by_k$n_families, c(1L, 2L))
  expect_identical(sm$n_families, 3L)
  expect_identical(sm$modal, 3L)
  expect_identical(sm$neighborhood, c(2L, 4L))
  expect_identical(sm$theory, list(k = 3L, in_suggested = TRUE, in_neighborhood = TRUE))

  st <- sieve_retention_statements(cr, sm)
  expect_match(st$range, "The criteria disagree: the suggested number of factors ranges from 1 to 3.", fixed = TRUE)
  expect_match(st$range, "1 factor (VSS complexity 1 and VSS complexity 2)", fixed = TRUE)
  expect_match(st$range, "3 factors (parallel analysis and MAP)", fixed = TRUE)
  expect_match(
    st$tally,
    "^Heuristic tally, not a calibrated consensus: 3 factors is suggested by 2 of the 3 criterion families"
  )
  expect_match(st$tally, "counts once", fixed = TRUE)
  expect_match(st$tally, "no known error rate", fixed = TRUE)
  expect_match(st$neighborhood, "Heuristic range of solutions worth examining: 2 to 4 factors", fixed = TRUE)
  expect_match(st$neighborhood, "not a confidence interval", fixed = TRUE)
  expect_match(st$theory, "not an override", fixed = TRUE)
  expect_null(st$unavailable)
  # No sentence recommends, and none calls the tally a consensus estimate.
  all_text <- paste(unlist(st), collapse = " ")
  expect_false(grepl("recommend", all_text, ignore.case = TRUE))
  expect_false(grepl("triangulat", all_text, ignore.case = TRUE))

  # An unavailable criterion is named with its reason and supports nothing.
  # Here that leaves two families with one count each: no count leads.
  cr2 <- ret_criteria(pa = 3, map = NA, vss1 = 1, vss2 = 1, reason = "the engine said no")
  sm2 <- sieve_retention_summary(cr2, theory_k = 5)
  expect_identical(sm2$available, c("pa", "vss1", "vss2"))
  expect_identical(sm2$unavailable, "map")
  expect_identical(sm2$n_families, 2L)
  expect_identical(sm2$modal, c(1L, 3L))
  expect_identical(sm2$neighborhood, c(1L, 4L))
  expect_false(sm2$theory$in_suggested)
  expect_false(sm2$theory$in_neighborhood)
  st2 <- sieve_retention_statements(cr2, sm2)
  expect_match(st2$unavailable, "MAP (the engine said no)", fixed = TRUE)
  expect_match(st2$unavailable, "not counted as agreeing", fixed = TRUE)
  expect_match(st2$tally, "no single count leads", fixed = TRUE)
  expect_match(st2$theory, "No criterion suggests that count, and it lies outside", fixed = TRUE)

  # Full agreement is stated without overclaiming, and VSS still counts once.
  cr3 <- ret_criteria(2, 2, 2, 2)
  sm3 <- sieve_retention_summary(cr3)
  expect_true(sm3$agree)
  expect_identical(sm3$by_k$n_families, 3L)
  expect_null(sm3$theory)
  expect_match(sieve_retention_statements(cr3, sm3)$range, "weaker evidence", fixed = TRUE)

  # No factor at all is a possible finding and stays in the range.
  cr4 <- ret_criteria(pa = 0, map = 1, vss1 = NA, vss2 = NA)
  sm4 <- sieve_retention_summary(cr4)
  expect_identical(sm4$range, c(0L, 1L))
  expect_identical(sm4$modal, 0:1)
  expect_identical(sm4$neighborhood, c(0L, 2L))

  # Nothing available: nothing is tallied.
  cr5 <- ret_criteria(NA, NA, NA, NA, reason = "no matrix")
  sm5 <- sieve_retention_summary(cr5, theory_k = 2)
  expect_identical(sm5$modal, integer(0))
  st5 <- sieve_retention_statements(cr5, sm5)
  expect_identical(st5$range, "No criterion produced a count for the current item pool.")
  expect_null(st5$tally)
  expect_null(st5$neighborhood)
})

test_that("an item without variance stops every criterion with the reason, and nothing is substituted", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  C <- ret_two_factor()[1:100, 1:6]
  C$a1 <- 1
  printed <- utils::capture.output(
    ev <- sieve_retention_evidence(C, rep("continuous", 6), n_iter = 20, max_k = 3, seed = 1)
  )
  expect_false(ev$basis$ok)
  expect_true(is.na(ev$basis$used))
  # The reason names the item and its cause (not "an item without variance,
  # or too few cases", with no item named).
  expect_match(ev$basis$reason,
    "Pearson correlations could not be computed: Item 'a1' takes a single value in the 100 rows used, so its correlations are undefined.",
    fixed = TRUE
  )
  expect_false(ev$pa$ok)
  expect_false(ev$vss$ok)
  # The fit by k is refused by name before lavaan runs, which would print
  # its whole variable table at every k.
  expect_false(ev$fit$ok)
  expect_match(ev$fit$reason, "Item 'a1' takes the same value (1) for every case", fixed = TRUE)
  expect_false(any(grepl("nobs", printed, fixed = TRUE)))
  cr <- sieve_retention_criteria(ev$basis, ev$pa, ev$vss)
  expect_true(all(is.na(cr$suggested)))
  expect_true(all(cr$status == "failed"))
  expect_true(all(nzchar(cr$reason)))
  expect_true(all(grepl("could not be computed", cr$reason, fixed = TRUE)))
  expect_true(all(startsWith(sieve_pa_code(ev$pa, sieve_pa_select(ev$pa)), "#")))
})

test_that("the module shows requested and used correlations next to every criterion and records the call that ran", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  # The bundled tests are continuous scores. Declared ordered, polychoric
  # correlations are requested and psych cannot compute them; the tab then
  # uses Pearson, and its text and its recorded call must say so rather than
  # name polychoric.
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols, drop = FALSE]
  st <- ret_state(d, types = "ordered", map = fx$theory_map, theory_k = 3L)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 20, max_k = 4, seed = 2026
    )
    expect_identical(cor_kind(), "poly")
    b <- basis()
    expect_identical(b$requested, "polychoric")
    expect_identical(b$used, "pearson")
    cr <- criteria()
    # (The model fit by k is fit by lavaan::efa under its own Estimator
    # control, here the study's setting, WLSMV, so it works from the
    # correlations WLSMV uses and its note names the estimator rather than
    # the correlation basis. It is not a criterion and has no row.)
    expect_false("fit" %in% cr$id)
    expect_true(all(cr$basis_requested == "polychoric" & cr$basis_used == "pearson"))
    expect_identical(fit_run()$basis_used, "polychoric")
    expect_match(ret_text(output$fit_note), "lavaan::efa at each k under WLSMV (DWLS)", fixed = TRUE)

    says <- "Requested: polychoric correlations. Used: Pearson correlations."
    for (out in list(
      output$overview, output$scree_note, output$mapvss_note,
      output$summary_reco
    )) {
      expect_match(ret_text(out), says, fixed = TRUE)
      expect_match(ret_text(out), "polychoric is probably not needed", fixed = TRUE)
    }
    tbl <- ret_text(output$summary_tbl)
    expect_match(tbl, "Pearson (requested: polychoric)", fixed = TRUE)
    expect_match(tbl, "polychoric is probably not needed", fixed = TRUE)
    # The overview states what was computed, not what was asked for.
    ov <- ret_text(output$overview)
    expect_match(ov,
      "What was computed: Pearson product-moment correlations",
      fixed = TRUE
    )

    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "requested polychoric, used Pearson", fixed = TRUE)
    # (The parallel analysis is DMAR::parallel_analysis() on the Pearson
    # basis, its data route.)
    expect_match(log, 'missing = "listwise",', fixed = TRUE)
    expect_false(grepl("ordered = TRUE", log, fixed = TRUE))
    expect_match(log, "# Requested:  polychoric correlations.", fixed = TRUE)
  })

  # Genuinely ordered items: polychoric is used, and said to be. The first
  # parallel analysis runs by itself only when its estimate is two seconds
  # or less, which a loaded machine can exceed; hold the estimate, as the
  # file does for fit by k (the estimate is DMAR's units per reference
  # dataset).
  local_mocked_bindings(sieve_pa_units = function(...) 0.01)
  O <- ret_ordinal()
  st <- ret_state(O, types = "ordered", map = list(A = names(O)[1:4], B = names(O)[5:6]))
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 20, max_k = 3, seed = 2026
    )
    expect_identical(basis()$used, "polychoric")
    expect_match(ret_text(output$summary_reco), "Correlations: polychoric, as requested.", fixed = TRUE)
    expect_match(ret_text(output$overview), "What was computed: polychoric correlations", fixed = TRUE)
    expect_identical(pa()$sieve_reference, "resampled_quantile_95")
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "ordered = TRUE,", fixed = TRUE)
    expect_match(log, 'reference = "resample",', fixed = TRUE)
    # The zero-df three-factor solution is said to test nothing; no row is
    # graded against cutoffs.
    # Here the three-factor solution is also improper; the row says both.
    expect_match(ret_text(output$fit_tbl), "no positive degrees of freedom: the indices do not test fit", fixed = TRUE)
    expect_match(ret_text(output$fit_tbl), "The solution is improper: negative variance estimate for b1; no positive degrees", fixed = TRUE)
    expect_false(grepl("Against the conventions|meets all three", ret_text(output$fit_tbl)))
    expect_match(ret_text(output$fit_note),
      "At k = 3 the model has no positive degrees of freedom, so its indices do not test fit.",
      fixed = TRUE
    )
    # Asking for Pearson is an explicit request, not a fallback.
    session$setInputs(cor_type = "pearson")
    expect_identical(basis()$requested, "pearson")
    expect_false(basis()$changed)
    expect_match(ret_text(output$summary_reco), "Correlations: Pearson, as requested.", fixed = TRUE)
  })
})

test_that("the module presents evidence, labels its heuristics, names MAP as component based, and keeps display controls instant", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  X <- ret_two_factor()
  st <- ret_state(X, map = list(A = names(X)[1:4], B = names(X)[5:8]), theory_k = 2L)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 30, max_k = 4, seed = 113
    )
    raw_first <- pa_raw()
    vss_first <- vss_res()
    expect_identical(pa_run()$rng$seed, 113L)
    expect_identical(pa()$nfact, 2L)

    reco <- ret_text(output$summary_reco)
    expect_match(reco, "Heuristic tally, not a calibrated consensus", fixed = TRUE)
    expect_match(reco, "not a confidence interval", fixed = TRUE)
    expect_match(reco, "SIEVER does not choose the number of factors", fixed = TRUE)
    expect_false(grepl("recommend", reco, ignore.case = TRUE))
    tbl <- ret_text(output$summary_tbl)
    expect_false(grepl("MAP (Velicer", tbl, fixed = TRUE))
    expect_match(tbl, "Very Simple Structure", fixed = TRUE)
    expect_match(tbl, "reference, not a criterion", fixed = TRUE)
    expect_false(grepl("component-based", ret_text(output$mapvss_note), fixed = TRUE))
    expect_match(ret_text(output$overview), "factor-analytic", fixed = TRUE)
    # Every result says what produced it and from which seed.
    expect_match(ret_text(output$scree_note),
      "Computed by DMAR::parallel_analysis() on the Pearson correlations of the 400 complete cases (30 reference datasets for each kind of reference data, seed 113 (supplied)",
      fixed = TRUE
    )
    # (VSS's solutions are DMAR::fa_exploratory()'s.)
    expect_match(ret_text(output$mapvss_note), "DMAR::fa_exploratory ML factor loadings and their factor correlations on the Pearson correlation matrix (n = 400 complete cases)", fixed = TRUE)
    # (The model fit by k is lavaan::efa under the estimator in force, ML
    # with FIML for continuous items.)
    expect_match(ret_text(output$fit_note),
      "lavaan::efa at each k under ML with FIML, on the 8 included items (n = 400",
      fixed = TRUE
    )
    # The MAP table starts at step zero.
    expect_match(ret_text(output$mapvss_tbl), "Factors extracted", fixed = TRUE)
    expect_false(grepl("MAP (components)", ret_text(output$mapvss_tbl), fixed = TRUE))

    # Changing the reference value or the reference data reads the same
    # draws: the simulation and the rotation are not run again.
    session$setInputs(criterion = "mean")
    expect_identical(pa_raw(), raw_first)
    expect_identical(pa()$sieve_reference, "fa.sim")
    session$setInputs(pa_reference = "resampled")
    expect_identical(pa_raw(), raw_first)
    expect_identical(vss_res(), vss_first)
    expect_identical(pa()$sieve_reference, "fa.simr")
    expect_equal(pa()$sieve_ref_values, pa_run()$reference$resampled$mean, tolerance = 1e-14)
    draws_first <- pa_run()$reference$simulated$draws
    # A new seed is a new analysis.
    session$setInputs(seed = 114)
    expect_false(identical(pa_run()$reference$simulated$draws, draws_first))
  })

  ui <- as.character(mod_retention_ui("ret"))
  for (id in c("ret-seed", "ret-pa_reference", "ret-decide_k", "ret-decide_why", "ret-decide_go")) {
    expect_true(grepl(sprintf('id="%s"', id), ui, fixed = TRUE), info = id)
  }
  expect_match(ui, "The number of factors is your decision", fixed = TRUE)
  expect_false(grepl("recommend", ui, ignore.case = TRUE))
})

test_that("the recorded R replays the reference quantiles, factor-based VSS, and fit in a clean R process", {
  # Live and clean-process calls both use a production-valid reference count.
  skip_on_cran()
  # Incomplete rows are in the data on purpose: the recorded R must use the
  # complete cases the application used.
  X <- ret_two_factor()[1:250, ]
  X$a2[c(3, 40)] <- NA
  X$b3[77] <- NA
  st <- ret_state(X, map = list(A = names(X)[1:4], B = names(X)[5:8]))
  got <- new.env()
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 200, max_k = 3
    ) # no seed supplied
    run <- pa_run()
    expect_false(run$rng$supplied)
    expect_identical(nrow(x_data()), 250L)
    list2env(list(
      seed = run$rng$seed, reference = pa()$sieve_ref_values, values = run$reference$simulated$draws,
      n_factors = pa()$nfact,
      vss1 = vss_run()$vss1, vss2 = vss_run()$vss2,
      fit = fit_res(),
      log = shiny::isolate(state$codelog)
    ), envir = got)
  })
  log <- got$log
  # The drawn seed is in the recorded code, said to be drawn. (VSS's DMAR
  # calls set the seed locally, so the recorded R states it in a comment
  # rather than calling set.seed() on the session's stream.)
  expect_true(any(grepl(sprintf("seed = %d (no seed was supplied", got$seed), log, fixed = TRUE)))
  # (The parallel analysis is DMAR's calls with their seed, which need no
  # mc.cores setting.)
  expect_true(any(grepl(sprintf("pa_normal <- withr::with_seed(%d,", got$seed), log, fixed = TRUE)))
  expect_true(any(grepl("complete_rows <- stats::complete.cases(em_data[", log, fixed = TRUE)))

  # A fresh R process: no objects, no random number stream, default options.
  dir <- tempfile("replay_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  saveRDS(X, file.path(dir, "active_items.rds"))
  script <- file.path(dir, "replay.R")
  writeLines(c(
    sprintf(".libPaths(%s)", paste(deparse(compat_source_libs()), collapse = "")),
    sprintf("active_items <- readRDS(%s)", deparse(file.path(dir, "active_items.rds"))),
    "grDevices::pdf(NULL)",
    log,
    sprintf("saveRDS(list(reference = reference, values = unname(attr(pa_normal, 'reference_draws')), n_factors = n_factors,
                          vss1 = vss_out$cfit.1,
                          vss2 = vss_out$cfit.2, fit_by_k = fit_by_k,
                          had_stream = exists('.Random.seed', envir = globalenv())),
                     %s)", deparse(file.path(dir, "out.rds")))
  ), script)
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)),
    stdout = TRUE, stderr = TRUE
  ))
  expect_true(file.exists(file.path(dir, "out.rds")), info = paste(out, collapse = "\n"))
  re <- readRDS(file.path(dir, "out.rds"))

  expect_equal(unname(re$reference), got$reference, tolerance = 1e-13)
  expect_identical(re$values, got$values)
  expect_identical(as.integer(re$n_factors), as.integer(got$n_factors))
  expect_false(any(grepl("map_values|map_k|psych::vss", log)))
  expect_equal(as.numeric(re$vss1), got$vss1, tolerance = 1e-13)
  expect_equal(as.numeric(re$vss2), got$vss2, tolerance = 1e-13)
  expect_equal(re$fit_by_k$chisq, got$fit$chisq, tolerance = 1e-12)
  expect_equal(re$fit_by_k$rmsea, got$fit$rmsea, tolerance = 1e-12)
  expect_equal(re$fit_by_k$cfi, got$fit$cfi, tolerance = 1e-12)
  expect_equal(re$fit_by_k$tli, got$fit$tli, tolerance = 1e-12)
  # No count is read off the fit sequence, in the page or in its R.
  expect_false(any(grepl("fit_k", log, fixed = TRUE)))
})

test_that("the researcher records the retention decision and nothing else changes", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols, drop = FALSE]
  st <- ret_state(d, map = fx$theory_map, theory_k = 3L)
  snapshot <- function(state) {
    all <- shiny::isolate(shiny::reactiveValuesToList(state))
    all[setdiff(
      names(all),
      c(
        "decisions",
        "codelog",
        "replay_steps",
        "replay_states",
        "replay_data",
        "replay_operations",
        "replay_versions"
      )
    )]
  }
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 20, max_k = 4, seed = 7
    )
    before <- snapshot(state)
    before_steps <- shiny::isolate(state$replay_steps)
    exact_inputs <- sieve_replay_inputs(state)
    expect_decision_binding <- function(ids) {
      steps <- shiny::isolate(state$replay_steps)
      log <- shiny::isolate(state$codelog)
      # Recording a decision adds provenance for its comment block, bound
      # to the same exact inputs; it does not alter prior step bindings.
      expect_identical(steps[names(before_steps)], before_steps)
      expect_length(setdiff(names(steps), names(before_steps)), length(ids))
      for (id in ids) {
        stamp <- grep(paste0("^# \\[[0-9]+\\] Decision ", id, " "), log, value = TRUE)
        expect_length(stamp, 1L)
        step <- sub("^# \\[([0-9]+)\\].*", "\\1", stamp)
        key <- steps[[step]]
        compact <- shiny::isolate(state$replay_states[[key]])
        expect_identical(key, sieve_hash(compact))
        expect_identical(shiny::isolate(sieve_replay_expand(compact, state$replay_data)), exact_inputs)
      }
    }
    expect_identical(shiny::isolate(state$decisions), list())
    expect_match(ret_text(output$decide_status), "No factor retention decision has been recorded", fixed = TRUE)

    # A number that is not a possible count of factors is refused. (A number
    # without a rationale is recorded as "no rationale given", tested in
    # test-dimensionality.R.)
    for (bad in list(NA, 2.5, -1, 9)) {
      session$setInputs(
        decide_k = bad,
        decide_why = "because",
        decide_go = (shiny::isolate(input$decide_go) %||% 0) + 1
      )
      expect_identical(shiny::isolate(state$decisions), list())
    }
    expect_match(ret_text(output$decide_status), "a whole number from 0 to 8", fixed = TRUE)

    # A decision that differs from what most criteria suggest is recorded
    # as entered: the tab does not choose k.
    cr <- criteria()
    session$setInputs(
      decide_k = 2, decide_why = "MAP and the weak third factor.",
      decide_go = shiny::isolate(input$decide_go) + 1
    )
    recs <- shiny::isolate(state$decisions)
    expect_named(recs, "D1")
    r1 <- recs$D1
    expect_identical(r1$topic, "factor_retention")
    expect_identical(r1$choice, 2L)
    expect_identical(r1$rationale, "MAP and the weak third factor.")
    expect_identical(r1$revision, sieve_revision(state))
    expect_identical(r1$evidence, sieve_retention_evidence_lines(cr, 3L))
    # Parallel analysis, MAP and VSS, then the theorized count; no line is
    # read from fit-index cutoffs.
    expect_length(r1$evidence, 4L)
    expect_match(
      r1$evidence[1],
      "^parallel analysis: 3 \\[Pearson correlations; 95th percentile of 20 simulated normal datasets; seed 7\\]$"
    )
    expect_identical(r1$evidence[4], "theorized number of factors: 3")
    expect_match(ret_text(output$decide_status),
      "Recorded as D1: 2 factors retained. Nothing else was changed.",
      fixed = TRUE
    )
    # Only the record, code log, and its exact-input step binding moved.
    expect_identical(snapshot(state), before)
    expect_decision_binding("D1")
    expect_identical(shiny::isolate(state$theory$k), 3L)
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("Decision D1 (factor_retention): 2", log, fixed = TRUE)))
    expect_true(any(grepl("# Rationale:  MAP and the weak third factor.", log, fixed = TRUE)))

    # A changed mind is a second record that names the first.
    session$setInputs(
      decide_k = 3, decide_why = "Three, as theorized, after reading the loadings.",
      decide_go = shiny::isolate(input$decide_go) + 1
    )
    recs <- shiny::isolate(state$decisions)
    expect_identical(names(recs), c("D1", "D2"))
    expect_identical(recs$D2$supersedes, "D1")
    expect_identical(recs$D1, r1)
    expect_match(ret_text(output$decide_status), "Decision in force (D2): 3 factor(s) retained.", fixed = TRUE)
    shown <- ret_text(output$decide_tbl)
    expect_match(shown, "replaced by D2", fixed = TRUE)
    expect_identical(snapshot(state), before)
    expect_decision_binding(c("D1", "D2"))
    expect_false(grepl("have changed since this decision was recorded", ret_text(output$decide_status), fixed = TRUE))

    # The decision belongs to the item pool it was made on. When the pool
    # changes the record stays as written and the tab says it is stale; it
    # is not carried over to the new pool as if it had been made there.
    m <- shiny::isolate(state$meta)
    m$status[9] <- "parked"
    state$meta <- m
    session$flushReact()
    expect_match(ret_text(output$decide_status), "have changed since this decision was recorded", fixed = TRUE)
    expect_identical(shiny::isolate(state$decisions), recs)
  })
})

test_that("in the browser the evidence is readable and a decision is recorded and written to the script", {
  skip_if_no_browser()
  # Chrome gets a minute to start and to answer (chromote waits 10 seconds
  # by default), and up to three attempts to start.
  withr::local_options(list(chromote.timeout = 60))
  skip_if_not(ret_chrome_ready(), "Chrome did not start in three attempts")

  # What testServer cannot see: that the evidence table and the decision
  # controls are mounted and visible in the page, that the Record decision
  # button fires, and that the record reaches the Reproducible Code tab.
  siever_with_app(load_example = TRUE, function(app) {
    expect_identical(siever_open_tab(app, "retention"), "retention")
    # Results first: the evidence summary and the decision control lead the
    # page above its sub-tabs; the records are in
    # the Decision records sub-tab, opened below before they are counted.
    siever_wait_render(app, timeout = 120 * 1000)
    # The bundled example's analyses wait for Update
    # (DMAR's 1,000 reference datasets, VSS on DMAR's solutions, the model
    # fit by k under ML with FIML); the test asks for them as a user would.
    # The run buttons are in the evidence table, drawn after the page.
    # A wait that times out stops with what the page showed; the limits
    # allow for a busy machine.
    siever_wait_or_report(app, "!!document.getElementById('retention-pa_go_table')",
      "The evidence table's run buttons", timeout = 300 * 1000)
    for (id in c("retention-pa_go_table", "retention-vss_go_table", "retention-fit_go_table")) {
      if (isTRUE(app$get_js(sprintf("!!document.getElementById('%s')", id)))) app$click(id)
    }
    siever_wait_or_report(app,
      "(function(){var t=(document.getElementById('retention-summary_reco')||{}).textContent||''; return t.length > 0 && !/Not run yet|not run yet|running|has not run/.test(t);})()",
      "The retention summary", report = c("retention-summary_reco", "retention-status_line"),
      timeout = 900 * 1000)
    siever_wait_render(app, timeout = 120 * 1000)

    reco <- app$get_text("#retention-summary_reco")
    expect_match(reco, "Heuristic tally, not a calibrated consensus", fixed = TRUE)
    expect_match(reco, "SIEVER does not choose the number of factors", fixed = TRUE)
    # One row for the theorized count and one per criterion, all of it on
    # the page (no cell is cut off: the table is not a clipped widget).
    expect_identical(
      as.integer(app$get_js("document.querySelectorAll('#retention-summary_tbl table tbody tr').length")),
      4L
    )
    tbl <- app$get_text("#retention-summary_tbl")
    expect_match(tbl, "Very Simple Structure", fixed = TRUE)
    expect_match(tbl, "Counts factors while the empirical factor-analytic eigenvalue", fixed = TRUE)
    visible <- function(id) {
      isTRUE(app$get_js(sprintf(
        "(function(){var e=document.getElementById('%s');return !!e && e.getClientRects().length > 0;})()", id
      )))
    }
    for (id in c("retention-decide_k", "retention-decide_why", "retention-decide_go")) {
      expect_true(visible(id), info = id)
    }

    # No rationale: recorded all the same, and the record says none was
    # given.
    app$set_inputs(`retention-decide_k` = 3)
    app$click("retention-decide_go")
    siever_wait_render(app, timeout = 60 * 1000)
    status <- app$get_text("#retention-decide_status")
    expect_match(status,
      "Recorded as D1: 3 factors retained, no rationale given. Nothing else was changed.",
      fixed = TRUE
    )
    expect_match(status, "No rationale was given.", fixed = TRUE)

    app$set_inputs(`retention-decide_why` = "Three factors, as theorized; MAP alone suggests two.")
    app$click("retention-decide_go")
    siever_wait_render(app, timeout = 60 * 1000)
    status <- app$get_text("#retention-decide_status")
    expect_match(status,
      "Recorded as D2: 3 factors retained (replaces D1, which is kept in the record). Nothing else was changed.",
      fixed = TRUE
    )
    expect_match(status, "Decision in force (D2)", fixed = TRUE)
    app$click(selector = "a.nav-link[data-value='VSS and Decisions']")
    siever_wait_render(app, timeout = 60 * 1000)
    expect_identical(siever_n_table_rows(app, "retention-decide_tbl"), 2L)
    expect_identical(siever_n_output_errors(app), 0L)

    # Changing a display control does not disturb the record or raise an error.
    app$set_inputs(`retention-criterion` = "mean")
    siever_wait_render(app, timeout = 120 * 1000)
    expect_match(app$get_text("#retention-decide_status"), "Decision in force (D2)", fixed = TRUE)
    expect_identical(siever_n_output_errors(app), 0L)

    expect_identical(siever_open_tab(app, "repro"), "repro")
    # The Report sub-tab leads the page; the script is in Session script.
    compat_open_subtab(app, "repro", "Session script", timeout = 60 * 1000)
    code <- app$get_text("#repro-code")
    expect_match(code, "Decision D1 (factor_retention): 3", fixed = TRUE)
    expect_match(code, "# Rationale:  no rationale given", fixed = TRUE)
    expect_match(code, "Decision D2 (factor_retention): 3, replacing D1", fixed = TRUE)
    expect_match(code, "# Rationale:  Three factors, as theorized; MAP alone suggests two.", fixed = TRUE)
    # (The parallel analysis in the script is DMAR's call with its seed,
    # which needs no mc.cores line.)
    expect_match(code, "DMAR::parallel_analysis(", fixed = TRUE)
    # (VSS is DMAR's call with its seed as well, so nothing draws from
    # set.seed(); the script says the seed was drawn, beside the call that
    # takes it.)
    expect_false(grepl("set\\.seed\\(", code))
    expect_match(code, "(no seed was supplied; SIEVER drew this one so the run can be replayed)", fixed = TRUE)
    expect_true(grepl("seed = [0-9]+", code))
  })
})

# ---- Identified models, kept results and the tally -------------------------

# Small pools from two factors: four to six continuous
# items, n = 150. With so few items an exploratory model with two or three
# factors has negative degrees of freedom.
ret_small_pool <- function(seed) {
  withr::with_seed(seed, {
    n <- 150L
    p <- sample(4:6, 1)
    f1 <- stats::rnorm(n)
    f2 <- stats::rnorm(n)
    L <- cbind(stats::runif(p, .3, .8), stats::runif(p, -.5, .6))
    X <- as.data.frame(sapply(seq_len(p), function(j) L[j, 1] * f1 + L[j, 2] * f2 + stats::rnorm(n, sd = .7)))
    names(X) <- paste0("v", seq_len(p))
    X
  })
}

test_that("the degrees of freedom of an exploratory model are the textbook count and agree with psych", {
  # Independent of the formula in the package: the correlations available,
  # p (p - 1) / 2, less the free loadings, p k - k (k - 1) / 2.
  for (p in 3:12) {
    for (k in seq_len(p - 1L)) {
      by_count <- p * (p - 1) / 2 - (p * k - k * (k - 1) / 2)
      expect_identical(sieve_efa_df(p, k), by_count)
    }
    # The largest k that passes the count, found by walking through them.
    largest <- 0L
    for (k in seq_len(p - 1L)) if (p * (p - 1) / 2 - (p * k - k * (k - 1) / 2) >= 0) largest <- k
    expect_identical(sieve_max_identified_k(p), largest)
  }
  expect_identical(vapply(3:10, sieve_max_identified_k, integer(1)), c(1L, 1L, 2L, 3L, 3L, 4L, 5L, 6L))
  expect_identical(sieve_max_identified_k(2L), 0L)
  # psych reports the same degrees of freedom for the models it fits.
  R <- stats::cor(ret_two_factor())
  for (k in 1:4) {
    f <- suppressWarnings(suppressMessages(psych::fa(R, nfactors = k, n.obs = 400, fm = "ml", rotate = "none")))
    expect_equal(f$dof, sieve_efa_df(8L, k))
  }
})

test_that("a Very Simple Structure count is read only over identified models that can be compared", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  # By hand: the largest fit is at k = 3, which is not identified.
  one <- sieve_vss_count(c(0.62, 0.77, 0.79), c(TRUE, FALSE, FALSE), 1L)
  expect_identical(one$k, NA_integer_) # only k = 1 is left: nothing to compare
  expect_identical(one$candidates, 1L)
  expect_identical(one$unrestricted, 3L)
  # Complexity 2 is not defined with one factor (psych's 0 is a placeholder).
  two <- sieve_vss_count(c(0, 0.81, 0.80), c(TRUE, TRUE, FALSE), 2L)
  expect_identical(two$k, NA_integer_)
  expect_identical(two$candidates, 2L)
  ok <- sieve_vss_count(c(0.50, 0.70, 0.60, 0.80), c(TRUE, TRUE, TRUE, FALSE), 1L)
  expect_identical(ok$k, 2L)
  expect_identical(ok$unrestricted, 4L)
  expect_identical(sieve_vss_count(c(0, 0.6, 0.7, 0.65), rep(TRUE, 4), 2L)$k, 3L)
  # A placeholder zero is never the answer, even when every fit is negative.
  expect_identical(sieve_vss_count(c(0, -0.2, -0.1), rep(TRUE, 3), 2L)$k, 3L)

  # A four item pool. psych returns a fit for k = 1, 2, 3, and
  # only k = 1 has an identified exploratory model.
  X <- ret_small_pool(1002)
  expect_identical(ncol(X), 4L)
  ev <- sieve_retention_evidence(X, rep("continuous", 4), n_iter = 10, max_k = 8, seed = 2)
  expect_identical(ev$vss$max_k, 3L)
  expect_identical(ev$vss$max_identified, 1L)
  expect_identical(ev$vss$identified, c(TRUE, FALSE, FALSE))
  expect_identical(ev$fit$not_identified, 2:3)
  # A k whose exploratory model is not identified is not fit
  # (DMAR::fa_exploratory() fits only an identified model), so it has no
  # value. An engine that fits it (psych::fa) returns values there, the
  # largest at a k that is not identified, which is why the count is read
  # over the identified k and is not the position of the largest value.
  direct <- ret_vss(stats::cor(X), 1L, nrow(X), seed = 2L)
  expect_equal(ev$vss$vss1[1], as.numeric(direct$cfit.1), tolerance = 1e-13)
  expect_true(all(is.na(ev$vss$vss1[2:3])))
  expect_true(all(is.na(ev$vss$vss2)))
  old_engine <- withr::with_seed(2, ret_vss(stats::cor(X), 3L, 150L, engine = "psych"))
  expect_gt(which.max(old_engine$cfit.1), 1L)
  expect_gt(which.max(old_engine$cfit.2), 1L)

  cr <- sieve_retention_criteria(ev$basis, ev$pa, ev$vss)
  expect_true(all(cr$max_identified == 1L))
  for (id in c("vss1", "vss2")) {
    row <- cr[cr$id == id, ]
    expect_true(is.na(row$suggested), info = id)
    expect_identical(row$status, "no count", info = id)
    expect_match(row$reason, "No count is read", fixed = TRUE)
    expect_match(row$reason, "identified up to k = 1 with 4 items", fixed = TRUE)
  }
  # Parallel analysis counts eigenvalues, so its number is reported as it
  # is, with what it cannot be used for.
  pa_row <- cr[cr$id == "pa", ]
  expect_identical(pa_row$suggested, 2L)
  expect_identical(pa_row$status, "ran")
  expect_match(pa_row$reason, "more than the 1 factor whose exploratory model is identified with 4 items", fixed = TRUE)

  sm <- sieve_retention_summary(cr)
  expect_identical(sm$max_identified, 1L)
  expect_identical(sm$beyond, "pa")
  expect_false(any(c("vss1", "vss2") %in% sm$available))
  expect_true(sm$capped)
  expect_lte(sm$neighborhood[2], 1L)
  expect_lte(sm$neighborhood[1], sm$neighborhood[2])
  st <- sieve_retention_statements(cr, sm)
  expect_false(grepl("Very Simple Structure\\)", st$tally)) # not among the families behind a count
  expect_match(st$neighborhood, "The range stops at 1", fixed = TRUE)
  expect_match(st$identified, "parallel analysis suggests 2", fixed = TRUE)
  expect_match(st$identified, "at most 1 factor", fixed = TRUE)
  expect_match(st$unavailable, "VSS complexity 1 (No count is read", fixed = TRUE)

  # The recorded R applies the same rule and gives the same counts.
  env <- new.env(parent = globalenv())
  env$R_items <- ev$basis$R
  env$n_complete <- ev$basis$n
  utils::capture.output(suppressWarnings(suppressMessages(eval(parse(text = sieve_vss_code(ev$vss)), envir = env))))
  expect_true(is.na(env$vss1_k) && is.na(env$vss2_k))
  expect_false("map" %in% cr$id)
  expect_false(exists("map_k", envir = env, inherits = FALSE))
})

test_that("over many small pools no Very Simple Structure count comes from a model that is not identified", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  seen <- 0L
  for (s in 1:24) {
    X <- ret_small_pool(1000 + s)
    p <- ncol(X)
    ev <- suppressWarnings(sieve_retention_evidence(X, rep("continuous", p), n_iter = 10, max_k = 8, seed = s))
    cr <- sieve_retention_criteria(ev$basis, ev$pa, ev$vss)
    top <- sieve_max_identified_k(p)
    vss_rows <- cr[cr$id %in% c("vss1", "vss2"), ]
    # A count, when there is one, lies at an identified k ...
    expect_true(all(is.na(vss_rows$suggested) | vss_rows$suggested <= top), info = paste("seed", s))
    # ... and equals the largest fit among the identified k at which the
    # complexity is defined and DMAR returned a solution, recomputed here
    # from the fit values (a k DMAR refused has no fit to compare, so it is
    # not a candidate).
    for (cx in 1:2) {
      fit <- if (cx == 1L) ev$vss$vss1 else ev$vss$vss2
      k <- seq_along(fit)
      cand <- k[k >= cx & k <= top & is.finite(fit)]
      want <- if (length(cand) < 2L) NA_integer_ else cand[which.max(fit[cand])]
      expect_identical(vss_rows$suggested[cx], as.integer(want), info = paste("seed", s, "complexity", cx))
      # Where the largest value lies at a k that is not identified, the row
      # says what was done instead of reporting that k.
      free <- k[k >= cx & is.finite(fit)]
      if (length(free) && free[which.max(fit[free])] > top) {
        seen <- seen + 1L
        expect_match(vss_rows$reason[cx],
          if (is.na(want)) "No count is read" else "The fit is larger still at k = ",
          fixed = TRUE
        )
      }
    }
    # Any other count above the identified maximum carries the note.
    big <- cr[cr$id %in% c("pa", "map") & !is.na(cr$suggested) & cr$suggested > top, ]
    expect_true(all(grepl("cannot be fit and compared as an exploratory model", big$reason, fixed = TRUE)))
    sm <- sieve_retention_summary(cr)
    if (!anyNA(sm$neighborhood)) expect_lte(sm$neighborhood[2], top)
  }
  # A larger fit at a k that is not identified arises only from an engine
  # that fits those models (psych). VSS reads DMAR's solutions, and no model
  # that is not identified is fit, so it does not arise from data; the case
  # is set by hand below.
  expect_identical(seen, 0L)

  # The case with a count and a larger fit beyond it, with values set by
  # hand on six items probed to five factors (identified up to three).
  X <- ret_two_factor()[1:200, 1:6]
  ev <- sieve_retention_evidence(X, rep("continuous", 6), n_iter = 10, max_k = 5, seed = 3)
  expect_identical(ev$vss$identified, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  ev$vss$vss1 <- c(0.50, 0.70, 0.60, 0.80, 0.75)
  ev$vss$vss2 <- c(0, 0.60, 0.65, 0.90, 0.70)
  ev$vss$refused <- character(0) # every k has a value in this hand-set case
  cr <- sieve_retention_criteria(ev$basis, ev$pa, ev$vss)
  expect_identical(cr$suggested[cr$id == "vss1"], 2L)
  expect_identical(cr$suggested[cr$id == "vss2"], 3L)
  expect_identical(cr$status[cr$id %in% c("vss1", "vss2")], c("ran", "ran"))
  expect_identical(
    cr$reason[cr$id == "vss1"],
    paste0(
      "The fit is larger still at k = 4, but an exploratory model with that many factors is not ",
      "identified with 6 items (negative degrees of freedom, so its loadings are not unique); ",
      "the count is read over k = 1 to 3."
    )
  )
  expect_match(cr$reason[cr$id == "vss2"], "the count is read over k = 2 to 3.", fixed = TRUE)
})

test_that("the nine bundled tests probed to eight factors are read over the five identified ones", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  fx <- sieve_hs9_fixture()
  H <- fx$data[, fx$item_cols]
  st <- ret_state(H, map = fx$theory_map, theory_k = 3L)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 10, max_k = 8, seed = 1
    )
    v <- vss_run()
    expect_identical(v$max_k, 8L)
    expect_identical(v$identified, c(rep(TRUE, 5), rep(FALSE, 3)))
    cr <- criteria()
    expect_identical(cr$suggested[cr$id == "vss1"], as.integer(which.max(v$vss1[1:5])))
    expect_identical(cr$suggested[cr$id == "vss2"], as.integer(1L + which.max(v$vss2[2:5])))
    expect_identical(names(v$refused), c("4", "5"))
    note <- ret_text(output$mapvss_note)
    expect_match(note,
      "for k = 6, 7, 8 an exploratory model has negative degrees of freedom with 9 items",
      fixed = TRUE
    )
    # The sentence is set as one piece: no space before its comma or period.
    # It also names the k DMAR refused and why.
    expect_match(note, "needs at least two factors, for k = 6, 7, 8", fixed = TRUE)
    expect_match(note, "so its loadings are not unique and it is not fit, and DMAR refused the solution at k = 4 (not converged) and k = 5 (no maximum)", fixed = TRUE)
    expect_match(note, "VSS is read over the remaining k", fixed = TRUE)
    expect_match(note, "30 random starts and the recorded seed 1 (supplied)", fixed = TRUE)
    # The fit note closes up its punctuation too, and reads no number of
    # factors from a cutoff.
    fit_said <- ret_text(output$fit_note)
    expect_match(fit_said, "no number of factors is read from them. Read the 90% RMSEA interval", fixed = TRUE)
    expect_false(grepl("RMSEA &lt;= 0.06", fit_said, fixed = TRUE))
    expect_match(fit_said,
      "(Finch, 2020; Xia, 2021). Not fit: k = 6, 7, 8, where the model has more parameters",
      fixed = TRUE
    )
    expect_match(ret_text(output$summary_note), "ran from seed 1 (supplied). Where the criteria disagree", fixed = TRUE)
    expect_false(grepl("MAP removes components", note, fixed = TRUE))
    # The same counts from the recorded R.
    log <- shiny::isolate(state$codelog)
    at <- max(which(grepl("Very Simple Structure", log, fixed = TRUE)))
    step <- sieve_vss_code(v)
    env <- new.env(parent = globalenv())
    env$R_items <- basis()$R
    env$n_complete <- basis()$n
    utils::capture.output(suppressWarnings(suppressMessages(eval(parse(text = step), envir = env))))
    expect_identical(as.integer(env$vss1_k), cr$suggested[cr$id == "vss1"])
    expect_identical(as.integer(env$vss2_k), cr$suggested[cr$id == "vss2"])
    expect_identical(env$identified, v$identified)

    # A retained number above what an exploratory model can carry is the
    # researcher's to record; the tab says what it implies and records it.
    session$setInputs(
      decide_k = 7, decide_why = "Seven narrow abilities, to be tested as a restricted model.",
      decide_go = 1
    )
    expect_identical(shiny::isolate(state$decisions)$D1$choice, 7L)
    said <- ret_text(output$decide_status)
    expect_match(said, "Recorded as D1: 7 factors retained. Nothing else was changed.", fixed = TRUE)
    expect_match(said,
      "an exploratory model with 7 factors has negative degrees of freedom for 9 items (at most 5",
      fixed = TRUE
    )
    expect_false(grepl("no longer matches the key", said, fixed = TRUE))

    # A record changed after it was made is reported: the key written to
    # the reproducible code does not match it.
    held <- shiny::isolate(state$decisions)
    held$D1$choice <- 2L
    state$decisions <- held
    session$flushReact()
    expect_match(ret_text(output$decide_status), "Record D1 no longer matches the key", fixed = TRUE)
  })
})

test_that("what psych reports while computing the correlations is kept and shown whatever the worker setting", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  # Five binary items in which two are never endorsed together: an empty
  # cell, which psych replaces and reports with a message.
  B <- withr::with_seed(4, {
    f <- stats::rnorm(100)
    d <- as.data.frame(sapply(1:5, function(j) as.integer(.6 * f + .8 * stats::rnorm(100) > 1.1)))
    names(d) <- paste0("b", 1:5)
    d$b2[d$b1 == 1] <- 0L
    d
  })
  expect_identical(sum(B$b1 == 1 & B$b2 == 1), 0L)
  # What psych says, from a direct call on one worker.
  direct <- sieve_capture(withr::with_options(list(mc.cores = 1L), {
    utils::capture.output(out <- psych::tetrachoric(B, correct = 0.5))
    out
  }))
  expect_true(any(grepl("A cell entry of 0 was replaced", direct$messages, fixed = TRUE)))

  results <- lapply(list(NULL, 1L, 2L), function(mc) {
    withr::with_options(list(mc.cores = mc), {
      b <- sieve_retention_cor(B, rep("binary", 5), "tetrachoric")
      expect_identical(getOption("mc.cores"), mc) # the session's setting is put back
      b
    })
  })
  for (b in results) {
    expect_identical(b$used, "tetrachoric")
    expect_identical(b$messages, direct$messages)
    expect_identical(b$R, results[[1]]$R) # the values do not depend on the workers
  }
  expect_equal(unname(results[[1]]$R), unname(direct$value$rho), tolerance = 1e-12)

  # The recorded R carries the notice and computes the same matrix.
  code <- sieve_retention_cor_code(results[[1]])
  expect_true(any(grepl("^# Reported: +For i = 2 j = 1 +A cell entry of 0 was replaced", code)))
  expect_true(any(code == "old_options <- options(mc.cores = 1L)"))
  env <- new.env(parent = globalenv())
  env$active_items <- B
  before <- getOption("mc.cores")
  utils::capture.output(suppressWarnings(suppressMessages(eval(parse(text = code), envir = env))))
  expect_identical(getOption("mc.cores"), before)
  expect_equal(unname(env$R_items), unname(results[[1]]$R), tolerance = 1e-12)

  # The page shows it next to every result computed from that matrix.
  st <- ret_state(B, types = "binary")
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 10, max_k = 3, seed = 2
    )
    expect_length(basis()$messages, 1L)
    # (The model fit by k reads the item data with lavaan, not this matrix,
    # so its note is not among them.)
    for (out in list(
      output$overview, output$scree_note, output$mapvss_note,
      output$summary_reco
    )) {
      expect_match(ret_text(out), "The engine also reported:", fixed = TRUE)
      expect_match(ret_text(out), "A cell entry of 0 was replaced", fixed = TRUE)
    }
    expect_true(any(grepl("A cell entry of 0 was replaced", shiny::isolate(state$codelog), fixed = TRUE)))
  })

  # When the polychoric step fails, the reason does not depend on how many
  # workers the session happens to allow, and it comes with what the
  # response categories look like.
  O <- ret_ordinal()
  O$a1[which.max(O$a1)] <- 6L
  failed <- lapply(list(NULL, 1L, 2L), function(mc) {
    withr::with_options(list(mc.cores = mc), sieve_retention_cor(O, rep("ordered", 6), "polychoric"))
  })
  expect_identical(failed[[1]]$reason, failed[[2]]$reason)
  expect_identical(failed[[1]]$reason, failed[[3]]$reason)
  expect_identical(failed[[1]]$warnings, failed[[3]]$warnings)
  expect_identical(failed[[1]]$used, "pearson")
  tab <- table(O$a1)
  expect_identical(unname(tab[["6"]]), 1L)
  expect_identical(
    failed[[1]]$facts,
    paste0(
      "What the data show: between 5 and 6 distinct response values per item, and the least ",
      "used value (6, on item a1) was observed once among the 300 complete cases."
    )
  )
  cr_code <- sieve_retention_cor_code(failed[[1]])
  expect_true(any(grepl("^# Data: +What the data show", cr_code)))
  # On the page the engine's reason and the facts follow one another as
  # sentences, in every panel that shows a result from that matrix.
  shiny::testServer(mod_retention_server, args = list(state = ret_state(O, types = "ordered")), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 10, max_k = 3, seed = 2
    )
    said <- paste0(
      "so this is a different analysis from the one requested. ", failed[[1]]$reason, ". ",
      "What the data show: between 5 and 6 distinct response values per item, and the least used ",
      "value (6, on item a1) was observed once among the 300 complete cases. Pearson correlations of ",
      "ordered or binary items are attenuated"
    )
    for (out in list(output$overview, output$scree_note, output$summary_reco)) {
      expect_match(ret_text(out), said, fixed = TRUE)
    }
    expect_match(criteria()$reason[1], paste0(failed[[1]]$reason, ". What the data show:"), fixed = TRUE)
  })
  expect_null(sieve_retention_cor(ret_ordinal(), rep("ordered", 6), "polychoric")$facts)
  expect_identical(
    sieve_category_facts(data.frame(x = c(1, 1, 2, 2, 2), y = c(0, 1, 1, 0, 0))),
    "What the data show: 2 distinct response values per item, and the least used value (1, on item x) was observed 2 times among the 5 complete cases."
  )
})

test_that("a result is kept under the identity of its inputs and computed again only when that changes", {
  store <- new.env(parent = emptyenv())
  count <- new.env()
  count$calls <- 0L
  count$nothing <- 0L
  compute <- function() {
    count$calls <- count$calls + 1L
    count$calls
  }
  expect_identical(sieve_keep(store, "a", list(1, "x"), compute), 1L)
  expect_identical(sieve_keep(store, "a", list(1, "x"), compute), 1L) # same identity: kept
  expect_identical(count$calls, 1L)
  expect_identical(sieve_keep(store, "a", list(2, "x"), compute), 2L) # new identity: computed
  expect_identical(sieve_keep(store, "a", list(1, "x"), compute), 3L) # one result is kept, the last
  expect_identical(sieve_keep(store, "b", list(1, "x"), compute), 4L) # another computation, another slot
  expect_identical(sieve_keep(store, "a", list(1, "x"), compute), 3L)
  # A result that is NULL is still a result.
  none <- function() {
    count$nothing <- count$nothing + 1L
    NULL
  }
  expect_null(sieve_keep(store, "n", 1, none))
  expect_null(sieve_keep(store, "n", 1, none))
  expect_identical(count$nothing, 1L)
})

test_that("an edit that changes nothing the analysis uses reruns nothing and draws no new seed", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols, drop = FALSE]
  st <- ret_state(d, map = fx$theory_map, theory_k = 3L)
  # Counters live in an environment the mocked functions write to.
  seen <- new.env()
  seen$ran <- c(cor = 0L, pa = 0L, vss = 0L, fit = 0L, progress = 0L)
  seen$shown <- character(0)
  tick <- function(what) seen$ran[[what]] <- seen$ran[[what]] + 1L
  real <- list(
    cor = sieve_retention_cor, pa = sieve_parallel_analysis,
    vss = sieve_retention_vss, fit = sieve_retention_fit
  )
  local_mocked_bindings(
    sieve_retention_cor = function(...) {
      tick("cor")
      real$cor(...)
    },
    sieve_parallel_analysis = function(...) {
      tick("pa")
      real$pa(...)
    },
    sieve_retention_vss = function(...) {
      tick("vss")
      real$vss(...)
    },
    sieve_retention_fit = function(...) {
      tick("fit")
      real$fit(...)
    },
    # The long run announces itself: the message is recorded, the run made.
    withProgress = function(expr, ..., message = NULL, detail = NULL) {
      tick("progress")
      seen$shown <- c(seen$shown, paste(message, detail))
      expr
    }
  )
  blocks <- function(state) {
    sum(grepl("pa_normal <- withr::with_seed(",
      shiny::isolate(state$codelog),
      fixed = TRUE
    ))
  }
  set.seed(31)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 20, max_k = 4
    ) # no seed supplied
    cr1 <- criteria()
    seed1 <- run_rng()$seed
    draws1 <- pa_run()$reference$simulated$draws
    log1 <- shiny::isolate(state$codelog)
    expect_false(run_rng()$supplied)
    expect_identical(unname(seen$ran[c("cor", "pa", "vss", "fit")]), rep(1L, 4))
    expect_identical(blocks(state), 1L)
    # The Monte Carlo ran under a progress message that names its size, and
    # the model fit by k, whose sequence is lavaan::efa fits, under its own
    # (two announcements).
    expect_identical(unname(seen$ran[["progress"]]), 2L)
    pa_shown <- seen$shown[grepl("^Parallel analysis", seen$shown)]
    expect_length(pa_shown, 1L)
    expect_match(pa_shown, "Parallel analysis: 20 reference datasets", fixed = TRUE)
    expect_match(pa_shown, "DMAR::parallel_analysis(): 20 reference datasets for each of the two kinds of reference data", fixed = TRUE)
    expect_match(pa_shown, "from one recorded seed", fixed = TRUE)
    expect_true(any(grepl("^Model fit by k: lavaan::efa for k = 1 to 4", seen$shown)))

    # A display label, and the descriptive columns of the ledger: none of
    # them is part of what the criteria are computed from.
    m <- shiny::isolate(state$meta)
    key_before <- sieve_revision(state)$meta_key
    m$label[1] <- "A new display label"
    if ("mean" %in% names(m)) m$mean[2] <- m$mean[2] + 0
    state$meta <- m
    session$flushReact()
    expect_identical(sieve_revision(state)$meta_key, key_before)
    cr2 <- criteria()
    expect_identical(unname(seen$ran[c("cor", "pa", "vss", "fit", "progress")]), c(rep(1L, 4), 2L))
    expect_identical(run_rng()$seed, seed1)
    expect_identical(pa_run()$reference$simulated$draws, draws1)
    expect_identical(cr2, cr1)
    expect_identical(shiny::isolate(state$codelog), log1) # nothing was written to the script

    # A decision recorded now lists the evidence on screen, and stays in
    # step with it through another label edit.
    session$setInputs(decide_k = 3, decide_why = "Three, as theorized.", decide_go = 1)
    m$label[2] <- "Another label"
    state$meta <- m
    session$flushReact()
    expect_identical(unname(seen$ran[["pa"]]), 1L)
    status <- ret_text(output$decide_status)
    expect_match(status, "Decision in force (D1)", fixed = TRUE)
    expect_false(grepl("is not the evidence this decision lists", status, fixed = TRUE))
    expect_false(grepl("have changed since this decision was recorded", status, fixed = TRUE))

    # The display controls still read the kept draws.
    session$setInputs(criterion = "mean")
    session$setInputs(pa_reference = "resampled")
    expect_identical(unname(seen$ran[c("cor", "pa", "vss", "fit", "progress")]), c(rep(1L, 4), 2L))
    session$setInputs(criterion = "p95", pa_reference = "auto")

    # Another seed is another analysis: it runs, and the tab says that the
    # evidence on screen is not what the decision lists. The record
    # is not touched.
    rec <- shiny::isolate(state$decisions)$D1
    session$setInputs(seed = 99)
    expect_identical(run_rng()$seed, 99L)
    # Under the run policy a parallel analysis after a change is from before
    # and waits for its button: it is not rerun.
    expect_identical(unname(seen$ran[["pa"]]), 1L)
    expect_identical(pa_run()$status, "not run")
    expect_true(isTRUE(pa_run()$stale))
    expect_identical(unname(seen$ran[["cor"]]), 1L) # the matrix does not depend on the seed
    expect_identical(unname(seen$ran[["fit"]]), 1L)
    status <- ret_text(output$decide_status)
    expect_match(status, "is not the evidence this decision lists", fixed = TRUE)
    expect_identical(shiny::isolate(state$decisions)$D1, rec)
    session$setInputs(seed = NA)

    # A change that the analysis does use (an item's confirmed type) is a
    # new analysis with a newly drawn seed.
    before <- seen$ran
    m$type[1] <- "ordered"
    state$meta <- m
    sieve_confirm_types(state)
    session$flushReact()
    expect_false(identical(sieve_revision(state)$meta_key, key_before))
    cr3 <- criteria()
    expect_identical(unname(seen$ran[["cor"]]), unname(before[["cor"]]) + 1L)
    expect_identical(unname(seen$ran[["pa"]]), unname(before[["pa"]])) # waits for its button
    expect_false(run_rng()$supplied)
    expect_false(identical(run_rng()$seed, seed1))
    expect_true(all(cr3$basis_requested == "mixed"))
  })
})

test_that("a run on polychoric correlations names DMAR and the correlations its reference datasets are analyzed with", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  # The run must start by itself here, whatever the load on the machine;
  # the announcement does not depend on the estimate. (The run is
  # DMAR::parallel_analysis(), and a long one goes to a background R process
  # with its elapsed time and Cancel, test-dmar-parallel.R; the automatic
  # run is at most two seconds, so its announcement does not warn of
  # minutes.)
  local_mocked_bindings(sieve_pa_units = function(...) 0.01)
  O <- ret_ordinal()
  st <- ret_state(O, types = "ordered")
  seen <- new.env()
  seen$said <- character(0)
  local_mocked_bindings(withProgress = function(expr, ..., message = NULL, detail = NULL) {
    seen$said <- c(seen$said, paste(message, detail))
    expr
  })
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 10, max_k = 3, seed = 5
    )
    expect_true(pa_run()$ok)
    # Two announcements: the shared correlation matrix (sieve_state_basis),
    # then the Monte Carlo. (The model fit by k announces itself as well and
    # is set aside here.)
    seen$said <- seen$said[!grepl("^Model fit by k", seen$said)]
    expect_length(seen$said, 2L)
    expect_match(seen$said[1], "Computing the polychoric correlations of 6 items", fixed = TRUE)
    expect_match(seen$said[2], "Parallel analysis: 10 reference datasets", fixed = TRUE)
    expect_match(seen$said[2],
      "DMAR::parallel_analysis(): 10 reference datasets for each of the two kinds of reference data on the polychoric correlations",
      fixed = TRUE
    )
    expect_identical(pa_run()$reference$resampled$correlations, "polychoric")
  })
})

test_that("a family enters the tally at most once, and a divided family counts toward no number", {
  # A divided family: the two complexities of Very Simple Structure differ.
  cr <- ret_criteria(pa = 4, map = 1, vss1 = 1, vss2 = 2)
  sm <- sieve_retention_summary(cr)
  expect_identical(sm$by_k$k, c(1L, 2L, 4L))
  expect_identical(sm$by_k$criteria, c("map, vss1", "vss2", "pa")) # each criterion under its count
  expect_identical(sm$by_k$families, c("MAP", "", "Parallel analysis"))
  expect_identical(sm$by_k$n_families, c(1L, 0L, 1L))
  expect_identical(sm$n_families, 3L)
  expect_identical(sm$divided$family, "VSS")
  expect_identical(sm$divided$criteria, "vss1, vss2")
  expect_identical(sm$divided$counts, "1, 2")
  expect_identical(sum(sm$by_k$n_families) + nrow(sm$divided), sm$n_families)
  st <- sieve_retention_statements(cr, sm)
  expect_match(st$tally,
    "no single count leads; 1 and 4 are each suggested by 1 of the 3 criterion families",
    fixed = TRUE
  )
  expect_match(st$tally,
    "Very Simple Structure is divided (VSS complexity 1 suggests 1 and VSS complexity 2 suggests 2) and counts toward none of them.",
    fixed = TRUE
  )
  expect_match(st$tally, "counts once at most", fixed = TRUE)

  # With the complexities at one number the family counts once for it.
  same <- sieve_retention_summary(ret_criteria(pa = 3, map = 2, vss1 = 3, vss2 = 3))
  expect_identical(same$by_k$n_families, c(1L, 2L))
  expect_identical(nrow(same$divided), 0L)
  # One complexity without a count: the family is the one that has one.
  half <- sieve_retention_summary(ret_criteria(pa = 3, map = 2, vss1 = 2, vss2 = NA))
  expect_identical(half$by_k$families, c("MAP, VSS", "Parallel analysis"))
  expect_identical(nrow(half$divided), 0L)
  # Only a divided family has counts: no number is behind a whole family.
  only <- ret_criteria(pa = NA, map = NA, vss1 = 1, vss2 = 2, reason = "not run")
  so <- sieve_retention_summary(only)
  expect_identical(so$modal, integer(0))
  expect_identical(so$neighborhood, c(1L, 3L))
  expect_match(
    sieve_retention_statements(
      only,
      so
    )$tally,
    "no number is suggested by a whole criterion family",
    fixed = TRUE
  )
  expect_match(
    sieve_retention_statements(
      only,
      so
    )$neighborhood,
    "(the suggested counts plus or minus one;",
    fixed = TRUE
  )

  # Whatever the four criteria suggest, every family is in exactly one
  # place: behind one number, divided, or without a count.
  values <- c(NA, 0L, 1L, 2L, 3L)
  grid <- expand.grid(pa = values, map = values, vss1 = values, vss2 = values)
  ok <- vapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i, ]
    s <- sieve_retention_summary(ret_criteria(g$pa, g$map, g$vss1, g$vss2, reason = "x"))
    with_count <- c(!is.na(g$pa), !is.na(g$map), !is.na(g$vss1) || !is.na(g$vss2))
    vss_split <- !is.na(g$vss1) && !is.na(g$vss2) && g$vss1 != g$vss2
    s$n_families == sum(with_count) &&
      nrow(s$divided) == as.integer(vss_split) &&
      sum(s$by_k$n_families) + nrow(s$divided) == s$n_families &&
      all(s$modal %in% s$by_k$k) &&
      (!length(s$modal) || all(s$by_k$n_families[match(s$modal, s$by_k$k)] == max(s$by_k$n_families)))
  }, logical(1))
  expect_true(all(ok))
  expect_identical(length(ok), 625L)
})

test_that("the heading says agreement only when at least two families agree", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  head_of <- function(...) {
    cr <- ret_criteria(..., reason = "not run")
    st <- sieve_retention_statements(cr, sieve_retention_summary(cr))
    c(st$heading, st$tone)
  }
  expect_identical(head_of(NA, NA, NA, NA), c("No count.", "warning"))
  expect_identical(head_of(NA, 1, NA, NA), c("One criterion only.", "secondary"))
  expect_identical(head_of(NA, NA, 2, 2), c("One criterion family only.", "secondary"))
  expect_identical(head_of(2, 2, NA, NA), c("Agreement.", "info"))
  expect_identical(head_of(2, 2, 2, 2), c("Agreement.", "info"))
  expect_identical(head_of(3, 2, 2, 2), c("Disagreement is a finding.", "warning"))
  expect_identical(head_of(NA, NA, 1, 2), c("Disagreement is a finding.", "warning"))

  one <- ret_criteria(NA, 1, NA, NA, reason = "not run")
  said <- sieve_retention_statements(one, sieve_retention_summary(one))
  expect_identical(
    said$range,
    paste0(
      "Only MAP produced a count: it suggests 1 factor. One criterion is not agreement; ",
      "the others are unavailable for the reasons given below."
    )
  )
  expect_false(grepl("weaker evidence", said$range, fixed = TRUE))
  fam <- ret_criteria(NA, NA, 2, 2, reason = "not run")
  expect_match(sieve_retention_statements(fam, sieve_retention_summary(fam))$range,
    "belong to one family (Very Simple Structure) and suggest 2 factors. One kind of evidence is not agreement",
    fixed = TRUE
  )

  # Through the module: the alert carries that heading.
  X <- ret_two_factor()[1:200, 1:5]
  real <- sieve_retention_criteria
  for (case in c("none", "one")) {
    local_mocked_bindings(sieve_retention_criteria = function(...) {
      cr <- real(...)
      # One criterion keeps its count: the first that has one (VSS may have
      # none here, where DMAR refuses a solution).
      keep <- which(!is.na(cr$suggested))[1L]
      drop <- if (case == "none") seq_len(nrow(cr)) else setdiff(seq_len(nrow(cr)), keep)
      cr$suggested[drop] <- NA_integer_
      cr$status[drop] <- "failed"
      cr$reason[drop] <- "forced in the test"
      cr
    })
    st <- ret_state(X)
    shiny::testServer(mod_retention_server, args = list(state = st), {
      session$setInputs(
        cor_type = "auto", criterion = "p95", pa_reference = "auto",
        n_iter = 10, max_k = 3, seed = 1
      )
      reco <- ret_text(output$summary_reco)
      expect_false(grepl("Agreement.", reco, fixed = TRUE), info = case)
      expect_false(grepl("Disagreement is a finding.", reco, fixed = TRUE), info = case)
      expect_match(reco, if (case == "none") "No count." else "One criterion only.", fixed = TRUE)
      expect_match(reco, if (case == "none") "alert alert-warning" else "alert alert-secondary", fixed = TRUE)
    })
  }
})

test_that("the recorded fit by k ends with its table, and no rule reads a count from it", {
  # (The recorded R carries no cutoff rule, such as RMSEA at most .06 and
  # CFI and TLI at least .95, and reads no count from one; the table is the
  # result.)
  fx <- sieve_hs9_fixture()
  hb <- sieve_retention_cor(fx$data[, fx$item_cols], rep("continuous", 9), "pearson")
  code <- sieve_fit_code(sieve_retention_fit(hb, max_k = 4, data = fx$data[, fx$item_cols], spec = sieve_lavaan_spec(data.frame(item = fx$item_cols, type = "continuous", status = "active"))))
  expect_false(any(grepl("^meets <-|^fit_k <-|rmsea <=|cfi >=|tli >=", code)))
  expect_identical(code[length(code)], "fit_by_k")
  env <- new.env(parent = globalenv())
  env$active_items <- fx$data[, fx$item_cols]
  utils::capture.output(suppressWarnings(eval(parse(text = code), envir = env)))
  expect_identical(env$fit_by_k$k, 1:4)
  expect_true(all(c("rmsea", "rmsea_lo", "rmsea_hi", "cfi", "tli") %in% names(env$fit_by_k)))
  expect_false(exists("fit_k", envir = env, inherits = FALSE))
})

test_that("imported engines remain available independently of optional-package probes", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  local_mocked_bindings(sieve_have = function(pkg) FALSE)
  X <- ret_two_factor()[1:150, 1:6]
  ev <- sieve_retention_evidence(X, rep("continuous", 6), n_iter = 3L, max_k = 3L, seed = 1L)
  expect_true(ev$basis$ok)
  expect_true(ev$pa$ok)
  expect_true(ev$vss$ok)
  expect_true(ev$fit$ok)
  expect_equal(ev$basis$R, stats::cor(X), tolerance = 1e-14)
})

# ---- Decision records, seeds, waits and the evidence table ----------------

test_that("the tab refuses a new decision while an earlier record is not what was written", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols, drop = FALSE]
  st <- ret_state(d, map = fx$theory_map, theory_k = 3L)
  shiny::isolate({ sieve_new_dataset(st); sieve_confirm_types(st) })
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 10, max_k = 4, seed = 3
    )
    session$setInputs(decide_k = 3, decide_why = "Three, as theorized.", decide_go = 1)
    expect_length(shiny::isolate(state$decisions), 1L)
    held <- shiny::isolate(state$decisions)
    held$D1$choice <- 9L
    state$decisions <- held
    log_before <- shiny::isolate(state$codelog)
    session$setInputs(decide_k = 2, decide_why = "Two, on reflection.", decide_go = 2)
    expect_length(shiny::isolate(state$decisions), 1L)
    expect_identical(shiny::isolate(state$codelog), log_before)
    status <- ret_text(output$decide_status)
    expect_match(status, "Decision record D1 no longer matches", fixed = TRUE)
    expect_match(status, "nothing was recorded", fixed = TRUE)
    # The page's own check of the records says the same thing.
    expect_match(status, "Record D1 no longer matches the key", fixed = TRUE)
  })
})

test_that("excluding an item is a new analysis with a newly drawn seed, and a label edit is not", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols, drop = FALSE]
  st <- ret_state(d, map = fx$theory_map, theory_k = 3L)
  seen <- new.env()
  seen$ran <- c(cor = 0L, pa = 0L, vss = 0L, fit = 0L)
  tick <- function(what) seen$ran[[what]] <- seen$ran[[what]] + 1L
  real <- list(
    cor = sieve_retention_cor, pa = sieve_parallel_analysis,
    vss = sieve_retention_vss, fit = sieve_retention_fit
  )
  local_mocked_bindings(
    sieve_retention_cor = function(...) {
      tick("cor")
      real$cor(...)
    },
    sieve_parallel_analysis = function(...) {
      tick("pa")
      real$pa(...)
    },
    sieve_retention_vss = function(...) {
      tick("vss")
      real$vss(...)
    },
    sieve_retention_fit = function(...) {
      tick("fit")
      real$fit(...)
    },
    withProgress = function(expr, ...) expr
  )
  set.seed(47)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 20, max_k = 4
    ) # no seed supplied
    cr1 <- criteria()
    seed1 <- run_rng()$seed
    key1 <- pool_key()
    expect_identical(unname(seen$ran), rep(1L, 4))
    session$setInputs(decide_k = 3, decide_why = "Three, as theorized.", decide_go = 1)

    # A label: nothing the analysis uses, so nothing runs and the seed stays.
    m <- shiny::isolate(state$meta)
    m$label[2] <- "Another display label"
    state$meta <- m
    session$flushReact()
    expect_identical(criteria(), cr1)
    expect_identical(run_rng()$seed, seed1)
    expect_identical(pool_key(), key1)
    expect_identical(unname(seen$ran), rep(1L, 4))
    expect_false(grepl("have changed since this decision was recorded",
      ret_text(output$decide_status),
      fixed = TRUE
    ))

    # An exclusion changes the item pool: every analysis runs again, from
    # a newly drawn seed, and the decision made on the earlier pool is
    # flagged as made on other inputs.
    m$status[1] <- "parked"
    state$meta <- m
    session$flushReact()
    cr2 <- criteria()
    expect_false(identical(pool_key(), key1))
    # The correlations run again; the results computed before are updated
    # by the page's Update, which an exclusion asks for (the inclusion
    # wire): VSS here, and parallel analysis, whose run is estimated under
    # two seconds and so is computed in the session at once. The model fit
    # by k goes to the background.
    expect_identical(unname(seen$ran), c(2L, 2L, 2L, 1L))
    expect_identical(pa_run()$status, "ran")
    expect_false(isTRUE(pa_run()$stale))
    expect_false(isTRUE(vss_run()$stale))
    expect_false(run_rng()$supplied)
    expect_false(identical(run_rng()$seed, seed1))
    expect_identical(ncol(x_data()), 8L)
    expect_false(identical(cr2, cr1))
    expect_match(ret_text(output$decide_status),
      "The data or the included items have changed since this decision was recorded",
      fixed = TRUE
    )
    # Included again, the pool is the first one. The store keeps one result
    # per analysis (the last), so the analyses run once more, but from the
    # seed drawn for this pool the first time: one seed is kept for every
    # pool seen (sieve_keep_each()), so inputs that return to an earlier
    # identity give back its results to the last digit (a restore of
    # removed respondents, or an item included again, gives back the first
    # results). The evidence on screen is the
    # evidence the decision lists.
    m$status[1] <- "active"
    state$meta <- m
    session$flushReact()
    expect_identical(pool_key(), key1)
    cr3 <- criteria()
    # Back to the first pool: its kept parallel analysis and VSS
    # (not run for the second pool, so still the first pool's)
    # and fit are read.
    expect_identical(unname(seen$ran), c(3L, 2L, 2L, 1L))
    expect_false(run_rng()$supplied)
    expect_identical(run_rng()$seed, seed1)
    expect_identical(cr3$basis_used, cr1$basis_used)
    expect_identical(cr3$suggested, cr1$suggested)
    status <- ret_text(output$decide_status)
    expect_false(grepl("have changed since this decision was recorded", status, fixed = TRUE))
    expect_false(grepl("is not the evidence this decision lists", status, fixed = TRUE))
  })
})

test_that("the progress message states a wait estimated from DMAR's rate per reference dataset, then from the measured rate", {
  local_measured_rates()
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  # (The estimate is DMAR's standard rate per reference dataset,
  # sieve_pa_units(), for every dataset of both kinds of reference data; a
  # run then measures the rate.)
  O <- ret_ordinal()
  st <- ret_state(O, types = "ordered")
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  seen <- new.env()
  seen$said <- character(0)
  seen$took <- NA_real_
  local_mocked_bindings(withProgress = function(expr, ..., message = NULL, detail = NULL) {
    # The model fit by k announces itself too; only the
    # basis and the Monte Carlo are read here.
    if (grepl("^Model fit by k", message %||% "")) return(expr)
    seen$said <- c(seen$said, paste(message, detail))
    started <- proc.time()[["elapsed"]]
    out <- expr
    seen$took <- proc.time()[["elapsed"]] - started
    out
  })
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 10, max_k = 3, seed = 5
    )
    plan0 <- sieve_at_default_rates(pa_plan())
    # (The plan's arithmetic, not its rate formula; the estimate is compared
    # with the measured run below and in test-dmar-parallel.R.)
    expect_equal(plan0$seconds, (20 + 2) * plan0$per_dataset, tolerance = 1e-12)
    expect_identical(plan0$draws, 20L)
    expect_true(plan0$automatic)
    expect_false(plan0$background)
    run <- pa_run()
    expect_true(run$ok)
    # The matrix announces itself first (sieve_state_basis), then the run.
    expect_length(seen$said, 2L)
    expect_match(seen$said[1], "Computing the polychoric correlations of 6 items", fixed = TRUE)
    said <- seen$said[2]
    expect_match(said, "Parallel analysis: 10 reference datasets", fixed = TRUE)
    expect_match(said, sprintf("estimated wait %s.", plan0$label), fixed = TRUE)
    # Nothing was reduced, and the run's own time is kept and shown.
    expect_identical(run$n_iter_requested, 10L)
    expect_identical(run$n_iter, 10L)
    expect_true(is.finite(run$seconds) && run$seconds > 0)
    expect_lte(run$seconds, seen$took + 0.05)
    # The stated estimate was of the right size: within a factor of three
    # of the run it announced (a check of wall-clock time, so only when
    # SIEVER_TIMING_TESTS is true).
    if (siever_timing_checks()) {
      expect_lt(abs(log(run$seconds / plan0$seconds)), log(3))
    }
    expect_match(ret_text(output$scree_note), "the run took [0-9.]+ seconds")
    # The run measured the rate, which the next estimate uses.
    expect_true(exists("pa_dmar_polychoric", envir = sieve_rates, inherits = FALSE))
    expect_identical(sieve_pa_plan(basis(), 10)$source, "the time per dataset an earlier run in this session measured")
  })
})

test_that("the evidence table shows the count or the reason in one cell, and the decision block states what the pool allows", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols, drop = FALSE]
  st <- ret_state(d, map = fx$theory_map, theory_k = 3L)
  real <- sieve_retention_criteria
  local_mocked_bindings(
    sieve_retention_criteria = function(...) {
      cr <- real(...)
      at <- cr$id == "vss2"
      cr$suggested[at] <- NA_integer_
      cr$status[at] <- "no count"
      cr$reason[at] <- "Forced: no count for this test."
      cr$reason[cr$id == "pa"] <- "Forced: a note on a count."
      cr
    },
    withProgress = function(expr, ...) expr
  )
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 10, max_k = 4, seed = 2
    )
    html <- ret_text(output$summary_tbl)
    expect_match(html, "Suggested factors, or why there is no count", fixed = TRUE)
    rows <- strsplit(html, "<tr>", fixed = TRUE)[[1]]
    vss2 <- rows[grepl("Forced: no count for this test.", rows, fixed = TRUE)]
    expect_length(vss2, 1L)
    # The reason stands where the count would be, once, and not as a note.
    expect_match(vss2, "<td>\\s*<span class=\"text-muted\">Forced: no count for this test.</span>")
    expect_false(grepl("<b>Note: </b>", vss2, fixed = TRUE))
    expect_false(grepl("not available", vss2, fixed = TRUE))
    pa <- rows[grepl("Forced: a note on a count.", rows, fixed = TRUE)]
    expect_length(pa, 1L)
    expect_match(pa, "<td>\\s*<b>[0-9]+</b>")
    # The note's label and text are written on separate lines of the page.
    expect_match(gsub("\\s+", " ", pa), "<b>Note: </b> Forced: a note on a count.", fixed = TRUE)
    # Before a number is entered, the block says what the pool allows.
    bounds <- ret_text(output$decide_bounds)
    expect_match(bounds, "The included pool has 9 items, so a whole number from 0 to 8 can be recorded.", fixed = TRUE)
    expect_match(bounds, sprintf("identified for at most %d factors", sieve_max_identified_k(9L)), fixed = TRUE)
    expect_match(bounds, "a larger number is recorded as entered and noted as such", fixed = TRUE)
  })
})

# ---- Parallel analysis on request ------------------------------------------

test_that("each reference is DMAR's own computation from the recorded seed, on every basis, and what DMAR reports is kept", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  # (The run is DMAR::parallel_analysis(), one call per kind of reference
  # data. A run is not stopped partway: a canceled background run keeps
  # nothing, test-dmar-parallel.R.)
  O <- ret_ordinal()
  b <- sieve_retention_cor(O, rep("ordered", 6), "polychoric")
  one <- sieve_parallel_analysis(O, b, n_iter = 10, seed = 5)
  expect_identical(one$status, "ran")
  expect_identical(one$n_iter, 10L)
  expect_identical(one$n_iter_requested, 10L)
  expect_identical(one$path, "data")
  expect_identical(one$cores, 1L)
  for (family in c("normal", "resample")) {
    direct <- DMAR::parallel_analysis(O, missing = "listwise", ordered = TRUE, reference = family,
      B = 10, percentile = 0.95, conf_level = 0.95, seed = 5)
    got <- one$reference[[if (identical(family, "normal")) "simulated" else "resampled"]]
    expect_identical(got$draws, unname(attr(direct, "reference_draws")), info = family)
    expect_identical(got$p95, direct$reference_quantile, info = family)
    expect_identical(one$empirical, direct$estimate)
  }
  # The same run again is the same run.
  expect_identical(sieve_parallel_analysis(O, b, n_iter = 10, seed = 5)$reference, one$reference)
  # Pearson items: the data route on the complete cases.
  X <- ret_two_factor()[1:200, 1:6]
  bp <- sieve_retention_cor(X, rep("continuous", 6), "pearson")
  rp <- sieve_parallel_analysis(X, bp, n_iter = 10, seed = 5)
  expect_identical(rp$reference$simulated$draws, unname(attr(DMAR::parallel_analysis(X, missing = "listwise",
    reference = "normal", B = 10, percentile = 0.95, conf_level = 0.95, seed = 5), "reference_draws")))
  # The mixed basis: DMAR is given the matrix, only normal data are simulated.
  M <- X
  M[4:6] <- O[1:200, 4:6]
  bm <- sieve_retention_cor(M, c(rep("continuous", 3), rep("ordered", 3)), "mixed")
  rm_ <- sieve_parallel_analysis(M, bm, n_iter = 10, seed = 5)
  expect_identical(rm_$path, "matrix")
  dm <- DMAR::parallel_analysis(S = bm$R, N = 200L, reference = "normal", B = 10, percentile = 0.95,
    conf_level = 0.95, seed = 5)
  expect_identical(rm_$reference$simulated$draws, unname(attr(dm, "reference_draws")))
  expect_false(rm_$reference$resampled$available)
  expect_true(rm_$reference$simulated$available)
  # What DMAR reports (its warnings about reference fits) comes back with the run.
  B <- withr::with_seed(4, {
    f <- stats::rnorm(100)
    d <- as.data.frame(sapply(1:5, function(j) as.integer(.6 * f + .8 * stats::rnorm(100) > 1.1)))
    names(d) <- paste0("b", 1:5)
    d$b2[d$b1 == 1] <- 0L
    d
  })
  bt <- sieve_retention_cor(B, rep("binary", 5), "tetrachoric")
  rt <- sieve_parallel_analysis(B, bt, n_iter = 10, seed = 5)
  said <- character(0)
  for (family in c("normal", "resample")) {
    withCallingHandlers(
      tryCatch(DMAR::parallel_analysis(B, missing = "listwise", ordered = TRUE, reference = family, B = 10,
        percentile = 0.95, conf_level = 0.95, seed = 5), error = function(e) NULL),
      warning = function(w) {
        said <<- c(said, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  }
  expect_setequal(rt$warnings, unique(said))
})

test_that("the plan states the wait before a run, from DMAR's rate and then from the measured rate, and decides whether the run waits for the button", {
  local_measured_rates()
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  # (The estimate is DMAR's standard rate per reference dataset over the
  # datasets of both kinds of reference data; there are no workers, and a run on request above two
  # seconds goes to a background R process.)
  O <- ret_ordinal()
  b <- sieve_retention_cor(O, rep("ordered", 6), "polychoric")
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  plan <- sieve_pa_plan(b, 100, threshold = 10)
  expect_identical(plan$n_iter, 100)
  expect_identical(plan$families, c("normal", "resample"))
  expect_identical(plan$draws, 200L)
  # (The plan's arithmetic, not its rate formula, which
  # test-dmar-parallel.R checks against measured runs. Each call also
  # computes its observed matrix.)
  expect_equal(plan$compute, (200 + 2) * plan$per_dataset, tolerance = 1e-12)
  expect_equal(plan$fixed, 0)
  expect_true(plan$background)
  expect_equal(plan$seconds, plan$compute + sieve_pa_start_seconds(), tolerance = 1e-12)
  expect_identical(plan$label, sieve_duration_label(plan$seconds))
  expect_identical(plan$source, "DMAR's standard rate on this kind of data")
  expect_true(plan$automatic) # about 5 seconds at the standard rate, under a threshold of 10
  expect_false(sieve_pa_plan(b, 100, threshold = 0)$automatic)
  expect_true(sieve_pa_plan(b, 10, threshold = 1e6)$automatic)
  quick <- sieve_pa_plan(b, 10)
  expect_false(quick$background)
  expect_equal(quick$seconds, quick$compute, tolerance = 1e-12)
  # After a run the rate it measured replaces DMAR's standard rate.
  run <- sieve_parallel_analysis(O, b, n_iter = 10, seed = 5)
  expect_true(exists("pa_dmar_polychoric", envir = sieve_rates, inherits = FALSE))
  later <- sieve_pa_plan(b, 100)
  expect_identical(later$source, "the time per dataset an earlier run in this session measured")
  # The measured rate rescales the standard estimate.
  expect_equal(later$per_dataset / plan$per_dataset, sieve_rate("pa_dmar_polychoric") / 0.03, tolerance = 1e-12)
  # The record of a run that waits: nothing suggested, the reason says what
  # the wait is and what to press, and the summary leaves it out and says so.
  nr <- sieve_pa_not_run(b, 100, sieve_draw_seed(5), plan)
  expect_identical(nr$status, "not run")
  expect_false(nr$ok)
  expect_match(
    nr$reason,
    sprintf(paste0(
      "^Parallel analysis has not run yet\\. DMAR::parallel_analysis\\(\\) would draw 100 reference datasets for each of ",
      "its two kinds of reference data \\(200 in all\\); at about [0-9.]+ seconds each, the estimated wait is %s, ",
      "in a background R process \\(its start included\\) while the page stays usable\\. ",
      "Press Run parallel analysis to start it\\.$"
    ), plan$label)
  )
  sel <- sieve_pa_select(nr, "p95", "auto", TRUE)
  expect_false(sel$ok)
  expect_identical(sel$reason, nr$reason)
  vss <- sieve_retention_vss(b, max_k = 3, seed = 5)
  cr <- sieve_retention_criteria(b, nr, vss, "p95", "auto", TRUE)
  expect_identical(cr$status[cr$id == "pa"], "not run")
  expect_true(is.na(cr$suggested[cr$id == "pa"]))
  expect_match(cr$reason[cr$id == "pa"], "Parallel analysis has not run yet.", fixed = TRUE)
  sm <- sieve_retention_summary(cr)
  expect_true("pa" %in% sm$unavailable)
  expect_false("pa" %in% sm$available)
  expect_false("Parallel analysis" %in% unlist(strsplit(sm$by_k$families, ", ")))
  st <- sieve_retention_statements(cr, sm)
  expect_match(
    st$unavailable,
    "^Not run yet, and not in the tally until it is: parallel analysis\\. Parallel analysis has not run yet\\. DMAR::parallel_analysis\\(\\) would draw 100 reference datasets"
  )
  # A criterion that failed or gave no count is still reported as such,
  # apart from the one that waits.
  expect_false(grepl(
    "Not available, and not counted as agreeing with anything: [^.]*parallel analysis",
    st$unavailable
  ))
  expect_match(sieve_pa_code(nr, sel)[1], "^# Not run:")
  expect_identical(sieve_pa_route(b)$form, "data")
})

# The contracts of the background DMAR job (a start failure shown and asked
# again, the wait before the button, elapsed time, Cancel, a changed
# request stopping the run) are in test-dmar-parallel.R.

test_that("a short parallel analysis still runs by itself, under its progress bar", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols, drop = FALSE]
  st <- ret_state(d, map = fx$theory_map, theory_k = 3L)
  seen <- new.env()
  seen$said <- character(0)
  local_mocked_bindings(withProgress = function(expr, ..., message = NULL, detail = NULL) {
    seen$said <- c(seen$said, paste(message, detail))
    expr
  })
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 20, max_k = 4, seed = 113
    )
    expect_true(pa_plan()$automatic)
    run <- pa_run()
    expect_true(run$ok)
    expect_identical(run$status, "ran")
    expect_identical(run$n_iter, 20L)
    expect_identical(run$cores, 1L)
    # The model fit by k announces itself too.
    seen$said <- seen$said[!grepl("^Model fit by k", seen$said)]
    expect_length(seen$said, 1L)
    expect_match(seen$said, "Parallel analysis: 20 reference datasets", fixed = TRUE)
    expect_match(ret_text(output$pa_control),
      "ran automatically (a first run, estimated at two seconds or less): DMAR::parallel_analysis(), 20 reference datasets",
      fixed = TRUE
    )
    expect_false(grepl("pa_go", ret_text(output$summary_tbl), fixed = TRUE))
    # The same as the plain function.
    plain <- sieve_parallel_analysis(x_data(), basis(), n_iter = 20, seed = 113)
    expect_identical(run$reference, plain$reference)
  })
})

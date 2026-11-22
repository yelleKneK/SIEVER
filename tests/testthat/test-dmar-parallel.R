# The Dimensionality page's parallel analysis is DMAR::parallel_analysis()
# (DMAR 1.1.0), on every correlation basis the page offers: DMAR forms the
# Pearson, saturated FIML and polychoric matrices from the item responses
# (data route), and the mixed matrix, or a FIML matrix with auxiliary
# variables, is passed as S with N (matrix route). The run is one DMAR call
# per kind of reference data from one recorded seed; a long run goes to a
# background R process, the EFA page's mechanism. The scree plot's reduced
# series is the same fixed one-factor reduction (stats::factanal). The EFA
# page's DMAR::factor_count() would run the same normal reference a second
# time, so it runs with B = 0 and reads this run. The parity checks compare
# the observed series and the reference draws with computations written
# out independently.
#
# Author: Ken Kelley

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("lavaan")
skip_if_not_installed("psych")
withr::local_options(sieve.cores = 2L)

pa_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
pa_hs9 <- function() {
  fx <- sieve_hs9_fixture()
  fx$data[, fx$item_cols, drop = FALSE]
}
# Values missing at random in every item: 20 per item, 169 complete rows.
pa_hs9_missing <- function() {
  withr::with_seed(3, {
    Z <- pa_hs9()
    for (j in seq_along(Z)) Z[sample.int(nrow(Z), 20L), j] <- NA
    Z
  })
}
# Two correlated factors, three items each, cut to five ordered categories.
pa_ordinal <- function(n = 300L) {
  withr::with_seed(20261014, {
    f1 <- stats::rnorm(n)
    f2 <- 0.3 * f1 + sqrt(1 - 0.09) * stats::rnorm(n)
    it <- function(f, l) l * f + sqrt(1 - l^2) * stats::rnorm(n)
    X <- data.frame(a1 = it(f1, .80), a2 = it(f1, .75), a3 = it(f1, .70), b1 = it(f2, .80), b2 = it(f2, .75), b3 = it(f2, .70))
    as.data.frame(lapply(X, function(v) as.integer(cut(v, c(-Inf, -1, -0.2, 0.5, 1.3, Inf)))))
  })
}
# The fixed one-factor reduction, written out independently of SIEVER.
pa_reduced <- function(R, n) {
  u2 <- stats::factanal(covmat = list(cov = R, n.obs = n), factors = 1, rotation = "none")$uniquenesses
  r <- R
  diag(r) <- 1 - u2
  eigen(r, symmetric = TRUE, only.values = TRUE)$values
}
# The one-factor ML discrepancy at the uniquenesses u2 (factanal's objective).
pa_objective <- function(R, u2) {
  Ds <- diag(1 / sqrt(u2))
  e <- eigen(Ds %*% R %*% Ds, symmetric = TRUE, only.values = TRUE)$values
  -sum(log(e[-1])) + sum(e[-1]) - (nrow(R) - 1)
}

# ---- How DMAR reads each of the page's correlation bases --------------------------

test_that("each correlation basis of the page maps to the DMAR call that forms it, or to S with N", {
  X <- pa_hs9()
  pearson <- sieve_retention_cor(X, rep("continuous", 9), "pearson")
  r <- sieve_pa_route(pearson)
  expect_identical(r[c("form", "missing", "ordered", "families", "correlations")],
    list(form = "data", missing = "listwise", ordered = FALSE, families = c("normal", "resample"), correlations = "pearson"))
  fiml <- sieve_retention_cor(pa_hs9_missing(), rep("continuous", 9), "pearson", missing = "fiml")
  expect_identical(sieve_pa_route(fiml)[c("form", "missing")], list(form = "data", missing = "fiml"))
  aux <- fiml
  aux$aux <- "x_aux"
  expect_identical(sieve_pa_route(aux)$form, "matrix")
  expect_match(sieve_pa_route(aux)$why, "auxiliary variables", fixed = TRUE)
  O <- pa_ordinal()
  poly <- sieve_retention_cor(O, rep("ordered", 6), "polychoric")
  expect_identical(sieve_pa_route(poly)[c("form", "missing", "ordered", "correlations")],
    list(form = "data", missing = "listwise", ordered = TRUE, correlations = "polychoric"))
  expect_identical(sieve_pa_route(list(used = "tetrachoric"))$ordered, TRUE)
  mixed <- sieve_pa_route(list(used = "mixed"))
  expect_identical(mixed[c("form", "families")], list(form = "matrix", families = "normal"))

  # Every argument is written out, from the one recorded seed.
  calls <- sieve_pa_dmar_calls(X, pearson, 200L, 41L)
  expect_identical(names(calls$calls), c("normal", "resample"))
  a <- calls$calls$resample$args
  expect_identical(a[c("missing", "reference", "B", "percentile", "conf_level", "seed")],
    list(missing = "listwise", reference = "resample", B = 200L, percentile = 0.95, conf_level = 0.95, seed = 41L))
  expect_null(a$ordered)
  expect_identical(sieve_pa_dmar_calls(O, poly, 200L, 1L)$calls$normal$args$ordered, TRUE)
  m <- sieve_pa_dmar_calls(X, list(used = "mixed", R = pearson$R, n = 301L, p = 9L), 200L, 1L)
  expect_identical(names(m$calls), "normal")
  expect_identical(names(m$calls$normal$args)[1:2], c("S", "N"))
  expect_identical(m$calls$normal$args$N, 301L)
})

# ---- Parity: the observed reduced eigenvalues ---------------------------------------

test_that("the observed series is the fixed one-factor reduction of the page's matrix, and psych's reduction agrees to its optimizer", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  X <- pa_hs9()
  b <- sieve_retention_cor(X, rep("continuous", 9), "pearson")
  pa <- sieve_parallel_analysis(X, b, n_iter = 20L, seed = 5L)
  expect_true(pa$ok)
  # Exact: DMAR reduces the same matrix with stats::factanal.
  expect_identical(max(abs(pa$empirical - pa_reduced(b$R, b$n))), 0)
  # The scree plot's reduced series is the same reduction.
  expect_identical(max(abs(sieve_scree_values(b, 1L)$table$reduced - pa$empirical)), 0)
  # psych's reduction (fa(fm = "ml", nfactors = 1, SMC = FALSE)) stops at
  # another point of the same flat optimum: the reduced eigenvalues differ by
  # about 1e-5 and the ML objectives by less than 1e-8 (measured: 1.32e-5 and
  # 3.1e-10, psych the lower; factanal stops 1.6e-5 in the uniquenesses from a
  # tightly converged optimum).
  ps <- psych::fa(b$R, fm = "ml", nfactors = 1, SMC = FALSE, warnings = FALSE)
  expect_lt(max(abs(ps$values - pa$empirical)), 5e-5)
  fa1 <- stats::factanal(covmat = list(cov = b$R, n.obs = b$n), factors = 1, rotation = "none")
  expect_lt(abs(pa_objective(b$R, fa1$uniquenesses) - pa_objective(b$R, ps$uniquenesses)), 1e-8)

  # The saturated FIML basis: DMAR's FIML matrix is the page's (exact), with
  # the complete-case N as its reference size, a stated benchmark.
  Xm <- pa_hs9_missing()
  bm <- sieve_retention_cor(Xm, rep("continuous", 9), "pearson", missing = "fiml")
  pm <- sieve_parallel_analysis(Xm, bm, n_iter = 20L, seed = 5L)
  expect_identical(pm$path, "data")
  expect_identical(max(abs(pm$empirical - pa_reduced(bm$R, bm$n))), 0)
  expect_identical(pm$n, bm$n)
  expect_identical(pm$n, sum(stats::complete.cases(Xm)))
  expect_true(pm$benchmark)

  # The mixed basis goes as S with N: the page's own matrix, exactly.
  Xx <- cbind(X[, 1:5], withr::with_seed(5, as.data.frame(lapply(X[, 6:9], function(v) {
    as.integer(cut(v, stats::quantile(v, c(0, .2, .45, .7, .9, 1)), include.lowest = TRUE))
  }))))
  bx <- sieve_retention_cor(Xx, c(rep("continuous", 5), rep("ordered", 4)), "mixed")
  px <- sieve_parallel_analysis(Xx, bx, n_iter = 20L, seed = 5L)
  expect_identical(px$path, "matrix")
  expect_lt(max(abs(px$empirical - pa_reduced(bx$R, bx$n))), 1e-10)

  # Ordered items: DMAR's polychoric matrix (lavaan's two-step estimate)
  # against the page's psych matrix: within 1e-4 (measured 2.0e-5 in the
  # correlations and 2.5e-5 in the reduced eigenvalues on this fixture).
  O <- pa_ordinal()
  bo <- sieve_retention_cor(O, rep("ordered", 6), "polychoric")
  po <- sieve_parallel_analysis(O, bo, n_iter = 20L, seed = 5L)
  lav <- as.matrix(lavaan::lavCor(O, ordered = names(O), missing = "pairwise", output = "cor"))
  expect_lt(max(abs(lav - bo$R)), 1e-4)
  expect_identical(max(abs(po$empirical - pa_reduced(lav, nrow(O)))), 0)
  expect_lt(max(abs(po$empirical - pa_reduced(bo$R, bo$n))), 1e-4)
})

# ---- Parity: the reference draws -------------------------------------------------------

test_that("the normal reference is the reduction of independent normal data of the page's N and p, draw for draw", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  X <- pa_hs9()
  b <- sieve_retention_cor(X, rep("continuous", 9), "pearson")
  pa <- sieve_parallel_analysis(X, b, n_iter = 30L, seed = 77L)
  # Written out independently: the draws DMAR makes from the seed.
  draws <- withr::with_seed(77L, t(vapply(seq_len(30L), function(i) {
    x <- matrix(stats::rnorm(301 * 9), 301, 9)
    pa_reduced(stats::cor(x), 301)
  }, numeric(9))))
  ref <- pa$reference$simulated
  expect_equal(ref$draws, unname(draws), tolerance = 1e-12)
  expect_equal(ref$p95, apply(draws, 2, stats::quantile, probs = 0.95, type = 7, names = FALSE), tolerance = 1e-12)
  expect_equal(ref$mean, colMeans(draws), tolerance = 1e-12)
  # DMAR's Monte Carlo limits of the percentile: binomial order statistics.
  lo <- stats::qbinom(0.025, 30, 0.95)
  expect_equal(ref$mc_lower, apply(draws, 2, function(v) sort(v)[lo]), tolerance = 1e-12)
  expect_true(all(is.infinite(ref$mc_upper)))
  expect_identical(ref$n_used, 30L)
  # The count is the sequential rule on the page's selection.
  sel <- sieve_pa_select(pa, "p95", "simulated")
  expect_identical(sel$suggested, as.integer(sum(cumprod(pa$empirical > ref$p95))))
  expect_identical(sel$suggested, attr(pa$dmar$normal, "suggested_factors"))
  sel_mean <- sieve_pa_select(pa, "mean", "resampled")
  expect_identical(sel_mean$values, pa$reference$resampled$mean)
  expect_match(sel_mean$label, "mean of the factor-analytic eigenvalues of 30 datasets of each item's observed responses resampled", fixed = TRUE)
  # The resampled reference draws from each item's own values.
  rs <- withr::with_seed(77L, t(vapply(seq_len(30L), function(i) {
    x <- as.data.frame(lapply(X, function(v) sample(v, 301, replace = TRUE)))
    pa_reduced(stats::cor(x), 301)
  }, numeric(9))))
  expect_equal(pa$reference$resampled$draws, unname(rs), tolerance = 1e-12)
})

test_that("on ordered items both references are analyzed with polychoric correlations, and the Pearson caution concerns the matrix route alone", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  O <- pa_ordinal()
  bo <- sieve_retention_cor(O, rep("ordered", 6), "polychoric")
  po <- sieve_parallel_analysis(O, bo, n_iter = 20L, seed = 2L)
  expect_identical(po$reference$simulated$correlations, "polychoric")
  expect_identical(po$reference$resampled$correlations, "polychoric")
  expect_match(po$reference$simulated$source, "categorized at each item's observed category proportions", fixed = TRUE)
  sel <- sieve_pa_select(po, "p95", "auto", categorical = TRUE)
  expect_identical(sel$family_used, "resampled")
  expect_null(sel$caution)
  expect_null(sieve_pa_select(po, "p95", "simulated", categorical = TRUE)$caution)
  # The mixed matrix goes as S: normal data with Pearson correlations only,
  # with the caution and the reason the resampled reference is absent.
  X <- pa_hs9()
  Xx <- cbind(X[, 1:5], withr::with_seed(5, as.data.frame(lapply(X[, 6:9], function(v) {
    as.integer(cut(v, stats::quantile(v, c(0, .2, .45, .7, .9, 1)), include.lowest = TRUE))
  }))))
  bx <- sieve_retention_cor(Xx, c(rep("continuous", 5), rep("ordered", 4)), "mixed")
  px <- sieve_parallel_analysis(Xx, bx, n_iter = 20L, seed = 2L)
  sx <- sieve_pa_select(px, "p95", "auto", categorical = TRUE)
  expect_identical(sx$family_used, "simulated")
  expect_true(sx$family_changed)
  expect_match(sx$reason, "reads ordered items only when every item is ordered", fixed = TRUE)
  expect_match(sx$caution, "analyzed with Pearson correlations, while the items were analyzed with mixed", fixed = TRUE)
})

# ---- The same computation as factor_count(): one parallel analysis ---------------------

test_that("factor_count's reference is parallel_analysis's normal reference, so the EFA page runs it with B = 0 and reads this page's run", {
  X <- pa_hs9()
  fc <- DMAR::factor_count(X, max_factors = 3, estimator = "MLR", missing = "fiml", B = 200, seed = 5)
  pa <- DMAR::parallel_analysis(X, missing = "fiml", reference = "normal", B = 200, seed = 5)
  expect_identical(attr(attr(fc, "parallel_analysis"), "reference_draws"), attr(pa, "reference_draws"))
  Xm <- pa_hs9_missing()
  fcm <- DMAR::factor_count(Xm, max_factors = 3, estimator = "MLR", missing = "fiml", B = 200, seed = 5)
  pam <- DMAR::parallel_analysis(Xm, missing = "fiml", reference = "normal", B = 200, seed = 5)
  expect_identical(attr(attr(fcm, "parallel_analysis"), "reference_draws"), attr(pam, "reference_draws"))

  # The page's count call: B = 0, no seed (nothing in it is random).
  route <- sieve_efa_dmar_route("ml", list(estimator = "MLR", missing = "fiml", ordered = NULL))
  call <- sieve_efa_count_call(route, 3L, data = Xm)
  expect_identical(call$args$B, 0L)
  expect_null(call$args$seed)
  fc0 <- do.call(DMAR::factor_count, call$args)
  b <- sieve_retention_cor(Xm, rep("continuous", 9), "pearson", missing = "fiml")
  # Only the following shared-page/replay portion uses the reduced-B fixture.
  siever_local_dmar_mechanics()
  run <- sieve_parallel_analysis(Xm, b, n_iter = 40L, seed = 8L)
  sel <- sieve_pa_select(run, "p95", "auto")
  shared <- list(revision = "pool", empirical = run$empirical, values = sel$values, label = sel$label,
    statistic = "p95", suggested = sel$suggested, seed = 8L, n_used = sel$n_used, matrix_words = "m")
  got <- sieve_efa_count_pa(fc0, shared, "pool")
  expect_true(got$ok)
  expect_identical(max(abs(fc0$eigenvalue - run$empirical[1:3])), 0)
  expect_identical(got$reference, sel$values[1:3])
  expect_identical(got$exceeds, fc0$eigenvalue > sel$values[1:3])
  # Another item pool, or another matrix, is not read.
  expect_identical(sieve_efa_count_pa(fc0, shared, "other pool")$why, "stale")
  listwise <- do.call(DMAR::factor_count, sieve_efa_count_call(
    sieve_efa_dmar_route("ml", list(estimator = "ML", missing = "listwise", ordered = NULL)), 3L, data = Xm)$args)
  expect_identical(sieve_efa_count_pa(listwise, shared, "pool")$why, "other")
  expect_identical(sieve_efa_count_pa(fc0, NULL, "pool")$why, "none")
})

# ---- Refusals and failures, by class -------------------------------------------------------

test_that("DMAR's refusals are read by class with their reason, and a failed run suggests nothing", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  X <- pa_hs9()
  b <- sieve_retention_cor(X, rep("continuous", 9), "pearson")
  call <- sieve_pa_dmar_calls(X, b, 20L, 1L)
  # A misnamed item: DMAR's dmar_items_invalid refusal, with its reason.
  call$calls <- lapply(call$calls, function(cl) {
    cl$args$items <- c("t1_visual_perception", "t1_visual_perception", "t2_cubes")
    cl
  })
  rec <- sieve_pa_dmar_run(call)
  expect_false(rec$ok)
  expect_true(inherits(structure(list(), class = rec$error_class), "dmar_items_invalid"))
  expect_true("dmar_refusal" %in% rec$error_class)
  expect_identical(rec$reason, "duplicate")
  res <- sieve_pa_dmar_result(b, call, rec, sieve_draw_seed(1L), 20L)
  expect_false(res$ok)
  expect_identical(res$status, "failed")
  expect_true("dmar_items_invalid" %in% res$error_class)
  expect_match(res$reason, "^DMAR::parallel_analysis\\(\\) did not run: ")
  expect_match(res$reason, "duplicate", fixed = TRUE)
  sel <- sieve_pa_select(res, "p95", "auto")
  expect_false(sel$ok)
  cr <- sieve_retention_criteria(b, res, list(ok = FALSE, status = "not run"))
  expect_identical(cr$status[cr$id == "pa"], "failed")
  expect_true(is.na(cr$suggested[cr$id == "pa"]))
  # A matrix that is not positive definite is refused on the matrix route,
  # and the scree plot's reduced series says why it is not drawn.
  bad <- list(ok = TRUE, used = "mixed", R = matrix(c(1, .9, .9, .9, 1, .1, .9, .1, 1), 3,
    dimnames = list(c("a", "b", "c"), c("a", "b", "c"))), n = 50L, p = 3L, items = c("a", "b", "c"))
  pb <- sieve_parallel_analysis(data.frame(a = 1:50, b = 1:50, c = 1:50), bad, n_iter = 10L, seed = 1L)
  expect_false(pb$ok)
  expect_match(pb$reason, "positive definite", fixed = TRUE)
  expect_match(sieve_pa_empirical(bad)$error, "not positive definite", fixed = TRUE)
})

# ---- The recorded R ------------------------------------------------------------------------

test_that("the recorded R is the DMAR calls with every argument and the seed, and replays the reference and the count exactly", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  for (case in c("pearson", "fiml", "mixed")) {
    X <- switch(case, pearson = pa_hs9(), fiml = pa_hs9_missing(), mixed = {
      Z <- pa_hs9()
      cbind(Z[, 1:5], withr::with_seed(5, as.data.frame(lapply(Z[, 6:9], function(v) {
        as.integer(cut(v, stats::quantile(v, c(0, .2, .45, .7, .9, 1)), include.lowest = TRUE))
      }))))
    })
    types <- if (identical(case, "mixed")) c(rep("continuous", 5), rep("ordered", 4)) else rep("continuous", 9)
    b <- sieve_retention_cor(X, types, if (identical(case, "mixed")) "mixed" else "pearson",
      missing = if (identical(case, "fiml")) "fiml" else "listwise")
    pa <- sieve_parallel_analysis(X, b, n_iter = 25L, seed = 314L)
    for (statistic in c("p95", "mean")) {
      sel <- sieve_pa_select(pa, statistic, "auto", categorical = identical(case, "mixed"))
      code <- c(sieve_retention_cor_code(b), sieve_pa_code(pa, sel))
      text <- paste(code, collapse = "\n")
      expect_match(text, "DMAR::parallel_analysis(", fixed = TRUE)
      expect_match(text, "seed = 314", fixed = TRUE)
      expect_match(text, "B = 25,", fixed = TRUE)
      expect_false(grepl("SIEVER:::", text, fixed = TRUE))
      expect_false(grepl("psych::fa(", text, fixed = TRUE))
      env <- new.env(parent = globalenv())
      env$active_items <- X
      grDevices::pdf(NULL)
      utils::capture.output(eval(parse(text = text), envir = env))
      grDevices::dev.off()
      expect_identical(unname(env$reference), sel$values, info = paste(case, statistic))
      expect_identical(as.integer(env$n_factors), sel$suggested, info = paste(case, statistic))
      expect_identical(unname(env$observed), pa$empirical, info = case)
    }
  }
  # The scree plot's code is the same reduction.
  b <- sieve_retention_cor(pa_hs9(), rep("continuous", 9), "pearson")
  env <- new.env(parent = globalenv())
  env$active_items <- pa_hs9()
  grDevices::pdf(NULL)
  eval(parse(text = c(sieve_retention_cor_code(b), sieve_scree_code(sieve_scree_values(b, 1L)))), envir = env)
  grDevices::dev.off()
  expect_identical(env$scree_reduced, sieve_scree_values(b, 1L)$table$reduced)
})

# ---- The page: in the session when quick, in a background process when not ----------------

test_that("a quick first parallel analysis runs by itself in the session; the page names DMAR, the Monte Carlo limits, and shares its run", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  st <- siever_test_study(sieve_hs9_fixture(), labels = FALSE)
  seen <- new.env()
  seen$said <- character(0)
  local_mocked_bindings(withProgress = function(expr, ..., message = NULL, detail = NULL) {
    seen$said <- c(seen$said, paste(message, detail))
    expr
  })
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 40, max_k = 3, seed = 9)
    plan <- pa_plan()
    expect_true(plan$automatic)
    expect_false(plan$background)
    expect_identical(plan$draws, 80L)
    run <- pa_run()
    expect_true(run$ok)
    expect_identical(run$n_iter_requested, 40L)
    expect_match(paste(seen$said, collapse = " "), "DMAR::parallel_analysis(): 40 reference datasets for each of the two kinds", fixed = TRUE)
    direct <- sieve_parallel_analysis(x_data(), basis(), n_iter = 40, rng = run_rng())
    expect_identical(run$reference$simulated$p95, direct$reference$simulated$p95)
    expect_identical(run$reference$resampled$mean, direct$reference$resampled$mean)
    ctrl <- pa_text(output$pa_control)
    expect_match(ctrl, "Parallel analysis ran automatically (a first run, estimated at two seconds or less): DMAR::parallel_analysis(), 40 reference datasets", fixed = TRUE)
    note <- pa_text(output$scree_note)
    expect_match(note, "Computed by DMAR::parallel_analysis() on the Pearson correlations of the 301 complete cases", fixed = TRUE)
    expect_match(note, "Monte Carlo limits are [", fixed = TRUE)
    expect_match(note, "DMAR's note: Common-factor parallel analysis: fixed one-factor ML communality reduction; normal reference; N = 301", fixed = TRUE)
    expect_false(grepl("psych::fa.parallel", note, fixed = TRUE))
    expect_match(pa_text(output$method_line), "DMAR::parallel_analysis for parallel analysis", fixed = TRUE)
    cr <- criteria()
    expect_match(cr$produced_by[cr$id == "pa"], "^DMAR::parallel_analysis\\(\\): the fixed one-factor maximum likelihood reduction")
    shared <- shiny::isolate(state$cache$pa)
    expect_identical(shared$empirical, run$empirical)
    expect_identical(shared$values, pa_sel()$values)
    expect_identical(shared$revision, shiny::isolate(sieve_pool_revision(state)))
    # A display choice is read from the same draws, and shared as chosen.
    session$setInputs(criterion = "mean", pa_reference = "resampled")
    expect_identical(pa_sel()$values, run$reference$resampled$mean)
    expect_identical(shiny::isolate(state$cache$pa)$values, run$reference$resampled$mean)
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("pa_resample <- withr::with_seed(9,", log, fixed = TRUE)))
    expect_false(any(grepl("pa_one|sieve_pa_|SIEVER:::", log)))
  })
})

test_that("a long parallel analysis waits for its button, runs in a background R process with its elapsed time and Cancel, and a changed request stops it", {
  st <- siever_test_study(sieve_hs9_fixture(), labels = FALSE)
  log <- new.env(parent = emptyenv())
  # A worker that stays alive until the test lets it finish.
  local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE))
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 3000, max_k = 3, seed = 11)
    plan <- pa_plan()
    expect_false(plan$automatic)
    expect_true(plan$background)
    run <- pa_run()
    expect_identical(run$status, "not run")
    expect_match(run$reason, "DMAR::parallel_analysis() would draw 3000 reference datasets for each of its two kinds of reference data (6000 in all)", fixed = TRUE)
    expect_match(run$reason, "in a background R process (its start included)", fixed = TRUE)
    expect_length(log$calls, 0L)
    expect_match(pa_text(output$pa_control), "Run parallel analysis", fixed = TRUE)
    session$setInputs(pa_go = 1L)
    expect_true(pa_running())
    expect_length(log$calls, 1L)
    expect_identical(log$calls[[1L]]$fun, "parallel_analysis")
    expect_identical(names(log$calls[[1L]]$calls), c("normal", "resample"))
    status <- pa_text(output$status_line)
    expect_match(status, "Parallel analysis (DMAR::parallel_analysis) is running in a background R process.", fixed = TRUE)
    # The elapsed time is the line's own text output (the line is drawn
    # once per run, test-progress-lines.R).
    expect_match(output$pa_progress_line, "^Elapsed:")
    expect_match(status, "Cancel", fixed = TRUE)
    expect_match(pa_text(output$pa_control), "is running in a background R process", fixed = TRUE)
    expect_identical(log$runners, "sieve_pa_dmar_run")
    log$process$alive <- FALSE
    efa_dmar_settle(session)
    expect_false(pa_running())
    done <- pa_run()
    expect_identical(done$status, "ran")
    expect_identical(done$n_iter_requested, 3000L)
    # Cancel: nothing from the stopped run is kept, and it waits for the button.
    session$setInputs(n_iter = 2000)
    session$setInputs(pa_go = 2L)
    expect_true(pa_running())
    session$setInputs(pa_stop = 1L)
    expect_false(pa_running())
    expect_identical(log$killed, 1L)
    stopped <- pa_run()
    expect_identical(stopped$status, "not run")
    expect_match(stopped$reason, "^Canceled: the run was stopped before it finished, and nothing from it is kept\\.")
    # A changed request stops a running one; its result is never shown.
    session$setInputs(pa_go = 3L)
    expect_true(pa_running())
    session$setInputs(seed = 12)
    session$elapse(300)
    expect_false(pa_running())
    expect_identical(log$killed, 2L)
    expect_identical(pa_run()$status, "not run")
    expect_false(isTRUE(pa_run()$stopped))
  })
})

test_that("Cancel of a parallel analysis an Update started lets the Update go on, and the next Update is accepted", {
  st <- siever_test_study(sieve_hs9_fixture(), labels = FALSE)
  log <- new.env(parent = emptyenv())
  said <- new.env()
  said$notes <- character(0)
  local_mocked_bindings(
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE),
    withProgress = function(expr, ...) expr,
    sieve_notify = function(text, ...) said$notes <- c(said$notes, text)
  )
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 3000, max_k = 3, seed = 11)
    session$setInputs(pa_go = 1L)
    log$process$alive <- FALSE
    efa_dmar_settle(session)
    expect_identical(pa_run()$status, "ran")
    # An edit makes it stale; the page's Update starts it again in the background.
    session$setInputs(seed = 12)
    expect_true(isTRUE(pa_run()$stale))
    update_current()
    expect_true(pa_running())
    expect_true(isTRUE(update_queue$active))
    session$setInputs(pa_stop = 1L)
    expect_false(pa_running())
    session$flushReact()
    expect_false(isTRUE(update_queue$active))
    expect_false(any(grepl("Updated parallel analysis", said$notes, fixed = TRUE)))
    # The page is not left waiting on the canceled run.
    session$setInputs(seed = 13)
    expect_true(isTRUE(update_current()) || pa_running())
  })
})

test_that("a start failure is shown and waits for a new request", {
  st <- siever_test_study(sieve_hs9_fixture(), labels = FALSE)
  seen <- new.env()
  seen$starts <- 0L
  seen$messages <- character(0)
  local_mocked_bindings(sieve_efa_dmar_start = function(call, rng_kind = RNGkind(), runner = "sieve_efa_dmar_run") {
    seen$starts <- seen$starts + 1L
    stop("process permission denied", call. = FALSE)
  })
  local_mocked_bindings(showNotification = function(ui, ...) {
    seen$messages <- c(seen$messages, as.character(ui))
    invisible(NULL)
  }, .package = "shiny")
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 3000, max_k = 3, seed = 4)
    session$setInputs(pa_go = 1L)
    expect_false(pa_running())
    expect_match(pa_error(), "could not start: process permission denied", fixed = TRUE)
    expect_match(pa_text(output$pa_control), "process permission denied", fixed = TRUE)
    expect_length(seen$messages, 1L)
    session$setInputs(pa_go = 2L)
    expect_identical(seen$starts, 2L)
  })
})

# ---- The wait estimate: items and cases ----------------------------------------------------
#
# The estimate reads the cases as well as the items: from the item count
# alone, 30 items would give 0.83 seconds at any N, so at 20,000 respondents
# the first run would be labeled under two seconds, run in the session by
# itself for 7 seconds without Cancel, and a requested run would stay in the
# session too. A reference dataset's draws and correlations grow with N, and
# on the saturated FIML route each DMAR call estimates the FIML matrix
# again. These tests compare the estimate with measured runs, not with the
# formula.

pa_big <- function(n, p, seed = 1) {
  withr::with_seed(seed, {
    f <- matrix(stats::rnorm(n * 3), n, 3)
    X <- as.data.frame(sapply(seq_len(p), function(j) 0.6 * f[, (j %% 3) + 1] + 0.8 * stats::rnorm(n)))
    names(X) <- sprintf("item_%02d", seq_len(p))
    X
  })
}

test_that("the wait grows with the respondents: at 20,000 the first run does not run by itself, and the estimate matches measured runs", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  local_measured_rates()
  # At the standard rates (what the run policy decides on): 30 items, 300
  # and 20,000 respondents, 100 reference datasets of each kind.
  small <- sieve_pa_plan(list(ok = TRUE, used = "pearson", missing = "listwise", p = 30L, n = 300L), 100)
  large <- sieve_pa_plan(list(ok = TRUE, used = "pearson", missing = "listwise", p = 30L, n = 20000L), 100)
  expect_true(small$automatic)
  expect_false(large$automatic)
  expect_true(large$background)
  expect_gt(large$compute, 2)
  skip_on_cran()
  skip_unless_timing()
  # Measured. The standard estimate at 20,000 respondents and 20 items is
  # within a factor of three of the run (the machine's speed varies); with
  # the rate a run at 2,000 respondents measured in this process (as the
  # page's later estimates use), the estimates at 301 and at 20,000
  # respondents are within a factor of two of the runs.
  X_large <- pa_big(20000L, 20L)
  b_large <- sieve_retention_cor(X_large, rep("continuous", 20), "pearson")
  first <- sieve_pa_plan(b_large, 30)
  took <- system.time(res <- sieve_parallel_analysis(X_large, b_large, n_iter = 30, seed = 2L))[["elapsed"]]
  expect_true(res$ok)
  expect_lt(abs(log(took / first$compute)), log(3), label = sprintf("log(measured %.2f s / standard estimate %.2f s)", took, first$compute))
  sieve_rate_reset()
  X_mid <- pa_big(2000L, 20L)
  invisible(sieve_parallel_analysis(X_mid, sieve_retention_cor(X_mid, rep("continuous", 20), "pearson"), n_iter = 50, seed = 1L))
  expect_true(exists("pa_dmar_pearson", envir = sieve_rates, inherits = FALSE))
  for (case in list(list(n = 301L, b = 100L), list(n = 20000L, b = 30L))) {
    X <- pa_big(case$n, 20L, seed = 3)
    b <- sieve_retention_cor(X, rep("continuous", 20), "pearson")
    plan <- sieve_pa_plan(b, case$b)
    took <- system.time(res <- sieve_parallel_analysis(X, b, n_iter = case$b, seed = 4L))[["elapsed"]]
    expect_true(res$ok)
    expect_lt(abs(log(took / plan$compute)), log(2),
      label = sprintf("N = %d: log(measured %.2f s / estimate %.2f s)", case$n, took, plan$compute))
  }
})

test_that("on the saturated FIML route the wait includes DMAR's FIML matrix, once for each call", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  X <- pa_big(2000L, 30L)
  withr::with_seed(2, for (j in seq_along(X)) X[sample.int(2000L, 100L), j] <- NA)
  b <- sieve_retention_cor(X, rep("continuous", 30), "pearson", missing = "fiml")
  expect_true(b$ok)
  expect_identical(sieve_pa_route(b)$missing, "fiml")
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  plan <- sieve_pa_plan(b, 20)
  # Both calls estimate the FIML matrix the page's basis timed.
  expect_equal(plan$fixed, 2 * b$seconds, tolerance = 1e-12)
  expect_equal(plan$compute, (40 + 2) * plan$per_dataset + 2 * b$seconds, tolerance = 1e-12)
  expect_match(sieve_pa_wait_sentence(plan),
    "for DMAR to estimate the saturated FIML matrix again, once in each of its two calls", fixed = TRUE)
  skip_on_cran()
  skip_unless_timing()
  took <- system.time(res <- sieve_parallel_analysis(X, b, n_iter = 20, seed = 4L))[["elapsed"]]
  expect_true(res$ok)
  expect_lt(abs(log(took / plan$compute)), log(2), label = sprintf("log(measured %.2f s / estimate %.2f s)", took, plan$compute))
})

test_that("the first view of 20,000 respondents does not hold the session for the parallel analysis", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  n <- 20000L
  p <- 30L
  d <- pa_big(n, p)
  fixture <- list(data = d, item_cols = names(d), theory_map = split(names(d), rep(c("A", "B", "C"), length.out = p)),
    name = "Large N fixture", labels = NULL, load_code = character())
  st <- siever_test_study(fixture, labels = FALSE, theory = FALSE, configure = function(s) {
    m <- s$meta
    m$type <- "continuous"
    s$meta <- m
    s$roles <- NULL
    s$estimator_choice <- "ml"
  })
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  test_frame <- environment()
  seen <- new.env()
  seen$runs <- 0L
  local_mocked_bindings(sieve_parallel_analysis = function(...) {
    seen$runs <- seen$runs + 1L
    stop("not expected in this test")
  }, .env = test_frame)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 100, max_k = 3, seed = 7)
    plan <- pa_plan()
    expect_false(plan$automatic)
    expect_true(plan$background)
    run <- pa_run()
    expect_identical(run$status, "not run")
    expect_match(run$reason, "in a background R process (its start included)", fixed = TRUE)
    expect_false(grepl("under 2 seconds", run$reason, fixed = TRUE))
    expect_identical(seen$runs, 0L)
  })
})

test_that("a real background worker returns the same parallel analysis as the session", {
  skip_on_cran()
  X <- pa_hs9()
  b <- sieve_retention_cor(X, rep("continuous", 9), "pearson")
  rng <- sieve_draw_seed(123L)
  call <- sieve_pa_dmar_calls(X, b, 200L, rng$seed)
  job <- sieve_efa_dmar_job(start = function(call, rng_kind) sieve_efa_dmar_start(call, rng_kind, runner = "sieve_pa_dmar_run"))
  expect_true(sieve_efa_dmar_submit(job, "k", call, seconds = 3, rng_kind = rng$kind))
  # A worker still running at the limit fails here and is stopped, so it
  # does not load the machine for later tests.
  if (!siever_wait_until(function() sieve_efa_dmar_poll(job), "The background parallel analysis",
    step = function() Sys.sleep(0.25))) {
    sieve_efa_dmar_cancel(job, remember = FALSE)
    return(invisible())
  }
  expect_true(job$record$ok)
  bg <- sieve_pa_dmar_result(b, call, job$record, rng, 200L)
  here <- sieve_parallel_analysis(X, b, n_iter = 200L, rng = rng)
  expect_identical(bg$reference$simulated$draws, here$reference$simulated$draws)
  expect_identical(bg$reference$resampled$p95, here$reference$resampled$p95)
  expect_identical(bg$empirical, here$empirical)
})

test_that("the EFA page's number of factors panel reads the Dimensionality page's run beside its reduced eigenvalues", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  st <- siever_test_study(sieve_hs9_fixture(), labels = FALSE)
  shiny::isolate(st$estimator_choice <- "ml")
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log))
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 40, max_k = 3, seed = 21)
    expect_true(pa_run()$ok)
    pa_sel()
  })
  shared <- shiny::isolate(st$cache$pa)
  expect_false(is.null(shared))
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(n_factors = 3L, fm = "ml", rotation = "oblimin", count_to = 4L, count_criterion = "BIC")
    session$setInputs(count_go = 1L)
    efa_dmar_settle(session)
    r <- count_run()
    expect_identical(r$status, "ran")
    call <- log$calls[[length(log$calls)]]
    expect_identical(call$fun, "factor_count")
    expect_identical(call$args$B, 0L)
    shown <- count_display()
    expect_true("reference_quantile" %in% names(shown$data))
    expect_identical(shown$data$reference_quantile, shared$values[1:4])
    expect_match(paste(shown$labels, collapse = " "), "Parallel analysis reference (Dimensionality page, 95th percentile)", fixed = TRUE)
    lines <- pa_text(output$count_lines)
    expect_match(lines, "Parallel analysis (the Dimensionality page's DMAR::parallel_analysis, the same reduction of the same matrix, seed 21)", fixed = TRUE)
    expect_match(lines, "It is not computed a second time here.", fixed = TRUE)
    code <- paste(r$code_view$code, collapse = "\n")
    expect_match(code, "B = 0,", fixed = TRUE)
    expect_match(code, "seed = NULL", fixed = TRUE)
    # Without the shared run the column is absent and the line says where it is.
    cache <- shiny::isolate(state$cache)
    cache$pa <- NULL
    state$cache <- cache
    session$flushReact()
    expect_false("reference_quantile" %in% names(count_display()$data))
    expect_match(pa_text(output$count_lines), "the reference for these reduced eigenvalues is the Dimensionality page's", fixed = TRUE)
  })
})

test_that("minres on the polychoric matrix says the two polychoric estimates differ, and that the estimator in force reads the run", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  # The line must not tell the researcher to set the Dimensionality page's
  # correlations to this route's, which no setting can do when both are
  # polychoric (lavaan's two-step estimate there, psych's here, about 1e-5
  # apart).
  fx <- sieve_hs9_fixture()
  fx$data[fx$item_cols] <- lapply(fx$data[fx$item_cols], function(v) {
    as.integer(cut(v, stats::quantile(v, c(0, .15, .4, .65, .85, 1)), include.lowest = TRUE))
  })
  st <- siever_test_study(fx, labels = FALSE, configure = function(s) {
    m <- s$meta
    m$type <- "ordered"
    s$meta <- m
  })
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 10, max_k = 3, seed = 3)
    expect_identical(pa_run()$status, "ran")
    expect_true(isTRUE(pa_run()$route$ordered))
    pa_sel()
  })
  shared <- shiny::isolate(st$cache$pa)
  expect_identical(shared$basis_used, "polychoric")
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log))
  seen <- new.env()
  for (fm in c("ml", "minres_matrix")) {
    shiny::testServer(mod_efa_server, args = list(state = st), {
      session$setInputs(n_factors = 3L, fm = fm, cor_type = "auto", rotation = "oblimin", estimator = "study", count_to = 3L)
      session$setInputs(count_go = 1L)
      efa_dmar_settle(session)
      expect_identical(count_run()$status, "ran")
      seen[[fm]] <- list(pa = count_pa(), lines = pa_text(output$count_lines), basis = count_run()$basis_used)
    })
  }
  # WLSMV on the item responses reduces DMAR's polychoric matrix: read.
  expect_true(seen$ml$pa$ok)
  # Minres on the page's (psych's) polychoric matrix: not read, and why.
  expect_identical(seen$minres_matrix$basis, "polychoric")
  expect_identical(seen$minres_matrix$pa$why, "estimator")
  expect_gt(seen$minres_matrix$pa$gap, 1e-8)
  expect_lt(seen$minres_matrix$pa$gap, 1e-3)
  expect_match(seen$minres_matrix$lines, "its polychoric correlations are DMAR's two-step estimate from the item responses (lavaan::lavCor)", fixed = TRUE)
  expect_match(seen$minres_matrix$lines, "No setting of the Dimensionality page changes this", fixed = TRUE)
  expect_match(seen$minres_matrix$lines, "the Method \"The estimator below\" (WLSMV on the ordered item responses) reduces DMAR's matrix and reads it", fixed = TRUE)
  expect_false(grepl("set the Dimensionality page's correlations to this route's", seen$minres_matrix$lines, fixed = TRUE))
  # Another matrix of another kind keeps the advice that can be followed.
  table <- data.frame(eigenvalue = shared$empirical[1:3] + 0.1)
  expect_identical(sieve_efa_count_pa(table, shared, shared$revision)$why, "other")
  expect_identical(sieve_efa_count_pa(table, shared, shared$revision, basis_used = "pearson")$why, "other")
  expect_identical(sieve_efa_count_pa(table, shared, shared$revision, basis_used = "polychoric")$why, "estimator")
})

test_that("the guide and the page describe DMAR's parallel analysis, not psych's", {
  # The guide is beside the tests in the source tree and under doc/ in an
  # installed package.
  guide_file <- test_path("..", "..", "vignettes", "user-guide.Rmd")
  if (!file.exists(guide_file)) guide_file <- system.file("doc", "user-guide.Rmd", package = "SIEVER")
  skip_if(!nzchar(guide_file) || !file.exists(guide_file), "the user guide is neither beside the tests nor installed")
  guide <- readLines(guide_file, warn = FALSE)
  text <- paste(guide, collapse = " ")
  expect_match(text, "DMAR::parallel_analysis()", fixed = TRUE)
  expect_false(grepl("as psych::fa.parallel", text, fixed = TRUE))
  help <- pa_text(mod_retention_ui("r"))
  expect_match(help, "DMAR::parallel_analysis()", fixed = TRUE)
  expect_match(help, "Workers (model fit by k)", fixed = TRUE)
  expect_false(grepl("100 reference datasets on 8 workers", help, fixed = TRUE))
})

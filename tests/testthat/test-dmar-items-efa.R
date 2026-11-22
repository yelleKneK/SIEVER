# The Items tab's diagnostics, the Dimensionality page's Very Simple Structure
# and the Optimizer's simple structure on DMAR::fa_exploratory() (DMAR is the
# only EFA engine), the Items tab's reading of the EFA page's solution when
# the two are the same model, and promax and equamax (offered nowhere, since
# DMAR does not offer them). Every expected value is asserted against DMAR
# or psych called directly on the same input, never against SIEVER's own
# output.
#
# Author: Ken Kelley

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("psych")
skip_if_not_installed("shiny")

ie_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}

ie_hs9 <- function() {
  fx <- sieve_hs9_fixture()
  X <- fx$data[, fx$item_cols, drop = FALSE]
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map)
  meta$status <- "active"
  list(X = X, meta = meta, fx = fx)
}
ie_hs24 <- function() {
  fx <- sieve_hs_fixture()
  X <- fx$data[, fx$item_cols, drop = FALSE]
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map)
  meta$status <- "active"
  list(X = X, meta = meta, fx = fx)
}
# HS9 cut into five ordered categories at its quantiles.
ie_ordered <- function(X) {
  out <- as.data.frame(lapply(X, function(v) {
    as.integer(cut(v, stats::quantile(v, c(0, .15, .40, .65, .85, 1)), include.lowest = TRUE))
  }))
  names(out) <- names(X)
  out
}
# Columns of B matched to those of A (permutation and reflection) by the
# largest summed absolute Tucker congruence.
ie_align <- function(A, B, Phi = NULL) {
  k <- ncol(A)
  perms <- function(n) if (n == 1L) matrix(1L) else do.call(rbind, lapply(seq_len(n), function(i) {
    rest <- perms(n - 1L)
    rest[] <- setdiff(seq_len(n), i)[rest]
    cbind(i, rest)
  }))
  C <- crossprod(A, B) / sqrt(outer(colSums(A^2), colSums(B^2)))
  P <- perms(k)
  best <- P[which.max(apply(P, 1, function(p) sum(abs(C[cbind(seq_len(k), p)])))), ]
  s <- sign(C[cbind(seq_len(k), best)])
  list(B = sweep(B[, best, drop = FALSE], 2, s, "*"),
    Phi = if (!is.null(Phi)) Phi[best, best, drop = FALSE] * outer(s, s))
}
# The maximum likelihood discrepancy of a factor solution of R.
ie_fml <- function(R, L, Phi, u) {
  Sigma <- L %*% Phi %*% t(L) + diag(u)
  as.numeric(determinant(Sigma)$modulus + sum(diag(R %*% solve(Sigma))) - determinant(R)$modulus - ncol(R))
}
ie_rng <- function(seed) list(seed = as.integer(seed), supplied = TRUE, kind = RNGkind())

# ---- parity of the Items model with psych::fa ----------------------------------

test_that("parity: the Items model is DMAR's maximum likelihood on the shared matrix, psych::fa's to the optimizers' tolerance at an equal or smaller objective", {
  cases <- list(
    hs9 = c(ie_hs9(), list(requested = "pearson", k = 3L)),
    hs24 = c(ie_hs24(), list(requested = "pearson", k = 5L)),
    poly9 = local({
      h <- ie_hs9()
      h$X <- ie_ordered(h$X)
      h$meta$type <- "ordered"
      c(h, list(requested = "polychoric", k = 3L))
    })
  )
  for (nm in names(cases)) {
    cs <- cases[[nm]]
    b <- sieve_retention_cor(cs$X, rep(if (identical(cs$requested, "pearson")) "continuous" else "ordered", ncol(cs$X)),
      requested = cs$requested)
    expect_identical(b$used, cs$requested, info = nm)
    d <- sieve_item_diagnostics(cs$X, cs$meta, basis = b, rng = ie_rng(7L))
    expect_identical(d$k, cs$k, info = nm)
    expect_identical(d$status$code, "ok", info = nm)
    expect_false(d$solution_reused)
    # DMAR called directly on the same matrix and seed: the same numbers.
    direct <- sieve_efa_dmar_solution(DMAR::fa_exploratory(S = b$R, N = b$n, nfactors = cs$k, estimator = "ML",
      rotation = "oblimin", conf_level = 0.95, seed = 7L))
    items <- d$items$item
    expect_equal(d$items$communality, unname(direct$communality[items]), tolerance = 1e-12, info = nm)
    expect_equal(d$items$complexity, unname(direct$complexity[items]), tolerance = 1e-12, info = nm)
    # Each item's target and largest off-target loading are entries of its
    # row of DMAR's pattern matrix (the target is the factor aligned to its
    # construct, which on the 24 tests is not always its largest loading).
    for (i in seq_along(items)) {
      row <- abs(direct$loadings[items[i], ])
      at <- which.min(abs(row - abs(d$items$loading_target[i])))
      expect_lt(abs(row[at] - abs(d$items$loading_target[i])), 1e-12)
      expect_equal(d$items$loading_off_max[i], max(row[-at]), tolerance = 1e-12, info = paste(nm, items[i]))
    }
    # psych::fa on the same matrix (maximum likelihood, direct oblimin):
    # loadings, communalities and Phi agree within
    # the two optimizers' tolerance (measured: 1.3e-5 to 2.4e-5), and DMAR's
    # solution has the equal or smaller maximum likelihood discrepancy.
    ps <- withr::with_seed(1, psych::fa(b$R, nfactors = cs$k, n.obs = b$n, fm = "ml", rotate = "oblimin"))
    a <- ie_align(unclass(ps$loadings), direct$loadings, direct$Phi)
    expect_lt(max(abs(unclass(ps$loadings) - a$B)), 1e-4)
    expect_lt(max(abs(ps$communality - direct$communality[names(ps$communality)])), 1e-4)
    expect_lt(max(abs(ps$Phi - a$Phi)), 1e-4)
    f_dmar <- ie_fml(b$R, direct$loadings, direct$Phi, 1 - direct$communality)
    f_psych <- ie_fml(b$R, unclass(ps$loadings), ps$Phi, ps$uniquenesses)
    expect_lte(f_dmar, f_psych + 1e-9)
    # psych's reported objective is the same discrepancy.
    expect_equal(f_psych, ps$objective, tolerance = 1e-8)
  }
})

test_that("the one-factor Items model is unrotated, and the seed does not move the global solution", {
  h <- ie_hs9()
  h$meta$construct <- "Ability"
  b <- sieve_retention_cor(h$X, rep("continuous", 9), requested = "pearson")
  d <- sieve_item_diagnostics(h$X, h$meta, basis = b, rng = ie_rng(3L))
  expect_identical(d$k, 1L)
  expect_identical(d$efa_call$args$rotation, "none")
  ps <- psych::fa(b$R, nfactors = 1, n.obs = b$n, fm = "ml")
  expect_lt(max(abs(abs(d$items$loading_target) - abs(unclass(ps$loadings)[, 1]))), 1e-5)
  # Other seeds reach the same rotated solution (lavaan's 30 random starts):
  # on the 24 tests at five factors the loadings move by at most 6e-5
  # (measured), the rotation's convergence tolerance, not another optimum.
  h24 <- ie_hs24()
  b24 <- sieve_retention_cor(h24$X, rep("continuous", 24), requested = "pearson")
  runs <- lapply(c(2L, 77L, 2026L), function(s) sieve_item_diagnostics(h24$X, h24$meta, basis = b24, rng = ie_rng(s)))
  for (r in runs[-1]) expect_lt(max(abs(r$items$loading_target - runs[[1]]$items$loading_target)), 2e-4)
})

# ---- refusals by class ----------------------------------------------------------

test_that("a solution DMAR refuses is named by its class and reason, the loading columns are empty and the reliability columns stay", {
  h <- ie_hs9()
  # Five factors on nine tests: the likelihood has no maximum (DMAR's
  # dmar_no_maximum, a dmar_refusal), where psych holds a uniqueness at 0.005.
  m <- h$meta
  m$construct <- c("A", "A", "B", "C", "C", "D", "E", "E", "D")
  d <- sieve_item_diagnostics(h$X, m, rng = ie_rng(1L))
  expect_identical(d$status$code, "failed")
  expect_identical(d$status$refusal, "dmar_no_maximum")
  expect_identical(d$status$reason, "no maximum")
  expect_match(d$status$message, "^DMAR refused the solution: the likelihood has no maximum \\(reason: no maximum\\)\\.")
  expect_true(all(is.na(d$items$loading_target)))
  expect_true(all(is.na(d$items$communality)))
  expect_false(any(d$items$cross_loading))
  expect_match(paste(d$notes, collapse = " "), "the loading, h2, cross loading and complexity columns are empty", fixed = TRUE)
  ps <- psych::fa(stats::cor(h$X), nfactors = 5, n.obs = 301, fm = "ml", rotate = "oblimin")
  expect_true(any(ps$uniquenesses <= 0.005 + 1e-6))
  # Read by class, never by its words: a refusal of another wording is the
  # same refusal.
  test_frame <- environment()
  local_mocked_bindings(
    fa_exploratory = function(...) {
      stop(structure(class = c("dmar_efa_no_solution", "error", "condition"),
        list(message = "Wording that changed in a later DMAR.", call = NULL, reason = "improper")))
    },
    .package = "DMAR", .env = test_frame
  )
  d2 <- sieve_item_diagnostics(h$X, h$meta, rng = ie_rng(1L))
  expect_identical(d2$status$refusal, "dmar_efa_no_solution")
  expect_match(d2$status$message, "^DMAR refused the solution \\(reason: improper\\)\\. Wording that changed")
  expect_true(all(is.finite(d2$constructs$omega)))
})

# ---- recorded R ------------------------------------------------------------------

test_that("the Items diagnostics' recorded R calls DMAR with every argument and replays the loadings in a fresh R process", {
  skip_if_not_installed("callr")
  h <- ie_hs24()
  b <- sieve_retention_cor(h$X, rep("continuous", 24), requested = "pearson")
  b$ok <- TRUE
  d <- sieve_item_diagnostics(h$X, h$meta, basis = b, rng = ie_rng(19L))
  code <- sieve_item_diagnostics_code(d, b, h$meta$construct, h$meta$item)
  text <- paste(code, collapse = "\n")
  for (arg in c("S = R_items", "N = n_complete", "nfactors = 5", "estimator = \"ML\"", "rotation = \"oblimin\"",
    "conf_level = 0.95", "seed = 19")) {
    expect_match(text, arg, fixed = TRUE)
  }
  expect_match(text, "items_efa <- withr::with_seed(19, .rng_kind = ", fixed = TRUE)
  expect_false(grepl("SIEVER:::|psych::fa", text))
  got <- callr::r(function(code, items) {
    active_items <- items
    eval(parse(text = code))
    list(L = items_loadings, h2 = items_h2, cx = items_complexity)
  }, args = list(code = code, items = h$X))
  expect_equal(unname(got$h2[d$items$item]), d$items$communality, tolerance = 1e-12)
  expect_equal(unname(got$cx[d$items$item]), d$items$complexity, tolerance = 1e-12)
  target <- vapply(seq_len(nrow(d$items)), function(i) {
    row <- got$L[d$items$item[i], ]
    row[which.min(abs(abs(row) - abs(d$items$loading_target[i])))]
  }, numeric(1))
  expect_equal(unname(target), d$items$loading_target, tolerance = 1e-12)
})

# ---- the EFA page's solution, read when it is the same model ---------------------

test_that("the EFA page's solution is the Items model only for maximum likelihood on complete continuous items at the same k with oblimin", {
  st <- siever_test_study(sieve_hs9_fixture())
  pool <- shiny::isolate(sieve_pool_revision(st))
  base_key <- list(dataset_id = pool$dataset, data = pool$data, items = pool$items, types = pool$types,
    rows = pool$rows, k = 3L, fm = "ml", rotation = "oblimin")
  keep <- function(route, rotation = "oblimin", k = 3L, n = 301L) {
    store_state <- sieve_new_state()
    key <- base_key
    key$rotation <- rotation
    key$k <- k
    sieve_efa_store_keep(store_state, key, list(
      fit = list(loadings = matrix(0)), status = list(code = "ok"), basis = NULL, n = n, route = route,
      rng = ie_rng(5L), rotation = rotation, call = list(fun = "fa_exploratory", args = list()), engine = "DMAR"
    ))
    sieve_efa_store(store_state, create = FALSE)
  }
  want <- list(dataset_id = pool$dataset, data = pool$data, items = pool$items, types = pool$types, rows = pool$rows,
    k = 3L, basis_used = "pearson", basis_missing = NULL, n = 301L)
  mlr <- list(form = "data", estimator = "MLR", missing = "fiml", label = "MLR with FIML")
  ml_cc <- list(form = "data", estimator = "ML", missing = "listwise", label = "ML on the complete cases")
  expect_true(sieve_efa_store_match(keep(mlr), want)$usable)
  expect_true(sieve_efa_store_match(keep(ml_cc), want)$usable)
  # FIML with values missing (more rows analyzed than are complete): another model.
  m <- sieve_efa_store_match(keep(mlr, n = 320L), want)
  expect_false(m$usable)
  expect_match(m$differences, "MLR with FIML on 320 respondents there, the Pearson correlations of the 301 complete cases here", fixed = TRUE)
  # The saturated FIML matrix on the Items side: another model.
  expect_false(sieve_efa_store_match(keep(mlr), utils::modifyList(want, list(basis_missing = "fiml")))$usable)
  # WLSMV, minres, another rotation or k: other models, each named.
  wlsmv <- list(form = "data", estimator = "WLSMV", missing = "listwise", label = "WLSMV on the ordered items (polychoric), complete cases")
  m <- sieve_efa_store_match(keep(wlsmv), utils::modifyList(want, list(basis_used = "polychoric")))
  expect_false(m$usable)
  expect_match(m$differences, "WLSMV on the ordered items (polychoric), complete cases there, maximum likelihood on the polychoric correlations of the complete cases here", fixed = TRUE)
  minres <- list(form = "matrix", estimator = "ULS", missing = NA_character_, label = "minimum residual (ULS) on the correlation matrix")
  expect_false(sieve_efa_store_match(keep(minres), want)$usable)
  m <- sieve_efa_store_match(keep(mlr, rotation = "varimax"), want)
  expect_false(m$usable)
  expect_match(m$differences, "varimax (orthogonal, Kaiser row weights) rotation there, direct oblimin here", fixed = TRUE)
  expect_match(sieve_efa_store_match(keep(mlr, k = 2L), want)$differences, "k = 2 there, 3 here", fixed = TRUE)
  # At one factor the rotation does not matter.
  expect_true(sieve_efa_store_match(keep(mlr, rotation = "geomin", k = 1L), utils::modifyList(want, list(k = 1L)))$usable)
})

test_that("the Items tab reads the EFA page's MLR solution of complete items, and its recorded R is the EFA page's call, which replays the loadings", {
  skip_if_not_installed("callr")
  withr::local_options(sieve.cores = 1L)
  st <- siever_test_study(sieve_hs9_fixture())
  shiny::testServer(function(input, output, session) mod_efa_server("efa", st), {
    session$setInputs(nav = "efa", `efa-n_factors` = 3L, `efa-rotation` = "oblimin", `efa-fm` = "ml",
      `efa-cor_type` = "auto", `efa-estimator` = "study",
      `efa-suppress` = 0.3, `efa-sort_loadings` = TRUE)
    expect_false(is.null(output[["efa-efa_method"]]))
  })
  store <- sieve_efa_store(st, create = FALSE)
  expect_identical(store$key$estimator, "ML") # Auto: ML with FIML
  expect_identical(store$key$missing, "fiml")
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    d <- diag()
    expect_true(d$solution_reused)
    expect_identical(d$rng$seed, store$rng$seed)
    # The loadings read are the EFA page's: DMAR's standardized pattern.
    expect_equal(unname(d$items$communality), unname(store$fit$communality[d$items$item]), tolerance = 1e-12)
    # And they are the Items model's: ML on the Pearson correlations of the
    # same complete cases (MLR's estimates are ML's; FIML on complete data
    # is the complete-case likelihood), to the rotation's convergence.
    X <- shiny::isolate(st$items)
    own <- sieve_efa_dmar_solution(DMAR::fa_exploratory(S = stats::cor(X), N = nrow(X), nfactors = 3, estimator = "ML",
      rotation = "oblimin", conf_level = 0.95, seed = 5L))
    expect_lt(max(abs(own$communality[d$items$item] - d$items$communality)), 1e-5)
    log <- shiny::isolate(st$codelog)
    at <- max(grep("Items: exploratory diagnostics", log, fixed = TRUE))
    end <- which(log == "")
    code <- log[seq.int(at + 1L, min(end[end > at]) - 1L)]
    text <- paste(code, collapse = "\n")
    expect_match(text, "The loadings were read from the EFA page's solution", fixed = TRUE)
    expect_match(text, "data = active_items", fixed = TRUE)
    expect_match(text, "estimator = \"ML\"", fixed = TRUE)
    expect_match(text, "missing = \"fiml\"", fixed = TRUE)
    expect_false(grepl("SIEVER:::", text, fixed = TRUE))
    m <- shiny::isolate(st$meta)
    items <- X[, m$item[m$status == "active"], drop = FALSE]
    got <- callr::r(function(code, items) {
      active_items <- items
      eval(parse(text = code))
      items_h2
    }, args = list(code = code, items = items))
    expect_equal(unname(got[d$items$item]), d$items$communality, tolerance = 1e-10)
  })
})

# ---- the wait, and the background R process --------------------------------------

test_that("the Items wait counts DMAR's point-only factor analysis, whose time grows with the items, as measured at 9 to 68 items", {
  # The wait of the call SIEVER makes, point estimates only. With the
  # standard errors the call takes about 0.25 seconds at 9 items and 3
  # factors, 1.0 at 24 and 5, 1.2 at 68 and 1, 9 at 68 and 6; psych::fa
  # takes 0.03, 0.09, 0.05 and 0.16. The point-only call (DMAR 1.1.0 from
  # CRAN, the matrix route) takes 0.15 to 0.23 seconds at 9 items and 3
  # factors, 0.38 to 0.43 at 24 and 5, and at 68 items (n = 600) the times
  # below for 1 to 8 factors. A wait of the fixed and per factor terms alone
  # would give 68 items the wait of 9 (0.09 to 0.30 seconds for 1 to 8
  # factors), and the run rule would start work of 15 seconds unasked.
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  w <- function(p, k) sieve_at_default_rates(sieve_efa_point_wait(p, k))
  measured_68 <- c(1.09, 0.83, 1.32, 1.26, 1.91, 1.82, 2.54, 4.44)
  ratio_68 <- measured_68 / w(68, 1:8)
  expect_true(all(ratio_68 > 1 / 2 & ratio_68 < 2))
  expect_equal(sum(w(68, 1:8)), sum(measured_68), tolerance = 0.1)
  expect_gt(w(68, 6), 2)
  for (cs in list(list(p = 9, k = 3, lo = 0.15, hi = 0.23), list(p = 24, k = 5, lo = 0.38, hi = 0.43))) {
    expect_gt(cs$lo / w(cs$p, cs$k), 1 / 2)
    expect_lt(cs$hi / w(cs$p, cs$k), 2)
    expect_lt(w(cs$p, cs$k), 2)
  }
  # The wait grows with the items at a given number of factors, and with
  # the factors at a given number of items.
  expect_gt(w(68, 3), w(24, 3))
  expect_gt(w(24, 3), w(9, 3))
  expect_true(all(diff(w(24, 1:8)) > 0))
  expect_true(all(c("fixed", "per_factor", "moments_npar") %in% names(sieve_efa_point_rate())))
  h <- ie_hs9()
  plan <- sieve_diagnostics_plan(h$X, h$meta)
  expect_identical(plan$k, 3L)
  expect_equal(plan$efa_seconds, sieve_efa_point_wait(9, 3))
  expect_false(plan$efa_background)
  expect_equal(sieve_diagnostics_plan(h$X, h$meta, efa = FALSE)$efa_seconds, 0)
  skip_on_cran()
  skip_unless_timing()
  # Measured runs within a factor of five of the estimate at the standard
  # rates (the estimate is for an unloaded machine; a session's measured
  # ratio rescales it, and a run of the suite shares the machine): the two
  # bundled sets, and 68 generated items of six factors at two factors.
  X68 <- withr::with_seed(1, {
    L <- matrix(0, 68, 6)
    L[cbind(1:68, (0:67 %% 6) + 1)] <- stats::runif(68, .5, .8)
    X <- matrix(stats::rnorm(600 * 6), 600, 6) %*% t(L) +
      matrix(stats::rnorm(600 * 68), 600, 68) %*% diag(sqrt(1 - rowSums(L^2)))
    colnames(X) <- paste0("i", 1:68)
    X
  })
  for (cs in list(list(X = ie_hs9()$X, k = 3L), list(X = ie_hs24()$X, k = 5L), list(X = X68, k = 2L))) {
    R <- stats::cor(cs$X)
    t <- system.time(sieve_efa_dmar_run(sieve_efa_point_call(cs$k, R = R, n = nrow(cs$X),
      seed = 1L)))[["elapsed"]]
    est <- w(ncol(R), cs$k)
    expect_lt(t / est, 5)
    expect_gt(t / est, 1 / 5)
  }
})

test_that("a solution DMAR is expected to refuse is estimated at a refusal's cost: the bounded screen marks what DMAR refuses", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  h <- ie_hs9()
  R9 <- stats::cor(h$X)
  # The nine tests: DMAR refuses four and five factors (measured: not
  # converged after iterating to lavaan's limit, and no maximum), and the
  # bounded maximum likelihood fit holds a uniqueness at 0.005 there.
  expect_identical(sieve_efa_boundary_ks(R9, 1:5), c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_true(all(sieve_efa_boundary_ks(R9, 6:8)))
  w <- sieve_at_default_rates(sieve_efa_point_wait(9, 4))
  expect_gt(sieve_at_default_rates(sieve_efa_point_wait(9, 4, refused = TRUE)), 5 * w)
  # Excluding t7_sentence leaves t6_paragraph_comprehension improper at three
  # factors: the plan says so and sends the factor analysis to the
  # background; DMAR refuses it, by class.
  m <- h$meta
  m$status[m$item == "t7_sentence"] <- "parked"
  plan <- sieve_diagnostics_plan(h$X, m)
  expect_true(plan$efa_refused)
  expect_true(plan$efa_background)
  expect_false(sieve_diagnostics_plan(h$X, h$meta)$efa_refused)
  d <- sieve_item_diagnostics(h$X, m, rng = ie_rng(1L))
  expect_false(identical(d$status$code, "ok"))
  expect_true(d$status$refusal %in% c("dmar_efa_no_solution", "dmar_no_maximum"))
  expect_true(all(is.na(d$items$loading_target)))
})

test_that("an Items factor analysis estimated above two seconds waits for Update, runs in a background R process with Cancel, and a changed pool stops it", {
  withr::local_options(sieve.cores = 1L)
  st <- siever_test_study(sieve_hs9_fixture())
  log <- new.env(parent = emptyenv())
  # Registered on the test's own frame, before the module block.
  local_mocked_bindings(
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE),
    sieve_efa_point_wait = function(p, k, form = "matrix", refused = FALSE) rep(5, length(k)),
    withProgress = function(expr, ...) expr
  )
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    r <- diag_run()
    expect_identical(r$status, "not run")
    expect_true(r$plan$efa_background)
    expect_length(log$calls, 0L)
    expect_match(ie_text(output$status_line), "Update (", fixed = TRUE)
    session$setInputs(update_go = 1L)
    expect_identical(diag_run()$status, "running")
    expect_length(log$calls, 1L)
    call <- log$calls[[1L]]
    expect_identical(call$fun, "fa_exploratory")
    expect_identical(call$args$estimator, "ML")
    expect_identical(call$args$rotation, "oblimin")
    expect_identical(call$args$nfactors, 3L)
    expect_identical(call$args$N, 301L)
    expect_identical(log$runners, "sieve_efa_dmar_run")
    line <- ie_text(output$status_line)
    expect_match(line, "(DMAR::fa_exploratory, maximum likelihood, k = 3, 9 items) is running in a background R process", fixed = TRUE)
    # The elapsed time is the line's own text output (the line is drawn
    # once per run, test-progress-lines.R).
    expect_match(output$efa_progress, "Elapsed:", fixed = TRUE)
    expect_match(line, "Cancel", fixed = TRUE)
    log$process$alive <- FALSE
    efa_dmar_settle(session)
    d <- diag()
    expect_false(is.null(d))
    expect_identical(d$status$code, "ok")
    expect_true(diag_run()$background)
    # The background record is this tab's own call: the loadings are DMAR's
    # on the same matrix and seed, called directly.
    direct <- sieve_efa_dmar_solution(DMAR::fa_exploratory(S = call$args$S, N = 301L, nfactors = 3L, estimator = "ML",
      rotation = "oblimin", conf_level = 0.95, seed = call$args$seed))
    expect_equal(d$items$communality, unname(direct$communality[d$items$item]), tolerance = 1e-12)
    expect_match(ie_text(output$model_line), "DMAR::fa_exploratory, fit here in a background R process", fixed = TRUE)
    # Cancel keeps nothing; Update asks again.
    sieve_set_item_status(st, "t2_cubes", "parked", "probe")
    session$flushReact()
    session$setInputs(update_go = 2L)
    expect_identical(diag_run()$status, "running")
    session$setInputs(efa_cancel = 1L)
    expect_identical(log$killed, 1L)
    stopped <- diag_run()
    expect_identical(stopped$status, "not run")
    expect_true(stopped$stopped)
    expect_match(ie_text(output$status_line), "Canceled: the exploratory factor analysis was stopped before it finished, and nothing from it is kept.", fixed = TRUE)
    session$setInputs(update_go = 3L)
    expect_identical(diag_run()$status, "running")
    # A changed pool stops a running one; its result is never shown.
    sieve_set_item_status(st, "t4_lozenges", "parked", "probe")
    session$flushReact()
    session$elapse(300)
    expect_identical(log$killed, 2L)
    expect_false(sieve_efa_dmar_active(efa_bg))
  })
})

# ---- Very Simple Structure -------------------------------------------------------

test_that("VSS reads DMAR's solutions: equal to the criterion computed from DMAR called directly, psych's to the optimizers' tolerance", {
  h <- ie_hs24()
  b <- sieve_retention_cor(h$X, rep("continuous", 24), requested = "pearson")
  v <- sieve_retention_vss(b, max_k = 6L, rng = ie_rng(23L))
  expect_true(v$ok)
  expect_length(v$refused, 0L)
  vss_of <- function(L, Phi, R) {
    total <- sum(R^2)
    vapply(1:2, function(cx) {
      if (ncol(L) < cx) return(NA_real_)
      s <- L
      for (j in seq_len(nrow(L))) s[j, -order(abs(L[j, ]), decreasing = TRUE)[seq_len(cx)]] <- 0
      1 - sum((R - s %*% Phi %*% t(s))^2) / total
    }, numeric(1))
  }
  for (k in 1:6) {
    sol <- sieve_efa_dmar_solution(DMAR::fa_exploratory(S = b$R, N = b$n, nfactors = k, estimator = "ML",
      rotation = if (k == 1L) "none" else "oblimin", conf_level = 0.95, seed = 23L))
    ref <- vss_of(sol$loadings, sol$Phi, b$R)
    expect_equal(v$vss1[k], ref[1], tolerance = 1e-12, info = k)
    if (k > 1L) expect_equal(v$vss2[k], ref[2], tolerance = 1e-12, info = k)
    ps <- withr::with_seed(1, psych::fa(b$R, nfactors = k, n.obs = b$n, fm = "ml",
      rotate = if (k == 1L) "none" else "oblimin"))
    pref <- vss_of(unclass(ps$loadings), if (k == 1L) diag(1) else ps$Phi, b$R)
    expect_lt(abs(v$vss1[k] - pref[1]), 1e-4)
  }
})

test_that("a VSS rank DMAR refuses has no fit, named by class with DMAR's reason, and the recorded R replays VSS in a fresh R process", {
  skip_if_not_installed("callr")
  h <- ie_hs9()
  b <- sieve_retention_cor(h$X, rep("continuous", 9), requested = "pearson")
  real <- DMAR::fa_exploratory
  test_frame <- environment()
  local_mocked_bindings(
    fa_exploratory = function(..., nfactors) {
      if (identical(as.integer(nfactors), 2L)) {
        stop(structure(class = c("dmar_efa_no_solution", "error", "condition"),
          list(message = "Any wording.", call = NULL, reason = "improper")))
      }
      real(..., nfactors = nfactors)
    },
    .package = "DMAR", .env = test_frame
  )
  v <- sieve_retention_vss(b, max_k = 3L, rng = ie_rng(31L))
  expect_true(v$ok)
  expect_identical(v$refused, c(`2` = "improper"))
  expect_true(is.na(v$vss1[2]) && is.na(v$vss2[2]))
  expect_true(is.finite(v$vss1[1]) && is.finite(v$vss1[3]))
  rows <- sieve_retention_criteria(b, list(ok = FALSE, status = "failed", reason = "skipped", reference = list()), v)
  expect_match(rows$reason[rows$id == "vss1"], "DMAR refused the solution at k = 2 (improper), so it has no fit to compare.", fixed = TRUE)
  # The recorded R carries the function whole (DMAR and base R alone) and
  # replays VSS in a fresh process, unmocked: every rank is fit there.
  testthat::local_mocked_bindings(fa_exploratory = real, .package = "DMAR", .env = test_frame)
  v <- sieve_retention_vss(b, max_k = 3L, rng = ie_rng(31L))
  code <- sieve_vss_code(v)
  text <- paste(code, collapse = "\n")
  expect_match(text, "DMAR::fa_exploratory(", fixed = TRUE)
  expect_match(text, "vss_out <- withr::with_seed(31, .rng_kind =", fixed = TRUE)
  expect_false(grepl("SIEVER:::|psych::fa", text))
  got <- callr::r(function(code, R, n) {
    R_items <- R
    n_complete <- n
    eval(parse(text = code))
    list(v1 = vss_out$cfit.1, v2 = vss_out$cfit.2, k1 = vss1_k)
  }, args = list(code = code, R = b$R, n = b$n))
  expect_equal(got$v1, v$vss1, tolerance = 1e-12)
  expect_equal(got$v2, v$vss2, tolerance = 1e-12)
})

test_that("VSS estimated above two seconds waits for Run VSS, runs in a background R process with Cancel, and a changed request stops it", {
  st <- siever_test_study(sieve_hs9_fixture(), labels = FALSE)
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE),
    sieve_vss_wait = function(max_k, p, R = NULL) 30
  )
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", pa_reference = "auto", n_iter = 20, max_k = 3, seed = 11)
    v <- vss_run()
    expect_identical(v$status, "not run")
    expect_match(v$reason, "and then run in a background R process", fixed = TRUE)
    session$setInputs(vss_go = 1L)
    expect_true(vss_running())
    vss_calls <- Filter(function(r) identical(r, "sieve_vss_dmar_run"), log$runners)
    expect_length(vss_calls, 1L)
    call <- log$calls[[length(log$calls)]]
    expect_identical(call$max_k, 3L)
    expect_identical(call$seed, 11L)
    status <- ie_text(output$status_line)
    expect_match(status, "Very Simple Structure (DMAR::fa_exploratory at each k) is running in a background R process.", fixed = TRUE)
    # The elapsed time is the line's own text output (the line is drawn
    # once per run, test-progress-lines.R).
    expect_match(output$vss_progress_line, "^Elapsed:")
    expect_match(status, "Cancel", fixed = TRUE)
    log$process$alive <- FALSE
    efa_dmar_settle(session)
    expect_false(vss_running())
    done <- vss_run()
    expect_true(done$ok)
    # The worker's result is the session's computation under the same seed.
    here <- sieve_retention_vss(basis(), max_k = 3L, rng = run_rng())
    expect_equal(done$vss1, here$vss1, tolerance = 1e-12)
    expect_true(any(grepl("vss_out <- withr::with_seed(11,", shiny::isolate(st$codelog), fixed = TRUE)))
    # Cancel keeps nothing.
    session$setInputs(max_k = 2)
    session$setInputs(vss_go = 2L)
    expect_true(vss_running())
    session$setInputs(vss_stop = 1L)
    expect_false(vss_running())
    expect_match(vss_run()$reason, "^Canceled: Very Simple Structure was stopped before it finished, and nothing from it is kept\\.")
    # A changed request stops a running one.
    session$setInputs(vss_go = 3L)
    expect_true(vss_running())
    session$setInputs(seed = 12)
    session$elapse(300)
    expect_false(vss_running())
    expect_identical(vss_run()$status, "not run")
  })
})

# ---- the Optimizer -----------------------------------------------------------------

test_that("the Optimizer's simple structure is DMAR's on the complete rows, psych's to the optimizers' tolerance, and a refusal is its reason", {
  h <- ie_hs9()
  X <- h$X[, -2]
  ss <- sieve_simple_structure(X, 3L, seed = 41L)
  expect_null(ss$reason)
  direct <- sieve_efa_dmar_solution(DMAR::fa_exploratory(data = X, nfactors = 3L, estimator = "ML", missing = "listwise",
    rotation = "oblimin", conf_level = 0.95, seed = 41L))
  expect_equal(ss$complexity, mean(direct$complexity), tolerance = 1e-12)
  expect_equal(ss$hyperplane, mean(abs(direct$loadings) < 0.10), tolerance = 1e-12)
  ps <- withr::with_seed(1, psych::fa(X, nfactors = 3, fm = "ml", rotate = "oblimin"))
  expect_lt(abs(ss$complexity - mean(ps$complexity)), 1e-4)
  expect_identical(ss$hyperplane, mean(abs(unclass(ps$loadings)) < 0.10))
  # With no value missing FIML (DMAR's default) gives the same estimates.
  fiml <- sieve_efa_dmar_solution(DMAR::fa_exploratory(data = X, nfactors = 3L, estimator = "ML", missing = "fiml",
    rotation = "oblimin", conf_level = 0.95, seed = 41L))
  expect_lt(max(abs(fiml$complexity - direct$complexity)), 1e-6)
  # An unidentified count is refused before DMAR, a DMAR refusal by class.
  expect_match(sieve_simple_structure(X[, 1:4], 3L, seed = 1L)$reason, "is not identified with 4 items", fixed = TRUE)
  test_frame <- environment()
  local_mocked_bindings(
    fa_exploratory = function(...) stop(structure(class = c("dmar_no_maximum", "dmar_efa_no_solution", "dmar_refusal", "error", "condition"),
      list(message = "Other words.", call = NULL, reason = "no maximum"))),
    .package = "DMAR", .env = test_frame
  )
  r <- sieve_simple_structure(X, 3L, seed = 41L)
  expect_true(is.na(r$complexity) && is.na(r$hyperplane))
  expect_match(r$reason, "the exploratory factor analysis gave no solution: DMAR refused the solution: the likelihood has no maximum (reason: no maximum).", fixed = TRUE)
  # No package guard: the rotation is lavaan's own.
  body <- paste(deparse(sieve_simple_structure), collapse = "\n")
  expect_false(grepl("GPArotation|psych::", body))
})

# ---- promax and equamax ---------------------------------------------------------

test_that("promax and equamax are offered nowhere; a saved one opens as direct oblimin or varimax with a sentence", {
  expect_false(any(c("promax", "Promax", "equamax") %in% sieve_efa_rotation_choices()))
  pro <- sieve_efa_retired_rotation("promax")
  expect_identical(pro$rotation, "oblimin")
  expect_identical(pro$form, "oblique")
  expect_identical(pro$sentence, paste(
    "The saved settings chose promax rotation (oblique), which SIEVER does not offer, since",
    "DMAR::fa_exploratory() does not; direct oblimin (delta = 0), the standard oblique rotation,",
    "is in its place until you choose another."
  ))
  eq <- sieve_efa_retired_rotation("equamax")
  expect_identical(eq$rotation, "varimax")
  expect_identical(eq$form, "orthogonal")
  expect_match(eq$sentence, "equamax rotation (orthogonal), which SIEVER does not offer, since DMAR::fa_exploratory() does not; varimax, the standard orthogonal rotation, is in its place", fixed = TRUE)
  expect_null(sieve_efa_retired_rotation("oblimin"))
  expect_null(sieve_efa_retired_rotation("varimax"))
  expect_identical(sieve_efa_retired_rotation("bentlerT")$rotation, "varimax")
  expect_identical(sieve_efa_retired_rotation("simplimax")$rotation, "oblimin")
  # A saved study naming equamax opens with varimax and says so; nothing is fit.
  source <- siever_test_study(sieve_hs9_fixture())
  source$efa_settings <- list(n_factors = 3L, fm = "ml", rotation = "equamax")
  restored <- sieve_new_state()
  sieve_project_restore(restored, sieve_project_snapshot(source))
  seen <- new.env(parent = emptyenv())
  seen$updates <- list()
  local_mocked_bindings(
    updateSelectInput = function(session, inputId, ..., selected = NULL) {
      seen$updates[[inputId]] <- selected
      invisible(NULL)
    },
    .package = "shiny"
  )
  shiny::testServer(mod_efa_server, args = list(state = restored), {
    session$flushReact()
    expect_identical(seen$updates$rotation, "varimax")
    expect_match(ie_text(output$rotation_note), "The saved settings chose equamax rotation (orthogonal)", fixed = TRUE)
  })
  # No page offers either: the EFA page's rotation control, and the Items
  # tab and the Optimizer, which offer no rotation choice.
  html <- paste(as.character(app_ui(list())), collapse = "")
  expect_false(grepl("value=\"promax\"|value=\"equamax\"", html))
  guide_path <- testthat::test_path("..", "..", "vignettes", "user-guide.Rmd")
  skip_if_not(file.exists(guide_path), "the guide's source is not beside the tests")
  guide <- gsub("\\s+", " ", paste(readLines(guide_path, warn = FALSE), collapse = " "))
  expect_match(guide, "Promax and equamax are not offered, since `DMAR::fa_exploratory()` does not offer them", fixed = TRUE)
  expect_match(guide, "one that chose equamax with varimax", fixed = TRUE)
})


test_that("the point-estimate solutions ask DMAR for point estimates only", {
  # The Items tab, the Optimizer and Very Simple Structure call
  # DMAR::fa_exploratory() with point_estimates_only = TRUE. DMAR 1.1.0, the
  # version SIEVER requires, takes it, so it is always passed, with no probe.
  expect_true("point_estimates_only" %in% names(formals(DMAR::fa_exploratory)))
  expect_false(exists("sieve_dmar_point_estimates_only", mode = "function"))
  R <- stats::cor(sieve_hs9_fixture()$data[, sieve_hs9_fixture()$item_cols])
  with <- sieve_efa_point_call(3L, R = R, n = 301L, seed = 1L)
  expect_true(with$args$point_estimates_only)
  expect_true(any(grepl("point_estimates_only = TRUE", sieve_efa_dmar_code(with), fixed = TRUE)))
  # The data route asks for the same.
  X <- sieve_hs9_fixture()$data[, sieve_hs9_fixture()$item_cols]
  expect_true(sieve_efa_point_call(2L, data = X, seed = 1L)$args$point_estimates_only)
  # No text says these solutions pay for standard errors they do not read.
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  files <- c(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
    file.path(root, "vignettes", "user-guide.Rmd"))
  text <- gsub("[[:space:]#']+", " ", paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = " "))
  for (stale in c("computes standard errors VSS does not read", "computes standard errors these",
    "computes standard errors the diagnostics do not read", "psych took a tenth of that")) {
    expect_false(grepl(stale, text, fixed = TRUE), info = stale)
  }
})

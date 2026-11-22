# The EFA page fits DMAR::fa_exploratory() (DMAR 1.1.0), and its number of
# factors panel is DMAR::factor_count(). The design: (a) DMAR is the only
# EFA engine; (b) minres maps to estimator = "ULS", ML to "ML", MLR to
# "MLR", ordered items to WLSMV on the raw ordered items, and minres on a
# supplied correlation matrix (S, N) is the matrix route; (c) SIEVER's
# table layout is filled with DMAR's values, with DMAR's intervals beside
# them; (d) the number of factors panel is factor_count(). The parity
# checks against psych::fa() are here, on SIEVER's bundled example.
#
# Author: Ken Kelley

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("lavaan")
skip_if_not_installed("psych")
withr::local_options(sieve.cores = 2L)

d9_items <- function() {
  fx <- sieve_hs9_fixture()
  fx$data[, fx$item_cols, drop = FALSE]
}
d9_state <- function(configure = NULL) {
  siever_test_study(sieve_hs9_fixture(), confirmed = TRUE, configure = configure)
}
dm_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))

# The permutation and reflection of A's columns that best match B's.
dm_align <- function(A, B) {
  k <- ncol(A)
  perms <- as.matrix(expand.grid(rep(list(seq_len(k)), k)))
  perms <- perms[apply(perms, 1L, function(x) !anyDuplicated(x)), , drop = FALSE]
  score <- apply(perms, 1L, function(ix) {
    sum(abs(colSums(A[, ix, drop = FALSE] * B)) / sqrt(colSums(A[, ix, drop = FALSE]^2) * colSums(B^2)))
  })
  ix <- perms[which.max(score), ]
  signs <- sign(colSums(A[, ix, drop = FALSE] * B))
  list(ix = ix, signs = signs, A = sweep(A[, ix, drop = FALSE], 2L, signs, `*`))
}
dm_reproduced <- function(L, Phi) {
  R <- L %*% Phi %*% t(L)
  R[upper.tri(R)]
}

# ---- (b) the estimator mapping ---------------------------------------------------

test_that("the page's settings resolve to DMAR's estimator, missing data method and ordered items", {
  mlr <- list(estimator = "MLR", missing = "fiml", ordered = NULL)
  ml <- list(estimator = "ML", missing = "listwise", ordered = NULL)
  ml_fiml <- list(estimator = "ML", missing = "fiml", ordered = NULL)
  wlsmv <- list(estimator = "WLSMV", missing = "listwise", ordered = c("a", "b"))
  r <- sieve_efa_dmar_route("ml", mlr)
  expect_identical(r[c("form", "estimator", "missing")], list(form = "data", estimator = "MLR", missing = "fiml"))
  expect_identical(sieve_efa_dmar_route("ml", ml)[c("estimator", "missing")], list(estimator = "ML", missing = "listwise"))
  expect_identical(sieve_efa_dmar_route("ml", ml_fiml)[c("estimator", "missing")], list(estimator = "ML", missing = "fiml"))
  w <- sieve_efa_dmar_route("ml", wlsmv)
  expect_identical(w[c("estimator", "missing")], list(estimator = "WLSMV", missing = "listwise"))
  expect_identical(w$ordered, c("a", "b"))
  # Minres is ULS: on the item responses it takes the complete cases (no
  # likelihood, so no FIML) and reads the items as continuous; on a matrix
  # it is the matrix route.
  u <- sieve_efa_dmar_route("minres", wlsmv)
  expect_identical(u[c("form", "estimator", "missing")], list(form = "data", estimator = "ULS", missing = "listwise"))
  expect_null(u$ordered)
  m <- sieve_efa_dmar_route("minres_matrix", mlr)
  expect_identical(m[c("form", "estimator")], list(form = "matrix", estimator = "ULS"))
  # Every method is a common factor method; principal components are not offered.
  expect_false(any(grepl("principal component", tolower(names(sieve_efa_method_choices())))))
  # DMAR's rotations: no promax or equamax.
  expect_false(any(c("promax", "equamax") %in% sieve_efa_rotation_choices()))
  expect_true(all(setdiff(sieve_efa_rotation_choices(), "target") %in%
    c("oblimin", "quartimin", "geomin", "simplimax", "oblimax", "infomax", "bentler", "varimax", "quartimax", "none")))
  # The calls write every argument out.
  call <- sieve_efa_dmar_call(r, 3L, "oblimin", data = d9_items(), seed = 11L)
  expect_identical(call$fun, "fa_exploratory")
  expect_identical(call$args[c("nfactors", "estimator", "missing", "rotation", "conf_level", "seed")],
    list(nfactors = 3L, estimator = "MLR", missing = "fiml", rotation = "oblimin", conf_level = 0.95, seed = 11L))
  matrix_call <- sieve_efa_dmar_call(m, 2L, "oblimin", basis = list(R = diag(3), n = 50L))
  expect_identical(names(matrix_call$args)[1:2], c("S", "N"))
  expect_null(matrix_call$args$missing)
})

# ---- parity checks against psych::fa() -----------------------------------------

test_that("parity: minres with oblimin equals psych::fa(fm = 'minres', rotate = 'oblimin') on the same correlation matrix", {
  X <- d9_items()
  R <- stats::cor(X)
  ps <- withr::with_seed(1, psych::fa(R, nfactors = 3, n.obs = nrow(X), fm = "minres", rotate = "oblimin"))
  for (fit in list(
    DMAR::fa_exploratory(S = R, N = nrow(X), nfactors = 3, estimator = "ULS", rotation = "oblimin", seed = 113),
    DMAR::fa_exploratory(X, nfactors = 3, estimator = "ULS", missing = "listwise", rotation = "oblimin", seed = 113)
  )) {
    s <- sieve_efa_dmar_solution(fit)
    a <- dm_align(s$loadings, unclass(ps$loadings))
    # Measured: 7.1e-6 (communalities), 1.2e-5 (loadings), 4.7e-6
    # (reproduced correlations); the optimizers' tolerances.
    expect_lt(max(abs(s$communality[names(ps$communality)] - ps$communality)), 1e-4)
    expect_lt(max(abs(a$A - unclass(ps$loadings))), 1e-4)
    expect_lt(max(abs(s$Phi[a$ix, a$ix] * outer(a$signs, a$signs) - ps$Phi)), 1e-4)
    expect_lt(max(abs(dm_reproduced(s$loadings, s$Phi) - dm_reproduced(unclass(ps$loadings), ps$Phi))), 1e-4)
  }
})

test_that("parity: ML equals psych::fa(fm = 'ml') after alignment, and MLR has the same point estimates", {
  X <- d9_items()
  R <- stats::cor(X)
  ps <- withr::with_seed(1, psych::fa(R, nfactors = 3, n.obs = nrow(X), fm = "ml", rotate = "oblimin"))
  ml <- sieve_efa_dmar_solution(DMAR::fa_exploratory(X, 3, estimator = "ML", missing = "listwise", seed = 113))
  mlr <- sieve_efa_dmar_solution(DMAR::fa_exploratory(X, 3, estimator = "MLR", missing = "fiml", seed = 113))
  a <- dm_align(ml$loadings, unclass(ps$loadings))
  # Measured: 1.5e-6, 1.3e-5 and 7.7e-7.
  expect_lt(max(abs(ml$communality[names(ps$communality)] - ps$communality)), 1e-4)
  expect_lt(max(abs(a$A - unclass(ps$loadings))), 1e-4)
  expect_lt(max(abs(dm_reproduced(ml$loadings, ml$Phi) - dm_reproduced(unclass(ps$loadings), ps$Phi))), 1e-4)
  expect_equal(mlr$loadings, ml$loadings, tolerance = 1e-6)
  # The table's other columns: DMAR's variance accounted equals psych's
  # SS loadings (pattern times structure), and Hofmann's complexity.
  va <- ml$variance_accounted
  va <- va[va$factor != "total", ]
  expect_lt(max(abs(va$sum_sq_loadings[a$ix] - ps$Vaccounted["SS loadings", ])), 1e-4)
  expect_lt(max(abs(ml$complexity[names(ps$complexity)] - ps$complexity)), 1e-3)
  # Varimax: psych's stats::varimax() stops about 6.5e-4 from the optimum
  # (its eps = 1e-5 on the criterion), DMAR's gradient projection within
  # 1.3e-6 of a tightly converged varimax; measured, not assumed. (The
  # tightly converged reference is stats::varimax() at eps = 1e-12, within
  # 1.8e-7 of GPArotation::GPForth() at eps = 1e-12, measured; GPArotation
  # is not among SIEVER's Suggests.)
  ps0 <- psych::fa(R, 3, n.obs = nrow(X), fm = "ml", rotate = "none")
  tight <- unclass(stats::varimax(unclass(ps0$loadings), normalize = TRUE, eps = 1e-12)$loadings)
  vm <- sieve_efa_dmar_solution(DMAR::fa_exploratory(X, 3, estimator = "ML", missing = "listwise", rotation = "varimax", seed = 113))
  expect_lt(max(abs(dm_align(vm$loadings, tight)$A - tight)), 1e-5)
})

test_that("parity: the polychoric matrix route is minres on the same supplied matrix as psych's", {
  X <- as.data.frame(lapply(d9_items(), function(v) as.integer(cut(v, stats::quantile(v, 0:5 / 5), include.lowest = TRUE))))
  b <- sieve_retention_cor(X, rep("ordered", ncol(X)), requested = "polychoric")
  expect_identical(b$used, "polychoric")
  ps <- withr::with_seed(1, psych::fa(b$R, nfactors = 3, n.obs = b$n, fm = "minres", rotate = "oblimin"))
  s <- sieve_efa_dmar_solution(DMAR::fa_exploratory(S = b$R, N = b$n, nfactors = 3, estimator = "ULS", seed = 113))
  a <- dm_align(s$loadings, unclass(ps$loadings))
  # Measured: 9.2e-7, 1.2e-5 and 6.2e-7.
  expect_lt(max(abs(s$communality[names(ps$communality)] - ps$communality)), 1e-4)
  expect_lt(max(abs(a$A - unclass(ps$loadings))), 1e-4)
  expect_lt(max(abs(dm_reproduced(s$loadings, s$Phi) - dm_reproduced(unclass(ps$loadings), ps$Phi))), 1e-4)
  # A supplied matrix carries no covariance matrix of the correlations:
  # DMAR reports no standard errors, and the page says so.
  expect_true(all(is.na(s$loadings_lower)))
})

test_that("FIML: DMAR's true FIML EFA differs by design from an EFA of the EM-estimated moments", {
  X <- d9_items()
  withr::with_seed(20261106, {
    p <- stats::plogis(-2.2 - 0.8 * as.numeric(scale(X$t7_sentence)))
    for (j in c(1L, 2L, 3L, 7L, 8L, 9L)) X[stats::runif(nrow(X)) < p, j] <- NA
  })
  em <- sieve_retention_cor(X, rep("continuous", 9), requested = "pearson", missing = "fiml")
  old <- withr::with_seed(1, psych::fa(em$R, nfactors = 3, n.obs = em$n, fm = "ml", rotate = "oblimin"))
  new <- sieve_efa_dmar_solution(DMAR::fa_exploratory(X, 3, estimator = "MLR", missing = "fiml", seed = 113))
  # Every respondent contributes to the FIML fit; the EM route reports the
  # complete-case n.
  expect_identical(as.integer(new$method$N), nrow(X))
  expect_lt(em$n, nrow(X))
  a <- dm_align(new$loadings, unclass(old$loadings))
  # Measured: loadings differ by up to 0.016 and communalities by 0.009;
  # the same structure (congruence above .9998).
  expect_gt(max(abs(a$A - unclass(old$loadings))), 1e-3)
  expect_lt(max(abs(a$A - unclass(old$loadings))), 0.05)
})

# ---- the solution DMAR returns, read into SIEVER's tables ----------------------

test_that("DMAR's rows with op complexity, determinacy and H are read, not counted as loadings, and determinacy carries its guardrail", {
  fit <- DMAR::fa_exploratory(d9_items(), 3, estimator = "ML", missing = "listwise", seed = 113)
  tab <- fit
  class(tab) <- "data.frame"
  expect_true(all(c("complexity", "determinacy", "H", "communality") %in% tab$op))
  s <- sieve_efa_dmar_solution(fit)
  expect_identical(dim(s$loadings), c(9L, 3L))
  expect_identical(length(s$complexity), 9L)
  expect_false(anyNA(s$loadings))
  expect_equal(unname(s$complexity), tab$estimate[tab$op == "complexity"])
  q <- sieve_efa_quality_table(s)
  expect_identical(nrow(q), 3L)
  # 2 rho^2 - 1, the lowest correlation of two equally consistent score sets.
  expect_equal(q$Min_score_r, 2 * q$Determinacy^2 - 1, tolerance = 1e-12)
  expect_true(all(q$Determinacy_lower < q$Determinacy & q$Determinacy < q$Determinacy_upper))
  # The determinacy's closed form on DMAR's own estimates.
  Rimp <- s$loadings %*% s$Phi %*% t(s$loadings)
  diag(Rimp) <- 1
  closed <- sqrt(diag(s$Phi %*% t(s$loadings) %*% solve(Rimp) %*% s$loadings %*% s$Phi))
  expect_equal(unname(q$Determinacy), unname(closed), tolerance = 1e-8)
  # The variance accounted: the diagonal of Phi Lambda' Lambda, which adds
  # to the total communality.
  va <- sieve_efa_vaccounted_table(s)
  expect_equal(unname(unlist(va[1L, s$factors])), unname(colSums(s$loadings * s$Structure)), tolerance = 1e-10)
  expect_equal(va$total[1L], sum(s$communality), tolerance = 1e-10)
  # The numeric export keeps every parameter and the intervals, unrounded.
  meta <- sieve_build_meta(d9_items(), names(d9_items()))
  tables <- sieve_efa_numeric_tables(s, meta)
  expect_identical(tables$pattern_lower, s$loadings_lower)
  expect_true(all(c("communality_lower", "communality_upper", "uniqueness_lower") %in% names(tables$items)))
  expect_identical(nrow(tables$parameters), nrow(tab))
  expect_identical(tables$factor_quality, s$factor_quality)
  # The exported factor plot reads DMAR's table directly.
  expect_identical(sieve_efa_plot_coordinates(fit), sieve_efa_plot_coordinates(s))
})

test_that("the fit line reads the fit_measures attribute, from the unrotated fit, so every rotation reports the same fit", {
  X <- d9_items()
  # The study's default for continuous items, DMAR's: ML with FIML.
  oblimin <- sieve_efa_dmar_solution(DMAR::fa_exploratory(X, 3, estimator = "ML", missing = "fiml", rotation = "oblimin", seed = 113))
  varimax <- sieve_efa_dmar_solution(DMAR::fa_exploratory(X, 3, estimator = "ML", missing = "fiml", rotation = "varimax", seed = 113))
  expect_identical(oblimin$fit_measures$value, varimax$fit_measures$value)
  st <- d9_state()
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(n_factors = 3, rotation = "geomin", fm = "ml", estimator = "study", efa_go = 1)
    fm <- efa()$fit_measures
    line <- dm_text(output$efa_fit_line)
    expect_match(line, sprintf("Chi-square = %.3f on 12 df", sieve_efa_fit_value(fm, "chi_square")), fixed = TRUE)
    # The interval is DMAR's at the RMSEA level, 90%, its rmsea_conf_level
    # default, from DMAR's unrotated solution of the same model, computed
    # here independently of the page.
    fm90 <- attr(DMAR::fa_exploratory(X, 3, estimator = "ML", missing = "fiml", rotation = "none", conf_level = 0.95), "fit_measures")
    expect_match(line, sprintf("RMSEA = %.3f, 90%% CI [%.3f, %.3f]", sieve_efa_fit_value(fm, "rmsea"),
      sieve_efa_fit_value(fm90, "rmsea_ci_lower"), sieve_efa_fit_value(fm90, "rmsea_ci_upper")), fixed = TRUE)
    expect_equal(fm$value, oblimin$fit_measures$value, tolerance = 1e-8)
    # DMAR's version lines say which indices they are.
    expect_match(line, "FIML corrected indices of Zhang and Savalei (2023)", fixed = TRUE)
    # Minres: the ULS discrepancy, with no reference distribution.
    session$setInputs(fm = "minres", efa_go = 2)
    expect_match(dm_text(output$efa_fit_line), "no chi-square reference distribution", fixed = TRUE)
  })
})

test_that("the RMSEA interval is DMAR's at the RMSEA level, 90%, beside the coefficients' 95%, from the one solution the page fits", {
  # Every RMSEA interval is at the RMSEA level (90% by default, DMAR's
  # rmsea_conf_level), labeled with its level, apart from the 95% of the
  # coefficients' intervals. The page fits the model once: no second fit
  # of the unrotated model at conf_level = 0.90, and no wait for one.
  # The runs below measure rates; later tests read the standard ones.
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  X <- d9_items()
  ml <- sieve_efa_dmar_route("ml", list(estimator = "ML", missing = "listwise"))
  call <- sieve_efa_dmar_call(ml, 3L, "oblimin", data = X, seed = 7L)
  expect_identical(call$args$conf_level, 0.95)
  expect_identical(call$args$rmsea_conf_level, 0.90)
  expect_null(call$rmsea)
  expect_identical(sieve_efa_count_call(ml, 3L, data = X)$args$conf_level, 0.90)
  # The solution's fit measures, under ML on the complete cases, equal
  # lavaan's own 90% interval of the unrotated model, an independent
  # computation.
  rec <- sieve_efa_dmar_run(call)
  expect_true(rec$ok)
  expect_null(rec$rmsea)
  fl <- lavaan::fitMeasures(lavaan::efa(data = X, nfactors = 3, rotation = "none", estimator = "ML", output = "lavaan"),
    c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper"), fm.args = list(rmsea.ci.level = 0.90))
  fm <- attr(rec$value, "fit_measures")
  expect_equal(sieve_efa_fit_value(fm, "rmsea"), unname(fl[["rmsea"]]), tolerance = 1e-6)
  expect_equal(sieve_efa_fit_value(fm, "rmsea_ci_lower"), unname(fl[["rmsea.ci.lower"]]), tolerance = 1e-6)
  expect_equal(sieve_efa_fit_value(fm, "rmsea_ci_upper"), unname(fl[["rmsea.ci.upper"]]), tolerance = 1e-6)
  st <- d9_state(configure = function(s) s$estimator_choice <- "ml")
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log))
  # Every DMAR::fa_exploratory() call is counted, wherever it runs.
  fa_calls <- new.env(parent = emptyenv())
  fa_calls$n <- 0L
  real_fa <- DMAR::fa_exploratory
  local_mocked_bindings(fa_exploratory = function(...) {
    fa_calls$n <- fa_calls$n + 1L
    real_fa(...)
  }, .package = "DMAR")
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(n_factors = 3, rotation = "oblimin", fm = "ml", estimator = "study", efa_go = 1)
    f <- efa()
    # One DMAR call for the selected solution, no second fit.
    expect_identical(fa_calls$n, 1L)
    line <- dm_text(output$efa_fit_line)
    expect_match(line, sprintf("RMSEA = %.3f, 90%% CI [%.3f, %.3f]", fl[["rmsea"]], fl[["rmsea.ci.lower"]], fl[["rmsea.ci.upper"]]), fixed = TRUE)
    expect_false(grepl("95% CI", line, fixed = TRUE))
    expect_match(dm_text(output$efa_method), "95% intervals (the RMSEA's at 90%).", fixed = TRUE)
    expect_identical(f$conf_level, 0.95)
    expect_identical(sieve_efa_fit_level(f), 0.90)
    # The recorded R is the one call, and it replays the line's interval.
    code <- paste(efa_run()$code_view$code, collapse = "\n")
    expect_false(grepl("efa_fit_rmsea", code, fixed = TRUE))
    expect_match(code, "conf_level = 0.95,", fixed = TRUE)
    expect_match(code, "rmsea_conf_level = 0.9,", fixed = TRUE)
    expect_false(grepl("SIEVER:::", code, fixed = TRUE))
    env <- new.env(parent = globalenv())
    env$active_items <- X
    lines <- efa_run()$code_view$code
    lines <- lines[!grepl("^#", lines)]
    withr::with_preserve_seed(eval(parse(text = lines), envir = env))
    expect_equal(sieve_efa_fit_value(attr(env$efa_fit, "fit_measures"), "rmsea_ci_upper"), unname(fl[["rmsea.ci.upper"]]), tolerance = 1e-6)
    # The count's RMSEA intervals are at the RMSEA level, 90%, and labeled so.
    session$setInputs(count_to = 3, count_criterion = "BIC", count_go = 1)
    efa_dmar_settle(session)
    expect_identical(count_run()$status, "ran")
    expect_identical(log$calls[[length(log$calls)]]$args$conf_level, 0.90)
    expect_true(all(c("RMSEA 90% CI lower", "RMSEA 90% CI upper") %in% count_display()$labels))
    expect_match(dm_text(output$count_lines), "the RMSEA intervals are at 90%, the level of every RMSEA interval SIEVER shows", fixed = TRUE)
  })
})

test_that("auxiliary variables declared for FIML are named as not used by DMAR's FIML route, on the page and in the recorded R", {
  # DMAR::fa_exploratory() and factor_count() take no auxiliary variables,
  # while the matrix route and the parallel analysis use them; the page
  # says so rather than nothing.
  fx <- sieve_hs9_fixture()
  d <- fx$data
  withr::with_seed(9, {
    for (j in fx$item_cols) {
      pr <- stats::plogis(-1.6 + 0.9 * as.numeric(scale(d$age_years)))
      d[[j]][stats::runif(nrow(d)) < pr] <- NA
    }
  })
  fx$data <- d
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  st <- siever_test_study(fx, labels = FALSE)
  roles <- shiny::isolate(st$roles)
  expect_true("age_years" %in% roles$variable[roles$role == "auxiliary"])
  expect_identical(sieve_efa_declared_aux(st, fx$item_cols), "age_years")
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log))
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(n_factors = 3L, fm = "ml", rotation = "oblimin", estimator = "study", efa_go = 1L)
    run <- efa_run()
    expect_identical(run$route$missing, "fiml")
    # DMAR's call reads the items alone (it has no auxiliary argument) ...
    expect_false("age_years" %in% names(run$call$args$data))
    expect_false("aux" %in% names(run$call$args))
    # ... and the page, the overview and the recorded R say so.
    said <- "The auxiliary variable declared for FIML (age_years) is not used: DMAR::fa_exploratory() takes no auxiliary variables"
    expect_match(dm_text(output$efa_method), said, fixed = TRUE)
    expect_match(dm_text(output$efa_overview), said, fixed = TRUE)
    code <- paste(run$code_view$code, collapse = " ")
    expect_match(gsub("\\s*#\\s*", " ", code), "The auxiliary variable declared for FIML (age_years) is not used", fixed = TRUE)
    session$setInputs(count_to = 3, count_criterion = "BIC", count_go = 1)
    efa_dmar_settle(session)
    expect_identical(count_run()$status, "ran")
    expect_match(dm_text(output$count_lines), "(age_years) is not used: DMAR::factor_count() takes no auxiliary variables", fixed = TRUE)
    expect_match(gsub("\\s*#\\s*", " ", paste(count_run()$code_view$code, collapse = " ")), "DMAR::factor_count() takes no auxiliary", fixed = TRUE)
    # The matrix route's saturated FIML matrix includes the auxiliary, and
    # the page does not repeat the sentence there.
    session$setInputs(fm = "minres_matrix", cor_type = "auto", efa_go = 2L)
    r2 <- efa_run()
    expect_identical(r2$basis$aux, "age_years")
    expect_length(r2$aux_ignored, 0L)
    expect_false(grepl("is not used: DMAR::fa_exploratory()", dm_text(output$efa_method), fixed = TRUE))
  })
  # Without missing values in the FIML route's sense (ML on the complete
  # cases), nothing is said.
  st2 <- siever_test_study(fx, labels = FALSE, configure = function(s) s$estimator_choice <- "ml")
  shiny::testServer(mod_efa_server, args = list(state = st2), {
    session$setInputs(n_factors = 3L, fm = "ml", rotation = "oblimin", estimator = "study", efa_go = 1L)
    expect_identical(efa_run()$route$missing, "listwise")
    expect_length(efa_run()$aux_ignored, 0L)
    expect_false(grepl("Auxiliary variables:", dm_text(output$efa_method), fixed = TRUE))
  })
})

test_that("the EFA estimate counts FIML's missing data patterns, so a first view of scattered missing values does not hold the session", {
  # DMAR's FIML (and its robust corrections) works pattern by pattern, so
  # an estimate from the items and cases alone falls short. At 18 items,
  # 300 cases and 10% of each item missing (165 patterns) the MLR solution
  # and its RMSEA call take about 20 seconds where such an estimate says
  # 1.8, and the first view would run them in the session.
  gen <- function(n, p, rate, seed = 1) {
    withr::with_seed(seed, {
      f <- matrix(stats::rnorm(n * 3), n, 3)
      X <- as.data.frame(sapply(seq_len(p), function(j) 0.7 * f[, (j %% 3) + 1] + 0.7 * stats::rnorm(n)))
      names(X) <- sprintf("item_%02d", seq_len(p))
      for (j in seq_len(p)) X[stats::runif(n) < rate, j] <- NA
      X
    })
  }
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  rt <- sieve_efa_dmar_route("ml", list(estimator = "MLR", missing = "fiml"))
  X18 <- gen(300L, 18L, 0.1)
  patterns <- sieve_efa_patterns(X18, rt)
  expect_gt(patterns, 100L)
  expect_identical(sieve_efa_patterns(X18, sieve_efa_dmar_route("ml", list(estimator = "ML", missing = "listwise"))), 0L)
  expect_identical(sieve_efa_patterns(stats::na.omit(X18), rt), 1L)
  estimate <- sieve_efa_dmar_wait(18, "MLR", 300, 3, "data", patterns = patterns)
  expect_gt(estimate, 2)
  # The first view of these data waits for its button, with the estimate.
  fixture <- list(data = X18, item_cols = names(X18), theory_map = split(names(X18), rep(c("A", "B", "C"), 6)),
    name = "Scattered missing values", labels = NULL, load_code = character())
  st <- siever_test_study(fixture, labels = FALSE, theory = FALSE)
  seen <- new.env()
  seen$runs <- 0L
  local({
    # Nothing is fit in the session (the mock is undone when this block ends).
    test_frame <- environment()
    local_mocked_bindings(sieve_efa_dmar_run = function(...) {
      seen$runs <- seen$runs + 1L
      stop("not expected in this test")
    }, .env = test_frame)
    shiny::testServer(mod_efa_server, args = list(state = st), {
      session$setInputs(n_factors = 3L, fm = "ml", rotation = "oblimin", estimator = "study")
      expect_identical(route()$missing, "fiml")
      expect_true(isTRUE(efa_run()$pending))
      expect_gt(primary_seconds(), 2)
      expect_match(dm_text(output$status_line), "Run selected EFA (", fixed = TRUE)
    })
  })
  expect_identical(seen$runs, 0L)
  skip_on_cran()
  skip_unless_timing()
  # Measured: the estimate against a run on 12 items with patterns, and on
  # the same items complete, within a factor of two each.
  X12 <- gen(300L, 12L, 0.1, seed = 2)
  for (X in list(X12, stats::na.omit(gen(300L, 12L, 0, seed = 2)))) {
    call <- sieve_efa_dmar_call(rt, 3L, "oblimin", data = X, seed = 3L)
    took <- system.time(rec <- sieve_efa_dmar_run(call))[["elapsed"]]
    expect_true(rec$ok)
    est <- sieve_efa_dmar_wait(12, "MLR", nrow(X), 3, "data", patterns = sieve_efa_patterns(X, rt))
    expect_lt(abs(log(took / est)), log(2),
      label = sprintf("%d patterns: log(measured %.2f s / estimate %.2f s)", sieve_efa_patterns(X, rt), took, est))
  }
})

# ---- refusals by class, with DMAR's reason ---------------------------------------

test_that("an improper solution and a likelihood without a maximum are refused by class, with DMAR's reason shown", {
  st <- d9_state(configure = function(s) s$estimator_choice <- "ml")
  shiny::testServer(mod_efa_server, args = list(state = st), {
    # Four factors on nine items under MLR: lavaan's post-estimation check fails.
    session$setInputs(n_factors = 4, rotation = "oblimin", fm = "ml", estimator = "mlr", efa_go = 1)
    run <- efa_run()
    expect_null(run$fit)
    expect_identical(run$refusal, "dmar_efa_no_solution")
    expect_identical(run$reason, "improper")
    line <- dm_text(output$status_line)
    expect_match(line, "The EFA was not fit. DMAR refused the solution (reason: improper).", fixed = TRUE)
    expect_identical(shiny::isolate(state$page_results$efa[[session$ns("primary")]]$state), "failed")
    # Five under ML: the likelihood has no maximum (class dmar_no_maximum, a
    # dmar_efa_no_solution and a dmar_refusal), with lavaan's code under it.
    session$setInputs(n_factors = 5, estimator = "ml", efa_go = 2)
    run <- efa_run()
    expect_identical(run$refusal, "dmar_no_maximum")
    expect_identical(run$reason, "no maximum")
    expect_match(dm_text(output$status_line), "the likelihood has no maximum (reason: no maximum)", fixed = TRUE)
    expect_match(dm_text(output$status_line), "DMAR's full message", fixed = TRUE)
    # Nothing is shown as a result.
    expect_error(output$pattern_tbl, class = "shiny.silent.error")
  })
})

test_that("a refusal is read by its class and reason, never by its words", {
  record <- list(ok = FALSE, value = NULL, error = "Wording that DMAR may change.",
    error_class = c("dmar_efa_no_solution", "error", "condition"), reason = "standard errors unavailable",
    warnings = character(0))
  status <- sieve_efa_dmar_status(record)
  expect_identical(status$code, "failed")
  expect_identical(status$refusal, "dmar_efa_no_solution")
  expect_match(status$message, "DMAR refused the solution (reason: standard errors unavailable).", fixed = TRUE)
  record$reason <- "not converged"
  expect_identical(sieve_efa_dmar_status(record)$code, "not_converged")
  for (cls in c("dmar_target_unidentified", "dmar_target_invalid", "dmar_items_invalid", "dmar_h1_unavailable")) {
    record$error_class <- c(cls, "dmar_refusal", "error", "condition")
    record$reason <- "x"
    expect_identical(sieve_efa_dmar_status(record)$refusal, cls)
  }
  # The runner keeps the class and the reason a DMAR refusal carries.
  run <- sieve_efa_dmar_run(list(fun = "fa_exploratory",
    args = list(data = d9_items(), nfactors = 2, rotation = "target", target = list(a = "t1_visual_perception"))))
  expect_false(run$ok)
  expect_true("dmar_refusal" %in% run$error_class)
  expect_true(is.character(run$reason))
})

# ---- target rotation -----------------------------------------------------------

test_that("target rotation rotates toward each construct's items, and its identification refusals are shown", {
  st <- d9_state()
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(n_factors = 3, rotation = "target", fm = "ml", estimator = "study", efa_go = 1)
    run <- efa_run()
    expect_false(is.null(run$fit))
    target <- run$call$args$target
    expect_identical(names(target), unique(shiny::isolate(state$meta)$construct))
    code <- paste(run$code_view$code, collapse = "\n")
    expect_match(code, 'rotation = "target",', fixed = TRUE)
    expect_match(code, "target = list(", fixed = TRUE)
    expect_match(dm_text(output$rotation_note), "one per construct", fixed = TRUE)
    # Two factors against three constructs: DMAR's target refusal, with its reason.
    session$setInputs(n_factors = 2, efa_go = 2)
    run <- efa_run()
    expect_identical(run$refusal, "dmar_target_invalid")
    expect_identical(run$reason, "nfactors")
    expect_match(dm_text(output$status_line), "DMAR refused the target rotation (reason: nfactors).", fixed = TRUE)
  })
})

# ---- the number of factors panel (decision d) --------------------------------------

test_that("the number of factors panel is DMAR::factor_count, without a second parallel analysis, and chooses nothing", {
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log))
  st <- d9_state()
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(n_factors = 3, rotation = "oblimin", fm = "ml", estimator = "study",
      count_to = 0, count_criterion = "BIC", efa_go = 1)
    # A count the items cannot carry is refused, never replaced.
    expect_identical(count_run()$status, "refused")
    expect_match(dm_text(output$count_note), "no other count was substituted", fixed = TRUE)
    session$setInputs(count_to = 5)
    expect_identical(count_run()$status, "not run")
    expect_length(log$calls, 0L)
    session$setInputs(count_go = 1)
    efa_dmar_settle(session)
    r <- count_run()
    expect_identical(r$status, "ran")
    call <- log$calls[[1L]]
    expect_identical(call$fun, "factor_count")
    # B = 0. The reference
    # factor_count() carries is DMAR::parallel_analysis()'s normal reference
    # (the same draws under the same seed), which the Dimensionality page
    # runs; the application shows one parallel analysis, not two, and this
    # panel reads that page's run (test-dmar-parallel.R). Nothing in the
    # comparison is random, so no seed is drawn.
    expect_identical(call$args[c("max_factors", "estimator", "missing", "criterion", "B", "percentile")],
      list(max_factors = 5L, estimator = "ML", missing = "fiml", criterion = "BIC", B = 0L, percentile = 0.95))
    expect_null(call$args$seed)
    t <- r$table
    expect_true(all(c("eigenvalue", "map", "bic", "criterion_weight") %in% names(t)))
    expect_true(all(is.na(t$reference_quantile)))
    # Counts without a proper solution are rows, with DMAR's reasons.
    expect_identical(t$factors, 1:5)
    expect_true(all(t$solution[4:5] != "proper"))
    lines <- dm_text(output$count_lines)
    expect_match(lines, "No proper solution: k = 4", fixed = TRUE)
    expect_match(lines, "Parallel analysis: the reference for these reduced eigenvalues is the Dimensionality page's", fixed = TRUE)
    expect_match(lines, "it has not run for these data", fixed = TRUE)
    expect_match(lines, "No cutoff is applied and no count is chosen", fixed = TRUE)
    # The same comparison as DMAR's own call.
    direct <- DMAR::factor_count(d9_items(), max_factors = 5, estimator = "MLR", missing = "fiml", B = 0)
    expect_equal(t$eigenvalue, direct$eigenvalue, tolerance = 1e-12)
    expect_equal(t$bic, direct$bic, tolerance = 1e-10)
    # The selected count's row is the selected solution's fit.
    expect_equal(t$chi_square[3], sieve_efa_fit_value(efa()$fit_measures, "chi_square"), tolerance = 1e-8)
    expect_match(dm_text(output$count_code), "DMAR::factor_count(", fixed = TRUE)
  })
})

# ---- the background contract on a real worker -------------------------------------

test_that("a real background worker runs a DMAR call and returns its record, refusals with their class", {
  skip_on_cran()
  skip_if_not_installed("callr")
  call <- sieve_efa_dmar_call(sieve_efa_dmar_route("ml", list(estimator = "ML", missing = "listwise")),
    3L, "oblimin", data = d9_items(), seed = 113L)
  p <- sieve_efa_dmar_start(call, RNGkind())
  withr::defer(if (p$is_alive()) p$kill())
  expect_false(identical(p$process$get_pid(), Sys.getpid()))
  siever_wait_until(function() !p$is_alive(), "The DMAR worker", step = function() Sys.sleep(.1))
  record <- p$get_result()
  expect_true(record$ok)
  expect_equal(sieve_efa_dmar_solution(record$value)$loadings,
    sieve_efa_dmar_solution(do.call(DMAR::fa_exploratory, call$args))$loadings, tolerance = 1e-12)
  bad <- call
  bad$args$nfactors <- 5L
  q <- sieve_efa_dmar_start(bad, RNGkind())
  withr::defer(if (q$is_alive()) q$kill())
  siever_wait_until(function() !q$is_alive(), "The refused DMAR worker", step = function() Sys.sleep(.1))
  refused <- q$get_result()
  expect_false(refused$ok)
  expect_true(all(c("dmar_no_maximum", "dmar_efa_no_solution", "dmar_refusal") %in% refused$error_class))
  expect_identical(refused$reason, "no maximum")
})

# ---- what the page says, and what it keeps ----------------------------------------

test_that("the page names DMAR in its method line, guide text and code, and calls no SIEVER internals", {
  st <- d9_state()
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(n_factors = 3, rotation = "oblimin", fm = "ml", estimator = "study", efa_go = 1)
    expect_match(dm_text(output$efa_method), "Model: DMAR::fa_exploratory, ML with FIML, direct oblimin rotation, k = 3", fixed = TRUE)
    overview <- dm_text(output$efa_overview)
    expect_match(overview, "full information maximum likelihood", fixed = TRUE)
    expect_match(overview, "differ from that route's by design", fixed = TRUE)
    expect_match(dm_text(output$quality_note), "2 rho^2 - 1", fixed = TRUE)
    expect_false(grepl("psych::fa", dm_text(output$efa_overview), fixed = TRUE))
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_false(grepl("SIEVER:::", log, fixed = TRUE))
    expect_false(grepl("psych::fa(", log, fixed = TRUE))
    # The Items tab's model is DMAR::fa_exploratory() too, maximum
    # likelihood on the Pearson correlations of the complete cases, so this
    # solution, MLR with FIML on the complete nine tests at k = 3 with
    # direct oblimin, is that model and the store says it is usable; at
    # another k the store names the difference. test-dmar-items-efa.R
    # checks every route.
    store <- sieve_efa_store(state, create = FALSE)
    expect_identical(store$key$engine, "DMAR")
    want <- list(
      dataset_id = store$key$dataset_id, data = store$key$data, items = store$key$items,
      types = store$key$types, rows = store$key$rows, k = 3L, basis_used = "pearson", n = 301L
    )
    expect_true(sieve_efa_store_match(store, want)$usable)
    want$k <- 2L
    m <- sieve_efa_store_match(store, want)
    expect_false(m$usable)
    expect_identical(m$differences, "k = 3 there, 2 here")
  })
  html <- paste(as.character(mod_efa_ui("efa")), collapse = "")
  expect_match(html, "DMAR::fa_exploratory fits the common-factor model", fixed = TRUE)
  expect_match(html, "Determinacy and H", fixed = TRUE)
  expect_match(html, "Number of factors", fixed = TRUE)
  # Promax and equamax are not offered, and the page says so.
  expect_false(any(grepl("promax|equamax", c(names(sieve_efa_rotation_choices()), sieve_efa_rotation_choices()), ignore.case = TRUE)))
  expect_match(html, "Promax and equamax are not offered, since DMAR::fa_exploratory() does not offer them", fixed = TRUE)
  # The user guide, where the source tree is beside the tests.
  guide_path <- testthat::test_path("..", "..", "vignettes", "user-guide.Rmd")
  guide <- if (file.exists(guide_path)) readLines(guide_path, warn = FALSE) else character(0)
  if (length(guide)) {
    efa <- paste(guide[seq(grep("^## EFA$", guide), grep("^## Items$", guide))], collapse = " ")
    expect_match(efa, "DMAR::fa_exploratory()", fixed = TRUE)
    expect_match(efa, "DMAR::factor_count()", fixed = TRUE)
    # psych::fa() is named only beside DMAR's solution, never as the page's fit.
    expect_false(grepl("fits the exploratory factor analysis with `psych::fa()`", efa, fixed = TRUE))
    expect_match(efa, "DMAR is the page's only engine", fixed = TRUE)
  }
})

test_that("the numeric download keeps DMAR's values unrounded, with their limits, the full parameter table and the fit measures", {
  st <- d9_state()
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(n_factors = 3, rotation = "oblimin", fm = "ml", estimator = "study", efa_go = 1)
    path <- output$dl_numeric
    expect_true(file.exists(path))
    dir <- withr::local_tempdir()
    utils::untar(path, exdir = dir)
    files <- list.files(dir)
    expect_true(all(c("pattern.csv", "pattern_lower.csv", "pattern_upper.csv", "phi_lower.csv", "parameters.csv",
      "factor_quality.csv", "variance_accounted.csv", "fit_measures.csv", "efa_results.rds", "manifest.R") %in% files))
    result <- readRDS(file.path(dir, "efa_results.rds"))
    expect_identical(result$tables$pattern, efa()$loadings)
    expect_identical(result$tables$pattern_lower, efa()$loadings_lower)
    expect_identical(result$manifest$seed, efa_run()$rng$seed)
    expect_match(result$manifest$engine, "DMAR 1.1.0, fa_exploratory()", fixed = TRUE)
    # The manifest records the level of the fit measures' RMSEA interval,
    # the RMSEA level, 90%; there is no second, 90% fit and no table of
    # its own for it.
    expect_false("fit_measures_rmsea.csv" %in% files)
    expect_identical(result$manifest$rmsea_conf_level, 0.90)
    expect_null(result$tables$fit_measures_rmsea)
    # No lavaan object travels in the export.
    expect_false(any(vapply(result$tables, function(x) inherits(x, "lavaan"), logical(1))))
  })
})

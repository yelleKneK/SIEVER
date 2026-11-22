# Factor scores: an independent oracle for the two estimators, and the
# algebra behind the guidance the interface gives about using scores in a
# later regression.

test_that("Bartlett and regression scores equal their matrix formulas", {
  skip_if_not_installed("lavaan")
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols[1:6]]
  meta <- sieve_build_meta(d, fx$item_cols[1:6], fx$theory_map[1:2], labels = fx$labels[1:6])
  # Complete data and plain ML, so the textbook formulas apply exactly.
  model <- sieve_cfa_plan(meta)$model
  fit <- lavaan::cfa(model, data = d, std.lv = TRUE, meanstructure = TRUE)

  L <- lavaan::lavInspect(fit, "est")$lambda
  Th <- lavaan::lavInspect(fit, "est")$theta
  Ph <- lavaan::lavInspect(fit, "est")$psi
  nu <- as.numeric(lavaan::lavInspect(fit, "est")$nu)
  X <- sweep(as.matrix(d[, rownames(L)]), 2, nu)
  Th_inv <- solve(Th)
  # Bartlett (1937): generalized least squares estimate of the factor given
  # the items, (L' Th^-1 L)^-1 L' Th^-1 (x - nu).
  bart <- X %*% Th_inv %*% L %*% solve(t(L) %*% Th_inv %*% L)
  # Regression (Thomson, 1934; Thurstone, 1935): the posterior mean,
  # Ph L' Sigma^-1 (x - nu).
  Sigma <- L %*% Ph %*% t(L) + Th
  regr <- X %*% solve(Sigma) %*% L %*% Ph

  got_b <- as.matrix(sieve_factor_scores(fit, method = "Bartlett"))
  got_r <- as.matrix(sieve_factor_scores(fit, method = "regression"))
  expect_equal(unname(got_b), unname(bart), tolerance = 1e-8)
  expect_equal(unname(got_r), unname(regr), tolerance = 1e-8)
  # The two estimators are different quantities, not a rounding of one
  # another: regression scores are shrunken toward the mean.
  expect_true(all(apply(got_r, 2, stats::var) < apply(got_b, 2, stats::var)))
})

test_that("which score recovers a regression slope depends on the score's role", {
  # Population algebra for a one factor model with k items, common loading
  # lambda and residual variance theta, factor variance 1, and an outcome
  # y = beta * f + e. No sampling is involved: these are the probability
  # limits of the regression slopes, so the comparison isolates the
  # estimators from estimation noise. The case: four loadings of .6,
  # residual variances .64, beta = .8.
  k <- 4
  lambda <- 0.6
  theta <- 0.64
  beta <- 0.8
  info <- k * lambda^2 / theta # L' Th^-1 L

  # Bartlett score: f_B = f + u with Var(u) = 1 / info, so it is unbiased
  # for each person's f (E[f_B | f] = f) but has variance 1 + 1 / info.
  var_bartlett <- 1 + 1 / info
  # Regression score: f_R = (info / (1 + info)) * f_B, the posterior mean.
  shrink <- info / (1 + info)
  var_regression <- shrink^2 * var_bartlett
  cov_f_regression <- shrink # Cov(f, f_R)

  # Score as the PREDICTOR of y.
  slope_y_on_bartlett <- beta * 1 / var_bartlett
  slope_y_on_regression <- beta * cov_f_regression / var_regression
  expect_equal(slope_y_on_bartlett, beta / (1 + 1 / info), tolerance = 1e-14)
  expect_equal(slope_y_on_bartlett, 0.553846153846154, tolerance = 1e-12)
  expect_equal(slope_y_on_regression, beta, tolerance = 1e-14)
  # A simulation with n = 100,000 gives .55219 and .79761, which are these
  # limits within sampling error.
  expect_lt(abs(slope_y_on_bartlett - 0.55219), 0.005)
  expect_lt(abs(slope_y_on_regression - 0.79761), 0.005)

  # Score as the OUTCOME, regressed on an error free predictor x with
  # f = gamma * x + d, Var(x) = 1.
  gamma <- 0.5
  slope_bartlett_on_x <- gamma # unbiased score: slope kept
  slope_regression_on_x <- shrink * gamma # shrunken score: slope attenuated
  expect_equal(slope_bartlett_on_x, gamma, tolerance = 1e-14)
  expect_lt(slope_regression_on_x, gamma)
  expect_equal(slope_regression_on_x, gamma * info / (1 + info), tolerance = 1e-14)
  # So neither score is the right choice in general: conditional
  # unbiasedness of a person's score (Bartlett) is a different property
  # from consistency of a slope estimated from scores (Skrondal & Laake,
  # 2001; Devlieger, Mayer, & Rosseel, 2016).
})

test_that("the score guidance does not recommend Bartlett scores as predictors", {
  skip_if_not_installed("lavaan")
  fx <- sieve_hs9_fixture()
  state <- sieve_new_state()
  shiny::isolate({
    state$raw <- fx$data
    state$items <- fx$data[, fx$item_cols]
    state$id_vars <- fx$id_vars
    state$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
    state$confirmed <- TRUE
  })
  shiny::testServer(mod_reliab_server, args = list(state = state), {
    for (method in c("Bartlett", "regression")) {
      session$setInputs(score_method = method)
      note <- paste(as.character(unlist(output$scores_note)), collapse = " ")
      # The note does not call Bartlett scores the choice for predictors or
      # say they avoid scoring error, for either method.
      expect_false(grepl("preferred choice when scores are used as predictors", note, fixed = TRUE))
      expect_false(grepl("avoids scoring error entirely", note, fixed = TRUE))
      # What the algebra above establishes is said instead.
      expect_match(note, "attenuates the slope")
      expect_match(note, "Skrondal")
      expect_match(note, "Devlieger")
    }
  })
})


test_that("the factor-score preview displays all 15 promised rows without a pager", {
  skip_if_no_browser()
  siever_with_app(load_example = TRUE, function(app) {
    siever_open_tab(app, "reliab")
    compat_open_subtab(app, "reliab", "Factor scores (latent)")
    if (isTRUE(app$get_js("!!document.getElementById('reliab-scores_go')"))) {
      app$click("reliab-scores_go")
    }
    app$wait_for_js("document.querySelectorAll('#reliab-scores_tbl tbody tr').length === 15",
      timeout = 120 * 1000)
    expect_identical(siever_n_table_rows(app, "reliab-scores_tbl"), 15L)
    expect_match(app$get_html("#reliab-scores_tbl"), "First 15 rows shown", fixed = TRUE)
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

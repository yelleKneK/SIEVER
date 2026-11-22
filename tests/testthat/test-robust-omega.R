# Robust omega as DMAR defines it, everywhere SIEVER shows reliability:
# its label and denominator, the categorical denominator, the explanation of
# what it is robust to, the largest one-factor residual beside it, the
# optional difference from model-implied omega, and its bootstrap
# intervals.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

omega_one_factor <- function(n, loadings, seed, prefix = "x") {
  X <- withr::with_seed(seed, {
    f <- stats::rnorm(n)
    sapply(loadings, function(l) l * f + sqrt(1 - l^2) * stats::rnorm(n))
  })
  X <- as.data.frame(X)
  names(X) <- paste0(prefix, seq_along(loadings))
  X
}

omega_state <- function(n = 300, seed = 20261021) {
  A <- omega_one_factor(n, c(.8, .7, .65, .6, .55), seed = seed, prefix = "a")
  B <- omega_one_factor(n, c(.75, .7, .6, .6, .5), seed = seed + 1L, prefix = "b")
  d <- cbind(A, B)
  fixture <- list(data = d, item_cols = names(d), theory_map = list(Alpha = names(A), Beta = names(B)),
    name = "Omega fixture", labels = NULL, load_code = character())
  s <- siever_test_study(fixture, labels = FALSE, theory = FALSE, name = NULL,
    configure = function(s) s$roles <- NULL)
  s
}

omega_value <- function(r, term) r$value[r$term == term]

test_that("every reliability value is labeled Robust omega (observed denominator) or Omega (model-implied denominator)", {
  its <- paste0("x", 1:4)
  robust <- "Robust omega (observed denominator)"
  implied <- "Omega (model-implied denominator)"
  ordinal <- "Robust omega (observed denominator: unrestricted polychoric correlations)"
  expect_identical(sieve_reliability_spec(its, rep("continuous", 4))$label, robust)
  expect_identical(sieve_reliability_spec(its, rep("continuous", 4), denominator = "model_implied")$label, implied)
  expect_identical(sieve_reliability_spec(its, rep("ordered", 4))$label, ordinal)
  expect_identical(
    sieve_reliability_spec(its, c("continuous", "continuous", "ordered", "ordered"))$label,
    "Robust omega (observed denominator; item scores as continuous)"
  )
  # An inadmissible estimate is named with its label, not as an unqualified omega.
  expect_match(sieve_reliability_outside(1.02, "omega_robust"), "^The estimate, Robust omega \\(observed denominator\\) = 1\\.02, is above 1")

  # The Reliability tab: the coefficient column, the value column, the
  # model-implied companion, the footnote, the method paragraph, the plot and
  # the recorded R.
  st <- omega_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "construct", treatment = "study", missing = "listwise",
      denominator = "observed", ci_method = "recommended", conf_level = 0.95,
      B = 40, boot_seed = 7, show_alpha = FALSE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    df <- reliab()
    expect_identical(df$coefficient, rep(robust, 2))
    tbl <- paste(as.character(unlist(output$omega_tbl)), collapse = " ")
    expect_match(tbl, "Estimate", fixed = TRUE)
    # (The compact table: the model-implied companion is one of the columns
    # Show all columns adds, its interval in one column.)
    session$setInputs(show_all = TRUE)
    tbl <- paste(as.character(unlist(output$omega_tbl)), collapse = " ")
    expect_match(tbl, implied, fixed = TRUE)
    expect_match(tbl, "Model-implied 95% CI", fixed = TRUE)
    expect_false(grepl("model implied; assumes", tbl, fixed = TRUE))
    foot <- as.character(output$footnote$html)
    expect_match(foot, "Robust omega (observed denominator), the default, or Omega (model-implied denominator)", fixed = TRUE)
    notes <- as.character(output$status_notes$html)
    expect_match(notes, "Robust omega (observed denominator) 0.", fixed = TRUE)
    expect_match(notes, "Beside it, Omega (model-implied denominator) 0.", fixed = TRUE)
    log <- paste(shiny::isolate(st$codelog), collapse = "\n")
    expect_match(log, "# Alpha: Robust omega (observed denominator), complete cases, point estimate.", fixed = TRUE)
    # DMAR's default for model-implied omega, its logit scale interval.
    expect_match(log, "# Alpha: Omega (model-implied denominator), complete cases, MLR (sandwich) SE, logit-scale limits (delta method) interval.", fixed = TRUE)
    expect_false(grepl("model implied; assumes", log, fixed = TRUE))
  })
  ui <- paste(as.character(mod_reliab_ui("reliab")), collapse = " ")
  expect_match(ui, sprintf("<option value=\"observed\" selected>%s</option>", robust), fixed = TRUE)
  expect_match(ui, sprintf("<option value=\"model_implied\">%s</option>", implied), fixed = TRUE)

  # The CV split names each half's coefficient the same way.
  d <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  cv <- sieve_cross_validate(d, meta, holdout = .5, seed = 113)
  expect_setequal(unique(cv$rel$Coefficient), c(robust, implied))
  expect_match(paste(as.character(mod_finalize_ui("final")), collapse = " "),
    "named in the Coefficient column: Robust omega (observed ", fixed = TRUE)

  # The Bifactor tab: the raw total score's robust omega, and the indices'
  # omega, whose denominator is the one the bifactor model reproduces.
  total <- sieve_total_omega(d, meta)
  expect_identical(total$label, robust)
  expect_match(sieve_total_omega_label(total), "^Robust omega \\(observed denominator\\) = 0\\.[0-9]{2} \\(point estimate; ")
  bif <- paste(as.character(mod_bifactor_ui("bif")), collapse = " ")
  expect_match(bif, "Omega (model-implied denominator)</b>", fixed = TRUE)

  # The CFA tab's model-linked composite names its observed denominator, and
  # says it is not a construct's own robust omega.
  fit <- lavaan::cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5", data = d, std.lv = TRUE)
  comp <- sieve_model_composite(fit)
  expect_identical(unique(comp$table$coefficient), "Model-linked composite reliability (observed denominator)")
  expect_match(comp$method, "not the Robust omega (observed denominator) of its own one-factor model", fixed = TRUE)
  O <- as.data.frame(lapply(d, function(v) as.integer(cut(v, 5))))
  fo <- lavaan::cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5", data = O,
    ordered = names(O), std.lv = TRUE)
  expect_identical(
    unique(sieve_model_composite(fo)$table$coefficient),
    "Model-linked composite reliability (observed denominator: unrestricted polychoric correlations)"
  )
  panel <- paste(as.character(sieve_cfa_evidence_ui(shiny::NS("cfa"))), collapse = " ")
  expect_match(panel, "labeled Model-linked composite reliability (observed denominator)", fixed = TRUE)
})

# Ordered items with a doublet (y1, y2), so the one-factor model misfits and
# the two categorical denominators differ.
omega_ordinal_doublet <- function(n = 500, seed = 20261021) {
  withr::with_seed(seed, {
    f <- stats::rnorm(n)
    s <- stats::rnorm(n)
    lam <- c(.7, .7, .6, .6, .5)
    Y <- sapply(seq_along(lam), function(j) {
      shared <- if (j <= 2) .45 else 0
      lam[j] * f + shared * s + sqrt(1 - lam[j]^2 - shared^2) * stats::rnorm(n)
    })
  })
  X <- as.data.frame(apply(Y, 2, function(v) as.integer(cut(v, c(-Inf, -1, -.3, .3, 1, Inf)))))
  names(X) <- paste0("y", seq_len(ncol(X)))
  X
}

# Green and Yang's (2009) coefficient by hand: the covariance of two category
# scores at latent correlation r, summed over item pairs, at the common-factor
# correlations (numerator) and at a denominator matrix.
omega_gy_by_hand <- function(thresholds, num, den) {
  # The bivariate normal probability P(X <= a, Y <= b) at correlation r,
  # in base R (no mvtnorm): the integral over x of the density times the
  # conditional probability that Y <= b given x (Y | x is normal with mean
  # r x and variance 1 - r^2).
  p2 <- function(a, b, r) {
    if (abs(r - 1) < 1e-12) return(stats::pnorm(min(a, b)))
    if (abs(r + 1) < 1e-12) return(max(0, stats::pnorm(a) + stats::pnorm(b) - 1))
    s <- sqrt(1 - r^2)
    stats::integrate(
      function(x) stats::dnorm(x) * stats::pnorm((b - r * x) / s),
      lower = -Inf, upper = a, rel.tol = 1e-10, abs.tol = 1e-12, subdivisions = 500L
    )$value
  }
  cov_at <- function(t1, t2, r) {
    sum(outer(seq_along(t1), seq_along(t2), Vectorize(function(c, d) p2(t1[c], t2[d], r)))) -
      sum(stats::pnorm(t1)) * sum(stats::pnorm(t2))
  }
  J <- length(thresholds)
  top <- 0
  bottom <- 0
  for (j in seq_len(J)) for (k in seq_len(J)) {
    top <- top + cov_at(thresholds[[j]], thresholds[[k]], num[j, k])
    bottom <- bottom + cov_at(thresholds[[j]], thresholds[[k]], den[j, k])
  }
  top / bottom
}

omega_fake_categorical <- function(estimate, calls) {
  function(data, ci_method = "none", ...) {
    calls$args <- c(list(ci_method = ci_method), list(...))
    value <- data.frame(
      term = c("estimate", "se", "ci_lower", "ci_upper", "conf_level", "N"),
      value = c(estimate, NA, NA, NA, 0.95, nrow(stats::na.omit(data))),
      stringsAsFactors = FALSE
    )
    attr(value, "ci_method") <- ci_method
    value
  }
}

test_that("the installed categorical omega is robust omega from the unrestricted polychoric correlations, checked by hand", {
  X <- omega_ordinal_doublet()
  fit <- lavaan::cfa("f =~ y1 + y2 + y3 + y4 + y5", data = X, ordered = names(X), std.lv = TRUE)
  lambda <- lavaan::lavInspect(fit, "est")$lambda[names(X), 1]
  tau <- lavaan::lavInspect(fit, "est")$tau
  thresholds <- split(as.numeric(tau), sub("[|].*$", "", rownames(tau)))[names(X)]
  unrestricted <- lavaan::lavInspect(fit, "sampstat")$cov[names(X), names(X)]
  implied <- lavaan::lavInspect(fit, "implied")$cov[names(X), names(X)]
  observed_by_hand <- omega_gy_by_hand(thresholds, outer(lambda, lambda), unrestricted)
  implied_by_hand <- omega_gy_by_hand(thresholds, outer(lambda, lambda), implied)
  # The two definitions differ on these data.
  expect_gt(abs(observed_by_hand - implied_by_hand), 1e-3)
  sp <- sieve_reliability_spec(names(X), rep("ordered", 5), construct = "C", ci_method = "none")
  expect_identical(sp$denominator, "observed")
  expect_identical(sp$label, "Robust omega (observed denominator: unrestricted polychoric correlations)")
  res <- sieve_reliability_omega(X, sp)
  # SIEVER's value is the observed (unrestricted polychoric) definition.
  expect_equal(res$estimate, observed_by_hand, tolerance = 1e-6)
  # DMAR's model-implied denominator is the other definition.
  direct <- DMAR::reliability_omega_categorical(X, ci_method = "none", denominator = "model_implied")
  expect_equal(omega_value(direct, "estimate"), implied_by_hand, tolerance = 1e-6)
})

test_that("the categorical denominator is passed, run and recorded", {
  X <- omega_ordinal_doublet(n = 200)
  its <- names(X)
  calls <- new.env()
  # DMAR 1.1.0 takes the argument, and SIEVER passes it with no probe and
  # no fallback to the observed denominator.
  expect_false(exists("sieve_dmar_categorical_denominator", mode = "function"))
  testthat::local_mocked_bindings(
    sieve_reliability_engine = function(fun) omega_fake_categorical(.8, calls)
  )
  obs <- sieve_reliability_spec(its, rep("ordered", 5), construct = "C", ci_method = "none", denominator = "observed")
  expect_identical(sieve_reliability_call(obs)$args$denominator, "observed")
  r <- sieve_reliability_omega(X, obs)
  expect_identical(calls$args$denominator, "observed")
  expect_match(paste(r$code, collapse = " "), "denominator = \"observed\"", fixed = TRUE)
  mi <- sieve_reliability_spec(its, rep("ordered", 5), construct = "C", ci_method = "none", denominator = "model_implied")
  expect_identical(mi$denominator, "model_implied")
  expect_identical(mi$label, "Omega (model-implied denominator: polychoric correlations implied by the one-factor model)")
  expect_identical(sieve_reliability_call(mi)$args$denominator, "model_implied")
  # Beside robust omega of ordered items stands the model-implied companion.
  companion <- sieve_reliability_model_implied(X, obs)
  expect_identical(companion$spec$denominator, "model_implied")
  expect_identical(calls$args$denominator, "model_implied")
  expect_identical(companion$label, mi$label)
  # A bootstrap passes the denominator to the engine's BCa as well.
  boot <- sieve_reliability_spec(its, rep("ordered", 5), construct = "C", ci_method = "dmar_bca", B = 200)
  expect_identical(sieve_reliability_call(boot)$args[c("ci_method", "denominator", "B")],
    list(ci_method = "bca", denominator = "observed", B = 200L))
  # No note speaks of an engine without the argument.
  expect_false(any(grepl("observed denominator only", mi$notes, fixed = TRUE)))
})

test_that("the explanation says what robust omega is robust to, and what it is not", {
  txt <- sieve_reliability_variant_text()
  expect_match(txt, "Robust means robust to misspecification of the total variance only", fixed = TRUE)
  expect_match(txt, "it is not robustness to outliers (Zhang & Yuan, 2016, use \"robust omega\" in that sense)", fixed = TRUE)
  expect_match(txt, "and not robust ML standard errors", fixed = TRUE)
  expect_match(txt, "a doublet or a method factor among a few same-signed items inflates omega under either denominator", fixed = TRUE)
  expect_match(txt, "Under a correct model the two estimate the same parameter", fixed = TRUE)
  expect_match(txt, "differ by exactly 1'(S - Σ̂)1, the sum of the residual covariances", fixed = TRUE)
  expect_match(txt, "not the bifactor omega hierarchical of Zinbarg et al., 2005", fixed = TRUE)
  ui <- paste(as.character(mod_reliab_ui("reliab")), collapse = " ")
  expect_match(ui, "What is robust is the denominator, and only against misspecification of the total variance", fixed = TRUE)
  expect_match(ui, "not robustness to outliers (Zhang &amp; Yuan, 2016", fixed = TRUE)
  expect_match(ui, "Under a correct model the two estimate the same parameter", fixed = TRUE)
  expect_match(ui, "(S - \u03a3\u0302)1, the sum of the residual covariances", fixed = TRUE)
  expect_match(ui, "inflates omega either way", fixed = TRUE)
  expect_match(ui, "built from the unrestricted polychoric correlations and the thresholds (Kelley &amp; Pornprasertmanit, 2016), which is not the literal variance of the sum score", fixed = TRUE)
  expect_false(grepl("the two coincide in the population when the model is correct", ui, fixed = TRUE))
})

test_that("the Reliability tab's help names H's standard error as its table does: MLR (sandwich), not robust ML", {
  # The table labels H's interval "MLR (sandwich) SE, logit-scale limits",
  # so that "robust" on the tab means only robust omega's observed
  # denominator; the H paragraph and the missing-data panel say the same.
  expect_match(sieve_reliability_H_label("mlr_logistic"), "MLR (sandwich) SE", fixed = TRUE)
  ui <- paste(as.character(mod_reliab_ui("reliab")), collapse = " ")
  expect_match(ui, "with the MLR (sandwich) standard error for continuous items (the fit's own robust standard errors for ordered items)", fixed = TRUE)
  expect_match(ui, "With FIML the ML and MLR (sandwich) standard errors come from the observed information matrix", fixed = TRUE)
  expect_false(grepl("robust ML standard error for continuous items", ui, fixed = TRUE))
  expect_false(grepl("ML and robust ML standard errors", ui, fixed = TRUE))
})

# Six continuous items with a doublet between x1 and x2.
omega_doublet <- function(n = 400, seed = 7) {
  withr::with_seed(seed, {
    f <- stats::rnorm(n)
    s <- stats::rnorm(n)
    lam <- c(.6, .6, .65, .6, .55, .5)
    X <- sapply(seq_along(lam), function(j) {
      shared <- if (j <= 2) .5 else 0
      lam[j] * f + shared * s + sqrt(1 - lam[j]^2 - shared^2) * stats::rnorm(n)
    })
  })
  X <- as.data.frame(X)
  names(X) <- paste0("x", seq_len(ncol(X)))
  X
}

# Residual correlations of an independently specified one-factor ML fit:
# the observed minus the model-implied correlations.
omega_residual_by_hand <- function(X, ...) {
  fit <- lavaan::cfa(sprintf("f =~ %s", paste(names(X), collapse = " + ")), data = X, std.lv = TRUE, ...)
  S <- lavaan::lavInspect(fit, "sampstat")$cov[names(X), names(X)]
  Sigma <- lavaan::lavInspect(fit, "implied")$cov[names(X), names(X)]
  list(fit = fit, R = stats::cov2cor(S) - stats::cov2cor(Sigma), S = S, Sigma = Sigma)
}

# The off-diagonal element largest in absolute value, and its pair.
omega_largest <- function(R) {
  at <- which(upper.tri(R), arr.ind = TRUE)
  k <- which.max(abs(R[at]))
  list(value = unname(R[at][k]), pair = rownames(R)[at[k, ]])
}

test_that("the largest single-factor residual correlation and its item pair stand beside robust omega", {
  X <- omega_doublet()
  spec <- sieve_reliability_spec(names(X), rep("continuous", 6), construct = "D", ci_method = "none")
  r <- sieve_reliability_construct(X, spec, alpha = FALSE)
  hand <- omega_residual_by_hand(X)
  expect_equal(r$residuals$largest, hand$R["x1", "x2"], tolerance = 1e-6)
  expect_identical(r$residuals$items, c("x1", "x2"))
  expect_equal(unname(r$residuals$matrix), unname(hand$R), tolerance = 1e-6)
  dm <- attr(DMAR::reliability_omega(X, ci_method = "none", denominator = "observed"), "residual_cor")
  if (is.null(dm)) {
    expect_match(r$residuals$source, "^SIEVER, from the one-factor lavaan fit")
  } else {
    # DMAR supplies it: SIEVER shows DMAR's, and its own computation agrees.
    expect_match(r$residuals$source, "^DMAR")
    expect_equal(r$residuals$largest, dm$largest, tolerance = 1e-8)
    expect_equal(unname(dm$matrix), unname(hand$R), tolerance = 1e-6)
  }
  # SIEVER's own fit and the model behind H give the same answer.
  own <- sieve_reliability_residuals(X, spec)
  expect_equal(own$largest, hand$R["x1", "x2"], tolerance = 1e-6)
  # The table, the paragraph, and the recorded R.
  tb <- sieve_reliability_table(list(D = r))
  expect_equal(tb$resid_largest, hand$R["x1", "x2"], tolerance = 1e-6)
  expect_identical(tb$resid_pair, "x1 with x2")
  p <- sieve_reliability_paragraph(r)
  expect_match(p, sprintf("Largest residual correlation of the one-factor model: %.3f, x1 with x2", hand$R["x1", "x2"]), fixed = TRUE)
  expect_match(p, "signals redundancy, not better measurement", fixed = TRUE)
  expect_match(p, "does not certify unidimensionality", fixed = TRUE)
  env <- new.env()
  env$items <- X
  eval(parse(text = c(sieve_reliability_H_helper_code(), r$H$code)), envir = env)
  expect_equal(unname(env$residual_cor_D["x1", "x2"]), hand$R["x1", "x2"], tolerance = 1e-6)

  # When the engine supplies the summary, it is the one shown.
  given <- list(matrix = hand$R, largest = .5, items = c("x3", "x4"))
  shown <- sieve_reliability_residuals(X, spec, given = given)
  expect_identical(shown$largest, .5)
  expect_identical(shown$items, c("x3", "x4"))
  expect_match(shown$source, "^DMAR")

  # Under FIML, the saturated FIML (EM) moments, as DMAR's fit uses.
  Xm <- X
  Xm$x3[1:40] <- NA
  Xm$x5[30:70] <- NA
  fspec <- sieve_reliability_spec(names(X), rep("continuous", 6), construct = "D", missing = "fiml", ci_method = "none")
  rf <- sieve_reliability_construct(Xm, fspec, alpha = FALSE)
  hf <- lavaan::cfa(sprintf("f =~ %s", paste(names(X), collapse = " + ")), data = Xm, std.lv = TRUE, missing = "ml")
  Rf <- as.matrix(lavaan::lavResiduals(hf, type = "cor.bollen", summary = FALSE)$cov)
  expect_equal(rf$residuals$largest, omega_largest(Rf)$value, tolerance = 1e-6)
  expect_identical(rf$residuals$items, omega_largest(Rf)$pair)

  # Ordered items: the unrestricted polychoric minus the model-implied correlations.
  O <- as.data.frame(lapply(X, function(v) as.integer(cut(v, 5))))
  ospec <- sieve_reliability_spec(names(O), rep("ordered", 6), construct = "O", ci_method = "none")
  ro <- sieve_reliability_construct(O, ospec, alpha = FALSE)
  fo <- lavaan::cfa(sprintf("f =~ %s", paste(names(O), collapse = " + ")), data = O, ordered = names(O), std.lv = TRUE)
  Ro <- lavaan::lavInspect(fo, "sampstat")$cov - lavaan::lavInspect(fo, "implied")$cov
  at <- which(upper.tri(Ro), arr.ind = TRUE)
  k <- which.max(abs(Ro[at]))
  expect_equal(ro$residuals$largest, Ro[at][k], tolerance = 1e-6)
  expect_match(ro$residuals$scale, "unrestricted polychoric correlations minus", fixed = TRUE)

  # A construct mixing continuous and ordered items: its own linear fit,
  # since the model behind H treats the ordered items as ordered.
  M <- X
  M$x5 <- O$x5
  M$x6 <- O$x6
  mspec <- sieve_reliability_spec(names(M), c(rep("continuous", 4), "ordered", "ordered"), construct = "M", ci_method = "none")
  rm <- sieve_reliability_construct(M, mspec, alpha = FALSE)
  expect_equal(rm$residuals$largest, omega_residual_by_hand(M)$R["x1", "x2"], tolerance = 1e-6)
  envm <- new.env()
  envm$items <- M
  eval(parse(text = c(sieve_reliability_H_helper_code(), rm$H$code)), envir = envm)
  expect_equal(unname(envm$residual_cor_M["x1", "x2"]), rm$residuals$largest, tolerance = 1e-6)

  # Three items: saturated, so the residuals say nothing.
  three <- sieve_reliability_construct(X[, 1:3], sieve_reliability_spec(names(X)[1:3], rep("continuous", 3),
    construct = "T", ci_method = "none"), alpha = FALSE)
  expect_true(three$residuals$saturated)
  expect_identical(sieve_reliability_table(list(T = three))$resid_pair, "not interpreted: three-item residual diagnostics")
  expect_match(sieve_reliability_paragraph(three), "Three-item residual diagnostics are not interpreted here", fixed = TRUE)
})

test_that("facets, the CV split, the bifactor total and the Reliability tab carry the residual, and facet reliability is robust omega of the facet's own items", {
  X <- omega_doublet()
  meta <- data.frame(item = names(X), label = names(X), construct = "Whole",
    facet = rep(c("First", "Second"), each = 3L), type = "continuous", status = "active",
    reverse = FALSE, stringsAsFactors = FALSE)
  groups <- sieve_reliability_items(meta, names(X), scope = "facet")
  expect_length(groups, 2L)
  for (g in groups) {
    sp <- sieve_reliability_spec(g$items, g$types, construct = g$construct, ci_method = "none",
      group_id = g$id, scope = g$scope, parent_construct = g$parent_construct, facet = g$facet)
    r <- sieve_reliability_construct(X, sp, alpha = FALSE)
    # The facet's robust omega is DMAR::reliability_omega() on its own items.
    # The oracle states the spec's missing data request, as SIEVER does
    # (FIML, DMAR's default): left unset on complete data,
    # DMAR takes its complete-data path, which differs in the last bit.
    direct <- DMAR::reliability_omega(X[, g$items], ci_method = "none", denominator = "observed",
      missing = sp$missing)
    expect_identical(r$omega$estimate, omega_value(direct, "estimate"))
    expect_identical(r$omega$label, "Robust omega (observed denominator)")
    expect_true(r$residuals$saturated)
  }
  # The CV split: each half's residual beside its coefficient.
  cv <- sieve_cross_validate(X, transform(meta, facet = NA_character_), holdout = .5, seed = 113)
  target <- cv$rel[cv$rel$Coefficient == "Robust omega (observed denominator)", ]
  dc <- X[cv$calibration_rows, ]
  hc <- omega_largest(omega_residual_by_hand(dc)$R)
  expect_identical(target$Calibration.residual, sprintf("%.3f (%s with %s)", hc$value, hc$pair[1], hc$pair[2]))
  expect_match(target$Validation.residual, "^0\\.[0-9]{3} \\(x[0-9] with x[0-9]\\)$")
  # The bifactor tab's total.
  total <- sieve_total_omega(X, transform(meta, facet = NA_character_))
  expect_match(sieve_total_omega_label(total),
    sprintf("largest residual correlation of the one-factor model of every item: %.2f (x1 with x2)",
      omega_residual_by_hand(X)$R["x1", "x2"]), fixed = TRUE)
  # The Reliability tab.
  st <- omega_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "construct", treatment = "study", missing = "listwise",
      denominator = "observed", ci_method = "recommended", conf_level = 0.95,
      B = 40, boot_seed = 7, show_alpha = FALSE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    df <- reliab()
    expect_true(all(is.finite(df$resid_largest)))
    tbl <- paste(as.character(unlist(output$omega_tbl)), collapse = " ")
    # (The residual and its pair are one column of the compact table, under
    # a short header, and the paragraph on reading them is in "More on how
    # to read this table", footnote_more.)
    expect_match(tbl, "Largest one-factor residual r (item pair)", fixed = TRUE)
    foot <- as.character(output$footnote_more$html)
    expect_match(foot, "signals redundancy, not better measurement", fixed = TRUE)
    expect_match(foot, "the single factor absorbs it and the misfit appears among the other items", fixed = TRUE)
  })
})

test_that("robust minus model-implied omega is an optional column, and the denominators differ by exactly the summed residual covariance", {
  X <- omega_doublet()
  spec <- sieve_reliability_spec(names(X), rep("continuous", 6), construct = "D", ci_method = "none")
  r <- sieve_reliability_construct(X, spec, alpha = FALSE)
  tb <- sieve_reliability_table(list(D = r))
  rob <- DMAR::reliability_omega(X, ci_method = "none", denominator = "observed")
  imp <- DMAR::reliability_omega(X, ci_method = "none", denominator = "model_implied")
  expect_equal(tb$omega_minus_model, omega_value(rob, "estimate") - omega_value(imp, "estimate"), tolerance = 1e-12)
  expect_lt(tb$omega_minus_model, 0)
  # The identity, by hand from an independent one-factor ML fit: robust omega
  # is (sum lambda)^2 / 1'S1, model-implied omega (sum lambda)^2 / 1'Sigma_hat 1,
  # and 1'S1 - 1'Sigma_hat 1 is the summed residual covariance.
  hand <- omega_residual_by_hand(X)
  lambda <- lavaan::lavInspect(hand$fit, "est")$lambda[names(X), 1]
  d <- sum(hand$S - hand$Sigma)
  expect_equal(tb$sum_residual_cov, d, tolerance = 1e-8)
  expect_equal(omega_value(rob, "estimate"), sum(lambda)^2 / sum(hand$S), tolerance = 1e-6)
  expect_equal(omega_value(imp, "estimate"), sum(lambda)^2 / (sum(hand$S) - d), tolerance = 1e-6)
  p <- sieve_reliability_paragraph(r)
  expect_match(p, sprintf("Robust minus model-implied omega is %.3f", tb$omega_minus_model), fixed = TRUE)
  expect_match(p, sprintf("1'(S - Σ̂)1 = %.3f", d), fixed = TRUE)
  # Not computed where the coefficient is model-implied omega itself.
  mi_spec <- sieve_reliability_spec(names(X), rep("continuous", 6), construct = "D", ci_method = "mlr", denominator = "model_implied")
  expect_true(is.na(sieve_reliability_table(list(D = sieve_reliability_construct(X, mi_spec, alpha = FALSE)))$omega_minus_model))
  # On the tab it is optional: off by default, shown when asked for.
  ui <- paste(as.character(mod_reliab_ui("reliab")), collapse = " ")
  expect_match(ui, "Show the denominator diagnostic: robust minus model-implied omega", fixed = TRUE)
  expect_false(grepl("id=\"reliab-show_difference\" type=\"checkbox\" class=\"shiny-input-checkbox\" checked", ui, fixed = TRUE))
  st <- omega_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "construct", treatment = "study", missing = "listwise",
      denominator = "observed", ci_method = "recommended", conf_level = 0.95,
      B = 40, boot_seed = 7, show_alpha = FALSE, show_difference = FALSE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    tbl <- paste(as.character(unlist(output$omega_tbl)), collapse = " ")
    expect_false(grepl("Robust minus model-implied omega", tbl, fixed = TRUE))
    session$setInputs(show_difference = TRUE)
    tbl <- paste(as.character(unlist(output$omega_tbl)), collapse = " ")
    expect_match(tbl, "Robust minus model-implied omega", fixed = TRUE)
    expect_match(tbl, "Summed residual covariance", fixed = TRUE)
  })
})

test_that("intervals are bootstraps on request with DMAR's default of B = 10000, the wait stated first", {
  expect_identical(formals(sieve_reliability_spec)$B, 10000L)
  its <- paste0("x", 1:4)
  pending <- sieve_reliability_spec(its, rep("continuous", 4), run_bootstrap = FALSE)
  expect_identical(pending$B, 10000L)
  expect_identical(pending$uncertainty$status, "pending")
  expect_match(pending$uncertainty$reason, "Bootstrap percentile, 10000 replications", fixed = TRUE)
  engine <- sieve_reliability_spec(its, rep("continuous", 4), ci_method = "dmar_bca")
  expect_identical(sieve_reliability_call(engine)$args$B, 10000L)
  ordered <- sieve_reliability_spec(its, rep("ordered", 4))
  expect_identical(ordered$ci_method, "dmar_bca")
  expect_identical(sieve_reliability_call(ordered)$args$B, 10000L)
  ui <- paste(as.character(mod_reliab_ui("reliab")), collapse = " ")
  expect_match(ui, "id=\"reliab-B\" type=\"number\" class=\"shiny-input-number form-control\" value=\"10000\"", fixed = TRUE)
  # The default is visible, while an invalid edited count is refused
  # instead of silently replacing it. The wait is stated before the run.
  st <- omega_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "construct", treatment = "study", missing = "listwise",
      denominator = "observed", ci_method = "recommended", conf_level = 0.95,
      B = NA, boot_seed = 7, show_alpha = FALSE, score_method = "Bartlett"
    )
    expect_error(settings(), "positive whole number", fixed = TRUE)
    session$setInputs(B = 10000L)
    expect_identical(settings()$B, 10000L)
    session$setInputs(run_go = 1)
    expect_true(all(reliab()$uncertainty == "pending"))
    expect_match(reliab()$interval[1], "10000 replications", fixed = TRUE)
    note <- as.character(output$boot_note$html)
    expect_match(note, "with 10000 replications", fixed = TRUE)
    expect_match(note, "should take", fixed = TRUE)
    expect_false(isTRUE(boot_running()))
  })
})

test_that("alpha keeps its own analytic interval while the omega bootstrap waits, in the function and on the Reliability tab", {
  X <- omega_doublet()
  its <- names(X)
  pending <- sieve_reliability_spec(its, rep("continuous", 6), construct = "D", ci_method = "recommended",
    run_bootstrap = FALSE, missing = "listwise")
  expect_identical(pending$uncertainty$status, "pending")
  r <- sieve_reliability_construct(X, pending, alpha = TRUE)
  expect_identical(r$alpha$uncertainty$status, "completed")
  # DMAR's default interval for analytic alpha on complete cases, Bonett's
  # (reliability_alpha(ci_method = NULL), DMAR's defaults), equal to DMAR
  # called directly.
  expect_identical(r$alpha$uncertainty$label, "Bonett (analytic alpha)")
  direct_alpha <- DMAR::reliability_alpha(X, missing = "listwise")
  expect_equal(c(r$alpha$estimate, r$alpha$lower, r$alpha$upper),
    c(omega_value(direct_alpha, "estimate"), omega_value(direct_alpha, "ci_lower"), omega_value(direct_alpha, "ci_upper")),
    tolerance = 1e-12)
  # Under FIML, the ML delta interval.
  fiml <- sieve_reliability_construct(X, sieve_reliability_spec(its, rep("continuous", 6), construct = "D",
    ci_method = "recommended", run_bootstrap = FALSE, missing = "fiml"), alpha = TRUE)
  expect_identical(fiml$alpha$uncertainty$status, "completed")
  expect_identical(fiml$alpha$uncertainty$label, "ML (delta method; analytic alpha)")
  # The Reliability tab agrees: before the bootstrap is run alpha shows its
  # analytic interval.
  st <- omega_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "construct", treatment = "study", missing = "listwise",
      denominator = "observed", ci_method = "recommended", conf_level = 0.95,
      B = 40, boot_seed = 7, show_alpha = TRUE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    df <- reliab()
    expect_true(all(df$uncertainty == "pending"))
    expect_true(all(df$alpha_uncertainty == "completed"))
    # DMAR's default on complete cases (Bonett), as above.
    expect_true(all(df$alpha_interval == "Bonett (analytic alpha)"))
    expect_true(all(is.finite(df$alpha_lower) & is.finite(df$alpha_upper)))
  })
})

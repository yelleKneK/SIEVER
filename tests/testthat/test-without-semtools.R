# Monte Carlo effect intervals, model-linked composite reliability, ordered
# partial invariance syntax, and longitudinal invariance syntax, computed
# with lavaan and base R and without semTools. Every test below makes
# semTools unavailable to SIEVER and checks the result against hand-written
# lavaan syntax, a closed form computed by hand, or reference values
# computed by semTools and stored here; no test calls semTools.

nst_no_semtools <- function(pkg) !identical(pkg, "semTools") && requireNamespace(pkg, quietly = TRUE)
nst_three <- "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9"
nst_cut <- function(d, items, k) {
  for (v in items) d[[v]] <- as.integer(cut(d[[v]], stats::quantile(d[[v]], 0:k / k), include.lowest = TRUE))
  d
}
nst_same_fit <- function(fit, direct, tolerance = 1e-8) {
  measures <- c("chisq", "df", "npar")
  if (!all(lavaan::lavInspect(fit, "options")$test == "standard")) measures <- c(measures, "chisq.scaled")
  expect_equal(lavaan::fitMeasures(fit, measures), lavaan::fitMeasures(direct, measures), tolerance = tolerance)
  a <- lavaan::parameterEstimates(fit)
  b <- lavaan::parameterEstimates(direct)
  a <- a[!a$op %in% c("==", ":="), ]
  at <- match(paste(a$lhs, a$op, a$rhs, a$group), paste(b$lhs, b$op, b$rhs, b$group))
  expect_false(anyNA(at))
  expect_equal(a$est, b$est[at], tolerance = tolerance)
  expect_equal(a$se, b$se[at], tolerance = tolerance)
}

test_that("Monte Carlo effect intervals are computed without semTools and match the semTools values", {
  local_mocked_bindings(sieve_have = nst_no_semtools)
  fit <- lavaan::sem("x4 ~ a*x1\nx7 ~ b*x4 + c*x1\nind := a*b\ntot := c + ind", data = lavaan::HolzingerSwineford1939)
  r <- sieve_indirect_mc(fit, B = 2000, seed = 113)
  expect_null(r$error)
  expect_identical(r$table$effect, c("ind", "tot"))
  # The point estimates are lavaan's standardized defined parameters.
  ss <- lavaan::standardizedSolution(fit)
  expect_equal(unname(r$table$est.std), ss$est.std[ss$op == ":="], tolerance = 1e-12)
  # Limits stored from semTools::monteCarloCI(standardized = TRUE) with the
  # same seed and B: the draws are the same draws.
  expect_equal(unname(r$table$ci.lower), c(0.020092823678954565, -0.042769657592032384), tolerance = 1e-10)
  expect_equal(unname(r$table$ci.upper), c(0.11720412264455976, 0.18082115569851062), tolerance = 1e-10)
  # Each draw is the standardized estimates plus normal deviates times the
  # Cholesky factor of vcov.std.all; each effect is its own definition
  # evaluated on the draw; the limits are the draws' percentiles.
  expect_identical(colnames(r$samples), c("a", "b", "c", "ind", "tot"))
  expect_identical(nrow(r$samples), 2000L)
  V <- lavaan::lavInspect(fit, "vcov.std.all")[c("a", "b", "c"), c("a", "b", "c")]
  z <- withr::with_seed(113, stats::rnorm(3 * 2000))
  center <- ss$est.std[ss$op == "~"]
  expect_equal(unname(r$samples[1, c("a", "b", "c")]), unname(center + drop(z[1:3] %*% chol(V))), tolerance = 1e-12)
  expect_equal(unname(r$samples[, "ind"]), unname(r$samples[, "a"] * r$samples[, "b"]))
  expect_equal(unname(r$samples[, "tot"]), unname(r$samples[, "c"] + r$samples[, "ind"]))
  expect_equal(unname(r$table$ci.lower[1]), unname(stats::quantile(r$samples[, "ind"], .025)))
  expect_identical(r$rng$seed, 113L)
  expect_identical(r$counts$requested, 2000L)
  expect_match(r$method, "2000 draws", fixed = TRUE)
  expect_match(sieve_indirect_mc(lavaan::sem("x4 ~ x1", data = lavaan::HolzingerSwineford1939), B = 100)$error, "defines no effects")
})

test_that("model-linked composite reliability is a closed form from the fitted CFA, unit and custom weights", {
  local_mocked_bindings(sieve_have = nst_no_semtools)
  x <- paste0("x", 1:9)
  fit <- lavaan::cfa(paste(nst_three, "x7 ~~ x8", sep = "\n"), data = lavaan::HolzingerSwineford1939, std.lv = TRUE)
  r <- sieve_model_composite(fit)
  expect_null(r$error)
  expect_identical(r$table$composite, c(
    "visual", "textual", "speed",
    "All modeled items (multidimensional common-factor variance share)"
  ))
  # By hand: the common-factor variance of the weighted sum over its observed
  # variance; the residual covariance x7 ~~ x8 counts as error.
  est <- lavaan::lavInspect(fit, "est")
  S <- lavaan::lavInspect(fit, "sampstat")$cov
  true <- est$lambda %*% est$psi %*% t(est$lambda)
  by_hand <- function(items, w = rep(1, length(items))) {
    c(crossprod(w, true[items, items] %*% w) / crossprod(w, S[items, items] %*% w))
  }
  expect_equal(r$table$estimate, c(by_hand(x[1:3]), by_hand(x[4:6]), by_hand(x[7:9]), by_hand(x)), tolerance = 1e-12)
  # Stored semTools::compRelSEM(obs.var = TRUE, tau.eq = FALSE, return.total = TRUE) values.
  expect_equal(r$table$estimate, c(0.61527783142610892, 0.88515911113290202, 0.55816055991480529, 0.82998891258256491),
    tolerance = 1e-10
  )
  expect_identical(r$target, "Observed unit-weighted item score")
  expect_match(r$method, "observed variance", fixed = TRUE)
  expect_match(r$interval_reason, "not requested", fixed = TRUE)
  # Custom weights; stored compRelSEM(W = ...) values.
  w <- c(2, 1, 1, 1, .5, 1, 1, 1, 3)
  rw <- sieve_model_composite(fit, weights = w)
  expect_null(rw$error)
  expect_equal(rw$table$estimate, c(by_hand(x[1:3], w[1:3]), by_hand(x[4:6], w[4:6]), by_hand(x[7:9], w[7:9]), by_hand(x, w)),
    tolerance = 1e-12
  )
  expect_equal(rw$table$estimate, c(0.66963593901701823, 0.87909654265745274, 0.80792262434203521, 0.86244219546910028),
    tolerance = 1e-10
  )
  expect_equal(unname(rw$weights), w)
  expect_identical(rw$target, "Observed weighted item score")
  expect_equal(sieve_model_composite(fit, weights = stats::setNames(rev(w), rev(x)))$table, rw$table)
  expect_error(sieve_model_composite(fit, weights = rep(0, 9)), "nonzero")
})

test_that("ordered composites use Green and Yang's coefficient with an exact bivariate normal", {
  local_mocked_bindings(sieve_have = nst_no_semtools)
  # The bivariate normal distribution function against stored mnormt::pmnorm
  # values (semTools' engine), and its limits at r = 0, 1, and -1.
  expect_equal(
    sieve_pbinorm(c(-.5, 1.2, .3, -1.1, .8, .4), c(.7, -.4, .3, 2, -.9, .4), c(.5, .95, 1, -.97, -.3, .999)),
    c(0.28410542671187411, 0.34457825383023966, 0.61791142218895256, 0.1129167687881321, 0.11931804390324757, 0.64885088521199197),
    tolerance = 1e-13
  )
  expect_equal(sieve_pbinorm(.3, -.8, 0), stats::pnorm(.3) * stats::pnorm(-.8), tolerance = 1e-15)
  expect_equal(sieve_pbinorm(.3, -.8, 1), stats::pnorm(-.8), tolerance = 1e-15)
  expect_equal(sieve_pbinorm(.3, .8, -1), stats::pnorm(.3) + stats::pnorm(.8) - 1, tolerance = 1e-15)
  x <- paste0("x", 1:9)
  d <- nst_cut(lavaan::HolzingerSwineford1939, x, 5)
  fit <- lavaan::cfa(nst_three, data = d, ordered = x, std.lv = TRUE, estimator = "WLSMV")
  r <- sieve_model_composite(fit)
  expect_null(r$error)
  expect_equal(r$table$estimate, c(0.59913180421172629, 0.86849834649709035, 0.66438729036406352, 0.84242596249254553),
    tolerance = 1e-8
  )
  rw <- sieve_model_composite(fit, weights = c(2, 1, 1, 1, .5, 1, 1, 1, 3))
  expect_equal(rw$table$estimate, c(0.68422477668217974, 0.85475471715960305, 0.73250848565268689, 0.81714101512236803),
    tolerance = 1e-8
  )
  expect_identical(r$target, "Observed ordinal unit-weighted score: the unit-weighted sum of the item codes as given in the fitted data (threshold-adjusted)")
  expect_match(r$method, "Green and Yang", fixed = TRUE)
  # A composite that mixes ordered and continuous items gets no coefficient,
  # and the reason is kept.
  mixed <- lavaan::HolzingerSwineford1939
  mixed$x1 <- d$x1
  fm <- lavaan::cfa(nst_three, data = mixed, ordered = "x1", std.lv = TRUE, estimator = "WLSMV")
  rm <- sieve_model_composite(fm)
  expect_identical(rm$table$composite, c("textual", "speed"))
  expect_true(any(grepl("visual mixes ordered and continuous items", rm$warnings, fixed = TRUE)))
})

test_that("in a higher-order model the second-order factor carries the true score", {
  local_mocked_bindings(sieve_have = nst_no_semtools)
  fit <- lavaan::cfa(paste(nst_three, "g =~ visual + textual + speed", sep = "\n"),
    data = lavaan::HolzingerSwineford1939, std.lv = TRUE
  )
  r <- sieve_model_composite(fit)
  expect_null(r$error)
  # The first-order composites equal the stored semTools values (semTools
  # reported no all-items composite for a higher-order model).
  expect_equal(r$table$estimate[1:3], c(0.46658740109902891, 0.2440572099586974, 0.19917257032613037), tolerance = 1e-10)
  # All items by hand: the second-order factor's part over the observed variance.
  est <- lavaan::lavInspect(fit, "est")
  g_part <- est$lambda %*% est$beta[, "g", drop = FALSE]
  S <- lavaan::lavInspect(fit, "sampstat")$cov
  w <- rep(1, 9)
  expect_equal(r$table$estimate[4],
    c(crossprod(w, g_part %*% est$psi["g", "g"] %*% t(g_part) %*% w) / crossprod(w, S %*% w)),
    tolerance = 1e-12
  )
})

test_that("ordered partial invariance is written directly and fits as hand-written Wu-Estabrook syntax", {
  local_mocked_bindings(sieve_have = nst_no_semtools)
  items <- c("x1", "x2", "x3", "x4")
  d <- nst_cut(lavaan::HolzingerSwineford1939[c(items, "school")], items, 3)
  ladder <- sieve_ordered_partial_ladder(d, "f =~ x1 + x2 + x3 + x4", "school", items,
    c("configural", "thresholds", "metric", "scalar"), "WLSMV", "listwise",
    c("f =~ x4", "x4 | t1", "x4 | t2", "x4 ~1")
  )
  # Wu and Estabrook (2016), theta parameterization: reference group
  # intercepts 0 and residual variances 1; elsewhere an item with two equal
  # thresholds has its residual variance free, and its intercept free unless
  # intercepts are equal. The released item x4 keeps intercept 0 and
  # residual variance 1 in both groups.
  equal_thresholds <- "
    x1 | c(a1, a1)*t1 + c(a2, a2)*t2
    x2 | c(b1, b1)*t1 + c(b2, b2)*t2
    x3 | c(c1, c1)*t1 + c(c2, c2)*t2
    x4 | c(NA, NA)*t1 + c(NA, NA)*t2"
  free_loadings <- "f =~ c(NA, NA)*x1 + c(NA, NA)*x2 + c(NA, NA)*x3 + c(NA, NA)*x4"
  equal_loadings <- "f =~ c(l1, l1)*x1 + c(l2, l2)*x2 + c(l3, l3)*x3 + c(NA, NA)*x4"
  free_scale <- "
    x1 ~~ c(1, NA)*x1
    x2 ~~ c(1, NA)*x2
    x3 ~~ c(1, NA)*x3
    x4 ~~ c(1, 1)*x4"
  hand <- list(
    configural = paste(free_loadings, "
      x1 | c(NA, NA)*t1 + c(NA, NA)*t2
      x2 | c(NA, NA)*t1 + c(NA, NA)*t2
      x3 | c(NA, NA)*t1 + c(NA, NA)*t2
      x4 | c(NA, NA)*t1 + c(NA, NA)*t2
      x1 ~ c(0, 0)*1
      x2 ~ c(0, 0)*1
      x3 ~ c(0, 0)*1
      x4 ~ c(0, 0)*1
      x1 ~~ c(1, 1)*x1
      x2 ~~ c(1, 1)*x2
      x3 ~~ c(1, 1)*x3
      x4 ~~ c(1, 1)*x4
      f ~ c(0, 0)*1
      f ~~ c(1, 1)*f"),
    thresholds = paste(free_loadings, equal_thresholds, free_scale, "
      x1 ~ c(0, NA)*1
      x2 ~ c(0, NA)*1
      x3 ~ c(0, NA)*1
      x4 ~ c(0, 0)*1
      f ~ c(0, 0)*1
      f ~~ c(1, 1)*f"),
    metric = paste(equal_loadings, equal_thresholds, free_scale, "
      x1 ~ c(0, NA)*1
      x2 ~ c(0, NA)*1
      x3 ~ c(0, NA)*1
      x4 ~ c(0, 0)*1
      f ~ c(0, 0)*1
      f ~~ c(1, NA)*f"),
    scalar = paste(equal_loadings, equal_thresholds, free_scale, "
      x1 ~ c(0, 0)*1
      x2 ~ c(0, 0)*1
      x3 ~ c(0, 0)*1
      x4 ~ c(0, 0)*1
      f ~ c(0, NA)*1
      f ~~ c(1, NA)*f")
  )
  for (rung in names(hand)) {
    direct <- lavaan::cfa(hand[[rung]],
      data = d, group = "school", ordered = items, estimator = "WLSMV",
      parameterization = "theta", std.lv = TRUE
    )
    nst_same_fit(attr(ladder, "fits")[[rung]], direct)
  }
  expect_match(attr(ladder, "identification"), "Wu and Estabrook (2016)", fixed = TRUE)
  expect_false(any(grepl("semTools", unlist(attr(ladder, "generated_syntax")), fixed = TRUE)))
})

# Three waves of one construct measured by the same three items, with equal
# loadings and intercepts and a rising latent mean.
nst_waves <- function() {
  withr::with_seed(2026, {
    n <- 500
    f1 <- stats::rnorm(n)
    f2 <- .2 + .7 * f1 + sqrt(.51) * stats::rnorm(n)
    f3 <- .4 + .7 * f2 + sqrt(.51) * stats::rnorm(n)
    item <- function(f) vapply(1:3, function(j) c(0, .3, -.2)[j] + c(.8, .7, .9)[j] * f + stats::rnorm(n, sd = .6), numeric(n))
    d <- as.data.frame(cbind(item(f1), item(f2), item(f3)))
  })
  names(d) <- paste0("x", 1:9)
  d
}

test_that("longitudinal invariance is written directly and fits as hand-written syntax", {
  local_mocked_bindings(sieve_have = nst_no_semtools)
  waves <- nst_waves()
  map <- data.frame(
    construct = "Scale", wave = rep(c("T1", "T2", "T3"), each = 3), item = rep(c("I1", "I2", "I3"), 3),
    variable = paste0("x", 1:9), wave_order = rep(1:3, each = 3)
  )
  r <- sieve_run_longitudinal(waves, map, level = "scalar", residuals = "adjacent", justification = "Same wording")
  f <- r$spec$latent
  # Equal loadings and intercepts across waves; first-wave mean 0 and
  # variance 1, later waves free; residual covariances of each repeated item
  # at adjacent waves.
  hand <- sprintf("
    %1$s =~ l1*x1 + l2*x2 + l3*x3
    %2$s =~ l1*x4 + l2*x5 + l3*x6
    %3$s =~ l1*x7 + l2*x8 + l3*x9
    x1 ~ n1*1
    x4 ~ n1*1
    x7 ~ n1*1
    x2 ~ n2*1
    x5 ~ n2*1
    x8 ~ n2*1
    x3 ~ n3*1
    x6 ~ n3*1
    x9 ~ n3*1
    %1$s ~ 0*1
    %2$s ~ NA*1
    %3$s ~ NA*1
    %1$s ~~ 1*%1$s
    %2$s ~~ NA*%2$s
    %3$s ~~ NA*%3$s
    x1 ~~ x4
    x4 ~~ x7
    x2 ~~ x5
    x5 ~~ x8
    x3 ~~ x6
    x6 ~~ x9", f[1], f[2], f[3])
  direct <- lavaan::cfa(hand, data = waves, std.lv = TRUE, estimator = "ML", missing = "fiml") # Auto
  nst_same_fit(r$fit, direct, tolerance = 1e-6)
  expect_equal(lavaan::fitMeasures(r$fit, "logl"), lavaan::fitMeasures(direct, "logl"), tolerance = 1e-8)

  # Ordered items, two waves, Wu and Estabrook (2016) under theta: equal
  # thresholds and intercepts (0), first-wave residual variances 1 and
  # later ones free.
  items <- paste0("x", 1:6)
  d <- nst_cut(waves[items], items, 3)
  map2 <- data.frame(
    construct = "Scale", wave = rep(c("T1", "T2"), each = 3), item = rep(c("I1", "I2", "I3"), 2),
    variable = items, wave_order = rep(1:2, each = 3)
  )
  r2 <- sieve_run_longitudinal(d, map2, level = "scalar", residuals = "adjacent", justification = "Same wording", ordered = items)
  g <- r2$spec$latent
  hand2 <- sprintf("
    %1$s =~ l1*x1 + l2*x2 + l3*x3
    %2$s =~ l1*x4 + l2*x5 + l3*x6
    x1 | a1*t1 + a2*t2
    x4 | a1*t1 + a2*t2
    x2 | b1*t1 + b2*t2
    x5 | b1*t1 + b2*t2
    x3 | c1*t1 + c2*t2
    x6 | c1*t1 + c2*t2
    x1 ~ 0*1
    x2 ~ 0*1
    x3 ~ 0*1
    x4 ~ 0*1
    x5 ~ 0*1
    x6 ~ 0*1
    x1 ~~ 1*x1
    x2 ~~ 1*x2
    x3 ~~ 1*x3
    x4 ~~ NA*x4
    x5 ~~ NA*x5
    x6 ~~ NA*x6
    %1$s ~ 0*1
    %2$s ~ NA*1
    %1$s ~~ 1*%1$s
    %2$s ~~ NA*%2$s
    x1 ~~ x4
    x2 ~~ x5
    x3 ~~ x6", g[1], g[2])
  direct2 <- lavaan::cfa(hand2, data = d, ordered = items, std.lv = TRUE, estimator = "WLSMV", parameterization = "theta")
  # The two parameter orders reach the same WLSMV minimum within the
  # optimizer's tolerance.
  nst_same_fit(r2$fit, direct2, tolerance = 1e-6)
  expect_identical(lavaan::lavInspect(r2$fit, "options")$parameterization, "theta")
})

test_that("no SIEVER code calls semTools", {
  # The incomplete-data Bollen-Stine bootstrap and the two-stage residual
  # statistic come from DMAR.
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  files <- list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)
  called <- vapply(files, function(f) any(grepl("semTools", readLines(f, warn = FALSE), fixed = TRUE) &
    !grepl("^\\s*#", readLines(f, warn = FALSE))), logical(1))
  expect_identical(basename(files[called]), character(0))
  expect_false("semTools" %in% SIEVER:::sieve_optional_packages()$package)
})

test_that("the inference panels state what is available in plain words", {
  html <- paste(
    as.character(sieve_cfa_evidence_ui(shiny::NS("cfa"))),
    as.character(sieve_operational_ui(shiny::NS("reliab")))
  )
  for (jargon in c("DMAR draft", "gated", "oracle", "semTools", "engine is available")) {
    expect_false(grepl(jargon, html, fixed = TRUE), info = jargon)
  }
  expect_match(html, "SIEVER does not estimate how a partial-response rule changes reliability", fixed = TRUE)
  expect_match(html, "unit weights unless you enter weights", fixed = TRUE)
})

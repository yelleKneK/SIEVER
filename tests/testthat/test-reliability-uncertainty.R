# Reliability uncertainty: omega under missing data, the per construct
# resolution of coefficient, missing data treatment, and interval method,
# and the interval for H.
#
# The checks here are oracles, not echoes. Omega under full information
# maximum likelihood is compared with (a) a one factor model specified
# independently in lavaan under a different identification, with omega as
# a defined parameter, and (b) a likelihood written out in this file and
# maximized with stats::nlminb(), with a numerical Hessian, which does not
# use lavaan or DMAR at all. H's variance is compared with the delta method
# through the unstandardized parameters and with finite differences.
#
# Anchors: two fixed examples (six_items_n400() and six_items_n500()) are
# regenerated from their seeds, and reference values for them are asserted
# where they are right and contradicted where they would come from an error
# (an interval narrowed by rows that carry no information, an empty row for
# ordered items under FIML, a variance of H without the loadings'
# covariances).

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

# ---- fixtures --------------------------------------------------------------

one_factor_data <- function(n, loadings, seed, prefix = "x") {
  withr::with_seed(seed, {
    f <- stats::rnorm(n)
    X <- sapply(loadings, function(l) l * f + sqrt(1 - l^2) * stats::rnorm(n))
  })
  X <- as.data.frame(X)
  names(X) <- paste0(prefix, seq_along(loadings))
  X
}

# Six items, 400 respondents, complete data.
six_items_n400 <- function() {
  withr::with_seed(113, {
    z <- stats::rnorm(400)
    d <- as.data.frame(sapply(
      c(.55, .60, .65, .70, .75, .80),
      function(l) l * z + sqrt(1 - l^2) * stats::rnorm(400)
    ))
  })
  names(d) <- paste0("a", 1:6)
  d
}

# Six items, 500 respondents, complete data.
six_items_n500 <- function() {
  withr::with_seed(20260929, {
    f <- stats::rnorm(500)
    X <- as.data.frame(sapply(
      c(.5, .6, .7, .8, .65, .75),
      function(l) l * f + sqrt(1 - l * l) * stats::rnorm(500)
    ))
  })
  names(X) <- paste0("x", 1:6)
  X
}

empty_rows <- function(X, n) {
  rbind(X, as.data.frame(matrix(NA_real_, n, ncol(X), dimnames = list(NULL, names(X)))))
}

ordered_version <- function(X) {
  as.data.frame(lapply(X, function(x) as.numeric(cut(x, c(-Inf, -.7, 0, .7, Inf)))))
}

# The function's defaults are robust omega with the recommended interval.
# The oracles in this file test model implied omega and its closed form
# intervals, so the helper names that variant unless a test asks otherwise.
# (Listwise is named, as the function's default is DMAR's "fiml": these
# tests are about complete cases.)
continuous_spec <- function(X, ..., denominator = "model_implied", missing = "listwise") {
  sieve_reliability_spec(names(X), rep("continuous", ncol(X)), construct = "F", ...,
    denominator = denominator, missing = missing
  )
}

# Omega as a defined parameter of a one factor model identified by a marker
# loading (the package's engine fixes the factor variance instead), fit by
# full information maximum likelihood.
lavaan_omega_oracle <- function(X, estimator = "ML", information = "observed",
                                missing = "fiml", se = NULL) {
  v <- names(X)
  k <- length(v)
  lam <- c("1", paste0("l", 2:k))
  model <- paste0(
    "f =~ ", paste0(lam, "*", v, collapse = " + "), "\n",
    "f ~~ phi*f\n",
    paste0(v, " ~~ e", seq_len(k), "*", v, collapse = "\n"), "\n",
    "lsum := 1 + ", paste(lam[-1], collapse = " + "), "\n",
    "omega := (lsum^2 * phi) / (lsum^2 * phi + ", paste0("e", seq_len(k), collapse = " + "), ")\n"
  )
  args <- list(
    model = model, data = X, estimator = estimator, missing = missing,
    information = information
  )
  if (!is.null(se)) args$se <- se
  fit <- do.call(lavaan::cfa, args)
  pe <- lavaan::parameterEstimates(fit, level = 0.95)
  row <- pe[pe$lhs == "omega" & pe$op == ":=", ]
  list(
    estimate = row$est, se = row$se, lower = row$ci.lower, upper = row$ci.upper,
    nobs = lavaan::lavInspect(fit, "nobs"),
    patterns = length(lavaan::lavInspect(fit, "patterns")[, 1])
  )
}

# The same quantity with no SEM software: the observed data log likelihood
# of the one factor model summed over missing data patterns, maximized
# directly; the standard error is the delta method with the inverse of a
# central difference Hessian (the observed information).
direct_fiml_omega <- function(X) {
  X <- as.matrix(X)
  X <- X[rowSums(!is.na(X)) > 0, , drop = FALSE]
  p <- ncol(X)
  seen <- !is.na(X)
  key <- apply(seen, 1L, function(r) paste(as.integer(r), collapse = ""))
  groups <- lapply(split(seq_len(nrow(X)), key), function(idx) {
    o <- seen[idx[1], ]
    Y <- X[idx, o, drop = FALSE]
    m <- colMeans(Y)
    list(o = o, n = nrow(Y), m = m, S = crossprod(sweep(Y, 2L, m)) / nrow(Y))
  })
  negll <- function(par) {
    lam <- par[seq_len(p)]
    psi <- exp(par[p + seq_len(p)])
    mu <- par[2L * p + seq_len(p)]
    Sigma <- tcrossprod(lam) + diag(psi)
    total <- 0
    for (g in groups) {
      So <- Sigma[g$o, g$o, drop = FALSE]
      d <- g$m - mu[g$o]
      total <- total + g$n * (sum(g$o) * log(2 * pi) +
        as.numeric(determinant(So, logarithm = TRUE)$modulus) +
        sum(diag(solve(So, g$S + tcrossprod(d)))))
    }
    0.5 * total
  }
  sds <- apply(X, 2L, stats::sd, na.rm = TRUE)
  start <- c(0.7 * sds, log(0.5 * sds^2), colMeans(X, na.rm = TRUE))
  opt <- stats::nlminb(start, negll,
    control = list(
      iter.max = 5000, eval.max = 10000,
      rel.tol = 1e-15, x.tol = 1e-12
    )
  )
  opt <- stats::nlminb(opt$par, negll,
    control = list(
      iter.max = 5000, eval.max = 10000,
      rel.tol = 1e-15, x.tol = 1e-12
    )
  )
  par <- opt$par
  q <- length(par)
  h <- 1e-4 * pmax(1, abs(par))
  hess <- matrix(0, q, q)
  for (i in seq_len(q)) {
    for (j in i:q) {
      ei <- replace(numeric(q), i, h[i])
      ej <- replace(numeric(q), j, h[j])
      hess[i, j] <- hess[j, i] <-
        (negll(par + ei + ej) - negll(par + ei - ej) -
          negll(par - ei + ej) + negll(par - ei - ej)) / (4 * h[i] * h[j])
    }
  }
  lam <- par[seq_len(p)]
  psi <- exp(par[p + seq_len(p)])
  L <- sum(lam)
  E <- sum(psi)
  omega <- L^2 / (L^2 + E)
  grad <- c(rep(2 * L * E / (L^2 + E)^2, p), -L^2 * psi / (L^2 + E)^2, rep(0, p))
  # First order condition at the solution, by central differences: the
  # largest element of the score, which is zero at a maximum.
  score <- vapply(seq_len(q), function(i) {
    ei <- replace(numeric(q), i, h[i])
    (negll(par + ei) - negll(par - ei)) / (2 * h[i])
  }, numeric(1))
  list(
    estimate = omega, se = sqrt(drop(grad %*% solve(hess, grad))),
    n = nrow(X), patterns = length(groups), score = max(abs(score)),
    curvature = min(eigen(hess, symmetric = TRUE, only.values = TRUE)$values)
  )
}

make_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = TRUE, name = fx$name)
  s
}

# Mark the named constructs' items ordered and cut them into five categories.
make_ordered <- function(st, constructs = NULL) {
  shiny::isolate({
    meta <- st$meta
    hit <- if (is.null(constructs)) rep(TRUE, nrow(meta)) else meta$construct %in% constructs
    items <- st$items
    for (it in meta$item[hit]) {
      items[[it]] <- as.integer(cut(as.numeric(items[[it]]), breaks = 5))
    }
    meta$type[hit] <- "ordered"
    st$items <- items
    st$meta <- meta
    sieve_confirm_types(st)
  })
  st
}

interval_of <- function(r) c(r$estimate, r$se, r$lower, r$upper)

# ---- Rows with no observed item carry no information -----------------------

test_that("entirely missing rows change neither omega nor its interval", {
  d <- six_items_n400()
  padded <- empty_rows(d, 3600)
  for (method in c("ml", "mlr")) {
    spec <- continuous_spec(d, missing = "fiml", ci_method = method)
    a <- sieve_reliability_omega(d, spec)
    b <- sieve_reliability_omega(padded, spec)
    expect_identical(a$uncertainty$status, "completed")
    expect_identical(interval_of(b), interval_of(a))
    # The provenance separates the rows there are from the rows that inform.
    expect_identical(unlist(a$n), c(
      rows = 400L, observed = 400L, empty = 0L,
      complete = 400L, patterns = 1L, used = 400L
    ))
    expect_identical(unlist(b$n), c(
      rows = 4000L, observed = 400L, empty = 3600L,
      complete = 400L, patterns = 1L, used = 400L
    ))
    expect_identical(b$missing$used, "fiml")
    expect_identical(b$uncertainty$information, "observed")
  }
  # The reference values: omega 0.8369434 with [0.8122146, 0.8616721] on the
  # 400 respondents (complete data, expected information). Counting 3,600
  # appended empty rows as information would narrow it to [0.8291234,
  # 0.8447633]. The complete data interval is reproduced, and the padded FIML
  # interval stays at that width instead of shrinking by the square root of
  # ten.
  cc <- sieve_reliability_omega(d, continuous_spec(d, missing = "listwise", ci_method = "ml"))
  expect_equal(cc$estimate, 0.8369434, tolerance = 1e-7)
  expect_equal(c(cc$lower, cc$upper), c(0.8122146, 0.8616721), tolerance = 1e-7)
  fiml_pad <- sieve_reliability_omega(padded, continuous_spec(d, missing = "fiml", ci_method = "ml"))
  expect_equal(fiml_pad$estimate, 0.8369434, tolerance = 1e-7)
  expect_equal(c(fiml_pad$lower, fiml_pad$upper), c(0.8122146, 0.8616721), tolerance = 1e-4)
  expect_gt(fiml_pad$upper - fiml_pad$lower, 0.049)
  expect_gt(abs(fiml_pad$lower - 0.8291234), 0.015)
})

test_that("entirely missing rows change neither H, alpha, nor a seeded bootstrap", {
  d <- six_items_n400()
  d[c(5, 80, 200), 2] <- NA
  padded <- empty_rows(d, 900)
  spec <- continuous_spec(d, missing = "fiml", ci_method = "percentile", B = 400, seed = 7)
  a <- sieve_reliability_construct(d, spec)
  b <- sieve_reliability_construct(padded, spec)
  expect_identical(a$omega$uncertainty$status, "completed")
  expect_identical(interval_of(b$omega), interval_of(a$omega))
  expect_identical(b$omega$uncertainty$rng$seed, 7L)
  expect_true(b$omega$uncertainty$rng$supplied)
  # The counted bootstrap resamples the rows with an observed item only, so
  # every replication, and every count, is the same with the empty rows.
  expect_identical(b$omega$uncertainty$rows, "observed")
  expect_identical(b$omega$uncertainty$draws, a$omega$uncertainty$draws)
  expect_identical(b$omega$uncertainty$counts, a$omega$uncertainty$counts)
  expect_gte(a$omega$uncertainty$B_used, 39L)
  expect_identical(
    c(b$H$estimate, b$H$variance, b$H$lower, b$H$upper),
    c(a$H$estimate, a$H$variance, a$H$lower, a$H$upper)
  )
  expect_identical(b$H$n$used, 400L)
  expect_identical(b$H$warnings, a$H$warnings)
  expect_identical(b$alpha$estimate, a$alpha$estimate)
  expect_identical(b$omega$n$patterns, 2L)
  expect_identical(b$omega$n$complete, 397L)
})

test_that("robust omega under FIML takes a bootstrap interval from the partially observed rows", {
  d <- six_items_n400()
  d[withr::with_seed(21, sample.int(400, 120)), 5:6] <- NA
  spec <- continuous_spec(d,
    missing = "fiml", ci_method = "percentile",
    denominator = "observed", B = 400, seed = 9
  )
  expect_identical(spec$coefficient, "omega_robust")
  # (DMAR's counted bootstrap.)
  expect_identical(spec$resampling, "dmar")
  res <- sieve_reliability_omega(d, spec)
  # (Labels name their target and use DMAR's denominator wording.)
  expect_identical(res$label, "Robust omega (observed denominator)")
  expect_identical(res$status$code, "ok")
  expect_identical(res$uncertainty$status, "completed")
  expect_true(res$lower < res$estimate && res$estimate < res$upper)
  expect_identical(c(res$n$used, res$n$complete, res$n$patterns), c(400L, 280L, 2L))
  # The pool is the 400 rows with an observed item, 120 of them incomplete;
  # a replication refitted by hand (its rows from boot under the seed, as
  # DMAR draws them, the coefficient from DMAR's point estimator by FIML) is
  # the one kept.
  expect_identical(res$uncertainty$rows, "observed")
  pool <- d[rowSums(!is.na(d)) > 0, ]
  expect_identical(nrow(pool), 400L)
  index <- withr::with_seed(9, boot::boot.array(boot::boot(pool, function(x, i) 0, R = 400L, stype = "i"), indices = TRUE))
  for (b in c(1L, 23L, 400L)) {
    rows <- index[b, ]
    expect_gt(sum(!stats::complete.cases(pool[rows, ])), 0L)
    hand <- DMAR::reliability_omega(
      data = pool[rows, ], ci_method = "none",
      denominator = "observed", missing = "fiml"
    )
    expect_identical(res$uncertainty$draws$estimate[b], hand$value[hand$term == "estimate"])
  }
  # Empty rows are not resampled.
  padded <- sieve_reliability_omega(empty_rows(d, 500), spec)
  expect_identical(interval_of(padded), interval_of(res))
  expect_identical(padded$uncertainty$draws, res$uncertainty$draws)
  # The engine's percentile code (dmar_percentile) is the same counted
  # call, and is the same call made directly.
  dmar <- sieve_reliability_omega(d, continuous_spec(d,
    missing = "fiml", ci_method = "dmar_percentile",
    denominator = "observed", B = 400, seed = 9
  ))
  expect_identical(dmar$uncertainty$label, "Bootstrap percentile")
  expect_identical(dmar$uncertainty$status, "completed")
  expect_identical(interval_of(dmar), interval_of(res))
  # DMAR returns effective bootstrap counts; they are preserved.
  direct <- DMAR::reliability_omega(
    data = d, ci_method = "percentile", denominator = "observed",
    missing = "fiml", conf_level = 0.95, B = 400, seed = 9
  )
  if (is.list(attr(direct, "bootstrap"))) {
    expect_identical(dmar$uncertainty$B_used, attr(direct, "bootstrap")$returned)
    expect_identical(dmar$uncertainty$engine_bootstrap, attr(direct, "bootstrap"))
  } else {
    expect_true(is.na(dmar$uncertainty$B_used))
    expect_null(dmar$uncertainty$engine_bootstrap)
    expect_true(is.na(dmar$uncertainty$mc_se))
  }
  expect_identical(dmar$estimate, direct$value[direct$term == "estimate"])
  expect_identical(
    c(dmar$lower, dmar$upper),
    direct$value[match(c("ci_lower", "ci_upper"), direct$term)]
  )
  expect_identical(interval_of(sieve_reliability_omega(empty_rows(d, 500), dmar$spec)), interval_of(dmar))
  # A closed form request for robust omega is declined with the reason.
  closed <- sieve_reliability_omega(d, continuous_spec(d,
    missing = "fiml", ci_method = "mlr",
    denominator = "observed"
  ))
  expect_identical(closed$uncertainty$status, "unavailable")
  expect_match(closed$uncertainty$reason, "model implied denominator")
  expect_identical(closed$estimate, res$estimate)
})

test_that("FIML omega agrees with an independently specified lavaan model (MCAR and MAR)", {
  X <- six_items_n500()
  # MCAR: 400 of 500 respondents missing items 4 to 6.
  mcar <- X
  mcar[withr::with_seed(11, sample.int(500, 400)), 4:6] <- NA
  # MAR: items 4 to 6 are missing more often when the observed x1 is high.
  mar <- X
  gone <- withr::with_seed(12, stats::runif(500)) < stats::plogis(-0.5 + 1.5 * X$x1)
  mar[gone, 4:6] <- NA
  expect_gt(sum(gone), 150)
  expect_lt(sum(gone), 350)

  for (data in list(mcar, mar)) {
    # Normal theory interval: observed information.
    ml <- sieve_reliability_omega(data, continuous_spec(data, missing = "fiml", ci_method = "ml"))
    ref <- lavaan_omega_oracle(data, estimator = "ML", information = "observed")
    # The two models are the same model under different identifications
    # (marker loading against unit factor variance), so the maximum
    # likelihood estimate of omega and its delta method standard error from
    # the observed information agree up to the optimizers' convergence
    # tolerance; 1e-6 is two to three orders below anything displayed and
    # four below the standard error itself.
    expect_equal(ml$estimate, ref$estimate, tolerance = 1e-6)
    expect_equal(ml$se, ref$se, tolerance = 1e-5)
    expect_equal(c(ml$lower, ml$upper), c(ref$lower, ref$upper), tolerance = 1e-6)
    expect_identical(ml$n$used, as.integer(ref$nobs))
    expect_identical(ml$n$patterns, ref$patterns)
    expect_identical(ml$uncertainty$information, "observed")
    # The recorded information type is a fact, not a label: the expected
    # information gives a different standard error under missing data.
    ref_expected <- lavaan_omega_oracle(data, estimator = "ML", information = "expected")
    expect_gt(abs(ml$se - ref_expected$se), 1e-5)

    # Robust interval: Huber-White sandwich (lavaan's MLR default under FIML).
    mlr <- sieve_reliability_omega(data, continuous_spec(data, missing = "fiml", ci_method = "mlr"))
    ref_r <- lavaan_omega_oracle(data, estimator = "MLR", information = "observed")
    expect_equal(mlr$estimate, ref_r$estimate, tolerance = 1e-6)
    expect_equal(mlr$se, ref_r$se, tolerance = 1e-5)
    expect_equal(c(mlr$lower, mlr$upper), c(ref_r$lower, ref_r$upper), tolerance = 1e-6)
    expect_match(mlr$uncertainty$se_type, "Huber-White")
  }
})

test_that("FIML omega agrees with a likelihood maximized without any SEM software", {
  X <- six_items_n500()
  mar <- X
  gone <- withr::with_seed(12, stats::runif(500)) < stats::plogis(-0.5 + 1.5 * X$x1)
  mar[gone, 4:6] <- NA
  mar[withr::with_seed(13, sample.int(500, 40)), 2] <- NA
  ml <- sieve_reliability_omega(mar, continuous_spec(mar, missing = "fiml", ci_method = "ml"))
  direct <- direct_fiml_omega(mar)
  # The oracle is at a maximum: the score vanishes and the Hessian of the
  # negative log likelihood is positive definite.
  expect_lt(direct$score, 1e-4)
  expect_gt(direct$curvature, 0)
  expect_equal(ml$estimate, direct$estimate, tolerance = 1e-6)
  # The Hessian is a central difference, accurate to roughly 1e-6 relative;
  # 1e-4 relative on the standard error is a wide margin for that and three
  # orders tighter than the error it guards against (a width off by a
  # factor of 1.4).
  expect_equal(ml$se, direct$se, tolerance = 1e-4)
  expect_identical(ml$n$used, direct$n)
  expect_identical(ml$n$patterns, direct$patterns)
  crit <- stats::qnorm(0.975)
  expect_equal(c(ml$lower, ml$upper),
    direct$estimate + c(-1, 1) * crit * direct$se,
    tolerance = 1e-5
  )
})

test_that("partial missingness is not reduced to a covariance matrix plus the row count", {
  X <- six_items_n500()
  mcar <- X
  mcar[withr::with_seed(11, sample.int(500, 400)), 4:6] <- NA
  # The wrong route, built here as the negative control: a saturated
  # FIML covariance matrix handed to a complete data analysis with N = all
  # rows.
  S <- lavaan::lavCor(mcar, missing = "fiml", output = "cov")
  old <- DMAR::reliability_omega(
    S = as.matrix(S), N = nrow(mcar), ci_method = "ml",
    denominator = "model_implied"
  )
  old_width <- old$value[old$term == "ci_upper"] - old$value[old$term == "ci_lower"]
  new <- sieve_reliability_omega(mcar, continuous_spec(mcar, missing = "fiml", ci_method = "ml"))
  # Three of six items are observed for only a fifth of the respondents; the
  # honest interval is far wider than the one that counted every row as
  # complete.
  expect_gt((new$upper - new$lower) / old_width, 1.3)
  expect_identical(new$n$complete, 100L)
  expect_identical(new$n$used, 500L)
  expect_identical(new$n$patterns, 2L)
  # And the two estimates differ: the direct FIML estimate of the one factor
  # model is not the complete data estimate applied to a saturated FIML
  # covariance matrix.
  expect_gt(abs(new$estimate - old$value[old$term == "estimate"]), 1e-4)
})

test_that("the recorded standard error types are the ones the engine uses", {
  # Skewed errors separate the normal theory and the two robust standard
  # errors clearly.
  X <- withr::with_seed(5, {
    f <- stats::rnorm(500)
    as.data.frame(sapply(c(.5, .6, .7, .8, .65, .75), function(l) {
      l * f + sqrt(1 - l^2) * (stats::rchisq(500, 2) - 2) / 2
    }))
  })
  names(X) <- paste0("x", 1:6)
  ml <- sieve_reliability_omega(X, continuous_spec(X, missing = "listwise", ci_method = "ml"))
  mlr <- sieve_reliability_omega(X, continuous_spec(X, missing = "listwise", ci_method = "mlr"))
  ref_ml <- lavaan_omega_oracle(X, "ML", information = "expected", missing = "listwise")
  ref_sb <- lavaan_omega_oracle(X, "MLR",
    information = "expected", missing = "listwise",
    se = "robust.sem"
  )
  ref_hw <- lavaan_omega_oracle(X, "MLR", information = "observed", missing = "listwise")
  expect_equal(ml$se, ref_ml$se, tolerance = 1e-5)
  expect_identical(ml$uncertainty$information, "expected")
  # Complete cases: the engine's robust ML interval is lavaan's robust.sem
  # standard error (the Satorra-Bentler correction), not the Huber-White
  # sandwich that lavaan's MLR estimator defaults to.
  expect_equal(mlr$se, ref_sb$se, tolerance = 1e-5)
  expect_gt(abs(mlr$se - ref_hw$se), 1e-6)
  expect_match(mlr$uncertainty$se_type, "robust.sem")
  expect_gt(abs(mlr$se - ml$se) / ml$se, 0.05)
  # Wald limits, symmetric about the estimate.
  crit <- stats::qnorm(0.975)
  expect_equal(c(mlr$lower, mlr$upper), mlr$estimate + c(-1, 1) * crit * mlr$se,
    tolerance = 1e-10
  )
})

# ---- Compatible combinations, resolved per construct -----------------------

test_that("the specification resolves each combination and says why", {
  items <- paste0("i", 1:4)
  resolve <- function(types, ...) sieve_reliability_spec(items, rep_len(types, 4), "C", ...)
  expected <- list(
    # class, missing, ci, denominator -> coefficient, missing used, ci used, status
    list("continuous", "listwise", "mlr", "model_implied", "omega", "listwise", "mlr", "requested"),
    list("continuous", "listwise", "likelihood", "model_implied", "omega", "listwise", "likelihood", "requested"),
    list("continuous", "fiml", "ml", "model_implied", "omega", "fiml", "ml", "requested"),
    list("continuous", "fiml", "likelihood", "model_implied", "omega", "fiml", "none", "unavailable"),
    list("continuous", "fiml", "dmar_bca", "model_implied", "omega", "fiml", "dmar_bca", "requested"),
    list("continuous", "listwise", "mlr", "observed", "omega_robust", "listwise", "none", "unavailable"),
    list("continuous", "fiml", "percentile", "observed", "omega_robust", "fiml", "percentile", "requested"),
    list("continuous", "listwise", "none", "model_implied", "omega", "listwise", "none", "not_requested"),
    list("ordered", "listwise", "mlr", "model_implied", "omega_categorical", "listwise", "none", "unavailable"),
    list("ordered", "fiml", "ml", "model_implied", "omega_categorical", "listwise", "none", "unavailable"),
    list("ordered", "fiml", "percentile", "model_implied", "omega_categorical", "listwise", "percentile", "requested"),
    list("binary", "listwise", "dmar_bca", "observed", "omega_categorical", "listwise", "dmar_bca", "requested"),
    # (DMAR's percentile code is the counted percentile bootstrap, so it
    # resolves to "percentile".)
    list(
      "ordered",
      "listwise",
      "dmar_percentile",
      "model_implied",
      "omega_categorical",
      "listwise",
      "percentile",
      "requested"
    ),
    list("binary", "listwise", "likelihood", "model_implied", "omega_categorical", "listwise", "none", "unavailable")
  )
  for (e in expected) {
    spec <- resolve(e[[1]], missing = e[[2]], ci_method = e[[3]], denominator = e[[4]])
    info <- paste(unlist(e[1:4]), collapse = " / ")
    expect_identical(spec$coefficient, e[[5]], info = info)
    expect_identical(spec$missing, e[[6]], info = info)
    expect_identical(spec$ci_method, e[[7]], info = info)
    expect_identical(spec$uncertainty$status, e[[8]], info = info)
    # A request that is not honored always carries its reason.
    if (!identical(spec$uncertainty$status, "requested")) {
      expect_true(is.character(spec$uncertainty$reason) && nzchar(spec$uncertainty$reason),
        info = info
      )
    }
    if (!identical(spec$missing, e[[2]])) {
      expect_match(spec$missing_reason, "not offered for ordered or binary", info = info)
    } else {
      expect_null(spec$missing_reason, info = info)
    }
    # What is used is always something the engine supports.
    if (!identical(spec$ci_method, "none")) {
      expect_true(
        spec$ci_method %in%
          sieve_reliability_supported(spec$item_class, spec$missing, e[[4]]),
        info = info
      )
    }
  }
  # A construct that mixes item types is labeled as what it is.
  # (The default variant is robust omega; the label says so.)
  mixed <- sieve_reliability_spec(items, c("continuous", "continuous", "ordered", "binary"), "M")
  expect_identical(mixed$item_class, "mixed")
  expect_identical(mixed$label, "Robust omega (observed denominator; item scores as continuous)")
  expect_match(paste(mixed$notes, collapse = " "), "numeric codes")
  # A selected bootstrap can wait for an explicit request; it is then
  # pending, with the same identity as the request it will become.
  wait <- resolve("ordered", ci_method = "percentile", run_bootstrap = FALSE)
  go <- resolve("ordered", ci_method = "percentile", run_bootstrap = TRUE)
  expect_identical(wait$uncertainty$status, "pending")
  expect_identical(wait$ci_method, "none")
  expect_identical(wait$key, go$key)
  expect_false(identical(go$key, resolve("ordered", ci_method = "dmar_bca")$key))
  # The two percentile codes are one counted call.
  expect_identical(go$key, resolve("ordered", ci_method = "dmar_percentile")$key)
  # Which bootstrap runs where: DMAR's counted bootstrap of independent rows,
  # or SIEVER's own under a declared cluster design.
  expect_identical(go$resampling, "dmar")
  expect_identical(resolve("ordered", ci_method = "dmar_percentile")$resampling, "dmar")
  expect_identical(resolve("ordered", ci_method = "dmar_bca")$resampling, "dmar")
  expect_identical(resolve("ordered", ci_method = "dmar_bca", cluster = rep(1:50, 2))$resampling, "siever")
  expect_true(is.na(resolve("continuous", ci_method = "mlr")$resampling))
  expect_identical(sieve_reliability_engine_ci("percentile"), "percentile")
  expect_identical(sieve_reliability_engine_ci("dmar_percentile"), "percentile")
  expect_identical(sieve_reliability_engine_ci("dmar_bca"), "bca")
  expect_identical(sieve_reliability_engine_ci("mlr"), "mlr")
  expect_error(resolve("continuous", conf_level = 1), "conf_level")
  expect_error(resolve("continuous", B = 0), "`B`")
})

test_that("ordered items with FIML requested give categorical omega, not an empty row", {
  Xo <- ordered_version(six_items_n500())
  Xo[c(3, 9, 27, 81, 243), 5] <- NA
  spec <- sieve_reliability_spec(names(Xo), rep("ordered", 6), "F",
    missing = "fiml", ci_method = "mlr"
  )
  res <- sieve_reliability_omega(Xo, spec)
  # Not an empty row (NA for the estimate and the interval) with lavaan's
  # "missing = ml not available in the categorical setting" discarded.
  expect_identical(res$status$code, "ok")
  expect_true(is.finite(res$estimate))
  expect_identical(res$coefficient, "omega_categorical")
  # (Labels name their target and use DMAR's denominator wording. A closed
  # form request that names no denominator asks for model-implied omega, which
  # DMAR honors.)
  expect_identical(res$label, categorical_omega_label("model_implied"))
  # The missing data treatment used is stated, with the reason it differs.
  expect_identical(res$missing$requested, "fiml")
  expect_identical(res$missing$used, "listwise")
  expect_match(res$missing$reason, "complete cases were used")
  expect_match(res$missing$reason, "5 of the 500 rows with an observed item are incomplete")
  expect_identical(res$n$used, 495L)
  expect_identical(res$n$complete, 495L)
  expect_identical(res$n$patterns, 2L)
  # No interval, and the reason is kept instead of a silent NA.
  expect_identical(res$uncertainty$status, "unavailable")
  expect_match(res$uncertainty$reason, "no closed form interval")
  expect_true(all(is.na(c(res$lower, res$upper))))
  expect_identical(res$uncertainty$method, "none")
  # The engine it resolves to is the one that was asked.
  direct <- DMAR::reliability_omega_categorical(Xo, ci_method = "none", denominator = "model_implied")
  expect_identical(res$estimate, direct$value[direct$term == "estimate"])
})

test_that("categorical omega agrees with an independent implementation", {
  Xo <- ordered_version(six_items_n500())
  res <- sieve_reliability_omega(
    Xo, sieve_reliability_spec(names(Xo), rep("ordered", 6), "F", ci_method = "none")
  )
  fit <- lavaan::cfa("f =~ x1 + x2 + x3 + x4 + x5 + x6",
    data = Xo,
    ordered = names(Xo), std.lv = TRUE
  )
  # Green and Yang's (2009) coefficient for the ordinal sum score, with the
  # total variance from the observed polychoric correlations: the value
  # semTools::compRelSEM(ord.scale = TRUE, tau.eq = FALSE, obs.var = TRUE)
  # gives for this fit (stored),
  # and SIEVER's own model-linked composite from the same fit.
  expect_equal(res$estimate, 0.79969619828422978, tolerance = 1e-6)
  expect_equal(res$estimate, sieve_model_composite(fit)$table$estimate[1], tolerance = 1e-6)
  expect_identical(res$uncertainty$status, "not_requested")
})

test_that("a categorical bootstrap is counted: every replication's outcome, a hand refit, a stop, and a replay", {
  Xo <- ordered_version(six_items_n500())[1:200, ]
  # SIEVER's counted engine runs only under a declared cluster design
  # (DMAR's counted bootstrap resamples independent rows;
  # test-dmar-bootstraps.R). Its contract is checked here on singleton
  # clusters, where the whole-cluster draw is the row draw, so every hand
  # refit below is a refit on resampled rows.
  spec <- sieve_reliability_spec(names(Xo), rep("ordered", 6), "F",
    ci_method = "percentile", B = 40, cluster = seq_len(nrow(Xo))
  )
  expect_identical(spec$resampling, "siever")
  # No seed supplied: one is drawn from the caller's stream and recorded, and
  # the stream advances by exactly that one draw.
  set.seed(3)
  res <- sieve_reliability_omega(Xo, spec)
  after <- .Random.seed
  set.seed(3)
  invisible(sample.int(.Machine$integer.max, 1L))
  expect_identical(after, .Random.seed)
  expect_false(res$uncertainty$rng$supplied)
  seed <- res$uncertainty$rng$seed
  expect_identical(res$uncertainty$status, "completed")
  expect_identical(res$uncertainty$label, "Whole-cluster bootstrap percentile")
  expect_true(is.finite(res$lower) && is.finite(res$upper) &&
    res$lower < res$estimate && res$estimate < res$upper)
  # The counts exist, nest, and are counts of what happened to each
  # replication.
  cnt <- res$uncertainty$counts
  expect_identical(cnt$requested, 40L)
  expect_identical(cnt$attempted, 40L)
  expect_identical(res$uncertainty$B_attempted, 40L)
  expect_identical(res$uncertainty$B_used, cnt$used)
  expect_true(cnt$attempted >= cnt$returned && cnt$returned >= cnt$converged &&
    cnt$converged >= cnt$used && cnt$used >= cnt$admissible)
  expect_gte(cnt$used, 39L)
  draws <- res$uncertainty$draws
  expect_identical(nrow(draws), 40L)
  expect_identical(sum(draws$used), cnt$used)
  expect_identical(sum(draws$converged), cnt$converged)
  expect_identical(as.integer(cnt$outcome[["used"]]) + cnt$improper_kept, cnt$used)
  expect_true(all(draws$outcome %in% c("used", "failed", "not_converged", "inadmissible")))
  expect_identical(res$uncertainty$rows, "complete")
  expect_identical(res$uncertainty$rule, sieve_reliability_boot_rule())
  expect_true(res$uncertainty$complete)
  # The limits are the type 7 quantiles of the replications used, the
  # standard error their standard deviation, and the Monte Carlo error is
  # stated for the number used.
  used <- draws$estimate[draws$used]
  expect_equal(c(res$lower, res$upper), unname(stats::quantile(used, c(0.025, 0.975))),
    tolerance = 1e-12
  )
  expect_equal(res$se, stats::sd(used))
  expect_equal(res$uncertainty$mc_se, sieve_bootstrap_mc_error(cnt$used, 0.95) * res$se)
  # A replication refitted by hand: its rows from two plain set.seed() calls
  # (the run's seed gives one seed per replication, the replication's seed
  # gives its rows), its coefficient from the engine's point estimator.
  pool <- Xo[stats::complete.cases(Xo), ]
  for (b in c(1L, 17L, 40L)) {
    seeds <- withr::with_preserve_seed({
      set.seed(seed)
      sample.int(.Machine$integer.max, 40L)
    })
    rows <- withr::with_preserve_seed({
      set.seed(seeds[b])
      sample.int(nrow(pool), nrow(pool), replace = TRUE)
    })
    expect_identical(draws$seed[b], seeds[b])
    hand <- DMAR::reliability_omega_categorical(pool[rows, ], ci_method = "none")
    expect_identical(draws$estimate[b], hand$value[hand$term == "estimate"])
  }
  # A supplied seed leaves the caller's stream exactly as it was, and
  # replays the run.
  seeded <- sieve_reliability_spec(names(Xo), rep("ordered", 6), "F",
    ci_method = "percentile", B = 40, seed = seed, cluster = seq_len(nrow(Xo))
  )
  set.seed(99)
  before <- .Random.seed
  again <- sieve_reliability_omega(Xo, seeded)
  expect_identical(.Random.seed, before)
  expect_true(again$uncertainty$rng$supplied)
  expect_identical(again$uncertainty$draws, draws)
  expect_identical(c(again$lower, again$upper), c(res$lower, res$upper))
  # A run stopped after b replications holds exactly the first b of the full
  # run, says it was stopped, and forms no interval from too few.
  stopped <- sieve_reliability_omega(Xo, seeded, progress = function(done, total, draw) done < 12)
  expect_identical(stopped$uncertainty$B_attempted, 12L)
  expect_false(stopped$uncertainty$complete)
  expect_identical(stopped$uncertainty$draws, draws[1:12, ])
  expect_identical(stopped$uncertainty$status, "failed")
  expect_match(stopped$uncertainty$reason, "at least 39 usable replications", fixed = TRUE)
  # SIEVER's counted bootstrap forms type 7 limits, which lie inside the
  # draws at any count; the floor is about the tail the most extreme
  # replication cuts off (the reason does not say the limits could only
  # repeat the extremes, which is true of boot's rule only).
  expect_match(stopped$uncertainty$reason, "1 / (B + 1) of the bootstrap distribution", fixed = TRUE)
  expect_false(grepl("could only repeat", stopped$uncertainty$reason, fixed = TRUE))
  expect_identical(sieve_reliability_interval_label(stopped), "failed")
  expect_true(any(grepl("(stopped; 40 were requested)", stopped$code, fixed = TRUE)))
  expect_true(any(grepl("sample.int(.Machine$integer.max, 40L)[seq_len(12L)]", stopped$code, fixed = TRUE)))
  # Rows with no observed item change nothing (the pool is the complete rows).
  padded <- sieve_reliability_omega(empty_rows(Xo, 300), sieve_reliability_spec(names(Xo), rep("ordered", 6), "F",
    ci_method = "percentile", B = 40, seed = seed, cluster = seq_len(nrow(Xo) + 300L)
  ))
  expect_identical(padded$uncertainty$draws, draws)
  expect_identical(c(padded$lower, padded$upper), c(res$lower, res$upper))
  # The recorded R is plain DMAR and replays the estimate, the counts, and
  # the limits in a clean environment.
  code <- paste(res$code, collapse = "\n")
  expect_match(code, sprintf("set.seed(%d)", seed), fixed = TRUE)
  expect_match(code, "SIEVER drew this one", fixed = TRUE)
  expect_match(code, "counted replications (40 replications, 95% level)", fixed = TRUE)
  expect_match(code, "formed only from at least 39 of them", fixed = TRUE)
  expect_false(any(grepl("sieve_", res$code)))
  env <- new.env(parent = globalenv())
  env$items <- Xo
  replay <- withr::with_preserve_seed(eval(parse(text = res$code), envir = env))
  expect_identical(unname(replay[["estimate"]]), res$estimate)
  expect_identical(as.integer(replay[["attempted"]]), 40L)
  expect_identical(as.integer(replay[["used"]]), cnt$used)
  expect_equal(unname(replay[c("lower", "upper")]), c(res$lower, res$upper), tolerance = 1e-12)
  # The table and the paragraph carry the counts.
  bundle <- list(F = list(spec = seeded, omega = again, H = sieve_reliability_H(Xo, seeded), alpha = NULL))
  tab <- sieve_reliability_table(bundle)
  expect_identical(tab$boot_attempted, 40L)
  expect_identical(tab$boot_used, cnt$used)
  para <- sieve_reliability_paragraph(bundle$F)
  expect_match(para,
    sprintf(
      "Replications: 40 attempted, %d returned an estimate, %d converged, %d admissible, %d used; seed %d.",
      cnt$returned, cnt$converged, cnt$admissible, cnt$used, seed
    ),
    fixed = TRUE
  )
  expect_match(para, "Monte Carlo standard error of about", fixed = TRUE)
})

test_that("DMAR's percentile bootstrap of categorical omega keeps DMAR's counts, its warnings and its call", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  # DMAR 1.1.0 counts every replication's fate, and its percentile
  # bootstrap is SIEVER's.
  Xo <- ordered_version(six_items_n500())[1:200, ]
  spec <- sieve_reliability_spec(names(Xo), rep("ordered", 6), "F",
    ci_method = "dmar_percentile", B = 20
  )
  expect_identical(spec$ci_method, "percentile")
  expect_identical(spec$resampling, "dmar")
  expect_identical(sieve_reliability_call(spec)$args$ci_method, "percentile")
  # No seed supplied: one is drawn from the caller's stream and recorded.
  res <- withr::with_seed(3, sieve_reliability_omega(Xo, spec))
  expect_false(res$uncertainty$rng$supplied)
  expect_identical(res$uncertainty$B, 20L)
  expect_identical(res$uncertainty$B_attempted, 20L)
  expect_identical(res$uncertainty$B_used, res$uncertainty$engine_bootstrap$returned)
  expect_identical(res$uncertainty$counts$used, res$uncertainty$engine_bootstrap$returned)
  expect_identical(res$uncertainty$counts$attempted, 20L)
  # Twenty replications cannot hold a 95% percentile limit inside the
  # sample (boot returns the extreme order statistics), so
  # no interval is shown, as with SIEVER's own counted bootstrap; DMAR's
  # limits and exploratory status are kept in the record. DMAR suppresses
  # boot's duplicate endpoint warning in an exploratory run;
  # this test's explicit mechanics helper muffles only the classed
  # exploratory warning, not other engine conditions.
  expect_identical(res$uncertainty$status, "failed")
  expect_match(res$uncertainty$reason, "at least 39 usable replications", fixed = TRUE)
  expect_true(all(is.na(c(res$lower, res$upper))))
  expect_true(all(is.finite(res$uncertainty$engine_limits)))
  expect_identical(res$engine_attributes$bootstrap_status, "exploratory")
  expect_false(any(grepl("extreme order statistics", res$uncertainty$warnings)))
  expect_length(res$warnings, 0L)
  expect_identical(sieve_reliability_interval_label(res), "failed")
  seed <- res$uncertainty$rng$seed
  # The recorded call carries that seed, the engine's name for the method
  # and the processes, and running it gives DMAR's limits and status.
  code <- paste(res$code, collapse = "\n")
  expect_match(code, sprintf("seed = %d", seed), fixed = TRUE)
  expect_match(code, "ci_method = \"percentile\"", fixed = TRUE)
  expect_match(code, "ncpus = 1", fixed = TRUE)
  expect_match(code, "SIEVER drew this one", fixed = TRUE)
  items <- Xo
  expect_no_warning(replay <- eval(parse(text = code)))
  expect_identical(attr(replay, "bootstrap_status", exact = TRUE), "exploratory")
  expect_identical(replay$value[match(c("ci_lower", "ci_upper"), replay$term)], unname(res$uncertainty$engine_limits))
  expect_identical(res$uncertainty$engine_method, attr(replay, "ci_method", exact = TRUE))
  expect_identical(res$uncertainty$engine_bootstrap, attr(replay, "bootstrap", exact = TRUE))
  bundle <- list(spec = spec, omega = res, H = sieve_reliability_H(Xo, spec), alpha = NULL)
  expect_match(
    paste(sieve_reliability_notes(bundle), collapse = " "),
    "The interval for \u03c9 failed: Only 20 replications were run."
  )
  para <- sieve_reliability_paragraph(bundle)
  expect_match(para, "DMAR's counted bootstrap (DMAR::reliability_omega_categorical, B = 20, 1 process", fixed = TRUE)
  expect_match(para, sprintf("Replications: 20 attempted, %d returned an estimate", res$uncertainty$counts$returned), fixed = TRUE)
  # With enough usable draws for this exploratory adapter check, the limits
  # are DMAR's and Monte Carlo error uses the effective count.
  more <- sieve_reliability_omega(Xo, sieve_reliability_spec(names(Xo), rep("ordered", 6), "F",
    ci_method = "percentile", B = 60, seed = seed))
  expect_identical(more$uncertainty$status, "completed")
  expect_identical(more$uncertainty$label, "Bootstrap percentile")
  expect_equal(more$uncertainty$mc_se, sieve_bootstrap_mc_error(more$uncertainty$B_used, 0.95) * more$se)
  expect_match(sieve_reliability_paragraph(list(spec = more$spec, omega = more, H = bundle$H, alpha = NULL)),
    "95% Bootstrap percentile interval", fixed = TRUE)
  # The engine's BCa interval, under the engine's name; its wait counts the
  # jackknife the engine runs when there are fewer replications than rows.
  bca <- sieve_reliability_spec(names(Xo), rep("ordered", 6), "F", ci_method = "dmar_bca", B = 20, seed = 5)
  expect_identical(bca$resampling, "dmar")
  expect_identical(sieve_reliability_call(bca)$args$ci_method, "bca")
  expect_identical(bca$interval$label, "Bootstrap BCa (DMAR)")
  expect_equal(sieve_bootstrap_wait(0.1, 20, 200, "dmar_bca"), 22)
  expect_equal(sieve_bootstrap_wait(0.1, 200, 200, "dmar_bca"), 20)
  expect_equal(sieve_bootstrap_wait(0.1, 20, 200, "percentile"), 2)
})

test_that("a failed interval keeps the engine's message and still reports the coefficient", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  X <- six_items_n500()
  real <- sieve_reliability_engine
  testthat::local_mocked_bindings(
    sieve_reliability_engine = function(fun) {
      force(fun)
      function(..., ci_method) {
        if (!identical(ci_method, "none")) {
          stop("estimated adjustment 'a' is NA", call. = FALSE)
        }
        real(fun)(..., ci_method = ci_method)
      }
    }
  )
  # A resampling interval and a closed form interval take different paths
  # to the same outcome.
  for (method in c("dmar_bca", "likelihood")) {
    res <- sieve_reliability_omega(X, continuous_spec(X, ci_method = method, B = 50, seed = 1))
    expect_identical(res$status$code, "ok")
    expect_equal(res$estimate, 0.8342445, tolerance = 1e-7) # the reference omega
    expect_identical(res$uncertainty$status, "failed")
    expect_identical(res$uncertainty$reason, "estimated adjustment 'a' is NA")
    expect_true(all(is.na(c(res$lower, res$upper))))
    expect_identical(sieve_reliability_interval_label(res), "failed")
    # The recorded R says what the session saw: the call that was made, that
    # it did not form the interval, why, and the call that gave the estimate.
    code <- paste(res$code, collapse = "\n")
    expect_match(code, sprintf(
      "did not form the %s interval: estimated adjustment 'a' is NA",
      res$uncertainty$label
    ), fixed = TRUE)
    expect_match(code, "The coefficient was then estimated without an interval", fixed = TRUE)
    expect_match(code, "ci_method = \"none\"", fixed = TRUE)
    para <- sieve_reliability_paragraph(list(
      spec = res$spec, omega = res,
      H = sieve_reliability_H(X, res$spec), alpha = NULL
    ))
    expect_match(para, sprintf(
      "the %s interval could not be formed: estimated adjustment 'a' is NA",
      res$uncertainty$label
    ), fixed = TRUE)
    expect_match(
      paste(
        sieve_reliability_notes(list(
          spec = res$spec, omega = res,
          H = sieve_reliability_H(X, res$spec), alpha = NULL
        )),
        collapse = " "
      ),
      "The interval for \u03c9 failed: estimated adjustment 'a' is NA",
      fixed = TRUE
    )
  }
})

test_that("a coefficient that cannot be estimated is a failure with its reason", {
  X <- six_items_n500()
  # Categorical omega on values that are not category codes.
  bad <- sieve_reliability_omega(
    X, sieve_reliability_spec(names(X), rep("ordered", 6), "F", ci_method = "percentile", B = 400)
  )
  expect_identical(bad$status$code, "failed")
  expect_match(bad$status$message, "integer category values")
  expect_identical(bad$uncertainty$status, "failed")
  expect_true(is.na(bad$estimate))
  # Too few items, and a column that is not there.
  two <- sieve_reliability_omega(X, sieve_reliability_spec(c("x1", "x2"), rep("continuous", 2), "F"))
  expect_identical(two$status$code, "failed")
  expect_match(two$status$message, "three or more items")
  gone <- sieve_reliability_omega(X[, 1:5], continuous_spec(X))
  expect_identical(gone$status$code, "failed")
  expect_match(gone$status$message, "x6")
  # The table and the notes carry the failure instead of an unexplained NA.
  bundle <- list(F = sieve_reliability_construct(X[, 1:5], continuous_spec(X)))
  expect_identical(sieve_reliability_table(bundle)$uncertainty, "failed")
  expect_match(
    paste(sieve_reliability_notes(bundle$F), collapse = " "),
    "could not be estimated"
  )
})

test_that("the interval menu offers only what applies and defaults sensibly", {
  # The menu leads with the recommended intervals (Kelley & Pornprasertmanit, 2016),
  # and the default omega variant is robust omega. The closed form cases
  # below name the model implied variant.
  # An all categorical project: closed forms are not on the menu, and a
  # closed form selection moves to the recommended BCa with a statement.
  # (One percentile entry: DMAR 1.1.0's percentile bootstrap is counted,
  # so the engine's and SIEVER's are one call.)
  # (The first choice is DMAR's defaults, the logit-scale methods are on the
  # menu, and the bootstrap standard error on the logit scale is among the
  # bootstraps; see test-dmar-defaults.R.)
  m <- sieve_reliability_menu(c("categorical", "categorical"), selected = "mlr")
  expect_identical(unname(m$choices), c("recommended", "percentile", "dmar_bca", "bootstrap_se_logistic"))
  expect_identical(m$selected, "recommended")
  expect_true(m$changed)
  expect_match(m$note, "categorical omega")
  expect_match(m$note, "categorical omega takes DMAR's default, the BCa bootstrap", fixed = TRUE)
  expect_match(m$note, "interval method was set to \"DMAR's defaults: ", fixed = TRUE)
  m <- sieve_reliability_menu(c("categorical", "categorical"), selected = "percentile")
  expect_false(m$changed)
  expect_match(m$note, "DMAR's counted bootstrap")
  expect_match(m$note, "actual method is reported, including a percentile fallback", fixed = TRUE)
  # All continuous, model implied, complete cases: everything, and the
  # selection stands.
  m <- sieve_reliability_menu(c("continuous", "continuous"), denominator = "model_implied", selected = "likelihood")
  expect_identical(
    unname(m$choices),
    c("recommended", "mlr_logistic", "mlr", "ml_logistic", "ml", "adf_logistic", "likelihood",
      "percentile", "dmar_bca", "bootstrap_se_logistic")
  )
  expect_identical(m$selected, "likelihood")
  expect_false(m$changed)
  expect_null(m$note)
  # FIML: the profile likelihood is not offered.
  m <- sieve_reliability_menu("continuous", missing = "fiml", denominator = "model_implied", selected = "likelihood")
  expect_false("likelihood" %in% m$choices)
  expect_identical(m$selected, "recommended")
  expect_match(m$note, "Profile likelihood")
  # Robust omega: DMAR's defaults and the bootstraps only.
  m <- sieve_reliability_menu("continuous", denominator = "observed", selected = "mlr")
  expect_identical(unname(m$choices), c("recommended", "percentile", "dmar_bca", "bootstrap_se_logistic"))
  # The labels name the engine and say the replications are counted.
  methods <- sieve_reliability_methods()
  expect_identical(
    names(methods)[match(c("percentile", "dmar_bca"), methods)],
    c("Bootstrap percentile (DMAR, counted replications)", "Bootstrap BCa (DMAR, counted replications)")
  )
  # A mixed project keeps the closed forms (they apply to some constructs).
  m <- sieve_reliability_menu(c("continuous", "categorical"), denominator = "model_implied", selected = "mlr")
  expect_true(all(c("mlr", "percentile") %in% m$choices))
  expect_identical(m$selected, "mlr")
  expect_match(m$note, "mixes constructs")
})

test_that("the Monte Carlo error of a percentile limit is what simulation gives", {
  B <- 400
  # Independent check: the standard deviation, over many bootstrap sized
  # samples from a normal distribution, of the 2.5th percentile.
  q <- withr::with_seed(1, replicate(4000, stats::quantile(stats::rnorm(B), 0.025, names = FALSE)))
  expect_equal(sieve_bootstrap_mc_error(B, 0.95), stats::sd(q), tolerance = 0.08)
  # It falls with the square root of the number of replications.
  expect_equal(sieve_bootstrap_mc_error(1000) / sieve_bootstrap_mc_error(4000), 2)
  expect_equal(sieve_bootstrap_mc_error(1000), 0.0845, tolerance = 1e-3)
  expect_gt(sieve_bootstrap_mc_error(200), 2 * sieve_bootstrap_mc_error(1000))
  # The wait: one fit per replication, plus one per case for BCa when
  # there are fewer replications than cases.
  expect_equal(sieve_bootstrap_wait(0.1, 1000, 300, "percentile"), 100)
  expect_equal(sieve_bootstrap_wait(0.1, 1000, 300, "bca"), 100)
  expect_equal(sieve_bootstrap_wait(0.1, 200, 300, "bca"), 50)
  expect_equal(sieve_bootstrap_wait(c(0.1, 0.2), 1000, c(300, 300)), c(100, 200))
  expect_identical(sieve_duration_label(420), "about 7 minutes")
  expect_identical(sieve_duration_label(0.4), "under 2 seconds")
  expect_identical(sieve_duration_label(62), "about 65 seconds")
})

# ---- H from the joint covariance of the loadings ---------------------------

test_that("the gradient of H matches finite differences", {
  H <- function(l) 1 / (1 + 1 / sum(l^2 / (1 - l^2)))
  cases <- list(
    c(.5, .6, .7, .8, .65, .75), c(.3, .9, .95), c(-.4, .6, .2, .85),
    withr::with_seed(2, stats::runif(9, 0.2, 0.95))
  )
  for (l in cases) {
    d <- sieve_H_delta(l)
    expect_equal(d$estimate, H(l), tolerance = 1e-14)
    expect_equal(d$estimate, sieve_reliability_value(DMAR::reliability_H(l, ci_method = "none"), "estimate"), tolerance = 1e-14)
    eps <- 1e-6
    numeric_gradient <- vapply(seq_along(l), function(i) {
      up <- replace(l, i, l[i] + eps)
      down <- replace(l, i, l[i] - eps)
      (H(up) - H(down)) / (2 * eps)
    }, numeric(1))
    expect_equal(d$gradient, numeric_gradient, tolerance = 1e-7)
  }
  expect_error(sieve_H_delta(c(.5, 1, .6)), "strictly between")
  expect_error(sieve_H_delta(c(.5, .6), diag(3)), "one row per loading")
})

test_that("H's interval uses the joint covariance of the loadings", {
  X <- six_items_n500()
  # The reference fit: normal theory maximum likelihood, complete data.
  res <- sieve_reliability_H(X, continuous_spec(X, missing = "listwise", ci_method = "ml"))
  expect_identical(res$status$code, "ok")
  expect_identical(res$uncertainty$status, "completed")
  expect_identical(res$uncertainty$transformation, "logit")
  expect_identical(res$uncertainty$information, "expected")
  expect_identical(res$target, "observed item scores")

  # The reference numbers: H = 0.8599774, variance 0.000105589 with the full
  # covariance matrix of the standardized loadings against 0.00007720223
  # from their marginal standard errors.
  expect_equal(res$estimate, 0.8599774, tolerance = 1e-7)
  expect_equal(res$variance, 0.000105589, tolerance = 1e-6)
  fit <- lavaan::cfa("f =~ x1 + x2 + x3 + x4 + x5 + x6", data = X, std.lv = TRUE)
  std <- lavaan::standardizedSolution(fit)
  std <- std[std$op == "=~", ]
  marginal <- sieve_capture(DMAR::reliability_H(std$est.std, se_loadings = std$se))
  expect_true(any(grepl("marginal standard errors", marginal$warnings, fixed = TRUE)))
  diagonal <- marginal$value
  diagonal_var <- diagonal$value[diagonal$term == "se"]^2
  expect_equal(diagonal_var, 0.00007720223, tolerance = 1e-6)
  expect_equal(diagonal_var / res$variance, 0.731158, tolerance = 1e-5)
  # The marginal standard errors are the square roots of the diagonal of
  # the matrix used, so the only difference is the covariances.
  expect_equal(sqrt(diag(res$vcov)), std$se, tolerance = 1e-10, ignore_attr = TRUE)
  expect_equal(unname(res$loadings), std$est.std, tolerance = 1e-10)
  expect_gt(max(abs(res$vcov[upper.tri(res$vcov)])), 1e-5)

  # Independent route to the same variance: the delta method through the
  # unstandardized loadings and residual variances, as a defined parameter.
  # H is one function of the model, so the two parameterizations must give
  # the same standard error.
  model <- paste0(
    "f =~ NA*x1 + ", paste0("l", 1:6, "*x", 1:6, collapse = " + "), "\n",
    "f ~~ 1*f\n",
    paste0("x", 1:6, " ~~ e", 1:6, "*x", 1:6, collapse = "\n"), "\n",
    "H := 1 / (1 + 1 / (", paste0("l", 1:6, "^2/e", 1:6, collapse = " + "), "))"
  )
  raw <- lavaan::parameterEstimates(lavaan::cfa(model, data = X))
  raw <- raw[raw$lhs == "H" & raw$op == ":=", ]
  expect_equal(res$estimate, raw$est, tolerance = 1e-8)
  expect_equal(res$se, raw$se, tolerance = 1e-6)

  # The interval: logit scale, so it contains the estimate and stays inside
  # (0, 1); the reference is [0.83860, 0.87893] for the full variance.
  expect_equal(c(res$lower, res$upper), c(0.8386002797, 0.8789319720), tolerance = 1e-8)
  expect_true(0 < res$lower && res$lower < res$estimate && res$estimate < res$upper &&
    res$upper < 1)
  crit <- stats::qnorm(0.975)
  expect_equal(stats::qlogis(c(res$lower, res$upper)),
    stats::qlogis(res$estimate) +
      c(-1, 1) * crit * res$se / (res$estimate * (1 - res$estimate)),
    tolerance = 1e-10
  )
})

test_that("H's interval stays inside (0, 1) near the boundary", {
  l <- rep(0.985, 6)
  V <- diag(0.02^2, 6) + 0.0001
  d <- sieve_H_delta(l, V, conf_level = 0.99)
  expect_gt(d$estimate, 0.99)
  expect_true(0 < d$lower && d$lower < d$estimate && d$estimate < d$upper && d$upper < 1)
  # The symmetric Wald limit on the coefficient scale would leave the range.
  expect_gt(d$estimate + stats::qnorm(0.995) * d$se, 1)
})

test_that("H uses the model that matches the item types", {
  X <- six_items_n500()
  Xo <- ordered_version(X)
  ord <- sieve_reliability_H(Xo, sieve_reliability_spec(names(Xo), rep("ordered", 6), "F"))
  cont <- sieve_reliability_H(Xo, sieve_reliability_spec(names(Xo), rep("continuous", 6), "F"))
  # A continuous model fit whatever the ledger said would make these two
  # identical (0.8197759).
  expect_identical(ord$model$estimator, "WLSMV")
  expect_identical(ord$model$ordered, names(Xo))
  expect_identical(ord$label, "H (latent responses)")
  expect_identical(ord$target, "latent response variables")
  # (Labels name their target and use DMAR's denominator wording.)
  expect_identical(cont$label, "H (observed item scores)")
  expect_gt(abs(ord$estimate - cont$estimate), 0.01)
  expect_identical(ord$uncertainty$status, "completed")

  # Independent check of the categorical H and its standard error: with
  # the delta parameterization the loadings are the standardized loadings
  # of the latent responses, so H is a defined parameter of them.
  model <- paste0(
    "f =~ NA*x1 + ", paste0("l", 1:6, "*x", 1:6, collapse = " + "), "\n",
    "f ~~ 1*f\n",
    "H := 1 / (1 + 1 / (", paste0("l", 1:6, "^2/(1 - l", 1:6, "^2)", collapse = " + "), "))"
  )
  ref <- lavaan::parameterEstimates(
    lavaan::cfa(model, data = Xo, ordered = names(Xo), estimator = "WLSMV")
  )
  ref <- ref[ref$lhs == "H" & ref$op == ":=", ]
  expect_equal(ord$estimate, ref$est, tolerance = 1e-8)
  expect_equal(ord$se, ref$se, tolerance = 1e-6)
  expect_true(ord$lower < ord$estimate && ord$estimate < ord$upper)

  # Ordered items never get FIML; H says which rows it used and why.
  Xo[1:25, 3] <- NA
  miss <- sieve_reliability_H(
    Xo, sieve_reliability_spec(names(Xo), rep("ordered", 6), "F", missing = "fiml")
  )
  expect_identical(miss$missing$used, "listwise")
  expect_match(miss$missing$reason, "complete cases")
  expect_identical(miss$n$used, 475L)

  # A construct that mixes types: the ordered items are modeled as ordered.
  Xm <- X
  Xm$x5 <- Xo$x5
  Xm$x6 <- Xo$x6
  mix <- sieve_reliability_H(
    Xm, sieve_reliability_spec(names(Xm), c(rep("continuous", 4), "ordered", "ordered"), "F")
  )
  expect_identical(mix$model$ordered, c("x5", "x6"))
  expect_match(mix$label, "latent responses for the ordered items")
  expect_true(is.finite(mix$estimate))
})

test_that("H under FIML uses the joint covariance from the observed information", {
  X <- six_items_n500()
  X[withr::with_seed(11, sample.int(500, 200)), 4:6] <- NA
  res <- sieve_reliability_H(X, continuous_spec(X, missing = "fiml", ci_method = "ml"))
  expect_identical(res$uncertainty$information, "observed")
  expect_identical(res$n$used, 500L)
  model <- paste0(
    "f =~ NA*x1 + ", paste0("l", 1:6, "*x", 1:6, collapse = " + "), "\n",
    "f ~~ 1*f\n",
    paste0("x", 1:6, " ~~ e", 1:6, "*x", 1:6, collapse = "\n"), "\n",
    "H := 1 / (1 + 1 / (", paste0("l", 1:6, "^2/e", 1:6, collapse = " + "), "))"
  )
  raw <- lavaan::parameterEstimates(lavaan::cfa(model, data = X, missing = "fiml"))
  raw <- raw[raw$lhs == "H" & raw$op == ":=", ]
  expect_equal(res$estimate, raw$est, tolerance = 1e-7)
  expect_equal(res$se, raw$se, tolerance = 1e-5)
  # Robust standard errors unless the normal theory interval was asked for.
  robust <- sieve_reliability_H(X, continuous_spec(X, missing = "fiml", ci_method = "mlr"))
  expect_identical(robust$model$estimator, "MLR")
  expect_identical(robust$uncertainty$se_type, "robust.huber.white")
  expect_equal(robust$estimate, res$estimate, tolerance = 1e-8)
})

test_that("an improper solution has no H, and omega says it is improper", {
  # A three item sample whose one factor solution has a negative residual
  # variance (found by search; the data are regenerated from the seed).
  Y <- withr::with_seed(11, {
    z <- stats::rnorm(40)
    data.frame(
      a = .9 * z + sqrt(1 - .81) * stats::rnorm(40),
      b = .4 * z + sqrt(1 - .16) * stats::rnorm(40),
      c = .4 * z + sqrt(1 - .16) * stats::rnorm(40)
    )
  })
  # Model implied omega named explicitly: the default variant is robust
  # omega.
  spec <- sieve_reliability_spec(names(Y), rep("continuous", 3), "F", ci_method = "ml", denominator = "model_implied")
  h <- sieve_reliability_H(Y, spec)
  expect_identical(h$status$code, "inadmissible")
  expect_true(is.na(h$estimate))
  expect_match(h$error, "improper")
  expect_gt(max(abs(h$loadings)), 1)
  om <- sieve_reliability_omega(Y, spec)
  expect_identical(om$status$code, "inadmissible")
  expect_match(om$status$message, "Heywood")
  expect_true(is.finite(om$estimate))
  notes <- sieve_reliability_notes(list(spec = spec, omega = om, H = h, alpha = NULL))
  expect_match(paste(notes, collapse = " "), "Heywood")
  expect_match(paste(notes, collapse = " "), "H could not be estimated")
})

# ---- the module only displays what the functions return --------------------

test_that("the module table is the functions' result at full precision", {
  st <- make_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "model_implied", ci_method = "mlr",
      missing = "listwise", conf_level = 0.95,
      show_alpha = TRUE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    df <- reliab()
    items <- shiny::isolate(state$items)
    meta <- shiny::isolate(state$meta)
    for (cn in df$construct) {
      its <- meta$item[meta$construct == cn]
      spec <- sieve_reliability_spec(its, rep("continuous", length(its)), cn,
        missing = "listwise", ci_method = "mlr", denominator = "model_implied"
      )
      direct <- sieve_reliability_construct(items, spec)
      row <- df[df$construct == cn, ]
      expect_identical(
        c(row$omega, row$ci_lower, row$ci_upper),
        c(direct$omega$estimate, direct$omega$lower, direct$omega$upper)
      )
      expect_identical(
        c(row$H, row$H_lower, row$H_upper),
        c(direct$H$estimate, direct$H$lower, direct$H$upper)
      )
      expect_identical(row$alpha, direct$alpha$estimate)
      # Under the same one factor model H is not below omega.
      expect_gte(row$H, row$omega - 1e-8)
    }
    expect_true(all(df$uncertainty == "completed"))
    expect_true(all(df$interval == "MLR (sandwich) SE (delta method)"))
    expect_true(all(df$missing == "complete cases"))
    expect_identical(df$n_used, rep(301L, 3))
    expect_no_error(output$omega_tbl)
    expect_no_error(output$detail_tbl)
    expect_no_error(output$omega_plot)
  })
})

test_that("the recorded reliability script reproduces the table", {
  st <- make_state()
  shiny::isolate({
    items <- st$items
    items[c(2, 30, 77), 1] <- NA
    items[c(5, 30, 140, 200), 7] <- NA
    st$items <- items
    sieve_confirm_types(st)
  })
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "model_implied", ci_method = "mlr",
      missing = "fiml", conf_level = 0.9,
      show_alpha = TRUE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    df <- reliab()
    expect_true(all(df$missing == "FIML"))
    expect_true(all(df$uncertainty == "completed"))
    log <- shiny::isolate(state$codelog)
    code <- log[!startsWith(log, "# [")]
    expect_true(any(grepl("missing = \"fiml\"", code, fixed = TRUE)))
    expect_true(any(grepl("conf_level = 0.9)", code, fixed = TRUE)))
    env <- new.env(parent = globalenv())
    assign("items", shiny::isolate(state$items), envir = env)
    assign("d", shiny::isolate(state$raw), envir = env)
    exprs <- parse(text = paste(code, collapse = "\n"))
    replay <- sieve_capture(lapply(exprs, function(e) eval(e, envir = env)))
    expect_true(replay$ok, info = replay$error)
    expect_true(all(grepl("factor 1000", replay$warnings, fixed = TRUE)))
    values <- replay$value
    omegas <- Filter(function(v) inherits(v, "dmar_reliability") &&
      identical(attr(v, "coefficient"), "omega"), values)
    alphas <- Filter(function(v) inherits(v, "dmar_reliability") &&
      identical(attr(v, "coefficient"), "alpha"), values)
    # (H is DMAR::reliability_H()'s, recorded as that call, so H's replay is
    # a DMAR table, and no helper of SIEVER's is recorded.)
    hs <- Filter(function(v) inherits(v, "dmar_reliability") &&
      identical(attr(v, "coefficient"), "H"), values)
    expect_length(omegas, 3L)
    expect_length(alphas, 3L)
    expect_length(hs, 3L)
    term <- function(v, name) v$value[v$term == name]
    expect_identical(vapply(omegas, term, numeric(1), name = "estimate"), df$omega)
    expect_identical(vapply(omegas, term, numeric(1), name = "ci_lower"), df$ci_lower)
    expect_identical(vapply(omegas, term, numeric(1), name = "ci_upper"), df$ci_upper)
    expect_identical(vapply(alphas, term, numeric(1), name = "estimate"), df$alpha)
    expect_identical(vapply(hs, term, numeric(1), name = "estimate"), df$H)
    expect_identical(vapply(hs, term, numeric(1), name = "ci_lower"), df$H_lower)
    expect_identical(vapply(hs, term, numeric(1), name = "ci_upper"), df$H_upper)
    # No helper is recorded, however often the analysis is rerun.
    session$setInputs(conf_level = 0.95)
    session$setInputs(run_go = 2)
    invisible(reliab())
    log <- shiny::isolate(state$codelog)
    expect_identical(sum(startsWith(log, "h_interval <- function")), 0L)
    expect_true(any(grepl("DMAR::reliability_H(data = h_data", log, fixed = TRUE)))
  })
})

test_that("the session script with reliability steps runs in a clean session", {
  # Load through the Ingest module, as a user does, so the script starts with
  # the recorded loading step and the preamble that rebuilds `items`.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9")
    session$setInputs(load = 1)
    session$setInputs(confirm = 1)
  })
  box <- new.env()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "model_implied", ci_method = "ml",
      missing = "listwise", conf_level = 0.95,
      show_alpha = FALSE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    box$table <- reliab()
  })
  shiny::testServer(mod_repro_server, args = list(state = st), {
    box$script <- script()
  })
  lines <- strsplit(box$script, "\n", fixed = TRUE)[[1]]
  at_pre <- which(grepl(".siever_store <- unserialize", lines, fixed = TRUE))
  at_rel <- which(grepl("DMAR::reliability_omega(data = items[", lines, fixed = TRUE))
  expect_length(at_pre, 1L)
  expect_length(at_rel, 3L)
  expect_true(all(at_pre < at_rel))
  env <- new.env(parent = globalenv())
  values <- lapply(parse(text = box$script), function(e) eval(e, envir = env))
  # Alpha is recorded with every run (the checkbox only
  # shows or hides its column), so the omega objects are told from it.
  omegas <- Filter(function(v) inherits(v, "dmar_reliability") &&
    identical(attr(v, "coefficient"), "omega"), values)
  hs <- Filter(function(v) inherits(v, "dmar_reliability") &&
    identical(attr(v, "coefficient"), "H"), values)
  expect_length(omegas, 3L)
  expect_identical(
    vapply(omegas, function(v) v$value[v$term == "estimate"], numeric(1)),
    box$table$omega
  )
  expect_identical(
    vapply(omegas, function(v) v$value[v$term == "ci_upper"], numeric(1)),
    box$table$ci_upper
  )
  expect_identical(vapply(hs, function(v) v$value[v$term == "ci_lower"], numeric(1)), box$table$H_lower)
  # The normal theory request carries through to the model behind H.
  expect_true(any(grepl("estimator = \"ML\"", lines, fixed = TRUE)))
})

test_that("a project of ordered constructs defaults to the counted bootstrap and runs it on request", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  st <- make_ordered(make_state())
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    # A closed form method is not on the menu for categorical omega; the
    # selection moves to the first method that applies, the recommended
    # interval:
    # BCa for categorical omega (Kelley & Pornprasertmanit, 2016).
    session$setInputs(
      denominator = "model_implied", ci_method = "mlr",
      missing = "listwise", conf_level = 0.95, B = 40, boot_seed = NA,
      show_alpha = FALSE, score_method = "Bartlett"
    )
    expect_identical(menu()$selected, "recommended")
    # (One percentile entry: DMAR 1.1.0's percentile bootstrap is counted,
    # so the engine's and SIEVER's are the same call; DMAR's bootstrap
    # standard error on the logit scale is on the menu.)
    expect_identical(unname(menu()$choices), c("recommended", "percentile", "dmar_bca", "bootstrap_se_logistic"))
    expect_match(as.character(output$method_note$html), "categorical omega")
    expect_match(as.character(output$method_note$html), "BCa bootstrap")
    session$setInputs(ci_method = "percentile")
    expect_match(as.character(output$method_note$html), "DMAR's counted bootstrap")
    session$setInputs(run_go = 1)
    df <- reliab()
    # (Labels name their target and use DMAR's denominator wording.)
    expect_true(all(df$coefficient == categorical_omega_label("model_implied")))
    expect_true(all(is.finite(df$omega)))
    # Nothing is resampled until it is asked for; the row says so.
    expect_true(all(df$uncertainty == "pending"))
    expect_true(all(df$interval == "bootstrap not run yet (Bootstrap percentile, 40 replications)"))
    expect_true(all(is.na(df$ci_lower)))
    # One paragraph per construct, each saying so.
    notes <- as.character(output$status_notes$html)
    expect_identical(lengths(regmatches(notes, gregexpr("interval: bootstrap not run yet", notes))), 3L)
    for (cn in df$construct) {
      expect_match(notes, sprintf("<b>%s: </b>", cn), fixed = TRUE)
    }
    # The wait and the cost of B are estimated before anything runs, and
    # the note says the run can be stopped.
    note <- as.character(output$boot_note$html)
    expect_match(note, "40 replications and 3 constructs")
    expect_match(note, "should take about")
    expect_match(note, "Monte Carlo standard error")
    # (DMAR's bootstrap in a background process, stopped at the next
    # replication through its progress hook.)
    expect_match(note, "Cancel ends the bootstrap at its next replication")
    expect_match(note, "half rule")
    expect_identical(boot_plan()$constructs, df$construct)
    expect_true(all(is.finite(boot_plan()$seconds) & boot_plan()$seconds > 0))

    session$setInputs(run_boot = 1)
    # The run is in progress, not finished inside the click: the page shows
    # the run and the Stop button first, then polls the background process.
    expect_true(boot_running())
    expect_true(all(reliab()$uncertainty == "pending"))
    expect_match(as.character(output$stop_boot_ui$html), "Cancel the bootstrap")
    # A run still going at the limit fails here, saying so.
    siever_wait_until(function() !isTRUE(boot_running()), what = "The page's background bootstrap",
      step = function() {
        session$elapse(200)
        Sys.sleep(0.05)
      })
    expect_false(boot_running())
    done <- reliab()
    expect_true(all(done$uncertainty == "completed"))
    expect_true(all(done$interval == "Bootstrap percentile"))
    expect_true(all(done$ci_lower < done$omega & done$omega < done$ci_upper))
    expect_identical(done$omega, df$omega)
    expect_identical(done$boot_attempted, rep(40L, 3))
    # A 95% percentile interval is shown from at least 39 used replications.
    expect_true(all(done$boot_used >= 39L & done$boot_used <= 40L))
    expect_length(boot_plan()$constructs, 0L)
    # The seed was drawn, recorded, shared by the constructs, and written
    # into the logged step with the counts.
    res <- results()
    seed <- res[[1]]$omega$uncertainty$rng$seed
    expect_false(res[[1]]$omega$uncertainty$rng$supplied)
    expect_true(all(vapply(res, function(r) r$omega$uncertainty$rng$seed, integer(1)) == seed))
    cnt <- res[["Visual"]]$omega$uncertainty$counts
    notes <- as.character(output$status_notes$html)
    expect_match(notes,
      sprintf(
        "Replications: 40 attempted, %d returned an estimate, %d converged, %d admissible, %d used; seed %d, drawn for this run and recorded.",
        cnt$returned, cnt$converged, cnt$admissible, cnt$used, seed
      ),
      fixed = TRUE
    )
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, sprintf("seed = %d", seed), fixed = TRUE)
    expect_match(log, "DMAR::reliability_omega_categorical(", fixed = TRUE)
    expect_match(log, "B = 40", fixed = TRUE)
    expect_match(log, sprintf(
      "Visual: 40 attempted, %d converged, %d admissible, %d used",
      cnt$converged, cnt$admissible, cnt$used
    ), fixed = TRUE)
    expect_match(as.character(output$footnote$html), sprintf("seed %d", seed), fixed = TRUE)
    expect_match(as.character(output$footnote$html), "attempted, converged, admissible, and used", fixed = TRUE)
    # Replaying the recorded code of a construct (DMAR's call) gives the
    # interval shown.
    env <- new.env(parent = globalenv())
    env$items <- shiny::isolate(state$items)
    replay <- suppressWarnings(withr::with_preserve_seed(eval(parse(text = res[["Visual"]]$omega$code), envir = env)))
    expect_identical(replay$value[match(c("ci_lower", "ci_upper"), replay$term)],
      c(done$ci_lower[done$construct == "Visual"], done$ci_upper[done$construct == "Visual"])
    )
    expect_identical(attr(replay, "B_used"), done$boot_used[done$construct == "Visual"])
    # The detail table carries the counts.
    detail <- paste(as.character(unlist(output$detail_tbl)), collapse = " ")
    expect_match(detail, "Bootstrap replications attempted", fixed = TRUE)
    expect_match(detail, "Bootstrap replications used", fixed = TRUE)

    # A completed bootstrap belongs to its request: change the request and
    # the interval is pending again, not carried over.
    session$setInputs(conf_level = 0.9)
    session$setInputs(run_go = 2)
    expect_true(all(reliab()$uncertainty == "pending"))
    session$setInputs(conf_level = 0.95)
    expect_identical(reliab()$ci_lower, done$ci_lower)
  })
  # The default number of replications is not lowered for the categorical path.
  # (DMAR's B = 10000.)
  ui <- as.character(mod_reliab_ui("reliab"))
  expect_match(ui, "id=\"reliab-B\" type=\"number\" class=\"shiny-input-number form-control\" value=\"10000\"",
    fixed = TRUE
  )
})

test_that("a project mixing continuous and ordered constructs is resolved per construct", {
  # Explicit reduced-B plumbing/replay mechanics; production counts are checked separately.
  siever_local_dmar_mechanics()
  st <- make_ordered(make_state(), constructs = "Speed")
  shiny::isolate({
    items <- st$items
    items[c(4, 9, 16, 25), "t1_visual_perception"] <- NA
    items[c(8, 27, 64), "t10_addition"] <- NA
    st$items <- items
    sieve_confirm_types(st)
  })
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "model_implied", ci_method = "mlr",
      missing = "fiml", conf_level = 0.95,
      show_alpha = TRUE, score_method = "Bartlett"
    )
    expect_identical(menu()$selected, "mlr")
    expect_match(as.character(output$method_note$html), "mixes constructs")
    session$setInputs(run_go = 1)
    df <- reliab()
    cont <- df[df$construct != "Speed", ]
    speed <- df[df$construct == "Speed", ]
    # Continuous constructs: omega by FIML with the robust interval.
    # (Labels name their target and use DMAR's denominator wording.)
    expect_true(all(cont$coefficient == "Omega (model-implied denominator)"))
    expect_true(all(cont$missing == "FIML"))
    expect_true(all(cont$uncertainty == "completed"))
    expect_true(all(is.finite(cont$ci_lower)))
    expect_identical(cont$n_used[cont$construct == "Visual"], 301L)
    expect_identical(cont$n_complete[cont$construct == "Visual"], 297L)
    expect_identical(cont$n_patterns[cont$construct == "Visual"], 2L)
    # The ordered construct: categorical omega on complete cases, no closed
    # form interval, and both facts stated.
    expect_identical(speed$coefficient, categorical_omega_label("model_implied"))
    expect_identical(speed$missing, "complete cases")
    expect_identical(speed$n_used, 298L)
    expect_identical(speed$uncertainty, "unavailable")
    expect_identical(speed$interval, "not available for this coefficient")
    expect_true(is.finite(speed$omega) && is.na(speed$ci_lower))
    expect_identical(speed$H_target, "latent response variables")
    notes <- as.character(output$status_notes$html)
    expect_match(notes, "Speed")
    expect_match(notes, "complete cases were used for this construct")
    expect_match(notes, "no closed form interval")
    # One paragraph per construct: the continuous constructs have their own,
    # which say nothing about complete cases or a missing closed form.
    expect_match(notes, "<b>Visual: </b>", fixed = TRUE)
    visual <- sieve_reliability_paragraph(results()[["Visual"]])
    expect_false(grepl("complete cases were used for this construct", visual, fixed = TRUE))
    expect_false(grepl("no closed form interval", visual, fixed = TRUE))
    # The logged calls differ by construct, as the analyses did.
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "missing_aux <- setdiff", fixed = TRUE)
    expect_match(log, 'DMAR::reliability_omega(data = (function(x)', fixed = TRUE)
    expect_match(log, 'aux = "age_years"', fixed = TRUE)
    expect_match(log, "ci_method = \"mlr\", denominator = \"model_implied\", missing = \"fiml\"", fixed = TRUE)
    expect_match(log, "DMAR::reliability_omega_categorical(data = items[, c(\"t10_addition\"", fixed = TRUE)
    expect_match(log, "ordered = c(\"t10_addition\"", fixed = TRUE)

    # With a bootstrap selected, every construct gets an interval on request.
    session$setInputs(ci_method = "percentile", B = 45, boot_seed = 21)
    session$setInputs(run_go = 2)
    expect_true(all(reliab()$uncertainty == "pending"))
    session$setInputs(run_boot = 1)
    # A run still going at the limit fails here, saying so.
    siever_wait_until(function() !isTRUE(boot_running()), what = "The page's background bootstrap",
      step = function() {
        session$elapse(200)
        Sys.sleep(0.05)
      })
    boot <- reliab()
    expect_true(all(boot$uncertainty == "completed"))
    expect_true(all(is.finite(boot$ci_lower) & is.finite(boot$ci_upper)))
    expect_identical(boot$boot_attempted, rep(45L, 3))
    expect_true(all(boot$boot_used >= 39L))
    expect_true(all(vapply(results(), function(r) r$omega$uncertainty$rng$supplied, logical(1))))
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "seed = 21", fixed = TRUE)
    # Each construct's recorded bootstrap is its own DMAR call:
    # reliability_omega for the continuous constructs, by FIML, and
    # reliability_omega_categorical for the ordered one, each with the
    # interval, B, the seed and the processes.
    codes <- lapply(results(), function(r) paste(r$omega$code, collapse = "\n"))
    expect_match(codes[["Visual"]], "DMAR::reliability_omega(data = ", fixed = TRUE)
    expect_match(codes[["Visual"]],
      "ci_method = \"percentile\", denominator = \"model_implied\", missing = \"fiml\"",
      fixed = TRUE
    )
    expect_match(codes[["Speed"]], "DMAR::reliability_omega_categorical(data = ", fixed = TRUE)
    for (cn in c("Visual", "Speed")) {
      expect_match(codes[[cn]], "B = 45, seed = 21, ncpus = ", fixed = TRUE)
    }
  })
})

test_that("the Reliability tab's method text says what is computed", {
  ui <- as.character(mod_reliab_ui("reliab"))
  # Robust ML: the sandwich actually used under each treatment, and lavaan's
  # MLR named for what it is.
  expect_match(ui, "Huber-White", fixed = TRUE)
  expect_match(ui, "Yuan-Bentler type scaled test statistic", fixed = TRUE)
  expect_match(ui, "not the Satorra-Bentler test", fixed = TRUE)
  expect_false(grepl("robust (Satorra-Bentler) standard error", ui, fixed = TRUE))
  # Alpha: a lower bound only under its assumptions.
  expect_match(ui, "not a general lower bound", fixed = TRUE)
  expect_match(ui, "essentially tau-equivalent", fixed = TRUE)
  expect_false(grepl("legacy lower bound", ui, fixed = TRUE))
  # No promise of an interval the method cannot deliver, and no description
  # of the covariance shortcut.
  expect_false(grepl("every coefficient carries a confidence interval", ui, fixed = TRUE))
  expect_false(grepl("passes the full-information ML covariance", ui, fixed = TRUE))
  expect_match(ui, "a respondent with no observed item or auxiliary contributes nothing", fixed = TRUE)
  expect_match(ui, 'id="reliab-boot_main"', fixed = TRUE)
  # The bootstraps are DMAR's counted bootstrap (DMAR 1.1.0), its rule is
  # stated, and the cluster exception is named.
  expect_match(ui, "All three are DMAR's counted bootstrap (DMAR 1.1.0), run in a background R process", fixed = TRUE)
  expect_match(ui, "forms no interval when fewer than half of the replications return a value", fixed = TRUE)
  expect_match(ui, "With a declared cluster design SIEVER's own counted bootstrap resamples whole clusters", fixed = TRUE)
  # DMAR supports a jackknife and records a percentile fallback.
  expect_match(ui, "actual method is reported if BCa falls back to percentile", fixed = TRUE)
  expect_match(ui, "Bootstrap percentile (DMAR, counted replications)", fixed = TRUE)
  expect_match(ui, "Bootstrap BCa (DMAR, counted replications)", fixed = TRUE)
})

# ---- the fit behind H carries a status, under the construct's name ---------

test_that("the one factor model behind H goes through the status contract under the construct's own name", {
  X <- six_items_n500()
  h <- sieve_reliability_H(X, continuous_spec(X, ci_method = "mlr"))
  expect_s3_class(h$model$spec, "sieve_spec")
  expect_identical(h$model$spec$kind, "reliability_H")
  expect_identical(h$model$latent, "F")
  expect_identical(h$model$spec$latent, "F")
  expect_identical(h$model$spec$observed, names(X))
  # With complete cases a robust ML request fits H by ML with lavaan's robust.sem
  # standard error, the one the engine's robust interval for omega uses, so
  # one request gives one standard error type (MLR's Huber-White sandwich
  # remains the one under FIML, where the engine uses it too).
  expect_identical(h$model$spec$estimator, "ML")
  expect_identical(h$model$spec$engine_args$se, "robust.sem")
  expect_identical(h$model$spec$missing, "listwise")
  expect_true(h$model$spec$std_lv)
  # The status is the one every fit wrapper reports, read from the fit.
  expect_identical(h$status$code, "ok")
  expect_match(h$status$message, "Converged and admissible", fixed = TRUE)
  expect_true(is.finite(h$status$df) && h$status$df > 0)
  expect_true(h$status$se_available)
  expect_identical(h$uncertainty$status, "completed")
  # The construct's label names the latent variable, in the fit and in the
  # recorded R, and the normal theory request carries to the estimator.
  named <- sieve_reliability_spec(names(X), rep("continuous", 6), "Attention", ci_method = "ml")
  hn <- sieve_reliability_H(X, named)
  expect_identical(hn$model$latent, "Attention")
  expect_identical(hn$model$model, "Attention =~ x1 + x2 + x3 + x4 + x5 + x6")
  expect_identical(hn$model$spec$estimator, "ML")
  expect_identical(unname(hn$model$spec$name_map), "Attention")
  expect_identical(lavaan::lavNames(lavaan::lavaanify(hn$model$model), "lv"), "Attention")
  expect_true(any(grepl("lavaan::cfa(\"Attention =~ x1", hn$code, fixed = TRUE)))
  expect_identical(hn$estimate, h$estimate)
  # An item that carries the label: the latent name keeps clear of it, the
  # identity check of the wrapper passes, and H is the same number.
  Xe <- X
  names(Xe)[1] <- "Attention"
  he <- sieve_reliability_H(Xe, sieve_reliability_spec(names(Xe), rep("continuous", 6), "Attention",
    ci_method = "ml"
  ))
  expect_identical(he$model$latent, "F_Attention")
  expect_identical(he$model$spec$observed[1], "Attention")
  expect_identical(he$status$code, "ok")
  expect_identical(he$estimate, hn$estimate)
  expect_identical(he$se, hn$se)
  # A label that is not usable as a name gets a usable one and is still
  # shown as itself.
  hu <- sieve_reliability_H(X, sieve_reliability_spec(names(X), rep("continuous", 6),
    "\u03c8\u03c5\u03c7\u03ae 1",
    ci_method = "ml"
  ))
  expect_true(sieve_name_usable(hu$model$latent))
  expect_identical(hu$model$label_shown, "\u03c8\u03c5\u03c7\u03ae 1")
  expect_identical(hu$status$code, "ok")
  # No construct label: a plain F.
  expect_identical(sieve_reliability_H(X, sieve_reliability_spec(names(X), rep("continuous", 6),
    ci_method = "ml"
  ))$model$latent, "F")
  # Ordered items: the same contract, WLSMV on complete cases.
  Xo <- ordered_version(X)
  ho <- sieve_reliability_H(Xo, sieve_reliability_spec(names(Xo), rep("ordered", 6), "Attention",
    missing = "fiml", ci_method = "percentile"
  ))
  expect_identical(ho$model$spec$estimator, "WLSMV")
  expect_identical(ho$model$spec$ordered, names(Xo))
  expect_identical(ho$model$spec$missing, "listwise")
  expect_identical(ho$status$code, "ok")
  expect_identical(ho$model$label, "H (latent responses)")
  # A fit the engine refuses is a failure with the engine's reason and no
  # number, through the same contract.
  Xz <- X
  Xz$x6 <- 1
  hz <- sieve_reliability_H(Xz, continuous_spec(Xz, ci_method = "ml"))
  expect_false(is.finite(hz$estimate))
  expect_false(hz$status$code %in% c("ok", "inadmissible"))
  expect_true(nzchar(hz$status$message))
  expect_match(hz$uncertainty$reason, "no interval was formed", fixed = TRUE)
})

test_that("the Reliability tab's method note is one paragraph per construct, naming each model and its status", {
  st <- make_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "model_implied", ci_method = "mlr",
      missing = "fiml", conf_level = 0.95,
      show_alpha = TRUE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    res <- results()
    notes <- as.character(output$status_notes$html)
    expect_match(notes, "Method, construct by construct", fixed = TRUE)
    for (cn in names(res)) {
      p <- sieve_reliability_paragraph(res[[cn]])
      expect_match(notes, sprintf("<b>%s: </b>", cn), fixed = TRUE)
      expect_match(notes, htmltools::htmlEscape(p), fixed = TRUE)
      # (The label names its target, in DMAR's denominator wording.)
      expect_match(p, "^Omega \\(model-implied denominator\\) 0\\.[0-9]{3} \\[0\\.[0-9]{3}, 0\\.[0-9]{3}\\]; 95% MLR \\(sandwich\\) SE \\(delta method\\) interval")
      expect_match(p,
        "From the one factor model of the item scores, with the total variance the model reproduces (model-implied denominator), fitted by DMAR::reliability_omega on 301 rows used (full information maximum likelihood",
        fixed = TRUE
      )
      expect_match(p, "the solution converged and is admissible", fixed = TRUE)
      expect_match(p,
        sprintf(
          "from the one factor model %s (MLR, full information maximum likelihood, 301 rows): converged and admissible",
          res[[cn]]$H$model$model
        ),
        fixed = TRUE
      )
      # (Alpha is named "Coefficient Alpha" with its symbol, and carries its
      # own interval and explicit analytic N.)
      expect_match(
        p,
        "Coefficient Alpha, \u03b1 = 0\\.[0-9]{3} \\[0\\.[0-9]{3}, 0\\.[0-9]{3}\\] on 301 rows, for comparison only"
      )
      expect_match(p, "Interval: ML (delta method; analytic alpha); completed", fixed = TRUE)
      expect_identical(res[[cn]]$H$model$latent, cn)
    }
    # Nothing in the paragraph is a decision; the numbers are formatted for
    # reading only and the table underneath is at full precision.
    df <- reliab()
    expect_true(all(df$H_status == "ok"))
    expect_true(all(nchar(sub("^.*\\.", "", format(df$omega, digits = 15))) > 3))
    # The detail table names the model behind H and its status, and holds
    # the bootstrap columns.
    detail <- paste(as.character(unlist(output$detail_tbl)), collapse = " ")
    expect_match(detail, "Model behind H", fixed = TRUE)
    expect_match(detail, "Status of the model behind H", fixed = TRUE)
    expect_match(detail, "Bootstrap replications used", fixed = TRUE)
  })
})

# ---- displays that must show the researcher's labels, and defaults ---------

test_that("factor score columns carry the researcher's labels in the table and the download", {
  st <- make_state()
  labels <- c("\u03c8\u03c5\u03c7\u03ae", "Zweck \u00fc", "t2_cubes")
  shiny::isolate({
    meta <- st$meta
    meta$construct[meta$construct == "Visual"] <- labels[1]
    meta$construct[meta$construct == "Textual"] <- labels[2]
    meta$construct[meta$construct == "Speed"] <- labels[3]
    st$meta <- meta
    sieve_confirm_types(st)
  })
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(score_method = "Bartlett")
    session$setInputs(scores_go = 1)
    fs <- fscores()
    expect_identical(names(fs), labels)
    # The fit itself uses allocated names (one label is an item name, two
    # are not syntactic), and none of them reaches the table or the file.
    allocated <- lavaan::lavNames(score_fit(), "lv")
    expect_false(any(allocated %in% labels))
    expect_true("F_t2_cubes" %in% allocated)
    html <- as.character(output$scores_tbl)
    for (l in labels) {
      expect_match(html, l, fixed = TRUE)
    }
    expect_false(any(vapply(allocated, function(a) grepl(sprintf("\"%s\"", a), html, fixed = TRUE), logical(1))))
    file <- output$dl_scores
    header <- readLines(file, n = 1L, encoding = "UTF-8")
    expect_identical(header, paste(sprintf("\"%s\"", c("row", "original_row", labels, paste(labels, "reason"))), collapse = ","))
    downloaded <- utils::read.csv(file, check.names = FALSE, fileEncoding = "UTF-8")
    expect_identical(downloaded$row, seq_len(nrow(fs)))
    expect_identical(downloaded$original_row, state$case_index %||% seq_len(nrow(fs)))
    expect_equal(as.matrix(downloaded[, labels, drop = FALSE]), as.matrix(fs), tolerance = 1e-12)
  })
})

test_that("no bootstrap default is lowered: DMAR's 10000 for reliability and 1000 for the bifactor indices, in the tabs and the functions", {
  # (The reliability default is DMAR's B = 10000; the bifactor defaults are
  # 1000.)
  expect_identical(formals(sieve_reliability_spec)$B, 10000L)
  expect_identical(formals(sieve_bifactor_boot_plan)$B, 1000L)
  expect_identical(formals(sieve_bifactor_bootstrap)$B, 1000L)
  expect_match(as.character(mod_reliab_ui("reliab")), "id=\"reliab-B\"[^>]*value=\"10000\"")
  expect_match(as.character(mod_bifactor_ui("bf")), "id=\"bf-idx_B\"[^>]*value=\"1000\"")
})

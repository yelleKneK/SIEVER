# Numerical identities and hostile row patterns for person-level inference.
inference_fixture <- function() {
  data <- lavaan::HolzingerSwineford1939
  items <- paste0("x", 1:6)
  list(data = data, items = items, model = "Visual =~ x1 + x2 + x3\nTextual =~ x4 + x5 + x6")
}

test_that("regression determinacy and posterior error match matrix identities", {
  x <- inference_fixture()
  fit <- lavaan::cfa(x$model, x$data, std.lv = TRUE)
  r <- sieve_factor_score_record(fit, method = "regression", data = x$data, conf_level = .90)
  E <- lavaan::lavInspect(fit, "est")
  Sigma <- lavaan::lavInspect(fit, "implied")$cov
  recovered <- E$psi %*% t(E$lambda) %*% solve(Sigma, E$lambda %*% E$psi)
  determinacy <- sqrt(diag(recovered) / diag(E$psi))
  posterior_sd <- sqrt(diag(E$psi - recovered))
  expect_equal(as.numeric(r$determinacy), as.numeric(determinacy), tolerance = 1e-10)
  for (j in seq_along(posterior_sd)) {
    rows <- r$uncertainty$factor == names(r$scores)[j]
    expect_equal(r$uncertainty$se[rows], rep(unname(posterior_sd[j]), nrow(x$data)), tolerance = 1e-10)
    expect_equal(r$uncertainty$upper[rows] - r$uncertainty$estimate[rows],
      rep(qnorm(.95) * unname(posterior_sd[j]), nrow(x$data)),
      tolerance = 1e-10
    )
  }
  expect_match(r$interval_method, "posterior interval")
  expect_identical(r$conf_level, .90)
})

test_that("score errors survive an all-missing person without shifting identities", {
  x <- inference_fixture()
  x$data[5, x$items] <- NA
  fit <- suppressWarnings(lavaan::cfa(x$model, x$data, std.lv = TRUE, missing = "ml"))
  original <- seq_len(nrow(x$data)) + 100L
  r <- sieve_factor_score_record(fit, data = x$data, original_rows = original)
  expect_equal(nrow(r$scores), nrow(x$data))
  expect_true(all(is.na(r$uncertainty$estimate[r$uncertainty$row == 5])))
  expect_true(all(is.na(r$uncertainty$se[r$uncertainty$row == 5])))
  expect_true(all(r$uncertainty$reason[r$uncertainty$row == 5] == "no observed items"))
  expect_true(all(is.finite(r$uncertainty$se[r$uncertainty$row != 5])))
  expect_identical(r$uncertainty$original_row, original[r$uncertainty$row])
  expect_null(r$reason)
})

test_that("grouped listwise scores retain model input and original row identity", {
  x <- inference_fixture()
  # Interleave groups so concatenating group scores would be visibly wrong.
  order <- order(seq_len(nrow(x$data)) %% 7L)
  x$data <- x$data[order, ]
  x$data[c(2, 5, 9), "x1"] <- NA
  fit <- lavaan::cfa(x$model, x$data, std.lv = TRUE, group = "school", missing = "listwise")
  r <- sieve_factor_score_record(fit, data = x$data, original_rows = order)
  indices <- lavaan::lavInspect(fit, "case.idx")
  oracle <- lavaan::lavPredict(fit, method = "Bartlett", assemble = FALSE)
  expect_equal(nrow(r$scores), nrow(x$data))
  for (g in seq_along(indices)) {
    expect_equal(as.matrix(r$scores)[indices[[g]], ], unclass(oracle[[g]]), ignore_attr = TRUE, tolerance = 1e-10)
  }
  expect_true(all(is.na(as.matrix(r$scores[c(2, 5, 9), ]))))
  expect_true(all(grepl("excluded|listwise", r$uncertainty$reason[r$uncertainty$row %in% c(2, 5, 9)])))
  expect_identical(r$uncertainty$original_row, order[r$uncertainty$row])
  expect_equal(unname(r$observed[c(2, 5, 9)]), rep(5, 3))
})

test_that("categorical endpoint likelihoods are refused while posterior modes remain available", {
  x <- inference_fixture()
  d <- as.data.frame(lapply(x$data[x$items], function(v) {
    as.integer(cut(v, quantile(v, 0:4 / 4), include.lowest = TRUE))
  }))
  # Deterministic endpoint patterns, a listwise omission, and an interior row.
  d[1, ] <- 1L
  d[2, ] <- 4L
  d[3, ] <- c(1, 2, 3, 2, 3, 4)
  d[5, 1] <- NA
  fit <- lavaan::cfa(x$model, d, ordered = x$items, std.lv = TRUE)
  r <- sieve_factor_score_record(fit, "ML", data = d)
  expect_true(all(is.na(as.matrix(r$scores[c(1, 2, 5), ]))))
  expect_true(all(grepl("no finite ML score", r$uncertainty$reason[r$uncertainty$row %in% 1:2])))
  expect_true(all(is.finite(as.matrix(r$scores[3, ]))))
  expect_true(all(is.finite(as.matrix(sieve_factor_scores(fit, "EBM")[1:2, ]))))
  expect_gt(r$extreme_patterns, 0)
})

test_that("residual columns use the same scale as the chosen residual", {
  x <- inference_fixture()
  fit <- lavaan::cfa(x$model, x$data, std.lv = TRUE)
  for (type in c("raw", "cor.bentler", "cor.bollen")) {
    d <- sieve_residual_pairs(fit, type)$table
    expect_equal(d$observed - d$implied, d$residual, tolerance = 1e-12)
    expect_true(all(nzchar(d$scale)))
  }
})

test_that("cluster rank is enforced before fitting and on externally supplied fits", {
  x <- inference_fixture()
  m <- sieve_build_meta(x$data, x$items, list(Visual = x$items[1:3], Textual = x$items[4:6]))
  m$type <- "continuous"
  state <- sieve_new_state()
  state$raw <- x$data
  state$sampling_design <- list(unit = "clustered", cluster = "school")
  prepared <- sieve_sampling_apply(sieve_cfa_spec(m), state, x$data)
  run <- sieve_run_model(prepared$spec, prepared$data)
  expect_null(run$fit)
  expect_match(run$error, "2 independent clusters")
  expect_match(run$error, "rank at most G - 1 = 1", fixed = TRUE)
  fit <- suppressWarnings(lavaan::cfa(x$model, x$data, std.lv = TRUE, estimator = "MLR", cluster = "school"))
  status <- sieve_fit_status(fit)
  expect_identical(status$code, "failed")
  expect_identical(status$clusters, 2L)
  expect_equal(qr(lavaan::lavInspect(fit, "vcov"))$rank, 1)
  expect_match(status$message, "19 free parameter")
  x$data$cluster_id <- rep(seq_len(30), length.out = nrow(x$data))
  fit <- lavaan::cfa(x$model, x$data, std.lv = TRUE, estimator = "MLR", cluster = "cluster_id")
  status <- sieve_fit_status(fit)
  expect_identical(status$code, "ok")
  expect_identical(status$clusters, 30L)
  expect_match(status$message, "small-cluster correction")
})

test_that("a signed correlation boundary reports its estimate and direction", {
  x <- inference_fixture()
  fit <- lavaan::cfa(x$model, x$data, std.lv = TRUE, estimator = "MLR")
  low <- sieve_latent_bound(fit, c("Visual", "Textual"), .3)
  high <- sieve_latent_bound(fit, c("Visual", "Textual"), .6)
  expect_match(low$summary$direction, "above")
  expect_match(high$summary$direction, "below")
  expect_equal(low$summary$estimated_correlation, high$summary$estimated_correlation)
  values <- unlist(low$summary[c("lower", "upper", "p")])
  expect_true(all(is.finite(values)))
  expect_match(low$summary$test_method, "Chi-Squared Difference")
})

test_that("tied quantile limits retain pointwise coverage and percentile ranks use midranks", {
  n <- 15L
  k <- 0:n
  coverage <- sum(dbinom(k, n, .6) * vapply(k, function(zeroes) {
    bounds <- sieve_quantile_limits(c(rep(0, zeroes), rep(1, n - zeroes)), .5)
    bounds$lower <= 0 && bounds$upper >= 0
  }, logical(1)))
  expect_gte(coverage, .95)
  d <- data.frame(a = rep(1:5, 5:1), b = rep(1:5, 5:1))
  m <- sieve_build_meta(d, names(d), list(Scale = names(d)))
  ref <- sieve_norm_freeze(d, m, name = "Fixed calibration", population = "A test fixture")
  applied <- sieve_norm_apply(ref, data.frame(a = 1:5, b = 1:5), m)
  expect_equal(applied$percentile, 100 * c(2.5, 7, 10.5, 13, 14.5) / 15)
  expect_true(all(grepl("midrank", applied$percentile_definition)))
})

test_that("ordinal partial anchors identify free response scales and preserve nonzero impact", {
  withr::with_seed(419, {
    eta <- rnorm(1000) + rep(c(0, .5), each = 500)
    d <- as.data.frame(sapply(seq_len(5), function(j) as.integer(cut(
      .8 * eta + rnorm(1000, sd = .6) + if (j == 5) rep(c(0, .5), each = 500) else 0,
      c(-Inf, -.7, 0, .7, Inf)
    ))))
    names(d) <- paste0("y", seq_len(5))
    d$g <- rep(c("A", "B"), each = 500)
  })
  m <- sieve_build_meta(d, paste0("y", seq_len(5)), list(Scale = paste0("y", seq_len(5))))
  m$type <- "ordered"
  r <- sieve_anchor_sensitivity(d, "g", m,
    list(Four = paste0("y", 1:4), Three = paste0("y", 1:3)),
    rationale = "Known fixture anchors", discovery = FALSE
  )
  expect_true(all(r$contrasts$licensed))
  expect_true(all(vapply(r$models, function(x) identical(sieve_fit_status(x$fit)$code, "ok"), logical(1))))
  fit <- r$models$Four$fit
  p <- lavaan::parTable(fit)
  # The released item's residual metric is fixed for identification.
  residual <- p[p$lhs == "y5" & p$rhs == "y5" & p$op == "~~", ]
  expect_true(all(residual$free == 0L))
  impact <- sieve_ordinal_fit_impact(fit,
    setNames(
      rep(
        list(1:4),
        5
      ),
      paste0(
        "y",
        1:5
      )
    ),
    "A",
    "B",
    anchors = "Fixture anchor declaration"
  )
  expect_gt(abs(impact$table$signed[impact$table$item == "y5"]), .05)
  expect_match(r$contrasts$metric[1], "reference-group factor SD")
})

test_that("wave order is explicit and invariant to CSV wave-block order", {
  data <- lavaan::HolzingerSwineford1939[paste0("x", 1:9)]
  map <- data.frame(
    construct = "Scale", wave = rep(c("T1", "T2", "T3"), each = 3),
    item = rep(c("I1", "I2", "I3"), 3), variable = names(data), wave_order = rep(1:3, each = 3)
  )
  a <- sieve_longitudinal_spec(data, map, justification = "Repeated identities in a fixture")
  scrambled <- map[c(7:9, 1:3, 4:6), ]
  b <- sieve_longitudinal_spec(data, scrambled, justification = "Repeated identities in a fixture")
  expect_identical(a$model, b$model)
  expect_identical(b$longitudinal$waves, c("T1", "T2", "T3"))
  expect_identical(b$longitudinal$adjacent_pairs, c("T1 - T2", "T2 - T3"))
  expect_identical(b$longitudinal$reference_wave, "T1")
  map$wave_order <- NULL
  expect_error(sieve_longitudinal_spec(data, map, justification = "Repeated identities"), "Declare every wave")
  a <- sieve_longitudinal_spec(data, map, justification = "Repeated identities", wave_order = c("T1", "T3", "T2"))
  expect_identical(a$longitudinal$adjacent_pairs, c("T1 - T3", "T3 - T2"))
})

test_that("independent-row bootstraps refuse declared dependence", {
  x <- inference_fixture()
  m <- sieve_build_meta(x$data, x$items, list(Visual = x$items[1:3], Textual = x$items[4:6]))
  m$type <- "continuous"
  spec <- sieve_cfa_spec(m)
  spec$sampling_design <- list(unit = "clustered", cluster = "school")
  out <- sieve_bollen_stine(spec, x$data, B = 100, seed = 719)
  expect_match(out$error, "design-matched bootstrap")
  expect_null(out$rng)
  state <- sieve_new_state()
  state$raw <- x$data
  state$items <- x$data[x$items]
  state$meta <- m
  state$confirmed <- TRUE
  state$sampling_design <- list(unit = "repeated", cluster = NULL)
  shiny::testServer(mod_validity_server, args = list(state = state), {
    session$setInputs(htmt_on = TRUE, htmt_boot = 1)
    expect_match(htmt_ci()$error, "declared sampling unit is repeated")
  })
})

test_that("assurance sensitivity carries the same design target into every plan", {
  calls <- list()
  testthat::local_mocked_bindings(ss_aipe_reliability = function(...) {
    args <- list(...)
    calls[[length(calls) + 1L]] <<- args
    data.frame(term = "necessary_N", value = round(100 / mean(args$true_factor_loading)))
  }, .package = "DMAR")
  out <- sieve_plan_congeneric(c(.8, .7, .6), c(.36, .51, .64), assurance = .8, seed = 729)
  expect_equal(length(calls), 3)
  expect_true(all(vapply(calls, function(x) identical(x$assurance, .8), logical(1))))
  expect_equal(out$sensitivity$N[2], out$result$value)
  expect_equal(out$sensitivity$assurance, rep(.8, 3))
})

test_that("one-item facets have no internal-consistency coefficient or interval label", {
  spec <- sieve_reliability_spec("x1", "continuous", construct = "Facet", ci_method = "mlr")
  data <- data.frame(x1 = seq_len(40))
  omega <- sieve_reliability_omega(data, spec)
  alpha <- sieve_reliability_alpha(data, spec)
  expect_match(omega$status$message, "One item supplies no internal-consistency evidence")
  expect_match(alpha$uncertainty$label, "Unavailable")
  expect_true(is.na(alpha$estimate))
})

test_that("a frame that is not the model's rows is refused before anything is attached to a score", {
  # Reversed rows must not be accepted, and an unrelated frame must not
  # give finite scores beside "no observed items".
  x <- inference_fixture()
  fit <- lavaan::cfa(x$model, x$data, std.lv = TRUE, missing = "ml")
  ok <- sieve_factor_score_record(fit, data = x$data)
  expect_equal(as.matrix(ok$scores), unclass(lavaan::lavPredict(fit, method = "Bartlett")),
    ignore_attr = TRUE, tolerance = 1e-12
  )
  expect_error(
    sieve_factor_score_record(fit, data = x$data[rev(seq_len(nrow(x$data))), ]),
    "differ from those the model used"
  )
  expect_error(
    sieve_factor_score_record(fit, data = data.frame(unrelated = seq_len(nrow(x$data)))),
    "observed variables x1, x2"
  )
  changed <- x$data
  changed$x4[10] <- changed$x4[10] + 1
  expect_error(sieve_factor_score_record(fit, data = changed), "values of x4 differ")
  expect_error(
    sieve_factor_score_record(fit, data = x$data[, setdiff(names(x$data), "x5")]),
    "observed variable x5"
  )
  expect_error(
    sieve_factor_score_record(fit, data = rbind(x$data, x$data[1, ])),
    "has 302 rows, but the model was fitted to 301"
  )
})

test_that("the model's own rows pass: ordered codes with an unused category, two groups, standardized variables", {
  x <- inference_fixture()
  d <- x$data[x$items]
  # Ordered items scored 2 to 4, which lavaan holds as the codes 1 to 3.
  ord <- as.data.frame(lapply(d, function(v) as.integer(cut(v, quantile(v, 0:3 / 3), include.lowest = TRUE)) + 1L))
  fo <- lavaan::cfa(x$model, ord, ordered = x$items, std.lv = TRUE)
  expect_error(sieve_factor_score_record(fo, data = ord), NA)
  as_factors <- ord
  as_factors[] <- lapply(ord, function(v) factor(v, levels = 2:4, ordered = TRUE))
  expect_error(sieve_factor_score_record(fo, data = as_factors), NA)
  flipped <- ord
  flipped$x2 <- 6L - flipped$x2
  expect_error(sieve_factor_score_record(fo, data = flipped), "values of x2 differ")
  # Two groups, and two rows of one group swapped.
  fg <- lavaan::cfa(x$model, x$data, std.lv = TRUE, group = "school")
  expect_error(sieve_factor_score_record(fg, data = x$data), NA)
  expect_error(
    sieve_factor_score_record(fg, data = x$data[c(2, 1, 3:nrow(x$data)), ]),
    "differ from those the model used"
  )
  # Variables the model standardized itself.
  fs <- lavaan::cfa(x$model, x$data, std.lv = TRUE, std.ov = TRUE)
  expect_error(sieve_factor_score_record(fs, data = x$data), NA)
})

# ---- Categorical scores computed directly; rows without a group ------------

# The posterior mode (EBM) and the maximum likelihood value (ML) of one
# person's factor, computed directly from the fitted ordered-probit model:
# the product of the category probabilities of the person's responses
# (thresholds, intercepts, loadings, and residual variances as fitted),
# times the fitted normal density of the factor for EBM. It shares nothing
# with lavPredict() but the fitted parameters.
ordinal_score_reference <- function(fit, data, items, method = c("EBM", "ML")) {
  method <- match.arg(method)
  est <- lavaan::lavInspect(fit, "est")
  if (!is.null(est$lambda)) est <- list(est)
  index <- lavaan::lavInspect(fit, "case.idx")
  if (!is.list(index)) index <- list(index)
  out <- rep(NA_real_, nrow(data))
  for (g in seq_along(index)) {
    e <- est[[g]]
    for (i in index[[g]]) {
      objective <- function(eta) {
        logp <- 0
        for (it in items) {
          y <- data[[it]][i]
          cuts <- c(-Inf, as.numeric(e$tau[startsWith(rownames(e$tau), paste0(it, "|")), 1L]), Inf)
          location <- e$nu[it, 1L] + e$lambda[it, 1L] * eta
          sd <- sqrt(e$theta[it, it])
          p <- stats::pnorm((cuts[y + 1L] - location) / sd) - stats::pnorm((cuts[y] - location) / sd)
          # Far in a tail the probability underflows; a floor keeps the
          # objective finite there and does not move an interior optimum.
          logp <- logp + log(max(p, 1e-300))
        }
        prior <- if (identical(method, "EBM")) stats::dnorm(eta, e$alpha[1L, 1L], sqrt(e$psi[1L, 1L]), log = TRUE) else 0
        -(logp + prior)
      }
      out[i] <- stats::optimize(objective, e$alpha[1L, 1L] + c(-10, 10), tol = 1e-10)$minimum
    }
  }
  out
}

test_that("categorical EBM and ML scores of a grouped model with nonzero intercepts agree with the directly computed values", {
  # The reference is computed from the fitted parameters directly, since
  # agreement with lavPredict() alone cannot detect a defect the two share:
  # adding the intercepts of ordered indicators twice would move the second
  # group's scores of this model by up to 0.47 (EBM) and 0.55 (ML). The
  # test also checks that lavaan is 0.7-3 or later, the version SIEVER
  # imports.
  raw <- lavaan::HolzingerSwineford1939
  items <- paste0("x", 4:6)
  d <- as.data.frame(lapply(raw[items], function(v) as.integer(cut(v, stats::quantile(v, 0:4 / 4), include.lowest = TRUE))))
  d$school <- raw$school
  fit <- lavaan::cfa("F =~ x4 + x5 + x6",
    data = d, ordered = items, group = "school",
    group.equal = c("loadings", "thresholds"), std.lv = TRUE, estimator = "WLSMV"
  )
  expect_identical(sieve_fit_status(fit)$code, "ok")
  # The condition under which doubled intercepts would show: the second
  # group's ordered indicators have intercepts well away from zero.
  nu <- lavaan::lavInspect(fit, "est")[[2L]]$nu
  expect_gt(max(abs(nu)), 0.3)
  expect_true(utils::packageVersion("lavaan") >= "0.7-3")
  ebm <- sieve_factor_score_record(fit, method = "EBM", data = d)
  expect_identical(ebm$method$method, "EBM")
  expect_identical(ebm$engine_version[["lavaan"]], as.character(utils::packageVersion("lavaan")))
  expect_identical(nrow(ebm$scores), nrow(d))
  reference <- ordinal_score_reference(fit, d, items, "EBM")
  expect_true(all(is.finite(ebm$scores[[1L]])))
  # lavaan finds the mode iteratively; its values agree with the direct
  # mode to within 2e-4 here, and the doubled intercepts would move them by
  # up to 0.47.
  expect_lt(max(abs(ebm$scores[[1L]] - reference)), 1e-3)
  for (g in lavaan::lavInspect(fit, "case.idx")) {
    expect_lt(abs(mean(ebm$scores[[1L]][g] - reference[g])), 1e-4)
  }
  # ML: the same likelihood without the prior. A uniform endpoint pattern
  # has no finite maximum, and SIEVER gives it no score.
  ml <- sieve_factor_score_record(fit, method = "ML", data = d)
  reference_ml <- ordinal_score_reference(fit, d, items, "ML")
  endpoint <- apply(d[items], 1L, function(r) all(r == 1L) || all(r == 4L))
  expect_true(all(is.na(ml$scores[[1L]][endpoint])))
  expect_true(all(is.finite(ml$scores[[1L]][!endpoint])))
  expect_lt(max(abs(ml$scores[[1L]][!endpoint] - reference_ml[!endpoint])), 1e-3)
})

test_that("rows a grouped model excluded for a missing group keep their place, without a score", {
  # The identity check of the rows counts more than the rows the fit
  # records: it identifies the rows without a group from the group
  # variable, so a frame whose last rows have no group is not refused
  # ("data has 301 rows, but the model was fitted to 300").
  base <- lavaan::HolzingerSwineford1939
  model <- "Visual =~ x1 + x2 + x3\nTextual =~ x4 + x5 + x6\nSpeed =~ x7 + x8 + x9"
  cases <- list(first = 1L, middle = 150L, last = 301L, last_two = 300:301, several = c(1L, 2L, 150L, 299L, 301L))
  for (where in names(cases)) {
    x <- base
    excluded <- cases[[where]]
    x$school[excluded] <- NA
    fit <- suppressWarnings(lavaan::cfa(model, x, group = "school", std.lv = TRUE, estimator = "MLR", missing = "fiml"))
    ids <- 1000L + seq_len(nrow(x))
    r <- sieve_factor_score_record(fit, data = x, original_rows = ids)
    expect_identical(nrow(r$scores), nrow(x), info = where)
    expect_identical(r$original_rows, ids, info = where)
    expect_true(all(is.na(as.matrix(r$scores[excluded, , drop = FALSE]))), info = where)
    reasons <- unique(r$uncertainty$reason[r$uncertainty$row %in% excluded])
    expect_identical(reasons, "no group membership; excluded by the model's group rule", info = where)
    expect_identical(unique(r$uncertainty$original_row[r$uncertainty$row %in% excluded]), ids[excluded], info = where)
    # Every other row has the engine's score for that row.
    engine <- lavaan::lavPredict(fit, method = "Bartlett", assemble = FALSE)
    index <- lavaan::lavInspect(fit, "case.idx")
    for (g in seq_along(index)) {
      expect_equal(unname(as.matrix(r$scores[index[[g]], ])), unname(engine[[g]][, c("Visual", "Textual", "Speed")]),
        tolerance = 1e-12, info = where
      )
    }
    expect_true(all(is.finite(as.matrix(r$scores[-excluded, ]))), info = where)
  }
  # Frames that cannot be the model's rows are still refused: reversed rows, a
  # group value the model never saw, a row moved to another group, an extra row
  # with a group, a missing variable, and trailing rows without the group
  # variable that would identify them.
  x <- base
  x$school[301] <- NA
  fit <- suppressWarnings(lavaan::cfa(model, x, group = "school", std.lv = TRUE))
  expect_error(sieve_factor_score_record(fit, data = x[rev(seq_len(nrow(x))), ]), "differ from those the model used")
  other <- x
  other$school <- as.character(other$school)
  other$school[5] <- "Other"
  expect_error(sieve_factor_score_record(fit, data = other), "the value Other of the group variable school")
  moved <- x
  moved$school[1] <- "Grant-White"
  expect_error(sieve_factor_score_record(fit, data = moved), "rows of its groups (school) differ", fixed = TRUE)
  expect_error(sieve_factor_score_record(fit, data = rbind(x, base[1, ])), "rows of its groups (school) differ", fixed = TRUE)
  expect_error(sieve_factor_score_record(fit, data = x[, setdiff(names(x), "x5")]), "observed variable x5")
  expect_error(
    sieve_factor_score_record(fit, data = x[, setdiff(names(x), "school")]),
    "with the group variable school, so that rows the model excluded for a missing group can be identified"
  )
  # A fit restricted to one school with group.label excluded the other
  # school's rows the same way: they keep their place without a score.
  gw <- lavaan::cfa(model, base, group = "school", group.label = "Grant-White", std.lv = TRUE)
  r <- sieve_factor_score_record(gw, data = base)
  pasteur <- which(base$school == "Pasteur")
  expect_identical(nrow(r$scores), nrow(base))
  expect_true(all(is.na(as.matrix(r$scores[pasteur, ]))))
  expect_identical(
    unique(r$uncertainty$reason[r$uncertainty$row %in% pasteur]),
    "a group the model was not fitted to (group.label); excluded by the model's group rule"
  )
  expect_equal(unname(as.matrix(r$scores[-pasteur, ])), unname(unclass(lavaan::lavPredict(gw, method = "Bartlett"))[, c("Visual", "Textual", "Speed")]),
    tolerance = 1e-12
  )
  # A model without groups: its own rows pass, an extra row does not.
  single <- lavaan::cfa(model, base, std.lv = TRUE)
  expect_identical(nrow(sieve_factor_score_record(single, data = base)$scores), nrow(base))
  expect_error(sieve_factor_score_record(single, data = rbind(base, base[1, ])), "has 302 rows, but the model was fitted to 301")
})

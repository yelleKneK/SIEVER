# The bootstrap of the bifactor indices: the rows it resamples,
# the model it refits, the rule that decides which draws are used, the
# counts it reports, and its random numbers.
#
# The checks here are oracles, not echoes. A draw is rebuilt by hand: its
# rows from two plain set.seed() calls, its fit from a lavaan call written
# out in this file with the point estimate's estimator and missing data
# treatment, and its indices from formulas written out in this file (no
# DMAR). Percentile limits are recomputed with stats::quantile(), and the
# bounds on a limit by brute force on an augmented sample. The recorded R is
# run in a clean environment and has to return the same counts and limits.
#
# The things that must not happen: resampling complete rows when the point
# estimate used incomplete ones, counting a returned row as converged,
# different draws behind different indices, and a zero width interval for
# PUC.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

# The Bifactor tab's bootstrap of independent rows is DMAR's
# ci_bifactor_indices() (tested with DMAR's other bootstraps), and SIEVER's
# own engine serves a declared cluster design. The function-level checks
# below are of that engine, its rows, its rule and its counts, so they ask
# for it by name (engine = "siever"); the page checks at the end run DMAR's.

# ---- fixtures --------------------------------------------------------------

siever_engine_bootstrap <- function(...) sieve_bifactor_bootstrap(..., engine = "siever")

# A bifactor population: y = lg G + ls S_k + sqrt(1 - lg^2 - ls^2) e.
bifactor_data <- function(n, lg, ls, group, seed) {
  p <- length(lg)
  Y <- withr::with_seed(seed, {
    G <- stats::rnorm(n)
    S <- matrix(stats::rnorm(n * length(unique(group))), n)
    E <- matrix(stats::rnorm(n * p), n)
    sapply(seq_len(p), function(j) {
      lg[j] * G + ls[j] * S[, group[j]] + sqrt(1 - lg[j]^2 - ls[j]^2) * E[, j]
    })
  })
  d <- as.data.frame(Y)
  names(d) <- sprintf("%s%d", c("a", "b", "c")[group], stats::ave(seq_len(p), group, FUN = seq_along))
  d
}

# A share of rows lose one to three items, completely at random.
punch <- function(d, share, seed) {
  withr::with_seed(seed, {
    for (i in sample.int(nrow(d), round(share * nrow(d)))) {
      d[i, sample.int(ncol(d), sample.int(3, 1))] <- NA
    }
  })
  d
}

ledger <- function(d, group, type = "continuous") {
  data.frame(
    item = names(d), label = names(d),
    construct = c("Alpha", "Beta", "Gamma")[group], facet = "",
    type = type, status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
}

empty_rows <- function(d, n) {
  rbind(d, as.data.frame(matrix(NA_real_, n, ncol(d), dimnames = list(NULL, names(d)))))
}

group9 <- rep(1:3, each = 3)
model9 <- paste(
  "general =~ a1 + a2 + a3 + b1 + b2 + b3 + c1 + c2 + c3",
  "Alpha =~ a1 + a2 + a3", "Beta =~ b1 + b2 + b3", "Gamma =~ c1 + c2 + c3",
  sep = "\n"
)

# A well defined structure, 400 respondents, 35 percent of rows incomplete.
stable_missing <- function() {
  d <- bifactor_data(400,
    lg = rep(c(.55, .60, .65), 3), ls = rep(c(.50, .55, .45), 3),
    group = group9, seed = 2101
  )
  d <- punch(d, 0.35, seed = 2102)
  list(data = d, meta = ledger(d, group9))
}

# An unstable model whose own solution is proper: 150 respondents and a weak
# third group factor, so many resamples are improper or do not converge.
unstable <- function() {
  d <- bifactor_data(150,
    lg = rep(c(.60, .65, .70), 3),
    ls = c(.50, .55, .45, .50, .45, .55, .30, .35, .30),
    group = group9, seed = 3002
  )
  list(data = d, meta = ledger(d, group9))
}

# A model whose own solution is improper: 90 respondents, weaker still.
improper_point <- function() {
  d <- bifactor_data(90,
    lg = rep(c(.60, .65, .70), 3),
    ls = c(.50, .55, .45, .50, .45, .55, .20, .15, .20),
    group = group9, seed = 2203
  )
  list(data = d, meta = ledger(d, group9))
}

# The bootstraps several tests read are run once.
cache <- new.env(parent = emptyenv())
boot_stable <- function() {
  if (is.null(cache$stable)) {
    s <- stable_missing()
    cache$stable <- siever_engine_bootstrap(s$data, s$meta,
      B = 40, seed = 7,
      conf_level = 0.95
    )
  }
  cache$stable
}
boot_unstable <- function() {
  if (is.null(cache$unstable)) {
    s <- unstable()
    cache$unstable <- siever_engine_bootstrap(s$data, s$meta,
      B = 40, seed = 5,
      conf_level = 0.80
    )
  }
  cache$unstable
}

# The rows of draw b, from plain R: the run's seed gives one seed per draw,
# and the draw's seed gives its rows.
hand_rows <- function(seed, B, b, n) {
  withr::with_preserve_seed({
    set.seed(seed)
    draw_seeds <- sample.int(.Machine$integer.max, B)
    set.seed(draw_seeds[b])
    sample.int(n, n, replace = TRUE)
  })
}

# The bifactor indices from a fitted lavaan model, by the published formulas
# (Rodriguez, Reise, & Haviland, 2016; Hancock & Mueller, 2001), in the order
# general (ECV, omega, omegaH, PUC, H), then each group factor (ECV, omega,
# omegaHS, H). No DMAR.
hand_indices <- function(fit, group) {
  L <- lavaan::lavInspect(fit, "std")$lambda
  lg <- L[, 1]
  ls <- rowSums(L[, -1, drop = FALSE])
  theta <- 1 - lg^2 - ls^2
  h_of <- function(l) {
    t <- sum(l^2 / (1 - l^2))
    t / (1 + t)
  }
  by_group <- split(seq_along(lg), group)
  common <- sum(lg)^2 + sum(vapply(by_group, function(i) sum(ls[i])^2, numeric(1)))
  total <- common + sum(theta)
  p <- length(lg)
  general <- c(
    sum(lg^2) / (sum(lg^2) + sum(ls^2)), common / total, sum(lg)^2 / total,
    1 - sum(vapply(by_group, function(i) choose(length(i), 2), numeric(1))) / choose(p, 2),
    h_of(lg)
  )
  groups <- unlist(lapply(by_group, function(i) {
    sub <- sum(lg[i])^2 + sum(ls[i])^2 + sum(theta[i])
    c(
      sum(lg[i]^2) / (sum(lg[i]^2) + sum(ls[i]^2)),
      (sum(lg[i])^2 + sum(ls[i])^2) / sub, sum(ls[i])^2 / sub, h_of(ls[i])
    )
  }))
  unname(c(general, groups))
}

# Whether a hand fit is a converged, proper solution: the rule, read from
# lavaan without the package's status function.
hand_usable <- function(fit) {
  if (!isTRUE(lavaan::lavInspect(fit, "converged"))) {
    return(FALSE)
  }
  est <- lavaan::lavInspect(fit, "est")
  std <- lavaan::lavInspect(fit, "std")$lambda
  se_ok <- tryCatch(all(is.finite(sqrt(diag(lavaan::vcov(fit))))), error = function(e) FALSE)
  all(diag(est$theta) >= 0) && all(diag(est$psi) >= 0) && all(abs(std) <= 1) && se_ok
}

boot_state <- function(sc) {
  fixture <- list(data = sc$data, item_cols = names(sc$data),
    name = "bifactor scenario", theory_map = NULL, labels = NULL, load_code = character())
  siever_test_study(fixture, labels = FALSE, theory = FALSE, configure = function(s) {
    s$meta <- sc$meta
    s$roles <- NULL
  })
}

# Let a batched bootstrap in a module under testServer run to its end.
run_boot <- function(session, running, passes = 5000L) {
  for (i in seq_len(passes)) {
    if (!isTRUE(running())) {
      break
    }
    session$elapse(25)
  }
  invisible(NULL)
}

# ---- the arithmetic of the limits ------------------------------------------

test_that("a percentile limit and its bounds are the quantiles they are said to be", {
  x <- withr::with_seed(11, stats::rbeta(57, 6, 3))
  for (p in c(0.025, 0.1, 0.5, 0.9, 0.975)) {
    expect_equal(sieve_quantile_at(sort(x), (length(x) - 1) * p + 1),
      unname(stats::quantile(x, p, type = 7)),
      tolerance = 1e-14
    )
  }
  expect_identical(sieve_quantile_at(sort(x), 0.5), -Inf)
  expect_identical(sieve_quantile_at(sort(x), length(x) + 0.5), Inf)

  # No unusable draw: the limits are the quantiles and the bounds collapse.
  all_used <- sieve_percentile_limits(x, attempted = length(x), conf_level = 0.90)
  expect_equal(all_used$lower, unname(stats::quantile(x, 0.05)), tolerance = 1e-14)
  expect_equal(all_used$upper, unname(stats::quantile(x, 0.95)), tolerance = 1e-14)
  expect_equal(all_used$se, stats::sd(x), tolerance = 1e-14)
  expect_identical(all_used$used, 57L)
  expect_equal(c(all_used$lower_min, all_used$lower_max), rep(all_used$lower, 2), tolerance = 1e-14)
  expect_equal(c(all_used$upper_min, all_used$upper_max), rep(all_used$upper, 2), tolerance = 1e-14)

  # Unusable draws: the bounds are the limits of the full set of draws when
  # every missing value is placed below, or above, the used ones. Brute
  # force: put the values there and take the quantile.
  set_aside <- 3L
  some <- sieve_percentile_limits(x, attempted = length(x) + set_aside, conf_level = 0.80)
  below <- c(rep(-1e6, set_aside), x)
  above <- c(x, rep(1e6, set_aside))
  expect_equal(some$lower_min, unname(stats::quantile(below, 0.10)), tolerance = 1e-12)
  expect_equal(some$lower_max, unname(stats::quantile(above, 0.10)), tolerance = 1e-12)
  expect_equal(some$upper_min, unname(stats::quantile(below, 0.90)), tolerance = 1e-12)
  expect_equal(some$upper_max, unname(stats::quantile(above, 0.90)), tolerance = 1e-12)
  expect_true(some$lower_min <= some$lower && some$lower <= some$lower_max)
  expect_true(some$upper_min <= some$upper && some$upper <= some$upper_max)

  # More unusable draws than a tail holds: the limit could lie anywhere
  # beyond the used draws, so the bound is the end of the index's range.
  many <- sieve_percentile_limits(x, attempted = length(x) + 20L, conf_level = 0.80)
  expect_identical(many$lower_min, 0)
  expect_identical(many$upper_max, 1)
  expect_true(is.finite(many$lower_max) && is.finite(many$upper_min))
  # Non-finite draws are not used, and fewer than two give no limit.
  expect_identical(sieve_percentile_limits(c(x, NA, Inf), 59L)$used, 57L)
  expect_true(is.na(sieve_percentile_limits(0.5, 10L)$lower))
})

test_that("the fewest usable draws and the caution share follow the confidence level", {
  expect_identical(sieve_bootstrap_min_draws(0.95), 39L)
  expect_identical(sieve_bootstrap_min_draws(0.90), 19L)
  expect_identical(sieve_bootstrap_min_draws(0.99), 199L)
  expect_identical(sieve_bootstrap_min_draws(0.80), 9L)
  # The defining property: with that many draws the limit's rank,
  # (B + 1) * alpha / 2, reaches the first order statistic, and with one
  # fewer it does not.
  for (conf in c(0.80, 0.90, 0.95, 0.99)) {
    need <- sieve_bootstrap_min_draws(conf)
    expect_gte((need + 1) * (1 - conf) / 2, 1 - 1e-9)
    expect_lt(need * (1 - conf) / 2, 1)
  }
  expect_equal(sieve_bootstrap_caution_share(0.95), 0.025)
  expect_equal(sieve_bootstrap_caution_share(0.80), 0.10)
})

# ---- the specification is read from the fitted model -----------------------

test_that("the estimator, missing data treatment, and scaling are read from the fit", {
  d <- withr::with_seed(31, {
    f <- stats::rnorm(250)
    as.data.frame(sapply(c(.6, .7, .65, .75), function(l) l * f + sqrt(1 - l^2) * stats::rnorm(250)))
  })
  names(d) <- paste0("y", 1:4)
  model <- "f =~ y1 + y2 + y3 + y4"
  refit <- function(args, data) do.call(lavaan::cfa, c(list(model = model, data = data), args))

  mlr <- lavaan::cfa(model, d, estimator = "MLR", missing = "fiml", std.lv = TRUE)
  sig <- sieve_lavaan_signature(mlr)
  expect_identical(sig$missing, "ml")
  expect_identical(sig$loadings, paste("f =~", names(d)))
  args <- sieve_lavaan_signature_args(sig)
  expect_identical(args$estimator, "MLR")
  expect_identical(args$missing, "fiml")
  expect_true(args$std.lv)
  expect_null(args$ordered)
  expect_identical(sieve_lavaan_signature(refit(args, d)), sig)

  ml <- lavaan::cfa(model, d, estimator = "ML", std.lv = TRUE)
  args_ml <- sieve_lavaan_signature_args(sieve_lavaan_signature(ml))
  expect_identical(args_ml$estimator, "ML")
  expect_identical(args_ml$missing, "listwise")
  # An ML fit is plain ML whether or not lavaan lists Browne's
  # normal-theory residual statistic beside the standard one (it does by
  # default from 0.7-3): no se or test is written out, and the refit has
  # the same signature.
  expect_null(args_ml$se)
  expect_null(args_ml$test)
  expect_identical(sieve_lavaan_signature(refit(args_ml, d)), sieve_lavaan_signature(ml))
  for (tests in list("standard", c("standard", "browne.residual.nt.model"))) {
    sig_b <- sieve_lavaan_signature(ml)
    sig_b$test <- tests
    expect_identical(sieve_lavaan_signature_args(sig_b)$estimator, "ML")
    expect_null(sieve_lavaan_signature_args(sig_b)$test)
  }
  expect_identical(sieve_lavaan_corrected_tests(c("standard", "browne.residual.nt.model")), character(0))
  expect_identical(sieve_lavaan_corrected_tests(c("standard", "yuan.bentler.mplus")), "yuan.bentler.mplus")
  # lavaan 0.7-3 lists Browne's ADF residual statistic ahead of the
  # Yuan-Bentler one for a weighted MLR fit; the corrected one is still the
  # Yuan-Bentler statistic, and the fit is still MLR.
  expect_identical(sieve_lavaan_corrected_tests(c("standard", "browne.residual.adf", "yuan.bentler.mplus")), "yuan.bentler.mplus")
  sig_wt <- sieve_lavaan_signature(mlr)
  sig_wt$test <- c("standard", "browne.residual.adf", "yuan.bentler.mplus")
  expect_identical(sieve_lavaan_signature_args(sig_wt)$estimator, "MLR")
  # A combination written out in full keeps every statistic beyond the
  # standard one, Browne's included, so the refit lists the same tests.
  uls <- lavaan::cfa(model, d, estimator = "ULS", std.lv = TRUE)
  sig_u <- sieve_lavaan_signature(uls)
  expect_identical(sieve_lavaan_signature(refit(sieve_lavaan_signature_args(sig_u), d)), sig_u)

  o <- as.data.frame(lapply(d, function(v) as.numeric(cut(v, c(-Inf, -.6, 0, .6, Inf)))))
  wls <- lavaan::cfa(model, o, estimator = "WLSMV", ordered = names(o), std.lv = TRUE)
  sig_w <- sieve_lavaan_signature(wls)
  args_w <- sieve_lavaan_signature_args(sig_w)
  expect_identical(args_w$estimator, "WLSMV")
  expect_identical(args_w$ordered, names(o))
  expect_identical(args_w$missing, "listwise")
  expect_identical(sieve_lavaan_signature(refit(args_w, o)), sig_w)

  # A combination with no short name is written out in full and still refits
  # to the same thing.
  mlm <- lavaan::cfa(model, d, estimator = "MLM", std.lv = TRUE)
  sig_m <- sieve_lavaan_signature(mlm)
  args_m <- sieve_lavaan_signature_args(sig_m)
  expect_identical(args_m$se, "robust.sem")
  expect_identical(sieve_lavaan_signature(refit(args_m, d)), sig_m)
  # A different model on the same data does not pass for this one.
  expect_false(identical(
    sieve_lavaan_signature(lavaan::cfa("f =~ y1 + y2 + y3", d, std.lv = TRUE)),
    sieve_lavaan_signature(ml)
  ))
})

# ---- the rows resampled are the rows the point estimate used ---------------

test_that("the bootstrap resamples the rows the FIML point estimate used, patterns and all", {
  s <- stable_missing()
  r <- boot_stable()
  any_observed <- rowSums(!is.na(s$data)) > 0
  complete <- stats::complete.cases(s$data)
  expect_identical(sum(any_observed), 400L)
  expect_identical(sum(complete), 260L)

  plan <- sieve_bifactor_boot_plan(s$data, s$meta, B = 40, seed = 7, engine = "siever")
  expect_identical(plan$status$code, "ready")
  expect_identical(plan$rows, "observed")
  # The pool is every row with an observed item, in order, missing values
  # where they were: not the complete rows alone.
  expect_equal(plan$pool, s$data[any_observed, ], ignore_attr = TRUE)
  expect_identical(sum(!stats::complete.cases(plan$pool)), sum(!complete))
  expect_identical(sum(!stats::complete.cases(plan$pool)), 140L)
  # It is exactly the rows lavaan says the point estimate used.
  point <- sieve_fit_bifactor(s$data, s$meta)
  expect_identical(nrow(plan$pool), as.integer(lavaan::lavInspect(point$fit, "nobs")))
  expect_identical(which(any_observed), as.integer(lavaan::lavInspect(point$fit, "case.idx")))
  expect_identical(plan$n$used, 400L)
  expect_identical(plan$n$complete, sum(complete))
  expect_identical(r$n$used, 400L)
  expect_identical(r$rows, "observed")
  expect_identical(lavaan::lavInspect(point$fit, "options")$missing, "ml")

  # The point estimate in the result is the fitted model's own.
  general <- point$indices[1, ]
  tb <- r$table[r$table$factor == "general", ]
  expect_identical(tb$estimate, unlist(general[c("ECV", "omega", "omega_H", "PUC", "H")], use.names = FALSE))
})

test_that("rows with no observed item add nothing to the bootstrap", {
  s <- stable_missing()
  padded <- empty_rows(s$data, 700)
  plan <- sieve_bifactor_boot_plan(s$data, s$meta, B = 40, seed = 7, engine = "siever")
  plan_padded <- sieve_bifactor_boot_plan(padded, s$meta, B = 40, seed = 7, engine = "siever")
  expect_identical(plan_padded$n$rows, 1100L)
  expect_identical(plan_padded$n$empty, 700L)
  expect_identical(plan_padded$n$used, 400L)
  expect_equal(plan_padded$pool, plan$pool, ignore_attr = TRUE)
  expect_identical(plan_padded$draw_seeds, plan$draw_seeds)
  expect_identical(plan_padded$key, plan$key)
  for (b in c(1L, 2L, 40L)) {
    expect_identical(sieve_bifactor_boot_rows(plan_padded, b), sieve_bifactor_boot_rows(plan, b))
  }
  expect_identical(sieve_bifactor_boot_draw(plan_padded, 3L), sieve_bifactor_boot_draw(plan, 3L))
  # The draw is the one the full run holds.
  r <- boot_stable()
  expect_identical(sieve_bifactor_boot_draw(plan, 3L)$values, unname(r$estimates[3, ]))
  # Row names are labels, not data: named rows (as a subset or a merge
  # leaves them) give the same plan.
  named <- s$data
  rownames(named) <- paste0("respondent_", seq_len(nrow(named)))
  plan_named <- sieve_bifactor_boot_plan(named, s$meta, B = 40, seed = 7, engine = "siever")
  expect_identical(plan_named$status$code, "ready")
  expect_identical(plan_named$key, plan$key)
  expect_identical(sieve_bifactor_boot_rows(plan_named, 2L), sieve_bifactor_boot_rows(plan, 2L))
})

test_that("with ordered items the pool is the complete rows the WLSMV fit used", {
  d <- bifactor_data(500,
    lg = rep(c(.55, .60, .65), 3), ls = rep(c(.50, .55, .45), 3),
    group = group9, seed = 4101
  )
  o <- as.data.frame(lapply(d, function(v) as.numeric(cut(v, c(-Inf, -.7, 0, .7, Inf)))))
  o <- punch(o, 0.15, seed = 4102)
  meta <- ledger(o, group9, type = "ordered")
  plan <- sieve_bifactor_boot_plan(o, meta, B = 12, seed = 3, engine = "siever")
  skip_if(!identical(plan$status$code, "ready"), plan$status$message)
  complete <- stats::complete.cases(o)
  expect_identical(plan$rows, "complete")
  expect_equal(plan$pool, o[complete, ], ignore_attr = TRUE)
  expect_identical(plan$n$used, sum(complete))
  expect_identical(plan$point$signature$estimator, "DWLS")
  expect_identical(plan$point$signature$ordered, names(o))
  expect_identical(sieve_lavaan_signature_args(plan$point$signature)$estimator, "WLSMV")

  # Draw 1 by hand: plain seeds, the complete rows, WLSMV on the ordered
  # items, and the indices from the formulas in this file.
  rows <- hand_rows(3, 12, 1, sum(complete))
  expect_identical(sieve_bifactor_boot_rows(plan, 1L), rows)
  hand <- suppressWarnings(lavaan::cfa(
    model9,
    data = o[complete, ][rows, ], estimator = "WLSMV", ordered = names(o),
    std.lv = TRUE, orthogonal = TRUE
  ))
  draw <- sieve_bifactor_boot_draw(plan, 1L)
  expect_identical(draw$used, hand_usable(hand))
  if (draw$used) {
    expect_equal(draw$values, hand_indices(hand, group9), tolerance = 1e-8)
  }
  # The recorded R names the same treatment.
  code <- sieve_bifactor_boot_code(plan)
  expect_true(any(grepl("estimator = \"WLSMV\"", code, fixed = TRUE)))
  expect_true(any(grepl("stats::complete.cases(bifactor_boot_pool)", code, fixed = TRUE)))
  expect_true(any(grepl("missing = \"listwise\"", code, fixed = TRUE)))
})

# ---- a draw is the point estimate's analysis on a resample -----------------

test_that("a draw equals the same resample fitted by hand with the point estimate's specification", {
  s <- stable_missing()
  r <- boot_stable()
  pool <- s$data[rowSums(!is.na(s$data)) > 0, ]
  fit_by_hand <- function(data) {
    lavaan::cfa(model9,
      data = data, estimator = "ML", missing = "fiml", # Auto: ML with FIML
      std.lv = TRUE, orthogonal = TRUE
    )
  }
  # The point estimate is that specification on all rows.
  point <- fit_by_hand(s$data)
  expect_equal(r$table$estimate, hand_indices(point, group9), tolerance = 1e-8)
  expect_identical(
    sieve_lavaan_signature(point),
    sieve_lavaan_signature(sieve_fit_bifactor(s$data, s$meta)$fit)
  )
  for (b in c(1L, 7L, 40L)) {
    rows <- hand_rows(7, 40, b, nrow(pool))
    resample <- pool[rows, ]
    # The resample carries incomplete rows: the missing data pattern is
    # resampled with the respondent.
    expect_gt(sum(!stats::complete.cases(resample)), 90L)
    hand <- fit_by_hand(resample)
    expect_true(isTRUE(lavaan::lavInspect(hand, "converged")))
    expect_true(all(is.finite(lavaan::vcov(hand))))
    expect_true(all(hand_indices(hand, group9) >= 0 & hand_indices(hand, group9) <= 1))
    expect_true(r$draws$used[b])
    expect_equal(unname(r$estimates[b, ]), hand_indices(hand, group9), tolerance = 1e-8)
    expect_identical(as.integer(lavaan::lavInspect(hand, "nobs")), 400L)
  }
  expect_identical(colnames(r$estimates), paste(r$table$factor, r$table$index, sep = ": "))
})

# ---- one rule, honest counts ------------------------------------------------

test_that("every outcome is counted as what it was and the counts add up", {
  d <- bifactor_data(300,
    lg = rep(c(.55, .60, .65), 3), ls = rep(c(.50, .55, .45), 3),
    group = group9, seed = 5102
  )
  meta <- ledger(d, group9)
  calls <- new.env()
  calls$n <- 0L
  # Call 1 is the point estimate. Then: draw 1 raises an error, draw 2
  # returns no fit, draw 3 returns a fit that did not converge, draw 4
  # returns a fit flagged improper, draw 5 returns a usable fit whose H is
  # missing, and the rest are fitted normally.
  staged <- function(data, meta) {
    calls$n <- calls$n + 1L
    draw <- calls$n - 1L
    if (draw == 1L) {
      stop("the engine fell over")
    }
    if (draw == 2L) {
      return(list(
        syntax = "", fit = NULL, indices = NULL, improper = NA,
        error = "lavaan returned no fit."
      ))
    }
    out <- sieve_fit_bifactor(data, meta)
    if (draw == 3L) {
      out$fit <- suppressWarnings(lavaan::cfa(
        out$syntax,
        data = data, estimator = "ML", missing = "fiml", std.lv = TRUE, # Auto: ML with FIML
       
        orthogonal = TRUE, control = list(iter.max = 2)
      ))
    }
    if (draw == 4L) {
      out$improper <- TRUE
    }
    if (draw == 5L) {
      out$indices$H[1] <- NA_real_
    }
    out
  }
  r <- siever_engine_bootstrap(d, meta,
    B = 12, seed = 21, conf_level = 0.50,
    fit_fun = staged
  )
  expect_identical(calls$n, 13L)
  expect_identical(
    r$draws$outcome[1:5],
    c("failed", "failed", "not_converged", "inadmissible", "no_indices")
  )
  expect_true(all(r$draws$outcome[6:12] == "used"))
  expect_match(r$draws$message[1], "the engine fell over", fixed = TRUE)
  expect_match(r$draws$message[2], "lavaan returned no fit", fixed = TRUE)
  expect_match(r$draws$message[3], "did not converge", fixed = TRUE)
  expect_match(r$draws$message[4], "improper", fixed = TRUE)
  expect_match(r$draws$message[5], "not every bifactor index", fixed = TRUE)
  expect_identical(r$draws$returned[1:5], c(FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_identical(r$draws$converged[1:5], c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_identical(r$draws$admissible[1:5], c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_identical(r$draws$used[1:5], c(FALSE, FALSE, FALSE, TRUE, FALSE))

  cnt <- r$counts
  expect_identical(cnt$requested, 12L)
  expect_identical(cnt$attempted, 12L)
  expect_identical(cnt$returned, 10L)
  expect_identical(cnt$converged, 9L)
  expect_identical(cnt$admissible, 8L)
  expect_identical(cnt$used, 8L)
  by <- cnt$outcome
  expect_identical(sum(by), cnt$attempted)
  expect_identical(cnt$returned, cnt$attempted - by[["failed"]])
  expect_identical(cnt$converged, cnt$returned - by[["not_converged"]])
  expect_identical(
    cnt$admissible,
    cnt$converged - by[["not_identified"]] - by[["inadmissible"]]
  )
  expect_identical(cnt$used, cnt$admissible + cnt$improper_kept - by[["no_indices"]])
  expect_identical(sum(r$reasons$draws), cnt$attempted - cnt$used)

  # One rule for every index: the draw whose H is missing is used for no
  # index (not kept for its ECV while dropped for its H), so
  # every interval rests on the same eight draws.
  expect_true(all(is.finite(r$estimates[5, colnames(r$estimates) != "general: H"])))
  expect_true(all(r$table$used == 8L))
  used <- r$draws$used
  for (j in which(r$table$status == "completed")) {
    expect_equal(r$table$lower[j], unname(stats::quantile(r$estimates[used, j], 0.25)),
      tolerance = 1e-12
    )
    expect_equal(r$table$upper[j], unname(stats::quantile(r$estimates[used, j], 0.75)),
      tolerance = 1e-12
    )
    with_fifth <- stats::quantile(r$estimates[used | seq_len(12) == 5L, j], 0.25, na.rm = TRUE)
    if (colnames(r$estimates)[j] != "general: H") {
      expect_false(isTRUE(all.equal(r$table$lower[j], unname(with_fifth), tolerance = 1e-12)))
    }
  }
  # A draw fitted to a different model is not an estimate of the same thing.
  other <- lavaan::cfa("general =~ a1 + a2 + a3 + b1 + b2 + b3\nAlpha =~ a1 + a2 + a3\nBeta =~ b1 + b2 + b3",
    data = d, estimator = "MLR", missing = "fiml", std.lv = TRUE,
    orthogonal = TRUE
  )
  plan <- sieve_bifactor_boot_plan(d, meta, B = 3, seed = 1, engine = "siever")
  mismatch <- sieve_bifactor_boot_read(list(fit = other, indices = plan$point$indices, improper = FALSE),
    keys = plan$quantities[, c("row", "index")],
    signature = plan$point$signature
  )
  expect_identical(mismatch$outcome, "failed")
  expect_false(mismatch$used)
  expect_match(mismatch$message, "different specification")
})

test_that("an unstable model reports attempted above used, and says the limits are conditional", {
  s <- unstable()
  r <- boot_unstable()
  cnt <- r$counts
  expect_identical(r$point$outcome, "used")
  expect_identical(cnt$attempted, 40L)
  expect_lt(cnt$used, cnt$attempted)
  expect_true(cnt$attempted >= cnt$returned && cnt$returned >= cnt$converged &&
    cnt$converged >= cnt$admissible && cnt$converged >= cnt$used)
  expect_identical(sum(cnt$outcome), cnt$attempted)
  expect_gt(cnt$outcome[["inadmissible"]], 0L)
  # The count of converged draws is a count of converged fits, not of
  # returned rows.
  expect_identical(cnt$converged, sum(r$draws$converged))
  expect_identical(cnt$used, sum(r$draws$used))
  expect_identical(cnt$improper_kept, sum(r$draws$used & !r$draws$admissible))
  expect_true(all(r$draws$outcome[r$draws$used] %in% c("used", "inadmissible")))
  expect_true(all(is.finite(r$estimates[r$draws$used, ])))

  # The caution: the unusable share is at least the tail area (10 percent
  # for an 80 percent interval).
  expect_equal(r$unusable_share, (cnt$attempted - cnt$used) / cnt$attempted)
  expect_equal(r$caution_share, 0.10)
  expect_gte(r$unusable_share, r$caution_share)
  expect_true(r$caution)
  said <- sieve_bifactor_boot_notes(r)
  expect_match(said$counts, sprintf("%d attempted", cnt$attempted), fixed = TRUE)
  expect_match(said$counts, sprintf("%d converged and admissible", cnt$admissible), fixed = TRUE)
  expect_match(said$counts, sprintf("%d used", cnt$used), fixed = TRUE)
  expect_match(said$caution, "were set aside", fixed = TRUE)
  expect_match(said$caution, "limits depend", fixed = TRUE)
  expect_match(said$caution, "retained", fixed = TRUE)
  expect_false(any(grepl("improper solution", said$reasons)))
  expect_match(said$rule, "The same replications are used for every index", fixed = TRUE)

  # The interval is formed from the used draws only, the same for every
  # index, and the bounds bracket each limit.
  expect_identical(r$uncertainty$status, "completed")
  expect_gte(cnt$used, sieve_bootstrap_min_draws(0.80))
  done <- r$table[r$table$status == "completed", ]
  expect_true(all(done$used == cnt$used))
  key <- paste(done$factor, done$index, sep = ": ")
  for (j in seq_len(nrow(done))) {
    q <- stats::quantile(r$estimates[r$draws$used, key[j]], c(0.10, 0.90), names = FALSE)
    expect_equal(c(done$lower[j], done$upper[j]), q, tolerance = 1e-12)
  }
  expect_true(all(done$lower_min <= done$lower & done$lower <= done$lower_max))
  expect_true(all(done$upper_min <= done$upper & done$upper <= done$upper_max))
  expect_true(all(done$lower_min == 0 & done$upper_max == 1))

  # The draws set aside really were unusable, and the draws used really were
  # usable: refit some of each by hand.
  fit_by_hand <- function(b) {
    rows <- hand_rows(5, 40, b, nrow(s$data))
    suppressWarnings(lavaan::cfa(model9,
      data = s$data[rows, ], estimator = "MLR",
      missing = "fiml", std.lv = TRUE, orthogonal = TRUE
    ))
  }
  for (b in utils::head(which(!r$draws$used), 3L)) {
    hand <- fit_by_hand(b)
    values <- tryCatch(hand_indices(hand, group9), error = function(e) NA_real_)
    expect_false(isTRUE(lavaan::lavInspect(hand, "converged")) &&
      all(is.finite(lavaan::vcov(hand))) && all(is.finite(values)) && all(values >= 0 & values <= 1))
  }
  for (b in utils::head(which(r$draws$used), 2L)) {
    hand <- fit_by_hand(b)
    expect_true(isTRUE(lavaan::lavInspect(hand, "converged")))
    expect_true(all(is.finite(lavaan::vcov(hand))))
    expect_true(all(hand_indices(hand, group9) >= 0 & hand_indices(hand, group9) <= 1))
    expect_equal(unname(r$estimates[b, ]), hand_indices(hand, group9), tolerance = 1e-8)
  }
})

test_that("a point estimate that fails the rule gets no interval, with the reason", {
  s <- improper_point()
  r <- siever_engine_bootstrap(s$data, s$meta, B = 40, seed = 5)
  expect_identical(r$uncertainty$status, "unavailable")
  # The reason is the status of the fitted model, in the words the other
  # tabs use for it. This solution has negative variance estimates (it is
  # improper); a status check that looks at the information matrix first
  # may name it as not identified instead. Either is why there is no
  # interval, and the fit is checked directly below.
  expect_true(r$point$outcome %in% c("inadmissible", "not_identified"))
  expect_match(r$uncertainty$reason, "improper|not identified")
  expect_match(r$uncertainty$reason, "applies to the estimate too", fixed = TRUE)
  direct <- suppressWarnings(lavaan::cfa(model9,
    data = s$data, estimator = "MLR", missing = "fiml",
    std.lv = TRUE, orthogonal = TRUE
  ))
  expect_false(hand_usable(direct))
  expect_true(any(diag(lavaan::lavInspect(direct, "est")$theta) < 0))
  expect_identical(r$counts$attempted, 0L)
  expect_null(r$table)
  expect_identical(nrow(r$draws), 0L)
  expect_identical(r$status$code, "failed")

  # A model that cannot be fitted at all says why.
  broken <- function(data, meta) stop("no such model")
  r2 <- siever_engine_bootstrap(s$data, s$meta, B = 5, fit_fun = broken)
  expect_identical(r2$uncertainty$status, "unavailable")
  expect_match(r2$uncertainty$reason, "could not be fitted: no such model", fixed = TRUE)
  # A ledger with no group factor is reported, not bootstrapped.
  thin <- s$meta
  thin$status[-(1:2)] <- "parked"
  r3 <- siever_engine_bootstrap(s$data, thin, B = 5)
  expect_identical(r3$uncertainty$status, "unavailable")
  expect_match(r3$uncertainty$reason, "at least three active items", fixed = TRUE)
  expect_error(sieve_bifactor_boot_plan(s$data, s$meta, B = 0, engine = "siever"), "positive whole number")
  expect_error(sieve_bifactor_boot_plan(s$data, s$meta, B = 10.5, engine = "siever"), "positive whole number")
  # The shared level check (sieve_check_conf_level()) refuses the level
  # before anything is fitted, in its one wording.
  expect_error(sieve_bifactor_boot_plan(s$data, s$meta, conf_level = 1, engine = "siever"),
    "`conf_level` must be one number strictly between 0 and 1; received 1.", fixed = TRUE)
  expect_error(sieve_bifactor_boot_plan(s$data, s$meta, conf_level = 95, engine = "siever"),
    "A level is a proportion (0.95 for 95%), not a percentage.", fixed = TRUE)
})

test_that("too few usable draws give no interval, and say how many are needed", {
  s <- stable_missing()
  r <- siever_engine_bootstrap(s$data, s$meta, B = 6, seed = 7, conf_level = 0.95)
  expect_identical(r$counts$attempted, 6L)
  expect_identical(r$uncertainty$status, "failed")
  expect_match(r$uncertainty$reason, "at least 39 usable replications", fixed = TRUE)
  # Type 7 limits lie inside the draws at any count; the floor is about the
  # tail the most extreme replication cuts off, and the reason does not say
  # the limits could only repeat the extremes.
  expect_match(r$uncertainty$reason, "1 / (B + 1) of the bootstrap distribution", fixed = TRUE)
  expect_false(grepl("could only repeat", r$uncertainty$reason, fixed = TRUE))
  expect_true(all(is.na(r$table$lower)) && all(is.na(r$table$upper)))
  expect_true(all(r$table$status[r$table$index != "PUC"] == "failed"))
  expect_true(all(r$table$interval[r$table$index != "PUC"] == "failed"))
  expect_identical(r$table$interval[r$table$index == "PUC"], "fixed by the model")
  # The same six draws are the first six of the longer run (one seed per
  # draw, from the run's seed).
  expect_identical(unname(r$estimates), unname(boot_stable()$estimates[1:6, ]))
  expect_identical(r$draws$seed, boot_stable()$draws$seed[1:6])
})

test_that("the caution is raised when the unusable share equals the tail area exactly", {
  r <- boot_stable()
  # The well defined model: every replication usable, no caution, no range.
  expect_identical(r$counts$used, 40L)
  expect_identical(r$unusable_share, 0)
  expect_false(r$caution)
  expect_null(sieve_bifactor_boot_notes(r)$caution)
  expect_identical(r$table$lower_min, r$table$lower)
  expect_identical(r$table$upper_max, r$table$upper)
  # One replication in forty is 2.5 percent, the tail area of a 95 percent
  # interval: the one set aside could be the whole tail.
  s <- stable_missing()
  plan <- sieve_bifactor_boot_plan(s$data, s$meta, B = 40, seed = 7, engine = "siever")
  draws <- lapply(seq_len(40), function(b) {
    list(
      draw = b, seed = r$draws$seed[b], outcome = r$draws$outcome[b], returned = TRUE,
      converged = TRUE, admissible = TRUE, used = TRUE, message = "",
      values = unname(r$estimates[b, ])
    )
  })
  draws[[17]][c("outcome", "admissible", "used", "message")] <-
    list("inadmissible", FALSE, FALSE, "The solution is improper: negative variance estimate for c2.")
  one_out <- sieve_bifactor_boot_summary(plan, draws)
  expect_identical(one_out$counts$used, 39L)
  expect_equal(one_out$unusable_share, 0.025)
  expect_true(one_out$caution)
  expect_identical(one_out$uncertainty$status, "completed")
  expect_match(sieve_bifactor_boot_notes(one_out)$caution, "1 of the 40 replications attempted",
    fixed = TRUE
  )
  # The seventeenth replication's values are in no interval.
  for (j in which(one_out$table$status == "completed")) {
    expect_equal(one_out$table$lower[j],
      unname(stats::quantile(r$estimates[-17, j], 0.025)),
      tolerance = 1e-12
    )
  }
  # With two set aside, 38 usable replications are too few for 95 percent.
  draws[[18]][c("outcome", "admissible", "used")] <- list("inadmissible", FALSE, FALSE)
  two_out <- sieve_bifactor_boot_summary(plan, draws)
  expect_identical(two_out$uncertainty$status, "failed")
  expect_match(two_out$uncertainty$reason, "Only 38 of the 40", fixed = TRUE)
})

test_that("PUC is reported as fixed by the model, not as an interval of zero width", {
  r <- boot_stable()
  puc <- r$table[r$table$index == "PUC", ]
  expect_identical(nrow(puc), 1L)
  expect_identical(puc$status, "fixed")
  expect_identical(puc$interval, "fixed by the model")
  expect_true(is.na(puc$lower) && is.na(puc$upper) && is.na(puc$se))
  # It is the share of item pairs that span two group factors, whatever the
  # resample: three groups of three among nine items.
  expect_equal(puc$estimate, 1 - 3 * choose(3, 2) / choose(9, 2))
  expect_true(all(r$estimates[r$draws$used, "general: PUC"] == puc$estimate))
  # Every other index varies and has an interval that contains its estimate.
  rest <- r$table[r$table$index != "PUC", ]
  expect_true(all(rest$status == "completed"))
  expect_true(all(rest$interval == "Bootstrap percentile"))
  expect_true(all(rest$lower < rest$estimate & rest$estimate < rest$upper))
  expect_true(all(rest$se > 0))
  expect_equal(rest$mc_se, sieve_bootstrap_mc_error(r$counts$used, 0.95) * rest$se)
  expect_setequal(unique(r$table$index), c("ECV", "omega", "omega_H", "omega_HS", "PUC", "H"))
})

# ---- random numbers ---------------------------------------------------------

test_that("the same seed gives the same draws, a different seed different ones, and the stream is restored", {
  d <- bifactor_data(300,
    lg = rep(c(.55, .60, .65), 3), ls = rep(c(.50, .55, .45), 3),
    group = group9, seed = 5102
  )
  meta <- ledger(d, group9)

  set.seed(20261012)
  before <- get(".Random.seed", envir = globalenv())
  a <- siever_engine_bootstrap(d, meta, B = 8, seed = 113, conf_level = 0.5)
  # A supplied seed leaves the caller's stream exactly where it was.
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  b <- siever_engine_bootstrap(d, meta, B = 8, seed = 113, conf_level = 0.5)
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_identical(a$estimates, b$estimates)
  expect_identical(a$table, b$table)
  expect_identical(a$draws, b$draws)
  expect_identical(a$uncertainty$rng$seed, 113L)
  expect_true(a$uncertainty$rng$supplied)

  other <- siever_engine_bootstrap(d, meta, B = 8, seed = 114, conf_level = 0.5)
  expect_false(identical(a$estimates, other$estimates))
  expect_false(identical(a$draws$seed, other$draws$seed))
  expect_identical(a$table$estimate, other$table$estimate)

  # No seed: one is drawn from the caller's stream (which moves by exactly
  # that one draw), recorded, and replays the run.
  set.seed(77)
  unseeded <- siever_engine_bootstrap(d, meta, B = 8, conf_level = 0.5)
  after <- get(".Random.seed", envir = globalenv())
  set.seed(77)
  expected_seed <- sample.int(.Machine$integer.max, 1L)
  expect_identical(get(".Random.seed", envir = globalenv()), after)
  expect_identical(unseeded$uncertainty$rng$seed, expected_seed)
  expect_false(unseeded$uncertainty$rng$supplied)
  replay <- siever_engine_bootstrap(d, meta, B = 8, seed = expected_seed, conf_level = 0.5)
  expect_identical(unseeded$estimates, replay$estimates)
  expect_true(any(grepl(sprintf("set.seed(%d)  # no seed was supplied", expected_seed),
    unseeded$code,
    fixed = TRUE
  )))

  # The draws are the ones plain R gives for that seed.
  plan <- sieve_bifactor_boot_plan(d, meta, B = 8, seed = 113, conf_level = 0.5, engine = "siever")
  for (draw in c(1L, 8L)) {
    expect_identical(sieve_bifactor_boot_rows(plan, draw), hand_rows(113, 8, draw, 300))
  }
})

test_that("a seeded bootstrap in a fresh session leaves no random number stream behind", {
  skip_on_cran()
  root <- normalizePath(testthat::test_path("..", ".."))
  installed <- !file.exists(file.path(root, "DESCRIPTION"))
  load <- if (installed) "suppressMessages(library(SIEVER))" else
    sprintf("suppressMessages(pkgload::load_all(%s, quiet = TRUE, helpers = FALSE))", deparse(root))
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  # The data are generated on a local stream too, so the session has no
  # stream before the bootstrap is called.
  writeLines(c(
    sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
    load,
    "group <- rep(1:3, each = 3)",
    "lg <- rep(c(.55, .60, .65), 3)",
    "ls <- rep(c(.50, .55, .45), 3)",
    "d <- withr::with_seed(5102, {",
    "  G <- rnorm(300); S <- matrix(rnorm(900), 300); E <- matrix(rnorm(2700), 300)",
    "  as.data.frame(sapply(1:9, function(j) lg[j] * G + ls[j] * S[, group[j]] +",
    "    sqrt(1 - lg[j]^2 - ls[j]^2) * E[, j]))",
    "})",
    "names(d) <- paste0(rep(c('a', 'b', 'c'), each = 3), 1:3)",
    "meta <- data.frame(item = names(d), label = names(d),",
    "  construct = rep(c('Alpha', 'Beta', 'Gamma'), each = 3), facet = '',",
    "  type = 'continuous', status = 'active', reverse = FALSE, stringsAsFactors = FALSE)",
    "cat('before:', exists('.Random.seed', envir = globalenv()), '\\n')",
    "r <- SIEVER:::sieve_bifactor_bootstrap(d, meta, B = 3, seed = 5, conf_level = 0.2, engine = \"siever\")",
    "cat('attempted:', r$counts$attempted, '\\n')",
    "cat('after:', exists('.Random.seed', envir = globalenv()), '\\n')"
  ), script)
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)),
    stdout = TRUE, stderr = TRUE
  ))
  expect_true(any(grepl("^before: FALSE", out)), info = paste(out, collapse = "\n"))
  expect_true(any(grepl("^attempted: 3", out)), info = paste(out, collapse = "\n"))
  expect_true(any(grepl("^after: FALSE", out)), info = paste(out, collapse = "\n"))
})

test_that("a run can be stopped between draws, and what it holds is the start of the full run", {
  s <- stable_missing()
  full <- boot_stable()
  heard <- new.env()
  heard$done <- integer(0)
  stopper <- function(done, total, draw) {
    heard$done <- c(heard$done, done)
    expect_identical(total, 40L)
    expect_identical(draw$draw, done)
    done < 5L
  }
  part <- siever_engine_bootstrap(s$data, s$meta,
    B = 40, seed = 7, conf_level = 0.95,
    progress = stopper
  )
  expect_identical(heard$done, 1:5)
  expect_false(part$complete)
  expect_true(full$complete)
  expect_identical(part$counts$requested, 40L)
  expect_identical(part$counts$attempted, 5L)
  expect_identical(part$estimates, full$estimates[1:5, , drop = FALSE])
  expect_identical(part$draws, full$draws[1:5, ])
  expect_match(sieve_bifactor_boot_notes(part)$stopped, "canceled after 5 of the 40 replications", fixed = TRUE)
  # The recorded R of the stopped run asks for the first five draw seeds.
  expect_true(any(grepl("sample.int(.Machine$integer.max, 40L)[seq_len(5L)]", part$code, fixed = TRUE)))
  # A progress function that does not ask to stop does not stop the run.
  expect_identical(full$counts$attempted, 40L)
})

# ---- the recorded R ---------------------------------------------------------

test_that("the recorded R replays the draws, the counts, and the limits in a clean environment", {
  replay <- function(result, data) {
    env <- new.env(parent = globalenv())
    env$items <- data
    values <- withr::with_preserve_seed(
      lapply(parse(text = result$code), function(e) eval(e, envir = env))
    )
    list(
      counts = values[[length(values) - 2L]], limits = values[[length(values)]],
      used = env$bifactor_boot_used
    )
  }
  check <- function(result, data) {
    again <- replay(result, data)
    expect_identical(unname(again$counts[["attempted"]]), result$counts$attempted)
    expect_identical(unname(again$counts[["used"]]), result$counts$used)
    # The same draws: the used estimates are the same numbers, in order.
    expect_equal(unname(again$used), unname(result$estimates[result$draws$used, , drop = FALSE]),
      tolerance = 1e-10
    )
    done <- result$table[result$table$status == "completed", ]
    expect_identical(colnames(again$limits), paste(done$factor, done$index, sep = ": "))
    expect_equal(unname(again$limits[1, ]), done$lower, tolerance = 1e-10)
    expect_equal(unname(again$limits[2, ]), done$upper, tolerance = 1e-10)
  }
  # Incomplete rows under FIML, every draw usable.
  check(boot_stable(), stable_missing()$data)
  # The unstable model: the recorded rule sets aside the same draws.
  check(boot_unstable(), unstable()$data)
  code <- boot_stable()$code
  expect_true(any(grepl("estimator = \"ML\"", code, fixed = TRUE))) # Auto: ML with FIML
  expect_true(any(grepl("missing = \"fiml\"", code, fixed = TRUE)))
  expect_true(any(grepl("rowSums(!is.na(bifactor_boot_pool)) > 0", code, fixed = TRUE)))
  expect_true(any(code == "set.seed(7)"))
  expect_false(any(grepl("SIEVER", code, fixed = TRUE)))
  # A run with too few usable draws: the panel forms no interval, and the
  # recorded R, which states the same minimum, forms none either (the counts
  # still replay).
  s <- stable_missing()
  few <- siever_engine_bootstrap(s$data, s$meta, B = 6, seed = 7)
  expect_identical(few$uncertainty$status, "failed")
  expect_true(any(grepl("formed only from at least 39 usable draws", few$code, fixed = TRUE)))
  expect_true(any(grepl("if (NROW(bifactor_boot_used) >= 39L)", few$code, fixed = TRUE)))
  again <- replay(few, s$data)
  expect_identical(unname(again$counts[["attempted"]]), 6L)
  expect_identical(unname(again$counts[["used"]]), few$counts$used)
  expect_null(again$limits)
  # At 80 percent the minimum is 9, and the recorded R says so.
  expect_true(any(grepl("formed only from at least 9 usable draws", boot_unstable()$code, fixed = TRUE)))
})

# ---- the module -------------------------------------------------------------

# Drive a background DMAR bootstrap under testServer to its end: the page
# polls the background process every 200 ms of session time. The limit
# allows for a busy machine; a run still going at the limit fails here,
# saying so, rather than in the expectations on its results.
run_background <- function(session, running, seconds = 600) {
  siever_wait_until(function() !isTRUE(running()), what = "The page's background bootstrap",
    seconds = seconds, stop_on_timeout = TRUE, step = function() {
      session$elapse(200)
      Sys.sleep(0.05)
    })
}

boot_stable_dmar <- function() {
  if (is.null(cache$stable_dmar)) {
    s <- stable_missing()
    cache$stable_dmar <- sieve_bifactor_bootstrap(s$data, s$meta, B = 40, seed = 7, conf_level = 0.95)
  }
  cache$stable_dmar
}

test_that("the Bifactor tab runs DMAR's bootstrap in the background, with counts, progress and a recorded call", {
  # A small fixture for the run's lifecycle and its parity with DMAR, below
  # the 400 replications a 95 percent percentile interval requires; a run
  # of 400 replications is tested separately.
  siever_local_dmar_mechanics(environment())
  s <- stable_missing()
  st <- boot_state(s)
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    # Nothing to bootstrap before a model is fitted.
    session$setInputs(idx_B = 40, idx_seed = 7, idx_boot = 1)
    expect_match(as.character(output$idx_ci_note$html), "Fit the bifactor model first")
    expect_false(boot_running())

    session$setInputs(fit = 1)
    expect_identical(sieve_fit_status(res()$fit)$code, "ok")
    # The wait is estimated before anything is run, with what B costs.
    plan_note <- as.character(output$idx_plan_note$html)
    # (On several processes a range: DMAR's blocks each wait for their
    # slowest replication.)
    expect_match(plan_note, "40 replications should take (between )?about")
    expect_match(plan_note, "Monte Carlo standard error")
    expect_match(plan_note, "background R process")
    expect_true(is.finite(draw_seconds()) && draw_seconds() > 0)

    session$setInputs(idx_boot = 2)
    # The run is in progress, not finished inside the click: nothing is
    # started in the click itself, so the page shows the run and the Stop
    # button first; the next pass starts the background process.
    expect_true(boot_running())
    expect_identical(job$mode, "dmar")
    expect_null(job$handle)
    expect_match(as.character(output$idx_stop_ui$html), "Cancel the bootstrap")
    expect_match(as.character(output$idx_ci_note$html), "The bootstrap is running")
    session$elapse(200)
    session$elapse(200)
    expect_false(is.null(job$handle))
    run_background(session, boot_running)
    expect_false(boot_running())

    r <- idx_ci()
    full <- boot_stable_dmar()
    # The module's result is the function's, at full precision (DMAR's
    # limits do not depend on its processes for a given seed).
    expect_identical(r$table, full$table)
    expect_identical(r$estimates, full$estimates)
    expect_identical(r$draws, full$draws)
    expect_identical(r$counts, full$counts)
    expect_identical(r$uncertainty$status, "completed")

    note <- as.character(output$idx_ci_note$html)
    expect_match(note, "Bootstrap percentile", fixed = TRUE)
    expect_match(note, "DMAR::ci_bifactor_indices()", fixed = TRUE)
    expect_match(note, "400 rows with an observed item or auxiliary", fixed = TRUE)
    expect_match(note, "140 of them incomplete", fixed = TRUE)
    expect_match(note, "ML, full information maximum likelihood", fixed = TRUE)
    expect_match(note, sprintf("%d attempted", full$counts$attempted), fixed = TRUE)
    expect_match(note, sprintf("%d converged and admissible", full$counts$admissible), fixed = TRUE)
    expect_match(note, sprintf("%d used", full$counts$used), fixed = TRUE)
    expect_match(note, "Seed 7", fixed = TRUE)
    expect_match(note, "The same replications are used for every index", fixed = TRUE)
    expect_match(note, "Half rule", fixed = TRUE)
    tbl <- paste(as.character(unlist(output$idx_ci_tbl)), collapse = " ")
    expect_match(tbl, "Replications used", fixed = TRUE)
    expect_match(tbl, "Interval", fixed = TRUE)
    # Rounded in the renderer only; the stored table is at full precision.
    expect_match(tbl, "formatRound(data, 3", fixed = TRUE)
    expect_identical(idx_label(c("omega_H", "omega_HS", "PUC")), c("omegaH", "omegaHS", "PUC"))
    expect_identical(r$table$interval[r$table$index == "PUC"], "fixed by the model")
    # No range note when every replication was used.
    if (full$counts$used == full$counts$attempted) {
      expect_null(tryCatch(output$idx_ci_range, error = function(e) NULL))
    }

    # The recorded step is DMAR's call, with the session's counts and the
    # processes the background run used.
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("nonparametric bootstrap interval (percentile, B = 40, seed = 7; DMAR's counted bootstrap", log, fixed = TRUE)))
    expect_true(any(grepl(sprintf("%d replications attempted", full$counts$attempted), log, fixed = TRUE)))
    expect_true(grepl(paste(r$code, collapse = "\n"), paste(log, collapse = "\n"), fixed = TRUE))
    processes <- function(code) gsub("ncpus = [0-9]+|[0-9]+ process(es)?", "", code)
    expect_identical(processes(r$code), processes(full$code))
    expect_true(any(grepl(sprintf("ncpus = %d,", r$ncpus), r$code, fixed = TRUE)))
    expect_true(any(grepl("DMAR::ci_bifactor_indices(bifactor_boot_fit,", r$code, fixed = TRUE)))

    # The total score omega is on the same rows as the model: FIML, 400 rows.
    om <- total_omega()
    expect_identical(om$missing$used, "fiml")
    expect_identical(om$n$used, 400L)
    ref <- DMAR::reliability_omega(
      data = s$data, ci_method = "none",
      denominator = "observed", missing = "fiml"
    )
    expect_identical(om$estimate, ref$value[ref$term == "estimate"])
    verdict <- paste(as.character(unlist(output$essential)), collapse = " ")
    # The label names the denominator.
    expect_match(verdict, sprintf("Robust omega (observed denominator) = %.2f (point estimate", om$estimate), fixed = TRUE)
    expect_match(verdict, "full information maximum likelihood on the 400 rows", fixed = TRUE)

    # A run stopped part way: DMAR forms no interval from a canceled run, so
    # the page says so and records nothing.
    steps <- sum(startsWith(shiny::isolate(state$codelog), "# ["))
    session$setInputs(idx_B = 4000, idx_boot = 3)
    expect_true(boot_running())
    until <- proc.time()[["elapsed"]] + 60
    while ((is.null(job$handle) || job$handle$progress$done < 1L) && proc.time()[["elapsed"]] < until) {
      session$elapse(200)
      Sys.sleep(0.05)
    }
    session$setInputs(idx_stop = 1)
    run_background(session, boot_running, 60)
    expect_false(boot_running())
    expect_match(idx_ci()$notice, "DMAR forms no interval from a canceled run", fixed = TRUE)
    expect_match(as.character(output$idx_ci_note$html), "canceled after", fixed = TRUE)
    expect_identical(sum(startsWith(shiny::isolate(state$codelog), "# [")), steps)

    # The result belongs to the data and the ledger it was run on.
    session$setInputs(idx_B = 40, idx_boot = 4)
    run_background(session, boot_running)
    expect_identical(idx_ci()$uncertainty$status, "completed")
    meta <- shiny::isolate(state$meta)
    meta$status[meta$item == "c3"] <- "parked"
    state$meta <- meta
    session$flushReact()
    expect_match(idx_ci()$error, "changed after these intervals were run", fixed = TRUE)
    # And a new one is refused while the fit on screen is the old one.
    session$setInputs(idx_B = 40, idx_boot = 5)
    expect_false(boot_running())
    expect_match(idx_ci()$error, "changed after the model shown here was fitted", fixed = TRUE)
  })
})

test_that("the Bifactor tab retains defined improper estimates and reports the affected draws", {
  # Explicit exploratory outcome-count/display fixture, not inference adequacy.
  siever_local_dmar_mechanics(environment())
  # The nine Holzinger and Swineford tests give an improper bifactor
  # solution (the existing module tests assert it); DMAR's bootstrap is run
  # with improper = "keep", SIEVER's rule, which reports it with a warning.
  fx <- sieve_hs9_fixture()
  st <- sieve_new_state()
  st$raw <- fx$data
  st$items <- fx$data[, fx$item_cols, drop = FALSE]
  st$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  st$confirmed <- TRUE
  st$thresholds <- sieve_default_thresholds()
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    suppressWarnings(session$setInputs(fit = 1))
    # A fixed seed, so the resamples, and the time they take, are the same
    # in every run (no assertion reads the seed). Twenty replications show
    # the improper estimates and their report as forty would, in half the
    # time.
    session$setInputs(idx_B = 20, idx_seed = 1, idx_boot = 1)
    expect_true(boot_running())
    # The run is slow by nature, not by SIEVER: many resamples of this
    # improper solution are refits whose likelihood has no maximum, and DMAR
    # spends some seconds on each before it sets it aside, so on a slow or
    # busy machine the run takes minutes. It is given ten minutes, and a run
    # still going then ends the test here, by name.
    run_background(session, boot_running, 600)
    expect_false(boot_running())
    r <- idx_ci()
    expect_identical(r$point$outcome, "inadmissible")
    expect_identical(r$counts$attempted, 20L)
    note <- as.character(output$idx_ci_note$html)
    expect_match(note, "improper", fixed = TRUE)
    expect_true(any(grepl("nonparametric bootstrap interval", shiny::isolate(state$codelog), fixed = TRUE)))
  })

  # A result with unusable draws, shown by the module's renderers (SIEVER's
  # engine, which a declared cluster design still uses).
  st2 <- boot_state(unstable())
  shiny::testServer(mod_bifactor_server, args = list(state = st2), {
    suppressWarnings(session$setInputs(fit = 1))
    r <- boot_unstable()
    r$revision <- sieve_revision(st2)
    # Renderer fixture: explicitly mark this injected object current. The
    # real module test above checks the executed request and CI provenance.
    r$request <- idx_request()
    boot_result(r)
    session$flushReact()
    note <- as.character(output$idx_ci_note$html)
    expect_match(note, "Read the limits as conditional on a successful fit", fixed = TRUE)
    expect_match(note, "were set aside", fixed = TRUE)
    expect_match(note, "Not used:", fixed = TRUE)
    expect_match(note, "improper solution", fixed = TRUE)
    expect_match(note, "improper solutions retained", fixed = TRUE)
    range_note <- as.character(output$idx_ci_range$html)
    expect_match(range_note, "How far the limits could move", fixed = TRUE)
    tbl <- paste(as.character(unlist(output$idx_ci_range_tbl)), collapse = " ")
    expect_match(tbl, "CI lower, at least", fixed = TRUE)
  })
})

test_that("the bootstrap controls default to 1000 replications and state the rule", {
  ui <- as.character(mod_bifactor_ui("bf"))
  expect_match(ui, "id=\"bf-idx_B\"[^>]*value=\"1000\"")
  expect_match(ui, "The counts of replications attempted", fixed = TRUE)
  expect_match(ui, "DMAR::ci_bifactor_indices()", fixed = TRUE)
  expect_match(ui, "so resampling cannot move", fixed = TRUE)
  expect_match(ui, "do not carry the uncertainty of having chosen the items", fixed = TRUE)
  # The interval table sizes to its rows (it holds one row per index of
  # every factor), so it cannot run over the text beneath it.
  expect_false(grepl("id=\"bf-idx_ci_tbl\"[^>]*html-fill-item", ui))
  expect_false(grepl("html-fill-item[^>]*id=\"bf-idx_ci_tbl\"", ui))
  expect_match(ui, "Bootstrap index CIs", fixed = TRUE)
  expect_false(grepl(intToUtf8(8212L), ui, fixed = TRUE))
})

# ---- the total score omega on the Bifactor tab ------------------------------

test_that("the total score omega uses the rows and the coefficient the item types call for", {
  s <- stable_missing()
  om <- sieve_total_omega(s$data, s$meta)
  expect_identical(om$coefficient, "omega_robust")
  expect_identical(om$missing$used, "fiml")
  expect_identical(om$n$used, 400L)
  expect_identical(om$uncertainty$status, "not_requested")
  # The label names the denominator.
  expect_match(sieve_total_omega_label(om), "Robust omega (observed denominator) = ", fixed = TRUE)
  expect_match(sieve_total_omega_label(om), "(point estimate; full information", fixed = TRUE)
  # Rows with no observed item do not change it.
  padded <- sieve_total_omega(empty_rows(s$data, 500), s$meta)
  expect_identical(padded$estimate, om$estimate)
  expect_identical(padded$n$used, 400L)

  # Ordered items: categorical omega on the complete rows, as the bifactor
  # model for ordered items is fitted.
  o <- as.data.frame(lapply(s$data, function(v) as.numeric(cut(v, c(-Inf, -.7, 0, .7, Inf)))))
  meta_o <- ledger(o, group9, type = "ordered")
  om_o <- sieve_total_omega(o, meta_o)
  expect_identical(om_o$coefficient, "omega_categorical")
  expect_identical(om_o$missing$used, "listwise")
  expect_identical(om_o$n$used, sum(stats::complete.cases(o)))
  expect_match(sieve_total_omega_label(om_o),
    sprintf(
      "Robust omega (observed denominator: unrestricted polychoric correlations) = %.2f (point estimate; %d complete rows",
      om_o$estimate, om_o$n$used
    ),
    fixed = TRUE
  )
  # Parked items are not part of the total.
  fewer <- s$meta
  fewer$status[7:9] <- "parked"
  expect_identical(sieve_total_omega(s$data, fewer)$k, 6L)
  expect_null(sieve_total_omega(s$data, transform(s$meta, status = "parked")))
  expect_identical(sieve_total_omega_label(NULL), "not available")
})

# ---- the same pattern on the Reliability tab ---------------------------------

test_that("the Reliability tab's bootstrap runs in the background and a stop leaves the construct in progress not run", {
  # Robust omega has bootstrap intervals only, so every construct of the
  # nine Holzinger and Swineford tests has a bootstrap to run. It is DMAR's
  # counted bootstrap, in a background R process.
  fx <- sieve_hs9_fixture()
  st <- sieve_new_state()
  st$raw <- fx$data
  st$items <- fx$data[, fx$item_cols, drop = FALSE]
  st$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  st$confirmed <- TRUE
  st$palette <- "nd"
  st$thresholds <- sieve_default_thresholds()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "observed", ci_method = "percentile",
      missing = "listwise", conf_level = 0.80, B = 100, boot_seed = 31,
      show_alpha = FALSE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    before <- reliab()
    expect_identical(before$uncertainty, rep("pending", 3))
    expect_match(as.character(output$boot_note$html), "Cancel ends the bootstrap at its next replication")

    # A stop part way: the constructs completed keep their intervals, the
    # construct in progress gets none (DMAR forms none from a canceled run),
    # and every construct not completed is pending for the next run.
    session$setInputs(B = 3000)
    session$setInputs(run_boot = 1)
    # Nothing is resampled inside the click: the page shows the run first.
    expect_true(boot_running())
    expect_match(as.character(output$stop_boot_ui$html), "Cancel the bootstrap")
    expect_length(boot_job$done, 0L)
    expect_null(boot_job$handle)
    until <- proc.time()[["elapsed"]] + 60
    while ((is.null(boot_job$handle) || boot_job$handle$progress$done < 5L) && proc.time()[["elapsed"]] < until) {
      session$elapse(200)
      Sys.sleep(0.05)
    }
    expect_true(boot_running())
    session$setInputs(stop_boot = 1)
    run_background(session, boot_running, 60)
    expect_false(boot_running())
    part <- reliab()
    expect_identical(part$uncertainty, rep("pending", 3))
    expect_true(all(startsWith(part$interval, "bootstrap not run yet (Bootstrap percentile")))
    expect_setequal(boot_plan()$constructs, part$construct)
    expect_match(boot_last()$sentence, "was canceled after", fixed = TRUE)
    expect_identical(sum(grepl("Reliability: nonparametric bootstrap interval", shiny::isolate(state$codelog), fixed = TRUE)), 0L)

    # A full run.
    session$setInputs(B = 100)
    session$setInputs(run_boot = 2)
    run_background(session, boot_running)
    expect_false(boot_running())
    all_done <- reliab()
    expect_identical(all_done$uncertainty, rep("completed", 3))
    expect_identical(all_done$boot_attempted, rep(100L, 3))
    expect_true(all(all_done$interval == "Bootstrap percentile"))
    expect_length(boot_plan()$constructs, 0L)
    expect_null(tryCatch(output$stop_boot_ui, error = function(e) NULL))
    full <- results()[[1]]$omega
    expect_identical(full$uncertainty$rng$seed, 31L)
    expect_true(full$uncertainty$rng$supplied)
    log <- shiny::isolate(state$codelog)
    expect_identical(sum(grepl("Reliability: nonparametric bootstrap interval (percentile, B = 100, seed = 31",
      log,
      fixed = TRUE
    )), 1L)
    expect_true(any(grepl(sprintf("%s: 100 attempted", all_done$construct[1]), log, fixed = TRUE)))
    # The recorded DMAR call replays the interval on screen.
    env <- new.env(parent = globalenv())
    env$items <- shiny::isolate(state$items)
    withr::with_preserve_seed(eval(parse(text = full$code), envir = env))
    replay <- env[[sieve_reliability_object("omega", full$spec)]]
    expect_identical(c(replay$value[replay$term == "ci_lower"], replay$value[replay$term == "ci_upper"]),
      c(full$lower, full$upper))
    # The counts are in the method note.
    cnt <- full$uncertainty$counts
    expect_match(as.character(output$status_notes$html),
      sprintf(
        "Replications: 100 attempted, %d returned an estimate, %d converged, %d admissible, %d used; seed 31.",
        cnt$returned, cnt$converged, cnt$admissible, cnt$used
      ),
      fixed = TRUE
    )

    # A stop before the first replication leaves everything as it was.
    session$setInputs(B = 25)
    expect_identical(reliab()$uncertainty, rep("pending", 3))
    steps <- sum(startsWith(shiny::isolate(state$codelog), "# ["))
    session$setInputs(run_boot = 3)
    session$setInputs(stop_boot = 2)
    run_background(session, boot_running, 60)
    expect_false(boot_running())
    expect_identical(reliab()$uncertainty, rep("pending", 3))
    expect_identical(sum(startsWith(shiny::isolate(state$codelog), "# [")), steps)
  })
})

# ---- the fit on the Bifactor tab is bound to its data and ledger ------------

test_that("the bifactor fit on screen is bound to the data and the ledger it was fitted to", {
  s <- stable_missing()
  st <- boot_state(s)
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    session$setInputs(fit = 1, idx_B = 400, idx_seed = 7)
    expect_identical(sieve_fit_status(res()$fit)$code, "ok")
    expect_null(shown()$stale)
    expect_false(is.null(tryCatch(output$idx_tbl, error = function(e) NULL)))
    r <- boot_stable()
    r$revision <- sieve_revision(st)
    # Renderer fixture: explicitly mark this injected object current. The
    # real module test above checks the executed request and CI provenance.
    r$request <- idx_request()
    boot_result(r)
    session$flushReact()
    expect_identical(idx_ci()$uncertainty$status, "completed")

    # Other data: the fit of the data before is withheld from the page
    # (a result of other data does not stand for these data, so the
    # fit, its loadings, indices and reading do not stay on screen under a
    # line saying a different dataset was loaded). The line says where it
    # is kept and nothing of it is shown; the bootstrap computed before is
    # withheld with its reason, and a new one is refused until the model is
    # fit for these data.
    d2 <- s$data[1:300, ]
    state$raw <- d2
    state$items <- d2
    sieve_new_dataset(state)
    sieve_confirm_types(state)
    session$flushReact()
    expect_null(shown())
    expect_identical(demand$status()$state, "not_run")
    expect_true(demand$status()$withheld)
    status <- paste(as.character(unlist(output$status)), collapse = " ")
    expect_match(status, "The bifactor model is not fit yet.", fixed = TRUE)
    expect_match(status, "is kept in the analysis registry and the saved-results report, not shown for these data.", fixed = TRUE)
    expect_false(grepl("Shown below as computed (Bifactor model", status, fixed = TRUE))
    expect_false(grepl("a different dataset was loaded", status, fixed = TRUE))
    expect_match(idx_ci()$error, "changed after these intervals were run", fixed = TRUE)
    session$setInputs(idx_boot = 1)
    expect_false(boot_running())
    expect_false(is.null(idx_ci()$error))
    # The stored fit is still held, with its data (nothing is destroyed).
    expect_null(res()$fit)
    expect_identical(lavaan::lavInspect(demand$store()[[1L]]$value$fit, "nobs"), 400L)

    # A fit on the current data.
    session$setInputs(fit = 2)
    expect_null(shown()$stale)
    expect_identical(lavaan::lavInspect(res()$fit, "nobs"), 300L)
    expect_false(is.null(tryCatch(output$idx_tbl, error = function(e) NULL)))

    # This exclusion leaves fewer than three three-item group factors.
    # The invalid replacement model cannot be fit, so the held result is stale.
    meta <- shiny::isolate(state$meta)
    meta$status[meta$item == "c3"] <- "parked"
    state$meta <- meta
    session$flushReact()
    expect_identical(shown()$stale, "c3 excluded")
    expect_false(is.null(tryCatch(output$fit_tbl, error = function(e) NULL)))
    expect_match(paste(as.character(unlist(output$status)), collapse = " "),
      "What changed: c3 excluded",
      fixed = TRUE
    )
  })
})

test_that("the Bifactor tab reads the indices against stated conventions with their sources and gives no verdict", {
  s <- stable_missing()
  st <- boot_state(s)
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    session$setInputs(fit = 1)
    text <- paste(as.character(unlist(output$essential)), collapse = " ")
    ix <- res()$indices
    gen <- ix[ix$factor == "general", ]
    expect_match(text,
      sprintf(
        "ECV = %.2f, PUC = %.2f, omegaH (general) = %.2f, Omega (model-implied denominator, total) = %.2f, H (general) = %.2f.",
        gen$ECV, gen$PUC, gen$omega_H, gen$omega, gen$H
      ),
      fixed = TRUE
    )
    expect_false(grepl("Essentially unidimensional", text, fixed = TRUE))
    expect_false(grepl("Not clearly unidimensional", text, fixed = TRUE))
    expect_false(grepl("alert-success", text, fixed = TRUE))
    expect_match(text, "conventions, not a verdict", fixed = TRUE)
    expect_match(text, "No cutoff for essential unidimensionality is established", fixed = TRUE)
    # Each convention is named once, with its source, where its index is
    # defined, and the reading of the values points there: the .70 for H
    # (this paragraph does not also say "H is read against the .70
    # benchmark of Hancock and Mueller (2001)"), and the ECV of .80 with a
    # PUC of .77 and the omegaH of .93 against an omega of .98 (named in
    # the definitions, not here and again there).
    expect_match(text, "are each named once, with its source, where the index is defined under More on the indices below", fixed = TRUE)
    for (named in c("Hancock and Mueller", "Rodriguez", "ECV of .80", "PUC of .77", "omegaH of .93",
      "omega of .98", "in their example", ".70 as a convention", "benchmark", "anchors")) {
      expect_false(grepl(named, text, fixed = TRUE), info = named)
    }
    expect_false(grepl("and H is read against", text, fixed = TRUE))
    expect_match(text, "SIEVER does not decide it", fixed = TRUE)
  })
  ui <- as.character(mod_bifactor_ui("bf"))
  ui_text <- gsub("&amp;", "&", gsub("[[:space:]]+", " ", gsub("<[^>]+>", "", ui)), fixed = TRUE)
  expect_false(grepl("No cutoff is established", ui_text, fixed = TRUE))
  # One sentence each, with its source, saying SIEVER applies no cutoff.
  conventions <- c(
    "Rodriguez, Reise, and Haviland (2016) read an ECV of .80 with a PUC of .77 as common variance that is essentially unidimensional, and SIEVER applies no cutoff.",
    "Rodriguez et al. (2016) read an omegaH of .93 against an omega of .98 as a total score that is essentially univocal for the general factor, and SIEVER applies no cutoff.",
    "Hancock and Mueller (2001) proposed .70 as a convention for H, which Rodriguez et al. (2016) apply to the general and the group factors, and SIEVER applies no cutoff."
  )
  for (sentence in conventions) {
    expect_identical(lengths(regmatches(ui_text, gregexpr(sentence, ui_text, fixed = TRUE))), 1L)
  }
  for (value in c("ECV of .80", "PUC of .77", "omegaH of .93", "omega of .98", ".70 as a convention")) {
    expect_identical(lengths(regmatches(ui_text, gregexpr(value, ui_text, fixed = TRUE))), 1L, info = value)
  }
  expect_match(ui_text, "the higher the PUC, the less the size of ECV matters for the bias a unidimensional model would carry (Reise, Scheines, Widaman, & Haviland, 2013; Bonifay, Reise, Scheines, & Meijer, 2015)", fixed = TRUE)
  expect_false(grepl("benchmark of .70", ui, fixed = TRUE))
  expect_match(ui, "These are conventions with the citations above, not rules", fixed = TRUE)
  expect_false(grepl("above about .70", ui, fixed = TRUE))
  expect_false(grepl("above about .80", ui, fixed = TRUE))
  expect_false(grepl("Concluding:", ui, fixed = TRUE))
  expect_false(grepl(intToUtf8(8212L), ui, fixed = TRUE))
})

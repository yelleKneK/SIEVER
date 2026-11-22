# The confint() contract of a headless analysis, by interval type, over
# every sieve_analyze() service: its coverage, level validation, the model
# families fit outside lavaan, and row identity. Each service gives its stored limits at the stored level, Wald limits
# formed at another level with their source recorded, or a refusal that
# says exactly why.

confint_hs9 <- function() lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]

confint_meta <- function(items) {
  data.frame(
    item = names(items), label = names(items),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = "continuous", status = "active",
    reverse = FALSE, stringsAsFactors = FALSE
  )
}

computed <- "computed from the stored estimate and standard error"

# The limits alone, without row names or attributes, for comparison with
# an engine's own table.
bare <- function(x) matrix(as.numeric(x), ncol = 2L)

test_that("confint() validates the level before anything else", {
  items <- confint_hs9()
  a <- sieve_analyze("cfa", items_df = items, meta = confint_meta(items))
  # An invalid level is refused by name, never passed on to qnorm() (a
  # matrix of NaN for 1.5) or to arithmetic (an error from R for "a").
  for (bad in list(1.5, 0, 1, NA_real_, "a", c(.9, .95), -0.1, Inf)) {
    expect_error(stats::confint(a, level = bad),
      sprintf("`level` must be one number strictly between 0 and 1; received %s.", paste(deparse(bad), collapse = " ")),
      fixed = TRUE
    )
  }
})

test_that("cfa: lavaan parameters are named as coef() names them, fixed parameters are omitted, and Wald limits are formed at another level", {
  items <- confint_hs9()
  a <- sieve_analyze("cfa", items_df = items, meta = confint_meta(items))
  fit <- a$result
  stored <- stats::confint(a)
  free <- lavaan::parameterEstimates(fit, remove_nonfree = TRUE)
  # Row names are coef()'s, with no trailing space ("x1 ~1 "), and no
  # fixed parameter appears as a zero-width "interval".
  expect_identical(rownames(stored), names(lavaan::coef(fit)))
  expect_identical(colnames(stored), c("lower", "upper"))
  expect_equal(bare(stored), bare(as.matrix(free[, c("ci.lower", "ci.upper")])), tolerance = 1e-12)
  expect_identical(attr(stored, "conf_level"), 0.95)
  expect_identical(attr(stored, "source"), "stored")
  expect_identical(attr(stored, "analysis"), "cfa")
  expect_identical(attr(stored, "table"), "parameters")
  expect_identical(attr(stored, "transformation"), "none")
  expect_identical(attr(stored, "interval_method"), "Wald interval, normal reference, standard errors of lavaan's se = \"standard\"")
  at90 <- stats::confint(a, level = .90)
  oracle <- lavaan::parameterEstimates(fit, level = .90, remove_nonfree = TRUE)
  expect_equal(bare(at90), bare(as.matrix(oracle[, c("ci.lower", "ci.upper")])), tolerance = 1e-12)
  expect_identical(attr(at90, "source"), computed)
  # parm by name and by position; an unknown name is refused with the
  # names available (not R's "subscript out of bounds").
  expect_identical(rownames(stats::confint(a, parm = c("Visual=~x2", "x1~1"))), c("Visual=~x2", "x1~1"))
  expect_identical(rownames(stats::confint(a, parm = 2:3)), rownames(stored)[2:3])
  expect_error(stats::confint(a, parm = "Visual =~ x1"),
    "Unknown parameter(s): Visual =~ x1. Available: Visual=~x1, Visual=~x2, Visual=~x3, Textual=~x4, Textual=~x5, Textual=~x6, Speed=~x7, Speed=~x8, Speed=~x9, x1~~x1 (first 10 of 30).",
    fixed = TRUE
  )
  expect_error(stats::confint(a, parm = "Visual~~Visual"),
    "Visual~~Visual has no interval: it is fixed in the model, so it has no interval.",
    fixed = TRUE
  )
  expect_error(stats::confint(a, parm = 31), "Positions 31 are outside 1 to 30, the rows with limits in this table.", fixed = TRUE)
  # A table without limits is refused, naming the table with limits.
  expect_error(stats::confint(a, table = "Results"), "This cfa analysis has no table 'Results'; the tables with limits are 'parameters'.", fixed = TRUE)
})

test_that("cfa: a Wald row whose stored limits are not reproduced is refused at another level", {
  items <- confint_hs9()
  a <- sieve_analyze("cfa", items_df = items, meta = confint_meta(items))
  rows <- sieve_confint_lavaan_rows(a$result)
  rows$lower[1L] <- rows$lower[1L] - 1e-4
  expect_error(sieve_confint_limits(rows[1L, ], .90, "cfa", "parameters"),
    "its stored limits cannot be reproduced from its estimate and standard error",
    fixed = TRUE
  )
  # At the stored level the stored limits are returned as they are.
  expect_identical(unname(sieve_confint_limits(rows[1L, ], .95, "cfa", "parameters")[1L, ]), c(rows$lower[1L], rows$upper[1L]))
})

test_that("cfa: multiple group parameters carry the group suffix and defined parameters their label", {
  hs <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa("visual =~ x1 + a*x2 + x3\ntextual =~ x4 + x5 + x6\nab := a*2", hs,
    group = "school", group.equal = "loadings", std.lv = TRUE
  )
  obj <- structure(list(analysis = "cfa", result = fit, status = list(code = "ok", message = "")), class = "siever_analysis")
  at80 <- stats::confint(obj, parm = c("ab", "visual=~x2.g2", "x1~~x1"), level = .80)
  oracle <- lavaan::parameterEstimates(fit, level = .80)
  pick <- function(lhs, op, rhs, group) unlist(oracle[oracle$lhs == lhs & oracle$op == op & oracle$rhs == rhs & oracle$group == group, c("ci.lower", "ci.upper")])
  expect_equal(bare(at80["ab", ]), bare(pick("ab", ":=", "a*2", 0L)), tolerance = 1e-12)
  expect_equal(bare(at80["visual=~x2.g2", ]), bare(pick("visual", "=~", "x2", 2L)), tolerance = 1e-12)
  expect_equal(bare(at80["x1~~x1", ]), bare(pick("x1", "~~", "x1", 1L)), tolerance = 1e-12)
  expect_false(anyDuplicated(rownames(stats::confint(obj))) > 0L)
})

test_that("sem, bifactor, and longitudinal reach the lavaan path through their stored fits", {
  items <- confint_hs9()
  meta <- confint_meta(items)
  # Each reads the limits of its stored lavaan fit.
  sem <- sieve_analyze("sem", items_df = items, raw = items, meta = meta, outcomes = "Speed", predictors = c("Visual", "Textual"))
  s <- stats::confint(sem)
  stored <- sem$result$estimates$parameters
  at <- match(rownames(s), paste0(stored$lhs, stored$op, stored$rhs))
  expect_false(anyNA(at))
  expect_equal(bare(s), bare(as.matrix(stored[at, c("ci.lower", "ci.upper")])), tolerance = 1e-12)
  expect_identical(rownames(s), names(lavaan::coef(sem$result$fit)))
  expect_true("Speed~Visual" %in% rownames(s))
  # The stored table's own name reads the same limits.
  expect_identical(stats::confint(sem, table = "Results: estimates: parameters"), s)
  s90 <- stats::confint(sem, level = .90)
  oracle <- lavaan::parameterEstimates(sem$result$fit, level = .90, remove_nonfree = TRUE)
  expect_equal(bare(s90), bare(as.matrix(oracle[, c("ci.lower", "ci.upper")])), tolerance = 1e-12)
  expect_identical(attr(s90, "source"), computed)
  bif <- suppressWarnings(sieve_analyze("bifactor", items_df = items, meta = meta))
  b <- stats::confint(bif, level = .90)
  expect_identical(attr(b, "table"), "parameters")
  expect_identical(rownames(b), names(lavaan::coef(bif$result$fit)))
  oracle <- lavaan::parameterEstimates(bif$result$fit, level = .90, remove_nonfree = TRUE)
  expect_equal(bare(b), bare(as.matrix(oracle[, c("ci.lower", "ci.upper")])), tolerance = 1e-12)
  map <- data.frame(
    construct = "Democracy", wave = rep(c("1960", "1965"), each = 4L),
    item = rep(c("press", "opposition", "elections", "legislature"), 2L),
    variable = paste0("y", 1:8), wave_order = rep(1:2, each = 4L)
  )
  long <- sieve_analyze("longitudinal",
    data = lavaan::PoliticalDemocracy, mapping = map, level = "metric",
    justification = "The same four indicators were rated at both waves"
  )
  l <- stats::confint(long, level = .90)
  oracle <- lavaan::parameterEstimates(long$result$fit, level = .90, remove_nonfree = TRUE)
  expect_equal(bare(l), bare(as.matrix(oracle[, c("ci.lower", "ci.upper")])), tolerance = 1e-12)
  # Equality constraints label the loadings across waves; rows keep the
  # lhs, op, and rhs of each parameter rather than the shared label.
  expect_identical(rownames(l), ifelse(oracle$op == ":=", oracle$lhs, paste0(oracle$lhs, oracle$op, oracle$rhs)))
  # A model without a solution is refused with its status.
  failed <- sem
  failed$result$fit <- NULL
  failed$status <- list(code = "failed", message = "The model did not converge.")
  # The interval descriptor is filled when the object is built; an object
  # edited by hand has its descriptor read again from the edited result.
  failed$intervals <- sieve_analysis_intervals(failed)
  expect_error(stats::confint(failed), "The sem model has no solution, so it has no parameter intervals. The model did not converge.", fixed = TRUE)
})

test_that("reliability: a request that mixes Wald and bootstrap rows is refused as a whole, and parm selects the Wald rows", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  spec <- sieve_reliability_spec(names(d), rep("continuous", 3),
    construct = "Textual", missing = "fiml", ci_method = "mlr", denominator = "model_implied"
  )
  a <- sieve_analyze("reliability", data = d, spec = spec)
  # Mark omega's stored interval as a counted percentile bootstrap, as a
  # bootstrap request records it, to test the rule without 400 refits.
  a$result$omega$uncertainty$method <- "percentile"
  a$result$omega$uncertainty$label <- "Bootstrap percentile"
  a$result$omega$uncertainty$transformation <- "none (limits are percentiles of the replications used, by boot's rule)"
  a$result$omega$uncertainty$B_used <- 1000L
  a$intervals <- sieve_analysis_intervals(a)
  omega <- paste0("Textual: ", a$result$omega$label)
  others <- setdiff(rownames(stats::confint(a)), omega)
  expect_error(stats::confint(a, level = .90), paste0(
    "No limits are returned at level 0.9: 1 of the 3 rows requested are not formed at that level. ",
    omega, ": Bootstrap percentile, stored at level 0.95 (1000 replications used); another level needs the replications, which are not stored. ",
    "Run sieve_analyze(\"reliability\", data, spec = sieve_reliability_spec(..., conf_level = 0.9)) again; ",
    "DMAR forms a percentile interval at that level from at least 200 replications. ",
    "With level = NULL, confint() returns every stored interval at its own level. ",
    "Select the rows that can be formed at 0.9 with parm (", paste(others, collapse = ", "), ")."
  ), fixed = TRUE)
  # Never a partly missing matrix: the stored limits at their level, or
  # the Wald rows alone.
  expect_true(all(is.finite(stats::confint(a))))
  wald <- stats::confint(a, parm = others, level = .90)
  expect_identical(rownames(wald), others)
  expect_identical(attr(wald, "source"), computed)
})

test_that("reliability: Bonett's alpha interval is formed again on the ln(1 - alpha) scale", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  spec <- function(level) {
    sieve_reliability_spec(names(d), rep("continuous", 3),
      construct = "Textual", missing = "listwise", ci_method = "recommended",
      denominator = "model_implied", conf_level = level
    )
  }
  a <- sieve_analyze("reliability", data = d, spec = spec(.95))
  expect_identical(sub("[ (].*$", "", a$result$alpha$uncertainty$method), "bonett")
  expect_match(a$result$alpha$uncertainty$transformation, "^log\\(1 - alpha\\)")
  rerun <- stats::confint(sieve_analyze("reliability", data = d, spec = spec(.90)))
  at90 <- stats::confint(a, level = .90)
  expect_equal(bare(at90), bare(rerun[rownames(at90), ]), tolerance = 1e-12)
})

test_that("efa, retention, and invariance: RMSEA limits are read at their level and refused at another with the option that sets it", {
  items <- confint_hs9()
  meta <- confint_meta(items)
  # The RMSEA limits are read from the stored table at their own level.
  efa <- sieve_analyze("efa", data = items, estimator = "ML", ks = 1:3)
  e <- stats::confint(efa)
  expect_identical(rownames(e), c("k = 1: RMSEA", "k = 2: RMSEA", "k = 3: RMSEA"))
  expect_equal(bare(e), bare(as.matrix(efa$result[, c("RMSEA.lo", "RMSEA.hi")])), tolerance = 0)
  expect_identical(attr(e, "conf_level"), 0.90)
  expect_identical(stats::confint(efa, level = .90), e)
  expect_error(stats::confint(efa, level = .95), paste0(
    "No limits are returned at level 0.95: 3 of the 3 rows requested are not formed at that level. ",
    "k = 1: RMSEA, k = 2: RMSEA, k = 3: RMSEA: RMSEA interval by inversion of the noncentral chi-square distribution ",
    "of the test statistic (standard family), stored at level 0.9; another level needs the noncentral chi-square ",
    "distribution inverted again at that level. Set options(sieve.rmsea_conf_level = 0.95) and run ",
    "sieve_analyze(\"efa\", ...) again (or pass conf_level = 0.95) for RMSEA limits at that level. ",
    "With level = NULL, confint() returns every stored interval at its own level."
  ), fixed = TRUE)
  # A table that records no level is refused, saying so.
  bare <- efa
  attr(bare$result, "conf_level") <- NULL
  bare$intervals <- sieve_analysis_intervals(bare)
  expect_error(stats::confint(bare),
    "The table names interval limits (RMSEA.lo, RMSEA.hi) but records no confidence level, so they are not returned as an interval.",
    fixed = TRUE
  )
  rt <- suppressWarnings(sieve_analyze("retention", X = items[, 1:6], types = rep("continuous", 6), n_iter = 200L, max_k = 2L, seed = 1))
  r <- stats::confint(rt)
  expect_identical(attr(r, "table"), "Results: fit: table")
  expect_equal(bare(r), bare(as.matrix(rt$result$fit$table[, c("rmsea_lo", "rmsea_hi")])), tolerance = 0)
  expect_identical(attr(r, "conf_level"), rt$result$fit$conf_level)
  expect_error(stats::confint(rt, level = .95), "Set options(sieve.rmsea_conf_level = 0.95) and run sieve_analyze(\"retention\", ...) again", fixed = TRUE)
  pa <- stats::confint(rt, table = "Results: pa: dmar: normal")
  expect_identical(attr(pa, "conf_level"), 0.95)
  expect_identical(rownames(pa), rt$result$pa$dmar$normal$term)
  expect_error(stats::confint(rt, table = "Results: pa: dmar: normal", level = .90),
    "Monte Carlo binomial order-statistic limits for the reference percentile (DMAR), stored at level 0.95; another level selects other order statistics, which the result does not keep.",
    fixed = TRUE
  )
  # A table summary() lists without limits is refused, naming those with limits.
  expect_error(stats::confint(rt, table = "Results: basis: n"),
    "Table 'Results: basis: n' holds no confidence limits that confint() reads; the tables with limits are 'Results: fit: table', 'Results: pa: dmar: normal', 'Results: pa: dmar: resample'.",
    fixed = TRUE
  )
  hs <- lavaan::HolzingerSwineford1939
  inv <- sieve_analyze("invariance", data = hs[, c(paste0("x", 1:9), "school")], group = "school", meta = meta, levels = c("configural", "metric"))
  v <- stats::confint(inv)
  expect_identical(rownames(v), c("configural: RMSEA", "metric: RMSEA"))
  expect_equal(bare(v), bare(as.matrix(inv$result$compare$table[, c("rmsea_lo", "rmsea_hi")])), tolerance = 0)
  expect_identical(attr(v, "conf_level"), inv$result$compare$conf_level)
  expect_error(stats::confint(inv, level = .95), "Set options(sieve.rmsea_conf_level = 0.95) and run sieve_analyze(\"invariance\", ...) again", fixed = TRUE)
  expect_identical(attr(stats::confint(inv, table = "Results: compare: by_family: standard"), "table"), "Results: compare: by_family: standard")
})

test_that("cross_validation: RMSEA and ECVI keep their own levels, and the split reliability is refused at another level", {
  items <- confint_hs9()
  cv <- sieve_analyze("cross_validation", data = items, meta = confint_meta(items), seed = 1)
  t <- stats::confint(cv)
  samples <- cv$result$table$Sample
  expect_identical(rownames(t), c(paste0(samples, ": RMSEA"), paste0(samples, ": ECVI")))
  expect_identical(attr(t, "conf_level"), stats::setNames(c(.90, .90, .95, .95), rownames(t)))
  # A request at 0.90 keeps the stored RMSEA rows and refuses the ECVI rows.
  expect_error(stats::confint(cv, level = .90), paste0(
    "No limits are returned at level 0.9: 2 of the 4 rows requested are not formed at that level. ",
    paste0(samples, ": ECVI", collapse = ", "),
    ": ECVI interval from the noncentral chi-square limits of the noncentrality (DMAR::ecvi()), stored at level 0.95; ",
    "another level needs the noncentral chi-square distribution inverted again at that level. ",
    "sieve_cross_validate() forms ECVI limits at 0.95 only; DMAR::ecvi(fit, conf_level = 0.9) forms them at another level for a fitted half. ",
    "With level = NULL, confint() returns every stored interval at its own level."
  ), fixed = TRUE)
  rmsea <- stats::confint(cv, parm = paste0(samples, ": RMSEA"), level = .90)
  expect_identical(attr(rmsea, "source"), "stored")
  rel <- stats::confint(cv, table = "rel")
  expect_identical(attr(rel, "conf_level"), 0.95)
  expect_true(all(grepl(": (calibration|validation|difference)$", rownames(rel))))
  expect_error(stats::confint(cv, table = "rel", level = .90), "without its standard error", fixed = TRUE)
})

test_that("factor_scores and norms keep row identity; norm limits are refused at another level", {
  items <- confint_hs9()
  meta <- confint_meta(items)
  norms <- sieve_analyze("norms", data = items, meta = meta)
  n <- stats::confint(norms)
  u <- norms$result$uncertainty
  # Each row names its construct and percentile ("Visual: p = 0.05", not
  # "Visual.1").
  expect_identical(rownames(n)[1:2], c("Visual: p = 0.05", "Visual: p = 0.1"))
  expect_equal(bare(n), bare(as.matrix(u[, c("lower", "upper")])), tolerance = 0)
  expect_identical(attr(n, "table"), "Results: uncertainty")
  expect_error(stats::confint(norms, level = .90),
    "pointwise distribution-free binomial order-statistic limits; conservative with ties, stored at level 0.95; another level selects other order statistics, which the result does not keep.",
    fixed = TRUE
  )
  fit <- lavaan::cfa("Visual =~ x1 + x2 + x3", items, std.lv = TRUE)
  fs <- sieve_analyze("factor_scores", fit = fit, data = items[, 1:3], conf_level = .90)
  f <- stats::confint(fs)
  expect_identical(rownames(f)[1:2], c("row 1: Visual", "row 2: Visual"))
  expect_identical(attr(f, "conf_level"), 0.90)
})

test_that("ave and content: DMAR levels are read through dmar_metadata(); bootstrap and exact binomial limits are refused at another level", {
  ratings <- withr::with_seed(1, matrix(sample(1:4, 40, TRUE), 10, 4))
  content <- sieve_analyze("content", ratings = ratings)
  cv <- stats::confint(content)
  # The level stored in DMAR's metadata is matched and the limits are
  # returned at it.
  expect_identical(rownames(cv), content$result$item)
  expect_equal(bare(cv), bare(as.matrix(content$result[, c("ci_lower", "ci_upper")])), tolerance = 0)
  expect_identical(attr(cv, "conf_level"), DMAR::dmar_metadata(content$result)$conf_level)
  expect_identical(stats::confint(content, level = .95), cv)
  expect_error(stats::confint(content, level = .90),
    "Exact (Clopper-Pearson) binomial interval for the item content validity index (DMAR), stored at level 0.95; DMAR forms it from the expert counts at the level it is asked for. Run sieve_analyze(\"content\", ratings = ..., conf_level = 0.9) again for limits at that level.",
    fixed = TRUE
  )
  # A level column that disagrees with DMAR's metadata is refused.
  clash <- content
  clash$result$conf_level <- 0.90
  clash$intervals <- sieve_analysis_intervals(clash)
  expect_error(stats::confint(clash),
    "The table records level 0.9 in its conf_level column and 0.95 in its DMAR metadata (conf_level); the limits cannot be attributed to one level.",
    fixed = TRUE
  )
  skip_on_cran()
  items <- confint_hs9()
  fit <- lavaan::cfa("Visual =~ x1 + x2 + x3\nTextual =~ x4 + x5 + x6", items, std.lv = TRUE)
  ave <- sieve_analyze("ave", fit = fit, ci_method = "percentile", B = 400L, seed = 1)
  av <- stats::confint(ave)
  expect_identical(rownames(av), ave$result$factor)
  expect_equal(bare(av), bare(as.matrix(ave$result[, c("ci_lower", "ci_upper")])), tolerance = 0)
  expect_identical(attr(av, "interval_method"), "Bootstrap percentile interval (DMAR)")
  expect_error(stats::confint(ave, level = .90), paste0(
    "Bootstrap percentile interval (DMAR), stored at level 0.95 (400 replications used); another level needs the replications, which are not stored. ",
    "Run sieve_analyze(\"ave\", fit = ..., ci_method = \"percentile\", conf_level = 0.9, B = 200 or more) again; ",
    "DMAR forms a percentile interval at that level from at least 200 replications."
  ), fixed = TRUE)
})

test_that("htmt, screening, and irt are refused with the reason", {
  items <- confint_hs9()
  htmt <- sieve_analyze("htmt", data = items, blocks = list(V = paste0("x", 1:3), T = paste0("x", 4:6)), B = 200, seed = 1)
  # The refusal names the one-sided bound the table holds (ci_upper).
  for (level in list(NULL, .95, .90)) {
    expect_error(stats::confint(htmt, level = level), paste0(
      "HTMT reports a one-sided upper confidence bound at level 0.95 (column ci_upper), not a two-sided interval, ",
      "and confint() returns two-sided intervals only. Read as.data.frame(result)$ci_upper"
    ), fixed = TRUE)
  }
  screening <- sieve_analyze("screening", data = items)
  expect_error(stats::confint(screening),
    "The screening analysis holds no confidence limits: its tables are counts, flags, and thresholds of the response screens.",
    fixed = TRUE
  )
  ordered <- as.data.frame(lapply(items[, 1:6], function(x) as.integer(cut(x, 4))))
  irt <- suppressWarnings(sieve_analyze("irt", data = ordered))
  expect_error(stats::confint(irt),
    "The graded response model's table holds item parameters without standard errors or confidence limits. The RMSEA interval of its fit (level 0.9) is in attr(result$result, \"fit_measures\").",
    fixed = TRUE
  )
})

test_that("an analysis that returned no interval table is refused with its own reason", {
  items <- confint_hs9()
  meta <- confint_meta(items)
  cv <- sieve_analyze("cross_validation", data = items[1:15, ], meta = meta)
  expect_identical(cv$status$code, "failed")
  expect_error(stats::confint(cv), paste("This analysis has no fit table, so it has no limits to return.", cv$result$error), fixed = TRUE)
  norms <- sieve_analyze("norms", data = items[0, ], meta = meta)
  expect_identical(norms$status$code, "failed")
  expect_error(stats::confint(norms), "This analysis has no table of percentile limits, so it has no limits to return.", fixed = TRUE)
  inv <- structure(list(analysis = "invariance", result = list(error = "lavaan refused the data."),
    status = list(code = "failed", message = "lavaan refused the data.")), class = "siever_analysis")
  expect_error(stats::confint(inv), "This invariance analysis holds no table with confidence limits. lavaan refused the data.", fixed = TRUE)
})

test_that("the HTMT and AVE rerun advice states DMAR's floor of 100 replications beside its formulas", {
  # The bare formulas, B = 10 / min(C, 1 - C) and B = 10 / ((1 - C) / 2),
  # give 50 for an HTMT bound at C = 0.80, which DMAR 1.1.0 refuses (it
  # never forms a limit from fewer than 100), so the advice states the
  # floor, and a filled rerun names 100, not "10 / min(0.8, 1 - 0.8)".
  items <- confint_hs9()
  blocks <- list(V = paste0("x", 1:3), T = paste0("x", 4:6))
  bounded <- sieve_analyze("htmt", data = items, blocks = blocks, B = 200, seed = 1)
  message <- tryCatch(stats::confint(bounded), error = function(e) conditionMessage(e))
  expect_match(message, paste(
    "for a bound at another level C, run sieve_analyze(\"htmt\", ..., conf_level = C,",
    "B = max(100, 10 / min(C, 1 - C)) or more, seed = ...) again (DMAR forms the bound from at least that",
    "many replications, 200 at 0.95)."
  ), fixed = TRUE)
  unbounded <- sieve_analyze("htmt", data = items, blocks = blocks)
  filled <- sieve_confint_fill_rerun(unbounded$intervals$rerun, .80, one_sided = TRUE)
  expect_match(filled, "conf_level = 0.8, B = 100 or more, seed = ...)", fixed = TRUE)
  expect_match(filled, "DMAR forms the bound from at least 100 replications.", fixed = TRUE)
  expect_false(grepl("10 / min", filled, fixed = TRUE))
  expect_match(sieve_confint_fill_rerun(unbounded$intervals$rerun, .99, one_sided = TRUE),
    "DMAR forms the bound from at least 1000 replications.", fixed = TRUE)
  # The advice for a level the call does not record, should DMAR's default
  # ever stop being one, carries the floor as well.
  test_frame <- environment()
  local_mocked_bindings(sieve_confint_asked_level = function(given, engine) NA_real_, .env = test_frame)
  fallback <- sieve_analyze("htmt", data = items, blocks = blocks)
  expect_match(fallback$intervals$reason,
    "conf_level = C, B = max(100, 10 / min(C, 1 - C)) or more, seed = ...) for a one-sided upper bound at level C",
    fixed = TRUE)
  skip_on_cran()
  fit <- lavaan::cfa("Visual =~ x1 + x2 + x3\nTextual =~ x4 + x5 + x6", items, std.lv = TRUE)
  ave <- sieve_analyze("ave", fit = fit)
  expect_match(ave$intervals$reason[1L],
    "conf_level = C, B = max(100, 10 / ((1 - C) / 2)) or more, seed = ...) for a percentile interval at level C",
    fixed = TRUE)
})

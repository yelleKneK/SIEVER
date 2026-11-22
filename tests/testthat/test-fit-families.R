# Fit-index families and comparisons of models. Four properties are pinned
# here:
#
# * the standard, scaled, and robust families are stored under their own
#   names, each with its availability and the reason when it is not
#   reported, and a scaled value is never shown under the name robust;
# * one family is selected for display by a stated rule and is named
#   wherever a value is shown;
# * a change in CFI or RMSEA is a difference of two values from ONE family
#   that every model in the comparison reports and that describes the whole
#   of every model (no fallback model by model);
# * a nested comparison is the engine's scaled chi square difference test,
#   never a subtraction of scaled statistics.
#
# Expected values come from sources that do not pass through the functions
# under test: the fit function and the baseline statistic computed from the
# data by hand, the published formulas of each family applied by hand, the
# Satorra and Bentler (2001) difference formula applied by hand,
# lavaan::fitMeasures() and lavaan::lavTestLRT() called directly on the
# fitted models, and populations with a known violation of invariance.
#
# ORDERED MULTIPLE GROUP FIXTURES (both documented where they are built):
# 1. fam_ordered_hs(): the nine bundled Holzinger and Swineford tests, each
#    cut at its quartiles into four ordered categories, with school (Pasteur,
#    Grant-White) as the group. Real data; what is true in the population is
#    not known.
# 2. fam_ordered_sim(): simulated, two groups of 500, six ordered items with
#    four categories on one factor (loadings .7, thresholds -.8, 0, .8), seed
#    20261012. Loadings are equal across groups; in the second group every
#    threshold of items 1 and 2 is higher by `shift` and the factor mean is
#    .3. With shift = .5 scalar invariance is violated by construction and
#    the rungs before it are not; with shift = 0 the data are invariant.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

fam_meta <- function(x, construct = "F", type = "continuous") {
  data.frame(
    item = names(x), label = names(x),
    construct = rep(construct, length.out = ncol(x)), facet = "",
    type = rep(type, length.out = ncol(x)),
    status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
}

fam_hs <- function() {
  fx <- sieve_hs9_fixture()
  list(
    fx = fx, items = fx$data[, fx$item_cols, drop = FALSE],
    meta = sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  )
}

# Fixture 1: the nine tests cut at their quartiles into four ordered
# categories, grouped by school.
fam_ordered_hs <- function(which = NULL) {
  hs <- fam_hs()
  d <- hs$fx$data
  cut_items <- if (is.null(which)) hs$fx$item_cols else which
  for (it in cut_items) {
    d[[it]] <- as.integer(cut(d[[it]],
      breaks = stats::quantile(d[[it]], c(0, .25, .5, .75, 1)),
      include.lowest = TRUE
    ))
  }
  meta <- hs$meta
  meta$type[meta$item %in% cut_items] <- "ordered"
  list(fx = hs$fx, data = d, meta = meta)
}

# Fixture 2: simulated ordered data for two groups with a known violation.
fam_ordered_sim <- function(shift = 0.5, n = 500L, seed = 20261012L) {
  items <- paste0("y", 1:6)
  one_group <- function(extra, mean_f) {
    eta <- stats::rnorm(n, mean_f, 1)
    out <- lapply(seq_along(items), function(j) {
      ystar <- 0.7 * eta + stats::rnorm(n, 0, sqrt(1 - 0.49))
      as.integer(cut(ystar, c(-Inf, c(-0.8, 0, 0.8) + if (j <= 2L) extra else 0, Inf)))
    })
    stats::setNames(as.data.frame(out), items)
  }
  d <- withr::with_seed(seed, rbind(
    cbind(one_group(0, 0), g = "one"),
    cbind(one_group(shift, 0.3), g = "two")
  ))
  list(data = d, items = items, meta = fam_meta(d[items], "F", "ordered"))
}

# estimator_choice: the study's setting. The tests whose subject is the
# three families of a continuous fit choose MLR, under which lavaan reports
# all three (Follow the item types gives continuous items ML with FIML,
# which has no scaled statistic).
fam_state <- function(data = NULL, meta = NULL, estimator_choice = "auto") {
  hs <- fam_hs()
  if (!is.null(data)) hs$fx$data <- data
  meta <- meta %||% hs$meta
  siever_test_study(hs$fx, configure = function(s) {
    s$estimator_choice <- estimator_choice
    s$meta <- meta
    roles <- sieve_declared_roles(s$raw, names(s$items))
    roles$role[roles$role == "auxiliary"] <- "criterion"
    s$roles <- roles
  })
}

# The text of a rendered output, with the HTML escape of "&" undone.
fam_text <- function(x) {
  if (is.null(x)) "" else gsub("&amp;", "&", paste(as.character(unlist(x)), collapse = " "), fixed = TRUE)
}

# lavaan's value under its own name, with nothing in between.
# lavaan's value of a measure, with the RMSEA interval at SIEVER's RMSEA
# level (0.90, DMAR's default for the RMSEA interval and lavaan's own).
lav <- function(fit, name) {
  as.numeric(suppressWarnings(lavaan::fitMeasures(fit, fm.args = list(rmsea.ci.level = 0.90)))[name])
}

# The ladders are fit once and shared by the tests that read them.
fam_cache <- new.env(parent = emptyenv())
fam_ladder <- function(which) {
  if (is.null(fam_cache[[which]])) {
    fam_cache[[which]] <- switch(which,
      continuous = {
        hs <- fam_hs()
        d <- cbind(hs$items, school = hs$fx$data$school)
        # The continuous MLR ladder: the scaled and robust families exist
        # under MLR, so it is chosen (Follow the item types gives continuous
        # items ML with FIML).
        list(data = d, meta = hs$meta, res = sieve_fit_invariance(d, "school", hs$meta, estimator = "mlr"))
      },
      ordered = {
        o <- fam_ordered_hs()
        d <- o$data[, c(o$fx$item_cols, "school")]
        list(data = d, meta = o$meta, res = sieve_fit_invariance(d, "school", o$meta))
      },
      mixed = {
        o <- fam_ordered_hs(which = fam_hs()$fx$theory_map[[1]])
        d <- o$data[, c(o$fx$item_cols, "school")]
        list(data = d, meta = o$meta, res = sieve_fit_invariance(d, "school", o$meta))
      },
      sim = {
        s <- fam_ordered_sim(0.5)
        list(data = s$data, meta = s$meta, res = sieve_fit_invariance(s$data, "g", s$meta))
      }
    )
  }
  fam_cache[[which]]
}

# ---- Three families, each under its own name ---------------------------------

test_that("the three families are stored under their own names at full precision", {
  hs <- fam_hs()
  # MLR with FIML, chosen: the three families exist under MLR (Follow the
  # item types gives ML with FIML, which has no scaled one).
  fit <- sieve_fit_cfa(hs$items, hs$meta, estimator = "mlr")
  x <- sieve_fit_families(fit)
  expect_s3_class(x, "sieve_fit_families")
  expect_identical(names(x$families), c("standard", "scaled", "robust"))

  # Every stored value is lavaan's value of exactly that name: the scaled
  # family holds the .scaled columns, the robust family the .robust columns,
  # and each RMSEA comes with the interval of its own family.
  cols <- list(
    standard = c(
      chisq = "chisq", df = "df", pvalue = "pvalue", cfi = "cfi", tli = "tli",
      rmsea = "rmsea", rmsea_lo = "rmsea.ci.lower", rmsea_hi = "rmsea.ci.upper"
    ),
    scaled = c(
      chisq = "chisq.scaled", df = "df.scaled", pvalue = "pvalue.scaled",
      cfi = "cfi.scaled", tli = "tli.scaled", rmsea = "rmsea.scaled",
      rmsea_lo = "rmsea.ci.lower.scaled", rmsea_hi = "rmsea.ci.upper.scaled"
    ),
    robust = c(
      cfi = "cfi.robust", tli = "tli.robust", rmsea = "rmsea.robust",
      rmsea_lo = "rmsea.ci.lower.robust", rmsea_hi = "rmsea.ci.upper.robust"
    )
  )
  for (family in names(cols)) {
    for (index in names(cols[[family]])) {
      expect_equal(x$families[[family]]$values[[index]], lav(fit, cols[[family]][[index]]),
        tolerance = 1e-12, info = paste(family, index)
      )
    }
    expect_true(x$families[[family]]$available, info = family)
    expect_true(x$families[[family]]$complete, info = family)
    expect_identical(x$families[[family]]$scope, "model", info = family)
    expect_true(is.na(x$families[[family]]$reason), info = family)
  }
  # The robust family has no test statistic of its own, and says so.
  expect_true(all(is.na(x$families$robust$values[c("chisq", "df", "pvalue")])))
  expect_match(x$families$robust$missing[["chisq"]], "no test statistic of its own", fixed = TRUE)
  expect_true(all(is.na(x$families$robust$columns[c("chisq", "df", "pvalue")])))

  # On this fit the scaled CFI and RMSEA are .9252068 and .0934834, and the
  # robust values are about .932 and .091: different statistics, not
  # rounding, so neither may stand in for the other.
  expect_equal(x$families$scaled$values[["cfi"]], 0.9252068, tolerance = 1e-6)
  expect_equal(x$families$scaled$values[["rmsea"]], 0.0934834, tolerance = 1e-6)
  expect_equal(x$families$robust$values[["cfi"]], 0.932, tolerance = 1e-3)
  expect_equal(x$families$robust$values[["rmsea"]], 0.091, tolerance = 5e-3)
  expect_gt(abs(x$families$robust$values[["cfi"]] - x$families$scaled$values[["cfi"]]), 5e-3)
  expect_gt(abs(x$families$robust$values[["rmsea"]] - x$families$scaled$values[["rmsea"]]), 1e-3)
  expect_gt(abs(x$families$robust$values[["rmsea_lo"]] - x$families$scaled$values[["rmsea_lo"]]), 1e-3)

  # SRMR, AIC, and BIC do not come in families and are kept separately. The
  # SRMR is of the covariance residuals alone, DMAR's SRMR (lavaan's
  # srmr_bentler_nomean), not lavaan's srmr, which also averages in the mean
  # residuals of a model with a mean structure.
  for (other in c("srmr", "aic", "bic")) {
    expect_equal(x[[other]]$value, lav(fit, if (other == "srmr") "srmr_bentler_nomean" else other),
      tolerance = 1e-12, info = other)
    expect_true(x[[other]]$available, info = other)
  }
  expect_identical(x$srmr$measure, "srmr_bentler_nomean")
  expect_equal(x$srmr$lavaan, lav(fit, "srmr"), tolerance = 1e-12)
  expect_equal(x$ci_level, 0.90)
  expect_equal(x$npar, lav(fit, "npar"))
  expect_identical(x$status, "ok")
  expect_identical(x$test, c("standard", "yuan.bentler.mplus"))

  # Stored at full precision: no value is a rounded number.
  stored <- unlist(lapply(x$families, function(f) f$values[c("cfi", "tli", "rmsea")]))
  expect_false(isTRUE(all.equal(stored, round(stored, 3), tolerance = 1e-9)))
})

test_that("each family is the statistic its name says, computed by hand", {
  hs <- fam_hs()
  X <- hs$items
  expect_identical(sum(stats::complete.cases(X)), nrow(X)) # complete data
  # Robust maximum likelihood on complete data, fit by lavaan directly.
  fit <- lavaan::cfa(sieve_cfa_plan(hs$meta)$model, data = X, estimator = "MLR", std.lv = TRUE)
  x <- sieve_fit_families(fit)

  # Standard family, from the data: the ML discrepancy of the implied
  # covariance matrix, the independence model's statistic from the
  # correlation matrix, and the textbook formulas.
  n <- nrow(X)
  p <- ncol(X)
  S <- stats::cov(X) * (n - 1) / n
  Sigma <- lavaan::lavInspect(fit, "implied")$cov[names(X), names(X)]
  chisq <- n * (log(det(Sigma)) + sum(diag(S %*% solve(Sigma))) - log(det(S)) - p)
  base <- -n * log(det(stats::cor(X)))
  df <- p * (p + 1) / 2 - (2 * p + 3) # moments less loadings, residuals, covariances
  df_base <- p * (p - 1) / 2
  expect_equal(df, 24)
  cfi <- function(t, d, tb, db) 1 - max(t - d, 0) / max(tb - db, t - d, 0)
  rmsea <- function(t, d) sqrt(max((t - d) / (n * d), 0))
  expect_equal(x$families$standard$values[["chisq"]], chisq, tolerance = 1e-6)
  expect_equal(x$families$standard$values[["df"]], df)
  expect_equal(x$families$standard$values[["cfi"]], cfi(chisq, df, base, df_base), tolerance = 1e-6)
  expect_equal(x$families$standard$values[["rmsea"]], rmsea(chisq, df), tolerance = 1e-6)
  expect_equal(x$families$standard$values[["tli"]],
    1 - ((chisq - df) * df_base) / ((base - df_base) * df),
    tolerance = 1e-6
  )

  # Scaled family: the scaled statistic in place of the standard one, in the
  # same formulas.
  chisq_s <- lav(fit, "chisq.scaled")
  base_s <- lav(fit, "baseline.chisq.scaled")
  expect_equal(x$families$scaled$values[["cfi"]], cfi(chisq_s, df, base_s, df_base),
    tolerance = 1e-10
  )
  expect_equal(x$families$scaled$values[["rmsea"]], rmsea(chisq_s, df), tolerance = 1e-10)
  expect_equal(x$families$scaled$values[["tli"]],
    1 - ((chisq_s - df) * df_base) / ((base_s - df_base) * df),
    tolerance = 1e-10
  )

  # Robust family (Brosseau-Liard, Savalei & Li, 2012; Brosseau-Liard &
  # Savalei, 2014): the standard statistic, with the degrees of freedom
  # multiplied by the scaling factor c = T / T_scaled.
  c_model <- chisq / chisq_s
  c_base <- base / base_s
  robust_cfi <- 1 - max(chisq - c_model * df, 0) /
    max(base - c_base * df_base, chisq - c_model * df, 0)
  robust_rmsea <- sqrt(max((chisq - c_model * df) / (n * df), 0))
  expect_equal(x$families$robust$values[["cfi"]], robust_cfi, tolerance = 1e-6)
  expect_equal(x$families$robust$values[["rmsea"]], robust_rmsea, tolerance = 1e-6)
  expect_equal(x$families$robust$values[["tli"]],
    1 - ((chisq - c_model * df) * df_base) / ((base - c_base * df_base) * df),
    tolerance = 1e-6
  )
  # The three are distinct numbers on this fit.
  three <- vapply(x$families, function(f) f$values[["cfi"]], numeric(1))
  expect_gt(min(abs(diff(sort(three)))), 5e-4)
})

# ---- The family shown for one model, and its label ---------------------------

test_that("one family is selected by a stated rule, and the value says which", {
  hs <- fam_hs()
  fit <- sieve_fit_cfa(hs$items, hs$meta, estimator = "mlr")
  m <- sieve_fit_measures(fit)
  # MLR: robust first. Every index in the vector is from that one family,
  # and the chi square is the scaled statistic.
  expect_identical(sieve_fit_family(m, "index"), "robust")
  expect_identical(sieve_fit_family(m, "test"), "scaled")
  expect_equal(unname(m[c("cfi", "tli", "rmsea", "rmsea_lo", "rmsea_hi")]),
    lav(fit, c(
      "cfi.robust", "tli.robust", "rmsea.robust",
      "rmsea.ci.lower.robust", "rmsea.ci.upper.robust"
    )),
    tolerance = 1e-12
  )
  expect_equal(unname(m[c("chisq", "df", "pvalue")]),
    lav(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled")),
    tolerance = 1e-12
  )
  expect_equal(unname(m[c("srmr", "aic", "bic", "npar")]), lav(fit, c("srmr_bentler_nomean", "aic", "bic", "npar")),
    tolerance = 1e-12
  )
  expect_identical(attr(m, "status"), "ok")
  expect_length(attr(m, "passed_over"), 0L)
  # The vector carries the names callers read.
  expect_identical(names(m), c(
    "npar", "chisq", "df", "pvalue", "cfi", "tli", "rmsea",
    "rmsea_lo", "rmsea_hi", "srmr", "aic", "bic"
  ))
  # Labels carry the family; measures without families carry none.
  expect_identical(
    sieve_fit_label(m, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea_lo", "srmr", "aic")),
    c(
      "Model chi square (scaled)", "df", "p-value (scaled)", "CFI (robust)", "TLI (robust)",
      "RMSEA (robust)", "RMSEA 90% CI lower (robust)", "SRMR", "AIC"
    )
  )
  note <- paste(sieve_fit_note(m), collapse = " ")
  expect_match(note, "robust family", fixed = TRUE)
  expect_match(note, "Brosseau-Liard", fixed = TRUE)
  expect_match(note, "Savalei", fixed = TRUE)
  expect_match(note, "scaled statistic", fixed = TRUE)
  expect_match(note, "yuan.bentler.mplus", fixed = TRUE)

  # A family asked for by name is returned as that family and labeled as
  # that family: scaled values are never handed back under the name robust.
  ms <- sieve_fit_measures(fit, family = "scaled")
  expect_identical(sieve_fit_family(ms), "scaled")
  expect_equal(unname(ms[c("cfi", "rmsea", "rmsea_lo", "rmsea_hi")]),
    lav(fit, c(
      "cfi.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled",
      "rmsea.ci.upper.scaled"
    )),
    tolerance = 1e-12
  )
  expect_identical(sieve_fit_label(ms, "cfi"), "CFI (scaled)")
  mst <- sieve_fit_measures(fit, family = "standard")
  expect_identical(sieve_fit_family(mst, "test"), "standard")
  expect_equal(unname(mst[c("chisq", "cfi", "rmsea")]), lav(fit, c("chisq", "cfi", "rmsea")),
    tolerance = 1e-12
  )
  expect_error(sieve_fit_measures(fit, family = "best"))

  # The side by side table holds all three, under their names.
  tab <- sieve_fit_family_table(m)
  expect_identical(names(tab), c("measure", "standard", "scaled", "robust"))
  expect_equal(tab$scaled[tab$measure == "CFI"], lav(fit, "cfi.scaled"), tolerance = 1e-12)
  expect_equal(tab$robust[tab$measure == "RMSEA"], lav(fit, "rmsea.robust"), tolerance = 1e-12)
  expect_true(is.na(tab$robust[tab$measure == "Model chi square"]))
  expect_true(any(grepl("^robust, Model chi square", attr(tab, "reasons"))))
})

test_that("an unavailable family is reported as unavailable, with the reason", {
  hs <- fam_hs()
  model <- sieve_cfa_plan(hs$meta)$model

  # Maximum likelihood with the standard test only, complete cases: lavaan
  # defines neither scaled nor robust indices. Not applicable, and said so.
  f_ml <- lavaan::cfa(model, data = hs$items, estimator = "ML", std.lv = TRUE)
  x <- sieve_fit_families(f_ml)
  expect_false(x$families$scaled$available)
  expect_false(x$families$robust$available)
  expect_match(x$families$scaled$reason, "Not applicable", fixed = TRUE)
  expect_match(x$families$scaled$reason, "fit without a scaled (corrected) test statistic", fixed = TRUE)
  expect_match(x$families$robust$reason, "Not applicable", fixed = TRUE)
  m <- sieve_fit_select(x)
  expect_identical(sieve_fit_family(m), "standard")
  expect_identical(sieve_fit_family(m, "test"), "standard")
  expect_setequal(names(attr(m, "passed_over")), c("robust", "scaled"))
  expect_equal(m[["cfi"]], lav(f_ml, "cfi"), tolerance = 1e-12)
  # A family that is not there cannot be had by asking for it.
  expect_true(is.na(sieve_fit_measures(f_ml, family = "robust")[["cfi"]]))

  # Maximum likelihood with FIML and the standard test: lavaan reports
  # robust indices (Zhang & Savalei, 2023) but no scaled ones.
  f_fiml <- lavaan::cfa(model, data = hs$items, estimator = "ML", missing = "fiml", std.lv = TRUE)
  xf <- sieve_fit_families(f_fiml)
  expect_true(xf$families$robust$available)
  expect_false(xf$families$scaled$available)
  mf <- sieve_fit_select(xf)
  expect_identical(sieve_fit_family(mf), "robust")
  expect_identical(sieve_fit_family(mf, "test"), "standard")
  expect_equal(mf[["chisq"]], lav(f_fiml, "chisq"), tolerance = 1e-12)
  expect_equal(mf[["cfi"]], lav(f_fiml, "cfi.robust"), tolerance = 1e-12)
  expect_match(xf$families$robust$missing[["chisq"]], "standard one", fixed = TRUE)

  # Ordered indicators, one group: thresholds are free, so the robust
  # indices (Savalei, 2021) describe the whole model and are shown.
  o <- fam_ordered_hs()
  f_ord <- sieve_fit_cfa(o$data[, o$fx$item_cols], o$meta)
  xo <- sieve_fit_families(f_ord)
  expect_true(xo$categorical)
  expect_identical(xo$families$robust$scope, "model")
  mo <- sieve_fit_select(xo)
  expect_identical(sieve_fit_family(mo), "robust")
  expect_equal(mo[["cfi"]], lav(f_ord, "cfi.robust"), tolerance = 1e-12)
  expect_equal(mo[["chisq"]], lav(f_ord, "chisq.scaled"), tolerance = 1e-12)
  expect_match(paste(sieve_fit_note(mo), collapse = " "), "Savalei, 2021", fixed = TRUE)
  # No likelihood, so no AIC or BIC: not applicable, with the reason.
  expect_false(xo$aic$available)
  expect_match(xo$aic$reason, "no likelihood", fixed = TRUE)
  expect_true(any(grepl("^AIC, BIC: Not applicable", attr(sieve_fit_family_table(mo), "reasons"))))
  # The unscaled statistic of this estimator has no p-value; the reason is kept.
  expect_true(is.na(xo$families$standard$values[["pvalue"]]))
  expect_match(xo$families$standard$missing[["pvalue"]], "no chi square reference", fixed = TRUE)

  # Ordered and continuous indicators together: lavaan returns NA for every
  # robust index. Not computed by the engine, with the engine's condition.
  mx <- fam_ordered_hs(which = hs$fx$theory_map[[1]])
  f_mix <- sieve_fit_cfa(mx$data[, mx$fx$item_cols], mx$meta)
  expect_true(all(is.na(lav(f_mix, c("cfi.robust", "rmsea.robust")))))
  xm <- sieve_fit_families(f_mix)
  expect_false(xm$families$robust$available)
  expect_match(xm$families$robust$reason, "mixes ordered and continuous", fixed = TRUE)
  mm <- sieve_fit_select(xm)
  expect_identical(sieve_fit_family(mm), "scaled")
  expect_equal(unname(mm[c("cfi", "rmsea")]), lav(f_mix, c("cfi.scaled", "rmsea.scaled")),
    tolerance = 1e-12
  )
  expect_identical(sieve_fit_label(mm, "cfi"), "CFI (scaled)")
  expect_identical(names(attr(mm, "passed_over")), "robust")
  expect_match(paste(sieve_fit_note(mm), collapse = " "),
    "The robust family is not shown: lavaan does not compute",
    fixed = TRUE
  )

  # A polychoric correlation matrix that is not positive definite (eight
  # two-valued items, 45 cases): lavaan returns NA for the robust indices.
  small <- withr::with_seed(1, {
    eta <- stats::rnorm(45)
    as.data.frame(sapply(rep(.8, 8), function(l) {
      as.integer(l * eta + sqrt(1 - l^2) * stats::rnorm(45) > 0.6)
    }))
  })
  names(small) <- paste0("y", 1:8)
  f_npd <- sieve_capture(lavaan::cfa(paste("f =~", paste(names(small), collapse = " + ")),
    data = small, ordered = names(small),
    estimator = "WLSMV", std.lv = TRUE
  ))$value
  expect_true(lavaan::lavInspect(f_npd, "converged"))
  ev <- eigen(lavaan::lavInspect(f_npd, "sampstat")$cov, only.values = TRUE)$values
  expect_lt(min(ev), 0)
  xn <- sieve_fit_families(f_npd)
  expect_false(xn$families$robust$available)
  expect_match(xn$families$robust$reason, "not positive definite", fixed = TRUE)
  expect_identical(sieve_fit_family(sieve_fit_select(xn)), "scaled")

  # Zero degrees of freedom: lavaan reports no scaled RMSEA. The family is
  # not complete, the reason is kept, and the rule moves to a family that is.
  f_zero <- lavaan::cfa(paste("f =~", paste(hs$fx$item_cols[1:3], collapse = " + ")),
    data = hs$items, estimator = "MLR", missing = "fiml", std.lv = TRUE
  )
  xz <- sieve_fit_families(f_zero)
  expect_true(is.na(xz$families$scaled$values[["rmsea"]]))
  expect_false(xz$families$scaled$complete)
  expect_match(xz$families$scaled$missing[["rmsea"]], "zero degrees of freedom", fixed = TRUE)
  expect_match(xz$families$standard$missing[["pvalue"]], "zero degrees of freedom", fixed = TRUE)
  expect_identical(sieve_primary_family(xz)$family, "robust")

  # No solution, no index, in any family (the status contract).
  two <- hs$meta
  two$status[!(two$item %in% hs$fx$item_cols[1:2])] <- "parked"
  f_two <- sieve_fit_cfa(hs$items, two)
  x2 <- sieve_fit_families(f_two)
  expect_identical(x2$status, "not_identified")
  expect_false(any(vapply(x2$families, function(f) f$available, logical(1))))
  expect_match(x2$families$robust$reason, "not identified", fixed = TRUE)
  m2 <- sieve_fit_measures(f_two)
  expect_true(is.na(sieve_fit_family(m2)))
  expect_equal(m2[["df"]], -1)
  expect_true(all(is.na(m2[c("chisq", "cfi", "tli", "rmsea", "srmr", "aic", "bic")])))
  f_few <- sieve_fit_cfa(hs$items[1:8, ], hs$meta)
  expect_identical(sieve_fit_families(f_few)$status, "not_converged")
  expect_match(sieve_fit_families(f_few)$families$standard$reason, "did not converge", fixed = TRUE)
  expect_null(sieve_fit_measures(f_few))
  expect_null(sieve_fit_measures(NULL))
  expect_identical(sieve_fit_families(NULL)$status, "failed")
})

# ---- What the robust indices of an ordered model describe ---------------------

test_that("robust indices of an ordered model leave out thresholds and intercepts, and are marked", {
  sim <- fam_ladder("sim")
  fits <- attr(sim$res$mi, "fits")
  expect_identical(names(fits), c("configural", "thresholds", "metric", "scalar", "strict"))
  # Configural: nothing is constrained across groups, so the robust indices
  # describe the whole model. Every later rung constrains thresholds.
  scope <- vapply(fits, sieve_robust_scope, character(1))
  expect_identical(unname(scope), c("model", rep("correlation_structure", 4L)))
  # The count behind the classification, from the parameter table directly.
  constrained <- vapply(fits, function(f) {
    pt <- lavaan::parTable(f)
    located <- pt$plabel[pt$op %in% c("|", "~1")]
    sum(pt$op == "==" & (pt$lhs %in% located | pt$rhs %in% located))
  }, numeric(1))
  expect_equal(unname(constrained[["configural"]]), 0)
  expect_true(all(constrained[-1L] == 18)) # 6 items x 3 thresholds
  # A continuous model, with any constraints, is described whole.
  cont <- attr(fam_ladder("continuous")$res$mi, "fits")
  expect_true(all(vapply(cont, sieve_robust_scope, character(1)) == "model"))

  # The population violates scalar invariance (every threshold of two items
  # is .5 higher in the second group) and nothing else. The scaled
  # difference test and the scaled indices register it at the scalar rung;
  # the robust indices do not move, although lavaan reports them at every
  # rung. This is why they are not used for the changes of an ordered ladder.
  lrt <- lavaan::lavTestLRT(fits$metric, fits$scalar)
  expect_lt(lrt[2L, "Pr(>Chisq)"], 1e-10)
  d_scaled <- lav(fits$scalar, "cfi.scaled") - lav(fits$metric, "cfi.scaled")
  d_robust <- lav(fits$scalar, "cfi.robust") - lav(fits$metric, "cfi.robust")
  expect_true(is.finite(d_robust))
  expect_lt(d_scaled, -0.03)
  expect_gt(d_robust, -0.001)
  expect_gt(lav(fits$scalar, "rmsea.scaled") - lav(fits$metric, "rmsea.scaled"), 0.03)
  expect_lt(abs(lav(fits$scalar, "rmsea.robust") - lav(fits$metric, "rmsea.robust")), 0.01)
  # The rungs before it hold in the population, and are not rejected.
  expect_gt(lavaan::lavTestLRT(fits$configural, fits$thresholds)[2L, "Pr(>Chisq)"], 0.01)
  expect_gt(lavaan::lavTestLRT(fits$thresholds, fits$metric)[2L, "Pr(>Chisq)"], 0.01)

  # So the comparison is made in the scaled family, and says why.
  cmp <- sim$res$compare
  expect_identical(cmp$family, "scaled")
  expect_false(cmp$common$eligible[["robust"]])
  expect_true(cmp$common$eligible[["scaled"]])
  expect_match(cmp$common$why_not[["robust"]], "leaves out thresholds, intercepts", fixed = TRUE)
  expect_match(cmp$common$reason, "The robust family is not used", fixed = TRUE)
  expect_equal(cmp$table$delta_cfi[4L], d_scaled, tolerance = 1e-12)
  # The robust values are still stored, by name, and marked rung by rung.
  expect_equal(cmp$by_family$robust$cfi, vapply(fits, lav, numeric(1), name = "cfi.robust"),
    tolerance = 1e-12, ignore_attr = TRUE
  )
  expect_identical(cmp$by_family$robust$scope, unname(scope))
  expect_true(is.na(cmp$by_family$robust$reason[1L]))
  expect_match(cmp$by_family$robust$reason[4L], "cannot register misfit", fixed = TRUE)

  # The same model on invariant data: no rung is rejected at the scalar step.
  inv <- fam_ordered_sim(0)
  res0 <- sieve_fit_invariance(inv$data, "g", inv$meta,
    levels = c("configural", "thresholds", "metric", "scalar")
  )
  expect_gt(res0$compare$table$delta_p[4L], 0.01)
  expect_gt(res0$compare$table$delta_cfi[4L], -0.01)
})

# ---- One family for a comparison ----------------------------------------------

test_that("a change is never a robust value minus a scaled one", {
  # Values of this kind: robust CFI .9048 at configural and none at
  # metric; scaled CFI .9588 and .9520. A fallback model by model would
  # report .9520 - .9048 = +.0472, an improvement that is an artifact. The
  # rule uses the family both models report: scaled, -.0068.
  cont <- attr(fam_ladder("continuous")$res$mi, "fits")
  base <- sieve_fit_families(cont$configural)
  set_values <- function(x, robust_cfi, scaled_cfi) {
    x$families$robust$values[["cfi"]] <- robust_cfi
    x$families$robust$complete <- is.finite(robust_cfi)
    if (!is.finite(robust_cfi)) {
      x$families$robust$reason <- "lavaan computed no value for this model."
    }
    x$families$scaled$values[["cfi"]] <- scaled_cfi
    x
  }
  measures <- list(
    configural = set_values(base, 0.9048, 0.9588),
    metric = set_values(base, NA_real_, 0.9520)
  )
  common <- sieve_common_family(measures)
  expect_identical(common$family, "scaled")
  expect_identical(unname(common$eligible), c(FALSE, TRUE, TRUE))
  expect_match(common$why_not[["robust"]], "not reported for 'metric'", fixed = TRUE)
  change <- measures$metric$families[[common$family]]$values[["cfi"]] -
    measures$configural$families[[common$family]]$values[["cfi"]]
  expect_equal(change, -0.0068, tolerance = 1e-12)
  expect_false(isTRUE(all.equal(change, 0.9520 - 0.9048)))
  # The per-model rule would have picked different families for the two.
  expect_identical(sieve_primary_family(measures$configural)$family, "robust")
  expect_identical(sieve_primary_family(measures$metric)$family, "scaled")

  # With the robust value present for both, robust is used for both.
  both <- list(
    configural = set_values(base, 0.9048, 0.9588),
    metric = set_values(base, 0.9000, 0.9520)
  )
  expect_identical(sieve_common_family(both)$family, "robust")
  # A preferred family is used only when it is eligible.
  expect_identical(sieve_common_family(both, prefer = "scaled")$family, "scaled")
  expect_identical(sieve_common_family(measures, prefer = "robust")$family, "scaled")
  # Nothing to compare when no model has a solution.
  none <- list(a = sieve_fit_families(NULL), b = sieve_fit_families(NULL))
  expect_true(is.na(sieve_common_family(none)$family))
  expect_match(sieve_common_family(none)$reason, "No model in the comparison has a solution",
    fixed = TRUE
  )
})

test_that("the continuous MLR ladder: one family, same-family changes, the engine's test", {
  lad <- fam_ladder("continuous")
  res <- lad$res
  fits <- attr(res$mi, "fits")
  cmp <- res$compare
  expect_s3_class(cmp, "sieve_fit_comparison")
  expect_identical(cmp$table$model, c("configural", "metric", "scalar", "strict"))
  expect_identical(cmp$family, "robust")
  expect_identical(cmp$test_family, "scaled")
  expect_true(all(cmp$common$eligible))

  # Every rung reports CFI and RMSEA in the common family, and they are
  # lavaan's values of that name on the rung's own fit.
  robust_cfi <- vapply(fits, lav, numeric(1), name = "cfi.robust")
  robust_rmsea <- vapply(fits, lav, numeric(1), name = "rmsea.robust")
  expect_true(all(is.finite(cmp$table$cfi)) && all(is.finite(cmp$table$rmsea)))
  expect_equal(cmp$table$cfi, unname(robust_cfi), tolerance = 1e-12)
  expect_equal(cmp$table$rmsea, unname(robust_rmsea), tolerance = 1e-12)
  expect_equal(cmp$table$chisq, unname(vapply(fits, lav, numeric(1), name = "chisq.scaled")),
    tolerance = 1e-12
  )
  # The changes are differences of those same-family values.
  expect_equal(cmp$table$delta_cfi, c(NA, diff(unname(robust_cfi))), tolerance = 1e-12)
  expect_equal(cmp$table$delta_rmsea, c(NA, diff(unname(robust_rmsea))), tolerance = 1e-12)
  # Each family's own changes are kept too, and are differences within it.
  for (family in c("standard", "scaled", "robust")) {
    suffix <- c(standard = "", scaled = ".scaled", robust = ".robust")[[family]]
    own <- vapply(fits, lav, numeric(1), name = paste0("cfi", suffix))
    expect_equal(cmp$by_family[[family]]$delta_cfi, c(NA, diff(unname(own))),
      tolerance = 1e-12, info = family
    )
  }
  # The scaled and robust changes are not the same numbers.
  expect_gt(max(abs(cmp$by_family$scaled$delta_cfi - cmp$by_family$robust$delta_cfi),
    na.rm = TRUE
  ), 1e-4)

  # The difference test of each rung is lavaan's own, called directly.
  for (i in 2:4) {
    ref <- lavaan::lavTestLRT(fits[[i - 1L]], fits[[i]])
    expect_equal(cmp$table$delta_chisq[i], ref[2L, "Chisq diff"], tolerance = 1e-12)
    expect_equal(cmp$table$delta_df[i], ref[2L, "Df diff"])
    expect_equal(cmp$table$delta_p[i], ref[2L, "Pr(>Chisq)"], tolerance = 1e-12)
  }
  expect_identical(
    cmp$test_label,
    "scaled chi square difference test (method = \"satorra.bentler.2001\")"
  )
  # Independent of lavTestLRT: the Satorra and Bentler (2001) formula applied
  # by hand to the standard statistics and their scaling factors.
  for (i in 2:4) {
    t1 <- lav(fits[[i - 1L]], "chisq")
    t0 <- lav(fits[[i]], "chisq")
    d1 <- lav(fits[[i - 1L]], "df")
    d0 <- lav(fits[[i]], "df")
    c1 <- lav(fits[[i - 1L]], "chisq.scaling.factor")
    c0 <- lav(fits[[i]], "chisq.scaling.factor")
    by_hand <- (t0 - t1) / ((d0 * c0 - d1 * c1) / (d0 - d1))
    expect_equal(cmp$table$delta_chisq[i], by_hand, tolerance = 1e-8)
    expect_equal(cmp$table$delta_p[i],
      stats::pchisq(by_hand, d0 - d1, lower.tail = FALSE),
      tolerance = 1e-8
    )
  }
  # It is not the difference of the two scaled statistics.
  raw <- diff(unname(vapply(fits, lav, numeric(1), name = "chisq.scaled")))
  expect_gt(max(abs(cmp$table$delta_chisq[-1L] - raw)), 1)
  expect_equal(cmp$table$delta_chisq[2L], 6.567307, tolerance = 1e-6)
  expect_equal(raw[1L], 4.257085, tolerance = 1e-6)

  # Interface equivalence: for continuous items DMAR's own table agrees.
  expect_equal(cmp$table$cfi, res$mi$cfi, tolerance = 1e-12)
  expect_equal(cmp$table$delta_cfi, res$mi$delta_cfi, tolerance = 1e-12)
  expect_equal(cmp$table$delta_chisq, res$mi$delta_chi_square, tolerance = 1e-12)
  expect_equal(cmp$table$delta_p, res$mi$p_value, tolerance = 1e-12)
  # Full precision.
  expect_false(isTRUE(all.equal(cmp$table$delta_cfi[-1L], round(cmp$table$delta_cfi[-1L], 3),
    tolerance = 1e-9
  )))
})

test_that("the ordered multiple group ladder: every rung in the common family", {
  lad <- fam_ladder("ordered")
  res <- lad$res
  fits <- attr(res$mi, "fits")
  cmp <- res$compare
  expect_identical(cmp$table$model, c("configural", "thresholds", "metric", "scalar", "strict"))
  expect_true(all(cmp$table$status == "ok"))
  # Robust is reported at every rung under this stack, but for the four
  # constrained rungs it describes the correlation structure only, so the
  # common family is scaled.
  expect_true(all(is.finite(vapply(fits, lav, numeric(1), name = "cfi.robust"))))
  expect_identical(cmp$family, "scaled")
  expect_identical(cmp$test_family, "scaled")
  expect_match(cmp$common$why_not[["robust"]],
    "'thresholds', 'metric', 'scalar', 'strict'",
    fixed = TRUE
  )

  scaled_cfi <- vapply(fits, lav, numeric(1), name = "cfi.scaled")
  scaled_rmsea <- vapply(fits, lav, numeric(1), name = "rmsea.scaled")
  expect_true(all(is.finite(cmp$table$cfi)) && all(is.finite(cmp$table$rmsea)))
  expect_equal(cmp$table$cfi, unname(scaled_cfi), tolerance = 1e-12)
  expect_equal(cmp$table$rmsea, unname(scaled_rmsea), tolerance = 1e-12)
  expect_equal(cmp$table$rmsea_lo,
    unname(vapply(fits, lav, numeric(1), name = "rmsea.ci.lower.scaled")),
    tolerance = 1e-12
  )
  expect_equal(cmp$table$delta_cfi, c(NA, diff(unname(scaled_cfi))), tolerance = 1e-12)
  expect_equal(cmp$table$delta_rmsea, c(NA, diff(unname(scaled_rmsea))), tolerance = 1e-12)
  expect_true(all(!nzchar(cmp$table$note)))

  # The difference tests are lavaan's (scaled and shifted, Satorra, 2000).
  for (i in 2:5) {
    ref <- lavaan::lavTestLRT(fits[[i - 1L]], fits[[i]])
    expect_equal(cmp$table$delta_chisq[i], ref[2L, "Chisq diff"], tolerance = 1e-10)
    expect_equal(cmp$table$delta_df[i], ref[2L, "Df diff"])
    expect_equal(cmp$table$delta_p[i], ref[2L, "Pr(>Chisq)"], tolerance = 1e-10)
  }
  expect_identical(
    cmp$test_label,
    "scaled chi square difference test (method = \"satorra.2000\")"
  )
  raw <- diff(unname(vapply(fits, lav, numeric(1), name = "chisq.scaled")))
  expect_gt(max(abs(cmp$table$delta_chisq[-1L] - raw)), 1)

  # DMAR's own table (res$mi) reports the robust indices for this ladder. At
  # the scalar rung its change in CFI is positive although the difference
  # test rejects the constraints; the scaled change, used here, is negative.
  scalar <- which(cmp$table$model == "scalar")
  expect_lt(cmp$table$delta_p[scalar], 1e-5)
  expect_gt(res$mi$delta_cfi[scalar], 0)
  expect_equal(res$mi$delta_cfi[scalar],
    lav(fits$scalar, "cfi.robust") - lav(fits$metric, "cfi.robust"),
    tolerance = 1e-12
  )
  expect_lt(cmp$table$delta_cfi[scalar], -0.02)
})

test_that("a ladder with ordered and continuous items reports its indices in the scaled family", {
  lad <- fam_ladder("mixed")
  res <- lad$res
  fits <- attr(res$mi, "fits")
  cmp <- res$compare
  # The missing-value pattern on the supported stack: lavaan returns no
  # robust index at any rung, and DMAR's own table reports the scaled
  # indices at every rung.
  expect_true(all(is.na(vapply(fits, lav, numeric(1), name = "cfi.robust"))))
  expect_equal(res$mi$cfi, unname(vapply(fits, lav, numeric(1), name = "cfi.scaled")), tolerance = 1e-12)
  # SIEVER reads the rungs: every rung has CFI and RMSEA, in the scaled
  # family, with the reason robust is not used.
  expect_identical(cmp$family, "scaled")
  expect_true(all(is.finite(cmp$table$cfi)) && all(is.finite(cmp$table$rmsea)))
  expect_equal(cmp$table$cfi, unname(vapply(fits, lav, numeric(1), name = "cfi.scaled")),
    tolerance = 1e-12
  )
  expect_equal(cmp$table$delta_rmsea,
    c(NA, diff(unname(vapply(fits, lav, numeric(1), name = "rmsea.scaled")))),
    tolerance = 1e-12
  )
  expect_match(cmp$common$why_not[["robust"]], "not reported for", fixed = TRUE)
  expect_match(cmp$common$why_not[["robust"]], "mixes ordered and continuous", fixed = TRUE)
  # The robust block says unavailable, with the reason, at every rung.
  expect_true(all(is.na(cmp$by_family$robust$cfi)))
  expect_true(all(grepl("mixes ordered and continuous", cmp$by_family$robust$reason)))
})

test_that("a rung without a solution has no indices, and nothing is compared with it", {
  lad <- fam_ladder("continuous")
  fits <- attr(lad$res$mi, "fits")
  status <- lad$res$status
  # The scalar rung is declared not converged (its fitted object is kept, so
  # that only the status decides what is reported).
  status$scalar$code <- "not_converged"
  status$scalar$message <- "The optimizer did not converge; the estimates are not a solution."
  cmp <- sieve_fit_compare(fits, status = status)
  row <- cmp$table[cmp$table$model == "scalar", ]
  expect_identical(row$status, "not_converged")
  expect_true(all(is.na(row[, c(
    "chisq", "pvalue", "cfi", "rmsea", "delta_chisq", "delta_p",
    "delta_cfi", "delta_rmsea"
  )])))
  expect_match(row$note, "No fit indices: The model did not converge", fixed = TRUE)
  # The rung after it is not compared with an earlier rung instead.
  after <- cmp$table[cmp$table$model == "strict", ]
  expect_true(is.finite(after$cfi))
  expect_true(all(is.na(after[, c("delta_chisq", "delta_p", "delta_cfi", "delta_rmsea")])))
  expect_match(after$note, "No change from 'scalar', which has no solution", fixed = TRUE)
  expect_match(cmp$tests$strict$reason, "that model has no solution", fixed = TRUE)
  # The rungs that do have solutions are untouched, and still share a family.
  expect_identical(cmp$family, "robust")
  expect_equal(cmp$table$delta_cfi[2L], lad$res$compare$table$delta_cfi[2L], tolerance = 1e-12)
  # A real fit that did not converge (eight cases) behaves the same way.
  hs <- fam_hs()
  f_few <- sieve_fit_cfa(hs$items[1:8, ], hs$meta)
  two <- sieve_fit_compare(list(first = fits$configural, second = f_few))
  expect_identical(two$table$status, c("ok", "not_converged"))
  expect_true(all(is.na(two$table$delta_cfi)))
  expect_false(two$tests$second$available)
  # A model with no fit at all.
  gone <- sieve_fit_compare(list(first = fits$configural, second = NULL))
  expect_identical(gone$table$status[2L], "failed")
  expect_match(gone$table$note[2L], "There is no fitted model", fixed = TRUE)
  # With nested = FALSE no test is run, and none is reported as missing.
  plain <- sieve_fit_compare(fits[1:2], nested = FALSE)
  expect_true(all(is.na(plain$table$delta_chisq)))
  expect_true(is.na(plain$test_label))
  expect_true(all(!nzchar(plain$table$note)))
  expect_equal(plain$table$delta_cfi[2L], lad$res$compare$table$delta_cfi[2L], tolerance = 1e-12)
})

test_that("a negative Satorra-Bentler scaling factor is handled, and said", {
  # Heavy tailed data, 60 cases per group: the Satorra and Bentler (2001)
  # scaling factor of the loading constraints is negative, so that statistic
  # is undefined. lavaan warns and returns no value.
  two_groups <- function(seed) {
    withr::with_seed(seed, {
      one <- function(n) {
        eta <- stats::rt(n, 3)
        x <- sapply(c(.8, .7, .6, .7, .5), function(l) l * eta + stats::rt(n, 3) * sqrt(1 - l^2))
        colnames(x) <- paste0("x", 1:5)
        as.data.frame(x)
      }
      rbind(cbind(one(60), g = "a"), cbind(one(60), g = "b"))
    })
  }
  fit_pair <- function(d) {
    model <- "f =~ x1 + x2 + x3 + x4 + x5"
    list(
      free = sieve_capture(lavaan::cfa(model, d,
        group = "g", estimator = "MLR",
        missing = "fiml", std.lv = TRUE
      ))$value,
      equal = sieve_capture(lavaan::cfa(model, d,
        group = "g", estimator = "MLR",
        missing = "fiml", std.lv = TRUE,
        group.equal = "loadings"
      ))$value
    )
  }
  pair <- fit_pair(two_groups(229))
  direct <- sieve_capture(lavaan::lavTestLRT(pair$free, pair$equal))
  expect_true(any(grepl("scaling factor is negative", direct$warnings, fixed = TRUE)))
  expect_true(is.na(direct$value[2L, "Chisq diff"]))
  # The strictly positive version (Satorra & Bentler, 2010) is computed
  # instead, labeled as such, and equals lavaan's when asked for directly.
  got <- sieve_nested_test(pair$free, pair$equal)
  ref <- suppressWarnings(lavaan::lavTestLRT(pair$free, pair$equal,
    method = "satorra.bentler.2010"
  ))
  expect_true(got$available)
  expect_identical(got$method, "satorra.bentler.2010")
  expect_match(got$label, "satorra.bentler.2010", fixed = TRUE)
  expect_equal(got$statistic, ref[2L, "Chisq diff"], tolerance = 1e-12)
  expect_equal(got$df, ref[2L, "Df diff"])
  expect_equal(got$p, ref[2L, "Pr(>Chisq)"], tolerance = 1e-12)
  expect_gt(got$statistic, 0)
  expect_match(got$note, "scaling factor of the Satorra and Bentler (2001)", fixed = TRUE)
  expect_true(any(grepl("scaling factor is negative", got$warnings, fixed = TRUE)))
  # The note reaches the comparison table.
  cmp <- sieve_fit_compare(list(configural = pair$free, metric = pair$equal))
  expect_equal(cmp$table$delta_chisq[2L], ref[2L, "Chisq diff"], tolerance = 1e-12)
  expect_match(cmp$table$note[2L], "Satorra & Bentler, 2010", fixed = TRUE)
  expect_true(any(grepl("method = \"satorra.bentler.2010\"", sieve_compare_code(cmp),
    fixed = TRUE
  )))

  # When the 2010 version cannot be computed either, the test is reported as
  # unavailable with the reason; no unscaled statistic is put in its place.
  hard <- fit_pair(two_groups(154))
  none <- sieve_nested_test(hard$free, hard$equal)
  expect_false(none$available)
  expect_true(is.na(none$statistic) && is.na(none$p))
  expect_match(none$reason, "could not be computed either", fixed = TRUE)

  # An ordinary pair uses lavaan's default and carries no note.
  fits <- attr(fam_ladder("continuous")$res$mi, "fits")
  plain <- sieve_nested_test(fits$configural, fits$metric)
  expect_identical(plain$method, "satorra.bentler.2001")
  expect_true(plain$scaled)
  expect_true(is.na(plain$note))
  # Models fit with the standard statistic get the standard test.
  hs <- fam_hs()
  d <- cbind(hs$items, school = hs$fx$data$school)
  m1 <- lavaan::cfa(sieve_cfa_plan(hs$meta)$model, d, group = "school", std.lv = TRUE)
  m0 <- lavaan::cfa(sieve_cfa_plan(hs$meta)$model, d,
    group = "school", std.lv = TRUE,
    group.equal = "loadings"
  )
  std <- sieve_nested_test(m1, m0)
  expect_identical(std$label, "standard chi square difference test")
  expect_false(std$scaled)
  expect_equal(std$statistic, lav(m0, "chisq") - lav(m1, "chisq"), tolerance = 1e-8)
  # The same model twice: no degrees of freedom, nothing to test.
  same <- sieve_nested_test(m1, m1)
  expect_false(same$available)
  expect_match(same$reason, "no degrees of freedom", fixed = TRUE)
  expect_false(sieve_nested_test(m1, NULL)$available)
})

test_that("the recorded R computes the table on screen from the fitted models", {
  replay <- function(code, fits_mi) {
    env <- new.env()
    env$fits_mi <- fits_mi
    out <- utils::capture.output(eval(parse(text = code), envir = env))
    list(env = env, printed = out)
  }
  # Continuous ladder: the script names the robust columns and the scaled
  # test, and running it gives the table's values and changes.
  lad <- fam_ladder("continuous")
  cmp <- lad$res$compare
  code <- sieve_compare_code(cmp)
  expect_true(any(grepl("cfi.robust", code, fixed = TRUE)))
  expect_true(any(grepl("chisq.scaled", code, fixed = TRUE)))
  expect_equal(sum(grepl("lavaan::lavTestLRT(", code, fixed = TRUE)), 3L)
  ran <- replay(code, attr(lad$res$mi, "fits"))
  expect_equal(unname(ran$env$rung_fit[, "cfi.robust"]), cmp$table$cfi, tolerance = 1e-12)
  expect_equal(unname(ran$env$rung_fit[, "rmsea.robust"]), cmp$table$rmsea, tolerance = 1e-12)
  expect_equal(unname(ran$env$rung_fit[, "chisq.scaled"]), cmp$table$chisq, tolerance = 1e-12)
  expect_equal(unname(diff(ran$env$rung_fit[, "cfi.robust"])), cmp$table$delta_cfi[-1L],
    tolerance = 1e-12
  )
  expect_true(any(grepl("satorra.bentler.2001", ran$printed, fixed = TRUE)))

  # Ordered ladder: the script names the scaled columns, the family on screen.
  ord <- fam_ladder("ordered")
  code_o <- sieve_compare_code(ord$res$compare)
  expect_true(any(grepl("\"cfi.scaled\", \"rmsea.scaled\"", code_o, fixed = TRUE)))
  expect_false(any(grepl("robust\"", code_o, fixed = TRUE)))
  ran_o <- replay(code_o, attr(ord$res$mi, "fits"))
  expect_equal(unname(ran_o$env$rung_fit[, "cfi.scaled"]), ord$res$compare$table$cfi,
    tolerance = 1e-12
  )

  # The module writes it to the reproducible code log with the DMAR call.
  o <- fam_ordered_hs()
  st <- fam_state(data = o$data, meta = o$meta)
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    log <- paste(state$codelog, collapse = "\n")
    expect_match(log, "DMAR::measurement_invariance(", fixed = TRUE)
    expect_match(log, "CFI and RMSEA in the scaled", fixed = TRUE)
    expect_match(log, "lavaan::fitMeasures(f, c(\"chisq.scaled\"", fixed = TRUE)
    expect_match(log, "lavaan::lavTestLRT(fits_mi[[\"metric\"]], fits_mi[[\"scalar\"]])",
      fixed = TRUE
    )
  })
  # No family, no script.
  expect_length(sieve_compare_code(sieve_fit_compare(list(a = NULL, b = NULL))), 0L)
})

# ---- The exploratory fit by number of factors ---------------------------------

test_that("the fit by number of factors is read from the unrotated solution", {
  # lavaan's robust CFI, TLI and RMSEA of a rotated efa object fit by FIML
  # depend on the rotation (lavaan 0.7-3; with complete data, listwise
  # deletion or WLSMV they do not); the model test, the scaled indices, SRMR
  # and BIC never do. The default is therefore "none", the rotation the
  # Dimensionality page and the EFA page pass; DMAR's fa_exploratory() reads
  # its fit measures from the unrotated fit for the same reason.
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  d[1:15, 1] <- NA_real_
  by_default <- sieve_efa_fit_by_k(d, "MLR", ks = 2:3, missing = "fiml")
  unrotated <- sieve_efa_fit_by_k(d, "MLR", rotation = "none", ks = 2:3, missing = "fiml")
  expect_identical(by_default, unrotated)
  expect_identical(attr(unrotated, "family"), "robust")
  rotated <- withr::with_seed(20261104,
    sieve_efa_fit_by_k(d, "MLR", rotation = "oblimin", ks = 2:3, missing = "fiml"))
  same <- c("k", "status", "chisq", "df", "pvalue", "SRMR", "BIC", "nobs")
  expect_equal(rotated[, same], unrotated[, same], tolerance = 1e-10)
  moved <- c("CFI", "TLI", "RMSEA", "RMSEA.hi")
  expect_true(all(abs(as.matrix(rotated[, moved]) - as.matrix(unrotated[, moved])) > 1e-4))
  # The measured case: three factors, robust RMSEA 0.048 unrotated and 0.068
  # under oblimin (lavaan 0.7-3).
  expect_equal(unrotated$RMSEA[2L], 0.0481, tolerance = 2e-3)
  expect_equal(rotated$RMSEA[2L], 0.0680, tolerance = 2e-3)
  # The scaled family of every solution does not move with the rotation.
  for (i in 1:2) {
    expect_equal(attr(rotated, "measures")[[i]]$families$scaled$values,
      attr(unrotated, "measures")[[i]]$families$scaled$values, tolerance = 1e-10)
  }
})

test_that("the fit by number of factors uses one family for every row", {
  skip_if_not_installed("callr")
  hs <- fam_hs()
  by_k <- sieve_efa_fit_by_k(hs$items, "MLR", NULL, rotation = "oblimin", max_k = 4L)
  expect_identical(attr(by_k, "family"), "robust")
  expect_match(attr(by_k, "family_reason"), "robust family for every model", fixed = TRUE)
  expect_length(attr(by_k, "measures"), 4L)
  for (k in 1:4) {
    ref <- lavaan::efa(data = hs$items, nfactors = k, rotation = "oblimin", estimator = "MLR")
    ref <- if (inherits(ref, "efaList")) ref[[1]] else ref
    expect_equal(c(by_k$CFI[k], by_k$TLI[k], by_k$RMSEA[k], by_k$RMSEA.lo[k], by_k$RMSEA.hi[k]),
      lav(ref, c(
        "cfi.robust", "tli.robust", "rmsea.robust", "rmsea.ci.lower.robust",
        "rmsea.ci.upper.robust"
      )),
      tolerance = 1e-8, info = paste("k =", k)
    )
    expect_equal(c(by_k$SRMR[k], by_k$BIC[k]), lav(ref, c("srmr_bentler_nomean", "bic")), tolerance = 1e-8)
    # Every family of every solution is kept by name.
    expect_equal(attr(by_k, "measures")[[k]]$families$scaled$values[["cfi"]],
      lav(ref, "cfi.scaled"),
      tolerance = 1e-8
    )
  }
  # The same family as the CFA panel shows for the same data: the two panels
  # agree about which statistic "CFI" is.
  expect_identical(
    attr(by_k, "family"),
    sieve_fit_family(sieve_fit_measures(sieve_fit_cfa(hs$items, hs$meta)))
  )

  # Ordered and continuous items together: no solution has a robust index,
  # so every row is scaled. Not robust for some rows and scaled for others.
  mx <- fam_ordered_hs(which = hs$fx$theory_map[[1]])
  spec <- sieve_lavaan_spec(mx$meta)
  mixed <- sieve_efa_fit_by_k(mx$data[, mx$fx$item_cols], spec$estimator, spec$ordered,
    rotation = "oblimin", max_k = 2L
  )
  expect_identical(attr(mixed, "family"), "scaled")
  ref <- lavaan::efa(
    data = mx$data[, mx$fx$item_cols], nfactors = 2, rotation = "oblimin",
    estimator = spec$estimator, ordered = spec$ordered
  )
  ref <- if (inherits(ref, "efaList")) ref[[1]] else ref
  expect_equal(mixed$CFI[2L], lav(ref, "cfi.scaled"), tolerance = 1e-8)
  expect_match(attr(mixed, "family_reason"), "mixes ordered and continuous", fixed = TRUE)

  # The EFA page's fit (DMAR's fit_measures of its own solution, from the
  # unrotated fit) names its version in DMAR's words beside the values: the
  # robust version after FIML, Zhang and Savalei's (2023) corrected indices.
  st <- fam_state()
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(n_factors = 3, rotation = "oblimin", suppress = 0.3, sort_loadings = TRUE, efa_go = 1)
    version <- sieve_efa_fit_version(efa()$fit_measures)
    expect_match(version[1L], "Version: robust", fixed = TRUE)
    line <- fam_text(output$efa_fit_line)
    expect_match(line, "Version: robust", fixed = TRUE)
    expect_match(line, "FIML corrected indices of Zhang and Savalei (2023)", fixed = TRUE)
  })
})

# ---- The interface names the family it shows -----------------------------------

test_that("value boxes, tables, and the report name the family they show", {
  hs <- fam_hs()
  # MLR, chosen: the subject is how the three families are named.
  reference <- sieve_fit_cfa(hs$items, hs$meta, estimator = "mlr")
  st <- fam_state(estimator_choice = "mlr")
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    session$setInputs(fit_go = 1)
    m <- fm()
    expect_identical(sieve_fit_family(m), "robust")
    # The boxes show the robust values, and say robust.
    expect_match(fam_text(output$vb_cfi), sprintf("%.3f", lav(reference, "cfi.robust")), fixed = TRUE)
    expect_match(fam_text(output$vb_cfi), "robust", fixed = TRUE)
    expect_match(fam_text(output$vb_tli), "robust", fixed = TRUE)
    expect_match(fam_text(output$vb_rmsea), sprintf("%.3f", lav(reference, "rmsea.robust")),
      fixed = TRUE
    )
    expect_match(fam_text(output$vb_rmsea),
      sprintf(
        "[%.3f, %.3f]", lav(reference, "rmsea.ci.lower.robust"),
        lav(reference, "rmsea.ci.upper.robust")
      ),
      fixed = TRUE
    )
    expect_match(fam_text(output$vb_rmsea), "robust", fixed = TRUE)
    expect_false(grepl("robust|scaled", fam_text(output$vb_srmr)))
    # The note says which family, why, and which statistic the chi square is.
    note <- fam_text(output$fit_family)
    expect_match(note, "robust family", fixed = TRUE)
    expect_match(note, "Brosseau-Liard", fixed = TRUE)
    expect_match(note, "scaled statistic", fixed = TRUE)
    # The table labels every row with its family (the rows are built by
    # sieve_fit_table(), which the panel renders).
    # The unbiased SRMR and its interval follow the SRMR.
    rows <- sieve_fit_table(m)
    expect_identical(rows$Statistic, c(
      "Free parameters", "Model chi square (scaled)", "df", "p-value (scaled)",
      "CFI (robust)", "TLI (robust)", "RMSEA (robust)", "RMSEA 90% CI lower (robust)",
      "RMSEA 90% CI upper (robust)", "SRMR", "Unbiased SRMR", "Unbiased SRMR 90% CI lower",
      "Unbiased SRMR 90% CI upper", "AIC", "BIC"
    ))
    expect_equal(rows$Value[rows$Statistic == "CFI (robust)"], lav(reference, "cfi.robust"),
      tolerance = 1e-12
    )
    expect_equal(rows$Value[rows$Statistic == "Model chi square (scaled)"],
      lav(reference, "chisq.scaled"),
      tolerance = 1e-12
    )
    expect_no_error(output$fit_tbl)
    # All three families side by side.
    side <- fam_text(output$fit_fam_tbl)
    expect_match(side, "Standard", fixed = TRUE)
    expect_match(side, "Scaled", fixed = TRUE)
    expect_match(side, "Robust", fixed = TRUE)
    expect_match(fam_text(output$fit_fam_why), "no test statistic of its own", fixed = TRUE)
  })

  # Ordered and continuous items: the boxes fall back to scaled and say so.
  mx <- fam_ordered_hs(which = hs$fx$theory_map[[1]])
  f_mix <- sieve_fit_cfa(mx$data[, mx$fx$item_cols], mx$meta)
  st <- fam_state(data = mx$data, meta = mx$meta)
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    session$setInputs(fit_go = 1)
    expect_match(fam_text(output$vb_cfi), sprintf("%.3f", lav(f_mix, "cfi.scaled")), fixed = TRUE)
    expect_match(fam_text(output$vb_cfi), "scaled", fixed = TRUE)
    expect_false(grepl("robust", fam_text(output$vb_cfi), fixed = TRUE))
    expect_true("CFI (scaled)" %in% sieve_fit_table(fm())$Statistic)
    expect_match(fam_text(output$fit_family), "The robust family is not shown", fixed = TRUE)
    expect_match(fam_text(output$fit_fam_why), "mixes ordered and continuous", fixed = TRUE)
  })

  # SEM tab: the two boxes carry the family.
  st <- fam_state()
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(
      outcomes = "Visual", predictors = "Textual", layout = "tree2",
      min_path = 0
    )
    session$setInputs(fit_go = 1)
    f <- fit()
    expect_s4_class(f, "lavaan")
    expect_match(fam_text(output$vb_cfi), sprintf("%.3f", lav(f, "cfi.robust")), fixed = TRUE)
    expect_match(fam_text(output$vb_cfi), "robust", fixed = TRUE)
    expect_match(fam_text(output$vb_rmsea), sprintf("%.3f", lav(f, "rmsea.robust")), fixed = TRUE)
    expect_match(fam_text(output$vb_rmsea), "robust", fixed = TRUE)
    expect_match(fam_text(output$fit_family), "robust family", fixed = TRUE)
  })

  # The methods draft names the family of every index it prints.
  draft <- sieve_methods_draft(hs$items, hs$meta, "the bundled example", estimator = "mlr")
  expect_match(draft, sprintf("robust CFI = %.3f", lav(reference, "cfi.robust")), fixed = TRUE)
  expect_match(draft, sprintf("robust RMSEA = %.3f", lav(reference, "rmsea.robust")), fixed = TRUE)
  expect_match(draft, sprintf("robust TLI = %.3f", lav(reference, "tli.robust")), fixed = TRUE)
  expect_match(draft, sprintf("scaled chi square(24) = %.2f", lav(reference, "chisq.scaled")),
    fixed = TRUE
  )
  expect_match(draft, "Yuan-Bentler scaled test statistic", fixed = TRUE)
  expect_false(grepl("Satorra-Bentler", draft, fixed = TRUE))
  draft_mix <- sieve_methods_draft(mx$data[, mx$fx$item_cols], mx$meta, "the mixed example")
  expect_match(draft_mix, sprintf("scaled CFI = %.3f", lav(f_mix, "cfi.scaled")), fixed = TRUE)
  expect_match(draft_mix, "The robust family is not shown", fixed = TRUE)

  # Other panels that print a fit index name its family as well.
  st <- fam_state()
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(
      coef = "omega", factor_cov = "correlated",
      load_low = .4, salient = .3, communality_low = .3,
      item_total_low = .3, redundancy_high = .8
    )
    session$elapse(400)
    txt <- fam_text(output$overall_fit)
    expect_match(txt, "RMSEA (robust) = ", fixed = TRUE)
    expect_match(txt, "CFI (robust) = ", fixed = TRUE)
  })
})

test_that("the invariance panel states its family, changes within it, and draws no verdict", {
  # Ordered items: the table is in the scaled family at every rung.
  o <- fam_ordered_hs()
  st <- fam_state(data = o$data, meta = o$meta)
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_null(r$error)
    expect_identical(r$family, "scaled")
    expect_identical(r$fit_indices, "scaled")
    fits <- r$fits
    scaled_cfi <- unname(vapply(fits, lav, numeric(1), name = "cfi.scaled"))
    scaled_rmsea <- unname(vapply(fits, lav, numeric(1), name = "rmsea.scaled"))
    expect_equal(r$table$cfi, scaled_cfi, tolerance = 1e-12)
    expect_equal(r$table$rmsea, scaled_rmsea, tolerance = 1e-12)
    expect_equal(r$table$delta_cfi, c(NA, diff(scaled_cfi)), tolerance = 1e-12)
    expect_equal(r$table$delta_rmsea, c(NA, diff(scaled_rmsea)), tolerance = 1e-12)
    ref <- lavaan::lavTestLRT(fits$metric, fits$scalar)
    expect_equal(r$table$delta_chi_square[4L], ref[2L, "Chisq diff"], tolerance = 1e-10)
    expect_equal(r$table$p_value[4L], ref[2L, "Pr(>Chisq)"], tolerance = 1e-10)
    # The column headers name the family.
    tbl <- fam_text(output$inv_tbl)
    for (header in c(
      "CFI (scaled)", "RMSEA (scaled)", "delta CFI (scaled)",
      "delta RMSEA (scaled)", "chi square (scaled)"
    )) {
      expect_match(tbl, header, fixed = TRUE)
    }
    engine <- fam_text(output$inv_head)
    expect_match(engine, "scaled family for every model", fixed = TRUE)
    expect_match(engine, "The robust family is not used", fixed = TRUE)
    expect_match(engine, "satorra.2000", fixed = TRUE)
    expect_match(engine, "never by subtracting one scaled chi square from another", fixed = TRUE)
    # The conventions are stated as conventions, with their conditions and
    # the family of the displayed changes; the references are kept.
    conv <- fam_text(output$inv_convention)
    expect_match(conv, "Cheung & Rensvold, 2002", fixed = TRUE)
    expect_match(conv, "Chen, 2007", fixed = TRUE)
    expect_match(conv, "conventions", fixed = TRUE)
    expect_match(conv, "maximum likelihood, continuous", fixed = TRUE)
    expect_match(conv, "scaled family", fixed = TRUE)
    expect_match(conv, "draws no verdict", fixed = TRUE)
    # Every family at every rung, with the robust rows marked.
    fam <- fam_text(output$inv_families)
    expect_match(fam, "the table above uses the scaled family", fixed = TRUE)
    expect_match(fam, "leaves out thresholds and intercepts", fixed = TRUE)
    expect_match(fam, sprintf("%.3f", lav(fits$configural, "cfi.robust")), fixed = TRUE)

    # Partial invariance: the pair is compared in the ladder's family, the
    # test is lavaan's, and the panel gives no verdict. The reference is the
    # rung before it WITH the freed parameter free (the model the partial
    # model is nested in), not the full metric rung, which is not nested
    # with a partial scalar model that frees a loading.
    session$setInputs(inv_level = "scalar")
    session$setInputs(inv_diag = 1)
    dg <- inv_diag()
    session$setInputs(inv_diag_tbl_rows_selected = 1)
    session$setInputs(inv_refit = 1)
    p_res <- inv_partial()
    expect_null(p_res$error)
    expect_identical(p_res$family, "scaled")
    expect_equal(p_res$cfi, lav(p_res$fit, "cfi.scaled"), tolerance = 1e-12)
    # Ordered partial models use Wu-Estabrook identification with theta.
    # DMAR's default delta models are a different parameterization, so the
    # oracle writes the intended restrictions and fits them through lavaan
    # outside the panel. The syntax writer itself is checked against
    # hand-written Wu-Estabrook syntax in test-without-semtools.R.
    restrictions <- list(configural = character(), thresholds = "thresholds",
      metric = c("thresholds", "loadings"), scalar = c("thresholds", "loadings", "intercepts"))
    template <- lavaan::cfa(sieve_cfa_spec(o$meta, reserved = names(r$data))$model,
      data = r$data, group = "school", ordered = o$fx$item_cols,
      parameterization = "theta", std.lv = TRUE, do.fit = FALSE)
    direct <- lapply(names(restrictions), function(rung) {
      syntax <- sieve_measurement_syntax(template,
        group_equal = restrictions[[rung]], group_partial = p_res$freed)
      lavaan::cfa(syntax, data = r$data, group = "school",
        ordered = o$fx$item_cols, estimator = "WLSMV", missing = "listwise", parameterization = "theta")
    })
    names(direct) <- names(restrictions)
    expect_equal(lav(p_res$prev_fit, "chisq"), lav(direct$metric, "chisq"), tolerance = 1e-8)
    expect_equal(lav(p_res$fit, "chisq"), lav(direct$scalar, "chisq"), tolerance = 1e-8)
    expect_equal(p_res$dcfi, lav(direct$scalar, "cfi.scaled") - lav(direct$metric, "cfi.scaled"),
      tolerance = 1e-8
    )
    expect_equal(p_res$drmsea,
      lav(direct$scalar, "rmsea.scaled") - lav(direct$metric, "rmsea.scaled"),
      tolerance = 1e-8
    )
    ref <- lavaan::lavTestLRT(direct$metric, direct$scalar)
    expect_equal(p_res$lrt_chisq, ref[2L, "Chisq diff"], tolerance = 1e-8)
    expect_equal(p_res$lrt_df, ref[2L, "Df diff"])
    expect_equal(p_res$lrt_p, ref[2L, "Pr(>Chisq)"], tolerance = 1e-8)
    # A constraint held at the metric rung (a loading or a threshold) makes
    # the reference a partial model too; an intercept does not.
    freed_earlier <- dg$table$type[1L] %in% c("loading", "threshold")
    expect_identical(isTRUE(p_res$reference_partial), freed_earlier)
    expect_identical(lav(p_res$prev_fit, "df") != lav(fits$metric, "df"), freed_earlier)
    out <- fam_text(output$inv_partial_out)
    expect_match(out, "CFI (scaled)", fixed = TRUE)
    expect_match(out, "RMSEA (scaled)", fixed = TRUE)
    expect_match(out, "satorra.2000", fixed = TRUE)
    expect_match(out, "conventions", fixed = TRUE)
    expect_false(grepl("alert-success", out, fixed = TRUE))
    expect_false(grepl("is tenable|cutoffs are met|cutoffs are still exceeded", out))
  })

  # Continuous items under MLR: the robust family, and the partial
  # comparison's scaled test equals lavaan's called directly.
  st <- fam_state(estimator_choice = "mlr")
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_identical(r$family, "robust")
    expect_match(fam_text(output$inv_tbl), "delta CFI (robust)", fixed = TRUE)
    expect_match(fam_text(output$inv_convention), "robust family", fixed = TRUE)
    session$setInputs(inv_level = "scalar")
    session$setInputs(inv_diag = 1)
    session$setInputs(inv_diag_tbl_rows_selected = 1)
    session$setInputs(inv_refit = 1)
    p_res <- inv_partial()
    expect_identical(p_res$family, "robust")
    # The largest score test at the scalar rung of the nine tests is an
    # intercept. An intercept is not constrained at the metric rung, so the
    # reference (the metric rung of the refit) is the ladder's own metric
    # model, and the comparison with either is the same nested test.
    expect_identical(inv_diag()$table$type[1L], "intercept")
    expect_false(p_res$reference_partial)
    expect_equal(lav(p_res$prev_fit, "chisq"), lav(r$fits$metric, "chisq"), tolerance = 1e-10)
    ref <- lavaan::lavTestLRT(r$fits$metric, p_res$fit)
    expect_equal(p_res$lrt_chisq, ref[2L, "Chisq diff"], tolerance = 1e-12)
    expect_equal(p_res$lrt_df, ref[2L, "Df diff"])
    expect_equal(p_res$lrt_p, ref[2L, "Pr(>Chisq)"], tolerance = 1e-12)
    expect_equal(p_res$dcfi, lav(p_res$fit, "cfi.robust") - lav(r$fits$metric, "cfi.robust"),
      tolerance = 1e-12
    )
    expect_match(fam_text(output$inv_partial_out), "so it is the ladder's own", fixed = TRUE)
    out <- fam_text(output$inv_partial_out)
    expect_match(out, "CFI (robust)", fixed = TRUE)
    expect_match(out, "satorra.bentler.2001", fixed = TRUE)
    expect_false(grepl("alert-success|alert-warning", out))
    expect_false(grepl("is tenable|cutoffs are met", out))

    # The calibration and validation halves are shown in one family.
    session$setInputs(holdout = 50, seed = 20261012)
    session$setInputs(split = 1)
    cv <- cv_result()
    expect_identical(cv$family, "robust")
    expect_true(all(is.finite(cv$table$CFI)))
    expect_match(fam_text(output$cv_tbl), "CFI (robust)", fixed = TRUE)
    expect_match(fam_text(output$cv_msg), "robust family for every model", fixed = TRUE)
  })
})

test_that("the method-factor comparison is made within one family", {
  hs <- fam_hs()
  substantive <- sieve_fit_cfa(hs$items, hs$meta)
  with_method <- sieve_run_method_factor(hs$items, hs$meta)$fit
  skip_if(
    is.null(with_method) || !sieve_has_solution(with_method),
    "the method-factor model has no solution on this fixture"
  )
  got <- sieve_method_factor_table(substantive, with_method)
  fam <- got$family
  expect_true(fam %in% c("robust", "scaled", "standard"))
  suffix <- c(standard = "", scaled = ".scaled", robust = ".robust")[[fam]]
  value <- function(label) got$table$Value[got$table$Statistic == label]
  # Both models in one family, named in every label, and the change is the
  # difference of the two values of that family on the fitted models.
  expect_equal(value(sprintf("Substantive CFI (%s)", fam)),
    lav(substantive, paste0("cfi", suffix)),
    tolerance = 1e-12
  )
  expect_equal(value(sprintf("With method factor CFI (%s)", fam)),
    lav(with_method, paste0("cfi", suffix)),
    tolerance = 1e-12
  )
  expect_equal(value(sprintf("Change in CFI (%s)", fam)),
    lav(with_method, paste0("cfi", suffix)) - lav(substantive, paste0("cfi", suffix)),
    tolerance = 1e-12
  )
  expect_equal(value(sprintf("Change in RMSEA (%s)", fam)),
    lav(with_method, paste0("rmsea", suffix)) -
      lav(substantive, paste0("rmsea", suffix)),
    tolerance = 1e-12
  )
  # The method loadings, from the standardized solution directly.
  ss <- lavaan::standardizedSolution(with_method)
  loading <- ss$est.std[ss$op == "=~" & ss$lhs == attr(with_method, "method_factor")]
  expect_equal(value("Average |method loading|"), mean(abs(loading)), tolerance = 1e-12)
  expect_equal(value("Variance from method (mean squared loading)"), mean(loading^2),
    tolerance = 1e-12
  )
  # No table, and the reason, when one of the models has no solution.
  few <- sieve_fit_cfa(hs$items[1:8, ], hs$meta)
  none <- sieve_method_factor_table(few, with_method)
  expect_null(none$table)
  # The panel renders it.
  st <- fam_state()
  shiny::testServer(mod_validity_server, args = list(state = st), {
    expect_no_error(output$ulmf_tbl)
  })
})

test_that("no interface text calls a scaled index robust or turns a convention into a rule", {
  files <- c(
    "mod_cfa.R", "mod_sem.R", "mod_efa.R", "mod_finalize.R", "mod_irt.R",
    "mod_validity.R", "mod_bifactor.R", "mod_winnow.R", "fct_report.R",
    "fct_models.R", "fct_fit.R"
  )
  root <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(root))
  text <- vapply(files, function(f) {
    paste(readLines(file.path(root, f), warn = FALSE), collapse = " ")
  }, character(1))
  squashed <- gsub("\\s+", " ", gsub("\"\\s*,\\s*\"", "", text))
  expect_false(any(grepl("Robust (scaled)", squashed, fixed = TRUE)))
  expect_false(any(grepl("robust (scaled)", squashed, fixed = TRUE)))
  # No rule text, and no verdict computed from the cutoffs.
  expect_false(any(grepl("dCFI <= .01", squashed, fixed = TRUE)))
  expect_false(any(grepl("partial invariance is tenable", squashed, fixed = TRUE)))
  expect_false(any(grepl("<= 0.015", squashed, fixed = TRUE)))
  # The references are kept, with the statement that they are conventions.
  expect_match(squashed[["mod_finalize.R"]], "Cheung & Rensvold, 2002", fixed = TRUE)
  expect_match(squashed[["mod_finalize.R"]], "Chen, 2007", fixed = TRUE)
  expect_match(squashed[["mod_finalize.R"]], "conventions", fixed = TRUE)
  convention <- sieve_invariance_convention("robust")
  expect_match(convention, "Cheung & Rensvold, 2002", fixed = TRUE)
  expect_match(convention, "Chen, 2007", fixed = TRUE)
  expect_match(convention, "robust family", fixed = TRUE)
  expect_match(sieve_invariance_convention("standard"), "standard family", fixed = TRUE)
  expect_false(grepl("family\\.", sieve_invariance_convention()))
  # The rendered panel text.
  ui <- fam_text(as.character(mod_finalize_ui("x")))
  expect_match(ui, "Cheung & Rensvold, 2002", fixed = TRUE)
  expect_match(ui, "conventions", fixed = TRUE)
  expect_false(grepl("dCFI", ui, fixed = TRUE))
})

# ---- Nesting, and one family across a set ------------------------------------
#
# Partial invariance data: two groups of 500, six continuous items
# on one factor, the loading of m2 lower by .45 and the intercept of m5
# higher by .30 in the second group, whose factor mean is .2 (seed 101).
fam_partial_data <- function(seed = 101L, n = 500L) {
  one <- function(lam, nu, mu) {
    f <- stats::rnorm(n, mu)
    as.data.frame(sapply(1:6, function(j) nu[j] + lam[j] * f + stats::rnorm(n, sd = 0.6)))
  }
  lam <- c(.8, .8, .7, .7, .75, .75)
  d <- withr::with_seed(seed, {
    a <- one(lam, rep(0, 6), 0)
    lam2 <- lam
    lam2[2] <- lam[2] - 0.45
    nu2 <- rep(0, 6)
    nu2[5] <- 0.30
    rbind(a, one(lam2, nu2, 0.2))
  })
  names(d) <- paste0("m", 1:6)
  d$grp <- rep(c("a", "b"), each = n)
  d
}

fam_partial_state <- function(d, estimator_choice = "mlr") {
  fixture <- list(data = d, item_cols = paste0("m", 1:6), theory_map = NULL,
    name = "partial", labels = NULL, load_code = character())
  siever_test_study(fixture, labels = FALSE, theory = FALSE, configure = function(s) {
    # The oracle below is MLR's (the Satorra and Bentler, 2001, statistic).
    s$estimator_choice <- estimator_choice
    s$meta <- fam_meta(d[paste0("m", 1:6)], "F")
    s$roles <- NULL
  })
}

test_that("a partial invariance model is compared with the rung it is nested in", {
  d <- fam_partial_data()
  # THE ORACLE, fit with lavaan directly on hand-written syntax: the metric
  # and the scalar model, each with the loading of m2 free across groups.
  # These two are nested; the scalar one adds six intercept constraints and
  # frees one factor mean, so the test has 5 degrees of freedom.
  syntax <- "SAFE =~ m1 + m2 + m3 + m4 + m5 + m6"
  direct <- function(equal) {
    lavaan::cfa(syntax,
      data = d, group = "grp", estimator = "MLR", missing = "fiml",
      std.lv = TRUE, group.equal = equal, group.partial = "SAFE=~m2"
    )
  }
  partial_metric <- direct("loadings")
  partial_scalar <- direct(c("loadings", "intercepts"))
  nested <- lavaan::lavTestLRT(partial_metric, partial_scalar)
  expect_equal(nested[2L, "Df diff"], 5)
  # The Satorra and Bentler (2001) statistic by hand, from the standard
  # statistics and scaling factors of the two models.
  t0 <- lav(partial_scalar, "chisq")
  t1 <- lav(partial_metric, "chisq")
  d0 <- lav(partial_scalar, "df")
  d1 <- lav(partial_metric, "df")
  c0 <- lav(partial_scalar, "chisq.scaling.factor")
  c1 <- lav(partial_metric, "chisq.scaling.factor")
  by_hand <- (t0 - t1) / ((d0 * c0 - d1 * c1) / (d0 - d1))
  expect_equal(nested[2L, "Chisq diff"], by_hand, tolerance = 1e-8)
  # The misfit is real: the intercept of m5 differs by construction.
  expect_gt(by_hand, 40)

  st <- fam_partial_state(d)
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "grp")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_null(r$error)

    # The ordinary sequence: the scalar rung is diagnosed and the loading of
    # m2, which leads its table, is freed.
    session$setInputs(inv_level = "scalar")
    session$setInputs(inv_diag = 1)
    dg <- inv_diag()
    row <- which(dg$table$type == "loading" & dg$table$rhs == "m2")[1L]
    expect_false(is.na(row))
    session$setInputs(inv_diag_tbl_rows_selected = row)
    session$setInputs(inv_refit = 1)
    p_res <- inv_partial()
    expect_null(p_res$error)
    expect_identical(p_res$prev, "metric")
    expect_true(p_res$reference_partial)

    # The comparison shown is the nested one, equal to the oracle.
    expect_equal(lav(p_res$fit, "chisq"), t0, tolerance = 1e-6)
    expect_equal(lav(p_res$prev_fit, "chisq"), t1, tolerance = 1e-6)
    expect_equal(p_res$lrt_df, 5)
    expect_equal(p_res$lrt_chisq, by_hand, tolerance = 1e-6)
    expect_equal(p_res$lrt_p, stats::pchisq(by_hand, 5, lower.tail = FALSE), tolerance = 1e-6)
    expect_lt(p_res$lrt_p, 1e-8)
    expect_equal(p_res$dcfi, lav(partial_scalar, "cfi.robust") - lav(partial_metric, "cfi.robust"),
      tolerance = 1e-6
    )
    expect_equal(p_res$drmsea,
      lav(partial_scalar, "rmsea.robust") - lav(partial_metric, "rmsea.robust"),
      tolerance = 1e-6
    )
    # The added constraints cost fit: CFI falls and RMSEA rises.
    expect_lt(p_res$dcfi, 0)
    expect_gt(p_res$drmsea, 0)

    # The FULL metric rung, which still holds the m2 loading equal, is not
    # the reference: it fits worse than the partial scalar model that is
    # supposed to be nested in it, so the two are not nested, and comparing
    # them gives a negative difference with p = 1.
    expect_gt(lav(r$fits$metric, "chisq"), lav(p_res$fit, "chisq"))
    old <- suppressWarnings(lavaan::lavTestLRT(r$fits$metric, p_res$fit))
    expect_lt(old[2L, "Chisq diff"], 0)
    expect_equal(old[2L, "Pr(>Chisq)"], 1)
    # Such a pair gets no statistic and no p-value from the nested test, the
    # reason is stated, and lavaan's own warning is kept.
    not_nested <- sieve_nested_test(r$fits$metric, p_res$fit)
    expect_false(not_nested$available)
    expect_false(not_nested$nested)
    expect_true(is.na(not_nested$statistic))
    expect_true(is.na(not_nested$p))
    expect_lt(not_nested$standard_difference, 0)
    expect_match(not_nested$reason, "fits better than the less restricted one", fixed = TRUE)
    expect_true(any(grepl("fit better than less restricted", gsub("\\s+", " ", not_nested$warnings))))
    cmp_old <- sieve_fit_compare(list(metric = r$fits$metric, "partial scalar" = p_res$fit))
    expect_true(is.na(cmp_old$table$delta_chisq[2L]))
    expect_true(is.na(cmp_old$table$delta_p[2L]))
    expect_match(cmp_old$table$note[2L], "not the cost of the added constraints", fixed = TRUE)
    expect_true(any(grepl("fit better than less restricted", gsub("\\s+", " ", cmp_old$warnings))))
    # Handed in the wrong order, a pair is refused as well.
    reversed <- sieve_nested_test(p_res$fit, p_res$prev_fit)
    expect_false(reversed$available)
    expect_false(reversed$nested)
    expect_match(reversed$reason, "fewer degrees of freedom", fixed = TRUE)

    # The panel says which model the partial model is read against, shows
    # the nested test, and never a p-value of one for these data.
    out <- fam_text(output$inv_partial_out)
    expect_match(out, "metric model with the same parameters free", fixed = TRUE)
    expect_match(out, sprintf("chi square difference %.3f on 5 df", by_hand), fixed = TRUE)
    expect_false(grepl("p = 1.000", out, fixed = TRUE))
    expect_false(grepl("chi square difference -", out, fixed = TRUE))

    # The recorded R builds the pair from the refit, and reproduces the test.
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log,
      "pair <- list(\"partial metric\" = fits_mi_partial[[\"metric\"]], \"partial scalar\" = fits_mi_partial[[\"scalar\"]])",
      fixed = TRUE
    )
    env <- new.env()
    env$pair <- list("partial metric" = p_res$prev_fit, "partial scalar" = p_res$fit)
    code <- sieve_compare_code(p_res$compare, fits = "pair")
    replay <- eval(parse(text = code[length(code)]), env)
    expect_equal(replay[2L, "Chisq diff"], by_hand, tolerance = 1e-6)

    # An intercept freed at the scalar rung leaves the metric rung as it
    # was: the reference is then the ladder's own metric model.
    row5 <- which(dg$table$type == "intercept" & dg$table$lhs == "m5")[1L]
    session$setInputs(inv_diag_tbl_rows_selected = row5)
    session$setInputs(inv_refit = 2)
    p_int <- inv_partial()
    expect_false(p_int$reference_partial)
    expect_equal(lav(p_int$prev_fit, "chisq"), lav(r$fits$metric, "chisq"), tolerance = 1e-10)
    ref <- lavaan::lavTestLRT(r$fits$metric, p_int$fit)
    expect_equal(p_int$lrt_chisq, ref[2L, "Chisq diff"], tolerance = 1e-10)
    expect_equal(p_int$lrt_df, 4)
    expect_match(fam_text(output$inv_partial_out), "so it is the ladder's own", fixed = TRUE)

    # Both noninvariant parameters freed: nothing is left to reject.
    session$setInputs(inv_diag_tbl_rows_selected = c(row, row5))
    session$setInputs(inv_refit = 3)
    p_both <- inv_partial()
    expect_true(p_both$reference_partial)
    expect_equal(p_both$lrt_df, 4)
    expect_gt(p_both$lrt_p, .01)
    expect_gt(p_both$lrt_chisq, 0)

    # A loading freed at the metric rung is compared with the configural
    # model, where nothing is constrained.
    session$setInputs(inv_level = "metric")
    session$setInputs(inv_diag = 2)
    dm <- inv_diag()
    session$setInputs(inv_diag_tbl_rows_selected = which(dm$table$rhs == "m2")[1L])
    session$setInputs(inv_refit = 4)
    p_met <- inv_partial()
    expect_identical(p_met$prev, "configural")
    expect_false(p_met$reference_partial)
    expect_equal(lav(p_met$fit, "chisq"), t1, tolerance = 1e-6)
    expect_equal(p_met$lrt_df, 4)
  })
})

test_that("the nested reference is used on an ordered ladder too", {
  # Fixture 2 (scalar invariance violated by a threshold shift in items 1
  # and 2). A loading is freed at the scalar rung: it is held equal at the
  # metric rung before it, so the full metric model is not the reference.
  s <- fam_ordered_sim(0.5)
  st <- sieve_new_state()
  st$dataset_name <- "ordered"
  st$raw <- s$data
  st$items <- s$data[, s$items]
  st$id_vars <- "g"
  st$meta <- s$meta
  st$confirmed <- TRUE
  st$palette <- "nd"
  st$thresholds <- sieve_default_thresholds()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "g")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_null(r$error)
    expect_identical(r$family, "scaled")
    session$setInputs(inv_level = "scalar")
    session$setInputs(inv_diag = 1)
    dg <- inv_diag()
    row <- which(dg$table$type == "loading")[1L]
    expect_false(is.na(row))
    session$setInputs(inv_diag_tbl_rows_selected = row)
    session$setInputs(inv_refit = 1)
    p_res <- inv_partial()
    expect_null(p_res$error)
    expect_true(p_res$reference_partial)
    expect_identical(p_res$family, "scaled")
    # The reference has one constraint fewer than the ladder's rung, and the
    # partial model has more degrees of freedom than its reference.
    prev <- p_res$prev
    expect_equal(lav(p_res$prev_fit, "df"), lav(r$fits[[prev]], "df") - 1)
    expect_gt(lav(p_res$fit, "df"), lav(p_res$prev_fit, "df"))
    # The standard statistics are ordered as nested models must be.
    expect_gte(lav(p_res$fit, "chisq"), lav(p_res$prev_fit, "chisq") - 1e-6)
    ref <- lavaan::lavTestLRT(p_res$prev_fit, p_res$fit)
    expect_equal(p_res$lrt_chisq, ref[2L, "Chisq diff"], tolerance = 1e-10)
    expect_equal(p_res$lrt_df, ref[2L, "Df diff"])
    expect_equal(p_res$dcfi, lav(p_res$fit, "cfi.scaled") - lav(p_res$prev_fit, "cfi.scaled"),
      tolerance = 1e-12
    )
    out <- fam_text(output$inv_partial_out)
    expect_match(out, "with the same parameters free", fixed = TRUE)
    expect_match(out, "satorra.2000", fixed = TRUE)
  })
})

test_that("a set of candidate models is read in one family (the optimizer's table)", {
  # Seven items in two constructs: A has three ordered items and one
  # continuous item (a4), B has three ordered items. Dropping at most one
  # item and keeping at least three gives five candidates, and exactly one
  # of them (the one that drops a4) is a model of ordered items only.
  # lavaan reports robust indices for that one and none for the four that
  # mix ordered and continuous items.
  n <- 400L
  d <- withr::with_seed(606L, {
    eta <- cbind(stats::rnorm(n), NA_real_)
    eta[, 2] <- 0.45 * eta[, 1] + sqrt(1 - 0.45^2) * stats::rnorm(n)
    lam <- c(.75, .7, .65, .6, .8, .7, .65)
    y <- sapply(1:7, function(j) {
      lam[j] * eta[, if (j <= 4L) 1L else 2L] + sqrt(1 - lam[j]^2) * stats::rnorm(n)
    })
    # a1 also reflects the second factor, which the fitted model leaves
    # out, so the model misfits and the families give different values.
    y[, 1] <- y[, 1] + 0.35 * eta[, 2]
    as.data.frame(y)
  })
  names(d) <- c(paste0("a", 1:4), paste0("b", 1:3))
  ordered <- setdiff(names(d), "a4")
  for (v in ordered) {
    d[[v]] <- as.integer(cut(d[[v]], c(-Inf, -1, 0, 1, Inf)))
  }
  meta <- data.frame(
    item = names(d), label = names(d), construct = rep(c("A", "B"), c(4L, 3L)), facet = "",
    type = ifelse(names(d) == "a4", "continuous", "ordered"),
    status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
  res <- sieve_optimize_items(d, meta, max_drop = 1L, min_keep = 3L, simple_structure = FALSE)
  expect_null(res$error)
  expect_identical(nrow(res$table), 5L)
  expect_identical(res$fit_family, "scaled")
  expect_match(res$fit_family_reason, "scaled family for every model", fixed = TRUE)
  expect_match(res$fit_family_reason, "The robust family is not used", fixed = TRUE)

  # Every candidate refit directly with lavaan on hand-written syntax.
  drops_a4 <- vapply(res$kept, function(k) !("a4" %in% k), logical(1))
  expect_identical(sum(drops_a4), 1L)
  for (i in seq_len(nrow(res$table))) {
    kept <- res$kept[[i]]
    syntax <- paste(sprintf("LA =~ %s", paste(intersect(kept, paste0("a", 1:4)), collapse = " + ")),
      sprintf("LB =~ %s", paste(intersect(kept, paste0("b", 1:3)), collapse = " + ")),
      sep = "\n"
    )
    direct <- lavaan::cfa(syntax,
      data = d[, kept], estimator = "WLSMV",
      ordered = intersect(kept, ordered), std.lv = TRUE
    )
    expect_equal(res$table$cfi[i], lav(direct, "cfi.scaled"), tolerance = 1e-10)
    expect_equal(res$table$rmsea[i], lav(direct, "rmsea.scaled"), tolerance = 1e-10)
    expect_equal(res$table$rmsea_lo[i], lav(direct, "rmsea.ci.lower.scaled"), tolerance = 1e-10)
    expect_equal(res$table$rmsea_hi[i], lav(direct, "rmsea.ci.upper.scaled"), tolerance = 1e-10)
    robust <- lav(direct, "cfi.robust")
    # Robust exists for the all-ordered candidate only, and there it is a
    # different number from the scaled one, so the column never holds the
    # scaled value under the robust name.
    expect_identical(is.finite(robust), drops_a4[i])
    if (drops_a4[i]) {
      expect_gt(abs(robust - lav(direct, "cfi.scaled")), 1e-6)
    }
  }

  # The helper on its own: a model with no fit has an empty row and does
  # not decide the family; with no family common to all, nothing is filled.
  ordered_only <- sieve_fit_measures(sieve_fit_cfa(
    d[, setdiff(names(d), "a4")],
    meta[meta$item != "a4", ]
  ))
  mixed <- sieve_fit_measures(sieve_fit_cfa(d, meta))
  expect_identical(sieve_fit_family(ordered_only), "robust")
  expect_identical(sieve_fit_family(mixed), "scaled")
  shared <- sieve_fit_common(list(ordered_only, mixed, NULL))
  expect_identical(shared$family, "scaled")
  expect_equal(shared$values$cfi[1L], attr(ordered_only, "families")$families$scaled$values[["cfi"]])
  expect_equal(shared$values$cfi[2L], unname(mixed[["cfi"]]))
  expect_true(all(is.na(shared$values[3L, ])))
  expect_true(all(is.na(sieve_fit_common(list(NULL, NULL))$values)))
  # Many models: the reason names the first few and counts the rest.
  many <- sieve_fit_common(c(list(ordered_only), rep(list(mixed), 9L)))
  expect_identical(many$family, "scaled")
  expect_match(many$reason, "'model 2', 'model 3', 'model 4', 'model 5', and 5 more", fixed = TRUE)

  # The module names the family in the column headers.
  st <- sieve_new_state()
  st$dataset_name <- "pool"
  st$raw <- d
  st$items <- d
  st$id_vars <- character(0)
  st$meta <- meta
  st$confirmed <- TRUE
  st$palette <- "nd"
  st$thresholds <- sieve_default_thresholds()
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(max_drop = 1, min_keep = 3, ss = FALSE, cap = 200)
    session$setInputs(run = 1)
    tbl <- fam_text(output$results)
    expect_match(tbl, "CFI (scaled)", fixed = TRUE)
    expect_match(tbl, "RMSEA (scaled)", fixed = TRUE)
    expect_match(fam_text(output$run_msg), "scaled family for every model", fixed = TRUE)
  })
})

test_that("ECVI is reported only for a model that has a solution", {
  hs <- fam_hs()
  # A model with a solution: ECVI by Browne and Cudeck's formula, the
  # statistic plus twice the number of free parameters, over the sample size.
  fit <- sieve_fit_cfa(hs$items, hs$meta)
  row <- sieve_cv_fit_row("all cases", fit)
  expected <- (lav(fit, "chisq") + 2 * lav(fit, "npar")) / lavaan::lavInspect(fit, "ntotal")
  expect_equal(row$ECVI, expected, tolerance = 1e-10)
  expect_true(row$ECVI.lo <= row$ECVI && row$ECVI <= row$ECVI.hi)
  expect_equal(row$CFI, lav(fit, "cfi.robust"), tolerance = 1e-12)
  # Not identified: three orthogonal factors, one with two indicators. The
  # engine still returns a statistic, and DMAR::ecvi() a number from it.
  two <- hs$meta[1:8, ]
  run <- sieve_run_cfa(sieve_cfa_spec(two, orthogonal = TRUE), hs$items)
  expect_identical(run$status$code, "not_identified")
  from_engine <- DMAR::ecvi(fit = run$fit)
  expect_true(is.finite(from_engine$value[from_engine$term == "ecvi"]))
  none <- sieve_cv_fit_row("not identified", run$fit)
  expect_true(all(is.na(none[, c("CFI", "RMSEA", "SRMR", "ECVI", "ECVI.lo", "ECVI.hi")])))
  # Not converged, and no fit at all.
  few <- sieve_fit_cfa(hs$items[1:8, ], hs$meta)
  expect_true(all(is.na(sieve_cv_fit_row("eight cases", few)[, c("ECVI", "ECVI.lo", "ECVI.hi")])))
  expect_true(all(is.na(sieve_cv_fit_row("none", NULL)[, c("ECVI", "ECVI.lo", "ECVI.hi")])))
})

test_that("the reason a robust family is absent names the test statistic the fit has", {
  hs <- fam_hs()
  model <- sieve_cfa_spec(hs$meta)$model
  d <- hs$items[stats::complete.cases(hs$items), ]
  # A mean and variance adjusted (Satterthwaite) statistic: lavaan reports
  # the scaled family for it and defines no robust index.
  adjusted <- sieve_fit_families(lavaan::cfa(model, d, estimator = "MLMVS", std.lv = TRUE))
  expect_true(is.finite(adjusted$families$scaled$values[["chisq"]]))
  expect_false(adjusted$families$robust$available)
  expect_match(adjusted$families$robust$reason, "mean.var.adjusted", fixed = TRUE)
  expect_false(grepl("this fit has neither", adjusted$families$robust$reason, fixed = TRUE))
  # The standard statistic alone on complete cases: the reason says the fit
  # has neither.
  plain <- sieve_fit_families(lavaan::cfa(model, d, estimator = "ML", std.lv = TRUE))
  expect_match(plain$families$robust$reason, "this fit has neither", fixed = TRUE)
})


test_that("a weighted MLR fit names the Yuan-Bentler statistic as its scaled chi square", {
  # lavaan lists Browne's ADF residual statistic ahead of the Yuan-Bentler
  # one for ML with sampling weights; the note must name the Yuan-Bentler
  # statistic, not Browne's, as the scaled one.
  d <- lavaan::HolzingerSwineford1939
  d$w <- seq(0.5, 1.5, length.out = nrow(d))
  # ML with sampling weights: lavaan uses robust standard errors and the
  # Yuan-Bentler statistic, and lists Browne's ADF residual statistic
  # between them.
  fit <- lavaan::cfa("Visual =~ x1 + x2 + x3\nTextual =~ x4 + x5 + x6", data = d, estimator = "ML", sampling.weights = "w", std.lv = TRUE)
  m <- sieve_fit_measures(fit)
  expect_identical(sieve_fit_family(m, "test"), "scaled")
  scaled <- grep("model chi square", sieve_fit_note(m), value = TRUE, ignore.case = TRUE)
  expect_length(scaled, 1L)
  expect_match(scaled, "(lavaan test \"yuan.bentler.mplus\")", fixed = TRUE)
  expect_false(grepl("browne", scaled, fixed = TRUE))
})

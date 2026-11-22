# Public boundary checks complement each package's internal numerical tests.
test_that("the supported DMAR contract carries estimator-matched SEs and requested RMSEA coverage", {
  # Not on the CRAN check's clock (about 1 s); the full suite runs it.
  skip_on_cran()
  x <- lavaan::HolzingerSwineford1939[, paste0("x", 4:7)]
  x[1:10, 1] <- NA_real_
  groups <- list(f = names(x))
  for (est in c("ML", "MLR")) {
    # The estimator-matched standard errors are DMAR 1.1.0's default. A
    # DMAR that announces that default once per session does so with a
    # dmar_default_changed notice; the handler lets the test run without a
    # warning whether the notice comes or not.
    fit <- withCallingHandlers(
      DMAR::cfa_k(x, groups, estimator = est, missing = "fiml", output = "fit"),
      dmar_default_changed = function(w) invokeRestart("muffleWarning")
    )
    info <- sieve_fit_inference(fit)
    expect_identical(info$se, if (est == "ML") "standard" else "robust.huber.white")
    expect_identical(info$missing, "ml")
    direct <- DMAR::sem_fit_measures(fit, conf_level = .90)
    reference <- lavaan::fitMeasures(fit, fm.args = list(rmsea.ci.level = .90))
    family <- attr(direct, "sem_fit")$version
    suffix <- if (family == "standard") "" else paste0(".", family)
    expect_equal(direct$value[direct$term == "rmsea_ci_lower"], unname(reference[paste0("rmsea.ci.lower", suffix)]))
    expect_equal(attr(direct, "conf_level"), .90)
  }
})

test_that("derived score columns stay aligned and one method can remain available", {
  hs <- sieve_hs_data()
  d <- data.frame(y1 = hs$t6_paragraph_comprehension, y2 = hs$t7_sentence,
    y3 = hs$t9_word_meaning, z = hs$t12_counting_groups_of_dots, a = hs$age_years)
  fit <- lavaan::sem("f =~ y1+y2+y3\nz ~ f\nz ~~ a", d, std.lv = TRUE, meanstructure = TRUE)
  direct <- DMAR::factor_scores(fit, data = d, conf_level = .90)
  map <- attr(direct, "derived_columns")
  expect_true(all(startsWith(unname(map), ".derived_")))
  regression <- sieve_factor_score_record(fit, "regression", data = d, conf_level = .90)
  bartlett <- sieve_factor_score_record(fit, "Bartlett", data = d, conf_level = .90)
  expect_equal(regression$scores[[1]], direct[[map[["regression"]]]])
  expect_true(all(is.finite(regression$uncertainty$se)))
  expect_true(all(is.na(bartlett$scores[[1]])))
  expect_true(all(is.na(bartlett$uncertainty$lower)))
  expect_length(bartlett$determinacy, 1L)
  expect_true(all(is.na(bartlett$determinacy)))
  expect_length(regression$determinacy, 1L)
  expect_true(all(is.finite(regression$determinacy)))
  expect_true(all(grepl("Bartlett scores withheld", bartlett$uncertainty$reason, fixed = TRUE)))
})

test_that("unavailable unrestricted moments withhold fit indices but retain proper H0 estimates", {
  d <- withr::with_seed(113, {
    f <- rnorm(60)
    as.data.frame(sapply(1:6, function(j) .7*f + rnorm(60, sd = sqrt(.51))))
  })
  names(d) <- paste0("y", 1:6)
  d[cbind(5:60, rep(1:6, length.out = 56))] <- NA_real_
  fit <- lavaan::cfa("f =~ y1+y2+y3+y4+y5+y6", d, missing = "ml", std.lv = TRUE)
  pe <- lavaan::parameterEstimates(fit)
  expect_true(all(is.finite(pe$se)))
  families <- sieve_fit_families(fit)
  expect_false(families$families$standard$available)
  expect_match(families$families$standard$reason, "unrestricted H1", fixed = TRUE)
  expect_match(sieve_residual_pairs(fit)$error, "unrestricted H1", fixed = TRUE)
  expect_match(sieve_model_composite(fit)$error, "unrestricted H1", fixed = TRUE)
  expect_error(sieve_composite_coefficients(fit, stats::setNames(rep(1, 6), names(d))), "unrestricted H1", fixed = TRUE)
  meta <- data.frame(item = names(d), label = names(d), construct = "f", facet = NA_character_,
    type = "continuous", status = "active", reverse = FALSE)
  spec <- sieve_two_stage_spec(sieve_cfa_spec(meta), TRUE)
  rejected <- sieve_run_model(spec, d)
  expect_null(rejected$fit)
  expect_match(rejected$error, "unrestricted H1", fixed = TRUE)
  expect_true(families$aic$available)
  expect_equal(families$aic$value, stats::AIC(fit))
  expect_warning(omega <- DMAR::reliability_omega(d, missing = "fiml", ci_method = "ml",
    denominator = "model_implied"), class = "dmar_h1_unavailable")
  expect_true(is.finite(sieve_reliability_value(omega, "ci_lower")))
  expect_error(DMAR::reliability_omega(d, missing = "fiml", denominator = "observed"),
    class = "dmar_fiml_unbounded")
})

test_that("DMAR reporting and bootstrap metadata remain intact at the adapter boundary", {
  # Not on the CRAN check's clock (a 400-replication bootstrap, about 12 s);
  # the full suite runs it.
  skip_on_cran()
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  result <- DMAR::reliability_omega(d, ci_method = "percentile", B = 400L, seed = 1L,
    denominator = "model_implied")
  spec <- sieve_reliability_spec(names(d), rep("continuous", 3L),
    ci_method = "ml", denominator = "model_implied")
  wrapped <- sieve_reliability_engine_metadata(list(spec = spec, uncertainty = list()), result)
  expect_identical(wrapped$uncertainty$engine_bootstrap, attr(result, "bootstrap"))
  expect_true(attr(result, "bootstrap")$returned >= 40L)
  expect_s3_class(generics::tidy(result), "data.frame")
  sentence <- DMAR::results_sentence(result)
  expect_true(is.character(sentence))
  expect_match(paste(sentence, collapse = " "), "interval|CI", ignore.case = TRUE)
})


test_that("DMAR's H1 refusal arrives as a classed condition the capture keeps", {
  # DMAR refuses with a classed condition (dmar_h1_unavailable, a
  # dmar_refusal); a refusal with the same words and no class is read by
  # the word fallback.
  skip_if_not("dmar_metadata" %in% getNamespaceExports("DMAR"),
    "the installed DMAR does not export dmar_metadata(), so its refusals carry no class")
  d <- withr::with_seed(113, {
    f <- rnorm(60)
    as.data.frame(sapply(1:6, function(j) .7*f + rnorm(60, sd = sqrt(.51))))
  })
  names(d) <- paste0("y", 1:6)
  d[cbind(5:60, rep(1:6, length.out = 56))] <- NA_real_
  fit <- lavaan::cfa("f =~ y1+y2+y3+y4+y5+y6", d, missing = "ml", std.lv = TRUE)
  capture <- sieve_capture(DMAR::sem_fit_measures(fit, conf_level = .90))
  expect_false(capture$ok)
  expect_match(capture$error, "unrestricted H1", fixed = TRUE)
  expect_true(all(c("dmar_h1_unavailable", "dmar_refusal") %in% capture$error_class))
  expect_match(sieve_h1_refusal(fit), "unrestricted H1", fixed = TRUE)
})

test_that("complete FIML samples still receive the unrestricted-model eligibility check", {
  d <- withr::with_seed(113, as.data.frame(matrix(rnorm(36), 6, 6)))
  names(d) <- paste0("y", 1:6)
  model <- paste0("f =~ ", paste0("l*y", 1:6, collapse = "+"), "\n",
    paste0("y", 1:6, " ~~ e*y", 1:6, collapse = "\n"))
  run <- sieve_capture(lavaan::cfa(model, d, missing = "ml", std.lv = TRUE))
  expect_true(run$ok)
  expect_match(sieve_h1_refusal(run$value), "unrestricted H1", fixed = TRUE)
})


test_that("grouped score determinacy retains group and factor identities", {
  d <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa("visual =~ x1+x2+x3\ntext =~ x4+x5+x6", d,
    group = "school", std.lv = TRUE)
  direct <- attr(DMAR::factor_scores(fit, data = d), "determinacy")
  record <- sieve_factor_score_record(fit, "regression", data = d)
  expect_equal(record$determinacy_table$determinacy, direct$determinacy_regression)
  expect_equal(record$determinacy_table$group, direct$group)
  expect_equal(record$determinacy_table$factor, direct$factor)
  expect_length(record$determinacy, 2L)
  expect_true(all(lengths(record$determinacy) == 2L))
})


test_that("one selected group retains the numeric determinacy compatibility contract", {
  d <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa("f =~ x4+x5+x6", d, group = "school", group.label = "Pasteur", std.lv = TRUE)
  record <- sieve_factor_score_record(fit, "regression", data = d)
  expect_type(record$determinacy, "double")
  expect_named(record$determinacy, "f")
  expect_identical(unique(record$determinacy_table$group), "Pasteur")
})

test_that("two-stage auxiliary composite intervals do not enter an unverified replay", {
  test_frame <- environment()
  d <- lavaan::HolzingerSwineford1939
  d[1:10, "x4"] <- NA_real_
  fit <- lavaan::cfa("f =~ x4+x5+x6", d, missing = "two.stage", aux = "ageyr", std.lv = TRUE)
  local_mocked_bindings(bootstrapLavaan = function(...) stop("Unverified replay invoked"),
    .package = "lavaan", .env = test_frame)
  result <- sieve_model_composite(fit, B = 100L)
  expect_null(result$error)
  expect_true(all(is.finite(result$table$estimate)))
  expect_true(all(is.na(result$table$lower)))
  expect_match(result$interval_reason, "aligned auxiliary resampling", fixed = TRUE)
})


test_that("saturated summaries identify conventions separately from parameter SEs", {
  fit <- lavaan::cfa("f =~ x4+x5+x6", lavaan::HolzingerSwineford1939, std.lv = TRUE)
  text <- sieve_inference_text(sieve_fit_inference(fit))
  expect_match(text, "Saturated model (df = 0)", fixed = TRUE)
  expect_match(text, "no evidence of approximate fit", fixed = TRUE)
  expect_match(text, "parameter SEs and intervals are separate", fixed = TRUE)
  families <- sieve_fit_families(fit)
  selected <- sieve_fit_select(families, "standard")
  expect_equal(unname(selected[c("tli", "rmsea", "rmsea_lo", "rmsea_hi")]), c(1, 0, 0, 0))
  alert <- as.character(sieve_status_alert(fit))
  expect_match(alert, "zero degrees of freedom", fixed = TRUE)
  expect_false(grepl("reproduces the data exactly", alert, fixed = TRUE))
})


test_that("positive discrepancy at zero df is not called saturated", {
  S <- matrix(c(1,.8,.8,.8,1,.5,.8,.5,1),3,3,
    dimnames = list(paste0("y",1:3), paste0("y",1:3)))
  run <- sieve_capture(lavaan::cfa("f =~ y1+y2+y3", sample.cov = S,
    sample.nobs = 200, std.lv = TRUE, bounds = "pos.var"))
  expect_true(run$ok)
  method <- sieve_fit_inference(run$value)
  expect_equal(method$df, 0)
  expect_gt(method$chisq, 0)
  text <- sieve_inference_text(method)
  expect_match(text, "conventions do not apply", fixed = TRUE)
  expect_false(grepl("Saturated model (df = 0)", text, fixed = TRUE))
  families <- sieve_fit_families(run$value)
  withheld <- c("tli", "rmsea", "rmsea_lo", "rmsea_hi")
  for (family in families$families) {
    expect_true(all(is.na(family$values[withheld])))
    expect_true(all(grepl("without established exact fit", family$missing[withheld], fixed = TRUE)))
  }
  selected <- sieve_fit_select(families)
  expect_true(all(is.na(selected[withheld])))
  expect_true(all(is.finite(selected[c("cfi", "srmr", "aic", "bic")])))
})

test_that("the public all-row correction failure is retained at the supplied-fit boundary", {
  test_frame <- environment()
  d <- lavaan::HolzingerSwineford1939
  d[1:10, "x4"] <- NA_real_
  fit <- lavaan::cfa("f =~ x4+x5+x6", d, missing = "two.stage", aux = "ageyr", std.lv = TRUE)
  message <- "Two-stage fit measures require the same all-row sample as the first-stage moments; the auxiliary-only case correction could not be computed."
  local_mocked_bindings(sem_fit_measures = function(...) stop(message, call. = FALSE),
    .package = "DMAR", .env = test_frame)
  expect_identical(sieve_h1_refusal(fit), message)
  families <- sieve_fit_families(fit)
  expect_false(families$aic$available)
  expect_false(families$bic$available)
  expect_match(sieve_residual_pairs(fit)$error, "correction could not be computed", fixed = TRUE)
})

test_that("DMAR's refusals are recognized by their condition class before their words", {
  d <- lavaan::HolzingerSwineford1939
  d[1:10, "x4"] <- NA_real_
  fiml <- lavaan::cfa("f =~ x4+x5+x6", d, missing = "ml", std.lv = TRUE)
  two_stage <- lavaan::cfa("f =~ x4+x5+x6", d, missing = "two.stage", aux = "ageyr", std.lv = TRUE)
  refusal <- function(condition_class, words) {
    force(condition_class)
    force(words)
    function(...) stop(structure(
      list(message = words, call = NULL, code = sub("^dmar_", "", condition_class)),
      class = c(condition_class, "dmar_refusal", "error", "condition")))
  }
  # The class decides; these words are not the ones the fallback looks for.
  h1 <- "The saturated moments of this fit are unavailable (classed, in new words)."
  with_mocked_bindings({
    expect_identical(sieve_h1_refusal(fiml), h1)
    expect_identical(sieve_h1_refusal(two_stage), h1)
  }, sem_fit_measures = refusal("dmar_h1_unavailable", h1), .package = "DMAR")
  rows <- "The fitted sample is not the all-row sample (classed, in new words)."
  with_mocked_bindings({
    expect_identical(sieve_h1_refusal(two_stage), rows)
    # As with the words, the two-stage refusal is read from a two-stage fit only.
    expect_null(sieve_h1_refusal(fiml))
  }, sem_fit_measures = refusal("dmar_two_stage_rows_unavailable", rows), .package = "DMAR")
  # Without a class, the words decide.
  with_mocked_bindings(
    expect_match(sieve_h1_refusal(fiml), "unrestricted H1", fixed = TRUE),
    sem_fit_measures = function(...) stop("The unrestricted H1 model is unavailable.", call. = FALSE),
    .package = "DMAR")
  with_mocked_bindings(
    expect_null(sieve_h1_refusal(fiml)),
    sem_fit_measures = function(...) stop("Some other failure.", call. = FALSE),
    .package = "DMAR")
  # A refusal of another class is not one of these two.
  with_mocked_bindings(
    expect_null(sieve_h1_refusal(two_stage)),
    sem_fit_measures = refusal("dmar_fiml_unbounded", "Another classed refusal."),
    .package = "DMAR")
})

test_that("the SRMR SIEVER reports is the SRMR DMAR reports (the anchor that runs on CRAN)", {
  # Two fits of one model, complete data and FIML: SIEVER's SRMR is DMAR's,
  # the covariance residuals alone, and a mean structure does not move it.
  # The block after this one holds the same over seven kinds of fit, with
  # the note and the auxiliary variables; it takes about three seconds and
  # runs off CRAN.
  hs <- lavaan::HolzingerSwineford1939
  model <- "visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6\n speed =~ x7 + x8 + x9"
  quiet <- function(x) suppressMessages(suppressWarnings(x))
  fits <- list(
    complete = lavaan::cfa(model, hs, std.lv = TRUE),
    fiml = quiet(lavaan::cfa(model, hs, std.lv = TRUE, missing = "fiml"))
  )
  shown <- lapply(fits, function(fit) quiet(sieve_fit_measures(fit)))
  for (name in names(fits)) {
    engine <- as.data.frame(quiet(DMAR::sem_fit_measures(fits[[name]])))
    expect_equal(unname(shown[[name]][["srmr"]]), as.numeric(engine$value[engine$term == "srmr"]),
      tolerance = 1e-12, info = name)
    expect_identical(attr(shown[[name]], "families")$srmr$measure, "srmr_bentler_nomean", info = name)
  }
  expect_equal(shown$fiml[["srmr"]], shown$complete[["srmr"]], tolerance = 1e-6)
  expect_equal(unname(shown$fiml[["srmr"]]), 0.0652, tolerance = 5e-4)
  expect_equal(attr(shown$fiml, "families")$srmr$lavaan, 0.0595, tolerance = 5e-4)
})

test_that("the SRMR SIEVER reports is the SRMR DMAR reports, for every kind of fit", {
  # Not on the CRAN check's clock (about 3 s); the block before this one is
  # the anchor there, and the full suite runs this one.
  skip_on_cran()
  # lavaan's srmr also averages in the mean residuals of a continuous model
  # with a mean structure (every FIML fit, every multiple group fit), while
  # DMAR::sem_fit_measures() reports the covariance residuals alone
  # (lavaan's srmr_bentler_nomean): 0.060 against 0.065 for this model
  # under FIML, one misfit under two averages. SIEVER reports DMAR's.
  hs <- lavaan::HolzingerSwineford1939
  model <- "visual =~ x1 + x2 + x3\n textual =~ x4 + x5 + x6\n speed =~ x7 + x8 + x9"
  ordered <- as.data.frame(lapply(hs[paste0("x", 1:9)], function(x) {
    as.integer(cut(x, stats::quantile(x, 0:4 / 4), include.lowest = TRUE))
  }))
  quiet <- function(x) suppressMessages(suppressWarnings(x))
  fits <- list(
    complete = lavaan::cfa(model, hs, std.lv = TRUE),
    fiml = quiet(lavaan::cfa(model, hs, std.lv = TRUE, missing = "fiml")),
    mlr = quiet(lavaan::cfa(model, hs, std.lv = TRUE, missing = "fiml", estimator = "MLR")),
    ordered = quiet(lavaan::cfa(model, ordered, std.lv = TRUE, ordered = TRUE, estimator = "WLSMV")),
    groups = quiet(lavaan::cfa(model, hs, std.lv = TRUE, group = "school")),
    scalar = quiet(lavaan::cfa(model, hs, std.lv = TRUE, group = "school", missing = "fiml",
      group.equal = c("loadings", "intercepts"))),
    auxiliary = quiet(lavaan::cfa(model, hs, std.lv = TRUE, missing = "fiml", aux = "ageyr"))
  )
  shown <- lapply(fits, function(fit) quiet(sieve_fit_measures(fit)))
  for (name in names(fits)) {
    engine <- as.data.frame(quiet(DMAR::sem_fit_measures(fits[[name]])))
    expect_equal(unname(shown[[name]][["srmr"]]), as.numeric(engine$value[engine$term == "srmr"]),
      tolerance = 1e-12, info = name)
    expect_identical(attr(shown[[name]], "families")$srmr$measure, "srmr_bentler_nomean", info = name)
  }
  # A mean structure does not move it: these data are complete, so FIML fits
  # the same covariances, and lavaan's own srmr is the smaller number.
  expect_equal(shown$fiml[["srmr"]], shown$complete[["srmr"]], tolerance = 1e-6)
  expect_equal(unname(shown$fiml[["srmr"]]), 0.0652, tolerance = 5e-4)
  expect_equal(attr(shown$fiml, "families")$srmr$lavaan, 0.0595, tolerance = 5e-4)
  # The note beside the indices says which SRMR this is and what lavaan prints.
  note <- sieve_fit_note(shown$fiml)
  expect_true(any(grepl("covariance residuals alone, the SRMR DMAR reports", note, fixed = TRUE)))
  expect_true(any(grepl("lavaan's own srmr, which also averages in the mean residuals", note, fixed = TRUE)))
  expect_true(any(grepl(sprintf("is %.3f for this fit", lavaan::fitMeasures(fits$fiml, "srmr")), note, fixed = TRUE)))
  expect_false(any(grepl("auxiliary", note, fixed = TRUE)))
  expect_false(any(grepl("lavaan's own srmr", sieve_fit_note(shown$complete), fixed = TRUE)))
  # With an auxiliary variable the SRMR is of the model's own variables, as
  # DMAR forms it. lavaan averages the auxiliary's variances and
  # covariances in, with residuals of zero or nearly so, and its measure is
  # the smaller number for the same model (0.060 against 0.065 here).
  aux <- attr(shown$auxiliary, "families")$srmr
  expect_identical(aux$aux, "ageyr")
  expect_identical(aux$source, "DMAR")
  expect_identical(attr(shown$fiml, "families")$srmr$source, "lavaan")
  # These data are complete, so the auxiliary changes nothing in the model's fit.
  expect_equal(unname(shown$auxiliary[["srmr"]]), unname(shown$fiml[["srmr"]]), tolerance = 1e-6)
  expect_equal(aux$with_aux, unname(lavaan::fitMeasures(fits$auxiliary, "srmr_bentler_nomean")), tolerance = 1e-12)
  expect_lt(aux$with_aux, aux$value - 1e-3)
  aux_note <- sieve_fit_note(shown$auxiliary)
  expect_true(any(grepl("This fit has 1 auxiliary variable, which is left out of this SRMR, as DMAR leaves it out",
    aux_note, fixed = TRUE)))
  expect_true(any(grepl(sprintf("its srmr_bentler_nomean is %.3f", aux$with_aux), aux_note, fixed = TRUE)))
  expect_false(any(grepl("the SRMR DMAR reports (lavaan's srmr_bentler_nomean)", aux_note, fixed = TRUE)))
  # Where DMAR returns no fit measures for such a fit, lavaan's measure stays
  # and the note says that the auxiliary variable is averaged in.
  test_frame <- environment()
  testthat::local_mocked_bindings(sem_fit_measures = function(...) stop("no measures for this fit"),
    .package = "DMAR", .env = test_frame)
  kept <- sieve_fit_families(fits$auxiliary)$srmr
  expect_identical(kept$source, "lavaan")
  expect_equal(kept$value, unname(lavaan::fitMeasures(fits$auxiliary, "srmr_bentler_nomean")), tolerance = 1e-12)
  expect_true(any(grepl("This fit has 1 auxiliary variable, whose variances and covariances are averaged into this SRMR",
    sieve_srmr_note(kept), fixed = TRUE)))
  # A record that does not name the measure carries no such sentence.
  expect_null(sieve_srmr_note(list(value = 0.06, available = TRUE, reason = NA_character_)))
  expect_null(sieve_srmr_note(list(value = NA_real_, available = FALSE, reason = "none")))
  expect_identical(sieve_srmr_measure(c("srmr", "cfi")), "srmr")
})

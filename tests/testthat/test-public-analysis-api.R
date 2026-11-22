# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

test_that("the public analysis dispatcher preserves the engine's numbers", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- sieve_build_meta(
    d,
    names(d),
    list(
      Visual = paste0(
        "x",
        1:3
      ),
      Textual = paste0(
        "x",
        4:6
      ),
      Speed = paste0(
        "x",
        7:9
      )
    )
  )
  result <- sieve_analyze("cfa", items_df = d, meta = meta, estimator = "mlr")
  direct <- lavaan::cfa("Visual =~ x1+x2+x3\nTextual =~ x4+x5+x6\nSpeed =~ x7+x8+x9",
    data = d, std.lv = TRUE, estimator = "MLR", missing = "fiml"
  )
  expect_s3_class(result, "siever_analysis")
  expect_equal(unname(lavaan::coef(result$result)), unname(lavaan::coef(direct)), tolerance = 1e-7)
  expect_equal(as.data.frame(result)$est, lavaan::parameterEstimates(direct)$est, tolerance = 1e-7)
  # A Wald interval at another level is formed from the stored estimate and
  # standard error, over the free parameters, named as coef() names them,
  # and says so.
  limits <- stats::confint(result, level = .90)
  oracle <- lavaan::parameterEstimates(direct, level = .90, remove_nonfree = TRUE)
  expect_identical(rownames(limits), names(lavaan::coef(direct)))
  expect_equal(matrix(as.numeric(limits), ncol = 2L), unname(as.matrix(oracle[, c("ci.lower", "ci.upper")])), tolerance = 1e-7)
  expect_identical(attr(limits, "source"), "computed from the stored estimate and standard error")
  expect_identical(attr(limits, "conf_level"), 0.90)
  expect_identical(summary(result)$analysis, "cfa")
  expect_error(sieve_analyze("reliability", design = list(unit = "clustered")), "design-aware")
})

test_that("short waits are not rounded to five seconds", {
  local_measured_rates()
  expect_identical(sieve_duration_label(.5), "under 2 seconds")
  expect_identical(sieve_duration_label(1.9), "under 2 seconds")
  expect_identical(sieve_duration_label(3.2), "about 4 seconds")
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  expect_gte(sieve_fit_wait(9, "MLR"), .5)
  original <- sieve_fit_wait(68, "MLR")
  sieve_rate_note("cfa_MLR", 40, sieve_fit_units(68, "MLR"))
  expect_equal(sieve_fit_wait(68, "MLR"), 40)
  expect_gt(original, 25)
})

test_that("a request-key observer does not estimate ordered correlations at Confirm", {
  fixture <- sieve_hs9_fixture()
  state <- sieve_new_state()
  state$raw <- fixture$data
  data <- as.data.frame(lapply(fixture$data[, fixture$item_cols], function(x) as.integer(cut(x, 5))))
  state$items <- data
  meta <- sieve_build_meta(data, names(data), fixture$theory_map)
  meta$type <- "ordered"
  state$meta <- meta
  state$confirmed <- TRUE
  called <- new.env(parent = emptyenv())
  called$n <- 0L
  local_mocked_bindings(sieve_state_basis = function(...) {
    called$n <- called$n + 1L
    stop("eager basis")
  })
  shiny::testServer(mod_efa_server, args = list(state = state), {
    session$setInputs(n_factors = 3, cor_type = "auto", estimator = "study")
    # The EFA page's requests (the selected solution's and the number of
    # factors comparison's) name the route, not a computed matrix.
    request <- primary_key()
    expect_identical(request$estimator, "WLSMV")
    expect_identical(count_key()$estimator, "WLSMV")
    expect_identical(called$n, 0L)
    expect_false("basis" %in% names(request))
    expect_false("basis" %in% names(count_key()))
  })
})

test_that("SHA-256 file exports work through the older-R utility path", {
  file <- tempfile()
  withr::defer(unlink(file))
  writeBin(charToRaw("abc"), file)
  expected <- "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  expect_identical(sieve_sha256_file(file), expected)
  expect_identical(sieve_sha256_file(file, base_available = FALSE), expected)
})

test_that("public score intervals accept their recorded level and reject a mismatch", {
  data <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  fit <- lavaan::cfa("visual =~ x1+x2+x3
textual =~ x4+x5+x6
speed =~ x7+x8+x9",
    data = data, std.lv = TRUE, estimator = "MLR", missing = "fiml"
  )
  result <- sieve_analyze("factor_scores", fit = fit, data = data, conf_level = .90)
  u <- result$result$uncertainty
  limits <- stats::confint(result, level = .90, table = "uncertainty")
  expect_equal(matrix(as.numeric(limits), ncol = 2L), unname(as.matrix(u[, c("lower", "upper")])))
  expect_identical(rownames(limits)[1:2], c("row 1: visual", "row 2: visual"))
  expect_identical(attr(limits, "source"), "stored")
  # The default is the stored level, 0.90 here.
  expect_identical(stats::confint(result), limits)
  # The conditional Wald interval is formed at another level from the
  # stored score and its standard error.
  at95 <- stats::confint(result, level = .95)
  z <- stats::qnorm(.975)
  expect_equal(matrix(as.numeric(at95), ncol = 2L), cbind(u$estimate - z * u$se, u$estimate + z * u$se), tolerance = 1e-12)
  expect_identical(attr(at95, "source"), "computed from the stored estimate and standard error")
  result$result$uncertainty$level <- NA_real_
  result$intervals <- sieve_analysis_intervals(result)
  expect_error(stats::confint(result, level = .90, table = "uncertainty"),
    "its level column holds no confidence level strictly between 0 and 1 for 903 of its 903 rows",
    fixed = TRUE
  )
})

test_that("recorded model text stays literal and numeric factor labels retain their values", {
  model <- "F =~ a + b + c\n# Respondents' ratings remain model text, including \"quoted\" comments."
  spec <- list(
    model = model, estimator = "MLR", estimator_choice = "mlr",
    ordered = NULL, std_lv = TRUE, orthogonal = FALSE, missing = "fiml",
    engine_args = list(), out_map = c(Y = "response"), pred_map = c(P = "predictor")
  )
  cfa <- parse(text = sieve_cfa_code(spec))
  sem <- parse(text = sieve_sem_code(spec))
  e <- new.env(parent = globalenv())
  eval(cfa[[1L]], e)
  expect_identical(e$cfa_model, model)
  eval(sem[[1L]], e)
  expect_identical(e$sem_model, model)
  e$items <- data.frame(a = 1:3, b = 3:1, c = c(2, 4, 3))
  e$d <- data.frame(
    response = factor(c(10, 30, 20), levels = c(30, 20, 10)),
    predictor = factor(c(100, 200, 300), levels = c(300, 100, 200))
  )
  local_mocked_bindings(sem = function(model, data, ...) data, .package = "lavaan")
  eval(sem, e)
  expect_identical(e$sem_data$Y, c(10, 30, 20))
  expect_identical(e$sem_data$P, c(100, 200, 300))
  expect_identical(
    e$sem_data[, c("Y", "P")],
    stats::setNames(sieve_numeric_columns(e$d, c("response", "predictor"))$data, c("Y", "P"))
  )
})

test_that("bifactor bootstrap replay preserves quoted model comments literally", {
  model <- "g =~ a + b + c\n# Respondents' ratings include \"quoted\" comments."
  plan <- list(
    point = list(syntax = model, signature = list(
      estimator = "ML", se = "standard",
      test = "standard", missing = "listwise", std_lv = TRUE, orthogonal = TRUE,
      ordered = character(), observed = c("a", "b", "c"), latent = "g"
    )),
    quantities = data.frame(row = 1L, index = "omega", key = "g.omega", fixed = FALSE),
    rows = "complete", B = 1L, conf_level = .95, rng = sieve_draw_seed(42), min_used = 39L
  )
  expect_no_warning(code <- parse(text = sieve_bifactor_boot_code(plan)))
  expect_type(code, "expression")
  env <- new.env(parent = globalenv())
  eval(code[[1L]], env)
  expect_identical(env$bifactor_boot_model, model)
  # A plan without an original-row map filters the full frame, never a
  # prefix whose length is the already eligible pool size.
  env$items <- data.frame(a = c(1, NA, 3, 4), b = c(4, 3, 2, 1), c = c(2, 3, NA, 5))
  plan$pool <- env$items[stats::complete.cases(env$items), , drop = FALSE]
  expect_no_warning(legacy <- parse(text = sieve_bifactor_boot_code(plan)))
  for (step in legacy) {
    if (is.call(step) && identical(step[[1L]], quote(`<-`)) &&
        identical(step[[2L]], quote(bifactor_boot_draw))) break
    eval(step, env)
  }
  expect_identical(env$bifactor_boot_pool, plan$pool)
})

test_that("confint() returns a reliability analysis's stored limits by coefficient and forms Wald limits at another level", {
  # The stored 95% limits are read by coefficient. Robust ML is a Wald
  # interval, so 0.90 limits are formed from the stored estimate and standard
  # error, on the recorded scale, and are identical to a rerun at 0.90.
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  spec <- sieve_reliability_spec(names(d), rep("continuous", 3),
    construct = "Textual", missing = "fiml", ci_method = "mlr", denominator = "model_implied"
  )
  a <- sieve_analyze("reliability", data = d, spec = spec)
  limits <- stats::confint(a, level = .95)
  expect_identical(stats::confint(a), limits)
  om <- a$result$omega
  expect_identical(rownames(limits)[1L], paste0("Textual: ", om$label))
  expect_equal(unname(limits[1L, ]), c(om$lower, om$upper), tolerance = 0)
  expect_true(any(startsWith(rownames(limits), "Textual: H")))
  if (!is.null(a$result$alpha)) {
    expect_equal(unname(limits["Textual: Alpha", ]), c(a$result$alpha$lower, a$result$alpha$upper), tolerance = 0)
    # Alpha's record names the scale of its interval, as omega's and H's do.
    expect_match(a$result$alpha$uncertainty$transformation, "^none \\(symmetric about the estimate")
  }
  expect_identical(as.data.frame(a)$conf_level, 0.95)
  expect_identical(attr(limits, "source"), "stored")
  at90 <- stats::confint(a, level = .90)
  expect_identical(attr(at90, "source"), "computed from the stored estimate and standard error")
  expect_identical(attr(at90, "conf_level"), 0.90)
  expect_identical(unname(attr(at90, "transformation")["Textual: H (observed item scores)"]), "logit")
  rerun <- sieve_analyze("reliability", data = d, spec = sieve_reliability_spec(names(d), rep("continuous", 3),
    construct = "Textual", missing = "fiml", ci_method = "mlr", denominator = "model_implied", conf_level = .90
  ))
  expect_equal(matrix(as.numeric(at90), ncol = 2L), matrix(as.numeric(stats::confint(rerun)), ncol = 2L), tolerance = 1e-12)
})

test_that("a failed headless CFA keeps its status and reason, and offers no estimates", {
  # The wrapper returns the failure with its reason, never NULL.
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- sieve_build_meta(d, names(d), list(
    Visual = paste0("x", 1:3), Textual = paste0("x", 4:6), Speed = paste0("x", 7:9)
  ))
  # A constant indicator.
  bad <- d
  bad$x1 <- 1
  a <- sieve_analyze("cfa", items_df = bad, meta = meta)
  expect_null(a$result)
  expect_identical(a$status$code, "failed")
  expect_match(a$status$message, "takes the same value", fixed = TRUE)
  expect_identical(a$diagnostics$error, a$status$message)
  expect_length(summary(a)$tables, 0L)
  expect_error(as.data.frame(a), "This analysis has no estimates.", fixed = TRUE)
  expect_error(stats::confint(a), "no estimates", fixed = TRUE)
  printed <- utils::capture.output(print(a))
  expect_identical(printed[2L], "Status: failed ")
  expect_true(any(grepl("takes the same value", printed, fixed = TRUE)))
  expect_true(any(printed == "Tables: none "))
  # A lone construct of two items is not identified, and says so.
  two <- sieve_build_meta(d[, 1:2], c("x1", "x2"), list(Visual = c("x1", "x2")))
  b <- sieve_analyze("cfa", items_df = d[, 1:2], meta = two)
  expect_identical(b$status$code, "not_identified")
  expect_match(b$status$message, "not identified", fixed = TRUE)
  # An unsupported estimator is refused, naming the supported ones.
  expect_error(sieve_analyze("cfa", items_df = d, meta = meta, estimator = "bogus"), "wlsmv", fixed = TRUE)
  # A fit that succeeds is the identical fit, with status ok.
  ok <- sieve_analyze("cfa", items_df = d, meta = meta, estimator = "mlr")
  expect_identical(ok$status$code, "ok")
  expect_s4_class(ok$result, "lavaan")
})

test_that("a service that returned a value without a usable result reports failed, with the reason", {
  # Every service has a status of its own: a value without a usable result
  # is reported "failed", not "ok".
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- sieve_build_meta(d, names(d), list(
    Visual = paste0("x", 1:3), Textual = paste0("x", 4:6), Speed = paste0("x", 7:9)
  ))
  constant <- d
  constant[, ] <- 1
  # Cross validation that refuses a sample too small to split.
  cv <- sieve_analyze("cross_validation", data = d[1:15, ], meta = meta)
  expect_identical(cv$status$code, "failed")
  expect_match(cv$status$message, "too few to split", fixed = TRUE)
  expect_identical(cv$status$message, cv$result$error)
  printed <- utils::capture.output(print(cv))
  expect_identical(printed[2L], "Status: failed ")
  expect_true(any(grepl("too few to split", printed, fixed = TRUE)))
  # Reliability whose every coefficient failed: no finite omega, H, or alpha.
  spec <- sieve_reliability_spec(names(d)[1:4], rep("continuous", 4),
    construct = "F", ci_method = "none", denominator = "model_implied"
  )
  # lavaan prints its variable table when it refuses constant data; that
  # console text is not a condition and is kept out of the test log.
  quietly <- function(expr) {
    utils::capture.output(value <- expr)
    value
  }
  rel <- quietly(sieve_analyze("reliability", data = constant, spec = spec))
  expect_identical(rel$status$code, "failed")
  expect_false(is.finite(rel$result$omega$estimate %||% NA_real_))
  parts <- summary(rel)$diagnostics$components
  expect_true(all(parts$code != "ok"))
  expect_true(any(grepl("do not vary", parts$message, fixed = TRUE)))
  expect_match(rel$status$message, "do not vary", fixed = TRUE)
  # Invariance that lavaan refuses, an exploratory fit with no solution at
  # any k, and factor scores without an admissible model.
  inv <- quietly(sieve_analyze("invariance", data = cbind(constant, school = lavaan::HolzingerSwineford1939$school), group = "school", meta = meta))
  expect_identical(inv$status$code, "failed")
  # The engine's text, on one line.
  expect_identical(inv$status$message, trimws(gsub("\\s+", " ", inv$result$error)))
  expect_false(grepl("\n", inv$status$message, fixed = TRUE))
  efa <- quietly(sieve_analyze("efa", data = constant, estimator = "ml", ks = 1:2))
  expect_identical(efa$status$code, "failed")
  expect_identical(summary(efa)$diagnostics$components$component, c("k = 1", "k = 2"))
  bad <- suppressWarnings(lavaan::cfa("V =~ x1 + x2 + x3", d, std.lv = TRUE, control = list(iter.max = 1)))
  fs <- sieve_analyze("factor_scores", fit = bad)
  expect_identical(fs$status$code, "failed")
  expect_match(fs$status$message, "admissible measurement model", fixed = TRUE)
  # A norm table that refused every construct.
  norms <- sieve_analyze("norms", data = d[0, ], meta = meta)
  expect_identical(norms$status$code, "failed")
  expect_identical(summary(norms)$diagnostics$components$component, c("Visual", "Textual", "Speed"))
  # The valid analyses stay ok.
  expect_identical(sieve_analyze("cross_validation", data = d, meta = meta, seed = 1)$status$code, "ok")
  expect_identical(sieve_analyze("reliability", data = d, spec = spec)$status$code, "ok")
  expect_identical(sieve_analyze("efa", data = d, estimator = "ml", ks = 1:2)$status$code, "ok")
  expect_identical(sieve_analyze("norms", data = d, meta = meta)$status$code, "ok")
  expect_identical(sieve_analyze("factor_scores", fit = lavaan::cfa("V =~ x1 + x2 + x3", d, std.lv = TRUE))$status$code, "ok")
  expect_identical(sieve_analyze("screening", data = d)$status$code, "ok")
})

test_that("a service with some of what was requested reports partial, keeping every result and reason", {
  # Legitimate partial results keep their estimates: a later invariance
  # level that fails, one coefficient of several, one value of k, one
  # construct refused, scores without score error.
  ok <- list(code = "ok", message = "Converged and admissible.")
  no <- list(code = "not_converged", message = "The model did not converge.")
  inv <- sieve_analysis_status("invariance", list(
    mi = data.frame(level = c("configural", "metric")),
    status = list(configural = ok, metric = ok, scalar = no)
  ))
  expect_identical(inv$code, "partial")
  expect_identical(inv$message, "scalar: The model did not converge.")
  expect_identical(inv$components$code, c("ok", "ok", "not_converged"))
  cv <- sieve_analysis_status("cross_validation", list(table = data.frame(x = 1), status = list(calibration = ok, validation = no)))
  expect_identical(cv$code, "partial")
  expect_identical(sieve_analysis_status("cross_validation", list(table = NULL, status = list(calibration = no, validation = no)))$code, "failed")
  rel <- sieve_analysis_status("reliability", list(
    omega = list(label = "Omega", estimate = 0.8, status = ok),
    H = list(label = "H", estimate = NA_real_, status = list(code = "failed", message = "H is undefined."))
  ))
  expect_identical(rel$code, "partial")
  expect_identical(rel$message, "H: H is undefined.")
  efa <- sieve_analysis_status("efa", data.frame(k = 1:3, status = c("ok", "ok", "not_identified"), message = c("", "", "Too many factors.")))
  expect_identical(efa$code, "partial")
  expect_identical(efa$message, "k = 3: Too many factors.")
  norms <- sieve_analysis_status("norms", list(T = list(Visual = 1:3), refused = c(Speed = "Too few scores.")))
  expect_identical(norms$code, "partial")
  fs <- sieve_analysis_status("factor_scores", list(scores = data.frame(F = c(0.1, NA)), reason = "Score error is unavailable."))
  expect_identical(fs$code, "partial")
  # Every model with a solution and some improper is inadmissible.
  improper <- list(code = "inadmissible", message = "The solution is improper.")
  expect_identical(sieve_analysis_status("invariance", list(mi = data.frame(level = "configural"), status = list(configural = ok, metric = improper)))$code, "inadmissible")
  # A status of the result's own is kept, and nothing at all is failed.
  expect_identical(sieve_analysis_status("sem", list(status = no))$code, "not_converged")
  expect_identical(sieve_analysis_status("content", NULL)$code, "failed")
})

test_that("an improper solution is reported as inadmissible, alike through every service that returns it", {
  # An improper solution has a solution (sieve_has_solution()), so a Heywood
  # case is "inadmissible", as through the CFA, not "failed", and an analysis
  # whose every model is admissible stays "ok".
  quietly <- function(expr) {
    utils::capture.output(value <- expr)
    value
  }
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  efa4 <- quietly(sieve_analyze("efa", data = d, estimator = "ml", ks = 4))
  expect_identical(efa4$status$code, "inadmissible")
  expect_match(efa4$status$message, "k = 4: The solution is improper (a Heywood case)", fixed = TRUE)
  expect_identical(nrow(as.data.frame(efa4)), 1L)
  expect_identical(quietly(sieve_analyze("efa", data = d, estimator = "ml", ks = 1:3))$status$code, "ok")
  # No number of factors to fit is a failure, with the reason.
  none <- sieve_analyze("efa", data = d[, 1:2], estimator = "ml")
  expect_identical(none$status$code, "failed")
  expect_match(none$status$message, "No number of factors was fitted", fixed = TRUE)
  # One construct whose population loading exceeds one (r12 = r13 = .8,
  # r23 = .5, so lambda1^2 = 1.28): the CFA, both halves of a cross
  # validation, and every level of the invariance ladder are improper.
  R <- matrix(c(1, .8, .8, .8, 1, .5, .8, .5, 1), 3L)
  heywood <- withr::with_seed(1, as.data.frame(matrix(stats::rnorm(1200L), 400L, 3L) %*% chol(R)))
  names(heywood) <- paste0("h", 1:3)
  hd <- cbind(heywood, withr::with_seed(1, d[sample(nrow(d), 400L, replace = TRUE), 4:9]))
  rownames(hd) <- NULL
  meta <- sieve_build_meta(hd, names(hd), list(H = paste0("h", 1:3), Textual = paste0("x", 4:6), Speed = paste0("x", 7:9)))
  expect_identical(quietly(sieve_analyze("cfa", items_df = hd, meta = meta))$status$code, "inadmissible")
  cv <- quietly(sieve_analyze("cross_validation", data = hd, meta = meta, seed = 1))
  expect_identical(cv$status$code, "inadmissible")
  expect_identical(summary(cv)$diagnostics$components$code[1:2], c("inadmissible", "inadmissible"))
  expect_true(length(summary(cv)$tables) > 0L)
  skip_on_cran()
  hd$g <- rep(c("a", "b"), each = 200L)
  inv <- quietly(sieve_analyze("invariance", data = hd, group = "g", meta = meta))
  expect_identical(inv$status$code, "inadmissible")
  expect_true(all(summary(inv)$diagnostics$components$code == "inadmissible"))
})

test_that("what was requested and is not available makes an analysis partial, with the reason", {
  # A split coefficient that is unavailable, a correlation basis replaced by
  # another, a requested interval that failed, ML scores that do not exist
  # for some respondents, and a screen that could not be computed each make
  # the analysis partial, not "ok".
  quietly <- function(expr) {
    utils::capture.output(value <- suppressWarnings(expr))
    value
  }
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  two <- sieve_build_meta(d, names(d), list(A = c("x1", "x2"), B = paste0("x", 4:6), C = paste0("x", 7:9)))
  cv <- quietly(sieve_analyze("cross_validation", data = d, meta = two, seed = 1))
  expect_identical(cv$status$code, "partial")
  expect_match(cv$status$message, "split reliability of A: not available (The one-factor model needs three or more items)", fixed = TRUE)
  # ML scores have no finite value at a uniform endpoint pattern; EBM has.
  o <- as.data.frame(lapply(d[4:6], function(v) as.integer(cut(v, stats::quantile(v, 0:4 / 4), include.lowest = TRUE))))
  fo <- lavaan::cfa("F =~ x4 + x5 + x6", o, ordered = names(o), std.lv = TRUE)
  ml <- sieve_analyze("factor_scores", fit = fo, method = "ML")
  expect_identical(ml$status$code, "partial")
  expect_match(ml$status$message, "respondents have no finite ML score", fixed = TRUE)
  expect_true(sum(is.finite(ml$result$scores[[1L]])) > 200L)
  expect_identical(sieve_analyze("factor_scores", fit = fo, method = "EBM")$status$code, "ok")
  # The screens: nothing to screen, and a distance screen without complete cases.
  expect_identical(sieve_analyze("screening", data = d[0, ])$status$code, "failed")
  incomplete <- d
  incomplete$x1 <- NA
  sc <- sieve_analyze("screening", data = incomplete)
  expect_identical(sc$status$code, "partial")
  expect_match(sc$status$message, "Too few complete cases", fixed = TRUE)
  # An AVE read from a fit that did not converge has the fit's status.
  bad <- suppressWarnings(lavaan::cfa("V =~ x1 + x2 + x3", d, std.lv = TRUE, control = list(iter.max = 1)))
  expect_identical(sieve_analyze("ave", fit = bad)$status$code, "not_converged")
  expect_identical(sieve_analyze("ave", fit = lavaan::cfa("V =~ x1 + x2 + x3", d, std.lv = TRUE))$status$code, "ok")
  # Norms with nothing to score.
  unassigned <- sieve_build_meta(d, names(d), list(Visual = paste0("x", 1:3)))
  unassigned$construct <- NA_character_
  expect_identical(sieve_analyze("norms", data = d, meta = unassigned)$status$code, "failed")
  skip_on_cran()
  # Eleven categories declared ordered: psych computes no polychoric
  # correlations, and the retention evidence uses Pearson correlations.
  d11 <- as.data.frame(lapply(d, function(v) as.integer(cut(v, 11L)) - 1L))
  rt <- quietly(sieve_analyze("retention", X = d11, types = rep("ordered", 9L), n_iter = 200L, seed = 1))
  expect_identical(rt$status$code, "partial")
  expect_match(rt$status$message, "correlation basis: Polychoric correlations could not be computed", fixed = TRUE)
  # A sub-minimum request retains the point estimate and reports the
  # interval as unavailable before dispatch; it does not simulate a tiny
  # bootstrap and then blame the usable count.
  spec <- sieve_reliability_spec(paste0("x", 4:6), rep("continuous", 3L), construct = "Textual", ci_method = "percentile", B = 10L)
  rel <- quietly(sieve_analyze("reliability", data = d[, 4:6], spec = spec, rng = list(seed = 1)))
  expect_true(is.finite(rel$result$omega$estimate))
  expect_identical(rel$status$code, "partial")
  expect_match(rel$status$message, "interval: DMAR requires at least 400", fixed = TRUE)
})

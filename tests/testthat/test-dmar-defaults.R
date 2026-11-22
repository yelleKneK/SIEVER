# DMAR 1.1.0's defaults in SIEVER: SIEVER follows DMAR's defaults unless
# there is a reason not to. Each
# block compares SIEVER with DMAR called directly with the same arguments,
# or with DMAR's own formals, so a later DMAR default that moves is caught
# here.
#
#   1. The reliability intervals: model-implied omega takes DMAR's
#      "mlr_logistic"; robust omega keeps the percentile bootstrap DMAR
#      names for it (DMAR offers it no closed form); categorical omega BCa.
#   2. The logit-scale methods on the menu, and what DMAR accepts.
#   3. Parity on the bundled nine tests, complete and with values removed
#      at random: SIEVER's omega, H, and alpha limits equal DMAR's to 1e-10.
#   4. H's engine is DMAR::reliability_H(); its interval agrees with
#      SIEVER's own delta method interval (sieve_H_delta()).
#   5. Alpha's interval at DMAR's default (ci_method = NULL resolved by
#      name).
#   6. The R API's missing default is DMAR's ("fiml"), and the report
#      drafts follow it.
#   7. B: the Bollen-Stine bootstrap and the parallel analysis at DMAR's
#      default B, with waits that follow B.
#   8. The recorded R replays what ran, with every argument named.

skip_if_not_installed("DMAR")
skip_if_not(utils::packageVersion("DMAR") >= "1.1.0")

d110_fx <- function() sieve_hs9_fixture()
d110_missing <- function(seed = 20261107L, n_each = 25L) {
  fx <- d110_fx()
  d <- fx$data
  items <- unlist(fx$theory_map, use.names = FALSE)
  withr::with_seed(seed, {
    for (it in items) d[sample(nrow(d), n_each), it] <- NA
  })
  d
}
d110_v <- function(res, term) res$value[res$term == term]
d110_limits <- function(res) c(d110_v(res, "estimate"), d110_v(res, "ci_lower"), d110_v(res, "ci_upper"))

# ---- 1. DMAR's defaults resolve construct by construct --------------------

test_that("the default intervals are DMAR's: mlr_logistic for model-implied omega, BCa for categorical omega", {
  expect_identical(sieve_reliability_recommended("continuous", "model_implied"), "mlr_logistic")
  expect_identical(sieve_reliability_recommended("mixed", "model_implied"), "mlr_logistic")
  expect_identical(sieve_reliability_recommended("categorical", "observed"), "dmar_bca")
  # DMAR's own defaults, read from its formals: the first choice.
  expect_identical(eval(formals(DMAR::reliability_omega)$ci_method)[1], "mlr_logistic")
  expect_identical(eval(formals(DMAR::reliability_omega_categorical)$ci_method)[1], "bca")
  expect_identical(sieve_reliability_engine_ci("dmar_bca"), "bca")
  # Under the FMI t reference DMAR's default is "mlr" (the rule is
  # calibrated on the scale of omega only, and refused for the logit forms).
  expect_identical(sieve_reliability_recommended("continuous", "model_implied", "missing_information"), "mlr")
  X <- d110_fx()$data[, d110_fx()$theory_map$Textual]
  expect_error(DMAR::reliability_omega(data = X, denominator = "model_implied",
    ci_method = "mlr_logistic", wald_df = "missing_information"))
  t_default <- suppressWarnings(DMAR::reliability_omega(data = X, denominator = "model_implied",
    wald_df = "missing_information"))
  expect_identical(attr(t_default, "ci_method"), "mlr")
})

test_that("robust omega keeps the percentile bootstrap: DMAR refuses mlr_logistic for it and computes no interval by default", {
  X <- d110_fx()$data[, d110_fx()$theory_map$Visual]
  expect_error(DMAR::reliability_omega(data = X, denominator = "observed", ci_method = "mlr_logistic"),
    "model_implied")
  msgs <- character(0)
  r <- withCallingHandlers(
    suppressWarnings(DMAR::reliability_omega(data = X)),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(attr(r, "ci_method"), "none")
  # DMAR names the percentile bootstrap as the interval to request.
  expect_true(any(grepl("percentile", msgs, fixed = TRUE)))
  expect_identical(sieve_reliability_recommended("continuous", "observed"), "percentile")
  sp <- sieve_reliability_spec(names(X), rep("continuous", 3), construct = "Visual", run_bootstrap = FALSE)
  expect_identical(sp$denominator, "observed")
  expect_identical(sp$ci_target, "percentile")
  expect_identical(sp$uncertainty$status, "pending")
  # The menu's first choice names DMAR's defaults, not a recommendation.
  first <- names(sieve_reliability_methods())[1]
  expect_match(first, "DMAR's defaults", fixed = TRUE)
  expect_false(grepl("Recommended:", first, fixed = TRUE))
})

# ---- 2. The logit-scale methods on the menu --------------------------------

test_that("the menu offers DMAR's logit-scale methods where DMAR accepts them", {
  codes <- unname(sieve_reliability_methods())
  expect_true(all(c("mlr_logistic", "ml_logistic", "adf_logistic", "bootstrap_se_logistic") %in% codes))
  # Every code SIEVER offers is a value DMAR's reliability_omega() takes.
  engine <- vapply(setdiff(codes, "recommended"), sieve_reliability_engine_ci, character(1))
  expect_true(all(engine %in% eval(formals(DMAR::reliability_omega)$ci_method)))
  # Model implied, complete cases: every closed form and bootstrap.
  lw <- sieve_reliability_supported("continuous", "listwise", "model_implied")
  expect_true(all(c("mlr_logistic", "ml_logistic", "adf_logistic", "likelihood", "bootstrap_se_logistic") %in% lw))
  # Under FIML DMAR refuses ADF and the profile likelihood.
  fi <- sieve_reliability_supported("continuous", "fiml", "model_implied")
  expect_true(all(c("mlr_logistic", "ml_logistic") %in% fi))
  expect_false(any(c("adf_logistic", "likelihood") %in% fi))
  dm <- d110_missing()
  X <- dm[, d110_fx()$theory_map$Speed]
  expect_error(DMAR::reliability_omega(data = X, denominator = "model_implied", missing = "fiml",
    ci_method = "adf_logistic"), "fiml")
  ok <- suppressWarnings(DMAR::reliability_omega(data = X, denominator = "model_implied", missing = "fiml",
    ci_method = "ml_logistic"))
  expect_identical(attr(ok, "ci_method"), "ml_logistic")
  # Robust omega and categorical omega: bootstraps only, the logit one included.
  expect_identical(sieve_reliability_supported("continuous", "fiml", "observed"), sieve_reliability_bootstrap_methods())
  expect_true("bootstrap_se_logistic" %in% eval(formals(DMAR::reliability_omega_categorical)$ci_method))
  # The descriptions name the scale of the limits.
  expect_match(sieve_reliability_interval("fiml", "mlr_logistic")$transformation, "logit", fixed = TRUE)
  expect_match(sieve_reliability_interval("fiml", "mlr_logistic")$se_type, "Huber-White", fixed = TRUE)
  expect_match(sieve_reliability_interval("listwise", "mlr_logistic")$se_type, "robust.sem", fixed = TRUE)
  expect_identical(sieve_reliability_interval("listwise", "ml_logistic")$label, "ML, logit scale (delta method)")
  expect_match(sieve_reliability_interval("listwise", "bootstrap_se_logistic")$transformation, "Eq. 32", fixed = TRUE)
  # The FIML menu note names both intervals it leaves out.
  m <- sieve_reliability_menu("continuous", missing = "fiml", denominator = "model_implied", selected = "likelihood")
  expect_true(m$changed)
  expect_identical(m$selected, "recommended")
  expect_match(m$note, "DMAR's default intervals", fixed = TRUE)
})

test_that("a logit-scale request under the FMI t reference keeps the normal reference and says so", {
  it <- d110_fx()$theory_map$Textual
  sp <- sieve_reliability_spec(it, rep("continuous", 3), denominator = "model_implied",
    ci_method = "mlr_logistic", wald_df = "missing_information")
  expect_match(paste(sp$notes, collapse = " "), "normal reference", fixed = TRUE)
  expect_null(sieve_reliability_call(sp)$args$wald_df)
  rec <- sieve_reliability_spec(it, rep("continuous", 3), denominator = "model_implied",
    ci_method = "recommended", wald_df = "missing_information")
  expect_identical(rec$ci_method, "mlr")
  expect_identical(sieve_reliability_call(rec)$args$wald_df, "missing_information")
})

# ---- 3. Parity: SIEVER's displayed limits are DMAR's ------------------------

test_that("omega, H and alpha limits equal DMAR called directly, on the nine tests complete and with values missing", {
  fx <- d110_fx()
  for (case in c("complete", "missing")) {
    d <- if (identical(case, "complete")) fx$data else d110_missing()
    for (cn in names(fx$theory_map)) {
      it <- fx$theory_map[[cn]]
      X <- d[, it]
      for (miss in c("fiml", "listwise")) {
        label <- paste(case, cn, miss)
        sp <- sieve_reliability_spec(it, rep("continuous", 3), construct = cn, missing = miss,
          run_bootstrap = FALSE)
        res <- suppressWarnings(sieve_reliability_construct(d, sp))
        # Model-implied omega beside robust omega, at DMAR's default interval.
        mi <- res$model_implied
        ref <- suppressWarnings(DMAR::reliability_omega(data = X, denominator = "model_implied",
          missing = miss, conf_level = 0.95))
        expect_identical(attr(ref, "ci_method"), "mlr_logistic", info = label)
        expect_equal(c(mi$estimate, mi$lower, mi$upper), d110_limits(ref), tolerance = 0, info = label)
        expect_identical(mi$uncertainty$method, "mlr_logistic", info = label)
        # Robust omega's point estimate.
        ro <- suppressWarnings(suppressMessages(DMAR::reliability_omega(data = X, missing = miss, ci_method = "none")))
        expect_identical(res$omega$estimate, d110_v(ro, "estimate"), info = label)
        # H at DMAR's defaults.
        href <- suppressWarnings(DMAR::reliability_H(data = X, missing = miss, conf_level = 0.95))
        expect_identical(attr(href, "ci_method"), "mlr_logistic", info = label)
        expect_lt(max(abs(c(res$H$estimate, res$H$lower, res$H$upper) - d110_limits(href))), 1e-10)
        expect_identical(res$H$uncertainty$ci_method, "mlr_logistic", info = label)
        # Alpha at DMAR's default: ci_method = NULL, missing named.
        aref <- suppressWarnings(DMAR::reliability_alpha(data = X, estimator = "analytic", missing = miss,
          conf_level = 0.95))
        expect_identical(attr(aref, "ci_method"), sieve_reliability_alpha_default(miss), info = label)
        expect_lt(max(abs(c(res$alpha$estimate, res$alpha$lower, res$alpha$upper) - d110_limits(aref))), 1e-10)
        expect_identical(res$alpha$uncertainty$method, sieve_reliability_alpha_default(miss), info = label)
      }
    }
  }
})

test_that("each logit-scale closed form equals DMAR's call with the same arguments", {
  d <- d110_missing()
  it <- d110_fx()$theory_map$Speed
  X <- d[, it]
  for (case in list(
    list(ci = "mlr_logistic", miss = "fiml"), list(ci = "ml_logistic", miss = "fiml"),
    list(ci = "mlr_logistic", miss = "listwise"), list(ci = "ml_logistic", miss = "listwise"),
    list(ci = "adf_logistic", miss = "listwise")
  )) {
    sp <- sieve_reliability_spec(it, rep("continuous", 3), construct = "Speed", missing = case$miss,
      ci_method = case$ci)
    expect_identical(sp$denominator, "model_implied")
    om <- suppressWarnings(sieve_reliability_omega(d, sp))
    ref <- suppressWarnings(DMAR::reliability_omega(data = X, denominator = "model_implied",
      missing = case$miss, ci_method = case$ci, conf_level = 0.95))
    expect_equal(c(om$estimate, om$lower, om$upper), d110_limits(ref), tolerance = 0, info = case$ci)
    expect_identical(om$uncertainty$status, "completed")
    # "MLR (sandwich) SE, logit-scale limits (delta method)", "ML, logit
    # scale (delta method)" and "ADF, logit scale (delta method)".
    expect_match(om$uncertainty$label, "logit[ -]scale")
    # Inside (0, 1) and asymmetric about the estimate, as logit limits are.
    expect_true(om$lower > 0 && om$upper < 1)
    expect_false(isTRUE(all.equal(om$estimate - om$lower, om$upper - om$estimate)))
  }
})

test_that("the bootstrap standard error on the logit scale is DMAR's counted bootstrap, seed for seed", {
  d <- d110_fx()$data
  it <- d110_fx()$theory_map$Textual
  sp <- sieve_reliability_spec(it, rep("continuous", 3), construct = "Textual", missing = "listwise",
    ci_method = "bootstrap_se_logistic", B = 100L, seed = 11L)
  expect_identical(sp$resampling, "dmar")
  om <- suppressWarnings(sieve_reliability_omega(d, sp))
  ref <- suppressWarnings(DMAR::reliability_omega(data = d[, it], denominator = "observed", missing = "listwise",
    ci_method = "bootstrap_se_logistic", B = 100L, seed = 11L, improper = "keep", conf_level = 0.95))
  expect_identical(om$uncertainty$status, "completed")
  expect_equal(c(om$estimate, om$lower, om$upper), d110_limits(ref), tolerance = 0)
  expect_identical(om$uncertainty$B_attempted, 100L)
  call <- sieve_reliability_call(sp, seed = 11L)
  expect_identical(call$args$ci_method, "bootstrap_se_logistic")
  expect_identical(call$args$improper, "keep")
  # Alpha's counterpart in the same run.
  al <- sieve_reliability_alpha_plan(d, sp)
  expect_identical(al$method, "bootstrap_se_logistic")
  expect_true(al$bootstrap)
})

# ---- 4. H: DMAR's engine, and its agreement with the delta method -----------

test_that("H is DMAR::reliability_H() and agrees with SIEVER's own delta method interval", {
  fx <- d110_fx()
  for (case in c("complete", "missing")) {
    d <- if (identical(case, "complete")) fx$data else d110_missing()
    for (cn in names(fx$theory_map)) {
      it <- fx$theory_map[[cn]]
      sp <- sieve_reliability_spec(it, rep("continuous", 3), construct = cn, missing = "fiml",
        ci_method = "mlr_logistic")
      H <- suppressWarnings(sieve_reliability_H(d, sp))
      expect_identical(H$uncertainty$status, "completed")
      # SIEVER's own delta method: lavaan's covariance of the standardized
      # loadings and the delta method on the logit scale (sieve_H_delta()).
      former <- sieve_H_delta(H$loadings, H$vcov, conf_level = 0.95)
      expect_equal(H$estimate, former$estimate, tolerance = 1e-12)
      expect_lt(max(abs(c(H$lower, H$upper) - c(former$lower, former$upper))), 1e-7)
    }
  }
  # Normal theory when ML was asked of omega.
  sp <- sieve_reliability_spec(fx$theory_map$Visual, rep("continuous", 3), missing = "listwise", ci_method = "ml_logistic")
  expect_identical(sieve_reliability_H_call(sp)$args$ci_method, "ml_logistic")
  H <- suppressWarnings(sieve_reliability_H(fx$data, sp))
  ref <- suppressWarnings(DMAR::reliability_H(data = fx$data[, fx$theory_map$Visual], ci_method = "ml_logistic",
    missing = "listwise", conf_level = 0.95))
  expect_equal(c(H$estimate, H$lower, H$upper), d110_limits(ref), tolerance = 0)
})

test_that("H of ordered items is DMAR's delta method on the WLSMV fit", {
  d <- d110_fx()$data
  it <- d110_fx()$theory_map$Textual
  for (i in it) d[[i]] <- as.integer(cut(d[[i]], stats::quantile(d[[i]], c(0, .2, .5, .8, 1)), include.lowest = TRUE))
  sp <- sieve_reliability_spec(it, rep("ordered", 3), construct = "Textual", run_bootstrap = FALSE)
  call <- sieve_reliability_H_call(sp)
  expect_identical(call$input, "fit")
  expect_identical(call$args$ci_method, "delta_logistic")
  H <- suppressWarnings(sieve_reliability_H(d, sp))
  m <- sieve_reliability_H_model(sp)
  fit <- lavaan::cfa(m$model, data = d[stats::complete.cases(d[, it]), it], std.lv = TRUE,
    ordered = it, estimator = "WLSMV")
  ref <- DMAR::reliability_H(fit = fit, ci_method = "delta_logistic", conf_level = 0.95)
  expect_lt(max(abs(c(H$estimate, H$lower, H$upper) - d110_limits(ref))), 1e-10)
  expect_match(H$uncertainty$label, "Delta method, logit scale", fixed = TRUE)
})

test_that("a refusal of DMAR's H is read by class and reported with its reason", {
  sp <- sieve_reliability_spec(d110_fx()$theory_map$Visual, rep("continuous", 3), missing = "fiml",
    run_bootstrap = FALSE)
  test_frame <- environment()
  local_mocked_bindings(
    sieve_dmar_boot_run = function(call, progress = NULL, fun = NULL) {
      cond <- structure(class = c("dmar_no_maximum", "dmar_refusal", "error", "condition"),
        list(message = "words that do not matter", call = NULL, reason = "no maximum"))
      list(ok = FALSE, value = NULL, condition = cond, error = conditionMessage(cond),
        error_class = class(cond), warnings = character(0), messages = character(0), seconds = 0)
    },
    .env = test_frame
  )
  H <- suppressWarnings(sieve_reliability_H(d110_fx()$data, sp))
  expect_true(is.na(H$estimate))
  expect_identical(H$uncertainty$status, "failed")
  expect_match(H$uncertainty$reason, "DMAR refused H (no maximum)", fixed = TRUE)
  expect_true("dmar_refusal" %in% H$uncertainty$condition_class)
})

# ---- 5. Alpha at DMAR's default ---------------------------------------------

test_that("alpha takes DMAR's default interval, by name, under DMAR's defaults and while omega's bootstrap waits", {
  d <- d110_fx()$data
  it <- d110_fx()$theory_map$Visual
  X <- d[, it]
  # DMAR's resolution of ci_method = NULL with missing named.
  for (miss in c("fiml", "listwise")) {
    ref <- suppressWarnings(DMAR::reliability_alpha(data = X, estimator = "analytic", missing = miss))
    expect_identical(attr(ref, "ci_method"), sieve_reliability_alpha_default(miss))
  }
  dm <- d110_missing()
  ref <- suppressWarnings(DMAR::reliability_alpha(data = dm[, it], estimator = "analytic", missing = "fiml"))
  expect_identical(attr(ref, "ci_method"), "ml")
  # Pending robust omega bootstrap: DMAR's default for alpha.
  pend <- sieve_reliability_spec(it, rep("continuous", 3), missing = "listwise", run_bootstrap = FALSE)
  expect_identical(sieve_reliability_alpha_plan(d, pend)$method, "bonett")
  # Model implied omega under DMAR's defaults.
  mi <- sieve_reliability_spec(it, rep("continuous", 3), missing = "listwise", denominator = "model_implied")
  expect_identical(sieve_reliability_alpha_plan(d, mi)$method, "bonett")
  mi_f <- sieve_reliability_spec(it, rep("continuous", 3), missing = "fiml", denominator = "model_implied")
  expect_identical(sieve_reliability_alpha_plan(d, mi_f)$method, "ml")
  # An interval the user chose keeps its counterpart.
  ch <- sieve_reliability_spec(it, rep("continuous", 3), missing = "listwise", ci_method = "mlr")
  expect_identical(sieve_reliability_alpha_plan(d, ch)$method, "adf")
  ch2 <- sieve_reliability_spec(it, rep("continuous", 3), missing = "listwise", ci_method = "mlr_logistic")
  expect_identical(sieve_reliability_alpha_plan(d, ch2)$method, "adf_logistic")
  ch3 <- sieve_reliability_spec(it, rep("continuous", 3), missing = "fiml", ci_method = "mlr_logistic")
  expect_identical(sieve_reliability_alpha_plan(d, ch3)$method, "ml_logistic")
  # The recorded call names the method.
  expect_match(sieve_reliability_alpha_plan(d, pend)$out$code, "ci_method = \"bonett\"", fixed = TRUE)
})

# ---- 6. The R API's missing default, and the drafts that follow it ----------

test_that("sieve_reliability_spec() defaults to DMAR's missing data treatment, and the drafts follow", {
  expect_identical(eval(formals(sieve_reliability_spec)$missing)[1], "fiml")
  expect_identical(eval(formals(DMAR::reliability_omega)$missing)[1], "fiml")
  expect_identical(eval(formals(DMAR::reliability_H)$missing)[1], "fiml")
  sp <- sieve_reliability_spec(c("a", "b", "c"), rep("continuous", 3))
  expect_identical(sp$missing, "fiml")
  expect_identical(sieve_reliability_call(sp)$args$missing, "fiml")
  expect_identical(formals(sieve_draft_reliability)$missing, "fiml")
  expect_identical(formals(sieve_draft_reliability_specs)$missing, "fiml")
  expect_identical(formals(sieve_draft_bootstrap_ids)$missing, "fiml")
  # The draft's companion and the split-half intervals take DMAR's default.
  fx <- d110_fx()
  rob <- sieve_reliability_spec(fx$theory_map$Speed, rep("continuous", 3), run_bootstrap = FALSE)
  mi <- suppressWarnings(sieve_reliability_model_implied(fx$data, rob))
  expect_identical(mi$uncertainty$method, "mlr_logistic")
})

# ---- 7. B at DMAR's defaults, and waits that follow it -----------------------

test_that("the Bollen-Stine bootstrap defaults to DMAR's B, up to 10000, and its stated wait follows B", {
  expect_identical(as.integer(formals(sieve_bollen_stine)$B), as.integer(formals(DMAR::sem_exact_fit_bootstrap)$B))
  expect_identical(as.integer(formals(sieve_bollen_stine_plan)$B), 1000L)
  ui <- paste(as.character(mod_cfa_ui("cfa")), collapse = " ")
  tag <- regmatches(ui, regexpr("<input[^>]*id=\"cfa-bs_B\"[^>]*>", ui))
  expect_match(tag, "value=\"1000\"", fixed = TRUE)
  expect_match(tag, "max=\"10000\"", fixed = TRUE)
  st <- siever_test_study(sieve_hs9_fixture(), confirmed = TRUE, labels = TRUE, theory = TRUE)
  test_frame <- environment()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE)
    expect_identical(bs_settings()$B, 1000L)
    at_default <- bs_wait()
    session$setInputs(bs_B = 250)
    expect_identical(bs_settings()$B, 250L)
    expect_equal(at_default / bs_wait(), 4, tolerance = 1e-12)
    session$setInputs(bs_B = 50000)
    expect_identical(bs_settings()$B, 10000L)
  })
})

test_that("the parallel analysis defaults to DMAR's B, percentile and reference, and its stated wait follows B", {
  expect_identical(as.integer(formals(sieve_parallel_analysis)$n_iter), as.integer(formals(DMAR::parallel_analysis)$B))
  expect_identical(as.integer(formals(sieve_retention_evidence)$n_iter), 1000L)
  expect_identical(eval(formals(sieve_pa_select)$reference)[1], "simulated")
  expect_identical(formals(DMAR::parallel_analysis)$reference, "normal")
  ui <- paste(as.character(mod_retention_ui("r")), collapse = " ")
  tag <- regmatches(ui, regexpr("<input[^>]*id=\"r-n_iter\"[^>]*>", ui))
  expect_match(tag, "value=\"1000\"", fixed = TRUE)
  expect_match(ui, "<option value=\"simulated\" selected>", fixed = TRUE)
  X <- d110_fx()$data[, d110_fx()$item_cols]
  basis <- sieve_retention_cor(X, rep("continuous", 9), "pearson", missing = "listwise")
  call <- sieve_pa_dmar_calls(X, basis, 1000L, 5L)
  for (cl in call$calls) {
    expect_identical(cl$args$B, 1000L)
    expect_identical(cl$args$percentile, formals(DMAR::parallel_analysis)$percentile)
    expect_identical(cl$args$conf_level, formals(DMAR::parallel_analysis)$conf_level)
  }
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  p1000 <- sieve_pa_plan(basis, 1000)
  p200 <- sieve_pa_plan(basis, 200)
  expect_identical(p1000$draws, 5L * p200$draws)
  expect_gt(p1000$seconds, p200$seconds)
  expect_match(sieve_pa_plan(basis, 100)$refused, "at least 200", fixed = TRUE)
})

# ---- 8. The recorded R replays what ran --------------------------------------

test_that("the recorded omega, H and alpha calls name every argument and replay the limits shown", {
  d <- d110_missing()
  it <- d110_fx()$theory_map$Visual
  sp <- sieve_reliability_spec(it, rep("continuous", 3), construct = "Visual", run_bootstrap = FALSE)
  res <- suppressWarnings(sieve_reliability_construct(d, sp))
  code <- c(res$model_implied$code, res$H$code, res$alpha$code)
  expect_false(any(grepl("SIEVER:::", code, fixed = TRUE)))
  expect_false(any(grepl("h_interval", code, fixed = TRUE)))
  expect_true(any(grepl("ci_method = \"mlr_logistic\", denominator = \"model_implied\", missing = \"fiml\", conf_level = 0.95", code, fixed = TRUE)))
  expect_true(any(grepl("DMAR::reliability_H(data = h_data, ci_method = \"mlr_logistic\", missing = \"fiml\", conf_level = 0.95)", code, fixed = TRUE)))
  expect_true(any(grepl("ci_method = \"ml\"", res$alpha$code, fixed = TRUE)))
  env <- new.env(parent = globalenv())
  env$items <- d
  suppressWarnings(eval(parse(text = code), envir = env))
  expect_identical(d110_limits(env$omega_model_implied_Visual), c(res$model_implied$estimate, res$model_implied$lower, res$model_implied$upper))
  expect_identical(d110_limits(env$H_Visual), c(res$H$estimate, res$H$lower, res$H$upper))
  expect_identical(d110_limits(env$alpha_Visual), c(res$alpha$estimate, res$alpha$lower, res$alpha$upper))
})

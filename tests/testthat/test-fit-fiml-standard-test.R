# SIEVER's own ML fits with FIML ask lavaan for the standard test alone.
# lavaan 0.7-3 adds Browne's residual based statistic to every ML fit by
# default; SIEVER never reports it, and under FIML it takes most of the time
# of the fit and of every fit measure call (DMAR's H1 check among them).

fst_items <- function() {
  items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  # A few missing values, so FIML differs from the complete cases.
  items$x1[c(3L, 17L, 40L)] <- NA
  items$x5[c(8L, 90L)] <- NA
  items$x9[c(5L, 150L, 200L)] <- NA
  items
}

fst_meta <- function(items) {
  data.frame(
    item = names(items), label = names(items),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = "continuous", status = "active",
    reverse = FALSE, stringsAsFactors = FALSE
  )
}

test_that("ML with FIML asks for the standard test alone; MLR, complete-case ML, WLSMV and a chosen test keep theirs", {
  items <- fst_items()
  meta <- fst_meta(items)
  spec <- sieve_cfa_spec(meta)
  expect_identical(spec$estimator, "ML")
  expect_identical(spec$missing, "fiml")
  expect_identical(sieve_spec_default_test(spec), "standard")
  expect_identical(sieve_spec_args(spec)$test, "standard")
  # The standard test alone, not lavaan's default list,
  # c("standard", "browne.residual.nt.model").
  run <- sieve_run_cfa(spec, items)
  expect_identical(run$status$code, "ok")
  expect_identical(lavaan::lavInspect(run$fit, "options")$test, "standard")
  # The recorded R fits the same model with the same test, and says why.
  code <- sieve_cfa_code(spec)
  expect_true(any(grepl('missing = "fiml", test = "standard"', code, fixed = TRUE)))
  expect_true(any(grepl("Browne's residual-based test, which SIEVER does not report", code, fixed = TRUE)))
  env <- new.env(parent = globalenv())
  env$items <- items
  expect_no_error(suppressWarnings(eval(parse(text = code), env)))
  expect_identical(lavaan::lavInspect(env$cfa_fit, "options")$test, "standard")
  expect_equal(lavaan::coef(env$cfa_fit), lavaan::coef(run$fit), tolerance = 1e-10)

  # MLR keeps lavaan's default list (an explicit one changes its scaled
  # statistic), as do ML on the complete cases and WLSMV.
  mlr <- sieve_cfa_spec(meta, estimator = "mlr")
  expect_null(sieve_spec_args(mlr)$test)
  expect_false(any(grepl("test = ", sieve_cfa_code(mlr), fixed = TRUE)))
  expect_null(sieve_spec_args(sieve_cfa_spec(meta, estimator = "ml"))$test)
  ordered <- meta
  ordered$type <- "ordered"
  expect_null(sieve_spec_args(sieve_cfa_spec(ordered))$test)
  # A test the researcher sets is kept, and no second one is written.
  chosen <- sieve_spec_engine_args(spec, list(test = "satorra.bentler"))
  expect_null(sieve_spec_default_test(chosen))
  expect_identical(sieve_spec_args(chosen)$test, "satorra.bentler")
  chosen_code <- sieve_cfa_code(chosen)
  expect_identical(sum(lengths(regmatches(chosen_code, gregexpr("test = ", chosen_code, fixed = TRUE)))), 1L)

  # The structural model's recorded R carries the same argument.
  sem_spec <- list(
    model = "Visual =~ x1 + x2 + x3\nSpeed =~ x7 + x8 + x9\nSpeed ~ Visual",
    observed = paste0("x", c(1:3, 7:9)), estimator = "ML", ordered = NULL,
    missing = "fiml", out_map = character(0), pred_map = character(0)
  )
  sem_code <- sieve_sem_code(sem_spec)
  expect_true(any(grepl('std.lv = TRUE, missing = "fiml", test = "standard")', sem_code, fixed = TRUE)))
  sem_spec$estimator <- "MLR"
  expect_false(any(grepl("test = ", sieve_sem_code(sem_spec), fixed = TRUE)))

  # So does the one-factor model behind H.
  sp <- sieve_reliability_spec(paste0("x", 1:3), rep("continuous", 3), "Visual",
    missing = "fiml", ci_method = "ml")
  expect_true(any(grepl('missing = "fiml", test = "standard"', sieve_reliability_H_code(sp), fixed = TRUE)))
})

test_that("every number SIEVER reports from an ML FIML fit is the same with the standard test alone", {
  items <- fst_items()
  spec <- sieve_cfa_spec(fst_meta(items))
  args <- sieve_spec_args(spec)
  lavaan_default <- args
  lavaan_default$test <- NULL
  standard <- do.call(lavaan::cfa, c(args, list(data = items)))
  default <- do.call(lavaan::cfa, c(lavaan_default, list(data = items)))
  expect_true("browne.residual.nt.model" %in% lavaan::lavInspect(default, "options")$test)
  expect_identical(lavaan::lavInspect(standard, "options")$test, "standard")

  # Estimates, standard errors, intervals and the standardized solution.
  pe <- function(f) lavaan::parameterEstimates(f, standardized = TRUE)
  expect_equal(pe(standard), pe(default), tolerance = 1e-12)
  expect_equal(lavaan::standardizedSolution(standard), lavaan::standardizedSolution(default), tolerance = 1e-12)

  # The fit measures SIEVER reads, in every family, with the reason for
  # each value it does not report, and the sentences beside them.
  fs <- sieve_fit_families(standard)
  fd <- sieve_fit_families(default)
  for (family in c("standard", "scaled", "robust")) {
    expect_equal(fs$families[[family]]$values, fd$families[[family]]$values, tolerance = 1e-12)
    expect_identical(fs$families[[family]]$missing, fd$families[[family]]$missing)
    expect_identical(fs$families[[family]]$available, fd$families[[family]]$available)
    expect_identical(fs$families[[family]]$reason, fd$families[[family]]$reason)
  }
  for (other in c("srmr", "aic", "bic")) {
    expect_equal(fs[[other]]$value, fd[[other]]$value, tolerance = 1e-12)
  }
  shown_s <- sieve_fit_select(fs)
  shown_d <- sieve_fit_select(fd)
  expect_equal(c(shown_s), c(shown_d), tolerance = 1e-12)
  expect_identical(attr(shown_s, "family"), attr(shown_d, "family"))
  expect_identical(sieve_fit_note(shown_s), sieve_fit_note(shown_d))

  # DMAR's table of fit measures, and its H1 decision, are the same.
  dmar <- function(f) {
    x <- DMAR::sem_fit_measures(f, conf_level = sieve_default_rmsea_conf_level())
    stats::setNames(suppressWarnings(as.numeric(x$value)), x$term)
  }
  expect_equal(dmar(standard), dmar(default), tolerance = 1e-12)
  expect_identical(sieve_h1_refusal(standard), sieve_h1_refusal(default))
})

test_that("an ML FIML fit whose observed information cannot be inverted is fit again with the expected information, with the standard test", {
  items <- fst_items()
  spec <- sieve_cfa_spec(fst_meta(items))
  args <- sieve_spec_args(spec)
  fit <- do.call(lavaan::cfa, c(args, list(data = items)))
  # Stand in for a solution whose observed information is singular: the
  # standard errors are available only under the expected information.
  local_mocked_bindings(sieve_fit_status = function(fit, ...) {
    list(se_available = identical(lavaan::lavInspect(fit, "options")$information[1L], "expected"))
  })
  # The standard test SIEVER passes does not stop the refit.
  retried <- sieve_expected_information_retry(fit, args, items, lavaan::cfa)
  expect_false(is.null(retried))
  expect_identical(lavaan::lavInspect(retried$fit, "options")$information[1L], "expected")
  expect_identical(lavaan::lavInspect(retried$fit, "options")$test, "standard")
  expect_identical(retried$args$test, "standard")
  # Any other test, or a chosen standard error, still stops it.
  expect_null(sieve_expected_information_retry(fit, utils::modifyList(args, list(test = "satorra.bentler")), items, lavaan::cfa))
  expect_null(sieve_expected_information_retry(fit, utils::modifyList(args, list(se = "robust.huber.white")), items, lavaan::cfa))
})

test_that("complete data with as many items as respondents stay refused for H1 with the standard test alone", {
  # The H1 check is not skipped when nothing is missing: DMAR refuses a
  # complete-data FIML fit whose sample covariance matrix is singular, and
  # the standard test does not change that decision.
  withr::local_seed(1)
  n <- 30L
  p <- 40L
  eta <- stats::rnorm(n)
  X <- as.data.frame(outer(eta, rep(0.7, p)) + matrix(stats::rnorm(n * p, sd = sqrt(0.51)), n, p))
  names(X) <- sprintf("i%02d", seq_len(p))
  meta <- data.frame(
    item = names(X), label = names(X), construct = "One", facet = NA_character_,
    type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
  spec <- sieve_cfa_spec(meta)
  run <- suppressWarnings(sieve_run_cfa(spec, X))
  skip_if(is.null(run$fit), "the one-factor model of 40 items on 30 respondents did not converge here")
  expect_identical(lavaan::lavInspect(run$fit, "options")$test, "standard")
  refusal <- sieve_h1_refusal(run$fit)
  expect_false(is.null(refusal))
  families <- sieve_fit_families(run$fit)
  expect_false(families$families$standard$available)
  expect_identical(families$families$standard$reason, refusal)
})

test_that("SIEVER's ML FIML fit and the same fit with lavaan's default tests have one signature, and a refit from it asks for the standard test", {
  items <- fst_items()
  spec <- sieve_cfa_spec(fst_meta(items))
  args <- sieve_spec_args(spec)
  mine <- do.call(lavaan::cfa, c(args, list(data = items)))
  default_args <- args
  default_args$test <- NULL
  default <- do.call(lavaan::cfa, c(default_args, list(data = items)))
  # "standard" and c("standard", "browne.residual.nt.model") give one
  # signature, since nothing either reports differs, so the bifactor
  # bootstrap's check that SIEVER's fit is the hand fit's specification
  # passes.
  expect_identical(sieve_lavaan_signature(mine), sieve_lavaan_signature(default))
  expect_identical(sieve_lavaan_signature(mine)$test, "standard")
  # A refit written from the signature (the bootstrap's draws and recorded
  # R) asks for the standard test too, with the comment that says why.
  sig_args <- sieve_lavaan_signature_args(sieve_lavaan_signature(mine))
  expect_identical(sig_args$estimator, "ML")
  expect_identical(sig_args$missing, "fiml")
  expect_identical(sig_args$test, "standard")
  refit <- do.call(lavaan::cfa, c(list(model = spec$model, data = items), sig_args))
  expect_identical(lavaan::lavInspect(refit, "options")$test, "standard")
  expect_identical(sieve_lavaan_signature(refit), sieve_lavaan_signature(mine))
  expect_match(sieve_signature_test_comment(sig_args), "^# test = \"standard\": the normal-theory model test, as the point estimate was fit")
  expect_false(grepl("SIEVER", sieve_signature_test_comment(sig_args), fixed = TRUE))
  # ML on the complete cases and MLR write no test.
  listwise <- sieve_lavaan_signature_args(sieve_lavaan_signature(lavaan::cfa(spec$model, data = items, std.lv = TRUE)))
  expect_null(listwise$test)
  expect_null(sieve_signature_test_comment(listwise))
  mlr <- lavaan::cfa(spec$model, data = items, std.lv = TRUE, estimator = "MLR", missing = "fiml")
  expect_null(sieve_lavaan_signature_args(sieve_lavaan_signature(mlr))$test)
})

test_that("the ML FIML memory refusal names what SIEVER's fit computes, not Browne's test", {
  refused <- sieve_mlr_guard(303, "ML", missing = "fiml", n = 500, limit = 32e9)
  # The refusal names the corrected indices SIEVER's fit computes, not
  # Browne's residual based test, which SIEVER does not ask lavaan for.
  expect_match(refused, "under ML with FIML, the corrected fit indices lavaan reports for a FIML fit (Zhang & Savalei, 2023)", fixed = TRUE)
  expect_false(grepl("Browne", refused, fixed = TRUE))
  # The need stays MLR's, and so does the rate class.
  expect_identical(sieve_ml_bytes(303, "ML", missing = "fiml"), sieve_ml_bytes(303, "MLR"))
  expect_identical(sieve_demand_class("ML", "fiml"), "MLR")
})

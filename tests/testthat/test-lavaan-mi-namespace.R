# lavaan.mi in the user's session. lavaan.mi registers its summary(),
# coef(), vcov(), nobs() and anova() methods where they dispatch only when
# lavaan is attached as its namespace loads. A launch that loads that
# namespace (a missing-package notice asking requireNamespace()), or an
# attach of lavaan and lavaan.mi in sieve_mi_model() without a detach,
# would leave a user's own lavaan.mi fit giving one coef() column per
# imputation and no vcov(), even after library(lavaan.mi). The tests below
# check that SIEVER does neither.

mi_namespace_data <- function() {
  raw <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  d <- as.data.frame(lapply(raw, function(x) as.integer(cut(x, 5))))
  d[1:10, 1] <- NA_integer_
  d
}

test_that("after SIEVER's launch check and sieve_mi_model(), a user's lavaan.mi fits behave as without SIEVER", {
  skip_on_cran()
  skip_if_not(sieve_installed("mice"), "mice is not installed")
  skip_if_not(sieve_installed("lavaan.mi"), "lavaan.mi is not installed")
  got <- mi_fresh_siever(function(d) {
    out <- list()
    optional <- utils::getFromNamespace("sieve_optional_packages", "SIEVER")()$package
    before <- loadedNamespaces()
    missing <- utils::getFromNamespace("sieve_missing_packages", "SIEVER")()
    out$missing <- missing
    out$loaded_by_check <- setdiff(loadedNamespaces(), before)
    search_before <- search()
    analysis <- SIEVER::sieve_mi_model("Textual =~ x4 + x5 + x6", d, ordered = names(d), seed = 1L)
    out$search_kept <- identical(search(), search_before)
    out$lavaan_mi_loaded <- "lavaan.mi" %in% loadedNamespaces()
    # The user's own work afterwards.
    suppressPackageStartupMessages(library(lavaan.mi))
    raw <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
    raw[1:20, 1] <- NA
    imputed <- mice::mice(raw, m = 5, printFlag = FALSE, seed = 2L)
    fit <- lavaan.mi::cfa.mi("Textual =~ x4 + x5 + x6", data = mice::complete(imputed, "all"))
    out$user_coef <- coef(fit)
    out$user_vcov_dim <- dim(vcov(fit))
    out$user_summary <- utils::capture.output(summary(fit))[1L]
    out$siever_coef <- coef(analysis$fit)
    out$siever_free <- sum(analysis$fit@ParTable$free > 0L)
    out$dispatches <- utils::getFromNamespace("sieve_lavaan_mi_dispatches", "SIEVER")()
    out
  }, list(d = mi_namespace_data()))
  # The check loaded no package: neither lavaan.mi nor any other.
  expect_identical(got$loaded_by_check, character(0))
  expect_false(any(c("mice", "lavaan.mi") %in% got$missing))
  expect_true(got$search_kept)
  expect_true(got$lavaan_mi_loaded)
  expect_true(got$dispatches)
  # Pooled estimates, one per free parameter, and their covariance matrix.
  expect_null(dim(got$user_coef))
  expect_length(got$user_coef, 6L)
  expect_identical(got$user_vcov_dim, c(6L, 6L))
  expect_match(got$user_summary, "lavaan.mi object", fixed = TRUE)
  expect_null(dim(got$siever_coef))
  expect_length(got$siever_coef, got$siever_free)
})

test_that("the replay check loads lavaan.mi so that its methods dispatch, leaving the search path as it was", {
  skip_on_cran()
  skip_if_not(sieve_installed("mice"), "mice is not installed")
  skip_if_not(sieve_installed("lavaan.mi"), "lavaan.mi is not installed")
  got <- mi_fresh_siever(function() {
    out <- list()
    store <- list(engine_steps = list(`1` = "k"),
      engine_identity = list(k = list(lavaan.mi = list(package = "lavaan.mi", id = "recorded elsewhere"))))
    search_before <- search()
    out$loaded_before <- "lavaan.mi" %in% loadedNamespaces()
    suppressWarnings(SIEVER::sieve_replay_engine_check(store, 1L))
    out$loaded_after <- "lavaan.mi" %in% loadedNamespaces()
    out$search_kept <- identical(search(), search_before)
    suppressPackageStartupMessages(library(lavaan.mi))
    raw <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
    raw[1:20, 1] <- NA
    imputed <- mice::mice(raw, m = 5, printFlag = FALSE, seed = 2L)
    fit <- lavaan.mi::cfa.mi("Textual =~ x4 + x5 + x6", data = mice::complete(imputed, "all"))
    out$user_coef <- coef(fit)
    out$user_vcov_dim <- dim(vcov(fit))
    out
  })
  expect_false(got$loaded_before)
  expect_true(got$loaded_after)
  expect_true(got$search_kept)
  expect_null(dim(got$user_coef))
  expect_length(got$user_coef, 6L)
  expect_identical(got$user_vcov_dim, c(6L, 6L))
})

test_that("the dispatch check tells a session whose lavaan.mi loaded before lavaan was attached", {
  skip_on_cran()
  skip_if_not(sieve_installed("lavaan.mi"), "lavaan.mi is not installed")
  check <- sieve_lavaan_mi_dispatches
  environment(check) <- baseenv()
  probe <- function(check, early) {
    # A launch check that loads the namespace while lavaan is not
    # attached.
    if (early) requireNamespace("lavaan.mi", quietly = TRUE)
    for (package in c("lavaan", "lavaan.mi")) {
      if (!paste0("package:", package) %in% search()) suppressPackageStartupMessages(attachNamespace(package))
    }
    check()
  }
  environment(probe) <- globalenv()
  expect_false(callr::r(probe, list(check = check, early = TRUE), libpath = .libPaths(), user_profile = FALSE))
  expect_true(callr::r(probe, list(check = check, early = FALSE), libpath = .libPaths(), user_profile = FALSE))
})

test_that("sieve_mi_model() restores the search path when it fails, and warns when lavaan.mi's methods cannot dispatch", {
  skip_on_cran()
  skip_if_not(sieve_installed("mice"), "mice is not installed")
  skip_if_not(sieve_installed("lavaan.mi"), "lavaan.mi is not installed")
  d <- mi_namespace_data()
  before <- search()
  # A failure after lavaan and lavaan.mi were attached.
  local_mocked_bindings(sieve_lavaan_mi_dispatches = function() stop("stopped after attaching"))
  expect_error(sieve_mi_model("Textual =~ x4 + x5 + x6", d, ordered = names(d), seed = 1L),
    "stopped after attaching", fixed = TRUE)
  expect_identical(search(), before)
  # A refused request attaches nothing.
  expect_error(sieve_mi_model("Textual =~ x4 + x5 + x6", d[c("x5", "x6")], ordered = "x5", seed = 1L),
    "The imputation data are missing: x4", fixed = TRUE)
  expect_identical(search(), before)
  local_mocked_bindings(sieve_lavaan_mi_dispatches = function() FALSE)
  expect_warning(
    analysis <- sieve_mi_model("Textual =~ x4 + x5 + x6", d, ordered = names(d), seed = 1L),
    "lavaan.mi was loaded in this R session before lavaan was attached", fixed = TRUE
  )
  expect_identical(search(), before)
  expect_true(all(is.finite(analysis$estimates$est)))
})

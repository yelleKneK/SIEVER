# Checks that cross tabs: factor score determinacy, residual correlations,
# labelled columns, and facets as the Items tab's factors.

skip_if_not_installed("lavaan")

test_that("the determinacy reported is that of the scores requested", {
  d <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6", data = d, std.lv = TRUE)
  b <- sieve_factor_score_record(fit, method = "Bartlett", data = d)
  r <- sieve_factor_score_record(fit, method = "regression", data = d)
  expect_equal(unname(b$determinacy), unname(lavaan::lavInspect(fit, "fs_determinacy_Bartlett")), tolerance = 1e-12)
  expect_equal(unname(r$determinacy), unname(lavaan::lavInspect(fit, "fs.det")), tolerance = 1e-12)
  expect_identical(b$determinacy_method, "Bartlett-score")
  expect_identical(r$determinacy_method, "Regression-score")
  expect_match(b$determinacy_target, "Bartlett scores", fixed = TRUE)
  expect_false(isTRUE(all.equal(unname(b$determinacy), unname(r$determinacy))))
})

test_that("the Items tab and the Reliability tab report the same residual correlations, quickly", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
  spec <- sieve_reliability_spec(names(d), rep("continuous", 6), construct = "A", ci_method = "none")
  rel <- sieve_reliability_residuals(d, spec)
  items <- sieve_single_factor_residual(d)
  expect_equal(unname(items$matrix), unname(rel$matrix), tolerance = 1e-6)
  expect_equal(abs(items$value), abs(rel$largest), tolerance = 1e-6)
  # The residuals are read without their z statistics, whose asymptotic
  # covariance matrix of the moments takes hours for 68 items.
  set.seed(7)
  f <- stats::rnorm(400)
  X <- as.data.frame(sapply(1:30, function(j) 0.6 * f + stats::rnorm(400, sd = 0.8)))
  spec30 <- sieve_reliability_spec(names(X), rep("continuous", 30), construct = "B", ci_method = "none")
  took <- system.time(sieve_reliability_residuals(X, spec30))[["elapsed"]]
  if (siever_timing_checks()) expect_lt(took, 20)
  expect_match(paste(sieve_reliability_residual_code(spec30), collapse = "\n"), "zstat = FALSE", fixed = TRUE)
})

test_that("labelled columns are read as their codes once haven is loaded (any session that read a .sav file)", {
  skip_if_not_installed("haven")
  loadNamespace("haven")
  v <- haven::labelled(c(1, 2, NA, 3), labels = c(low = 1, high = 3))
  expect_identical(sieve_plain_values(v), c(1, 2, NA, 3))
  expect_identical(sieve_values_key(v), sieve_values_key(c(1, 2, NA, 3)))
})

test_that("a one-construct study split into facets has the facets as the Items tab's factors", {
  skip_if_not_installed("psych")
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- data.frame(
    item = names(d), label = names(d), construct = "Ability",
    facet = rep(c("Visual", "Textual", "Speed"), each = 3L),
    type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
  rng <- list(seed = 11L, kind = RNGkind())
  diag <- sieve_item_diagnostics(d, meta, rng = rng)
  expect_identical(diag$k, 3L)
  expect_identical(diag$factor_level, "facet")
  expect_identical(diag$items$factor_group, meta$facet)
  # The target loading is the item's loading on its facet's factor, as
  # DMAR::fa_exploratory gives it under the same seed (psych::fa on the
  # same matrix agrees within the optimizers' tolerance).
  dm <- DMAR::fa_exploratory(S = stats::cor(d), N = nrow(d), nfactors = 3, estimator = "ML",
    rotation = "oblimin", conf_level = 0.95, seed = 11L)
  target_of <- function(P) vapply(seq_len(9), function(i) {
    own <- which(meta$facet == meta$facet[i])
    f <- which.max(colMeans(abs(P[own, , drop = FALSE])))
    P[i, f]
  }, numeric(1))
  target <- target_of(sieve_efa_dmar_solution(dm)$loadings)
  expect_equal(diag$items$loading_target, unname(target), tolerance = 1e-10)
  fa <- withr::with_seed(11L, psych::fa(stats::cor(d), nfactors = 3, n.obs = nrow(d), fm = "ml", rotate = "oblimin"))
  expect_lt(max(abs(diag$items$loading_target - unname(target_of(unclass(fa$loadings))))), 1e-4)
  expect_true(all(diag$items$off_construct[is.finite(diag$items$loading_off_max)] %in% meta$facet))
  # Reliability stays by construct.
  expect_identical(nrow(diag$constructs), 1L)
  expect_match(paste(sieve_item_diagnostics_code(diag, NULL, meta$construct, meta$item), collapse = "\n"),
    "nfactors = 3", fixed = TRUE)
  # Without facets: one factor, the construct.
  meta$facet <- NA_character_
  one <- sieve_item_diagnostics(d, meta, rng = rng)
  expect_identical(one$k, 1L)
  expect_identical(one$factor_level, "construct")
  expect_true(all(is.na(one$items$loading_off_max)))
})

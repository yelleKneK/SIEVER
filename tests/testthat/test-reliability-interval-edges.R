# The Reliability tab's intervals: a negative alpha just below zero keeps
# DMAR's informative interval, worded from its own limits; an ordered
# construct's model-implied omega says why it has no interval rather than
# "none requested"; and the help and the articles say which rows of the
# compact table have an interval before the bootstrap is run (every
# construct of continuous items), not that every row does.

skip_if_not_installed("lavaan")

rf_hs <- function() lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]

# Three items whose sample covariance matrix is exactly S (correlations
# -.06, .02 and .01), so alpha is -0.0306 on every platform.
rf_negative_alpha_data <- function() {
  S <- matrix(c(1, -0.06, 0.02, -0.06, 1, 0.01, 0.02, 0.01, 1), 3)
  Z <- withr::with_seed(5, matrix(stats::rnorm(900), 300))
  Z <- scale(Z, scale = FALSE)
  Z <- Z %*% solve(chol(stats::cov(Z))) %*% chol(S)
  X <- as.data.frame(Z)
  names(X) <- c("a", "b", "c")
  X
}

test_that("an alpha just below zero keeps DMAR's clamped interval, with a reason worded from its limits", {
  X <- rf_negative_alpha_data()
  spec <- sieve_reliability_spec(c("a", "b", "c"), rep("continuous", 3), construct = "C", missing = "listwise",
    run_bootstrap = FALSE)
  r <- sieve_reliability_construct(X, spec, model_implied = FALSE)
  a <- r$alpha
  # DMAR warns that the estimate is negative; the test reads its limits.
  engine <- suppressWarnings(DMAR::reliability_alpha(data = X, missing = "listwise"))
  expect_equal(a$estimate, engine$value[engine$term == "estimate"], tolerance = 1e-12)
  expect_lt(a$estimate, 0)
  # DMAR's limits, clamped into [0, 1]: the upper one is informative.
  expect_identical(a$lower, 0)
  expect_equal(a$upper, engine$value[engine$term == "ci_upper"], tolerance = 1e-12)
  expect_gt(a$upper, 0)
  expect_identical(a$uncertainty$status, "completed")
  expect_match(a$uncertainty$reason,
    sprintf("the interval [0.000, %.3f] does not contain the estimate", a$upper), fixed = TRUE)
  expect_match(a$uncertainty$reason, "its lower limit, 0.000, is the bound of that range", fixed = TRUE)
  expect_false(grepl("[0, 0]", a$uncertainty$reason, fixed = TRUE))
  tab <- sieve_reliability_table(list(r))
  expect_equal(tab$alpha_upper, a$upper, tolerance = 1e-12)
  expect_match(sieve_reliability_paragraph(r), sprintf("[0.000, %.3f]", a$upper), fixed = TRUE)
  # Far below zero both limits are clamped to 0: none is reported, and the
  # reason says so from the limits the engine returned.
  d <- lavaan::HolzingerSwineford1939
  d$x6 <- -d$x6
  far <- sieve_reliability_construct(d, sieve_reliability_spec(c("x4", "x5", "x6"), rep("continuous", 3),
    construct = "Textual", run_bootstrap = FALSE))
  expect_identical(far$alpha$uncertainty$status, "unavailable")
  expect_true(is.na(far$alpha$upper))
  expect_match(far$alpha$uncertainty$reason, "here both limits are clamped to 0", fixed = TRUE)
})

test_that("an ordered construct's model-implied omega says it has only bootstrap intervals, not 'none requested'", {
  d <- rf_hs()
  d[, 4:6] <- lapply(d[, 4:6], function(v) as.integer(cut(v, stats::quantile(v, 0:4 / 4), include.lowest = TRUE)))
  res <- list(
    Visual = sieve_reliability_construct(d, sieve_reliability_spec(paste0("x", 1:3), rep("continuous", 3),
      construct = "Visual", run_bootstrap = FALSE), alpha = FALSE),
    Textual = sieve_reliability_construct(d, sieve_reliability_spec(paste0("x", 4:6), rep("ordered", 3),
      construct = "Textual", run_bootstrap = FALSE), alpha = FALSE)
  )
  mi <- res$Textual$model_implied
  expect_true(is.finite(mi$estimate))
  expect_identical(mi$uncertainty$status, "unavailable")
  tab <- sieve_reliability_table(res)
  expect_identical(tab$model_interval[2], "none: ordered items have only bootstrap intervals")
  shown <- sieve_reliability_display(tab, res)$data
  expect_match(shown$model_ci[1], "^\\[0\\.[0-9]{3}, 0\\.[0-9]{3}\\]$")
  expect_identical(shown$model_ci[2], "none: ordered items have only bootstrap intervals")
  expect_false(any(shown$model_ci == "none requested"))
  expect_match(sieve_reliability_paragraph(res$Textual),
    "every interval DMAR gives categorical omega is a bootstrap that refits the WLSMV model", fixed = TRUE)
})

test_that("the help and the articles say which rows have an interval before the bootstrap, not every row", {
  files <- c(
    testthat::test_path("..", "..", "R", "mod_reliab.R"),
    testthat::test_path("..", "..", "vignettes", "user-guide.Rmd"),
    testthat::test_path("..", "..", "vignettes", "validation.Rmd")
  )
  skip_if_not(all(file.exists(files)))
  text <- vapply(files, function(f) gsub("[\"[:space:],]+", " ", paste(readLines(f, warn = FALSE), collapse = " ")),
    character(1))
  for (t in text) {
    expect_false(grepl("so every row has an interval in view", t, fixed = TRUE))
    expect_match(t, "every construct of continuous items has an interval in view", fixed = TRUE)
  }
  guide <- text[[2]]
  # The compact view holds model-implied omega; Show all columns adds its method.
  expect_false(grepl("**Show all columns** adds model-implied omega", guide, fixed = TRUE))
  expect_match(guide, "**Show all columns** adds the method of the model-implied interval", fixed = TRUE)
})

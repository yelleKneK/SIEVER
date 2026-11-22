# The Items tab: each construct's omega, alpha and omega if dropped use that
# construct's own rows, by FIML for continuous items, as the Reliability tab
# does, so they do not move when another construct's items are included or
# excluded and a planned-missing design gives values; and an item loading
# opposite in sign to the others of its factor is named for its keying
# rather than flagged as low loading.

skip_if_not_installed("lavaan")

ir_meta <- function(items, constructs) {
  data.frame(
    item = items, label = items, construct = constructs, facet = NA_character_,
    type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
}

ir_hs <- function() {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  withr::with_seed(1, {
    d$x7[sample(nrow(d), 150)] <- NA
    d$x8[sample(nrow(d), 100)] <- NA
  })
  d
}

ir_robust_omega <- function(x, missing = "fiml") {
  r <- DMAR::reliability_omega(data = x, missing = missing, ci_method = "none", denominator = "observed")
  r$value[r$term == "estimate"]
}

test_that("a construct's omega and alpha are those of its own rows, whatever another construct's items are", {
  d <- ir_hs()
  meta <- ir_meta(names(d), rep(c("Visual", "Textual", "Speed"), each = 3L))
  rng <- sieve_draw_seed(1)
  all_in <- sieve_item_diagnostics(d, meta, rng = rng)
  # Visual's items are complete in all 301 rows; the pool's complete cases
  # (n = 105, set by the Speed items) would give .684 against .633.
  expect_equal(all_in$constructs["Visual", "omega"], ir_robust_omega(d[, 1:3]), tolerance = 1e-10)
  expect_identical(all_in$constructs["Visual", "n_reliability"], 301L)
  expect_identical(all_in$constructs$missing_treatment, rep("fiml", 3L))
  # Speed's items are incomplete: FIML on its own rows, the Reliability tab's value.
  expect_equal(all_in$constructs["Speed", "omega"], ir_robust_omega(d[, 7:9]), tolerance = 1e-10)
  expect_identical(all_in$constructs["Speed", "n_reliability"], 301L)
  expect_identical(all_in$constructs["Speed", "n_complete"], sum(stats::complete.cases(d[, 7:9])))
  spec <- sieve_reliability_spec(paste0("x", 7:9), rep("continuous", 3), construct = "Speed",
    run_bootstrap = FALSE)
  tab <- sieve_reliability_construct(d, spec, model_implied = FALSE)
  expect_equal(all_in$constructs["Speed", "omega"], tab$omega$estimate, tolerance = 1e-10)
  expect_equal(all_in$constructs["Speed", "alpha"], tab$alpha$estimate, tolerance = 1e-10)
  # Excluding Speed changes neither Visual nor Textual.
  out <- meta
  out$status[7:9] <- "excluded"
  without <- sieve_item_diagnostics(d, out, rng = rng)
  for (cn in c("Visual", "Textual")) {
    expect_identical(without$constructs[cn, "omega"], all_in$constructs[cn, "omega"])
    expect_identical(without$constructs[cn, "alpha"], all_in$constructs[cn, "alpha"])
  }
  # Complete cases on request: the rows complete on the construct's own items.
  lw <- sieve_item_diagnostics(d, meta, rng = rng, missing = "listwise")
  expect_equal(lw$constructs["Speed", "omega"],
    ir_robust_omega(d[stats::complete.cases(d[, 7:9]), 7:9], "listwise"), tolerance = 1e-10)
  expect_identical(lw$constructs["Speed", "n_reliability"], sum(stats::complete.cases(d[, 7:9])))
  expect_equal(lw$constructs["Visual", "omega"], all_in$constructs["Visual", "omega"], tolerance = 1e-10)
  # The recorded R reproduces the values.
  code <- sieve_item_diagnostics_code(all_in, NULL, meta$construct, meta$item)
  env <- new.env()
  env$active_items <- d
  env$d <- d
  eval(parse(text = code), envir = env)
  got <- vapply(env$items_reliability, function(z) z$robust_omega, numeric(1))
  expect_equal(unname(got), unname(all_in$constructs$omega), tolerance = 1e-10)
})

test_that("excluding an item of one construct leaves another's drop deltas and flags alone and recomputes only its own", {
  # Missing at random in B4 as a function of construct A: computed on the
  # pool's complete cases, A would be truncated and A2's removal flag would
  # flip with B4's inclusion (+0.005 flagged, -0.122 not).
  n <- 600
  sim <- withr::with_seed(1, {
    f <- matrix(stats::rnorm(2 * n), n, 2) %*% chol(matrix(c(1, .3, .3, 1), 2))
    lam_a <- c(.75, .70, .65, .25)
    A <- sapply(lam_a, function(l) l * f[, 1] + stats::rnorm(n, sd = sqrt(1 - l^2)))
    B <- sapply(rep(.7, 4), function(l) l * f[, 2] + stats::rnorm(n, sd = sqrt(1 - l^2)))
    colnames(A) <- paste0("A", 1:4)
    colnames(B) <- paste0("B", 1:4)
    D <- as.data.frame(cbind(A, B))
    D$B4[stats::runif(n) < stats::plogis(-0.4 - 1.5 * rowMeans(A[, 1:3]))] <- NA
    D
  })
  meta <- ir_meta(names(sim), rep(c("A", "B"), each = 4L))
  rng <- sieve_draw_seed(1)
  keep <- new.env()
  with_b4 <- sieve_item_diagnostics(sim, meta, rng = rng, keep = keep)
  out <- meta
  out$status[out$item == "B4"] <- "excluded"
  without_b4 <- sieve_item_diagnostics(sim, out, rng = rng, keep = keep)
  expect_identical(without_b4$recomputed, "B")
  a <- function(x) x$items[x$items$construct == "A", , drop = FALSE]
  expect_identical(a(without_b4)$omega_drop_delta, a(with_b4)$omega_drop_delta)
  expect_identical(a(without_b4)$alpha_drop_delta, a(with_b4)$alpha_drop_delta)
  expect_identical(a(without_b4)$flag_omega_drop, a(with_b4)$flag_omega_drop)
  expect_identical(without_b4$constructs["A", "omega"], with_b4$constructs["A", "omega"])
  # A is complete: its coefficient is that of its own 600 rows.
  expect_equal(with_b4$constructs["A", "omega"], ir_robust_omega(sim[, 1:4]), tolerance = 1e-10)
  expect_identical(with_b4$constructs["A", "n_reliability"], 600L)
  # B's drop deltas are FIML refits on B's rows.
  full_b <- ir_robust_omega(sim[, 5:8])
  expect_equal(with_b4$constructs["B", "omega"], full_b, tolerance = 1e-10)
  expect_equal(with_b4$items$omega_drop_delta[with_b4$items$item == "B1"],
    ir_robust_omega(sim[, c("B2", "B3", "B4")]) - full_b, tolerance = 1e-10)
})

test_that("a planned-missing design gives each construct its coefficients", {
  P <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  form <- withr::with_seed(1, sample(1:3, nrow(P), replace = TRUE))
  P[form == 1, c("x4", "x5", "x6")] <- NA
  P[form == 2, c("x7", "x8", "x9")] <- NA
  P[form == 3, c("x1", "x2", "x3")] <- NA
  expect_identical(sum(stats::complete.cases(P)), 0L)
  meta <- ir_meta(names(P), rep(c("Visual", "Textual", "Speed"), each = 3L))
  basis <- sieve_retention_cor(P, rep("continuous", 9), "pearson", missing = "fiml")
  pm <- sieve_item_diagnostics(P, meta, rng = sieve_draw_seed(1), basis = basis)
  expect_true(all(is.finite(pm$constructs$omega)))
  expect_true(all(is.finite(pm$constructs$alpha)))
  blocks <- split(names(P), meta$construct)
  for (cn in names(blocks)) {
    expect_equal(pm$constructs[cn, "omega"], ir_robust_omega(P[, blocks[[cn]]]), tolerance = 1e-10)
    expect_identical(pm$constructs[cn, "n_reliability"], sum(rowSums(!is.na(P[, blocks[[cn]]])) > 0L))
  }
})

test_that("the wait of a FIML construct is estimated at the FIML rate", {
  sieve_rate_reset()
  fast <- sieve_diagnostics_wait(12, efa = FALSE, n = 3000)
  fiml <- sieve_diagnostics_wait(12, efa = FALSE, fiml = TRUE, n = 3000)
  expect_equal(fiml, 0.05 + 14 * (0.08 + 4.8e-6 * 3000 * 66), tolerance = 1e-12)
  expect_gt(fiml, 5 * fast)
  d <- ir_hs()
  plan <- sieve_diagnostics_plan(d, ir_meta(names(d), rep(c("Visual", "Textual", "Speed"), each = 3L)))
  expect_identical(plan$blocks$fiml, c(FALSE, FALSE, TRUE))
  expect_identical(plan$blocks$rows, rep(301L, 3L))
})

test_that("a reverse keyed item is named for its keying, not flagged as low loading or for removal", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  d$x6 <- -d$x6
  meta <- ir_meta(names(d), rep(c("Visual", "Textual", "Speed"), each = 3L))
  diag <- sieve_item_diagnostics(d, meta, rng = sieve_draw_seed(1))
  x6 <- diag$items[diag$items$item == "x6", ]
  expect_lt(x6$loading_target, -0.70)
  expect_false(x6$flag_loading)
  expect_true(x6$flag_keying)
  expect_true(x6$flag_item_total)
  expect_identical(x6$removal_signals, 0L)
  expect_false(any(diag$items$flag_keying[diag$items$item != "x6"]))
  note <- diag$constructs["Textual", "keying_note"]
  expect_match(note, "x6 loads -0\\.8[0-9]{2}, opposite in sign to the other 2 items of Textual")
  expect_match(note, "set its Reverse? cell to Yes on the Ingest tab rather than excluding it", fixed = TRUE)
  expect_true(all(is.na(diag$constructs[c("Visual", "Speed"), "keying_note"])))
  # Marked reverse keyed already: a second reversal, not a new one.
  marked <- meta
  marked$reverse[marked$item == "x6"] <- TRUE
  twice <- sieve_item_keying(diag$items, salient = 0.30, meta = marked)
  expect_match(twice$notes[["Textual"]], "x6 is marked reverse keyed and was reverse scored", fixed = TRUE)
  expect_match(twice$notes[["Textual"]], "reversed twice", fixed = TRUE)
  # With the reverse keyed item first the factor is oriented by it, and it is
  # still the one named, not the two correctly keyed items.
  first <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  first$x4 <- -first$x4
  fd <- sieve_item_diagnostics(first, meta, rng = sieve_draw_seed(1))
  expect_identical(fd$items$item[fd$items$flag_keying], "x4")
  # A small loading of the other sign is left to the low loading flag.
  small <- data.frame(item = c("a", "b", "c"), construct = "K", factor_group = "K",
    loading_target = c(0.7, 0.6, -0.1), stringsAsFactors = FALSE)
  expect_identical(sieve_item_keying(small, salient = 0.30)$flag, c(FALSE, FALSE, FALSE))
})

test_that("declared auxiliaries enter a construct's FIML fit only where its items have missing values", {
  d <- lavaan::HolzingerSwineford1939
  withr::with_seed(1, d$x7[sample(nrow(d), 150)] <- NA)
  meta <- ir_meta(paste0("x", 1:9), rep(c("Visual", "Textual", "Speed"), each = 3L))
  diag <- sieve_item_diagnostics(d, meta, rng = sieve_draw_seed(1), aux = "ageyr")
  speed <- DMAR::reliability_omega(data = d[, c("x7", "x8", "x9", "ageyr")], aux = "ageyr", missing = "fiml",
    ci_method = "none", denominator = "observed")
  expect_equal(diag$constructs["Speed", "omega"], speed$value[speed$term == "estimate"], tolerance = 1e-10)
  # Visual is complete: the FIML estimate is the complete-data one, with
  # or without the auxiliary, so the complete-data route gives it.
  expect_equal(diag$constructs["Visual", "omega"], ir_robust_omega(d[, c("x1", "x2", "x3")]), tolerance = 1e-10)
  plan <- sieve_diagnostics_plan(d, meta, aux = "ageyr")
  expect_identical(plan$blocks$fiml, c(FALSE, FALSE, TRUE))
  # The recorded R reproduces both, reading the auxiliary from the session data d.
  code <- sieve_item_diagnostics_code(diag, NULL, meta$construct, meta$item)
  env <- new.env()
  env$active_items <- d[, meta$item]
  env$d <- d
  eval(parse(text = code), envir = env)
  got <- vapply(env$items_reliability, function(z) z$robust_omega, numeric(1))
  expect_equal(unname(got), unname(diag$constructs$omega), tolerance = 1e-10)
})

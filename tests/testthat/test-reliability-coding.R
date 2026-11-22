# What categorical omega scores, and the coefficient of the codes as scored.
# DMAR's categorical omega is the reliability of the sum of category ranks;
# SIEVER's scores use the codes. The coded values are checked against an
# independent Green and Yang (2009) assembly whose bivariate normal
# probabilities come from stats::integrate(), not from sieve_pbinorm().

# Five ordered items from a one-factor model with the thresholds given.
rc_gen <- function(N, thr, lambda = c(.75, .7, .65, .6, .55), seed = 1) {
  set.seed(seed)
  f <- stats::rnorm(N)
  X <- as.data.frame(lapply(seq_along(lambda), function(j) {
    ystar <- lambda[j] * f + sqrt(1 - lambda[j]^2) * stats::rnorm(N)
    1L + as.integer(rowSums(outer(ystar, thr[[j]], ">")))
  }))
  names(X) <- paste0("o", seq_along(lambda))
  X
}
rc_thr4 <- list(c(-1, 0, 1), c(-1.1, .1, 1.1), c(-.9, -.1, .9), c(-1.2, .2, 1.2), c(-.8, 0, .8))

rc_meta <- function(X, lo = 1, hi = 4) {
  data.frame(
    item = names(X), label = names(X), construct = "Ordinal", facet = NA_character_,
    status = "active", type = "ordered", reverse = FALSE,
    rev_lo = lo, rev_hi = hi, rev_bounds = if (is.na(lo)) NA_character_ else "declared",
    stringsAsFactors = FALSE
  )
}

rc_run <- function(X, meta = NULL, denominator = "observed", ci_method = "none", ...) {
  sp <- sieve_reliability_spec(names(X), rep("ordered", ncol(X)),
    construct = "Ordinal",
    denominator = denominator, ci_method = ci_method, missing = "listwise", ...
  )
  sieve_reliability_construct(X, sp, alpha = TRUE, model_implied = FALSE, meta = meta)
}

# The independent assembly: P(Z1 <= a, Z2 <= b) by one-dimensional
# integration, and Green and Yang's covariance of item scores with step
# weights d (the increments between adjacent category scores).
rc_phi2 <- function(a, b, r) {
  if (r >= 1 - 1e-12) return(stats::pnorm(min(a, b)))
  s <- sqrt(1 - r^2)
  stats::integrate(function(x) stats::dnorm(x) * stats::pnorm((b - r * x) / s),
    -Inf, a, rel.tol = 1e-12, abs.tol = 0, subdivisions = 1000L
  )$value
}
rc_cov <- function(tj, tk, dj, dk, r) {
  out <- 0
  for (a in seq_along(tj)) for (b in seq_along(tk)) {
    out <- out + dj[a] * dk[b] * (rc_phi2(tj[a], tk[b], r) - stats::pnorm(tj[a]) * stats::pnorm(tk[b]))
  }
  out
}
rc_gy <- function(lambda, tau, R_total, steps, w = rep(1, length(lambda))) {
  num <- den <- 0
  for (j in seq_along(lambda)) for (k in seq_along(lambda)) {
    rt <- if (j == k) lambda[j]^2 else lambda[j] * lambda[k]
    rd <- if (j == k) 1 else R_total[j, k]
    num <- num + w[j] * w[k] * rc_cov(tau[[j]], tau[[k]], steps[[j]], steps[[k]], rt)
    den <- den + w[j] * w[k] * rc_cov(tau[[j]], tau[[k]], steps[[j]], steps[[k]], rd)
  }
  num / den
}
# The one-factor model of the latent responses, fitted as DMAR fits it, and
# the four coefficients (ranks or codes, observed or model-implied
# denominator).
rc_targets <- function(X, w = rep(1, ncol(X))) {
  ids <- names(X)
  fit <- lavaan::cfa(paste("F =~", paste(ids, collapse = " + ")),
    data = X, ordered = ids, std.lv = TRUE
  )
  est <- lavaan::lavInspect(fit, "est")
  lambda <- as.numeric(est$lambda[ids, 1])
  th <- est$tau[, 1]
  th_item <- sub("[|].*$", "", names(th))
  tau <- lapply(ids, function(v) as.numeric(th[th_item == v]))
  R_obs <- lavaan::lavInspect(fit, "sampstat")$cov[ids, ids]
  R_mi <- tcrossprod(lambda)
  diag(R_mi) <- 1
  codes <- lapply(X, function(x) sort(unique(x[!is.na(x)])))
  codes_steps <- lapply(codes, diff)
  ranks <- lapply(codes, function(cc) rep(1, length(cc) - 1L))
  c(
    rank_obs = rc_gy(lambda, tau, R_obs, ranks, w), code_obs = rc_gy(lambda, tau, R_obs, codes_steps, w),
    rank_mi = rc_gy(lambda, tau, R_mi, ranks, w), code_mi = rc_gy(lambda, tau, R_mi, codes_steps, w)
  )
}

test_that("codes that step by one shared amount: the target is stated and no coding note is made", {
  X <- rc_gen(400, rc_thr4)
  r <- rc_run(X, rc_meta(X))
  expect_true(is.na(r$coding$note))
  expect_true(r$coding$shared)
  par <- sieve_reliability_paragraph(r)
  expect_match(par, "for the sum of the items' category ranks (each item's observed categories scored 0, 1, ..., C - 1", fixed = TRUE)
  expect_match(par, "The sum of the category ranks has the same reliability as the sum of the codes SIEVER scores only when every item's codes step by one shared amount.", fixed = TRUE)
  expect_match(par, "Here every item's codes step by 1, so the coefficient also describes those scores.", fixed = TRUE)
  expect_false(grepl("observed sum score", par, fixed = TRUE))
  tab <- sieve_reliability_table(list(r))
  expect_identical(tab$score_target, "sum of category ranks")
  expect_true(is.na(tab$coding_note))
  expect_identical(tab$coded_estimate, tab$omega)
  expect_length(sieve_reliability_notes(r), 0L)
  # A continuous construct scores the item scores; nothing is noted.
  hs <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  rc <- sieve_reliability_construct(hs, sieve_reliability_spec(names(hs), rep("continuous", 3),
    construct = "Textual", ci_method = "none", missing = "listwise"), alpha = FALSE, model_implied = FALSE)
  tc <- sieve_reliability_table(list(rc))
  expect_identical(tc$score_target, "sum of item scores")
  expect_true(is.na(tc$coded_estimate))
})

test_that("a common step and per-item shifts leave the coefficient and the silence unchanged", {
  X <- rc_gen(400, rc_thr4)
  base <- rc_run(X)
  doubled <- as.data.frame(lapply(X, function(x) 2L * (x - 1L)))
  shifted <- as.data.frame(Map(function(x, j) x + j * 10L, X, seq_along(X)))
  for (Y in list(doubled, shifted)) {
    r <- rc_run(Y)
    expect_equal(r$omega$estimate, base$omega$estimate, tolerance = 1e-12)
    expect_true(is.na(r$coding$note))
    expect_true(r$coding$shared)
  }
  expect_match(sieve_reliability_paragraph(rc_run(doubled)), "Here every item's codes step by 2", fixed = TRUE)
  # Different numbers of categories with a step of 1, and binary 0/1 beside
  # partial credit 0/1/2, share one step.
  mixed <- X
  mixed$o4 <- as.integer(X$o4 >= 3L)
  mixed$o5 <- pmin(X$o5, 3L) - 1L
  expect_true(is.na(rc_run(mixed)$coding$note))
})

test_that("reversing one item, or reflecting every item, makes no coding note", {
  X <- rc_gen(400, rc_thr4)
  meta <- rc_meta(X)
  rev <- sieve_reverse_item(X, meta, "o2", lo = 1, hi = 4)
  r <- rc_run(rev$data, rev$meta)
  expect_true(is.na(r$coding$note))
  expect_equal(rev$data$o2, 5 - X$o2)
  reflected <- as.data.frame(lapply(X, function(x) 5L - x))
  expect_equal(rc_run(reflected)$omega$estimate, rc_run(X)$omega$estimate, tolerance = 1e-6)
  expect_true(is.na(rc_run(reflected)$coding$note))
})

test_that("one item stepping by 2 is named with its codes and calls for attention", {
  X <- rc_gen(400, rc_thr4)
  X$o1 <- 2L * X$o1
  r <- rc_run(X, rc_meta(X, NA, NA))
  expect_false(r$coding$shared)
  expect_identical(r$coding$items$flagged, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_match(r$coding$note, "o1 is scored 2, 4, 6, 8, so its categories count 0, 1, 2, 3 in the coefficient and 0, 2, 4, 6 in the score; the other items step by 1.", fixed = TRUE)
  notes <- sieve_reliability_notes(r)
  expect_true(length(notes) > 0L)
  expect_true(r$coding$note %in% notes)
  # DMAR's own message on the codes is replaced by SIEVER's note, not repeated.
  expect_true(any(grepl("^Categorical omega scores the categories", r$omega$messages)))
  par <- sieve_reliability_paragraph(r)
  expect_false(grepl("These codes are not one rescaling", par, fixed = TRUE))
  expect_match(par, "Alpha is computed on the item codes as scored", fixed = TRUE)
  tt <- rc_targets(X)
  expect_equal(r$coding$coded_estimate, unname(tt["code_obs"]), tolerance = 1e-8)
})

test_that("an empty interior category is named against the declared bounds, with the coded value", {
  thr <- rc_thr4
  thr[[1]] <- c(-1, 0, 0)
  X <- rc_gen(400, thr, seed = 21)
  expect_identical(sort(unique(X$o1)), c(1L, 2L, 4L))
  r <- rc_run(X, rc_meta(X))
  expect_match(r$coding$note, "o1 is scored 1, 2, 4 (no respondent chose 3 of the declared 1 to 4), so its categories count 0, 1, 2 in the coefficient and 0, 1, 3 in the score", fixed = TRUE)
  tt <- rc_targets(X)
  expect_equal(r$omega$estimate, unname(tt["rank_obs"]), tolerance = 1e-8)
  expect_equal(r$coding$rank_estimate, r$omega$estimate, tolerance = 1e-8)
  expect_equal(r$coding$coded_estimate, unname(tt["code_obs"]), tolerance = 1e-8)
  expect_match(r$coding$note, sprintf("the coded composite's reliability is %.3f against %.3f", tt["code_obs"], tt["rank_obs"]), fixed = TRUE)
  expect_match(r$coding$note, "forms intervals for the rank composite only", fixed = TRUE)
  tab <- sieve_reliability_table(list(r))
  expect_identical(tab$coding_note, r$coding$note)
  expect_equal(tab$coded_estimate, unname(tt["code_obs"]), tolerance = 1e-8)
  # The model-implied denominator gets its own coded value.
  rm <- rc_run(X, rc_meta(X), denominator = "model_implied")
  expect_equal(rm$coding$coded_estimate, unname(tt["code_mi"]), tolerance = 1e-8)
  # The display shows the composite scored and the coded composite.
  shown <- sieve_reliability_display(tab, list(r))
  expect_true(all(c("score_target", "coded_estimate") %in% names(shown$data)))
})

test_that("codes 1, 2, 5, 10 give a coding note and the coded value of the independent assembly", {
  X <- rc_gen(400, rc_thr4)
  map <- c(1L, 2L, 5L, 10L)
  Y <- as.data.frame(lapply(X, function(x) map[x]))
  r <- rc_run(Y, rc_meta(Y, NA, NA))
  expect_match(r$coding$note, "o1, o2, o3, o4, and o5 are each scored 1, 2, 5, 10, so their categories count 0, 1, 2, 3 in the coefficient and 0, 1, 4, 9 in the score.", fixed = TRUE)
  tt <- rc_targets(Y)
  expect_equal(r$coding$coded_estimate, unname(tt["code_obs"]), tolerance = 1e-8)
  expect_false(isTRUE(all.equal(unname(tt["code_obs"]), unname(tt["rank_obs"]))))
})

test_that("an unrecoded missing-value code of 9 is listed", {
  X <- rc_gen(400, list(c(-1, 0), c(-1.1, .1), c(-.9, -.1), c(-1.2, .2), c(-.8, 0)))
  X$o2[c(3, 17, 40, 77, 120, 200, 260, 333)] <- 9L
  r <- rc_run(X, rc_meta(X, 1, 3))
  expect_match(r$coding$note, "o2 is scored 1, 2, 3, 9 (9 lies outside the declared 1 to 3), so its categories count 0, 1, 2, 3 in the coefficient and 0, 1, 2, 8 in the score", fixed = TRUE)
  expect_identical(r$coding$items$outside, c(NA, "9", NA, NA, NA))
  expect_equal(r$coding$coded_estimate, unname(rc_targets(X)["code_obs"]), tolerance = 1e-8)
  # Without declared bounds the codes alone are named.
  expect_match(rc_run(X)$coding$note, "o2 is scored 1, 2, 3, 9, so its categories count", fixed = TRUE)
})

test_that("a reversed item with an empty interior category names its administered and scored codes", {
  thr <- rc_thr4
  thr[[1]] <- c(-1, 0, 0)
  X <- rc_gen(400, thr, seed = 21)
  rev <- sieve_reverse_item(X, rc_meta(X), "o1", lo = 1, hi = 4)
  r <- rc_run(rev$data, rev$meta)
  expect_match(r$coding$note, "o1 is scored 1, 3, 4 (administered 1, 2, 4, scored 4, 3, 1 after reversal within 1 to 4; no respondent chose 3 of the declared 1 to 4), so its categories count 0, 1, 2 in the coefficient and 0, 2, 3 in the score", fixed = TRUE)
  expect_equal(r$coding$coded_estimate, unname(rc_targets(rev$data)["code_obs"]), tolerance = 1e-8)
})

test_that("every engine message attached to the coefficient reaches the paragraph and the notes", {
  test_frame <- environment()
  X <- rc_gen(400, rc_thr4)
  real <- getExportedValue("DMAR", "reliability_omega_categorical")
  local_mocked_bindings(sieve_reliability_engine = function(fun) {
    function(...) {
      message("X")
      real(...)
    }
  }, .env = test_frame)
  r <- rc_run(X)
  expect_match(sieve_reliability_paragraph(r), "The engine noted: \"X\".", fixed = TRUE)
  expect_true("The engine noted: \"X\"." %in% sieve_reliability_notes(r))
})

test_that("DMAR's undetermined pair under the model-implied denominator is reported", {
  # The mixed-skew design of Green and Yang (2009) as DMAR's own tests draw
  # it: two items piled into the top category and two into the bottom; at
  # this seed every respondent below the top category of y1 is in the
  # bottom category of y3, so their polychoric correlation is undetermined.
  set.seed(113)
  p_top <- c(.011, .027, .047, .118, .797)
  cut_top <- c(-Inf, stats::qnorm(cumsum(p_top)[-5]), Inf)
  cut_bottom <- c(-Inf, stats::qnorm(cumsum(rev(p_top))[-5]), Inf)
  latent <- outer(stats::rnorm(500), rep(0.8, 4)) + matrix(stats::rnorm(500 * 4, sd = 0.6), 500, 4)
  X <- data.frame(
    y1 = cut(latent[, 1], cut_top, labels = FALSE), y2 = cut(latent[, 2], cut_top, labels = FALSE),
    y3 = cut(latent[, 3], cut_bottom, labels = FALSE), y4 = cut(latent[, 4], cut_bottom, labels = FALSE)
  )
  expect_true(all(X$y3[X$y1 < 5] == 1))
  r <- rc_run(X, denominator = "model_implied")
  expect_true(is.finite(r$omega$estimate))
  expect_true(any(grepl("do not determine the polychoric correlation of y1 and y3", r$omega$messages, fixed = TRUE)))
  expect_match(sieve_reliability_paragraph(r), "The engine noted: \"The data do not determine the polychoric correlation of y1 and y3", fixed = TRUE)
  expect_true(any(grepl("The engine noted: \"The data do not determine", sieve_reliability_notes(r), fixed = TRUE)))
})

test_that("the methods draft and the scoring syntax state the rank target for categorical constructs", {
  thr <- rc_thr4
  thr[[1]] <- c(-1, 0, 0)
  X <- rc_gen(400, thr, seed = 21)
  meta <- rc_meta(X)
  sp <- sieve_reliability_spec(names(X), rep("ordered", 5), construct = "Ordinal", ci_method = "none", missing = "listwise")
  expect_match(sieve_draft_coefficient(sp), "the reliability of the sum of category ranks", fixed = TRUE)
  draft <- sieve_draft_reliability(X, meta, settings = NULL, estimator = "auto", revision = list(), missing = "listwise")
  expect_match(draft$method, "the reliability of the sum of the items' category ranks", fixed = TRUE)
  expect_match(draft$lines[1], "o1 is scored 1, 2, 4 (no respondent chose 3 of the declared 1 to 4)", fixed = TRUE)
  code <- sieve_scoring_code(meta, values = "processed")$code
  expect_true(any(grepl("Categorical omega (Ordinal: every item ordered or binary) is the reliability of the sum of category", code, fixed = TRUE)))
  expect_true(any(grepl("only when every item's codes step by one shared amount", code, fixed = TRUE)))
  cont <- meta
  cont$type <- "continuous"
  expect_false(any(grepl("Categorical omega", sieve_scoring_code(cont, values = "processed")$code, fixed = TRUE)))
})

test_that("a bootstrap of categorical omega names a sparse interior category with its expected omission share", {
  X <- rc_gen(400, rc_thr4)
  X$o3[X$o3 == 2L] <- 1L
  X$o3[1] <- 2L
  N <- nrow(X)
  r <- rc_run(X, ci_method = "percentile", run_bootstrap = FALSE)
  expect_identical(r$omega$uncertainty$status, "pending")
  hit <- r$coding$sparse[r$coding$sparse$item == "o3" & r$coding$sparse$code == 2, ]
  expect_identical(hit$n, 1L)
  expect_identical(hit$share, (1 - 1 / N)^N)
  expect_match(r$coding$sparse_note, sprintf("in o3, category 2 has 1 respondent of %d (about 37%% of replications omit it, (1 - 1/%d)^%d)", N, N, N), fixed = TRUE)
  expect_match(sieve_reliability_paragraph(r), "Bootstrap replications can omit a sparse interior category", fixed = TRUE)
  expect_true(r$coding$sparse_note %in% sieve_reliability_notes(r))
  # Without a bootstrap there is no interval to qualify.
  expect_true(is.na(rc_run(X)$coding$sparse_note))
})

test_that("the Items tab's live omega keeps the engine's messages and shows the coding note once per construct", {
  X <- rc_gen(400, rc_thr4)
  X$o1 <- 2L * X$o1
  om <- sieve_item_omega_point(X, ordered = TRUE)
  expect_true(any(grepl("^Categorical omega scores the categories", attr(om, "messages"))))
  meta <- rc_meta(X, NA, NA)
  note <- sieve_item_omega_note(X, names(X), meta, TRUE, attr(om, "messages"))
  expect_match(note, "o1 is scored 2, 4, 6, 8", fixed = TRUE)
  expect_match(note, "The Reliability tab gives the coded composite's reliability", fixed = TRUE)
  expect_false(grepl("These codes are not one rescaling", note, fixed = TRUE))
  diag <- sieve_item_diagnostics(X, meta, ordered = names(X))
  expect_identical(diag$constructs$omega_note, note)
  expect_true(is.na(sieve_item_omega_note(X[, 2:5], names(X)[2:5], meta, TRUE, NULL)))
})

test_that("the model-linked composite scores the codes, with the rank coefficient beside it", {
  X <- rc_gen(400, rc_thr4)
  ids <- names(X)
  model <- paste("F =~", paste(ids, collapse = " + "))
  fit <- lavaan::cfa(model, data = X, ordered = ids, std.lv = TRUE)
  # Consecutive codes: the coefficient is DMAR's categorical omega.
  r <- sieve_model_composite(fit)
  dmar <- DMAR::reliability_omega_categorical(X, ci_method = "none", denominator = "observed")
  expect_equal(r$table$estimate[1], dmar$value[dmar$term == "estimate"], tolerance = 1e-8)
  expect_false("rank_estimate" %in% names(r$table))
  expect_match(r$target, "the unit-weighted sum of the item codes as given in the fitted data", fixed = TRUE)
  expect_false(grepl("observed category scores", r$method, fixed = TRUE))
  # Codes 1, 2, 5, 10 with weights 2, 1, 1, 1, 1.
  map <- c(1L, 2L, 5L, 10L)
  Y <- as.data.frame(lapply(X, function(x) map[x]))
  fy <- lavaan::cfa(model, data = Y, ordered = ids, std.lv = TRUE)
  w <- c(2, 1, 1, 1, 1)
  rw <- sieve_model_composite(fy, weights = w)
  tt <- rc_targets(Y, w = w)
  expect_equal(rw$table$estimate[1], unname(tt["code_obs"]), tolerance = 1e-8)
  expect_equal(rw$table$rank_estimate[1], unname(tt["rank_obs"]), tolerance = 1e-8)
  expect_true(any(grepl("F: the weighted sum of the item codes as given in the fitted data is not the weighted sum of consecutive category ranks", rw$warnings, fixed = TRUE)))
  expect_true(any(grepl("o1, o2, o3, o4, and o5 are each coded 1, 2, 5, 10", rw$warnings, fixed = TRUE)))
  # The codes are read from the fitted data; text labels are scored by rank.
  expect_identical(sieve_fit_category_codes(fy)$o1, c(1, 2, 5, 10))
  Z <- as.data.frame(lapply(X, function(x) factor(c("a", "b", "c", "d")[x], ordered = TRUE)))
  fz <- lavaan::cfa(model, data = Z, ordered = ids, std.lv = TRUE)
  rz <- sieve_model_composite(fz)
  expect_equal(rz$table$estimate[1], r$table$estimate[1], tolerance = 1e-8)
  expect_true(any(grepl("no numeric codes in the fitted data", rz$warnings, fixed = TRUE)))
  # The generalized assembly reproduces the unit-step one exactly.
  expect_equal(
    sieve_green_yang(diag(2), diag(2), list(c(-1, 1), c(0, .5)), c(1, 1), list(c(1, 1), c(1, 1))),
    sieve_green_yang(diag(2), diag(2), list(c(-1, 1), c(0, .5)), c(1, 1))
  )
})

# Two groups with five four-category items, scalar invariance, and a latent
# mean of mu in group B, so a composite that ignored the group's
# thresholds would be visibly wrong.
rc_two_groups <- function(mu, n = 4000L, seed = 2026) {
  lam <- c(.8, .7, .75, .65, .7)
  tau <- c(-1, 0, 1)
  set.seed(seed)
  one <- function(m) {
    f <- stats::rnorm(n, m, 1)
    stats::setNames(
      as.data.frame(lapply(lam, function(l) findInterval(l * f + sqrt(1 - l^2) * stats::rnorm(n), tau) + 1L)),
      paste0("y", 1:5)
    )
  }
  d <- rbind(one(0), one(mu))
  d$g <- rep(c("A", "B"), each = n)
  d
}

test_that("ordered composites use the model-implied thresholds of each group", {
  d <- rc_two_groups(1.2)
  it <- paste0("y", 1:5)
  m <- paste("F =~", paste(it, collapse = " + "))
  w <- stats::setNames(rep(1, 5), it)
  single <- lavaan::cfa(m, data = d[d$g == "B", ], ordered = it, std.lv = TRUE)
  alone <- sieve_composite_coefficients(single, w)$value[[1]]
  for (param in c("delta", "theta")) {
    fs <- lavaan::cfa(m,
      data = d, group = "g", ordered = it, std.lv = TRUE, parameterization = param,
      group.equal = c("thresholds", "loadings", "intercepts")
    )
    e <- lavaan::lavInspect(fs, "est")[[2]]
    expect_gt(abs(e$alpha[1, 1]), 1)
    v <- sieve_composite_coefficients(fs, w)$value
    # By hand: (tau - nu - lambda alpha) times the scaling factor.
    sc <- e$delta[it, 1]
    mu_star <- as.numeric(e$nu[it, 1] + e$lambda[it, , drop = FALSE] %*% e$alpha)
    th_item <- sub("[|].*$", "", rownames(e$tau))
    th <- lapply(seq_along(it), function(i) (e$tau[th_item == it[i], 1] - mu_star[i]) * sc[i])
    true <- (e$lambda %*% e$psi %*% t(e$lambda))[it, it]
    S <- lavaan::lavInspect(fs, "sampstat")[[2]]$cov[it, it]
    by_hand <- sieve_green_yang(diag(sc) %*% true %*% diag(sc), stats::cov2cor(S), th, rep(1, 5))
    expect_equal(unname(v["F (B)"]), by_hand, tolerance = 1e-10)
    expect_lt(abs(unname(v["F (B)"]) - alone), .005)
  }
})

test_that("a one-group model with a free occasion mean uses its thresholds too", {
  tau <- c(-1, 0, 1)
  lam <- c(.8, .7, .75, .65, .7)
  cut3 <- function(x) findInterval(x, tau) + 1L
  set.seed(7)
  n <- 5000
  f1 <- stats::rnorm(n)
  f2 <- 0.6 * f1 + 0.8 * stats::rnorm(n) + 1.5
  d <- data.frame(
    sapply(lam, function(l) cut3(l * f1 + sqrt(1 - l^2) * stats::rnorm(n))),
    sapply(lam, function(l) cut3(l * f2 + sqrt(1 - l^2) * stats::rnorm(n)))
  )
  names(d) <- c(paste0("a", 1:5), paste0("b", 1:5))
  m <- paste(
    "T1 =~ NA*a1 + l1*a1 + l2*a2 + l3*a3 + l4*a4 + l5*a5",
    "T2 =~ NA*b1 + l1*b1 + l2*b2 + l3*b3 + l4*b4 + l5*b5",
    paste(sprintf("a%d | t%d1*t1 + t%d2*t2 + t%d3*t3", 1:5, 1:5, 1:5, 1:5), collapse = "\n"),
    paste(sprintf("b%d | t%d1*t1 + t%d2*t2 + t%d3*t3", 1:5, 1:5, 1:5, 1:5), collapse = "\n"),
    "T1 ~~ 1*T1", "T2 ~~ T2", "T1 ~ 0*1", "T2 ~ NA*1",
    paste(sprintf("b%d ~*~ NA*b%d", 1:5, 1:5), collapse = "\n"),
    sep = "\n"
  )
  fl <- lavaan::cfa(m, data = d, ordered = names(d))
  expect_true(lavaan::lavInspect(fl, "converged"))
  r <- sieve_model_composite(fl)
  it <- paste0("b", 1:5)
  alone <- sieve_composite_coefficients(
    lavaan::cfa(paste("F =~", paste(it, collapse = " + ")), data = d, ordered = it, std.lv = TRUE),
    stats::setNames(rep(1, 5), it)
  )$value[[1]]
  expect_lt(abs(r$table$estimate[r$table$composite == "T2"] - alone), .005)
})

# Validity evidence read against the researcher's stated predictions.
# Three kinds of expectation are kept apart:
# unspecified (reported, never scored), directional (a sign, optionally a
# smallest magnitude), and discriminant (small, within the researcher's own
# bound). An estimate is described by where its confidence interval lies in
# relation to the prediction; no verdict is printed from a fixed cutoff on
# the absolute correlation, and a wrong-signed correlation is never read as
# agreeing with a prediction.

val_classify <- function(r, lo, hi, ...) sieve_validity_classify(r, lo, hi, ...)

# Two constructs with three items each and two external variables. The
# first criterion relates NEGATIVELY to construct A (the theory will say
# positive); the second relates modestly to construct B.
val_data <- function() {
  withr::with_seed(20261012, {
    n <- 400
    f1 <- stats::rnorm(n)
    f2 <- stats::rnorm(n)
    item <- function(f, l) l * f + sqrt(1 - l^2) * stats::rnorm(n)
    d <- data.frame(
      a1 = item(f1, .8), a2 = item(f1, .75), a3 = item(f1, .7),
      b1 = item(f2, .8), b2 = item(f2, .75), b3 = item(f2, .7)
    )
    d$crit_neg <- -0.6 * f1 + sqrt(1 - .36) * stats::rnorm(n)
    d$crit_mid <- 0.33 * f2 + sqrt(1 - .33^2) * stats::rnorm(n)
    d
  })
}

val_state <- function(d = val_data()) {
  fixture <- list(data = d, item_cols = names(d)[1:6],
    name = "Study fixture", theory_map = NULL, labels = NULL, load_code = character())
  siever_test_study(fixture, labels = FALSE, theory = FALSE, name = NULL, configure = function(s) {
    s$meta <- sieve_build_meta(d, names(d)[1:6], list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3")))
    s$roles <- NULL
  })
}

val_id <- function(what, criterion, construct) {
  paste("exp", what, sieve_input_key(criterion), sieve_input_key(construct), sep = "__")
}

val_set <- function(session, id, value) {
  do.call(session$setInputs, stats::setNames(list(value), id))
}

val_text <- function(x) {
  if (is.null(x)) return("")
  paste(as.character(unlist(x)), collapse = " ")
}

# The first browser test of an R session is the one that starts Chrome, and
# on a machine busy with other jobs that first start does not always open
# its debugging port; a later start in the same session then succeeds.
# shinytest2 skips a test whose Chrome did not start, which is not a pass,
# so Chrome is started here first, with up to three attempts.
val_chrome_ready <- function(attempts = 3L) {
  for (i in seq_len(attempts)) {
    started <- tryCatch(
      {
        tab <- chromote::default_chromote_object()$new_session()
        tab$close()
        TRUE
      },
      error = function(e) FALSE
    )
    if (started) {
      return(TRUE)
    }
  }
  FALSE
}

test_that("a directional prediction is read by where the interval lies, and a wrong sign is never agreement", {
  # Right sign: the interval is entirely on the predicted side.
  right <- val_classify(0.40, 0.25, 0.53, kind = "directional", sign = 1)
  expect_identical(right$position, "predicted_side")
  expect_true(right$scored)
  expect_match(right$statement, "entirely on the predicted side of zero", fixed = TRUE)
  expect_identical(val_classify(-0.40, -0.53, -0.25, "directional", sign = -1)$position, "predicted_side")

  # Wrong sign, large in absolute value: a rule on |r| alone would call this
  # "convergent: supported" because |r| is at least .10.
  wrong <- val_classify(-0.58, -0.64, -0.51, kind = "directional", sign = 1)
  expect_identical(wrong$position, "opposite_side")
  expect_match(wrong$statement, "opposite to the prediction", fixed = TRUE)
  expect_match(wrong$statement, "negative side of zero", fixed = TRUE)
  expect_identical(val_classify(0.58, 0.51, 0.64, "directional", sign = -1)$position, "opposite_side")

  # The interval includes zero: the direction is not settled, whatever the
  # sign of the point estimate.
  expect_identical(val_classify(0.08, -0.05, 0.20, "directional", sign = 1)$position, "straddles_zero")
  expect_identical(val_classify(-0.08, -0.20, 0.05, "directional", sign = 1)$position, "straddles_zero")
  expect_identical(val_classify(0.08, -0.05, 0.20, "directional", sign = -1)$position, "straddles_zero")
  # An interval that only touches zero is not entirely on either side.
  expect_identical(val_classify(0.10, 0, 0.20, "directional", sign = 1)$position, "straddles_zero")
  expect_identical(val_classify(-0.10, -0.20, 0, "directional", sign = 1)$position, "straddles_zero")
  expect_match(val_classify(0.08, -0.05, 0.20, "directional", sign = 1)$statement,
    "includes zero",
    fixed = TRUE
  )

  # Reflecting the estimate, the interval, and the prediction together
  # changes nothing: the reading depends on the side, not on the sign.
  grid <- expand.grid(lo = c(-0.6, -0.2, 0.05, 0.3), width = c(0.1, 0.3), sign = c(-1, 1))
  for (i in seq_len(nrow(grid))) {
    lo <- grid$lo[i]
    hi <- lo + grid$width[i]
    a <- val_classify((lo + hi) / 2, lo, hi, "directional", sign = grid$sign[i], min_abs = 0.2)
    b <- val_classify(-(lo + hi) / 2, -hi, -lo, "directional", sign = -grid$sign[i], min_abs = 0.2)
    expect_identical(a$position, b$position)
  }
})

test_that("a predicted smallest magnitude refines an interval on the predicted side only", {
  at <- val_classify(0.50, 0.35, 0.62, "directional", sign = 1, min_abs = 0.30)
  expect_identical(at$position, "at_or_beyond_minimum")
  expect_match(at$statement, "at least 0.3 in magnitude", fixed = TRUE)
  expect_identical(
    val_classify(0.35, 0.22, 0.47, "directional", sign = 1, min_abs = 0.30)$position,
    "straddles_minimum"
  )
  expect_identical(
    val_classify(0.15, 0.05, 0.25, "directional", sign = 1, min_abs = 0.30)$position,
    "below_minimum"
  )
  # The limit itself counts as reaching the minimum.
  expect_identical(
    val_classify(0.40, 0.30, 0.50, "directional", sign = 1, min_abs = 0.30)$position,
    "at_or_beyond_minimum"
  )
  # A negative prediction mirrors.
  expect_identical(
    val_classify(-0.50, -0.62, -0.35, "directional", sign = -1, min_abs = 0.30)$position,
    "at_or_beyond_minimum"
  )
  # The minimum never rescues the wrong side or an unsettled direction.
  expect_identical(
    val_classify(-0.50, -0.62, -0.35, "directional", sign = 1, min_abs = 0.30)$position,
    "opposite_side"
  )
  expect_identical(
    val_classify(0.10, -0.02, 0.22, "directional", sign = 1, min_abs = 0.30)$position,
    "straddles_zero"
  )
})

test_that("a discriminant prediction is read against the researcher's bound with its uncertainty", {
  inside <- val_classify(0.05, -0.05, 0.15, "discriminant", bound = 0.20)
  expect_identical(inside$position, "inside_bound")
  expect_match(inside$statement, "within plus or minus 0.2", fixed = TRUE)
  expect_identical(val_classify(0.20, 0.10, 0.30, "discriminant", bound = 0.20)$position, "straddles_bound")
  expect_identical(val_classify(0.35, 0.25, 0.45, "discriminant", bound = 0.20)$position, "outside_bound")
  # The bound is two sided.
  expect_identical(val_classify(-0.35, -0.45, -0.25, "discriminant", bound = 0.20)$position, "outside_bound")
  expect_identical(val_classify(-0.20, -0.30, -0.10, "discriminant", bound = 0.20)$position, "straddles_bound")
  # An interval that reaches the bound is not entirely inside it.
  expect_identical(val_classify(0.10, 0.00, 0.20, "discriminant", bound = 0.20)$position, "straddles_bound")
  # r = .28 is below a cutoff of .30, and a rule on the point estimate
  # would call it "discriminant: supported". Its interval runs past the
  # bound, so the data do not settle that the relation is that small.
  old <- val_classify(0.282, 0.189, 0.370, "discriminant", bound = 0.30)
  expect_identical(old$position, "straddles_bound")
  expect_match(old$statement, "do not settle", fixed = TRUE)
  # The bound is the researcher's: the same interval is inside a wider one.
  expect_identical(val_classify(0.282, 0.189, 0.370, "discriminant", bound = 0.40)$position, "inside_bound")
})

test_that("no prediction is not scored, and a missing estimate or interval is unavailable", {
  none <- val_classify(-0.58, -0.64, -0.51, kind = "unspecified")
  expect_identical(none$position, "not_scored")
  expect_false(none$scored)
  # Unspecified stays unspecified whatever else is supplied.
  expect_identical(val_classify(0.01, -0.05, 0.07, "unspecified", sign = 1, bound = 0.3)$position, "not_scored")

  gone <- val_classify(0.40, NA, NA, "directional", sign = 1)
  expect_identical(gone$position, "interval_unavailable")
  expect_false(gone$scored)
  expect_match(gone$statement, "Interval unavailable", fixed = TRUE)
  expect_identical(val_classify(0.10, NA, 0.2, "discriminant", bound = 0.3)$position, "interval_unavailable")
  expect_identical(val_classify(NA, NA, NA, "directional", sign = 1)$position, "unavailable")
  expect_identical(val_classify(NA, NA, NA, "discriminant", bound = 0.3)$position, "unavailable")

  # A prediction that is not fully stated is not scored either.
  expect_identical(val_classify(0.4, 0.3, 0.5, "directional", sign = NA)$position, "prediction_incomplete")
  expect_identical(
    val_classify(0.4,
      0.3,
      0.5,
      "directional",
      sign = 1,
      min_abs = 1.5
    )$position,
    "prediction_incomplete"
  )
  expect_identical(val_classify(0.1, 0.0, 0.2, "discriminant", bound = NA)$position, "prediction_incomplete")
  expect_identical(val_classify(0.1, 0.0, 0.2, "discriminant", bound = 0)$position, "prediction_incomplete")
  expect_false(val_classify(0.1, 0.0, 0.2, "discriminant", bound = NA)$scored)

  # No reading, of any kind, is worded as a verdict.
  cases <- list(
    val_classify(0.4, 0.25, 0.53, "directional", sign = 1),
    val_classify(-0.58, -0.64, -0.51, "directional", sign = 1),
    val_classify(0.08, -0.05, 0.2, "directional", sign = 1),
    val_classify(0.5, 0.35, 0.62, "directional", sign = 1, min_abs = 0.3),
    val_classify(0.35, 0.22, 0.47, "directional", sign = 1, min_abs = 0.3),
    val_classify(0.15, 0.05, 0.25, "directional", sign = 1, min_abs = 0.3),
    val_classify(0.05, -0.05, 0.15, "discriminant", bound = 0.2),
    val_classify(0.2, 0.1, 0.3, "discriminant", bound = 0.2),
    val_classify(0.35, 0.25, 0.45, "discriminant", bound = 0.2),
    none, gone
  )
  said <- vapply(cases, function(x) x$statement, character(1))
  expect_false(any(grepl("support|concern|weak|valid|confirm|pass|fail", said, ignore.case = TRUE)))
})

test_that("the estimate and its Fisher z interval are the textbook ones, with the reason kept when they are not available", {
  d <- val_data()
  x <- rowMeans(d[, c("a1", "a2", "a3")])
  y <- d$crit_neg
  y[c(5, 9)] <- NA
  est <- sieve_validity_estimate(x, y, conf_level = 0.95)
  ok <- !is.na(y)
  r <- sum((x[ok] - mean(x[ok])) * (y[ok] - mean(y[ok]))) /
    sqrt(sum((x[ok] - mean(x[ok]))^2) * sum((y[ok] - mean(y[ok]))^2))
  n <- sum(ok)
  half <- stats::qnorm(0.975) / sqrt(n - 3)
  expect_identical(est$n, 398L)
  expect_equal(est$r, r, tolerance = 1e-12)
  expect_equal(est$lower, tanh(atanh(r) - half), tolerance = 1e-10)
  expect_equal(est$upper, tanh(atanh(r) + half), tolerance = 1e-10)
  expect_true(is.na(est$reason))
  expect_match(est$method, "Fisher z", fixed = TRUE)
  # Another level is honored.
  est90 <- sieve_validity_estimate(x, y, conf_level = 0.90)
  expect_equal(est90$upper, tanh(atanh(r) + stats::qnorm(0.95) / sqrt(n - 3)), tolerance = 1e-10)

  # Three pairs give an estimate and no interval; two give neither.
  three <- sieve_validity_estimate(c(1, 2, 4), c(2, 1, 5))
  expect_equal(three$r, stats::cor(c(1, 2, 4), c(2, 1, 5)), tolerance = 1e-12)
  expect_true(is.na(three$lower) && is.na(three$upper))
  expect_match(three$reason, "at least 4 complete pairs", fixed = TRUE)
  two <- sieve_validity_estimate(c(1, 2, NA), c(2, 1, 5))
  expect_true(is.na(two$r))
  expect_match(two$reason, "Only 2 complete pairs", fixed = TRUE)
  flat <- suppressWarnings(sieve_validity_estimate(rep(1, 10), 1:10))
  expect_true(is.na(flat$r))
  expect_false(is.na(flat$reason))
})

test_that("a factor criterion is read by its labels, never by its level codes, and text and unequal lengths are refused", {
  # as.numeric() on a factor gives its level codes, so factor(c("5", "10",
  # "20")) would be correlated as 3, 1 and 2, and the coefficient could
  # change sign with no reason given.
  withr::local_seed(1)
  n <- 300L
  years <- sample(c(5, 8, 9, 10, 11, 12, 15, 20), n, replace = TRUE)
  score <- 0.1 * years + stats::rnorm(n)
  as_factor <- factor(as.character(years))
  codes <- as.numeric(as_factor)
  by_value <- sieve_validity_estimate(score, years)
  by_label <- sieve_validity_estimate(score, as_factor)
  # The codes give a different (here wrong-signed) coefficient, so the
  # reading below is the one being tested.
  expect_lt(stats::cor(score, codes) * stats::cor(score, years), 0)
  expect_equal(by_label$r, by_value$r, tolerance = 1e-15)
  expect_equal(c(by_label$lower, by_label$upper), c(by_value$lower, by_value$upper), tolerance = 1e-15)
  expect_identical(by_label$n, by_value$n)
  # The documented contract: reason is NA when the estimate and interval exist.
  expect_true(is.na(by_label$reason))
  # The score side is read the same way.
  expect_equal(sieve_validity_estimate(factor(as.character(years)), score)$r, by_value$r, tolerance = 1e-15)
  # Numerals stored as text are read as numbers, and a blank entry is missing.
  text <- as.character(years)
  text[c(3, 7)] <- c("", "  ")
  blank <- sieve_validity_estimate(score, text)
  expect_identical(blank$n, n - 2L)
  expect_equal(blank$r, stats::cor(score[-c(3, 7)], years[-c(3, 7)]), tolerance = 1e-12)
  expect_identical(sieve_criterion_values(factor(c("5", "10", "20", NA, ""))), c(5, 10, 20, NA, NA))

  # A factor whose labels are not numbers is refused, with the count and an
  # example, as is a character criterion with entries such as "n/a".
  likert <- factor(sample(c("Disagree", "Neutral", "Agree"), n, replace = TRUE))
  expect_error(
    sieve_validity_estimate(score, likert),
    sprintf(paste0(
      "`y` holds %d values that are not numbers (for example \"%s\"); score or recode it as numbers ",
      "before correlating. A factor is read by its labels, never by its level codes."
    ), n, as.character(likert[1])),
    fixed = TRUE
  )
  na_text <- as.character(years)
  na_text[c(10, 20)] <- "n/a"
  expect_error(sieve_validity_estimate(score, na_text),
    "`y` holds 2 values that are not numbers (for example \"n/a\")", fixed = TRUE)
  one_bad <- as.character(years)
  one_bad[4] <- "7 (approx.)"
  expect_error(sieve_validity_estimate(one_bad, score),
    "`x` holds 1 value that is not a number (for example \"7 (approx.)\")", fixed = TRUE)
  expect_error(sieve_validity_estimate(score, data.frame(y = years)),
    "`y` must be one value per case", fixed = TRUE)

  # x and y are the same cases in the same order: recycling a shorter
  # vector (which would report the longer n) is refused.
  expect_error(sieve_validity_estimate(score, years[1:100]),
    "`x` has 300 values and `y` has 100; they must be the same cases in the same order.", fixed = TRUE)
  # The level is checked, not passed on to whatever cor.test makes of it.
  expect_error(sieve_validity_estimate(score, years, conf_level = 95),
    "`conf_level` must be one number strictly between 0 and 1; received 95. A level is a proportion (0.95 for 95%), not a percentage.",
    fixed = TRUE)
  # A level given as text is refused, as every SIEVER level is (one shared
  # check), rather than read as a number.
  expect_error(sieve_validity_estimate(score, years, conf_level = "0.9"),
    "`conf_level` must be one number strictly between 0 and 1; received \"0.9\".", fixed = TRUE)
  expect_equal(sieve_validity_estimate(score, years, conf_level = 0.9)$upper,
    stats::cor.test(score, years, conf.level = 0.9)$conf.int[2], tolerance = 1e-15)
})

test_that("the recorded criterion code reads a factor criterion by its labels and writes the table's level", {
  stated <- data.frame(construct = character(0), criterion = character(0), kind = character(0),
    sign = numeric(0), min_abs = numeric(0), bound = numeric(0))
  withr::local_seed(1)
  n <- 200
  years <- sample(c(5, 8, 10, 20), n, replace = TRUE)
  items <- data.frame(a1 = 0.1 * years + stats::rnorm(n), a2 = 0.1 * years + stats::rnorm(n))
  meta <- data.frame(item = c("a1", "a2"), construct = "A", status = "active")
  d <- cbind(items, years = factor(as.character(years)))
  score <- rowMeans(items)
  for (design in list(NULL, "Rows are clustered.")) {
    code <- sieve_validity_code("years", "mean score", stated, design = design, conf_level = 0.8)
    env <- new.env(parent = globalenv())
    env$d <- d
    env$items <- items
    env$meta <- meta
    eval(parse(text = code), envir = env)
    expect_equal(env$criterion_r$r, stats::cor(score, years), tolerance = 1e-12)
    if (is.null(design)) {
      expect_true(any(grepl("conf.level = 0.8)", code, fixed = TRUE)))
      expect_false(any(grepl("0.95", code, fixed = TRUE)))
      expected <- stats::cor.test(score, years, conf.level = 0.8)$conf.int
      expect_equal(c(env$criterion_r$lower, env$criterion_r$upper), as.numeric(expected), tolerance = 1e-12)
    }
  }
  # The script stops on a criterion with entries that are not numbers.
  code <- sieve_validity_code("years", "mean score", stated, conf_level = 0.8)
  env <- new.env(parent = globalenv())
  env$d <- cbind(items, years = c("n/a", as.character(years[-1])))
  env$items <- items
  env$meta <- meta
  expect_error(eval(parse(text = code), envir = env),
    "A criterion holds entries that are not numbers (1 of them, for example \"n/a\")", fixed = TRUE)
  # The factor score form states the level in its comment and its call.
  described <- sieve_validity_code("years", "Bartlett factor score", stated, conf_level = 0.8)
  expect_true(any(grepl("(Pearson, 80% Fisher z interval)", described, fixed = TRUE)))
  expect_true(any(grepl("conf.level = 0.8)", described, fixed = TRUE)))
  expect_error(sieve_validity_code("years", "mean score", stated, conf_level = 80),
    "`conf_level` must be one number strictly between 0 and 1; received 80. A level is a proportion (0.8 for 80%), not a percentage.",
    fixed = TRUE)
})

test_that("unspecified pairs enter no tally, and a pair without a stated prediction is never discriminant", {
  estimates <- data.frame(
    construct = c("A", "A", "B", "B", "C", "C"),
    criterion = c("y1", "y2", "y1", "y2", "y1", "y2"),
    r = c(0.40, -0.58, 0.05, 0.28, 0.02, 0.30),
    lower = c(0.25, -0.64, -0.05, 0.19, -0.08, NA),
    upper = c(0.53, -0.51, 0.15, 0.37, 0.12, NA),
    reason = c(NA, NA, NA, NA, NA, "Interval unavailable: too few pairs."),
    stringsAsFactors = FALSE
  )
  expectations <- data.frame(
    construct = c("A", "A", "B", "B", "C"),
    criterion = c("y1", "y2", "y1", "y2", "y2"),
    kind = c("directional", "directional", "discriminant", "discriminant", "directional"),
    sign = c(1, 1, NA, NA, 1),
    min_abs = NA_real_,
    bound = c(NA, NA, 0.20, 0.30, NA),
    stringsAsFactors = FALSE
  )
  ev <- sieve_validity_evidence(estimates, expectations)
  expect_identical(ev$position, c(
    "predicted_side", "opposite_side", "inside_bound",
    "straddles_bound", "not_scored", "interval_unavailable"
  ))
  # (C, y1) has no row in the expectations: unspecified, not discriminant,
  # although its correlation is near zero.
  expect_identical(ev$kind[5], "unspecified")
  expect_false(ev$scored[5])
  expect_match(ev$statement[6], "too few pairs", fixed = TRUE)
  # Full precision is carried through.
  expect_identical(ev$r, estimates$r)

  tally <- sieve_validity_tally(ev)
  expect_identical(sum(tally$directional), 2L)
  expect_identical(tally$directional[["predicted_side"]], 1L)
  expect_identical(tally$directional[["opposite_side"]], 1L)
  expect_identical(sum(tally$discriminant), 2L)
  expect_identical(tally$discriminant[["inside_bound"]], 1L)
  expect_identical(tally$discriminant[["straddles_bound"]], 1L)
  expect_identical(tally$unspecified, 1L)
  expect_identical(tally$unavailable, 1L)
  # Every row is in exactly one place, and the unspecified one in no count
  # of positions.
  expect_identical(
    sum(tally$directional) + sum(tally$discriminant) + tally$unspecified + tally$unavailable,
    nrow(ev)
  )
  said <- sieve_validity_tally_text(tally)
  expect_match(said[1],
    "Directional predictions (2): the 95% interval lies entirely on the predicted side of zero for 1, includes zero for 0, and lies entirely on the opposite side for 1.",
    fixed = TRUE
  )
  expect_match(said[2], "Discriminant predictions (2)", fixed = TRUE)
  expect_true(any(grepl("Pairs with no prediction: 1. They are reported and enter none of the counts above.",
    said,
    fixed = TRUE
  )))
  expect_false(any(grepl("support", said, ignore.case = TRUE)))

  # With no expectations at all, everything is unspecified and nothing is tallied.
  bare <- sieve_validity_evidence(estimates, NULL)
  expect_true(all(bare$position == "not_scored"))
  t0 <- sieve_validity_tally(bare)
  expect_identical(sum(t0$directional), 0L)
  expect_identical(sum(t0$discriminant), 0L)
  expect_identical(t0$unspecified, 6L)
  expect_identical(
    sieve_validity_tally_text(t0),
    "Pairs with no prediction: 6. They are reported and enter none of the counts above."
  )
})

test_that("input keys are stable, safe, and distinct for names that clean to the same text", {
  k <- sieve_input_key(c("crit neg", "crit_neg", "crit.neg", "crit neg"))
  expect_true(all(grepl("^[A-Za-z0-9_]+$", k)))
  expect_identical(k[1], k[4])
  expect_length(unique(k), 3L)
  expect_identical(sieve_input_key("age_years"), sieve_input_key("age_years"))
})

test_that("the criterion table reads each estimate against the researcher's prediction", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  d <- val_data()
  st <- val_state(d)
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(
      score_method = "mean", criteria = c("crit_neg", "crit_mid"),
      htmt_on = FALSE, marker = ""
    )

    # With nothing stated, every pair is unspecified and nothing is scored.
    ev <- crit_df()
    expect_identical(nrow(ev), 4L)
    expect_true(all(ev$kind == "unspecified"))
    expect_true(all(ev$position == "not_scored"))
    expect_match(val_text(output$crit_tally), "Pairs with no prediction: 4", fixed = TRUE)
    expect_false(grepl("Directional predictions", val_text(output$crit_tally), fixed = TRUE))

    # The estimates are the correlations of the mean scores, at full precision.
    score_a <- rowMeans(d[, c("a1", "a2", "a3")])
    direct <- stats::cor.test(score_a, d$crit_neg)
    row <- ev[ev$construct == "A" & ev$criterion == "crit_neg", ]
    expect_equal(row$r, unname(direct$estimate), tolerance = 1e-12)
    expect_equal(c(row$lower, row$upper), as.numeric(direct$conf.int), tolerance = 1e-12)
    expect_identical(row$n, 400L)
    expect_lt(row$r, -0.5)

    # The first case: theory predicts a positive relation of A with
    # crit_neg, and the data give a clearly negative one.
    val_set(session, val_id("kind", "crit_neg", "A"), "positive")
    row <- crit_df()[1, ]
    expect_identical(c(row$construct, row$criterion), c("A", "crit_neg"))
    expect_identical(row$kind, "directional")
    expect_identical(row$sign, 1)
    expect_identical(row$position, "opposite_side")
    # The second case: no prediction was made for B with crit_mid,
    # so it is not a discriminant prediction and is not scored.
    other <- crit_df()
    b_mid <- other[other$construct == "B" & other$criterion == "crit_mid", ]
    expect_identical(b_mid$kind, "unspecified")
    expect_identical(b_mid$position, "not_scored")
    expect_true(all(other$kind[-1] == "unspecified"))

    # A discriminant prediction with the researcher's own bound: r is below
    # .30 and the interval runs past it.
    val_set(session, val_id("kind", "crit_mid", "B"), "discriminant")
    val_set(session, val_id("bound", "crit_mid", "B"), 0.30)
    b_mid <- crit_df()[4, ]
    expect_identical(b_mid$kind, "discriminant")
    expect_lt(abs(b_mid$r), 0.30)
    expect_gt(b_mid$upper, 0.30)
    expect_identical(b_mid$position, "straddles_bound")
    val_set(session, val_id("bound", "crit_mid", "B"), 0.50)
    expect_identical(crit_df()[4, "position"], "inside_bound")
    # A discriminant choice without a usable bound is incomplete, not scored.
    val_set(session, val_id("bound", "crit_mid", "B"), NA)
    expect_identical(crit_df()[4, "position"], "prediction_incomplete")
    val_set(session, val_id("bound", "crit_mid", "B"), 0.30)

    # A direction with a smallest magnitude.
    val_set(session, val_id("kind", "crit_neg", "A"), "negative")
    val_set(session, val_id("min", "crit_neg", "A"), 0.30)
    expect_identical(crit_df()[1, "position"], "at_or_beyond_minimum")
    expect_identical(crit_df()[1, "min_abs"], 0.30)

    # What is displayed: the prediction, the interval, and a reading, with
    # no verdict words; the tally counts only the stated predictions.
    shown <- crit_shown()
    expect_identical(shown$Prediction, c(
      "negative, |r| at least 0.3", "none stated",
      "none stated", "small, |r| below 0.3"
    ))
    expect_identical(shown$Reading, crit_df()$statement)
    expect_identical(shown$r, crit_df()$r) # not rounded before the renderer
    expect_false(any(grepl("supported|concern|convergent|weak", unlist(shown[c("Prediction", "Reading")]))))
    expect_match(val_text(output$crit_tbl), "The interval in relation to your prediction", fixed = TRUE)
    expect_false(grepl("Support", val_text(output$crit_tbl), fixed = TRUE))
    tally <- val_text(output$crit_tally)
    expect_match(tally, "Directional predictions (1)", fixed = TRUE)
    expect_match(tally, "Discriminant predictions (1)", fixed = TRUE)
    expect_match(tally, "Pairs with no prediction: 2", fixed = TRUE)
    expect_match(tally, "They are not a score for the scale", fixed = TRUE)
    note <- val_text(output$crit_note)
    # The score is named as the specification labels it.
    expect_match(note, "Pearson correlation between a construct score (mean of the answered items) and a criterion", fixed = TRUE)
    expect_match(note, "95% Fisher z confidence interval", fixed = TRUE)
    # How to read the table, with the three
    # kinds of prediction, is below it (crit_more).
    expect_match(val_text(output$crit_more), "is not a prediction that it is small", fixed = TRUE)

    # A stated prediction stays with its pair when the selection changes:
    # removing the first criterion must not hand its prediction to the next.
    session$setInputs(criteria = "crit_mid")
    only <- crit_df()
    expect_identical(only$criterion, c("crit_mid", "crit_mid"))
    expect_identical(only$kind, c("unspecified", "discriminant"))
    session$setInputs(criteria = c("crit_neg", "crit_mid"))
    expect_identical(crit_df()$kind, c("directional", "unspecified", "unspecified", "discriminant"))

    # The predictions are written to the reproducible code as comments, and
    # the recorded R reproduces the estimates from the data and the ledger.
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("#   A with crit_neg: negative, |r| at least 0.3", log, fixed = TRUE)))
    expect_true(any(grepl("#   B with crit_mid: small, |r| below 0.3", log, fixed = TRUE)))
    last <- max(which(grepl("^# \\[[0-9]+\\] Validity: construct to criterion correlations", log)))
    step <- log[(last + 1L):length(log)]
    step <- step[seq_len(which(step == "")[1] - 1L)]
    env <- new.env(parent = globalenv())
    env$d <- d
    env$items <- d[, 1:6]
    env$meta <- shiny::isolate(state$meta)
    eval(parse(text = step), envir = env)
    now <- crit_df()
    expect_identical(env$criterion_r$construct, now$construct)
    expect_identical(env$criterion_r$criterion, now$criterion)
    expect_equal(env$criterion_r$r, now$r, tolerance = 1e-12)
    expect_equal(env$criterion_r$lower, now$lower, tolerance = 1e-12)
    expect_equal(env$criterion_r$upper, now$upper, tolerance = 1e-12)

    # Under a dependent design the note says the correlations are
    # descriptive and gives no interval, as the table does, and it follows
    # a design changed after the estimates.
    state$sampling_design <- list(unit = "clustered", cluster = NULL)
    session$flushReact()
    note <- val_text(output$crit_note)
    expect_match(note, "descriptive Pearson correlation", fixed = TRUE)
    expect_match(note, "The declared clustered sampling unit makes the rows dependent", fixed = TRUE)
    expect_false(grepl("Fisher z confidence interval", note, fixed = TRUE))
    expect_true(all(is.na(crit_df()$lower)))
    state$sampling_design <- list(unit = "independent")
    session$flushReact()
    expect_match(val_text(output$crit_note), "95% Fisher z confidence interval", fixed = TRUE)
    expect_true(any(is.finite(crit_df()$lower)))
  })
})

test_that("the page's confidence level sets, labels and records every interval on the Validity page (level .80)", {
  # At a page level of .80 the criterion table holds 80% limits under "80%
  # CI" headers, the note and the recorded step say 80%, the recorded
  # cor.test() uses conf.level = 0.8, and the latent correlations follow
  # the page's level, not lavaan's 95%, under headers that name it.
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  d <- val_data()
  st <- val_state(d)
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(
      score_method = "mean", criteria = c("crit_neg", "crit_mid"),
      htmt_on = FALSE, marker = "", conf_level = 0.80
    )
    expect_identical(page_level(), 0.80)
    ev <- crit_df()
    score_a <- rowMeans(d[, c("a1", "a2", "a3")])
    at80 <- as.numeric(stats::cor.test(score_a, d$crit_neg, conf.level = 0.80)$conf.int)
    at95 <- as.numeric(stats::cor.test(score_a, d$crit_neg, conf.level = 0.95)$conf.int)
    row <- ev[ev$construct == "A" & ev$criterion == "crit_neg", ]
    expect_equal(c(row$lower, row$upper), at80, tolerance = 1e-12)
    expect_gt(min(abs(c(row$lower, row$upper) - at95)), 1e-3)

    # The headers, the note and the counts name the level the limits are at.
    tbl <- val_text(output$crit_tbl)
    expect_match(tbl, "80% CI lower", fixed = TRUE)
    expect_match(tbl, "80% CI upper", fixed = TRUE)
    expect_false(grepl("95%", tbl, fixed = TRUE))
    note <- val_text(output$crit_note)
    expect_match(note, "with its two-sided 80% Fisher z confidence interval (the page's confidence level)", fixed = TRUE)
    expect_false(grepl("95%", note, fixed = TRUE))
    val_set(session, val_id("kind", "crit_neg", "A"), "negative")
    tally <- val_text(output$crit_tally)
    expect_match(tally, "the 80% interval lies entirely on the predicted side", fixed = TRUE)
    expect_false(grepl("95%", tally, fixed = TRUE))

    # The recorded step says 80%, its cor.test() calls use conf.level = 0.8,
    # and replayed on the data it returns the limits in the table.
    invisible(output$crit_tbl)
    log <- shiny::isolate(state$codelog)
    last <- max(which(grepl("^# \\[[0-9]+\\] Validity: construct to criterion correlations", log)))
    expect_match(log[last], "(mean of the answered items; Pearson, two-sided 80% Fisher z interval)", fixed = TRUE)
    step <- log[(last + 1L):length(log)]
    step <- step[seq_len(which(step == "")[1] - 1L)]
    expect_true(any(grepl("stats::cor.test(construct_scores[[cn]], y, conf.level = 0.8)", step, fixed = TRUE)))
    expect_false(any(grepl("conf.level = 0.95", step, fixed = TRUE)))
    env <- new.env(parent = globalenv())
    env$d <- d
    env$items <- d[, 1:6]
    env$meta <- shiny::isolate(state$meta)
    eval(parse(text = step), envir = env)
    now <- crit_df()
    expect_identical(env$criterion_r$construct, now$construct)
    expect_identical(env$criterion_r$criterion, now$criterion)
    expect_equal(env$criterion_r$lower, now$lower, tolerance = 1e-12)
    expect_equal(env$criterion_r$upper, now$upper, tolerance = 1e-12)

    # The latent correlations: computed at the page's level, headed with it,
    # and recorded with it.
    session$setInputs(fit_go = 1)
    f <- fit()
    expect_false(is.null(f))
    lv <- lavaan::lavNames(f, "lv")
    pick <- function(s) s[s$op == "~~" & s$lhs != s$rhs & s$lhs %in% lv & s$rhs %in% lv, , drop = FALSE]
    s80 <- pick(lavaan::standardizedSolution(f, level = 0.80))
    s95 <- pick(lavaan::standardizedSolution(f))
    read <- phi_solution()
    expect_true(read$ok)
    got <- pick(read$value)
    expect_equal(c(got$ci.lower, got$ci.upper), c(s80$ci.lower, s80$ci.upper), tolerance = 1e-12)
    expect_gt(abs(got$ci.lower - s95$ci.lower), 1e-3)
    phi_html <- val_text(output$phi_tbl)
    expect_match(phi_html, "80% CI lower", fixed = TRUE)
    expect_match(phi_html, "80% CI upper", fixed = TRUE)
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl(
      "Validity: inter-construct latent correlations from the standardized CFA solution, with two-sided 80% confidence intervals.",
      log, fixed = TRUE
    )))
    ss_line <- tail(grep("^validity_ss <- ", log, value = TRUE), 1L)
    expect_identical(ss_line, "validity_ss <- lavaan::standardizedSolution(validity_fit, level = 0.8)")
    replay <- new.env(parent = globalenv())
    replay$validity_fit <- f
    eval(parse(text = ss_line), envir = replay)
    replayed <- pick(replay$validity_ss)
    expect_equal(c(replayed$ci.lower, replayed$ci.upper), c(got$ci.lower, got$ci.upper), tolerance = 1e-12)

    # A level outside (0, 1) is refused with the reason, and nothing on the
    # page is computed at a substituted level.
    session$setInputs(conf_level = 95)
    expect_error(crit_df(), "The page's confidence level is 95; it must be a number strictly between 0 and 1", fixed = TRUE)
    expect_error(phi_solution(), "The page's confidence level is 95", fixed = TRUE)
    # Each table shows a note (the reason, from the errors above) in place
    # of limits.
    for (shown in list(val_text(output$crit_tbl), val_text(output$phi_tbl))) {
      expect_match(shown, "<th>Note<\\/th>", fixed = TRUE)
      expect_false(grepl("CI lower", shown, fixed = TRUE))
    }
    # An empty input is the default level.
    session$setInputs(conf_level = NA)
    expect_identical(page_level(), sieve_default_conf_level())
    expect_match(val_text(output$crit_tbl), "95% CI lower", fixed = TRUE)
  })
})

test_that("the HTMT bound's recorded call and its readout comment state the page's level", {
  skip_if_not_installed("DMAR")
  d <- val_data()
  st <- val_state(d)
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(
      score_method = "mean", htmt_on = TRUE, marker = "",
      htmt_B = 100L, htmt_thresh = 0.85, htmt_seed = 7, conf_level = 0.80
    )
    expect_match(val_text(output$htmt_boot_plan), "upper bound (DMAR::htmt) at the 80% level", fixed = TRUE)
    session$setInputs(htmt_boot = 1)
    r <- htmt_ci()
    expect_null(r$error)
    expect_identical(r$level, 0.80)
    expect_true(any(grepl("conf_level = 0.8, seed = 7)", shiny::isolate(state$codelog), fixed = TRUE)))
    code <- val_text(output$htmt_ci_code)
    expect_match(code, "one-sided 80% percentile upper bound", fixed = TRUE)
    expect_false(grepl("one-sided 95%", code, fixed = TRUE))
  })
})

test_that("a criterion loaded as a factor is correlated on its labels, as the gate that offers it reads it", {
  # A data frame given to sieve_prepare_study() keeps a factor column; the
  # gate reads its labels and offers it, so the table must correlate its
  # labels, not its level codes.
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  d <- val_data()
  years <- withr::with_seed(1, sample(c(5, 8, 9, 10, 11, 12, 15, 20), nrow(d), replace = TRUE))
  d$a1 <- d$a1 + 0.15 * (years - mean(years))
  d$years_school <- factor(as.character(years))
  d$rating <- factor(withr::with_seed(2, sample(c("low", "mid", "high"), nrow(d), replace = TRUE)))
  st <- val_state(d)
  expect_true(is.factor(shiny::isolate(st$raw)$years_school))
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(score_method = "mean", htmt_on = FALSE, marker = "")
    expect_true("years_school" %in% aux_numeric())
    # A factor whose labels are not numbers, given the criterion role, is not
    # offered, with the reason.
    expect_null(sieve_apply_roles(state, c(rating = "criterion"))$refused)
    session$flushReact()
    expect_false("rating" %in% aux_numeric())
    expect_match(aux_refused()[["rating"]], "holds 400 values that are not numbers", fixed = TRUE)
    # A role change asks for the types to be confirmed again.
    sieve_confirm_types(state)
    session$setInputs(criteria = "years_school")
    ev <- crit_df()
    score_a <- rowMeans(d[, c("a1", "a2", "a3")])
    by_label <- stats::cor.test(score_a, years)
    by_code <- stats::cor(score_a, as.numeric(d$years_school))
    row <- ev[ev$construct == "A", ]
    expect_equal(row$r, unname(by_label$estimate), tolerance = 1e-12)
    expect_equal(c(row$lower, row$upper), as.numeric(by_label$conf.int), tolerance = 1e-12)
    expect_gt(abs(row$r - by_code), 0.05)
    # The recorded R reads the labels too, so replay gives the table.
    invisible(output$crit_tbl)
    log <- shiny::isolate(state$codelog)
    last <- max(which(grepl("^# \\[[0-9]+\\] Validity: construct to criterion correlations", log)))
    step <- log[(last + 1L):length(log)]
    step <- step[seq_len(which(step == "")[1] - 1L)]
    expect_true(any(grepl("y <- criterion_values(d[[cr]])", step, fixed = TRUE)))
    env <- new.env(parent = globalenv())
    env$d <- d
    env$items <- d[, 1:6]
    env$meta <- shiny::isolate(state$meta)
    eval(parse(text = step), envir = env)
    expect_equal(env$criterion_r$r, crit_df()$r, tolerance = 1e-12)
    expect_equal(env$criterion_r$lower, crit_df()$lower, tolerance = 1e-12)
    # The marker is read the same way.
    session$setInputs(marker = "years_school")
    expect_equal(marker_df()$marker_r[["A"]], stats::cor(score_a, years), tolerance = 1e-12)
  })
})

test_that("the validity tab states conventions as conventions and prints no threshold verdict", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  ui <- gsub("\\s+", " ", as.character(mod_validity_ui("val")))
  expect_false(grepl("Discriminant validity is supported", ui, fixed = TRUE))
  expect_false(grepl("less attenuated", ui, fixed = TRUE))
  # No cutoff in output: the HTMT convention is named with its source, and
  # SIEVER applies none.
  expect_match(ui, "Henseler, Ringle and Sarstedt (2015) proposed 0.85, and 0.90 for conceptually similar constructs, as conventions for HTMT; SIEVER applies no cutoff", fixed = TRUE)
  expect_false(grepl("htmt_thresh", ui, fixed = TRUE))
  expect_false(grepl("Value to read the upper bound against", ui, fixed = TRUE))
  expect_match(ui, "none of them declares a scale valid", fixed = TRUE)
  # The sidebar offers Bartlett and regression scores.
  expect_match(ui, "factor scores do not avoid that", fixed = TRUE)

  st <- val_state()
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(
      score_method = "mean", criteria = "crit_mid", htmt_on = TRUE, marker = "",
      htmt_B = 200, htmt_thresh = 0.85, htmt_seed = 11
    )
    session$setInputs(fit_go = 1)
    # The prediction controls start with no prediction and label the bound
    # as a convention.
    ex <- val_text(output$expectations)
    expect_match(ex, "No prediction", fixed = TRUE)
    expect_match(ex, ".30 is a convention, not a rule", fixed = TRUE)
    expect_match(ex, sprintf('<option value="unspecified" selected'), fixed = TRUE)

    # Fornell and Larcker's comparison is named once and not made: sqrt(AVE)
    # and the largest |r| with another construct, and which construct that
    # is, side by side, with no column that reads "sqrt(AVE) not above
    # max |r|" and no shading.
    ave <- ave_df()
    expect_identical(names(ave), c("Construct", "AVE", "sqrt.AVE", "Max.r.with.other", "With"))
    expect_false("Fornell.Larcker" %in% names(ave))
    expect_equal(ave$sqrt.AVE, sqrt(ave$AVE))
    expect_true(all(ave$With %in% ave$Construct))
    expect_false(any(ave$With == ave$Construct))
    ss <- lavaan::standardizedSolution(fit())
    lv <- lavaan::lavNames(fit(), "lv")
    phi <- ss[ss$op == "~~" & ss$lhs != ss$rhs & ss$lhs %in% lv & ss$rhs %in% lv, ]
    phi$lhs <- sieve_show_names(phi$lhs, fit())
    phi$rhs <- sieve_show_names(phi$rhs, fit())
    for (i in seq_len(nrow(ave))) {
      pair <- (phi$lhs == ave$Construct[i] & phi$rhs == ave$With[i]) |
        (phi$rhs == ave$Construct[i] & phi$lhs == ave$With[i])
      expect_equal(abs(phi$est.std[pair]), ave$Max.r.with.other[i])
      own <- phi$lhs == ave$Construct[i] | phi$rhs == ave$Construct[i]
      expect_equal(max(abs(phi$est.std[own])), ave$Max.r.with.other[i])
    }
    tbl <- val_text(output$ave_tbl)
    for (gone in c("fdf3d6", "not above max", "above max |r|", "Fornell-Larcker comparison")) {
      expect_false(grepl(gone, tbl, fixed = TRUE))
    }
    expect_false(grepl("\"pass\"|\"fail\"", tbl))
    expect_match(tbl, "Fornell and Larcker (1981) compare the square root of each construct's AVE", fixed = TRUE)
    expect_match(tbl, "SIEVER applies no cutoff", fixed = TRUE)
    expect_identical(lengths(regmatches(tbl, gregexpr("Fornell", tbl, fixed = TRUE))), 1L)

    # Harman's screen reports the value alone, with no row that reads it
    # against the .50 convention ("above it" or "not above it").
    expect_identical(harman_df()$Statistic, c("Active items", "Proportion variance, factor 1"))
    expect_false(any(grepl("above it|Conventional value", unlist(harman_df()))))

    # The HTMT interval table gives each bound and reads none against a
    # cutoff: no column says whether each bound lies below a value entered,
    # and nothing is shaded.
    session$setInputs(htmt_boot = 1)
    boot <- htmt_ci()
    expect_null(boot$error)
    expect_null(boot$thr)
    tbl <- val_text(output$htmt_ci_tbl)
    expect_match(tbl, "One-sided 95% upper bound", fixed = TRUE)
    expect_false(grepl("against the value entered|below the value|#fdf3d6", tbl))
    expect_false(grepl("supported|concern", tbl))
    # The point table is not shaded above any value.
    pairs <- val_text(output$htmt_tbl)
    expect_false(grepl("#f8e3e3", pairs, fixed = TRUE))
    expect_false(grepl("highlighted", val_text(output$htmt_note), fixed = TRUE))
    expect_match(val_text(output$htmt_note), "SIEVER applies no cutoff", fixed = TRUE)
  })
})

test_that("in the browser a pair starts without a prediction and each kind shows the magnitude that belongs to it", {
  skip_if_no_browser()
  # Chrome gets a minute to start and to answer (chromote waits 10 seconds
  # by default), and up to three attempts to start.
  withr::local_options(list(chromote.timeout = 60))
  skip_if_not(val_chrome_ready(), "Chrome did not start in three attempts")

  # What testServer cannot see: the prediction controls are generated in the
  # page with namespaced identifiers, and which magnitude input is visible
  # is decided by a condition evaluated in the browser.
  siever_with_app(load_example = TRUE, function(app) {
    expect_identical(siever_open_tab(app, "validity"), "validity")
    app$set_inputs(`validity-criteria` = "age_years")
    app$click(selector = "a.nav-link[data-value='Criterion validity']")
    siever_wait_render(app, timeout = 120 * 1000)

    id <- function(what) paste0("validity-", val_id(what, "age_years", "Visual"))
    shown <- function(what) {
      isTRUE(app$get_js(sprintf(
        "(function(){var e=document.getElementById('%s');return !!e && e.getClientRects().length > 0;})()",
        id(what)
      )))
    }
    expect_identical(app$get_value(input = id("kind")), "unspecified")
    expect_false(shown("min"))
    expect_false(shown("bound"))
    expect_identical(siever_n_table_rows(app, "validity-crit_tbl"), 3L)
    expect_match(app$get_text("#validity-crit_tally"), "Pairs with no prediction: 3", fixed = TRUE)
    expect_match(app$get_text("#validity-crit_tbl"), "No prediction stated", fixed = TRUE)

    do.call(app$set_inputs, stats::setNames(list("discriminant"), id("kind")))
    siever_wait_render(app, timeout = 60 * 1000)
    expect_true(shown("bound"))
    expect_false(shown("min"))
    expect_equal(app$get_value(input = id("bound")), 0.3)
    expect_match(app$get_text("#validity-crit_tally"), "Discriminant predictions (1)", fixed = TRUE)
    expect_match(app$get_text("#validity-crit_tally"), "Pairs with no prediction: 2", fixed = TRUE)

    do.call(app$set_inputs, stats::setNames(list("positive"), id("kind")))
    siever_wait_render(app, timeout = 60 * 1000)
    expect_true(shown("min"))
    expect_false(shown("bound"))
    expect_match(app$get_text("#validity-crit_tally"), "Directional predictions (1)", fixed = TRUE)
    page <- app$get_text("#validity-crit_tbl")
    expect_false(grepl("supported|concern", page))
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

# ---- Stored predictions and the controls drawn from them -------------------

test_that("stated predictions are kept in a store, each part on its own, and read back as expectations", {
  expect_identical(sieve_prediction_default(), list(choice = "unspecified", min_abs = NA_real_, bound = 0.30))
  empty <- sieve_predictions_empty()
  expect_identical(names(empty), c("construct", "criterion", "choice", "min_abs", "bound"))
  expect_identical(nrow(empty), 0L)
  # A pair that is not in the store has no prediction.
  expect_identical(sieve_get_prediction(NULL, "A", "y"), sieve_prediction_default())
  expect_identical(sieve_get_prediction(empty, "A", "y"), sieve_prediction_default())

  s <- sieve_set_prediction(NULL, "A", "y", choice = "negative", min_abs = 0.1)
  expect_identical(nrow(s), 1L)
  expect_identical(sieve_get_prediction(s, "A", "y"), list(choice = "negative", min_abs = 0.1, bound = 0.30))
  # Only what is supplied is written: a part that is NULL keeps its value.
  s <- sieve_set_prediction(s, "A", "y", bound = 0.15)
  expect_identical(sieve_get_prediction(s, "A", "y"), list(choice = "negative", min_abs = 0.1, bound = 0.15))
  expect_identical(sieve_set_prediction(s, "A", "y"), s)
  # A cleared magnitude is NA, which is a value, and is stored as missing.
  s2 <- sieve_set_prediction(s, "A", "y", min_abs = NA)
  expect_true(is.na(sieve_get_prediction(s2, "A", "y")$min_abs))
  expect_identical(sieve_get_prediction(s2, "A", "y")$bound, 0.15)
  # Pairs are told apart by construct and by criterion.
  s <- sieve_set_prediction(s, "B", "y", choice = "discriminant", bound = 0.2)
  s <- sieve_set_prediction(s, "A", "z", choice = "positive")
  expect_identical(nrow(s), 3L)
  expect_identical(sieve_get_prediction(s, "A", "z")$choice, "positive")
  expect_identical(sieve_get_prediction(s, "B", "z"), sieve_prediction_default())
  expect_error(sieve_set_prediction(s, "A", "y", choice = "supported"), "choice")
  # Full precision in the store.
  expect_identical(
    sieve_get_prediction(sieve_set_prediction(s, "B", "y", bound = 0.1234567), "B", "y")$bound,
    0.1234567
  )

  # As expectations: every construct with every criterion, criterion by
  # criterion; a magnitude belongs to its own kind and to no other.
  ex <- sieve_predictions_expectations(s, c("A", "B"), c("y", "z"))
  expect_identical(ex$construct, c("A", "B", "A", "B"))
  expect_identical(ex$criterion, c("y", "y", "z", "z"))
  expect_identical(ex$kind, c("directional", "discriminant", "directional", "unspecified"))
  expect_identical(ex$sign, c(-1, NA, 1, NA))
  expect_identical(ex$min_abs, c(0.1, NA, NA, NA)) # A-y also holds a bound, which is not applied
  expect_identical(ex$bound, c(NA, 0.2, NA, NA))
  # Changing the kind does not discard the magnitude entered for the other.
  back <- sieve_set_prediction(
    sieve_set_prediction(s,
      "A",
      "y",
      choice = "discriminant"
    ),
    "A",
    "y",
    choice = "negative"
  )
  expect_identical(sieve_predictions_expectations(back, "A", "y")$min_abs, 0.1)
  expect_identical(sieve_predictions_expectations(
    sieve_set_prediction(s, "A", "y", choice = "discriminant"),
    "A", "y"
  )$bound, 0.15)
  # Setting a pair back to no prediction is stored as that.
  none <- sieve_set_prediction(s, "A", "y", choice = "unspecified")
  expect_identical(sieve_predictions_expectations(none, "A", "y")$kind, "unspecified")

  # Every stated pair, and the stated pairs a table does not show.
  all_stated <- sieve_predictions_expectations(s)
  expect_identical(paste(all_stated$construct, all_stated$criterion), c("A y", "B y", "A z"))
  off <- sieve_predictions_elsewhere(s, constructs = c("A", "B"), criteria = "z")
  expect_identical(paste(off$construct, off$criterion), c("A y", "B y"))
  expect_identical(nrow(sieve_predictions_elsewhere(s, c("A", "B"), c("y", "z"))), 0L)
  expect_identical(nrow(sieve_predictions_elsewhere(s, "A", c("y", "z"))), 1L) # construct B is not in the table
  expect_identical(nrow(sieve_predictions_elsewhere(NULL, "A", "y")), 0L)
  expect_identical(nrow(sieve_predictions_expectations(NULL)), 0L)
  # The read functions agree with the classification they feed.
  ev <- sieve_validity_evidence(
    data.frame(construct = "A", criterion = "y", r = -0.4, lower = -0.5, upper = -0.3, stringsAsFactors = FALSE),
    sieve_predictions_expectations(s, "A", "y")
  )
  expect_identical(ev$position, "at_or_beyond_minimum")
})

test_that("a magnitude the researcher entered is written back exactly, in the reading and in the script", {
  expect_identical(sieve_magnitude_text(0.1234), "0.1234")
  expect_identical(sieve_magnitude_text(0.3), "0.3")
  expect_identical(sieve_magnitude_text(0.15), "0.15")
  expect_identical(sieve_validity_prediction_text("discriminant", bound = 0.1234), "small, |r| below 0.1234")
  expect_identical(
    sieve_validity_prediction_text("directional", sign = -1, min_abs = 0.1234),
    "negative, |r| at least 0.1234"
  )
  expect_match(val_classify(0.05, -0.05, 0.10, "discriminant", bound = 0.1234)$statement,
    "within plus or minus 0.1234.",
    fixed = TRUE
  )
  expect_match(val_classify(0.4, 0.3, 0.5, "directional", sign = 1, min_abs = 0.1234)$statement,
    "at least 0.1234 in magnitude",
    fixed = TRUE
  )
  # The reading itself uses the number, not its text: an upper limit of
  # 0.1233 is inside a bound of 0.1234 and outside one of 0.123.
  expect_identical(val_classify(0.05, -0.05, 0.1233, "discriminant", bound = 0.1234)$position, "inside_bound")
  expect_identical(val_classify(0.05, -0.05, 0.1233, "discriminant", bound = 0.123)$position, "straddles_bound")
  stated <- data.frame(
    construct = "A", criterion = "y", kind = "discriminant", sign = NA_real_,
    min_abs = NA_real_, bound = 0.1234, stringsAsFactors = FALSE
  )
  off <- data.frame(
    construct = "B", criterion = "w", kind = "directional", sign = 1,
    min_abs = 0.25, bound = NA_real_, stringsAsFactors = FALSE
  )
  code <- sieve_validity_code("y", "mean score", stated, off)
  expect_true("#   A with y: small, |r| below 0.1234" %in% code)
  # Predictions for pairs the table does not show are written too.
  at <- which(code == "# Also stated, for pairs that are not in this table (kept, and applied when")
  expect_length(at, 1L)
  expect_identical(code[at + 2L], "#   B with w: positive, |r| at least 0.25")
  expect_false(any(grepl("Also stated", sieve_validity_code("y", "mean score", stated), fixed = TRUE)))
  expect_false(any(grepl("Also stated", sieve_validity_code("y", "mean score", stated, off[0, ]), fixed = TRUE)))
  expect_silent(parse(text = code))
})

test_that("the prediction controls are drawn from the stored predictions when the page rebuilds them", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  # The controls are rebuilt when the selected criteria or the kind of
  # score change; rebuilt with fixed starting values, every stated
  # prediction would go back to "No prediction" in the browser. testServer
  # has no browser, but it does show what the rebuilt controls are given,
  # which decides it.
  selected <- function(html, id) {
    block <- regmatches(html, regexpr(sprintf('(?s)<select[^>]*id="%s".*?</select>', id), html, perl = TRUE))
    regmatches(block, regexpr('(?<=<option value=")[a-z]+(?=" selected)', block, perl = TRUE))
  }
  # The value a numeric control is drawn with ("NA" is how an empty one is written).
  value_of <- function(html, id) {
    tag <- regmatches(html, regexpr(sprintf('<input[^>]*id="%s"[^>]*>', id), html, perl = TRUE))
    sub('.*value="([^"]*)".*', "\\1", tag)
  }
  kept_note <- function(output) tryCatch(val_text(output$crit_kept), error = function(e) "")
  d <- val_data()
  st <- val_state(d)
  shiny::testServer(mod_validity_server, args = list(state = st), {
    ns <- session$ns
    session$setInputs(score_method = "mean", criteria = "crit_neg", htmt_on = FALSE, marker = "")
    expect_null(shiny::isolate(state$predictions))
    html <- val_text(output$expectations)
    expect_identical(selected(html, ns(val_id("kind", "crit_neg", "A"))), "unspecified")
    expect_identical(value_of(html, ns(val_id("min", "crit_neg", "A"))), "NA")
    expect_identical(value_of(html, ns(val_id("bound", "crit_neg", "B"))), "0.3")

    # What a browser sends as the researcher states two predictions.
    val_set(session, val_id("kind", "crit_neg", "A"), "negative")
    val_set(session, val_id("min", "crit_neg", "A"), 0.1)
    val_set(session, val_id("kind", "crit_neg", "B"), "discriminant")
    val_set(session, val_id("bound", "crit_neg", "B"), 0.15)
    held <- shiny::isolate(state$predictions)
    expect_identical(
      sieve_get_prediction(held, "A", "crit_neg")[c("choice", "min_abs")],
      list(choice = "negative", min_abs = 0.1)
    )
    expect_identical(
      sieve_get_prediction(held, "B", "crit_neg")[c("choice", "bound")],
      list(choice = "discriminant", bound = 0.15)
    )
    before <- crit_df()

    # A criterion is added: the controls are rebuilt, and they are rebuilt
    # with what was stated.
    session$setInputs(criteria = c("crit_neg", "crit_mid"))
    html <- val_text(output$expectations)
    expect_identical(selected(html, ns(val_id("kind", "crit_neg", "A"))), "negative")
    expect_identical(value_of(html, ns(val_id("min", "crit_neg", "A"))), "0.1")
    expect_identical(selected(html, ns(val_id("kind", "crit_neg", "B"))), "discriminant")
    expect_identical(value_of(html, ns(val_id("bound", "crit_neg", "B"))), "0.15")
    expect_identical(selected(html, ns(val_id("kind", "crit_mid", "A"))), "unspecified") # the new pairs start empty
    # The browser then reports the rebuilt controls. They hold the stored
    # values, so the report changes nothing; with fixed starting values it
    # would erase the predictions here.
    for (pair in list(c("crit_neg", "A"), c("crit_neg", "B"), c("crit_mid", "A"), c("crit_mid", "B"))) {
      kind_id <- val_id("kind", pair[1], pair[2])
      val_set(session, kind_id, selected(html, ns(kind_id)))
      for (what in c("min", "bound")) {
        raw <- value_of(html, ns(val_id(what, pair[1], pair[2])))
        val_set(session, val_id(what, pair[1], pair[2]), if (identical(raw, "NA")) NA else as.numeric(raw))
      }
    }
    now <- crit_df()
    expect_identical(now[now$criterion == "crit_neg", c("kind", "sign", "min_abs", "bound", "position")],
      before[, c("kind", "sign", "min_abs", "bound", "position")],
      ignore_attr = TRUE
    )
    expect_true(all(now$kind[now$criterion == "crit_mid"] == "unspecified"))
    tally <- val_text(output$crit_tally)
    expect_match(tally, "Directional predictions (1)", fixed = TRUE)
    expect_match(tally, "Discriminant predictions (1)", fixed = TRUE)
    expect_match(tally, "Pairs with no prediction: 2", fixed = TRUE)

    # The kind of score changes: rebuilt again, with the same values.
    session$setInputs(score_method = "bartlett")
    session$setInputs(fit_go = 1)
    expect_match(construct_scores()$method, "Bartlett", fixed = TRUE)
    html <- val_text(output$expectations)
    expect_identical(selected(html, ns(val_id("kind", "crit_neg", "A"))), "negative")
    expect_identical(value_of(html, ns(val_id("bound", "crit_neg", "B"))), "0.15")
    bart <- crit_df()
    expect_identical(bart$kind, c("directional", "discriminant", "unspecified", "unspecified"))
    expect_true(all(is.finite(bart$r))) # the Bartlett path computes
    expect_false(isTRUE(all.equal(bart$r, now$r))) # and is a different score
    session$setInputs(score_method = "mean")

    # A criterion is taken out: its predictions are kept, named on the
    # page and in the script, and come back with it.
    session$setInputs(criteria = "crit_mid")
    expect_true(all(crit_df()$kind == "unspecified"))
    kept <- kept_note(output)
    expect_match(kept, "Also stated, for pairs not in this table:", fixed = TRUE)
    expect_match(kept, "A with crit_neg (negative, |r| at least 0.1)", fixed = TRUE)
    expect_match(kept, "B with crit_neg (small, |r| below 0.15)", fixed = TRUE)
    invisible(output$crit_tbl)
    log <- shiny::isolate(state$codelog)
    last <- max(which(grepl("^# \\[[0-9]+\\] Validity: construct to criterion correlations", log)))
    step <- log[(last + 1L):length(log)]
    step <- step[seq_len(which(step == "")[1] - 1L)]
    expect_match(log[last], "(0 stated, 2 pairs without one)", fixed = TRUE)
    expect_true("#   none" %in% step)
    expect_true("#   A with crit_neg: negative, |r| at least 0.1" %in% step)
    expect_true(which(step == "#   A with crit_neg: negative, |r| at least 0.1") >
      which(grepl("^# Also stated", step)))
    session$setInputs(criteria = c("crit_mid", "crit_neg"))
    html <- val_text(output$expectations)
    expect_identical(selected(html, ns(val_id("kind", "crit_neg", "A"))), "negative")
    back <- crit_df()
    expect_identical(back$kind[back$criterion == "crit_neg"], c("directional", "discriminant"))
    expect_identical(kept_note(output), "")
    # Through all of it nothing that was stated moved; the pairs that were
    # only shown are in the store as what they are, without a prediction.
    final <- shiny::isolate(state$predictions)
    expect_identical(final[1:2, ], held, ignore_attr = TRUE)
    expect_identical(sieve_predictions_expectations(final), sieve_predictions_expectations(held))
    expect_true(all(final$choice[final$criterion == "crit_mid"] == "unspecified"))

    # One stated, one not: the note in the script counts in the singular.
    session$setInputs(criteria = "crit_neg")
    val_set(session, val_id("kind", "crit_neg", "B"), "unspecified")
    invisible(output$crit_tbl)
    log <- shiny::isolate(state$codelog)
    last <- max(which(grepl("^# \\[[0-9]+\\] Validity: construct to criterion correlations", log)))
    expect_match(log[last], "(1 stated, 1 pair without one)", fixed = TRUE)
  })

  # No wording reads a value against a threshold.
  ui <- gsub("\\s+", " ", as.character(mod_validity_ui("val")))
  # (There is no HTMT input "Value to read the upper bound against": no
  # bound is read against a cutoff.)
  expect_false(grepl("Value to read the upper bound against", ui, fixed = TRUE))
  expect_false(grepl(">Threshold<", ui, fixed = TRUE))
  src <- paste(deparse(mod_validity_server), collapse = "\n")
  expect_false(grepl("flagged", src, fixed = TRUE))
  expect_false(grepl("vs. threshold", src, fixed = TRUE))
})

test_that("in the browser stated predictions survive a new criterion, another score, and a criterion taken out and put back", {
  skip_if_no_browser()
  # Chrome gets a minute to start and to answer (chromote waits 10 seconds
  # by default), and up to three attempts to start.
  withr::local_options(list(chromote.timeout = 60))
  skip_if_not(val_chrome_ready(), "Chrome did not start in three attempts")

  # The same in headless Chrome, where predictions could be erased: the
  # browser rebinds the rebuilt controls and reports what they hold.
  siever_with_app(load_example = TRUE, function(app) {
    idle <- function(ms = 900) siever_wait_render(app, timeout = 180 * 1000)
    # Each step waits for the server's answer that it then asserts, not only
    # for a quiet period. The outputs of a sub-tab are drawn once the browser
    # reports them visible, and on a loaded machine that report can follow the
    # quiet period (the prediction controls can arrive 2.3 s after a 900 ms
    # one, and controls set before they exist are lost). shinytest2 only prints a message for an input it cannot find,
    # so set() fails the test itself when its control never appears.
    wait <- function(script, timeout = 60 * 1000) siever_wait_for(app, script, timeout = timeout)
    bound <- function(input) {
      sprintf("(function(){var e=jQuery('#%s');return e.length === 1 && !!e.data('shinyInputBinding');})()", input)
    }
    rows_are <- function(n) {
      sprintf("document.querySelectorAll('#validity-crit_tbl table tbody tr').length === %d", n)
    }
    # Poll the server (not the page) until it holds an input's value.
    server_holds <- function(input, value, timeout = 60) {
      start <- Sys.time()
      while (!identical(app$get_value(input = input), value)) {
        if (difftime(Sys.time(), start, units = "secs") > timeout) return(invisible(FALSE))
        Sys.sleep(0.1)
      }
      invisible(TRUE)
    }
    text_has <- function(input, text, has = TRUE) {
      sprintf(
        "(function(){var e=document.getElementById('%s');return !!e && (e.textContent.indexOf(%s) >= 0) === %s;})()",
        input, compat_js_string(text), if (has) "true" else "false"
      )
    }
    # The example's identifier is not a criterion (only numeric columns with
    # the role criterion or auxiliary are offered), so a second criterion is
    # made the way the researcher makes one: grade is given the role
    # criterion in the roles grid of the Ingest tab.
    app$set_inputs(
      `ingest-cell_edit` = list(item = "grade", field = "role", value = "Criterion"),
      allow_no_input_binding_ = TRUE, priority_ = "event"
    )
    idle()
    app$click("ingest-confirm_main")
    siever_wait_render(app)
    expect_identical(siever_open_tab(app, "validity"), "validity")
    app$set_inputs(`validity-criteria` = "age_years")
    app$click(selector = "a.nav-link[data-value='Criterion validity']")
    id <- function(what, criterion, construct) paste0("validity-", val_id(what, criterion, construct))
    set <- function(input, value) {
      if (!wait(bound(input))) {
        fail(sprintf(
          "the control %s did not appear (active Validity sub-tab: %s)", input,
          app$get_js("(function(){var a=document.querySelector(\".tab-pane[data-value='validity'] ul.nav a.nav-link.active\");return a ? a.getAttribute('data-value') : 'none';})()")
        ))
        return(invisible(FALSE))
      }
      do.call(app$set_inputs, stats::setNames(list(value), input))
      idle()
    }
    stated <- function() {
      list(
        textual = app$get_value(input = id("kind", "age_years", "Textual")),
        smallest = app$get_value(input = id("min", "age_years", "Textual")),
        visual = app$get_value(input = id("kind", "age_years", "Visual")),
        bound = app$get_value(input = id("bound", "age_years", "Visual"))
      )
    }
    want <- list(textual = "negative", smallest = 0.1, visual = "discriminant", bound = 0.15)

    set(id("kind", "age_years", "Textual"), "negative")
    set(id("min", "age_years", "Textual"), 0.1)
    set(id("kind", "age_years", "Visual"), "discriminant")
    set(id("bound", "age_years", "Visual"), 0.15)
    expect_equal(stated(), want)
    expect_match(app$get_text("#validity-crit_tally"), "Pairs with no prediction: 1", fixed = TRUE)

    # A second criterion: three new pairs without a prediction, and the two
    # stated ones as they were.
    app$set_inputs(`validity-criteria` = c("age_years", "grade"))
    wait(paste(bound(id("kind", "grade", "Textual")), rows_are(6L), sep = " && "))
    # The table is drawn twice here: binding the new controls reports their
    # starting values, which writes the new pairs to the store, and the
    # server draws the table again (DT empties it and refills it by its own
    # request, which the busy state does not track). So the server is waited
    # on until it holds the new controls' values (it answers after that
    # second answer is sent), the browser until it is quiet, and the table
    # until it is refilled; on a timeout the expectations report the state.
    server_holds(id("kind", "grade", "Textual"), "unspecified")
    idle()
    wait(rows_are(6L))
    expect_equal(stated(), want)
    expect_identical(app$get_value(input = id("kind", "grade", "Textual")), "unspecified")
    tally <- app$get_text("#validity-crit_tally")
    expect_match(tally, "Directional predictions (1)", fixed = TRUE)
    expect_match(tally, "entirely at or beyond it for 1", fixed = TRUE)
    expect_match(tally, "Discriminant predictions (1)", fixed = TRUE)
    expect_match(tally, "Pairs with no prediction: 4", fixed = TRUE)
    expect_identical(siever_n_table_rows(app, "validity-crit_tbl"), 6L)
    # The magnitude that belongs to the stated kind is the one on screen.
    visible <- function(input) {
      isTRUE(app$get_js(sprintf(
        "(function(){var e=document.getElementById('%s');return !!e && e.getClientRects().length > 0;})()", input
      )))
    }
    expect_true(visible(id("min", "age_years", "Textual")))
    expect_true(visible(id("bound", "age_years", "Visual")))
    expect_false(visible(id("bound", "age_years", "Textual")))

    # Bartlett factor scores in place of mean scores: they come from the
    # measurement model, which is fit from its button.
    app$set_inputs(`validity-score_method` = "bartlett")
    wait(siever_js_present("validity-scores_fit_go"))
    # The scores are unavailable, with what they are read from, and nothing
    # is substituted; the note's own button has its own id.
    note <- app$get_text("#validity-crit_note")
    expect_match(note, "The construct scores are unavailable.", fixed = TRUE)
    expect_match(note, "which is not fit", fixed = TRUE)
    expect_identical(as.integer(app$get_js("document.querySelectorAll('#validity-fit_go').length")), 1L)
    expect_identical(as.integer(app$get_js("document.querySelectorAll('#validity-scores_fit_go').length")), 1L)
    app$click("validity-fit_go")
    wait(paste(text_has("validity-crit_note", "Bartlett factor score"), rows_are(6L), sep = " && "),
      timeout = 180 * 1000
    )
    expect_match(app$get_text("#validity-crit_note"), "Bartlett factor score", fixed = TRUE)
    expect_equal(stated(), want)
    tally <- app$get_text("#validity-crit_tally")
    expect_match(tally, "Directional predictions (1)", fixed = TRUE)
    expect_match(tally, "Discriminant predictions (1)", fixed = TRUE)
    expect_match(tally, "Pairs with no prediction: 4", fixed = TRUE)
    expect_identical(siever_n_table_rows(app, "validity-crit_tbl"), 6L)
    app$set_inputs(`validity-score_method` = "mean")
    wait(text_has("validity-crit_note", "Bartlett factor score", has = FALSE))

    # The criterion is taken out: the page says the predictions are kept.
    app$set_inputs(`validity-criteria` = "grade")
    wait(text_has("validity-crit_kept", "Textual with age_years"))
    expect_match(app$get_text("#validity-crit_tally"), "Pairs with no prediction: 3", fixed = TRUE)
    kept <- app$get_text("#validity-crit_kept")
    expect_match(kept, "Textual with age_years (negative, |r| at least 0.1)", fixed = TRUE)
    expect_match(kept, "Visual with age_years (small, |r| below 0.15)", fixed = TRUE)
    # And put back: they are there, and they are applied.
    app$set_inputs(`validity-criteria` = c("grade", "age_years"))
    wait(paste(bound(id("kind", "age_years", "Textual")), rows_are(6L), sep = " && "))
    expect_equal(stated(), want)
    tally <- app$get_text("#validity-crit_tally")
    expect_match(tally, "Directional predictions (1)", fixed = TRUE)
    expect_match(tally, "Discriminant predictions (1)", fixed = TRUE)
    expect_identical(siever_n_output_errors(app), 0L)

    # The reproducible code: the last criterion step lists both predictions.
    expect_identical(siever_open_tab(app, "repro"), "repro")
    # The Report sub-tab leads the page, so the Session script sub-tab is
    # opened first.
    compat_open_subtab(app, "repro", "Session script", timeout = 60 * 1000)
    # The last criterion step of the script, as the expectations read it
    # (an earlier step has the same counts, so the whole text would not do).
    wait(paste0(
      "(function(){var e=document.getElementById('repro-code');if(!e)return false;",
      "var t=e.textContent,i=t.lastIndexOf('Validity: construct to criterion correlations');",
      "if(i<0)return false;var j=t.indexOf('\\n',i);",
      "return t.slice(i,j<0?t.length:j).indexOf('(2 stated, 4 pairs without one)')>=0;})()"
    ))
    lines <- strsplit(app$get_text("#repro-code"), "\n", fixed = TRUE)[[1]]
    last <- max(grep("Validity: construct to criterion correlations", lines, fixed = TRUE))
    step <- lines[last:length(lines)]
    step <- step[seq_len(min(which(step == ""), length(step) + 1L) - 1L)]
    expect_match(step[1], "(2 stated, 4 pairs without one)", fixed = TRUE)
    expect_true("#   Textual with age_years: negative, |r| at least 0.1" %in% step)
    expect_true("#   Visual with age_years: small, |r| below 0.15" %in% step)
    expect_false("#   none" %in% step)
  })
})

# ---- Exact magnitudes, the criteria offered, and the HTMT bootstrap's data --

test_that("the stated magnitudes are written to the script as exact literals and as data", {
  b <- 0.1 + 0.2 # 0.30000000000000004: fifteen decimal digits read back as 0.3
  expect_false(identical(as.numeric(format(b, digits = 15)), b))
  expect_identical(as.numeric(sieve_magnitude_text(b)), b)
  expect_identical(sieve_magnitude_text(0.1234), "0.1234")
  stated <- data.frame(
    construct = c("A", "B"), criterion = c("y", "y"),
    kind = c("discriminant", "directional"), sign = c(NA_real_, -1),
    min_abs = c(NA_real_, b), bound = c(b, NA_real_), stringsAsFactors = FALSE
  )
  for (method in c("mean score", "Bartlett factor score")) {
    code <- sieve_validity_code("y", method, stated)
    expect_silent(parse(text = code))
    # The comment names the number the researcher entered, in full.
    expect_true(paste0("#   A with y: small, |r| below ", sieve_magnitude_text(b)) %in% code)
    expect_true(paste0("#   B with y: negative, |r| at least ", sieve_magnitude_text(b)) %in% code)
    # The data frame in the script holds the doubles themselves.
    at <- which(code == "predictions <- data.frame(")
    expect_length(at, 1L)
    end <- at + which(code[-seq_len(at)] == ")")[1]
    env <- new.env()
    eval(parse(text = code[at:end]), envir = env)
    p <- env$predictions
    expect_identical(p$construct, c("A", "B"))
    expect_identical(p$criterion, c("y", "y"))
    expect_identical(p$kind, c("discriminant", "directional"))
    expect_identical(p$sign, c(NA_real_, -1))
    expect_identical(p$min_abs, c(NA_real_, b))
    expect_identical(p$bound, c(b, NA_real_))
  }
  # With nothing stated there is no data frame, and the script says none.
  none <- sieve_validity_code("y", "mean score", stated[0, ])
  expect_true("#   none" %in% none)
  expect_false(any(grepl("^predictions <-", none)))
  expect_silent(parse(text = none))
})

test_that("the criteria offered are the numeric criterion and auxiliary variables, never an identifier or a group", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  fx <- sieve_hs9_fixture()
  st <- sieve_new_state()
  st$raw <- fx$data
  st$items <- fx$data[, fx$item_cols, drop = FALSE]
  st$id_vars <- fx$id_vars
  st$roles <- sieve_declared_roles(fx$data, fx$item_cols)
  st$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  st$confirmed <- TRUE
  st$palette <- "nd"
  st$thresholds <- sieve_default_thresholds()
  roles <- shiny::isolate(sieve_roles_table(st$raw, st$items, st$roles))
  expect_identical(roles$role[roles$variable == "id"], "identifier")
  expect_identical(roles$role[roles$variable == "grade"], "group")
  expect_identical(roles$role[roles$variable == "age_years"], "auxiliary")
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(score_method = "mean", htmt_on = FALSE, marker = "")
    # The identifier (301 distinct numbers) and the groups are not criteria.
    expect_identical(aux_numeric(), "age_years")
    # A role changed on the Ingest tab reaches the selector at once.
    out <- sieve_apply_roles(state, c(grade = "criterion"))
    expect_null(out$refused)
    session$flushReact()
    expect_setequal(aux_numeric(), c("grade", "age_years"))
    expect_true("grade" %in% sieve_role_vars(state, "criterion"))
    # An identifier is never offered, whatever its values look like.
    out <- sieve_apply_roles(state, c(age_years = "identifier"))
    expect_null(out$refused)
    session$flushReact()
    expect_identical(aux_numeric(), "grade")
    # A criterion that is not stored as a number cannot be correlated and is
    # not offered either.
    out <- sieve_apply_roles(state, c(sex = "criterion"))
    expect_null(out$refused)
    session$flushReact()
    expect_identical(aux_numeric(), "grade")
  })
})

test_that("the HTMT bootstrap is shown only for the data it was computed from", {
  skip_if_not_installed("DMAR")
  d <- val_data()
  st <- val_state(d)
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(
      score_method = "mean", htmt_on = TRUE, marker = "",
      htmt_B = 200L, htmt_thresh = 0.85, htmt_seed = 7
    )
    session$setInputs(htmt_boot = 1)
    r <- htmt_ci()
    expect_null(r$error)
    expect_identical(r$revision, sieve_revision(state))
    expect_null(htmt_ci_now()$stale)
    expect_match(val_text(output$htmt_ci_note), "B = 200", fixed = TRUE)
    expect_no_error(output$htmt_ci_tbl)
    # The recorded code reads the bounds against no value.
    expect_false(any(grepl("read against", shiny::isolate(state$codelog), fixed = TRUE)))
    expect_true(any(grepl("no cutoff is applied to the bounds", shiny::isolate(state$codelog), fixed = TRUE)))

    # Another dataset is loaded the way the loader loads one: the kept
    # bootstrap is not shown, and the reason is given.
    d2 <- d[1:300, ]
    sieve_new_dataset(state)
    state$raw <- d2
    state$items <- d2[, 1:6]
    state$meta <- sieve_build_meta(
      d2, names(d2)[1:6],
      list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3"))
    )
    session$flushReact()
    expect_identical(htmt_ci(), r) # the kept result itself is untouched
    expect_match(htmt_ci_now()$stale, "a different dataset has been loaded since", fixed = TRUE)
    note <- val_text(output$htmt_ci_note)
    expect_match(note, "is not shown: a different dataset has been loaded since", fixed = TRUE)
    expect_match(note, "Press Compute HTMT CIs", fixed = TRUE)
    hidden <- tryCatch(output$htmt_ci_tbl, error = function(e) e)
    expect_s3_class(hidden, "shiny.silent.error")

    # Computed again for the newly confirmed data, it is shown again.
    sieve_confirm_types(state)
    session$setInputs(htmt_boot = 2)
    expect_null(htmt_ci_now()$stale)
    expect_identical(htmt_ci()$revision$dataset_id, "D2")
    expect_no_error(output$htmt_ci_tbl)

    # An item excluded changes the ledger: a bootstrap for the earlier pool
    # is not shown either.
    m <- shiny::isolate(state$meta)
    m$status[1] <- "parked"
    state$meta <- m
    session$flushReact()
    expect_match(htmt_ci_now()$stale, "the item ledger has changed since", fixed = TRUE)
    expect_match(val_text(output$htmt_ci_note), "is not shown: the item ledger has changed since", fixed = TRUE)
  })
})

test_that("the stated predictions are listed with a control to remove each, and the tally names the pairs", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  d <- val_data()
  st <- val_state(d)
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(
      score_method = "mean", criteria = c("crit_neg", "crit_mid"),
      htmt_on = FALSE, marker = ""
    )
    expect_match(val_text(output$stated_list), "No prediction has been stated yet", fixed = TRUE)
    val_set(session, val_id("kind", "crit_neg", "A"), "positive")
    val_set(session, val_id("min", "crit_neg", "A"), 0.1)
    val_set(session, val_id("kind", "crit_mid", "B"), "discriminant")
    val_set(session, val_id("bound", "crit_mid", "B"), 0.2)
    # The rendered list is HTML over several lines, with the quotes of the
    # onclick attribute written as entities.
    squash <- function(x) gsub("\\s+", " ", val_text(x))
    listed <- squash(output$stated_list)
    expect_match(listed, "Stated predictions (2).", fixed = TRUE)
    expect_match(listed, "A with crit_neg: positive, |r| at least 0.1.", fixed = TRUE)
    expect_match(listed, "B with crit_mid: small, |r| below 0.2.", fixed = TRUE)
    expect_identical(lengths(regmatches(listed, gregexpr(">Remove</button>", listed, fixed = TRUE))), 2L)
    # The input is namespaced by the module (session$ns), whatever the id
    # the module was given.
    expect_match(listed,
      sprintf(
        "Shiny.setInputValue(&#39;%s&#39;, {construct: &#39;A&#39;, criterion: &#39;crit_neg&#39;}",
        session$ns("remove_prediction")
      ),
      fixed = TRUE
    )
    expect_false(grepl("Not in the table now", listed, fixed = TRUE))

    # The tally names each pair behind its count, with the prediction.
    tally <- val_text(output$crit_tally)
    expect_match(tally, "Directional predictions (1)", fixed = TRUE)
    expect_match(tally, "On the opposite side: A with crit_neg (positive, |r| at least 0.1).", fixed = TRUE)
    expect_match(tally, "Discriminant predictions (1)", fixed = TRUE)
    expect_match(tally, ": B with crit_mid (small, |r| below 0.2).", fixed = TRUE)
    # The pairs are named in the order of the table (criterion by criterion).
    expect_match(tally, "Pairs with no prediction: 2 (B with crit_neg, A with crit_mid).", fixed = TRUE)

    # A pair whose criterion is not selected is listed as not in the table.
    session$setInputs(criteria = "crit_mid")
    listed <- squash(output$stated_list)
    expect_match(listed,
      "A with crit_neg: positive, |r| at least 0.1. Not in the table now (select its criterion in the sidebar to edit it).",
      fixed = TRUE
    )
    session$setInputs(criteria = c("crit_neg", "crit_mid"))

    # Remove: the pair leaves the store; the page's control is told to show
    # no prediction (here, with no browser, it is set by hand).
    session$setInputs(remove_prediction = list(construct = "A", criterion = "crit_neg"))
    expect_identical(sieve_get_prediction(shiny::isolate(state$predictions), "A", "crit_neg")$choice, "unspecified")
    expect_identical(nrow(shiny::isolate(state$predictions)), 1L)
    val_set(session, val_id("kind", "crit_neg", "A"), "unspecified")
    ex <- expectations()
    expect_identical(ex$kind[ex$construct == "A" & ex$criterion == "crit_neg"], "unspecified")
    listed <- squash(output$stated_list)
    expect_match(listed, "Stated predictions (1).", fixed = TRUE)
    expect_false(grepl("A with crit_neg", listed, fixed = TRUE))
    expect_match(val_text(output$crit_tally),
      "Pairs with no prediction: 3 (A with crit_neg, B with crit_neg, A with crit_mid).",
      fixed = TRUE
    )
  })
  # The tally text without the evidence is the counts alone, and the text
  # with the evidence begins with it.
  ev <- sieve_validity_evidence(
    data.frame(construct = "A", criterion = "y", r = 0.4, lower = 0.3, upper = 0.5, stringsAsFactors = FALSE),
    data.frame(
      construct = "A", criterion = "y", kind = "directional", sign = 1, min_abs = NA_real_,
      bound = NA_real_, stringsAsFactors = FALSE
    )
  )
  plain <- sieve_validity_tally_text(sieve_validity_tally(ev))
  named <- sieve_validity_tally_text(sieve_validity_tally(ev), evidence = ev)
  expect_false(grepl("A with y", plain, fixed = TRUE))
  expect_match(named, "On the predicted side: A with y (positive).", fixed = TRUE)
  expect_true(startsWith(named, plain))
  # Removing from an empty store is a no-op that returns the empty store.
  expect_identical(nrow(sieve_drop_prediction(NULL, "A", "y")), 0L)
})

# With a factor score asked for and the measurement model not fit, the
# criterion note must not repeat the page's own fit button under its id
# (validity-fit_go): two elements with one input id leave one of them
# unbound in the browser. The note's button has its own id and runs the
# same fit.
test_that("the criterion note's fit button has its own input id and runs the page's measurement model", {
  st <- val_state()
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(estimator = "study", criteria = "crit_neg", score_method = "mean")
    session$setInputs(score_method = "bartlett")
    html <- paste(as.character(unlist(output$crit_note)), collapse = " ")
    expect_match(html, "The construct scores are unavailable.", fixed = TRUE)
    expect_match(html, '-scores_fit_go"', fixed = TRUE)
    expect_false(grepl('-fit_go"', html, fixed = TRUE))
    expect_identical(demand$status()$state, "not_run")
    session$setInputs(scores_fit_go = 1L)
    expect_identical(demand$status()$state, "current")
    expect_false(is.null(demand$status()$value$fit))
  })
})

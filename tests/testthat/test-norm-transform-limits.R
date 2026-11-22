# The calibration transformation bootstrap reports each interval beside its
# point estimate, requires the resamples a percentile interval at its level
# needs, and validates the level before using it: the table gives an
# estimate beside the limits for the mean, the SD and T, refuses B = 99 at
# level = .99 (below SIEVER's floor of 2 / (1 - .99) - 1 = 199 resamples
# for a percentile interval), and refuses level = NA with its own message
# rather than R's "missing value where TRUE/FALSE needed".
#
# Author: Ken Kelley

skip_if_not_installed("lavaan")

nt_reference <- function() {
  items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- data.frame(
    item = names(items), label = names(items),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = "continuous", status = "active",
    reverse = FALSE, stringsAsFactors = FALSE
  )
  sieve_norm_freeze(items, meta, name = "Calibration", population = "Demonstration only")
}

test_that("each interval stands beside its estimate from the calibration sample", {
  ref <- nt_reference()
  r <- sieve_norm_transform_limits(ref, B = 99L, seed = 1)
  expect_true("estimate" %in% names(r))
  expect_identical(match("estimate", names(r)), match("lower", names(r)) - 1L)
  for (cn in names(ref$scores)) {
    x <- ref$scores[[cn]]
    m <- mean(x)
    s <- stats::sd(x)
    p <- as.numeric(stats::quantile(x, c(.05, .5, .95), type = 7))
    rows <- r[r$construct == cn, ]
    expect_identical(rows$estimate, c(m, s, 50 + 10 * (p - m) / s))
    expect_true(all(rows$lower <= rows$estimate & rows$estimate <= rows$upper))
  }
  expect_match(r$method[1], "an interval is formed when at least 90 percent of the resamples, and at least 39, give a finite value",
    fixed = TRUE)
})

test_that("the resamples required follow the level, and the level is checked before it is used", {
  ref <- nt_reference()
  expect_error(sieve_norm_transform_limits(ref, B = 99L, seed = 1, conf_level = .99),
    "`B` must be one whole number of at least 199: at least 99 resamples, and for a percentile interval at 99% at least 2 / (1 - 0.99) - 1 = 199",
    fixed = TRUE)
  expect_error(sieve_norm_transform_limits(ref, B = 98L, seed = 1), "at least 99", fixed = TRUE)
  expect_error(sieve_norm_transform_limits(ref, B = 120.5, seed = 1), "120.5 was given", fixed = TRUE)
  expect_error(sieve_norm_transform_limits(ref, B = "199", seed = 1), "\"199\" was given", fixed = TRUE)
  for (bad in list(NA_real_, NA, "0.95", c(.9, .95), 95, 0, 1, Inf)) {
    expect_error(sieve_norm_transform_limits(ref, B = 199L, seed = 1, conf_level = bad),
      sprintf("`conf_level` must be one number strictly between 0 and 1; received %s.", paste(deparse(bad, width.cutoff = 200L, nlines = 2L), collapse = " ")),
      fixed = TRUE)
  }
  ok <- sieve_norm_transform_limits(ref, B = 199L, seed = 1, conf_level = .99)
  expect_true(all(ok$conf_level == .99))
  expect_false(anyNA(ok$lower))
  expect_identical(SIEVER:::sieve_norm_min_resamples(.95), 99L)
  expect_identical(SIEVER:::sieve_norm_min_resamples(.99), 199L)
  expect_identical(SIEVER:::sieve_norm_min_resamples(.999), 1999L)
})

test_that("the floor of resamples is given its actual basis, not a condition type 7 limits never need", {
  # The help and the refusal do not say 2 / (1 - C) - 1 is the fewest for
  # which a percentile limit lies inside the resamples. The
  # limits are type 7 quantiles, at position (B - 1)(1 - C) / 2 + 1, which
  # lie inside the resamples at any B: at B = 39 and C = .95 the lower
  # limit sits between the two smallest. The floor's basis is that the
  # smallest of B resamples cuts off 1 / (B + 1) of the distribution on
  # average, and it is where boot's (B + 1) position reaches the first one.
  # SIEVER's own type 7 limits lie strictly inside the draws even below the
  # floor, so the floor is not about reaching past the extremes.
  lim <- sieve_percentile_limits(as.numeric(1:39), 39L, .95)
  expect_true(lim$lower > 1 && lim$lower < 2)
  lim20 <- sieve_percentile_limits(as.numeric(1:20), 20L, .95)
  expect_true(lim20$lower > 1 && lim20$upper < 20)
  ref <- nt_reference()
  message <- tryCatch(sieve_norm_transform_limits(ref, B = 99L, seed = 1, conf_level = .99),
    error = function(e) conditionMessage(e))
  expect_match(message, paste(
    "= 199, the floor SIEVER sets for every percentile interval (with fewer, even the most extreme",
    "resample cuts off on average more than the tail of 0.005 a limit stands for); 99L was given."
  ), fixed = TRUE)
  expect_false(grepl("inside the resamples", message, fixed = TRUE))
  root <- compat_source_root()
  rd <- if (!is.null(root)) {
    tools::parse_Rd(file.path(root, "man", "sieve_norm_transform_limits.Rd"))
  } else {
    tools::Rd_db("SIEVER")[["sieve_norm_transform_limits.Rd"]]
  }
  # Code is quoted in the rendered text, with plain or typographic quotes
  # by the session's setting, so the quotes are dropped.
  rd <- gsub("\\s+", " ", paste(utils::capture.output(tools::Rd2txt(rd,
    options = list(underline_titles = FALSE))), collapse = " "))
  rd <- gsub("[\u2018\u2019'`]", "", rd)
  expect_false(grepl("lies inside the resamples", rd, fixed = TRUE))
  expect_match(rd, "the smallest of B resamples cuts off, on average, a share 1 / (B + 1)", fixed = TRUE)
  expect_match(rd, "The type 7 limits used here lie within the resamples at any count", fixed = TRUE)
})

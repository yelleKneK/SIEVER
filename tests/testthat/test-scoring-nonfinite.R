# Infinite values are refused with the item and the rows named, NaN is a
# missing value, and "eligible" means a score was formed. The shared numeric
# reader (sieve_numeric_columns()) keeps Inf from lavaan and DMAR, which
# would otherwise fail a CFA with lavaan's "no finite variance", fail robust
# omega and alpha with DMAR's internal "missing value where TRUE/FALSE
# needed", and let sieve_composite_score() return a row with score NA and
# eligible TRUE. Finite responses whose sum overflows (1e308 + 1e308) are
# not scored, with the reason, so the exported sensitivity table never
# reports NA means for every policy.
#
# Author: Ken Kelley

skip_if_not_installed("lavaan")

nf_meta <- function(items) {
  data.frame(
    item = names(items), label = names(items),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = "continuous", status = "active",
    reverse = FALSE, stringsAsFactors = FALSE
  )
}

test_that("the shared reader names each infinite value and its row, and reads NaN as missing", {
  d <- data.frame(a = c(1, Inf, 3, 4, -Inf), b = c(2, 3, NaN, 5, 6), c = c("1", "Inf", "1e999", "2", NA))
  read <- SIEVER:::sieve_numeric_columns(d, c("a", "b", "c"))
  expect_identical(read$problems$column, c("a", "c"))
  expect_identical(read$problems$kind, c("not_finite", "not_finite"))
  expect_identical(read$problems$n, c(2L, 2L))
  expect_identical(read$problems$example, c("Inf in row 2; -Inf in row 5", "Inf in row 2; Inf in row 3"))
  expect_match(read$text[1],
    "Item 'a' holds 2 infinite values (Inf in row 2; -Inf in row 5), so the model cannot be fit: a response is a finite number or missing (NA).",
    fixed = TRUE)
  expect_match(read$text[1], "or exclude the item on the Items tab.", fixed = TRUE)
  # NaN is a missing value, returned as NA, and not a problem.
  expect_identical(read$data$b, c(2, 3, NA, 5, 6))
  expect_false("b" %in% read$problems$column)
  # More than three rows are counted.
  many <- SIEVER:::sieve_numeric_columns(data.frame(x = c(Inf, 1, Inf, Inf, 2, Inf)), "x")
  expect_identical(many$problems$example, "Inf in row 1; Inf in row 3; Inf in row 4; and 1 more")
  # The model route refuses with SIEVER's sentence, not lavaan's.
  items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  items$x1[3] <- Inf
  a <- sieve_analyze("cfa", items_df = items, meta = nf_meta(items))
  expect_match(a$status$message, "Item 'x1' holds 1 infinite value (Inf in row 3), so the model cannot be fit", fixed = TRUE)
  expect_false(grepl("finite variance", a$status$message, fixed = TRUE))
  # NaN in the same place is a missing value: the model is fit by FIML.
  items$x1[3] <- NaN
  b <- sieve_analyze("cfa", items_df = items, meta = nf_meta(items))
  expect_identical(b$status$code, "ok")
})

test_that("reliability, item-rest and response screens refuse an infinite value with the same sentence", {
  items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:3)]
  items$x1[3] <- Inf
  sentence <- "Item 'x1' holds 1 infinite value (Inf in row 3), so the model cannot be fit"
  expect_error(SIEVER:::sieve_reliability_data(items, names(items)), sentence, fixed = TRUE)
  expect_error(SIEVER:::sieve_item_rest(items, names(items)), sentence, fixed = TRUE)
  expect_error(sieve_response_screens(items), sentence, fixed = TRUE)
  # NaN reaches neither as anything but missing.
  items$x1[3] <- NaN
  X <- SIEVER:::sieve_reliability_data(items, names(items))
  expect_true(is.na(X$x1[3]) && !is.nan(X$x1[3]))
})

test_that("a composite refuses infinite responses with the item and rows, reads NaN as missing, and is eligible only when a score was formed", {
  d <- data.frame(a = c(1, Inf, 3), b = c(2, 3, -Inf))
  expect_error(sieve_composite_score(d, c("a", "b")),
    "Item 'a' holds 1 infinite value (Inf in row 2), so its score cannot be formed", fixed = TRUE)
  expect_error(sieve_composite_score(d, c("a", "b")),
    "Item 'b' holds 1 infinite value (-Inf in row 3), so its score cannot be formed", fixed = TRUE)
  # Text that reads as an infinite number is refused too.
  expect_error(sieve_composite_score(data.frame(a = c("1", "1e999"), b = c(2, 3)), c("a", "b")),
    "Item 'a' holds 1 infinite value (Inf in row 2)", fixed = TRUE)
  # NaN is a missing response: one answered item, scored on it.
  nan <- sieve_composite_score(data.frame(a = c(1, NaN), b = c(3, 4)), c("a", "b"))
  expect_identical(nan$answered, c(2, 1))
  expect_equal(nan$scores, c(2, 4))
  expect_identical(nan$eligible, c(TRUE, TRUE))
  # An all-missing row is not scored, and says why.
  none <- sieve_composite_score(data.frame(a = c(1, NA), b = c(3, NA)), c("a", "b"))
  expect_identical(none$eligible, c(TRUE, FALSE))
  expect_identical(none$reason, c(NA, "answered 0 of 2 items; 1 required"))
  # Finite responses whose weighted sum overflows: not scored, with the reason.
  big <- sieve_composite_score(data.frame(a = c(1e308, 1), b = c(1e308, 2)), c("a", "b"), method = "sum")
  expect_identical(big$eligible, c(FALSE, TRUE))
  expect_identical(big$reason, c("the weighted sum of the responses exceeds double precision", NA))
  expect_equal(big$scores, c(NA, 3))
  # Weights whose sum overflows are refused.
  expect_error(sieve_composite_score(data.frame(a = 1:3, b = 1:3), c("a", "b"), weights = c(a = 1e308, b = 1e308)),
    "`weights` must have a finite sum", fixed = TRUE)
  # Answered items that carry no weight.
  zero <- sieve_composite_score(data.frame(a = c(1, 2), b = c(NA, 3)), c("a", "b"), weights = c(a = 0, b = 1))
  expect_identical(zero$eligible, c(FALSE, TRUE))
  expect_identical(zero$reason, c("every item answered has weight 0", NA))
})

test_that("eligible is is.finite(scores) for every accepted input, and the counts that read it agree", {
  battery <- list(
    list(d = data.frame(a = c(1, NA, 3, NaN), b = c(2, NA, NA, 4), c = c(NA, NA, 5, 6)), method = "mean", k = 1L),
    list(d = data.frame(a = c(1, NA, 3, NaN), b = c(2, NA, NA, 4), c = c(NA, NA, 5, 6)), method = "sum", k = 2L),
    list(d = data.frame(a = c(1e308, 1e308, 1), b = c(1e308, NA, 2)), method = "sum", k = 1L),
    list(d = data.frame(a = c(1e308, -1e308, 1), b = c(1e308, 1e308, 2)), method = "mean", k = 2L),
    list(d = data.frame(a = c(NA, NA), b = c(NA, NA)), method = "mean", k = 1L)
  )
  for (case in battery) {
    r <- sieve_composite_score(case$d, names(case$d), method = case$method, min_count = case$k)
    expect_identical(r$eligible, is.finite(r$scores))
    expect_identical(is.na(r$reason), r$eligible)
  }
  # The sensitivity table counts the scored rows and summarizes their scores.
  d <- data.frame(a = c(1e308, 1, 2, 3, NA), b = c(1e308, 2, NA, 4, NA))
  sens <- sieve_score_policy_sensitivity(d, c("a", "b"), counts = 1L)
  expect_identical(sens$table$scored, 3L)
  expect_identical(sens$table$unscored, 2L)
  expect_equal(sens$table$mean, mean(c(1.5, 2, 3.5)))
  expect_false(anyNA(sens$table$sd))
})

test_that("applying a frozen reference says per row whether a score was formed and why not", {
  items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- nf_meta(items)
  reference <- sieve_norm_freeze(items, meta, name = "Calibration", population = "Demonstration only")
  cohort <- items[1:4, ]
  cohort[2, c("x1", "x2", "x3")] <- NA
  applied <- sieve_norm_apply(reference, cohort, meta)
  visual <- applied[applied$construct == "Visual", ]
  expect_identical(visual$eligible, c(TRUE, FALSE, TRUE, TRUE))
  expect_identical(visual$answered, c(3L, 0L, 3L, 3L))
  expect_identical(visual$reason[2], "answered 0 of 3 items; 1 required")
  expect_true(all(is.na(visual$reason[-2])))
  expect_identical(applied$eligible, is.finite(applied$score))
})

# The Reliability tab: an item loading opposite in sign to the others of
# its construct is named with its interval, with what it does to omega (H
# ignores sign) and the action, and a negative alpha shows no [0, 0]
# interval marked completed.

skip_if_not_installed("lavaan")

rk_run <- function(d, items = c("x4", "x5", "x6"), meta = NULL) {
  spec <- sieve_reliability_spec(items, rep("continuous", length(items)), construct = "Textual",
    run_bootstrap = FALSE)
  sieve_reliability_construct(d, spec, meta = meta)
}

test_that("an item loading opposite in sign is named with its interval, and what it does to omega and H is said", {
  d <- lavaan::HolzingerSwineford1939
  d$x6 <- -d$x6
  r <- rk_run(d)
  # The estimate is correct for the total as scored: its status stays ok.
  expect_identical(r$omega$status$code, "ok")
  k <- r$keying
  expect_identical(k$items$item, "x6")
  expect_true(k$items$named)
  expect_identical(k$majority, 2L)
  se <- sqrt(r$H$vcov["x6", "x6"])
  expect_equal(k$items$lower, r$H$loadings[["x6"]] - stats::qnorm(0.975) * se, tolerance = 1e-12)
  expect_lt(k$items$upper, 0)
  text <- sieve_reliability_paragraph(r)
  expect_match(text, "x6 loads -0\\.832 \\[-0\\.[0-9]{3}, -0\\.[0-9]{3}\\] \\(95% interval\\) on the one-factor model of Textual, opposite in sign to the other 2 items")
  expect_match(text, "in which x6 subtracts from the common part", fixed = TRUE)
  expect_match(text, "is unaffected because its optimal weights take each loading's sign", fixed = TRUE)
  expect_match(text, "set its Reverse? cell to Yes on the Ingest tab", fixed = TRUE)
  # The row calls for attention, and the table names the item.
  expect_true(any(grepl("opposite in sign", sieve_reliability_notes(r), fixed = TRUE)))
  expect_identical(sieve_reliability_table(list(r))$keying_items, "x6")
  # Already marked reverse keyed: a possible second reversal.
  meta <- data.frame(item = c("x4", "x5", "x6"), reverse = c(FALSE, FALSE, TRUE))
  twice <- sieve_reliability_paragraph(rk_run(d, meta = meta))
  expect_match(twice, "x6 is marked reverse keyed and was reverse scored", fixed = TRUE)
  expect_match(twice, "reversed twice", fixed = TRUE)
})

test_that("the item named is the one opposite to most, even when it comes first", {
  d <- lavaan::HolzingerSwineford1939
  d$x4 <- -d$x4
  r <- rk_run(d)
  # lavaan orients the factor by x4, so x5 and x6 load negatively; x4 is named.
  expect_gt(r$H$loadings[["x4"]], 0)
  expect_identical(r$keying$items$item, "x4")
  expect_false(grepl("x5 loads", sieve_reliability_paragraph(r), fixed = TRUE))
})

test_that("correctly keyed items give no keying sentence", {
  r <- rk_run(lavaan::HolzingerSwineford1939)
  expect_identical(nrow(r$keying$items), 0L)
  expect_false(grepl("opposite in sign", sieve_reliability_paragraph(r), fixed = TRUE))
  expect_true(is.na(sieve_reliability_table(list(r))$keying_items))
})

test_that("a negative alpha has no interval and says why, rather than [0, 0] marked completed", {
  d <- lavaan::HolzingerSwineford1939
  d$x6 <- -d$x6
  r <- rk_run(d)
  expect_lt(r$alpha$estimate, 0)
  expect_true(is.na(r$alpha$lower))
  expect_true(is.na(r$alpha$upper))
  expect_identical(r$alpha$uncertainty$status, "unavailable")
  tab <- sieve_reliability_table(list(r))
  expect_true(is.na(tab$alpha_lower))
  expect_identical(tab$alpha_uncertainty, "unavailable")
  text <- sieve_reliability_paragraph(r)
  expect_false(grepl("[0.000, 0.000]", text, fixed = TRUE))
  expect_false(grepl("completed", text, fixed = TRUE))
  expect_match(text, "While estimating alpha the engine warned", fixed = TRUE)
  expect_match(text, "no interval is reported", fixed = TRUE)
})

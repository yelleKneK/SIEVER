# A stratified validation reservation is allocated by largest remainder
# (Hamilton), so its total is the reservation's size without strata and
# each stratum is within one row of its quota. Rounding each stratum on its
# own, round(N_h * proportion), misses the total: the Holzinger and
# Swineford schools at .3 would reserve 91 rows where round(301 * .3) = 90,
# 40 strata of 11 at .25 would reserve 120 rows (realized .273), 8 strata
# of 5 at .5 would reserve 16 (.400, every half rounded to even), and 20
# strata of one row at .3 would reserve none.
#
# Author: Ken Kelley

skip_if_not_installed("lavaan")

ha_cases <- list(
  list(label = "HS school at .3", strata = as.character(lavaan::HolzingerSwineford1939$school), p = .3),
  list(label = "40 strata of 11 at .25", strata = rep(sprintf("s%02d", 1:40), each = 11), p = .25),
  list(label = "8 strata of 7 at .5", strata = rep(letters[1:8], each = 7), p = .5),
  list(label = "8 strata of 5 at .5", strata = rep(letters[1:8], each = 5), p = .5),
  list(label = "10 strata of 3 at .3", strata = rep(letters[1:10], each = 3), p = .3),
  list(label = "20 singletons at .3", strata = sprintf("s%02d", 1:20), p = .3),
  list(label = "20 singletons at .6", strata = sprintf("s%02d", 1:20), p = .6),
  list(label = "uneven strata at .37", strata = rep(c("a", "b", "c", NA), c(17, 4, 9, 2)), p = .37)
)

test_that("the realized total is round(N * proportion) and every stratum is within one row of its quota", {
  for (case in ha_cases) {
    n <- length(case$strata)
    for (seed in c(1, 11, 2026)) {
      drawn <- sieve_holdout_draw(seq_len(n), case$p, seed = seed, strata = case$strata)
      sizes <- drawn$sizes
      expect_identical(length(drawn$rows), as.integer(round(n * case$p)), label = case$label)
      expect_identical(sum(sizes$reserved), as.integer(round(n * case$p)), label = case$label)
      expect_true(all(abs(sizes$reserved - sizes$quota) < 1), label = case$label)
      # The rows drawn match the counts by stratum.
      key <- match(case$strata, unique(case$strata))
      expect_identical(tabulate(key[drawn$positions], nrow(sizes)), sizes$reserved, label = case$label)
      expect_equal(sizes$realized, sizes$reserved / sizes$n)
      expect_identical(drawn$allocation, SIEVER:::sieve_holdout_allocation_rule())
    }
    # The sizes before the draw give the same total and say how many rows
    # the seed places.
    before <- sieve_holdout_sizes(n, case$p, case$strata)
    expect_identical(sum(before$reserved) + attr(before, "by_seed_rows"), as.integer(round(n * case$p)))
    expect_identical(attr(before, "total"), as.integer(round(n * case$p)))
  }
  # The realized proportions match the request: overall, within one row of
  # the quota in every stratum.
  big <- sieve_holdout_draw(seq_len(440), .25, seed = 1, strata = rep(sprintf("s%02d", 1:40), each = 11))
  expect_equal(length(big$rows) / 440, .25)
  hs <- sieve_holdout_sizes(301, .3, lavaan::HolzingerSwineford1939$school)
  expect_identical(hs$reserved, c(47L, 43L))
  expect_equal(hs$quota, c(156, 145) * .3)
  expect_false(any(hs$by_seed))
})

test_that("tied remainders at the cut are placed by the seed, not by the order of the data", {
  strata <- sprintf("s%02d", 1:20)
  sizes <- sieve_holdout_sizes(20, .3, strata)
  expect_true(all(sizes$by_seed))
  expect_identical(attr(sizes, "by_seed_rows"), 6L)
  expect_true(all(sizes$reserved == 0L))
  # Every singleton has an empty half whichever way the seed places it.
  expect_true(all(sizes$empty_half))
  chosen <- lapply(1:30, function(seed) sieve_holdout_draw(seq_len(20), .3, seed = seed, strata = strata)$rows)
  expect_true(all(lengths(chosen) == 6L))
  # Not always the first six: the seed decides.
  expect_gt(length(unique(unlist(chosen))), 6L)
  expect_false(all(vapply(chosen, function(r) identical(r, 1:6), logical(1))))
  # The same seed gives the same rows, and the caller's stream is untouched.
  set.seed(3)
  before <- .Random.seed
  one <- sieve_holdout_draw(seq_len(20), .3, seed = 9, strata = strata)
  expect_identical(.Random.seed, before)
  expect_identical(sieve_holdout_draw(seq_len(20), .3, seed = 9, strata = strata)$rows, one$rows)
  # A tie that does not straddle the cut takes no draw: the two strata whose
  # remainders tie (.75 each) both receive the two rows that remain.
  even <- sieve_holdout_sizes(8, .25, rep(c("a", "b", "c"), c(3, 3, 2)))
  expect_identical(even$reserved, c(1L, 1L, 0L))
  expect_false(any(even$by_seed))
  expect_identical(attr(even, "by_seed_rows"), 0L)
})

test_that("a draw whose allocation equals per-stratum rounding's draws the same rows", {
  # Per-stratum rounding, as the reference.
  rounding_draw <- function(index, p, seed, strata) {
    st <- SIEVER:::sieve_holdout_strata(strata)
    reserved <- as.integer(round(tabulate(st$key, length(st$levels)) * p))
    picks <- withr::with_seed(seed, {
      picks <- integer(0)
      for (i in seq_along(st$levels)) {
        at <- which(st$key == i)
        if (reserved[i] > 0L) picks <- c(picks, at[sample.int(length(at), reserved[i])])
      }
      sort(picks)
    })
    list(rows = index[picks], reserved = reserved)
  }
  school <- lavaan::HolzingerSwineford1939$school
  unchanged <- 0L
  changed <- 0L
  for (p in c(.2, .3, .5)) {
    for (seed in c(1, 11, 113)) {
      new <- sieve_holdout_draw(seq_len(301), p, seed = seed, strata = school)
      old <- rounding_draw(seq_len(301), p, seed, school)
      if (identical(new$sizes$reserved, old$reserved)) {
        unchanged <- unchanged + 1L
        expect_identical(new$rows, old$rows)
      } else {
        changed <- changed + 1L
        expect_false(identical(new$rows, old$rows))
      }
    }
  }
  # At .2 and .5 the allocations agree; at .3 (47 and 44 by per-stratum
  # rounding, total 91) they do not.
  expect_identical(unchanged, 6L)
  expect_identical(changed, 3L)
})

test_that("the Screening plan reserves the exact total and the recorded reservation keeps the rule", {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, name = "Nine tests", configure = function(st) {
    st$roles <- sieve_propose_roles(fx$data)
  })
  plan <- shiny::isolate(SIEVER:::sieve_holdout_plan(s, .3, "school"))
  expect_identical(plan$reserved, 90L)
  expect_identical(plan$calibration, 211L)
  out <- SIEVER:::sieve_reserve_holdout(s, .3, seed = 11, stratify = "school")
  expect_null(out$refused)
  expect_identical(out$record$n_reserved, 90L)
  expect_identical(out$record$sizes$reserved, c(47L, 43L))
  expect_identical(out$record$allocation, SIEVER:::sieve_holdout_allocation_rule())
})

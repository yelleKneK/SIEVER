# Row numbers, holdout strata and seeds are checked as given, before any
# coercion. Each refusal below guards an input that coercion would
# truncate, recode or drop without notice: a factor of identifiers read as
# its level codes, 1.9 run as row 1 or seed 1, a group whose text is
# "(missing)" pooled with the rows that have no group, and an unsorted
# index returning its rows unsorted. Valid integer input gives the draws
# and the random number behavior of a plain per-stratum rounding draw
# exactly, which the reference implementation below checks.
#
# Author: Ken Kelley

skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

iv_text <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))

# A plain per-stratum rounding draw, with string labels for strata and a
# sort of the positions, so valid input can be checked against it.
iv_draw_per_stratum_rounding <- function(index, proportion, seed = NULL, strata = NULL) {
  index <- suppressWarnings(as.integer(index))
  n <- length(index)
  if (is.null(strata)) {
    s <- rep("(all)", n)
  } else {
    s <- if (is.factor(strata)) as.character(strata) else as.character(sieve_plain_values(strata))
    s[is.na(s)] <- "(missing)"
  }
  levels <- unique(s)
  reserved <- as.integer(round(vapply(levels, function(l) sum(s == l), integer(1)) * proportion))
  used <- if (is.null(seed)) sample.int(.Machine$integer.max, 1L) else as.integer(seed)
  picks <- withr::with_seed(used, {
    picks <- integer(0)
    for (i in seq_along(levels)) {
      at <- which(s == levels[i])
      if (reserved[i] > 0L) picks <- c(picks, at[sample.int(length(at), reserved[i])])
    }
    sort(picks)
  })
  list(rows = index[picks], positions = picks, seed = used, reserved = unname(reserved))
}

# ---- Row numbers --------------------------------------------------------------------

test_that("row numbers are refused before coercion, each kind with its own reason and the values at fault", {
  rows <- function(x, n = NULL) sieve_row_positions(x, n, arg = "validation_rows", of = "`data`")
  expect_identical(rows(c(3, 1, 2), 5), c(3L, 1L, 2L))
  expect_identical(rows(NULL, 5), integer(0))
  expect_error(rows(c(TRUE, FALSE, TRUE), 3),
    "a logical vector of length 3 was given. For the rows a logical vector marks, pass which() of it",
    fixed = TRUE, class = "sieve_invalid_rows")
  expect_error(rows(factor(c("10", "20")), 30), "a factor was given. A factor or text identifier is not a row number: to reserve rows by identifier, pass match(ids, id_column)",
    fixed = TRUE, class = "sieve_invalid_rows")
  expect_error(rows(c("7", "8"), 30), "text was given", fixed = TRUE, class = "sieve_invalid_rows")
  expect_error(rows(list(1, 2), 30), "an object of class list was given", fixed = TRUE, class = "sieve_invalid_rows")
  expect_error(rows(c(1, NA, 3, Inf), 5), "holds 2 missing or infinite values (at elements 2, 4: NA, Inf)",
    fixed = TRUE, class = "sieve_invalid_rows")
  # Truncation would have made 1.2 and 1.7 one repeated row; the reason given
  # is the fractional values, not a repeat.
  expect_error(rows(c(1.2, 1.7, 3, 4), 5), "holds 2 values that are not whole numbers (1.2, 1.7)",
    fixed = TRUE, class = "sieve_invalid_rows")
  expect_error(rows(c(0, -1, 2), 5), "holds 2 values below 1 (0, -1)", fixed = TRUE, class = "sieve_invalid_rows")
  expect_error(rows(c(1, 6, 7), 5), "holds 2 values above 5, the number of rows of `data` (6, 7). Row positions are not respondent identifiers",
    fixed = TRUE, class = "sieve_invalid_rows")
  expect_error(rows(c(1, 2^31)), "above 2147483647, the largest row number R represents", fixed = TRUE,
    class = "sieve_invalid_rows")
  expect_error(rows(c(1, 4, 4, 7, 7), 9), "`validation_rows` repeats row numbers 4, 7; each row may appear once.",
    fixed = TRUE, class = "sieve_invalid_rows")
  expect_error(rows(1:2, 10.8), "`n` must be one whole number of rows, 0 or more; 10.8 was given.", fixed = TRUE)
  # Long lists of offending values are shortened.
  expect_error(rows(seq(1.5, 10.5), 20), "(1.5, 2.5, 3.5, 4.5, 5.5, and 5 more)", fixed = TRUE)
})

test_that("the reservation draw and sizes refuse non-row input before the random number stream is touched", {
  set.seed(5)
  before <- .Random.seed
  draw <- function(index, ...) sieve_holdout_draw(index, .5, seed = 1, ...)
  expect_error(draw(c(1.2, 2.2, 3.2, 4.2)), "not whole numbers", class = "sieve_invalid_rows")
  # A factor of identifiers would be read as its level codes (rows 1 and 3
  # for identities 10 and 30).
  expect_error(draw(factor(c("10", "20", "30", "40"))), "a factor was given", class = "sieve_invalid_rows")
  expect_error(draw(c("7", "8", "9", "10")), "text was given", class = "sieve_invalid_rows")
  expect_error(draw(c(0, -1, -2, -3)), "below 1", class = "sieve_invalid_rows")
  expect_error(draw(c(TRUE, FALSE)), "logical vector", class = "sieve_invalid_rows")
  expect_error(draw(c(1, NA, 3)), "missing or infinite", class = "sieve_invalid_rows")
  expect_error(draw(c(1.2, 1.7, 3, 4)), "not whole numbers (1.2, 1.7)", fixed = TRUE, class = "sieve_invalid_rows")
  expect_error(sieve_holdout_sizes(10.8, .5), "`n` must be one whole number of rows, 0 or more; 10.8 was given.",
    fixed = TRUE)
  expect_error(sieve_holdout_sizes("10", .5), "\"10\" was given", fixed = TRUE)
  expect_error(sieve_holdout_sizes(TRUE, .5), "TRUE was given", fixed = TRUE)
  expect_error(sieve_holdout_sizes(-1, .5), "0 or more", fixed = TRUE)
  expect_error(sieve_holdout_sizes(10, "0.3"), "`proportion` must be one number strictly between 0 and 1; \"0.3\" was given.",
    fixed = TRUE)
  expect_error(sieve_holdout_sizes(10, .5, strata = c("a", "b")), "2 values were given for 10 rows", fixed = TRUE)
  expect_error(draw(1:10, strata = letters[1:3]), "3 values were given for 10 rows", fixed = TRUE)
  expect_identical(.Random.seed, before)
})

test_that("valid integer input keeps the draws and the random number behavior of a per-stratum rounding draw wherever the allocation agrees", {
  school <- lavaan::HolzingerSwineford1939$school
  with_na <- school
  with_na[c(3, 40, 200)] <- NA
  offset <- c(5L, 9L, 20:318)
  for (seed in c(1, 11, 113, 2026)) {
    for (p in c(.2, .3, .5)) {
      cases <- list(
        list(index = seq_len(301), strata = NULL),
        list(index = seq_len(301), strata = school),
        list(index = offset, strata = school),
        list(index = as.numeric(offset), strata = school),
        list(index = offset, strata = with_na),
        list(index = seq_len(301), strata = as.integer(school))
      )
      for (case in cases) {
        set.seed(31)
        before <- .Random.seed
        new <- sieve_holdout_draw(case$index, p, seed = seed, strata = case$strata)
        expect_identical(.Random.seed, before)
        old <- iv_draw_per_stratum_rounding(case$index, p, seed = seed, strata = case$strata)
        expect_identical(new$rng$seed, as.integer(seed))
        # Largest remainder allocation changes the counts only where
        # per-stratum rounding misses round(n * p); the rows then differ, and
        # otherwise they are those of the reference draw.
        if (identical(new$sizes$reserved, old$reserved)) {
          expect_identical(new$rows, old$rows)
          expect_identical(new$positions, old$positions)
        } else {
          expect_identical(length(new$rows), as.integer(round(length(case$index) * p)))
          expect_false(identical(sum(old$reserved), as.integer(round(length(case$index) * p))))
        }
      }
    }
  }
  # Unseeded: one draw from the caller's stream chooses the seed, as in the
  # reference draw.
  set.seed(77)
  old <- iv_draw_per_stratum_rounding(offset, .2, strata = school)
  after_old <- .Random.seed
  set.seed(77)
  new <- sieve_holdout_draw(offset, .2, strata = school)
  expect_identical(.Random.seed, after_old)
  expect_identical(new$rows, old$rows)
  expect_identical(new$rng$seed, old$seed)
  expect_false(new$rng$supplied)
  # The sizes too, apart from the column that says which stratum is missing.
  sizes <- sieve_holdout_sizes(301, .3, strata = school)
  expect_identical(sizes$stratum, c("Pasteur", "Grant-White"))
  expect_identical(sizes$missing, c(FALSE, FALSE))
  # 46.8 and 43.5: per-stratum rounding gives 47 and 44 (91 rows); largest
  # remainder gives 47 and 43, round(301 * .3) = 90.
  expect_identical(sizes$reserved, c(47L, 43L))
  expect_identical(sieve_holdout_sizes(301, .3)$stratum, "(all)")
})

# ---- Strata keyed without a label ---------------------------------------------------

test_that("a missing group value is a stratum of its own, apart from a group named \"(missing)\"", {
  # Keyed by label, the one row whose group is the text "(missing)" would be
  # pooled with the five rows with no group: one stratum of 6, reserving 3,
  # the labeled row among the drawn.
  strata <- c("(missing)", rep(NA, 5))
  sizes <- sieve_holdout_sizes(6, .5, strata)
  expect_identical(sizes$stratum, c("(missing)", NA))
  expect_identical(sizes$missing, c(FALSE, TRUE))
  expect_identical(sizes$n, c(1L, 5L))
  # Quotas .5 and 2.5: 0 and 2, and the row left of round(6 * .5) = 3 goes
  # to one of the two by the seed, since their remainders tie.
  expect_identical(sizes$reserved, c(0L, 2L))
  expect_identical(sizes$by_seed, c(TRUE, TRUE))
  expect_identical(attr(sizes, "by_seed_rows"), 1L)
  drawn <- sieve_holdout_draw(1:6, .5, seed = 1, strata = strata)
  expect_length(drawn$rows, 3L)
  expect_identical(drawn$sizes$n, c(1L, 5L))
  expect_identical(as.integer(1L %in% drawn$rows), drawn$sizes$reserved[1])
  expect_identical(sum(drawn$rows %in% 2:6), drawn$sizes$reserved[2])
  # A factor with a "(missing)" level and missing values.
  f <- factor(c("(missing)", NA, NA, "a", "a", NA))
  by <- sieve_holdout_sizes(6, .5, f)
  expect_identical(by$stratum, c("(missing)", NA, "a"))
  expect_identical(by$n, c(1L, 3L, 2L))
  expect_identical(by$missing, c(FALSE, TRUE, FALSE))
  # addNA() makes NA a level; it is still the missing stratum.
  expect_identical(sieve_holdout_sizes(6, .5, addNA(f))$n, c(1L, 3L, 2L))
  # Every value missing: one stratum, the missing one.
  all_missing <- sieve_holdout_sizes(6, .5, rep(NA, 6))
  expect_identical(all_missing$stratum, NA_character_)
  expect_true(all_missing$missing)
  expect_identical(all_missing$reserved, 3L)
  # The text "NA" is a value, not a missing one.
  expect_identical(sieve_holdout_sizes(4, .5, c("NA", NA, "NA", NA))$n, c(2L, 2L))
  keyed <- sieve_holdout_strata(c("b", NA, "a", "b", NA))
  expect_identical(keyed$levels, c("b", NA, "a"))
  expect_identical(keyed$key, c(1L, 2L, 3L, 1L, 2L))
})

test_that("the Screening page names the missing stratum in words and refuses an invalid seed with the reason", {
  fx <- sieve_hs9_fixture()
  fx$data$school[1:5] <- NA
  s <- siever_test_study(fx, name = "Nine tests", configure = function(st) {
    st$roles <- sieve_propose_roles(fx$data)
  })
  shiny::testServer(mod_screen_server, args = list(state = s), {
    session$setInputs(holdout_proportion = .3, holdout_seed = NA, holdout_stratify = "school")
    # Quotas 1.5, 45.3 and 43.5 for 90 rows: 1, 45 and 43, and the row left
    # goes by the seed to the missing level or Grant-White, whose remainders
    # tie at .5.
    expect_match(iv_text(output$holdout_sizes),
      paste(
        "By level of school (largest remainder allocation, so the total is exact): missing value 1 or 2 of 5;",
        "Pasteur 45 of 151; Grant-White 43 or 44 of 145. The levels with two counts tie in their remainders,",
        "and the seed places 1 more row among them."
      ), fixed = TRUE)
    # 12.7 is not run as seed 12; it is refused, with the reason on the
    # page, and nothing is reserved.
    session$setInputs(holdout_seed = 12.7, holdout_reserve = 1)
    result <- iv_text(output$holdout_result)
    expect_match(result, "The random seed must be a whole number from -2147483647 to 2147483647; 12.7 was entered.",
      fixed = TRUE)
    expect_match(result, "Nothing was reserved.", fixed = TRUE)
    expect_null(sieve_holdout(state))
    expect_identical(nrow(state$items), 301L)
    session$setInputs(holdout_seed = 11, holdout_reserve = 2)
    expect_identical(sieve_holdout(state)$rng$seed, 11L)
    expect_identical(sieve_holdout(state)$sizes$missing, c(TRUE, FALSE, FALSE))
  })
  out <- sieve_reserve_holdout(s, .3, seed = 1.9)
  expect_match(out$refused, "reservation R1 is already in force", fixed = TRUE)
  sieve_release_holdout(s)
  out <- sieve_reserve_holdout(s, .3, seed = 1.9)
  expect_identical(out$refused, paste(
    "`seed` must be NULL or one whole number from -2147483647 to 2147483647; 1.9 was given.",
    "Nothing was reserved."
  ))
})

# ---- The order of the reserved rows ------------------------------------------------

test_that("an unsorted index gives its reserved rows ascending, with their positions in the same order", {
  index <- c(50L, 10L, 40L, 20L, 30L, 60L)
  drawn <- sieve_holdout_draw(index, .5, seed = 1)
  expect_false(is.unsorted(drawn$rows))
  expect_identical(index[drawn$positions], drawn$rows)
  # The same rows as the reference draw, in ascending order.
  old <- iv_draw_per_stratum_rounding(index, .5, seed = 1)
  expect_identical(drawn$rows, sort(old$rows))
  expect_true(is.unsorted(old$rows))
})

# ---- Seeds --------------------------------------------------------------------------------

test_that("a supplied seed must be one whole number in R's integer range, refused before the stream is touched", {
  set.seed(9)
  before <- .Random.seed
  message <- "`seed` must be NULL or one whole number from -2147483647 to 2147483647; %s was given."
  for (bad in list(1.9, 3e9, -2147483648, TRUE, "1", NA, NA_integer_, Inf, NaN, c(1, 2), factor(1))) {
    expect_error(sieve_draw_seed(bad), sprintf(message, sieve_given_text(bad)), fixed = TRUE,
      class = "sieve_seed_invalid")
  }
  expect_identical(sieve_draw_seed(113)$seed, 113L)
  expect_identical(sieve_draw_seed(113L)$seed, 113L)
  expect_identical(sieve_draw_seed(-5)$seed, -5L)
  expect_identical(sieve_draw_seed(2147483647)$seed, .Machine$integer.max)
  expect_identical(.Random.seed, before)
  # 1.9 is not run as seed 1 and recorded as the supplied seed 1.
  expect_error(sieve_holdout_draw(seq_len(301), .3, seed = 1.9), "1.9 was given", fixed = TRUE,
    class = "sieve_seed_invalid")
  expect_error(sieve_with_seed(7.8, stats::runif(1)), "7.8 was given", fixed = TRUE, class = "sieve_seed_invalid")
  expect_identical(.Random.seed, before)
  # The exported reliability specification reads its seed strictly.
  spec <- function(seed) sieve_reliability_spec(c("a", "b", "c"), rep("continuous", 3), seed = seed)
  expect_identical(spec(7)$seed, 7L)
  expect_null(spec(NULL)$seed)
  for (bad in list("abc", 3e9, c(1, 2), 7.8)) {
    expect_error(spec(bad), sprintf(message, sieve_given_text(bad)), fixed = TRUE, class = "sieve_seed_invalid")
  }
})

test_that("a seed box left empty means no seed, and any other invalid entry is refused with the reason", {
  for (cleared in list(NULL, NA, NA_real_, NA_integer_, "", "  ", numeric(0))) {
    expect_null(sieve_seed_input(cleared))
  }
  expect_identical(sieve_seed_input(12), 12L)
  expect_identical(sieve_seed_input(-3L), -3L)
  for (bad in list(12.7, 3e9, "abc", TRUE, Inf, c(1, 2))) {
    refusal <- tryCatch(sieve_seed_input(bad), error = function(e) e)
    # Raised through shiny::validate(): a page shows the reason where its
    # result would be, and an observer stops without ending the session.
    expect_s3_class(refusal, "sieve_seed_invalid")
    expect_s3_class(refusal, "shiny.silent.error")
    expect_s3_class(refusal, "validation")
    expect_identical(conditionMessage(refusal), sprintf(paste(
      "The random seed must be a whole number from -2147483647 to 2147483647; %s was entered.",
      "Clear the box to have a seed drawn and recorded."
    ), sieve_given_text(bad)))
  }
})

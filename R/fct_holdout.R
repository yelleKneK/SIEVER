# fct_holdout.R
# An optional validation reservation made before development, honored by
# every development page.
#
# A reservation draws a proportion of the rows in the analysis set by a
# seed (optionally within the levels of a declared grouping column) and
# removes them from the analysis through the one respondent path
# (sieve_remove_cases(), fct_cases.R) with the reason "reserved for
# validation" and a marker, so every development result (the shared
# correlation matrix, Dimensionality, EFA, Items, CFA, Reliability) reads
# the calibration rows only, without further change, and the ordinary
# Restore of the Screening page does not return a reserved row: only
# Release the reservation does. The reservation is recorded in the state
# (state$holdout), in saved projects, and in the session script, whose
# recorded step draws the same rows from the same seed.
#
# On the CV and Invariance page, a named, frozen version of the measurement
# model (item set, model syntax, estimator, scoring specification, with a
# hash and a time) is fit on the calibration rows and evaluated on the
# reserved rows once, through sieve_cross_validate() with the rows given
# rather than drawn. The result records the version and says whether the
# version was specified before the first evaluation or revised during
# development afterwards; an earlier evaluation then becomes historical.
#
# The methodological basis is Browne's (2000) account of cross-validation:
# a model's predictive validity is assessed on a validation sample that
# played no part in fitting or selecting it, and a split made after the
# full data were examined does not restore that protection (Cudeck &
# Browne, 1983). The Standards (AERA, APA, & NCME, 2014) ask that a validity
# claim rest on evidence gathered for the interpretation proposed; evidence
# from the sample that shaped the item set is not evidence that the item set
# generalizes beyond it. Fitting a CFA on the CFA page does not make a model
# confirmatory; a model is confirmed by data that did not shape it.

# ---- Sizes and the draw (Shiny-free, exported) ----------------------------------

#' A valid reservation proportion
#'
#' Checked before any coercion: text such as `"0.3"` and a logical value are
#' refused, not converted.
#'
#' @param proportion One number strictly between zero and one.
#' @param arg The argument's name, for the refusal.
#' @return The proportion, as a double.
#' @author Ken Kelley
#' @noRd
sieve_holdout_proportion <- function(proportion, arg = "proportion") {
  if (!is.numeric(proportion) || length(proportion) != 1L || !is.finite(proportion) ||
      proportion <= 0 || proportion >= 1) {
    stop(sprintf(
      "`%s` must be one number strictly between 0 and 1; %s was given.", arg, sieve_given_text(proportion)
    ), call. = FALSE)
  }
  as.double(proportion)
}

#' The stratum of each row, keyed without a label
#'
#' Each distinct value is a stratum, in the order of first appearance, and a
#' missing value is a stratum of its own: `match()` pairs `NA` with `NA` and
#' text with text, so no value in the data can be confused with the missing
#' stratum. (A missing value relabeled as text, say `"(missing)"`, would
#' pool a row whose group is the text "(missing)" with the rows that have
#' no group.) The words for the missing stratum belong to the display.
#'
#' @param strata A vector, one value per row.
#' @return A list with `key` (each row's stratum, an integer from 1 to the
#'   number of strata) and `levels` (the value of each stratum as text, `NA`
#'   for the stratum of missing values).
#' @author Ken Kelley
#' @noRd
sieve_holdout_strata <- function(strata) {
  s <- if (is.factor(strata)) as.character(strata) else as.character(sieve_plain_values(strata))
  levels <- unique(s)
  list(key = match(s, levels), levels = levels)
}

#' The rule by which a stratified reservation is allocated, in words
#'
#' @return One string, recorded with every set of sizes and every draw.
#' @author Ken Kelley
#' @noRd
sieve_holdout_allocation_rule <- function() {
  paste(
    "largest remainder (Hamilton) proportional allocation: the total reserved is round(N * proportion),",
    "the size the reservation has without strata (R's round() rounds a half to the even number);",
    "each stratum h first reserves floor(N_h * proportion) rows, and the rows that remain go one each",
    "to the strata with the largest remainders N_h * proportion - floor(N_h * proportion), the seed",
    "choosing among strata whose remainders tie at the cut"
  )
}

#' The Sizes a Validation Reservation Would Give
#'
#' The number of rows a reservation of a given proportion reserves and the
#' number it leaves for calibration, overall or within each stratum of a
#' grouping variable. The total reserved is `round(N * proportion)` with or
#' without strata (R's [round()] rounds a half to the even number, so 2.5
#' gives 2), and with strata it is allocated by largest remainder
#' (Hamilton's method of proportional allocation): stratum \eqn{h} first
#' reserves \eqn{\lfloor N_h p \rfloor}{floor(N_h p)} rows, and the rows that remain go
#' one each to the strata with the largest remainders
#' \eqn{N_h p - \lfloor N_h p \rfloor}{N_h p - floor(N_h p)}. The total is therefore exact and
#' every stratum is within one row of its quota \eqn{N_h p}. When strata
#' whose remainders are equal straddle the cut (equal strata at a
#' proportion of .5, or many strata of one row), the rows that remain
#' among them are placed by the seed at the draw ([sieve_holdout_draw()]),
#' never by the order of the data; their `reserved` here is the smaller
#' count, `by_seed` is `TRUE`, and the attribute `by_seed_rows` says how
#' many rows the seed places among them. The counts are integer index
#' arithmetic, not rounded statistics.
#'
#' Reserving `round(N_h * proportion)` in each stratum on its own would let
#' the stratified total drift from the requested proportion (40 strata of
#' 11 rows at .25 would reserve 120 rows where 110 are requested, and 20
#' strata of one row at .3 would reserve none), and halves rounded to even
#' would bias exact-half quotas downward.
#'
#' @param n The number of rows in the analysis set, one whole number (10.8
#'   or `"10"` is refused, not read as 10). With `strata`, it must equal
#'   `length(strata)`.
#' @param proportion The proportion of the rows to reserve, one number
#'   strictly between zero and one.
#' @param strata `NULL`, or a vector with one value per row naming each
#'   row's stratum; a missing value forms a stratum of its own
#'   (`stratum = NA`, `missing = TRUE`), apart from every value in the data,
#'   the text "(missing)" included.
#' @return A data frame with one row per stratum, in the order of first
#'   appearance (one row, `stratum = "(all)"`, without strata): `stratum`
#'   (the value as text; `NA` for the stratum of missing values), `missing`
#'   (whether the row is that stratum), `n`, `quota` (\eqn{N_h p}, at full
#'   precision), `reserved`, `calibration`, `by_seed` (whether the seed
#'   places one more row in this stratum or in another whose remainder ties
#'   with it), and `empty_half` (whether the stratum has no reserved row or
#'   no calibration row whichever way the seed places the tied rows). The
#'   attributes `allocation` (the rule, in words), `total` (the rows
#'   reserved in all) and `by_seed_rows` (the rows the seed places among
#'   the strata marked `by_seed`) are set.
#' @author Ken Kelley
#' @examples
#' sieve_holdout_sizes(301, proportion = .3)
#' school <- lavaan::HolzingerSwineford1939$school
#' sieve_holdout_sizes(length(school), proportion = .3, strata = school)
#' # Ten strata of three rows at .5: 15 rows in all, one or two from each,
#' # the seed choosing which five strata give two.
#' sizes <- sieve_holdout_sizes(30, proportion = .5, strata = rep(letters[1:10], 3))
#' sizes
#' attr(sizes, "by_seed_rows")
#' @export
sieve_holdout_sizes <- function(n, proportion, strata = NULL) {
  p <- sieve_holdout_proportion(proportion)
  n <- sieve_row_count(n, "n")
  if (is.null(strata)) {
    out <- data.frame(stratum = "(all)", missing = FALSE, n = n, stringsAsFactors = FALSE)
  } else {
    st <- sieve_holdout_strata(strata)
    if (length(st$key) != n) {
      stop(sprintf(
        "`strata` must have one value per row of the analysis set: %d values were given for %d rows.",
        length(st$key), n
      ), call. = FALSE)
    }
    out <- data.frame(
      stratum = st$levels, missing = is.na(st$levels), n = tabulate(st$key, length(st$levels)),
      stringsAsFactors = FALSE
    )
  }
  total <- as.integer(round(n * p))
  quota <- out$n * p
  # A quota that is a whole number up to representation error (10 * .3 is
  # 3.0000000000000004, 0.7 * 10 is 6.999999999999999) is that number.
  tol <- sqrt(.Machine$double.eps)
  base <- as.integer(floor(quota + tol))
  remainder <- pmax(quota - base, 0)
  left <- total - sum(base)
  extra <- integer(nrow(out))
  by_seed <- logical(nrow(out))
  tied_rows <- 0L
  if (left > 0L) {
    ranked <- order(-remainder)
    cut <- remainder[ranked[left]]
    above <- remainder > cut + tol
    tied <- abs(remainder - cut) <= tol
    extra[above] <- 1L
    tied_rows <- left - sum(above)
    if (tied_rows == sum(tied)) {
      # The tie does not straddle the cut: every tied stratum gets a row.
      extra[tied] <- 1L
      tied_rows <- 0L
    } else {
      by_seed <- tied
    }
  }
  out$quota <- quota
  out$reserved <- base + extra
  out$calibration <- out$n - out$reserved
  out$by_seed <- by_seed
  # A half is empty for certain when it is empty with the smaller count and,
  # for a stratum the seed may give one more row, with that row as well.
  empty_now <- out$reserved == 0L | out$calibration == 0L
  empty_more <- out$reserved + 1L == out$n
  out$empty_half <- empty_now & (!out$by_seed | empty_more)
  rownames(out) <- NULL
  attr(out, "allocation") <- sieve_holdout_allocation_rule()
  attr(out, "total") <- total
  attr(out, "by_seed_rows") <- as.integer(tied_rows)
  out
}

#' Draw the Rows of a Validation Reservation
#'
#' Draws, by a seed, the rows of an analysis set to reserve for validation,
#' optionally within the strata of a grouping variable (largest remainder
#' proportional allocation, see [sieve_holdout_sizes()]). The draw runs on a
#' local random number stream: a supplied seed leaves the caller's stream
#' exactly as it was, and with `seed = NULL` one seed is drawn from the
#' caller's stream and returned, so the same rows can be drawn again.
#' When strata whose remainders tie straddle the cut,
#' the seed first chooses which of them reserve one more row (one call to
#' [sample.int()] over those strata), and then the rows are drawn within
#' each stratum; a draw without such a tie uses the stream only for the
#' draws within the strata.
#'
#' The reservation is the device by which a cross-validation can be more
#' than internal assessment: rows reserved before the structure and the
#' items are examined play no part in fitting or selecting the model, which
#' is the condition under which a validation sample estimates predictive
#' validity (Browne, 2000; Cudeck & Browne, 1983). A reservation drawn after
#' results exist is still a split, but of data already examined.
#'
#' @param index The row identities of the analysis set (the original row
#'   numbers of the rows now in the analysis): whole numbers of 1 or more,
#'   without repeats. They are checked before any coercion: a logical
#'   vector, a factor, text, and missing, fractional, zero, negative or
#'   repeated values are each refused with the reason (a factor of
#'   identifiers would otherwise be read as its level codes, so other rows
#'   would be reserved).
#' @param proportion The proportion to reserve, one number strictly between
#'   zero and one.
#' @param seed `NULL` to draw and record a seed, or one whole number from
#'   -2147483647 to 2147483647 (1.9 is refused, not run as 1).
#' @param strata `NULL`, or one stratum value per element of `index`.
#' @return A list with `rows` (the reserved row identities, ascending),
#'   `positions` (their positions in `index`, in the same order, so
#'   `index[positions]` is `rows`), `rng` (the seed used, whether it was
#'   supplied, and the [RNGkind()] settings), `sizes` (from
#'   [sieve_holdout_sizes()], with the counts the draw realized: `reserved`
#'   and `calibration` include the rows the seed placed, and `realized` is
#'   each stratum's reserved share), `allocation` (the rule, in words), and
#'   `proportion`. Every refusal is made before the random number stream is
#'   touched.
#' @author Ken Kelley
#' @references
#' Browne, M. W. (2000). Cross-validation methods. *Journal of Mathematical
#' Psychology, 44*, 108--132.
#'
#' Cudeck, R., & Browne, M. W. (1983). Cross-validation of covariance
#' structures. *Multivariate Behavioral Research, 18*, 147--167.
#' @examples
#' drawn <- sieve_holdout_draw(seq_len(301), proportion = .3, seed = 1)
#' length(drawn$rows)
#' drawn$sizes
#' # Within the two schools, largest remainder proportional allocation.
#' school <- lavaan::HolzingerSwineford1939$school
#' sieve_holdout_draw(seq_len(301), proportion = .3, seed = 1, strata = school)$sizes
#' @export
sieve_holdout_draw <- function(index, proportion, seed = NULL, strata = NULL) {
  index <- sieve_row_positions(index, arg = "index")
  n <- length(index)
  sizes <- sieve_holdout_sizes(n, proportion, strata)
  # Each row's stratum by its key, the row of `sizes` it belongs to.
  key <- if (is.null(strata)) rep(1L, n) else sieve_holdout_strata(strata)$key
  tied <- which(sizes$by_seed)
  placed <- attr(sizes, "by_seed_rows")
  drawn <- sieve_with_seed(seed, {
    reserved <- sizes$reserved
    if (placed > 0L) {
      # The seed, not the order of the data, chooses which of the tied
      # strata reserve one more row.
      chosen <- tied[sample.int(length(tied), placed)]
      reserved[chosen] <- reserved[chosen] + 1L
    }
    picks <- integer(0)
    for (i in seq_len(nrow(sizes))) {
      at <- which(key == i)
      k <- reserved[i]
      if (k > 0L) {
        picks <- c(picks, at[sample.int(length(at), k)])
      }
    }
    # Rows ascending, positions in the same order (for an ascending index,
    # the only kind the application makes, this is sort(picks)).
    list(picks = picks[order(index[picks])], reserved = reserved)
  })
  sizes$reserved <- drawn$value$reserved
  sizes$calibration <- sizes$n - sizes$reserved
  sizes$empty_half <- sizes$reserved == 0L | sizes$calibration == 0L
  sizes$realized <- ifelse(sizes$n > 0L, sizes$reserved / sizes$n, NA_real_)
  attr(sizes, "by_seed_rows") <- 0L
  list(
    rows = index[drawn$value$picks], positions = drawn$value$picks, rng = drawn$rng,
    sizes = sizes, allocation = sieve_holdout_allocation_rule(),
    proportion = sieve_holdout_proportion(proportion)
  )
}

#' Why a reservation of these sizes is refused
#'
#' The rule the calibration/validation split applies, each half at least 20
#' rows, and the MLR refusal of the model tabs on the smaller half for the
#' current model (`sieve_spec_mlr_guard()`), so a reservation never leaves
#' a half the model cannot be fit on.
#'
#' @param reserved,calibration The two sizes.
#' @param spec The current model specification (`sieve_cfa_spec()`), or
#'   `NULL` to skip the MLR refusal.
#' @param minimum The smallest half allowed.
#' @return `NULL` when the reservation may proceed, otherwise the reason,
#'   one string.
#' @author Ken Kelley
#' @noRd
sieve_holdout_refusal <- function(reserved, calibration, spec = NULL, minimum = 20L) {
  smaller <- min(reserved, calibration)
  if (smaller < minimum) {
    return(sprintf(
      paste(
        "this reservation would give %d reserved and %d calibration rows; each half needs at",
        "least %d rows (the rule the calibration/validation split applies), so nothing was reserved"
      ),
      as.integer(reserved), as.integer(calibration), as.integer(minimum)
    ))
  }
  if (!is.null(spec)) {
    why <- sieve_spec_mlr_guard(spec, n = smaller)
    if (!is.null(why)) {
      return(sprintf("the smaller half (%d rows) cannot carry the current model: %s", as.integer(smaller), why))
    }
  }
  NULL
}

# ---- The reservation in the state ---------------------------------------------

#' The validation reservation in force, if any
#'
#' The last record of `state$holdout` whose status is `"reserved"` and that
#' belongs to the loaded dataset. Read where called, so a reactive that
#' calls this follows a reservation or a release.
#'
#' @param state The shared state store.
#' @return The record, or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_holdout <- function(state) {
  records <- state$holdout %||% list()
  if (!length(records)) {
    return(NULL)
  }
  rec <- records[[length(records)]]
  if (!is.list(rec) || !identical(rec$status, "reserved")) {
    return(NULL)
  }
  if (!identical(rec$dataset_id, state$dataset_id %||% NA_character_)) {
    return(NULL)
  }
  rec
}

#' The reservation records without their evaluations
#'
#' What a recorded input revision carries (see `sieve_replay_inputs()`):
#' which rows are reserved, by what proportion and seed, and when, without
#' the fitted results of the evaluations.
#'
#' @param state The shared state store (read under [shiny::isolate()]).
#' @return A list of brief records (possibly empty).
#' @author Ken Kelley
#' @noRd
sieve_holdout_brief <- function(state) {
  records <- shiny::isolate(state$holdout) %||% list()
  lapply(records, function(rec) {
    rec[intersect(
      c("id", "status", "rows", "proportion", "rng", "stratify", "time", "after_development", "released"),
      names(rec)
    )]
  })
}

#' The rows a reservation holds in the removed respondents
#'
#' @param state The shared state store.
#' @param id The reservation identifier.
#' @return The held records (elements of `state$removed_cases`) marked
#'   with that reservation, in the order of the data as loaded.
#' @author Ken Kelley
#' @noRd
sieve_holdout_held <- function(state, id) {
  held <- state$removed_cases %||% list()
  mine <- held[vapply(held, function(r) is.list(r) && identical(r$holdout, id), logical(1))]
  mine[order(vapply(mine, function(r) as.integer(r$row), integer(1)))]
}

#' The current model specification the reservation sizes are checked against
#'
#' The model the CFA page fits, read from its saved settings
#' (`state$cfa_settings`, through `sieve_cfa_choices()`): its structure,
#' its facets and its identification, with the latent names planned as the
#' page plans them, not the default first-order model with the variances
#' at 1 whatever the page chose, because the CV and Invariance page
#' promises the syntax the CFA page fits.
#'
#' @param state The shared state store.
#' @param estimator The estimator choice; by default the study's setting.
#' @return A specification from `sieve_cfa_spec()` with `choices` (the
#'   `sieve_cfa_choices()` it was built from) added, or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_holdout_spec <- function(state, estimator = NULL) {
  meta <- state$meta
  items <- state$items
  if (is.null(meta) || is.null(items)) {
    return(NULL)
  }
  choice <- sieve_estimator_effective(estimator, state)
  choices <- sieve_cfa_choices(state$cfa_settings)
  spec <- tryCatch(sieve_cfa_spec(meta,
    structure = choices$structure, facet_ids = choices$facet_ids,
    std_lv = !identical(choices$ident, "marker"),
    reserved = union(names(items), names(state$raw)), estimator = choice
  ), error = function(e) NULL)
  if (!is.null(spec)) spec$choices <- choices
  spec
}

#' What a reservation would do to the current analysis set
#'
#' The sizes it would give (overall and by stratum) and the reason it would
#' be refused, computed before any row is drawn, so the Screening page can
#' show them as the proportion moves and the reservation itself can refuse
#' without touching the random number stream.
#'
#' @param state The shared state store.
#' @param proportion The proportion to reserve.
#' @param stratify `NULL`, `""`, or the name of a column of the data as
#'   loaded to stratify by.
#' @return A list with `n`, `sizes`, `reserved`, `calibration`, `strata`
#'   (the stratifying column's value for each row, or `NULL`), `stratify`
#'   (the column, or
#'   `NA`), and `refusal` (`NULL` or the reason). An invalid proportion or
#'   an unknown column gives a `refusal` and no sizes.
#' @author Ken Kelley
#' @noRd
sieve_holdout_plan <- function(state, proportion, stratify = NULL) {
  raw <- state$raw
  n <- NROW(raw)
  p <- tryCatch(sieve_holdout_proportion(proportion), error = function(e) NULL)
  if (is.null(p)) {
    return(list(n = n, refusal = "the proportion to reserve must be a number strictly between 0 and 1, so nothing was reserved"))
  }
  column <- if (length(stratify) == 1L && !is.na(stratify) && nzchar(stratify)) stratify else NA_character_
  strata <- NULL
  if (!is.na(column)) {
    if (is.null(raw) || !column %in% names(raw)) {
      return(list(n = n, refusal = sprintf(
        "%s is not a column of the loaded data, so nothing was reserved", column
      )))
    }
    strata <- raw[[column]]
  }
  sizes <- sieve_holdout_sizes(n, p, strata)
  # The total is exact under the allocation rule; the seed places only
  # which tied strata give the remaining rows.
  reserved <- attr(sizes, "total")
  calibration <- n - reserved
  list(
    n = n, proportion = p, sizes = sizes, reserved = reserved, calibration = calibration,
    strata = strata, stratify = column,
    refusal = sieve_holdout_refusal(reserved, calibration, sieve_holdout_spec(state))
  )
}

#' The development results that exist when a reservation is drawn
#'
#' A reservation drawn before any development result exists protects an
#' evaluation; one drawn afterwards is recorded as made after development
#' began, with the results that preceded it: the records of the analysis
#' registry (every model fit), the analysis steps of the session log that
#' were requested (a Dimensionality, EFA, Items, CFA or Reliability result,
#' among others), and the results the other pages published this session.
#' The Screening page's own descriptives, correlations and respondent
#' screens, and the shared correlation matrix it computes on view (the
#' passive steps of the log), are screening, which the pipeline places
#' before development and where the reservation is made; they do not count,
#' and the matrix is computed again from the calibration rows afterwards.
#'
#' @param state The shared state store (read under [shiny::isolate()]).
#' @return A list with `analyses` (a data frame with `id`, `kind`, `note`
#'   and `time`, possibly with no rows), `steps` (the notes of the requested
#'   analysis steps, possibly empty), and `pages` (the titles of results
#'   other pages published, possibly empty).
#' @author Ken Kelley
#' @noRd
sieve_holdout_preceding <- function(state) {
  shiny::isolate({
    records <- state$analyses %||% list()
    pick <- function(field) vapply(records, function(r) as.character(r[[field]] %||% NA_character_), character(1))
    analyses <- data.frame(
      id = pick("id"), kind = pick("kind"), note = pick("note"), time = pick("time"),
      stringsAsFactors = FALSE
    )
    log <- state$codelog %||% character(0)
    stamps <- grep("^# \\[([0-9]+)\\] ", log, value = TRUE)
    numbers <- sub("^# \\[([0-9]+)\\].*", "\\1", stamps)
    operations <- state$replay_operations %||% list()
    requested <- vapply(numbers, function(k) {
      identical(operations[[k]]$kind, "analysis") && !isTRUE(operations[[k]]$passive)
    }, logical(1))
    pages <- state$page_results %||% list()
    published <- unlist(lapply(setdiff(names(pages), c("screen", "ingest")), function(page) {
      vapply(pages[[page]], function(r) {
        if (is.list(r) && r$state %in% c("current", "stale", "failed")) as.character(r$title %||% page) else NA_character_
      }, character(1))
    }), use.names = FALSE)
    list(
      analyses = analyses, steps = unname(sub("^# \\[[0-9]+\\] ", "", stamps[requested])),
      pages = unique(published[!is.na(published)])
    )
  })
}

#' The R that draws a reservation's rows again from its seed
#'
#' @param rec A reservation record.
#' @return A character vector of R source lines: the random number kind, the
#'   draw by `sieve_holdout_draw()`, and a check that it gives the recorded
#'   rows.
#' @author Ken Kelley
#' @noRd
sieve_holdout_reserve_code <- function(rec) {
  strata <- if (is.na(rec$stratify)) "NULL" else sprintf("d[[%s]]", encodeString(rec$stratify, quote = "\""))
  c(
    sprintf(
      "# Validation reservation %s: %d of %d rows (proportion %s), drawn by seed %d%s%s.",
      rec$id, length(rec$rows), rec$n_reserved + rec$n_calibration, format(rec$proportion, digits = 17),
      rec$rng$seed, if (isTRUE(rec$rng$supplied)) "" else " (drawn for this run)",
      if (is.na(rec$stratify)) "" else sprintf(", stratified by %s", rec$stratify)
    ),
    "# The reserved rows leave the analysis set through the respondent path and are read",
    "# by no development result; Release the reservation on the Screening page returns them.",
    if (isTRUE(rec$after_development)) {
      "# Reserved after development began: results existed when the rows were drawn, so"
    },
    if (isTRUE(rec$after_development)) {
      "# an evaluation on them is a split of data already examined, not independent validation."
    },
    "if (!exists(\"case_index\")) case_index <- seq_len(nrow(d))",
    sprintf("do.call(RNGkind, as.list(%s))", paste(utils::capture.output(dput(rec$rng$kind)), collapse = "")),
    sprintf(
      "reserved <- SIEVER::sieve_holdout_draw(case_index, proportion = %s, seed = %dL, strata = %s)$rows",
      format(rec$proportion, digits = 17), rec$rng$seed, strata
    ),
    sprintf(
      "stopifnot(identical(reserved, c(%s)))",
      paste(sprintf("%dL", as.integer(rec$rows)), collapse = ", ")
    )
  )
}

#' Reserve rows for validation
#'
#' Draws a proportion of the rows in the analysis set by a seed (see
#' [sieve_holdout_draw()]), optionally within the levels of a grouping
#' column, records the reservation in `state$holdout` (the rows as original
#' row identities, the proportion, the seed, the time, and the revision it
#' was drawn from), and removes the reserved rows from the analysis through
#' the one respondent path (`sieve_remove_cases()`) with the reason
#' "reserved for validation" and a marker, so the ordinary Restore does not
#' return them. Every development result therefore reads the calibration
#' rows only. The step is logged with the R that draws the same rows from
#' the seed.
#'
#' A reservation is refused, before any row is drawn, when a half would be
#' too small for the current model (`sieve_holdout_refusal()`), when a
#' reservation is already in force, when the data are not loaded, or when
#' the seed is not a whole number from -2147483647 to 2147483647. A
#' reservation drawn after results exist is allowed and recorded as made
#' after development began, with the results that preceded it.
#'
#' @param state The shared state store.
#' @param proportion The proportion of the analysis set to reserve.
#' @param seed `NULL` to draw and record a seed, or one whole number from
#'   -2147483647 to 2147483647. It is read here, so the Screening page's
#'   `sieve_seed_input()` refusal of an invalid entry (12.7, 3e9) becomes
#'   this function's refusal, shown on the page, rather than a silent stop.
#' @param stratify `NULL`, or the name of a grouping column of the data as
#'   loaded.
#' @return Invisibly, a list with `reserved` (the original rows reserved),
#'   `record` (the reservation record, or `NULL`), and `refused` (`NULL`,
#'   or the reason nothing was reserved).
#' @author Ken Kelley
#' @noRd
sieve_reserve_holdout <- function(state, proportion, seed = NULL, stratify = NULL) {
  shiny::isolate({
    nothing <- function(why) invisible(list(reserved = integer(0), record = NULL, refused = why))
    if (is.null(state$raw) || is.null(state$items) || is.null(state$meta)) {
      return(nothing("no data are loaded, so nothing was reserved"))
    }
    if (!is.null(sieve_holdout(state))) {
      return(nothing(sprintf(
        "reservation %s is already in force; release it before drawing another", sieve_holdout(state)$id
      )))
    }
    seed <- tryCatch(sieve_seed_value(seed), sieve_seed_invalid = function(e) e)
    if (inherits(seed, "condition")) {
      return(nothing(paste(conditionMessage(seed), "Nothing was reserved.")))
    }
    plan <- sieve_holdout_plan(state, proportion, stratify)
    if (!is.null(plan$refusal)) {
      return(nothing(plan$refusal))
    }
    sieve_ensure_dataset(state)
    index <- sieve_case_index(state)
    drawn <- sieve_holdout_draw(index, plan$proportion, seed = seed, strata = plan$strata)
    if (!length(drawn$rows)) {
      return(nothing("the proportion reserves no row, so nothing was reserved"))
    }
    preceding <- sieve_holdout_preceding(state)
    records <- state$holdout %||% list()
    rec <- list(
      id = sprintf("R%d", length(records) + 1L),
      status = "reserved",
      rows = as.integer(drawn$rows),
      proportion = plan$proportion,
      rng = drawn$rng,
      stratify = plan$stratify,
      sizes = drawn$sizes,
      allocation = drawn$allocation,
      n_reserved = length(drawn$rows),
      n_calibration = length(index) - length(drawn$rows),
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      dataset_id = state$dataset_id,
      revision = sieve_revision(state),
      after_development = nrow(preceding$analyses) > 0L || length(preceding$steps) > 0L ||
        length(preceding$pages) > 0L,
      preceded_by = preceding,
      versions = list(),
      evaluations = list(),
      released = NULL
    )
    rec <- sieve_audit_stamp(rec, state)
    note <- sprintf(
      paste(
        "Screening: reserve %d of %d respondents for validation (reservation %s, proportion %s,",
        "seed %d%s%s)%s; %d calibration rows remain"
      ),
      rec$n_reserved, length(index), rec$id, format(rec$proportion, digits = 7),
      rec$rng$seed, if (isTRUE(rec$rng$supplied)) "" else " drawn for this run",
      if (is.na(rec$stratify)) "" else sprintf(", stratified by %s", rec$stratify),
      if (isTRUE(rec$after_development)) ", reserved after development began" else "",
      rec$n_calibration
    )
    state$holdout <- c(records, list(rec))
    out <- sieve_remove_cases(state, rec$rows,
      reason = "reserved for validation",
      holdout = list(id = rec$id, note = note, code = sieve_holdout_reserve_code(rec))
    )
    if (!is.null(out$refused) || !length(out$removed)) {
      state$holdout <- records
      return(nothing(out$refused %||% "no row could be removed from the analysis, so nothing was reserved"))
    }
    invisible(list(reserved = rec$rows, record = rec, refused = NULL))
  })
}

#' Release the validation reservation
#'
#' Restores the reserved rows to the analysis through the one respondent
#' path (`sieve_restore_cases()`, with the reservation named, which the
#' ordinary Restore cannot do), marks the record released, and logs the
#' step: development then sees the reserved rows, and the reservation's
#' protection of an evaluation is gone. The record and its evaluations are
#' kept.
#'
#' @param state The shared state store.
#' @return Invisibly, a list with `restored` (the original rows restored),
#'   `record` (the released record, or `NULL`), and `refused` (`NULL`, or
#'   the reason nothing was released).
#' @author Ken Kelley
#' @noRd
sieve_release_holdout <- function(state) {
  shiny::isolate({
    rec <- sieve_holdout(state)
    if (is.null(rec)) {
      return(invisible(list(restored = integer(0), record = NULL, refused = "no validation reservation is in force, so nothing was released")))
    }
    held <- sieve_holdout_held(state, rec$id)
    rows <- vapply(held, function(r) as.integer(r$row), integer(1))
    note <- sprintf(
      paste(
        "Screening: release validation reservation %s: restore %d reserved respondents to the analysis;",
        "development now sees the reserved rows and the reservation's protection of an evaluation is gone"
      ),
      rec$id, length(rows)
    )
    out <- if (length(rows)) {
      sieve_restore_cases(state, rows, holdout = list(id = rec$id, note = note))
    } else {
      list(restored = integer(0), unknown = integer(0), refused = NULL)
    }
    if (!is.null(out$refused)) {
      return(invisible(list(restored = integer(0), record = NULL, refused = out$refused)))
    }
    records <- state$holdout %||% list()
    at <- which(vapply(records, function(r) identical(r$id, rec$id), logical(1)))
    rec$status <- "released"
    rec$released <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    records[[at[length(at)]]] <- rec
    state$holdout <- records
    invisible(list(restored = out$restored, record = rec, refused = NULL))
  })
}

#' The sample size sentence a page shows while rows are reserved
#'
#' One sentence every development page can read beside its n, so the n it
#' states is understood as the calibration rows: `"n = 211 calibration
#' rows; 90 reserved for validation (reservation R1), read by no development
#' result."`.
#'
#' @param state The shared state store (read where called).
#' @param n The rows the page analyzes; by default the rows of the item
#'   data.
#' @return One sentence, or `NULL` when no reservation is in force.
#' @author Ken Kelley
#' @noRd
sieve_holdout_n_sentence <- function(state, n = NULL) {
  rec <- sieve_holdout(state)
  if (is.null(rec)) {
    return(NULL)
  }
  n <- n %||% NROW(state$items)
  sprintf(
    "n = %d calibration rows; %d reserved for validation (reservation %s), read by no development result.",
    as.integer(n), as.integer(rec$n_reserved), rec$id
  )
}

#' The reservation sentence as a tag beside a page's method line
#'
#' @param state The shared state store (read where called).
#' @return A span holding `sieve_holdout_n_sentence()` with a leading space,
#'   or `NULL` when no reservation is in force (nothing is added).
#' @author Ken Kelley
#' @noRd
sieve_holdout_n_tag <- function(state) {
  sentence <- sieve_holdout_n_sentence(state)
  if (is.null(sentence)) {
    return(NULL)
  }
  shiny::tags$span(" ", sentence)
}

# ---- Frozen versions and the evaluation ---------------------------------------

#' The current measurement model as a version to freeze
#'
#' The item set (the included items with their construct, facet, type and
#' keying), the model syntax of the correlated-factors CFA the CFA page
#' fits, the estimator in force, and the scoring specification, with a
#' content hash and the time. Two states of development that differ in
#' none of these give the same hash, so the same version.
#'
#' @param state The shared state store (read under [shiny::isolate()]).
#' @param estimator The estimator choice (`"auto"`, `"mlr"`, `"ml_fiml"`,
#'   `"ml"`, `"wlsmv"`, or `"study"`).
#' @return A list with `items` (a data frame), `syntax`, `std_lv`,
#'   `model` (the model in words, `sieve_cfa_choices_text()`), `estimator`
#'   (the choice, the resolved estimator, the missing-data treatment and the
#'   ordered items), `scoring`, `hash` and `time`. The marker
#'   identification enters the hash only when chosen, so a version of the
#'   default model (factor variances at 1) has the same hash as a saved
#'   version of that model whose record does not state its identification.
#' @author Ken Kelley
#' @noRd
sieve_holdout_version <- function(state, estimator = "auto") {
  shiny::isolate({
    meta <- state$meta
    ledger <- sieve_ledger_analytic(meta)
    items <- if (is.null(ledger)) NULL else ledger[ledger$status == "active", setdiff(names(ledger), "status"), drop = FALSE]
    if (!is.null(items)) rownames(items) <- NULL
    choice <- sieve_estimator_effective(estimator, state)
    spec <- sieve_holdout_spec(state, choice)
    est <- list(
      choice = choice, resolved = spec$estimator %||% NA_character_,
      missing = spec$missing %||% NA_character_, ordered = spec$ordered %||% character(0)
    )
    scoring <- list(policy = state$score_policy %||% list(), policies = state$scoring_policies %||% list())
    syntax <- spec$model %||% ""
    std_lv <- !isFALSE(spec$std_lv)
    identity <- list(items = items, syntax = syntax, estimator = est, scoring = scoring)
    if (!std_lv) identity$identification <- "marker"
    list(
      items = items, syntax = syntax, std_lv = std_lv,
      model = sieve_cfa_choices_text(spec$choices %||% sieve_cfa_choices(NULL)),
      estimator = est, scoring = scoring,
      hash = sieve_hash(identity),
      time = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  })
}

#' Freeze the current version in the reservation in force
#'
#' Adds the current version (`sieve_holdout_version()`) to the record when
#' its hash is new, named `v1`, `v2`, and so on, and notes whether an
#' evaluation already existed when it was created; a version with the hash
#' of an existing one is that version.
#'
#' @param state The shared state store.
#' @param estimator The estimator choice.
#' @return The version (with `name` and `after_evaluation`), or `NULL` when
#'   no reservation is in force.
#' @author Ken Kelley
#' @noRd
sieve_holdout_freeze <- function(state, estimator = "auto") {
  shiny::isolate({
    rec <- sieve_holdout(state)
    if (is.null(rec)) {
      return(NULL)
    }
    version <- sieve_holdout_version(state, estimator)
    for (v in rec$versions) {
      if (identical(v$hash, version$hash)) {
        return(v)
      }
    }
    version$name <- sprintf("v%d", length(rec$versions) + 1L)
    version$after_evaluation <- length(rec$evaluations) > 0L
    rec$versions <- c(rec$versions, list(version))
    sieve_holdout_update(state, rec)
    version
  })
}

#' Replace the record of a reservation in the state
#'
#' @param state The shared state store.
#' @param rec The record, with its `id`.
#' @return `rec`, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_holdout_update <- function(state, rec) {
  records <- shiny::isolate(state$holdout) %||% list()
  at <- which(vapply(records, function(r) identical(r$id, rec$id), logical(1)))
  if (length(at)) {
    records[[at[length(at)]]] <- rec
  } else {
    records <- c(records, list(rec))
  }
  state$holdout <- records
  invisible(rec)
}

#' The reversals made since the reserved rows left the analysis
#'
#' The same list `sieve_restore_cases()` builds: each reversal recorded
#' after a held row was removed, with the rows it concerns, so the held
#' values can take the change the rows in the analysis took.
#'
#' @param held The held records of the reservation.
#' @param records The transformation records (`state$transforms`).
#' @return A list of fixes, each with `item`, `rows`, `applied`, `lo`, `hi`.
#' @author Ken Kelley
#' @noRd
sieve_holdout_fixes <- function(held, records) {
  fixes <- list()
  for (k in seq_along(records)) {
    rec <- records[[k]]
    if (!identical(rec$type, "reverse")) next
    concern <- vapply(held, function(r) (r$transforms_at %||% 0L) < k, logical(1))
    if (any(concern)) {
      fixes[[length(fixes) + 1L]] <- list(
        item = rec$item, rows = vapply(held[concern], function(r) as.integer(r$row), integer(1)),
        applied = rec$applied, lo = rec$lo, hi = rec$hi
      )
    }
  }
  fixes
}

#' The R that gives the held rows the reversals made since the reservation
#'
#' Made to a copy (`held_items`), never to `removed_cases` itself, so a
#' later release in the script applies the same changes once, at the
#' restore.
#'
#' @param fixes From `sieve_holdout_fixes()`.
#' @return A character vector of R source lines.
#' @author Ken Kelley
#' @noRd
sieve_holdout_fix_code <- function(fixes) {
  unlist(lapply(fixes, function(f) {
    it <- encodeString(f$item, quote = "\"")
    value <- if (identical(f$applied, FALSE)) {
      sprintf("removed_cases$d[[%s]][fix]", it)
    } else if (identical(f$applied, TRUE)) {
      sprintf("(%s + %s) - as.numeric(removed_cases$d[[%s]][fix])", sieve_num_lit(f$lo), sieve_num_lit(f$hi), it)
    } else {
      sprintf("(%s + %s) - held_items[[%s]][fix]", sieve_num_lit(f$lo), sieve_num_lit(f$hi), it)
    }
    what <- if (identical(f$applied, FALSE)) {
      "returned to its values as read"
    } else {
      sprintf("reverse scored within %s to %s", sieve_num_text(f$lo), sieve_num_text(f$hi))
    }
    c(
      sprintf("# %s was %s after these rows were reserved; their held values take the same change.", f$item, what),
      sprintf("fix <- removed_cases$case_index %%in%% c(%s)", paste(sprintf("%dL", as.integer(f$rows)), collapse = ", ")),
      sprintf("if (%s %%in%% names(held_items)) held_items[[%s]][fix] <- %s", it, it, value)
    )
  }))
}

#' The calibration rows and the reserved rows, for one evaluation
#'
#' The item data in the analysis (the calibration rows) followed by the
#' reserved rows, each reserved row with the values held at the
#' reservation and every reversal recorded since made to them, exactly as
#' a restore would (`sieve_cases_held_value()`); an item that joined the
#' pool since takes its value as loaded. A held value a later reversal
#' cannot reflect within its bounds refuses the evaluation, as it refuses a
#' restore.
#'
#' @param state The shared state store (read under [shiny::isolate()]).
#' @return A list with `data` (the item data, calibration rows first),
#'   `validation_rows` (the positions of the reserved rows in `data`),
#'   `rows` (the original row numbers of the reserved rows), `fixes` (see
#'   `sieve_holdout_fixes()`), and `error` (`NULL`, or why no data could be
#'   assembled).
#' @author Ken Kelley
#' @noRd
sieve_holdout_data <- function(state) {
  shiny::isolate({
    rec <- sieve_holdout(state)
    if (is.null(rec)) {
      return(list(error = "No validation reservation is in force."))
    }
    held <- sieve_holdout_held(state, rec$id)
    items <- state$items
    raw <- state$raw
    if (!length(held) || is.null(items)) {
      return(list(error = sprintf("Reservation %s holds no reserved rows.", rec$id)))
    }
    records <- state$transforms %||% list()
    back <- NULL
    off_scale <- character(0)
    for (r in held) {
      r_raw <- r$raw[, names(raw), drop = FALSE]
      later <- if (length(records) > (r$transforms_at %||% 0L)) records[((r$transforms_at %||% 0L) + 1L):length(records)] else list()
      held_values <- lapply(names(items), function(it) sieve_cases_held_value(r, r_raw, it, later))
      cols <- lapply(held_values, `[[`, "value")
      off_scale <- c(off_scale, unlist(lapply(held_values, `[[`, "off"), use.names = FALSE))
      names(cols) <- names(items)
      r_items <- sieve_cases_bind(
        sieve_cases_take(items, integer(0)),
        structure(cols, class = "data.frame", row.names = attr(r$items, "row.names"))
      )
      back <- if (is.null(back)) r_items else sieve_cases_bind(back, r_items)
    }
    if (length(off_scale)) {
      return(list(error = paste0(
        "The reserved rows cannot be evaluated: ", paste(unique(off_scale), collapse = "; "),
        ". A value outside the response bounds has no reflection on the scale."
      )))
    }
    data <- sieve_cases_bind(items, back)
    list(
      data = data,
      validation_rows = nrow(items) + seq_len(nrow(back)),
      rows = vapply(held, function(r) as.integer(r$row), integer(1)),
      fixes = sieve_holdout_fixes(held, records),
      error = NULL
    )
  })
}

#' The R that evaluates a frozen version on the reserved rows in a replay
#'
#' @param rec The reservation record.
#' @param version The version evaluated.
#' @param rows The original rows of the reserved rows.
#' @param fixes From `sieve_holdout_fixes()`.
#' @param estimator,design,denominator The arguments of the evaluation.
#' @return A character vector of R source lines.
#' @author Ken Kelley
#' @noRd
sieve_holdout_evaluate_code <- function(rec, version, rows, fixes, estimator, design, denominator) {
  c(
    sprintf(
      "# Evaluation on the reserved rows of validation reservation %s: version %s (hash %s, frozen %s),",
      rec$id, version$name, substr(version$hash, 1L, 12L), version$time
    ),
    "# fit on the calibration rows and evaluated once on the reserved rows, which played no part",
    "# in fitting or selecting it (Browne, 2000).",
    "held_items <- removed_cases$items",
    sieve_holdout_fix_code(fixes),
    sprintf("reserved_at <- removed_cases$case_index %%in%% c(%s)", paste(sprintf("%dL", as.integer(rows)), collapse = ", ")),
    "holdout_data <- rbind(items, held_items[reserved_at, names(items), drop = FALSE])",
    sprintf("# The frozen model: %s.", version$model %||% "correlated-factors model, factor variances fixed at 1"),
    sprintf("holdout_model <- %s", sieve_r_literal(version$syntax %||% "")),
    sprintf(
      paste0(
        "holdout_evaluation <- SIEVER::sieve_cross_validate(holdout_data, meta, ",
        "validation_rows = nrow(items) + seq_len(sum(reserved_at)), estimator = %s, design = %s, denominator = %s, ",
        "syntax = if (nzchar(holdout_model)) holdout_model, std_lv = %s)"
      ),
      sieve_r_literal(estimator), paste(utils::capture.output(dput(design)), collapse = ""),
      sieve_r_literal(denominator), if (isFALSE(version$std_lv)) "FALSE" else "TRUE"
    )
  )
}

#' Evaluate the frozen version on the reserved rows
#'
#' Freezes the current version (`sieve_holdout_freeze()`), fits it on the
#' calibration rows and evaluates it on the reserved rows once, through
#' [sieve_cross_validate()] with the rows given rather than drawn. A
#' version evaluated before is not refit: its evaluation is returned with
#' `repeated = TRUE`. The evaluation records the version and its label:
#' "specified before evaluation" when the version predates the first
#' evaluation of this reservation, "revised during development" when a
#' development change created the version after an evaluation existed;
#' every earlier evaluation then becomes historical and cannot carry the
#' independent-validation label. The step is logged.
#'
#' @param state The shared state store.
#' @param estimator The estimator choice.
#' @param denominator The Reliability page's omega variant (`"observed"`
#'   or `"model_implied"`).
#' @param design The sampling design (`sieve_sampling_spec()`).
#' @return A list: the evaluation record (with `id`, `version`,
#'   `version_hash`, `label`, `time`, `rows`, `n_cal`, `n_val`, `result`,
#'   `historical`, and `repeated`), or a list with `error`.
#' @author Ken Kelley
#' @noRd
sieve_holdout_evaluate <- function(state, estimator = "auto", denominator = "observed",
                                   design = list(unit = "independent")) {
  shiny::isolate({
    rec <- sieve_holdout(state)
    if (is.null(rec)) {
      return(list(error = paste(
        "No validation reservation is in force. Reserve rows on the Screening page (Observations",
        "panel) before the structure and the items are examined; without a reservation the",
        "calibration/validation split above is an internal assessment."
      )))
    }
    version <- sieve_holdout_freeze(state, estimator)
    rec <- sieve_holdout(state)
    for (e in rec$evaluations) {
      if (identical(e$version_hash, version$hash)) {
        e$repeated <- TRUE
        return(e)
      }
    }
    assembled <- sieve_holdout_data(state)
    if (!is.null(assembled$error)) {
      return(list(error = assembled$error))
    }
    if (!denominator %in% c("observed", "model_implied")) denominator <- "observed"
    choice <- sieve_estimator_effective(estimator, state)
    # The frozen version is the model fit and evaluated: its syntax and
    # identification, not a model rebuilt from the ledger with the defaults.
    syntax <- if (nzchar(version$syntax %||% "")) version$syntax
    std_lv <- !isFALSE(version$std_lv)
    result <- sieve_cross_validate(assembled$data, state$meta,
      validation_rows = assembled$validation_rows, estimator = choice,
      design = design, denominator = denominator, syntax = syntax, std_lv = std_lv
    )
    label <- if (isTRUE(version$after_evaluation)) "revised during development" else "specified before evaluation"
    evaluation <- list(
      id = sprintf("E%d", length(rec$evaluations) + 1L),
      version = version$name, version_hash = version$hash, version_time = version$time,
      model = version$model %||% NA_character_,
      label = label, time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      rows = assembled$rows, n_cal = result$n_cal, n_val = result$n_val,
      result = result[setdiff(names(result), c("eligible_rows", "calibration_rows", "validation_rows", "rng"))],
      error = result$error,
      historical = FALSE, superseded_by = NULL, repeated = FALSE
    )
    if (is.null(result$error)) {
      rec$evaluations <- lapply(rec$evaluations, function(e) {
        e$historical <- TRUE
        e$superseded_by <- evaluation$id
        e
      })
      rec$evaluations <- c(rec$evaluations, list(evaluation))
      sieve_holdout_update(state, rec)
      sieve_log(
        state,
        sprintf(
          "CV and Invariance: evaluate version %s on the %d reserved rows of reservation %s (%s; calibration n = %d, reserved n = %d complete on the included items)",
          version$name, length(assembled$rows), rec$id, label, result$n_cal, result$n_val
        ),
        sieve_holdout_evaluate_code(rec, version, assembled$rows, assembled$fixes,
          sieve_estimator_record_choice(choice, state$meta), design, denominator)
      )
    }
    evaluation
  })
}

#' The evaluations of a reservation as a table
#'
#' @param rec A reservation record (or `NULL`).
#' @return A data frame with one row per evaluation: `Evaluation`,
#'   `Version`, `Model` (the frozen model in words), `Label`, `Time`,
#'   `Calibration n`, `Reserved n`, `Status`.
#' @author Ken Kelley
#' @noRd
sieve_holdout_evaluation_table <- function(rec) {
  evaluations <- rec$evaluations %||% list()
  if (!length(evaluations)) {
    return(data.frame(
      Evaluation = character(0), Version = character(0), Model = character(0), Label = character(0), Time = character(0),
      `Calibration n` = integer(0), `Reserved n` = integer(0), Status = character(0),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  pick <- function(field, as) vapply(evaluations, function(e) as(e[[field]] %||% NA), as(NA))
  model <- pick("model", as.character)
  model[is.na(model)] <- "not recorded"
  data.frame(
    Evaluation = pick("id", as.character), Version = pick("version", as.character),
    Model = model,
    Label = pick("label", as.character), Time = pick("time", as.character),
    `Calibration n` = pick("n_cal", as.integer), `Reserved n` = pick("n_val", as.integer),
    Status = ifelse(
      vapply(evaluations, function(e) isTRUE(e$historical), logical(1)),
      sprintf("historical (superseded by %s)", pick("superseded_by", as.character)),
      "current"
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

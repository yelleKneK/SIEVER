# utils_contract.R
# Small shared contracts that the analysis code is built on: capturing the
# conditions an engine raises instead of discarding them, a local random
# number stream with recorded provenance, and content-derived identities for
# the data and the item ledger. They are deliberately free of Shiny, so the
# same helpers serve the modules, the reports, and plain R use; the one
# exception, sieve_seed_input(), reads a seed box and refuses an invalid
# entry through shiny::validate().

#' Evaluate an expression and keep what it signaled
#'
#' Runs `expr`, collecting every warning and message it raises (in order,
#' without repeats) and converting an error into a recorded failure. Nothing
#' is discarded: rather than wrap an engine call in `suppressWarnings()`
#' and `tryCatch(..., error = function(e) NULL)`, a caller keeps the reason
#' and shows it.
#'
#' @param expr An expression to evaluate.
#' @return A list with `value` (the result, or `NULL` after an error),
#'   `error` (the error message, or `NULL`), `error_class` (the error's
#'   condition classes, so a caller can recognize a classed refusal whatever
#'   its words; `NULL` without an error), `warnings`, and `messages`
#'   (character vectors, possibly empty), and `ok` (`TRUE` when no error
#'   occurred).
#' @author Ken Kelley
#' @noRd
sieve_capture <- function(expr) {
  seen <- new.env(parent = emptyenv())
  seen$warnings <- character(0)
  seen$messages <- character(0)
  seen$error <- NULL
  seen$error_class <- NULL
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        seen$warnings <- c(seen$warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        seen$messages <- c(seen$messages, sub("\n$", "", conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) {
      seen$error <- conditionMessage(e)
      seen$error_class <- class(e)
      NULL
    }
  )
  list(
    value = value,
    error = seen$error,
    error_class = seen$error_class,
    warnings = unique(seen$warnings),
    messages = unique(seen$messages),
    ok = is.null(seen$error)
  )
}

#' A value as it was given, for a refusal
#'
#' The R that reproduces the value, shortened to `width` characters, so a
#' refusal can quote what it refused (`30`, `"0.3"`, `TRUE`) without
#' printing a long vector whole.
#'
#' @param x Any value.
#' @param width The longest text returned.
#' @return One string.
#' @author Ken Kelley
#' @noRd
sieve_given_text <- function(x, width = 60L) {
  text <- paste(deparse(x, width.cutoff = 500L), collapse = " ")
  if (nchar(text) > width) paste0(substr(text, 1L, width - 3L), "...") else text
}

#' The one whole number of rows a function was given
#'
#' Checked before any coercion, so 10.8 is refused rather than read as 10,
#' and text or a logical value is refused rather than converted.
#'
#' @param n The value given.
#' @param arg The argument's name, for the refusal.
#' @return `n` as an integer.
#' @author Ken Kelley
#' @noRd
sieve_row_count <- function(n, arg = "n") {
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 0 || n != trunc(n) ||
      n > .Machine$integer.max) {
    stop(errorCondition(sprintf(
      "`%s` must be one whole number of rows, 0 or more; %s was given.", arg, sieve_given_text(n)
    ), class = "sieve_invalid_rows", call = NULL))
  }
  as.integer(n)
}

#' Row numbers, checked before any coercion
#'
#' The one check every function that takes row numbers runs before it reads
#' them (`sieve_cross_validate()`'s `validation_rows`, `sieve_holdout_draw()`'s
#' `index`). Coercing first would make a logical mask a one-row sample (its
#' `TRUE` row 1), a factor of identifiers its level codes, and 1.9 row 1. Each kind of value that is not a row number is refused with its own
#' reason, naming the values at fault: a logical vector (pass `which()` of
#' it), a factor or text (an identifier is not a row number; pass
#' `match(ids, id_column)`), another non-numeric value, missing or infinite
#' values, fractional values, values below 1, values above `n` (row
#' positions are not respondent identifiers), and repeated values. The
#' refusal carries the class `sieve_invalid_rows`.
#'
#' @param x The values given. `NULL` is read as no rows.
#' @param n The number of rows the numbers refer to, or `NULL` when they have
#'   no upper bound but R's integer range (row identities rather than
#'   positions).
#' @param arg The argument's name, for the refusal.
#' @param of What the `n` rows are the rows of, for the refusal.
#' @return `x` as an integer vector, in the order given.
#' @author Ken Kelley
#' @noRd
sieve_row_positions <- function(x, n = NULL, arg = "x", of = "the data") {
  if (!is.null(n)) n <- sieve_row_count(n, "n")
  refuse <- function(text) stop(errorCondition(text, class = "sieve_invalid_rows", call = NULL))
  rule <- if (is.null(n)) {
    "whole numbers of 1 or more"
  } else {
    sprintf("whole numbers from 1 to %d, the rows of %s", n, of)
  }
  by_identifier <- paste(
    "to reserve rows by identifier, pass match(ids, id_column), the positions of those",
    "identifiers in the identifier column."
  )
  if (is.null(x)) {
    return(integer(0))
  }
  if (is.logical(x)) {
    refuse(sprintf(paste(
      "`%s` must be row numbers, %s; a logical vector of length %d was given.",
      "For the rows a logical vector marks, pass which() of it, for example which(mask)."
    ), arg, rule, length(x)))
  }
  if (is.factor(x) || is.character(x)) {
    refuse(sprintf(
      "`%s` must be row numbers, %s; %s was given. A factor or text identifier is not a row number: %s",
      arg, rule, if (is.factor(x)) "a factor" else "text", by_identifier
    ))
  }
  if (!is.numeric(x)) {
    refuse(sprintf("`%s` must be row numbers, %s; an object of class %s was given.", arg, rule, class(x)[1L]))
  }
  values <- as.vector(unclass(x))
  shown <- function(v) {
    text <- vapply(utils::head(v, 5L), function(z) format(z, digits = 15L), character(1))
    paste0(paste(text, collapse = ", "), if (length(v) > 5L) sprintf(", and %d more", length(v) - 5L) else "")
  }
  count <- function(k, one, many) sprintf("%d %s", k, if (k == 1L) one else many)
  bad <- which(!is.finite(values))
  if (length(bad)) {
    refuse(sprintf(
      "`%s` holds %s (at %s %s: %s); row numbers are %s.",
      arg, count(length(bad), "missing or infinite value", "missing or infinite values"),
      if (length(bad) == 1L) "element" else "elements", shown(bad), shown(values[bad]), rule
    ))
  }
  bad <- which(values != trunc(values))
  if (length(bad)) {
    refuse(sprintf(
      "`%s` holds %s (%s); row numbers are %s, and a fractional value is not truncated to one.",
      arg, count(length(bad), "value that is not a whole number", "values that are not whole numbers"),
      shown(values[bad]), rule
    ))
  }
  bad <- which(values < 1)
  if (length(bad)) {
    refuse(sprintf(
      "`%s` holds %s (%s); row numbers are %s.",
      arg, count(length(bad), "value below 1", "values below 1"), shown(values[bad]), rule
    ))
  }
  top <- if (is.null(n)) .Machine$integer.max else n
  bad <- which(values > top)
  if (length(bad)) {
    refuse(sprintf(
      "`%s` holds %s (%s). Row positions are not respondent identifiers: %s",
      arg,
      sprintf(
        "%s above %d, %s", count(length(bad), "value", "values"), top,
        if (is.null(n)) "the largest row number R represents" else sprintf("the number of rows of %s", of)
      ),
      shown(values[bad]), by_identifier
    ))
  }
  values <- as.integer(values)
  repeated <- unique(values[duplicated(values)])
  if (length(repeated)) {
    refuse(sprintf(
      "`%s` repeats row %s %s; each row may appear once.",
      arg, if (length(repeated) == 1L) "number" else "numbers", shown(repeated)
    ))
  }
  values
}

#' Check a seed supplied to a function
#'
#' The strict reading of a `seed` argument: `NULL`, or one whole number in
#' R's integer range. Checked before any coercion, so 1.9 is refused rather
#' than run (and recorded) as seed 1, and `TRUE`, text, `NA`, `Inf` and
#' 3e9 are refused rather than converted or dropped. Nothing here touches
#' the random number stream. The refusal carries the class
#' `sieve_seed_invalid`.
#'
#' @param seed The value given.
#' @param arg The argument's name, for the refusal.
#' @return `NULL`, or the seed as an integer.
#' @author Ken Kelley
#' @noRd
sieve_seed_value <- function(seed, arg = "seed") {
  if (is.null(seed)) {
    return(NULL)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) || seed != trunc(seed) ||
      abs(seed) > .Machine$integer.max) {
    stop(errorCondition(sprintf(
      "`%s` must be NULL or one whole number from -2147483647 to 2147483647; %s was given.",
      arg, sieve_given_text(seed)
    ), class = "sieve_seed_invalid", call = NULL))
  }
  as.integer(seed)
}

#' Read an optional seed from an input
#'
#' A cleared numeric input arrives as `NA` or `NULL` (a cleared text input
#' as `""`); each means that no seed was supplied, and `NULL` is returned.
#' Any other entry must be one whole number from -2147483647 to 2147483647.
#' An entry that is not (12.7, 3e9, text) is refused, not truncated or
#' read as "no seed": the refusal is raised through [shiny::validate()]
#' with the class `sieve_seed_invalid`, so a page shows the reason where
#' the result depending on the seed would be, an observer that reads the
#' input stops without acting rather than ending the session, a caller
#' that keeps its refusals (`tryCatch()`, `sieve_capture()`) shows the
#' message, and outside a session it is an ordinary error with the same
#' message.
#'
#' @param x A value from a seed input.
#' @return A single integer, or `NULL` when the box is empty.
#' @author Ken Kelley
#' @noRd
sieve_seed_input <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NULL)
  }
  if (length(x) == 1L && is.atomic(x) && (is.na(x) || (is.character(x) && !nzchar(trimws(x))))) {
    return(NULL)
  }
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x != trunc(x) ||
      abs(x) > .Machine$integer.max) {
    shiny::validate(shiny::need(FALSE, sprintf(paste(
      "The random seed must be a whole number from -2147483647 to 2147483647; %s was entered.",
      "Clear the box to have a seed drawn and recorded."
    ), sieve_given_text(x))), errorClass = "sieve_seed_invalid")
  }
  as.integer(x)
}

#' Resolve the seed a stochastic analysis will use
#'
#' A supplied seed is used as given. With `seed = NULL`, one integer is drawn
#' from the caller's random number stream and used as the seed: successive
#' unseeded runs therefore differ, as sampling variability requires, the
#' caller's stream advances by exactly that one draw, and the seed actually
#' used is known, so the run can still be replayed. The record is what a
#' result keeps as its random number provenance.
#'
#' Use this directly when the analysis is run by a function that takes its
#' own `seed` argument (pass it `rng$seed` and record the same call); use
#' `sieve_with_seed()` when the stochastic code is SIEVER's own.
#'
#' A supplied seed is checked by `sieve_seed_value()` before anything else:
#' one whole number from -2147483647 to 2147483647, or the call is refused
#' with the value named and the stream untouched (1.9 is refused, never run
#' and recorded as the supplied seed 1).
#'
#' @param seed `NULL`, or one whole number from -2147483647 to 2147483647.
#' @return A list with `seed` (the integer seed to use), `supplied` (whether
#'   the caller supplied it), and `kind` (the three [RNGkind()] settings in
#'   force).
#' @author Ken Kelley
#' @noRd
sieve_draw_seed <- function(seed = NULL) {
  supplied <- !is.null(seed)
  seed <- if (supplied) sieve_seed_value(seed) else sample.int(.Machine$integer.max, 1L)
  list(seed = seed, supplied = supplied, kind = RNGkind())
}

#' Run stochastic code on a local stream and record how it was seeded
#'
#' The one place SIEVER seeds a random number stream of its own. The seed is
#' resolved by `sieve_draw_seed()` and the code is evaluated on a local
#' stream with [withr::with_seed()], so the analysis can be replayed exactly
#' and the caller's stream is put back when the code finishes or fails:
#'
#' * with a supplied `seed`, the caller's stream is restored exactly as it
#'   was, including the case in which no stream existed yet (a fresh
#'   session has no `.Random.seed`, and none is left behind);
#' * with `seed = NULL`, the caller's stream is left one draw further on
#'   (the draw that chose the seed).
#'
#' @param seed `NULL`, or one whole number from -2147483647 to 2147483647
#'   (`sieve_draw_seed()` refuses any other value before the code runs).
#' @param code Code to evaluate on the local stream.
#' @return A list with `value` (the result of `code`) and `rng` (the record
#'   from `sieve_draw_seed()`).
#' @author Ken Kelley
#' @noRd
sieve_with_seed <- function(seed, code) {
  rng <- sieve_draw_seed(seed)
  value <- withr::with_seed(rng$seed, code)
  list(value = value, rng = rng)
}

#' The `set.seed()` line that replays a recorded run
#'
#' @param rng The `rng` element returned by `sieve_with_seed()`.
#' @param note Optional trailing comment.
#' @return A length one character string of R code.
#' @author Ken Kelley
#' @noRd
sieve_seed_code <- function(rng, note = NULL) {
  why <- if (isTRUE(rng$supplied)) {
    note
  } else {
    paste(c("no seed was supplied; SIEVER drew this one so the run can be replayed", note),
      collapse = "; "
    )
  }
  sprintf(
    "set.seed(%d)%s", rng$seed,
    if (length(why) && nzchar(why)) paste0("  # ", why) else ""
  )
}

#' A content hash of an R object
#'
#' The MD5 digest of the object's serialized form, with the serialization
#' header (which records the R version that wrote it) removed, so the same
#' values give the same key in different sessions. Used for identity, not
#' for security.
#'
#' @param x Any R object.
#' @return A length one character string of 32 hexadecimal digits.
#' @author Ken Kelley
#' @noRd
sieve_hash <- function(x) {
  bytes <- serialize(x, connection = NULL, ascii = FALSE, xdr = TRUE, version = 2L)
  bytes <- bytes[-seq_len(14L)]
  path <- tempfile("sieve_hash_")
  on.exit(unlink(path), add = TRUE)
  writeBin(bytes, path)
  unname(tools::md5sum(path))
}

#' A column read as numbers
#'
#' The one way SIEVER turns a data column into numbers: the stored codes,
#' whatever class the column carries. `unclass()` comes first because a
#' column read from SPSS or Stata by haven is `haven_labelled`, and once
#' haven is loaded (any session that has read a .sav or .dta file)
#' `as.numeric()` on such a column dispatches to vctrs and refuses. Text that
#' is not a number becomes `NA` without a warning; a factor gives its codes.
#'
#' @param v A column.
#' @return A double vector as long as `v`.
#' @author Ken Kelley
#' @noRd
sieve_as_number <- function(v) {
  x <- suppressWarnings(as.numeric(unclass(v)))
  values <- attr(v, "na_values", exact = TRUE)
  range <- attr(v, "na_range", exact = TRUE)
  recorded <- attr(v, "spss_missing", exact = TRUE)
  values <- c(values, recorded$values)
  if (is.null(range)) range <- recorded$range
  if (length(values)) x[x %in% values] <- NA_real_
  if (length(range) == 2L && all(is.finite(range))) {
    x[!is.na(x) & x >= range[1] & x <= range[2]] <- NA_real_
  }
  x
}

#' The values of a column, without its storage details
#'
#' Numbers and logical values become doubles, everything else text, and
#' attributes are dropped. Every missing number becomes the one `NA_real_`:
#' a missing value that arithmetic has passed through (the reflection of a
#' missing response) differs from a missing value as read in bits that are
#' not part of its value, and a content key has to treat the two alike, as
#' [identical()] does.
#'
#' @param v A column.
#' @return A double or character vector.
#' @author Ken Kelley
#' @noRd
sieve_plain_values <- function(v) {
  if (!is.factor(v) && (is.numeric(v) || is.logical(v))) {
    # unclass() first: with haven loaded (any session that read a .sav file),
    # as.double() on a labelled column dispatches to vctrs and refuses.
    x <- as.double(unclass(v))
    x[is.na(x)] <- NA_real_
    x
  } else {
    as.character(v)
  }
}

#' The content key of one column's values
#'
#' @param x A column.
#' @return A hash string.
#' @author Ken Kelley
#' @noRd
sieve_values_key <- function(x) {
  sieve_hash(sieve_plain_values(x))
}

#' The identity of a data frame's values
#'
#' Two data frames have the same key exactly when they have the same column
#' names in the same order and the same values. Storage details that do not
#' change a value (integer against double, factor against its labels, row
#' names, which missing value marker a missing number carries) do not
#' change the key, so data written to a file at full precision and read
#' back keep their identity.
#'
#' @param data A data frame, or `NULL`.
#' @return A hash string, or `NA_character_` for `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_data_key <- function(data) {
  if (is.null(data)) {
    return(NA_character_)
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  cols <- lapply(data, sieve_plain_values)
  sieve_hash(list(names = names(data), n = nrow(data), columns = unname(cols)))
}

#' The identity of the analysis-relevant part of the item ledger
#'
#' Covers what changes an analysis: which items exist, in what order, how
#' they are assigned (construct and facet), their measurement level, whether
#' they are included, and their keying. Display labels and the descriptive
#' summaries are not part of the key.
#'
#' @param meta The item ledger, or `NULL`.
#' @return A hash string, or `NA_character_` for `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_meta_key <- function(meta) {
  if (is.null(meta)) {
    return(NA_character_)
  }
  pick <- function(name, as) {
    if (name %in% names(meta)) as(meta[[name]]) else rep(as(NA), nrow(meta))
  }
  sieve_hash(list(
    item = pick("item", as.character),
    construct = pick("construct", as.character),
    facet = pick("facet", as.character),
    type = pick("type", as.character),
    status = pick("status", as.character),
    reverse = pick("reverse", as.logical)
  ))
}

#' The inputs an analysis or a cached result was computed from
#'
#' A snapshot of which dataset was loaded and the identities of the analytic
#' item data and of the ledger at this moment. A cache, an undo record, or a
#' candidate produced by a search keeps the revision it was built from, and
#' is applied only while `sieve_same_revision()` says the inputs are still
#' the ones it was built from.
#'
#' The respondents in the analysis are part of the revision: `rows_key`
#' is the content key of the original row number of every row (the rows of
#' the data as loaded when nobody is removed), so swapping which of two
#' respondents with identical item values is removed changes the revision,
#' and an analysis that reads a column other than the items (a criterion,
#' a group) is not shown as current for other respondents. A full restore
#' gives back the key the session started with.
#'
#' @param state The shared state store (read under [shiny::isolate()], so
#'   taking a snapshot never creates a reactive dependency).
#' @return A list with `dataset_id`, `data_key`, `meta_key`, and
#'   `rows_key`.
#' @author Ken Kelley
#' @noRd
sieve_revision <- function(state) {
  shiny::isolate(list(
    dataset_id = state$dataset_id %||% NA_character_,
    data_key = sieve_data_key(state$items),
    meta_key = sieve_meta_key(state$meta),
    rows_key = sieve_rows_key(state$case_index, NROW(state$items))
  ))
}

#' The content key of the respondents in the analysis
#'
#' @param index The original row number of every row (`state$case_index`),
#'   or `NULL` when nobody is removed.
#' @param n The number of rows of the item data.
#' @return A hash string, or `NA_character_` when there are no rows.
#' @author Ken Kelley
#' @noRd
sieve_rows_key <- function(index, n) {
  if (!n) {
    return(NA_character_)
  }
  rows <- if (is.null(index) || length(index) != n) seq_len(n) else index
  sieve_hash(as.integer(rows))
}

#' Do two revisions describe the same inputs?
#'
#' @param a,b Revisions from `sieve_revision()`.
#' @param what Which parts must agree: any of `"dataset"`, `"data"`, and
#'   `"meta"`. The data part covers the item values and, when both
#'   revisions record them (a saved revision may carry no `rows_key`), the
#'   respondents in the analysis.
#' @return `TRUE` or `FALSE`. A missing part never matches.
#' @author Ken Kelley
#' @noRd
sieve_same_revision <- function(a, b, what = c("dataset", "data", "meta")) {
  what <- match.arg(what, several.ok = TRUE)
  field <- c(dataset = "dataset_id", data = "data_key", meta = "meta_key")[what]
  same <- vapply(field, function(f) {
    x <- a[[f]]
    y <- b[[f]]
    length(x) == 1L && length(y) == 1L && !is.na(x) && !is.na(y) && identical(x, y)
  }, logical(1))
  if ("data" %in% what && !is.null(a[["rows_key"]]) && !is.null(b[["rows_key"]])) {
    same <- c(same, identical(a[["rows_key"]], b[["rows_key"]]))
  }
  all(same)
}

#' Give a newly loaded dataset its identity
#'
#' Called once whenever data are loaded or replaced. The identifier counts
#' loads within the session, so loading the same file twice still yields two
#' datasets: an undo record or a search candidate from the first load does
#' not apply to the second. The correlation cache is emptied, and the
#' respondents removed from the analysis of the previous data
#' (`state$removed_cases`, `state$case_index`) are cleared, and the
#' session's fit store (`state$fit_store`) is emptied; an opened
#' project puts its own removed respondents back after this call.
#'
#' @param state The shared state store.
#' @return The new dataset identifier, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_new_dataset <- function(state) {
  n <- shiny::isolate(state$dataset_seq %||% 0L) + 1L
  state$dataset_seq <- n
  state$dataset_id <- sprintf("D%d", n)
  state$scientific_dataset_id <- paste0("sample_", sieve_hash(list(
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6", tz = "UTC"),
    data = shiny::isolate(sieve_data_key(state$raw)), load = n
  )))
  # Nothing computed from the previous data applies to the new data, and no
  # respondent of the previous data is removed from the new one.
  state$cache <- list()
  state$case_index <- NULL
  state$removed_cases <- list()
  # No fit of the previous data stands for a fit of the new data: the
  # session's fit store is emptied in place (the same environment, so every
  # holder sees it empty) and bound to the new identity.
  store <- shiny::isolate(state$fit_store)
  if (is.environment(store)) {
    sieve_fit_store_clear(store, state$dataset_id)
  } else {
    state$fit_store <- sieve_fit_store(state$dataset_id)
  }
  invisible(state$dataset_id)
}

#' Replace dataset-bound state with fresh defaults before a new import
#' @param state Shared state.
#' @return New dataset identity invisibly.
#' @author Ken Kelley
#' @noRd
sieve_reset_dataset <- function(state) {
  shiny::isolate({
    keep <- names(Filter(function(field) !field$reset, sieve_state_fields()))
    held <- lapply(keep, function(name) state[[name]])
    names(held) <- keep
    fresh <- shiny::reactiveValuesToList(sieve_new_state())
    for (nm in union(names(shiny::reactiveValuesToList(state)), names(fresh))) state[[nm]] <- fresh[[nm]]
    for (name in keep) state[[name]] <- held[[name]] %||% fresh[[name]]
    sieve_new_dataset(state)
  })
}

#' Numeric values of an item column, without dispatching on its class
#'
#' An item read from an SPSS or Stata file is a haven_labelled column. Once
#' vctrs is loaded (ggplot2 and many other packages load it), `as.numeric()`
#' on such a column dispatches to the vctrs method, which fails unless haven
#' is loaded as well, because the cast method is haven's. The class is
#' dropped first here, so the values are read whatever is loaded. A factor
#' is read through its labels, so a factor of numerals keeps its numbers.
#'
#' @param x A column of the item data.
#' @return A double vector without attributes; `NA` where no number can be
#'   read.
#' @author Ken Kelley
#' @noRd
sieve_screen_numeric <- function(x) {
  if (is.factor(x)) {
    return(suppressWarnings(as.numeric(as.character(x))))
  }
  x <- unclass(x)
  if (is.numeric(x) || is.logical(x)) {
    return(as.vector(x, mode = "double"))
  }
  suppressWarnings(as.numeric(as.character(x)))
}

# fct_transform.R
# The one transformation service. Reverse scoring is the only operation that
# changes a value of the analytic item data, and every route to it (the
# item grid's Reverse? cell on the Ingest tab and its Reverse? row for
# several items at once, the specification import) goes through the
# functions in this file. (A role
# change adds or removes a whole column with its values as loaded, and
# opening a project replaces the session; both are in fct_project.R.) The
# rules they keep:
#
# * A reversal reflects an item within explicit response bounds,
#   x -> (lo + hi) - x. The bounds are the ones the researcher gave, or the
#   ones stored with the item, and only when there are neither are they read
#   from the observed range, which is said and stored, so that a second
#   application uses the same bounds.
# * The data and the ledger change together or not at all.
# * The data as loaded are never changed. When they are at hand, an item that
#   is returned to them is given those values themselves, not a second
#   reflection, so reversing twice restores the original values exactly
#   (floating point subtraction alone does not: about a quarter of
#   non-integer values come back one unit in the last place away).
# * Every applied operation is a record with the dataset it belongs to, and
#   an undo is applied only to the item state it was recorded from.
#
# The first half of the file is plain R with no Shiny in it; the second half
# applies those functions to the shared state.

#' A number as R source that reads back to the same value
#'
#' Recorded code has to carry a bound exactly. A decimal constant does that
#' only up to 15 significant digits: with more, R's parser (and
#' `as.numeric()`) can return a neighboring double, on some platforms for
#' about one value in seven. So the number is written as a decimal constant
#' when 15 digits identify it and the parser reads it back exactly (7, 0.1,
#' 2.25), and otherwise as a hexadecimal floating point constant, which is
#' exact by construction.
#'
#' @param x A numeric vector.
#' @return A character vector of R constants, one per element.
#' @author Ken Kelley
#' @noRd
sieve_num_lit <- function(x) {
  vapply(as.numeric(x), function(v) {
    if (is.na(v)) {
      return("NA_real_")
    }
    if (!is.finite(v)) {
      return(if (v > 0) "Inf" else "-Inf")
    }
    out <- if (v == trunc(v) && abs(v) < 2^53) sprintf("%.0f", v) else sprintf("%.15g", v)
    reads_back <- tryCatch(identical(eval(str2lang(out)), v), error = function(e) FALSE)
    if (reads_back) out else sprintf("%a", v)
  }, character(1))
}

#' A number as text for a note or a message
#'
#' The decimal digits of the number, as few as identify it (at most 17).
#' For reading by a person; recorded code uses `sieve_num_lit()`.
#'
#' @param x A numeric vector.
#' @return A character vector, one element per number.
#' @author Ken Kelley
#' @noRd
sieve_num_text <- function(x) {
  vapply(as.numeric(x), function(v) {
    if (!is.finite(v)) {
      return(as.character(v))
    }
    out <- ""
    for (digits in 15:17) {
      out <- sprintf("%.*g", digits, v)
      if (identical(suppressWarnings(as.numeric(out)), v)) {
        break
      }
    }
    out
  }, character(1))
}

#' The item data views a recorded reversal keeps in step
#'
#' The recorded steps read the item data under several names (the preamble
#' of the session script builds them from `items`). A reversal recorded
#' after they exist has to reach each of them, or a later step would read
#' the values from before the reversal.
#'
#' @return A character vector of object names.
#' @author Ken Kelley
#' @noRd
sieve_item_views <- function() {
  c("items_data", "active_items", "X", "items_df", "inv_data")
}

#' Give a ledger the columns that describe keying
#'
#' `reverse` is the current keying of the analytic values (`TRUE` when they
#' are reverse scored relative to the item as it was administered). `rev_lo`
#' and `rev_hi` are the response bounds a reversal of the item reflects
#' within, and `rev_bounds` says where they came from: `"declared"` (given by
#' the researcher) or `"observed"` (read from the observed range because
#' none was declared). A ledger without these columns (a saved one, or one
#' built by hand) gets them here with nothing declared.
#'
#' @param meta The item ledger.
#' @return The ledger with `reverse`, `rev_lo`, `rev_hi`, and `rev_bounds`.
#' @author Ken Kelley
#' @noRd
sieve_meta_keying <- function(meta) {
  n <- nrow(meta)
  if (!"reverse" %in% names(meta)) {
    meta$reverse <- rep(FALSE, n)
  }
  if (!"rev_lo" %in% names(meta)) {
    meta$rev_lo <- rep(NA_real_, n)
  }
  if (!"rev_hi" %in% names(meta)) {
    meta$rev_hi <- rep(NA_real_, n)
  }
  if (!"rev_bounds" %in% names(meta)) {
    meta$rev_bounds <- rep(NA_character_, n)
  }
  meta
}

#' The descriptive summaries the ledger keeps for one item
#'
#' The same expressions `sieve_build_meta()` evaluates, call for call, so a
#' ledger row refreshed after a transformation is identical to the row a
#' ledger built from the transformed data would hold (the two ways of
#' leaving out missing values in [stats::sd()] can differ in the last place).
#'
#' @param x The item's values.
#' @return A list with `mean`, `sd`, `min`, and `max` (full precision; `NA`
#'   where the summary is not defined).
#' @author Ken Kelley
#' @noRd
sieve_item_summary <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  none <- all(is.na(v))
  list(
    mean = if (none) NA_real_ else mean(v, na.rm = TRUE),
    sd = if (sum(!is.na(v)) < 2L) NA_real_ else stats::sd(v, na.rm = TRUE),
    min = if (none) NA_real_ else min(v, na.rm = TRUE),
    max = if (none) NA_real_ else max(v, na.rm = TRUE)
  )
}

#' Add or remove one annotation in a ledger note
#'
#' Notes are free text with annotations separated by semicolons. Only the
#' named annotation is touched; whatever else the note holds is kept.
#'
#' @param note A single note (possibly `NA`).
#' @param token The annotation.
#' @param present `TRUE` to make sure it is there, `FALSE` to remove it.
#' @return The edited note, `NA` when nothing is left.
#' @author Ken Kelley
#' @noRd
sieve_note_token <- function(note, token, present) {
  parts <- if (is.na(note) || !nzchar(note)) {
    character(0)
  } else {
    trimws(strsplit(note, ";", fixed = TRUE)[[1]])
  }
  parts <- parts[nzchar(parts) & parts != token]
  if (isTRUE(present)) {
    parts <- c(parts, token)
  }
  if (length(parts)) paste(parts, collapse = "; ") else NA_character_
}

#' Which values lie outside the response bounds
#'
#' A value counts as outside only when it is further from the bounds than
#' floating point subtraction can put a reflected value, so the reflection
#' of an in-range value is never reported.
#'
#' @param x Numeric values.
#' @param lo,hi The bounds.
#' @return The positions of the non-missing values outside `[lo, hi]`.
#' @author Ken Kelley
#' @noRd
sieve_outside_bounds <- function(x, lo, hi) {
  x <- as.numeric(x)
  slack <- 8 * .Machine$double.eps * max(abs(lo), abs(hi))
  which(!is.na(x) & (x < lo - slack | x > hi + slack))
}

#' Stop with a transformation error
#'
#' @param ... Passed to [sprintf()].
#' @return Does not return.
#' @author Ken Kelley
#' @noRd
sieve_transform_stop <- function(...) {
  stop(structure(
    list(message = sprintf(...), call = NULL),
    class = c("sieve_transform_error", "error", "condition")
  ))
}

#' Reverse score one item, in the data and the ledger together
#'
#' Reflects the item within explicit response bounds, `x -> (lo + hi) - x`,
#' and updates its ledger row in the same step: the keying (`reverse`), the
#' bounds used and where they came from (`rev_lo`, `rev_hi`, `rev_bounds`),
#' the descriptive summaries, and the "reverse coded" annotation. The
#' function works on copies and returns them; nothing is changed when it
#' stops with an error.
#'
#' Which bounds are used:
#'
#' * An item that is reverse scored with stored bounds (the ledger says so,
#'   or its values are the reflection of the values as loaded within those
#'   bounds) is reflected with those same bounds, whatever is passed in `lo`
#'   and `hi`, because only that reflection returns the earlier values.
#'   Bounds passed and not used are reported in the record (`unused`).
#' * An item recorded as reverse scored with no bounds stored, and none
#'   passed, is refused: the reflection that returns its administered
#'   values is not known (its observed range need not span the scale).
#' * Otherwise the bounds passed in `lo` and `hi` are used and become the
#'   item's declared bounds.
#' * Otherwise the bounds stored with the item are used.
#' * Otherwise, and only then, the observed minimum and maximum are used.
#'   The record and the ledger say so (`"observed"`), and the bounds are
#'   stored so that the next application uses the same ones.
#'
#' Missing values stay missing. A non-missing value outside the bounds has
#' no reflection on the response scale: the function stops, reports how many
#' there are and what they are, and changes nothing.
#'
#' When `loaded` holds the item's values as loaded, the result is always
#' either those values themselves or their single reflection, so applying
#' the reversal twice restores the original values exactly, storage type
#' included. Without `loaded` the second application is a second
#' subtraction, which is exact for integer responses and can differ in the
#' last place for other values.
#'
#' @param data The analytic item data (a data frame).
#' @param meta The item ledger.
#' @param item The name of the item to reverse.
#' @param lo,hi The response bounds, or `NULL` for the stored or observed
#'   bounds. Give both or neither.
#' @param loaded Optional: the item's values as loaded (one per row of
#'   `data`), the source of exact restoration.
#' @param held Optional: the item's values as loaded for the respondents
#'   removed from the analysis (see `sieve_removed_values()`). Bounds read
#'   from the observed range are read from the data as loaded, these
#'   respondents included, so a respondent restored later is reflected on
#'   the same scale.
#' @return A list with `data` and `meta` (the updated copies), `record` (a
#'   list of class `sieve_transform`: `type`, `item`, `lo`, `hi`, `bounds`
#'   (`"declared"` or `"observed"`), `from_ledger` (the bounds were the
#'   stored ones), `unused` (bounds passed and not used, or `NULL`), `from`
#'   and `to` (the keying before and after), `applied` (`TRUE` when the
#'   values are now the reflection of the values as loaded, `FALSE` when
#'   they are now the values as loaded, `NA` when that is not known),
#'   `basis` (`"loaded"` or `"current"`: which values the result was
#'   computed from), `n`, `n_missing`, `before_key`, and `after_key` (content
#'   hashes of the item's values)), `code` (the R that reproduces the
#'   operation on the analytic data in a replay), and `note` (the one line
#'   description for the session script).
#' @author Ken Kelley
#' @noRd
sieve_reverse_item <- function(data, meta, item, lo = NULL, hi = NULL, loaded = NULL, held = NULL) {
  if (!is.character(item) || length(item) != 1L || is.na(item) || !nzchar(item)) {
    sieve_transform_stop("One item name is needed to reverse score an item.")
  }
  if (!is.data.frame(data) || !item %in% names(data)) {
    sieve_transform_stop("Item \"%s\" is not in the item data, so nothing was changed.", item)
  }
  if (is.null(meta) || !"item" %in% names(meta) || !item %in% meta$item) {
    sieve_transform_stop("Item \"%s\" is not in the item ledger, so nothing was changed.", item)
  }
  meta <- sieve_meta_keying(meta)
  idx <- which(meta$item == item)
  if (length(idx) != 1L) {
    sieve_transform_stop(
      "Item \"%s\" is listed %d times in the item ledger, so nothing was changed.",
      item, length(idx)
    )
  }
  column <- data[[item]]
  if (!is.numeric(column)) {
    sieve_transform_stop(
      "Item \"%s\" is not numeric (it is stored as %s), so it cannot be reverse scored.",
      item, class(column)[1]
    )
  }
  current <- as.numeric(column)

  given <- !is.null(lo) || !is.null(hi)
  if (given) {
    ok <- function(b) is.numeric(b) && length(b) == 1L && is.finite(b)
    if (!ok(lo) || !ok(hi)) {
      sieve_transform_stop(
        "Give both response bounds for item \"%s\" (a minimum and a maximum), or neither.", item
      )
    }
    lo <- as.numeric(lo)
    hi <- as.numeric(hi)
    if (lo >= hi) {
      sieve_transform_stop(
        "The response bounds for item \"%s\" need a minimum below the maximum (%s is not below %s).",
        item, sieve_num_text(lo), sieve_num_text(hi)
      )
    }
  }

  origin <- NULL
  if (!is.null(loaded)) {
    if (length(loaded) != length(current)) {
      sieve_transform_stop(
        "The values as loaded for item \"%s\" have %d rows and the item data have %d.",
        item, length(loaded), length(current)
      )
    }
    if (is.numeric(loaded)) {
      origin <- as.numeric(loaded)
    }
  }
  have_loaded <- !is.null(origin)
  at_loaded <- have_loaded && identical(current, origin)

  stored_lo <- as.numeric(meta$rev_lo[idx])
  stored_hi <- as.numeric(meta$rev_hi[idx])
  has_stored <- is.finite(stored_lo) && is.finite(stored_hi) && stored_lo < stored_hi
  stored_label <- as.character(meta$rev_bounds[idx])
  if (is.na(stored_label) || !stored_label %in% c("declared", "observed")) {
    stored_label <- "declared"
  }
  was <- isTRUE(as.logical(meta$reverse[idx]))
  # The current values are the reflection of the values as loaded within the
  # stored bounds: a reversal is in force and can be taken back exactly.
  reflected <- has_stored && have_loaded && !at_loaded &&
    identical(current, (stored_lo + stored_hi) - origin)
  # Recorded as reverse scored with no bounds stored (it arrived reverse
  # scored: sieve_mark_reversed() with bounds unknown, from a specification,
  # the description rows, the Ingest answer or a variable label): the
  # reflection that returns the administered values is not known, and the
  # observed range of the reversed values need not span the scale.
  if (was && !has_stored && !given) {
    sieve_transform_stop(
      paste(
        "Item \"%s\" is recorded as reverse scored, but the response bounds of its reversal",
        "are not recorded, so the reflection that returns its administered values is not",
        "known and nothing was changed. Type the scale's bounds in its Response bounds cell",
        "on the Ingest tab, then set Reverse? to No."
      ),
      item
    )
  }

  unused <- NULL
  if (has_stored && (was || reflected)) {
    if (given && !(identical(lo, stored_lo) && identical(hi, stored_hi))) {
      unused <- c(lo = lo, hi = hi)
    }
    lo <- stored_lo
    hi <- stored_hi
    label <- stored_label
    from_ledger <- TRUE
  } else if (given) {
    label <- "declared"
    from_ledger <- FALSE
  } else if (has_stored) {
    lo <- stored_lo
    hi <- stored_hi
    label <- stored_label
    from_ledger <- TRUE
  } else {
    # The observed range of the data as loaded: the rows in the analysis
    # and the respondents removed from it, whose values return with them.
    held <- if (is.null(held)) numeric(0) else suppressWarnings(as.numeric(unlist(held, use.names = FALSE)))
    seen <- c(current, held)
    seen <- seen[is.finite(seen)]
    if (!length(seen)) {
      sieve_transform_stop(
        paste(
          "Item \"%s\" has no observed values, so its response bounds cannot be read from",
          "the data. Enter the bounds of the response scale to reverse score it."
        ),
        item
      )
    }
    lo <- min(seen)
    hi <- max(seen)
    if (lo >= hi) {
      sieve_transform_stop(
        paste(
          "Item \"%s\" takes a single value (%s), so its observed range does not define a",
          "reflection. Enter the bounds of the response scale to reverse score it."
        ),
        item, sieve_num_text(lo)
      )
    }
    label <- "observed"
    from_ledger <- FALSE
  }

  if (reflected) {
    # Back to the values as loaded: the column itself, so the original
    # values and their storage type return exactly.
    after <- loaded
    basis <- "loaded"
    applied <- FALSE
  } else {
    outside <- sieve_outside_bounds(current, lo, hi)
    if (length(outside)) {
      distinct <- sort(unique(current[outside]))
      shown <- utils::head(distinct, 5L)
      sieve_transform_stop(
        paste(
          "%d value%s of item \"%s\" %s outside the response bounds %s to %s (%s%s), so",
          "nothing was changed. A value outside the response scale has no reflection on it.",
          "If these are codes for missing responses, set them to missing in the data file",
          "and load it again; if the response scale is wider, enter its bounds."
        ),
        length(outside), if (length(outside) == 1L) "" else "s", item,
        if (length(outside) == 1L) "lies" else "lie",
        sieve_num_text(lo), sieve_num_text(hi),
        paste(sieve_num_text(shown), collapse = ", "),
        if (length(distinct) > length(shown)) ", ..." else ""
      )
    }
    after <- (lo + hi) - current
    basis <- if (at_loaded) "loaded" else "current"
    applied <- if (at_loaded) TRUE else NA
  }

  data[[item]] <- after
  meta$reverse[idx] <- !was
  meta$rev_lo[idx] <- lo
  meta$rev_hi[idx] <- hi
  meta$rev_bounds[idx] <- label
  now <- sieve_item_summary(after)
  for (field in intersect(names(now), names(meta))) {
    meta[[field]][idx] <- now[[field]]
  }
  if ("note" %in% names(meta)) {
    meta$note[idx] <- sieve_note_token(as.character(meta$note[idx]), "reverse coded", !was)
  }

  record <- structure(
    list(
      type = "reverse",
      item = item,
      lo = lo,
      hi = hi,
      bounds = label,
      from_ledger = from_ledger,
      unused = unused,
      from = was,
      to = !was,
      applied = applied,
      basis = basis,
      n = length(current),
      n_missing = sum(is.na(current)),
      before_key = sieve_values_key(current),
      after_key = sieve_values_key(after)
    ),
    class = "sieve_transform"
  )
  list(
    data = data,
    meta = meta,
    record = record,
    code = sieve_reverse_code(record),
    note = sieve_reverse_note(record)
  )
}

#' The R that reproduces a recorded reversal in a replay
#'
#' The step reflects the item in `items`, the analytic item data of the
#' session script, and in every other view of the item data that exists when
#' the step runs (see `sieve_item_views()`), so the steps after it read the
#' reversed values whichever name they use. The data as read (`d`) are left
#' as they are. The bounds are written so that they are read back exactly
#' (see `sieve_num_lit()`).
#'
#'
#' Two things keep a replay from going wrong without a word. The step first
#' checks that `items` is a data frame that holds the item, and stops with
#' the reason when it is not (an earlier step of the script may have replaced
#' `items`; indexing a list or a character vector by name would otherwise
#' run and leave nonsense). And a step that reflected the values as loaded
#' (`applied` is `TRUE`) first keeps them aside, in a list named
#' `values_as_read` that no other logged step uses, so that a step that
#' returns the item to them (`applied` is `FALSE`) restores that copy and
#' does not subtract a second time, which does not return non-integer values
#' exactly. The copy is not taken from the data as read (`d`), because a
#' later logged step (the calibration and validation split) binds that name
#' to the analytic item data, after which `d` would hold the reflected
#' values and the restoration would be wrong without a word.
#'
#' @param record A record from `sieve_reverse_item()`.
#' @return A character vector of R source lines.
#' @author Ken Kelley
#' @noRd
sieve_reverse_code_raw <- function(record) {
  it <- encodeString(record$item, quote = "\"")
  lo <- sieve_num_lit(record$lo)
  hi <- sieve_num_lit(record$hi)
  views <- paste(encodeString(sieve_item_views(), quote = "\""), collapse = ", ")
  said <- function(...) encodeString(sprintf(...), quote = "\"")
  guard <- c(
    sprintf("if (!exists(\"items\") || !is.data.frame(items) || !%s %%in%% names(items)) {", it),
    sprintf("  stop(%s, call. = FALSE)", said(
      paste(
        "This step changes %s in the item data, but `items` is not a data frame that holds it:",
        "an earlier step of this script replaced `items`, so the step was not run."
      ),
      record$item
    )),
    "}"
  )
  if (identical(record$applied, FALSE)) {
    return(c(
      guard,
      sprintf(paste0(
        "if (!exists(\"values_as_read\") || is.null(values_as_read[[%s]]) ||",
        " length(values_as_read[[%s]]) != nrow(items)) {"
      ), it, it),
      sprintf("  stop(%s, call. = FALSE)", said(
        paste(
          "This step returns %s to its values as read, which the step that reflected it kept",
          "aside as values_as_read[[\"%s\"]]; they are not there, so the step was not run."
        ),
        record$item, record$item
      )),
      "}",
      sprintf("items[[%s]] <- values_as_read[[%s]]", it, it),
      "# The other views of the item data take the same values: by row name when",
      "# the view is a subset of the rows of items, otherwise by a second",
      "# reflection within the same bounds.",
      sprintf("for (view in intersect(c(%s), ls())) {", views),
      sprintf("  if (%s %%in%% names(get(view))) {", it),
      "    held <- get(view)",
      sprintf("    held[[%s]] <- if (nrow(held) == nrow(items)) {", it),
      sprintf("      items[[%s]]", it),
      "    } else if (all(rownames(held) %in% rownames(items))) {",
      sprintf("      items[[%s]][match(rownames(held), rownames(items))]", it),
      "    } else {",
      sprintf("      (%s + %s) - held[[%s]]", lo, hi, it),
      "    }",
      "    assign(view, held)",
      "  }",
      "}"
    ))
  }
  keep <- if (identical(record$applied, TRUE)) {
    c(
      "# The values as read are kept aside before the reflection, so that a later",
      "# step that returns the item to them can restore them exactly (a second",
      "# subtraction does not return non-integer values exactly).",
      "if (!exists(\"values_as_read\")) values_as_read <- list()",
      sprintf("values_as_read[[%s]] <- items[[%s]]", it, it)
    )
  }
  c(
    guard,
    keep,
    sprintf("items[[%s]] <- (%s + %s) - items[[%s]]", it, lo, hi, it),
    "# The other views of the item data are kept in step, so every later step",
    "# reads the same values whichever name it uses.",
    sprintf("for (view in intersect(c(%s), ls())) {", views),
    sprintf("  if (%s %%in%% names(get(view))) {", it),
    "    held <- get(view)",
    sprintf("    held[[%s]] <- (%s + %s) - held[[%s]]", it, lo, hi, it),
    "    assign(view, held)",
    "  }",
    "}"
  )
}

#' Reversal source and its analysis-relevant keying change
#' @param record Recorded reversal.
#' @return Executable R source.
#' @author Ken Kelley
#' @noRd
sieve_reverse_code <- function(record) {
  it <- encodeString(record$item, quote = '"')
  c(
    sieve_reverse_code_raw(record),
    "if (exists('meta') && is.data.frame(meta)) {",
    sprintf("  meta$reverse[match(%s, meta$item)] <- %s", it, if (isTRUE(record$to)) "TRUE" else "FALSE"),
    "}"
  )
}

#' The one line description of a recorded reversal
#'
#' @param record A record from `sieve_reverse_item()`.
#' @return A length one character string for the session script.
#' @author Ken Kelley
#' @noRd
sieve_reverse_note <- function(record) {
  lo <- sieve_num_text(record$lo)
  hi <- sieve_num_text(record$hi)
  where <- if (identical(record$bounds, "observed")) {
    "the observed range, because no response bounds were declared"
  } else {
    "the declared response bounds"
  }
  id <- if (is.null(record$id)) "" else sprintf(" [%s]", record$id)
  if (identical(record$applied, FALSE)) {
    # The values are again the values as read: said as that, because the
    # recorded step takes them from the data as read and reflects nothing.
    if (isTRUE(record$to)) {
      sprintf(
        paste(
          "Reverse score %s: its values are again the values as read, which hold it",
          "reverse scored within %s to %s (%s)%s"
        ),
        record$item, lo, hi, where, id
      )
    } else {
      sprintf(
        paste(
          "Undo the reverse scoring of %s: its values are again the values as read",
          "(it had been reflected within %s to %s, %s)%s"
        ),
        record$item, lo, hi, where, id
      )
    }
  } else if (isTRUE(record$to)) {
    sprintf(
      "Reverse score %s within %s to %s (%s): x -> (%s + %s) - x%s",
      record$item, lo, hi, where, lo, hi, id
    )
  } else {
    sprintf(
      "Undo the reverse scoring of %s: reflect it again within the same bounds, %s to %s (%s)%s",
      record$item, lo, hi, where, id
    )
  }
}

#' What a reversal did, in a sentence for the interface
#'
#' @param record A record from `sieve_reverse_item()`.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_reverse_message <- function(record) {
  show <- function(v) format(v, digits = 7)
  lo <- show(record$lo)
  hi <- show(record$hi)
  out <- if (isTRUE(record$to)) {
    if (identical(record$bounds, "observed") && isTRUE(record$from_ledger)) {
      sprintf(
        paste(
          "%s is now reverse scored within %s to %s, the observed range stored with it when",
          "it was first reversed (no response bounds were declared for it)."
        ),
        record$item, lo, hi
      )
    } else if (identical(record$bounds, "observed")) {
      sprintf(
        paste(
          "%s is now reverse scored within its observed range, %s to %s. No response bounds",
          "were declared for it, so the observed minimum and maximum of the data as loaded",
          "(respondents removed from the analysis included) were used and are stored with the",
          "item. If the response scale is wider than what was observed, undo this (Reverse? No)",
          "and declare the scale bounds in the item's Response bounds cell on the Ingest tab."
        ),
        record$item, lo, hi
      )
    } else {
      sprintf("%s is now reverse scored within the response bounds %s to %s.", record$item, lo, hi)
    }
  } else if (identical(record$applied, FALSE)) {
    sprintf(
      "%s is no longer reverse scored: its values are again the values as loaded.",
      record$item
    )
  } else {
    sprintf(
      "%s is no longer reverse scored (reflected again within %s to %s).",
      record$item, lo, hi
    )
  }
  if (!is.null(record$unused)) {
    out <- paste(out, sprintf(
      "The bounds entered (%s to %s) were not used: an item is returned with the bounds it was reversed with.",
      show(record$unused[["lo"]]), show(record$unused[["hi"]])
    ))
  }
  out
}

# ---- The service applied to the shared state -------------------------------

#' Reverse score items in the shared state
#'
#' The single route by which the app changes the analytic item data. Each
#' item is reversed with `sieve_reverse_item()` on copies of the data and
#' the ledger; when the batch is done, the data and the ledger are written
#' to the state together, each applied operation is appended to
#' `state$transforms` with the dataset and revision it belongs to, and the R
#' that reproduces it is logged. An item that cannot be reversed changes
#' nothing and its reason is returned. The data as loaded (`state$raw`) are
#' read as the source of exact restoration and are never written.
#'
#' @param state The shared state store.
#' @param items Item names.
#' @param lo,hi Response bounds for every item in `items`, or `NULL`.
#' @param origin Where the request came from (`"ingest"` for the item
#'   grid's Reverse? cell and its Reverse? row, `"specification"`), kept in
#'   the record. A record in a saved study may name another origin
#'   (`"prepare"` or `"ai"`).
#' @param undo_of Optional character vector named by item: the identifier of
#'   the recorded transformation each operation undoes.
#' @return Invisibly, a list with `applied` (the records, named by item) and
#'   `refused` (the reasons, named by item).
#' @author Ken Kelley
#' @noRd
sieve_apply_reverse <- function(state, items, lo = NULL, hi = NULL,
                                origin = "ingest", undo_of = NULL) {
  shiny::isolate({
    replay_before <- sieve_replay_inputs(state)
    data <- state$items
    meta <- state$meta
    raw <- state$raw
    applied <- list()
    refused <- character(0)
    if (is.null(data) || is.null(meta)) {
      refused <- stats::setNames(rep("No data are loaded.", length(items)), items)
      return(invisible(list(applied = applied, refused = refused)))
    }
    batch <- sieve_reverse_batch(data, meta, raw, items,
      lo = lo, hi = hi,
      removed = state$removed_cases %||% list()
    )
    refused <- batch$refused
    steps <- batch$steps
    if (!length(steps)) {
      return(invisible(list(applied = applied, refused = refused)))
    }

    # Commit: the data and the ledger together, then the records and the log.
    sieve_ensure_dataset(state)
    before <- sieve_revision(state)
    state$items <- batch$data
    sieve_write_ledger(state, batch$meta)
    records <- sieve_record_steps(state, steps,
      origin = origin, undo_of = undo_of,
      before = before, after = sieve_revision(state), replay_before = replay_before
    )
    for (record in records) {
      applied[[record$item]] <- record
    }
    invisible(list(applied = applied, refused = refused))
  })
}

#' Reverse score a batch of items on copies
#'
#' The plain part of `sieve_apply_reverse()`: each item is reversed with
#' `sieve_reverse_item()` in turn, on copies that are returned. Nothing in
#' the shared state is read or written, so a caller that changes more than
#' the keying (the specification import) can try the whole change first and
#' commit it only when every part of it can be made.
#'
#' @param data The analytic item data.
#' @param meta The item ledger.
#' @param raw The data as loaded (the source of exact restoration), or
#'   `NULL`.
#' @param items Item names.
#' @param lo,hi Response bounds for every item in `items`, or `NULL`.
#' @param removed The respondents removed from the analysis
#'   (`state$removed_cases`), whose values as loaded the observed range of
#'   an item covers.
#' @return A list with `data` and `meta` (the updated copies), `steps` (the
#'   results of `sieve_reverse_item()` that were applied, named by item), and
#'   `refused` (the reasons the others were not, named by item).
#' @author Ken Kelley
#' @noRd
sieve_reverse_batch <- function(data, meta, raw, items, lo = NULL, hi = NULL, removed = list()) {
  steps <- list()
  refused <- character(0)
  for (it in unique(as.character(items))) {
    loaded <- if (!is.null(raw) && it %in% names(raw) && NROW(raw) == NROW(data)) raw[[it]] else NULL
    step <- tryCatch(
      sieve_reverse_item(data, meta, it,
        lo = lo, hi = hi, loaded = loaded,
        held = sieve_removed_values(removed, it)
      ),
      sieve_transform_error = function(e) e
    )
    if (inherits(step, "sieve_transform_error")) {
      refused[it] <- conditionMessage(step)
      next
    }
    data <- step$data
    meta <- step$meta
    steps[[it]] <- step
  }
  list(data = data, meta = meta, steps = steps, refused = refused)
}

#' Record applied transformation steps and log their R
#'
#' Called once the data and the ledger the steps produced have been written
#' to the state. Each step's record is given its identifier, its origin, the
#' dataset it belongs to and the revisions before and after the commit, is
#' appended to `state$transforms`, and its R is logged.
#'
#' @param state The shared state store.
#' @param steps A list of steps in the order they were applied. A step has a
#'   `record` (class `sieve_transform`) and the `code` that reproduces it.
#' @param origin Where the request came from, kept in the records.
#' @param undo_of Optional character vector named by item: the identifier of
#'   the recorded transformation each step undoes.
#' @param before,after The revisions before and after the commit.
#' @return The completed records, in order.
#' @author Ken Kelley
#' @noRd
sieve_record_steps <- function(state, steps, origin, undo_of = NULL, before, after, replay_before = NULL) {
  shiny::isolate({
    dataset_id <- sieve_ensure_dataset(state)
    existing <- state$transforms %||% list()
    records <- list()
    for (step in steps) {
      record <- step$record
      it <- record$item
      record$id <- sprintf("T%d", length(existing) + 1L)
      record$origin <- origin
      record$undo_of <- if (!is.null(undo_of) && it %in% names(undo_of)) undo_of[[it]] else NULL
      record$dataset_id <- dataset_id
      record$revision_before <- before
      record$revision <- after
      record <- sieve_audit_stamp(record, state)
      existing[[record$id]] <- record
      records[[length(records) + 1L]] <- record
      note <- if (identical(record$type, "keying")) sieve_keying_note(record) else sieve_reverse_note(record)
      expected <- sieve_replay_inputs(state)
      expected$items <- step$data
      expected$meta <- step$meta
      prior <- replay_before %||% expected
      if (is.null(replay_before) && identical(record$type, "reverse")) {
        # Without `replay_before` (a batch committed before its steps are
        # recorded), the inputs before a reversal are rebuilt from its
        # record, which states exactly whether its input was the loaded
        # column or its single reflection.
        prior$items <- step$data
        prior$items[[it]] <- if (identical(record$applied, TRUE)) {
          state$raw[[it]]
        } else if (identical(record$applied, FALSE)) {
          (record$lo + record$hi) - as.numeric(state$raw[[it]])
        } else {
          (record$lo + record$hi) - step$data[[it]]
        }
      }
      sieve_log(state, note, step$code,
        inputs = expected,
        before = prior, kind = "mutation"
      )
      replay_before <- expected
    }
    state$transforms <- existing
    records
  })
}

#' Record that an item's values are already reverse scored, on copies
#'
#' For data that arrive with an item already reverse scored (an analytic
#' export read back in). No value is changed: the ledger is told the keying
#' the values have and the response bounds the reversal was made within, so
#' that taking the reversal back reflects within those bounds. The record
#' has the type `"keying"`, which sets it apart from a reversal that changed
#' values.
#'
#' @param data The analytic item data.
#' @param meta The item ledger.
#' @param item The item.
#' @param lo,hi The response bounds the values were reversed within, or
#'   `NA` when they are not known.
#' @param label Where the bounds came from (`"declared"` or `"observed"`).
#' @return A list with `meta` (the updated copy), `record`, `code` (a
#'   comment: nothing is computed in a replay), and `note`.
#' @author Ken Kelley
#' @noRd
sieve_mark_reversed <- function(data, meta, item, lo = NA_real_, hi = NA_real_, label = "declared") {
  meta <- sieve_meta_keying(meta)
  idx <- which(meta$item == item)
  if (length(idx) != 1L || !item %in% names(data)) {
    sieve_transform_stop("Item \"%s\" is not in the item data, so nothing was changed.", item)
  }
  known <- is.finite(lo) && is.finite(hi) && lo < hi
  if (known) {
    outside <- sieve_outside_bounds(data[[item]], lo, hi)
    if (length(outside)) {
      sieve_transform_stop(
        paste(
          "%d value%s of item \"%s\" %s outside the response bounds %s to %s the specification",
          "gives for it, so nothing was changed."
        ),
        length(outside), if (length(outside) == 1L) "" else "s", item,
        if (length(outside) == 1L) "lies" else "lie", sieve_num_text(lo), sieve_num_text(hi)
      )
    }
  }
  was <- isTRUE(as.logical(meta$reverse[idx]))
  meta$reverse[idx] <- TRUE
  meta$rev_lo[idx] <- if (known) as.numeric(lo) else NA_real_
  meta$rev_hi[idx] <- if (known) as.numeric(hi) else NA_real_
  meta$rev_bounds[idx] <- if (known) label else NA_character_
  if ("note" %in% names(meta)) {
    meta$note[idx] <- sieve_note_token(as.character(meta$note[idx]), "reverse coded", TRUE)
  }
  key <- sieve_values_key(data[[item]])
  record <- structure(
    list(
      type = "keying",
      item = item,
      lo = if (known) as.numeric(lo) else NA_real_,
      hi = if (known) as.numeric(hi) else NA_real_,
      bounds = if (known) label else NA_character_,
      from_ledger = FALSE,
      unused = NULL,
      from = was,
      to = TRUE,
      applied = NA,
      basis = "loaded",
      n = NROW(data),
      n_missing = sum(is.na(data[[item]])),
      before_key = key,
      after_key = key
    ),
    class = "sieve_transform"
  )
  list(
    meta = meta,
    record = record,
    code = sprintf(
      "# %s holds reverse scored values in the data as read; no value is changed here.",
      encodeString(item, quote = "\"")
    ),
    note = sieve_keying_note(record)
  )
}

#' The one line description of a recorded keying
#'
#' @param record A record from `sieve_mark_reversed()`.
#' @return A length one character string for the session script.
#' @author Ken Kelley
#' @noRd
sieve_keying_note <- function(record) {
  id <- if (is.null(record$id)) "" else sprintf(" [%s]", record$id)
  within <- if (is.finite(record$lo) && is.finite(record$hi)) {
    sprintf(" (within %s to %s)", sieve_num_text(record$lo), sieve_num_text(record$hi))
  } else {
    " (the response bounds are not recorded)"
  }
  sprintf(
    "Record that %s is already reverse scored in the data as read%s: no value is changed%s",
    record$item, within, id
  )
}
#' Declare the response bounds of items
#'
#' The declaration an item's Response bounds cell makes in the item grid,
#' and the one the Response bounds row above it makes for a set of items:
#' the bounds
#' are written to the ledger (`rev_lo`, `rev_hi`,
#' `rev_bounds = "declared"`) for every item in scope whose values lie
#' within them and that does not hold the bounds of a reversal in force
#' (`sieve_holds_reversal_bounds()`: a reflection made in this session, or
#' a reversal as loaded whose bounds its keying record states, keeps them;
#' set its Reverse? to No first). An item reverse scored as loaded whose
#' record gave no bounds takes the declaration as the bounds of that
#' reversal, so that returning it to its scoring reflects within the scale,
#' and a wrong declaration can be corrected the same way.
#' The ledger write is recorded
#' in the session script, the project saves the ledger, and the
#' specification export carries the bounds. No value changes (declaring
#' reverses nothing), so the confirmation of the item types stands.
#'
#' @param state The shared state store.
#' @param lo,hi The lowest and the highest response of the scale.
#' @param items The items to declare the bounds for, or `NULL` for every
#'   item of the ledger.
#' @return Invisibly, a list with `declared` (the items the bounds were
#'   written for), `reversed` (the items left alone because they hold the
#'   bounds of a reversal), `outside` (the items left alone because a value lies
#'   outside the bounds), and `text` (one or two sentences).
#' @author Ken Kelley
#' @noRd
sieve_declare_bounds <- function(state, lo, hi, items = NULL) {
  shiny::isolate({
    meta <- sieve_meta_keying(state$meta)
    data <- as.data.frame(state$items)
    lo <- as.numeric(lo)
    hi <- as.numeric(hi)
    scope <- as.character(meta$item)
    if (!is.null(items)) {
      scope <- scope[scope %in% as.character(items)]
    }
    reversed <- scope[sieve_holds_reversal_bounds(meta, scope, data, state$raw, state$transforms %||% list(), state$dataset_id)]
    outside <- vapply(scope, function(it) {
      it %in% names(data) && length(sieve_outside_bounds(data[[it]], lo, hi)) > 0L
    }, logical(1))
    outside <- scope[outside]
    declared <- setdiff(scope, c(reversed, outside))
    if (length(declared)) {
      at <- match(declared, meta$item)
      meta$rev_lo[at] <- lo
      meta$rev_hi[at] <- hi
      meta$rev_bounds[at] <- "declared"
      sieve_write_ledger(state, meta)
      quoted <- paste(encodeString(declared, quote = "\""), collapse = ", ")
      keyed <- declared[meta$reverse[at] %in% TRUE]
      sieve_log(
        state,
        sprintf(
          "Declare the response bounds %s to %s for %d item%s", sieve_num_text(lo), sieve_num_text(hi),
          length(declared), if (length(declared) == 1L) "" else "s"
        ),
        c(
          "# The bounds a reversal reflects within, declared for the study rather than",
          "# read from each item's observed range; no value changes here.",
          if (length(keyed)) {
            one <- length(keyed) == 1L
            c(
              sprintf("# For %s, reverse scored as loaded, these are the bounds of", sieve_name_some(keyed)),
              sprintf(
                "# %s: returning %s to %s scoring reflects within them.",
                if (one) "its reversal" else "their reversals", if (one) "it" else "them", if (one) "its" else "their"
              )
            )
          },
          sprintf("declared_items <- c(%s)", quoted),
          sprintf("meta$rev_lo[meta$item %%in%% declared_items] <- %s", sieve_num_text(lo)),
          sprintf("meta$rev_hi[meta$item %%in%% declared_items] <- %s", sieve_num_text(hi)),
          "meta$rev_bounds[meta$item %in% declared_items] <- \"declared\""
        )
      )
    }
    text <- paste(c(
      if (length(declared)) {
        sprintf(
          "Response bounds %s to %s declared for %d item%s; a reversal now reflects within them.",
          sieve_num_text(lo), sieve_num_text(hi), length(declared), if (length(declared) == 1L) "" else "s"
        )
      } else {
        "No bounds were declared."
      },
      if (length(declared) && any(meta$reverse[match(declared, meta$item)] %in% TRUE)) {
        keyed <- declared[meta$reverse[match(declared, meta$item)] %in% TRUE]
        one <- length(keyed) == 1L
        sprintf(
          "%s %s reverse scored as loaded, so these are now the bounds of %s: Reverse? No reflects within them.",
          sieve_name_some(keyed), if (one) "is" else "are", if (one) "that reversal" else "their reversals"
        )
      },
      if (length(reversed)) {
        one <- length(reversed) == 1L
        sprintf(
          "%s %s reverse scored already and keep%s the bounds of %s: set Reverse? to No first to declare new bounds for %s.",
          sieve_name_some(reversed), if (one) "is" else "are",
          if (one) "s" else "", if (one) "that reversal" else "their reversals", if (one) "it" else "them"
        )
      },
      if (length(outside)) {
        sprintf(
          "%s %s a value outside %s to %s, so no bounds were declared for %s.",
          sieve_name_some(outside), if (length(outside) == 1L) "has" else "have",
          sieve_num_text(lo), sieve_num_text(hi), if (length(outside) == 1L) "it" else "them"
        )
      }
    ), collapse = " ")
    invisible(list(declared = declared, reversed = reversed, outside = outside, text = text))
  })
}

#' Take back the declared response bounds of items
#'
#' Clearing an item's Response bounds cell (or the Response bounds row's
#' Clear)
#' removes its declaration: the ledger's `rev_lo`, `rev_hi` and `rev_bounds`
#' return to empty, so a reversal reads the observed range again unless
#' bounds are declared anew (an item reverse scored as loaded, whose bounds
#' are cleared whatever their kind, is not returned to its scoring until
#' they are). An item that holds the bounds of a reversal keeps them (set
#' its Reverse? to No first), and an item with no declaration is left as
#' it is. A declaration the load made from the
#' file's value labels is cleared for the session and the specification
#' export; loading the file again declares it again. No value changes, so the confirmation of
#' the item types stands. The write is recorded in the session script.
#'
#' @param state The shared state store.
#' @param items The items to clear.
#' @return Invisibly, a list with `cleared`, `reversed` (left alone),
#'   `keyed` (the cleared items reverse scored as loaded) and `text` (one or
#'   two sentences).
#' @author Ken Kelley
#' @noRd
sieve_clear_bounds <- function(state, items) {
  shiny::isolate({
    meta <- sieve_meta_keying(state$meta)
    scope <- intersect(as.character(items), as.character(meta$item))
    at <- match(scope, meta$item)
    holds <- sieve_holds_reversal_bounds(meta, scope, state$items, state$raw, state$transforms %||% list(), state$dataset_id)
    reversed <- scope[holds]
    stored <- is.finite(suppressWarnings(as.numeric(meta$rev_lo[at]))) &
      is.finite(suppressWarnings(as.numeric(meta$rev_hi[at])))
    # A reversal as loaded that does not hold its bounds has them cleared
    # whatever their kind, as its cell offers; it is then returned to its
    # scoring only within bounds declared anew (sieve_reverse_item()).
    as_loaded <- !holds & meta$reverse[at] %in% TRUE & stored
    cleared <- scope[!holds & (as.character(meta$rev_bounds[at]) %in% "declared" | as_loaded)]
    keyed <- intersect(cleared, scope[as_loaded])
    plain <- setdiff(cleared, keyed)
    if (length(cleared)) {
      rows <- match(cleared, meta$item)
      meta$rev_lo[rows] <- NA_real_
      meta$rev_hi[rows] <- NA_real_
      meta$rev_bounds[rows] <- NA_character_
      sieve_write_ledger(state, meta)
      quoted <- paste(encodeString(cleared, quote = "\""), collapse = ", ")
      sieve_log(
        state,
        sprintf("Clear the declared response bounds of %d item%s", length(cleared), if (length(cleared) == 1L) "" else "s"),
        c(
          if (length(plain)) "# A reversal reads the observed range again unless bounds are declared anew;",
          if (length(keyed)) "# An item reverse scored as loaded is returned to its scoring only within bounds declared anew;",
          "# no value changes here.",
          sprintf("cleared_items <- c(%s)", quoted),
          "meta$rev_lo[meta$item %in% cleared_items] <- NA_real_",
          "meta$rev_hi[meta$item %in% cleared_items] <- NA_real_",
          "meta$rev_bounds[meta$item %in% cleared_items] <- NA_character_"
        )
      )
    }
    # The load declares the bounds the file's value labels give when every
    # value as loaded lies within them, so a declaration cleared here returns
    # when the file is loaded again. state$raw holds only the rows left in
    # the analysis, so the values of the respondents removed since are
    # checked as well: one outside the labels means a reload declares none.
    labelled <- if (length(cleared)) {
      lb <- sieve_label_bounds_of(state$raw, cleared)
      cleared[vapply(cleared, function(it) {
        b <- lb[[it]]
        held <- sieve_removed_values(state$removed_cases, it)
        held <- held[is.finite(held)]
        length(b) == 2L && all(is.finite(b)) && all(held >= b[1] & held <= b[2])
      }, logical(1))]
    } else {
      character(0)
    }
    text <- paste(c(
      if (length(plain)) {
        sprintf(
          "The declared response bounds of %s are cleared; a reversal reads the observed range again.",
          sieve_name_some(plain)
        )
      },
      if (length(keyed)) {
        one <- length(keyed) == 1L
        sprintf(
          "The response bounds of %s are cleared; %s reverse scored as loaded, so Reverse? No is refused until the scale's bounds are typed in %s Response bounds cell.",
          sieve_name_some(keyed), if (one) "it is" else "they are", if (one) "its" else "each item's"
        )
      },
      if (!length(cleared)) "No declared bounds were cleared.",
      if (length(labelled)) {
        sprintf(
          "The file's value labels give the bounds of %s, so loading the file again declares them again.",
          sieve_name_some(labelled)
        )
      },
      if (length(reversed)) {
        one <- length(reversed) == 1L
        sprintf(
          "%s %s reverse scored and keep%s the bounds of %s: set Reverse? to No first.",
          sieve_name_some(reversed), if (one) "is" else "are",
          if (one) "s" else "", if (one) "that reversal" else "their reversals"
        )
      }
    ), collapse = " ")
    invisible(list(cleared = cleared, reversed = reversed, keyed = keyed, text = text))
  })
}

#' Whether items hold the bounds of a reversal in force
#'
#' The bounds of a reversal stay fixed while they are what returns the item
#' exactly to its administered values: an item whose values are a
#' reflection made in this session within its stored bounds, and an item
#' reverse scored as loaded whose stored bounds are the ones its keying
#' record states (the file's variable label, a specification, or the
#' bounds stored when the record was made). They are not declared over or
#' cleared. Bounds stored later for a reversal as loaded whose record gave
#' none (typed in its Response bounds cell after the item arrived reverse
#' scored with no bounds) are the researcher's statement about the scale
#' and can be corrected or cleared; Reverse? No reflects within them.
#'
#' @param meta The item ledger (with the keying columns).
#' @param items The items.
#' @param data,raw The item data and the data as loaded. When either is
#'   `NULL`, or the values as loaded cannot be compared (see
#'   `sieve_loaded_keying()`), a reverse scored item with stored bounds is
#'   taken to hold them.
#' @param transforms The session's transformation records
#'   (`state$transforms`), whose keying records say which bounds a reversal
#'   as loaded was recorded with; `NULL` takes every reversal as loaded with
#'   stored bounds to hold them.
#' @param dataset_id The dataset the records must belong to
#'   (`state$dataset_id`), or `NULL` for any.
#' @return A logical vector, one per item.
#' @author Ken Kelley
#' @noRd
sieve_holds_reversal_bounds <- function(meta, items, data = NULL, raw = NULL, transforms = NULL, dataset_id = NULL) {
  items <- as.character(items)
  at <- match(items, as.character(meta$item))
  lo <- suppressWarnings(as.numeric(meta$rev_lo[at]))
  hi <- suppressWarnings(as.numeric(meta$rev_hi[at]))
  held <- meta$reverse[at] %in% TRUE & is.finite(lo) & is.finite(hi)
  if (any(held) && !is.null(data) && !is.null(raw) && !is.null(transforms)) {
    keying <- sieve_loaded_keying(raw, data, meta[at[held], , drop = FALSE])
    as_loaded <- rep(FALSE, length(items))
    as_loaded[held] <- keying$at_loaded[match(items[held], keying$item)] %in% TRUE
    # The bounds the latest keying record of the item states, if any.
    stated <- vapply(seq_along(items), function(k) {
      if (!as_loaded[k]) {
        return(FALSE)
      }
      recs <- Filter(function(r) {
        identical(r$type, "keying") && identical(r$item, items[k]) &&
          (is.null(dataset_id) || identical(r$dataset_id, dataset_id))
      }, transforms)
      if (!length(recs)) {
        return(TRUE) # no record to say otherwise: the bounds are held
      }
      r <- recs[[length(recs)]]
      r_lo <- suppressWarnings(as.numeric(r$lo))
      r_hi <- suppressWarnings(as.numeric(r$hi))
      length(r_lo) == 1L && length(r_hi) == 1L && is.finite(r_lo) && is.finite(r_hi) &&
        identical(r_lo, lo[k]) && identical(r_hi, hi[k])
    }, logical(1))
    held <- held & (!as_loaded | stated)
  }
  held
}

#' The bounds the file's value labels give for each item
#'
#' @param raw The data as loaded (`state$raw`), whose columns keep the
#'   value labels haven read from an SPSS or Stata file.
#' @param items Item names.
#' @return A list named by item, each `c(lo, hi)` or `c(NA, NA)`.
#' @author Ken Kelley
#' @noRd
sieve_label_bounds_of <- function(raw, items) {
  items <- as.character(items)
  out <- lapply(items, function(it) {
    if (is.data.frame(raw) && it %in% names(raw)) sieve_label_bounds(raw[[it]]) else c(NA_real_, NA_real_)
  })
  stats::setNames(out, items)
}

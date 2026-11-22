# fct_export.R
# The export contract: what the data file and the specification that are
# downloaded from the Ingest tab hold, and how a specification is read back.
#
# The data file is written in one of two kinds, and says which:
#
# * "raw" (the default): every value exactly as the data file was read. No
#   reverse scoring made in SIEVER is applied.
# * "processed": the values the analyses use. An item that is reverse
#   scored in the session is written reverse scored. (A file whose kind is
#   "analytic" is read as processed.)
#
# A third export, the summary, holds the covariance matrix of the included
# items' processed values, their means when asked for, and the number of
# complete cases, for an analysis that starts from moments.
#
# The specification has one row per column of the data. For every item it
# records the included or excluded status, the measurement type, the
# construct and the facet, the keying direction (`reverse`), the response
# bounds the reversal is made within (`rev_lo`, `rev_hi`, written so that
# they are read back exactly) and where they came from, and whether the
# data file it was written with already holds the item reverse scored
# (`reverse_applied`). It also carries a content key of each item's values
# in both kinds of file, so that an import can tell which values are in
# front of it instead of assuming.
#
# The rule an import keeps: a reversal is applied only to values that are
# known not to hold it yet, and an item is recorded as reverse scored
# without any change when its values already hold the reversal. When the
# import cannot tell which is the case, it stops and asks; it never guesses.
# The whole import is tried on copies first and is committed only when
# every part of it can be made, so a specification that is refused leaves
# the project exactly as it was.

#' The specification schema version SIEVER writes
#'
#' Schema 1 files (item, label, wording, type, construct, facet, reverse) do
#' not say whether the data hold the reversal. Schema 2 is described at the
#' top of this file. A schema 2 file may also carry `type_source`, where
#' each item's type came from (`proposed` or `chosen`, see
#' `sieve_type_source()`); the column is optional, a file without it (or
#' with the cell empty) reads as `proposed`, and a reader that does not
#' know the column ignores it, so a file with the column is still schema 2.
#'
#' @return A single integer.
#' @author Ken Kelley
#' @noRd
sieve_spec_schema <- function() {
  2L
}

#' The kinds of data file an export writes
#'
#' @return `c("raw", "processed")`, the default first.
#' @author Ken Kelley
#' @noRd
sieve_export_kinds <- function() {
  c("raw", "processed")
}

#' One kind of data file, as SIEVER names it
#'
#' `"analytic"` is read as `"processed"`; anything else outside the two
#' kinds is an error.
#'
#' @param values A kind.
#' @return `"raw"` or `"processed"`.
#' @author Ken Kelley
#' @noRd
sieve_export_kind <- function(values) {
  values <- as.character(values)[1]
  if (identical(values, "analytic")) {
    values <- "processed"
  }
  match.arg(values, sieve_export_kinds())
}

#' Stop an import with every problem that was found
#'
#' @param problems A character vector, one problem per element.
#' @return Does not return.
#' @author Ken Kelley
#' @noRd
sieve_import_stop <- function(problems) {
  stop(structure(
    list(message = paste(problems, collapse = "\n"), call = NULL, problems = problems),
    class = c("sieve_import_error", "error", "condition")
  ))
}

#' A number as text that is read back to the same value
#'
#' For the cells of the specification file. A decimal is written when at
#' most 15 significant digits identify the number and reading the text back
#' gives the same value (1, 7, 0.5); otherwise the number is written as a
#' hexadecimal floating point constant, which is exact by construction and
#' which [as.numeric()] reads. A missing value is an empty cell.
#'
#' @param x A numeric vector.
#' @return A character vector, one element per number.
#' @author Ken Kelley
#' @noRd
sieve_num_csv <- function(x) {
  vapply(as.numeric(x), function(v) {
    if (is.na(v)) {
      return("")
    }
    if (!is.finite(v)) {
      return(as.character(v))
    }
    out <- if (v == trunc(v) && abs(v) < 2^53) sprintf("%.0f", v) else sprintf("%.15g", v)
    if (identical(suppressWarnings(as.numeric(out)), v)) out else sprintf("%a", v)
  }, character(1))
}

#' How each item's loaded values are keyed
#'
#' The analytic values of an item are either its values as loaded or their
#' reflection within the stored bounds. Together with the ledger's keying
#' of the analytic values, that says how the values as loaded are keyed: the
#' same way when the analytic values are the loaded ones, the other way
#' when they are the reflection.
#'
#' @param raw The data as loaded.
#' @param items The analytic item data.
#' @param meta The item ledger.
#' @return A data frame with one row per ledger row: `item`, `reverse` (the
#'   keying of the analytic values), `at_loaded` (the analytic values are
#'   the loaded ones), and `loaded_reversed` (the loaded values are reverse
#'   scored); the last two are `NA` when the loaded values are not at hand
#'   or the analytic values are neither of the two.
#' @author Ken Kelley
#' @noRd
sieve_loaded_keying <- function(raw, items, meta) {
  meta <- sieve_meta_keying(meta)
  n <- nrow(meta)
  at_loaded <- rep(NA, n)
  loaded_reversed <- rep(NA, n)
  for (i in seq_len(n)) {
    it <- meta$item[i]
    if (is.null(raw) || !it %in% names(raw) || !it %in% names(items) || NROW(raw) != NROW(items) ||
      !is.numeric(raw[[it]]) || !is.numeric(items[[it]])) {
      next
    }
    current <- as.numeric(items[[it]])
    loaded <- as.numeric(raw[[it]])
    reversed <- isTRUE(as.logical(meta$reverse[i]))
    if (identical(current, loaded)) {
      at_loaded[i] <- TRUE
      loaded_reversed[i] <- reversed
    } else {
      at_loaded[i] <- FALSE
      lo <- as.numeric(meta$rev_lo[i])
      hi <- as.numeric(meta$rev_hi[i])
      if (is.finite(lo) && is.finite(hi) && identical(current, (lo + hi) - loaded)) {
        loaded_reversed[i] <- !reversed
      }
    }
  }
  data.frame(
    item = meta$item, reverse = meta$reverse %in% TRUE, at_loaded = at_loaded,
    loaded_reversed = loaded_reversed, stringsAsFactors = FALSE
  )
}

#' What a variable label says about a reverse scored item
#'
#' The text an analytic export appends to the label of an item whose values
#' in the file are reverse scored, so the file states its own keying to
#' whoever opens it, and so that SIEVER can read the statement back (see
#' `sieve_label_keying()`). The bounds are written with the digits that
#' identify them, and bounds that were read from the observed range are
#' called that.
#'
#' @param lo,hi The response bounds, possibly `NA`.
#' @param bounds Where the bounds came from (`"declared"` or `"observed"`).
#' @return A length one character string to append to the label.
#' @author Ken Kelley
#' @noRd
sieve_reversed_suffix <- function(lo, hi, bounds = "declared") {
  if (is.finite(lo) && is.finite(hi)) {
    sprintf(
      " [reverse scored within %s%s to %s]",
      if (identical(bounds, "observed")) "the observed range " else "",
      sieve_num_text(lo), sieve_num_text(hi)
    )
  } else {
    " [reverse scored]"
  }
}

#' Read the keying a variable label states
#'
#' The inverse of `sieve_reversed_suffix()`: a label that ends with the
#' text an analytic export wrote is split into the label itself and the
#' statement that the values are reverse scored, with the bounds. Any other
#' label is returned as it is, with no statement. Only the exact form
#' SIEVER writes is read; a label that merely mentions reverse scoring says
#' nothing here.
#'
#' @param label A single variable label, possibly `NA`.
#' @return A list with `label` (without the appended text), `reversed`
#'   (`TRUE` when the label states that the values are reverse scored),
#'   `lo`, `hi` (the bounds, `NA` when the label gives none), and `bounds`
#'   (`"declared"`, `"observed"`, or `NA`).
#' @author Ken Kelley
#' @noRd
sieve_label_keying <- function(label) {
  none <- list(label = label, reversed = FALSE, lo = NA_real_, hi = NA_real_, bounds = NA_character_)
  if (!is.character(label) || length(label) != 1L || is.na(label)) {
    return(none)
  }
  pattern <- " \\[reverse scored( within (the observed range )?([^] ]+) to ([^] ]+))?\\]$"
  found <- regmatches(label, regexec(pattern, label))[[1]]
  if (!length(found)) {
    return(none)
  }
  stripped <- substr(label, 1L, nchar(label) - nchar(found[1]))
  if (!nzchar(found[2])) {
    return(list(label = stripped, reversed = TRUE, lo = NA_real_, hi = NA_real_, bounds = NA_character_))
  }
  lo <- suppressWarnings(as.numeric(found[4]))
  hi <- suppressWarnings(as.numeric(found[5]))
  if (!is.finite(lo) || !is.finite(hi) || lo >= hi) {
    return(none)
  }
  list(
    label = stripped, reversed = TRUE, lo = lo, hi = hi,
    bounds = if (nzchar(found[3])) "observed" else "declared"
  )
}

#' The data frame an export writes
#'
#' Every column of the data as loaded, in order. With
#' `values = "processed"` the item columns hold the values the analyses
#' use; with `values = "raw"` (the default) every column is exactly as it
#' was read. The Item Name is attached to each item as its variable label,
#' and the label of an item whose values in this file are reverse scored
#' says so, with the bounds, so the file states its own keying to whoever
#' opens it.
#'
#' @param raw The data as loaded.
#' @param items The processed item data.
#' @param meta The item ledger.
#' @param values `"raw"` or `"processed"` (`"analytic"` is read as
#'   `"processed"`).
#' @param rows The original row number of every row (`state$case_index`),
#'   or `NULL`. When respondents were removed from the analysis (the rows
#'   are not 1 to n), the file begins with a column `original_row` that
#'   gives each row's number in the data as loaded, so a row can be traced
#'   to its source.
#' @return A data frame with the attribute `siever_values` set to `values`.
#' @author Ken Kelley
#' @noRd
sieve_export_data <- function(raw, items, meta, values = sieve_export_kinds(), rows = NULL) {
  values <- sieve_export_kind(values)
  if (is.null(raw) || is.null(items) || is.null(meta)) {
    stop("No data are loaded, so there is nothing to export.", call. = FALSE)
  }
  meta <- sieve_meta_keying(meta)
  keying <- sieve_loaded_keying(raw, items, meta)
  d <- raw
  for (i in seq_len(nrow(meta))) {
    it <- meta$item[i]
    if (!it %in% names(d)) {
      next
    }
    if (identical(values, "processed") && it %in% names(items)) {
      d[[it]] <- items[[it]]
    }
    reversed <- if (identical(values, "processed")) keying$reverse[i] else keying$loaded_reversed[i]
    # The Item Name without any statement of keying an earlier export left
    # in it, so the statement is written once and only when it is true.
    label <- sieve_label_keying(as.character(meta$label[i]))$label
    if (is.na(label) || !nzchar(label)) {
      label <- it
    }
    if (isTRUE(reversed)) {
      label <- paste0(label, sieve_reversed_suffix(
        as.numeric(meta$rev_lo[i]), as.numeric(meta$rev_hi[i]),
        as.character(meta$rev_bounds[i])
      ))
    }
    attr(d[[it]], "label") <- label
  }
  if (!is.null(rows) && length(rows) == nrow(d) && !identical(as.integer(rows), seq_len(nrow(d)))) {
    original_row <- as.integer(rows)
    attr(original_row, "label") <- "Row of the data as loaded (respondents removed from the analysis are not in this file)"
    d <- cbind(data.frame(original_row = original_row), d)
  }
  attr(d, "siever_values") <- values
  d
}

#' The specification that goes with an exported data file
#'
#' One row per column of the data, in data order, every cell text (so that
#' nothing is rounded on the way to the file). See the top of this file for
#' the columns. `reverse_applied` and `data_values` describe the data file
#' of the same kind (`values`); `key_processed` and `key_loaded` are the
#' content keys of the item's values in a processed and in an as loaded
#' data file, and `loaded_reversed` says whether the as loaded values are
#' reverse scored. `kind` is the validity kind of a criterion
#' (`sieve_role_kinds()`), empty for every other row.
#'
#' @param raw The data as loaded.
#' @param items The processed item data.
#' @param meta The item ledger.
#' @param roles The roles table, or `NULL` to derive one.
#' @param values The kind of data file the specification is written for.
#' @return A data frame of character columns.
#' @author Ken Kelley
#' @noRd
sieve_spec_table <- function(raw, items, meta, roles = NULL, values = sieve_export_kinds()) {
  values <- sieve_export_kind(values)
  if (is.null(raw) || is.null(items) || is.null(meta)) {
    stop("No data are loaded, so there is no specification to write.", call. = FALSE)
  }
  roles <- sieve_roles_table(raw, items, roles)
  meta <- sieve_meta_keying(meta)
  keying <- sieve_loaded_keying(raw, items, meta)
  vars <- roles$variable
  at <- match(vars, meta$item)
  text <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  }
  flag <- function(x) ifelse(is.na(x), "", ifelse(x, "TRUE", "FALSE"))
  pick <- function(name) if (name %in% names(meta)) meta[[name]][at] else rep(NA, length(vars))
  status <- text(pick("status"))
  status[status == "active"] <- "included"
  status[status == "parked"] <- "excluded"
  reverse <- keying$reverse[at]
  loaded_reversed <- keying$loaded_reversed[at]
  key_of <- function(data) {
    vapply(seq_along(vars), function(j) {
      if (is.na(at[j]) || is.null(data) || !vars[j] %in% names(data)) "" else sieve_values_key(data[[vars[j]]])
    }, character(1))
  }
  kind <- if ("kind" %in% names(roles)) text(roles$kind) else rep("", length(vars))
  kind[roles$role != "criterion"] <- ""
  data.frame(
    siever_specification = as.character(sieve_spec_schema()),
    data_values = values,
    variable = vars,
    role = roles$role,
    kind = kind,
    label = text(pick("label")),
    wording = text(pick("wording")),
    type = text(pick("type")),
    type_source = ifelse(is.na(at), "", sieve_type_source(meta)[at]),
    construct = text(pick("construct")),
    facet = text(pick("facet")),
    status = status,
    reverse = flag(reverse),
    reverse_applied = flag(if (identical(values, "processed")) reverse else loaded_reversed),
    rev_lo = sieve_num_csv(pick("rev_lo")),
    rev_hi = sieve_num_csv(pick("rev_hi")),
    rev_bounds = text(pick("rev_bounds")),
    loaded_reversed = flag(loaded_reversed),
    key_processed = key_of(items),
    key_loaded = key_of(raw),
    stringsAsFactors = FALSE
  )
}

# ---- The summary export --------------------------------------------------------

#' The moments of the included items
#'
#' The covariance matrix of the included items' processed values (reverse
#' scored items as scored in the session), on the rows complete on every
#' included item, with their means when asked for and the number of
#' complete cases: what an analysis that starts from moments needs
#' (`sample.cov`, `sample.mean` and `sample.nobs` in lavaan). Full
#' precision; nothing is rounded.
#'
#' @param items The processed item data.
#' @param meta The item ledger.
#' @param means Whether to compute the means.
#' @param basis The shared correlation basis of the included items (a
#'   `sieve_state_basis()` or `sieve_retention_cor()` result), or `NULL`. When
#'   the correlations it used are polychoric, tetrachoric or mixed (the
#'   analysis basis of ordered or binary items), that matrix is part of the
#'   summary as well, under its name; a Pearson basis adds nothing, since the
#'   covariance matrix holds it.
#' @return A list with `items` (the included item names, in ledger order),
#'   `cov` (the covariance matrix, named), `means` (a named numeric vector,
#'   or `NULL`), `n` (complete cases), `n_rows` (rows of the item data),
#'   `cor` (the basis's correlation matrix, or `NULL`), `cor_kind` (its
#'   identifier, such as `"polychoric"`, or `NA`), `cor_requested` (what
#'   was requested of the basis) and `cor_n` (the complete cases it was
#'   computed on). Stops when fewer than two complete cases or no included
#'   item remain.
#' @author Ken Kelley
#' @noRd
sieve_summary_table <- function(items, meta, means = TRUE, basis = NULL) {
  if (is.null(items) || is.null(meta)) {
    stop("No data are loaded, so there is no summary to write.", call. = FALSE)
  }
  active <- meta[meta$status == "active", , drop = FALSE]
  its <- intersect(as.character(active$item), names(items))
  if (!length(its)) {
    stop("No item is included, so there is no summary to write.", call. = FALSE)
  }
  X <- as.data.frame(items)[, its, drop = FALSE]
  X[] <- lapply(X, sieve_as_number)
  X <- X[stats::complete.cases(X), , drop = FALSE]
  if (nrow(X) < 2L) {
    stop("Fewer than two rows are complete on every included item, so no covariance matrix can be written.",
      call. = FALSE
    )
  }
  S <- stats::cov(as.matrix(X))
  dimnames(S) <- list(its, its)
  out <- list(
    items = its, cov = S, means = if (isTRUE(means)) colMeans(as.matrix(X)) else NULL,
    n = nrow(X), n_rows = nrow(items), cor = NULL, cor_kind = NA_character_,
    cor_requested = NA_character_, cor_n = NA_integer_
  )
  # The analysis basis, when it is a matrix the covariance matrix does not
  # hold (polychoric, tetrachoric, or mixed correlations of the same pool).
  if (is.list(basis) && isTRUE(basis$ok) && is.matrix(basis$R) &&
    basis$used %in% c("polychoric", "tetrachoric", "mixed") &&
    all(its %in% rownames(basis$R)) && all(its %in% colnames(basis$R))) {
    R <- basis$R[its, its, drop = FALSE]
    dimnames(R) <- list(its, its)
    out$cor <- R
    out$cor_kind <- basis$used
    out$cor_requested <- basis$requested %||% basis$used
    out$cor_n <- as.integer(basis$n %||% NA_integer_)
  }
  out
}

#' The header lines of a summary file
#'
#' @param summary A `sieve_summary_table()` result.
#' @param dataset The dataset name, or `NULL`.
#' @return A character vector of comment lines, each starting with `#`.
#' @author Ken Kelley
#' @noRd
sieve_summary_header <- function(summary, dataset = NULL) {
  c(
    sprintf(
      paste(
        "# SIEVER summary of %d included item%s: the covariance matrix of the processed values",
        "(reverse scored items as scored in the session), computed on the %d rows complete on",
        "every included item (of %d rows)."
      ),
      length(summary$items), if (length(summary$items) == 1L) "" else "s", summary$n, summary$n_rows
    ),
    sprintf(
      "# Basis: processed item values, complete cases, n = %d.%s", summary$n,
      if (is.null(summary$means)) " Means were not requested." else " The row named mean, after the covariance rows, holds the means."
    ),
    if (!is.null(summary$cor)) {
      sprintf(
        paste(
          "# Correlations: the rows named %s.<item> hold the %s correlation matrix of the included",
          "items, the analysis basis shared by the Screening, Dimensionality, EFA and Items tabs",
          "(requested %s, used %s; complete cases, n = %s)."
        ),
        summary$cor_kind, sieve_retention_basis_label(summary$cor_kind),
        sieve_retention_basis_label(summary$cor_requested), sieve_retention_basis_label(summary$cor_kind),
        if (is.finite(summary$cor_n)) summary$cor_n else "not recorded"
      )
    } else {
      "# Correlations: the analysis basis is Pearson, which the covariance matrix holds; no correlation block."
    },
    sprintf(
      "# Dataset: %s. Read it back with read.csv(file, comment.char = \"#\", row.names = 1).",
      dataset %||% "(no name)"
    )
  )
}

#' Write the summary of the included items to a comma separated file
#'
#' The header comment lines name the basis (processed values, complete
#' cases, n), then a table with one row per included item (the covariance
#' matrix, with the item names as row names and column names), when the
#' means were computed one more row named `mean`, and, when the analysis
#' basis is polychoric, tetrachoric or mixed, that correlation matrix as
#' one row per item named `<kind>.<item>` (so one `read.csv()` reads the
#' whole file and the blocks are told apart by their row names). The
#' numbers are written with 17 significant digits, which identify every
#' double.
#'
#' @param summary A `sieve_summary_table()` result.
#' @param file The path to write.
#' @param dataset The dataset name, for the header.
#' @return The path, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_summary_write <- function(summary, file, dataset = NULL) {
  con <- file(file, open = "w", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(sieve_summary_header(summary, dataset), con)
  rows <- summary$cov
  if (!is.null(summary$means)) {
    rows <- rbind(rows, mean = summary$means)
  }
  if (!is.null(summary$cor)) {
    block <- summary$cor
    rownames(block) <- paste(summary$cor_kind, rownames(block), sep = ".")
    rows <- rbind(rows, block)
  }
  text <- matrix(sprintf("%.17g", rows), nrow = nrow(rows), dimnames = dimnames(rows))
  utils::write.csv(text, con, row.names = TRUE, quote = TRUE)
  invisible(file)
}

#' The R that writes the summary file in a replay
#'
#' @param means Whether the means are written.
#' @param file The file name, for the script.
#' @param cor The identifier of the correlation block written after the
#'   matrix (`"polychoric"`, `"tetrachoric"` or `"mixed"`), or `NULL` for
#'   none. The block is read from `R_items`, which the recorded basis step
#'   computes.
#' @return A character vector of R source lines, reading the included
#'   items from `active_items` (the preamble of the session script builds
#'   it).
#' @author Ken Kelley
#' @noRd
sieve_summary_code <- function(means, file, cor = NULL) {
  c(
    "# The summary export: the covariance matrix of the included items' processed",
    "# values on the complete cases, with the means when asked for, and n.",
    "summary_x <- active_items[stats::complete.cases(active_items), , drop = FALSE]",
    "summary_cov <- stats::cov(as.matrix(summary_x))",
    if (isTRUE(means)) "summary_means <- colMeans(as.matrix(summary_x))",
    "summary_n <- nrow(summary_x)",
    if (isTRUE(means)) {
      "summary_rows <- rbind(summary_cov, mean = summary_means)"
    } else {
      "summary_rows <- summary_cov  # no means row"
    },
    if (!is.null(cor)) {
      c(
        sprintf("# The analysis basis (%s correlations, R_items from the basis step above), one row per", cor),
        sprintf("# item named %s.<item> after the covariance rows.", cor),
        "summary_block <- R_items[colnames(summary_x), colnames(summary_x), drop = FALSE]",
        sprintf("rownames(summary_block) <- paste(\"%s\", rownames(summary_block), sep = \".\")", cor),
        "summary_rows <- rbind(summary_rows, summary_block)"
      )
    },
    sprintf("summary_con <- file(%s, open = \"w\", encoding = \"UTF-8\")", encodeString(file, quote = "\"")),
    "writeLines(sprintf(\"# SIEVER summary: covariance matrix of the processed values of %d included items, complete cases n = %d\",",
    "                   ncol(summary_x), summary_n), summary_con)",
    if (!is.null(cor)) {
      sprintf(
        "writeLines(\"# Rows named %s.<item> hold the %s correlation matrix of the included items.\", summary_con)",
        cor,
        cor
      )
    },
    "write.csv(matrix(sprintf(\"%.17g\", summary_rows), nrow = nrow(summary_rows), dimnames = dimnames(summary_rows)),",
    "          summary_con, row.names = TRUE)",
    "close(summary_con)"
  )
}

#' Write a specification to a comma separated file
#'
#' @param table A specification from `sieve_spec_table()`.
#' @param file The path to write.
#' @return The path, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_spec_write <- function(table, file) {
  utils::write.csv(table, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(file)
}

#' Read a specification file as text
#'
#' Every cell is read as text, so no type is guessed on the way in: the
#' vocabulary and the numbers are checked by `sieve_spec_import()`. The
#' bytes are checked first, as a data file's are (`sieve_text_bytes()`):
#' the file is read as UTF-8 (what `sieve_spec_write()` writes and Excel's
#' "CSV UTF-8" saves) or as the stated `encoding`, in every locale, so a
#' wording with one curly apostrophe is read as written, or refused with the
#' reason in a session that is not UTF-8, never read as no rows. The
#' separator is the one the header shows after (or before)
#' `siever_specification`, so a specification that a spreadsheet saved with
#' semicolons (Excel does in locales whose decimal mark is a comma) is read
#' as well; without that column the file is read as comma separated.
#'
#' @param file The path of a comma separated file.
#' @param encoding One of `sieve_text_encodings()`.
#' @param utf8_first Whether bytes that are valid UTF-8 are read as UTF-8
#'   whatever `encoding` says (see `sieve_text_bytes()`):
#'   `sieve_prepare_study()` reads a specification so by default, since
#'   SIEVER writes every specification in UTF-8 whatever the data file's
#'   encoding.
#' @param argument The argument a refusal names for reading the file in
#'   another encoding, or `NULL` (the default: the Ingest tab, which reads a
#'   specification only as UTF-8) to say that a file that is not UTF-8 must
#'   be saved as CSV UTF-8 (`"encoding"` when `encoding` is not UTF-8).
#' @return A data frame of character columns with lower case names. Stops
#'   with a `sieve_import_error` when the file cannot be read as a table.
#' @author Ken Kelley
#' @noRd
sieve_spec_read <- function(file, encoding = "UTF-8", utf8_first = FALSE, argument = NULL) {
  if (!is.character(file) || length(file) != 1L || !file.exists(file)) {
    sieve_import_stop("The specification file was not found. Choose the specification file again.")
  }
  encoding <- sieve_text_choices(",", ".", encoding)$encoding
  # A stated encoding other than UTF-8 is named in a refusal by its own
  # argument.
  if (is.null(argument) && !identical(encoding, "UTF-8")) argument <- "encoding"
  checked <- sieve_text_bytes(file, encoding, ",", utf8_first = utf8_first, argument = argument, ingest = FALSE)
  if (!isTRUE(checked$ok)) {
    sieve_import_stop(sprintf("The specification could not be read: %s.", checked$error))
  }
  end <- which(checked$bytes == as.raw(0x0A))[1]
  header <- rawToChar(if (is.na(end)) checked$bytes else checked$bytes[seq_len(end - 1L)])
  sep <- ","
  at <- regexpr("siever_specification", tolower(header), fixed = TRUE)
  if (at > 0L) {
    # The character after the column name (or before it, when it is the
    # last column), past the quotation mark of a quoted name.
    after <- at + attr(at, "match.length")
    before <- at - 1L
    if (identical(substr(header, after, after), "\"")) after <- after + 1L
    if (identical(substr(header, before, before), "\"")) before <- before - 1L
    beside <- c(substr(header, after, after), if (before >= 1L) substr(header, before, before))
    beside <- beside[beside %in% sieve_text_separators()]
    if (length(beside)) sep <- beside[1]
  }
  read <- sieve_text_parse(checked,
    header = TRUE, sep = sep, quote = "\"", fill = TRUE, comment.char = "",
    colClasses = "character", check.names = FALSE, na.strings = character(0),
    strip.white = TRUE, stringsAsFactors = FALSE
  )
  if (!read$ok) {
    sieve_import_stop(sprintf(
      "The specification could not be read as a %s separated table (%s).",
      if (identical(sep, ",")) "comma" else names(sieve_text_separators())[match(sep, sieve_text_separators())],
      read$error
    ))
  }
  spec <- read$value
  names(spec) <- tolower(trimws(names(spec)))
  spec
}

#' Name a few of a set, for a message
#'
#' @param x A character vector.
#' @param most How many to name.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_name_some <- function(x, most = 6L) {
  x <- unique(x)
  shown <- utils::head(x, most)
  paste0(
    paste(shown, collapse = ", "),
    if (length(x) > length(shown)) sprintf(", and %d more", length(x) - length(shown)) else ""
  )
}

#' What a specification's content keys show about an item's values
#'
#' A specification carries the content key of each item's values in the
#' analytic file it was saved with (keyed as `reverse` says) and in the file
#' as loaded (keyed as `loaded_reversed` says). This compares the values
#' now in the session with those keys, in three ways, and reports the
#' keying the values are thereby known to have:
#'
#' * the values themselves match a key: they have that key's keying;
#' * the session has moved the item away from its values as loaded and the
#'   values as loaded match a key: the current values are their reflection,
#'   so they have the other keying;
#' * the session has not moved the item and its values match a key once
#'   they are reflected within the bounds tried (those of the
#'   specification, those stored with the item, the observed range): they
#'   are the reflection of the saved values, so they have the other keying.
#'   A match is a match of the content key of every value, so it is not a
#'   guess; a reflection that does not reproduce the saved values exactly
#'   (non-integer responses can differ in the last place) is no match.
#'
#' @param column The item's values now.
#' @param loaded The item's values as loaded, or `NULL`.
#' @param want The specification's `reverse` cell (the keying of the
#'   analytic values it was saved with).
#' @param loaded_rev The specification's `loaded_reversed` cell, or `NA`.
#' @param key_analytic,key_loaded The content keys, `""` when not given.
#' @param bounds A list of `c(lo, hi)` pairs to try a reflection within, in
#'   the order to try them, named by where each pair comes from (for
#'   example `specification`, `stored`, `observed`). A pair with a missing
#'   bound, or outside which a value lies, is not tried.
#' @return A list with `holds` (`TRUE` when the values are known to be
#'   reverse scored, `FALSE` when known not to be, `NA` when the keys show
#'   nothing), `by` (`"values"`, `"loaded"`, `"reflection"`, or `NA`),
#'   `lo` and `hi` (the bounds of the reflection that matched, else `NA`),
#'   `bounds` (the name of the pair that matched, else `NA`), and `conflict`
#'   (`TRUE` when the keys point both ways, which a specification SIEVER
#'   wrote never does).
#' @author Ken Kelley
#' @noRd
sieve_keying_evidence <- function(column, loaded, want, loaded_rev, key_analytic, key_loaded,
                                  bounds = list()) {
  out <- list(
    holds = NA, by = NA_character_, lo = NA_real_, hi = NA_real_,
    bounds = NA_character_, conflict = FALSE
  )
  if (!is.numeric(column)) {
    return(out)
  }
  # The keying the specification gives to values with this content key.
  said <- function(key) {
    c(
      if (nzchar(key_analytic) && identical(key, key_analytic) && !is.na(want)) isTRUE(want),
      if (nzchar(key_loaded) && identical(key, key_loaded) && !is.na(loaded_rev)) isTRUE(loaded_rev)
    )
  }
  settle <- function(votes, by, lo = NA_real_, hi = NA_real_, which = NA_character_) {
    if (length(unique(votes)) > 1L) {
      return(utils::modifyList(out, list(conflict = TRUE)))
    }
    list(holds = votes[1], by = by, lo = lo, hi = hi, bounds = which, conflict = FALSE)
  }
  current <- as.numeric(column)
  moved <- !is.null(loaded) && is.numeric(loaded) && !identical(current, as.numeric(loaded))
  votes <- said(sieve_values_key(current))
  by <- "values"
  if (moved) {
    of_loaded <- said(sieve_values_key(loaded))
    if (length(of_loaded)) {
      votes <- c(votes, !of_loaded)
      by <- if (length(votes) > length(of_loaded)) "values" else "loaded"
    }
  }
  if (length(votes)) {
    return(settle(votes, by))
  }
  if (moved) {
    return(out)
  }
  seen <- current[is.finite(current)]
  labels <- names(bounds)
  if (is.null(labels)) {
    labels <- rep(NA_character_, length(bounds))
  }
  for (b in seq_along(bounds)) {
    pair <- bounds[[b]]
    lo <- as.numeric(pair[1])
    hi <- as.numeric(pair[2])
    if (!is.finite(lo) || !is.finite(hi) || lo >= hi || !length(seen) ||
      length(sieve_outside_bounds(current, lo, hi))) {
      next
    }
    of_reflection <- said(sieve_values_key((lo + hi) - current))
    if (length(of_reflection)) {
      which <- if (is.na(labels[b]) || !nzchar(labels[b])) NA_character_ else labels[b]
      return(settle(!of_reflection, "reflection", lo, hi, which))
    }
  }
  out
}

#' Apply a specification to a project, on copies
#'
#' Checks the specification and works out everything it changes, on copies
#' of the item data, the ledger and the roles table. Nothing is committed
#' here; the result is what `sieve_apply_spec()` writes to the state. The
#' function stops with a `sieve_import_error` that lists every problem it
#' found, and in that case nothing has been changed anywhere.
#'
#' What is checked: the specification has a `variable` (or `item`) column
#' and at least one row; no variable name is blank or listed twice; every
#' variable is in the loaded data; roles, validity kinds, measurement
#' types, statuses, the data kind, the true or false columns, the bounds
#' and the content keys are of their vocabularies; a column that is not
#' stored as a number is not made an item; a variable that is not an item
#' is not reverse scored, and one that is not a criterion carries no
#' validity kind; a construct does not have the name of a variable in the
#' data, and a facet has a construct to belong to; and a specification of
#' a newer schema is not read as if it were the current one.
#'
#' The keying of each item whose `reverse` cell is filled is settled before
#' anything is reflected (an item whose cell is empty keeps its keying, and
#' bounds given for it are stored as its declared bounds). The values now in
#' the item data are:
#'
#' * what this session made them, when the session has reverse scored or
#'   recorded the item (its own record is used);
#' * the analytic values the specification was saved with, when their
#'   content key matches `key_analytic` (they have the keying `reverse`);
#' * the as loaded values the specification was saved with, when their key
#'   matches `key_loaded` (they have the keying `loaded_reversed`);
#' * otherwise unknown. Then `keying = "verify"` uses `reverse_applied`
#'   only when the specification carries no content keys (a specification
#'   written by hand: its author's statement is all there is), and stops
#'   when it carries keys that do not match or says nothing;
#'   `"administered"` is the researcher's statement that the loaded data
#'   hold every item as administered; and `"reversed"` is the statement
#'   that they already hold the reverse keyed items reverse scored (they
#'   are keyed as `reverse` says, so nothing is reflected).
#'
#' An item whose values already hold the keying the specification asks for
#' is recorded as such, with the bounds, and no value is changed. An item
#' whose values hold the other keying is reflected once, through
#' `sieve_reverse_item()`, within the bounds of the specification (or the
#' bounds in force for it in the session, which are never overwritten).
#'
#' @param spec A specification as read by `sieve_spec_read()`.
#' @param raw The data as loaded.
#' @param items The analytic item data.
#' @param meta The item ledger.
#' @param roles The roles table, or `NULL` to derive one.
#' @param keying What to take the values of a reverse keyed item to be when
#'   the specification cannot settle it: `"verify"` (stop and ask),
#'   `"administered"`, or `"reversed"`.
#' @param held The ledger rows held aside for columns that left the item
#'   pool (see `sieve_set_roles()`).
#' @param removed The respondents removed from the analysis
#'   (`state$removed_cases`): a reversal within the observed range reads
#'   the range of the data as loaded, their values included.
#' @param type_source Where the types the specification gives come from
#'   when its `type_source` cell is empty: `"proposed"` (the default) or
#'   `"chosen"` (the pre-SIEVER step with the rule off).
#' @param transforms,dataset_id The session's transformation records and
#'   dataset (`state$transforms`, `state$dataset_id`): whether a reverse
#'   scored item holds its stored bounds is decided as its Response bounds
#'   cell decides it (`sieve_holds_reversal_bounds()`). An item that does
#'   not hold them takes the specification's bounds; `NULL` takes every
#'   reversal with stored bounds to hold them.
#' @param min_options The five-option rule (the checkbox in step 1 of the
#'   Ingest tab): an item the specification types ordered that has at
#'   least this many response options (its distinct values, or the points
#'   its declared bounds span; see `sieve_option_rule()`) is typed
#'   continuous. `NULL` keeps every type the specification gives.
#' @return A list with `items`, `meta`, `roles`, `id_vars`, `held`, `steps`
#'   (the reversals and keying records, in the order they were applied),
#'   `pool_changed`, and `summary` (what was done, for the message and the
#'   log; `switched` names the items the five-option rule typed
#'   continuous and `switch_options` their response options).
#' @author Ken Kelley
#' @noRd
sieve_spec_import <- function(spec, raw, items, meta, roles = NULL,
                              keying = c("verify", "administered", "reversed"), held = list(),
                              min_options = NULL, removed = list(), type_source = "proposed",
                              transforms = NULL, dataset_id = NULL) {
  keying <- match.arg(keying)
  type_source <- match.arg(type_source, c("proposed", "chosen"))
  if (is.null(raw) || is.null(items) || is.null(meta)) {
    sieve_import_stop("No data are loaded, so there is nothing to apply a specification to.")
  }
  if (!is.data.frame(spec) || !ncol(spec)) {
    sieve_import_stop(paste(
      "The specification has no columns. Restore a file that SIEVER's Download",
      "specification wrote, or one with at least a variable column."
    ))
  }
  names(spec) <- tolower(trimws(names(spec)))
  problems <- character(0)
  twice <- unique(names(spec)[duplicated(names(spec))])
  if (length(twice)) {
    problems <- c(problems, sprintf(
      "The specification has the column %s more than once.",
      sieve_name_some(twice)
    ))
  }
  id_col <- if ("variable" %in% names(spec)) "variable" else if ("item" %in% names(spec)) "item" else NA
  if (is.na(id_col)) {
    sieve_import_stop(c(problems, sprintf(
      paste(
        "The specification needs a column named variable (or item) that holds the variable",
        "names. Its columns are: %s."
      ),
      sieve_name_some(names(spec), 12L)
    )))
  }
  if (!nrow(spec)) {
    sieve_import_stop(c(problems, paste(
      "The specification has no rows. Restore a file that SIEVER's",
      "Download specification wrote, with one row per variable."
    )))
  }
  col <- function(name) {
    if (!name %in% names(spec)) {
      return(NULL)
    }
    v <- trimws(as.character(spec[[name]]))
    v[is.na(v)] <- ""
    v
  }
  ids <- col(id_col)
  n <- length(ids)

  blank <- which(!nzchar(ids))
  if (length(blank)) {
    problems <- c(problems, sprintf(
      "A row of the specification has no variable name (line %s of the file, counting the header).",
      sieve_name_some(as.character(blank + 1L))
    ))
  }
  cons <- col("construct")
  repeated <- unique(ids[nzchar(ids) & duplicated(ids)])
  for (v in repeated) {
    rows <- which(ids == v)
    under <- if (!is.null(cons)) unique(cons[rows][nzchar(cons[rows])]) else character(0)
    problems <- c(problems, sprintf(
      "%s is listed %d times%s. Each variable can be listed once.", v, length(rows),
      if (length(under) > 1L) sprintf(", under the constructs %s", sieve_name_some(under)) else ""
    ))
  }
  schema <- col("siever_specification")
  if (!is.null(schema)) {
    said <- unique(schema[nzchar(schema)])
    number <- suppressWarnings(as.numeric(said))
    if (length(said) != 1L || is.na(number) || number != trunc(number) || number < 1) {
      problems <- c(problems, "The siever_specification column does not hold one schema version.")
    } else if (number > sieve_spec_schema()) {
      problems <- c(problems, sprintf(
        paste(
          "The specification was written by a newer SIEVER (specification schema %d; this",
          "version reads up to schema %d). Update SIEVER to restore it."
        ),
        as.integer(number), sieve_spec_schema()
      ))
    }
  }
  absent <- setdiff(ids[nzchar(ids)], names(raw))
  if (length(absent)) {
    problems <- c(problems, sprintf(
      "%d variable%s in the specification %s not in the loaded data: %s.",
      length(absent), if (length(absent) == 1L) "" else "s",
      if (length(absent) == 1L) "is" else "are", sieve_name_some(absent)
    ))
  }

  offenders <- function(bad, cells) {
    sieve_name_some(sprintf("%s (\"%s\")", ids[bad], cells[bad]))
  }
  role_in <- col("role")
  role_low <- if (is.null(role_in)) NULL else tolower(role_in)
  if (!is.null(role_low)) {
    bad <- nzchar(role_low) & !role_low %in% sieve_role_levels()
    if (any(bad)) {
      problems <- c(problems, sprintf(
        "A role is not one of %s: %s.",
        paste(sieve_role_levels(), collapse = ", "),
        offenders(bad, role_in)
      ))
    }
  }
  type_in <- col("type")
  type_low <- if (is.null(type_in)) NULL else tolower(type_in)
  if (!is.null(type_low)) {
    # The three types the grid offers, and the type the item already has (a
    # constant item is detected as "constant" and has to pass unchanged).
    now <- as.character(meta$type[match(ids, meta$item)])
    now[is.na(now)] <- ""
    bad <- nzchar(type_low) & !type_low %in% c("continuous", "ordered", "binary") & type_low != now
    if (any(bad)) {
      problems <- c(problems, sprintf(
        "A measurement type is not one of continuous, ordered, binary: %s.", offenders(bad, type_in)
      ))
    }
  }
  source_in <- col("type_source")
  source_low <- if (is.null(source_in)) NULL else tolower(source_in)
  if (!is.null(source_low)) {
    bad <- nzchar(source_low) & !source_low %in% c("proposed", "chosen")
    if (any(bad)) {
      problems <- c(problems, sprintf(
        "A type_source cell is neither proposed nor chosen: %s.", offenders(bad, source_in)
      ))
    }
  }
  status_map <- c(
    included = "active", excluded = "parked", active = "active", parked = "parked",
    `in use` = "active", dropped = "parked"
  )
  status_in <- col("status")
  status_low <- if (is.null(status_in)) NULL else tolower(status_in)
  if (!is.null(status_low)) {
    bad <- nzchar(status_low) & !status_low %in% names(status_map)
    if (any(bad)) {
      problems <- c(problems, sprintf(
        "A status is neither included nor excluded: %s.",
        offenders(bad, status_in)
      ))
    }
  }
  # Each reader returns the values and, when a cell is outside its
  # vocabulary, the problem to report.
  as_flag <- function(name) {
    v <- col(name)
    if (is.null(v)) {
      return(list(value = rep(NA, n), problem = NULL))
    }
    low <- tolower(v)
    out <- rep(NA, n)
    out[low %in% c("true", "t", "yes", "y", "1")] <- TRUE
    out[low %in% c("false", "f", "no", "n", "0")] <- FALSE
    bad <- nzchar(low) & is.na(out)
    list(value = out, problem = if (any(bad)) {
      sprintf("A %s cell is neither TRUE nor FALSE: %s.", name, offenders(bad, v))
    })
  }
  as_bound <- function(name) {
    v <- col(name)
    if (is.null(v)) {
      return(list(value = rep(NA_real_, n), problem = NULL))
    }
    out <- suppressWarnings(as.numeric(v))
    out[!nzchar(v)] <- NA_real_
    bad <- nzchar(v) & !is.finite(out)
    out[bad] <- NA_real_
    list(value = out, problem = if (any(bad)) {
      sprintf("A %s cell is not a number: %s.", name, offenders(bad, v))
    })
  }
  as_key <- function(name) {
    v <- col(name)
    if (is.null(v)) {
      return(list(value = rep("", n), problem = NULL))
    }
    bad <- nzchar(v) & !grepl("^[0-9a-f]{32}$", v)
    list(value = v, problem = if (any(bad)) {
      sprintf(
        "A %s cell is not a content key (the file was edited or damaged): %s.",
        name, sieve_name_some(ids[bad])
      )
    })
  }
  cells <- list(
    reverse = as_flag("reverse"),
    reverse_applied = as_flag("reverse_applied"),
    loaded_reversed = as_flag("loaded_reversed"),
    rev_lo = as_bound("rev_lo"),
    rev_hi = as_bound("rev_hi"),
    # The content key of the processed values: key_processed, or
    # key_analytic in a file without a key_processed column.
    key_processed = as_key(if ("key_processed" %in% names(spec)) "key_processed" else "key_analytic"),
    key_loaded = as_key("key_loaded")
  )
  problems <- c(problems, unlist(lapply(cells, function(cell) cell$problem), use.names = FALSE))
  want_reverse <- cells$reverse$value
  applied <- cells$reverse_applied$value
  loaded_rev <- cells$loaded_reversed$value
  lo_in <- cells$rev_lo$value
  hi_in <- cells$rev_hi$value
  key_analytic <- cells$key_processed$value
  key_loaded <- cells$key_loaded$value
  values_in <- col("data_values")
  if (!is.null(values_in)) {
    low <- tolower(values_in)
    bad <- nzchar(low) & !low %in% c("raw", "processed", "analytic")
    if (any(bad)) {
      problems <- c(problems, sprintf(
        "A data_values cell is neither raw nor processed: %s.",
        offenders(bad, values_in)
      ))
    }
  }
  kind_in <- col("kind")
  kind_low <- if (is.null(kind_in)) NULL else tolower(kind_in)
  if (!is.null(kind_low)) {
    bad <- nzchar(kind_low) & !kind_low %in% sieve_role_kinds()
    if (any(bad)) {
      problems <- c(problems, sprintf(
        "A validity kind is not one of %s: %s.",
        paste(sieve_role_kinds(), collapse = ", "), offenders(bad, kind_in)
      ))
    }
  }
  half <- nzchar(ids) & (is.na(lo_in) != is.na(hi_in))
  if (any(half)) {
    problems <- c(problems, sprintf(
      "Response bounds need both a minimum (rev_lo) and a maximum (rev_hi): %s.",
      sieve_name_some(ids[half])
    ))
  }
  inverted <- !is.na(lo_in) & !is.na(hi_in) & lo_in >= hi_in
  if (any(inverted)) {
    problems <- c(problems, sprintf(
      "Response bounds need a minimum below the maximum: %s.", sieve_name_some(ids[inverted])
    ))
  }
  label_in <- col("rev_bounds")
  if (!is.null(label_in)) {
    bad <- nzchar(label_in) & !tolower(label_in) %in% c("declared", "observed")
    if (any(bad)) {
      problems <- c(problems, sprintf(
        "A rev_bounds cell is neither declared nor observed: %s.",
        offenders(bad, label_in)
      ))
    }
  }
  if (!is.null(cons)) {
    clash <- nzchar(cons) & cons %in% names(raw)
    if (any(clash)) {
      problems <- c(problems, sprintf(
        paste(
          "A construct has the name of a variable in the data (%s). A construct and a variable",
          "cannot share a name: the model could not tell the factor from the observed variable."
        ),
        sieve_name_some(sprintf("construct %s for %s", cons[clash], ids[clash]))
      ))
    }
  }
  if (length(problems)) {
    sieve_import_stop(problems)
  }

  # ---- Roles ---------------------------------------------------------------
  roles <- sieve_roles_table(raw, items, roles)
  role_now <- roles$role[match(ids, roles$variable)]
  role_want <- if (is.null(role_low)) rep("item", n) else ifelse(nzchar(role_low), role_low, role_now)
  role_changes <- data.frame(
    variable = character(0), from = character(0), to = character(0),
    stringsAsFactors = FALSE
  )
  move <- role_want != role_now
  if (any(move)) {
    set <- tryCatch(
      sieve_set_roles(raw, items, meta, roles, stats::setNames(role_want[move], ids[move]),
        held = held
      ),
      sieve_role_error = function(e) e
    )
    if (inherits(set, "sieve_role_error")) {
      sieve_import_stop(conditionMessage(set))
    }
    items <- set$items
    meta <- set$meta
    roles <- set$roles
    held <- set$held
    role_changes <- set$changed
  }
  meta <- sieve_meta_keying(meta)
  is_item <- role_want == "item"
  at <- match(ids, meta$item)
  # The validity kind of each criterion the specification states; a kind
  # on a variable that is not a criterion is refused with the others.
  if (!is.null(kind_low)) {
    stated_kind <- nzchar(kind_low)
    misplaced_kind <- stated_kind & role_want != "criterion"
    if (any(misplaced_kind)) {
      sieve_import_stop(sprintf(
        paste(
          "A validity kind belongs to a criterion, and %s %s not %s in the specification: give",
          "%s the role criterion, or clear the kind cell."
        ),
        sieve_name_some(ids[misplaced_kind]), if (sum(misplaced_kind) == 1L) "is" else "are",
        if (sum(misplaced_kind) == 1L) "one" else "criteria",
        if (sum(misplaced_kind) == 1L) "it" else "them"
      ))
    }
    if (!"kind" %in% names(roles)) {
      roles$kind <- rep(NA_character_, nrow(roles))
    }
    at_role <- match(ids, roles$variable)
    roles$kind[at_role[stated_kind]] <- kind_low[stated_kind]
    roles$kind[at_role[role_want != "criterion"]] <- NA_character_
  }
  misplaced <- !is_item & want_reverse %in% TRUE
  if (any(misplaced)) {
    sieve_import_stop(sprintf(
      paste(
        "A variable that is not an item cannot be reverse scored: %s. Give it the role item in",
        "the specification, or clear its reverse cell."
      ), sieve_name_some(ids[misplaced])
    ))
  }

  # ---- Ledger fields -------------------------------------------------------
  rows <- which(is_item)
  before_fields <- meta
  label_txt <- col("label")
  wording_txt <- col("wording")
  facet_txt <- col("facet")
  # The value each listed item gets in each ledger column the specification
  # has. An empty cell means: the variable name for the Item Name, no
  # wording, Unassigned, no facet, and "as it is" for the type and the status.
  fields <- list(
    label = if (!is.null(label_txt)) ifelse(nzchar(label_txt), label_txt, ids),
    wording = if (!is.null(wording_txt)) ifelse(nzchar(wording_txt), wording_txt, NA_character_),
    type = if (!is.null(type_low)) ifelse(nzchar(type_low), type_low, as.character(meta$type[at])),
    construct = if (!is.null(cons)) ifelse(nzchar(cons), cons, "Unassigned"),
    facet = if (!is.null(facet_txt)) ifelse(nzchar(facet_txt), facet_txt, NA_character_),
    status = if (!is.null(status_low)) {
      ifelse(nzchar(status_low), unname(status_map[status_low]), as.character(meta$status[at]))
    }
  )
  for (field in names(fields)) {
    if (!is.null(fields[[field]]) && field %in% names(meta)) {
      meta[[field]][at[rows]] <- fields[[field]][rows]
    }
  }
  # Where each type the specification gives came from: its type_source
  # cell, or, when it has none, `type_source` (proposed unless the caller
  # says the types are the project's own choices). An item whose type cell
  # is empty keeps where its type came from.
  if (!is.null(type_low)) {
    typed_rows <- rows[nzchar(type_low[rows])]
    given <- if (is.null(source_low)) rep("", n) else source_low
    src <- ifelse(nzchar(given), given, type_source)
    for (s_val in unique(src[typed_rows])) {
      meta <- sieve_mark_type_source(meta, ids[typed_rows[src[typed_rows] == s_val]], s_val)
    }
  }
  orphan <- rows[!is.na(meta$facet[at[rows]]) & nzchar(meta$facet[at[rows]]) &
    (is.na(meta$construct[at[rows]]) | meta$construct[at[rows]] %in% c("", "Unassigned"))]
  if (length(orphan)) {
    sieve_import_stop(sprintf(
      "A facet needs a construct to belong to, and these items have a facet and no construct: %s.",
      sieve_name_some(ids[orphan])
    ))
  }
  changed_cells <- vapply(
    intersect(c("label", "wording", "type", "construct", "facet", "status"), names(meta)),
    function(field) {
      a <- as.character(before_fields[[field]])
      b <- as.character(meta[[field]])
      same <- (a == b) %in% TRUE | (is.na(a) & is.na(b))
      sum(!same)
    },
    integer(1)
  )

  # ---- Keying --------------------------------------------------------------
  steps <- list()
  reversed <- character(0)
  returned <- character(0)
  recorded <- character(0)
  bounds_kept <- character(0)
  unmatched <- character(0)
  unmatched_held <- character(0)
  unmatched_plain <- character(0)
  unstated <- character(0)
  contradicted <- list()
  label_low <- if (is.null(label_in)) rep("", n) else tolower(label_in)
  loaded_of <- function(it) {
    if (it %in% names(raw) && NROW(raw) == NROW(items)) raw[[it]] else NULL
  }
  for (j in rows) {
    it <- ids[j]
    i <- at[j]
    stated <- !is.na(want_reverse[j])
    has_bounds <- is.finite(lo_in[j]) && is.finite(hi_in[j])
    if (!stated && !has_bounds) {
      next
    }
    column <- items[[it]]
    loaded <- loaded_of(it)
    recorded_reverse <- isTRUE(as.logical(meta$reverse[i]))
    moved <- !is.null(loaded) && is.numeric(loaded) && is.numeric(column) &&
      !identical(as.numeric(column), as.numeric(loaded))
    in_session <- recorded_reverse || moved
    # A row that gives bounds and says nothing about the keying leaves the
    # keying as it is and declares the bounds.
    want <- if (stated) want_reverse[j] else recorded_reverse
    bounds_label <- if (nzchar(label_low[j])) label_low[j] else "declared"
    has_keys <- nzchar(key_analytic[j]) || nzchar(key_loaded[j])
    # What the specification's content keys show about the values now
    # loaded: a match of the values themselves, of the values as loaded, or
    # of their reflection within the bounds the specification gives, the
    # bounds stored with the item, or the observed range.
    seen <- if (is.numeric(column)) as.numeric(column)[is.finite(as.numeric(column))] else numeric(0)
    shown <- sieve_keying_evidence(
      column, loaded,
      want = want_reverse[j], loaded_rev = loaded_rev[j],
      key_analytic = key_analytic[j], key_loaded = key_loaded[j],
      bounds = list(
        specification = c(lo_in[j], hi_in[j]),
        stored = c(as.numeric(meta$rev_lo[i]), as.numeric(meta$rev_hi[i])),
        observed = if (length(seen)) c(min(seen), max(seen)) else c(NA_real_, NA_real_)
      )
    )
    if (isTRUE(shown$conflict)) {
      problems <- c(problems, sprintf(
        paste(
          "The content keys of the specification for %s contradict each other (the file was",
          "edited or damaged after it was written). Download the specification again from the",
          "session that made it, or clear the key_analytic and key_loaded cells of that row."
        ),
        it
      ))
      next
    }
    if (!stated) {
      holds <- recorded_reverse
    } else if (in_session) {
      # The session reversed or recorded the item: its own record says how
      # the values are keyed. The keys are still compared with it, and a
      # record the keys show to be wrong stops the import (see below).
      holds <- recorded_reverse
      if (!is.na(shown$holds) && !identical(isTRUE(shown$holds), recorded_reverse)) {
        contradicted[[it]] <- list(ledger = recorded_reverse, keys = isTRUE(shown$holds))
        next
      }
    } else if (!is.na(shown$holds)) {
      holds <- isTRUE(shown$holds)
    } else if (identical(keying, "administered")) {
      holds <- FALSE
    } else if (identical(keying, "reversed")) {
      holds <- want
    } else if (!has_keys && !is.na(applied[j])) {
      # A specification written by hand: its author's statement is all there is.
      holds <- applied[j]
    } else if (!has_keys && !isTRUE(want)) {
      # Written by hand, asks for no reversal and says nothing about the
      # data: there is nothing to tell and nothing to do.
      holds <- FALSE
    } else if (!has_keys) {
      unstated <- c(unstated, it)
      next
    } else if (isTRUE(want)) {
      unmatched <- c(unmatched, it)
      next
    } else if (isTRUE(loaded_rev[j]) || isTRUE(applied[j])) {
      unmatched_held <- c(unmatched_held, it)
      next
    } else {
      # The keys match nothing, whatever the reverse cell says: the values
      # are not the ones the specification knows, so it cannot confirm that
      # they hold no reversal made since it was saved.
      unmatched_plain <- c(unmatched_plain, it)
      next
    }

    # What has to be done to this item, settled before anything is done.
    record_it <- FALSE
    store_bounds <- FALSE
    if (in_session) {
      # The bounds of a reversal in force stay (a reflection made in the
      # session, or a reversal as loaded whose keying record states them).
      # An item that does not hold them (none stored, or bounds typed later
      # for a reversal as loaded whose record gave none) takes the
      # specification's, as its Response bounds cell would
      # (sieve_holds_reversal_bounds(), sieve_declare_bounds()), so that a
      # return reflects within the scale.
      stored <- c(as.numeric(meta$rev_lo[i]), as.numeric(meta$rev_hi[i]))
      # An item the session returned to its scoring keeps the bounds it was
      # returned within when both are stored.
      held_it <- if (recorded_reverse) {
        sieve_holds_reversal_bounds(meta[i, , drop = FALSE], it, items, raw, transforms, dataset_id)
      } else {
        all(is.finite(stored))
      }
      if (has_bounds && !held_it) {
        outside <- if (is.numeric(column)) sieve_outside_bounds(column, lo_in[j], hi_in[j]) else integer(0)
        if (length(outside)) {
          problems <- c(problems, sprintf(
            paste(
              "%d value%s of item \"%s\" %s outside the response bounds %s to %s the specification",
              "gives for it, so nothing was changed."
            ),
            length(outside), if (length(outside) == 1L) "" else "s", it,
            if (length(outside) == 1L) "lies" else "lie", sieve_num_text(lo_in[j]), sieve_num_text(hi_in[j])
          ))
          next
        }
        store_bounds <- TRUE
      } else if (has_bounds && !(identical(lo_in[j], stored[1]) && identical(hi_in[j], stored[2]))) {
        bounds_kept <- c(bounds_kept, it)
      }
      reflect_it <- !identical(holds, want)
    } else if (isTRUE(holds)) {
      record_it <- TRUE
      reflect_it <- !isTRUE(want)
    } else {
      store_bounds <- has_bounds
      reflect_it <- isTRUE(want)
    }
    if (record_it) {
      # The bounds the reversal was made within: the specification's, or
      # when it gives none, the bounds under which the reflection of the
      # values matched the specification's key.
      mark_lo <- if (has_bounds) lo_in[j] else shown$lo
      mark_hi <- if (has_bounds) hi_in[j] else shown$hi
      mark_label <- if (has_bounds || identical(shown$bounds, "specification")) {
        bounds_label
      } else if (identical(shown$bounds, "stored")) {
        stored_label <- as.character(meta$rev_bounds[i])
        if (is.na(stored_label) || !stored_label %in% c("declared", "observed")) "declared" else stored_label
      } else {
        "observed"
      }
      mark <- tryCatch(
        sieve_mark_reversed(items, meta, it, mark_lo, mark_hi, mark_label),
        sieve_transform_error = function(e) e
      )
      if (inherits(mark, "sieve_transform_error")) {
        problems <- c(problems, conditionMessage(mark))
        next
      }
      meta <- mark$meta
      steps[[length(steps) + 1L]] <- mark
      recorded <- c(recorded, it)
    }
    if (store_bounds) {
      meta$rev_lo[i] <- lo_in[j]
      meta$rev_hi[i] <- hi_in[j]
      meta$rev_bounds[i] <- bounds_label
    }
    if (reflect_it) {
      step <- tryCatch(
        sieve_reverse_item(items, meta, it, loaded = loaded, held = sieve_removed_values(removed, it)),
        sieve_transform_error = function(e) e
      )
      if (inherits(step, "sieve_transform_error")) {
        problems <- c(problems, conditionMessage(step))
        next
      }
      items <- step$data
      meta <- step$meta
      steps[[length(steps) + 1L]] <- step
      if (isTRUE(want)) {
        reversed <- c(reversed, it)
      } else {
        returned <- c(returned, it)
      }
    }
  }
  choose <- paste(
    "Under \"When a specification is restored\", choose whether the loaded data hold these items",
    "as administered (they are reverse scored now) or already reverse scored (they are not",
    "reversed again), and restore the specification again."
  )
  them <- function(x) if (length(x) == 1L) "it" else "them"
  if (length(unmatched)) {
    problems <- c(problems, sprintf(
      paste(
        "The specification marks %s as reverse keyed, but the values now loaded for %s are not",
        "the values of either data file the specification was saved with, so it cannot be told",
        "whether they already hold the reversal. %s"
      ),
      sieve_name_some(unmatched), them(unmatched), choose
    ))
  }
  if (length(unmatched_held)) {
    problems <- c(problems, sprintf(
      paste(
        "The data the specification was saved with held %s reverse scored, and the values now",
        "loaded for %s are not the values of either data file it was saved with, so it cannot",
        "be told how they are keyed. %s"
      ),
      sieve_name_some(unmatched_held), them(unmatched_held), choose
    ))
  }
  if (length(unstated)) {
    problems <- c(problems, sprintf(
      paste(
        "The specification marks %s as reverse keyed and does not say whether the data file",
        "already holds the reversal (it has no reverse_applied entry for %s). %s"
      ),
      sieve_name_some(unstated), them(unstated), choose
    ))
  }
  if (length(unmatched_plain)) {
    problems <- c(problems, sprintf(
      paste(
        "The specification lists %s as not reverse keyed, but the values now loaded for %s are",
        "not the values of either data file it was saved with, so it cannot confirm that they",
        "hold no reversal made since it was saved. %s"
      ),
      sieve_name_some(unmatched_plain), them(unmatched_plain), choose
    ))
  }
  for (it in names(contradicted)) {
    said <- contradicted[[it]]
    problems <- c(problems, sprintf(
      paste(
        "The item ledger of this session says %s is %s, but the specification's content keys",
        "show that its values now are the ones it records as %s. The record was made when the",
        "file was loaded with a keying its values do not have. Load the data file again, so",
        "that every value is as read and nothing is recorded for it, and restore the",
        "specification again."
      ),
      it,
      if (isTRUE(said$ledger)) "reverse scored" else "as administered (not reverse scored)",
      if (isTRUE(said$keys)) "reverse scored" else "administered (not reverse scored)"
    ))
  }
  if (length(problems)) {
    sieve_import_stop(problems)
  }

  blank_label <- is.na(meta$label) | !nzchar(meta$label)
  meta$label[blank_label] <- meta$item[blank_label]
  blank_construct <- is.na(meta$construct) | !nzchar(meta$construct)
  meta$construct[blank_construct] <- "Unassigned"

  # The five-option rule: an item the specification types ordered that has
  # five or more response options (with its declared bounds, which are stored)
  # is read as continuous when its type is proposed; one the specification
  # records as chosen is kept; a binary item and one with four or fewer
  # options keep the specification's type.
  switched <- character(0)
  switch_options <- integer(0)
  kept_chosen <- character(0)
  if (!is.null(min_options) && !is.null(type_low)) {
    rule <- sieve_option_rule(items, meta,
      candidates = ids[is_item & type_low %in% "ordered"],
      min_options = min_options
    )
    switched <- rule$switched
    kept_chosen <- rule$kept
    switch_options <- rule$options[switched]
    meta$type[match(switched, meta$item)] <- "continuous"
  }

  # Whether the specification types every item of the pool it leaves: a
  # study whose specification does is confirmed from it (the estimator
  # decision is in the specification), on the Ingest tab when the data and
  # the specification arrive together, and by sieve_prepare_study().
  typed <- if (is.null(type_low)) {
    character(0)
  } else {
    ids[is_item & type_low %in% c("continuous", "ordered", "binary")]
  }
  untyped <- setdiff(meta$item, typed)

  list(
    items = items,
    meta = meta,
    roles = roles,
    id_vars = roles$variable[roles$role != "item"],
    held = held,
    steps = steps,
    pool_changed = any(role_changes$from == "item" | role_changes$to == "item"),
    summary = list(
      rows = n,
      variables = ncol(raw),
      not_listed = setdiff(names(raw), ids),
      role_changes = role_changes,
      fields = changed_cells,
      reversed = reversed,
      returned = returned,
      recorded = recorded,
      bounds_kept = bounds_kept,
      keying = keying,
      typed_all = !length(untyped),
      untyped = untyped,
      switched = switched,
      switch_options = switch_options,
      kept_chosen = kept_chosen
    )
  )
}

#' What an import did, in sentences for the interface
#'
#' @param summary The `summary` element from `sieve_spec_import()`.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_spec_message <- function(summary) {
  parts <- sprintf(
    "Specification restored for %d of the %d variables in the data.",
    summary$rows, summary$variables
  )
  if (nrow(summary$role_changes)) {
    parts <- c(parts, sprintf(
      "Roles changed: %s.",
      paste(sprintf("%s to %s", summary$role_changes$variable, summary$role_changes$to), collapse = "; ")
    ))
  }
  if (length(summary$reversed)) {
    parts <- c(parts, sprintf(
      "Reverse scored now, because the loaded values did not hold the reversal: %s.",
      sieve_name_some(summary$reversed)
    ))
  }
  if (length(summary$recorded)) {
    parts <- c(parts, sprintf(
      "Recorded as already reverse scored in the loaded data, with no value changed: %s.",
      sieve_name_some(summary$recorded)
    ))
  }
  if (length(summary$returned)) {
    parts <- c(parts, sprintf(
      "Returned to the administered direction: %s.",
      sieve_name_some(summary$returned)
    ))
  }
  if (length(summary$switched)) {
    parts <- c(parts, sieve_option_rule_text(summary$switched, "the specification"))
  }
  if (length(summary$kept_chosen)) {
    parts <- c(parts, sprintf(
      "Kept ordered because the specification records the type as chosen: %s.",
      sieve_name_some(summary$kept_chosen)
    ))
  }
  if (length(summary$bounds_kept)) {
    parts <- c(parts, sprintf(
      "The bounds in the specification were not used for %s: a reversal in force keeps the bounds it was made with.",
      sieve_name_some(summary$bounds_kept)
    ))
  }
  if (length(summary$not_listed)) {
    parts <- c(parts, sprintf(
      "%d variable%s in the data %s not in the specification and %s left as %s: %s.",
      length(summary$not_listed), if (length(summary$not_listed) == 1L) "" else "s",
      if (length(summary$not_listed) == 1L) "is" else "are",
      if (length(summary$not_listed) == 1L) "was" else "were",
      if (length(summary$not_listed) == 1L) "it was" else "they were",
      sieve_name_some(summary$not_listed)
    ))
  }
  paste(parts, collapse = " ")
}

#' Restore a specification in the shared state
#'
#' Reads the file and tries the import with `sieve_spec_import()`. When the
#' specification is refused, the reasons are returned and the state is not
#' touched. When it can be applied, the item data, the ledger, the roles
#' table and the non item columns are written together, the confirmation of
#' the item types is withdrawn (the types and the item pool may have
#' changed), the import is logged, and each reversal or keying record is
#' appended to `state$transforms` and logged with the R that reproduces it.
#'
#' @param state The shared state store.
#' @param file The path of the specification file.
#' @param keying Passed to `sieve_spec_import()`.
#' @param name The file name to show in the log.
#' @param min_options Passed to `sieve_spec_import()` (the five-option
#'   rule; `NULL` keeps the specification's types).
#' @return Invisibly, a list with `ok`; when `ok` is `FALSE`, `problems`
#'   (the reasons); when `TRUE`, `summary`, `message`, and `records`.
#' @author Ken Kelley
#' @noRd
sieve_apply_spec <- function(state, file, keying = "verify", name = basename(file), min_options = NULL) {
  spec <- tryCatch(sieve_spec_read(file), sieve_import_error = function(e) e)
  if (inherits(spec, "sieve_import_error")) {
    return(invisible(list(ok = FALSE, problems = spec$problems)))
  }
  sieve_apply_spec_table(state, spec, keying = keying, name = name, min_options = min_options)
}

#' Restore a specification that is already read, in the shared state
#'
#' The part of `sieve_apply_spec()` after the file is read, for a
#' specification that arrives as a table (the pre-SIEVER step takes a data
#' frame as well as a file).
#'
#' @param state The shared state store.
#' @param spec A specification table (character columns, as
#'   `sieve_spec_read()` returns).
#' @param keying Passed to `sieve_spec_import()`.
#' @param name The name to show in the log.
#' @param min_options Passed to `sieve_spec_import()`.
#' @param type_source Passed to `sieve_spec_import()`.
#' @return As `sieve_apply_spec()`.
#' @author Ken Kelley
#' @noRd
sieve_apply_spec_table <- function(state, spec, keying = "verify", name = "the specification",
                                   min_options = NULL, type_source = "proposed") {
  shiny::isolate({
    out <- tryCatch(
      sieve_spec_import(spec, state$raw, state$items, state$meta, state$roles,
        keying = keying, held = state$held_rows %||% list(),
        min_options = min_options, removed = state$removed_cases %||% list(),
        type_source = type_source, transforms = state$transforms %||% list(),
        dataset_id = state$dataset_id
      ),
      sieve_import_error = function(e) e
    )
    if (inherits(out, "sieve_import_error")) {
      return(invisible(list(ok = FALSE, problems = out$problems)))
    }
    sieve_ensure_dataset(state)
    before <- sieve_revision(state)
    state$items <- out$items
    sieve_write_ledger(state, out$meta)
    state$roles <- out$roles

    state$held_rows <- out$held
    state$confirmed <- FALSE
    after <- sieve_revision(state)
    s <- out$summary
    sieve_log(
      state,
      sprintf("Restore the specification from %s (matched by variable name).", name),
      c(
        "# Item names, wordings, types, constructs, facets and included status were set from the saved file.",
        if (nrow(s$role_changes)) {
          sprintf(
            "# variable role: %s is %s", encodeString(s$role_changes$variable, quote = "\""),
            s$role_changes$to
          )
        },
        if (length(s$recorded)) {
          sprintf(
            "# already reverse scored in the data as read (no value changed): %s",
            paste(s$recorded, collapse = ", ")
          )
        },
        if (length(s$switched)) {
          c(
            sprintf(
              paste(
                "# Typed ordered in the specification and read as continuous by the five-option",
                "rule (Ingest tab, step 1; Rhemtulla, Brosseau-Liard & Savalei, 2012): %d item%s."
              ),
              length(s$switched), if (length(s$switched) == 1L) "" else "s"
            ),
            sprintf("#   %s (%d options): continuous", s$switched, s$switch_options)
          )
        },
        if (length(s$kept_chosen)) {
          sprintf(
            "# Kept ordered because the specification records the type as chosen: %s.",
            paste(s$kept_chosen, collapse = ", ")
          )
        },
        if (length(c(s$reversed, s$returned))) {
          "# The reversals this specification asked for are the steps that follow."
        }
      )
    )
    records <- sieve_record_steps(state, out$steps,
      origin = "specification",
      before = before, after = after
    )
    invisible(list(ok = TRUE, summary = s, message = sieve_spec_message(s), records = records))
  })
}

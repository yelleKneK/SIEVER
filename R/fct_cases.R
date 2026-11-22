# fct_cases.R
# Respondents removed from the analysis, and items excluded from it, from the
# Screening tab. A respondent flagged by a careless-responding screen (or any
# other respondent the researcher names) can be removed from the analysis
# with a stated reason: the row leaves the data as loaded (state$raw) and the
# item data (state$items) together, so the two stay aligned, and is held,
# with its original row number, in state$removed_cases, from which it can be
# restored to its original position. state$case_index holds the original row
# number of every remaining row. Every tab reads state$items, so every tab
# follows; the shared correlation basis is keyed by the data content, and
# results bound to a revision become stale. Nothing is deleted.
#
# The item exclusion helper, sieve_exclude_items(), sets the ledger status
# that the Ingest grid's Use cell sets ("parked", shown as excluded), records
# the reason in the ledger column exclude_reason, and logs the step.

# ---- Identifiers and row numbers -----------------------------------------------

#' The identifier column of the data, if any
#'
#' The first column whose role is identifier in the roles table of the
#' loaded data (see `sieve_roles_table()`).
#'
#' @param state The shared state store (read where called, so a reactive
#'   that calls this follows a change of roles; wrap the call in
#'   [shiny::isolate()] to read without a dependency).
#' @return A column name, or `NA_character_` when no column is an identifier.
#' @author Ken Kelley
#' @noRd
sieve_case_id_variable <- function(state) {
  raw <- state$raw
  if (is.null(raw)) {
    return(NA_character_)
  }
  roles <- sieve_roles_table(raw, state$items, state$roles)
  ids <- if (is.data.frame(roles)) as.character(roles$variable[roles$role == "identifier"]) else character(0)
  ids <- ids[ids %in% names(raw)]
  if (length(ids)) ids[1] else NA_character_
}

#' Identifier values as text
#'
#' A number is written with as few digits as identify it, a factor through
#' its labels, and anything else as text, without dispatching on the
#' column's class (an identifier read from an SPSS file is a labelled
#' column).
#'
#' @param x An identifier column (or part of one).
#' @return A character vector, `NA` where the value is missing.
#' @author Ken Kelley
#' @noRd
sieve_case_id_text <- function(x) {
  if (is.factor(x)) {
    return(as.character(x))
  }
  v <- unclass(x)
  attributes(v) <- NULL
  if (is.numeric(v)) {
    out <- sieve_num_text(v)
    out[is.na(v)] <- NA_character_
    return(out)
  }
  as.character(v)
}

#' The original row number of every row in the analysis
#'
#' `state$case_index` when respondents have been removed; otherwise the row
#' numbers of the data as loaded. Nothing is written to the state.
#'
#' @param state The shared state store (read where called; wrap the call in
#'   [shiny::isolate()] to read without a dependency).
#' @return An integer vector, one per row of `state$raw`.
#' @author Ken Kelley
#' @noRd
sieve_case_index <- function(state) {
  raw <- state$raw
  n <- NROW(raw)
  index <- state$case_index
  if (is.null(index) || length(index) != n) {
    return(seq_len(n))
  }
  as.integer(index)
}

#' The respondents in the analysis: original row number and identifier
#'
#' @param state The shared state store (read where called, like
#'   `sieve_case_index()`).
#' @return A data frame with one row per row of `state$raw`: `row` (the row
#'   of the data as loaded) and `id` (the identifier, as text, or `NA` when
#'   the data have no identifier column). The attribute `variable` names
#'   the identifier column (`NA` when there is none).
#' @author Ken Kelley
#' @noRd
sieve_case_ids <- function(state) {
  raw <- state$raw
  index <- sieve_case_index(state)
  var <- sieve_case_id_variable(state)
  id <- if (!is.na(var)) sieve_case_id_text(raw[[var]]) else rep(NA_character_, length(index))
  out <- data.frame(row = index, id = id, stringsAsFactors = FALSE)
  attr(out, "variable") <- var
  out
}

#' Respondents named in a box: identifiers or row numbers
#'
#' Entries are separated by commas, semicolons, or white space. With an
#' identifier column, an entry is an identifier of a respondent in the
#' analysis, and a row of the data as loaded is named explicitly with a
#' leading `#` (`#17`); an entry that matches no identifier is never read
#' as a row, so a mistyped identifier that happens to be a valid row number
#' removes no one rather than removing that row in silence. Without an
#' identifier column every entry is a row, with or without the `#`.
#'
#' @param text What was typed.
#' @param ids The result of `sieve_case_ids()`.
#' @return A list with `rows` (the original row numbers found, in the order
#'   typed, without repeats), `unmatched` (the entries that name no
#'   respondent in the analysis), `resolved` (a data frame with one row per
#'   entry: `entry`, `row`, `id` and `how`, one of `"identifier"`, `"row"`
#'   or `"not found"`), and `hint` (a sentence for an unmatched entry that
#'   is a row number of a respondent in the analysis, saying how to name
#'   that row, or `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_case_parse <- function(text, ids) {
  entries <- unlist(strsplit(paste(text %||% "", collapse = " "), "[,;[:space:]]+"))
  entries <- unique(entries[nzchar(entries)])
  has_id <- !all(is.na(ids$id)) && !is.na(attr(ids, "variable") %||% NA_character_)
  as_row <- function(x) {
    number <- suppressWarnings(as.numeric(x))
    if (length(number) == 1L && is.finite(number) && number == trunc(number)) which(ids$row == as.integer(number)) else integer(0)
  }
  resolved <- lapply(entries, function(e) {
    explicit <- startsWith(e, "#")
    at <- if (explicit) {
      as_row(substring(e, 2L))
    } else {
      found <- which(!is.na(ids$id) & ids$id == e)
      if (!length(found) && !has_id) as_row(e) else found
    }
    how <- if (!length(at)) "not found" else if (explicit || !has_id) "row" else "identifier"
    data.frame(
      entry = e, row = if (length(at)) ids$row[at[[1L]]] else NA_integer_,
      id = if (length(at)) ids$id[at[[1L]]] else NA_character_, how = how,
      stringsAsFactors = FALSE
    )
  })
  resolved <- if (length(resolved)) do.call(rbind, resolved) else
    data.frame(entry = character(0), row = integer(0), id = character(0), how = character(0), stringsAsFactors = FALSE)
  unmatched <- resolved$entry[resolved$how == "not found"]
  # An unmatched entry that is the row number of a respondent in the
  # analysis: say how to name that row instead of removing it.
  numbers <- if (has_id) unmatched[!startsWith(unmatched, "#") & vapply(unmatched, function(e) length(as_row(e)) > 0L, logical(1))] else character(0)
  hint <- if (length(numbers)) {
    sprintf(
      "%s %s not %s of %s; to name a row of the data as loaded, type it with a leading # (for example #%s).",
      sieve_name_some(numbers), if (length(numbers) == 1L) "is" else "are",
      if (length(numbers) == 1L) "an identifier" else "identifiers", attr(ids, "variable"), numbers[[1L]]
    )
  }
  list(
    rows = unique(as.integer(resolved$row[resolved$how != "not found"])),
    unmatched = unmatched, resolved = resolved, hint = hint
  )
}

#' How the entries typed for removal resolve, in one sentence
#'
#' Shown under the box as it is typed, before anything is removed: each
#' entry with the respondent it names (identifier and row of the data as
#' loaded), and the entries that name no one.
#'
#' @param parsed A result of `sieve_case_parse()`.
#' @param variable The identifier column, or `NA`.
#' @return One string, or `NULL` when nothing is typed.
#' @author Ken Kelley
#' @noRd
sieve_case_parse_text <- function(parsed, variable = NA_character_) {
  r <- parsed$resolved
  if (is.null(r) || !nrow(r)) {
    return(NULL)
  }
  found <- r[r$how != "not found", , drop = FALSE]
  named <- if (nrow(found)) {
    vapply(seq_len(nrow(found)), function(i) {
      if (!is.na(found$id[[i]]) && !is.na(variable)) {
        sprintf("%s %s (row %d)", variable, found$id[[i]], found$row[[i]])
      } else {
        sprintf("row %d", found$row[[i]])
      }
    }, character(1))
  } else character(0)
  paste0(
    if (length(named)) sprintf("Remove would take out: %s.", paste(unique(named), collapse = ", ")) else "Remove would take out no one typed here.",
    if (length(parsed$unmatched)) sprintf(" Not found, so not removed: %s.", sieve_name_some(parsed$unmatched)) else "",
    if (length(parsed$hint)) paste0(" ", parsed$hint) else ""
  )
}

# ---- Rows of a data frame, without dispatching on column classes -----------

#' One column's values at some positions, with its attributes kept
#'
#' The class is dropped while indexing and put back afterwards, so a
#' labelled column read from an SPSS file, a factor, or a date keeps what it
#' is whatever packages are loaded.
#'
#' @param x A column.
#' @param i Positions.
#' @return The column at those positions.
#' @author Ken Kelley
#' @noRd
sieve_cases_column <- function(x, i) {
  if (!is.atomic(x) || !is.null(dim(x))) {
    return(x[i])
  }
  at <- attributes(x)
  at$names <- NULL
  v <- unclass(x)
  attributes(v) <- NULL
  v <- v[i]
  attributes(v) <- at
  v
}

#' Two columns' values one after the other, with the first one's attributes
#'
#' @param a,b Columns of the same variable (from the same data, or the
#'   same variable before and after a change of storage).
#' @return One column.
#' @author Ken Kelley
#' @noRd
sieve_cases_join <- function(a, b) {
  if (!is.atomic(a) || !is.null(dim(a)) || !is.atomic(b) || !is.null(dim(b))) {
    return(c(a, b))
  }
  at <- attributes(a)
  at$names <- NULL
  va <- unclass(a)
  attributes(va) <- NULL
  vb <- unclass(b)
  attributes(vb) <- NULL
  v <- c(va, vb)
  attributes(v) <- at
  v
}

#' The row names a data frame gets for some of its rows
#'
#' Compact (automatic) row names when the result's names would be 1 to n,
#' as they are for a tibble; otherwise the original names of those rows.
#'
#' @param rn The full row names (integer or character).
#' @param tibble Whether the data frame is a tibble.
#' @return Row names, as `attr(x, "row.names")` takes them.
#' @author Ken Kelley
#' @noRd
sieve_cases_row_names <- function(rn, tibble = FALSE) {
  n <- length(rn)
  if (tibble || (is.numeric(rn) && identical(as.integer(rn), seq_len(n)))) {
    return(.set_row_names(n))
  }
  rn
}

#' Some rows of a data frame, every column kept as it is stored
#'
#' @param df A data frame (or tibble).
#' @param i Row positions.
#' @return A data frame of the same class with those rows.
#' @author Ken Kelley
#' @noRd
sieve_cases_take <- function(df, i) {
  cols <- lapply(df, sieve_cases_column, i = i)
  at <- attributes(df)
  at$names <- names(df)
  at$row.names <- sieve_cases_row_names(attr(df, "row.names")[i], inherits(df, "tbl_df"))
  attributes(cols) <- at
  cols
}

#' Two data frames' rows one after the other
#'
#' The columns of `a`, in its order; a column `b` does not have is missing
#' in its rows (the caller fills it).
#'
#' @param a,b Data frames of the same data.
#' @return A data frame of the class of `a`.
#' @author Ken Kelley
#' @noRd
sieve_cases_bind <- function(a, b) {
  nb <- nrow(b)
  cols <- lapply(names(a), function(v) {
    tail <- if (v %in% names(b)) b[[v]] else sieve_cases_column(a[[v]], rep(NA_integer_, nb))
    sieve_cases_join(a[[v]], tail)
  })
  at <- attributes(a)
  at$names <- names(a)
  rn <- c(attr(a, "row.names"), attr(b, "row.names"))
  at$row.names <- sieve_cases_row_names(rn, inherits(a, "tbl_df"))
  attributes(cols) <- at
  cols
}

# ---- The ledger after a removal or a restore ----------------------------------

#' A data frame's attributes in the order they had before an edit
#'
#' `$<-` and `[<-` on a data frame move its class attribute after its row
#' names. The values are the same, but the bytes the content keys of the
#' session's input revisions are computed from are not, so a restore that
#' returns every respondent would not give back the ledger and the roles
#' table exactly as they were. The attributes are put back in their earlier
#' order, the row names in their compact form when they had it.
#'
#' @param x The edited data frame.
#' @param order The names of its attributes before the edit.
#' @return `x`, with its attributes in that order.
#' @author Ken Kelley
#' @noRd
sieve_cases_attribute_order <- function(x, order) {
  at <- attributes(x)
  # attributes() writes compact row names out; their stored form is kept.
  at$row.names <- .row_names_info(x, 0L)
  attributes(x) <- at[c(intersect(order, names(at)), setdiff(names(at), order))]
  x
}

#' Describe the items and the other columns from the rows in the analysis
#'
#' The ledger keeps per item summaries (`mean`, `sd`, `min`, `max`, the
#' number of missing responses and of distinct values) that describe the
#' data in the analysis; the roles table keeps the number of missing and
#' distinct values of every column. After respondents leave or return, they
#' are computed again from the rows that remain, with the expressions the
#' ledger is built with (`sieve_item_summary()`, `sieve_build_meta()`).
#' Ledger rows held aside for columns that left the item pool are
#' described from the data as loaded, which is what such a column holds.
#'
#' @param state The shared state store (written).
#' @return The state, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_cases_describe <- function(state) {
  shiny::isolate({
    items <- state$items
    raw <- state$raw
    describe <- function(row, x) {
      now <- sieve_item_summary(sieve_screen_numeric(x))
      for (field in intersect(names(now), names(row))) {
        row[[field]] <- now[[field]]
      }
      if ("n_missing" %in% names(row)) row$n_missing <- sum(is.na(x))
      if ("n_levels" %in% names(row)) row$n_levels <- length(unique(x[!is.na(x)]))
      row
    }
    m <- state$meta
    if (is.data.frame(m) && nrow(m)) {
      before <- names(attributes(m))
      for (i in which(m$item %in% names(items))) {
        m[i, ] <- describe(m[i, , drop = FALSE], items[[m$item[i]]])
      }
      sieve_write_ledger(state, sieve_cases_attribute_order(m, before))
    }
    held <- state$held_rows
    if (is.list(held) && length(held) && !is.data.frame(held)) {
      for (v in intersect(names(held), names(raw))) {
        if (is.data.frame(held[[v]]) && nrow(held[[v]]) == 1L) {
          held[[v]] <- sieve_cases_attribute_order(describe(held[[v]], raw[[v]]), names(attributes(held[[v]])))
        }
      }
      state$held_rows <- held
    }
    roles <- state$roles
    if (is.data.frame(roles) && all(c("variable", "n_distinct", "n_missing") %in% names(roles))) {
      before <- names(attributes(roles))
      at <- roles$variable %in% names(raw)
      roles$n_missing[at] <- vapply(roles$variable[at], function(v) sum(is.na(raw[[v]])), integer(1),
        USE.NAMES = FALSE
      )
      roles$n_distinct[at] <- vapply(roles$variable[at],
        function(v) length(unique(raw[[v]][!is.na(raw[[v]])])),
        integer(1),
        USE.NAMES = FALSE
      )
      state$roles <- sieve_cases_attribute_order(roles, before)
    }
    invisible(state)
  })
}

# ---- Removing and restoring respondents --------------------------------------

#' The item data views a recorded removal keeps in step
#'
#' The data as read (`d`) and every name under which the recorded steps
#' read the item data with one row per respondent (see
#' `sieve_item_views()`).
#'
#' @return A character vector of object names.
#' @author Ken Kelley
#' @noRd
sieve_cases_views <- function() {
  c("d", "items", sieve_item_views())
}

#' Who a set of removed respondents is, in words
#'
#' @param rows Original row numbers.
#' @param ids Identifiers (text, `NA` where there is none).
#' @param var The identifier column, or `NA`.
#' @param most How many to name.
#' @return One phrase.
#' @author Ken Kelley
#' @noRd
sieve_cases_who <- function(rows, ids, var, most = 10L) {
  n <- length(rows)
  shown <- seq_len(min(n, most))
  more <- if (n > most) sprintf(", and %d more", n - most) else ""
  row_words <- sprintf(
    "row%s %s of the data as loaded", if (n == 1L) "" else "s",
    paste0(paste(rows[shown], collapse = ", "), more)
  )
  if (is.na(var) || all(is.na(ids))) {
    return(row_words)
  }
  sprintf("%s %s (%s)", var, paste0(
    paste(ifelse(is.na(ids[shown]), "(none)", ids[shown]), collapse = ", "),
    more
  ), row_words)
}

#' The R that removes respondents in a replay
#'
#' The rows leave the data as read (`d`) and every view of the item data
#' with one row per respondent, together, and are kept in `removed_cases`
#' (one data frame per view, and their rows of the data as read in
#' `removed_cases$case_index`), so that a later step can put them back;
#' `case_index` is the row of the data as read held by each row of `d`. The
#' respondents are found by their rows of the data as read (`case_index`,
#' which the session script binds with each recorded input revision), never
#' by an identifier, so the script carries no respondent identifier.
#' The copy of the values as read that a reversal keeps (`values_as_read`)
#' loses the same rows.
#'
#' @param rows Original row numbers.
#' @param ids Their identifiers (text); kept for callers, not written.
#' @param var The identifier column; kept for callers, not used: the rows
#'   are found by `case_index`.
#' @param numeric,id_values Kept for callers, not used.
#' @param reason The reason given, carried as a comment (the Reproducible
#'   Code tab says so).
#' @return A character vector of R source lines.
#' @author Ken Kelley
#' @noRd
sieve_cases_remove_code <- function(rows, ids, var, numeric = FALSE, id_values = NULL, reason = "") {
  views <- paste(encodeString(sieve_cases_views(), quote = "\""), collapse = ", ")
  find <- sprintf("out <- case_index %%in%% c(%s)", paste(sprintf("%dL", as.integer(rows)), collapse = ", "))
  c(
    sieve_comment_lines("Reason:", reason),
    "# The rows leave the data as read (d) and every view of the item data with one",
    "# row per respondent, together; removed_cases keeps them, so a later step can",
    "# put them back, and case_index is the row of the data as read in each row of d.",
    "if (!exists(\"case_index\")) case_index <- seq_len(nrow(d))",
    "if (!exists(\"removed_cases\")) removed_cases <- list(case_index = integer(0))",
    find,
    sprintf("for (view in intersect(c(%s), ls())) {", views),
    "  held <- get(view)",
    "  if (NROW(held) == length(out)) {",
    "    removed_cases[[view]] <- rbind(removed_cases[[view]], held[out, , drop = FALSE])",
    "    assign(view, held[!out, , drop = FALSE])",
    "  } else {",
    "    warning(view, \" does not have one row per respondent of d, so it was left as it is.\", call. = FALSE)",
    "  }",
    "}",
    "if (exists(\"values_as_read\")) {",
    "  values_as_read <- lapply(values_as_read, function(v) if (length(v) == length(out)) v[!out] else v)",
    "}",
    "removed_cases$case_index <- c(removed_cases$case_index, case_index[out])",
    "case_index <- case_index[!out]"
  )
}

#' The R that restores removed respondents in a replay
#'
#' The kept rows return to `d` and to every view that lost them, each at its
#' row of the data as read. A reversal recorded after a respondent was
#' removed did not reach the kept rows (the recorded reversal changes the
#' views, not `removed_cases`), so the same change is made to them first,
#' in the views the recorded reversal reaches (the data frames that hold
#' the item): each such reversal is written out for the rows it concerns.
#'
#' @param rows Original row numbers restored.
#' @param fixes A list of changes to the kept rows, each a list with
#'   `item`, `rows` (the original rows it concerns), `applied` (as in a
#'   transformation record), `lo`, and `hi`.
#' @return A character vector of R source lines.
#' @author Ken Kelley
#' @noRd
sieve_cases_restore_code <- function(rows, fixes = list()) {
  lit_rows <- function(r) paste(sprintf("%dL", as.integer(r)), collapse = ", ")
  fix_lines <- unlist(lapply(fixes, function(f) {
    it <- encodeString(f$item, quote = "\"")
    value <- if (identical(f$applied, FALSE)) {
      sprintf("removed_cases$d[[%s]][fix]", it)
    } else if (identical(f$applied, TRUE)) {
      sprintf("(%s + %s) - as.numeric(removed_cases$d[[%s]][fix])", sieve_num_lit(f$lo), sieve_num_lit(f$hi), it)
    } else {
      sprintf("(%s + %s) - removed_cases[[view]][[%s]][fix]", sieve_num_lit(f$lo), sieve_num_lit(f$hi), it)
    }
    what <- if (identical(f$applied, FALSE)) {
      "returned to its values as read"
    } else {
      sprintf("reverse scored within %s to %s", sieve_num_text(f$lo), sieve_num_text(f$hi))
    }
    c(
      sprintf(
        "# %s was %s after these respondents were removed; their kept rows take the same change.",
        f$item, what
      ),
      sprintf("fix <- removed_cases$case_index %%in%% c(%s)", lit_rows(f$rows)),
      "for (view in setdiff(names(removed_cases), c(\"case_index\", \"d\"))) {",
      sprintf("  if (%s %%in%% names(removed_cases[[view]])) {", it),
      sprintf("    removed_cases[[view]][[%s]][fix] <- %s", it, value),
      "  }",
      "}"
    )
  }))
  c(
    "# The kept rows return to the data as read (d) and to every view of the item",
    "# data that lost them, each at its row of the data as read.",
    sprintf("back <- removed_cases$case_index %%in%% c(%s)", lit_rows(rows)),
    "if (!any(back)) {",
    "  stop(\"These respondents are not among those removed earlier in this script.\", call. = FALSE)",
    "}",
    fix_lines,
    "rows <- c(case_index, removed_cases$case_index[back])",
    "for (view in setdiff(names(removed_cases), \"case_index\")) {",
    "  if (view %in% ls() && NROW(get(view)) == length(case_index)) {",
    "    held <- rbind(get(view), removed_cases[[view]][back, , drop = FALSE])",
    "    assign(view, held[order(rows), , drop = FALSE])",
    "  }",
    "  removed_cases[[view]] <- removed_cases[[view]][!back, , drop = FALSE]",
    "}",
    "case_index <- sort(rows)",
    "removed_cases$case_index <- removed_cases$case_index[!back]",
    "if (exists(\"values_as_read\")) {",
    "  for (it in intersect(names(values_as_read), names(d))) values_as_read[[it]] <- d[[it]]",
    "}"
  )
}

#' Remove respondents from the analysis
#'
#' The rows leave the data as loaded (`state$raw`) and the item data
#' (`state$items`) together, so the two stay aligned, and each is held in
#' `state$removed_cases` with its original row number, identifier, the
#' screens that flagged it, the reason and the time, from which it can be
#' restored with `sieve_restore_cases()`. `state$case_index` (set to the
#' row numbers of the data as loaded the first time a respondent is
#' removed) keeps the original row number of every remaining row. The
#' ledger's summaries are computed again from the remaining rows, and the
#' step is logged with the R that reproduces it and the reason as a
#' comment. Every tab reads `state$items`, so every tab follows.
#'
#' @param state The shared state store.
#' @param rows Original row numbers (rows of the data as loaded) of
#'   respondents now in the analysis.
#' @param reason The researcher's reason, in their words (may be empty).
#' @param flagged_by Optional: the screens that flagged each respondent,
#'   one string per element of `rows` (or named by row).
#' @param holdout `NULL`, or, when the rows are reserved for validation
#'   (`sieve_reserve_holdout()`, fct_holdout.R), a list with `id` (the
#'   reservation, kept with each held row as its marker, so the ordinary
#'   Restore refuses the row), `note` (the log note) and `code` (lines
#'   placed before the removal code, which draw the rows from the seed).
#' @return Invisibly, a list with `removed` (the original rows removed),
#'   `unknown` (rows that are not in the analysis), and `refused` (`NULL`,
#'   or the reason nothing was removed).
#' @author Ken Kelley
#' @noRd
sieve_remove_cases <- function(state, rows, reason = "", flagged_by = NULL, holdout = NULL) {
  shiny::isolate({
    replay_before <- sieve_replay_inputs(state)
    raw <- state$raw
    items <- state$items
    rows <- unique(suppressWarnings(as.integer(rows)))
    rows <- rows[!is.na(rows)]
    empty <- list(removed = integer(0), unknown = rows, refused = NULL)
    if (is.null(raw) || is.null(items) || !length(rows)) {
      return(invisible(empty))
    }
    if (NROW(raw) != NROW(items)) {
      empty$refused <- "the item data and the data as loaded do not have the same rows, so no respondent was removed"
      return(invisible(empty))
    }
    sieve_ensure_dataset(state)
    index <- sieve_case_index(state)
    at <- match(rows, index)
    unknown <- rows[is.na(at)]
    rows <- rows[!is.na(at)]
    at <- at[!is.na(at)]
    # In the order of the data as loaded.
    at <- at[order(rows)]
    rows <- sort(rows)
    if (!length(rows)) {
      return(invisible(list(removed = integer(0), unknown = unknown, refused = NULL)))
    }
    left <- NROW(raw) - length(rows)
    if (left < 3L) {
      return(invisible(list(
        removed = integer(0), unknown = unknown,
        refused = sprintf(
          paste(
            "removing %d respondent%s would leave %d in the analysis; a correlation",
            "needs at least three, so no respondent was removed"
          ),
          length(rows), if (length(rows) == 1L) "" else "s", left
        )
      )))
    }
    reason <- trimws(paste(reason %||% "", collapse = " "))
    var <- sieve_case_id_variable(state)
    ids <- if (!is.na(var)) sieve_case_id_text(raw[[var]]) else rep(NA_character_, NROW(raw))
    flags <- if (is.null(flagged_by)) {
      rep("", length(rows))
    } else if (!is.null(names(flagged_by))) {
      unname(as.character(flagged_by[as.character(rows)]))
    } else {
      as.character(rep_len(flagged_by, length(rows)))
    }
    flags[is.na(flags)] <- ""
    stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    n_transforms <- length(state$transforms %||% list())
    held <- lapply(seq_along(rows), function(k) {
      list(
        row = rows[k],
        id = ids[at[k]],
        id_variable = var,
        flagged_by = flags[k],
        reason = if (nzchar(reason)) reason else NA_character_,
        time = stamp,
        transforms_at = n_transforms,
        holdout = holdout$id,
        raw = sieve_cases_take(raw, at[k]),
        items = sieve_cases_take(items, at[k])
      )
    })
    keep <- setdiff(seq_len(NROW(raw)), at)
    state$case_index <- index[keep]
    state$raw <- sieve_cases_take(raw, keep)
    state$items <- sieve_cases_take(items, keep)
    state$removed_cases <- c(state$removed_cases %||% list(), held)
    sieve_cases_describe(state)
    # The session script names the respondents by their rows of the data as
    # loaded, never by an identifier; the Screening tab shows the
    # identifiers.
    who <- sieve_cases_who(rows, rep(NA_character_, length(rows)), NA_character_)
    sieve_log(
      state,
      holdout$note %||% sprintf(
        "Screening: remove %d respondent%s from the analysis: %s%s; %d remain%s",
        length(rows), if (length(rows) == 1L) "" else "s", who,
        if (nzchar(reason)) sprintf(" (reason: %s)", reason) else "",
        left, if (left == 1L) "s" else ""
      ),
      c(holdout$code, sieve_cases_remove_code(rows, ids[at], NA_character_, reason = reason)),
      before = replay_before, kind = "mutation"
    )
    invisible(list(removed = rows, unknown = unknown, refused = NULL))
  })
}

#' One held respondent's value of one item, with the changes recorded since
#' the removal
#'
#' The value held at the removal (or the value as loaded, for an item that
#' joined the pool since), with each reversal recorded since the removal
#' made to it as it was made to the rows in the analysis: the reflection
#' of the value as loaded, the value as loaded, or the reflection of the
#' current value, within the recorded bounds. A value a reflection is made
#' of that lies outside the bounds of that reflection has no reflection on
#' the response scale, and is reported.
#'
#' @param r One held respondent (an element of `state$removed_cases`).
#' @param r_raw The respondent's row of the data as loaded, in the columns
#'   of the data now.
#' @param it The item.
#' @param later The transformation records made since the removal.
#' @return A list with `value` and `off` (a phrase for each reflection
#'   made of a value outside its bounds; empty when there is none).
#' @author Ken Kelley
#' @noRd
sieve_cases_held_value <- function(r, r_raw, it, later) {
  value <- if (it %in% names(r$items)) r$items[[it]] else r_raw[[it]]
  off <- character(0)
  for (rec in later) {
    if (!identical(rec$type, "reverse") || !identical(rec$item, it)) {
      next
    }
    if (!identical(rec$applied, FALSE)) {
      from <- if (identical(rec$applied, TRUE)) sieve_screen_numeric(r_raw[[it]]) else sieve_screen_numeric(value)
      if (length(sieve_outside_bounds(from, rec$lo, rec$hi))) {
        off <- c(off, sprintf(
          "row %d holds %s on %s, outside the bounds %s to %s within which %s was reverse scored after the respondent was removed",
          as.integer(r$row), paste(sieve_num_text(from), collapse = ", "), it,
          sieve_num_text(rec$lo), sieve_num_text(rec$hi), it
        ))
      }
    }
    value <- if (identical(rec$applied, FALSE)) {
      r_raw[[it]]
    } else if (identical(rec$applied, TRUE)) {
      (rec$lo + rec$hi) - sieve_screen_numeric(r_raw[[it]])
    } else {
      (rec$lo + rec$hi) - sieve_screen_numeric(value)
    }
  }
  list(value = value, off = off)
}

#' The values as loaded of one item for the respondents removed from the
#' analysis
#'
#' @param removed The removed respondents (`state$removed_cases`).
#' @param item An item (variable) name.
#' @return A numeric vector, one value per removed respondent who holds the
#'   variable (possibly empty).
#' @author Ken Kelley
#' @noRd
sieve_removed_values <- function(removed, item) {
  if (!is.list(removed) || !length(removed)) {
    return(numeric(0))
  }
  out <- lapply(removed, function(r) {
    if (is.list(r) && is.data.frame(r$raw) && item %in% names(r$raw)) sieve_screen_numeric(r$raw[[item]]) else numeric(0)
  })
  unlist(out, use.names = FALSE)
}

#' Restore removed respondents to the analysis
#'
#' The rows held in `state$removed_cases` return to the data as loaded and
#' the item data at their original positions. An item reverse scored (or
#' returned to its values as loaded) after a respondent was removed did not
#' reach the held row, so the transformation records made since then are
#' applied to it first, exactly as they were applied to the rows in the
#' analysis: the reflection of the values as loaded, the values as loaded,
#' or the reflection of the current values, within the recorded bounds. An
#' item that joined the item pool after the removal takes its value as
#' loaded. The ledger's summaries are computed again and the step is
#' logged.
#'
#' A reflection made after a respondent was removed is checked against the
#' bounds it was made within, as the transformation service checks every
#' value it reflects: a held value outside them has no reflection on the
#' response scale, so the restore is refused, nothing is changed, and the
#' reason names the respondent, the item, the value and the bounds.
#'
#' A row reserved for validation (its held record carries the marker
#' `holdout`, see `sieve_reserve_holdout()`) is refused by the ordinary
#' restore, with the reason: only Release the reservation returns it,
#' which names the reservation in `holdout`.
#'
#' @param state The shared state store.
#' @param rows Original row numbers of removed respondents.
#' @param holdout `NULL`, or a list with `id` (the reservation whose rows
#'   may be restored) and `note` (the log note of the release).
#' @return Invisibly, a list with `restored` (the original rows restored),
#'   `unknown` (rows that are not among the removed respondents), and
#'   `refused` (`NULL`, or the reason nothing was restored).
#' @author Ken Kelley
#' @noRd
sieve_restore_cases <- function(state, rows, holdout = NULL) {
  shiny::isolate({
    replay_before <- sieve_replay_inputs(state)
    held <- state$removed_cases %||% list()
    rows <- unique(suppressWarnings(as.integer(rows)))
    rows <- rows[!is.na(rows)]
    held_rows <- vapply(held, function(r) as.integer(r$row), integer(1))
    unknown <- setdiff(rows, held_rows)
    rows <- intersect(rows, held_rows)
    raw <- state$raw
    items <- state$items
    if (!length(rows) || is.null(raw) || is.null(items)) {
      return(invisible(list(restored = integer(0), unknown = unknown, refused = NULL)))
    }
    reserved <- vapply(held, function(r) {
      !is.null(r$holdout) && !identical(r$holdout, holdout$id)
    }, logical(1))
    if (any(reserved & held_rows %in% rows)) {
      kept <- held_rows[reserved & held_rows %in% rows]
      return(invisible(list(
        restored = integer(0), unknown = unknown,
        refused = sprintf(
          paste(
            "no respondent was restored: %s %s reserved for validation (reservation %s), and the",
            "ordinary Restore does not return reserved rows. Release the reservation on the Screening",
            "page to return them, after which development sees them and the reservation's",
            "protection of an evaluation is gone"
          ),
          sieve_cases_who(kept, rep(NA_character_, length(kept)), NA_character_),
          if (length(kept) == 1L) "is" else "are",
          paste(unique(vapply(held[reserved & held_rows %in% rows], function(r) as.character(r$holdout), character(1))), collapse = ", ")
        )
      )))
    }
    back <- held[held_rows %in% rows]
    back <- back[order(held_rows[held_rows %in% rows])]
    records <- state$transforms %||% list()
    fixes <- list()
    raw_back <- NULL
    items_back <- NULL
    off_scale <- character(0)
    for (r in back) {
      r_raw <- r$raw[, names(raw), drop = FALSE]
      # The item values: those held, with the transformations recorded since
      # the removal made to them; an item that joined the pool since then
      # (or was not held) takes its value as loaded.
      later <- if (length(records) > r$transforms_at) records[(r$transforms_at + 1L):length(records)] else list()
      held_values <- lapply(names(items), function(it) sieve_cases_held_value(r, r_raw, it, later))
      cols <- lapply(held_values, `[[`, "value")
      off_scale <- c(off_scale, unlist(lapply(held_values, `[[`, "off"), use.names = FALSE))
      names(cols) <- names(items)
      r_items <- sieve_cases_take(items, integer(0))
      r_items <- sieve_cases_bind(r_items, structure(cols,
        class = "data.frame",
        row.names = attr(r$items, "row.names")
      ))
      raw_back <- if (is.null(raw_back)) r_raw else sieve_cases_bind(raw_back, r_raw)
      items_back <- if (is.null(items_back)) r_items else sieve_cases_bind(items_back, r_items)
    }
    if (length(off_scale)) {
      return(invisible(list(
        restored = integer(0), unknown = unknown,
        refused = paste0(
          "no respondent was restored: ", paste(unique(off_scale), collapse = "; "),
          ". A value outside the response bounds has no reflection on the scale. If it is a code for a ",
          "missing response, set it to missing in the data file and load it again; otherwise return the ",
          "item to its values as loaded (its Reverse? cell), restore the respondent, and reverse score the ",
          "item again within bounds that hold the value"
        )
      )))
    }
    # The same changes, for the replay: each recorded reversal since a
    # removal, for the restored rows it concerns.
    for (k in seq_along(records)) {
      rec <- records[[k]]
      if (!identical(rec$type, "reverse")) next
      concern <- vapply(back, function(r) r$transforms_at < k, logical(1))
      if (any(concern)) {
        fixes[[length(fixes) + 1L]] <- list(
          item = rec$item,
          rows = vapply(
            back[concern],
            function(r) r$row,
            integer(1)
          ),
          applied = rec$applied, lo = rec$lo, hi = rec$hi
        )
      }
    }
    index <- c(sieve_case_index(state), vapply(back, function(r) as.integer(r$row), integer(1)))
    order_rows <- order(index)
    state$raw <- sieve_cases_take(sieve_cases_bind(raw, raw_back), order_rows)
    state$items <- sieve_cases_take(sieve_cases_bind(items, items_back), order_rows)
    remaining <- held[!held_rows %in% rows]
    if (length(remaining)) {
      state$case_index <- index[order_rows]
      state$removed_cases <- remaining
    } else {
      state$case_index <- NULL
      state$removed_cases <- list()
    }
    sieve_cases_describe(state)
    var <- sieve_case_id_variable(state)
    ids <- vapply(back, function(r) as.character(r$id %||% NA), character(1))
    restored <- vapply(back, function(r) as.integer(r$row), integer(1))
    sieve_log(
      state,
      holdout$note %||% sprintf(
        "Screening: restore %d respondent%s to the analysis: %s; %d in the analysis",
        length(restored), if (length(restored) == 1L) "" else "s",
        sieve_cases_who(restored, rep(NA_character_, length(restored)), NA_character_), NROW(state$raw)
      ),
      sieve_cases_restore_code(restored, fixes),
      before = replay_before, kind = "mutation"
    )
    invisible(list(restored = restored, unknown = unknown, refused = NULL))
  })
}

#' The respondents removed from the analysis, as a table
#'
#' @param state The shared state store (read where called).
#' @return A data frame with `row`, `id`, `flagged_by`, `reason`, and
#'   `time`, one row per removed respondent in the order of the data as
#'   loaded; no rows when nobody is removed.
#' @author Ken Kelley
#' @noRd
sieve_removed_table <- function(state) {
  held <- state$removed_cases %||% list()
  if (!length(held)) {
    return(data.frame(
      row = integer(0), id = character(0), flagged_by = character(0),
      reason = character(0), time = character(0), stringsAsFactors = FALSE
    ))
  }
  pick <- function(field, as) vapply(held, function(r) as(r[[field]] %||% NA), as(NA))
  out <- data.frame(
    row = pick("row", as.integer),
    id = pick("id", as.character),
    flagged_by = pick("flagged_by", as.character),
    reason = pick("reason", as.character),
    time = pick("time", as.character),
    stringsAsFactors = FALSE
  )
  out <- out[order(out$row), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---- Excluding items ---------------------------------------------------------

#' Exclude items from the analysis, with a reason
#'
#' The change the Ingest grid's Use cell makes: the ledger status becomes
#' `"parked"` (shown as excluded; the item leaves every analysis and stays
#' in the data), so the ledger, the log and every tab follow. The reason is
#' kept in the ledger column `exclude_reason` (created when absent; `NA`
#' when no reason is given), and the step is logged with the reason as a
#' comment. Items that are not in the ledger, or are already excluded, are
#' left alone and reported.
#'
#' @param state The shared state store.
#' @param items Item (variable) names.
#' @param reason The researcher's reason, in their words (may be empty).
#' @return Invisibly, a list with `changed` (the items excluded) and
#'   `unchanged` (the items left alone).
#' @author Ken Kelley
#' @noRd
sieve_exclude_items <- function(state, items, reason = "") {
  # The one change behind every way of dropping an item (see
  # sieve_set_item_status() in mod_drop.R).
  sieve_set_item_status(state, items, "parked", reason)
}

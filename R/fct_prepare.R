# fct_prepare.R
# The pre-SIEVER step, and the ways a prepared study comes into the Ingest tab.
#
# A study is prepared once, outside the application: a data frame (or a
# .csv, .sav or .dta file) and a specification (the schema 2 table that the
# Ingest tab's Download specification writes, or the template that
# sieve_specification_template() writes, filled in) become a project file
# through sieve_prepare_study(). The function loads the data exactly as the
# Ingest tab loads an uploaded file, applies the specification through the
# import's own checks (sieve_spec_import(), every problem reported and
# nothing written when there is one), confirms the item types when the
# specification types every item, and writes the project with
# sieve_project_write(). The project then opens on the Ingest tab confirmed.
#
# The plain functions in the first half of the file serve the Ingest tab as
# well, so the tab and the pre-SIEVER step cannot drift apart:
# sieve_read_data() reads a data file, sieve_load_frame() places a data
# frame in the session with its roles proposed, sieve_structure_from_names()
# proposes constructs and facets carried in the column names or in the
# variable labels, sieve_sort_uploads() tells a project file, a data file
# and a specification apart when several files arrive together, and
# sieve_confirm_types() records the confirmation of the item types.

# ---- Reading and placing data -----------------------------------------------

#' Read a data file the way the Ingest tab reads an upload
#'
#' The file type is taken from `name` (the name the file was uploaded
#' under; the temporary path an upload is stored at says nothing). SPSS and
#' Stata files come through haven, which maps user missing values to `NA`
#' and keeps each variable's label; comma and tab separated files are read
#' as [utils::read.csv()] and [utils::read.delim()] read them, after their
#' bytes are checked (`sieve_text_bytes()`): they are read as UTF-8 by
#' default (or as the stated `encoding`) in every locale, a byte order mark
#' is removed, a file that this session cannot read correctly is refused
#' with the reason, and a parse that would return part of the file is
#' refused rather than truncated.
#'
#' @param path The file to read.
#' @param name The file name that gives the type.
#' @param description Whether the rows under the header whose first cell
#'   says `construct`, `facet`, `reverse`, `question` or `wording` are read
#'   as a description of the columns (see `sieve_description_rows()`);
#'   `FALSE` reads every row as data, for a file whose second row is data
#'   and must never be misread.
#' @param sep The separator of a `.csv` or `.txt` file; `NULL` (the
#'   default) is a comma for `.csv` and a tab for `.txt`.
#' @param dec The decimal mark of a `.csv` or `.txt` file.
#' @param encoding The encoding of a `.csv` or `.txt` file, one of
#'   `sieve_text_encodings()`. A file stated as windows-1252 or latin1 whose
#'   bytes are valid UTF-8 and hold a character outside ASCII is refused as
#'   UTF-8 (`sieve_text_bytes()`).
#' @param ingest Whether a refusal names the Ingest tab's control as well as
#'   the argument: `TRUE` only for the Ingest tab's Load; `FALSE` (the
#'   default) for a console caller ([sieve_prepare_study()] and
#'   [sieve_specification_template()] through `sieve_frame_of()`), which is
#'   given the argument to pass.
#' @return The list `sieve_capture()` returns (`value`, `ok`, `error`,
#'   `warnings`, `messages`), with `code`, the R that reads the file under
#'   its name, for the session script, `description` (the rows read,
#'   or `NULL`), and `measure` (the SPSS measurement level of each column
#'   of a .sav file, see `sieve_spss_measures()`, or `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_read_data <- function(path, name = basename(path), description = TRUE,
                            sep = NULL, dec = ".", encoding = "UTF-8", ingest = FALSE) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("csv", "txt")) {
    choices <- sieve_text_choices(sep, dec, encoding, ext)
    call <- if (identical(ext, "txt")) "read.delim" else "read.csv"
    checked <- sieve_text_bytes(path, choices$encoding, choices$sep, check_separator = TRUE, ingest = ingest)
    read <- if (!isTRUE(checked$ok)) {
      list(
        value = NULL, error = checked$error, error_class = NULL,
        warnings = character(0), messages = character(0), ok = FALSE
      )
    } else {
      # read.csv() and read.delim() differ only in their separator.
      sieve_text_parse(checked,
        header = TRUE, sep = choices$sep, dec = choices$dec, quote = "\"", fill = TRUE,
        comment.char = "", stringsAsFactors = FALSE
      )
    }
    read$code <- sieve_text_read_code(name, list(ascii = isTRUE(checked$ascii), bom = isTRUE(checked$bom)),
      choices$encoding, call, sep = choices$sep, dec = choices$dec
    )
  } else {
    read <- sieve_capture(switch(ext,
      sav = if (sieve_have("haven")) haven::read_sav(path) else stop("haven is needed for .sav files"),
      dta = if (sieve_have("haven")) haven::read_dta(path) else stop("haven is needed for .dta files"),
      stop("only .sav, .dta, .csv and .txt files are read")
    ))
    reader <- switch(ext,
      sav = "haven::read_sav",
      dta = "haven::read_dta",
      "read.csv"
    )
    # The file name is written as an R string literal, so a name with a quote
    # or a backslash reads back as the same name.
    read$code <- sprintf("d <- %s(%s)", reader, encodeString(name, quote = "\""))
  }
  read$description <- NULL
  # The SPSS measurement level (Nominal, Ordinal, Scale) of each column,
  # which haven does not return, read from the file's dictionary and kept
  # on the columns (see fct_spss.R).
  read$measure <- NULL
  if (identical(ext, "sav") && isTRUE(read$ok) && is.data.frame(read$value)) {
    read$measure <- sieve_spss_measures(path)
    read$value <- sieve_spss_mark(read$value, read$measure, missing = sieve_spss_missing(path))
  }
  # A comma or tab separated file may carry its constructs and facets as
  # rows under the header (see sieve_description_rows()); they are kept
  # apart here, before the columns are typed, so that the text in them does
  # not make every column a text column.
  if (isTRUE(description) && ext %in% c("csv", "txt") && isTRUE(read$ok) && is.data.frame(read$value)) {
    found <- sieve_description_rows(read$value, dec = choices$dec)
    if (isTRUE(found$found)) {
      read$value <- found$data
      read$description <- found$description
      read$code <- c(read$code, sieve_description_code(dec = choices$dec))
    }
  }
  read
}

#' The words that name a description row
#'
#' @return `c("construct", "facet", "reverse", "reverse_applied",
#'   "question", "wording")`: `question` and `wording` name the same row, the
#'   item's wording; `reverse_applied` says whether the file already holds
#'   a reverse keyed item reverse scored, as the specification's column of
#'   that name does.
#' @author Ken Kelley
#' @noRd
sieve_description_words <- function() {
  c("construct", "facet", "reverse", "reverse_applied", "question", "wording")
}

#' Description rows under the header of a data file
#'
#' The one-file form of a study: a `.csv` whose header is followed by rows
#' whose first cell is `construct`, `facet` and, optionally, `reverse` and
#' `question` (or `wording`), in any order, and then the data rows. Each
#' description row gives, under every other column, that column's
#' construct, its facet, whether it is reverse keyed (`yes` or `no`, `1` or
#' `0`, `TRUE` or `FALSE`; an empty cell says nothing), whether the file
#' already holds a reverse keyed item reverse scored (the optional
#' `reverse_applied` row, in the same vocabulary), or the question
#' the item asked (its wording, kept in the ledger's `wording` column and
#' shown as the Question of the item). The first column names the rows, so
#' it carries no description of its own. [utils::read.csv()] reads such a
#' file with every column as text; this function takes the description
#' rows off the top, then types the remaining columns as `read.csv()`
#' would have ([utils::type.convert()]).
#'
#' @param df The data frame as `read.csv()` returned it (a column whose
#'   description cells are all empty is already typed; one with text in
#'   them is a text column until the rows are taken off).
#' @return A list with `found` (`TRUE` when description rows were there),
#'   `description` (a data frame with `variable`, `construct`, `facet`,
#'   `reverse`, `reverse_applied` and `wording` for every column but the
#'   first, with the attribute `bad_reverse` naming the columns whose
#'   reverse or reverse_applied cell was outside the vocabulary and `rows`
#'   naming the rows found), and `data` (the data rows, typed).
#' @param dec The decimal mark of the file (`"."` or `","`), with which the
#'   data columns are typed once the rows are taken off, as the reader typed
#'   the file.
#' @author Ken Kelley
#' @noRd
sieve_description_rows <- function(df, dec = ".") {
  none <- list(found = FALSE, description = NULL, data = df)
  if (!is.data.frame(df) || !nrow(df) || ncol(df) < 2L) {
    return(none)
  }
  first <- tolower(trimws(as.character(df[[1]])))
  first[is.na(first)] <- ""
  leading <- cumprod(first %in% sieve_description_words()) == 1
  rows <- which(leading)
  if (!length(rows)) {
    return(none)
  }
  variables <- names(df)[-1]
  cell <- function(word) {
    at <- match(word, first[rows])
    if (is.na(at)) {
      return(rep(NA_character_, length(variables)))
    }
    v <- trimws(vapply(df[-1], function(col) as.character(col[rows[at]]), character(1)))
    v[is.na(v) | !nzchar(v)] <- NA_character_
    unname(v)
  }
  as_flag <- function(low) {
    out <- rep(NA, length(variables))
    out[low %in% c("true", "t", "yes", "y", "1")] <- TRUE
    out[low %in% c("false", "f", "no", "n", "0")] <- FALSE
    out
  }
  low <- tolower(cell("reverse"))
  reverse <- as_flag(low)
  low_applied <- tolower(cell("reverse_applied"))
  reverse_applied <- as_flag(low_applied)
  # The question row, under either of its names; a file that carries both
  # is read from the first of the two that appears.
  wording <- cell("question")
  other <- cell("wording")
  wording[is.na(wording)] <- other[is.na(wording)]
  description <- data.frame(
    variable = variables, construct = cell("construct"), facet = cell("facet"), reverse = reverse,
    reverse_applied = reverse_applied, wording = wording, stringsAsFactors = FALSE
  )
  attr(description, "rows") <- first[rows]
  attr(description, "bad_reverse") <- unique(c(
    variables[!is.na(low) & is.na(reverse)],
    variables[!is.na(low_applied) & is.na(reverse_applied)]
  ))
  data <- df[-rows, , drop = FALSE]
  rownames(data) <- NULL
  # Only a column the description text made a text column is typed again;
  # a column read.csv() already typed (its description cells were empty)
  # keeps its values exactly.
  data[] <- lapply(data, function(col) if (is.character(col)) utils::type.convert(col, as.is = TRUE, dec = dec) else col)
  list(found = TRUE, description = description, data = data)
}

#' The R that takes the description rows off a data frame in a replay
#'
#' The same rule as `sieve_description_rows()`: the rows at the top whose
#' first cell is one of the description words are kept apart as
#' `description`, and the columns of what remains are typed, with the
#' file's decimal mark.
#'
#' @param dec The decimal mark of the file.
#' @return A character vector of R source lines.
#' @author Ken Kelley
#' @noRd
sieve_description_code <- function(dec = ".") {
  c(
    "# The rows under the header whose first cell says construct, facet, reverse,",
    "# question or wording describe the columns; they are kept apart and the data",
    "# columns are then typed.",
    sprintf(
      "described <- which(cumprod(tolower(trimws(as.character(d[[1]]))) %%in%% c(%s)) == 1)",
      paste(encodeString(sieve_description_words(), quote = "\""), collapse = ", ")
    ),
    "description <- d[described, , drop = FALSE]",
    "d <- d[-described, , drop = FALSE]",
    "rownames(d) <- NULL",
    if (identical(dec, ".")) {
      "d[] <- lapply(d, function(col) if (is.character(col)) type.convert(col, as.is = TRUE) else col)"
    } else {
      sprintf(
        "d[] <- lapply(d, function(col) if (is.character(col)) type.convert(col, as.is = TRUE, dec = %s) else col)",
        encodeString(dec, quote = "\"")
      )
    }
  )
}

#' Lay a file's description rows over the structure proposed from the names
#'
#' The description rows are explicit, so for an item they name they replace
#' what the column name or the label proposed. A construct that has the
#' name of a variable in the data is withheld, as for the other forms.
#'
#' @param structure A `sieve_structure_from_names()` result.
#' @param description The `description` of `sieve_description_rows()`, or
#'   `NULL`.
#' @param taken The names of every column in the data.
#' @return The structure, with `source` `"file"` on the rows the
#'   description filled, and its `withheld` attribute extended.
#' @author Ken Kelley
#' @noRd
sieve_describe_structure <- function(structure, description, taken = character(0)) {
  if (is.null(description) || !NROW(description)) {
    return(structure)
  }
  at <- match(structure$variable, description$variable)
  has <- !is.na(at) & !is.na(description$construct[at])
  if (!any(has)) {
    return(structure)
  }
  withheld <- attr(structure, "withheld") %||% character(0)
  structure$construct[has] <- description$construct[at[has]]
  structure$facet[has] <- description$facet[at[has]]
  structure$source[has] <- "file"
  clash <- has & structure$construct %in% as.character(taken)
  if (any(clash)) {
    withheld <- unique(c(withheld, structure$construct[clash]))
    structure$construct[clash] <- NA_character_
    structure$facet[clash] <- NA_character_
    structure$source[clash] <- NA_character_
  }
  attr(structure, "withheld") <- withheld
  structure
}

#' The counts of a ledger's structure
#'
#' @param meta The item ledger.
#' @return A list with `n_items` (included), `n_all`, `constructs` (the
#'   names, Unassigned left out, in order of first appearance among the
#'   included items), and `n_facets` (counted by construct, as
#'   `sieve_pool_text()` counts them).
#' @author Ken Kelley
#' @noRd
sieve_pool_counts <- function(meta) {
  active <- meta[meta$status == "active", , drop = FALSE]
  constructs <- unique(as.character(active$construct)[sieve_construct_assigned(active$construct)])
  facet <- as.character(active$facet)
  has_facet <- !is.na(facet) & nzchar(facet)
  facets <- unique(paste(as.character(active$construct)[has_facet], facet[has_facet], sep = "\r"))
  list(n_items = nrow(active), n_all = nrow(meta), constructs = constructs, n_facets = length(facets))
}

#' What a load read from the file's description rows, in words
#'
#' @param meta The item ledger after the load.
#' @param described The `described` element `sieve_load_frame()` returns.
#' @return One sentence such as `"Constructs and facets read from the file:
#'   6 constructs, 27 facets."`; `"Keying read from the file: 2 items
#'   reverse scored as the reverse row says."` when the rows gave no
#'   construct (a reverse row alone); `NULL` when the file carried no
#'   description rows, or rows that gave neither.
#' @author Ken Kelley
#' @noRd
sieve_described_text <- function(meta, described) {
  if (is.null(described)) {
    return(NULL)
  }
  if (isTRUE(described$off)) {
    return("Rows under the header were read as data (the step 1 option to read description rows is off).")
  }
  if (!isTRUE(described$found)) {
    return(NULL)
  }
  counts <- sieve_pool_counts(meta)
  n_c <- length(counts$constructs)
  n_r <- length(described$reversed)
  n_q <- length(described$questions %||% character(0))
  n_k <- length(described$recorded %||% character(0))
  n_h <- length(described$held %||% numeric(0))
  keyed <- c(
    if (n_r) sprintf("%d item%s reverse scored as the reverse row says", n_r, if (n_r == 1L) "" else "s"),
    if (n_k) sprintf("%d item%s recorded as already reverse scored in the file", n_k, if (n_k == 1L) "" else "s"),
    if (n_h) {
      sprintf(
        "%d marked item%s not reverse scored because %s already correlate positively with %s construct (see above the grid)",
        n_h, if (n_h == 1L) "" else "s", if (n_h == 1L) "its values" else "their values",
        if (n_h == 1L) "its" else "their"
      )
    }
  )
  asked <- if (n_q) sprintf("the question row read for %d item%s", n_q, if (n_q == 1L) "" else "s")
  tail <- c(keyed, asked)
  if (n_c == 0L && counts$n_facets == 0L) {
    if (!length(tail)) {
      return(NULL)
    }
    return(if (is.null(keyed)) {
      sprintf("Questions read from the file: %s.", asked)
    } else {
      sprintf("Keying read from the file: %s.", paste(tail, collapse = "; "))
    })
  }
  sprintf(
    "Constructs%s read from the file: %d construct%s, %d facet%s%s.",
    if (counts$n_facets) " and facets" else "", n_c, if (n_c == 1L) "" else "s",
    counts$n_facets, if (counts$n_facets == 1L) "" else "s",
    if (!length(tail)) "" else paste0("; ", paste(tail, collapse = "; "))
  )
}

#' The variables that are not items, in words
#'
#' @param roles A roles table, or `NULL`.
#' @return One sentence such as `"17 variables are not items: 5 groups, 12
#'   criteria."`, or `"Every variable is an item."`; `NULL` for no table.
#' @author Ken Kelley
#' @noRd
sieve_not_items_text <- function(roles) {
  if (is.null(roles)) {
    return(NULL)
  }
  other <- roles[roles$role != "item", , drop = FALSE]
  if (!nrow(other)) {
    return("Every variable is an item.")
  }
  plural <- c(
    identifier = "identifiers", group = "groups", criterion = "criteria",
    antecedent = "antecedents", auxiliary = "auxiliary variables"
  )
  single <- c(
    identifier = "identifier", group = "group", criterion = "criterion",
    antecedent = "antecedent", auxiliary = "auxiliary variable"
  )
  n <- vapply(names(plural), function(r) sum(other$role == r), integer(1))
  n <- n[n > 0L]
  sprintf(
    "%d variable%s %s not %s: %s.", nrow(other), if (nrow(other) == 1L) "" else "s",
    if (nrow(other) == 1L) "is" else "are", if (nrow(other) == 1L) "an item" else "items",
    paste(sprintf("%d %s", n, ifelse(n == 1L, single[names(n)], plural[names(n)])), collapse = ", ")
  )
}

#' What changed in the study since it was confirmed
#'
#' Compares the ledger (its analysis-relevant columns) and the item data at
#' the confirmation with the ledger and the data now, and says the first
#' changes in plain words, for the Confirm button's third state.
#'
#' @param then A list with `meta` and `data_key` at the confirmation.
#' @param now The same for the present.
#' @return `NULL` when nothing that matters has changed, otherwise a phrase
#'   such as `"item_120 reverse scored"` or `"t4_lozenges excluded; t1,
#'   t2 reassigned"`, with the attribute `kinds`, the kinds of change found
#'   (any of `"pool"`, `"reverse"`, `"status"`, `"type"`, `"construct"`,
#'   `"facet"`, `"role"`, `"data"`), so that a caller can tell a change that leaves the
#'   confirmation in force (an inclusion, an exclusion, a facet move) from
#'   one that asks for it again.
#' @author Ken Kelley
#' @noRd
sieve_confirm_change <- function(then, now) {
  a <- then$meta
  b <- now$meta
  if (is.null(a) || is.null(b)) {
    return(NULL)
  }
  changes <- character(0)
  kinds <- character(0)
  diff <- sieve_ledger_diff(a, b)
  joined <- diff$joined
  left <- diff$left
  if (length(joined)) {
    changes <- c(changes, sprintf("%s joined the item pool", sieve_name_some(joined, 3L)))
    kinds <- c(kinds, "pool")
  }
  if (length(left)) {
    changes <- c(changes, sprintf("%s left the item pool", sieve_name_some(left, 3L)))
    kinds <- c(kinds, "pool")
  }
  common <- diff$common
  ai <- diff$a
  bi <- diff$b
  differ <- function(col) diff$changed[[col]] %||% rep(FALSE, length(common))
  rv <- differ("reverse")
  if (any(rv)) {
    now_rev <- bi$reverse[rv] %in% TRUE
    changes <- c(changes, sprintf(
      "%s %s", sieve_name_some(common[rv], 3L),
      if (all(now_rev)) "reverse scored" else if (!any(now_rev)) "no longer reverse scored" else "changed keying"
    ))
    kinds <- c(kinds, "reverse")
  }
  st <- differ("status")
  if (any(st)) {
    now_in <- as.character(bi$status[st]) == "active"
    changes <- c(changes, sprintf(
      "%s %s", sieve_name_some(common[st], 3L),
      if (all(now_in)) "included" else if (!any(now_in)) "excluded" else "included or excluded"
    ))
    kinds <- c(kinds, "status")
  }
  ty <- differ("type")
  if (any(ty)) {
    changes <- c(changes, sprintf("%s retyped", sieve_name_some(common[ty], 3L)))
    kinds <- c(kinds, "type")
  }
  cn <- differ("construct")
  if (any(cn)) {
    changes <- c(changes, sprintf("%s reassigned", sieve_name_some(common[cn], 3L)))
    kinds <- c(kinds, "construct")
  }
  fc <- differ("facet")
  if (any(fc)) {
    changes <- c(changes, sprintf("%s moved to another facet", sieve_name_some(common[fc], 3L)))
    kinds <- c(kinds, "facet")
  }
  if (!is.null(then$roles_key) && !is.null(now$roles_key) &&
    !identical(then$roles_key, now$roles_key)) {
    changes <- c(changes, "variable roles changed")
    kinds <- c(kinds, "role")
  }
  if (!length(changes) && !identical(then$data_key, now$data_key)) {
    changes <- "the item values changed"
    kinds <- "data"
  }
  if (!length(changes)) {
    return(NULL)
  }
  structure(paste(changes, collapse = "; "), kinds = unique(kinds))
}

#' What a Load that replaces the study would discard
#'
#' Says, in plain words, what was done to the study since it was loaded (or
#' opened) that a Load reading a source afresh would throw away: the changes
#' the confirmation reads (types, constructs, facets, keying, inclusion,
#' the item values), the Item Names and wordings edited, the response bounds
#' declared or cleared (bounds marked declared), the variable roles changed, and respondents removed from the
#' analysis. Fits, results and the session script are discarded too and are
#' said by the caller.
#'
#' @param then,now Snapshots of the study at the load and now: lists with
#'   `meta` (the ledger), `roles` (the roles table, or `NULL`), `rows` (the
#'   number of rows of the item data) and `data_key`.
#' @return A character vector of phrases, empty when nothing is detected.
#' @author Ken Kelley
#' @noRd
sieve_load_discards <- function(then, now) {
  if (is.null(then$meta) || is.null(now$meta)) {
    return(character(0))
  }
  cols <- c("item", "construct", "facet", "type", "status", "reverse")
  pick <- function(meta) meta[intersect(cols, names(meta))]
  then_cols <- pick(then$meta)
  ledger <- sieve_confirm_change(
    list(meta = then_cols, data_key = then$data_key),
    list(meta = pick(now$meta)[intersect(names(then_cols), names(pick(now$meta)))], data_key = now$data_key)
  )
  out <- if (is.null(ledger)) character(0) else as.vector(ledger)
  diff <- sieve_ledger_diff(then$meta, now$meta)
  edited <- function(col) diff$common[diff$changed[[col]] %||% rep(FALSE, length(diff$common))]
  rekeyed <- edited("reverse")
  labels <- edited("label")
  if (length(labels)) {
    out <- c(out, sprintf("the Item Name of %s edited", sieve_name_some(labels, 3L)))
  }
  worded <- unique(c(edited("wording"), edited("question")))
  if (length(worded)) {
    out <- c(out, sprintf("the wording of %s edited", sieve_name_some(worded, 3L)))
  }
  # A reversal stores the bounds it reflected within; those items are named
  # by their keying above.
  bounded <- setdiff(unique(c(edited("rev_lo"), edited("rev_hi"), edited("rev_bounds"))), rekeyed)
  if (length(bounded)) {
    # A declaration is bounds marked "declared" (by a cell, the row for
    # several items, the file's value labels or a specification). One taken
    # back is said apart from one made. Observed range bounds a reversal
    # stored, and kept after it was taken back, are no declaration: a Load
    # that drops them loses nothing, since a later reversal reads the same
    # observed range from the data as loaded.
    decl <- function(meta) {
      at <- match(bounded, meta$item)
      lo <- suppressWarnings(as.numeric(meta$rev_lo[at]))
      hi <- suppressWarnings(as.numeric(meta$rev_hi[at]))
      kind <- if ("rev_bounds" %in% names(meta)) as.character(meta$rev_bounds[at]) else rep(NA_character_, length(at))
      kind %in% "declared" & is.finite(lo) & is.finite(hi)
    }
    cleared <- bounded[decl(then$meta) & !decl(now$meta)]
    declared <- bounded[decl(now$meta)]
    if (length(declared)) {
      out <- c(out, sprintf("response bounds declared for %s", sieve_name_some(declared, 3L)))
    }
    if (length(cleared)) {
      out <- c(out, sprintf("the declared response bounds of %s cleared", sieve_name_some(cleared, 3L)))
    }
  }
  if (is.data.frame(then$roles) && is.data.frame(now$roles) && all(c("variable", "role") %in% names(then$roles)) &&
    all(c("variable", "role") %in% names(now$roles))) {
    vars <- intersect(then$roles$variable, now$roles$variable)
    a <- then$roles$role[match(vars, then$roles$variable)]
    b <- now$roles$role[match(vars, now$roles$variable)]
    moved <- vars[!((a == b) %in% TRUE)]
    if (length(moved)) {
      out <- c(out, sprintf("the role of %s changed", sieve_name_some(moved, 3L)))
    }
  }
  if (is.numeric(then$rows) && is.numeric(now$rows) && now$rows < then$rows) {
    gone <- then$rows - now$rows
    # Removing respondents changes the item data's key, which the ledger
    # comparison reports as changed values; the removal is the change.
    out <- setdiff(out, "the item values changed")
    out <- c(out, sprintf("%d respondent%s removed from the analysis", gone, if (gone == 1) "" else "s"))
  }
  out
}

#' Match pasted or typed variable names to the columns of the data
#'
#' Names are separated by commas, semicolons, spaces or line breaks, may be
#' quoted, and are matched without regard to case.
#'
#' @param text What was pasted or typed.
#' @param candidates The column names of the data.
#' @return A list with `matched` (the column names, in the order given,
#'   each once) and `unmatched` (the names that match no column).
#' @author Ken Kelley
#' @noRd
sieve_match_names <- function(text, candidates) {
  tokens <- unlist(strsplit(paste(as.character(text), collapse = " "), "[,;[:space:]]+"))
  tokens <- gsub("^[\"']+|[\"']+$", "", trimws(tokens))
  tokens <- unique(tokens[nzchar(tokens)])
  at <- match(tolower(tokens), tolower(as.character(candidates)))
  list(matched = unique(as.character(candidates)[at[!is.na(at)]]), unmatched = tokens[is.na(at)])
}

#' The constructs of a ledger with their item counts
#'
#' @param meta The item ledger.
#' @return A data frame with one row per construct, in order of first
#'   appearance (Unassigned last when any item has no construct):
#'   `construct`, `n` (items), `n_active` (included), `n_excluded`.
#' @author Ken Kelley
#' @noRd
sieve_construct_table <- function(meta) {
  cn <- as.character(meta$construct)
  cn[is.na(cn) | !nzchar(cn)] <- "Unassigned"
  order_of <- unique(cn)
  order_of <- c(setdiff(order_of, "Unassigned"), intersect("Unassigned", order_of))
  n <- vapply(order_of, function(c) sum(cn == c), integer(1))
  n_active <- vapply(order_of, function(c) sum(cn == c & meta$status == "active"), integer(1))
  data.frame(
    construct = order_of, n = unname(n), n_active = unname(n_active),
    n_excluded = unname(n - n_active), stringsAsFactors = FALSE
  )
}

#' The ledger by construct and facet
#'
#' @param meta The item ledger.
#' @return A data frame with one row per construct and facet, in order of
#'   first appearance: `construct`, `facet` (`"(no facet)"` for items
#'   without one), `n`, `n_active`, `n_excluded`, `types` (the included
#'   items by type, in words), `n_reversed`.
#' @author Ken Kelley
#' @noRd
sieve_ledger_summary <- function(meta) {
  cn <- as.character(meta$construct)
  cn[is.na(cn) | !nzchar(cn)] <- "Unassigned"
  fc <- as.character(meta$facet)
  fc[is.na(fc) | !nzchar(fc)] <- "(no facet)"
  key <- paste(cn, fc, sep = "\r")
  order_of <- unique(key)
  rows <- lapply(order_of, function(k) {
    at <- key == k
    block <- meta[at, , drop = FALSE]
    active <- block[block$status == "active", , drop = FALSE]
    data.frame(
      construct = cn[at][1], facet = fc[at][1], n = nrow(block), n_active = nrow(active),
      n_excluded = nrow(block) - nrow(active),
      types = if (nrow(active)) sieve_types_text(block) else "none included",
      n_reversed = sum(block$reverse %in% TRUE),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' The variable label of each column
#'
#' @param df A data frame (a `.sav` or `.dta` file read by haven carries a
#'   `label` attribute on each labeled column).
#' @return A character vector named by column, `NA` where there is no label.
#' @author Ken Kelley
#' @noRd
sieve_var_labels <- function(df) {
  vapply(df, function(column) {
    l <- attr(column, "label")
    if (is.null(l) || length(l) != 1L || is.na(l) || !nzchar(l)) NA_character_ else as.character(l)
  }, character(1))
}

#' Constructs and facets carried in the column names or the variable labels
#'
#' The optional way to carry the structure of a study in the data itself.
#' A column named `Construct__Facet__item` or `Construct__item` (two
#' underscores between the parts) names its construct and, with three parts,
#' its facet; within a part, single underscores and periods are read as
#' spaces (`Social_trust` is the facet `Social trust`), because a
#' column name cannot hold a space. A variable label of the form
#' `Construct: Facet: wording` or `Construct: wording` (a colon and a space
#' between the parts) names them as well, and its last part is the item's
#' wording. A label is read this way only when its construct part is short
#' (at most four words, no sentence punctuation) and shared by at least one
#' other item, so that a label such as `Q1: I like the product` is not read
#' as a construct of one item. A name that follows the form is read as it
#' is, because the two underscores are deliberate.
#'
#' What is returned is a proposal: the Ingest tab writes it into the ledger
#' for review, where the researcher changes any part of it before
#' confirming the study. A construct that has the name of a variable in
#' the data (`taken`) is not proposed, with its facet, because the
#' specification import refuses such a construct (`sieve_spec_import()`);
#' the names withheld are the `withheld` attribute of the result.
#'
#' @param variables The item column names.
#' @param labels Their variable labels (with any keying statement already
#'   removed), or `NULL`.
#' @param taken The names of every column in the data.
#' @return A data frame with one row per item: `variable`, `construct`,
#'   `facet`, `wording` (from a label, else `NA`) and `source` (`"name"`,
#'   `"label"`, or `NA` when nothing was proposed); its `withheld`
#'   attribute names the constructs that were not proposed because a
#'   variable has their name.
#' @author Ken Kelley
#' @noRd
sieve_structure_from_names <- function(variables, labels = NULL, taken = character(0)) {
  variables <- as.character(variables)
  n <- length(variables)
  out <- data.frame(
    variable = variables, construct = rep(NA_character_, n),
    facet = rep(NA_character_, n), wording = rep(NA_character_, n),
    source = rep(NA_character_, n), stringsAsFactors = FALSE
  )
  attr(out, "withheld") <- character(0)
  if (!n) {
    return(out)
  }
  clean <- function(x) trimws(gsub("[_.]+", " ", x))
  parts <- strsplit(variables, "__", fixed = TRUE)
  for (i in seq_len(n)) {
    p <- parts[[i]]
    if (length(p) %in% c(2L, 3L) && all(nzchar(trimws(p)))) {
      out$construct[i] <- clean(p[1])
      out$facet[i] <- if (length(p) == 3L) clean(p[2]) else NA_character_
      out$source[i] <- "name"
    }
  }
  if (!is.null(labels)) {
    labels <- as.character(labels)
    short <- function(x) {
      x <- trimws(x)
      nzchar(x) && length(strsplit(x, "[[:space:]]+")[[1]]) <= 4L && !grepl("[.?!,;]", x)
    }
    pieces <- lapply(labels, function(l) if (is.na(l)) character(0) else trimws(strsplit(l, ": ", fixed = TRUE)[[1]]))
    fits <- vapply(seq_len(n), function(i) {
      p <- pieces[[i]]
      is.na(out$source[i]) && length(p) %in% c(2L, 3L) && all(nzchar(p)) && short(p[1]) &&
        (length(p) == 2L || short(p[2]))
    }, logical(1))
    heads <- rep(NA_character_, n)
    heads[fits] <- vapply(pieces[fits], function(p) p[1], character(1))
    shared <- fits & heads %in% heads[fits][duplicated(heads[fits])]
    for (i in which(shared)) {
      p <- pieces[[i]]
      out$construct[i] <- p[1]
      out$facet[i] <- if (length(p) == 3L) p[2] else NA_character_
      out$wording[i] <- p[length(p)]
      out$source[i] <- "label"
    }
  }
  clash <- !is.na(out$construct) & out$construct %in% as.character(taken)
  if (any(clash)) {
    attr(out, "withheld") <- unique(out$construct[clash])
    out$construct[clash] <- NA_character_
    out$facet[clash] <- NA_character_
    out$wording[clash] <- NA_character_
    out$source[clash] <- NA_character_
  }
  out
}

#' Why a construct carried in the names was not proposed
#'
#' @param withheld The construct names withheld (see
#'   `sieve_structure_from_names()`).
#' @return One sentence.
#' @author Ken Kelley
#' @noRd
sieve_withheld_text <- function(withheld) {
  one <- length(withheld) == 1L
  sprintf(
    paste(
      "The construct%s %s carried in the column names %s the name of a variable in the data,",
      "so %s not proposed (a construct cannot have a variable's name): the items stay",
      "Unassigned until the construct is named in the grid."
    ),
    if (one) "" else "s", sieve_name_some(withheld), if (one) "has" else "have",
    if (one) "it was" else "they were"
  )
}

#' The Item Names, the keying statements and the structure the labels carry
#'
#' For the item columns of a loaded data frame: the variable label of each
#' (the Item Name on load), with the keying statement SIEVER's own processed
#' export writes read off it (`sieve_label_keying()`), and the constructs,
#' facets and wordings the names or the labels carry
#' (`sieve_structure_from_names()`). An item whose label names its
#' construct gets the wording part as its Item Name.
#'
#' @param item_cols The item column names.
#' @param var_labels The variable labels of every column, named by column
#'   (see `sieve_var_labels()`).
#' @return A list with `labels` (a character vector named by item),
#'   `keyed` (a list of `sieve_label_keying()` results named by item) and
#'   `structure` (the proposal, see `sieve_structure_from_names()`).
#' @author Ken Kelley
#' @noRd
sieve_item_labels <- function(item_cols, var_labels) {
  keyed <- lapply(var_labels[item_cols], sieve_label_keying)
  names(keyed) <- item_cols
  stripped <- vapply(keyed, function(k) as.character(k$label), character(1))
  structure <- sieve_structure_from_names(item_cols, stripped, taken = names(var_labels))
  labels <- stripped
  from_label <- !is.na(structure$source) & structure$source == "label"
  labels[from_label] <- structure$wording[from_label]
  labels[is.na(labels)] <- item_cols[is.na(labels)]
  names(labels) <- item_cols
  list(labels = labels, keyed = keyed, structure = structure)
}

#' A proposed structure in words
#'
#' @param proposed The rows of a `sieve_structure_from_names()` result that
#'   carry a proposal.
#' @return A character vector of lines: what the proposal came from, then
#'   one line per construct with its item count and facets.
#' @author Ken Kelley
#' @noRd
sieve_structure_text <- function(proposed) {
  if (!NROW(proposed)) {
    return(character(0))
  }
  from <- c(
    name = "the column names (Construct__Facet__item)",
    label = "the variable labels (Construct: Facet: wording)",
    file = "the description rows under the header (construct, facet)"
  )
  by <- split(proposed, factor(proposed$construct, levels = unique(proposed$construct)))
  c(
    sprintf(
      "Constructs%s proposed from %s, for review:",
      if (any(!is.na(proposed$facet))) " and facets" else "",
      paste(from[unique(proposed$source)], collapse = " and ")
    ),
    vapply(names(by), function(cn) {
      b <- by[[cn]]
      facets <- unique(b$facet[!is.na(b$facet)])
      sprintf(
        "%s: %d item%s%s", cn, nrow(b), if (nrow(b) == 1L) "" else "s",
        if (length(facets)) sprintf(" (facets %s)", paste(facets, collapse = ", ")) else ""
      )
    }, character(1), USE.NAMES = FALSE)
  )
}

#' Place a data frame in the session as the Ingest tab loads an upload
#'
#' Everything the load observer does to the shared state for an uploaded
#' file, as a plain function, so that the pre-SIEVER step loads a study
#' exactly as the tab does. A role is proposed for every column
#' (`sieve_propose_roles()`); the item columns become the item data and the
#' ledger; the variable labels seed the Item Names; constructs and facets
#' carried in the names or the labels are written into the ledger as a
#' proposal (`sieve_structure_from_names()`); the load is logged; and an
#' item whose label states that its values are reverse scored, in the form
#' SIEVER's processed export writes, is recorded as such with the bounds the
#' label gives, with no value changed. A statement the values contradict is
#' not recorded, and the item is loaded as administered.
#'
#' When no column is proposed as an item, nothing in the session changes
#' and the reason is returned.
#'
#' A file that carried description rows under its header
#' (`sieve_description_rows()`) is loaded with the constructs and facets
#' they give (they replace what a column name or a label proposed), the
#' question row as each item's wording (the Question column; the Item Name
#' stays the variable's name or label, since the question is the full
#' text), and each item its reverse row marks is settled the way a
#' specification's `verify` rule settles it, never by
#' a silent second reversal of a file that already holds the reversal:
#'
#' * a `reverse_applied` row that says yes records the item as already
#'   reverse scored, with no value changed (`sieve_mark_reversed()`);
#' * one that says no reverse scores it through the transformation
#'   service, within its observed range unless bounds are stored for it,
#'   with the record and the R that every reversal gets;
#' * without a `reverse_applied` entry the item's values are compared with
#'   the rest of its construct (the construct's items the row does not
#'   mark, or every unmarked item when the item has no construct): an item
#'   whose item-rest correlation is negative, as an administered reverse
#'   keyed item's is, is reverse scored; one whose correlation is already
#'   positive, as values that hold the reversal give, is not reversed and
#'   is held for the researcher's answer (`described$held`); an item the
#'   comparison cannot be made for is reverse scored and named as unchecked.
#'
#' @param state The shared state store.
#' @param df The data frame as read (variable labels still attached).
#' @param name The name the data are known by (the file name).
#' @param code The R that reads the data, for the session script.
#' @param note The note the load is logged under.
#' @param description The description rows the file carried (see
#'   `sieve_description_rows()`), or `NULL`.
#' @return Invisibly, a list with `ok`; when `FALSE`, `problem` (one
#'   sentence) and `roles`; when `TRUE`, `roles`, `structure` (the rows of
#'   the proposal that were applied), `withheld` (the constructs the names
#'   carried that were not proposed because a variable has their name),
#'   `keyed` (the label keying of each item), `stated` (a list with
#'   `steps`, the keying records made from the labels, and `refused`, the
#'   reasons named by item for the statements that were not recorded), and
#'   `described` (a list with `found`, `reversed` (the items the reverse
#'   row had reverse scored), `refused` (reasons named by item for the ones
#'   that could not be), `ignored` (columns the rows describe that are not
#'   items), `bad_reverse` (reverse cells outside the vocabulary),
#'   `questions` (the items whose wording the question row gave), `held`
#'   (a named numeric vector: the item-rest correlation of each marked item
#'   that was not reversed because its values already correlate positively
#'   with the rest of its construct), `unchecked` (marked items reverse
#'   scored without the comparison, which could not be made), `checked` (the
#'   item-rest correlation of each marked item reverse scored after the
#'   comparison) and `recorded` (the items a reverse_applied row said were
#'   already reverse scored)).
#' @param record Whether to record the import immediately.
#' @author Ken Kelley
#' @noRd
sieve_load_frame <- function(state, df, name, code = NULL,
                             note = sprintf("Read data file: %s", name), description = NULL,
                             record = TRUE) {
  shiny::isolate({
    var_labels <- sieve_var_labels(df)
    df <- as.data.frame(df)
    roles <- sieve_propose_roles(df)
    item_cols <- roles$variable[roles$role == "item"]
    if (!length(item_cols)) {
      return(invisible(list(ok = FALSE, roles = roles, problem = sprintf(
        paste(
          "%s was not loaded: none of its %d columns was proposed as an item (%s).",
          "A file needs at least one numeric column that is not an identifier, a grouping",
          "variable or a background variable. The session is unchanged."
        ),
        name, nrow(roles), sieve_roles_summary(roles)
      ))))
    }
    found <- sieve_item_labels(item_cols, var_labels)
    found$structure <- sieve_describe_structure(found$structure, description, taken = names(df))
    withheld <- attr(found$structure, "withheld") %||% character(0)
    proposed <- found$structure[!is.na(found$structure$source), , drop = FALSE]
    rownames(proposed) <- NULL
    described <- list(
      found = !is.null(description), reversed = character(0), refused = character(0),
      ignored = character(0), bad_reverse = character(0), questions = character(0),
      held = numeric(0), unchecked = character(0), checked = numeric(0), recorded = character(0)
    )
    questions <- NULL
    if (!is.null(description)) {
      asked <- !is.na(description$wording %||% rep(NA_character_, nrow(description)))
      says <- description[!is.na(description$construct) | description$reverse %in% TRUE | asked, , drop = FALSE]
      described$ignored <- setdiff(says$variable, item_cols)
      described$bad_reverse <- attr(description, "bad_reverse") %||% character(0)
      questions <- description[asked & description$variable %in% item_cols, c("variable", "wording"), drop = FALSE]
      described$questions <- as.character(questions$variable)
    }
    sieve_reset_dataset(state)
    state$dataset_name <- name
    state$raw <- df
    state$roles <- roles
    state$held_rows <- list()

    state$items <- df[, item_cols, drop = FALSE]
    meta <- sieve_build_meta(df, item_cols, labels = found$labels)
    if (nrow(proposed)) {
      at <- match(proposed$variable, meta$item)
      meta$construct[at] <- proposed$construct
      meta$facet[at] <- proposed$facet
    }
    # The question row is the item's wording (the Question column); the
    # Item Name stays the variable's name or label.
    if (NROW(questions)) {
      at <- match(questions$variable, meta$item)
      meta$wording[at] <- as.character(questions$wording)
    }
    sieve_write_ledger(state, meta)
    state$theory <- NULL
    state$confirmed <- FALSE
    other <- roles[roles$role != "item", , drop = FALSE]
    if (isTRUE(record)) sieve_log(
      state,
      note,
      c(
        code,
        sprintf("# Variable roles proposed on reading, for review: %s.", sieve_roles_summary(roles)),
        if (nrow(other)) sprintf("#   %s: %s", other$variable, other$role),
        if (nrow(proposed)) {
          lines <- sieve_structure_text(proposed)
          c(paste("#", lines[1]), paste("#  ", lines[-1]))
        },
        if (length(withheld)) paste("#", sieve_withheld_text(withheld)),
        if (any(meta$rev_bounds %in% "declared")) {
          by_labels <- meta$rev_bounds %in% "declared"
          c(
            "# Response bounds declared by the value labels (lowest and highest labeled value):",
            sprintf("#   %s: %s to %s", meta$item[by_labels], sieve_num_text(meta$rev_lo[by_labels]), sieve_num_text(meta$rev_hi[by_labels]))
          )
        },
        if (length(described$ignored)) {
          sprintf(
            "# The description rows describe %s, which %s not %s, so that part was not used.",
            sieve_name_some(described$ignored), if (length(described$ignored) == 1L) "is" else "are",
            if (length(described$ignored) == 1L) "an item" else "items"
          )
        }
      ),
      kind = "import"
    )
    steps <- list()
    refused <- character(0)
    stated <- item_cols[vapply(found$keyed, function(k) isTRUE(k$reversed), logical(1))]
    if (length(stated)) {
      meta_now <- state$meta
      for (it in stated) {
        k <- found$keyed[[it]]
        mark <- tryCatch(
          sieve_mark_reversed(
            state$items, meta_now, it, k$lo, k$hi,
            if (is.na(k$bounds)) "declared" else k$bounds
          ),
          sieve_transform_error = function(e) e
        )
        if (inherits(mark, "sieve_transform_error")) {
          refused[it] <- conditionMessage(mark)
          next
        }
        meta_now <- mark$meta
        steps[[it]] <- mark
      }
      if (length(steps)) {
        before <- sieve_revision(state)
        sieve_write_ledger(state, meta_now)
        sieve_record_steps(state, steps,
          origin = "load", before = before,
          after = sieve_revision(state)
        )
      }
    }
    # The reverse row of the description: each item it marks is settled
    # the way the specification's verify rule settles it (see the
    # description above), and an item that is reverse scored is reversed
    # through the service, so the reversal has its record and its R like
    # any other. An item whose label already stated that its values are
    # reverse scored is left as the label says.
    if (!is.null(description)) {
      marked <- description$variable[description$reverse %in% TRUE]
      marked <- intersect(marked, item_cols)
      marked <- setdiff(marked, names(steps))
      if (length(marked)) {
        said_applied <- description$reverse_applied[match(marked, description$variable)]
        if (is.null(said_applied)) said_applied <- rep(NA, length(marked))
        already <- marked[said_applied %in% TRUE]
        stated_not <- marked[said_applied %in% FALSE]
        open <- marked[is.na(said_applied)]
        evidence <- sieve_reverse_evidence(state$items, state$meta, open, marked)
        r_open <- evidence$r[intersect(open, names(evidence$r))]
        held <- r_open[r_open > 0]
        described$held <- held
        described$unchecked <- setdiff(open, names(evidence$r))
        described$checked <- r_open[r_open <= 0]
        to_reverse <- c(stated_not, setdiff(open, names(held)))
        if (length(already)) {
          rec <- sieve_record_already_reversed(state, already, origin = "load")
          described$recorded <- rec$recorded
          described$refused <- c(described$refused, rec$refused)
        }
        if (length(to_reverse)) {
          out <- sieve_apply_reverse(state, to_reverse, origin = "load")
          described$reversed <- names(out$applied)
          described$refused <- c(described$refused, out$refused)
        }
      }
    }
    invisible(list(
      ok = TRUE, roles = roles, structure = proposed, withheld = withheld,
      keyed = found$keyed, stated = list(steps = steps, refused = refused),
      described = described
    ))
  })
}

#' How marked items correlate with the rest of their construct
#'
#' The evidence the load reads when a description row marks items as
#' reverse keyed and the file does not say whether they already hold the
#' reversal: the correlation of each item with the mean of the rest of its
#' construct (the construct's items that are not marked, on the rows
#' where they are observed; every unmarked item of the pool when the item
#' has no construct). An administered reverse keyed item correlates
#' negatively with the rest; values that already hold the reversal
#' correlate positively.
#'
#' @param items The item data.
#' @param meta The item ledger (its `construct` column).
#' @param check The items to compute the correlation for.
#' @param marked Every item the reverse row marks (left out of the rest).
#' @return A list with `r` (a named numeric vector of the item-rest
#'   correlations that could be computed) and `rest` (named list: the items
#'   each correlation used).
#' @author Ken Kelley
#' @noRd
sieve_reverse_evidence <- function(items, meta, check, marked = check) {
  r <- numeric(0)
  rests <- list()
  for (it in check) {
    if (!it %in% names(items)) next
    cn <- as.character(meta$construct[match(it, meta$item)])
    pool <- setdiff(intersect(as.character(meta$item[meta$status == "active"]), names(items)), marked)
    rest <- if (is.na(cn) || cn %in% c("", "Unassigned")) {
      pool
    } else {
      intersect(pool, as.character(meta$item[meta$construct %in% cn]))
    }
    if (!length(rest)) next
    x <- sieve_screen_numeric(items[[it]])
    y <- rowMeans(as.matrix(as.data.frame(lapply(items[rest], sieve_screen_numeric))), na.rm = TRUE)
    y[!is.finite(y)] <- NA_real_
    ok <- !is.na(x) & !is.na(y)
    if (sum(ok) < 3L || stats::sd(x[ok]) == 0 || stats::sd(y[ok]) == 0) next
    r[it] <- stats::cor(x[ok], y[ok])
    rests[[it]] <- rest
  }
  list(r = r, rest = rests)
}

#' Record items as already reverse scored in the data as loaded
#'
#' For items the file says it holds reverse scored (a `reverse_applied`
#' description row that says yes, or the researcher's answer on the Ingest
#' tab): the ledger records the keying with no value changed, with the
#' bounds stored for the item when there are any, through
#' `sieve_mark_reversed()`, and each record is logged.
#'
#' @param state The shared state store.
#' @param items Item names.
#' @param origin Kept in the records.
#' @return Invisibly, a list with `recorded` and `refused` (reasons named
#'   by item).
#' @author Ken Kelley
#' @noRd
sieve_record_already_reversed <- function(state, items, origin = "load") {
  shiny::isolate({
    meta_now <- sieve_meta_keying(state$meta)
    steps <- list()
    refused <- character(0)
    for (it in items) {
      i <- match(it, meta_now$item)
      lo <- as.numeric(meta_now$rev_lo[i])
      hi <- as.numeric(meta_now$rev_hi[i])
      label <- as.character(meta_now$rev_bounds[i])
      mark <- tryCatch(
        sieve_mark_reversed(state$items, meta_now, it, lo, hi, if (is.na(label)) "declared" else label),
        sieve_transform_error = function(e) e
      )
      if (inherits(mark, "sieve_transform_error")) {
        refused[it] <- conditionMessage(mark)
        next
      }
      meta_now <- mark$meta
      steps[[it]] <- mark
    }
    if (length(steps)) {
      sieve_ensure_dataset(state)
      before <- sieve_revision(state)
      sieve_write_ledger(state, meta_now)
      sieve_record_steps(state, steps, origin = origin, before = before, after = sieve_revision(state))
    }
    invisible(list(recorded = names(steps), refused = refused))
  })
}

#' The marked items held at load, in words
#'
#' @param held The `held` element of a load's `described` list.
#' @return One paragraph, or `NULL` when nothing is held.
#' @author Ken Kelley
#' @noRd
sieve_reverse_held_text <- function(held) {
  n <- length(held)
  if (!n) {
    return(NULL)
  }
  one <- n == 1L
  sprintf(
    paste(
      "The reverse row marks %s as reverse keyed, but %s already correlate%s positively with the rest of",
      "%s construct (item-rest r %s), as values that already hold the reversal do, so %s not reverse",
      "scored on loading. If the file holds %s as administered, reverse %s now; if it already holds %s",
      "reverse scored, record %s as such (no value changes). A reverse_applied row (yes or no) under the",
      "header settles this when the file is loaded."
    ),
    if (one) sprintf("1 item (%s)", names(held)) else sprintf("%d items (%s)", n, sieve_name_some(names(held))),
    if (one) "its values" else "their values", "",
    if (one) "its" else "their",
    if (one) sprintf("%.2f", held) else sprintf("%.2f to %.2f", min(held), max(held)),
    if (one) "it was" else "they were",
    if (one) "it" else "them", if (one) "it" else "them", if (one) "it" else "them", if (one) "it" else "them"
  )
}

#' Record the confirmation of the item types
#'
#' The estimator in force for the study is written to the state with the
#' confirmation flag: the one the active items' types resolve to, or the
#' one the study's Estimator control (step 4 of the Ingest tab,
#' `state$estimator_choice`) names (`sieve_estimator_resolve()`). The decision
#' is logged with the counts by type, the roles, the estimator setting and
#' the rationale. The ways the confirmation is reached are logged by name:
#' the button, the automatic confirmation of a bundled example whose items
#' are all continuous, a specification that types every item, and the
#' confirmation made again after items typed ordered with five or more
#' response options were switched to continuous (`"rule"`, see
#' `sieve_apply_option_rule()`).
#'
#' @param state The shared state store.
#' @param origin `"button"`, `"auto"`, `"specification"`, or `"rule"`.
#' @return Invisibly, the estimator list from `sieve_estimator_resolve()`.
#' @author Ken Kelley
#' @noRd
sieve_confirm_types <- function(state, origin = c("button", "auto", "specification", "rule")) {
  origin <- match.arg(origin)
  shiny::isolate({
    meta <- state$meta
    if (is.null(meta)) {
      stop("No data are loaded, so there are no item types to confirm.", call. = FALSE)
    }
    choice <- sieve_estimator_effective("study", state)
    e <- sieve_estimator_resolve(meta$type[meta$status == "active"], n = NROW(state$items), choice = choice)
    state$estimator <- e$label
    state$confirmed <- TRUE
    sieve_remember_confirmation(state, origin)
    # The counts describe the included items, the ones the sentence below
    # counts and the estimator decision reads; an excluded item's type is
    # not part of the decision.
    active <- meta$status == "active"
    n_cont <- sum(meta$type[active] == "continuous")
    n_ord <- sum(meta$type[active] == "ordered")
    n_bin <- sum(meta$type[active] == "binary")
    roles <- sieve_roles_table(state$raw, state$items, state$roles)
    not_items <- if (is.null(roles)) NULL else roles[roles$role != "item", , drop = FALSE]
    basis <- state$construct_basis %||% "theoretical"
    note <- switch(origin,
      button = sprintf(
        "Confirm item types (%s construct basis); estimator = %s on a %s matrix",
        basis, e$label, e$basis
      ),
      auto = sprintf(
        "Auto-confirm item types (%s construct basis); estimator = %s on a %s matrix",
        basis, e$label, e$basis
      ),
      specification = sprintf(
        "Confirm item types from the specification (%s construct basis); estimator = %s on a %s matrix",
        basis, e$label, e$basis
      ),
      rule = sprintf(
        "Confirm item types again after the five-option rule (%s construct basis); estimator = %s on a %s matrix",
        basis, e$label, e$basis
      )
    )
    sieve_log(
      state,
      note,
      c(
        sprintf(
          "# Measurement levels confirmed for %d active items: %d continuous, %d ordered, %d binary.",
          sum(active), n_cont, n_ord, n_bin
        ),
        if (identical(origin, "specification")) {
          "# The specification typed every item, so the study was confirmed from it."
        },
        if (identical(origin, "rule")) {
          paste(
            "# The study was confirmed when it was opened; items typed ordered with five or more",
            "response options were switched to continuous, so it is confirmed again with them."
          )
        },
        sprintf("# Estimator decision: %s on a %s correlation matrix.", e$label, e$basis),
        if (!identical(choice, "auto")) {
          sprintf(
            "# Set for every model in the study with the Estimator control (Ingest tab, step 4): %s.",
            sieve_estimator_choice_text(choice)
          )
        },
        if (!is.null(roles)) sprintf("# Variable roles: %s.", sieve_roles_summary(roles)),
        if (NROW(not_items)) {
          sprintf(
            "# Not items: %s.",
            paste(sprintf("%s (%s)", not_items$variable, not_items$role), collapse = ", ")
          )
        },
        # The reason the resolution gives (sieve_estimator_resolve()): the
        # rule applied, the alternative, and the small sample caution.
        sieve_comment_lines("Rationale:", e$reason),
        "# An item with five or more scale points is proposed as continuous: with five or more",
        "# categories, estimation that treats items as continuous performs comparably to the",
        "# categorical estimator in many conditions, especially with roughly symmetric",
        "# thresholds; with fewer categories or strongly asymmetric thresholds the categorical",
        "# estimator is more accurate (Rhemtulla, Brosseau-Liard & Savalei, 2012)."
      )
    )
    invisible(e)
  })
}

#' Read items typed ordered with five or more response options as continuous
#'
#' The five-option rule (`sieve_option_rule()`) applied to the session, for
#' a project that was just opened (see `sieve_project_open()`): every item
#' typed ordered with `min_options` or more response options whose type is
#' proposed is typed continuous, in one ledger write, and the switch is
#' logged with the items, their response options and the reason; an
#' ordered type the researcher chose is kept. The confirmation is
#' withdrawn (a type changed);
#' `sieve_project_open()` confirms a project that was opened confirmed
#' again at once, since the switch is the rule the researcher asked for.
#'
#' @param state The shared state store.
#' @param min_options The fewest response options read as continuous, or
#'   `NULL` to leave every type as it is.
#' @param source Where the ordered types came from, in words.
#' @return Invisibly, the `sieve_option_rule()` result (`switched`, `kept`,
#'   `options`) with `was_confirmed` (whether the study was confirmed before
#'   the switch) and `text` (the sentence for the interface, or `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_apply_option_rule <- function(state, min_options = 5L, source = "the project") {
  shiny::isolate({
    meta <- state$meta
    items <- state$items
    was <- sieve_confirmation_status(state)$confirmed
    none <- list(switched = character(0), kept = character(0), options = integer(0), was_confirmed = was, text = NULL)
    if (is.null(meta) || is.null(items) || is.null(min_options)) {
      return(invisible(none))
    }
    found <- sieve_option_rule(items, meta, min_options = min_options)
    found$was_confirmed <- was
    found$text <- sieve_option_rule_text(found$switched, source)
    if (!length(found$switched)) {
      return(invisible(found))
    }
    replay_before <- sieve_replay_inputs(state)
    at <- match(found$switched, meta$item)
    meta$type[at] <- "continuous"
    # The switch is the rule's proposal, so the type stays proposed.
    meta <- sieve_mark_type_source(meta, found$switched, "proposed")
    sieve_write_ledger(state, meta)
    state$confirmed <- FALSE
    n <- length(found$switched)
    quoted <- paste(sprintf("\"%s\"", found$switched), collapse = ", ")
    sieve_log(
      state,
      sprintf(
        "Read %d item%s typed ordered in %s as continuous (five or more response options; the types were proposed, not chosen)",
        n, if (n == 1L) "" else "s", source
      ),
      c(
        "# The five-option rule (Ingest tab, step 1): an item typed ordered with five or more",
        "# response options (its distinct values, or the points its declared bounds span) whose",
        "# type was proposed (detected, or read from a specification) is treated as continuous.",
        "# With five or more categories, estimation that treats items as continuous performs",
        "# comparably to the categorical estimator in many conditions, especially with roughly",
        "# symmetric thresholds (Rhemtulla, Brosseau-Liard & Savalei, 2012).",
        sprintf("#   %s (%d options): continuous", found$switched, found$options[found$switched]),
        if (length(found$kept)) sprintf("# Kept ordered as the researcher's choice: %s.", paste(found$kept, collapse = ", ")),
        sprintf("meta$type[meta$item %%in%% c(%s)] <- \"continuous\"", quoted)
      ),
      before = replay_before, kind = "mutation"
    )
    invisible(found)
  })
}

#' Set the type of chosen items at once
#'
#' The one change behind the Ingest tab's "Set the type of" control (every
#' included item, a construct's items, or a facet's), its "Set these items to
#' ordered" button under the SPSS note, and the button that keeps items the
#' five-option rule switched on opening a project ordered: the ledger type of
#' each named item becomes `to` and is recorded as the researcher's choice
#' (`type_source = "chosen"`), the confirmation is withdrawn as a Type cell
#' edit withdraws it (the estimator decision changes), and the step is written
#' to the session script as the assignment that reproduces it. Items already
#' of that type are recorded as chosen and otherwise left alone; names not in
#' the ledger are left alone.
#'
#' @param state The shared state store.
#' @param items Item (variable) names.
#' @param to `"ordered"` or `"continuous"`.
#' @param what The items in words, for the log and the note (for example
#'   `"every included item"` or `"the items of construct Trust"`).
#' @return Invisibly, a list with `changed` and `text` (one sentence for
#'   the line above the grid).
#' @author Ken Kelley
#' @noRd
sieve_set_types <- function(state, items, to = c("ordered", "continuous"), what = "the chosen items") {
  to <- match.arg(to)
  shiny::isolate({
    meta <- state$meta
    if (is.null(meta)) {
      return(invisible(list(changed = character(0), text = "Load the data first (step 2).")))
    }
    items <- intersect(unique(as.character(items)), meta$item)
    changed <- items[meta$type[match(items, meta$item)] != to]
    if (!length(changed)) {
      # The researcher's request still says what the types are to be.
      if (length(items) && any(sieve_type_source(meta)[match(items, meta$item)] != "chosen")) {
        sieve_write_ledger(state, sieve_mark_type_source(meta, items, "chosen"))
      }
      return(invisible(list(changed = character(0), text = sprintf(
        "Every one of %s is typed %s already, so no type was changed.", what, to
      ))))
    }
    meta$type[match(changed, meta$item)] <- to
    meta <- sieve_mark_type_source(meta, items, "chosen")
    sieve_write_ledger(state, meta)
    state$confirmed <- FALSE
    n <- length(changed)
    quoted <- paste(sprintf("\"%s\"", changed), collapse = ", ")
    sieve_log(
      state,
      sprintf("Set the type of %d item%s (%s) to %s", n, if (n == 1L) "" else "s", what, to),
      c(
        sprintf("meta$type[meta$item %%in%% c(%s)] <- \"%s\"", quoted, to),
        "# Recorded as the researcher's choice (type_source = \"chosen\")."
      )
    )
    invisible(list(changed = changed, text = sprintf(
      "%s of %s %s typed %s now; the study is confirmed again in step 4.",
      if (n == 1L) sprintf("1 item (%s)", changed) else sprintf("%d items (%s)", n, sieve_name_some(changed)),
      what, if (n == 1L) "is" else "are", to
    )))
  })
}

#' Set the type of many items at once by their number of response options
#'
#' The two buttons above the item grid: `to = "continuous"` types every
#' item typed ordered that has five or more response options as
#' continuous (the five-option rule, `sieve_option_rule()`), and `to =
#' "ordered"` types every item typed continuous that has two to four
#' response options as ordered. Items typed binary are left alone. A change
#' withdraws the confirmation (the estimator decision follows the types)
#' and is logged; when no item qualifies nothing changes and the note says
#' so.
#'
#' @param state The shared state store.
#' @param to `"continuous"` or `"ordered"`.
#' @return Invisibly, a list with `changed` (the items retyped) and `text`
#'   (one sentence for the line above the grid).
#' @author Ken Kelley
#' @noRd
sieve_bulk_type <- function(state, to = c("continuous", "ordered")) {
  to <- match.arg(to)
  shiny::isolate({
    meta <- state$meta
    items <- state$items
    if (is.null(meta) || is.null(items)) {
      return(invisible(list(changed = character(0), text = "Load the data first (step 2).")))
    }
    tab <- sieve_item_options(items, meta)
    # The researcher's own request: an ordered type chosen earlier is
    # switched too.
    changed <- if (identical(to, "continuous")) {
      sieve_option_rule(items, meta, min_options = 5L, respect_chosen = FALSE)$switched
    } else {
      tab$item[tab$type %in% "continuous" & tab$options >= 2L & tab$options <= 4L]
    }
    rule <- if (identical(to, "continuous")) "five or more response options" else "four or fewer response options"
    if (!length(changed)) {
      text <- if (identical(to, "continuous")) {
        "No item typed ordered has five or more response options, so no type was changed."
      } else {
        "No item typed continuous has four or fewer response options, so no type was changed."
      }
      return(invisible(list(changed = character(0), text = text)))
    }
    at <- match(changed, meta$item)
    meta$type[at] <- to
    meta <- sieve_mark_type_source(meta, changed, "chosen")
    sieve_write_ledger(state, meta)
    state$confirmed <- FALSE
    n <- length(changed)
    options <- tab$options[match(changed, tab$item)]
    sieve_log(
      state,
      sprintf("Set the type of %d item%s with %s to %s", n, if (n == 1L) "" else "s", rule, to),
      sprintf("#   %s (%d options): %s", changed, options, to)
    )
    invisible(list(changed = changed, text = sprintf(
      "%s with %s %s typed %s now; the study is confirmed again in step 4.",
      if (n == 1L) sprintf("1 item (%s)", changed) else sprintf("%d items (%s)", n, sieve_name_some(changed)),
      rule, if (n == 1L) "is" else "are", to
    )))
  })
}

#' Include or exclude whole constructs in one ledger write
#'
#' The constructs to analyze: every item of a construct in `include` is
#' included (status active) and every item of the other constructs is
#' excluded (status parked, kept in the item data, never deleted), in one
#' write of the ledger, logged with the R that makes the same change in a
#' replay. The confirmation is left as it is: an inclusion or an exclusion
#' keeps the analysis tabs unlocked, as it does from the grid, and the
#' Confirm button reports the change. Items with no construct are the
#' construct `Unassigned` here.
#'
#' @param state The shared state store.
#' @param include The constructs to analyze.
#' @return Invisibly, a list with `ok`; when `FALSE`, `problem`; when
#'   `TRUE`, `include`, `exclude` (the constructs set aside), `changed`
#'   (the items whose status changed), `n_active` and `n`.
#' @author Ken Kelley
#' @noRd
sieve_apply_constructs <- function(state, include) {
  shiny::isolate({
    meta <- state$meta
    if (is.null(meta)) {
      return(invisible(list(ok = FALSE, problem = "No data are loaded.")))
    }
    cn <- as.character(meta$construct)
    cn[is.na(cn) | !nzchar(cn)] <- "Unassigned"
    all_c <- unique(cn)
    include <- intersect(all_c, as.character(include))
    if (!length(include)) {
      return(invisible(list(ok = FALSE, problem = "At least one construct has to be analyzed: tick one, then Apply.")))
    }
    exclude <- setdiff(all_c, include)
    status <- ifelse(cn %in% include, "active", "parked")
    changed <- meta$item[status != meta$status]
    out <- list(
      ok = TRUE, include = include, exclude = exclude, changed = changed,
      n_active = sum(status == "active"), n = nrow(meta)
    )
    if (!length(changed)) {
      return(invisible(out))
    }
    sieve_set_item_status(state, meta$item, status, reason = "Constructs selected for analysis",
      label = sprintf("Analyze %d of %d constructs: %s (%d of %d items included)",
        length(include), length(all_c), paste(include, collapse = ", "), out$n_active, out$n))

    invisible(out)
  })
}

#' The included items in words, by measurement type
#'
#' @param meta The item ledger.
#' @return A length one character string such as `"68 ordered items"`,
#'   `"60 ordered and 8 binary items"` or
#'   `"6 continuous, 2 ordered and 1 binary items"`.
#' @author Ken Kelley
#' @noRd
sieve_types_text <- function(meta) {
  active <- meta[meta$status == "active", , drop = FALSE]
  counts <- vapply(c("continuous", "ordered", "binary"), function(t) sum(active$type == t), integer(1))
  counts <- counts[counts > 0L]
  if (length(counts)) {
    parts <- sprintf("%d %s", counts, names(counts))
    listed <- if (length(parts) > 1L) {
      paste(paste(parts[-length(parts)], collapse = ", "), parts[length(parts)], sep = " and ")
    } else {
      parts
    }
    sprintf("%s item%s", listed, if (sum(counts) == 1L) "" else "s")
  } else {
    sprintf("%d item%s", nrow(active), if (nrow(active) == 1L) "" else "s")
  }
}

#' The item pool in words: types, constructs and facets
#'
#' @param meta The item ledger.
#' @return A length one character string such as
#'   `"68 ordered items, 1 construct, 6 facets"`, counting the included
#'   items. A facet belongs to its construct, so two constructs that each
#'   have a facet of the same name count as two facets.
#' @author Ken Kelley
#' @noRd
sieve_pool_text <- function(meta) {
  active <- meta[meta$status == "active", , drop = FALSE]
  constructs <- unique(as.character(active$construct)[sieve_construct_assigned(active$construct)])
  facet <- as.character(active$facet)
  has_facet <- !is.na(facet) & nzchar(facet)
  facets <- unique(paste(as.character(active$construct)[has_facet], facet[has_facet], sep = "\r"))
  sprintf(
    "%s, %d construct%s, %d facet%s", sieve_types_text(meta), length(constructs),
    if (length(constructs) == 1L) "" else "s", length(facets), if (length(facets) == 1L) "" else "s"
  )
}

# ---- Several files at once ---------------------------------------------------

#' Whether a comma separated file is a specification, by its header
#'
#' The header is matched byte by byte, since the column name is ASCII and
#' the file may not be UTF-8: `tolower()` of a header in windows-1252
#' (what Excel on Windows saves as CSV (Comma delimited)) with an accented
#' column name stops with "invalid input in 'utf8towcs'", which, inside the
#' observer of the chosen files, would end the session before the Load
#' could refuse the file with the encoding to choose.
#'
#' @param path A file.
#' @return `TRUE` when the first line names a `siever_specification` column
#'   (in any case).
#' @author Ken Kelley
#' @noRd
sieve_spec_header <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path)) {
    return(FALSE)
  }
  first <- sieve_capture(readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8"))
  isTRUE(first$ok) && length(first$value) == 1L &&
    grepl("siever_specification", first$value, ignore.case = TRUE, useBytes = TRUE)
}

#' Tell apart the files that arrive in the data file input
#'
#' The data file input of the Ingest tab takes one file or two: a project
#' file on its own (`.siever.rds`, `.siever` or `.rds`), a data file on its
#' own (`.sav`, `.dta`, `.csv`, `.txt`), a data file with its specification
#' (a `.csv` whose name ends in `specification.csv` or whose header names
#' the `siever_specification` column), or a specification on its own for
#' data already loaded. Anything else is refused with the reason, and
#' nothing is read beyond the first line of a `.csv`.
#'
#' @param names The names the files were uploaded under.
#' @param paths Where they are stored.
#' @return A list with `project`, `data` and `spec` (each `NULL` or a list
#'   with `name` and `path`) and `problem` (`NULL`, or one sentence saying
#'   why the files cannot be used together).
#' @author Ken Kelley
#' @noRd
sieve_sort_uploads <- function(names, paths) {
  names <- as.character(names)
  paths <- as.character(paths)
  out <- list(project = NULL, data = NULL, spec = NULL, problem = NULL)
  if (!length(names) || length(names) != length(paths)) {
    out$problem <- "No file was chosen. Choose a data file, a data file with its specification, or a project file."
    return(out)
  }
  ext <- tolower(tools::file_ext(names))
  is_project <- ext %in% c("rds", "siever")
  is_spec <- vapply(seq_along(names), function(i) {
    ext[i] == "csv" && (grepl("specification\\.csv$", tolower(names[i])) || sieve_spec_header(paths[i]))
  }, logical(1))
  is_data <- !is_project & !is_spec & ext %in% c("csv", "txt", "sav", "dta")
  other <- !is_project & !is_spec & !is_data
  one <- function(which) list(name = names[which][1], path = paths[which][1])
  if (any(other)) {
    out$problem <- sprintf(
      paste(
        "%s is not a file SIEVER reads. Choose a data file (.sav, .dta, .csv or .txt), a data",
        "file together with its specification (a .csv whose name ends in specification.csv),",
        "or a project file (.siever.rds)."
      ),
      sieve_name_some(names[other])
    )
    return(out)
  }
  if (sum(is_project) > 1L || (any(is_project) && length(names) > 1L)) {
    out$problem <- paste(
      "A project file opens on its own: choose one project file and nothing with it",
      "(the project already holds the data and the specification)."
    )
    return(out)
  }
  if (sum(is_data) > 1L) {
    out$problem <- sprintf(
      "Choose one data file at a time (%s were chosen), with its specification when there is one.",
      sieve_name_some(names[is_data])
    )
    return(out)
  }
  if (sum(is_spec) > 1L) {
    out$problem <- sprintf("Choose one specification (%s were chosen).", sieve_name_some(names[is_spec]))
    return(out)
  }
  if (any(is_project)) {
    out$project <- one(is_project)
  }
  if (any(is_data)) {
    out$data <- one(is_data)
  }
  if (any(is_spec)) {
    out$spec <- one(is_spec)
  }
  out
}

# ---- The pre-SIEVER step: template and project ------------------------------

#' The data behind a data frame or a path
#'
#' @param data A data frame, or the path of a `.csv`, `.txt`, `.sav` or
#'   `.dta` file.
#' @param name The name the data are known by, or `NULL` for the file name
#'   (or the expression, for a data frame).
#' @param expression What the caller wrote for a data frame, for the
#'   session script.
#' @param sep,dec,encoding How a `.csv` or `.txt` file is read (see
#'   `sieve_read_data()`).
#' @return A list with `df` (as read, variable labels attached), `name`,
#'   `code` (the R that reads the data, for the session script), and
#'   `description` (the description rows a `.csv` carried, or `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_frame_of <- function(data, name = NULL, expression = "data", sep = NULL, dec = ".", encoding = "UTF-8") {
  if (is.data.frame(data)) {
    name <- name %||% expression
    return(list(
      df = data, name = name, description = NULL,
      # The script's step says what the study came from: a data frame here,
      # a file below (the Ingest tab's load reads a file).
      note = sprintf("Use data frame: %s", name),
      code = c(
        "# The data frame given to sieve_prepare_study(); bind yours to this name before running the script.",
        sprintf("d <- %s", expression)
      )
    ))
  }
  if (!is.character(data) || length(data) != 1L || is.na(data)) {
    stop("`data` must be a data frame or the path of a .csv, .txt, .sav or .dta file.", call. = FALSE)
  }
  if (!file.exists(data)) {
    stop(sprintf("The data file %s was not found.", data), call. = FALSE)
  }
  name <- name %||% basename(data)
  read <- sieve_read_data(data, basename(data), sep = sep, dec = dec, encoding = encoding)
  if (!read$ok) {
    stop(sprintf("%s could not be read: %s", basename(data), read$error), call. = FALSE)
  }
  for (w in read$warnings) {
    warning(sprintf("While reading %s: %s", basename(data), w), call. = FALSE)
  }
  list(
    df = read$value, name = name, code = read$code, description = read$description,
    note = sprintf("Read data file: %s", name)
  )
}

#' The specification behind a data frame or a path
#'
#' @param specification A data frame (every cell is read as text), or the
#'   path of a specification file.
#' @param encoding,utf8_first,argument How a specification file is read
#'   (see `sieve_spec_read()`).
#' @return A list with `table` (character columns, lower case names) and
#'   `name`.
#' @author Ken Kelley
#' @noRd
sieve_spec_of <- function(specification, encoding = "UTF-8", utf8_first = FALSE, argument = NULL) {
  if (is.data.frame(specification)) {
    table <- as.data.frame(specification, stringsAsFactors = FALSE)
    table[] <- lapply(table, function(v) {
      v <- as.character(v)
      v[is.na(v)] <- ""
      v
    })
    names(table) <- tolower(trimws(names(table)))
    return(list(table = table, name = "the specification"))
  }
  if (!is.character(specification) || length(specification) != 1L || is.na(specification)) {
    stop("`specification` must be a data frame or the path of a specification file (.csv).", call. = FALSE)
  }
  list(
    table = sieve_spec_read(specification, encoding = encoding, utf8_first = utf8_first, argument = argument),
    name = basename(specification)
  )
}

#' Write a Specification Template for a Data Frame
#'
#' The first half of the pre-SIEVER step. The template is the specification
#' table of schema 2, one row per column of the data, with what SIEVER can
#' propose filled in and what only the researcher knows left blank: the
#' roles as the Ingest tab proposes them (`sieve_propose_roles()` from the
#' name and the values of each column), the measurement type detected for
#' each item, the Item Name and wording from the variable labels when the
#' file carries them, and the constructs and facets from the column names
#' or the labels when they follow the `Construct__Facet__item` or
#' `Construct: Facet: wording` form, or from the description rows a `.csv`
#' carries under its header (rows whose first cell is `construct`, `facet`,
#' `reverse` or `question`; the question row gives each item's wording).
#' The researcher fills in the rest in a spreadsheet, saves
#' it as a `.csv` beside the data, and gives both to
#' [sieve_prepare_study()], or chooses both together on the Ingest tab.
#'
#' The columns, in order:
#'
#' * `siever_specification`: the schema version, `2`. Leave it.
#' * `data_values`: `raw`, saying that the data file holds every value as
#'   administered (the reversals are made on preparation). A processed file
#'   SIEVER exported says `processed` (`analytic` is read the same way).
#' * `variable`: the column name in the data, exactly. One row per column.
#' * `role`: what the column is for. `item` (it belongs to the item pool),
#'   `identifier` (it names the rows), `group` (it defines the groups of a
#'   multiple group analysis), `criterion` (an external variable the scores
#'   are related to), or `auxiliary` (continuous variables entered as saturated
#'   correlates in FIML models). A column that is not stored as a number cannot be an item.
#' * `kind`: for a criterion, the validity evidence it is for:
#'   `convergent`, `discriminant` or `criterion` (criterion related). Blank
#'   for every other role, and for a criterion whose kind is not stated.
#' * `label`: the Item Name, the short name that follows the item through
#'   every table. Blank means the variable name.
#' * `wording`: the item's full text, shown in the detail panes. Optional.
#' * `type`: the measurement level of an item: `continuous`, `ordered`
#'   (ordered categories such as a rating scale), or `binary` (two
#'   values). Continuous items are analyzed with maximum likelihood on
#'   Pearson correlations; any ordered or binary item makes the estimator
#'   WLSMV on polychoric (or tetrachoric) correlations. The template holds
#'   the detected level; correct it where the detection is wrong (a five
#'   point rating is ordered even when every point was used).
#' * `construct`: the construct the item measures. Items with the same
#'   construct form one factor. Blank means Unassigned. A construct cell
#'   that is blank, missing, or `Unassigned` means the item has no
#'   construct; such items are left out of every construct model,
#'   reliability analysis, score and norm, and are listed in `not_modeled`
#'   with the reason "no construct is assigned". A construct cannot have
#'   the name of a variable in the data.
#' * `facet`: the facet of the construct, when the construct has facets.
#'   A facet needs a construct.
#' * `status`: `included` (the item takes part in every analysis) or
#'   `excluded` (it is kept in the data and left out of the analyses). An
#'   item is never deleted, so an exclusion can be undone.
#' * `reverse`: `TRUE` for an item keyed in the direction opposite to its
#'   construct, which is reverse scored on preparation (reflected within
#'   its response bounds, `x` to `lo + hi - x`); `FALSE` otherwise.
#' * `reverse_applied`: whether the data file already holds the item
#'   reverse scored. `FALSE` for data as administered, which is what a raw
#'   file holds. A reverse keyed item whose cell says `TRUE` is recorded as
#'   reverse scored and no value is changed. For an item a description row
#'   of the file marks as reverse keyed, the cell is the file's own
#'   `reverse_applied` row when it has one; otherwise it is `FALSE` unless
#'   the item already correlates positively with the rest of its construct
#'   (see `sieve_reverse_evidence()`), when it is left blank, so the import
#'   asks rather than reverse scoring the values a second time.
#' * `rev_lo`, `rev_hi`: the response bounds of the item, the lowest and the
#'   highest point of its scale (1 and 7 for a seven point item). Needed
#'   for a reverse keyed item; without them the observed minimum and
#'   maximum are used, and the ledger says so. Give both or neither.
#' * `rev_bounds`: `declared` when the bounds are the scale's, `observed`
#'   when they were read from the data. Blank means declared.
#'
#' Rows for the non item columns need only `variable` and `role`. Every
#' cell is read as text, so nothing is rounded on the way in, and the
#' vocabulary is checked on import: a cell outside it is reported with the
#' variable it belongs to, and nothing is applied until every problem is
#' fixed.
#'
#' @param data A data frame, or the path of a `.csv`, `.txt`, `.sav` or
#'   `.dta` file. SPSS and Stata files come through haven (a package in
#'   Suggests), which keeps each variable's label.
#' @param file Where to write the template as a comma separated file, or
#'   `NULL` to return it without writing. The file is written in UTF-8
#'   whatever `encoding` the data file is read in, and
#'   [sieve_prepare_study()] reads a specification whose bytes are valid
#'   UTF-8 as UTF-8 (its `specification_encoding`).
#' @param sep The separator of a `.csv` or `.txt` data file: `","`, `";"`,
#'   `"\t"` (tab) or `"|"`. `NULL` (the default) is a comma for a `.csv`
#'   file and a tab for a `.txt` file. A file whose header shows another
#'   separator is refused with the one it uses (a file that a spreadsheet
#'   saved with semicolons, as Excel does where the decimal mark is a comma,
#'   needs `sep = ";"` and usually `dec = ","`).
#' @param dec The decimal mark of a `.csv` or `.txt` data file, `"."` (the
#'   default) or `","`.
#' @param encoding The encoding of a `.csv` or `.txt` file: `"UTF-8"` (the
#'   default; what Excel's "CSV UTF-8" and every SIEVER export write),
#'   `"windows-1252"` (what Excel on Windows saves as "CSV (Comma
#'   delimited)"), or `"latin1"`. The file is read in this encoding in every
#'   locale and a UTF-8 byte order mark is removed. A file that is not valid
#'   in the stated encoding is refused with the line and the byte. A file
#'   stated as `"windows-1252"` or `"latin1"` whose bytes are valid UTF-8
#'   and hold a character outside ASCII is UTF-8 (text in a single byte
#'   encoding almost never is), and it is refused with the first such
#'   character and `encoding = "UTF-8"` named, rather than read with each
#'   such character turned into two to four others. A file
#'   that holds characters outside ASCII is read only in an R session whose
#'   character locale is UTF-8, the one locale that represents every
#'   character (a C or POSIX locale represents none outside ASCII, a single
#'   byte locale such as Latin-1 only some); in another session it is
#'   refused with that rule rather than read with its column names changed
#'   or its rows cut short. SPSS and Stata files carry their own encoding,
#'   and `sep`, `dec` and `encoding` do not apply to them.
#' @return The template as a data frame of character columns, invisibly
#'   when `file` is given.
#' @author Ken Kelley
#' @seealso [sieve_prepare_study()], which turns the data and the filled in
#'   template into a project file.
#' @examples
#' d <- DMAR::holzinger_swineford[, c(
#'   "id", "school", "t1_visual_perception",
#'   "t2_cubes", "t4_lozenges"
#' )]
#' template <- sieve_specification_template(d)
#' template[, c("variable", "role", "type", "construct", "facet", "status")]
#' # Fill in the constructs, then write the file beside the data.
#' template$construct[template$role == "item"] <- "Visual"
#' path <- tempfile("holzinger_specification_", fileext = ".csv")
#' sieve_specification_template(d, file = path)
#' file.exists(path)
#' @md
#' @export
sieve_specification_template <- function(data, file = NULL, sep = NULL, dec = ".", encoding = "UTF-8") {
  sieve_text_choices(sep, dec, encoding)
  source <- sieve_frame_of(data, sep = sep, dec = dec, encoding = encoding)
  var_labels <- sieve_var_labels(source$df)
  df <- as.data.frame(source$df)
  roles <- sieve_propose_roles(df)
  item_cols <- roles$variable[roles$role == "item"]
  found <- sieve_item_labels(item_cols, var_labels)
  found$structure <- sieve_describe_structure(found$structure, source$description, taken = names(df))
  n <- nrow(roles)
  blank <- rep("", n)
  at <- match(roles$variable, item_cols)
  is_item <- !is.na(at)
  pick <- function(values) {
    out <- blank
    out[is_item] <- values[at[is_item]]
    out[is.na(out)] <- ""
    out
  }
  labels <- unname(found$labels)
  given <- !is.na(var_labels[item_cols])
  # The question row of a described file is the wording of the items it
  # names; a variable label, when there is one, stays the Item Name.
  asked <- rep(NA_character_, length(item_cols))
  if (!is.null(source$description) && "wording" %in% names(source$description)) {
    at_q <- match(item_cols, source$description$variable)
    asked <- as.character(source$description$wording)[at_q]
  }
  wording_out <- ifelse(!is.na(asked), asked, ifelse(given, labels, ""))
  type <- vapply(df[item_cols], sieve_detect_type, character(1), USE.NAMES = FALSE)
  reversed <- vapply(found$keyed, function(k) isTRUE(k$reversed), logical(1), USE.NAMES = FALSE)
  lo <- vapply(found$keyed, function(k) as.numeric(k$lo), numeric(1), USE.NAMES = FALSE)
  hi <- vapply(found$keyed, function(k) as.numeric(k$hi), numeric(1), USE.NAMES = FALSE)
  bounds <- vapply(found$keyed, function(k) as.character(k$bounds), character(1), USE.NAMES = FALSE)
  # The reverse row of a described file marks an item as reverse keyed in
  # data as administered: reverse TRUE, reverse_applied FALSE.
  # Whether the file already holds such an item reverse scored: its
  # reverse_applied row, or, without one, the item's correlation with the
  # rest of its construct (left blank, so the import asks, when it is
  # already positive).
  keyed_by_file <- rep(FALSE, length(item_cols))
  applied_cell <- ifelse(reversed, "TRUE", "FALSE")
  if (!is.null(source$description)) {
    at_desc <- match(item_cols, source$description$variable)
    keyed_by_file <- !is.na(at_desc) & source$description$reverse[at_desc] %in% TRUE & !reversed
    said <- if ("reverse_applied" %in% names(source$description)) {
      source$description$reverse_applied[at_desc]
    } else {
      rep(NA, length(item_cols))
    }
    applied_cell[keyed_by_file & said %in% TRUE] <- "TRUE"
    open <- item_cols[keyed_by_file & is.na(said)]
    if (length(open)) {
      cons <- found$structure$construct
      ledger <- data.frame(
        item = item_cols, construct = ifelse(is.na(cons), "Unassigned", cons),
        status = "active", stringsAsFactors = FALSE
      )
      evidence <- sieve_reverse_evidence(df[item_cols], ledger, open, item_cols[keyed_by_file])
      positive <- names(evidence$r)[evidence$r > 0]
      applied_cell[item_cols %in% positive] <- ""
    }
  }
  flag <- function(x) ifelse(x, "TRUE", "FALSE")
  table <- data.frame(
    siever_specification = rep(as.character(sieve_spec_schema()), n),
    data_values = rep("raw", n),
    variable = roles$variable,
    role = roles$role,
    kind = blank,
    label = pick(ifelse(given, labels, "")),
    wording = pick(wording_out),
    type = pick(type),
    construct = pick(found$structure$construct),
    facet = pick(found$structure$facet),
    status = pick(rep("included", length(item_cols))),
    reverse = pick(flag(reversed | keyed_by_file)),
    reverse_applied = pick(applied_cell),
    rev_lo = pick(sieve_num_csv(lo)),
    rev_hi = pick(sieve_num_csv(hi)),
    rev_bounds = pick(ifelse(reversed & !is.na(bounds), bounds, "")),
    stringsAsFactors = FALSE
  )
  if (is.null(file)) {
    return(table)
  }
  sieve_spec_write(table, file)
  invisible(table)
}

#' Stop the pre-SIEVER step with every problem, and nothing written
#'
#' @param problems The problems, one per element.
#' @param what The file or table they belong to.
#' @return Does not return.
#' @author Ken Kelley
#' @noRd
sieve_prepare_stop <- function(problems, what) {
  problems <- as.character(problems)
  stop(structure(
    list(
      message = paste(c(
        sprintf(
          "No project file was written. %s has %d problem%s:", what, length(problems),
          if (length(problems) == 1L) "" else "s"
        ),
        paste("*", problems)
      ), collapse = "\n"),
      call = NULL,
      problems = problems
    ),
    class = c("sieve_import_error", "error", "condition")
  ))
}

#' Prepare a Study: Data and a Specification Become a Project File
#'
#' The pre-SIEVER step, one call per study. The data are loaded exactly as
#' the Ingest tab loads a chosen file (a role proposed for every column,
#' the item ledger built from the item columns, the variable labels read as
#' Item Names); the specification is applied through the import's own
#' checks (`sieve_spec_import()` on that fresh session), which set the
#' roles, the Item Names and wordings, the measurement types, the
#' constructs and facets, the included or excluded status, and the keying
#' of every item it lists, reverse scoring each reverse keyed item within
#' its response bounds; the item types are confirmed when the specification
#' types every item, so the project opens ready to analyze; and the project
#' is written with `sieve_project_write()`. On the Ingest tab, open the
#' file with Open project, or drop it into the data file input.
#'
#' Every problem in the specification is reported at once (a variable that
#' is not in the data, a cell outside the vocabulary, a facet without a
#' construct, a reverse keyed item whose keying cannot be settled), as a
#' `sieve_import_error` whose `problems` element lists them, and no file is
#' written until there is none. The data are never changed on disk: the
#' project holds the values as loaded and, separately, the processed values
#' after the reversals, with a record of each. A `.csv` that carries its
#' constructs and facets as rows under the header (the first cell of a row
#' says `construct`, `facet`, `reverse` or `question`, the last giving each
#' item's wording) is read with them; the specification then adds or
#' overrides what it says.
#'
#' `keying` is what to take the values of a reverse keyed item to be when
#' the specification does not settle it: `"verify"` (the default) stops and
#' asks when a specification that carries content keys matches neither
#' data file it was saved with, and takes the `reverse_applied` cell at its
#' word in a specification written by hand (the template); `"administered"`
#' says the data hold every item as administered, so every reverse keyed
#' item is reverse scored; `"reversed"` says they already hold the reverse
#' keyed items reverse scored, so nothing is reflected.
#'
#' `continuous_from` is the rule the Ingest tab applies with the checkbox in
#' its step 1 (on by default): an item the specification types ordered
#' that has five or more response options (its distinct values, or the
#' points its declared bounds `rev_lo` to `rev_hi` span, whichever is more)
#' is typed continuous: with five or more categories, estimation that
#' treats items as continuous (Pearson covariances, ML or MLR with full
#' information maximum likelihood) performs comparably to the categorical
#' estimator in many conditions, especially with roughly symmetric
#' thresholds, while with fewer categories or strongly asymmetric
#' thresholds the categorical estimator is more accurate (Rhemtulla,
#' Brosseau-Liard & Savalei, 2012). Binary items and items with four or
#' fewer options keep the specification's type; `continuous_from = NULL`
#' keeps every type the specification gives and records them as the
#' project's own choices (`type_source = "chosen"`), so the rule keeps them
#' when the project is opened on the Ingest tab.
#'
#' @param data A data frame, or the path of a `.csv`, `.txt`, `.sav` or
#'   `.dta` file (SPSS and Stata files come through haven, a package in
#'   Suggests).
#' @param specification A data frame, or the path of a specification file:
#'   the template from [sieve_specification_template()] filled in, or a
#'   file the Ingest tab's Download specification wrote.
#' @param file The project file to write (`.siever.rds`).
#' @param name The name the data are known by in the project; the file name
#'   by default.
#' @param keying `"verify"`, `"administered"`, or `"reversed"` (see above).
#' @param continuous_from The fewest response options at which an item the
#'   specification types ordered is typed continuous (see above), or `NULL`
#'   to keep the specification's types.
#' @param sep The separator of a `.csv` or `.txt` data file: `","`, `";"`,
#'   `"\t"` (tab) or `"|"`. `NULL` (the default) is a comma for a `.csv`
#'   file and a tab for a `.txt` file. A file whose header shows another
#'   separator is refused with the one it uses (a file that a spreadsheet
#'   saved with semicolons, as Excel does where the decimal mark is a comma,
#'   needs `sep = ";"` and usually `dec = ","`).
#' @param dec The decimal mark of a `.csv` or `.txt` data file, `"."` (the
#'   default) or `","`.
#' @param encoding The encoding of a `.csv` or `.txt` data file:
#'   `"UTF-8"` (the default; what Excel's "CSV UTF-8" and every SIEVER
#'   export write), `"windows-1252"` (what Excel on Windows saves as "CSV
#'   (Comma delimited)"), or `"latin1"`. The file is read in this encoding
#'   in every locale and a UTF-8 byte order mark is removed. A file that is
#'   not valid in the stated encoding is refused with the line and the byte.
#'   A file stated as `"windows-1252"` or `"latin1"` whose bytes are valid
#'   UTF-8 and hold a character outside ASCII is UTF-8 (text in a single
#'   byte encoding almost never is), and it is refused with the first such
#'   character and `encoding = "UTF-8"` named, rather than read with each
#'   such character turned into two to four others.
#'   A file that holds characters outside ASCII is read only in an R session
#'   whose character locale is UTF-8, the one locale that represents every
#'   character (a C or POSIX locale represents none outside ASCII, a single
#'   byte locale such as Latin-1 only some), so that it, and the session
#'   script that replays its read, give the same data frame on every
#'   machine; in another session it is refused with that rule rather than
#'   read with its column names changed or its rows cut short. SPSS and
#'   Stata files carry their own encoding, and `sep`, `dec` and `encoding`
#'   do not apply to them.
#' @param specification_encoding The encoding of a specification file.
#'   `NULL` (the default) reads it as UTF-8 when its bytes are valid UTF-8,
#'   and otherwise in `encoding`, the data file's. SIEVER writes every
#'   specification in UTF-8 whatever the data file's encoding
#'   ([sieve_specification_template()], the Ingest tab's Download
#'   specification), and so does a spreadsheet that saves it as CSV UTF-8,
#'   so a template filled in beside a data file in windows-1252 is read as
#'   written; a specification that Excel on Windows saved as "CSV (Comma
#'   delimited)" is in windows-1252, as its data file usually is. Text in
#'   windows-1252 or Latin-1 that holds characters outside ASCII is almost
#'   never also valid UTF-8, so the rule tells the two apart. `"UTF-8"`,
#'   `"windows-1252"` or `"latin1"` reads the specification in that
#'   encoding (a UTF-8 data file with a specification that Excel on Windows
#'   saved needs `specification_encoding = "windows-1252"`); by the same
#'   rule, a specification stated as `"windows-1252"` or `"latin1"` whose
#'   bytes are valid UTF-8 with a character outside ASCII is refused with
#'   `specification_encoding = "UTF-8"` named. A
#'   specification file is read with the separator its header shows beside
#'   `siever_specification`.
#' @return Invisibly, the project snapshot (class `sieve_project`), which
#'   prints as the summary the function prints: the items by type, the
#'   constructs and facets with their item counts, the roles, the reverse
#'   scored items, and the estimator the types resolve to.
#' @author Ken Kelley
#' @seealso [sieve_specification_template()] for the specification's
#'   columns and vocabulary.
#' @examples
#' d <- DMAR::holzinger_swineford[, c(
#'   "id", "school", "t1_visual_perception",
#'   "t2_cubes", "t4_lozenges"
#' )]
#' spec <- sieve_specification_template(d)
#' spec$construct[spec$role == "item"] <- "Visual"
#' spec$label[spec$variable == "t2_cubes"] <- "Cubes"
#' path <- tempfile("holzinger_", fileext = ".siever.rds")
#' study <- sieve_prepare_study(d, spec, file = path, name = "Holzinger and Swineford")
#' study$meta[, c("item", "label", "construct", "type", "status")]
#' study$confirmed
#' study$estimator
#' # A specification with a problem writes nothing and names it.
#' bad <- spec
#' bad$type[bad$variable == "t2_cubes"] <- "likert"
#' other <- tempfile("holzinger_", fileext = ".siever.rds")
#' try(sieve_prepare_study(d, bad, file = other))
#' file.exists(other)
#' @md
#' @export
sieve_prepare_study <- function(data, specification, file, name = NULL,
                                keying = c("verify", "administered", "reversed"),
                                continuous_from = 5L, sep = NULL, dec = ".", encoding = "UTF-8",
                                specification_encoding = NULL) {
  keying <- match.arg(keying)
  sieve_text_choices(sep, dec, encoding)
  if (!is.null(specification_encoding) &&
    (!is.character(specification_encoding) || length(specification_encoding) != 1L ||
      is.na(specification_encoding) || !specification_encoding %in% sieve_text_encodings())) {
    stop(sprintf(
      "`specification_encoding` must be NULL (UTF-8 when the file is valid UTF-8, otherwise `encoding`) or one of %s.",
      paste(sprintf("\"%s\"", sieve_text_encodings()), collapse = ", ")
    ), call. = FALSE)
  }
  if (!is.null(continuous_from) &&
    (!is.numeric(continuous_from) || length(continuous_from) != 1L || !is.finite(continuous_from) ||
      continuous_from < 2)) {
    stop("`continuous_from` must be one number of response options (five by default), or NULL.", call. = FALSE)
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    stop("`file` must be the path of the project file to write.", call. = FALSE)
  }
  if (!dir.exists(dirname(file))) {
    stop(sprintf(
      "The folder %s does not exist, so the project file cannot be written there.",
      dirname(file)
    ), call. = FALSE)
  }
  source <- sieve_frame_of(data, name,
    expression = deparse1(substitute(data)), sep = sep, dec = dec,
    encoding = encoding
  )
  # SIEVER writes a specification in UTF-8 whatever the data file's
  # encoding, so by default a specification whose bytes are valid UTF-8 is
  # read as UTF-8 and only one that is not is read in the data file's
  # encoding (reading a template filled in beside a windows-1252 data file
  # as windows-1252 would misread every character outside ASCII in its
  # wordings).
  spec <- tryCatch(
    sieve_spec_of(specification,
      encoding = specification_encoding %||% encoding,
      utf8_first = is.null(specification_encoding), argument = "specification_encoding"
    ),
    sieve_import_error = function(e) e
  )
  if (inherits(spec, "sieve_import_error")) {
    sieve_prepare_stop(spec$problems, "The specification")
  }
  state <- sieve_new_state()
  loaded <- sieve_load_frame(state, source$df, source$name, source$code,
    note = source$note, description = source$description
  )
  if (!isTRUE(loaded$ok)) {
    sieve_prepare_stop(loaded$problem, "The data")
  }
  # With the rule off (`continuous_from = NULL`) the specification's types
  # are the project's own explicit choices, recorded as chosen, so the rule
  # keeps them when the project is opened.
  out <- sieve_apply_spec_table(state, spec$table,
    keying = keying, name = spec$name,
    min_options = continuous_from,
    type_source = if (is.null(continuous_from)) "chosen" else "proposed"
  )
  if (!isTRUE(out$ok)) {
    sieve_prepare_stop(out$problems, if (identical(spec$name, "the specification")) "The specification" else spec$name)
  }
  if (isTRUE(out$summary$typed_all)) {
    sieve_confirm_types(state, origin = "specification")
  }
  snapshot <- sieve_project_snapshot(state)
  sieve_project_write(snapshot, file)
  cat(sieve_study_summary(snapshot,
    file = file, untyped = out$summary$untyped,
    switched = out$summary$switched
  ), sep = "\n")
  invisible(snapshot)
}

#' A prepared study in words
#'
#' @param snapshot A project snapshot.
#' @param file The file it was written to, or `NULL`.
#' @param untyped The items the specification did not type, or `NULL`.
#' @param switched The items the specification typed ordered that the
#'   five-option rule typed continuous, or `NULL`.
#' @return A character vector of lines.
#' @author Ken Kelley
#' @noRd
sieve_study_summary <- function(snapshot, file = NULL, untyped = NULL, switched = NULL) {
  meta <- snapshot$meta
  roles <- snapshot$roles
  active <- meta[meta$status == "active", , drop = FALSE]
  lines <- c(
    if (!is.null(file)) sprintf("SIEVER project written: %s", file) else "SIEVER project",
    sprintf(
      "Data: %s, %s rows, %d columns; %s", snapshot$dataset$name %||% "(no name)",
      format(nrow(snapshot$raw), big.mark = ","), ncol(snapshot$raw), sieve_roles_summary(roles)
    ),
    sprintf(
      "Items: %d included, %d excluded; included: %s", nrow(active), nrow(meta) - nrow(active),
      sieve_types_text(meta)
    ),
    if (length(switched)) {
      sprintf(
        paste(
          "Typed ordered in the specification and read as continuous (five or more response",
          "options): %d item%s (%s)"
        ), length(switched), if (length(switched) == 1L) "" else "s",
        sieve_name_some(switched, 8L)
      )
    }
  )
  constructs <- unique(as.character(active$construct))
  constructs <- constructs[sieve_construct_assigned(constructs)]
  if (length(constructs)) {
    lines <- c(lines, sprintf("Constructs: %d", length(constructs)))
    for (cn in constructs) {
      rows <- active[active$construct == cn, , drop = FALSE]
      facets <- as.character(rows$facet)
      has_facet <- !is.na(facets) & nzchar(facets)
      detail <- if (any(has_facet)) {
        counts <- table(factor(facets[has_facet], levels = unique(facets[has_facet])))
        parts <- sprintf("%s %d", names(counts), as.integer(counts))
        if (any(!has_facet)) {
          parts <- c(parts, sprintf("no facet %d", sum(!has_facet)))
        }
        sprintf("; facets: %s", paste(parts, collapse = ", "))
      } else {
        ""
      }
      lines <- c(lines, sprintf("  %s: %d item%s%s", cn, nrow(rows), if (nrow(rows) == 1L) "" else "s", detail))
    }
  } else {
    lines <- c(lines, "Constructs: none assigned (every item is Unassigned)")
  }
  unassigned <- sum(!sieve_construct_assigned(active$construct))
  if (length(constructs) && unassigned) {
    lines <- c(lines, sprintf("  Unassigned: %d item%s", unassigned, if (unassigned == 1L) "" else "s"))
  }
  reversed <- meta$item[meta$reverse %in% TRUE]
  lines <- c(lines, if (length(reversed)) {
    sprintf(
      "Reverse scored: %d item%s (%s)", length(reversed), if (length(reversed) == 1L) "" else "s",
      sieve_name_some(reversed, 8L)
    )
  } else {
    "Reverse scored: none"
  })
  lines <- c(lines, if (isTRUE(snapshot$confirmed)) {
    e <- sieve_estimator_resolve(active$type, choice = sieve_snapshot_choice(snapshot))
    sprintf("Item types: confirmed; estimator %s on a %s matrix", snapshot$estimator, e$basis)
  } else if (length(untyped)) {
    sprintf(paste(
      "Item types: not confirmed (the specification does not type %s); the project opens",
      "awaiting confirmation on the Ingest tab"
    ), sieve_name_some(untyped))
  } else {
    "Item types: not confirmed; the project opens awaiting confirmation on the Ingest tab"
  })
  lines
}

#' @rdname sieve_prepare_study
#' @param x A project snapshot returned by [sieve_prepare_study()].
#' @param ... Not used.
#' @export
print.sieve_project <- function(x, ...) {
  cat(sieve_study_summary(x), sep = "\n")
  invisible(x)
}

#' Resolve What the Load Action Will Do
#' @param source Chosen bundled or file source.
#' @param files Shiny upload metadata or NULL.
#' @param loaded Whether a study is loaded.
#' @param applied_spec Path of a specification already applied.
#' @return A plan code used by preview and execution.
#' @author Ken Kelley
#' @noRd
sieve_load_plan <- function(source = "hs9", files = NULL, loaded = FALSE, applied_spec = NULL) {
  if (source %in% c("hs", "hs9")) return(if (loaded) "replace" else "read")
  if (!NROW(files)) return("none")
  chosen <- sieve_sort_uploads(as.character(files$name), as.character(files$datapath))
  if (!is.null(chosen$problem)) return("problem")
  if (!is.null(chosen$project)) return("project")
  if (is.null(chosen$data)) return(if (!loaded) "spec_alone" else if (identical(chosen$spec$path, applied_spec)) "spec_applied" else "spec")
  if (loaded) "replace" else "read"
}

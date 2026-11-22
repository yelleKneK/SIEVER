# mod_content.R
# Content validity from a panel of subject matter experts: the stage of scale
# development that happens before a single respondent is recruited. A panel
# rates each candidate item for relevance to the construct, and those ratings
# are the evidence that the item pool covers its domain (Lynn, 1986;
# Polit & Beck, 2006; Bandalos, 2018). Every rating statistic on this tab
# comes from DMAR::content_validity_index(), so the app and the package
# report identical numbers: the item level index (I-CVI) with an exact
# binomial confidence interval, the chance corrected modified kappa of
# Polit, Beck, and Owen (2007), Lawshe's (1975) content validity ratio, and
# the scale level summaries S-CVI/Ave and S-CVI/UA. The Item Writing Checks
# sub-tab reads the item wording in the ledger (sieve_wording_checks()),
# not the ratings.
#
# The panel deliberately does not gate on state$confirmed: content evidence is
# pre-data evidence and must be usable before any data set is loaded. Only the
# ledger wiring (writing an I-CVI note onto an item, excluding the items the
# researcher selects) needs a loaded item ledger, and that section carries
# the shared sieve_gate() message. No I-CVI, CVR or S-CVI is graded against
# a cutoff (SIEVER applies no cutoff in output): the values are shown with
# their intervals, and each convention is named once, with its source, as
# context (sieve_content_convention_text()).

#' The illustrative expert ratings used by the demonstration button
#'
#' A constructed 9 item by 6 expert relevance matrix on the conventional
#' 4 point scale. The item names are those of the bundled nine test example
#' so the ledger wiring can be demonstrated end to end. These are illustrative
#' numbers written for teaching, not ratings collected from a real panel.
#'
#' @return A numeric matrix, items in rows and experts in columns, with row
#'   names naming the items and column names naming the experts.
#' @author Ken Kelley
#' @noRd
sieve_content_demo_ratings <- function() {
  m <- rbind(
    c(4, 4, 4, 3, 4, 4),
    c(4, 3, 4, 4, 3, 4),
    c(3, 4, 4, 4, 4, 3),
    c(4, 4, 3, 4, 4, 2),
    c(4, 4, 4, 4, 3, 4),
    c(3, 4, 4, 3, 4, 4),
    c(2, 3, 1, 2, 3, 2),
    c(4, 3, 4, 2, 3, 4),
    c(1, 2, 3, 2, 2, 1)
  )
  fx <- tryCatch(sieve_hs9_fixture(), error = function(e) NULL)
  items <- if (!is.null(fx) && length(fx$item_cols) == nrow(m)) {
    fx$item_cols
  } else {
    paste0("item_", seq_len(nrow(m)))
  }
  dimnames(m) <- list(items, paste0("expert_", seq_len(ncol(m))))
  m
}

#' Whether the illustrative ratings apply to the loaded study
#'
#' The illustrative ratings are written for the bundled nine test example, so
#' that the ledger wiring can be shown end to end. They are offered only when
#' that example is the loaded study, that is, when every rated item is in the
#' item ledger. With a user's data, or before any data are loaded, the tab
#' offers the upload and the template alone: illustrative ratings for another
#' study's items would match nothing in the ledger and would read as evidence
#' that exists.
#'
#' @param meta The item ledger, or `NULL` before data are loaded.
#' @return `TRUE` when the illustrative ratings are for the loaded items.
#' @author Ken Kelley
#' @noRd
sieve_content_demo_applies <- function(meta) {
  if (is.null(meta) || !NROW(meta) || is.null(meta$item)) {
    return(FALSE)
  }
  all(rownames(sieve_content_demo_ratings()) %in% meta$item)
}

#' The example dialog behind "Show an example"
#'
#' A filled expert rating sheet for the bundled Holzinger and Swineford nine
#' tests, shipped as `inst/app/www/content_validity_example.png`, with the layout
#' of the matching .csv file described in words. The image is served from the
#' package's `www` resource path, so no file is read at run time.
#'
#' @return A [shiny::modalDialog()].
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny modalDialog modalButton tags p strong
sieve_content_example_modal <- function() {
  modalDialog(
    title = "An example of a filled expert rating sheet",
    size = "xl",
    easyClose = TRUE,
    tags$img(
      src = "www/content_validity_example.png",
      alt = paste(
        "A rating sheet for the nine Holzinger and Swineford tests: one row per",
        "item with its name and wording, one column per expert, each cell a",
        "relevance rating from 1 to 4."
      ),
      style = "max-width: 100%; height: auto; border: 1px solid #d9d6cc;"
    ),
    p(
      class = "small mt-3",
      strong("What the picture shows. "),
      "One row per candidate item and one column per expert. Six experts rated ",
      "the nine bundled tests for relevance to the ability each test is meant to ",
      "measure, on the 4 point scale (1 = not relevant, 2 = somewhat relevant, ",
      "3 = quite relevant, 4 = highly relevant). The ratings are illustrative, ",
      "constructed for teaching, not collected from a panel."
    ),
    p(
      class = "small",
      strong("The matching file. "),
      "A comma separated file (.csv) with a header row and one row per item. ",
      "The first column, headed item, holds the item names as they appear in ",
      "the data (the variable names, or the Item Names from the Ingest grid); ",
      "that is how each rating finds its item in the ledger. A column headed ",
      "wording is optional: it carries the item text so the experts can read ",
      "what they rate, and it is skipped when the file is read. Every other ",
      "column is one expert, headed as you like (expert_1, expert_2 and so on ",
      "in the template), and holds that expert's rating of each item, 1, 2, 3 ",
      "or 4; a cell left empty means the expert did not rate that item. The ",
      "sheet above, saved as a .csv, begins:"
    ),
    tags$pre(
      class = "small mb-2",
      paste("item,wording,expert_1,expert_2,expert_3,expert_4,expert_5,expert_6",
        "t1_visual_perception,Visual perception,4,4,4,3,4,4",
        "t2_cubes,Cubes (spatial visualization),4,3,4,4,3,4",
        sep = "\n"
      )
    ),
    p(
      class = "small text-muted mb-0",
      "The template download in the sidebar has this layout, with five item rows ",
      "and six expert columns to fill in."
    ),
    footer = modalButton("Close")
  )
}

#' The runnable R that reconstructs a ratings matrix
#'
#' Written so the Reproducible Code tab can rebuild the exact matrix the app
#' analyzed. Used for the demonstration data, where there is no file for the
#' user to point at.
#'
#' @param m A numeric ratings matrix, items in rows and experts in columns.
#' @return A character vector of R lines.
#' @author Ken Kelley
#' @noRd
sieve_content_ratings_code <- function(m) {
  rows <- vapply(seq_len(nrow(m)), function(i) {
    vals <- ifelse(is.na(m[i, ]), "NA", format(m[i, ], trim = TRUE))
    sprintf(
      "  %s = c(%s)%s", rownames(m)[i], paste(vals, collapse = ", "),
      if (i < nrow(m)) "," else ""
    )
  }, character(1L))
  c(
    "ratings <- rbind(",
    rows,
    ")",
    sprintf(
      "colnames(ratings) <- c(%s)",
      paste(sprintf('"%s"', colnames(m)), collapse = ", ")
    )
  )
}

#' Whether a Ratings File's First Column Names the Items
#'
#' With items in rows and experts in columns, the first column is ambiguous
#' whenever it is numeric: item identifiers numbered 1, 2, 3, and so on are
#' indistinguishable from an expert's relevance ratings by storage type alone.
#' Deciding on type would count such an identifier column as one more expert
#' and fold the identifiers into the ratings, which changes every I-CVI, its
#' interval and the expert count without any warning.
#'
#' The decision rests on evidence. A header of item, id, name, or variable
#' settles it, whatever the values look like, and that is the case the
#' downloadable template produces; a header naming an expert (expert, rater,
#' judge, reviewer, panelist, sme, or e, r, j or p followed by a number,
#' such as r1 or e2) settles it the other way. Failing that, values a relevance
#' rating cannot take (anything that is not a whole number from 1 to 4) are
#' item identifiers, and repeated values are an expert's ratings, since
#' identifiers do not repeat. What is left (distinct whole numbers from 1 to
#' 4, which a small pool's identifiers and an expert's ratings can both be)
#' is not guessed: the reading is undecided and the researcher states it with
#' `first_column` (a guess would read a three by three sheet headed
#' expert_1 to expert_3 as item names and two experts whenever its first
#' column happens to be distinct).
#'
#' @param raw The data frame as read from the file.
#' @param first_column `"auto"` for the rule above, `"items"` or `"expert"`
#'   for the researcher's own statement.
#' @return A list with `names` (`TRUE` when the first column is taken to hold
#'   item names, `FALSE` when it is an expert, `NA` when the rule cannot
#'   decide) and `note` (an actionable sentence when a numeric first column
#'   had to be resolved, `NA_character_` otherwise).
#' @author Ken Kelley
#' @noRd
sieve_ratings_item_column <- function(raw, first_column = "auto") {
  first <- raw[[1L]]
  label <- names(raw)[1L]
  header <- tolower(trimws(as.character(label)))
  if (identical(first_column, "items")) {
    return(list(names = TRUE, note = sprintf("The first column ('%s') was read as item identifiers, as chosen.", label)))
  }
  if (identical(first_column, "expert")) {
    return(list(names = FALSE, note = sprintf(
      "The first column ('%s') was read as an expert, as chosen, and the items below are numbered in file order.", label
    )))
  }
  if (!is.numeric(first)) {
    return(list(names = TRUE, note = NA_character_))
  }
  if (length(header) == 1L && !is.na(header) &&
    header %in% c("item", "id", "name", "variable")) {
    return(list(names = TRUE, note = NA_character_))
  }
  if (length(header) == 1L && !is.na(header) &&
    grepl("^(expert|rater|judge|reviewer|panelist|panellist|sme)|^[erjp][ _]?[0-9]+$", header)) {
    return(list(names = FALSE, note = sprintf(
      "The first column ('%s') holds numbers and its header names an expert, so it was read as an expert rather than as item names, and the items below are numbered in file order. If it is really an item identifier, give it the header 'item' and choose the file again.",
      label
    )))
  }
  observed <- first[!is.na(first)]
  rating_values <- length(observed) && all(observed %in% 1:4)
  distinct <- !anyNA(first) && anyDuplicated(first) == 0L
  if (!rating_values && distinct && ncol(raw) >= 3L) {
    return(list(names = TRUE, note = sprintf(
      "The first column ('%s') holds numbers, so it could be item identifiers or one expert's ratings. Its values are all different and are not all ratings on the 1 to 4 relevance scale, so it was read as item identifiers and not as an expert. Give that column the header 'item' to state it outright.",
      label
    )))
  }
  if (!distinct || ncol(raw) < 3L) {
    # Two ways to land here, and the note names the one that applies rather
    # than asserting a reason that may not hold.
    reason <- if (!distinct) {
      "its values repeat, which item identifiers would not"
    } else {
      "reading it as item names would leave fewer than two expert columns"
    }
    return(list(names = FALSE, note = sprintf(
      "The first column ('%s') holds numbers and %s, so it was read as an expert rather than as item names, and the items below are numbered in file order. If that column is really an item identifier, its values are being counted as relevance ratings and every index on this tab is wrong: rename its header to 'item' and choose the file again.",
      label, reason
    )))
  }
  # Distinct whole numbers from 1 to 4: a small pool's identifiers and an
  # expert's ratings look the same, so the reading is the researcher's.
  list(names = NA, note = sprintf(
    "The first column ('%s') holds distinct whole numbers from 1 to 4, which could be the identifiers of a small item pool or one expert's ratings, and nothing in the file decides which. Choose what the first column holds (under the file box) and the file is read again; the header 'item' or a header naming an expert decides it when the file is read.",
    label
  ))
}

#' Read and validate a ratings CSV
#'
#' Items in rows, experts in columns. Whether the first column holds item names
#' or one expert's ratings is decided by `sieve_ratings_item_column()`, on the
#' header and the values rather than on storage type, and a numeric first
#' column is reported in the `note` so the reading is never silent. A column
#' headed `wording` (or `label`) is the item text the experts read while
#' rating; it is skipped, by its header and never by its content, so the sheet
#' the panel filled in is uploaded as it is. Every remaining cell must be
#' missing or a rating of 1, 2, 3, or 4. The failure messages name exactly
#' what the user has to change, because a silent coercion here would quietly
#' redefine the panel's ratings.
#'
#' @param path Path to the uploaded file.
#' @param display_name The file name to show the user.
#' @param first_column What the first column holds: `"auto"` (decided by
#'   `sieve_ratings_item_column()`), `"items"` or `"expert"` (the
#'   researcher's statement, for a first column the rule cannot decide).
#' @return A list with `ok`, `note` (an actionable sentence about a numeric
#'   first column, or `NA_character_`), and either `ratings` (a numeric matrix
#'   with row and column names) together with `has_names` (whether the first
#'   column was consumed as item names), `has_wording` (whether a wording
#'   column was skipped) and `item_source` (`"column"` when the row names came
#'   from that column, `"numbered"` when they were generated), or `message`
#'   (an actionable character string), with `ambiguous = TRUE` when the
#'   first column cannot be decided and `first_column` must be stated.
#' @author Ken Kelley
#' @noRd
sieve_parse_ratings <- function(path, display_name = basename(path), first_column = "auto") {
  bad <- function(msg, note = NA_character_, ambiguous = FALSE) {
    list(
      ok = FALSE, ratings = NULL, message = msg, note = note,
      has_names = NA, has_wording = NA, item_source = NA_character_, ambiguous = ambiguous
    )
  }
  # The bytes are checked as a data file's are (sieve_text_bytes(), UTF-8 in
  # every locale), and a parse that would return part of the file refuses.
  # A ratings file is read only as UTF-8 (the tab has no encoding choice),
  # so a file that is not is told to be saved as CSV UTF-8.
  checked <- sieve_text_bytes(path, "UTF-8", ",", argument = NULL, ingest = FALSE)
  if (!isTRUE(checked$ok)) {
    return(bad(sprintf("%s could not be read: %s.", display_name, checked$error)))
  }
  read <- sieve_text_parse(checked,
    header = TRUE, sep = ",", quote = "\"", fill = TRUE, comment.char = "",
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!isTRUE(read$ok) && !is.null(read$error) && grepl("could not be read completely", read$error, fixed = TRUE)) {
    return(bad(sprintf("%s could not be read: %s.", display_name, read$error)))
  }
  raw <- read$value
  if (is.null(raw) || !is.data.frame(raw) || !nrow(raw) || !ncol(raw)) {
    return(bad(sprintf(
      "%s could not be read as a comma separated file. Save the ratings as .csv with one row per item and one column per expert, then choose it again.",
      display_name
    )))
  }

  decision <- sieve_ratings_item_column(raw, first_column = first_column)
  if (is.na(decision$names)) {
    return(bad(decision$note, ambiguous = TRUE))
  }
  has_names <- decision$names
  note <- decision$note
  item_names <- if (has_names) as.character(raw[[1L]]) else NULL
  # Identifiers decide which ledger item each row rates, so a blank or a
  # repeated one is refused by its rows rather than replaced: replacing one
  # blank would rename every item item_1, item_2, ... with no note.
  if (has_names) {
    blank <- which(is.na(item_names) | !nzchar(trimws(item_names)))
    if (length(blank)) {
      return(bad(sprintf(
        "Row%s %s of %s %s no item identifier in the first column ('%s'). Each row's identifier says which item of the ledger it rates; fill in %s and choose the file again.",
        if (length(blank) == 1L) "" else "s", paste(blank, collapse = ", "), display_name,
        if (length(blank) == 1L) "has" else "have", names(raw)[1L],
        if (length(blank) == 1L) "that identifier" else "those identifiers"
      ), note))
    }
    repeated <- unique(trimws(item_names)[duplicated(trimws(item_names))])
    if (length(repeated)) {
      return(bad(sprintf(
        "The first column ('%s') of %s names an item more than once: %s. Each item is rated in one row; combine the rows or correct the identifiers and choose the file again.",
        names(raw)[1L], display_name, paste(repeated, collapse = ", ")
      ), note))
    }
  }
  vals <- if (has_names) raw[, -1L, drop = FALSE] else raw

  # The optional wording column, recognized by its header alone.
  is_wording <- tolower(trimws(names(vals))) %in% c("wording", "label")
  has_wording <- any(is_wording)
  vals <- vals[, !is_wording, drop = FALSE]

  if (!ncol(vals)) {
    return(bad(sprintf(
      "%s has an item name column but no rating columns. Add one column per expert, each holding that expert's relevance rating for every item.",
      display_name
    ), note))
  }

  # Non numeric rating columns: name them rather than coercing silently.
  not_num <- names(vals)[!vapply(vals, is.numeric, logical(1L))]
  if (length(not_num)) {
    return(bad(sprintf(
      "These rating columns of %s contain values that are not numbers: %s. Every cell after the item name column must be a rating of 1, 2, 3, or 4 (or left empty for an expert who did not rate that item). Remove text, footnotes, and symbols such as \"n/a\" from the rating cells.",
      display_name, paste(not_num, collapse = ", ")
    ), note))
  }

  if (ncol(vals) < 2L) {
    return(bad(sprintf(
      "%s has only %d expert column. A content validity index summarizes agreement across a panel, so at least 2 expert columns are required; five to ten experts is the conventional panel size (Lynn, 1986). Add one column per expert and choose the file again.",
      display_name, ncol(vals)
    ), note))
  }

  m <- as.matrix(vals)
  storage.mode(m) <- "numeric"
  observed <- m[!is.na(m)]
  off_scale <- sort(unique(observed[!(observed %in% 1:4)]))
  if (length(off_scale)) {
    return(bad(
      sprintf(
        "%s contains ratings outside the 4 point relevance scale: %s. Recode every rating to 1 (not relevant), 2 (somewhat relevant), 3 (quite relevant), or 4 (highly relevant), and leave a cell empty when an expert did not rate that item.",
        display_name, paste(format(off_scale, trim = TRUE), collapse = ", ")
      ),
      note
    ))
  }

  empty_rows <- which(rowSums(!is.na(m)) == 0L)
  if (length(empty_rows)) {
    return(bad(sprintf(
      "Rows %s of %s have no ratings at all. Every item must be rated by at least one expert; delete those rows or fill them in.",
      paste(empty_rows, collapse = ", "), display_name
    ), note))
  }

  numbered <- is.null(item_names)
  if (numbered) {
    item_names <- paste0("item_", seq_len(nrow(m)))
  }
  rownames(m) <- trimws(item_names)
  if (is.null(colnames(m)) || !all(nzchar(colnames(m)))) {
    colnames(m) <- paste0("expert_", seq_len(ncol(m)))
  }
  list(
    ok = TRUE, ratings = m, message = NA_character_, note = note,
    has_names = has_names, has_wording = has_wording,
    item_source = if (numbered) "numbered" else "column",
    text = checked[c("ascii", "bom")]
  )
}

#' Match rated item names onto the item ledger
#'
#' Matching is exact on the ledger's `item` column (the syntactic data column
#' name) and then, for names not matched that way, exact on the ledger's
#' `label` column (the researcher's editable Item Name), so a panel that rated
#' the readable item names still lands on the right ledger rows.
#'
#' @param items Character vector of rated item names.
#' @param meta The item ledger.
#' @return A character vector, the same length as `items`, holding the matched
#'   ledger `item` value or `NA` where no match was found.
#' @author Ken Kelley
#' @noRd
sieve_content_match <- function(items, meta) {
  out <- rep(NA_character_, length(items))
  if (is.null(meta) || !nrow(meta)) {
    return(out)
  }
  hit <- match(items, meta$item)
  out[!is.na(hit)] <- meta$item[hit[!is.na(hit)]]
  todo <- is.na(out)
  if (any(todo) && !is.null(meta$label)) {
    hit2 <- match(items[todo], meta$label)
    out[todo][!is.na(hit2)] <- meta$item[hit2[!is.na(hit2)]]
  }
  out
}

#' Content validity module UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput fileInput checkboxGroupInput numericInput
#' @importFrom shiny actionButton downloadButton div tags p strong em helpText hr tagList
#' @importFrom bslib layout_sidebar sidebar card card_header navset_card_tab nav_panel
#' @importFrom bslib accordion accordion_panel
#' @importFrom DT DTOutput
mod_content_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 340,
      title = "Content Validity",
      div(
        class = "text-muted small",
        "Expert relevance ratings for the candidate item pool, analyzed before ",
        "any respondent data are collected. Items in rows, experts in columns, ",
        "on the 4 point relevance scale."
      ),
      fileInput(ns("file"), "Expert ratings file (.csv)", accept = ".csv"),
      # The column-role choice: decided from the header and values, or
      # stated, for a numeric first column the rule cannot decide; a change
      # reads the uploaded file again.
      shiny::radioButtons(ns("first_column"), "The first column of the file holds",
        choices = c(
          "Decide from its header and values" = "auto",
          "Item identifiers" = "items",
          "One expert's ratings" = "expert"
        ),
        selected = "auto"
      ),
      downloadButton(ns("dl_template"), "Download Template (.csv)",
        class = "btn-outline-primary btn-sm w-100 mb-2"
      ),
      # The illustrative ratings are for the bundled nine test example and
      # are offered only when it is the loaded study; with a user's data
      # the upload and the template are the only ways in.
      uiOutput(ns("demo_ui")),
      hr(),
      checkboxGroupInput(
        ns("relevant"), "Rating values counted as relevant",
        choices = c(
          "1 = not relevant" = "1", "2 = somewhat relevant" = "2",
          "3 = quite relevant" = "3", "4 = highly relevant" = "4"
        ),
        selected = c("3", "4")
      ),
      helpText(
        class = "small text-muted",
        "3 and 4 is the standard dichotomization of the 4 point scale into ",
        "relevant versus not relevant (Lynn, 1986; Polit & Beck, 2006)."
      ),
      sieve_conf_level_input(ns("conf_level"), "Confidence level",
        value = 0.95,
        min = 0.50, max = 0.999, step = 0.01
      ),
      helpText(
        class = "small text-muted",
        "The interval on each I-CVI is the exact (Clopper-Pearson) binomial ",
        "interval, computed by DMAR::content_validity_index()."
      )
    ),
    sieve_optional_tab_note("expert ratings of item relevance from a panel of subject matter experts (I-CVI with its interval, modified kappa, CVR, S-CVI), computed from a ratings file without respondent data"),
    # The status line: the ratings read (source, counts) or what is needed.
    uiOutput(ns("parse_note")),
    # Results first: the item level results open the tabs; what the tab asks
    # for and how to read it follow them, the first paragraph visible and the
    # remaining explanation in an open panel.
    navset_card_tab(
      nav_panel(
        "Item Level Results",
        card(
          card_header("I-CVI, Exact Binomial Interval, Modified Kappa, and CVR"),
          uiOutput(ns("cvi_note")),
          DTOutput(ns("cvi_tbl")),
          p(
            class = "small text-muted mt-2",
            "Model: no model is fit here. Every value is a function of the ",
            "expert ratings alone, computed by ",
            "DMAR::content_validity_index(ratings, relevant, conf_level). ",
            sieve_content_convention_text("item"), " Read each I-CVI beside its ",
            "confidence interval, which says how firmly the panel's proportion ",
            "is pinned down."
          )
        )
      ),
      nav_panel(
        "Scale Level Summary",
        card(
          card_header("S-CVI/Ave and S-CVI/UA Across the Item Pool"),
          uiOutput(ns("scvi_note")),
          DTOutput(ns("scvi_tbl")),
          p(
            class = "small text-muted mt-2",
            "S-CVI/Ave is the mean I-CVI over the items in the pool; S-CVI/UA ",
            "is the proportion of items on which the panel was unanimous. ",
            sieve_content_convention_text("scale"), " A pool's mean can be high ",
            "while it contains an item no expert would keep, which is why the ",
            "item level table comes first."
          )
        )
      ),
      nav_panel(
        "Ratings as Read",
        card(
          card_header("The Expert Ratings the App Parsed"),
          p(
            class = "small text-muted",
            "Check this table against the file before reading the results. ",
            "Rows are candidate items, columns are experts, cells are ",
            "relevance ratings on the 4 point scale, and an empty cell means ",
            "that expert did not rate that item (which lowers that item's ",
            "expert count, and only that item's)."
          ),
          DTOutput(ns("ratings_tbl"))
        )
      ),
      nav_panel(
        "Write to the Item Ledger",
        card(
          card_header("Record Content Evidence on the Items, Before Any Model Is Fit"),
          uiOutput(ns("ledger_gate")),
          p(
            class = "small",
            "The item ledger is the spine the rest of the pipeline reads. When ",
            "the rated item names match items in the ledger, the I-CVI can be ",
            "recorded on those items as a note, and the items you judge ",
            "inadequate on content (select their rows in the table below) can ",
            "be excluded before a single model is fit, which is the order in ",
            "which scale development actually proceeds. No item is selected ",
            "for you: SIEVER applies no I-CVI cutoff. ",
            "Excluding an item marks it excluded in the ledger; the row, its ",
            "wording, and its note are kept, so the decision is reversible on ",
            "the Items tab."
          ),
          uiOutput(ns("match_note")),
          div(
            class = "d-flex align-items-center gap-2 mb-2 flex-wrap",
            actionButton(ns("record_note"), "Record I-CVI in the Item Ledger",
              class = "btn-primary"
            ),
            actionButton(ns("park_selected"), "Exclude the Selected Items",
              class = "btn-outline-danger"
            )
          ),
          uiOutput(ns("action_note")),
          tags$details(
            open = NA, tags$summary("Review item matching"),
            p(
              class = "small",
              "Match expert-study identifiers to one variable ID at a time. Labels and wording help identify an item; duplicate wording requires an explicit variable choice. Response values are unchanged."
            ),
            shiny::selectInput(ns("match_rated"), "Rated item identifier", choices = character(0)),
            shiny::selectInput(ns("match_ledger"), "Ledger variable, name, and wording", choices = character(0)),
            actionButton(ns("keep_match"), "Keep reviewed item match"),
            actionButton(ns("keep_all_matches"), "Confirm the proposed matches shown below"),
            downloadButton(ns("dl_matches"), "Download reviewed crosswalk (.csv)")
          ),
          DTOutput(ns("ledger_tbl"))
        )
      ),
      # The checks of the item wording that can be computed from the text
      # alone, a review of content beside the panel's. They read the item
      # ledger, not the ratings.
      nav_panel(
        "Item Writing Checks",
        card(
          card_header("Deterministic Item Writing Checks"),
          p(
            class = "small",
            "The item writing guidelines that can be computed from the text alone, ",
            "after Bandalos (2018): keep items short and readable, ask ", strong("one"),
            " thing at a time, avoid negations (double negatives above all), and avoid ",
            "absolute (\"always\", \"never\") and vague (\"often\", \"sometimes\") ",
            "quantifiers, which respondents interpret inconsistently. Each check is a ",
            "plain string heuristic run on this machine only; nothing leaves it and no ",
            "model is involved. Optional external review is available from Scientific task ",
            "assistant with your own key and payload authorization, and never for these ",
            "checks. The reading grade is an approximate Flesch-Kincaid level from ",
            "a syllable heuristic; items above grade 12 or beyond 20 words are flagged ",
            "as hard to read. A flag is a place to look, not a verdict: read the item ",
            "and decide. The checks read the wording of the included items of the study ",
            "loaded on the Ingest tab (its Question column; see the item-wording vignette ",
            "for the ways to attach it), not the expert ratings."
          ),
          DTOutput(ns("wording_tbl"))
        )
      )
    ),
    # What the tab asks for, in plain words, with an example on request. The
    # card keeps its own height (fill = FALSE): as a fill item of the sidebar
    # layout it would shrink to a strip whenever the panels below it are
    # taller than the window, which is always.
    card(
      fill = FALSE,
      class = "mt-2 mb-2 sieve-content-ask",
      card_header("What this tab asks for: expert ratings of the items"),
      p(
        class = "small mb-2",
        strong("An expert rating"), " is one judge's rating of one item's relevance ",
        "to the construct as the construct has been defined, on the 4 point scale: ",
        "1 = not relevant, 2 = somewhat relevant, 3 = quite relevant, 4 = highly ",
        "relevant. ", strong("Who provides it: "), "a panel of subject matter ",
        "experts, conventionally five to ten (Lynn, 1986), each rating every ",
        "candidate item, before any respondent data are collected. ",
        strong("What the tab computes from it, "), "through ",
        "DMAR::content_validity_index(): the item level content validity index ",
        "(I-CVI, the proportion of the panel that called the item relevant) with ",
        "its exact binomial confidence interval, the modified kappa of Polit, ",
        "Beck, and Owen (2007), Lawshe's (1975) content validity ratio (CVR), and ",
        "the scale level summaries S-CVI/Ave and S-CVI/UA. The ratings arrive as ",
        "a .csv file with one row per item and one column per expert."
      ),
      div(
        class = "d-flex align-items-center gap-2 flex-wrap",
        actionButton(ns("show_example"), "Show an example",
          class = "btn-outline-primary btn-sm"
        ),
        tags$span(
          class = "small text-muted",
          "A filled rating sheet for the bundled Holzinger and Swineford nine ",
          "tests, and the layout of the file."
        )
      )
    ),
    p(
      class = "small",
      strong("Content validity is evidence about the items, not about the data."),
      " Every other tab in this environment asks what a sample of respondents ",
      "reveals about a scale that already exists. This tab asks the prior ",
      "question: does the candidate item pool cover the construct it claims to ",
      "measure? The evidence is the judgment of a panel of subject matter ",
      "experts, who rate each item for relevance to the construct as the ",
      "construct has been defined (Lynn, 1986; Polit & Beck, 2006; Bandalos, ",
      "2018). No factor analysis can repair a pool that omits part of its ",
      "domain or that includes items the domain does not contain, because a ",
      "structure fit to the wrong items still fits. Content evidence is ",
      "collected first, and it is the one form of validity evidence a ",
      "researcher can gather before recruiting a single respondent. The rating ",
      "statistics on this tab (I-CVI with its interval, modified kappa, CVR, S-CVI) ",
      "come from DMAR::content_validity_index(); the Item Writing Checks sub-tab ",
      "reads the item wording with SIEVER::sieve_wording_checks(), not the ratings."
    ),
    tags$details(open = NA,
      class = "mb-2",
      tags$summary("How to read this tab: I-CVI, the interval, modified kappa, and CVR"),
      div(
        class = "small mt-1",
        tags$p(
          tags$b("I-CVI, the item level content validity index."),
          " With ", tags$i("N"), " experts rating an item and ", tags$i("A"),
          " of them calling it relevant, I-CVI = ", tags$i("A"), " / ",
          tags$i("N"), ": the proportion of the panel that judged the item ",
          "relevant. Lynn (1986), as Polit and Beck (2006) summarize her, proposed ",
          "conventions that depend on panel ",
          "size, because a small panel can reach a high proportion by luck: ",
          "1.00 with five or fewer experts, and at least 0.78 with six to ",
          "ten. SIEVER applies no cutoff: the table shows each I-CVI with its ",
          "interval, uncolored, and no item is marked as passing or failing."
        ),
        tags$p(
          tags$b("The interval is not decoration."),
          " An I-CVI is a binomial proportion estimated from a handful of ",
          "judges, so the confidence interval is wide almost by ",
          "construction. With five experts an I-CVI of 0.80 (four of five) ",
          "carries an exact 95% interval running from roughly 0.28 to 0.99, ",
          "which is compatible with an item most of the population of ",
          "experts would reject. This is the accuracy in parameter ",
          "estimation view of content validity that runs through the whole ",
          "package: report the interval, judge the item on where its lower ",
          "limit sits, and when the interval is too wide to act on, the ",
          "remedy is more experts rather than a firmer verdict. The interval ",
          "here is the exact (Clopper-Pearson) binomial interval from ",
          "stats::binom.test(), which holds its coverage at or above the ",
          "nominal level for every panel size. That coverage treats the ",
          "experts as independent judges drawn from a population of qualified ",
          "experts; a panel chosen purposively is not a probability sample of ",
          "experts, so read the interval as the uncertainty of this panel's ",
          "proportion under that idealization."
        ),
        tags$p(
          tags$b("Modified kappa corrects for chance agreement."),
          " Part of any observed agreement on relevance is what a panel ",
          "answering at random with probability 0.5 would produce anyway. ",
          "Polit, Beck, and Owen (2007) subtract it: the chance probability ",
          "is the binomial point probability of exactly ", tags$i("A"),
          " of ", tags$i("N"), " agreeing under random responding, and the ",
          "modified kappa is (I-CVI minus that probability) divided by ",
          "(1 minus that probability). Kappa is always at or below the ",
          "I-CVI, and the gap is largest for small panels, which is the ",
          "same warning the confidence interval gives in a different form."
        ),
        tags$p(
          tags$b("CVR answers a different question."),
          " Lawshe's (1975) content validity ratio asks whether the item ",
          "measures behavior that is ", tags$i("essential"), " to the ",
          "performance domain, not merely relevant to it. With ",
          tags$i("n"), " experts calling the item essential, CVR = (",
          tags$i("n"), " minus ", tags$i("N"), " / 2) divided by (",
          tags$i("N"), " / 2), which is 1 when the whole panel says ",
          "essential, 0 when exactly half do, and -1 when none do. A CVR at ",
          "or below zero means no better than half the panel considers the ",
          "item essential. When the panel answered only the relevance ",
          "question, as in the ratings read on this tab, CVR is computed ",
          "from the same dichotomization as relevance, so it reproduces the ",
          "relevance judgment on Lawshe's scale rather than adding ",
          "independent information; ask the panel a separate essential ",
          "versus useful versus not necessary question when you want the ",
          "two to differ."
        ),
        tags$p(
          class = "text-muted mb-0",
          tags$b("Scale level summaries."),
          " S-CVI/Ave is the mean of the I-CVIs across items, the averaging ",
          "approach Polit and Beck (2006) recommend reporting; S-CVI/UA is ",
          "the universal agreement proportion, the fraction of items every ",
          "expert called relevant, a considerably more conservative summary. ",
          "Polit and Beck (2006) proposed 0.90 for S-CVI/Ave as a convention; ",
          "SIEVER applies no cutoff, and a summary of the pool says nothing ",
          "about which item is the weak one."
        )
      )
    )
  )
}

#' Content validity module server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveVal req renderUI observeEvent
#' @importFrom shiny downloadHandler showNotification showModal isolate div tags HTML
#' @importFrom shiny actionButton helpText tagList
#' @importFrom DT renderDT datatable formatRound formatStyle styleEqual styleInterval
mod_content_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Item writing checks (local; Bandalos, 2018) -----------------------
    # They read the ledger's wording of the included items; the table is
    # drawn when the sub-tab is shown.
    wording_checks <- reactive({
      m <- state$meta
      req(m)
      sieve_wording_checks(m)
    })

    # The checks enter the reproducible code once for each set of worded
    # items, as the call that computes the table: the ledger columns the
    # checks read, written out for the included items that carry wording,
    # and sieve_wording_checks() on them. Showing the same table again
    # (returning to the sub-tab, an edit elsewhere in the ledger) writes
    # nothing; a changed wording, name, construct, or pool writes the new
    # call. The key is kept in a plain environment, not a reactive value, so
    # recording it never invalidates the table.
    wording_logged <- new.env(parent = emptyenv())
    log_wording_checks <- function(meta) {
      active <- meta[meta$status %in% "active", , drop = FALSE]
      worded <- !is.na(active$wording) & nzchar(trimws(active$wording))
      m <- active[worded, c("item", "label", "construct", "wording"), drop = FALSE]
      key <- sieve_hash(list(isolate(state$dataset_id), m))
      if (identical(key, wording_logged$key)) {
        return(invisible(NULL))
      }
      wording_logged$key <- key
      sieve_log(
        state,
        "Wording checks: deterministic item-writing screens (length, reading grade, double-barreled, negations, absolute and vague quantifiers; Bandalos, 2018).",
        c(
          "# The ledger columns the checks read, for the included items with wording.",
          "wording_meta <- data.frame(",
          sieve_df_field("item", sieve_chr_lit(m$item)),
          sieve_df_field("label", sieve_chr_lit(m$label)),
          sieve_df_field("construct", sieve_chr_lit(m$construct)),
          sieve_df_field("wording", sieve_chr_lit(m$wording)),
          "  status = \"active\",",
          "  stringsAsFactors = FALSE",
          ")",
          "wording_checks <- SIEVER::sieve_wording_checks(wording_meta)"
        )
      )
    }

    output$wording_tbl <- renderDT({
      if (is.null(state$meta)) {
        return(sieve_dt(
          data.frame(Note = "Load a study on the Ingest tab to check the wording of its items."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      df <- tryCatch(wording_checks(), error = function(e) NULL)
      if (is.null(df) || !nrow(df)) {
        return(sieve_dt(
          data.frame(Note = "No included item carries wording. Attach wordings (SPSS variable labels, the label attribute, or the item-spec CSV; see the item-wording vignette) to run the checks."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      log_wording_checks(isolate(state$meta))
      yn <- function(x) ifelse(x, "flag", "")
      show <- data.frame(
        Item = df$item, ItemName = df$label, Construct = df$construct,
        Words = df$words, Grade = df$grade,
        DoubleBarreled = yn(df$double_barreled),
        Negation = yn(df$negation),
        DoubleNegative = yn(df$double_negative),
        Absolute = yn(df$absolute),
        Vague = yn(df$vague),
        Flags = df$flags,
        MarkersFound = df$found,
        stringsAsFactors = FALSE
      )
      show <- show[order(-show$Flags), , drop = FALSE]
      # One line per row, so the table shows its rows in the sub-tab's view:
      # the Item Name and the markers found are cut with an ellipsis, the
      # full text on hover.
      clip <- function(x, width) {
        x <- ifelse(is.na(x), "", as.character(x))
        sprintf(
          '<span title="%s" style="display:inline-block;max-width:%dpx;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;vertical-align:bottom">%s</span>',
          htmltools::htmlEscape(x, attribute = TRUE), width, htmltools::htmlEscape(x)
        )
      }
      show$ItemName <- clip(show$ItemName, 260L)
      show$MarkersFound <- clip(show$MarkersFound, 220L)
      sieve_dt(show,
        rownames = FALSE, selection = "none",
        # Every column is escaped except the two clip() built, which escaped
        # their text itself (a construct name is free text, so it is never
        # sent to the page as markup). Positions, since colnames renames.
        escape = setdiff(seq_along(show), match(c("ItemName", "MarkersFound"), names(show))),
        class = "compact nowrap",
        options = list(dom = "tp", pageLength = 25, scrollX = TRUE),
        colnames = c(
          "Variable Name", "Item Name", "Construct", "Words",
          "Reading grade", "Double-barreled", "Negation",
          "Double negative", "Absolute", "Vague", "Flags",
          "Markers found"
        )
      ) |>
        formatRound("Grade", 1) |>
        formatStyle("Flags",
          backgroundColor = styleInterval(c(0, 2), c("#ffffff", "#fdf3d6", "#f8e3e3"))
        )
    })

    # The example behind "Show an example": the image and the file layout.
    observeEvent(input$show_example, {
      showModal(sieve_content_example_modal())
    })

    # The illustrative ratings are offered only for the bundled nine test
    # example (sieve_content_demo_applies()); with a user's data the sidebar
    # holds the upload and the template alone.
    output$demo_ui <- renderUI({
      if (!sieve_content_demo_applies(state$meta)) {
        return(NULL)
      }
      tagList(
        actionButton(ns("demo"), "Load Illustrative Ratings",
          class = "btn-outline-secondary btn-sm w-100 mb-2"
        ),
        helpText(
          class = "small text-muted",
          "The illustrative ratings are written for teaching and are not ratings ",
          "collected from a real panel. Their item names are the nine Holzinger ",
          "and Swineford tests of the bundled example, all of which are in the ",
          "item ledger loaded now, so the ledger wiring below can be shown end ",
          "to end."
        )
      )
    })

    # The parsed ratings: list(ok, ratings, message, source, code, revision).
    # Set by the upload and by the demonstration button; NULL until one of
    # them fires. The ratings are evidence about the items, not about the
    # respondents, so they are not computed from the loaded data and are not
    # discarded when other data are loaded; they carry the revision in force
    # when they were read (sieve_revision), and the tab says when the data
    # now loaded are not those (sieve_stale_reason).
    ratings_in <- reactiveVal(NULL)

    read_ratings <- function(f) {
      req(f, f$datapath)
      choice <- input$first_column %||% "auto"
      if (!choice %in% c("auto", "items", "expert")) choice <- "auto"
      res <- sieve_parse_ratings(f$datapath, display_name = f$name, first_column = choice)
      res$source <- f$name
      res$revision <- sieve_revision(state)
      # The logged R follows the branch the parser actually took: whether the
      # first column was consumed as item names, and whether the row names came
      # from it or were generated.
      res$code <- if (isTRUE(res$ok)) {
        c(
          # The file name as an R literal, so a name holding a quote does
          # not make the recorded call a parse error.
          if (!isTRUE(res$text$ascii)) sieve_text_guard_code(),
          sprintf(
            "ratings <- utils::read.csv(%s, stringsAsFactors = FALSE, check.names = FALSE%s)",
            sieve_r_literal(f$name),
            if (isTRUE(res$text$ascii) && !isTRUE(res$text$bom)) "" else ", fileEncoding = \"UTF-8-BOM\""
          ),
          if (isTRUE(res$has_names)) {
            c(
              if (!identical(choice, "auto")) "# the first column was stated to hold the item identifiers",
              "# items in rows, experts in columns; the first column names the items",
              "item_names <- trimws(as.character(ratings[[1]]))",
              "ratings <- ratings[, -1, drop = FALSE]"
            )
          } else {
            c(
              if (!identical(choice, "auto")) "# the first column was stated to hold one expert's ratings",
              "# items in rows, experts in columns; no item name column was identified,",
              "# so every column was read as an expert"
            )
          },
          if (isTRUE(res$has_wording)) {
            c(
              "# the wording column is the item text the experts read, not a rating",
              'ratings <- ratings[, !(tolower(trimws(names(ratings))) %in% c("wording", "label")), drop = FALSE]'
            )
          },
          "ratings <- as.matrix(ratings)",
          "storage.mode(ratings) <- \"numeric\"",
          if (identical(res$item_source, "column")) {
            "rownames(ratings) <- item_names"
          } else {
            "rownames(ratings) <- paste0(\"item_\", seq_len(nrow(ratings)))"
          }
        )
      } else {
        character(0)
      }
      ratings_in(res)
      if (isTRUE(res$ok)) {
        sieve_log(
          state,
          sprintf(
            "Content validity: read expert relevance ratings from %s (%d items, %d experts).",
            f$name, nrow(res$ratings), ncol(res$ratings)
          ),
          res$code
        )
      }
      invisible(res)
    }
    observeEvent(input$file, read_ratings(input$file))
    # A stated first column reads the uploaded file again.
    observeEvent(input$first_column, {
      f <- input$file
      if (!is.null(f$datapath) && file.exists(f$datapath)) read_ratings(f)
    }, ignoreInit = TRUE)

    observeEvent(input$demo, {
      if (!sieve_content_demo_applies(state$meta)) {
        sieve_notify(
          paste(
            "The illustrative ratings are written for the bundled nine test example",
            "and are not offered for another study. Choose the panel's ratings file, or",
            "download the template to fill in."
          ),
          type = "warning"
        )
        return(invisible(NULL))
      }
      m <- sieve_content_demo_ratings()
      res <- list(
        ok = TRUE, ratings = m, message = NA_character_,
        note = NA_character_,
        source = "illustrative demonstration ratings (not real data)",
        code = sieve_content_ratings_code(m),
        revision = sieve_revision(state)
      )
      ratings_in(res)
      sieve_log(
        state,
        sprintf(
          "Content validity: loaded the illustrative demonstration ratings (%d items, %d experts). These are constructed for teaching, not ratings from a real panel.",
          nrow(m), ncol(m)
        ),
        res$code
      )
    })

    # The rating values counted as relevant, and the confidence level. Both are
    # passed straight through to DMAR, so the guard rails live here.
    relevant <- reactive({
      v <- suppressWarnings(as.numeric(input$relevant))
      v <- v[is.finite(v) & v %in% 1:4]
      sort(unique(v))
    })

    conf_level <- reactive({
      cl <- sieve_finite_or(input$conf_level, 0.95)
      if (cl <= 0 || cl >= 1) 0.95 else cl
    })

    # ---- The content validity index -------------------------------------

    # DMAR::content_validity_index() is the single implementation: the app
    # reports exactly the numbers the package reports. Full precision is kept
    # here; rounding happens only in the renderers below.
    cvi <- reactive({
      r <- ratings_in()
      req(!is.null(r), isTRUE(r$ok))
      rel <- relevant()
      req(length(rel) > 0L)
      cl <- conf_level()
      out <- tryCatch(
        DMAR::content_validity_index(r$ratings,
          relevant = rel,
          essential = NULL, conf_level = cl
        ),
        error = function(e) NULL
      )
      req(!is.null(out))
      sieve_log(
        state,
        sprintf(
          "Content validity: I-CVI with an exact binomial %g%% interval, the modified kappa (Polit, Beck & Owen, 2007), and Lawshe's CVR computed from the relevance ratings (no separate essentiality ratings), for %d items rated by %d experts; relevant = %s.",
          100 * cl, nrow(r$ratings), ncol(r$ratings),
          paste(rel, collapse = " and ")
        ),
        sprintf(
          "cvi <- DMAR::content_validity_index(ratings, relevant = c(%s), essential = NULL, conf_level = %s)",
          paste(rel, collapse = ", "), format(cl)
        )
      )
      out
    })


    # ---- Ratings as read --------------------------------------------------

    # The dataset in force when the ratings were read, and whether the data
    # now loaded are those. sieve_revision() reads in isolation, so the
    # dependency on the dataset identity is taken here.
    ratings_context <- reactive({
      r <- ratings_in()
      req(!is.null(r), isTRUE(r$ok))
      state$dataset_id
      made <- r$revision
      now <- sieve_revision(state)
      loaded_with <- if (is.null(made) || is.na(made$dataset_id)) NA_character_ else made$dataset_id
      now_id <- if (is.na(now$dataset_id)) NA_character_ else now$dataset_id
      changed <- !identical(loaded_with, now_id)
      list(
        loaded_with = loaded_with, now = now_id, changed = changed,
        reason = if (changed && !is.na(loaded_with)) sieve_stale_reason(made, now, what = "dataset") else NULL
      )
    })

    # What the page says about that: the dataset the ratings were read
    # with, and, when the data have changed since, that the ratings are
    # kept as read and the ledger match is made against the data now loaded.
    context_note <- function() {
      cx <- ratings_context()
      with_data <- function(id) if (is.na(id)) "no data loaded" else sprintf("dataset %s", id)
      if (!cx$changed) {
        return(div(
          class = "text-muted small",
          if (is.na(cx$loaded_with)) "Read before any data were loaded." else
            sprintf("Read with %s in force.", with_data(cx$loaded_with))
        ))
      }
      div(
        class = "alert alert-warning py-2 mt-2 mb-0",
        strong(if (is.na(cx$loaded_with)) "Data have been loaded since these ratings were read. " else
          paste0(sub("^a", "A", cx$reason), ". ")),
        sprintf(
          paste0(
            "The ratings were read with %s; %s is loaded now. Expert ratings are ",
            "evidence about the items, not about the respondents, so they are kept as ",
            "read, and the match on the Write to the Item Ledger panel is made against ",
            "the items now loaded. Choose the panel's ratings file for the new item pool if ",
            "these do not belong to it."
          ),
          with_data(cx$loaded_with), with_data(cx$now)
        )
      )
    }

    output$parse_note <- renderUI({
      r <- ratings_in()
      if (is.null(r)) {
        return(div(
          class = "alert alert-info",
          strong("No expert ratings loaded yet. "),
          "Choose a ratings .csv in the sidebar (items in rows, experts in ",
          "columns, ratings of 1 to 4), or download the template to see the ",
          "expected shape; ", strong("Show an example"), " below the results shows a filled ",
          "sheet.",
          if (sieve_content_demo_applies(state$meta)) {
            tagList(
              " Or press ", strong("Load Illustrative Ratings"),
              " to work through the tab with constructed data for the ",
              "bundled example loaded now."
            )
          }
        ))
      }
      # A numeric first column is a genuine fork (item identifiers or one
      # expert's ratings), so how it was read is stated on both the failure and
      # the success path rather than left for the user to infer from the table.
      first_col <- if (is.null(r$note) || is.na(r$note)) {
        NULL
      } else {
        div(
          class = "alert alert-warning py-2 mt-2 mb-0",
          strong("How the first column was read. "), r$note
        )
      }
      if (isTRUE(r$ambiguous)) {
        return(div(
          class = "alert alert-warning",
          strong("What does the first column hold? "), r$message
        ))
      }
      if (!isTRUE(r$ok)) {
        return(tagList(
          div(
            class = "alert alert-danger",
            strong("The ratings file could not be used. "), r$message
          ),
          first_col
        ))
      }
      tagList(
        div(
          class = "text-muted small",
          sprintf(
            "Source: %s. %d items rated by %d experts on the 4 point relevance scale; %d cells are empty.",
            r$source, nrow(r$ratings), ncol(r$ratings),
            sum(is.na(r$ratings))
          )
        ),
        context_note(),
        first_col
      )
    })

    output$ratings_tbl <- renderDT({
      r <- ratings_in()
      req(!is.null(r), isTRUE(r$ok))
      df <- data.frame(
        item = rownames(r$ratings),
        as.data.frame(r$ratings, stringsAsFactors = FALSE),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25, scrollX = TRUE),
        colnames = c("Item", colnames(r$ratings))
      )
    })

    # ---- Item level results -----------------------------------------------

    output$cvi_note <- renderUI({
      r <- ratings_in()
      if (is.null(r) || !isTRUE(r$ok)) {
        return(div(
          class = "alert alert-info",
          "Load expert ratings in the sidebar to compute the content validity index."
        ))
      }

      if (!length(relevant())) {
        return(div(
          class = "alert alert-warning",
          strong("No rating values are marked relevant. "),
          "Check at least one value in the sidebar; 3 and 4 is the standard dichotomization of the 4 point scale."
        ))
      }
      # The method statement of the tab's primary result, in one line.
      div(
        class = "text-muted small mb-1",
        sprintf(
          "Method: DMAR::content_validity_index(), %d items rated by %d experts; ratings of %s count as relevant; exact binomial %g%% intervals. Source: %s.",
          nrow(r$ratings), ncol(r$ratings),
          paste(relevant(), collapse = " and "), 100 * conf_level(),
          r$source
        )
      )
    })

    # The values with their intervals, uncolored: no I-CVI or CVR is graded
    # against a cutoff (SIEVER applies no cutoff in output), so no cell is
    # shaded by Lynn's panel-size value or by a CVR at or below zero.
    output$cvi_tbl <- renderDT({
      d <- tryCatch(cvi(), error = function(e) NULL)
      req(!is.null(d))
      df <- data.frame(
        item = d$item, n_experts = d$n_experts, n_relevant = d$n_relevant,
        i_cvi = d$i_cvi, ci_lower = d$ci_lower, ci_upper = d$ci_upper,
        kappa = d$kappa, cvr = d$cvr,
        stringsAsFactors = FALSE
      )
      sieve_dt(
        df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25, scrollX = TRUE),
        colnames = c(
          "Item", "Experts", "Relevant", "I-CVI", "CI lower",
          "CI upper", "Modified kappa", "CVR (relevance ratings)"
        )
      ) |>
        formatRound(c("i_cvi", "ci_lower", "ci_upper", "kappa", "cvr"), 3)
    })

    # ---- Scale level summary ----------------------------------------------

    scvi <- reactive({
      d <- cvi()
      data.frame(
        statistic = c(
          "S-CVI/Ave (mean I-CVI over items)",
          "S-CVI/UA (proportion of items with universal agreement)",
          "Items in the pool",
          "Experts on the panel"
        ),
        value = c(
          attr(d, "s_cvi_ave"), attr(d, "s_cvi_ua"),
          nrow(d), max(d$n_experts, na.rm = TRUE)
        ),
        stringsAsFactors = FALSE
      )
    })

    output$scvi_note <- renderUI({
      d <- tryCatch(cvi(), error = function(e) NULL)
      if (is.null(d)) {
        return(div(
          class = "alert alert-info",
          "Load expert ratings in the sidebar to compute the scale level summaries."
        ))
      }
      div(
        class = "text-muted small",
        sprintf(
          "S-CVI/Ave = %.3f and S-CVI/UA = %.3f. %s Both summaries are attributes of the DMAR::content_validity_index() table and describe the pool, not any one item.",
          attr(d, "s_cvi_ave"), attr(d, "s_cvi_ua"), sieve_content_convention_text("scale")
        )
      )
    })

    output$scvi_tbl <- renderDT({
      df <- tryCatch(scvi(), error = function(e) NULL)
      req(!is.null(df))
      sieve_log(
        state,
        "Content validity: scale level summaries S-CVI/Ave and S-CVI/UA read from the DMAR table's attributes.",
        c('attr(cvi, "s_cvi_ave")', 'attr(cvi, "s_cvi_ua")')
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t"),
        colnames = c("Statistic", "Value")
      ) |>
        formatRound("value", 3)
    })

    # ---- The template CSV --------------------------------------------------

    output$dl_template <- downloadHandler(
      filename = function() "siever_content_validity_template.csv",
      content = function(file) {
        # The layout the example dialog describes: the item names, the
        # optional wording column (skipped when the file is read), one
        # column per expert.
        tmpl <- data.frame(
          item = paste0("item_", 1:5),
          wording = NA_character_,
          expert_1 = NA_integer_, expert_2 = NA_integer_, expert_3 = NA_integer_,
          expert_4 = NA_integer_, expert_5 = NA_integer_, expert_6 = NA_integer_,
          stringsAsFactors = FALSE
        )
        utils::write.csv(tmpl, file, row.names = FALSE, na = "")
      }
    )

    # ---- The item ledger ---------------------------------------------------

    output$ledger_gate <- renderUI({
      if (is.null(state$meta)) {
        return(sieve_gate(state))
      }
      NULL
    })

    # Rated item names mapped onto ledger rows (exact on the data column name,
    # then on the editable Item Name).
    mapping_key <- reactive({
      r <- ratings_in()
      sieve_hash(list(ratings = r$ratings, sample = state$scientific_dataset_id))
    })
    observeEvent(list(ratings_in(), state$meta),
      {
        r <- ratings_in()
        m <- state$meta
        shiny::updateSelectInput(session,
          "match_rated",
          choices = if (is.null(r$ratings)) character(0) else rownames(r$ratings)
        )
        choices <- if (is.null(m)) character(0) else stats::setNames(m$item, vapply(seq_len(nrow(m)), function(i) {
          paste(c(m$item[i], m$label[i], if ("wording" %in% names(m)) m$wording[i]), collapse = " | ")
        }, character(1)))
        shiny::updateSelectInput(session, "match_ledger", choices = choices)
      },
      ignoreInit = FALSE
    )
    observeEvent(input$keep_match, {
      tryCatch(
        {
          req(input$match_rated, input$match_ledger, state$meta)
          records <- isolate(state$content_mapping) %||% list()
          old <- records[[mapping_key()]]$mapping
          if (is.null(old)) old <- data.frame(rated = character(), ledger = character())
          old <- old[old$rated != input$match_rated, , drop = FALSE]
          map <- rbind(old, data.frame(rated = input$match_rated, ledger = input$match_ledger, stringsAsFactors = FALSE))
          map <- sieve_check_content_mapping(rownames(ratings_in()$ratings), state$meta$item, map)
          records[[mapping_key()]] <- list(
            mapping = map, scientific_dataset_id = state$scientific_dataset_id,
            time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
          )
          state$content_mapping <- records
          sieve_log(
            state,
            "Keep reviewed expert-item crosswalk",
            c(
              "# Reviewed rated-to-ledger mapping; response values are unchanged.",
              utils::capture.output(dput(map))
            )
          )
        },
        error = function(e) sieve_notify(conditionMessage(e), type = "error")
      )
    })
    observeEvent(input$keep_all_matches, {
      tryCatch(
        {
          mm <- matched()
          hit <- !is.na(mm$ledger_item)
          map <- data.frame(rated = mm$item[hit], ledger = mm$ledger_item[hit], stringsAsFactors = FALSE)
          map <- sieve_check_content_mapping(mm$item, state$meta$item, map)
          records <- isolate(state$content_mapping) %||% list()
          records[[mapping_key()]] <- list(
            mapping = map, scientific_dataset_id = state$scientific_dataset_id,
            time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
          )
          state$content_mapping <- records
          sieve_notify(
            sprintf(
              "Confirmed %d one-to-one item matches. Unmatched rated items remain unavailable for ledger actions.",
              nrow(map)
            ),
            type = "message"
          )
        },
        error = function(e) sieve_notify(conditionMessage(e), type = "error")
      )
    })
    output$dl_matches <- downloadHandler(
      filename = function() "siever_expert_item_crosswalk.csv",
      content = function(file) {
        x <- isolate(state$content_mapping)[[mapping_key()]]$mapping
        if (is.null(x)) x <- data.frame(rated = character(), ledger = character())
        utils::write.csv(x, file, row.names = FALSE)
      }
    )
    matched <- reactive({
      d <- cvi()
      req(state$meta)
      auto <- sieve_content_match(d$item, state$meta)
      ambiguous_label <- vapply(
        d$item,
        function(id) !id %in% state$meta$item && sum(!is.na(state$meta$label) & state$meta$label == id) > 1L,
        logical(1)
      )
      auto[ambiguous_label] <- NA_character_
      # Ambiguous auto-matches never write several ratings to one item.
      duplicated_target <- duplicated(auto) | duplicated(auto, fromLast = TRUE)
      auto[!is.na(auto) & duplicated_target] <- NA_character_
      mappings <- state$content_mapping %||% list()
      kept <- mappings[[mapping_key()]]
      confirmed <- rep(FALSE, length(auto))
      if (!is.null(kept)) {
        z <- kept$mapping
        good <- z$rated %in% d$item & z$ledger %in% state$meta$item
        auto[match(z$rated[good], d$item)] <- z$ledger[good]
        confirmed[match(z$rated[good], d$item)] <- TRUE
        dup <- duplicated(auto) | duplicated(auto, fromLast = TRUE)
        confirmed[!is.na(auto) & dup] <- FALSE
        auto[!is.na(auto) & dup] <- NA_character_
      }
      data.frame(
        item = d$item,
        ledger_item = auto, confirmed = confirmed,
        i_cvi = d$i_cvi, n_experts = d$n_experts,
        ci_lower = d$ci_lower, ci_upper = d$ci_upper,
        stringsAsFactors = FALSE
      )
    })

    output$match_note <- renderUI({
      d <- tryCatch(cvi(), error = function(e) NULL)
      if (is.null(d)) {
        return(div(
          class = "alert alert-info",
          "Load expert ratings in the sidebar; the ledger actions become available once the index is computed."
        ))
      }
      if (is.null(state$meta)) {
        return(NULL)
      }
      mm <- matched()
      n_hit <- sum(!is.na(mm$ledger_item))
      cx <- ratings_context()
      # The match is against the ledger now loaded, whatever data were in
      # force when the ratings were read; when those differ, the note
      # above the ratings says so and this one names the dataset matched.
      against <- if (is.na(cx$now)) "the item ledger" else sprintf("the item ledger of dataset %s", cx$now)
      if (!n_hit) {
        return(div(
          class = "alert alert-warning",
          strong(sprintf("None of the rated item names match %s. ", against)),
          "Use Review item matching below to keep a one-to-one crosswalk without changing the expert-study identifiers. Rated names read: ",
          paste(utils::head(mm$item, 5), collapse = ", "),
          if (nrow(mm) > 5) ", ..." else "", "."
        ))
      }
      div(
        class = "text-muted small",
        sprintf(
          "%d of %d rated items match %s; %d matches are confirmed. The remaining matches are proposals. Review the crosswalk and confirm it before ledger actions. To exclude items on content, select their rows in the table below and press Exclude the Selected Items.",
          n_hit, nrow(mm), against, sum(mm$confirmed)
        )
      )
    })

    output$ledger_tbl <- renderDT({
      mm <- tryCatch(matched(), error = function(e) NULL)
      req(!is.null(mm))
      meta <- state$meta
      df <- data.frame(
        item = mm$item,
        ledger = ifelse(is.na(mm$ledger_item), "(no match)", mm$ledger_item),
        match_review = ifelse(mm$confirmed, "confirmed", "proposed: review before ledger actions"),
        # The ledger stores "active" and "parked"; the interface says
        # included and excluded, as the Items tab does.
        status = ifelse(is.na(mm$ledger_item), NA_character_,
          ifelse(meta$status[match(mm$ledger_item, meta$item)] == "active",
            "included", "excluded"
          )
        ),
        note = ifelse(is.na(mm$ledger_item), NA_character_,
          meta$note[match(mm$ledger_item, meta$item)]
        ),
        i_cvi = mm$i_cvi,
        stringsAsFactors = FALSE
      )
      df$ci_lower <- mm$ci_lower
      df$ci_upper <- mm$ci_upper
      # Rows are selected for exclusion by the researcher; nothing is
      # preselected by a cutoff.
      sieve_dt(df,
        rownames = FALSE, selection = "multiple",
        options = list(dom = "tp", pageLength = 25, scrollX = TRUE),
        colnames = c(
          "Rated item", "Ledger item", "Match review", "Status",
          "Ledger note", "I-CVI", "CI lower", "CI upper"
        )
      ) |>
        formatRound(c("i_cvi", "ci_lower", "ci_upper"), 3)
    })

    # Why the last ledger action (Record I-CVI, Exclude the Selected Items)
    # did nothing, written under its buttons until the next ledger action or
    # a new selection in the table: a refusal is written on the page and
    # stays, rather than in a notification that vanishes.
    action_note <- reactiveVal(NULL)
    refuse <- function(text) {
      action_note(text)
      invisible(NULL)
    }
    output$action_note <- renderUI({
      text <- action_note()
      if (is.null(text)) {
        return(NULL)
      }
      div(class = "mb-2", sieve_status_line(text, tone = "warning"))
    })
    observeEvent(input$ledger_tbl_rows_selected, action_note(NULL), ignoreNULL = FALSE, ignoreInit = TRUE)

    # Write the I-CVI onto the matched ledger rows as a note. The note is
    # display text (an annotation a human reads in the grid), so it is
    # formatted here; the full precision index stays in the DMAR table and in
    # the logged call, which recomputes it.
    observeEvent(input$record_note, {
      action_note(NULL)
      if (is.null(state$meta)) {
        return(refuse("Load data on the Ingest tab first: the item ledger does not exist yet."))
      }
      mm <- tryCatch(matched(), error = function(e) NULL)
      if (is.null(mm) || !any(!is.na(mm$ledger_item))) {
        return(refuse("No rated item name matches an item in the ledger, so there is nothing to record."))
      }
      cl <- conf_level()
      hit <- which(!is.na(mm$ledger_item) & mm$confirmed)
      if (!length(hit)) {
        return(refuse("Review and confirm the proposed item matches before writing ledger notes."))
      }
      meta <- state$meta
      for (i in hit) {
        idx <- match(mm$ledger_item[i], meta$item)
        if (is.na(idx)) next
        prior <- meta$note[idx]
        prior <- if (is.na(prior)) "" else prior
        # Replace any note this action wrote earlier rather than stacking them,
        # while preserving annotations written elsewhere (reverse coding).
        prior <- trimws(gsub("I-CVI = [^;]*;?", "", prior))
        prior <- trimws(sub(";$", "", prior))
        new <- sprintf(
          "I-CVI = %.3f (%d experts, %g%% CI [%.3f, %.3f])",
          mm$i_cvi[i], as.integer(mm$n_experts[i]), 100 * cl,
          mm$ci_lower[i], mm$ci_upper[i]
        )
        meta$note[idx] <- if (nzchar(prior)) paste(prior, new, sep = "; ") else new
      }
      sieve_set_note(state, meta$item, meta$note)
      sieve_log(
        state,
        sprintf(
          "Content validity: recorded the I-CVI and its %g%% interval as a note on %d item(s) in the ledger.",
          100 * cl, length(hit)
        ),
        c(
          paste0(
            "content_crosswalk <- ",
            paste(
              utils::capture.output(dput(data.frame(
                rated = mm$item[hit],
                ledger = mm$ledger_item[hit],
                stringsAsFactors = FALSE
              ))),
              collapse = "\n"
            )
          ),
          "idx <- match(content_crosswalk$ledger, meta$item)",
          "rated <- match(content_crosswalk$rated, cvi$item)",
          "keep <- !is.na(idx) & !is.na(rated)",
          "for (.k in which(keep)) {",
          "  .prior <- meta$note[idx[.k]]; if (is.na(.prior)) .prior <- ''",
          "  .prior <- trimws(gsub('I-CVI = [^;]*;?', '', .prior))",
          "  .prior <- trimws(sub(';$', '', .prior))",
          sprintf('  .new <- sprintf("I-CVI = %%.3f (%%d experts, %g%%%% CI [%%.3f, %%.3f])",', 100 * cl),
          "                  cvi$i_cvi[rated[.k]], cvi$n_experts[rated[.k]],",
          "                  cvi$ci_lower[rated[.k]], cvi$ci_upper[rated[.k]])",
          "  meta$note[idx[.k]] <- if (nzchar(.prior)) paste(.prior, .new, sep = '; ') else .new",
          "}"
        )
      )
      sieve_notify(sprintf("Recorded the I-CVI on %d item(s).", length(hit)),
        type = "message", duration = 4
      )
    })

    # Exclude by status, never by deletion: the row, its wording, and its note
    # stay in the ledger and the Items tab can restore it. The items are the
    # ones the researcher selects in the ledger table (no I-CVI cutoff selects
    # them, since SIEVER applies no cutoff). They are listed for confirmation
    # first, and each exclusion goes through the one change every way of
    # dropping an item makes (sieve_set_item_status()), with a reason written
    # from the item's own evidence, into the ledger's exclude_reason, the item
    # actions, and the replayed code.
    park_pending <- reactiveVal(NULL)
    observeEvent(input$park_selected, {
      action_note(NULL)
      if (is.null(state$meta)) {
        return(refuse("Load data on the Ingest tab first: the item ledger does not exist yet."))
      }
      mm <- tryCatch(matched(), error = function(e) NULL)
      if (is.null(mm)) {
        return(invisible(NULL))
      }
      picked <- sieve_content_selected_rows(input$ledger_tbl_rows_selected, nrow(mm))
      if (!length(picked)) {
        return(refuse("Select the rows of the items to exclude in the table below, then press Exclude the Selected Items."))
      }
      hit <- picked[!is.na(mm$ledger_item[picked]) & mm$confirmed[picked]]
      active <- shiny::isolate(state$meta)
      active <- active$item[active$status == "active"]
      hit <- hit[mm$ledger_item[hit] %in% active]
      if (!length(hit)) {
        return(refuse(
          "None of the selected rows is an included item with a confirmed match, so nothing was excluded. Confirm the match first (Review item matching)."
        ))
      }
      cl <- input$conf_level %||% 0.95
      rows <- mm[hit, , drop = FALSE]
      rows$reason <- sieve_content_exclusion_reason(rows, cl)
      park_pending(rows)
      showModal(modalDialog(
        title = "Exclude these items on content grounds?",
        p(
          class = "small",
          "These are the items you selected, with each I-CVI and its interval. ",
          sieve_content_convention_text("item"), " ",
          "An excluded item stays in the data and in the ledger ",
          "with its reason, and it can be included again on the Items tab or with Exclude / Include items."
        ),
        tags$table(
          class = "table table-sm small",
          tags$thead(tags$tr(
            tags$th("Item"), tags$th("Rated as"), tags$th("I-CVI"), tags$th("Experts"),
            tags$th(sprintf("%s%% CI", format(100 * cl)))
          )),
          tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
            tags$tr(
              tags$td(rows$ledger_item[i]), tags$td(rows$item[i]),
              tags$td(sprintf("%.3f", rows$i_cvi[i])), tags$td(as.integer(rows$n_experts[i])),
              tags$td(sprintf("[%.3f, %.3f]", rows$ci_lower[i], rows$ci_upper[i]))
            )
          }))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("park_selected_confirm"),
            sprintf("Exclude %d item%s", nrow(rows), if (nrow(rows) == 1L) "" else "s"),
            class = "btn-danger"
          )
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$park_selected_confirm, {
      rows <- park_pending()
      park_pending(NULL)
      shiny::removeModal()
      if (is.null(rows) || !nrow(rows) || is.null(state$meta)) {
        return(invisible(NULL))
      }
      changed <- character(0)
      for (i in seq_len(nrow(rows))) {
        done <- sieve_coverage_notify(sieve_set_item_status(state, rows$ledger_item[i], status = "parked", reason = rows$reason[i]))
        changed <- c(changed, done$changed)
      }
      sieve_notify(
        sprintf(
          "Excluded %d item(s) on content grounds; the reason is kept in the ledger, and they can be included again on the Items tab.",
          length(changed)
        ),
        type = "message", duration = 5
      )
    })
  })
}

#' The content validity conventions, named with their sources, as context
#'
#' One sentence each, the same wherever the Content Validity page states
#' them: the item level convention of Lynn (1986) for the I-CVI with what a
#' CVR above zero means by the ratio's definition, and the scale level
#' convention of Polit and Beck (2006) for S-CVI/Ave, each followed by the
#' statement that SIEVER applies no cutoff (no value is shaded and no item
#' is selected by one). No retention convention is attributed to Lawshe
#' (1975): what a CVR above zero means follows from the ratio's definition,
#' not from a rule SIEVER credits to him.
#'
#' @param level `"item"` for the I-CVI and CVR, `"scale"` for S-CVI/Ave.
#' @return One string.
#' @author Ken Kelley
#' @noRd
sieve_content_convention_text <- function(level = c("item", "scale")) {
  level <- match.arg(level)
  switch(level,
    item = paste(
      "Lynn (1986), as Polit and Beck (2006) summarize her, proposed an I-CVI of 1.00 with five or fewer",
      "experts and of at least 0.78 with six to ten",
      "as a convention, and a CVR above zero means, by its definition, that more than half the panel",
      "called the item essential; SIEVER applies no cutoff to either."
    ),
    scale = paste(
      "Polit and Beck (2006) proposed 0.90 for S-CVI/Ave as a convention; SIEVER applies no cutoff."
    )
  )
}

#' The reason recorded with a content exclusion
#'
#' Written from the item's own evidence (its I-CVI, the panel size and the
#' interval), with no criterion: the researcher selected the item.
#'
#' @param rows Rows of the matched table, with `i_cvi`, `n_experts`,
#'   `ci_lower` and `ci_upper`.
#' @param conf_level The confidence level of the intervals.
#' @return A character vector, one reason per row.
#' @author Ken Kelley
#' @noRd
sieve_content_exclusion_reason <- function(rows, conf_level) {
  sprintf(
    "Content validity: I-CVI = %s with %d experts (%s%% CI [%s, %s]); excluded after review on the Content Validity tab.",
    sprintf("%.3f", rows$i_cvi), as.integer(rows$n_experts), format(100 * conf_level),
    sprintf("%.3f", rows$ci_lower), sprintf("%.3f", rows$ci_upper)
  )
}

#' The rows the researcher selected in the ledger table
#'
#' @param selected The table's `_rows_selected` input (row positions), or
#'   `NULL`.
#' @param n The number of rows in the table.
#' @return The distinct valid positions, in increasing order (possibly
#'   empty).
#' @author Ken Kelley
#' @noRd
sieve_content_selected_rows <- function(selected, n) {
  x <- suppressWarnings(as.integer(selected))
  sort(unique(x[!is.na(x) & x >= 1L & x <= n]))
}

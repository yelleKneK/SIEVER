# mod_ingest.R
# Module 2: ingest and data type detection. Read a dataset, classify each
# item's measurement level, present an editable grid, and pick the matching
# correlation matrix and estimator. Confirmation is required before the
# downstream modules unlock (project brief, section 7.1).
#
# The sidebar is the way in, in four numbered steps: choose the files (a
# SIEVER-ready file, a data file with its specification, a data file alone,
# or a bundled example), load, review the roles and the types, confirm.
# Each step's control is enabled when the step before it is done, and a
# step that is not yet available says why. The action bar in the main panel
# holds the status chip, which names the step the researcher is at, and the
# Confirm button, which has three states: before the confirmation, confirmed
# (with the study in words), and changed since the confirmation (with what
# changed), so that a change that matters is confirmed again.
#
# Everything in the grid is edited in place: the measurement Type (auto
# detected, then adjustable from the cell), the Construct and the Facet
# (theory, not data), the Item Name (free text, so a variable can be given
# a meaningful name that follows it downstream), the included status and
# the keying. Columns are drag resizable (see www/sieve.js). A reverse
# toggle updates its own row through a message to the table (the data and
# the ledger change in the one transformation service); the grid is
# rebuilt only when rows move (an exclusion, a role change, a load).
#
# Which columns are items is not decided by how they are stored. A role is
# proposed for every column of an uploaded file (item, identifier, group,
# criterion, auxiliary), shown for review, and editable in the Variable
# roles grid or, by name, in the roles editor; the item grid lists the
# items only. The exports (data, specification, summary, project) are on
# the Reproducible Code tab; the Open project action and the restoring of a
# specification are here. Their logic is in fct_export.R, fct_prepare.R
# and fct_project.R.

#' The JavaScript that updates rows of the item grid in place
#'
#' The server sends, in one message, the rows that one edit or one action
#' for several items changed: for each, the value of the key column that
#' identifies it and its cells by column index (as the table holds them).
#' The table that holds the rows is found among DataTables' own
#' (`$.fn.dataTable.tables()`), not as the first table in the box: the grid
#' scrolls horizontally, so DataTables puts a copy of the header in a table
#' of its own before it, which holds no rows (a handler that took that copy
#' would reach no row). Each cell is
#' rewritten in place through the DataTables API (its column renderer
#' included), with no redraw of the table: a redraw detaches every row of
#' the page for a moment, and a browser then commits a value being typed in
#' another row (a change event fires on the detach). So the table keeps its
#' paging, its ordering, its scroll position and the keyboard focus.
#'
#' A field removed with its cell also fires a change event when it holds
#' typed text; the handler stops the change events the rewrite causes, so
#' nothing half typed is sent. When the focus was in a rewritten cell, it
#' moves to the cell's new control, and when that control is the same field
#' and the text in it had not been committed, the text and the caret are
#' put back, so that leaving the field commits it as before; when the new
#' cell has no control (the bounds of a reversal, as text), the focus goes
#' to the cell itself. The controls of a rewritten cell get what sieve.js
#' gives a drawn table (no Tab stop of their own, their titles as the
#' explanation shown on focus or hover). The handler is registered once per
#' page; it does nothing when the table is not drawn yet (the next draw
#' shows the current ledger anyway).
#'
#' @return A length one character string of JavaScript.
#' @author Ken Kelley
#' @noRd
sieve_ingest_row_js <- function() {
  paste(
    "(function () {",
    "  if (!window.Shiny || window.sieveIngestRowHandler) return;",
    "  window.sieveIngestRowHandler = true;",
    "  // The value each grid field last committed (a change event), so that",
    "  // text typed since can be told from text the server already has.",
    "  document.addEventListener('change', function (e) {",
    "    var t = e.target;",
    "    if (t && t.classList && t.classList.contains('sieve-cell')) t.setAttribute('data-sieve-committed', t.value);",
    "  }, true);",
    "  Shiny.addCustomMessageHandler('sieve_ingest_row', function (m) {",
    "    if (!window.jQuery || !jQuery.fn.dataTable) return;",
    "    var box = document.getElementById(m.id);",
    "    if (!box) return;",
    "    var nodes = jQuery.fn.dataTable.tables().filter(function (t) { return box.contains(t); });",
    "    if (!nodes.length) return;",
    "    var api = jQuery(nodes[0]).DataTable();",
    "    var active = document.activeElement;",
    "    var held = active && active !== document.body && box.contains(active);",
    "    var td0 = held ? jQuery(active).closest('td')[0] : null;",
    "    var at = td0 ? api.cell(td0).index() : null;",
    "    var typed = null;",
    "    if (held && active.tagName === 'INPUT' && active.classList.contains('sieve-cell')) {",
    "      var committed = active.hasAttribute('data-sieve-committed') ? active.getAttribute('data-sieve-committed') : active.defaultValue;",
    "      if (active.value !== committed) {",
    "        typed = {item: active.getAttribute('data-item'), field: active.getAttribute('data-field'), value: active.value,",
    "          start: active.selectionStart, end: active.selectionEnd};",
    "      }",
    "    }",
    "    var mute = function (e) { e.stopImmediatePropagation(); };",
    "    box.addEventListener('change', mute, true);",
    "    try {",
    "      (m.rows || []).forEach(function (row) {",
    "        var idx = api.rows(function (i, data) { return String(data[m.key_col]) === String(row.key); }).indexes();",
    "        if (!idx.length) return;",
    "        Object.keys(row.cells).forEach(function (c) {",
    "          var cell = api.cell(idx[0], parseInt(c, 10));",
    "          cell.data(row.cells[c]);",
    "          var td = cell.node();",
    "          if (!td) return;",
    "          jQuery(td).find('input, select, textarea, button').attr('tabindex', '-1');",
    "          jQuery(td).find('[title]').each(function () {",
    "            var text = this.getAttribute('title');",
    "            this.setAttribute('data-sieve-explanation', text);",
    "            if (!this.getAttribute('aria-label')) this.setAttribute('aria-label', text);",
    "            this.removeAttribute('title');",
    "          });",
    "        });",
    "      });",
    "    } finally {",
    "      box.removeEventListener('change', mute, true);",
    "    }",
    "    if (held && !document.body.contains(active) && at) {",
    "      var node = api.cell(at.row, at.column).node();",
    "      if (!node) return;",
    "      var same = typed ? jQuery(node).find('input.sieve-cell').filter(function () {",
    "        return this.getAttribute('data-item') === typed.item && this.getAttribute('data-field') === typed.field;",
    "      })[0] : null;",
    "      if (same) {",
    "        same.focus({preventScroll: true});",
    "        same.value = typed.value;",
    "        try { same.setSelectionRange(typed.start, typed.end); } catch (e) {}",
    "        return;",
    "      }",
    "      var target = jQuery(node).find('input, select, textarea, button').first()[0] || node;",
    "      target.focus({preventScroll: true});",
    "    }",
    "  });",
    "})();",
    sep = "\n"
  )
}

#' Send a custom message to the browser when a session can carry one
#'
#' A mock session (shiny::testServer) carries no message to a browser; the
#' call is then a no-op, and a test counts the calls through this one
#' function.
#'
#' @param session The session (or a module's session proxy).
#' @param type The message type.
#' @param message The message.
#' @return `NULL`, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_session_message <- function(session, type, message) {
  send <- session$sendCustomMessage
  if (is.function(send)) {
    send(type, message)
  }
  invisible(NULL)
}

#' The columns of the item grid, in order
#'
#' @return A character vector of the data frame's column names, from which
#'   the zero based column indexes the table uses are taken.
#' @author Ken Kelley
#' @noRd
sieve_grid_columns <- function() {
  c(
    "grp", "Variable", "ItemName", "Question", "Use", "Type", "Levels", "Missing", "Mean", "SD",
    "Min", "Max", "Distribution", "Reverse", "Bounds", "Construct", "Facet"
  )
}

#' The Reverse? cell of one item: the select
#'
#' The bounds a reversal reflects within, and the range warning, are in the
#' item's Response bounds cell beside it (`sieve_bounds_cell()`).
#'
#' @param input_id The namespaced edit input.
#' @param m The item ledger.
#' @param i The row.
#' @param check Unused; kept so that callers written for the cell that also
#'   held the bounds still work.
#' @return A length one HTML string.
#' @author Ken Kelley
#' @noRd
sieve_reverse_cell <- function(input_id, m, i, check = NULL) {
  sieve_cell_select(input_id, m$item[i], "reverse", if (isTRUE(m$reverse[i])) "Yes" else "No", c("No", "Yes"))
}

#' The Response bounds cell of one item
#'
#' The lowest and the highest response of the item's scale, edited in place
#' like every other cell of the grid. For an item that is not reverse scored
#' it is a text field: the declared bounds ("1 to 7"), or blank when none
#' are declared, with the file's value labels or the observed range as the
#' hint; typing bounds declares them (`sieve_declare_bounds()`), and
#' clearing the field takes the declaration back (`sieve_clear_bounds()`).
#' An item that holds the bounds of its reversal (`held`: a reflection made
#' in this session, or a reversal as loaded whose keying record states
#' them; `sieve_holds_reversal_bounds()`) shows them as plain text, since
#' returning it to its scoring (Reverse? No) reflects within the same bounds
#' and so returns exactly the original values; they stay stored with the
#' item afterwards, and a later reversal uses them unless bounds are
#' declared for it. An item reverse scored as loaded whose record gave no
#' bounds keeps the field, holding any bounds typed for it since, with a
#' line that says Reverse? No reflects within them (or is refused without
#' them). The bounds the file's value labels give are
#' named in every form of the cell, and an item whose observed range
#' differs from the study's common range while no bounds are declared
#' carries that warning in every form.
#'
#' @param input_id The namespaced edit input.
#' @param m The item ledger (with the keying columns).
#' @param i The row.
#' @param labels `c(lo, hi)`, the bounds the file's value labels give for
#'   the item (`sieve_label_bounds()`), or `c(NA, NA)`.
#' @param check A `sieve_range_check()` result for the ledger, or `NULL`.
#' @param held Whether the item holds the bounds of a reversal in force
#'   (`sieve_holds_reversal_bounds()`), or `NULL` to take any reverse scored
#'   item with stored bounds to hold them.
#' @return A length one HTML string.
#' @author Ken Kelley
#' @noRd
sieve_bounds_cell <- function(input_id, m, i, labels = c(NA_real_, NA_real_), check = NULL, held = NULL) {
  lo <- suppressWarnings(as.numeric(m$rev_lo[i]))
  hi <- suppressWarnings(as.numeric(m$rev_hi[i]))
  kind <- as.character(m$rev_bounds[i])
  stored <- is.finite(lo) && is.finite(hi)
  labels <- suppressWarnings(as.numeric(labels))
  has_labels <- length(labels) == 2L && all(is.finite(labels))
  span <- function(a, b) sprintf("%s to %s", sieve_num_text(a), sieve_num_text(b))
  reversed <- isTRUE(m$reverse[i])
  held <- if (is.null(held)) reversed && stored else isTRUE(held) && reversed && stored
  label_line <- if (has_labels) {
    sprintf('<div class="small text-muted">the file\'s value labels give %s</div>', span(labels[1], labels[2]))
  } else {
    ""
  }
  # The range warning, in every form of the cell (the line above the grid
  # points to it).
  warning <- ""
  if (!is.null(check) && nrow(check$differing) && m$item[i] %in% check$differing$item) {
    at <- match(m$item[i], check$differing$item)
    warning <- sprintf(
      '<div class="small text-warning-emphasis">observed %s; the study\'s common range is %s and no bounds are declared%s</div>',
      span(check$differing$min[at], check$differing$max[at]), span(check$common[1], check$common[2]),
      if (held) ": set Reverse? to No, declare its bounds, and reverse it again" else ""
    )
  }
  if (held) {
    return(paste0(
      sprintf('<div class="small">%s</div>', span(lo, hi)),
      sprintf(
        '<div class="small text-muted">the bounds of its reversal%s; set Reverse? to No to change them</div>',
        if (identical(kind, "observed")) ", the observed range (none were declared)" else ""
      ),
      label_line,
      warning
    ))
  }
  declared <- identical(kind, "declared") && stored
  obs_lo <- suppressWarnings(as.numeric(m$min[i]))
  obs_hi <- suppressWarnings(as.numeric(m$max[i]))
  hint <- if (has_labels) {
    sprintf("labels: %s", span(labels[1], labels[2]))
  } else if (is.finite(obs_lo) && is.finite(obs_hi)) {
    sprintf("observed %s", span(obs_lo, obs_hi))
  }
  # A reversal as loaded shows the bounds recorded for it whatever their
  # kind, since Reverse? No reflects within them.
  shown <- declared || (reversed && stored)
  cell <- sieve_cell_text(input_id, m$item[i], "bounds", if (shown) span(lo, hi) else "", placeholder = hint)
  same_as_labels <- declared && has_labels && isTRUE(all.equal(c(lo, hi), labels))
  if (reversed) {
    # Reverse scored as loaded with no bounds in its keying record (recorded
    # by a specification, the description rows, the answer that the file
    # holds it reverse scored already, or a variable label that says so):
    # bounds typed since are the researcher's and can be corrected here;
    # Reverse? No reflects within them, and is refused while none are
    # recorded (sieve_reverse_item()).
    cell <- paste0(cell, if (stored) {
      '<div class="small text-muted">reverse scored as loaded: Reverse? No reflects within these bounds; correct them here if they are not the scale\'s</div>'
    } else {
      '<div class="small text-muted">reverse scored as loaded with no bounds recorded: type the scale\'s bounds before setting Reverse? to No</div>'
    })
  }
  if (same_as_labels) {
    # Where the declaration came from (the hint is hidden once the field
    # holds a value): the bounds the file's value labels give, declared at
    # load.
    cell <- paste0(cell, '<div class="small text-muted">from the file\'s value labels</div>')
  } else {
    cell <- paste0(cell, label_line)
  }
  paste0(cell, warning)
}

#' Read bounds typed in a cell
#'
#' Accepts the two numbers with "to", a hyphen, an en dash, a comma, a
#' semicolon or a space between them ("1 to 7", "1-7", "-3 to 3", "0, 4").
#' A space is read as the separator before a hyphen is, so "-3 -1" is -3
#' to -1 (not -3 to 1); "1 -7" is then 1 to -7, which is refused with the
#' reading named. Pasted typography is read as typed: a no break space as a
#' space and the minus sign (U+2212) as a hyphen.
#'
#' @param text The typed text.
#' @return A list: `empty` (the field was cleared), and either `lo`, `hi`,
#'   or `problem` (a sentence).
#' @author Ken Kelley
#' @noRd
sieve_parse_bounds <- function(text) {
  text <- trimws(paste(as.character(text %||% ""), collapse = " "))
  text <- trimws(gsub("\u2212", "-", gsub("[\u00a0\u2007\u202f]", " ", text)))
  if (!nzchar(text)) {
    return(list(empty = TRUE))
  }
  number <- "(-?[0-9]*\\.?[0-9]+)"
  pattern <- paste0("^", number, "(?:\\s+|\\s*(?:to|TO|To|,|;|-|\u2013)\\s*)", number, "$")
  hit <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1]]
  if (length(hit) != 3L) {
    return(list(empty = FALSE, problem = sprintf(
      "\"%s\" is not two numbers; write the lowest and the highest response, as 1 to 7.", text
    )))
  }
  lo <- as.numeric(hit[2])
  hi <- as.numeric(hit[3])
  if (!is.finite(lo) || !is.finite(hi) || lo >= hi) {
    return(list(empty = FALSE, problem = sprintf(
      "\"%s\" reads as %s to %s. The lowest response must be below the highest; nothing was declared.",
      text, sieve_num_text(lo), sieve_num_text(hi)
    )))
  }
  list(empty = FALSE, lo = lo, hi = hi)
}

#' The line under an item's Type cell: where the type came from and what
#' it rests on
#'
#' `proposed` (detected, or read from a specification or description rows)
#' or `your choice` (`type_source`, see `sieve_type_source()`), then the
#' item's response options, the distinct values observed when fewer, and
#' its skew; the count of each category is the line's tooltip. The
#' conditional finding of Rhemtulla, Brosseau-Liard and Savalei (2012) is
#' judged against these.
#'
#' @param m The item ledger.
#' @param i The row.
#' @param items The item data.
#' @return A length one HTML string (empty for an item not in the data).
#' @author Ken Kelley
#' @noRd
sieve_type_cell_note <- function(m, i, items) {
  it <- m$item[i]
  if (!it %in% names(items)) {
    return("")
  }
  declared <- "rev_bounds" %in% names(m) && identical(as.character(m$rev_bounds[i]), "declared")
  ev <- sieve_type_evidence(
    items[[it]],
    if (declared) m$rev_lo[i] else NA_real_, if (declared) m$rev_hi[i] else NA_real_
  )
  counts <- if (length(ev$counts)) {
    sprintf(' title="Category counts: %s"', paste(sprintf("%s: %d", names(ev$counts), ev$counts), collapse = ", "))
  } else {
    ""
  }
  sprintf(
    '<div class="small text-muted"%s>%s; %s</div>', counts,
    if (identical(sieve_type_source(m)[i], "chosen")) "your choice" else "proposed", ev$text
  )
}

#' Why the items SPSS calls ordinal are typed continuous, in words
#'
#' One sentence per reason `sieve_spss_level_conflicts()` gives, so the
#' note says "five or more response options" only of the items the rule
#' typed.
#'
#' @param clash A `sieve_spss_level_conflicts()` result.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_spss_reason_text <- function(clash) {
  part <- function(why, text) {
    its <- clash$item[clash$reason == why]
    if (length(its)) sprintf(text, sieve_name_some(its))
  }
  paste(c(
    part("options", paste0(
      "SIEVER proposes %s as continuous because each has five or more response options: with five or more ",
      "categories, estimation that treats items as continuous performs comparably to the categorical ",
      "estimator in many conditions, especially with roughly symmetric thresholds, while with strongly ",
      "asymmetric thresholds the categorical estimator is more accurate (Rhemtulla, Brosseau-Liard & ",
      "Savalei, 2012; the Type cells show each item's skew)."
    )),
    part("chosen", "%s were typed continuous by you."),
    part("values", "%s hold values that are not whole numbers, so they are continuous."),
    part("typed", "%s are typed continuous here.")
  ), collapse = " ")
}

#' The Range column of the ledger's question table
#'
#' @param m The item ledger.
#' @return A character vector, one per row: the observed range, the
#'   declared bounds when there are any, and the range warning for an item
#'   `sieve_range_check()` names.
#' @author Ken Kelley
#' @noRd
sieve_range_column <- function(m) {
  check <- sieve_range_check(m)
  vapply(seq_len(nrow(m)), function(i) {
    lo <- suppressWarnings(as.numeric(m$min[i]))
    hi <- suppressWarnings(as.numeric(m$max[i]))
    if (!is.finite(lo) || !is.finite(hi)) {
      return("")
    }
    out <- sprintf("%s to %s (observed)", sieve_num_text(lo), sieve_num_text(hi))
    d_lo <- suppressWarnings(as.numeric(m$rev_lo[i]))
    d_hi <- suppressWarnings(as.numeric(m$rev_hi[i]))
    if (identical(as.character(m$rev_bounds[i]), "declared") && is.finite(d_lo) && is.finite(d_hi)) {
      out <- sprintf("%s; declared %s to %s", out, sieve_num_text(d_lo), sieve_num_text(d_hi))
    }
    if (!is.null(check) && m$item[i] %in% check$differing$item) {
      out <- sprintf(
        "%s; differs from the common range %s to %s, no bounds declared", out,
        sieve_num_text(check$common[1]), sieve_num_text(check$common[2])
      )
    }
    out
  }, character(1))
}

#' The range warning in one line, for the line above the item grid
#'
#' The count of the items `sieve_range_check()` names and where they are
#' marked (their Response bounds cells); the full statement
#' (`sieve_range_text()`) and where to declare bounds open from the line.
#'
#' @param check A `sieve_range_check()` result with items to name.
#' @return One sentence.
#' @author Ken Kelley
#' @noRd
sieve_range_summary <- function(check) {
  d <- check$differing
  k <- nrow(d)
  outside <- if ("outside" %in% names(d)) d$outside %in% TRUE else rep(length(check$declared) > 0L, k)
  k_out <- sum(outside)
  k_rest <- k - k_out
  n_declared <- length(check$declared)
  see <- if (k == 1L) "see its Response bounds cell" else "see their Response bounds cells"
  parts <- character(0)
  if (k_out) {
    parts <- c(parts, sprintf(
      "%d included item%s %s outside the declared bounds",
      k_out, if (k_out == 1L) "" else "s", if (k_out == 1L) "lies" else "lie"
    ))
  }
  if (k_rest) {
    pre <- if (!n_declared) {
      "none declared; "
    } else if (!k_out) {
      sprintf("bounds declared for %d of the %d items; ", n_declared, check$n)
    } else {
      ""
    }
    subject <- if (!n_declared) {
      sprintf("%d of the %d included items", k_rest, check$n)
    } else {
      sprintf("%d %sincluded item%s without declared bounds", k_rest, if (k_out) "other " else "", if (k_rest == 1L) "" else "s")
    }
    parts <- c(parts, if (isTRUE(check$majority)) {
      sprintf(
        "%s%s %s an observed range other than %s to %s",
        pre, subject, if (k_rest == 1L) "has" else "have",
        sieve_num_text(check$common[1]), sieve_num_text(check$common[2])
      )
    } else if (!n_declared) {
      sprintf("none declared, and the observed ranges differ across the items (%d outside the most common range", k_rest)
    } else {
      sprintf("%sthe observed ranges differ across the items (%d outside the most common range", pre, k_rest)
    })
  }
  lead <- paste(parts, collapse = "; ")
  # The no-majority wording carries its parenthesis open, so the pointer to
  # the cells joins it.
  lead <- if (grepl("most common range$", lead)) sprintf("%s; %s)", lead, see) else sprintf("%s (%s)", lead, see)
  paste0(lead, ". Open to read why and where to declare bounds.")
}


#' The constructs under analysis, in words, when some are set aside
#'
#' @param meta The item ledger.
#' @return A sentence such as `"Analyzing 1 of 6 constructs: Trust (45 of
#'   303 items)."`, or `NULL` when every construct has an included item.
#' @author Ken Kelley
#' @noRd
sieve_analyzing_text <- function(meta) {
  tbl <- sieve_construct_table(meta)
  if (nrow(tbl) < 2L || all(tbl$n_active > 0L)) {
    return(NULL)
  }
  on <- tbl[tbl$n_active > 0L, , drop = FALSE]
  sprintf(
    "Analyzing %d of %d constructs: %s (%d of %d items).", nrow(on), nrow(tbl),
    paste(on$construct, collapse = ", "), sum(on$n_active), nrow(meta)
  )
}

#' The column headers of the Variable roles grid, in order
#'
#' The variable first (always shown), then its Item Name (the ledger's
#' editable label for an item, blank for a column that is not an item),
#' the role and what was proposed, and last the question wording, which is
#' long text and is offered under Columns shown rather than shown by
#' default.
#'
#' @return A character vector of headers.
#' @author Ken Kelley
#' @noRd
sieve_roles_grid_headers <- function() {
  c(
    "Variable Name", "Item Name", "Role", "Validity kind", "Proposed", "Why", "Stored as",
    "Distinct values", "Missing", "First values", "Question"
  )
}

#' The Variable roles grid's columns shown by default
#'
#' @return The headers ticked under Columns shown when the tab opens: every
#'   one but the variable (always shown) and the question wording.
#' @author Ken Kelley
#' @noRd
sieve_roles_grid_shown <- function() {
  setdiff(sieve_roles_grid_headers(), c("Variable Name", "Question"))
}

#' The Item Name and the question of each column of the data
#'
#' @param variables The column names of the data.
#' @param meta The item ledger, or `NULL`.
#' @return A data frame with `item_name` (the ledger's label of an item,
#'   `""` for a column that is not an item) and `wording` (the item's
#'   question, `NA` when there is none or the column is not an item), one
#'   row per variable.
#' @author Ken Kelley
#' @noRd
sieve_item_names_of <- function(variables, meta = NULL) {
  variables <- as.character(variables)
  at <- if (is.null(meta)) rep(NA_integer_, length(variables)) else match(variables, as.character(meta$item))
  label <- if (is.null(meta)) rep(NA_character_, length(variables)) else as.character(meta$label)[at]
  label[is.na(at) | is.na(label)] <- ""
  wording <- if (!is.null(meta) && "wording" %in% names(meta)) {
    as.character(meta$wording)[at]
  } else {
    rep(NA_character_, length(variables))
  }
  data.frame(item_name = label, wording = wording, stringsAsFactors = FALSE)
}

#' The choices of the roles editor's picker
#'
#' Each column of the data by its variable name, with an item's Item Name
#' after it when the two differ (`"item_7 (Openness 01)"`), so a column
#' can be found by either name when typing in the picker. The values are
#' the variable names.
#'
#' @param variables The column names of the data.
#' @param meta The item ledger, or `NULL`.
#' @return A named character vector: shown text as names, variables as
#'   values.
#' @author Ken Kelley
#' @noRd
sieve_roles_pick_choices <- function(variables, meta = NULL) {
  variables <- as.character(variables)
  name <- sieve_item_names_of(variables, meta)$item_name
  shown <- ifelse(nzchar(name) & name != variables, sprintf("%s (%s)", variables, name), variables)
  stats::setNames(variables, shown)
}

#' The Use cell of the item grid
#'
#' The included or excluded select and, for an excluded item whose ledger
#' records why it was excluded (`exclude_reason`, written by the Screening
#' tab and the Drop items control), the reason under it, truncated, with
#' the full text on hover.
#'
#' @param input_id The namespaced edit input.
#' @param m The item ledger.
#' @param i The row.
#' @return A length one HTML string.
#' @author Ken Kelley
#' @noRd
sieve_use_cell <- function(input_id, m, i) {
  active <- identical(as.character(m$status[i]), "active")
  cell <- sieve_cell_select(
    input_id, m$item[i], "status", if (active) "Included" else "Excluded",
    c("Included", "Excluded")
  )
  reason <- if ("exclude_reason" %in% names(m)) as.character(m$exclude_reason[i]) else NA_character_
  if (!active && length(reason) == 1L && !is.na(reason) && nzchar(trimws(reason))) {
    reason <- trimws(reason)
    shown <- if (nchar(reason) > 40L) paste0(substr(reason, 1L, 39L), "\u2026") else reason
    cell <- paste0(cell, sprintf(
      '<div class="small text-muted" title="%s">%s</div>',
      htmltools::htmlEscape(reason, attribute = TRUE),
      htmltools::htmlEscape(shown)
    ))
  }
  cell
}

#' What the distribution of one variable shows
#'
#' The Dataset overview's "Distribution of one variable", as data, so the
#' drawing (`sieve_distribution_plot()`) and a test read the same values.
#' An integer valued variable with at most `max_bars` distinct values (a
#' rating scale, a count, a 0/1 item) gets a bar per value, every value
#' labeled on the axis, with the values inside its declared response
#' bounds that no one chose shown as bars of height zero (and the values
#' between the lowest and the highest that no one chose, when the span is
#' short enough to label every point). Any other numeric variable gets a
#' histogram whose breaks are [base::pretty()] values. A variable that is
#' not stored as a number gets a bar per level. The subtitle states n, the
#' number missing, and for a numeric variable the mean and the SD of the
#' non-missing values (the SD with the n - 1 divisor); the mean is drawn
#' as a vertical line. Nothing is rounded here: the values are rounded in
#' the subtitle's text only.
#'
#' @param x The variable's values.
#' @param name The variable's name.
#' @param role Its role, or `NULL`.
#' @param label Its Item Name when it is an item and the name differs, or
#'   `NULL`.
#' @param lo,hi Its declared response bounds, or `NA`.
#' @param max_bars The most distinct values drawn as a bar each.
#' @return A list with `kind` (`"bars"`, `"histogram"`, `"levels"` or
#'   `"empty"`), `at` (the bar positions, the histogram's bin midpoints, or
#'   the levels), `counts`, `breaks` (the axis breaks: every value for
#'   bars, the bin limits for a histogram), `n`, `missing`, `mean`, `sd`,
#'   `title` and `subtitle`.
#' @author Ken Kelley
#' @noRd
sieve_distribution_data <- function(x, name, role = NULL, label = NULL, lo = NA_real_, hi = NA_real_,
                                    max_bars = 20L) {
  title <- paste0(
    name, if (length(role) && !is.na(role) && nzchar(role)) sprintf(" (%s)", role) else "",
    if (length(label) && !is.na(label) && nzchar(label) && !identical(label, name)) {
      sprintf(": %s", label)
    } else {
      ""
    }
  )
  base <- unclass(x)
  missing <- sum(is.na(x))
  numeric <- !is.factor(x) && !is.logical(base) && is.numeric(base)
  if (!numeric) {
    vals <- as.character(x)
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (!length(vals)) {
      return(list(
        kind = "empty", title = title, subtitle = sprintf("No non-missing values (%d missing).", missing),
        n = 0L, missing = missing
      ))
    }
    tb <- table(if (is.factor(x)) droplevels(factor(vals, levels = levels(x))) else vals)
    return(list(
      kind = "levels", at = names(tb), counts = as.integer(tb), breaks = names(tb),
      n = length(vals), missing = missing, mean = NA_real_, sd = NA_real_, title = title,
      subtitle = sprintf(
        "n = %s, %s missing; %d level%s (not stored as a number)",
        format(length(vals), big.mark = ","), format(missing, big.mark = ","),
        length(tb), if (length(tb) == 1L) "" else "s"
      )
    ))
  }
  v <- as.numeric(base)
  missing <- sum(!is.finite(v))
  v <- v[is.finite(v)]
  n <- length(v)
  if (!n) {
    return(list(
      kind = "empty", title = title, subtitle = sprintf("No non-missing values (%d missing).", missing),
      n = 0L, missing = missing
    ))
  }
  m <- mean(v)
  s <- if (n >= 2L) stats::sd(v) else NA_real_
  # Two lines, so the statistics are not cut off when the plot is narrow.
  subtitle <- sprintf(
    "n = %s, %s missing\nmean %s (the vertical line), SD %s",
    format(n, big.mark = ","), format(missing, big.mark = ","), sprintf("%.2f", m),
    if (is.finite(s)) sprintf("%.2f", s) else "not defined (one value)"
  )
  u <- sort(unique(v))
  whole <- function(z) all(abs(z - round(z)) < .Machine$double.eps^0.5)
  if (whole(u) && length(u) <= max_bars) {
    lo <- suppressWarnings(as.numeric(lo))
    hi <- suppressWarnings(as.numeric(hi))
    # Declared bounds widen the bars to every point of the scale.
    bounds <- if (length(lo) == 1L && length(hi) == 1L && is.finite(lo) && is.finite(hi) &&
      whole(c(lo, hi)) && hi > lo) c(lo, hi) else numeric(0)
    from <- min(c(u, bounds))
    to <- max(c(u, bounds))
    at <- as.numeric(if (to - from + 1 <= max_bars + 5L) seq(from, to) else sort(unique(c(u, bounds))))
    counts <- vapply(at, function(a) sum(abs(v - a) < .Machine$double.eps^0.5), integer(1))
    return(list(
      kind = "bars", at = at, counts = counts, breaks = at, n = n, missing = missing,
      mean = m, sd = s, title = title, subtitle = subtitle
    ))
  }
  breaks <- pretty(range(v), n = grDevices::nclass.Sturges(v))
  h <- graphics::hist(v, breaks = breaks, plot = FALSE)
  list(
    kind = "histogram", at = h$mids, counts = as.integer(h$counts), breaks = h$breaks, n = n,
    missing = missing, mean = m, sd = s, title = title, subtitle = subtitle
  )
}

#' Draw the distribution of one variable
#'
#' Draws what `sieve_distribution_data()` holds with ggplot2:
#' bars with their counts above them and every value on the axis, or a
#' histogram on its breaks, the mean as a vertical line, and n, the number
#' missing, the mean and the SD in the subtitle; a variable that is not a
#' number as horizontal bars per level.
#'
#' @param d A `sieve_distribution_data()` result.
#' @param col The fill color (the session palette's first color).
#' @param line_col The color of the mean line.
#' @return A ggplot object.
#' @author Ken Kelley
#' @noRd
sieve_distribution_plot <- function(d, col = "#0c2340", line_col = "#c99700") {
  if (identical(d$kind, "empty")) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = d$title, subtitle = d$subtitle) +
      ggplot2::theme_void(base_size = 13))
  }
  if (identical(d$kind, "levels")) {
    dd <- data.frame(level = factor(d$at, levels = rev(d$at)), count = d$counts)
    return(
      ggplot2::ggplot(dd, ggplot2::aes(x = .data$level, y = .data$count)) +
        ggplot2::geom_col(fill = col) +
        ggplot2::geom_text(ggplot2::aes(label = .data$count), hjust = -0.2, size = 3.6) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Count", title = d$title, subtitle = d$subtitle) +
        ggplot2::theme_minimal(base_size = 13)
    )
  }
  if (identical(d$kind, "bars")) {
    dd <- data.frame(value = d$at, count = d$counts)
    return(
      ggplot2::ggplot(dd, ggplot2::aes(x = .data$value, y = .data$count)) +
        ggplot2::geom_col(fill = col, width = 0.8) +
        # The mean line under the counts, so a count it crosses stays legible.
        ggplot2::geom_vline(xintercept = d$mean, colour = line_col, linewidth = 1) +
        ggplot2::geom_text(ggplot2::aes(label = .data$count), vjust = -0.4, size = 3.6, fontface = "bold") +
        ggplot2::scale_x_continuous(breaks = d$breaks, minor_breaks = NULL) +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::labs(x = NULL, y = "Count", title = d$title, subtitle = d$subtitle) +
        ggplot2::theme_minimal(base_size = 13)
    )
  }
  dd <- data.frame(mid = d$at, count = d$counts)
  width <- diff(d$breaks)[1]
  ggplot2::ggplot(dd, ggplot2::aes(x = .data$mid, y = .data$count)) +
    ggplot2::geom_col(fill = col, colour = "white", width = width) +
    ggplot2::geom_vline(xintercept = d$mean, colour = line_col, linewidth = 1) +
    ggplot2::scale_x_continuous(breaks = d$breaks) +
    ggplot2::labs(x = NULL, y = "Count", title = d$title, subtitle = d$subtitle) +
    ggplot2::theme_minimal(base_size = 13)
}

#' Ingest module UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS radioButtons conditionalPanel fileInput actionButton
#' @importFrom shiny helpText uiOutput verbatimTextOutput textOutput plotOutput hr h6 p strong tags
#' @importFrom shiny selectInput tagList icon textAreaInput checkboxGroupInput checkboxInput selectizeInput HTML numericInput
#' @importFrom bslib layout_sidebar sidebar navset_card_tab nav_panel card card_header
#' @importFrom bslib layout_columns value_box
#' @importFrom DT DTOutput
mod_ingest_ui <- function(id) {
  ns <- NS(id)
  file_sources <- c("ready", "pair", "alone", "file")
  layout_sidebar(
    # The way in lives in the sidebar, so the sidebar is always open: it
    # never collapses to a chevron that would hide the steps.
    sidebar = sidebar(
      width = 340,
      open = "always",
      title = "The way in, in four steps",
      h6(class = "mt-1 mb-1", "Step 1. Choose the files"),
      radioButtons(
        ns("source"), NULL,
        choices = c(
          "SIEVER-ready file: a project (.siever.rds), or a data file that carries its constructs and facets" = "ready",
          "Data file with its specification (two files, chosen together)" = "pair",
          "Data file alone (.csv, .sav, .dta, .txt)" = "alone",
          "Bundled example: Holzinger-Swineford, primary 9 tests (n = 301, 3 factors)" = "hs9",
          "Bundled example: Holzinger-Swineford, full data (n = 301, 24 tests)" = "hs"
        ),
        selected = "hs9"
      ),
      uiOutput(ns("source_help")),
      conditionalPanel(
        sprintf(
          "%s.indexOf(input['%s']) >= 0",
          sprintf("[%s]", paste(sprintf("'%s'", file_sources), collapse = ", ")), ns("source")
        ),
        # Once a file is chosen its bar says what comes next (sieve.js
        # replaces Shiny's "Upload complete"; nothing is uploaded anywhere).
        htmltools::tagAppendAttributes(
          fileInput(ns("file"), NULL,
            multiple = TRUE, buttonLabel = "Choose",
            placeholder = "one file, or a data file with its specification",
            accept = c(".sav", ".dta", ".csv", ".txt", ".rds", ".siever")
          ),
          `data-sieve-done` = "Ready: press Load (step 2)"
        ),
        tags$details(
          tags$summary("Delimited file parsing and preview"),
          # The encoding applies whether or not the box below it is ticked
          # (a file's encoding does not depend on its type, and UTF-8, the
          # default, is what an unticked Load read), so the refusal of a
          # file that is not UTF-8 is followed by choosing the encoding it
          # names, whether the box is ticked or not. The box governs the delimiter and the decimal
          # mark, whose defaults follow the file type.
          selectInput(ns("parse_encoding"), "Encoding", sieve_text_encodings()),
          checkboxInput(ns("parse_override"), "Use explicit CSV/TXT parsing choices", FALSE),
          selectInput(
            ns("parse_sep"),
            "Delimiter",
            c(
              "Comma" = ",",
              "Semicolon" = ";",
              "Tab" = "tab",
              "Vertical bar" = "|"
            )
          ),
          selectInput(ns("parse_dec"), "Decimal mark", c("Period" = ".", "Comma" = ",")),
          helpText(paste(
            "The encoding applies to every CSV/TXT data file, whether or not Use explicit CSV/TXT parsing choices",
            "is ticked: UTF-8 by default (a byte order mark, which Excel's CSV UTF-8 writes, is removed), and",
            "windows-1252 for what Excel on Windows saves as CSV (Comma delimited). A file whose bytes are valid UTF-8",
            "and hold an accented letter or another character outside ASCII is UTF-8, and under windows-1252 or latin1",
            "it is refused with UTF-8 named, rather than read with each such character turned into two to four others",
            "(a choice made here for an earlier file applies to the next). Without explicit choices, a .csv",
            "file is read as comma separated and a .txt file as tab separated, with a period as the decimal mark.",
            "The preview reads only the first ten data rows. Loading uses the same choices and records them in the",
            "script. These choices apply only to CSV/TXT data, not the paired specification, whose separator is read",
            "from its header and which is read as UTF-8 (SIEVER writes it so; one that Excel on Windows saved as CSV",
            "(Comma delimited) is saved again as CSV UTF-8)."
          )),
          uiOutput(ns("parse_preview"))
        ),
        # On by default; off for a file whose second row is data, so that
        # it is never misread as a description.
        checkboxInput(ns("read_rows"),
          "Read construct, facet, reverse and question rows under the header",
          value = TRUE
        ),
        # The five-option rule is on by default, at every way in: loading
        # data, applying a specification and opening a project. An ordered
        # type the researcher chose is kept. Shown with the file sources only
        # (the bundled examples are continuous), so step 2's Load stays in
        # view on a laptop screen.
        checkboxInput(ns("five_continuous"),
          paste(
            "Treat an item with five or more response options as continuous",
            "when loading data, applying a specification or opening a project"
          ),
          value = TRUE
        ),
        helpText(paste(
          "In an opened project only types SIEVER proposed (detected, or read from a specification)",
          "are switched, and the note above the item grid names them with one click to keep them",
          "ordered; a type you chose is kept."
        )),
        actionButton(ns("open_project"), "Open project (shows what the file holds first)",
          class = "btn-outline-secondary btn-sm w-100 mb-2"
        )
      ),
      h6(class = "mt-2 mb-1", "Step 2. Load"),
      actionButton(ns("load"), "Load", class = "btn-primary w-100"),
      uiOutput(ns("step2_why")),
      h6(class = "mt-2 mb-1", "Step 3. Review roles and types"),
      uiOutput(ns("step3_why")),
      radioButtons(
        ns("basis"), "Construct basis",
        choices = c(
          "Theoretical (researcher-specified assignments)" = "theoretical",
          "Empirical (provisional assignments)" = "empirical"
        ),
        selected = "theoretical"
      ),
      actionButton(ns("apply_basis"), "Apply construct basis", class = "btn-outline-primary btn-sm w-100"),
      uiOutput(ns("basis_policy")),
      actionButton(ns("clear_constructs"), "Clear constructs to Unassigned",
        class = "btn-outline-secondary btn-sm w-100", disabled = TRUE
      ),
      uiOutput(ns("construct_picker")),
      tags$details(
        class = "small mt-2",
        tags$summary("Apply a specification to the loaded data"),
        helpText(
          "A specification (.csv) written by Download specification or by ",
          tags$code("sieve_specification_template()"), " is matched by variable name. A reverse ",
          "keyed item is reverse scored only when the loaded values do not hold the reversal yet; ",
          "when they do, the item is recorded as reverse scored and no value is changed. A ",
          "specification with a problem is refused as a whole and changes nothing."
        ),
        radioButtons(
          ns("spec_keying"), "The reverse keyed items in the loaded data are",
          choices = c(
            "checked against the specification (stop when it cannot tell)" = "verify",
            "as administered: reverse score them now" = "administered",
            "already reverse scored: do not reverse them again" = "reversed"
          ),
          selected = "verify"
        ),
        fileInput(ns("up_spec"), "Specification file", accept = ".csv")
      ),
      h6(class = "mt-2 mb-1", "Step 4. Confirm"),
      # The study's estimator: every model tab offers the same control and
      # follows this one unless it is set otherwise there.
      sieve_estimator_input(ns("estimator_choice"), "Estimator for every model in the study",
        selected = "auto", study = FALSE
      ),
      uiOutput(ns("estimator_box")),
      # The study's sampling design: one declaration every page reads through
      # sieve_sampling_spec() (Reliability, Validity, Bifactor, SEM, CV and
      # Invariance, the report), set here beside the estimator.
      selectInput(
        ns("sampling_unit"),
        "Sampling design of the study (every page reads it)",
        c(
          "Independent rows" = "independent",
          "Clustered rows" = "clustered",
          "Repeated observations" = "repeated"
        )
      ),
      selectInput(ns("sampling_cluster"), "Cluster or respondent ID column", choices = NULL),
      uiOutput(ns("sampling_box")),

      uiOutput(ns("confirm_state")),
      # After Confirm: the core path and the optional tabs (the one wording,
      # from sieve_tab_paths()).
      sieve_tab_paths_note(class = "small text-muted mt-3 mb-0"),
      hr(),
      selectInput(ns("palette"), "Color palette", choices = sieve_palette_choices(), selected = "nd")
    ),
    tags$script(HTML(sieve_ingest_row_js())),
        div(
          class = "d-flex align-items-center gap-2 mt-2 mb-2 flex-wrap",
          actionButton(ns("load_main"), "Load (step 2)", class = "btn-primary"),
          uiOutput(ns("confirm_button"), inline = TRUE),
          uiOutput(ns("status_chip"), inline = TRUE),
          tags$button(
            type = "button", class = "btn btn-outline-secondary btn-sm sieve-reset-widths",
            `data-sieve-table` = "ingest-items", "Reset column widths"
          )
        ),
    # The panels carry an id so that the server knows which one is open.
    navset_card_tab(
      id = ns("panels"),
      nav_panel(
        "Item grid",
        value = "grid",
        # The action bar: the status chip names the step the researcher is
        # at, and the Confirm button carries its state. It is in the main
        # panel so that it is visible whatever the window width.

        # What each column of the file is for, said here where the item pool
        # is shown, so a proposed role is never applied without being seen.
        uiOutput(ns("roles_notice")),
        # Results first: the item grid
        # sits under the action bar, the roles line and one compact row of
        # type controls; the response-bounds warning is one line here, with
        # each item it concerns marked in its Response bounds cell; the help and the
        # batch tools follow the grid.
        conditionalPanel(
          sprintf("output['%s'] === true", ns("has_items")),
          # Bulk typing by the number of response options (the distinct
          # values, or the points the declared bounds span), and any set of
          # items at once (every included item, a construct's items or a
          # facet's) to ordered or continuous (sieve_set_types()), in one row.
          tags$style(HTML(paste0(
            "#", ns("types_row"), " .form-group{margin-bottom:0;}",
            "#", ns("types_row"), " .selectize-control{margin-bottom:0;}"
          ))),
          div(
            id = ns("types_row"),
            class = "d-flex gap-2 flex-wrap align-items-center small",
            span(class = "text-muted", "Types:"),
            actionButton(ns("types_continuous"), "Five or more options: continuous",
              class = "btn-outline-secondary btn-sm",
              title = "Set every item with five or more options to continuous"
            ),
            actionButton(ns("types_ordered"), "Four or fewer options: ordered",
              class = "btn-outline-secondary btn-sm",
              title = "Set every item with four or fewer options to ordered"
            ),
            span(class = "text-muted ms-2", "Items:"),
            selectInput(ns("types_scope"), NULL,
              choices = c("every included item" = "all"),
              width = "220px"
            ),
            span(class = "text-muted", "to"),
            selectInput(ns("types_to"), NULL,
              choices = c("ordered" = "ordered", "continuous" = "continuous"),
              width = "130px"
            ),
            actionButton(ns("types_apply"), "Apply", class = "btn-outline-primary btn-sm")
          ),
          # The response bounds and the keying of a set of items at once, in
          # the same form as the types. An item's own are in its Response
          # bounds and Reverse? cells.
          tags$style(HTML(paste0(
            "#", ns("keying_row"), " .form-group{margin-bottom:0;}",
            "#", ns("keying_row"), " .selectize-control{margin-bottom:0;}"
          ))),
          # One closed line, so that the grid stays in view: an item's own
          # bounds and keying are edited in its cells, and these set many
          # items at once.
          tags$details(
            class = "small mt-1 mb-1",
            tags$summary("Response bounds and Reverse? for several items at once"),
          div(
            id = ns("keying_row"),
            class = "d-flex gap-2 flex-wrap align-items-center small mt-1",
            span(class = "text-muted", "Response bounds:"),
            numericInput(ns("bounds_lo"), NULL, value = NA, width = "90px"),
            span(class = "text-muted", "to"),
            numericInput(ns("bounds_hi"), NULL, value = NA, width = "90px"),
            span(class = "text-muted", "for"),

            actionButton(ns("bounds_apply"), "Declare",
              class = "btn-outline-primary btn-sm",
              title = "Declare these response bounds for the items in scope; nothing is reversed"
            ),
            actionButton(ns("bounds_clear"), "Clear",
              class = "btn-outline-secondary btn-sm",
              title = "Take back the declared response bounds of the items in scope"
            ),
            uiOutput(ns("bounds_labels"), inline = TRUE),
            span(class = "text-muted ms-2", "Reverse? of"),

            span(class = "text-muted", "to"),
            selectInput(ns("keying_to"), NULL, choices = c("Yes", "No"), width = "90px"),
            actionButton(ns("keying_apply"), "Apply", class = "btn-outline-primary btn-sm")
          )
          ),
          # Many items at once from a pasted list (sieve_item_list_entries(),
          # sieve_match_item_list()): exclude them, include them, or include
          # them and exclude every other item, with one step back.
          tags$details(
            class = "small mt-1 mb-1",
            tags$summary("Include or exclude items from a pasted list"),
            div(
              class = "mt-1",
              p(
                class = "text-muted mb-1",
                "Paste item names, one per line or separated by commas, semicolons or tabs (a column ",
                "copied from a spreadsheet works). Each entry is matched to a Variable Name, then an ",
                "Item Name, then either one ignoring case, then an item's wording ignoring case."
              ),
              textAreaInput(ns("list_text"), NULL, rows = 4, width = "100%", placeholder = "q1\nq2\nq7_r"),
              div(
                class = "d-flex gap-2 flex-wrap align-items-center",
                actionButton(ns("list_exclude"), "Exclude these",
                  class = "btn-outline-secondary btn-sm",
                  title = "Exclude the listed items from every analysis (they stay in the data)"
                ),
                actionButton(ns("list_include"), "Include these",
                  class = "btn-outline-secondary btn-sm",
                  title = "Include the listed items; every other item keeps its Use"
                ),
                actionButton(ns("list_only"), "Include only these",
                  class = "btn-outline-primary btn-sm",
                  title = "Include the listed items and exclude every other item"
                ),
                actionButton(ns("list_undo"), "Undo the last list",
                  class = "btn-outline-secondary btn-sm",
                  title = "Put back the Use every item had before the last of these three buttons"
                )
              ),
              uiOutput(ns("list_result"))
            )
          ),
          # The notes in one block: the card body spaces each of its items,
          # so a note that is empty takes no room inside it.
          div(
            class = "sieve-grid-notes",
            # The items an SPSS file calls ordinal (or nominal) that are typed
            # continuous here, with the one click that types them ordered.
            uiOutput(ns("spss_note")),
            # Items a description row marks as reverse keyed whose values
            # already correlate positively with their construct: held, with
            # the two answers.
            uiOutput(ns("reverse_ask")),
            # Items the five-option rule switched when a project was opened,
            # with the one click that keeps them ordered.
            uiOutput(ns("types_switched")),
            # A saved study whose Follow the item types setting recorded MLR
            # with FIML for continuous items keeps it on opening, with the one
            # click that takes ML with FIML (sieve_keep_legacy_mlr()).
            uiOutput(ns("estimator_kept")),
            # Stored scoring policies the opened project could not use as saved.
            uiOutput(ns("score_policy_issues")),
            # Items saved in the file beside their reverse-coded copies (q5
            # and q5_r), with the one click that excludes every original
            # (sieve_reversed_originals()).
            uiOutput(ns("originals_note")),
            # Two included items that are the same item twice (an item and its
            # reversed copy, both in the file), with the one click that
            # excludes the copy not keyed with the construct.
            uiOutput(ns("copies_note")),
            uiOutput(ns("grid_note")),
            uiOutput(ns("bounds_note")),
            # Hidden spans that seed the shared construct and facet datalists with
            # EVERY known value (not only those on the current page of the grid),
            # so the dropdowns are complete even when the item list is paginated.
            uiOutput(ns("dl_seed"))
          )
        ),
        DTOutput(ns("type_grid")),
        conditionalPanel(
          sprintf("output['%s'] === true", ns("has_items")),
          class = "mt-2",
          shiny::selectInput(ns("metadata_ids"),
            "Items for batch assignment (stable variable IDs)",
            choices = character(0),
            multiple = TRUE
          ),
          sieve_metadata_controls_ui(ns)
        ),
        # The explanation, below the grid: the first paragraph visible, the
        # rest in closed panels.
        div(
          class = "small text-muted mt-2",
          p(
            class = "mb-1",
            "This grid lists the items. When a data file is loaded, a role is proposed ",
            "for every column: a numeric column is proposed as an item unless it looks like an ",
            "identifier (a different whole number for every row, consecutive or not, or a name such ",
            "as id), a grouping variable (2 to 6 distinct values and a name such as sex, group or ",
            "school) or a background variable or outcome (a name such as age or gpa); a name that is ",
            "one of a numbered series (q1, q2, q3) is read as an item whatever its stem says, and a ",
            "column that is not stored as a number is never an item. These are proposals: review them on the ",
            strong("Variable roles"), " panel and change any that are wrong, by name in the roles ",
            "editor or in the Role cell, so that an ID, a grouping variable or a criterion does not ",
            "enter the item pool."
          )
        ),
        tags$details(open = NA,
          class = "small text-muted mb-2",
          tags$summary("How to edit the grid"),
          p(
            class = "mb-1",
            "Edit in place: the measurement ", strong("Type"), " (auto detected, then adjustable), ",
            "the ", strong("Construct"), " and the ", strong("Facet"), " (theory, not data; once you ",
            "name one it becomes a dropdown suggestion in every other row), and the ", strong("Item Name"),
            " (rename a variable to something meaningful and it follows downstream). The ",
            strong("Question"), " column shows the wording the file or the specification carried; ",
            "the full text is on hover. Set ", strong("Use"), " to ", strong("Excluded"),
            " to exclude an item from every analysis; excluded items move below the ",
            strong("Items excluded"), " line and are never deleted. Set ", strong("Reverse?"), " to ",
            strong("Yes"), " to reverse score an item within its response bounds, and to ",
            strong("No"), " to return it to its scoring: the reflection uses the same bounds both ways, ",
            "so it returns exactly the original values. An item that arrived reverse scored (recorded ",
            "by a specification, the description rows, the answer that the file holds it reverse ",
            "scored already, or a variable label that says so) is returned within the bounds recorded ",
            "with it; with none recorded, Reverse? No is refused until the scale's bounds are typed in ",
            "its Response bounds cell, where they can be corrected. Drag a column border to resize."
          ),
          p(
            class = "mb-1",
            strong("Many items at once."), " Under ", strong("Include or exclude items from a pasted list"),
            ", above the grid, paste item names (a column copied from a spreadsheet, or names separated ",
            "by commas, semicolons or tabs). ", strong("Exclude these"), " excludes the listed items, ",
            strong("Include these"), " includes them, and ", strong("Include only these"), " includes them ",
            "and excludes every other item. Each entry is matched to a Variable Name, then an Item Name, ",
            "then an item's wording; entries that match no item, or more than one, are listed and left ",
            "alone, and ", strong("Undo the last list"), " puts back the Use every changed item had. When ",
            "the file holds items beside their reverse-coded copies (named like q5 and q5_r, or rev_q5, ",
            "with values that run opposite), a note above the grid offers to exclude every original and ",
            "keep the copies when the copy is the one keyed with its construct: read from the construct's ",
            "other included items where it is assigned, and from the name where it is not. Every such ",
            "change is a step of the reproducible code, with its reason in the Use cell."
          ),
          p(
            class = "mb-1",
            strong("Response bounds"), " are the lowest and the highest response the instrument ",
            "allowed (1 and 7 on a seven point scale), a fact about the instrument, not about the ",
            "sample. A reversal reflects an item within them, ", tags$code("(min + max) - x"), ", ",
            "which flips the scale without changing its range. Type an item's bounds in its ",
            strong("Response bounds"), " cell (as 1 to 7; clear the cell to take them back), or ",
            "declare them for every included item, or for the items of one construct or facet, under ",
            "Response bounds and Reverse? for several items at once, above the grid; declaring ",
            "reverses nothing. When none are ",
            "declared, a reversal uses the item's observed minimum and maximum, the cell says so, and ",
            "those bounds are stored with the item: returning it to its scoring (Reverse? No) uses ",
            "them, and a later reversal uses them again unless bounds are declared for it, which ",
            "replaces them. The observed range is ",
            "not evidence of the instrument's endpoints: a point no respondent chose (an endpoint of a ",
            "skewed item, any point in a small sample) leaves it narrower than the scale, and an item ",
            "reflected within a narrower range is placed on a different metric from the items ",
            "reflected within the scale, so declare the bounds whenever the scale is known (Bandalos, ",
            "2018, on reverse keyed items). When a data file carries value labels (SPSS, Stata) and ",
            "every observed response lies within them, the lowest and the highest labeled values are ",
            "declared as the item's bounds at load, and its cell says so (from the file's value ",
            "labels). A cell cleared of them keeps them as its hint, and when every item in scope has ",
            "the same, the button under Response bounds and Reverse? for several items at once enters ",
            "them in the two fields, which Declare then declares for the items in scope. A ",
            "specification's rev_lo and rev_hi columns declare bounds too."
          )
        ),
        tags$details(open = NA,
          class = "small text-muted mb-2",
          tags$summary("Ordered versus continuous: what the type changes"),
          p(
            class = "mb-1",
            "The type of each item decides how its associations are estimated and which estimator ",
            "the models use. An ", strong("ordered"), " item (a rating scale with a few points) or a ",
            strong("binary"), " item is modeled through the polychoric (or tetrachoric) correlations ",
            "of its latent response, and every model that includes such an item is fit with WLSMV ",
            "(diagonally weighted least squares with a mean and variance adjusted test statistic); ",
            "Pearson correlations of coarse responses attenuate the associations between the latent ",
            "continua, and the attenuation grows as the number of categories falls. A ",
            strong("continuous"), " item is modeled through Pearson covariances with maximum likelihood (ML, DMAR's default), ",
            "and missing responses are handled by full information maximum likelihood (FIML), ",
            "so an incomplete case still contributes; under WLSMV the complete cases on the included items are used. ",
            "ML's standard errors and test statistic are normal theory: for markedly non-normal items ",
            "(strongly skewed or heavy-tailed), choose MLR with FIML (robust maximum likelihood: ",
            "Huber-White standard errors and a scaled test statistic) with the Estimator control in step 4; ",
            "ML on the complete cases is a choice there too. SIEVER does not switch the estimator on its own."
          ),
          p(
            class = "mb-1",
            strong("The proposed type."), " An item with two distinct values is proposed as binary, one with ",
            "three or four distinct integer values as ordered, and one with five or more distinct values ",
            "(or values that are not integers) as continuous. The reason: Rhemtulla, Brosseau-Liard and ",
            "Savalei (2012) found that with five or more categories, estimation that treats the items as ",
            "continuous performs comparably to the categorical estimator in many conditions, especially ",
            "when the thresholds are roughly symmetric, and avoids the categorical estimator's cost (a ",
            "polychoric matrix per item pool, a weight matrix that grows with the square of the number ",
            "of thresholds and correlations) and its listwise deletion; with fewer categories, or with ",
            "strongly asymmetric thresholds, the categorical estimator is more accurate. The line under ",
            "each Type cell says whether the type is proposed or your choice and gives the item's response ",
            "options and skew (the category counts on hover), so you can judge. With the ",
            "checkbox in step 1 on (the default), the rule also holds for a type a specification ",
            "gives and for an opened project's types that were proposed (detected, or read from a ",
            "specification); a type you chose (in a Type cell, with the buttons below, or recorded so in ",
            "the specification) is kept, and the note above the grid names any item switched when a ",
            "project is opened, with one click to keep it ordered. An item typed ordered ",
            "in a specification with five or more response options (its distinct ",
            "values, or the points its declared response bounds span when that is more, so a seven point ",
            "item on which four points were chosen has seven) is loaded as continuous, and the status ",
            "chip says how many were switched; a binary item, an item with four or fewer options, and an ",
            "item observed with fewer than three values keep their type. To keep the categorical ",
            "treatment, set Type to ordered in the grid, or turn the checkbox off before loading. The two ",
            "buttons above the grid set the type of every item with five or more options, or with four ",
            "or fewer, at once, and the Set the type of control sets every included item, or the items ",
            "of one construct or one facet, to ordered or continuous. When the data come from an SPSS ",
            "file, its dictionary's measurement level (Nominal, Ordinal, Scale) is read as well, and a ",
            "note above the grid names the items SPSS calls ordinal or nominal that are typed ",
            "continuous here, with a button that sets them to ordered. The proposal ",
            "is yours to change: set any item to ordered in its Type cell, and the estimator follows. ",
            "The Estimator control in step 4 of the sidebar sets the estimator for every model in the ",
            "study without retyping the items, and each model tab (CFA, SEM, Reliability and the others) ",
            "has the same control, which follows the study's setting unless it is set otherwise there."
          ),
          p(
            class = "mb-1",
            "Downstream: reliability is Robust omega (observed denominator) throughout; for a construct whose ",
            "items are all ordered or binary its denominator is built from the unrestricted polychoric ",
            "correlations, and otherwise from the items' covariance matrix (the Reliability tab's item treatment ",
            "control can switch between the two); the Items tab's diagnostics name the ",
            "correlations they were computed from; parallel analysis draws its reference datasets with the ",
            "matching correlations, which is slower for polychoric ones (the Dimensionality tab states the ",
            "estimated wait). Changing a type changes the estimator decision, so the study is confirmed again."
          )
        )
      ),
      nav_panel(
        "Variable roles",
        p(
          class = "text-muted small mt-2",
          "Every column of the data, with the role it has. An ", strong("item"), " belongs to the ",
          "item pool; an ", strong("identifier"), " names the rows; a ", strong("group"),
          " defines the groups of a multiple group analysis (CV and Invariance); a ",
          strong("criterion"), " is an external variable the scores are related to (Validity, SEM), ",
          "and can carry the kind of validity evidence it is for (convergent, discriminant, or ",
          "criterion related); an ", strong("antecedent"), " is a variable that exists before the ",
          "target construct (age, tenure, a prior condition) and is offered as an observed predictor ",
          "of the constructs on the SEM tab, never as a criterion (it is never proposed automatically; ",
          "you give the role); an ", strong("auxiliary"), " variable is kept with the data and used ",
          "as a continuous saturated correlate in FIML fits when an analyzed item has a missing value (with every item observed it adds no information and is left out, as the method lines of the CFA, SEM, Validity, Bifactor and Reliability pages say). Review the auxiliary set when complete cases are few. A role is independent of how the column is stored: a numeric column ",
          "can be a group or a criterion, and only a column that is not stored as a number cannot be ",
          "an item. ", strong("Proposed"), " is the role proposed when the data were read and ",
          strong("Why"), " is the reason; neither is a decision. A column that becomes an item joins ",
          "the item grid with its values as loaded; a column that stops being an item leaves the ",
          "item grid and the item ledger (the data keep it), and the item types then need to be ",
          "confirmed again. ", strong("Item Name"), " is the item's name in the item grid (edited ",
          "there), blank for a column that is not an item; ", strong("Question"), " is the item's ",
          "wording, under Columns shown."
        ),
        card(
          card_header("Set a role by name"),
          p(
            class = "text-muted small",
            "Paste or type the variable names (separated by commas, spaces or line breaks; matched ",
            "without regard to case), or pick them from the list, choose the role, and apply. The list ",
            "shows an item's Item Name after its variable name, so an item can be found by either. Names ",
            "that match no column are reported and nothing is changed for them."
          ),
          layout_columns(
            col_widths = c(3, 3, 6),
            selectInput(ns("roles_role"), "Role",
              choices = c(
                "Identifier" = "identifier", "Group" = "group",
                "Criterion" = "criterion", "Antecedent" = "antecedent",
                "Auxiliary" = "auxiliary", "Item" = "item"
              ),
              selected = "auxiliary"
            ),
            conditionalPanel(
              sprintf("input['%s'] == 'criterion'", ns("roles_role")),
              selectInput(ns("roles_kind"), "Validity kind (for a criterion)",
                choices = c(
                  "not stated" = "", "convergent" = "convergent",
                  "discriminant" = "discriminant", "criterion related" = "criterion"
                ),
                selected = ""
              )
            ),
            selectizeInput(ns("roles_pick"), "Pick from the list", choices = NULL, multiple = TRUE)
          ),
          textAreaInput(ns("roles_names"), "Variable names",
            rows = 2, width = "100%",
            placeholder = "age, gpa, tenure (or one per line)"
          ),
          actionButton(ns("roles_apply"), "Apply the role", class = "btn-outline-primary btn-sm"),
          uiOutput(ns("roles_editor_note"))
        ),
        checkboxGroupInput(
          ns("roles_cols"), "Columns shown",
          inline = TRUE,
          choices = sieve_roles_grid_headers()[-1],
          selected = sieve_roles_grid_shown()
        ),
        tags$button(
          type = "button", class = "btn btn-outline-secondary btn-sm sieve-reset-widths",
          `data-sieve-table` = "ingest-roles", "Reset column widths"
        ),
        DTOutput(ns("roles_grid"))
      ),
      nav_panel(
        "Ledger summary",
        card(
          card_header("Items by construct and facet"),
          p(
            class = "text-muted small",
            "What the item ledger holds, read from the grid: one row per construct and facet with ",
            "the items, the included and excluded counts, the types of the included items, and the ",
            "reverse scored items. The constructs to analyze (sidebar, step 3) include or exclude ",
            "whole constructs."
          ),
          DTOutput(ns("ledger_tbl"))
        ),
        # The keying balance of each construct.
        card(
          card_header("Reverse keyed items per construct"),
          p(
            class = "text-muted small",
            "Reverse keyed items are a deliberate design choice, not an accident: ",
            "none (avoiding the method factor mixed keying induces) or enough to ",
            "detect acquiescent responding, never a single stray one. This table ",
            "only reports the count of the included items; the choice is substantive."
          ),
          DTOutput(ns("reverse_balance_tbl"))
        ),
        card(
          card_header("Items and their questions"),
          p(
            class = "text-muted small",
            "Each item with its construct, facet, type, status, keying and the question (wording) the ",
            "file or the specification carried; the full text is on hover."
          ),
          DTOutput(ns("question_tbl"))
        ),
        card(
          card_header("Seeded theory"),
          verbatimTextOutput(ns("theory_txt"))
        )
      ),
      nav_panel(
        "Dataset overview",
        card(
          card_header("Summary"),
          DTOutput(ns("summary_tbl"))
        ),
        card(
          card_header("Distribution of one variable"),
          p(
            class = "text-muted small",
            "Pick any column of the data, item or not; the list shows each variable's role. The ",
            "first item is shown first. An item is drawn with its processed values (reverse scored ",
            "as scored in the session). A whole number variable with at most 20 distinct values (a ",
            "rating scale, a count, a 0/1 item) has a bar per value with its count above it and every ",
            "value on the axis, the points of an item's declared response bounds that no one chose ",
            "included as empty bars; any other number has a histogram; a variable not stored as a ",
            "number has a bar per level. The subtitle gives n, the number missing, the mean and the ",
            "SD (n - 1 divisor) of the values drawn, and the vertical line marks the mean."
          ),
          layout_columns(
            col_widths = c(4, 8),
            selectInput(ns("plot_var"), "Variable", choices = NULL),
            plotOutput(ns("dist_plot"), height = "300px")
          )
        ),
        card(
          card_header("Variables that are not items (identifiers, groups, criteria, antecedents, auxiliary)"),
          p(
            class = "text-muted small",
            "Non item columns with their roles and their validity kind, shown like the item grid so you ",
            "can verify what you have. Roles are changed on the Variable roles panel."
          ),
          # The antecedents under their own heading, when there are any.
          uiOutput(ns("antecedent_note")),
          DTOutput(ns("aux_tbl"))
        )
      )
    )
  )
}

#' Ingest module server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer observe observeEvent req validate need reactive
#' @importFrom shiny reactiveVal reactiveValues isolate updateSelectInput updateActionButton
#' @importFrom shiny updateSelectizeInput updateTextAreaInput
#' @importFrom shiny renderText renderUI renderPlot showNotification div br em tags strong span
#' @importFrom DT renderDT datatable formatRound formatStyle styleEqual
mod_ingest_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$has_items <- reactive(!is.null(state$meta) && nrow(state$meta) > 0L)
    shiny::outputOptions(output, "has_items", suspendWhenHidden = FALSE)

    # Bumped whenever the table should be fully rebuilt (data load, palette
    # change, clear, an exclusion that regroups the rows, a role change).
    # In-cell edits flow one way into state without a rebuild, so the user
    # never loses scroll position mid-edit; a reverse toggle updates its
    # own row through a message to the table.
    rebuild <- reactiveVal(0L)

    # How the current confirmation was reached: "specification" when a
    # specification that typed every item confirmed the study (the status
    # chip says so), otherwise NULL. Cleared when a load or an open changes
    # the session; a file that is refused changes nothing, the chip included.
    confirmed_from <- function(value) {
      if (missing(value)) {
        record <- state$confirmation
        return(if (identical(record$origin, "specification")) "specification" else NULL)
      }
      invisible(NULL)
    }

    # What the study was when it was confirmed (the dataset, the ledger's
    # analysis-relevant columns, the content key of the item data), so the
    # Confirm button can say what changed since. NULL before any
    # confirmation of the loaded data.
    confirmed_at <- function(value) {
      if (missing(value)) return(state$confirmation)
      state$confirmation <- value
      invisible(value)
    }

    # The one line status above the grid (a reversal's bounds, a refused
    # edit), in place of a notification.
    grid_note <- reactiveVal(NULL)

    # What the last load read from the file's description rows, for the chip.
    described <- reactiveVal(NULL)
    # The notes of the last Load: what needs attention is written in the
    # status line and stays there until the next Load, rather than in
    # stacked notifications that vanish in seconds. `kind` is "proposal" (settled by
    # the confirmation), "warning", "error" or "info".
    load_notes <- reactiveVal(list(text = character(0), kind = character(0)))
    load_note <- function(text, kind = "info") {
      now <- isolate(load_notes())
      # The notes belong to the dataset the Load made: data opened another
      # way (a project) leave them behind, and a note written for the data
      # now in force (a specification applied from step 3) starts afresh
      # rather than bringing them back.
      if (length(now$text) && !identical(now$dataset, isolate(state$dataset_id))) {
        now <- list(text = character(0), kind = character(0))
      }
      # A note already listed is not listed twice (a specification applied
      # again from step 3 says the same things).
      text <- setdiff(text, now$text)
      load_notes(list(
        text = c(now$text, text), kind = c(now$kind, rep(kind, length(text))),
        dataset = isolate(state$dataset_id)
      ))
    }
    # Why the last Load attempt changed nothing (a selection SIEVER cannot
    # load, a file it cannot read, a file with no item column, a
    # specification refused, from step 1 or step 3), in the status line until
    # the next attempt; the notes of the data in force stay beside it, since
    # the session is unchanged.
    load_error <- reactiveVal(NULL)
    clear_load_notes <- function() load_notes(list(text = character(0), kind = character(0)))
    # The marked items the last load held for the researcher's answer (a
    # named vector of item-rest correlations), or NULL.
    reverse_held <- reactiveVal(NULL)

    # What the roles editor did last.
    roles_note <- reactiveVal(NULL)

    # The five-option rule's switch at the last load or open, in one
    # sentence for the chip (NULL when nothing was switched).
    switched_note <- reactiveVal(NULL)
    # The rule in force: five or more response options read as continuous
    # while the checkbox in step 1 is on (the default), nothing otherwise.
    min_options <- function() {
      if (isTRUE(isolate(input$five_continuous) %||% TRUE)) 5L else NULL
    }
    # The project bar opens projects too, so it reads the checkbox from the
    # session (see mod_project.R).
    observe({
      state$five_continuous <- isTRUE(input$five_continuous %||% TRUE)
    })

    # Derived keys, written only when their value changes (a reactive value
    # that is assigned an identical value invalidates nothing), so an output
    # that depends on one is evaluated again only when what it shows can
    # differ: the pool key covers which items there are, their constructs,
    # facets, types and included status; the keying key the reverse flags,
    # the bounds and where they came from (declared or observed); the ledger
    # key every ledger column; the roles key the
    # roles table. A reverse toggle moves the keying key and the ledger key
    # and nothing else, so the summary table, the estimator, the chip's
    # counts and the constructs to analyze are left alone.
    # The labels key covers the items, their Item Names and their wordings,
    # which the Variable roles grid and the roles picker show.
    keys <- reactiveValues(
      pool = NULL, keying = NULL, ledger = NULL, roles = NULL, dataset = NULL,
      labels = NULL
    )
    observe({
      m <- state$meta
      keys$pool <- if (is.null(m)) NULL else sieve_hash(list(
        item = as.character(m$item), construct = as.character(m$construct), facet = as.character(m$facet),
        type = as.character(m$type), status = as.character(m$status)
      ))
      keys$labels <- if (is.null(m)) NULL else sieve_hash(list(
        item = as.character(m$item), label = as.character(m$label),
        wording = if ("wording" %in% names(m)) as.character(m$wording)
      ))
      keys$keying <- if (is.null(m)) NULL else sieve_hash(list(
        item = as.character(m$item), reverse = m$reverse %in% TRUE,
        lo = as.numeric(m$rev_lo %||% rep(NA_real_, nrow(m))), hi = as.numeric(m$rev_hi %||% rep(NA_real_, nrow(m))),
        kind = as.character(m$rev_bounds %||% rep(NA_character_, nrow(m)))
      ))
      keys$ledger <- if (is.null(m)) NULL else sieve_ledger_key(m)
      keys$dataset <- state$dataset_id
    })
    observe({
      roles <- sieve_roles_table(state$raw, state$items, state$roles)
      keys$roles <- if (is.null(roles)) {
        NULL
      } else {
        sieve_hash(list(
          variable = as.character(roles$variable), role = as.character(roles$role),
          kind = as.character(roles$kind %||% rep(NA_character_, nrow(roles)))
        ))
      }
    })
    roles_now <- function() isolate(sieve_roles_table(state$raw, state$items, state$roles))

    # The keying of each item as the grid last drew it (the reverse flag,
    # the bounds and where they came from), so that a reversal made
    # anywhere (this grid's cell or its Reverse? row, a specification) is
    # pushed into its row without a rebuild, and a declaration of bounds
    # (which changes the response options under the Type cell too) draws
    # the grid again.
    drawn <- new.env(parent = emptyenv())
    drawn$keying <- NULL
    keying_of <- function(m) {
      m <- sieve_meta_keying(m)
      stats::setNames(
        paste(m$reverse %in% TRUE, as.numeric(m$rev_lo), as.numeric(m$rev_hi), as.character(m$rev_bounds), sep = "|"),
        m$item
      )
    }
    # The included status of each item as the grid last drew it, so that an
    # exclusion or an inclusion made on another tab (the Items tab, the
    # Screening tab, the Drop items control) redraws the grid, which then
    # shows each item above or below the Items excluded line as it now is.
    drawn$status <- NULL

    # The range check the Response bounds cells were drawn with (set where
    # the grid is drawn and by each row update): what the cells state, the
    # common range and the items each warns about with their observed
    # ranges, compared without the rows an update pushes itself. (Whether
    # an item lies outside the declared bounds is said only by the line
    # above the grid, which follows the keying key.)
    drawn$range <- NULL
    range_view <- function(check, except = character()) {
      if (is.null(check)) {
        return(NULL)
      }
      d <- check$differing
      keep <- !d$item %in% except
      list(common = check$common, differing = paste(d$item[keep], d$min[keep], d$max[keep], sep = "|"))
    }
    # Rows of the item grid, pushed to the table in place in one message: for
    # each, the Type cell (whose note reads the declared bounds), the
    # summaries, the distribution, the Reverse? cell and the Response bounds
    # cell. A reversal within bounds other than the item's observed range
    # moves its observed minimum and maximum, and with them possibly the
    # study's common range (the modal observed range) that the other items'
    # Response bounds cells state: when their warnings differ from the ones
    # drawn, the grid is drawn again instead (rare; a declaration only takes
    # its own item out of the warning, so it never does).
    push_rows <- function(items) {
      m <- isolate(state$meta)
      it <- isolate(state$items)
      items <- intersect(as.character(items), intersect(as.character(m$item), names(it)))
      if (!length(items)) {
        return(invisible(NULL))
      }
      check <- sieve_range_check(m)
      if (!is.null(drawn$range) && !identical(range_view(drawn$range, items), range_view(check, items))) {
        drawn$range <- check
        rebuild(isolate(rebuild()) + 1L)
        return(invisible(NULL))
      }
      drawn$range <- check
      pal <- sieve_palette_colors(isolate(state$palette) %||% "nd", 1L)[1]
      cols <- sieve_grid_columns()
      keyed <- sieve_meta_keying(m)
      labels <- sieve_label_bounds_of(isolate(state$raw), items)
      holds <- sieve_holds_reversal_bounds(
        keyed, items, it, isolate(state$raw), isolate(state$transforms) %||% list(), isolate(state$dataset_id)
      )
      rows <- lapply(seq_along(items), function(k) {
        item <- items[k]
        i <- match(item, m$item)
        cells <- list(
          Type = paste0(
            sieve_cell_select(ns("cell_edit"), item, "type", m$type[i], c("continuous", "ordered", "binary")),
            sieve_type_cell_note(m, i, it)
          ),
          Mean = m$mean[i], SD = m$sd[i], Min = m$min[i], Max = m$max[i],
          Distribution = sieve_spark_svg(it[[item]], fill = pal),
          Reverse = sieve_reverse_cell(ns("cell_edit"), m, i),
          Bounds = sieve_bounds_cell(ns("cell_edit"), keyed, i,
            labels = labels[[item]], check = check, held = holds[k]
          )
        )
        names(cells) <- as.character(match(names(cells), cols) - 1L)
        list(key = item, cells = cells)
      })
      if (!is.null(drawn$keying)) {
        drawn$keying[items] <- keying_of(m)[items]
      }
      sieve_session_message(session, "sieve_ingest_row", list(
        id = ns("type_grid"), key_col = match("Variable", cols) - 1L, rows = rows
      ))
      invisible(NULL)
    }
    observe({
      keys$keying
      m <- isolate(state$meta)
      if (is.null(m) || is.null(drawn$keying)) {
        return()
      }
      now <- keying_of(m)
      common <- intersect(names(drawn$keying), names(now))
      moved <- common[drawn$keying[common] != now[common]]
      if (!length(moved)) {
        return()
      }
      # The items whose reverse flag moved are pushed into their rows in one
      # message; an item whose bounds moved with its keying unchanged was
      # declared bounds outside its own cell (the row for several items, a
      # specification), and the grid is drawn again for it.
      flag <- function(k) startsWith(k, "TRUE|")
      flipped <- flag(drawn$keying[moved]) != flag(now[moved])
      if (any(!flipped)) {
        drawn$keying <- now
        rebuild(isolate(rebuild()) + 1L)
        return()
      }
      push_rows(moved)
    })
    # An item included or excluded anywhere: when the status of an item the
    # grid shows is not the one it drew, the grid is drawn again (the row
    # moves across the Items excluded line, so a row update would not do).
    observe({
      keys$pool
      m <- isolate(state$meta)
      if (is.null(m) || is.null(drawn$status)) {
        return()
      }
      now <- stats::setNames(as.character(m$status), m$item)
      common <- intersect(names(drawn$status), names(now))
      if (any(drawn$status[common] != now[common])) {
        drawn$status <- now
        rebuild(isolate(rebuild()) + 1L)
      }
    })

    # Reverse scoring goes through the one transformation service
    # (sieve_apply_reverse() in fct_transform.R): it reflects each item within
    # its response bounds, updates the data and the ledger together, records
    # the operation, and logs the R that reproduces it. This wrapper reports
    # the outcome in the status line above the grid: what was done (and that
    # the observed range was used, when no bounds were declared), or why
    # nothing was changed. No pop-up.
    reverse_items <- function(items, origin) {
      out <- sieve_apply_reverse(state, items, origin = origin)
      lines <- c(
        vapply(out$applied, sieve_reverse_message, character(1)),
        unname(out$refused)
      )
      if (length(lines)) {
        grid_note(list(
          kind = if (length(out$refused)) "error" else if (any(vapply(
            out$applied,
            function(rec) identical(
              rec$bounds,
              "observed"
            ),
            logical(1)
          ))) "warning" else "info",
          text = paste(lines, collapse = " ")
        ))
      }
      out
    }

    # ---- Step 1 and 2: the files and the load ------------------------------

    is_file_source <- function(src) !src %in% c("hs", "hs9")

    output$source_help <- renderUI({
      src <- input$source %||% "hs9"
      text <- switch(src,
        ready = paste(
          "Afterwards SIEVER knows the roles of the variables and the constructs and facets of the items;",
          "a project file also brings the item types, the keying and the session script, confirmed as",
          "it was saved. A data file is SIEVER-ready when its constructs and facets are in it: as",
          "description rows under the header whose first cell says construct, facet, reverse or question (a .csv; the",
          "question row is each item's wording), as column names of the form Construct__Facet__item",
          "(two underscores), or as SPSS labels of the form Construct: Facet: wording. The checkbox",
          "turns the reading of such rows off for a file whose second row is data. You confirm the",
          "item types."
        ),
        pair = paste(
          "Choose the data file and its specification (.csv) together. Afterwards SIEVER knows the roles,",
          "the item names and wordings, the types, the constructs and facets, the included items and the",
          "keying; when the specification types every item the study is confirmed from it. A template",
          "to fill in comes from sieve_specification_template()."
        ),
        alone = paste(
          "Afterwards SIEVER knows the roles it proposed and the types it detected; the constructs and",
          "facets go in the grid (step 3), then you confirm."
        ),
        hs9 = "The nine Holzinger and Swineford tests, declared with their three constructs; every test is continuous, so the types confirm on load.",
        hs = "The full Holzinger and Swineford data, 24 tests in five a priori domains; the types confirm on load.",
        "A data file alone: the roles are proposed and the types detected; the constructs go in the grid."
      )
      p(class = "small text-muted mb-1", text)
    })

    can_load <- reactive({
      src <- input$source %||% "hs9"
      !is_file_source(src) || NROW(input$file) > 0L
    })
    # What Load would do now. With no study in the session it reads the
    # chosen source. With one, a bundled example or a data file replaces the
    # study (every grid change, fit and result, and the session script), a
    # specification alone is applied to the loaded data, and a project file
    # is shown first in its own dialog (sieve_sort_uploads(), as do_load()
    # reads them). The sidebar's Load and the step's note say which, and a
    # replacing Load asks first (ask_replace() below), so that exclusions
    # are not lost to a press of Load taken for the next step. The main
    # bar's Load is the next step only before the first load: once a study
    # is in it is hidden, since nothing in the item grid calls for replacing
    # the study, which is a step 1 and 2 decision made in the sidebar.
    study_loaded <- reactive({
      keys$dataset
      !is.null(isolate(state$meta))
    })
    load_effect <- reactive(sieve_load_plan(input$source %||% "hs9", input$file,
      loaded = study_loaded(), applied_spec = spec_applied()))
    # The selection the loaded study came from, so that a Load of the same
    # selection is called a reload and a Load of anything else (another
    # source, or a file chosen again, which may hold other values under
    # the same name) a new study. A chosen file is known by the path Shiny
    # gave it.
    same_selection <- function() {
      at <- loaded_at()
      !is.null(at) && identical(input$source %||% "hs9", at$src) &&
        (!is_file_source(at$src) || identical(sort(as.character(input$file$datapath %||% character(0))), at$files))
    }
    # The text under the file box follows the study: what Load would do with
    # the chosen files, or that they are the ones loaded (sieve.js shows it
    # in place of Shiny's "Upload complete").
    observe({
      effect <- load_effect()
      text <- switch(effect,
        read = "Ready: press Load (step 2)",
        replace = if (same_selection()) "Loaded" else "Ready: Load replaces this study",
        spec = "Ready: Load applies the specification",
        spec_alone = "A specification: choose it with its data file",
        spec_applied = "Applied",
        project = "Ready: Load shows the project first",
        problem = "Not loadable: see step 2",
        NULL
      )
      if (!is.null(text)) {
        session$sendCustomMessage("sieve-file-bar", list(id = ns("file"), text = text))
      }
    })
    observe({
      effect <- load_effect()
      label <- switch(effect,
        replace = "Load (replaces this study)",
        spec = "Load (applies the specification)",
        project = "Load (shows the project first)",
        NULL
      )
      first <- !study_loaded()
      refused <- effect %in% c("problem", "spec_alone", "spec_applied")
      updateActionButton(session, "load", disabled = !can_load() || refused, label = label %||% "Load")
      updateActionButton(session, "load_main", disabled = !can_load() || refused || !first)
      session$sendCustomMessage("sieve-button-class", list(
        id = ns("load_main"),
        add = if (first) character(0) else "d-none",
        remove = if (first) "d-none" else character(0)
      ))
    })
    output$step2_why <- renderUI({
      src <- input$source %||% "hs9"
      files <- input$file
      effect <- load_effect()
      if (identical(effect, "problem")) {
        chosen <- sieve_sort_uploads(as.character(files$name), as.character(files$datapath))
        return(p(class = "small text-warning mb-1", "Load cannot use the chosen files: ", chosen$problem))
      }
      if (identical(effect, "spec_alone")) {
        return(p(
          class = "small text-warning mb-1",
          sprintf(paste(
            "%s is a specification. Choose it together with its data file, or load the data",
            "first and then apply the specification."
          ), paste(as.character(files$name), collapse = " and "))
        ))
      }
      if (identical(effect, "spec_applied")) {
        return(p(
          class = "small text-muted mb-1",
          "The chosen specification is applied to the loaded data; choose another file in step 1 to load or apply it."
        ))
      }
      if (study_loaded()) {
        what <- switch(effect,
          replace = if (same_selection()) {
            "Load would read the same source afresh and replace this study, so it asks first."
          } else {
            "Load would read the chosen source and replace this study with it, so it asks first."
          },
          spec = "Load would apply the chosen specification to the loaded data.",
          project = "Load would show the chosen project first; nothing changes until you open it.",
          "Choose a file in step 1 to load another study."
        )
        return(p(
          class = "small text-muted mb-1",
          sprintf("Loaded: %s. ", isolate(state$dataset_name) %||% "the study"), what,
          " A change in the grid (an exclusion, a type, a construct) takes effect as it is made, without Load."
        ))
      }
      if (!is_file_source(src)) {
        return(p(class = "small text-muted mb-1", "Press Load to read the bundled example."))
      }
      if (is.null(files) || !NROW(files)) {
        return(p(
          class = "small text-warning mb-1",
          "Choose a file first (step 1); Load is enabled once one is chosen."
        ))
      }
      p(
        class = "small text-muted mb-1",
        sprintf("Press Load to read %s.", paste(as.character(files$name), collapse = " and "))
      )
    })

    # Loading is a plain function so that the bundled examples and the
    # uploaded files share one path.
    do_load <- function() {
      load_error(NULL)
      # The study's estimator setting as it stands before this Load, which
      # resets it with the rest of the session (see reset_estimator_choice()).
      estimator_before <- isolate(state$estimator_choice) %||% "auto"
      src <- input$source %||% "hs9"
      effect <- sieve_load_plan(src, input$file, loaded = !is.null(isolate(state$meta)), applied_spec = spec_applied())
      bundled <- !is_file_source(src)
      # The selection this Load reads, tagged with the dataset it makes
      # (sieve_reset_dataset() gives the new identity here, synchronously),
      # so that the snapshot below can tell a study loaded from it from one
      # opened as a project, and a reload from a new study.
      files_now <- if (bundled) character(0) else sort(as.character(input$file$datapath %||% character(0)))
      on.exit(load_selection(list(id = isolate(state$dataset_id), src = src, files = files_now)), add = TRUE)
      if (bundled) {
        fx <- if (identical(src, "hs9")) sieve_hs9_fixture() else sieve_hs_fixture()
        validate(need(!is.null(fx), "DMAR is not available, so the bundled example cannot be loaded."))
        sieve_study_state(fx, state = state, confirmed = FALSE)
        clear_load_notes()
        confirmed_from(NULL)
        confirmed_at(NULL)
        described(NULL)
        reverse_held(NULL)
        grid_note(NULL)
        switched_note(NULL)
        reset_estimator_choice(estimator_before)
      } else {
        req(input$file)
        # The input takes one file or two: a project file, a data file, or
        # a data file with its specification (sieve_sort_uploads() in
        # fct_prepare.R tells them apart). A project file goes to the Open
        # project dialog, with what it holds shown before anything changes.
        chosen <- sieve_sort_uploads(as.character(input$file$name), as.character(input$file$datapath))
        if (identical(effect, "problem")) {
          load_error(chosen$problem)
          return(invisible(NULL))
        }
        if (identical(effect, "project")) {
          open_dialog(preview = chosen$project)
          return(invisible(NULL))
        }
        if (effect %in% c("spec_alone", "spec", "spec_applied")) {
          # A specification on its own: for the data already loaded, or
          # nothing to apply it to yet.
          if (identical(effect, "spec_alone")) {
            load_error(sprintf(paste(
              "%s is a specification. Choose it together with its data file, or load",
              "the data first and then apply the specification (step 3)."
            ), chosen$spec$name))
            return(invisible(NULL))
          }
          if (identical(effect, "spec_applied")) {
            sieve_notify("This specification is already applied to the loaded data.", type = "message")
            return(invisible(NULL))
          }
          if (!is.null(restore_spec(chosen$spec$path, chosen$spec$name, from_load = TRUE))) {
            spec_applied(chosen$spec$path)
          }
          return(invisible(NULL))
        }
        name <- chosen$data$name
        # The file is read and its roles are proposed before anything in the
        # session is touched (sieve_load_frame()), so a file that cannot be
        # used leaves the loaded project as it is.
        read_rows <- isTRUE(input$read_rows %||% TRUE)
        read <- if (isTRUE(input$parse_override) && tolower(tools::file_ext(name)) %in% c("csv", "txt")) {
          sieve_read_delimited(chosen$data$path, name,
            sep = if (identical(input$parse_sep, "tab")) "\t" else input$parse_sep %||% ",",
            dec = input$parse_dec %||% ".", encoding = input$parse_encoding %||% "UTF-8", description = read_rows
          )
        } else {
          # The encoding chosen applies without the box (see the select). A
          # refusal names the select as well as the argument, since this is
          # the Ingest tab's own read.
          sieve_read_data(chosen$data$path, name,
            description = read_rows, encoding = input$parse_encoding %||% "UTF-8",
            ingest = TRUE
          )
        }
        if (!read$ok) {
          load_error(sprintf("%s was not loaded: %s. The session is unchanged.", name, read$error))
          return(invisible(NULL))
        }
        loaded <- sieve_load_frame(state, read$value, name, read$code, description = read$description)
        if (!isTRUE(loaded$ok)) {
          load_error(loaded$problem)
          return(invisible(NULL))
        }
        # The data are replaced: the notes of the earlier data go with them.
        clear_load_notes()
        if (length(read$warnings)) {
          load_note(sprintf("While reading %s: %s", name, paste(read$warnings, collapse = "; ")), "warning")
        }
        reset_estimator_choice(estimator_before)
        confirmed_from(NULL)
        confirmed_at(NULL)
        switched_note(NULL)
        said_now <- loaded$described
        if (!read_rows) {
          # The chip says that the rows under the header were read as data.
          said_now$off <- TRUE
        }
        described(said_now)
        reverse_held(if (length(said_now$held)) said_now$held else NULL)
        grid_note(NULL)
        roles <- loaded$roles
        # The one notification of a Load, under one id so that a second Load
        # replaces it; what needs attention is in the status line. A
        # specification chosen with the data is named in it (spec_said, set
        # below), and what the specification did or why it was refused is a
        # note of the status line, not a notification of its own.
        spec_said <- ""
        on.exit(sieve_notify(
          sprintf(
            "%s loaded (%d columns)%s.%s", name, nrow(roles), spec_said,
            if (length(isolate(load_notes())$text)) " What to review is listed in the status line above the grid." else ""
          ),
          type = "message", id = session$ns("load_notification")
        ), add = TRUE)
        if (is.null(chosen$spec)) {
          load_note(sprintf(
            paste(
              "A role was proposed for each of the %d columns of %s: %s. These are",
              "proposals: review them on the Variable roles panel (step 3), change any that are",
              "wrong, then confirm the study (step 4)."
            ),
            nrow(roles), name, sieve_roles_summary(roles)
          ), "proposal")
        }
        # Constructs and facets carried in the column names, the variable
        # labels or the description rows are in the ledger as a proposal,
        # for review in the grid.
        if (nrow(loaded$structure)) {
          lines <- sieve_structure_text(loaded$structure)
          load_note(sprintf(
            "%s %s. These are proposals: review them in the grid%s.",
            lines[1], paste(lines[-1], collapse = "; "),
            if (is.null(chosen$spec)) ", then confirm the study" else ""
          ), "proposal")
        }
        # A construct the names carry that is also a variable's name is
        # not proposed (the import refuses such a construct); the grid is
        # where it gets a name of its own.
        if (length(loaded$withheld)) {
          load_note(sieve_withheld_text(loaded$withheld), "warning")
        }
        said <- loaded$described
        if (isTRUE(said$found)) {
          notes <- c(
            if (length(said$reversed)) {
              sprintf(
                "The reverse row of %s marked %s, so %s reverse scored on loading (%s).", name,
                sieve_name_some(said$reversed), if (length(said$reversed) == 1L) "it was" else "they were",
                "see the Response bounds cells for the bounds used"
              )
            },
            if (length(said$recorded)) {
              sprintf(
                "The reverse_applied row of %s says the file holds %s reverse scored already, so %s recorded as such and no value was changed.",
                name, sieve_name_some(said$recorded), if (length(said$recorded) == 1L) "it was" else "they were"
              )
            },
            if (length(said$unchecked)) {
              sprintf(
                paste(
                  "%s had no unmarked item in %s construct to compare with, so %s reverse scored as the",
                  "reverse row says without that check; add a reverse_applied row to the file to state it."
                ),
                sieve_name_some(said$unchecked), if (length(said$unchecked) == 1L) "its" else "their",
                if (length(said$unchecked) == 1L) "it was" else "they were"
              )
            },
            if (length(said$refused)) {
              sprintf(
                "The reverse row marked %s, but %s not reverse scored: %s", sieve_name_some(names(said$refused)),
                if (length(said$refused) == 1L) "it was" else "they were", paste(said$refused, collapse = " ")
              )
            },
            if (length(said$bad_reverse)) {
              sprintf(
                "The reverse cell of %s is not yes or no, 1 or 0, TRUE or FALSE, so it was ignored.",
                sieve_name_some(said$bad_reverse)
              )
            },
            if (length(said$ignored)) {
              sprintf(
                "The description rows describe %s, which %s not %s, so that part was not used.",
                sieve_name_some(said$ignored), if (length(said$ignored) == 1L) "is" else "are",
                if (length(said$ignored) == 1L) "an item" else "items"
              )
            }
          )
          if (length(notes)) {
            grid_note(list(
              kind = if (length(said$refused) || length(said$bad_reverse) || length(said$unchecked)) "warning" else "info",
              text = paste(notes, collapse = " ")
            ))
          }
        }
        # An item whose variable label states that its values are reverse
        # scored, in the form SIEVER's processed export writes, was recorded
        # as such with the bounds the label gives; no value was changed. A
        # statement the values contradict (a value outside its bounds) was
        # not recorded, and the item was loaded as administered.
        steps <- loaded$stated$steps
        if (length(steps)) {
          one <- length(steps) == 1L
          load_note(
            sprintf(
              paste(
                "The variable %s in %s %s that %s already %s reverse scored values, so %s",
                "recorded as reverse scored, within the bounds the %s, and no value was",
                "changed. To return %s to the administered direction, set Reverse? to No."
              ),
              if (one) "label" else "labels", name, if (one) "says" else "say",
              sieve_name_some(names(steps)), if (one) "holds" else "hold",
              if (one) "it is" else "they are", if (one) "label gives" else "labels give",
              if (one) "it" else "them"
            ),
            "info"
          )
        }
        for (it in names(loaded$stated$refused)) {
          k <- loaded$keyed[[it]]
          load_note(
            sprintf(
              paste(
                "The variable label of %s in %s says that its values are reverse scored within",
                "%s to %s, but %s So nothing was recorded for it and it was loaded as",
                "administered. Check the label against the values, then set Reverse? or",
                "apply a specification."
              ),
              it, name, sieve_num_text(k$lo), sieve_num_text(k$hi), loaded$stated$refused[[it]]
            ),
            "warning"
          )
        }
        # The choice made in step 1, checked against what arrived.
        if (identical(src, "ready") && is.null(chosen$spec) && !nrow(loaded$structure)) {
          load_note(
            sprintf(paste(
              "%s carries no constructs or facets (no description rows under the header, no",
              "Construct__Facet__item column names, no labeled .sav), so it was loaded as a data",
              "file alone: the constructs and facets go in the grid (step 3)."
            ), name),
            "warning"
          )
        }
        if (identical(src, "pair") && is.null(chosen$spec)) {
          load_note(
            sprintf(paste(
              "No specification was chosen with %s. Choose the data file and the specification",
              "together (step 1), or apply the specification to the loaded data (step 3)."
            ), name),
            "warning"
          )
        }
        if (!is.null(chosen$spec)) {
          # The specification that arrived with the data is restored now,
          # roles and types included, and confirms the study when it types
          # every item.
          rebuild(rebuild() + 1L)
          applied_now <- restore_spec(chosen$spec$path, chosen$spec$name, from_load = TRUE, with_data = TRUE)
          spec_said <- sprintf(
            if (is.null(applied_now)) "; the specification %s was not applied" else " with the specification %s",
            chosen$spec$name
          )
          return(invisible(NULL))
        }
      }
      rebuild(rebuild() + 1L)
      # House rule: auto-detection is confirmed silently only when EVERY item
      # is continuous (nothing methodological is at stake in that case, since
      # ML with FIML on a Pearson matrix is the unambiguous default). Any ordered or
      # binary item leaves the gate up so the analyst owns the polychoric/
      # tetrachoric + WLSMV decision. This holds for a bundled example, whose
      # items are declared. An uploaded file is never confirmed silently:
      # which of its columns are items is a proposal the researcher reviews.
      if (bundled && !is.null(state$meta) && all(state$meta$type == "continuous")) {
        do_confirm(auto = TRUE)
      }
    }
    # The study as it was loaded (or opened): what a second Load would
    # discard is said against it. Read after the load's own flush, when the
    # ledger of the new dataset is in place.
    loaded_at <- reactiveVal(NULL)
    load_selection <- reactiveVal(NULL)
    # The specification a Load applied to the loaded data: Load again with
    # it chosen would apply it once more, reverting what was done since.
    spec_applied <- reactiveVal(NULL)
    observeEvent(state$dataset_id,
      {
        m <- isolate(state$meta)
        sel <- isolate(load_selection())
        own <- !is.null(sel) && identical(sel$id, isolate(state$dataset_id))
        spec_applied(NULL)
        loaded_at(if (is.null(m)) NULL else list(
          meta = m, roles = isolate(state$roles), rows = NROW(isolate(state$items)),
          data_key = sieve_data_key(isolate(state$items)),
          # A study opened as a project, not read by this Load.
          src = if (own) sel$src else "project",
          files = if (own) sel$files else character(0)
        ))
      },
      ignoreNULL = FALSE
    )
    # A Load with a study in the session asks before it replaces it, and
    # names what would go: the grid's changes since the load, every fit and
    # result, and the session script. Nothing changes until the dialog's
    # Load and replace.
    ask_replace <- function() {
      at <- loaded_at()
      discards <- if (!is.null(at)) {
        sieve_load_discards(at, list(
          meta = isolate(state$meta), roles = isolate(state$roles), rows = NROW(isolate(state$items)),
          data_key = sieve_data_key(isolate(state$items))
        ))
      } else {
        character(0)
      }
      again <- same_selection()
      src <- input$source %||% "hs9"
      source_text <- if (!is_file_source(src)) {
        if (identical(src, "hs9")) "the bundled nine-test example" else "the full bundled example"
      } else if (NROW(input$file)) {
        paste(as.character(input$file$name), collapse = " and ")
      } else {
        "the chosen file"
      }
      shiny::showModal(modalDialog(
        title = "Replace the loaded study?",
        p(sprintf(
          if (again) "Load reads %s afresh and starts a new study in place of %s:" else "Load reads %s and starts a new study from it in place of %s:",
          source_text, isolate(state$dataset_name) %||% "the loaded study"
        )),
        tags$ul(
          tags$li(if (again) "the item grid returns to the file as loaded;" else "its items replace the item grid;"),
          if (length(discards)) {
            tags$li(sprintf("what was done since the load is discarded: %s;", paste(discards, collapse = "; ")))
          },
          if (identical(isolate(basis_in_force()), "empirical")) {
            tags$li("the construct basis returns to theoretical and the Optimizer tab is hidden (apply empirical again after the load);")
          },
          tags$li("every fitted model and result on the other tabs is cleared, and the session script starts over.")
        ),
        p(
          class = "small text-muted mb-0",
          "To keep this work, cancel and save the project first. A change in the grid (an exclusion, a ",
          "type, a construct) takes effect as it is made and needs no Load."
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("load_replace"), "Load and replace", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    }
    load_pressed <- function() {
      if (identical(isolate(load_effect()), "replace")) ask_replace() else do_load()
    }
    observeEvent(input$load, load_pressed())
    observeEvent(input$load_main, load_pressed())
    observeEvent(input$load_replace, {
      removeModal()
      do_load()
    })

    # ---- Step 3: the roles, the types, the constructs ----------------------

    output$step3_why <- renderUI({
      keys$dataset
      if (is.null(isolate(state$meta))) {
        return(p(class = "small text-warning mb-1", "Load the data first (step 2)."))
      }
      p(
        class = "small text-muted mb-1",
        "The item grid and the Variable roles panel on the right are live: change a type, a construct, ",
        "a facet, a name or a role in its cell, or set roles by name in the roles editor."
      )
    })
    observe({
      keys$dataset
      updateActionButton(session,
        "clear_constructs",
        disabled = is.null(isolate(state$meta)) || !sieve_workflow_policy(state)$optimization
      )
    })

    observeEvent(input$clear_constructs, {
      req(state$meta)
      if (!sieve_workflow_policy(state)$optimization) {
        grid_note(list(
          kind = "error",
          text = "Theoretical assignments are retained. Deliberately change to empirical before clearing them."
        ))
        return(invisible(NULL))
      }
      cleared <- state$meta
      cleared$facet <- NA_character_
      patch <- rbind(
        sieve_metadata_patch(state$meta, state$meta$item, "facet", NA_character_),
        sieve_metadata_patch(cleared, cleared$item, "construct", "Unassigned"))
      sieve_apply_metadata_patch(state, patch, "Clear construct and facet assignments")
      state$confirmed <- FALSE

      rebuild(rebuild() + 1L)
      grid_note(list(kind = "info", text = "Constructs and facets cleared to Unassigned."))
    })

    observeEvent(input$palette, {
      state$palette <- input$palette
      rebuild(rebuild() + 1L)
    })
    # The construct basis is changed deliberately, by Apply, and the change
    # shows elsewhere (the Optimizer tab, Clear constructs, the next step the
    # status line names), so Apply is enabled only when the selection differs
    # from the basis in force, the note says before the press what a change
    # does, and a notification says after it what changed (Apply on the
    # basis already in force would otherwise seem to do nothing).
    basis_in_force <- reactive(sieve_workflow_policy(state)$basis)
    basis_name <- function(b) {
      if (identical(b, "empirical")) "Empirical (provisional assignments)" else "Theoretical (researcher-specified assignments)"
    }
    basis_effect <- function(b) {
      if (identical(b, "empirical")) {
        paste(
          "the Optimizer tab is shown, Clear constructs to Unassigned is enabled, and the status",
          "line points to Screening, Dimensionality and EFA"
        )
      } else {
        paste(
          "the Optimizer tab is hidden (candidates it searched are not revived if the basis",
          "returns to empirical), Clear constructs to Unassigned is disabled, and the status line",
          "points to the CFA (to the Construct column first while no included item has a construct)"
        )
      }
    }
    observeEvent(input$apply_basis, {
      req(input$basis, study_loaded())
      from <- isolate(basis_in_force())
      if (isTRUE(sieve_set_construct_basis(state, input$basis))) {
        sieve_notify(
          sprintf(
            "Construct basis changed from %s to %s: %s. Nothing assigned, fitted or decided was changed, and the change is in the session script.",
            tolower(basis_name(from)), tolower(basis_name(input$basis)), basis_effect(input$basis)
          ),
          type = "message", duration = 12
        )
      }
    })
    # The selector follows the basis in force when that changes (Apply, an
    # opened project); another edit does not reset it, so a selection not
    # yet applied stays selected.
    observeEvent(basis_in_force(), shiny::updateRadioButtons(session, "basis", selected = basis_in_force()))
    observe({
      updateActionButton(session, "apply_basis",
        disabled = !study_loaded() || identical(input$basis %||% basis_in_force(), basis_in_force())
      )
    })
    observe({
      updateActionButton(session, "clear_constructs",
        disabled = is.null(state$meta) || !identical(basis_in_force(), "empirical")
      )
    })
    output$basis_policy <- renderUI({
      now <- basis_in_force()
      chosen <- input$basis %||% now
      tagList(
        p(class = "small mb-1", strong("In force:"), paste0(basis_name(now), ". ", sieve_workflow_policy(now)$message)),
        if (!study_loaded()) {
          # A load starts every study from the theoretical basis, so the
          # basis is set for the loaded study.
          p(
            class = "small text-muted mb-1",
            "Load the data first (step 2): the basis is set for the loaded study, and a load starts it from theoretical."
          )
        } else if (!identical(chosen, now)) {
          p(
            class = "small text-warning-emphasis mb-1",
            sprintf("Selected, not yet applied: %s. Apply construct basis changes to it: %s.", basis_name(chosen), basis_effect(chosen))
          )
        } else {
          p(
            class = "small text-muted mb-1",
            "Apply construct basis is enabled once the selection differs from the basis in force."
          )
        }
      )
    })

    # The constructs to analyze: whole constructs are included or excluded
    # in one ledger write (sieve_apply_constructs() in fct_prepare.R), so a
    # large study can be worked one construct at a time.
    output$construct_picker <- renderUI({
      keys$pool
      m <- isolate(state$meta)
      if (is.null(m)) {
        return(NULL)
      }
      tbl <- sieve_construct_table(m)
      if (nrow(tbl) < 2L) {
        return(p(
          class = "small text-muted mt-2 mb-1",
          sprintf(
            "One construct (%s), so every included item is analyzed together; name constructs in the grid to work one at a time.",
            tbl$construct[1]
          )
        ))
      }
      labels <- sprintf(
        "%s (%d item%s%s)", tbl$construct, tbl$n, ifelse(tbl$n == 1L, "", "s"),
        ifelse(tbl$n_excluded > 0L, sprintf(", %d excluded", tbl$n_excluded), "")
      )
      tagList(
        strong(class = "d-block mt-2", "Constructs to analyze"),
        helpText(
          "Tick the constructs whose items take part in the analyses; the items of the others are ",
          "excluded (kept in the data, never deleted). One ledger change, recorded in the script."
        ),
        checkboxGroupInput(ns("analyze_constructs"), NULL,
          choices = stats::setNames(tbl$construct, labels),
          selected = tbl$construct[tbl$n_active > 0L]
        ),
        actionButton(ns("apply_constructs"), "Apply", class = "btn-outline-primary btn-sm")
      )
    })
    observeEvent(input$apply_constructs, {
      req(state$meta)
      out <- sieve_apply_constructs(state, isolate(input$analyze_constructs))
      if (!isTRUE(out$ok)) {
        grid_note(list(kind = "error", text = out$problem))
        return(invisible(NULL))
      }
      rebuild(rebuild() + 1L)
      grid_note(list(kind = "info", text = sprintf(
        "Analyzing %d of %d constructs: %s (%d of %d items included%s).",
        length(out$include), length(out$include) + length(out$exclude), paste(out$include, collapse = ", "),
        out$n_active, out$n,
        if (length(out$changed)) sprintf(
          "; %d item%s changed status", length(out$changed),
          if (length(out$changed) == 1L) "" else "s"
        ) else "; nothing changed"
      )))
    })

    # ---- The item grid --------------------------------------------------------

    output$type_grid <- renderDT(
      {
        rebuild()
        state$palette
        m <- isolate(state$meta)
        it <- isolate(state$items)
        req(m, it)
        pal <- sieve_palette_colors(isolate(state$palette) %||% "nd", 1L)[1]
        # The items whose observed range differs from the study's common range
        # carry the warning in their Response bounds cell (see the note above
        # the grid); the file's value labels give the hint of an empty one.
        range_check <- sieve_range_check(m)
        label_bounds <- sieve_label_bounds_of(isolate(state$raw), m$item)
        type_opts <- c("continuous", "ordered", "binary")
        cons_opts <- sort(unique(c(m$construct, "Unassigned")))
        facet_opts <- sort(unique(m$facet[!is.na(m$facet) & nzchar(m$facet)]))
        # Included items come first; excluded items sink below the "Items
        # excluded" divider (a DT RowGroup over this hidden group label,
        # ordered so the included group sorts first: "Included" sorts before
        # "Items").
        dropped_flag <- m$status != "active"
        grp <- ifelse(dropped_flag, "Items excluded from every analysis (kept in the data; set Use to Included to include one again)",
          "Included items"
        )
        ord <- order(dropped_flag, m$construct, m$item)
        m <- m[ord, , drop = FALSE]
        grp <- grp[ord]
        keyed <- sieve_meta_keying(m)
        holds <- sieve_holds_reversal_bounds(
          keyed, keyed$item, it, isolate(state$raw), isolate(state$transforms) %||% list(), isolate(state$dataset_id)
        )
        eid <- ns("cell_edit")
        df <- data.frame(
          grp = grp, # hidden: drives the RowGroup
          Variable = m$item,
          ItemName = vapply(seq_len(nrow(m)), function(i) {
            sieve_cell_text(eid, m$item[i], "label", m$label[i])
          }, character(1)),
          Question = vapply(seq_len(nrow(m)), function(i) {
            sieve_wording_cell(if ("wording" %in% names(m)) m$wording[i] else NA_character_)
          }, character(1)),
          Use = vapply(seq_len(nrow(m)), function(i) sieve_use_cell(eid, m, i), character(1)),
          Type = vapply(seq_len(nrow(m)), function(i) {
            paste0(
              sieve_cell_select(eid, m$item[i], "type", m$type[i], type_opts),
              sieve_type_cell_note(m, i, it)
            )
          }, character(1)),
          Levels = m$n_levels,
          Missing = m$n_missing,
          Mean = m$mean,
          SD = m$sd,
          Min = m$min,
          Max = m$max,
          Distribution = vapply(m$item, function(x) sieve_spark_svg(it[[x]], fill = pal), character(1)),
          Reverse = vapply(seq_len(nrow(m)), function(i) sieve_reverse_cell(eid, m, i), character(1)),
          Bounds = vapply(seq_len(nrow(m)), function(i) {
            sieve_bounds_cell(eid, keyed, i, labels = label_bounds[[m$item[i]]], check = range_check, held = holds[i])
          }, character(1)),
          Construct = vapply(seq_len(nrow(m)), function(i) {
            sieve_cell_combo(eid, m$item[i], "construct", m$construct[i], cons_opts)
          }, character(1)),
          Facet = vapply(seq_len(nrow(m)), function(i) {
            sieve_cell_combo(
              eid, m$item[i], "facet",
              if (is.na(m$facet[i])) "" else m$facet[i], facet_opts
            )
          }, character(1)),
          stringsAsFactors = FALSE
        )
        stopifnot(identical(names(df), sieve_grid_columns()))
        drawn$keying <- keying_of(m)
        drawn$status <- stats::setNames(as.character(m$status), m$item)
        drawn$range <- range_check
        no_html <- which(names(df) %in% c("grp", "Variable", "Levels", "Missing", "Mean", "SD", "Min", "Max"))
        # The distribution sparkline and the in-cell widget columns hold HTML, not
        # sortable data, so turn off their ordering (no misleading sort arrow).
        no_sort <- which(names(df) %in% c(
          "ItemName", "Question", "Use", "Type", "Distribution", "Reverse", "Bounds",
          "Construct", "Facet"
        )) - 1L
        grp_idx <- which(names(df) == "grp") - 1L # 0-based, for RowGroup + hide
        # A DT build without the RowGroup extension still gets the grid: ordered
        # the same way, the group shown as a column, and a caption saying why.
        rg <- sieve_dt_row_group(grp_idx)
        sieve_dt(
          df,
          rownames = FALSE, selection = "none", escape = no_html,
          extensions = rg$extensions, caption = rg$caption,
          options = c(list(
            pageLength = 100, dom = "tp", autoWidth = FALSE, scrollX = TRUE, stateSave = TRUE,
            stateSaveCallback = sieve_dt_state_save(), stateLoadCallback = sieve_dt_state_load(),
            sieve = list(id = "ingest-items", keys = names(df)),
            # Order by the group column first so "Included items" precedes the excluded.
            order = list(list(grp_idx, "asc")),
            columnDefs = list(
              list(visible = !rg$available, targets = grp_idx),
              list(orderable = FALSE, targets = no_sort),
              # The Response bounds cell holds the item's range warning, a
              # sentence; a wider column keeps it to a few lines, so the rows
              # stay short enough for five of them to show in any font.
              list(className = "sieve-reverse-cell", targets = which(names(df) == "Bounds") - 1L)
            )
          ), rg$options),
          colnames = c(
            "Group", "Variable Name", "Item Name", "Question", "Use", "Type", "Levels", "Missing",
            "Mean", "SD", "Min", "Max", "Distribution", "Reverse?", "Response bounds", "Construct", "Facet"
          )
        ) |>
          formatRound(c("Mean", "SD", "Min", "Max"), 2) |>
          # Grey the dropped rows so the in-use/unused split reads at a glance.
          formatStyle("grp",
            target = "row",
            color = styleEqual("Included items", "inherit", default = "#9aa0a6")
          )
      },
      server = FALSE
    )

    # Seed the shared construct and facet datalists with every value in the
    # ledger, so the in-cell dropdowns are complete even when the grid is
    # paginated. sieve.js merges these into the datalists alongside the live
    # cell values.
    output$dl_seed <- renderUI({
      keys$pool
      m <- isolate(state$meta)
      req(m)
      cons <- sort(unique(c(m$construct, "Unassigned")))
      facets <- sort(unique(m$facet[!is.na(m$facet) & nzchar(m$facet)]))
      tagList(
        tags$span(
          class = "sieve-dl-seed", style = "display:none;",
          `data-dl` = "sieve_dl_construct", `data-vals` = paste(cons, collapse = "|")
        ),
        tags$span(
          class = "sieve-dl-seed", style = "display:none;",
          `data-dl` = "sieve_dl_facet", `data-vals` = paste(facets, collapse = "|")
        )
      )
    })

    shiny::observeEvent(state$meta,
      {
        m <- state$meta
        shiny::updateSelectInput(session,
          "metadata_ids",
          choices = if (is.null(m)) character(0) else stats::setNames(
            m$item,
            paste(m$item,
              m$label,
              sep = " | "
            )
          ),
          selected = intersect(shiny::isolate(input$metadata_ids) %||% character(0), m$item)
        )
      },
      ignoreInit = FALSE
    )
    sieve_metadata_controls_server(
      input,
      output,
      session,
      state,
      function() isolate(input$metadata_ids %||% character(0))
    )
    output$parse_preview <- renderUI({
      f <- input$file
      if (is.null(f)) return(p(class = "small", "Choose a CSV/TXT data file to preview its parsing."))
      chosen <- sieve_sort_uploads(as.character(f$name), as.character(f$datapath))
      if (is.null(chosen$data) || !tolower(tools::file_ext(chosen$data$name)) %in% c(
        "csv",
        "txt"
      )) return(p(
        class = "small",
        "Preview applies to CSV/TXT data files."
      ))
      sep <- if (isTRUE(input$parse_override)) {
        if (identical(
          input$parse_sep,
          "tab"
        )) "\t" else input$parse_sep %||% ","
      } else if (tolower(tools::file_ext(chosen$data$name)) == "txt") "\t" else ","
      read <- tryCatch(
        sieve_read_delimited(chosen$data$path, chosen$data$name,
          sep = sep,
          dec = if (isTRUE(input$parse_override)) input$parse_dec %||% "." else ".",
          encoding = input$parse_encoding %||% "UTF-8",
          description = isTRUE(input$read_rows %||% TRUE),
          nrows = 14L
        ),
        error = function(e) list(
          ok = FALSE,
          error = conditionMessage(e)
        )
      )
      if (!isTRUE(read$ok)) return(div(
        class = "alert alert-warning small",
        paste(
          "Parsing preview refused:",
          read$error
        )
      ))
      tagList(
        p(class = "small", sprintf("%d columns; preview only, project unchanged.", ncol(read$value))),
        sieve_plain_table_ui(utils::head(read$value, 10L))
      )
    })
    output$grid_note <- renderUI({
      note <- grid_note()
      if (is.null(note)) {
        return(NULL)
      }
      cls <- switch(note$kind,
        error = "text-danger",
        warning = "text-warning-emphasis",
        "text-muted"
      )
      p(class = paste("small mb-1", cls), note$text)
    })

    # The same item twice: two
    # included items that correlate -1 are almost certainly an item and its
    # reversed copy, both in the file, and only one belongs in the analysis:
    # the one keyed with its construct (sieve_item_copies(), utils_detect.R).
    # Read from the pool, the keying and the dataset, as the range check.
    copies <- reactive({
      keys$pool
      keys$keying
      keys$dataset
      # Respondents removed or restored change the rows the pair is read on.
      state$removed_cases
      sieve_item_copies(isolate(state$items), isolate(state$meta))
    })
    # The pairs the note about reverse-coded copies below already proposes
    # are left to it, so that one pair is never the subject of two notes.
    copies_shown <- reactive({
      cp <- copies()
      op <- originals()
      op <- op[op$proposed, , drop = FALSE]
      if (!nrow(cp) || !nrow(op)) {
        return(cp)
      }
      covered <- paste(pmin(op$original, op$copy), pmax(op$original, op$copy), sep = "\r")
      key <- paste(pmin(cp$a, cp$b), pmax(cp$a, cp$b), sep = "\r")
      cp[!key %in% covered, , drop = FALSE]
    })
    output$copies_note <- renderUI({
      cp <- copies_shown()
      if (!nrow(cp)) {
        return(NULL)
      }
      m <- isolate(state$meta)
      construct_of <- stats::setNames(as.character(m$construct), as.character(m$item))
      reversed_here <- stats::setNames(m$reverse %in% TRUE, as.character(m$item))
      pair_line <- function(i) {
        x <- cp[i, ]
        # Exactly one of the two reverse scored here: the file held them the
        # other way round (the same item twice, or an item and its reversed
        # copy).
        flipped <- xor(reversed_here[[x$a]], reversed_here[[x$b]])
        what <- if (x$r < 0 && !flipped) {
          sprintf(
            "%s and %s correlate %.2f (n = %d)%s: one is almost certainly the other reversed, both in the file.",
            x$a, x$b, x$r, x$n,
            if (isTRUE(x$constant_sum)) sprintf(", and their sum is %s in every case", sieve_num_text(x$sum)) else ""
          )
        } else if (x$r < 0) {
          sprintf(
            "%s and %s correlate %.2f (n = %d): the same item twice in the file, one of them reverse scored here.",
            x$a, x$b, x$r, x$n
          )
        } else if (flipped) {
          sprintf(
            "%s and %s correlate %.2f (n = %d): an item and its reversed copy in the file, one of them reverse scored here, so now the same item twice.",
            x$a, x$b, x$r, x$n
          )
        } else {
          sprintf("%s and %s correlate %.2f (n = %d): the same item twice in the file.", x$a, x$b, x$r, x$n)
        }
        advice <- if (is.na(x$keep)) {
          if (x$r < 0 && is.finite(x$r_a) && is.finite(x$r_b)) {
            # Both read against their constructs, and the direction does
            # not decide (they are assigned to different constructs).
            sprintf(
              paste(
                "%s correlates %.2f with the mean of the other included items of %s, and %s %.2f with",
                "those of %s, so the direction does not tell which to keep: keep the one in the",
                "construct the item belongs to and exclude the other."
              ),
              x$a, x$r_a, construct_of[[x$a]], x$b, x$r_b, construct_of[[x$b]]
            )
          } else if (x$r < 0) {
            paste(
              "Keep the one keyed with its construct and exclude the other; with the construct",
              "assigned (and at least two other included items in it), this note names it."
            )
          } else {
            "Keep one and exclude the other."
          }
        } else if (x$r < 0) {
          r_keep <- if (identical(x$keep, x$a)) x$r_a else x$r_b
          r_drop <- if (identical(x$keep, x$a)) x$r_b else x$r_a
          sprintf(
            "Keep %s, the one keyed with %s (r = %.2f with the mean of its other included items; %s, %.2f), and exclude %s.",
            x$keep, construct_of[[x$keep]], r_keep, x$drop, r_drop, x$drop
          )
        } else {
          sprintf("Keep %s and exclude %s.", x$keep, x$drop)
        }
        tags$li(paste(what, advice))
      }
      drops <- unique(stats::na.omit(cp$drop))
      div(
        class = "alert alert-warning py-2 px-3 small mb-2",
        strong("The same item twice."),
        " Only one item of each pair belongs in the analysis, which otherwise counts it twice",
        " (two distinct items do not correlate this strongly):",
        tags$ul(class = "mb-1", lapply(seq_len(nrow(cp)), pair_line)),
        if (length(drops)) {
          actionButton(ns("copies_exclude"), sprintf("Exclude %s", sieve_name_some(drops, 4L)),
            class = "btn-outline-secondary btn-sm"
          )
        }
      )
    })
    observeEvent(input$copies_exclude, {
      cp <- isolate(copies_shown())
      cp <- cp[!is.na(cp$drop), , drop = FALSE]
      for (i in seq_len(nrow(cp))) {
        sieve_coverage_notify(sieve_set_item_status(state, cp$drop[i], "parked",
          reason = sprintf("the same item as %s (r = %.2f)", cp$keep[i], cp$r[i])
        ))
      }
      if (nrow(cp)) rebuild(rebuild() + 1L)
    })

    # ---- Items beside their reverse-coded copies ---------------------------
    # A file that holds an item as administered and its reverse-coded copy
    # (q5 and q5_r) carries one response twice; the copy, keyed with its
    # construct, is the one the analysis keeps (sieve_reversed_originals()).
    # Read from the pool, the keying and the dataset, as the copies above.
    originals <- reactive({
      keys$pool
      keys$keying
      keys$dataset
      state$removed_cases
      sieve_reversed_originals(isolate(state$items), isolate(state$meta))
    })
    output$originals_note <- renderUI({
      op <- originals()
      if (!nrow(op)) {
        return(NULL)
      }
      m <- isolate(state$meta)
      included <- as.character(m$item[m$status == "active"])
      go <- op[op$proposed, , drop = FALSE]
      # A pair with either member already excluded needs nothing, and a pair
      # whose original is the keyed one is the note on the same item twice's
      # when that note lists it.
      cp <- copies()
      listed <- paste(pmin(cp$a, cp$b), pmax(cp$a, cp$b), sep = "\r")
      held <- op[!op$proposed & op$original %in% included & op$copy %in% included &
        !(op$keyed %in% "original" & paste(pmin(op$original, op$copy), pmax(op$original, op$copy), sep = "\r") %in% listed),
      , drop = FALSE]
      if (!nrow(go) && !nrow(held)) {
        return(NULL)
      }
      r_text <- function(x) {
        sprintf(
          "r = %.2f, n = %d%s", x$r, x$n,
          if (isTRUE(x$flipped)) ", one of the two reverse scored here" else ""
        )
      }
      keyed_text <- function(x) {
        if (identical(x$keyed, "copy")) {
          known <- is.finite(c(x$r_copy, x$r_original))
          sprintf(
            "%s is keyed with %s (correlation with the mean of its other included items: %s)",
            x$copy, x$construct,
            paste(sprintf("%s, %.2f", c(x$copy, x$original)[known], c(x$r_copy, x$r_original)[known]), collapse = "; ")
          )
        } else {
          "no construct with two other included items to read the direction from, so the name decides"
        }
      }
      by_name <- sum(is.na(go$keyed))
      originals_go <- unique(go$original)
      div(
        class = "alert alert-info py-2 px-3 small mb-2",
        if (nrow(go)) {
          tagList(
            strong("Items saved beside their reverse-coded copies."),
            sprintf(
              paste(
                " %d item%s %s in the file twice, as administered and reverse coded (named like %s and %s,",
                "with values that run opposite). The analysis needs one of each pair, the one keyed with",
                "its construct, and here that is the reverse-coded copy%s; the original counts the same",
                "response a second time."
              ),
              length(originals_go), if (length(originals_go) == 1L) "" else "s",
              if (length(originals_go) == 1L) "is" else "are", go$original[1L], go$copy[1L],
              if (by_name == nrow(go)) {
                " (no construct is assigned to read the direction from, so the names decide: a file reverse codes the items worded against their construct)"
              } else if (by_name) {
                sprintf(" (read from the construct's other included items, or from the names for %d pair%s whose construct cannot tell)", by_name, if (by_name == 1L) "" else "s")
              } else {
                " (read from the construct's other included items)"
              }
            ),
            div(
              class = "mt-1",
              actionButton(ns("originals_exclude"),
                sprintf(
                  "Exclude the %d original%s, keep the reverse-coded cop%s",
                  length(originals_go), if (length(originals_go) == 1L) "" else "s",
                  if (length(originals_go) == 1L) "y" else "ies"
                ),
                class = "btn-outline-primary btn-sm"
              )
            ),
            tags$details(
              class = "mt-1",
              tags$summary(sprintf("The %d pair%s", nrow(go), if (nrow(go) == 1L) "" else "s")),
              tags$ul(class = "mb-0", lapply(seq_len(nrow(go)), function(i) {
                tags$li(sprintf("%s, the copy %s (%s; %s)", go$original[i], go$copy[i], r_text(go[i, ]), keyed_text(go[i, ])))
              }))
            )
          )
        },
        if (nrow(held)) {
          tags$details(
            class = "mt-1",
            tags$summary(sprintf(
              "%d pair%s named like an item and its reverse-coded copy, not proposed",
              nrow(held), if (nrow(held) == 1L) "" else "s"
            )),
            tags$ul(class = "mb-0", lapply(seq_len(nrow(held)), function(i) {
              x <- held[i, ]
              tags$li(sprintf(
                "%s and %s: %s%s.", x$original, x$copy, x$reason,
                if (is.finite(x$r)) sprintf(" (%s)", r_text(x)) else ""
              ))
            }))
          )
        }
      )
    })
    observeEvent(input$originals_exclude, {
      op <- isolate(originals())
      op <- op[op$proposed & !duplicated(op$original), , drop = FALSE]
      if (!nrow(op)) {
        return()
      }
      out <- sieve_set_item_status(state, op$original, "parked",
        reasons = sprintf("the original of the reverse-coded copy %s", op$copy),
        label = sprintf(
          "Exclude %d item%s saved beside a reverse-coded copy: %s",
          nrow(op), if (nrow(op) == 1L) "" else "s", paste(op$original, collapse = ", ")
        )
      )
      sieve_coverage_notify(out)
      if (length(out$changed)) rebuild(rebuild() + 1L)
    })

    # ---- Include or exclude items from a pasted list -----------------------
    # The entries are matched by sieve_match_item_list(); the change is one
    # step of sieve_set_item_status(), so it is in the reproducible code, and
    # the Use each changed item had before is kept for one step back.
    list_last <- reactiveVal(NULL)
    list_outcome <- reactiveVal(NULL)
    # A list applied to one study is never undone in another: a Load or an
    # opened project starts afresh.
    observeEvent(state$dataset_id, {
      list_last(NULL)
      list_outcome(NULL)
    }, ignoreInit = TRUE)
    list_apply <- function(mode) {
      m <- isolate(state$meta)
      if (is.null(m) || !nrow(m)) {
        return(invisible(NULL))
      }
      found <- sieve_match_item_list(sieve_item_list_entries(input$list_text), m)
      if (!length(found$items)) {
        list_outcome(list(found = found, mode = mode, changed = character(0)))
        return(invisible(NULL))
      }
      before <- stats::setNames(as.character(m$status), m$item)
      before_reason <- stats::setNames(
        if ("exclude_reason" %in% names(m)) as.character(m$exclude_reason) else rep(NA_character_, nrow(m)),
        m$item
      )
      targets <- switch(mode,
        exclude = stats::setNames(rep("parked", length(found$items)), found$items),
        include = stats::setNames(rep("active", length(found$items)), found$items),
        only = stats::setNames(ifelse(m$item %in% found$items, "active", "parked"), m$item)
      )
      reasons <- switch(mode,
        exclude = rep("excluded by a pasted list", length(targets)),
        include = rep("", length(targets)),
        only = ifelse(targets == "parked", "not in the pasted list of items to include", "")
      )
      n_listed <- length(found$items)
      label <- switch(mode,
        exclude = sprintf("Exclude the %d item%s of a pasted list", n_listed, if (n_listed == 1L) "" else "s"),
        include = sprintf("Include the %d item%s of a pasted list", n_listed, if (n_listed == 1L) "" else "s"),
        only = sprintf(
          "Include only the %d item%s of a pasted list, and exclude every other item",
          n_listed, if (n_listed == 1L) "" else "s"
        )
      )
      out <- sieve_set_item_status(state, names(targets), unname(targets), reasons = reasons, label = label)
      changed <- out$changed
      if (length(changed)) {
        list_last(list(
          items = changed, status = unname(before[changed]), reasons = unname(before_reason[changed]),
          dataset = isolate(state$dataset_id), n_actions = length(isolate(state$item_actions) %||% list())
        ))
      }
      sieve_coverage_notify(out)
      list_outcome(list(found = found, mode = mode, changed = changed))
      if (length(changed)) rebuild(rebuild() + 1L)
      invisible(out)
    }
    observeEvent(input$list_exclude, list_apply("exclude"))
    observeEvent(input$list_include, list_apply("include"))
    observeEvent(input$list_only, list_apply("only"))
    # Undo puts back the Use each item had before the last list, except for
    # an item changed again since (read from the record of item actions), so
    # a later decision is never reverted unseen.
    observeEvent(input$list_undo, {
      last <- list_last()
      if (is.null(last) || !identical(last$dataset, isolate(state$dataset_id))) {
        list_last(NULL)
        list_outcome(list(undo = NA_integer_))
        return()
      }
      actions <- isolate(state$item_actions) %||% list()
      since <- if (length(actions) > last$n_actions) actions[-seq_len(last$n_actions)] else list()
      later <- unique(unlist(lapply(since, function(a) a$items), use.names = FALSE))
      keep <- !last$items %in% later
      out <- if (any(keep)) {
        sieve_set_item_status(state, last$items[keep], last$status[keep],
          reasons = ifelse(is.na(last$reasons[keep]), "", last$reasons[keep]),
          label = sprintf(
            "Put back the Use of the %d item%s the last pasted list changed",
            sum(keep), if (sum(keep) == 1L) "" else "s"
          )
        )
      } else {
        list(changed = character(0))
      }
      list_last(NULL)
      if (any(keep)) sieve_coverage_notify(out)
      list_outcome(list(undo = length(out$changed), kept = last$items[!keep]))
      if (length(out$changed)) rebuild(rebuild() + 1L)
    })
    output$list_result <- renderUI({
      o <- list_outcome()
      if (is.null(o)) {
        return(NULL)
      }
      if (!is.null(o$undo)) {
        return(div(
          class = "small text-muted mt-1 mb-0",
          div(if (is.na(o$undo)) {
            "There is no pasted list to undo."
          } else {
            sprintf("The Use of %d item%s is back as it was.", o$undo, if (o$undo == 1L) "" else "s")
          }),
          if (length(o$kept)) {
            div(sprintf(
              "Changed again after the list, so left as %s: %s.",
              if (length(o$kept) == 1L) "it is" else "they are", sieve_name_some(o$kept, 20L)
            ))
          }
        ))
      }
      f <- o$found
      n_found <- length(f$items)
      done <- switch(o$mode,
        exclude = "excluded",
        include = "included",
        only = "included, and every other item excluded"
      )
      lines <- list(if (length(o$changed)) {
        sprintf(
          "%d item%s matched; %d changed (%s).",
          n_found, if (n_found == 1L) "" else "s", length(o$changed), done
        )
      } else {
        sprintf(
          "%d item%s matched, already %s; nothing changed.",
          n_found, if (n_found == 1L) "" else "s",
          switch(o$mode, exclude = "excluded", include = "included", only = "as the list asks")
        )
      })
      if (!n_found) {
        lines <- list("No entry of the list names an item, so nothing changed.")
      }
      if (length(f$unmatched)) {
        lines <- c(lines, list(sprintf("Not found: %s.", sieve_name_some(f$unmatched, 20L))))
      }
      if (length(f$ambiguous)) {
        lines <- c(lines, list(sprintf(
          "Names more than one item, so left alone: %s.", sieve_name_some(f$ambiguous, 20L)
        )))
      }
      flag <- length(f$unmatched) || length(f$ambiguous) || !n_found
      div(
        class = paste("small mt-1", if (flag) "text-warning-emphasis" else "text-muted"),
        lapply(lines, function(x) div(x))
      )
    })

    # The range warning: when no
    # bounds are declared and an item's observed range differs from the
    # range most items share, one sentence names the items, and the bounds
    # of every item can be declared at once. Read from the pool and the
    # keying keys: a declaration moves the keying key, so the note goes.
    range_check <- reactive({
      keys$pool
      keys$keying
      keys$dataset
      sieve_range_check(isolate(state$meta))
    })
    output$bounds_note <- renderUI({
      check <- range_check()
      text <- sieve_range_text(check)
      if (is.null(text)) {
        return(NULL)
      }
      common <- check$common
      # One line above the grid: each
      # item it concerns is marked in its Response bounds cell; the full statement
      # and where to declare bounds open from the line.
      tags$details(
        class = "small mb-2",
        tags$summary(
          class = "text-warning-emphasis",
          strong("Response bounds. "), sieve_range_summary(check)
        ),
        div(
          class = "alert alert-warning py-2 small mt-1 mb-0",
          text,
          if (length(check$declared)) {
            p(
              class = "mb-0 mt-1",
              sprintf("Bounds are already declared for %s and are kept.", sieve_name_some(check$declared))
            )
          },
          p(
            class = "mb-0 mt-1",
            "Declare the bounds of the scale under ", strong("Response bounds and Reverse? for several items at once"),
            " above the grid (every included item, or the items of one construct or facet; the study's ",
            sprintf("common observed range is %s to %s, ", sieve_num_text(common[1]), sieve_num_text(common[2])),
            "which a point no respondent chose would leave narrower than the scale), or in an ",
            "item's own ", strong("Response bounds"), " cell, as 1 to 7."
          )
        )
      )
    })
    # The two bulk typing buttons above the grid (sieve_bulk_type() in
    # fct_prepare.R): a change withdraws the confirmation, as a Type cell
    # does, and the line above the grid says what changed or that nothing did.
    bulk_type <- function(to) {
      if (is.null(isolate(state$meta))) {
        grid_note(list(kind = "warning", text = "Load the data first (step 2)."))
        return(invisible(NULL))
      }
      out <- sieve_bulk_type(state, to)
      if (length(out$changed)) {
        rebuild(isolate(rebuild()) + 1L)
      }
      grid_note(list(kind = "info", text = out$text))
      invisible(out)
    }
    observeEvent(input$types_continuous, bulk_type("continuous"))
    observeEvent(input$types_ordered, bulk_type("ordered"))

    # "Set the type of" (every included item, a construct's or a facet's
    # items) "to" ordered or continuous. The scope's choices follow the
    # ledger.
    observe({
      keys$pool
      m <- isolate(state$meta)
      choices <- c("every included item" = "all")
      if (!is.null(m)) {
        act <- m[m$status == "active", , drop = FALSE]
        cons <- unique(act$construct[!is.na(act$construct) & nzchar(act$construct)])
        facs <- unique(act$facet[!is.na(act$facet) & nzchar(act$facet)])
        if (length(cons)) {
          choices <- c(choices, stats::setNames(paste0("construct:", cons), sprintf("the items of construct %s", cons)))
        }
        if (length(facs)) {
          choices <- c(choices, stats::setNames(paste0("facet:", facs), sprintf("the items of facet %s", facs)))
        }
      }
      # The same scopes for the types, the response bounds and the keying.
      for (id in "types_scope") {
        current <- isolate(input[[id]])
        updateSelectInput(session, id,
          choices = choices,
          selected = if (!is.null(current) && current %in% choices) current else "all"
        )
      }
    })
    # The included items a scope names ("all", "construct:<name>" or
    # "facet:<name>"), and the scope in words.
    items_in_scope <- function(scope, m) {
      act <- m[m$status == "active", , drop = FALSE]
      kind <- sub(":.*$", "", scope %||% "all")
      name <- sub("^[^:]*:", "", scope %||% "all")
      list(
        items = switch(kind,
          construct = act$item[act$construct %in% name],
          facet = act$item[act$facet %in% name],
          act$item
        ),
        what = switch(kind,
          construct = sprintf("the items of construct %s", name),
          facet = sprintf("the items of facet %s", name),
          "every included item"
        )
      )
    }
    set_types <- function(items, to, what) {
      if (is.null(isolate(state$meta))) {
        grid_note(list(kind = "warning", text = "Load the data first (step 2)."))
        return(invisible(NULL))
      }
      out <- sieve_set_types(state, items, to, what)
      if (length(out$changed)) {
        rebuild(isolate(rebuild()) + 1L)
      }
      grid_note(list(kind = "info", text = out$text))
      invisible(out)
    }
    observeEvent(input$types_apply, {
      m <- isolate(state$meta)
      req(m)
      to <- if (identical(input$types_to, "continuous")) "continuous" else "ordered"
      sc <- items_in_scope(input$types_scope, m)
      items <- sc$items
      what <- sc$what
      # Binary items keep their type: a 0/1 item is modeled through its
      # tetrachoric correlations whichever way the others go.
      items <- items[!m$type[match(items, m$item)] %in% c("binary", "constant", "text")]
      set_types(items, to, what)
    })

    # The SPSS note: the items whose SPSS measurement level (read from the
    # .sav dictionary, see fct_spss.R) is Ordinal or Nominal but that are
    # typed continuous here (five or more response options).
    output$spss_note <- renderUI({
      keys$pool
      clash <- sieve_spss_level_conflicts(isolate(state$meta), isolate(state$items))
      n <- nrow(clash)
      if (!n) {
        return(NULL)
      }
      levels <- table(factor(clash$spss_measure, levels = c("Ordinal", "Nominal")))
      said <- paste(
        c(
          if (levels[["Ordinal"]]) sprintf("%d Ordinal", levels[["Ordinal"]]),
          if (levels[["Nominal"]]) sprintf("%d Nominal", levels[["Nominal"]])
        ),
        collapse = " and "
      )
      div(
        class = "alert alert-warning py-2 small mb-2",
        strong(sprintf("SPSS calls %d of the items typed continuous here ordinal or nominal (%s): ", n, said)),
        sieve_name_some(clash$item), ". ",
        "The .sav file's dictionary sets their measurement level so. ",
        sieve_spss_reason_text(clash),
        paste0(
          " To model them as ordered categories (polychoric correlations, WLSMV), ",
          "set them to ordered here, with the Set the type of control above, or in their Type ",
          "cells; the study is then confirmed again in step 4."
        ),
        tags$details(
          class = "mt-1",
          tags$summary(sprintf("The %d item%s", n, if (n == 1L) "" else "s")),
          p(class = "mb-0", paste(sprintf(
            "%s (%s, SPSS %s, %s)", clash$item, clash$label, clash$spss_measure,
            ifelse(is.na(clash$options), "response options not counted", sprintf("%d response options", clash$options))
          ), collapse = "; "))
        ),
        div(
          class = "mt-1",
          actionButton(ns("spss_to_ordered"),
            sprintf("Set these %d item%s to ordered", n, if (n == 1L) "" else "s"),
            class = "btn-outline-primary btn-sm"
          )
        )
      )
    })
    # The marked items held at load (sieve_reverse_held_text()), with the
    # two answers: the file holds them as administered (reverse them now,
    # through the transformation service) or reverse scored already (record
    # the keying, no value changes).
    output$reverse_ask <- renderUI({
      held <- reverse_held()
      keys$dataset
      if (!length(held)) {
        return(NULL)
      }
      n <- length(held)
      div(
        class = "alert alert-warning py-2 small mb-2",
        strong("Reverse keyed items to settle. "), sieve_reverse_held_text(held),
        tags$details(
          class = "mt-1",
          tags$summary(sprintf("The %d item%s and %s item-rest correlation%s", n, if (n == 1L) "" else "s",
            if (n == 1L) "its" else "their", if (n == 1L) "" else "s")),
          p(class = "mb-0", paste(sprintf("%s (r = %.3f)", names(held), held), collapse = "; "))
        ),
        div(
          class = "mt-1 d-flex gap-2 flex-wrap",
          actionButton(ns("reverse_held_go"),
            sprintf("The file holds %s as administered: reverse %s now", if (n == 1L) "it" else "them", if (n == 1L) "it" else "them"),
            class = "btn-outline-primary btn-sm"
          ),
          actionButton(ns("reverse_held_mark"),
            sprintf("The file holds %s reverse scored already: record %s", if (n == 1L) "it" else "them", if (n == 1L) "it" else "them"),
            class = "btn-outline-secondary btn-sm"
          )
        )
      )
    })
    observeEvent(input$reverse_held_go, {
      held <- reverse_held()
      req(length(held))
      out <- sieve_apply_reverse(state, names(held), origin = "ingest")
      reverse_held(NULL)
      rebuild(isolate(rebuild()) + 1L)
      grid_note(list(
        kind = if (length(out$refused)) "warning" else "info",
        text = paste(c(
          if (length(out$applied)) sprintf("Reverse scored now: %s.", sieve_name_some(names(out$applied))),
          if (length(out$refused)) paste(out$refused, collapse = " ")
        ), collapse = " ")
      ))
    })
    observeEvent(input$reverse_held_mark, {
      held <- reverse_held()
      req(length(held))
      out <- sieve_record_already_reversed(state, names(held), origin = "ingest")
      reverse_held(NULL)
      rebuild(isolate(rebuild()) + 1L)
      grid_note(list(
        kind = if (length(out$refused)) "warning" else "info",
        text = paste(c(
          if (length(out$recorded)) {
            sprintf("Recorded as already reverse scored in the file, with no value changed: %s.", sieve_name_some(out$recorded))
          },
          if (length(out$refused)) paste(out$refused, collapse = " ")
        ), collapse = " ")
      ))
    })

    # The note an opened project's repaired scoring policies leave
    # (sieve_project_open()): what was stored,
    # what is in force instead, and where to choose. It goes once every
    # repaired construct has a kept specification again.
    output$score_policy_issues <- renderUI({
      opened <- state$project$score_policy_issues
      if (!length(opened)) {
        return(NULL)
      }
      still <- sieve_score_policy_issues(state$scoring_policies %||% list())
      still <- still[names(still) %in% names(opened)]
      if (!length(still)) {
        return(NULL)
      }
      div(
        class = "alert alert-warning py-2 small mb-2",
        strong("Scoring specifications to review. "),
        paste(sprintf("%s: %s", names(still), still), collapse = " "),
        " Choose and keep each construct's score on the Reliability tab, under Operational scores."
      )
    })

    # The note the five-option rule leaves when a project is opened
    # (sieve_project_open()): the switched items, why, that the
    # confirmation was renewed, and the one click that keeps them ordered.
    output$types_switched <- renderUI({
      opened <- state$project$switched_types
      if (is.null(opened) || !length(opened$items)) {
        return(NULL)
      }
      n <- length(opened$items)
      div(
        class = "alert alert-warning py-2 small mb-2",
        strong("Item types switched on opening. "), opened$text,
        tags$details(
          class = "mt-1",
          tags$summary(sprintf("The %d item%s and %s response options", n, if (n == 1L) "" else "s", if (n == 1L) "its" else "their")),
          p(class = "mb-0", paste(sprintf("%s (%d options)", opened$items, as.integer(opened$options)), collapse = "; "))
        ),
        div(
          class = "mt-1",
          actionButton(ns("keep_ordered"),
            sprintf("Keep %s ordered", if (n == 1L) "it" else sprintf("these %d items", n)),
            class = "btn-outline-primary btn-sm"
          )
        )
      )
    })
    # The note sieve_keep_legacy_mlr() leaves when a saved study whose
    # Follow the item types setting recorded MLR with FIML is opened: what
    # was kept and why, and the two answers (keep it, or take ML with FIML,
    # the default for continuous items). Shown once: either answer clears it.
    output$estimator_kept <- renderUI({
      kept <- state$project$estimator_kept
      if (is.null(kept)) {
        return(NULL)
      }
      div(
        class = "alert alert-info py-2 small mb-2",
        strong("Estimator kept as saved. "), kept$text,
        div(
          class = "mt-1",
          actionButton(ns("legacy_auto_release"), "Use ML with FIML (Follow the item types)",
            class = "btn-outline-primary btn-sm"
          ),
          actionButton(ns("legacy_auto_keep"), "Keep MLR with FIML", class = "btn-outline-secondary btn-sm")
        )
      )
    })
    observeEvent(input$legacy_auto_release, {
      req(!is.null(isolate(state$project$estimator_kept)))
      e <- sieve_legacy_auto_release(state)
      grid_note(list(kind = "info", text = paste0(
        "The study's estimator now follows the item types",
        if (!is.null(e)) sprintf(" (%s)", e$label) else "",
        "; each model page fits under it when its fit is next requested."
      )))
    })
    observeEvent(input$legacy_auto_keep, {
      project <- isolate(state$project) %||% list()
      req(!is.null(project$estimator_kept))
      project$estimator_kept <- NULL
      state$project <- project
    })

    observeEvent(input$keep_ordered, {
      req(length(isolate(state$project$switched_types$items)))
      out <- sieve_keep_ordered(state)
      if (isTRUE(isolate(state$confirmed))) {
        remember_confirmation()
      }
      rebuild(isolate(rebuild()) + 1L)
      grid_note(list(kind = "info", text = out$text))
    })

    observeEvent(input$spss_to_ordered, {
      clash <- sieve_spss_level_conflicts(isolate(state$meta), isolate(state$items))
      req(nrow(clash) > 0)
      set_types(clash$item, "ordered", "the items SPSS calls ordinal or nominal")
    })

    # The Response bounds row: declare, or clear, the bounds of the items in
    # scope; the cells hold each item's own.
    observeEvent(input$bounds_apply, {
      m <- isolate(state$meta)
      req(m)
      lo <- sieve_finite_or(input$bounds_lo, NA_real_)
      hi <- sieve_finite_or(input$bounds_hi, NA_real_)
      if (!is.finite(lo) || !is.finite(hi) || lo >= hi) {
        grid_note(list(
          kind = "error",
          text = "Give the lowest and the highest response of the scale, the lowest below the highest; nothing was declared."
        ))
        return(invisible(NULL))
      }
      sc <- items_in_scope(input$types_scope, m)
      out <- sieve_declare_bounds(state, lo, hi, items = sc$items)
      rebuild(rebuild() + 1L)
      grid_note(list(
        kind = if (length(out$outside) || length(out$reversed)) "warning" else "info",
        text = out$text
      ))
    })
    observeEvent(input$bounds_clear, {
      m <- isolate(state$meta)
      req(m)
      sc <- items_in_scope(input$types_scope, m)
      out <- sieve_clear_bounds(state, sc$items)
      rebuild(rebuild() + 1L)
      grid_note(list(kind = if (length(out$reversed) || length(out$keyed)) "warning" else "info", text = out$text))
    })
    # The bounds the file's value labels give for the items in scope: one
    # click enters them when they are the same for every item in scope.
    labels_in_scope <- reactive({
      keys$pool
      keys$dataset
      m <- isolate(state$meta)
      req(m)
      its <- items_in_scope(input$types_scope, m)$items
      if (!length(its)) {
        return(NULL)
      }
      have <- Filter(function(b) length(b) == 2L && all(is.finite(b)), sieve_label_bounds_of(isolate(state$raw), its))
      if (!length(have)) {
        return(NULL)
      }
      distinct <- unique(lapply(have, as.numeric))
      if (length(distinct) != 1L || length(have) != length(its)) {
        return(list(
          mixed = TRUE, n = length(have), of = length(its), agree = length(distinct) == 1L,
          lo = distinct[[1]][1], hi = distinct[[1]][2]
        ))
      }
      list(mixed = FALSE, lo = distinct[[1]][1], hi = distinct[[1]][2])
    })
    output$bounds_labels <- renderUI({
      got <- labels_in_scope()
      if (is.null(got)) {
        return(NULL)
      }
      if (isTRUE(got$mixed)) {
        # Two cases: the items whose labels give bounds agree, but some items
        # in scope have no labels or labels that give no bounds (a value lies
        # outside them), or the labels give different bounds.
        return(span(class = "text-muted", if (isTRUE(got$agree)) {
          sprintf(
            "(the file's value labels give %s to %s for %d of the %d items in scope; %s)",
            sieve_num_text(got$lo), sieve_num_text(got$hi), got$n, got$of,
            if (got$of - got$n == 1L) {
              "the other has no value labels that give its bounds"
            } else {
              "the others have no value labels that give their bounds"
            }
          )
        } else {
          sprintf(
            "(the file's value labels give different bounds across these items; %d of the %d have labels that give their bounds, shown in each item's Response bounds cell)",
            got$n, got$of
          )
        }))
      }
      actionButton(ns("bounds_use_labels"),
        sprintf("Enter the value labels' %s to %s", sieve_num_text(got$lo), sieve_num_text(got$hi)),
        class = "btn-outline-secondary btn-sm"
      )
    })
    observeEvent(input$bounds_use_labels, {
      got <- isolate(labels_in_scope())
      req(!is.null(got), !isTRUE(got$mixed))
      updateNumericInput(session, "bounds_lo", value = got$lo)
      updateNumericInput(session, "bounds_hi", value = got$hi)
    })
    # Reverse? of a set of items: each item whose Reverse? differs from the
    # one chosen is reverse scored (or returned to its scoring) within its
    # bounds, as its own cell does; a change withdraws the confirmation, as
    # a reversal in a cell does, and the rows that changed are redrawn.
    observeEvent(input$keying_apply, {
      m <- isolate(state$meta)
      req(m)
      sc <- items_in_scope(input$types_scope, m)
      want <- identical(input$keying_to %||% "Yes", "Yes")
      now <- m$reverse[match(sc$items, m$item)] %in% TRUE
      flip <- sc$items[now != want]
      if (!length(flip)) {
        grid_note(list(kind = "info", text = sprintf(
          "Nothing to change: Reverse? is already %s for %s.", if (want) "Yes" else "No", sc$what
        )))
        return(invisible(NULL))
      }
      out <- reverse_items(flip, origin = "ingest")
      if (length(out$applied)) {
        state$confirmed <- FALSE
      }
    })


    # The roles in force, said above the item grid. While the types are not
    # confirmed the line is a call to review; afterwards it is a quiet record.
    output$roles_notice <- renderUI({
      keys$roles
      keys$dataset
      confirmed <- isTRUE(sieve_confirmation_status(state)$confirmed)
      roles <- roles_now()
      req(roles, isolate(state$meta))
      not_items <- roles[roles$role != "item", , drop = FALSE]
      listed <- if (nrow(not_items)) {
        paste(sprintf("%s (%s)", not_items$variable, not_items$role), collapse = ", ")
      } else {
        "none"
      }
      if (confirmed) {
        # A quiet record: the counts once and the variables that are not
        # items by name (the status line above already says how many).
        return(p(
          class = "text-muted small mb-1", strong("Variable roles:"),
          sprintf("%s; not items: %s.", sieve_roles_summary(roles), listed)
        ))
      }
      div(
        class = "alert alert-warning py-2 small",
        strong("Step 3. Review the variable roles before you confirm."),
        sprintf("%s. %s Not items: %s.", sieve_roles_summary(roles), sieve_not_items_text(roles), listed),
        "Every column in the grid below is treated as an item. If one of them is an identifier, a ",
        "grouping variable or a criterion, or a column listed here is an item, change its role on the ",
        strong("Variable roles"), " panel."
      )
    })

    # ---- The roles grid and the roles editor -----------------------------------

    output$roles_grid <- renderDT(
      {
        rebuild()
        keys$roles
        keys$labels
        shown_cols <- input$roles_cols
        raw <- isolate(state$raw)
        req(raw, isolate(state$meta))
        roles <- roles_now()
        named <- sieve_item_names_of(roles$variable, isolate(state$meta))
        eid <- ns("cell_edit")
        shown <- function(role) paste0(toupper(substring(role, 1L, 1L)), substring(role, 2L))
        choices <- shown(sieve_role_levels())
        kinds <- c("not stated" = "", sieve_role_kinds())
        example <- vapply(roles$variable, function(v) {
          seen <- unique(raw[[v]][!is.na(raw[[v]])])
          first <- utils::head(seen, 4L)
          txt <- if (is.numeric(first)) format(as.numeric(first), digits = 4L, trim = TRUE) else as.character(first)
          paste0(paste(txt, collapse = ", "), if (length(seen) > 4L) ", ..." else "")
        }, character(1))
        kind <- as.character(roles$kind %||% rep(NA_character_, nrow(roles)))
        df <- data.frame(
          Variable = roles$variable,
          ItemName = named$item_name,
          Role = vapply(seq_len(nrow(roles)), function(i) {
            sieve_cell_select(
              eid, roles$variable[i], "role", shown(roles$role[i]),
              if (isTRUE(roles$numeric[i])) choices else setdiff(choices, "Item")
            )
          }, character(1)),
          Kind = vapply(seq_len(nrow(roles)), function(i) {
            if (!identical(roles$role[i], "criterion")) {
              return("")
            }
            sieve_cell_select(eid, roles$variable[i], "kind", if (is.na(kind[i])) "" else kind[i], kinds)
          }, character(1)),
          Proposed = shown(roles$proposed),
          Why = roles$reason,
          Stored = ifelse(roles$numeric, "number", "not a number"),
          Distinct = roles$n_distinct,
          Missing = roles$n_missing,
          Values = example,
          Question = vapply(named$wording, sieve_wording_cell, character(1), USE.NAMES = FALSE),
          stringsAsFactors = FALSE
        )
        headers <- sieve_roles_grid_headers()
        hidden <- if (is.null(shown_cols)) integer(0) else which(!headers %in% c("Variable Name", shown_cols)) - 1L
        html_cols <- c("Role", "Kind", "Question")
        sieve_dt(
          df,
          rownames = FALSE, selection = "none",
          escape = which(!names(df) %in% html_cols),
          options = list(
            pageLength = 100, dom = "tp", autoWidth = FALSE, stateSave = TRUE,
            stateSaveCallback = sieve_dt_state_save(), stateLoadCallback = sieve_dt_state_load(),
            sieve = list(id = "ingest-roles", keys = names(df)),
            columnDefs = c(
              list(list(orderable = FALSE, targets = which(names(df) %in% html_cols) - 1L)),
              if (length(hidden)) list(list(visible = FALSE, targets = hidden))
            )
          ),
          colnames = headers
        )
      },
      server = FALSE
    )

    # The picker lists every column, an item with its Item Name after the
    # variable name; a rename keeps what is already picked.
    observe({
      keys$dataset
      keys$labels
      raw <- isolate(state$raw)
      vars <- if (is.null(raw)) character(0) else names(raw)
      updateSelectizeInput(session, "roles_pick",
        choices = sieve_roles_pick_choices(vars, isolate(state$meta)),
        selected = intersect(as.character(isolate(input$roles_pick)), vars), server = FALSE
      )
    })
    observeEvent(input$roles_apply, {
      if (is.null(state$meta)) {
        roles_note(list(kind = "warning", text = "Load the data first (step 2)."))
        return(invisible(NULL))
      }
      role <- tolower(input$roles_role %||% "auxiliary")
      found <- sieve_match_names(input$roles_names %||% "", names(isolate(state$raw)))
      vars <- unique(c(found$matched, as.character(input$roles_pick %||% character(0))))
      if (!length(vars)) {
        roles_note(list(kind = "warning", text = paste0(
          "No variable named: paste or type variable names, or pick them from the list.",
          if (length(found$unmatched)) sprintf(
            " Names that match no column: %s.",
            sieve_name_some(found$unmatched)
          ) else ""
        )))
        return(invisible(NULL))
      }
      out <- sieve_apply_roles(state, stats::setNames(rep(role, length(vars)), vars))
      if (!is.null(out$refused)) {
        roles_note(list(kind = "error", text = out$refused))
        rebuild(rebuild() + 1L)
        return(invisible(NULL))
      }
      kind <- if (identical(role, "criterion")) input$roles_kind %||% "" else ""
      kind_out <- if (identical(role, "criterion")) sieve_apply_kind(state, vars, kind) else NULL
      rebuild(rebuild() + 1L)
      roles_note(list(kind = "info", text = paste0(
        sprintf(
          "Role %s%s set for %d variable%s: %s%s.", role,
          if (nzchar(kind)) sprintf(" (%s)", kind) else "",
          length(vars), if (length(vars) == 1L) "" else "s", sieve_name_some(vars, 10L),
          if (NROW(out$changed)) sprintf(" (%d changed)", NROW(out$changed)) else " (none changed)"
        ),
        if (!is.null(kind_out$refused)) paste0(" ", kind_out$refused) else "",
        if (length(found$unmatched)) sprintf(
          " Names that match no column: %s.",
          sieve_name_some(found$unmatched)
        ) else ""
      )))
    })
    output$roles_editor_note <- renderUI({
      note <- roles_note()
      if (is.null(note)) {
        return(NULL)
      }
      cls <- switch(note$kind,
        error = "text-danger",
        warning = "text-warning-emphasis",
        "text-muted"
      )
      p(class = paste("small mt-2 mb-0", cls), note$text)
    })

    # ---- In-cell edits ---------------------------------------------------------

    # In-cell edits update the ledger without rebuilding the table.
    observeEvent(input$cell_edit, {
      info <- input$cell_edit
      req(state$meta, info$item, info$field, !is.null(info$value))
      if (identical(info$field, "role")) {
        # A role is changed through the one function that keeps the item
        # data, the ledger, the roles table and the non item columns in
        # step (sieve_apply_roles() in fct_project.R). A change that cannot
        # be made changes nothing and says why.
        out <- sieve_apply_roles(state, stats::setNames(tolower(trimws(info$value)), info$item))
        if (!is.null(out$refused)) {
          roles_note(list(kind = "error", text = out$refused))
        } else {
          for (i in seq_len(NROW(out$changed))) {
            ch <- out$changed[i, ]
            roles_note(list(kind = "info", text = paste0(sieve_role_note(ch$variable, ch$from, ch$to), ".")))
          }
        }
        # Redraw either way: after a refusal the cell must show the role the
        # variable actually has.
        rebuild(rebuild() + 1L)
        return(invisible())
      }
      if (identical(info$field, "kind")) {
        out <- sieve_apply_kind(state, info$item, info$value)
        if (!is.null(out$refused)) {
          roles_note(list(kind = "error", text = out$refused))
          rebuild(rebuild() + 1L)
        } else if (length(out$changed)) {
          roles_note(list(kind = "info", text = sprintf(
            "Validity kind of %s: %s.", info$item,
            if (nzchar(info$value)) info$value else "not stated"
          )))
        }
        return(invisible())
      }
      idx <- which(state$meta$item == info$item)
      if (!length(idx)) {
        return(invisible())
      }
      if (info$field == "type") {
        v <- tolower(trimws(info$value))
        if (v %in% c("continuous", "ordered", "binary")) {
          sieve_apply_metadata_patch(
            state,
            sieve_metadata_patch(
              isolate(state$meta),
              info$item,
              "type",
              v
            ),
            "Set item type"
          )
          state$confirmed <- FALSE
        }
        # The row is pushed with the type in force (also when the value is
        # refused), as the bounds and reverse edits push theirs: a push an
        # earlier edit sent, built from the ledger as it was then, would
        # otherwise leave the cell showing the old type, and its note
        # "proposed" where the ledger now says "your choice".
        push_rows(info$item)
      } else if (info$field == "construct") {
        cn <- trimws(info$value)
        sieve_apply_metadata_patch(
          state,
          sieve_metadata_patch(
            isolate(state$meta),
            info$item,
            "construct",
            cn
          ),
          "Assign construct"
        )
        state$confirmed <- FALSE
      } else if (info$field == "facet") {
        fc <- trimws(info$value)
        construct <- state$meta$construct[idx]
        if (nzchar(fc) && (is.na(construct) || construct %in% c("", "Unassigned"))) {
          grid_note(list(kind = "error", text = sprintf(
            "A facet needs a construct to belong to: name the construct of %s before its facet.", info$item
          )))
          rebuild(rebuild() + 1L)
          return(invisible())
        }
        sieve_apply_metadata_patch(
          state,
          sieve_metadata_patch(
            isolate(state$meta),
            info$item,
            "facet",
            fc
          ),
          "Assign facet"
        )
      } else if (info$field == "status") {
        # Exclude the item (reversible; the ledger status is "parked"). It
        # leaves the active pool and every downstream fit, and the grid moves it
        # below the Items excluded line on the next draw. "Dropped" is read as
        # "Excluded".
        drop <- info$value %in% c("Excluded", "Dropped")
        sieve_coverage_notify(sieve_set_item_status(state, info$item, if (drop) "parked" else "active"))
        rebuild(rebuild() + 1L)
      } else if (info$field == "label") {
        lbl <- trimws(info$value)
        sieve_apply_metadata_patch(
          state,
          sieve_metadata_patch(
            isolate(state$meta),
            info$item,
            "label",
            if (nzchar(lbl)) lbl else info$item
          ),
          "Edit item name"
        )
      } else if (info$field == "bounds") {
        # The Response bounds cell: typed bounds are declared, a cleared cell
        # takes the declaration back, and anything else is refused. The row
        # is then pushed with the bounds in force (its Type note reads them),
        # not the grid drawn again, so a value being typed in another cell
        # and the keyboard focus are kept; the push records the item's new
        # keying, so the keying observer above finds nothing to redraw. The
        # other items' cells do not depend on this item's declaration (the
        # common range is read from the observed ranges, and whether an
        # item lies outside declared bounds is judged by its own range);
        # the line above the grid does, and follows the keying key.
        read <- sieve_parse_bounds(info$value)
        if (isTRUE(read$empty)) {
          out <- sieve_clear_bounds(state, info$item)
          if (length(out$cleared) || length(out$reversed)) {
            grid_note(list(kind = if (length(out$reversed) || length(out$keyed)) "warning" else "info", text = out$text))
          }
        } else if (!is.null(read$problem)) {
          grid_note(list(kind = "error", text = read$problem))
        } else {
          out <- sieve_declare_bounds(state, read$lo, read$hi, items = info$item)
          grid_note(list(
            kind = if (length(out$outside) || length(out$reversed)) "warning" else "info",
            text = out$text
          ))
        }
        push_rows(info$item)
      } else if (info$field == "reverse") {
        desired <- identical(info$value, "Yes")
        current <- isTRUE(state$meta$reverse[idx])
        if (desired != current) {
          out <- reverse_items(info$item, origin = "ingest")
          if (length(out$applied)) {
            state$confirmed <- FALSE
          }
        }
        # The row shows the keying the item has, applied or refused: its
        # summaries, its distribution and its Reverse? and Response bounds cells,
        # with no rebuild of the grid.
        push_rows(info$item)
      }
    })

    # ---- Step 4: the estimator and the confirmation ---------------------------

    # The estimator in force for the study, with its reason: the one the
    # included items' types resolve to, or the one the Estimator control in
    # step 4 names (sieve_estimator_resolve(), the rule every model fit uses).
    est <- reactive({
      keys$pool
      keys$dataset
      choice <- state$estimator_choice %||% "auto"
      m <- isolate(state$meta)
      req(m)
      sieve_estimator_resolve(m$type[m$status == "active"], n = NROW(isolate(state$items)), choice = choice)
    })

    output$estimator_box <- renderUI({
      keys$dataset
      choice <- sieve_estimator_effective(state$estimator_choice %||% "auto")
      tabs <- paste(
        "Each model tab (CFA, SEM, Reliability and the others) has the same Estimator control,",
        "which follows this setting unless it is set otherwise there."
      )
      if (is.null(isolate(state$meta))) {
        return(p(
          class = "small text-muted mb-1",
          "Load the data first (step 2); the estimator then follows the item types unless it is set otherwise above.",
          tabs
        ))
      }
      e <- est()
      div(
        class = "border rounded p-2 mt-2 bg-light",
        tags$small(
          strong("In force: "),
          if (identical(choice, "auto")) "following the item types" else "set above for the study"
        ),
        br(),
        tags$code(e$label), br(),
        tags$small(sprintf("Matrix: %s", e$basis)), br(),
        tags$small(class = "text-muted", e$reason), br(),
        tags$small(class = "text-muted", tabs)
      )
    })

    # ---- Step 4: the study's sampling design -----------------------------------
    observe({
      req(state$raw)
      updateSelectInput(session,
        "sampling_cluster",
        choices = c("Choose column" = "", names(state$raw)),
        selected = isolate(sieve_sampling_spec(state)$cluster) %||% ""
      )
    })
    observeEvent(state$sampling_design,
      {
        design <- sieve_sampling_spec(state)
        updateSelectInput(session, "sampling_unit", selected = design$unit)
        updateSelectInput(session, "sampling_cluster", selected = design$cluster %||% "")
      },
      ignoreNULL = TRUE
    )
    observeEvent(list(input$sampling_unit, input$sampling_cluster),
      {
        if (is.null(input$sampling_unit)) return()
        new <- list(unit = input$sampling_unit, cluster = input$sampling_cluster %||% "")
        old <- isolate(state$sampling_design) %||% list(unit = "independent", cluster = "")
        if (identical(new$unit, old$unit %||% "independent") && identical(new$cluster, old$cluster %||% "")) return()
        state$sampling_design <- new
        sieve_log(
          state,
          sprintf("Set the study's sampling design: %s", sieve_sampling_text(new)),
          c(
            sprintf("# The study's sampling design (Ingest tab, step 4) is now %s.", sieve_sampling_text(new)),
            "# Every page that computes uncertainty reads it; a procedure that assumes independent",
            "# rows refuses under another declaration rather than reporting an interval it cannot support.",
            sprintf("# sampling_design <- %s", paste(utils::capture.output(dput(new)), collapse = ""))
          )
        )
      },
      ignoreInit = TRUE
    )
    output$sampling_box <- renderUI({
      design <- sieve_sampling_spec(state)
      state$sampling_design
      p(
        class = "small text-muted mt-1",
        strong("In force: "), sieve_sampling_text(design), ". ",
        "The Reliability, Validity, Bifactor, SEM, and CV and Invariance pages and the report read this ",
        "declaration. Clustered or repeated rows need the cluster or respondent column; the continuous SEM ",
        "and CFA paths then use cluster-robust inference, which does not by itself fit a longitudinal ",
        "measurement model, and a procedure that assumes independent rows refuses rather than report an ",
        "interval it cannot support."
      )
    })

    # The study's estimator setting. A change of it changes no item type,
    # so the confirmation stands; the estimator recorded with the
    # confirmation follows, and the change is logged (the code each model
    # records names the estimator it was fit with).
    set_estimator_choice <- function(choice) {
      valid <- unname(sieve_estimator_choices())
      if (length(choice) != 1L || is.na(choice) || !choice %in% valid) {
        return(invisible(NULL))
      }
      old <- sieve_estimator_effective(isolate(state$estimator_choice) %||% "auto")
      if (identical(choice, old)) {
        return(invisible(NULL))
      }
      state$estimator_choice <- choice
      m <- isolate(state$meta)
      e <- if (is.null(m)) NULL else {
        sieve_estimator_resolve(m$type[m$status == "active"], n = NROW(isolate(state$items)), choice = choice)
      }
      if (isTRUE(isolate(state$confirmed)) && !is.null(e)) {
        state$estimator <- e$label
      }
      sieve_log(
        state,
        sprintf("Set the study's estimator: %s", sieve_estimator_choice_text(choice)),
        c(
          sprintf("# The study's estimator setting (Ingest tab, step 4) is now \"%s\" (it was \"%s\").", choice, old),
          "# Every model tab follows it unless its own Estimator control is set otherwise; the",
          "# code each model records names the estimator it was fit with.",
          if (!is.null(e)) sprintf("# With the included items: %s on a %s matrix.", e$label, e$basis)
        )
      )
      invisible(e)
    }
    observeEvent(input$estimator_choice, set_estimator_choice(input$estimator_choice), ignoreInit = TRUE)
    # The control shows the study's setting: after a project is opened, a
    # dataset is loaded, or the setting is changed anywhere else.
    observe({
      keys$dataset
      choice <- sieve_estimator_effective(state$estimator_choice %||% "auto")
      if (!identical(isolate(input$estimator_choice), choice)) {
        updateSelectInput(session, "estimator_choice", selected = choice)
      }
    })
    # A new dataset starts from the item types: the setting was made for
    # the data that were loaded before. The Load has already put the setting
    # back to "auto" (sieve_load_frame() resets the session through
    # sieve_reset_dataset(), and estimator_choice is a field it resets), so
    # the setting is the one do_load() read before it loaded anything
    # (`before`), not one read after the reset, which would return a setting
    # such as MLR with FIML to "auto" without a word.
    # A change is said once: as a note of the Load in the status line, where
    # it stays until the next Load (a Load raises one notification, and this
    # is not a second), and as a step of the session script.
    reset_estimator_choice <- function(before) {
      old <- sieve_estimator_effective(before)
      if (identical(old, "auto")) {
        return(invisible(FALSE))
      }
      state$estimator_choice <- "auto"
      sieve_log(
        state, "The study's estimator follows the item types for the new data",
        sprintf("# The setting \"%s\" was made for the data loaded before; it is \"auto\" again.", old)
      )
      load_note(
        sprintf(
          paste(
            "The study's estimator follows the item types again for the new data (it was set to %s",
            "for the data loaded before); set it in step 4 if you want another."
          ),
          sieve_estimator_choice_text(old)
        ),
        "info"
      )
      invisible(TRUE)
    }

    # The confirmation itself is sieve_confirm_types() in fct_prepare.R (the
    # state, the estimator and the log), shared with the pre-SIEVER step;
    # this wrapper guards the empty session, records what the study was at
    # the confirmation, and reports.
    do_confirm <- function(auto = FALSE, origin = if (auto) "auto" else "button") {
      # The confirm button is the most inviting control on the tab, so a click
      # before any data exists must say what to do next, never fail silently.
      if (is.null(state$meta)) {
        sieve_notify(
          paste(
            "No data loaded yet. Choose a file or a bundled example (step 1) and press Load",
            "(step 2); the study is confirmed here once a dataset arrives."
          ),
          type = "warning"
        )
        return(invisible(NULL))
      }
      e <- sieve_confirm_types(state, origin = origin)
      confirmed_from(if (identical(origin, "specification")) "specification" else NULL)
      remember_confirmation(origin)
      # The estimator in force, and where it was set when it is not the
      # one the item types imply.
      in_force <- sprintf(
        "Estimator %s on a %s matrix%s", e$label, e$basis,
        if (identical(e$choice, "auto")) "" else ", the study's setting in step 4"
      )
      # A confirmation made on load (every item continuous, or typed by the
      # specification) is stated by the status line and the Confirm button,
      # so it raises no notification (the confirmation is said once on the
      # screen); one the researcher just made is confirmed for a moment.
      if (origin %in% c("auto", "specification")) {
        return(invisible(NULL))
      }
      # With no included item assigned to a construct, the page gate keeps
      # the construct pages closed, so the message says what it unlocks.
      unlocked <- if (sieve_constructs_needed(state$meta)) {
        paste(
          "Screening, Dimensionality and EFA are unlocked; CFA and the pages after it wait for",
          "constructs in the grid's Construct column."
        )
      } else if (identical(origin, "rule")) {
        "All tabs are unlocked."
      } else {
        "Downstream tabs are unlocked."
      }
      sieve_notify(
        switch(origin,
          rule = sprintf(
            "The project is confirmed again with the switched types: %s. %s. %s",
            sieve_pool_text(state$meta), in_force, unlocked
          ),
          sprintf(
            "Confirmed: %s. %s. %s",
            sieve_pool_text(state$meta), in_force, unlocked
          )
        ),
        type = "message", id = session$ns("confirm_notification")
      )
    }
    # `origin` says how the study was confirmed: "button" (the press),
    # "auto" (every item detected continuous, confirmed on load),
    # "specification" (typed by the file, confirmed on load) or "rule" (a
    # reopened project confirmed again with switched types); the button
    # reads it, so a study confirmed on load says so instead of looking
    # like a button that was pressed.
    remember_confirmation <- function(origin = "button") {
      sieve_remember_confirmation(state, origin)
    }
    observeEvent(input$confirm, do_confirm())
    observeEvent(input$confirm_main, do_confirm())
    # Respondents removed from the analysis (the Screening tab subsets the
    # rows of the data and the item data) change no type and no item, so
    # they do not ask for the confirmation again: the stored content key of
    # the item data is brought up to the rows now in use.
    observeEvent(state$removed_cases,
      {
        at <- confirmed_at()
        if (is.null(at) || !identical(at$dataset_id, state$dataset_id)) {
          return(invisible(NULL))
        }
        at$data_key <- sieve_data_key(state$items)
        confirmed_at(at)
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )

    # The three states of the Confirm button: before the confirmation,
    # confirmed (the study in words, disabled; a change that left the
    # confirmation in force, an inclusion, an exclusion or a facet move, is
    # named in the note as in effect), and changed since the confirmation
    # (what changed, enabled) once a change withdrew it (a reversal, a
    # type, a construct, a role).
    confirm_status <- reactive({
      confirmed <- isTRUE(sieve_confirmation_status(state)$confirmed)
      keys$pool
      keys$keying
      keys$dataset
      at <- confirmed_at()
      m <- isolate(state$meta)
      if (is.null(m)) {
        return(list(
          state = "none", label = "Confirm the study (step 4)", class = "btn-primary",
          disabled = TRUE, why = "Load the data first (step 2)."
        ))
      }
      same_data <- !is.null(at) && identical(at$dataset_id, isolate(state$dataset_id))
      change <- if (same_data) {
        now <- list(meta = m[intersect(names(at$meta), names(m))], data_key = at$data_key,
          roles_key = sieve_hash(sieve_roles_table(isolate(state$raw), isolate(state$items),
            isolate(state$roles))[c("variable", "role")]))
        found <- sieve_confirm_change(at, now)
        if (is.null(found) && !identical(at$data_key, sieve_data_key(isolate(state$items)))) {
          found <- structure("the item values changed", kinds = "data")
        }
        found
      } else {
        NULL
      }
      # The changes that leave the confirmation in force: an inclusion, an
      # exclusion, a facet move (an inclusion or exclusion that changes the
      # estimator the types call for withdraws it, sieve_set_item_status()).
      # Any other change asks for the confirmation again, also where the
      # path that made it left the confirmation standing (a construct set on
      # the Items tab, a reversal through the shared transformation service).
      in_force_kinds <- c("status", "facet")
      # The estimator the included items' types called for at the
      # confirmation and now, resolved under the present setting (a backstop
      # for any writer of the status that did not withdraw the confirmation
      # itself, sieve_recheck_confirmation()).
      implied_of <- function(meta) {
        sieve_estimator_resolve(meta$type[meta$status == "active"],
          n = NROW(isolate(state$items)),
          choice = sieve_estimator_effective(state$estimator_choice %||% "auto")
        )$label
      }
      moved <- same_data && all(c("type", "status") %in% names(at$meta)) &&
        !identical(implied_of(at$meta), implied_of(m))
      if (confirmed && !moved && all(attr(change, "kinds") %in% in_force_kinds)) {
        # A study confirmed on load (every item detected continuous, or
        # every item typed by the specification) needed no press, and the
        # button says so; a study confirmed by the press reads as before.
        # A change that leaves the confirmation in force (an inclusion, an
        # exclusion, a facet move: a type, a construct or a keying withdraws
        # it) is in effect at once, so the button stays confirmed and the
        # note names the change; asking to confirm again would say the
        # exclusion was not yet in effect, which it is.
        origin <- at$origin %||% "button"
        on_load <- origin %in% c("auto", "specification")
        why <- switch(origin,
          auto = paste(
            "Confirmed on load: every item was detected continuous, so no press was needed",
            "(the confirmation gate exists for the ordered or binary estimator decision).",
            "Review the types in the grid; a change of type, construct, role or keying asks",
            "for the confirmation again."
          ),
          specification = paste(
            "Confirmed on load from the specification, which typed every item, so no press was",
            "needed. Review the types in the grid; a change of type, construct, role or keying",
            "asks for the confirmation again."
          ),
          "Confirmed"
        )
        if (!is.null(change)) {
          why <- paste0(
            why, if (!endsWith(why, ".")) "." else "",
            " Since the confirmation: ", change, ". In effect now: an inclusion, an exclusion or a ",
            "facet move needs no new confirmation unless it changes the estimator the types call ",
            "for (a type, a construct or a keying does)."
          )
        }
        return(list(
          state = "confirmed",
          label = sprintf(
            if (on_load) "Confirmed on load (step 4): %s" else "Confirmed: %s",
            sieve_pool_text(m)
          ),
          class = "btn-success", disabled = TRUE,
          why = why
        ))
      }
      if (same_data) {
        # What asks for the confirmation again, said apart from the changes
        # already in effect, so that an exclusion is never named as the
        # reason. With no such change left the ledger and the data are as
        # confirmed, yet the confirmation was withdrawn: a change (a
        # reversal, a type, a construct, a role) was made and then undone.
        # Say so, rather than claiming a change that does not show.
        keep <- setdiff(names(at$meta), in_force_kinds)
        withdrawing <- sieve_confirm_change(
          list(meta = at$meta[keep], data_key = at$data_key, roles_key = at$roles_key),
          list(meta = m[intersect(keep, names(m))], data_key = sieve_data_key(isolate(state$items)),
            roles_key = now$roles_key)
        )
        # Inclusions, exclusions and facet moves of the items present then
        # and now (an item that joined or left the pool is a withdrawing
        # change, named above).
        cols <- intersect(c("item", in_force_kinds), names(at$meta))
        common <- intersect(as.character(at$meta$item), as.character(m$item))
        in_effect <- sieve_confirm_change(
          list(meta = at$meta[at$meta$item %in% common, cols, drop = FALSE], data_key = NULL),
          list(meta = m[m$item %in% common, intersect(cols, names(m)), drop = FALSE], data_key = NULL)
        )
        moved <- moved && !is.null(in_effect)
        reason <- if (!is.null(withdrawing)) {
          as.vector(withdrawing)
        } else if (moved) {
          sprintf("%s, so the included items' types now call for %s", as.vector(in_effect), implied_of(m))
        }
        undone <- is.null(reason)
        also <- if (!is.null(in_effect) && !moved) sprintf(" Already in effect: %s.", as.vector(in_effect)) else ""
        return(list(
          state = "changed",
          label = sprintf(
            "Changed since confirmation (%s): confirm again",
            if (undone) "a change made and then undone" else reason
          ),
          class = "btn-warning", disabled = FALSE,
          why = paste0(if (undone) {
            "Changed since confirmation: a change was made and then undone, which withdrew the confirmation. Confirm again."
          } else {
            sprintf("Changed since confirmation: %s. Confirm again.", reason)
          }, also)
        ))
      }
      list(
        state = "before", label = "Confirm the study (step 4)", class = "btn-primary", disabled = FALSE,
        why = "Review the roles and the types (step 3), then confirm."
      )
    })
    observe({
      s <- confirm_status()
      updateActionButton(session, "confirm", label = s$label, disabled = isTRUE(s$disabled))
    })
    output$confirm_button <- renderUI({
      s <- confirm_status()
      actionButton(ns("confirm_main"),
        label = tagList(shiny::icon(switch(s$state,
          confirmed = "circle-check",
          changed = "triangle-exclamation",
          "circle-check"
        )), s$label),
        class = paste(
          "btn-lg rounded-3 shadow-sm fw-semibold", s$class,
          # Opaque and darker when confirmed, for contrast (custom.css).
          if (identical(s$state, "confirmed")) "sieve-confirmed-state"
        ),
        disabled = isTRUE(s$disabled),
        `aria-disabled` = if (isTRUE(s$disabled)) "true"
      )
    })
    output$confirm_state <- renderUI({
      s <- confirm_status()
      switch(s$state,
        none = div(class = "text-muted small mt-1", s$why),
        confirmed = div(class = "text-success small mt-1", s$why),
        changed = div(class = "text-warning-emphasis small mt-1", s$why),
        div(class = "text-warning small mt-1", "Awaiting confirmation: review, then confirm.")
      )
    })

    # The always-visible status line in the main-panel action bar (the
    # output keeps its name, status_chip): it names the step the researcher
    # is at and what the study holds, and lists the notes that are present,
    # those of the last Load among them (load_notes()). The run policy is in
    # the page guide and the tab paths in the sidebar and About.
    output$status_chip <- renderUI({
      confirmed <- isTRUE(sieve_confirmation_status(state)$confirmed)
      choice <- sieve_estimator_effective(state$estimator_choice %||% "auto")
      keys$pool
      keys$roles
      keys$dataset
      from <- confirmed_from()
      said <- described()
      m <- isolate(state$meta)
      refused <- load_error()
      if (is.null(m)) {
        return(sieve_status_line(
          "No data loaded. Step 1: choose a file or a bundled example in the sidebar, then press Load (step 2).",
          notes = refused,
          tone = if (length(refused)) "danger" else "neutral"
        ))
      }
      n <- nrow(m)
      roles <- roles_now()
      opened <- state$project$switched_types
      extra <- c(
        sieve_not_items_text(roles), sieve_described_text(m, said), sieve_analyzing_text(m),
        switched_note(), opened$text
      )
      notes <- load_notes()
      if (!identical(notes$dataset, isolate(state$dataset_id))) {
        notes <- list(text = character(0), kind = character(0))
      }
      if (confirmed) {
        # The next step follows the construct basis: a theoretical structure
        # goes to the CFA directly, nothing on that tab waits for the
        # exploratory tabs; an empirical one looks at the items first. It
        # also follows the page gate: while no included item has a
        # construct, the CFA and the other construct models are blocked
        # (sieve_constructs_needed(), the test sieve_page_status() applies),
        # so the theoretical step is to assign the constructs here, and
        # neither basis says every tab is unlocked: a study confirmed with
        # every item Unassigned is not sent to a CFA that would refuse to fit.
        unassigned <- sieve_constructs_needed(m)
        open_next <- if (identical(state$construct_basis %||% "theoretical", "theoretical")) {
          if (unassigned) {
            paste(
              "Constructs are still needed for CFA and the pages after it: assign them in the grid's",
              "Construct column, then confirm again."
            )
          } else {
            "Open CFA to fit the theoretical structure, or Screening to look at the items first."
          }
        } else {
          paste0(
            "Open Screening to look at the items, then Dimensionality and EFA for the number of factors and the structure",
            if (unassigned) "; CFA and the other construct models wait for constructs." else "."
          )
        }
        # The estimator in force; where it was set when it is not the one
        # the item types imply.
        in_force <- paste0(
          isolate(state$estimator) %||% "",
          if (identical(choice, "auto")) "" else ", the study's setting in step 4"
        )
        head <- if (identical(from, "specification")) {
          sprintf("Confirmed from the specification: %s (%s).", sieve_pool_text(m), in_force)
        } else {
          sprintf("%d items loaded; types confirmed (%s).", n, in_force)
        }
        # A proposal of the load is settled by the confirmation; its
        # warnings stay until the next Load.
        kept <- notes$text[notes$kind != "proposal"]
        sieve_status_line(paste(c(head, if (!unassigned) "All tabs unlocked.", open_next), collapse = " "),
          notes = c(extra, kept, refused),
          tone = if (length(refused)) "danger" else if (any(notes$kind[notes$kind != "proposal"] == "warning")) "warning" else "success")
      } else {
        sieve_status_line(
          sprintf(paste(
            "Step 3: %d items loaded; review the variable roles and the item types,",
            "then press Confirm (step 4) to unlock the analysis tabs."
          ), n),
          notes = c(extra, notes$text, refused), tone = if (length(refused)) "danger" else "warning"
        )
      }
    })

    # ---- Ledger summary ---------------------------------------------------------

    output$ledger_tbl <- renderDT(
      {
        keys$ledger
        m <- isolate(state$meta)
        req(m)
        df <- sieve_ledger_summary(m)
        sieve_dt(
          df,
          rownames = FALSE, selection = "none",
          options = list(dom = "tp", pageLength = 50, autoWidth = FALSE),
          colnames = c(
            "Construct", "Facet", "Items", "Included", "Excluded", "Included items by type",
            "Reverse scored"
          )
        )
      },
      server = FALSE
    )

    # The reverse keyed items of each construct (sieve_reverse_balance()).
    output$reverse_balance_tbl <- renderDT({
      keys$pool
      keys$keying
      m <- isolate(state$meta)
      req(m)
      df <- sieve_reverse_balance(m)
      req(nrow(df) > 0)
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t"),
        colnames = c("Construct", "Active items", "Reverse keyed", "Reading")
      ) |>
        formatStyle("reversed",
          backgroundColor = styleEqual(1, "#fdf3d6")
        )
    })

    output$question_tbl <- renderDT(
      {
        keys$ledger
        m <- isolate(state$meta)
        req(m)
        df <- data.frame(
          Variable = m$item,
          ItemName = m$label,
          Construct = m$construct,
          Facet = ifelse(is.na(m$facet), "", m$facet),
          Type = m$type,
          Status = ifelse(m$status == "active", "included", "excluded"),
          Reverse = ifelse(m$reverse %in% TRUE, "yes", "no"),
          # The observed range, the declared bounds when there are any, and
          # the range warning for the items it names.
          Range = sieve_range_column(m),
          Question = vapply(seq_len(nrow(m)), function(i) {
            sieve_wording_cell(if ("wording" %in% names(m)) m$wording[i] else NA_character_, n = 110L)
          }, character(1)),
          stringsAsFactors = FALSE
        )
        sieve_dt(
          df,
          rownames = FALSE, selection = "none", escape = which(names(df) != "Question"),
          options = list(
            dom = "tp", pageLength = 50, autoWidth = FALSE,
            columnDefs = list(list(orderable = FALSE, targets = which(names(df) == "Question") - 1L))
          ),
          colnames = c(
            "Variable Name", "Item Name", "Construct", "Facet", "Type", "Status", "Reverse scored",
            "Range", "Question"
          )
        )
      },
      server = FALSE
    )

    output$theory_txt <- renderText({
      if (is.null(state$theory)) {
        return(paste(
          "No theory seeded: a seeded theory comes only with a bundled example (or was cleared). A data",
          "file's constructs, from its names, description rows or specification, are in the grid; assign",
          "or edit them there."
        ))
      }
      paste0(
        "k = ", state$theory$k, ", ", state$theory$label, "\n\n",
        paste(
          sprintf(
            "  %-10s: %s",
            names(state$theory$map),
            vapply(state$theory$map, paste, character(1), collapse = ", ")
          ),
          collapse = "\n"
        )
      )
    })

    # ---- Dataset overview --------------------------------------------------

    output$summary_tbl <- renderDT({
      keys$pool
      keys$dataset
      m <- isolate(state$meta)
      raw <- isolate(state$raw)
      req(m, raw)
      act <- m[m$status == "active", , drop = FALSE]
      items <- intersect(act$item, names(isolate(state$items)))
      X <- isolate(state$items)[, items, drop = FALSE]
      X[] <- lapply(X, sieve_as_number)
      n_complete <- if (ncol(X)) sum(stats::complete.cases(X)) else 0L
      miss_pct <- if (ncol(X) && nrow(X)) 100 * mean(is.na(as.matrix(X))) else NA_real_
      cons <- sort(unique(act$construct))
      e <- est()
      vals <- c(
        isolate(state$dataset_name) %||% "(none)",
        format(nrow(raw), big.mark = ","),
        format(n_complete, big.mark = ","),
        as.character(nrow(m)),
        sprintf(
          "%d continuous, %d ordered, %d binary",
          sum(act$type == "continuous"), sum(act$type == "ordered"), sum(act$type == "binary")
        ),
        sprintf("%d (%s)", length(cons), paste(cons, collapse = ", ")),
        if (is.finite(miss_pct)) sprintf("%.1f%%", miss_pct) else "n/a",
        switch(isolate(state$construct_basis) %||% "theoretical",
          theoretical = "Theoretical (researcher-specified)",
          empirical = "Empirical (provisional)",
          isolate(state$construct_basis)
        ),
        sprintf(
          "%s on a %s matrix (%s)", e$label, e$basis,
          if (identical(e$choice, "auto")) "following the item types" else "the study's setting in step 4"
        )
      )
      df <- data.frame(
        Field = c(
          "Dataset", "Cases (rows)", "Complete cases (included items)",
          "Items", "Included item types", "Constructs",
          "Missing on included items", "Construct basis", "Estimator in force"
        ),
        Value = vals, stringsAsFactors = FALSE
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        colnames = c("", ""), options = list(dom = "t", ordering = FALSE)
      )
    })

    # Every column with its role, for the distribution plot: the first item
    # first, never an identifier.
    column_choices <- reactive({
      keys$roles
      keys$dataset
      roles <- roles_now()
      if (is.null(roles)) {
        return(character(0))
      }
      stats::setNames(roles$variable, sprintf("%s (%s)", roles$variable, roles$role))
    })
    observe({
      ch <- column_choices()
      roles <- roles_now()
      first_item <- if (!is.null(roles) && any(roles$role == "item")) roles$variable[roles$role == "item"][1] else NULL
      updateSelectInput(session, "plot_var", choices = ch, selected = first_item)
    })

    # Auxiliary (non item) variables, classified and summarized.
    aux_info <- reactive({
      keys$roles
      keys$dataset
      raw <- isolate(state$raw)
      roles <- roles_now()
      req(raw, roles)
      idv <- roles$variable[roles$role != "item"]
      lapply(idv, function(v) {
        x <- raw[[v]]
        xn <- sieve_as_number(x)
        numeric_ok <- sum(!is.na(xn)) >= 1L && length(unique(xn[!is.na(xn)])) > 2L &&
          !is.character(x) && !is.factor(x)
        list(
          name = v,
          role = roles$role[match(v, roles$variable)],
          kind = as.character(roles$kind %||% rep(NA_character_, nrow(roles)))[match(v, roles$variable)],
          storage = if (numeric_ok) "numeric" else "categorical",
          n = sum(!is.na(x)),
          missing = sum(is.na(x)),
          mean = if (numeric_ok) mean(xn, na.rm = TRUE) else NA_real_,
          sd = if (numeric_ok) stats::sd(xn, na.rm = TRUE) else NA_real_,
          levels = if (!numeric_ok) length(unique(x[!is.na(x)])) else NA_integer_,
          x = x, xn = xn, numeric_ok = numeric_ok
        )
      })
    })

    output$antecedent_note <- renderUI({
      ai <- aux_info()
      ante <- vapply(Filter(function(z) identical(z$role, "antecedent"), ai), function(z) z$name, character(1))
      if (!length(ante)) {
        return(NULL)
      }
      div(
        class = "small mb-2",
        strong(sprintf(
          "Antecedent%s (variables that exist before the target construct): ",
          if (length(ante) == 1L) "" else "s"
        )),
        paste(ante, collapse = ", "),
        ". Offered as observed predictors of the constructs on the SEM tab; not offered as criteria on the Validity tab."
      )
    })

    output$aux_tbl <- renderDT({
      state$palette
      ai <- aux_info()
      req(length(ai) > 0)
      pal <- sieve_palette_colors(isolate(state$palette) %||% "nd", 1L)[1]
      df <- data.frame(
        Variable = vapply(ai, function(z) z$name, character(1)),
        Role = vapply(ai, function(z) z$role %||% NA_character_, character(1)),
        Kind = vapply(ai, function(z) if (is.na(z$kind)) "" else z$kind, character(1)),
        Stored = vapply(ai, function(z) z$storage, character(1)),
        n = vapply(ai, function(z) z$n, integer(1)),
        Missing = vapply(ai, function(z) z$missing, integer(1)),
        Mean = vapply(ai, function(z) z$mean, numeric(1)),
        SD = vapply(ai, function(z) z$sd, numeric(1)),
        Levels = vapply(ai, function(z) if (is.na(z$levels)) NA_integer_ else z$levels, integer(1)),
        Distribution = vapply(ai, function(z) {
          if (z$numeric_ok) sieve_spark_svg(z$xn, fill = pal) else ""
        }, character(1)),
        stringsAsFactors = FALSE
      )
      no_html <- which(names(df) != "Distribution")
      sieve_dt(df,
        rownames = FALSE, selection = "none", escape = no_html,
        options = list(dom = "tp", pageLength = 15),
        colnames = c(
          "Variable", "Role", "Validity kind", "Stored as", "n", "Missing", "Mean", "SD",
          "Levels", "Distribution"
        )
      ) |>
        formatRound(c("Mean", "SD"), 2)
    })

    output$dist_plot <- renderPlot(alt = "Distribution of the chosen variable: a bar per value with its count, or a histogram.", {
      v <- input$plot_var
      keys$keying
      keys$dataset
      keys$labels
      req(v)
      raw <- isolate(state$raw)
      items <- isolate(state$items)
      req(raw, v %in% names(raw))
      roles <- roles_now()
      role <- roles$role[match(v, roles$variable)]
      is_item <- identical(role, "item") && v %in% names(items)
      x <- if (is_item) items[[v]] else raw[[v]]
      # An item's Item Name and its declared response bounds, so every point
      # of its scale is drawn, the ones no one chose included.
      m <- isolate(state$meta)
      i <- if (is_item && !is.null(m)) match(v, m$item) else NA_integer_
      declared <- !is.na(i) && "rev_bounds" %in% names(m) && identical(as.character(m$rev_bounds[i]), "declared")
      d <- sieve_distribution_data(
        x, v,
        role = role,
        label = if (!is.na(i)) as.character(m$label[i]) else NULL,
        lo = if (declared) m$rev_lo[i] else NA_real_,
        hi = if (declared) m$rev_hi[i] else NA_real_
      )
      cols <- sieve_palette_colors(isolate(state$palette) %||% "nd", 2L)
      sieve_distribution_plot(d, col = cols[1], line_col = cols[2])
    })

    # ---- Apply a specification -------------------------------------------
    # Checked, tried on copies, and committed only as a whole
    # (sieve_apply_spec() in fct_export.R). A refused specification changes
    # nothing.
    # The same restore from the sidebar's specification input (step 3) and
    # from a specification that arrives with its data file (step 1). A
    # specification that arrives with the data confirms the study when it
    # types every item (the estimator decision is in the specification);
    # one applied from the sidebar onto a session leaves the confirmation
    # for the researcher, as before, since the pool it was written for may
    # not be this one.
    # What the specification did, a reversal within observed bounds, the
    # items it does not type, and why it was refused are written in the
    # status line (load_note(), load_error()), where they stay until the next
    # Load, and the action gives one message notification at most, rather
    # than one for the specification, one per reversal within observed
    # bounds, one for the items it does not type and the Load's own.
    # `with_data` is TRUE when the
    # specification came with a data file this Load read: the data are then
    # loaded whatever becomes of the specification, so a refusal is a note
    # of the new data, and the Load's own notification names the
    # specification (do_load()).
    restore_spec <- function(path, name, from_load = FALSE, with_data = FALSE) {
      if (!with_data) load_error(NULL)
      out <- sieve_apply_spec(state, path,
        keying = input$spec_keying %||% "verify", name = name,
        min_options = min_options()
      )
      if (!isTRUE(out$ok)) {
        refusal <- paste(
          sprintf(
            "The specification %s was not applied, and it changed nothing%s:", name,
            if (with_data) "; the data are loaded with their roles and types as proposed, awaiting confirmation" else ""
          ),
          paste(out$problems, collapse = " ")
        )
        if (with_data) load_note(refusal, "warning") else load_error(refusal)
        return(invisible(NULL))
      }
      confirmed_from(NULL)
      rebuild(rebuild() + 1L)
      # Items the specification typed ordered that the five-option rule
      # read as continuous: said on the chip and above the grid.
      said <- sieve_option_rule_text(out$summary$switched, "the specification")
      if (!is.null(said)) {
        switched_note(said)
        grid_note(list(kind = "info", text = said))
      }
      load_note(out$message, "info")
      for (rec in out$records) {
        if (identical(rec$type, "reverse") && identical(rec$bounds, "observed")) {
          load_note(sieve_reverse_message(rec), "warning")
        }
      }
      if (from_load) {
        if (isTRUE(out$summary$typed_all)) {
          do_confirm(origin = "specification")
        } else {
          # Settled, like the proposals of a Load, by the confirmation.
          load_note(
            sprintf(
              paste(
                "The specification does not type %s, so the study is not confirmed from it.",
                "Review the types in the grid, then confirm the study (step 4)."
              ),
              sieve_name_some(out$summary$untyped)
            ),
            "proposal"
          )
        }
      }
      if (!with_data) {
        sieve_notify(
          sprintf("The specification %s was applied; what it did is listed in the status line above the grid.", name),
          type = "message", id = session$ns("load_notification")
        )
      }
      invisible(out)
    }
    observeEvent(input$up_spec, {
      req(input$up_spec)
      if (is.null(state$meta)) {
        load_error("Load the data first (step 2), then apply the specification.")
        return(invisible(NULL))
      }
      restore_spec(input$up_spec$datapath, input$up_spec$name)
    })

    # The same checked dialog serves the project bar and this entry point.
    project_open <- sieve_project_open_wire(input, output, session, state,
      ids = list(open = "open_project", file = "project_file", preview = "project_preview", confirm = "confirm_open"),
      min_options = min_options)
    open_dialog <- project_open$dialog
    preview_project <- project_open$preview
    pending <- project_open$pending
    # Every open route renews the Ingest baseline, not only this module's button.
    # Read through a reactiveVal, which changes only when the opened
    # dataset's identity does, so that a later edit of state$project (a note
    # answered, such as Keep these items ordered or the estimator kept on
    # opening) does not renew the baseline and clear the page's notes.
    project_dataset <- reactiveVal(NULL)
    observe(project_dataset(state$project$dataset_id), priority = 90)
    observeEvent(project_dataset(), {
      confirmed_from(NULL)
      described(NULL)
      reverse_held(NULL)
      grid_note(NULL)
      switched_note(NULL)
      if (!isTRUE(isolate(state$confirmed))) confirmed_at(NULL)
      shiny::updateRadioButtons(session, "basis", selected = state$construct_basis %||% "theoretical")
      updateSelectInput(session, "palette", selected = state$palette %||% "nd")
      rebuild(isolate(rebuild()) + 1L)
    }, ignoreNULL = TRUE, priority = 80)

  })
}

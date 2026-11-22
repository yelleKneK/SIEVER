# The local preparation of the items: the response bounds and the reverse
# coding in the item grid (an item's Response bounds and Reverse? cells,
# and for a set of items the Response bounds and Reverse? rows under
# "Response bounds and Reverse? for several items at once" above the grid),
# the wording checks on the Content Validity tab (sub-tab Item Writing
# Checks) and the keying balance on the Ingest tab's Ledger summary panel.
# The blocks test a bounds declaration that is always available beside the
# keying, the value labels' proposal with one click, the declaration saved
# with the project, the reverse coding and the wording checks with their
# identifiers and logged steps, and the local tools in their places (the
# Update label with an unknown component is in
# test-page-update-duration.R). Section 6 checks the reading of typed
# bounds, an item that arrived reverse scored (its bounds typed, corrected,
# or given by a specification, and Reverse? No refused without them), the
# range warning of a reversed item, the line above the grid after one
# cell's declaration, the redraw when a reversal moves the common range, a
# declaration that changes only the kind of stored bounds, cleared bounds
# in the Load and replace dialog and in the reload sentence, the value
# labels' note for mixed items, and the escaping of the wording table; the
# row updated in place in a browser, with a value being typed, is in
# test-shinytest2-interface.R. The undo records are checked through the
# transformation service itself (sieve_apply_reverse(), the Reverse? cell
# and row).
#
# Author: Ken Kelley

ip_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}
ip_html <- function(out) {
  x <- if (is.list(out) && !is.null(out$html)) out$html else out
  paste(as.character(unlist(x)), collapse = "")
}
ip_path <- function(name) {
  path <- file.path(tempfile("ip_"), name)
  dir.create(dirname(path))
  path
}

# The item grid as drawn (output$type_grid): one row per item, its cells
# named by the grid's own columns (sieve_grid_columns()), with the headers
# the table shows in the attribute "headers".
ip_grid <- function(out) {
  x <- jsonlite::fromJSON(as.character(out), simplifyVector = FALSE)$x
  cols <- lapply(x$data, function(col) {
    vapply(col, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1))
  })
  names(cols) <- sieve_grid_columns()[seq_along(cols)]
  heads <- regmatches(x$container, gregexpr("<th>[^<]*</th>", x$container))[[1]]
  structure(
    as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE),
    headers = gsub("</?th>", "", heads)
  )
}
ip_cell <- function(grid, item, col) grid[[col]][match(item, grid$Variable)]

# The cell of one column in a row pushed to the grid in place (push_row():
# the cells are named by their zero based column index).
ip_pushed_cell <- function(msg, col) {
  msg$cells[[as.character(match(col, sieve_grid_columns()) - 1L)]]
}

# Nine items on a seven point scale of which only 2 to 6 were chosen: every
# item has the same observed range, so the range warning has nothing to
# say, and the observed range is narrower than the scale.
ip_narrow <- function(n = 40L) {
  set.seed(1)
  d <- as.data.frame(lapply(1:9, function(j) sample(2:6, n, replace = TRUE)))
  names(d) <- paste0("q", 1:9)
  d
}

# A state with the nine Holzinger and Swineford tests, loaded by the Ingest
# server (so that the dataset identity and the roles are as in the app).
ip_hs9 <- function() {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
  })
  st
}

# ---- 1. A declaration without a range warning, then a reversal within it ----

test_that("bounds are declared in the item grid for an item observed 2 to 6 without a range warning, and a reversal then reflects within 1 to 7", {
  d <- ip_narrow()
  path <- ip_path("narrow.csv")
  utils::write.csv(d, path, row.names = FALSE)
  # The rows pushed to the grid in place (a reversal redraws its row, not
  # the grid), kept so that the cells the researcher sees can be read.
  pushed <- list()
  testthat::local_mocked_bindings(
    sieve_session_message = function(session, type, message) {
      # One message carries every row an edit or an action changed.
      if (identical(type, "sieve_ingest_row")) for (row in message$rows) pushed[[length(pushed) + 1L]] <<- row
      invisible(NULL)
    },
    .package = "SIEVER"
  )
  last_push <- function(item) {
    hit <- Filter(function(msg) identical(msg$key, item), pushed)
    if (length(hit)) hit[[length(hit)]] else NULL
  }
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "narrow.csv", datapath = path))
    session$setInputs(load = 1L)
    # No range warning: every item shares the observed range 2 to 6.
    expect_null(sieve_range_text(sieve_range_check(state$meta)))
    expect_null(output$bounds_note)
    # The declaration is available all the same, in each item's Response
    # bounds cell: the column
    # follows Reverse?, and an empty cell shows the observed range as its
    # hint, with no warning under any cell.
    grid <- ip_grid(output$type_grid)
    heads <- attr(grid, "headers")
    expect_identical(match("Response bounds", heads), match("Reverse?", heads) + 1L)
    q1 <- ip_cell(grid, "q1", "Bounds")
    expect_match(q1, 'data-item="q1" data-field="bounds" value="" placeholder="observed 2 to 6"', fixed = TRUE)
    expect_false(any(grepl("no bounds are declared", grid$Bounds, fixed = TRUE)))
    items_before <- state$items
    confirmed_before <- state$confirmed
    log_before <- state$codelog
    drawn_before <- output$type_grid
    # Declare 1 to 7 for q1 by typing them in its cell.
    session$setInputs(cell_edit = list(item = "q1", field = "bounds", value = "1 to 7"))
    m <- sieve_meta_keying(state$meta)
    i <- match("q1", m$item)
    expect_identical(c(m$rev_lo[i], m$rev_hi[i]), c(1, 7))
    expect_identical(m$rev_bounds[i], "declared")
    expect_false(isTRUE(m$reverse[i]))
    expect_true(all(is.na(m$rev_bounds[-i])))
    # Declaring reverses nothing and withdraws no confirmation.
    expect_identical(state$items, items_before)
    expect_identical(state$confirmed, confirmed_before)
    note <- ip_text(output$grid_note)
    expect_match(note, "Response bounds 1 to 7 declared for 1 item", fixed = TRUE)
    # Shown against the observed range: in q1's cell (the declared bounds,
    # with the observed range still its hint), in no other cell, and in the
    # ledger's Range column. The row is pushed in place, with its Type cell
    # (whose note reads the declared bounds), and the grid is not drawn
    # again: a redraw would take a value being typed in another cell and
    # the keyboard focus.
    expect_identical(output$type_grid, drawn_before)
    row <- last_push("q1")
    expect_false(is.null(row))
    expect_match(
      ip_pushed_cell(row, "Bounds"),
      'data-field="bounds" value="1 to 7" placeholder="observed 2 to 6"',
      fixed = TRUE
    )
    expect_match(ip_pushed_cell(row, "Type"), 'data-item="q1" data-field="type"', fixed = TRUE)
    expect_identical(sum(grepl('value="1 to 7"', unlist(lapply(pushed, ip_pushed_cell, "Bounds")), fixed = TRUE)), 1L)
    expect_match(paste(as.character(output$question_tbl), collapse = ""), "2 to 6 (observed); declared 1 to 7", fixed = TRUE)
    # Logged in the session script as the ledger write it is, once.
    log <- state$codelog
    added <- setdiff(log, log_before)
    expect_identical(sum(grepl("Declare the response bounds 1 to 7 for 1 item", added, fixed = TRUE)), 1L)
    expect_true(any(grepl('declared_items <- c("q1")', log, fixed = TRUE)))
    expect_true(any(grepl("meta$rev_lo[meta$item %in% declared_items] <- 1", log, fixed = TRUE)))
    # A reversal from the grid's Reverse? cell now reflects within 1 to 7:
    # x -> 8 - x, not the observed 2 to 6 (which would also give 8 - x here,
    # so the ledger says which bounds were used).
    session$setInputs(cell_edit = list(item = "q1", field = "reverse", value = "Yes"))
    expect_identical(as.numeric(state$items$q1), 8 - as.numeric(d$q1))
    m <- state$meta
    expect_identical(m$rev_bounds[m$item == "q1"], "declared")
    expect_true(m$reverse[m$item == "q1"])
    expect_match(ip_html(output$grid_note), "within the response bounds 1 to 7", fixed = TRUE)
    expect_true(any(grepl('items[["q1"]] <- (1 + 7) - items[["q1"]]', state$codelog, fixed = TRUE)))
    # Its row shows the bounds of the reversal as plain text, not a field,
    # while it is reverse scored; they stay stored with the item afterwards.
    row <- last_push("q1")
    expect_false(is.null(row))
    expect_match(ip_pushed_cell(row, "Reverse"), '<option value="Yes" selected>', fixed = TRUE)
    bounds_cell <- ip_pushed_cell(row, "Bounds")
    expect_match(bounds_cell, '<div class="small">1 to 7</div>', fixed = TRUE)
    expect_match(bounds_cell, "the bounds of its reversal; set Reverse? to No to change them", fixed = TRUE)
    expect_false(grepl('data-field="bounds"', bounds_cell, fixed = TRUE))
    # Reversing again returns the values as loaded, within the same bounds.
    session$setInputs(cell_edit = list(item = "q1", field = "reverse", value = "No"))
    expect_identical(state$items$q1, d$q1)
    # A declaration for every included item from the Response bounds row
    # above the grid: q1 is covered too (it is not reverse scored any more).
    session$setInputs(types_scope = "all", bounds_lo = 1, bounds_hi = 7)
    session$setInputs(bounds_apply = 1L)
    m <- state$meta
    expect_true(all(m$rev_bounds == "declared"))
    expect_true(all(m$rev_lo == 1 & m$rev_hi == 7))
    expect_match(ip_text(output$grid_note), "Response bounds 1 to 7 declared for 9 items", fixed = TRUE)
    # A reversal of a set of items from the Reverse? row reflects q2, the one item of its construct,
    # within 1 to 7 and says so; the other items are left as they are.
    session$setInputs(cell_edit = list(item = "q2", field = "construct", value = "Negative"))
    session$setInputs(types_scope = "construct:Negative", keying_to = "Yes")
    session$setInputs(keying_apply = 1L)
    expect_identical(as.numeric(state$items$q2), 8 - as.numeric(d$q2))
    for (v in setdiff(names(d), "q2")) expect_identical(state$items[[v]], d[[v]], info = v)
    expect_identical(state$transforms[[length(state$transforms)]]$item, "q2")
    expect_identical(state$transforms[[length(state$transforms)]]$origin, "ingest")
    expect_identical(state$transforms[[length(state$transforms)]]$bounds, "declared")
    expect_match(ip_text(output$grid_note), "q2 is now reverse scored within the response bounds 1 to 7", fixed = TRUE)
    expect_false(isTRUE(state$confirmed))
    row <- last_push("q2")
    expect_false(is.null(row))
    expect_match(ip_pushed_cell(row, "Reverse"), '<option value="Yes" selected>', fixed = TRUE)
    expect_match(ip_pushed_cell(row, "Bounds"), "the bounds of its reversal; set Reverse? to No", fixed = TRUE)
    # The row again changes nothing, and says so.
    n_transforms <- length(state$transforms)
    session$setInputs(keying_apply = 2L)
    expect_length(state$transforms, n_transforms)
    expect_match(ip_text(output$grid_note), "Nothing to change: Reverse? is already Yes for the items of construct Negative", fixed = TRUE)
    # A reverse scored item keeps the bounds of its reversal: the next
    # declaration leaves it alone and says so; an item with a value outside
    # the bounds is left alone and named.
    session$setInputs(types_scope = "all", bounds_lo = 3, bounds_hi = 9)
    session$setInputs(bounds_apply = 2L)
    said <- ip_text(output$grid_note)
    expect_match(said, "q2 is reverse scored already", fixed = TRUE)
    expect_match(said, "a value outside 3 to 9", fixed = TRUE)
    # So does a Clear of its bounds, while the declaration of an item that
    # is not reverse scored is taken back by clearing its cell.
    session$setInputs(types_scope = "construct:Negative")
    session$setInputs(bounds_clear = 1L)
    expect_match(ip_text(output$grid_note), "q2 is reverse scored and keeps the bounds of that reversal", fixed = TRUE)
    expect_identical(state$meta$rev_bounds[state$meta$item == "q2"], "declared")
    session$setInputs(cell_edit = list(item = "q3", field = "bounds", value = ""))
    m <- sieve_meta_keying(state$meta)
    expect_true(is.na(m$rev_bounds[m$item == "q3"]))
    expect_true(is.na(m$rev_lo[m$item == "q3"]) && is.na(m$rev_hi[m$item == "q3"]))
    expect_true(any(grepl("Clear the declared response bounds of 1 item", state$codelog, fixed = TRUE)))
    expect_true(any(grepl('cleared_items <- c("q3")', state$codelog, fixed = TRUE)))
    expect_match(ip_text(output$grid_note),
      "The declared response bounds of q3 are cleared; a reversal reads the observed range again.",
      fixed = TRUE
    )
    # Bounds the wrong way round, or one of the two, declare nothing, in
    # the row or in a cell, nor does text that is not two numbers.
    log_n <- length(state$codelog)
    meta_n <- state$meta
    session$setInputs(types_scope = "all", bounds_lo = 7, bounds_hi = 1)
    session$setInputs(bounds_apply = 3L)
    expect_match(ip_text(output$grid_note), "nothing was declared", fixed = TRUE)
    session$setInputs(bounds_lo = 1, bounds_hi = NA_real_)
    session$setInputs(bounds_apply = 4L)
    expect_match(ip_text(output$grid_note), "nothing was declared", fixed = TRUE)
    session$setInputs(cell_edit = list(item = "q3", field = "bounds", value = "7 to 1"))
    expect_match(ip_text(output$grid_note), "The lowest response must be below the highest; nothing was declared", fixed = TRUE)
    session$setInputs(cell_edit = list(item = "q3", field = "bounds", value = "seven"))
    expect_match(ip_text(output$grid_note), "is not two numbers", fixed = TRUE)
    expect_length(state$codelog, log_n)
    expect_identical(state$meta, meta_n)
    # A refused entry pushes the row back with the bounds in force (none for
    # q3, cleared above), so the cell does not keep the refused text.
    expect_match(ip_pushed_cell(last_push("q3"), "Bounds"), 'data-field="bounds" value=""', fixed = TRUE)
  })
})

# ---- 2. The value-label proposal ---------------------------------------------

test_that("a file's value labels propose the bounds, shown against the observed range, with one click to enter them", {
  skip_if_not_installed("haven")
  d <- ip_narrow()
  labs <- stats::setNames(1:7, c("Never", "Rarely", "Seldom", "Sometimes", "Often", "Usually", "Always"))
  for (v in names(d)) d[[v]] <- haven::labelled(as.double(d[[v]]), labels = labs)
  path <- ip_path("labelled.sav")
  haven::write_sav(d, path)
  # What the click enters, kept in an environment so that the test can
  # clear it from inside the server block.
  entered <- new.env(parent = emptyenv())
  pushed <- list()
  shown <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    showModal = function(ui, ...) shown$ui <- ui,
    .package = "shiny"
  )
  testthat::local_mocked_bindings(
    updateNumericInput = function(session, inputId, ..., value = NULL) {
      assign(inputId, value, envir = entered)
      invisible(NULL)
    },
    sieve_session_message = function(session, type, message) {
      # One message carries every row an edit or an action changed.
      if (identical(type, "sieve_ingest_row")) for (row in message$rows) pushed[[length(pushed) + 1L]] <<- row
      invisible(NULL)
    },
    .package = "SIEVER"
  )
  last_push <- function(item) {
    hit <- Filter(function(msg) identical(msg$key, item), pushed)
    if (length(hit)) hit[[length(hit)]] else NULL
  }
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "labelled.sav", datapath = path))
    session$setInputs(load = 1L)
    # The labels declare 1 to 7 at load (sieve_label_bounds()); each item's
    # Response bounds cell holds them with the labels as its hint, and says
    # under the field where they came from, "declared 1 to 7 (the file's
    # value labels)" (the hint is hidden once the field holds a value). The observed range is
    # beside them in the ledger's Range column.
    m <- state$meta
    expect_true(all(m$rev_bounds == "declared"))
    expect_true(all(m$rev_lo == 1 & m$rev_hi == 7))
    grid <- ip_grid(output$type_grid)
    expect_true(all(grepl('data-field="bounds" value="1 to 7" placeholder="labels: 1 to 7"', grid$Bounds, fixed = TRUE)))
    expect_true(all(grepl("from the file's value labels", grid$Bounds, fixed = TRUE)))
    expect_false(any(grepl("the file's value labels give", grid$Bounds, fixed = TRUE)))
    expect_match(paste(as.character(output$question_tbl), collapse = ""), "2 to 6 (observed); declared 1 to 7", fixed = TRUE)
    # The proposal for every included item, in the Response bounds row, and
    # the click that enters it.
    session$setInputs(types_scope = "all")
    said <- ip_html(output$bounds_labels)
    expect_match(said, "bounds_use_labels", fixed = TRUE)
    expect_match(said, "Enter the value labels&#39; 1 to 7|Enter the value labels' 1 to 7")
    session$setInputs(bounds_use_labels = 1L)
    expect_identical(entered$bounds_lo, 1)
    expect_identical(entered$bounds_hi, 7)
    # For one item: the one item of its construct.
    rm(list = ls(entered), envir = entered)
    session$setInputs(cell_edit = list(item = "q3", field = "construct", value = "Solo"))
    session$setInputs(types_scope = "construct:Solo")
    expect_match(ip_html(output$bounds_labels), "Enter the value labels&#39; 1 to 7|Enter the value labels' 1 to 7")
    session$setInputs(bounds_use_labels = 2L)
    expect_identical(entered$bounds_lo, 1)
    expect_identical(entered$bounds_hi, 7)
    # A declaration other than the labels' is shown beside them: in the
    # cell (its row pushed in place), with the labels' bounds named under
    # it, and in the Range column.
    session$setInputs(cell_edit = list(item = "q3", field = "bounds", value = "0 to 10"))
    m <- state$meta
    expect_identical(c(m$rev_lo[m$item == "q3"], m$rev_hi[m$item == "q3"]), c(0, 10))
    grid <- ip_grid(output$type_grid)
    q3 <- ip_pushed_cell(last_push("q3"), "Bounds")
    expect_match(q3, 'data-field="bounds" value="0 to 10" placeholder="labels: 1 to 7"', fixed = TRUE)
    expect_match(q3, "the file's value labels give 1 to 7", fixed = TRUE)
    expect_false(grepl("from the file's value labels", q3, fixed = TRUE))
    expect_false(grepl("the file's value labels give", ip_cell(grid, "q4", "Bounds"), fixed = TRUE))
    expect_match(ip_cell(grid, "q4", "Bounds"), "from the file's value labels", fixed = TRUE)
    expect_match(paste(as.character(output$question_tbl), collapse = ""), "2 to 6 (observed); declared 0 to 10", fixed = TRUE)
    # Clearing a declaration the labels made says that loading the file
    # again declares it again (the specification carries no "cleared"), and
    # the Load and replace dialog names it as cleared, not declared.
    session$setInputs(cell_edit = list(item = "q4", field = "bounds", value = ""))
    expect_true(is.na(state$meta$rev_bounds[state$meta$item == "q4"]))
    expect_match(ip_text(output$grid_note),
      "The file's value labels give the bounds of q4, so loading the file again declares them again.",
      fixed = TRUE
    )
    expect_match(ip_pushed_cell(last_push("q4"), "Bounds"), 'value="" placeholder="labels: 1 to 7"', fixed = TRUE)
    # The dialog of a second Load (nothing changes until Load and replace).
    session$setInputs(load = 2L)
    said <- ip_text(shown$ui)
    expect_match(said, "the declared response bounds of q4 cleared", fixed = TRUE)
    expect_match(said, "response bounds declared for q3", fixed = TRUE)
    expect_false(grepl("declared for q3, q4", said, fixed = TRUE))
    expect_true(is.na(state$meta$rev_bounds[state$meta$item == "q4"]))
  })
  # No proposal for data without labels.
  expect_identical(sieve_label_bounds_of(data.frame(a = 1:3), "a")$a, c(NA_real_, NA_real_))
  expect_identical(sieve_label_bounds_of(NULL, "a")$a, c(NA_real_, NA_real_))
})

# ---- 3. The declaration is saved with the project -----------------------------

test_that("declared bounds survive a project's save, read and restore", {
  st <- ip_hs9()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    # The Response bounds row above the grid, which declares bounds for
    # every included item.
    expect_true(isTRUE(state$confirmed))
    session$setInputs(types_scope = "all", bounds_lo = 0, bounds_hi = 400)
    session$setInputs(bounds_apply = 1L)
    expect_match(ip_text(output$grid_note), "Response bounds 0 to 400 declared for 9 items", fixed = TRUE)
    # Declaring reverses nothing, so the confirmation stands.
    expect_true(isTRUE(state$confirmed))
  })
  saved <- shiny::isolate(st$meta)
  expect_true(all(saved$rev_bounds == "declared"))
  expect_true(all(saved$rev_lo == 0 & saved$rev_hi == 400))
  path <- ip_path("declared.siever.rds")
  SIEVER:::sieve_project_write(SIEVER:::sieve_project_snapshot(st), path)
  snap <- SIEVER:::sieve_project_read(path)
  other <- sieve_new_state()
  SIEVER:::sieve_project_restore(other, snap, source = basename(path))
  back <- shiny::isolate(other$meta)
  expect_identical(back[, c("item", "rev_lo", "rev_hi", "rev_bounds")], saved[, c("item", "rev_lo", "rev_hi", "rev_bounds")])
  # The declaration's step is in the restored script, and a reversal in the
  # restored session reflects within the declared bounds.
  expect_true(any(grepl("Declare the response bounds 0 to 400 for 9 items", shiny::isolate(other$codelog), fixed = TRUE)))
  loaded <- shiny::isolate(other$items$t2_cubes)
  shiny::isolate(SIEVER:::sieve_apply_reverse(other, "t2_cubes", origin = "ingest"))
  expect_identical(as.numeric(shiny::isolate(other$items$t2_cubes)), 400 - as.numeric(loaded))
  expect_identical(shiny::isolate(other$transforms)[[1]]$bounds, "declared")
})

# ---- 4. The reverse coding in the grid, the wording checks on Content Validity ----

test_that("the reverse coding is in the item grid and the wording checks on the Content Validity tab, with the same records and the same logged steps", {
  html <- as.character(mod_ingest_ui("ingest"))
  # The Ingest tab has no Prepare items panel; the grid holds the
  # declaration and the reverse coding of a set of items in its Response
  # bounds and Reverse? rows, the note line says what they did, and the
  # Ledger summary panel holds the keying balance.
  expect_false(grepl('data-value="prepare"', html, fixed = TRUE))
  expect_false(grepl("Prepare items", html, fixed = TRUE))
  for (id in c(
    "bounds_lo", "bounds_hi", "types_scope", "bounds_apply", "bounds_clear", "bounds_labels",
    "keying_to", "keying_apply", "grid_note", "reverse_balance_tbl"
  )) {
    expect_match(html, sprintf('id="ingest-%s"', id), fixed = TRUE, info = id)
  }
  content_html <- as.character(mod_content_ui("content"))
  expect_match(content_html, 'id="content-wording_tbl"', fixed = TRUE)
  expect_match(content_html, "Item Writing Checks", fixed = TRUE)
  expect_match(ip_text(content_html), "Deterministic Item Writing Checks", fixed = TRUE)
  # The help under the grid names the cells and the row, and no panel or
  # tab elsewhere.
  text <- ip_text(html)
  expect_match(text, "Type an item's bounds in its Response bounds cell (as 1 to 7; clear the cell to take them back)", fixed = TRUE)
  expect_match(text, "or for the items of one construct or facet, under Response bounds and Reverse? for several items at once, above the grid; declaring reverses nothing", fixed = TRUE)
  expect_match(html, "<summary>Response bounds and Reverse? for several items at once</summary>", fixed = TRUE)
  expect_match(text, "the reflection uses the same bounds both ways, so it returns exactly the original values", fixed = TRUE)
  expect_false(grepl("More > AI Analysis", text, fixed = TRUE))
  expect_false(grepl("Reverse coding (local)", text, fixed = TRUE))
  # Why the observed range is not the scale (the grid's help).
  expect_match(text, "The observed range is not evidence of the instrument's endpoints", fixed = TRUE)
  # What the bounds do after a reversal is taken back, what the value
  # labels do at load, and the reversal that arrived; the Content Validity
  # tab scopes its DMAR statement to the
  # ratings.
  expect_match(text, "returning it to its scoring (Reverse? No) uses them, and a later reversal uses them again unless bounds are declared for it, which replaces them", fixed = TRUE)
  expect_false(grepl("stay with the item until its Reverse? is set to No", text, fixed = TRUE))
  expect_match(text, "the lowest and the highest labeled values are declared as the item's bounds at load", fixed = TRUE)
  expect_match(text, "when every item in scope has the same, the button under Response bounds and Reverse? for several items at once enters them in the two fields", fixed = TRUE)
  expect_match(text, "with none recorded, Reverse? No is refused until the scale's bounds are typed in its Response bounds cell, where they can be corrected", fixed = TRUE)
  content_text <- ip_text(content_html)
  expect_false(grepl("All statistics on this tab come from DMAR::content_validity_index()", content_text, fixed = TRUE))
  expect_match(content_text, "the Item Writing Checks sub-tab reads the item wording with SIEVER::sieve_wording_checks(), not the ratings", fixed = TRUE)
  expect_match(text, "a point no respondent chose (an endpoint of a skewed item, any point in a small sample) leaves it narrower than the scale", fixed = TRUE)

  # Driven through the Ingest server: the reversal, its record (named
  # "ingest", the grid's origin) and its logged R.
  st <- ip_hs9()
  fx <- sieve_hs9_fixture()
  it <- "t10_addition"
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    # The Ingest tab computes no wording checks and logs none.
    expect_false(any(grepl("Wording checks", state$codelog, fixed = TRUE)))
    session$setInputs(cell_edit = list(item = it, field = "reverse", value = "Yes"))
    loaded <- as.numeric(fx$data[[it]])
    expect_identical(as.numeric(state$items[[it]]), (30 + 171) - loaded)
    rec <- state$transforms$T1
    expect_identical(rec$item, it)
    expect_identical(rec$origin, "ingest")
    expect_identical(rec$bounds, "observed")
    log <- paste(state$codelog, collapse = "\n")
    expect_match(log, 'items[["t10_addition"]] <- (30 + 171) - items[["t10_addition"]]', fixed = TRUE)
    expect_match(log, "Reverse score t10_addition within 30 to 171 (the observed range, because no response bounds were declared)", fixed = TRUE)
    # The note says the observed range was used and where to declare bounds.
    note <- ip_text(output$grid_note)
    expect_match(note, "t10_addition is now reverse scored within its observed range, 30 to 171", fixed = TRUE)
    expect_match(note, "declare the scale bounds in the item's Response bounds cell on the Ingest tab", fixed = TRUE)
    expect_false(isTRUE(state$confirmed))
    expect_false(any(grepl("Wording checks", state$codelog, fixed = TRUE)))
    # The keying balance, on the Ledger summary panel.
    expect_match(ip_html(output$reverse_balance_tbl), "Reverse keyed", fixed = TRUE)
    # Reverse? No returns the values as loaded exactly, and is logged as the
    # undo it is.
    session$setInputs(cell_edit = list(item = it, field = "reverse", value = "No"))
    expect_identical(state$items[[it]], fx$data[[it]])
    expect_false(isTRUE(state$meta$reverse[state$meta$item == it]))
    expect_match(paste(state$codelog, collapse = "\n"), "Undo the reverse scoring of t10_addition", fixed = TRUE)
  })

  # The tables drawn (the data each DT output is built from, since a
  # server side table does not carry its rows in the output) and the rows
  # pushed to the grid in place, counted on this test's frame.
  counts <- new.env(parent = emptyenv())
  counts$msg <- 0L
  counts$messages <- 0L
  counts$frames <- list()
  real_dt <- sieve_dt
  testthat::local_mocked_bindings(
    sieve_dt = function(data, ...) {
      counts$frames[[length(counts$frames) + 1L]] <- data
      real_dt(data, ...)
    },
    sieve_session_message = function(session, type, message) {
      if (identical(type, "sieve_ingest_row")) {
        counts$messages <- counts$messages + 1L
        counts$msg <- counts$msg + length(message$rows)
        counts$last <- message$rows[[length(message$rows)]]
      }
      invisible(NULL)
    },
    .package = "SIEVER"
  )
  drawn_with <- function(col) Filter(function(df) col %in% names(df), counts$frames)

  # The wording checks, on the Content Validity tab: computed from the
  # ledger while the types are not confirmed (the reversal above withdrew
  # the confirmation), and logged once with the same step and the same
  # call.
  expect_false(isTRUE(shiny::isolate(st$confirmed)))
  shiny::testServer(mod_content_server, args = list(state = st), {
    expect_false(any(grepl("Wording checks", state$codelog, fixed = TRUE)))
    counts$frames <- list()
    expect_match(ip_html(output$wording_tbl), "Double-barreled", fixed = TRUE)
    checks <- drawn_with("DoubleBarreled")
    expect_length(checks, 1L)
    expect_gt(nrow(checks[[1]]), 0L)
    expect_true(all(checks[[1]]$Item %in% state$meta$item[state$meta$status == "active"]))
    # One line per row: the Item Name and the markers
    # found are cut with an ellipsis, with the full text on hover.
    expect_true(all(grepl("text-overflow:ellipsis;white-space:nowrap", checks[[1]]$ItemName, fixed = TRUE)))
    expect_true(all(grepl('<span title="', checks[[1]]$MarkersFound, fixed = TRUE)))
    expect_identical(sum(grepl("Wording checks", state$codelog, fixed = TRUE)), 1L)
    log <- paste(state$codelog, collapse = "\n")
    expect_match(
      log,
      "Wording checks: deterministic item-writing screens (length, reading grade, double-barreled, negations, absolute and vague quantifiers; Bandalos, 2018).",
      fixed = TRUE
    )
    expect_match(log, "wording_checks <- SIEVER::sieve_wording_checks(wording_meta)", fixed = TRUE)
    invisible(output$wording_tbl)
    expect_identical(sum(grepl("Wording checks", state$codelog, fixed = TRUE)), 1L)
  })
  # With no study loaded the tab says where to load one, and logs nothing.
  empty <- sieve_new_state()
  shiny::testServer(mod_content_server, args = list(state = empty), {
    counts$frames <- list()
    invisible(output$wording_tbl)
    note <- drawn_with("Note")
    expect_length(note, 1L)
    expect_identical(note[[1]]$Note, "Load a study on the Ingest tab to check the wording of its items.")
    expect_false(any(grepl("Wording checks", state$codelog, fixed = TRUE)))
  })

  # The grid's rows are pushed for a reversal made from the Reverse? row,
  # all in one message, and the grid
  # is not drawn again, as for a reversal from an item's own cell. A
  # declaration in an item's cell pushes its row too (its Type note reads
  # the declared bounds); one from the Response bounds row is shown by
  # drawing the grid again.
  grid_drawn <- function() length(Filter(function(df) all(c("Bounds", "Distribution") %in% names(df)), counts$frames))
  st <- ip_hs9()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    invisible(output$type_grid)
    # A reversal from an item's own cell: one row in one message, no grid.
    counts$msg <- 0L
    counts$messages <- 0L
    counts$frames <- list()
    session$setInputs(cell_edit = list(item = "t1_visual_perception", field = "reverse", value = "Yes"))
    expect_identical(counts$msg, 1L)
    expect_identical(counts$messages, 1L)
    expect_identical(grid_drawn(), 0L)
    by_cell <- lapply(counts$frames, names)
    # The Reverse? row for the three Speed tests: their three rows in one
    # message (drawn once in the browser), no
    # grid, and the same tables redrawn as for the cell.
    counts$msg <- 0L
    counts$messages <- 0L
    counts$frames <- list()
    session$setInputs(types_scope = "construct:Speed", keying_to = "Yes")
    session$setInputs(keying_apply = 1L)
    expect_true(all(state$meta$reverse[state$meta$construct == "Speed"]))
    expect_identical(counts$msg, 3L)
    expect_identical(counts$messages, 1L)
    expect_identical(grid_drawn(), 0L)
    expect_identical(lapply(counts$frames, names), by_cell)
    # A declaration in t2_cubes's cell: one row message that shows it, and
    # no grid (a redraw would take a value being typed in another cell and
    # the keyboard focus).
    counts$msg <- 0L
    counts$frames <- list()
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "bounds", value = "0 to 400"))
    expect_identical(counts$msg, 1L)
    expect_identical(grid_drawn(), 0L)
    expect_identical(counts$last$key, "t2_cubes")
    expect_match(ip_pushed_cell(counts$last, "Bounds"), 'data-field="bounds" value="0 to 400"', fixed = TRUE)
  })
})

# ---- 5. The local tools in their places --------------------------------------

test_that("the Ingest UI has no separate preparation panel, and the live Ingest and wording tools are available", {
  # No identifier of a Prepare items panel is in the Ingest UI, and the
  # package has no such module and no bounds text of its own; the value
  # labels' reader and the transformation service are there.
  ingest_html <- as.character(mod_ingest_ui("ingest"))
  expect_false(grepl("ingest-prepare", ingest_html, fixed = TRUE))
  for (id in c(
    "scale_min", "scale_max", "reverse", "restore", "items", "reverse_note",
    "wording_tbl", "reverse_balance_tbl", "declare", "declare_note", "scope",
    "label_proposal", "use_labels"
  )) {
    expect_false(grepl(sprintf('id="ingest-prepare-%s"', id), ingest_html, fixed = TRUE), info = id)
  }
  ns <- asNamespace("SIEVER")
  for (f in c("mod_ingest_prepare_ui", "mod_ingest_prepare_server", "sieve_bounds_text")) {
    expect_false(exists(f, envir = ns, inherits = FALSE), info = f)
  }
  for (f in c("sieve_label_bounds_of", "sieve_apply_reverse", "sieve_parse_bounds", "sieve_clear_bounds")) {
    expect_true(exists(f, envir = ns, inherits = FALSE), info = f)
  }
  # The package documentation and the prose point to Ingest for the local
  # tools, and no code line (comments aside) names a Prepare items panel
  # or its identifiers.
  for (f in c("R/SIEVER-package.R", "R/mod_ingest.R", "R/utils_detect.R", "R/fct_transform.R", "R/mod_content.R")) {
    path <- testthat::test_path("..", "..", f)
    skip_if_not(file.exists(path))
    lines <- readLines(path, warn = FALSE)
    src <- paste(lines, collapse = "\n")
    expect_false(grepl("More > AI Analysis", src, fixed = TRUE), info = f)
    expect_false(grepl("reverse coding panel of the AI tab", src, fixed = TRUE), info = f)
    code <- lines[!grepl("^\\s*#", lines)]
    expect_false(any(grepl("Prepare items|ingest-prepare|mod_ingest_prepare|\"prepare\"", code)), info = f)
  }
})

# ---- 6. Typed bounds, items that arrived reverse scored, and redraws ---------

test_that("typed bounds are read with a space as the separator before a hyphen", {
  expect_identical(sieve_parse_bounds("-3 -1")[c("lo", "hi")], list(lo = -3, hi = -1))
  expect_identical(sieve_parse_bounds("-3 - -1")[c("lo", "hi")], list(lo = -3, hi = -1))
  expect_identical(sieve_parse_bounds("-3 to -1")[c("lo", "hi")], list(lo = -3, hi = -1))
  expect_identical(sieve_parse_bounds("-3, -1")[c("lo", "hi")], list(lo = -3, hi = -1))
  expect_identical(sieve_parse_bounds("1-7")[c("lo", "hi")], list(lo = 1, hi = 7))
  expect_identical(sieve_parse_bounds("1 - 7")[c("lo", "hi")], list(lo = 1, hi = 7))
  expect_identical(sieve_parse_bounds("-3-3")[c("lo", "hi")], list(lo = -3, hi = 3))
  expect_identical(sieve_parse_bounds("1 7")[c("lo", "hi")], list(lo = 1, hi = 7))
  expect_identical(sieve_parse_bounds("0,4")[c("lo", "hi")], list(lo = 0, hi = 4))
  expect_identical(sieve_parse_bounds("1 \u2013 5")[c("lo", "hi")], list(lo = 1, hi = 5))
  # Ambiguous or reversed: refused rather than guessed.
  expect_match(sieve_parse_bounds("0 -2")$problem, "must be below the highest", fixed = TRUE)
  expect_match(sieve_parse_bounds("1 -7")$problem, "must be below the highest", fixed = TRUE)
  # The refusal says how the text was read, since the user typed 1 and 7.
  expect_match(sieve_parse_bounds("1 -7")$problem, "\"1 -7\" reads as 1 to -7.", fixed = TRUE)
  expect_true(sieve_parse_bounds("  ")$empty)
  # Pasted typography: a no break space, and the minus sign.
  expect_identical(sieve_parse_bounds("1\u00a0to\u00a07")[c("lo", "hi")], list(lo = 1, hi = 7))
  expect_identical(sieve_parse_bounds("\u22123 to 3")[c("lo", "hi")], list(lo = -3, hi = 3))
})

test_that("an item that arrived reverse scored is returned within bounds typed in its cell, which can be corrected, and Reverse? No waits for them", {
  # The values administered on 1 to 7 use only 1 to 5, and q1 arrived
  # reflected within 1 to 7 (8 - x, observed 3 to 7), recorded as reverse
  # scored with no bounds (sieve_record_already_reversed(), as the Ingest
  # answer, a specification or the description rows record it). Within its
  # observed range Reverse? No would give 10 - y = x + 2; it is refused
  # until the scale's bounds are typed, and then gives 8 - y = x.
  set.seed(1)
  base <- as.data.frame(lapply(1:9, function(j) sample(rep_len(1:5, 40))))
  names(base) <- paste0("q", 1:9)
  d <- base
  d$q1 <- 8 - base$q1
  path <- ip_path("arrived.csv")
  utils::write.csv(d, path, row.names = FALSE)
  pushed <- list()
  testthat::local_mocked_bindings(
    sieve_session_message = function(session, type, message) {
      # One message carries every row an edit or an action changed.
      if (identical(type, "sieve_ingest_row")) for (row in message$rows) pushed[[length(pushed) + 1L]] <<- row
      invisible(NULL)
    },
    .package = "SIEVER"
  )
  last_push <- function(item) {
    hit <- Filter(function(msg) identical(msg$key, item), pushed)
    if (length(hit)) hit[[length(hit)]] else NULL
  }
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "arrived.csv", datapath = path))
    session$setInputs(load = 1L)
    expect_identical(sieve_record_already_reversed(state, "q1")$recorded, "q1")
    session$flushReact()
    m <- sieve_meta_keying(state$meta)
    expect_true(m$reverse[m$item == "q1"])
    expect_true(is.na(m$rev_lo[m$item == "q1"]))
    # The keying observer pushes q1's row: the field stays, with the line
    # that asks for the scale's bounds.
    cell <- ip_pushed_cell(last_push("q1"), "Bounds")
    expect_match(cell, 'data-item="q1" data-field="bounds" value=""', fixed = TRUE)
    expect_match(cell, "reverse scored as loaded with no bounds recorded: type the scale's bounds before setting Reverse? to No", fixed = TRUE)
    # Reverse? No is refused, from the cell and from the row for several
    # items, and nothing changes.
    n_transforms <- length(state$transforms)
    session$setInputs(cell_edit = list(item = "q1", field = "reverse", value = "No"))
    expect_identical(as.numeric(state$items$q1), as.numeric(d$q1))
    expect_length(state$transforms, n_transforms)
    expect_match(ip_text(output$grid_note), "the response bounds of its reversal are not recorded", fixed = TRUE)
    expect_match(ip_pushed_cell(last_push("q1"), "Reverse"), '<option value="Yes" selected>', fixed = TRUE)
    session$setInputs(types_scope = "all", keying_to = "No")
    session$setInputs(keying_apply = 1L)
    expect_identical(as.numeric(state$items$q1), as.numeric(d$q1))
    expect_length(state$transforms, n_transforms)
    # A wrong declaration ("1 to 8") is taken, as the bounds of that
    # reversal, and can be corrected while the values are as loaded.
    session$setInputs(cell_edit = list(item = "q1", field = "bounds", value = "1 to 8"))
    m <- sieve_meta_keying(state$meta)
    i <- match("q1", m$item)
    expect_identical(c(m$rev_lo[i], m$rev_hi[i]), c(1, 8))
    expect_match(ip_text(output$grid_note),
      "q1 is reverse scored as loaded, so these are now the bounds of that reversal: Reverse? No reflects within them.",
      fixed = TRUE
    )
    expect_true(any(grepl("# For q1, reverse scored as loaded, these are the bounds of", state$codelog, fixed = TRUE)))
    expect_true(any(grepl("# its reversal: returning it to its scoring reflects within them.", state$codelog, fixed = TRUE)))
    cell <- ip_pushed_cell(last_push("q1"), "Bounds")
    expect_match(cell, 'data-field="bounds" value="1 to 8"', fixed = TRUE)
    expect_match(cell, "reverse scored as loaded: Reverse? No reflects within these bounds; correct them here if they are not the scale's", fixed = TRUE)
    session$setInputs(cell_edit = list(item = "q1", field = "bounds", value = "1 to 7"))
    m <- sieve_meta_keying(state$meta)
    expect_identical(c(m$rev_lo[i], m$rev_hi[i]), c(1, 7))
    expect_identical(m$rev_bounds[i], "declared")
    expect_true(m$reverse[i])
    expect_identical(as.numeric(state$items$q1), as.numeric(d$q1)) # declaring reverses nothing
    # Reverse? No reflects within 1 to 7 and returns the administered values.
    session$setInputs(cell_edit = list(item = "q1", field = "reverse", value = "No"))
    expect_identical(as.numeric(state$items$q1), as.numeric(base$q1))
    # An item reversed in this session holds the bounds of its reflection: a
    # declaration and a clearing are both refused until it is taken back.
    session$setInputs(cell_edit = list(item = "q2", field = "bounds", value = "1 to 5"))
    session$setInputs(cell_edit = list(item = "q2", field = "reverse", value = "Yes"))
    expect_identical(as.numeric(state$items$q2), 6 - as.numeric(base$q2))
    session$setInputs(cell_edit = list(item = "q2", field = "bounds", value = "0 to 9"))
    expect_match(ip_text(output$grid_note), "q2 is reverse scored already and keeps the bounds of that reversal", fixed = TRUE)
    session$setInputs(cell_edit = list(item = "q2", field = "bounds", value = ""))
    expect_match(ip_text(output$grid_note), "q2 is reverse scored and keeps the bounds of that reversal", fixed = TRUE)
    m <- sieve_meta_keying(state$meta)
    expect_identical(c(m$rev_lo[m$item == "q2"], m$rev_hi[m$item == "q2"]), c(1, 5))
    expect_match(ip_pushed_cell(last_push("q2"), "Bounds"), '<div class="small">1 to 5</div>', fixed = TRUE)
  })
  # The rule itself: a reflection made in this session holds its bounds; a
  # reversal as loaded holds the bounds its keying record states, and not
  # bounds stored since over a record that gave none; without the data as
  # loaded or the records, a reverse scored item with stored bounds is
  # taken to hold them.
  m <- data.frame(
    item = c("a", "b", "c", "e"), reverse = c(TRUE, TRUE, FALSE, TRUE), rev_lo = c(1, NA, 1, 1), rev_hi = c(7, NA, 7, 7),
    rev_bounds = c("declared", NA, "declared", "declared"), stringsAsFactors = FALSE
  )
  raw <- data.frame(a = c(1, 2, 7), b = c(1, 2, 3), c = c(1, 2, 3), e = c(3, 4, 5))
  now <- data.frame(a = c(7, 6, 1), b = c(1, 2, 3), c = c(1, 2, 3), e = c(3, 4, 5))
  keying <- function(lo, hi) list(type = "keying", item = "e", lo = lo, hi = hi)
  expect_identical(sieve_holds_reversal_bounds(m, m$item, now, raw, list(keying(NA_real_, NA_real_))), c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(sieve_holds_reversal_bounds(m, m$item, now, raw, list(keying(1, 7))), c(TRUE, FALSE, FALSE, TRUE))
  expect_identical(sieve_holds_reversal_bounds(m, m$item, now, raw, list()), c(TRUE, FALSE, FALSE, TRUE))
  expect_identical(sieve_holds_reversal_bounds(m, m$item), c(TRUE, FALSE, FALSE, TRUE))
  # The service refuses the unbounded return itself, and takes bounds given.
  meta <- sieve_build_meta(base, names(base))
  meta <- sieve_meta_keying(meta)
  meta$reverse[meta$item == "q1"] <- TRUE
  expect_error(sieve_reverse_item(d, meta, "q1"), class = "sieve_transform_error")
  expect_identical(as.numeric(sieve_reverse_item(d, meta, "q1", lo = 1, hi = 7)$data$q1), as.numeric(base$q1))
  # Several items at once are named in agreement.
  st2 <- sieve_new_state()
  shiny::isolate({
    st2$items <- base
    st2$raw <- base
    mm <- sieve_meta_keying(sieve_build_meta(base, names(base)))
    mm$reverse[mm$item %in% c("q2", "q3")] <- TRUE
    st2$meta <- mm
  })
  out <- sieve_declare_bounds(st2, 1, 7, items = c("q1", "q2", "q3"))
  expect_match(out$text, "q2, q3 are reverse scored as loaded, so these are now the bounds of their reversals", fixed = TRUE)
  expect_true(any(grepl("# their reversals: returning them to their scoring reflects within them.", shiny::isolate(st2$codelog), fixed = TRUE)))
})

test_that("a reverse scored item whose observed range differs keeps the range warning in its Response bounds cell", {
  set.seed(1)
  d <- as.data.frame(lapply(1:8, function(j) sample(rep_len(1:5, 40))))
  names(d) <- paste0("q", 1:8)
  d$q9 <- sample(rep_len(2:5, 40))
  path <- ip_path("differ.csv")
  utils::write.csv(d, path, row.names = FALSE)
  pushed <- list()
  testthat::local_mocked_bindings(
    sieve_session_message = function(session, type, message) {
      # One message carries every row an edit or an action changed.
      if (identical(type, "sieve_ingest_row")) for (row in message$rows) pushed[[length(pushed) + 1L]] <<- row
      invisible(NULL)
    },
    .package = "SIEVER"
  )
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "differ.csv", datapath = path))
    session$setInputs(load = 1L)
    grid <- ip_grid(output$type_grid)
    expect_match(ip_cell(grid, "q9", "Bounds"),
      "observed 2 to 5; the study's common range is 1 to 5 and no bounds are declared</div>",
      fixed = TRUE
    )
    expect_match(ip_text(output$bounds_note), "Open to read why and where to declare bounds.", fixed = TRUE)
    expect_false(grepl("every item below", ip_text(output$bounds_note), fixed = TRUE))
    # Reversed within its observed range 2 to 5, its values still span 2 to
    # 5, so the line above the grid still names it, and its cell (now the
    # bounds of the reversal as text) keeps the warning, with what to do.
    session$setInputs(cell_edit = list(item = "q9", field = "reverse", value = "Yes"))
    expect_true("q9" %in% sieve_range_check(state$meta)$differing$item)
    hit <- Filter(function(msg) identical(msg$key, "q9"), pushed)
    cell <- ip_pushed_cell(hit[[length(hit)]], "Bounds")
    expect_match(cell, "the bounds of its reversal, the observed range (none were declared)", fixed = TRUE)
    expect_match(cell,
      "the study's common range is 1 to 5 and no bounds are declared: set Reverse? to No, declare its bounds, and reverse it again",
      fixed = TRUE
    )
    m <- sieve_meta_keying(state$meta)
    built <- sieve_bounds_cell("x", m, match("q9", m$item), check = sieve_range_check(m))
    expect_match(built, "declare its bounds, and reverse it again", fixed = TRUE)
  })
})

test_that("the Load and replace dialog names bounds cleared apart from bounds declared, and not observed bounds a reversal left", {
  meta <- function(lo, hi, kind) {
    data.frame(
      item = c("q1", "q2", "q3", "q4", "q5"), construct = "A", facet = NA_character_, type = "continuous",
      status = "active", reverse = FALSE, rev_lo = lo, rev_hi = hi, rev_bounds = kind,
      stringsAsFactors = FALSE
    )
  }
  # q4: a reversal taken back left its observed range stored; q5: a
  # declaration replaced by such observed bounds.
  then <- list(
    meta = meta(c(1, NA, 1, NA, 1), c(7, NA, 7, NA, 7), c("declared", NA, "declared", NA, "declared")),
    roles = NULL, rows = 10, data_key = "k"
  )
  now <- list(
    meta = meta(c(NA, 1, 0, 9, 2), c(NA, 7, 10, 37, 6), c(NA, "declared", "declared", "observed", "observed")),
    roles = NULL, rows = 10, data_key = "k"
  )
  said <- sieve_load_discards(then, now)
  expect_true("the declared response bounds of q1, q5 cleared" %in% said)
  expect_true("response bounds declared for q2, q3" %in% said)
  expect_false(any(grepl("declared for q1", said, fixed = TRUE)))
  expect_false(any(grepl("q4", said, fixed = TRUE)))
  # In the app: a reversal and its return leave only observed bounds, which
  # a Load loses nothing by dropping, so the dialog names nothing.
  st <- ip_hs9()
  then <- list(meta = shiny::isolate(st$meta), roles = NULL, rows = nrow(shiny::isolate(st$items)), data_key = "k")
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "Yes"))
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "No"))
    expect_identical(state$meta$rev_bounds[state$meta$item == "t2_cubes"], "observed")
  })
  now <- list(meta = shiny::isolate(st$meta), roles = NULL, rows = nrow(shiny::isolate(st$items)), data_key = "k")
  expect_identical(sieve_load_discards(then, now), character(0))
})

test_that("the value labels' note says which items in scope have labels and whether they agree", {
  skip_if_not_installed("haven")
  d <- ip_narrow()
  for (v in paste0("q", 1:4)) d[[v]] <- haven::labelled(as.double(d[[v]]), labels = stats::setNames(1:7, paste("point", 1:7)))
  d$q5 <- haven::labelled(as.double(d$q5), labels = stats::setNames(0:10, paste("point", 0:10)))
  for (v in paste0("q", 6:9)) d[[v]] <- as.double(d[[v]])
  path <- ip_path("mixed.sav")
  haven::write_sav(d, path)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "mixed.sav", datapath = path))
    session$setInputs(load = 1L)
    # Every item: the labelled items disagree (1 to 7 against 0 to 10).
    session$setInputs(types_scope = "all")
    said <- ip_text(output$bounds_labels)
    expect_match(said, "the file's value labels give different bounds across these items; 5 of the 9 have labels that give their bounds, shown in each item's Response bounds cell", fixed = TRUE)
    expect_false(grepl("bounds_use_labels", ip_html(output$bounds_labels), fixed = TRUE))
    # One labelled item and one without labels: they do not disagree, and
    # the note says so rather than that the labels differ.
    session$setInputs(cell_edit = list(item = "q1", field = "construct", value = "Pair"))
    session$setInputs(cell_edit = list(item = "q6", field = "construct", value = "Pair"))
    session$setInputs(types_scope = "construct:Pair")
    said <- ip_text(output$bounds_labels)
    expect_match(said, "the file's value labels give 1 to 7 for 1 of the 2 items in scope; the other has no value labels that give its bounds", fixed = TRUE)
    expect_false(grepl("different bounds", said, fixed = TRUE))
    expect_false(grepl("bounds_use_labels", ip_html(output$bounds_labels), fixed = TRUE))
    # A reverse scored item's cell names the labels' bounds too (the note
    # says they are shown in each cell): q1 declared 0 to 10, then reversed.
    session$setInputs(cell_edit = list(item = "q1", field = "bounds", value = "0 to 10"))
    session$setInputs(cell_edit = list(item = "q1", field = "reverse", value = "Yes"))
    m <- sieve_meta_keying(state$meta)
    cell <- sieve_bounds_cell("x", m, match("q1", m$item),
      labels = sieve_label_bounds_of(state$raw, "q1")[["q1"]],
      held = sieve_holds_reversal_bounds(m, "q1", state$items, state$raw)
    )
    expect_match(cell, "the bounds of its reversal; set Reverse? to No to change them", fixed = TRUE)
    expect_match(cell, "the file's value labels give 1 to 7", fixed = TRUE)
  })
  # Labels that do not bound an item's values (a 99 among them) give it no
  # bounds, and the note does not call it unlabeled.
  d2 <- ip_narrow(n = 30L)[, 1:6]
  for (v in names(d2)) d2[[v]] <- as.double(d2[[v]])
  d2$q6[1] <- 99
  for (v in names(d2)) d2[[v]] <- haven::labelled(d2[[v]], labels = stats::setNames(1:7, paste("point", 1:7)))
  path2 <- ip_path("uncovered.sav")
  haven::write_sav(d2, path2)
  st2 <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st2), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "uncovered.sav", datapath = path2))
    session$setInputs(load = 1L)
    session$setInputs(types_scope = "all")
    said <- ip_text(output$bounds_labels)
    expect_match(said, "for 5 of the 6 items in scope; the other has no value labels that give its bounds", fixed = TRUE)
    expect_false(grepl("the others have none", said, fixed = TRUE))
    expect_false(grepl("bounds_use_labels", ip_html(output$bounds_labels), fixed = TRUE))
  })
})

test_that("a declaration in one item's cell does not make the line above the grid say other items lie outside the declared bounds", {
  # Eight items observed 1 to 5 and q9 observed 2 to 5: q9 differs and has
  # no bounds, but 2 to 5 lies inside the 1 to 5 declared for q1.
  set.seed(1)
  d <- as.data.frame(lapply(1:8, function(j) sample(rep_len(1:5, 40))))
  names(d) <- paste0("q", 1:8)
  d$q9 <- sample(rep_len(2:5, 40))
  path <- ip_path("one-cell.csv")
  utils::write.csv(d, path, row.names = FALSE)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "one-cell.csv", datapath = path))
    session$setInputs(load = 1L)
    session$setInputs(cell_edit = list(item = "q1", field = "bounds", value = "1 to 5"))
    note <- ip_text(output$bounds_note)
    expect_false(grepl("outside the declared bounds", note, fixed = TRUE))
    expect_false(grepl("lies outside them", note, fixed = TRUE))
    expect_match(note, "Bounds are declared for 1 of the 9 included items but not for the item named here, so a reversal reflects it within its observed range", fixed = TRUE)
    expect_match(note, "but 1 item without declared bounds has a different observed range: q9 (2 to 5)", fixed = TRUE)
    expect_match(note, "bounds declared for 1 of the 9 items; 1 included item without declared bounds has an observed range other than 1 to 5", fixed = TRUE)
  })
  # A study-wide declaration that leaves an item out still says so, and
  # only of that item.
  m <- sieve_build_meta(d, names(d))
  m <- sieve_meta_keying(m)
  m$max[m$item == "q8"] <- 7
  in_scope <- !m$item %in% c("q8", "q9")
  m$rev_lo[in_scope] <- 1
  m$rev_hi[in_scope] <- 5
  m$rev_bounds[in_scope] <- "declared"
  check <- sieve_range_check(m)
  expect_identical(check$differing$item, c("q8", "q9"))
  expect_identical(check$differing$outside, c(TRUE, FALSE))
  text <- sieve_range_text(check)
  expect_match(text, "1 is not covered because its observed range lies outside them: q8 (1 to 7).", fixed = TRUE)
  expect_match(text, "but 1 other item without declared bounds has a different observed range: q9 (2 to 5).", fixed = TRUE)
  expect_identical(lengths(regmatches(text, gregexpr("Bounds are declared for", text, fixed = TRUE))), 1L)
  expect_match(sieve_range_summary(check), "1 included item lies outside the declared bounds; 1 other included item without declared bounds has an observed range other than 1 to 5", fixed = TRUE)
  # Two undeclared items differing, and no majority with a declaration:
  # the wording agrees in number and names the items it speaks of.
  m2 <- m
  m2$rev_lo[] <- NA
  m2$rev_hi[] <- NA
  m2$rev_bounds[] <- NA
  m2$rev_lo[1] <- 1
  m2$rev_hi[1] <- 5
  m2$rev_bounds[1] <- "declared"
  m2$min[m2$item == "q8"] <- 2
  m2$max[m2$item == "q8"] <- 5
  text2 <- sieve_range_text(sieve_range_check(m2))
  expect_match(text2, "but not for the items named here, so a reversal reflects each of them within its observed range", fixed = TRUE)
  expect_match(text2, "2 items without declared bounds have a different observed range", fixed = TRUE)
  m3 <- m2
  m3$max[m3$item %in% c("q2", "q3", "q4")] <- 6
  m3$min[m3$item %in% c("q5", "q6")] <- 0
  text3 <- sieve_range_text(sieve_range_check(m3))
  expect_match(text3, "no range shared by a majority", fixed = TRUE)
  expect_false(grepl("named here", text3, fixed = TRUE))
  expect_match(text3, "without declared bounds and outside the most common range:", fixed = TRUE)
})

test_that("a reversal that moves the study's common range draws the grid again, so the other items' warnings follow", {
  # qa, qb and qc observed 1 to 7, qd and qf 2 to 7, and qe1, qe2 1 to 6
  # declared 1 to 7: reversing qe1 and qe2 within 1 to 7 gives them 2 to 7,
  # which becomes the common range, so qa, qb and qc now carry the warning
  # and qd and qf no longer do.
  set.seed(1)
  spans <- list(qa = 1:7, qb = 1:7, qc = 1:7, qd = 2:7, qf = 2:7, qe1 = 1:6, qe2 = 1:6)
  d <- as.data.frame(lapply(spans, function(v) sample(rep_len(v, 60))))
  path <- ip_path("common.csv")
  utils::write.csv(d, path, row.names = FALSE)
  pushed <- list()
  testthat::local_mocked_bindings(
    sieve_session_message = function(session, type, message) {
      if (identical(type, "sieve_ingest_row")) for (row in message$rows) pushed[[length(pushed) + 1L]] <<- row
      invisible(NULL)
    },
    .package = "SIEVER"
  )
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "common.csv", datapath = path))
    session$setInputs(load = 1L)
    expect_identical(sieve_range_check(state$meta)$common, c(1, 7))
    for (it in c("qe1", "qe2")) {
      session$setInputs(cell_edit = list(item = it, field = "bounds", value = "1 to 7"))
      session$setInputs(cell_edit = list(item = it, field = "construct", value = "Negative"))
    }
    drawn_before <- output$type_grid
    grid <- ip_grid(drawn_before)
    expect_match(ip_cell(grid, "qd", "Bounds"), "the study's common range is 1 to 7", fixed = TRUE)
    pushed <<- list()
    session$setInputs(types_scope = "construct:Negative", keying_to = "Yes")
    session$setInputs(keying_apply = 1L)
    expect_identical(sieve_range_check(state$meta)$common, c(2, 7))
    grid <- ip_grid(output$type_grid)
    for (it in c("qa", "qb", "qc")) {
      expect_match(ip_cell(grid, it, "Bounds"), "observed 1 to 7; the study's common range is 2 to 7 and no bounds are declared", fixed = TRUE, info = it)
    }
    for (it in c("qd", "qf")) {
      expect_false(grepl("no bounds are declared", ip_cell(grid, it, "Bounds"), fixed = TRUE), info = it)
    }
    expect_length(pushed, 0L) # drawn again rather than pushed
  })
})

test_that("declaring stored observed bounds as the same numbers updates the line above the grid", {
  # q4's reversal and its return leave its observed range 2 to 7 stored as
  # "observed"; typing 2 to 7 in its cell changes only the kind, which the
  # keying key covers.
  set.seed(1)
  d <- as.data.frame(lapply(c(q1 = 1, q2 = 1, q3 = 1, q4 = 2), function(a) sample(rep_len(a:7, 50))))
  path <- ip_path("kind.csv")
  utils::write.csv(d, path, row.names = FALSE)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "kind.csv", datapath = path))
    session$setInputs(load = 1L)
    expect_match(ip_text(output$bounds_note), "none declared", fixed = TRUE)
    session$setInputs(cell_edit = list(item = "q4", field = "reverse", value = "Yes"))
    session$setInputs(cell_edit = list(item = "q4", field = "reverse", value = "No"))
    m <- state$meta
    expect_identical(c(m$rev_lo[m$item == "q4"], m$rev_hi[m$item == "q4"]), c(2, 7))
    expect_identical(m$rev_bounds[m$item == "q4"], "observed")
    session$setInputs(cell_edit = list(item = "q4", field = "bounds", value = "2 to 7"))
    expect_identical(state$meta$rev_bounds[state$meta$item == "q4"], "declared")
    expect_null(output$bounds_note)
  })
})

test_that("clearing bounds says a reload declares them again only when the load would", {
  skip_if_not_installed("haven")
  # q1 is labeled 1 to 7 but holds a 99 in row 5, so the load declares no
  # bounds for it; once that respondent is removed, the rows left would fit
  # the labels, but a reload brings the 99 back.
  d <- ip_narrow()
  for (v in names(d)) d[[v]] <- as.double(d[[v]])
  d$q1[5] <- 99
  for (v in names(d)) d[[v]] <- haven::labelled(d[[v]], labels = stats::setNames(1:7, paste("point", 1:7)))
  path <- ip_path("removed.sav")
  haven::write_sav(d, path)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = "removed.sav", datapath = path))
    session$setInputs(load = 1L)
    m <- state$meta
    expect_true(is.na(m$rev_bounds[m$item == "q1"]))
    expect_identical(m$rev_bounds[m$item == "q2"], "declared")
    sieve_remove_cases(state, 5L, reason = "a test")
    session$setInputs(cell_edit = list(item = "q1", field = "bounds", value = "1 to 7"))
    expect_identical(state$meta$rev_bounds[state$meta$item == "q1"], "declared")
    session$setInputs(cell_edit = list(item = "q1", field = "bounds", value = ""))
    expect_match(ip_text(output$grid_note), "The declared response bounds of q1 are cleared", fixed = TRUE)
    expect_false(grepl("loading the file again declares", ip_text(output$grid_note), fixed = TRUE))
    session$setInputs(cell_edit = list(item = "q2", field = "bounds", value = ""))
    expect_match(ip_text(output$grid_note), "The file's value labels give the bounds of q2, so loading the file again declares them again.", fixed = TRUE)
  })
})

test_that("the wording table escapes every column but the two it clips itself", {
  seen <- new.env(parent = emptyenv())
  real_dt <- sieve_dt
  testthat::local_mocked_bindings(
    sieve_dt = function(data, ..., escape = TRUE) {
      if ("Construct" %in% names(data)) {
        seen$data <- data
        seen$escape <- escape
      }
      real_dt(data, ..., escape = escape)
    },
    .package = "SIEVER"
  )
  st <- sieve_new_state()
  shiny::isolate({
    st$meta <- data.frame(
      item = c("a", "b"), label = c("A <i>one</i>", "B"),
      construct = c("<b>X</b>", "R&D"), facet = NA_character_, type = "continuous",
      status = "active", reverse = FALSE,
      wording = c("I always never like it and hate it", "Often fine"),
      stringsAsFactors = FALSE
    )
    st$items <- data.frame(a = 1:3, b = 1:3)
  })
  shiny::testServer(mod_content_server, args = list(state = st), {
    invisible(output$wording_tbl)
  })
  expect_false(is.null(seen$data))
  cols <- names(seen$data)
  # Escaped: every column but the two clip() built and escaped itself.
  expect_setequal(seen$escape, setdiff(seq_along(cols), match(c("ItemName", "MarkersFound"), cols)))
  expect_true(match("Construct", cols) %in% seen$escape)
  # The clipped Item Name carries its text escaped once, in the span.
  expect_match(seen$data$ItemName[seen$data$Item == "a"], ">A &lt;i&gt;one&lt;/i&gt;</span>", fixed = TRUE)
})

test_that("a specification gives an item that arrived reverse scored the bounds of its reversal, and a session's reversal keeps its own", {
  # q1 arrived reflected within 1 to 7 and is recorded as reverse scored
  # with no bounds. A specification row that gives 1 to 7 stores them (as
  # its Response bounds cell would) instead of saying the reversal keeps
  # bounds it never had; Reverse? No then returns the administered values.
  set.seed(1)
  base <- as.data.frame(lapply(1:4, function(j) sample(rep_len(1:5, 40))))
  names(base) <- paste0("q", 1:4)
  d <- base
  d$q1 <- 8 - base$q1
  path <- ip_path("arrived-spec.csv")
  utils::write.csv(d, path, row.names = FALSE)
  arrived <- function() {
    st <- sieve_new_state()
    shiny::testServer(mod_ingest_server, args = list(state = st), {
      session$setInputs(
        source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
        read_rows = TRUE
      )
      session$setInputs(file = list(name = "arrived-spec.csv", datapath = path))
      session$setInputs(load = 1L)
      sieve_record_already_reversed(state, "q1")
    })
    st
  }
  st <- arrived()
  spec <- shiny::isolate(sieve_spec_table(st$raw, st$items, st$meta, st$roles))
  r <- spec$variable == "q1"
  spec$key_processed[r] <- ""
  spec$key_loaded[r] <- ""
  spec$rev_lo[r] <- "1"
  spec$rev_hi[r] <- "7"
  spec$rev_bounds[r] <- "declared"
  out <- sieve_apply_spec_table(st, spec)
  expect_true(out$ok)
  m <- shiny::isolate(st$meta)
  expect_identical(c(m$rev_lo[m$item == "q1"], m$rev_hi[m$item == "q1"]), c(1, 7))
  expect_identical(m$rev_bounds[m$item == "q1"], "declared")
  expect_false("q1" %in% out$summary$bounds_kept)
  expect_identical(as.numeric(shiny::isolate(st$items)$q1), as.numeric(d$q1))
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "q1", field = "reverse", value = "No"))
    expect_identical(as.numeric(state$items$q1), as.numeric(base$q1))
  })
  # Bounds typed for it before (a wrong 1 to 8) are the researcher's, not
  # the reversal's: the specification's 1 to 7 replace them, and a return
  # reflects within the scale.
  st <- arrived()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "q1", field = "bounds", value = "1 to 8"))
  })
  out <- sieve_apply_spec_table(st, spec)
  expect_true(out$ok)
  m <- shiny::isolate(st$meta)
  expect_identical(c(m$rev_lo[m$item == "q1"], m$rev_hi[m$item == "q1"]), c(1, 7))
  expect_false("q1" %in% out$summary$bounds_kept)
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "q1", field = "reverse", value = "No"))
    expect_identical(as.numeric(state$items$q1), as.numeric(base$q1))
  })
  # Bounds a value lies outside are refused, and nothing changes.
  st <- arrived()
  bad <- spec
  bad$rev_lo[r] <- "2"
  bad$rev_hi[r] <- "6"
  out <- sieve_apply_spec_table(st, bad)
  expect_false(out$ok)
  expect_match(paste(out$problems, collapse = " "), "outside the response bounds 2 to 6 the specification gives for it", fixed = TRUE)
  expect_true(is.na(shiny::isolate(st$meta)$rev_lo[shiny::isolate(st$meta)$item == "q1"]))
  # A reversal made in the session keeps the bounds it was made with.
  st <- arrived()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "q2", field = "bounds", value = "1 to 5"))
    session$setInputs(cell_edit = list(item = "q2", field = "reverse", value = "Yes"))
  })
  spec2 <- shiny::isolate(sieve_spec_table(st$raw, st$items, st$meta, st$roles))
  r2 <- spec2$variable == "q2"
  spec2$rev_lo[r2] <- "0"
  spec2$rev_hi[r2] <- "9"
  out2 <- sieve_apply_spec_table(st, spec2)
  expect_true(out2$ok)
  expect_true("q2" %in% out2$summary$bounds_kept)
  m <- shiny::isolate(st$meta)
  expect_identical(c(m$rev_lo[m$item == "q2"], m$rev_hi[m$item == "q2"]), c(1, 5))
})

test_that("the bounds a file states for the reversal it holds stay fixed, while bounds typed for a reversal recorded with none can be corrected or cleared", {
  skip_if_not_installed("haven")
  # Session 1 reverses q1 (administered on 1 to 5, observed 2 to 5) within
  # its observed range and exports the processed values: the label states
  # "[reverse scored within the observed range 2 to 5]". Read back, the
  # file's statement is what returns the administered values, so a
  # declaration of the scale's 1 to 5 must not replace it.
  set.seed(1)
  base <- as.data.frame(lapply(1:9, function(j) sample(rep_len(1:5, 40))))
  names(base) <- paste0("q", 1:9)
  base$q1 <- sample(rep_len(2:5, 40))
  path <- ip_path("plain.csv")
  utils::write.csv(base, path, row.names = FALSE)
  sav <- ip_path("export.sav")
  pushed <- list()
  testthat::local_mocked_bindings(
    sieve_session_message = function(session, type, message) {
      if (identical(type, "sieve_ingest_row")) for (row in message$rows) pushed[[length(pushed) + 1L]] <<- row
      invisible(NULL)
    },
    .package = "SIEVER"
  )
  last_push <- function(item) {
    hit <- Filter(function(msg) identical(msg$key, item), pushed)
    if (length(hit)) hit[[length(hit)]] else NULL
  }
  load_file <- function(session, name, datapath) {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      read_rows = TRUE
    )
    session$setInputs(file = list(name = name, datapath = datapath))
    session$setInputs(load = 1L)
  }
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    load_file(session, "plain.csv", path)
    session$setInputs(cell_edit = list(item = "q1", field = "reverse", value = "Yes"))
    haven::write_sav(sieve_export_data(state$raw, state$items, state$meta, values = "processed"), sav)
  })
  for (how in c("cell", "row")) {
    st2 <- sieve_new_state()
    shiny::testServer(mod_ingest_server, args = list(state = st2), {
      load_file(session, "export.sav", sav)
      m <- sieve_meta_keying(state$meta)
      i <- match("q1", m$item)
      expect_true(m$reverse[i])
      expect_identical(c(m$rev_lo[i], m$rev_hi[i]), c(2, 5))
      grid <- ip_grid(output$type_grid)
      expect_match(ip_cell(grid, "q1", "Bounds"), "the bounds of its reversal", fixed = TRUE, info = how)
      expect_false(grepl('data-field="bounds"', ip_cell(grid, "q1", "Bounds"), fixed = TRUE), info = how)
      if (identical(how, "cell")) {
        session$setInputs(cell_edit = list(item = "q1", field = "bounds", value = "1 to 5"))
      } else {
        session$setInputs(types_scope = "all", bounds_lo = 1, bounds_hi = 5)
        session$setInputs(bounds_apply = 1L)
      }
      expect_match(ip_text(output$grid_note), "q1 is reverse scored already and keeps the bounds of that reversal", fixed = TRUE, info = how)
      session$setInputs(cell_edit = list(item = "q1", field = "bounds", value = ""))
      expect_match(ip_text(output$grid_note), "q1 is reverse scored and keeps the bounds of that reversal", fixed = TRUE, info = how)
      m <- sieve_meta_keying(state$meta)
      expect_identical(c(m$rev_lo[i], m$rev_hi[i]), c(2, 5), info = how)
      session$setInputs(cell_edit = list(item = "q1", field = "reverse", value = "No"))
      expect_identical(as.numeric(state$items$q1), as.numeric(base$q1), info = how)
    })
  }
  # A reversal recorded with no bounds (the Ingest answer): bounds typed for
  # it are the researcher's, so clearing them is allowed and says that
  # Reverse? No is then refused, not that it reads the observed range.
  d <- base
  d$q2 <- 6 - base$q2
  path3 <- ip_path("recorded.csv")
  utils::write.csv(d, path3, row.names = FALSE)
  st3 <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st3), {
    load_file(session, "recorded.csv", path3)
    sieve_record_already_reversed(state, "q2")
    session$flushReact()
    session$setInputs(cell_edit = list(item = "q2", field = "bounds", value = "1 to 5"))
    expect_identical(state$meta$rev_bounds[state$meta$item == "q2"], "declared")
    session$setInputs(cell_edit = list(item = "q2", field = "bounds", value = ""))
    said <- ip_text(output$grid_note)
    expect_match(said, "The response bounds of q2 are cleared; it is reverse scored as loaded, so Reverse? No is refused until the scale's bounds are typed in its Response bounds cell.", fixed = TRUE)
    expect_false(grepl("reads the observed range again", said, fixed = TRUE))
    expect_true(any(grepl("# An item reverse scored as loaded is returned to its scoring only within bounds declared anew;", state$codelog, fixed = TRUE)))
    expect_match(ip_pushed_cell(last_push("q2"), "Bounds"), "reverse scored as loaded with no bounds recorded", fixed = TRUE)
    session$setInputs(cell_edit = list(item = "q2", field = "reverse", value = "No"))
    expect_match(ip_text(output$grid_note), "the response bounds of its reversal are not recorded", fixed = TRUE)
    expect_identical(as.numeric(state$items$q2), as.numeric(d$q2))
  })
  # Items reversed in this session hold their bounds, and the refusals name
  # several of them in agreement.
  st4 <- sieve_new_state()
  shiny::isolate({
    st4$raw <- base
    now <- base
    now[c("q3", "q4")] <- lapply(base[c("q3", "q4")], function(x) 6 - x)
    st4$items <- now
    mm <- sieve_meta_keying(sieve_build_meta(base, names(base)))
    at <- mm$item %in% c("q3", "q4")
    mm$reverse[at] <- TRUE
    mm$rev_lo[at] <- 1
    mm$rev_hi[at] <- 5
    mm$rev_bounds[at] <- "declared"
    st4$meta <- mm
  })
  out <- sieve_declare_bounds(st4, 0, 9, items = c("q3", "q4"))
  expect_identical(out$reversed, c("q3", "q4"))
  expect_match(out$text, "q3, q4 are reverse scored already and keep the bounds of their reversals: set Reverse? to No first to declare new bounds for them.", fixed = TRUE)
  out <- sieve_clear_bounds(st4, c("q3", "q4"))
  expect_identical(out$reversed, c("q3", "q4"))
  expect_match(out$text, "q3, q4 are reverse scored and keep the bounds of their reversals: set Reverse? to No first.", fixed = TRUE)
})

test_that("a Type edit pushes its row, so a row update sent before it cannot leave the cell showing the old type", {
  pushed <- list()
  testthat::local_mocked_bindings(
    sieve_session_message = function(session, type, message) {
      if (identical(type, "sieve_ingest_row")) for (row in message$rows) pushed[[length(pushed) + 1L]] <<- row
      invisible(NULL)
    },
    .package = "SIEVER"
  )
  st <- ip_hs9()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    drawn_before <- output$type_grid
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "reverse", value = "Yes"))
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "type", value = "ordered"))
    row <- pushed[[length(pushed)]]
    expect_identical(row$key, "t4_lozenges")
    type_cell <- ip_pushed_cell(row, "Type")
    expect_match(type_cell, '<option value="ordered" selected>', fixed = TRUE)
    expect_match(type_cell, "your choice", fixed = TRUE)
    expect_identical(output$type_grid, drawn_before)
    # A refused value pushes the type in force.
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "type", value = "nominal"))
    expect_match(ip_pushed_cell(pushed[[length(pushed)]], "Type"), '<option value="ordered" selected>', fixed = TRUE)
  })
})

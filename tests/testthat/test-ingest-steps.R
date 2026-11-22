# The Ingest tab: the four numbered steps and their gating, the three ways
# in (a SIEVER-ready
# file, a data file with its specification, a data file alone), the Confirm
# button's three states, a reverse toggle that updates one row and
# recomputes nothing else, the constructs to analyze, the roles editor, the
# ledger summary, the distribution plot's selector, and the exports (raw by
# default, processed, summary) on the Reproducible Code tab.
#
# The module tests drive the real observers with shiny::testServer. The
# redraws are counted through mocked bindings of the functions a draw calls
# (one sparkline per row, one datatable() per table) and the row messages
# through the one function that sends them. Expected ledgers and files are
# written out by hand.

skip_if_not_installed("DMAR")
skip_if_not_installed("shiny")

is_strip <- function(html) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(html, collapse = " ")))

is_path <- function(name) {
  path <- file.path(tempfile("is_"), name)
  dir.create(dirname(path))
  path
}

# A .csv whose header is followed by description rows (construct, facet and
# optionally reverse, in the order given) and then the data: the SIEVER-ready
# one-file form. The first column names the rows, so it carries nothing.
is_described_csv <- function(d, rows, name = "described.csv") {
  path <- is_path(name)
  text <- as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE)
  top <- do.call(rbind, lapply(names(rows), function(word) {
    values <- rows[[word]]
    cells <- vapply(names(d)[-1], function(v) {
      x <- values[v]
      if (is.null(x) || is.na(x)) "" else as.character(x)
    }, character(1))
    as.data.frame(as.list(stats::setNames(c(word, cells), names(d))), stringsAsFactors = FALSE)
  }))
  utils::write.csv(rbind(top, text), path, row.names = FALSE, na = "")
  path
}

# The bundled nine tests through the real load observer, as an upload (so
# the roles are a proposal and nothing confirms by itself) or as the bundled
# example (which confirms on load).
is_load_csv <- function(session, path, source = "alone", name = basename(path)) {
  session$setInputs(source = source, basis = "theoretical", palette = "nd", spec_keying = "verify")
  session$setInputs(file = list(name = name, datapath = path))
  session$setInputs(load = 1L)
}

is_counts <- function() {
  counts <- new.env(parent = emptyenv())
  counts$spark <- 0L
  counts$dt <- 0L
  counts$msg <- list()
  counts
}
is_reset <- function(counts) {
  counts$spark <- 0L
  counts$dt <- 0L
  counts$msg <- list()
  invisible(counts)
}

# ---- Plain functions ---------------------------------------------------------

test_that("description rows under the header are taken off the top, typed away, and read as construct, facet and reverse", {
  fx <- sieve_hs9_fixture()
  d <- fx$data[, c("id", fx$item_cols[1:4], "age_years")]
  rows <- list(
    construct = c(
      t1_visual_perception = "Visual", t2_cubes = "Visual", t4_lozenges = "Visual",
      t6_paragraph_comprehension = "Textual"
    ),
    facet = c(t1_visual_perception = "Shapes", t2_cubes = "Shapes"),
    reverse = c(t2_cubes = "yes", t4_lozenges = "0", t6_paragraph_comprehension = "maybe")
  )
  path <- is_described_csv(d, rows)
  # read.csv alone makes every column with a description text (so no column
  # would be proposed as an item); a column whose cells in those rows are
  # empty is typed as usual.
  as_text <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_true(all(vapply(as_text[names(rows$construct)], is.character, logical(1))))
  expect_true(is.numeric(as_text$age_years))
  found <- SIEVER:::sieve_description_rows(as_text)
  expect_true(found$found)
  expect_identical(found$description$variable, names(d)[-1])
  expect_identical(found$description$construct, c("Visual", "Visual", "Visual", "Textual", NA))
  expect_identical(found$description$facet, c("Shapes", "Shapes", NA, NA, NA))
  expect_identical(found$description$reverse, c(NA, TRUE, FALSE, NA, NA))
  expect_identical(attr(found$description, "bad_reverse"), "t6_paragraph_comprehension")
  # The data rows are what read.csv() gives for the same file without the
  # description rows: the same types and the same values.
  plain <- is_path("plain.csv")
  utils::write.csv(d, plain, row.names = FALSE)
  as_plain <- utils::read.csv(plain, stringsAsFactors = FALSE)
  expect_identical(found$data, as_plain)
  expect_identical(found$data$t2_cubes, d$t2_cubes)
  expect_identical(found$data$id, d$id)
  # sieve_read_data() does the same and records the R that does it.
  read <- SIEVER:::sieve_read_data(path, "described.csv")
  expect_true(read$ok)
  expect_identical(read$value, found$data)
  expect_identical(read$description, found$description)
  env <- new.env()
  withr::with_dir(dirname(path), eval(parse(text = read$code), envir = env))
  expect_identical(env$d, found$data)
  expect_identical(nrow(env$description), 3L)
  # A file without description rows is untouched.
  expect_false(SIEVER:::sieve_description_rows(as_plain)$found)
  expect_null(SIEVER:::sieve_read_data(plain, "plain.csv")$description)
  expect_identical(SIEVER:::sieve_read_data(plain, "plain.csv")$value, as_plain)
})

test_that("the chip's sentences and the confirmation's change phrase are written as specified", {
  roles <- data.frame(
    variable = c("id", "g1", "g2", "c1", "q1"),
    role = c("identifier", "group", "group", "criterion", "item"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    SIEVER:::sieve_not_items_text(roles),
    "4 variables are not items: 1 identifier, 2 groups, 1 criterion."
  )
  expect_identical(
    SIEVER:::sieve_not_items_text(roles[roles$role %in% c("item", "identifier"), ]),
    "1 variable is not an item: 1 identifier."
  )
  expect_identical(SIEVER:::sieve_not_items_text(roles[roles$role == "item", ]), "Every variable is an item.")
  m <- data.frame(
    item = c("a", "b", "c", "d"), construct = c("X", "X", "Y", "Y"),
    facet = c("f", "f", NA, NA), type = "ordered", status = "active",
    reverse = FALSE, stringsAsFactors = FALSE
  )
  then <- list(meta = m, data_key = "k1")
  expect_null(SIEVER:::sieve_confirm_change(then, list(meta = m, data_key = "k1")))
  now <- m
  now$reverse[1] <- TRUE
  # The phrase, and the kinds of change a caller reads to tell a change that
  # leaves the confirmation in force from one that asks for it again.
  change_of <- function(meta, key) SIEVER:::sieve_confirm_change(then, list(meta = meta, data_key = key))
  ch <- change_of(now, "k2")
  expect_identical(as.vector(ch), "a reverse scored")
  expect_identical(attr(ch, "kinds"), "reverse")
  now <- m
  now$status[3] <- "parked"
  now$facet[2] <- "g"
  ch <- change_of(now, "k1")
  expect_identical(as.vector(ch), "c excluded; b moved to another facet")
  expect_identical(attr(ch, "kinds"), c("status", "facet"))
  now <- m
  now$construct[4] <- "Z"
  now$type[2] <- "continuous"
  ch <- change_of(now, "k1")
  expect_identical(as.vector(ch), "b retyped; d reassigned")
  expect_identical(attr(ch, "kinds"), c("type", "construct"))
  ch <- change_of(m[-1, ], "k1")
  expect_identical(as.vector(ch), "a left the item pool")
  expect_identical(attr(ch, "kinds"), "pool")
  ch <- change_of(m, "k9")
  expect_identical(as.vector(ch), "the item values changed")
  expect_identical(attr(ch, "kinds"), "data")
  # What the chip says of a file's description rows: the constructs and
  # facets they gave, the keying alone when they gave no construct, and
  # nothing when they gave neither.
  m2 <- data.frame(
    item = c("a", "b", "c"), construct = c("X", "X", "Unassigned"), facet = c("f", NA, NA),
    type = "ordered", status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
  expect_identical(
    SIEVER:::sieve_described_text(m2, list(found = TRUE, reversed = character(0))),
    "Constructs and facets read from the file: 1 construct, 1 facet."
  )
  expect_identical(
    SIEVER:::sieve_described_text(m2, list(found = TRUE, reversed = "a")),
    "Constructs and facets read from the file: 1 construct, 1 facet; 1 item reverse scored as the reverse row says."
  )
  m2$construct <- "Unassigned"
  m2$facet <- NA_character_
  expect_identical(
    SIEVER:::sieve_described_text(m2, list(found = TRUE, reversed = c("a", "b"))),
    "Keying read from the file: 2 items reverse scored as the reverse row says."
  )
  expect_null(SIEVER:::sieve_described_text(m2, list(found = TRUE, reversed = character(0))))
  expect_null(SIEVER:::sieve_described_text(m2, NULL))
  # Names pasted in any form, matched without regard to case.
  found <- SIEVER:::sieve_match_names(
    "age_years, GRADE\n\"school\" nothing_here; Age_Years",
    c("id", "age_years", "grade", "school")
  )
  expect_identical(found$matched, c("age_years", "grade", "school"))
  expect_identical(found$unmatched, "nothing_here")
  # The ledger by construct and facet.
  m$reverse[1] <- TRUE
  m$status[4] <- "parked"
  s <- SIEVER:::sieve_ledger_summary(m)
  expect_identical(s$construct, c("X", "Y"))
  expect_identical(s$facet, c("f", "(no facet)"))
  expect_identical(s$n, c(2L, 2L))
  expect_identical(s$n_active, c(2L, 1L))
  expect_identical(s$n_excluded, c(0L, 1L))
  expect_identical(s$types, c("2 ordered items", "1 ordered item"))
  expect_identical(s$n_reversed, c(1L, 0L))
})

test_that("the Ingest sidebar reads as four numbered steps, the action bar holds Load, the chip and Confirm, and the type help names the estimators", {
  html <- as.character(mod_ingest_ui("ingest"))
  for (step in c("Step 1. Choose the files", "Step 2. Load", "Step 3. Review roles and types", "Step 4. Confirm")) {
    expect_match(html, step, fixed = TRUE)
  }
  # Both Load controls are unique; the main action bar keeps Load accessible
  # when the sidebar control falls below the visible window.
  expect_identical(lengths(regmatches(html, gregexpr("id=\"ingest-load\"", html, fixed = TRUE))), 1L)
  expect_identical(lengths(regmatches(html, gregexpr("id=\"ingest-load_main\"", html, fixed = TRUE))), 1L)
  expect_match(html, "id=\"ingest-confirm_button\"", fixed = TRUE)
  expect_match(html, "id=\"ingest-status_chip\"", fixed = TRUE)
  # The sidebar never collapses on this tab.
  expect_match(html, "data-open-desktop=\"always\"", fixed = TRUE)
  expect_match(html, "data-collapsible-desktop=\"false\"", fixed = TRUE)
  # The three ways in, in one control, with the bundled examples after them.
  for (choice in c("value=\"ready\"", "value=\"pair\"", "value=\"alone\"", "value=\"hs9\"", "value=\"hs\"")) {
    expect_match(html, choice, fixed = TRUE)
  }
  expect_lt(regexpr("value=\"ready\"", html, fixed = TRUE), regexpr("value=\"hs9\"", html, fixed = TRUE))
  # The exports are not on this tab.
  for (gone in c("ingest-dl_data", "ingest-dl_spec", "ingest-save_project", "ingest-export_values")) {
    expect_false(grepl(gone, html, fixed = TRUE))
  }
  # The help beside the type control.
  expect_match(html, "Ordered versus continuous", fixed = TRUE)
  expect_match(html, "polychoric (or tetrachoric) correlations", fixed = TRUE)
  expect_match(html, "WLSMV", fixed = TRUE)
  expect_match(html, "full information maximum likelihood", fixed = TRUE)
  # Reliability is named as the Reliability tab names it.
  expect_match(html, "Robust omega (observed denominator)", fixed = TRUE)
  expect_match(html, "unrestricted polychoric", fixed = TRUE)
  expect_match(html, "parallel analysis", fixed = TRUE)
  # The row message handler is on the page.
  expect_match(html, "sieve_ingest_row", fixed = TRUE)
  # The gates of the analysis tabs name the buttons as the sidebar shows them.
  gate <- is_strip(as.character(shiny::isolate(sieve_gate(sieve_new_state()))))
  expect_match(gate, "Go to Ingest and press Load in the action bar above the panels", fixed = TRUE)
  expect_false(grepl("Load data", gate, fixed = TRUE))
  # The exports are on the Reproducible Code tab, raw by default.
  repro <- as.character(mod_repro_ui("repro"))
  expect_match(repro, "id=\"repro-dl_data\"", fixed = TRUE)
  expect_match(repro, "id=\"repro-dl_summary\"", fixed = TRUE)
  expect_false(grepl("id=\"repro-save_project\"", repro, fixed = TRUE))
  expect_match(as.character(mod_project_ui("project")), "id=\"project-save\"", fixed = TRUE)
  expect_match(repro, "value=\"raw\" checked", fixed = TRUE)
  expect_match(repro, "value=\"processed\"", fixed = TRUE)
  expect_false(grepl('value="analytic"', repro, fixed = TRUE))
})

# ---- The steps and their gating -------------------------------------------------

test_that("each step is enabled when the one before it is done, and a step that is not available says why", {
  fx <- sieve_hs9_fixture()
  csv <- is_path("nine.csv")
  utils::write.csv(fx$data, csv, row.names = FALSE)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    # Nothing chosen: step 2 says what to do, steps 3 and 4 say to load first.
    session$setInputs(source = "alone", basis = "theoretical", palette = "nd")
    expect_match(is_strip(output$step2_why$html), "Choose a file first (step 1)", fixed = TRUE)
    expect_false(can_load())
    expect_match(is_strip(output$step3_why$html), "Load the data first (step 2)", fixed = TRUE)
    expect_match(is_strip(output$estimator_box$html), "Load the data first (step 2)", fixed = TRUE)
    expect_match(is_strip(output$confirm_state$html), "Load the data first (step 2)", fixed = TRUE)
    button <- as.character(output$confirm_button$html)
    expect_match(button, "Confirm the study (step 4)", fixed = TRUE)
    expect_match(button, "disabled", fixed = TRUE)
    expect_match(is_strip(output$status_chip$html), "No data loaded", fixed = TRUE)
    expect_match(is_strip(output$status_chip$html), "Step 1", fixed = TRUE)
    expect_null(output$construct_picker$html)
    # A bundled example needs no file.
    session$setInputs(source = "hs9")
    expect_true(can_load())
    expect_match(is_strip(output$step2_why$html), "Press Load to read the bundled example", fixed = TRUE)
    # A file chosen: Load is enabled and says what it will read.
    session$setInputs(source = "alone", file = list(name = "nine.csv", datapath = csv))
    expect_true(can_load())
    expect_match(is_strip(output$step2_why$html), "Press Load to read nine.csv", fixed = TRUE)
    # Loaded: step 3 is live, step 4 is enabled, the chip names step 3.
    session$setInputs(load = 1L)
    expect_false(isTRUE(state$confirmed))
    expect_match(is_strip(output$step3_why$html), "are live", fixed = TRUE)
    # The estimator label reads the resolved estimator (stated, not "ML or MLR"): ML with FIML, DMAR's
    # default, for continuous items.
    expect_match(is_strip(output$estimator_box$html), "ML with FIML", fixed = TRUE)
    button <- as.character(output$confirm_button$html)
    expect_match(button, "Confirm the study (step 4)", fixed = TRUE)
    expect_false(grepl("disabled", button, fixed = TRUE))
    chip <- is_strip(output$status_chip$html)
    expect_match(chip, "Step 3: 9 items loaded", fixed = TRUE)
    expect_match(chip, "press Confirm (step 4)", fixed = TRUE)
    expect_match(chip, "5 variables are not items: 1 identifier, 3 groups, 1 auxiliary variable.", fixed = TRUE)
    expect_match(is_strip(output$confirm_state$html), "Awaiting confirmation", fixed = TRUE)
    # Each choice of step 1 says what SIEVER will know afterwards.
    for (src in c("ready", "pair", "alone", "hs9", "hs")) {
      session$setInputs(source = src)
      expect_match(is_strip(output$source_help$html), "SIEVER knows|confirm", info = src)
    }
    session$setInputs(source = "ready")
    expect_match(is_strip(output$source_help$html), "description rows|construct, facet or reverse")
    expect_match(is_strip(output$source_help$html), "Construct__Facet__item", fixed = TRUE)
  })
})

# ---- The three ways in --------------------------------------------------------------

test_that("a SIEVER-ready .csv with description rows loads as data plus specification, says the counts, and confirms", {
  fx <- sieve_hs9_fixture()
  d <- fx$data
  rows <- list(
    construct = c(
      t1_visual_perception = "Visual", t2_cubes = "Visual", t4_lozenges = "Visual",
      t6_paragraph_comprehension = "Textual", t7_sentence = "Textual", t9_word_meaning = "Textual",
      t10_addition = "Speed", t12_counting_groups_of_dots = "Speed",
      t13_straight_and_curved_capitals = "Speed"
    ),
    facet = c(t1_visual_perception = "Shapes", t2_cubes = "Shapes", t10_addition = "Number"),
    reverse = c(t2_cubes = "yes", t4_lozenges = "no"),
    # A marked item whose values already correlate positively with its
    # construct is held rather than reversed a second time; the file says it
    # holds t2_cubes as administered, so it is reverse scored on loading.
    reverse_applied = c(t2_cubes = "no")
  )
  path <- is_described_csv(d, rows, "nine_described.csv")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    is_load_csv(session, path, source = "ready")
    expect_identical(nrow(state$meta), 9L)
    expect_identical(sieve_role_vars(state, c("identifier", "group", "criterion", "antecedent", "auxiliary")), c("id", "sex", "grade", "age_years", "school"))
    # The description rows are the ledger's constructs and facets, as a proposal.
    expect_identical(state$meta$construct, unname(rows$construct[state$meta$item]))
    expect_identical(
      state$meta$facet[match(c("t1_visual_perception", "t2_cubes", "t10_addition"), state$meta$item)],
      c("Shapes", "Shapes", "Number")
    )
    expect_true(all(is.na(state$meta$facet[!state$meta$item %in% c(
      "t1_visual_perception",
      "t2_cubes",
      "t10_addition"
    )])))
    # The columns were typed after the rows were taken off: continuous, not text.
    expect_identical(unique(state$meta$type), "continuous")
    expect_true(is.numeric(state$items$t2_cubes))
    # The reverse row: t2_cubes reverse scored within its observed range, recorded and logged.
    i <- match("t2_cubes", state$meta$item)
    expect_true(state$meta$reverse[i])
    expect_identical(state$meta$rev_bounds[i], "observed")
    expect_identical(
      as.numeric(state$items$t2_cubes),
      (min(d$t2_cubes) + max(d$t2_cubes)) - as.numeric(d$t2_cubes)
    )
    expect_false(state$meta$reverse[match("t4_lozenges", state$meta$item)])
    expect_identical(as.numeric(state$items$t4_lozenges), as.numeric(d$t4_lozenges))
    recs <- state$transforms
    expect_length(recs, 1L)
    expect_identical(recs[[1]][c("item", "type", "origin")], list(item = "t2_cubes", type = "reverse", origin = "load"))
    expect_match(paste(state$codelog, collapse = "\n"), "description rows under the header", fixed = TRUE)
    expect_match(paste(state$codelog, collapse = "\n"), "Reverse score t2_cubes within", fixed = TRUE)
    # An upload is never confirmed silently; the chip says the counts and the step.
    expect_false(isTRUE(state$confirmed))
    chip <- is_strip(output$status_chip$html)
    expect_match(chip,
      "Constructs and facets read from the file: 3 constructs, 2 facets; 1 item reverse scored as the reverse row says.",
      fixed = TRUE
    )
    expect_match(chip, "Step 3", fixed = TRUE)
    expect_match(is_strip(output$grid_note$html), "reverse row of nine_described.csv marked t2_cubes", fixed = TRUE)
    # The ledger summary reads the same structure.
    expect_match(as.character(output$ledger_tbl), "Shapes", fixed = TRUE)
    # Confirm (step 4): the button says what was confirmed.
    session$setInputs(confirm_main = 1L)
    expect_true(isTRUE(state$confirmed))
    expect_match(is_strip(output$confirm_button$html),
      "9 continuous items, 3 constructs, 2 facets",
      fixed = TRUE
    )
    expect_match(is_strip(output$status_chip$html), "9 items loaded; types confirmed (ML with FIML)", fixed = TRUE)
  })
  # The recorded R replays the load: the same data frame as the session's.
  env <- new.env()
  first <- which(startsWith(shiny::isolate(st$codelog), "# [1]"))
  code <- shiny::isolate(st$codelog)[(first + 1L):(which(startsWith(shiny::isolate(st$codelog), "# [2]")) - 1L)]
  withr::with_dir(dirname(path), eval(parse(text = code[!startsWith(code, "#")]), envir = env))
  expect_identical(env$d, shiny::isolate(st$raw))
  # The template and the pre-SIEVER step read the description rows too.
  template <- sieve_specification_template(path)
  expect_identical(template$construct[match("t7_sentence", template$variable)], "Textual")
  expect_identical(template$facet[match("t2_cubes", template$variable)], "Shapes")
  expect_identical(template$reverse[match("t2_cubes", template$variable)], "TRUE")
  expect_identical(template$reverse_applied[match("t2_cubes", template$variable)], "FALSE")
  expect_true("kind" %in% names(template))
  out <- is_path("nine_described.siever.rds")
  study <- NULL
  invisible(utils::capture.output(study <- sieve_prepare_study(path, template, file = out)))
  expect_true(study$confirmed)
  expect_identical(study$meta$construct, shiny::isolate(st$meta$construct))
  expect_identical(study$meta$reverse, shiny::isolate(st$meta$reverse))
})

test_that("a two-row-header file loads as is, confirms, and shows 303 items, 6 constructs, 27 facets", {
  aa <- siever_profile_file("aa_two_row_header.csv")
  skip_if_not(file.exists(aa), "the private two-row-header file of 303 items is not on this machine")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "ready", basis = "theoretical", palette = "nd", spec_keying = "verify")
    session$setInputs(file = list(name = "aa_two_row_header.csv", datapath = aa))
    t <- system.time(session$setInputs(load = 1L))[["elapsed"]]
    expect_identical(nrow(state$meta), 303L)
    expect_identical(sieve_role_vars(state, c("identifier", "group", "criterion", "antecedent", "auxiliary")), "response_id")
    # The items have five or more scale points, so they are proposed as
    # continuous and the study resolves to ML with FIML; the categorical
    # treatment is one Type cell, or the CFA tab's Estimator control, away.
    expect_identical(unique(state$meta$type), "continuous")
    expect_true(all(state$meta$n_levels >= 5L))
    counts <- SIEVER:::sieve_pool_counts(state$meta)
    expect_identical(length(counts$constructs), 6L)
    expect_identical(counts$n_facets, 27L)
    chip <- is_strip(output$status_chip$html)
    expect_match(chip, "Constructs and facets read from the file: 6 constructs, 27 facets.", fixed = TRUE)
    expect_match(chip, "1 variable is not an item: 1 identifier.", fixed = TRUE)
    session$setInputs(confirm_main = 1L)
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$estimator, "ML with FIML")
    expect_match(is_strip(output$confirm_button$html),
      "303 continuous items, 6 constructs, 27 facets",
      fixed = TRUE
    )
    skip_unless_timing()
    expect_lt(t, 5)
  })
})

test_that("a project file, a data file with its specification, and a data file alone end confirmed, confirmed, and at step 3", {
  fx <- sieve_hs9_fixture()
  csv <- is_path("nine.csv")
  utils::write.csv(fx$data, csv, row.names = FALSE)
  spec <- sieve_specification_template(fx$data)
  for (cn in names(fx$theory_map)) {
    spec$construct[spec$variable %in% fx$theory_map[[cn]]] <- cn
  }
  spec_file <- is_path("nine_specification.csv")
  SIEVER:::sieve_spec_write(spec, spec_file)
  project <- is_path("nine.siever.rds")
  invisible(utils::capture.output(sieve_prepare_study(csv, spec, file = project)))

  # (a) A project file: the dialog's preview, then Open this project, confirmed.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "ready", basis = "theoretical", palette = "nd")
    session$setInputs(file = list(name = "nine.siever.rds", datapath = project), load = 1L)
    expect_match(is_strip(output$project_preview$html), "nine.siever.rds holds", fixed = TRUE)
    expect_false(isTRUE(state$confirmed))
    session$setInputs(confirm_open = 1L)
    expect_true(isTRUE(state$confirmed))
    expect_match(is_strip(output$confirm_button$html),
      "9 continuous items, 3 constructs, 0 facets",
      fixed = TRUE
    )
    expect_match(is_strip(output$status_chip$html), "Confirmed from the specification: 9 continuous items, 3 constructs, 0 facets (ML with FIML). All tabs unlocked", fixed = TRUE)
  })
  # (b) The data file with its specification, in one upload: confirmed from it.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "pair", basis = "theoretical", palette = "nd", spec_keying = "verify")
    session$setInputs(file = data.frame(
      name = c("nine.csv", "nine_specification.csv"), size = 0, type = "",
      datapath = c(csv, spec_file), stringsAsFactors = FALSE
    ))
    session$setInputs(load = 1L)
    expect_true(isTRUE(state$confirmed))
    expect_match(is_strip(output$status_chip$html),
      "Confirmed from the specification: 9 continuous items, 3 constructs, 0 facets",
      fixed = TRUE
    )
    expect_match(is_strip(output$confirm_button$html),
      "Confirmed on load (step 4): 9 continuous items, 3 constructs, 0 facets",
      fixed = TRUE
    )
  })
  # (c) The data file alone: at step 3, awaiting the confirmation.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    is_load_csv(session, csv, source = "alone")
    expect_false(isTRUE(state$confirmed))
    expect_match(is_strip(output$status_chip$html), "Step 3: 9 items loaded", fixed = TRUE)
    expect_identical(unique(state$meta$construct), "Unassigned")
    expect_match(is_strip(output$confirm_button$html), "Confirm the study (step 4)", fixed = TRUE)
  })
})

# ---- The Confirm button's three states ---------------------------------------------

test_that("the Confirm button is before, confirmed, or changed since the confirmation, and a label edit keeps it confirmed", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    # The bundled example confirms on load: state two, disabled, and the
    # button says so (a green button reading only "Confirmed" would look
    # like one that had been pressed).
    expect_true(isTRUE(state$confirmed))
    button <- as.character(output$confirm_button$html)
    expect_match(is_strip(button), "Confirmed on load (step 4): 9 continuous items, 3 constructs, 0 facets", fixed = TRUE)
    expect_match(button, "btn-success", fixed = TRUE)
    expect_match(button, "disabled", fixed = TRUE)
    expect_match(is_strip(output$confirm_state$html), "Confirmed", fixed = TRUE)
    # A label edit changes nothing that matters.
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "label", value = "Cubes"))
    expect_match(is_strip(output$confirm_button$html), "Confirmed on load (step 4): 9 continuous items", fixed = TRUE)
    # A change made and then undone (reverse scored, then not): the ledger
    # and the data are as confirmed, but the confirmation was withdrawn and
    # the tabs are locked, so the button asks for it again and says why,
    # rather than naming a change that does not show.
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "Yes"))
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "No"))
    expect_false(isTRUE(state$confirmed))
    expect_identical(as.numeric(state$items$t2_cubes), as.numeric(sieve_hs9_fixture()$data$t2_cubes))
    button <- as.character(output$confirm_button$html)
    expect_match(is_strip(button),
      "Changed since confirmation (a change made and then undone): confirm again",
      fixed = TRUE
    )
    expect_match(button, "btn-warning", fixed = TRUE)
    expect_match(is_strip(output$confirm_state$html),
      "a change was made and then undone, which withdrew the confirmation",
      fixed = TRUE
    )
    session$setInputs(confirm_main = 1L)
    expect_true(isTRUE(state$confirmed))
    expect_match(is_strip(output$confirm_button$html),
      "9 continuous items, 3 constructs, 0 facets",
      fixed = TRUE
    )
    # A reversal withdraws the confirmation: state three, enabled, naming the item.
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "Yes"))
    expect_false(isTRUE(state$confirmed))
    button <- as.character(output$confirm_button$html)
    expect_match(is_strip(button), "Changed since confirmation (t2_cubes reverse scored): confirm again", fixed = TRUE)
    expect_match(button, "btn-warning", fixed = TRUE)
    expect_false(grepl("disabled", button, fixed = TRUE))
    expect_match(is_strip(output$confirm_state$html),
      "Changed since confirmation: t2_cubes reverse scored",
      fixed = TRUE
    )
    # Confirm again: state two.
    session$setInputs(confirm_main = 2L)
    expect_true(isTRUE(state$confirmed))
    expect_match(is_strip(output$confirm_button$html),
      "9 continuous items, 3 constructs, 0 facets",
      fixed = TRUE
    )
    # An exclusion is in effect at once and keeps the confirmation: the
    # button stays confirmed with the pool as it is now, and the note names
    # the exclusion as in effect, never asking to confirm again ("confirm
    # again" would read as if the exclusion still waited for something).
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "status", value = "Dropped"))
    expect_true(isTRUE(state$confirmed))
    button <- as.character(output$confirm_button$html)
    expect_match(is_strip(button), "8 continuous items, 3 constructs, 0 facets", fixed = TRUE)
    expect_false(grepl("confirm again", is_strip(button), fixed = TRUE))
    expect_match(button, "btn-success", fixed = TRUE)
    expect_match(is_strip(output$confirm_state$html),
      "Since the confirmation: t4_lozenges excluded. In effect now",
      fixed = TRUE
    )
    session$setInputs(confirm = 1L)
    expect_match(is_strip(output$confirm_button$html),
      "8 continuous items, 3 constructs, 0 facets",
      fixed = TRUE
    )
    # A type change: retyped.
    session$setInputs(cell_edit = list(item = "t7_sentence", field = "type", value = "ordered"))
    expect_match(is_strip(output$confirm_button$html),
      "Changed since confirmation (t7_sentence retyped): confirm again",
      fixed = TRUE
    )
    # Load with a study in the session asks first and changes nothing until
    # the dialog's Load and replace; then another dataset: state one again.
    session$setInputs(load = 2L)
    expect_identical(state$meta$status[state$meta$item == "t4_lozenges"], "parked")
    expect_identical(state$meta$type[state$meta$item == "t7_sentence"], "ordered")
    session$setInputs(load_replace = 1L)
    expect_identical(state$meta$status[state$meta$item == "t4_lozenges"], "active")
    expect_match(is_strip(output$confirm_button$html),
      "Confirmed on load (step 4): 9 continuous items, 3 constructs, 0 facets",
      fixed = TRUE
    )
  })
})

# ---- A reverse toggle updates one row and nothing else recomputes ---------------------

test_that("a reverse toggle sends one row to the grid, rebuilds no table, recomputes nothing else, and shows no pop-up", {
  counts <- is_counts()
  real_spark <- SIEVER:::sieve_spark_svg
  real_dt <- sieve_dt
  local_mocked_bindings(
    sieve_spark_svg = function(...) {
      counts$spark <- counts$spark + 1L
      real_spark(...)
    },
    sieve_dt = function(...) {
      counts$dt <- counts$dt + 1L
      real_dt(...)
    },
    sieve_session_message = function(session, type, message) {
      counts$msg[[length(counts$msg) + 1L]] <- list(type = type, message = message)
      invisible(NULL)
    },
    .package = "SIEVER"
  )
  shown <- 0L
  # Every notification goes through sieve_notify().
  local_mocked_bindings(sieve_notify = function(...) {
    shown <<- shown + 1L
    invisible(NULL)
  }, .package = "SIEVER")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    # Every output of the tab drawn once.
    for (o in c(
      "type_grid", "roles_grid", "summary_tbl", "aux_tbl", "ledger_tbl", "question_tbl",
      "status_chip", "confirm_button", "estimator_box", "dl_seed", "roles_notice", "grid_note"
    )) {
      invisible(output[[o]])
    }
    # Nine sparklines for the grid and two for the numeric non items (id,
    # age_years) in the aux table.
    expect_identical(counts$spark, 11L)
    is_reset(counts)
    shown <<- 0L
    d <- shiny::isolate(state$items$t2_cubes)
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "Yes"))
    # The service did its work.
    expect_true(state$meta$reverse[state$meta$item == "t2_cubes"])
    expect_identical(as.numeric(state$items$t2_cubes), (min(d) + max(d)) - as.numeric(d))
    # One row message, no sparkline but the row's own, no pop-up. The grid,
    # the roles grid, the summary table and the aux table are not rebuilt;
    # the three tables of the Ledger summary panel are, because they show
    # the reverse scored count (three datatable() calls, a few
    # milliseconds; the third is the reverse keyed items per construct).
    expect_length(counts$msg, 1L)
    expect_identical(counts$dt, 3L)
    expect_identical(counts$spark, 1L)
    expect_identical(shown, 0L)
    m <- counts$msg[[1]]
    expect_identical(m$type, "sieve_ingest_row")
    expect_identical(m$message$id, session$ns("type_grid"))
    # One message carries the rows an edit changed: here the one row.
    expect_length(m$message$rows, 1L)
    expect_identical(m$message$rows[[1]]$key, "t2_cubes")
    cols <- SIEVER:::sieve_grid_columns()
    expect_identical(m$message$key_col, match("Variable", cols) - 1L)
    cells <- m$message$rows[[1]]$cells
    # The Type cell rides with the row: its note reads the
    # declared bounds, which a Response bounds edit pushes the same way.
    expect_setequal(
      names(cells),
      as.character(match(
        c(
          "Type",
          "Mean",
          "SD",
          "Min",
          "Max",
          "Distribution",
          "Reverse",
          "Bounds"
        ),
        cols
      ) - 1L)
    )
    i <- match("t2_cubes", state$meta$item)
    expect_identical(cells[[as.character(match("Mean", cols) - 1L)]], state$meta$mean[i])
    expect_match(cells[[as.character(match("Reverse", cols) - 1L)]], "selected>Yes", fixed = TRUE)
    # The bounds of the reversal are in the Response bounds cell beside it.
    expect_match(cells[[as.character(match("Bounds", cols) - 1L)]],
      sprintf("%s to %s", min(d), max(d)),
      fixed = TRUE
    )
    expect_match(cells[[as.character(match("Bounds", cols) - 1L)]],
      "the bounds of its reversal, the observed range (none were declared)",
      fixed = TRUE
    )
    expect_match(cells[[as.character(match("Distribution", cols) - 1L)]], "<svg", fixed = TRUE)
    # The sentence is a line above the grid, not a pop-up.
    expect_match(is_strip(output$grid_note$html),
      "t2_cubes is now reverse scored within its observed range",
      fixed = TRUE
    )
    # Toggling back: one more row message, still no rebuild.
    is_reset(counts)
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "No"))
    expect_identical(as.numeric(state$items$t2_cubes), as.numeric(d))
    expect_length(counts$msg, 1L)
    expect_identical(counts$dt, 3L)
    expect_identical(counts$spark, 1L)
    expect_match(
      counts$msg[[1]]$message$rows[[1]]$cells[[as.character(match(
        "Reverse",
        cols
      ) - 1L)]],
      "selected>No",
      fixed = TRUE
    )
    # A refused toggle (a column that is not numeric cannot be reversed) resets
    # its row too, says why in the line above the grid, and changes nothing.
    is_reset(counts)
    shiny::isolate(state$items$t4_lozenges <- as.character(state$items$t4_lozenges))
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "reverse", value = "Yes"))
    expect_false(state$meta$reverse[state$meta$item == "t4_lozenges"])
    expect_length(counts$msg, 1L)
    expect_identical(counts$dt, 0L)
    expect_match(
      counts$msg[[1]]$message$rows[[1]]$cells[[as.character(match(
        "Reverse",
        cols
      ) - 1L)]],
      "selected>No",
      fixed = TRUE
    )
    expect_match(is_strip(output$grid_note$html), "cannot be reverse scored", fixed = TRUE)
    expect_identical(shown, 0L)
    # A reversal made elsewhere (the reverse coding panel) reaches the row too.
    is_reset(counts)
    shiny::isolate(SIEVER:::sieve_apply_reverse(state, "t7_sentence", lo = 0, hi = 60, origin = "ai"))
    session$setInputs(cell_edit = list(item = "t7_sentence", field = "label", value = "Sentence completion"))
    expect_length(counts$msg, 1L)
    expect_identical(counts$msg[[1]]$message$rows[[1]]$key, "t7_sentence")
    expect_identical(counts$dt, 3L)
    # An exclusion still rebuilds the grid (the row moves below the line).
    is_reset(counts)
    session$setInputs(cell_edit = list(item = "t9_word_meaning", field = "status", value = "Dropped"))
    invisible(output$type_grid)
    expect_gte(counts$dt, 1L)
    expect_gte(counts$spark, 9L)
  })
})

test_that("on a private study of 68 items a reverse toggle takes under a second and recomputes nothing else", {
  project <- siever_profile_file("ethics.siever.rds")
  skip_if_not(file.exists(project), "the private study of 68 items is not on this machine")
  requireNamespace("haven", quietly = TRUE)
  counts <- is_counts()
  real_spark <- SIEVER:::sieve_spark_svg
  real_dt <- sieve_dt
  local_mocked_bindings(
    sieve_spark_svg = function(...) {
      counts$spark <- counts$spark + 1L
      real_spark(...)
    },
    sieve_dt = function(...) {
      counts$dt <- counts$dt + 1L
      real_dt(...)
    },
    sieve_session_message = function(session, type, message) {
      counts$msg[[length(counts$msg) + 1L]] <- message
      invisible(NULL)
    },
    .package = "SIEVER"
  )
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    # The project as saved, typed ordered: the five-option checkbox of step 1
    # is turned off so that the
    # project opens with its ordered types, which is what this test times.
    session$setInputs(source = "ready", basis = "theoretical", palette = "nd", five_continuous = FALSE)
    session$setInputs(open_project = 1L)
    session$setInputs(project_file = list(name = "ethics.siever.rds", datapath = project))
    session$setInputs(confirm_open = 1L)
    expect_true(isTRUE(state$confirmed))
    for (o in c(
      "type_grid", "roles_grid", "summary_tbl", "aux_tbl", "ledger_tbl", "question_tbl",
      "status_chip", "confirm_button", "estimator_box", "dl_seed", "roles_notice"
    )) {
      invisible(output[[o]])
    }
    expect_match(is_strip(output$confirm_button$html),
      "68 ordered items, 1 construct, 6 facets",
      fixed = TRUE
    )
    is_reset(counts)
    t <- system.time(session$setInputs(cell_edit = list(
      item = "item_120",
      field = "reverse",
      value = "Yes"
    )))[["elapsed"]]
    expect_true(state$meta$reverse[state$meta$item == "item_120"])
    if (siever_timing_checks()) expect_lt(t, 1)
    # The three Ledger summary tables (reverse scored counts) and nothing else.
    expect_identical(counts$dt, 3L)
    expect_identical(counts$spark, 1L)
    expect_length(counts$msg, 1L)
    expect_identical(counts$msg[[1]]$rows[[1]]$key, "item_120")
    expect_match(is_strip(output$confirm_button$html),
      "Changed since confirmation (item_120 reverse scored): confirm again",
      fixed = TRUE
    )
    is_reset(counts)
    t <- system.time(session$setInputs(cell_edit = list(
      item = "item_120",
      field = "reverse",
      value = "No"
    )))[["elapsed"]]
    if (siever_timing_checks()) expect_lt(t, 1)
    expect_identical(counts$dt, 3L)
    expect_length(counts$msg, 1L)
  })
})

# ---- The constructs to analyze ------------------------------------------------------

test_that("the constructs to analyze include or exclude whole constructs in one recorded ledger write, and the chip and the button follow", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    picker <- is_strip(output$construct_picker$html)
    expect_match(picker, "Constructs to analyze", fixed = TRUE)
    for (cn in c("Visual (3 items)", "Textual (3 items)", "Speed (3 items)")) {
      expect_match(picker, cn, fixed = TRUE)
    }
    steps_before <- sum(startsWith(state$codelog, "# ["))
    session$setInputs(analyze_constructs = "Textual", apply_constructs = 1L)
    expect_identical(state$meta$status, ifelse(state$meta$construct == "Textual", "active", "parked"))
    expect_true(isTRUE(state$confirmed))
    expect_identical(sum(startsWith(state$codelog, "# [")), steps_before + 1L)
    chip <- is_strip(output$status_chip$html)
    expect_match(chip, "Analyzing 1 of 3 constructs: Textual (3 of 9 items).", fixed = TRUE)
    expect_match(is_strip(output$confirm_button$html), "Confirmed on load (step 4): ", fixed = TRUE)
    expect_false(grepl("confirm again", is_strip(output$confirm_button$html), fixed = TRUE))
    expect_match(is_strip(output$confirm_state$html),
      "Since the confirmation: t1_visual_perception, t2_cubes, t4_lozenges, and 3 more excluded. In effect now",
      fixed = TRUE
    )
    expect_match(is_strip(output$construct_picker$html), "Visual (3 items, 3 excluded)", fixed = TRUE)
    expect_match(is_strip(output$grid_note$html),
      "Analyzing 1 of 3 constructs: Textual (3 of 9 items included; 6 items changed status).",
      fixed = TRUE
    )
    # The recorded R makes the same change to a ledger.
    log <- state$codelog
    stamps <- which(startsWith(log, "# ["))
    code <- log[(stamps[length(stamps)] + 1L):length(log)]
    env <- new.env()
    env$meta <- data.frame(
      item = state$meta$item, construct = state$meta$construct, status = "active",
      stringsAsFactors = FALSE
    )
    eval(parse(text = code[!startsWith(code, "#") & nzchar(code)]), envir = env)
    expect_identical(env$meta$status, state$meta$status)
    # Applying the same choice again changes nothing and records nothing.
    session$setInputs(apply_constructs = 2L)
    expect_identical(sum(startsWith(state$codelog, "# [")), steps_before + 1L)
    expect_match(is_strip(output$grid_note$html), "nothing changed", fixed = TRUE)
    # Every construct back: every item included, the chip's sentence gone.
    session$setInputs(analyze_constructs = c("Visual", "Textual", "Speed"), apply_constructs = 3L)
    expect_identical(unique(state$meta$status), "active")
    expect_false(grepl("Analyzing", is_strip(output$status_chip$html), fixed = TRUE))
    # No construct ticked: refused, nothing changed.
    session$setInputs(analyze_constructs = character(0), apply_constructs = 4L)
    expect_identical(unique(state$meta$status), "active")
    expect_match(is_strip(output$grid_note$html), "At least one construct", fixed = TRUE)
  })
})

# ---- The roles editor and the roles grid ---------------------------------------------

test_that("the roles editor sets a role by pasted names, keeps a criterion's validity kind, reports unmatched names, and the grid hides columns", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    session$setInputs(
      roles_role = "criterion", roles_kind = "convergent",
      roles_names = "age_years, GRADE\nnothing_here", roles_pick = character(0),
      roles_apply = 1L
    )
    roles <- state$roles
    expect_identical(roles$role[match(c("age_years", "grade"), roles$variable)], c("criterion", "criterion"))
    expect_identical(roles$kind[match(c("age_years", "grade"), roles$variable)], c("convergent", "convergent"))
    expect_true(all(is.na(roles$kind[!roles$variable %in% c("age_years", "grade")])))
    note <- is_strip(output$roles_editor_note$html)
    expect_match(note, "Role criterion (convergent) set for 2 variables: age_years, grade (2 changed).", fixed = TRUE)
    expect_match(note, "Names that match no column: nothing_here.", fixed = TRUE)
    expect_match(paste(state$codelog, collapse = "\n"), "validity kind: \"age_years\" is convergent", fixed = TRUE)
    expect_match(is_strip(output$status_chip$html),
      "5 variables are not items: 1 identifier, 2 groups, 2 criteria.",
      fixed = TRUE
    )
    # The kind is editable in the grid and leaves with the role.
    session$setInputs(cell_edit = list(item = "grade", field = "kind", value = "discriminant"))
    expect_identical(state$roles$kind[state$roles$variable == "grade"], "discriminant")
    session$setInputs(cell_edit = list(item = "grade", field = "role", value = "Group"))
    expect_true(is.na(state$roles$kind[state$roles$variable == "grade"]))
    # A kind on a variable that is not a criterion is refused.
    session$setInputs(cell_edit = list(item = "sex", field = "kind", value = "convergent"))
    expect_true(is.na(state$roles$kind[state$roles$variable == "sex"]))
    expect_match(is_strip(output$roles_editor_note$html), "belongs to a criterion", fixed = TRUE)
    # Picked from the list, with no text: the role is set for the picks.
    session$setInputs(roles_role = "auxiliary", roles_names = "", roles_pick = "age_years", roles_apply = 2L)
    expect_identical(state$roles$role[state$roles$variable == "age_years"], "auxiliary")
    expect_true(is.na(state$roles$kind[state$roles$variable == "age_years"]))
    # Nothing named: said so.
    session$setInputs(roles_names = "", roles_pick = character(0), roles_apply = 3L)
    expect_match(is_strip(output$roles_editor_note$html), "No variable named", fixed = TRUE)
    # The grid carries the Validity kind column and hides what is unticked.
    json <- as.character(output$roles_grid)
    expect_match(json, "Validity kind", fixed = TRUE)
    expect_false(grepl("\"visible\":false", json, fixed = TRUE))
    session$setInputs(roles_cols = c("Role", "Validity kind"))
    json <- as.character(output$roles_grid)
    expect_match(json, "\"visible\":false", fixed = TRUE)
    hidden <- regmatches(json, regexpr("\"visible\":false,\"targets\":\\[[0-9,]+\\]", json))
    # The Item Name (column 1) and the Question (column 10) are in the grid,
    # so the hidden columns are counted with them.
    expect_match(hidden, "[1,4,5,6,7,8,9,10]", fixed = TRUE)
  })
  # The kind travels with the specification and the project.
  spec <- SIEVER:::sieve_spec_table(
    shiny::isolate(st$raw), shiny::isolate(st$items), shiny::isolate(st$meta),
    shiny::isolate(st$roles)
  )
  expect_true("kind" %in% names(spec))
  expect_identical(spec$kind, rep("", nrow(spec)))
  shiny::isolate(SIEVER:::sieve_apply_kind(st, "age_years", "criterion"))
  expect_identical(
    SIEVER:::sieve_apply_kind(st, "age_years", "criterion")$refused,
    paste(
      "A validity kind belongs to a criterion, and age_years is not one (give it the role criterion",
      "first). No kind was set."
    )
  )
  shiny::isolate(SIEVER:::sieve_apply_roles(st, c(age_years = "criterion")))
  shiny::isolate(SIEVER:::sieve_apply_kind(st, "age_years", "criterion"))
  spec <- SIEVER:::sieve_spec_table(
    shiny::isolate(st$raw), shiny::isolate(st$items), shiny::isolate(st$meta),
    shiny::isolate(st$roles)
  )
  expect_identical(spec$kind[spec$variable == "age_years"], "criterion")
  file <- is_path("kind_specification.csv")
  SIEVER:::sieve_spec_write(spec, file)
  other <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = other), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    session$setInputs(up_spec = list(name = "kind_specification.csv", datapath = file))
    expect_identical(state$roles$role[state$roles$variable == "age_years"], "criterion")
    expect_identical(state$roles$kind[state$roles$variable == "age_years"], "criterion")
  })
  # A kind outside the vocabulary, or on a non criterion, refuses the specification.
  bad <- spec
  bad$kind[bad$variable == "age_years"] <- "predictive"
  bad_file <- is_path("bad_kind.csv")
  SIEVER:::sieve_spec_write(bad, bad_file)
  out <- shiny::isolate(SIEVER:::sieve_apply_spec(other, bad_file))
  expect_false(out$ok)
  expect_match(
    paste(out$problems,
      collapse = "\n"
    ),
    "validity kind is not one of convergent, discriminant, criterion",
    fixed = TRUE
  )
  bad <- spec
  bad$kind[bad$variable == "sex"] <- "convergent"
  SIEVER:::sieve_spec_write(bad, bad_file)
  out <- shiny::isolate(SIEVER:::sieve_apply_spec(other, bad_file))
  expect_false(out$ok)
  expect_match(paste(out$problems, collapse = "\n"), "belongs to a criterion, and sex is not one", fixed = TRUE)
  # The project file keeps it.
  snapshot <- SIEVER:::sieve_project_snapshot(st)
  expect_identical(snapshot$roles$kind[snapshot$roles$variable == "age_years"], "criterion")
  path <- is_path("kind.siever.rds")
  SIEVER:::sieve_project_write(snapshot, path)
  back <- SIEVER:::sieve_project_read(path)
  expect_identical(back$roles$kind[back$roles$variable == "age_years"], "criterion")
})

# ---- The distribution plot and the ledger summary ----------------------------------

test_that("the distribution plot offers every column with its role, defaults to the first item, and the ledger summary shows the questions", {
  fx <- sieve_hs9_fixture()
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    choices <- column_choices()
    expect_identical(unname(choices), names(fx$data))
    expect_identical(names(choices)[match("id", choices)], "id (identifier)")
    expect_identical(names(choices)[match("t1_visual_perception", choices)], "t1_visual_perception (item)")
    roles <- roles_now()
    first_item <- roles$variable[roles$role == "item"][1]
    expect_identical(first_item, "t1_visual_perception")
    expect_false(identical(roles$role[1], "item")) # the first column is the identifier, never the default
    # The plot draws for an item (its processed values) and for any other column.
    session$setInputs(plot_var = first_item)
    expect_no_error(output$dist_plot)
    session$setInputs(plot_var = "id")
    expect_no_error(output$dist_plot)
    session$setInputs(plot_var = "school")
    expect_no_error(output$dist_plot)
    # The ledger summary by construct and facet, and the questions.
    json <- as.character(output$ledger_tbl)
    for (cn in c("Visual", "Textual", "Speed", "(no facet)", "3 continuous items")) {
      expect_match(json, cn, fixed = TRUE)
    }
    json <- as.character(output$question_tbl)
    expect_match(json, "Question", fixed = TRUE)
    expect_match(json, fx$labels[["t2_cubes"]], fixed = TRUE)
    expect_match(as.character(output$type_grid), "Question", fixed = TRUE)
    expect_match(as.character(output$type_grid), "Facet", fixed = TRUE)
  })
})

# ---- The exports: raw by default, processed, and the summary --------------------------

test_that("the exports on the Reproducible Code tab are raw by default, write processed on request, and the summary holds the covariance matrix, the means and n", {
  skip_if_not_installed("haven")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "Yes"))
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "status", value = "Dropped"))
  })
  raw <- shiny::isolate(st$raw)
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  shiny::testServer(mod_repro_server, args = list(state = st), {
    # Nothing chosen: raw.
    expect_identical(export_kind(), "raw")
    data_raw <- as.data.frame(haven::read_sav(output$dl_data))
    expect_identical(as.numeric(data_raw$t2_cubes), as.numeric(raw$t2_cubes))
    spec_raw <- utils::read.csv(output$dl_spec, colClasses = "character")
    expect_identical(unique(spec_raw$data_values), "raw")
    expect_identical(spec_raw$reverse_applied[spec_raw$variable == "t2_cubes"], "FALSE")
    expect_true(all(c("kind", "key_processed") %in% names(spec_raw)))
    expect_false("key_analytic" %in% names(spec_raw))
    # Processed on request, and the names say so.
    session$setInputs(export_values = "processed")
    expect_identical(export_kind(), "processed")
    data_processed <- as.data.frame(haven::read_sav(output$dl_data))
    expect_identical(as.numeric(data_processed$t2_cubes), as.numeric(items$t2_cubes))
    expect_match(attr(data_processed$t2_cubes, "label"), "[reverse scored within", fixed = TRUE)
    spec_processed <- utils::read.csv(output$dl_spec, colClasses = "character")
    expect_identical(unique(spec_processed$data_values), "processed")
    expect_identical(spec_processed$reverse_applied[spec_processed$variable == "t2_cubes"], "TRUE")
    # The summary: the covariance matrix of the included items' processed
    # values on the complete cases, the means, n.
    session$setInputs(summary_means = TRUE)
    file <- output$dl_summary
    header <- readLines(file, n = 3L)
    expect_match(header[1], "^# SIEVER summary of 8 included items")
    expect_match(header[2], "# Basis: processed item values, complete cases, n = 301.", fixed = TRUE)
    read <- utils::read.csv(file, comment.char = "#", row.names = 1, check.names = FALSE)
    included <- meta$item[meta$status == "active"]
    expect_identical(rownames(read), c(included, "mean"))
    expect_identical(colnames(read), included)
    X <- items[, included]
    X[] <- lapply(X, as.numeric)
    X <- X[stats::complete.cases(X), ]
    expect_equal(as.matrix(read[included, ]), stats::cov(as.matrix(X)), tolerance = 1e-12, ignore_attr = TRUE)
    expect_equal(unlist(read["mean", ]), colMeans(as.matrix(X)), tolerance = 1e-12, ignore_attr = TRUE)
    expect_identical(as.numeric(read["t2_cubes", "t2_cubes"]), stats::var(as.numeric(items$t2_cubes)))
    # Without the means: no mean row, and the file name says so.
    session$setInputs(summary_means = FALSE)
    file2 <- output$dl_summary
    read2 <- utils::read.csv(file2, comment.char = "#", row.names = 1, check.names = FALSE)
    expect_identical(rownames(read2), included)
    expect_match(readLines(file2, n = 2L)[2], "Means were not requested", fixed = TRUE)
    expect_identical(summary_file(), "siever_summary_processed.csv")
    session$setInputs(summary_means = TRUE)
    expect_identical(summary_file(), "siever_summary_processed_with_means.csv")
    # The export is recorded, and the recorded R writes the same matrix.
    log <- state$codelog
    expect_match(
      paste(log,
        collapse = "\n"
      ),
      "Export the summary of the 8 included items (covariance matrix and means, n = 301 complete cases)",
      fixed = TRUE
    )
    stamps <- which(startsWith(log, "# ["))
    at <- max(stamps[grepl("Export the summary of the 8 included items (covariance matrix and means",
      log[stamps],
      fixed = TRUE
    )])
    next_stamp <- stamps[stamps > at]
    code <- log[(at + 1L):(if (length(next_stamp)) next_stamp[1] - 1L else length(log))]
    env <- new.env()
    env$active_items <- X
    dir <- withr::local_tempdir()
    withr::with_dir(dir, eval(parse(text = code[!startsWith(code, "#") & nzchar(code)]), envir = env))
    replayed <- utils::read.csv(file.path(dir, "siever_summary_processed_with_means.csv"),
      comment.char = "#",
      row.names = 1, check.names = FALSE
    )
    expect_identical(replayed, read)
  })
  # The summary table itself, and its refusals.
  s <- SIEVER:::sieve_summary_table(items, meta, means = FALSE)
  expect_null(s$means)
  expect_identical(s$n, 301L)
  expect_identical(s$items, meta$item[meta$status == "active"])
  none <- meta
  none$status <- "parked"
  expect_error(SIEVER:::sieve_summary_table(items, none), "No item is included")
  expect_error(SIEVER:::sieve_summary_table(NULL, meta), "No data are loaded")
})

test_that("a study confirmed on load says so, and a file of the researcher's own asks for the press and then says it was pressed", {
  # The bundled example: every item continuous, confirmed on load.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    expect_true(isTRUE(state$confirmed))
    expect_identical(confirmed_at()$origin, "auto")
    button <- is_strip(output$confirm_button$html)
    expect_match(button, "Confirmed on load (step 4): 9 continuous items, 3 constructs, 0 facets", fixed = TRUE)
    note <- is_strip(output$confirm_state$html)
    expect_match(note, "Confirmed on load: every item was detected continuous, so no press was needed", fixed = TRUE)
    expect_match(note, "ordered or binary estimator decision", fixed = TRUE)
  })
  # The same nine tests from a file of the researcher's own: the press is
  # asked for, and once pressed the button says it was pressed, not loaded.
  fx <- sieve_hs9_fixture()
  csv <- is_path("nine_own.csv")
  utils::write.csv(fx$data, csv, row.names = FALSE)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    is_load_csv(session, csv, source = "alone")
    expect_false(isTRUE(state$confirmed))
    expect_match(is_strip(output$confirm_button$html), "Confirm the study (step 4)", fixed = TRUE)
    expect_match(is_strip(output$confirm_state$html), "Awaiting confirmation", fixed = TRUE)
    session$setInputs(confirm_main = 1L)
    expect_true(isTRUE(state$confirmed))
    expect_identical(confirmed_at()$origin, "button")
    button <- is_strip(output$confirm_button$html)
    expect_match(button, "9 continuous items", fixed = TRUE)
    expect_false(grepl("on load", button, fixed = TRUE))
    expect_identical(trimws(is_strip(output$confirm_state$html)), "Confirmed")
  })
})

# ---- Load with a study in the session, and the construct basis ---------------------

test_that("Load says what it would do once a study is in, and replaces the study only through its dialog", {
  # The main bar's Load (step 2) must not read as the next step once a
  # study is in, and pressing it after an exclusion must not reload the file
  # and lose the exclusion without a word.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    expect_identical(load_effect(), "read")
    session$setInputs(load = 1L)
    expect_identical(load_effect(), "replace")
    why <- is_strip(output$step2_why$html)
    expect_match(why, "Loaded: ", fixed = TRUE)
    expect_match(why, "Load would read the same source afresh and replace this study, so it asks first.", fixed = TRUE)
    expect_match(why, "takes effect as it is made, without Load", fixed = TRUE)
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "status", value = "Excluded"))
    first <- state$dataset_id
    # The main bar's Load and the sidebar's: nothing changes until the
    # dialog's Load and replace.
    session$setInputs(load_main = 1L)
    session$setInputs(load = 2L)
    expect_identical(state$dataset_id, first)
    expect_identical(state$meta$status[state$meta$item == "t4_lozenges"], "parked")
    session$setInputs(load_replace = 1L)
    expect_false(identical(state$dataset_id, first))
    expect_identical(state$meta$status[state$meta$item == "t4_lozenges"], "active")
  })
})

test_that("a specification alone or a project file is not a replacement, so Load does not ask for one", {
  fx <- sieve_hs9_fixture()
  dir <- withr::local_tempdir()
  data_csv <- file.path(dir, "nine.csv")
  utils::write.csv(fx$data, data_csv, row.names = FALSE)
  spec_csv <- file.path(dir, "nine_specification.csv")
  sieve_specification_template(fx$data[, fx$item_cols], file = spec_csv)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "file", basis = "theoretical", palette = "nd")
    session$setInputs(file = list(name = "nine.csv", datapath = data_csv))
    expect_identical(load_effect(), "read")
    session$setInputs(load = 1L)
    expect_identical(load_effect(), "replace")
    session$setInputs(file = list(name = "nine_specification.csv", datapath = spec_csv))
    expect_identical(load_effect(), "spec")
    expect_match(is_strip(output$step2_why$html), "Load would apply the chosen specification to the loaded data.", fixed = TRUE)
    session$setInputs(file = list(name = "nine.siever.rds", datapath = spec_csv))
    expect_identical(load_effect(), "project")
  })
})

test_that("Apply construct basis acts only on a change, says before the press what a change does, and records only a change", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    note <- is_strip(output$basis_policy$html)
    expect_match(note, "In force: Theoretical (researcher-specified assignments).", fixed = TRUE)
    expect_match(note, "Apply construct basis is enabled once the selection differs from the basis in force.", fixed = TRUE)
    # The basis already in force: nothing is changed or recorded (the
    # button is disabled in the browser, and a press does nothing).
    steps <- length(state$codelog)
    session$setInputs(apply_basis = 1L)
    expect_identical(state$construct_basis %||% "theoretical", "theoretical")
    expect_identical(length(state$codelog), steps)
    # A selection not yet applied is said with what it would do, and
    # changes nothing.
    session$setInputs(basis = "empirical")
    note <- is_strip(output$basis_policy$html)
    expect_match(note, "Selected, not yet applied: Empirical (provisional assignments). Apply construct basis changes to it: the Optimizer tab is shown", fixed = TRUE)
    expect_identical(state$construct_basis %||% "theoretical", "theoretical")
    # Applied: the basis changes, once, in the session script.
    session$setInputs(apply_basis = 2L)
    expect_identical(state$construct_basis, "empirical")
    expect_true(any(grepl("Change construct basis from theoretical to empirical", state$codelog, fixed = TRUE)))
    expect_match(is_strip(output$basis_policy$html), "In force: Empirical (provisional assignments).", fixed = TRUE)
  })
})

test_that("the item grid names an item and its reversed copy, and one click excludes the copy not keyed with the construct", {
  fx <- sieve_hs9_fixture()
  d <- data.frame(id = fx$data$id, fx$data[, fx$item_cols])
  x <- d$t2_cubes
  d$t2_cubes_r <- (min(x, na.rm = TRUE) + max(x, na.rm = TRUE)) - x
  by_item <- stats::setNames(
    rep(names(fx$theory_map), lengths(fx$theory_map)),
    unlist(fx$theory_map, use.names = FALSE)
  )
  construct <- c(by_item[fx$item_cols], t2_cubes_r = "Visual")
  path <- is_described_csv(d, list(construct = construct), name = "with_copy.csv")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    is_load_csv(session, path, source = "ready")
    expect_identical(state$meta$construct[state$meta$item == "t2_cubes_r"], "Visual")
    note <- is_strip(output$copies_note$html)
    expect_match(note, "The same item twice.", fixed = TRUE)
    expect_match(note, "t2_cubes and t2_cubes_r correlate -1.00 (n = 301), and their sum is", fixed = TRUE)
    expect_match(note, "one is almost certainly the other reversed, both in the file.", fixed = TRUE)
    expect_match(note, "Keep t2_cubes, the one keyed with Visual", fixed = TRUE)
    expect_match(note, "and exclude t2_cubes_r.", fixed = TRUE)
    expect_match(note, "Exclude t2_cubes_r", fixed = TRUE)
    session$setInputs(copies_exclude = 1L)
    expect_identical(state$meta$status[state$meta$item == "t2_cubes_r"], "parked")
    expect_identical(state$meta$status[state$meta$item == "t2_cubes"], "active")
    expect_match(state$meta$exclude_reason[state$meta$item == "t2_cubes_r"], "the same item as t2_cubes (r = -1.00)", fixed = TRUE)
    expect_null(output$copies_note$html)
  })
})

test_that("the Replace dialog names what a Load would discard, tells a reload from another source, and says the basis returns to theoretical", {
  shown <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    showModal = function(ui, ...) shown$ui <- ui,
    .package = "shiny"
  )
  dialog <- function() is_strip(as.character(shown$ui))
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    # A reload of the same source, with an Item Name edited (which the
    # confirmation does not read, and a Load still discards).
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "label", value = "Lozenges renamed"))
    session$setInputs(load = 2L)
    d <- dialog()
    expect_match(d, "Load reads the bundled nine-test example afresh and starts a new study in place of", fixed = TRUE)
    expect_match(d, "the item grid returns to the file as loaded;", fixed = TRUE)
    expect_match(d, "what was done since the load is discarded: the Item Name of t4_lozenges edited;", fixed = TRUE)
    expect_false(grepl("nothing in it has changed", d, fixed = TRUE))
    # Another source: a new study from it, not a return to the file.
    shown$ui <- NULL
    session$setInputs(source = "hs")
    session$setInputs(load = 3L)
    d <- dialog()
    expect_match(d, "Load reads the full bundled example and starts a new study from it in place of", fixed = TRUE)
    expect_match(d, "its items replace the item grid;", fixed = TRUE)
    expect_false(grepl("returns to the file as loaded", d, fixed = TRUE))
    # The empirical basis does not survive a load, and the dialog says so.
    session$setInputs(source = "hs9", basis = "empirical", apply_basis = 1L)
    expect_identical(state$construct_basis, "empirical")
    shown$ui <- NULL
    session$setInputs(load = 4L)
    expect_match(dialog(), "the construct basis returns to theoretical and the Optimizer tab is hidden", fixed = TRUE)
    session$setInputs(load_replace = 1L)
    expect_identical(state$construct_basis %||% "theoretical", "theoretical")
  })
})

test_that("what a Load would discard is read from the ledger, the roles and the rows", {
  fx <- sieve_hs9_fixture()
  m <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  roles <- sieve_declared_roles(fx$data, fx$item_cols)
  then <- list(meta = m, roles = roles, rows = 301L, data_key = "k1")
  expect_identical(SIEVER:::sieve_load_discards(then, then), character(0))
  now <- then
  now$meta$label[now$meta$item == "t2_cubes"] <- "Cubes renamed"
  now$meta$status[now$meta$item == "t4_lozenges"] <- "parked"
  now$roles$role[now$roles$variable == "grade"] <- "criterion"
  now$rows <- 298L
  now$data_key <- "k2"
  out <- SIEVER:::sieve_load_discards(then, now)
  expect_identical(out, c(
    "t4_lozenges excluded", "the Item Name of t2_cubes edited",
    "the role of grade changed", "3 respondents removed from the analysis"
  ))
})

test_that("a change the app left the confirmation standing for still asks for it, an exclusion is never named as the reason, and an inclusion that moves the estimator withdraws it", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    expect_true(isTRUE(state$confirmed))
    # A construct set through the shared metadata patch (as the Items tab
    # sets it), which leaves state$confirmed standing: still a change that
    # asks for the confirmation, never "in effect".
    sieve_apply_metadata_patch(
      state, sieve_metadata_patch(shiny::isolate(state$meta), "t2_cubes", "construct", "Textual"), "Assign construct"
    )
    session$flushReact()
    button <- is_strip(output$confirm_button$html)
    expect_match(button, "Changed since confirmation (t2_cubes reassigned): confirm again", fixed = TRUE)
    expect_false(grepl("In effect now", is_strip(output$confirm_state$html), fixed = TRUE))
    sieve_apply_metadata_patch(
      state, sieve_metadata_patch(shiny::isolate(state$meta), "t2_cubes", "construct", "Visual"), "Assign construct"
    )
    session$flushReact()
    session$setInputs(confirm_main = 1L)
    # A reversal withdraws the confirmation; undone, with an exclusion made
    # meanwhile, the exclusion is in effect and is not the reason.
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "Yes"))
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "status", value = "Excluded"))
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "No"))
    expect_false(isTRUE(state$confirmed))
    expect_match(is_strip(output$confirm_button$html),
      "Changed since confirmation (a change made and then undone): confirm again",
      fixed = TRUE
    )
    expect_match(is_strip(output$confirm_state$html), "Already in effect: t4_lozenges excluded.", fixed = TRUE)
    session$setInputs(confirm_main = 2L)
    expect_true(isTRUE(state$confirmed))
    # An ordered item excluded, the study confirmed with continuous items
    # only (ML with FIML), then the ordered item included again: the types
    # now call for another estimator, so the inclusion withdraws the
    # confirmation.
    session$setInputs(cell_edit = list(item = "t1_visual_perception", field = "type", value = "ordered"))
    session$setInputs(cell_edit = list(item = "t1_visual_perception", field = "status", value = "Excluded"))
    session$setInputs(confirm_main = 3L)
    expect_true(isTRUE(state$confirmed))
    confirmed_with <- state$estimator
    session$setInputs(cell_edit = list(item = "t1_visual_perception", field = "status", value = "Included"))
    expect_false(isTRUE(state$confirmed))
    expect_false(identical(est()$label, confirmed_with))
    expect_match(is_strip(output$confirm_button$html),
      sprintf("Changed since confirmation (t1_visual_perception included, so the included items' types now call for %s): confirm again", est()$label),
      fixed = TRUE
    )
  })
})

test_that("a selection Load would refuse is said in step 2, and Apply construct basis waits for a study", {
  fx <- sieve_hs9_fixture()
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "nine.csv")
  utils::write.csv(fx$data, csv, row.names = FALSE)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "file", basis = "theoretical", palette = "nd")
    expect_match(is_strip(output$basis_policy$html), "Load the data first (step 2): the basis is set for the loaded study", fixed = TRUE)
    # Before any study: Apply does nothing.
    session$setInputs(basis = "empirical", apply_basis = 1L)
    expect_identical(state$construct_basis %||% "theoretical", "theoretical")
    session$setInputs(file = data.frame(
      name = c("nine.csv", "again.csv"), size = 0, type = "",
      datapath = c(csv, csv), stringsAsFactors = FALSE
    ))
    expect_identical(load_effect(), "problem")
    expect_match(is_strip(output$step2_why$html), "Load cannot use the chosen files:", fixed = TRUE)
    session$setInputs(file = list(name = "nine.csv", datapath = csv))
    expect_identical(load_effect(), "read")
    session$setInputs(load = 1L)
    expect_identical(load_effect(), "replace")
    expect_match(is_strip(output$step2_why$html), "Load would read the same source afresh", fixed = TRUE)
    session$setInputs(file = data.frame(
      name = c("nine.csv", "again.csv"), size = 0, type = "",
      datapath = c(csv, csv), stringsAsFactors = FALSE
    ))
    expect_match(is_strip(output$step2_why$html), "Load cannot use the chosen files:", fixed = TRUE)
  })
})

test_that("every writer of the item status withdraws the confirmation when the estimator decision changes, and a stored label of another wording does not", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    # Visual's first item ordered, Visual left out, the study confirmed
    # with continuous items only.
    session$setInputs(cell_edit = list(item = "t1_visual_perception", field = "type", value = "ordered"))
    session$setInputs(analyze_constructs = c("Textual", "Speed"), apply_constructs = 1L)
    session$setInputs(confirm_main = 1L)
    expect_true(isTRUE(state$confirmed))
    # The constructs to analyze bring the ordered item back: the types now
    # call for another estimator, so the confirmation is withdrawn.
    session$setInputs(analyze_constructs = c("Visual", "Textual", "Speed"), apply_constructs = 2L)
    expect_false(isTRUE(state$confirmed))
    expect_match(is_strip(output$confirm_button$html), "so the included items' types now call for", fixed = TRUE)
  })
  # A confirmed state whose stored estimator label is worded otherwise (as
  # a saved project may hold it): an exclusion that does not change the
  # decision keeps the confirmation.
  fx <- sieve_hs9_fixture()
  s <- sieve_new_state()
  s$raw <- fx$data
  s$items <- fx$data[, fx$item_cols, drop = FALSE]
  s$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  s$confirmed <- TRUE
  s$estimator <- "MLR (an older wording)"
  sieve_set_item_status(s, "t4_lozenges", "parked")
  expect_true(isTRUE(shiny::isolate(s$confirmed)))
})

test_that("a study opened as a project is never called a reload, and a bundled example is a reload whatever file is chosen", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "nine.siever.rds")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
  })
  sieve_project_write(sieve_project_snapshot(st), path)
  shown <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    showModal = function(ui, ...) shown$ui <- ui,
    .package = "shiny"
  )
  st2 <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st2), {
    session$setInputs(source = "file", basis = "theoretical", palette = "nd")
    session$setInputs(file = list(name = "nine.siever.rds", datapath = path), load = 1L)
    session$setInputs(confirm_open = 1L)
    expect_false(is.null(state$meta))
    expect_identical(loaded_at()$src, "project")
    # The bundled example after the opened project: a new study, not a
    # return to the file as loaded.
    session$setInputs(source = "hs9")
    expect_false(same_selection())
    session$setInputs(load = 2L)
    d <- is_strip(as.character(shown$ui))
    expect_match(d, "starts a new study from it", fixed = TRUE)
    expect_false(grepl("afresh", d, fixed = TRUE))
    session$setInputs(load_replace = 1L)
    expect_identical(loaded_at()$src, "hs9")
    # A file chosen meanwhile does not make the bundled example another source.
    session$setInputs(source = "alone", file = list(name = "nine.siever.rds", datapath = path))
    session$setInputs(source = "hs9")
    expect_true(same_selection())
  })
})

test_that("a specification Load applied is not applied again by Load, and one chosen before any data says so", {
  fx <- sieve_hs9_fixture()
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "nine.csv")
  utils::write.csv(fx$data, csv, row.names = FALSE)
  spec <- file.path(dir, "nine_specification.csv")
  sieve_specification_template(fx$data[, fx$item_cols], file = spec)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "file", basis = "theoretical", palette = "nd")
    session$setInputs(file = list(name = "nine_specification.csv", datapath = spec))
    expect_identical(load_effect(), "spec_alone")
    expect_match(is_strip(output$step2_why$html), "nine_specification.csv is a specification. Choose it together with its data file", fixed = TRUE)
    session$setInputs(file = list(name = "nine.csv", datapath = csv), load = 1L)
    session$setInputs(file = list(name = "nine_specification.csv", datapath = spec))
    expect_identical(load_effect(), "spec")
    session$setInputs(load = 2L)
    expect_identical(load_effect(), "spec_applied")
    expect_match(is_strip(output$step2_why$html), "The chosen specification is applied to the loaded data", fixed = TRUE)
  })
})

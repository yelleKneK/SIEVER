# The pre-SIEVER step (sieve_specification_template() and
# sieve_prepare_study()) and the ways a prepared study comes into the Ingest
# tab: a data file with its specification in one upload, a project file in
# the data file input, and constructs and facets carried in the column names
# or the variable labels.
#
# The expected ledgers are written out by hand from the specification each
# test fills in; the reflections follow from the algebra of a reversal
# within declared bounds. The module tests drive the real load observer with
# shiny::testServer.

skip_if_not_installed("DMAR")
skip_if_not_installed("shiny")

pr_strip <- function(html) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", html))

pr_quiet <- function(expr) invisible(utils::capture.output(expr))

# A folder of its own for each file, so that a refused write can be seen to
# have left nothing behind.
pr_path <- function(name) {
  path <- file.path(tempfile("pr_"), name)
  dir.create(dirname(path))
  path
}

pr_csv <- function(d, name = "nine.csv") {
  path <- pr_path(name)
  utils::write.csv(d, path, row.names = FALSE)
  path
}

# The specification columns, in the order the template writes them.
pr_columns <- c(
  "siever_specification", "data_values", "variable", "role", "kind", "label", "wording",
  "type", "construct", "facet", "status", "reverse", "reverse_applied",
  "rev_lo", "rev_hi", "rev_bounds"
)

# The bundled nine tests with the template filled in: the three constructs,
# a facet on the Visual items, one item excluded, one reverse keyed within
# declared bounds, and one Item Name.
pr_filled <- function(fx) {
  spec <- sieve_specification_template(fx$data)
  for (cn in names(fx$theory_map)) {
    spec$construct[spec$variable %in% fx$theory_map[[cn]]] <- cn
  }
  spec$facet[spec$variable %in% fx$theory_map$Visual] <- "Shapes"
  spec$status[spec$variable == "t4_lozenges"] <- "excluded"
  spec$reverse[spec$variable == "t1_visual_perception"] <- "TRUE"
  spec$rev_lo[spec$variable == "t1_visual_perception"] <- "0"
  spec$rev_hi[spec$variable == "t1_visual_perception"] <- "60"
  spec$label[spec$variable == "t2_cubes"] <- "Cubes"
  spec
}

# The ledger that specification describes, by hand.
pr_expected_ledger <- function(fx) {
  items <- fx$item_cols
  construct <- rep("Unassigned", length(items))
  for (cn in names(fx$theory_map)) {
    construct[items %in% fx$theory_map[[cn]]] <- cn
  }
  data.frame(
    item = items,
    label = ifelse(items == "t2_cubes", "Cubes", items),
    construct = construct,
    facet = ifelse(items %in% fx$theory_map$Visual, "Shapes", NA_character_),
    type = rep("continuous", length(items)),
    status = ifelse(items == "t4_lozenges", "parked", "active"),
    reverse = items == "t1_visual_perception",
    stringsAsFactors = FALSE
  )
}

pr_ledger_of <- function(meta) {
  out <- meta[, c("item", "label", "construct", "facet", "type", "status", "reverse")]
  out$reverse <- out$reverse %in% TRUE
  rownames(out) <- NULL
  out
}

# ---- The template --------------------------------------------------------

test_that("a specification template is written and read back, with the roles proposed, the types detected and the constructs left to the researcher", {
  fx <- sieve_hs9_fixture()
  d <- fx$data
  template <- sieve_specification_template(d)
  expect_identical(names(template), pr_columns)
  expect_true(all(vapply(template, is.character, logical(1))))
  expect_identical(nrow(template), ncol(d))
  expect_identical(template$variable, names(d))
  expect_identical(template$siever_specification, rep("2", ncol(d)))
  expect_identical(template$data_values, rep("raw", ncol(d)))
  # The roles are the Ingest tab's proposal for these columns.
  expect_identical(
    template$role[match(c("id", "sex", "grade", "age_years", "school"), template$variable)],
    c("identifier", "group", "group", "auxiliary", "group")
  )
  expect_identical(template$role[template$variable %in% fx$item_cols], rep("item", 9L))
  is_item <- template$role == "item"
  # Types detected for the items, nothing for the other columns.
  expect_identical(template$type[is_item], rep("continuous", 9L))
  expect_identical(template$type[!is_item], rep("", sum(!is_item)))
  # The constructs and facets are for the researcher to fill in.
  expect_identical(template$construct, rep("", ncol(d)))
  expect_identical(template$facet, rep("", ncol(d)))
  expect_identical(template$status[is_item], rep("included", 9L))
  expect_identical(template$status[!is_item], rep("", sum(!is_item)))
  expect_identical(template$reverse[is_item], rep("FALSE", 9L))
  expect_identical(template$reverse_applied[is_item], rep("FALSE", 9L))
  expect_identical(template$rev_lo, rep("", ncol(d)))
  expect_identical(template$label[is_item], rep("", 9L)) # no variable labels in a csv

  # Written and read back exactly, by the import's own reader; and the same
  # template from the file as from the data frame.
  path <- pr_path("nine_specification.csv")
  expect_invisible(sieve_specification_template(d, file = path))
  expect_true(file.exists(path))
  expect_identical(SIEVER:::sieve_spec_read(path), template)
  expect_identical(sieve_specification_template(pr_csv(d)), template)
})

test_that("a template from a labeled .sav carries the Item Names, a stated keying, and the constructs the labels name", {
  skip_if_not_installed("haven")
  fx <- sieve_hs9_fixture()
  d <- fx$data[, c("id", "t1_visual_perception", "t2_cubes", "t4_lozenges", "t6_paragraph_comprehension")]
  attr(d$t1_visual_perception, "label") <- "Visual: Shapes: Visual perception test"
  attr(d$t2_cubes, "label") <- "Visual: Shapes: Cubes"
  attr(d$t4_lozenges, "label") <- "Q3: a lonely label that is not a construct"
  attr(d$t6_paragraph_comprehension, "label") <- "Paragraph comprehension [reverse scored within 0 to 60]"
  sav <- pr_path("labeled.sav")
  haven::write_sav(d, sav)
  template <- sieve_specification_template(sav)
  expect_identical(template$role, c("identifier", rep("item", 4L)))
  expect_identical(template$label[-1], c(
    "Visual perception test", "Cubes",
    "Q3: a lonely label that is not a construct",
    "Paragraph comprehension"
  ))
  expect_identical(template$wording[-1], template$label[-1])
  expect_identical(template$construct, c("", "Visual", "Visual", "", ""))
  expect_identical(template$facet, c("", "Shapes", "Shapes", "", ""))
  # The keying statement SIEVER's own export writes is read: the file holds
  # the item reverse scored, within the bounds the label gives.
  expect_identical(template$reverse[5], "TRUE")
  expect_identical(template$reverse_applied[5], "TRUE")
  expect_identical(c(template$rev_lo[5], template$rev_hi[5], template$rev_bounds[5]), c("0", "60", "declared"))
  expect_identical(template$reverse[2:4], rep("FALSE", 3L))
})

# ---- sieve_prepare_study() ---------------------------------------------------

test_that("a study prepared from the bundled data and a specification opens through sieve_project_read() confirmed, with the ledger the specification describes", {
  fx <- sieve_hs9_fixture()
  d <- fx$data
  spec <- pr_filled(fx)
  path <- pr_path("nine.siever.rds")
  expect_output(
    study <- sieve_prepare_study(d, spec, file = path, name = "Nine tests"),
    "SIEVER project written"
  )
  expect_s3_class(study, "sieve_project")
  expect_true(file.exists(path))
  printed <- utils::capture.output(print(study))
  expect_true(any(grepl("Data: Nine tests, 301 rows, 14 columns; 9 items, 1 identifier, 3 groups, 1 auxiliary variable",
    printed,
    fixed = TRUE
  )))
  expect_true(any(grepl("Items: 8 included, 1 excluded; included: 8 continuous items", printed, fixed = TRUE)))
  expect_true(any(grepl("Constructs: 3", printed, fixed = TRUE)))
  expect_true(any(grepl("Visual: 2 items; facets: Shapes 2", printed, fixed = TRUE)))
  expect_true(any(grepl("Reverse scored: 1 item (t1_visual_perception)", printed, fixed = TRUE)))
  # The estimator label states the resolved estimator, not "ML or MLR": ML with FIML (DMAR's default)
  # for continuous items.
  expect_true(any(grepl("Item types: confirmed; estimator ML with FIML on a Pearson matrix", printed, fixed = TRUE)))

  q <- SIEVER:::sieve_project_read(path)
  expect_identical(attr(q, "container"), 1L)
  expect_true(q$confirmed)
  expect_identical(q$estimator, "ML with FIML")
  expect_identical(q$dataset$name, "Nine tests")
  expect_identical(pr_ledger_of(q$meta), pr_expected_ledger(fx))
  i <- which(q$meta$item == "t1_visual_perception")
  expect_identical(c(q$meta$rev_lo[i], q$meta$rev_hi[i]), c(0, 60))
  expect_identical(q$meta$rev_bounds[i], "declared")
  # The data as loaded are untouched; the analytic values hold the reflection.
  expect_identical(q$raw, d)
  expect_identical(q$items$t1_visual_perception, (0 + 60) - as.numeric(d$t1_visual_perception))
  expect_identical(q$items$t2_cubes, d$t2_cubes)
  expect_identical(names(q$items), fx$item_cols)
  expect_identical(
    q$roles$role[match(c("id", "sex", "grade", "age_years", "school"), q$roles$variable)],
    c("identifier", "group", "group", "auxiliary", "group")
  )
  expect_identical(q$id_vars, c("id", "sex", "grade", "age_years", "school"))
  expect_length(q$transforms, 1L)
  expect_identical(q$transforms[[1]]$item, "t1_visual_perception")
  expect_identical(q$transforms[[1]]$origin, "specification")
  script <- paste(q$codelog, collapse = "\n")
  # Given a data frame, the script's step says so; no file was read.
  expect_match(script, "Use data frame: Nine tests", fixed = TRUE)
  expect_false(grepl("Read data file", script, fixed = TRUE))
  expect_match(script,
    "# The data frame given to sieve_prepare_study(); bind yours to this name before running the script.\nd <- d",
    fixed = TRUE
  )
  expect_match(script, "Restore the specification from the specification", fixed = TRUE)
  expect_match(script,
    "Confirm item types from the specification (theoretical construct basis); estimator = ML with FIML on a Pearson matrix",
    fixed = TRUE
  )
  # What the function returns is what the file holds.
  expect_identical(unclass(study)[setdiff(names(study), "saved")], unclass(q)[setdiff(names(q), "saved")])

  # The same study from the data file and the specification file: the same
  # ledger and the same values, named after the file.
  csv <- pr_csv(d)
  spec_file <- pr_path("nine_specification.csv")
  SIEVER:::sieve_spec_write(spec, spec_file)
  other <- pr_path("nine_from_files.siever.rds")
  pr_quiet(from_files <- sieve_prepare_study(csv, spec_file, file = other))
  expect_identical(from_files$dataset$name, "nine.csv")
  expect_identical(pr_ledger_of(from_files$meta), pr_expected_ledger(fx))
  expect_identical(SIEVER:::sieve_data_key(from_files$items), SIEVER:::sieve_data_key(q$items))
  # (age_years, a twelfth, loses its last digits in write.csv; the item
  # columns are whole numbers and come back exactly.)
  expect_identical(
    SIEVER:::sieve_data_key(from_files$raw[, fx$item_cols]),
    SIEVER:::sieve_data_key(q$raw[, fx$item_cols])
  )
  expect_match(paste(from_files$codelog, collapse = "\n"), 'd <- read.csv("nine.csv")', fixed = TRUE)
  expect_match(
    paste(from_files$codelog,
      collapse = "\n"
    ),
    "Restore the specification from nine_specification.csv",
    fixed = TRUE
  )
})

test_that("a specification that does not type every item prepares a project that opens awaiting confirmation", {
  fx <- sieve_hs9_fixture()
  spec <- pr_filled(fx)
  spec$type[spec$variable == "t7_sentence"] <- ""
  path <- pr_path("untyped.siever.rds")
  expect_output(
    study <- sieve_prepare_study(fx$data, spec, file = path),
    "Item types: not confirmed (the specification does not type t7_sentence)",
    fixed = TRUE
  )
  q <- SIEVER:::sieve_project_read(path)
  expect_false(q$confirmed)
  expect_null(q$estimator)
  expect_identical(pr_ledger_of(q$meta), pr_expected_ledger(fx)) # the detected type stands
  expect_false(any(grepl("Confirm item types", q$codelog, fixed = TRUE)))
})

test_that("a specification with a problem writes nothing and names every problem", {
  fx <- sieve_hs9_fixture()
  # The import reports every problem of the vocabulary and the variable
  # names together, before anything is resolved ...
  bad <- pr_filled(fx)
  bad$type[bad$variable == "t2_cubes"] <- "likert"
  bad$variable[bad$variable == "t9_word_meaning"] <- "t9_nope"
  bad$status[bad$variable == "t7_sentence"] <- "maybe"
  path <- pr_path("refused.siever.rds")
  caught <- tryCatch(sieve_prepare_study(fx$data, bad, file = path), sieve_import_error = function(e) e)
  expect_s3_class(caught, "sieve_import_error")
  expect_length(caught$problems, 3L)
  expect_match(conditionMessage(caught), "No project file was written. The specification has 3 problems:", fixed = TRUE)
  expect_match(conditionMessage(caught), "t9_nope", fixed = TRUE)
  expect_match(conditionMessage(caught), 't2_cubes ("likert")', fixed = TRUE)
  expect_match(conditionMessage(caught), 't7_sentence ("maybe")', fixed = TRUE)
  expect_false(file.exists(path))
  expect_length(list.files(dirname(path)), 0L)
  # ... and a problem of the structure, once the cells are in their vocabulary.
  orphan <- pr_filled(fx)
  orphan$construct[orphan$variable == "t10_addition"] <- ""
  orphan$facet[orphan$variable == "t10_addition"] <- "Arithmetic"
  expect_error(sieve_prepare_study(fx$data, orphan, file = path),
    "A facet needs a construct to belong to, and these items have a facet and no construct: t10_addition",
    class = "sieve_import_error"
  )
  expect_false(file.exists(path))

  # A reverse keyed item whose keying the specification leaves unsettled is
  # a problem too, with the choice named.
  unsettled <- pr_filled(fx)
  unsettled$reverse_applied[unsettled$variable == "t1_visual_perception"] <- ""
  expect_error(sieve_prepare_study(fx$data, unsettled, file = path),
    "does not say whether the data file already holds the reversal",
    class = "sieve_import_error"
  )
  expect_false(file.exists(path))
  # ... and keying = "administered" settles it.
  pr_quiet(settled <- sieve_prepare_study(fx$data, unsettled, file = path, keying = "administered"))
  expect_true(file.exists(path))
  expect_identical(settled$items$t1_visual_perception, 60 - as.numeric(fx$data$t1_visual_perception))

  # No specification, no data, nowhere to write: each refused before anything happens.
  expect_error(sieve_prepare_study(fx$data, data.frame(role = "item"), file = path),
    "needs a column named variable",
    class = "sieve_import_error"
  )
  expect_error(sieve_prepare_study("/nowhere/nine.csv", pr_filled(fx), file = path), "was not found")
  expect_error(
    sieve_prepare_study(fx$data, pr_filled(fx), file = "/nowhere/at/all/nine.siever.rds"),
    "does not exist"
  )
  expect_error(sieve_prepare_study(fx$data, pr_filled(fx), file = NULL), "`file` must be")
})

# ---- The Ingest tab recognizes a prepared study ---------------------------------

pr_upload <- function(session, names, paths) {
  session$setInputs(source = "file", basis = "theoretical", palette = "nd", spec_keying = "verify")
  session$setInputs(file = data.frame(
    name = names, size = 0, type = "", datapath = paths,
    stringsAsFactors = FALSE
  ))
  session$setInputs(load = 1L)
}

test_that("a data file and its specification uploaded together confirm the study from the specification", {
  fx <- sieve_hs9_fixture()
  d <- fx$data
  csv <- pr_csv(d)
  spec_file <- pr_path("nine_specification.csv")
  SIEVER:::sieve_spec_write(pr_filled(fx), spec_file)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    pr_upload(session, c("nine.csv", "nine_specification.csv"), c(csv, spec_file))
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$estimator, "ML with FIML")
    expect_identical(pr_ledger_of(state$meta), pr_expected_ledger(fx))
    expect_identical(state$items$t1_visual_perception, 60 - as.numeric(d$t1_visual_perception))
    expect_identical(sieve_role_vars(state, c("identifier", "group", "criterion", "antecedent", "auxiliary")), c("id", "sex", "grade", "age_years", "school"))
    chip <- pr_strip(output$status_chip$html)
    expect_match(chip,
      "Confirmed from the specification: 8 continuous items, 3 constructs, 1 facet (ML with FIML). All tabs unlocked",
      fixed = TRUE
    )
    expect_match(pr_strip(output$roles_notice$html),
      "Variable roles: 9 items, 1 identifier, 3 groups, 1 auxiliary variable",
      fixed = TRUE
    )
    script <- paste(state$codelog, collapse = "\n")
    expect_match(script, 'd <- read.csv("nine.csv")', fixed = TRUE)
    expect_match(script, "Restore the specification from nine_specification.csv", fixed = TRUE)
    expect_match(script, "Confirm item types from the specification", fixed = TRUE)
    expect_match(script, "# The specification typed every item, so the study was confirmed from it.", fixed = TRUE)
    expect_length(state$transforms, 1L)
    expect_no_error(output$type_grid)
    # The pipeline gate is open.
    expect_null(sieve_gate(state))
  })

  # The order of the two files does not matter, and a specification is
  # recognized by its header when its name does not say.
  plain_name <- pr_path("filled.csv")
  file.copy(spec_file, plain_name)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    pr_upload(session, c("filled.csv", "nine.csv"), c(plain_name, csv))
    expect_true(isTRUE(state$confirmed))
    expect_identical(pr_ledger_of(state$meta), pr_expected_ledger(fx))
    expect_match(pr_strip(output$status_chip$html), "Confirmed from the specification", fixed = TRUE)
  })

  # A specification that does not type every item loads the study and
  # leaves the confirmation to the researcher, and the chip says so.
  partial <- pr_filled(fx)
  partial$type[partial$variable == "t7_sentence"] <- ""
  partial_file <- pr_path("partial_specification.csv")
  SIEVER:::sieve_spec_write(partial, partial_file)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    pr_upload(session, c("nine.csv", "partial_specification.csv"), c(csv, partial_file))
    expect_false(isTRUE(state$confirmed))
    expect_identical(pr_ledger_of(state$meta), pr_expected_ledger(fx))
    expect_match(pr_strip(output$status_chip$html), "press Confirm (step 4) to unlock", fixed = TRUE)
    session$setInputs(confirm_main = 1L)
    expect_true(isTRUE(state$confirmed))
    expect_match(pr_strip(output$status_chip$html), "9 items loaded; types confirmed (ML with FIML)", fixed = TRUE)
  })

  # A specification with a problem is refused as a whole: the data stay
  # loaded as proposed, nothing of the specification is applied, and the
  # study is not confirmed.
  bad <- pr_filled(fx)
  bad$type[bad$variable == "t2_cubes"] <- "likert"
  bad_file <- pr_path("bad_specification.csv")
  SIEVER:::sieve_spec_write(bad, bad_file)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    pr_upload(session, c("nine.csv", "bad_specification.csv"), c(csv, bad_file))
    expect_false(isTRUE(state$confirmed))
    expect_identical(state$meta$construct, rep("Unassigned", 9L))
    expect_identical(state$meta$status, rep("active", 9L))
    expect_false(any(state$meta$reverse %in% TRUE))
    expect_identical(state$items$t1_visual_perception, d$t1_visual_perception)
    expect_length(state$transforms, 0L)
  })

  # Two data files, or a file SIEVER does not read: refused, session unchanged.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    pr_upload(session, c("nine.csv", "again.csv"), c(csv, csv))
    expect_null(state$meta)
    expect_null(state$raw)
    session$setInputs(file = list(name = "notes.xlsx", datapath = csv), load = 2L)
    expect_null(state$meta)
    # A specification alone, with nothing loaded: nothing happens.
    session$setInputs(file = list(name = "nine_specification.csv", datapath = spec_file), load = 3L)
    expect_null(state$meta)
  })

  # A specification alone, with its data already loaded, restores onto them
  # and confirms the study.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    pr_upload(session, "nine.csv", csv)
    expect_false(isTRUE(state$confirmed))
    session$setInputs(file = list(name = "nine_specification.csv", datapath = spec_file), load = 2L)
    expect_true(isTRUE(state$confirmed))
    expect_identical(pr_ledger_of(state$meta), pr_expected_ledger(fx))
    expect_match(pr_strip(output$status_chip$html), "Confirmed from the specification", fixed = TRUE)
  })
})

test_that("a project file chosen in the data file input opens through the dialog's preview, then Open This Project", {
  fx <- sieve_hs9_fixture()
  path <- pr_path("nine.siever.rds")
  pr_quiet(study <- sieve_prepare_study(fx$data, pr_filled(fx), file = path, name = "Nine tests"))
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "file", basis = "theoretical", palette = "nd")
    session$setInputs(file = list(name = "nine.siever.rds", datapath = path), load = 1L)
    shown <- pr_strip(output$project_preview$html)
    expect_match(shown, "nine.siever.rds holds:", fixed = TRUE)
    expect_match(shown, "Nine tests", fixed = TRUE)
    expect_match(shown, "9 (8 included, 1 excluded)", fixed = TRUE)
    expect_match(shown, "yes (ML with FIML)", fixed = TRUE)
    expect_match(shown, "Open This Project", fixed = TRUE)
    # Nothing has changed until the button is pressed.
    expect_null(state$meta)
    expect_null(state$raw)
    session$setInputs(confirm_open = 1L)
    expect_identical(state$meta, study$meta)
    expect_identical(state$items, study$items)
    expect_identical(state$roles, study$roles)
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$project$source, "nine.siever.rds")
    expect_match(pr_strip(output$status_chip$html),
      "Confirmed from the specification: 8 continuous items, 3 constructs, 1 facet (ML with FIML). All tabs unlocked",
      fixed = TRUE
    )
  })

  # A project file is not combined with anything, and a file that is not a
  # project is refused in the dialog.
  csv <- pr_csv(fx$data)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    pr_upload(session, c("nine.siever.rds", "nine.csv"), c(path, csv))
    expect_null(state$meta)
    expect_null(output$project_preview$html)
    session$setInputs(file = list(name = "other.rds", datapath = csv), load = 2L)
    expect_match(pr_strip(output$project_preview$html),
      "other.rds was not opened, and the session is unchanged",
      fixed = TRUE
    )
    expect_null(state$meta)
  })
})

# ---- Constructs and facets in the data itself -----------------------------------

test_that("constructs and facets carried in the column names are proposed in the grid, as a proposal", {
  fx <- sieve_hs9_fixture()
  d <- fx$data
  renamed <- c(
    t1_visual_perception = "Visual__Shapes__t1_visual_perception",
    t2_cubes = "Visual__Shapes__t2_cubes",
    t4_lozenges = "Visual__t4_lozenges",
    t6_paragraph_comprehension = "Textual__Reading_and_meaning__t6_paragraph_comprehension",
    t7_sentence = "Textual__Reading.and.meaning__t7_sentence"
  )
  names(d)[match(names(renamed), names(d))] <- renamed
  csv <- pr_csv(d, "named.csv")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    pr_upload(session, "named.csv", csv)
    expect_identical(state$meta$item, names(d)[-(1:5)])
    expect_identical(
      state$meta$construct,
      c(
        "Visual",
        "Visual",
        "Visual",
        "Textual",
        "Textual",
        "Unassigned",
        "Unassigned",
        "Unassigned",
        "Unassigned"
      )
    )
    expect_identical(
      state$meta$facet,
      c("Shapes", "Shapes", NA, "Reading and meaning", "Reading and meaning", NA, NA, NA, NA)
    )
    # It is a proposal: not confirmed, and changed in the cell like any other.
    expect_false(isTRUE(state$confirmed))
    expect_match(pr_strip(output$status_chip$html), "press Confirm", fixed = TRUE)
    script <- paste(state$codelog, collapse = "\n")
    expect_match(script,
      "# Constructs and facets proposed from the column names (Construct__Facet__item), for review:",
      fixed = TRUE
    )
    expect_match(script, "#   Visual: 3 items (facets Shapes)", fixed = TRUE)
    expect_match(script, "#   Textual: 2 items (facets Reading and meaning)", fixed = TRUE)
    session$setInputs(cell_edit = list(item = "Visual__t4_lozenges", field = "construct", value = "Spatial"))
    expect_identical(state$meta$construct[3], "Spatial")
    # The columns named with a construct (and facet) prefix are items by
    # their numbered series, as they are without the prefix.
    expect_identical(state$roles$role[state$roles$variable %in% renamed], rep("item", 5L))
    expect_identical(sieve_role_vars(state, c("identifier", "group", "criterion", "antecedent", "auxiliary")), c("id", "sex", "grade", "age_years", "school"))
  })

  # The proposal itself: the forms it reads and the ones it leaves alone.
  got <- SIEVER:::sieve_structure_from_names(c(
    "A__B__x1",
    "A__x2",
    "x3",
    "A__B__C__x4",
    "A____x5",
    "Self_esteem__item.1"
  ))
  expect_identical(got$construct, c("A", "A", NA, NA, NA, "Self esteem"))
  expect_identical(got$facet, c("B", NA, NA, NA, NA, NA))
  expect_identical(got$source, c("name", "name", NA, NA, NA, "name"))
  # From labels: a shared, short head is a construct; a lone head, a head
  # with sentence punctuation, and a column whose name already says are not
  # read from the label.
  got <- SIEVER:::sieve_structure_from_names(
    c("q1", "q2", "q3", "q4", "q5", "A__q6"),
    c(
      "Climate: Openness: Ideas are welcomed here", "Climate: Openness: People speak freely here",
      "Q3: I like the product", "Note, please: something: else", "Note, please: and more",
      "Climate: Openness: named already"
    )
  )
  expect_identical(got$construct, c("Climate", "Climate", NA, NA, NA, "A"))
  expect_identical(got$facet, c("Openness", "Openness", NA, NA, NA, NA))
  expect_identical(got$wording, c("Ideas are welcomed here", "People speak freely here", NA, NA, NA, NA))
  expect_identical(got$source, c("label", "label", NA, NA, NA, "name"))
})

test_that("constructs and facets carried in the variable labels of a .sav are proposed, with the wording as the Item Name", {
  skip_if_not_installed("haven")
  fx <- sieve_hs9_fixture()
  d <- fx$data
  attr(d$t1_visual_perception, "label") <- "Visual: Shapes: Visual perception test"
  attr(d$t2_cubes, "label") <- "Visual: Shapes: Cubes"
  attr(d$t4_lozenges, "label") <- "Visual: Lozenges"
  attr(d$t6_paragraph_comprehension, "label") <- "Paragraph comprehension"
  sav <- pr_path("labeled.sav")
  haven::write_sav(d, sav)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    pr_upload(session, "labeled.sav", sav)
    m <- state$meta
    expect_identical(m$construct[1:4], c("Visual", "Visual", "Visual", "Unassigned"))
    expect_identical(m$facet[1:4], c("Shapes", "Shapes", NA, NA))
    expect_identical(m$label[1:4], c("Visual perception test", "Cubes", "Lozenges", "Paragraph comprehension"))
    expect_identical(m$wording[1:4], m$label[1:4])
    expect_false(isTRUE(state$confirmed))
    expect_match(paste(state$codelog, collapse = "\n"),
      "proposed from the variable labels (Construct: Facet: wording), for review:",
      fixed = TRUE
    )
  })
})

# ---- Refused uploads, the pool text, and constructs from column names ---------------

test_that("a refused upload leaves the session and the status chip as they were", {
  fx <- sieve_hs9_fixture()
  d <- fx$data
  csv <- pr_csv(d)
  spec_file <- pr_path("nine_specification.csv")
  SIEVER:::sieve_spec_write(pr_filled(fx), spec_file)
  wrong <- pr_filled(fx)
  wrong$variable[wrong$variable == "t9_word_meaning"] <- "t9_nope"
  wrong_file <- pr_path("wrong_specification.csv")
  SIEVER:::sieve_spec_write(wrong, wrong_file)
  project <- pr_path("nine.siever.rds")
  pr_quiet(sieve_prepare_study(d, pr_filled(fx), file = project))
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    pr_upload(session, c("nine.csv", "nine_specification.csv"), c(csv, spec_file))
    chip <- pr_strip(output$status_chip$html)
    expect_match(chip, "Confirmed from the specification: 8 continuous items", fixed = TRUE)
    meta <- state$meta
    # Why a Load changed nothing is said in the status line, under its
    # unchanged headline, not in a notification.
    headline <- function() {
      html <- as.character(output$status_chip$html)
      pr_strip(regmatches(html, regexpr("<span class=\"sieve-status-headline\">[^<]*</span>", html)))
    }
    first <- headline()
    unchanged <- function(why = NULL) {
      expect_identical(state$meta, meta)
      expect_true(isTRUE(state$confirmed))
      expect_identical(headline(), first)
      if (!is.null(why)) {
        expect_match(pr_strip(output$status_chip$html), why, fixed = TRUE)
        expect_match(as.character(output$status_chip$html), "sieve-status-danger", fixed = TRUE)
      }
    }
    # A file SIEVER does not read.
    session$setInputs(file = list(name = "notes.xlsx", datapath = csv), load = 2L)
    unchanged()
    expect_match(as.character(output$status_chip$html), "sieve-status-danger", fixed = TRUE)
    # A specification alone that names a variable the data do not have.
    session$setInputs(file = list(name = "wrong_specification.csv", datapath = wrong_file), load = 3L)
    unchanged()
    # Two data files at once.
    session$setInputs(
      file = data.frame(
        name = c("nine.csv", "again.csv"), size = 0, type = "",
        datapath = c(csv, csv), stringsAsFactors = FALSE
      ),
      load = 4L
    )
    unchanged()
    # A project file with another file.
    session$setInputs(
      file = data.frame(
        name = c("nine.siever.rds", "nine.csv"), size = 0, type = "",
        datapath = c(project, csv), stringsAsFactors = FALSE
      ),
      load = 5L
    )
    unchanged()
    # A data file that is read but holds no item column.
    none <- pr_csv(data.frame(id = 1:5, name = letters[1:5]), "none.csv")
    session$setInputs(file = list(name = "none.csv", datapath = none), load = 6L)
    # A data file would replace the study, so Load asks first: nothing yet.
    unchanged()
    session$setInputs(load_replace = 1L)
    unchanged("none.csv was not loaded: none of its 2 columns was proposed as an item")
    # A load that changes the session does change the chip.
    session$setInputs(file = list(name = "nine.csv", datapath = csv), load = 7L)
    session$setInputs(load_replace = 2L)
    expect_false(isTRUE(state$confirmed))
    expect_match(pr_strip(output$status_chip$html), "press Confirm", fixed = TRUE)
  })
})

test_that("the pool text lists three types as a list, and counts the included items only", {
  meta <- data.frame(
    item = paste0("i", 1:6),
    type = c("continuous", "continuous", "ordered", "ordered", "binary", "continuous"),
    status = "active", construct = "A", facet = NA_character_, stringsAsFactors = FALSE
  )
  expect_identical(SIEVER:::sieve_types_text(meta), "3 continuous, 2 ordered and 1 binary items")
  expect_identical(SIEVER:::sieve_pool_text(meta), "3 continuous, 2 ordered and 1 binary items, 1 construct, 0 facets")
  meta$type[5] <- "ordered"
  expect_identical(SIEVER:::sieve_types_text(meta), "3 continuous and 3 ordered items")
  meta$type[] <- "ordered"
  expect_identical(SIEVER:::sieve_types_text(meta), "6 ordered items")
  meta$status[2:6] <- "parked"
  expect_identical(SIEVER:::sieve_types_text(meta), "1 ordered item")
  expect_identical(SIEVER:::sieve_pool_text(meta), "1 ordered item, 1 construct, 0 facets")
  # A facet belongs to its construct: the same facet name under two
  # constructs is two facets, and an excluded item's facet is not counted.
  meta$status[] <- "active"
  meta$construct <- c("A", "A", "B", "B", "B", "C")
  meta$facet <- c("General", "General", "General", "Specific", NA, "General")
  expect_identical(SIEVER:::sieve_pool_text(meta), "6 ordered items, 3 constructs, 4 facets")
  meta$status[6] <- "parked"
  expect_identical(SIEVER:::sieve_pool_text(meta), "5 ordered items, 2 constructs, 3 facets")
})

test_that("a construct carried in the column names that is also a variable's name is not proposed, as the import refuses it", {
  d <- data.frame(
    id = 1:40,
    A = rep(c("x", "y"), 20),
    A__q1 = sin(1:40) * 10 + 50,
    A__q2 = cos(1:40) * 10 + 50,
    B__q3 = sin(1:40 / 2) * 10 + 50,
    stringsAsFactors = FALSE
  )
  got <- SIEVER:::sieve_structure_from_names(c("A__q1", "A__q2", "B__q3"), taken = names(d))
  expect_identical(got$construct, c(NA, NA, "B"))
  expect_identical(got$source, c(NA, NA, "name"))
  expect_identical(attr(got, "withheld"), "A")
  # With nothing taken, the same names propose A.
  expect_identical(SIEVER:::sieve_structure_from_names(c("A__q1", "A__q2", "B__q3"))$construct, c("A", "A", "B"))
  # The template proposes what the import will accept, so the template prepares as written.
  template <- sieve_specification_template(d)
  expect_identical(template$construct[template$role == "item"], c("", "", "B"))
  path <- pr_path("a.siever.rds")
  pr_quiet(study <- sieve_prepare_study(d, template, file = path))
  expect_identical(study$meta$construct, c("Unassigned", "Unassigned", "B"))
  # The tab says why, in the notification's words and in the session script.
  csv <- pr_csv(d, "a.csv")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    pr_upload(session, "a.csv", csv)
    expect_identical(state$meta$construct, c("Unassigned", "Unassigned", "B"))
    expect_match(paste(state$codelog, collapse = "\n"),
      "# The construct A carried in the column names has the name of a variable in the data, so it was not proposed",
      fixed = TRUE
    )
    expect_match(paste(state$codelog, collapse = "\n"), "#   B: 1 item", fixed = TRUE)
  })
})

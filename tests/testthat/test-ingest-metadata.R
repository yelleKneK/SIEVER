# The Ingest tab: the Item Name on the Variable roles panel, the study's
# Estimator control in step 4, five or more response options read as
# continuous at every way in, the distribution of one variable with every
# value, its mean and its SD, and the item grid following an exclusion made
# on another tab.
#
# The module tests drive the real observers with shiny::testServer; a
# table is read from the widget the output holds (its data and headers).

skip_if_not_installed("DMAR")
skip_if_not_installed("shiny")
skip_if_not_installed("jsonlite")

ns_strip <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(x), collapse = " ")))
ns_html <- function(out) ns_strip(out$html)

ns_path <- function(name) {
  path <- file.path(tempfile("ns_"), name)
  dir.create(dirname(path))
  path
}

# The data of a DT output, as a data frame of character columns named by
# the headers the table shows.
ns_dt_frame <- function(out) {
  x <- jsonlite::fromJSON(as.character(out), simplifyVector = FALSE)$x
  cols <- lapply(x$data, function(col) {
    vapply(col, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1))
  })
  heads <- regmatches(x$container, gregexpr("<th>[^<]*</th>", x$container))[[1]]
  heads <- gsub("</?th>", "", heads)
  names(cols) <- heads[seq_along(cols)]
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

# The zero based indexes of the columns a DT output hides.
ns_dt_hidden <- function(out) {
  defs <- jsonlite::fromJSON(as.character(out), simplifyVector = FALSE)$x$options$columnDefs
  hidden <- Filter(function(d) identical(d$visible, FALSE), defs)
  sort(as.integer(unlist(lapply(hidden, function(d) d$targets))))
}

# Optional local probes read private files from the directory that
# SIEVER_PRIVATE_PROFILE_DIR names and skip without it; they supplement the
# bundled and synthetic cases.
ns_profile <- function(name) siever_profile_file(name)

# ---- 1. The Item Name on the Variable roles panel ------------------------------

test_that("the Variable roles grid shows each item's Item Name after the variable, blank for the identifier, and the picker finds a column by it", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    grid <- ns_dt_frame(output$roles_grid)
    expect_identical(names(grid)[1:3], c("Variable Name", "Item Name", "Role"))
    expect_identical(
      grid[["Item Name"]][grid[["Variable Name"]] == "t2_cubes"],
      shiny::isolate(state$meta$label[state$meta$item == "t2_cubes"])
    )
    expect_identical(grid[["Item Name"]][grid[["Variable Name"]] == "t2_cubes"], "Cubes (spatial visualization)")
    expect_identical(grid[["Item Name"]][grid[["Variable Name"]] == "id"], "")
    expect_identical(grid[["Item Name"]][grid[["Variable Name"]] == "school"], "")
    # The question wording is a column too, offered under Columns shown.
    expect_true("Question" %in% names(grid))
    expect_match(grid$Question[grid[["Variable Name"]] == "t2_cubes"],
      shiny::isolate(state$meta$wording[state$meta$item == "t2_cubes"]),
      fixed = TRUE
    )
    # A rename in the item grid reaches the roles grid.
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "label", value = "Cube rotation"))
    grid <- ns_dt_frame(output$roles_grid)
    expect_identical(grid[["Item Name"]][grid[["Variable Name"]] == "t2_cubes"], "Cube rotation")
    # Columns shown: Item Name is ticked by default and the question is not.
    expect_true("Item Name" %in% SIEVER:::sieve_roles_grid_shown())
    expect_false("Question" %in% SIEVER:::sieve_roles_grid_shown())
    session$setInputs(roles_cols = SIEVER:::sieve_roles_grid_shown())
    expect_identical(ns_dt_hidden(output$roles_grid), 10L)
    session$setInputs(roles_cols = c("Item Name", "Role"))
    expect_identical(ns_dt_hidden(output$roles_grid), c(3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L))
    # The picker shows an item's Item Name after its variable; its values
    # are the variable names.
    pick <- SIEVER:::sieve_roles_pick_choices(names(state$raw), state$meta)
    expect_identical(unname(pick), names(state$raw))
    expect_identical(names(pick)[pick == "t2_cubes"], "t2_cubes (Cube rotation)")
    expect_identical(names(pick)[pick == "id"], "id")
  })
  # An item whose Item Name is its variable name is listed once.
  m <- data.frame(item = c("q1", "q2"), label = c("q1", "Second"), stringsAsFactors = FALSE)
  expect_identical(
    names(SIEVER:::sieve_roles_pick_choices(c("pid", "q1", "q2"), m)),
    c("pid", "q1", "q2 (Second)")
  )
})

# ---- The item grid follows an exclusion made elsewhere ------------------------

test_that("an item excluded on another tab shows as Dropped in the item grid, with the reason the ledger records", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    grid <- ns_dt_frame(output$type_grid)
    use <- grid$Use[grid[["Variable Name"]] == "t4_lozenges"]
    # The Use cell says Included.
    expect_match(use, "selected>Included", fixed = TRUE)
    # Excluded from outside the module, with a reason, as the Screening tab
    # and the Drop items control write it.
    shiny::isolate({
      m <- state$meta
      m$status[m$item == "t4_lozenges"] <- "parked"
      m$exclude_reason <- ifelse(m$item == "t4_lozenges", "Removed after the pilot: ambiguous wording", NA_character_)
      state$meta <- m
    })
    session$flushReact()
    grid <- ns_dt_frame(output$type_grid)
    use <- grid$Use[grid[["Variable Name"]] == "t4_lozenges"]
    # The Use cell and the group line say Included and Excluded, the house
    # words for items.
    expect_match(use, "selected>Excluded", fixed = TRUE)
    expect_match(use, "Removed after the pilot: ambiguous wording", fixed = TRUE)
    expect_identical(
      grid$Group[grid[["Variable Name"]] == "t4_lozenges"],
      "Items excluded from every analysis (kept in the data; set Use to Included to include one again)"
    )
    # Included again from outside: In use, above the line.
    shiny::isolate(state$meta$status[state$meta$item == "t4_lozenges"] <- "active")
    session$flushReact()
    grid <- ns_dt_frame(output$type_grid)
    expect_match(grid$Use[grid[["Variable Name"]] == "t4_lozenges"], "selected>Included", fixed = TRUE)
    expect_identical(grid$Group[grid[["Variable Name"]] == "t4_lozenges"], "Included items")
  })
})

# ---- 2. The study's estimator, chosen in step 4 -----------------------------------

test_that("step 4 holds the study's Estimator control with the four choices, following the item types by default", {
  html <- as.character(mod_ingest_ui("ingest"))
  expect_match(html, "id=\"ingest-estimator_choice\"", fixed = TRUE)
  expect_match(html, "Estimator for every model in the study", fixed = TRUE)
  for (choice in c("auto", "mlr", "ml", "wlsmv")) {
    expect_match(html, sprintf("value=\"%s\"", choice), fixed = TRUE)
  }
  expect_match(html, "<option value=\"auto\" selected>", fixed = TRUE)
  # The study's own control does not offer to follow the study.
  expect_false(grepl("value=\"study\"", html, fixed = TRUE))
  # Confirm stays in the main action bar above the panels.
  expect_lt(regexpr("ingest-confirm_button", html, fixed = TRUE),
    regexpr("ingest-estimator_choice", html, fixed = TRUE))
  expect_false(grepl('id="ingest-confirm"', html, fixed = TRUE))
  # The type help says where the control is.
  strip <- ns_strip(html)
  expect_match(strip,
    "The Estimator control in step 4 of the sidebar sets the estimator for every model in the study",
    fixed = TRUE
  )
  expect_false(grepl("The CFA tab's Estimator control can also fit", strip, fixed = TRUE))
})

test_that("choosing the study's estimator keeps the confirmation, records the estimator in force, is logged, and every tab that follows the study uses it", {
  updates <- list()
  local_mocked_bindings(updateSelectInput = function(session, inputId, ...) {
    updates[[length(updates) + 1L]] <<- c(list(id = inputId), list(...))
    invisible(NULL)
  }, .package = "SIEVER")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    expect_match(ns_html(output$estimator_box), "Load the data first (step 2)", fixed = TRUE)
    session$setInputs(load = 1L)
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$estimator, "ML with FIML")
    expect_match(ns_html(output$estimator_box), "following the item types", fixed = TRUE)
    steps_before <- sum(startsWith(state$codelog, "# ["))
    session$setInputs(estimator_choice = "wlsmv")
    expect_identical(state$estimator_choice, "wlsmv")
    # No type changed, so the study stays confirmed; the estimator recorded
    # with the confirmation is the one in force now.
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$estimator, "WLSMV (DWLS)")
    expect_identical(unique(state$meta$type), "continuous")
    expect_match(ns_html(output$confirm_button), "Confirmed on load (step 4): 9 continuous items, 3 constructs, 0 facets", fixed = TRUE)
    box <- ns_html(output$estimator_box)
    expect_match(box, "WLSMV (DWLS)", fixed = TRUE)
    expect_match(box, "set above for the study", fixed = TRUE)
    expect_match(box, "WLSMV was chosen with the Estimator control: every item is treated as ordered", fixed = TRUE)
    expect_match(box,
      "Each model tab (CFA, SEM, Reliability and the others) has the same Estimator control",
      fixed = TRUE
    )
    expect_match(ns_html(output$status_chip),
      "types confirmed (WLSMV (DWLS), the study's setting in step 4)",
      fixed = TRUE
    )
    # The Dataset overview's summary reads the same resolution.
    expect_identical(
      est()[c("estimator", "choice")],
      list(estimator = "WLSMV", choice = "wlsmv")
    )
    # One logged step that names the choice, in comments.
    expect_identical(sum(startsWith(state$codelog, "# [")), steps_before + 1L)
    log <- paste(state$codelog, collapse = "\n")
    expect_match(log,
      "Set the study's estimator: WLSMV on polychoric correlations (items treated as ordered)",
      fixed = TRUE
    )
    expect_match(log,
      "# The study's estimator setting (Ingest tab, step 4) is now \"wlsmv\" (it was \"auto\").",
      fixed = TRUE
    )
    # Every tab that follows the study fits under it.
    expect_identical(sieve_estimator_effective("study", state), "wlsmv")
    spec <- sieve_cfa_spec(state$meta,
      reserved = names(state$raw),
      estimator = sieve_estimator_effective("study", state)
    )
    expect_identical(spec$estimator, "WLSMV")
    expect_setequal(spec$ordered, state$meta$item)
    # Back to the item types: ML with FIML again (DMAR's default), and the
    # chip says nothing more.
    session$setInputs(estimator_choice = "auto")
    expect_identical(state$estimator, "ML with FIML")
    expect_match(ns_html(output$status_chip), "types confirmed (ML with FIML). All tabs unlocked", fixed = TRUE)
    # A confirmation made under a chosen estimator records it.
    session$setInputs(estimator_choice = "ml")
    session$setInputs(cell_edit = list(item = "t7_sentence", field = "type", value = "ordered"))
    expect_false(isTRUE(state$confirmed))
    session$setInputs(confirm_main = 1L)
    expect_identical(state$estimator, "ML (complete cases)")
    log <- paste(state$codelog, collapse = "\n")
    expect_match(log, "estimator = ML (complete cases) on a Pearson matrix", fixed = TRUE)
    expect_match(log,
      "# Set for every model in the study with the Estimator control (Ingest tab, step 4): ML on the complete cases (items treated as continuous).",
      fixed = TRUE
    )
    # A new dataset starts from the item types again, and the control is told.
    updates <<- list()
    # With a study loaded, Load asks first (the dialog's Load and replace).
    session$setInputs(load = 2L)
    session$setInputs(load_replace = 1L)
    expect_identical(state$estimator_choice, "auto")
    expect_identical(state$estimator, "ML with FIML")
    # Logged after the load it belongs to, and before the confirmation the
    # load makes (the return is read before the Load's reset).
    stamps <- sub("^# \\[[0-9]+\\] ", "", grep("^# \\[", state$codelog, value = TRUE))
    at_load <- max(which(startsWith(stamps, "Load bundled example")))
    expect_identical(stamps[at_load + 1L], "The study's estimator follows the item types for the new data")
    expect_match(stamps[at_load + 2L], "Auto-confirm item types", fixed = TRUE)
    # Said in the status line as well, once.
    expect_identical(lengths(regmatches(ns_html(output$status_chip),
      gregexpr("The study's estimator follows the item types again for the new data (it was set to ML on the complete cases",
        ns_html(output$status_chip), fixed = TRUE))), 1L)
    expect_identical(state$estimator_choice, "auto")
    sent <- Filter(function(u) identical(u$id, "estimator_choice"), updates)
    expect_true(length(sent) >= 1L)
    expect_identical(sent[[length(sent)]]$selected, "auto")
  })
})

test_that("the project file keeps the study's estimator setting, and a project saved without one follows the item types", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    session$setInputs(estimator_choice = "mlr")
  })
  snap <- SIEVER:::sieve_project_snapshot(st)
  expect_identical(snap$study_estimator_choice, "mlr")
  expect_false("estimator_choice" %in% names(snap$other))
  # Under a name that does not begin with "estimator", so that snap$estimator
  # never returns it by partial matching.
  expect_false(any(startsWith(setdiff(names(snap), "estimator"), "estimator")))
  expect_identical(SIEVER:::sieve_project_check(snap), character(0))
  path <- ns_path("choice.siever.rds")
  SIEVER:::sieve_project_write(snap, path)
  back <- SIEVER:::sieve_project_read(path)
  expect_identical(back$study_estimator_choice, "mlr")
  expect_identical(
    SIEVER:::sieve_project_describe(back)[["estimator_setting"]],
    "MLR with FIML (items treated as continuous; for markedly non-normal items)"
  )
  # Opened through the dialog: the setting is the saved one, and the
  # control is told.
  updates <- list()
  local_mocked_bindings(updateSelectInput = function(session, inputId, ...) {
    updates[[length(updates) + 1L]] <<- c(list(id = inputId), list(...))
    invisible(NULL)
  }, .package = "SIEVER")
  opened <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = opened), {
    session$setInputs(source = "ready", basis = "theoretical", palette = "nd")
    session$setInputs(open_project = 1L)
    session$setInputs(project_file = list(name = "choice.siever.rds", datapath = path))
    session$setInputs(confirm_open = 1L)
    expect_identical(state$estimator_choice, "mlr")
    sent <- Filter(function(u) identical(u$id, "estimator_choice"), updates)
    expect_identical(sent[[length(sent)]]$selected, "mlr")
    expect_match(ns_html(output$estimator_box), "set above for the study", fixed = TRUE)
  })
  # A project that does not hold the setting (no field, and the content key
  # it was saved with) follows the item types; when its confirmation
  # recorded MLR with FIML for continuous items, it opens with MLR with FIML
  # kept, recorded and noted (sieve_keep_legacy_mlr()), not refit under ML
  # with FIML.
  old <- unclass(snap)
  old$study_estimator_choice <- NULL
  old$keys$content <- SIEVER:::sieve_content_key(old)
  old <- structure(old, class = "sieve_project")
  expect_identical(SIEVER:::sieve_project_check(old), character(0))
  expect_identical(old$estimator, "MLR with FIML")
  fresh <- sieve_new_state()
  shiny::isolate(fresh$estimator_choice <- "wlsmv")
  SIEVER:::sieve_project_restore(fresh, old)
  expect_identical(shiny::isolate(fresh$estimator_choice), "mlr")
  expect_false(is.null(shiny::isolate(fresh$project$estimator_kept)))
  # One saved without the setting whose confirmation recorded another
  # estimator follows the item types.
  older <- unclass(snap)
  older$study_estimator_choice <- NULL
  older$estimator <- "ML with FIML"
  older$keys$content <- SIEVER:::sieve_content_key(older)
  older <- structure(older, class = "sieve_project")
  fresh <- sieve_new_state()
  shiny::isolate(fresh$estimator_choice <- "wlsmv")
  SIEVER:::sieve_project_restore(fresh, older)
  expect_identical(shiny::isolate(fresh$estimator_choice), "auto")
  expect_null(shiny::isolate(fresh$project$estimator_kept))
  # One that holds the setting among the additional fields opens with it.
  between <- unclass(snap)
  between$study_estimator_choice <- NULL
  between$other$estimator_choice <- "wlsmv"
  between$keys$content <- SIEVER:::sieve_content_key(between)
  between <- structure(between, class = "sieve_project")
  expect_identical(SIEVER:::sieve_project_check(between), character(0))
  SIEVER:::sieve_project_restore(fresh, between)
  expect_identical(shiny::isolate(fresh$estimator_choice), "wlsmv")
  # A setting that is not one of the choices is refused.
  bad <- unclass(snap)
  bad$study_estimator_choice <- "dwls"
  bad$keys$content <- SIEVER:::sieve_content_key(bad)
  # The choices include ML with FIML ("ml_fiml").
  expect_match(paste(SIEVER:::sieve_project_check(bad), collapse = "; "),
    "the study's estimator setting is not one of auto, mlr, ml_fiml, ml, wlsmv",
    fixed = TRUE
  )
})

test_that("respondents removed from the analysis do not ask for the confirmation again", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1L)
    expect_true(isTRUE(state$confirmed))
    # The Screening tab removes five respondents: the rows of the data and
    # of the item data, and the record of the removal.
    sieve_remove_cases(state, 1:5, "straight lining")
    session$flushReact()
    expect_identical(confirmed_at()$data_key, SIEVER:::sieve_data_key(shiny::isolate(state$items)))
    # An exclusion made and undone afterwards leaves the study as it was
    # confirmed: no change is reported, the removal included.
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "status", value = "Dropped"))
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "status", value = "In use"))
    expect_true(isTRUE(state$confirmed))
    button <- ns_html(output$confirm_button)
    expect_match(button, "Confirmed on load (step 4): 9 continuous items, 3 constructs, 0 facets", fixed = TRUE)
    expect_false(grepl("item values changed", button, fixed = TRUE))
    # A respondent restored: the same.
    sieve_restore_cases(state, 5L)
    session$flushReact()
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "status", value = "Dropped"))
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "status", value = "In use"))
    expect_match(ns_html(output$confirm_button), "Confirmed on load (step 4): 9 continuous items", fixed = TRUE)
  })
})

# ---- 3. Five or more response options are continuous at every way in -------------

# Seven point items q1 to q4; q5 a four point item; q6 a seven point item
# on which only the points 2 to 5 were chosen (its bounds, 1 to 7, are
# declared in the specification); an identifier.
ns_seven_point <- function(n = 240L) {
  withr::with_seed(20261015, {
    z <- stats::rnorm(n)
    cut7 <- function(x) as.integer(cut(x, c(-Inf, -1.5, -0.8, -0.25, 0.25, 0.8, 1.5, Inf)))
    d <- data.frame(
      pid = sprintf("P%03d", seq_len(n)),
      q1 = cut7(0.7 * z + stats::rnorm(n, sd = 0.7)),
      q2 = cut7(0.7 * z + stats::rnorm(n, sd = 0.7)),
      q3 = cut7(0.6 * z + stats::rnorm(n, sd = 0.8)),
      q4 = cut7(0.6 * z + stats::rnorm(n, sd = 0.8)),
      q5 = as.integer(cut(0.6 * z + stats::rnorm(n, sd = 0.8), c(-Inf, -0.6, 0, 0.6, Inf))),
      q6 = as.integer(cut(0.6 * z + stats::rnorm(n, sd = 0.8), c(-Inf, -0.6, 0, 0.6, Inf))) + 1L,
      stringsAsFactors = FALSE
    )
  })
  d
}
ns_seven_spec <- function(d) {
  spec <- sieve_specification_template(d)
  items <- spec$role == "item"
  spec$type[items] <- "ordered"
  spec$construct[items] <- "Trust"
  spec$rev_lo[spec$variable == "q6"] <- "1"
  spec$rev_hi[spec$variable == "q6"] <- "7"
  spec
}

test_that("an item's response options are its distinct values or the points its declared bounds span, and the rule switches only ordered items with five or more", {
  expect_identical(SIEVER:::sieve_response_options(c(1, 2, 4, 7, 7, NA)), 4L)
  expect_identical(SIEVER:::sieve_response_options(c(2, 3, 4, 5), lo = 1, hi = 7), 7L)
  expect_identical(SIEVER:::sieve_response_options(c(1, 2, 3, 4, 5, 6, 7), lo = 1, hi = 5), 7L)
  # Values that are not integers: their distinct values; bounds add nothing.
  expect_identical(SIEVER:::sieve_response_options(c(1.5, 2.25, 3), lo = 1, hi = 7), 3L)
  expect_identical(SIEVER:::sieve_response_options(c(NA, NA)), 0L)
  meta <- data.frame(
    item = c("a", "b", "c", "d", "e", "f"),
    type = c("ordered", "ordered", "ordered", "binary", "continuous", "ordered"),
    rev_lo = c(NA, 1, 1, 1, NA, 1), rev_hi = c(NA, 7, 7, 7, NA, 7),
    rev_bounds = c(NA, "declared", "observed", "declared", NA, "declared"),
    stringsAsFactors = FALSE
  )
  items <- data.frame(
    a = c(1:7, 1), b = c(2, 3, 4, 5, 2, 3, 4, 5), c = c(2, 3, 4, 5, 2, 3, 4, 5),
    d = c(1, 7, 1, 7, 1, 7, 1, 7), e = c(1:7, 7), f = c(1, 2, 1, 2, 1, 2, 1, 2)
  )
  tab <- SIEVER:::sieve_item_options(items, meta)
  expect_identical(tab$options, c(7L, 7L, 4L, 7L, 7L, 7L))
  expect_identical(tab$observed, c(7L, 4L, 4L, 2L, 7L, 2L))
  rule <- SIEVER:::sieve_option_rule(items, meta)
  # a: seven observed; b: four observed within declared bounds 1 to 7. Not c
  # (bounds read from the observed range), not d (binary), not e (already
  # continuous), not f (two observed values: binary in the data).
  expect_identical(rule$switched, c("a", "b"))
  expect_identical(SIEVER:::sieve_option_rule(items, meta, candidates = "b")$switched, "b")
  text <- SIEVER:::sieve_option_rule_text(c("a", "b"), "the project")
  expect_match(text,
    "2 items (a, b) typed ordered in the project have five or more response options and were read as continuous",
    fixed = TRUE
  )
  expect_match(text, "Rhemtulla, Brosseau-Liard & Savalei, 2012", fixed = TRUE)
  expect_match(text,
    "set Type to ordered in the grid, or turn off the five-option checkbox in step 1 before loading",
    fixed = TRUE
  )
  expect_null(SIEVER:::sieve_option_rule_text(character(0)))
})

test_that("a specification that types seven point items ordered loads them as continuous with the checkbox on, and as ordered with it off", {
  d <- ns_seven_point()
  csv <- ns_path("seven.csv")
  utils::write.csv(d, csv, row.names = FALSE)
  spec_file <- ns_path("seven_specification.csv")
  SIEVER:::sieve_spec_write(ns_seven_spec(d), spec_file)
  files <- data.frame(
    name = c("seven.csv", "seven_specification.csv"), size = 0, type = "",
    datapath = c(csv, spec_file), stringsAsFactors = FALSE
  )
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "pair", basis = "theoretical", palette = "nd", spec_keying = "verify",
      five_continuous = TRUE
    )
    session$setInputs(file = files)
    session$setInputs(load = 1L)
    types <- stats::setNames(state$meta$type, state$meta$item)
    expect_identical(types, c(
      q1 = "continuous", q2 = "continuous", q3 = "continuous", q4 = "continuous",
      q5 = "ordered", q6 = "continuous"
    ))
    # q6 has four observed values and seven options by its declared bounds.
    expect_identical(state$meta$n_levels[state$meta$item == "q6"], 4L)
    expect_true(isTRUE(state$confirmed))
    chip <- ns_html(output$status_chip)
    expect_match(chip,
      "5 items (q1, q2, q3, q4, q6) typed ordered in the specification have five or more response options and were read as continuous",
      fixed = TRUE
    )
    expect_match(chip, "(Rhemtulla, Brosseau-Liard &amp; Savalei, 2012)", fixed = TRUE)
    expect_match(ns_html(output$grid_note), "turn off the five-option checkbox in step 1 before loading", fixed = TRUE)
    log <- paste(state$codelog, collapse = "\n")
    expect_match(log,
      "# Typed ordered in the specification and read as continuous by the five-option rule",
      fixed = TRUE
    )
    expect_match(log, "#   q6 (7 options): continuous", fixed = TRUE)
  })
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "pair", basis = "theoretical", palette = "nd", spec_keying = "verify",
      five_continuous = FALSE
    )
    session$setInputs(file = files)
    session$setInputs(load = 1L)
    expect_identical(unique(state$meta$type), "ordered")
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$estimator, "WLSMV (DWLS)")
    expect_false(grepl("five or more response options", ns_html(output$status_chip), fixed = TRUE))
  })
  # The same specification applied to loaded data (step 3) follows the checkbox too.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(
      source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify",
      five_continuous = TRUE
    )
    session$setInputs(file = list(name = "seven.csv", datapath = csv))
    session$setInputs(load = 1L)
    session$setInputs(up_spec = list(name = "seven_specification.csv", datapath = spec_file))
    expect_identical(state$meta$type[state$meta$item %in% c("q5", "q6")], c("ordered", "continuous"))
    expect_match(ns_html(output$status_chip), "typed ordered in the specification have five or more", fixed = TRUE)
  })
})

test_that("a private study's specification pair loads as 68 continuous items under ML with FIML with the checkbox on, and as 68 ordered under WLSMV with it off", {
  data_path <- ns_profile("ethics.csv")
  spec_path <- ns_profile("ethics_specification.csv")
  skip_if(is.na(data_path) || is.na(spec_path), "the private study of 68 items is not available")
  files <- data.frame(
    name = c("ethics.csv", "ethics_specification.csv"), size = 0, type = "",
    datapath = c(data_path, spec_path), stringsAsFactors = FALSE
  )
  for (on in c(TRUE, FALSE)) {
    st <- sieve_new_state()
    shiny::testServer(mod_ingest_server, args = list(state = st), {
      session$setInputs(
        source = "pair", basis = "theoretical", palette = "nd", spec_keying = "verify",
        five_continuous = on
      )
      session$setInputs(file = files)
      session$setInputs(load = 1L)
      expect_identical(nrow(state$meta), 68L)
      expect_true(isTRUE(state$confirmed))
      if (on) {
        expect_identical(unique(state$meta$type), "continuous")
        expect_identical(state$estimator, "ML with FIML")
        expect_match(ns_html(output$status_chip), "68 items (", fixed = TRUE)
        expect_match(ns_html(output$status_chip), "Confirmed from the specification: 68 continuous items", fixed = TRUE)
      } else {
        expect_identical(unique(state$meta$type), "ordered")
        expect_identical(state$estimator, "WLSMV (DWLS)")
      }
    })
    spec <- sieve_cfa_spec(shiny::isolate(st$meta), reserved = names(shiny::isolate(st$raw)))
    # Continuous items are fit by ML with FIML, DMAR's default, which the
    # page's own estimator label above already states.
    expect_identical(spec$estimator, if (on) "ML" else "WLSMV")
    expect_identical(spec$missing, if (on) "fiml" else "listwise")
  }
})

test_that("an opened project follows the five-option rule for its proposed ordered types and keeps the chosen ones", {
  # A saved project follows the five-option rule, a type the researcher chose
  # is kept (type_source "chosen"), and the switch is made visible: before the
  # file is opened, on the status chip, above the item grid with one click
  # that keeps the items ordered, and in the log.
  d <- ns_seven_point()
  spec <- ns_seven_spec(d)
  path <- ns_path("seven.siever.rds")
  # With the rule off, the specification's types are the project's own
  # choices.
  invisible(utils::capture.output(study <- sieve_prepare_study(d, spec, file = path, continuous_from = NULL)))
  expect_identical(unique(study$meta$type), "ordered")
  expect_identical(unique(study$meta$type_source[study$meta$item %in% paste0("q", 1:6)]), "chosen")
  expect_true(study$confirmed)
  expect_identical(study$estimator, "WLSMV (DWLS)")
  # The same project without a record of where its types came from: its
  # types read as proposed.
  old <- ns_path("seven_old.siever.rds")
  st0 <- sieve_new_state()
  sieve_project_restore(st0, sieve_project_read(path))
  shiny::isolate({
    m <- st0$meta
    m$type_source <- NULL
    st0$meta <- m
  })
  sieve_project_write(sieve_project_snapshot(st0), old)

  # Checkbox on, proposed types: q1 to q4 (seven observed) and q6 (four of
  # seven declared points observed) open as continuous; q5 (four options)
  # stays ordered.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "ready", basis = "theoretical", palette = "nd", five_continuous = TRUE)
    session$setInputs(open_project = 1L)
    session$setInputs(project_file = list(name = "seven_old.siever.rds", datapath = old))
    preview <- ns_html(output$project_preview)
    expect_match(preview, "5 items (q1, q2, q3, q4, q6) typed ordered with five or more response options will open as continuous", fixed = TRUE)
    expect_match(preview, "The study's confirmation will be renewed with them as continuous.", fixed = TRUE)
    session$setInputs(confirm_open = 1L)
    expect_identical(
      stats::setNames(state$meta$type, state$meta$item)[paste0("q", 1:6)],
      c(q1 = "continuous", q2 = "continuous", q3 = "continuous", q4 = "continuous", q5 = "ordered", q6 = "continuous")
    )
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$estimator, "WLSMV (DWLS)")
    expect_match(ns_html(output$status_chip),
      "5 items (q1, q2, q3, q4, q6) typed ordered in the project have five or more response options and were read as continuous",
      fixed = TRUE
    )
    expect_match(ns_html(output$types_switched), "Item types switched on opening.", fixed = TRUE)
    expect_match(ns_html(output$types_switched), "the study was confirmed again with these items as continuous", fixed = TRUE)
    log <- paste(state$codelog, collapse = "\n")
    expect_match(log, "Read 5 items typed ordered in the project as continuous", fixed = TRUE)
    expect_match(log, "Confirm item types again after the five-option rule", fixed = TRUE)
    # One click keeps them ordered, recorded as the researcher's choice,
    # and the study is confirmed again with the types it was saved with.
    session$setInputs(keep_ordered = 1L)
    expect_identical(unique(state$meta$type[state$meta$item %in% paste0("q", 1:6)]), "ordered")
    expect_identical(
      unique(sieve_type_source(state$meta)[state$meta$item %in% c("q1", "q2", "q3", "q4", "q6")]),
      "chosen"
    )
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$estimator, "WLSMV (DWLS)")
    expect_false(grepl("Item types switched on opening.", ns_html(output$types_switched), fixed = TRUE))
  })

  # Checkbox on, chosen types: kept as saved, and the preview says so.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "ready", basis = "theoretical", palette = "nd", five_continuous = TRUE)
    session$setInputs(open_project = 1L)
    session$setInputs(project_file = list(name = "seven.siever.rds", datapath = path))
    expect_false(grepl("will open as continuous", ns_html(output$project_preview), fixed = TRUE))
    expect_match(ns_html(output$project_preview), "Kept ordered as your choice: q1, q2, q3, q4, q6.", fixed = TRUE)
    session$setInputs(confirm_open = 1L)
    expect_identical(
      stats::setNames(state$meta$type, state$meta$item)[c("q1", "q5", "q6")],
      c(q1 = "ordered", q5 = "ordered", q6 = "ordered")
    )
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$estimator, "WLSMV (DWLS)")
    expect_match(ns_html(output$confirm_button), "6 ordered items", fixed = TRUE)
    expect_false(any(grepl("Confirm item types again after the five-option rule", state$codelog, fixed = TRUE)))
  })
  # With the checkbox off the project opens as it was saved, whatever its
  # types' source.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "ready", basis = "theoretical", palette = "nd", five_continuous = FALSE)
    session$setInputs(open_project = 1L)
    session$setInputs(project_file = list(name = "seven_old.siever.rds", datapath = old))
    expect_match(ns_html(output$project_preview), "Kept as saved (the five-option checkbox", fixed = TRUE)
    session$setInputs(confirm_open = 1L)
    expect_identical(unique(state$meta$type), "ordered")
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$estimator, "WLSMV (DWLS)")
  })
  # The pre-SIEVER step applies the same rule by default.
  other <- ns_path("seven_default.siever.rds")
  printed <- utils::capture.output(study <- sieve_prepare_study(d, spec, file = other))
  expect_identical(
    stats::setNames(study$meta$type, study$meta$item)[c("q1", "q5", "q6")],
    c(q1 = "continuous", q5 = "ordered", q6 = "continuous")
  )
  expect_true(any(grepl("Typed ordered in the specification and read as continuous (five or more response options): 5 items (q1, q2, q3, q4, q6)",
    printed,
    fixed = TRUE
  )))
  expect_error(sieve_prepare_study(d, spec, file = ns_path("x.siever.rds"), continuous_from = "five"),
    "continuous_from",
    fixed = TRUE
  )
})

test_that("a private project of 68 items keeps its saved types and estimator when reopened with the five-option rule off, and says so when the rule reads them as continuous", {
  project <- ns_profile("ethics.siever.rds")
  skip_if(is.na(project), "the private project of 68 items is not available")
  requireNamespace("haven", quietly = TRUE)
  saved <- sieve_project_read(project)
  # With the five-option rule off the saved types and estimator are kept.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "ready", basis = "theoretical", palette = "nd", five_continuous = FALSE)
    session$setInputs(open_project = 1L)
    session$setInputs(project_file = list(name = "ethics.siever.rds", datapath = project))
    session$setInputs(confirm_open = 1L)
    expect_identical(state$meta$type, saved$meta$type)
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$estimator, saved$estimator)
    expect_match(ns_html(output$confirm_button), "Confirmed:", fixed = TRUE)
  })
  # With the rule on (the default), a project with no record of who chose
  # its types has its ordered items with five or more response options read
  # as continuous, the study is confirmed
  # again under the switched types, and the switch is stated
  # (sieve_project_open()).
  st2 <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st2), {
    session$setInputs(source = "ready", basis = "theoretical", palette = "nd", five_continuous = TRUE)
    session$setInputs(open_project = 1L)
    session$setInputs(project_file = list(name = "ethics.siever.rds", datapath = project))
    session$setInputs(confirm_open = 1L)
    expect_true(all(saved$meta$type == "ordered"))
    expect_true(all(state$meta$type == "continuous"))
    expect_true(isTRUE(state$confirmed))
    expect_identical(state$estimator, "ML with FIML")
    switched <- state$project$switched_types
    expect_length(switched$items, length(saved$meta$item))
    expect_true(isTRUE(switched$confirmed_again))
  })
})

test_that("the two buttons above the grid set the type of every item by its number of response options, and say when nothing changes", {
  d <- ns_seven_point()
  csv <- ns_path("seven.csv")
  utils::write.csv(d, csv, row.names = FALSE)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "alone", basis = "theoretical", palette = "nd", five_continuous = TRUE)
    session$setInputs(file = list(name = "seven.csv", datapath = csv))
    session$setInputs(load = 1L)
    # Detection: seven observed values continuous, four observed values ordered.
    expect_identical(
      stats::setNames(state$meta$type, state$meta$item),
      c(
        q1 = "continuous", q2 = "continuous", q3 = "continuous", q4 = "continuous",
        q5 = "ordered", q6 = "ordered"
      )
    )
    session$setInputs(confirm_main = 1L)
    expect_true(isTRUE(state$confirmed))
    steps <- sum(startsWith(state$codelog, "# ["))
    # Nothing to change either way: said, nothing logged, still confirmed.
    session$setInputs(types_continuous = 1L)
    expect_match(ns_html(output$grid_note),
      "No item typed ordered has five or more response options, so no type was changed.",
      fixed = TRUE
    )
    session$setInputs(types_ordered = 1L)
    expect_match(ns_html(output$grid_note),
      "No item typed continuous has four or fewer response options, so no type was changed.",
      fixed = TRUE
    )
    expect_identical(sum(startsWith(state$codelog, "# [")), steps)
    expect_true(isTRUE(state$confirmed))
    # Every item to ordered by hand, then the five or more to continuous at once.
    for (it in c("q1", "q2", "q3", "q4")) {
      session$setInputs(cell_edit = list(item = it, field = "type", value = "ordered"))
    }
    session$setInputs(cell_edit = list(item = "q5", field = "type", value = "continuous"))
    session$setInputs(confirm_main = 2L)
    session$setInputs(types_continuous = 2L)
    expect_identical(
      state$meta$type[state$meta$item %in% c("q1", "q2", "q3", "q4", "q6")],
      c("continuous", "continuous", "continuous", "continuous", "ordered")
    )
    expect_false(isTRUE(state$confirmed))
    expect_match(ns_html(output$grid_note),
      "4 items (q1, q2, q3, q4) with five or more response options are typed continuous now",
      fixed = TRUE
    )
    expect_match(
      paste(state$codelog,
        collapse = "\n"
      ),
      "Set the type of 4 items with five or more response options to continuous",
      fixed = TRUE
    )
    # The four or fewer to ordered: q5, typed continuous by hand.
    session$setInputs(types_ordered = 2L)
    expect_identical(state$meta$type[state$meta$item == "q5"], "ordered")
    expect_match(ns_html(output$grid_note),
      "1 item (q5) with four or fewer response options is typed ordered now",
      fixed = TRUE
    )
    grid <- ns_dt_frame(output$type_grid)
    expect_match(grid$Type[grid[["Variable Name"]] == "q5"], "selected>ordered", fixed = TRUE)
  })
  html <- as.character(mod_ingest_ui("ingest"))
  expect_match(html, "Set every item with five or more options to continuous", fixed = TRUE)
  expect_match(html, "Set every item with four or fewer options to ordered", fixed = TRUE)
  expect_match(html,
    "<input id=\"ingest-five_continuous\" type=\"checkbox\" class=\"shiny-input-checkbox\" checked=\"checked\"/>",
    fixed = TRUE
  )
  expect_match(ns_strip(html), "Treat an item with five or more response options as continuous", fixed = TRUE)
  # An opened project follows the rule for the types SIEVER proposed and keeps
  # the types chosen.
  expect_match(ns_strip(html), "In an opened project only types SIEVER proposed", fixed = TRUE)
  expect_match(ns_strip(html), "a type you chose is kept", fixed = TRUE)
  expect_match(ns_strip(html), "turn the checkbox off before loading", fixed = TRUE)
})

# ---- 4. The distribution of one variable: every value, the mean and the SD --------

test_that("a seven point item is drawn with a bar per value, the unchosen points of its bounds included, and n, the mean and the SD stated", {
  x <- c(1, 2, 2, 3, 3, 3, 5, 5, 6, NA, NA)
  d <- SIEVER:::sieve_distribution_data(x, "q1", role = "item", label = "Trust in the system", lo = 1, hi = 7)
  expect_identical(d$kind, "bars")
  # Every point of 1 to 7, the 4 and the 7 that no one chose as zero bars.
  expect_identical(d$at, as.numeric(1:7))
  expect_identical(d$breaks, as.numeric(1:7))
  expect_identical(d$counts, c(1L, 2L, 3L, 0L, 2L, 1L, 0L))
  expect_identical(d$n, 9L)
  expect_identical(d$missing, 2L)
  expect_equal(d$mean, mean(x, na.rm = TRUE), tolerance = 1e-12)
  expect_equal(d$sd, stats::sd(x, na.rm = TRUE), tolerance = 1e-12)
  expect_identical(d$title, "q1 (item): Trust in the system")
  expect_identical(d$subtitle, sprintf(
    "n = 9, 2 missing\nmean %.2f (the vertical line), SD %.2f",
    mean(x, na.rm = TRUE), stats::sd(x, na.rm = TRUE)
  ))
  # Without bounds: from the lowest to the highest value seen, the gap at 4 included.
  expect_identical(SIEVER:::sieve_distribution_data(x, "q1")$at, as.numeric(1:6))
  # A 0/1 item: two bars, the mean a proportion.
  b <- SIEVER:::sieve_distribution_data(c(0, 1, 1, 1), "b")
  expect_identical(b$at, c(0, 1))
  expect_identical(b$counts, c(1L, 3L))
  expect_equal(b$mean, 0.75, tolerance = 1e-12)
  # Whole numbers far apart: a bar per value seen, each labeled.
  w <- SIEVER:::sieve_distribution_data(c(0, 100, 100, 250), "w")
  expect_identical(w$at, c(0, 100, 250))
  # Many distinct values, or values that are not whole numbers: a histogram on pretty breaks.
  h <- SIEVER:::sieve_distribution_data(seq(0.5, 30.5, by = 0.25), "score", role = "criterion")
  expect_identical(h$kind, "histogram")
  expect_identical(
    h$breaks,
    pretty(
      range(seq(0.5,
        30.5,
        by = 0.25
      )),
      n = grDevices::nclass.Sturges(seq(0.5,
        30.5,
        by = 0.25
      ))
    )
  )
  expect_identical(sum(h$counts), length(seq(0.5, 30.5, by = 0.25)))
  expect_match(h$subtitle, "mean 15.50 (the vertical line), SD", fixed = TRUE)
  # Not a number: a bar per level, with n and the number missing.
  g <- SIEVER:::sieve_distribution_data(factor(c("a", "b", "b", NA)), "school", role = "group")
  expect_identical(g$kind, "levels")
  expect_identical(g$at, c("a", "b"))
  expect_identical(g$counts, c(1L, 2L))
  expect_identical(g$subtitle, "n = 3, 1 missing; 2 levels (not stored as a number)")
  expect_identical(SIEVER:::sieve_distribution_data(c(NA, NA), "e")$kind, "empty")
})

test_that("the drawing labels every value on the axis, puts the counts above the bars, marks the mean and states the summary", {
  x <- c(1, 2, 2, 3, 3, 3, 5, 5, 6)
  d <- SIEVER:::sieve_distribution_data(x, "q1", role = "item", lo = 1, hi = 7)
  p <- SIEVER:::sieve_distribution_plot(d, col = "#0c2340", line_col = "#c99700")
  expect_s3_class(p, "ggplot")
  expect_identical(p$scales$get_scales("x")$breaks, as.numeric(1:7))
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true(all(c("GeomCol", "GeomText", "GeomVline") %in% geoms))
  vline <- p$layers[[which(geoms == "GeomVline")]]
  expect_equal(vline$data$xintercept, mean(x), tolerance = 1e-12)
  expect_identical(p$labels$subtitle, d$subtitle)
  expect_identical(p$labels$title, "q1 (item)")
  built <- ggplot2::ggplot_build(p)
  expect_identical(
    as.numeric(built$layout$panel_params[[1]]$x$breaks[!is.na(built$layout$panel_params[[1]]$x$breaks)]),
    as.numeric(1:7)
  )
  # ggplot2 is imported, so the drawing does not ask whether it is
  # installed: with sieve_have() answering no, it is still the same ggplot,
  # with the same subtitle.
  out <- local({
    local_mocked_bindings(sieve_have = function(pkg) FALSE, .package = "SIEVER")
    SIEVER:::sieve_distribution_plot(d)
  })
  expect_s3_class(out, "ggplot")
  expect_identical(out$labels$subtitle, d$subtitle)
})

test_that("the Dataset overview draws an item with its Item Name, its declared bounds and its summary", {
  d <- ns_seven_point()
  csv <- ns_path("seven.csv")
  utils::write.csv(d, csv, row.names = FALSE)
  spec_file <- ns_path("seven_specification.csv")
  SIEVER:::sieve_spec_write(ns_seven_spec(d), spec_file)
  plotted <- list()
  real <- SIEVER:::sieve_distribution_plot
  local_mocked_bindings(sieve_distribution_plot = function(d, ...) {
    plotted[[length(plotted) + 1L]] <<- d
    real(d, ...)
  }, .package = "SIEVER")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "pair", basis = "theoretical", palette = "nd", spec_keying = "verify")
    session$setInputs(file = data.frame(
      name = c("seven.csv", "seven_specification.csv"), size = 0, type = "",
      datapath = c(csv, spec_file), stringsAsFactors = FALSE
    ))
    session$setInputs(load = 1L)
    session$setInputs(cell_edit = list(item = "q6", field = "label", value = "Fairness of outcomes"))
    session$setInputs(plot_var = "q6")
    expect_no_error(output$dist_plot)
    shown <- plotted[[length(plotted)]]
    expect_identical(shown$title, "q6 (item): Fairness of outcomes")
    # Observed 2 to 5, declared 1 to 7: seven bars.
    expect_identical(shown$at, as.numeric(1:7))
    expect_identical(shown$counts[c(1, 6, 7)], c(0L, 0L, 0L))
    expect_identical(sum(shown$counts), nrow(d))
    expect_match(shown$subtitle, sprintf("n = %d, 0 missing\nmean %.2f", nrow(d), mean(d$q6)), fixed = TRUE)
    # A column that is not a number: a bar per level.
    session$setInputs(plot_var = "pid")
    expect_no_error(output$dist_plot)
    expect_identical(plotted[[length(plotted)]]$kind, "levels")
  })
})

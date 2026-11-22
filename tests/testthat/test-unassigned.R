# Tests for the one rule for an item without a construct. Every supported
# way of building a ledger (Load,
# a cleared cell, Clear constructs, a specification import and
# sieve_prepare_study()) stores such an item's construct as "Unassigned",
# and a hand-built ledger can hold NA or a blank cell. All three mean no
# construct: the item is left out of every construct model, reliability
# analysis, score and norm, and is listed in not_modeled with the reason
# "no construct is assigned". A model builder that tested only is.na()
# would fit "Unassigned" as a common factor (reported ok) and make a blank
# cell an anonymous factor F3.

skip_if_not_installed("lavaan")

unassigned_items <- function() lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]

unassigned_meta <- function(construct) {
  items <- unassigned_items()
  data.frame(
    item = names(items), label = names(items), construct = construct,
    facet = NA_character_, type = "continuous", status = "active", reverse = FALSE,
    stringsAsFactors = FALSE
  )
}

# A prepared study: three visual tests assigned to Visual, three verbal
# tests left blank in the
# specification, which sieve_prepare_study() stores as "Unassigned".
unassigned_prepared <- function() {
  d <- DMAR::holzinger_swineford[, c(
    "id", "school", "t1_visual_perception", "t2_cubes", "t4_lozenges",
    "t6_paragraph_comprehension", "t7_sentence", "t9_word_meaning"
  )]
  spec <- sieve_specification_template(d)
  spec$construct[spec$variable %in% c("t1_visual_perception", "t2_cubes", "t4_lozenges")] <- "Visual"
  path <- tempfile("unassigned_", fileext = ".siever.rds")
  printed <- utils::capture.output(sieve_prepare_study(d, spec, file = path, name = "unassigned"))
  list(data = d, project = sieve_project_read(path), printed = printed)
}

test_that("a construct cell that is missing, blank, white space or Unassigned names no construct", {
  x <- c("Visual", NA, "", "   ", "Unassigned", "Textual")
  expect_identical(sieve_construct_assigned(x), c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_identical(sieve_construct_assigned(factor(x)), c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_identical(sieve_construct_assigned(character(0)), logical(0))
  expect_false(anyNA(sieve_construct_assigned(c(NA, NA))))
})

test_that("on a prepared study with Unassigned items, every construct model is the assigned construct only", {
  skip_if_not_installed("DMAR")
  p <- unassigned_prepared()
  meta <- p$project$meta
  items <- p$project$items
  verbal <- c("t6_paragraph_comprehension", "t7_sentence", "t9_word_meaning")
  expect_identical(meta$construct[match(verbal, meta$item)], rep("Unassigned", 3L))
  # The study summary says one construct, and the models agree.
  expect_true(any(grepl("^Constructs: 1$", p$printed)))

  spec <- sieve_cfa_spec(meta, reserved = names(items))
  expect_identical(spec$model, "Visual =~ t1_visual_perception + t2_cubes + t4_lozenges")
  expect_identical(spec$latent, "Visual")
  expect_identical(nrow(spec$not_modeled), 1L)
  expect_identical(spec$not_modeled$reason, "no construct is assigned")
  expect_identical(spec$not_modeled$items, paste(verbal, collapse = ", "))

  expect_identical(names(sieve_reliability_items(meta, names(items))), "Visual")
  expect_identical(names(sieve_efa_target(meta, names(items))), "Visual")
  expect_identical(sieve_structure_groups(meta)$groups, "Visual")
  expect_false("Unassigned" %in% names(sieve_name_plan(meta, reserved = names(items))$construct))

  run <- sieve_analyze("cfa", items_df = items, meta = meta)
  expect_identical(run$status$code, "ok")
  expect_identical(lavaan::lavNames(run$result, "lv"), "Visual")
})

test_that("with every item Unassigned, the construct pages are blocked and the CFA refuses with the reason", {
  items <- unassigned_items()
  meta <- unassigned_meta(rep("Unassigned", 9L))
  st <- sieve_new_state()
  shiny::isolate({
    st$meta <- meta
    st$items <- items
    st$raw <- items
    st$confirmed <- TRUE
  })
  for (page in c("cfa", "reliab", "validity", "bifactor", "sem")) {
    status <- shiny::isolate(sieve_page_status(st, page))
    expect_identical(status$state, "blocked", info = page)
    expect_match(status$message, "assign the included items to constructs on Ingest", info = page)
  }

  run <- sieve_analyze("cfa", items_df = items, meta = meta)
  expect_identical(run$status$code, "failed")
  expect_null(run$result)
  expect_identical(
    run$status$message,
    paste(
      "No included item is assigned to a construct (each is blank or Unassigned), so there is no confirmatory",
      "model to fit. Assign items to constructs (the construct column of the specification, or the Construct",
      "cell on Ingest) before fitting."
    )
  )
  # The same ledger with NA, a blank cell, or a mix says the same thing.
  mixed <- unassigned_meta(c(NA, "", "  ", rep("Unassigned", 6L)))
  expect_identical(sieve_analyze("cfa", items_df = items, meta = mixed)$status$message, run$status$message)
  # A ledger whose items are all excluded keeps the general message: its
  # items are not unassigned, they are out of the analysis.
  parked <- unassigned_meta(rep("Visual", 9L))
  parked$status <- "parked"
  expect_match(
    sieve_analyze("cfa", items_df = items, meta = parked)$status$message,
    "^The item definitions specify no model to fit"
  )
})

test_that("a blank or white space construct gives no F-numbered factor", {
  items <- unassigned_items()
  for (blank in c("", "   ")) {
    meta <- unassigned_meta(c(rep("Visual", 3L), rep("Textual", 3L), rep(blank, 3L)))
    spec <- sieve_cfa_spec(meta, reserved = names(items))
    expect_identical(spec$latent, c("Visual", "Textual"), info = deparse(blank))
    expect_false(grepl("(^|\n)F[0-9]+ =~", spec$model), info = deparse(blank))
    expect_identical(spec$not_modeled$items, "x7, x8, x9", info = deparse(blank))
    expect_identical(spec$not_modeled$reason, "no construct is assigned", info = deparse(blank))
    expect_length(sieve_name_plan(meta, reserved = names(items))$construct, 2L)
  }
})

test_that("missing and Unassigned constructs give one reverse-balance row, labeled Unassigned", {
  meta <- unassigned_meta(c(NA, "", rep("Unassigned", 7L)))
  meta$reverse[c(1L, 3L)] <- TRUE
  out <- sieve_reverse_balance(meta)
  expect_identical(out$construct, "Unassigned")
  expect_identical(out$k, 9L)
  expect_identical(out$reversed, 2L)
  # The item coverage table uses the same label for the same items.
  expect_identical(sieve_item_coverage(meta)$construct, "Unassigned")
})

test_that("an exported specification round-trips through sieve_prepare_study to the same CFA", {
  skip_if_not_installed("DMAR")
  p <- unassigned_prepared()
  tab <- sieve_spec_table(p$project$raw %||% p$data, p$project$items, p$project$meta)
  items_rows <- tab$role == "item"
  # SIEVER's own export writes Unassigned, so the label cannot be refused.
  expect_identical(sum(tab$construct[items_rows] == "Unassigned"), 3L)
  path <- tempfile("unassigned_round_trip_", fileext = ".siever.rds")
  invisible(utils::capture.output(sieve_prepare_study(p$data, tab, file = path, name = "round trip")))
  again <- sieve_project_read(path)
  expect_identical(again$meta$construct, p$project$meta$construct)
  expect_identical(
    sieve_cfa_spec(again$meta, reserved = names(again$items))$model,
    sieve_cfa_spec(p$project$meta, reserved = names(p$project$items))$model
  )
})

test_that("scoring, norms, the pool counts and the structural model read the one rule", {
  items <- unassigned_items()
  meta <- unassigned_meta(c(rep("Visual", 3L), rep("Textual", 3L), NA, "  ", "Unassigned"))
  expect_identical(sieve_pool_counts(meta)$constructs, c("Visual", "Textual"))
  expect_identical(names(sieve_norm_definition(meta)), c("Visual", "Textual"))
  expect_identical(names(sieve_score_spec_build(meta)), c("Visual", "Textual"))
  expect_false(grepl("score_  |score_Unassigned", paste(sieve_scoring_code(meta)$code, collapse = "\n")))
  lam <- sieve_network_lambda(meta, names(items))
  expect_setequal(lam$not_modeled$item, c("x7", "x8", "x9"))

  crit <- cbind(items, crit = rowMeans(items))
  plan <- sieve_structural_plan(meta, outcomes = "crit", all_constructs = TRUE, reserved = names(crit))
  expect_identical(unname(plan$lat_map), c("Visual", "Textual"))
  # Unassigned chosen as a predictor construct is not a construct.
  plan2 <- sieve_structural_plan(meta, outcomes = "Textual", predictors = "Unassigned", reserved = names(items))
  expect_identical(plan2$model, "")
  expect_match(plan2$problems, "No predictor construct")
})

test_that("the IRT tab offers constructs only, and every included item as one scale when none is assigned", {
  skip_if_not_installed("DMAR")
  fx <- sieve_hs9_fixture()
  st <- sieve_new_state()
  shiny::isolate({
    st$raw <- fx$data
    st$items <- fx$data[, fx$item_cols, drop = FALSE]
    meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
    meta$construct[meta$construct == "Speed"] <- "Unassigned"
    st$meta <- meta
    st$confirmed <- TRUE
  })
  shiny::testServer(mod_irt_server, args = list(state = st), {
    expect_identical(construct_choices(), c("Visual", "Textual"))
    meta <- shiny::isolate(st$meta)
    meta$construct <- "Unassigned"
    st$meta <- meta
    session$flushReact()
    expect_identical(construct_choices(), character(0))
    expect_identical(scope_key(), "__all__")
  })
})

test_that("with every item Unassigned and the study confirmed, the SEM page offers no outcome and raises no error", {
  # The ledger every Load writes before constructs are given. The SEM
  # page's outcome table has no row to offer, and ifelse() on an empty test
  # returns a logical vector that make.unique() refuses; raised in an
  # observer, such an error would end the session as soon as such a study
  # is confirmed or opened.
  none <- sieve_outcome_choices(character(0), character(0))
  expect_identical(nrow(none), 0L)
  expect_type(none$value, "character")
  expect_type(none$label, "character")
  expect_identical(sieve_outcome_choices(NULL, character(0)), none)
  # One name in both lists still gets two values of its own.
  both <- sieve_outcome_choices("y", "y")
  expect_identical(both$value, c("y (construct)", "y (data column)"))
  expect_identical(both$label, both$value)
  items <- unassigned_items()
  st <- sieve_new_state()
  shiny::isolate({
    st$meta <- unassigned_meta(rep("Unassigned", 9L))
    st$items <- items
    st$raw <- items
    st$roles <- sieve_declared_roles(items, names(items))
    st$confirmed <- TRUE
  })
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$flushReact()
    expect_length(constructs(), 0L)
    expect_identical(nrow(outcome_choices()), 0L)
    expect_length(picked_outcomes()$name, 0L)
  })
})

test_that("with every included item Unassigned and the study confirmed, the Ingest status line names the step the page gate asks for", {
  # A plain data file names no construct, so every item loads as
  # Unassigned. Once the study is confirmed, the page gate blocks the CFA
  # and the other construct models until a construct is assigned, and the
  # status line must not say every tab is unlocked and send the researcher
  # to the CFA, which would then refuse to fit.
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "plain.csv")
  utils::write.csv(unassigned_items(), csv, row.names = FALSE)
  headline <- function(html) {
    html <- paste(as.character(html), collapse = " ")
    span <- regmatches(html, regexpr("<span class=\"sieve-status-headline\">[^<]*</span>", html))
    trimws(gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", span)))
  }
  st <- sieve_new_state()
  test_frame <- environment()
  notes <- new.env(parent = emptyenv())
  notes$said <- character(0)
  local_mocked_bindings(sieve_notify = function(message, ...) {
    notes$said <- c(notes$said, paste(as.character(message), collapse = " "))
    invisible(NULL)
  }, .env = test_frame)
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "alone", basis = "theoretical", palette = "nd",
      file = list(name = "plain.csv", datapath = csv))
    session$setInputs(load = 1L)
    session$setInputs(confirm_main = 1L)
    expect_true(isTRUE(shiny::isolate(state$confirmed)))
    expect_true(all(shiny::isolate(state$meta$construct) == "Unassigned"))
    for (page in c("cfa", "reliab", "validity", "bifactor", "sem")) {
      expect_identical(shiny::isolate(sieve_page_status(state, page))$state, "blocked", info = page)
    }
    expect_true(sieve_constructs_needed(shiny::isolate(state$meta)))
    said <- headline(output$status_chip$html)
    expect_identical(said, paste(
      "9 items loaded; types confirmed (ML with FIML). Constructs are still needed for CFA and the pages",
      "after it: assign them in the grid's Construct column, then confirm again."
    ))
    # The Confirm press's own message says what it unlocks, not every tab.
    expect_true(any(grepl("Screening, Dimensionality and EFA are unlocked; CFA and the pages after it wait for",
      notes$said, fixed = TRUE)))
    expect_false(any(grepl("Downstream tabs are unlocked", notes$said, fixed = TRUE)))
    expect_false(grepl("Open CFA", said, fixed = TRUE))
    expect_false(grepl("All tabs unlocked", said, fixed = TRUE))
    expect_lte(nchar(said), sieve_status_line_budget())

    # Assigning a construct in the grid withdraws the confirmation; once
    # confirmed again the gate opens and the line names the CFA.
    for (item in c("x1", "x2", "x3")) {
      session$setInputs(cell_edit = list(item = item, field = "construct", value = "Visual"))
    }
    expect_false(isTRUE(shiny::isolate(state$confirmed)))
    session$setInputs(confirm_main = 2L)
    expect_false(sieve_constructs_needed(shiny::isolate(state$meta)))
    expect_false(identical(shiny::isolate(sieve_page_status(state, "cfa"))$state, "blocked"))
    expect_identical(headline(output$status_chip$html), paste(
      "9 items loaded; types confirmed (ML with FIML). All tabs unlocked.",
      "Open CFA to fit the theoretical structure, or Screening to look at the items first."
    ))
    expect_match(notes$said[length(notes$said)], "Downstream tabs are unlocked.", fixed = TRUE)

    # On the empirical basis the next step is the exploratory tabs either
    # way; with no construct the line does not say every tab is unlocked.
    session$setInputs(basis = "empirical", apply_basis = 1L)
    session$setInputs(clear_constructs = 1L)
    session$setInputs(confirm_main = 3L)
    expect_true(isTRUE(shiny::isolate(state$confirmed)))
    expect_true(sieve_constructs_needed(shiny::isolate(state$meta)))
    expect_identical(headline(output$status_chip$html), paste(
      "9 items loaded; types confirmed (ML with FIML). Open Screening to look at the items, then Dimensionality",
      "and EFA for the number of factors and the structure; CFA and the other construct models wait for constructs."
    ))
  })
})

# The study of the global Open browser test (test-workflow-browser.R): four
# ordered items, n = 500. `map` is the theory map given to
# sieve_build_meta(); the browser test gives one keyed by item, so every
# item opens as Unassigned (with the SEM page's outcome table empty), and
# list(Trait = items) assigns them to Trait.
open_fixture_file <- function(map) {
  data <- withr::with_seed(1, {
    trait <- stats::rnorm(500)
    x <- as.data.frame(lapply(c(.65, .8, .9, .7), function(a) {
      cut(a * trait + stats::rnorm(500), c(-Inf, -.5, .6, Inf), labels = FALSE)
    }))
    names(x) <- paste0("item", 1:4)
    x
  })
  state <- sieve_new_state()
  state$raw <- state$items <- data
  state$meta <- sieve_build_meta(data, names(data), map(names(data)))
  m <- shiny::isolate(state$meta)
  m$type <- "ordered"
  state$meta <- m
  state$roles <- sieve_declared_roles(data, names(data))
  state$confirmed <- TRUE
  state$estimator <- "WLSMV"
  state$dataset_name <- "Synthetic ordered evidence fixture"
  shiny::isolate({
    sieve_new_dataset(state)
    sieve_confirm_types(state)
  })
  file <- tempfile(fileext = ".siever.rds")
  sieve_project_write(sieve_project_snapshot(state), file)
  file
}

test_that("the global Open keeps the session, offers the SEM choices the constructs give, and computes the IRT evidence panels, with a named construct and with none", {
  skip_if_not_installed("DMAR")
  test_frame <- environment()
  # The SEM page offers its outcomes and predictors through
  # updateSelectInput(), which the test session does not deliver; the
  # choices are kept here by full input id.
  offered <- new.env(parent = emptyenv())
  local_mocked_bindings(
    updateSelectInput = function(session = shiny::getDefaultReactiveDomain(), inputId, label = NULL,
                                 choices = NULL, selected = NULL) {
      assign(session$ns(inputId), choices, envir = offered)
      invisible(NULL)
    },
    .env = test_frame
  )
  cases <- list(
    named = list(map = function(items) list(Trait = items), constructs = "Trait", scope = "(Trait)"),
    unassigned = list(map = function(items) stats::setNames(rep("Trait", length(items)), items),
      constructs = character(0), scope = "(all active items)")
  )
  for (case in names(cases)) {
    spec <- cases[[case]]
    file <- open_fixture_file(spec$map)
    on.exit(unlink(file), add = TRUE)
    rm(list = ls(offered), envir = offered)
    shiny::testServer(app_server, {
      text_of <- function(id) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(output[[id]]$html), collapse = " ")))
      session$setInputs(`project-open` = 1L)
      session$setInputs(`project-file` = list(datapath = file, name = basename(file), size = file.size(file), type = ""))
      preview <- text_of("project-open_preview")
      expect_match(preview, "Synthetic ordered evidence fixture", fixed = TRUE, info = case)
      expect_match(preview, "Rows 500", fixed = TRUE, info = case)
      session$setInputs(`project-restore` = 1L)
      expect_false(session$isClosed(), info = case)
      expect_true(isTRUE(shiny::isolate(state$confirmed)), info = case)
      expect_identical(shiny::isolate(state$meta$construct),
        if (length(spec$constructs)) rep("Trait", 4L) else rep("Unassigned", 4L), info = case)

      # The SEM page: each construct with two or more included items is an
      # outcome and a predictor, there is no criterion column, and no
      # antecedent column, so neither selector offers anything else.
      outcomes <- get("sem-outcomes", envir = offered)
      expect_identical(names(outcomes), c("Constructs", "External variables"), info = case)
      expect_identical(unname(unlist(outcomes$Constructs)) %||% character(0), spec$constructs, info = case)
      expect_identical(names(outcomes$Constructs) %||% character(0), spec$constructs, info = case)
      expect_length(outcomes$`External variables`, 0L)
      predictors <- get("sem-predictors", envir = offered)
      expect_identical(unname(unlist(predictors$Constructs)) %||% character(0), spec$constructs, info = case)
      expect_length(predictors$Antecedents, 0L)

      # The IRT tab fits the graded response model on the construct, or on
      # every included item when none is assigned, and each evidence panel
      # computes and records its result.
      session$setInputs(`irt-fit_go` = 1L)
      expect_match(text_of("irt-model_note"), sprintf("measured by the 4 items in scope %s", spec$scope),
        fixed = TRUE, info = case)
      expect_match(paste(output[["irt-parameter_uncertainty"]], collapse = ""), "a_lower", fixed = TRUE, info = case)
      session$setInputs(`irt-parameter_keep` = 1L)
      session$setInputs(`irt-information_draws` = 50, `irt-information_seed` = 9)
      session$setInputs(`irt-information_bands_go` = 1L)
      expect_match(text_of("irt-information_bands_note"), "Attempted 50, used 50, failed 0. Seed 9.", fixed = TRUE, info = case)
      session$setInputs(`irt-pair_item1` = "item1", `irt-pair_item2` = "item2")
      session$setInputs(`irt-pair_go` = 1L)
      expect_match(text_of("irt-pair_note"), "item1 versus item2; fitted analytic n = 500.", fixed = TRUE, info = case)
      session$setInputs(`irt-trace_item` = "item1", `irt-trace_bins` = 3)
      session$setInputs(`irt-trace_go` = 1L)
      expect_match(text_of("irt-trace_note"), "is not a latent score", fixed = TRUE, info = case)
      expect_match(paste(output[["irt-category_probabilities"]], collapse = ""), "theta", fixed = TRUE, info = case)
      recorded <- Filter(function(r) startsWith(r$note %||% "", "IRT "), shiny::isolate(state$analyses))
      expect_setequal(vapply(recorded, function(r) r$note, ""), c(
        "IRT parameter uncertainty", "IRT information parameter-uncertainty bands",
        "IRT bivariate residual inspection", "IRT category response inspection"
      ))
      expect_true(all(vapply(recorded, function(r) identical(r$status$code, "ok"), logical(1))), info = case)
      expect_false(session$isClosed(), info = case)
    })
  }
})

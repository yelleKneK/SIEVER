test_that("mixed inclusion is one recorded mutation and an empty request changes nothing", {
  st <- missing_study_state()
  shiny::isolate({
    ids <- st$meta$item[1:2]
    sieve_set_item_status(st, ids[1], "parked")
    before <- length(st$item_actions)
    logs <- length(grep("^# \\[", st$codelog))
    sieve_set_item_status(st, ids, c("active", "parked"), label = "Swap included items")
    expect_identical(st$meta$status[match(ids, st$meta$item)], c("active", "parked"))
    expect_length(st$item_actions, before + 1L)
    expect_length(grep("^# \\[", st$codelog), logs + 1L)
    expect_identical(tail(st$item_actions, 1)[[1]]$status, c("active", "parked"))
    expect_length(sieve_set_item_status(st, character(), character())$changed, 0)
  })
})

test_that("Clear constructs clears facets in one ledger edit", {
  st <- missing_study_state()
  st$construct_basis <- "empirical"
  m <- shiny::isolate(st$meta)
  m$facet <- rep(c("A", "B", "C"), each = 3)
  st$meta <- m
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(clear_constructs = 1)
    expect_true(all(state$meta$construct == "Unassigned"))
    expect_true(all(is.na(state$meta$facet)))
    expect_length(state$metadata_edits, 1)
  })
})

test_that("the Bifactor tab displays the engine's overlapping-group refusal as guidance", {
  st <- missing_study_state()
  # Mocks belong to the test's own frame throughout testServer evaluation.
  test_frame <- environment()
  local_mocked_bindings(bifactor_indices = function(...) {
    stop("Item t1 loads on two group factors.")
  }, .package = "DMAR", .env = test_frame)
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    session$setInputs(fit_go = 1, fit = 1)
    session$flushReact()
    # The controller computes from the actual module specification.
    text <- paste(as.character(output$status), collapse = " ")
    expect_match(text, "Assign each item to only one group factor", fixed = TRUE)
    expect_false(grepl("shiny-output-error", text, fixed = TRUE))
  })
})

test_that("the EM basis uses incomplete values and explicit ML keeps complete cases", {
  st <- missing_study_state()
  shiny::isolate({
    b <- sieve_state_basis(st)
    expect_identical(b$missing, "fiml")
    expect_gt(b$n_used, b$n)
    k <- sieve_basis_key(st, sieve_state_items(st), "pearson")
    st$items[1, 2] <- st$items[1, 2] + 1
    expect_false(identical(k, sieve_basis_key(st, sieve_state_items(st), "pearson")))
    st$estimator_choice <- "ml"
    cc <- sieve_state_basis(st)
    expect_false(identical(cc$missing, "fiml"))
    expect_equal(cc$R, stats::cor(stats::na.omit(st$items)), tolerance = 1e-10)
  })
})

test_that("the interface has the settings control and no ai page, scree count, bounds scope or keying scope input", {
  ui <- as.character(app_ui())
  expect_false(grepl('data-value="ai"', ui, fixed = TRUE))
  expect_false(grepl('id="retention-scree_nfactors"', ui, fixed = TRUE))
  expect_false(grepl('id="ingest-bounds_scope"', ui, fixed = TRUE))
  expect_false(grepl('id="ingest-keying_scope"', ui, fixed = TRUE))
  expect_match(ui, "sieve-settings", fixed = TRUE)
})

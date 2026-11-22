# Synthetic regressions for reversible item inclusion and workflow policy.
# Author: Ken Kelley

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

u082_item_state <- function() {
  d <- as.data.frame(lapply(seq_len(6L), function(j) sin(seq_len(40L) / (j + 1)) + seq_len(40L) / 20))
  names(d) <- paste0("i", seq_len(6L))
  fixture <- list(data = data.frame(id = c("duplicate", "duplicate", NA_character_, as.character(4:40)), d),
    item_cols = names(d), theory_map = split(names(d), rep(c("A", "B"), each = 3L)),
    name = "Item fixture", labels = NULL, load_code = character())
  siever_test_study(fixture, labels = FALSE, theory = FALSE, configure = function(s) {
    s$roles <- data.frame(variable = names(fixture$data), role = c("identifier", rep("item", 6)))
    m <- s$meta
    m$facet <- rep(c("Planning", "Planning", "Other"), 2)
    s$meta <- m
  })
}

test_that("inclusion uses identifiers rather than reordered, filtered visual positions", {
  s <- u082_item_state()
  before <- shiny::isolate(s$raw)
  intended <- sieve_selected_items(c("i5", "i2"), rows = c(1L, 2L), items = paste0("i", 1:6))
  expect_identical(intended, c("i5", "i2"))
  sieve_set_item_status(s, intended, "parked", "Inspect coverage before deciding.")
  shiny::isolate({
    expect_identical(s$meta$item[s$meta$status == "parked"], c("i2", "i5"))
    expect_identical(s$raw, before)
    expect_setequal(sieve_state_items(s)$items, c("i1", "i3", "i4", "i6"))
  })
  sieve_set_item_status(s, rev(intended), "active")
  expect_true(shiny::isolate(all(s$meta$status == "active")))
  expect_identical(sieve_selected_items(NULL, c(6L, 2L, 99L), paste0("i", 1:6)), c("i6", "i2"))
})

test_that("workflow transitions preserve scientific choices and reject theoretical optimization", {
  s <- u082_item_state()
  original <- shiny::isolate(list(meta = s$meta, items = s$items, decisions = s$decisions))
  expect_error(sieve_optimize_items(shiny::isolate(s$items), shiny::isolate(s$meta), project = s), "unavailable")
  expect_true(sieve_set_construct_basis(s, "empirical"))
  first <- shiny::isolate(sieve_workflow_token(s))
  expect_true(shiny::isolate(sieve_workflow_policy(s)$optimization))
  expect_true(sieve_set_construct_basis(s, "theoretical"))
  expect_true(sieve_set_construct_basis(s, "empirical"))
  expect_false(identical(first, shiny::isolate(sieve_workflow_token(s))))
  shiny::isolate({
    expect_identical(list(meta = s$meta, items = s$items, decisions = s$decisions), original)
    expect_length(s$workflow_history, 3L)
    expect_match(paste(s$codelog, collapse = "\n"), "construct_basis <-")
  })
})

test_that("theoretical optimizer server rejects direct run and apply events before fitting", {
  s <- u082_item_state()
  calls <- 0L
  testthat::local_mocked_bindings(sieve_optimizer_panel_search = function(...) {
    calls <<- calls + 1L
    stop("must not run")
  })
  shiny::testServer(mod_optimize_server, args = list(state = s), {
    before <- shiny::isolate(state$meta)
    session$setInputs(run = 1L, apply = 1L, undo = 1L)
    expect_equal(calls, 0L)
    expect_null(res())
    expect_identical(shiny::isolate(state$meta), before)
  })
})

test_that("Basic and Advanced preserve values and do not refit, facet edits only refresh item-rest", {
  s <- u082_item_state()
  calls <- 0L
  testthat::local_mocked_bindings(
    sieve_fit_wait = function(...) 10,
    sieve_item_diagnostics = function(items_df, meta, ...) {
      calls <<- calls + 1L
      it <- meta$item[meta$status == "active"]
      list(
        items = data.frame(
          item = it, loading_target = .6, communality = .4, item_total = .5,
          item_total_n = 40L, item_total_reason = "", alpha_drop_delta = 0,
          omega_drop_delta = 0, redundancy_max = .4, loading_off_max = .1,
          off_construct = NA_character_
        ),
        constructs = data.frame(construct = c("A", "B"), alpha = .7, omega = .7, n_active = 3L),
        recomputed = character(0), n_obs = 40L, notes = character(0)
      )
    }
  )
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    first <- diag()
    initial <- calls
    session$setInputs(information_view = "advanced")
    expect_identical(diag(), first)
    session$setInputs(information_view = "basic")
    expect_identical(diag(), first)
    expect_identical(calls, initial)
    before_facet <- facet_diag()
    session$setInputs(cell_edit = list(item = "i2", field = "facet", value = "Other"))
    after_facet <- facet_diag()
    expect_false(identical(before_facet, after_facet))
    expect_identical(diag(), first)
    expect_identical(calls, initial)
    session$setInputs(pool_item_ids = c("i5", "i2"), exclude_selected = 1L)
    expect_setequal(shiny::isolate(state$meta$item[state$meta$status != "active"]), c("i2", "i5"))
    expect_true(all(c("item_total", "facet_item_total") %in% names(display())))
    session$setInputs(include_selected = 1L)
    expect_true(shiny::isolate(all(state$meta$status == "active")))
    expect_identical(shiny::isolate(state$presentation$items_view), "basic")
  })
})

test_that("item explanations escape wording and identifiers without executable interpolation", {
  malicious <- "A 'quote' <img src=x onerror=alert(1)> \u03a9"
  for (cell in list(
    sieve_cell_text("x", malicious, "label", malicious),
    sieve_cell_combo("x", malicious, "construct", malicious),
    sieve_cell_select("x", malicious, "status", "Included", c("Included", "Excluded"))
  )) {
    expect_false(grepl("onchange=", cell, fixed = TRUE))
    expect_match(cell, "data-item=")
    expect_match(cell, "&lt;img")
    expect_match(cell, "aria-label=")
  }
  cell <- sieve_wording_cell(malicious)
  expect_match(cell, 'tabindex="0"', fixed = TRUE)
  expect_match(cell, "&lt;img")
  expect_match(sieve_item_rest_cell(NA_real_, 2L, "Fewer than three pairs."), "Undefined")
})

test_that("original respondent identities distinguish duplicate and missing imported IDs", {
  s <- u082_item_state()
  ids <- shiny::isolate(sieve_case_ids(s))
  expect_identical(ids$row, 1:40)
  expect_identical(ids$id[1:3], c("duplicate", "duplicate", NA_character_))
  sieve_remove_cases(s, c(2L, 3L), reason = "Synthetic identity regression")
  now <- shiny::isolate(sieve_case_ids(s))
  expect_identical(now$row[1:3], c(1L, 4L, 5L))
  sieve_restore_cases(s, c(2L, 3L))
  expect_identical(shiny::isolate(sieve_case_ids(s)), ids)
})

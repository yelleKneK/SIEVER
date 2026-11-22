# Behavior that spans several pages: the draft's operational score sentence
# and its currency, a reused fit kept out of the measured rates and named on
# the current line, the coverage advisory as a notification for the callers
# without a status line, and the reservation sentence beside a page's n.

skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

hs_nine <- function() {
  items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- data.frame(
    item = names(items), label = names(items),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = "continuous", status = "active",
    reverse = FALSE, stringsAsFactors = FALSE
  )
  list(items = items, meta = meta)
}

test_that("the draft states the operational scores and is stale once the specification changes", {
  fx <- hs_nine()
  items <- fx$items
  items$x1[1:5] <- NA
  spec <- sieve_score_spec_build(fx$meta, list(Visual = list(min_count = 3L)))
  text <- sieve_draft_scores_text(items, spec)
  expect_match(text, "Operational scores, as specified on the Reliability tab", fixed = TRUE)
  expect_match(text, "Visual, the mean of the answered items with at least 3 of 3 answered (n = 296 eligible)", fixed = TRUE)
  expect_match(text, "Textual, the mean of the answered items with at least 1 of 3 answered (n = 301 eligible)", fixed = TRUE)
  expect_identical(sieve_draft_scores_text(items, NULL), "")
  draft <- sieve_methods_draft(items, fx$meta, scores = spec)
  expect_match(draft, "Operational scores, as specified on the Reliability tab", fixed = TRUE)
  # The snapshot a draft is kept with covers the specification (the draft
  # states it), so a changed minimum answered rule makes the draft stale.
  st <- sieve_new_state()
  shiny::isolate({
    st$raw <- items
    st$items <- items
    st$meta <- fx$meta
    st$confirmed <- TRUE
    sieve_new_dataset(st)
  })
  made <- sieve_draft_inputs(st)
  expect_null(sieve_draft_stale_reason(made, sieve_draft_inputs(st)))
  shiny::isolate(sieve_set_score_specification(st, "Visual", min_count = 2L))
  expect_identical(
    sieve_draft_stale_reason(made, sieve_draft_inputs(st)),
    "the operational score specification changed"
  )
})

test_that("a reused fit enters no measured rate, and the current line names the page it came from", {
  seen <- new.env(parent = emptyenv())
  seen$notes <- 0L
  testthat::local_mocked_bindings(
    sieve_demand_note = function(...) {
      seen$notes <- seen$notes + 1L
      invisible(NULL)
    },
    .package = "SIEVER"
  )
  shiny::testServer(function(input, output, session) {
    state <- sieve_new_state()
    state$confirmed <- TRUE
    wire <- sieve_demand_wire(input, session, state, "fit_go", "A small CFA",
      key = shiny::reactive(list(choices = list(module = "x"))),
      plan = shiny::reactive(list(seconds = .1, p = 3L, estimator = "MLR")),
      compute = function(...) {
        list(reused = TRUE, reused_from = "CFA", reused_when = "10:00", seconds = 1.5)
      },
      kind = "cfa"
    )
  }, {
    session$setInputs(fit_go = 1L)
    expect_identical(seen$notes, 0L)
    expect_identical(wire$status()$state, "current")
    expect_match(wire$current_text(), "Reused the fit from CFA at 10:00 (the fit took 1.5 seconds then)", fixed = TRUE)
  })
})

test_that("the coverage notification returns its argument, and the reservation tag is empty without a reservation", {
  out <- list(changed = "x1", unchanged = character(0), coverage = NULL)
  expect_identical(sieve_coverage_notify(out), out)
  expect_null(shiny::isolate(sieve_holdout_n_tag(sieve_new_state())))
})

test_that("a page that computes on request adopts the identical fit the session already holds, without a click", {
  skip_if_not_installed("DMAR")
  seen <- new.env(parent = emptyenv())
  seen$fits <- 0L
  real_run <- sieve_run_model
  testthat::local_mocked_bindings(
    sieve_run_model = function(...) {
      seen$fits <- seen$fits + 1L
      real_run(...)
    },
    sieve_demand_note = function(...) invisible(NULL),
    .package = "SIEVER"
  )
  fx <- sieve_hs9_fixture()
  st <- sieve_new_state()
  shiny::isolate({
    st$raw <- fx$data
    st$items <- fx$data[, fx$item_cols, drop = FALSE]
    st$id_vars <- fx$id_vars
    st$roles <- sieve_declared_roles(fx$data, fx$item_cols)
    st$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
    st$theory <- list(k = fx$theory_k, label = fx$theory_lbl, map = fx$theory_map)
    st$confirmed <- TRUE
    st$thresholds <- sieve_default_thresholds()
    sieve_new_dataset(st)
  })
  # The page on screen is read from the root session's nav, so the module
  # runs under an app-level function (as the run-policy tests do) and its
  # state is read from the page guide's publication.
  published <- function() shiny::isolate(st$page_results$validity[["validity-fit_go"]])$state
  gate_text <- function(output) {
    gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(output[["validity-gate"]])), collapse = " ")))
  }
  page <- function(input, output, session) mod_validity_server("validity", st)
  # Nothing in the store: the page waits for its button, then fits once.
  shiny::testServer(page, {
    session$setInputs(nav = "validity", `validity-score_method` = "mean", `validity-htmt_on` = FALSE, `validity-marker` = "")
    session$flushReact()
    expect_identical(published(), "not_run")
    expect_identical(seen$fits, 0L)
    session$setInputs(`validity-fit_go` = 1L)
    expect_identical(published(), "current")
    expect_identical(seen$fits, 1L)
  })
  # The identical fit is in the session's store: a new instance of the page
  # adopts it as soon as it is shown, with the reuse sentence and no fit.
  shiny::testServer(page, {
    session$setInputs(nav = "validity", `validity-score_method` = "mean", `validity-htmt_on` = FALSE, `validity-marker` = "")
    session$flushReact()
    expect_identical(published(), "current")
    expect_identical(seen$fits, 1L)
    expect_match(gate_text(output), "Reused the fit from Validity at", fixed = TRUE)
  })
  # On another page nothing is adopted: the page is not shown.
  shiny::testServer(page, {
    session$setInputs(nav = "cfa", `validity-score_method` = "mean", `validity-htmt_on` = FALSE, `validity-marker` = "")
    session$flushReact()
    expect_identical(published(), "not_run")
    expect_identical(seen$fits, 1L)
  })
})

test_that("the coverage note names a construct below three included items and a facet below two", {
  meta <- data.frame(
    item = paste0("i", 1:9), label = paste0("i", 1:9), construct = "Well-being",
    facet = c("A", "A", "A", "B", "B", "C", "C", "C", "C"), type = "continuous",
    status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
  # Facet B has two included items: by design, not named.
  expect_null(sieve_coverage_thin_text(sieve_item_coverage(meta)))
  after <- meta
  after$status[4] <- "parked"
  text <- sieve_coverage_thin_text(sieve_item_coverage(after))
  expect_match(text, "Below three included items (two for a facet): facet B of Well-being (1 of 2 included)", fixed = TRUE)
  change <- sieve_coverage_change(sieve_item_coverage(meta), sieve_item_coverage(after))
  expect_true(change$thin[change$level == "facet" & change$facet == "B"])
  expect_false(any(change$thin[change$level == "facet" & change$facet %in% c("A", "C")]))
  expect_false(any(change$thin[change$level == "construct"]))
})

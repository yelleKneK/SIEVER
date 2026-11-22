# End to end tests: the real app, real UI wiring, headless Chrome.
#
# These complement (they do not replace) the shiny::testServer tests in
# test-pipeline-smoke.R. testServer calls a module server function directly and
# never builds a page, so it cannot catch a broken button, a mis-namespaced
# output, a tab that fails to mount, or a navbar link that does not fire. Those
# are exactly the regressions covered here.
#
# Every test opens with skip_if_no_browser() (see helper-browser.R): never on
# CRAN, never without shinytest2 and chromote, never without a Chrome or
# Chromium binary, and never without the launcher app. On a machine with no
# browser the whole file skips and the suite stays green.
#
# There are deliberately no expect_values() snapshots. A full input/output
# snapshot of an app with fourteen tabs is brittle by construction: an unrelated
# wording change or a new control rewrites the snapshot and reports a failure
# that is not one. The assertions below are targeted instead, on the specific
# values that carry the meaning, plus a page wide check that no output rendered
# an error.
#
# Author: Ken Kelley

skip_if_not_installed("shinytest2")
skip_if_not_installed("DMAR")

test_that("the app starts and the Ingest action bar is visible", {
  skip_if_no_browser()

  siever_with_app(function(app) {
    # Ingest is the landing tab.
    expect_identical(app$get_value(input = "nav"), "study")

    # Load is available in the always-open sidebar and in the persistent
    # main-panel action bar beside Confirm, so a narrow window that collapses
    # the sidebar does not hide Load and Confirm and leave the app with no
    # reachable way to start.
    expect_identical(
      as.integer(app$get_js("document.querySelectorAll('#ingest-load').length")),
      1L
    )
    expect_identical(
      as.integer(app$get_js("document.querySelectorAll('#ingest-load_main').length")),
      1L
    )
    expect_identical(
      as.integer(app$get_js("document.querySelectorAll('#ingest-confirm_main').length")),
      1L
    )
    # bslib renders the toggle button for an always-open sidebar and hides
    # it (display none): no toggle is shown, and the layout is not
    # collapsible.
    expect_identical(
      as.integer(app$get_js(paste0(
        "Array.from(document.querySelectorAll(\".tab-pane[data-value='ingest'] .collapse-toggle\"))",
        ".filter(function (e) { return getComputedStyle(e).display !== 'none'; }).length"
      ))),
      0L
    )
    expect_identical(
      app$get_js("document.querySelector(\".tab-pane[data-value='ingest'] .bslib-sidebar-layout\").getAttribute('data-collapsible-desktop')"),
      "false"
    )

    # The live status chip states where the user is in the load -> confirm ->
    # analyze flow, and before any load it must say so.
    expect_match(app$get_html("#ingest-status_chip"), "No data loaded", fixed = TRUE)

    expect_identical(siever_n_output_errors(app), 0L)
  })
})

test_that("clicking Load data reaches the confirmed state and unlocks the tabs", {
  skip_if_no_browser()

  # The single most important path in the app. Every HS test is continuous, so
  # measurement levels auto-confirm on load and the analysis tabs unlock without
  # a second click. A regression here makes every other tab unreachable.
  siever_with_app(load_example = TRUE, function(app) {
    chip <- app$get_html("#ingest-status_chip")
    expect_match(chip, "9 items loaded", fixed = TRUE)
    expect_match(chip, "types confirmed", fixed = TRUE)
    expect_match(chip, "All tabs unlocked", fixed = TRUE)
    # Confirmed, not merely loaded: the warning chip would say "press Confirm".
    expect_false(grepl("press Confirm", chip, fixed = TRUE))

    # The sidebar's own confirmation readout must agree with the chip.
    expect_match(app$get_html("#ingest-confirm_state"), "Confirmed", fixed = TRUE)

    # The item grid is populated with the whole pool: one row per item, all on
    # a single page, under the "Included items" grouping header (which is itself a
    # table row, so the row count runs one above the item count).
    # The group line says Included.
    grid <- app$get_html("#ingest-type_grid")
    expect_match(grid, "Included items", fixed = TRUE)
    expect_gte(siever_n_table_rows(app, "ingest-type_grid"), 9L)
    for (variable in c(
      "t1_visual_perception", "t2_cubes", "t4_lozenges",
      "t6_paragraph_comprehension", "t7_sentence",
      "t9_word_meaning", "t10_addition",
      "t12_counting_groups_of_dots",
      "t13_straight_and_curved_capitals"
    )) {
      expect_match(grid, variable, fixed = TRUE)
    }

    expect_identical(siever_n_output_errors(app), 0L)
  })
})

test_that("analysis tabs render after loading, and the code log accumulates", {
  skip_if_no_browser()

  siever_with_app(load_example = TRUE, function(app) {
    # ---- Screening -------------------------------------------------------
    expect_identical(siever_open_tab(app, "screen"), "screen")

    # An empty gate is the signal that the tab is usable: sieve_gate() returns
    # NULL (rendering nothing) only when data is loaded and types are confirmed.
    gate <- app$get_js("document.querySelector('#screen-gate').textContent.trim()")
    expect_identical(gate, "")

    # One descriptives row per item in the primary nine example.
    expect_identical(siever_n_table_rows(app, "screen-desc_tbl"), 9L)
    expect_identical(siever_n_output_errors(app), 0L)

    # ---- Reliability -----------------------------------------------------
    # The heaviest of the three: it fits a measurement model per construct and
    # forms the omega intervals, hence the longer wait.
    expect_identical(siever_open_tab(app, "reliab"), "reliab")
    # This small, fresh request runs automatically on entering the tab.
    # Wait for the actual three construct results, not an idle interval.
    app$wait_for_js("document.querySelectorAll('#reliab-omega_tbl table tbody tr').length === 3",
      timeout = 120 * 1000)

    omega_html <- app$get_html("#reliab-omega_tbl")
    # One row per theorized construct, each named, each carrying an interval.
    expect_identical(siever_n_table_rows(app, "reliab-omega_tbl"), 3L)
    for (construct in c("Visual", "Textual", "Speed")) {
      expect_match(omega_html, construct, fixed = TRUE)
    }
    expect_identical(siever_n_output_errors(app), 0L)

    # ---- Reproducible Code -----------------------------------------------
    expect_identical(siever_open_tab(app, "repro"), "repro")
    # The Report sub-tab leads the page; the script is in Session
    # script, and a hidden output is not drawn.
    compat_open_subtab(app, "repro", "Session script", timeout = 60 * 1000)

    code <- app$get_value(output = "repro-code")
    expect_type(code, "character")
    expect_gt(nchar(code), 0L)
    # Something was actually logged: the placeholder text is gone and the log
    # holds recognizable, runnable calls from the actions taken above.
    expect_false(grepl("(no actions recorded yet)", code, fixed = TRUE))
    expect_match(code, "library(DMAR)", fixed = TRUE)
    expect_match(code, "DMAR::holzinger_swineford", fixed = TRUE)
    # The auto-confirm step records its estimator decision, so the log covers
    # the confirm as well as the load.
    expect_match(code, "estimator", fixed = TRUE)

    expect_identical(siever_n_output_errors(app), 0L)
  })
})

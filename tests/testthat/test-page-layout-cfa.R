# The CFA, Screening, Ingest and Content Validity pages put their result
# first, the CFA tab updates what an edit left from before with one Update,
# the Screening tab's respondent tools say where a removal is restored, ask
# for its reason, and round their numbers at display, and the item and
# respondent tools use their own words.

uc_state <- function(which = c("hs9", "hs24"), confirmed = TRUE) {
  which <- match.arg(which)
  fx <- if (which == "hs9") sieve_hs9_fixture() else sieve_hs_fixture()
  s <- siever_test_study(fx, confirmed = confirmed, configure = function(s) {
    s$roles$role[s$roles$role == "auxiliary"] <- "criterion"
  })
  s
}

uc_text <- function(x) gsub("\\s+", " ", paste(as.character(unlist(x)), collapse = " "))
uc_plain <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", uc_text(x)))
uc_html <- function(ui) paste(as.character(ui), collapse = "")
uc_at <- function(html, pattern) {
  at <- regexpr(pattern, html, fixed = TRUE)
  if (at < 0) stop(sprintf("not found: %s", pattern), call. = FALSE)
  as.integer(at)
}
uc_cfa_inputs <- function(session) {
  session$setInputs(scope = "construct", structure = "first", ident = "varone", min_path = 0)
}

# ---- Results first ---------------------------------------------------------

test_that("CFA: the status line and a one-line method statement are above the tabs, and the model note is below the diagram", {
  html <- uc_html(mod_cfa_ui("cfa"))
  gate <- uc_at(html, 'id="cfa-gate"')
  method <- uc_at(html, 'id="cfa-method_line"')
  tabs <- uc_at(html, 'class="nav nav-tabs')
  plot <- uc_at(html, 'id="cfa-diagram"')
  note <- uc_at(html, 'id="cfa-model_note"')
  read <- uc_at(html, "How to read this diagram")
  expect_true(gate < method && method < tabs)
  expect_true(plot < note && note < read)
  # The notices about the display follow the figure.
  expect_true(plot < uc_at(html, 'id="cfa-diagram_readability"'))
  expect_true(uc_at(html, 'id="cfa-diagram_status"') < plot)
})

test_that("CFA: the method line states the model, the estimator, what it is fit to and the cases, before and after the fit", {
  skip_if_not_installed("lavaan")
  st <- uc_state("hs9")
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    uc_cfa_inputs(session)
    before <- uc_plain(output$method_line)
    expect_match(before,
      "Method: correlated-factors CFA, 9 items on 3 factors (lavaan::cfa(), std.lv = TRUE); ML with FIML, Pearson covariances; 301 rows supplied (FIML).",
      fixed = TRUE
    )
    session$setInputs(fit_go = 1L)
    after <- uc_plain(output$method_line)
    expect_match(after, "n = 301 of 301 rows (FIML).", fixed = TRUE)
    expect_match(after, "Fit at [0-9]{2}:[0-9]{2} in [0-9.]+ seconds, current\\.")
    # The marker identification is named as such.
    session$setInputs(ident = "marker")
    expect_match(uc_plain(output$method_line), "(lavaan::cfa(), marker indicators fixed at one)", fixed = TRUE)
  })
  # The estimator words are the resolved choice's own.
  spec <- list(observed = letters[1:6], latent = c("A", "B"), structure = "first_order", std_lv = TRUE, missing = "listwise")
  est <- sieve_estimator_resolve(rep("ordered", 6), n = 200, choice = "auto")
  expect_identical(
    sieve_cfa_method_text(spec, est, rows = 200, used = 190),
    sprintf(
      "correlated-factors CFA, 6 items on 2 factors (lavaan::cfa(), std.lv = TRUE); %s, polychoric correlations and thresholds; n = 190 of 200 rows (complete cases).",
      est$label
    )
  )
})

# After one respondent is removed, a result from before reads both counts
# from the retained fit and states the rows now separately (never "n = 301
# of 300 rows", its n from the fit and its rows from the analysis now), and
# the estimator words stay the fit's after a type edit.
test_that("CFA: the method line of a result from before reads its n and rows from the fit and states the rows now separately", {
  skip_if_not_installed("lavaan")
  st <- uc_state("hs9")
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    uc_cfa_inputs(session)
    session$setInputs(fit_go = 1L)
    expect_match(uc_plain(output$method_line), "n = 301 of 301 rows (FIML).", fixed = TRUE)
    removed <- sieve_remove_cases(state, rows = 5L, reason = "a test")
    session$flushReact()
    expect_identical(removed$removed, 5L)
    expect_identical(demand$status()$state, "stale")
    line <- uc_plain(output$method_line)
    expect_false(grepl("of 300 rows", line, fixed = TRUE))
    expect_match(line,
      "n = 301 of 301 rows when fit (FIML); 300 rows are in the analysis now, 1 fewer than then.",
      fixed = TRUE
    )
    expect_false(grepl("current\\.", line))
    # A type edit leaves the held fit's estimator words in place.
    m <- shiny::isolate(state$meta)
    m$type[m$item == m$item[[1L]]] <- "ordered"
    state$meta <- m
    sieve_confirm_types(state)
    session$flushReact()
    line <- uc_plain(output$method_line)
    expect_match(line, "ML with FIML, Pearson covariances; n = 301 of 301 rows when fit", fixed = TRUE)
    # Restored, the counts agree again and the plain form returns.
    sieve_restore_cases(state, rows = 5L)
    m$type[m$item == m$item[[1L]]] <- "continuous"
    state$meta <- m
    sieve_confirm_types(state)
    session$flushReact()
    expect_match(uc_plain(output$method_line), "n = 301 of 301 rows (FIML).", fixed = TRUE)
  })
  spec <- list(observed = letters[1:6], latent = c("A", "B"), structure = "first_order", std_lv = TRUE, missing = "fiml")
  est <- sieve_estimator_resolve(rep("continuous", 6), n = 300, choice = "auto")
  expect_match(sieve_cfa_method_text(spec, est, rows = 300, used = 300, rows_now = 302),
    "n = 300 of 300 rows when fit (FIML); 302 rows are in the analysis now, 2 more than then.",
    fixed = TRUE
  )
  expect_match(sieve_cfa_method_text(spec, est, rows = 300, used = 300, rows_now = 300),
    "n = 300 of 300 rows (FIML).", fixed = TRUE
  )
})

test_that("CFA: the size notices of a large diagram are one line under the figure, not alerts above it", {
  skip_if_not_installed("lavaan")
  st <- uc_state("hs24")
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    uc_cfa_inputs(session)
    session$setInputs(fit_go = 1L)
    status <- uc_text(output$diagram_status)
    expect_false(grepl("17 inches", status, fixed = TRUE))
    expect_false(grepl("Nominal node label size", status, fixed = TRUE))
    line <- uc_text(output$diagram_readability)
    expect_match(line, "over 17 inches at its recommended size", fixed = TRUE)
    expect_match(line, "Labels about", fixed = TRUE)
    expect_false(grepl("alert", line, fixed = TRUE))
    # The full sentences are kept, as the line's tooltip.
    expect_match(line, "At the recommended size this figure is more than 17 inches on its longer side", fixed = TRUE)
    expect_match(line, "Nominal node label size", fixed = TRUE)
  })
  said <- sieve_diagram_notice_text(5, 10, note = NULL)
  expect_match(said$short, "below 8 pt", fixed = TRUE)
  expect_false(grepl("17 inches", said$short, fixed = TRUE))
  expect_identical(sieve_diagram_notice_text(9, 10)$short, "Labels about 9.0 pt on screen, 10.0 pt in the file.")
})

test_that("Screening: every sub-tab opens on its table or figure, with its help below, and the opening paragraph follows the sub-tabs", {
  html <- gsub("&amp;", "&", uc_html(mod_screen_ui("screen")), fixed = TRUE)
  pairs <- list(
    c('id="screen-desc_tbl"', "normality, which matters because the normal-theory test statistic"),
    c('id="screen-miss_plot_ui"', "of missing data is only part of the story"),
    c('id="screen-cor_plot_ui"', "The inter item association matrix"),
    c('id="screen-itc_tbl"', "item total correlation: the"),
    c('id="screen-case_grid"', "Original row is stable"),
    c('id="screen-careless_summary"', "Survey data routinely contain respondents"),
    c('id="screen-careless_tbl"', "Every respondent flagged by at least one screen")
  )
  for (pr in pairs) {
    expect_true(uc_at(html, pr[1]) < uc_at(html, pr[2]), info = pr[1])
  }
  # The help beyond the first paragraph is in closed panels.
  expect_true(uc_at(html, "<summary>More on reading this table: reference values and flags</summary>") >
    uc_at(html, 'id="screen-desc_tbl"'))
  expect_true(grepl("<details open", html, fixed = TRUE))
  expect_false(grepl('id="screen-other_tbl"', html, fixed = TRUE))
  expect_match(uc_html(mod_ingest_ui("ingest")), "Dataset overview", fixed = TRUE)
})

test_that("Ingest: the item grid follows the action bar, the roles line and one row of type controls; the help is below it", {
  html <- uc_html(mod_ingest_ui("ingest"))
  grid <- uc_at(html, 'id="ingest-type_grid"')
  row <- uc_at(html, 'id="ingest-types_row"')
  expect_true(uc_at(html, 'id="ingest-roles_notice"') < row && row < grid)
  # The two bulk buttons, the scope, the type and Apply share the row.
  row_html <- substr(html, row, grid)
  row_end <- uc_at(row_html, 'class="sieve-grid-notes"')
  for (id in c("ingest-types_continuous", "ingest-types_ordered", "ingest-types_scope", "ingest-types_to", "ingest-types_apply")) {
    expect_true(uc_at(row_html, sprintf('id="%s"', id)) < row_end, info = id)
  }
  # The full description of each button is its tooltip.
  expect_match(html, 'title="Set every item with five or more options to continuous"', fixed = TRUE)
  expect_true(grid < uc_at(html, "This grid lists the items."))
  expect_true(grid < uc_at(html, "Ordered versus continuous: what the type changes"))
  expect_true(grid < uc_at(html, 'id="ingest-metadata_ids"'))
})

test_that("Ingest: the response-bounds warning is one line above the grid, with the items marked in their Response bounds cells", {
  d <- data.frame(
    id = 1:40,
    q1 = rep(1:5, 8), q2 = rep(c(1:5, 5:1), 4), q3 = rep(c(2, 4, 1, 5, 3), 8),
    q4 = rep(c(5, 1, 3, 2, 4), 8), q5 = rep(c(1, 1, 5, 5, 3), 8), q6 = rep(c(3, 2, 1, 4, 5), 8),
    q7 = rep(c(4, 5, 1, 2, 3), 8), q8 = rep(c(1, 3, 5, 2, 4), 8), q9 = rep(c(1, 7, 2, 6, 4), 8)
  )
  path <- file.path(withr::local_tempdir(), "ranges.csv")
  utils::write.csv(d, path, row.names = FALSE)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "alone", basis = "theoretical", palette = "nd", spec_keying = "verify", read_rows = TRUE)
    session$setInputs(file = list(name = "ranges.csv", datapath = path))
    session$setInputs(load = 1L)
    note <- uc_text(output$bounds_note)
    expect_match(note, "^\\s*<details", perl = TRUE)
    expect_match(note, "<summary class=\"text-warning-emphasis\">", fixed = TRUE)
    expect_match(uc_plain(output$bounds_note),
      "Response bounds. none declared; 1 of the 9 included items has an observed range other than 1 to 5 (see its Response bounds cell).",
      fixed = TRUE
    )
    # The full statement is kept, inside, with where to declare the bounds
    # (the Response bounds row or the item's cell).
    expect_match(note, "q9 (1 to 7)", fixed = TRUE)
    expect_match(note, "Declare the bounds of the scale under", fixed = TRUE)
    grid <- uc_text(output$type_grid)
    expect_match(grid, "observed 1 to 7; the study's common range is 1 to 5 and no bounds are declared", fixed = TRUE)
  })
})

test_that("Content Validity: the item level results open the tab, under the status line; what the tab asks for and how to read it follow", {
  html <- uc_html(mod_content_ui("content"))
  parse <- uc_at(html, 'id="content-parse_note"')
  tabs <- uc_at(html, 'class="nav nav-tabs')
  after_tabs <- substr(html, tabs, nchar(html))
  first_tab <- regmatches(after_tabs, regexpr('data-value="[^"]+"', after_tabs))
  expect_identical(first_tab, 'data-value="Item Level Results"')
  cvi <- uc_at(html, 'id="content-cvi_tbl"')
  expect_true(parse < tabs && tabs < cvi)
  ask <- uc_at(html, "What this tab asks for: expert ratings of the items")
  evidence <- uc_at(html, "Content validity is evidence about the items, not about the data.")
  read <- uc_at(html, "How to read this tab: I-CVI, the interval, modified kappa, and CVR")
  expect_true(cvi < ask && ask < evidence && evidence < read)
  expect_false(grepl("accordion", html, fixed = TRUE))
  skip_if_not_installed("DMAR")
  st <- uc_state("hs9")
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(demo = 1L)
    expect_match(uc_plain(output$cvi_note),
      "Method: DMAR::content_validity_index(), 9 items rated by 6 experts; ratings of 3 and 4 count as relevant; exact binomial 95% intervals.",
      fixed = TRUE
    )
  })
})

# ---- One Update per page ----------------------------------------------------

test_that("CFA: an edit leaves the model and the compared structures from before, and one Update refits both in turn", {
  skip_if_not_installed("lavaan")
  st <- uc_state("hs9")
  notes <- character()
  testthat::local_mocked_bindings(sieve_notify = function(message, ...) {
    notes <<- c(notes, message)
    invisible(NULL)
  })
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    uc_cfa_inputs(session)
    session$setInputs(fit_go = 1L)
    session$setInputs(compare_go = 1L)
    expect_identical(c(demand$status()$state, compare$status()$state), c("current", "current"))
    sieve_set_case_status(state, 1L, "excluded", "Update test")
    session$flushReact()
    expect_identical(c(demand$status()$state, compare$status()$state), c("stale", "stale"))
    gate <- uc_text(output$gate)
    # One action in the status line, with the combined estimate; no
    # per-result Refit button there.
    expect_identical(lengths(regmatches(gate, gregexpr("action-button", gate, fixed = TRUE))), 1L)
    expect_match(gate, 'id="proxy1-update_go"', fixed = TRUE)
    expect_match(gate, "Update (", fixed = TRUE)
    expect_false(grepl("fit_go", gate, fixed = TRUE))
    expect_false(grepl("Refit", gate, fixed = TRUE))
    expect_match(uc_plain(output$gate), "Update refits the model and the compared structures in turn.", fixed = TRUE)
    # The page guide's records name the page's action, and the comparison's
    # own panel keeps a button for the structures alone.
    guide <- shiny::isolate(state$page_results)[["proxy1"]]
    expect_identical(guide[["proxy1-fit_go"]]$state, "stale")
    expect_identical(guide[["proxy1-fit_go"]]$action, "Update")
    expect_identical(guide[["proxy1-compare_go"]]$action, "Update")
    panel <- uc_text(output$compare_line)
    expect_match(panel, 'id="proxy1-compare_go"', fixed = TRUE)
    expect_match(panel, "Refit the structures (", fixed = TRUE)
    session$setInputs(update_go = 1L)
    expect_identical(c(demand$status()$state, compare$status()$state), c("current", "current"))
    expect_length(lavaan::lavNames(fit(), "ov"), 9L)
    expect_match(notes[length(notes)],
      "^Update finished in [0-9.]+ seconds: the model and the compared structures are current\\.$"
    )
    # The status line is gone; what lavaan established about the new fit
    # (here an improper solution of the eight items) is still stated.
    gate <- uc_text(output$gate)
    expect_false(grepl("update", gate, fixed = TRUE))
    expect_false(grepl("from before", gate, fixed = TRUE))
  })
})

test_that("CFA: the Update starts nothing that was never requested", {
  skip_if_not_installed("lavaan")
  st <- uc_state("hs9")
  testthat::local_mocked_bindings(sieve_notify = function(message, ...) invisible(NULL))
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    uc_cfa_inputs(session)
    session$setInputs(fit_go = 1L)
    sieve_set_case_status(state, 1L, "excluded", "Update test")
    session$flushReact()
    expect_identical(demand$status()$state, "stale")
    expect_match(uc_plain(output$gate), "The result below is from before (CFA: ML, 9 items at", fixed = TRUE)
    session$setInputs(update_go = 1L)
    expect_identical(demand$status()$state, "current")
    # The structures were never fit: they are still not fit, and the
    # Bollen-Stine bootstrap was not run.
    expect_identical(compare$status()$state, "not_run")
    expect_null(compare_result())
    expect_error(bs_result_run(), class = "shiny.silent.error")
  })
})

test_that("CFA: the Bollen-Stine button states its wait, and a run measures the rate the next estimate uses", {
  local_measured_rates()
  skip_if_not_installed("lavaan")
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  st <- uc_state("hs9")
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    uc_cfa_inputs(session)
    session$setInputs(bs_B = 250, bs_seed = 1L)
    first <- bs_wait()
    expect_equal(first, 250 * 0.1 * sieve_demand_wait("cfa", 9L, "ML"))
    expect_match(sieve_duration_label(first), "^about [0-9]+ seconds$")
    session$setInputs(bs_run = 1L)
    # The run is a background process: the mock clock advances while real
    # time passes, and a run still going at the limit fails here.
    finished <- siever_wait_until(function() !isTRUE(shiny::isolate(bs_running())),
      "The Bollen-Stine run", step = function() {
        Sys.sleep(0.05)
        session$elapse(250)
      })
    if (finished) {
      expect_false(is.null(bs_result()$p_bs))
      expect_true(is.finite(sieve_rate("bollen_stine_ML")))
      expect_equal(bs_wait(), sieve_rate("bollen_stine_ML") * sum(sieve_fit_units(rep(9L, 250L), "ML")))
    }
  })
})

# ---- The respondent tools of the Screening tab ------------------------------

test_that("Screening: a removal in the Observations grid with no reason asks for one; Cancel changes nothing, a blank reason is recorded as no reason given", {
  st <- uc_state("hs9")
  shiny::testServer(mod_screen_server, args = list(state = st), {
    session$flushReact()
    key <- observation_key()
    session$setInputs(case_reason = "", case_use = list(row = 5, status = "excluded", key = key))
    expect_identical(nrow(state$raw), 301L)
    expect_identical(pending_removal()$rows, 5L)
    drawn <- grid_redraw()
    session$setInputs(case_reason_cancel = 1L)
    expect_identical(nrow(state$raw), 301L)
    expect_null(pending_removal())
    # The grid is drawn again, so the status cell shows the respondent in
    # the analysis.
    expect_identical(grid_redraw(), drawn + 1L)
    expect_match(uc_plain(output$case_grid_result), "Nothing was removed.", fixed = TRUE)
    session$setInputs(case_use = list(row = 5, status = "excluded", key = key))
    session$setInputs(case_reason_dialog = "  ", case_reason_confirm = 1L)
    expect_identical(nrow(state$raw), 300L)
    expect_identical(state$removed_cases[[1]]$reason, "no reason given")
    expect_true(any(grepl("(reason: no reason given)", state$codelog, fixed = TRUE)))
    # A reason typed in the dialog is the one recorded; the bulk button asks
    # in the same way.
    key <- observation_key()
    session$setInputs(case_selected = list(rows = list(7L), key = key), case_exclude = 1L)
    expect_identical(nrow(state$raw), 300L)
    session$setInputs(case_reason_dialog = "Answered in under a minute", case_reason_confirm = 2L)
    expect_identical(nrow(state$raw), 299L)
    expect_identical(state$removed_cases[[2]]$reason, "Answered in under a minute")
    # With a reason in the box no dialog is needed; a restore needs none.
    key <- observation_key()
    session$setInputs(case_reason = "Reviewed", case_use = list(row = 9, status = "excluded", key = key))
    expect_identical(nrow(state$raw), 298L)
    expect_identical(state$removed_cases[[3]]$reason, "Reviewed")
    key <- observation_key()
    session$setInputs(case_use = list(row = 9, status = "included", key = key))
    expect_identical(nrow(state$raw), 299L)
  })
})

test_that("Screening: after a removal in the grid the page says where it is restored, and the Careless responding panel lists it", {
  st <- uc_state("hs9")
  shiny::testServer(mod_screen_server, args = list(state = st), {
    session$flushReact()
    key <- observation_key()
    session$setInputs(case_reason = "Reviewed", case_use = list(row = 12, status = "excluded", key = key))
    pool <- uc_plain(output$pool_note)
    expect_match(pool,
      "300 respondents are in the analysis; 1 is removed from it (restore it on the Observations panel, with the status cell or Restore the selected respondents, or in the Removed respondents table of the Careless responding panel).",
      fixed = TRUE
    )
    expect_false(grepl("see the Careless responding panel, where", pool, fixed = TRUE))
    expect_identical(removed_shown()$row, 12L)
    expect_match(uc_plain(output$case_grid_result),
      "Removed 1 respondent from the analysis; every tab now works on the 300 remaining. Restore it here",
      fixed = TRUE
    )
  })
})

test_that("Screening: the numbers of the Observations grid are rounded at display only", {
  d <- data.frame(original_row = 1:3, irv = c(0.894427190999916, 1, NA), longstring = c(3, 4, 2), id = c("a", "b", "c"))
  shown <- sieve_screen_display_round(d, 3L)
  expect_identical(shown$irv, c("0.894", "1.000", NA))
  expect_identical(shown$longstring, c(3, 4, 2))
  expect_identical(shown$original_row, 1:3)
  st <- uc_state("hs9")
  shiny::testServer(mod_screen_server, args = list(state = st), {
    session$flushReact()
    v <- observation_view()
    # Stored at full precision.
    r <- careless()
    expect_identical(v$irv, r$tbl$irv[match(v$original_row, r$tbl$row)])
    expect_true(any(abs(v$irv * 1000 - round(v$irv * 1000)) > 1e-6, na.rm = TRUE))
  })
  # The grid draws the rounded copy.
  src <- deparse(body(mod_screen_server))
  expect_true(any(grepl("sieve_screen_display_round(d, digits = 3L)", src, fixed = TRUE)))
})

# ---- The words --------------------------------------------------------------

test_that("the item tools say Exclude and Include; the respondent tools say Remove and Restore", {
  screen <- uc_html(mod_screen_ui("screen"))
  expect_false(grepl("Include the selected items", screen, fixed = TRUE))
  expect_false(grepl("Excluded items to include again", screen, fixed = TRUE))
  expect_false(grepl("Include selected / Restore", screen, fixed = TRUE))
  expect_match(screen, "Remove the selected respondents", fixed = TRUE)
  expect_match(screen, "Restore the selected respondents", fixed = TRUE)
  expect_false(grepl("Exclude selected observations", screen, fixed = TRUE))
  # The Ingest grid's Use cell.
  st <- uc_state("hs9")
  m <- shiny::isolate(st$meta)
  cell <- sieve_use_cell("ingest-cell_edit", m, 1L)
  expect_match(cell, ">Included<", fixed = TRUE)
  expect_match(cell, ">Excluded<", fixed = TRUE)
  expect_false(grepl("Dropped|In use", cell))
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "status", value = "Excluded"))
    expect_identical(state$meta$status[state$meta$item == "t4_lozenges"], "parked")
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "status", value = "Included"))
    expect_identical(state$meta$status[state$meta$item == "t4_lozenges"], "active")
  })
})

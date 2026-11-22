# The browser profile runs separately from the portable CRAN anchors.
test_that("Ingest batch controls appear only after the first dataset is loaded", {
  skip_if_no_browser()
  siever_with_app(function(app) {
    app$set_window_size(width = 1440, height = 900)
    visible_controls <- function() app$get_js("(function(){
      function shown(node){return !!node && node.getClientRects().length > 0;}
      function container(id){var node=document.getElementById(id);return node && node.closest('.shiny-input-container');}
      var batch=document.getElementById('ingest-meta_batch');
      return {
        bulkContinuous:shown(document.getElementById('ingest-types_continuous')),
        bulkOrdered:shown(document.getElementById('ingest-types_ordered')),
        scope:shown(container('ingest-types_scope')),
        targetType:shown(container('ingest-types_to')),
        metadataItems:shown(container('ingest-metadata_ids')),
        batchSummary:shown(batch && batch.closest('details').querySelector('summary'))
      };
    })()")
    siever_wait_render(app)
    before <- visible_controls()
    expect_length(before, 6L)
    expect_false(any(unlist(before)))
    expect_identical(siever_n_output_errors(app), 0L)
    app$click("ingest-load")
    siever_wait_render(app)
    after <- visible_controls()
    expect_true(all(unlist(after)))
    expect_gt(siever_n_table_rows(app, "ingest-type_grid"), 0L)
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

test_that("edited item labels stay literal in the EFA tables and item grids have bounded keyboard entry", {
  skip_if_no_browser()
  siever_with_app(function(app) {
    app$set_window_size(width = 1280, height = 800)
    app$click("ingest-load")
    siever_wait_render(app)
    label <- '<img src="missing" onerror="window.sieverLabelExecuted=true"> literal & "quoted"'
    app$run_js(paste0(
      "Shiny.setInputValue('ingest-cell_edit',",
      jsonlite::toJSON(
        list(
          item = "t1_visual_perception",
          field = "label",
          value = label
        ),
        auto_unbox = TRUE
      ),
      ",{priority:'event'})"
    ))
    siever_wait_render(app)
    siever_open_tab(app, "efa")
    # The bundled example's selected EFA, estimated at two seconds or less,
    # runs by itself on its shown page (the run policy); the
    # button is pressed only when the first result waits for it.
    if (isTRUE(app$get_js("!!document.getElementById('efa-efa_go')"))) {
      app$click("efa-efa_go")
    }
    siever_wait_render(app)
    expect_true(isTRUE(app$get_js("Array.from(document.querySelectorAll('#efa-pattern_tbl tbody td')).some(x => x.textContent.includes('onerror='))")))
    expect_identical(
      app$get_js("document.querySelectorAll('#efa-pattern_tbl tbody img,#efa-pattern_tbl tbody script').length"),
      0L
    )
    expect_false(isTRUE(app$get_js("window.sieverLabelExecuted === true")))
    expect_true(isTRUE(app$get_js("document.querySelector('#efa-pattern_tbl input[type=search],#efa-pattern_tbl .dataTables_filter input,#efa-pattern_tbl .dt-search input') !== null")))
    siever_open_tab(app, "winnow")
    expect_identical(app$get_js("document.querySelectorAll('#winnow-pool table tbody td[tabindex=\"0\"]').length"), 1L)
    expect_identical(
      app$get_js("document.querySelectorAll('#winnow-pool table tbody input[tabindex=\"0\"],#winnow-pool table tbody select[tabindex=\"0\"]').length"),
      0L
    )
    app$run_js("document.querySelector('#winnow-pool table tbody td[tabindex=\"0\"]').focus()")
    original <- app$get_js("document.activeElement.cellIndex")
    app$run_js("document.activeElement.dispatchEvent(new KeyboardEvent('keydown',{key:'ArrowRight',bubbles:true}))")
    expect_identical(app$get_js("document.activeElement.cellIndex"), original + 1L)
    expect_match(app$get_js("document.getElementById('items-pool-keyboard-help').textContent"),
      "Tab leaves the grid",
      fixed = TRUE
    )
    app$run_js("window.uxGrid = jQuery('#winnow-pool table').DataTable(); uxGrid.page.len(3).draw(false); document.querySelector('#winnow-pool table tbody tr:last-child td[tabindex]').focus(); document.activeElement.dispatchEvent(new KeyboardEvent('keydown',{key:'ArrowDown',bubbles:true}))")
    expect_identical(app$get_js("uxGrid.page.info().page"), 1L)
    # Enter a construct editor by keyboard on the second page, then leave it
    # without changing the values. Focus stays associated with that cell.
    app$run_js("var c = document.querySelector('#winnow-pool table tbody input[data-field=construct]'); c.closest('td').focus(); c.closest('td').dispatchEvent(new KeyboardEvent('keydown',{key:'F2',bubbles:true}))")
    expect_identical(app$get_js("document.activeElement.getAttribute('data-field')"), "construct")
    app$run_js("document.activeElement.dispatchEvent(new KeyboardEvent('keydown',{key:'Escape',bubbles:true}))")
    expect_identical(app$get_js("document.activeElement.tagName"), "TD")
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

test_that("assistant stages expose prerequisites and external review includes a bound local copy", {
  skip_if_no_browser()
  siever_with_app(function(app) {
    # Each step waits for the server's answer that it then asserts, not for a
    # fixed quiet period: a plan is prepared on the server, and a 200 ms quiet
    # period could end before the click reached it, and the test would read
    # the button as still disabled and the summary empty.
    # The button states arrive as sieve-action-state messages.
    enabled <- function(id) {
      sprintf("(function(){var b=document.getElementById('%s');return !!b && !b.disabled;})()", id)
    }
    disabled <- function(id) {
      sprintf("(function(){var b=document.getElementById('%s');return !!b && b.disabled;})()", id)
    }
    text_has <- function(id, text) {
      sprintf(
        "(function(){var e=document.getElementById('%s');return !!e && e.textContent.indexOf(%s) >= 0;})()",
        id, compat_js_string(text)
      )
    }
    app$click("ingest-load")
    siever_wait_render(app)
    app$click("project-assistant")
    # The dialog is open when its controls are bound.
    siever_wait_for(app, "(function(){var e=jQuery('#project-assist_task');return e.length === 1 && !!e.data('shinyInputBinding');})()")
    expect_true(isTRUE(app$get_js("document.getElementById('project-assist_execute').disabled")))
    app$set_inputs(
      `project-assist_goal` = "Review content before collecting a new sample",
      `project-assist_task` = "content"
    )
    app$click("project-assist_plan")
    siever_wait_for(app, paste(
      enabled("project-assist_execute"),
      text_has("project-assist_plan_summary", "Reviewed research plan"),
      sep = " && "
    ))
    expect_false(isTRUE(app$get_js("document.getElementById('project-assist_execute').disabled")))
    expect_match(app$get_js("document.getElementById('project-assist_plan_summary').textContent"),
      "Reviewed research plan",
      fixed = TRUE
    )
    app$set_inputs(`project-assist_goal` = "Changed research goal")
    siever_wait_for(app, disabled("project-assist_execute"))
    expect_true(isTRUE(app$get_js("document.getElementById('project-assist_execute').disabled")))
    app$click("project-assist_plan")
    siever_wait_for(app, enabled("project-assist_execute"))
    app$click(selector = "a.nav-link[data-value='Optional external review']")
    app$set_inputs(`project-assist_model` = "explicit-test-model")
    siever_wait_for(app, enabled("project-assist_prepare_external"))
    expect_false(isTRUE(app$get_js("document.getElementById('project-assist_prepare_external').disabled")))
    app$click("project-assist_prepare_external")
    siever_wait_for(app, text_has("project-assist_payload_summary", "Respondent data rows"))
    expect_match(app$get_js("document.getElementById('project-assist_payload_summary').textContent"),
      "Respondent data rows",
      fixed = TRUE
    )
    expect_match(app$get_js("document.getElementById('project-assist_payload_summary').textContent"),
      "Narratives and analysis notes may quote private item wording",
      fixed = TRUE
    )
    expect_true(isTRUE(app$get_js("document.getElementById('project-assist_send').disabled")))
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

test_that("the global Open shows its preview and the IRT evidence panels run in the browser", {
  skip_if_no_browser()
  data <- withr::with_seed(1, {
    trait <- stats::rnorm(500)
    x <- as.data.frame(lapply(
      c(
        .65,
        .8,
        .9,
        .7
      ),
      function(a) cut(a * trait + stats::rnorm(500),
        c(
          -Inf,
          -.5,
          .6,
          Inf
        ),
        labels = FALSE
      )
    ))
    names(x) <- paste0("item", 1:4)
    x
  })
  state <- sieve_new_state()
  state$raw <- state$items <- data
  state$meta <- sieve_build_meta(data, names(data), stats::setNames(rep("Trait", 4L), names(data)))
  m <- shiny::isolate(state$meta)
  m$type <- "ordered"
  state$meta <- m
  state$roles <- sieve_declared_roles(data, names(data))
  state$confirmed <- TRUE
  state$estimator <- "WLSMV"
  state$dataset_name <- "Synthetic ordered evidence fixture"
  shiny::isolate({ sieve_new_dataset(state); sieve_confirm_types(state) })
  file <- tempfile(fileext = ".siever.rds")
  on.exit(unlink(file), add = TRUE)
  sieve_project_write(sieve_project_snapshot(state), file)
  siever_with_app(function(app) {
    # Each assertion waits for the server's answer it reads, not for a fixed
    # quiet period: a sub-tab's outputs are drawn only once the browser
    # reports them visible, which on a loaded machine can follow the quiet
    # period (a fixed wait can read an empty uncertainty table at a load of
    # 23).
    text_has <- function(selector, text) {
      sprintf(
        "(function(){var e=document.querySelector(%s);return !!e && e.textContent.indexOf(%s) >= 0;})()",
        compat_js_string(selector), compat_js_string(text)
      )
    }
    app$click("project-open")
    app$upload_file(`project-file` = file)
    siever_wait_for(app, text_has("#project-open_preview", "Synthetic ordered evidence fixture"))
    expect_match(app$get_js("document.getElementById('project-open_preview').textContent"),
      "Synthetic ordered evidence fixture",
      fixed = TRUE
    )
    expect_match(app$get_js("document.getElementById('project-open_preview').textContent"), "500", fixed = TRUE)
    app$click("project-restore")
    siever_wait_render(app)
    siever_open_tab(app, "irt")
    app$click("irt-fit_go")
    siever_wait_render(app)
    app$click(selector = "a.nav-link[data-value='Item Parameters']")
    siever_wait_for(app, text_has("#irt-parameter_uncertainty tbody", "completed"), timeout = 180 * 1000)
    expect_true(isTRUE(app$get_js("document.querySelector('#irt-parameter_uncertainty tbody').textContent.includes('completed')")))
    app$click("irt-parameter_keep")
    siever_wait_render(app)
    app$set_inputs(`irt-information_draws` = 50, `irt-information_seed` = 9, wait_ = FALSE)
    app$click("irt-information_bands_go")
    siever_wait_for(app, text_has("#irt-information_bands_note", "Attempted 50"), timeout = 180 * 1000)
    expect_match(app$get_js("document.getElementById('irt-information_bands_note').textContent"),
      "Attempted 50",
      fixed = TRUE
    )
    app$click(selector = "a.nav-link[data-value='Local Dependence and Response Traces']")
    siever_wait_render(app)
    app$set_inputs(`irt-pair_item1` = "item1", `irt-pair_item2` = "item2", wait_ = FALSE)
    app$click("irt-pair_go")
    siever_wait_for(app, text_has("#irt-pair_note", "analytic n = 500"), timeout = 120 * 1000)
    expect_match(app$get_js("document.getElementById('irt-pair_note').textContent"), "analytic n = 500", fixed = TRUE)
    app$click(selector = "a.nav-link[data-value='Local Dependence and Response Traces']")
    siever_wait_render(app)
    app$set_inputs(`irt-trace_item` = "item1", `irt-trace_bins` = 3, wait_ = FALSE)
    app$click("irt-trace_go")
    siever_wait_for(app, paste(
      text_has("#irt-trace_note", "not a latent score"),
      "document.querySelectorAll('#irt-category_probabilities tbody tr').length > 0",
      sep = " && "
    ), timeout = 120 * 1000)
    expect_match(app$get_js("document.getElementById('irt-trace_note').textContent"),
      "not a latent score",
      fixed = TRUE
    )
    expect_identical(app$get_js("document.querySelectorAll('#irt-category_probabilities tbody tr').length > 0"), TRUE)
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

test_that("a chosen pair says it is ready, an exclusion stays confirmed, Load asks before it replaces the study, and Apply construct basis acts only on a change", {
  skip_if_no_browser()
  # The sequence: the two files, Load, an item excluded, then the main
  # bar's Load and the construct basis.
  fx <- sieve_hs9_fixture()
  dir <- withr::local_tempdir()
  data_csv <- file.path(dir, "nine.csv")
  utils::write.csv(fx$data, data_csv, row.names = FALSE)
  spec_csv <- file.path(dir, "nine_specification.csv")
  sieve_specification_template(fx$data[, fx$item_cols], file = spec_csv)
  spec <- utils::read.csv(spec_csv, colClasses = "character", na.strings = character(0))
  by_item <- stats::setNames(
    rep(names(fx$theory_map), lengths(fx$theory_map)),
    unlist(fx$theory_map, use.names = FALSE)
  )
  spec$construct <- unname(by_item[spec$variable])
  utils::write.csv(spec, spec_csv, row.names = FALSE)
  siever_with_app(function(app) {
    text_of <- function(selector) {
      app$get_js(sprintf(
        "(function(){var e=document.querySelector(%s);return e ? e.textContent.trim() : '';})()",
        compat_js_string(selector)
      ))
    }
    has_text <- function(selector, text) {
      sprintf(
        "(function(){var e=document.querySelector(%s);return !!e && e.textContent.indexOf(%s) >= 0;})()",
        compat_js_string(selector), compat_js_string(text)
      )
    }
    enabled <- "(function(){var b=document.getElementById('ingest-apply_basis');return !!b && !b.disabled;})()"
    # The bar under the file box says what the files are ready for, not
    # that they were uploaded (run locally, nothing leaves the computer).
    app$set_inputs(`ingest-source` = "pair")
    app$upload_file(`ingest-file` = c(data_csv, spec_csv))
    siever_wait_for(app, has_text("#ingest-file_progress .progress-bar", "Ready"))
    expect_identical(text_of("#ingest-file_progress .progress-bar"), "Ready: press Load (step 2)")
    app$click("ingest-load")
    siever_wait_for(app, has_text("#ingest-confirm_main", "Confirmed"))
    # Once loaded, the bar says so, rather than inviting the press that
    # would now replace the study.
    siever_wait_for(app, "(function(){var b=document.querySelector('#ingest-file_progress .progress-bar');return !!b && b.textContent === 'Loaded';})()")
    expect_identical(text_of("#ingest-file_progress .progress-bar"), "Loaded")
    # With a study in, the main bar's Load is gone (nothing in the item grid
    # calls for replacing the study), and the sidebar's says what it would do.
    siever_wait_for(app, "document.getElementById('ingest-load_main').classList.contains('d-none')")
    expect_identical(app$get_js("document.getElementById('ingest-load_main').getClientRects().length"), 0L)
    siever_wait_for(app, has_text("#ingest-load", "replaces this study"))
    expect_identical(text_of("#ingest-load"), "Load (replaces this study)")
    # An exclusion is in effect at once: the button stays confirmed with
    # the pool as it is now, and the note names the exclusion.
    app$set_inputs(
      `ingest-cell_edit` = list(item = "t4_lozenges", field = "status", value = "Excluded"),
      allow_no_input_binding_ = TRUE, priority_ = "event"
    )
    siever_wait_for(app, has_text("#ingest-confirm_state", "t4_lozenges excluded"))
    expect_match(text_of("#ingest-confirm_main"), "Confirmed on load (step 4): 8 continuous items", fixed = TRUE)
    expect_false(grepl("confirm again", text_of("#ingest-confirm_main"), fixed = TRUE))
    expect_match(text_of("#ingest-confirm_state"), "In effect now", fixed = TRUE)
    # Load asks first and names what it would discard; Cancel changes nothing.
    app$click("ingest-load")
    siever_wait_for(app, "!!document.querySelector('.modal.show #ingest-load_replace')")
    dialog <- text_of(".modal.show .modal-body")
    expect_match(dialog, "Load reads nine.csv and nine_specification.csv afresh", fixed = TRUE)
    expect_match(dialog, "the item grid returns to the file as loaded;", fixed = TRUE)
    expect_match(dialog, "what was done since the load is discarded: t4_lozenges excluded;", fixed = TRUE)
    expect_match(dialog, "the session script starts over", fixed = TRUE)
    app$click(selector = ".modal.show button[data-bs-dismiss='modal'], .modal.show button[data-dismiss='modal']")
    siever_wait_for(app, "!document.querySelector('.modal.show')")
    expect_match(text_of("#ingest-confirm_main"), "8 continuous items", fixed = TRUE)
    # Apply construct basis is enabled only for a change; a selection not yet
    # applied is said and survives another edit (a selector that followed
    # every ledger change would undo it).
    expect_true(isTRUE(app$get_js("document.getElementById('ingest-apply_basis').disabled")))
    app$set_inputs(`ingest-basis` = "empirical")
    siever_wait_for(app, enabled)
    expect_match(text_of("#ingest-basis_policy"), "Selected, not yet applied: Empirical", fixed = TRUE)
    # Another ledger edit, which the confirmation note shows once the server
    # has made it (the selector keeps its choice through that answer).
    app$set_inputs(
      `ingest-cell_edit` = list(item = "t2_cubes", field = "status", value = "Excluded"),
      allow_no_input_binding_ = TRUE, priority_ = "event"
    )
    siever_wait_for(app, has_text("#ingest-confirm_state", "t2_cubes"))
    expect_identical(app$get_value(input = "ingest-basis"), "empirical")
    expect_true(isTRUE(app$get_js("document.querySelector('#ingest-basis input[value=empirical]').checked")))
    # Apply says what changed.
    app$click("ingest-apply_basis")
    siever_wait_for(app, has_text(".shiny-notification", "Construct basis changed from theoretical"))
    expect_match(text_of(".shiny-notification"), "the Optimizer tab is shown", fixed = TRUE)
    expect_true(isTRUE(app$get_js("document.body.classList.contains('sieve-empirical')")))
    siever_wait_for(app, "(function(){var b=document.getElementById('ingest-apply_basis');return !!b && b.disabled;})()")
    expect_true(isTRUE(app$get_js("document.getElementById('ingest-apply_basis').disabled")))
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

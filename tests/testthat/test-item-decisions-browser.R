# In a real browser: from the CFA Estimates table, a selected item goes to the
# shared Exclude / Include dialog filled in, the exclusion refits the model by
# itself, and the method line says Current, what the tables now belong to,
# and that the refit followed the item change; the button and its sentence
# are in view on a laptop window (1,280 by 800). The reason given is in the
# decision history the dialog shows afterwards.

skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")

browser_flow <- function(app) {
    siever_open_tab(app, "cfa")
    badge <- "document.querySelector('#cfa-method_line .sieve-lifecycle')?.textContent || ''"
    # The first result of the bundled example runs by itself when quick; a
    # slower machine presses the button.
    if (!siever_wait_for(app, sprintf("/^Current$/.test(%s)", badge), 30 * 1000)) {
      app$click("cfa-fit_go")
      expect_true(siever_wait_for(app, sprintf("/^Current$/.test(%s)", badge), 120 * 1000))
    }
    compat_open_subtab(app, "cfa", "Estimates")
    expect_true(siever_wait_for(app, "document.querySelectorAll('#cfa-est_tbl tbody tr').length >= 9"))
    # The button and its sentence sit directly above the table and, once the
    # table is scrolled to, are drawn whole inside the window.
    placed <- app$get_js(paste0(
      "(function(){var b=document.querySelector('#cfa-est_decide'), t=document.querySelector('#cfa-est_tbl');",
      " if(!b||!t) return null; t.scrollIntoView({block:'center'}); var r=b.getBoundingClientRect(), s=t.getBoundingClientRect();",
      " return {w:r.width, h:r.height, left:r.left, right:r.right, top:r.top, bottom:r.bottom,",
      " above:(s.top-r.bottom), iw:window.innerWidth, ih:window.innerHeight};})()"
    ))
    expect_false(is.null(placed))
    expect_true(placed$w > 0 && placed$h > 0)
    expect_true(placed$left >= 0 && placed$right <= placed$iw)
    expect_true(placed$top >= 0 && placed$bottom <= placed$ih)
    expect_true(placed$above >= 0 && placed$above < 80)
    expect_match(app$get_js("document.querySelector('.tab-pane[data-value=\"cfa\"] .sieve-item-decision').innerText"),
      "An exclusion applies to this whole study", fixed = TRUE)
    # Select the first row and open the dialog from it. DT selects a row on
    # the mouse press (its handler listens to mousedown, not click), so the
    # press is dispatched as a reader's mouse makes it.
    first_variable <- app$get_js("document.querySelector('#cfa-est_tbl tbody tr:nth-child(1) td:nth-child(2)').textContent")
    app$run_js(paste0(
      "(function(){var cell=document.querySelector('#cfa-est_tbl tbody tr:nth-child(1) td:nth-child(2)');",
      " ['mousedown','mouseup','click'].forEach(function(type){",
      " cell.dispatchEvent(new MouseEvent(type,{bubbles:true,cancelable:true,button:0,view:window}));});})()"
    ))
    expect_true(siever_wait_for(app, "document.querySelectorAll('#cfa-est_tbl tbody tr.selected').length === 1"))
    app$click("cfa-est_decide")
    expect_true(siever_wait_for(app, "!!document.querySelector('.modal.show .sieve-item-decision-origin')"))
    body <- app$get_js("document.querySelector('.modal.show .modal-body').innerText")
    expect_match(body, "Opened from the CFA Estimates table. 1 selected item is filled in below", fixed = TRUE)
    picked <- app$get_value(input = "drop-drop_pick")
    expect_identical(picked, first_variable)
    app$set_inputs(`drop-drop_reason` = "excluded in the browser check")
    app$click("drop-drop_go")
    expect_true(siever_wait_for(app, "!document.querySelector('.modal.show')"))
    # The model refits by itself; the method line says so and what it is fit to.
    line <- "document.querySelector('#cfa-method_line')?.textContent || ''"
    expect_true(siever_wait_for(app, sprintf("/from this fit of the 8 included items/.test(%s)", line), 120 * 1000))
    text <- app$get_js(line)
    expect_match(text, "Current", fixed = TRUE)
    expect_match(text, "It was refit by itself after your item change.", fixed = TRUE)
    expect_identical(siever_n_output_errors(app), 0L)
    # The Estimates table now holds eight rows, without the excluded item,
    # and the Estimates sub-tab is still the one open.
    expect_true(siever_wait_for(app, "document.querySelectorAll('#cfa-est_tbl tbody tr').length === 8"))
    variables <- compat_js_strings(app$get_js(
      "Array.from(document.querySelectorAll('#cfa-est_tbl tbody tr td:nth-child(2)')).map(function(td){return td.textContent})"
    ))
    expect_length(variables, 8L)
    expect_false(picked %in% variables)
    expect_identical(
      app$get_js("document.querySelector('.tab-pane[data-value=\"cfa\"] ul.nav a.nav-link.active').getAttribute('data-value')"),
      "Estimates"
    )
    # The decision history carries the reason.
    app$click("drop-open")
    expect_true(siever_wait_for(app, "!!document.querySelector('.modal.show')"))
    app$run_js("document.querySelector('#drop-item_history').closest('details').querySelector('summary').click()")
    expect_true(siever_wait_for(app, "document.querySelectorAll('#drop-item_history tbody tr').length >= 1"))
    history <- app$get_js("document.querySelector('.modal.show .modal-body').innerText")
    expect_match(history, "excluded in the browser check", fixed = TRUE)
    expect_match(history, "Excluded items (1)", fixed = TRUE)
}

test_that("an item is excluded from the CFA Estimates table through the shared dialog and the page says it refit itself, on a laptop window", {
  skip_if_no_browser()
  compat_with_app(load_example = TRUE, width = 1280L, height = 800L, browser_flow)
})

test_that("the same flow holds on a wider laptop window", {
  skip_if_no_browser()
  compat_with_app(load_example = TRUE, width = 1440L, height = 900L, browser_flow)
})

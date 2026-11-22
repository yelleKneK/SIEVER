test_that("observation status edits retain original rows after sort and search", {
  skip_if_no_browser()
  siever_with_app(function(app) {
    app$click("ingest-load")
    siever_wait_render(app)
    siever_open_tab(app, "screen")
    compat_open_subtab(app, "screen", "Observations")
    expect_true(isTRUE(app$get_js("document.querySelector('#screen-case_grid select') !== null")))
    app$run_js("window.obs = jQuery('#screen-case_grid table').DataTable(); obs.order([0,'desc']).draw(); obs.search('301').draw()")
    siever_wait_render(app)
    original <- app$get_js("document.querySelector('#screen-case_grid tbody tr td').textContent")
    expect_identical(original, "301")
    app$set_inputs(`screen-case_reason` = "Browser identity test")
    app$run_js("var s=document.querySelector('#screen-case_grid tbody select'); s.value='excluded'; s.dispatchEvent(new Event('change',{bubbles:true}))")
    siever_wait_render(app)
    # Respondents are removed and restored, in the words of the respondent
    # tools.
    expect_match(app$get_js("document.getElementById('screen-case_grid_result').textContent"),
      "Removed 1 respondent from the analysis",
      fixed = TRUE
    )
    app$run_js("window.obs = jQuery('#screen-case_grid table').DataTable(); obs.search('301').draw()")
    siever_wait_render(app)
    expect_identical(app$get_js("document.querySelector('#screen-case_grid tbody select').value"), "excluded")
    expect_identical(app$get_js("document.querySelector('#screen-case_grid tbody tr td').textContent"), "301")
    app$run_js("var s=document.querySelector('#screen-case_grid tbody select'); s.value='included'; s.dispatchEvent(new Event('change',{bubbles:true}))")
    siever_wait_render(app)
    expect_match(app$get_js("document.getElementById('screen-case_grid_result').textContent"),
      "Restored 1 respondent to the analysis",
      fixed = TRUE
    )
    app$run_js("document.querySelector('#screen-case_grid tbody input.sieve-observation-pick').click()")
    siever_wait_render(app)
    # A removal with the reason box empty asks for the reason first; the
    # reason is given here so the button removes at once.
    app$set_inputs(`screen-case_reason` = "Browser identity test")
    app$click("screen-case_exclude")
    siever_wait_render(app)
    expect_match(app$get_js("document.getElementById('screen-case_grid_result').textContent"),
      "Removed 1 respondent from the analysis",
      fixed = TRUE
    )
    app$run_js("document.querySelector('#screen-case_grid tbody input.sieve-observation-pick').click()")
    siever_wait_render(app)
    app$click("screen-case_include")
    siever_wait_render(app)
    expect_match(app$get_js("document.getElementById('screen-case_grid_result').textContent"),
      "Restored 1 respondent to the analysis",
      fixed = TRUE
    )
    expect_identical(siever_n_output_errors(app), 0L)
    compat_open_subtab(app, "screen", "Careless responding")
    expect_match(app$get_js("document.getElementById('screen-careless_note').textContent"),
      "before reverse scoring",
      fixed = TRUE
    )
  })
})

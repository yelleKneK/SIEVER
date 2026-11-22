# The keyboard-only path through the pipeline (WCAG keyboard access), in
# the shinytest2 harness of helper-browser.R. From the top of the page, Tab
# and, within a tab list, the arrow keys reach each pipeline-critical
# action; its focus is drawn and in view; and Enter does what a click does.
# Load and Confirm on the Ingest
# action bar; the Structure workspace and its CFA page, whose fit runs on
# opening; the Fit panel's Bollen-Stine bootstrap, started and its
# background job canceled; the Scores workspace and its Reliability page; the
# Report workspace's saved-analysis selector and its session-script
# download. At 760 px the collapsed navigation's toggle is reached and opened
# the same way.
#
# The Confirm step's precondition, a role edit that withdraws the
# confirmation, is made by the test through the inputs (the role controls
# have their own tests); Confirm itself is reached and pressed by keyboard.
#
# The walk drives a real browser through the whole pipeline and can take
# several minutes, so the file runs only when SIEVER_TEST_KEYBOARD_PATH is
# true; otherwise it skips with that reason.
#
# Author: Ken Kelley

siever_keyboard_path_enabled <- function() {
  isTRUE(as.logical(Sys.getenv("SIEVER_TEST_KEYBOARD_PATH", unset = "false")))
}
siever_skip_keyboard_path <- function() {
  skip_if_no_browser()
  skip_if_not(siever_keyboard_path_enabled(),
    "the keyboard-only path runs only with SIEVER_TEST_KEYBOARD_PATH=true")
}

# Each step needs the one before it, so the walk stops at the first step
# that breaks, naming it, instead of waiting out every later step (waits
# that follow one broken step can take most of a run).
siever_keyboard_step <- function(ok, what) {
  if (!isTRUE(ok)) stop("The keyboard path broke at: ", what, call. = FALSE)
  invisible(TRUE)
}
siever_keyboard_js <- function(app, script, what, timeout = 120 * 1000) {
  ok <- siever_wait_for(app, script, timeout)
  testthat::expect_true(ok, info = what)
  siever_keyboard_step(ok, what)
}
siever_js_active <- function(selector) sprintf(
  "(function(){var e=document.querySelector(%s);return !!e && e.classList.contains('active');})()",
  compat_js_string(selector)
)
siever_js_text_has <- function(selector, words) sprintf(
  "(function(){var e=document.querySelector(%s);return !!e && e.textContent.includes(%s);})()",
  compat_js_string(selector), compat_js_string(words)
)
# Load the bundled example from the Ingest action bar by keyboard.
siever_keyboard_load <- function(app) {
  siever_keyboard_step(siever_keyboard_reach(app, "#ingest-load_main", activate = TRUE), "Tab to Load and press Enter")
  siever_keyboard_js(app, "document.querySelectorAll('#ingest-type_grid table tbody tr').length >= 9",
    "the example's items listed after Load")
  siever_wait_render(app)
}

test_that("a keyboard alone reaches and runs the pipeline-critical actions, with visible focus", {
  siever_skip_keyboard_path()
  # A workspace from the navbar, then one of its pages, by keyboard.
  open_by_keyboard <- function(app, page) {
    workspace <- sieve_page_workspace(page)
    root <- sprintf("nav.navbar a.nav-link[data-value='%s']", workspace)
    siever_keyboard_step(siever_keyboard_tab(app, "nav.navbar", root), sprintf("the %s workspace in the navbar", workspace))
    siever_keyboard_js(app, siever_js_active(root), sprintf("the %s workspace shown", workspace))
    siever_wait_render(app)
    link <- sprintf("#ws_%s a.nav-link[data-value='%s']", workspace, page)
    siever_keyboard_step(siever_keyboard_tab(app, sprintf("#ws_%s", workspace), link, from_top = FALSE),
      sprintf("the %s page in the %s workspace's page list", page, workspace))
    siever_keyboard_js(app, siever_js_active(link), sprintf("the %s page shown", page))
    siever_wait_render(app)
  }
  compat_with_app(width = 1280L, height = 800L, function(app) {
    siever_wait_render(app)
    # Load (step 2) on the Ingest action bar.
    siever_keyboard_load(app)
    siever_keyboard_js(app, siever_js_text_has("#ingest-status_chip", "types confirmed"), "the status chip after Load")
    # Confirm, when a change asks for it again: a role edit (made here by
    # the test; the role controls have their own tests) withdraws the
    # confirmation, and the action bar's Confirm is reached and pressed.
    app$set_inputs(`ingest-roles_role` = "criterion", `ingest-roles_names` = "age_years")
    app$click("ingest-roles_apply")
    siever_wait_render(app)
    siever_keyboard_js(app, "(function(){var b=document.getElementById('ingest-confirm_main');return !!b && !b.disabled;})()",
      "Confirm enabled after the role edit")
    siever_keyboard_step(siever_keyboard_reach(app, "#ingest-confirm_main", activate = TRUE), "Tab to Confirm and press Enter")
    siever_wait_render(app)
    siever_keyboard_js(app, "(function(){var b=document.getElementById('ingest-confirm_main');return !!b && b.disabled;})()",
      "Confirm pressed")
    siever_keyboard_js(app, siever_js_text_has("#ingest-status_chip", "types confirmed"), "the status chip after Confirm")
    # Structure, CFA: the first fit runs on opening the page.
    open_by_keyboard(app, "cfa")
    siever_keyboard_js(app, siever_js_text_has("#cfa-method_line", ", current."), "the CFA fit on opening")
    # The Fit panel's Bollen-Stine bootstrap, a background job, and its Cancel.
    panels <- ".tab-pane[data-value='cfa'] .card-header ul.nav"
    fit_tab <- sprintf("%s a.nav-link[data-value='Fit']", panels)
    siever_keyboard_step(siever_keyboard_tab(app, panels, fit_tab, from_top = FALSE), "the CFA page's Fit panel")
    siever_keyboard_js(app, siever_js_active(fit_tab), "the Fit panel shown")
    siever_wait_render(app)
    siever_keyboard_step(siever_keyboard_reach(app, "#cfa-bs_run", activate = TRUE, from_top = FALSE),
      "Tab to the Bollen-Stine bootstrap and press Enter")
    siever_keyboard_js(app, "!!document.getElementById('cfa-bs_stop')", "the bootstrap's Cancel shown", timeout = 60 * 1000)
    siever_keyboard_step(siever_keyboard_reach(app, "#cfa-bs_stop", activate = TRUE, from_top = FALSE, max = 10L),
      "Tab to Cancel and press Enter")
    siever_keyboard_js(app, siever_js_text_has(".tab-pane[data-value='cfa']", "the Bollen-Stine bootstrap was canceled"),
      "the bootstrap canceled", timeout = 60 * 1000)
    siever_keyboard_js(app, "!document.getElementById('cfa-bs_stop')", "Cancel removed")
    # Scores, Reliability: omega for the three constructs on opening.
    open_by_keyboard(app, "reliab")
    siever_keyboard_js(app, "document.querySelectorAll('#reliab-omega_tbl table tbody tr').length === 3",
      "omega for the three constructs")
    # Report: the saved-analysis selector and the session script's download.
    open_by_keyboard(app, "repro")
    siever_keyboard_step(siever_keyboard_reach(app, "#repro-saved_ids-selectized", from_top = FALSE),
      "Tab to the saved-analysis selector")
    panels <- ".tab-pane[data-value='repro'] .card-header ul.nav"
    script_tab <- sprintf("%s a.nav-link[data-value='Session script']", panels)
    siever_keyboard_step(siever_keyboard_tab(app, panels, script_tab), "the Report's Session script panel")
    siever_keyboard_js(app, siever_js_active(script_tab), "the Session script panel shown")
    siever_keyboard_js(app, "(function(){var d=document.getElementById('repro-dl');return !!d && !d.classList.contains('disabled');})()",
      "the session script's download enabled")
    siever_keyboard_step(siever_keyboard_reach(app, "#repro-dl", from_top = FALSE), "Tab to the session script's download")
    expect_identical(compat_output_errors(app), character(0))
  })
})

test_that("at 760 px a keyboard alone opens the collapsed navigation and selects a workspace", {
  siever_skip_keyboard_path()
  compat_with_app(width = 760L, height = 900L, function(app) {
    siever_wait_render(app)
    siever_keyboard_load(app)
    # The navigation collapses behind its toggle at this width: the toggle is
    # reached and opened, and a workspace is selected from the menu.
    toggler <- "nav.navbar .navbar-toggler"
    shown <- isTRUE(app$get_js(sprintf(
      "(function(){var t=document.querySelector(%s);return !!t && t.getClientRects().length>0;})()",
      compat_js_string(toggler)
    )))
    structure <- "nav.navbar a.nav-link[data-value='structure']"
    if (shown) {
      siever_keyboard_step(siever_keyboard_reach(app, toggler, activate = TRUE), "Tab to the navigation toggle and press Enter")
      siever_keyboard_js(app,
        "(function(){var m=document.querySelector('nav.navbar .navbar-collapse');return !!m && m.classList.contains('show');})()",
        "the navigation menu opened")
      siever_keyboard_step(siever_keyboard_tab(app, "nav.navbar", structure, from_top = FALSE), "the Structure workspace in the menu")
    } else {
      siever_keyboard_step(siever_keyboard_tab(app, "nav.navbar", structure), "the Structure workspace in the navbar")
    }
    siever_keyboard_js(app, siever_js_active(structure), "the Structure workspace shown")
  })
})

# Setup for the end to end (shinytest2) tests.
#
# The rest of the suite drives module servers with shiny::testServer, which is
# headless and fast. The shinytest2 tests in test-shinytest2.R drive the real
# app in a headless Chrome, so they need a browser, a second R process for the
# app, and a launcher (apps/siever/app.R). None of that is guaranteed to exist
# on an arbitrary machine, so everything here is written to skip cleanly rather
# than fail when the environment cannot support it.
#
# Author: Ken Kelley

# The launcher directory shinytest2 starts the app from.
siever_app_dir <- function() {
  testthat::test_path("apps", "siever")
}

# TRUE when chromote can locate a Chrome or Chromium binary on this machine.
# find_chrome() returns NULL with a warning when it finds nothing, and it echoes
# the CHROMOTE_CHROME environment variable back without checking it, so the path
# is verified here rather than taken on trust.
siever_has_chrome <- function() {
  if (!requireNamespace("chromote", quietly = TRUE)) {
    return(FALSE)
  }
  path <- tryCatch(
    chromote::find_chrome(),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(FALSE)
  }
  file.exists(path) || nzchar(Sys.which(path))
}

# The skip guard every browser driven test opens with: never on CRAN, never
# without shinytest2 and chromote, never without a browser, and never without
# the launcher app (which is absent if the tests are run from a tarball built
# with apps/ excluded).
skip_if_no_browser <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  testthat::skip_if_not(
    siever_has_chrome(),
    "no Chrome or Chromium binary is available for chromote"
  )
  testthat::skip_if_not(
    file.exists(file.path(siever_app_dir(), "app.R")),
    "the end to end launcher app (tests/testthat/apps/siever) is not present"
  )
}

# Run fun(app) against a freshly started SIEVER app driver.
#
# The driver is created and stopped inside a real function frame, so the
# on.exit() teardown runs on a normal return, on an expectation failure, and on
# an unexpected error alike. That is what keeps a failing test from leaving an
# orphan Chrome or an orphan Shiny process behind.
#
# Timeouts are generous (the app loads DMAR and lavaan on startup, and the
# Reliability tab fits a measurement model) but bounded, so a hung app fails the
# test instead of hanging the suite.
siever_with_app <- function(fun, load_example = FALSE, app = "siever", libs = NULL,
                            width = 1600, height = 1000) {
  load_timeout <- siever_app_load_timeout()
  start <- function() {
    chrome <- chromote::default_chromote_object()
    budget <- chrome$default_timeout
    chrome$default_timeout <- 60
    on.exit(chrome$default_timeout <- budget, add = TRUE)
    shinytest2::AppDriver$new(testthat::test_path("apps", app), name = app,
      load_timeout = load_timeout, timeout = 120000, width = width, height = height)
  }
  attempt <- function() {
    if (is.null(libs)) start() else withr::with_libpaths(libs, start(), action = "prefix")
  }
  driver <- tryCatch(attempt(), error = function(e) {
    try(e$app$stop(), silent = TRUE)
    attempt()
  })
  on.exit(try(driver$stop(), silent = TRUE), add = TRUE)
  siever_wait_render(driver)
  if (isTRUE(load_example)) {
    driver$click("ingest-load")
    if (!siever_wait_for(driver,
      "document.querySelectorAll('#ingest-type_grid table tbody tr').length >= 9",
      load_timeout)) {
      stop(sprintf("The example did not populate the item ledger within %s seconds.",
        format(load_timeout / 1000)))
    }
  }
  fun(driver)
}

# The milliseconds the app may take to start, and the bundled example to
# load once it has started: the environment variable SIEVER_APP_LOAD_TIMEOUT
# when it holds a positive number, and 300 seconds otherwise. The app's own R
# process loads SIEVER (from source under devtools::test()), shiny, bslib,
# DMAR and lavaan before it serves the page, which takes minutes on a busy
# machine.
siever_app_load_timeout <- function() {
  value <- suppressWarnings(as.numeric(Sys.getenv("SIEVER_APP_LOAD_TIMEOUT", "300000")))
  if (length(value) != 1L || !is.finite(value) || value <= 0) 300000 else value
}

# Click a top level navbar tab and wait for it to settle. Returns the value the
# navbar input reports, so the caller can confirm the click actually landed.
siever_open_tab <- function(app, value, timeout = 120 * 1000) {
  ws <- sieve_page_workspace(value)
  page <- if (identical(value, "scoring")) "reliab" else value
  app$click(selector = sprintf("nav.navbar a[data-value='%s']", ws))
  app$click(selector = sprintf("#ws_%s a[data-value='%s']", ws, page))
  selector <- sprintf("#ws_%s a[data-value='%s']", ws, page)
  if (identical(value, "scoring")) {
    app$click(selector = "#reliab-panels a[data-value='scoring']")
    selector <- "#reliab-panels a[data-value='scoring']"
  }
  root <- sprintf("nav.navbar a[data-value='%s']", ws)
  page_link <- sprintf("#ws_%s a[data-value='%s']", ws, page)
  active <- function(css) sprintf("document.querySelector(%s)?.classList.contains('active')", jsonlite::toJSON(css, auto_unbox = TRUE))
  condition <- paste(active(root), active(page_link), active(selector), sep = " && ")
  if (!siever_wait_for(app, condition, timeout)) stop("Navigation did not open ", value)
  siever_wait_render(app, timeout = timeout)
  inputs <- list(nav = app$get_value(input = "nav"))
  inputs[[paste0("ws_", ws)]] <- app$get_value(input = paste0("ws_", ws))
  inputs[["reliab-panels"]] <- app$get_value(input = "reliab-panels")
  sieve_active_page(inputs)
}

# Wait until a JavaScript condition holds in the page, for the state a test is
# about to assert. A fixed idle period is not a wait for the server: the page
# counts as idle until the server has received an event and begun on it, so on
# a loaded machine the period can end before the click it follows has had any
# effect, and the test then reads the page from before the click. On a timeout
# this returns FALSE and leaves the verdict to the expectation that follows,
# which reports what the page holds; any other error (a condition that throws,
# a session that has gone) is raised. shinytest2 0.5.1 turns an error of the
# browser session into a returned value, so a wait on a closed session returns
# at once as if its condition held; the session is asked to answer before the
# wait counts as met. Write the condition so that it is false, not an error,
# while its element is absent.
siever_wait_for <- function(app, script, timeout = 60 * 1000) {
  tryCatch(
    {
      app$wait_for_js(script, timeout = timeout)
      if (!identical(app$get_js("true"), TRUE)) {
        stop("the browser session did not answer after waiting for: ", script)
      }
      TRUE
    },
    error = function(e) {
      if (!startsWith(conditionMessage(e), "Timed out waiting for JavaScript script")) stop(e)
      FALSE
    }
  )
}

# A JavaScript condition: the element with this id is in the page.
siever_js_present <- function(id) {
  sprintf("document.getElementById('%s') !== null", id)
}

# Wait for the current server flush, visible widgets, images, and finite CSS
# transitions. Background jobs need their own result predicate separately.
siever_wait_render <- function(app, timeout = 120000) {
  token <- app$get_js(paste0(
    "(function(){if(!window.sieverTestFlush){window.sieverTestFlush={sent:0,received:0};",
    "Shiny.addCustomMessageHandler('siever-test-flush',function(x){window.sieverTestFlush.received=x;});}",
    "var n=++window.sieverTestFlush.sent;Shiny.setInputValue('siever_test_flush',n,{priority:'event'});return n;})()"))
  condition <- sprintf(paste0(
    "(function(){var s=window.sieverTestFlush;if(!s||s.received<%d)return false;",
    "var visible=function(e){return !!e&&!!e.getClientRects&&e.getClientRects().length>0;};",
    "if(document.documentElement.classList.contains('shiny-busy'))return false;",
    "if(Array.from(document.querySelectorAll('.recalculating')).some(visible))return false;",
    "if(Array.from(document.querySelectorAll('.shiny-image-output img,.shiny-plot-output img')).some(function(e){return visible(e)&&!e.complete;}))return false;",
    "if(Array.from(document.querySelectorAll('div.datatables')).some(function(e){return visible(e)&&!!e.querySelector('script[data-for]')&&!e.querySelector('table.dataTable')&&!e.matches('.shiny-output-error')&&!e.querySelector('.shiny-output-error');}))return false;",
    "return !document.getAnimations().some(function(a){return a.playState==='running'&&a.effect&&Number.isFinite(a.effect.getComputedTiming().endTime)&&visible(a.effect.target);});})()"),
    as.integer(token))
  if (!siever_wait_for(app, condition, timeout)) stop("The Shiny flush or visible widgets did not finish.")
  invisible(NULL)
}

# Wait for the first result of a page whose first result runs by itself when
# the run policy allows (sieve_runs_by_itself(): an estimate of two seconds or
# less at the standard rates and of at most six at the rates measured in the
# app's process). When the page's status line says instead that the result
# waits for its button, the button is pressed, as a researcher would press it.
# Either way the completed result (`done`, a JavaScript condition) is
# required, and a page that shows a failure is not retried. A result that
# does not complete stops the test with the text of the page's status line,
# its method line and any element named in `report`, whether the browser is
# still connected to the app, and the app's last lines of output. Returns,
# invisibly, the status line the page showed when it waited for its button,
# or NULL when the result ran by itself.
siever_first_result <- function(app, page, done, button, report = NULL, timeout = 120 * 1000) {
  shown <- function(id) siever_element_text(app, id)
  gate <- paste0(page, "-gate")
  waits <- sprintf(
    "!!document.getElementById(%s) && ((document.getElementById(%s) || {}).textContent || '').includes('so it waits for you')",
    jsonlite::toJSON(button, auto_unbox = TRUE), jsonlite::toJSON(gate, auto_unbox = TRUE)
  )
  settled <- siever_wait_for(app, sprintf("(%s) || (%s)", done, waits), timeout)
  waited <- NULL
  if (settled && !isTRUE(app$get_js(done))) {
    if (isTRUE(app$get_js(waits))) {
      waited <- shown(gate)
      app$click(button)
      siever_wait_render(app, timeout = timeout)
    }
    settled <- siever_wait_for(app, done, timeout)
  }
  if (!settled) {
    stop(sprintf(
      "The %s page's first result did not complete%s. %s",
      page, if (is.null(waited)) "" else " after its button was pressed",
      siever_page_report(app, c(gate, paste0(page, "-method_line"), report))
    ), call. = FALSE)
  }
  invisible(waited)
}

# Wait for a JavaScript condition that a click of the test started, and when
# it does not hold within the timeout, stop with what the page showed
# (siever_page_report()) rather than with the condition alone. `what` names
# the awaited result in the message.
siever_wait_or_report <- function(app, condition, what, report = NULL, timeout = 120 * 1000) {
  if (!siever_wait_for(app, condition, timeout)) {
    stop(sprintf("%s did not appear within %s seconds. %s", what, format(timeout / 1000),
      siever_page_report(app, report)), call. = FALSE)
  }
  invisible(TRUE)
}

# The text of the element with this id, with runs of white space as one space
# and at most 600 characters; "(absent)" when the page has no such element.
siever_element_text <- function(app, id) {
  text <- tryCatch(app$get_js(sprintf(
    "(function(){var e=document.getElementById(%s);return e?e.textContent:'(absent)';})()",
    jsonlite::toJSON(id, auto_unbox = TRUE)
  )), error = function(e) paste("(not read:", conditionMessage(e), ")"))
  substr(gsub("\\s+", " ", trimws(text)), 1L, 600L)
}

# What the page shows, for the message of a wait that failed: whether the
# browser is still connected to the app and whether Shiny is busy, the text
# of each element named in `ids`, and the app's last 15 lines of output.
siever_page_report <- function(app, ids = NULL) {
  session <- tryCatch(app$get_js(paste0(
    "(document.getElementById('shiny-disconnected-overlay') ? 'disconnected' : 'connected') + ",
    "(document.documentElement.classList.contains('shiny-busy') ? ' and busy' : ' and idle')"
  )), error = function(e) paste("not read:", conditionMessage(e)))
  output <- tryCatch({
    logs <- app$get_logs()
    utils::tail(as.character(logs$message[logs$location == "shiny"]), 15L)
  }, error = function(e) paste("not read:", conditionMessage(e)))
  ids <- unique(ids)
  shown <- if (length(ids)) {
    paste(sprintf("#%s: \"%s\"", ids, vapply(ids, function(id) siever_element_text(app, id), character(1))),
      collapse = "; ")
  } else {
    ""
  }
  sprintf("The browser is %s. %s\nThe app's last output:\n%s", session, shown, paste(output, collapse = "\n"))
}

# Number of outputs currently showing a Shiny error. Validation messages (the
# deliberate validate()/need() text) carry their own subclass and are excluded,
# so this counts genuine render failures only.
siever_n_output_errors <- function(app) {
  as.integer(app$get_js(
    "document.querySelectorAll('.shiny-output-error:not(.shiny-output-error-validation)').length"
  ))
}

# Number of rows currently drawn in a DT output.
siever_n_table_rows <- function(app, id) {
  as.integer(app$get_js(sprintf(
    "document.querySelectorAll('#%s table tbody tr').length", id
  )))
}

# A full-navigation fixture deliberately enables the empirical workflow.
# The theoretical workflow hides Optimizer; its policy is tested separately.
siever_use_empirical_basis <- function(app) {
  app$set_inputs(`ingest-basis` = "empirical")
  # Apply is enabled only with a study loaded (a load starts every study from
  # the theoretical basis) and a selection that differs from the basis in
  # force; a switch that cannot be made fails the test rather than leaving
  # it on the theoretical basis unnoticed.
  if (!siever_wait_for(app, "(function(){var b=document.getElementById('ingest-apply_basis');return !!b && !b.disabled;})()")) {
    testthat::fail("Apply construct basis stayed disabled: load a study before switching the basis")
    return(invisible(NULL))
  }
  app$click("ingest-apply_basis")
  siever_wait_render(app, timeout = 60 * 1000)
  invisible(NULL)
}

# Keyboard-only operation (WCAG 2.2 2.1.1 Keyboard, 2.4.3 Focus Order, 2.4.7
# Focus Visible, 2.4.11 Focus Not Obscured). Keys are sent through the Chrome
# DevTools Protocol as a real keyboard sends them, so the browser moves focus
# and decides :focus-visible itself; nothing calls focus() or click() on the
# control tested.
#
# A tab list is one stop in the Tab order. Bootstrap 5.3 gives its inactive
# tabs tabindex = -1 and moves between them with the arrow keys, Home and
# End, showing each tab it reaches (the WAI-ARIA tabs pattern with automatic
# activation), so the navbar's workspaces, a workspace's pages and a page's
# panels are reached by Tab to the selected tab and then the arrow keys.

# A value from the page, read through the browser session directly (the
# loops below read the focus after every key).
siever_page_value <- function(app, script) {
  app$get_chromote_session()$Runtime$evaluate(script, returnByValue = TRUE)$result$value
}

# Press one key (Shift with shift = TRUE).
siever_key <- function(app, key = c("Tab", "Enter", "Space", "Escape", "ArrowRight", "ArrowLeft", "Home", "End"),
                       shift = FALSE) {
  key <- match.arg(key)
  spec <- switch(key,
    Tab = list(key = "Tab", code = "Tab", vk = 9L, text = NULL),
    Enter = list(key = "Enter", code = "Enter", vk = 13L, text = "\r"),
    Space = list(key = " ", code = "Space", vk = 32L, text = " "),
    Escape = list(key = "Escape", code = "Escape", vk = 27L, text = NULL),
    ArrowRight = list(key = "ArrowRight", code = "ArrowRight", vk = 39L, text = NULL),
    ArrowLeft = list(key = "ArrowLeft", code = "ArrowLeft", vk = 37L, text = NULL),
    Home = list(key = "Home", code = "Home", vk = 36L, text = NULL),
    End = list(key = "End", code = "End", vk = 35L, text = NULL)
  )
  session <- app$get_chromote_session()
  common <- list(key = spec$key, code = spec$code, windowsVirtualKeyCode = spec$vk,
    nativeVirtualKeyCode = spec$vk, modifiers = if (shift) 8L else 0L)
  down <- c(list(type = if (is.null(spec$text)) "rawKeyDown" else "keyDown"), common,
    if (!is.null(spec$text)) list(text = spec$text, unmodifiedText = spec$text))
  do.call(session$Input$dispatchKeyEvent, down)
  do.call(session$Input$dispatchKeyEvent, c(list(type = "keyUp"), common))
  invisible(NULL)
}

# Start the next Tab from the top of the page: a script-only marker at the
# start of the body takes focus, so the next Tab goes to the first control
# in the page's tab order.
siever_focus_top <- function(app) {
  siever_page_value(app, paste0(
    "(function(){var m=document.getElementById('siever-test-focus-top');",
    "if(!m){m=document.createElement('span');m.id='siever-test-focus-top';m.tabIndex=-1;}",
    "document.body.insertBefore(m,document.body.firstChild);m.focus();return true;})()"
  ))
  invisible(NULL)
}

# Whether the first element matching `selector` has focus.
siever_has_focus <- function(app, selector) {
  isTRUE(siever_page_value(app, sprintf(
    "(function(){var t=document.querySelector(%s);return !!t && document.activeElement===t;})()",
    compat_js_string(selector)
  )))
}

# Press `key` until the control matching `selector` has focus; the number of
# presses, or NA when `max` presses never reach it.
siever_press_to <- function(app, selector, key = "Tab", max = 250L) {
  for (i in seq_len(max)) {
    siever_key(app, key)
    if (siever_has_focus(app, selector)) return(i)
  }
  NA_integer_
}

# Whether the control with focus shows it: "visible", or what is wrong. The
# browser must match :focus-visible (keyboard focus), an outline or a focus
# ring must be drawn on the control (for a selectize input, on its box), and
# the control must be inside the viewport.
siever_focus_state <- function(app) {
  siever_page_value(app, paste0(
    "(function(){var e=document.activeElement;",
    "if(!e||e===document.body)return 'nothing has focus';",
    "var name=e.id||(e.getAttribute('data-value')?e.tagName+'[data-value='+e.getAttribute('data-value')+']':e.tagName);",
    "if(!e.matches(':focus-visible'))return name+': the focus is not :focus-visible';",
    "var drawn=function(x){if(!x)return false;var s=getComputedStyle(x);",
    "var outline=s.outlineStyle!=='none'&&parseFloat(s.outlineWidth)>0;",
    "var ring=!!s.boxShadow&&s.boxShadow!=='none';return outline||ring;};",
    "var host=e.closest('.selectize-input')||e;",
    "if(!drawn(e)&&!drawn(host))return name+': no outline or focus ring is drawn';",
    "var r=host.getBoundingClientRect();",
    "if(r.bottom<=0||r.top>=window.innerHeight||r.right<=0||r.left>=window.innerWidth)",
    "return name+': the focused control is outside the viewport';",
    "return 'visible';})()"
  ))
}

# Reach a control by Tab (from the top of the page, or from where the focus
# is), check that its focus is visible, and optionally activate it with
# Enter. The expectations name the control, so a failure says which step of
# the path broke.
siever_keyboard_reach <- function(app, selector, activate = FALSE, from_top = TRUE, max = 250L) {
  if (from_top) siever_focus_top(app)
  presses <- siever_press_to(app, selector, "Tab", max = max)
  testthat::expect_false(is.na(presses), info = paste("Tab never reached", selector))
  if (is.na(presses)) return(invisible(FALSE))
  testthat::expect_identical(siever_focus_state(app), "visible", info = selector)
  if (activate) siever_key(app, "Enter")
  invisible(TRUE)
}

# Select a tab of a tab list by keyboard: Tab to the list's selected tab
# (`list` is a CSS selector of the list's container), then the arrow keys to
# `target`, right first and then left (a list wraps around). Bootstrap shows
# each tab the arrows reach.
siever_keyboard_tab <- function(app, list, target, from_top = TRUE) {
  active <- sprintf("%s a.nav-link.active", list)
  if (!siever_keyboard_reach(app, active, from_top = from_top)) return(invisible(FALSE))
  if (!siever_has_focus(app, target)) {
    n <- as.integer(siever_page_value(app, sprintf("document.querySelectorAll(%s).length",
      compat_js_string(sprintf("%s a.nav-link", list)))))
    reached <- siever_press_to(app, target, "ArrowRight", max = n)
    if (is.na(reached)) reached <- siever_press_to(app, target, "ArrowLeft", max = 2L * n)
    testthat::expect_false(is.na(reached), info = paste("The arrow keys never reached", target))
    if (is.na(reached)) return(invisible(FALSE))
  }
  testthat::expect_identical(siever_focus_state(app), "visible", info = target)
  invisible(TRUE)
}

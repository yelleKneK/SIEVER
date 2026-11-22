# Semantic browser contracts complement presentation snapshots. They drive
# the app in a headless Chrome and skip where no browser is available.
test_that('project and workflow actions remain named, reachable, and state preserving', {
  skip_if_no_browser()
  for (width in c(1280L, 760L)) {
    compat_with_app(width = width, height = if (width == 1280L) 800L else 900L, function(app) {
      evidence <- Sys.getenv('SIEVER_BROWSER_EVIDENCE')
      if (nzchar(evidence)) on.exit(
        {
          dir.create(evidence, recursive = TRUE, showWarnings = FALSE)
          app$get_screenshot(file = file.path(evidence, paste0('semantic-', width, '.png')))
          saveRDS(app$get_logs(), file.path(evidence, paste0('semantic-', width, '-logs.rds')))
          writeLines(
            c(
              paste(
                'width',
                width
              ),
              paste(
                'height',
                if (width == 1280L) 800L else 900L
              ),
              paste(
                'body',
                app$get_js('document.body.className')
              )
            ),
            file.path(
              evidence,
              paste0(
                'semantic-',
                width,
                '-viewport.txt'
              )
            )
          )
        },
        add = TRUE
      )
      siever_wait_render(app)
      expect_true(app$get_js("document.querySelector('#ingest-load_main') && /Load/.test(document.querySelector('#ingest-load_main').textContent)"))
      # A keyboard user can reach the main load action by focusing its native button.
      expect_true(app$get_js("(function(){var e=document.getElementById('ingest-load_main');e.focus();return document.activeElement===e;})()"))
      app$click('ingest-load_main')
      siever_wait_render(app)
      before <- app$get_value(output = 'ingest-status_chip')
      before_rows <- app$get_js("window.jQuery('#ingest-type_grid table').DataTable().rows().data().toArray()")
      expect_length(before_rows, 9L)
      # Buttons use their semantic accessible text; logo/markup is irrelevant.
      for (id in c('project-save', 'project-save_as', 'project-open')) {
        expect_true(app$get_js(sprintf(
          "(function(){var e=document.getElementById('%s');return !!e && e.textContent.trim().length>0 && !e.disabled;})()",
          id
        )))
      }
      expect_false(app$get_js("document.body.classList.contains('sieve-empirical')"))
      app$set_inputs(`ingest-basis` = 'empirical')
      # Apply is enabled only once the selection differs from the basis in force.
      siever_wait_for(app, "(function(){var b=document.getElementById('ingest-apply_basis');return !!b && !b.disabled;})()")
      app$click('ingest-apply_basis')
      siever_wait_render(app)
      expect_true(app$get_js("document.body.classList.contains('sieve-empirical')"))
      # Changing workflow must preserve the loaded data and confirmation.
      after <- app$get_value(output = 'ingest-status_chip')
      expect_match(paste(unlist(after), collapse = ' '), '9 items loaded; types confirmed', fixed = TRUE)
      expect_match(paste(unlist(before), collapse = ' '), '9 items loaded; types confirmed', fixed = TRUE)
      after_rows <- app$get_js("window.jQuery('#ingest-type_grid table').DataTable().rows().data().toArray()")
      expect_identical(after_rows, before_rows)
      expect_identical(compat_output_errors(app), character(0))
      # Once a study is in, the main bar's Load is hidden: replacing the study
      # is the sidebar's step 1 and 2.
      expect_identical(app$get_js("document.getElementById('ingest-load_main').getClientRects().length"), 0L)
    })
  }
})

test_that("rotated EFA factor plots preserve fitted labels, display controls, and currentness", {
  skip_if_no_browser()
  for (width in c(1280L, 760L)) {
    compat_with_app(width = width, height = 1000L, function(app) {
      text_at <- function(id) app$get_js(sprintf(
        "document.getElementById(%s).textContent", compat_js_string(id)
      ))
      axis_options <- function(id) compat_js_strings(app$get_js(sprintf(
        paste0("(function(){var e=document.getElementById(%s);",
          "return e.selectize ? Object.keys(e.selectize.options) : ",
          "Array.from(e.options).map(function(o){return o.value;});})()"),
        compat_js_string(id)
      )))
      plot_ready <- paste0(
        "(function(){var e=document.querySelector('#efa-factor_plot img');",
        "return !!e && e.complete && e.naturalWidth>100 && e.naturalHeight>100;})()"
      )
      # A fit can run in a background process, which the render wait does
      # not cover, so the plot's wait allows for a busy machine and, on a
      # timeout, says what the page showed.
      plot_wait <- function() siever_wait_or_report(app, plot_ready,
        "The factor plot", report = c("efa-status_line", "efa-factor_plot"),
        timeout = 600 * 1000)
      readout <- function() app$get_js(paste0(
        "(function(){var e=document.querySelectorAll('#efa-factor_plot_code pre code');",
        "return e.length ? e[e.length-1].textContent : '';})()"
      ))
      # The selected EFA's button: a first fit waits
      # for Run selected EFA only when it is estimated above two seconds (it
      # runs by itself otherwise, under the run policy), and a solution
      # from before an edit is updated with the status line's one Update.
      press_efa <- function(required = TRUE) {
        shown <- Filter(function(id) isTRUE(app$get_js(sprintf("!!document.getElementById('%s')", id))),
          c("efa-update_go", "efa-efa_go"))
        if (required) expect_length(shown, 1L)
        if (length(shown)) app$click(shown[[1L]])
      }
      parse_readout <- function() {
        expressions <- parse(text = readout())
        expect_length(expressions, 1L)
        call <- expressions[[1L]]
        expect_identical(call[[1L]], quote(SIEVER::sieve_efa_factor_plot))
        as.list(call)[-1L]
      }

      app$click("ingest-load_main")
      siever_wait_render(app)
      app$run_js("document.querySelectorAll('.shiny-notification-close').forEach(function(b){b.click();})")
      siever_open_tab(app, "efa")
      press_efa(required = FALSE)
      siever_wait_render(app)
      compat_open_subtab(app, "efa", "Factor plot", timeout = 30 * 1000)
      plot_wait()
      expect_true(app$get_js(plot_ready))
      app$click(selector = "#efa-factor_plot_controls summary")
      siever_wait_render(app)
      columns <- axis_options("efa-plot_x")
      expect_length(columns, 3L)
      expect_true(all(nzchar(columns)))
      for (column in columns) {
        expect_match(text_at("efa-factor_order_pattern"), column, fixed = TRUE)
      }
      x <- app$get_value(input = "efa-plot_x")
      y <- app$get_value(input = "efa-plot_y")
      expect_setequal(axis_options("efa-plot_y"), setdiff(columns, x))
      expect_false(identical(x, y))
      expect_match(text_at("efa-factor_plot_note"), "pattern coefficients", fixed = TRUE)
      for (id in c("efa-factor_plot_png", "efa-factor_plot_pdf")) {
        expect_true(app$get_js(sprintf(
          paste0("(function(){var e=document.getElementById(%s);return !!e && ",
            "e.getClientRects().length>0 && !!e.getAttribute('href') && ",
            "!e.classList.contains('disabled') && e.textContent.trim().length>0;})()"),
          compat_js_string(id)
        )))
      }
      # Display operations must retain the exact recorded extraction and rotation.
      fit_code <- app$get_js("document.querySelector('#efa-factor_plot_code pre code').textContent")
      app$set_inputs(`efa-plot_x` = y)
      siever_wait_render(app)
      app$set_inputs(`efa-plot_y` = x, `efa-plot_matrix` = "structure", `efa-plot_labels` = FALSE)
      siever_wait_render(app)
      app$wait_for_js(plot_ready, timeout = 15 * 1000)
      expect_identical(app$get_value(input = "efa-plot_x"), y)
      expect_identical(app$get_value(input = "efa-plot_y"), x)
      expect_setequal(axis_options("efa-plot_y"), setdiff(columns, y))
      expect_match(text_at("efa-factor_plot_note"), "structure coefficients", fixed = TRUE)
      expect_match(text_at("efa-factor_plot_note"), paste0("Factor correlation (", y, ", ", x, ")"), fixed = TRUE)
      expect_identical(app$get_js("document.querySelector('#efa-factor_plot_code pre code').textContent"), fit_code)
      app$click(selector = "#efa-factor_plot_code details.siever-code-view > summary")
      arguments <- parse_readout()
      expect_identical(eval(arguments$factors, baseenv()), c(y, x))
      expect_identical(eval(arguments$matrix, baseenv()), "structure")
      expect_false(eval(arguments$labels, baseenv()))
      expect_length(eval(arguments$items, baseenv()), 9L)
      expect_true(all(nzchar(eval(arguments$items, baseenv()))))
      app$click(selector = "#efa-factor_plot_code details.siever-code-view > summary")
      expect_identical(compat_output_errors(app), character(0))

      # A natural image size is insufficient: the complete graph must fit
      # inside the expanded card's actual clipping ancestors and viewport.
      app$click(selector = "#efa-factor_plot_controls summary")
      app$click(selector = ".card[data-full-screen]:has(#efa-factor_plot) .bslib-full-screen-enter")
      siever_wait_render(app)
      app$run_js("document.getElementById('efa-factor_plot').scrollIntoView({block:'center'})")
      siever_wait_render(app)
      expect_true(app$get_js(paste0(
        "(function(){var e=document.querySelector('#efa-factor_plot img'),r=e.getBoundingClientRect();",
        "var l=Math.max(0,r.left),t=Math.max(0,r.top),b=Math.min(innerHeight,r.bottom),",
        "a=Math.min(innerWidth,r.right);for(var p=e.parentElement;p;p=p.parentElement){",
        "var s=getComputedStyle(p),q=p.getBoundingClientRect();",
        "if(/auto|scroll|hidden|clip/.test(s.overflowY)){t=Math.max(t,q.top);b=Math.min(b,q.bottom);}",
        "if(/auto|scroll|hidden|clip/.test(s.overflowX)){l=Math.max(l,q.left);a=Math.min(a,q.right);}",
        "if(s.position==='fixed')break;}",
        "return r.height>=590 && b-t>=r.height-2 && a-l>=r.width-2;})()"
      )))

      evidence <- Sys.getenv("SIEVER_BROWSER_EVIDENCE")
      if (nzchar(evidence)) {
        dir.create(evidence, recursive = TRUE, showWarnings = FALSE)
        app$run_js("document.getElementById('efa-factor_plot').scrollIntoView({block:'center'})")
        app$get_screenshot(file = file.path(evidence, paste0("efa-factor-plot-", width, ".png")))
        saveRDS(app$get_logs(), file.path(evidence, paste0("efa-factor-plot-", width, "-logs.rds")))
      }
      app$click(selector = ".bslib-full-screen-exit")
      siever_wait_render(app)

      # The wide viewport covers the changed-input and single-factor branches;
      # the narrow viewport independently checks that the same controls render.
      if (width == 1280L) {
        app$click(selector = "#efa-factor_plot_controls summary")
        siever_wait_render(app)
        app$set_inputs(`efa-n_factors` = 2L)
        siever_wait_render(app)
        expect_false(app$get_js("document.querySelector('#efa-factor_plot img') !== null"))
        expect_match(text_at("efa-factor_plot"), "Run selected EFA", fixed = TRUE)
        expect_identical(compat_output_errors(app), character(0))
        press_efa()
        siever_wait_render(app)
        plot_wait()
        expect_length(axis_options("efa-plot_x"), 2L)
        expect_match(text_at("efa-factor_plot_note"), "structure coefficients", fixed = TRUE)
        app$set_inputs(`efa-n_factors` = 1L)
        siever_wait_render(app)
        expect_match(text_at("efa-factor_plot"), "Run selected EFA", fixed = TRUE)
        press_efa()
        siever_wait_render(app)
        plot_wait()
        expect_length(axis_options("efa-plot_x"), 1L)
        expect_true(app$get_js("document.getElementById('efa-plot_y') === null"))
        expect_match(text_at("efa-factor_plot_note"), "One factor was fitted", fixed = TRUE)
        expect_length(eval(parse_readout()$factors, baseenv()), 1L)
        expect_identical(compat_output_errors(app), character(0))
      }
    })
  }
})

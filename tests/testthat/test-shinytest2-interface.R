# Interface compatibility in a real browser, on current stacks and on older
# ones (such as R 4.3.3 and DT 0.31 as packaged by Ubuntu 24.04): the
# header's colors, the version shown, the item grid's row groups and row
# updates, and tables in full view. Each test below states
# what the app does on the stack it is run with, in headless Chrome, with no
# script injected to repair the page and no resize of the window after it
# opens. They follow the conventions of test-shinytest2.R: every test opens
# with skip_if_no_browser() (never on CRAN, never without shinytest2,
# chromote, and a Chrome binary), there are no snapshots, and every assertion
# is on a value that carries the meaning. Helpers are in helper-interface.R
# and helper-browser.R.
#
# Author: Ken Kelley

skip_if_not_installed("shinytest2")
skip_if_not_installed("DMAR")

# Fifteen pages, each a sub-tab of one of the five workspaces
# (fct_workspaces.R).
interface_tabs <- c(
  "ingest", "screen", "content", "retention", "efa", "winnow",
  "optimize", "cfa", "reliab", "bifactor", "validity", "finalize",
  "irt", "sem", "repro"
)

# The pages the running app actually has, workspace by workspace. The
# fifteen above must all be there; any other page is swept along with them.
interface_tabs_present <- function(app) {
  present <- compat_js_strings(app$get_js(paste0(
    "Array.from(document.querySelectorAll('ul[id^=\"ws_\"] a[data-value]'))",
    ".map(function(a){return a.getAttribute('data-value')})"
  )))
  expect_true(all(interface_tabs %in% present),
    info = paste("missing:", paste(setdiff(interface_tabs, present), collapse = ", "))
  )
  present
}

# The contrast of each piece of header text against the header background,
# from the colors the browser computed.
interface_header_contrast <- function(app) {
  col <- compat_navbar_colors(app)
  # The first tab that is not the active one, hovered with a real pointer move.
  hover <- compat_hover_color(app, "nav.navbar .navbar-nav .nav-link:not(.active)")
  list(
    colors = col,
    hover = hover,
    ratios = c(
      title = compat_contrast(col$brand, col$background),
      subtitle = compat_contrast(col$subtitle, col$background, opacity = col$subtitle_opacity),
      active = compat_contrast(col$active, col$background),
      rest = compat_contrast(col$rest, col$background),
      hover = compat_contrast(hover$color, col$background)
    )
  )
}

test_that("the header is navy and every piece of its text is readable", {
  skip_if_no_browser()

  compat_with_app(function(app) {
    h <- interface_header_contrast(app)
    navy <- SIEVER:::sieve_chrome_colors()[["navy"]]

    # The background is the declared color (the navy of the default
    # palette), not white (as bslib 0.7.0 can draw it) and not transparent.
    expect_identical(
      compat_parse_color(h$colors$background),
      compat_parse_color(navy)
    )
    # The header holds seven links: the five workspaces, item inclusion,
    # and About. Every page is a sub-tab of its workspace.
    expect_identical(h$colors$n_links, 7L)
    roots <- compat_js_strings(app$get_js("Array.from(document.querySelectorAll('nav.navbar a[data-value]')).map(a=>a.dataset.value)"))
    expect_identical(roots, c("study", "structure", "scores", "evidence", "report"))
    expect_true(all(interface_tabs %in% interface_tabs_present(app)))
    expect_identical(h$colors$n_distinct_rest, 1L)
    # The hover measurement really is the hover state.
    expect_true(h$hover$hovered)

    # WCAG 2 AA for normal text: at least 4.5 to 1, for every state.
    expect_true(all(h$ratios >= 4.5),
      info = paste(names(h$ratios), sprintf("%.2f", h$ratios), collapse = "; ")
    )
    # And the specific values the declared colors imply, so a silent change of
    # palette is noticed: white on the navy is 15.79, a resting tab (white at
    # 65 percent) is 7.37.
    expect_equal(h$ratios[["title"]], 15.7885, tolerance = 1e-3)
    expect_equal(h$ratios[["active"]], 15.7885, tolerance = 1e-3)
    expect_equal(h$ratios[["rest"]], 7.3708, tolerance = 1e-3)
    expect_gt(h$ratios[["hover"]], h$ratios[["rest"]])
    # Optional pages are distinct and readable on the workspace surface.
    opt <- app$get_js(paste0(
      "(function(){var a=document.querySelector('#ws_study .nav-link:has(> .sieve-nav-optional)');",
      "var core=document.querySelector('#ws_study .nav-link:not(.active):not(:has(> .sieve-nav-optional))');",
      "var e=a,bg;while(e){bg=getComputedStyle(e).backgroundColor;if(bg!=='rgba(0, 0, 0, 0)'&&bg!=='transparent')break;e=e.parentElement;}",
      "return {color:getComputedStyle(a).color,core:getComputedStyle(core).color,background:bg}})()"
    ))
    optional_ratio <- compat_contrast(opt$color, opt$background)
    expect_gte(optional_ratio, 4.5)
    expect_lt(optional_ratio, compat_contrast(opt$core, opt$background))

    # Narrow the question to the collapsed navbar too: the toggle button that
    # replaces the links on a small window is drawn in a light stroke.
    icon <- app$get_js(paste0(
      "getComputedStyle(document.querySelector('nav.navbar .navbar-toggle .icon-bar:last-child, ",
      "nav.navbar .navbar-toggler-icon')).backgroundImage"
    ))
    expect_match(icon, "stroke='rgba%28255,255,255,0.85%29'", fixed = TRUE)
  })
})

test_that("the stylesheet alone produces the header when the theme says nothing", {
  skip_if_no_browser()

  # apps/header builds the page shell with navbar-bg withheld from the theme,
  # which is the situation of a bslib release that ignores the variable.
  compat_with_app(app = "header", function(app) {
    h <- interface_header_contrast(app)
    expect_identical(
      compat_parse_color(h$colors$background),
      compat_parse_color(SIEVER:::sieve_chrome_colors()[["navy"]])
    )
    expect_true(h$hover$hovered)
    expect_true(all(h$ratios >= 4.5),
      info = paste(names(h$ratios), sprintf("%.2f", h$ratios), collapse = "; ")
    )
    expect_equal(h$ratios[["active"]], 15.7885, tolerance = 1e-3)
    expect_equal(h$ratios[["rest"]], 7.3708, tolerance = 1e-3)
  })
})

test_that("the About dialog shows the version of the running code", {
  skip_if_no_browser()

  # The launcher loads the source tree when the tests run from one and the
  # installed package otherwise; the expected label follows the same rule and
  # is read from the DESCRIPTION, not from the helper under test.
  root <- compat_source_root()
  expected <- if (!is.null(root)) {
    sprintf(
      "%s (development source)",
      unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[1, 1])
    )
  } else {
    sprintf("%s (installed package)", as.character(utils::packageVersion("SIEVER")))
  }

  compat_with_app(function(app) {
    app$click(selector = "nav.navbar a.nav-link[onclick*='show_about']")
    siever_wait_render(app)
    body <- app$get_js("document.querySelector('.modal.show .modal-body').textContent")
    body <- gsub("[[:space:]]+", " ", body)
    expect_match(body, sprintf("SIEVER version %s.", expected), fixed = TRUE)
  })
})

test_that("the browser tests drive the working source even when another SIEVER is installed", {
  skip_if_no_browser()
  skip_if_not_installed("pkgload")
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  source_version <- unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[1, 1])

  # A SIEVER labeled 0.0.1, installed in a library placed first on the path of
  # the app process. The About dialog must still report the source tree. (It
  # does under shinytest2 0.5.0 and later whatever the launcher does, because
  # shinytest2 loads the package under development itself; the launcher loads
  # the source tree directly so that this does not rest on that.)
  lib <- compat_install_copy(root, version = "0.0.1")
  compat_with_app(libs = lib, function(app) {
    app$click(selector = "nav.navbar a.nav-link[onclick*='show_about']")
    siever_wait_render(app)
    body <- gsub(
      "[[:space:]]+", " ",
      app$get_js("document.querySelector('.modal.show .modal-body').textContent")
    )
    expect_match(body, sprintf("SIEVER version %s (development source).", source_version),
      fixed = TRUE
    )
    expect_false(grepl("0.0.1", body, fixed = TRUE))
  })
})

test_that("the item grid draws its row groups", {
  skip_if_no_browser()

  compat_with_app(load_example = TRUE, function(app) {
    groups <- function() {
      compat_js_strings(app$get_js(paste0(
        "Array.from(document.querySelectorAll(",
        "'#ingest-type_grid tbody tr.dtrg-group')).map(function(r){return r.textContent.trim()})"
      )))
    }
    item_rows <- function() {
      as.integer(app$get_js(
        "document.querySelectorAll('#ingest-type_grid tbody tr:not(.dtrg-group)').length"
      ))
    }
    # One group row, spanning the grid, above the nine items in use.
    # The Use cell and the group lines say Included and Excluded, the house
    # words for items.
    expect_identical(groups(), "Included items")
    expect_identical(item_rows(), 9L)
    # The column that carries the group label is hidden when it is drawn as a
    # divider: the first header is the variable name.
    header <- compat_js_strings(app$get_js(paste0(
      "Array.from(document.querySelectorAll(",
      "'#ingest-type_grid thead th')).map(function(t){return t.textContent.trim()})"
    )))
    expect_identical(header[1], "Variable Name")
    expect_false("Group" %in% header)
    expect_identical(as.integer(app$get_js(
      "document.querySelectorAll('#ingest-type_grid caption').length"
    )), 0L)

    # Drop one item from the grid's own Use cell: a second divider appears and
    # the item moves under it.
    app$run_js(paste0(
      "var s = Array.from(document.querySelectorAll('#ingest-type_grid tbody select'))",
      ".find(function(x){ return Array.from(x.options).some(function(o){ return o.value === 'Excluded'; }); });",
      "s.value = 'Excluded'; s.dispatchEvent(new Event('change', {bubbles: true}));"
    ))
    siever_wait_render(app)
    expect_identical(groups(), c(
      "Included items",
      "Items excluded from every analysis (kept in the data; set Use to Included to include one again)"
    ))
    expect_identical(item_rows(), 9L)
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

test_that("an edit in the item grid updates its row in place, keeping the table, the focus and a value being typed", {
  skip_if_no_browser()
  # The row update finds the grid's body table, not the first table in the
  # grid's box, which is DataTables' copy of the header when the grid
  # scrolls horizontally (the server's tests read the message, not the
  # page, so only a browser shows it). It rewrites the changed cells with
  # no redraw: a redraw detaches every row, and the browser then commits a
  # value being typed in another row.
  compat_with_app(load_example = TRUE, function(app) {
    cell <- function(item, field) {
      sprintf("document.querySelector('#ingest-type_grid [data-item=\"%s\"][data-field=\"%s\"]')", item, field)
    }
    js_value <- function(expr) app$get_js(expr)
    focused <- function() {
      js_value("(document.activeElement && document.activeElement.getAttribute('data-item') + '/' + document.activeElement.getAttribute('data-field')) || ''")
    }
    # Marks: a redraw of the grid replaces its table, and a row update
    # replaces the controls of the cells it sets.
    app$run_js("document.querySelector('#ingest-type_grid .dataTables_scrollBody table').setAttribute('data-probe', 'drawn');")
    same_table <- function() {
      isTRUE(js_value("document.querySelector('#ingest-type_grid .dataTables_scrollBody table').getAttribute('data-probe') === 'drawn'"))
    }
    app$run_js(paste0(cell("t10_addition", "bounds"), ".setAttribute('data-probe', 'old');"))
    # Bounds typed in t10's cell and committed, then the focus moved on to
    # its Construct cell (as Tab does) before the server answers.
    app$run_js(paste0(
      "var e = ", cell("t10_addition", "bounds"), "; e.scrollIntoView({block: 'center'}); e.focus();",
      "e.value = '0 to 200'; e.dispatchEvent(new Event('change', {bubbles: true}));",
      "var c = ", cell("t10_addition", "construct"), "; c.focus(); c.setSelectionRange(1, 3);"
    ))
    expect_true(siever_wait_for(app, paste0(
      "(function(){ var e = ", cell("t10_addition", "bounds"), "; return !!e && e.getAttribute('value') === '0 to 200'; })()"
    ), timeout = 15000))
    expect_true(js_value(paste0(cell("t10_addition", "bounds"), ".getAttribute('data-probe') === null")))
    expect_true(same_table())
    expect_match(
      js_value(paste0(cell("t10_addition", "type"), ".closest('td').textContent")),
      "201 options (97 observed)", fixed = TRUE
    )
    expect_identical(focused(), "t10_addition/construct")
    expect_identical(js_value("[document.activeElement.selectionStart, document.activeElement.selectionEnd].join(',')"), "1,3")
    # The rewritten controls get what a drawn table gives its controls: no
    # Tab stop of their own, and their title as the explanation.
    expect_identical(js_value(paste0(cell("t10_addition", "bounds"), ".getAttribute('tabindex')")), "-1")
    expect_true(js_value(paste0(cell("t10_addition", "bounds"), ".getAttribute('title') === null")))
    expect_match(js_value(paste0(cell("t10_addition", "bounds"), ".getAttribute('data-sieve-explanation')")), "t10_addition: bounds", fixed = TRUE)
    # A value being typed (as keystrokes, which mark the field as edited) in
    # another row's Construct cell while bounds are committed in t13's cell:
    # the typed value is not committed, keeps the focus, and stays.
    app$run_js(paste0("var c = ", cell("t1_visual_perception", "construct"), "; c.scrollIntoView({block: 'center'}); c.focus(); c.setSelectionRange(c.value.length, c.value.length);"))
    app$get_chromote_session()$Input$insertText(text = "Zq")
    typed <- js_value(paste0(cell("t1_visual_perception", "construct"), ".value"))
    expect_match(typed, "Zq$")
    app$run_js(paste0(
      "var e = ", cell("t13_straight_and_curved_capitals", "bounds"), ";",
      "e.value = '0 to 400'; e.dispatchEvent(new Event('change', {bubbles: true}));"
    ))
    expect_true(siever_wait_for(app, paste0(
      "(function(){ var e = ", cell("t13_straight_and_curved_capitals", "bounds"), "; return !!e && e.getAttribute('value') === '0 to 400'; })()"
    ), timeout = 15000))
    siever_wait_render(app)
    expect_identical(app$get_value(input = "ingest-cell_edit")$field, "bounds")
    expect_identical(focused(), "t1_visual_perception/construct")
    expect_identical(js_value(paste0(cell("t1_visual_perception", "construct"), ".value")), typed)
    expect_false(isTRUE(js_value(paste0(
      "Array.from(document.querySelectorAll('datalist option')).some(function(o){ return /Zq$/.test(o.value); })"
    ))))
    # Committed with the focus kept in the cell (as Enter does): the cell's
    # field is replaced, and the focus is in the new one.
    app$run_js(paste0(
      "var e = ", cell("t12_counting_groups_of_dots", "bounds"), "; e.scrollIntoView({block: 'center'}); e.focus();",
      "e.value = '0 to 250'; e.dispatchEvent(new Event('change', {bubbles: true}));"
    ))
    expect_true(siever_wait_for(app, paste0(
      "(function(){ var e = ", cell("t12_counting_groups_of_dots", "bounds"), "; return !!e && e.getAttribute('value') === '0 to 250'; })()"
    ), timeout = 15000))
    expect_identical(focused(), "t12_counting_groups_of_dots/bounds")
    expect_true(same_table())
    # Reverse? from its cell: the row shows the bounds of the reversal, and
    # the focus stays on the Reverse? control.
    app$run_js(paste0(
      "var s = ", cell("t9_word_meaning", "reverse"), "; s.scrollIntoView({block: 'center'}); s.focus();",
      "s.value = 'Yes'; s.dispatchEvent(new Event('change', {bubbles: true}));"
    ))
    expect_true(siever_wait_for(app, paste0(
      "(function(){ var s = ", cell("t9_word_meaning", "reverse"),
      "; return !!s && s.closest('tr').textContent.indexOf('the bounds of its reversal') >= 0; })()"
    ), timeout = 15000))
    expect_identical(focused(), "t9_word_meaning/reverse")
    expect_true(same_table())
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

test_that("a row update that rewrites the cell being typed in sends nothing half typed, and keeps the text when the field survives", {
  skip_if_no_browser()
  # Removing a field that holds typed text fires a change event, which would
  # send the half typed text. The update is dispatched in the page itself, so
  # it arrives while the text is being typed (as keystrokes, which mark the
  # field as edited).
  compat_with_app(load_example = TRUE, function(app) {
    cell <- function(item, field) {
      sprintf("document.querySelector('#ingest-type_grid [data-item=\"%s\"][data-field=\"%s\"]')", item, field)
    }
    bounds_col <- match("Bounds", sieve_grid_columns()) - 1L
    dispatch <- function(item, html) {
      msg <- jsonlite::toJSON(list(custom = list(sieve_ingest_row = list(
        id = "ingest-type_grid", key_col = match("Variable", sieve_grid_columns()) - 1L,
        rows = list(list(key = item, cells = stats::setNames(list(html), as.character(bounds_col))))
      ))), auto_unbox = TRUE)
      app$run_js(sprintf("Shiny.shinyapp.dispatchMessage(%s);", encodeString(as.character(msg), quote = "'")))
    }
    before <- app$get_value(input = "ingest-cell_edit")
    # A cell that becomes text (the bounds of a reversal): nothing is sent,
    # and the focus stays in the grid, on the cell.
    app$run_js(paste0("var e = ", cell("t2_cubes", "bounds"), "; e.scrollIntoView({block: 'center'}); e.focus();"))
    app$get_chromote_session()$Input$insertText(text = "1 t")
    dispatch("t2_cubes", '<div class="small">0 to 40</div>')
    siever_wait_render(app)
    expect_identical(app$get_value(input = "ingest-cell_edit"), before)
    expect_identical(app$get_js("document.activeElement.tagName"), "TD")
    expect_match(app$get_js("document.activeElement.textContent"), "0 to 40", fixed = TRUE)
    # A field that survives the update keeps the text being typed, with the
    # focus, and leaving it commits the text as before.
    app$run_js(paste0("var e = ", cell("t4_lozenges", "bounds"), "; e.scrollIntoView({block: 'center'}); e.focus();"))
    app$get_chromote_session()$Input$insertText(text = "1 to")
    dispatch("t4_lozenges", sieve_cell_text("ingest-cell_edit", "t4_lozenges", "bounds", ""))
    siever_wait_render(app)
    expect_identical(app$get_value(input = "ingest-cell_edit"), before)
    expect_identical(app$get_js("document.activeElement.getAttribute('data-item') + '/' + document.activeElement.value"), "t4_lozenges/1 to")
    app$get_chromote_session()$Input$insertText(text = " 7")
    app$run_js(paste0(cell("t4_lozenges", "bounds"), ".blur();"))
    expect_true(siever_wait_for(app, "(function(){ var v = Shiny.shinyapp.$inputValues['ingest-cell_edit']; return !!v && v.value === '1 to 7'; })()", timeout = 15000))
    edit <- app$get_value(input = "ingest-cell_edit")
    expect_identical(c(edit$item, edit$field, edit$value), c("t4_lozenges", "bounds", "1 to 7"))
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

test_that("a DT without RowGroup still gives a working grid and says why", {
  skip_if_no_browser()

  # The app process gets a DT whose RowGroup extension has been removed, as in
  # a distribution build that leaves bundled extensions out. The grid is
  # drawn, never "The extension RowGroup does not exist" in its place.
  lib <- compat_dt_without("RowGroup")
  compat_with_app(load_example = TRUE, libs = lib, function(app) {
    expect_identical(compat_output_errors(app), character(0))
    expect_false(grepl("does not exist", app$get_html("#ingest-type_grid"), fixed = TRUE))

    # The grid is there with all nine items, ordered and labeled by group in
    # an ordinary first column, with no divider rows.
    expect_identical(as.integer(app$get_js(
      "document.querySelectorAll('#ingest-type_grid tbody tr.dtrg-group').length"
    )), 0L)
    expect_identical(siever_n_table_rows(app, "ingest-type_grid"), 9L)
    header <- compat_js_strings(app$get_js(paste0(
      "Array.from(document.querySelectorAll(",
      "'#ingest-type_grid thead th')).map(function(t){return t.textContent.trim()})"
    )))
    expect_identical(header[1:2], c("Group", "Variable Name"))
    first_col <- compat_js_strings(app$get_js(paste0(
      "Array.from(document.querySelectorAll(",
      "'#ingest-type_grid tbody tr td:first-child')).map(function(t){return t.textContent.trim()})"
    )))
    # The group line says Included.
    expect_identical(unique(first_col), "Included items")

    # The note is on the page above the grid, names the extension, and says
    # how to get it.
    caption <- app$get_js("document.querySelector('#ingest-type_grid caption').textContent")
    expect_match(caption, "RowGroup", fixed = TRUE)
    expect_match(caption, "install.packages(\"DT\")", fixed = TRUE)
    expect_identical(
      app$get_js("getComputedStyle(document.querySelector('#ingest-type_grid caption')).captionSide"),
      "top"
    )

    # The rest of the tab is unaffected: the status chip reports the load.
    expect_match(app$get_html("#ingest-status_chip"), "9 items loaded", fixed = TRUE)
  })
})

test_that("tables in every sub-tab are drawn in full view without a resize", {
  skip_if_no_browser()

  compat_with_app(load_example = TRUE, function(app) {
    idle <- function() siever_wait_render(app, timeout = 600 * 1000)
    # A sub-tab can have a natural height greater than the browser window.
    # Classify a table by the space actually available through all clipping
    # ancestors and the viewport, measured at the same scroll position as
    # its visible rows. The raw region height alone overstates that capacity.
    visible_tables <- function() {
      # All metrics share one DOM observation: a redraw between two browser
      # commands can otherwise replace a table before its viewport is read.
      measured <- app$get_js("(function(){
        var out=[];
        document.querySelectorAll('div.datatables').forEach(function(w){
          if(!w.offsetParent)return;
          var body=w.querySelector('.dataTables_scrollBody table')||w.querySelector('table.dataTable');
          if(!body)return;
          var head=w.querySelector('.dataTables_scrollHead table')||body;
          var pane=w.closest('.tab-pane');
          var region=pane?pane.querySelector(':scope > .card-body'):null;
          var squeezed=[],a=w.parentElement;
          while(region&&a&&a!==region&&a!==document.body){
            var cs=getComputedStyle(a);
            if(cs.overflowY!=='visible'&&a.scrollHeight-a.clientHeight>1&&a.clientHeight<region.clientHeight-120)
              squeezed.push(String(a.className).split(' ').slice(0,2).join('.')+' '+a.clientHeight+'/'+a.scrollHeight);
            a=a.parentElement;
          }
          w.scrollIntoView({block:'start'});
          var top=0,bottom=window.innerHeight;a=w.parentElement;
          while(a&&a!==document.documentElement){
            if(getComputedStyle(a).overflowY!=='visible'){
              var r=a.getBoundingClientRect();
              top=Math.max(top,r.top+a.clientTop);
              bottom=Math.min(bottom,r.top+a.clientTop+a.clientHeight);
            }
            a=a.parentElement;
          }
          var rows=Array.from(body.querySelectorAll('tbody tr'));
          var visible=rows.filter(function(r){var b=r.getBoundingClientRect();
            return b.height>0&&b.width>0&&b.top>=top-1&&b.bottom<=bottom+1;}).length;
          var ths=Array.from(head.querySelectorAll('thead th'));
          var tds=rows.length?Array.from(rows[rows.length-1].querySelectorAll('td')):[];
          var mis=-1;
          if(ths.length&&tds.length===ths.length){mis=0;ths.forEach(function(t,i){
            mis=Math.max(mis,Math.abs(t.getBoundingClientRect().width-tds[i].getBoundingClientRect().width));});}
          var wr=w.getBoundingClientRect(),inner=w.querySelector('.dataTables_wrapper')||body,ir=inner.getBoundingClientRect();
          var sc=w.querySelector('.dataTables_scrollBody, .dt-scroll-body');
          var horizontalTable=(sc&&sc.querySelector('table'))||w.querySelector('table.dataTable');
          var tr=horizontalTable.getBoundingClientRect();
          var inside=tr.left>=wr.left-2&&tr.right<=wr.right+2,accessible=false;
          if(sc){var sr=sc.getBoundingClientRect(),left=tr.left-sr.left+sc.scrollLeft;
            accessible=['auto','scroll'].includes(getComputedStyle(sc).overflowX)&&sc.clientWidth>=200&&
              sc.clientWidth<=wr.width+2&&left>=-2&&left+tr.width<=sc.scrollWidth+2;}
          out.push({id:w.id,width:Math.round(body.getBoundingClientRect().width),box_w:Math.round(wr.width),
            box_h:Math.round(wr.height),table_h:Math.round(ir.height),spill:Math.round(ir.bottom-wr.bottom),
            rows:rows.length,visible:visible,cols:ths.length,zero_cols:ths.filter(t=>t.getBoundingClientRect().width<2).length,
            misalign:Math.round(mis*10)/10,region_h:region?region.clientHeight:-1,squeezed:squeezed.join(' ; '),
            view_h:Math.max(0,bottom-top),horizontal_ok:inside||accessible});
        });return out;
      })()")
      if(!length(measured))return(data.frame(id=character(0)))
      m <- do.call(rbind,lapply(measured,function(x)as.data.frame(x,stringsAsFactors=FALSE)))
      whole <- c("width","box_w","box_h","table_h","spill","rows","visible","cols","zero_cols","region_h")
      m[whole] <- lapply(m[whole],as.integer)
      m$misalign <- as.numeric(m$misalign)
      m$view_h <- as.numeric(m$view_h)
      m
    }
    siever_use_empirical_basis(app)
    found <- new.env()
    found$tables <- list()
    found$squeezed <- list()
    found$region <- integer(0)
    measure <- function(tab, subtab, first) {
      expect_identical(compat_output_errors(app), character(0), info = paste(tab, subtab))
      key <- paste(tab, subtab, sep = " / ")
      found$squeezed[[key]] <- compat_squeezed(app)
      found$region[[key]] <- compat_region_height(app)
      m <- visible_tables()
      if (nrow(m)) {
        m$where <- key
        m$first <- first
        found$tables[[length(found$tables) + 1L]] <- m
      }
      expect_identical(compat_output_errors(app), character(0), info = key)
    }

    for (tab in interface_tabs_present(app)) {
      siever_open_tab(app, tab, timeout = 300 * 1000)
      # The actions that produce a table and cost a few seconds at most.
      if (tab == "content") {
        app$click("content-demo")
        idle()
      }
      if (tab == "sem") {
        app$set_inputs(`sem-outcomes` = "Speed", `sem-predictors` = c("Visual", "Textual"))
        idle()
        app$click("sem-fit_go")
        idle()
      }
      # The first CFA fit and the first reliability table run on entering
      # their tab when the run policy allows, or wait for the tab's button,
      # which is then pressed (siever_first_result()). The tables are
      # measured with the completed scientific output either way.
      if (tab == "cfa") {
        # A current fit is stated in the method line above the tabs. The
        # identical model, fit on the Items tab (its overall model) before
        # this tab is reached, is adopted without a click (fct_fit_store.R),
        # whatever the measured rates estimate for a fit of its own.
        waited <- siever_first_result(app, "cfa",
          "(document.querySelector('#cfa-method_line')?.textContent || '').includes(', current.')",
          button = "cfa-fit_go"
        )
        expect_null(waited)
      }
      if (tab == "reliab") {
        siever_first_result(app, "reliab",
          "document.querySelectorAll('#reliab-omega_tbl table tbody tr').length === 3",
          button = "reliab-run_go", report = "reliab-omega_tbl"
        )
      }
      if (tab == "bifactor") {
        if (isTRUE(app$get_js("!!document.getElementById('bifactor-fit')"))) app$click("bifactor-fit")
        idle()
      }
      if (tab == "validity") {
        app$click("validity-htmt_on")
        idle()
        # The identical measurement model, fit on the Items tab (its overall
        # model) before this tab is reached, is adopted without a click
        # (fct_fit_store.R), and the page says whose fit it reads.
        expect_false(app$get_js("!!document.getElementById('validity-fit_go')"))
        expect_match(app$get_js("document.querySelector('#validity-gate').textContent"),
          "Reused the fit from", fixed = TRUE)
        app$wait_for_js("document.querySelectorAll('#validity-ave_tbl table tbody tr').length === 3",
          timeout = 120 * 1000)
        idle()
      }
      if (tab == "finalize") {
        app$click("finalize-split")
        idle()
      }
      if (tab == "retention") {
        # The bundled example's Dimensionality analyses wait for their
        # buttons (DMAR's 1,000 reference datasets, VSS on
        # DMAR's solutions, the fit by k under ML with FIML).
        # A wait that times out stops with what the page showed. The
        # summary waits for all three analyses, so its limit allows for a
        # busy machine.
        siever_wait_or_report(app, "!!document.getElementById('retention-pa_go_table')",
          "The parallel analysis button", report = c("retention-summary_reco", "retention-status_line"))
        for (id in c("retention-pa_go_table", "retention-vss_go_table", "retention-fit_go_table")) {
          if (isTRUE(app$get_js(sprintf("!!document.getElementById('%s')", id)))) app$click(id)
        }
        siever_wait_or_report(app,
          "(function(){var t=(document.getElementById('retention-summary_reco')||{}).textContent||''; return t.length > 0 && !/Not run yet|not run yet|running|has not run/.test(t);})()",
          "The retention summary", report = c("retention-summary_reco", "retention-status_line"),
          timeout = 900 * 1000)
        idle()
      }
      subtabs <- compat_subtabs(app, tab)
      if (!length(subtabs)) next
      measure(tab, subtabs[1], first = TRUE)
      for (subtab in subtabs[-1]) {
        compat_open_subtab(app, tab, subtab)
        if (tab == "cfa" && subtab == "Bollen-Stine") {
          app$click("cfa-bs_run")
          idle()
        }
        if (tab == "cfa" && subtab == "Compare structures") {
          app$click("cfa-compare_go")
          idle()
        }
        if (tab == "reliab" && subtab == "Factor scores (latent)") {
          # The identical measurement model the CFA page fit is adopted
          # without a click, so the button is there only when the session
          # holds no such fit.
          if (isTRUE(app$get_js("!!document.getElementById('reliab-scores_go')"))) {
            app$click("reliab-scores_go")
          }
          idle()
        }
        if (tab == "finalize" && subtab == "Measurement Invariance") {
          # Run invariance reads the grouping variable, which the server
          # fills in when the page opens, so the click waits for it.
          expect_true(siever_wait_for(app,
            "(function(){var e=document.getElementById('finalize-group');return !!e && !!e.value;})()",
            120 * 1000))
          idle()
          app$click("finalize-run_inv")
          idle()
          app$click("finalize-inv_diag")
          idle()
          app$click("finalize-dmacs_go")
          idle()
          # The layout measurement must follow the asynchronous DT row draw.
          siever_wait_or_report(app,
            "document.querySelectorAll('#finalize-inv_tbl table tbody tr').length === 4",
            "The four rows of the invariance table",
            report = c("finalize-group", "finalize-inv_head", "finalize-inv_msg"))
        }
        if (tab == "finalize" && subtab == "Alignment") {
          app$click("finalize-align_go")
          idle()
        }
        measure(tab, subtab, first = FALSE)
      }
    }
    tables <- do.call(rbind, found$tables)

    # The invariance, bifactor indices and AVE tables are drawn, with the
    # rows the bundled example gives them.
    named <- c("finalize-inv_tbl" = 4L, "bifactor-idx_tbl" = 4L, "validity-ave_tbl" = 3L)
    expect_true(all(names(named) %in% tables$id))
    expect_identical(tables$rows[match(names(named), tables$id)], unname(named),
      info = paste(capture.output(print(tables[tables$id %in% names(named),
        c("id", "where", "rows", "first")], row.names = FALSE)), collapse = "\n"))

    # Twelve of the fifteen pages have a table in a sub-tab other than the
    # first (Items and Optimizer have no sub-tabs, and Reproducible Code has
    # no table). The sweep draws such tables on eleven of them. IRT is the
    # exception: its
    # later tables need ordered items, which the bundled example does not
    # have, so for IRT the layout of each sub-tab is checked in step 5 below
    # without a table in it.
    later <- tables[!tables$first, , drop = FALSE]
    reached <- unique(sub("-.*$", "", later$id))
    expected <- c(
      "ingest", "content", "screen", "retention", "efa", "cfa", "sem", "reliab",
      "bifactor", "validity", "finalize"
    )
    expect_true(all(expected %in% reached),
      info = paste("not reached:", paste(setdiff(expected, reached), collapse = ", "))
    )
    expect_gte(nrow(later), 35L)

    # 1. Width: natural or researcher-chosen column widths may leave space in
    #    the box. Every column must either fit inside it or remain reachable
    #    through a usable horizontal scroll area. No column has collapsed,
    #    and header columns sit over their body columns.
    expect_true(all(tables$width > 200), info = paste(tables$id[tables$width <= 200], collapse = ", "))
    expect_true(all(tables$horizontal_ok),
      info = paste(tables$id[is.na(tables$horizontal_ok) | !tables$horizontal_ok], collapse = ", ")
    )
    expect_identical(sum(tables$zero_cols), 0L)
    expect_true(all(tables$misalign <= 1), info = paste(tables$id[tables$misalign > 1], collapse = ", "))

    # 2. Height: a table does not run past the bottom of its own box, where it
    #    would be drawn over whatever follows it in the card.
    expect_true(all(tables$spill <= 1),
      info = paste(tables$id[tables$spill > 1], tables$spill[tables$spill > 1], collapse = ", ")
    )

    # 3. Position: no card between a table and the scrolling body of its
    #    sub-tab is a short scroll area of its own: no card is squeezed to a
    #    few lines, with the table below its fold.
    in_subtab <- tables[tables$region_h > 0, , drop = FALSE]
    expect_true(all(!nzchar(in_subtab$squeezed)),
      info = paste(in_subtab$id[nzchar(in_subtab$squeezed)],
        in_subtab$squeezed[nzchar(in_subtab$squeezed)],
        collapse = " | "
      )
    )

    # 4. Visible rows: scrolled to the top of its sub-tab, a table that fits in
    #    the sub-tab shows every row, and a longer one shows at least five.
    expect_true(all(is.finite(in_subtab$view_h) & in_subtab$view_h > 0),
      info = paste(in_subtab$id[!is.finite(in_subtab$view_h) | in_subtab$view_h <= 0], collapse = ", "))
    fits <- in_subtab$table_h + 60 <= pmin(in_subtab$region_h, in_subtab$view_h)
    expect_true(all(in_subtab$visible[fits] == in_subtab$rows[fits]),
      info = paste(in_subtab$id[fits][in_subtab$visible[fits] != in_subtab$rows[fits]],
        collapse = ", "
      )
    )
    expect_true(all(in_subtab$visible[!fits] >= pmin(5L, in_subtab$rows[!fits])),
      info = paste(in_subtab$id[!fits], in_subtab$visible[!fits], collapse = ", ")
    )

    # 5. The same for every sub-tab, with or without a table in it: nothing in
    #    the body of a sub-tab is a short nested scroll area, and the body
    #    itself is tall enough to work in (not a strip of about a hundred
    #    pixels under an open accordion).
    offenders <- Filter(length, found$squeezed)
    expect_identical(length(offenders), 0L,
      info = paste(names(offenders), vapply(offenders, paste, "", collapse = " ; "),
        collapse = " | "
      )
    )
    expect_gte(length(found$squeezed), 50L)
    expect_true(all(found$region >= 300L),
      info = paste(names(found$region)[found$region < 300L],
        found$region[found$region < 300L],
        collapse = ", "
      )
    )
    logs <- app$get_logs()
    throws <- logs[logs$location == "chromote" & logs$level == "throw", , drop = FALSE]
    expect_equal(nrow(throws), 0L)
  })

})

test_that("the bifactor indices, AVE and invariance tables are in view on a laptop window", {
  skip_if_no_browser()

  # 1366 by 768 is the harder case: less height to share, so a card body
  # squeezed to a few dozen pixels would show none or few of these tables'
  # rows.
  compat_with_app(load_example = TRUE, width = 1366, height = 768, function(app) {
    idle <- function() siever_wait_render(app, timeout = 600 * 1000)
    one <- function(id) {
      m <- compat_measure_tables(app)
      m[m$id == id, , drop = FALSE]
    }
    check <- function(m, rows) {
      expect_identical(nrow(m), 1L)
      expect_identical(m$rows, rows, info = m$id)
      expect_lte(abs(m$width - m$box_w), 2)
      expect_identical(m$zero_cols, 0L)
      expect_lte(m$spill, 1)
      expect_identical(m$squeezed, "", info = m$id)
      # Every row when the table fits its sub-tab, all but one otherwise (the
      # invariance ladder's wrapped model names make it taller than the
      # sub-tab at this width).
      fits <- m$table_h + 60 <= m$region_h
      expect_gte(m$visible, if (fits) rows else rows - 1L)
    }

    siever_open_tab(app, "bifactor")
    app$click("bifactor-fit")
    idle()
    compat_open_subtab(app, "bifactor", "Bifactor indices")
    check(one("bifactor-idx_tbl"), 4L)

    siever_open_tab(app, "validity")
    if (isTRUE(app$get_js("!!document.getElementById('validity-fit_go')"))) app$click("validity-fit_go")
    app$wait_for_js("document.querySelectorAll('#validity-ave_tbl table tbody tr').length === 3",
      timeout = 120 * 1000)
    idle()
    check(one("validity-ave_tbl"), 3L)

    siever_open_tab(app, "finalize")
    compat_open_subtab(app, "finalize", "Measurement Invariance")
    app$click("finalize-run_inv")
    idle()
    check(one("finalize-inv_tbl"), 4L)
    # Compact rung names retain the complete constraints for pointer and
    # keyboard users; shortening the visible cell must not erase meaning.
    descriptions <- app$get_js(paste0(
      "Array.from(document.querySelectorAll('#finalize-inv_tbl tbody tr td:first-child .sieve-explanation'))",
      ".map(function(e){return {text:e.textContent,description:e.getAttribute('data-sieve-explanation'),",
      "nativeTitle:e.hasAttribute('title'),aria:e.getAttribute('aria-label'),tabindex:e.getAttribute('tabindex')}})"
    ))
    expect_length(descriptions, 4L)
    expect_identical(vapply(descriptions, `[[`, "", "text"), c("Configural", "Metric", "Scalar", "Strict"))
    expected_constraints <- c("Configural (same pattern, all free)", "Metric (loadings)",
      "Scalar (loadings + intercepts)", "Strict (loadings + intercepts + residuals)")
    expect_identical(vapply(descriptions, `[[`, "", "description"), expected_constraints)
    expect_false(any(vapply(descriptions, `[[`, FALSE, "nativeTitle")))
    expect_identical(vapply(descriptions, `[[`, "", "aria"), expected_constraints)
    expect_identical(vapply(descriptions, `[[`, "", "tabindex"), rep("0", 4L))
    app$run_js("document.querySelector('#finalize-inv_tbl tbody tr td:first-child .sieve-explanation').focus()")
    expect_true(app$get_js("document.activeElement.matches('#finalize-inv_tbl .sieve-explanation')"))
    app$wait_for_js("(function(){var e=document.querySelector('.sieve-accessible-explanation');return e&&e.getBoundingClientRect().width>0&&e.getBoundingClientRect().height>0})()",
      timeout = 10000)
    expect_identical(app$get_js("document.querySelector('.sieve-accessible-explanation').textContent"), expected_constraints[1L])

    expect_identical(compat_output_errors(app), character(0))
  })
})

test_that("a table shown again after the layout changed while it was hidden is still aligned", {
  skip_if_no_browser()

  compat_with_app(load_example = TRUE, function(app) {
    one <- function(id) {
      m <- compat_measure_tables(app)
      m[m$id == id, , drop = FALSE]
    }
    siever_open_tab(app, "screen")
    app$run_js("document.querySelector(\".tab-pane[data-value='screen']\").style.width='70%';")
    siever_wait_render(app)
    before <- one("screen-desc_tbl")
    expect_identical(before$rows, 9L)

    # Hide the table, widen its page while hidden, then show it again.
    # Screening has no sidebar; this exercises its responsive container.
    # The table has to pick up the new width when it is shown.
    compat_open_subtab(app, "screen", "Item-total")
    app$run_js("document.querySelector(\".tab-pane[data-value='screen']\").style.width='100%';")
    siever_wait_render(app)
    compat_open_subtab(app, "screen", "Descriptives")
    after <- one("screen-desc_tbl")

    expect_gt(after$box_w, before$box_w + 100)
    # Semantic reading widths can exceed either viewport. Re-showing the
    # table must preserve its usable columns and make its last column
    # reachable inside the table's own scroll area.
    expect_gte(after$width, min(before$width, after$box_w) - 2)
    horizontal <- app$get_js(paste0(
      "(function(){var w=document.getElementById('screen-desc_tbl');",
      "var s=w.querySelector('.dataTables_scrollBody, .dt-scroll-body');",
      "var t=(s&&s.querySelector('table'))||w.querySelector('table.dataTable');",
      "var box=w.getBoundingClientRect(),r=t.getBoundingClientRect();",
      "if(r.left>=box.left-2&&r.right<=box.right+2)return true;",
      "if(!s)return false;var sr=s.getBoundingClientRect();",
      "if(!['auto','scroll'].includes(getComputedStyle(s).overflowX)||",
      "s.clientWidth<200||s.clientWidth>box.width+2)return false;",
      "s.scrollLeft=s.scrollWidth-s.clientWidth;",
      "var cells=Array.from(t.querySelectorAll('tbody tr:first-child td'))",
      ".filter(function(c){return getComputedStyle(c).display!=='none'});",
      "if(!cells.length)return false;var last=cells[cells.length-1].getBoundingClientRect();",
      "return last.left>=sr.left-2&&last.right<=sr.right+2})()"
    ))
    expect_true(horizontal)
    siever_wait_render(app)
    scrolled <- one("screen-desc_tbl")
    expect_identical(after$zero_cols, 0L)
    expect_lte(after$misalign, 1)
    expect_lte(scrolled$misalign, 1)
    expect_identical(after$rows, 9L)
    expect_identical(scrolled$visible, 9L)
  })
})

# ---- Gates, the action bar, and pagination --------------------------------

# The analysis tabs that show sieve_gate(state) until data are confirmed.
# Content Validity computes from a ratings file without respondent data,
# Reproducible Code is usable at any time, and Ingest is where
# the data arrive; Items is the one analysis tab with no gate output. The EFA
# page shows its gate in its one status line (results first), every other
# gated tab in its gate output.
interface_gated_tabs <- c(
  "screen", "retention", "efa", "optimize", "cfa", "irt", "sem",
  "reliab", "bifactor", "validity", "finalize"
)

test_that("before any data are loaded every gated tab says what to do next, at 1,280 and 1,600 px", {
  skip_if_no_browser()

  for (width in c(1280L, 1600L)) {
    compat_with_app(width = width, height = if (width == 1280L) 800L else 1000L, function(app) {
      # Before any data the Optimizer cannot be reached: it is offered under
      # the empirical basis, which is applied to a loaded study (a load starts
      # every study from the theoretical basis), so its link is hidden. The
      # page draws the link and the server hides it once the session starts
      # (sieve_workflow_wire()), so the check waits for that answer (on a
      # loaded machine the link can be read before it); a link that is
      # never hidden still fails.
      visible_optimize <- "Array.from(document.querySelectorAll(\"#ws_structure a[data-value='optimize']\")).filter(function(a){return a.closest('li') && getComputedStyle(a.closest('li')).display !== 'none' && !a.closest('li').hidden;}).length"
      siever_wait_for(app, sprintf("%s === 0", visible_optimize), timeout = 15000)
      expect_identical(as.integer(app$get_js(visible_optimize)), 0L)
      for (tab in setdiff(interface_gated_tabs, "optimize")) {
        expect_identical(siever_open_tab(app, tab, timeout = 300 * 1000), tab)
        # The output container is display: contents under bslib's fill layout,
        # so it has no box of its own; whether the gate is on screen is read
        # from the alert it holds.
        gate <- app$get_js(sprintf(
          "(function(){var g=document.querySelector('#%s'); var a=g ? g.firstElementChild : null; return a && a.offsetParent ? g.textContent.replace(/\\s+/g,' ').trim() : ''})()",
          if (tab == "efa") "efa-status_line" else paste0(tab, "-gate")
        ))
        # The gate names the tab to go to and the control to press.
        expect_match(gate, "No data loaded yet.", fixed = TRUE, info = sprintf("%s at %d px", tab, width))
        expect_match(gate, "Go to Ingest and press Load in the action bar above the panels",
          fixed = TRUE,
          info = sprintf("%s at %d px", tab, width)
        )
        expect_identical(compat_output_errors(app), character(0), info = sprintf("%s at %d px", tab, width))
      }
    })
  }
})

test_that("the Ingest action bar stays in view at 1,280 px with the sidebar collapsed", {
  skip_if_no_browser()

  compat_with_app(width = 1280L, height = 800L, function(app) {
    in_view <- function(id) {
      app$get_js(sprintf(
        "(function(){var e=document.getElementById('%s'); if(!e) return false; var r=e.getBoundingClientRect();
          return r.width>0 && r.height>0 && r.top>=0 && r.left>=0 && r.bottom<=window.innerHeight && r.right<=window.innerWidth})()",
        id
      ))
    }
    # The main action bar exposes Load, Confirm, and the status chip even
    # when the longer file-choice sidebar needs scrolling.
    expect_true(in_view("ingest-load_main"))
    expect_true(in_view("ingest-confirm_main"))
    expect_true(in_view("ingest-status_chip"))
    # The sidebar of the Ingest tab is always open (bslib open = "always"):
    # its collapse toggle is rendered hidden (display none) and the layout is
    # not collapsible, so the way in cannot be hidden.
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
    expect_false(app$get_js(
      "document.querySelector(\".tab-pane[data-value='ingest'] .bslib-sidebar-layout\").classList.contains('sidebar-collapsed')"
    ))
    expect_true(in_view("ingest-load_main"))
    expect_true(in_view("ingest-confirm_main"))
    expect_true(in_view("ingest-status_chip"))
    expect_match(app$get_html("#ingest-status_chip"), "No data loaded", fixed = TRUE)
  })
})

test_that("pagination controls are shown only under a table with more than one page", {
  skip_if_no_browser()

  compat_with_app(load_example = TRUE, function(app) {
    # With scrollX the header is cloned into .dataTables_scrollHead ahead of
    # the table, so the table the API is asked about is taken from the scroll
    # body when there is one.
    pagination <- function(id) {
      app$get_js(sprintf(
        "(function(){var w=document.getElementById('%s'); if(!w) return {present:false};
          var p=w.querySelector('.dataTables_paginate, .dt-paging'); if(!p) return {present:false};
          var t=w.querySelector('.dataTables_scrollBody table.dataTable') || w.querySelector('table.dataTable');
          var api=jQuery(t).DataTable();
          return {present:true, shown: getComputedStyle(p).display !== 'none' && !!p.offsetParent, pages: api.page.info().pages}})()",
        id
      ))
    }
    # Nine items on one page of up to a hundred: no controls.
    grid <- pagination("ingest-type_grid")
    expect_true(grid$present)
    expect_identical(as.integer(grid$pages), 1L)
    expect_false(grid$shown)

    siever_open_tab(app, "winnow")
    pool <- pagination("winnow-pool")
    expect_true(pool$present)
    expect_identical(as.integer(pool$pages), 1L)
    expect_false(pool$shown)

    # The CFA parameter table runs past one page of twenty-five rows, and its
    # controls stay.
    siever_open_tab(app, "cfa", timeout = 300 * 1000)
    # A current fit is stated in the method line above the tabs.
    app$wait_for_js("document.querySelector('#cfa-method_line').textContent.includes(', current.')",
      timeout = 120 * 1000)
    compat_open_subtab(app, "cfa", "Path diagram")
    params <- pagination("cfa-diagram_params")
    expect_true(params$present)
    expect_gt(as.integer(params$pages), 1L)
    expect_true(params$shown)
    expect_identical(compat_output_errors(app), character(0))
  })
})

test_that("at a narrow width the tabs fold behind a toggler, stay reachable, and the active one is told from the rest", {
  skip_if_no_browser()

  # 800 px is below the breakpoint at which bslib collapses the navbar.
  # The example is loaded so that the empirical basis can be applied and the
  # Optimizer's link is among the tabs.
  compat_with_app(width = 800L, height = 900L, load_example = TRUE, function(app) {
    siever_use_empirical_basis(app)
    visible_links <- function() {
      as.integer(app$get_js(
        "Array.from(document.querySelectorAll('nav.navbar .navbar-nav .nav-link')).filter(function(a){return a.offsetParent}).length"
      ))
    }
    toggler <- "nav.navbar .navbar-toggle, nav.navbar .navbar-toggler"
    expect_true(app$get_js(sprintf(
      "(function(){var t=document.querySelector(%s); return !!t && getComputedStyle(t).display !== 'none' && !!t.offsetParent})()",
      compat_js_string(toggler)
    )))
    expect_identical(visible_links(), 0L)

    app$click(selector = toggler)
    expect_gte(visible_links(), length(sieve_workspaces()))
    col <- compat_navbar_colors(app)
    # The open menu sits on the header color, so the stated ratios hold there.
    menu_bg <- app$get_js(paste0(
      "(function(){var e=document.querySelector('nav.navbar .navbar-collapse'); while(e){var b=getComputedStyle(e).backgroundColor;",
      " if(b && b!=='rgba(0, 0, 0, 0)' && b!=='transparent') return b; e=e.parentElement;} return 'none'})()"
    ))
    expect_identical(compat_parse_color(menu_bg), compat_parse_color(col$background))
    active <- compat_contrast(col$active, menu_bg)
    rest <- compat_contrast(col$rest, menu_bg)
    expect_gte(active, 4.5)
    expect_gte(rest, 4.5)
    expect_gt(active, rest)
    # Told apart by color and by weight.
    expect_false(identical(compat_parse_color(col$active), compat_parse_color(col$rest)))
    weights <- app$get_js(paste0(
      "(function(){var a=document.querySelector('nav.navbar .navbar-nav .nav-link.active'),",
      " r=document.querySelector('nav.navbar .navbar-nav .nav-link:not(.active)');",
      " return {active: getComputedStyle(a).fontWeight, rest: getComputedStyle(r).fontWeight}})()"
    ))
    expect_gt(as.integer(weights$active), as.integer(weights$rest))

    # A tab chosen from the open menu is reached.
    expect_identical(siever_open_tab(app, "screen"), "screen")
    expect_identical(app$get_value(input = "nav"), "study")
    if (visible_links() == 0L) {
      app$click(selector = toggler)
      siever_wait_render(app)
    }
    expect_identical(siever_open_tab(app, "repro"), "repro")
    expect_identical(compat_output_errors(app), character(0))
  })
})

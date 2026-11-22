# Helpers for the compatibility and interface tests (test-interface-compat.R
# and test-shinytest2-interface.R). Functions only: nothing here runs at load.
#
# Author: Ken Kelley

# ---- the source tree, fresh processes, installed copies ------------------

# The package source tree the tests are running from, or NULL when they run
# against an installed package (R CMD check), where there is no source tree
# beside the tests.
compat_source_root <- function() {
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  desc <- file.path(root, "DESCRIPTION")
  if (!file.exists(desc) || !dir.exists(file.path(root, "R"))) {
    return(NULL)
  }
  if (!identical(unname(read.dcf(desc, fields = "Package")[1, 1]), "SIEVER")) {
    return(NULL)
  }
  root
}

# The package names in a DESCRIPTION dependency field, without version bounds.
compat_field <- function(root, field) {
  x <- unname(read.dcf(file.path(root, "DESCRIPTION"), fields = field)[1, 1])
  if (is.na(x)) {
    return(character(0))
  }
  x <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  sub("[[:space:]]*\\(.*$", "", x[nzchar(x)])
}

# Run R code in a new R process whose library path is exactly `libs`, and
# return what it printed. A new process is the point: the version helpers read
# the namespace that is loaded, so the question has to be asked where nothing
# is loaded yet.
compat_fresh_r <- function(code, libs) {
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(sprintf(".libPaths(%s)", paste(deparse(libs), collapse = "")), code), script)
  suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)),
    stdout = TRUE, stderr = TRUE
  ))
}

# Install a copy of the source tree into a new library and return the library.
# `version` rewrites the Version field in the copy only; the source tree is
# never touched. The copy and the library are removed when `env` exits,
# unless `lib` names a library of the caller's, which is then left in place.
# `byte_compile = FALSE` installs faster and runs the same code.
compat_install_copy <- function(root, version = NULL, env = parent.frame(), lib = NULL,
                                byte_compile = TRUE) {
  work <- withr::local_tempdir(.local_envir = env)
  package <- unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "Package")[1L, 1L])
  src <- file.path(work, package)
  if (is.null(lib)) lib <- file.path(work, "library")
  dir.create(src)
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  # data/ is kept so that a dependency shipping datasets (DMAR, whose
  # Holzinger and Swineford data feed the bundled example) is complete.
  keep <- intersect(
    c("DESCRIPTION", "NAMESPACE", "LICENSE", "R", "data", "inst", "man"),
    list.files(root)
  )
  file.copy(file.path(root, keep), src, recursive = TRUE)
  if (!is.null(version)) {
    d <- read.dcf(file.path(src, "DESCRIPTION"))
    d[1, "Version"] <- version
    write.dcf(d, file.path(src, "DESCRIPTION"))
  }
  log <- suppressWarnings(system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD", "INSTALL", "--no-docs", "--no-html", "--no-multiarch",
      if (!byte_compile) "--no-byte-compile",
      paste0("--library=", shQuote(lib)), shQuote(src)
    ),
    stdout = TRUE, stderr = TRUE,
    env = paste0("R_LIBS=", paste(unique(c(lib, .libPaths())), collapse = .Platform$path.sep))
  ))
  if (!dir.exists(file.path(lib, package))) {
    stop("R CMD INSTALL of the ", package, " copy failed:\n", paste(log, collapse = "\n"))
  }
  lib
}

# The library path a child R process needs to run the package under test.
# A recorded session script calls SIEVER:: functions and checks the SIEVER
# version before its first step, so a child process must load the code being
# tested, never whatever SIEVER happens to be installed on the machine. From
# a source tree, a copy of the tree is installed once into a library under
# this R session's temporary directory (removed when the session ends) and
# put first on the path; the copy is reinstalled when a file in R/, data/,
# inst/, DESCRIPTION or NAMESPACE changes. Under R CMD check there is no source
# tree, the installed package is the package under test, and the library
# path is returned unchanged.
compat_source_libs <- function() {
  root <- compat_source_root()
  if (is.null(root)) {
    return(.libPaths())
  }
  files <- c(
    file.path(root, c("DESCRIPTION", "NAMESPACE")),
    list.files(file.path(root, c("R", "data", "inst")), recursive = TRUE, full.names = TRUE)
  )
  stamp <- unname(tools::md5sum(files))
  # Fresh-process replay must use the same dependency source as this test run.
  dmar <- if (isNamespaceLoaded("DMAR")) getNamespaceInfo(getNamespace("DMAR"), "path") else NULL
  dmar_source <- !is.null(dmar) && dir.exists(file.path(dmar, "R")) &&
    !file.exists(file.path(dmar, "Meta", "package.rds"))
  if (dmar_source) {
    dependency_files <- c(file.path(dmar, c("DESCRIPTION", "NAMESPACE")),
      list.files(file.path(dmar, c("R", "data")), recursive = TRUE, full.names = TRUE))
    stamp <- c(stamp, dmar, unname(tools::md5sum(dependency_files)))
  }
  key <- substr(compat_hash(c(root, basename(files), stamp)), 1L, 16L)
  lib <- file.path(tempdir(), paste0("siever-source-under-test-", key))
  if (!dir.exists(file.path(lib, "SIEVER", "Meta"))) {
    unlink(lib, recursive = TRUE)
    if (dmar_source) compat_install_copy(dmar, env = environment(), lib = lib, byte_compile = FALSE)
    compat_install_copy(root, env = environment(), lib = lib, byte_compile = FALSE)
  }
  unique(c(lib, .libPaths()))
}

# A short content hash from base R: the MD5 of the values written to a file.
compat_hash <- function(x) {
  f <- tempfile()
  on.exit(unlink(f), add = TRUE)
  writeLines(as.character(x), f, useBytes = TRUE)
  unname(tools::md5sum(f))
}

# A library holding a copy of the installed DT with one bundled DataTables
# extension removed: what a repackaged DT without that extension looks like to
# the app. The library is removed when `env` exits.
compat_dt_without <- function(extension, env = parent.frame()) {
  lib <- withr::local_tempdir(.local_envir = env)
  file.copy(find.package("DT"), lib, recursive = TRUE)
  gone <- file.path(lib, "DT", "htmlwidgets", "lib", "datatables-extensions", extension)
  if (!dir.exists(gone)) {
    stop("the installed DT has no ", extension, " extension to remove")
  }
  unlink(gone, recursive = TRUE)
  lib
}

# ---- color arithmetic ----------------------------------------------------

# WCAG 2 contrast ratio, written from the definition (relative luminance of
# linearized sRGB) so the check does not lean on any color package or on the
# app. `fg` may carry an alpha, and the element showing it an opacity; both
# are composited over `bg` before the ratio is taken.
compat_parse_color <- function(x) {
  x <- trimws(x)
  if (grepl("^#[0-9a-fA-F]{6}$", x)) {
    return(c(as.numeric(grDevices::col2rgb(x)[, 1]), 1))
  }
  m <- regmatches(x, regexec("^rgba?\\(([^)]*)\\)$", x))[[1]]
  if (length(m) != 2L) {
    stop("cannot parse the color '", x, "'")
  }
  v <- as.numeric(trimws(strsplit(m[2], "[,/ ]+")[[1]]))
  if (length(v) == 3L) v <- c(v, 1)
  v
}
compat_luminance <- function(rgb) {
  c01 <- rgb / 255
  lin <- ifelse(c01 <= 0.04045, c01 / 12.92, ((c01 + 0.055) / 1.055)^2.4)
  sum(c(0.2126, 0.7152, 0.0722) * lin)
}
compat_contrast <- function(fg, bg, opacity = 1) {
  f <- compat_parse_color(fg)
  b <- compat_parse_color(bg)[1:3]
  a <- f[4] * opacity
  shown <- a * f[1:3] + (1 - a) * b
  l <- sort(c(compat_luminance(shown), compat_luminance(b)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}

# ---- the stylesheet and a loaded state ------------------------------------

compat_css <- function() {
  paste(readLines(SIEVER:::app_sys("app", "www", "custom.css"), warn = FALSE), collapse = "\n")
}
compat_css_var <- function(css, name) {
  m <- regmatches(css, regexec(paste0(name, ":[[:space:]]*([^;]+);"), css))[[1]]
  if (length(m) != 2L) {
    stop("custom.css does not define ", name)
  }
  trimws(m[2])
}

# A loaded, confirmed state on the primary nine example.
compat_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = TRUE, name = fx$name)
  s
}

# ---- driving the app in a browser -----------------------------------------

# Run fun(app) against a freshly started app through siever_with_app():
# `app` picks the launcher under tests/testthat/apps, and `libs` puts extra
# libraries first on the library path of the app process only (the test
# process keeps its own). The window is never resized after it opens: several
# assertions are about what is drawn without a resize.
#
# Starting the app is the one step that depends on how busy the machine is:
# opening the page is a single chromote command with a ten second budget, and
# under heavy load that command can time out before any test has run. The
# commands of this session get a minute instead, and an app that could not be
# started at all is started once more. Neither touches an assertion: a test
# that reaches the app still passes or fails on what the app shows.
compat_with_app <- function(fun, load_example = FALSE, app = "siever", libs = NULL,
                            width = 1600, height = 1000) {
  siever_with_app(fun, load_example = load_example, app = app, libs = libs,
    width = width, height = height)
}

# A JavaScript string literal for an R string (a CSS selector, in practice).
compat_js_string <- function(x) {
  paste0("\"", gsub("([\"\\\\])", "\\\\\\1", x), "\"")
}

# What AppDriver$get_js() returns for a JavaScript array of strings, as a
# character vector (an empty array comes back as an empty list).
compat_js_strings <- function(x) {
  if (!length(x)) character(0) else as.character(unlist(x))
}

# The sub-tab values of a top level tab, in order.
compat_subtabs <- function(app, tab) {
  compat_js_strings(app$get_js(sprintf(
    "Array.from(document.querySelectorAll(%s)).map(function(a){return a.getAttribute('data-value')})",
    compat_js_string(sprintf(".tab-pane[data-value='%s'] ul.nav a.nav-link", tab))
  )))
}

# Open a sub-tab of a top level tab and wait for it to settle.
compat_open_subtab <- function(app, tab, subtab, timeout = 300 * 1000) {
  sel <- sprintf(".tab-pane[data-value='%s'] ul.nav a.nav-link[data-value=\"%s\"]", tab, subtab)
  app$click(selector = sel)
  siever_wait_render(app, timeout = timeout)
  invisible(NULL)
}

# Every DataTable currently on screen, measured where it stands.
#
#   width, box_w   rendered width of the table and of its widget box
#   box_h, table_h height of the widget box and of what DataTables drew in it
#   spill          how far the drawn table runs past the bottom of its box
#   rows, visible  body rows, and how many of them can be seen once the table
#                  is scrolled to the top of its sub-tab (inside every clipping
#                  ancestor and the window)
#   zero_cols      header cells narrower than 2 px
#   misalign       largest header to body column width difference, in px
#   region_h       height of the scrolling body of the sub-tab (-1 if none)
#   squeezed       containers between the table and that body that clip their
#                  own content and are much shorter than the sub-tab (more
#                  than `slack` px shorter): squeezed card bodies.
#                  A single card that fills its sub-tab and scrolls inside is
#                  shorter only by its own header and padding (74 px) and is
#                  not counted.
compat_measure_tables <- function(app, slack = 120) {
  js <- sprintf("
  (function(){
    var slack = %d;
    function clips(el){ var cs = getComputedStyle(el); return cs.overflowY !== 'visible' && (el.scrollHeight - el.clientHeight) > 1; }
    var out = [];
    document.querySelectorAll('div.datatables').forEach(function(w){
      if (!w.offsetParent) return;
      var body = w.querySelector('.dataTables_scrollBody table') || w.querySelector('table.dataTable');
      if (!body) return;
      var head = w.querySelector('.dataTables_scrollHead table') || body;
      var pane = w.closest('.tab-pane');
      var region = pane ? pane.querySelector(':scope > .card-body') : null;
      var squeezed = [], a = w.parentElement;
      while (region && a && a !== region && a !== document.body) {
        if (clips(a) && a.clientHeight < region.clientHeight - slack) squeezed.push(String(a.className).split(' ').slice(0, 2).join('.') + ' ' + a.clientHeight + '/' + a.scrollHeight);
        a = a.parentElement;
      }
      w.scrollIntoView({block: 'start'});
      var top = 0, bottom = window.innerHeight; a = w.parentElement;
      while (a && a !== document.documentElement) {
        if (getComputedStyle(a).overflowY !== 'visible') {
          var r = a.getBoundingClientRect();
          top = Math.max(top, r.top + a.clientTop);
          bottom = Math.min(bottom, r.top + a.clientTop + a.clientHeight);
        }
        a = a.parentElement;
      }
      var rows = Array.from(body.querySelectorAll('tbody tr'));
      var visible = rows.filter(function(r){ var b = r.getBoundingClientRect(); return b.height > 0 && b.width > 0 && b.top >= top - 1 && b.bottom <= bottom + 1; }).length;
      var ths = Array.from(head.querySelectorAll('thead th'));
      var tds = rows.length ? Array.from(rows[rows.length - 1].querySelectorAll('td')) : [];
      var mis = -1;
      if (ths.length && tds.length === ths.length) { mis = 0; ths.forEach(function(t, i){ mis = Math.max(mis, Math.abs(t.getBoundingClientRect().width - tds[i].getBoundingClientRect().width)); }); }
      var wr = w.getBoundingClientRect(), inner = w.querySelector('.dataTables_wrapper') || body, ir = inner.getBoundingClientRect();
      out.push({id: w.id, width: Math.round(body.getBoundingClientRect().width), box_w: Math.round(wr.width),
        box_h: Math.round(wr.height), table_h: Math.round(ir.height), spill: Math.round(ir.bottom - wr.bottom),
        rows: rows.length, visible: visible, cols: ths.length,
        zero_cols: ths.filter(function(t){ return t.getBoundingClientRect().width < 2; }).length,
        misalign: Math.round(mis * 10) / 10, region_h: region ? region.clientHeight : -1, squeezed: squeezed.join(' ; ')});
    });
    return out;
  })()", as.integer(slack))
  res <- app$get_js(js)
  if (!length(res)) {
    return(data.frame(id = character(0), stringsAsFactors = FALSE))
  }
  res <- do.call(rbind, lapply(res, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
  whole <- c(
    "width", "box_w", "box_h", "table_h", "spill", "rows", "visible", "cols",
    "zero_cols", "region_h"
  )
  res[whole] <- lapply(res[whole], as.integer)
  res$misalign <- as.numeric(res$misalign)
  res
}

# Containers inside the body of the sub-tab now showing that clip their own
# content and are much shorter than the sub-tab itself (more than `slack` px
# shorter): a nested scroll area a few lines tall. Code listings, text areas,
# and the scrolling parts of a DataTable are meant to scroll and are not
# counted; neither is a single card that fills its sub-tab.
compat_squeezed <- function(app, slack = 120) {
  js <- sprintf("
  (function(){
    var slack = %d, out = [];
    document.querySelectorAll('.tab-pane.active .tab-pane.active > .card-body').forEach(function(region){
      if (!region.offsetParent) return;
      region.querySelectorAll('*').forEach(function(el){
        var cs = getComputedStyle(el);
        if (cs.overflowY !== 'visible' && (el.scrollHeight - el.clientHeight) > 1 && el.clientHeight > 0 &&
            el.clientHeight < region.clientHeight - slack &&
            !el.closest('.dataTables_wrapper') && ['PRE', 'TEXTAREA', 'SELECT'].indexOf(el.tagName) < 0)
          out.push((el.id ? '#' + el.id + ' ' : '') + String(el.className).slice(0, 50) + ' ' + el.clientHeight + '/' + el.scrollHeight);
      });
    });
    return out;
  })()", as.integer(slack))
  compat_js_strings(app$get_js(js))
}

# Height of the scrolling body of the sub-tab now showing, in px.
compat_region_height <- function(app) {
  as.integer(app$get_js(paste0(
    "(function(){var r = Array.from(document.querySelectorAll(",
    "'.tab-pane.active .tab-pane.active > .card-body')).find(function(x){return x.offsetParent});",
    " return r ? r.clientHeight : -1;})()"
  )))
}

# The ids and messages of outputs showing a Shiny error (validation messages
# excluded), so a failure names the output and its owner.
compat_output_errors <- function(app) {
  compat_js_strings(app$get_js(paste0(
    "Array.from(document.querySelectorAll(",
    "'.shiny-output-error:not(.shiny-output-error-validation)')).map(function(e){",
    "return e.id + ': ' + e.textContent.trim().slice(0, 200)})"
  )))
}

# Computed colors of the header and of its text in each resting state.
compat_navbar_colors <- function(app) {
  js <- "
  (function(){
    function cs(el, p){ return el ? getComputedStyle(el)[p] : null; }
    var nav = document.querySelector('nav.navbar');
    var links = Array.from(document.querySelectorAll('nav.navbar .navbar-nav .nav-link'));
    var small = document.querySelector('nav.navbar .navbar-brand small');
    return {
      background: cs(nav, 'backgroundColor'),
      brand: cs(document.querySelector('nav.navbar .navbar-brand'), 'color'),
      subtitle: cs(small, 'color'), subtitle_opacity: small ? parseFloat(cs(small, 'opacity')) : null,
      active: cs(links.find(function(a){ return a.classList.contains('active'); }), 'color'),
      rest: cs(links.find(function(a){ return !a.classList.contains('active'); }), 'color'),
      n_links: links.length,
      n_distinct_rest: Array.from(new Set(links.filter(function(a){ return !a.classList.contains('active'); }).map(function(a){ return cs(a, 'color'); }))).length
    };
  })()"
  col <- app$get_js(js)
  col$n_links <- as.integer(col$n_links)
  col$n_distinct_rest <- as.integer(col$n_distinct_rest)
  col
}

# Put the mouse over an element (a real pointer move, not a forced style) and
# return its computed color once the color transition has finished.
compat_hover_color <- function(app, selector) {
  sel <- compat_js_string(selector)
  pos <- app$get_js(sprintf(
    "(function(){var r = document.querySelector(%s).getBoundingClientRect(); return {x: r.left + r.width / 2, y: r.top + r.height / 2};})()",
    sel
  ))
  app$get_chromote_session()$Input$dispatchMouseEvent(type = "mouseMoved", x = pos$x, y = pos$y)
  siever_wait_render(app)
  app$get_js(sprintf(
    "(function(){var el = document.querySelector(%s); return {hovered: el.matches(':hover'), color: getComputedStyle(el).color};})()",
    sel
  ))
}

# ---- replaying the recorded script in a clean process ----------------------

# Drive the real module servers through one shared state store, as the app
# does: load the bundled nine tests (every item is continuous, so the types
# confirm on load), reverse one item from the Ingest grid (which withdraws the
# confirmation), confirm again from the action bar, exclude one item (from the
# Items tab, or from the Ingest grid's own Use cell), and fit the CFA. Returns
# the state, the CFA the session fitted, and the script the Reproducible Code
# tab shows for that session. The defaults reverse t2_cubes and exclude
# t10_addition: with that item out, the correlated three factor model on the
# remaining eight is admissible, so the comparison is between two proper
# solutions (excluding a Speed item other than t10 leaves a negative residual
# variance).
compat_replay_session <- function(exclude_from = c("items", "ingest"),
                                  reverse = "t2_cubes",
                                  exclude = "t10_addition") {
  exclude_from <- match.arg(exclude_from)
  st <- sieve_new_state()
  got <- new.env()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1)
    stopifnot(isTRUE(shiny::isolate(state$confirmed)))
    session$setInputs(cell_edit = list(item = reverse, field = "reverse", value = "Yes"))
    stopifnot(isTRUE(shiny::isolate(state$meta$reverse[state$meta$item == reverse])))
    if (identical(exclude_from, "ingest")) {
      session$setInputs(cell_edit = list(item = exclude, field = "status", value = "Dropped"))
    }
    session$setInputs(confirm_main = 1)
    stopifnot(isTRUE(shiny::isolate(state$confirmed)))
  })
  if (identical(exclude_from, "items")) {
    shiny::testServer(mod_winnow_server, args = list(state = st), {
      session$setInputs(
        coef = "omega", factor_cov = "correlated",
        load_low = .4, salient = .3, communality_low = .3,
        item_total_low = .3, redundancy_high = .8
      )
      session$setInputs(cell_edit = list(item = exclude, field = "status", value = "Excluded"))
    })
  }
  stopifnot(identical(shiny::isolate(st$meta$status[st$meta$item == exclude]), "parked"))
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    # The Fit the model button runs the fit and records its R.
    session$setInputs(fit_go = 1)
    invisible(output$fit_tbl)
    got$fit <- fit()
  })
  shiny::testServer(mod_repro_server, args = list(state = st), {
    got$script <- output$code
  })
  list(
    state = st, fit = got$fit, script = got$script,
    log = shiny::isolate(st$codelog)
  )
}

# Run a recorded script in a new R process (Rscript --vanilla, with the
# source under test installed first on the library path; see
# compat_source_libs()), one recorded step at a time, and report the first
# step that fails. The steps are the blocks the "# [n]" stamps of sieve_log()
# delimit; the header before the first stamp is run first. After every step
# the class of `items` is noted, since the recorded steps bind that name
# both to the item names and to the item data. When every step ran, the
# coefficients of each fit named in `fits` are returned as the process
# printed them, at full precision: `coefs`, a list by fit, and `coef`, that
# of `cfa_fit` when it is named. Each object named in `values` (a DMAR
# reliability table of term and value, such as omega_Visual, H_Visual or
# alpha_Visual from the Reliability tab's steps) is returned the same way:
# `values`, a list by object of its values named by term, and `aux`, the
# auxiliary variables DMAR records the object was computed with.
compat_replay_run <- function(script, libs = compat_source_libs(), state = NULL, fits = "cfa_fit",
                              values = character(0)) {
  directory <- tempfile("siever_replay_bundle_")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  script_file <- file.path(directory, "siever_session.R")
  runner <- tempfile("siever_replay_runner_", fileext = ".R")
  on.exit(unlink(c(script_file, runner)), add = TRUE)
  if (is.null(state)) writeLines(script, script_file) else sieve_session_write(state, directory)
  writeLines(c(
    sprintf(".libPaths(%s)", paste(deparse(libs), collapse = "")),
    sprintf("setwd(%s)", deparse(directory)),
    sprintf("lines <- readLines(%s)", deparse(script_file)),
    "stamps <- which(startsWith(lines, '# ['))",
    "starts <- c(1L, stamps); ends <- c(stamps - 1L, length(lines))",
    "for (i in seq_along(starts)) {",
    "  if (ends[i] < starts[i]) next",
    "  label <- if (i == 1L) '(header)' else lines[starts[i]]",
    "  ok <- tryCatch({",
    "    eval(parse(text = lines[starts[i]:ends[i]]), envir = globalenv())",
    "    TRUE",
    "  }, error = function(e) {",
    "    cat('STEP_FAILED ', label, ' :: ', conditionMessage(e), '\\n', sep = '')",
    "    FALSE",
    "  })",
    "  if (!ok) quit(status = 1)",
    "  what <- if (exists('items', envir = globalenv())) paste(class(get('items', envir = globalenv())), collapse = '/') else '(unbound)'",
    "  cat('STEP_OK ', label, ' :: items is ', what, '\\n', sep = '')",
    "}",
    sprintf("for (fit_name in %s) {", paste(deparse(as.character(fits)), collapse = "")),
    "  if (!exists(fit_name, envir = globalenv())) { cat('STEP_FAILED (end) :: the script defines no ', fit_name, '\\n', sep = ''); quit(status = 1) }",
    "  co <- lavaan::coef(get(fit_name, envir = globalenv()))",
    # One coefficient per line, the fit, the name and the value separated by
    # tabs: lavaan's parameter names hold '=~', '~~', ':=' and '|', so no
    # symbol separator is safe and a line format is used instead.
    "  for (i in seq_along(co)) cat('COEF\\t', fit_name, '\\t', names(co)[i], '\\t', sprintf('%.17g', co[i]), '\\n', sep = '')",
    "}",
    sprintf("for (value_name in %s) {", paste(deparse(as.character(values)), collapse = "")),
    "  if (!exists(value_name, envir = globalenv())) { cat('STEP_FAILED (end) :: the script defines no ', value_name, '\\n', sep = ''); quit(status = 1) }",
    "  x <- as.data.frame(get(value_name, envir = globalenv()))",
    "  for (i in seq_len(nrow(x))) cat('VAL\\t', value_name, '\\t', x$term[i], '\\t', sprintf('%.17g', x$value[i]), '\\n', sep = '')",
    "  cat('AUX\\t', value_name, '\\t', paste(attr(get(value_name, envir = globalenv()), 'aux'), collapse = ','), '\\n', sep = '')",
    "}"
  ), runner)
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(runner)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status")
  status <- if (is.null(status)) 0L else as.integer(status)
  failed <- grep("^STEP_FAILED ", out, value = TRUE)
  failed_step <- if (length(failed)) sub("^STEP_FAILED (.*) :: .*$", "\\1", failed[1]) else NA_character_
  error <- if (length(failed)) sub("^STEP_FAILED .* :: ", "", failed[1]) else NA_character_
  ok_lines <- grep("^STEP_OK ", out, value = TRUE)
  steps <- data.frame(
    step = sub("^STEP_OK (.*) :: items is .*$", "\\1", ok_lines),
    items_class = sub("^STEP_OK .* :: items is ", "", ok_lines),
    stringsAsFactors = FALSE
  )
  coef_lines <- grep("^COEF\t", out, value = TRUE)
  coefs <- list()
  if (length(coef_lines)) {
    parts <- strsplit(sub("^COEF\t", "", coef_lines), "\t", fixed = TRUE)
    fit_of <- vapply(parts, `[`, "", 1L)
    coef_values <- stats::setNames(as.numeric(vapply(parts, `[`, "", 3L)), vapply(parts, `[`, "", 2L))
    coefs <- lapply(stats::setNames(unique(fit_of), unique(fit_of)), function(f) coef_values[fit_of == f])
  }
  value_lines <- grep("^VAL\t", out, value = TRUE)
  tables <- list()
  if (length(value_lines)) {
    parts <- strsplit(sub("^VAL\t", "", value_lines), "\t", fixed = TRUE)
    object_of <- vapply(parts, `[`, "", 1L)
    numbers <- stats::setNames(as.numeric(vapply(parts, `[`, "", 3L)), vapply(parts, `[`, "", 2L))
    tables <- lapply(stats::setNames(unique(object_of), unique(object_of)), function(o) numbers[object_of == o])
  }
  aux_lines <- strsplit(sub("^AUX\t", "", grep("^AUX\t", out, value = TRUE)), "\t", fixed = TRUE)
  aux <- stats::setNames(
    lapply(aux_lines, function(p) if (length(p) > 1L) strsplit(p[2L], ",", fixed = TRUE)[[1L]] else character(0)),
    vapply(aux_lines, `[`, "", 1L)
  )
  list(
    status = status, failed_step = failed_step, error = error, steps = steps,
    coef = coefs[["cfa_fit"]], coefs = coefs, values = tables, aux = aux, output = out
  )
}

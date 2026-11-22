# Compatibility and interface presentation: the displayed version, the item
# grid, the header, tables below the fold, and declared and used packages.
#
# Everything here runs without a browser. The matching browser checks, which
# check the same behavior as a browser shows it, are in
# test-shinytest2-interface.R. The two files are deliberately redundant where
# they can be: a machine with no Chrome still gets the arithmetic on the
# declared header colors, the item grid's fallback, and the version contract.
# The compat_* helpers both files use are in helper-interface.R.
#
# Author: Ken Kelley

skip_if_not_installed("DT")
skip_if_not_installed("shiny")

# ---- The displayed version ----------------------------------------------

test_that("the version label names the running code and how it was loaded", {
  root <- compat_source_root()
  dev <- requireNamespace("pkgload", quietly = TRUE) && pkgload::is_dev_package("SIEVER")
  expected <- if (dev) {
    # Loaded from source: the source DESCRIPTION is the truth, whatever any
    # library on the path may hold.
    skip_if(is.null(root), "the source tree is not beside the tests")
    unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[1, 1])
  } else {
    unname(read.dcf(system.file("DESCRIPTION", package = "SIEVER"), fields = "Version")[1, 1])
  }
  expect_identical(SIEVER:::sieve_version(), expected)
  expect_identical(SIEVER:::sieve_is_dev_source(), dev)
  expect_identical(
    SIEVER:::sieve_version_label(),
    sprintf("%s (%s)", expected, if (dev) "development source" else "installed package")
  )
})

test_that("an installed copy reports its own version and says it is installed", {
  skip_on_cran()
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  source_version <- unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[1, 1])

  lib <- compat_install_copy(root)
  out <- compat_fresh_r(
    c(
      "suppressMessages(library(SIEVER))",
      "cat('VERSION=', SIEVER:::sieve_version(), '\\n', sep = '')",
      "cat('DEV=', SIEVER:::sieve_is_dev_source(), '\\n', sep = '')",
      "cat('LABEL=', SIEVER:::sieve_version_label(), '\\n', sep = '')",
      "cat('FROM=', normalizePath(dirname(find.package('SIEVER'))), '\\n', sep = '')"
    ),
    libs = c(lib, .libPaths())
  )
  expect_true(paste0("VERSION=", source_version) %in% out, info = paste(out, collapse = "\n"))
  expect_true("DEV=FALSE" %in% out)
  expect_true(sprintf("LABEL=%s (installed package)", source_version) %in% out)
  # The process really did use the copy installed a moment ago.
  expect_true(paste0("FROM=", normalizePath(lib)) %in% out)
})

test_that("a development checkout reports its own version when a different one is installed", {
  skip_on_cran()
  skip_if_not_installed("pkgload")
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  source_version <- unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[1, 1])
  expect_false(identical(source_version, "0.0.1"))

  # A library, first on the path, that holds a SIEVER labeled 0.0.1.
  lib <- compat_install_copy(root, version = "0.0.1")
  out <- compat_fresh_r(
    c(
      "cat('INSTALLED=', as.character(utils::packageVersion('SIEVER')), '\\n', sep = '')",
      sprintf("suppressMessages(pkgload::load_all(%s, quiet = TRUE, helpers = FALSE))", deparse(root)),
      "cat('VERSION=', SIEVER:::sieve_version(), '\\n', sep = '')",
      "cat('DEV=', SIEVER:::sieve_is_dev_source(), '\\n', sep = '')",
      "cat('LABEL=', SIEVER:::sieve_version_label(), '\\n', sep = '')"
    ),
    libs = c(lib, .libPaths())
  )
  # The trap is real: the library answers 0.0.1 ...
  expect_true("INSTALLED=0.0.1" %in% out, info = paste(out, collapse = "\n"))
  # ... and the running development source does not repeat it.
  expect_true(paste0("VERSION=", source_version) %in% out)
  expect_true("DEV=TRUE" %in% out)
  expect_true(sprintf("LABEL=%s (development source)", source_version) %in% out)
})

test_that("the About dialog shows the version label", {
  shown <- new.env()
  testthat::local_mocked_bindings(
    showModal = function(ui, session = NULL) assign("ui", ui, envir = shown),
    .package = "SIEVER"
  )
  shiny::testServer(SIEVER:::app_server, {
    session$setInputs(show_about = 0.5)
  })
  expect_true(exists("ui", envir = shown))
  text <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", "", paste(as.character(shown$ui), collapse = " ")))
  expect_match(text, sprintf("SIEVER version %s.", SIEVER:::sieve_version_label()), fixed = TRUE)
  expect_match(text, "(development source|installed package)")
})

test_that("the About dialog lists the versions this session runs on, and the citation", {
  v <- SIEVER:::sieve_about_versions()
  expect_identical(v$component, c("R", "DMAR", "lavaan", "psych", "shiny", "bslib", "DT"))
  expect_identical(v$version[1], paste(R.version$major, R.version$minor, sep = "."))
  expect_identical(v$status[1], "running")
  expect_true(all(v$status[-1] %in% c("loaded", "installed, not loaded", "not installed")))
  # A loaded package reports the version of the namespace that is loaded.
  expect_true(isNamespaceLoaded("shiny"))
  expect_identical(v$version[v$component == "shiny"], as.character(getNamespaceVersion("shiny")))
  expect_identical(v$status[v$component == "shiny"], "loaded")
  # Asking does not load anything.
  before <- loadedNamespaces()
  SIEVER:::sieve_about_versions()
  expect_setequal(loadedNamespaces(), before)
  absent <- SIEVER:::sieve_about_versions("NoSuchPackage0")
  expect_identical(absent$status[2], "not installed")
  expect_identical(absent$version[2], "")

  cite <- SIEVER:::sieve_citation_text()
  expect_match(cite, "Kelley, K. (2026). SIEVER: Scale Item Evaluation and Validation Environment in R.", fixed = TRUE)
  expect_match(cite, sprintf("R package version %s.", SIEVER:::sieve_version()), fixed = TRUE)

  shown <- new.env()
  testthat::local_mocked_bindings(
    showModal = function(ui, session = NULL) assign("ui", ui, envir = shown),
    .package = "SIEVER"
  )
  shiny::testServer(SIEVER:::app_server, {
    session$setInputs(show_about = 0.5)
  })
  html <- paste(as.character(shown$ui), collapse = " ")
  text <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", html))
  expect_true(grepl("<table class=\"table table-sm sieve-about\">", html, fixed = TRUE))
  for (component in c("R", "DMAR", "lavaan", "psych", "shiny", "bslib", "DT")) {
    row <- v[v$component == component, ]
    expect_true(grepl(sprintf("<td>%s</td> <td>%s</td> <td>%s</td>", component, row$version, row$status),
      gsub("[[:space:]]+", " ", html),
      fixed = TRUE
    ), info = component)
  }
  expect_match(text, sprintf("SIEVER %s (development source|installed package)", SIEVER:::sieve_version()))
  expect_match(text, "To cite SIEVER:", fixed = TRUE)
  expect_match(text, "Kelley, K. (2026). SIEVER", fixed = TRUE)
})

# ---- The item grid and the DataTables RowGroup extension ----------------

test_that("the installed DT bundles RowGroup, and the lookup reads the DT library tree", {
  ext_dir <- system.file("htmlwidgets", "lib", "datatables-extensions", package = "DT")
  expect_true(nzchar(ext_dir))
  on_disk <- basename(list.dirs(ext_dir, recursive = FALSE))
  expect_setequal(SIEVER:::sieve_dt_extensions(), on_disk)
  expect_true(SIEVER:::sieve_dt_has_extension("RowGroup"))
  expect_true(file.exists(file.path(ext_dir, "RowGroup", "js", "dataTables.rowGroup.min.js")))
  expect_false(SIEVER:::sieve_dt_has_extension("NoSuchExtension"))
  # What the guard protects against: DT stops on an extension it does not have.
  expect_error(
    DT::datatable(data.frame(a = 1), extensions = "NoSuchExtension"),
    "does not exist"
  )
})

test_that("the row group request follows what the installed DT can do", {
  with_ext <- SIEVER:::sieve_dt_row_group(0L)
  expect_true(with_ext$available)
  expect_identical(with_ext$extensions, "RowGroup")
  expect_identical(with_ext$options, list(rowGroup = list(dataSrc = 0L)))
  expect_null(with_ext$caption)

  testthat::local_mocked_bindings(
    sieve_dt_extensions = function() c("Buttons", "RowReorder"),
    .package = "SIEVER"
  )
  without <- SIEVER:::sieve_dt_row_group(0L)
  expect_false(without$available)
  expect_identical(without$extensions, character(0))
  expect_identical(without$options, list())
  # The message says what is missing, what the user still has, and what to do.
  expect_match(without$caption, "RowGroup", fixed = TRUE)
  expect_match(without$caption, "Group column", fixed = TRUE)
  expect_match(without$caption, "install.packages(\"DT\")", fixed = TRUE)
})

# The grid reaches the browser as the widget's JSON text. The assertions below
# read that text directly (the package declares no JSON parser of its own):
# the keys asked for are literal fragments of what DataTables will receive.
grid_group_column <- function(json) {
  first <- regmatches(json, regexec("\"data\":\\[\\[([^]]*)\\]", json))[[1]][2]
  gsub("\"", "", strsplit(first, "\",\"", fixed = TRUE)[[1]], fixed = TRUE)
}

test_that("the item grid asks for row groups when DT has them", {
  st <- compat_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(palette = "nd")
    json <- as.character(output$type_grid)
    expect_match(json, "\"extensions\":[\"RowGroup\"]", fixed = TRUE)
    expect_match(json, "\"rowGroup\":{\"dataSrc\":0}", fixed = TRUE)
    # The column that carries the group label (column 0) is hidden, and it is
    # the only hidden column.
    expect_match(json, "{\"visible\":false,\"targets\":0}", fixed = TRUE)
    expect_identical(lengths(regmatches(json, gregexpr("\"visible\":false", json, fixed = TRUE))), 1L)
    expect_false(grepl("\"caption\":", json, fixed = TRUE))
    # One row per item, each labeled with its group; the group line says
    # Included.
    expect_identical(grid_group_column(json), rep("Included items", 9L))
  })
})

test_that("without RowGroup the item grid is still drawn, with a clear note", {
  testthat::local_mocked_bindings(
    sieve_dt_extensions = function() c("Buttons", "RowReorder"),
    .package = "SIEVER"
  )
  st <- compat_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(palette = "nd")
    expect_no_error(output$type_grid)
    json <- as.character(output$type_grid)
    expect_false(grepl("RowGroup\"", json, fixed = TRUE))
    expect_false(grepl("\"rowGroup\"", json, fixed = TRUE))
    expect_false(grepl("\"extensions\":", json, fixed = TRUE))
    # Without RowGroup the group label is an ordinary visible column, the
    # grid is still ordered by it, and its header reads Group.
    expect_match(json, "{\"visible\":true,\"targets\":0}", fixed = TRUE)
    expect_false(grepl("\"visible\":false", json, fixed = TRUE))
    expect_match(json, "\"order\":[[0,\"asc\"]]", fixed = TRUE)
    expect_match(json, "<th>Group<\\/th>", fixed = TRUE)
    # The note travels with the table as its caption.
    caption <- regmatches(json, regexec("\"caption\":\"<caption>(.*?)<\\\\/caption>\"", json, perl = TRUE))[[1]][2]
    expect_match(caption, "DataTables RowGroup extension", fixed = TRUE)
    expect_match(caption, "install.packages(", fixed = TRUE)
    # The group line says Included.
    expect_identical(grid_group_column(json), rep("Included items", 9L))
  })
})

# ---- The header ---------------------------------------------------------

test_that("the contrast arithmetic reproduces the published anchors", {
  expect_equal(compat_contrast("#000000", "#ffffff"), 21)
  expect_equal(compat_contrast("#ffffff", "#ffffff"), 1)
  # The familiar boundary pair: #767676 on white passes 4.5 to 1, #777777 does not.
  expect_gt(compat_contrast("#767676", "#ffffff"), 4.5)
  expect_lt(compat_contrast("#777777", "#ffffff"), 4.5)
  # Alpha is composited over the background before the ratio is taken.
  expect_equal(
    compat_contrast("rgba(0, 0, 0, 0.5)", "#ffffff"),
    compat_contrast("rgb(127.5, 127.5, 127.5)", "#ffffff")
  )
})

test_that("the stylesheet states the header color the theme declares", {
  css <- compat_css()
  # The header is the navy of the default palette; the lighter blue is the
  # accent, the theme's primary color and --sieve-accent in the stylesheet.
  chrome <- SIEVER:::sieve_chrome_colors()
  navy <- chrome[["navy"]]
  expect_identical(navy, SIEVER:::sieve_palettes()$nd[1])
  expect_identical(tolower(compat_css_var(css, "--sieve-navbar-bg")), tolower(navy))
  expect_identical(
    unname(bslib::bs_get_variables(SIEVER:::sieve_theme(), "navbar-bg")),
    navy
  )
  expect_identical(tolower(compat_css_var(css, "--sieve-accent")), tolower(chrome[["accent"]]))
  expect_identical(tolower(unname(bslib::bs_get_variables(SIEVER:::sieve_theme(), "primary"))), chrome[["accent"]])
  expect_false("marian" %in% names(chrome))
  # The variable can be withheld, which is how the browser test shows the
  # stylesheet is sufficient by itself.
  expect_true(is.na(bslib::bs_get_variables(SIEVER:::sieve_theme(navbar_bg = NULL), "navbar-bg")))
  # The header rules are explicit: a background on the navbar element, and a
  # color for the title and for each state of a tab.
  expect_match(css, "background-color: var(--sieve-navbar-bg) !important;", fixed = TRUE)
  for (rule in c(
    ".navbar-brand", ".navbar-nav .nav-link", ".navbar-nav .nav-link:hover",
    ".navbar-nav .nav-link.active"
  )) {
    expect_match(css, paste0("nav.navbar.navbar-default ", rule), fixed = TRUE)
  }
})

test_that("every declared header text color has at least 4.5 to 1 contrast", {
  css <- compat_css()
  bg <- compat_css_var(css, "--sieve-navbar-bg")
  ratios <- vapply(
    c(active = "--sieve-navbar-fg", hover = "--sieve-navbar-fg-hover", rest = "--sieve-navbar-fg-rest"),
    function(v) compat_contrast(compat_css_var(css, v), bg), numeric(1)
  )
  expect_true(all(ratios >= 4.5), info = paste(names(ratios), round(ratios, 2), collapse = "; "))
  # White on the navy, from the definition, to four decimals (9.8274 on the
  # lighter blue accent).
  expect_equal(unname(ratios["active"]), 15.7885, tolerance = 1e-4)
  # An optional tab at rest is lighter than a core
  # tab and still above the rule.
  optional <- compat_contrast(compat_css_var(css, "--sieve-navbar-fg-optional"), bg)
  expect_gte(optional, 4.5)
  expect_equal(optional, 5.6722, tolerance = 1e-4)
  expect_lt(optional, ratios[["rest"]])
  # The states are ordered as the eye expects: active strongest, rest weakest.
  expect_true(ratios[["active"]] > ratios[["hover"]] && ratios[["hover"]] > ratios[["rest"]])
})

# ---- Tables below the fold of a squeezed card ---------------------------

test_that("the stylesheet keeps stacked cards and tables at their own height", {
  # The behavior itself is measured in the browser test. This is the tripwire
  # for a machine without a browser: the rules that produce it are there.
  css <- gsub("[[:space:]]+", " ", compat_css())
  expect_match(css, ".card-body.html-fill-container > .html-fill-item:not(:only-child) { flex-shrink: 0; }",
    fixed = TRUE
  )
  expect_match(css, ".card:has(> .card-body ~ .card-body) > .card-body { flex-shrink: 0 !important; }",
    fixed = TRUE
  )
  expect_match(css, ".main > .card.html-fill-item:has(> .tab-content) { min-height: 40rem; }",
    fixed = TRUE
  )
  expect_match(css, ".card-body .datatables.html-fill-item { flex: 0 0 auto; }", fixed = TRUE)
  # The floor that keeps an empty table output from collapsing to zero height
  # (where DT would defer drawing it) is in place.
  expect_match(css, ".card-body .datatables.html-fill-item { min-height: 240px; }", fixed = TRUE)
})

test_that("every class the stylesheet names is one the page bslib builds carries", {
  # The tripwire for a bslib release that renames a class: each class a
  # custom.css selector names must be in the HTML of app_ui(), except the
  # ones DataTables and sieve.js add when a table is drawn, the state
  # classes, and the navbar-inverse alternative (page_navbar() is built
  # with inverse = FALSE, so the page carries navbar-default).
  css <- gsub("(?s)/\\*.*?\\*/", "", compat_css(), perl = TRUE)
  selectors <- regmatches(css, gregexpr("[^{}]+(?=\\{)", css, perl = TRUE))[[1]]
  selectors <- trimws(unlist(strsplit(selectors, ",", fixed = TRUE)))
  classes <- unique(unlist(regmatches(selectors, gregexpr("\\.[A-Za-z_-][A-Za-z0-9_-]*", selectors))))
  classes <- substring(classes, 2)
  drawn_later <- c(
    # DataTables inserts its table, length selector and search field after load.
    "dataTable", "dataTables_length", "dataTables_filter",
    "dtrg-group", "col-resizer", "sieve-cell", "paginate_button",
    # The item grid's Response bounds cells, classed by DataTables' columnDefs.
    "sieve-reverse-cell",
    # Saved comparison cards are rendered only after records are selected.
    # Their actual markup and laptop layout are exercised by reporting tests.
    "sieve-saved-results", "sieve-saved-record", "sieve-saved-compact",
    "show", "active", "selected", "navbar-inverse", "sieve-missing-packages", "sieve-about",
    "sieve-accessible-explanation", "sieve-empirical",
    # The lifecycle badge at the start of a status line is drawn by the
    # server with the line (fct_item_decision.R); test-item-decisions.R
    # renders it.
    "sieve-lifecycle", "sieve-lifecycle-current", "sieve-lifecycle-running", "sieve-lifecycle-refitting",
    "sieve-lifecycle-stale", "sieve-lifecycle-settings", "sieve-lifecycle-stopped", "sieve-lifecycle-failed",
    "sieve-lifecycle-not_run", "sieve-lifecycle-blocked",
    # The status line and its notes, drawn by sieve_status_line()
    # (fct_page_layout.R) in the Ingest page's status_chip and the Content
    # Validity page's action_note renderUI(); the tone classes are those of
    # the line's state. test-status-and-notifications.R renders each.
    "sieve-status-line", "sieve-status-success", "sieve-status-warning", "sieve-status-danger",
    "sieve-status-notes",
    # The confirmed Confirm button, drawn by the Ingest page's
    # confirm_button renderUI() once the types are confirmed.
    "sieve-confirmed-state",
    # The provenance disclosure under a method statement, drawn with the
    # result by sieve_method_line() (fct_page_layout.R), the reuse note
    # (fct_fit_store.R) and the CFA and Reliability method renderUI().
    "sieve-provenance",
    # Shiny's notification and its close control, built in the browser by
    # shiny.js when sieve_notify() shows one; sieve.js makes the close
    # control a Dismiss button.
    "shiny-notification", "shiny-notification-close"
  )
  # The pages in their five workspaces; a class drawn there is in use.
  html <- paste(as.character(htmltools::renderTags(app_ui(list()))$html), collapse = "\n")
  present <- vapply(classes, function(cl) {
    pattern <- paste0("class=\"([^\"]*[[:space:]])?", cl, "([[:space:]][^\"]*)?\"")
    grepl(pattern, html, perl = TRUE)
  }, logical(1))
  expect_true(length(classes) >= 15L)
  expect_identical(classes[!present & !classes %in% drawn_later], character(0))
  # The header rules key on the class bslib gives the navbar.
  expect_match(html, "<nav class=\"navbar navbar-default", fixed = TRUE)
})

# ---- sieve_dt() sends DataTables no named vector ------------------------

test_that("sieve_dt() adds no column definition, so columns of markup sort as they always have and nothing is sent by name", {
  # shiny and htmlwidgets write a named vector as a JSON object, with a
  # message from jsonlite, and DataTables reads a number, a string or an
  # array as column targets. The detector fires on the form that was sent.
  write_json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, keep_vec_names = TRUE))
  expect_message(sent <- write_json(list(list(targets = c(f1 = 2L), orderable = FALSE))), "named vector")
  expect_match(sent, "\"targets\":{\"f1\":2}", fixed = TRUE)

  # A definition making every column with a cell beginning with a tag
  # unsortable, its targets which() of a logical named by column
  # (c(f1 = 2, Note = 3)), is skipped by DataTables (in Chrome the header of
  # such a column has the class "sorting", and "sorting_disabled" once the
  # names are gone), and jsonlite prints its message at every such table; a
  # jsonlite that writes a named vector as an array would apply it and take
  # the sort of the loadings of the exploratory tables, emphasized in
  # markup, away. sieve_dt() sends what the table itself defines.
  df <- data.frame(
    Item = c("b", "a", "c"), Score = c(2, 3, 1),
    f1 = sprintf("<span style=\"font-weight:600\">%.3f</span>", c(0.851, 0.404, 0.723)),
    Note = c("<i>x</i>", "<i>y</i>", "<i>z</i>"),
    stringsAsFactors = FALSE
  )
  unsortable <- function(widget) {
    Filter(function(def) identical(def$orderable, FALSE), widget$x$options$columnDefs)
  }
  named <- function(x) {
    if (is.list(x)) return(any(vapply(x, named, logical(1))))
    is.atomic(x) && length(names(x)) > 0L
  }
  for (rownames in c(FALSE, TRUE)) {
    w <- sieve_dt(df, rownames = rownames, selection = "none", escape = FALSE, options = list(dom = "t"))
    plain <- DT::datatable(df, rownames = rownames, selection = "none", escape = FALSE, options = list(dom = "t"))
    expect_identical(w$x$options, plain$x$options)
    # No definition turns off the ordering of a data column (with row names
    # DT turns it off for the row name column, column 0, itself).
    expect_identical(
      sort(unique(as.integer(unlist(lapply(unsortable(w), function(def) def$targets))))),
      if (rownames) 0L else integer(0)
    )
    expect_false(named(w$x$options))
    expect_no_message(write_json(w$x$options))
  }
  # A table's own definitions arrive unchanged and first, as it wrote them.
  own <- list(list(orderable = FALSE, targets = 3L), list(width = "120px", targets = 0L))
  w <- sieve_dt(df, rownames = FALSE, options = list(dom = "t", columnDefs = own))
  expect_identical(w$x$options$columnDefs[seq_along(own)], own)
  expect_identical(lapply(unsortable(w), function(def) def$targets), list(3L))
  expect_no_message(json <- write_json(w$x$options$columnDefs))
  expect_match(json, "\"orderable\":false,\"targets\":3", fixed = TRUE)
  # No targets arrive as an object keyed by a column's name.
  expect_false(grepl("\"targets\":{", json, fixed = TRUE))
})

# ---- Declared and used packages -----------------------------------------

test_that("every package the code calls is declared, and every declaration is used", {
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  imports <- compat_field(root, "Imports")
  suggests <- compat_field(root, "Suggests")
  # The base-priority packages R CMD check lets code call by namespace
  # without declaring them: every one but methods and stats4
  # (tools:::.check_packages_used() in R 4.6, whose "'::' or ':::' import
  # not declared from" is a WARNING). The list leaves methods out, so a
  # methods::existsMethod() call needs methods declared, as it is in
  # Imports.
  standard <- c(
    "base", "stats", "utils", "tools", "graphics", "grDevices",
    "datasets", "grid", "parallel", "splines", "tcltk", "compiler"
  )

  # Packages called by namespace in code, read from the parse tree so that a
  # name inside a comment or a string does not count.
  called <- function(files) {
    unique(unlist(lapply(files, function(f) {
      pd <- utils::getParseData(parse(f, keep.source = TRUE))
      pd$text[pd$token == "SYMBOL_PACKAGE"]
    })))
  }
  # Packages a file loads by name, loadNamespace("x") or requireNamespace("x"),
  # also read from the parse tree. R CMD check holds each to a declaration
  # ("unstated dependencies in tests"), so a package the tests load this way
  # is a declaration in use although no pkg:: names it. processx is the case:
  # one bootstrap test loads it to reproduce a session that has started a
  # background process, and rule 4 below must see that use.
  loaded_by_name <- function(files) {
    unique(unlist(lapply(files, function(f) {
      pd <- utils::getParseData(parse(f, keep.source = TRUE))
      pd <- pd[pd$terminal, , drop = FALSE]
      pd <- pd[order(pd$line1, pd$col1), , drop = FALSE]
      at <- which(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% c("loadNamespace", "requireNamespace"))
      at <- at[at + 2L <= nrow(pd)]
      at <- at[pd$token[at + 1L] == "'('" & pd$token[at + 2L] == "STR_CONST"]
      gsub("^[\"']|[\"']$", "", pd$text[at + 2L])
    })))
  }
  in_r <- called(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))
  test_files <- list.files(file.path(root, "tests"),
    pattern = "[.]R$",
    full.names = TRUE, recursive = TRUE
  )
  in_tests <- called(test_files)
  loaded_in_tests <- loaded_by_name(test_files)
  # The reader finds the one load it was written for, and not a package that
  # is only named in a string or a comment.
  expect_true("processx" %in% loaded_in_tests)
  expect_false("processx" %in% in_tests)
  probe <- tempfile(fileext = ".R")
  writeLines(c(
    "# loadNamespace(\"commented\")", "x <- \"loadNamespace('quoted')\"",
    "loadNamespace(\"loaded\")", "if (base::requireNamespace('required', quietly = TRUE)) NULL",
    "loadNamespace(variable)"
  ), probe)
  expect_setequal(loaded_by_name(probe), c("loaded", "required"))
  unlink(probe)
  vignettes <- list.files(file.path(root, "vignettes"), pattern = "[.]Rmd$", full.names = TRUE)
  in_vignettes <- unique(unlist(lapply(vignettes, function(f) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    c(
      regmatches(txt, gregexpr("[A-Za-z][A-Za-z0-9.]*(?=::)", txt, perl = TRUE))[[1]],
      sub(
        "^library\\(([A-Za-z0-9.]+)\\).*$", "\\1",
        regmatches(txt, gregexpr("library\\([A-Za-z0-9.]+\\)", txt))[[1]]
      )
    )
  })))
  ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  ns_imports <- unique(sub("^import(From)?\\(([^,)]+).*$", "\\2", grep("^import", ns, value = TRUE)))

  # 1. Nothing in R/ is called without being declared.
  undeclared <- setdiff(in_r, c(imports, suggests, standard, "SIEVER"))
  expect_identical(undeclared, character(0))
  # 2. A package under Suggests is never imported unconditionally.
  expect_identical(intersect(ns_imports, suggests), character(0))
  # 3. Every Imports entry is used by the package code.
  expect_identical(setdiff(imports, c(in_r, ns_imports)), character(0))
  # 4. Every Suggests entry is used somewhere. Every exploratory solution is
  #    DMAR::fa_exploratory(), whose rotations are lavaan's own gradient
  #    projection, and no psych function SIEVER calls rotates, so
  #    GPArotation (the rotation backend of psych::fa()) is not in Suggests
  #    and nothing in R/ names it.
  used_anywhere <- unique(c(in_r, in_tests, in_vignettes, loaded_in_tests))
  # boot is called by DMAR's bootstrap intervals, which the Reliability tab
  # offers and the tests exercise; under R CMD check only declared packages
  # are available, so it is declared here although no SIEVER code names it.
  # vctrs, which a test loads to reproduce a researcher's session (ggplot2
  # loads it), is allowed too: no code in R/ calls it, and its absence
  # cannot prevent installing or running SIEVER's core paths.
  expect_identical(setdiff(suggests, c(used_anywhere, "boot", "vctrs")), character(0))
  expect_false("GPArotation" %in% c(imports, suggests))
  expect_false("GPArotation" %in% in_r)
  code_r <- unlist(lapply(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE), function(f) {
    txt <- readLines(f, warn = FALSE)
    txt[!grepl("^[[:space:]]*#", txt)]
  }))
  # No code fits psych::fa() (whose oblique rotations need GPArotation).
  expect_false(any(grepl("psych::fa(", code_r, fixed = TRUE)))
  expect_false(any(grepl("sieve_have(\"GPArotation\")", code_r, fixed = TRUE)))
  # semTools is not used: DMAR computes the incomplete-data Bollen-Stine
  # bootstrap and the two-stage residual test.
  expect_false("semTools" %in% in_r)
  expect_false("semTools" %in% c(imports, suggests))
})

test_that("every optional package guard has an entry that tells the user what to do", {
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  imports <- compat_field(root, "Imports")
  suggests <- compat_field(root, "Suggests")
  registry <- SIEVER:::sieve_optional_packages()
  expect_identical(anyDuplicated(registry$package), 0L)
  expect_true(all(nzchar(registry$provides)) && all(nzchar(registry$tabs)))
  # The registry only names packages that are in fact optional.
  expect_identical(setdiff(registry$package, suggests), character(0))

  # Every package tested for with sieve_have() anywhere in R/ is either a hard
  # dependency (so the test can never fail for an installed SIEVER) or has a
  # registry row, and so appears in the notice when it is missing.
  guarded <- unique(unlist(lapply(
    list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
    function(f) {
      txt <- readLines(f, warn = FALSE)
      txt <- txt[!grepl("^[[:space:]]*#", txt)]
      m <- regmatches(txt, gregexpr("sieve_have\\(\"[A-Za-z0-9.]+\"\\)", txt))
      sub("^sieve_have\\(\"(.*)\"\\)$", "\\1", unlist(m))
    }
  )))
  # Four (haven, mice, psychonetrics, svglite): ggplot2 is imported and not
  # guarded.
  expect_true(length(guarded) >= 4L)
  expect_false("ggplot2" %in% guarded)
  expect_identical(setdiff(guarded, c(imports, registry$package)), character(0))
  # The tabs named in the registry are real tab labels.
  tabs <- unique(trimws(unlist(strsplit(registry$tabs, ",", fixed = TRUE))))
  html <- paste(as.character(app_ui(list())), collapse = "")
  for (tab in tabs) {
    # An optional tab's label sits in a span.
    expect_true(grepl(paste0(">", tab, "(</span>)?[[:space:]]*</a>"), html), info = tab)
  }
})

test_that("the page says which optional packages are missing and how to install them", {
  expect_identical(SIEVER:::sieve_install_command("haven"), "install.packages(\"haven\")")
  expect_identical(
    SIEVER:::sieve_install_command(c("semPlot", "haven")),
    "install.packages(c(\"semPlot\", \"haven\"))"
  )
  expect_null(SIEVER:::sieve_missing_packages_notice(character(0)))

  # With everything installed the page carries no notice at all. (Asked
  # without loading anything, as the notice asks.)
  everything <- SIEVER:::sieve_optional_packages()$package
  if (all(vapply(everything, function(p) nzchar(system.file(package = p)), logical(1)))) {
    expect_identical(SIEVER:::sieve_missing_packages(), character(0))
    expect_false(grepl("sieve-missing-packages", paste(as.character(app_ui(list())), collapse = ""),
      fixed = TRUE
    ))
  }

  # Two packages reported missing: the notice names each, says what it costs
  # and where, and gives one command that installs both. GPArotation is not
  # an optional package, so it is not reported even when missing.
  testthat::local_mocked_bindings(
    sieve_installed = function(pkg) !pkg %in% c("svglite", "haven", "GPArotation"),
    .package = "SIEVER"
  )
  expect_identical(SIEVER:::sieve_missing_packages(), c("svglite", "haven"))
  html <- paste(as.character(app_ui(list())), collapse = "")
  expect_true(grepl("sieve-missing-packages", html, fixed = TRUE))
  notice <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", "", as.character(
    SIEVER:::sieve_missing_packages_notice()
  )))
  # SIEVER draws its path diagrams itself; only the SVG file format can
  # depend on an optional package.
  expect_match(notice, "svglite: path diagrams written as SVG", fixed = TRUE)
  expect_match(notice, "(CFA, SEM).", fixed = TRUE)
  expect_false(grepl("semPlot", notice, fixed = TRUE))
  expect_match(notice, "haven: reading SPSS (.sav) and Stata (.dta) files", fixed = TRUE)
  expect_match(notice, "install.packages(c(\"svglite\", \"haven\"))", fixed = TRUE)
  expect_false(grepl("GPArotation", notice, fixed = TRUE))
  expect_false(grepl("lavaan", notice, fixed = TRUE))
})

# ---- The graphics device probe (SVG download) -----------------------------

# The probe keeps its answers for the session; each test starts from an empty
# cache and leaves the cache as it found it, so no test depends on another.
local_empty_device_cache <- function(env = parent.frame()) {
  cache <- SIEVER:::sieve_device_cache
  kept <- mget(ls(cache, all.names = TRUE), envir = cache)
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  withr::defer(
    {
      rm(list = ls(cache, all.names = TRUE), envir = cache)
      for (nm in names(kept)) assign(nm, kept[[nm]], envir = cache)
    },
    envir = env
  )
  invisible(cache)
}

test_that("the device probe opens the device once, closes it, and remembers the answer", {
  local_empty_device_cache()
  before <- grDevices::dev.list()
  opened <- new.env()
  opened$files <- character(0)
  real_open <- SIEVER:::sieve_device_open
  testthat::local_mocked_bindings(
    sieve_device_open = function(format, file) {
      opened$files <- c(opened$files, file)
      real_open(format, file)
    },
    .package = "SIEVER"
  )
  expect_true(SIEVER:::sieve_device_works("pdf"))
  expect_length(opened$files, 1L)
  # The second question is answered from the cache: no device is opened.
  expect_true(SIEVER:::sieve_device_works("pdf"))
  expect_length(opened$files, 1L)
  # A refresh tries again.
  expect_true(SIEVER:::sieve_device_works("pdf", refresh = TRUE))
  expect_length(opened$files, 2L)
  # Nothing is left open and the temporary files are gone.
  expect_identical(grDevices::dev.list(), before)
  expect_false(any(file.exists(opened$files)))
  expect_error(SIEVER:::sieve_device_works("tiff"), "should be one of")
})

test_that("a device that cannot be opened is reported as not working, and the answer is kept", {
  local_empty_device_cache()
  before <- grDevices::dev.list()
  # R is built with cairo (capabilities() says TRUE)
  # and the device still fails to open because the module does not load.
  testthat::local_mocked_bindings(
    sieve_device_open = function(format, file) {
      stop("unable to load shared object 'R_X11.so'")
    },
    .package = "SIEVER"
  )
  expect_false(SIEVER:::sieve_device_works("svg"))
  expect_identical(grDevices::dev.list(), before)
  expect_identical(get("svg", envir = SIEVER:::sieve_device_cache), FALSE)

  # The cached answer stands until a refresh, even when the device would now
  # open: the interface asks once per session.
  testthat::local_mocked_bindings(
    sieve_device_open = function(format, file) grDevices::pdf(file),
    .package = "SIEVER"
  )
  expect_false(SIEVER:::sieve_device_works("svg"))
  expect_true(SIEVER:::sieve_device_works("svg", refresh = TRUE))
  expect_identical(grDevices::dev.list(), before)
})

test_that("a device call that opens no device, or more than one, does not count as working", {
  local_empty_device_cache()
  before <- grDevices::dev.list()
  testthat::local_mocked_bindings(
    sieve_device_open = function(format, file) invisible(NULL),
    .package = "SIEVER"
  )
  expect_false(SIEVER:::sieve_device_works("png"))
  expect_identical(grDevices::dev.list(), before)

  stray <- tempfile(fileext = ".pdf")
  withr::defer(unlink(stray))
  testthat::local_mocked_bindings(
    sieve_device_open = function(format, file) {
      grDevices::pdf(file)
      grDevices::pdf(stray)
    },
    .package = "SIEVER"
  )
  expect_false(SIEVER:::sieve_device_works("png", refresh = TRUE))
  # Both stray devices were closed.
  expect_identical(grDevices::dev.list(), before)
})

test_that("svglite is reported as unavailable without the package, and no device is tried", {
  local_empty_device_cache()
  testthat::local_mocked_bindings(
    sieve_have = function(pkg) FALSE,
    sieve_device_open = function(format, file) stop("the probe should not open a device"),
    .package = "SIEVER"
  )
  expect_false(SIEVER:::sieve_device_works("svglite"))
})

test_that("the probe leaves the device that was current in place", {
  local_empty_device_cache()
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  current <- grDevices::dev.cur()
  n_open <- length(grDevices::dev.list())
  expect_true(SIEVER:::sieve_device_works("pdf", refresh = TRUE))
  expect_identical(grDevices::dev.cur(), current)
  expect_length(grDevices::dev.list(), n_open)
})

test_that("the formats every R build provides are reported as working here", {
  local_empty_device_cache()
  expect_true(SIEVER:::sieve_device_works("pdf"))
  skip_if_not(isTRUE(capabilities("png")), "this R build has no png device")
  expect_true(SIEVER:::sieve_device_works("png"))
  # The SVG answer is whatever this build gives; it is recorded rather than
  # assumed, which is the point of the probe (capabilities("cairo") can be
  # TRUE on a build whose svg() cannot open).
  svg <- SIEVER:::sieve_device_works("svg")
  expect_type(svg, "logical")
  expect_length(svg, 1L)
})

# ---- The Reproducible Code tab: the script runs in a clean process ----------

# The session the tab is asked to reproduce: load the bundled nine tests,
# confirm, reverse one item, exclude one item, fit the CFA. The script it
# shows is written to a file and run step by step in a fresh Rscript --vanilla
# process. The CFA the clean process fits is compared with an independent
# lavaan fit built here from DMAR's data and the two stated transformations,
# not with the session's own values; the session's fit is compared with the
# same independent fit. The bundled example declares age_years an auxiliary
# variable, and a declared auxiliary enters a FIML fit only when an analyzed
# item value is missing (Graham, 2003): the eight tests are observed in all
# 301 rows, so the fit has no auxiliary.
replay_independent_fit <- function(reverse = "t2_cubes", exclude = "t10_addition") {
  d <- DMAR::holzinger_swineford
  # The reversal rule the Ingest tab records when no response bounds were
  # declared: x -> (min + max) - x over the observed range.
  d[[reverse]] <- (min(d[[reverse]]) + max(d[[reverse]])) - d[[reverse]]
  # The three theorized constructs of the primary nine, less the excluded item.
  constructs <- list(
    Visual = c("t1_visual_perception", "t2_cubes", "t4_lozenges"),
    Textual = c("t6_paragraph_comprehension", "t7_sentence", "t9_word_meaning"),
    Speed = c("t10_addition", "t12_counting_groups_of_dots", "t13_straight_and_curved_capitals")
  )
  constructs <- lapply(constructs, setdiff, exclude)
  model <- paste(
    sprintf(
      "%s =~ %s", names(constructs),
      vapply(constructs, paste, "", collapse = " + ")
    ),
    collapse = "\n"
  )
  # Every analyzed item observed in every row: the rule leaves the declared
  # auxiliary out.
  expect_false(anyNA(d[unlist(constructs, use.names = FALSE)]))
  lavaan::cfa(model, data = d, estimator = "MLR", std.lv = TRUE, missing = "fiml")
}

replay_expect_runs <- function(exclude_from) {
  session <- compat_replay_session(exclude_from = exclude_from)
  expect_true(inherits(session$fit, "lavaan"))
  expect_match(session$script, "cfa_fit <- lavaan::cfa(", fixed = TRUE)
  # The script says why the declared auxiliary is not in the fit.
  expect_match(session$script, paste(
    "# Auxiliary variable age_years is not used: every analyzed item is observed in every analyzed row,",
    "so saturated correlates add no information (Graham, 2003)."
  ), fixed = TRUE)
  steps <- grep("^# \\[", session$log, value = TRUE)
  # When the exclusion is made on the Items tab, that tab records the shared
  # correlation basis of the included items when it first reads it and again
  # after the exclusion (a new item pool), its diagnostics for each pool,
  # and its overall measurement model once (Show code at the point of use),
  # so eleven steps. A drop from the Ingest grid never visits the Items
  # tab: six.
  expect_length(steps, if (identical(exclude_from, "items")) 11L else 6L)

  independent <- replay_independent_fit()
  want <- lavaan::coef(independent)
  # The session itself fitted the model the two transformations imply.
  have <- lavaan::coef(session$fit)
  expect_identical(names(have), names(want))
  expect_equal(unname(have), unname(want), tolerance = 1e-8)

  res <- compat_replay_run(session$script, state = session$state)
  # A failure names the recorded step that stopped the script, the error, and
  # what the name `items` was bound to after each step that did run.
  trail <- paste(sprintf("%s [items: %s]", res$steps$step, res$steps$items_class), collapse = "\n")
  testthat::expect(
    is.na(res$failed_step) && identical(res$status, 0L),
    sprintf(
      "the script stopped at %s with: %s\nsteps that ran:\n%s",
      res$failed_step, res$error, trail
    )
  )
  if (is.null(res$coef)) {
    return(invisible(NULL))
  }
  expect_identical(names(res$coef), names(want))
  expect_equal(unname(res$coef), unname(want), tolerance = 1e-8)
}

test_that("the session script replays in a clean process after an exclusion from the Items tab", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  replay_expect_runs("items")
})

test_that("the session script replays in a clean process after a drop from the Ingest grid", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  replay_expect_runs("ingest")
})

test_that("the session script replays each page's fit that carries a declared auxiliary variable", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  # The two replays above reach a fit without an auxiliary: the nine tests
  # are complete, so age_years stays out. With 25
  # values of t1_visual_perception missing it enters every FIML fit, and the
  # script of each page that fits one (CFA, Validity, Bifactor, SEM, the
  # Reliability tab's omega, H and alpha of each construct, and the
  # measurement model behind the factor scores) must fit the same model to
  # the same estimates in a clean process, the auxiliary included.
  st <- siever_test_study(configure = function(s) s$items[1:25, "t1_visual_perception"] <- NA_real_)
  got <- new.env()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", estimator = "auto", layout = "top_down")
    session$setInputs(fit_go = 1L)
    got$cfa_fit <- cfa_run()$fit
  })
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(fit_go = 1L)
    # The page writes its fit's call into the script when the correlations
    # are shown.
    invisible(output$phi_tbl)
    got$validity_fit <- fit()
  })
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    suppressWarnings(session$setInputs(fit = 1))
    got$bifactor_fit <- res()$fit
  })
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(outcomes = "Visual", predictors = c("Textual", "Speed"), layout = "tree2", min_path = 0)
    session$setInputs(fit_go = 1L)
    got$sem_fit <- path_run()$fit
  })
  # The Reliability tab: omega (model-implied denominator, MLR interval), H
  # and alpha of each construct under FIML. Each is a DMAR call on the
  # construct's items and age_years, recorded as omega_<construct>,
  # H_<construct> and alpha_<construct>.
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(scope = "construct", ci_method = "mlr", denominator = "model_implied", missing = "fiml")
    session$setInputs(run_go = 1L)
    got$reliability <- immediate()
  })
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(score_method = "Bartlett", conf_level = .95, scores_go = 1L)
    got$score_fit <- demand_scores$result()$fit
  })
  shiny::testServer(mod_repro_server, args = list(state = st), {
    got$script <- output$code
  })
  fits <- c("cfa_fit", "validity_fit", "bifactor_fit", "sem_fit", "score_fit")
  for (nm in fits) {
    expect_true(inherits(got[[nm]], "lavaan"), info = nm)
    expect_true("age_years" %in% lavaan::lavNames(got[[nm]], "ov"), info = nm)
  }
  constructs <- c("Visual", "Textual", "Speed")
  expect_identical(names(got$reliability), constructs)
  coefficients <- c("omega", "H", "alpha")
  reliability_values <- as.vector(outer(coefficients, constructs, paste, sep = "_"))
  for (cn in constructs) {
    expect_identical(got$reliability[[cn]]$spec$aux, "age_years", info = cn)
    for (co in coefficients) {
      expect_true(is.finite(got$reliability[[cn]][[co]]$estimate), info = paste(co, cn))
    }
  }
  expect_false(grepl("Auxiliary variable age_years is not used", got$script, fixed = TRUE))

  res <- compat_replay_run(got$script, state = st, fits = fits, values = reliability_values)
  trail <- paste(sprintf("%s [items: %s]", res$steps$step, res$steps$items_class), collapse = "\n")
  testthat::expect(
    is.na(res$failed_step) && identical(res$status, 0L),
    sprintf("the script stopped at %s with: %s\nsteps that ran:\n%s", res$failed_step, res$error, trail)
  )
  for (nm in fits) {
    have <- lavaan::coef(got[[nm]])
    expect_identical(names(res$coefs[[nm]]), names(have), info = nm)
    expect_equal(unname(res$coefs[[nm]]), unname(have), tolerance = 1e-8, info = nm)
  }
  # Each replayed coefficient was computed with age_years, and its
  # estimate, standard error and confidence limits are the session's.
  for (cn in constructs) {
    for (co in coefficients) {
      nm <- paste(co, cn, sep = "_")
      kept <- got$reliability[[cn]][[co]]
      expect_identical(res$aux[[nm]], "age_years", info = nm)
      expect_equal(
        unname(res$values[[nm]][c("estimate", "se", "ci_lower", "ci_upper")]),
        c(kept$estimate, kept$se, kept$lower, kept$upper),
        tolerance = 1e-8, info = nm
      )
    }
  }

  # The session's CFA against an independent lavaan fit of the same data
  # with age_years as a saturated correlate (Graham, 2003). lavaan warns
  # that the observed variances differ by a factor of over 1,000 (age in
  # years against t13); SIEVER says so in its own sentence.
  d <- DMAR::holzinger_swineford
  d$t1_visual_perception[1:25] <- NA
  model <- paste(
    "Visual =~ t1_visual_perception + t2_cubes + t4_lozenges",
    "Textual =~ t6_paragraph_comprehension + t7_sentence + t9_word_meaning",
    "Speed =~ t10_addition + t12_counting_groups_of_dots + t13_straight_and_curved_capitals",
    sep = "\n"
  )
  expect_warning(
    independent <- suppressMessages(lavaan::cfa(model, data = d, estimator = "ML", std.lv = TRUE,
      missing = "fiml", aux = "age_years")),
    "some observed variances"
  )
  want <- lavaan::coef(independent)
  expect_identical(names(lavaan::coef(got$cfa_fit)), names(want))
  expect_equal(unname(lavaan::coef(got$cfa_fit)), unname(want), tolerance = 1e-8)
})

# ---- The methods draft says what it computed and from which data -----------

test_that("the methods draft names the model it refit and the revision it used", {
  skip_if_not_installed("lavaan")
  st <- compat_state()
  rev <- SIEVER:::sieve_revision(st)
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  draft <- SIEVER:::sieve_methods_draft(items, meta, dataset_name = "HS9", revision = rev)
  expect_match(draft, "What this draft computed.", fixed = TRUE)
  expect_match(draft, "refit for the draft", fixed = TRUE)
  expect_match(draft, sprintf("data revision %s", rev$data_key), fixed = TRUE)
  expect_match(draft, sprintf("ledger revision %s", rev$meta_key), fixed = TRUE)
  expect_match(draft, "does not read the results shown on the CFA", fixed = TRUE)
  expect_match(draft, "does not reproduce an analysis run earlier in the session", fixed = TRUE)
  # The model it refit is written out, in the ledger's own terms.
  expect_match(draft, "The model refit for this draft:", fixed = TRUE)
  expect_match(draft, "Visual =~ t1_visual_perception + t2_cubes + t4_lozenges", fixed = TRUE)
  # Auto is ML with FIML for continuous items.
  expect_match(draft, "with full information maximum likelihood, so incomplete cases contribute to the CFA", fixed = TRUE)
  # The provenance precedes the fit statistics it describes.
  expect_lt(
    regexpr("What this draft computed.", draft, fixed = TRUE),
    regexpr("confirmatory factor analysis fit as follows", draft, fixed = TRUE)
  )

  # Without a revision the keys are computed from the inputs themselves.
  plain <- SIEVER:::sieve_methods_draft(items, meta)
  expect_match(plain, sprintf("data revision %s", SIEVER:::sieve_data_key(items)), fixed = TRUE)
  expect_match(plain, "dataset not recorded", fixed = TRUE)
})

test_that("the Reproducible Code tab says the draft refits rather than reads", {
  html <- paste(as.character(SIEVER:::mod_repro_ui("repro")), collapse = "")
  text <- gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", html))
  expect_match(text, "The draft refits its own model", fixed = TRUE)
  # Reliability is the Reliability tab's default coefficient, and a
  # bootstrap interval comes only from a run on that tab.
  expect_match(text, "does not read the results shown on the CFA tab", fixed = TRUE)
  expect_match(text, "the coefficient the Reliability tab reports by default", fixed = TRUE)
  expect_false(grepl("cites the same fits", text, fixed = TRUE))

  # And the recorded step says the same, with the revision passed through.
  st <- compat_state()
  shiny::testServer(SIEVER:::mod_repro_server, args = list(state = st), {
    session$setInputs(draft_go = 1)
    expect_match(output$draft, "What this draft computed.", fixed = TRUE)
    expect_match(output$draft, sprintf("data revision %s", SIEVER:::sieve_data_key(state$items)), fixed = TRUE)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "does not read the fits logged above", fixed = TRUE)
    expect_false(grepl("cites the same fits", log, fixed = TRUE))
    # The recorded step names the two results the draft may take from a
    # record, as the draft and the page do, and does not say that every
    # number is recomputed.
    expect_match(log, "apart from two results taken from records, which the draft names", fixed = TRUE)
    expect_match(log, "# SRMR, or DMAR's refusal of it, the CFA tab computed for the identical fit: the", fixed = TRUE)
    expect_false(grepl("all numbers recomputed", log, fixed = TRUE))
  })
})

test_that("the README's table of optional features names every optional package the app checks for", {
  # install_github() installs no Suggests, so the README says which feature
  # needs which package; the
  # table and the app's own list (sieve_optional_packages()) must agree.
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  readme <- file.path(root, "README.md")
  skip_if_not(file.exists(readme), "no README beside the tests")
  lines <- readLines(readme, warn = FALSE)
  start <- grep("^[|] Optional feature [|]", lines)
  expect_length(start, 1L)
  rows <- lines[seq(start, length(lines))]
  rows <- rows[seq_len(match(FALSE, startsWith(rows, "|")) - 1L)]
  named <- unique(unlist(regmatches(rows, gregexpr("`[A-Za-z0-9.]+`", rows))))
  named <- gsub("`", "", named, fixed = TRUE)
  expect_identical(setdiff(sieve_optional_packages()$package, named), character(0))
  # Every package the table names is declared under Suggests.
  suggests <- compat_field(root, "Suggests")
  expect_identical(setdiff(named, suggests), character(0))
})

test_that("lavaan is imported, so its version floor is enforced when SIEVER loads", {
  # R checks an Imports version only for a namespace that is imported; with
  # lavaan called only through lavaan::, an installed SIEVER would load and
  # run on lavaan 0.7-2, whose categorical EBM and ML scores are wrong.
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  expect_true(any(grepl("^importFrom[(]lavaan,", ns)))
  imports <- read.dcf(file.path(root, "DESCRIPTION"), fields = "Imports")[1L, 1L]
  expect_match(imports, "lavaan (>= 0.7-3)", fixed = TRUE)
  expect_true("lavaan" %in% loadedNamespaces())
  expect_true(utils::packageVersion("lavaan") >= "0.7-3")
})

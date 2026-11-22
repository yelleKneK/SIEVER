# Tests for the interface layout: the embedded default palette offered as
# "Default", the tab order with IRT and SEM optional, the Content Validity
# tab's explanation and example, and the screening sentence, which carries
# no citation. Interface behavior is read from the tags the UI functions build
# and from the module servers under shiny::testServer.

skip_if_not_installed("shiny")

interface_layout_html <- function(tag) paste(as.character(tag), collapse = "")
# The same, with runs of whitespace collapsed, for prose that the tag
# renderer breaks across lines at the boundaries of its string children.
interface_layout_text <- function(tag) gsub("\\s+", " ", interface_layout_html(tag))

# The package source tree, when the tests run from it (pkgload or
# devtools::test()); NULL for an installed copy, where the source checks are
# skipped.
interface_layout_source_root <- function() {
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

# ---- 1. The palette ----------------------------------------------------------

test_that("the embedded default palette is offered as Default and holds its five colors", {
  ch <- sieve_palette_choices()
  expect_identical(ch, c(
    "Default" = "nd",
    "Okabe-Ito (colorblind safe)" = "okabe_ito",
    "Viridis (perceptually uniform)" = "viridis"
  ))
  expect_identical(names(ch)[1], "Default")
  expect_false(any(grepl("Notre Dame", names(ch), fixed = TRUE)))

  # The default palette is the Notre Dame inspired set of hex values, under
  # the label Default.
  expect_identical(
    sieve_palettes()$nd,
    c("#0c2340", "#d39f10", "#0a843d", "#1c4f8f", "#ae9142")
  )
  expect_identical(sieve_palette_colors("nd", 5L), sieve_palettes()$nd)
  expect_identical(sieve_palette_colors(n = 2L), sieve_palettes()$nd[1:2])
  expect_identical(sieve_palette_colors("no_such_palette", 3L), sieve_palettes()$nd[1:3])
  expect_identical(names(sieve_palettes()), c("nd", "okabe_ito", "viridis"))
  # The state starts on it.
  expect_identical(shiny::isolate(sieve_new_state()$palette), "nd")

  # The settings control on the Ingest tab offers it under that label, and
  # selected; no control names the palette's origin.
  html <- interface_layout_html(mod_ingest_ui("ingest"))
  expect_match(html, '<option value="nd" selected>Default</option>', fixed = TRUE)
  expect_match(html, '<option value="okabe_ito">Okabe-Ito (colorblind safe)</option>', fixed = TRUE)
  expect_match(html, '<option value="viridis">Viridis (perceptually uniform)</option>', fixed = TRUE)
  expect_false(grepl("Notre Dame", html, fixed = TRUE))
  # Choosing it on the tab keeps the stored value "nd", which the project
  # file, the diagrams and every plot read.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(palette = "nd")
    expect_identical(state$palette, "nd")
    session$setInputs(palette = "okabe_ito")
    expect_identical(state$palette, "okabe_ito")
  })

  # NDPalette is the source of the hex values and is never called at run
  # time: no code in the package names it as a dependency or calls it.
  root <- interface_layout_source_root()
  skip_if(is.null(root), "not running from the package source")
  r_files <- list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)
  code <- unlist(lapply(r_files, readLines, warn = FALSE))
  expect_false(any(grepl("NDPalette::|library\\(NDPalette\\)|requireNamespace\\(\"NDPalette\"", code)))
  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  deps <- paste(desc[1, intersect(c("Imports", "Suggests", "Depends"), colnames(desc))], collapse = " ")
  expect_false(grepl("NDPalette", deps, fixed = TRUE))
})

# ---- 2. The tab order -------------------------------------------------------

interface_layout_tab_labels <- c(
  "Ingest", "Screening", "Content Validity", "Dimensionality", "EFA",
  "Items", "CFA", "Reliability", "Validity", "CV and Invariance",
  "Optimizer", "Bifactor", "Network", "IRT", "SEM", "Reproducible Code"
)

# The position of a tab's label in the page: a core label is the text of its
# link, an optional label sits in a span inside the link.
interface_layout_label_at <- function(html, label) {
  regexpr(paste0(">", label, "(</span>)?[[:space:]]*</a>"), html)
}

# The optional tabs and the nine core tabs: most tabs are optional, and a
# known structure goes to the CFA directly.
interface_layout_optional <- c(
  "Content Validity", "Optimizer", "Bifactor", "Network", "IRT", "SEM",
  "Reproducible Code"
)
interface_layout_core <- c(
  "Ingest", "Screening", "Dimensionality", "EFA", "Items", "CFA",
  "Reliability", "Validity", "CV and Invariance"
)
interface_layout_optional_modules <- list(
  list(ui = mod_content_ui, id = "content", adds = "expert ratings of item relevance", gate = FALSE),
  list(ui = mod_optimize_ui, id = "optimize", adds = "a bounded search over the item sets", gate = TRUE),
  list(ui = mod_bifactor_ui, id = "bifactor", adds = "the bifactor model and its indices", gate = TRUE),
  # The trial of psychometric network models.
  list(ui = mod_network_ui, id = "network", adds = "a trial of psychometric network models", gate = TRUE),
  list(ui = mod_irt_ui, id = "irt", adds = "item response theory reading", gate = TRUE),
  list(ui = mod_sem_ui, id = "sem", adds = "structural relations among the constructs", gate = TRUE),
  # The local tools are on Ingest; the tab adds the gate and the entry.
  list(ui = mod_repro_ui, id = "repro", adds = "the annotated session script", gate = FALSE)
)
interface_layout_core_modules <- list(
  mod_ingest_ui, mod_screen_ui, mod_retention_ui, mod_efa_ui, mod_winnow_ui,
  mod_cfa_ui, mod_reliab_ui, mod_validity_ui, mod_finalize_ui
)

test_that("the tabs are in order: Screening before Content Validity, Reliability before IRT and SEM", {
  html <- interface_layout_html(app_ui(list()))
  page_order <- sieve_workspace_pages()
  labels <- vapply(sieve_page_catalog()[page_order], function(x) x$label, character(1))
  at <- vapply(labels, function(l) interface_layout_label_at(html, l), integer(1))
  expect_true(all(at > 0))
  expect_identical(order(at), seq_along(labels))
  # The sixteen tabs, by their bookmarkable values, in document order (each
  # value appears twice, on the navbar link and on its tab pane, in the
  # same order).
  values <- regmatches(html, gregexpr('data-value="[a-z]+"', html))[[1]]
  values <- sub('data-value="([a-z]+)"', "\\1", values)
  top <- sieve_workspace_pages()
  expect_identical(unique(values[values %in% top]), top)
  expect_true(all(table(factor(values[values %in% top], levels = top)) == 2L))
  # No label carries a mark: the label is the name alone.
  for (l in interface_layout_tab_labels) {
    expect_false(grepl(paste0(l, " (optional)"), html, fixed = TRUE))
  }
})

test_that("the list of core and optional tabs is one list, and the navbar labels the optional tabs in the lighter style with specialist analyses in More", {
  paths <- sieve_tab_paths()
  expect_identical(paths$core, interface_layout_core)
  expect_identical(paths$optional, interface_layout_optional)
  expect_identical(sort(c(paths$core, paths$optional)), sort(interface_layout_tab_labels))
  # The label helper: a span for an optional tab, the plain string otherwise.
  expect_identical(sieve_nav_label("CFA"), "CFA")
  span <- interface_layout_text(sieve_nav_label("Bifactor"))
  expect_match(span,
    '<span class="sieve-nav-optional" title="Optional tab: nothing in another tab depends on it.">Bifactor</span>',
    fixed = TRUE
  )

  html <- interface_layout_text(app_ui(list()))
  for (l in interface_layout_optional) {
    expect_match(html,
      paste0(
        '<span class="sieve-nav-optional" title="Optional tab: nothing in another tab depends on it.">',
        l,
        "</span>"
      ),
      fixed = TRUE, info = l
    )
  }
  for (l in interface_layout_core) {
    expect_match(html, paste0('data-value="[a-z]+">', l, "</a>"), info = l)
    expect_false(grepl(paste0("sieve-nav-optional[^>]*>", l, "</span>"), html), info = l)
  }
  expect_false(grepl('data-value="More"', html, fixed = TRUE))
  for (page in c("optimize", "bifactor", "network", "irt", "sem", "repro")) {
    expect_match(html, paste0('data-value="', page, '"'), fixed = TRUE)
    expect_true(page %in% sieve_workspaces()[[sieve_page_workspace(page)]]$pages)
  }
  css <- compat_css()
  expect_match(css, ".nav-link:has(> .sieve-nav-optional)", fixed = TRUE)
  expect_match(css, "color: var(--sieve-navbar-fg-optional);", fixed = TRUE)
  bg <- compat_css_var(css, "--sieve-navbar-bg")
  ratio <- compat_contrast(compat_css_var(css, "--sieve-navbar-fg-optional"), bg)
  expect_gte(ratio, 4.5)
  # Lighter than a core tab at rest, and no lighter than the rule allows.
  expect_lt(ratio, compat_contrast(compat_css_var(css, "--sieve-navbar-fg-rest"), bg))
})

test_that("each of the eight optional tabs opens with the sentence saying it is optional and what it adds, and no core tab does", {
  for (mod in interface_layout_optional_modules) {
    html <- interface_layout_text(mod$ui(mod$id))
    note_at <- regexpr("This tab is optional.", html, fixed = TRUE)
    expect_gt(note_at, 0, label = mod$id)
    expect_match(html,
      "Nothing in another tab depends on it, and a study that does not need it skips it; it adds",
      fixed = TRUE,
      info = mod$id
    )
    expect_match(html, mod$adds, fixed = TRUE, info = mod$id)
    if (isTRUE(mod$gate)) {
      gate_at <- regexpr(sprintf('id="%s-gate"', mod$id), html, fixed = TRUE)
      expect_gt(gate_at, 0, label = mod$id)
      expect_lt(note_at, gate_at, label = mod$id) # the sentence opens the gate text
    }
    # The sentence is the first thing in the tab's main area: before any
    # card or sub-tab of the tab (the sidebar, where there is one, is markup
    # before the main area, and holds no card).
    first_card <- regexpr('class="card', html, fixed = TRUE)
    expect_lt(note_at, first_card, label = mod$id)
    # Exactly one such sentence per tab.
    expect_length(gregexpr("This tab is optional.", html, fixed = TRUE)[[1]], 1L)
  }
  # The sentence is one tag, with the addition as given.
  note <- interface_layout_html(sieve_optional_tab_note("a thing"))
  expect_match(note, "This tab is optional.", fixed = TRUE)
  expect_match(note, "it adds a thing.", fixed = TRUE)
  # The core tabs do not carry it.
  for (ui in interface_layout_core_modules) {
    expect_false(grepl("This tab is optional.", interface_layout_html(ui("x")), fixed = TRUE))
  }
})

test_that("the package page, the Ingest sidebar, the About dialog and the vignette name the core path and the optional tabs in one wording", {
  sentence <- sieve_tab_paths_sentence()
  # The wording itself names every core tab in order, then every optional
  # tab in order, and the CFA-first rule (a known structure goes to the CFA
  # directly).
  expect_identical(
    sentence,
    paste(
      "The core path is Ingest, Screening, Dimensionality, EFA, Items, CFA, Reliability,",
      "Validity, and CV and Invariance. With a theoretical structure the CFA tab can be",
      "opened directly after Confirm; nothing on it waits for the Dimensionality, EFA or",
      "Items tabs. The optional tabs, each saying so when opened, are Content Validity,",
      "Optimizer, Bifactor, Network, IRT, SEM, and Reproducible Code."
    )
  )
  in_order <- function(text, names) {
    at <- vapply(names, function(n) regexpr(n, text, fixed = TRUE), integer(1))
    all(at > 0) && !is.unsorted(at)
  }
  expect_true(in_order(sentence, interface_layout_core))
  expect_true(in_order(sub("^.*The optional tabs", "", sentence), interface_layout_optional))
  expect_identical(sieve_tab_list_text("a"), "a")
  expect_identical(sieve_tab_list_text(c("a", "b")), "a and b")
  expect_identical(sieve_tab_list_text(c("a", "b", "c")), "a, b, and c")

  # The Ingest sidebar (under the Confirm step) and the About dialog carry
  # the paragraph; the About dialog is built inside a server observer, so
  # its source is read for the call.
  note <- interface_layout_text(sieve_tab_paths_note())
  expect_match(note, "<strong>The tabs. </strong>", fixed = TRUE)
  expect_match(note, sentence, fixed = TRUE)
  ingest <- interface_layout_text(mod_ingest_ui("ingest"))
  expect_match(ingest, sentence, fixed = TRUE)
  sidebar_end <- regexpr("</aside>", ingest, fixed = TRUE)
  expect_lt(regexpr(sentence, ingest, fixed = TRUE), sidebar_end)
  expect_lt(regexpr("Confirm the study (step 4)", ingest, fixed = TRUE), regexpr(sentence, ingest, fixed = TRUE))

  root <- interface_layout_source_root()
  skip_if(is.null(root), "not running from the package source")
  server <- paste(readLines(file.path(root, "R", "app_server.R"), warn = FALSE), collapse = "\n")
  expect_match(server, "sieve_tab_paths_note(", fixed = TRUE)
  expect_lt(
    regexpr("How to tell what SIEVER is doing", server, fixed = TRUE),
    regexpr("sieve_tab_paths_note(", server, fixed = TRUE)
  )

  # The package page (roxygen) and the vignette are text: each carries the
  # sentence verbatim, with roxygen's line breaks collapsed.
  pkg <- readLines(file.path(root, "R", "SIEVER-package.R"), warn = FALSE)
  pkg <- gsub("\\s+", " ", paste(sub("^#' ?", "", pkg), collapse = " "))
  expect_match(pkg, sentence, fixed = TRUE)
  # The numbered list is the core path in order, with each optional tab
  # named in bold in the paragraph that follows it.
  for (i in seq_along(interface_layout_core)) {
    expect_match(pkg, sprintf("%d. **%s.**", i, interface_layout_core[i]), fixed = TRUE)
  }
  expect_true(in_order(pkg, sprintf("%d. **%s.**", seq_along(interface_layout_core), interface_layout_core)))
  expect_false(grepl("10. **", pkg, fixed = TRUE))
  optional_para <- sub("^.*The optional tabs, in the order they sit", "", pkg)
  expect_true(in_order(optional_para, paste0("**", interface_layout_optional, "**")))
  expect_lt(
    regexpr("9. **CV and Invariance.**", pkg, fixed = TRUE),
    regexpr("The optional tabs, in the order they sit", pkg, fixed = TRUE)
  )
  rd <- paste(readLines(file.path(root, "man", "SIEVER-package.Rd"), warn = FALSE), collapse = " ")
  rd <- gsub("\\s+", " ", rd)
  expect_match(rd, sentence, fixed = TRUE)
  vignette <- paste(readLines(file.path(root, "vignettes", "getting-started.Rmd"), warn = FALSE), collapse = " ")
  vignette <- gsub("\\s+", " ", vignette)
  expect_match(vignette, paste("The tabs.", sentence), fixed = TRUE)
  # The README names no tab order and is not one of the four; the vignette
  # names the Confirm the study button by its label, never "Looks good".
  expect_false(grepl("Looks good", vignette, fixed = TRUE))

  # The Reproducible Code tab's description of the script.
  repro <- interface_layout_text(mod_repro_ui("repro"))
  expect_match(repro, "Source data are stored once and checked by their content hash.", fixed = TRUE)
  expect_match(repro, "Each operation uses its recorded input revision", fixed = TRUE)
})

test_that("nothing in another tab reads the IRT or SEM tab's results", {
  # The two optional tabs keep their fits in their own module sessions; no
  # other module reads them through the shared state or the registry by
  # name. Read from the source: every module file other than the two.
  root <- interface_layout_source_root()
  skip_if(is.null(root), "not running from the package source")
  files <- list.files(file.path(root, "R"), pattern = "^mod_.*[.]R$", full.names = TRUE)
  files <- files[!basename(files) %in% c("mod_irt.R", "mod_sem.R")]
  for (f in files) {
    code <- readLines(f, warn = FALSE)
    code <- code[!grepl("^\\s*#", code)]
    expect_false(any(grepl("state\\$(irt|sem|grm)|irt_grm|grm_fit|mod_irt_server|mod_sem_server", code)),
      info = basename(f)
    )
  }
  # Display/settings may persist, but neither tab exports a fitted result
  # that another analysis implicitly consumes. Saved records are explicit.
  st <- sieve_new_state()
  optional_fields <- grep("^(irt|sem|grm)", names(shiny::isolate(shiny::reactiveValuesToList(st))), value = TRUE)
  expect_true(all(grepl("_settings$", optional_fields)))
  expect_false(any(vapply(
    shiny::isolate(shiny::reactiveValuesToList(st))[optional_fields],
    function(x) inherits(
      x,
      "lavaan"
    ),
    logical(1)
  )))
})

# ---- 2b. Start with the CFA --------------------------------------------------

test_that("after Confirm the chip names the CFA tab first under a theoretical basis, and Screening under an empirical one", {
  fx <- sieve_hs9_fixture()
  skip_if(is.null(fx), "DMAR data not available")
  chip_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", interface_layout_html(x$html)))
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1)
    expect_true(shiny::isolate(state$confirmed))
    chip <- chip_text(output$status_chip)
    expect_match(chip,
      "9 items loaded; types confirmed (ML with FIML). All tabs unlocked. Open CFA to fit the theoretical structure, or Screening to look at the items first.",
      fixed = TRUE
    )
    # The status line carries neither the run policy (it is in the page guide,
    # from its one list, sieve_run_policy_text()) nor the core path and the
    # optional tabs (they are in the sidebar under the Confirm step,
    # sieve_tab_paths_note()).
    expect_false(grepl("A first result runs by itself", chip, fixed = TRUE))
    expect_false(grepl("The core path runs left to right", chip, fixed = TRUE))
    expect_false(grepl("On the visible CFA and Reliability tabs", chip, fixed = TRUE))
    expect_false(grepl("the other tabs are optional", chip, fixed = TRUE))
    sidebar <- interface_layout_text(mod_ingest_ui("ingest"))
    expect_match(gsub("\\s+", " ", sidebar), sieve_tab_paths_sentence(), fixed = TRUE)
    # Switching the basis changes the next step, and nothing else on the chip.
    session$setInputs(basis = "empirical", apply_basis = 1L)
    expect_identical(shiny::isolate(state$construct_basis), "empirical")
    chip2 <- chip_text(output$status_chip)
    expect_match(chip2,
      "All tabs unlocked. Open Screening to look at the items, then Dimensionality and EFA for the number of factors and the structure.",
      fixed = TRUE
    )
    expect_false(grepl("Open CFA", chip2, fixed = TRUE))
    expect_identical(
      sub("Open Screening to look at the items, then Dimensionality and EFA for the number of factors and the structure.",
        "",
        chip2,
        fixed = TRUE
      ),
      sub("Open CFA to fit the theoretical structure, or Screening to look at the items first.",
        "",
        chip,
        fixed = TRUE
      )
    )
    session$setInputs(basis = "theoretical", apply_basis = 2L)
    expect_match(chip_text(output$status_chip), "Open CFA to fit the theoretical structure", fixed = TRUE)
  })
})

test_that("nothing on the CFA tab waits for the Dimensionality, EFA or Items tabs: the gates read the confirmation, the data and the ledger alone", {
  # Every gate in mod_cfa.R, read from the source. The state fields the
  # module reads are drawn from its inputs alone: the confirmation, the item
  # data and the row index that follows a respondent's removal, the raw data
  # (for the groups of the Bollen-Stine bootstrap), the ledger, and the
  # tab's own settings and palette. A field outside these is a new
  # dependency and fails the test. None of them is a field the Dimensionality,
  # EFA or Items tabs write, and no decision, basis cache or exploratory
  # result is read.
  root <- interface_layout_source_root()
  skip_if(is.null(root), "not running from the package source")
  code <- readLines(file.path(root, "R", "mod_cfa.R"), warn = FALSE)
  code <- code[!grepl("^\\s*#", code)]
  fields <- unique(unlist(regmatches(code, gregexpr("state\\$[A-Za-z_]+", code))))
  # The tab also reads the session's fit store (state$fit_store,
  # fct_fit_store.R): fitted models by exact identity, which the tab reads to
  # reuse an identical fit and writes with its own. It never waits for it (an
  # absent fit is fit here), so it is not a dependency on another tab's
  # result.
  inputs <- c(
    "state$confirmed", "state$items", "state$case_index", "state$raw",
    "state$meta", "state$cfa_settings", "state$palette", "state$fit_store"
  )
  expect_identical(setdiff(fields, inputs), character(0))
  expect_true(all(c("state$items", "state$meta") %in% fields) && grepl("sieve_confirmation_status", paste(deparse(body(mod_cfa_server)), collapse = " "), fixed = TRUE))
  written <- unique(unlist(lapply(c("mod_retention.R", "mod_efa.R", "mod_winnow.R"), function(f) {
    src <- readLines(file.path(root, "R", f), warn = FALSE)
    src <- src[!grepl("^\\s*#", src)]
    hits <- regmatches(src, gregexpr("state\\$[A-Za-z_]+(?=\\s*<-)", src, perl = TRUE))
    unlist(hits)
  })))
  expect_identical(intersect(fields, c(written, "state$decisions", "state$cache", "state$theory",
    "state$page_results")), character(0))
  expect_false(any(grepl(
    "state\\$(decisions|cache|theory)|sieve_state_basis|mod_(retention|efa|winnow)_server|sieve_item_diagnostics|sieve_record_decision",
    code
  )))
  # And at the module level: a fresh session on the bundled nine tests,
  # confirmed on the Ingest tab with the theoretical basis and no other tab
  # visited, opens to the Fit button and fits from it.
  skip_if_not_installed("lavaan")
  fx <- sieve_hs9_fixture()
  skip_if(is.null(fx), "DMAR data not available")
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1)
  })
  expect_true(shiny::isolate(st$confirmed))
  expect_identical(shiny::isolate(st$construct_basis), "theoretical")
  expect_length(shiny::isolate(st$decisions), 0L)
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    gate <- gsub("\\s+", " ", interface_layout_html(output$gate$html))
    expect_match(gate, "The model is not fit yet.", fixed = TRUE)
    expect_match(gate, '-fit_go"', fixed = TRUE)
    expect_false(grepl("Dimensionality|EFA|Items tab", gate))
    session$setInputs(fit_go = 1)
    expect_true(sieve_has_solution(cfa_run()))
    expect_identical(cfa_run()$status$code, "ok")
    expect_true(is.finite(fm()[["rmsea"]]))
    expect_no_error(output$fit_tbl)
  })
})

test_that("a saved private study of 68 items opens straight to the CFA tab's Fit button, with no other tab visited and nothing fit", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  path <- siever_profile_file("ethics.siever.rds")
  skip_if_not(file.exists(path), "the private study of 68 items is not available")
  requireNamespace("haven", quietly = TRUE)
  snap <- sieve_project_read(path)
  st <- sieve_new_state()
  sieve_project_restore(st, snap, source = "ethics.siever.rds")
  expect_true(shiny::isolate(st$confirmed))
  expect_identical(shiny::isolate(st$construct_basis), "theoretical")
  seen <- new.env()
  seen$lavaan <- 0L
  real_lavaan <- lavaan::lavaan
  testthat::local_mocked_bindings(
    lavaan = function(...) {
      seen$lavaan <- seen$lavaan + 1L
      real_lavaan(...)
    },
    .package = "lavaan"
  )
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    t0 <- proc.time()[["elapsed"]]
    gate <- gsub("\\s+", " ", interface_layout_html(output$gate$html))
    for (id in c("model_note", "vb_rmsea", "phi_tbl", "mi_tbl", "bs_note", "plan_rmsea_result")) {
      invisible(tryCatch(output[[id]], error = function(e) NULL))
    }
    seconds <- proc.time()[["elapsed"]] - t0
    expect_match(gate, "The model is not fit yet.", fixed = TRUE)
    expect_match(gate, "68 items under WLSMV", fixed = TRUE)
    expect_match(gate, '-fit_go"', fixed = TRUE)
    expect_identical(seen$lavaan, 0L)
    expect_true(cfa_run()$not_run)
    if (siever_timing_checks()) expect_lt(seconds, 10)
  })
})

# ---- 3. The Content Validity tab -------------------------------------------

# A user's study that is not the bundled example: six ordered items.
interface_layout_user_state <- function() {
  d <- withr::with_seed(7, as.data.frame(matrix(sample(1:5, 60 * 6, replace = TRUE), 60, 6,
    dimnames = list(NULL, paste0("q", 1:6)))))
  fixture <- list(data = d, item_cols = names(d), theory_map = NULL,
    name = "a user's study", labels = NULL, load_code = character())
  siever_test_study(fixture, labels = FALSE, theory = FALSE, configure = function(s) s$roles <- NULL)
}

interface_layout_hs9_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = FALSE, name = fx$name)
  s
}

test_that("the Content Validity tab says what an expert rating is, who gives it and what is computed, with an example on request", {
  skip_if_not_installed("DMAR")
  html <- interface_layout_text(mod_content_ui("content"))
  # The explanation, at the top of the tab, before the long methodological
  # paragraph.
  for (phrase in c(
    "What this tab asks for: expert ratings of the items",
    "is one judge's rating of one item's relevance to the construct",
    "1 = not relevant, 2 = somewhat relevant, 3 = quite relevant, 4 = highly relevant",
    "a panel of subject matter experts, conventionally five to ten (Lynn, 1986)",
    "DMAR::content_validity_index()",
    "item level content validity index (I-CVI",
    "modified kappa of Polit, Beck, and Owen (2007)",
    "content validity ratio (CVR)",
    "S-CVI/Ave and S-CVI/UA",
    "one row per item and one column per expert"
  )) {
    expect_match(html, phrase, fixed = TRUE, info = phrase)
  }
  expect_lt(
    regexpr("What this tab asks for", html, fixed = TRUE),
    regexpr("Content validity is evidence about the items, not about the data.", html, fixed = TRUE)
  )
  expect_match(html, 'id="content-show_example"', fixed = TRUE)
  expect_match(html, "Show an example", fixed = TRUE)
  # The static sidebar holds the upload and the template; the illustrative
  # button is not static (it is rendered only for the bundled example).
  expect_match(html, 'id="content-file"', fixed = TRUE)
  expect_match(html, 'id="content-dl_template"', fixed = TRUE)
  expect_false(grepl('id="content-demo"', html, fixed = TRUE))

  # The example dialog: the image and the file layout in words.
  modal <- interface_layout_text(sieve_content_example_modal())
  expect_match(modal, 'src="www/content_validity_example.png"', fixed = TRUE)
  expect_match(modal, "An example of a filled expert rating sheet", fixed = TRUE)
  expect_match(modal, "The first column, headed item, holds the item names", fixed = TRUE)
  expect_match(modal, "A column headed wording is optional", fixed = TRUE)
  expect_match(modal, "Every other column is one expert", fixed = TRUE)
  expect_match(modal, "a cell left empty means the expert did not rate that item", fixed = TRUE)
  expect_match(modal, "item,wording,expert_1,expert_2,expert_3,expert_4,expert_5,expert_6", fixed = TRUE)
  expect_match(modal, "constructed for teaching, not collected from a panel", fixed = TRUE)

  # The image ships with the package: a PNG of at most 200 KB, served from
  # the www resource path.
  path <- app_sys("app/www/content_validity_example.png")
  expect_true(file.exists(path))
  expect_lte(file.info(path)$size, 200 * 1024)
  expect_identical(
    readBin(path, "raw", 8L),
    as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  )
})

test_that("with a user's data the Content Validity tab starts empty, offers no illustrative ratings, and shows the example on request", {
  skip_if_not_installed("DMAR")
  st <- interface_layout_user_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    expect_null(ratings_in())
    # No illustrative button for a user's study.
    expect_false(sieve_content_demo_applies(state$meta))
    demo_ui <- output$demo_ui
    # (The mock session namespaces the module as proxy1, so the id is matched
    # on its field name.)
    expect_false(grepl('-demo"', interface_layout_html(demo_ui$html %||% ""), fixed = TRUE))
    expect_false(grepl("Load Illustrative Ratings", interface_layout_html(demo_ui$html %||% ""), fixed = TRUE))
    # The tab says what to do, and does not name the illustrative ratings.
    note <- interface_layout_text(output$parse_note$html)
    expect_match(note, "No expert ratings loaded yet", fixed = TRUE)
    expect_match(note, "Show an example", fixed = TRUE)
    expect_false(grepl("Illustrative", note, fixed = TRUE))
    expect_match(interface_layout_text(output$cvi_note$html), "Load expert ratings in the sidebar", fixed = TRUE)
    # Pressing the (absent) illustrative control loads nothing.
    expect_no_error(session$setInputs(demo = 1))
    expect_null(ratings_in())
    expect_error(cvi(), class = "shiny.silent.error")
    expect_false(any(grepl("illustrative", state$codelog, fixed = TRUE)))
    # The example is shown on request, and the tab is otherwise unchanged.
    expect_no_error(session$setInputs(show_example = 1))
    expect_null(ratings_in())
    # The panel's ratings, uploaded, are read and matched to the user's items.
    m <- sieve_content_demo_ratings()[1:6, ]
    rownames(m) <- paste0("q", 1:6)
    path <- tempfile(fileext = ".csv")
    utils::write.csv(
      data.frame(
        item = rownames(m), as.data.frame(m, check.names = FALSE),
        check.names = FALSE
      ),
      path,
      row.names = FALSE, na = ""
    )
    session$setInputs(file = list(datapath = path, name = "panel.csv"))
    expect_true(isTRUE(ratings_in()$ok))
    expect_identical(matched()$ledger_item, paste0("q", 1:6))
  })

  # With the bundled nine test example loaded, the illustrative ratings are
  # offered, named in the note, and load.
  st <- interface_layout_hs9_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    expect_true(sieve_content_demo_applies(state$meta))
    expect_match(interface_layout_html(output$demo_ui$html), '-demo"', fixed = TRUE)
    expect_match(interface_layout_html(output$demo_ui$html), "Load Illustrative Ratings", fixed = TRUE)
    expect_match(interface_layout_text(output$parse_note$html), "Load Illustrative Ratings", fixed = TRUE)
    session$setInputs(demo = 1)
    expect_true(isTRUE(ratings_in()$ok))
    expect_match(ratings_in()$source, "illustrative", fixed = TRUE)
  })
  # Before any data are loaded there is nothing for them to apply to.
  expect_false(sieve_content_demo_applies(NULL))
  expect_false(sieve_content_demo_applies(sieve_build_meta(data.frame(a = 1:3, b = 1:3), c("a", "b"))))
})

test_that("with the bundled 24 test example loaded, the illustrative ratings are offered and the help text names the items they are for; the ledger panel says excluded", {
  skip_if_not_installed("DMAR")
  # The nine rated test names are among the 24 of the full bundled example, so
  # the illustrative ratings apply there too; the sidebar must not then call
  # the loaded study the nine test example. And the ledger panel speaks of
  # included and excluded items, as the Items tab does, never of the stored
  # status value.
  fx <- sieve_hs_fixture()
  skip_if(is.null(fx), "DMAR data not available")
  s <- sieve_new_state()
  s$dataset_name <- fx$name
  s$raw <- fx$data
  s$items <- fx$data[, fx$item_cols, drop = FALSE]
  s$id_vars <- fx$id_vars
  s$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  s$confirmed <- TRUE
  s$palette <- "nd"
  s$thresholds <- sieve_default_thresholds()
  expect_true(sieve_content_demo_applies(shiny::isolate(s$meta)))
  shiny::testServer(mod_content_server, args = list(state = s), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    help <- interface_layout_text(output$demo_ui$html)
    expect_match(help, "Load Illustrative Ratings", fixed = TRUE)
    expect_false(grepl("nine test example loaded now", help, fixed = TRUE))
    expect_match(help,
      "the nine Holzinger and Swineford tests of the bundled example, all of which are in the item ledger loaded now",
      fixed = TRUE
    )
    session$setInputs(demo = 1)
    expect_identical(sum(!is.na(matched()$ledger_item)), 9L)
    # Excluding the selected items changes the stored status, not the row
    # count; the ledger table's Status column (a server side DataTable, read
    # in the browser test below) says excluded and included.
    session$setInputs(keep_all_matches = 1)
    # (The exclusion waits for confirmation of the listed items; the
    # researcher selects them, and no I-CVI cutoff does.)
    session$setInputs(ledger_tbl_rows_selected = c(7L, 9L))
    session$setInputs(park_selected = 1)
    session$setInputs(park_selected_confirm = 1)
    failed <- matched()$ledger_item[c(7L, 9L)]
    expect_identical(sort(failed), c("t10_addition", "t13_straight_and_curved_capitals"))
    expect_identical(sort(state$meta$item[state$meta$status == "parked"]), sort(failed))
    expect_identical(nrow(state$meta), 24L)
  })
  # The ledger panel speaks of excluded items, never of the stored value.
  panel <- interface_layout_text(mod_content_ui("content"))
  expect_match(panel, "Excluding an item marks it excluded in the ledger", fixed = TRUE)
  expect_false(grepl("sets its status to parked", panel, fixed = TRUE))
})

test_that("a rating sheet with the wording column the example shows is read as the example says, and the recorded R reproduces it", {
  skip_if_not_installed("DMAR")
  m <- sieve_content_demo_ratings()
  m[2, 4] <- NA
  wording <- unname(sieve_hs_labels_for(rownames(m)))
  path <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      item = rownames(m), wording = wording,
      as.data.frame(m, check.names = FALSE), check.names = FALSE
    ),
    path,
    row.names = FALSE, na = ""
  )
  res <- sieve_parse_ratings(path, "sheet.csv")
  expect_true(res$ok)
  expect_true(res$has_wording)
  expect_identical(dim(res$ratings), dim(m))
  expect_identical(dimnames(res$ratings), dimnames(m))
  expect_equal(unname(res$ratings), unname(m))
  expect_true(is.na(res$ratings[2, 4]))
  # The same without the column: identical ratings.
  without <- sieve_parse_ratings(
    {
      p <- tempfile(fileext = ".csv")
      utils::write.csv(data.frame(
        item = rownames(m), as.data.frame(m, check.names = FALSE),
        check.names = FALSE
      ), p, row.names = FALSE, na = "")
      p
    },
    "plain.csv"
  )
  expect_false(without$has_wording)
  expect_identical(without$ratings, res$ratings)
  # The header decides, not the content: a column of text under another
  # header is still refused as a rating column that is not numbers.
  p3 <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      item = rownames(m), text = wording,
      as.data.frame(m, check.names = FALSE), check.names = FALSE
    ),
    p3,
    row.names = FALSE, na = ""
  )
  r3 <- sieve_parse_ratings(p3, "text_column.csv")
  expect_false(r3$ok)
  expect_match(r3$message, "are not numbers")
  expect_match(r3$message, "text", fixed = TRUE)

  # Through the module, with the recorded R replayed from a clean environment.
  st <- interface_layout_hs9_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(file = list(datapath = path, name = path))
    parsed <- ratings_in()$ratings
    expect_identical(parsed, res$ratings)
    env <- new.env()
    eval(parse(text = paste(state$codelog, collapse = "\n")), envir = env)
    expect_identical(env$ratings, parsed)
    ref <- DMAR::content_validity_index(m, relevant = c(3, 4), conf_level = 0.95)
    expect_identical(cvi()$i_cvi, ref$i_cvi)
    expect_identical(matched()$ledger_item, rownames(m))
  })

  # The template has the layout the example describes.
  st <- interface_layout_user_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    tmpl <- utils::read.csv(output$dl_template, check.names = FALSE)
    expect_identical(names(tmpl), c("item", "wording", paste0("expert_", 1:6)))
    expect_identical(nrow(tmpl), 5L)
  })
})

test_that("in the browser the tabs are in order, a user's data get no illustrative button, and the example image is served", {
  skip_if_no_browser()

  siever_with_app(function(app) {
    roots <- app$get_js("Array.from(document.querySelectorAll('nav.navbar a[data-value]')).map(a=>a.dataset.value)")
    expect_identical(unlist(roots), c("study", "structure", "scores", "evidence", "report"))
    values <- app$get_js("Array.from(document.querySelectorAll('ul[id^=\"ws_\"] a[data-value]')).map(a=>a.dataset.value)")
    expect_identical(unlist(values), sieve_workspace_pages())
    labels <- app$get_js("Array.from(document.querySelectorAll('ul[id^=\"ws_\"] a[data-value]')).map(a=>a.textContent.trim())")
    expect_identical(unlist(labels), unname(vapply(sieve_page_catalog()[sieve_workspace_pages()], function(x) x$label, character(1))))

    # Before any data are loaded: no illustrative button, the example opens
    # and its image is served from the package's www path.
    expect_identical(siever_open_tab(app, "content"), "content")
    # The explanation card is on screen at its own height, not shrunk to a
    # strip by the fill layout, and its button is visible.
    ask <- app$get_js(paste0(
      "(function(){var c=document.querySelector('.tab-pane[data-value=\"content\"] .sieve-content-ask');",
      "var b=document.getElementById('content-show_example');",
      "return {h:c.getBoundingClientRect().height, bh:b.getBoundingClientRect().height,",
      "txt:c.textContent.replace(/\\s+/g,' ').trim().slice(0,120)}})()"
    ))
    expect_gt(ask$h, 120)
    expect_gt(ask$bh, 20)
    expect_match(ask$txt, "^What this tab asks for: expert ratings of the items An expert rating is one judge")
    expect_identical(as.integer(app$get_js("document.querySelectorAll('#content-demo').length")), 0L)
    expect_identical(as.integer(app$get_js("document.querySelectorAll('#content-file').length")), 1L)
    expect_identical(as.integer(app$get_js("document.querySelectorAll('#content-dl_template').length")), 1L)
    app$click("content-show_example")
    siever_wait_render(app, timeout = 30 * 1000)
    app$wait_for_js("document.querySelector('.modal.show img') !== null && document.querySelector('.modal.show img').complete",
      timeout = 30 * 1000
    )
    img <- app$get_js(paste0(
      "(function(){var i=document.querySelector('.modal.show img');",
      "return {w:i.naturalWidth,h:i.naturalHeight,src:i.getAttribute('src')}})()"
    ))
    expect_identical(img$src, "www/content_validity_example.png")
    expect_gt(img$w, 1000)
    expect_gt(img$h, 400)
    title <- app$get_js("document.querySelector('.modal.show .modal-title').textContent.trim()")
    expect_identical(title, "An example of a filled expert rating sheet")
    app$click(selector = ".modal.show .modal-footer button")
    siever_wait_render(app, timeout = 30 * 1000)

    # The optional sentence opens each of the optional tabs.
    for (tab in c("content", "optimize", "bifactor", "network", "irt", "sem", "repro")) {
      expect_identical(siever_open_tab(app, tab), tab)
      txt <- app$get_js(sprintf(
        "document.querySelector('.tab-pane[data-value=\"%s\"] .sieve-optional-tab').textContent.replace(/\\s+/g,' ').trim()",
        tab
      ))
      expect_match(txt, "^This tab is optional. Nothing in another tab depends on it")
    }

    # With the bundled example loaded, the illustrative button is offered.
    app$click("ingest-load")
    siever_wait_render(app, timeout = 60 * 1000)
    expect_identical(siever_open_tab(app, "content"), "content")
    app$wait_for_js("document.querySelectorAll('#content-demo').length === 1", timeout = 30 * 1000)
    expect_identical(as.integer(app$get_js("document.querySelectorAll('#content-demo').length")), 1L)
    # The ledger table's Status column says included and excluded, as the
    # Items tab does, before and after the selected items are excluded.
    app$click("content-demo")
    siever_wait_render(app, timeout = 30 * 1000)
    app$click(selector = ".tab-pane[data-value='content'] ul.nav a.nav-link[data-value=\"Write to the Item Ledger\"]")
    siever_wait_render(app, timeout = 30 * 1000)
    column_cells_js <- function(header) {
      sprintf(paste0(
        "(function(){var rows=Array.from(document.querySelectorAll('#content-ledger_tbl table tbody tr'));",
        "if(!rows.length)return [];var table=rows[0].closest('table');",
        "var columns=Array.from(table.querySelectorAll('thead th'));",
        "var index=columns.findIndex(function(h){return h.textContent.trim()===%s;});",
        "if(index<0)throw new Error('Expected content ledger column is absent');",
        "return rows.map(function(row){return row.cells[index].textContent.trim();});})()"
      ), compat_js_string(header))
    }
    column_cells <- function(header) {
      as.character(unlist(app$get_js(column_cells_js(header))))
    }
    status_cells <- function() column_cells("Status")
    app$wait_for_js("document.querySelectorAll('#content-ledger_tbl table tbody tr td').length > 0",
      timeout = 30 * 1000
    )
    expect_identical(status_cells(), rep("included", 9L))
    expect_identical(
      column_cells("Match review"),
      rep("proposed: review before ledger actions", 9L)
    )
    app$click("content-keep_all_matches")
    siever_wait_render(app)
    expect_identical(column_cells("Match review"), rep("confirmed", 9L))
    # The researcher selects the rows (no I-CVI cutoff does); DT selects a row
    # on mousedown (shift for a second row is not needed in the multiple mode:
    # each press toggles its row).
    app$run_js(paste0(
      "var rows = document.querySelectorAll('#content-ledger_tbl table tbody tr');",
      "[6, 8].forEach(function(i){rows[i].dispatchEvent(new MouseEvent('mousedown', {bubbles: true}));});"
    ))
    app$wait_for_js(
      "document.querySelectorAll('#content-ledger_tbl table tbody tr.selected').length === 2",
      timeout = 30 * 1000
    )
    siever_wait_render(app)
    app$click("content-park_selected")
    # The exclusion lists the items in a dialog and excludes them on its
    # confirmation.
    app$wait_for_js("document.querySelectorAll('#content-park_selected_confirm').length === 1", timeout = 30 * 1000)
    app$click("content-park_selected_confirm")
    siever_wait_render(app, timeout = 30 * 1000)
    app$wait_for_js(
      paste0(
        column_cells_js("Status"),
        ".some(function(value){return value === 'excluded';})"
      ),
      timeout = 30 * 1000
    )
    cells <- status_cells()
    expect_identical(sort(unique(cells)), c("excluded", "included"))
    expect_identical(sum(cells == "excluded"), 2L)
    expect_false(any(grepl("parked|active", cells)))
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

# ---- 3b. The navy header and the accent -------------------------------------

test_that("the header is the navy of the default palette, the lighter blue is the accent, and every header text passes 4.5 to 1", {
  chrome <- sieve_chrome_colors()
  expect_identical(names(chrome), c("navy", "accent", "copper", "go"))
  expect_identical(chrome[["navy"]], "#0c2340")
  expect_identical(chrome[["navy"]], sieve_palettes()$nd[1])
  expect_identical(chrome[["accent"]], "#243e8b")
  # The theme: the navbar background is the navy, the primary color (links,
  # buttons) is the accent.
  theme <- sieve_theme()
  # (bslib reports a Sass variable's hex in upper case.)
  expect_identical(tolower(unname(bslib::bs_get_variables(theme, "navbar-bg"))), chrome[["navy"]])
  expect_identical(tolower(unname(bslib::bs_get_variables(theme, "primary"))), chrome[["accent"]])
  expect_identical(tolower(unname(bslib::bs_get_variables(theme, "secondary"))), chrome[["copper"]])
  # The stylesheet repeats both, draws the accent on the active sub-tab and
  # the value box border, and its header text colors pass the rule.
  css <- compat_css()
  expect_identical(tolower(compat_css_var(css, "--sieve-navbar-bg")), chrome[["navy"]])
  expect_identical(tolower(compat_css_var(css, "--sieve-accent")), chrome[["accent"]])
  expect_match(
    gsub(
      "\\s+",
      " ",
      css
    ),
    ".nav-tabs .nav-link.active { box-shadow: inset 0 -3px 0 var(--sieve-accent); }",
    fixed = TRUE
  )
  expect_match(gsub("\\s+", " ", css), ".bslib-value-box { border-color: var(--sieve-accent); }", fixed = TRUE)
  bg <- compat_css_var(css, "--sieve-navbar-bg")
  ratios <- vapply(
    c(
      active = "--sieve-navbar-fg", hover = "--sieve-navbar-fg-hover",
      rest = "--sieve-navbar-fg-rest", optional = "--sieve-navbar-fg-optional"
    ),
    function(v) compat_contrast(compat_css_var(css, v), bg), numeric(1)
  )
  expect_true(all(ratios >= 4.5), info = paste(names(ratios), round(ratios, 2), collapse = "; "))
  # The values of the suite's own arithmetic (compat_contrast(): alpha
  # composited over the header before the luminance is taken).
  expect_equal(unname(ratios), c(15.7885, 10.4907, 7.3708, 5.6722), tolerance = 1e-4)
  expect_true(ratios[["active"]] > ratios[["hover"]] && ratios[["hover"]] > ratios[["rest"]] &&
    ratios[["rest"]] > ratios[["optional"]])
  # The accent on white (links, the sub-tab underline) and the busy
  # indicators in the navy and the gold of the same palette.
  expect_equal(compat_contrast(chrome[["accent"]], "#ffffff"), 9.8274, tolerance = 1e-4)
  ui <- as.character(app_ui(NULL))
  expect_match(ui, "--shiny-spinner-color:#0C2340", fixed = TRUE)
  # The palette is labeled Default and holds the same five colors.
  expect_identical(names(sieve_palette_choices())[1], "Default")
  expect_identical(sieve_palettes()$nd, c("#0c2340", "#d39f10", "#0a843d", "#1c4f8f", "#ae9142"))
})

test_that("no user-facing string names Notre Dame, ND or Marian; the stored palette name is nd, with its five hex values", {
  root <- interface_layout_source_root()
  skip_if(is.null(root), "not running from the package source")
  files <- c(
    list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
    list.files(file.path(root, "inst", "app", "www"), pattern = "[.](css|js|html)$", full.names = TRUE),
    list.files(file.path(root, "vignettes"), pattern = "[.]Rmd$", full.names = TRUE),
    list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE),
    file.path(root, "README.md")
  )
  pattern <- "Notre Dame|ND inspired|ND-inspired|Marian|\\bND\\b"
  hits <- unlist(lapply(files, function(f) {
    lines <- readLines(f, warn = FALSE)
    at <- grep(pattern, lines, perl = TRUE)
    if (!length(at)) return(character(0))
    sprintf("%s:%d: %s", basename(f), at, trimws(lines[at]))
  }))
  # The one allowance: the palette source comment that keeps the attribution
  # (a plain comment, not roxygen, in utils_theme.R).
  allowed <- grepl("^utils_theme[.]R:[0-9]+: # ", hits)
  expect_identical(hits[!allowed], character(0))
  # The internal name is the stored value, and the hex values are the same
  # five.
  expect_identical(shiny::isolate(sieve_new_state()$palette), "nd")
  expect_identical(sieve_palette_colors("nd", 1L), "#0c2340")
})

test_that("in the browser at 1,280 and 1,600 px the header is navy, the optional labels are lighter and readable, More follows CV and Invariance, and the accent marks the active sub-tab and the value boxes", {
  skip_if_no_browser()
  chrome <- sieve_chrome_colors()
  for (width in c(1280L, 1600L)) {
    compat_with_app(width = width, height = if (width == 1280L) 800L else 1000L, load_example = TRUE, function(app) {
      # All links are visible only under the explicitly empirical fixture.
      siever_use_empirical_basis(app)
      info <- sprintf("at %d px", width)
      col <- compat_navbar_colors(app)
      expect_identical(compat_parse_color(col$background), compat_parse_color(chrome[["navy"]]), info = info)
      expect_equal(compat_contrast(col$rest, col$background), 7.3708, tolerance = 1e-3, info = info)
      expect_equal(compat_contrast(col$active, col$background), 15.7885, tolerance = 1e-3, info = info)
      # Every workspace exposes its pages within the viewport, with readable
      # optional labels on their actual light background.
      for (ws in names(sieve_workspaces())) {
        app$click(selector = sprintf("nav.navbar a[data-value='%s']", ws))
        siever_wait_render(app)
        links <- app$get_js(sprintf(paste0(
          "Array.from(document.querySelectorAll('#ws_%s a[data-value]')).map(function(a){",
          "var r=a.getBoundingClientRect(),e=a,bg;while(e){bg=getComputedStyle(e).backgroundColor;",
          "if(bg!=='rgba(0, 0, 0, 0)'&&bg!=='transparent')break;e=e.parentElement;}",
          "return {value:a.dataset.value,optional:!!a.querySelector('.sieve-nav-optional'),",
          "color:getComputedStyle(a).color,background:bg,left:r.left,right:r.right,visible:r.width>0&&r.height>0}})"), ws))
        values <- vapply(links, function(x) x$value, character(1))
        expect_identical(values, sieve_workspaces()[[ws]]$pages, info = info)
        expect_true(all(vapply(links, function(x) isTRUE(x$visible), logical(1))), info = info)
        expect_true(all(vapply(links, function(x) x$left >= -1 && x$right <= width + 1, logical(1))), info = info)
        for (link in links) expect_gte(compat_contrast(link$color, link$background), 4.5)
      }
      # The optional sentence on an optional tab, and the accent: the active
      # sub-tab of the Screening tab carries the underline, the CFA tab's
      # value boxes the border.
      expect_identical(siever_open_tab(app, "screen"), "screen")
      shadow <- app$get_js("getComputedStyle(document.querySelector('.tab-pane.active .nav-tabs .nav-link.active')).boxShadow")
      expect_match(shadow, "rgb(36, 62, 139)", fixed = TRUE, info = info)
      expect_identical(siever_open_tab(app, "cfa"), "cfa")
      border <- app$get_js("getComputedStyle(document.querySelector('.tab-pane.active .bslib-value-box')).borderTopColor")
      expect_identical(compat_parse_color(border), compat_parse_color(chrome[["accent"]]), info = info)
      expect_identical(siever_open_tab(app, "bifactor"), "bifactor")
      txt <- app$get_js("document.querySelector('.tab-pane[data-value=\"bifactor\"] .sieve-optional-tab').textContent.replace(/\\s+/g,' ').trim()")
      expect_match(txt, "^This tab is optional. Nothing in another tab depends on it", info = info)
      expect_identical(compat_output_errors(app), character(0), info = info)
    })
  }
})

# ---- 4. The screening sentence ---------------------------------------------

test_that("the screening sentence states its claim without a citation and says exclude", {
  # The ampersand of the citation is escaped in the rendered tags.
  html <- gsub("&amp;", "&", interface_layout_text(mod_screen_ui("screen")), fixed = TRUE)
  expect_match(html,
    "Screening is diagnostic, not confirmatory: it informs which items to retain, revise, or exclude before structure is estimated.",
    fixed = TRUE
  )
  expect_false(grepl("or exclude before structure is estimated (Clark & Watson, 1995)", html, fixed = TRUE))
  expect_false(grepl("or park before structure is estimated", html, fixed = TRUE))
  # The sentence is in the tab's opening paragraph, not a later panel.
  # Results first, so the opening paragraph follows the sub-tabs; the
  # sentence is in it.
  opening <- regexpr("Preliminary item analysis.", html, fixed = TRUE)
  sentence <- regexpr("Screening is diagnostic, not confirmatory", html, fixed = TRUE)
  first_panel <- regexpr("Per item descriptive statistics", html, fixed = TRUE)
  expect_true(opening > 0 && sentence > opening && first_panel < opening)
  # The reference is cited where it anchors a method on the tab (the
  # corrected item total panel), not in this sentence.
  expect_match(html, "(Clark & Watson, 1995)", fixed = TRUE)
})

# ---- 5. The Item-total table on the Screening tab --------------------------

test_that("in the browser the Item-total table is in full view inside the body of its sub-tab", {
  skip_if_no_browser()
  # At 1600 by 1000 px the Item-total table fits its sub-tab, so every row
  # must be in view. The panel's card keeps its own height: as a fill item
  # it would be a second scrolling box inside the body of the sub-tab, 10 px
  # shorter than the table, with the last row below its fold. The body of
  # the sub-tab is the one scrolling box.
  compat_with_app(load_example = TRUE, function(app) {
    idle <- function() siever_wait_render(app, timeout = 600 * 1000)
    expect_identical(siever_open_tab(app, "screen"), "screen")
    idle()
    expect_true("Item-total" %in% compat_subtabs(app, "screen"))
    compat_open_subtab(app, "screen", "Item-total")
    idle()
    m <- compat_measure_tables(app)
    m <- m[m$id == "screen-itc_tbl", , drop = FALSE]
    expect_identical(nrow(m), 1L)
    expect_identical(m$rows, 9L)
    expect_true(app$get_js("document.querySelector('.sieve-workspace-head').offsetParent !== null"))
    # Scrolled to the top of its sub-tab, every row is in view, the table
    # ends inside its box, and no box between it and the sub-tab is short.
    expect_identical(m$visible, m$rows, info = jsonlite::toJSON(m))
    expect_lte(m$spill, 1L)
    expect_identical(m$squeezed, "")
    # Nothing between the table and the body of its sub-tab clips its own
    # content.
    clipping <- app$get_js(paste0(
      "(function(){var w=document.getElementById('screen-itc_tbl');",
      "var region=w.closest('.tab-pane').querySelector(':scope > .card-body');",
      "var out=[];var a=w.parentElement;",
      "while(a && a!==region && a!==document.body){var cs=getComputedStyle(a);",
      "if(cs.overflowY!=='visible' && (a.scrollHeight-a.clientHeight)>1)",
      " out.push(String(a.className).split(' ').slice(0,2).join('.')+' '+a.clientHeight+'/'+a.scrollHeight);",
      "a=a.parentElement;}return out;})()"
    ))
    expect_identical(length(unlist(clipping)), 0L,
      info = paste(unlist(clipping), collapse = " ; ")
    )
    expect_identical(compat_output_errors(app), character(0))
  })
})

# ---- 6. Plain words: exclude, never park ---------------------------------------

test_that("the Screening tab and the Content Validity exclusion note say exclude, never park", {
  # The interface speaks of included and excluded items, never of parked
  # ones: the Item-total panel's help and the recorded annotation of the
  # content exclusion say what happens in the house word, and the stored
  # value is named only as the ledger status it is.
  html <- gsub("&amp;", "&", interface_layout_text(mod_screen_ui("screen")), fixed = TRUE)
  expect_match(html, "revision, reverse scoring, or exclusion (Clark & Watson, 1995).", fixed = TRUE)
  expect_false(grepl("\\bpark(ing|ed)?\\b", html))

  skip_if_not_installed("DMAR")
  st <- interface_layout_hs9_state()
  shiny::testServer(mod_content_server, args = list(state = st), {
    session$setInputs(relevant = c("3", "4"), conf_level = 0.95)
    session$setInputs(demo = 1)
    session$setInputs(keep_all_matches = 1)
    session$setInputs(ledger_tbl_rows_selected = c(7L, 9L))
    session$setInputs(park_selected = 1)
    session$setInputs(park_selected_confirm = 1)
    # (The exclusion is sieve_set_item_status()'s recorded step, which says
    # drop, with the reason, and the recorded R says excluded.)
    note <- grep("^# \\[[0-9]+\\] Drop 1 item from further consideration", state$codelog, value = TRUE)
    expect_length(note, 2L)
    expect_true(all(grepl("(reason: Content validity: I-CVI", note, fixed = TRUE)))
    expect_false(any(grepl("rows are parked", state$codelog, fixed = TRUE)))
    expect_true(any(grepl("meta$status", state$codelog, fixed = TRUE)))
    # The stored value is the ledger status "parked".
    expect_identical(
      sort(state$meta$item[state$meta$status == "parked"]),
      c("t10_addition", "t13_straight_and_curved_capitals")
    )
    expect_true(any(grepl('meta$status[meta$item %in% c("t10_addition")] <- "parked"', state$codelog, fixed = TRUE)))
  })
})

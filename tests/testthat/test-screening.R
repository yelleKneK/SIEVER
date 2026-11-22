# Behavioral tests for the Screening tab (mod_screen.R): what it plots, what
# it only summarizes, how it sizes the correlation heatmap and the
# missingness plot, what its downloads write, and that the display controls
# recompute nothing. The item pools are a private study (500 respondents by
# 68 ordered items, one construct with six facets, one identifier column; a
# SIEVER project file named by SIEVER_ETHICS_PROJECT or found under
# SIEVER_PRIVATE_PROFILE_DIR, so those tests skip where it is absent) and the
# bundled nine Holzinger and Swineford tests (n = 301, three constructs, five
# columns that are not items).
#
# Author: Ken Kelley

skip_if_not_installed("DMAR")
skip_if_not_installed("psych")
skip_if_not_installed("shiny")

# The private study: a SIEVER project file, opened the way the Ingest tab
# opens one.
screen_private_path <- function() {
  Sys.getenv("SIEVER_ETHICS_PROJECT", siever_profile_file("ethics.siever.rds"))
}

screen_private_state <- function() {
  path <- screen_private_path()
  skip_if_not(
    file.exists(path),
    "the private study of 68 items (SIEVER_ETHICS_PROJECT, or SIEVER_PRIVATE_PROFILE_DIR) is not available"
  )
  # The items are haven_labelled columns (the data were read from a .sav).
  # Reading the project calls as.numeric() on them (sieve_project_check),
  # which fails when vctrs is loaded and haven is not; the session that
  # produced the project had haven loaded, and so does this one.
  skip_if_not_installed("haven")
  loadNamespace("haven")
  snap <- sieve_project_read(path)
  s <- sieve_new_state()
  sieve_project_restore(s, snap, source = basename(path))
  s
}

# A loaded, confirmed state on the primary nine tests, with the five columns
# that are not items (id, sex, grade, age_years, school) in the data as read.
screen_hs9_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = TRUE, name = fx$name)
  s
}

# Flatten a renderUI or renderDT payload to a searchable string.
screen_text <- function(x) {
  if (is.null(x)) return("")
  paste(as.character(unlist(x)), collapse = " ")
}

# Count the polychoric matrices psych computes, whoever asks for them: the
# engine is traced in its namespace for the rest of the calling test.
screen_count_polychoric <- function(env = parent.frame()) {
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  utils::capture.output(suppressMessages(trace(
    "polychoric",
    where = asNamespace("psych"), print = FALSE,
    tracer = bquote(assign("n", get("n", envir = .(counter)) + 1L, envir = .(counter)))
  )))
  withr::defer(utils::capture.output(suppressMessages(
    untrace("polychoric", where = asNamespace("psych"))
  )), envir = env)
  counter
}

# ---- Items only ------------------------------------------------------------

test_that("item values are read without dispatching on the column class", {
  skip_if_not_installed("vctrs")
  # A researcher's session has vctrs loaded (ggplot2 loads it); haven need
  # not be, since a project file is read without it.
  loadNamespace("vctrs")
  x <- structure(c(1, 2, NA, 4),
    class = c("haven_labelled", "vctrs_vctr", "double"),
    labels = c(yes = 1, no = 2), label = "An item"
  )
  expect_identical(sieve_screen_numeric(x), c(1, 2, NA, 4))
  expect_null(attributes(sieve_screen_numeric(x)))
  expect_identical(sieve_screen_numeric(factor(c("3", "1", "2"))), c(3, 1, 2))
  expect_identical(sieve_screen_numeric(c(TRUE, FALSE)), c(1, 0))
  expect_identical(sieve_screen_numeric(c("a", "2")), c(NA, 2))
  expect_identical(sieve_screen_storage(x), "number")
  expect_identical(sieve_screen_storage(c("a", "b")), "text")
  expect_identical(sieve_screen_storage(factor("a")), "category")
})

test_that("items are ordered by construct then facet in order of first appearance", {
  m <- data.frame(
    item = c("a", "b", "c", "d", "e", "f"),
    construct = c("Second", "First", "Second", "First", "First", "Second"),
    facet = c("y", "p", "x", "q", "p", "y"),
    stringsAsFactors = FALSE
  )
  # Second comes first because it appears first; within Second, facet y
  # before x for the same reason; within a facet the ledger order is kept.
  expect_identical(sieve_screen_item_order(m), c("a", "f", "c", "b", "e", "d"))
  expect_identical(sieve_screen_item_order(m[0, ]), character(0))
  m$facet <- NA_character_
  expect_identical(sieve_screen_item_order(m), c("a", "c", "f", "b", "d", "e"))
})

test_that("nine tests: the five columns that are not items are summarized and plotted nowhere", {
  st <- screen_hs9_state()
  fx <- sieve_hs9_fixture()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    it <- included_items()
    expect_setequal(it, fx$item_cols)
    # Construct order as the ledger has it (Visual, Textual, Speed), not
    # alphabetical.
    expect_identical(it, unlist(fx$theory_map, use.names = FALSE))
    o <- other_vars()
    expect_setequal(o$Variable, fx$id_vars)
    expect_true(all(o$Role %in% c("identifier", "group", "criterion", "auxiliary")))
    expect_identical(o$n + o$Missing, rep(301L, nrow(o)))
    expect_false(any(o$Variable %in% desc()$Item))
    expect_false(any(o$Variable %in% miss()$item))
    expect_identical(desc()$Item, it)
    expect_identical(miss()$item, it)
    note <- screen_text(output$pool_note)
    expect_match(note, "Showing 9 included items of the 9 in the item pool.", fixed = TRUE)
    expect_match(note, "5 other variables in the data", fixed = TRUE)
    expect_match(note, "not plotted", fixed = TRUE)
    # The table is served server side, so its payload carries the headers
    # and the rows stay in other_vars().
    tbl <- screen_text(output$other_tbl)
    for (header in c("Variable", "Role", "Stored as", "Missing", "Distinct values")) {
      expect_match(tbl, header, fixed = TRUE)
    }
    expect_identical(o$Type[o$Variable == "school"], sieve_screen_storage(fx$data$school))
    # An excluded item leaves every view, and the note counts it.
    m <- shiny::isolate(state$meta)
    m$status[m$item == "t2_cubes"] <- "parked"
    state$meta <- m
    session$flushReact()
    expect_false("t2_cubes" %in% included_items())
    expect_false("t2_cubes" %in% desc()$Item)
    expect_match(screen_text(output$pool_note),
      "Showing 8 included items of the 9 in the item pool (1 excluded).",
      fixed = TRUE
    )
  })
})

test_that("a private study of 68 items: every plot and table covers the 68 included items and nothing else", {
  st <- screen_private_state()
  if (requireNamespace("vctrs", quietly = TRUE)) loadNamespace("vctrs")
  shiny::testServer(mod_screen_server, args = list(state = st), {
    it <- included_items()
    expect_length(it, 68L)
    # The ledger is already grouped by facet, so its order is the plot order.
    expect_identical(it, shiny::isolate(state$meta$item))
    expect_false("response_id" %in% it)
    o <- other_vars()
    expect_identical(o$Variable, "response_id")
    expect_identical(o$Role, "identifier")
    expect_identical(o$n, 500L)
    expect_identical(o$Distinct, 500L)
    note <- screen_text(output$pool_note)
    expect_match(note, "Showing 68 included items of the 68 in the item pool.", fixed = TRUE)
    expect_match(note, "The 1 other variable in the data (1 identifier) is summarized", fixed = TRUE)
    expect_identical(desc()$Item, it)
    expect_identical(miss()$item, it)
    expect_identical(sort(colnames(active_data())), sort(it))
    # Distinct values per item, and no small multiples for 68 items: the
    # table is the per item view and the panel says so.
    expect_identical(
      desc()$distinct,
      vapply(active_data(), function(x) length(unique(x[!is.na(x)])), integer(1),
        USE.NAMES = FALSE
      )
    )
    expect_null(multiples())
    expect_match(screen_text(output$desc_plot_ui),
      "With 68 included items the per item plots are replaced by the table above",
      fixed = TRUE
    )
    # The missingness plot draws all 68 items (a fill scale of 64 colors
    # would fail above that), one bar per item at the heatmap's height.
    expect_no_error(output$miss_plot)
    expect_equal(output$miss_plot$height, sieve_screen_heatmap_size(68)$height)
    expect_match(screen_text(output$miss_plot_ui),
      sprintf("height:%dpx", sieve_screen_heatmap_size(68)$height),
      fixed = TRUE
    )
    # The items are haven_labelled columns: the per respondent screens read
    # them whatever is loaded.
    r <- careless()
    expect_identical(r$n_item, 68L)
    expect_identical(nrow(r$tbl), 500L)
    expect_no_error(output$careless_summary)
    expect_no_error(output$desc_tbl)
  })
})

# ---- The heatmap -----------------------------------------------------------

test_that("the heatmap size keeps every tile at 14 px or more and grows with the item count", {
  counts <- c(2L, 9L, 17L, 18L, 25L, 50L, 68L, 120L, 300L)
  sizes <- lapply(counts, sieve_screen_heatmap_size)
  cells <- vapply(sizes, function(s) s$cell, numeric(1))
  widths <- vapply(sizes, function(s) s$width, numeric(1))
  heights <- vapply(sizes, function(s) s$height, numeric(1))
  expect_true(all(cells >= 14))
  expect_true(all(diff(widths) >= 0))
  expect_true(all(diff(heights) >= 0))
  # Nine items: 40 px tiles; 68 items: 14 px tiles; 300 items: 14 px tiles.
  expect_equal(sizes[[2]]$cell, 40)
  expect_equal(sizes[[7]]$cell, 14)
  expect_equal(sizes[[9]]$cell, 14)
  expect_equal(sizes[[7]]$width, 68 * 14 + 190 + 90)
  expect_equal(sizes[[7]]$height, 68 * 14 + 190 + 30)
  expect_gt(sizes[[7]]$width, sizes[[2]]$width)
  expect_gt(sizes[[9]]$width, sizes[[7]]$width)
  # The same geometry in inches, for the downloads.
  expect_equal(sizes[[7]]$width_in, sizes[[7]]$width / 96)
  expect_equal(sizes[[7]]$mai, c(190, 190, 30, 90) / 96)
  expect_identical(
    sieve_screen_short_label(c("A long item name indeed, yes", NA, "short"), 10L),
    c("A long ...", "", "short")
  )
})

test_that("sieve_correlation_heatmap draws the matrix in its order, grays below the highlight, and separates the blocks", {
  R <- stats::cor(attitude)
  groups <- data.frame(
    construct = rep(c("Climate", "Advancement"), c(4, 3)),
    facet = c(
      "Overall", "Overall", "Treatment", "Treatment",
      "Pay", "Pay", "Prospects"
    ),
    stringsAsFactors = FALSE
  )
  draw <- function(...) {
    f <- withr::local_tempfile(fileext = ".pdf")
    grDevices::pdf(f, width = 7, height = 7)
    tryCatch(sieve_correlation_heatmap(...), finally = grDevices::dev.off())
  }
  drawn <- draw(R, groups = groups, highlight = 0.5)
  expect_identical(drawn$items, colnames(attitude))
  expect_identical(drawn$n, 7L)
  expect_equal(drawn$highlight, 0.5)
  # Every off diagonal tile below .5 in absolute value is gray, both halves.
  expect_identical(drawn$faded, sum(abs(R[upper.tri(R)]) < 0.5) * 2L)
  # A facet line after 2 and 6; the construct line after 4 takes the place
  # of the facet line there.
  expect_identical(
    drawn$separators,
    data.frame(
      after = c(2L, 4L, 6L), kind = c("facet", "construct", "facet"),
      stringsAsFactors = FALSE
    )
  )
  # No highlight: nothing gray; no groups: no separators.
  plain <- draw(R)
  expect_identical(plain$faded, 0L)
  expect_identical(nrow(plain$separators), 0L)
  # Labels by name, cut to the width, and the dimnames otherwise.
  named <- draw(R, labels = c(rating = "The overall rating of the department"), label_width = 12L)
  expect_identical(named$labels[1], "The overa...")
  expect_identical(named$labels[-1], colnames(attitude)[-1])
  expect_error(sieve_correlation_heatmap(matrix(1:6, 2)), "square numeric correlation matrix")
  expect_error(sieve_correlation_heatmap(R, labels = "x"), "one entry per item")
  expect_null(grDevices::dev.list())
})

test_that("nine tests: the heatmap is the Pearson matrix of the complete cases, in construct order, at 40 px tiles", {
  st <- screen_hs9_state()
  fx <- sieve_hs9_fixture()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    it <- included_items()
    cm <- cor_mat()
    expect_identical(cm$kind, "pearson")
    expect_identical(cm$requested, "pearson")
    expect_identical(rownames(cm$R), it)
    # Against plain R on the same complete cases.
    X <- fx$data[, it]
    X <- X[stats::complete.cases(X), ]
    expect_equal(cm$R, stats::cor(X), tolerance = 1e-12)
    expect_match(screen_text(output$cor_note),
      "Matrix shown: Pearson correlations among 9 active items, on the observed item scores. Complete cases: n = 301 of 301.",
      fixed = TRUE
    )
    s <- heat_size()
    expect_identical(s$n, 9L)
    expect_equal(s$cell, 40)
    expect_lt(s$width, sieve_screen_heatmap_size(68)$width)
    expect_match(screen_text(output$cor_plot_ui), sprintf("width:%dpx", s$width), fixed = TRUE)
    img <- output$cor_plot
    expect_equal(img$width, s$width)
    expect_equal(img$height, s$height)
    # Construct separators after Visual (3) and Textual (6); no facets.
    a <- heat_args()
    f <- withr::local_tempfile(fileext = ".pdf")
    grDevices::pdf(f)
    drawn <- tryCatch(sieve_correlation_heatmap(a$R, labels = a$labels, groups = a$groups),
      finally = grDevices::dev.off()
    )
    expect_identical(drawn$separators$after, c(3L, 6L))
    expect_identical(drawn$separators$kind, c("construct", "construct"))
    expect_identical(unname(a$labels), unname(fx$labels[it]))
    # The key lists the items in the order drawn.
    expect_identical(item_info()$item, it)
    expect_no_error(output$cor_key)
  })
})

test_that("a private study of 68 items: one shared polychoric matrix, a heatmap sized to 68 items, and display controls that recompute nothing", {
  st <- screen_private_state()
  # An independent reference for three of the items, computed before the
  # engine is counted: a pair's polychoric correlation does not depend on
  # the other items.
  first_three <- shiny::isolate(st$meta$item)[1:3]
  three <- as.data.frame(lapply(shiny::isolate(st$items)[, first_three], sieve_screen_numeric))
  ref <- withr::with_options(
    list(mc.cores = 1L),
    psych::polychoric(three, correct = 0.5, progress = FALSE)$rho
  )
  n_poly <- screen_count_polychoric()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    it <- included_items()
    # The matrix of a reopened project, and a polychoric matrix estimated
    # above two seconds, waits for Compute the correlations (basis_go, in
    # the matrix note).
    expect_match(screen_text(output$cor_note), "The correlations have not been computed.", fixed = TRUE)
    expect_identical(n_poly$n, 0L)
    started <- proc.time()[["elapsed"]]
    session$setInputs(basis_go = 1L)
    html <- screen_text(output$cor_plot_ui)
    img <- output$cor_plot
    first_open <- proc.time()[["elapsed"]] - started
    expect_identical(n_poly$n, 1L)
    cm <- cor_mat()
    expect_identical(cm$kind, "polychoric")
    expect_identical(cm$requested, "polychoric")
    expect_identical(rownames(cm$R), it)
    expect_identical(colnames(cm$R), it)
    expect_match(screen_text(output$cor_note),
      "Matrix shown: polychoric correlations among 68 active items, estimated from the ordered responses (psych). Complete cases: n = 500 of 500.",
      fixed = TRUE
    )
    expect_identical(it[1:3], first_three)
    expect_equal(unname(cm$R[first_three, first_three]), unname(ref), tolerance = 1e-8)
    # Sized to the matrix: 14 px tiles, 1232 by 1172 px, on screen and in
    # the rendered image.
    s <- heat_size()
    expect_identical(s$n, 68L)
    expect_equal(s$cell, 14)
    expect_match(html, sprintf("width:%dpx", s$width), fixed = TRUE)
    expect_match(html, sprintf("height:%dpx", s$height), fixed = TRUE)
    expect_equal(img$width, s$width)
    expect_equal(img$height, s$height)
    expect_gt(s$width, sieve_screen_heatmap_size(9)$width)
    # Facet separators at the five boundaries between the six facets.
    a <- heat_args()
    f <- withr::local_tempfile(fileext = ".pdf")
    grDevices::pdf(f)
    drawn <- tryCatch(sieve_correlation_heatmap(a$R, labels = a$labels, groups = a$groups),
      finally = grDevices::dev.off()
    )
    facet_sizes <- table(factor(a$groups$facet, levels = unique(a$groups$facet)))
    expect_identical(drawn$separators$after, as.integer(cumsum(facet_sizes))[-6])
    expect_identical(drawn$separators$kind, rep("facet", 5L))
    expect_identical(drawn$items, it)
    # The highlight control recolors only: no new matrix.
    session$setInputs(cor_highlight = 0.6)
    invisible(output$cor_plot)
    expect_equal(heat_args()$highlight, 0.6)
    expect_identical(n_poly$n, 1L)
    # A label edit recomputes nothing and the new name reaches the axis.
    m <- shiny::isolate(state$meta)
    m$label[1] <- "First item (edited)"
    state$meta <- m
    session$flushReact()
    started <- proc.time()[["elapsed"]]
    for (o in c(
      "pool_note", "desc_tbl", "miss_plot", "cor_note", "cor_plot", "cor_key",
      "itc_tbl", "careless_summary"
    )) {
      invisible(output[[o]])
    }
    second_open <- proc.time()[["elapsed"]] - started
    expect_identical(n_poly$n, 1L)
    expect_identical(unname(heat_args()$labels[1]), "First item (edited)")
    expect_lt(second_open, first_open / 4)
    # The matrix is recorded once, with the engine call that reproduces it.
    log <- shiny::isolate(st$codelog)
    expect_identical(
      sum(grepl("Screening: correlations among the 68 included items (requested polychoric, used polychoric); complete cases, n = 500 of 500.",
        log,
        fixed = TRUE
      )),
      1L
    )
    expect_true(any(grepl("psych::polychoric(active_items[complete_rows, , drop = FALSE], correct = 0.5, progress = FALSE)$rho",
      log,
      fixed = TRUE
    )))

    # The three downloads: the figure as shown (highlight .6, every item) at
    # 300 dots per inch and as a vector, and the matrix with its diagonal.
    files <- c(png = output$dl_png, pdf = output$dl_pdf, csv = output$dl_csv)
    expect_true(all(file.exists(files)))
    expect_identical(
      unname(basename(files)),
      paste0("siever_correlations_polychoric.", c("png", "pdf", "csv"))
    )
    expect_identical(as.integer(readBin(files[["png"]], "raw", 4L)), c(137L, 80L, 78L, 71L))
    expect_identical(rawToChar(readBin(files[["pdf"]], "raw", 4L)), "%PDF")
    # The PNG is the screen geometry at 300 dots per inch: the IHDR chunk
    # holds its width and height.
    ihdr <- readBin(files[["png"]], "raw", 24L)
    px <- function(bytes) sum(as.integer(bytes) * 256^(3:0))
    expect_equal(px(ihdr[17:20]), s$width_in * 300, tolerance = 1e-3)
    expect_equal(px(ihdr[21:24]), s$height_in * 300, tolerance = 1e-3)
    csv <- as.matrix(utils::read.csv(files[["csv"]],
      comment.char = "#", row.names = 1,
      check.names = FALSE
    ))
    expect_identical(rownames(csv), it)
    expect_identical(colnames(csv), it)
    expect_equal(csv, cor_mat()$R, tolerance = 1e-12)
    expect_identical(
      readLines(files[["csv"]], n = 1L),
      "# SIEVER: polychoric correlations among 68 included items (complete cases, n = 500 of 500); rows and columns in the order shown, diagonal included."
    )
    expect_identical(n_poly$n, 1L)
    # The recorded R writes the same figure: run it on the recorded matrix
    # and the ledger, and compare the PNG byte for byte.
    log <- shiny::isolate(st$codelog)
    code <- sieve_screen_heatmap_code(heat_args(), s, "siever_correlations_polychoric.png", "png")
    expect_true(all(code %in% log))
    expect_true(any(grepl("written as CSV, siever_correlations_polychoric.csv", log, fixed = TRUE)))
    env <- new.env(parent = globalenv())
    assign("R_items", cor_mat()$R, envir = env)
    assign("meta", shiny::isolate(state$meta), envir = env)
    withr::with_dir(withr::local_tempdir(), {
      eval(parse(text = code), envir = env)
      expect_identical(
        readBin("siever_correlations_polychoric.png", "raw", 1e7),
        readBin(files[["png"]], "raw", 1e7)
      )
      csv_code <- sieve_screen_csv_code(
        heat_args(), "siever_correlations_polychoric.csv",
        readLines(files[["csv"]], n = 1L)
      )
      expect_true(all(csv_code %in% log))
      eval(parse(text = csv_code), envir = env)
      expect_identical(readLines("siever_correlations_polychoric.csv"), readLines(files[["csv"]]))
    })
    # Scoped to one facet, the download is that block and its name says so.
    session$setInputs(cor_scope = "facet:Equality")
    scoped <- output$dl_csv
    expect_identical(basename(scoped), "siever_correlations_polychoric_facet_equality.csv")
    block <- as.matrix(utils::read.csv(scoped, comment.char = "#", row.names = 1, check.names = FALSE))
    equality <- item_info()$item[item_info()$facet == "Equality"]
    expect_identical(rownames(block), equality)
    expect_equal(block, cor_mat()$R[equality, equality], tolerance = 1e-12)
    expect_identical(n_poly$n, 1L)
    # A facet rename recomputes nothing either: the matrix does not depend
    # on the grouping, only the separators and the scope choices do.
    m <- shiny::isolate(state$meta)
    m$facet[m$facet == "Equality"] <- "Equality (renamed)"
    state$meta <- m
    session$flushReact()
    invisible(output$cor_plot)
    expect_identical(n_poly$n, 1L)
    expect_identical(heat_items(), it) # the renamed facet is no longer a scope: every item
    expect_identical(unique(item_info()$facet)[1], "Equality (renamed)")
  })
  expect_null(grDevices::dev.list())
})

test_that("nine tests: the scope selector shows one construct or one facet from the cached matrix", {
  st <- screen_hs9_state()
  fx <- sieve_hs9_fixture()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    full <- heat_args()
    expect_identical(heat_items(), included_items())
    expect_match(screen_text(output$cor_scope_note), "Showing all 9 included items.", fixed = TRUE)
    session$setInputs(cor_scope = "construct:Textual")
    expect_identical(heat_items(), fx$theory_map$Textual)
    a <- heat_args()
    expect_identical(rownames(a$R), fx$theory_map$Textual)
    expect_equal(a$R, full$R[fx$theory_map$Textual, fx$theory_map$Textual])
    expect_equal(heat_size()$n, 3L)
    expect_lt(heat_size()$width, sieve_screen_heatmap_size(9)$width)
    expect_match(screen_text(output$cor_scope_note),
      "Showing the 3 items of construct Textual (of 9 included).",
      fixed = TRUE
    )
    img <- output$cor_plot
    expect_equal(img$width, heat_size()$width)
    # A scope that does not exist falls back to every item.
    session$setInputs(cor_scope = "facet:none such")
    expect_identical(heat_items(), included_items())
    session$setInputs(cor_scope = "all")
    expect_identical(heat_items(), included_items())
  })
})

test_that("in the browser the heatmap is drawn at its requested size inside a box that scrolls", {
  skip_if_no_browser()
  siever_with_app(load_example = TRUE, function(app) {
    expect_identical(siever_open_tab(app, "screen"), "screen")
    app$click(selector = ".tab-pane[data-value='screen'] a.nav-link[data-value='Correlation heatmap']")
    siever_wait_render(app, timeout = 60 * 1000)
    app$wait_for_js("document.querySelector('#screen-cor_plot img') !== null", timeout = 60 * 1000)
    measure <- function() {
      app$get_js(paste0(
        "(function(){",
        "  const el = document.querySelector('#screen-cor_plot');",
        "  const img = el.querySelector('img');",
        "  const box = document.querySelector('#screen-cor_plot_ui').parentElement;",
        "  const cs = getComputedStyle(box);",
        "  return {box_height: el.getBoundingClientRect().height,",
        "          box_width: el.getBoundingClientRect().width,",
        "          img_height: img.getBoundingClientRect().height,",
        "          img_natural_height: img.naturalHeight,",
        "          img_natural_width: img.naturalWidth,",
        "          overflow: cs.overflowY, max_height: cs.maxHeight};",
        "})()"
      ))
    }
    size <- sieve_screen_heatmap_size(9)
    got <- measure()
    # The plot keeps the height it asked for (580 px for nine items), not
    # the pane's share of a fill layout, and the image is that size.
    expect_equal(got$box_height, size$height, tolerance = 0.02)
    expect_equal(got$box_width, size$width, tolerance = 0.02)
    expect_equal(got$img_height, size$height, tolerance = 0.02)
    expect_equal(got$img_natural_height, size$height, tolerance = 0.02)
    expect_equal(got$img_natural_width, size$width, tolerance = 0.02)
    expect_identical(got$overflow, "auto")
    expect_false(identical(got$max_height, "none"))
    # One construct: the figure shrinks to its three items.
    app$set_inputs(`screen-cor_scope` = "construct:Visual")
    siever_wait_render(app, timeout = 60 * 1000)
    small <- sieve_screen_heatmap_size(3)
    got <- measure()
    expect_equal(got$img_natural_height, small$height, tolerance = 0.02)
    expect_equal(got$box_height, small$height, tolerance = 0.02)
    expect_identical(siever_n_output_errors(app), 0L)
  })
})

test_that("nine tests: the downloads are named for the Pearson matrix and hold it", {
  st <- screen_hs9_state()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    files <- c(png = output$dl_png, pdf = output$dl_pdf, csv = output$dl_csv)
    expect_identical(
      unname(basename(files)),
      paste0("siever_correlations_pearson.", c("png", "pdf", "csv"))
    )
    expect_true(all(file.size(files) > 1000))
    csv <- as.matrix(utils::read.csv(files[["csv"]],
      comment.char = "#", row.names = 1,
      check.names = FALSE
    ))
    expect_equal(csv, cor_mat()$R, tolerance = 1e-12)
    expect_match(readLines(files[["csv"]], n = 1L),
      "# SIEVER: Pearson correlations among 9 included items (complete cases, n = 301 of 301)",
      fixed = TRUE
    )
    session$setInputs(cor_scope = "construct:Speed")
    expect_identical(basename(output$dl_pdf), "siever_correlations_pearson_construct_speed.pdf")
  })
  expect_null(grDevices::dev.list())
  expect_identical(sieve_screen_slug("Social trust"), "social_trust")
  expect_identical(sieve_screen_slug("  Value / congruence! "), "value_congruence")
  expect_identical(sieve_screen_slug(""), "unnamed")
  expect_identical(
    sieve_screen_file_name("polychoric", list(kind = "all", name = NA), "png"),
    "siever_correlations_polychoric.png"
  )
  expect_identical(
    sieve_screen_file_name("mixed", list(kind = "construct", name = "Ethics"), "csv"),
    "siever_correlations_mixed_construct_ethics.csv"
  )
})

# ---- Descriptives ----------------------------------------------------------

test_that("nine tests: small multiples of the nine items, and the distinct-values column", {
  st <- screen_hs9_state()
  fx <- sieve_hs9_fixture()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    it <- included_items()
    d <- desc()
    expect_identical(
      d$distinct,
      vapply(fx$data[, it], function(x) length(unique(x[!is.na(x)])), integer(1),
        USE.NAMES = FALSE
      )
    )
    expect_identical(names(d)[names(d) == "distinct"], "distinct")
    m <- multiples()
    expect_identical(names(m$data), it)
    expect_identical(m$layout, list(rows = 3L, cols = 4L, height = 3L * 190L + 10L))
    expect_identical(unname(m$labels), unname(sieve_screen_short_label(fx$labels[it], 30L)))
    html <- screen_text(output$desc_plot_ui)
    expect_match(html, "Response distribution of each of the 9 included items", fixed = TRUE)
    expect_match(html, sprintf("height:%dpx", m$layout$height), fixed = TRUE)
    img <- output$desc_plot
    expect_equal(img$height, m$layout$height)
    # What is drawn is exactly the included items.
    f <- withr::local_tempfile(fileext = ".pdf")
    grDevices::pdf(f)
    drawn <- tryCatch(sieve_screen_draw_multiples(m$data, m$labels, m$types), finally = grDevices::dev.off())
    expect_identical(drawn, it)
    expect_false(any(fx$id_vars %in% drawn))
    # Twenty one items: no plots.
    expect_identical(sieve_screen_multiples_layout(20L)$rows, 5L)
    expect_identical(sieve_screen_multiples_layout(1L), list(rows = 1L, cols = 1L, height = 200L))
  })
  expect_null(grDevices::dev.list())
})

# ---- Missingness -----------------------------------------------------------

test_that("nine tests: the missingness plot is sized like the heatmap and carries its separators", {
  st <- screen_hs9_state()
  fx <- sieve_hs9_fixture()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    it <- included_items()
    size <- sieve_screen_heatmap_size(9)
    expect_match(screen_text(output$miss_plot_ui), sprintf("height:%dpx", size$height), fixed = TRUE)
    img <- output$miss_plot
    expect_equal(img$height, size$height)
    df <- miss()
    expect_identical(df$item, it)
    expect_equal(df$pct_missing, 100 * colSums(is.na(fx$data[, it])) / nrow(fx$data),
      ignore_attr = TRUE
    )
    info <- item_info()
    f <- withr::local_tempfile(fileext = ".pdf")
    grDevices::pdf(f, width = size$width_in, height = size$height_in)
    drawn <- tryCatch(
      sieve_screen_draw_missing(
        df$pct_missing, sieve_screen_short_label(info$label),
        info[, c("construct", "facet")]
      ),
      finally = grDevices::dev.off()
    )
    expect_identical(drawn$n, 9L)
    expect_equal(drawn$upper, 5)
    expect_identical(drawn$separators$after, c(3L, 6L))
    expect_identical(drawn$separators$kind, c("construct", "construct"))
  })
  expect_null(grDevices::dev.list())
  # The separators of an item sequence: a construct line replaces a facet
  # line at the same place; nothing for one item or without groups.
  g <- data.frame(construct = c("A", "A", "B", "B", "B"), facet = c("x", "y", "y", "y", "z"))
  expect_identical(
    sieve_screen_separators(g, 5L),
    data.frame(
      after = c(1L, 2L, 4L), kind = c("facet", "construct", "facet"),
      stringsAsFactors = FALSE
    )
  )
  expect_identical(nrow(sieve_screen_separators(NULL, 5L)), 0L)
  expect_identical(nrow(sieve_screen_separators(g[1, , drop = FALSE], 1L)), 0L)
  # A pool with missing values: the axis limit follows the largest percent.
  f <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  drawn <- tryCatch(sieve_screen_draw_missing(c(0, 12.5, NA, 31), letters[1:4]),
    finally = grDevices::dev.off()
  )
  expect_equal(drawn$upper, 35)
  expect_null(grDevices::dev.list())
})

# ---- The help index -----------------------------------------------------------

test_that("every exported function has a place in the pkgdown reference index", {
  # _pkgdown.yml lists its reference topics explicitly, and pkgdown refuses to
  # build the site when an exported topic is missing from the list. Read with
  # base R only: the exports from NAMESPACE, the listed topics from the yml.
  # The yml is not part of a built package, so this runs on the source tree.
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  skip_if_not(file.exists(file.path(root, "_pkgdown.yml")), "_pkgdown.yml is not in this tree")
  ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
  methods <- sub("^S3method\\(([^,]+),([^)]+)\\)$", "\\1.\\2", grep("^S3method\\(", ns, value = TRUE))
  yml <- readLines(file.path(root, "_pkgdown.yml"), warn = FALSE)
  listed <- trimws(sub("^\\s*-\\s*", "", grep("^\\s+-\\s+[A-Za-z]", yml, value = TRUE)))
  expect_true("sieve_correlation_heatmap" %in% exports)
  # Several S3 methods can share one indexed help topic. Resolve the topic's
  # documented aliases so shared help pages are not listed repeatedly.
  help_files <- list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE)
  covered <- unique(unlist(lapply(help_files, function(file) {
    rd <- tools::parse_Rd(file)
    aliases <- vapply(Filter(function(node) identical(attr(node, "Rd_tag"), "\\alias"), rd),
      function(node) paste(unlist(node), collapse = ""), character(1))
    topic <- sub("[.]Rd$", "", basename(file))
    if (topic %in% listed || any(aliases %in% listed)) aliases else character()
  }), use.names = FALSE))
  expect_identical(setdiff(c(exports, methods), c(covered, "run_app")), character(0))
  expect_true("run_app" %in% listed)
})

# ---- The recorded R in a clean process ------------------------------------------

test_that("the recorded R of the Screening tab replays in a clean R process and writes the downloaded files", {
  skip_on_cran()
  root <- compat_source_root()
  # The nine tests through the real Ingest loader, so the load step is
  # recorded; the types confirm automatically (every item continuous).
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    suppressMessages(session$setInputs(load = 1L))
  })
  expect_true(shiny::isolate(st$confirmed))
  got <- new.env(parent = emptyenv())
  dir <- withr::local_tempdir()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    got$careless <- careless()$tbl
    got$R <- cor_mat()$R
    file.copy(output$dl_png, file.path(dir, "download.png"))
    file.copy(output$dl_csv, file.path(dir, "download.csv"))
  })
  shiny::testServer(mod_repro_server, args = list(state = st), {
    got$script <- script()
  })
  writeLines(got$script, file.path(dir, "siever_session.R"))
  # The script is run step by step in a new R process (nothing of this
  # session is there but what the script defines), and the first step that
  # fails is named. The package under test is loaded the way the browser
  # tests' launcher loads it.
  runner <- file.path(dir, "runner.R")
  writeLines(c(
    sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
    if (is.null(root)) "library(SIEVER)" else
      sprintf("pkgload::load_all(%s, quiet = TRUE, export_all = FALSE, helpers = FALSE)", deparse(root)),
    sprintf("setwd(%s)", deparse(dir)),
    "lines <- readLines('siever_session.R')",
    "stamps <- which(startsWith(lines, '# ['))",
    "starts <- c(1L, stamps); ends <- c(stamps - 1L, length(lines))",
    "for (i in seq_along(starts)) {",
    "  if (ends[i] < starts[i]) next",
    "  label <- if (i == 1L) '(header)' else lines[starts[i]]",
    "  ok <- tryCatch({ eval(parse(text = lines[starts[i]:ends[i]]), envir = globalenv()); TRUE },",
    "                 error = function(e) { cat('STEP_FAILED ', label, ' :: ', conditionMessage(e), '\\n', sep = ''); FALSE })",
    "  if (!ok) quit(status = 1)",
    "}",
    "saveRDS(list(longstring = longstring, irv = irv, R_items = R_items), 'replayed.rds')"
  ), runner)
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(runner)),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status")
  expect_identical(grep("^STEP_FAILED", out, value = TRUE), character(0))
  expect_true(is.null(status) || identical(status, 0L), info = paste(out, collapse = "\n"))
  expect_true(file.exists(file.path(dir, "replayed.rds")))
  replayed <- readRDS(file.path(dir, "replayed.rds"))
  # The careless screens of the script are the tab's: the same runs over the
  # same column order, and the same within-person variability.
  expect_identical(unname(replayed$longstring), unname(got$careless$longstring))
  expect_equal(unname(replayed$irv), unname(got$careless$irv), tolerance = 1e-12)
  expect_equal(replayed$R_items[rownames(got$R), colnames(got$R)], got$R, tolerance = 1e-12)
  # The files the script writes are the downloads: the CSV line for line,
  # the PNG at the same pixel size.
  expect_identical(
    readLines(file.path(dir, "siever_correlations_pearson.csv")),
    readLines(file.path(dir, "download.csv"))
  )
  png_dims <- function(f) {
    h <- readBin(f, "raw", 24L)
    px <- function(b) sum(as.integer(b) * 256^(3:0))
    c(px(h[17:20]), px(h[21:24]))
  }
  expect_identical(
    png_dims(file.path(dir, "siever_correlations_pearson.png")),
    png_dims(file.path(dir, "download.png"))
  )
})

test_that("nine tests: a label edit records no second careless-responding step", {
  st <- screen_hs9_state()
  stamps <- function(pattern) {
    sum(grepl(pattern, shiny::isolate(st$codelog), fixed = TRUE))
  }
  shiny::testServer(mod_screen_server, args = list(state = st), {
    invisible(careless())
    invisible(cor_mat())
    expect_identical(stamps("Screening: careless-responding screens over 9 active items"), 1L)
    # The screens run again after a label edit; the step is recorded once.
    m <- shiny::isolate(state$meta)
    m$label[1] <- "Visual perception (edited)"
    state$meta <- m
    session$flushReact()
    invisible(careless())
    invisible(cor_mat())
    expect_identical(stamps("Screening: careless-responding screens over 9 active items"), 1L)
    expect_identical(stamps("Screening: correlations among the 9 included items"), 1L)
    # A changed item set is a new step.
    m <- shiny::isolate(state$meta)
    m$status[m$item == "t2_cubes"] <- "parked"
    state$meta <- m
    session$flushReact()
    invisible(careless())
    expect_identical(stamps("Screening: careless-responding screens over 8 active items"), 1L)
    expect_identical(stamps("Screening: careless-responding screens over 9 active items"), 1L)
  })
})

# ---- The within-construct and within-facet blocks, boxed -----------------

test_that("the heatmap boxes each construct's and each facet's block, and only when asked", {
  R <- stats::cor(attitude)
  groups <- data.frame(
    construct = rep(c("Climate", "Advancement"), c(4, 3)),
    facet = c("Overall", "Overall", "Treatment", "Treatment", "Pay", "Pay", "Prospects")
  )
  # Each box is one rectangle; the key and the highlight draw none here.
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  utils::capture.output(suppressMessages(trace("rect",
    where = asNamespace("graphics"), print = FALSE,
    tracer = bquote(assign("n", get("n", envir = .(counter)) + 1L, envir = .(counter)))
  )))
  withr::defer(utils::capture.output(suppressMessages(untrace("rect", where = asNamespace("graphics")))))
  grDevices::pdf(NULL)
  plain <- sieve_correlation_heatmap(R, groups = groups, key = FALSE)
  drawn_plain <- counter$n
  boxed <- sieve_correlation_heatmap(R, groups = groups, key = FALSE, boxes = TRUE)
  drawn_boxed <- counter$n - drawn_plain
  grDevices::dev.off()
  expect_identical(nrow(plain$boxes), 0L)
  expect_identical(drawn_plain, 0L)
  b <- boxed$boxes
  expect_identical(b$kind, c("construct", "construct", "facet", "facet", "facet", "facet"))
  expect_identical(b$from, c(1L, 5L, 1L, 3L, 5L, 7L))
  expect_identical(b$to, c(4L, 7L, 2L, 4L, 6L, 7L))
  expect_identical(b$name, c("Climate", "Advancement", "Overall", "Treatment", "Pay", "Prospects"))
  expect_identical(drawn_boxed, nrow(b))
  # The separators are drawn as well.
  expect_identical(boxed$separators, plain$separators)
  # A facet name used in two constructs is two blocks; an item without a
  # construct or a facet is in no block of that kind.
  bl <- sieve_screen_blocks(data.frame(construct = c("A", "A", "B", "B", NA), facet = c("x", "x", "x", NA, NA)), 5L)
  expect_identical(bl$from[bl$kind == "construct"], c(1L, 3L))
  expect_identical(bl$to[bl$kind == "construct"], c(2L, 4L))
  expect_identical(bl$from[bl$kind == "facet"], c(1L, 3L))
  expect_identical(bl$to[bl$kind == "facet"], c(2L, 3L))
})

test_that("with facets the page offers the boxes, in the heatmap, the table and the recorded code; without them it does not", {
  st <- screen_hs9_state()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    # The nine tests have constructs and no facets: no option, no boxes.
    expect_identical(screen_text(output$cor_boxes_ui), "")
    session$setInputs(cor_boxes = TRUE)
    expect_false(heat_args()$boxes)
    # With facets (two per construct, where the construct has more than one
    # item for each), the option appears and boxes both kinds of block.
    m <- shiny::isolate(state$meta)
    m$facet <- c("v1", "v1", "v2", "t1", "t1", "t2", "s1", "s1", "s2")
    state$meta <- m
    session$flushReact()
    expect_match(screen_text(output$cor_boxes_ui), "Box the within-construct and within-facet correlations", fixed = TRUE)
    session$setInputs(cor_boxes = TRUE)
    a <- heat_args()
    expect_true(a$boxes)
    grDevices::pdf(NULL)
    drawn <- tryCatch(sieve_correlation_heatmap(a$R, labels = a$labels, groups = a$groups, boxes = a$boxes),
      finally = grDevices::dev.off()
    )
    expect_identical(sum(drawn$boxes$kind == "construct"), 3L)
    expect_identical(sum(drawn$boxes$kind == "facet"), 6L)
    # The table draws the same blocks on its displayed rows.
    expect_match(screen_text(output$cor_table), "drawCallback", fixed = TRUE)
    expect_match(screen_text(output$cor_table), "2px solid #d39f10", fixed = TRUE)
    # The recorded call of a download boxes them too.
    code <- sieve_screen_heatmap_code(a, heat_size(), "heat.png", "png")
    expect_true(any(grepl("boxes = TRUE)", code, fixed = TRUE)))
    expect_true(any(grepl("the within-construct and within-facet blocks boxed", code, fixed = TRUE)))
    # Off again: no boxes, no callback, and the recorded call as before.
    session$setInputs(cor_boxes = FALSE)
    expect_false(heat_args()$boxes)
    expect_false(grepl("drawCallback", screen_text(output$cor_table), fixed = TRUE))
    expect_false(any(grepl("boxes", sieve_screen_heatmap_code(heat_args(), heat_size(), "heat.png", "png"), fixed = TRUE)))
  })
})

# Plot coordinates must come from the fitted, rotated matrices, without
# suppressing small coefficients, clipping coefficients above one, or refitting.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

efa_plot_example <- function() {
  pattern <- matrix(c(
    .8, .1, -.2,
    -.3, .7, .4,
    1.12, -.25, .3,
    .05, -.1, .9
  ), nrow = 4L, byrow = TRUE,
  dimnames = list(c("item_A", "item_B", "item_C", "item_D"), c("ML3", "ML1", "ML4")))
  phi <- matrix(c(1, .4, -.2, .4, 1, .15, -.2, .15, 1), 3L,
    dimnames = list(colnames(pattern), colnames(pattern)))
  list(loadings = pattern, Phi = phi)
}

efa_plot_state <- function() {
  siever_test_study(name = NULL, configure = function(s) {
    m <- s$meta
    m$facet <- rep(c("first", "second", "second"), 3L)
    s$meta <- m
    s$theory <- list(k = 3L)
  })
}

test_that("rotated pattern coordinates preserve actual factor labels and coefficients", {
  fit <- efa_plot_example()
  got <- sieve_efa_plot_coordinates(fit)
  expect_identical(got$item, rownames(fit$loadings))
  expect_equal(got$x, c(.8, -.3, 1.12, .05))
  expect_equal(got$y, c(.1, .7, -.25, -.1))
  expect_identical(attr(got, "factors"), c("ML3", "ML1"))
  expect_identical(attr(got, "matrix"), "pattern")
  expect_equal(attr(got, "factor_correlation"), .4)

  chosen <- sieve_efa_plot_coordinates(fit, factors = c("ML4", "ML3"),
    items = c("item_D", "item_A"))
  expect_identical(chosen$item, c("item_D", "item_A"))
  expect_equal(chosen$x, c(.9, -.2))
  expect_equal(chosen$y, c(.05, .8))
  expect_identical(attr(chosen, "factors"), c("ML4", "ML3"))
  expect_equal(attr(chosen, "factor_correlation"), -.2)
  indexed <- sieve_efa_plot_coordinates(fit, factors = c(3L, 1L),
    items = c("item_D", "item_A"))
  expect_identical(indexed, chosen)
})

test_that("structure coordinates align named Phi and stored Structure before subsetting", {
  fit <- efa_plot_example()
  # These values are calculated independently from P and Phi, in ML3/ML1/ML4 order.
  expected <- matrix(c(
    .88, .39, -.345,
    -.1, .64, .565,
    .96, .243, .0385,
    -.17, .055, .875
  ), nrow = 4L, byrow = TRUE, dimnames = dimnames(fit$loadings))
  fit$Phi <- fit$Phi[c("ML1", "ML4", "ML3"), c("ML4", "ML3", "ML1")]
  got <- sieve_efa_plot_coordinates(fit, matrix = "structure", factors = c("ML4", "ML1"))
  expect_equal(got$x, expected[, "ML4"], ignore_attr = TRUE)
  expect_equal(got$y, expected[, "ML1"], ignore_attr = TRUE)
  expect_equal(attr(got, "factor_correlation"), .15)

  # A stored structure matrix may have its rows and columns in another order.
  fit$Structure <- expected[c("item_C", "item_A", "item_D", "item_B"), c("ML1", "ML4", "ML3")]
  stored <- sieve_efa_plot_coordinates(fit, matrix = "structure", factors = c("ML4", "ML1"))
  expect_identical(stored$item, rownames(fit$loadings))
  expect_equal(stored$x, expected[, "ML4"], ignore_attr = TRUE)
  expect_equal(stored$y, expected[, "ML1"], ignore_attr = TRUE)
})

test_that("orthogonal and single-factor solutions have honest coordinates", {
  fit <- efa_plot_example()
  fit$Phi <- NULL
  pattern <- sieve_efa_plot_coordinates(fit, factors = c(2L, 3L))
  structure <- sieve_efa_plot_coordinates(fit, matrix = "structure", factors = c(2L, 3L))
  expect_equal(pattern$x, structure$x)
  expect_equal(pattern$y, structure$y)
  expect_equal(attr(structure, "factor_correlation"), 0)

  fit$loadings <- fit$loadings[, "ML4", drop = FALSE]
  single <- sieve_efa_plot_coordinates(fit)
  expect_identical(single$item, rownames(fit$loadings))
  expect_equal(single$x, c(-.2, .4, .3, .9))
  expect_equal(single$y, seq_len(4L))
  expect_identical(attr(single, "factors"), "ML4")
  expect_true(is.na(attr(single, "factor_correlation")))
})

test_that("factor plot rejects ambiguous axes and malformed coefficient matrices", {
  fit <- efa_plot_example()
  expect_error(sieve_efa_plot_coordinates(fit, factors = "missing"), "factor|Factor")
  expect_error(sieve_efa_plot_coordinates(fit, factors = c("ML3", "ML3")), "distinct|duplicat|same")
  expect_error(sieve_efa_plot_coordinates(fit, factors = c(1L, 4L)), "factor|Factor|range")
  expect_error(sieve_efa_plot_coordinates(fit, factors = c(1L, 2L, 3L)), "two|2|one|1")
  expect_error(sieve_efa_plot_coordinates(fit, factors = c(1.5, 2)), "factor|Factor|integer")
  expect_error(sieve_efa_plot_coordinates(fit, items = "missing"), "item|Item")
  expect_error(sieve_efa_plot_coordinates(fit, matrix = "other"))
  fit$loadings[1L, 1L] <- Inf
  expect_error(sieve_efa_plot_coordinates(fit), "finite|coefficient|loading")
  fit <- efa_plot_example()
  fit$Phi[1L, 2L] <- NA_real_
  expect_error(sieve_efa_plot_coordinates(fit, matrix = "structure"), "finite|Phi|correlation")
  fit <- efa_plot_example()
  fit$Structure <- fit$loadings
  fit$Structure[1L, 1L] <- NaN
  expect_error(sieve_efa_plot_coordinates(fit, matrix = "structure"), "finite|Structure|structure|coefficient")
})

test_that("public plot draws exact coordinates with equal scales and restores graphics settings", {
  fit <- efa_plot_example()
  device <- tempfile(fileext = ".pdf")
  grDevices::pdf(device, width = 7, height = 5)
  on.exit({ grDevices::dev.off(); unlink(device) }, add = TRUE)
  graphics::par(mar = c(4, 5, 3, 2), mgp = c(2.2, .7, 0), xpd = FALSE)
  before <- graphics::par(c("mar", "mgp", "xpd", "pty", "xaxs", "yaxs"))
  seen <- new.env(parent = emptyenv())
  original_window <- graphics::plot.window
  local_mocked_bindings(plot.window = function(xlim, ylim, log = "", asp = NA, ...) {
    seen$xlim <- xlim
    seen$ylim <- ylim
    seen$asp <- asp
    original_window(xlim = xlim, ylim = ylim, log = log, asp = asp, ...)
  }, .package = "graphics")
  original_points <- graphics::points.default
  local_mocked_bindings(points.default = function(x, y = NULL, ...) {
    seen$x <- x
    seen$y <- y
    original_points(x, y, ...)
  }, .package = "graphics")
  returned <- withVisible(sieve_efa_factor_plot(fit, labels = TRUE))
  expect_false(returned$visible)
  expect_identical(returned$value, sieve_efa_plot_coordinates(fit))
  expect_equal(seen$asp, 1)
  expect_equal(seen$x, returned$value$x)
  expect_equal(seen$y, returned$value$y)
  expect_lte(min(seen$xlim), min(returned$value$x))
  expect_gte(max(seen$xlim), max(returned$value$x))
  expect_lte(min(seen$ylim), min(returned$value$y))
  expect_gte(max(seen$ylim), max(returned$value$y))
  expect_equal(graphics::par(names(before)), before)
  expect_silent(sieve_efa_factor_plot(fit, factors = c("ML4", "ML3"), labels = FALSE))
  single_fit <- list(loadings = fit$loadings[, "ML4", drop = FALSE])
  single <- withVisible(sieve_efa_factor_plot(single_fit, labels = FALSE))
  expect_false(single$visible)
  expect_equal(single$value$y, seq_len(4L))
  expect_equal(single$value$x, fit$loadings[, "ML4"], ignore_attr = TRUE)
  expect_error(sieve_efa_factor_plot(fit, items = character()), "No items")
  expect_error(sieve_efa_factor_plot(fit, labels = NA), "Labels")
})

test_that("plot display choices reuse one fit and changed data or factor counts wait for Run", {
  skip_if_not_installed("lavaan")
  state <- efa_plot_state()
  calls <- new.env(parent = emptyenv())
  calls$fa <- 0L
  calls$plot_code <- NULL
  # The EFA page's engine: DMAR::fa_exploratory.
  engine <- DMAR::fa_exploratory
  code_view <- sieve_code_view
  local_mocked_bindings(fa_exploratory = function(...) {
    calls$fa <- calls$fa + 1L
    engine(...)
  }, .package = "DMAR")
  withr::local_options(sieve.auto_run = "never")
  # The fits here run in the session whatever the machine's load: a
  # measured rate on a loaded machine could otherwise send a refit to a
  # background process. This test is about reusing one fit, not about the
  # wait (test-dimensionality-efa-workflows.R covers that).
  local_mocked_bindings(sieve_efa_dmar_wait = function(...) 0.5)
  local_mocked_bindings(
    sieve_code_view = function(..., readout = character()) {
      if (identical(list(...)$title, "Show code: factor plot")) calls$plot_code <- readout
      code_view(..., readout = readout)
    }
  )
  shiny::testServer(mod_efa_server, args = list(state = state), {
    session$setInputs(efa_go = 1L, n_factors = 3L, fm = "ml", rotation = "oblimin",
      cor_type = "pearson", plot_matrix = "pattern", plot_labels = TRUE)
    fit <- efa_run()$fit
    labels <- colnames(unclass(fit$loadings))
    session$setInputs(plot_x = labels[1L], plot_y = labels[2L])
    initial <- factor_plot_data()
    expect_equal(initial$x, as.numeric(unclass(fit$loadings)[, 1L]))
    expect_equal(initial$y, as.numeric(unclass(fit$loadings)[, 2L]))
    expect_identical(calls$fa, 1L)
    log_before <- shiny::isolate(state$codelog)

    session$setInputs(plot_x = labels[3L], plot_y = labels[1L], plot_labels = FALSE,
      plot_matrix = "structure", sort_loadings = TRUE)
    changed <- factor_plot_data()
    structure <- unclass(fit$Structure)
    expect_equal(changed$x, as.numeric(structure[, labels[3L]]))
    expect_equal(changed$y, as.numeric(structure[, labels[1L]]))
    expect_identical(calls$fa, 1L)

    construct <- shiny::isolate(state$meta$construct[1L])
    session$setInputs(display_construct = construct, display_facet = "first")
    filtered <- factor_plot_data()
    meta <- shiny::isolate(state$meta)
    expected <- meta$item[meta$construct == construct & meta$facet == "first"]
    expect_identical(filtered$item, expected)
    expect_equal(filtered$x, as.numeric(structure[expected, labels[3L]]))
    output$factor_plot_code
    expect_match(paste(calls$plot_code, collapse = "\n"), "SIEVER::sieve_efa_factor_plot", fixed = TRUE)
    # The readout is executable even when factors and items are vectors.
    device <- tempfile(fileext = ".pdf")
    grDevices::pdf(device)
    replayed <- tryCatch({
      env <- new.env(parent = baseenv())
      # The recorded R's efa_fit is DMAR's table, which the exported plot reads.
      env$efa_fit <- fit$dmar
      eval(parse(text = paste(calls$plot_code, collapse = "\n")), envir = env)
    }, finally = { grDevices::dev.off(); unlink(device) })
    expect_identical(replayed, filtered)
    expect_identical(calls$fa, 1L)
    expect_identical(shiny::isolate(state$codelog), log_before)

    # Suppression controls the loading table and the simple-structure threshold,
    # which has its own decision record. It must not alter plot coordinates.
    session$setInputs(suppress = .6)
    expect_identical(factor_plot_data(), filtered)
    expect_identical(calls$fa, 1L)

    shiny::isolate({
      edited <- state$items
      edited[1L, 1L] <- edited[1L, 1L] + .1
      state$items <- edited
    })
    sieve_confirm_types(state)
    session$flushReact()
    expect_error(factor_plot_data(), class = "shiny.silent.error")
    expect_identical(calls$fa, 1L)
    session$setInputs(efa_go = 2L)
    factor_plot_data()
    expect_identical(calls$fa, 2L)

    session$setInputs(n_factors = 2L)
    expect_error(factor_plot_data(), class = "shiny.silent.error")
    expect_identical(calls$fa, 2L)
    session$setInputs(efa_go = 3L, plot_x = NULL, plot_y = NULL)
    current <- factor_plot_data()
    expect_equal(ncol(unclass(efa_run()$fit$loadings)), 2L)
    expect_gt(nrow(current), 0L)
    expect_identical(calls$fa, 3L)
  })
})

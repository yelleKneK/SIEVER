# Text alternatives and focus (WCAG): every figure the application draws
# has a text alternative, and focus returns to the control that opened a
# dialog. A renderPlot() call without `alt` has each figure announced as
# Shiny's default "Plot object", and a dialog shown from the server would
# leave focus on the page body when it closed.

sieve_source_dir <- function() {
  candidates <- c(
    testthat::test_path("..", "..", "R"),
    file.path(getwd(), "..", "..", "R")
  )
  hit <- candidates[dir.exists(candidates)]
  if (!length(hit)) testthat::skip("The package source is not beside the tests.")
  normalizePath(hit[1])
}

test_that("every figure the application draws passes a text alternative", {
  files <- list.files(sieve_source_dir(), pattern = "[.]R$", full.names = TRUE)
  calls <- list()
  for (f in files) {
    exprs <- parse(f, keep.source = FALSE)
    walk <- function(e) {
      if (is.call(e)) {
        head <- e[[1]]
        name <- if (is.name(head)) as.character(head) else if (is.call(head) &&
          identical(head[[1]], as.name("::"))) as.character(head[[3]]) else ""
        if (identical(name, "renderPlot")) calls[[length(calls) + 1L]] <<- list(file = basename(f), call = e)
        for (part in as.list(e)[-1]) if (!missing(part)) walk(part)
      }
    }
    for (e in exprs) walk(e)
  }
  expect_gte(length(calls), 17L)
  for (x in calls) {
    alt <- x$call$alt
    # The path diagram's text alternative is built from the drawn model
    # (sieve_diagram_alt(), tested below); every other is a literal.
    if (identical(x$file, "fct_diagram_controls.R") && !is.character(alt)) {
      expect_true(any(grepl("sieve_diagram_alt(", deparse(alt), fixed = TRUE)), info = x$file)
      next
    }
    expect_true(is.character(alt) && length(alt) == 1L && nchar(alt) >= 30L,
      info = paste(x$file, paste(deparse(x$call)[1], collapse = " "))
    )
    expect_false(grepl("Plot object", alt %||% "", fixed = TRUE), info = x$file)
  }
})

test_that("the path diagram's text alternative says what the drawn model is, on the CFA and the SEM page", {
  # Not on the CRAN check's clock: one fit on each page.
  skip_on_cran()
  # One fixed text for both pages, "each factor with its items, their
  # loadings, and the factor correlations", would leave out on the SEM page
  # the regression paths that are the figure's point.
  st <- siever_test_study()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", layout = "tree2", what = "std",
      min_path = 0, resid = TRUE, covs = TRUE)
    session$setInputs(fit_go = 1L)
    alt <- sieve_diagram_alt(diagram$spec())
    expect_identical(alt, paste0(
      "Path diagram of the confirmatory factor model: each factor with its items and their loadings, ",
      "and the correlations the model estimates among the factors. The estimates are in the tables of the page that drew it."
    ))
    expect_identical(output$diagram$alt, alt)
  })
  st <- siever_test_study()
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(outcomes = "Speed", predictors = c("Visual", "Textual"))
    session$setInputs(fit_go = 1L)
    alt <- sieve_diagram_alt(diagram$spec())
    expect_match(alt, "Path diagram of the structural model: each factor with its items and their loadings, the regression paths among the factors and the outcomes", fixed = TRUE)
    expect_false(grepl("confirmatory", alt, fixed = TRUE))
    expect_identical(output$diagram$alt, alt)
  })
  # Before a model is drawn the text names no model kind.
  expect_match(sieve_diagram_alt(NULL), "Path diagram of the fitted model", fixed = TRUE)
})

test_that("focus returns to the control that opened a dialog", {
  js <- paste(readLines(system.file("app", "www", "sieve.js", package = "SIEVER"), warn = FALSE), collapse = "\n")
  # jQuery's handlers: the native hidden event never reaches the document
  # (Shiny removes the dialog in its own jQuery handler first).
  expect_match(js, "jQuery(document).on(\"show.bs.modal\"", fixed = TRUE)
  expect_match(js, "jQuery(document).on(\"hidden.bs.modal\"", fixed = TRUE)
  expect_match(js, "target.focus();", fixed = TRUE)
})

# One vocabulary for long computations: every running computation is ended with
# a Cancel button, naming what it cancels where a page runs more than one
# job; the lifecycle state is Stopped; and the sentence says the run was
# canceled and that nothing was kept or recorded.
#
# Author: Ken Kelley

test_that("every control that ends a running computation is a Cancel button", {
  root <- normalizePath(testthat::test_path("..", ".."))
  # An installed package has an R directory too, holding no source files.
  sources <- list.files(file.path(root, "R"), "[.]R$", full.names = TRUE)
  skip_if_not(length(sources) > 0L, "source tree not available")
  code <- unlist(lapply(sources, readLines, warn = FALSE))
  calls <- regmatches(code, regexpr('actionButton\\((session\\$)?ns\\("[a-z_]*(stop|cancel)[a-z_]*"\\),[^"]*"[^"]*"', code))
  calls <- calls[!grepl("case_reason_cancel", calls, fixed = TRUE)] # a dialog's Cancel, not a computation
  expect_gt(length(calls), 8L)
  labels <- sub('.*"([^"]*)"$', "\\1", calls)
  expect_true(all(grepl("^Cancel", labels)), info = paste(labels[!grepl("^Cancel", labels)], collapse = " | "))
  # No button anywhere is labeled Stop.
  expect_false(any(grepl('actionButton\\([^)]*"Stop', code)))
})

test_that("the categorical MI panel cancels with the same words", {
  ui <- paste(as.character(mod_reliab_ui("reliab")), collapse = " ")
  expect_match(ui, "Cancel ends the run", fixed = TRUE)
  expect_false(grepl("Stop ends", ui, fixed = TRUE))
})

test_that("the Dimensionality page names its sample, estimand and uncertainty above the evidence", {
  sieve_rate_reset()
  fx <- sieve_hs9_fixture()
  st <- siever_test_study(fx, confirmed = TRUE, labels = TRUE, theory = TRUE, name = fx$name)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", criterion = "p95", n_iter = 1000, max_k = 4, seed = 7)
    line <- gsub(">\\s+", ">", gsub("\\s+<", "<", paste(as.character(output$context_line$html), collapse = " ")))
    expect_match(line, "sieve-context-line", fixed = TRUE)
    expect_match(line, "<b>Sample:</b>9 items, n = 301 complete cases, Pearson correlations.", fixed = TRUE)
    expect_match(line, "<b>Estimand:</b>the number of common factors", fixed = TRUE)
    expect_match(line, "the 95th percentile of 1000 reference datasets (seed 7)", fixed = TRUE)
  })
})

test_that("the context line labels each part and leaves out a part not given", {
  html <- gsub(">\\s+", ">", gsub("\\s+<", "<", as.character(sieve_context_line("301 respondents", NULL, "a 95% interval"))))
  expect_match(html, "<b>Sample:</b>301 respondents.", fixed = TRUE)
  expect_false(grepl("Estimand", html, fixed = TRUE))
  expect_match(html, "<b>Uncertainty:</b>a 95% interval.", fixed = TRUE)
})

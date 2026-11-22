# A result disclosure is a read-only view of a completed calculation.

code_view_state <- function() {
  siever_test_study(theory = FALSE, name = NULL, configure = function(s) {
    s$roles$role[s$roles$role == "auxiliary"] <- "criterion"
  })
}

code_view_html <- function(x) paste(as.character(unlist(x)), collapse = "\n")

test_that("code disclosures escape source and explain empty, failed and earlier results", {
  marker <- 0L
  source <- 'marker <- marker + 1L; "<script>alert(1)</script>"'
  record <- list(code = source, context = "<b>A & B</b>", input_key = "saved-123")
  html <- as.character(sieve_code_view(record, "current"))
  expect_match(html, "<details", fixed = TRUE)
  expect_false(grepl("<details[^>]* open", html))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_match(html, "&lt;b&gt;A &amp; B&lt;/b&gt;", fixed = TRUE)
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_identical(marker, 0L)
  expect_match(html, "saved-123", fixed = TRUE)
  expect_match(as.character(sieve_code_view(NULL)), "Run this analysis", fixed = TRUE)
  expect_match(as.character(sieve_code_view(NULL, "failed")), "did not finish", fixed = TRUE)
  expect_match(as.character(sieve_code_view(NULL, "current")), "no saved code disclosure", fixed = TRUE)
  stale <- as.character(sieve_code_view(record, list(state = "stale", sentence = "An item was parked.")))
  expect_match(stale, "earlier result on screen", fixed = TRUE)
  expect_match(stale, "An item was parked.", fixed = TRUE)
  expect_match(stale, source |> htmltools::htmlEscape(), fixed = TRUE)
})

test_that("several disclosures share navigation actions without duplicate DOM ids", {
  ns <- shiny::NS("cfa")
  panel <- sieve_code_view(list(code = "x <- 1"), "current", ns = ns)
  html <- as.character(shiny::tagList(panel, panel, panel))
  expect_false(grepl('id="cfa-code_open_', html, fixed = TRUE))
  expect_match(html, "cfa-code_open_ingest", fixed = TRUE)
  expect_match(html, "cfa-code_open_repro", fixed = TRUE)
  expect_match(html, "Shiny.setInputValue", fixed = TRUE)
  expect_match(html, 'priority: &quot;event&quot;', fixed = TRUE)
})

test_that("a snapshot retains its own code, settings and input revision after later edits", {
  state <- code_view_state()
  original <- c('fitted <- "<TRUST>"', "# first result")
  sieve_log(state, "The completed fit", original)
  snapshot <- sieve_code_snapshot(state, context = "Estimator: MLR; missing: fiml")
  expect_identical(snapshot$code[-1L], original)
  expect_identical(snapshot$step, "1")
  expect_identical(snapshot$input_rows, nrow(shiny::isolate(state$items)))
  expect_identical(snapshot$item_names, names(shiny::isolate(state$items)))
  key <- snapshot$input_key
  sieve_log(state, "Another calculation", 'new_fit <- "different"')
  shiny::isolate({
    items <- state$items
    items[1L, 1L] <- items[1L, 1L] + 1
    state$items <- items
  })
  expect_identical(snapshot$input_key, key)
  expect_identical(snapshot$code[-1L], original)
  expect_identical(sieve_code_snapshot(state, step = 1L)$code, snapshot$code)
  expect_null(sieve_code_snapshot(state, step = 99L))
  expect_null(sieve_code_snapshot(sieve_new_state()))
})

test_that("new snapshots after restore identify executing namespaces and preserve old records", {
  state <- code_view_state()
  initial <- c(R = "4.1.0", SIEVER = "0.1.0", lavaan = "0.6-1", psych = "0.1.0")
  state$replay_versions <- initial
  sieve_log(state, "A new fit after restoring an earlier project", 'fit <- "current calculation"')
  # packageVersion() searches installed libraries. The namespace already
  # executing can differ, so this lookup must never supply the provenance.
  testthat::local_mocked_bindings(packageVersion = function(...) {
    stop("Do not look up installed versions through library paths")
  }, .package = "utils")
  snapshot <- sieve_code_snapshot(state)
  expect_identical(snapshot$initial_session_versions, initial)
  expect_identical(snapshot$versions[["SIEVER"]], as.character(getNamespaceVersion("SIEVER")))
  expect_identical(snapshot$namespace_paths[["SIEVER"]],
    normalizePath(getNamespaceInfo("SIEVER", "path"), mustWork = FALSE))
  expect_identical(snapshot$versions[["R"]], as.character(getRversion()))
  # GPArotation is not recorded: no SIEVER computation rotates through it.
  expect_false("GPArotation" %in% names(snapshot$versions))
  for (package in intersect(c("lavaan", "psych", "DMAR"), loadedNamespaces())) {
    expect_identical(snapshot$versions[[package]], as.character(getNamespaceVersion(package)))
    expect_identical(snapshot$namespace_paths[[package]],
      normalizePath(getNamespaceInfo(package, "path"), mustWork = FALSE))
  }
  expect_false(identical(snapshot$versions[["SIEVER"]], initial[["SIEVER"]]))
  old_record <- list(code = 'old_fit <- "earlier calculation"', versions = initial)
  old_copy <- old_record
  html <- as.character(sieve_code_view(old_record, "stale"))
  expect_identical(old_record, old_copy)
  expect_match(html, "SIEVER 0.1.0", fixed = TRUE)
  expect_match(html, "earlier result on screen", fixed = TRUE)
  expect_match(as.character(sieve_code_view(snapshot, "current")), snapshot$namespace_paths[["SIEVER"]], fixed = TRUE)
})

test_that("CFA code panels read the saved fit without starting or repeating a fit", {
  skip_if_not_installed("lavaan")
  state <- code_view_state()
  seen <- new.env(parent = emptyenv())
  seen$fits <- 0L
  engine <- sieve_run_cfa
  testthat::local_mocked_bindings(sieve_run_cfa = function(...) {
    seen$fits <- seen$fits + 1L
    engine(...)
  }, .package = "SIEVER")
  shiny::testServer(mod_cfa_server, args = list(state = state), {
    session$setInputs(scope = "construct", structure = "first", ident = "varone")
    expect_match(code_view_html(output$est_code), "Run this analysis", fixed = TRUE)
    expect_identical(seen$fits, 0L)
    session$setInputs(fit_go = 1L)
    expect_identical(seen$fits, 1L)
    saved <- demand$status()$value$code_view
    expect_match(paste(saved$code, collapse = "\n"), "lavaan::cfa", fixed = TRUE)
    expect_match(paste(saved$context, collapse = " "), "std.all", fixed = TRUE)
    for (name in c("fit_code", "phi_code", "est_code")) {
      expect_match(code_view_html(output[[name]]), saved$input_key, fixed = TRUE)
    }
    expect_identical(seen$fits, 1L)
    # The saved engine block is executable against the saved item view,
    # including its actual estimator and missing-data arguments.
    replay <- new.env(parent = globalenv())
    replay$items <- shiny::isolate(state$items)
    replay$d <- shiny::isolate(state$raw)
    invisible(utils::capture.output(eval(parse(text = saved$code), envir = replay)))
    expect_equal(lavaan::coef(replay$cfa_fit), lavaan::coef(demand$status()$value$fit), tolerance = 1e-10)
    expect_false(any(grepl("SIEVER:::", saved$code, fixed = TRUE)))
    shiny::isolate({
      items <- state$items
      items[1L, 1L] <- items[1L, 1L] + .1
      state$items <- items
    })
    sieve_confirm_types(state)
    session$flushReact()
    html <- code_view_html(output$est_code)
    expect_match(html, "earlier result on screen", fixed = TRUE)
    expect_match(html, saved$input_key, fixed = TRUE)
    expect_identical(seen$fits, 1L)
  })
})

test_that("reliability and score panels show recorded engines and do not recompute them", {
  skip_if_not_installed("DMAR")
  state <- code_view_state()
  seen <- new.env(parent = emptyenv())
  seen$constructs <- 0L
  engine <- sieve_reliability_construct
  testthat::local_mocked_bindings(sieve_reliability_construct = function(...) {
    seen$constructs <- seen$constructs + 1L
    engine(...)
  }, .package = "SIEVER")
  shiny::testServer(mod_reliab_server, args = list(state = state), {
    session$setInputs(scope = "construct", treatment = "study", denominator = "model_implied",
      ci_method = "mlr", missing = "listwise", conf_level = .95, score_method = "Bartlett")
    expect_match(code_view_html(output$reliability_code), "Run this analysis", fixed = TRUE)
    expect_identical(seen$constructs, 0L)
    session$setInputs(run_go = 1L)
    count <- seen$constructs
    expect_identical(count, 3L)
    saved <- attr(demand$status()$value, "code_view", exact = TRUE)
    html <- code_view_html(output$reliability_code)
    expect_match(html, "DMAR::reliability_omega", fixed = TRUE)
    # H's interval is DMAR::reliability_H()'s, so the recorded code calls
    # DMAR and defines no helper of SIEVER's (h_interval()).
    expect_match(html, "DMAR::reliability_H(data = h_data", fixed = TRUE)
    expect_false(grepl("h_interval", html, fixed = TRUE))
    expect_match(html, saved$input_key, fixed = TRUE)
    expect_silent(parse(text = saved$code))
    expect_false(any(grepl("SIEVER:::", saved$code, fixed = TRUE)))
    expect_match(paste(saved$context, collapse = " "), "Visual: modeled item columns", fixed = TRUE)
    expect_identical(seen$constructs, count)
    expect_match(code_view_html(output$scores_code), "Run this analysis", fixed = TRUE)
    session$setInputs(scores_go = 1L)
    score_snapshot <- demand_scores$status()$value$code_view
    expect_match(paste(score_snapshot$code, collapse = "\n"), "score_fit <- lavaan::cfa", fixed = TRUE)
    replay <- new.env(parent = globalenv())
    replay$items <- shiny::isolate(state$items)
    replay$d <- shiny::isolate(state$raw)
    invisible(utils::capture.output(eval(parse(text = score_snapshot$code), envir = replay)))
    expect_equal(lavaan::coef(replay$score_fit), lavaan::coef(demand_scores$status()$value$fit), tolerance = 1e-10)
    expect_false(any(grepl("SIEVER:::", score_snapshot$code, fixed = TRUE)))
    # The readout names only exported functions, so it runs where SIEVER is
    # installed (an internal one, such as sieve_factor_scores(), would not).
    html <- code_view_html(output$scores_code)
    expect_match(html, "SIEVER::sieve_factor_score_record(score_fit, method = ", fixed = TRUE)
    named <- unique(sub("^SIEVER::", "", regmatches(html, gregexpr("SIEVER::[A-Za-z0-9_.]+", html))[[1]]))
    expect_identical(setdiff(named, getNamespaceExports("SIEVER")), character(0))
    # The readout's scores are the panel's, for this admissible model.
    readout <- sieve_factor_score_record(demand_scores$status()$value$fit, method = "Bartlett")$scores
    expect_equal(as.matrix(readout), as.matrix(sieve_factor_scores(demand_scores$status()$value$fit, "Bartlett", labels = TRUE)),
      tolerance = 0, ignore_attr = TRUE
    )
    expect_match(code_view_html(output$scores_code), score_snapshot$input_key, fixed = TRUE)
    expect_identical(seen$constructs, count)
  })
})

page_guide_state <- function() {
  d <- as.data.frame(matrix(seq_len(80L) %% 7L, nrow = 10L))
  names(d) <- paste0("x", seq_len(ncol(d)))
  fixture <- list(data = d, item_cols = names(d),
    theory_map = list(A = names(d)[1:4], B = names(d)[5:8]),
    name = "Guide fixture", labels = NULL, load_code = character())
  siever_test_study(fixture, labels = FALSE, theory = FALSE, configure = function(s) {
    s$roles <- NULL
    m <- s$meta
    m$type <- "continuous"
    s$meta <- m
  })
}

test_that("every navigation page has the same compact actionable guide", {
  # "scoring" is the Scoring and Norms panel of the Reliability page, in the
  # Scores workspace (fct_workspaces.R).
  expected <- c("ingest", "screen", "content", "retention", "efa", "winnow", "cfa", "reliab",
    "scoring", "validity", "finalize", "optimize", "bifactor", "network", "irt", "sem", "repro")
  expect_setequal(names(sieve_page_catalog()), expected)
  s <- page_guide_state()
  for (page in expected) {
    status <- shiny::isolate(sieve_page_status(s, page))
    html <- as.character(sieve_page_status_ui(status, s))
    expect_match(html, "what's needed here", fixed = TRUE)
    expect_match(html, '<section id="sieve-page-guide"', fixed = TRUE)
    expect_match(html, "<details>", fixed = TRUE)
    expect_false(grepl("<details open", html, fixed = TRUE))
    expect_true(nzchar(status$next_step))
  }
})

test_that("the guide names prerequisites without forcing an analysis sequence", {
  s <- sieve_new_state()
  expect_match(shiny::isolate(sieve_page_status(s, "cfa"))$message, "steps 1 and 2", fixed = TRUE)
  expect_identical(shiny::isolate(sieve_page_status(s, "content"))$state, "ready")
  expect_identical(shiny::isolate(sieve_page_status(s, "repro"))$state, "ready")
  s <- page_guide_state()
  s$confirmed <- FALSE
  expect_match(shiny::isolate(sieve_page_status(s, "cfa"))$message, "steps 3 and 4", fixed = TRUE)
  s$confirmed <- TRUE
  expect_match(shiny::isolate(sieve_page_status(s, "ingest"))$message, "Preparation confirmed", fixed = TRUE)
  expect_identical(shiny::isolate(sieve_page_status(s, "efa"))$state, "ready")
  expect_identical(shiny::isolate(sieve_page_status(s, "optimize"))$state, "blocked")
  s$construct_basis <- "empirical"
  expect_identical(shiny::isolate(sieve_page_status(s, "optimize"))$state, "ready")
})

test_that("the guide records metadata only and never requests a scientific calculation", {
  s <- page_guide_state()
  fingerprint <- sieve_project_fingerprint(s)
  local_mocked_bindings(sieve_state_basis = function(...) stop("guide requested correlations"),
    sieve_fit_cfa = function(...) stop("guide requested a model"))
  record <- sieve_page_publish(s, "cfa", "cfa-fit_go", "CFA",
    list(state = "current", key = list(model = "A =~ x1 + x2 + x3"), value = new.env()))
  expect_null(record$value)
  expect_identical(sieve_project_fingerprint(s), fingerprint)
  expect_type(record$key, "character")
  expect_no_error(shiny::isolate(sieve_page_status(s, "cfa")))
  expect_identical(shiny::isolate(sieve_page_status(s, "cfa"))$state, "current")
  sieve_page_publish(s, "cfa", "cfa-compare_go", "Optional structures", list(state = "not_run"))
  current <- shiny::isolate(sieve_page_status(s, "cfa"))
  expect_length(current$results, 1L)
  expect_false(grepl("Optional structures", current$message, fixed = TRUE))
  before <- shiny::isolate(s$page_results)
  for (page in names(sieve_page_catalog())) shiny::isolate(sieve_page_status(s, page))
  expect_identical(shiny::isolate(s$page_results), before)
})

test_that("saved projects neither retain nor revive page assurances", {
  s <- page_guide_state()
  sieve_page_publish(s, "cfa", "cfa-fit_go", "CFA", list(state = "current", key = list()))
  snapshot <- sieve_project_snapshot(s)
  expect_false("page_results" %in% names(snapshot$other))
  restored <- sieve_new_state()
  sieve_project_restore(restored, snapshot)
  expect_length(shiny::isolate(restored$page_results), 0L)
  expect_false(identical(shiny::isolate(sieve_page_status(restored, "cfa"))$state, "current"))
  # Even a snapshot that carries page results, with its content key
  # recomputed, cannot restore a live badge.
  snapshot$other$page_results <- shiny::isolate(s$page_results)
  snapshot$keys$content <- sieve_content_key(snapshot)
  sieve_project_restore(restored, snapshot)
  expect_length(shiny::isolate(restored$page_results), 0L)
})

test_that("recoding invalidates assurance but label edits and navigation do not", {
  s <- page_guide_state()
  sieve_page_publish(s, "cfa", "cfa-fit_go", "CFA", list(state = "current", key = list(model = "A")))
  m <- shiny::isolate(s$meta)
  m$label[1L] <- "Clearer item name"
  s$meta <- m
  expect_identical(shiny::isolate(sieve_page_status(s, "cfa"))$state, "current")
  before <- shiny::isolate(s$codelog)
  shiny::isolate(sieve_page_status(s, "efa"))
  shiny::isolate(sieve_page_status(s, "cfa"))
  expect_identical(shiny::isolate(s$codelog), before)
  sieve_apply_reverse(s, "x1", lo = 0, hi = 6)
  shiny::isolate(sieve_confirm_types(s))
  after <- shiny::isolate(sieve_page_status(s, "cfa"))
  expect_identical(after$state, "stale")
  expect_match(after$message, "Refresh before using: CFA", fixed = TRUE)
  sieve_page_publish(s, "cfa", "cfa-fit_go", "CFA", list(state = "current", key = list(model = "A", new = TRUE)))
  expect_identical(shiny::isolate(sieve_page_status(s, "cfa"))$state, "current")
})

test_that("respondent identity and sampling changes cannot inherit a current badge", {
  s <- page_guide_state()
  sieve_page_publish(s, "reliab", "reliab-scores_go", "Factor scores", list(state = "current", key = list()))
  old_data <- shiny::isolate(sieve_data_key(s$items))
  s$case_index <- rev(shiny::isolate(sieve_case_index(s)))
  expect_identical(shiny::isolate(sieve_data_key(s$items)), old_data)
  expect_identical(shiny::isolate(sieve_page_status(s, "reliab"))$state, "stale")
  sieve_page_publish(s, "reliab", "reliab-scores_go", "Factor scores", list(state = "current", key = list()))
  s$sampling_design <- list(unit = "clustered", cluster = "x8")
  expect_identical(shiny::isolate(sieve_page_status(s, "reliab"))$state, "stale")
  s$dataset_id <- "another-load"
  expect_identical(shiny::isolate(sieve_page_status(s, "reliab"))$state, "blocked")
})

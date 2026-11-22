ux_review_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = FALSE, name = fx$name, configure = function(s) {
  s$roles <- sieve_declared_roles(fx$data, fx$item_cols)
s$estimator <- "MLR"
  })
  s
}

test_that("a prepared named project acknowledges only its own revision across save entry points", {
  s <- ux_review_state()
  path <- tempfile()
  on.exit(unlink(path), add = TRUE)
  sieve_prepare_project_copy(s, path, "Study revision A")
  shiny::isolate({
    expect_identical(s$project_control$prepared$name, "Study revision A.siever.rds")
    expect_null(s$project_control$acknowledged)
    saved_key <- s$project_control$prepared$key
    m <- s$meta
    m$label[1L] <- "Later item name"
    s$meta <- m
    expect_true(sieve_acknowledge_project_copy(s))
    expect_identical(s$project_control$acknowledged, saved_key)
    expect_false(identical(s$project_control$acknowledged, sieve_project_fingerprint(s)))
  })
  snap <- sieve_project_read(path)
  expect_identical(snap$other$project_name, "Study revision A")
  expect_match(as.character(sieve_project_preview_ui(snap, "study.rds")), "Registered analyses", fixed = TRUE)
})

test_that("metadata batches use nonadjacent stable IDs and undo preserves unrelated changes", {
  s <- ux_review_state()
  before <- shiny::isolate(s$meta)
  ids <- before$item[c(7L, 2L)]
  patch <- sieve_metadata_patch(before, ids, "construct", "Revised construct")
  sieve_apply_metadata_patch(s, patch, "Review selected assignments")
  shiny::isolate({
    expect_identical(s$meta$construct[match(ids, s$meta$item)], rep("Revised construct", 2L))
    expect_identical(s$meta$item, before$item)
    expect_identical(s$items, s$raw[, before$item, drop = FALSE])
    m <- s$meta
    m$label[9L] <- "Independent name edit"
    s$meta <- m
  })
  sieve_undo_metadata_edit(s)
  shiny::isolate({
    expect_identical(s$meta$construct, before$construct)
    expect_identical(s$meta$label[9L], "Independent name edit")
    expect_true(s$metadata_edits[[1L]]$undone)
  })
  t <- sieve_new_state()
  sieve_project_restore(t, sieve_project_snapshot(s))
  expect_identical(shiny::isolate(t$metadata_edits), shiny::isolate(s$metadata_edits))
  invalid <- before
  invalid$construct[1L] <- "Unassigned"
  expect_error(sieve_metadata_patch(invalid, invalid$item[1L], "facet", "Cannot belong"), "parent construct")
  expect_identical(shiny::isolate(s$meta$construct), before$construct)
})

test_that("facet group operations remain inside one parent and conflicting undo is refused", {
  s <- ux_review_state()
  m <- shiny::isolate(s$meta)
  m$facet[c(1L, 4L)] <- "Shared name"
  s$meta <- m
  parent <- m$construct[1L]
  ids <- m$item[!is.na(m$facet) & m$facet == "Shared name" & m$construct == parent]
  sieve_apply_metadata_patch(s, sieve_metadata_patch(m, ids, "facet", "Corrected name"), "Rename facet")
  shiny::isolate({
    expect_identical(s$meta$facet[4L], "Shared name")
    z <- s$meta
    z$facet[1L] <- "Later decision"
    s$meta <- z
  })
  expect_error(sieve_undo_metadata_edit(s), "changed again")
})

test_that("CSV preview and full import preserve explicit decimal, quoting, encoding and replay choices", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c('id;wording;response', '1;"Caf\u00e9; wording";1,5', '2;"quoted ""word""";2,5'), path, useBytes = TRUE)
  got <- sieve_read_delimited(path, "semicolon.csv", ";", ",", "UTF-8", description = FALSE)
  expect_true(got$ok)
  expect_equal(got$value$response, c(1.5, 2.5))
  expect_identical(got$value$wording, c("Caf\u00e9; wording", 'quoted "word"'))
  preview <- sieve_read_delimited(path, "semicolon.csv", ";", ",", "UTF-8", description = FALSE, nrows = 1L)
  expect_identical(preview$value, got$value[1L, , drop = FALSE])
  env <- new.env(parent = baseenv())
  code <- gsub('"semicolon.csv"', paste(deparse(path), collapse = ""), got$code, fixed = TRUE)
  eval(parse(text = code), env)
  expect_identical(env$d, got$value)
  expect_error(sieve_read_delimited(path, "bad.csv", ",", ","), "must differ")
})

test_that("matched bundle data specification project and summary share one frozen revision", {
  s <- ux_review_state()
  snap <- sieve_project_snapshot(s)
  path <- tempfile(fileext = ".tar.gz")
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(c(path, dir), recursive = TRUE), add = TRUE)
  manifest <- sieve_export_study_bundle(snap, path, "raw")
  utils::untar(path, exdir = dir, tar = "internal")
  expect_true(all(c(
    "data.csv",
    "specification.csv",
    "summary.csv",
    "project.siever.rds",
    "manifest.R"
  ) %in% list.files(dir)))
  shared <- sieve_project_read(file.path(dir, "project.siever.rds"))
  expect_identical(names(shared$raw), snap$meta$item)
  expect_equal(shared$items, snap$items, ignore_attr = TRUE)
  expect_length(shared$analyses, 0L)
  expect_length(shared$other$removed_cases, 0L)
  expect_false(manifest$complete_working_project)
  expect_identical(manifest$revision, snap$revision)
  data <- utils::read.csv(file.path(dir, "data.csv"))
  expect_equal(data[, snap$meta$item], snap$raw[, snap$meta$item], tolerance = 1e-13)
  expect_identical(manifest$summary$n, sum(stats::complete.cases(snap$items[, snap$meta$item, drop = FALSE])))
  expect_true(nrow(utils::read.csv(file.path(dir, "specification.csv"))) >= nrow(snap$meta))
})

test_that("stored-result presentation never runs an engine or loses the exact appendix", {
  s <- ux_review_state()
  s$analyses <- list(A1 = list(
    id = "A1",
    kind = "cfa",
    note = "Correlated construct model",
    status = list(
      code = "ok",
      message = "converged"
    ),
    time = "2026-10-20", revision = sieve_revision(s), spec = list(estimator = "MLR", items = c("a", "b")),
    estimates = c(CFI = .9876543210987654, RMSEA = .0345678901234567)
  ))
  testthat::local_mocked_bindings(
    sieve_run_cfa = function(...) stop("unexpected model fit"),
    sieve_reliability_omega = function(...) stop("unexpected reliability run")
  )
  original <- shiny::isolate(s$analyses)
  catalog <- sieve_analysis_catalog(s)
  expect_identical(catalog$id, "A1")
  expect_identical(catalog$revision, "current")
  expect_identical(catalog$estimator, "MLR")
  report <- sieve_readable_saved_report(s, "A1")
  expect_match(report, "Correlated construct model", fixed = TRUE)
  expect_match(report, "0.987654321098765", fixed = TRUE)
  expect_identical(shiny::isolate(s$analyses), original)
  # A requested record that is absent is listed as absent, never refit,
  # and the report goes on.
  absent <- sieve_readable_saved_report(s, c("A1", "A404"))
  expect_match(absent, "A404 absent", fixed = TRUE)
  expect_match(absent, "Nothing was refit in its place.", fixed = TRUE)
  expect_identical(shiny::isolate(s$analyses), original)
})

test_that("saved CFA reports identify estimator fitted rows fit family and paired RMSEA limits", {
  s <- ux_review_state()
  d <- shiny::isolate(s$items)
  model <- sieve_cfa_plan(shiny::isolate(s$meta))$model
  fit <- lavaan::cfa(model, data = d, estimator = "MLR", std.lv = TRUE)
  fitted_measures <- sieve_fit_measures(fit)
  n <- shiny::isolate(nrow(s$items))
  id <- sieve_register_analysis(s, list(
    spec = list(kind = "cfa", estimator = "MLR"),
    fit = fit, status = sieve_fit_status(fit), warnings = character()
  ), note = "Three-construct CFA")
  record <- shiny::isolate(s$analyses[[id]])
  expect_equal(record$sample_size$input, n)
  expect_equal(sum(record$sample_size$fitted), lavaan::lavInspect(fit, "nobs"))
  original <- shiny::isolate(s$analyses)
  local_mocked_bindings(
    sieve_run_cfa = function(...) stop("unexpected refit"),
    sieve_fit_measures = function(...) stop("unexpected engine read")
  )
  report <- sieve_readable_saved_report(s, id)
  expect_match(report, "Estimator: MLR", fixed = TRUE)
  expect_match(report, "fitted rows: 301", fixed = TRUE)
  expect_match(report, "Fit-index family: robust ; model-test family: scaled", fixed = TRUE)
  expect_match(report, "RMSEA (robust)", fixed = TRUE)
  # The interval named with the level the record carries (the RMSEA level,
  # 90%).
  expect_match(report, "90% CI", fixed = TRUE)
  expect_match(report, "Lower", fixed = TRUE)
  expect_match(report, "Upper", fixed = TRUE)
  expect_match(report, "Exact specifications and unrounded stored results", fixed = TRUE)
  expect_identical(shiny::isolate(s$analyses), original)
  expect_identical(record$estimates$fit, fitted_measures)
})

test_that("expert mappings refuse ambiguous targets and survive project reopening", {
  s <- ux_review_state()
  ids <- shiny::isolate(s$meta$item)
  map <- data.frame(rated = c("Expert A", "Expert B"), ledger = ids[c(3L, 8L)])
  expect_identical(sieve_check_content_mapping(map$rated, ids, map), map)
  expect_error(sieve_check_content_mapping(map$rated, ids, transform(map, ledger = ids[1L])), "only once")
  s$content_mapping <- list(review1 = list(mapping = map))
  t <- sieve_new_state()
  sieve_project_restore(t, sieve_project_snapshot(s))
  expect_identical(shiny::isolate(t$content_mapping), shiny::isolate(s$content_mapping))
})

test_that("numeric EFA exports retain suppressed values and compute orthogonal structure correctly", {
  p <- matrix(c(.0123456789012345, .7, .5, -.02), 2L, dimnames = list(c("a", "b"), c("F1", "F2")))
  fit <- list(loadings = p, communality = c(.3, .5), complexity = c(1.2, 1.1), Vaccounted = matrix(1:4, 2L))
  m <- data.frame(
    item = c(
      "a",
      "b"
    ),
    label = c(
      '<img src=x onerror="bad()">',
      'quoted & "literal"'
    ),
    construct = "A",
    facet = NA_character_,
    type = "continuous",
    status = "active"
  )
  tables <- sieve_efa_numeric_tables(fit, m)
  expect_identical(tables$pattern, p)
  expect_equal(tables$structure, p)
  expect_identical(tables$items$label, m$label)
  expected_phi <- diag(2L)
  dimnames(expected_phi) <- list(colnames(p), colnames(p))
  expect_identical(tables$phi, expected_phi)
  expect_equal(sieve_diagram_effective_label(12, 6), 5)
  expect_equal(sieve_diagram_effective_label(12, 18), 15)
  expect_true(is.na(sieve_diagram_effective_label(0, 6)))
})

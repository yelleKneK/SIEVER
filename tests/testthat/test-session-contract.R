# A complete public-data session executed through shared services and replayed
# in a fresh process against the installed package. It uses only bundled data.
test_that('import, transformations, decisions, reports and fresh replay retain one scientific history', {
  package_path <- getNamespaceInfo(asNamespace('SIEVER'), 'path')
  skip_if_not(
    file.exists(file.path(package_path, 'Meta', 'package.rds')),
    'The fresh-process replay runs against an installed SIEVER.'
  )
  fx <- sieve_hs9_fixture()
  dat <- fx$data[, c(fx$id_vars[1L], fx$item_cols), drop = FALSE]
  names(dat)[1L] <- 'id'
  # Deliberately heterogeneous response scales, with fixed declared bounds.
  dat[[2L]] <- dat[[2L]] / max(dat[[2L]]) * 100
  dat[[3L]] <- dat[[3L]] / max(dat[[3L]]) * 50
  file <- tempfile(fileext = '.csv')
  before_file <- tempfile(fileext = '.siever.rds')
  after_file <- tempfile(fileext = '.siever.rds')
  on.exit(unlink(c(file, before_file, after_file)), add = TRUE)
  utils::write.csv(dat, file, row.names = FALSE)
  read <- sieve_read_data(file, name = 'session.csv')
  expect_true(read$ok)
  state <- sieve_new_state()
  loaded <- sieve_load_frame(state, read$value, name = 'Public session', code = read$code)
  expect_true(loaded$ok)
  meta <- shiny::isolate(state$meta)
  expect_setequal(meta$item, fx$item_cols)
  map <- fx$theory_map
  meta$construct <- vapply(
    meta$item,
    function(it) names(map)[vapply(
      map,
      function(its) it %in% its,
      logical(1)
    )][1L],
    character(1)
  )
  meta$facet <- paste(meta$construct, 'first facet')
  meta$type <- 'continuous'
  state$meta <- meta
  state$confirmed <- TRUE
  state$estimator <- 'MLR'
  state$estimator_choice <- 'mlr'
  initial_items <- shiny::isolate(state$items)
  before <- sieve_run_cfa(sieve_cfa_spec(meta, reserved = names(initial_items), estimator = 'mlr'), initial_items)
  expect_true(sieve_has_solution(before))
  first_id <- sieve_register_analysis(state, before, note = 'Before changes')
  sieve_log(state, 'First model', c(
    sieve_cfa_code(before$spec, fit_name = 'before_fit', model_name = 'before_model'),
    'before_items <- items', "before_chisq <- unname(lavaan::fitMeasures(before_fit, 'chisq'))"
  ))
  first_record <- shiny::isolate(state$analyses[[first_id]])
  sieve_project_write(sieve_project_snapshot(state), before_file)
  # Transformations use the real stable-identity services.
  reverse_one <- sieve_apply_reverse(state, fx$item_cols[1L], lo = 0, hi = 100)
  reverse_two <- sieve_apply_reverse(state, fx$item_cols[2L], lo = 0, hi = 50)
  expect_length(c(reverse_one$applied, reverse_two$applied), 2L)
  expect_equal(shiny::isolate(state$items[[fx$item_cols[1L]]]), 100 - initial_items[[fx$item_cols[1L]]], tolerance = 1e-12)
  expect_equal(shiny::isolate(state$items[[fx$item_cols[2L]]]), 50 - initial_items[[fx$item_cols[2L]]], tolerance = 1e-12)
  removed <- sieve_remove_cases(state, c(4L, 19L), 'Known original rows')
  expect_identical(removed$removed, c(4L, 19L))
  expect_false(any(c(4L, 19L) %in% shiny::isolate(sieve_case_index(state))))
  sieve_set_item_status(state, fx$item_cols[9L], 'parked', 'Alternative item pool')
  meta <- shiny::isolate(state$meta)
  meta$facet[3L] <- 'Alternative facet'
  state$meta <- meta
  sieve_set_construct_basis(state, 'empirical')
  state$estimator_choice <- 'ml'
  decision <- sieve_record_decision(state,
    'item_pool',
    'Which candidate?',
    paste(fx$item_cols[1:8],
      collapse = ', '
    ),
    rationale = 'Public acceptance fixture.'
  )
  current_items <- shiny::isolate(state$items)
  after <- sieve_run_cfa(sieve_cfa_spec(meta, reserved = names(current_items), estimator = 'ml'), current_items)
  expect_true(sieve_has_solution(after))
  second_id <- sieve_register_analysis(state, after, note = 'After changes')
  sieve_log(state, 'Second model', c(
    sieve_cfa_code(after$spec, fit_name = 'after_fit', model_name = 'after_model'),
    'after_items <- items', "after_chisq <- unname(lavaan::fitMeasures(after_fit, 'chisq'))"
  ))
  expect_false(identical(first_id, second_id))
  expect_identical(shiny::isolate(state$analyses[[first_id]]$inputs$items), first_record$inputs$items)
  # Report generation and persistence must not refit scientific models.
  testthat::local_mocked_bindings(sieve_run_cfa = function(...) stop('Unexpected scientific refit'))
  report <- sieve_saved_analyses_report(state, first_id)
  expect_match(report, 'Before changes', fixed = TRUE)
  expect_false(grepl('After changes', report, fixed = TRUE))
  sieve_project_write(sieve_project_snapshot(state), after_file)
  restored <- sieve_new_state()
  sieve_project_restore(restored, sieve_project_read(after_file))
  old <- sieve_new_state()
  sieve_project_restore(old, sieve_project_read(before_file))
  expect_identical(shiny::isolate(old$items), initial_items)
  expect_identical(shiny::isolate(restored$items), current_items)
  expect_identical(shiny::isolate(restored$meta), meta)
  expect_identical(shiny::isolate(restored$construct_basis), 'empirical')
  expect_identical(shiny::isolate(restored$estimator_choice), 'ml')
  expect_identical(shiny::isolate(sieve_current_decision(restored, 'item_pool', restored$dataset_id)$id), decision$id)
  expect_identical(shiny::isolate(restored$analyses[[first_id]]$estimates), first_record$estimates)
  script <- sieve_replay_script(restored)
  # All script statements run, not only extracted snippets. Values are compared
  # with the original fit and data, not another call to the replay builder.
  got <- callr::r(function(script, lib) {
    library(SIEVER, lib.loc = lib)
    env <- new.env(parent = baseenv())
    invisible(capture.output(eval(parse(text = script), env)))
    list(
      before = env$before_items, after = env$after_items,
      first = env$before_chisq, second = env$after_chisq
    )
  }, args = list(script = script, lib = dirname(package_path)), libpath = .libPaths())
  expect_identical(got$before, initial_items)
  # Recorded R literals and subsequent subtraction can differ at machine
  # precision; require the same rows, columns and values to 1e-12.
  expect_identical(names(got$after), names(current_items))
  expect_identical(dim(got$after), dim(current_items))
  expect_equal(got$after, current_items, tolerance = 1e-12)
  expect_equal(got$first, unname(lavaan::fitMeasures(before$fit, 'chisq')), tolerance = 1e-10)
  expect_equal(got$second, unname(lavaan::fitMeasures(after$fit, 'chisq')), tolerance = 1e-10)
})

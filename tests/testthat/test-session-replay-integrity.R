replay_integrity_state <- function(n = 80L, p = 9L) {
  d <- as.data.frame(matrix(seq_len(n * p) %% 7L, nrow = n))
  names(d) <- paste0("item", seq_len(p))
  d$id <- seq_len(n)
  state <- sieve_new_state()
  state$raw <- d
  state$items <- d[, setdiff(names(d), "id"), drop = FALSE]
  state$meta <- sieve_build_meta(d, setdiff(names(d), "id"))
  m <- shiny::isolate(state$meta)
  m$construct <- rep(c("A", "B", "C"), length.out = p)
  state$meta <- m
  state$roles <- sieve_declared_roles(d, setdiff(names(d), "id"))
  shiny::isolate(sieve_new_dataset(state))
  state
}

replay_integrity_run <- function(state) {
  script <- sieve_replay_script(state)
  envir <- new.env(parent = baseenv())
  eval(parse(text = script), envir = envir)
  envir
}

test_that("metadata edits, removal, reversal, and restore replay in order", {
  # Not on the CRAN check's clock (about 3 s); the full suite runs it.
  skip_on_cran()
  s <- replay_integrity_state()
  sieve_log(s, "First analysis", "first_n <- nrow(items); first_mean <- mean(items$item1)")
  p <- sieve_metadata_patch(shiny::isolate(s$meta), c("item1", "item2"), "label", c("First", "Second"))
  sieve_apply_metadata_patch(s, p, "Rename two items")
  sieve_remove_cases(s, c(2L, 6L), "Review the response pattern")
  sieve_apply_reverse(s, "item2", lo = 0, hi = 6)
  sieve_restore_cases(s, 2L)
  sieve_log(s, "Last analysis", "last_n <- nrow(items); last_mean <- mean(items$item2)")
  env <- replay_integrity_run(s)
  expect_identical(env$first_n, 80L)
  expect_identical(env$last_n, 79L)
  expect_equal(env$last_mean, shiny::isolate(mean(s$items$item2)))
  expect_identical(sieve_data_key(env$items), shiny::isolate(sieve_data_key(s$items)))
  expect_identical(sieve_meta_key(env$meta), shiny::isolate(sieve_meta_key(s$meta)))
  expect_identical(env$case_index, shiny::isolate(s$case_index))
})

test_that("free-text reasons cannot become executable script source", {
  s <- replay_integrity_state()
  reason <- "Reason\nstop('FREE TEXT EXECUTED')\n# a final line"
  sieve_set_item_status(s, "item1", "parked", reason)
  sieve_remove_cases(s, 2L, reason)
  script <- sieve_replay_script(s)
  expect_no_error(parse(text = script))
  expect_no_error(replay_integrity_run(s))
  expect_match(script, "# +stop\\('FREE TEXT EXECUTED'\\)")
  sieve_set_item_status(s, "item1", "active")
  expect_identical(shiny::isolate(s$item_actions[[1L]]$reason), reason)
})

test_that("source data are stored once while metadata edits and exclusions accumulate", {
  # Not on the CRAN check's clock (about 1 s); the full suite runs it.
  skip_on_cran()
  s <- replay_integrity_state(500L, 63L)
  sieve_log(s, "First", "n_first <- nrow(items)")
  for (i in seq_len(30L)) {
    p <- sieve_metadata_patch(shiny::isolate(s$meta), "item1", "label", paste("Label", i))
    sieve_apply_metadata_patch(s, p)
  }
  for (i in seq_len(10L)) sieve_remove_cases(s, i, paste("Decision", i))
  store <- sieve_replay_bundle(s)
  sources <- Filter(function(x) is.list(x) && is.data.frame(x$raw), store$data)
  expect_length(sources, 1L)
  expect_length(shiny::isolate(s$replay_operations), 41L)
  # No full raw or item matrix occurs in an individual revision.
  expect_true(all(vapply(store$revisions, function(x) is.null(x$raw) && is.null(x$items), logical(1))))
  # Immutable build provenance is stored once, referenced by each step.
  expect_length(store$engine_identity, 1L)
  expect_length(store$engine_steps, 41L)
  expect_lt(as.numeric(object.size(store$engine_identity)), 100000)
  expect_lt(as.numeric(object.size(store)), 3100000)
})

test_that("sidecar inputs and recorded outputs reject tampering", {
  s <- replay_integrity_state()
  sieve_log(s, "Analysis", "answer <- mean(items$item1)")
  directory <- tempfile("replay_bundle_")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  paths <- sieve_session_write(s, directory)
  old <- setwd(directory)
  on.exit(setwd(old), add = TRUE)
  env <- new.env(parent = baseenv())
  expect_no_error(sys.source(paths[["script"]], envir = env))
  expect_equal(env$answer, shiny::isolate(mean(s$items$item1)))
  writeBin(as.raw(c(1, 2, 3)), paths[["inputs"]])
  expect_error(sys.source(paths[["script"]], envir = new.env(parent = baseenv())), "does not match")
  store <- sieve_replay_bundle(s)
  key <- shiny::isolate(s$replay_steps[["1"]])
  sieve_replay_bind(store, key, env)
  env$items$item1[1L] <- 100
  expect_error(sieve_replay_verify(store, key, env), "item data do not match")
  sieve_replay_bind(store, key, env)
  env$meta$status[1L] <- "parked"
  expect_error(sieve_replay_verify(store, key, env), "item ledger does not match")
  altered <- store
  altered$revisions[[key]]$meta$status[1L] <- "parked"
  expect_error(sieve_replay_verify(altered, key, env), "recorded hash")
})

test_that("new imports clear every dataset-bound setting and undo history", {
  s <- replay_integrity_state()
  s$screening_spec <- list(min_run = 99L)
  s$sampling_design <- list(unit = "cluster", cluster = "id")
  s$scoring_policies <- list(A = list(min_count = 99L))
  s$metadata_edits <- list(list(note = "old"))
  s$efa_settings <- list(n_factors = 7L)
  s$norm_references <- list(old = TRUE)
  s$item_actions <- list(list(reason = "old"))
  d <- data.frame(itemA = stats::rnorm(60L), itemB = stats::rnorm(60L), itemC = stats::rnorm(60L))
  expect_true(sieve_load_frame(s, d, "Second sample", "d <- read.csv('unavailable-local.csv')")$ok)
  fresh <- sieve_new_state()
  for (field in c(
    "screening_spec",
    "sampling_design",
    "scoring_policies",
    "metadata_edits",
    "efa_settings",
    "norm_references",
    "item_actions"
  )) {
    expect_identical(shiny::isolate(s[[field]]), shiny::isolate(fresh[[field]]), info = field)
  }
  expect_no_error(replay_integrity_run(s))
})

test_that("already reversed loaded data are not reversed again by scoring syntax", {
  s <- replay_integrity_state()
  shiny::isolate({
    m <- s$meta
    m$reverse[1L] <- TRUE
    m$rev_lo[1L] <- 0
    m$rev_hi[1L] <- 6
    code <- sieve_scoring_code(m, raw = s$raw, processed = s$items)$code
    env <- new.env(parent = baseenv())
    env$dat <- s$raw
    eval(parse(text = code), env)
    expected <- rowMeans(s$items[, m$item[m$construct == "A"], drop = FALSE])
    expect_equal(env$dat[[env$score_name_map[["A"]]]], expected)
  })
})

test_that("a schema 2 project is migrated safely, and new records replay after its historical steps", {
  s <- replay_integrity_state()
  sieve_log(s, "Historical metadata", "stop('unrecoverable historical input')")
  snapshot <- sieve_project_snapshot(s)
  snapshot$schema_version <- 2L
  snapshot$other$replay_operations <- NULL
  snapshot$keys$content <- sieve_content_key(snapshot)
  upgraded <- sieve_project_migrate(snapshot)
  expect_identical(upgraded$schema_version, 3L)
  expect_identical(upgraded$migrated_from, 2L)
  t <- sieve_new_state()
  sieve_project_restore(t, upgraded)
  sieve_log(t, "New verified analysis", "new_n <- nrow(items)")
  env <- replay_integrity_run(t)
  expect_identical(env$new_n, 80L)
  expect_match(sieve_replay_script(t), "Historical step")
  snapshot$other$screening_spec <- list(min_run = 123L)
  expect_error(sieve_project_migrate(snapshot), "do not match")
})


test_that("view-only screening and initialized default controls keep the saved status", {
  s <- replay_integrity_state()
  s$confirmed <- TRUE
  s$estimator <- "MLR"
  opened <- sieve_new_state()
  sieve_project_restore(opened, sieve_project_snapshot(s))
  before <- sieve_project_fingerprint(opened)
  shiny::testServer(mod_screen_server, args = list(state = opened), {
    invisible(basis())
    invisible(careless())
  })
  expect_identical(sieve_project_fingerprint(opened), before)
  # Materializing the controls' default values is scientifically unchanged.
  opened$cfa_settings <- list(
    scope = "construct", structure = "first", ident = "varone",
    facets = character(0), all_facets = TRUE
  )
  opened$sem_settings <- list(outcome_types = list(), exogenous_missing = "engine_default")
  opened$reliability_settings <- list(
    scope = "construct", treatment = "study", missing = "study",
    denominator = "observed", ci_method = "recommended", conf_level = .95, B = 10000, show_alpha = FALSE
  )
  expect_identical(sieve_project_fingerprint(opened), before)
  opened$cfa_settings <- list(scope = "construct", structure = "higher", ident = "varone")
  expect_false(identical(sieve_project_fingerprint(opened), before))
})


test_that("both project Open routes apply the five-option rule alike: proposed ordered types switch, chosen ones are kept", {
  # Not on the CRAN check's clock (about 2 s); the full suite runs it.
  skip_on_cran()
  # Both routes apply the five-option rule to proposed types and keep the
  # types the researcher chose; they must agree with each other.
  open_both <- function(source) {
    original <- replay_integrity_state()
    m <- shiny::isolate(original$meta)
    m$type <- "ordered"
    m$type_source <- source
    original$meta <- m
    original$confirmed <- TRUE
    original$estimator <- "WLSMV (DWLS)"
    file <- tempfile(fileext = ".siever.rds")
    on.exit(unlink(file), add = TRUE)
    sieve_project_write(sieve_project_snapshot(original), file)
    upload <- list(name = "ordered.siever.rds", datapath = file)
    toolbar <- sieve_new_state()
    ingest <- sieve_new_state()
    shiny::testServer(mod_project_server, args = list(state = toolbar), {
      session$setInputs(file = upload)
      session$setInputs(restore = 1L)
    })
    shiny::testServer(mod_ingest_server, args = list(state = ingest), {
      session$setInputs(project_file = upload)
      session$setInputs(confirm_open = 1L)
    })
    expect_identical(shiny::isolate(ingest$meta$type), shiny::isolate(toolbar$meta$type))
    expect_identical(shiny::isolate(ingest$estimator), shiny::isolate(toolbar$estimator))
    expect_true(shiny::isolate(isTRUE(toolbar$confirmed) && isTRUE(ingest$confirmed)))
    list(type = shiny::isolate(toolbar$meta$type), estimator = shiny::isolate(toolbar$estimator))
  }
  chosen <- open_both("chosen")
  expect_identical(chosen$type, rep("ordered", 9L))
  expect_identical(chosen$estimator, "WLSMV (DWLS)")
  proposed <- open_both("proposed")
  expect_identical(proposed$type, rep("continuous", 9L))
  expect_identical(proposed$estimator, "ML with FIML")
})


test_that("the session code panel refreshes when a recorded action is added", {
  s <- replay_integrity_state()
  shiny::testServer(mod_repro_server, args = list(state = s), {
    expect_false(grepl("A newly recorded action", output$code, fixed = TRUE))
    sieve_log(state, "A newly recorded action", "n_latest <- nrow(items)")
    session$flushReact()
    expect_match(output$code, "A newly recorded action", fixed = TRUE)
    expect_match(output$code, "SIEVER::sieve_replay_read", fixed = TRUE)
    expect_false(grepl("item1 =", output$code, fixed = TRUE))
  })
})


test_that("self-contained inspection preserves non-native label encodings", {
  s <- replay_integrity_state()
  m <- shiny::isolate(s$meta)
  m$label[1L] <- iconv("Caf\u00e9", from = "UTF-8", to = "latin1")
  s$meta <- m
  sieve_log(s, "Inspect encoded labels", "encoded_mean <- mean(items$item1)")
  env <- replay_integrity_run(s)
  expect_identical(env$meta$label, m$label)
  expect_equal(env$encoded_mean, shiny::isolate(mean(s$items$item1)))
})


test_that("reopening the project dialog clears a canceled file preview", {
  original <- replay_integrity_state()
  file <- tempfile(fileext = ".siever.rds")
  on.exit(unlink(file), add = TRUE)
  sieve_project_write(sieve_project_snapshot(original), file)
  state <- sieve_new_state()
  shiny::testServer(mod_project_server, args = list(state = state), {
    session$setInputs(open = 1L)
    session$setInputs(file = list(name = "previous-selection.siever.rds", datapath = file))
    expect_match(output$open_preview$html, "previous-selection.siever.rds", fixed = TRUE)
    expect_false(is.null(pending()))
    # Canceling the browser modal changes no project state. Reopening it
    # must require a fresh file selection rather than offering the old one.
    session$setInputs(open = 2L)
    expect_null(pending())
    expect_null(output$open_preview)
    expect_null(state$raw)
  })
})


test_that("invalid respondent mappings are refused before replay data can recycle", {
  state <- replay_integrity_state()
  state$items <- shiny::isolate(state$items)[1:40, , drop = FALSE]
  expect_no_warning(expect_error(
    sieve_log(state, "Malformed direct-state rows", "n_bad <- nrow(items)"),
    "same number of loaded-data and item-data rows"
  ))
  expect_length(shiny::isolate(state$replay_data), 0L)
  expect_length(shiny::isolate(state$replay_states), 0L)
  expect_length(shiny::isolate(state$codelog), 0L)
  state <- replay_integrity_state()
  state$case_index <- rep(1L, 80L)
  expect_error(
    sieve_log(state, "Duplicated respondent mapping", "n_bad <- nrow(items)"),
    "identities must name each current row exactly once"
  )
  expect_length(shiny::isolate(state$replay_data), 0L)
})

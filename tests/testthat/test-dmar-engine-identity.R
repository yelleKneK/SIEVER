# The DMAR preflight checks the interface SIEVER needs (the arguments of the
# functions it calls), not only the version number, without pinning DMAR to
# one build.
test_that("DMAR preflight is deterministic and refuses a DMAR whose functions lack the arguments SIEVER calls", {
  withr::with_seed(75, {
    before <- .Random.seed
    expect_invisible(sieve_dmar_contract(refresh = TRUE))
    expect_identical(.Random.seed, before)
  })
  functions <- c("ss_aipe_reliability", "ss_aipe_rmsea", "sem_fit_measures", "irt_information",
    "reliability_omega", "parallel_analysis", "ci_bifactor_indices", "ci_proportion")
  old <- list2env(stats::setNames(lapply(functions, function(name) getExportedValue("DMAR", name)), functions),
    parent = emptyenv())
  old$ss_aipe_rmsea <- function(RMSEA, df, width, conf_level) NULL
  expect_error(sieve_dmar_contract(old, refresh = TRUE), "true_rmsea.*restart R", class = "sieve_dmar_compatibility_error")
  expect_error(sieve_dmar_require_columns(data.frame(estimate = 1), c("estimate", "ci_upper"), "HTMT"),
    "HTMT.*ci_upper", class = "sieve_dmar_schema_error")
  expect_error(sieve_dmar_require_terms(data.frame(term = c("ci_lower", "ci_lower"), value = 1:2),
    "ci_lower", "interval"), "exactly once", class = "sieve_dmar_schema_error")
})

test_that("immutable engine identity is loaded-code identity and survives repeated reads", {
  first <- sieve_engine_identity(c("SIEVER", "DMAR", "lavaan"))
  second <- sieve_engine_identity(c("SIEVER", "DMAR", "lavaan"))
  expect_identical(first, second)
  for (engine in first) {
    expect_identical(engine$status, "loaded")
    expect_match(engine$id, "^[0-9a-f]{64}$")
    expect_match(engine$namespace_sha256, "^[0-9a-f]{64}$")
    expect_match(engine$runtime_sha256, "^[0-9a-f]{64}$")
    expect_true(dir.exists(engine$namespace_path))
  }
  moved <- first
  moved$DMAR$namespace_path <- "/a/different/install/location"
  expect_identical(sieve_engine_keys(first), sieve_engine_keys(moved))
  changed <- first
  changed$DMAR$id <- paste0(if (startsWith(first$DMAR$id, "0")) "1" else "0", substring(first$DMAR$id, 2))
  expect_false(identical(sieve_engine_keys(first), sieve_engine_keys(changed)))
})

test_that("saved runs preserve engine identities and identical reruns do not accumulate duplicates", {
  # Skipped on CRAN to save check time (about 1 s); the full suite runs it.
  skip_on_cran()
  state <- siever_test_study()
  items <- shiny::isolate(state$items)
  meta <- shiny::isolate(state$meta)
  run <- sieve_run_cfa(sieve_cfa_spec(meta, reserved = names(items)), items)
  expect_true(sieve_has_solution(run))
  one <- sieve_register_analysis(state, run, "Original engine run")
  expect_identical(sieve_register_analysis(state, run), one)
  before <- shiny::isolate(state$analyses[[one]])
  changed <- run
  changed$engine_identity <- before$engine_identity
  changed$engine_identity$DMAR$id <- paste0(if (startsWith(before$engine_identity$DMAR$id, "0")) "1" else "0",
    substring(before$engine_identity$DMAR$id, 2))
  two <- sieve_register_analysis(state, changed, "A changed engine build")
  expect_false(identical(one, two))
  expect_identical(sieve_register_analysis(state, changed), two)
  expect_identical(shiny::isolate(state$analyses[[one]]), before)
  expect_length(shiny::isolate(state$analyses), 2L)
  file <- tempfile(fileext = ".siever.rds")
  on.exit(unlink(file), add = TRUE)
  sieve_project_write(sieve_project_snapshot(state), file)
  restored <- sieve_new_state()
  sieve_project_restore(restored, sieve_project_read(file))
  expect_identical(shiny::isolate(restored$analyses[[one]]$engine_identity), before$engine_identity)
  expect_identical(shiny::isolate(restored$analyses[[two]]$engine_identity), changed$engine_identity)
  # A real reopen changes the captured dataset identity. The new run may
  # receive a new ID, but repeated reads/reruns under that same identity do not.
  three <- sieve_register_analysis(restored, run)
  expect_identical(sieve_register_analysis(restored, run), three)
  expect_identical(shiny::isolate(restored$analyses[[one]]$estimates), before$estimates)
})

test_that("code disclosures retain the step's engines when a project is reopened", {
  state <- siever_test_study()
  sieve_log(state, "Identity history", "recorded_value <- 1")
  original <- sieve_code_snapshot(state)
  snapshot <- sieve_project_snapshot(state)
  restored <- sieve_new_state()
  sieve_project_restore(restored, snapshot)
  # Reopening the disclosure must not relabel the historical computation
  # with a new engine or current wall-clock time.
  historical <- sieve_code_record(restored, "Identity history")
  expect_identical(historical$engine_identity, original$engine_identity)
  expect_identical(historical$versions, original$versions)
  expect_identical(historical$recorded_at, original$recorded_at)
  store <- sieve_replay_bundle(restored)
  expect_identical(store$engine_identity[[store$engine_steps[[original$step]]]], original$engine_identity)
  expect_invisible(sieve_replay_engine_check(store, original$step))
  changed <- store
  changed$engine_identity[[changed$engine_steps[[original$step]]]]$DMAR$id <- "different"
  expect_warning(expect_false(sieve_replay_engine_check(changed, original$step)),
    "different engine builds: DMAR", class = "sieve_replay_engine_mismatch")
  expect_identical(store$engine_identity[[store$engine_steps[[original$step]]]], original$engine_identity)
  expect_match(paste(sieve_code_readme(original), collapse = "\n"), original$engine_identity$DMAR$id, fixed = TRUE)
})


test_that("identical logged code gets a new step after an engine build changes", {
  state <- siever_test_study()
  sieve_log(state, "Repeated calculation", "answer <- 1")
  first <- sieve_code_snapshot(state)
  sieve_log(state, "Repeated calculation", "answer <- 1")
  expect_length(sieve_script_records(state), 1L)
  different <- first$engine_identity
  different$DMAR$id <- "a new executing build"
  # The stand-in keeps the function's contract: engines that ran in another
  # R process (`executed`, the ordered-item MI step's mice and lavaan.mi)
  # replace this process's records of them.
  testthat::local_mocked_bindings(sieve_recording_engine_identity = function(executed = NULL) {
    identity <- different
    if (length(executed)) identity[names(executed)] <- executed
    identity
  })
  sieve_log(state, "Repeated calculation", "answer <- 1")
  expect_length(sieve_script_records(state), 2L)
  expect_identical(sieve_code_snapshot(state, step = 1L)$engine_identity, first$engine_identity)
  expect_identical(sieve_code_snapshot(state, step = 2L)$engine_identity, different)
  sieve_log(state, "Repeated calculation", "answer <- 1")
  expect_length(sieve_script_records(state), 2L)
})
test_that("headless analysis records freeze the executing engine identity", {
  x <- data.frame(a = c(1, 1, 2), b = c(1, 2, 2), c = c(1, 3, 2))
  result <- sieve_analyze("screening", data = x, min_run = 3L)
  expect_identical(result$engine_identity, sieve_engine_identity())
  for (package in c("SIEVER", "DMAR")) {
    identity <- result$engine_identity[[package]]
    expect_match(identity$id, "^[0-9a-f]{64}$")
    expect_identical(identity$namespace_path,
      normalizePath(getNamespaceInfo(asNamespace(package), "path"), mustWork = FALSE))
  }
  saved <- tempfile(fileext = ".rds")
  on.exit(unlink(saved), add = TRUE)
  saveRDS(result, saved)
  expect_identical(readRDS(saved)$engine_identity, result$engine_identity)
  expect_identical(summary(result)$engine_identity, result$engine_identity)
})

# The identity of an engine is the identity of its code: a key does not
# include the installed files, which change with every installation, and a
# background worker does not accept any package found at the session's
# path, whatever build it is.
siever_probe_package <- function(dir, body = "x + probe_tolerance", tolerance = "1e-8") {
  src <- file.path(dir, "sieveridprobe")
  unlink(src, recursive = TRUE)
  dir.create(file.path(src, "R"), recursive = TRUE)
  writeLines(c("Package: sieveridprobe", "Title: Identity Probe", "Version: 0.0.1",
    "Description: A two-object package for the engine identity tests.", "License: MIT + file LICENSE",
    "Encoding: UTF-8"), file.path(src, "DESCRIPTION"))
  writeLines("export(probe)", file.path(src, "NAMESPACE"))
  writeLines(c(sprintf("probe_tolerance <- %s", tolerance), sprintf("probe <- function(x) %s", body)),
    file.path(src, "R", "probe.R"))
  src
}
siever_probe_install <- function(src, lib) {
  dir.create(lib, showWarnings = FALSE, recursive = TRUE)
  out <- suppressWarnings(system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-docs", "--no-html",
    "--no-multiarch", "--no-test-load", paste0("--library=", shQuote(lib)), shQuote(src)), stdout = TRUE, stderr = TRUE))
  if (!file.exists(file.path(lib, "sieveridprobe", "Meta", "package.rds"))) {
    stop("The probe package was not installed: ", paste(utils::tail(out, 5L), collapse = " "))
  }
  invisible(lib)
}
siever_probe_identity <- function(lib) {
  loadNamespace("sieveridprobe", lib.loc = lib)
  on.exit({
    unloadNamespace("sieveridprobe")
    cache <- get(".siever_engine_identity_cache", asNamespace("SIEVER"))
    if (exists("sieveridprobe", envir = cache, inherits = FALSE)) rm(list = "sieveridprobe", envir = cache)
  }, add = TRUE)
  sieve_package_identity("sieveridprobe")
}

test_that("an engine key is made of the package, its version and its code, and of nothing an installation rewrites", {
  record <- list(package = "DMAR", status = "loaded", version = "1.1.0", namespace_sha256 = strrep("a", 64L),
    runtime_sha256 = strrep("b", 64L), remote_sha = NA_character_, packaged = "2026-11-13 12:00:00 UTC; builder",
    namespace_path = "/one/library/DMAR")
  key <- sieve_identity_id(record)
  expect_match(key, "^[0-9a-f]{64}$")
  for (field in c("runtime_sha256", "remote_sha", "packaged", "namespace_path")) {
    moved <- record
    moved[[field]] <- "something else"
    expect_identical(sieve_identity_id(moved), key, info = field)
  }
  for (field in c("package", "version", "namespace_sha256")) {
    changed <- record
    changed[[field]] <- "something else"
    expect_false(identical(sieve_identity_id(changed), key), info = field)
  }
  # The executing record is keyed the same way.
  loaded <- sieve_package_identity("DMAR")
  expect_identical(loaded$id, sieve_identity_id(loaded))
})

test_that("the hashed content of a namespace is its functions and plain constants, in an order no locale changes", {
  make <- function(first = TRUE, tolerance = 1e-8, body = quote(x + tolerance)) {
    e <- new.env()
    put <- list(
      function() assign("tolerance", tolerance, e),
      function() assign("Beta", eval(call("function", as.pairlist(alist(x = )), body)), e),
      function() assign("alpha_terms", data.frame(term = c("estimate", "se"), stringsAsFactors = FALSE), e),
      function() assign("state", new.env(), e),
      function() assign(".__S3MethodsTable__.", new.env(), e)
    )
    for (step in if (first) put else rev(put)) step()
    e
  }
  base <- sieve_namespace_content(make())
  expect_type(base, "raw")
  expect_identical(sieve_namespace_content(make(first = FALSE)), base)
  expect_false(identical(sieve_namespace_content(make(tolerance = 1e-7)), base))
  expect_false(identical(sieve_namespace_content(make(body = quote(x - tolerance))), base))
  expect_identical(sieve_identity_digest(base), sieve_identity_digest(sieve_namespace_content(make())))
  # A digest is of bytes: the same lines give the same digest in any session.
  expect_identical(sieve_identity_digest(c("a", "b")), sieve_identity_digest(charToRaw("a\nb")))
  expect_true(sieve_identity_plain(list(a = 1, b = factor("x"), c = data.frame(d = 1:2))))
  expect_false(sieve_identity_plain(list(a = 1, f = function() 1)))
  expect_false(sieve_identity_plain(structure(1, origin = globalenv())))
  # The header skipped in a constant's bytes is the 14 bytes that name the writing R.
  expect_identical(serialize(1L, NULL, xdr = TRUE, version = 2L)[1:6], as.raw(c(0x58, 0x0a, 0, 0, 0, 2)))
  expect_length(serialize(1L, NULL, xdr = TRUE, version = 2L), 26L)
})

test_that("reinstalling the same source keeps the engine key, and changed code or a changed constant gives another", {
  skip_on_cran()
  root <- withr::local_tempdir()
  src <- siever_probe_package(root)
  one <- siever_probe_identity(siever_probe_install(src, file.path(root, "lib1")))
  two <- siever_probe_identity(siever_probe_install(src, file.path(root, "lib2")))
  expect_identical(one$status, "loaded")
  expect_identical(two$id, one$id)
  expect_identical(two$namespace_sha256, one$namespace_sha256)
  # The installations themselves differ (each has its own Built field), and
  # the record keeps that beside the key.
  expect_false(identical(two$runtime_sha256, one$runtime_sha256))
  expect_false(identical(two$namespace_path, one$namespace_path))
  constant <- siever_probe_identity(siever_probe_install(siever_probe_package(root, tolerance = "1e-6"), file.path(root, "lib3")))
  expect_false(identical(constant$id, one$id))
  code <- siever_probe_identity(siever_probe_install(siever_probe_package(root, body = "x - probe_tolerance"), file.path(root, "lib4")))
  expect_false(identical(code$id, one$id))
  expect_false(identical(code$id, constant$id))
})

test_that("a worker refuses a package reinstalled at the session's path after the session loaded it", {
  skip_on_cran()
  root <- withr::local_tempdir()
  lib <- file.path(root, "lib")
  siever_probe_install(siever_probe_package(root), lib)
  loadNamespace("sieveridprobe", lib.loc = lib)
  withr::defer({
    unloadNamespace("sieveridprobe")
    cache <- get(".siever_engine_identity_cache", asNamespace("SIEVER"))
    cache$stamps[["sieveridprobe"]] <- NULL
  })
  paths <- sieve_worker_paths("sieveridprobe")
  stamp <- attr(paths$sieveridprobe, "stamp", exact = TRUE)
  expect_match(stamp, "^R [0-9.]+; .* \\| [0-9a-f]{32}$")
  expect_identical(sieve_loaded_stamp("sieveridprobe"), stamp)
  # Subsetting, as the DMAR-only jobs do, keeps the stamp with its path.
  expect_identical(attr(paths["sieveridprobe"][[1L]], "stamp", exact = TRUE), stamp)
  expect_invisible(sieve_worker_bootstrap(paths))
  # The session has used the first build, so its code is in memory; the path
  # then receives another build, and the session goes on computing with the first.
  probe <- get("probe", asNamespace("sieveridprobe"))
  expect_identical(probe(1), 1 + 1e-8)
  siever_probe_install(siever_probe_package(root, body = "x - probe_tolerance"), lib)
  expect_identical(probe(1), 1 + 1e-8)
  expect_identical(sieve_loaded_stamp("sieveridprobe"), stamp)
  expect_error(sieve_worker_bootstrap(sieve_worker_paths("sieveridprobe")),
    "not the installation this session loaded.*restart R")
  # A fresh R process, which is what a background job is, refuses it too.
  bootstrap <- sieve_worker_bootstrap
  environment(bootstrap) <- baseenv()
  expect_error(callr::r(function(paths, bootstrap) bootstrap(paths),
    args = list(paths = sieve_worker_paths("sieveridprobe"), bootstrap = bootstrap), libpath = .libPaths()),
    "not the installation this session loaded")
  # A path given without a stamp, and a package loaded from source, are not compared.
  expect_identical(unname(sieve_worker_bootstrap(list(sieveridprobe = file.path(root, "absent")), stamp_only = TRUE)), NA_character_)
})

test_that("the session records the installations it loaded when SIEVER loads", {
  cache <- get(".siever_engine_identity_cache", asNamespace("SIEVER"))
  expect_true(all(c("DMAR", "SIEVER") %in% names(cache$stamps)))
  paths <- sieve_worker_paths()
  expect_named(paths, c("DMAR", "SIEVER"))
  expect_identical(as.character(paths$DMAR), getNamespaceInfo(asNamespace("DMAR"), "path"))
  expect_identical(attr(paths$DMAR, "stamp", exact = TRUE), cache$stamps$DMAR$stamp)
})

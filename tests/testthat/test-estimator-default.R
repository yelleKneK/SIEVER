# Follow the item types ("auto"), the study's default for continuous items,
# is DMAR's default, ML with FIML; MLR with FIML, ML on the complete cases
# and WLSMV are choices; ordered and binary items take WLSMV. A saved study
# whose setting is "auto" and whose confirmation records MLR with FIML for
# continuous items keeps MLR, recorded as such, with a one-time note.

# Nine continuous Holzinger and Swineford tests with values missing at
# random in two tests, so FIML is exercised.
estim_items <- function() {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  d$x1[seq(1, 60, by = 3)] <- NA_real_
  d$x5[seq(2, 90, by = 4)] <- NA_real_
  d
}
estim_meta <- function(items, type = "continuous") {
  data.frame(
    item = names(items), label = names(items),
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = type, status = "active",
    reverse = FALSE, stringsAsFactors = FALSE
  )
}
estim_model <- paste(
  "Visual =~ x1 + x2 + x3",
  "Textual =~ x4 + x5 + x6",
  "Speed =~ x7 + x8 + x9",
  sep = "\n"
)
estim_html <- function(x) paste(as.character(x$html %||% x), collapse = "")

test_that("Follow the item types resolves continuous items to ML with FIML and keeps WLSMV for ordered or binary items", {
  r <- sieve_estimator_resolve(rep("continuous", 9), n = 301, choice = "auto")
  expect_identical(r$estimator, "ML")
  expect_identical(r$missing, "fiml")
  expect_identical(r$label, "ML with FIML")
  expect_false(any(r$categorical))
  # The reason names DMAR's default, says the standard errors and the test
  # are normal theory, and points to MLR with FIML for markedly non-normal
  # items as the researcher's choice.
  expect_match(r$reason, "maximum likelihood (ML) with full information maximum likelihood (FIML)", fixed = TRUE)
  expect_match(r$reason, "DMAR's default", fixed = TRUE)
  expect_match(r$reason, "normal theory", fixed = TRUE)
  expect_match(r$reason, "For markedly non-normal items (strongly skewed or heavy-tailed), choose MLR with FIML", fixed = TRUE)
  expect_false(grepl("robust maximum likelihood (MLR) with full", r$reason, fixed = TRUE))

  ordered <- sieve_estimator_resolve(c(rep("continuous", 3), "ordered"), choice = "auto")
  expect_identical(ordered$estimator, "WLSMV")
  expect_identical(ordered$missing, "listwise")
  expect_identical(ordered$categorical, c(FALSE, FALSE, FALSE, TRUE))
  binary <- sieve_estimator_resolve(rep("binary", 4), choice = "auto")
  expect_identical(binary$estimator, "WLSMV")
  expect_identical(binary$basis, "tetrachoric")
  expect_match(ordered$reason, "offers ML or MLR with FIML instead", fixed = TRUE)

  # Every other choice is kept, with its own estimator and treatment.
  expect_identical(unname(sieve_estimator_choices()), c("auto", "mlr", "ml_fiml", "ml", "wlsmv"))
  expect_match(names(sieve_estimator_choices())[1], "ML with FIML (DMAR's default) when every item is continuous", fixed = TRUE)
  got <- lapply(c(mlr = "mlr", ml_fiml = "ml_fiml", ml = "ml", wlsmv = "wlsmv"), function(ch) {
    x <- sieve_estimator_resolve(rep("continuous", 9), choice = ch)
    c(x$estimator, x$missing, x$label)
  })
  expect_identical(got$mlr, c("MLR", "fiml", "MLR with FIML"))
  expect_identical(got$ml_fiml, c("ML", "fiml", "ML with FIML"))
  expect_identical(got$ml, c("ML", "listwise", "ML (complete cases)"))
  expect_identical(got$wlsmv, c("WLSMV", "listwise", "WLSMV (DWLS)"))
  expect_match(sieve_estimator_resolve(rep("continuous", 9), choice = "mlr")$reason,
    "which suit markedly non-normal items", fixed = TRUE)
  # The tab controls and the study's control read the same words.
  expect_match(sieve_estimator_choice_text("auto"), "ML with FIML if every item is continuous", fixed = TRUE)
  expect_match(sieve_estimator_choice_text("mlr"), "for markedly non-normal items", fixed = TRUE)
})

test_that("the default CFA is ML with FIML, equal to lavaan called directly, with the standard family of fit indices", {
  items <- estim_items()
  meta <- estim_meta(items)
  spec <- sieve_cfa_spec(meta, reserved = names(items))
  expect_identical(spec$estimator, "ML")
  expect_identical(spec$missing, "fiml")
  expect_identical(spec$estimator_choice, "auto")
  # "auto" and the named ML with FIML are one specification.
  expect_identical(spec$key, sieve_cfa_spec(meta, reserved = names(items), estimator = "ml_fiml")$key)
  expect_false(identical(spec$key, sieve_cfa_spec(meta, reserved = names(items), estimator = "mlr")$key))

  run <- sieve_run_model(spec, items)
  expect_identical(run$status$code, "ok")
  ref <- lavaan::cfa(estim_model, data = items, estimator = "ML", missing = "ml", std.lv = TRUE)
  expect_equal(lavaan::coef(run$fit), lavaan::coef(ref), tolerance = 1e-8)
  expect_equal(sqrt(diag(lavaan::vcov(run$fit))), sqrt(diag(lavaan::vcov(ref))), tolerance = 1e-7)
  # Under ML lavaan reports the standard model test and no scaled one; with
  # FIML its robust CFI, TLI and RMSEA are the incomplete data corrections
  # of Zhang and Savalei (2023), not corrections for nonnormality, and the
  # page says so.
  fm_ref <- lavaan::fitMeasures(ref, c(
    "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "cfi.robust", "tli.robust", "rmsea.robust"
  ))
  fm <- sieve_fit_select(sieve_fit_families(run$fit))
  expect_identical(attr(fm, "test_family"), "standard")
  expect_identical(attr(fm, "family"), "robust")
  expect_equal(unname(fm[c("chisq", "df", "pvalue")]), unname(fm_ref[c("chisq", "df", "pvalue")]), tolerance = 1e-8)
  expect_equal(unname(fm[c("cfi", "tli", "rmsea")]),
    unname(fm_ref[c("cfi.robust", "tli.robust", "rmsea.robust")]), tolerance = 1e-8)
  fam <- sieve_fit_families(run$fit)$families
  expect_false(isTRUE(fam$scaled$available))
  expect_equal(unname(fam$standard$values[c("cfi", "rmsea")]), unname(fm_ref[c("cfi", "rmsea")]), tolerance = 1e-8)
  note <- paste(sieve_fit_note(fm), collapse = " ")
  expect_match(note, "corrected for the incomplete data only (Zhang & Savalei, 2023), not for nonnormality", fixed = TRUE)
  expect_match(note, "The model chi square is the standard statistic.", fixed = TRUE)
  # The lavaan options read back from the fit are ML with FIML, standard
  # errors and test.
  opts <- lavaan::lavInspect(run$fit, "options")
  expect_identical(opts$estimator, "ML")
  expect_identical(opts$missing, "ml")
  expect_identical(opts$se, "standard")

  # MLR with FIML is still there for the researcher who chooses it, and is
  # lavaan's MLR: the same estimates, robust standard errors.
  mlr <- sieve_run_model(sieve_cfa_spec(meta, reserved = names(items), estimator = "mlr"), items)
  ref_mlr <- lavaan::cfa(estim_model, data = items, estimator = "MLR", missing = "ml", std.lv = TRUE)
  expect_equal(lavaan::coef(mlr$fit), lavaan::coef(ref_mlr), tolerance = 1e-8)
  expect_equal(sqrt(diag(lavaan::vcov(mlr$fit))), sqrt(diag(lavaan::vcov(ref_mlr))), tolerance = 1e-7)
  expect_equal(lavaan::coef(mlr$fit), lavaan::coef(run$fit), tolerance = 1e-6)
  fm_mlr <- sieve_fit_select(sieve_fit_families(mlr$fit))
  expect_identical(attr(fm_mlr, "family"), "robust")
  expect_identical(attr(fm_mlr, "test_family"), "scaled")
  expect_match(paste(sieve_fit_note(fm_mlr), collapse = " "), "with nonnormal or incomplete data", fixed = TRUE)
})

test_that("the recorded R names ML with FIML explicitly and replays to the fit", {
  items <- estim_items()
  meta <- estim_meta(items)
  spec <- sieve_cfa_spec(meta, reserved = names(items))
  code <- sieve_cfa_code(spec, data_name = "items")
  text <- paste(code, collapse = "\n")
  expect_match(text, "estimator = \"ML\"", fixed = TRUE)
  expect_match(text, "missing = \"fiml\"", fixed = TRUE)
  expect_match(text, "ML with full information ML for incomplete cases (DMAR's default)", fixed = TRUE)
  expect_false(grepl("SIEVER:::", text, fixed = TRUE))
  env <- new.env()
  env$items <- items
  eval(parse(text = code), envir = env)
  expect_equal(lavaan::coef(env$cfa_fit), lavaan::coef(sieve_run_model(spec, items)$fit), tolerance = 1e-10)

  # A SIEVER call recorded for continuous items names "ml_fiml", which
  # does not depend on what "auto" means; "auto" with an ordered item is
  # kept, and a named choice is recorded as named.
  expect_identical(sieve_estimator_record_choice("auto", meta), "ml_fiml")
  expect_identical(sieve_estimator_record_choice("auto", estim_meta(items, c(rep("continuous", 8), "ordered"))), "auto")
  expect_identical(sieve_estimator_record_choice("mlr", meta), "mlr")
  expect_identical(sieve_estimator_record_choice("ml", meta), "ml")
})

test_that("ML with FIML is costed and guarded as MLR's fit measures need, and ML on the complete cases is not", {
  # The premise, read from lavaan itself: the fit measures of an ML fit by
  # FIML form p* by p* matrices of the moments, those of ML on the complete
  # cases do not. With lavaan's default tests Browne's residual statistic
  # forms the asymptotic covariance matrix (lav_object_gamma()); SIEVER's own
  # FIML fits ask for the standard test alone (sieve_spec_default_test()),
  # and their fit measures still compute the corrected indices
  # of Zhang and Savalei (2023) from the unrestricted model's information
  # matrices (lav_fit_fiml_corrected()), so the need stays MLR's.
  skip_if_not(exists("lav_object_gamma", envir = asNamespace("lavaan"), inherits = FALSE))
  skip_if_not(exists("lav_fit_fiml_corrected", envir = asNamespace("lavaan"), inherits = FALSE))
  items <- estim_items()[, 1:6]
  model <- "Visual =~ x1 + x2 + x3\nTextual =~ x4 + x5 + x6"
  formed <- new.env()
  formed$dims <- integer(0)
  suppressMessages(trace("lav_object_gamma", where = asNamespace("lavaan"), print = FALSE,
    exit = bquote({
      g <- returnValue()
      assign("dims", c(get("dims", envir = .(formed)),
        vapply(g, function(m) if (is.matrix(m)) nrow(m) else NA_integer_, integer(1))), envir = .(formed))
    })))
  on.exit(suppressMessages(untrace("lav_object_gamma", where = asNamespace("lavaan"))), add = TRUE)
  fiml <- lavaan::cfa(model, data = items, estimator = "ML", missing = "ml", std.lv = TRUE)
  formed$dims <- integer(0)
  invisible(lavaan::fitMeasures(fiml))
  p <- 6L
  expect_true((p * (p + 3L) / 2L) %in% formed$dims)
  cc <- lavaan::cfa(model, data = items, estimator = "ML", missing = "listwise", std.lv = TRUE)
  formed$dims <- integer(0)
  invisible(lavaan::fitMeasures(cc))
  expect_length(formed$dims, 0L)
  # SIEVER's fit: no Browne statistic, but the corrected FIML indices, which
  # the complete cases do not compute.
  corrected <- new.env()
  corrected$calls <- 0L
  suppressMessages(trace("lav_fit_fiml_corrected", where = asNamespace("lavaan"), print = FALSE,
    tracer = bquote(assign("calls", get("calls", envir = .(corrected)) + 1L, envir = .(corrected)))))
  on.exit(suppressMessages(untrace("lav_fit_fiml_corrected", where = asNamespace("lavaan"))), add = TRUE)
  own <- lavaan::cfa(model, data = items, estimator = "ML", missing = "ml", std.lv = TRUE, test = "standard")
  formed$dims <- integer(0)
  invisible(lavaan::fitMeasures(own))
  expect_length(formed$dims, 0L)
  expect_gt(corrected$calls, 0L)
  corrected$calls <- 0L
  invisible(lavaan::fitMeasures(cc))
  expect_identical(corrected$calls, 0L)

  # The rate class and the memory need follow.
  expect_identical(sieve_demand_class("ML", "fiml"), "MLR")
  expect_identical(sieve_demand_class("ML", "listwise"), "ML")
  expect_identical(sieve_demand_class("ML"), "ML")
  expect_identical(sieve_demand_class("MLR", "fiml"), "MLR")
  expect_identical(sieve_demand_wait("cfa", 35, "ML", missing = "fiml"), sieve_demand_wait("cfa", 35, "MLR"))
  expect_identical(sieve_ml_bytes(303, "ML", missing = "fiml"), sieve_ml_bytes(303, "MLR"))
  expect_lt(sieve_ml_bytes(303, "ML", missing = "listwise"), sieve_ml_bytes(303, "MLR"))
  refused <- sieve_mlr_guard(303, "ML", missing = "fiml", n = 500, limit = 32e9)
  expect_match(refused, "under ML with FIML, the corrected fit indices lavaan reports for a FIML fit", fixed = TRUE)
  expect_false(grepl("Browne", refused, fixed = TRUE))
  expect_match(refused, "Second, ML on the complete cases", fixed = TRUE)
  # No rank check under ML: its standard errors are not a sandwich.
  expect_false(grepl("rank deficient", refused, fixed = TRUE))
  expect_null(sieve_mlr_guard(303, "ML", missing = "listwise", n = 500, limit = 32e9))
  expect_null(sieve_mlr_guard(30, "ML", missing = "fiml", n = 20, limit = 32e9))
  # A specification carries its treatment to the guard.
  meta <- estim_meta(estim_items())
  spec <- sieve_cfa_spec(meta, reserved = paste0("x", 1:9))
  withr::local_options(sieve.memory_limit = 1)
  expect_match(sieve_spec_mlr_guard(spec, n = 301), "under ML with FIML", fixed = TRUE)
})

test_that("the network page's default, ML with FIML, is FIML in psychonetrics and is timed as ML on complete data", {
  complete <- sieve_network_engine("ML", "fiml", NULL, data.frame(a = 1:3, b = 3:1))
  expect_identical(complete$estimator, "FIML")
  expect_identical(sieve_network_wait_class(complete), "ML")
  incomplete <- sieve_network_engine("ML", "fiml", NULL, data.frame(a = c(1, NA, 3), b = 3:1))
  expect_identical(incomplete$estimator, "FIML")
  expect_identical(sieve_network_wait_class(incomplete), "FIML")
  expect_identical(sieve_network_wait_class(sieve_network_engine("MLR", "fiml", NULL, data.frame(a = 1:3))), "MLR")
})

# ---- A saved study whose "auto" records MLR with FIML ----------------------

# A saved project whose study setting is "auto" and whose confirmation
# records "MLR with FIML" for continuous items, with the content key it was
# written with.
estim_legacy_snapshot <- function(st = siever_test_study()) {
  snap <- unclass(sieve_project_snapshot(st))
  snap$estimator <- "MLR with FIML"
  snap$study_estimator_choice <- "auto"
  snap$keys$content <- sieve_content_key(snap)
  structure(snap, class = "sieve_project")
}

test_that("a saved study whose \"auto\" records MLR with FIML keeps MLR, recorded as such, and refits nothing", {
  st <- siever_test_study()
  # Follow the item types records ML with FIML for this study.
  expect_identical(shiny::isolate(st$estimator), "ML with FIML")
  expect_false(sieve_snapshot_legacy_auto(sieve_project_snapshot(st)))
  legacy <- estim_legacy_snapshot(st)
  expect_true(sieve_snapshot_legacy_auto(legacy))
  expect_identical(sieve_project_check(legacy), character(0))
  expect_match(sieve_project_describe(legacy)[["estimator_setting"]],
    "with continuous items recorded as fit under MLR with FIML; opens as MLR with FIML", fixed = TRUE)

  path <- tempfile(fileext = ".siever.rds")
  on.exit(unlink(path), add = TRUE)
  sieve_project_write(legacy, path)
  read <- sieve_project_read(path)
  opened <- sieve_new_state()
  sieve_project_open(opened, read, source = basename(path))
  shiny::isolate({
    expect_identical(opened$estimator_choice, "mlr")
    expect_identical(opened$estimator, "MLR with FIML")
    expect_true(isTRUE(sieve_confirmation_status(opened)$confirmed))
    expect_match(opened$project$estimator_kept$text,
      "Follow the item types, which gives continuous items ML with FIML, DMAR's default", fixed = TRUE)
    log <- paste(opened$codelog, collapse = "\n")
    expect_match(log, sieve_legacy_auto_note(), fixed = TRUE)
    expect_match(log, "to rerun\n# it as it was fit, pass estimator = \"mlr\"", fixed = TRUE)
    # Every model the study fits is MLR's: the specification the study's
    # setting gives is the one the saved results were fit with.
    choice <- sieve_estimator_effective("study", opened)
    spec <- sieve_cfa_spec(opened$meta, reserved = names(opened$items), estimator = choice)
    expect_identical(spec$estimator, "MLR")
    expect_identical(spec$missing, "fiml")
    expect_identical(spec$key, sieve_cfa_spec(opened$meta, reserved = names(opened$items), estimator = "mlr")$key)
    run <- sieve_run_model(spec, opened$items)
    ref <- lavaan::cfa(run$spec$model %||% spec$model, data = opened$items, estimator = "MLR", missing = "ml", std.lv = TRUE)
    expect_equal(sqrt(diag(lavaan::vcov(run$fit))), sqrt(diag(lavaan::vcov(ref))), tolerance = 1e-7)
    # The session script says what a recorded "auto" meant.
    script <- sieve_replay_script(opened)
    expect_match(script, "items under MLR with FIML in each SIEVER call with estimator = \"auto\", which gives them ML", fixed = TRUE)
  })

  # The one click takes the default: the setting follows the item types,
  # the recorded estimator follows, it is logged, and the note goes.
  e <- sieve_legacy_auto_release(opened)
  shiny::isolate({
    expect_identical(e$label, "ML with FIML")
    expect_identical(opened$estimator_choice, "auto")
    expect_identical(opened$estimator, "ML with FIML")
    expect_null(opened$project$estimator_kept)
    expect_match(paste(opened$codelog, collapse = "\n"),
      "The study's estimator setting (Ingest tab, step 4) is now \"auto\" (it was \"mlr\").", fixed = TRUE)
  })
})

test_that("only \"auto\" with MLR recorded is kept at MLR: \"auto\" with ML recorded, a chosen setting and ordered items are left as saved", {
  st <- siever_test_study()
  # "auto" with ML with FIML recorded.
  now <- sieve_new_state()
  sieve_project_restore(now, sieve_project_snapshot(st))
  expect_identical(shiny::isolate(now$estimator_choice), "auto")
  expect_null(shiny::isolate(now$project$estimator_kept))
  # A setting the researcher chose is kept as chosen.
  chosen <- unclass(sieve_project_snapshot(st))
  chosen$estimator <- "ML (complete cases)"
  chosen$study_estimator_choice <- "ml"
  chosen$keys$content <- sieve_content_key(chosen)
  expect_false(sieve_snapshot_legacy_auto(chosen))
  # Ordered items resolve to WLSMV; MLR recorded with them is left as saved.
  ordered <- unclass(sieve_project_snapshot(st))
  ordered$meta$type[1:3] <- "ordered"
  ordered$estimator <- "MLR with FIML"
  expect_false(sieve_snapshot_legacy_auto(ordered))
  # A project without the setting reads as "auto", and with MLR recorded
  # it is kept at MLR.
  older <- unclass(estim_legacy_snapshot(st))
  older$study_estimator_choice <- NULL
  expect_true(sieve_snapshot_legacy_auto(older))
  # The shared study bundle states how the study was fit.
  bundled <- sieve_study_bundle_project(estim_legacy_snapshot(st), shiny::isolate(st$raw), shiny::isolate(st$items))
  expect_identical(bundled$study_estimator_choice, "mlr")
})

test_that("the Ingest tab shows the note once, beside the study, with the two answers", {
  st0 <- siever_test_study()
  path <- tempfile(fileext = ".siever.rds")
  on.exit(unlink(path), add = TRUE)
  sieve_project_write(estim_legacy_snapshot(st0), path)
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "ready", basis = "theoretical", palette = "nd", five_continuous = TRUE)
    session$setInputs(open_project = 1L)
    session$setInputs(project_file = list(name = "legacy.siever.rds", datapath = path))
    expect_match(estim_html(output$project_preview), "opens as MLR with FIML", fixed = TRUE)
    session$setInputs(confirm_open = 1L)
    expect_identical(state$estimator_choice, "mlr")
    note <- estim_html(output$estimator_kept)
    expect_match(note, "Estimator kept as saved.", fixed = TRUE)
    expect_match(note, "Use ML with FIML (Follow the item types)", fixed = TRUE)
    expect_match(note, "Keep MLR with FIML", fixed = TRUE)
    # Keeping it clears the note and leaves MLR with FIML in force.
    session$setInputs(legacy_auto_keep = 1L)
    expect_identical(estim_html(output$estimator_kept), "")
    expect_identical(state$estimator_choice, "mlr")
  })
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "ready", basis = "theoretical", palette = "nd", five_continuous = TRUE)
    session$setInputs(open_project = 1L)
    session$setInputs(project_file = list(name = "legacy.siever.rds", datapath = path))
    session$setInputs(confirm_open = 1L)
    session$setInputs(legacy_auto_release = 1L)
    expect_identical(state$estimator_choice, "auto")
    expect_identical(state$estimator, "ML with FIML")
    expect_identical(estim_html(output$estimator_kept), "")
    expect_match(estim_html(output$grid_note), "now follows the item types (ML with FIML)", fixed = TRUE)
  })
})

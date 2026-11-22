# ML with FIML as an Estimator choice ("ml_fiml"), the MLR refusal before
# every MLR fit of the CV and Invariance tab (the split, the invariance
# ladder, the longitudinal rungs), and the words for items ("Exclude" and
# "Include"; "Remove" and "Restore" are the respondents' words).
#
# Author: Ken Kelley

skip_if_not_installed("shiny")
skip_if_not_installed("lavaan")
withr::local_options(sieve.cores = 2L) # at most two workers, whatever the cores

ue_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))

# The nine Holzinger-Swineford tests of lavaan with incomplete cases: every
# seventh row misses x1, every eleventh x5, every thirteenth x9 (no row
# misses everything), so FIML and the complete cases differ.
ue_hs_missing <- function() {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  d$x1[seq(1L, nrow(d), by = 7L)] <- NA
  d$x5[seq(2L, nrow(d), by = 11L)] <- NA
  d$x9[seq(3L, nrow(d), by = 13L)] <- NA
  d
}

ue_meta <- function(items = paste0("x", 1:9), type = "continuous") {
  data.frame(
    item = items, label = items,
    construct = rep(c("Visual", "Textual", "Speed"), each = 3L)[seq_along(items)],
    facet = NA_character_, type = type, status = "active",
    reverse = FALSE, stringsAsFactors = FALSE
  )
}

# The bundled nine tests, loaded and confirmed, with the study's estimator
# set to `choice` and, optionally, incomplete cases and ordered types.
ue_state <- function(choice = "ml_fiml", missing = FALSE, ordered = character(0)) {
  fx <- sieve_hs9_fixture()
  d <- fx$data
  if (isTRUE(missing)) {
    its <- fx$item_cols
    d[[its[1]]][seq(1L, nrow(d), by = 7L)] <- NA
    d[[its[5]]][seq(2L, nrow(d), by = 11L)] <- NA
    d[[its[9]]][seq(3L, nrow(d), by = 13L)] <- NA
  }
  s <- sieve_new_state()
  s$dataset_name <- fx$name
  s$raw <- d
  s$items <- d[, fx$item_cols, drop = FALSE]
  s$id_vars <- fx$id_vars
  s$roles <- sieve_declared_roles(d, fx$item_cols)
  m <- sieve_build_meta(d, fx$item_cols, fx$theory_map, labels = fx$labels)
  if (length(ordered)) m$type[m$item %in% ordered] <- "ordered"
  s$meta <- m
  s$theory <- list(k = fx$theory_k, label = fx$theory_lbl, map = fx$theory_map)
  shiny::isolate(sieve_new_dataset(s))
  s$case_index <- seq_len(nrow(d))
  s$estimator_choice <- choice
  s$palette <- "nd"
  s$thresholds <- sieve_default_thresholds()
  shiny::isolate(sieve_confirm_types(s, origin = "button"))
  s
}

# ---- 1. ML with FIML is an Estimator choice -----------------------------------

test_that("ML with FIML is a choice of the Estimator control and resolves to ML with FIML, with its trade-off stated", {
  own <- sieve_estimator_choices()
  expect_identical(
    unname(own[names(own) == "ML with FIML, every item treated as continuous"]), "ml_fiml"
  )
  expect_true("ml_fiml" %in% sieve_estimator_select_choices())
  expect_true("ml_fiml" %in% sieve_estimator_select_choices(study = FALSE))
  expect_identical(sieve_estimator_effective("study", list(estimator_choice = "ml_fiml")), "ml_fiml")
  r <- sieve_estimator_resolve(c("continuous", "ordered", NA), n = 301, choice = "ml_fiml")
  expect_identical(r$estimator, "ML")
  expect_identical(r$missing, "fiml")
  expect_identical(r$label, "ML with FIML")
  expect_identical(r$categorical, c(FALSE, FALSE, FALSE))
  expect_identical(r$basis, "Pearson")
  expect_match(r$reason, "ML with FIML was chosen with the Estimator control", fixed = TRUE)
  expect_match(r$reason, "an incomplete case contributes what it holds", fixed = TRUE)
  expect_match(r$reason, "the standard errors and the test statistic (and the RMSEA and CFI built on it) are normal theory", fixed = TRUE)
  # The ordered item it covers is named, with the attenuation.
  expect_match(r$reason, "the 1 item typed ordered or binary included", fixed = TRUE)
  # A small sample (n = 80) adds no small-sample note to the chosen ML with
  # FIML.
  small <- sieve_estimator_resolve(rep("continuous", 3), n = 80, choice = "ml_fiml")
  expect_false(grepl("sample is small", small$reason, fixed = TRUE))
  # The choice "ml" is ML on the complete cases.
  ml <- sieve_estimator_resolve(rep("continuous", 3), choice = "ml")
  expect_identical(c(ml$estimator, ml$missing, ml$label), c("ML", "listwise", "ML (complete cases)"))
  # The words every tab shows for the choice.
  expect_identical(sieve_estimator_choice_text("ml_fiml"), "ML with FIML (items treated as continuous)")
  expect_identical(
    sieve_estimator_source_text("ml_fiml"),
    "In force here: ML with FIML (items as continuous), chosen on this tab."
  )
  e <- sieve_estimator_resolve(c("continuous", "continuous"), n = 301, choice = "ml_fiml")
  expect_identical(c(e$label, e$basis), c("ML with FIML", "Pearson"))
})

test_that("a CFA under ML with FIML is lavaan's ML with missing = \"ml\", and differs from MLR and from the complete cases", {
  d <- ue_hs_missing()
  meta <- ue_meta()
  spec <- sieve_cfa_spec(meta, reserved = names(d), estimator = "ml_fiml")
  expect_identical(c(spec$estimator, spec$missing, spec$estimator_choice), c("ML", "fiml", "ml_fiml"))
  run <- sieve_run_model(spec, d)
  expect_identical(run$status$code, "ok")
  want <- lavaan::cfa(spec$model, data = d, estimator = "ML", missing = "ml", std.lv = TRUE)
  expect_identical(names(lavaan::coef(run$fit)), names(lavaan::coef(want)))
  expect_equal(unname(lavaan::coef(run$fit)), unname(lavaan::coef(want)), tolerance = 1e-10)
  fm <- c("chisq", "df", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "cfi", "srmr")
  expect_equal(lavaan::fitMeasures(run$fit, fm), lavaan::fitMeasures(want, fm), tolerance = 1e-10)
  pe <- function(f) lavaan::parameterEstimates(f)$se
  expect_equal(pe(run$fit), pe(want), tolerance = 1e-10)
  # Every row with a response is used (FIML), with normal-theory standard
  # errors and test statistic: the standard chi square comes first and is
  # the one the fit measures use, and no corrected statistic is computed.
  # lavaan 0.7-3 also lists Browne's residual-based normal-theory statistic
  # ("browne.residual.nt.model") by default; it is an alternative
  # normal-theory test, not a robust one, so it is allowed beside the
  # standard statistic.
  expect_identical(lavaan::lavInspect(run$fit, "nobs"), nrow(d))
  opt <- lavaan::lavInspect(run$fit, "options")
  expect_identical(opt$se, "standard")
  expect_identical(opt$test[1L], "standard")
  expect_true(all(opt$test %in% c("standard", "browne.residual.nt.model")))
  expect_identical(sieve_lavaan_corrected_tests(opt$test), character(0))
  tests <- lavaan::lavInspect(run$fit, "test")
  expect_identical(tests[[1L]]$test, "standard")
  expect_equal(unname(lavaan::fitMeasures(run$fit, "chisq")), tests[[1L]]$stat, tolerance = 1e-10)
  expect_false("chisq.scaled" %in% names(lavaan::fitMeasures(run$fit)))
  # Not MLR (the same estimates, other standard errors) ...
  mlr <- sieve_run_model(sieve_cfa_spec(meta, reserved = names(d), estimator = "mlr"), d)
  expect_equal(unname(lavaan::coef(mlr$fit)), unname(lavaan::coef(run$fit)), tolerance = 1e-6)
  expect_false(isTRUE(all.equal(pe(mlr$fit), pe(run$fit))))
  # ... and not ML on the complete cases (other rows, other estimates).
  cc <- sieve_run_model(sieve_cfa_spec(meta, reserved = names(d), estimator = "ml"), d)
  expect_lt(lavaan::lavInspect(cc$fit, "nobs"), nrow(d))
  expect_false(isTRUE(all.equal(unname(lavaan::coef(cc$fit)), unname(lavaan::coef(run$fit)))))
  # The recorded R fits the same model and says why.
  code <- sieve_cfa_code(spec)
  expect_true(any(grepl("estimator = \"ML\"", code, fixed = TRUE) & grepl("missing = \"fiml\"", code, fixed = TRUE)))
  expect_true(any(grepl("ML with full information ML, every indicator treated as continuous; normal-theory standard errors and test statistic", code, fixed = TRUE)))
})

test_that("every switch on the estimator choice reads ML with FIML as a continuous treatment", {
  st <- list(estimator_choice = "ml_fiml")
  # The correlation basis of Screening, Dimensionality, EFA and Items.
  expect_identical(sieve_study_basis_request(st), "pearson")
  expect_match(
    sieve_basis_source_text("auto", st, c("ordered", "ordered")),
    "which treats the items as continuous (its Estimator control is set to ML with FIML)",
    fixed = TRUE
  )
  # The reliability treatment of the methods draft.
  expect_identical(sieve_draft_treatment("study", "ml_fiml"), "continuous")
  # The split reliability of cross-validation: the ordered items are
  # treated as continuous, as under MLR.
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  d[] <- lapply(d, function(v) as.integer(cut(v, 5L)))
  meta <- ue_meta(type = "ordered")
  expect_equal(
    sieve_split_omega(d, meta, estimator = "ml_fiml"),
    sieve_split_omega(d, meta, estimator = "mlr")
  )
  # No switch on the choice codes leaves it out: every switch() in the
  # package source with an `mlr = ,` arm has an `ml_fiml` arm.
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  skip_if_not(dir.exists(file.path(root, "R")), "no package source beside the tests")
  missed <- character(0)
  for (f in list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)) {
    lines <- readLines(f, warn = FALSE)
    for (at in grep("^\\s*mlr = ,\\s*$", lines)) {
      window <- lines[max(1L, at - 3L):min(length(lines), at + 3L)]
      if (!any(grepl("ml_fiml", window, fixed = TRUE))) missed <- c(missed, sprintf("%s:%d", basename(f), at))
    }
  }
  expect_identical(missed, character(0))
})

test_that("the methods draft, the assistant and the Reliability tab's study treatment follow ML with FIML", {
  st <- ue_state("ml_fiml", missing = TRUE, ordered = c("t1_visual_perception", "t2_cubes", "t4_lozenges"))
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  draft <- sieve_methods_draft(items, meta, estimator = "ml_fiml")
  expect_match(draft, paste0(
    "maximum likelihood (ML), with every item treated as continuous, with full information maximum ",
    "likelihood, so incomplete cases contribute to the CFA, and normal-theory standard errors and ",
    "test statistic (without MLR's robust corrections)"
  ), fixed = TRUE)
  expect_match(draft, "ML with full information maximum likelihood for incomplete cases", fixed = TRUE)
  # The assistant's CFA plan fits ML with FIML; its reliability plan treats
  # every item as continuous, with FIML.
  cfa <- sieve_assistant_plan(st, "Fit the measurement model", task = "cfa")
  expect_identical(c(cfa$specs[[1L]]$estimator, cfa$specs[[1L]]$missing), c("ML", "fiml"))
  expect_null(cfa$specs[[1L]]$ordered)
  rel <- sieve_assistant_plan(st, "Estimate reliability", task = "reliability")
  expect_true(all(vapply(rel$specs, function(sp) all(sp$types == "continuous"), logical(1))))
  expect_true(all(vapply(rel$specs, function(sp) identical(sp$missing, "fiml"), logical(1))))
  # The Reliability tab's "study" treatment (mod_reliab.R).
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(treatment = "study")
    expect_identical(treatment(), "continuous")
  })
})

test_that("a saved project keeps ML with FIML as the study's estimator setting", {
  st <- ue_state("ml_fiml")
  snap <- sieve_project_snapshot(st)
  expect_identical(snap$study_estimator_choice, "ml_fiml")
  expect_identical(sieve_project_check(snap), character(0))
  path <- withr::local_tempfile(fileext = ".siever.rds")
  sieve_project_write(snap, path)
  back <- sieve_project_read(path)
  expect_identical(back$study_estimator_choice, "ml_fiml")
  expect_identical(sieve_project_describe(back)[["estimator_setting"]], "ML with FIML (items treated as continuous)")
  fresh <- sieve_new_state()
  sieve_project_restore(fresh, back)
  expect_identical(shiny::isolate(fresh$estimator_choice), "ml_fiml")
  expect_identical(shiny::isolate(sieve_estimator_effective("study", fresh)), "ml_fiml")
  expect_identical(sieve_lavaan_spec(shiny::isolate(fresh$meta), "ml_fiml")$missing, "fiml")
})

test_that("the session script replays a CFA fit under ML with FIML in a clean process", {
  skip_on_cran()
  skip_if_not_installed("DMAR")
  st <- ue_state("ml_fiml", missing = TRUE)
  got <- new.env()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    session$setInputs(fit_go = 1)
    invisible(output$fit_tbl)
    got$fit <- fit()
  })
  expect_true(inherits(got$fit, "lavaan"))
  opt <- lavaan::lavInspect(got$fit, "options")
  expect_identical(c(opt$estimator, opt$missing, opt$se), c("ML", "ml", "standard"))
  spec <- attr(got$fit, "sieve_spec")
  d <- shiny::isolate(st$items)
  d$age_years <- shiny::isolate(st$raw$age_years)
  want <- suppressWarnings(lavaan::cfa(spec$model, data = d, estimator = "ML", missing = "ml", std.lv = TRUE, aux = "age_years"))
  expect_equal(unname(lavaan::coef(got$fit)), unname(lavaan::coef(want)), tolerance = 1e-8)
  shiny::testServer(mod_repro_server, args = list(state = st), {
    got$script <- output$code
  })
  expect_match(got$script, "estimator = \"ML\"", fixed = TRUE)
  expect_match(got$script, "missing = \"fiml\"", fixed = TRUE)
  res <- compat_replay_run(got$script, state = st)
  testthat::expect(
    is.na(res$failed_step) && identical(res$status, 0L),
    sprintf("the script stopped at %s with: %s", res$failed_step, res$error)
  )
  expect_identical(names(res$coef), names(lavaan::coef(want)))
  expect_equal(unname(res$coef), unname(lavaan::coef(want)), tolerance = 1e-8)
})

# ---- 2. The MLR refusal reaches the fits of the CV and Invariance tab ------

test_that("the calibration and validation split refuses an MLR model before the split is drawn, with the model tabs' message", {
  withr::local_options(sieve.memory_limit = 1e6)
  calls <- 0L
  testthat::local_mocked_bindings(
    cfa = function(...) {
      calls <<- calls + 1L
      stop("lavaan::cfa must not run")
    },
    .package = "lavaan"
  )
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- ue_meta()
  set.seed(20261022)
  before <- get(".Random.seed", envir = globalenv())
  # The subject is MLR's refusal, so MLR is chosen (Follow the item types
  # gives continuous items ML with FIML).
  out <- sieve_cross_validate(d, meta, holdout = .5, seed = NULL, estimator = "mlr")
  expect_identical(calls, 0L)
  # No seed was drawn: nothing started.
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_null(out$table)
  expect_identical(
    out$error,
    sieve_mlr_guard(9, "MLR", n = 150, npar = sieve_free_parameters(9, 3))
  )
  expect_match(out$error, "^Not fit: MLR's scaled test statistic")
  # The default, ML with FIML, is refused for the matrices its corrected
  # FIML fit indices form, with that reason.
  out_default <- sieve_cross_validate(d, meta, holdout = .5, seed = NULL)
  expect_identical(calls, 0L)
  expect_identical(
    out_default$error,
    sieve_mlr_guard(9, "ML", n = 150, npar = sieve_free_parameters(9, 3), missing = "fiml")
  )
  expect_match(out_default$error, "^Not fit: under ML with FIML, the corrected fit indices lavaan reports for a FIML fit")
})

test_that("a split whose halves are too small for MLR's sandwich is refused, and ML with FIML fits it", {
  d <- lavaan::HolzingerSwineford1939[1:40, paste0("x", 1:9)]
  meta <- ue_meta()
  refused <- sieve_cross_validate(d, meta, holdout = .5, seed = 11, estimator = "mlr")
  expect_match(refused$error, "^Not fit: MLR's robust standard errors would be rank deficient: the model has 30 free parameters and there are 20 respondents")
  expect_match(refused$error, "Second, ML with FIML (the Estimator control's choice \"ML with FIML\")", fixed = TRUE)
  fitted <- sieve_cross_validate(d, meta, holdout = .5, seed = 11, estimator = "ml_fiml")
  expect_null(fitted$error)
  expect_identical(c(fitted$n_cal, fitted$n_val), c(20L, 20L))
  expect_identical(nrow(fitted$table), 2L)
  # The default, ML with FIML, has no sandwich to refuse.
  by_default <- sieve_cross_validate(d, meta, holdout = .5, seed = 11)
  expect_null(by_default$error)
  expect_identical(by_default$table, fitted$table)
})

test_that("the invariance ladder and the anchor comparison refuse an MLR model before any rung is fit", {
  skip_if_not_installed("DMAR")
  withr::local_options(sieve.memory_limit = 1e6)
  calls <- 0L
  testthat::local_mocked_bindings(
    measurement_invariance = function(...) {
      calls <<- calls + 1L
      stop("DMAR::measurement_invariance must not run")
    },
    .package = "DMAR"
  )
  hs <- lavaan::HolzingerSwineford1939
  meta <- ue_meta()
  r <- sieve_fit_invariance(hs[, c(meta$item, "school")], "school", meta, estimator = "mlr")
  expect_identical(calls, 0L)
  expect_true(isTRUE(r$refused))
  expect_true(isTRUE(r$precondition))
  expect_identical(
    r$error,
    sieve_mlr_guard(9, "MLR", n = 301, npar = 2 * sieve_free_parameters(9, 3))
  )
  expect_error(
    sieve_anchor_sensitivity(hs[, c(meta$item, "school")], "school", meta,
      anchor_sets = list(
        First = c("x1", "x2", "x4", "x5", "x7", "x8"),
        Last = c("x2", "x3", "x5", "x6", "x8", "x9")
      ),
      rationale = "Two anchor sets stated before the data were examined", discovery = FALSE,
      estimator = "mlr"
    ),
    "Anchor set 'First': Not fit: MLR's scaled test statistic", fixed = TRUE
  )
  expect_identical(calls, 0L)
})

test_that("a longitudinal rung refuses an MLR model before it is fit", {
  withr::local_options(sieve.memory_limit = 1e6)
  real_cfa <- lavaan::cfa
  calls <- 0L
  testthat::local_mocked_bindings(
    cfa = function(..., do.fit = TRUE) {
      if (isTRUE(do.fit)) {
        calls <<- calls + 1L
        stop("lavaan::cfa must not fit")
      }
      real_cfa(..., do.fit = do.fit)
    },
    .package = "lavaan"
  )
  map <- data.frame(
    construct = "Democracy", wave = rep(c("1960", "1965"), each = 4L),
    item = rep(c("press", "opposition", "elections", "legislature"), 2L),
    variable = paste0("y", 1:8), wave_order = rep(1:2, each = 4L)
  )
  r <- sieve_run_longitudinal(lavaan::PoliticalDemocracy, map,
    level = "metric",
    justification = "The same four indicators were rated at both waves",
    estimator = "mlr"
  )
  expect_identical(calls, 0L)
  expect_null(r$fit)
  expect_identical(r$status$code, "failed")
  expect_identical(
    r$error,
    sieve_mlr_guard(8, "MLR", n = 75, npar = sieve_free_parameters(8, 2, means = TRUE))
  )
  expect_true(is.na(r$row_flow$used))
})

# ---- 3. Words: Exclude and Include for items --------------------------------

test_that("items are excluded and included, never removed or restored, in these strings", {
  meta <- ue_meta()
  gone <- meta[meta$item != "x8", ]
  s <- sieve_stale_sentence(sieve_ledger_analytic(meta), sieve_ledger_analytic(gone))
  expect_identical(s, "x8 no longer in the ledger")
  st <- ue_state("auto")
  draft <- sieve_methods_draft(shiny::isolate(st$items), shiny::isolate(st$meta))
  expect_match(draft, "the item-retention narrative (what was excluded and why)", fixed = TRUE)
  expect_false(grepl("what was dropped", draft, fixed = TRUE))
  # A project whose removed respondents overlap the current rows names them
  # as removed respondents.
  snap <- unclass(sieve_project_snapshot(st))
  snap$other$removed_cases <- list(list(row = 1L, reason = "test"))
  snap$keys$content <- sieve_content_key(snap)
  problems <- sieve_project_check(structure(snap, class = "sieve_project"))
  expect_true("the current and removed respondent row identities disagree" %in% problems)
  # The Ingest tab has no Prepare items panel: an item is reverse scored and
  # returned to its scoring with its Reverse? cell in the item grid (Yes,
  # then No), or a set of items with the Reverse? row above the grid, and its
  # response bounds are declared in the Response bounds row and cells.
  # "Restore" stays the respondents' word.
  expect_false(exists("mod_ingest_prepare_ui", envir = asNamespace("SIEVER"), inherits = FALSE))
  ui <- as.character(mod_ingest_ui("ingest"))
  html <- ue_text(ui)
  expect_match(html, paste(
    "Set Reverse? to Yes to reverse score an item within its response bounds, and to No to",
    "return it to its scoring: the reflection uses the same bounds both ways"
  ), fixed = TRUE)
  expect_match(html, "Response bounds: to", fixed = TRUE)
  expect_match(html, "Declare Clear Reverse?", fixed = TRUE)
  for (id in c("bounds_lo", "bounds_hi", "types_scope", "bounds_apply", "bounds_clear", "keying_to", "keying_apply")) {
    expect_match(ui, sprintf('id="ingest-%s"', id), fixed = TRUE)
  }
  expect_false(grepl("Prepare items", html, fixed = TRUE))
  expect_false(grepl("Undo reverse scoring", html, fixed = TRUE))
  expect_false(grepl("Restore original", html, fixed = TRUE))
  # A reversal's own message says how it is taken back, in the same words.
  rec <- sieve_apply_reverse(st, "t2_cubes", origin = "ingest")$applied$t2_cubes
  msg <- sieve_reverse_message(rec)
  expect_match(msg, "undo this (Reverse? No)", fixed = TRUE)
  expect_match(msg, "Response bounds cell on the Ingest tab", fixed = TRUE)
  expect_false(grepl("Restore", msg, fixed = TRUE))
})

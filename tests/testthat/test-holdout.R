# An optional validation reservation before development, honored by every
# development page. The tests below check that the rows are drawn by a seed
# and leave the analysis through the one respondent path with the reason
# "reserved for validation" and a marker, every development result reads the
# calibration rows only, the ordinary Restore refuses a reserved row, a saved
# project and the session script reproduce the reserved rows from the seed,
# the CV and Invariance page evaluates a named frozen version on the reserved
# rows once and labels it, and small samples are refused with the reason.
#
# Author: Ken Kelley

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("psych")
skip_if_not_installed("shiny")

# The nine tests (n = 301, no missing values, so the complete cases are the
# calibration rows), confirmed continuous under the study's setting (ML
# with FIML); id is the identifier and school a grouping variable.
hd_state <- function(n = NULL) {
  fx <- sieve_hs9_fixture()
  if (!is.null(n)) fx$data <- fx$data[seq_len(n), , drop = FALSE]
  siever_test_study(fx, name = "Nine tests", configure = function(s) {
    s$roles <- sieve_propose_roles(fx$data)
  })
}

hd_text <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))

# The expected draw: positions in the analysis set, with the draw's own
# local stream (sieve_with_seed(), withr::with_seed()).
hd_expected <- function(n, proportion, seed) {
  withr::with_seed(seed, sort(sample.int(n, round(n * proportion))))
}

# Every lavaan fit's number of observations, every DMAR::fa_exploratory()
# call's N (the rows of its data, or the N given with a matrix) and every
# psych::fa's n.obs made while a test runs, so a page's n can be read from
# the engine it called. (psych::fa is counted to show that no page calls
# it.)
hd_engines <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$nobs <- integer(0)
  seen$fa <- integer(0)
  seen$dmar <- integer(0)
  real_dmar <- DMAR::fa_exploratory
  testthat::local_mocked_bindings(
    fa_exploratory = function(data = NULL, nfactors = 1L, S = NULL, N = NULL, ...) {
      seen$dmar <- c(seen$dmar, if (is.null(S)) nrow(data) else as.integer(N))
      real_dmar(data = data, nfactors = nfactors, S = S, N = N, ...)
    },
    .package = "DMAR", .env = env
  )
  real_lavaan <- lavaan::lavaan
  testthat::local_mocked_bindings(
    lavaan = function(...) {
      fit <- real_lavaan(...)
      seen$nobs <- c(seen$nobs, as.integer(sum(lavaan::lavInspect(fit, "nobs"))))
      fit
    },
    .package = "lavaan", .env = env
  )
  real_fa <- psych::fa
  testthat::local_mocked_bindings(
    fa = function(r, ..., n.obs = NA) {
      seen$fa <- c(seen$fa, as.integer(n.obs))
      real_fa(r, ..., n.obs = n.obs)
    },
    .package = "psych", .env = env
  )
  testthat::local_mocked_bindings(sieve_demand_note = function(...) invisible(NULL), .env = env)
  seen
}

# ---- The draw ----------------------------------------------------------------

test_that("the sizes follow round(n * proportion) overall and largest remainder within strata, and the draw restores the caller's stream", {
  sizes <- sieve_holdout_sizes(301, .3)
  expect_identical(sizes$stratum, "(all)")
  expect_identical(sizes$reserved, 90L)
  expect_identical(sizes$calibration, 211L)
  school <- lavaan::HolzingerSwineford1939$school
  by <- sieve_holdout_sizes(301, .3, strata = school)
  expect_identical(by$stratum, c("Pasteur", "Grant-White"))
  expect_identical(by$n, c(156L, 145L))
  # Largest remainder: 46.8 and 43.5 give 46 and 43, and the one row left
  # goes to the larger remainder (.8), so the total is round(301 * .3) = 90.
  expect_identical(by$reserved, c(47L, 43L))
  # A missing group value forms a stratum of its own, with no label (the
  # keyed strata; test-input-validation.R has the cases).
  school[1:5] <- NA
  missing_stratum <- sieve_holdout_sizes(301, .3, strata = school)
  expect_identical(missing_stratum$stratum, c(NA, "Pasteur", "Grant-White"))
  expect_identical(missing_stratum$missing, c(TRUE, FALSE, FALSE))
  expect_identical(missing_stratum$n, c(5L, 151L, 145L))
  expect_error(sieve_holdout_sizes(301, 1), "strictly between 0 and 1")
  expect_error(sieve_holdout_sizes(301, .3, strata = school[1:10]), "one value per row")

  set.seed(31)
  before <- .Random.seed
  drawn <- sieve_holdout_draw(seq_len(301), .3, seed = 11)
  expect_identical(.Random.seed, before)
  expect_identical(drawn$rows, hd_expected(301, .3, 11))
  expect_identical(drawn$positions, drawn$rows)
  expect_true(drawn$rng$supplied)
  expect_identical(drawn$rng$seed, 11L)
  # Row identities are returned, not positions.
  index <- c(5L, 9L, 20:400)
  again <- sieve_holdout_draw(index, .3, seed = 11)
  expect_identical(again$rows, index[hd_expected(length(index), .3, 11)])
  # With seed = NULL the seed is drawn from the caller's stream and recorded.
  set.seed(77)
  expected_seed <- sample.int(.Machine$integer.max, 1L)
  set.seed(77)
  free <- sieve_holdout_draw(seq_len(301), .3)
  expect_false(free$rng$supplied)
  expect_identical(free$rng$seed, expected_seed)
  expect_identical(sieve_holdout_draw(seq_len(301), .3, seed = free$rng$seed)$rows, free$rows)
  # Stratified: every stratum contributes its own count.
  school <- lavaan::HolzingerSwineford1939$school
  strat <- sieve_holdout_draw(seq_len(301), .3, seed = 11, strata = school)
  expect_identical(sum(school[strat$rows] == "Pasteur"), 47L)
  expect_identical(sum(school[strat$rows] == "Grant-White"), 43L)
  expect_identical(strat$sizes$reserved, c(47L, 43L))
  expect_error(sieve_holdout_draw(c(1L, 1L, 2L), .3, seed = 1), "repeats row number 1; each row may appear once",
    fixed = TRUE)
})

# ---- The reservation in the state ----------------------------------------------

test_that("a reservation removes exactly the drawn rows through the respondent path, and each development page reads the calibration rows", {
  seen <- hd_engines()
  s <- hd_state()
  d <- shiny::isolate(s$items)
  set.seed(5)
  before <- .Random.seed
  out <- sieve_reserve_holdout(s, .3, seed = 11)
  expect_identical(.Random.seed, before)
  expect_null(out$refused)
  expected <- hd_expected(301, .3, 11)
  expect_identical(out$reserved, expected)
  rec <- shiny::isolate(sieve_holdout(s))
  expect_identical(rec$id, "R1")
  expect_identical(rec$status, "reserved")
  expect_identical(rec$rows, expected)
  expect_identical(rec$n_reserved, 90L)
  expect_identical(rec$n_calibration, 211L)
  expect_false(rec$after_development)
  expect_identical(rec$rng$seed, 11L)
  expect_identical(rec$dataset_id, shiny::isolate(s$dataset_id))
  # The one respondent path: the rows left raw and items together, with
  # the reason and the marker, and nothing else moved.
  expect_identical(shiny::isolate(s$case_index), setdiff(1:301, expected))
  expect_identical(nrow(shiny::isolate(s$items)), 211L)
  expect_identical(sieve_data_key(shiny::isolate(s$items)), sieve_data_key(d[setdiff(1:301, expected), , drop = FALSE]))
  expect_identical(sieve_data_key(shiny::isolate(s$raw)), sieve_data_key(shiny::isolate(hd_state()$raw)[setdiff(1:301, expected), , drop = FALSE]))
  held <- shiny::isolate(s$removed_cases)
  expect_length(held, 90L)
  expect_identical(vapply(held, function(r) r$row, integer(1)), expected)
  expect_true(all(vapply(held, function(r) identical(r$reason, "reserved for validation"), logical(1))))
  expect_true(all(vapply(held, function(r) identical(r$holdout, "R1"), logical(1))))
  expect_identical(shiny::isolate(sieve_holdout_n_sentence(s)),
    "n = 211 calibration rows; 90 reserved for validation (reservation R1), read by no development result.")
  # The log names the reservation, the seed and the draw.
  log <- shiny::isolate(s$codelog)
  expect_match(paste(log, collapse = "\n"), "reserve 90 of 301 respondents for validation (reservation R1, proportion 0.3, seed 11)", fixed = TRUE)
  expect_match(paste(log, collapse = "\n"), "sieve_holdout_draw(case_index, proportion = ", fixed = TRUE)

  # The shared correlation basis, the EFA, the Items diagnostics and the
  # CFA read the 211 calibration rows, without further change.
  basis <- shiny::isolate(sieve_state_basis(s))
  expect_identical(as.integer(basis$n), 211L)
  expect_identical(as.integer(basis$n_rows), 211L)
  shiny::testServer(mod_efa_server, args = list(state = s), {
    session$setInputs(n_factors = 3L, fm = "ml", rotation = "oblimin", cor_type = "auto")
    session$setInputs(efa_go = 1)
    expect_identical(as.integer(efa_run()$fit$method$N), 211L)
  })
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    expect_false(is.null(diag()$items))
    expect_identical(diag()$n_obs, 211L)
  })
  expect_true(length(seen$dmar) > 0L)
  expect_true(all(seen$dmar == 211L))
  expect_length(seen$fa, 0L)
  shiny::testServer(mod_cfa_server, args = list(state = s), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    session$setInputs(fit_go = 1)
  })
  expect_true(length(seen$nobs) > 0L)
  expect_true(all(seen$nobs == 211L))

  # A second reservation while one is in force is refused.
  twice <- sieve_reserve_holdout(s, .3, seed = 12)
  expect_match(twice$refused, "R1 is already in force", fixed = TRUE)
  expect_length(shiny::isolate(s$holdout), 1L)
})

test_that("the ordinary Restore refuses a reserved row with the reason; Release returns them exactly", {
  s <- hd_state()
  d <- shiny::isolate(s$items)
  raw <- shiny::isolate(s$raw)
  out <- sieve_reserve_holdout(s, .3, seed = 11)
  rows <- out$reserved
  # The Screening page's restore (the status cell, Restore the selected
  # respondents, the Removed respondents table) all call this service.
  refused <- sieve_set_case_status(s, rows[1:2], "included", "try")
  expect_length(refused$restored, 0L)
  expect_match(refused$refused, "rows 3, 6 of the data as loaded are reserved for validation (reservation R1)", fixed = TRUE)
  expect_match(refused$refused, "Release the reservation", fixed = TRUE)
  expect_identical(nrow(shiny::isolate(s$items)), 211L)
  # Mixed with an ordinary removal: the reserved row still refuses the whole restore.
  sieve_set_case_status(s, 1L, "excluded", "flagged")
  expect_identical(nrow(shiny::isolate(s$items)), 210L)
  mixed <- sieve_set_case_status(s, c(1L, rows[1]), "included", "try")
  expect_match(mixed$refused, "reserved for validation", fixed = TRUE)
  expect_identical(nrow(shiny::isolate(s$items)), 210L)
  expect_identical(sieve_set_case_status(s, 1L, "included", "back")$restored, 1L)
  expect_null(sieve_set_case_status(s, 1L, "included", "back")$refused)

  rel <- sieve_release_holdout(s)
  expect_null(rel$refused)
  expect_identical(rel$restored, rows)
  expect_identical(rel$record$status, "released")
  expect_null(shiny::isolate(sieve_holdout(s)))
  expect_null(shiny::isolate(sieve_holdout_n_sentence(s)))
  expect_null(shiny::isolate(s$case_index))
  expect_length(shiny::isolate(s$removed_cases), 0L)
  expect_identical(shiny::isolate(s$items), d)
  expect_identical(shiny::isolate(s$raw), raw)
  # The record is kept, released; a second release has nothing to do.
  expect_identical(shiny::isolate(s$holdout)[[1L]]$status, "released")
  expect_match(sieve_release_holdout(s)$refused, "no validation reservation is in force", fixed = TRUE)
  expect_match(paste(shiny::isolate(s$codelog), collapse = "\n"), "release validation reservation R1: restore 90 reserved respondents", fixed = TRUE)
  # A new reservation after a release is R2.
  expect_identical(sieve_reserve_holdout(s, .3, seed = 11)$record$id, "R2")
})

test_that("a reservation is refused, before any row is drawn, when a half would be too small or cannot carry the model", {
  # 30 rows at .5: 15 per half, under the 20-row rule of the split.
  s <- hd_state(30)
  set.seed(9)
  before <- .Random.seed
  out <- sieve_reserve_holdout(s, .5, seed = 11)
  expect_match(out$refused, "15 reserved and 15 calibration rows; each half needs at least 20 rows", fixed = TRUE)
  expect_identical(.Random.seed, before)
  expect_null(shiny::isolate(sieve_holdout(s)))
  expect_length(shiny::isolate(s$holdout), 0L)
  expect_identical(nrow(shiny::isolate(s$items)), 30L)
  expect_length(shiny::isolate(s$codelog), 0L)
  # 60 rows at .5: 30 per half, which the MLR rank refusal (free parameters
  # not below n) refuses for the study's estimator; ML with FIML is not refused.
  # (The study's setting gives ML with FIML, so the study chooses MLR here.)
  s <- hd_state(60)
  s$estimator_choice <- "mlr"
  out <- sieve_reserve_holdout(s, .5, seed = 11)
  expect_match(out$refused, "the smaller half (30 rows) cannot carry the current model", fixed = TRUE)
  expect_identical(nrow(shiny::isolate(s$items)), 60L)
  plan <- shiny::isolate(sieve_holdout_plan(s, .5))
  expect_identical(plan$reserved, 30L)
  expect_match(plan$refusal, "cannot carry the current model", fixed = TRUE)
  s$estimator_choice <- "ml_fiml"
  expect_null(shiny::isolate(sieve_holdout_plan(s, .5))$refusal)
  expect_null(sieve_reserve_holdout(s, .5, seed = 11)$refused)
  # An invalid proportion and an unknown column are refused with the reason.
  s <- hd_state()
  expect_match(sieve_reserve_holdout(s, 1.5)$refused, "strictly between 0 and 1", fixed = TRUE)
  expect_match(sieve_reserve_holdout(s, .3, stratify = "nowhere")$refused, "nowhere is not a column", fixed = TRUE)
  expect_length(shiny::isolate(s$holdout), 0L)
})

test_that("a stratified reservation draws within the grouping variable, and one drawn after results exist says so", {
  s <- hd_state()
  # The shared correlation basis (a passive step, computed on view and
  # computed again from the calibration rows) is screening, not development.
  shiny::isolate(sieve_state_basis(s))
  expect_false(sieve_reserve_holdout(s, .3, seed = 11)$record$after_development)
  sieve_release_holdout(s)
  # A requested development result before the reservation: the Items
  # diagnostics, a logged analysis step.
  shiny::testServer(mod_winnow_server, args = list(state = s), expect_false(is.null(diag()$items)))
  out <- sieve_reserve_holdout(s, .3, seed = 11, stratify = "school")
  expect_null(out$refused)
  rec <- out$record
  expect_identical(rec$stratify, "school")
  school <- as.character(sieve_hs9_fixture()$data$school)
  expect_identical(rec$sizes$stratum, unique(school))
  expect_identical(rec$sizes$reserved, c(47L, 43L))
  expect_identical(rec$n_reserved, 90L)
  for (level in unique(school)) {
    expect_identical(sum(school[rec$rows] == level), rec$sizes$reserved[rec$sizes$stratum == level])
  }
  direct <- sieve_holdout_draw(seq_len(301), .3, seed = 11, strata = sieve_hs9_fixture()$data$school)
  expect_identical(rec$rows, direct$rows)
  expect_identical(rec$id, "R2")
  expect_true(rec$after_development)
  expect_true(length(rec$preceded_by$steps) >= 1L)
  expect_true(any(grepl("^Items:", rec$preceded_by$steps)))
  expect_false(any(grepl("Correlation basis", rec$preceded_by$steps, fixed = TRUE)))
  expect_match(paste(shiny::isolate(s$codelog), collapse = "\n"), "reserved after development began", fixed = TRUE)
  expect_match(paste(shiny::isolate(s$codelog), collapse = "\n"), 'strata = d[["school"]]', fixed = TRUE)
})

# ---- Saved projects and the session script ----------------------------------------

test_that("a saved project and the session script reproduce the same reserved rows from the seed", {
  s <- hd_state()
  out <- sieve_reserve_holdout(s, .3, seed = 11)
  rows <- out$reserved
  snap <- sieve_project_snapshot(s)
  expect_length(unlist(sieve_project_check(snap)), 0L)
  expect_identical(unname(sieve_project_describe(snap)[["rows"]]),
    "211 in the analysis (301 loaded, 90 removed on the Screening tab, 90 of them reserved for validation)")
  back <- sieve_new_state()
  sieve_project_restore(back, snap, source = "test")
  rec <- shiny::isolate(sieve_holdout(back))
  expect_false(is.null(rec))
  expect_identical(rec$rows, rows)
  expect_identical(rec$rng$seed, 11L)
  expect_identical(rec$dataset_id, shiny::isolate(back$dataset_id))
  expect_identical(shiny::isolate(back$case_index), setdiff(1:301, rows))
  held <- shiny::isolate(back$removed_cases)
  expect_true(all(vapply(held, function(r) identical(r$holdout, "R1"), logical(1))))
  expect_match(sieve_set_case_status(back, rows[1], "included", "try")$refused, "reserved for validation", fixed = TRUE)
  # A project whose reservation names a row that is not held is refused.
  bad <- snap
  bad$other$holdout[[1L]]$rows <- c(rows, 1L)
  expect_match(paste(unlist(sieve_project_check(bad)), collapse = " "), "validation reservation names rows that are not held", fixed = TRUE)

  # The session script draws the rows again from the seed and checks them.
  script <- shiny::isolate(sieve_replay_script(s))
  expect_match(script, "reserved <- SIEVER::sieve_holdout_draw(case_index, proportion = ", fixed = TRUE)
  expect_match(script, "seed = 11L", fixed = TRUE)
  env <- new.env(parent = baseenv())
  expect_no_error(suppressWarnings(capture.output(eval(parse(text = script), env))))
  expect_identical(env$reserved, rows)
  expect_identical(env$case_index, setdiff(1:301, rows))
  expect_identical(env$removed_cases$case_index, rows)
  expect_identical(env$holdout[[1L]]$rows, rows)
  # Released, the script restores them.
  sieve_release_holdout(s)
  script <- shiny::isolate(sieve_replay_script(s))
  env <- new.env(parent = baseenv())
  expect_no_error(suppressWarnings(capture.output(eval(parse(text = script), env))))
  expect_identical(env$case_index, 1:301)
  expect_length(env$removed_cases$case_index, 0L)
})

# ---- Evaluation on the reserved rows ---------------------------------------------

test_that("the evaluation uses the reserved rows exactly once, labels the version, and matches a direct fit", {
  seen <- hd_engines()
  s <- hd_state()
  none <- sieve_holdout_evaluate(s, "mlr")
  expect_match(none$error, "No validation reservation is in force", fixed = TRUE)
  out <- sieve_reserve_holdout(s, .3, seed = 11)
  rows <- out$reserved
  fits_before <- length(seen$nobs)
  ev <- sieve_holdout_evaluate(s, "mlr")
  expect_null(ev$error)
  expect_identical(ev$id, "E1")
  expect_identical(ev$version, "v1")
  expect_identical(ev$label, "specified before evaluation")
  expect_false(ev$historical)
  expect_false(ev$repeated)
  expect_identical(ev$rows, rows)
  expect_identical(ev$n_val, 90L)
  expect_identical(ev$n_cal, 211L)
  expect_null(ev$result$rng)
  expect_true(length(seen$nobs) > fits_before)
  # Each half was fit on its own rows.
  expect_true(all(seen$nobs[-seq_len(fits_before)] %in% c(211L, 90L)))
  # The calibration half of the evaluation and the reserved half agree with
  # lavaan on the same rows, digit for digit.
  fx <- sieve_hs9_fixture()
  d <- fx$data
  model <- paste(vapply(names(fx$theory_map), function(k) {
    sprintf("%s =~ %s", k, paste(fx$theory_map[[k]], collapse = " + "))
  }, character(1)), collapse = "\n")
  cal <- lavaan::cfa(model, data = d[setdiff(1:301, rows), ], std.lv = TRUE, estimator = "MLR", missing = "fiml")
  val <- lavaan::cfa(model, data = d[rows, ], std.lv = TRUE, estimator = "MLR", missing = "fiml")
  suffix <- if (ev$result$family == "standard") "" else paste0(".", ev$result$family)
  indices <- paste0(c("cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"), suffix)
  # The RMSEA interval is at 90%, the RMSEA level (DMAR's default for it
  # and lavaan's).
  level <- list(rmsea.ci.level = 0.90)
  expect_equal(unname(as.numeric(ev$result$table[1L, c("CFI", "RMSEA", "RMSEA.lo", "RMSEA.hi")])),
    unname(lavaan::fitMeasures(cal, indices, fm.args = level)), tolerance = 1e-10)
  expect_equal(unname(as.numeric(ev$result$table[2L, c("CFI", "RMSEA", "RMSEA.lo", "RMSEA.hi")])),
    unname(lavaan::fitMeasures(val, indices, fm.args = level)), tolerance = 1e-10)
  expect_equal(ev$result$table$n, c(211L, 90L))
  # Once: the same version is not refit.
  fits_after <- length(seen$nobs)
  again <- sieve_holdout_evaluate(s, "mlr")
  expect_true(again$repeated)
  expect_identical(again$id, "E1")
  expect_identical(length(seen$nobs), fits_after)
  expect_length(shiny::isolate(sieve_holdout(s))$evaluations, 1L)
  # The version records the item set, the syntax, the estimator and the scoring specification.
  v <- shiny::isolate(sieve_holdout(s))$versions[[1L]]
  expect_identical(v$name, "v1")
  expect_false(v$after_evaluation)
  expect_identical(v$items$item, fx$item_cols)
  expect_match(v$syntax, "=~", fixed = TRUE)
  expect_identical(v$estimator$choice, "mlr")
  expect_true(all(c("policy", "policies") %in% names(v$scoring)))
  # A development change creates a new version; its evaluation is revised
  # during development and the first becomes historical.
  m <- shiny::isolate(s$meta)
  m$status[m$item == fx$item_cols[9]] <- "parked"
  s$meta <- m
  ev2 <- sieve_holdout_evaluate(s, "mlr")
  expect_identical(ev2$id, "E2")
  expect_identical(ev2$version, "v2")
  expect_identical(ev2$label, "revised during development")
  rec <- shiny::isolate(sieve_holdout(s))
  expect_true(rec$versions[[2L]]$after_evaluation)
  expect_true(rec$evaluations[[1L]]$historical)
  expect_identical(rec$evaluations[[1L]]$superseded_by, "E2")
  expect_false(rec$evaluations[[2L]]$historical)
  tbl <- sieve_holdout_evaluation_table(rec)
  expect_identical(tbl$Status, c("historical (superseded by E2)", "current"))
  expect_identical(tbl$Label, c("specified before evaluation", "revised during development"))
  # Back to the first item set: v1 again, its evaluation shown, nothing refit.
  m$status[m$item == fx$item_cols[9]] <- "active"
  s$meta <- m
  fits_after <- length(seen$nobs)
  expect_identical(sieve_holdout_evaluate(s, "mlr")$version, "v1")
  expect_identical(length(seen$nobs), fits_after)
  # The log carries the evaluation and the script replays it to the same table.
  script <- shiny::isolate(sieve_replay_script(s))
  expect_match(script, "holdout_evaluation <- SIEVER::sieve_cross_validate(holdout_data, meta, validation_rows = ", fixed = TRUE)
  env <- new.env(parent = baseenv())
  expect_no_error(suppressWarnings(capture.output(eval(parse(text = script), env))))
  expect_equal(env$holdout_evaluation$table, ev2$result$table, tolerance = 1e-12)
  expect_identical(env$holdout_evaluation$n_val, 90L)
})

test_that("an item reverse scored after the reservation reaches the reserved rows at evaluation and in the script", {
  s <- hd_state()
  fx <- sieve_hs9_fixture()
  out <- sieve_reserve_holdout(s, .3, seed = 11)
  rows <- out$reserved
  item <- fx$item_cols[1]
  rev <- sieve_apply_reverse(s, item, origin = "test")
  expect_length(rev$refused, 0L)
  assembled <- shiny::isolate(sieve_holdout_data(s))
  expect_null(assembled$error)
  expect_identical(nrow(assembled$data), 301L)
  expect_identical(assembled$validation_rows, 211L + seq_len(90L))
  expect_identical(assembled$rows, rows)
  expect_length(assembled$fixes, 1L)
  # The held values take the same reflection the calibration rows took.
  rec <- shiny::isolate(s$transforms)[[1L]]
  expected <- (rec$lo + rec$hi) - fx$data[rows, item]
  expect_equal(as.numeric(assembled$data[[item]][assembled$validation_rows]), expected)
  ev <- sieve_holdout_evaluate(s, "mlr")
  expect_null(ev$error)
  script <- shiny::isolate(sieve_replay_script(s))
  expect_match(script, "was reverse scored within", fixed = TRUE)
  env <- new.env(parent = baseenv())
  expect_no_error(suppressWarnings(capture.output(eval(parse(text = script), env))))
  expect_equal(as.numeric(env$holdout_data[[item]][211L + seq_len(90L)]), expected)
  expect_equal(env$holdout_evaluation$table, ev$result$table, tolerance = 1e-12)
  # The held rows themselves were not changed by the evaluation: a release
  # applies the reflection once, at the restore.
  sieve_release_holdout(s)
  expect_equal(as.numeric(shiny::isolate(s$items)[[item]][rows]), expected)
})

# ---- The pages ----------------------------------------------------------------------

test_that("the Screening page's control states the sizes, reserves, refuses the ordinary restore, and releases", {
  s <- hd_state()
  shiny::testServer(mod_screen_server, args = list(state = s), {
    session$setInputs(holdout_proportion = .3, holdout_seed = 11, holdout_stratify = "")
    expect_match(hd_text(output$holdout_status), "No validation reservation is in force", fixed = TRUE)
    expect_match(hd_text(output$holdout_sizes), "Reserve would set aside 90 of the 301 respondents in the analysis and leave 211 calibration rows", fixed = TRUE)
    session$setInputs(holdout_stratify = "school")
    expect_match(hd_text(output$holdout_sizes), "By level of school", fixed = TRUE)
    session$setInputs(holdout_stratify = "")
    session$setInputs(holdout_reserve = 1)
    expect_identical(nrow(state$items), 211L)
    expect_match(hd_text(output$holdout_result), "Reserved 90 respondents for validation (reservation R1, seed 11)", fixed = TRUE)
    status <- hd_text(output$holdout_status)
    expect_match(status, "Reservation R1 is in force", fixed = TRUE)
    expect_match(status, "n = 211 calibration rows; 90 reserved for validation", fixed = TRUE)
    expect_match(status, "Reserved before any result existed", fixed = TRUE)
    # The page's respondent note reads the calibration rows.
    expect_match(hd_text(output$pool_note), "211 respondents are in the analysis", fixed = TRUE)
    # The removed respondents table lists them with the reason.
    expect_true(all(removed_shown()$reason == "reserved for validation"))
    # Release asks first: the press alone releases
    # nothing; the confirmation in the dialog releases.
    session$setInputs(holdout_release = 1)
    expect_identical(nrow(state$items), 211L)
    expect_false(is.null(sieve_holdout(state)))
    session$setInputs(holdout_release_confirm = 1)
    expect_identical(nrow(state$items), 301L)
    expect_match(hd_text(output$holdout_result), "Released reservation R1: 90 respondents returned", fixed = TRUE)
    expect_match(hd_text(output$holdout_status), "was released at", fixed = TRUE)
  })
})

test_that("the CV page evaluates the frozen version on the reserved rows and says what the evaluation is", {
  s <- hd_state()
  shiny::testServer(mod_finalize_server, args = list(state = s), {
    session$setInputs(estimator = "mlr", holdout = 50)
    expect_match(hd_text(output$holdout_note), "No validation reservation is in force", fixed = TRUE)
    expect_match(hd_text(output$holdout_msg), "No validation reservation is in force", fixed = TRUE)
    expect_match(hd_text(output$split_sizes), "of 301 complete cases", fixed = TRUE)
  })
  # The reservation is made on the Screening page before development; the
  # module is then started as the application starts it (a test server
  # evaluates every output at once, which the application does only when a
  # panel is viewed).
  s <- hd_state()
  sieve_reserve_holdout(s, .3, seed = 11)
  shiny::testServer(mod_finalize_server, args = list(state = s), {
    session$setInputs(estimator = "mlr", holdout = 50)
    expect_match(hd_text(output$holdout_note), "Reservation R1 is in force: 90 rows reserved", fixed = TRUE)
    expect_match(hd_text(output$split_sizes), "n = 211 calibration rows; 90 reserved", fixed = TRUE)
    expect_match(hd_text(output$holdout_msg), "Not evaluated yet", fixed = TRUE)
    session$setInputs(holdout_go = 1)
    ev <- holdout_result()
    expect_null(ev$error)
    expect_identical(ev$label, "specified before evaluation")
    msg <- hd_text(output$holdout_msg)
    expect_match(msg, "Version v1, specified before evaluation", fixed = TRUE)
    expect_match(msg, "evaluated", fixed = TRUE)
    expect_match(msg, "played no part in fitting or selecting this version", fixed = TRUE)
    expect_match(hd_text(output$holdout_tables), "Reserved", fixed = TRUE)
    # Pressed again: shown, not refit.
    session$setInputs(holdout_go = 2)
    expect_true(holdout_result()$repeated)
    expect_match(hd_text(output$holdout_msg), "evaluated before; its evaluation is shown and nothing was refit", fixed = TRUE)
    # The late split, with a reservation, draws from the calibration rows.
    session$setInputs(seed = 113, split = 1)
    r <- cv_shown()
    expect_null(r$error)
    expect_identical(r$n_cal + r$n_val, 211L)
    expect_match(hd_text(output$cv_msg), "Drawn from the calibration rows only", fixed = TRUE)
    expect_identical(state$page_results$finalize[[session$ns("result_holdout")]]$state, "current")
  })
})

# The frozen version is the model the CFA page fits, as the page promises,
# not the default first-order model with the variances at 1 rebuilt from
# the ledger whatever the CFA page chose. One construct with three
# facets makes the two differ: the default is a one-factor model of the
# nine tests, the CFA page's higher-order model has three facets under it.
test_that("the frozen version is the model the CFA page fits, and that model is what the reserved rows evaluate", {
  seen <- hd_engines()
  s <- hd_state()
  fx <- sieve_hs9_fixture()
  m <- shiny::isolate(s$meta)
  m$facet <- m$construct
  m$construct <- "Ability"
  s$meta <- m
  s$cfa_settings <- list(scope = "construct", structure = "higher", ident = "varone", facets = character(0), all_facets = TRUE)
  out <- sieve_reserve_holdout(s, .3, seed = 11)
  rows <- out$reserved
  ev <- sieve_holdout_evaluate(s, "mlr")
  expect_null(ev$error)
  v <- shiny::isolate(sieve_holdout(s))$versions[[1L]]
  expect_identical(v$model, "higher-order model, factor variances fixed at 1")
  expect_true(v$std_lv)
  expect_identical(ev$model, v$model)
  expect_identical(length(sieve_model_latent(v$syntax)), 4L)
  # The calibration half is the higher-order model on the calibration rows,
  # digit for digit, and not the one-factor model the default would build.
  d <- fx$data[setdiff(1:301, rows), fx$item_cols]
  cal <- lavaan::cfa(v$syntax, data = d, std.lv = TRUE, estimator = "MLR", missing = "fiml")
  one <- lavaan::cfa(paste("Ability =~", paste(fx$item_cols, collapse = " + ")), data = d,
    std.lv = TRUE, estimator = "MLR", missing = "fiml")
  # The higher-order model's robust indices cannot be computed by lavaan
  # here, so the evaluation reads the scaled family; lavaan warns about the
  # robust ones while computing the measures this test reads directly.
  suffix <- if (ev$result$family == "standard") "" else paste0(".", ev$result$family)
  shown <- unname(as.numeric(ev$result$table[1L, c("CFI", "RMSEA")]))
  expect_true(all(is.finite(shown)))
  direct <- suppressWarnings(lavaan::fitMeasures(cal, paste0(c("cfi", "rmsea"), suffix)))
  expect_equal(shown, unname(direct), tolerance = 1e-10)
  expect_gt(abs(shown[[1L]] - unname(suppressWarnings(lavaan::fitMeasures(one, paste0("cfi", suffix))))), .01)
  tbl <- sieve_holdout_evaluation_table(shiny::isolate(sieve_holdout(s)))
  expect_identical(tbl$Model, "higher-order model, factor variances fixed at 1")
  # The session script carries the frozen syntax and replays the evaluation.
  script <- shiny::isolate(sieve_replay_script(s))
  expect_match(script, "holdout_model <- ", fixed = TRUE)
  expect_match(script, "syntax = if (nzchar(holdout_model)) holdout_model, std_lv = TRUE)", fixed = TRUE)
  env <- new.env(parent = baseenv())
  expect_no_error(suppressWarnings(capture.output(eval(parse(text = script), env))))
  expect_equal(unname(as.numeric(env$holdout_evaluation$table[1L, c("CFI", "RMSEA")])), shown, tolerance = 1e-10)
  # The marker identification is frozen as such and is another version.
  s$cfa_settings <- list(scope = "construct", structure = "higher", ident = "marker", facets = character(0), all_facets = TRUE)
  v2 <- sieve_holdout_freeze(s, "mlr")
  expect_false(v2$std_lv)
  expect_identical(v2$model, "higher-order model, marker indicators fixed at 1")
  expect_false(identical(v2$hash, v$hash))
  # A version of the default model is hashed from its items, syntax,
  # estimator and scoring alone, so it matches a saved version of that
  # model whose record does not state its identification.
  s0 <- hd_state()
  sieve_reserve_holdout(s0, .3, seed = 11)
  v0 <- sieve_holdout_freeze(s0, "mlr")
  expect_identical(v0$model, "correlated-factors model, factor variances fixed at 1")
  expect_identical(v0$hash, sieve_hash(list(items = v0$items, syntax = v0$syntax, estimator = v0$estimator, scoring = v0$scoring)))
  # The CFA page's controls and the saved settings map to the model one way.
  expect_identical(sieve_cfa_choices(NULL), list(structure = "first_order", facet_ids = NULL, ident = "varone"))
  expect_identical(sieve_cfa_choices(list(scope = "facet", all_facets = FALSE, facets = c("f1", "f2"), ident = "marker")),
    list(structure = "facet_first_order", facet_ids = c("f1", "f2"), ident = "marker"))
  expect_identical(sieve_cfa_choices(list(scope = "facet", all_facets = TRUE, facets = "f1"))$facet_ids, NULL)
})

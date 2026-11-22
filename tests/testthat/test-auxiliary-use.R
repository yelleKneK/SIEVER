# Declared auxiliary variables enter a FIML fit only when an analyzed item
# value is missing. On the bundled example's complete data a CFA that
# carried the age in years beside the nine test scores would draw lavaan's
# warning that the observed variances differ by a factor of over 1,000, and
# Validity would count 10 observed variables for the model CFA calls 9
# items on 3 factors, with identical estimates and fit either way.
#
# Author: Ken Kelley

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
withr::local_options(sieve.cores = 2L)

aux_text <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
aux_complete_study <- function() siever_test_study()
aux_incomplete_study <- function() siever_test_study(configure = function(s) s$items[1:25, 1] <- NA_real_)
aux_prepare <- function(state) {
  shiny::isolate({
    spec <- sieve_cfa_spec(state$meta, reserved = union(names(state$items), names(state$raw)), estimator = "auto")
    sieve_sampling_apply(spec, state, state$items)
  })
}

test_that("the bundled example's complete-data CFA has no auxiliary variable, no warning and nine observed variables", {
  state <- aux_complete_study()
  roles <- shiny::isolate(state$roles)
  expect_identical(roles$variable[roles$role %in% "auxiliary"], "age_years")
  prepared <- aux_prepare(state)
  expect_null(prepared$spec$engine_args$aux)
  expect_identical(names(prepared$data), names(shiny::isolate(state$items)))
  expect_identical(prepared$notes, paste(
    "Auxiliary variable age_years is not used: every analyzed item is observed in every analyzed row,",
    "so saturated correlates add no information (Graham, 2003)."
  ))
  run <- sieve_run_cfa(prepared$spec, prepared$data)
  expect_identical(run$status$code, "ok")
  expect_length(run$warnings, 0L)
  expect_length(lavaan::lavNames(run$fit, "ov"), 9L)
  expect_match(sieve_method_text(run$fit, "Correlated-factors CFA"), "; 9 observed variables, 3 factors;", fixed = TRUE)
})

test_that("with an item value missing the auxiliary enters, is counted apart from the items, and lavaan's variance warning is said with its numbers", {
  prepared <- aux_prepare(aux_incomplete_study())
  expect_identical(prepared$spec$engine_args$aux, "age_years")
  expect_length(prepared$notes, 0L)
  run <- sieve_run_cfa(prepared$spec, prepared$data)
  expect_identical(run$status$code, "ok")
  expect_true("age_years" %in% lavaan::lavNames(run$fit, "ov"))
  expect_match(sieve_method_text(run$fit, "Correlated-factors CFA"),
    "; 9 observed variables and 1 auxiliary variable as a saturated correlate (age_years), 3 factors;", fixed = TRUE)
  vt <- lavaan::varTable(run$fit)
  ratio <- max(vt$var) / min(vt$var)
  expect_gt(ratio, 1000)
  said <- grep("lavaan's numerical check", run$warnings, value = TRUE, fixed = TRUE)
  expect_length(said, 1L)
  expect_match(said, sprintf(
    "the observed variances are unequal by a factor of %s (largest %s, %s; smallest age_years, %s, an auxiliary variable).",
    formatC(ratio, format = "f", digits = 0, big.mark = ","), vt$name[which.max(vt$var)],
    formatC(max(vt$var), format = "f", digits = 2, big.mark = ","), formatC(min(vt$var), format = "f", digits = 2, big.mark = ",")
  ), fixed = TRUE)
  expect_match(said, "The fit converged. SIEVER checks the solution for admissibility itself", fixed = TRUE)
  expect_false(any(grepl("varTable(fit)", run$warnings, fixed = TRUE)))
})

test_that("lavaan's variance warnings become one sentence, and other warnings and fits are left as they are", {
  hs <- lavaan::HolzingerSwineford1939[, c("x1", "x2", "x3")]
  hs$x2 <- hs$x2 * 1000
  caught <- character(0)
  # (lavaan also says, as a message, that it rescaled the variables
  # internally.)
  utils::capture.output(fit <- suppressMessages(withCallingHandlers(lavaan::cfa("v =~ x1 + x2 + x3", data = hs, std.lv = TRUE),
    warning = function(w) {
      caught <<- c(caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )), type = "output")
  expect_true(any(grepl("observed variances are", gsub("\\s+", " ", caught), fixed = TRUE)))
  out <- sieve_variance_ratio_note(c("another warning", caught), fit)
  expect_identical(out[1L], "another warning")
  expect_length(out, 2L)
  expect_match(out[2L], "largest x2, ", fixed = TRUE)
  expect_false(grepl("auxiliary", out[2L], fixed = TRUE))
  # x2 times 1000 has a variance over 1,000,000 too, so lavaan gave both
  # warnings, and the sentence has a clause for each.
  kinds <- sieve_variance_warning_kinds(caught)
  expect_true(any(kinds$ratio) && any(kinds$size))
  expect_match(out[2L], "), and the observed variances are larger than 1,000,000 for 1 of the 3 variables.", fixed = TRUE)
  expect_identical(lapply(sieve_variance_warning_kinds(out[2L]), any), list(ratio = TRUE, size = TRUE))
  expect_identical(sieve_variance_ratio_note("another warning", fit), "another warning")
  expect_identical(sieve_variance_ratio_note(caught, NULL), caught)
})

test_that("lavaan's warning about variances over 1,000,000 is said as one about their size, not their ratio", {
  # Items recorded on one large scale (response times in milliseconds, say):
  # every variance is over 1,000,000 and the ratio is near 1, so lavaan
  # warns about the size alone. Reading any warning with "observed
  # variances are" as the ratio warning would say the variances are
  # "unequal by a factor of 1".
  hs <- lavaan::HolzingerSwineford1939[, c("x1", "x2", "x3")] * 2000
  caught <- character(0)
  utils::capture.output(fit <- suppressMessages(withCallingHandlers(lavaan::cfa("v =~ x1 + x2 + x3", data = hs, std.lv = TRUE),
    warning = function(w) {
      caught <<- c(caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )), type = "output")
  kinds <- sieve_variance_warning_kinds(caught)
  expect_true(any(kinds$size))
  expect_false(any(kinds$ratio))
  out <- sieve_variance_ratio_note(caught, fit)
  expect_length(out, 1L)
  vt <- lavaan::varTable(fit)
  expect_identical(out, sprintf(paste0(
    "lavaan's numerical check: the observed variances are larger than 1,000,000 for 3 of the 3 variables ",
    "(largest %s, %s). The fit converged. SIEVER checks the solution for admissibility itself, and dividing a ",
    "variable by a constant (response times in milliseconds expressed in seconds, say) changes no standardized estimate."
  ), vt$name[which.max(vt$var)], formatC(max(vt$var), format = "f", digits = 2, big.mark = ",")))
  expect_false(grepl("unequal by a factor", out, fixed = TRUE))
  expect_identical(lapply(sieve_variance_warning_kinds(out), any), list(ratio = FALSE, size = TRUE))
  # The fit status's hint about scales tells the two apart too, in lavaan's
  # words and in SIEVER's sentence (a two-indicator factor orthogonal to
  # the other has no standard errors).
  no_se <- sieve_capture(lavaan::cfa("V =~ x1 + x2\nT =~ x4 + x5 + x6",
    data = lavaan::HolzingerSwineford1939, std.lv = TRUE, orthogonal = TRUE))
  size_hint <- "lavaan also reports observed variances larger than 1,000,000, which can cause this on its own; dividing the variables by a constant may resolve it."
  for (said in list(caught, out)) {
    st <- sieve_fit_status(no_se$value, warnings = c(no_se$warnings, said))
    expect_identical(st$code, "not_identified")
    expect_match(st$message, size_hint, fixed = TRUE)
    expect_false(grepl("factor of 1000", st$message, fixed = TRUE))
  }
  ratio <- "lavaan->lav_data_full():  \n   some observed variances are (at least) a factor 1000 times larger than others"
  expect_match(sieve_fit_status(no_se$value, warnings = c(no_se$warnings, ratio))$message,
    "lavaan also reports observed variances that differ by a factor of 1000 or more, which can cause this on its own; putting the variables on similar scales may resolve it.",
    fixed = TRUE)
  expect_match(sieve_fit_status(no_se$value, warnings = c(no_se$warnings, ratio, caught))$message,
    "that differ by a factor of 1000 or more, some larger than 1,000,000, which can cause this on its own; putting the variables on similar, smaller scales may resolve it.",
    fixed = TRUE)
  expect_false(grepl("lavaan also reports", sieve_fit_status(no_se$value, warnings = no_se$warnings)$message, fixed = TRUE))
})

test_that("the CFA page says the declared auxiliary is not used, or names it with the items", {
  state <- aux_complete_study()
  shiny::testServer(mod_cfa_server, args = list(state = state), {
    session$setInputs(structure = "first", ident = "varone", estimator = "auto", layout = "top_down")
    session$setInputs(fit_go = 1L)
    run <- cfa_run()
    expect_null(run$spec$engine_args$aux)
    expect_length(run$warnings, 0L)
    line <- aux_text(output$method_line$html)
    expect_match(line, "correlated-factors CFA, 9 items on 3 factors (lavaan::cfa()", fixed = TRUE)
    expect_match(line, "Auxiliary variable age_years is not used: every analyzed item is observed in every analyzed row", fixed = TRUE)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "# Auxiliary variable age_years is not used", fixed = TRUE)
    expect_false(grepl("aux = ", log, fixed = TRUE))
  })
  spec <- sieve_spec_engine_args(shiny::isolate(sieve_cfa_spec(state$meta, reserved = names(state$raw))), list(aux = "age_years"))
  est <- list(basis = "Pearson", label = "ML with FIML")
  expect_match(sieve_cfa_method_text(spec, est, rows = 301L, used = 301L),
    "9 items on 3 factors (lavaan::cfa(), std.lv = TRUE), with 1 auxiliary variable as a saturated correlate (age_years);", fixed = TRUE)
})

test_that("Reliability uses the declared auxiliary only when an item value is missing", {
  for (case in c("complete", "incomplete")) {
    state <- if (identical(case, "complete")) aux_complete_study() else aux_incomplete_study()
    shiny::testServer(mod_reliab_server, args = list(state = state), {
      session$setInputs(scope = "construct", ci_method = "mlr", denominator = "model_implied", missing = "fiml")
      session$setInputs(run_go = 1)
      res <- immediate()
      want <- if (identical(case, "complete")) character(0) else "age_years"
      expect_true(all(vapply(res, function(r) identical(as.character(r$omega$spec$aux), want), logical(1))))
      expect_true(all(vapply(res, function(r) is.finite(r$omega$estimate), logical(1))))
      expect_identical(setdiff(names(reliability_data()), names(state$items)), want)
    })
  }
})

test_that("the EFA page names no unused auxiliary variable when every item value is observed", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log))
  state <- aux_complete_study()
  expect_identical(sieve_efa_declared_aux(state, sieve_hs9_fixture()$item_cols), "age_years")
  shiny::testServer(mod_efa_server, args = list(state = state), {
    session$setInputs(n_factors = 3L, fm = "ml", rotation = "oblimin", estimator = "study", efa_go = 1L)
    run <- efa_run()
    expect_identical(run$route$missing, "fiml")
    expect_length(run$aux_ignored, 0L)
    expect_false(grepl("is not used: DMAR::fa_exploratory()", aux_text(output$efa_method), fixed = TRUE))
  })
})

test_that("the SEM, Validity, Bifactor and Reliability pages say the declared auxiliary is not used, on the method line and in the recorded R", {
  # Every page that leaves the auxiliary out says so, as the Ingest help
  # and the user guide say the method line does, not the CFA page alone.
  expect_identical(sieve_auxiliary_unused_note(c("age_years", "income"), "variable"), paste(
    "Auxiliary variables age_years, income are not used: every analyzed variable is observed in every analyzed row,",
    "so saturated correlates add no information (Graham, 2003)."
  ))
  item_note <- "Auxiliary variable age_years is not used: every analyzed item is observed in every analyzed row"
  log_note <- "# Auxiliary variable age_years is not used"
  state <- aux_complete_study()
  shiny::testServer(mod_sem_server, args = list(state = state), {
    session$setInputs(outcomes = "Visual", predictors = c("Textual", "Speed"), layout = "tree2", min_path = 0)
    session$setInputs(fit_go = 1L)
    run <- path_run()
    expect_identical(run$status$code, "ok")
    expect_null(run$spec$engine_args$aux)
    expect_identical(run$aux_notes, sieve_auxiliary_unused_note("age_years", "variable"))
    expect_match(aux_text(output$method$html),
      "Auxiliary variable age_years is not used: every analyzed variable is observed in every analyzed row", fixed = TRUE)
    expect_match(paste(shiny::isolate(state$codelog), collapse = "\n"), log_note, fixed = TRUE)
  })
  state <- aux_complete_study()
  shiny::testServer(mod_validity_server, args = list(state = state), {
    session$setInputs(fit_go = 1L)
    expect_match(aux_text(output$method$html), item_note, fixed = TRUE)
    expect_match(paste(validity_fit_code(), collapse = "\n"), log_note, fixed = TRUE)
  })
  state <- aux_complete_study()
  shiny::testServer(mod_bifactor_server, args = list(state = state), {
    suppressWarnings(session$setInputs(fit = 1))
    expect_match(aux_text(output$method$html), item_note, fixed = TRUE)
    expect_match(paste(shiny::isolate(state$codelog), collapse = "\n"), log_note, fixed = TRUE)
  })
  for (case in c("complete", "incomplete")) {
    state <- if (identical(case, "complete")) aux_complete_study() else aux_incomplete_study()
    shiny::testServer(mod_reliab_server, args = list(state = state), {
      session$setInputs(scope = "construct", ci_method = "mlr", denominator = "model_implied", missing = "fiml")
      session$setInputs(run_go = 1)
      line <- aux_text(output$method_line$html)
      log <- paste(shiny::isolate(state$codelog), collapse = "\n")
      if (identical(case, "complete")) {
        expect_match(line, item_note, fixed = TRUE)
        expect_match(log, log_note, fixed = TRUE)
      } else {
        expect_false(grepl("Auxiliary variable", line, fixed = TRUE))
        expect_false(grepl(log_note, log, fixed = TRUE))
      }
    })
  }
  # With an item value missing the auxiliary enters the structural model,
  # and nothing is said.
  state <- aux_incomplete_study()
  run <- shiny::isolate(sieve_run_structural(state$items, state$raw, state$meta, "Visual", c("Textual", "Speed"),
    sampling_state = state))
  expect_identical(run$spec$engine_args$aux, "age_years")
  expect_length(run$aux_notes, 0L)
  expect_false(grepl("Auxiliary variable", sieve_method_text(run$fit, "Structural model", notes = run$aux_notes), fixed = TRUE))
  expect_match(sieve_method_text(run$fit, "Structural model", notes = "A note."), "[.] A note[.]$")
})

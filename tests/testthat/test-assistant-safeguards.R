assistant_review_fixture <- function() {
  fx <- sieve_hs9_fixture()
  st <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = FALSE, name = NULL, configure = function(st) {
  st$estimator <- "MLR"

  })
  st
}

test_that("external consent binds the exact payload and source plan before browser acknowledgment", {
  st <- assistant_review_fixture()
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  testthat::local_mocked_bindings(sieve_assistant_send = function(request, api_key, consent, ...) {
    stopifnot(identical(api_key, "session-secret-only"), isTRUE(consent))
    calls$n <- calls$n + 1L
    'assign("siever_untrusted_response_executed", TRUE, envir = .GlobalEnv)'
  })
  shiny::testServer(mod_project_server, args = list(state = st), {
    session$setInputs(
      assistant = 1, assist_goal = "Goal A", assist_task = "understand",
      assist_provider = "OpenAI", assist_model = "explicit-test-model", assist_key = "session-secret-only"
    )
    session$setInputs(assist_plan = 1)
    session$setInputs(assist_prepare_external = 1)
    session$setInputs(assist_consent = TRUE)
    # Same project context, different prepared plan: the old consent and
    # payload must not authorize transmission of the earlier goal.
    session$setInputs(assist_goal = "Goal B", assist_plan = 2)
    session$setInputs(assist_send = 1)
    expect_identical(calls$n, 0L)
    session$setInputs(assist_prepare_external = 2)
    # No client checkbox acknowledgment has arrived; input is still TRUE.
    session$setInputs(assist_send = 2)
    expect_identical(calls$n, 0L)
    session$setInputs(assist_consent = FALSE)
    session$setInputs(assist_consent = TRUE)
    session$setInputs(assist_send = 3)
    expect_identical(calls$n, 1L) # positive control: authorized current request runs
    session$setInputs(assist_send = 4)
    expect_identical(calls$n, 1L) # consent is consumed before the transport call
    expect_false(exists("siever_untrusted_response_executed", envir = .GlobalEnv, inherits = FALSE))
    old_revision <- sieve_revision(state)
    decisions_before <- state$decisions
    state$meta$status[1] <- "parked"
    session$flushReact()
    session$setInputs(
      assist_disposition = "adopted",
      assist_revision = "My judgment after reviewing evidence.",
      assist_record = 1
    )
    expect_length(state$assistant_records, 1L) # automatic disclosure remains even without adopting
    expect_identical(state$decisions, decisions_before)
    session$setInputs(assist_disposition = "rejected", assist_record = 2)
    expect_length(state$assistant_records, 2L)
    kept <- state$assistant_records[[2]]
    expect_true(kept$historical)
    expect_identical(kept$revision, old_revision)
    expect_false(identical(kept$recorded_revision, kept$revision))
    snapshot <- sieve_project_snapshot(state)
    expect_false(grepl("session-secret-only", paste(capture.output(dput(snapshot)), collapse = ""), fixed = TRUE))
    expect_false(grepl("session-secret-only", paste(state$codelog, collapse = ""), fixed = TRUE))
  })
})

test_that("editing a local goal cannot silently execute the previously prepared task", {
  st <- assistant_review_fixture()
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  real <- sieve_run_cfa
  testthat::local_mocked_bindings(sieve_run_cfa = function(...) {
    calls$n <- calls$n + 1L
    real(...)
  })
  shiny::testServer(mod_project_server, args = list(state = st), {
    session$setInputs(assist_goal = "Inspect covariance structure", assist_task = "cfa", assist_plan = 1)
    session$setInputs(assist_goal = "Estimate reliability", assist_task = "reliability", assist_execute = 1)
    expect_identical(calls$n, 0L)
    session$setInputs(assist_goal = "Inspect covariance structure", assist_task = "cfa", assist_execute = 2)
    expect_identical(calls$n, 1L)
  })
})

test_that("saved reliability reports retain exact groups and inputs without invoking engines", {
  st <- assistant_review_fixture()
  plan <- sieve_assistant_plan(st, "Inspect reliability and uncertainty", "reliability")
  run <- sieve_assistant_execute(st, plan)
  stored <- shiny::isolate(st$analyses[[1]])
  expect_identical(stored$kind, "reliability")
  expect_length(stored$spec$groups, 3L)
  inputs <- shiny::isolate(sieve_replay_expand(st$replay_states[[stored$inputs$replay_key]], st$replay_data))
  expect_identical(inputs$items, shiny::isolate(st$items))
  expect_identical(stored$inputs$data_hash, sieve_hash(inputs$items))
  expect_identical(stored$estimates[[1]]$omega$estimate, run$results[[1]]$omega$estimate)
  expect_identical(stored$estimates[[1]]$alpha$estimate, run$results[[1]]$alpha$estimate)
  expect_identical(stored$estimates[[1]]$alpha$uncertainty$status, "completed")
  # Every estimate shown by the assistant has its corresponding engine
  # call in the replay log, including analytic alpha and its interval.
  expect_true(any(grepl("DMAR::reliability_alpha", shiny::isolate(st$codelog), fixed = TRUE)))
  m <- shiny::isolate(st$meta)
  m$status[1] <- "parked"
  st$meta <- m
  testthat::local_mocked_bindings(
    sieve_reliability_omega = function(...) stop("unexpected omega refit"),
    sieve_reliability_alpha = function(...) stop("unexpected alpha refit"),
    sieve_run_cfa = function(...) stop("unexpected CFA refit")
  )
  text <- sieve_saved_analyses_report(st, stored$id)
  expect_match(text, "No model is refit", fixed = TRUE)
  expect_match(text, "reliability", fixed = TRUE)
  expect_match(text, "alpha", fixed = TRUE)
  # The assistant uses the study estimator's treatment of incomplete cases
  # (FIML here), so alpha's analytic interval is ML; ADF goes with complete
  # cases.
  expect_match(text, "ML (delta method; analytic alpha)", fixed = TRUE)
  expect_identical(shiny::isolate(st$analyses[[1]]), stored)
  reopened <- sieve_new_state()
  sieve_project_restore(reopened, sieve_project_snapshot(st))
  expect_identical(shiny::isolate(reopened$analyses[[1]]$estimates), stored$estimates)
  reopened_inputs <- shiny::isolate(sieve_replay_expand(
    reopened$replay_states[[reopened$analyses[[1]]$inputs$replay_key]], reopened$replay_data
  ))
  expect_identical(reopened_inputs$meta, inputs$meta)
})

test_that("the Reliability tab registers its selected facet scope and per-group conditions", {
  st <- assistant_review_fixture()
  m <- shiny::isolate(st$meta)
  m$facet <- "Planning"
  st$meta <- m
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "facet", denominator = "model_implied", ci_method = "ml",
      missing = "listwise", conf_level = .95, treatment = "study", show_alpha = TRUE, run_go = 1
    )
    rows <- reliab()
    saved <- state$analyses[[1]]
    expect_identical(saved$kind, "reliability")
    expect_identical(saved$spec$scopes, "facet")
    expect_identical(
      vapply(saved$estimates, function(r) r$omega$estimate, numeric(1)),
      stats::setNames(rows$omega, names(saved$estimates))
    )
    expect_true(all(vapply(saved$spec$groups, function(sp) sp$scope == "facet", logical(1))))
    expect_match(sieve_saved_analyses_report(state, saved$id), "Planning", fixed = TRUE)
  })
})

test_that("local researcher judgments retain optional rationale and exact current evidence", {
  st <- assistant_review_fixture()
  fitted <- sieve_assistant_plan(st, "Inspect structure", "cfa")
  sieve_assistant_execute(st, fitted)
  before_items <- shiny::isolate(st$items)
  before_meta <- shiny::isolate(st$meta)
  testthat::local_mocked_bindings(sieve_assistant_send = function(...) stop("No provider is needed"))
  shiny::testServer(mod_project_server, args = list(state = st), {
    session$setInputs(assist_task = "decision", assist_goal = "Retain content despite modest fit", assist_plan = 1)
    session$setInputs(
      assist_local_choice = "Retain the content-critical item for evaluation in a new sample.",
      assist_local_rationale = "", assist_keep_decision = 1
    )
    expect_length(state$decisions, 1L)
    rec <- state$decisions[[1]]
    expect_identical(rec$question, "Retain content despite modest fit")
    expect_identical(rec$rationale, sieve_no_rationale())
    expect_identical(rec$evidence, "A1")
    expect_identical(state$items, before_items)
    expect_identical(state$meta, before_meta)
    session$setInputs(assist_plan = 2)
    state$meta$status[1] <- "parked"
    session$flushReact()
    session$setInputs(assist_keep_decision = 2)
    expect_length(state$decisions, 1L) # earlier A1 cannot be relabeled current
    session$setInputs(
      assist_plan = 3,
      assist_local_rationale = "Preserve the substantive facet while uncertainty is evaluated.",
      assist_local_evidence = "Expert content review is planned."
    )
    session$setInputs(assist_keep_decision = 3)
    expect_length(state$decisions, 2L)
    expect_identical(state$decisions[[2]]$rationale, "Preserve the substantive facet while uncertainty is evaluated.")
    expect_false("A1" %in% state$decisions[[2]]$evidence)
    expect_identical(state$decisions[[2]]$evidence, "Expert content review is planned.")
    expect_identical(state$decisions[[2]]$supersedes, rec$id)
  })
})

test_that("frozen claims retain the stated score target and exact applicable model specifications", {
  st <- assistant_review_fixture()
  sieve_assistant_execute(st, sieve_assistant_plan(st, "Evaluate structure", "cfa"))
  spec <- shiny::isolate(st$analyses[[1]]$spec)
  rule <- "Bartlett factor score under the saved continuous CFA; FIML uses observed responses; an all-missing case receives NA."
  claim <- sieve_freeze_claim(st, "The specified factor score predicts the external criterion.", score_rule = rule)
  expect_identical(claim$score_rule, rule)
  expect_identical(claim$models[[1]]$id, "A1")
  expect_identical(claim$models[[1]]$spec, spec)
  expect_identical(claim$models[[1]]$status, shiny::isolate(st$analyses[[1]]$status))
  expect_error(sieve_freeze_claim(st, "A claim", score_rule = " "), "operational score")
  m <- shiny::isolate(st$meta)
  m$status[1] <- "parked"
  st$meta <- m
  later <- sieve_freeze_claim(st,
    "A new claim after the item change.",
    score_rule = "Mean of the remaining items; no observed item yields NA."
  )
  expect_length(later$models, 0L)
  expect_match(later$model_record_status, "No saved analysis specification", fixed = TRUE)
  expect_identical(shiny::isolate(st$frozen_claims[[1]]), claim)
  reopened <- sieve_new_state()
  sieve_project_restore(reopened, sieve_project_snapshot(st))
  expect_identical(shiny::isolate(reopened$frozen_claims[[1]]), claim)
  expect_identical(shiny::isolate(reopened$frozen_claims[[1]]$models[[1]]$spec), spec)
})

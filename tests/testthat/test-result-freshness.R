# Freshness follows scientific inputs rather than page visits. Failed requests
# remain retryable, and stored row-level estimates keep their original people.
freshness_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = FALSE, name = NULL, configure = function(s) {
  s$roles <- sieve_declared_roles(fx$data, fx$item_cols)

  })
  s
}

freshness_cfa_inputs <- function(session) session$setInputs(
  scope = "construct", structure = "first", ident = "varone", estimator = "study",
  layout = "tree2", what = "std", min_path = 0, resid = TRUE, covs = TRUE
)

test_that("returned engine errors and nonconvergence cannot certify a current result", {
  state <- freshness_state()
  seen <- new.env(parent = emptyenv())
  seen$runs <- 0L
  shiny::testServer(function(input, output, session) {
    wire <- sieve_demand_wire(input, session, state, "run", "Retryable analysis",
      key = shiny::reactive(sieve_demand_key(state)),
      plan = shiny::reactive(list(seconds = 3, estimator = "MLR", p = 9)),
      compute = function(...) {
        seen$runs <- seen$runs + 1L
        if (seen$runs == 1L) return(list(error = "No finite solution"))
        if (seen$runs == 2L) return(list(status = list(code = "not_converged", message = "Optimizer did not converge")))
        list(estimate = .7)
      })
  }, {
    session$setInputs(run = 1)
    expect_identical(wire$status()$state, "failed")
    expect_identical(wire$status()$error, "No finite solution")
    session$setInputs(run = 2)
    expect_identical(wire$status()$state, "failed")
    expect_identical(wire$status()$error, "Optimizer did not converge")
    session$setInputs(run = 3)
    expect_identical(wire$status()$state, "current")
    session$setInputs(nav = "ingest")
    session$setInputs(nav = "cfa")
    session$setInputs(run = 4)
    expect_identical(seen$runs, 3L)
    expect_identical(wire$status()$state, "current")
  })
})

test_that("a primary CFA reuses navigation but requires a run after a recode", {
  state <- freshness_state()
  seen <- new.env(parent = emptyenv())
  seen$runs <- 0L
  shiny::testServer(function(input, output, session) {
    scoped <- session$makeScope("cfa")
    wire <- sieve_demand_wire(scoped$input, scoped, state, "fit_go", "CFA",
      key = shiny::reactive(sieve_demand_key(state, list(model = "three constructs", ident = scoped$input$ident %||% "varone"))),
      plan = shiny::reactive(list(seconds = .1, estimator = "MLR", p = 9)),
      compute = function(...) {
        seen$runs <- seen$runs + 1L
        run <- sieve_run_cfa(sieve_cfa_spec(state$meta, std_lv = !identical(scoped$input$ident, "marker")), state$items)
        sieve_log(state, "Freshness regression CFA", sieve_cfa_code(run$spec))
        run
      }, kind = "cfa")
  }, {
    session$setInputs(nav = "cfa")
    expect_identical(seen$runs, 1L)
    expect_identical(wire$status()$state, "current")
    expect_identical(shiny::isolate(state$page_results)$cfa[["cfa-fit_go"]]$state, "current")
    original_fit <- wire$result()$fit
    original_log <- shiny::isolate(state$codelog)
    session$setInputs(nav = "ingest")
    session$setInputs(nav = "reliab")
    session$setInputs(nav = "cfa")
    expect_identical(seen$runs, 1L)
    expect_identical(wire$result()$fit, original_fit)
    expect_identical(shiny::isolate(state$codelog), original_log)
    d <- shiny::isolate(state$items)
    d[[1L]] <- d[[1L]] + .25 * d[[2L]]
    state$items <- d
    session$setInputs(nav = "ingest")
    session$setInputs(nav = "cfa")
    expect_identical(wire$status()$state, "stale")
    expect_identical(shiny::isolate(state$page_results)$cfa[["cfa-fit_go"]]$state, "stale")
    expect_identical(seen$runs, 1L)
    expect_identical(shiny::isolate(state$codelog), original_log)
    session$setInputs(`cfa-fit_go` = 1)
    expect_identical(wire$status()$state, "current")
    expect_identical(seen$runs, 2L)
    expect_false(isTRUE(all.equal(lavaan::coef(wire$result()$fit), lavaan::coef(original_fit))))
    m <- shiny::isolate(state$meta)
    m$label[1L] <- "A clearer item name"
    state$meta <- m
    session$setInputs(nav = "ingest")
    session$setInputs(nav = "cfa")
    expect_identical(wire$status()$state, "current")
    expect_identical(seen$runs, 2L)
    session$setInputs(`cfa-ident` = "marker")
    expect_identical(wire$status()$state, "not_run")
    expect_identical(seen$runs, 2L)
    session$setInputs(nav = "ingest")
    session$setInputs(nav = "cfa")
    expect_identical(seen$runs, 2L)
    session$setInputs(`cfa-fit_go` = 2L)
    expect_identical(wire$status()$state, "current")
    expect_identical(seen$runs, 3L)
  })
})

test_that("a computation finishing after an input change remains stale", {
  state <- freshness_state()
  shiny::testServer(function(input, output, session) {
    wire <- sieve_demand_wire(input, session, state, "run", "Delayed result",
      key = shiny::reactive(sieve_demand_key(state)),
      plan = shiny::reactive(list(seconds = 3, estimator = "MLR", p = 9)),
      compute = function(...) {
        sieve_remove_cases(state, 1L, "changed during computation")
        list(estimate = .7)
      })
  }, {
    session$setInputs(run = 1)
    expect_identical(wire$status()$state, "stale")
    held <- shiny::isolate(wire$store())[[1L]]
    expect_true(all(held$ledger$status == "active"))
    expect_false(identical(held$key$data, sieve_demand_key(state)$data))
  })
})

test_that("factor-score identities stay with the saved fit after respondent replacement", {
  state <- freshness_state()
  shiny::testServer(mod_reliab_server, args = list(state = state), {
    session$setInputs(denominator = "model_implied", ci_method = "mlr", missing = "listwise",
      conf_level = .95, show_alpha = FALSE, score_method = "Bartlett")
    session$setInputs(scores_go = 1)
    expect_identical(demand_scores$status()$state, "current")
    saved <- demand_scores$result()
    expected_rows <- shiny::isolate(sieve_case_index(state))
    expect_identical(saved$case_index, expected_rows)
    n <- nrow(shiny::isolate(state$items))
    state$case_index <- rev(seq_len(n)) + 1000L
    session$flushReact()
    expect_identical(demand_scores$status()$state, "stale")
    expect_identical(demand_scores$result()$case_index, expected_rows)
    expect_error(output$dl_scores, "Recompute factor scores", fixed = TRUE)
    expect_match(demand_scores$status()$sentence, "respondent row identities", fixed = TRUE)
    session$setInputs(nav = "ingest")
    session$setInputs(nav = "reliab")
    expect_identical(demand_scores$result()$fit, saved$fit)
    session$setInputs(scores_go = 2)
    expect_identical(demand_scores$status()$state, "current")
    expect_identical(demand_scores$result()$case_index, shiny::isolate(state$case_index))
  })
})

test_that("Bollen-Stine results are bound to the model and bootstrap request", {
  state <- freshness_state()
  seen <- new.env(parent = emptyenv())
  seen$runs <- 0L
  # The preparation is mocked to return a finished result, so no background
  # bootstrap is started (the module runs DMAR's bootstrap in a worker).
  local_mocked_bindings(sieve_bollen_stine_plan = function(spec, items, B, seed, ...) {
    seen$runs <- seen$runs + 1L
    list(result = list(error = NULL, rng = sieve_draw_seed(seed), chisq = 20, df = 24, p_ml = .7, p_bs = .6,
      n = nrow(items), status = list(code = "ok"), warnings = character()))
  })
  shiny::testServer(mod_cfa_server, args = list(state = state), {
    freshness_cfa_inputs(session)
    session$setInputs(bs_B = 100L, bs_seed = 113L, bs_run = 1L)
    expect_null(bs_result()$error)
    session$setInputs(bs_seed = 114L)
    expect_true(bs_result()$stale)
    expect_match(bs_result()$error, "bootstrap settings changed", fixed = TRUE)
    expect_identical(seen$runs, 1L)
    session$setInputs(bs_run = 2L)
    expect_null(bs_result()$error)
    session$setInputs(ident = "marker")
    expect_true(bs_result()$stale)
    expect_identical(seen$runs, 2L)
  })
})

test_that("an older reliability bootstrap keeps frozen replay inputs and cannot cross datasets", {
  # This low-count fixture checks frozen request ownership and replay,
  # rather than interval adequacy. Production counts are checked separately.
  siever_local_dmar_mechanics()
  state <- freshness_state()
  shiny::isolate({
    m <- state$meta
    m$status[m$construct != "Visual"] <- "parked"
    state$meta <- m
  })
  shiny::testServer(mod_reliab_server, args = list(state = state), {
    session$setInputs(denominator = "model_implied", ci_method = "percentile", missing = "listwise",
      conf_level = .90, B = 20L, boot_seed = 31L, show_alpha = FALSE)
    session$setInputs(run_go = 1L)
    session$setInputs(run_boot = 1L)
    expect_true(boot_running())
    frozen <- boot_job$inputs
    sp <- boot_job$todo[[1L]]
    completed <- sieve_reliability_omega(boot_job$items, sp, rng = boot_job$rng)
    boot_job$done <- stats::setNames(list(completed), sieve_reliability_id(sp))
    boot_job$plan <- NULL
    boot_job$draws <- list()
    old_key <- boot_key(sp, boot_job$items, boot_job$identity)
    state$dataset_id <- "replacement_dataset_with_identical_values"
    sieve_confirm_types(state)
    finish_boot()
    expect_false(boot_running())
    expect_false(identical(old_key, boot_key(sp, state$items)))
    expect_null(shiny::isolate(boot_store())[[boot_key(sp, state$items)]])
    expect_true(!is.null(shiny::isolate(boot_store())[[old_key]]))
    step <- tail(names(shiny::isolate(state$replay_steps)), 1L)
    input_key <- shiny::isolate(state$replay_steps[[step]])
    saved <- sieve_replay_expand(shiny::isolate(state$replay_states[[input_key]]),
      shiny::isolate(state$replay_data))
    expect_identical(saved$revision, frozen$revision)
    expect_identical(saved$items, frozen$items)
    expect_false(identical(saved$revision$dataset_id, state$dataset_id))
    # A result of other data is withheld from the page, status not run and
    # no value, with a line saying where it is kept, rather than shown as
    # stale; it cannot cross datasets.
    expect_identical(demand$status()$state, "not_run")
    expect_true(demand$status()$withheld)
    expect_null(demand$result())
    session$flushReact()
    page <- sub("-.*$", "", session$ns(""))
    expect_identical(shiny::isolate(state$page_results)[[page]][[session$ns("run_boot")]]$state, "stale")
  })
})

test_that("new IRT diagnostics refuse a stale respondent parent without refitting", {
  state <- freshness_state()
  shiny::isolate({
    state$items <- as.data.frame(lapply(state$items, function(v) as.integer(cut(v, breaks = 5L))))
    m <- state$meta
    m$type <- "ordered"
    state$meta <- m
  })
  shiny::isolate(sieve_confirm_types(state))
  shiny::testServer(mod_irt_server, args = list(state = state), {
    session$setInputs(scope = "Textual", estimator = "WLSMV", fit_go = 1L)
    expect_identical(demand$status()$state, "current")
    previous <- shiny::isolate(state$analyses)
    fit_before <- grm_fit()$grm
    state$case_index <- shiny::isolate(sieve_case_index(state)) + 1000L
    session$setInputs(parameter_keep = 1L)
    expect_identical(demand$status()$state, "stale")
    expect_null(info_res())
    expect_identical(shiny::isolate(state$analyses), previous)
    expect_identical(grm_fit()$grm, fit_before)
  })
})

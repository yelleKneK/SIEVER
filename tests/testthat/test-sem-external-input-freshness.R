# Raw observed variables belong to a fitted model's input identity, even
# when the item frame and the selected variable names remain unchanged.

sem_external_state <- function() {
  fx <- sieve_hs9_fixture()
  i <- seq_len(nrow(fx$data))
  fx$data$criterion <- as.numeric(scale(fx$data[[fx$item_cols[4L]]])) / 3 + 2 * sin(i)
  fx$data$antecedent <- as.numeric(scale(fx$data$age_years)) + cos(i / 3) / 3
  fx$data$unused_criterion <- sin(i / 7)
  siever_test_study(fx, theory = FALSE, name = NULL, configure = function(s) {
    roles <- s$roles
    roles$role[roles$variable %in% c("criterion", "unused_criterion")] <- "criterion"
    roles$role[roles$variable == "antecedent"] <- "antecedent"
    s$roles <- roles
    s$estimator_choice <- "mlr"
  })
}

test_that("SEM tracks selected raw outcomes and antecedents outside its choice slot", {
  state <- sem_external_state()
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  engine <- sieve_run_model
  local_mocked_bindings(sieve_run_model = function(...) {
    calls$n <- calls$n + 1L
    engine(...)
  })
  shiny::testServer(mod_sem_server, args = list(state = state), {
    session$setInputs(outcomes = "criterion", predictors = c("Visual", "antecedent:antecedent"),
      estimator = "mlr", layout = "tree2", min_path = 0)
    original_key <- sem_key()
    original_items <- shiny::isolate(sieve_data_key(state$items))
    original_raw <- shiny::isolate(state$raw)
    expect_identical(demand$status()$state, "not_run")
    expect_identical(calls$n, 0L)
    session$setInputs(fit_go = 1L)
    expect_identical(path_run()$status$code, "ok")
    expect_identical(demand$status()$state, "current")
    expect_identical(calls$n, 1L)
    first_fit <- path_run()$fit

    # A raw column that is offered but unselected, item wording, and
    # navigation do not change this model's inputs or request another fit.
    changed <- original_raw
    changed$unused_criterion <- -changed$unused_criterion
    state$raw <- changed
    m <- shiny::isolate(state$meta)
    m$label[1L] <- "A clearer display label"
    state$meta <- m
    session$setInputs(nav = "ingest")
    session$setInputs(nav = "sem")
    expect_identical(sem_key(), original_key)
    expect_identical(demand$status()$state, "current")
    session$setInputs(fit_go = 2L)
    expect_identical(calls$n, 1L)
    expect_identical(path_run()$fit, first_fit)

    changed$criterion <- -changed$criterion
    state$raw <- changed
    session$flushReact()
    new_key <- sem_key()
    expect_identical(sieve_data_key(state$items), original_items)
    expect_identical(new_key$choices, original_key$choices)
    expect_false(identical(new_key$external_data, original_key$external_data))
    expect_identical(demand$status()$state, "stale")
    expect_identical(demand$result()$path$fit, first_fit)
    expect_identical(calls$n, 1L)
    session$setInputs(fit_go = 3L)
    expect_identical(path_run()$status$code, "ok")
    expect_identical(demand$status()$state, "current")
    expect_identical(calls$n, 2L)

    fitted_key <- sem_key()
    changed$antecedent <- -changed$antecedent
    state$raw <- changed
    session$flushReact()
    expect_identical(sem_key()$choices, fitted_key$choices)
    expect_false(identical(sem_key()$external_data, fitted_key$external_data))
    expect_identical(demand$status()$state, "stale")
    expect_identical(calls$n, 2L)
    changed$antecedent <- -changed$antecedent
    state$raw <- changed
    session$flushReact()
    expect_identical(demand$status()$state, "current")
    expect_identical(calls$n, 2L)
  })
})

test_that("Validity SEM tracks selected criteria and reuses unrelated raw edits", {
  state <- sem_external_state()
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  engine <- sieve_run_model
  local_mocked_bindings(sieve_run_model = function(...) {
    calls$n <- calls$n + 1L
    engine(...)
  })
  shiny::testServer(mod_validity_server, args = list(state = state), {
    session$setInputs(criteria = "criterion", estimator = "mlr")
    original_key <- sem_key()
    original_items <- shiny::isolate(sieve_data_key(state$items))
    original_raw <- shiny::isolate(state$raw)
    expect_identical(demand_sem$status()$state, "not_run")
    expect_identical(calls$n, 0L)
    session$setInputs(sem_go = 1L)
    expect_identical(sem_run()$status$code, "ok")
    expect_identical(demand_sem$status()$state, "current")
    expect_identical(calls$n, 1L)
    first_fit <- sem_run()$fit

    changed <- original_raw
    changed$unused_criterion <- -changed$unused_criterion
    changed$antecedent <- -changed$antecedent
    state$raw <- changed
    m <- shiny::isolate(state$meta)
    m$label[1L] <- "Clearer display wording"
    state$meta <- m
    session$setInputs(nav = "sem")
    session$setInputs(nav = "validity")
    expect_identical(sem_key(), original_key)
    expect_identical(demand_sem$status()$state, "current")
    session$setInputs(sem_go = 2L)
    expect_identical(calls$n, 1L)
    expect_identical(sem_run()$fit, first_fit)

    changed$criterion <- -changed$criterion
    state$raw <- changed
    session$flushReact()
    expect_identical(sieve_data_key(state$items), original_items)
    expect_identical(sem_key()$choices, original_key$choices)
    expect_false(identical(sem_key()$external_data, original_key$external_data))
    expect_identical(demand_sem$status()$state, "stale")
    expect_identical(demand_sem$result()$fit, first_fit)
    expect_identical(calls$n, 1L)
    state$raw <- original_raw
    session$flushReact()
    expect_identical(demand_sem$status()$state, "current")
    expect_identical(calls$n, 1L)
  })
})

test_that("categorical MI omega runs through DMAR, keeps its interval and records the exact request", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  skip_if_not_installed("mice")
  skip_if_not("imputations" %in% names(formals(DMAR::reliability_omega_categorical)))
  test_frame <- environment()
  # The page runs DMAR's call in a background R process; the
  # starts are counted here.
  calls <- new.env(parent = emptyenv()); calls$n <- 0L
  starter <- sieve_ordinal_omega_mi_start
  local_mocked_bindings(sieve_ordinal_omega_mi_start = function(...) {
    calls$n <- calls$n + 1L
    starter(...)
  }, .env = test_frame)
  withr::local_options(sieve.auto_run = "never")
  items <- sieve_hs9_fixture()$theory_map$Textual
  state <- siever_test_study(configure = function(s) {
    d <- s$items
    for (nm in items) d[[nm]] <- as.integer(cut(d[[nm]],
      breaks = stats::quantile(d[[nm]], 0:4 / 4), include.lowest = TRUE))
    d[[items[1L]]][seq(3, 90, 3)] <- NA_real_
    s$items <- d
    s$raw[items] <- d[items]
    s$meta$type[s$meta$item %in% items] <- "ordered"
    roles <- sieve_declared_roles(s$raw, names(s$items))
    roles$role[roles$variable == "age_years"] <- "auxiliary"
    s$roles <- roles
  })
  # A fresh R process has no random number state until something draws.
  if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE)) invisible(stats::runif(1))
  old_seed <- .Random.seed
  shiny::testServer(mod_reliab_server, args = list(state = state), {
    session$setInputs(ordinal_mi_construct = "Textual", ordinal_mi_B = 2L,
      ordinal_mi_M = 2L, ordinal_mi_seed = 1L, ordinal_mi_level = .90,
      ordinal_mi_denominator = "model_implied", ordinal_mi_go = 1L)
    # While it runs the page says so and offers Stop; nothing is shown as a result.
    expect_null(ordinal_mi())
    expect_match(output$ordinal_mi_progress, "background R process", fixed = TRUE)
    expect_identical(output$ordinal_mi_running, "true")
    expect_identical(output$ordinal_mi_cancelable, "true")
    # The limit allows for a busy machine; a run with no result at it fails
    # here, saying so, rather than in the expectations on the result.
    siever_wait_until(function() !is.null(ordinal_mi()), what = "The categorical MI process",
      step = function() {
        Sys.sleep(0.2)
        session$elapse(500)
      })
    result <- ordinal_mi()
    expect_s3_class(result, "dmar_reliability")
    expect_true(is.finite(sieve_reliability_value(result, "estimate")))
    expect_true(all(is.finite(c(sieve_reliability_value(result, "ci_lower"),
      sieve_reliability_value(result, "ci_upper")))))
    expect_identical(attr(result, "missing"), "mi")
    expect_identical(attr(result, "aux"), "age_years")
    expect_true(attr(result, "imputation")$imputations_point >= 20L)
    expect_match(paste(state$codelog, collapse = "\n"), "DMAR::reliability_omega_categorical", fixed = TRUE)
    expect_match(paste(state$codelog, collapse = "\n"), 'missing = "mi"', fixed = TRUE)
    expect_match(paste(state$codelog, collapse = "\n"), "imputations = 2, seed = 1", fixed = TRUE)
    expect_match(paste(as.character(output$ordinal_mi_note$html), collapse = " "), "Point estimate:", fixed = TRUE)
    log <- state$codelog
    session$setInputs(ordinal_mi_go = 2L)
    expect_identical(calls$n, 1L)
    expect_identical(state$codelog, log)
    session$setInputs(ordinal_mi_level = .95)
    expect_null(ordinal_mi())
    expect_match(paste(as.character(output$ordinal_mi_note$html), collapse = " "), "MI settings changed", fixed = TRUE)
    expect_identical(state$codelog, log)
  })
  expect_identical(.Random.seed, old_seed)
})


test_that("categorical MI without auxiliary variables passes NULL to the public engine", {
  # Exploratory mechanics/parity fixture; production count boundaries are tested separately.
  siever_local_dmar_mechanics(environment())
  skip_if_not_installed("mice")
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  d[] <- lapply(d, function(v) as.integer(cut(v, stats::quantile(v, 0:4 / 4), include.lowest = TRUE)))
  d$x4[seq(3, 90, 3)] <- NA_real_
  read <- sieve_capture(sieve_ordinal_omega_mi(d, names(d), B = 2L,
    imputations = 2L, seed = 1L, conf_level = .9, denominator = "model_implied"))
  expect_true(read$ok, info = read$error)
  expect_s3_class(read$value, "dmar_reliability")
  expect_null(attr(read$value, "aux", exact = TRUE))
  expect_true(is.finite(sieve_reliability_value(read$value, "estimate")))
  expect_true(is.finite(sieve_reliability_value(read$value, "ci_lower")))
})


test_that("Reliability exposes the categorical MAR sensitivity request", {
  ui <- as.character(mod_reliab_ui("reliab"))
  expect_match(ui, "Categorical Omega Under MAR", fixed = TRUE)
  for (id in c("ordinal_mi_go", "ordinal_mi_table", "ordinal_mi_construct")) {
    expect_equal(sum(gregexpr(sprintf('id="reliab-%s"', id), ui, fixed = TRUE)[[1L]] > 0L), 1L)
  }
  expect_match(ui, "bootstrap-then-impute percentile interval", fixed = TRUE)
})


test_that("categorical MI cites its actual software route", {
  keys <- sieve_spec_methods(list(kind = "categorical_omega_mi"), fitted = TRUE)
  refs <- paste(sieve_method_bibliography(keys), collapse = "\n")
  expect_match(refs, "DMAR", fixed = TRUE)
  expect_match(refs, "mice", fixed = TRUE)
  expect_false(grepl("lavaan.mi", refs, fixed = TRUE))
})


test_that("categorical MI omega stops on Stop: the process ends and nothing is recorded", {
  skip_if_not_installed("mice")
  skip_if_not("progress" %in% names(formals(DMAR::reliability_omega_categorical)))
  test_frame <- environment()
  withr::local_options(sieve.auto_run = "never")
  items <- sieve_hs9_fixture()$theory_map$Textual
  state <- siever_test_study(configure = function(s) {
    d <- s$items
    for (nm in items) d[[nm]] <- as.integer(cut(d[[nm]],
      breaks = stats::quantile(d[[nm]], 0:4 / 4), include.lowest = TRUE))
    d[[items[1L]]][seq(3, 90, 3)] <- NA_real_
    s$items <- d
    s$raw[items] <- d[items]
    s$meta$type[s$meta$item %in% items] <- "ordered"
  })
  handles <- new.env(parent = emptyenv())
  starter <- sieve_ordinal_omega_mi_start
  local_mocked_bindings(sieve_ordinal_omega_mi_start = function(...) {
    handles$job <- starter(...)
    handles$job
  }, .env = test_frame)
  shiny::testServer(mod_reliab_server, args = list(state = state), {
    log <- state$codelog
    session$setInputs(ordinal_mi_construct = "Textual", ordinal_mi_B = 500L,
      ordinal_mi_M = 5L, ordinal_mi_seed = 2L, ordinal_mi_go = 1L)
    expect_true(handles$job$process$is_alive())
    session$setInputs(ordinal_mi_stop = 1L)
    until <- proc.time()[["elapsed"]] + 60
    while (handles$job$process$is_alive() && proc.time()[["elapsed"]] < until) {
      Sys.sleep(0.2)
      session$elapse(500)
    }
    session$elapse(500)
    expect_false(handles$job$process$is_alive())
    expect_null(ordinal_mi())
    expect_match(paste(as.character(output$ordinal_mi_note$html), collapse = " "), "Stopped", fixed = TRUE)
    expect_identical(state$codelog, log)
  })
})


test_that("the categorical MI panel's Cancel button is static UI that the polls never redraw", {
  # A progress line redrawn by renderUI() at every poll (twice a second),
  # Cancel button included, would lose keyboard focus on the button within
  # half a second, and a press straddling a redraw would reach no button.
  # The button is in sieve_ordinal_omega_mi_ui(), shown by the running and
  # cancelable flags, and only the sentence is redrawn.
  ui <- as.character(sieve_ordinal_omega_mi_ui(shiny::NS("reliab")))
  expect_equal(lengths(regmatches(ui, gregexpr('id="reliab-ordinal_mi_stop"', ui, fixed = TRUE))), 1L)
  expect_match(ui, "data-display-if=\"output[&#39;reliab-ordinal_mi_running&#39;] === &#39;true&#39;\"", fixed = TRUE)
  expect_match(ui, "data-display-if=\"output[&#39;reliab-ordinal_mi_cancelable&#39;] === &#39;true&#39;\"", fixed = TRUE)
  expect_match(ui, 'id="reliab-ordinal_mi_progress" class="shiny-text-output', fixed = TRUE)
  items <- sieve_hs9_fixture()$theory_map$Textual
  state <- siever_test_study(configure = function(s) {
    d <- s$items
    for (nm in items) d[[nm]] <- as.integer(cut(d[[nm]],
      breaks = stats::quantile(d[[nm]], 0:4 / 4), include.lowest = TRUE))
    d[[items[1L]]][seq(3, 90, 3)] <- NA_real_
    s$items <- d
    s$raw[items] <- d[items]
    s$meta$type[s$meta$item %in% items] <- "ordered"
  })
  # A background run that stays alive at replication 3 of 400 until the
  # page kills it, so the panel's own logic is tested without DMAR or mice.
  handle <- new.env(parent = emptyenv())
  handle$alive <- TRUE
  handle$stopped <- 0L
  handle$progress <- list(done = 3L, total = 400L)
  handle$poll <- function() handle$alive
  handle$stop <- function() handle$stopped <- handle$stopped + 1L
  handle$kill <- function() {
    handle$alive <- FALSE
    invisible(TRUE)
  }
  handle$results <- list(NULL)
  handle$failure <- function() NULL
  test_frame <- environment()
  local_mocked_bindings(
    sieve_ordinal_omega_mi_call = function(...) list(fun = "reliability_omega_categorical", args = list()),
    sieve_ordinal_omega_mi_start = function(call) handle, .env = test_frame)
  server <- function(input, output, session) {
    ordinal_mi <- sieve_ordinal_omega_mi_server(input, output, session, state)
  }
  shiny::testServer(server, {
    expect_identical(output$ordinal_mi_running, "false")
    expect_identical(output$ordinal_mi_progress, "")
    session$setInputs(ordinal_mi_construct = "Textual", ordinal_mi_B = 400L, ordinal_mi_M = 5L,
      ordinal_mi_seed = 2L, ordinal_mi_level = .95, ordinal_mi_denominator = "observed", ordinal_mi_go = 1L)
    for (i in 1:3) {
      session$elapse(500)
      expect_identical(output$ordinal_mi_running, "true")
      expect_identical(output$ordinal_mi_cancelable, "true")
      expect_type(output$ordinal_mi_progress, "character")
      expect_match(output$ordinal_mi_progress, "Running in a background R process: 3 of 400 bootstrap replications", fixed = TRUE)
    }
    # Cancel asks DMAR to stop; the button is withdrawn while the line
    # says the run is being canceled.
    session$setInputs(ordinal_mi_stop = 1L)
    expect_identical(handle$stopped, 1L)
    expect_identical(output$ordinal_mi_running, "true")
    expect_identical(output$ordinal_mi_cancelable, "false")
    expect_identical(output$ordinal_mi_progress, "Canceling: DMAR stops at its next replication.")
    handle$alive <- FALSE
    session$elapse(500)
    expect_identical(output$ordinal_mi_running, "false")
    expect_identical(output$ordinal_mi_progress, "")
    expect_match(paste(as.character(output$ordinal_mi_note$html), collapse = " "),
      "Stopped: the run was canceled before DMAR returned", fixed = TRUE)
  })
})

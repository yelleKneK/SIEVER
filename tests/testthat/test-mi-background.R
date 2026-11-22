# The ordered-item MI sensitivity analysis runs in a background R process.
# In the Shiny process a run of M = 70 at nine items would hold the process
# for 31 seconds with a progress bar that never moves and no Cancel,
# blocking every session of a shared server.

mi_background_data <- function() {
  raw <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  d <- as.data.frame(lapply(raw, function(v) as.integer(cut(v,
    breaks = unique(stats::quantile(v, probs = seq(0, 1, .2))), include.lowest = TRUE))))
  # MAR on x4, driven by x5 (about 42% missing): the pilot sets M above
  # twenty, so the run has a final stage as well as the pilot.
  withr::with_seed(1L, {
    d$x4[stats::runif(nrow(d)) < stats::plogis(-0.6 + 0.8 * as.numeric(scale(raw$x5)))] <- NA
  })
  d
}

test_that("the ordered MI analysis runs in a background R process and returns the synchronous result exactly", {
  skip_on_cran()
  skip_if_not(sieve_installed("mice"), "mice is not installed")
  skip_if_not(sieve_installed("lavaan.mi"), "lavaan.mi is not installed")
  d <- mi_background_data()
  args <- list(model = "Textual =~ x4 + x5 + x6", data = d, ordered = names(d), aux = character(),
    fun = "cfa", conf_level = .95, seed = 1L, std_lv = TRUE, engine_args = list(),
    pooled_variance = "rubin_parameterwise")
  sync <- sieve_mi_model(args$model, d, ordered = names(d), aux = character(), seed = 1L)
  expect_gt(sync$imputations, 20L)

  started <- proc.time()[["elapsed"]]
  job <- sieve_mi_start(args, rng_kind = RNGkind())
  withr::defer(job$kill())
  # The request returns at once; the analysis runs in another process.
  # (The time is checked only when SIEVER_TIMING_TESTS is true.)
  if (siever_timing_checks()) expect_lt(proc.time()[["elapsed"]] - started, 3)
  expect_false(identical(job$process$get_pid(), Sys.getpid()))
  stages <- character()
  if (!siever_wait_until(function() !job$poll(), "The background MI process", step = function() {
    stages <<- c(stages, job$progress$stage)
    Sys.sleep(.05)
  })) {
    return(invisible())
  }
  job$poll()
  expect_false(job$process$is_alive())
  # The progress the page reads counts the pilot's and the final fits.
  expect_true(all(c("pilot", "final") %in% stages))
  expect_identical(job$progress$total, sync$imputations)
  run <- job$result()
  expect_true(run$ok, info = run$error)
  expect_false(dir.exists(job$dir))
  # Pooling exactly as in this process: every element but the lavaan.mi
  # fit is identical, bit for bit.
  expect_null(run$value$fit)
  expect_identical(run$value, sync[setdiff(names(sync), "fit")])
  expect_true(is.numeric(run$seconds) && run$seconds > 0)
  # The provenance of the step: the background process records the mice
  # and lavaan.mi that computed it, which the Shiny process never loads.
  # They are the builds this process loaded for the synchronous run, with
  # the same keys (the replay check compares keys).
  expect_identical(names(run$engine_identity), c("mice", "lavaan.mi"))
  expect_true(all(vapply(run$engine_identity, function(engine)
    identical(engine$status, "loaded") && is.character(engine$id) && nzchar(engine$id), logical(1))))
  expect_identical(sieve_engine_keys(run$engine_identity),
    sieve_engine_keys(sieve_engine_identity(c("mice", "lavaan.mi"))))
  # Why the fit stays behind: read in a fresh R process, the background
  # result loads no lavaan.mi there, whereas the synchronous result, whose
  # fit holds a closure of lavaan.mi's namespace, loads it (and, with
  # lavaan not attached, keeps its methods from dispatching).
  loads <- function(x) {
    force(x)
    "lavaan.mi" %in% loadedNamespaces()
  }
  environment(loads) <- globalenv()
  expect_false(callr::r(loads, list(run$value), libpath = .libPaths(), user_profile = FALSE))
  expect_true(callr::r(loads, list(sync), libpath = .libPaths(), user_profile = FALSE))
})

test_that("killing the background MI process ends it at once and removes its folder", {
  skip_on_cran()
  d <- mi_background_data()
  job <- sieve_mi_start(list(model = "Textual =~ x4 + x5 + x6", data = d, ordered = names(d),
    fun = "cfa", seed = 1L))
  expect_true(dir.exists(job$dir))
  job$kill()
  expect_false(job$process$is_alive())
  expect_false(dir.exists(job$dir))
})

test_that("the MI panel states the wait, shows the fits with Cancel, and a canceled run records nothing", {
  st <- mi_ordered_study()
  test_frame <- environment()
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_mi_start = mi_fake_start(alive = TRUE,
    progress = list(stage = "final", done = 31L, total = 70L), log = log),
    sieve_installed = mi_suggests_installed(), .env = test_frame)
  parent <- shiny::reactiveVal(list(spec = shiny::isolate(sieve_cfa_spec(st$meta)), fit = list(input = 1L)))
  server <- function(input, output, session) {
    mi <- sieve_mi_server(input, output, session, st, parent, function() TRUE)
  }
  before <- length(shiny::isolate(st$analyses))
  shiny::testServer(server, {
    wait <- paste(as.character(output$mi_sensitivity_wait$html), collapse = " ")
    expect_match(wait, "20 pilot fits, then M final fits", fixed = TRUE)
    expect_match(wait, "(20 to 201 at CV = .05;", fixed = TRUE)
    expect_match(wait, "a fit of these 9 observed variables", fixed = TRUE)
    expect_match(wait, "It runs in a background R process", fixed = TRUE)
    session$setInputs(mi_sensitivity_go = 1L, mi_sensitivity_seed = 1L)
    expect_identical(log$starts, 1L)
    expect_true(mi$running())
    # The request carried the session's random-number settings and the
    # ordered indicators the check accepted.
    expect_identical(log$rng_kind, RNGkind())
    expect_identical(log$args$seed, 1L)
    expect_true(all(sieve_hs9_fixture()$theory_map$Textual %in% log$args$ordered))
    session$elapse(600)
    # The progress line is text; Cancel is static UI outside it, shown by
    # the running flag (see the next test).
    expect_identical(output$mi_sensitivity_running, "true")
    progress <- output$mi_sensitivity_progress
    expect_type(progress, "character")
    expect_match(progress, "Running in a background R process: final fit 31 of 70, after the 20 pilot fits;", fixed = TRUE)
    expect_false(grepl("mi_sensitivity_stop", progress, fixed = TRUE))
    # A second click while it runs starts nothing.
    session$setInputs(mi_sensitivity_go = 2L)
    expect_identical(log$starts, 1L)
    session$setInputs(mi_sensitivity_stop = 1L)
    expect_identical(log$killed, 1L)
    expect_false(mi$running())
    expect_identical(mi$shown()$reason,
      "Stopped: canceled after the 20 pilot fits and 31 of 70 final fits; nothing was computed or recorded.")
    expect_identical(output$mi_sensitivity_running, "false")
    expect_identical(output$mi_sensitivity_progress, "")
  })
  expect_identical(length(shiny::isolate(st$analyses)), before)
  expect_false(any(grepl("sieve_mi_model", shiny::isolate(st$codelog), fixed = TRUE)))
})

test_that("the MI panel's Cancel button is static UI that the polls never redraw", {
  # A progress line drawn by a renderUI() at every poll (twice a second),
  # Cancel button included, would drop keyboard focus on the button to the
  # page body within half a second, and a press straddling a redraw would
  # reach no button. The button is in sieve_mi_ui(), in a panel shown by
  # the running flag, and only the sentence is redrawn.
  ui <- as.character(sieve_mi_ui(shiny::NS("cfa")))
  expect_equal(lengths(regmatches(ui, gregexpr('id="cfa-mi_sensitivity_stop"', ui, fixed = TRUE))), 1L)
  expect_match(ui, "Cancel the MI run", fixed = TRUE)
  expect_match(ui, "data-display-if=\"output[&#39;cfa-mi_sensitivity_running&#39;] === &#39;true&#39;\"", fixed = TRUE)
  expect_match(ui, '<span role="status">', fixed = TRUE)
  expect_match(ui, 'id="cfa-mi_sensitivity_progress" class="shiny-text-output', fixed = TRUE)
  expect_false(grepl('id="cfa-mi_sensitivity_progress" class="shiny-html-output', ui, fixed = TRUE))
  st <- mi_ordered_study()
  test_frame <- environment()
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_mi_start = mi_fake_start(alive = TRUE,
    progress = list(stage = "pilot", done = 4L, total = 20L), log = log),
    sieve_installed = mi_suggests_installed(), .env = test_frame)
  parent <- shiny::reactiveVal(list(spec = shiny::isolate(sieve_cfa_spec(st$meta)), fit = list(input = 1L)))
  server <- function(input, output, session) {
    mi <- sieve_mi_server(input, output, session, st, parent, function() TRUE)
  }
  shiny::testServer(server, {
    expect_identical(output$mi_sensitivity_running, "false")
    expect_identical(output$mi_sensitivity_progress, "")
    session$setInputs(mi_sensitivity_go = 1L, mi_sensitivity_seed = 1L)
    expect_identical(output$mi_sensitivity_running, "true")
    # Polls bump the tick; each one redraws the sentence alone, as text.
    for (i in 1:3) {
      session$elapse(500)
      expect_identical(output$mi_sensitivity_running, "true")
      expect_type(output$mi_sensitivity_progress, "character")
      expect_match(output$mi_sensitivity_progress, "pilot fit 4 of 20", fixed = TRUE)
    }
    # The static button still stops the run.
    session$setInputs(mi_sensitivity_stop = 1L)
    expect_identical(log$killed, 1L)
    expect_identical(output$mi_sensitivity_running, "false")
  })
})

test_that("a finished background MI run is recorded with its seed, and a change while it runs stops it", {
  st <- mi_ordered_study()
  test_frame <- environment()
  log <- new.env(parent = emptyenv())
  start <- mi_fake_start(log = log)
  local_mocked_bindings(sieve_mi_start = function(args, rng_kind = NULL) start(args, rng_kind),
    sieve_installed = mi_suggests_installed(), .env = test_frame)
  parent <- shiny::reactiveVal(list(spec = shiny::isolate(sieve_cfa_spec(st$meta)), fit = list(input = 1L)))
  server <- function(input, output, session) {
    mi <- sieve_mi_server(input, output, session, st, parent, function() TRUE)
  }
  shiny::testServer(server, {
    session$setInputs(mi_sensitivity_go = 1L, mi_sensitivity_seed = 7L)
    session$elapse(600)
    expect_false(mi$running())
    expect_identical(mi$shown()$value$imputations, 20L)
    expect_match(mi$shown()$reason, "M = 20; 20 fits pooled", fixed = TRUE)
  })
  records <- shiny::isolate(st$analyses)
  spec <- records[[length(records)]]$spec
  expect_identical(spec$kind, "ordered_mi")
  expect_identical(spec$seed, 7L)
  code <- paste(shiny::isolate(st$codelog), collapse = "\n")
  expect_match(code, "SIEVER::sieve_mi_model(", fixed = TRUE)
  expect_match(code, "seed = 7,", fixed = TRUE)

  # A run whose ordinary model is refit while it runs is stopped, and
  # nothing of it is recorded.
  st <- mi_ordered_study()
  log <- new.env(parent = emptyenv())
  start <- mi_fake_start(alive = TRUE, progress = list(stage = "pilot", done = 4L, total = 20L), log = log)
  parent <- shiny::reactiveVal(list(spec = shiny::isolate(sieve_cfa_spec(st$meta)), fit = list(input = 1L)))
  shiny::testServer(server, {
    session$setInputs(mi_sensitivity_go = 1L, mi_sensitivity_seed = 7L)
    expect_true(mi$running())
    changed <- parent()
    changed$fit <- list(input = 2L)
    parent(changed)
    session$elapse(600)
    expect_identical(log$killed, 1L)
    expect_false(mi$running())
    expect_match(mi$shown()$reason, "historical", fixed = TRUE)
  })
  expect_length(shiny::isolate(st$analyses), 0L)
  expect_false(any(grepl("sieve_mi_model", shiny::isolate(st$codelog), fixed = TRUE)))
})

test_that("a run whose model the page can no longer give is stopped, not left running", {
  # The SEM page's model reactive stops on req() when its model choices
  # change. A poll that read it inside isolate() would end silently every
  # time: the run would be neither stopped nor read, and the panel would
  # say it was running for as long as the model was unavailable.
  st <- mi_ordered_study()
  test_frame <- environment()
  log <- new.env(parent = emptyenv())
  start <- mi_fake_start(alive = TRUE, progress = list(stage = "pilot", done = 4L, total = 20L), log = log)
  local_mocked_bindings(sieve_mi_start = function(args, rng_kind = NULL) start(args, rng_kind),
    sieve_installed = mi_suggests_installed(), .env = test_frame)
  available <- shiny::reactiveVal(TRUE)
  spec <- shiny::isolate(sieve_cfa_spec(st$meta))
  parent <- shiny::reactive({
    shiny::req(available())
    list(spec = spec, fit = list(input = 1L))
  })
  server <- function(input, output, session) {
    mi <- sieve_mi_server(input, output, session, st, parent, function() TRUE)
  }
  shiny::testServer(server, {
    session$setInputs(mi_sensitivity_go = 1L, mi_sensitivity_seed = 7L)
    expect_true(mi$running())
    available(FALSE)
    session$flushReact()
    session$elapse(600)
    expect_identical(log$killed, 1L)
    expect_false(mi$running())
  })
  expect_length(shiny::isolate(st$analyses), 0L)
})

test_that("a finished background MI run records the mice and lavaan.mi that computed it", {
  # Neither is loaded in the Shiny process, so the step's engine identity
  # takes them from the background process (sieve_mi_worker()), not "not
  # loaded" with no ID, which the replay check, comparing only engines with
  # an ID, would leave out.
  st <- mi_ordered_study()
  test_frame <- environment()
  engines <- mi_fake_engines()
  local_mocked_bindings(sieve_mi_start = mi_fake_start(engines = engines),
    sieve_installed = mi_suggests_installed(), .env = test_frame)
  parent <- shiny::reactiveVal(list(spec = shiny::isolate(sieve_cfa_spec(st$meta)), fit = list(input = 1L)))
  server <- function(input, output, session) {
    mi <- sieve_mi_server(input, output, session, st, parent, function() TRUE)
  }
  shiny::testServer(server, {
    session$setInputs(mi_sensitivity_go = 1L, mi_sensitivity_seed = 3L)
    session$elapse(600)
    expect_false(mi$running())
    expect_identical(mi$shown()$value$imputations, 20L)
  })
  operations <- shiny::isolate(st$replay_operations)
  step <- names(operations)[length(operations)]
  operation <- operations[[step]]
  expect_match(operation$note, "Ordered-item MI sensitivity: M = 20", fixed = TRUE)
  expect_identical(operation$engine_identity[c("mice", "lavaan.mi")], engines)
  # The engines of the Shiny process are recorded as this process has them.
  here <- c("SIEVER", "DMAR", "lavaan", "psych")
  expect_identical(sieve_engine_keys(operation$engine_identity[here]),
    sieve_engine_keys(sieve_engine_identity(here)))
  # The analysis record carries them too.
  records <- shiny::isolate(st$analyses)
  expect_identical(records[[length(records)]]$engine_identity[c("mice", "lavaan.mi")], engines)
  # And the replay check of the step compares them.
  store <- sieve_replay_bundle(st)
  recorded <- store$engine_identity[[store$engine_steps[[step]]]]
  compared <- names(recorded)[vapply(recorded, function(engine) !is.null(engine$id), logical(1))]
  expect_true(all(c("mice", "lavaan.mi") %in% compared))
})

test_that("without mice or lavaan.mi the MI panel refuses at once, saying what to install, and starts nothing", {
  st <- mi_ordered_study()
  test_frame <- environment()
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_mi_start = mi_fake_start(log = log),
    sieve_installed = mi_suggests_installed(FALSE), .env = test_frame)
  parent <- shiny::reactiveVal(list(spec = shiny::isolate(sieve_cfa_spec(st$meta)), fit = list(input = 1L)))
  server <- function(input, output, session) {
    mi <- sieve_mi_server(input, output, session, st, parent, function() TRUE)
  }
  shiny::testServer(server, {
    session$setInputs(mi_sensitivity_go = 1L, mi_sensitivity_seed = 1L)
    session$elapse(600)
    expect_identical(log$starts, 0L)
    expect_false(mi$running())
    expect_identical(mi$shown()$reason, "Install mice to request the ordered-item MI sensitivity analysis.")
  })
  expect_length(shiny::isolate(st$analyses), 0L)
  expect_false(any(grepl("sieve_mi_model", shiny::isolate(st$codelog), fixed = TRUE)))
})

test_that("the MI wait and its words follow the plan's limits and the stages", {
  expect_identical(sieve_mi_most_imputations(), sieve_mi_plan(1)$imputations)
  expect_identical(sieve_mi_most_imputations(), 201L)
  wait <- sieve_mi_wait(9L)
  expect_equal(wait$per_fit, sieve_rate("mi_fit") * 36)
  expect_equal(wait$pilot, 20 * wait$per_fit)
  expect_equal(wait$most, (20 + 201) * wait$per_fit)
  expect_identical(sieve_mi_progress_words(list(stage = "pilot", done = 12L, total = 20L)), "pilot fit 12 of 20")
  expect_identical(sieve_mi_cancel_text(list(stage = "pilot", done = 12L, total = 20L)),
    "Stopped: canceled after 12 of 20 pilot fits; nothing was computed or recorded.")
  expect_identical(sieve_mi_cancel_text(list(stage = "starting", done = 0L, total = NA_integer_)),
    "Stopped: canceled before the background R process began; nothing was computed or recorded.")
})

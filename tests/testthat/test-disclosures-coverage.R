# Show code at the point of use (the actual call first, copy and download,
# a closed provenance panel, disclosures tied to the displayed result on
# the Items, Validity, CV and Invariance, SEM, Bifactor and Optimizer
# pages), the content coverage advisory beside every inclusion decision,
# and conditional estimator guidance on the CFA tab.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("lavaan")
skip_if_not_installed("DMAR")
skip_if_not_installed("shiny")
withr::local_options(sieve.cores = 2L)

dc_html <- function(x) paste(as.character(unlist(x)), collapse = "\n")
dc_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", dc_html(x)))

dc_state <- function(fixture = sieve_hs9_fixture()) {
  siever_test_study(fixture, configure = function(s) {
    s$roles$role[s$roles$role == "auxiliary"] <- "criterion"
  })
}

# Facets on the Visual items: two in Spatial, one (t4) alone in Closure, so
# excluding t4 empties a facet without emptying its construct.
dc_facets <- function(s) {
  m <- shiny::isolate(s$meta)
  m$facet[m$item %in% c("t1_visual_perception", "t2_cubes")] <- "Spatial"
  m$facet[m$item == "t4_lozenges"] <- "Closure"
  s$meta <- m
  s
}

# Every engine call: lavaan's own lavaan() (behind every lavaan::cfa(),
# lavaan::sem() and DMAR fit), psych::fa, and DMAR::fa_exploratory (behind
# the Items diagnostics), as test-ondemand.R counts them.
dc_count <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$lavaan <- 0L
  seen$fa <- 0L
  seen$dmar <- 0L
  real_dmar <- DMAR::fa_exploratory
  testthat::local_mocked_bindings(
    fa_exploratory = function(...) {
      seen$dmar <- seen$dmar + 1L
      real_dmar(...)
    },
    .package = "DMAR", .env = env
  )
  real_lavaan <- lavaan::lavaan
  testthat::local_mocked_bindings(
    lavaan = function(...) {
      seen$lavaan <- seen$lavaan + 1L
      real_lavaan(...)
    },
    .package = "lavaan", .env = env
  )
  real_fa <- psych::fa
  testthat::local_mocked_bindings(
    fa = function(...) {
      seen$fa <- seen$fa + 1L
      real_fa(...)
    },
    .package = "psych", .env = env
  )
  seen
}
dc_calls <- function(seen) c(lavaan = seen$lavaan, fa = seen$fa)

# ---- 1. The code view: the call first, actions, provenance closed ----------

test_that("a code view shows the actual call first, then the readout, the actions, and a closed provenance panel", {
  record <- list(
    code = c("# [7] Fit the CFA", "cfa_fit <- lavaan::cfa(cfa_model, data = items)"), step = "7",
    context = "Estimator: MLR.", input_key = "rev-abc", input_rows = 301L, item_names = c("x1", "x2"),
    versions = c(R = "4.4.0", SIEVER = "0.1.0"), namespace_paths = c(SIEVER = "/lib/SIEVER"),
    recorded_at = "2026-10-22 10:00:00 UTC"
  )
  html <- as.character(sieve_code_view(record, "current", ns = shiny::NS("cfa"), readout = "lavaan::fitMeasures(cfa_fit)"))
  at <- function(text) regexpr(text, html, fixed = TRUE)[[1]]
  expect_true(at("sieve-code-source") > 0)
  expect_true(at("sieve-code-source") < at("Reading the fitted result"))
  expect_true(at("Reading the fitted result") < at("Copy recorded code"))
  expect_true(at("Copy recorded code") < at("Download this analysis"))
  expect_true(at("Download this analysis") < at("Recorded inputs and provenance"))
  expect_true(at("Recorded inputs and provenance") < at("rev-abc"))
  expect_true(at("Recorded inputs and provenance") < at("/lib/SIEVER"))
  # The provenance is a nested details, closed, inside the closed panel.
  expect_false(grepl("<details[^>]*\\bopen", html))
  expect_identical(lengths(regmatches(html, gregexpr("<details", html, fixed = TRUE))), 2L)
  expect_match(html, 'data-sieve-step="7"', fixed = TRUE)
  expect_match(html, 'data-sieve-input="cfa-code_download"', fixed = TRUE)
  expect_match(html, "sieve-code-copy-note", fixed = TRUE)
  # Without a module namespace the copy stays (it is done in the browser)
  # and the download, which a module serves, is not offered.
  plain <- as.character(sieve_code_view(record, "current"))
  expect_match(plain, "Copy recorded code", fixed = TRUE)
  expect_false(grepl("Download this analysis", plain, fixed = TRUE))
  # No record, no actions.
  none <- as.character(sieve_code_view(NULL, "not_run", ns = shiny::NS("cfa")))
  expect_false(grepl("Copy recorded code", none, fixed = TRUE))
  expect_false(grepl("Download this analysis", none, fixed = TRUE))
  expect_false(grepl("Recorded inputs and provenance", none, fixed = TRUE))
})

test_that("the analysis bundle holds the recorded code and a README naming the revision, and the input view only under the sharing choice", {
  record <- list(
    code = c("# [3] Validity: AVE per construct", "ave <- DMAR::average_variance_extracted(fit = validity_fit)"),
    step = "3", note = "Validity: AVE per construct", context = "Estimator: MLR.",
    input_key = "rev-xyz", input_rows = 12L, item_names = c("a", "b"),
    versions = c(R = "4.4.0", SIEVER = "0.1.0"), recorded_at = "2026-10-22 10:00:00 UTC"
  )
  input <- data.frame(a = 1:3, b = c(2.5, NA, 4))
  listed <- function(file) sort(utils::untar(file, list = TRUE))
  read <- function(file, name) {
    dir <- tempfile("bundle_")
    utils::untar(file, exdir = dir)
    readLines(file.path(dir, name))
  }
  withheld <- tempfile(fileext = ".tar.gz")
  sieve_code_bundle_write(record, withheld, input = input, shared = FALSE)
  expect_identical(listed(withheld), c("README.txt", "recorded_code.R"))
  readme <- read(withheld, "README.txt")
  expect_true(any(grepl("Input view: withheld.", readme, fixed = TRUE)))
  expect_true(any(grepl("Revision rev-xyz identifies it", readme, fixed = TRUE)))
  expect_true(any(grepl("Analysis: Validity: AVE per construct", readme, fixed = TRUE)))
  expect_true(any(grepl("Input respondent rows: 12", readme, fixed = TRUE)))
  expect_true(any(grepl("Item columns in the recorded input view: a, b", readme, fixed = TRUE)))
  expect_true(any(grepl("Execution versions: R 4.4.0; SIEVER 0.1.0", readme, fixed = TRUE)))
  expect_identical(read(withheld, "recorded_code.R"), record$code)
  shared <- tempfile(fileext = ".tar.gz")
  sieve_code_bundle_write(record, shared, input = input, shared = TRUE)
  expect_identical(listed(shared), c("README.txt", "input_view.csv", "recorded_code.R"))
  expect_true(any(grepl("Input view: included", read(shared, "README.txt"), fixed = TRUE)))
  dir <- tempfile("bundle_")
  utils::untar(shared, exdir = dir)
  expect_equal(utils::read.csv(file.path(dir, "input_view.csv")), input)
  # The choice released but the revision not held: withheld, with the reason.
  absent <- tempfile(fileext = ".tar.gz")
  sieve_code_bundle_write(record, absent, input = NULL, shared = TRUE)
  expect_identical(listed(absent), c("README.txt", "recorded_code.R"))
  expect_true(any(grepl("not held in this session", read(absent, "README.txt"), fixed = TRUE)))
})

test_that("the sharing choice withholds the input view by default and releases it when set", {
  s <- dc_state()
  expect_false(sieve_sharing_allows(s))
  expect_false(sieve_sharing_allows(NULL))
  sieve_sharing_set(s, input_view = TRUE)
  expect_true(sieve_sharing_allows(s))
  sieve_sharing_set(s, input_view = FALSE)
  expect_false(sieve_sharing_allows(s))
})

test_that("a record found by its note prefix is current until the ledger changes, then historical with what changed", {
  s <- dc_state()
  sieve_log(s, "Items: exploratory diagnostics of 9 included items (k = 3)", "items_efa <- psych::fa(R_items)")
  sieve_log(s, "Another step", "x <- 1")
  record <- sieve_code_record(s, "Items: exploratory diagnostics")
  expect_identical(record$step, "1")
  expect_match(record$code[2], "psych::fa", fixed = TRUE)
  expect_identical(record$revision, shiny::isolate(sieve_revision(s)))
  expect_s3_class(record$ledger, "data.frame")
  expect_identical(sieve_code_status(record, s)$state, "current")
  expect_null(sieve_code_record(s, "Nothing recorded under this"))
  # The module's own status wins when it says stale or failed.
  expect_identical(sieve_code_status(record, s, list(state = "stale", sentence = "an edit"))$sentence, "an edit")
  expect_identical(sieve_code_status(record, s, list(state = "failed"))$state, "failed")
  expect_identical(sieve_code_status(NULL, s)$state, "not_run")
  sieve_set_item_status(s, "t2_cubes", "parked", "probe")
  st <- sieve_code_status(record, s)
  expect_identical(st$state, "stale")
  expect_match(st$sentence, "What changed: t2_cubes excluded.", fixed = TRUE)
  # The regular expression form finds a step whose note varies.
  sieve_log(s, "Partial scalar invariance across 'school': freed x1", "y <- 2")
  expect_identical(sieve_code_step(s, "^Partial (metric|scalar|strict) invariance across", regex = TRUE), "4")
  html <- as.character(shiny::isolate(sieve_code_disclosure(s, "Items: exploratory diagnostics", ns = shiny::NS("winnow"))))
  expect_match(html, "earlier result on screen", fixed = TRUE)
  expect_match(html, "t2_cubes excluded", fixed = TRUE)
})

# ---- 2. Copy and download run no analysis -----------------------------------

test_that("the CFA download serves the panel's recorded code, respects the sharing choice, and fits nothing", {
  state <- dc_state()
  seen <- dc_count()
  served <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    sieve_code_download_register = function(session, record, state = NULL) {
      served$record <- record
      served$state <- state
      invisible("session/x/download/cfa-code_bundle_1")
    },
    sieve_code_download_send = function(session, url) {
      served$url <- url
      invisible(NULL)
    },
    .package = "SIEVER"
  )
  shiny::testServer(mod_cfa_server, args = list(state = state), {
    session$setInputs(scope = "construct", structure = "first", ident = "varone")
    session$setInputs(fit_go = 1L)
    saved <- demand$status()$value$code_view
    expect_type(saved$step, "character")
    after <- dc_calls(seen)
    # Reading the panel registers the record it shows; the download reads it
    # back and recomputes nothing.
    html <- dc_html(output$est_code)
    expect_match(html, sprintf('data-sieve-step="%s"', saved$step), fixed = TRUE)
    expect_match(html, sprintf('data-sieve-input="%s"', session$ns("code_download")), fixed = TRUE)
    session$setInputs(code_download = list(step = saved$step, nonce = 1))
    expect_identical(served$record$code, saved$code)
    expect_identical(served$record$context, saved$context)
    expect_true(identical(served$state, state))
    expect_identical(served$url, "session/x/download/cfa-code_bundle_1")
    expect_identical(dc_calls(seen), after)
    # An unknown step is refused with a notice and nothing else.
    session$setInputs(code_download = list(step = "999", nonce = 2))
    expect_identical(dc_calls(seen), after)
    # The bundle from that record: the input view only under the sharing choice.
    file <- tempfile(fileext = ".tar.gz")
    sieve_code_bundle_write(served$record, file, shared = sieve_sharing_allows(state))
    expect_false("input_view.csv" %in% utils::untar(file, list = TRUE))
    sieve_sharing_set(state, input_view = TRUE)
    view <- sieve_code_input_view(state, served$record)
    expect_identical(dim(view), dim(shiny::isolate(state$items)))
    expect_equal(unname(as.matrix(view)), unname(as.matrix(shiny::isolate(state$items))))
    sieve_code_bundle_write(served$record, file, input = view, shared = sieve_sharing_allows(state))
    expect_true("input_view.csv" %in% utils::untar(file, list = TRUE))
    expect_identical(dc_calls(seen), after)
  })
})

# ---- 3. Disclosures tied to the displayed result ----------------------------

test_that("the Items disclosures are current after the diagnostics and the overall model, historical after an exclusion, and read without an engine call", {
  state <- dc_state()
  seen <- dc_count()
  test_frame <- environment()
  shiny::testServer(mod_winnow_server, args = list(state = state), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    # The first result runs by itself (nine items, under two seconds) as
    # soon as the page's outputs render, so the panels are read after it.
    invisible(diag())
    invisible(overall_fit())
    session$flushReact()
    after <- dc_calls(seen)
    # The diagnostics' factor analysis is DMAR::fa_exploratory(), fit
    # through lavaan; psych::fa is not called.
    expect_true(seen$dmar >= 1L && after[["lavaan"]] >= 1L)
    expect_identical(after[["fa"]], 0L)
    html <- dc_html(output$diag_code)
    expect_match(html, "belong to the result on screen", fixed = TRUE)
    expect_match(html, "DMAR::fa_exploratory(", fixed = TRUE)
    expect_match(html, "S = R_items", fixed = TRUE)
    expect_match(html, "items_reliability", fixed = TRUE)
    expect_match(html, "Recorded inputs and provenance", fixed = TRUE)
    model <- dc_html(output$model_code)
    expect_match(model, "belong to the result on screen", fixed = TRUE)
    expect_match(model, "items_overall_fit &lt;- lavaan::cfa", fixed = TRUE)
    expect_match(model, "Items: overall measurement model", fixed = TRUE)
    expect_identical(dc_calls(seen), after)
    # The recorded overall model replays to the fit on screen.
    replay <- new.env(parent = globalenv())
    replay$items <- shiny::isolate(state$items)
    replay$d <- shiny::isolate(state$raw)
    invisible(utils::capture.output(eval(parse(text = model_record()$record$code), envir = replay)))
    expect_equal(lavaan::coef(replay$items_overall_fit), lavaan::coef(overall_fit()), tolerance = 1e-10)
    # An exclusion: the diagnostics wait (a slow pool) and the model waits for
    # Update; both panels are historical, naming the change, with no fit.
    # Registered on the test's own frame: a block of testServer is not a
    # function frame, so a mock made on it would outlive the test and make
    # the diagnostics wait in every later file.
    testthat::local_mocked_bindings(sieve_diagnostics_wait = function(...) 30, .env = test_frame)
    before <- dc_calls(seen)
    sieve_set_item_status(state, "t2_cubes", "parked", "probe")
    session$flushReact()
    expect_identical(diag_run()$status, "not run")
    historical <- dc_text(output$diag_code)
    expect_match(historical, "earlier result on screen", fixed = TRUE)
    expect_match(historical, "t2_cubes excluded", fixed = TRUE)
    model_historical <- dc_text(output$model_code)
    expect_match(model_historical, "earlier result on screen", fixed = TRUE)
    expect_match(model_historical, "t2_cubes excluded", fixed = TRUE)
    expect_identical(dc_calls(seen), before)
  })
})

test_that("the Validity AVE and HTMT disclosures follow the measurement model, read without a fit, and are historical after respondent exclusion", {
  state <- dc_state()
  seen <- dc_count()
  shiny::testServer(mod_validity_server, args = list(state = state), {
    session$setInputs(estimator = "study", htmt_on = TRUE, htmt_thresh = 0.85)
    expect_match(dc_text(output$ave_code), "Run this analysis", fixed = TRUE)
    expect_identical(dc_calls(seen), c(lavaan = 0L, fa = 0L))
    session$setInputs(fit_go = 1)
    invisible(output$ave_tbl)
    invisible(output$htmt_tbl)
    after <- dc_calls(seen)
    expect_true(after[["lavaan"]] >= 1L)
    ave <- dc_html(output$ave_code)
    expect_match(ave, "belong to the result on screen", fixed = TRUE)
    expect_match(ave, "DMAR::average_variance_extracted", fixed = TRUE)
    expect_match(ave, sprintf('data-sieve-input="%s"', session$ns("code_download")), fixed = TRUE)
    htmt <- dc_html(output$htmt_code)
    expect_match(htmt, "belong to the result on screen", fixed = TRUE)
    expect_match(htmt, "DMAR::htmt(active_items", fixed = TRUE)
    expect_match(dc_text(output$htmt_ci_code), "Run this analysis", fixed = TRUE)
    expect_match(dc_text(output$crit_code), "Run this analysis", fixed = TRUE)
    expect_identical(dc_calls(seen), after)
    sieve_set_case_status(state, 1L, "excluded", "probe")
    session$flushReact()
    expect_identical(demand$status()$state, "stale")
    stale <- dc_text(output$ave_code)
    expect_match(stale, "earlier result on screen", fixed = TRUE)
    expect_match(stale, "respondent", fixed = TRUE)
    # The HTMT point estimates follow the pool as the table does (DMAR::htmt
    # is immediate), so their record is the new step and current.
    invisible(output$htmt_tbl)
    expect_match(dc_text(output$htmt_code), "earlier result on screen", fixed = TRUE)
    expect_identical(dc_calls(seen), after)
  })
})

test_that("the SEM disclosure is current after the fit and historical after respondent exclusion, with the recorded call", {
  fx <- sieve_hs9_fixture()
  raw <- fx$data
  i <- seq_len(nrow(raw))
  raw$criterion <- as.numeric(scale(raw[[fx$item_cols[4L]]])) / 3 + 2 * sin(i)
  state <- sieve_new_state()
  state$raw <- raw
  state$items <- raw[, fx$item_cols, drop = FALSE]
  state$meta <- sieve_build_meta(raw, fx$item_cols, fx$theory_map, labels = fx$labels)
  roles <- sieve_declared_roles(raw, fx$item_cols)
  roles$role[roles$variable == "criterion"] <- "criterion"
  state$roles <- roles
  state$confirmed <- TRUE
  state$estimator_choice <- "mlr"
  shiny::isolate(sieve_new_dataset(state))
  seen <- dc_count()
  shiny::testServer(mod_sem_server, args = list(state = state), {
    session$setInputs(outcomes = "criterion", predictors = "Visual", estimator = "mlr", layout = "tree2", min_path = 0)
    expect_match(dc_text(output$sem_code), "Run this analysis", fixed = TRUE)
    expect_identical(dc_calls(seen), c(lavaan = 0L, fa = 0L))
    session$setInputs(fit_go = 1)
    after <- dc_calls(seen)
    html <- dc_html(output$sem_code)
    expect_match(html, "belong to the result on screen", fixed = TRUE)
    expect_match(html, "sem_fit &lt;- lavaan::sem", fixed = TRUE)
    expect_match(html, "Outcomes: criterion; predictors: Visual.", fixed = TRUE)
    expect_match(dc_html(output$effects_code), "sem_std_effects", fixed = TRUE)
    expect_identical(dc_calls(seen), after)
    sieve_set_case_status(state, 1L, "excluded", "probe")
    session$flushReact()
    stale <- dc_text(output$sem_code)
    expect_match(stale, "earlier result on screen", fixed = TRUE)
    expect_match(stale, "respondent", fixed = TRUE)
    expect_identical(dc_calls(seen), after)
  })
})

test_that("the Bifactor disclosure is current after the fit, and the bootstrap's after its run, both read without a fit", {
  state <- dc_state()
  seen <- dc_count()
  shiny::testServer(mod_bifactor_server, args = list(state = state), {
    session$setInputs(estimator = "study")
    expect_match(dc_text(output$bf_code), "Run this analysis", fixed = TRUE)
    expect_match(dc_text(output$boot_code), "Run this analysis", fixed = TRUE)
    suppressWarnings(session$setInputs(fit = 1))
    after <- dc_calls(seen)
    html <- dc_html(output$bf_code)
    expect_match(html, "belong to the result on screen", fixed = TRUE)
    expect_match(html, "bifactor_fit &lt;- lavaan::cfa", fixed = TRUE)
    expect_match(dc_text(output$boot_code), "Run this analysis", fixed = TRUE)
    expect_identical(dc_calls(seen), after)
    sieve_set_item_status(state, "t2_cubes", "parked", "probe")
    session$flushReact()
    stale <- dc_text(output$bf_code)
    expect_match(stale, "earlier result on screen", fixed = TRUE)
    expect_match(stale, "t2_cubes excluded", fixed = TRUE)
    expect_identical(dc_calls(seen), after)
  })
})

test_that("the CV and Invariance split disclosure and the scoring syntax read their records without a fit", {
  state <- dc_state()
  seen <- dc_count()
  shiny::testServer(mod_finalize_server, args = list(state = state), {
    session$setInputs(holdout = 50, seed = 113, estimator = "mlr")
    expect_match(dc_text(output$cv_code), "Run this analysis", fixed = TRUE)
    expect_match(dc_text(output$inv_code), "Run this analysis", fixed = TRUE)
    expect_match(dc_text(output$align_code), "Run this analysis", fixed = TRUE)
    expect_match(dc_text(output$partial_code), "Run this analysis", fixed = TRUE)
    expect_match(dc_text(output$norm_transform_code), "Run this analysis", fixed = TRUE)
    syntax <- dc_html(output$scoring_code)
    expect_match(syntax, "belong to the result on screen", fixed = TRUE)
    expect_match(syntax, "Copy recorded code", fixed = TRUE)
    expect_identical(dc_calls(seen), c(lavaan = 0L, fa = 0L))
    session$setInputs(split = 1)
    expect_null(cv_shown()$error)
    after <- dc_calls(seen)
    html <- dc_html(output$cv_code)
    expect_match(html, "belong to the result on screen", fixed = TRUE)
    expect_match(html, "SIEVER::sieve_cross_validate", fixed = TRUE)
    expect_identical(dc_calls(seen), after)
    session$setInputs(holdout = 40)
    session$flushReact()
    expect_true(isTRUE(cv_shown()$stale))
    stale <- dc_text(output$cv_code)
    expect_match(stale, "earlier result on screen", fixed = TRUE)
    expect_match(stale, "settings for this split changed", fixed = TRUE)
    expect_identical(dc_calls(seen), after)
  })
})

test_that("the Optimizer disclosure is current after a search and historical after an edit outside it", {
  state <- dc_state()
  shiny::isolate(state$construct_basis <- "empirical")
  seen <- dc_count()
  shiny::testServer(mod_optimize_server, args = list(state = state), {
    session$setInputs(max_drop = 1, min_keep = 2, ss = FALSE, cap = 200, undersized = "hold")
    expect_match(dc_text(output$search_code), "Run this analysis", fixed = TRUE)
    session$setInputs(run = 1)
    expect_null(res()$error)
    after <- dc_calls(seen)
    html <- dc_html(output$search_code)
    expect_match(html, "belong to the result on screen", fixed = TRUE)
    expect_match(html, "Item set search", fixed = TRUE)
    expect_match(html, "item sets examined", fixed = TRUE)
    expect_identical(dc_calls(seen), after)
    sieve_set_item_status(state, "t2_cubes", "parked", "probe")
    session$flushReact()
    stale <- dc_text(output$search_code)
    expect_match(stale, "earlier result on screen", fixed = TRUE)
    expect_match(stale, "What changed:", fixed = TRUE)
    expect_identical(dc_calls(seen), after)
  })
})

# ---- 4. Content coverage --------------------------------------------------

test_that("sieve_item_coverage counts included items per construct and facet, and sieve_coverage_change names what is emptied", {
  meta <- data.frame(
    item = paste0("x", 1:6),
    construct = rep(c("Visual", "Textual"), each = 3L),
    facet = c("Spatial", "Spatial", "Closure", NA, "", NA),
    status = "active", stringsAsFactors = FALSE
  )
  before <- sieve_item_coverage(meta)
  expect_identical(before$level, c("construct", "construct", "facet", "facet"))
  expect_identical(before$construct, c("Visual", "Textual", "Visual", "Visual"))
  expect_identical(before$facet, c(NA, NA, "Spatial", "Closure"))
  expect_identical(before$included, c(3L, 3L, 2L, 1L))
  expect_identical(before$total, c(3L, 3L, 2L, 1L))
  meta$status[3] <- "parked"
  change <- sieve_coverage_change(before, sieve_item_coverage(meta))
  gone <- change[change$emptied, ]
  expect_identical(nrow(gone), 1L)
  expect_identical(gone$level, "facet")
  expect_identical(gone$facet, "Closure")
  expect_identical(c(gone$before, gone$after), c(1L, 0L))
  expect_true(change$thin[change$level == "construct" & change$construct == "Visual"])
  expect_false(change$thin[change$level == "construct" & change$construct == "Textual"])
  expect_match(sieve_coverage_sentences(change),
    "This leaves facet Closure of construct Visual with no included items (1 included before, 0 now); its content is no longer represented.",
    fixed = TRUE)
  thin <- sieve_coverage_thin_text(sieve_item_coverage(meta))
  expect_match(thin, "Visual (2 of 3 included)", fixed = TRUE)
  expect_match(thin, "facet Closure of Visual (0 of 1 included, its content is not represented)", fixed = TRUE)
  expect_null(sieve_coverage_thin_text(sieve_item_coverage(data.frame(construct = "A", status = rep("active", 3)))))
  expect_identical(nrow(sieve_item_coverage(NULL)), 0L)
  # A facet named after the fact counts as zero before.
  later <- meta
  later$facet[4] <- "Reading"
  grown <- sieve_coverage_change(before, sieve_item_coverage(later))
  expect_identical(grown$before[grown$level == "facet" & grown$facet == "Reading"], 0L)
})

test_that("excluding the last item of a facet reports the coverage with its counts and leaves the exclusion in place", {
  s <- dc_facets(dc_state())
  out <- sieve_set_item_status(s, "t4_lozenges", "parked", "probe")
  expect_identical(out$changed, "t4_lozenges")
  expect_identical(shiny::isolate(s$meta)$status[match("t4_lozenges", shiny::isolate(s$meta)$item)], "parked")
  expect_identical(out$emptied$constructs, character(0))
  expect_identical(out$emptied$facets, data.frame(construct = "Visual", facet = "Closure", stringsAsFactors = FALSE))
  row <- out$coverage[out$coverage$emptied, ]
  expect_identical(c(row$before, row$after), c(1L, 0L))
  expect_identical(out$coverage$after[out$coverage$level == "construct" & out$coverage$construct == "Visual"], 2L)
  # Nothing changed: the report is empty and the ledger untouched.
  again <- sieve_set_item_status(s, "t4_lozenges", "parked")
  expect_length(again$changed, 0L)
  expect_false(any(again$coverage$emptied))
  # Emptying a construct names it.
  whole <- sieve_set_item_status(s, c("t1_visual_perception", "t2_cubes"), "parked")
  expect_identical(whole$emptied$constructs, "Visual")
  expect_identical(whole$emptied$facets$facet, "Spatial")
  # The advisory: counts, the citations, the link, and no reason demanded.
  html <- as.character(sieve_coverage_advisory(whole, ns = shiny::NS("drop")))
  expect_match(html, "This leaves construct Visual with no included items (2 included before, 0 now)", fixed = TRUE)
  expect_match(html, "Clark &amp; Watson, 2019", fixed = TRUE)
  expect_match(html, "AERA, APA, &amp; NCME, 2014", fixed = TRUE)
  expect_match(html, "drop-coverage_open_content", fixed = TRUE)
  expect_match(html, "The exclusion stands", fixed = TRUE)
  expect_null(sieve_coverage_advisory(again))
})

test_that("the Exclude / Include dialog shows the coverage advisory beside the decision and clears it on an inclusion", {
  s <- dc_facets(dc_state())
  shiny::testServer(mod_drop_server, args = list(state = s), {
    session$setInputs(drop_pick = "t2_cubes", drop_reason = "probe", drop_go = 1)
    expect_null(output$coverage$html %||% NULL)
    session$setInputs(drop_pick = "t4_lozenges", drop_reason = "last of its facet", drop_go = 2)
    html <- dc_html(output$coverage)
    expect_match(html, "facet Closure of construct Visual with no included items (1 included before, 0 now)", fixed = TRUE)
    expect_match(html, "Content Validity page", fixed = TRUE)
    m <- shiny::isolate(s$meta)
    expect_identical(m$status[m$item == "t4_lozenges"], "parked")
    expect_identical(m$exclude_reason[m$item == "t4_lozenges"], "last of its facet")
    session$setInputs(restore_pick = "t4_lozenges", restore_reason = "", restore_go = 1)
    expect_true(is.null(output$coverage$html) || !nzchar(dc_text(output$coverage)))
  })
})

test_that("the Items page shows the advisory above the grid after a cell edit and names an emptied construct and facet in the thin note", {
  s <- dc_facets(dc_state())
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "status", value = "Excluded"))
    summary <- dc_text(output$rel_summary)
    expect_match(summary, "Content coverage.", fixed = TRUE)
    expect_match(summary, "facet Closure of construct Visual with no included items (1 included before, 0 now)", fixed = TRUE)
    expect_match(summary, "facet Closure of Visual (0 of 1 included, its content is not represented)", fixed = TRUE)
    expect_match(summary, "Visual (2 of 3 included)", fixed = TRUE)
    expect_identical(shiny::isolate(s$meta)$status[3], "parked")
    # Including it again clears the advisory; the thin note follows the ledger.
    session$setInputs(cell_edit = list(item = "t4_lozenges", field = "status", value = "Included"))
    summary <- dc_text(output$rel_summary)
    expect_false(grepl("Content coverage.", summary, fixed = TRUE))
    # A facet of one item stays below three, but its content is represented.
    expect_match(summary, "facet Closure of Visual (1 of 1 included)", fixed = TRUE)
    expect_false(grepl("not represented", summary, fixed = TRUE))
    # Exclude selected: the whole construct, through the button.
    selected_ids(c("t1_visual_perception", "t2_cubes", "t4_lozenges"))
    session$setInputs(pool_item_ids = c("t1_visual_perception", "t2_cubes", "t4_lozenges"), exclude_selected = 1)
    summary <- dc_text(output$rel_summary)
    expect_match(summary, "This leaves construct Visual with no included items (3 included before, 0 now)", fixed = TRUE)
    expect_match(summary, "Visual (0 of 3 included, its content is not represented)", fixed = TRUE)
    expect_identical(sum(shiny::isolate(s$meta)$status == "active"), 6L)
  })
})

test_that("Screening's exclusion shows the coverage advisory beside its result", {
  s <- dc_facets(dc_state())
  shiny::testServer(mod_screen_server, args = list(state = s), {
    df <- itc()
    at <- which(df$item == "t4_lozenges")
    session$setInputs(itc_tbl_rows_selected = at, itc_reason = "last of its facet")
    session$setInputs(itc_exclude = 1L)
    html <- dc_html(output$itc_result)
    expect_match(html, "Excluded 1 item from every analysis", fixed = TRUE)
    expect_match(html, "facet Closure of construct Visual with no included items (1 included before, 0 now)", fixed = TRUE)
    expect_match(html, session$ns("coverage_open_content"), fixed = TRUE)
    expect_identical(shiny::isolate(s$meta)$status[3], "parked")
  })
})

# ---- 5. Estimator guidance on the CFA tab ---------------------------------

test_that("the CFA estimator guidance is conditional and cited, not a blanket small-sample rule", {
  # The sidebar's help text is rendered one string per line; read it as text.
  html <- dc_text(mod_cfa_ui("cfa"))
  # (ML with FIML is the default for continuous items.)
  expect_match(html, "ML with FIML (full information maximum likelihood) is the default for continuous items (DMAR's default) and the default treatment of incomplete cases", fixed = TRUE)
  expect_match(html, "Enders &(amp;)? Bandalos, 2001")
  expect_match(html, "more respondents than free parameters", fixed = TRUE)
  expect_match(html, "comparison to run when the missing-at-random assumption is in doubt", fixed = TRUE)
  expect_match(html, "not a small-sample rule", fixed = TRUE)
  expect_match(html, "Rhemtulla, Brosseau-Liard and Savalei (2012)", fixed = TRUE)
  expect_match(html, "five or more categories and approximately symmetric thresholds", fixed = TRUE)
  expect_match(html, "up to seven categories under asymmetry", fixed = TRUE)
  expect_false(grepl("100 or fewer", html, fixed = TRUE))
})

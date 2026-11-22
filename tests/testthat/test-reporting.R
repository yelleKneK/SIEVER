# Reporting from recorded analyses, draft currency, and references from the
# methods used. The methods draft's currency compares every input the draft
# reads (the revision of the data, the ledger and the rows, the study
# estimator, the sampling design, the Reliability settings and the analysis
# registry); the readable report of saved analyses lists an absent
# identifier as absent and goes on; the reference lists are built from the
# methods used, not fixed text; and the draft's CFA and the assistant's
# pass the MLR memory refusal of the model pages.
#
# Fits are counted by mocking lavaan's lavaan() in its namespace and
# SIEVER's sieve_run_model(), as test-ondemand.R does.
#
# Author: Ken Kelley

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

rp_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}

# The bundled nine tests, confirmed continuous, with the variable roles
# declared and the study estimator following the item types (ML with FIML
# for continuous items).
rp_nine <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = TRUE, name = fx$name, configure = function(s) {
  s$roles <- sieve_declared_roles(fx$data, fx$item_cols)
  s$estimator <- "MLR"

  })
  s
}

# Notifications are kept, not shown.
rp_notes <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$notes <- character(0)
  testthat::local_mocked_bindings(
    sieve_notify = function(message, type = "message", duration = 8) {
      seen$notes <- c(seen$notes, message)
      invisible(NULL)
    },
    .env = env
  )
  seen
}

# Count every lavaan model fit and every SIEVER fit made while a test runs.
rp_count <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$lavaan <- 0L
  seen$runs <- 0L
  real_lavaan <- lavaan::lavaan
  testthat::local_mocked_bindings(
    lavaan = function(...) {
      seen$lavaan <- seen$lavaan + 1L
      real_lavaan(...)
    },
    .package = "lavaan", .env = env
  )
  real_run <- sieve_run_model
  testthat::local_mocked_bindings(
    sieve_run_model = function(...) {
      seen$runs <- seen$runs + 1L
      real_run(...)
    },
    .env = env
  )
  seen
}
rp_fits <- function(seen) c(lavaan = seen$lavaan, runs = seen$runs)

# A completed reliability bootstrap record for one construct, written into
# the registry by hand (the record's shape, not its computation, is what
# the draft's currency and the report read), with the specification the
# draft resolves for that construct: robust omega, the recommended interval,
# the study estimator's treatment of incomplete cases, at the given level.
rp_register_bootstrap <- function(state, construct, conf_level = .95, method = "percentile", missing = "fiml") {
  meta <- shiny::isolate(state$meta)
  its <- meta$item[meta$status == "active" & meta$construct == construct]
  sp <- sieve_reliability_spec(its, rep("continuous", length(its)),
    construct = construct, missing = missing, ci_method = "recommended",
    conf_level = conf_level, run_bootstrap = TRUE, group_id = construct,
    scope = "construct", parent_construct = construct, facet = NA_character_
  )
  om <- list(
    spec = sp, construct = construct, k = length(its), coefficient = sp$coefficient, label = sp$label,
    estimate = .81, se = .02, lower = .77, upper = .85, conf_level = conf_level,
    status = list(code = "ok", message = "Converged."),
    missing = list(requested = missing, used = missing, reason = NULL),
    n = list(rows = 301L, observed = 301L, empty = 0L, complete = 301L, patterns = 1L, used = 301L),
    warnings = character(0),
    uncertainty = list(
      status = "completed", method = method, label = if (identical(method, "dmar_bca")) "Bootstrap BCa (DMAR)" else "Bootstrap percentile",
      complete = TRUE, B = 200L, B_used = 200L, B_attempted = 200L, rng = list(seed = 7L)
    ),
    versions = sieve_reliability_versions()
  )
  shiny::isolate(sieve_register_reliability(
    state, stats::setNames(list(list(spec = sp, omega = om, alpha = NULL, H = NULL)), construct),
    note = "Requested reliability bootstrap intervals"
  ))
}

test_that("report selection follows stable record IDs from the list and catalog without fitting", {
  st <- rp_nine()
  first <- rp_register_bootstrap(st, "Visual")
  second <- rp_register_bootstrap(st, "Textual")
  original <- shiny::isolate(st$analyses)
  seen <- rp_count()
  shiny::testServer(mod_repro_server, args = list(state = st), {
    session$setInputs(saved_ids = c(first, second))
    expect_identical(report_ids(), c(first, second))
    session$setInputs(saved_ids = c(second, first))
    session$setInputs(analysis_catalog_rows_selected = c(1L, 2L))
    expect_identical(report_ids(), c(second, first))
    session$setInputs(analysis_catalog_rows_selected = 2L)
    expect_identical(report_ids(), second)
    expect_match(rp_text(output$report_selection_note), "1 analysis selected", fixed = TRUE)
    path <- output$dl_saved
    expect_identical(
      paste(readLines(path, warn = FALSE), collapse = "\n"),
      sieve_readable_saved_report(st, second)
    )
    # Catalog changes retain the selected identity, rather than a stale
    # position in a sorted or redrawn table.
    st$analysis_labels <- stats::setNames(list("Textual reliability"), second)
    session$flushReact()
    expect_identical(report_ids(), second)
    expect_identical(catalog()$id[[2L]], second)
    # DataTables sends NULL when the last selected row is cleared.
    session$setInputs(analysis_catalog_rows_selected = NULL)
    expect_length(report_ids(), 0L)
    expect_match(rp_text(output$report_selection_note), "Select at least one analysis", fixed = TRUE)
    expect_identical(rp_text(output$saved_download), "")
    expect_error(output$dl_saved, "Select at least one analysis", class = "shiny.silent.error")
    expect_identical(shiny::isolate(st$analyses), original)
    expect_identical(rp_fits(seen), c(lavaan = 0L, runs = 0L))
  })
})

test_that("a replacement project cannot inherit a selection through reused analysis IDs", {
  st <- rp_nine()
  analysis_id <- rp_register_bootstrap(st, "Visual")
  seen <- rp_count()
  shiny::testServer(mod_repro_server, args = list(state = st), {
    session$setInputs(saved_ids = analysis_id, rename_id = analysis_id)
    expect_identical(report_ids(), analysis_id)
    replacement <- shiny::isolate(st$analyses)
    replacement[[analysis_id]]$note <- "An unrelated analysis with the same local ID"
    st$analyses <- replacement
    st$dataset_id <- "D2"
    session$flushReact()
    expect_length(report_ids(), 0L)
    expect_identical(rename_target(), "")
    expect_identical(rp_text(output$saved_download), "")
    expect_identical(rp_text(output$saved_readable), "")
    session$setInputs(analysis_label = "An old click must not rename the new project", label_analysis = 1L)
    expect_null(shiny::isolate(st$analysis_labels)[[analysis_id]])
    expect_identical(rp_fits(seen), c(lavaan = 0L, runs = 0L))
  })
})

test_that("report provenance refreshes when respondent identities change without different item values", {
  st <- rp_nine()
  st$case_index <- seq_len(nrow(shiny::isolate(st$items)))
  analysis_id <- rp_register_bootstrap(st, "Visual")
  seen <- rp_count()
  shiny::testServer(mod_repro_server, args = list(state = st), {
    session$setInputs(saved_ids = analysis_id)
    expect_identical(catalog()$revision, "current")
    before <- rp_text(output$saved_readable)
    st$case_index <- c(2L, 1L, 3:301)
    session$flushReact()
    expect_identical(catalog()$revision, "earlier")
    expect_false(identical(rp_text(output$saved_readable), before))
    expect_match(rp_text(output$saved_readable), "Inputs: earlier", fixed = TRUE)
    expect_identical(rp_fits(seen), c(lavaan = 0L, runs = 0L))
  })
})

test_that("renaming uses its explicit target and does not change the report selection or results", {
  st <- rp_nine()
  first <- rp_register_bootstrap(st, "Visual")
  second <- rp_register_bootstrap(st, "Textual")
  original <- shiny::isolate(st$analyses)
  seen <- rp_count()
  shiny::testServer(mod_repro_server, args = list(state = st), {
    session$setInputs(saved_ids = c(first, second), rename_id = second)
    session$setInputs(analysis_label = "My Textual result", label_analysis = 1L)
    expect_identical(shiny::isolate(st$analysis_labels)[[second]], "My Textual result")
    expect_null(shiny::isolate(st$analysis_labels)[[first]])
    expect_identical(report_ids(), c(first, second))
    expect_match(rp_text(output$saved_readable), paste(second, "My Textual result"), fixed = TRUE)
    report <- sieve_readable_saved_report(st, second)
    expect_match(report, paste(second, "My Textual result"), fixed = TRUE)
    expect_match(report, paste("Recorded analysis:", original[[second]]$note), fixed = TRUE)
    expect_identical(shiny::isolate(st$analyses), original)
    expect_identical(rp_fits(seen), c(lavaan = 0L, runs = 0L))
  })
})

# The download handler itself is called with nothing selected: with records
# present but none chosen, and with no record at all, the button is absent,
# the handler stops with the instruction and writes no file, and nothing is
# fitted.
test_that("the report download called with an empty selection writes nothing and says what to select", {
  st <- rp_nine()
  analysis_id <- rp_register_bootstrap(st, "Visual")
  original <- shiny::isolate(st$analyses)
  seen <- rp_count()
  shiny::testServer(mod_repro_server, args = list(state = st), {
    session$flushReact()
    expect_length(report_ids(), 0L)
    expect_identical(rp_text(output$saved_download), "")
    expect_match(rp_text(output$report_selection_note), "Select at least one analysis to preview or download a report.", fixed = TRUE)
    expect_error(output$dl_saved, "Select at least one analysis to download a report.", class = "shiny.silent.error")
    # A selection of the list's empty value is the same empty selection.
    session$setInputs(saved_ids = character(0))
    expect_length(report_ids(), 0L)
    expect_error(output$dl_saved, "Select at least one analysis", class = "shiny.silent.error")
    # One selection makes the handler write the report.
    session$setInputs(saved_ids = analysis_id)
    expect_match(rp_text(output$saved_download), "Download report (.txt)", fixed = TRUE)
    expect_identical(
      paste(readLines(output$dl_saved, warn = FALSE), collapse = "\n"),
      sieve_readable_saved_report(st, analysis_id)
    )
    expect_identical(shiny::isolate(st$analyses), original)
  })
  empty <- rp_nine()
  shiny::testServer(mod_repro_server, args = list(state = empty), {
    session$flushReact()
    expect_identical(nrow(catalog()), 0L)
    expect_error(output$dl_saved, "Select at least one analysis", class = "shiny.silent.error")
  })
  expect_identical(rp_fits(seen), c(lavaan = 0L, runs = 0L))
})

test_that("a researcher label held as a named character vector is read like a list, and a missing name falls back to the record's note", {
  st <- rp_nine()
  first <- rp_register_bootstrap(st, "Visual")
  second <- rp_register_bootstrap(st, "Textual")
  original <- shiny::isolate(st$analyses)
  st$analysis_labels <- stats::setNames("My Visual result", first)
  cat_x <- sieve_analysis_catalog(st)
  expect_identical(cat_x$label[match(first, cat_x$id)], "My Visual result")
  expect_identical(cat_x$label[match(second, cat_x$id)], original[[second]]$note)
  lines <- shiny::isolate(sieve_record_lines(original[[second]], st))
  expect_identical(lines[[1L]], paste(second, original[[second]]$note))
  expect_false(any(startsWith(lines, "Recorded analysis:")))
})

# ---- 1. Draft currency over everything the draft reads --------------------

test_that("the draft is stale after each of the four kinds of change, current after a label edit, and the page's Update assembles it again", {
  seen <- rp_notes()
  st <- rp_nine()
  shiny::testServer(mod_repro_server, args = list(state = st), {
    session$setInputs(draft_go = 1)
    expect_null(draft_stale())
    expect_null(output$status)
    first <- draft()
    expect_match(first, "maximum likelihood (ML), with every item treated as continuous", fixed = TRUE) # Auto for continuous items

    # A label edit leaves the draft current.
    m <- shiny::isolate(state$meta)
    m$label[1] <- paste(m$label[1], "(renamed)")
    state$meta <- m
    session$flushReact()
    expect_null(draft_stale())
    expect_identical(output$draft, first)

    # 1. The study estimator.
    state$estimator_choice <- "ml"
    session$flushReact()
    expect_identical(
      draft_stale(),
      "the study estimator changed (following the item types then, ML on the complete cases now)"
    )
    line <- rp_text(output$status)
    expect_match(line, "The methods draft is from before.", fixed = TRUE)
    expect_match(line, "What changed: the study estimator changed", fixed = TRUE)
    expect_match(output$draft, "The draft assembled earlier is not shown: the study estimator changed", fixed = TRUE)
    expect_error(output$dl_draft, "Press Run a new analysis and draft first", fixed = TRUE)
    # The page guide carries the same sentence and offers the Update.
    guide <- sieve_page_status(state, "repro")
    expect_identical(guide$state, "stale")
    expect_true(guide$update)
    expect_match(guide$results[[1L]]$sentence, "the study estimator changed", fixed = TRUE)
    # The page's Update assembles the draft again under the new estimator.
    session$setInputs(update_go = 1)
    expect_null(draft_stale())
    expect_null(output$status)
    expect_match(draft(), "maximum likelihood (ML), with every item treated as continuous", fixed = TRUE)
    expect_false(identical(draft(), first))

    # 2. The sampling design.
    state$sampling_design <- list(unit = "clustered", cluster = "school")
    session$flushReact()
    expect_identical(draft_stale(), "the sampling design changed (independent rows then, clustered now)")
    state$sampling_design <- list(unit = "independent")
    session$flushReact()
    expect_null(draft_stale())

    # 3. A Reliability setting the draft follows.
    s <- shiny::isolate(state$reliability_settings)
    s <- if (is.list(s)) s else list()
    s$conf_level <- .9
    state$reliability_settings <- s
    session$flushReact()
    expect_identical(draft_stale(), "the Reliability tab's confidence level changed")
    session$setInputs(draft_go = 2)
    expect_null(draft_stale())
    expect_match(draft(), "at the 90% level", fixed = TRUE)
    s$treatment <- "ordered"
    state$reliability_settings <- s
    session$flushReact()
    expect_identical(draft_stale(), "the Reliability tab's item treatment changed")
    s$treatment <- "study"
    s$scope <- "facet"
    state$reliability_settings <- s
    session$flushReact()
    expect_identical(draft_stale(), "the Reliability tab's reporting scope changed")
    s$scope <- "construct"
    state$reliability_settings <- s
    session$flushReact()
    expect_null(draft_stale())

    # 4. A bootstrap interval completed since, for a construct the draft
    # reports: the draft is stale, the line names the construct and the
    # record, and the Update cites that record. The study estimator is ML
    # on the complete cases here, so the matching request is listwise.
    before <- draft()
    expect_match(before, "- Visual: Robust omega (observed denominator)", fixed = TRUE)
    expect_false(grepl("from the bootstrap run on the Reliability tab", before, fixed = TRUE))
    boot_id <- rp_register_bootstrap(state, "Visual", conf_level = .9, missing = "listwise")
    session$flushReact()
    expect_identical(draft_stale(), sprintf("a bootstrap interval for Visual was completed since (analysis %s)", boot_id))
    expect_match(rp_text(output$status), "a bootstrap interval for Visual was completed since", fixed = TRUE)
    session$setInputs(update_go = 2)
    expect_null(draft_stale())
    after <- draft()
    expect_match(after, sprintf("90%% CI [0.77, 0.85], Bootstrap percentile (200 of 200 replications used), from the bootstrap run on the Reliability tab on these data (analysis %s)", boot_id), fixed = TRUE)
    # The draft's references now include the bootstrap it cited, and only it.
    refs <- strsplit(after, "\n", fixed = TRUE)[[1]]
    refs <- refs[seq(match("## References for This Draft", refs), length(refs))]
    expect_true(any(grepl("Efron, B., & Tibshirani, R. J. (1993)", refs, fixed = TRUE)))
    expect_true(any(grepl("Hesterberg, T. C. (2015)", refs, fixed = TRUE)))
    expect_false(any(grepl("Efron, B. (1987)", refs, fixed = TRUE)))
    expect_false(any(grepl("Efron, B., & Tibshirani", strsplit(before, "\n", fixed = TRUE)[[1]], fixed = TRUE)))
  })
})

test_that("the draft's snapshot and its stale reason are plain functions of the state", {
  st <- rp_nine()
  made <- sieve_draft_inputs(st)
  expect_identical(made$estimator, "auto")
  expect_identical(made$sampling$unit, "independent")
  expect_identical(made$reliability, list(treatment = "study", scope = "construct", conf_level = .95))
  expect_identical(names(made$bootstraps), c("Visual", "Textual", "Speed"))
  expect_true(all(is.na(made$bootstraps)))
  expect_null(sieve_draft_stale_reason(made, sieve_draft_inputs(st)))
  expect_null(sieve_draft_stale_reason(NULL, sieve_draft_inputs(st)))
  # The revision still comes first: an excluded item is named as the ledger.
  m <- shiny::isolate(st$meta)
  m$status[9] <- "parked"
  shiny::isolate(st$meta <- m)
  expect_match(sieve_draft_stale_reason(made, sieve_draft_inputs(st)), "the item ledger has changed since", fixed = TRUE)
})

# ---- 2. The report from recorded analyses runs nothing -------------------

test_that("an uncertainty method a record does not hold is unrecorded rather than reconstructed from an estimator label", {
  test_frame <- environment()
  st <- rp_nine()
  run <- sieve_run_cfa(sieve_cfa_spec(shiny::isolate(st$meta)), shiny::isolate(st$items))
  id <- shiny::isolate(sieve_register_analysis(st, run))
  records <- shiny::isolate(st$analyses)
  records[[id]]$inference <- NULL
  st$analyses <- records
  seen <- rp_count(env = test_frame)
  text <- c(sieve_record_uncertainty_text(records[[id]]),
    sieve_analysis_catalog(st)$uncertainty, sieve_readable_saved_report(st, id))
  expect_true(all(grepl("methods were not recorded", text, fixed = TRUE)))
  expect_false(any(grepl("sandwich standard errors", text, fixed = TRUE)))
  expect_false(any(grepl("normal-theory standard errors", text, fixed = TRUE)))
  expect_identical(rp_fits(seen), c(lavaan = 0L, runs = 0L))
  derived <- list(kind = "latent_boundary", spec = list(parent_spec = list(estimator = "MLR")),
    estimates = list(method = list(method = "delta", label = "Delta method for the derived estimand")))
  expect_identical(sieve_record_uncertainty_text(derived), "Delta method for the derived estimand")
})

test_that("saved parameter comparisons show both interval limits at laptop width", {
  skip_if_no_browser()
  compat_with_app(width = 1280, height = 900, load_example = TRUE,
    libs = compat_source_libs(), function(app) {
      siever_open_tab(app, "cfa")
      # The first quick CFA runs automatically. A new estimator specifies
      # another model, which the page fits on request.
      expect_true(siever_wait_for(app,
        "document.querySelector('#cfa-method_line')?.textContent.includes('model-based standard errors') && document.querySelector('#cfa-method_line')?.textContent.includes(', current.')"))
      app$set_inputs(`cfa-estimator` = "ml")
      expect_true(siever_wait_for(app, "!!document.querySelector('#cfa-fit_go.shiny-bound-input')"))
      app$click("cfa-fit_go")
      siever_wait_render(app)
      expect_true(siever_wait_for(app,
        "document.querySelector('#cfa-method_line')?.textContent.includes('model-based') && document.querySelector('#cfa-method_line')?.textContent.includes(', current.')"))
      siever_open_tab(app, "repro")
      app$set_inputs(`repro-saved_ids` = c("A1", "A2"))
      siever_wait_render(app)
      expect_true(siever_wait_for(app,
        "document.querySelectorAll('#repro-saved_comparison .sieve-saved-compact').length === 2"))
      expect_true(app$get_js(paste0("(function(){const cards=Array.from(document.querySelectorAll('#repro-saved_comparison .sieve-saved-compact'));",
        "return cards.length===2&&cards.every(c=>{const t=c.querySelector('table');const h=t.querySelector('thead th:last-child');",
        "return h.textContent==='CI Upper'&&t.scrollWidth<=c.clientWidth+1&&h.getBoundingClientRect().right<=c.getBoundingClientRect().right+1;});})()")))
      expect_length(compat_output_errors(app), 0L)
    })
})

test_that("the readable report makes zero engine calls, lists an absent record as absent, and ends with the bibliography of exactly the methods and software of the selected records", {
  st <- rp_nine()
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  # One CFA recorded through the registry, as the CFA page records it.
  spec <- sieve_cfa_spec(meta, reserved = names(items), estimator = "auto")
  run <- sieve_run_cfa(spec, items)
  # Named cfa_id: inside testServer() the name id is the module's own.
  cfa_id <- shiny::isolate(sieve_register_analysis(st, run, note = "Fit the correlated-factors CFA (factor variance = 1)"))
  rec <- shiny::isolate(st$analyses)[[cfa_id]]
  # The record carries the methods its specification used, and the psych
  # version beside the others.
  expect_setequal(rec$methods, c("ml", "fiml", "fit_indices", "fit_indices_caution", "cfa"))
  expect_identical(rec$versions$psych, as.character(utils::packageVersion("psych")))

  seen <- rp_count()
  report <- sieve_readable_saved_report(st, c(cfa_id, "A99"))
  expect_identical(rp_fits(seen), c(lavaan = 0L, runs = 0L))
  lines <- strsplit(report, "\n", fixed = TRUE)[[1]]
  expect_identical(lines[1L], "SIEVER report from recorded analyses")
  # The record: specification, currency, actual sample, estimator and
  # missing data treatment, uncertainty method, software versions.
  expect_true(any(grepl(paste(cfa_id, "Fit the correlated-factors CFA"), lines, fixed = TRUE)))
  expect_true(any(grepl("Specification: kind cfa; 9 observed variables (", lines, fixed = TRUE)))
  expect_true(any(grepl("Latent variables: Visual, Textual, Speed.", lines, fixed = TRUE)))
  expect_true(any(grepl("Model syntax as fit:", lines, fixed = TRUE)))
  expect_true(any(grepl("Inputs: the data and the item pool now in the analysis (current).", lines, fixed = TRUE)))
  expect_true(any(grepl("Actual sample: input rows: 301; fitted rows: 301.", lines, fixed = TRUE)))
  # Auto is ML with FIML for continuous items: normal-theory standard errors.
  expect_true(any(grepl("Estimator: ML; missing data: FIML.", lines, fixed = TRUE)))
  expect_true(any(grepl("Uncertainty: model-based standard errors", lines, fixed = TRUE)))
  expect_true(any(grepl("se = standard", lines, fixed = TRUE)))
  expect_true(any(grepl(paste0("test = ", paste(rec$inference$test, collapse = ", ")), lines, fixed = TRUE)))
  expect_true(any(grepl(sprintf(
    "Software: SIEVER %s; DMAR %s; lavaan %s; psych %s; R %s.",
    rec$versions$SIEVER, rec$versions$DMAR, rec$versions$lavaan, rec$versions$psych, rec$versions$R
  ), lines, fixed = TRUE)))
  # The absent record is listed as absent and never refit.
  expect_true(any(grepl("A99 absent", lines, fixed = TRUE)))
  expect_true(any(grepl("Nothing was refit in its place.", lines, fixed = TRUE)))
  expect_false(grepl("Select existing", report, fixed = TRUE))
  # The bibliography: exactly the record's methods, deduplicated, then the
  # software with the recorded versions; nothing the record did not use.
  at <- match("References", lines)
  end <- match("Exact specifications and unrounded stored results", lines)
  expect_true(is.finite(at) && is.finite(end) && at < end)
  block <- lines[(at + 1L):(end - 1L)]
  entries <- block[nzchar(block) & !startsWith(block, "The methods")]
  expected <- sieve_method_bibliography(rec$methods, versions = rec$versions)
  expect_identical(entries, as.character(expected))
  expect_identical(entries, unique(entries))
  registry <- sieve_method_references()
  cited <- registry$citation[registry$key %in% rec$methods]
  expect_setequal(entries[seq_along(unique(cited))], sort(unique(cited), method = "radix"))
  expect_true(any(grepl(sprintf("(lavaan version %s)", rec$versions$lavaan), entries, fixed = TRUE)))
  expect_true(any(grepl(sprintf("SIEVER: Scale item evaluation and validation environment in R (Version %s)", rec$versions$SIEVER), entries, fixed = TRUE)))
  expect_true(any(grepl(sprintf("statistical computing (Version %s)", rec$versions$R), entries, fixed = TRUE)))
  expect_false(any(grepl("Pornprasertmanit", entries, fixed = TRUE)))
  expect_false(any(grepl("psych:", entries, fixed = TRUE)))
  expect_false(any(grepl("DMAR:", entries, fixed = TRUE)))

  # A reliability record with two groups, one with a completed percentile
  # bootstrap: the shared citation appears once, the bootstrap is cited
  # because it ran, DMAR joins the software, and still nothing is fit.
  boot_id <- rp_register_bootstrap(st, "Visual")
  rel <- shiny::isolate(st$analyses)[[boot_id]]
  expect_true(all(c("omega_robust", "omega_numerator", "fiml", "bootstrap_percentile", "bootstrap_guidance") %in% rel$methods))
  expect_true(any(grepl("Estimator: DMAR::reliability_omega; missing data: FIML.", strsplit(sieve_readable_saved_report(st, boot_id), "\n", fixed = TRUE)[[1]], fixed = TRUE)))
  report2 <- sieve_readable_saved_report(st, c(cfa_id, boot_id))
  expect_identical(rp_fits(seen), c(lavaan = 0L, runs = 0L))
  lines2 <- strsplit(report2, "\n", fixed = TRUE)[[1]]
  expect_true(any(grepl("Uncertainty: Visual: Bootstrap percentile (B = 200; seed 7; 200 of 200 replications used), completed.", lines2, fixed = TRUE)))
  at2 <- match("References", lines2)
  end2 <- match("Exact specifications and unrounded stored results", lines2)
  entries2 <- lines2[(at2 + 1L):(end2 - 1L)]
  entries2 <- entries2[nzchar(entries2) & !startsWith(entries2, "The methods")]
  expect_identical(entries2, unique(entries2))
  expect_identical(sum(grepl("Kelley, K., & Pornprasertmanit, S. (2016)", entries2, fixed = TRUE)), 1L)
  expect_identical(sum(grepl("Efron, B., & Tibshirani, R. J. (1993)", entries2, fixed = TRUE)), 1L)
  expect_identical(sum(grepl("DMAR: Design, measurement, and analysis in R (Version", entries2, fixed = TRUE)), 1L)
  expect_false(any(grepl("Efron, B. (1987)", entries2, fixed = TRUE)))
  expect_false(any(grepl("Hancock", entries2, fixed = TRUE)))

  # On the page: the same records, the absent one, the references, no fit.
  shiny::testServer(mod_repro_server, args = list(state = st), {
    session$setInputs(saved_ids = c(cfa_id, "A99", boot_id))
    html <- rp_text(output$saved_readable)
    expect_identical(rp_fits(seen), c(lavaan = 0L, runs = 0L))
    expect_match(html, "A99 absent", fixed = TRUE)
    expect_match(html, "Estimator: ML; missing data: FIML.", fixed = TRUE) # Auto for continuous items
    expect_match(html, "References of the selected analyses", fixed = TRUE)
    expect_match(html, "Kelley, K., &amp; Pornprasertmanit, S. (2016)", fixed = TRUE)
    path <- output$dl_saved
    expect_match(basename(path), "siever_report_recorded_analyses", fixed = TRUE)
    expect_identical(
      paste(readLines(path, warn = FALSE), collapse = "\n"),
      sieve_readable_saved_report(st, c(cfa_id, "A99", boot_id))
    )
    expect_identical(rp_fits(seen), c(lavaan = 0L, runs = 0L))
    # The catalog describes each record without refitting it.
    cat <- catalog()
    expect_identical(rp_fits(seen), c(lavaan = 0L, runs = 0L))
    expect_true(all(c("kind", "note", "revision", "changed", "rows", "fitted", "estimator", "missing", "uncertainty", "versions") %in% names(cat)))
    expect_identical(cat$revision, c("current", "current"))
    expect_identical(cat$changed, c("", ""))
    expect_identical(cat$fitted[1L], 301L)
    expect_identical(cat$missing, c("FIML", "FIML"))
    expect_match(cat$uncertainty[2L], "Bootstrap percentile (B = 200; seed 7", fixed = TRUE)
    expect_match(cat$versions[1L], sprintf("lavaan %s", rec$versions$lavaan), fixed = TRUE)
  })

  # A record of earlier inputs is labeled earlier, with what changed, and
  # is never read as current.
  sieve_apply_reverse(st, meta$item[2], origin = "test")
  cat <- sieve_analysis_catalog(st)
  expect_true(all(cat$revision == "earlier"))
  expect_true(all(grepl("the item data have changed since", cat$changed, fixed = TRUE)))
  later <- sieve_readable_saved_report(st, cfa_id)
  expect_match(later, "Inputs: earlier than those now in the analysis (the item data have changed since", fixed = TRUE)
  expect_match(later, "This is a record of that earlier analysis, not a result for the data now in the analysis.", fixed = TRUE)
  expect_identical(rp_fits(seen), c(lavaan = 0L, runs = 0L))
})

test_that("a failed record is reported as failed with nothing refit, and the exact appendix lists an absent identifier", {
  st <- rp_nine()
  shiny::isolate({
    st$analyses <- list(A1 = list(
      id = "A1", kind = "cfa", note = "A fit that failed",
      spec = list(kind = "cfa", key = "k1", estimator = "MLR", missing = "fiml", observed = c("a", "b", "c"), latent = "F"),
      revision = sieve_revision(st), sample_size = list(input = 301L, fitted = NA_integer_),
      estimates = NULL, status = list(code = "failed", message = "The engine stopped."),
      warnings = character(0), versions = list(SIEVER = "0.1.0", lavaan = "0.7.2", R = "4.5.1"),
      time = "2026-10-22T10:00:00Z"
    ))
  })
  seen <- rp_count()
  report <- sieve_readable_saved_report(st, c("A1", "A7"))
  expect_identical(rp_fits(seen), c(lavaan = 0L, runs = 0L))
  expect_match(report, "Status: failed (The engine stopped). No result was stored for it and nothing was refit.", fixed = TRUE)
  expect_match(report, "Results: none stored (see the status above); nothing was refit.", fixed = TRUE)
  expect_match(report, "A7 absent", fixed = TRUE)
  # A failed fit cites no fit indices: only its estimator family and FIML.
  expect_setequal(sieve_spec_methods(shiny::isolate(st$analyses)$A1$spec, NULL, fitted = FALSE), c("mlr", "fiml", "cfa"))
  expect_false(grepl("Hu, L.-T.", report, fixed = TRUE))
  expect_match(sieve_saved_analyses_report(st, "A7"), "A7 absent", fixed = TRUE)
})

# ---- 3. The MLR memory refusal reaches the draft and the assistant ------

test_that("the MLR guard is reached by the draft and by the assistant before any fit", {
  withr::local_options(sieve.memory_limit = 1)
  testthat::local_mocked_bindings(
    sieve_fit_cfa = function(...) stop("the draft must not fit a refused model"),
    sieve_run_cfa = function(...) stop("the assistant must not fit a refused model")
  )
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  items <- fx$data[, fx$item_cols, drop = FALSE]
  d <- sieve_methods_draft(items, meta, "the bundled example", estimator = "mlr")
  expect_match(d, "The correlated-factors CFA was not fit for this draft. Not fit:", fixed = TRUE)
  expect_match(d, "SIEVER allows", fixed = TRUE)
  expect_false(grepl("CFI = ", d, fixed = TRUE))
  # The unfit model's fit indices are not cited; the estimator still is.
  refs <- strsplit(d, "\n", fixed = TRUE)[[1]]
  refs <- refs[seq(match("## References for This Draft", refs), length(refs))]
  expect_false(any(grepl("Hu, L.-T.", refs, fixed = TRUE)))
  expect_true(any(grepl("Yuan, K.-H., & Bentler", refs, fixed = TRUE)))

  st <- rp_nine()
  plan <- shiny::isolate(sieve_assistant_plan(st, "Inspect structure", "cfa"))
  expect_error(shiny::isolate(sieve_assistant_execute(st, plan)), "Not fit:")
  expect_length(shiny::isolate(st$analyses), 0L)
})

# ---- 4. The registry ---------------------------------------------------------

test_that("the methods registry is verified, keyed once, and the bibliography excludes what is not verified", {
  refs <- sieve_method_references()
  expect_s3_class(refs, "data.frame")
  expect_identical(names(refs), c("key", "kind", "citation", "doi", "claim", "applicability", "software", "verified"))
  expect_false(anyDuplicated(refs$key) > 0L)
  expect_true(all(refs$kind %in% c("method", "software")))
  expect_true(all(nzchar(refs$claim)))
  # Every verified entry but the package itself and the one book chapter
  # (coefficient H: Hancock & Mueller, 2001, verified against the edited
  # volume; no DOI exists) carries its DOI or URL, and nothing
  # in the registry is unverified.
  expect_true(all(!is.na(refs$doi[refs$verified & !refs$key %in% c("SIEVER", "coefficient_h")])))
  expect_identical(refs$key[!refs$verified], character(0))
  bib <- sieve_method_bibliography(c("coefficient_h", "omega_robust"))
  expect_identical(attr(bib, "omitted"), character(0))
  expect_true(any(grepl("Hancock", bib, fixed = TRUE)))
  expect_true(any(grepl("Pornprasertmanit", bib, fixed = TRUE)))
  # Nothing is reported as not listed.
  lines <- sieve_reference_lines(c("coefficient_h", "omega_robust"), markdown = FALSE)
  expect_false(any(grepl("Not listed, because no verified bibliographic record exists", lines, fixed = TRUE)))
  # An unknown key cites nothing; software alone still lists R and SIEVER.
  expect_identical(length(sieve_method_bibliography("no_such_method")), 2L)
  expect_length(sieve_method_bibliography("no_such_method", software = FALSE), 0L)
  # No em dash anywhere in the registry, and page ranges take "--".
  expect_false(any(grepl("\u2014", unlist(refs[c("citation", "claim", "applicability")]), fixed = TRUE)))
  expect_false(any(grepl("[0-9]-[0-9]", sub(" https?://.*$", "", refs$citation[refs$kind == "method"]))))
})

test_that("the methods of a specification follow its kind and settings, and a pending bootstrap is not a method used", {
  expect_setequal(
    sieve_spec_methods(list(kind = "cfa", estimator = "WLSMV", missing = "listwise", ordered = c("a", "b")), list(fit = c(df = 24)), fitted = TRUE),
    c("wlsmv", "categorical_vs_continuous", "fit_indices", "fit_indices_caution", "cfa")
  )
  expect_true("rmsea_small_df" %in% sieve_spec_methods(list(kind = "cfa", estimator = "ML"), list(fit = c(df = 3)), fitted = TRUE))
  expect_setequal(
    sieve_spec_methods(list(kind = "invariance", estimator = "WLSMV", missing = "listwise", ordered = "a"), NULL, fitted = FALSE),
    c("wlsmv", "categorical_vs_continuous", "cfa", "invariance", "invariance_fit", "invariance_fit_rmsea", "invariance_ordered")
  )
  expect_setequal(sieve_spec_methods(list(kind = "irt", estimator = "WLSMV")), c("wlsmv", "categorical_vs_continuous", "irt_grm"))
  expect_setequal(sieve_spec_methods(list(kind = "cross_validation", estimator = "MLR", missing = "fiml")), c("mlr", "fiml", "cfa", "cross_validation"))
  expect_identical(sieve_spec_methods(list(kind = "alignment")), "alignment")
  expect_setequal(sieve_spec_methods(list(kind = "parallel_analysis", basis = "polychoric")), c("parallel_analysis", "parallel_analysis_threshold", "parallel_analysis_ordered"))
  expect_identical(sieve_spec_methods(list(kind = "map")), "map")
  expect_identical(sieve_spec_methods(list(kind = "vss")), "vss")
  expect_setequal(sieve_spec_methods(list(kind = "htmt", methods = "ave")), c("ave", "htmt"))
  expect_identical(sieve_spec_methods(list(kind = "marker")), "marker")
  pending <- list(kind = "reliability", groups = list(list(coefficient = "omega_robust", denominator = "observed", missing = "fiml", ci_method = "none")))
  expect_setequal(sieve_spec_methods(pending, NULL), c("omega_robust", "omega_numerator", "fiml"))
  done <- pending
  done$groups[[1L]]$ci_method <- "dmar_bca"
  done$groups[[1L]]$coefficient <- "omega_categorical"
  done$groups[[1L]]$missing <- "listwise"
  expect_setequal(sieve_spec_methods(done, NULL), c("omega_categorical", "omega_numerator", "bootstrap_bca", "bootstrap_guidance"))
  expect_identical(sieve_spec_methods(NULL), character(0))
  expect_identical(sieve_spec_methods(list(kind = "cfa", estimator = "MLR"), extra = c("cfa", "reporting")), c("cfa", "reporting", "mlr"))
})

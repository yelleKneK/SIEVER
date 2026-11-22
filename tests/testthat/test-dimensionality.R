# The Dimensionality tab and the CFA tab: the number of factors is decided
# from a control at the top of the tab and a rationale is optional;
# under a theoretical construct basis the tab says it is optional and the
# CFA tab waits for no decision; the CFA tab compares the one-factor, the
# correlated-factors, the higher-order and the bifactor structures on
# request in one family of fit indices with difference tests for the nested
# pairs only; a scree plot from the shared basis marks the Kaiser criterion
# at 1 as a reference; the parallel analysis figure tells its series apart
# by line type and point shape; and every figure has a Save button that
# writes a PNG at 300 dots per inch and a PDF at the screen geometry.
#
# The checks are independent of the code under test where a number is
# involved: eigenvalues against eigen(), fit values against direct lavaan
# fits, difference tests against lavaan::lavTestLRT(), file sizes against
# the PNG header.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("psych")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")
withr::local_options(sieve.cores = 2L) # at most two workers while the tests run

dim_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}
dim_html <- function(x) paste(as.character(unlist(x)), collapse = "")

# The bundled nine tests (three constructs, no facets), confirmed, under
# the construct basis asked for.
dim_nine <- function(confirmed = TRUE, basis = "theoretical", theory = TRUE) {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = confirmed, labels = TRUE,
    theory = FALSE, name = fx$name, configure = function(s) {
  s$roles <- sieve_declared_roles(fx$data, fx$item_cols)
  if (theory) {
    s$theory <- list(k = fx$theory_k, label = fx$theory_lbl, map = fx$theory_map)
  }
  s$construct_basis <- basis
  s$estimator <- "MLR"

  })
  s
}

# The same nine tests as one construct with three facets (what a
# one-construct study with facets looks like), for the structures.
dim_one_construct <- function() {
  s <- dim_nine()
  shiny::isolate({
    m <- s$meta
    m$facet <- m$construct
    m$construct <- "Ability"
    s$meta <- m
    s$theory <- NULL
    sieve_confirm_types(s)
  })
  s
}

dim_retention_inputs <- function(session, ...) {
  defaults <- list(
    cor_type = "auto", criterion = "p95", pa_reference = "auto",
    n_iter = 20, max_k = 4, seed = 7
  )
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}
dim_cfa_inputs <- function(session) {
  session$setInputs(
    structure = "first", ident = "varone", layout = "tree2",
    what = "std", min_path = 0, resid = TRUE, covs = TRUE
  )
}

# ---- 1. The decision: at the top, without a rationale -----------------------

test_that("the decision is recorded without a rationale from the control at the top of the tab, and the choice in force is repeated on every panel", {
  # Decision/plot/export mechanics use the explicitly reduced reference sample.
  siever_local_dmar_mechanics()
  st <- dim_nine()
  # The control precedes the panels in the page, and says the rationale is
  # optional.
  html <- as.character(mod_retention_ui("retention"))
  at_k <- regexpr('id="retention-decide_k"', html, fixed = TRUE)
  at_overview <- regexpr('id="retention-overview"', html, fixed = TRUE)
  at_tbl <- regexpr('id="retention-decide_tbl"', html, fixed = TRUE)
  expect_true(at_k > 0 && at_overview > 0 && at_k < at_overview)
  # Results first: the explanations (the overview) sit below the panels,
  # the decision records included.
  expect_true(at_tbl > 0 && at_tbl < at_overview)
  expect_match(html, "Rationale (optional)", fixed = TRUE)
  expect_false(grepl("Rationale (required)", html, fixed = TRUE))
  expect_match(html, "sieve-decide-k input.form-control { font-size: 1.75rem", fixed = TRUE)

  shiny::testServer(mod_retention_server, args = list(state = st), {
    dim_retention_inputs(session)
    # Beside the control: the heading, one line per criterion, the theorized
    # number, and that no decision is recorded yet.
    ev <- dim_text(output$decide_evidence)
    expect_match(ev, "parallel analysis: 3", fixed = TRUE)
    expect_false(grepl("MAP:", ev, fixed = TRUE))
    expect_match(ev, "VSS complexity 1:", fixed = TRUE)
    # The model fit by k is not a criterion.
    expect_false(grepl("sequential fit", ev, fixed = TRUE))
    expect_match(ev, "Theorized number of factors (a reference, not a criterion): 3.", fixed = TRUE)
    expect_match(ev, "No decision recorded yet.", fixed = TRUE)
    for (id in c("scree_note", "mapvss_note", "fit_note", "summary_reco")) {
      expect_match(dim_text(output[[id]]), "No decision recorded yet.", fixed = TRUE)
    }
    expect_identical(shiny::isolate(state$decisions), list())

    # No rationale: recorded, with the words "no rationale given".
    session$setInputs(decide_k = 3, decide_why = "", decide_go = 1)
    recs <- shiny::isolate(state$decisions)
    expect_named(recs, "D1")
    expect_identical(recs$D1$choice, 3L)
    expect_identical(recs$D1$rationale, "no rationale given")
    expect_identical(recs$D1$rationale, sieve_no_rationale())
    # The record says what the evidence table showed at the time.
    expect_identical(recs$D1$evidence, sieve_retention_evidence_lines(criteria(), 3L))
    expect_match(recs$D1$evidence[1], "^parallel analysis: 3 \\[")
    status <- dim_text(output$decide_status)
    expect_match(status,
      "Recorded as D1: 3 factors retained, no rationale given. Nothing else was changed.",
      fixed = TRUE
    )
    expect_match(status, "Decision in force (D1): 3 factor(s) retained. No rationale was given.", fixed = TRUE)
    # The recorded R carries the record with the words, as a comment block.
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("Decision D1 (factor_retention): 3", log, fixed = TRUE)))
    expect_true(any(grepl("# Rationale:  no rationale given", log, fixed = TRUE)))
    expect_true(all(startsWith(log[grepl("Rationale:", log, fixed = TRUE)], "#")))
    # The choice in force is on every panel and beside the control.
    for (id in c("scree_note", "mapvss_note", "fit_note", "summary_reco", "decide_evidence")) {
      expect_match(dim_text(output[[id]]), "Decision in force: 3 factors (D1). No rationale was given.", fixed = TRUE)
    }
    expect_identical(decision_k(), 3L)

    # With a rationale: a second record that names the first, and the
    # panels follow.
    session$setInputs(decide_k = 2, decide_why = "MAP suggests two.", decide_go = 2)
    recs <- shiny::isolate(state$decisions)
    expect_identical(names(recs), c("D1", "D2"))
    expect_identical(recs$D2$supersedes, "D1")
    expect_identical(recs$D2$rationale, "MAP suggests two.")
    expect_match(dim_text(output$decide_status),
      "Recorded as D2: 2 factors retained (replaces D1, which is kept in the record). Nothing else was changed.",
      fixed = TRUE
    )
    for (id in c("scree_note", "mapvss_note", "fit_note", "summary_reco", "decide_evidence")) {
      expect_match(dim_text(output[[id]]),
        "Decision in force: 2 factors (D2). Rationale: MAP suggests two.",
        fixed = TRUE
      )
    }
    expect_identical(decision_k(), 2L)
    # The records table lists both.
    shown <- dim_text(output$decide_tbl)
    expect_match(shown, "no rationale given", fixed = TRUE)
    expect_match(shown, "replaced by D2", fixed = TRUE)
  })
})

# ---- 2. Optional under a theoretical basis; the CFA tab waits for nothing ----

test_that("under a theoretical basis the tab opens with the optional sentence, the CFA tab fits without a decision, and the Ingest option names provisional assignments", {
  st <- dim_nine(basis = "theoretical")
  shiny::testServer(mod_retention_server, args = list(state = st), {
    dim_retention_inputs(session)
    note <- dim_text(output$optional_note)
    expect_match(note, "This tab is optional for a confirmatory study.", fixed = TRUE)
    expect_match(note,
      "the CFA tab is ready to fit the structure you specified; nothing on the CFA tab waits for a decision made here",
      fixed = TRUE
    )
    expect_match(dim_html(output$optional_note), 'class="small text-muted sieve-optional-tab mb-2"', fixed = TRUE)
    expect_null(output$gate)
    # The sentence is there before confirmation as well.
    state$confirmed <- FALSE
    session$flushReact()
    expect_match(dim_text(output$optional_note), "This tab is optional for a confirmatory study.", fixed = TRUE)
    expect_match(dim_text(output$gate), "Item types await confirmation", fixed = TRUE)
  })
  # Under an empirical basis the tab is a step, and the sentence is absent.
  st_emp <- dim_nine(basis = "empirical")
  shiny::testServer(mod_retention_server, args = list(state = st_emp), {
    dim_retention_inputs(session)
    expect_null(output$optional_note)
    expect_null(output$gate)
  })
  # Results first: the sentence does not precede the gate (the status line
  # when the page is blocked) but sits beside the decision control it
  # concerns, before the number of factors.
  html <- as.character(mod_retention_ui("retention"))
  at_note <- regexpr('id="retention-optional_note"', html, fixed = TRUE)
  at_gate <- regexpr('id="retention-gate"', html, fixed = TRUE)
  at_k <- regexpr('id="retention-decide_k"', html, fixed = TRUE)
  expect_true(at_note > 0 && at_gate < at_note && at_note < at_k)

  # The CFA tab: no decision recorded, the model fits from its button, and
  # neither its gate nor its model note mentions a decision.
  st <- dim_nine(basis = "theoretical")
  expect_identical(shiny::isolate(st$decisions), list())
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    dim_cfa_inputs(session)
    gate <- dim_text(output$gate)
    expect_match(gate, "The model is not fit yet.", fixed = TRUE)
    expect_false(grepl("decision", gate, ignore.case = TRUE))
    expect_false(grepl("Dimensionality", gate, fixed = TRUE))
    session$setInputs(fit_go = 1)
    expect_s4_class(fit(), "lavaan")
    expect_identical(cfa_run()$status$code, "ok")
    expect_false(grepl("decision", dim_text(output$model_note), ignore.case = TRUE))
  })
  expect_identical(shiny::isolate(st$decisions), list())
  # The shared gate of the analysis tabs does not read the decision records
  # (checked in the source, where there is one).
  root <- compat_source_root()
  if (!is.null(root)) {
    src <- paste(readLines(file.path(root, "R", "utils_state.R")), collapse = "\n")
    gate_fun <- regmatches(src, regexpr("sieve_gate <- function\\(state\\) \\{.*?\\n\\}", src))
    expect_length(gate_fun, 1L)
    expect_false(grepl("decision", gate_fun, fixed = TRUE))
  }

  # The Ingest option names provisional assignments, without implying EFA drives them.
  html <- as.character(mod_ingest_ui("ingest"))
  expect_match(html, "Empirical (provisional assignments)", fixed = TRUE)
  expect_false(grepl("EFA driven", html, fixed = TRUE))
})

# ---- 3. Compare structures on the CFA tab -----------------------------------

# Count every SIEVER fit made while a test runs, and keep the notifications.
dim_count <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$runs <- 0L
  seen$notes <- character(0)
  real_run <- sieve_run_model
  testthat::local_mocked_bindings(
    sieve_run_model = function(...) {
      seen$runs <- seen$runs + 1L
      real_run(...)
    },
    sieve_notify = function(message, type = "message", duration = 8) {
      seen$notes <- c(seen$notes, message)
      invisible(NULL)
    },
    .env = env
  )
  seen
}

# The four structures of the nine tests written by hand, fit directly.
dim_direct_nine <- function(items) {
  v <- "t1_visual_perception + t2_cubes + t4_lozenges"
  t <- "t6_paragraph_comprehension + t7_sentence + t9_word_meaning"
  s <- "t10_addition + t12_counting_groups_of_dots + t13_straight_and_curved_capitals"
  groups <- sprintf("Visual =~ %s\nTextual =~ %s\nSpeed =~ %s", v, t, s)
  # lavaan warns that the bifactor solution of the nine tests has a negative
  # residual variance; the package keeps that condition and reports the
  # solution as improper (asserted below), so the direct fit is quiet here.
  fit <- function(model, orthogonal = FALSE) {
    suppressWarnings(lavaan::cfa(model,
      data = items, estimator = "MLR", std.lv = TRUE, # these tests choose MLR (its scaled and robust families)
      missing = "fiml", orthogonal = orthogonal
    ))
  }
  list(
    one_factor = fit(sprintf("G =~ %s + %s + %s", v, t, s)),
    correlated = fit(groups),
    higher_order = fit(paste0(groups, "\nG =~ Visual + Textual + Speed")),
    bifactor = fit(sprintf("G =~ %s + %s + %s\n%s", v, t, s, groups), orthogonal = TRUE)
  )
}

test_that("the Compare structures panel fits the four structures on request, reads their fit in one family equal to direct lavaan fits, and tests the nested pairs only", {
  seen <- dim_count()
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  st <- dim_nine()
  # The subject is MLR's scaled and robust families; Auto is ML with FIML,
  # so the study chooses MLR.
  st$estimator_choice <- "mlr"
  html <- gsub("\\s+", " ", as.character(mod_cfa_ui("cfa")))
  expect_match(html, "Compare structures", fixed = TRUE)
  expect_match(html, 'id="cfa-go_efa"', fixed = TRUE)
  expect_match(html, "needs an assignment of items to factors", fixed = TRUE)
  expect_match(html, "the correlated model and the bifactor model are not nested", fixed = TRUE)
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    dim_cfa_inputs(session)
    # Nothing is fit on view: the line states the estimate with the button,
    # the tables say what to press, and the note names the groups.
    line <- dim_text(output$compare_line)
    expect_match(line, "The comparison of structures is not fit yet.", fixed = TRUE)
    expect_match(line,
      "Fitting the 4 structures (one factor, correlated constructs, higher order, bifactor) of 9 items under MLR is estimated to take",
      fixed = TRUE
    )
    expect_match(dim_html(output$compare_line), '-compare_go"', fixed = TRUE)
    expect_match(dim_text(output$compare_tbl), "Press Fit the structures", fixed = TRUE)
    expect_match(dim_text(output$compare_tests), "read with the structures", fixed = TRUE)
    expect_match(dim_text(output$compare_note),
      "The first-order groups are the 3 constructs of the included items: Visual (3 items), Textual (3 items), Speed (3 items).",
      fixed = TRUE
    )
    expect_error(output$compare_plot, class = "shiny.silent.error")
    expect_null(compare_result())
    expect_identical(seen$runs, 0L)
    expect_identical(compare$status()$state, "not_run")

    # Run: four SIEVER fits, one notification, the result current.
    session$setInputs(compare_go = 1)
    expect_identical(seen$runs, 4L)
    expect_match(
      seen$notes[length(seen$notes)],
      "^Compare structures: MLR, 9 items, 4 models finished in [0-9.]+ seconds\\.$"
    )
    expect_identical(compare$status()$state, "current")
    res <- compare_result()
    expect_s3_class(res, "sieve_structures")
    expect_identical(res$fitted, c("one_factor", "correlated", "higher_order", "bifactor"))
    expect_identical(res$groups$level, "construct")
    expect_identical(res$groups$groups, c("Visual", "Textual", "Speed"))
    for (st_ in res$structures) {
      expect_s3_class(st_$run, "sieve_result")
      expect_s3_class(st_$spec, "sieve_spec")
    }
    expect_identical(res$structures$bifactor$spec$orthogonal, TRUE)
    expect_identical(res$structures$correlated$spec$orthogonal, FALSE)

    # One family for every structure, equal to direct lavaan fits of the
    # models written by hand.
    tab <- compare_table()
    expect_identical(attr(tab, "family"), "robust")
    expect_identical(attr(tab, "test_family"), "scaled")
    expect_identical(tab$structure, c("One factor", "Correlated constructs", "Higher order", "Bifactor"))
    expect_identical(tab$status, c("ok", "ok", "ok", "inadmissible"))
    direct <- dim_direct_nine(shiny::isolate(state$items))
    want <- c(
      chisq = "chisq.scaled", df = "df.scaled", pvalue = "pvalue.scaled", cfi = "cfi.robust",
      tli = "tli.robust", rmsea = "rmsea.robust", rmsea_lo = "rmsea.ci.lower.robust",
      rmsea_hi = "rmsea.ci.upper.robust", srmr = "srmr_bentler_nomean"
    )
    # (The RMSEA interval is at the RMSEA level, 90%; lavaan is asked for
    # the table's level.)
    expect_identical(attr(tab, "conf_level"), 0.90)
    for (i in seq_along(direct)) {
      fm <- lavaan::fitMeasures(direct[[i]], unname(want), fm.args = list(rmsea.ci.level = attr(tab, "conf_level")))
      for (col in names(want)) {
        expect_equal(tab[[col]][i], unname(fm[[want[[col]]]]),
          tolerance = 1e-8,
          info = paste(names(direct)[i], col)
        )
      }
      expect_equal(tab$npar[i], unname(lavaan::fitMeasures(direct[[i]], "npar")), tolerance = 1e-12)
    }
    # The bifactor solution of the nine tests is improper, and the row says so.
    expect_match(tab$note[4], "Improper solution", fixed = TRUE)

    # The nested pairs only: five pairs with a test defined, one of them
    # with no degrees of freedom to test, and the pair that is not nested.
    t <- res$tests
    expect_identical(nrow(t), 6L)
    expect_identical(t$nested, c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE))
    expect_identical(t$available, c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE))
    expect_match(t$reason[2], "The added constraints cost no degrees of freedom", fixed = TRUE)
    expect_identical(t$df[2], 0)
    expect_match(t$reason[6], "not nested", fixed = TRUE)
    expect_true(all(is.na(t$statistic[c(2, 6)])))
    # The group loadings of the bifactor model fixed at zero are a boundary
    # constraint too, and the improper bifactor solution is named in the
    # tests that involve it.
    expect_identical(t$boundary, c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE))
    expect_true(all(grepl("boundary of the parameter space", t$note[t$boundary & t$available], fixed = TRUE)))
    expect_true(all(grepl("The Bifactor solution is improper", t$note[3:4], fixed = TRUE)))
    for (i in which(t$available)) {
      got <- lavaan::lavTestLRT(direct[[t$less[i]]], direct[[t$more[i]]])
      expect_equal(t$statistic[i], unname(got[2, "Chisq diff"]), tolerance = 1e-8, info = i)
      expect_equal(t$df[i], unname(got[2, "Df diff"]), tolerance = 1e-12, info = i)
      expect_equal(t$p[i], unname(got[2, "Pr(>Chisq)"]), tolerance = 1e-8, info = i)
      expect_match(t$label[i], "scaled chi square difference test", fixed = TRUE)
    }
    # The panels render from the result, and the note names the family and
    # the pair that is not nested.
    expect_no_error(output$compare_tbl)
    expect_no_error(output$compare_tests)
    expect_no_error(output$compare_plot)
    note <- dim_text(output$compare_note)
    expect_match(note, "CFI and RMSEA are in the robust family for every model", fixed = TRUE)
    expect_match(note, "Not nested, so no difference test: Correlated constructs and Bifactor.", fixed = TRUE)
    shown <- dim_text(output$compare_tbl)
    expect_match(shown, "CFI (robust)", fixed = TRUE)
    expect_match(shown, "Chi square (scaled)", fixed = TRUE)
    expect_match(dim_text(output$compare_tests), "the second is the first with constraints added", fixed = TRUE)

    # The recorded R: one block per structure from its own specification,
    # the list of fits, the measures in the family shown, and lavaan's
    # test of each nested pair that has one.
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("fits_structures <- list(one_factor = fit_one_factor, correlated = fit_correlated, higher_order = fit_higher_order, bifactor = fit_bifactor)",
      log,
      fixed = TRUE
    )))
    expect_identical(sum(grepl("lavaan::lavTestLRT(fits_structures", log, fixed = TRUE)), 4L)
    expect_true(any(grepl("fit_bifactor <- lavaan::cfa(model_bifactor, data =", log, fixed = TRUE)))
    expect_true(any(grepl("# Correlated constructs and Bifactor are not nested: no difference test.",
      log,
      fixed = TRUE
    )))
    # Every fit is in the analysis registry.
    kinds <- vapply(shiny::isolate(state$analyses), function(a) a$spec$kind %||% "", character(1))
    expect_true(all(c(
      "structure_one_factor",
      "structure_correlated",
      "structure_higher_order",
      "structure_bifactor"
    ) %in% kinds))

    # A label edit keeps the result; Run again reuses it; a reverse toggle
    # marks it stale with the sentence, and the result stays on screen.
    m <- shiny::isolate(state$meta)
    m$label[1] <- "Renamed"
    state$meta <- m
    session$flushReact()
    expect_identical(compare$status()$state, "current")
    expect_identical(seen$runs, 4L)
    session$setInputs(compare_go = 2)
    expect_identical(seen$runs, 4L)
    expect_match(
      seen$notes[length(seen$notes)],
      "^Nothing analytic has changed since Compare structures: MLR, 9 items, 4 models ran at"
    )
    it <- shiny::isolate(state$meta$item[2])
    sieve_apply_reverse(state, it, origin = "test")
    sieve_confirm_types(state)
    session$flushReact()
    expect_identical(compare$status()$state, "stale")
    expect_identical(compare$status()$sentence, sprintf("%s reverse scored", it))
    expect_match(dim_text(output$compare_line), "Refit the structures", fixed = TRUE)
    expect_s3_class(compare_result(), "sieve_structures")
    expect_no_error(output$compare_tbl)
  })
})

test_that("the structures follow the ledger: the facets of one construct are the groups, two groups give two structures, and the recorded R reproduces the table", {
  d <- sieve_hs9_fixture()$data[, sieve_hs9_fixture()$item_cols, drop = FALSE]
  # One construct split into three facets: the facets are the groups, shown
  # under their own names.
  st <- dim_one_construct()
  st$estimator_choice <- "mlr" # MLR's families are the subject (Auto is ML with FIML)
  res <- sieve_compare_structures(shiny::isolate(st$meta), d, estimator = "mlr")
  expect_identical(res$groups$level, "facet")
  expect_identical(res$groups$scale, "Ability")
  expect_identical(res$groups$groups, c("Visual", "Textual", "Speed"))
  expect_identical(res$fitted, c("one_factor", "correlated", "higher_order", "bifactor"))
  expect_identical(res$structures$correlated$label, "Correlated facets")
  expect_identical(
    unname(res$structures$higher_order$spec$name_map),
    c("Visual", "Textual", "Speed", "Ability")
  )
  expect_identical(unname(res$structures$one_factor$spec$name_map), "Ability")
  expect_identical(unname(res$structures$bifactor$spec$name_map), c("general", "Visual", "Textual", "Speed"))
  tab <- sieve_structures_table(res)
  direct <- dim_direct_nine(d)
  expect_equal(tab$cfi, unname(vapply(direct, function(f) lavaan::fitMeasures(f, "cfi.robust"), numeric(1))),
    tolerance = 1e-8
  )
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    dim_cfa_inputs(session)
    expect_match(dim_text(output$compare_line),
      "Fitting the 4 structures (one factor, correlated facets, higher order, bifactor) of 9 items under MLR",
      fixed = TRUE
    )
    expect_match(dim_text(output$compare_note),
      "The first-order groups are the 3 facets of the included items",
      fixed = TRUE
    )
  })
  # An item of the one construct with no facet is left out of the grouped
  # structures and named.
  st2 <- dim_one_construct()
  m2 <- shiny::isolate(st2$meta)
  m2$facet[1] <- NA_character_
  st2$meta <- m2
  g <- sieve_structure_groups(m2)
  expect_identical(g$ungrouped, m2$item[1])
  expect_identical(unname(g$sizes), c(2L, 3L, 3L))
  plans <- sieve_structure_plans(m2)
  expect_false(m2$item[1] %in% plans$structures$correlated$plan$observed)
  expect_true(m2$item[1] %in% plans$structures$one_factor$plan$observed)
  shiny::testServer(mod_cfa_server, args = list(state = st2), {
    dim_cfa_inputs(session)
    expect_match(dim_text(output$compare_note),
      sprintf("1 item with no facet is left out of the grouped structures (%s)", m2$item[1]),
      fixed = TRUE
    )
  })

  # Two constructs: the one-factor and the correlated model only, the other
  # two named with the reason.
  st3 <- dim_nine()
  m3 <- shiny::isolate(st3$meta)
  m3$status[m3$construct == "Speed"] <- "parked"
  res3 <- sieve_compare_structures(m3, d)
  expect_identical(res3$fitted, c("one_factor", "correlated"))
  expect_match(res3$structures$higher_order$reason,
    "the ledger has 2 constructs and this structure needs at least 3",
    fixed = TRUE
  )
  # The bifactor model's reason is the Bifactor tab's rule.
  expect_match(res3$structures$bifactor$reason, "needs at least three group factors", fixed = TRUE)
  tab3 <- sieve_structures_table(res3)
  expect_identical(tab3$status, c("ok", "ok", "not fit", "not fit"))
  expect_match(tab3$note[3], "Not fit: the ledger has 2 constructs", fixed = TRUE)
  expect_identical(sum(res3$tests$available), 1L)
  expect_identical(res3$tests$less[res3$tests$available], "correlated")
  code3 <- sieve_structures_code(res3)
  expect_true(any(grepl("# Higher order: not fit (the ledger has 2 constructs", code3, fixed = TRUE)))
  expect_identical(sum(grepl("lavaan::lavTestLRT", code3, fixed = TRUE)), 1L)

  # Nothing to build: no construct assigned.
  m4 <- shiny::isolate(dim_nine()$meta)
  m4$construct <- NA_character_
  res4 <- sieve_compare_structures(m4, d)
  expect_identical(res4$fitted, character(0))
  expect_null(res4$compare)
  expect_true(all(!res4$tests$available))
  expect_identical(nrow(sieve_structures_table(res4)), 4L)

  # The recorded R, evaluated in a fresh environment that holds only the
  # item data, reproduces the table's values.
  res <- sieve_compare_structures(shiny::isolate(dim_nine()$meta), d, estimator = "mlr")
  env <- new.env(parent = globalenv())
  env$items <- d
  # (The replayed bifactor fit raises lavaan's negative variance warning, as
  # the direct fit above does.)
  suppressWarnings(utils::capture.output(eval(parse(text = sieve_structures_code(res)), envir = env)))
  expect_identical(names(env$fits_structures), res$fitted)
  tab <- sieve_structures_table(res)
  for (i in seq_along(res$fitted)) {
    fm <- lavaan::fitMeasures(env$fits_structures[[i]], c("cfi.robust", "rmsea.robust", "chisq.scaled"))
    expect_equal(tab$cfi[i], unname(fm[["cfi.robust"]]), tolerance = 1e-10)
    expect_equal(tab$rmsea[i], unname(fm[["rmsea.robust"]]), tolerance = 1e-10)
    expect_equal(tab$chisq[i], unname(fm[["chisq.scaled"]]), tolerance = 1e-10)
  }
  # The figure: one point per structure with indices, RMSEA with its
  # interval, in the family shown.
  gg <- sieve_structures_plot(tab)
  expect_s3_class(gg, "ggplot")
  built <- ggplot2::ggplot_build(gg)
  expect_identical(nrow(built$data[[2]]), 8L)
  expect_null(sieve_structures_plot(sieve_structures_table(res4)))
})

# ---- 4. The scree plot from the shared basis --------------------------------

test_that("the scree plot draws the eigenvalues of the basis, equal to eigen(), with the Kaiser criterion at 1 as a reference, before any parallel analysis has run", {
  # Decision/plot/export mechanics use the explicitly reduced reference sample.
  siever_local_dmar_mechanics()
  st <- dim_nine()
  html <- as.character(mod_retention_ui("retention"))
  at_scree <- regexpr('id="retention-scree_plot"', html, fixed = TRUE)
  at_pa <- regexpr('id="retention-scree"', html, fixed = TRUE)
  expect_true(at_scree > 0 && at_scree < at_pa)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    dim_retention_inputs(session)
    expect_no_error(output$scree_plot)
    note <- dim_text(output$scree_plot_note)
    v <- scree_vals()
    expect_true(v$ok)
    R <- basis()$R
    expect_equal(v$table$unreduced, eigen(R, symmetric = TRUE, only.values = TRUE)$values, tolerance = 1e-12)
    # The reduction is stats::factanal's, the fixed reduction of
    # DMAR::parallel_analysis(); psych's ML reduction stops about 1e-5 away
    # on the same flat optimum (1.3e-5 on these tests), so the reference
    # here is factanal's.
    fa1 <- stats::factanal(covmat = R, factors = 1, rotation = "none")
    reduced <- R
    diag(reduced) <- 1 - fa1$uniquenesses
    expect_equal(v$table$reduced, eigen(reduced, symmetric = TRUE, only.values = TRUE)$values, tolerance = 1e-12)
    expect_lt(max(abs(v$table$reduced -
      psych::fa(R, nfactors = 1, rotate = "none", fm = "ml", warnings = FALSE)$values)), 5e-5)
    expect_identical(v$kaiser, sum(eigen(R, only.values = TRUE)$values > 1))
    expect_identical(v$kaiser, 3L)
    expect_match(note, "Kaiser criterion (eigenvalue 1): 3 eigenvalues of the unreduced matrix exceed 1.", fixed = TRUE)
    expect_match(note, "The line at 1 is a reference, not a rule.", fixed = TRUE)
    expect_match(note,
      "Circles, solid line: the eigenvalues of the matrix as it stands (unreduced). Triangles, dashed line: the factor-analytic eigenvalues",
      fixed = TRUE
    )
    expect_match(note, "No decision recorded yet.", fixed = TRUE)
    # The values are kept: a label edit and a display control compute nothing new.
    v1 <- scree_vals()
    m <- shiny::isolate(state$meta)
    m$label[1] <- "Renamed"
    state$meta <- m
    session$flushReact()
    session$setInputs(criterion = "mean")
    expect_identical(scree_vals(), v1)
    # The recorded R replays both series from R_items.
    log <- shiny::isolate(state$codelog)
    expect_identical(
      sum(grepl("scree_unreduced <- eigen(R_items, symmetric = TRUE, only.values = TRUE)$values",
        log,
        fixed = TRUE
      )),
      1L
    )
    env <- new.env(parent = globalenv())
    env$R_items <- R
    grDevices::pdf(NULL)
    eval(parse(text = sieve_scree_code(v)), envir = env)
    grDevices::dev.off()
    expect_equal(env$scree_unreduced, v$table$unreduced, tolerance = 1e-12)
    expect_equal(env$scree_reduced, v$table$reduced, tolerance = 1e-12)
    expect_identical(env$kaiser_count, 3L)
    # After a decision the plot carries the decision line and the note the choice.
    session$setInputs(decide_k = 3, decide_why = "", decide_go = 1)
    expect_match(dim_text(output$scree_plot_note), "Decision in force: 3 factors (D1).", fixed = TRUE)
    gg <- sieve_scree_plot(v, theory_k = theory_k(), decision_k = decision_k())
    labels <- unlist(lapply(gg$layers, function(l) l$aes_params$label))
    expect_true(all(c("Kaiser criterion (eigenvalue 1)", "theory k", "decision k") %in% labels))
  })

  # The plot and its note read the basis alone: with the parallel analysis
  # failing they are still drawn, while the parallel analysis figure is not.
  local_mocked_bindings(sieve_parallel_analysis = function(...) stop("forced: no parallel analysis"))
  st <- dim_nine()
  shiny::testServer(mod_retention_server, args = list(state = st), {
    dim_retention_inputs(session)
    expect_no_error(output$scree_plot)
    expect_match(dim_text(output$scree_plot_note), "Kaiser criterion (eigenvalue 1): 3 eigenvalues", fixed = TRUE)
    expect_error(output$scree, "forced: no parallel analysis")
  })

  # The figure: two series told apart by line type and point shape as well
  # as color, with a legend, and the reference line at 1.
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols, drop = FALSE]
  b <- sieve_retention_cor(d, rep("continuous", 9L), "pearson")
  v <- sieve_scree_values(b)
  gg <- sieve_scree_plot(v)
  expect_s3_class(gg, "ggplot")
  aes_of <- vapply(gg$scales$scales, function(s) s$aesthetics[1], character(1))
  expect_true(all(c("colour", "linetype", "shape") %in% aes_of))
  built <- ggplot2::ggplot_build(gg)
  lines <- built$data[[3]]
  points <- built$data[[4]]
  expect_identical(sort(unique(as.character(lines$linetype))), c("longdash", "solid"))
  expect_identical(sort(unique(points$shape)), c(16, 17))
  expect_identical(length(unique(lines$colour)), 2L)
  expect_identical(built$data[[1]]$yintercept, 1)
  expect_identical(gg$theme$legend.position, "top")
  expect_match(gg$labels$subtitle, "Kaiser criterion (eigenvalue 1): a reference, not a rule.", fixed = TRUE)
  # Without a matrix there is nothing to draw, and the record says so.
  none <- sieve_scree_values(list(ok = FALSE, reason = "no matrix for the test", used = "pearson", n = 0L, p = 0L))
  expect_false(none$ok)
  expect_null(sieve_scree_plot(none))
  expect_match(sieve_scree_code(none)[1], "no matrix for the test", fixed = TRUE)
})

test_that("the rank slider zooms without refitting the fixed one-factor reduction", {
  st <- dim_nine()
  shiny::testServer(mod_retention_server, args = list(state = st), {
    dim_retention_inputs(session)
    control <- dim_text(output$scree_control)
    expect_match(control, "Eigenvalue ranks shown", fixed = TRUE)
    expect_false(grepl("Factors extracted for the reduced series", control, fixed = TRUE))
    expect_match(control, "fixed reduction matches the parallel analysis", fixed = TRUE)
    expect_match(dim_html(output$scree_control), 'data-from="1"', fixed = TRUE)
    expect_match(dim_html(output$scree_control), 'data-to="9"', fixed = TRUE)
    first <- scree_vals()
    expect_identical(first$nfactors, 1L)
    before <- length(unlist(shiny::isolate(state$codelog)))
    session$setInputs(scree_ranks = c(1, 6), scree_nfactors = 5)
    session$elapse(600)
    expect_identical(scree_vals(), first)
    expect_length(unlist(shiny::isolate(state$codelog)), before)
    expect_no_error(output$scree_plot)
    expect_match(dim_text(output$scree_plot_note), "Ranks 1 to 6 of 9 are shown", fixed = TRUE)
    log <- paste(unlist(shiny::isolate(state$codelog)), collapse = "\n")
    expect_match(log, "factors = 1, rotation", fixed = TRUE)
    expect_false(grepl("factors = 5", log, fixed = TRUE))
    expect_match(dim_text(output$pa_window), "Eigenvalue ranks shown", fixed = TRUE)
    sieve_new_dataset(state)
    sieve_confirm_types(state)
    session$flushReact()
    expect_identical(scree_vals()$nfactors, 1L)
    expect_match(dim_html(output$scree_control), 'data-to="9"', fixed = TRUE)
  })
})

# ---- 5. The parallel analysis figure: line types and shapes, with a legend --

test_that("the parallel analysis figure tells the empirical and the reference eigenvalues apart by line type and point shape with a legend, and is drawn from the series the count was read from", {
  # Decision/plot/export mechanics use the explicitly reduced reference sample.
  siever_local_dmar_mechanics()
  emp <- c(2.7, 0.9, 0.45, 0.01, -0.02)
  ref <- c(0.6, 0.4, 0.25, 0.1, 0.0)
  gg <- sieve_pa_plot(emp, ref, "95th percentile of 100 simulated normal datasets", "polychoric",
    theory_k = 2L, decision_k = 1L
  )
  expect_s3_class(gg, "ggplot")
  aes_of <- vapply(gg$scales$scales, function(s) s$aesthetics[1], character(1))
  expect_true(all(c("colour", "linetype", "shape") %in% aes_of))
  built <- ggplot2::ggplot_build(gg)
  lines <- built$data[[2]]
  points <- built$data[[3]]
  expect_identical(sort(unique(as.character(lines$linetype))), c("longdash", "solid"))
  expect_identical(sort(unique(points$shape)), c(16, 17))
  expect_identical(length(unique(lines$colour)), 2L)
  expect_identical(gg$theme$legend.position, "top")
  # The two series carry their names in the legend, and the values are the
  # ones given.
  series <- levels(gg$data$series)
  expect_identical(series, c(
    "Empirical factor-analytic eigenvalues (maximum likelihood, polychoric correlations)",
    "Reference: 95th percentile of 100 simulated normal datasets"
  ))
  expect_equal(points$y[points$group == 1], emp, tolerance = 1e-12)
  expect_equal(points$y[points$group == 2], ref, tolerance = 1e-12)
  labels <- unlist(lapply(gg$layers, function(l) l$aes_params$label))
  expect_true(all(c("theory k", "decision k") %in% labels))
  expect_match(gsub("\\s+", " ", gg$labels$subtitle),
    "Kaiser's rule concerns the unreduced correlation matrix (see the Scree plot panel) and is not used.",
    fixed = TRUE
  )
  # Without a reference the empirical series is drawn alone, the legend
  # still naming it.
  alone <- sieve_pa_plot(emp, NULL)
  expect_identical(nrow(ggplot2::ggplot_build(alone)$data[[3]]), 5L)

  # In the module the figure is drawn from the reference the count was read
  # from, and follows the display controls.
  st <- dim_nine()
  shiny::testServer(mod_retention_server, args = list(state = st), {
    dim_retention_inputs(session)
    expect_no_error(output$scree)
    sel <- pa_sel()
    expect_true(sel$ok)
    gg <- sieve_pa_plot(as.numeric(pa()$fa.values), as.numeric(sel$values), sel$label, pa_run()$basis_used,
      theory_k = theory_k(), decision_k = decision_k()
    )
    built <- ggplot2::ggplot_build(gg)
    pts <- built$data[[3]]
    expect_equal(pts$y[pts$group == 2], as.numeric(sel$values), tolerance = 1e-12)
    expect_match(levels(gg$data$series)[2], "^Reference: 95th percentile", fixed = FALSE)
    session$setInputs(criterion = "mean")
    expect_match(pa_sel()$label, "^mean", fixed = FALSE)
    expect_no_error(output$scree)
  })
})

# ---- 6. The Save buttons ----------------------------------------------------

# The width and height a PNG's IHDR chunk records.
dim_png_size <- function(path) {
  ihdr <- readBin(path, "raw", 24L)
  px <- function(bytes) sum(as.integer(bytes) * 256^(3:0))
  c(width = px(ihdr[17:20]), height = px(ihdr[21:24]))
}
# The page size a PDF written by R's pdf device records, in points (the
# file is binary, so it is read by bytes and the MediaBox entry is found
# as bytes).
dim_pdf_size <- function(path) {
  raw <- readBin(path, "raw", file.size(path))
  txt <- rawToChar(raw[raw != as.raw(0L)])
  Encoding(txt) <- "bytes"
  box <- regmatches(txt, regexpr("/MediaBox \\[[0-9. ]+\\]", txt, useBytes = TRUE))
  nums <- as.numeric(strsplit(gsub("/MediaBox \\[|\\]", "", box, useBytes = TRUE), " ")[[1]])
  c(width = nums[3], height = nums[4])
}

test_that("every figure of the Dimensionality and CFA tabs has Save buttons that write a PNG at 300 dots per inch and a PDF at the screen geometry", {
  # Decision/plot/export mechanics use the explicitly reduced reference sample.
  siever_local_dmar_mechanics()
  # The model fit by k on the nine tests is estimated at about 2.6 seconds
  # under ML with FIML (Auto), so it would wait for its
  # button; this test reads its figure, so the estimate is pinned.
  test_frame <- environment()
  local_mocked_bindings(sieve_efa_plan_wait = function(...) 1, .env = test_frame)
  # The buttons are in the page, one pair per figure, above the plot.
  html <- as.character(mod_retention_ui("retention"))
  for (id in c("scree_plot", "scree", "fit_plot")) {
    expect_match(html, sprintf('id="retention-%s_png"', id), fixed = TRUE)
    expect_match(html, sprintf('id="retention-%s_pdf"', id), fixed = TRUE)
    expect_true(regexpr(sprintf('id="retention-%s_png"', id), html, fixed = TRUE) <
      regexpr(sprintf('id="retention-%s"', id), html, fixed = TRUE))
  }
  html_cfa <- as.character(mod_cfa_ui("cfa"))
  expect_match(html_cfa, 'id="cfa-compare_plot_png"', fixed = TRUE)
  expect_match(html_cfa, 'id="cfa-compare_plot_pdf"', fixed = TRUE)
  expect_match(as.character(sieve_plot_save_ui("x-y")), "Save the figure:", fixed = TRUE)

  st <- dim_nine()
  dir <- withr::local_tempdir()
  shiny::testServer(mod_retention_server, args = list(state = st), {
    dim_retention_inputs(session)
    session$setInputs(decide_k = 3, decide_why = "", decide_go = 1)
    # The browser reports the geometry of each plot output; the mock
    # session answers 600 by 400 px for every output, as a browser would
    # answer with the drawn size.
    expect_identical(sieve_plot_screen_size(session, "scree"), c(width = 600, height = 400))
    stems <- c(
      scree_plot = "siever_scree_plot", scree = "siever_parallel_analysis",
      fit_plot = "siever_fit_by_factors"
    )
    for (id in names(stems)) {
      png <- output[[paste0(id, "_png")]]
      pdf <- output[[paste0(id, "_pdf")]]
      expect_true(file.exists(png), info = id)
      expect_true(file.exists(pdf), info = id)
      expect_identical(basename(png), paste0(stems[[id]], ".png"))
      expect_identical(basename(pdf), paste0(stems[[id]], ".pdf"))
      expect_identical(as.integer(readBin(png, "raw", 4L)), c(137L, 80L, 78L, 71L))
      expect_identical(rawToChar(readBin(pdf, "raw", 4L)), "%PDF")
      # 600 by 400 px at 96 px per inch is 6.25 by 4.1667 inches: 1875 by
      # 1250 px at 300 dots per inch, 450 by 300 points on the PDF page.
      expect_identical(dim_png_size(png), c(width = 1875, height = 1250))
      expect_equal(dim_pdf_size(pdf), c(width = 450, height = 300), tolerance = 1e-6)
      expect_gt(file.size(png), 20000)
      expect_gt(file.size(pdf), 2000)
      file.copy(png, file.path(dir, basename(png)))
    }
    expect_null(grDevices::dev.list())
    # Each save is recorded as a note that names the figure and the file.
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("saved from the screen as siever_scree_plot.png", log, fixed = TRUE)))
    expect_true(any(grepl("saved from the screen as siever_parallel_analysis.pdf", log, fixed = TRUE)))
    expect_true(any(grepl("on a png device of 6.25 by 4.17 inches at res = 300", log, fixed = TRUE)))
  })
  # Without a reported geometry (no browser, or a plot not yet drawn) the
  # default is used, and a size the browser cannot have reported is
  # ignored.
  bare <- list(clientData = list(), ns = function(x) paste0("m-", x))
  expect_identical(sieve_plot_screen_size(bare, "scree"), c(width = 900, height = 460))
  odd <- list(
    clientData = list(`output_m-scree_width` = 10, `output_m-scree_height` = "wide"),
    ns = function(x) paste0("m-", x)
  )
  expect_identical(sieve_plot_screen_size(odd, "scree"), c(width = 900, height = 460))
  given <- list(
    clientData = list(`output_m-scree_width` = 960, `output_m-scree_height` = 480),
    ns = function(x) paste0("m-", x)
  )
  expect_identical(sieve_plot_screen_size(given, "scree"), c(width = 960, height = 480))
  # The writer at a stated geometry: 960 by 480 px is 10 by 5 inches, 3000
  # by 1500 px at 300 dots per inch and 720 by 360 points on the page.
  wired <- sieve_plot_save_server("scree",
    output = list(), session = given, state = NULL,
    draw = function() ggplot2::ggplot() +
      ggplot2::geom_blank()
  )
  png <- file.path(dir, "stated.png")
  pdf <- file.path(dir, "stated.pdf")
  wired$write(png, "png")
  wired$write(pdf, "pdf")
  expect_identical(dim_png_size(png), c(width = 3000, height = 1500))
  expect_equal(dim_pdf_size(pdf), c(width = 720, height = 360), tolerance = 1e-6)
  expect_null(grDevices::dev.list())

  # The CFA tab's figure, after the structures are fit.
  st <- dim_nine()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    dim_cfa_inputs(session)
    # Before the run there is no figure to save: the download fails with the
    # validation message rather than writing an empty file.
    expect_error(output$compare_plot_png, "not fit yet")
    expect_null(grDevices::dev.list())
    session$setInputs(compare_go = 1)
    png <- output$compare_plot_png
    pdf <- output$compare_plot_pdf
    expect_identical(basename(png), "siever_cfa_structures.png")
    expect_identical(dim_png_size(png), c(width = 1875, height = 1250))
    expect_equal(dim_pdf_size(pdf), c(width = 450, height = 300), tolerance = 1e-6)
    expect_null(grDevices::dev.list())
  })
})

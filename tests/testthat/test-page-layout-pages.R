# The layout and the Update of the Validity, CV and Invariance, SEM,
# Bifactor, IRT, Optimizer and Reproducible Code pages. Results first: on
# each page the status line, a one-line method statement and the result come
# before the explanations, whose first paragraph stays visible and the rest
# sits in a closed disclosure. One Update per page: a result an edit made
# stale is updated by one button, labeled with the combined estimate, that
# computes again every result of the page that was computed before and is
# now stale, and never a result that was never requested (a bootstrap, a
# Monte Carlo interval keep their own buttons). The words: Exclude and
# Include for items.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

up_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}
up_html <- function(x) paste(as.character(x), collapse = " ")

# The bundled nine tests, confirmed, continuous or cut into five ordered
# categories, with the variable roles declared.
up_nine <- function(ordered = FALSE) {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = TRUE, name = fx$name, configure = function(s) {
  s$roles <- sieve_declared_roles(fx$data, fx$item_cols)
  s$roles$role[s$roles$role == "auxiliary"] <- "criterion"
  s$estimator <- "MLR"
if (ordered) {
    shiny::isolate({
      s$items <- as.data.frame(lapply(s$items, function(v) as.integer(cut(as.numeric(v), breaks = 5))))
      m <- s$meta
      m$type <- "ordered"
      s$meta <- m
      s$estimator <- "WLSMV"
    })
  }
  })
  s
}

# Notifications are kept, not shown.
up_notes <- function(env = parent.frame()) {
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

up_reverse <- function(state, session, item) {
  out <- sieve_apply_reverse(state, item, origin = "test")
  sieve_confirm_types(state)
  session$flushReact()
  out
}

# The record a result published for the page guide, found by its id.
up_published <- function(state, id) {
  pages <- shiny::isolate(state$page_results) %||% list()
  for (pg in pages) if (!is.null(pg[[id]])) return(pg[[id]])
  NULL
}

# The position of a fragment in a page's markup (or 0 when absent).
up_at <- function(html, fragment) {
  at <- regexpr(fragment, html, fixed = TRUE)
  if (at < 0) 0L else as.integer(at)
}

# ---- One Update per page -----------------------------------------------------

test_that("Validity: one Update in the status line brings every stale model up to date, and the bootstrap keeps its button", {
  seen <- up_notes()
  st <- up_nine()
  sieve_apply_roles(st, c(age_years = "criterion"))
  shiny::isolate(sieve_confirm_types(st))
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(score_method = "mean", htmt_on = TRUE, marker = "", htmt_B = 200, htmt_seed = 5, htmt_thresh = 0.85)
    session$setInputs(fit_go = 1)
    session$setInputs(criteria = "age_years")
    session$setInputs(sem_go = 1)
    session$setInputs(ulmf_go = 1)
    session$setInputs(htmt_boot = 1)
    expect_identical(demand$status()$state, "current")
    expect_identical(demand_sem$status()$state, "current")
    expect_identical(demand_ulmf$status()$state, "current")
    # The method statement of the measurement model, read from the fit.
    expect_match(up_text(output$method), "Correlated-factors CFA in lavaan: ML with FIML on the covariances", fixed = TRUE)
    expect_match(up_text(output$method), "9 observed variables, 3 factors; n = 301.", fixed = TRUE)
    loading_of <- function(item) {
      ss <- cfa_run()$standardized$value
      ss$est.std[ss$op == "=~" & ss$rhs == item]
    }
    it <- shiny::isolate(state$meta$item[3])
    first_loading <- loading_of(it)
    up_reverse(state, session, it)
    for (d in list(demand, demand_sem, demand_ulmf)) expect_identical(d$status()$state, "stale")
    gate <- output$gate$html
    # One status line, one Update with the combined estimate, no per-result
    # refit button in it, and the bootstrap named with its own button.
    expect_identical(lengths(regmatches(gate, gregexpr(sprintf('id="%s"', session$ns("update_go")), gate, fixed = TRUE))), 1L)
    expect_match(up_text(gate), "Update (", fixed = TRUE)
    expect_match(up_text(gate), sprintf("What changed: %s reverse scored", it), fixed = TRUE)
    expect_match(up_text(gate), "The correlated-factors model, structural model, method-factor model, and HTMT bootstrap interval are from before.", fixed = TRUE)
    expect_match(up_text(gate), "computed again with its own button (Compute HTMT CIs, a bootstrap)", fixed = TRUE)
    expect_false(grepl(sprintf('id="%s"', session$ns("fit_go")), gate, fixed = TRUE))
    # The page guide is told that the page's Update brings them up to date.
    for (id in c("fit_go", "sem_go", "ulmf_go")) {
      expect_identical(up_published(state, session$ns(id))$action, "Update", info = id)
    }
    # The structural model's own panel points to the Update rather than
    # offering a second, narrower one.
    expect_match(up_text(output$sem_note), "Update, in the line at the top of the page, fits it again", fixed = TRUE)
    expect_false(grepl(sprintf('id="%s"', session$ns("sem_go")), up_html(output$sem_note), fixed = TRUE))
    boot_before <- htmt_ci()
    session$setInputs(update_go = 1)
    for (d in list(demand, demand_sem, demand_ulmf)) expect_identical(d$status()$state, "current")
    expect_match(seen$notes[length(seen$notes)],
      "^Updated the correlated-factors model, structural model, and method-factor model in"
    )
    # The bootstrap was not run again by Update.
    expect_identical(htmt_ci(), boot_before)
    # The refit is the model of the reversed data, not the one from before:
    # the reversed item's loading changes sign.
    expect_equal(loading_of(it), -first_loading, tolerance = 1e-6)
    # With nothing left for Update, the measurement model's own line and one
    # small line for the bootstrap.
    gate <- up_text(output$gate)
    expect_match(gate, "nothing analytic has changed since", fixed = TRUE)
    expect_match(gate, "The HTMT bootstrap interval is from before; it is computed again with its own button (Compute HTMT CIs, a bootstrap).", fixed = TRUE)
    expect_false(grepl(session$ns("update_go"), output$gate$html, fixed = TRUE))
  })
})

test_that("SEM, Bifactor and IRT: the stale line carries one Update, which refits the model", {
  seen <- up_notes()
  st <- up_nine()
  sieve_apply_roles(st, c(age_years = "criterion"))
  shiny::isolate(sieve_confirm_types(st))
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(layout = "tree2", min_path = 0)
    session$setInputs(outcomes = c("Textual", "age_years"), predictors = c("Visual", "Textual"))
    session$setInputs(fit_go = 1)
    expect_match(up_text(output$method), "Structural model in lavaan:", fixed = TRUE)
    expect_match(up_text(output$method), "Fit: CFI (robust)", fixed = TRUE)
    it <- shiny::isolate(state$meta$item[4])
    up_reverse(state, session, it)
    expect_identical(demand$status()$state, "stale")
    gate <- output$gate$html
    expect_match(up_text(gate), "The structural model is from before.", fixed = TRUE)
    expect_match(gate, sprintf('id="%s"', session$ns("update_go")), fixed = TRUE)
    session$setInputs(update_go = 1)
    expect_identical(demand$status()$state, "current")
    expect_match(seen$notes[length(seen$notes)], "^Updated the structural model in")
  })

  # Three group factors of three items: the bifactor plan's smallest model.
  # A reversal, not an exclusion, makes it stale (one exclusion leaves two
  # group factors, which the plan refuses).
  st <- up_nine()
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    session$setInputs(idx_B = 40, idx_seed = 7)
    suppressWarnings(session$setInputs(fit = 1))
    expect_match(up_text(output$method), "Bifactor model in lavaan:", fixed = TRUE)
    expect_match(up_text(output$method), "general and group factors orthogonal", fixed = TRUE)
    it <- shiny::isolate(state$meta$item[9])
    up_reverse(state, session, it)
    expect_identical(demand$status()$state, "stale")
    status <- output$status$html
    expect_match(up_text(status), "The bifactor model is from before.", fixed = TRUE)
    expect_match(status, sprintf('id="%s"', session$ns("update_go")), fixed = TRUE)
    suppressWarnings(session$setInputs(update_go = 1))
    expect_identical(demand$status()$state, "current")
  })

  st <- up_nine(ordered = TRUE)
  shiny::testServer(mod_irt_server, args = list(state = st), {
    session$setInputs(
      scope = "__all__", metric = "normal_ogive", estimator = "WLSMV",
      theta_min = -4, theta_max = 4, theta_n = 81, info_threshold = 10
    )
    session$setInputs(fit_go = 1)
    expect_match(up_text(output$model_note), "Fit: CFI (", fixed = TRUE)
    sieve_remove_cases(state, 1L, "manual update coverage")
    session$flushReact()
    expect_identical(demand$status()$state, "stale")
    expect_match(up_text(output$gate), "The graded response model is from before.", fixed = TRUE)
    session$setInputs(update_go = 1)
    expect_identical(demand$status()$state, "current")
    expect_length(grm_fit()$items, 9L)
  })
})

test_that("CV and Invariance: one Update computes the split and the ladder again for the current data", {
  seen <- up_notes()
  st <- up_nine()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(holdout = 50, seed = 7, group = "school", inv_missing = "auto")
    session$setInputs(split = 1)
    session$setInputs(run_inv = 1)
    expect_null(cv_shown()$error)
    expect_null(inv_shown()$error)
    expect_null(output$gate)
    it <- shiny::isolate(state$meta$item[2])
    up_reverse(state, session, it)
    expect_true(isTRUE(cv_shown()$stale))
    expect_true(isTRUE(inv_shown()$stale))
    gate <- output$gate$html
    expect_match(up_text(gate), "The calibration/validation split and invariance ladder are from before.", fixed = TRUE)
    expect_match(up_text(gate), "Not shown until updated", fixed = TRUE)
    expect_match(gate, sprintf('id="%s"', session$ns("update_go")), fixed = TRUE)
    expect_identical(up_published(state, session$ns("result_inv"))$action, "Update")
    session$setInputs(update_go = 1)
    expect_null(cv_shown()$stale)
    expect_null(inv_shown()$stale)
    expect_null(output$gate)
    # The ladder Update fitted is the one Run invariance fits on these data.
    updated <- inv_result()$table
    session$setInputs(run_inv = 2)
    expect_identical(inv_result()$table, updated)
    expect_match(seen$notes[length(seen$notes)], "^Updated the calibration/validation split and invariance ladder in")
  })
})

test_that("CV and Invariance: a fit refused before it starts is stated in the refusal's own words", {
  st <- up_nine()
  testthat::local_mocked_bindings(sieve_fit_invariance = function(...) {
    list(error = "Not fit: the robust (MLR) fit would need more memory than this machine has.", refused = TRUE)
  })
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school", inv_missing = "auto")
    session$setInputs(run_inv = 1)
    expect_identical(inv_result()$error, "Not fit: the robust (MLR) fit would need more memory than this machine has.")
    expect_false(grepl("could not be fit", up_text(output$inv_msg), fixed = TRUE))
  })
})

test_that("Optimizer: candidates from before are searched again by the page's Update", {
  seen <- up_notes()
  st <- up_nine()
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(max_drop = 1, min_keep = 2, ss = FALSE, cap = 300, undersized = "hold")
    expect_match(up_text(output$gate), "No search has been run yet.", fixed = TRUE)
    session$setInputs(run = 1)
    first <- res()
    expect_match(up_text(output$run_head), "Examined all", fixed = TRUE)
    it <- shiny::isolate(state$meta$item[1])
    up_reverse(state, session, it)
    expect_false(is.null(stale()))
    gate <- output$gate$html
    expect_match(up_text(gate), "The optimizer candidates are from before.", fixed = TRUE)
    expect_match(gate, sprintf('id="%s"', session$ns("update_go")), fixed = TRUE)
    session$setInputs(update_go = 1)
    expect_null(stale())
    expect_false(identical(res()$revision, first$revision))
  })
})

test_that("Reproducible Code: a methods draft from before is assembled again by the page's Update", {
  seen <- up_notes()
  st <- up_nine()
  shiny::testServer(mod_repro_server, args = list(state = st), {
    session$setInputs(draft_go = 1)
    expect_null(draft_stale())
    expect_null(output$status)
    it <- shiny::isolate(state$meta$item[5])
    up_reverse(state, session, it)
    expect_false(is.null(draft_stale()))
    expect_match(up_text(output$status), "The methods draft is from before.", fixed = TRUE)
    session$setInputs(update_go = 1)
    expect_null(draft_stale())
    expect_match(up_text(output$code_line), "recorded step", fixed = TRUE)
  })
})

test_that("an edit elsewhere keeps the choices a page's results were computed under, so its Update follows them", {
  calls <- new.env(parent = emptyenv())
  calls$log <- list()
  testthat::local_mocked_bindings(
    updateSelectInput = function(session, inputId, label = NULL, choices = NULL, selected = NULL) {
      calls$log[[length(calls$log) + 1L]] <- list(id = inputId, selected = selected)
      invisible(NULL)
    }
  )
  last <- function(id) {
    hit <- Filter(function(x) identical(x$id, id), calls$log)
    if (length(hit)) hit[[length(hit)]] else NULL
  }
  st <- up_nine()
  sieve_apply_roles(st, c(age_years = "criterion"))
  shiny::isolate(sieve_confirm_types(st))
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(layout = "tree2", min_path = 0)
    session$setInputs(outcomes = c("Textual", "age_years"), predictors = "Visual")
    # A label edit re-reads the constructs, and the selectors are offered
    # their choices again: the outcomes and the predictor stay.
    m <- shiny::isolate(state$meta)
    m$label[1] <- "Renamed for the test"
    state$meta <- m
    session$flushReact()
    expect_identical(last("outcomes")$selected, c("Textual", "age_years"))
    expect_identical(last("predictors")$selected, "Visual")
  })
  st <- up_nine()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school", inv_level = "metric")
    up_reverse(state, session, shiny::isolate(state$meta$item[2]))
    expect_identical(last("group")$selected, "school")
  })
  st <- up_nine()
  sieve_apply_roles(st, c(age_years = "criterion"))
  shiny::isolate(sieve_confirm_types(st))
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(criteria = "age_years", marker = "", score_method = "mean", htmt_on = FALSE)
    up_reverse(state, session, shiny::isolate(state$meta$item[2]))
    expect_identical(last("criteria")$selected, "age_years")
  })
})

# ---- Results first -----------------------------------------------------------

test_that("each page puts its result before its explanation, and the explanation's rest is closed", {
  open_after <- function(html, fragment) {
    at <- up_at(html, fragment)
    expect_gt(at, 0L, label = fragment)
    before <- substr(html, 1L, at)
    opened <- gregexpr("<details", before, fixed = TRUE)[[1]]
    closed <- gregexpr("</details>", before, fixed = TRUE)[[1]]
    n_open <- sum(opened > 0) - sum(closed > 0)
    expect_gt(n_open, 0L, label = paste("inside a disclosure:", fragment))
    last <- max(opened)
    expect_true(grepl("^<details[^>]*open", substr(html, last, last + 80L)), label = fragment)
  }
  v <- up_html(mod_validity_ui("v"))
  expect_lt(up_at(v, 'id="v-phi_tbl"'), up_at(v, "asks whether the scale measures the construct it claims to"))
  expect_lt(up_at(v, 'id="v-ave_tbl"'), up_at(v, "Your Latent Correlation Boundary"))
  expect_lt(up_at(v, 'id="v-method"'), up_at(v, 'id="v-phi_tbl"'))
  expect_lt(up_at(v, 'id="v-harman_tbl"'), up_at(v, 'id="v-harman_note"'))
  expect_lt(up_at(v, 'id="v-ulmf_tbl"'), up_at(v, "A single method factor, orthogonal"))
  s <- up_html(mod_sem_ui("s"))
  expect_lt(up_at(s, 'id="s-method"'), up_at(s, 'id="s-diagram"'))
  expect_lt(up_at(s, 'id="s-diagram"'), up_at(s, "measurement loading"))
  open_after(s, "<strong>residual (disturbance) covariance</strong>")
  expect_lt(up_at(s, 'id="s-paths_tbl"'), up_at(s, "Each row names the fitted outcome scale"))
  expect_lt(up_at(s, 'id="s-effects_tbl"'), up_at(s, "acts as a "))
  b <- up_html(mod_bifactor_ui("b"))
  expect_lt(up_at(b, ">Bifactor indices<"), up_at(b, ">Model<"))
  expect_lt(up_at(b, 'id="b-idx_tbl"'), up_at(b, "These indices answer one question"))
  expect_lt(up_at(b, 'id="b-idx_tbl"'), up_at(b, 'id="b-improper"'))
  open_after(b, "(explained common variance)")
  open_after(b, "What a row holds.")
  i <- up_html(mod_irt_ui("i"))
  expect_lt(up_at(i, 'id="i-param_tbl"'), up_at(i, "One model, two parameterizations."))
  open_after(i, "The constant 1.702.")
  open_after(i, "Read the fit before the parameters.")
  expect_lt(up_at(i, 'id="i-param_tbl"'), up_at(i, "What this tab adds."))
  o <- up_html(mod_optimize_ui("o"))
  expect_lt(up_at(o, 'id="o-results"'), up_at(o, "What this search does, and what it does not"))
  open_after(o, "A chosen item set needs independent data.")
  open_after(o, "AIC and BIC are not shown.")
  f <- up_html(mod_finalize_ui("f"))
  expect_lt(up_at(f, 'id="f-inv_tbl"'), up_at(f, "The ladder is fit by"))
  open_after(f, "With ordered or binary items the ladder has five rungs")
  expect_lt(up_at(f, 'id="f-dmacs_tbl"'), up_at(f, "A score test or a likelihood ratio test"))
  open_after(f, "Which fitted model dMACS can be read from.")
  expect_lt(up_at(f, 'id="f-align_tbl"'), up_at(f, "How the criterion decides."))
  scoring <- up_html(mod_finalize_scoring_ui("f"))
  expect_lt(up_at(scoring, 'id="f-norms_tbl"'), up_at(scoring, "Scores follow each construct"))
  expect_lt(up_at(f, 'id="f-cv_tbl"'), up_at(f, "Two safeguards against over-fitting"))
  open_after(f, "(Meredith, 1993) tests whether the model means the same thing")
  l <- up_html(mod_longitudinal_ui("l"))
  expect_lt(up_at(l, 'id="l-fit_table"'), up_at(l, 'id="l-mapping"'))
  r <- up_html(mod_repro_ui("r"))
  expect_lt(up_at(r, 'id="r-code"'), up_at(r, "Each action records its equivalent R"))
  expect_lt(up_at(r, 'id="r-draft"'), up_at(r, "This draft runs a new measurement analysis"))
  # The sentence names the button "Run a new analysis and draft"; the
  # disclosure is closed.
  open_after(r, "with the study's estimator, on the data as they stand when the button is pressed")
})

test_that("the method statement is read from the fitted model", {
  hs <- lavaan::HolzingerSwineford1939
  model <- "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9"
  mlr <- lavaan::cfa(model, data = hs, estimator = "MLR", missing = "fiml", std.lv = TRUE)
  expect_identical(
    sieve_method_text(mlr, "Correlated-factors CFA"),
    "Correlated-factors CFA in lavaan: MLR with FIML on the covariances, latent variances fixed at 1; 9 observed variables, 3 factors; n = 301."
  )
  ml <- lavaan::cfa(model, data = hs[1:200, ], std.lv = TRUE)
  expect_match(sieve_method_text(ml, "A model"), "ML on the complete cases on the covariances", fixed = TRUE)
  cut5 <- as.data.frame(lapply(hs[paste0("x", 1:9)], function(v) as.integer(cut(v, 5))))
  wls <- suppressWarnings(lavaan::cfa(model, data = cut5, ordered = names(cut5), std.lv = TRUE))
  expect_match(sieve_method_text(wls, "A model"), "WLSMV on the complete cases on the polychoric correlations", fixed = TRUE)
  m <- sieve_fit_measures(mlr)
  expect_match(sieve_method_text(mlr, "A model", measures = m), sprintf("Fit: CFI (robust) %.3f", m[["cfi"]]), fixed = TRUE)
  expect_null(sieve_method_text(NULL, "nothing"))
})

test_that("Exclude and Include are the words for items on these pages", {
  o <- up_text(up_html(mod_optimize_ui("o")))
  expect_match(o, "Maximum items to exclude per construct", fixed = TRUE)
  expect_false(grepl("items to drop", o, fixed = TRUE))
  expect_false(grepl("by dropping items", o, fixed = TRUE))
  expect_identical(names(sieve_order_choices())[1], "Search order (fewest items excluded first)")
  src <- function(f) {
    root <- testthat::test_path("..", "..", "R", f)
    if (!file.exists(root)) skip("not running from the package source")
    paste(readLines(root, warn = FALSE), collapse = "\n")
  }
  expect_match(src("mod_longitudinal.R"), "include an excluded item again", fixed = TRUE)
  expect_false(grepl("restore an excluded item", src("mod_longitudinal.R"), fixed = TRUE))
})

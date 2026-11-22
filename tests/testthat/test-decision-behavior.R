# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

test_that("reliability scripts serialize multiple items and auxiliaries as one call", {
  fx <- sieve_hs9_fixture()
  d <- fx$data
  its <- fx$item_cols[1:3]
  d$aux_a <- seq_len(nrow(d)) / nrow(d)
  d$aux_b <- cos(seq_len(nrow(d)))
  d[1:15, its[1]] <- NA_real_
  spec <- sieve_reliability_spec(its, rep("continuous", 3), "Visual", missing = "fiml",
    ci_method = "ml", denominator = "model_implied", aux = c("aux_a", "aux_b"))
  code <- sieve_reliability_call_code(spec, object = "replayed")
  env <- new.env(parent = globalenv())
  env$d <- d
  env$items <- d[, its]
  expect_no_error(eval(parse(text = code), env))
  reference <- DMAR::reliability_omega(data = d[, c(its, "aux_a", "aux_b")],
    aux = c("aux_a", "aux_b"), ci_method = "ml", missing = "fiml", denominator = "model_implied")
  expect_equal(env$replayed$value, reference$value, tolerance = 1e-10)
  expect_length(sieve_r_literal(c("aux_a", "aux_b")), 1L)
})

test_that("EM replay supplies the complete-case reference view on incomplete data", {
  st <- missing_study_state()
  env <- new.env(parent = globalenv())
  env$active_items <- shiny::isolate(st$items)
  env$d <- shiny::isolate(st$raw)
  b <- shiny::isolate(sieve_state_basis(st))
  expect_no_error(eval(parse(text = sieve_retention_cor_code(b)), env))
  expect_identical(env$complete_rows, stats::complete.cases(env$active_items))
  expect_equal(env$R_items, b$R, tolerance = 1e-10)
  expect_equal(env$n_complete, 276L)
})

test_that("SEM replay carries both declared FIML auxiliaries", {
  st <- missing_study_state()
  raw <- shiny::isolate(st$raw)
  raw$aux_a <- seq_len(nrow(raw)) / 10
  raw$aux_b <- 10 * cos(seq_len(nrow(raw)))
  st$raw <- raw
  st$roles <- sieve_declared_roles(raw, names(shiny::isolate(st$items)))
  shiny::isolate(st$roles$role[st$roles$variable %in% c("aux_a", "aux_b")] <- "auxiliary")
  shiny::isolate(st$roles$role[st$roles$variable == "age_years"] <- "criterion")
  run <- shiny::isolate(sieve_run_structural(st$items, st$raw, st$meta,
    outcomes = "Speed", predictors = c("Visual", "Textual"), sampling_state = st))
  expect_true(inherits(run$fit, "lavaan"))
  env <- new.env(parent = globalenv())
  env$items <- shiny::isolate(st$items)
  env$d <- raw
  expect_no_error(suppressWarnings(eval(parse(text = sieve_sem_code(run$spec)), env)))
  expect_equal(lavaan::coef(env$sem_fit), lavaan::coef(run$fit), tolerance = 1e-8)
})

test_that("score downloads preserve fitted row identities and reject stale requests", {
  st <- missing_study_state()
  raw <- shiny::isolate(st$raw)
  # A construct label used as an auxiliary must not collide with its factor.
  raw$Visual <- seq_len(nrow(raw)) / 10
  st$raw <- raw
  st$roles <- data.frame(variable = "Visual", role = "auxiliary")
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(score_method = "Bartlett", conf_level = .95, scores_go = 1L)
    run <- demand_scores$result()
    expect_true(inherits(run$fit, "lavaan"))
    table <- utils::read.csv(output$dl_both_scores)
    expect_identical(sort(unique(table$original_row)), run$case_index)
    expect_true("Visual" %in% names(run$score_data))
    session$setInputs(score_uncertainty_go = 1L)
    expect_no_error(advanced_reliability$scores())
    state$case_index <- rev(seq_len(nrow(state$items))) + 1000L
    session$flushReact()
    expect_error(output$dl_both_scores, "Recompute factor scores", fixed = TRUE)
  })
})

test_that("score answered counts exclude auxiliaries and SPSS user-missing codes", {
  skip_if_not_installed("haven")
  skip_if_not("factor_scores" %in% getNamespaceExports("DMAR"))
  st <- missing_study_state()
  d <- shiny::isolate(st$items)
  its <- names(d)
  d[1, ] <- NA_real_
  d$aux_a <- seq_len(nrow(d)) / nrow(d)
  d[[its[1]]] <- haven::labelled_spss(replace(d[[its[1]]], 2L, 99), na_values = 99)
  meta <- shiny::isolate(st$meta)
  spec <- sieve_cfa_spec(meta, reserved = names(d))
  spec <- sieve_spec_engine_args(spec, list(aux = "aux_a"))
  fit <- sieve_run_cfa(spec, d)$fit
  expect_true(inherits(fit, "lavaan"))
  record <- sieve_factor_score_record(fit, data = d)
  expect_true(all(record$uncertainty$answered[record$uncertainty$row == 1L] == 0L))
  expect_true(all(record$uncertainty$answered[record$uncertainty$row == 2L] == 8L))
  expect_true(all(is.na(record$uncertainty$estimate[record$uncertainty$row == 1L])))
})

test_that("the initial FMI table renders before requesting its bootstrap intervals", {
  st <- missing_study_state()
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", estimator = "study", fit_go = 1L)
    expect_match(as.character(output$missing_info_note$html), "have not been requested", fixed = TRUE)
    expect_no_error(output$missing_info_table)
    expect_no_error(output$mi_note)
    expect_match(as.character(output$mi_sensitivity_note$html), "No MI sensitivity", fixed = TRUE)
  })
})

test_that("item status restoration restores each researcher's exclusion reason in one action", {
  st <- missing_study_state()
  shiny::isolate({
    ids <- st$meta$item[1:2]
    sieve_set_item_status(st, ids, "parked", reasons = c("Content overlap", "Ambiguous wording"))
    before <- st$meta
    sieve_set_item_status(st, ids, "active")
    actions <- length(st$item_actions)
    sieve_set_item_status(st, ids, before$status[match(ids, before$item)],
      reasons = before$exclude_reason[match(ids, before$item)])
    expect_identical(st$meta$exclude_reason[match(ids, st$meta$item)], c("Content overlap", "Ambiguous wording"))
    expect_length(st$item_actions, actions + 1L)
    script <- sieve_replay_script(st)
    expect_match(script, "Content overlap", fixed = TRUE)
  })
})

test_that("the first exclusion replays with one reason and eight missing reasons", {
  st <- missing_study_state()
  its <- shiny::isolate(st$meta$item)
  sieve_set_item_status(st, its[2L], "parked", reason = "Ambiguous wording")
  env <- new.env(parent = globalenv())
  expect_no_error(eval(parse(text = sieve_replay_script(st)), env))
  expect_identical(env$meta$exclude_reason, shiny::isolate(st$meta$exclude_reason))
  expect_identical(which(!is.na(env$meta$exclude_reason)), 2L)
})

test_that("an auxiliary-only change creates a new saved analysis record", {
  st <- missing_study_state()
  raw <- shiny::isolate(st$raw)
  raw$aux_a <- sin(seq_len(nrow(raw)))
  st$raw <- raw
  st$roles <- sieve_declared_roles(raw, names(shiny::isolate(st$items)))
  shiny::isolate(st$roles$role[st$roles$variable == "aux_a"] <- "auxiliary")
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", estimator = "study", fit_go = 1L)
    first <- cfa_run()$analysis_id
    saved <- state$analyses[[first]]$estimates
    meta <- state$meta
    meta$label[1L] <- "A new display label"
    sieve_write_ledger(state, meta)
    expect_match(sieve_record_currency(state$analyses[[first]], state), "current", fixed = TRUE)
    raw <- state$raw
    raw$aux_a <- rev(raw$aux_a)
    state$raw <- raw
    shiny::isolate(sieve_confirm_types(state))
    expect_identical(sieve_analysis_catalog(state)$revision[1L], "earlier")
    expect_match(sieve_record_currency(state$analyses[[first]], state), "auxiliary", fixed = TRUE)
    session$setInputs(fit_go = 2L)
    expect_false(identical(cfa_run()$analysis_id, first))
    expect_identical(state$analyses[[first]]$estimates, saved)
  })
})

test_that("Network sparse-cell checks consider only ordinal pairs and ignore SPSS missing codes", {
  full <- expand.grid(a = 1:3, b = 1:3)
  expect_false(sieve_network_empty_cells(full))
  expect_true(sieve_network_empty_cells(full[-1, ]))
  skip_if_not_installed("haven")
  full <- rbind(full, data.frame(a = 99, b = 2))
  full$a <- haven::labelled_spss(full$a, na_values = 99)
  expect_false(sieve_network_empty_cells(full))
})

test_that("MI count planning uses the pilot FMI upper bound and preserves identification", {
  plan <- sieve_mi_plan(.5)
  expect_equal(plan$table$upper_limit, stats::plogis(stats::qnorm(.975) * sqrt(2 / 20)), tolerance = 1e-12)
  expect_identical(plan$imputations, 86L)
  expect_error(sieve_mi_model("F =~ x1 + x2 + x3", data.frame(), ordered = "x1", pilot = 10L), "at least 20")
  expect_error(sieve_mi_plan(.5, pilot = 1), "at least two")
})

test_that("inclusion refits wait for confirmation and ignore threshold changes", {
  state <- missing_study_state()
  calls <- 0L
  server <- function(input, output, session) {
    sieve_inclusion_wire(state, function() calls <<- calls + 1L)
  }
  shiny::testServer(server, {
    session$flushReact()
    expect_equal(calls, 0L)
    state$confirmed <- FALSE
    m <- state$meta
    m$status[1] <- "parked"
    state$meta <- m
    session$flushReact()
    expect_equal(calls, 0L)
    state$confirmed <- TRUE
    session$flushReact()
    expect_equal(calls, 1L)
    state$thresholds$salient <- .4
    session$flushReact()
    expect_equal(calls, 1L)
    sieve_set_item_status(state, m$item[1], "active")
    session$flushReact()
    expect_equal(calls, 2L)
  })
})

test_that("EM scree and loadings survive patterned missingness with few complete cases", {
  st <- missing_study_state()
  X <- shiny::isolate(st$items)
  # Three missingness patterns retain each pair in two thirds of the sample.
  for (row in 6:nrow(X)) X[row, 1L + row %% ncol(X)] <- NA_real_
  st$items <- X
  expect_lt(sum(stats::complete.cases(X)), 10L)
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", max_k = 3L, n_iter = 10L)
    expect_true(isTRUE(basis()$ok))
    expect_no_error(output$scree_plot)
    session$setInputs(pa_go = 1L)
    # (DMAR documents the complete-case reference N of incomplete data as a
    # benchmark, not guaranteed to be conservative, so the message does not
    # call it conservative.)
    expect_match(pa_error(), "ten complete cases for its reference N", fixed = TRUE)
  })
  b <- shiny::isolate(sieve_state_basis(st))
  diagnostics <- sieve_item_diagnostics(X, shiny::isolate(st$meta), basis = b, cores = 1L)
  expect_true(is.data.frame(diagnostics$items))
  expect_identical(diagnostics$basis_used, "pearson")
})

test_that("MI results remain historical after an auxiliary-only change or a new seed", {
  st <- missing_study_state()
  raw <- shiny::isolate(st$raw)
  raw$aux_a <- seq_len(nrow(raw))
  st$raw <- raw
  st$roles <- data.frame(variable = "aux_a", role = "auxiliary")
  test_frame <- environment()
  # The analysis runs in a background process (sieve_mi_start(), replaced
  # here by a finished job); the panel's own check needs ordered items,
  # which this continuous study has not, so it is passed over.
  local_mocked_bindings(sieve_mi_start = mi_fake_start(), sieve_mi_check = function(...) invisible(NULL),
    .env = test_frame)
  parent <- shiny::reactiveVal(list(spec = shiny::isolate(sieve_cfa_spec(st$meta)),
    fit = list(input = 1L)))
  server <- function(input, output, session) {
    mi <- sieve_mi_server(input, output, session, st, parent, function() TRUE)
  }
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(server, {
    session$setInputs(mi_sensitivity_go = 1L, mi_sensitivity_seed = 1L)
    session$elapse(600)
    expect_true(is.list(mi$shown()$value))
    session$setInputs(mi_sensitivity_seed = 2L)
    expect_match(mi$shown()$reason, "historical", fixed = TRUE)
    session$setInputs(mi_sensitivity_seed = 1L)
    raw <- st$raw
    raw$aux_a[1L] <- raw$aux_a[1L] + 1
    st$raw <- raw
    changed <- parent()
    changed$fit <- list(input = 2L)
    parent(changed)
    expect_match(mi$shown()$reason, "historical", fixed = TRUE)
  })
})

test_that("bootstrap source preserves SPSS value and range missingness", {
  skip_if_not_installed("haven")
  v <- haven::labelled_spss(c(1, 2, 99, 95, 3), na_values = 99, na_range = c(90, 98))
  env <- new.env(parent = baseenv())
  env$pool <- data.frame(x = v)
  eval(parse(text = sieve_numeric_code("pool")), env)
  expect_identical(env$pool$x, c(1, 2, NA_real_, NA_real_, 3))
  recorded <- c(1, 99, 3)
  attr(recorded, "spss_missing") <- list(values = 99, range = NULL)
  env$pool <- data.frame(x = recorded)
  eval(parse(text = sieve_numeric_code("pool")), env)
  expect_identical(env$pool$x, c(1, NA_real_, 3))
})

test_that("the shared study builder matches bundled Ingest Load", {
  expected <- sieve_study_state()
  actual <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = actual), {
    session$setInputs(source = "hs9", load = 1L)
    for (field in c("raw", "items", "meta", "roles", "theory", "confirmed", "estimator", "case_index")) {
      expect_identical(state[[field]], shiny::isolate(expected[[field]]), info = field)
    }
  })
})

test_that("EFA settings that name a range of factor counts take the selected count when it changes", {
  withr::local_options(sieve.auto_run = "never")
  state <- siever_test_study()
  state$efa_settings <- list(n_factors = 3L, cor_type = "pearson", fm = "ml",
    rotation = "oblimin", fit_scope = "range", fit_from = 1L, fit_to = 5L)
  shiny::testServer(mod_efa_server, args = list(state = state), {
    session$setInputs(n_factors = 3L, cor_type = "pearson", fm = "ml", rotation = "oblimin")
    expect_null(efa_binding$pending())
    session$setInputs(n_factors = 2L)
    expect_identical(state$efa_settings$n_factors, 2L)
    expect_identical(state$efa_settings$fit_scope, "k")
    expect_null(state$efa_settings$fit_to)
  })
})

test_that("a bootstrap replication without an estimate is set aside", {
  test_frame <- environment()
  local_mocked_bindings(sieve_reliability_boot_estimate = function(...) NA_real_, .env = test_frame)
  draw <- sieve_reliability_boot_replicate(list(pool = data.frame(x = 1:8), seeds = 1L), 1L)
  expect_false(draw$converged)
  expect_false(draw$used)
  expect_identical(draw$outcome, "not_converged")
})

test_that("retained improper bootstrap coefficients trigger the tail-area caution", {
  state <- siever_test_study()
  data <- shiny::isolate(state$items[, 4:6])
  spec <- sieve_reliability_spec(names(data), rep("continuous", 3), construct = "Textual",
    missing = "listwise", ci_method = "percentile", B = 40L, seed = 1L)
  plan <- sieve_reliability_boot_plan(data, spec, workers = 1L)
  expect_identical(plan$status$code, "ready")
  draws <- lapply(seq_len(40L), function(i) list(draw = i, seed = i,
    outcome = if (i == 1L) "inadmissible" else "used", returned = TRUE, converged = TRUE,
    admissible = i != 1L, used = TRUE, estimate = .75 + i / 1000, message = ""))
  result <- sieve_reliability_boot_result(plan, draws)
  expect_identical(result$uncertainty$B_used, 40L)
  expect_identical(result$uncertainty$counts$improper_kept, 1L)
  expect_true(result$uncertainty$caution)
  sentence <- sieve_reliability_paragraph(list(omega = result, H = list(estimate = NA_real_, label = "H", error = "not requested")))
  expect_match(sentence, "1 improper coefficients retained", fixed = TRUE)
  expect_match(sentence, "limits depend", fixed = TRUE)
})

test_that("confirmation origin survives opening, and a project without a confirmation record has no invented time", {
  source <- sieve_study_state()
  original <- shiny::isolate(source$confirmation)
  snapshot <- sieve_project_snapshot(source)
  restored <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = restored), {
    sieve_project_restore(state, snapshot)
    session$flushReact()
    expect_identical(state$confirmation$origin, original$origin)
    expect_identical(state$confirmation$time, original$time)
    expect_true(sieve_confirmation_status(state)$confirmed)
  })
  snapshot$other$confirmation <- NULL
  snapshot$keys$content <- sieve_content_key(snapshot)
  legacy <- sieve_new_state()
  shiny::isolate(sieve_project_restore(legacy, snapshot))
  expect_identical(shiny::isolate(legacy$confirmation$origin), "project")
  expect_null(shiny::isolate(legacy$confirmation$time))
})

test_that("mixed plain-text and structured script steps remain ordered and inert through a project", {
  state <- siever_test_study()
  shiny::isolate({
    state$codelog <- c("# [1] Historical action", "stop('Historical code must stay inert')", "")
    sieve_log(state, "Verified second action", "verified_second <- TRUE")
    sieve_log(state, "Third action with a numbered comment", c("# [999] An example, not another step", "verified_third <- TRUE"))
    expect_identical(names(sieve_script_records(state)), c("1", "2", "3"))
    expect_true(state$replay_operations[["1"]]$legacy)
    path <- tempfile(fileext = ".siever.rds")
    sieve_project_write(sieve_project_snapshot(state), path)
    restored <- sieve_new_state()
    sieve_project_restore(restored, sieve_project_read(path))
    script <- sieve_replay_script(restored)
    environment <- new.env(parent = baseenv())
    expect_no_error(eval(parse(text = script), environment))
    expect_true(environment$verified_second)
    expect_true(environment$verified_third)
    expect_identical(names(sieve_script_records(restored)), c("1", "2", "3"))
  })
})


test_that("auxiliary-only observations remain in H and its replay", {
  d <- withr::with_seed(149L, {
    latent <- stats::rnorm(240)
    items <- as.data.frame(sapply(seq_len(5L), function(i) .7 * latent + stats::rnorm(240, sd = .7)))
    names(items) <- paste0("x", seq_len(5L))
    items$aux <- latent + stats::rnorm(240)
    items[seq_len(20L), seq_len(5L)] <- NA_real_
    items
  })
  sp <- sieve_reliability_spec(paste0("x", 1:5), rep("continuous", 5), "Scale",
    missing = "fiml", ci_method = "ml", denominator = "model_implied", aux = "aux")
  observed <- sieve_reliability_H(d, sp)
  expect_identical(observed$n$used, 240L)
  env <- new.env(parent = globalenv())
  env$items <- d[, sp$items]
  env$d <- d
  code <- c(sieve_reliability_H_helper_code(), sieve_reliability_H_code(sp))
  expect_no_error(eval(parse(text = code), env))
  expect_equal(sum(lavaan::lavInspect(env$h_fit, "nobs")), 240L)
  expected <- DMAR::reliability_H(fit = env$h_fit)
  expect_equal(observed$estimate, sieve_reliability_value(expected, "estimate"), tolerance = 1e-9)
})

# Respondents removed from the analysis (and restored to it), and items
# reverse scored: every result is either computed again on the rows in the
# analysis, with n stated, or withheld or marked as computed on earlier data
# with the action that updates it, and a restore gives back the first
# results exactly. The checks at the end compare recomputed results with the same analysis run directly on
# the data without the removed rows (lavaan, psych, DMAR, base R).
#
# Author: Ken Kelley

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("psych")
skip_if_not_installed("shiny")

# The nine tests (n = 301), confirmed continuous (ML with FIML), with
# missing responses for three respondents: id 11 (row 10, two items), row
# 20 and row 77. id is the identifier.
rc_hs9 <- function(missing = TRUE) {
  fx <- sieve_hs9_fixture()
  if (missing) {
    fx$data[10, c("t2_cubes", "t7_sentence")] <- NA
    fx$data[20, "t1_visual_perception"] <- NA
    fx$data[77, c("t10_addition", "t12_counting_groups_of_dots")] <- NA
  }
  siever_test_study(fx, name = "Nine tests with missing responses",
    configure = function(s) {
      roles <- sieve_propose_roles(fx$data)
      roles$role[roles$role == "auxiliary"] <- "criterion"
      s$roles <- roles
    })
}

# The same respondents cut into four ordered categories per item and
# confirmed as ordered (the graded response model is defined for them).
rc_hs9_ordered <- function() {
  s <- rc_hs9()
  shiny::isolate({
    cut4 <- function(v) as.integer(cut(v, stats::quantile(v, c(0, .25, .5, .75, 1), na.rm = TRUE), include.lowest = TRUE))
    it <- s$items
    it[] <- lapply(it, cut4)
    raw <- s$raw
    raw[names(it)] <- it
    s$items <- it
    s$raw <- raw
    m <- s$meta
    m$type <- "ordered"
    s$meta <- m
    s$estimator <- "WLSMV"
    sieve_confirm_types(s)
  })
  s
}

rc_text <- function(x) gsub("[[:space:]]+", " ", paste(as.character(unlist(x)), collapse = " "))

# Remove id 11 (row 10, with missing responses) and id 349 (row 300), typed
# as identifiers in the Screening box, through the shared service the
# Screening tab calls.
rc_remove <- function(s) {
  ids <- shiny::isolate(sieve_case_ids(s))
  found <- sieve_case_parse("11 349", ids)
  expect_identical(found$rows, c(10L, 300L))
  out <- sieve_set_case_status(s, found$rows, "excluded", "review test")
  expect_identical(out$removed, c(10L, 300L))
  invisible(out)
}

rc_restore <- function(s) {
  out <- sieve_set_case_status(s, c(10L, 300L), "included", "review test")
  expect_setequal(out$restored, c(10L, 300L))
  invisible(out)
}

# ---- Panels that kept a result and did not notice a removal ------------------

test_that("the longitudinal rung is marked historical once respondents are removed", {
  x <- withr::with_seed(14, {
    n <- 300
    f <- stats::rnorm(n)
    g <- .7 * f + sqrt(.51) * stats::rnorm(n)
    d <- data.frame(
      id = seq_len(n),
      a1 = .8 * f + stats::rnorm(n, 0, .6), a2 = .7 * f + stats::rnorm(n, 0, .7), a3 = .9 * f + stats::rnorm(n, 0, .5),
      b1 = .8 * g + stats::rnorm(n, 0, .6), b2 = .7 * g + stats::rnorm(n, 0, .7), b3 = .9 * g + stats::rnorm(n, 0, .5)
    )
    map <- data.frame(construct = "A", wave = rep(c("T1", "T2"), each = 3), item = rep(paste0("I", 1:3), 2),
      variable = names(d)[-1], wave_order = rep(1:2, each = 3))
    list(data = d, map = map)
  })
  s <- sieve_new_state()
  shiny::isolate({
    sieve_new_dataset(s)
    s$raw <- x$data
    s$items <- x$data[, -1]
    m <- sieve_build_meta(x$data, names(x$data)[-1], list(A = names(x$data)[-1]))
    m$type <- "continuous"
    s$meta <- m
    s$confirmed <- TRUE
  })
  csv <- paste(utils::capture.output(utils::write.csv(x$map, row.names = FALSE)), collapse = "\n")
  shiny::testServer(mod_longitudinal_server, args = list(state = s), {
    session$setInputs(mapping = csv, why = "Same wording at each wave", confirm = TRUE,
      level = "metric", residuals = "adjacent", run = 1)
    expect_true(sieve_has_solution(result()))
    expect_null(current()$error)
    expect_false(grepl("historical", rc_text(output$status), fixed = TRUE))
    sieve_set_case_status(state, c(5L, 6L), "excluded", "review test")
    session$flushReact()
    expect_identical(nrow(state$items), 298L)
    expect_match(current()$error, "historical", fixed = TRUE)
    expect_match(rc_text(output$status), "The longitudinal result is historical", fixed = TRUE)
    # Restored, the rung describes the data again.
    sieve_set_case_status(state, c(5L, 6L), "included", "review test")
    session$flushReact()
    expect_null(current()$error)
  })
})

test_that("the response-policy comparison is marked historical once respondents are removed", {
  s <- rc_hs9()
  shiny::testServer(mod_reliab_server, args = list(state = s), {
    session$setInputs(policy_construct = "Visual", policy_count = 2L)
    session$setInputs(policy_sensitivity = 1)
    before <- rc_text(output$policy_table)
    expect_match(before, "<th>scored", fixed = TRUE)
    rc_remove(state)
    session$flushReact()
    after <- rc_text(output$policy_table)
    expect_false(grepl("<th>scored", after, fixed = TRUE))
    expect_match(after, "<th>Note", fixed = TRUE)
  })
})

test_that("IRT diagnostics computed on request are withheld as historical after a removal", {
  s <- rc_hs9_ordered()
  shiny::testServer(mod_irt_server, args = list(state = s), {
    session$setInputs(scope = "__all__", metric = "normal_ogive", estimator = "WLSMV",
      theta_min = -3, theta_max = 3, theta_n = 31, info_threshold = 10)
    session$setInputs(fit_go = 1)
    expect_identical(demand$status()$state, "current")
    session$setInputs(information_draws = 100L, information_seed = 4L, information_bands_go = 1)
    session$setInputs(pair_item1 = "t1_visual_perception", pair_item2 = "t2_cubes", pair_go = 1)
    session$setInputs(trace_item = "t1_visual_perception", trace_bins = 3L, trace_go = 1)
    expect_false(grepl("historical", rc_text(output$information_bands_note), fixed = TRUE))
    expect_match(rc_text(output$pair_note), "t1_visual_perception versus t2_cubes", fixed = TRUE)
    expect_match(rc_text(output$trace_note), "Focal item", fixed = TRUE)
    rc_remove(state)
    session$flushReact()
    expect_identical(demand$status()$state, "stale")
    expect_match(rc_text(output$information_bands_note), "information band computed earlier is historical", fixed = TRUE)
    expect_match(rc_text(output$pair_note), "bivariate residual inspection computed earlier is historical", fixed = TRUE)
    expect_match(rc_text(output$trace_note), "category response inspection computed earlier is historical", fixed = TRUE)
    expect_error(output$information_bands_plot)
    # After a refit on the remaining rows the diagnostics are computed again.
    session$setInputs(fit_go = 2, pair_go = 2)
    expect_identical(demand$status()$state, "current")
    expect_match(rc_text(output$pair_note), "t1_visual_perception versus t2_cubes", fixed = TRUE)
  })
})

test_that("the structural diagram names the analysis it shows and records nothing after a removal", {
  s <- rc_hs9()
  shiny::testServer(mod_sem_server, args = list(state = s), {
    session$setInputs(outcomes = "Speed", predictors = c("Visual", "Textual"))
    session$setInputs(fit_go = 1)
    expect_identical(demand$status()$state, "current")
    fit_id <- path_run()$analysis_id
    expect_false(is.null(fit_id))
    expect_identical(diagram$spec()$meta$analysis_id, fit_id)
    recorded <- names(state$analyses)
    rc_remove(state)
    session$flushReact()
    expect_identical(demand$status()$state, "stale")
    # The diagram on screen is the fit from before the removal: it keeps that
    # fit's identifier, and no record bound to the new rows was made for it.
    expect_identical(diagram$spec()$meta$analysis_id, fit_id)
    expect_identical(names(state$analyses), recorded)
    catalog <- sieve_analysis_catalog(state)
    expect_true(all(catalog$revision == "earlier"))
    rc_restore(state)
    session$flushReact()
    expect_identical(names(state$analyses), recorded)
  })
})

# ---- A restore gives back the first results exactly --------------------------

test_that("Dimensionality: a restore gives back the seed and the parallel analysis of the first computation", {
  s <- rc_hs9()
  shiny::testServer(mod_retention_server, args = list(state = s), {
    session$setInputs(cor_type = "auto", criterion = "p95", n_iter = 20, max_k = 4)
    first_seed <- run_rng()$seed
    first <- pa_run()
    rc_remove(state)
    session$flushReact()
    expect_false(identical(run_rng()$seed, first_seed))
    rc_restore(state)
    session$flushReact()
    expect_identical(run_rng()$seed, first_seed)
    again <- pa_run()
    # Everything but the timing of the run.
    same <- setdiff(names(first), c("seconds", "iteration_seconds"))
    expect_identical(again[same], first[same])
    expect_identical(again$reference, first$reference)
  })
})

test_that("EFA: run again after a restore, the selected EFA gives back the first solution and its seed", {
  s <- rc_hs9()
  shiny::testServer(mod_efa_server, args = list(state = s), {
    session$setInputs(n_factors = 3L, fm = "ml", rotation = "oblimin", cor_type = "auto")
    session$setInputs(efa_go = 1)
    first <- efa_run()
    expect_false(is.null(first$fit))
    rc_remove(state)
    session$setInputs(efa_go = 2)
    expect_false(identical(efa_run()$rng$seed, first$rng$seed))
    rc_restore(state)
    session$setInputs(efa_go = 3)
    again <- efa_run()
    expect_identical(again$rng$seed, first$rng$seed)
    expect_identical(unclass(again$fit$loadings), unclass(first$fit$loadings))
    expect_identical(again$fit$Phi, first$fit$Phi)
  })
})

test_that("Items: after a restore the diagnostics equal the first ones, digit for digit", {
  s <- rc_hs9()
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    first <- diag()
    expect_false(is.null(first$items))
    expect_false(is.null(first$rng))
    rc_remove(state)
    session$flushReact()
    removed <- diag()
    expect_false(identical(removed$items$loading_target, first$items$loading_target))
    rc_restore(state)
    session$flushReact()
    again <- diag()
    expect_identical(again$items, first$items)
    expect_identical(again$constructs, first$constructs)
    expect_identical(again$rng, first$rng)
  })
  # Seeded, the diagnostics do not depend on the caller's random stream.
  d <- shiny::isolate(s$items)
  m <- shiny::isolate(s$meta)
  rng <- sieve_draw_seed(31L)
  a <- withr::with_seed(1, sieve_item_diagnostics(d, m, rng = rng))
  b <- withr::with_seed(2, sieve_item_diagnostics(d, m, rng = rng))
  expect_identical(a$items, b$items)
})

test_that("a restore of every respondent gives back the roles table and the ledger exactly, byte for byte", {
  s <- rc_hs9()
  roles0 <- shiny::isolate(s$roles)
  rc_remove(s)
  rc_restore(s)
  roles1 <- shiny::isolate(s$roles)
  expect_identical(roles1, roles0)
  expect_identical(sieve_hash(roles1), sieve_hash(roles0))
  # The whole input revision a session script binds, byte for byte.
  s <- rc_hs9()
  before <- sieve_replay_inputs(s)
  rc_remove(s)
  rc_restore(s)
  after <- sieve_replay_inputs(s)
  for (nm in names(before)) {
    expect_identical(serialize(after[[nm]], NULL), serialize(before[[nm]], NULL), info = nm)
  }
})

# ---- What a result from before says ------------------------------------------

test_that("the line over a result from before names the respondents removed and the n then and now", {
  s <- rc_hs9()
  shiny::testServer(function(input, output, session) {
    wire <- sieve_demand_wire(input, session, s, "run", "Review analysis",
      key = shiny::reactive(sieve_demand_key(s)),
      plan = shiny::reactive(list(seconds = 1, estimator = "MLR", p = 9)),
      compute = function(...) list(estimate = nrow(shiny::isolate(s$items)))
    )
  }, {
    session$setInputs(run = 1)
    expect_identical(wire$status()$state, "current")
    rc_remove(s)
    session$flushReact()
    st <- wire$status()
    expect_identical(st$state, "stale")
    expect_identical(
      st$sentence,
      "2 respondents removed from the analysis (rows 10, 300 of the data as loaded); n = 301 then, 299 now"
    )
    expect_match(rc_text(wire$line()), "What changed: 2 respondents removed from the analysis", fixed = TRUE)
    session$setInputs(run = 2)
    sieve_set_case_status(s, 300L, "included", "review test")
    session$flushReact()
    expect_identical(
      wire$status()$sentence,
      "1 respondent restored to the analysis (row 300 of the data as loaded); n = 299 then, 300 now"
    )
    # A reversal after a removal is named with the respondents.
    session$setInputs(run = 3)
    sieve_set_case_status(s, 20L, "excluded", "review test")
    sieve_apply_reverse(s, "t2_cubes", lo = 0, hi = 40)
    session$flushReact()
    expect_identical(
      wire$status()$sentence,
      "t2_cubes reverse scored; 1 respondent removed from the analysis (row 20 of the data as loaded); n = 300 then, 299 now"
    )
  })
  # The message of the results bound with sieve_bind_result() names both causes.
  a <- shiny::isolate(sieve_revision(s))
  sieve_set_case_status(s, 21L, "excluded", "review test")
  expect_match(sieve_stale_reason(a, shiny::isolate(sieve_revision(s))),
    "respondents were removed from or restored to the analysis", fixed = TRUE)
})

test_that("the methods draft is withheld, and not downloaded, once respondents are removed", {
  s <- rc_hs9()
  shiny::testServer(mod_repro_server, args = list(state = s), {
    session$setInputs(draft_go = 1)
    d <- draft()
    expect_match(d, "301 respondents on 9 items", fixed = TRUE)
    expect_identical(output$draft, d)
    rc_remove(state)
    session$flushReact()
    shown <- output$draft
    expect_false(grepl("301 respondents on 9 items", shown, fixed = TRUE))
    expect_match(shown, "The draft assembled earlier is not shown", fixed = TRUE)
    # The button is named "Run a new analysis and draft" (the report from
    # recorded analyses leads the page, and the draft that runs a new
    # analysis says so); the draft is withheld all the same.
    expect_match(shown, "Run a new analysis and draft", fixed = TRUE)
    expect_error(output$dl_draft, "Press Run a new analysis and draft first", fixed = TRUE)
    session$setInputs(draft_go = 2)
    expect_match(output$draft, "299 respondents on 9 items", fixed = TRUE)
  })
})

test_that("the EFA's waiting panels name every reason the selected EFA waits, not only an opened project", {
  s <- rc_hs9()
  shiny::testServer(mod_efa_server, args = list(state = s), {
    session$setInputs(n_factors = 3L, fm = "ml", rotation = "oblimin", cor_type = "auto")
    session$setInputs(efa_go = 1)
    rc_remove(state)
    session$flushReact()
    # The status line says the data changed (respondents were removed), not
    # that a project was opened; the number of factors panel says nothing
    # about restored settings.
    line <- rc_text(output$status_line)
    expect_false(grepl("Opening a project", line, fixed = TRUE))
    expect_false(grepl("This project was reopened", line, fixed = TRUE))
    expect_match(line, "The data, the item pool or the EFA settings changed since the loadings were computed", fixed = TRUE)
    expect_false(grepl("restored settings", rc_text(output$count_note), fixed = TRUE))
  })
})

test_that("a saved report says whether each record describes the data now in the analysis", {
  s <- rc_hs9()
  fit <- sieve_fit_cfa(shiny::isolate(s$items), shiny::isolate(s$meta))
  id <- sieve_register_analysis(s, list(
    spec = list(kind = "cfa", estimator = "MLR"), fit = fit,
    status = sieve_fit_status(fit), warnings = character()
  ), note = "CFA before the removal")
  expect_match(sieve_readable_saved_report(s, id),
    "Inputs: the data and the item pool now in the analysis (current).", fixed = TRUE)
  rc_remove(s)
  report <- sieve_readable_saved_report(s, id)
  expect_match(report, "Inputs: earlier than those now in the analysis", fixed = TRUE)
  expect_match(report, "respondents were removed from or restored to the analysis", fixed = TRUE)
  expect_match(report, "input rows 301 then, 299 now", fixed = TRUE)
  expect_identical(sieve_analysis_catalog(s)$revision, "earlier")
  expect_match(sieve_saved_analyses_report(s, id), "input rows 301 then, 299 now", fixed = TRUE)
  rc_restore(s)
  expect_match(sieve_readable_saved_report(s, id), "(current)", fixed = TRUE)
})

# ---- Reverse scoring and removed respondents -----------------------------------

rc_keyed <- function(st, it = "t2_cubes") {
  m <- sieve_meta_keying(shiny::isolate(st$meta))
  isTRUE(m$reverse[m$item == it])
}

test_that("a reversal made in the item grid is taken back after respondents are removed, and after they return", {
  # The bounds are declared in the item's Response bounds cell, and the
  # reversal is made and taken back with its Reverse? cell (or the Reverse?
  # row above the grid), where No reflects the item again within the same
  # bounds. The undo records are tested through the service in the next
  # block.
  s <- rc_hs9()
  items0 <- shiny::isolate(s$items)
  visual <- c("t1_visual_perception", "t2_cubes", "t4_lozenges")
  shiny::testServer(mod_ingest_server, args = list(state = s), {
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "bounds", value = "0 to 40"))
    m <- sieve_meta_keying(state$meta)
    at <- which(m$item == "t2_cubes")
    expect_identical(c(m$rev_lo[at], m$rev_hi[at]), c(0, 40))
    expect_identical(m$rev_bounds[at], "declared")
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "Yes"))
    expect_true(rc_keyed(state))
    expect_identical(as.numeric(state$items$t2_cubes), 40 - as.numeric(items0$t2_cubes))
    # Respondents removed after the reversal: Reverse? No still takes it
    # back, on the rows in the analysis, to the values as loaded.
    rc_remove(state)
    session$setInputs(cell_edit = list(item = "t2_cubes", field = "reverse", value = "No"))
    expect_false(rc_keyed(state))
    keep <- sieve_case_index(state)
    expect_identical(sieve_values_key(state$items$t2_cubes), sieve_values_key(items0$t2_cubes[keep]))
    # The respondents return with their values as loaded.
    rc_restore(state)
    expect_identical(sieve_values_key(state$items$t2_cubes), sieve_values_key(items0$t2_cubes))
    # Reversed again, now with the Reverse? row for the construct (t2_cubes
    # within its declared bounds, the other two within their observed
    # ranges), respondents removed and restored, then taken back by the row.
    session$setInputs(types_scope = "construct:Visual", keying_to = "Yes")
    session$setInputs(keying_apply = 1)
    expect_true(all(vapply(visual, function(it) rc_keyed(state, it), logical(1))))
    rc_remove(state)
    rc_restore(state)
    session$setInputs(keying_to = "No")
    session$setInputs(keying_apply = 2)
    expect_false(any(vapply(visual, function(it) rc_keyed(state, it), logical(1))))
    for (it in visual) {
      expect_identical(sieve_values_key(state$items[[it]]), sieve_values_key(items0[[it]]))
    }
  })
})

# ---- Recomputed results against direct computations ---------------------------

test_that("after a removal the recomputed results equal lavaan, psych, DMAR and base R on the remaining rows", {
  s <- rc_hs9()
  items0 <- shiny::isolate(s$items)
  meta0 <- shiny::isolate(s$meta)
  rc_remove(s)
  # And one more by its row of the data as loaded (row 20, missing one response).
  expect_identical(sieve_set_case_status(s, 20L, "excluded", "by row")$removed, 20L)
  keep <- setdiff(seq_len(nrow(items0)), c(10L, 20L, 300L))
  reduced <- items0[keep, , drop = FALSE]
  expect_identical(shiny::isolate(sieve_case_index(s)), keep)
  # Screening: descriptives, the correlation basis and the item-total panel.
  shiny::testServer(mod_screen_server, args = list(state = s), {
    d <- desc()
    expect_equal(as.numeric(d$n), unname(colSums(!is.na(reduced[, d$Item]))))
    expect_equal(d$mean, unname(colMeans(reduced[, d$Item], na.rm = TRUE)), tolerance = 1e-12)
    expect_equal(d$sd, unname(apply(reduced[, d$Item], 2, stats::sd, na.rm = TRUE)), tolerance = 1e-12)
    b <- basis()
    cc <- reduced[stats::complete.cases(reduced), , drop = FALSE]
    expect_identical(b$n, nrow(cc))
    expected_R <- if (identical(b$missing, "fiml")) stats::cov2cor(DMAR::cov_fiml(reduced[, rownames(b$R)])$S) else stats::cor(cc[, rownames(b$R)])
    expect_equal(unname(b$R), unname(expected_R), tolerance = 1e-12)
  })
  # CFA (the study's estimator, ML with FIML): the fit on the remaining rows
  # against lavaan.
  shiny::testServer(mod_cfa_server, args = list(state = s), {
    session$setInputs(scope = "construct", structure = "first", ident = "varone", estimator = "study",
      layout = "tree2", what = "std", min_path = 0, resid = TRUE, covs = TRUE)
    session$setInputs(fit_go = 1)
    run <- demand$result()
    spec <- run$spec
    direct <- lavaan::cfa(spec$model, data = reduced, std.lv = TRUE, estimator = spec$estimator, missing = spec$missing)
    expect_identical(lavaan::lavInspect(run$fit, "nobs"), lavaan::lavInspect(direct, "nobs"))
    expect_identical(lavaan::lavInspect(run$fit, "nobs"), 298L)
    expect_equal(lavaan::coef(run$fit), lavaan::coef(direct), tolerance = 1e-8)
    want <- c("chisq.scaled", "cfi.robust", "rmsea.robust", "srmr")
    expect_equal(unname(lavaan::fitMeasures(run$fit, want)), unname(lavaan::fitMeasures(direct, want)), tolerance = 1e-8)
  })
  # Reliability: omega and its interval against the DMAR call it records.
  shiny::testServer(mod_reliab_server, args = list(state = s), {
    session$setInputs(denominator = "model_implied", ci_method = "mlr", missing = "fiml",
      conf_level = .95, show_alpha = FALSE)
    session$setInputs(run_go = 1)
    res <- demand$result()
    for (cn in names(res)) {
      spec <- res[[cn]]$spec
      call <- sieve_reliability_call(spec)
      direct <- do.call(getExportedValue("DMAR", call$fun), c(list(data = reduced[, spec$items, drop = FALSE]), call$args))
      got <- res[[cn]]$omega
      v <- function(term) direct$value[direct$term == term]
      expect_equal(got$estimate, v("estimate"), tolerance = 1e-8, info = cn)
      expect_equal(c(got$lower, got$upper), c(v("ci_lower"), v("ci_upper")), tolerance = 1e-8, info = cn)
      expect_identical(as.integer(got$n$used), nrow(reduced), info = cn)
    }
  })
  # EFA: the solution against DMAR::fa_exploratory on the remaining rows
  # with the seed it records (the EFA page's engine).
  shiny::testServer(mod_efa_server, args = list(state = s), {
    session$setInputs(n_factors = 3L, fm = "ml", rotation = "oblimin", cor_type = "auto")
    session$setInputs(efa_go = 1)
    r <- efa_run()
    items <- r$fit$items
    X <- reduced[, items, drop = FALSE]
    expect_identical(as.integer(r$fit$method$N), sum(rowSums(!is.na(X)) > 0L))
    direct <- DMAR::fa_exploratory(X, nfactors = 3, estimator = r$route$estimator, missing = r$route$missing,
      rotation = "oblimin", seed = r$rng$seed)
    expect_equal(r$fit$loadings, sieve_efa_dmar_solution(direct)$loadings, tolerance = 1e-8)
  })
})

test_that("a restore after the analyses returns the data, the ledger summaries and the keys of the first state", {
  s <- rc_hs9()
  raw0 <- shiny::isolate(s$raw)
  items0 <- shiny::isolate(s$items)
  rev0 <- shiny::isolate(sieve_revision(s))
  page0 <- shiny::isolate(sieve_page_revision(s))
  rc_remove(s)
  expect_false(sieve_same_revision(rev0, shiny::isolate(sieve_revision(s))))
  rc_restore(s)
  expect_identical(shiny::isolate(s$raw), raw0)
  expect_identical(shiny::isolate(s$items), items0)
  expect_true(sieve_same_revision(rev0, shiny::isolate(sieve_revision(s))))
  expect_identical(shiny::isolate(sieve_page_revision(s)), page0)
  expect_null(shiny::isolate(s$case_index))
})

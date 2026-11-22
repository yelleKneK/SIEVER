# The Dimensionality and EFA tabs, and the names of the reliability
# coefficients: the Kaiser line is solid; a worker is explained wherever
# the word is shown, and the number of workers is a control; the EFA tab
# fits the number of factors asked for, with the fit indices for that k
# only; omega and alpha are named "Coefficient Omega" and "Coefficient
# Alpha" with their symbols; and each tab's estimation choices are controls
# of their own, said in words next to what they produce.
#
# Numbers are checked against direct psych and lavaan calls, not against
# the package's own intermediate values.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("psych")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")
withr::local_options(sieve.cores = 2L) # at most two workers, whatever the cores

ns_text <- function(x) {
  if (is.null(x)) return("")
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}
ns_html <- function(x) paste(as.character(x), collapse = "")

# The bundled nine tests (three constructs), loaded and confirmed.
ns_nine <- function(estimator_choice = "auto") {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = TRUE, name = fx$name, configure = function(s) {
  s$estimator_choice <- estimator_choice
  })
  s
}

# Six items in five ordered categories, two factors, n = 300, typed ordered.
ns_ordinal_state <- function(estimator_choice = "auto") {
  d <- withr::with_seed(20261015, {
    n <- 300L
    f1 <- stats::rnorm(n)
    f2 <- 0.3 * f1 + sqrt(1 - 0.09) * stats::rnorm(n)
    it <- function(f, l) l * f + sqrt(1 - l^2) * stats::rnorm(n)
    X <- data.frame(
      a1 = it(f1, .80), a2 = it(f1, .75), a3 = it(f1, .70),
      b1 = it(f2, .80), b2 = it(f2, .75), b3 = it(f2, .70)
    )
    as.data.frame(lapply(X, function(v) as.integer(cut(v, c(-Inf, -1, -0.2, 0.5, 1.3, Inf)))))
  })
  fixture <- list(data = d, item_cols = names(d),
    theory_map = list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3")),
    name = "Ordered fixture", labels = NULL, load_code = character())
  s <- siever_test_study(fixture, labels = FALSE, theory = FALSE, name = NULL, configure = function(s) {
    m <- s$meta
    m$type <- "ordered"
    s$meta <- m
    s$roles <- NULL
    s$estimator_choice <- estimator_choice
  })
  s
}

ns_retention_inputs <- function(session, ...) {
  defaults <- list(
    cor_type = "auto", criterion = "p95", pa_reference = "auto",
    n_iter = 20, max_k = 4, seed = 7, workers = 2, estimator = "study"
  )
  do.call(session$setInputs, utils::modifyList(defaults, list(...)))
}

# ---- The Kaiser criterion line is solid -------------------------------------

test_that("the Kaiser criterion line at 1 is solid and gray, thinner than the series, in both figures and in the recorded R", {
  # Plot/replay or timing mechanics at reduced B, not reported inference.
  siever_local_dmar_mechanics()
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols, drop = FALSE]
  b <- sieve_retention_cor(d, rep("continuous", 9L), "pearson")
  v <- sieve_scree_values(b)
  gg <- sieve_scree_plot(v)
  hline <- Filter(function(l) inherits(l$geom, "GeomHline"), gg$layers)
  expect_length(hline, 1L)
  expect_identical(hline[[1]]$aes_params$linetype, "solid")
  line_layer <- Filter(function(l) inherits(l$geom, "GeomLine"), gg$layers)[[1]]
  expect_lt(hline[[1]]$aes_params$linewidth, line_layer$aes_params$linewidth)
  built <- ggplot2::ggplot_build(gg)
  expect_identical(as.character(built$data[[1]]$linetype), "solid")
  expect_identical(built$data[[1]]$yintercept, 1)
  # Gray: the three channels equal within a few units.
  rgb <- grDevices::col2rgb(built$data[[1]]$colour)
  expect_lt(diff(range(rgb)), 20)
  labels <- unlist(lapply(gg$layers, function(l) l$aes_params$label))
  expect_true("Kaiser criterion (eigenvalue 1)" %in% labels)
  expect_match(gg$labels$subtitle, "The solid gray line at 1 is the Kaiser criterion", fixed = TRUE)
  expect_false(grepl("dotted", gg$labels$subtitle, fixed = TRUE))
  expect_match(paste(sieve_scree_code(v), collapse = "\n"), 'abline(h = 1, lty = 1, col = "gray50")', fixed = TRUE)

  emp <- c(2.7, 0.9, 0.45, 0.01, -0.02)
  ref <- c(0.6, 0.4, 0.25, 0.1, 0.0)
  pg <- sieve_pa_plot(emp, ref, "95th percentile of 100 simulated normal datasets", "pearson")
  ph <- Filter(function(l) inherits(l$geom, "GeomHline"), pg$layers)
  expect_identical(ph[[1]]$aes_params$linetype, "solid")
  expect_lt(ph[[1]]$aes_params$linewidth, 0.8)
  expect_match(gsub("\\s+", " ", pg$labels$subtitle), "Solid gray line at 1: orientation only", fixed = TRUE)

  # The parallel analysis code draws its figure with the same solid line,
  # and replays.
  pa <- sieve_parallel_analysis(d, b, n_iter = 10, seed = 3)
  sel <- sieve_pa_select(pa, "p95", "auto")
  code <- sieve_pa_code(pa, sel)
  expect_true(any(grepl('abline(h = 1, lty = 1, col = "gray50")', code, fixed = TRUE)))
  env <- new.env(parent = globalenv())
  env$active_items <- d
  env$R_items <- b$R
  env$complete_rows <- stats::complete.cases(d)
  env$n_complete <- sum(env$complete_rows)
  grDevices::pdf(NULL)
  withr::with_preserve_seed(eval(parse(text = code), envir = env))
  grDevices::dev.off()
  expect_identical(as.integer(env$n_factors), as.integer(sel$suggested))
})

# ---- What a worker is, and a Workers control --------------------------------

test_that("the wait of a parallel analysis that has not run names DMAR, its reference datasets and the background process in plain words", {
  # Plot/replay or timing mechanics at reduced B, not reported inference.
  siever_local_dmar_mechanics()
  # The parallel analysis is DMAR::parallel_analysis(), one call per kind of
  # reference data in one process, a long one in a background R process;
  # the workers do not apply to it. They are for the model fit by k.
  detected <- parallel::detectCores()
  b <- list(ok = TRUE, used = "polychoric", requested = "polychoric", p = 68L, n = 500L, seconds = 4)
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  plan <- sieve_pa_plan(b, 100)
  expect_identical(plan$cores, 1L)
  # This test checks the plan's arithmetic and that the cases count, not
  # the rate formula itself; the rates are checked against measured runs in
  # the parallel analysis tests. 100 datasets for each of two kinds of
  # reference data, each call's observed matrix, and the worker's start:
  per <- plan$per_dataset
  expect_identical(plan$draws, 200L)
  expect_equal(plan$compute, (200 + 2) * per, tolerance = 1e-12)
  expect_equal(plan$seconds, plan$compute + sieve_pa_start_seconds(), tolerance = 1e-12)
  expect_gt(sieve_pa_plan(utils::modifyList(b, list(n = 5000L)), 100)$per_dataset, per)
  said <- sieve_pa_wait_sentence(plan)
  expect_identical(said, sprintf(paste0(
    "Parallel analysis has not run yet. DMAR::parallel_analysis() would draw 100 reference datasets for each of ",
    "its two kinds of reference data (200 in all); at %s, the estimated wait is %s, in a ",
    "background R process (its start included) while the page stays usable. Press Run parallel analysis to start it."
  ), if (per < 1) sprintf("about %.3f seconds each", per) else sprintf("about %.0f seconds each", per),
  sieve_duration_label(plan$seconds)))
  nr <- sieve_pa_not_run(b, 100, sieve_draw_seed(1), plan)
  expect_identical(nr$reason, said)
  expect_false(grepl("Not run yet:", nr$reason, fixed = TRUE))
  if (length(detected) == 1L && is.finite(detected) && detected >= 1L) {
    expect_match(sieve_workers_help(),
      sprintf(
        "This computer has %d cores; the default is all but one, at most 8.",
        detected
      ),
      fixed = TRUE
    )
  } else {
    expect_match(sieve_workers_help(), "The default is all of this computer's cores but one, at most 8.", fixed = TRUE)
  }
  expect_false(grepl("NA cores", sieve_workers_help(), fixed = TRUE))
  expect_match(sieve_workers_help(), "fit the exploratory models of a requested model fit by k", fixed = TRUE)
  # A short parallel analysis runs in the session, so the help does not say
  # it always runs in a background process.
  expect_match(sieve_workers_help(), paste(
    "Parallel analysis does not use them: a run estimated above two seconds runs in one background R process",
    "of its own, and a shorter one in this session."
  ), fixed = TRUE)
  # In the evidence statements the wait follows the criterion's name.
  vss <- list(ok = FALSE, reason = "not computed for this test")
  b6 <- sieve_retention_cor(shiny::isolate(ns_ordinal_state()$items), rep("ordered", 6), "polychoric")
  cr <- sieve_retention_criteria(
    b6, sieve_pa_not_run(b6, 100, sieve_draw_seed(1), sieve_pa_plan(b6, 100)),
    vss, "p95", "auto", TRUE
  )
  st <- sieve_retention_statements(cr, sieve_retention_summary(cr))
  expect_match(
    st$unavailable,
    "^Not run yet, and not in the tally until it is: parallel analysis\\. Parallel analysis has not run yet\\. DMAR::parallel_analysis\\(\\) would draw 100 reference datasets"
  )
})

test_that("the Workers control belongs to the model fit by k, and the parallel analysis says where it runs and why polychoric runs take longer", {
  # Plot/replay or timing mechanics at reduced B, not reported inference.
  siever_local_dmar_mechanics()
  # The parallel analysis does not run on the workers, so its plan does not
  # depend on them.
  st <- ns_ordinal_state()
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  local_mocked_bindings(
    # 30 seconds per reference dataset: the run waits for its button.
    sieve_pa_units = function(...) 1000,
    withProgress = function(expr, ...) expr
  )
  ui <- gsub("\\s+", " ", ns_html(mod_retention_ui("ret")))
  expect_match(ui, 'id="ret-workers"', fixed = TRUE)
  expect_match(ui, "Workers (model fit by k)", fixed = TRUE)
  expect_match(ui, "Workers are separate R processes, each on its own processor core, that fit the exploratory models", fixed = TRUE)
  expect_match(ui, "with ordered items the polychoric correlations of every reference dataset are estimated", fixed = TRUE)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    ns_retention_inputs(session, workers = 2, max_k = 3)
    plan2 <- pa_plan()
    expect_identical(plan2$cores, 1L)
    expect_equal(plan2$per_dataset, 30, tolerance = 1e-12)
    # 20 datasets for each of two references and each call's observed matrix.
    expect_equal(plan2$seconds, (40 + 2) * 30 + 2, tolerance = 1e-12)
    run <- pa_run()
    expect_identical(run$status, "not run")
    expect_match(run$reason, "in a background R process (its start included)", fixed = TRUE)
    ctrl <- ns_text(output$pa_control)
    expect_match(ctrl,
      "Parallel analysis has not run yet. DMAR::parallel_analysis() would draw 20 reference datasets for each of its two kinds of reference data (40 in all); at about 30 seconds each",
      fixed = TRUE
    )
    expect_match(ctrl, "a run estimated above two seconds goes to a background R process, so the page stays usable, and Cancel stops it", fixed = TRUE)
    expect_match(ctrl,
      "the wait is longer here because the polychoric correlations of every reference dataset are estimated",
      fixed = TRUE
    )
    # The plot area says the same thing once, not "did not run. Not run yet".
    expect_error(output$scree, "^Parallel analysis has not run yet\\.")
    session$setInputs(workers = 1)
    expect_identical(pa_plan()$seconds, plan2$seconds)
    # A number above what the machine allows is held at the largest allowed.
    session$setInputs(workers = 500)
    expect_identical(workers_now(), sieve_cores(max = 64L))
  })
})

# ---- The EFA tab fits the number of factors asked for -----------------------

test_that("the EFA tab fits the selected k, its fit line is the same model's, and range inputs request nothing", {
  html <- ns_html(mod_efa_ui("efa"))
  tabs <- regmatches(html, gregexpr('data-value="[^"]+"', html))[[1]]
  tabs <- gsub('data-value="|"', "", tabs)
  tabs <- tabs[tabs %in% c(
    "Pattern matrix", "Structure matrix", "Factor correlations (Phi)",
    "Communalities and Simple Structure", "Variance accounted", "Determinacy and H",
    "Fit indices", "Number of factors"
  )]
  expect_identical(tabs[1], "Pattern matrix")
  expect_identical(tabs[length(tabs)], "Number of factors")
  expect_false("Fit indices" %in% tabs)
  for (id in c("efa-fit_scope", "efa-fit_from", "efa-fit_to")) {
    expect_false(grepl(sprintf('id="%s"', id), html, fixed = TRUE))
  }
  expect_match(html, "DMAR::factor_count", fixed = TRUE)
  expect_false(grepl("a range of k", html, fixed = TRUE))

  st <- ns_nine()
  X <- shiny::isolate(st$items)
  calls <- new.env()
  calls$n <- 0L
  real <- DMAR::fa_exploratory
  local_mocked_bindings(fa_exploratory = function(...) {
    calls$n <- calls$n + 1L
    real(...)
  }, .package = "DMAR")
  local_mocked_bindings(withProgress = function(expr, ...) expr)
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(
      n_factors = 3, rotation = "oblimin", suppress = 0.3, sort_loadings = TRUE, efa_go = 1
    )
    expect_identical(calls$n, 1L)
    # The fit line is the selected solution's own fit: DMAR's fit measures of
    # the unrotated model (no row is incomplete, so FIML uses every case),
    # with the RMSEA's interval at the RMSEA level, 90% (DMAR's default
    # rmsea_conf_level, here called independently), for the study's default
    # for continuous items, DMAR's ML with FIML.
    fm <- attr(DMAR::fa_exploratory(X, nfactors = 3, estimator = "ML", missing = "fiml", rotation = "none"), "fit_measures")
    calls$n <- 1L
    v <- function(t) fm$value[fm$term == t]
    expect_equal(efa()$fit_measures$value, fm$value, tolerance = 1e-8)
    expect_match(ns_text(output$efa_fit_line),
      sprintf("RMSEA = %.3f, 90%% CI [%.3f, %.3f]; CFI = %.3f; TLI = %.3f; SRMR = %.3f.",
        v("rmsea"), v("rmsea_ci_lower"), v("rmsea_ci_upper"), v("cfi"), v("tli"), v("srmr")),
      fixed = TRUE
    )
    # Range inputs the tab does not offer request no extra models.
    session$setInputs(fit_scope = "range", fit_from = 2, fit_to = 4)
    expect_identical(calls$n, 1L)
    expect_identical(ncol(efa()$loadings), 3L)
    # A k beyond what an exploratory model of nine items identifies is
    # refused, not fit and not replaced.
    session$setInputs(n_factors = 6, efa_go = 2)
    expect_match(ns_text(output$status_line), "requested k = 6", fixed = TRUE)
    expect_null(efa())
    expect_identical(calls$n, 1L)
  })
})

test_that("a selected fit estimated above two seconds waits for its button with its estimate and runs in the background", {
  st <- ns_nine()
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(
    sieve_efa_dmar_wait = function(p, estimator, n, k, form = "data", ...) 30 * k,
    sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE),
    withProgress = function(expr, ...) expr
  )
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(n_factors = 3, rotation = "oblimin", suppress = 0.3, sort_loadings = TRUE)
    expect_true(isTRUE(efa_run()$pending))
    line <- ns_text(output$status_line)
    expect_match(line, sprintf("Run selected EFA (%s)", sieve_efa_wait_label(90)), fixed = TRUE)
    expect_length(log$calls, 0L)
    session$setInputs(efa_go = 1)
    expect_true(isTRUE(efa_run()$running))
    log$process$alive <- FALSE
    efa_dmar_settle(session)
    expect_identical(ncol(efa()$loadings), 3L)
    expect_length(log$calls, 1L)
    # Range inputs the tab does not offer change nothing and start nothing.
    kept <- efa_run()
    session$setInputs(fit_scope = "range", fit_from = 1, fit_to = 5)
    expect_identical(efa_run(), kept)
    expect_length(log$calls, 1L)
  })
})

# ---- The estimation choices of the Dimensionality and EFA tabs --------------

test_that("the Dimensionality tab's Correlations control offers the study's setting first and says what is in force and where it was set", {
  html <- gsub("\\s+", " ", ns_html(mod_retention_ui("ret")))
  expect_match(html, "<option value=\"auto\" selected>The study's setting (Ingest tab)</option>", fixed = TRUE)
  expect_match(html, '<option value="pearson">Pearson</option>', fixed = TRUE)
  expect_match(html, '<option value="poly">Polychoric or tetrachoric</option>', fixed = TRUE)
  expect_match(html, 'id="ret-cor_source"', fixed = TRUE)
  expect_match(html, 'id="ret-estimator"', fixed = TRUE)
  expect_match(html, "Estimator for the model fit by k", fixed = TRUE)
  expect_match(html,
    "<option value=\"study\" selected>The study's estimator (set on the Ingest tab)</option>",
    fixed = TRUE
  )

  # Six ordered items: the study's setting follows the types (polychoric)
  # until the Ingest tab treats the items as continuous (Pearson).
  st <- ns_ordinal_state()
  local_mocked_bindings(withProgress = function(expr, ...) expr)
  shiny::testServer(mod_retention_server, args = list(state = st), {
    ns_retention_inputs(session, max_k = 3, n_iter = 10)
    expect_identical(basis_request(), "polychoric")
    expect_identical(basis()$used, "polychoric")
    expect_identical(
      ns_text(output$cor_source),
      " In force here: polychoric correlations, the study's setting on the Ingest tab, which follows the confirmed item types. "
    )
    expect_identical(
      sieve_study_basis_label(state, rep("ordered", 6)),
      "The study's setting (Ingest tab): polychoric correlations"
    )
    state$estimator_choice <- "mlr"
    session$flushReact()
    expect_identical(basis_request(), "pearson")
    expect_identical(basis()$used, "pearson")
    expect_match(ns_text(output$cor_source),
      "In force here: Pearson correlations, the study's setting on the Ingest tab, which treats the items as continuous (its Estimator control is set to MLR).",
      fixed = TRUE
    )
    expect_identical(
      sieve_study_basis_label(state, rep("ordered", 6)),
      "The study's setting (Ingest tab): Pearson correlations"
    )
    session$setInputs(cor_type = "poly")
    expect_identical(basis_request(), "polychoric")
    expect_match(ns_text(output$cor_source),
      "In force here: polychoric correlations, chosen on this tab.",
      fixed = TRUE
    )
  })
})

test_that("the Dimensionality tab's model fit by k is fit by lavaan::efa under the estimator in force, the study's or the panel's", {
  st <- ns_nine()
  X <- shiny::isolate(st$items)
  # The fits run by themselves here whatever an earlier test measured
  # (three solutions of 0.5 seconds: within the run policy's two seconds).
  local_mocked_bindings(
    withProgress = function(expr, ...) expr,
    sieve_fit_wait = function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L, ...) 0.5
  )
  direct <- function(k, estimator, missing, family) {
    f <- suppressWarnings(lavaan::efa(
      data = X, nfactors = k, rotation = "none", estimator = estimator,
      missing = missing
    ))
    f <- if (inherits(f, "efaList")) f[[1]] else f
    sfx <- if (family == "standard") "" else paste0(".", family)
    unname(suppressWarnings(lavaan::fitMeasures(f, paste0(c("cfi", "tli", "rmsea"), sfx))))
  }
  shiny::testServer(mod_retention_server, args = list(state = st), {
    ns_retention_inputs(session, max_k = 3, n_iter = 10, estimator = "study")
    f <- fit_run()
    expect_identical(f$engine, "lavaan")
    # The item types, every item continuous: ML with FIML (DMAR's default).
    expect_identical(f$estimator, "ML")
    expect_identical(f$missing, "fiml")
    expect_identical(f$lavaan_missing, "listwise") # no incomplete row: FIML is the complete cases
    expect_identical(f$table$k, 1:3)
    want <- direct(3, "ML", "listwise", f$family)
    expect_equal(c(f$table$cfi[3], f$table$tli[3], f$table$rmsea[3]), want, tolerance = 1e-8)
    expect_match(ns_text(output$estimator_source),
      "In force here: the estimator the item types imply, the study's setting on the Ingest tab. For these items: ML with FIML.",
      fixed = TRUE
    )
    expect_match(ns_text(output$fit_note), "lavaan::efa at each k under ML with FIML", fixed = TRUE)
    # The model fit by k is beside the criteria, not one of them.
    expect_false("fit" %in% criteria()$id)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log,
      'lavaan::efa(data = active_items, nfactors = k, rotation = "none", estimator = "ML", missing = "listwise")',
      fixed = TRUE
    )
    expect_match(ns_text(output$fit_note),
      "every case, all complete, so FIML reduces to the complete cases",
      fixed = TRUE
    )
    # The panel's own choice: ML on the complete cases. Under the run policy
    # the sequence fit earlier is stale and the new one waits for Run the
    # model fit by k.
    session$setInputs(estimator = "ml")
    expect_identical(fit_run()$status, "not run")
    expect_true(isTRUE(fit_run()$stale))
    # The sequence as the button's run computes it (here in this process
    # rather than in background workers).
    fit_store(list(key = fit_key(), value = run_fit(fit_key(), fit_plan())))
    g <- fit_run()
    expect_identical(g$estimator, "ML")
    expect_identical(g$family, "standard")
    expect_equal(c(g$table$cfi[3], g$table$tli[3], g$table$rmsea[3]), direct(3, "ML", "listwise", "standard"),
      tolerance = 1e-8
    )
    expect_match(ns_text(output$estimator_source),
      "In force here: ML on the complete cases (items as continuous), chosen on this tab.",
      fixed = TRUE
    )
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log,
      'lavaan::efa(data = active_items, nfactors = k, rotation = "none", estimator = "ML", missing = "listwise")',
      fixed = TRUE
    )
    # The study's setting moves the "study" choice and not the panel's own.
    state$estimator_choice <- "ml"
    session$setInputs(estimator = "study")
    expect_identical(fit_run()$estimator, "ML")
    expect_match(ns_text(output$estimator_source),
      "In force here: ML on the complete cases (items as continuous), the study's setting on the Ingest tab.",
      fixed = TRUE
    )
  })

  # Ordered items under WLSMV: every item enters as ordered.
  so <- ns_ordinal_state()
  O <- shiny::isolate(so$items)
  shiny::testServer(mod_retention_server, args = list(state = so), {
    ns_retention_inputs(session, max_k = 2, n_iter = 10, estimator = "wlsmv")
    f <- fit_run()
    expect_identical(f$estimator, "WLSMV")
    expect_setequal(f$ordered, names(O))
    ref <- suppressWarnings(lavaan::efa(
      data = O, nfactors = 2, rotation = "none", estimator = "WLSMV",
      ordered = names(O)
    ))
    ref <- if (inherits(ref, "efaList")) ref[[1]] else ref
    sfx <- if (f$family == "standard") "" else paste0(".", f$family)
    expect_equal(f$table$cfi[2], unname(lavaan::fitMeasures(ref, paste0("cfi", sfx))), tolerance = 1e-8)
    expect_identical(f$basis_used, "polychoric")
    expect_false("fit" %in% criteria()$id)
  })
})

test_that("a model fit by k estimated above ten seconds waits for its button on its panel, outside the evidence table", {
  st <- ns_nine()
  # A requested sequence runs in background workers, split over the Workers
  # set on the tab, and the estimate stated is that plan's. The workers here
  # fit in this process.
  started <- list()
  local_mocked_bindings(
    sieve_fit_wait = function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L, ...) 20 * k,
    withProgress = function(expr, ...) expr,
    sieve_efa_worker_start = function(data, key, k) {
      started[[length(started) + 1L]] <<- k
      p <- new.env(parent = emptyenv())
      p$alive <- TRUE
      p$is_alive <- function() p$alive
      p$kill <- function() invisible(TRUE)
      p$get_result <- function() lapply(k, function(kk) sieve_efa_fit_by_k(data, key$estimator, key$ordered,
        rotation = key$rotation, ks = kk, missing = key$missing
      ))
      p
    }
  )
  shiny::testServer(mod_retention_server, args = list(state = st), {
    ns_retention_inputs(session, max_k = 3, n_iter = 200L)
    f <- fit_run()
    expect_identical(f$status, "not run")
    expect_match(f$reason,
      sprintf(
        paste0(
          "The model fit by k has not run yet. It would fit 3 exploratory models (k = 1 to 3) to 9 items with ",
          "lavaan::efa under ML with FIML; the estimated wait is %s on 2 background workers at once"
        ),
        sieve_duration_label(sieve_efa_plan_wait(9, "ML", 301, 1:3, 2L))
      ),
      fixed = TRUE
    )
    expect_match(ns_html(output$fit_control), '-fit_go"', fixed = TRUE)
    # The model fit by k is not a criterion, so the evidence table has no
    # row or button for it and the tally does not wait for it; the status
    # line says where its button is.
    expect_false("fit" %in% criteria()$id)
    expect_false(grepl("fit_go_table", ns_html(output$summary_tbl), fixed = TRUE))
    expect_false(grepl("sequential fit", ns_text(output$summary_reco), fixed = TRUE))
    expect_match(ns_text(output$status_line), "the model fit by k (", fixed = TRUE)
    expect_match(ns_text(output$status_line), "on the Model fit by k panel", fixed = TRUE)
    session$setInputs(fit_go = 1)
    expect_identical(started, list(1:2, 3L))
    expect_true(isTRUE(fit_run()$running))
    expect_match(ns_html(output$fit_control), '-fit_cancel"', fixed = TRUE)
    for (slot in fit_jobs$slots) slot$process$alive <- FALSE
    session$elapse(200)
    expect_identical(fit_run()$status, "ran")
    expect_match(ns_text(output$fit_control), "The model fit by k ran on request, on 2 background workers: 3 solutions in", fixed = TRUE)
  })
})

test_that("the EFA tab's method, estimator and correlations controls each change what DMAR fits and the recorded R", {
  html <- gsub("\\s+", " ", ns_html(mod_efa_ui("efa")))
  at <- function(id) regexpr(sprintf('id="%s"', id), html, fixed = TRUE)
  expect_true(all(c(at("efa-cor_type"), at("efa-fm"), at("efa-estimator")) > 0))
  expect_true(at("efa-fm") < at("efa-estimator") && at("efa-estimator") < at("efa-cor_type") &&
    at("efa-cor_type") < at("efa-n_factors"))
  expect_match(html, "every method is a common factor method, none is principal components", fixed = TRUE)
  expect_match(html, "The matrix minres is fit to under the third method", fixed = TRUE)
  for (fm in c("ml", "minres", "minres_matrix")) {
    expect_match(html, sprintf('<option value="%s"', fm), fixed = TRUE)
  }
  for (fm in c("pa", "wls", "gls")) {
    expect_false(grepl(sprintf('<option value="%s"', fm), html, fixed = TRUE))
  }
  expect_false(grepl("principal component", tolower(paste(names(sieve_efa_extraction_choices()), collapse = " "))))

  st <- ns_nine()
  X <- shiny::isolate(st$items)
  R <- stats::cor(X)
  local_mocked_bindings(withProgress = function(expr, ...) expr)
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(
      n_factors = 3, rotation = "oblimin", suppress = 0.3, sort_loadings = TRUE,
      cor_type = "auto", fm = "ml", estimator = "study", efa_go = 1
    )
    run <- efa_run()
    expect_identical(run$fm, "ml")
    # The study's default for continuous items: ML with FIML.
    expect_identical(run$route$estimator, "ML")
    expect_identical(run$route$missing, "fiml")
    expect_null(run$basis)
    direct_ml <- suppressWarnings(psych::fa(r = R, nfactors = 3, fm = "ml", rotate = "oblimin", n.obs = nrow(X)))
    expect_equal(unname(efa()$communality), unname(direct_ml$communality), tolerance = 1e-4)
    expect_match(ns_text(output$efa_overview), "The estimator is ML with FIML", fixed = TRUE)
    # Minres on the item responses: DMAR's ULS, psych's minres.
    session$setInputs(fm = "minres", efa_go = 2)
    expect_identical(efa_run()$route$estimator, "ULS")
    direct_ul <- suppressWarnings(psych::fa(r = R, nfactors = 3, fm = "minres", rotate = "oblimin", n.obs = nrow(X)))
    expect_equal(unname(efa()$communality), unname(direct_ul$communality), tolerance = 1e-4)
    expect_false(isTRUE(all.equal(unname(efa()$communality), unname(direct_ml$communality), tolerance = 1e-6)))
    expect_match(ns_text(output$efa_overview), "Minres (unweighted least squares on the correlation matrix", fixed = TRUE)
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl('    estimator = "ULS",', log, fixed = TRUE)))
    expect_true(any(grepl("EFA: 3 factor(s) by DMAR::fa_exploratory, minimum residual (ULS) on the item responses, complete cases", log, fixed = TRUE)))
    # The estimator: ML on the complete cases.
    session$setInputs(fm = "ml", estimator = "ml", efa_go = 3)
    expect_identical(efa_run()$route[c("estimator", "missing")], list(estimator = "ML", missing = "listwise"))
    ref <- attr(DMAR::fa_exploratory(X, 3, estimator = "ML", missing = "listwise", rotation = "none"), "fit_measures")
    expect_equal(sieve_efa_fit_value(efa()$fit_measures, "cfi"), sieve_efa_fit_value(ref, "cfi"), tolerance = 1e-8)
    expect_match(ns_text(output$estimator_source),
      "In force here: ML on the complete cases (items as continuous), chosen on this tab. For these items: ML on the complete cases.",
      fixed = TRUE
    )
    expect_match(ns_text(output$efa_overview), "The estimator is ML on the complete cases", fixed = TRUE)
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl('    missing = "listwise",', log, fixed = TRUE)))
  })

  # Correlations: the matrix route, polychoric chosen on the tab for
  # ordered items that the study treats as continuous.
  so <- ns_ordinal_state(estimator_choice = "mlr")
  O <- shiny::isolate(so$items)
  shiny::testServer(mod_efa_server, args = list(state = so), {
    session$setInputs(
      n_factors = 2, rotation = "oblimin", suppress = 0.3, sort_loadings = TRUE,
      cor_type = "auto", fm = "minres_matrix", estimator = "study", efa_go = 1
    )
    expect_identical(efa_run()$basis$used, "pearson")
    session$setInputs(cor_type = "poly", efa_go = 2)
    expect_identical(efa_run()$basis$used, "polychoric")
    Rp <- psych::polychoric(O, correct = 0.5, progress = FALSE)$rho
    direct <- suppressWarnings(psych::fa(r = Rp, nfactors = 2, fm = "minres", rotate = "oblimin", n.obs = nrow(O)))
    expect_equal(unname(efa()$communality), unname(direct$communality), tolerance = 1e-4)
    expect_match(ns_text(output$cor_source),
      "In force here: polychoric correlations, chosen on this tab.",
      fixed = TRUE
    )
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("# Correlations: In force here: polychoric correlations, chosen on this tab.",
      log,
      fixed = TRUE
    )))
    expect_true(any(grepl("R_items <- psych::polychoric(active_items[complete_rows, , drop = FALSE], correct = 0.5, progress = FALSE)$rho",
      log,
      fixed = TRUE
    )))
    # The estimator on the item responses: WLSMV, every item ordered.
    session$setInputs(fm = "ml", estimator = "wlsmv", efa_go = 3)
    run <- efa_run()
    expect_identical(run$route$estimator, "WLSMV")
    expect_setequal(run$route$ordered, names(O))
    expect_true(is.finite(sieve_efa_fit_value(efa()$fit_measures, "cfi")))
    expect_match(ns_text(output$efa_overview), "fit as they stand by WLSMV", fixed = TRUE)
  })
})

# ---- Coefficient Omega and Coefficient Alpha --------------------------------

test_that("the Items tab names the coefficients Coefficient Omega and Coefficient Alpha with their symbols", {
  html <- ns_html(mod_winnow_ui("winnow"))
  # Qualified by its denominator.
  expect_match(html, "Coefficient Omega, \u03c9, robust (observed denominator; recommended)", fixed = TRUE)
  expect_match(html, "Coefficient Alpha, \u03b1 (legacy)", fixed = TRUE)
  expect_false(grepl("McDonald", html, fixed = TRUE))
  expect_false(grepl("Cronbach", html, fixed = TRUE))
  # The values the server reads are the codes omega and alpha.
  expect_match(html, 'value="omega"', fixed = TRUE)
  expect_match(html, 'value="alpha"', fixed = TRUE)
})

test_that("the Reliability tab's headers, captions and plot name Coefficient Omega and Coefficient Alpha with their symbols", {
  ui <- ns_html(mod_reliab_ui("reliab"))
  expect_match(ui, "Reliability by Construct or Facet: Omega and Alpha With Uncertainty", fixed = TRUE)
  expect_match(ui, "Also show Coefficient Alpha, \u03b1 (for comparison with older reports)", fixed = TRUE)
  expect_match(ui, 'data-value="Reliability table"', fixed = TRUE)
  expect_false(grepl("McDonald's", ui, fixed = TRUE))
  st <- ns_nine()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "model_implied", ci_method = "mlr", missing = "listwise",
      conf_level = 0.95, show_alpha = TRUE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    expect_true(nrow(reliab()) == 3L)
    # The data table's header: the symbols arrive in the table's container
    # (the widget's JSON escapes them as \\u03c9, which the browser reads back).
    tbl <- jsonlite::fromJSON(unclass(output$omega_tbl)[1])$x$container
    # The Coefficient column names each value, "Omega (model-implied
    # denominator)" here, and the
    # value column is Estimate.
    expect_match(tbl, "<th>Coefficient</th>", fixed = TRUE)
    expect_match(tbl, "<th>Estimate</th>", fixed = TRUE)
    expect_match(tbl, "<th>\u03b1 (comparison)</th>", fixed = TRUE)
    expect_false(grepl("<th>Omega</th>", tbl, fixed = TRUE))
    foot <- ns_text(output$footnote)
    expect_match(foot, "Coefficient Omega, \u03c9, with a 95% confidence interval", fixed = TRUE)
    expect_match(foot, "Coefficient Alpha, \u03b1, is shown only for comparison", fixed = TRUE)
    expect_false(grepl("McDonald's", foot, fixed = TRUE))
    # The plot's axis label is plotmath, so it renders on every device.
    expect_no_error(output$omega_plot)
  })
  # The axis label draws omega as a symbol on the PDF device as well.
  gg <- ggplot2::ggplot(data.frame(x = 0.8, y = "A"), ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = expression(paste("Coefficient Omega, ", omega)))
  path <- withr::local_tempfile(fileext = ".pdf")
  expect_no_warning({
    grDevices::pdf(path, width = 4, height = 3)
    print(gg)
    grDevices::dev.off()
  })
  expect_gt(file.size(path), 0)
})

test_that("under MLR an incomplete row puts the exploratory fits under FIML, and a complete pool on the complete cases", {
  X <- shiny::isolate(ns_nine()$items)
  expect_identical(sieve_efa_missing_arg("fiml", X), "listwise")
  expect_identical(sieve_efa_missing_arg("listwise", X), "listwise")
  expect_null(sieve_efa_missing_arg(NULL, X))
  Xm <- X
  Xm[c(3, 50), 1] <- NA
  Xm[7, 5] <- NA
  expect_identical(sieve_efa_missing_arg("fiml", Xm), "ml")
  byk <- sieve_efa_fit_by_k(Xm, "MLR", NULL, rotation = "none", ks = 2L, missing = "fiml")
  expect_identical(attr(byk, "lavaan_missing"), "ml")
  expect_identical(byk$nobs, 301) # every case contributes
  ref <- suppressWarnings(lavaan::efa(data = Xm, nfactors = 2, rotation = "none", estimator = "MLR", missing = "ml"))
  ref <- if (inherits(ref, "efaList")) ref[[1]] else ref
  fam <- attr(byk, "family")
  expect_equal(byk$CFI, unname(lavaan::fitMeasures(ref, paste0("cfi.", fam))), tolerance = 1e-8)
  # In the Dimensionality tab, the recorded R says so and replays.
  st <- ns_nine()
  shiny::isolate({ st$items <- Xm; sieve_confirm_types(st) })
  local_mocked_bindings(
    withProgress = function(expr, ...) expr,
    sieve_fit_wait = function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L, ...) 1
  )
  shiny::testServer(mod_retention_server, args = list(state = st), {
    ns_retention_inputs(session, max_k = 2, n_iter = 10)
    f <- fit_run()
    expect_identical(f$lavaan_missing, "ml")
    expect_identical(f$n, 301L)
    expect_identical(nrow(x_data()), 301L) # EM correlations retain the raw FIML rows
    expect_match(ns_text(output$fit_note), "(n = 301, every case with at least one response, under FIML;", fixed = TRUE)
    log <- shiny::isolate(state$codelog)
    at <- grep("fit_by_k <- do.call(rbind, lapply(1:2, function(k) {", log, fixed = TRUE)
    expect_length(at, 1L)
    expect_match(log[at + 1L], 'missing = "ml")', fixed = TRUE)
    env <- new.env(parent = globalenv())
    env$active_items <- Xm
    # Replay the whole recorded call, including its fit-index safeguards.
    # Its length can change without changing the FIML specification.
    end <- which(seq_along(log) > at & trimws(log) == "}))")[1L]
    expect_false(is.na(end))
    suppressWarnings(eval(parse(text = log[seq.int(at, end)]), envir = env))
    expect_equal(env$fit_by_k$cfi, f$table$cfi, tolerance = 1e-10)
    expect_equal(env$fit_by_k$rmsea, f$table$rmsea, tolerance = 1e-10)
  })
})

test_that("the EFA tab's fit is that of the unrotated solution, so a rotation changes nothing in it, under FIML too", {
  st <- ns_nine()
  Xm <- shiny::isolate(st$items)
  Xm[c(3, 50), 1] <- NA
  Xm[7, 5] <- NA
  shiny::isolate({ st$items <- Xm; sieve_confirm_types(st) })
  local_mocked_bindings(withProgress = function(expr, ...) expr)
  # The study's default for continuous items: ML with FIML.
  unrotated <- attr(DMAR::fa_exploratory(Xm, nfactors = 3, estimator = "ML", missing = "fiml", rotation = "none"), "fit_measures")
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(
      n_factors = 3, rotation = "oblimin", suppress = 0.3, sort_loadings = TRUE,
      cor_type = "auto", fm = "ml", estimator = "study", efa_go = 1
    )
    first <- efa()$fit_measures
    expect_equal(first$value, unrotated$value, tolerance = 1e-8)
    expect_identical(as.integer(efa()$method$N), nrow(Xm))
    session$setInputs(rotation = "geomin", efa_go = 2)
    expect_equal(efa()$fit_measures$value, first$value, tolerance = 1e-10)
    expect_match(ns_text(output$efa_fit_line), "the unrotated solution of the same model (every rotation shares it)", fixed = TRUE)
    expect_match(ns_text(output$efa_method), "FIML, N = 301", fixed = TRUE)
  })
})

test_that("the eigenvalue rank axis labels every rank up to 25 and every fifth beyond", {
  expect_identical(sieve_rank_breaks(9L), 1:9)
  expect_identical(sieve_rank_breaks(25L), 1:25)
  expect_identical(sieve_rank_breaks(68L), c(1L, seq(5L, 65L, by = 5L)))
  # A narrowed window labels from its own first rank.
  expect_identical(sieve_rank_breaks(10L, 3L), 3:10)
  expect_identical(sieve_rank_breaks(60L, 3L), c(3L, seq(10L, 60L, by = 5L)))
  # A multiple of five within two ranks of the first is left out, so the
  # labels of 24 and 25 do not run together.
  expect_identical(sieve_rank_breaks(68L, 24L), c(24L, seq(30L, 65L, by = 5L)))
  expect_identical(sieve_rank_breaks(68L, 22L), c(22L, seq(25L, 65L, by = 5L)))
})

test_that("the ranks an eigenvalue plot shows default to all and accept only a real range", {
  expect_identical(sieve_rank_window(24L, NULL), c(1L, 24L))
  expect_identical(sieve_rank_window(24L, c(1, 8)), c(1L, 8L))
  expect_identical(sieve_rank_window(24L, c(8, 2)), c(2L, 8L))
  expect_identical(sieve_rank_window(24L, c(30, 2)), c(2L, 24L))
  # Both ends on one rank: the nearest two ranks, so a line is drawn where
  # the slider is; something that is not two
  # numbers is all ranks.
  expect_identical(sieve_rank_window(24L, c(5, 5)), c(5L, 6L))
  expect_identical(sieve_rank_window(24L, c(24, 24)), c(23L, 24L))
  expect_identical(sieve_rank_window(24L, "a"), c(1L, 24L))
  expect_identical(sieve_rank_window(24L, c(NA, 3)), c(1L, 24L))
  expect_null(sieve_rank_window_text(c(1L, 24L), 24L))
  expect_identical(
    sieve_rank_window_text(c(2L, 8L), 24L),
    "Ranks 2 to 8 of 24 are shown, with the vertical axis fitted to them; the full plot is ranks 1 to 24."
  )
  # A reference line outside the window is named, never drawn by widening it.
  out <- sieve_rank_window_lines(c(1L, 8L), theory_k = 5, decision_k = 12)
  expect_identical(out$theory_k, 5)
  expect_null(out$decision_k)
  expect_identical(out$outside, "Not drawn because outside the ranks shown: the recorded decision (12).")
  both <- sieve_rank_window_lines(c(3L, 8L), theory_k = 2, decision_k = 9)
  expect_null(both$theory_k)
  expect_match(both$outside, "the theorized number of factors (2) and the recorded decision (9)", fixed = TRUE)
  expect_null(sieve_rank_window_lines(c(1L, 8L))$outside)
  # A saved figure's window is recorded as R that draws it.
  expect_null(sieve_rank_window_code(c(1L, 24L), 24L, "a", "b", "x", "y"))
  code <- sieve_rank_window_code(c(2L, 5L), 9L, "scree_unreduced", "scree_reduced", "Eigenvalue rank", "Eigenvalue")
  expect_match(code[1L], "Ranks 2 to 5 of 9 were shown", fixed = TRUE)
  env <- new.env(parent = globalenv())
  env$scree_unreduced <- c(4, 2, 1.2, 0.8, 0.6, 0.5, 0.4, 0.3, 0.2)
  env$scree_reduced <- env$scree_unreduced - 0.3
  grDevices::pdf(NULL)
  eval(parse(text = code), envir = env)
  limits <- graphics::par("usr")
  grDevices::dev.off()
  expect_identical(env$shown, 2:5)
  # The axis is fitted to the ranks shown, not to the first eigenvalue.
  expect_lt(limits[4L], 2.5)
})

test_that("the factors extracted for the scree plot's reduced series are 1 to the Ledermann bound", {
  # The largest k with nonnegative degrees of freedom, ((p - k)^2 - (p + k)) / 2.
  expect_identical(sieve_scree_max_factors(9L), 5L)
  expect_identical(sieve_scree_max_factors(24L), 17L)
  expect_identical(sieve_scree_max_factors(3L), 1L)
  expect_identical(sieve_scree_max_factors(2L), 1L)
  expect_identical(sieve_scree_max_factors(NA), 1L)
  for (p in 3:40) {
    k <- sieve_scree_max_factors(p)
    expect_gte(((p - k)^2 - (p + k)) / 2, 0)
    if (k < p) expect_lt(((p - k - 1)^2 - (p + k + 1)) / 2, 0)
  }
  expect_identical(sieve_scree_nfactors(NULL, 9L), 1L)
  expect_identical(sieve_scree_nfactors(NA, 9L), 1L)
  expect_identical(sieve_scree_nfactors(0, 9L), 1L)
  expect_identical(sieve_scree_nfactors("3", 9L), 3L)
  expect_identical(sieve_scree_nfactors(40, 9L), 5L)
  expect_identical(sieve_scree_factor_words(1L), "one-factor")
  expect_identical(sieve_scree_factor_words(3L), "three-factor")
  expect_identical(sieve_scree_factor_words(12L), "12-factor")
  # The sentence for an improper (Heywood) solution, for one item and for
  # several, and none for a proper one.
  expect_null(sieve_scree_improper_text(list(nfactors = 3L, improper = character(0))))
  expect_identical(
    sieve_scree_improper_text(list(nfactors = 4L, improper = "t7")),
    paste(
      "Improper solution (Heywood case): in the four-factor solution the uniqueness of t7 is at the",
      "lower bound, so that item's communality and the reduced eigenvalues computed with it should",
      "not be read; a smaller number of factors extracted may give a proper solution."
    )
  )
  expect_match(
    sieve_scree_improper_text(list(nfactors = 5L, improper = c("t7", "t9"))),
    "the uniquenesses of t7, t9 are at the lower bound, so those items' communalities and the reduced eigenvalues computed with them",
    fixed = TRUE
  )
})

test_that("the scree plot's reduced series uses the communalities of the factors extracted, and its window draws only those ranks", {
  b <- sieve_retention_cor(lavaan::HolzingerSwineford1939[, paste0("x", 1:9)], requested = "pearson")
  v1 <- sieve_scree_values(b)
  v3 <- sieve_scree_values(b, 3)
  expect_identical(v1$nfactors, 1L)
  expect_identical(v3$nfactors, 3L)
  expect_identical(v3$table$unreduced, v1$table$unreduced)
  expect_identical(v3$kaiser, v1$kaiser)
  # The eigenvalues of R with its diagonal replaced by the three-factor ML
  # communalities of stats::factanal (the reduction of
  # DMAR::parallel_analysis(); psych's ML solution stops within 1e-5 of it
  # on the same optimum).
  f3 <- stats::factanal(covmat = b$R, factors = 3, rotation = "none")
  reduced <- b$R
  diag(reduced) <- 1 - f3$uniquenesses
  expect_equal(v3$table$reduced, eigen(reduced, symmetric = TRUE, only.values = TRUE)$values, tolerance = 1e-10)
  expect_lt(max(abs(v3$table$reduced -
    psych::fa(b$R, nfactors = 3, rotate = "none", fm = "ml", warnings = FALSE)$values)), 5e-5)
  expect_identical(sieve_scree_values(b, 99)$nfactors, 5L)
  # The recorded R reproduces the series it describes.
  code <- sieve_scree_code(v3)
  expect_true(any(grepl("factors = 3, rotation", code, fixed = TRUE)))
  expect_true(any(grepl("three-factor maximum likelihood solution", code, fixed = TRUE)))
  env <- new.env(parent = globalenv())
  env$R_items <- b$R
  grDevices::pdf(NULL)
  eval(parse(text = code), envir = env)
  grDevices::dev.off()
  expect_equal(env$scree_reduced, v3$table$reduced, tolerance = 1e-12)
  # The window: four ranks of each series, breaks 1 to 4, the decision line
  # outside it named in the subtitle and not drawn.
  gg <- sieve_scree_plot(v3, theory_k = 3, decision_k = 6, shown = c(1, 4))
  built <- ggplot2::ggplot_build(gg)
  pts <- built$data[[which(vapply(gg$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))]]
  expect_identical(sort(unique(pts$x)), c(1, 2, 3, 4))
  expect_identical(nrow(pts), 8L)
  vlines <- unlist(lapply(gg$layers, function(l) if (inherits(l$geom, "GeomVline")) l$data$xintercept))
  expect_identical(unname(vlines), 3)
  sub <- gsub("\n", " ", gg$labels$subtitle)
  # A narrowed window carries no label on the line at 1, which its series
  # can run through; the subtitle names the line.
  labels <- unlist(lapply(gg$layers, function(l) l$aes_params$label))
  expect_false("Kaiser criterion (eigenvalue 1)" %in% labels)
  expect_true("Kaiser criterion (eigenvalue 1)" %in% unlist(lapply(sieve_scree_plot(v1)$layers, function(l) l$aes_params$label)))
  # A sentence that does not apply leaves no gap behind.
  expect_false(grepl("  ", sub, fixed = TRUE))
  expect_false(grepl("  ", gsub("\n", " ", sieve_scree_plot(v1)$labels$subtitle), fixed = TRUE))
  expect_match(sub, "Ranks 1 to 4 of 9 are shown", fixed = TRUE)
  expect_match(sub, "the recorded decision (6)", fixed = TRUE)
  expect_match(sub, "Parallel analysis compares the one-factor reduction, not this one.", fixed = TRUE)
  legend <- levels(gg$data$series)
  expect_true(any(grepl("three-factor ML communalities", legend, fixed = TRUE)))
  # By default the whole plot, every rank.
  whole <- sieve_scree_plot(v1)
  full <- ggplot2::ggplot_build(whole)
  fpts <- full$data[[which(vapply(whole$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))]]
  expect_identical(sort(unique(fpts$x)), as.numeric(1:9))
  # The parallel analysis figure takes the same window.
  pg <- sieve_pa_plot(v1$table$reduced, v1$table$reduced / 2, "a test reference", "pearson", shown = c(2, 5))
  pb <- ggplot2::ggplot_build(pg)
  ppts <- pb$data[[which(vapply(pg$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1)))]]
  expect_identical(sort(unique(ppts$x)), c(2, 3, 4, 5))
  expect_match(gsub("\n", " ", pg$labels$subtitle), "Ranks 2 to 5 of 9", fixed = TRUE)
})

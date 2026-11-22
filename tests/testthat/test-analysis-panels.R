# Tests of the wording checks, the methods draft, the AIPE planners, the
# Bollen-Stine bootstrap, the IRT readout gate, the partial invariance
# workflow, and the scoring and norms export. Each test drives the real
# module server or helper on the bundled example.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

make_state <- function(confirmed = TRUE) {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = confirmed, labels = TRUE,
    theory = TRUE, name = fx$name)
  s
}

test_that("wording checks flag the classic item-writing faults and clear clean items", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  # A deliberately bad item: double-barreled, double negative, absolute.
  meta$wording[1] <- "I do not never like my job and my coworkers at all times"
  # A clean item.
  meta$wording[2] <- "I enjoy solving spatial puzzles"
  wc <- sieve_wording_checks(meta)
  bad <- wc[wc$item == meta$item[1], ]
  clean <- wc[wc$item == meta$item[2], ]
  expect_true(bad$double_barreled)
  expect_true(bad$negation)
  expect_true(bad$double_negative)
  expect_true(bad$absolute)
  expect_gte(bad$flags, 4L)
  expect_false(clean$double_barreled)
  expect_false(clean$negation)
  expect_identical(clean$flags, 0L)
  # Reverse balance: a single reversed item draws the stray-artifact note.
  meta$reverse[1] <- TRUE
  rb <- sieve_reverse_balance(meta)
  expect_match(rb$note[rb$construct == meta$construct[1]], "single reversed")
})

test_that("the methods draft reference list uses the en dash for page ranges", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  d <- sieve_methods_draft(fx$data[, fx$item_cols], meta, "the bundled example")
  lines <- strsplit(d, "\n", fixed = TRUE)[[1]]
  refs <- grep("^- ", lines[seq(match("## References for This Draft", lines), length(lines))], value = TRUE)
  expect_gte(length(refs), 2L)
  # House style writes a numeric range with "--", which renders as an en dash
  # in the markdown the draft is downloaded as. A single hyphen between digits
  # is the typographic error this pins. Each entry ends with its DOI or
  # URL, whose hyphens are part of the identifier, so
  # the check reads the citation before the link.
  expect_false(any(grepl("[0-9]-[0-9]", sub(" https?://.*$", "", refs))))
  expect_true(all(grepl("https://", refs[!grepl("SIEVER:", refs, fixed = TRUE)], fixed = TRUE)))
  # Green and Yang (2009) is listed only when omega for ordered items with
  # their model-implied denominator is reported (DMAR's definition of
  # robust omega).
  if (any(grepl("Green", refs))) expect_match(refs[grepl("Green", refs)], "155--167", fixed = TRUE)
  expect_match(refs[grepl("Pornprasertmanit", refs)], "69--92", fixed = TRUE)
  # lavaan's own article, read by its first author; the unbiased SRMR's
  # interval cites Maydeu-Olivares, Shi, and Rosseel (2018), which names
  # Rosseel too.
  lavaan_ref <- refs[startsWith(refs, "- Rosseel, Y. (2012)")]
  expect_length(lavaan_ref, 1L)
  expect_match(lavaan_ref, "1--36", fixed = TRUE)
  usrmr_ref <- refs[startsWith(refs, "- Maydeu-Olivares, A., Shi, D., & Rosseel, Y. (2018)")]
  expect_length(usrmr_ref, 1L)
  expect_match(usrmr_ref, "389--402", fixed = TRUE)
})

test_that("the methods draft recomputes its numbers from the live data", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  d <- sieve_methods_draft(fx$data[, fx$item_cols], meta, "the bundled example")
  expect_match(d, "301 respondents")
  expect_match(d, "RMSEA")
  # The heading names the coefficient with its symbol; coefficient
  # sentences use the shared engine's plain-language label.
  expect_match(d, "Coefficient Omega, \u03c9", fixed = TRUE)
  # Each coefficient names its target: "Robust omega (observed
  # denominator) = ..." or
  # "Omega (model-implied denominator) = ...".
  expect_match(d, "[Oo]mega \\([^)]*\\) = 0\\.")
  expect_match(d, "Kelley and Pornprasertmanit")
  # The draft never leaves a construct silently unreported.
  for (cn in unique(meta$construct)) expect_match(d, cn, fixed = TRUE)
})

test_that("the omega AIPE planner reports a required N and logs the call", {
  st <- make_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "model_implied", ci_method = "mlr",
      missing = "listwise", conf_level = 0.95,
      show_alpha = FALSE, score_method = "Bartlett",
      plan_omega = 0.80, plan_k = 6, plan_width = 0.10
    )
    html <- as.character(output$plan_result$html)
    expect_match(html, "Required N: \\d+")
    # Spearman-Brown mapping: omega .80 with k = 6 implies rho = .4.
    expect_match(html, "0\\.400")
  })
})

test_that("the omega AIPE planner with assurance is seeded, larger, and logged", {
  st <- make_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "model_implied", ci_method = "mlr",
      missing = "listwise", conf_level = 0.95,
      show_alpha = FALSE, score_method = "Bartlett",
      plan_omega = 0.80, plan_k = 6, plan_width = 0.10
    )
    html0 <- as.character(output$plan_result$html)
    n0 <- as.integer(sub(".*Required N: (\\d+).*", "\\1", html0))
    expect_true(is.finite(n0))

    session$setInputs(plan_assurance = "0.85", plan_seed = 113)
    # The assurance answer is Monte Carlo based, so it waits for its button
    # whatever its estimate.
    waiting <- as.character(output$plan_result$html)
    expect_match(waiting, "waits for its button", fixed = TRUE)
    expect_false(grepl("seed = 113", waiting, fixed = TRUE))
    session$setInputs(plan_go = 1)
    html1 <- as.character(output$plan_result$html)
    expect_match(html1, "85% assurance", fixed = TRUE)
    expect_match(html1, "seed = 113", fixed = TRUE)
    n1 <- as.integer(sub(".*Required N: (\\d+).*", "\\1", html1))
    # Assurance can only demand more respondents than expected width.
    expect_gt(n1, n0)
    # Pin against the direct DMAR call under the same seed and stream, the
    # same reproduction the logged code hands the reader.
    rho <- 0.80 / (6 - 0.80 * 5)
    set.seed(113)
    ref <- DMAR::ss_aipe_reliability(
      model = "Parallel", type = "Normal Theory",
      width = 0.10, n_items = 6, true_item_covariance = rho,
      true_residual_var = 1 - rho, conf_level = 0.95,
      assurance = 0.85
    )
    expect_identical(n1, as.integer(ref$value[ref$term == "necessary_N"]))
    # The logged call carries the assurance level and the seed. (The plan
    # is recorded on request, not whenever it is shown.)
    expect_false(any(grepl("Plan the next study", shiny::isolate(state$codelog), fixed = TRUE)))
    session$setInputs(plan_log = 1)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "assurance = 0.85", fixed = TRUE)
    expect_match(log, "set.seed(113)", fixed = TRUE)
  })
})

test_that("the RMSEA AIPE planner reports a required N from the fitted model", {
  st <- make_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE,
      plan_rmsea = 0.05, plan_df = 24, plan_rmsea_width = 0.05
    )
    html <- as.character(output$plan_rmsea_result$html)
    expect_match(html, "Required N: \\d+")
    # Pin against the direct DMAR call, at the level of the interval the
    # Fit sub-tab shows (90%, DMAR's default for the RMSEA interval).
    ref <- DMAR::ss_aipe_rmsea(true_rmsea = 0.05, df = 24, width = 0.05, conf_level = 0.90)
    expect_match(html, sprintf("Required N: %d", as.integer(ref$value[ref$term == "necessary_N"])))
  })
})

test_that("the Bollen-Stine bootstrap returns a p-value in [0, 1] and restores RNG state", {
  st <- make_state()
  set.seed(2)
  before <- .Random.seed
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE,
      bs_B = 100, bs_seed = 113
    )
    session$setInputs(bs_run = 1)
    # The run is a background process: the mock clock advances while real
    # time passes, and a run still going at the limit fails here.
    finished <- siever_wait_until(function() !isTRUE(shiny::isolate(bs_running())),
      "The Bollen-Stine run", step = function() {
        Sys.sleep(0.05)
        session$elapse(250)
      })
    if (finished) {
      r <- bs_result()
      expect_null(r$error)
      expect_true(is.finite(r$p_bs) && r$p_bs >= 0 && r$p_bs <= 1)
      expect_true(is.finite(r$p_ml))
      expect_identical(r$B, 100L)
    }
  })
  expect_identical(.Random.seed, before)
})

test_that("the CFA tab carries no IRT view of its own", {
  # IRT parameters belong to the IRT module (R/mod_irt.R), which routes
  # through DMAR::irt_grm() and DMAR::irt_information() instead of
  # hand-rolled arithmetic. A second view on the CFA tab would be a second
  # implementation of the same quantity, and one scaled by 1.7 (the
  # conventional rounding; DMAR uses 1.702) disagrees in the third digit of
  # the logistic slope. This test keeps such a duplicate out; the
  # parameterization itself is asserted against DMAR in
  # tests/testthat/test-mod-irt.R.
  st <- make_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    # No such reactive or outputs are in the module's environment.
    expect_false(exists("irt_frame", inherits = FALSE))
    expect_false("irt_tbl" %in% names(output))
    expect_false("irt_note" %in% names(output))
  })
  ui <- as.character(mod_cfa_ui("cfa"))
  expect_false(grepl("IRT view", ui, fixed = TRUE))
  # The module that owns the parameterization carries it.
  expect_true(grepl("Item Parameters", as.character(mod_irt_ui("irt")),
    fixed = TRUE
  ) ||
    grepl("IRT", as.character(mod_irt_ui("irt")), fixed = TRUE))
})

test_that("the partial invariance workflow diagnoses, frees, and refits", {
  st <- make_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_true(is.data.frame(r$table))
    expect_true(!is.null(r$fits$scalar))
    session$setInputs(inv_level = "scalar")
    session$setInputs(inv_diag = 1)
    d <- inv_diag()
    expect_null(d$error)
    expect_true(nrow(d$table) > 0)
    expect_true(all(c("parameter", "type", "X2", "p") %in% names(d$table)))
    # Intercept constraints are present at the scalar rung.
    expect_true(any(d$table$type == "intercept"))
    # Free the top constraint and refit.
    session$setInputs(inv_diag_tbl_rows_selected = 1)
    session$setInputs(inv_refit = 1)
    p_res <- inv_partial()
    expect_null(p_res$error)
    expect_length(p_res$freed, 1L)
    expect_true(is.finite(p_res$cfi))
    expect_true(is.finite(p_res$dcfi))
  })
})

test_that("scoring and norms produce sample-bound percentiles and runnable syntax", {
  st <- make_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    df <- norms()
    expect_true(all(c("construct", "k", "n", "mean", "sd", "p05", "p50", "p95") %in% names(df)))
    expect_true(all(df$p05 <= df$p50 & df$p50 <= df$p95))
    expect_equal(nrow(df), 3L)
    syntax <- scoring_syntax()
    for (cn in df$construct) expect_match(syntax, cn, fixed = TRUE)
    # The syntax parses as R.
    expect_no_error(parse(text = syntax))

    # Downloaded syntax accepts raw values and must reproduce the scores that
    # feed the norms. The oracle is independent of the generated implementation.
    replay <- new.env(parent = baseenv())
    replay$dat <- shiny::isolate(state$raw)
    eval(parse(text = syntax), envir = replay)
    meta <- shiny::isolate(state$meta)
    raw <- shiny::isolate(state$raw)
    for (cn in df$construct) {
      its <- meta$item[meta$status == "active" & !is.na(meta$construct) &
        meta$construct == cn]
      expected <- rowMeans(raw[, its, drop = FALSE], na.rm = TRUE)
      expected[!is.finite(expected)] <- NA_real_
      expect_equal(replay$dat[[replay$score_name_map[[cn]]]], expected,
        tolerance = 1e-12
      )
      expect_equal(norms_read()$scores[[cn]], expected, tolerance = 1e-12)
    }

    # Distinct declared item bounds, all-missing and partial rows distinguish
    # raw-input reversal from already processed scoring and minimum-response
    # eligibility. A nondefault weighted policy also needs executable replay.
    cn <- df$construct[1]
    its <- meta$item[meta$status == "active" & !is.na(meta$construct) &
      meta$construct == cn]
    raw[1, its] <- NA_real_
    raw[2, its[-1]] <- NA_real_
    raw[3, its[1]] <- NA_real_
    processed <- raw[, names(shiny::isolate(state$items)), drop = FALSE]
    for (j in seq_len(2L)) {
      item <- its[j]
      i <- match(item, meta$item)
      lo <- floor(min(raw[[item]], na.rm = TRUE)) - j
      hi <- ceiling(max(raw[[item]], na.rm = TRUE)) + 3 * j
      meta$reverse[i] <- TRUE
      meta$rev_lo[i] <- lo
      meta$rev_hi[i] <- hi
      meta$rev_bounds[i] <- "declared"
      processed[[item]] <- lo + hi - raw[[item]]
    }
    weights <- seq_along(its)
    policy <- list(
      min_count = 2L, min_prop = 0,
      weights = stats::setNames(weights, its)
    )
    state$raw <- raw
    state$items <- processed
    state$meta <- meta
    state$scoring_policies <- stats::setNames(list(policy), cn)
    sieve_confirm_types(state)
    session$flushReact()
    replay <- new.env(parent = baseenv())
    replay$dat <- raw
    expect_no_error(eval(parse(text = scoring_syntax()), envir = replay))
    expected <- vapply(seq_len(nrow(processed)), function(i) {
      x <- unlist(processed[i, its, drop = FALSE], use.names = FALSE)
      seen <- !is.na(x)
      if (sum(seen) < 2L) return(NA_real_)
      sum(x[seen] * weights[seen]) / sum(weights[seen])
    }, numeric(1))
    expect_true(all(is.na(expected[1:2])))
    expect_true(is.finite(expected[3]))
    expect_equal(replay$dat[[replay$score_name_map[[cn]]]], expected,
      tolerance = 1e-12
    )
    expect_equal(norms_read()$scores[[cn]], expected, tolerance = 1e-12)
  })
})

test_that("the methods draft panel assembles on demand in the app", {
  st <- make_state()
  shiny::testServer(mod_repro_server, args = list(state = st), {
    session$setInputs(draft_go = 1)
    d <- draft()
    expect_match(d, "Method \\(draft")
    expect_match(d, "Results \\(draft")
  })
})

test_that("the live replay binder rebuilds the recorded step views", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  meta$status[meta$item == fx$item_cols[1]] <- "parked"
  items <- fx$data[, fx$item_cols, drop = FALSE]
  # One line per object the recorded steps are known to name, so the test
  # fails if a rebuild is dropped rather than only when a whole session breaks.
  log <- c(
    "# [1] Load bundled example",
    "d <- DMAR::holzinger_swineford",
    'items <- c("t1_visual_perception")',
    "",
    "# [2] Every step that reads a session object",
    "psych::fa.parallel(active_items, fm = \"ml\")",
    "efa_fit <- psych::fa(r = X, nfactors = 3)",
    "DMAR::simple_structure(loadings, salient = 0.30)",
    "cfa_fit <- lavaan::cfa(cfa_model, data = items)",
    "grm <- DMAR::irt_grm(items_data[, irt_items])",
    "DMAR::reliability_omega(data = items_df)",
    "score <- rowMeans(items[, construct_items], na.rm = TRUE)",
    'mi <- DMAR::measurement_invariance(inv_data, group = "school")',
    'meta$status[meta$item %in% failing] <- "parked"',
    ""
  )
  st <- siever_test_study(fx)
  st$meta <- meta
  inputs <- shiny::isolate(sieve_replay_inputs(st))
  key <- sieve_replay_store(st, inputs)
  env <- new.env(parent = globalenv())
  expect_no_error(sieve_replay_bind(shiny::isolate(sieve_replay_bundle(st)), key, env))

  active <- meta$item[meta$status == "active"]
  expect_identical(env$item_cols, meta$item)
  expect_identical(env$meta$status, meta$status)
  expect_identical(env$meta$construct, meta$construct)
  expect_identical(names(env$items), meta$item)
  expect_identical(env$items, fx$data[, meta$item, drop = FALSE])
  expect_identical(env$items_data, env$items)
  # active_items is the status == "active" subset, which is the semantics the
  # dimensionality and common method steps use.
  expect_identical(env$active_item_names, active)
  expect_identical(names(env$active_items), active)
  expect_identical(env$X, env$active_items)
  # The per construct lists are one construct's included items.
  expect_true(all(env$construct_items %in% active))
  expect_identical(names(env$items_df), env$construct_items)
  expect_length(unique(meta$construct[match(env$construct_items, meta$item)]), 1L)
  # The invariance frame is the included items plus the non item columns,
  # complete on the items.
  expect_true(all(active %in% names(env$inv_data)))
  expect_true("school" %in% names(env$inv_data))
  expect_false(any(is.na(env$inv_data[, active])))
})

test_that("the reproducible script runs in a clean session", {
  skip_if_not_installed("psych")
  # The whole point of the Reproducible Code view is a script a user can run.
  # This drives real modules, takes the script the download button offers, and
  # evaluates it in a fresh environment.
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9")
    session$setInputs(load = 1)
    session$setInputs(confirm = 1)
  })
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(criterion = "mean", max_k = 3, n_iter = 10)
    invisible(tryCatch(output$scree_note, error = function(e) NULL))
  })
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    session$setInputs(fit_go = 1)
    invisible(tryCatch(output$fit_tbl, error = function(e) NULL))
  })
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    invisible(tryCatch(output$norms_tbl, error = function(e) NULL))
  })

  txt <- NULL
  shiny::testServer(mod_repro_server, args = list(state = st), {
    txt <<- script()
  })
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]

  # Verified source inputs are bound once before the first recorded action;
  # each operation then binds its own compact input revision.
  at_store <- which(startsWith(lines, ".siever_store <-"))
  at_bind <- which(startsWith(lines, "SIEVER::sieve_replay_bind"))
  expect_length(at_store, 1L)
  expect_gt(length(at_bind), 0L)
  expect_lt(at_store, at_bind[1L])

  # Some logged calls (psych::vss) draw, so the run is sent to a scratch
  # device rather than leaving an Rplots.pdf in the test directory.
  env <- new.env(parent = globalenv())
  grDevices::pdf(tempfile(fileext = ".pdf"))
  err <- tryCatch(
    {
      suppressWarnings(suppressMessages(
        utils::capture.output(eval(parse(text = txt), envir = env))
      ))
      NULL
    },
    error = function(e) conditionMessage(e)
  )
  grDevices::dev.off()
  expect_null(err)
  # The objects the recorded steps read exist and mean what the steps assume.
  active <- shiny::isolate(st$meta)$item[shiny::isolate(st$meta)$status == "active"]
  expect_identical(names(env$active_items), active)
  expect_true(is.data.frame(env$items))
  expect_true(inherits(env$cfa_fit, "lavaan"))
  expect_true(all(is.finite(env$score)))
})

test_that("the dimensionality module renders its retention guidance", {
  skip_if_not_installed("psych")
  st <- make_state()
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(criterion = "mean", max_k = 5)
    expect_no_error(output$scree_note)
    expect_no_error(output$mapvss_note)
  })
})

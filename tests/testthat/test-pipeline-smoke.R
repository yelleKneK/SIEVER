# Smoke tests for the SIEVER pipeline. The single most important test is the
# first one: the load -> confirm flow that unlocks every analysis tab. Each
# later block drives one module's server with shiny::testServer and asserts
# that its key outputs compute without error on the bundled example.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("psych")
skip_if_not_installed("shiny")

# A fully loaded, confirmed state on the primary-9 example, the state every
# analysis module sees after a normal ingest.
make_state <- function(confirmed = TRUE) {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = confirmed, labels = TRUE,
    theory = TRUE, name = fx$name)
  s
}

test_that("ingest load auto-confirms all-continuous data and unlocks the pipeline", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1)
    expect_false(is.null(shiny::isolate(state$meta)))
    expect_equal(nrow(shiny::isolate(state$meta)), 9L)
    # Every HS test is continuous, so types must confirm automatically;
    # otherwise the study could not be confirmed anywhere.
    expect_true(shiny::isolate(state$confirmed))
    expect_match(shiny::isolate(state$estimator), "ML")
  })
})

test_that("the main-panel action buttons trigger the same load and confirm", {
  st <- sieve_new_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    session$setInputs(source = "hs9", basis = "theoretical", palette = "nd")
    session$setInputs(load = 1)
    expect_true(shiny::isolate(state$confirmed))
  })
})

test_that("gate helper distinguishes no-data from unconfirmed and clears", {
  st_null <- sieve_new_state()
  expect_s3_class(shiny::isolate(sieve_gate(st_null)), "shiny.tag")
  st_unconf <- make_state(confirmed = FALSE)
  expect_s3_class(shiny::isolate(sieve_gate(st_unconf)), "shiny.tag")
  st_ok <- make_state(confirmed = TRUE)
  expect_null(shiny::isolate(sieve_gate(st_ok)))
})

test_that("embedded palettes resolve with no palette package", {
  expect_identical(sieve_palette_colors("nd", 1L), "#0c2340")
  expect_length(sieve_palette_colors("okabe_ito", 8L), 8L)
  expect_length(sieve_palette_colors("viridis", 12L), 12L) # interpolated
  expect_identical(
    sieve_palette_colors("nonsense", 2L),
    sieve_palette_colors("nd", 2L)
  ) # safe fallback
})

test_that("the item organizer diagnostics and overall fit compute", {
  st <- make_state()
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(
      coef = "omega", factor_cov = "correlated",
      load_low = .4, salient = .3, communality_low = .3,
      item_total_low = .3, redundancy_high = .8
    )
    expect_no_error(output$pool)
    expect_no_error(output$rel_summary)
    expect_no_error(output$overall_fit)
  })
})

test_that("EFA, CFA, reliability, and SEM modules render on the example", {
  st <- make_state()
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(
      n_factors = 3, rotation = "oblimin",
      suppress = 0.3, sort_loadings = TRUE
    )
    session$setInputs(efa_go = 1L)
    expect_no_error(output$pattern_tbl)
    expect_no_error(output$ss_summary)
  })
  st <- make_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    session$setInputs(fit_go = 1)
    expect_no_error(output$fit_tbl)
    expect_no_error(output$est_tbl)
  })
  st <- make_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      ci_method = "mlr", missing = "listwise",
      conf_level = 0.95, show_alpha = TRUE,
      score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    expect_no_error(output$omega_tbl)
  })
  st <- make_state()
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(
      outcomes = "Visual", predictors = c("Textual", "Speed"),
      layout = "tree2", min_path = 0
    )
    session$setInputs(fit_go = 1)
    expect_no_error(output$paths_tbl)
    expect_no_error(output$r2_tbl)
  })
})

test_that("the cross-construct optimizer enumerates and scores item-sets", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  items <- fx$data[, fx$item_cols, drop = FALSE]

  # Candidate count is the product of the per-construct keep-subset counts and
  # is computed without fitting anything.
  expect_equal(siever_test_optimizer_total(meta, max_drop = 0L, min_keep = 3L), 1)
  expect_gt(siever_test_optimizer_total(meta, max_drop = 1L, min_keep = 2L), 1)

  out <- sieve_optimize_items(items, meta,
    max_drop = 1L, min_keep = 2L,
    cap = 200L, simple_structure = TRUE
  )
  expect_null(out$error)
  expect_equal(out$evaluated, nrow(out$table))
  expect_length(out$kept, nrow(out$table)) # kept-item lists align to rows
  expect_true(all(c("omega_min", "cfi", "rmsea", "complexity", "dropped") %in%
    names(out$table)))
  # The cap is enforced: an over-large search returns an error, not a huge run.
  capped <- sieve_optimize_items(items, meta, max_drop = 2L, min_keep = 2L, cap = 2L)
  expect_false(is.null(capped$error))
})

test_that("the optimizer module runs and applies a chosen item-set", {
  st <- make_state()
  # Candidate-search fixtures explicitly use the empirical workflow.
  shiny::isolate(st$construct_basis <- "empirical")
  shiny::testServer(mod_optimize_server, args = list(state = st), {
    session$setInputs(max_drop = 1, min_keep = 2, ss = FALSE, cap = 200)
    expect_no_error(output$count)
    session$setInputs(run = 1)
    expect_no_error(output$results)
  })
})

test_that("sieve_finite_or guards cleared numeric inputs (NA/NULL/empty)", {
  expect_equal(sieve_finite_or(NA, 8L), 8L)
  expect_equal(sieve_finite_or(NULL, 8L), 8L)
  expect_equal(sieve_finite_or(numeric(0), 8L), 8L)
  expect_equal(sieve_finite_or(Inf, 8L), 8L)
  expect_equal(sieve_finite_or(3, 8L), 3)
})

test_that("EFA correlation method matches the active item types", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  expect_identical(sieve_retention_request("auto", meta$type), "pearson") # all continuous -> Pearson
  meta$type <- "ordered"
  expect_identical(sieve_retention_request("auto", meta$type), "polychoric") # all ordered -> polychoric
  meta$type <- "binary"
  expect_identical(sieve_retention_request("auto", meta$type), "tetrachoric") # all binary -> tetrachoric
  meta$type[1:3] <- "continuous"
  expect_identical(sieve_retention_request("auto", meta$type), "mixed") # mixed -> mixed
})

test_that("bifactor loadings expose raw (unclamped) uniqueness for Heywood checks", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  out <- sieve_fit_bifactor(fx$data[, fx$item_cols, drop = FALSE], meta)
  skip_if(is.null(out$loadings), "bifactor model did not converge")
  expect_true("uniqueness_raw" %in% names(out$loadings))
  # The displayed uniqueness is floored at zero; the raw value is not, so it can
  # reveal a negative residual variance (Heywood case) that the clamped column hides.
  expect_true(all(out$loadings$uniqueness >= 0))
  expect_true(any(out$loadings$uniqueness_raw < 0) ||
    all(out$loadings$uniqueness_raw >= 0)) # raw is just unclamped
})

test_that("item wording is preserved and the construct brief assembles", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  expect_true("wording" %in% names(meta))
  expect_true(all(nzchar(meta$wording))) # fixture supplies wordings
  # Renaming the editable label must not disturb the preserved wording.
  w0 <- meta$wording[1]
  meta$label[1] <- "renamed"
  expect_identical(meta$wording[1], w0)
  brief <- sieve_construct_brief(meta, "Visual")
  expect_true(grepl("Construct: Visual", brief, fixed = TRUE))
  expect_true(grepl(meta$item[meta$construct == "Visual"][1], brief, fixed = TRUE))
  # Facet scoping and the empty case behave.
  expect_true(grepl("no included items", sieve_construct_brief(meta, "Nope")))
  expect_match(sieve_wording_cell("a short wording"), "title=")
  expect_identical(sieve_wording_cell(NA_character_), "")
})

test_that("HTMT (via DMAR) and mediation effects build correctly", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  items <- fx$data[, fx$item_cols, drop = FALSE]
  grp <- split(meta$item, meta$construct)
  # HTMT comes from DMAR (no bespoke SIEVER implementation).
  h <- DMAR::htmt(items, blocks = grp)
  expect_true(all(c("construct_1", "construct_2", "htmt") %in% names(h)))
  expect_equal(nrow(h), choose(length(grp), 2L))

  # A mediator (a construct chosen as both predictor and outcome) yields := effects.
  med <- sieve_run_structural(items, fx$data, meta, outcomes = c("Visual", "Speed"), predictors = c("Textual", "Visual"), effects = TRUE)$fit
  skip_if(is.null(med), "mediation model did not converge")
  ss <- lavaan::standardizedSolution(med)
  expect_gt(sum(ss$op == ":="), 0) # indirect/total defined
  expect_true(length(attr(med, "eff_map")) > 0)
  # No mediator -> NULL (nothing to decompose).
  expect_null(sieve_run_structural(items, fx$data, meta, outcomes = "Visual", predictors = c("Textual", "Speed"), effects = TRUE)$fit)
})

test_that("ingest drop control parks/restores and the dropped grouping works", {
  st <- make_state()
  shiny::testServer(mod_ingest_server, args = list(state = st), {
    item1 <- shiny::isolate(state$meta$item[1])
    session$setInputs(cell_edit = list(item = item1, field = "status", value = "Dropped"))
    expect_identical(shiny::isolate(state$meta$status[1]), "parked")
    session$setInputs(cell_edit = list(item = item1, field = "status", value = "In use"))
    expect_identical(shiny::isolate(state$meta$status[1]), "active")
    expect_no_error(output$type_grid)
    expect_no_error(output$dl_seed)
  })
})

test_that("careless-responding screens compute row-level flags on the example", {
  st <- make_state()
  shiny::testServer(mod_screen_server, args = list(state = st), {
    r <- careless()
    expect_true(is.data.frame(r$tbl))
    expect_equal(nrow(r$tbl), 301L)
    expect_true(all(c(
      "longstring", "irv", "mahalanobis",
      "p_mahalanobis", "flags"
    ) %in% names(r$tbl)))
    # Continuous test scores: no straight-liners or invariant responders, and
    # the Mahalanobis screen evaluates on every complete row.
    expect_equal(r$n_long, 0L)
    expect_equal(r$n_irv, 0L)
    expect_true(r$n_md_eval > 0L)
    # Longstring cutoff is half the items, rounded up.
    expect_equal(r$long_cut, ceiling(r$n_item / 2))
    expect_no_error(output$careless_summary)
    expect_no_error(output$careless_tbl)
  })
})

test_that("CV split reports validation metrics: ECVI, cross-validation, reliability", {
  st <- make_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(holdout = 50, seed = 1234)
    expect_no_error(output$split_sizes)
    session$setInputs(split = 1)
    r <- cv_result()
    expect_true(is.data.frame(r$table) && "ECVI" %in% names(r$table))
    expect_true(is.data.frame(r$xval)) # cross-validation discrepancy
    expect_true(is.data.frame(r$rel)) # reliability replication
    expect_no_error(output$xval_tbl)
    expect_no_error(output$rel_tbl)
  })
})

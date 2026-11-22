# The estimator policy and item exclusion: one study-wide estimator setting
# on the Ingest tab, and the same Estimator control on every model tab,
# whose first choice follows the study; and a Drop or restore items dialog
# reachable from every tab.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("shiny")
skip_if_not_installed("lavaan")
skip_if_not_installed("DMAR")
withr::local_options(sieve.cores = 2L)

ns_strip <- function(x) gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", paste(as.character(x), collapse = " ")))

# The bundled nine tests, loaded and confirmed, around a fresh state.
ns_nine_state <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = FALSE, name = fx$name, configure = function(s) {
  s$roles <- sieve_declared_roles(fx$data, fx$item_cols)
  s$estimator <- "ML with FIML"
  })
  s
}

test_that("a tab's Estimator control follows the study's setting unless set on the tab", {
  expect_identical(sieve_estimator_effective("study", list(estimator_choice = "ml")), "ml")
  expect_identical(sieve_estimator_effective(NULL, list(estimator_choice = "wlsmv")), "wlsmv")
  expect_identical(sieve_estimator_effective("mlr", list(estimator_choice = "wlsmv")), "mlr")
  expect_identical(sieve_estimator_effective("study", list()), "auto")
  expect_identical(sieve_estimator_effective("study", list(estimator_choice = "nonsense")), "auto")
  expect_identical(unname(sieve_estimator_select_choices())[1], "study")
  expect_false("study" %in% sieve_estimator_select_choices(study = FALSE))
  expect_match(sieve_estimator_source_text("study", list(estimator_choice = "ml")),
    "ML on the complete cases (items as continuous), the study's setting on the Ingest tab",
    fixed = TRUE
  )
  expect_match(sieve_estimator_source_text("wlsmv", list(estimator_choice = "ml")),
    "WLSMV on polychoric correlations (items as ordered), chosen on this tab",
    fixed = TRUE
  )
})

test_that("the shared correlation basis follows the study's item treatment", {
  st <- ns_nine_state()
  expect_identical(shiny::isolate(sieve_study_basis_request(st)), "auto")
  st$estimator_choice <- "wlsmv"
  expect_identical(shiny::isolate(sieve_study_basis_request(st)), "poly")
  st$estimator_choice <- "ml"
  expect_identical(shiny::isolate(sieve_study_basis_request(st)), "pearson")
  # Ordered items under a continuous study setting: Pearson correlations.
  set.seed(11)
  z <- stats::rnorm(200)
  d <- as.data.frame(lapply(1:4, function(j) cut(z + stats::rnorm(200), c(-Inf, -1, 0, 1, Inf), labels = FALSE)))
  names(d) <- paste0("q", 1:4)
  s2 <- sieve_new_state()
  s2$raw <- d
  s2$items <- d
  m2 <- sieve_build_meta(d, names(d))
  m2$type <- "ordered"
  s2$meta <- m2
  s2$confirmed <- TRUE
  s2$estimator_choice <- "mlr"
  b <- shiny::isolate(sieve_state_basis(s2))
  expect_identical(b$used, "pearson")
  expect_equal(unname(b$R), unname(stats::cor(d)), tolerance = 1e-12)
  s2$estimator_choice <- "auto"
  expect_identical(shiny::isolate(sieve_state_basis(s2))$used, "polychoric")
})

test_that("the CFA tab follows the study's estimator and its own control overrides it", {
  st <- ns_nine_state()
  st$estimator_choice <- "ml"
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", estimator = "study", layout = "top_down")
    expect_identical(estimator_choice(), "ml")
    expect_identical(cfa_plan()$estimator, "ML")
    expect_match(ns_strip(output$estimator_src$html), "the study's setting on the Ingest tab", fixed = TRUE)
    session$setInputs(fit_go = 1L)
    expect_identical(cfa_run()$spec$estimator, "ML")
    expect_identical(cfa_run()$spec$missing, "listwise")
    # The tab's own choice wins over the study's.
    session$setInputs(estimator = "mlr")
    expect_identical(cfa_plan()$estimator, "MLR")
    expect_match(ns_strip(output$estimator_src$html), "chosen on this tab", fixed = TRUE)
    # Compare structures is kept under the estimator in force.
    expect_identical(compare_key()$choices$estimator, "mlr")
    expect_identical(compare_plan()$estimator, "MLR")
  })
})

test_that("the structural, bifactor, validity, invariance and Items tabs pass the estimator in force to their fits", {
  st <- ns_nine_state()
  st$estimator_choice <- "ml"
  meta <- shiny::isolate(st$meta)
  items <- shiny::isolate(st$items)
  # The runners take the estimator.
  bf <- sieve_fit_bifactor(items, meta, estimator = "ml")
  expect_identical(bf$spec$estimator, "ML")
  expect_identical(bf$spec$missing, "listwise")
  mf <- sieve_run_method_factor(items, meta, estimator = "ml")
  expect_identical(mf$spec$estimator, "ML")
  cmp <- sieve_compare_structures(meta, items, estimator = "ml")
  specs <- Filter(Negate(is.null), lapply(cmp$structures, function(s) s$spec))
  expect_true(length(specs) > 0L)
  expect_true(all(vapply(specs, function(s) identical(s$estimator, "ML"), logical(1))))
  fit <- sieve_fit_cfa(items, meta, estimator = "ml")
  expect_identical(lavaan::lavInspect(fit, "options")$estimator, "ML")
  # The modules read the study's setting.
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    session$setInputs(estimator = "study")
    expect_identical(bf_plan()$estimator, "ML")
    expect_identical(bf_key()$choices$estimator, "ml")
    session$setInputs(estimator = "wlsmv")
    expect_identical(bf_plan()$estimator, "WLSMV")
  })
  shiny::testServer(mod_validity_server, args = list(state = st), {
    session$setInputs(estimator = "study")
    expect_identical(cfa_plan()$estimator, "ML")
    expect_identical(cfa_key()$choices$estimator, "ml")
    expect_identical(ulmf_key()$choices$estimator, "ml")
  })
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(estimator = "study", factor_cov = "correlated")
    expect_identical(estimator_choice(), "ml")
  })
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(estimator = "mlr")
    expect_identical(estimator_choice(), "mlr")
  })
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(estimator = "study")
    expect_identical(estimator_choice(), "ml")
  })
})

test_that("the Reliability tab's item treatment follows the study's setting by default", {
  st <- ns_nine_state()
  st$estimator_choice <- "wlsmv"
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(treatment = "study")
    expect_identical(treatment(), "ordered")
    expect_identical(scores_estimator(), "wlsmv")
    expect_match(ns_strip(output$treatment_src$html), "every item as ordered", fixed = TRUE)
    session$setInputs(treatment = "continuous")
    expect_identical(treatment(), "continuous")
    expect_match(ns_strip(output$treatment_src$html), "chosen on this tab", fixed = TRUE)
  })
  st$estimator_choice <- "auto"
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(treatment = "study")
    expect_identical(treatment(), "auto")
  })
  # The bifactor tab's total score omega follows the treatment too.
  meta <- shiny::isolate(st$meta)
  items <- shiny::isolate(st$items)
  ml <- sieve_total_omega(items, meta, estimator = "ml")
  auto <- sieve_total_omega(items, meta)
  expect_false(is.null(ml))
  expect_false(is.null(auto))
})

test_that("items are dropped with a reason and restored, and the step is recorded", {
  st <- ns_nine_state()
  it <- shiny::isolate(st$meta)$item
  out <- sieve_set_item_status(st, c(it[1], it[2], "not_an_item"), "parked", "wording overlaps the third")
  meta <- shiny::isolate(st$meta)
  expect_identical(out$changed, it[1:2])
  expect_identical(out$unchanged, "not_an_item")
  expect_identical(meta$status[1:2], c("parked", "parked"))
  expect_identical(meta$exclude_reason[1], "wording overlaps the third")
  log <- paste(shiny::isolate(st$codelog), collapse = "\n")
  expect_match(log, sprintf('meta$status[meta$item %%in%% c("%s", "%s")] <- "parked"', it[1], it[2]), fixed = TRUE)
  expect_match(log, "wording overlaps the third", fixed = TRUE)
  # Dropping again changes nothing.
  expect_length(sieve_set_item_status(st, it[1], "parked")$changed, 0L)
  back <- sieve_set_item_status(st, it[1], "active")
  meta <- shiny::isolate(st$meta)
  expect_identical(back$changed, it[1])
  expect_identical(meta$status[1], "active")
  expect_true(is.na(meta$exclude_reason[1]))
  # The pickers label items by name, variable and construct.
  ch <- sieve_drop_choices(meta, "active")
  expect_true(it[1] %in% ch)
  expect_false(it[2] %in% ch)
  expect_true(any(grepl("Visual", names(ch), fixed = TRUE)))
})

test_that("the Drop or restore items dialog drops and restores through the ledger", {
  st <- ns_nine_state()
  it <- shiny::isolate(st$meta)$item
  shiny::testServer(mod_drop_server, args = list(state = st), {
    session$setInputs(drop_pick = it[4:5], drop_reason = "low item-total r")
    session$setInputs(drop_go = 1L)
    meta <- shiny::isolate(state$meta)
    expect_identical(meta$status[4:5], c("parked", "parked"))
    expect_identical(meta$exclude_reason[4], "low item-total r")
    expect_match(ns_strip(output$body$html), "Excluded items (2)", fixed = TRUE)
    session$setInputs(restore_pick = it[5])
    session$setInputs(restore_go = 1L)
    meta <- shiny::isolate(state$meta)
    expect_identical(meta$status[5], "active")
    # At least two items stay included.
    keep <- setdiff(meta$item[meta$status == "active"], it[1])
    session$setInputs(drop_pick = keep)
    session$setInputs(drop_go = 2L)
    now <- shiny::isolate(state$meta)
    expect_identical(now$status[match(keep, now$item)], rep("active", length(keep)))
  })
})

test_that("the methods draft is fitted under the study's estimator and says so", {
  st <- ns_nine_state()
  items <- shiny::isolate(st$items)
  meta <- shiny::isolate(st$meta)
  ml <- sieve_methods_draft(items, meta, estimator = "ml")
  # The sentence says that complete cases is SIEVER's setting.
  expect_match(ml, "maximum likelihood (ML), with every item treated as continuous, on the complete cases (listwise deletion, SIEVER's setting for this estimator)", fixed = TRUE)
  # The default (Follow the item types) is ML with FIML; a chosen MLR is
  # named as robust maximum likelihood.
  auto <- sieve_methods_draft(items, meta)
  expect_match(auto, "maximum likelihood (ML), with every item treated as continuous, with full information maximum likelihood", fixed = TRUE)
  expect_false(grepl("robust maximum likelihood (MLR", auto, fixed = TRUE))
  mlr <- sieve_methods_draft(items, meta, estimator = "mlr")
  expect_match(mlr, "robust maximum likelihood (MLR", fixed = TRUE)
})

test_that("the Export panel says when respondents removed on the Screening tab are not in the files", {
  st <- ns_nine_state()
  shiny::testServer(mod_repro_server, args = list(state = st), {
    expect_null(output$export_rows_note)
    sieve_remove_cases(state, c(3L, 7L), reason = "straight-lining")
    session$flushReact()
    expect_match(ns_strip(output$export_rows_note$html),
      "2 respondents removed from the analysis on the Screening tab are not in the data file",
      fixed = TRUE
    )
  })
})

test_that("the wait of an MLR fit grows with the square of the item pairs, as measured", {
  local_measured_rates()
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  # Equal to the pairs at 24 items, where the default rates were measured.
  expect_equal(sieve_fit_units(24, "MLR"), sieve_pairs(24))
  expect_equal(sieve_fit_units(68, "MLR"), sieve_pairs(68)^2 / 276)
  expect_equal(sieve_fit_units(68, "ML"), sieve_pairs(68))
  expect_equal(sieve_fit_units(c(10, 20), "WLSMV"), sieve_pairs(c(10, 20)))
  # At the measured rates a 68-item MLR fit is predicted at about 25 seconds.
  # Before this process has a measurement, larger pools receive a twofold
  # allowance; an observed rate then replaces that initial allowance.
  measured_mlr <- 0.37 / 276 * sieve_pairs(68)^2 / 276
  measured_ml <- 0.37 / 276 * sieve_pairs(68)
  expect_equal(sieve_fit_wait(68, "MLR", "cfa"), 2 * measured_mlr, tolerance = 1e-12)
  expect_equal(sieve_fit_wait(68, "ML", "cfa"), 2 * measured_ml, tolerance = 1e-12)
  sieve_rate_note("cfa_MLR", measured_mlr, sieve_fit_units(68, "MLR"))
  sieve_rate_note("cfa_ML", measured_ml, sieve_fit_units(68, "ML"))
  expect_equal(sieve_fit_wait(68, "MLR", "cfa"), measured_mlr, tolerance = 1e-12)
  expect_equal(sieve_fit_wait(68, "ML", "cfa"), measured_ml, tolerance = 1e-12)
  # A fresh measured time is recorded per unit, so it predicts the same model.
  sieve_rate_reset()
  sieve_rate_note("cfa_MLR", 10, sieve_fit_units(40, "MLR"))
  expect_equal(sieve_fit_wait(40, "MLR", "cfa"), 10, tolerance = 1e-12)
  expect_equal(sieve_demand_wait("cfa", 40, "MLR"), 10, tolerance = 1e-12)
})

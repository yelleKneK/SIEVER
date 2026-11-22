# The CFA, bifactor and SEM tabs: fit guidance without verdicts, the tab's
# opening view, the intervals the tables state, the RMSEA planner, the
# bifactor model's group factors, boundary and improper solutions, and the
# mediation models of the SEM tab.

rm_text <- function(x) {
  if (is.null(x)) return("")
  txt <- gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
  txt <- gsub("&gt;", ">", gsub("&lt;", "<", txt, fixed = TRUE), fixed = TRUE)
  gsub("&amp;", "&", txt, fixed = TRUE)
}

rm_cfa_ui_html <- function() {
  as.character(htmltools::renderTags(mod_cfa_ui("cfa"))$html)
}

# ---- CFA fit guidance without verdicts -------------------------------------

test_that("the CFA fit guidance gives no reference values and no verbal grades", {
  txt <- rm_text(rm_cfa_ui_html())
  # No verbal grade of fit appears.
  for (grade in c(
    "good fit", "acceptable fit", "reasonable fit", "real trouble", "usual benchmark",
    "guidance, not law", "compatible with close fit"
  )) {
    expect_false(grepl(grade, txt, fixed = TRUE), info = grade)
  }
  # The reading it gives, with its sources.
  expect_match(txt, "Interpret the selected estimator, the degrees of freedom, the residuals, and the RMSEA interval together", fixed = TRUE)
  expect_match(txt, "its upper limit alone does not establish substantial misfit", fixed = TRUE)
  expect_match(txt, "A wide interval expresses uncertainty", fixed = TRUE)
  expect_match(txt, "Hu & Bentler, 1999; Marsh, Hau & Wen, 2004; Kenny, Kaniskan & McCoach, 2015", fixed = TRUE)
  # No list of Hu and Bentler's values is shown "for orientation"; the
  # literature is cited, and no value of an index is offered as a reference.
  expect_false(grepl("Reference values", txt, fixed = TRUE))
  expect_false(grepl("close to .95|close to .06|close to .08|for orientation", txt))
  expect_match(txt, "no value of an index makes a model acceptable or not", fixed = TRUE)
  expect_match(txt, "SIEVER applies none of them", fixed = TRUE)
})

# The bundled nine tests, confirmed and continuous, with the variable roles
# declared (the state the on-demand tests use).
rm_state <- function(fx = sieve_hs9_fixture()) {
  siever_test_study(fx)
}

rm_cfa_inputs <- function(session) {
  session$setInputs(
    scope = "construct", structure = "first", ident = "varone", layout = "tree2",
    what = "std", min_path = 0, resid = TRUE, covs = TRUE
  )
}

# ---- The CFA tab opens on the fitted model ---------------------------------

test_that("the CFA tab opens on the path diagram with the fit, and Model specification is a later sub-tab", {
  html <- rm_cfa_ui_html()
  tabs <- regmatches(html, gregexpr('data-value="[^"]+"', html))[[1]]
  tabs <- unique(sub('^data-value="(.*)"$', "\\1", tabs))
  tabs <- intersect(tabs, c(
    "Model", "Path diagram", "Fit", "Compare structures", "Estimates", "Local Fit"
  ))
  expect_identical(tabs[1], "Path diagram")
  expect_identical(tabs[2], "Fit")
  expect_true(all(c("Estimates", "Local Fit") %in% tabs))
  expect_false("Model specification" %in% tabs)
  # The first sub-tab holds the fit of the model above the diagram.
  first <- sub('.*data-value="Path diagram"', "", html)
  first <- sub('data-value="Fit".*', "", first)
  expect_match(first, 'id="cfa-overview_fit"', fixed = TRUE)
  expect_match(first, 'id="cfa-diagram"', fixed = TRUE)

  st <- rm_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    rm_cfa_inputs(session)
    expect_error(output$overview_fit, class = "shiny.silent.error")
    session$setInputs(fit_go = 1)
    line <- rm_text(output$overview_fit$html)
    m <- fm()
    expect_match(line, "Fit of this model:", fixed = TRUE)
    expect_match(line, sprintf("%s = %.3f", sieve_fit_label(m, "cfi"), m[["cfi"]]), fixed = TRUE)
    # (The interval is at the RMSEA level, 90%, and the line names its
    # level.)
    expect_identical(sieve_fit_ci_level(m), 0.90)
    expect_match(line, sprintf(
      "%s = %.3f, 90%% CI [%.3f, %.3f]", sieve_fit_label(m, "rmsea"),
      m[["rmsea"]], m[["rmsea_lo"]], m[["rmsea_hi"]]
    ), fixed = TRUE)
    expect_match(line, sprintf("SRMR = %.3f", m[["srmr"]]), fixed = TRUE)
  })
})

# ---- The CFA loading and Phi tables state their intervals

test_that("the CFA loading and factor correlation tables state the interval level and method, and the card names what it shows", {
  txt <- rm_text(rm_cfa_ui_html())
  expect_false(grepl("Standardized loadings and variances", txt, fixed = TRUE))
  expect_match(txt, "Standardized loadings with 95% confidence intervals", fixed = TRUE)
  expect_match(txt, "Factor correlations with 95% confidence intervals", fixed = TRUE)
  expect_match(txt, "each interval is the 95% Wald interval, the estimate plus or minus 1.96 standard errors", fixed = TRUE)
  expect_match(txt, "delta method standard error", fixed = TRUE)

  st <- rm_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    rm_cfa_inputs(session)
    session$setInputs(fit_go = 1)
    est <- as.character(output$est_tbl)
    phi <- as.character(output$phi_tbl)
    expect_match(est, "95% CI lower", fixed = TRUE)
    expect_match(est, "95% CI upper", fixed = TRUE)
    expect_match(phi, "95% CI lower", fixed = TRUE)
    # What the tables show is what the text says: std.all estimates with
    # 95 percent Wald intervals.
    ss <- standardized(cfa_run())$value
    rows <- ss[ss$op %in% c("=~", "~~") & ss$lhs != ss$rhs | ss$op == "=~", , drop = FALSE]
    expect_equal(rows$ci.lower, rows$est.std - stats::qnorm(0.975) * rows$se, tolerance = 1e-8)
    expect_equal(rows$ci.upper, rows$est.std + stats::qnorm(0.975) * rows$se, tolerance = 1e-8)
  })
})

# ---- The RMSEA planner says which RMSEA it plans from ----------------------

test_that("the RMSEA planner names the RMSEA it was prefilled with and says its computation is normal theory", {
  txt <- rm_text(rm_cfa_ui_html())
  expect_match(txt, "The computation is normal theory", fixed = TRUE)
  expect_match(txt, "robust under MLR and WLSMV", fixed = TRUE)

  st <- rm_state()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    rm_cfa_inputs(session)
    session$setInputs(fit_go = 1)
    m <- fm()
    expect_identical(sieve_fit_family(m, "index"), "robust")
    # A displayed fit hands the measures to the planner's prefill.
    invisible(output$vb_rmsea)
    session$flushReact()
    prefilled <- round(max(m[["rmsea"]], 0.005), 3)
    session$setInputs(plan_rmsea = prefilled, plan_df = as.integer(m[["df"]]), plan_rmsea_width = 0.05)
    res <- rm_text(output$plan_rmsea_result$html)
    expect_match(res, sprintf("The planning RMSEA is the %s of the model fit on this tab", sieve_fit_label(m, "rmsea")), fixed = TRUE)
    expect_match(res, "RMSEA (robust)", fixed = TRUE)
    expect_match(res, "The computation is normal theory: it plans the width of the maximum likelihood interval", fixed = TRUE)
    # A value typed in is said to be one.
    session$setInputs(plan_rmsea = 0.04)
    res <- rm_text(output$plan_rmsea_result$html)
    expect_match(res, "The planning RMSEA was entered here, not read from the fit.", fixed = TRUE)
  })
})

# ---- The RMSEA plan is logged once, on request, at full precision ----------

test_that("the RMSEA planner logs nothing on display and logs the full-precision call once when asked", {
  st <- rm_state()
  steps <- function() {
    log <- shiny::isolate(st$codelog) %||% character(0)
    grep("ss_aipe_rmsea(", log, fixed = TRUE, value = TRUE)
  }
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    rm_cfa_inputs(session)
    session$setInputs(fit_go = 1)
    invisible(output$vb_rmsea)
    # Inputs whose rounding to three decimals changes the required N from
    # 455 to 466.
    session$setInputs(plan_rmsea = 0.0525, plan_df = 24, plan_rmsea_width = 0.0375)
    shown <- rm_text(output$plan_rmsea_result$html)
    n_app <- as.integer(sub(".*Required N: ([0-9]+)\\..*", "\\1", shown))
    # (The planner plans the interval at the level the Fit sub-tab shows,
    # 90%, DMAR's default for ss_aipe_rmsea(); the direct
    # DMAR call is made at that level.)
    ref <- DMAR::ss_aipe_rmsea(true_rmsea = 0.0525, df = 24, width = 0.0375, conf_level = 0.90)
    expect_identical(n_app, as.integer(ref$value[ref$term == "necessary_N"]))
    # Showing and redrawing the plan writes nothing.
    session$setInputs(plan_rmsea_width = 0.04)
    invisible(output$plan_rmsea_result)
    session$setInputs(plan_rmsea_width = 0.0375)
    invisible(output$plan_rmsea_result)
    expect_length(steps(), 0L)
    # Asked for: one step, whose call returns the N on screen.
    session$setInputs(plan_rmsea_log = 1)
    got <- steps()
    expect_length(got, 1L)
    replay <- eval(str2lang(got))
    expect_identical(as.integer(replay$value[replay$term == "necessary_N"]), n_app)
    expect_match(got, "true_rmsea = 0.0525", fixed = TRUE)
    expect_match(got, "width = 0.0375", fixed = TRUE)
    # A second press with nothing changed adds nothing; redrawing adds nothing.
    session$setInputs(plan_rmsea_log = 2)
    invisible(output$plan_rmsea_result)
    expect_length(steps(), 1L)
  })
})

# ---- The bifactor model of a one-construct study ---------------------------

# The nine tests as one construct whose facets are the three domains.
rm_one_construct <- function(meta, facets = TRUE) {
  m <- meta
  m$facet <- if (facets) m$construct else NA_character_
  m$construct <- "Ability"
  m
}

test_that("a one-construct study with facets gets the facets as group factors, as Compare structures builds them", {
  fx <- sieve_hs9_fixture()
  items <- fx$data[, fx$item_cols, drop = FALSE]
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  one <- rm_one_construct(meta)
  plan <- sieve_bifactor_plan(one, reserved = names(items))
  expect_identical(plan$level, "facet")
  expect_identical(plan$groups, c("Visual", "Textual", "Speed"))
  lines <- strsplit(plan$model, "\n", fixed = TRUE)[[1]]
  expect_length(lines, 4L)
  expect_match(lines[1], "^general =~ ")
  expect_identical(unname(plan$name_map), c("general", "Visual", "Textual", "Speed"))
  # The groups are the ones Compare structures uses for the same ledger.
  cs <- sieve_structure_plans(one, reserved = names(items))
  expect_identical(cs$groups$level, "facet")
  expect_identical(cs$groups$groups, plan$groups)
  # The model is the bifactor model of the three domains: the same fit as
  # the one with the domains as constructs.
  by_facet <- sieve_fit_bifactor(items, one, estimator = "ml")
  by_construct <- sieve_fit_bifactor(items, meta, estimator = "ml")
  expect_false(identical(by_facet$status$code, "not_identified"))
  expect_identical(by_facet$status$code, by_construct$status$code)
  expect_equal(
    unname(lavaan::fitMeasures(by_facet$fit, c("chisq", "df"))),
    unname(lavaan::fitMeasures(by_construct$fit, c("chisq", "df"))),
    tolerance = 1e-8
  )
  expect_identical(by_facet$indices$factor, c("general", "Visual", "Textual", "Speed"))
  expect_equal(by_facet$indices$omega_H, by_construct$indices$omega_H, tolerance = 1e-6)
})

test_that("a one-construct study without facets is refused with the reason, before anything is fit", {
  fx <- sieve_hs9_fixture()
  items <- fx$data[, fx$item_cols, drop = FALSE]
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  flat <- rm_one_construct(meta, facets = FALSE)
  plan <- sieve_bifactor_plan(flat, reserved = names(items))
  expect_identical(plan$model, "")
  expect_match(plan$reason, "one construct ('Ability') that is not split into two or more facets", fixed = TRUE)
  expect_match(plan$reason, "two uncorrelated factors on the same items are not identified", fixed = TRUE)
  runs <- 0L
  real_run <- sieve_run_model
  local_mocked_bindings(sieve_run_model = function(...) {
    runs <<- runs + 1L
    real_run(...)
  })
  out <- sieve_fit_bifactor(items, flat)
  expect_null(out$fit)
  expect_identical(out$error, plan$reason)
  expect_identical(runs, 0L)
  # The tab says why before the button is pressed, and fits nothing.
  st <- rm_state()
  shiny::isolate({ st$meta <- flat; sieve_confirm_types(st) })
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    expect_match(rm_text(output$status), "not split into two or more facets", fixed = TRUE)
    session$setInputs(fit = 1)
    expect_null(res())
  })
  expect_identical(runs, 0L)
})

# ---- One rule for the number of group factors ------------------------------

test_that("the Bifactor tab and Compare structures need the same three group factors and refuse for the same reason", {
  hs <- sieve_hs_fixture()
  items24 <- hs$data[, hs$item_cols, drop = FALSE]
  meta24 <- sieve_build_meta(hs$data, hs$item_cols, hs$theory_map, labels = hs$labels)
  same_reason <- function(tab, compare) {
    tab <- sub("[.]$", "", tab)
    expect_identical(compare, paste0(tolower(substr(tab, 1, 1)), substring(tab, 2)))
  }
  # Two group factors: Verbal and Memory.
  two <- meta24
  two$status[!two$construct %in% c("Verbal", "Memory")] <- "parked"
  bf <- sieve_fit_bifactor(items24, two, estimator = "ml")
  expect_null(bf$fit)
  expect_match(bf$error, "A bifactor model needs at least three group factors of at least three included items each", fixed = TRUE)
  expect_match(bf$error, "the included items give 2 (Verbal, 5 items; Memory, 6 items)", fixed = TRUE)
  cs <- sieve_structure_plans(two, reserved = names(items24))
  expect_false(cs$structures$bifactor$available)
  same_reason(bf$error, cs$structures$bifactor$reason)

  # Three constructs, one of them too small for a group factor: two group
  # factors, refused by both (Compare structures does not fit this one).
  fx <- sieve_hs9_fixture()
  items9 <- fx$data[, fx$item_cols, drop = FALSE]
  meta9 <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  small <- meta9
  small$status[small$item == small$item[small$construct == "Speed"][1]] <- "parked"
  bf_small <- sieve_fit_bifactor(items9, small, estimator = "ml")
  expect_null(bf_small$fit)
  cs_small <- sieve_structure_plans(small, reserved = names(items9))
  expect_false(cs_small$structures$bifactor$available)
  same_reason(bf_small$error, cs_small$structures$bifactor$reason)

  # Three group factors: both build the same model.
  cs9 <- sieve_structure_plans(meta9, reserved = names(items9))
  expect_true(cs9$structures$bifactor$available)
  expect_identical(cs9$structures$bifactor$plan$model, sieve_bifactor_plan(meta9, reserved = names(items9))$model)

  # The reason given, shown on the data: two group factors of three items
  # each are not identified.
  vt <- meta9[meta9$construct %in% c("Visual", "Textual"), , drop = FALSE]
  syntax <- paste(
    sprintf("g =~ %s", paste(vt$item, collapse = " + ")),
    sprintf("V =~ %s", paste(vt$item[vt$construct == "Visual"], collapse = " + ")),
    sprintf("T =~ %s", paste(vt$item[vt$construct == "Textual"], collapse = " + ")),
    sep = "\n"
  )
  two3 <- suppressWarnings(sieve_fit_cfa(items9[, vt$item], vt, syntax = syntax, orthogonal = TRUE, estimator = "ml"))
  expect_identical(SIEVER:::sieve_status_of(two3)$code, "not_identified")
})

# ---- The bifactor group rows say what they hold ----------------------------

test_that("the bifactor indices table names the group rows' ECV and omega and shows the group factor's own share", {
  hs <- sieve_hs_fixture()
  st <- rm_state(hs)
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    session$setInputs(fit = 1)
    r <- res()
    expect_identical(r$status$code, "ok")
    html <- paste(as.character(unlist(output$idx_tbl)), collapse = " ")
    expect_match(html, "ECV of the general factor (all items, or the group's items)", fixed = TRUE)
    expect_match(html, "ECV of the group factor (its items)", fixed = TRUE)
    # (bifactor_indices() omega has the model-implied denominator, and the label says so.)
    expect_match(html, "Omega (model-implied denominator; total, or subscale in a group row)", fixed = TRUE)
    # The group factor's own share, by hand from the standardized loadings:
    # for the Reasoning row it is about .07, where DMAR's ECV (the
    # general factor's share within the group) is about .93.
    ss <- lavaan::standardizedSolution(r$fit)
    L <- ss[ss$op == "=~", ]
    gen <- r$spec$latent[1]
    for (i in seq_len(nrow(r$indices))[-1]) {
      grp_latent <- names(r$name_map)[r$name_map == r$indices$factor[i]]
      its <- L$rhs[L$lhs == grp_latent]
      ls <- L$est.std[L$lhs == grp_latent]
      lg <- L$est.std[L$lhs == gen][match(its, L$rhs[L$lhs == gen])]
      own <- sum(ls^2) / (sum(ls^2) + sum(lg^2))
      expect_equal(1 - r$indices$ECV[i], own, tolerance = 1e-10)
    }
    reasoning <- which(r$indices$factor == "Reasoning")
    expect_lt(1 - r$indices$ECV[reasoning], 0.10)
    # The interval table names the group rows' quantities the same way.
    expect_identical(
      idx_label(c("ECV", "omega", "ECV", "omega", "omega_HS"), c("general", "general", "Verbal", "Verbal", "Verbal")),
      # (bifactor_indices() omega has the model-implied denominator, and the label says so.)
      c(
        "ECV", "Omega (model-implied denominator)", "ECV of the general factor within the group's items",
        "Omega (model-implied denominator; subscale)", "omegaHS"
      )
    )
  })
  txt <- rm_text(htmltools::renderTags(mod_bifactor_ui("bifactor"))$html)
  expect_match(txt, "In a group factor's row they are computed over that group's items only", fixed = TRUE)
})

# ---- Boundary and improper solutions in the tests --------------------------

test_that("Compare structures flags the bifactor versus one-factor test as a boundary case and annotates tests with an improper solution", {
  fx <- sieve_hs9_fixture()
  items <- fx$data[, fx$item_cols, drop = FALSE]
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  res <- sieve_compare_structures(meta, items, estimator = "ml")
  expect_identical(res$structures$bifactor$run$status$code, "inadmissible")
  t <- res$tests
  bo <- which(t$less == "bifactor" & t$more == "one_factor")
  bh <- which(t$less == "bifactor" & t$more == "higher_order")
  expect_true(t$boundary[bo])
  expect_true(t$available[bo])
  expect_match(t$note[bo], "Fixing the group loadings at zero is the same as fixing the group factors' variances at zero", fixed = TRUE)
  expect_match(t$note[bo], "boundary of the parameter space", fixed = TRUE)
  for (i in c(bo, bh)) {
    expect_match(t$note[i], "The Bifactor solution is improper", fixed = TRUE)
    expect_match(t$note[i], "reference distribution of this test is not assured", fixed = TRUE)
  }
  # Pairs between admissible solutions carry no such note.
  for (i in which(t$available & t$less != "bifactor")) {
    expect_false(grepl("improper", t$note[i] %||% "", fixed = TRUE))
  }
  # The recorded R says the same before each test it concerns.
  code <- sieve_structures_code(res)
  at <- grep('lavaan::lavTestLRT(fits_structures[["bifactor"]], fits_structures[["one_factor"]]', code, fixed = TRUE)
  expect_length(at, 1L)
  before <- paste(code[seq_len(at - 1L)], collapse = " ")
  expect_match(sub(".*lavTestLRT", "", before), "The Bifactor solution is improper", fixed = TRUE)
})

# ---- Two mediators need an order (serial mediation) ------------------------

test_that("two constructs chosen as both predictor and outcome are refused without an order and fit as serial mediation with one", {
  hs <- sieve_hs_fixture()
  items <- hs$data[, hs$item_cols, drop = FALSE]
  meta <- sieve_build_meta(hs$data, hs$item_cols, hs$theory_map, labels = hs$labels)
  outcomes <- c("Verbal", "Speed", "Memory")
  predictors <- c("Spatial", "Verbal", "Speed")
  kinds <- rep("construct", 3)
  # No order, so each mediator would be regressed on
  # the other. Refused with the reason, and nothing is fit.
  plan <- sieve_structural_plan(meta, outcomes, predictors,
    outcome_kind = kinds, effects = TRUE, reserved = names(hs$data)
  )
  expect_identical(plan$model, "")
  expect_match(plan$problems, "The constructs 'Verbal' and 'Speed' are each chosen as both a predictor and an outcome", fixed = TRUE)
  expect_match(plan$problems, "a feedback loop (a nonrecursive model)", fixed = TRUE)
  fits <- 0L
  real_lavaan <- lavaan::lavaan
  local_mocked_bindings(lavaan = function(...) {
    fits <<- fits + 1L
    real_lavaan(...)
  }, .package = "lavaan")
  refused <- sieve_run_structural(items, hs$data, meta, outcomes, predictors,
    outcome_kind = kinds, effects = TRUE, estimator = "ml"
  )
  expect_null(refused$fit)
  expect_match(refused$error, "Set the order of the mediators", fixed = TRUE)
  expect_identical(fits, 0L)

  # With the order Verbal, then Speed: a recursive model.
  run <- sieve_run_structural(items, hs$data, meta, outcomes, predictors,
    outcome_kind = kinds, effects = TRUE, estimator = "ml",
    mediator_order = c("Verbal", "Speed")
  )
  expect_identical(run$status$code, "ok")
  pe <- lavaan::parameterEstimates(run$fit)
  reg <- pe[pe$op == "~", ]
  lat <- attr(run$fit, "lat_map")
  name_of <- function(x) names(lat)[match(x, lat)]
  expect_false(any(reg$lhs == name_of("Verbal") & reg$rhs == name_of("Speed")))
  expect_true(any(reg$lhs == name_of("Speed") & reg$rhs == name_of("Verbal")))
  emap <- attr(run$fit, "eff_map")
  expect_true("Spatial -> Verbal -> Speed -> Memory (indirect)" %in% emap)
  # Every total effect is the element of (I - B)^-1 - I of the
  # standardized solution: the sum over every chain.
  B <- lavaan::lavInspect(run$fit, "std")$beta
  tot <- solve(diag(nrow(B)) - B) - diag(nrow(B))
  ss <- lavaan::standardizedSolution(run$fit)
  defs <- ss[ss$op == ":=", ]
  totals <- defs[grepl("(total", emap[defs$lhs], fixed = TRUE), ]
  expect_gt(nrow(totals), 0L)
  for (i in seq_len(nrow(totals))) {
    xy <- strsplit(sub(" \\(total.*$", "", emap[[totals$lhs[i]]]), " -> ", fixed = TRUE)[[1]]
    expect_equal(totals$est.std[i], tot[name_of(xy[2]), name_of(xy[1])], tolerance = 1e-10, info = emap[[totals$lhs[i]]])
  }
  # The other order is the other model.
  reversed <- sieve_structural_plan(meta, outcomes, predictors,
    outcome_kind = kinds, reserved = names(hs$data), mediator_order = c("Speed", "Verbal")
  )
  expect_length(reversed$problems, 0L)
  expect_match(reversed$model, sprintf("%s ~ %s\n", name_of("Speed"), name_of("Spatial")), fixed = TRUE)
  # One mediator needs no order; its indirect effect is the product of its
  # two paths.
  one <- sieve_structural_plan(meta, c("Verbal", "Speed"), c("Spatial", "Verbal"),
    outcome_kind = c("construct", "construct"), effects = TRUE, reserved = names(hs$data)
  )
  expect_length(one$problems, 0L)
  expect_match(one$model, "ind1 := b1*b3", fixed = TRUE)
})

test_that("the SEM tab asks for the order of two mediators before fitting and fits the serial model once it is set", {
  hs <- sieve_hs_fixture()
  st <- rm_state(hs)
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(
      outcomes = c("Verbal", "Speed", "Memory"), predictors = c("Spatial", "Verbal", "Speed"),
      layout = "tree2", min_path = 0
    )
    expect_identical(mediators_now(), c("Verbal", "Speed"))
    expect_match(rm_text(output$mediator_order_ui), "Order of the mediators", fixed = TRUE)
    gate <- rm_text(output$gate)
    expect_match(gate, "Not fit.", fixed = TRUE)
    expect_match(gate, "Set the order of the mediators", fixed = TRUE)
    session$setInputs(fit_go = 1)
    expect_null(demand$result())
    session$setInputs(mediator_order = c("Verbal", "Speed"))
    session$setInputs(fit_go = 2)
    expect_identical(path_run()$status$code, "ok")
    pt <- lavaan::parTable(path_run()$fit)
    lat <- attr(path_run()$fit, "lat_map")
    v <- names(lat)[lat == "Verbal"]
    s <- names(lat)[lat == "Speed"]
    expect_true(any(pt$op == "~" & pt$lhs == s & pt$rhs == v))
    expect_false(any(pt$op == "~" & pt$lhs == v & pt$rhs == s))
  })
})

# ---- The mediation model is fit once ---------------------------------------

test_that("the labeled mediation model is the unlabeled model, so the SEM tab fits it once and both panels read that fit", {
  hs <- sieve_hs_fixture()
  items <- hs$data[, hs$item_cols, drop = FALSE]
  meta <- sieve_build_meta(hs$data, hs$item_cols, hs$theory_map, labels = hs$labels)
  # The equality the single fit rests on, for one
  # mediator and for two ordered mediators.
  same_fit <- function(out, pred, ...) {
    kinds <- rep("construct", length(out))
    plain <- sieve_run_structural(items, hs$data, meta, out, pred,
      outcome_kind = kinds, estimator = "ml", ...
    )
    labeled <- sieve_run_structural(items, hs$data, meta, out, pred,
      outcome_kind = kinds, effects = TRUE, estimator = "ml", ...
    )
    expect_identical(plain$status$code, "ok")
    expect_identical(labeled$status$code, "ok")
    expect_equal(
      unname(lavaan::fitMeasures(labeled$fit, c("fmin", "chisq", "df", "npar"))),
      unname(lavaan::fitMeasures(plain$fit, c("fmin", "chisq", "df", "npar"))),
      tolerance = 1e-10
    )
    sp <- lavaan::standardizedSolution(plain$fit)
    sl <- lavaan::standardizedSolution(labeled$fit)
    sl <- sl[sl$op != ":=", ]
    expect_identical(paste(sl$lhs, sl$op, sl$rhs), paste(sp$lhs, sp$op, sp$rhs))
    expect_equal(sl$est.std, sp$est.std, tolerance = 1e-10)
    expect_equal(sl$se, sp$se, tolerance = 1e-8)
  }
  same_fit(c("Verbal", "Speed"), c("Spatial", "Verbal"))

  # The tab: one model fit for the model and its effects. Counted are the
  # fits SIEVER asks lavaan for (a specification with no problem); one the
  # plan refuses is not fit.
  st <- rm_state(hs)
  fits <- 0L
  real_run <- sieve_run_model
  local_mocked_bindings(sieve_run_model = function(spec, ...) {
    if (!length(spec$problems)) fits <<- fits + 1L
    real_run(spec, ...)
  })
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(outcomes = c("Verbal", "Speed"), predictors = c("Spatial", "Verbal"), layout = "tree2", min_path = 0)
    expect_match(rm_text(output$gate), "(with labeled paths for the indirect and total effects)", fixed = TRUE)
    expect_false(grepl("twice", rm_text(output$gate), fixed = TRUE))
    session$setInputs(fit_go = 1)
    expect_identical(fits, 1L)
    expect_identical(path_run()$status$code, "ok")
    expect_identical(med_run()$analysis_id, path_run()$analysis_id)
    expect_identical(med_fit(), fit())
    invisible(output$paths_tbl)
    invisible(output$effects_tbl)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_false(grepl("sem_fit_effects", log, fixed = TRUE))
    expect_match(log, "sem_std_effects <- lavaan::standardizedSolution(sem_fit)", fixed = TRUE)
    expect_match(log, "defined parameters of this one fit", fixed = TRUE)
    expect_identical(fits, 1L)
    # A mediator with no outcome it predicts: the effects cannot be defined,
    # so the model is fit without them and the panel says why.
    session$setInputs(outcomes = "Verbal", predictors = c("Spatial", "Verbal"))
    session$setInputs(fit_go = 2)
    expect_identical(path_run()$status$code, "ok")
    expect_null(med_run()$fit)
    expect_match(rm_text(output$effects_note), "no X -> M -> Y chain", fixed = TRUE)
    expect_identical(fits, 2L)
  })
  # The same equality with two ordered mediators.
  same_fit(c("Verbal", "Speed", "Memory"), c("Spatial", "Verbal", "Speed"), mediator_order = c("Verbal", "Speed"))
})

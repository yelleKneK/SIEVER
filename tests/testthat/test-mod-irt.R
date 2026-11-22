# Tests for the item response theory module (R/mod_irt.R). The module owns no
# statistics of its own: the graded response model is DMAR::irt_grm() and the
# information functions are DMAR::irt_information(), so every numeric test
# below pins the module's output against a direct DMAR call on the same data.
# The gate is tested on the bundled continuous example, which is what a user
# meets first, and the identities that make the information function readable
# (additivity across items, SE = 1/sqrt(I)) are tested as identities.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

# The bundled fixture: nine continuous Holzinger and Swineford tests.
irt_state <- function(confirmed = TRUE) {
  fx <- sieve_hs9_fixture()
  s <- sieve_new_state()
  s$dataset_name <- fx$name
  s$raw <- fx$data
  s$items <- fx$data[, fx$item_cols, drop = FALSE]
  s$id_vars <- fx$id_vars
  s$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map,
    labels = fx$labels
  )
  s$theory <- list(k = fx$theory_k, label = fx$theory_lbl, map = fx$theory_map)
  s$confirmed <- confirmed
  s$palette <- "nd"
  s$thresholds <- sieve_default_thresholds()
  s
}

# The same fixture cut into five ordered categories per item and confirmed as
# ordered, which is the situation the graded response model is defined for.
# This mirrors the discretization test-dmar-api-shapes.R uses for categorical
# omega, so the two tabs are exercised on the same synthetic ordinal data.
irt_state_ordered <- function() {
  s <- irt_state()
  shiny::isolate({
    s$items <- as.data.frame(lapply(s$items, function(v) {
      as.integer(cut(as.numeric(v), breaks = 5))
    }))
    meta <- s$meta
    meta$type <- "ordered"
    s$meta <- meta
  })
  s
}

# The item matrix the module hands to DMAR, reproduced for the direct calls.
irt_scope_data <- function(s, items) {
  d <- shiny::isolate(s$items)[, items, drop = FALSE]
  d[] <- lapply(d, function(v) suppressWarnings(as.numeric(v)))
  d
}

irt_inputs <- function(session, ...) {
  defaults <- list(
    scope = "__all__", metric = "normal_ogive",
    estimator = "WLSMV", theta_min = -4, theta_max = 4,
    theta_n = 81, info_threshold = 10
  )
  overrides <- list(...)
  do.call(session$setInputs, utils::modifyList(defaults, overrides))
}

test_that("continuous items are gated with an actionable message and nothing is fit", {
  st <- irt_state()
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session)
    session$setInputs(fit_go = 1)
    g <- irt_gate()
    expect_false(isTRUE(g$ok))
    expect_identical(g$kind, "not_ordered")

    html <- as.character(output$gate$html)
    # The model requirement, stated as a requirement.
    expect_match(html, "ordered categorical responses")
    expect_match(html, "Samejima")
    # The attenuation argument for the categorical estimator, with its source.
    expect_match(html, "attenuates")
    expect_match(html, "Rhemtulla")
    # Exactly what the user must change, named as it appears in the app.
    expect_match(html, "Ingest")
    expect_match(html, "Confirm the study", fixed = TRUE)
    # And the error the message must not invite.
    expect_match(html, "do not relabel", ignore.case = TRUE)

    # Nothing was estimated: no model, no information, no logged DMAR call.
    expect_null(grm_fit())
    expect_null(info_res())
    expect_null(coverage())
    expect_null(item_peaks())
    log <- shiny::isolate(state$codelog)
    expect_false(any(grepl("irt_grm", log, fixed = TRUE)))
    expect_false(any(grepl("irt_information", log, fixed = TRUE)))
  })
})

test_that("the pipeline gate applies before the model gate", {
  st <- irt_state(confirmed = FALSE)
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session)
    session$setInputs(fit_go = 1)
    g <- irt_gate()
    expect_false(isTRUE(g$ok))
    expect_match(as.character(output$gate$html), "await confirmation")
    expect_null(grm_fit())
  })
})

test_that("ordered items reproduce DMAR::irt_grm() to machine precision", {
  st <- irt_state_ordered()
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session)
    session$setInputs(fit_go = 1)
    expect_true(isTRUE(irt_gate()$ok))
    g <- grm_fit()
    expect_null(g$error)

    its <- shiny::isolate(scope_items())
    expect_length(its, 9L)
    ref <- DMAR::irt_grm(irt_scope_data(st, its),
      items = its,
      estimator = "WLSMV", metric = "normal_ogive"
    )

    got <- g$grm
    expect_identical(as.character(got$item), as.character(ref$item))
    expect_identical(got$category, ref$category)
    expect_equal(got$lambda, ref$lambda, tolerance = 1e-12)
    expect_equal(got$tau, ref$tau, tolerance = 1e-12)
    expect_equal(got$a, ref$a, tolerance = 1e-12)
    expect_equal(got$b, ref$b, tolerance = 1e-12)
    # The factor analytic origin of a and b, which the table shows alongside.
    expect_equal(got$a, got$lambda / sqrt(1 - got$lambda^2), tolerance = 1e-12)
    expect_equal(got$b, got$tau / got$lambda, tolerance = 1e-12)
    # The attributes the module reads for the model note and the fit table.
    expect_identical(attr(got, "estimator"), "WLSMV")
    expect_identical(attr(got, "metric"), "normal_ogive")
    expect_identical(unname(attr(got, "n_categories")), rep(5L, 9L))
    expect_true(is.finite(attr(got, "N")))

    # The rendered table carries lambda and tau beside a and b, so the factor
    # analytic origin of the item response theory parameters is visible, and it
    # shows both discrimination metrics side by side.
    tbl <- as.character(output$param_tbl)
    for (h in c(
      "Lambda", "Tau", "a (normal ogive)", "a (logistic)",
      "b (location)", "Boundary k", "Variable"
    )) {
      expect_match(tbl, h, fixed = TRUE)
    }

    # The fit of the model whose solution was converted travels with it.
    fm <- sieve_fit_measures(attr(got, "fit"))
    expect_true(all(is.finite(fm[c(
      "cfi", "rmsea", "rmsea_lo", "rmsea_hi",
      "srmr"
    )])))
    expect_match(as.character(output$fit_note$html), "fitMeasures", fixed = TRUE)
  })
})

test_that("a zero-df three-item model distinguishes fit conventions from evidence", {
  st <- irt_state_ordered()
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session, scope = "Visual")
    session$setInputs(fit_go = 1)
    fm <- sieve_fit_measures(attr(grm_fit()$grm, "fit"))
    expect_equal(unname(fm[["df"]]), 0)
    expect_match(as.character(output$fit_note$html),
      "zero degrees of freedom",
      fixed = TRUE
    )
    expect_no_match(as.character(output$fit_note$html), "reproduces them exactly", fixed = TRUE)
    expect_match(as.character(output$fit_note$html), "convergence and admissibility", fixed = TRUE)
    # The parameters and the information function are still defined.
    expect_true(all(is.finite(grm_fit()$grm$a)))
    expect_true(all(is.finite(info_res()$info$test_information)))
  })
})

test_that("the metric toggle rescales the displayed a by 1.702 without refitting", {
  # The two metrics differ by the exact constant 1.702 on the slope and not at
  # all on the location, so the radio is a display control: it must rebuild the
  # table and leave the fit alone. A refit here is an eight second freeze on a
  # WLSMV model, and it is counted rather than inferred.
  st <- irt_state_ordered()
  n_fits <- 0L
  real_irt_grm <- DMAR::irt_grm
  testthat::local_mocked_bindings(
    irt_grm = function(...) {
      n_fits <<- n_fits + 1L
      real_irt_grm(...)
    },
    .package = "DMAR"
  )
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session)
    session$setInputs(fit_go = 1)
    g_no <- grm_fit()
    expect_null(g_no$error)
    expect_identical(attr(g_no$grm, "metric"), "normal_ogive")
    p_no <- param_table()
    expect_identical(p_no$metric, "normal_ogive")
    expect_equal(p_no$df$a, g_no$grm$a, tolerance = 1e-12)
    expect_identical(n_fits, 1L)

    ir_no <- info_res()
    expect_null(ir_no$error)

    session$setInputs(metric = "logistic")
    g_lo <- grm_fit()
    p_lo <- param_table()
    # The displayed column is exactly 1.702 times the normal ogive slope, the
    # complementary column is the normal ogive slope, and b does not move.
    expect_identical(p_lo$metric, "logistic")
    expect_equal(p_lo$df$a, 1.702 * p_no$df$a, tolerance = 1e-12)
    expect_equal(p_lo$df$a_other, p_no$df$a, tolerance = 1e-12)
    expect_equal(p_lo$df$b, p_no$df$b, tolerance = 1e-12)
    expect_identical(
      p_lo$colnames[6:7],
      c("a (logistic)", "a (normal ogive)")
    )
    expect_identical(
      p_no$colnames[6:7],
      c("a (normal ogive)", "a (logistic)")
    )

    # The fit itself is untouched: the same object, from the same single call
    # to the estimator, still in the normal ogive metric.
    expect_identical(n_fits, 1L)
    expect_identical(g_lo, g_no)
    expect_identical(attr(g_lo$grm, "metric"), "normal_ogive")

    # Information is a property of the model, not of how a is written down, so
    # it is computed on the normal ogive slopes and does not change either.
    ir_lo <- info_res()
    expect_equal(ir_lo$params$a, g_no$grm$a, tolerance = 1e-12)
    expect_equal(ir_lo$info$test_information, ir_no$info$test_information,
      tolerance = 1e-12
    )

    # The counter is sensitive rather than stuck: a control that genuinely
    # changes the model does refit, which is what makes the two assertions
    # above evidence and not an artifact of a mock that never fires.
    session$setInputs(estimator = "ULSMV")
    session$setInputs(fit_go = 2)
    invisible(grm_fit())
    expect_identical(n_fits, 2L)
  })
})

test_that("the estimator selector reaches the model and the log", {
  # Every other test in this file runs at the WLSMV default, so wiring the
  # five option selector out of the module would go unnoticed. ULSMV is the
  # right contrast: DWLS and WLSMV are the same estimator with different test
  # statistic corrections and give identical point estimates, so only an
  # unweighted least squares fit shows that the control changed the model.
  st <- irt_state_ordered()
  a_wlsmv <- NULL
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session)
    session$setInputs(fit_go = 1)
    a_wlsmv <<- grm_fit()$grm$a
  })
  st2 <- irt_state_ordered()
  shiny::testServer(mod_irt_server, args = list(state = st2), {
    irt_inputs(session, estimator = "ULSMV")
    session$setInputs(fit_go = 1)
    g <- grm_fit()
    expect_null(g$error)
    expect_identical(attr(g$grm, "estimator"), "ULSMV")
    expect_identical(g$estimator, "ULSMV")
    # A different estimator is a different fit, not a relabeling.
    expect_false(isTRUE(all.equal(g$grm$a, a_wlsmv, tolerance = 1e-8)))
    # The rendered note names the estimator that actually ran.
    expect_match(as.character(output$model_note$html), "ULSMV", fixed = TRUE)
    # And the logged R carries it, so the script reproduces this fit.
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl('estimator = "ULSMV"', log, fixed = TRUE)))
    expect_false(any(grepl('estimator = "WLSMV"', log, fixed = TRUE)))
  })
})

test_that("the information table reproduces DMAR::irt_information()", {
  st <- irt_state_ordered()
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session)
    session$setInputs(fit_go = 1)
    its <- shiny::isolate(scope_items())
    ref_grm <- DMAR::irt_grm(irt_scope_data(st, its),
      items = its,
      estimator = "WLSMV", metric = "normal_ogive"
    )
    theta <- seq(-4, 4, length.out = 81)
    ref <- DMAR::irt_information(
      grm = data.frame(
        item = as.character(ref_grm$item),
        category = ref_grm$category,
        a = ref_grm$a, b = ref_grm$b,
        stringsAsFactors = FALSE
      ),
      trait_value = theta
    )

    ir <- info_res()
    expect_null(ir$error)
    got <- ir$info
    expect_equal(got$trait_value, ref$trait_value, tolerance = 1e-12)
    expect_equal(got$test_information, ref$test_information, tolerance = 1e-12)
    expect_equal(got$se, ref$se, tolerance = 1e-12)
    expect_equal(attr(got, "item_information"), attr(ref, "item_information"),
      tolerance = 1e-12
    )
    expect_equal(attr(got, "trait_value_max_information"),
      attr(ref, "trait_value_max_information"),
      tolerance = 1e-12
    )
    expect_identical(colnames(attr(got, "item_information")), its)
  })
})

test_that("test information is the row sum of item information and SE is 1/sqrt(I)", {
  st <- irt_state_ordered()
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session)
    session$setInputs(fit_go = 1)
    got <- info_res()$info
    M <- attr(got, "item_information")
    # Information is additive across items: that is what makes the item
    # figure decompose the test figure.
    expect_equal(got$test_information, unname(rowSums(M)), tolerance = 1e-12)
    # Precision as a function of where the respondent sits.
    expect_equal(got$se, 1 / sqrt(got$test_information), tolerance = 1e-12)
    expect_true(all(is.finite(got$test_information)))
    expect_true(all(got$test_information >= 0))
  })
})

test_that("the theta grid inputs govern the evaluation grid", {
  st <- irt_state_ordered()
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session, theta_min = -3, theta_max = 3, theta_n = 25)
    session$setInputs(fit_go = 1)
    th <- theta_grid()
    expect_length(th, 25L)
    expect_equal(range(th), c(-3, 3), tolerance = 1e-12)
    expect_equal(info_res()$info$trait_value, th, tolerance = 1e-12)
    # A cleared or inverted range falls back to the standard normal window
    # rather than erroring.
    session$setInputs(theta_min = 4, theta_max = -4)
    expect_equal(range(theta_grid()), c(-4, 4), tolerance = 1e-12)
  })
})

test_that("the coverage readout states the SE arithmetic and the covered band", {
  st <- irt_state_ordered()
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session, info_threshold = 4)
    session$setInputs(fit_go = 1)
    cv <- coverage()
    expect_equal(cv$threshold, 4)
    # The stated equivalence: I >= 4 is SE(theta) <= 0.5.
    expect_equal(cv$se_at_threshold, 1 / sqrt(4), tolerance = 1e-12)
    expect_equal(cv$se_at_peak, 1 / sqrt(cv$peak_information), tolerance = 1e-12)

    info <- info_res()$info
    expect_equal(cv$peak_information, max(info$test_information),
      tolerance = 1e-12
    )
    expect_equal(cv$peak_theta, attr(info, "trait_value_max_information"),
      tolerance = 1e-12
    )

    # The band is exactly the stretch of grid values at or above the threshold.
    expect_false(is.null(cv$bands))
    covered <- info$trait_value[info$test_information >= 4]
    expect_equal(min(cv$bands$lower), min(covered), tolerance = 1e-12)
    expect_equal(max(cv$bands$upper), max(covered), tolerance = 1e-12)
    expect_equal(cv$bands$width, cv$bands$upper - cv$bands$lower,
      tolerance = 1e-12
    )
    expect_equal(cv$bands$normal_share,
      stats::pnorm(cv$bands$upper) - stats::pnorm(cv$bands$lower),
      tolerance = 1e-12
    )

    note <- as.character(output$coverage_note$html)
    expect_match(note, "1 / sqrt", fixed = TRUE)
    expect_match(note, "0.500")

    # An unreachable threshold says so instead of showing an empty band.
    session$setInputs(info_threshold = 1e6)
    expect_null(coverage()$bands)
    expect_match(
      as.character(output$coverage_note$html),
      "No part of the evaluated range reaches the threshold"
    )
  })
})

test_that("the per item peak table decomposes the test peak", {
  st <- irt_state_ordered()
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session)
    session$setInputs(fit_go = 1)
    pk <- item_peaks()
    info <- info_res()$info
    M <- attr(info, "item_information")
    expect_identical(pk$item, colnames(M))
    expect_equal(pk$peak_information, unname(apply(M, 2L, max)),
      tolerance = 1e-12
    )
    expect_equal(pk$theta_at_peak, info$trait_value[apply(M, 2L, which.max)],
      tolerance = 1e-12
    )
    # Contribution at the theta where the test as a whole peaks.
    row <- which.max(info$test_information)
    expect_equal(pk$info_at_test_peak, unname(M[row, ]), tolerance = 1e-12)
    expect_equal(sum(pk$share_at_test_peak), 1, tolerance = 1e-12)
    # The discrimination shown beside the peaks is the one the information used.
    expect_equal(pk$a, unname(attr(info, "discrimination")[pk$item]), tolerance = 1e-12)
    # Item labels come from the ledger, not the raw column names.
    expect_true(all(nzchar(pk$label)))
  })
})

test_that("both figures return ggplot objects built in the SIEVER palette", {
  # ggplot2 is imported, so neither figure asks whether it is installed:
  # with sieve_have() answering no, each reactive still returns its ggplot
  # and each panel draws it.
  test_frame <- environment()
  local_mocked_bindings(sieve_have = function(pkg) FALSE, .env = test_frame)
  st <- irt_state_ordered()
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session)
    session$setInputs(fit_go = 1)
    p_test <- test_info_plot()
    expect_true(inherits(p_test, "ggplot"))
    p_item <- item_info_plot()
    expect_true(inherits(p_item, "ggplot"))
    expect_no_error(output$test_info_plot)
    expect_no_error(output$item_info_plot)
    # The item figure is DMAR's, restyled: the SIEVER palette must be the
    # color scale actually attached to the returned plot.
    pal <- sieve_palette_colors("nd", 9L)
    vals <- unlist(lapply(p_item$scales$scales, function(s) {
      if (any(c("colour", "color") %in% s$aesthetics)) {
        tryCatch(s$palette(9L), error = function(e) NULL)
      }
    }))
    expect_true(length(vals) > 0)
    expect_true(all(pal %in% unname(vals)))
  })
})

test_that("the DMAR calls reach the reproducible code log", {
  st <- irt_state_ordered()
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session)
    session$setInputs(fit_go = 1)
    invisible(grm_fit())
    invisible(info_res())
    log <- shiny::isolate(state$codelog)
    expect_true(any(grepl("DMAR::irt_grm(", log, fixed = TRUE)))
    expect_true(any(grepl("DMAR::irt_information(", log, fixed = TRUE)))
    # The logged R must be what actually ran: estimator, metric, grid.
    expect_true(any(grepl('estimator = "WLSMV"', log, fixed = TRUE)))
    expect_true(any(grepl('metric = "normal_ogive"', log, fixed = TRUE)))
    expect_true(any(grepl("length.out = 81", log, fixed = TRUE)))
    # And it must parse as R.
    code <- log[!startsWith(log, "# [")]
    expect_no_error(parse(text = paste(code, collapse = "\n")))
  })
})

test_that("a scope with fewer than three included items is refused with a reason", {
  st <- irt_state_ordered()
  shiny::isolate({
    meta <- st$meta
    meta$status[meta$construct == "Visual"][1] <- "parked"
    st$meta <- meta
  })
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session, scope = "Visual")
    session$setInputs(fit_go = 1)
    g <- irt_gate()
    expect_false(isTRUE(g$ok))
    expect_match(
      as.character(output$gate$html),
      "At least three items are needed"
    )
    expect_null(grm_fit())
  })
})

test_that("a construct scope fits only that construct's items", {
  st <- irt_state_ordered()
  shiny::testServer(mod_irt_server, args = list(state = st), {
    irt_inputs(session, scope = "Textual")
    session$setInputs(fit_go = 1)
    its <- shiny::isolate(scope_items())
    expect_length(its, 3L)
    g <- grm_fit()
    expect_null(g$error)
    expect_identical(sort(unique(as.character(g$grm$item))), sort(its))
    expect_identical(colnames(attr(info_res()$info, "item_information")), its)
  })
})

test_that("an item that cannot be read as numbers is refused by name before the model is fit", {
  s <- irt_state_ordered()
  shiny::isolate({
    d <- s$items
    d[[1L]] <- as.character(d[[1L]])
    d[[1L]][1:12] <- "refused"
    s$items <- d
  })
  bad <- names(shiny::isolate(s$items))[1L]
  shiny::testServer(mod_irt_server, args = list(state = s), {
    irt_inputs(session)
    session$setInputs(fit_go = 1)
    g <- irt_gate()
    expect_false(g$ok)
    txt <- paste(as.character(unlist(g$ui)), collapse = " ")
    expect_match(txt, "cannot be read as numbers", fixed = TRUE)
    expect_match(txt, sprintf("Item '%s' holds 12 values that are not numbers", bad), fixed = TRUE)
    expect_null(grm_fit())
  })
})

test_that("a graded response model without a solution shows its status and nothing else", {
  s <- irt_state_ordered()
  its <- names(shiny::isolate(s$items))
  real <- DMAR::irt_grm(irt_scope_data(s, its),
    items = its, estimator = "WLSMV",
    metric = "normal_ogive"
  )
  # A fit with no solution in place of the engine's: two continuous
  # indicators on one factor are not identified. The engine is stood in for
  # here because a graded response model that does not converge is an error
  # inside DMAR::irt_grm(), not a returned fit.
  two <- withr::with_seed(3L, data.frame(a = stats::rnorm(120), b = stats::rnorm(120)))
  bad <- suppressWarnings(lavaan::cfa("F =~ a + b", two, std.lv = TRUE))
  expect_identical(sieve_fit_status(bad)$code, "not_identified")
  fake <- real
  attr(fake, "fit") <- bad
  testthat::local_mocked_bindings(irt_grm = function(...) fake, .package = "DMAR")
  shiny::testServer(mod_irt_server, args = list(state = s), {
    irt_inputs(session)
    session$setInputs(fit_go = 1)
    g <- grm_fit()
    expect_false(is.null(g$error))
    expect_identical(g$status$code, "not_identified")
    note <- paste(as.character(unlist(output$model_note)), collapse = " ")
    expect_match(note, "The graded response model is not identified", fixed = TRUE)
    expect_match(note, "not interpretable", fixed = TRUE)
    expect_null(info_res())
    expect_null(coverage())
    expect_null(param_table())
    expect_error(output$fit_tbl, class = "shiny.silent.error")
    expect_error(output$coverage_tbl, class = "shiny.silent.error")
    # The parameter table is withheld, as the fit and coverage tables are:
    # the status alert above is all the tab shows for the model.
    expect_error(output$param_tbl, class = "shiny.silent.error")
    expect_null(output$test_info_note)
  })
})

test_that("every note on the IRT tab names the model and the estimator behind it", {
  s <- irt_state_ordered()
  shiny::testServer(mod_irt_server, args = list(state = s), {
    irt_inputs(session, estimator = "ULSMV")
    session$setInputs(fit_go = 1)
    expect_null(grm_fit()$error)
    txt <- function(id) paste(as.character(unlist(output[[id]])), collapse = " ")
    for (id in c("model_note", "test_info_note", "item_info_note", "coverage_note", "peak_note")) {
      expect_match(txt(id), "graded response model", fixed = TRUE, info = id)
      expect_match(txt(id), "ULSMV", fixed = TRUE, info = id)
    }
    expect_match(txt("fit_note"), "lavaan::fitMeasures()", fixed = TRUE)
  })
})

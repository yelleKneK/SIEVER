# Tests of the CV and Invariance tab on DMAR:
# the invariance ladder comes from DMAR::measurement_invariance() (including
# the five-rung ordered ladder), the fitted objects it returns are reused
# rather than refit, the dMACS effect size of noninvariance comes from
# DMAR::dmacs() and is refused where it would be meaningless, and the
# alignment panel comes from DMAR::measurement_alignment(). Every test drives
# the real module server with shiny::testServer on the bundled fixture.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

make_state <- function(confirmed = TRUE) {
  siever_test_study(confirmed = confirmed, configure = function(s) s$roles <- NULL)
}

# The same state, with the nine tests coarsened into four ordered categories
# so the ordered ladder (thresholds between configural and metric) is in force.
make_ordered_state <- function() {
  fx <- sieve_hs9_fixture()
  d <- fx$data
  for (it in fx$item_cols) {
    d[[it]] <- as.integer(cut(
      d[[it]],
      breaks = stats::quantile(d[[it]], c(0, .25, .5, .75, 1)),
      include.lowest = TRUE
    ))
  }
  meta <- sieve_build_meta(d, fx$item_cols, fx$theory_map, labels = fx$labels)
  meta$type <- "ordered"
  fx$data <- d
  s <- siever_test_study(fx, theory = FALSE, configure = function(s) {
    s$meta <- meta
    s$roles <- NULL
  })
  s
}

# The analysis frame the module builds: active items as numeric plus the
# grouping column, complete on both.
invariance_frame <- function(items_df, raw, meta, group) {
  cols <- intersect(meta$item[meta$status == "active"], names(items_df))
  d <- items_df[, cols, drop = FALSE]
  d[] <- lapply(d, function(v) suppressWarnings(as.numeric(v)))
  d[[group]] <- raw[[group]]
  d[stats::complete.cases(d[, cols, drop = FALSE]) & !is.na(d[[group]]), ,
    drop = FALSE
  ]
}

test_that("the invariance ladder is DMAR's, to machine precision", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map,
    labels = fx$labels
  )
  d <- invariance_frame(fx$data[, fx$item_cols], fx$data, meta, "school")
  ref <- DMAR::measurement_invariance(
    d,
    model = sieve_cfa_plan(meta)$model, group = "school",
    estimator = "ML", missing = "fiml", std.lv = TRUE
  )

  st <- make_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_null(r$error)
    # The module built the same analysis frame.
    expect_equal(r$data, d)
    # The continuous ladder has four rungs, in the conventional order.
    expect_identical(r$levels, c("configural", "metric", "scalar", "strict"))
    # Every number in the table is DMAR's, unrounded.
    expect_equal(r$table$chi_square, ref$chi_square, tolerance = 1e-12)
    expect_equal(r$table$df, ref$df, tolerance = 1e-12)
    expect_equal(r$table$cfi, ref$cfi, tolerance = 1e-12)
    expect_equal(r$table$rmsea, ref$rmsea, tolerance = 1e-12)
    expect_equal(r$table$delta_chi_square, ref$delta_chi_square,
      tolerance = 1e-12
    )
    expect_equal(r$table$delta_df, ref$delta_df, tolerance = 1e-12)
    expect_equal(r$table$p_value, ref$p_value, tolerance = 1e-12)
    expect_equal(r$table$delta_cfi, ref$delta_cfi, tolerance = 1e-12)
    expect_equal(r$table$delta_rmsea, ref$delta_rmsea, tolerance = 1e-12)
    # Stored at full precision: the display rounds, the stored value does not.
    expect_false(isTRUE(all.equal(
      r$table$chi_square,
      round(r$table$chi_square, 3)
    )))
    # The attributes that say what produced the numbers are surfaced.
    expect_identical(r$estimator, attr(ref, "estimator"))
    expect_identical(r$fit_indices, attr(ref, "fit_indices"))
    expect_identical(r$test, attr(ref, "test"))
    expect_match(r$test, "standard chi square difference test") # ML with FIML, the Auto default
    expect_identical(r$missing, "fiml")
    expect_false(r$ordered)
    expect_identical(r$group_labels, c("Pasteur", "Grant-White"))
    expect_no_error(output$inv_tbl)
    expect_no_error(output$inv_head)
    # The logged R is the call that actually ran.
    log <- paste(state$codelog, collapse = "\n")
    expect_match(log, "DMAR::measurement_invariance(", fixed = TRUE)
    expect_match(log, "estimator = \"ML\", missing = \"fiml\"", fixed = TRUE)
    expect_match(log, "std.lv = TRUE", fixed = TRUE)
    expect_match(log, "attr(mi, \"fits\")", fixed = TRUE)
  })
})

test_that("ordered items get the five-rung ladder and the panel does not assume four", {
  st <- make_ordered_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_null(r$error)
    # Thresholds sits between configural and metric (Wu & Estabrook, 2016).
    expect_identical(
      r$levels,
      c("configural", "thresholds", "metric", "scalar", "strict")
    )
    expect_equal(nrow(r$table), 5L)
    expect_true(r$ordered)
    expect_identical(r$estimator, "WLSMV")
    # The categorical estimator cannot do full information maximum likelihood.
    expect_identical(r$missing, "listwise")
    expect_match(r$table$Model[2], "Thresholds")
    expect_match(r$table$Model[3], "thresholds \\+ loadings")
    # DMAR's methodological messages reach the user rather than the console.
    expect_true(any(grepl("theta", r$notes)))
    expect_no_error(output$inv_tbl)
    expect_no_error(output$inv_head)
    # The thresholds rung is score-testable, and its constraints are thresholds.
    session$setInputs(inv_level = "thresholds")
    session$setInputs(inv_diag = 1)
    dg <- inv_diag()
    expect_null(dg$error)
    expect_true(nrow(dg$table) > 0)
    expect_true(all(dg$table$type == "threshold"))
  })
})

test_that("the diagnosis reads the fits DMAR returned rather than refitting", {
  st <- make_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_null(r$error)
    # The fits carried on the tab are exactly the ones DMAR handed back.
    expect_identical(r$fits, attr(r$mi, "fits"))
    expect_identical(names(r$fits), r$levels)
    expect_true(all(vapply(r$fits, inherits, logical(1), "lavaan")))
    # The diagnosis works on those fits.
    session$setInputs(inv_level = "scalar")
    session$setInputs(inv_diag = 1)
    dg <- inv_diag()
    expect_null(dg$error)
    expect_true(nrow(dg$table) > 0)
    expect_true(any(dg$table$type == "intercept"))
    # Largest score statistic first.
    expect_false(is.unsorted(rev(dg$table$X2)))
    # The same score test run directly on the stored fit gives the same values.
    ref <- suppressWarnings(
      lavaan::lavTestScore(attr(r$mi, "fits")$scalar, univariate = TRUE)
    )
    expect_equal(sort(dg$table$X2), sort(ref$uni$X2), tolerance = 1e-12)
    expect_no_error(output$inv_diag_tbl)
  })
})

test_that("the partial invariance workflow diagnoses, frees, and refits through DMAR", {
  st <- make_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_true(is.data.frame(r$table))
    expect_true(!is.null(r$fits$scalar))
    session$setInputs(inv_level = "scalar")
    session$setInputs(inv_diag = 1)
    dg <- inv_diag()
    expect_null(dg$error)
    session$setInputs(inv_diag_tbl_rows_selected = 1)
    session$setInputs(inv_refit = 1)
    p_res <- inv_partial()
    expect_null(p_res$error)
    expect_length(p_res$freed, 1L)
    expect_identical(p_res$level, "scalar")
    expect_identical(p_res$prev, "metric")
    expect_true(is.finite(p_res$cfi))
    expect_true(is.finite(p_res$dcfi))
    expect_true(inherits(p_res$fit, "lavaan"))
    # The refit is DMAR's, with the freed parameter named in group_partial.
    ref <- DMAR::measurement_invariance(
      r$data,
      model = sieve_cfa_plan(state$meta)$model, group = "school",
      estimator = "ML", missing = "fiml", std.lv = TRUE,
      group_partial = p_res$freed,
      levels = c("configural", "metric", "scalar")
    )
    expect_equal(p_res$cfi, ref$cfi[ref$level == "scalar"], tolerance = 1e-12)
    expect_equal(p_res$rmsea, ref$rmsea[ref$level == "scalar"],
      tolerance = 1e-12
    )
    # The delta is against the full metric rung, the conventional comparison.
    expect_equal(p_res$dcfi,
      p_res$cfi - r$table$cfi[r$table$level == "metric"],
      tolerance = 1e-12
    )
    expect_no_error(output$inv_partial_out)
    log <- paste(state$codelog, collapse = "\n")
    expect_match(log, "group_partial = c(", fixed = TRUE)
  })
})

test_that("dMACS matches DMAR and leads with the largest violation", {
  st <- make_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    session$setInputs(inv_level = "scalar")
    session$setInputs(inv_diag = 1)
    session$setInputs(inv_diag_tbl_rows_selected = 1)
    session$setInputs(inv_refit = 1)
    p_res <- inv_partial()
    expect_null(p_res$error)

    session$setInputs(
      dmacs_source = "partial", dmacs_ref = "Pasteur",
      dmacs_foc = "Grant-White"
    )
    session$setInputs(dmacs_go = 1)
    dm <- dmacs_res()
    expect_null(dm$error)
    expect_true(dm$partial)
    expect_identical(dm$reference, "Pasteur")
    expect_identical(dm$focal, "Grant-White")
    # Every item of the model gets a row, and the values are DMAR's.
    ref <- DMAR::dmacs(p_res$fit, reference = "Pasteur", focal = "Grant-White")
    expect_equal(nrow(dm$table), nrow(ref))
    expect_equal(dm$table$dmacs, ref$dmacs[match(dm$table$item, ref$item)],
      tolerance = 1e-12
    )
    expect_equal(dm$table$lambda_reference,
      ref$factor_loading_reference[match(dm$table$item, ref$item)],
      tolerance = 1e-12
    )
    expect_equal(dm$table$nu_focal,
      ref$intercept_focal[match(dm$table$item, ref$item)],
      tolerance = 1e-12
    )
    # Sorted by dMACS, largest first.
    expect_false(is.unsorted(rev(dm$table$dmacs)))
    # The freed intercept is the item that can differ, so it leads; the
    # constrained items are zero to numerical precision.
    freed_item <- strsplit(trimws(p_res$freed[1]), " ", fixed = TRUE)[[1]][1]
    expect_identical(dm$table$item[1], freed_item)
    expect_true(dm$table$dmacs[1] > 1e-6)
    expect_true(all(dm$table$dmacs[-1] < 1e-6))
    # No verbal size label anywhere in the table.
    expect_false(any(grepl("small|medium|large", unlist(dm$table),
      ignore.case = TRUE
    )))
    expect_no_error(output$dmacs_tbl)
    expect_no_error(output$dmacs_note)
    log <- paste(state$codelog, collapse = "\n")
    expect_match(log, "DMAR::dmacs(", fixed = TRUE)
  })
})

test_that("dMACS is refused from a configural fit, with the reason", {
  st <- make_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    # A configural selection is refused rather than computed.
    session$setInputs(
      dmacs_source = "rung", inv_level = "configural",
      dmacs_ref = "Pasteur", dmacs_foc = "Grant-White"
    )
    session$setInputs(dmacs_go = 1)
    dm <- dmacs_res()
    expect_false(is.null(dm$error))
    expect_null(dm$table)
    expect_match(dm$error, "not interpretable")
    expect_match(dm$error, "common metric")
    expect_match(dm$error, "latent scale separately")
    # The note surfaces the refusal, and the table stays empty: the render
    # stops on its own req(), which is Shiny's silent error, not a failure
    # of any other kind.
    expect_no_error(output$dmacs_note)
    expect_error(output$dmacs_tbl, class = "shiny.silent.error")
    # The METRIC rung is refused too, and this is the case that matters:
    # there the loadings are equal but every intercept is free and the group
    # latent means are fixed at zero, so a real latent mean difference is
    # absorbed into the intercepts and would be reported as noninvariance
    # that does not exist. See the invariant-by-construction test below.
    session$setInputs(inv_level = "metric")
    session$setInputs(dmacs_go = 2)
    dm2 <- dmacs_res()
    expect_false(is.null(dm2$error))
    expect_null(dm2$table)
    expect_match(dm2$error, "latent mean")
    # A rung that constrains the intercepts is computed.
    session$setInputs(inv_level = "scalar")
    session$setInputs(dmacs_go = 3)
    dm3 <- dmacs_res()
    expect_null(dm3$error)
    expect_false(dm3$partial)
    expect_equal(nrow(dm3$table), 9L)
    expect_true(all(is.finite(dm3$table$dmacs)))
    expect_true(all(dm3$table$dmacs >= 0))
  })
})

test_that("dMACS does not manufacture noninvariance from a latent mean difference", {
  # The test for the rung gate. These data are EXACTLY invariant by
  # construction: identical loadings and intercepts in both groups, differing
  # only in the latent mean. A correct index reports zero. Read from the metric
  # rung it does not, because there the latent means are fixed at zero and the
  # mean difference is absorbed into the free intercepts, which is precisely
  # why that rung is refused.
  skip_if_not_installed("lavaan")
  set.seed(1)
  n <- 400
  gen <- function(n, alpha) {
    eta <- stats::rnorm(n, alpha, 1)
    lam <- c(1, 0.9, 0.8, 1.1)
    nu <- c(2, 2.5, 3, 1.5)
    X <- vapply(seq_along(lam), function(j) {
      nu[j] + lam[j] * eta + stats::rnorm(n, 0, 0.7)
    }, numeric(n))
    colnames(X) <- paste0("x", seq_along(lam))
    as.data.frame(X)
  }
  d <- rbind(cbind(gen(n, 0), g = "A"), cbind(gen(n, 0.8), g = "B"))
  model <- "f =~ x1 + x2 + x3 + x4"

  fit_metric <- lavaan::cfa(model,
    data = d, group = "g", std.lv = TRUE,
    group.equal = "loadings"
  )
  fit_scalar <- lavaan::cfa(model,
    data = d, group = "g", std.lv = TRUE,
    group.equal = c("loadings", "intercepts")
  )
  dm_metric <- DMAR::dmacs(fit_metric, reference = "A", focal = "B")
  dm_scalar <- DMAR::dmacs(fit_scalar, reference = "A", focal = "B")

  # The metric rung invents a large effect where there is no noninvariance.
  expect_gt(max(dm_metric$dmacs), 0.3)
  # The scalar rung, which the app allows, correctly reports zero.
  expect_lt(max(dm_scalar$dmacs), 1e-6)
})

test_that("dMACS is refused for a categorical fit, whose value would be an identification artifact", {
  # The test for the categorical refusal. With ordered items the
  # ladder constrains the thresholds, which are what locate an item on the
  # latent scale, while the intercept of the underlying continuous response is
  # fixed at zero in the reference group and freed in the focal group purely to
  # identify the model. DMAR::dmacs() reads lambda and nu and never inspects
  # the thresholds, so on a categorical fit it reports that identification rule
  # as though it were noninvariance.
  st <- make_ordered_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_null(r$error)
    expect_true(r$ordered)

    # The artifact, measured on the fit the app itself produced. Every
    # threshold and every loading is invariant to machine precision, so the
    # size of the noninvariance is zero for every item, and the index still
    # returns a value far from zero.
    f <- r$fits[["metric"]]
    pt <- lavaan::parTable(f)
    th <- pt[pt$op == "|", ]
    expect_gt(nrow(th), 0L)
    expect_lt(max(abs(th$est[th$group == 1L] - th$est[th$group == 2L])), 1e-10)
    est <- lavaan::lavInspect(f, "est")
    expect_lt(max(abs(est[[1L]]$lambda - est[[2L]]$lambda)), 1e-10)
    expect_true(all(est[[1L]]$nu == 0))
    expect_gt(max(abs(est[[2L]]$nu)), 0.5)
    artifact <- DMAR::dmacs(f, reference = "Pasteur", focal = "Grant-White")
    expect_gt(max(artifact$dmacs), 0.9)

    # None of it reaches the panel: every rung of the ordered ladder is
    # refused, and the message gives the reason rather than only the verdict.
    for (i in seq_along(r$levels)) {
      session$setInputs(
        dmacs_source = "rung", inv_level = r$levels[i],
        dmacs_ref = "Pasteur", dmacs_foc = "Grant-White"
      )
      session$setInputs(dmacs_go = i)
      dm <- dmacs_res()
      expect_null(dm$table)
      expect_match(dm$error, "ordered or binary")
      expect_match(dm$error, "thresholds")
      expect_match(dm$error, "expected item score function")
      expect_match(dm$error, "ordinal expected-score impact")
    }
    expect_no_error(output$dmacs_note)
    expect_error(output$dmacs_tbl, class = "shiny.silent.error")

    # A partial invariance refit of a categorical fit is refused too: freeing a
    # parameter does not change what the index is defined on.
    session$setInputs(inv_level = "scalar")
    session$setInputs(inv_diag = 1)
    dg <- inv_diag()
    expect_null(dg$error)
    lr <- which(dg$table$type == "loading")[1]
    expect_false(is.na(lr))
    session$setInputs(inv_diag_tbl_rows_selected = lr)
    session$setInputs(inv_refit = 1)
    p_res <- inv_partial()
    expect_null(p_res$error)
    session$setInputs(dmacs_source = "partial")
    session$setInputs(dmacs_go = 99)
    dm <- dmacs_res()
    expect_null(dm$table)
    expect_match(dm$error, "ordered or binary")

    # A refusal ran no R, so nothing about dMACS is in the reproducible log.
    expect_false(any(grepl("DMAR::dmacs(", state$codelog, fixed = TRUE)))
  })
})

test_that("the categorical test does not fire on a continuous multiple group fit", {
  # lavInspect(fit, "th") returns one element per group, so a continuous
  # multiple group fit gives a list of empty vectors rather than an empty list.
  # Reading length() off that list would call every multiple group fit
  # categorical and refuse dMACS everywhere.
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map,
    labels = fx$labels
  )
  f <- lavaan::cfa(sieve_cfa_plan(meta)$model,
    data = fx$data, group = "school",
    std.lv = TRUE
  )
  expect_length(lavaan::lavInspect(f, "th"), 2L)
  expect_false(sieve_fit_is_categorical(f))
  expect_false(sieve_fit_is_categorical(NULL))
})

test_that("with more than two groups dMACS requires a named reference and focal", {
  fx <- sieve_hs9_fixture()
  d <- fx$data
  d$cohort <- as.character(cut(
    d$age_years,
    breaks = stats::quantile(d$age_years, c(0, 1 / 3, 2 / 3, 1)),
    labels = c("younger", "middle", "older"), include.lowest = TRUE
  ))
  st <- sieve_new_state()
  st$raw <- d
  st$items <- d[, fx$item_cols, drop = FALSE]
  st$id_vars <- c(fx$id_vars, "cohort")
  st$meta <- sieve_build_meta(d, fx$item_cols, fx$theory_map,
    labels = fx$labels
  )
  st$confirmed <- TRUE
  st$palette <- "nd"
  st$thresholds <- sieve_default_thresholds()

  shiny::testServer(mod_finalize_server, args = list(state = st), {
    expect_true("cohort" %in% eligible_groups())
    session$setInputs(group = "cohort")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_null(r$error)
    expect_length(r$group_labels, 3L)
    # Unnamed groups: refused, and the message says what to do.
    session$setInputs(
      inv_level = "scalar", dmacs_source = "rung",
      dmacs_ref = "", dmacs_foc = ""
    )
    session$setInputs(dmacs_go = 1)
    dm <- dmacs_res()
    expect_false(is.null(dm$error))
    expect_match(dm$error, "exactly two groups")
    expect_match(dm$error, "3 groups")
    # Named: computed for that pair.
    session$setInputs(dmacs_ref = "younger", dmacs_foc = "older")
    session$setInputs(dmacs_go = 2)
    dm2 <- dmacs_res()
    expect_null(dm2$error)
    expect_identical(dm2$reference, "younger")
    expect_identical(dm2$focal, "older")
    expect_equal(dm2$n_groups, 3L)
    ref <- DMAR::dmacs(r$fits$scalar, reference = "younger", focal = "older")
    expect_equal(dm2$table$dmacs, ref$dmacs[match(dm2$table$item, ref$item)],
      tolerance = 1e-12
    )
    expect_false(is.unsorted(rev(dm2$table$dmacs)))
  })
})

test_that("the ladder gate names what to fix when there is no model to test", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map,
    labels = fx$labels
  )
  meta$status <- "parked"
  meta$status[c(1, 4, 7)] <- "active" # one item in each of three constructs
  st <- make_state()
  st$meta <- meta
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_false(is.null(r$error))
    expect_match(r$error, "two or more active items")
    expect_match(r$error, "Items tab")
    # A setup problem does not get the group size advice.
    expect_false(grepl("each group here has", r$error))
    expect_no_error(output$inv_msg)
  })
})

test_that("alignment returns one row per group with its convergence diagnostics", {
  st <- make_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(
      group = "school", align_construct = "Visual",
      align_ident = "fixed", align_starts = 10,
      align_seed = 113, align_std = TRUE
    )
    session$setInputs(align_go = 1)
    a <- align_res()
    expect_null(a$error)
    # One row per group, with finite factor means and variances.
    expect_equal(nrow(a$table), 2L)
    expect_identical(a$table$group, c("Pasteur", "Grant-White"))
    expect_true(all(is.finite(a$table$factor_mean)))
    expect_true(all(is.finite(a$table$factor_variance)))
    expect_true(all(a$table$factor_variance > 0))
    expect_true(all(a$table$n > 0))
    # Fixed identification: the first group is the reference (mean 0, var 1).
    expect_equal(a$table$factor_mean[1], 0, tolerance = 1e-10)
    expect_equal(a$table$factor_variance[1], 1, tolerance = 1e-10)
    # The convergence attributes are surfaced, not swallowed.
    expect_true(is.logical(a$converged))
    expect_true(a$converged)
    expect_true(is.numeric(a$n_optima) && a$n_optima >= 1L)
    expect_identical(a$n_starts, 10L)
    expect_true(is.finite(a$simplicity))
    expect_length(a$starts, 10L)
    expect_true(all(c("loadings", "intercepts") %in% names(a$R2_total)))
    # One R-squared row per item, sorted by total simplicity contribution.
    expect_equal(nrow(a$r2), 3L)
    expect_true(all(c("R2_loadings", "R2_intercepts", "loss_total") %in%
      names(a$r2)))
    expect_false(is.unsorted(rev(a$r2$loss_total)))
    expect_no_error(output$align_tbl)
    expect_no_error(output$align_diag)
    expect_no_error(output$align_note)
    expect_no_error(output$align_r2_tbl)
    log <- paste(state$codelog, collapse = "\n")
    expect_match(log, "DMAR::measurement_alignment(", fixed = TRUE)
    expect_match(log, "seed = 113", fixed = TRUE)
  })
})

test_that("alignment matches a direct DMAR call and refuses a one-factor-short construct", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map,
    labels = fx$labels
  )
  its <- meta$item[meta$construct == "Visual"]
  d <- fx$data[, c(fx$item_cols, "school")]
  d[fx$item_cols] <- lapply(d[fx$item_cols], as.numeric)
  d <- d[stats::complete.cases(d[, c(its, "school")]), c(its, "school")]
  d[its] <- lapply(d[its], function(v) as.numeric(scale(v)))
  ref <- DMAR::measurement_alignment(d,
    items = its, group = "school",
    alignment = "fixed", estimator = "ML",
    n_starts = 10, seed = 113
  )

  st <- make_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(
      group = "school", align_construct = "Visual",
      align_ident = "fixed", align_starts = 10,
      align_seed = 113, align_std = TRUE
    )
    session$setInputs(align_go = 1)
    a <- align_res()
    expect_null(a$error)
    expect_equal(a$table$factor_mean, ref$factor_mean, tolerance = 1e-10)
    expect_equal(a$table$factor_variance, ref$factor_variance,
      tolerance = 1e-10
    )
    expect_equal(a$simplicity, attr(ref, "simplicity_function"),
      tolerance = 1e-10
    )
    expect_equal(a$r2$R2_loadings[match(its, a$r2$item)],
      unname(attr(ref, "R2_loadings")[its]),
      tolerance = 1e-10
    )

    # Alignment is a single factor method: a construct with fewer than three
    # active items is refused with an actionable message.
    m <- state$meta
    m$status[m$item %in% its[1]] <- "parked"
    state$meta <- m
    session$setInputs(align_construct = "Visual")
    session$setInputs(align_go = 2)
    a2 <- align_res()
    expect_false(is.null(a2$error))
    expect_match(a2$error, "three or more")
  })
})

test_that("a result computed on one dataset is not shown after another is loaded, or after the data or ledger change", {
  st <- make_state()
  stale_text <- function(x) {
    paste(as.character(unlist(x)), collapse = " ")
  }
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    # Every result the tab keeps, computed on the bundled data.
    session$setInputs(holdout = 50, seed = 7)
    session$setInputs(split = 1)
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    session$setInputs(inv_level = "scalar")
    session$setInputs(inv_diag = 1)
    session$setInputs(inv_diag_tbl_rows_selected = 1)
    session$setInputs(inv_refit = 1)
    session$setInputs(dmacs_source = "rung", dmacs_ref = "Pasteur", dmacs_foc = "Grant-White")
    session$setInputs(dmacs_go = 1)
    session$setInputs(
      group = "school", align_construct = "Visual", align_std = FALSE,
      align_starts = 2, align_seed = 3, align_ident = "fixed"
    )
    session$setInputs(align_go = 1)
    results <- list(
      split = cv_shown, ladder = inv_shown, diagnosis = inv_diag,
      partial = inv_partial, dmacs = dmacs_res, alignment = align_res
    )
    for (nm in names(results)) {
      r <- results[[nm]]()
      expect_null(r$error, info = nm)
      expect_false(isTRUE(r$stale), info = nm)
      expect_type(r$revision, "list")
    }
    first <- inv_shown()$table
    tables <- c("cv_tbl", "inv_tbl", "inv_diag_tbl", "dmacs_tbl", "align_tbl")
    for (id in tables) {
      expect_no_error(output[[id]], message = id)
    }

    # Another dataset is loaded (the loader gives it a new identity; the
    # values being the same changes nothing about the rule). A write to the
    # state from here does not run the outputs' observers by itself, as an
    # input would; the flush stands in for the one the app does.
    sieve_new_dataset(state)
    session$flushReact()
    for (nm in names(results)) {
      r <- results[[nm]]()
      expect_true(isTRUE(r$stale), info = nm)
      expect_match(r$error, "computed from different inputs", fixed = TRUE, info = nm)
      expect_match(r$error, "a different dataset has been loaded since", fixed = TRUE, info = nm)
      expect_null(r$table, info = nm)
    }
    expect_match(inv_shown()$error, "Press Run invariance", fixed = TRUE)
    expect_match(cv_shown()$error, "Press Create split", fixed = TRUE)
    expect_match(inv_partial()$error, "Press Refit freeing selected", fixed = TRUE)
    expect_match(align_res()$error, "Press Run alignment", fixed = TRUE)
    # The tables are gone and the panels say why.
    for (id in tables) {
      expect_error(output[[id]], class = "shiny.silent.error")
    }
    for (id in c("cv_msg", "inv_msg", "inv_diag_note", "inv_partial_out", "dmacs_note", "align_note")) {
      expect_match(stale_text(output[[id]]), "computed from different inputs", fixed = TRUE, info = id)
    }
    # A diagnosis, a refit, or a dMACS asked for on the stale ladder is
    # refused with the same reason, not computed on the old fits.
    session$setInputs(inv_diag = 2)
    expect_match(inv_diag()$error, "a different dataset has been loaded since", fixed = TRUE)
    session$setInputs(dmacs_go = 2)
    expect_match(dmacs_res()$error, "a different dataset has been loaded since", fixed = TRUE)
    # Run again on the current data: the result is current, and the same.
    sieve_confirm_types(state)
    session$setInputs(run_inv = 2)
    r <- inv_shown()
    expect_null(r$error)
    expect_equal(r$table$chi_square, first$chi_square, tolerance = 1e-10)
    expect_no_error(output$inv_tbl)

    # The ledger changes (an item is excluded): the ladder no longer
    # describes the model on screen.
    sieve_remove_cases(state, 1L, "historical ladder coverage")
    session$flushReact()
    r <- inv_shown()
    expect_true(isTRUE(r$stale))
    expect_match(r$error, "respondents|item data")
    expect_error(output$inv_tbl, class = "shiny.silent.error")
    session$setInputs(run_inv = 3)
    expect_null(inv_shown()$error)
    expect_identical(inv_shown()$table$df[1], first$df[1])

    # The item data change (an item is reverse scored).
    d <- shiny::isolate(state$items)
    d$t1_visual_perception <- 50 - d$t1_visual_perception
    state$items <- d
    session$flushReact()
    r <- inv_shown()
    expect_true(isTRUE(r$stale))
    expect_match(r$error, "the item data have changed since", fixed = TRUE)
    expect_match(stale_text(output$inv_msg), "the item data have changed since", fixed = TRUE)
  })
})

test_that("the grouping selector reads the roles and the one level rule, and names the ceiling", {
  fx <- sieve_hs9_fixture()
  d <- fx$data
  n <- nrow(d)
  d$pcode <- withr::with_seed(2L, sample(100000:999999, n)) # a six digit code per case
  d$tx <- rep_len(c(0L, 1L), n) # a two level treatment code
  d$region <- rep_len(paste0("r", 1:7), n) # seven regions
  d$gpa <- withr::with_seed(3L, stats::rnorm(n, 3, .4))
  st <- make_state()
  st$raw <- d
  st$id_vars <- setdiff(names(d), fx$item_cols)
  roles <- sieve_roles_table(d, d[, fx$item_cols, drop = FALSE], NULL)
  set_role <- function(roles, variable, role) {
    roles$role[roles$variable == variable] <- role
    roles
  }
  roles <- set_role(roles, "pcode", "identifier")
  roles <- set_role(roles, "tx", "group")
  roles <- set_role(roles, "region", "group")
  roles <- set_role(roles, "gpa", "criterion")
  roles <- set_role(roles, "grade", "auxiliary") # two levels, but not a grouping
  st$roles <- roles
  expect_identical(sieve_role_vars(st, "group"), c("sex", "school", "tx", "region"))
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    # Offered: the columns with the group role and a usable number of
    # levels. Not offered: the seven level region (named as over the
    # ceiling), the identifiers, the criterion, and a two level column that
    # is not a grouping.
    expect_identical(eligible_groups(), c("sex", "school", "tx"))
    expect_identical(sieve_groups_over_ceiling(state), "region")
    expect_false("grade" %in% eligible_groups())
    expect_false(any(c("id", "pcode", "gpa", "age_years") %in% eligible_groups()))
    expect_match(paste(as.character(unlist(output$align_note)), collapse = " "),
      "excluded by the ceiling of 6 distinct values on this tab: region",
      fixed = TRUE
    )
    # The rule is read from one place.
    expect_identical(unname(sieve_group_levels()), c(2L, 6L))
    expect_identical(sieve_group_levels_text(), "2 to 6 distinct values")
    expect_true(sieve_group_usable_levels(d$tx))
    expect_false(sieve_group_usable_levels(d$region))
    expect_false(sieve_group_usable_levels(rep(1, n)))
    # A role change is followed: the treatment code leaves the selector
    # when it is no longer a grouping, and grade enters when it becomes one.
    state$roles <- set_role(set_role(roles, "tx", "auxiliary"), "grade", "group")
    sieve_confirm_types(state)
    session$flushReact()
    expect_identical(eligible_groups(), c("sex", "grade", "school"))
    # The ladder runs on the numeric two level grouping as on any other.
    state$roles <- roles
    sieve_confirm_types(state)
    session$flushReact()
    session$setInputs(group = "tx")
    session$setInputs(run_inv = 1)
    expect_null(inv_result()$error)
    expect_length(inv_result()$group_labels, 2L)
  })
})

test_that("the ladder names the rung each row is compared with, the family, and the test; a failure names the item and the action", {
  st <- make_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_null(r$error)
    # Each row names the rung it is compared with: the one before it.
    expect_identical(r$table$compared_with, c("", r$table$Model[-nrow(r$table)]))
    expect_identical(names(r$table)[match("compared_with", names(r$table)) + 1L], "delta_chi_square")
    # The panel states the family, the comparison rule, the test, and that
    # the reference values are conventions.
    msg <- gsub("&amp;", "&", paste(as.character(unlist(output$inv_msg)), collapse = " "), fixed = TRUE)
    expect_match(msg, sprintf("CFI and RMSEA are in the %s family", r$family), fixed = TRUE)
    expect_match(msg, "compared with the rung before it, named in the Compared with column, by", fixed = TRUE)
    expect_match(msg, r$test, fixed = TRUE)
    expect_match(msg, "conventions (Cheung & Rensvold, 2002; Chen, 2007), not verdicts", fixed = TRUE)
    expect_no_error(output$inv_tbl)
  })

  # An ordered item with a category no case in one group uses: the ladder
  # cannot be fit, and the message names the item, the group, and the one
  # action that fixes it.
  n <- 300L
  d <- withr::with_seed(7L, {
    eta <- stats::rnorm(n)
    out <- as.data.frame(lapply(1:4, function(j) {
      as.integer(cut(.7 * eta + sqrt(.51) * stats::rnorm(n), c(-Inf, -.8, 0, .8, Inf)))
    }))
    names(out) <- paste0("y", 1:4)
    out
  })
  d$g <- rep(c("one", "two"), each = n / 2L)
  d$y1[d$g == "two" & d$y1 == 4L] <- 3L
  s <- sieve_new_state()
  s$dataset_name <- "sparse"
  s$raw <- d
  s$items <- d[, paste0("y", 1:4)]
  s$id_vars <- "g"
  s$meta <- data.frame(
    item = paste0("y", 1:4), label = paste0("y", 1:4), construct = "F",
    facet = "", type = "ordered", status = "active", reverse = FALSE,
    stringsAsFactors = FALSE
  )
  s$confirmed <- TRUE
  s$palette <- "nd"
  s$thresholds <- sieve_default_thresholds()
  shiny::testServer(mod_finalize_server, args = list(state = s), {
    session$setInputs(group = "g")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_match(r$error, "could not be fit", fixed = TRUE)
    expect_match(r$error, "Item 'y1' has a response category that no case in group 2 uses", fixed = TRUE)
    expect_match(r$error, "Collapse that category with a neighboring one", fixed = TRUE)
    expect_false(grepl("usual fixes are a grouping variable with", r$error, fixed = TRUE))
    expect_match(paste(as.character(unlist(output$inv_msg)), collapse = " "), "Item 'y1'", fixed = TRUE)
    expect_error(output$inv_tbl, class = "shiny.silent.error")
  })
})

# The reliability API (denominator argument, ci_method menu, categorical
# omega), the bifactor indices, the CMV builders, simple structure, and the
# ECVI interval all come from DMAR. These tests pin the shapes SIEVER reads
# so a DMAR API change fails loudly here rather than silently blanking a
# table.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

make_state <- function(confirmed = TRUE) {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = confirmed, labels = TRUE,
    theory = TRUE, name = fx$name)
  s
}

test_that("optimizer omega estimate pins the observed denominator", {
  fx <- sieve_hs9_fixture()
  X <- fx$data[, fx$theory_map$Visual, drop = FALSE]
  X <- X[stats::complete.cases(X), , drop = FALSE]
  om <- sieve_omega_estimate(X)$value
  expect_true(is.finite(om) && om > 0 && om < 1)
  # The value is DMAR's robust omega (the observed denominator), the
  # coefficient the Optimizer's omega columns name.
  ref <- DMAR::reliability_omega(
    data = X, ci_method = "none",
    denominator = "observed"
  )
  expect_equal(om, ref$value[ref$term == "estimate"], tolerance = 1e-10)
})

test_that("the reliability table carries the coefficient label and interval", {
  st <- make_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "model_implied", ci_method = "mlr",
      missing = "listwise", conf_level = 0.95,
      show_alpha = TRUE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    df <- reliab()
    expect_true(all(c(
      "construct", "coefficient", "omega",
      "ci_lower", "ci_upper", "H", "alpha"
    ) %in% names(df)))
    # (Labels name their target, in DMAR's denominator wording.)
    expect_true(all(df$coefficient == "Omega (model-implied denominator)"))
    expect_true(all(is.finite(df$omega)))
    # The default configuration reports an interval with every omega.
    expect_true(all(is.finite(df$ci_lower) & is.finite(df$ci_upper)))
    expect_true(all(df$ci_lower < df$omega & df$omega < df$ci_upper))
  })
})

test_that("constructs of ordered items report categorical omega", {
  st <- make_state()
  # Discretize the items into five ordered categories and mark them ordered,
  # the situation categorical omega (Green & Yang, 2009) is defined for.
  shiny::isolate({
    st$items <- as.data.frame(lapply(st$items, function(v) {
      as.integer(cut(as.numeric(v), breaks = 5))
    }))
    meta <- st$meta
    meta$type <- "ordered"
    st$meta <- meta
  })
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      denominator = "model_implied", ci_method = "mlr",
      missing = "listwise", conf_level = 0.95,
      show_alpha = FALSE, score_method = "Bartlett"
    )
    session$setInputs(run_go = 1)
    df <- reliab()
    # (Labels name their target, in DMAR's denominator wording.)
    expect_true(all(df$coefficient == categorical_omega_label("model_implied")))
    expect_true(all(is.finite(df$omega)))
    # No closed-form interval exists for categorical omega; with a closed-form
    # method selected the interval is absent, never silently wrong.
    expect_true(all(is.na(df$ci_lower)))
  })
})

test_that("bifactor indices come from DMAR with its column schema", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  out <- sieve_fit_bifactor(fx$data[, fx$item_cols, drop = FALSE], meta)
  skip_if(is.null(out$indices), "bifactor model did not converge")
  expect_identical(
    names(out$indices),
    c("factor", "ECV", "omega", "omega_H", "omega_HS", "PUC", "H")
  )
  # The general factor is the first row and carries omega_H and PUC; the group
  # factors carry omega_HS instead.
  expect_identical(out$indices$factor[1], "general")
  expect_true(is.finite(out$indices$omega_H[1]))
  expect_true(is.finite(out$indices$PUC[1]))
  expect_true(all(is.na(out$indices$omega_H[-1])))
  expect_true(all(is.finite(out$indices$omega_HS[-1])))
  expect_true(is.logical(out$improper) && !is.na(out$improper))
  # The indices must agree with a direct DMAR call on the same fit.
  ref <- as.data.frame(suppressWarnings(
    DMAR::bifactor_indices(out$fit, general = "general")
  ))
  expect_equal(out$indices$ECV, ref$ECV, tolerance = 1e-12)
  expect_equal(out$indices$H, ref$H, tolerance = 1e-12)
})

test_that("simple structure metrics agree with DMAR::simple_structure", {
  skip_if_not_installed("psych")
  fx <- sieve_hs9_fixture()
  X <- fx$data[, fx$item_cols, drop = FALSE]
  f <- suppressWarnings(suppressMessages(
    psych::fa(X, nfactors = 3, fm = "ml", rotate = "oblimin")
  ))
  P <- unclass(f$loadings)
  ss <- DMAR::simple_structure(P, salient = 0.30, hyperplane = 0.10)
  expect_true(all(c(
    "mean_complexity", "hyperplane_proportion",
    "n_pure", "n_complex"
  ) %in% ss$term))
  cmplx <- attr(ss, "complexity")
  expect_length(cmplx, nrow(P))
  # Hofmann complexity re-derived from the loadings pins the formula.
  expect_equal(unname(cmplx),
    unname(apply(P, 1L, function(l) sum(l^2)^2 / sum(l^4))),
    tolerance = 1e-12
  )
})

test_that("the CMV tab uses DMAR's Harman screen and marker adjustment", {
  st <- make_state()
  shiny::testServer(mod_validity_server, args = list(state = st), {
    df <- harman_df()
    expect_true(is.data.frame(df))
    prop <- suppressWarnings(as.numeric(df$Value[2]))
    expect_true(is.finite(prop) && prop > 0 && prop < 1)
    # The displayed share must match the DMAR call on the same items.
    ref <- DMAR::common_method_single_factor(data = shiny::isolate(active_x()))
    expect_equal(prop, round(ref$value[ref$term == "variance_explained"], 3),
      tolerance = 1e-6
    )
    # Marker adjustment through DMAR::common_method_marker.
    session$setInputs(marker = "age_years", score_method = "mean")
    res <- marker_df()
    expect_true(is.finite(res$rM) || is.null(res$tbl))
    if (!is.null(res$tbl)) {
      expect_true(all(c("r.unadjusted", "r.adjusted") %in% names(res$tbl)))
      # The Lindell and Whitney identity r_adj = (r - rM) / (1 - rM).
      expect_equal(res$tbl$r.adjusted,
        (res$tbl$r.unadjusted - res$rM) / (1 - res$rM),
        tolerance = 1e-10
      )
    }
  })
})

test_that("the CV split reports the ECVI with its interval and an optional seed", {
  st <- make_state()
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    # No seed supplied: the split must still run, drawing from the session RNG.
    session$setInputs(holdout = 50, seed = NA)
    session$setInputs(split = 1)
    r <- cv_result()
    expect_true(is.data.frame(r$table))
    expect_true(all(c("ECVI", "ECVI.lo", "ECVI.hi") %in% names(r$table)))
    ok <- is.finite(r$table$ECVI) & is.finite(r$table$ECVI.lo)
    # Where the interval exists it brackets sensibly (lower <= upper).
    expect_true(all(r$table$ECVI.lo[ok] <= r$table$ECVI.hi[ok]))
  })
})

test_that("a supplied bootstrap seed reproduces the HTMT upper bound and restores RNG state", {
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  items <- fx$data[, fx$item_cols, drop = FALSE]
  grp <- split(meta$item, meta$construct)
  set.seed(1)
  before <- .Random.seed
  h1 <- DMAR::htmt(items, blocks = grp, B = 200, conf_level = 0.95, seed = 113)
  expect_identical(.Random.seed, before) # caller's RNG state restored
  h2 <- DMAR::htmt(items, blocks = grp, B = 200, conf_level = 0.95, seed = 113)
  expect_equal(h1$ci_upper, h2$ci_upper, tolerance = 1e-12)
})

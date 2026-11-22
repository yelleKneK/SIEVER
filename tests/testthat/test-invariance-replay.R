# Recorded diagnostics must rebuild their scientific parent in a fresh R
# environment. A previous fit under a common name is deliberately poisoned.
invariance_replay_data <- function(ordered = FALSE) {
  d <- withr::with_seed(284, {
    f <- stats::rnorm(480)
    x <- as.data.frame(lapply(seq_len(6), function(j) .7 * f + sqrt(.51) * stats::rnorm(480)))
    names(x) <- paste0("x", seq_len(6))
    x$group <- rep(c('reference "quoted"', "focal"), each = 240)
    x$x6[241:480] <- x$x6[241:480] + .45
    x
  })
  items <- paste0("x", seq_len(6))
  if (ordered) d[items] <- lapply(d[items], function(x) as.integer(cut(x, c(-Inf, -.7, 0, .7, Inf))))
  meta <- sieve_build_meta(d, items, list(Ability = items))
  meta$type <- if (ordered) "ordered" else "continuous"
  list(data = d, items = items, meta = meta)
}

invariance_replay_env <- function(fixture) {
  e <- new.env(parent = globalenv())
  e$inv_data <- fixture$data
  e$d <- fixture$data[1L, , drop = FALSE] # Never use the raw-data shortcut.
  e$meta <- fixture$meta
  e$fits_mi <- list(scalar = "unrelated previous fit")
  e$fits_mi_partial <- list(scalar = "unrelated previous partial fit")
  e
}

test_that("identified ordered partial code rebuilds its eligible parent fits", {
  skip_if_not_installed("DMAR")
  fx <- invariance_replay_data(TRUE)
  # One blank group and one incomplete response are outside this sample.
  fx$data$group[1L] <- ""
  fx$data$x1[2L] <- NA_integer_
  eligible <- fx$data[stats::complete.cases(fx$data) & nzchar(fx$data$group), ]
  res <- sieve_fit_invariance(eligible, "group", fx$meta,
    group_partial = c("Ability =~ x6", "x6 ~1"),
    levels = c("configural", "thresholds", "metric", "scalar"), object = "mi_partial"
  )
  expect_null(res$error)
  env <- invariance_replay_env(fx)
  expect_no_error(suppressWarnings(eval(parse(text = res$code), env)))
  expect_identical(names(env$fits_mi_partial), names(attr(res$mi, "fits")))
  for (rung in names(env$fits_mi_partial)) {
    expect_equal(lavaan::coef(env$fits_mi_partial[[rung]]),
      lavaan::coef(attr(res$mi, "fits")[[rung]]),
      tolerance = 1e-10
    )
    expect_equal(sum(lavaan::lavInspect(env$fits_mi_partial[[rung]], "nobs")), nrow(eligible))
  }
})

test_that("invariance diagnostics and effect sizes replay their actual retained parents", {
  skip_if_not_installed("DMAR")
  fx <- invariance_replay_data()
  st <- sieve_new_state()
  st$raw <- fx$data
  st$items <- fx$data[fx$items]
  st$meta <- fx$meta
  st$id_vars <- "group"
  st$confirmed <- TRUE
  st$palette <- "nd"
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_log = function(state, note, code, ...) {
    captured[[note]] <- code
    invisible(NULL)
  })
  code_for <- function(pattern) {
    name <- grep(pattern, ls(captured), value = TRUE)
    expect_length(name, 1L)
    captured[[name]]
  }
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "group", inv_level = "scalar", inv_interpret_level = "scalar")
    session$setInputs(run_inv = 1)
    r <- inv_shown()
    expect_null(r$error)
    session$setInputs(inv_diag = 1)
    diagnosis <- inv_diag()
    expect_null(diagnosis$error)
    env <- invariance_replay_env(fx)
    suppressWarnings(eval(parse(text = code_for("^Partial invariance diagnosis")), env))
    expect_equal(env$sc$uni, lavaan::lavTestScore(r$fits$scalar, univariate = TRUE)$uni, tolerance = 1e-10)

    row <- which(diagnosis$table$type == "intercept" & diagnosis$table$lhs == "x6")[1L]
    expect_false(is.na(row))
    session$setInputs(inv_diag_tbl_rows_selected = row, inv_refit = 1)
    part <- inv_partial()
    expect_null(part$error)
    session$setInputs(dmacs_source = "partial", dmacs_ref = 'reference "quoted"', dmacs_foc = "focal", dmacs_go = 1)
    effect <- dmacs_res()
    expect_null(effect$error)
    env <- invariance_replay_env(fx)
    suppressWarnings(eval(parse(text = code_for("^dMACS effect size")), env))
    direct <- DMAR::dmacs(part$fit, reference = 'reference "quoted"', focal = "focal")
    expect_equal(env$dmacs_result$dmacs, direct$dmacs, tolerance = 1e-10)
    expect_equal(lavaan::coef(env$fit_dmacs), lavaan::coef(part$fit), tolerance = 1e-10)

    interpreted <- inv_interpretation()
    env <- invariance_replay_env(fx)
    suppressWarnings(eval(parse(text = code_for("^Interpret measurement")), env))
    expect_equal(env$invariance_interpretation$contrasts, interpreted$contrasts, tolerance = 1e-10)

    session$setInputs(
      inv_anchor_sets = "first: x1, x2\nsecond: x2, x3",
      inv_anchor_rationale = "Two researcher specified reference sets.", inv_anchor_discovery = TRUE,
      inv_anchor_run = 1
    )
    anchor <- anchor_shown()
    expect_null(anchor$error)
    env <- invariance_replay_env(fx)
    suppressWarnings(eval(parse(text = code_for("^Freeze anchor")), env))
    expect_equal(env$anchor_comparison$contrasts, anchor$contrasts, tolerance = 1e-10)

    session$setInputs(
      group = "group", align_construct = "Ability", align_starts = 2,
      align_seed = 93, align_std = FALSE, align_go = 1
    )
    aligned <- align_res()
    expect_null(aligned$error)
    # The alignment log also binds its own data frame rather than relying on
    # an undeclared object from an earlier script step.
    env <- invariance_replay_env(fx)
    suppressWarnings(eval(parse(text = code_for("^Alignment of")), env))
    expect_equal(as.numeric(env$al$factor_mean), aligned$table$factor_mean, tolerance = 1e-10)
  })
})

norm_fixture <- function() {
  d <- data.frame(a = seq_len(80) / 10, b = seq_len(80) / 10 + sin(seq_len(80)))
  m <- sieve_build_meta(d, names(d), list(Scale = names(d)))
  list(data = d, meta = m)
}

test_that("frozen references preserve shifts and exact scoring contracts", {
  x <- norm_fixture()
  ref <- sieve_norm_freeze(x$data, x$meta, name = "Calibration 1", population = "Synthetic demonstration only")
  old <- sieve_norm_apply(ref, x$data, x$meta)
  shifted <- sieve_norm_apply(ref, x$data + 2, x$meta)
  expect_equal(old$T, sieve_construct_norms(x$data, x$meta)$T$Scale)
  expect_equal(shifted$T - old$T, rep(20 / ref$table$sd, nrow(x$data)))
  expect_gt(mean(shifted$T), 50)
  expect_error(sieve_norm_apply(ref, x$data, x$meta, list(Scale = list(min_count = 2L))), "differ")
  m <- x$meta
  m$reverse[1] <- TRUE
  expect_error(sieve_norm_apply(ref, x$data, m), "differ")
  expect_error(sieve_norm_freeze(x$data, x$meta, name = "", population = "P"), "Name")
  one <- sieve_norm_apply(ref, x$data[1, ], x$meta)
  expect_equal(nrow(one), 1)
  withr::with_seed(51, before <- .Random.seed)
  r <- sieve_norm_transform_limits(ref, B = 199, seed = 42)
  expect_true(all(r$successful == 199))
  expect_true(all(r$lower <= r$upper))
  ref$design$unit <- "clustered"
  expect_error(sieve_norm_transform_limits(ref), "aligned cluster labels")
})

test_that("percentile limits match binomial ranks and ties keep discrete support", {
  x <- seq_len(200)
  p <- c(.05, .5, .95)
  r <- sieve_quantile_limits(x, p)
  lo <- qbinom(.025, 200, p)
  hi <- qbinom(.975, 200, p) + 1
  expect_equal(r$lower, x[lo])
  expect_equal(r$upper, x[hi])
  expect_equal(r$expected_upper_tail, 200 * (1 - p))
  tied <- rep(1:4, c(40, 20, 30, 10))
  t <- sieve_quantile_limits(tied, p)
  expect_match(t$method[1], "conservative with ties")
  expect_true(all(t$lower %in% c(-Inf, unique(tied))))
  expect_true(all(t$upper %in% c(Inf, unique(tied))))
  expect_identical(tied, rep(1:4, c(40, 20, 30, 10)))
  tail <- sieve_quantile_limits(1:10, .95)
  expect_identical(tail$upper, Inf)
})

test_that("exported scoring reproduces minimum response and weighted policies", {
  x <- norm_fixture()
  x$data$a[1:3] <- NA
  pol <- list(Scale = list(min_count = 2L, min_prop = 0, weights = c(a = 1, b = 2)))
  e <- new.env(parent = baseenv())
  e$dat <- x$data
  code <- sieve_scoring_code(x$meta, "processed", pol)
  eval(parse(text = code$code), e)
  expected <- sieve_score_policy(x$data, c("a", "b"), 2, 0, c(a = 1, b = 2))$scores
  expect_equal(e$dat[[code$name_map[["Scale"]]]], expected)
  expect_true(all(is.na(expected[1:3])))
})

test_that("invariance eligible pools retain partial cases only for continuous FIML", {
  d <- data.frame(a = c(1, 2, NA, NA, 5), b = c(2, NA, NA, 4, 6))
  g <- c("A", "A", "A", NA, "B")
  f <- sieve_invariance_frame(d, g, names(d), missing = "fiml")
  expect_identical(f$rows, c(1L, 2L, 5L))
  expect_equal(f$row_flow$partial_rows_retained, 1)
  expect_equal(f$row_flow$all_items_missing, 1)
  expect_equal(f$row_flow$missing_group, 1)
  c <- sieve_invariance_frame(d, g, names(d), missing = "complete")
  expect_identical(c$rows, c(1L, 5L))
  o <- sieve_invariance_frame(d, g, names(d), categorical = TRUE, missing = "fiml")
  expect_equal(o$rows, c$rows)
  expect_identical(o$missing, "listwise")
})

test_that("mean comparisons read actual constraints rather than ladder names", {
  withr::with_seed(19, {
    eta <- rnorm(800) + rep(c(0, .8), each = 400)
    d <- as.data.frame(sapply(c(.8, .9, 1, 1.1), function(l) l * eta + rnorm(800, .0, .7)))
    names(d) <- paste0("x", 1:4)
    d$g <- rep(c("A", "B"), each = 400)
  })
  model <- "F =~ x1 + x2 + x3 + x4"
  metric <- lavaan::cfa(model, d, group = "g", std.lv = TRUE, group.equal = "loadings", meanstructure = TRUE)
  a <- sieve_invariance_interpret(metric)
  expect_false(any(a$contrasts$licensed))
  scalar <- lavaan::cfa(model, d, group = "g", std.lv = TRUE, group.equal = c("loadings", "intercepts"), meanstructure = TRUE)
  b <- sieve_invariance_interpret(scalar)
  expect_true(all(b$contrasts$licensed))
  pe <- lavaan::parameterEstimates(scalar)
  direct <- pe[pe$lhs == "F" & pe$op == "~1" & pe$group == 2, ]
  expect_equal(b$contrasts$estimate, direct$est)
  expect_equal(b$contrasts$se, direct$se)
  expect_equal(b$contrasts$lower, direct$ci.lower)
  expect_error(
    sieve_anchor_sensitivity(d,
      "g",
      sieve_build_meta(
        d,
        names(d)[1:4],
        list(F = names(d)[1:4])
      ),
      list(
        A = "x1",
        B = c(
          "x2",
          "x3"
        )
      ),
      rationale = "Test"
    ),
    "at least two"
  )
})

test_that("ordinal expected-score impact has correct zero, sign, and metric invariance", {
  one <- list(i = list(loading = 1, intercept = 0, thresholds = c(-1, 0, 1), residual_sd = 1, scores = 1:4))
  theta <- seq(-4, 4, length.out = 501)
  mass <- dnorm(theta)
  a <- sieve_ordinal_score_impact(one,
    one,
    theta,
    mass,
    distribution = "N(0,1) truncated grid",
    anchors = "Known invariant simulated anchor"
  )
  expect_equal(a$items$signed, 0, tolerance = 1e-14)
  expect_equal(a$items$rms, 0, tolerance = 1e-14)
  shifted <- one
  shifted$i$thresholds <- shifted$i$thresholds + .5
  b <- sieve_ordinal_score_impact(one, shifted, theta, mass, distribution = "N(0,1)", anchors = "Known")
  expect_lt(b$items$signed, 0)
  expect_gt(b$items$rms, 0)
  # Equivalent underlying-response rescaling changes no category probability.
  rescaled <- shifted
  rescaled$i$loading <- 3
  rescaled$i$thresholds <- 3 * shifted$i$thresholds
  rescaled$i$residual_sd <- 3
  c <- sieve_ordinal_score_impact(one, rescaled, theta, mass, distribution = "N(0,1)", anchors = "Known")
  expect_equal(b$items, c$items, tolerance = 1e-13)
  # Equivalent latent metric theta_new=2*theta+3.
  conv <- function(z) {
    z$i$loading <- z$i$loading / 2
    z$i$intercept <- z$i$intercept - 3 * z$i$loading
    z
  }
  d <- sieve_ordinal_score_impact(conv(one), conv(shifted), 2 * theta + 3, mass, distribution = "N(3,2)", anchors = "Known")
  expect_equal(b$items, d$items, tolerance = 1e-13)
})


test_that("a frozen reference and its exact definition survive project restore", {
  x <- norm_fixture()
  st <- sieve_new_state()
  shiny::isolate({
    st$raw <- x$data
    st$items <- x$data
    st$meta <- x$meta
    st$roles <- sieve_propose_roles(x$data)
    st$roles$role <- "item"
    st$confirmed <- TRUE
    st$estimator <- "MLR"
    st$dataset_name <- "Norm calibration fixture"
    ref <- sieve_norm_freeze(st$items, st$meta, name = "Frozen", population = "Synthetic")
    st$norm_references <- list(Frozen = ref)
    snap <- sieve_project_snapshot(st)
    fresh <- sieve_new_state()
    sieve_project_restore(fresh, snap)
    expect_identical(fresh$norm_references$Frozen, ref)
    expect_equal(
      sieve_norm_apply(fresh$norm_references$Frozen, fresh$items, fresh$meta)$T,
      sieve_norm_apply(ref, x$data, x$meta)$T
    )
    dep <- sieve_norm_freeze(x$data,
      x$meta,
      name = "Dependent",
      population = "Synthetic",
      design = list(
        unit = "clustered",
        cluster = "cluster"
      )
    )
    expect_true(all(is.na(dep$uncertainty$lower)))
    expect_match(dep$uncertainty$method[1], "design-matched")
  })
})

test_that("separate anchor models retain true means and expose contaminated-anchor sensitivity", {
  withr::with_seed(23, {
    eta <- rnorm(800) + rep(c(0, .7), each = 400)
    d <- as.data.frame(sapply(seq_len(6), function(i) .8 * eta + rnorm(800, 0, .6)))
    names(d) <- paste0("x", 1:6)
    d$g <- rep(c("A", "B"), each = 400)
  })
  m <- sieve_build_meta(d, paste0("x", 1:6), list(`My Scale` = paste0("x", 1:6)))
  sets <- list(First = paste0("x", 1:3), Last = paste0("x", 4:6))
  a <- sieve_anchor_sensitivity(d, "g", m, sets, rationale = "Two prespecified synthetic anchor sets")
  expect_length(unique(a$contrasts$model_id), 2)
  expect_true(all(a$contrasts$licensed))
  expect_true(all(a$contrasts$lower < .7 & a$contrasts$upper > .7))
  contaminated <- d
  contaminated$x1[contaminated$g == "B"] <- contaminated$x1[contaminated$g == "B"] + 1
  b <- sieve_anchor_sensitivity(contaminated, "g", m, sets, rationale = "Known contaminated first anchor", discovery = TRUE)
  expect_gt(abs(diff(b$contrasts$estimate)), .15)
  expect_identical(b$exposure, "same-data discovery; confirm independently")
  expect_false(identical(a$models$First$model_id, b$models$First$model_id))
  # The second set did not include the contaminated item.
  expect_lt(abs(b$contrasts$estimate[2] - a$contrasts$estimate[2]), .05)
})


test_that("norm-freeze replay retains excluded row identities and dependent design", {
  x <- norm_fixture()
  st <- sieve_new_state()
  shiny::isolate({
    st$raw <- x$data
    st$items <- x$data
    st$meta <- x$meta
    st$roles <- sieve_propose_roles(x$data)
    st$roles$role <- "item"
    st$confirmed <- TRUE
    st$estimator <- "MLR"
    st$dataset_name <- "Norm replay fixture"
    sieve_ensure_dataset(st)
    sieve_set_case_status(st, 2L, "excluded", "Fixture")
    st$sampling_design <- list(unit = "repeated", cluster = NULL)
  })
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(
      norm_name = "Dependent calibration",
      norm_population = "Synthetic independent-unit declaration unavailable",
      norm_freeze = 1
    )
    expect_length(state$norm_references, 1)
    r <- state$norm_references[[1]]
    expect_false(2L %in% r$calibration_rows)
    expect_true(all(is.na(r$uncertainty$lower)))
    session$setInputs(norm_reference = r$name)
    live <- norm_applied()
    script <- sieve_replay_script(state)
    env <- new.env(parent = globalenv())
    eval(parse(text = script), env)
    expect_identical(env$norm_reference$calibration_rows, r$calibration_rows)
    expect_identical(env$norm_reference$design, r$design)
    expect_equal(env$norm_reference$uncertainty, r$uncertainty, tolerance = 1e-12)
    expect_equal(env$norm_applied$T, live$T, tolerance = 1e-12)
    expect_identical(env$norm_applied$original_row, sieve_case_index(state)[live$row])
  })
})

test_that("total ordinal impact aggregates functions before taking absolute distance", {
  p <- list(loading = 1, intercept = 0, thresholds = c(-1, 0, 1), residual_sd = 1, scores = 1:4)
  q <- p
  q$thresholds <- q$thresholds + .5
  theta <- seq(-4, 4, length.out = 401)
  r <- sieve_ordinal_score_impact(
    list(
      a = p,
      b = q
    ),
    list(
      a = q,
      b = p
    ),
    theta,
    dnorm(theta),
    distribution = "N(0,1)",
    anchors = "Known common metric"
  )
  expect_gt(sum(r$items$absolute), 0)
  expect_equal(r$total$absolute, 0, tolerance = 1e-14)
  expect_equal(r$total$rms, 0, tolerance = 1e-14)
})

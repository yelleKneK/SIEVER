long20_fixture <- function() withr::with_seed(14, {
  n <- 400
  f <- stats::rnorm(n)
  g <- .7 * f + sqrt(.51) * stats::rnorm(n)
  d <- data.frame(
    a1 = .8 * f + stats::rnorm(
      n,
      0,
      .6
    ),
    a2 = .7 * f + stats::rnorm(
      n,
      0,
      .7
    ),
    a3 = .9 * f + stats::rnorm(
      n,
      0,
      .5
    ),
    b1 = .8 * g + stats::rnorm(
      n,
      0,
      .6
    ),
    b2 = .7 * g + stats::rnorm(
      n,
      0,
      .7
    ),
    b3 = .9 * g + stats::rnorm(
      n,
      0,
      .5
    )
  )
  map <- data.frame(
    construct = "A",
    wave = rep(
      c(
        "T1",
        "T2"
      ),
      each = 3
    ),
    item = rep(
      paste0(
        "I",
        1:3
      ),
      2
    ),
    variable = names(d),
    wave_order = rep(1:2,
      each = 3
    )
  )
  list(data = d, map = map)
})

long20_same_estimates <- function(fit, direct, tolerance = 1e-10) {
  a <- lavaan::parameterEstimates(fit)
  b <- lavaan::parameterEstimates(direct)
  at <- match(paste(a$lhs, a$op, a$rhs), paste(b$lhs, b$op, b$rhs))
  expect_false(anyNA(at))
  expect_equal(a$est, b$est[at], tolerance = tolerance)
  expect_equal(a$se, b$se[at], tolerance = tolerance)
  expect_equal(
    lavaan::fitMeasures(fit, c("chisq", "df", "logl")),
    lavaan::fitMeasures(direct, c("chisq", "df", "logl")),
    tolerance = tolerance
  )
}

test_that("explicit longitudinal maps match hand-written syntax and fits", {
  x <- long20_fixture()
  why <- "The same item wording is repeated."
  r <- sieve_run_longitudinal(x$data, x$map, level = "scalar", justification = why)
  # The scalar rung written by hand: equal loadings and intercepts across
  # waves, first-wave mean 0 and variance 1, the second wave's free, and the
  # residual covariance of each repeated item at adjacent waves.
  syntax <- "
    A_T1 =~ l1*a1 + l2*a2 + l3*a3
    A_T2 =~ l1*b1 + l2*b2 + l3*b3
    a1 ~ n1*1
    b1 ~ n1*1
    a2 ~ n2*1
    b2 ~ n2*1
    a3 ~ n3*1
    b3 ~ n3*1
    A_T1 ~ 0*1
    A_T2 ~ NA*1
    A_T1 ~~ 1*A_T1
    A_T2 ~~ NA*A_T2
    a1 ~~ b1
    a2 ~~ b2
    a3 ~~ b3"
  direct <- lavaan::cfa(syntax, data = x$data, std.lv = TRUE, estimator = "ML", missing = "fiml", parameterization = "delta") # Auto's estimator
  long20_same_estimates(r$fit, direct)
  expect_identical(r$spec$longitudinal$mapping, x$map)
  expect_identical(r$spec$longitudinal$justification, why)
  expect_match(r$spec$model, "a1 ~~ NA\\*b1")
  env <- new.env(parent = globalenv())
  env$items <- x$data
  eval(parse(text = sieve_cfa_code(r$spec, fit_name = "replay_long")), env)
  expect_equal(lavaan::coef(env$replay_long), lavaan::coef(r$fit), tolerance = 1e-10)
  contrasts <- sieve_longitudinal_contrasts(r)
  p <- lavaan::parTable(direct)
  j <- which(p$lhs == "A_T2" & p$op == "~1")
  expect_equal(contrasts$estimate, p$est[j], tolerance = 1e-12)
  expect_equal(contrasts$se, sqrt(lavaan::lavInspect(direct, "vcov")[p$free[j], p$free[j]]), tolerance = 1e-12)
  metric <- sieve_run_longitudinal(x$data, x$map, level = "metric", justification = why)
  cmp <- sieve_longitudinal_compare(metric, r)
  oracle <- sieve_fit_compare(list(metric = metric$fit, scalar = direct))
  expect_equal(cmp$table, oracle$table, tolerance = 1e-10)
  expect_match(sieve_longitudinal_contrasts(metric)$reason, "identification")
})

test_that("incomplete, duplicated or incompatible longitudinal identities are refused", {
  x <- long20_fixture()
  d <- x$data
  m <- x$map
  expect_error(sieve_longitudinal_spec(d, m[-6, ], justification = "repeat"), "incomplete or scrambled")
  m$item[6] <- "I1"
  expect_error(sieve_longitudinal_spec(d, m, justification = "repeat"), "incomplete or scrambled")
  m <- x$map
  m$variable[6] <- m$variable[5]
  expect_error(sieve_longitudinal_spec(d, m, justification = "repeat"), "occur once")
  expect_error(sieve_longitudinal_spec(d, x$map), "why")
  expect_error(sieve_longitudinal_spec(d, x$map, level = "thresholds", justification = "repeat"), "categorical")
  a <- sieve_run_longitudinal(d, x$map, level = "configural", justification = "repeat")
  b <- sieve_run_longitudinal(d, x$map, level = "metric", residuals = "none", justification = "repeat")
  expect_error(sieve_longitudinal_compare(a, b), "exactly the same")
  expect_match(as.character(mod_longitudinal_ui("l")), "researcher declaration")
})

test_that("ordered longitudinal thresholds and scale identification reach the fitted engine", {
  x <- long20_fixture()
  d <- as.data.frame(lapply(x$data, function(v) as.integer(cut(v, c(-Inf, -.5, 0, .5, Inf)))))
  r <- sieve_run_longitudinal(d, x$map, level = "scalar", justification = "Repeat wording", ordered = names(d))
  expect_true(sieve_has_solution(r))
  expect_identical(lavaan::lavInspect(r$fit, "options")$parameterization, "theta")
  expect_true(all(c("thresholds", "loadings", "intercepts") %in% r$spec$longitudinal$equal))
  oracle <- lavaan::cfa(r$spec$model,
    data = d,
    std.lv = TRUE,
    estimator = "WLSMV",
    missing = "listwise",
    ordered = names(d),
    parameterization = "theta"
  )
  expect_equal(lavaan::coef(r$fit), lavaan::coef(oracle), tolerance = 1e-10)
  expect_error(
    sieve_longitudinal_spec(d,
      x$map,
      justification = "repeat",
      ordered = names(d),
      estimator = "mlr"
    ),
    "ordered-indicator"
  )
})

test_that("longitudinal module keeps exact model and comparison replay without data disclosure", {
  x <- long20_fixture()
  s <- sieve_new_state()
  s$items <- x$data
  s$raw <- x$data
  s$confirmed <- TRUE
  m <- sieve_build_meta(x$data, names(x$data), list(A = names(x$data)))
  m$type <- "continuous"
  s$meta <- m
  csv <- paste(utils::capture.output(utils::write.csv(x$map, row.names = FALSE)), collapse = "\n")
  shiny::testServer(mod_longitudinal_server, args = list(state = s), {
    session$setInputs(
      mapping = csv,
      why = "The same wording is repeated",
      confirm = TRUE,
      level = "metric",
      residuals = "adjacent",
      run = 1
    )
    expect_true(sieve_has_solution(result()))
    first <- result()$analysis_id
    session$setInputs(reference = first, level = "scalar", run = 2)
    expect_null(result()$comparison$error)
    expect_equal(nrow(result()$contrasts), 1L)
    registry <- shiny::isolate(state$analyses)
    expect_true(any(vapply(registry, function(r) identical(r$kind, "longitudinal_comparison"), logical(1))))
    expect_true(any(vapply(registry, function(r) identical(r$spec$longitudinal$mapping, x$map), logical(1))))
    log <- shiny::isolate(state$log)
    session$setInputs(why = "A changed residual rationale")
    expect_match(current()$error, "historical")
    state$sampling_design <- list(unit = "clustered", cluster = "id")
    session$setInputs(run = 3)
    expect_match(result()$error, "cluster-robust")
    state$confirmed <- FALSE
    session$setInputs(run = 4)
    expect_match(result()$error, "Confirm the item pool")
  })
})

test_that("longitudinal comparison refuses changed observed data and respects study treatment", {
  x <- long20_fixture()
  a <- sieve_run_longitudinal(x$data, x$map, level = "metric", justification = "Repeat")
  d <- x$data
  d$a1[1] <- d$a1[1] + 1
  b <- sieve_run_longitudinal(d, x$map, level = "scalar", justification = "Repeat")
  expect_error(sieve_longitudinal_compare(a, b), "different observed data")
  d <- as.data.frame(lapply(x$data, function(v) as.integer(cut(v, c(-Inf, -.5, 0, .5, Inf)))))
  spec <- sieve_longitudinal_spec(d, x$map, level = "thresholds", justification = "Repeat", estimator = "wlsmv")
  expect_identical(spec$ordered, names(d))
  # Each repeated item's thresholds carry the first wave's labels.
  expect_match(spec$model, "b1 | NA*t1 + tau.1_1*t1", fixed = TRUE)
})

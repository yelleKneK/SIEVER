# Whole sampling units are checked against an independent block oracle.
test_that("cluster blocks retain exact identifiers and every eligible row", {
  id <- c(1, 1, 1 + 1e-15, 1 + 1e-15, 2, 2, 2)
  plan <- sieve_bootstrap_partition(length(id), id)
  expect_identical(plan$G, 3L)
  expected <- withr::with_seed(17, {
    blocks <- list(1:2, 3:4, 5:7)
    unlist(blocks[sample.int(3, 3, replace = TRUE)], use.names = FALSE)
  })
  expect_identical(withr::with_seed(17, sieve_bootstrap_rows(plan)), expected)
  expect_error(sieve_bootstrap_partition(7L, id[-1L]), "one nonmissing")
  expect_error(sieve_bootstrap_partition(7L, replace(id, 1, NA)), "one nonmissing")
  expect_error(sieve_bootstrap_partition(7L, rep(1, 7)), "at least two")
  spss <- haven::labelled_spss(c(1, 2, 9), na_values = 9)
  expect_error(sieve_bootstrap_partition(3L, spss), "one nonmissing")
})

test_that("counted omega resamples held clusters and replay returns the same estimates", {
  x <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  x$x4[1:20] <- NA_real_
  id <- rep(1:60, length.out = nrow(x))
  spec <- sieve_reliability_spec(names(x), rep("continuous", 3), construct = "Textual",
    missing = "fiml", ci_method = "dmar_bca", B = 4L, seed = 7L, cluster = id)
  expect_identical(spec$ci_method, "percentile")
  plan <- sieve_reliability_boot_plan(x, spec, workers = 1L)
  expect_identical(plan$status$code, "ready")
  expect_identical(plan$sampling$G, 60L)
  draws <- sieve_reliability_boot_draws(plan, 1:4)
  expect_true(all(vapply(draws, function(r) is.finite(r$estimate), logical(1))))
  expected_rows <- withr::with_seed(plan$draw_seeds[1], {
    groups <- split(seq_len(nrow(x)), factor(id, levels = unique(id)))
    unlist(groups[sample.int(60, 60, replace = TRUE)], use.names = FALSE)
  })
  oracle <- DMAR::reliability_omega(x[expected_rows, ], missing = "fiml", ci_method = "none",
    denominator = "observed")
  expect_equal(draws[[1]]$estimate, sieve_reliability_value(oracle, "estimate"), tolerance = 1e-10)
  result <- sieve_reliability_boot_result(plan, draws)
  expect_match(result$uncertainty$label, "Whole-cluster", fixed = TRUE)
  env <- new.env(parent = globalenv()); env$items <- x
  replay <- sieve_capture(eval(parse(text = result$code), envir = env))
  expect_true(replay$ok, info = replay$error)
  expect_equal(unname(unlist(env$omega_boot_draws)), vapply(draws, `[[`, numeric(1), "estimate"), tolerance = 1e-10)
})

test_that("clustered Reliability runs with an ID auxiliary and withholds supplementary IID limits", {
  withr::local_options(sieve.auto_run = "never")
  st <- siever_test_study(configure = function(s) {
    s$raw$block <- rep(1:60, length.out = nrow(s$items))
    s$sampling_design <- list(unit = "clustered", cluster = "block")
    roles <- sieve_declared_roles(s$raw, names(s$items))
    roles$role[roles$variable == "block"] <- "auxiliary"
    s$roles <- roles
  })
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(scope = "construct", treatment = "study", missing = "fiml",
      denominator = "observed", ci_method = "recommended", conf_level = .95,
      B = 4L, boot_seed = 7L, run_go = 1L)
    r <- results()
    expect_length(r, 3L)
    expect_true(all(vapply(r, function(x) is.finite(x$omega$estimate), logical(1))))
    expect_true(all(vapply(r, function(x) !"block" %in% x$spec$aux, logical(1))))
    expect_true(all(vapply(r, function(x) identical(x$omega$uncertainty$status, "pending"), logical(1))))
    for (x in r) {
      for (record in list(x$H, x$alpha, x$model_implied)) {
        expect_true(all(is.na(c(record$lower, record$upper))))
        expect_identical(record$uncertainty$status, "unavailable")
      }
      expect_true(all(is.na(c(x$H$fit_summary$lower, x$H$fit_summary$upper))))
      env <- new.env(parent = globalenv()); env$items <- state$items; env$d <- state$raw
      replay <- sieve_capture(eval(parse(text = c(sieve_reliability_H_helper_code(), x$H$code, x$alpha$code, x$model_implied$code)), env))
      expect_true(replay$ok, info = replay$error)
      alpha <- env[[sieve_reliability_object("alpha", x$spec)]]
      expect_true(all(c("ci_lower", "ci_upper") %in% alpha$term))
      expect_true(all(is.na(alpha$value[alpha$term %in% c("se", "ci_lower", "ci_upper")])))
      omega <- env[[sieve_reliability_object("omega_model_implied", x$spec)]]
      expect_true(all(c("ci_lower", "ci_upper") %in% omega$term))
      expect_true(all(is.na(omega$value[omega$term %in% c("se", "ci_lower", "ci_upper")])))
    }
    expect_match(as.character(output$method_line$html), "60 whole independent clusters", fixed = TRUE)
  })
})

test_that("clustered composite intervals replay the held specification and sample", {
  withr::local_options(sieve.auto_run = "never")
  st <- siever_test_study(configure = function(s) {
    s$raw$block <- rep(1:60, length.out = nrow(s$items))
    s$sampling_design <- list(unit = "clustered", cluster = "block")
  })
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "first", ident = "varone", estimator = "study", fit_go = 1L,
      composite_B = 100L, composite_seed = 11L)
    before <- length(state$codelog)
    session$setInputs(composite_go = 1L)
    r <- inference$composite()
    expect_null(r$result$error)
    expect_identical(r$result$sampling$G, 60L)
    expect_true(all(is.finite(r$result$table$lower)))
    records <- Filter(function(x) identical(x$spec$kind, "model_composite"), state$analyses)
    expect_length(records, 1L)
    code <- state$codelog[seq.int(before + 1L, length(state$codelog))]
    env <- new.env(parent = globalenv()); env$items <- state$items; env$d <- state$raw
    replay <- sieve_capture(eval(parse(text = code), env))
    expect_true(replay$ok, info = replay$error)
    expect_equal(env$composite_result$draws, r$result$draws, tolerance = 1e-10)
    expect_equal(env$composite_result$table, r$result$table, tolerance = 1e-10)
  })
})

test_that("frozen norms resample aligned cluster score blocks before sorting", {
  fixture <- sieve_hs9_fixture()
  st <- siever_test_study(fixture)
  x <- shiny::isolate(st$items); meta <- shiny::isolate(st$meta)
  id <- rep(1:40, length.out = nrow(x))
  ref <- sieve_norm_freeze(x, meta, name = "Blocks", population = "Calibration example",
    design = list(unit = "clustered", cluster = "block"), cluster = id)
  result <- sieve_norm_transform_limits(ref, B = 100L, seed = 5L)
  expect_true(all(result$clusters == 40L))
  expect_true(all(is.finite(result$lower)))
  expect_true(all(grepl("whole independent clusters", result$method, fixed = TRUE)))
  held <- ref$bootstrap_scores[[1L]]
  draws <- withr::with_seed(5, replicate(100L, {
    groups <- split(seq_along(held$score), factor(held$cluster, levels = unique(held$cluster)))
    at <- unlist(groups[sample.int(length(groups), length(groups), replace = TRUE)], use.names = FALSE)
    mean(held$score[at])
  }))
  expected <- unname(stats::quantile(draws, c(.025, .975)))
  expect_equal(unname(unlist(result[1, c("lower", "upper")])), expected, tolerance = 1e-12)
})

test_that("Bifactor cluster replay preserves rows excluded for missing design IDs", {
  n <- 450L
  x <- withr::with_seed(51, {
    general <- stats::rnorm(n)
    group <- matrix(stats::rnorm(n * 3), n, 3)
    as.data.frame(sapply(1:9, function(j) .6 * general + .5 * group[, ceiling(j / 3)] +
      sqrt(.39) * stats::rnorm(n)))
  })
  names(x) <- paste0("x", 1:9)
  meta <- sieve_build_meta(x, names(x), list(A = names(x)[1:3], B = names(x)[4:6], C = names(x)[7:9]))
  id <- rep(1:75, length.out = n); id[1] <- NA_integer_
  rows <- which(!is.na(id))
  r <- sieve_bifactor_bootstrap(x[rows, ], meta, B = 2L, seed = 11L,
    cluster = id[rows], original_rows = rows)
  expect_identical(r$sampling$G, 75L)
  expect_identical(r$counts$attempted, 2L)
  expect_true(any(is.finite(r$estimates)))
  env <- new.env(parent = globalenv()); env$items <- x
  replay <- sieve_capture(eval(parse(text = r$code), env))
  expect_true(replay$ok, info = replay$error)
  expect_equal(unname(env$bifactor_boot_used), unname(r$estimates[r$draws$used, , drop = FALSE]), tolerance = 1e-8)
  expect_match(sieve_bifactor_boot_notes(r)$method, "75 whole independent clusters", fixed = TRUE)
})

test_that("categorical companion replay keeps the observed and model-implied targets separate", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  d[] <- lapply(d, function(v) as.integer(cut(v, stats::quantile(v, 0:4 / 4), include.lowest = TRUE)))
  spec <- sieve_reliability_spec(names(d), rep("ordered", 3), construct = "Textual",
    denominator = "observed", ci_method = "none")
  primary <- sieve_reliability_omega(d, spec)
  companion <- sieve_reliability_model_implied(d, spec)
  env <- new.env(parent = globalenv()); env$items <- d
  eval(parse(text = c(primary$code, companion$code)), env)
  expect_equal(sieve_reliability_value(env[[sieve_reliability_object("omega", spec)]], "estimate"), primary$estimate)
  expect_equal(sieve_reliability_value(env[[sieve_reliability_object("omega_model_implied", spec)]], "estimate"), companion$estimate)
})


test_that("Reliability reports differing eligible cluster counts by construct", {
  records <- lapply(c(A = 40L, B = 35L), function(G) list(
    spec = list(construct = "", cluster = 1:2), omega = list(uncertainty = list(sampling =
      sieve_bootstrap_partition(G * 3L, rep(seq_len(G), each = 3L))))))
  for (nm in names(records)) records[[nm]]$spec$construct <- nm
  text <- sieve_reliability_sampling_line(records)
  expect_match(text, "A G = 40; B G = 35", fixed = TRUE)
  expect_match(text, "each construct's own eligible rows", fixed = TRUE)
})

test_that("one construct with a single eligible cluster does not discard the reliability table", {
  withr::local_options(sieve.auto_run = "never")
  st <- siever_test_study(configure = function(s) {
    s$raw$block <- rep(1:3, length.out = nrow(s$items))
    s$sampling_design <- list(unit = "clustered", cluster = "block")
    visual <- s$meta$item[s$meta$construct == "Visual"]
    s$items[s$raw$block != 1L, visual] <- NA_real_
  })
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(scope = "construct", treatment = "study", missing = "listwise",
      denominator = "observed", ci_method = "recommended", conf_level = .95,
      B = 4L, boot_seed = 7L, run_go = 1L)
    records <- results()
    expect_length(records, 3L)
    expect_true(is.finite(records$Visual$omega$estimate))
    expect_identical(records$Visual$omega$uncertainty$status, "unavailable")
    expect_match(records$Visual$omega$uncertainty$reason, "at least two", fixed = TRUE)
    expect_identical(records$Textual$omega$uncertainty$status, "pending")
    spec <- resolve_specs(settings(), run_bootstrap = TRUE)[[1L]]
    plan <- sieve_reliability_boot_plan(state$items, spec, workers = 1L)
    expect_identical(plan$status$code, "unavailable")
    expect_false(any(grepl("sample.int", plan$result$code, fixed = TRUE)))
    expect_match(paste(plan$result$code, collapse = " "), "ci_method = \"none\"", fixed = TRUE)
  })
})


test_that("completed independent bootstraps are not labeled clustered or unavailable", {
  record <- list(spec = list(construct = "A"), omega = list(uncertainty = list(
    sampling = sieve_bootstrap_partition(100L))))
  expect_identical(sieve_reliability_sampling_line(list(record)), "")
})


test_that("bootstrap replay refuses invalid original row positions before fitting", {
  d <- data.frame(a = 1:3, b = 3:1)
  calls <- 0L
  fit <- function(...) { calls <<- calls + 1L; stop("Must validate before fitting") }
  invalid <- list(c(0, 2, 3), c(-1, 2, 3), c(1.5, 2, 3), c(Inf, 2, 3),
    c(NA_real_, 2, 3), c(NaN, 2, 3), c(1, 1, 3), c("1", "2", "3"),
    c(TRUE, FALSE, TRUE), c(1+0i, 2+0i, 3+0i), 1:2, matrix(c(1, 1, 2), nrow = 1L),
    array(1:3, dim = c(1L, 3L, 1L)))
  for (rows in invalid) {
    expect_error(sieve_bifactor_boot_plan(d, NULL, B = 1L, fit_fun = fit,
      original_rows = rows), "unique finite positive whole positions")
    expect_error(sieve_bootstrap_partition(3L, rows = rows), "unique finite positive whole positions")
  }
  expect_identical(calls, 0L)
  expect_identical(sieve_bootstrap_row_references(c(2, 5, 9), 3L), c(2, 5, 9))
  expect_identical(sieve_bootstrap_row_references(c(2L, 5L, 9L), 3L), c(2L, 5L, 9L))
})

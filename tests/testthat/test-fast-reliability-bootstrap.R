# A fast reliability bootstrap with the same estimates: robust omega of
# complete rows from DMAR's one-factor ML fit instead of the engine's robust
# ML fit (which adds a scaled test statistic the coefficient does not use),
# the replications spread over socket workers, and the wait stated before a
# run following both.
#
# The checks are equalities, not timings: each replication's estimate against
# DMAR's raw-data call on the same resample indices (to 1e-10), every
# replication's outcome against the engine call's, the replications on two
# workers against the same run in one process, the same seed twice, and the
# recorded R replayed.
#
# A bootstrap of independent rows is DMAR's counted bootstrap
# (test-dmar-bootstraps.R), and SIEVER's counted engine runs only under a
# declared cluster design, which DMAR's engine does not resample. Its
# mechanics are checked here on singleton clusters: with one row per
# cluster, the whole-cluster draw is the row draw (sieve_bootstrap_rows()
# returns sample.int(n, n, replace = TRUE) either way), so every equality
# below holds for the row bootstrap as well.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")

boot_one_factor <- function(n, loadings, seed) {
  withr::with_seed(seed, {
    f <- stats::rnorm(n)
    X <- sapply(loadings, function(l) l * f + sqrt(1 - l^2) * stats::rnorm(n))
  })
  X <- as.data.frame(X)
  names(X) <- paste0("y", seq_along(loadings))
  X
}

boot_hs9 <- function() {
  fx <- sieve_hs9_fixture()
  skip_if(is.null(fx))
  fx
}

# One cluster per row, aligned with every row of the data.
singletons <- function(data) seq_len(nrow(data))

# The specs below name missing = "listwise": their subject is the complete
# case replication path (an ML fit per replication), and a spec's default
# is FIML, DMAR's default.

test_that("each replication of robust omega equals DMAR's raw-data estimate for the same rows", {
  fx <- boot_hs9()
  hs24 <- sieve_hs_fixture()
  cases <- list(
    list(data = fx$data, items = fx$theory_map$Visual),
    list(data = hs24$data, items = hs24$theory_map[[which.max(lengths(hs24$theory_map))]]),
    list(data = boot_one_factor(300, c(.8, .7, .6, .5, .6, .7, .4, .55), 31), items = paste0("y", 1:8))
  )
  for (case in cases) {
    spec <- sieve_reliability_spec(case$items, rep("continuous", length(case$items)), "F",
      missing = "listwise", ci_method = "percentile", B = 25, seed = 20261022, cluster = singletons(case$data)
    )
    plan <- sieve_reliability_boot_plan(case$data, spec, workers = 1L)
    expect_identical(plan$status$code, "ready")
    # Robust omega of complete rows: each replication from DMAR's one-factor
    # ML fit, confirmed on the full rows before it is used.
    expect_identical(plan$path, "ml_fit")
    expect_identical(plan$result$uncertainty$boot_path, "ml_fit")
    expect_true(is.finite(plan$seconds_per_draw) && plan$seconds_per_draw > 0)
    pool <- plan$pool
    for (b in seq_len(spec$B)) {
      draw <- sieve_reliability_boot_replicate(plan$task, b)
      rows <- withr::with_seed(plan$draw_seeds[b], sample.int(nrow(pool), nrow(pool), replace = TRUE))
      engine <- suppressWarnings(DMAR::reliability_omega(pool[rows, , drop = FALSE],
        ci_method = "none", denominator = "observed", missing = "listwise"
      ))
      expect_lte(abs(draw$estimate - engine$value[engine$term == "estimate"]), 1e-10)
    }
  }
})

test_that("every replication's outcome is the engine call's, improper and failed ones included", {
  # 60 respondents, one item with a loading near 1: some resamples give a
  # negative error variance, one does not converge.
  X <- boot_one_factor(60, c(.97, .55, .45, .35), 4)
  spec <- sieve_reliability_spec(names(X), rep("continuous", 4), "F",
    missing = "listwise", ci_method = "percentile", B = 60, seed = 11, cluster = singletons(X)
  )
  plan <- sieve_reliability_boot_plan(X, spec, workers = 1L)
  expect_identical(plan$path, "ml_fit")
  raw <- plan$task
  raw$path <- "raw"
  fast <- lapply(seq_len(spec$B), function(b) sieve_reliability_boot_replicate(plan$task, b))
  engine <- lapply(seq_len(spec$B), function(b) sieve_reliability_boot_replicate(raw, b))
  outcomes <- vapply(fast, function(d) d$outcome, character(1))
  expect_true(all(c("used", "inadmissible") %in% outcomes))
  for (b in seq_len(spec$B)) {
    expect_identical(fast[[b]][setdiff(names(fast[[b]]), c("estimate", "message"))],
      engine[[b]][setdiff(names(engine[[b]]), c("estimate", "message"))]
    )
    if (!isTRUE(engine[[b]]$admissible)) {
      expect_match(fast[[b]]$message, "negative|improper|converge", ignore.case = TRUE)
      expect_match(engine[[b]]$message, "negative|improper|converge", ignore.case = TRUE)
    }
    if (is.finite(engine[[b]]$estimate)) {
      expect_lte(abs(fast[[b]]$estimate - engine[[b]]$estimate), 1e-10)
    } else {
      expect_identical(fast[[b]]$estimate, engine[[b]]$estimate)
    }
  }
})

test_that("the engine call is repeated where the ML fit is not the engine's coefficient", {
  X <- boot_one_factor(200, c(.7, .6, .65, .5), 8)
  X$y2[c(3, 9, 40)] <- NA
  fiml <- sieve_reliability_spec(names(X), rep("continuous", 4), "F",
    missing = "fiml", ci_method = "percentile", B = 20, seed = 3, cluster = singletons(X)
  )
  implied <- sieve_reliability_spec(names(X), rep("continuous", 4), "F",
    ci_method = "percentile", denominator = "model_implied", B = 20, seed = 3
  )
  ordered <- sieve_reliability_spec(names(X), rep("ordered", 4), "F", ci_method = "percentile", B = 20)
  expect_identical(sieve_reliability_boot_path(fiml), "raw")
  expect_identical(sieve_reliability_boot_path(implied), "raw")
  expect_identical(sieve_reliability_boot_path(ordered), "raw")
  plan <- sieve_reliability_boot_plan(X, fiml, workers = 1L)
  expect_identical(plan$path, "raw")
  # The time of a replication is then the time of the engine call.
  expect_identical(plan$seconds_per_draw, plan$result$seconds)
  expect_false(any(grepl("cfa_1", sieve_reliability_code(fiml, rng = plan$rng), fixed = TRUE)))
})

test_that("two workers return the same replications and interval as one process, and the same seed twice", {
  fx <- boot_hs9()
  spec <- sieve_reliability_spec(fx$theory_map$Textual, rep("continuous", 3), "Textual",
    missing = "listwise", ci_method = "percentile", B = 45, seed = 515, cluster = singletons(fx$data)
  )
  one <- sieve_reliability_omega(fx$data, spec, workers = 1L)
  two <- sieve_reliability_omega(fx$data, spec, workers = 2L)
  again <- sieve_reliability_omega(fx$data, spec, workers = 2L)
  expect_identical(one$uncertainty$status, "completed")
  expect_identical(two$uncertainty$draws, one$uncertainty$draws)
  expect_identical(c(two$lower, two$upper, two$se), c(one$lower, one$upper, one$se))
  expect_identical(c(again$lower, again$upper), c(two$lower, two$upper))
  expect_identical(two$uncertainty$counts, one$uncertainty$counts)
  # A stop keeps exactly the replications before it, wherever they ran.
  stopped <- sieve_reliability_omega(fx$data, spec,
    workers = 2L,
    progress = function(done, total, draw) done < 12
  )
  expect_identical(stopped$uncertainty$B_attempted, 12L)
  expect_identical(stopped$uncertainty$draws, one$uncertainty$draws[1:12, ])
  # A counted bootstrap of ordered items on two workers is the same too.
  Xo <- as.data.frame(lapply(boot_one_factor(150, c(.7, .6, .65, .55), 21), function(x) {
    as.numeric(cut(x, c(-Inf, -.7, 0, .7, Inf)))
  }))
  ospec <- sieve_reliability_spec(names(Xo), rep("ordered", 4), "F", ci_method = "percentile", B = 8, seed = 2,
    cluster = singletons(Xo))
  expect_identical(
    sieve_reliability_omega(Xo, ospec, workers = 2L)$uncertainty$draws,
    sieve_reliability_omega(Xo, ospec, workers = 1L)$uncertainty$draws
  )
})

test_that("the batches and the workers follow the plan, and the workers stop with the run", {
  fx <- boot_hs9()
  spec <- sieve_reliability_spec(fx$theory_map$Speed, rep("continuous", 3), "Speed",
    missing = "listwise", ci_method = "percentile", B = 30, seed = 9, cluster = singletons(fx$data)
  )
  serial <- sieve_reliability_boot_plan(fx$data, spec, workers = 1L)
  expect_identical(sieve_reliability_boot_next(serial, 0L), 1L)
  expect_identical(sieve_reliability_boot_next(serial, 30L), integer(0))
  plan <- sieve_reliability_boot_plan(fx$data, spec, workers = 2L)
  # About half a second of replications per worker: at 0.1 seconds a
  # replication, five each.
  plan$seconds_per_draw <- 0.1
  first <- sieve_reliability_boot_next(plan, 0L)
  expect_identical(first, 1:10)
  expect_identical(sieve_reliability_boot_next(plan, 25L), 26:30)
  got <- sieve_reliability_boot_draws(plan, first)
  expect_false(is.null(plan$crew$pool))
  expect_identical(got, lapply(first, function(b) sieve_reliability_boot_replicate(serial$task, b)))
  rest <- 11:30
  got_rest <- sieve_reliability_boot_draws(plan, rest)
  # The last replication is in: the workers are stopped.
  expect_null(plan$crew$pool)
  expect_identical(vapply(c(got, got_rest), function(d) d$draw, integer(1)), 1:30)
  # The decision rule: workers only when the run would take longer than
  # the threshold in one process, and only with two or more available.
  expect_identical(sieve_reliability_boot_workers(5, cores = 4L), 1L)
  expect_identical(sieve_reliability_boot_workers(600, cores = 4L), 4L)
  expect_identical(sieve_reliability_boot_workers(600, cores = 1L), 1L)
})

test_that("the wait stated before a run follows the replication path and the workers", {
  fx <- boot_hs9()
  spec <- sieve_reliability_spec(fx$theory_map$Visual, rep("continuous", 3), "Visual",
    missing = "listwise", run_bootstrap = FALSE, cluster = singletons(fx$data)
  )
  p <- sieve_reliability_point(fx$data, spec)
  rec <- p$out
  # A pending robust omega bootstrap knows how each replication will be
  # estimated and how long one takes.
  expect_identical(rec$uncertainty$status, "pending")
  expect_identical(rec$uncertainty$boot_path, "ml_fit")
  expect_true(is.finite(rec$uncertainty$seconds_per_draw))
  rec$seconds <- 0.05
  rec$uncertainty$seconds_per_draw <- 0.02
  sieve_at_default_rates({
    # One process: the time of one replication times B.
    expect_equal(sieve_reliability_boot_wait(rec, 200L, "percentile", cores = 4L), 0.02 * 200)
    # Workers: divided over them, plus their start.
    expect_equal(sieve_reliability_boot_wait(rec, 10000L, "percentile", cores = 4L), 2 + 0.02 * 10000 / 4)
    expect_equal(sieve_reliability_boot_wait(rec, 10000L, "percentile", cores = 1L), 0.02 * 10000)
    # Without a cluster design the bootstrap is DMAR's, in the background
    # process: the fits, the BCa jackknife included, over DMAR's processes,
    # plus the process's start (sieve_dmar_boot_wait()).
    independent <- rec
    independent$spec$cluster <- NULL
    expect_equal(
      sieve_reliability_boot_wait(independent, 200L, "dmar_bca", cores = 4L),
      sieve_dmar_boot_wait(sieve_bootstrap_wait(0.05, 200L, rec$n$used, "dmar_bca"), 4L)
    )
  })
})

test_that("the recorded R replays the replications of the ML fit exactly", {
  fx <- boot_hs9()
  spec <- sieve_reliability_spec(fx$theory_map$Visual, rep("continuous", 3), "Visual",
    missing = "listwise", ci_method = "percentile", B = 40, cluster = singletons(fx$data)
  )
  res <- sieve_reliability_omega(fx$data, spec, workers = 2L)
  code <- paste(res$code, collapse = "\n")
  # (missing = "listwise" by name: DMAR's cfa_1() defaults to FIML, on
  # complete data too.)
  expect_match(code, "DMAR::cfa_1(data = data, estimator = \"ML\", se = \"none\", output = \"fit\", missing = \"listwise\")", fixed = TRUE)
  expect_match(code, "DMAR::reliability_omega(data = omega_boot_pool", fixed = TRUE)
  expect_false(any(grepl("sieve_", res$code)))
  env <- new.env(parent = globalenv())
  env$items <- fx$data
  replay <- withr::with_preserve_seed(eval(parse(text = res$code), envir = env))
  draws <- res$uncertainty$draws
  expect_identical(env$omega_boot_used, draws$estimate[draws$used])
  expect_identical(unname(replay[["estimate"]]), res$estimate)
  expect_identical(as.integer(replay[["used"]]), res$uncertainty$B_used)
  expect_equal(unname(replay[c("lower", "upper")]), c(res$lower, res$upper), tolerance = 1e-14)
})

test_that("DMAR's bootstraps share the background process, and their waits add up to the wait of the run", {
  # (Every DMAR bootstrap of a run runs in one background process, over
  # DMAR's own processes; test-dmar-bootstraps.R checks that it returns
  # what each call returns alone.)
  rec <- list(seconds = 0.2, n = list(used = 500L), uncertainty = list(), spec = list())
  sieve_at_default_rates({
    serial <- sieve_bootstrap_wait(0.2, 1000L, 500L, "dmar_bca")
    # The page asks the background process for the four workers, whatever
    # this process may do (it decides there whether to fork).
    ncpus <- 4L
    blocks <- 50 * sieve_rate("dmar_boot_block")
    expect_equal(sieve_reliability_boot_wait(rec, 1000L, "dmar_bca", cores = 4L, alongside = 3L),
      sieve_rate("dmar_boot_start") / 3 + serial / ncpus + blocks)
    expect_equal(sieve_reliability_boot_wait(rec, 1000L, "dmar_bca", cores = 1L, alongside = 1L),
      sieve_rate("dmar_boot_start") + serial)
  })
})

test_that("the Reliability tab's bootstrap on workers gives the intervals it gives in one process", {
  # Controlled worker/serial transport mechanics; production B400 intervals
  # are independently checked in test-dmar-results.R.
  siever_local_dmar_mechanics()
  boot_state <- function(ordered = NULL) {
    fx <- boot_hs9()
    # The first 120 respondents, so the BCa jackknife (one refit per row when
    # there are fewer replications than rows) stays short.
    fx$data <- fx$data[1:120, ]
    s <- sieve_new_state()
    s$dataset_name <- fx$name
    s$raw <- fx$data
    s$items <- fx$data[, fx$item_cols, drop = FALSE]
    s$id_vars <- fx$id_vars
    s$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
    s$theory <- list(k = fx$theory_k, label = fx$theory_lbl, map = fx$theory_map)
    s$confirmed <- TRUE
    s$palette <- "nd"
    s$thresholds <- sieve_default_thresholds()
    shiny::isolate({
      meta <- s$meta
      items <- s$items
      hit <- meta$construct %in% ordered
      for (it in meta$item[hit]) items[[it]] <- as.integer(cut(as.numeric(items[[it]]), breaks = 5))
      meta$type[hit] <- "ordered"
      s$items <- items
      s$meta <- meta
    })
    s
  }
  run_tab <- function(processes) {
    # DMAR's processes in the background process: for a given seed the
    # intervals do not depend on them. The page asks for
    # sieve_dmar_boot_cores(); the background process decides whether it
    # may fork, so this process need not.
    testthat::local_mocked_bindings(
      sieve_dmar_boot_cores = function(cores = 1L) processes,
      .package = "SIEVER"
    )
    # Visual continuous (the percentile bootstrap), Textual and Speed
    # ordered (BCa): all DMAR's counted bootstrap, in one background run.
    st <- boot_state(ordered = c("Textual", "Speed"))
    out <- NULL
    shiny::testServer(mod_reliab_server, args = list(state = st), {
      session$setInputs(
        denominator = "observed", ci_method = "recommended", missing = "listwise",
        conf_level = 0.95, B = 50, boot_seed = 123, show_alpha = FALSE, score_method = "Bartlett"
      )
      session$setInputs(run_go = 1)
      waits <- boot_plan()$seconds
      session$setInputs(run_boot = 1)
      # A run still going at the limit fails here, saying so.
      siever_wait_until(function() !isTRUE(boot_running()),
        what = sprintf("The page's background bootstrap (sieve_dmar_boot_cores() = %d)", processes),
        step = function() {
          session$elapse(200)
          Sys.sleep(0.05)
        })
      expect_false(boot_running())
      df <- reliab()
      out <<- list(df = df, waits = waits)
    })
    out
  }
  workers <- run_tab(2L)
  serial <- run_tab(1L)
  expect_true(all(serial$df$uncertainty == "completed"))
  expect_identical(sort(unique(serial$df$interval)), c("Bootstrap BCa (DMAR)", "Bootstrap percentile"))
  expect_identical(
    workers$df[, c("construct", "omega", "ci_lower", "ci_upper", "interval", "boot_attempted", "boot_used")],
    serial$df[, c("construct", "omega", "ci_lower", "ci_upper", "interval", "boot_attempted", "boot_used")]
  )
  # The wait was stated before each run.
  expect_true(all(is.finite(workers$waits) & workers$waits > 0))
  expect_true(all(is.finite(serial$waits) & serial$waits > 0))
})

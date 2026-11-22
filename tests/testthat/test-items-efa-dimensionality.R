# The Items, EFA and Dimensionality tabs: the correlations and estimators
# they follow, inadmissible solutions, retention criteria, currency after
# edits, the visible model statement, the one run policy, the memory
# refusal for maximum likelihood fits, and the Items tab's robust omega.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")
withr::local_options(sieve.cores = 2L) # at most two workers, whatever the cores

ri_text <- function(x) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))

ri_state <- function(fixture = sieve_hs9_fixture()) {
  siever_test_study(fixture, configure = function(s) s$roles <- NULL)
}

# ---- the memory refusal for MLR --------------------------------------------

test_that("an MLR fit whose memory need exceeds the machine is refused before it starts, with the ways forward in order", {
  # The need comes from the formula, never from a trial: 303 items have
  # 46,359 sample moments and need over 300 GB under MLR; under ML 2.4 GB.
  expect_equal(sieve_ml_bytes(303, "MLR"), 6e8 + 147 * (303 * 304 / 2 + 303)^2)
  expect_gt(sieve_ml_bytes(303, "MLR"), 300e9)
  expect_equal(sieve_ml_bytes(110, "MLR") / 1e9, 6.3, tolerance = 0.1)
  expect_equal(sieve_ml_bytes(68, "MLR") / 1e9, 1.3, tolerance = 0.2)
  expect_equal(sieve_ml_bytes(303, "ML") / 1e9, 2.4, tolerance = 0.1)

  limit <- 24e9 # three quarters of a 32 GB machine
  msg <- sieve_mlr_guard(303, "MLR", limit = limit)
  expect_type(msg, "character")
  expect_match(msg, "^Not fit: MLR's scaled test statistic needs")
  expect_match(msg, "46,359", fixed = TRUE)
  first <- regexpr("one at a time", msg, fixed = TRUE)
  # The second way forward from a memory refusal is ML on the complete
  # cases: lavaan 0.7-3 forms the same matrix of the moments
  # for the fit measures of an ML fit by FIML (test-estimator-default.R).
  second <- regexpr("Second, ML on the complete cases", msg, fixed = TRUE)
  expect_true(first > 0 && second > first)
  expect_false(grepl("Second, ML with FIML", msg, fixed = TRUE))
  expect_match(msg, "inflate the chi square", fixed = TRUE)
  expect_match(msg, "does not change the estimator", fixed = TRUE)
  # A pool of 303 items, n = 500. Memory, and the rank of the sandwich
  # (909 free parameters with the intercepts for 500 respondents).
  aa <- sieve_mlr_guard(303, "MLR", limit = limit, n = 500)
  expect_match(aa, "^Not fit, for two reasons")
  expect_match(aa, "rank deficient: the model has 909 free parameters and there are 500 respondents", fixed = TRUE)
  expect_match(aa, "Robust (Huber-White) standard errors with ML are not offered here", fixed = TRUE)
  # The sample alone can refuse MLR, whatever the memory.
  rank_only <- sieve_mlr_guard(200, "MLR", limit = 1e12, n = 300)
  expect_match(rank_only, "^Not fit: MLR's robust standard errors would be rank deficient")
  # Robust standard errors with the normal-theory test are offered when the
  # free parameters are well below n and the ML need fits.
  offered <- sieve_mlr_guard(150, "MLR", limit = 8e9, n = 2000)
  expect_match(offered, "^Not fit: MLR's scaled test statistic")
  expect_match(offered, "Third, ML with FIML and robust (Huber-White) standard errors", fixed = TRUE)
  expect_true(regexpr("Second, ML on the complete cases", offered) < regexpr("Third, ML with FIML", offered))
  expect_null(sieve_mlr_guard(30, "MLR", limit = limit, n = 500))
  # Within the budget, or under ML at 303 items, or for another estimator:
  # nothing is refused.
  expect_null(sieve_mlr_guard(68, "MLR", limit = limit))
  expect_null(sieve_mlr_guard(303, "ML", limit = limit))
  expect_null(sieve_mlr_guard(303, "ML", limit = limit, missing = "listwise"))
  # ML with FIML forms MLR's matrix of the moments for its fit measures.
  expect_match(sieve_mlr_guard(303, "ML", limit = limit, missing = "fiml"), "^Not fit: under ML with FIML")
  expect_null(sieve_mlr_guard(303, "WLSMV", limit = limit))
  # One count per separately fit model: the largest decides.
  expect_type(sieve_mlr_guard(c(20, 303), "MLR", limit = limit), "character")
  expect_null(sieve_mlr_guard(c(20, 40), "MLR", limit = limit))
})

test_that("the exploratory engine refuses an MLR request over the memory limit without fitting", {
  withr::local_options(sieve.memory_limit = 1e6)
  X <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  calls <- 0L
  testthat::local_mocked_bindings(
    efa = function(...) {
      calls <<- calls + 1L
      stop("lavaan::efa must not run")
    },
    .package = "lavaan"
  )
  out <- sieve_efa_solutions(X, "MLR", ks = 1:2)
  expect_identical(calls, 0L)
  msgs <- vapply(out, function(o) paste(o$row$message, collapse = " "), character(1))
  expect_true(all(grepl("Not fit: MLR's scaled test statistic", msgs, fixed = TRUE)))
})

test_that("every on-request fit refuses an MLR model over the memory limit and computes nothing", {
  withr::local_options(sieve.memory_limit = 1e6)
  s <- ri_state()
  ran <- new.env()
  ran$n <- 0L
  probe_server <- function(id, state) {
    shiny::moduleServer(id, function(input, output, session) {
      wire <- sieve_demand_wire(input, session, state,
        id = "go", what = "Probe",
        key = shiny::reactive(sieve_demand_key(state, list())),
        plan = shiny::reactive(list(seconds = 1, label = "about 1 second", progress = "Probe",
          p = 303L, estimator = "MLR")),
        compute = function(key, plan) {
          ran$n <- ran$n + 1L
          list(value = 1)
        }
      )
      line <- shiny::reactive(wire$line())
      run <- wire$run
    })
  }
  shiny::testServer(probe_server, args = list(state = s), {
    expect_match(ri_text(line()), "MLR's scaled test statistic needs", fixed = TRUE)
    expect_match(ri_text(line()), "rank deficient", fixed = TRUE) # 909 parameters, 301 respondents
    run()
    expect_identical(ran$n, 0L)
  })
})

test_that("the EFA tab's DMAR fits and the Dimensionality fit by k refuse an MLR request over the limit", {
  withr::local_options(sieve.memory_limit = 1e6)
  s <- ri_state()
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_mocked_bindings(sieve_efa_dmar_start = function(...) {
    calls$n <- calls$n + 1L
    stop("A refused comparison must not start a worker.")
  })
  shiny::testServer(mod_efa_server, args = list(state = s), {
    session$setInputs(fm = "ml", rotation = "oblimin", cor_type = "auto",
      n_factors = 3L, estimator = "mlr", count_to = 3L, count_criterion = "BIC")
    session$setInputs(efa_go = 1)
    r <- efa_run()
    expect_null(r$fit)
    expect_match(r$refused, "Not fit: MLR's scaled test statistic", fixed = TRUE)
    expect_identical(count_run()$status, "refused")
    expect_match(count_run()$error, "Not fit: MLR's scaled test statistic", fixed = TRUE)
    session$setInputs(count_go = 1)
    expect_identical(calls$n, 0L)
    expect_null(count_job$key)
  })
  s2 <- ri_state()
  shiny::testServer(mod_retention_server, args = list(state = s2), {
    session$setInputs(cor_type = "auto", n_iter = 20, seed = 11, max_k = 3, estimator = "mlr")
    f <- fit_run()
    expect_identical(f$status, "not run")
    expect_true(isTRUE(f$refused))
    expect_match(f$reason, "Not fit: MLR's scaled test statistic", fixed = TRUE)
  })
})

# ---- the Items EFA follows the study's correlation request -----------------

ri_ordered_fixture <- function(n = 300L, seed = 113L) {
  withr::with_seed(seed, {
    f <- matrix(stats::rnorm(n * 2L), n, 2L) %*% chol(matrix(c(1, .4, .4, 1), 2L))
    lam <- c(.8, .7, .6, .7, .6, .5)
    which_f <- rep(1:2, each = 3L)
    y <- sapply(seq_along(lam), function(j) {
      z <- lam[j] * f[, which_f[j]] + sqrt(1 - lam[j]^2) * stats::rnorm(n)
      as.integer(cut(z, c(-Inf, -1.2, -0.3, 0.4, 1.1, Inf)))
    })
  })
  colnames(y) <- paste0("q", seq_len(ncol(y)))
  d <- as.data.frame(y)
  list(
    name = "ordered probe", data = d, item_cols = names(d), id_vars = character(0),
    theory_map = list(A = names(d)[1:3], B = names(d)[4:6]), labels = stats::setNames(names(d), names(d)),
    theory_k = 2L, theory_lbl = "two constructs"
  )
}

test_that("the Items diagnostics follow a change of the study's estimator to the polychoric basis", {
  s <- ri_state(ri_ordered_fixture())
  m <- shiny::isolate(s$meta)
  m$type <- "continuous"
  s$meta <- m
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    d1 <- diag()
    expect_identical(d1$basis_used, "pearson")
    s$estimator_choice <- "wlsmv" # the Ingest tab: every item treated as ordered
    session$flushReact()
    d2 <- diag()
    b <- sieve_state_basis(s)
    expect_identical(b$used, "polychoric")
    expect_identical(d2$basis_used, "polychoric")
    # (The Items tab's factor analysis is DMAR::fa_exploratory(), maximum
    # likelihood on the shared matrix, so the loadings are asserted against
    # DMAR called directly on the polychoric matrix with the recorded seed,
    # exactly, and against psych::fa on the same matrix within the optimizers'
    # tolerance.)
    direct <- withr::with_seed(d2$rng$seed,
      DMAR::fa_exploratory(S = b$R, N = b$n, nfactors = 2, estimator = "ML", rotation = "oblimin",
        conf_level = 0.95, seed = d2$rng$seed),
      .rng_kind = d2$rng$kind[1L], .rng_normal_kind = d2$rng$kind[2L], .rng_sample_kind = d2$rng$kind[3L]
    )
    P <- sieve_efa_dmar_solution(direct)$loadings
    expect_equal(sort(abs(d2$items$loading_target)), unname(sort(apply(abs(P), 1, max))), tolerance = 1e-10)
    ps <- withr::with_seed(1, psych::fa(b$R, nfactors = 2, n.obs = b$n, fm = "ml", rotate = "oblimin"))
    expect_lt(max(abs(sort(abs(d2$items$loading_target)) - sort(apply(abs(unclass(ps$loadings)), 1, max)))), 1e-4)
    expect_false(isTRUE(all.equal(d1$items$loading_target, d2$items$loading_target)))
  })
})

# ---- a Heywood case at the engine's bound is inadmissible ------------------

test_that("a uniqueness at psych's bound and a variance at lavaan's bound are inadmissible, and the Items diagnostics say so", {
  fx <- sieve_hs9_fixture()
  X <- fx$data[, fx$item_cols, drop = FALSE]
  R <- stats::cor(X)
  f5 <- withr::with_seed(1, psych::fa(R, nfactors = 5, n.obs = 301, fm = "ml", rotate = "oblimin"))
  expect_true(any(f5$uniquenesses <= 0.005 + 1e-6))
  st <- sieve_efa_status(f5)
  expect_identical(st$code, "inadmissible")
  expect_setequal(st$at_bound, names(f5$uniquenesses)[f5$uniquenesses <= 0.005 + 1e-6])
  expect_match(st$message, "lower bound of 0.005", fixed = TRUE)
  f4 <- withr::with_seed(1, psych::fa(R, nfactors = 4, n.obs = 301, fm = "ml", rotate = "oblimin"))
  expect_identical(sieve_efa_status(f4)$code, "inadmissible")
  f3 <- withr::with_seed(1, psych::fa(R, nfactors = 3, n.obs = 301, fm = "ml", rotate = "oblimin"))
  expect_identical(sieve_efa_status(f3)$code, "ok")

  hs <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  fl <- suppressWarnings(lavaan::efa(data = hs, nfactors = 5, rotation = "none", estimator = "ML"))
  if (inherits(fl, "efaList")) fl <- fl[[1]]
  theta <- diag(lavaan::lavInspect(fl, "est")$theta)
  expect_true(any(theta <= 1e-8))
  sl <- sieve_fit_status(fl)
  expect_identical(sl$code, "inadmissible")
  expect_setequal(sl$boundary_variances, names(theta)[theta <= 1e-8])
  fl3 <- suppressWarnings(lavaan::efa(data = hs, nfactors = 3, rotation = "none", estimator = "ML"))
  if (inherits(fl3, "efaList")) fl3 <- fl3[[1]]
  expect_identical(sieve_fit_status(fl3)$code, "ok")

  # The Items diagnostics' factor analysis is DMAR::fa_exploratory(): DMAR
  # does not hold a uniqueness at a bound, it refuses the improper solution by
  # class with its reason (here the likelihood has no maximum), so the
  # five-factor solution of these nine tests is not shown at all and the
  # status line says why.
  m <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  m$construct <- c("A", "A", "B", "C", "C", "D", "E", "E", "D")
  d <- sieve_item_diagnostics(X, m, rng = sieve_draw_seed(1))
  expect_identical(d$status$code, "failed")
  expect_identical(d$status$refusal, "dmar_no_maximum")
  expect_identical(d$status$reason, "no maximum")
  expect_true(all(is.na(d$items$loading_target)))
  # The item-rest correlations do not depend on the factor analysis (B has
  # one item, so it has none).
  expect_true(all(is.finite(d$items$item_total[d$items$construct != "B"])))
  s <- ri_state()
  mm <- shiny::isolate(s$meta)
  mm$construct <- m$construct
  s$meta <- mm
  shiny::isolate(sieve_confirm_types(s))
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    # A count DMAR is expected to refuse is estimated at several seconds
    # (sieve_efa_refusal_wait()), so the diagnostics wait for Update.
    expect_match(ri_text(output$status_line), "not computed yet", fixed = TRUE)
    session$setInputs(update_go = 1)
    # The refused fit runs in a background R process.
    siever_wait_until(function() !is.null(diag()$status$code), "The refused fit", step = function() {
      Sys.sleep(0.2)
      session$elapse(500)
    })
    expect_identical(diag()$status$code, "failed")
    expect_match(ri_text(output$status_line), "was not fit: DMAR refused the solution: the likelihood has no maximum (reason: no maximum).", fixed = TRUE)
  })
})

# ---- a MAP or VSS optimum at the largest k probed is "at least" ------------

test_that("common factor retention has no component MAP criterion", {
  fx <- sieve_hs_fixture()
  X <- fx$data[, fx$item_cols, drop = FALSE]
  b <- sieve_retention_cor(X, rep("continuous", ncol(X)), "pearson")
  no_pa <- list(ok = FALSE, status = "failed", reason = "skipped", reference = list())
  wide <- sieve_retention_vss(b, max_k = 8L, seed = 2L)
  expect_null(wide$map)
  expect_null(wide$map0)
  expect_true(any(is.finite(wide$vss1)))
  expect_true(any(is.finite(wide$vss2)))
  crit <- sieve_retention_criteria(b, no_pa, wide)
  expect_false("map" %in% crit$id)
  expect_false("map" %in% sieve_retention_summary(crit)$available)
  expect_false(grepl("MAP|component", paste(sieve_retention_evidence_lines(crit), collapse = " ")))
  expect_equal(crit$suggested[crit$id == "vss1"], sieve_vss_count(wide$vss1, wide$identified)$k)
})

test_that("the EFA fit-index note gives reference values for orientation in the CFA tab's words, with no verdict", {
  src <- paste(deparse(mod_efa_server), collapse = " ")
  src <- gsub("\\s+", " ", gsub("\" *, *\"|\",\\s*\"", "", src))
  expect_false(grepl("become acceptable", src, fixed = TRUE))
  expect_true(grepl("Reference values are shown for orientation.", src, fixed = TRUE))
  expect_true(grepl("RMSEA interval together (Hu & Bentler, 1999;", src, fixed = TRUE))
})

# ---- a construct rename or facet edit leaves Dimensionality and EFA current ----

test_that("a construct rename or a facet edit leaves the Dimensionality and EFA results current, in the module and in the guide", {
  rename <- function(st) {
    m <- shiny::isolate(st$meta)
    m$construct[m$construct == "Visual"] <- "Spatial"
    m$facet[m$item == m$item[1]] <- "a facet"
    st$meta <- m
    shiny::isolate(sieve_confirm_types(st))
  }
  st <- ri_state()
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "auto", n_iter = 20, seed = 20261021, max_k = 3)
    v <- scree_vals()
    p <- pa_run()
    invisible(vss_run())
    session$flushReact()
    before <- shiny::isolate(sieve_page_status(st, "retention"))
    expect_identical(before$state, "current")
    rename(st)
    session$flushReact()
    expect_identical(scree_vals(), v)
    expect_identical(pa_run()$engine, p$engine)
    session$flushReact()
    after <- shiny::isolate(sieve_page_status(st, "retention"))
    expect_identical(after$state, "current")
    expect_true(all(vapply(after$results, function(r) r$state, character(1)) == "current"))
    expect_false(grepl("stale", ri_text(output$pa_code), ignore.case = TRUE))
  })
  st2 <- ri_state()
  shiny::testServer(mod_efa_server, args = list(state = st2), {
    session$setInputs(fm = "ml", rotation = "oblimin", cor_type = "auto", fit_scope = "k", n_factors = 3L)
    session$setInputs(efa_go = 1)
    r1 <- efa_run()
    session$flushReact()
    expect_identical(primary_status()$state, "current")
    rename(st2)
    session$flushReact()
    expect_identical(primary_status()$state, "current")
    r2 <- efa_run()
    expect_false(isTRUE(r2$pending))
    expect_identical(unclass(r2$fit$loadings), unclass(r1$fit$loadings))
    rec <- shiny::isolate(sieve_page_status(st2, "efa"))$results
    primary <- Filter(function(r) identical(r$title, "EFA loadings and factor correlations"), rec)[[1]]
    expect_identical(primary$state, "current")
    # An inclusion edit refits the requested EFA for its new indicator set.
    sieve_set_item_status(st2, shiny::isolate(st2$meta$item[1]), "parked", "probe")
    session$flushReact()
    expect_identical(primary_status()$state, "current")
    expect_equal(nrow(unclass(efa_run()$fit$loadings)), 8L)
  })
})

# ---- the model statement and the status line are visible -------------------

test_that("the Items tab states its model in a visible line and shows its status line outside any closed element", {
  html <- as.character(mod_winnow_ui("items"))
  first_details <- regexpr("<details", html, fixed = TRUE)
  for (id in c("items-model_line", "items-status_line", "items-overall_fit")) {
    at <- regexpr(sprintf("id=\"%s\"", id), html, fixed = TRUE)
    expect_gt(at, 0)
    expect_lt(at, first_details)
  }
  # (Results first: the explanation is below the grid, its
  # first paragraph visible outside any details element and the rest in
  # "More on how these diagnostics are computed", closed by default.)
  note_at <- regexpr("id=\"items-model_note\"", html, fixed = TRUE)
  expect_gt(note_at, regexpr("id=\"items-pool\"", html, fixed = TRUE))
  expect_lt(note_at, first_details)
  expect_match(substr(html, first_details, first_details + 160), "<details open>\\s*<summary>More on how these diagnostics are computed")

  s <- ri_state()
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    line <- ri_text(output$model_line)
    # (The engine is DMAR::fa_exploratory().)
    for (part in c("DMAR::fa_exploratory", "maximum likelihood", "direct oblimin rotation (delta = 0)",
      "k = 3", "Pearson", "9 included items", "n = 301")) {
      expect_match(line, part, fixed = TRUE)
    }
    session$setInputs(exclude_selected = 1)
    expect_match(ri_text(output$status_line), "Select one or more item rows first", fixed = TRUE)
  })
})

# ---- the Items tab's reliability: robust omega -----------------------------

test_that("the Items tab's reliability is robust omega with the largest residual pair, and categorical robust omega for ordered items", {
  skip_if_not_installed("DMAR")
  fx <- sieve_hs_fixture()
  X <- fx$data[, fx$item_cols, drop = FALSE]
  m <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  d <- sieve_item_diagnostics(X, m, rng = sieve_draw_seed(1))
  cons <- d$constructs
  expect_true(all(cons$omega_label == "Robust omega (observed denominator)"))
  cn <- cons$construct[which.max(cons$n_active)]
  its <- m$item[m$construct == cn]
  Xc <- X[stats::complete.cases(X), its, drop = FALSE]
  ref <- function(data) {
    r <- suppressMessages(DMAR::reliability_omega(data = data, ci_method = "none", denominator = "observed"))
    r$value[r$term == "estimate"]
  }
  expect_equal(cons$omega[cons$construct == cn], ref(Xc), tolerance = 1e-7)
  drop1 <- d$items$omega_drop_delta[d$items$item == its[1]]
  expect_equal(drop1, ref(Xc[, -1, drop = FALSE]) - ref(Xc), tolerance = 1e-7)
  # The largest single-factor residual correlation, from lavaan directly.
  dd <- Xc
  names(dd) <- paste0("v", seq_along(its))
  fit <- lavaan::cfa(paste("f =~", paste(names(dd), collapse = " + ")), data = dd, std.lv = TRUE)
  res <- unclass(lavaan::lavResiduals(fit, type = "cor")$cov)
  res[!upper.tri(res)] <- NA
  at <- which(abs(res) == max(abs(res), na.rm = TRUE), arr.ind = TRUE)[1, ]
  expect_equal(cons$residual_max[cons$construct == cn], unname(res[at[1], at[2]]), tolerance = 1e-8)
  expect_identical(cons$residual_pair[cons$construct == cn], paste(its[at[1]], "and", its[at[2]]))

  # Ordered items: categorical robust omega (unrestricted polychorics).
  ofx <- ri_ordered_fixture()
  om <- sieve_build_meta(ofx$data, ofx$item_cols, ofx$theory_map)
  om$type <- "ordered"
  od <- sieve_item_diagnostics(ofx$data, om, rng = sieve_draw_seed(1))
  args <- list(data = ofx$data[, 1:3], ci_method = "none")
  if ("denominator" %in% names(formals(DMAR::reliability_omega_categorical))) args$denominator <- "observed"
  cref <- suppressMessages(do.call(DMAR::reliability_omega_categorical, args))
  expect_equal(od$constructs$omega[od$constructs$construct == "A"], cref$value[cref$term == "estimate"], tolerance = 1e-10)
  expect_identical(od$constructs$treatment, c("ordered", "ordered"))
  expect_match(od$constructs$omega_matrix[1], "unrestricted polychoric correlations", fixed = TRUE)

  s <- ri_state(sieve_hs_fixture())
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    txt <- ri_text(output$rel_summary)
    expect_match(txt, "Robust omega (observed denominator) =", fixed = TRUE)
    expect_match(txt, "largest residual r =", fixed = TRUE)
    # (Results first: the explanation of the line is below the grid, in
    # rel_note.)
    expect_match(ri_text(output$rel_note), "signals redundancy, not better measurement", fixed = TRUE)
    expect_false(grepl("ω =", txt, fixed = TRUE))
  })
})

# ---- one run policy --------------------------------------------------------

test_that("the run policy is one rule: a first result at two seconds or less on the shown page, an unknown estimate on request, the Items diagnostics live within two seconds", {
  expect_true(sieve_runs_by_itself(1.9))
  expect_false(sieve_runs_by_itself(2.1))
  expect_false(sieve_runs_by_itself(NA_real_))
  expect_false(sieve_runs_by_itself(NULL))
  expect_false(sieve_runs_by_itself(0.5, first = FALSE))
  expect_false(sieve_runs_by_itself(0.5, visible = FALSE))
  expect_true(sieve_runs_by_itself(0.5, first = FALSE, live = TRUE))
  expect_false(sieve_runs_by_itself(2.5, live = TRUE))
  expect_false(sieve_pa_plan(list(used = "polychoric", p = 9, seconds = NA_real_), 100)$automatic)
  # Said once, in the About dialog and in the page guide.
  about <- paste(deparse(body(app_server)), collapse = " ")
  expect_match(about, "sieve_run_policy_text()", fixed = TRUE)
  guide <- ri_text(sieve_page_status_ui(list(page = "winnow", label = "Items", state = "ready",
    message = "", next_step = "", results = list()), NULL))
  expect_match(guide, "two seconds or less", fixed = TRUE)
})

test_that("after an exclusion the Items diagnostics follow by themselves while quick, and the overall model waits for Update", {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  counts <- new.env()
  counts$cfa <- 0L
  counts$diag <- 0L
  real_cfa <- sieve_run_cfa
  real_diag <- sieve_item_diagnostics
  testthat::local_mocked_bindings(
    sieve_run_cfa = function(...) {
      counts$cfa <- counts$cfa + 1L
      real_cfa(...)
    },
    sieve_item_diagnostics = function(...) {
      counts$diag <- counts$diag + 1L
      real_diag(...)
    }
  )
  s <- ri_state()
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    invisible(overall_fit())
    invisible(diag())
    expect_identical(c(counts$cfa, counts$diag), c(1L, 1L))
    sieve_set_item_status(s, "t2_cubes", "parked", "probe")
    session$flushReact()
    expect_false(is.null(diag())) # live: estimated well under two seconds
    expect_identical(counts$diag, 2L)
    r <- fit_run()
    expect_identical(r$status, "not run")
    expect_true(isTRUE(r$stale))
    expect_identical(counts$cfa, 1L)
    expect_match(ri_text(output$status_line), "Update (", fixed = TRUE)
    expect_identical(shiny::isolate(sieve_page_status(s, "winnow"))$state, "stale")
    session$setInputs(update_go = 1)
    expect_identical(counts$cfa, 2L)
    expect_false(is.null(overall_fit()))
  })
})

test_that("Items diagnostics estimated above two seconds wait for Update and show nothing from before", {
  testthat::local_mocked_bindings(sieve_diagnostics_wait = function(...) 30) # a slow pool
  s <- ri_state()
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    r <- diag_run()
    expect_identical(r$status, "not run")
    expect_null(diag())
    expect_gt(r$plan$seconds, 2)
    expect_match(ri_text(output$status_line), "not computed yet", fixed = TRUE)
    session$setInputs(update_go = 1)
    expect_false(is.null(diag()))
    # An exclusion: the earlier values are withheld, not shown as current.
    sieve_set_item_status(s, "t2_cubes", "parked", "probe")
    session$flushReact()
    expect_null(diag())
    expect_true(isTRUE(diag_run()$stale))
    expect_match(ri_text(output$status_line), "withheld until they are updated", fixed = TRUE)
  })
})

test_that("parallel analysis runs by itself only as a first result; after an exclusion the page's Update refits it, and a long refit shows the earlier one as from before", {
  # This is a request/update lifecycle fixture, not an interval-accuracy
  # check. Keep its entered count explicit and opt in for this test only.
  siever_local_dmar_mechanics(environment())
  # Only PA is meant to switch to the background in the second case.
  # Prior fits' measured rates must not put the unrelated fit-by-k job
  # ahead of the PA status line this test is inspecting.
  local_mocked_bindings(sieve_pa_units = function(...) 0.000001,
    sieve_efa_plan_wait = function(...) 0.01)
  # (An exclusion asks the page's Update to refit what it had computed.
  # DMAR's run of this size is estimated under two seconds, so the Update
  # computes it at once in the session. A long refit runs in a background
  # R process, and until it finishes the earlier result is from before.)
  s <- ri_state()
  shiny::testServer(mod_retention_server, args = list(state = s), {
    session$setInputs(cor_type = "auto", n_iter = 20, seed = 11, max_k = 3)
    p1 <- pa_run()
    expect_true(isTRUE(p1$ok))
    expect_length(p1$empirical, 9L)
    sieve_set_item_status(s, "t2_cubes", "parked", "probe")
    session$flushReact()
    p2 <- pa_run()
    expect_identical(p2$status, "ran")
    expect_length(p2$empirical, 8L)
  })
  s <- ri_state()
  log <- new.env(parent = emptyenv())
  test_frame <- environment()
  local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log, alive = TRUE))
  shiny::testServer(mod_retention_server, args = list(state = s), {
    session$setInputs(cor_type = "auto", n_iter = 20, seed = 11, max_k = 3)
    expect_true(isTRUE(pa_run()$ok))
    # From here on every run is estimated far above two seconds (the mock is
    # registered on the test's own frame, so it ends with the test).
    local_mocked_bindings(sieve_pa_units = function(...) 1e4, .env = test_frame)
    sieve_set_item_status(s, "t2_cubes", "parked", "probe")
    session$flushReact()
    expect_true(pa_running())
    p2 <- pa_run()
    expect_identical(p2$status, "not run")
    expect_true(isTRUE(p2$stale))
    expect_match(p2$reason, "from before", fixed = TRUE)
    expect_match(ri_text(output$status_line), "is running in a background R process", fixed = TRUE)
  })
})

# ---- the Items numbers reach the Reproducible Code, seeded -----------------

test_that("the Items diagnostics write R that reproduces their loadings and robust omega under the recorded seed", {
  skip_if_not_installed("DMAR")
  s <- ri_state(sieve_hs_fixture())
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    d <- diag()
    log <- shiny::isolate(s$codelog)
    at <- max(grep("Items: exploratory diagnostics", log, fixed = TRUE))
    end <- which(log == "")
    code <- log[seq.int(at + 1L, min(end[end > at]) - 1L)]
    expect_true(any(grepl(sprintf("withr::with_seed(%d,", d$rng$seed), code, fixed = TRUE)))
    env <- new.env()
    m <- shiny::isolate(s$meta)
    env$meta <- m
    env$active_items <- shiny::isolate(s$items)[, m$item[m$status == "active"], drop = FALSE]
    eval(parse(text = code), envir = env)
    # (The recorded R reads DMAR's table into items_loadings.)
    P <- env$items_loadings
    target <- vapply(seq_len(nrow(d$items)), function(i) {
      row <- P[d$items$item[i], ]
      row[which.min(abs(abs(row) - abs(d$items$loading_target[i])))]
    }, numeric(1))
    expect_equal(unname(target), d$items$loading_target, tolerance = 1e-8)
    om <- vapply(env$items_reliability, function(x) x$robust_omega, numeric(1))
    expect_equal(unname(om[d$constructs$construct]), d$constructs$omega, tolerance = 1e-8)
  })
})

# ---- the cross-loading rule compares magnitudes ----------------------------

test_that("an item with a negative target loading is flagged as cross loading only when the off-target loading rivals its magnitude", {
  # Two factors; item a4 is keyed against its factor (target about -0.8)
  # and has a salient but much smaller off-target loading: no cross loading.
  X <- withr::with_seed(113, {
    n <- 500
    f1 <- stats::rnorm(n)
    f2 <- 0.3 * f1 + sqrt(0.91) * stats::rnorm(n)
    e <- function(l) sqrt(1 - l^2) * stats::rnorm(n)
    data.frame(
      a1 = .75 * f1 + e(.75), a2 = .7 * f1 + e(.7), a3 = .7 * f1 + e(.7), a4 = -.75 * f1 + .5 * f2 + sqrt(1 - .5875) * stats::rnorm(n),
      b1 = .75 * f2 + e(.75), b2 = .7 * f2 + e(.7), b3 = .7 * f2 + e(.7), b4 = .65 * f2 + e(.65)
    )
  })
  m <- data.frame(item = names(X), label = names(X), construct = rep(c("A", "B"), each = 4),
    facet = NA_character_, type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE)
  d <- sieve_item_diagnostics(X, m, rng = sieve_draw_seed(1))
  a4 <- d$items[d$items$item == "a4", ]
  expect_lt(a4$loading_target, -0.5)
  expect_lt(a4$loading_off_max, abs(a4$loading_target) - 0.10)
  expect_gte(a4$loading_off_max, 0.30) # salient: a signed rule would flag it
  expect_false(a4$cross_loading)
  # A rule on the signed target would make any off-target loading of .30
  # or more a cross loading; the rule is the magnitude, so a salient one
  # that rivals |target| is still flagged.
  th <- sieve_default_thresholds()
  expect_identical(a4$cross_loading, isTRUE(a4$loading_off_max >= th$salient) &&
    isTRUE(a4$loading_off_max >= abs(a4$loading_target) - 0.10))
})

# ---- the max r header names its correlations -------------------------------

test_that("the largest-correlation column names the correlations it reads and the absolute value", {
  defs <- sieve_item_column_definitions()
  expect_false(grepl("Largest Pearson correlation", defs[["redundancy"]], fixed = TRUE))
  expect_match(defs[["redundancy"]], "Largest absolute correlation", fixed = TRUE)
  s <- ri_state()
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    invisible(diag())
    expect_match(paste(as.character(output$pool), collapse = " "), "max |r| (Pearson)", fixed = TRUE)
  })
})

# ---- the "auto" parallel-analysis reference follows the correlations used ----

test_that("with polychoric correlations of items typed continuous, the automatic reference is the resampled one", {
  s <- ri_state(ri_ordered_fixture())
  m <- shiny::isolate(s$meta)
  m$type <- "continuous"
  s$meta <- m
  s$estimator_choice <- "wlsmv" # the study treats every item as ordered: polychoric correlations
  shiny::testServer(mod_retention_server, args = list(state = s), {
    session$setInputs(cor_type = "auto", n_iter = 10, seed = 11, max_k = 2, pa_reference = "auto", criterion = "p95")
    expect_identical(basis()$used, "polychoric")
    expect_true(categorical())
    sel <- pa_sel()
    if (isTRUE(sel$ok)) {
      expect_identical(sel$family_used, "resampled")
      expect_null(sel$caution)
    }
  })
})

# ---- psych's partition labeled; no simple-structure verdict ----------------

test_that("the oblique variance rows are labeled as what they are and the simple-structure panel gives no verdict", {
  html <- as.character(mod_efa_ui("efa"))
  expect_false(grepl("ss_verdict", html, fixed = TRUE))
  src <- paste(deparse(mod_efa_server), collapse = " ")
  expect_false(grepl("Velicer's VSS", src, fixed = TRUE))
  expect_false(grepl("Close to simple structure", src, fixed = TRUE))
  s <- ri_state(sieve_hs_fixture())
  shiny::testServer(mod_efa_server, args = list(state = s), {
    session$setInputs(fm = "ml", rotation = "oblimin", cor_type = "auto", n_factors = 5L, estimator = "ml")
    session$setInputs(efa_go = 1)
    f <- efa()
    P <- unclass(f$loadings)
    S <- unclass(f$Structure)
    # DMAR's variance accounted (the diagonal of Phi Lambda' Lambda) is the
    # column sum of pattern times structure, as psych's oblique SS
    # loadings are; it is labeled as such, not as SPSS's rotation sums.
    va <- sieve_efa_vaccounted_table(f)
    expect_equal(unname(unlist(va[1L, colnames(P)])), unname(colSums(P * S)), tolerance = 1e-8)
    expect_identical(va$Statistic[1], "Variance accounted for (diagonal of Phi Lambda' Lambda; pattern x structure)")
    expect_no_error(output$vacc_tbl)
    expect_match(ri_text(output$vacc_note), "not SPSS's rotation sums of squared loadings", fixed = TRUE)
    expect_match(ri_text(output$ss_note), "Very Simple Structure (Revelle &amp; Rocklin, 1979)", fixed = TRUE)
  })
})

# ---- the Items overall model applies the declared sampling design ----------

test_that("the Items overall model is fit under a declared clustered design, refuses a repeated one, and says independent rows otherwise", {
  s <- ri_state()
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    session$setInputs(coef = "omega", factor_cov = "correlated", estimator = "study")
    f0 <- overall_fit()
    expect_false(is.null(f0))
    expect_match(ri_text(output$overall_fit), "rows treated as independent", fixed = TRUE)
    raw <- shiny::isolate(s$raw)
    raw$classroom <- rep(seq_len(60), length.out = nrow(raw))
    s$raw <- raw
    sieve_confirm_types(s)
    s$sampling_design <- list(unit = "clustered", cluster = "classroom")
    session$flushReact()
    expect_identical(fit_run()$status, "not run") # a new model: it waits for Update
    session$setInputs(update_go = 1)
    f1 <- overall_fit()
    expect_false(is.null(f1))
    expect_identical(lavaan::lavInspect(f1, "options")$estimator, "ML")
    expect_true(nzchar(paste(lavaan::lavInspect(f1, "cluster"), collapse = "")))
    expect_match(ri_text(output$overall_fit), "cluster-robust standard errors and test over the clusters in classroom", fixed = TRUE)
    # Two schools are too few clusters: said, with no fit indices.
    s$sampling_design <- list(unit = "clustered", cluster = "school")
    session$flushReact()
    session$setInputs(update_go = 2)
    expect_true(is.character(fit_run()$error))
    s$sampling_design <- list(unit = "repeated")
    session$flushReact()
    session$setInputs(update_go = 3)
    expect_match(fit_run()$error, "Repeated observations require", fixed = TRUE)
  })
})

# ---- a recorded retention decision proposes the EFA tab's number of factors ----

test_that("a decision recorded on the Dimensionality tab sets the EFA tab's number of factors as a proposal", {
  s <- ri_state()
  shiny::testServer(mod_efa_server, args = list(state = s), {
    session$setInputs(fm = "ml", rotation = "oblimin", cor_type = "auto", fit_scope = "k", n_factors = 3L)
    expect_null(output$decision_note)
    shiny::isolate(sieve_record_decision(s, "factor_retention", question = "How many factors?", choice = "2", rationale = "Two, as the evidence suggests."))
    session$flushReact()
    expect_match(ri_text(output$decision_note), "Proposed by the decision recorded on the Dimensionality tab", fixed = TRUE)
    expect_match(ri_text(output$decision_note), "2 factors", fixed = TRUE)
    expect_identical(decision_seen(), retention_decision()$id)
  })
})

# ---- exploratory fits skip the unused matrices -----------------------------

test_that("the identification and covariance checks set an exploratory fit aside before forming its matrices, with the same results", {
  hs <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  fl <- suppressWarnings(lavaan::efa(data = hs, nfactors = 2, rotation = "none", estimator = "ML"))
  if (inherits(fl, "efaList")) fl <- fl[[1]]
  asked <- character(0)
  real <- lavaan::lavInspect
  testthat::local_mocked_bindings(
    lavInspect = function(object, what = "free", ...) {
      asked <<- c(asked, what)
      real(object, what, ...)
    },
    .package = "lavaan"
  )
  expect_identical(sieve_information_check(fl), list(singular = NA, min_eigen = NA_real_))
  expect_identical(sieve_vcov_singular(fl), NA)
  expect_false(any(c("information.expected", "vcov") %in% asked))
  # A confirmatory model is still checked.
  cf <- lavaan::cfa("f1 =~ x1 + x2 + x3\nf2 =~ x4 + x5 + x6", data = hs, std.lv = TRUE)
  expect_false(sieve_information_check(cf)$singular)
  expect_false(sieve_vcov_singular(cf))
})

# The shared correlation basis. Every tab that works
# from the inter-item correlations of the included items reads one matrix,
# computed once per item pool and kept in the state under the identity of
# its inputs. The checks below count the engine calls, compare the kept
# matrix with psych's own, and drive the three consuming modules through
# shiny::testServer.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("psych")
skip_if_not_installed("shiny")
withr::local_options(sieve.cores = 2L)

# Two correlated factors, four items each, cut into five ordered categories.
basis_ordinal <- function(n = 300L) {
  withr::with_seed(20261014, {
    f1 <- stats::rnorm(n)
    f2 <- 0.3 * f1 + sqrt(1 - 0.09) * stats::rnorm(n)
    it <- function(f, l) l * f + sqrt(1 - l^2) * stats::rnorm(n)
    X <- data.frame(
      a1 = it(f1, .80), a2 = it(f1, .75), a3 = it(f1, .70),
      b1 = it(f2, .80), b2 = it(f2, .75), b3 = it(f2, .70)
    )
    as.data.frame(lapply(X, function(v) as.integer(cut(v, c(-Inf, -1, -0.2, 0.5, 1.3, Inf)))))
  })
}

# A loaded, confirmed state around a data frame of items.
basis_state <- function(d, types = "ordered", map = NULL) {
  map <- map %||% list(A = names(d)[1:3], B = names(d)[4:6])
  fixture <- list(data = d, item_cols = names(d), theory_map = map,
    name = "basis test", theory_k = length(map), theory_lbl = "basis test",
    labels = NULL, load_code = character())
  s <- siever_test_study(fixture, labels = FALSE, theory = FALSE, configure = function(s) {
    m <- shiny::isolate(s$meta)
    m$type <- types
    m$facet <- rep(c("f1", "f2"), length.out = nrow(m))
    s$meta <- m
    s$estimator <- if (all(types == "continuous")) "MLR" else "WLSMV (DWLS)"
  })
  s
}

basis_text <- function(x) paste(as.character(unlist(x)), collapse = " ")

test_that("the included items are read once, as numbers, on the complete cases", {
  d <- basis_ordinal()
  d$a2[c(3, 40)] <- NA
  st <- basis_state(d)
  m <- shiny::isolate(st$meta)
  m$status[m$item == "b3"] <- "parked"
  st$meta <- m
  pool <- shiny::isolate(sieve_state_items(st))
  expect_identical(pool$items, c("a1", "a2", "a3", "b1", "b2"))
  expect_identical(names(pool$X), pool$items)
  expect_identical(pool$n, 298L)
  expect_identical(pool$n_rows, 300L)
  expect_identical(unname(pool$types), rep("ordered", 5))
  expect_true(all(vapply(pool$X, is.numeric, logical(1))))
  expect_identical(pool$key, sieve_data_key(pool$raw))
  # A column with a class whose numeric method is not available is read
  # through unclass(), as its codes.
  st2 <- basis_state(d)
  it <- shiny::isolate(st2$items)
  it$a1 <- structure(as.numeric(it$a1), class = c("sieve_test_labelled", "numeric"))
  st2$items <- it
  registerS3method("as.double", "sieve_test_labelled",
    function(x, ...) stop("no numeric method in this session"),
    envir = globalenv()
  )
  withr::defer(registerS3method("as.double", "sieve_test_labelled",
    function(x, ...) as.double(unclass(x)),
    envir = globalenv()
  ))
  expect_error(as.numeric(it$a1), "no numeric method")
  pool2 <- shiny::isolate(sieve_state_items(st2))
  expect_identical(pool2$X$a1, as.numeric(unclass(it$a1))[stats::complete.cases(d)])
  expect_null(shiny::isolate(sieve_state_items(sieve_new_state())))
})

test_that("the basis is computed once per identity, shared by every call, and emptied with the data", {
  d <- basis_ordinal()
  st <- basis_state(d)
  seen <- new.env()
  seen$cor <- 0L
  real_cor <- sieve_retention_cor
  local_mocked_bindings(sieve_retention_cor = function(...) {
    seen$cor <- seen$cor + 1L
    real_cor(...)
  })

  b1 <- shiny::isolate(sieve_state_basis(st))
  expect_identical(seen$cor, 1L)
  expect_false(b1$cached)
  expect_identical(b1$requested, "polychoric")
  expect_identical(b1$used, "polychoric")
  expect_true(b1$ok)
  direct <- withr::with_options(list(mc.cores = 1L), suppressWarnings(suppressMessages(
    psych::polychoric(d, correct = 0.5, progress = FALSE)$rho
  )))
  expect_equal(unname(b1$R), unname(direct), tolerance = 1e-12)
  expect_identical(b1$n_rows, 300L)
  expect_true(is.finite(b1$seconds) && b1$seconds > 0)
  # Kept in the state as plain data, under its key, one entry per request.
  held <- shiny::isolate(st$cache)$basis$polychoric
  expect_identical(held$key, b1$key)
  expect_length(SIEVER:::sieve_not_plain(shiny::isolate(st$cache)), 0L)
  # The R that computed it was written once.
  log <- shiny::isolate(st$codelog)
  expect_identical(
    sum(grepl("Correlation basis of the included items (requested polychoric, used polychoric)",
      log,
      fixed = TRUE
    )),
    1L
  )
  expect_true(any(grepl("psych::polychoric(", log, fixed = TRUE)))

  # The same identity: the kept matrix, no engine call, nothing written.
  b2 <- shiny::isolate(sieve_state_basis(st))
  expect_identical(seen$cor, 1L)
  expect_true(b2$cached)
  expect_identical(b2$R, b1$R)
  expect_identical(shiny::isolate(st$codelog), log)
  expect_identical(shiny::isolate(sieve_state_basis(st, "auto"))$cached, TRUE)
  expect_identical(shiny::isolate(sieve_state_basis(st, "polychoric"))$cached, TRUE)

  # Edits that leave the matrix what it is: a label, a facet, a construct.
  m <- shiny::isolate(st$meta)
  m$label[1] <- "A new display label"
  st$meta <- m
  expect_true(shiny::isolate(sieve_state_basis(st))$cached)
  m$facet[m$facet == "f1"] <- "first facet"
  st$meta <- m
  expect_true(shiny::isolate(sieve_state_basis(st))$cached)
  m$construct[2] <- "B"
  st$meta <- m
  expect_true(shiny::isolate(sieve_state_basis(st))$cached)
  expect_identical(seen$cor, 1L)

  # Another request is another matrix, kept beside the first (one entry per
  # request of the current pool), so asking for the first again costs nothing.
  p <- shiny::isolate(sieve_state_basis(st, "pearson"))
  expect_identical(seen$cor, 2L)
  expect_false(p$cached)
  expect_identical(p$used, "pearson")
  expect_equal(unname(p$R), unname(stats::cor(d)), tolerance = 1e-14)
  expect_true(shiny::isolate(sieve_state_basis(st))$cached)
  expect_true(shiny::isolate(sieve_state_basis(st, "pearson"))$cached)
  expect_identical(seen$cor, 2L)
  expect_setequal(names(shiny::isolate(st$cache)$basis), c("polychoric", "pearson"))

  # Edits that change the matrix: a measurement level, an exclusion, a
  # reversed item (the values change). Each is a new pool, and only the
  # bases of the current pool are kept.
  m$type[1] <- "continuous"
  st$meta <- m
  mixed <- shiny::isolate(sieve_state_basis(st))
  expect_identical(seen$cor, 3L)
  expect_identical(mixed$requested, "mixed")
  expect_identical(names(shiny::isolate(st$cache)$basis), "mixed")
  m$type[1] <- "ordered"
  m$status[6] <- "parked"
  st$meta <- m
  five <- shiny::isolate(sieve_state_basis(st))
  expect_identical(seen$cor, 4L)
  expect_identical(five$p, 5L)
  expect_identical(five$items, names(d)[1:5])
  it <- shiny::isolate(st$items)
  it$a1 <- 6 - it$a1
  st$items <- it
  shiny::isolate(sieve_state_basis(st))
  expect_identical(seen$cor, 5L)
  expect_identical(names(shiny::isolate(st$cache)$basis), "polychoric")

  # A new dataset empties the cache.
  shiny::isolate(sieve_new_dataset(st))
  expect_identical(shiny::isolate(st$cache), list())
  shiny::isolate(sieve_state_basis(st))
  expect_identical(seen$cor, 6L)
  expect_identical(shiny::isolate(sieve_state_basis(st))$cached, TRUE)
})

test_that("the bases of two requests are kept side by side, so a tab on Pearson and a tab on the automatic basis do not recompute each other's matrix", {
  d <- basis_ordinal()
  st <- basis_state(d)
  seen <- new.env()
  seen$calls <- character(0)
  real_cor <- sieve_retention_cor
  local_mocked_bindings(
    sieve_retention_cor = function(X, types = NULL, requested = "pearson", ...) {
      seen$calls <- c(seen$calls, requested)
      real_cor(X, types, requested, ...)
    },
    withProgress = function(expr, ...) expr
  )
  # The Dimensionality tab with Pearson correlations chosen, and the EFA tab
  # on the automatic basis, as the researcher moves between them. Each visit
  # is a session of its own here; in the application a label edit
  # invalidates the Dimensionality tab's basis() as well, since it reads the
  # ledger, so the sequence is the application's.
  dim_pearson <- function() {
    got <- new.env()
    shiny::testServer(mod_retention_server, args = list(state = st), {
      session$setInputs(
        cor_type = "pearson", criterion = "p95", pa_reference = "auto",
        n_iter = 10, max_k = 3, seed = 2
      )
      got$used <- basis()$used
    })
    got$used
  }
  # The EFA tab reads the shared basis on its
  # matrix route (DMAR's minres on the supplied matrix); its other routes
  # fit the item responses and compute no matrix.
  efa_auto <- function() {
    got <- new.env()
    shiny::testServer(mod_efa_server, args = list(state = st), {
      session$setInputs(n_factors = 2, rotation = "oblimin", fm = "minres_matrix", suppress = 0.3, sort_loadings = TRUE)
      session$setInputs(efa_go = 1L)
      got$used <- efa_run()$basis$used
    })
    got$used
  }
  expect_identical(dim_pearson(), "pearson")
  expect_identical(efa_auto(), "polychoric")
  expect_identical(seen$calls, c("pearson", "polychoric"))
  # Three label edits: both tabs are evaluated again and neither computes a
  # matrix (a cache of one matrix would cost the EFA tab a polychoric
  # matrix again at every edit).
  for (i in 1:3) {
    m <- shiny::isolate(st$meta)
    m$label[1] <- paste("label", i)
    st$meta <- m
    expect_identical(dim_pearson(), "pearson")
    expect_identical(efa_auto(), "polychoric")
  }
  expect_identical(seen$calls, c("pearson", "polychoric"))
  expect_setequal(names(shiny::isolate(st$cache)$basis), c("pearson", "polychoric"))
  # A new item pool: only the bases of the current pool are kept.
  m <- shiny::isolate(st$meta)
  m$status[6] <- "parked"
  st$meta <- m
  expect_identical(efa_auto(), "polychoric")
  expect_identical(names(shiny::isolate(st$cache)$basis), "polychoric")
  expect_identical(seen$calls, c("pearson", "polychoric", "polychoric"))
})

test_that("inside a session the computation announces itself with the estimated wait, outside it is silent", {
  d <- basis_ordinal()
  seen <- new.env()
  seen$said <- character(0)
  local_mocked_bindings(withProgress = function(expr, ..., message = NULL, detail = NULL) {
    seen$said <- c(seen$said, paste(message, detail))
    expr
  })
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  st <- basis_state(d)
  srv <- function(id, state) {
    shiny::moduleServer(id, function(input, output, session) {
      b <- shiny::reactive(sieve_state_basis(state, input$cor %||% "auto"))
      b
    })
  }
  shiny::testServer(srv, args = list(state = st), {
    session$setInputs(cor = "auto")
    expect_true(b()$ok)
    expect_length(seen$said, 1L)
    expect_match(seen$said, "^Computing the polychoric correlations of 6 items \\(under 2 seconds\\)")
    expect_match(seen$said, "computed once per item pool", fixed = TRUE)
    # The kept matrix announces nothing.
    session$setInputs(cor = "poly")
    expect_true(b()$cached)
    expect_length(seen$said, 1L)
    # Pearson correlations are immediate and announce nothing.
    session$setInputs(cor = "pearson")
    expect_identical(b()$used, "pearson")
    expect_length(seen$said, 1L)
  })
  # Outside a session: silent.
  shiny::isolate(sieve_state_basis(basis_state(d)))
  expect_length(seen$said, 1L)
})

test_that("the wait estimates come from the measured rates, the defaults before any", {
  local_measured_rates()
  sieve_rate_reset()
  withr::defer(sieve_rate_reset())
  expect_equal(sieve_pairs(68), 2278)
  expect_equal(sieve_basis_wait(68, "polychoric"), 21, tolerance = 1e-12)
  expect_identical(sieve_basis_wait(68, "pearson"), 0)
  expect_equal(sieve_fit_wait(68, "WLSMV"), 41.4, tolerance = 1e-12)
  expect_equal(sieve_fit_wait(24, "MLR"), 0.74, tolerance = 1e-12)
  expect_equal(sieve_fit_wait(68, "WLSMV", "efa", k = 8L), 8 * 124, tolerance = 1e-12)
  # The Items diagnostics of one construct of 68 continuous items: the factor
  # analysis and 70 robust omega fits. The factor analysis is
  # DMAR::fa_exploratory(), estimated by its own measured constants,
  # sieve_efa_point_wait().
  expect_equal(sieve_diagnostics_wait(68), sieve_efa_point_base(68, 1) + 0.05 + 70 * (0.034 + 8.4e-5 * 2278),
    tolerance = 1e-12)
  expect_equal(sieve_diagnostics_wait(68, efa = FALSE), 0.05 + 70 * (0.034 + 8.4e-5 * 2278), tolerance = 1e-12)
  expect_true(is.na(sieve_rate("nothing")))
  # Only the initial large-pool estimate carries the conservative allowance.
  # Once measured, fit forecasts equal observed seconds per unit exactly.
  sieve_rate_note("cfa_WLSMV", 20.7, sieve_fit_units(68, "WLSMV"))
  expect_equal(sieve_fit_wait(68, "WLSMV"), 20.7, tolerance = 1e-12)
  sieve_rate_note("cfa_MLR", .37, sieve_fit_units(24, "MLR"))
  expect_equal(sieve_fit_wait(24, "MLR"), .37, tolerance = 1e-12)
  sieve_rate_note("efa_WLSMV", 62, sieve_fit_units(68, "WLSMV"))
  expect_equal(sieve_fit_wait(68, "WLSMV", "efa", k = 8L), 8 * 62, tolerance = 1e-12)
  # A measurement replaces the default; two measurements pool their work.
  sieve_rate_note("polychoric", 10, sieve_pairs(68))
  expect_equal(sieve_basis_wait(68, "polychoric"), 10, tolerance = 1e-12)
  sieve_rate_note("polychoric", 30, sieve_pairs(68))
  expect_equal(sieve_basis_wait(68, "polychoric"), 20, tolerance = 1e-12)
  # Nothing is recorded from a time that is not a time.
  sieve_rate_note("polychoric", NA_real_, 10)
  sieve_rate_note("polychoric", 5, 0)
  expect_equal(sieve_basis_wait(68, "polychoric"), 20, tolerance = 1e-12)
  sieve_rate_reset()
  expect_equal(sieve_basis_wait(68, "polychoric"), 21, tolerance = 1e-12)
  # The basis records the time it took.
  st <- basis_state(basis_ordinal())
  b <- shiny::isolate(sieve_state_basis(st))
  expect_equal(sieve_rate("polychoric"), b$seconds / sieve_pairs(6), tolerance = 1e-12)
  # Workers: an explicit setting governs, up to 64 whatever caps the
  # default; the default is the cores less one, at most the cap, one on
  # Windows.
  withr::with_options(list(sieve.cores = 3L), expect_identical(sieve_cores(), 3L))
  withr::with_options(list(sieve.cores = 50L), expect_identical(sieve_cores(max = 8L), 50L))
  withr::with_options(list(sieve.cores = 100L), expect_identical(sieve_cores(), 64L))
  withr::with_options(list(sieve.cores = NULL), {
    n <- sieve_cores()
    expect_true(n >= 1L && n <= 8L)
    expect_identical(sieve_cores(max = 1L), 1L)
    withr::with_envvar(c(SIEVER_CORES = "2"), expect_identical(sieve_cores(), 2L))
    withr::with_envvar(c(SIEVER_CORES = "nonsense"), expect_true(sieve_cores() >= 1L))
  })
})

test_that("the Dimensionality, EFA and Items tabs work from the one kept matrix", {
  skip_if_not_installed("DMAR")
  d <- basis_ordinal()
  st <- basis_state(d)
  seen <- new.env()
  seen$cor <- 0L
  real_cor <- sieve_retention_cor
  local_mocked_bindings(
    sieve_retention_cor = function(...) {
      seen$cor <- seen$cor + 1L
      real_cor(...)
    },
    withProgress = function(expr, ...) expr
  )
  got <- new.env()
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(
      cor_type = "auto", criterion = "p95", pa_reference = "auto",
      n_iter = 10, max_k = 3, seed = 2
    )
    got$R <- basis()$R
    expect_identical(basis()$used, "polychoric")
    expect_match(basis_text(output$overview), "What was computed: polychoric correlations", fixed = TRUE)
  })
  expect_identical(seen$cor, 1L)
  held <- shiny::isolate(st$cache)$basis$polychoric$value
  expect_identical(held$R, got$R)

  # The EFA tab's matrix route: DMAR's minres on the kept polychoric matrix.
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(n_factors = 2, rotation = "oblimin", fm = "minres_matrix", suppress = 0.3, sort_loadings = TRUE)
    session$setInputs(efa_go = 1L)
    f <- efa()
    expect_false(is.null(f))
    direct <- suppressWarnings(suppressMessages(
      psych::fa(r = got$R, nfactors = 2, fm = "minres", rotate = "oblimin", n.obs = 300)
    ))
    # Minres on the same matrix, DMAR's against psych's: DMAR and psych
    # rotate from different starts and may order the factors differently,
    # so the communalities (rotation free) and the
    # sorted absolute loadings of each item are compared, within the
    # rotation's convergence tolerance.
    # On this flat objective psych's optimizer stops about 1e-5 from the
    # communalities DMAR reaches, and DMAR's solution has the smaller minres
    # objective (the sum of squared off-diagonal residuals), so the
    # difference is psych's convergence, measured here rather than assumed.
    objective <- function(L, Phi) {
      E <- got$R - L %*% Phi %*% t(L)
      sum(E[lower.tri(E)]^2)
    }
    expect_lte(objective(f$loadings, f$Phi), objective(unclass(direct$loadings), direct$Phi) + 1e-12)
    expect_equal(unname(f$communality), unname(direct$communality), tolerance = 1e-4)
    sorted <- function(P) t(apply(abs(unclass(P)), 1L, sort))
    expect_equal(sorted(f$loadings), sorted(direct$loadings), tolerance = 1e-4)
    expect_match(basis_text(output$efa_overview), "Minres is fit to the <b>polychoric</b> correlation matrix", fixed = TRUE)
  })
  expect_identical(seen$cor, 1L)
  log <- shiny::isolate(st$codelog)
  expect_true(any(grepl("  DMAR::fa_exploratory(", log, fixed = TRUE)))
  expect_true(any(grepl("S = R_items,", log, fixed = TRUE)))
  expect_true(any(grepl("N = n_complete,", log, fixed = TRUE)))
  expect_true(any(grepl('estimator = "ULS",', log, fixed = TRUE)))
  expect_identical(sum(grepl("Correlation basis of the included items", log, fixed = TRUE)), 1L)

  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(
      coef = "omega", factor_cov = "correlated",
      load_low = .4, salient = .3, communality_low = .3,
      item_total_low = .3, redundancy_high = .8
    )
    session$elapse(400)
    dg <- diag()
    expect_identical(dg$basis_used, "polychoric")
    expect_match(basis_text(output$fit_note), "fit to the polychoric correlation matrix", fixed = TRUE)
    # The loadings are those of psych::fa on the shared matrix, not on the
    # Pearson matrix.
    direct <- suppressWarnings(suppressMessages(
      psych::fa(got$R, nfactors = 2, n.obs = 300, fm = "ml", rotate = "oblimin")
    ))
    P <- unclass(direct$loadings)
    a1 <- dg$items[dg$items$item == "a1", ]
    expect_equal(a1$loading_target, max(abs(P["a1", ])), tolerance = 1e-5)
    got$pearson <- sieve_item_diagnostics(d, shiny::isolate(st$meta))
    expect_identical(got$pearson$basis_used, "pearson")
    expect_false(isTRUE(all.equal(got$pearson$items$loading_target, dg$items$loading_target,
      tolerance = 1e-3
    )))
  })
  expect_identical(seen$cor, 1L)

  # A basis for other items is not read: the diagnostics say so and fall back.
  other <- held
  other$R <- held$R[1:4, 1:4]
  fallen <- sieve_item_diagnostics(d, shiny::isolate(st$meta), basis = other)
  expect_identical(fallen$basis_used, "pearson")
  expect_match(fallen$notes, "does not cover these items", fixed = TRUE)
  expect_equal(fallen$items$loading_target, got$pearson$items$loading_target, tolerance = 1e-5)
})

test_that("a project with a cached basis saves as plain data and opens with an empty cache", {
  d <- basis_ordinal()
  st <- basis_state(d)
  shiny::isolate(sieve_state_basis(st))
  expect_false(is.null(shiny::isolate(st$cache)$basis$polychoric))
  snap <- SIEVER:::sieve_project_snapshot(st)
  expect_identical(snap$dropped, character(0))
  fresh <- sieve_new_state()
  SIEVER:::sieve_project_restore(fresh, snap, source = "test")
  expect_identical(shiny::isolate(fresh$cache), list())
  expect_identical(shiny::isolate(fresh$meta$item), names(d))
})

test_that("a project's labelled columns are readable in a session that has not loaded haven", {
  d <- basis_ordinal()
  it <- d
  it$a1 <- structure(as.numeric(d$a1),
    class = c("haven_labelled", "vctrs_vctr", "double"),
    labels = c("Strongly disagree" = 1)
  )
  snap <- list(format = "SIEVER project", raw = it, items = it)
  # Without loading haven: the codes.
  out <- sieve_labelled_ready(snap, load = FALSE)
  expect_identical(out$items$a1, as.numeric(d$a1))
  expect_identical(out$raw$a1, as.numeric(d$a1))
  expect_false(inherits(out$items$a1, "haven_labelled"))
  expect_identical(out$items$a2, d$a2)
  # Nothing to do without labelled columns.
  plain <- list(format = "SIEVER project", raw = d, items = d)
  expect_identical(sieve_labelled_ready(plain), plain)
  # With haven available, the columns are left for haven's methods.
  skip_if_not_installed("haven")
  kept <- sieve_labelled_ready(snap)
  expect_true(inherits(kept$items$a1, "haven_labelled"))
  expect_true(isNamespaceLoaded("haven"))
  expect_identical(as.numeric(kept$items$a1), as.numeric(d$a1))
})

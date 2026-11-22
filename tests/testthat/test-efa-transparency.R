# Result disclosures reproduce the fitted numbers without starting a calculation.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

efa_transparency_state <- function() {
  d <- withr::with_seed(20261020, {
    n <- 240L
    phi <- matrix(.35, 4L, 4L)
    diag(phi) <- 1
    factors <- matrix(stats::rnorm(n * 4L), n, 4L) %*% chol(phi)
    x <- vapply(seq_len(16L), function(j) {
      .7 * factors[, ceiling(j / 4)] + sqrt(1 - .7^2) * stats::rnorm(n)
    }, numeric(n))
    as.data.frame(x)
  })
  names(d) <- sprintf("item_%03d", seq_len(ncol(d)))
  d[1L, 16L] <- NA_real_
  groups <- split(names(d), rep(c("TRUST", "SECOND", "THIRD", "FOURTH"), each = 4L))
  fixture <- list(data = d, item_cols = names(d), theory_map = groups,
    name = "Transparency fixture", labels = NULL, load_code = character())
  s <- siever_test_study(fixture, labels = FALSE, theory = FALSE, configure = function(s) {
    m <- s$meta
    m$type <- "continuous"
    s$meta <- m
    s$roles <- NULL
    s$theory <- list(k = 4L, map = groups)
    s$estimator_choice <- "ml"
  })
  s
}

efa_transparency_text <- function(x) {
  gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(as.character(unlist(x)), collapse = " ")))
}

test_that("four-factor disclosure replays exact loading matrices on its frozen input view", {
  skip_if_not_installed("psych")
  skip_if_not_installed("lavaan")
  s <- efa_transparency_state()
  calls <- new.env(parent = emptyenv())
  calls$fa <- 0L
  original_fa <- DMAR::fa_exploratory
  local_mocked_bindings(fa_exploratory = function(...) {
    calls$fa <- calls$fa + 1L
    original_fa(...)
  }, .package = "DMAR")
  withr::local_options(sieve.auto_run = "never")
  shiny::testServer(mod_efa_server, args = list(state = s), {
    before <- shiny::isolate(state$codelog)
    output$efa_code
    output$count_code
    output$ss_code
    expect_identical(calls$fa, 0L)
    expect_identical(shiny::isolate(state$codelog), before)
    session$setInputs(
      efa_go = 1L, n_factors = 4L, cor_type = "pearson", fm = "ml",
      rotation = "oblimin", suppress = .3, sort_loadings = TRUE
    )
    run <- efa_run()
    expect_false(is.null(run$fit))
    expect_identical(run$rotation, "oblimin")
    expect_identical(run$fm, "ml")
    # The study's ML on the complete cases: DMAR's listwise deletion, the
    # one incomplete row out.
    expect_identical(run$route$estimator, "ML")
    expect_identical(run$route$missing, "listwise")
    expect_identical(run$n, 239L)
    expect_length(run$fit$items, 16L)
    expect_false(is.null(run$code_view))
    expect_identical(run$code_view$versions[["DMAR"]], as.character(utils::packageVersion("DMAR")))
    code <- paste(run$code_view$code, collapse = "\n")
    expect_match(code, sprintf("efa_fit <- withr::with_seed(%d, .rng_kind = \"Mersenne-Twister\"", run$rng$seed), fixed = TRUE)
    expect_match(code, "DMAR::fa_exploratory(", fixed = TRUE)
    expect_match(code, "nfactors = 4,", fixed = TRUE)
    expect_match(code, 'estimator = "ML",', fixed = TRUE)
    expect_match(code, 'missing = "listwise",', fixed = TRUE)
    expect_match(code, 'rotation = "oblimin",', fixed = TRUE)
    expect_match(code, sprintf("seed = %d", run$rng$seed), fixed = TRUE)
    expect_false(grepl("psych::fa", code, fixed = TRUE))
    expect_match(paste(run$code_view$context, collapse = " "), "Analyzed: 16 items; 239 complete cases.", fixed = TRUE)

    # The expanded revision is the exact retained/reversed input for this result.
    frozen <- sieve_replay_expand(
      shiny::isolate(state$replay_states[[run$code_view$input_key]]),
      shiny::isolate(state$replay_data)
    )
    env <- new.env(parent = baseenv())
    env$active_items <- frozen$items[, frozen$meta$item[frozen$meta$status == "active"], drop = FALSE]
    eval(parse(text = code), envir = env)
    expect_equal(env$pattern, unclass(run$fit$loadings), tolerance = 1e-12)
    expect_equal(env$Phi, run$fit$Phi, tolerance = 1e-12)
    expect_equal(env$structure_matrix, unclass(run$fit$Structure), tolerance = 1e-12)
    pattern <- unclass(run$fit$loadings)
    expect_equal(unclass(run$fit$Structure), pattern %*% run$fit$Phi, tolerance = 1e-12,
      ignore_attr = TRUE)
    expect_equal(as.numeric(run$fit$communality), unname(diag(pattern %*% run$fit$Phi %*% t(pattern))),
      tolerance = 1e-8)

    # An independent ML engine estimates the same solution when the matrix,
    # rotation and scaling match (psych::fa on the complete cases' Pearson
    # correlations). Factor labels and signs are arbitrary.
    X <- env$active_items[stats::complete.cases(env$active_items), , drop = FALSE]
    independent <- withr::with_seed(42L, psych::fa(stats::cor(X), nfactors = 4L, n.obs = nrow(X), fm = "ml", rotate = "oblimin"))
    other_all <- unclass(independent$loadings)
    permutations <- as.matrix(expand.grid(rep(list(seq_len(4L)), 4L)))
    permutations <- permutations[apply(permutations, 1L, function(x) !anyDuplicated(x)), , drop = FALSE]
    scores <- apply(permutations, 1L, function(ix) {
      other <- other_all[, ix, drop = FALSE]
      sum(abs(colSums(pattern * other)) / sqrt(colSums(pattern^2) * colSums(other^2)))
    })
    ix <- permutations[which.max(scores), ]
    signs <- sign(colSums(pattern * other_all[, ix, drop = FALSE]))
    aligned_pattern <- sweep(other_all[, ix, drop = FALSE], 2L, signs, "*")
    aligned_phi <- independent$Phi[ix, ix, drop = FALSE] * outer(signs, signs)
    expect_lt(max(abs(pattern - aligned_pattern)), 1e-3)
    expect_lt(max(abs(run$fit$Phi - aligned_phi)), 1e-3)
    expect_lt(max(abs(pattern %*% run$fit$Phi %*% t(pattern) -
      aligned_pattern %*% aligned_phi %*% t(aligned_pattern))), 1e-4)

    # A display-only construct filter cannot silently turn this into a TRUST-only EFA.
    calls_before <- calls$fa
    session$setInputs(display_construct = "TRUST", display_facet = "")
    filtered <- efa_run()
    expect_identical(filtered, run)
    expect_identical(nrow(pattern_mat()), 16L)
    expect_match(efa_transparency_text(output$display_pool_note), "full fitted item pool (16 items)", fixed = TRUE)
    expect_identical(calls$fa, calls_before)

    # Reading a disclosure has no additional scientific work or decision-log entry.
    output$ss_code
    steps <- shiny::isolate(state$codelog)
    calls_before <- calls$fa
    output$efa_code
    output$count_code
    output$ss_code
    output$efa_code
    expect_identical(calls$fa, calls_before)
    expect_identical(shiny::isolate(state$codelog), steps)
    expect_match(efa_transparency_text(output$efa_code), "fit_measures attribute", fixed = TRUE)
    expect_match(efa_transparency_text(output$count_code), "Run this analysis", fixed = TRUE)
  })
})

test_that("the completed number of factors disclosure replays its DMAR::factor_count call", {
  skip_if_not_installed("lavaan")
  fx <- sieve_hs9_fixture()
  s <- sieve_new_state()
  s$raw <- fx$data
  s$items <- fx$data[, fx$item_cols, drop = FALSE]
  s$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  s$confirmed <- TRUE
  s$theory <- list(k = 3L)
  s$estimator_choice <- "ml"
  log <- new.env(parent = emptyenv())
  local_mocked_bindings(sieve_efa_dmar_start = efa_dmar_fake_start(log))
  shiny::testServer(mod_efa_server, args = list(state = s), {
    session$setInputs(efa_go = 1L, n_factors = 3L, fm = "minres", rotation = "geomin",
      count_to = 4L, count_criterion = "BIC")
    expect_false(is.null(efa_run()$fit))
    # Minres on the item responses: DMAR's ULS on the complete cases.
    expect_match(paste(efa_run()$code_view$code, collapse = "\n"), 'estimator = "ULS",', fixed = TRUE)
    expect_match(paste(efa_run()$code_view$code, collapse = "\n"), 'rotation = "geomin",', fixed = TRUE)
    expect_identical(count_run()$status, "not run")
    expect_length(log$calls, 0L)
    session$setInputs(count_go = 1L)
    efa_dmar_settle(session)
    result <- count_run()
    expect_identical(result$status, "ran")
    expect_length(log$calls, 1L)
    expect_identical(log$calls[[1L]]$fun, "factor_count")
    expect_false(is.null(result$code_view))
    code <- paste(result$code_view$code, collapse = "\n")
    # B = 0: nothing in the comparison is random,
    # so the call carries no seed and needs no seeded wrapper; its parallel
    # analysis reference is the Dimensionality page's (test-dmar-parallel.R).
    expect_match(code, "factor_counts <- DMAR::factor_count(", fixed = TRUE)
    expect_match(code, 'estimator = "ULS",', fixed = TRUE)
    expect_match(code, "max_factors = 4,", fixed = TRUE)
    expect_match(code, "B = 0,", fixed = TRUE)
    expect_match(code, "seed = NULL", fixed = TRUE)
    expect_null(result$rng)
    expect_false(grepl("psych::fa", code, fixed = TRUE))
    expect_false(identical(result$code_view$step, efa_run()$code_view$step))

    frozen <- sieve_replay_expand(
      shiny::isolate(state$replay_states[[result$code_view$input_key]]),
      shiny::isolate(state$replay_data)
    )
    env <- new.env(parent = baseenv())
    env$active_items <- frozen$items
    utils::capture.output(eval(parse(text = code), envir = env))
    replayed <- env$factor_counts
    class(replayed) <- "data.frame"
    shown <- result$table
    class(shown) <- "data.frame"
    expect_equal(replayed[, c("factors", "solution", "srmr", "eigenvalue", "map")],
      shown[, c("factors", "solution", "srmr", "eigenvalue", "map")], tolerance = 1e-10)
    steps <- shiny::isolate(state$codelog)
    count <- length(log$calls)
    output$count_code
    output$count_code
    expect_identical(length(log$calls), count)
    expect_identical(shiny::isolate(state$codelog), steps)
  })
})

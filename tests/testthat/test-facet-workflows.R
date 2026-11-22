# Facet identity and numerical contracts. References are independent
# item/rest calculations and direct calls to the existing statistical engine.

facet_fixture <- function(n = 180L) {
  X <- withr::with_seed(1, {
    f <- stats::rnorm(n)
    g <- .35 * f + sqrt(1 - .35^2) * stats::rnorm(n)
    as.data.frame(cbind(
      sapply(c(.55, .65, .75), function(l) l * f + stats::rnorm(n)),
      sapply(c(.6, .7, .8), function(l) l * g + stats::rnorm(n)),
      .7 * f + stats::rnorm(n), .8 * f + stats::rnorm(n)
    ))
  })
  names(X) <- paste0("q", seq_len(ncol(X)))
  meta <- sieve_build_meta(X, names(X))
  meta$construct <- c(rep("A", 3), rep("B", 3), "A", "A")
  meta$facet <- c(rep("Planning", 6), "Pair", "Pair")
  list(data = X, meta = meta)
}

test_that("facet item-rest uses other-item means and available Pearson pairs", {
  x <- facet_fixture(18)
  x$data$q1 <- -x$data$q1 # scored reversal is supplied, as in state$items
  x$meta$reverse[1] <- TRUE
  x$data$q2[c(2, 4, 6)] <- NA
  x$data$q3[c(3, 4, 7)] <- NA
  x$data$q1[8] <- NA
  d <- sieve_facet_item_rest(x$data, x$meta)
  # Independently form the k-1 mean, retaining a row when one other item
  # is observed and excluding a row when both other items are missing.
  rest <- (ifelse(is.na(x$data$q2), 0, x$data$q2) +
    ifelse(is.na(x$data$q3), 0, x$data$q3)) /
    ((!is.na(x$data$q2)) + (!is.na(x$data$q3)))
  ok <- is.finite(rest) & is.finite(x$data$q1)
  expect_equal(d$facet_item_total[d$item == "q1"], stats::cor(x$data$q1[ok], rest[ok]), tolerance = 1e-14)
  expect_identical(d$facet_item_total_n[d$item == "q1"], as.integer(sum(ok)))
  expect_identical(d$facet_item_total_reason[d$item == "q1"], "")
  # A construct with the same facet label contributes none of these items.
  expect_equal(d$facet_item_total[d$item == "q4"],
    stats::cor(x$data$q4, (x$data$q5 + x$data$q6) / 2),
    tolerance = 1e-14
  )
  complete <- sieve_facet_item_rest(facet_fixture()$data, facet_fixture()$meta)
  expect_true(all(is.finite(complete$facet_item_total)))
  full <- facet_fixture()$data
  expect_equal(complete$facet_item_total[1], stats::cor(full$q1, (full$q2 + full$q3) / 2), tolerance = 1e-14)
  x$meta$status[3] <- "parked"
  d <- sieve_facet_item_rest(x$data, x$meta)
  expect_match(d$facet_item_total_reason[d$item == "q3"], "excluded")
  expect_equal(d$facet_item_total[d$item == "q1"], stats::cor(x$data$q1, x$data$q2, use = "pairwise.complete.obs"))
})

test_that("facet item-rest gives reasons for undefined results and updates membership", {
  x <- facet_fixture(12)
  x$meta$facet[1] <- NA_character_
  x$meta$facet[2] <- "Single"
  x$data$q4 <- 1
  x$data$q7 <- NA_real_
  d <- sieve_facet_item_rest(x$data, x$meta)
  expect_match(d$facet_item_total_reason[1], "No facet")
  expect_match(d$facet_item_total_reason[2], "Only one")
  expect_match(d$facet_item_total_reason[4], "zero variance")
  expect_match(d$facet_item_total_reason[7], "Fewer than two")
  expect_true(all(is.na(d$facet_item_total[c(1, 2, 4, 7)])))
  x <- facet_fixture()
  keep <- new.env(parent = emptyenv())
  first <- sieve_facet_item_rest(x$data, x$meta, keep)
  x$meta$facet[1] <- "Pair"
  second <- sieve_facet_item_rest(x$data, x$meta, keep)
  expect_false(identical(first$facet_item_total[1], second$facet_item_total[1]))
  expect_identical(first[4:6, ], second[4:6, ])
})

test_that("nested facet identity and latent names do not conflate labels", {
  x <- facet_fixture()
  expect_false(sieve_facet_id("A", "Planning") == sieve_facet_id("B", "Planning"))
  expect_false(sieve_facet_id("A.B", "C") == sieve_facet_id("A", "B.C"))
  x$meta$construct[1:3] <- "A.B"
  x$meta$facet[1:3] <- "C"
  x$meta$construct[4:6] <- "A"
  x$meta$facet[4:6] <- "B.C"
  plan <- sieve_name_plan(x$meta)
  expect_false(anyDuplicated(plan$facet$name) > 0L)
  expect_identical(length(sieve_facet_groups(x$meta)), 3L)
})

test_that("direct facet CFA contains selected facet factors without higher-order constructs", {
  skip_if_not_installed("lavaan")
  x <- facet_fixture()
  ids <- names(sieve_facet_groups(x$meta))[1:2]
  spec <- sieve_cfa_spec(x$meta, structure = "facet_first_order", facet_ids = ids)
  expect_identical(spec$structure, "facet_first_order")
  expect_setequal(spec$observed, paste0("q", 1:6))
  expect_length(spec$latent, 2L)
  expect_setequal(unname(spec$name_map), c("A: Planning", "B: Planning"))
  expect_false(any(names(sieve_name_plan(x$meta)$construct) %in% spec$latent))
  expect_false(grepl("a\\*|==", spec$model))
  run <- sieve_run_cfa(spec, x$data)
  expect_true(sieve_has_solution(run))
  expect_setequal(lavaan::lavNames(run$fit, "ov"), spec$observed)
  expect_setequal(lavaan::lavNames(run$fit, "lv"), spec$latent)
  expect_match(paste(sieve_cfa_code(spec), collapse = "\n"), "A_Planning", fixed = TRUE)
  single <- sieve_cfa_spec(x$meta, "facet_first_order", facet_ids = ids[1])
  expect_length(single$latent, 1L)
  expect_length(single$observed, 3L)
  pair <- sieve_cfa_spec(x$meta, "facet_first_order", facet_ids = names(sieve_facet_groups(x$meta))[3])
  expect_match(paste(pair$problems, collapse = " "), "two indicators")
  expect_null(sieve_run_cfa(pair, x$data)$fit)
  higher <- sieve_cfa_plan(x$meta, "higher_order")
  expect_true("A" %in% higher$latent)
  expect_gt(length(higher$latent), length(spec$latent))
})

test_that("facet reliability uses exact items and engine estimands including small facets", {
  skip_if_not_installed("DMAR")
  skip_if_not_installed("lavaan")
  x <- facet_fixture()
  x$data$q1 <- -x$data$q1
  x$meta$reverse[1] <- TRUE
  x$data$q2[1:8] <- NA_real_
  x$meta$status[8] <- "parked"
  groups <- sieve_reliability_items(x$meta, names(x$data), "combined")
  expect_length(groups, 5L)
  facets <- Filter(function(g) g$scope == "facet", groups)
  expect_length(facets, 3L)
  # The default variant is robust omega, so the model implied comparison
  # names it.
  for (g in facets[1:2]) {
    sp <- sieve_reliability_spec(g$items, g$types, g$construct,
      missing = "fiml", ci_method = "ml", denominator = "model_implied",
      group_id = g$id, scope = g$scope, parent_construct = g$parent_construct, facet = g$facet
    )
    r <- sieve_reliability_construct(x$data, sp)
    direct <- DMAR::reliability_omega(
      x$data[
        ,
        g$items
      ],
      ci_method = "ml",
      missing = "fiml",
      denominator = "model_implied"
    )
    a <- suppressWarnings(DMAR::reliability_alpha(x$data[, g$items], estimator = "analytic", ci_method = "ml", missing = "fiml"))
    expect_equal(r$omega$estimate, direct$value[direct$term == "estimate"], tolerance = 1e-12)
    expect_equal(r$omega$lower, direct$value[direct$term == "ci_lower"], tolerance = 1e-12)
    expect_equal(r$alpha$estimate, a$value[a$term == "estimate"], tolerance = 1e-12)
    expect_equal(r$alpha$lower, a$value[a$term == "ci_lower"], tolerance = 1e-12)
    expect_equal(r$alpha$n$used, a$value[a$term == "N"])
  }
  # One excluded item leaves a one-item facet, whose failure is explicit.
  g <- facets[[3]]
  sp <- sieve_reliability_spec(g$items, g$types, g$construct, ci_method = "ml")
  r <- sieve_reliability_construct(x$data, sp)
  expect_match(r$alpha$status$message, "two included items")
  x$meta$status[8] <- "active"
  g <- sieve_reliability_items(x$meta, names(x$data), "facet")[[3]]
  sp <- sieve_reliability_spec(g$items, g$types, g$construct, ci_method = "ml")
  r <- sieve_reliability_construct(x$data, sp)
  expect_true(is.finite(r$alpha$estimate))
  expect_identical(r$alpha$uncertainty$status, "completed")
  expect_identical(r$omega$status$code, "failed")
  expect_match(r$omega$status$message, "not identified")
})

test_that("ordinal and mixed facet reliability delegate the same scored responses", {
  skip_if_not_installed("DMAR")
  x <- facet_fixture(220)
  for (nm in names(x$data)[1:3]) x$data[[nm]] <- as.numeric(cut(x$data[[nm]], c(-Inf, -.5, .3, 1, Inf)))
  x$meta$type[1:3] <- "ordered"
  x$data$q2[1:4] <- NA_real_
  groups <- sieve_reliability_items(x$meta, names(x$data), "facet")
  g <- groups[[1]]
  sp <- sieve_reliability_spec(g$items, g$types, g$construct, ci_method = "none")
  r <- sieve_reliability_construct(x$data, sp)
  # The direct call states the missing data treatment SIEVER's specification
  # resolved, because DMAR's defaults are not SIEVER's to rely on (several of
  # them are FIML).
  direct <- DMAR::reliability_omega_categorical(x$data[, g$items], ci_method = "none",
                                                missing = sp$missing)
  expect_equal(r$omega$estimate, direct$value[direct$term == "estimate"], tolerance = 1e-12)
  expect_equal(r$omega$n$used, direct$value[direct$term == "N"])
  x$meta$type[1] <- "continuous"
  g <- sieve_reliability_items(x$meta, names(x$data), "facet")[[1]]
  # Model implied named explicitly: robust omega is the default.
  sp <- sieve_reliability_spec(g$items, g$types, g$construct, ci_method = "none", denominator = "model_implied")
  r <- sieve_reliability_construct(x$data, sp)
  direct <- DMAR::reliability_omega(x$data[, g$items], ci_method = "none", denominator = "model_implied",
                                    missing = sp$missing)
  expect_equal(r$omega$estimate, direct$value[direct$term == "estimate"], tolerance = 1e-12)
  expect_identical(sp$item_class, "mixed")
})

test_that("reliability retains effective engine counts, reasons, and actual fallback method", {
  x <- facet_fixture()
  sp <- sieve_reliability_spec(names(x$data)[1:3], rep("continuous", 3), "A", ci_method = "dmar_bca", B = 30)
  out <- sieve_reliability_point(x$data, sp, ci_method = "none")$out
  source <- data.frame(term = c("estimate", "se", "ci_lower", "ci_upper"), value = c(.8, .05, .7, .9))
  fallback <- "percentile (BCa acceleration could not be estimated: 60 jackknife replicates failed)"
  attr(source, "ci_method") <- fallback
  counts <- list(
    attempted = 30L, returned = 10L, failed = 20L,
    reasons = c(error = 5L, non_finite = 15L), error_messages = c("fit failed" = 5L),
    acceleration = NA_real_, acceleration_method = NA_character_,
    jackknife = list(attempted = 8L, returned = 7L, failed = 1L)
  )
  attr(source, "bootstrap") <- counts
  out <- sieve_reliability_engine_metadata(out, source)
  expect_identical(out$uncertainty$engine_bootstrap, counts)
  expect_identical(out$uncertainty$B_attempted, 30L)
  expect_identical(out$uncertainty$B_used, 10L)
  expect_identical(out$uncertainty$engine_method, fallback)
  expect_match(out$uncertainty$label, "percentile")
  # (The engine's percentile is DMAR's counted percentile bootstrap,
  # labeled as such; the fallback reason stays with the method.)
  expect_identical(out$uncertainty$label, "Bootstrap percentile")
  expect_identical(out$uncertainty$method_reason, fallback)
  expect_identical(out$uncertainty$requested, "dmar_bca")
  # Exercise the entire wrapper, so its Monte Carlo approximation must use
  # the ten usable replicates and its table must name the actual fallback.
  testthat::local_mocked_bindings(sieve_reliability_engine = function(fun) {
    function(data, ci_method, ...) {
      value <- data.frame(
        term = c("estimate", "se", "ci_lower", "ci_upper", "N"),
        value = c(.8, .05, .7, .9, nrow(data))
      )
      attr(value, "ci_method") <- if (ci_method == "none") "none" else fallback
      if (ci_method != "none") attr(value, "bootstrap") <- counts
      value
    }
  })
  # Controlled fallback/count metadata, not an inferential low-B result.
  siever_local_dmar_mechanics()
  actual <- sieve_reliability_omega(x$data, sp)
  expect_identical(actual$uncertainty$B_used, 10L)
  expect_identical(actual$uncertainty$engine_method, fallback)
  # Ten used replications cannot hold a 95% percentile limit inside the
  # sample (39 are needed; boot returns the extreme values), so no interval
  # is shown, as with SIEVER's own counted bootstrap; the engine's limits and
  # its counts are kept with the reason.
  expect_identical(actual$uncertainty$status, "failed")
  expect_match(actual$uncertainty$reason, "Only 10 of the 30 replications attempted could be used.", fixed = TRUE)
  expect_identical(unname(actual$uncertainty$engine_limits), c(.7, .9))
  expect_identical(sieve_reliability_interval_label(actual), "failed")
})

test_that("sparse categorical resampling preserves effective counts against a direct engine call", {
  skip_if_not_installed("DMAR")
  X <- withr::with_seed(113, {
    eta <- stats::rnorm(60)
    continuous <- outer(eta, rep(.7, 6)) + matrix(stats::rnorm(60 * 6), 60, 6) * sqrt(1 - .7^2)
    binary <- apply(continuous > 0, 2L, as.integer)
    binary[, 1L] <- 0L
    binary[1L, 1L] <- 1L
    as.data.frame(binary)
  })
  names(X) <- paste0("y", 1:6)
  sp <- sieve_reliability_spec(names(X), rep("binary", 6), "Sparse facet",
    ci_method = "dmar_percentile", conf_level = .80, B = 100L, seed = 113
  )
  direct <- suppressWarnings(DMAR::reliability_omega_categorical(X,
    ci_method = "percentile",
    conf_level = .80, B = 100L, seed = 113
  ))
  result <- sieve_reliability_omega(X, sp)
  counts <- attr(direct, "bootstrap")
  expect_equal(c(result$estimate, result$lower, result$upper),
    direct$value[match(c("estimate", "ci_lower", "ci_upper"), direct$term)],
    tolerance = 1e-12
  )
  if (is.list(counts)) {
    expect_identical(counts$attempted, 100L)
    expect_lt(counts$returned, counts$attempted)
    expect_identical(result$uncertainty$engine_bootstrap, counts)
    expect_identical(result$uncertainty$B_used, counts$returned)
    expect_equal(result$uncertainty$mc_se, sieve_bootstrap_mc_error(counts$returned, .8) * result$se, tolerance = 1e-14)
  } else {
    # A result without these attributes: the adapter must report unknown
    # counts and MC error rather than substituting requested B.
    expect_true(is.na(result$uncertainty$B_used))
    expect_true(is.na(result$uncertainty$B_attempted))
    expect_null(result$uncertainty$engine_bootstrap)
    expect_true(is.na(result$uncertainty$mc_se))
  }
  # Every replication's fate is DMAR's status, so the counts are DMAR's,
  # not invented: used is what DMAR returned.
  expect_identical(result$uncertainty$counts$attempted, 100L)
  expect_identical(result$uncertainty$counts$used, counts$returned)
  expect_identical(sum(result$uncertainty$counts$outcome), 100L)
})

test_that("facet scope reaches Shiny fits, saved settings, and membership staleness", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  x <- facet_fixture()
  st <- sieve_new_state()
  st$raw <- st$items <- x$data
  st$meta <- x$meta
  st$confirmed <- TRUE
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    ids <- names(sieve_facet_groups(state$meta))[1:2]
    session$setInputs(
      scope = "facet", all_facets = FALSE, facets = ids,
      structure = "first", ident = "varone"
    )
    expect_identical(build_spec()$structure, "facet_first_order")
    expect_length(build_spec()$latent, 2L)
    session$setInputs(fit_go = 1)
    expect_true(sieve_has_solution(cfa_run()))
    expect_identical(state$cfa_settings$scope, "facet")
    expect_identical(state$cfa_settings$facets, ids)
    session$setInputs(facets = ids[1])
    # A different model choice has its own fit history and has not run yet.
    expect_identical(demand$status()$state, "not_run")
    expect_null(demand$status()$value)
    session$setInputs(fit_go = 2)
    expect_length(cfa_run()$spec$latent, 1L)
    expect_length(cfa_run()$spec$observed, 3L)
    expect_match(output$specification$html, "q1, q2, q3", fixed = TRUE)
    expect_match(output$specification$html, "outside the selected model", fixed = TRUE)
  })
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "facet", denominator = "model_implied", ci_method = "ml",
      missing = "listwise", conf_level = .95, show_alpha = TRUE, treatment = "study"
    )
    session$setInputs(run_go = 1)
    table <- reliab()
    expect_identical(nrow(table), 3L)
    expect_true(all(table$scope == "facet"))
    expect_true(all(is.finite(table$alpha)))
    expect_true(is.na(table$omega[table$facet == "Pair"]))
    expect_identical(table$alpha_n[table$facet == "Pair"], 180L)
    expect_identical(state$reliability_settings$scope, "facet")
    state$meta$facet[1] <- "Pair"
    session$flushReact()
    expect_identical(demand$status()$state, "stale")
  })
})

test_that("restored facet settings wait for browser input acknowledgments", {
  skip_if_not_installed("shiny")
  x <- facet_fixture()
  st <- sieve_new_state()
  st$raw <- st$items <- x$data
  st$meta <- x$meta
  st$confirmed <- TRUE
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(scope = "construct", structure = "first", ident = "varone", all_facets = TRUE)
    ids <- names(sieve_facet_groups(state$meta))[1]
    saved <- list(scope = "facet", structure = "first", ident = "marker", facets = ids, all_facets = FALSE)
    state$cfa_settings <- saved
    session$flushReact()
    # update*Input sends a browser message. Before its acknowledgment,
    # the old input values must not overwrite the restored project.
    expect_identical(state$cfa_settings, saved)
    session$setInputs(scope = "facet", structure = "first", ident = "marker", facets = ids, all_facets = FALSE)
    expect_identical(state$cfa_settings, saved)
    expect_identical(build_spec()$structure, "facet_first_order")
    expect_false(build_spec()$std_lv)
  })
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(
      scope = "construct", treatment = "study", missing = "listwise",
      denominator = "model_implied", ci_method = "ml", conf_level = .95, B = 30, show_alpha = TRUE, small_sample_t = FALSE
    )
    saved <- list(
      scope = "facet", treatment = "continuous", missing = "fiml",
      denominator = "model_implied", ci_method = "ml", conf_level = .90, B = 50,
      boot_seed = 2L, show_alpha = TRUE, small_sample_t = FALSE
    )
    state$reliability_settings <- saved
    session$flushReact()
    expect_identical(state$reliability_settings, saved)
    session$setInputs(
      scope = "facet", treatment = "continuous", missing = "fiml",
      denominator = "model_implied", ci_method = "ml", conf_level = .90, B = 50,
      boot_seed = 2, show_alpha = TRUE, small_sample_t = FALSE
    )
    expect_identical(state$reliability_settings, saved)
    expect_identical(settings()$missing, "fiml")
  })
})

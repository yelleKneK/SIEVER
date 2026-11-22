# One operational score per construct: sieve_composite_score() as the one
# composite scorer, the score specification stored per construct and read by
# the Validity tab, no silent mean score where a factor score was asked for,
# the marker-variable adjustment with its sensitivity and t test, and the HTMT
# and calibration transformation bootstraps on the Reliability tab's request
# pattern.

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("shiny")

spec_text <- function(x) {
  if (is.null(x)) return("")
  html <- if (inherits(x, c("shiny.tag", "shiny.tag.list"))) {
    as.character(x)
  } else {
    paste(as.character(unlist(x)), collapse = " ")
  }
  gsub("&amp;", "&", gsub("\\s+", " ", gsub("<[^>]+>", " ", html)), fixed = TRUE)
}

spec_html <- function(x) paste(as.character(unlist(x)), collapse = " ")

# Two constructs of three items, a criterion related to the first, a marker
# that is positively related to both constructs through a shared method
# component, and a marker that is negatively related to both.
spec_data <- function(n = 400) {
  withr::with_seed(20261022, {
    f1 <- stats::rnorm(n)
    f2 <- 0.3 * f1 + sqrt(1 - 0.09) * stats::rnorm(n)
    method <- stats::rnorm(n)
    item <- function(f, l) l * f + 0.25 * method + sqrt(1 - l^2 - 0.0625) * stats::rnorm(n)
    d <- data.frame(
      a1 = item(f1, .8), a2 = item(f1, .75), a3 = item(f1, .7),
      b1 = item(f2, .8), b2 = item(f2, .75), b3 = item(f2, .7)
    )
    d$crit <- 0.5 * f1 + sqrt(0.75) * stats::rnorm(n)
    d$marker_pos <- 0.5 * method + sqrt(0.75) * stats::rnorm(n)
    d$marker_neg <- -0.6 * (f1 + f2) + stats::rnorm(n)
    d
  })
}

spec_state <- function(d = spec_data()) {
  fixture <- list(data = d, item_cols = names(d)[1:6],
    name = "Study fixture", theory_map = NULL, labels = NULL, load_code = character())
  siever_test_study(fixture, labels = FALSE, theory = FALSE, name = NULL, configure = function(s) {
    s$meta <- sieve_build_meta(d, names(d)[1:6], list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3")))
    s$roles <- sieve_declared_roles(d, names(d)[1:6])
  })
}

spec_nine <- function() {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = TRUE, name = fx$name, configure = function(s) {
  s$roles <- sieve_declared_roles(fx$data, fx$item_cols)
  s$estimator <- "MLR"

  })
  s
}

# ---- sieve_composite_score() -------------------------------------------------

test_that("sieve_composite_score applies the minimum answered rule, weights and proration exactly", {
  d <- data.frame(a = c(1, 2, NA, NA, 4), b = c(3, NA, 4, NA, 6), c = c(NA, 6, 8, NA, 2))
  r <- sieve_composite_score(d, names(d))
  expect_equal(r$scores, c(2, 4, 6, NA, 4))
  expect_identical(r$answered, c(2, 2, 2, 0, 3))
  expect_identical(r$eligible, c(TRUE, TRUE, TRUE, FALSE, TRUE))
  expect_identical(r$policy$method, "mean")
  expect_identical(r$policy$required, 1L)
  # min_count and min_prop combine as the larger requirement.
  r2 <- sieve_composite_score(d, names(d), min_count = 2L, min_prop = .5)
  expect_identical(r2$policy$required, 2L)
  expect_equal(r2$scores, c(2, 4, 6, NA, 4))
  r3 <- sieve_composite_score(d, names(d), min_count = 3L)
  expect_equal(r3$scores, c(NA, NA, NA, NA, 4))
  r4 <- sieve_composite_score(d, names(d), min_prop = 1)
  expect_identical(r4$policy$required, 3L)
  expect_identical(r4$eligible, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  # Weights, named by item in any order.
  w <- sieve_composite_score(d, names(d), weights = c(c = 1, a = 2, b = 1))
  expect_equal(w$scores, c(5 / 3, 10 / 3, 6, NA, (8 + 6 + 2) / 4))
  expect_identical(w$policy$weights, c(a = 2, b = 1, c = 1))
  # The prorated sum is the mean times the total weight, person by person.
  s <- sieve_composite_score(d, names(d), method = "sum", min_count = 2L)
  expect_equal(s$scores, c(2, 4, 6, NA, 4) * 3)
  expect_match(s$policy$missing, "prorated")
  sw <- sieve_composite_score(d, names(d), weights = c(a = 2, b = 1, c = 1), method = "sum")
  expect_equal(sw$scores, w$scores * 4)
  # An all-missing row is never scored, whatever the rule.
  expect_true(is.na(sieve_composite_score(d[4, , drop = FALSE], names(d))$scores))
  expect_equal(sieve_composite_score(d[5, , drop = FALSE], names(d), method = "sum")$scores, 12)
  # The refusals.
  expect_error(sieve_composite_score(d, c("a", "a")), "distinct")
  expect_error(sieve_composite_score(d, names(d), min_prop = 1.1), "min_prop")
  expect_error(sieve_composite_score(d, names(d), min_count = 0), "min_count")
  expect_error(sieve_composite_score(d, names(d), weights = c(a = -1, b = 1, c = 1)), "weights")
  expect_error(sieve_composite_score(as.matrix(d), names(d)), "data frame")
})

test_that("sieve_score_policy and sieve_construct_norms are the composite scorer, result for result", {
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  d$x2[1:5] <- NA
  d$x3[1:2] <- NA
  d$x7[10:30] <- NA
  for (count in 1:3) {
    a <- sieve_score_policy(d, c("x1", "x2", "x3"), min_count = count, min_prop = .3, weights = c(x1 = 2, x2 = 1, x3 = 1))
    b <- sieve_composite_score(d, c("x1", "x2", "x3"), weights = c(x1 = 2, x2 = 1, x3 = 1), min_count = count, min_prop = .3)
    expect_identical(a$scores, b$scores)
    expect_identical(a$answered, b$answered)
    expect_identical(a$eligible, b$eligible)
    expect_identical(a$policy, b$policy[names(b$policy) != "method"])
  }
  meta <- data.frame(
    item = names(d), label = names(d), construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = "continuous", status = "active", reverse = FALSE,
    stringsAsFactors = FALSE
  )
  policies <- list(Visual = list(min_count = 2L), Speed = list(method = "sum", min_prop = .5))
  norms <- sieve_construct_norms(d, meta, policies)
  # Each construct's scores are the composite scorer's under its policy.
  expect_identical(norms$scores$Visual, sieve_composite_score(d, paste0("x", 1:3), min_count = 2L)$scores)
  expect_identical(norms$scores$Textual, sieve_composite_score(d, paste0("x", 4:6))$scores)
  expect_identical(norms$scores$Speed, sieve_composite_score(d, paste0("x", 7:9), method = "sum", min_prop = .5)$scores)
  # The table's n, mean and SD are the direct computation on the eligible scores.
  speed <- norms$scores$Speed[is.finite(norms$scores$Speed)]
  row <- norms$table[norms$table$construct == "Speed", ]
  expect_identical(row$n, length(speed))
  expect_equal(row$mean, mean(speed), tolerance = 1e-12)
  expect_equal(row$sd, stats::sd(speed), tolerance = 1e-12)
  expect_equal(row$p50, unname(stats::quantile(speed, .5, type = 7)), tolerance = 1e-12)
  expect_identical(norms$definition$Speed$method, "sum")
  # A construct specified as a factor score is not a composite: refused by name.
  latent <- sieve_construct_norms(d, meta, list(Textual = list(method = "bartlett")))
  expect_identical(latent$table$construct, c("Visual", "Speed"))
  expect_match(latent$refused, "'Textual' is not scored here: its operational score is the Bartlett factor score", fixed = TRUE)
  # A frozen reference applies under the same specification and refuses a changed method.
  ref <- sieve_norm_freeze(d, meta, policies, name = "Cal", population = "Demonstration")
  applied <- sieve_norm_apply(ref, d, meta, policies)
  expect_equal(applied$score[applied$construct == "Speed"], norms$scores$Speed)
  expect_error(sieve_norm_apply(ref, d, meta, list(Visual = list(min_count = 2L), Speed = list(method = "mean", min_prop = .5))), "differ")
  # A reference whose definition has no `method` applies.
  old <- ref
  old$definition <- lapply(old$definition, function(x) {
    x$method <- NULL
    x$min_count <- as.numeric(x$min_count)
    x
  })
  old$definition$Speed$method <- "sum"
  expect_equal(sieve_norm_apply(old, d, meta, policies)$score, applied$score)
})

test_that("the specification is built from the ledger, repaired on read, and kept by the setter", {
  s <- spec_nine()
  spec <- sieve_score_specification(s)
  expect_identical(names(spec), c("Visual", "Textual", "Speed"))
  expect_true(all(vapply(spec, function(x) identical(x$method, "mean") && x$required == 1L && !x$latent, logical(1))))
  # A policy with no method is read as the mean, without a flag. An unknown
  # method is read as the mean too, but flagged for review.
  s$scoring_policies <- list(Visual = list(min_count = 2L, min_prop = 0), Speed = list(method = "nonsense"))
  spec <- sieve_score_specification(s)
  expect_identical(spec$Visual$method, "mean")
  expect_identical(spec$Visual$required, 2L)
  expect_null(spec$Visual$issue)
  expect_identical(spec$Speed$method, "mean")
  expect_match(spec$Speed$issue, "\"nonsense\" is not one this version of SIEVER knows", fixed = TRUE)
  stored <- sieve_set_score_specification(s, "Textual", method = "bartlett", min_count = 2L)
  expect_identical(stored, list(method = "bartlett", min_count = 2L, min_prop = 0))
  expect_identical(shiny::isolate(s$scoring_policies$Textual), stored)
  spec <- sieve_score_specification(s)
  expect_true(spec$Textual$latent)
  expect_identical(spec$Textual$label, "Bartlett factor score")
  # Parts not supplied keep their stored values; weights are kept too.
  sieve_set_score_specification(s, "Textual", min_prop = .5)
  expect_identical(shiny::isolate(s$scoring_policies$Textual), list(method = "bartlett", min_count = 2L, min_prop = .5))
  sieve_set_score_specification(s, "Visual", weights = c(2, 1, 1))
  sieve_set_score_specification(s, "Visual", method = "sum")
  expect_identical(shiny::isolate(s$scoring_policies$Visual)$weights, c(2, 1, 1))
  expect_error(sieve_set_score_specification(s, "Visual", method = "pca"), "must be one of")
  expect_error(sieve_set_score_specification(s, "Visual", min_count = 0), "positive integer")
  expect_error(sieve_set_score_specification(s, "", method = "mean"), "construct")
  # The table of the specification in force carries the eligible n.
  items <- shiny::isolate(s$items)
  items[1, 1:2] <- NA
  s$items <- items
  tab <- sieve_score_specification_table(items, sieve_score_specification(s, available = names(items)))
  expect_identical(tab$construct, c("Visual", "Textual", "Speed"))
  expect_identical(tab$required, c(2L, 2L, 1L))
  expect_identical(tab$eligible[1], nrow(items) - 1L)
  expect_identical(tab$not_scored[1], 1L)
})

test_that("the operational score card keeps a construct's specification with its method and shows the eligible n", {
  s <- spec_nine()
  shiny::testServer(mod_reliab_server, args = list(state = s), {
    expect_match(spec_text(sieve_operational_ui(shiny::NS("x"))), "Operational Score Specification", fixed = TRUE)
    session$setInputs(policy_construct = "Visual", policy_method = "bartlett", policy_count = 2, policy_prop = 0)
    session$setInputs(policy_apply = 1)
    kept <- shiny::isolate(state$scoring_policies$Visual)
    expect_identical(kept, list(method = "bartlett", min_count = 2L, min_prop = 0))
    expect_match(spec_text(output$policy_note), "Specification kept for Visual: Bartlett factor score; at least 2 of 3 answered items", fixed = TRUE)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, 'scoring_policies[["Visual"]] <- list(method = "bartlett", min_count = 2L, min_prop = 0)', fixed = TRUE)
    # The table of the specification in force (the card's policy_specification output).
    items <- shiny::isolate(state$items)
    tab <- sieve_score_specification_table(items, sieve_score_specification(state, available = names(items)))
    expect_identical(tab$score[tab$construct == "Visual"], "Bartlett factor score")
    expect_identical(tab$required[tab$construct == "Visual"], 2L)
    expect_identical(tab$eligible[tab$construct == "Visual"], 301L)
    expect_no_error(output$policy_specification)
    session$setInputs(policy_construct = "Speed", policy_method = "sum", policy_count = 1, policy_prop = 0)
    session$setInputs(policy_apply = 2)
    expect_identical(shiny::isolate(state$scoring_policies$Speed)$method, "sum")
    expect_match(paste(shiny::isolate(state$codelog), collapse = "\n"), 'sieve_composite_score(items, items = c("t10_addition", "t12_counting_groups_of_dots", "t13_straight_and_curved_capitals"), method = "sum", min_count = 1L, min_prop = 0)', fixed = TRUE)
  })
})

# ---- The Validity tab reads the specification --------------------------------

test_that("a respondent below the policy is excluded from every criterion correlation, with the n shown", {
  d <- spec_data()
  d$a1[1] <- NA
  s <- spec_state(d)
  sieve_set_score_specification(s, "A", min_count = 3L)
  shiny::testServer(mod_validity_server, args = list(state = s), {
    session$setInputs(criteria = "crit", htmt_on = FALSE, marker = "")
    sc <- construct_scores()
    expect_null(sc$error)
    expect_true(is.na(sc$scores$A[1]))
    expect_false(is.na(sc$scores$B[1]))
    expect_identical(sc$eligible$A, nrow(d) - 1L)
    expect_identical(sc$eligible$B, nrow(d))
    ev <- crit_df()
    a <- ev[ev$construct == "A", ]
    b <- ev[ev$construct == "B", ]
    expect_identical(a$n, nrow(d) - 1L)
    expect_identical(b$n, nrow(d))
    # The direct computation: the composite under the policy, correlated with the criterion.
    direct <- stats::cor.test(sieve_composite_score(d, c("a1", "a2", "a3"), min_count = 3L)$scores, d$crit)
    expect_equal(a$r, unname(direct$estimate), tolerance = 1e-12)
    expect_equal(c(a$lower, a$upper), direct$conf.int[1:2], tolerance = 1e-12)
    expect_equal(b$r, unname(stats::cor.test(rowMeans(d[, c("b1", "b2", "b3")]), d$crit)$estimate), tolerance = 1e-12)
    note <- spec_text(output$crit_note)
    expect_match(note, sprintf("A: mean of the answered items, at least 3 of 3 items answered; n eligible = %d", nrow(d) - 1L), fixed = TRUE)
    expect_match(note, sprintf("B: mean of the answered items, at least 1 of 3 items answered; n eligible = %d", nrow(d)), fixed = TRUE)
    expect_match(note, "from the cases that have an eligible score and the criterion", fixed = TRUE)
    invisible(output$crit_tbl)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, 'construct_scores[["A"]] <- SIEVER::sieve_composite_score(items, items = c("a1", "a2", "a3"), weights = NULL, method = "mean", min_count = 3L, min_prop = 0)$scores', fixed = TRUE)
    # The recorded code reproduces the table.
    env <- new.env(parent = baseenv())
    env$items <- shiny::isolate(state$items)
    env$d <- shiny::isolate(state$raw)
    code <- sub(".*(# The operational score of each construct.*?\\}\\)\\))", "\\1", paste(log, collapse = "\n"))
    expect_no_error(eval(parse(text = code), envir = env))
    expect_equal(env$criterion_r$r[env$criterion_r$construct == "A"], a$r, tolerance = 1e-12)
    expect_identical(env$criterion_r$n[env$criterion_r$construct == "A"], as.integer(nrow(d) - 1L))
  })
})

test_that("a factor score asked for without a fit is unavailable, with the Fit action and an explicit mean-score choice, never a silent mean", {
  s <- spec_state()
  sieve_set_score_specification(s, "A", method = "bartlett")
  sieve_set_score_specification(s, "B", method = "regression", min_count = 2L)
  shiny::testServer(mod_validity_server, args = list(state = s), {
    session$setInputs(criteria = "crit", htmt_on = FALSE, marker = "")
    sc <- construct_scores()
    expect_null(sc$scores)
    expect_identical(sc$unavailable, "not_fit")
    expect_match(sc$error, "which is not fit", fixed = TRUE)
    expect_match(sc$method, "^unavailable")
    expect_error(crit_df(), "not fit")
    note <- spec_html(output$crit_note)
    expect_match(note, "The construct scores are unavailable.", fixed = TRUE)
    # The note's copy of the fit button has its own input id, so the page's
    # own fit_go is not repeated (two elements with one id leave one of
    # them unbound in the browser).
    expect_match(note, sprintf('id="%s"', session$ns("scores_fit_go")), fixed = TRUE)
    expect_no_match(note, sprintf('id="%s"', session$ns("fit_go")), fixed = TRUE)
    expect_match(note, sprintf('id="%s"', session$ns("scores_use_mean")), fixed = TRUE)
    expect_match(note, "Use mean scores instead", fixed = TRUE)
    expect_match(note, "no mean score is substituted on its own", fixed = TRUE)
    expect_no_error(output$crit_tbl)
    # The marker adjustment reads the same scores and is unavailable too.
    session$setInputs(marker = "marker_pos")
    expect_error(marker_df(), "not fit")
    # The explicit choice: mean scores, with each construct's minimum answered rule kept.
    session$setInputs(scores_use_mean = 1)
    spec <- sieve_score_specification(state)
    expect_identical(spec$A$method, "mean")
    expect_identical(spec$B$method, "mean")
    expect_identical(spec$B$min_count, 2L)
    sc <- construct_scores()
    expect_null(sc$error)
    expect_identical(sc$method, "mean of the answered items")
    expect_true(nrow(crit_df()) > 0)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, 'scoring_policies[["A"]]$method <- "mean"', fixed = TRUE)
  })
})

test_that("factor scores come from the fitted model under the policy; a model from before names Update; an engine failure is reported, not replaced", {
  d <- spec_data()
  d$a1[1:2] <- NA
  s <- spec_state(d)
  sieve_set_score_specification(s, "A", method = "bartlett", min_count = 3L)
  shiny::testServer(mod_validity_server, args = list(state = s), {
    session$setInputs(criteria = "crit", htmt_on = FALSE, marker = "")
    session$setInputs(fit_go = 1)
    expect_identical(demand$status()$state, "current")
    sc <- construct_scores()
    expect_null(sc$error)
    expect_identical(sc$labels[["A"]], "Bartlett factor score")
    expect_identical(sc$labels[["B"]], "mean of the answered items")
    expect_match(sc$method, "as specified per construct (A: Bartlett factor score; B: mean of the answered items)", fixed = TRUE)
    # The minimum answered rule applies to the factor score as well.
    expect_true(all(is.na(sc$scores$A[1:2])))
    expect_identical(sc$eligible$A, nrow(d) - 2L)
    direct <- sieve_factor_scores(fit(), method = "Bartlett")
    names(direct) <- sieve_show_names(names(direct), fit())
    expect_equal(sc$scores$A[-(1:2)], direct$A[-(1:2)], tolerance = 1e-12)
    ev <- crit_df()
    expect_identical(ev$n[ev$construct == "A"], nrow(d) - 2L)
    invisible(output$crit_tbl)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, '.fs_Bartlett <- SIEVER::sieve_factor_score_record(validity_fit, method = "Bartlett", data = items)$scores', fixed = TRUE)
    expect_match(log, 'construct_scores[["A"]] <- .fs_Bartlett[["A"]]', fixed = TRUE)
    expect_match(log, 'construct_scores[["A"]][rowSums(!is.na(items[, c("a1", "a2", "a3"), drop = FALSE])) < 3L] <- NA_real_', fixed = TRUE)
    # The sidebar control follows the specification: mixed methods leave it, one method sets it.
    sieve_set_score_specification(state, "B", method = "bartlett")
    session$flushReact()
    expect_identical(construct_scores()$method, "Bartlett factor score")
    # A change of the item data leaves the model from before: the stale
    # message names the button that exists (Update), and no score is formed.
    x <- shiny::isolate(state$items)
    x$b1 <- -x$b1
    state$items <- x
    sieve_confirm_types(state)
    session$flushReact()
    expect_identical(demand$status()$state, "stale")
    sc <- construct_scores()
    expect_null(sc$scores)
    expect_identical(sc$unavailable, "stale")
    expect_match(sc$error, "from before the last change", fixed = TRUE)
    expect_match(sc$error, "press Update, in the line at the top of the page", fixed = TRUE)
    expect_false(grepl("Refit", sc$error, fixed = TRUE))
    expect_error(crit_df(), "press Update")
    note <- spec_html(output$crit_note)
    expect_match(note, "Use mean scores instead", fixed = TRUE)
    session$setInputs(update_go = 1)
    expect_identical(demand$status()$state, "current")
    expect_null(construct_scores()$error)
  })
  # A scoring engine failure is reported as such: no mean score in its place.
  s2 <- spec_state(d)
  sieve_set_score_specification(s2, "A", method = "bartlett")
  testthat::local_mocked_bindings(sieve_factor_scores = function(...) stop("the engine refused"), .package = "SIEVER")
  shiny::testServer(mod_validity_server, args = list(state = s2), {
    session$setInputs(criteria = "crit", htmt_on = FALSE, marker = "")
    session$setInputs(fit_go = 1)
    sc <- construct_scores()
    expect_null(sc$scores)
    expect_identical(sc$unavailable, "engine")
    expect_match(sc$error, "The scoring engine stopped for the Bartlett factor score of A: the engine refused", fixed = TRUE)
    expect_error(crit_df(), "engine refused")
  })
})

test_that("the sidebar score control sets every construct's specification and follows it", {
  s <- spec_state()
  shiny::testServer(mod_validity_server, args = list(state = s), {
    session$setInputs(criteria = "crit", htmt_on = FALSE, marker = "")
    expect_identical(sieve_score_specification(state)$A$method, "mean")
    session$setInputs(score_method = "regression")
    spec <- sieve_score_specification(state)
    expect_identical(unname(vapply(spec, `[[`, character(1), "method")), c("regression", "regression"))
    expect_match(paste(shiny::isolate(state$codelog), collapse = "\n"), 'scoring_policies[["A"]]$method <- "regression"', fixed = TRUE)
    session$setInputs(score_method = "mean")
    expect_identical(sieve_score_specification(state)$B$method, "mean")
  })
})

# ---- The marker-variable adjustment --------------------------------------------

test_that("sieve_marker_sensitivity adjusts through DMAR and tests each adjusted correlation on n - 3 degrees of freedom", {
  hs <- lavaan::HolzingerSwineford1939
  scores <- data.frame(
    Visual = rowMeans(hs[, c("x1", "x2", "x3")]),
    Textual = rowMeans(hs[, c("x4", "x5", "x6")]),
    Speed = rowMeans(hs[, c("x7", "x8", "x9")])
  )
  R <- stats::cor(scores)
  n <- nrow(hs)
  sens <- sieve_marker_sensitivity(R, marker_r = c(0.05, 0.10), n = n)
  expect_identical(nrow(sens), 6L)
  expect_identical(sens$marker_r, rep(c(0.05, 0.10), each = 3L))
  expect_identical(sens$variable_1, rep(c("Visual", "Visual", "Textual"), 2L))
  expect_identical(sens$variable_2, rep(c("Textual", "Speed", "Speed"), 2L))
  for (rM in c(0.05, 0.10)) {
    rows <- sens[sens$marker_r == rM, ]
    dmar <- attr(DMAR::common_method_marker(R, marker_r = rM), "adjusted")
    expect_equal(rows$r_adjusted, c(dmar["Visual", "Textual"], dmar["Visual", "Speed"], dmar["Textual", "Speed"]), tolerance = 1e-12)
    expect_equal(rows$r_adjusted, (rows$r - rM) / (1 - rM), tolerance = 1e-12)
    expect_equal(attr(sens, "adjusted")[[as.character(rM)]], dmar, tolerance = 1e-12)
    t_direct <- rows$r_adjusted / sqrt((1 - rows$r_adjusted^2) / (n - 3))
    expect_equal(rows$t, t_direct, tolerance = 1e-12)
    expect_identical(rows$df, rep(n - 3, 3L))
    expect_equal(rows$p, 2 * stats::pt(abs(t_direct), n - 3, lower.tail = FALSE), tolerance = 1e-12)
  }
  # Without n, the adjustment alone.
  bare <- sieve_marker_sensitivity(R, 0.05)
  expect_true(all(is.na(bare$t)) && all(is.na(bare$p)))
  expect_null(attr(bare, "n", exact = TRUE))
  # An adjusted correlation at or beyond one has no t.
  R2 <- matrix(c(1, -.9, -.9, 1), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  beyond <- sieve_marker_sensitivity(R2, 0.2, n = 50)
  expect_lt(beyond$r_adjusted, -1)
  expect_true(is.na(beyond$t))
  expect_error(sieve_marker_sensitivity(R, 1), "inside (-1, 1)", fixed = TRUE)
  expect_error(sieve_marker_sensitivity(R, 0.1, n = 3), "above 3")
  expect_error(sieve_marker_sensitivity(R[1, , drop = FALSE], 0.1), "symmetric")
})

test_that("the marker panel is headed, worded, and read over three values of r_M with the t test", {
  ui <- spec_text(mod_validity_ui("v"))
  expect_match(ui, "Marker-Variable Adjustment of Observed Construct-Score Correlations (Lindell & Whitney, 2001)", fixed = TRUE)
  expect_false(grepl("CFA marker variable", ui, fixed = TRUE))
  d <- spec_data()
  s <- spec_state(d)
  shiny::testServer(mod_validity_server, args = list(state = s), {
    session$setInputs(criteria = character(0), htmt_on = FALSE, marker = "marker_pos", marker_r2 = NA, marker_r3 = NA)
    res <- marker_df()
    expect_null(res$error)
    # r_M: the smallest positive marker to construct-score correlation; the direct computation.
    scores <- data.frame(A = rowMeans(d[, c("a1", "a2", "a3")]), B = rowMeans(d[, c("b1", "b2", "b3")]))
    r_marker <- c(A = stats::cor(scores$A, d$marker_pos), B = stats::cor(scores$B, d$marker_pos))
    expect_true(all(r_marker > 0))
    expect_equal(res$rM, min(r_marker), tolerance = 1e-12)
    R <- stats::cor(scores)
    expect_equal(res$tbl$r.unadjusted, R["A", "B"], tolerance = 1e-12)
    expect_equal(res$tbl$r.adjusted, (R["A", "B"] - res$rM) / (1 - res$rM), tolerance = 1e-12)
    expect_identical(res$n, nrow(d))
    expect_equal(res$tbl$t, res$tbl$r.adjusted / sqrt((1 - res$tbl$r.adjusted^2) / (nrow(d) - 3)), tolerance = 1e-12)
    # Three values: r_M and, blank inputs, r_M + .05 and r_M + .10.
    expect_equal(res$values, c(res$rM, res$rM + .05, res$rM + .10), tolerance = 1e-12)
    expect_identical(nrow(res$sensitivity), 3L)
    expect_identical(res$sensitivity$marker_r, res$values)
    note <- spec_text(output$marker_note)
    expect_match(note, "What is adjusted: the Pearson correlation matrix of the operational construct scores in force (mean of the answered items)", fixed = TRUE)
    expect_match(note, "assumes a constant method effect: the same r_M is removed from every correlation", fixed = TRUE)
    expect_match(note, "consistent with a relationship that is not solely a method artifact under that assumption", fixed = TRUE)
    expect_false(grepl("evidence a relationship is not a method artifact", note, fixed = TRUE))
    expect_match(spec_text(output$marker_sens_note), "Sensitivity: the adjustment at r_M =", fixed = TRUE)
    expect_no_error(output$marker_sens_tbl)
    invisible(output$marker_tbl)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, sprintf("SIEVER::sieve_marker_sensitivity(R, marker_r = c(%s), n = %d)", paste(sieve_num_lit(res$values), collapse = ", "), nrow(d)), fixed = TRUE)
    # The researcher's own values are used.
    session$setInputs(marker_r2 = 0.3, marker_r3 = 0.4)
    expect_equal(marker_df()$values, c(res$rM, 0.3, 0.4), tolerance = 1e-12)
    # No positive marker correlation: undefined, said as such.
    session$setInputs(marker = "marker_neg")
    res <- marker_df()
    expect_true(is.na(res$rM))
    expect_null(res$tbl)
    expect_no_error(output$marker_tbl)
    # The note's wording (the table is served to the browser, so its text is read from the source).
    src <- paste(deparse(mod_validity_server), collapse = "\n")
    expect_match(src, "undefined here without a positive marker correlation", fixed = TRUE)
    expect_false(grepl("no shared method variance", src, fixed = TRUE))
    expect_false(grepl("Refit the measurement model", src, fixed = TRUE))
  })
})

test_that("the reflective paragraph qualifies exchangeability with content coverage and cites the attenuation paradox", {
  ui <- spec_text(mod_validity_ui("v"))
  expect_match(ui, "exchangeable indicators of one common cause", fixed = TRUE)
  expect_match(ui, "dropping items can narrow the content the scale represents", fixed = TRUE)
  expect_match(ui, "raises internal consistency at the expense of validity, the attenuation paradox (Loevinger, 1954; Clark & Watson, 2019)", fixed = TRUE)
  expect_match(ui, "Bollen & Lennox, 1991; Bollen & Bauldry, 2011", fixed = TRUE)
  expect_false(grepl("dropping one does not change what the construct is", ui, fixed = TRUE))
  expect_match(ui, "HTMT reads the items, not the scores", fixed = TRUE)
})

# ---- The HTMT bootstrap request ---------------------------------------------------

test_that("the HTMT bootstrap records its method, level, B, seed and counts, reuses a matching request, and never lowers B", {
  s <- spec_state()
  calls <- new.env(parent = emptyenv())
  calls$B <- integer(0)
  calls$level <- numeric(0)
  real <- DMAR::htmt
  testthat::local_mocked_bindings(
    htmt = function(data, blocks, B = 0, conf_level = 0.95, seed = NULL) {
      out <- real(data, blocks)
      if (B > 0) {
        calls$B <- c(calls$B, as.integer(B))
        calls$level <- c(calls$level, conf_level)
        out$ci_upper <- out$htmt + 0.1
        attr(out, "conf_level") <- conf_level
        attr(out, "B") <- as.integer(B)
        attr(out, "B_used") <- as.integer(B)
        attr(out, "n_dropped") <- 0L
        attr(out, "B_required") <- sieve_dmar_boot_required("percentile", conf_level, "one")
        attr(out, "bootstrap_status") <- "complete"
      }
      out
    },
    .package = "DMAR"
  )
  shiny::testServer(mod_validity_server, args = list(state = s), {
    session$setInputs(criteria = character(0), htmt_on = TRUE, marker = "", htmt_B = 100, htmt_seed = 8, conf_level = 0.9, htmt_thresh = 0.85)
    invisible(htmt_pairs())
    expect_true(is.finite(htmt_seconds()))
    plan <- spec_text(output$htmt_boot_plan)
    expect_match(plan, "Method: one-sided percentile bootstrap upper bound (DMAR::htmt) at the 90% level, resampling independent rows", fixed = TRUE)
    expect_match(plan, "With B = 100 the bootstrap is estimated to take", fixed = TRUE)
    expect_match(plan, "Hesterberg (2015) suggests 10,000 for routine use", fixed = TRUE)
    expect_match(spec_text(output$htmt_note), "HTMT is computed from the item correlations", fixed = TRUE)
    session$setInputs(htmt_boot = 1)
    r <- htmt_ci()
    expect_null(r$error)
    expect_identical(calls$B, 100L)
    expect_identical(calls$level, 0.9)
    expect_identical(r$B, 100)
    expect_identical(r$attempted, 100)
    expect_identical(r$used, 100L)
    expect_identical(r$seed, 8L)
    expect_true(r$seed_supplied)
    expect_identical(r$level, 0.9)
    expect_false(r$reused)
    expect_match(r$method, "one-sided percentile bootstrap upper bound (DMAR::htmt) at the 90% level, resampling independent rows", fixed = TRUE)
    note <- spec_text(output$htmt_ci_note)
    expect_match(note, "B = 100 resamples attempted, 100 of the 100 resamples were used; seed = 8; it took", fixed = TRUE)
    # The analysis registry holds the request with its counts.
    recorded <- Filter(function(a) identical(a$kind, "htmt_bootstrap"), shiny::isolate(state$analyses))
    expect_length(recorded, 1L)
    expect_identical(recorded[[1]]$spec$B, 100)
    expect_identical(recorded[[1]]$spec$seed, 8L)
    expect_identical(recorded[[1]]$spec$conf_level, 0.9)
    expect_identical(recorded[[1]]$estimates$counts,
      list(attempted = 100L, used = 100L, dropped = 0L, required = 100L, status = "complete"))
    expect_match(recorded[[1]]$estimates$method, "percentile", fixed = TRUE)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "DMAR::htmt(htmt_rows, blocks = construct_items, B = 100, conf_level = 0.9, seed = 8)", fixed = TRUE)
    # The same request again is reused, not rerun.
    session$setInputs(htmt_boot = 2)
    r2 <- htmt_ci()
    expect_identical(calls$B, 100L)
    expect_true(r2$reused)
    expect_identical(r2$table, r$table)
    expect_match(spec_text(output$htmt_ci_note), "completed earlier at", fixed = TRUE)
    # A changed seed, level or B is a new request.
    session$setInputs(htmt_seed = 9)
    session$setInputs(htmt_boot = 3)
    expect_identical(calls$B, c(100L, 100L))
    session$setInputs(conf_level = 0.95)
    session$setInputs(htmt_boot = 4)
    expect_match(htmt_ci()$error, "at least 200", fixed = TRUE)
    expect_identical(calls$level, c(0.9, 0.9))
    session$setInputs(htmt_B = 200L, htmt_boot = 5)
    expect_identical(calls$level, c(0.9, 0.9, 0.95))
    # B is preserved: 20000 runs as entered; 50 is refused without a call.
    session$setInputs(htmt_B = 20000)
    session$setInputs(htmt_boot = 6)
    expect_identical(htmt_ci()$B, 20000)
    expect_identical(calls$B[length(calls$B)], 20000L)
    before_refusal <- calls$B
    session$setInputs(htmt_B = 50)
    expect_match(spec_text(output$htmt_boot_plan), "at least 200", fixed = TRUE)
    expect_match(spec_text(output$htmt_boot_plan), "50 were entered", fixed = TRUE)
    session$setInputs(htmt_boot = 7)
    expect_match(htmt_ci()$error, "at least 200", fixed = TRUE)
    expect_match(spec_text(output$htmt_ci_note), "no replications were run", fixed = TRUE)
    expect_identical(calls$B, before_refusal)
  })
})

# ---- The calibration transformation bootstrap request ------------------------------

test_that("the calibration transformation bootstrap takes B, seed and level, records its counts, and reuses a matching request", {
  s <- spec_nine()
  shiny::testServer(mod_finalize_server, args = list(state = s), {
    session$setInputs(norm_name = "Cal", norm_population = "Demonstration only")
    session$setInputs(norm_freeze = 1)
    expect_true("Cal" %in% names(shiny::isolate(state$norm_references)))
    session$setInputs(norm_reference = "Cal", norm_B = 99, norm_seed = 5, norm_level = 0.9)
    plan <- spec_text(output$norm_transform_plan)
    expect_match(plan, "Method: Percentile bootstrap of the calibration constants", fixed = TRUE)
    expect_match(plan, "at the 90% level, the quantile method of sieve_norm_transform_limits", fixed = TRUE)
    expect_match(plan, "resampling unit: independent rows of the frozen calibration sample", fixed = TRUE)
    expect_match(plan, "With B = 99 the bootstrap is estimated to take", fixed = TRUE)
    session$setInputs(norm_transform_ci = 1)
    r <- norm_limits()
    expect_identical(r$B, 99L)
    expect_identical(r$seed, 5L)
    expect_true(r$seed_supplied)
    expect_identical(r$level, 0.9)
    expect_false(r$reused)
    expect_identical(r$counts$attempted, 99L)
    expect_true(all(r$table$requested == 99L))
    # The level column is conf_level, as the argument is, not level.
    expect_true(all(r$table$conf_level == 0.9))
    expect_true(all(r$table$seed == 5L))
    # The direct computation under the same request.
    ref <- shiny::isolate(state$norm_references)$Cal
    direct <- sieve_norm_transform_limits(ref, B = 99L, seed = 5, conf_level = 0.9)
    expect_equal(r$table$lower, direct$lower, tolerance = 1e-12)
    expect_equal(r$table$upper, direct$upper, tolerance = 1e-12)
    note <- spec_text(output$norm_transform_note)
    expect_match(note, "B = 99 resamples attempted, 99 to 99 successful per target", fixed = TRUE)
    expect_match(note, "seed = 5; 90% level; it took", fixed = TRUE)
    expect_no_error(output$norm_transform_limits)
    recorded <- Filter(function(a) identical(a$kind, "norm_transform_limits"), shiny::isolate(state$analyses))
    expect_length(recorded, 1L)
    expect_identical(recorded[[1]]$spec$B, 99L)
    expect_identical(recorded[[1]]$spec$seed, 5L)
    expect_identical(recorded[[1]]$estimates$counts$attempted, 99L)
    log <- paste(shiny::isolate(state$codelog), collapse = "\n")
    expect_match(log, "SIEVER::sieve_norm_transform_limits(norm_reference, B = 99L, seed = 5, conf_level = 0.9)", fixed = TRUE)
    # The same request is reused.
    session$setInputs(norm_transform_ci = 2)
    expect_true(norm_limits()$reused)
    expect_match(norm_feedback(), "was completed at", fixed = TRUE)
    expect_match(spec_text(output$norm_transform_note), "completed earlier at", fixed = TRUE)
    # Another seed is a new request, kept beside the first.
    session$setInputs(norm_seed = 6)
    session$setInputs(norm_transform_ci = 3)
    expect_false(norm_limits()$reused)
    expect_identical(norm_limits()$seed, 6L)
    expect_length(shiny::isolate(state$norm_transform_records), 2L)
    expect_true(all(vapply(state$norm_transform_records, function(x)
      identical(x$scientific_dataset_id, state$scientific_dataset_id), logical(1))))
    # A request below the minimum is moved up, never lowered. The minimum
    # is 99 and at least 2 / (1 - level) - 1 (sieve_norm_min_resamples()),
    # so the plan says it is the level's.
    session$setInputs(norm_B = 50, norm_seed = 7)
    expect_match(spec_text(output$norm_transform_plan),
      "50 was entered; the bootstrap needs at least 99 resamples at this level, so 99 will run.", fixed = TRUE)
    session$setInputs(norm_transform_ci = 4)
    expect_identical(norm_limits()$B, 99L)
    # At 99 percent the minimum is 199.
    session$setInputs(norm_level = 0.99)
    expect_match(spec_text(output$norm_transform_plan),
      "50 was entered; the bootstrap needs at least 199 resamples at this level, so 199 will run.", fixed = TRUE)
  })
})

# ---- Saved projects and replay carry the specification -----------------------------

test_that("the score specification survives a project round-trip and is bound by replay", {
  s <- spec_nine()
  sieve_set_score_specification(s, "Visual", method = "bartlett", min_count = 2L)
  sieve_set_score_specification(s, "Speed", method = "sum", min_prop = .5)
  sieve_log(s, "Scores", "scored <- scoring_policies")
  dest <- tempfile(fileext = ".siever.rds")
  on.exit(unlink(dest), add = TRUE)
  sieve_project_write(sieve_project_snapshot(s), dest)
  read <- sieve_project_read(dest)
  t <- sieve_new_state()
  sieve_project_restore(t, read)
  spec <- sieve_score_specification(t)
  expect_identical(spec$Visual$method, "bartlett")
  expect_identical(spec$Visual$required, 2L)
  expect_identical(spec$Speed$method, "sum")
  expect_identical(spec$Speed$required, 2L)
  expect_identical(spec$Textual$method, "mean")
  expect_identical(shiny::isolate(t$scoring_policies), shiny::isolate(s$scoring_policies))
  # A project whose stored method is not an operational score (here "pca";
  # a later version of SIEVER may store methods this one does not know)
  # opens with the method kept as saved, the mean in force, and the method
  # flagged for review, rather than refused whole by the reader.
  bad <- sieve_project_snapshot(s)
  bad$other$scoring_policies$Visual$method <- "pca"
  bad$keys$content <- sieve_content_key(bad)
  expect_error(sieve_project_write(bad, dest), NA)
  u <- sieve_new_state()
  sieve_project_open(u, sieve_project_read(dest))
  expect_identical(shiny::isolate(u$scoring_policies$Visual$method), "pca")
  expect_identical(sieve_score_specification(u)$Visual$method, "mean")
  expect_match(shiny::isolate(u$project$score_policy_issues)[["Visual"]], "\"pca\"", fixed = TRUE)
  # An invalid response minimum is still refused by the writer and the reader.
  worse <- sieve_project_snapshot(s)
  worse$other$scoring_policies$Visual$min_count <- -1
  worse$keys$content <- sieve_content_key(worse)
  expect_error(sieve_project_write(worse, dest), "scoring policies")
  unchecked <- tempfile(fileext = ".rds")
  on.exit(unlink(unchecked), add = TRUE)
  saveRDS(worse, unchecked)
  expect_error(sieve_project_read(unchecked), "scoring policies")
  # Replay: the recorded revision carries the specification, and the bound
  # environment exposes it.
  inputs <- sieve_replay_inputs(s)
  expect_identical(inputs$scoring_policies$Visual$method, "bartlett")
  store <- sieve_replay_bundle(s)
  key <- shiny::isolate(s$replay_steps[["1"]])
  env <- new.env(parent = baseenv())
  sieve_replay_bind(store, key, env)
  expect_identical(env$scoring_policies$Visual, list(method = "bartlett", min_count = 2L, min_prop = 0))
  expect_identical(env$scoring_policies$Speed$method, "sum")
  # The scoring syntax of the restored project reads the specification.
  code <- sieve_scoring_code(shiny::isolate(t$meta), values = "processed", policies = shiny::isolate(t$scoring_policies))$code
  expect_match(code, "Visual (3 items; Bartlett factor score from score_fit; at least 2 answered)", fixed = TRUE)
  expect_match(code, "prorated to the sum over every item", fixed = TRUE)
  env <- new.env(parent = baseenv())
  env$dat <- shiny::isolate(t$items)
  expect_no_error(eval(parse(text = code), envir = env))
  expect_equal(
    env$dat$score_Speed,
    sieve_composite_score(shiny::isolate(t$items), c("t10_addition", "t12_counting_groups_of_dots", "t13_straight_and_curved_capitals"), method = "sum", min_prop = .5)$scores,
    tolerance = 1e-12
  )
  expect_true(sum(is.finite(env$dat$score_Visual)) > 250)
})

# ---- A stored policy repaired on reading is said, not silent ----------------

test_that("a stored method this version does not know is read as the mean and flagged, never silently", {
  # "regresion" does not become the mean without a word.
  p <- sieve_score_policy_read(list(method = "regresion", min_count = 3L))
  expect_identical(p$method, "mean")
  expect_identical(p$min_count, 3L)
  expect_match(p$issue, "The stored scoring method \"regresion\" is not one this version of SIEVER knows", fixed = TRUE)
  expect_match(p$issue, "the mean of the answered items is in force", fixed = TRUE)
  # Invalid minimums are replaced by the defaults and said as well.
  p <- sieve_score_policy_read(list(method = "sum", min_count = -2, min_prop = 1.5))
  expect_identical(p$method, "sum")
  expect_identical(p$min_count, 1L)
  expect_identical(p$min_prop, 0)
  expect_match(p$issue, "minimum answered count \"-2\" is not valid, so 1 is in force", fixed = TRUE)
  expect_match(p$issue, "minimum answered proportion \"1.5\" is not valid, so 0 is in force", fixed = TRUE)
  # A policy without a method, and a valid one, are not flagged.
  expect_null(sieve_score_policy_read(list(min_count = 2L))$issue)
  expect_null(sieve_score_policy_read(list(method = "bartlett", min_count = 2L, min_prop = 0))$issue)
  issues <- sieve_score_policy_issues(list(Visual = list(min_count = 2L), Speed = list(method = "eap")))
  expect_identical(names(issues), "Speed")
})

test_that("the operational score card says which stored policy needs review until one is kept", {
  s <- spec_nine()
  s$scoring_policies <- list(Speed = list(method = "eap", min_count = 1L, min_prop = 0))
  shiny::testServer(mod_reliab_server, args = list(state = s), {
    note <- spec_text(output$policy_note)
    expect_match(note, "Needs review: Speed: The stored scoring method \"eap\"", fixed = TRUE)
    session$setInputs(policy_construct = "Speed", policy_method = "mean", policy_count = 1, policy_prop = 0)
    session$setInputs(policy_apply = 1)
    expect_identical(shiny::isolate(state$scoring_policies$Speed)$method, "mean")
    expect_false(grepl("Needs review", spec_text(output$policy_note), fixed = TRUE))
  })
})

test_that("opening a project with a stored method this version does not know opens it and says so", {
  s <- spec_nine()
  s$scoring_policies <- list(Speed = list(method = "eap", min_count = 1L, min_prop = 0))
  path <- tempfile(fileext = ".siever.rds")
  on.exit(unlink(path), add = TRUE)
  sieve_project_write(sieve_project_snapshot(s), path)
  opened <- sieve_new_state()
  sieve_project_open(opened, sieve_project_read(path), source = basename(path))
  issues <- shiny::isolate(opened$project$score_policy_issues)
  expect_identical(names(issues), "Speed")
  expect_match(issues[["Speed"]], "\"eap\"", fixed = TRUE)
  # The stored value is kept as saved: nothing is rewritten on opening.
  expect_identical(shiny::isolate(opened$scoring_policies$Speed$method), "eap")
  shiny::testServer(mod_ingest_server, args = list(state = opened), {
    expect_match(spec_text(output$score_policy_issues), "Scoring specifications to review.", fixed = TRUE)
    expect_match(spec_text(output$score_policy_issues), "Speed: The stored scoring method \"eap\"", fixed = TRUE)
  })
})

test_that("an exported specification and scoring code carry a repaired policy's stored method and its repair", {
  # The card flags the repair, and the specification table and the
  # generated code say so too, rather than saying the declared rules were
  # followed and computing a mean without a word.
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- data.frame(
    item = names(d), label = names(d), construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
  policies <- list(Visual = list(method = "regresion", min_count = 2L), Textual = list(min_count = 1L))
  spec <- sieve_score_spec_build(meta, policies)
  expect_identical(spec$Visual$method, "mean")
  expect_identical(spec$Visual$requested_method, "regresion")
  # A policy without a method is the mean, and it is not flagged.
  expect_identical(spec$Textual$requested_method, NA_character_)
  expect_null(spec$Textual$issue)
  tab <- sieve_score_specification_table(d, spec)
  expect_identical(tab$requested_method, c("regresion", NA, NA))
  expect_match(tab$review[1L], "The stored scoring method \"regresion\" is not one this version of SIEVER knows", fixed = TRUE)
  expect_identical(tab$review[2:3], c(NA_character_, NA_character_))
  code <- sieve_score_specification_code(spec)
  expect_true(any(startsWith(code, "# PROVISIONAL: a stored scoring policy could not be used as saved.")))
  review <- grep("^# NEEDS REVIEW, Visual: ", code)
  expect_length(review, 1L)
  expect_match(code[review + 1L], "construct_scores[[\"Visual\"]]", fixed = TRUE)
  exported <- sieve_scoring_code(meta, values = "processed", policies = policies)
  expect_identical(names(exported$issues), "Visual")
  expect_match(exported$code, "## PROVISIONAL: a stored scoring policy could not be used as saved", fixed = TRUE)
  expect_match(exported$code, "## NEEDS REVIEW, Visual: The stored scoring method \"regresion\"", fixed = TRUE)
  expect_match(exported$code, "# Visual (3 items; mean of the answered items; at least 2 answered)\n# NEEDS REVIEW: ", fixed = TRUE)
  expect_false(grepl("Scores follow the declared minimum-response and weighting rules", exported$code, fixed = TRUE))
  # The repaired code still computes the mean that is in force.
  env <- new.env()
  env$dat <- d
  eval(parse(text = exported$code), env)
  expect_equal(env$dat$score_Visual, sieve_composite_score(d, paste0("x", 1:3), min_count = 2L)$scores, tolerance = 0)
  # Valid policies, and one without a method, export with no repairs.
  clean <- sieve_scoring_code(meta, values = "processed", policies = list(Textual = list(min_count = 1L)))
  expect_length(clean$issues, 0L)
  expect_match(clean$code, "## Scores follow the declared minimum-response and weighting rules below.", fixed = TRUE)
  expect_false(grepl("NEEDS REVIEW|PROVISIONAL", clean$code))
  expect_false(any(grepl("NEEDS REVIEW|PROVISIONAL", sieve_score_specification_code(sieve_score_spec_build(meta, list())))))
})

test_that("the norm table, a frozen reference, the methods draft and the reader say a repaired policy too", {
  # Beyond the scoring code: the norm definition, the methods draft, and
  # malformed stored policies.
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- data.frame(
    item = names(d), label = names(d), construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
    facet = NA_character_, type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
  policies <- list(Visual = list(method = "regresion", min_count = 2L))
  norms <- sieve_construct_norms(d, meta, policies)
  expect_identical(names(norms$review), "Visual")
  expect_identical(norms$definition$Visual$requested_method, "regresion")
  expect_match(norms$table$review[norms$table$construct == "Visual"], "\"regresion\"", fixed = TRUE)
  expect_true(all(is.na(norms$table$review[norms$table$construct != "Visual"])))
  # The scores are the mean in force, unchanged.
  expect_equal(norms$scores$Visual, sieve_composite_score(d, paste0("x", 1:3), min_count = 2L)$scores, tolerance = 0)
  # A clean policy set has no review column and no repairs.
  clean <- sieve_construct_norms(d, meta)
  expect_false("review" %in% names(clean$table))
  expect_length(clean$review, 0L)
  # Definitions compare without the stored method and the repair, which are
  # not part of a definition's identity.
  expect_identical(sieve_norm_definition_read(norms$definition), sieve_norm_definition_read(sieve_norm_definition(meta, list(Visual = list(min_count = 2L)))))
  # A reference that scores later cohorts is not frozen on a repaired policy.
  expect_error(
    sieve_norm_freeze(d, meta, policies, name = "Calibration", population = "Grade 7 and 8 students"),
    "the stored scoring policy of Visual needs review", fixed = TRUE
  )
  expect_s3_class(sieve_norm_freeze(d, meta, name = "Calibration", population = "Grade 7 and 8 students"), "siever_norm_reference")
  a <- sieve_analyze("norms", data = d, meta = meta, policies = policies)
  expect_identical(a$status$code, "ok")
  expect_match(a$status$message, "Needs review: Visual: ", fixed = TRUE)
  # The methods draft names the policy as provisional, not as specified.
  spec <- sieve_score_spec_build(meta, policies)
  draft <- sieve_draft_scores_text(d, spec)
  expect_match(draft, "Visual, the mean of the answered items with at least 2 of 3 answered (n = 301 eligible) (provisional: the stored policy, method \"regresion\",", fixed = TRUE)
  expect_false(grepl("provisional", sieve_draft_scores_text(d, sieve_score_spec_build(meta, list())), fixed = TRUE))
  # Malformed stored policies are flagged, read by exact name.
  expect_match(sieve_score_policy_read("sum")$issue, "is not a list of scoring rules", fixed = TRUE)
  p <- sieve_score_policy_read(list(methods = "sum", min_count = 2L))
  expect_identical(p$method, "mean")
  expect_identical(p$requested, NA_character_)
  expect_match(p$issue, "The stored field \"methods\" is not part of a scoring policy and was not used", fixed = TRUE)
  expect_match(sieve_score_policy_read(list(method = "sum", min_count = 2.5))$issue, "minimum answered count \"2.5\" is not valid", fixed = TRUE)
  typed <- sieve_score_policy_read(list(method = factor("sum")))
  expect_identical(typed$method, "mean")
  expect_match(typed$issue, "is not a single method name (it holds \"sum\" as a factor)", fixed = TRUE)
  # The policies SIEVER writes, and one without a method, are read without a
  # flag.
  expect_null(sieve_score_policy_read(NULL)$issue)
  expect_null(sieve_score_policy_read(list(min_count = 2L, min_prop = 0))$issue)
  expect_null(sieve_score_policy_read(list(method = "sum", min_count = 2L, min_prop = .5, weights = c(x1 = 1, x2 = 2)))$issue)
})

test_that("the exported scoring code parses whatever a construct name holds", {
  # A backslash in a construct name must not make the score_name_map line,
  # written with dput(), unparseable.
  d <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
  names_held <- c("A\\B \"quoted\"", "Textual")
  meta <- data.frame(
    item = names(d), label = names(d), construct = rep(names_held, each = 3L),
    status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
  x <- sieve_scoring_code(meta, values = "processed")
  env <- new.env()
  env$dat <- d
  expect_no_error(eval(parse(text = x$code), env))
  expect_identical(env$score_name_map, x$name_map)
  expect_equal(env$dat[[x$name_map[[names_held[1L]]]]], rowMeans(d[, 1:3]), tolerance = 1e-12)
})

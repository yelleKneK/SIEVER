# Scientific contracts of the inference workflows. The oracles are
# direct engine calls, hand-worked scores, and independent model identities.
psych20_data <- function(n = 300L) withr::with_seed(113, {
  a <- stats::rnorm(n)
  b <- .3 * a + sqrt(.91) * stats::rnorm(n)
  item <- function(f, l) l * f + sqrt(1 - l * l) * stats::rnorm(n)
  data.frame(
    a1 = item(
      a,
      .8
    ),
    a2 = item(
      a,
      .75
    ),
    a3 = item(
      a,
      .7
    ),
    b1 = item(
      b,
      .8
    ),
    b2 = item(
      b,
      .75
    ),
    b3 = item(
      b,
      .7
    ),
    x = a + .4 * stats::rnorm(n),
    y = .6 * a + .8 * stats::rnorm(n)
  )
})
psych20_meta <- function(d) {
  m <- sieve_build_meta(d, names(d)[1:6], list(A = names(d)[1:3], B = names(d)[4:6]))
  m$type <- "continuous"
  m
}
psych20_state <- function(d = psych20_data()) {
  fixture <- list(data = d, item_cols = names(d)[1:6],
    name = "Study fixture", theory_map = NULL, labels = NULL, load_code = character())
  siever_test_study(fixture, labels = FALSE, theory = FALSE, name = NULL, configure = function(s) {
    s$meta <- psych20_meta(d)
    roles <- sieve_declared_roles(d, names(d)[1:6])
    roles$role[roles$variable %in% c("x", "y")] <- "criterion"
    if ("cl" %in% roles$variable) roles$role[roles$variable == "cl"] <- "group"
    s$roles <- roles
  })
}

test_that("scientific literals preserve hostile names and remain runnable", {
  values <- c('a"b', 'a\\b', 'two\nlines', "'quoted'", 'x);stop("bad")')
  code <- paste0("c(", paste(sieve_r_literal(values), collapse = ", "), ")")
  expect_identical(eval(parse(text = code)), values)
  d <- psych20_data()
  names(d)[8] <- values[1]
  run <- sieve_run_structural(d[, 1:6], d, psych20_meta(d), values[1], predictors = "A")
  expect_true(sieve_has_solution(run))
  env <- new.env(parent = globalenv())
  env$items <- d[, 1:6]
  env$d <- d
  eval(parse(text = sieve_sem_code(run$spec)), env)
  expect_equal(lavaan::coef(env$sem_fit), lavaan::coef(run$fit), tolerance = 1e-10)
})

test_that("categorical score names are actual ML and EBM, and continuous scores stay unchanged", {
  d <- psych20_data()
  m <- psych20_meta(d)
  f <- sieve_run_cfa(sieve_cfa_spec(m), d)$fit
  expect_equal(sieve_factor_scores(f, "Bartlett"), as.data.frame(lavaan::lavPredict(f, method = "Bartlett", assemble = TRUE)))
  for (nm in names(d)[1:6]) d[[nm]] <- as.integer(cut(d[[nm]], c(-Inf, -.6, 0, .6, Inf)))
  m$type <- "ordered"
  f <- sieve_run_cfa(sieve_cfa_spec(m), d)$fit
  expect_identical(sieve_score_method(f, "Bartlett")$method, "ML")
  expect_identical(sieve_score_method(f, "regression")$method, "EBM")
  actual <- sieve_factor_scores(f, "Bartlett")
  scored <- DMAR::factor_scores(f)
  direct <- matrix(scored$.derived_bartlett, nrow = nrow(actual), dimnames = dimnames(as.matrix(actual)))
  expect_equal(as.matrix(actual), direct, ignore_attr = TRUE)
  lavaan_scores <- as.matrix(lavaan::lavPredict(f, method = "ML", assemble = TRUE))
  common <- is.finite(lavaan_scores) & is.finite(as.matrix(actual))
  expect_equal(as.matrix(actual)[common], lavaan_scores[common])
  expect_true(anyNA(actual))
  expect_equal(sieve_factor_scores(f, "regression"), as.data.frame(lavaan::lavPredict(f, method = "EBM", assemble = TRUE)))
  expect_match(sieve_score_method(f, "regression")$label, "modal")
})

test_that("current criteria cannot be paired with a stale measurement score model", {
  st <- psych20_state()
  shiny::testServer(mod_validity_server, args = list(state = st), {
    # The sidebar control writes the score
    # specification of every construct (it is read after the first flush,
    # as in a browser), and the message of a model from before an edit
    # names the page's Update button.
    session$setInputs(criteria = "y", htmt_on = FALSE, marker = "")
    session$setInputs(score_method = "bartlett")
    session$setInputs(fit_go = 1)
    expect_true(nrow(crit_df()) > 0)
    old <- construct_scores()$scores
    raw <- shiny::isolate(state$raw)
    raw$y <- -raw$y
    state$raw <- raw
    x <- shiny::isolate(state$items)
    x$a1 <- -x$a1
    state$items <- x
    sieve_confirm_types(state)
    session$flushReact()
    expect_identical(demand$status()$state, "stale")
    expect_null(construct_scores()$scores)
    expect_match(construct_scores()$error, "from before.*Update")
    expect_error(crit_df(), "from before.*Update")
    session$setInputs(score_method = "mean")
    expect_true(nrow(crit_df()) > 0)
    expect_false(identical(old, construct_scores()$scores))
  })
})

test_that("residual pairs reproduce Bentler scaling and correlation differences separately", {
  d <- psych20_data()
  f <- sieve_run_cfa(sieve_cfa_spec(psych20_meta(d)), d)$fit
  r <- sieve_residual_pairs(f)
  direct <- lavaan::lavResiduals(f, type = "cor.bentler", se = TRUE, zstat = TRUE)
  ij <- which(upper.tri(direct$cov), arr.ind = TRUE)
  expect_equal(r$table$residual, as.numeric(direct$cov[ij]), tolerance = 1e-12)
  expect_equal(r$table$z, as.numeric(direct$cov.z[ij]), tolerance = 1e-12)
  cr <- sieve_residual_pairs(f, "cor.bollen")
  expect_equal(cr$table$residual,
    as.numeric((stats::cov2cor(lavaan::lavInspect(
      f,
      "sampstat"
    )$cov) - stats::cov2cor(lavaan::lavInspect(
      f,
      "implied"
    )$cov))[ij]),
    tolerance = 1e-12
  )
  ui <- as.character(mod_cfa_ui("x"))
  expect_false(grepl("average absolute standardized residual", ui, fixed = TRUE))
  expect_match(ui, "root mean square residual", fixed = TRUE)
})

test_that("the item pair lookup of a large pool is served from the server", {
  # Forty-six items have 1,035 pairs; a pool of 68 items has 2,278.
  # A select of 1,000 options or more sent whole to the browser is slow, and
  # Shiny warns of it; the lookup is a server-side selectize.
  d <- withr::with_seed(211, {
    f <- stats::rnorm(200L)
    as.data.frame(sapply(seq_len(46L), function(j) .6 * f + .8 * stats::rnorm(200L)))
  })
  names(d) <- sprintf("x%02d", seq_len(46L))
  m <- sieve_build_meta(d, names(d), list(A = names(d)))
  m$type <- "continuous"
  run <- sieve_run_cfa(sieve_cfa_spec(m), d)
  expect_true(sieve_has_solution(run))
  pairs <- sieve_residual_pairs(run$fit)$table
  expect_identical(nrow(pairs), 1035L)
  module <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      sieve_cfa_evidence_server(input, output, session,
        state = NULL, parent = shiny::reactive(run), current = function() TRUE
      )
    })
  }
  shiny::testServer(module, {
    expect_no_warning(session$flushReact())
    # The last pair, chosen from the search, is looked up without error.
    session$setInputs(residual_pair = "1035")
    expect_no_error(output$residual_lookup)
  })
})

test_that("latent correlation boundary uses the specified equality and engine reference test", {
  d <- psych20_data()
  f <- sieve_run_cfa(sieve_cfa_spec(psych20_meta(d)), d)$fit
  r <- sieve_latent_bound(f, c("A", "B"), .5)
  expect_null(r$error)
  oracle <- lavaan::cfa("A =~ a1+a2+a3\nB =~ b1+b2+b3\nA ~~ .5*B", data = d, estimator = "ML", missing = "fiml", std.lv = TRUE) # Auto: ML with FIML
  expect_equal(lavaan::lavInspect(r$fit, "implied")$cov, lavaan::lavInspect(oracle, "implied")$cov, tolerance = 1e-6)
  constrained <- lavaan::standardizedSolution(r$fit)
  expect_equal(constrained$est.std[constrained$lhs == "A" & constrained$op == "~~" & constrained$rhs == "B"],
    .5,
    tolerance = 1e-9
  )
  expect_equal(as.numeric(r$test$Chisq), as.numeric(lavaan::lavTestLRT(f, oracle)$Chisq), tolerance = 1e-6)
  expect_error(sieve_latent_bound(f, c("A", "B"), 1), "strictly inside")
  expect_error(sieve_latent_bound(f, c("A", "A"), .5), "distinct")
  # A marker-identified ML parent: the boundary is a nonlinear constraint,
  # under which lavaan cannot compute the Browne residual statistic that
  # lavaan 0.7-3 lists by default; the refit asks only for the standard
  # statistic, and no warning about it is recorded.
  marker <- lavaan::cfa("A =~ a1+a2+a3\nB =~ b1+b2+b3", data = d, estimator = "ML")
  m <- sieve_latent_bound(marker, c("A", "B"), .5)
  expect_null(m$error)
  expect_false(any(grepl("browne", m$warnings, ignore.case = TRUE)))
  expect_identical(lavaan::lavInspect(m$fit, "options")$test, "standard")
})

test_that("model-linked unit weights use the selected CFA residual covariance", {
  d <- psych20_data()
  model <- "A =~ a1+a2+a3\nB =~ b1+b2+b3\na1 ~~ a2"
  f <- lavaan::cfa(model, data = d, estimator = "MLR", missing = "fiml", std.lv = TRUE)
  r <- sieve_model_composite(f)
  # Reference values computed by semTools::compRelSEM(obs.var = TRUE,
  # tau.eq = FALSE, return.total = TRUE) for this fit and stored here.
  expect_equal(r$table$estimate, c(0.72447044168088359, 0.80910513812120211, 0.80667890547375709), tolerance = 1e-10)
  expect_equal(r$weights, stats::setNames(rep(1, 6), names(d)[1:6]))
  expect_true(all(is.na(r$table$lower)))
  expect_match(r$interval_reason, "not requested")
  # Custom weights are computed, with compRelSEM(W = ...) values stored.
  weighted <- sieve_model_composite(f, weights = c(2, 1, 1, 1, 1, 1))
  expect_null(weighted$error)
  expect_equal(weighted$table$estimate, c(0.68953724080047973, 0.80910513812120211, 0.77588475368080068), tolerance = 1e-10)
})

test_that("operational policy calculations are exact for missing patterns and content", {
  d <- data.frame(a = c(1, 2, NA, NA), b = c(3, NA, 4, NA), c = c(NA, 6, 8, NA))
  r <- sieve_score_policy(d, names(d), min_count = 2, min_prop = .5)
  expect_equal(r$scores, c(2, 4, 6, NA))
  expect_identical(r$answered, c(2, 2, 2, 0))
  expect_identical(r$eligible, c(TRUE, TRUE, TRUE, FALSE))
  weighted <- sieve_score_policy(d, names(d), weights = c(a = 2, b = 1, c = 1))
  expect_equal(weighted$scores, c(5 / 3, 10 / 3, 6, NA))
  m <- data.frame(item = names(d), facet = c("F1", "F1", "F2"))
  s <- sieve_score_policy_sensitivity(d, names(d), counts = c(1, 2, 3), meta = m)
  expect_identical(s$table$scored, c(3L, 3L, 0L))
  expect_equal(s$cases$facets_represented, c(1, 2, 2, 0))
  expect_error(sieve_score_policy(d, names(d), min_prop = 1.1), "min_prop")
})

test_that("score uncertainty is the actual engine output and determinacy has its own target", {
  d <- psych20_data()
  f <- sieve_run_cfa(sieve_cfa_spec(psych20_meta(d)), d)$fit
  r <- sieve_factor_score_record(f, data = d)
  direct <- lavaan::lavPredict(f, method = "Bartlett", assemble = TRUE, se = "standard", acov = "standard", fsm = TRUE)
  expect_null(r$reason)
  expect_true(nrow(r$uncertainty) == 600)
  se <- attr(direct, "se")[[1]]
  expect_equal(r$uncertainty$se[1:300], rep(as.numeric(se[1, 1]), 300), tolerance = 1e-12)
  # The determinacy of the scores requested: the
  # default Bartlett scores report the Bartlett-score determinacy.
  expect_equal(r$determinacy, lavaan::lavInspect(
    f, if (identical(r$method$method, "Bartlett")) "fs_determinacy_Bartlett" else "fs.det"
  ))
  expect_identical(r$observed, rep(6, 300))
})

test_that("congeneric AIPE calls the matching existing factor-analytic engine", {
  l <- c(.8, .7, .6, .75)
  p <- 1 - l * l
  r <- sieve_plan_congeneric(l, p, width = .1)
  oracle <- DMAR::ss_aipe_reliability(
    model = "Congeneric",
    type = "Factor Analytic",
    width = .1,
    n_items = 4,
    true_factor_loading = l,
    true_residual_var = p,
    conf_level = .95
  )
  expect_equal(r$result, oracle, tolerance = 1e-12)
  expect_equal(r$sensitivity$N[2], oracle$value[oracle$term == "necessary_N"])
  expect_true(r$sensitivity$N[1] > r$sensitivity$N[3])
  expect_error(sieve_plan_congeneric(l, p[-1]), "same length")
})

test_that("R squared intervals use joint residual uncertainty and preserve the engine point value", {
  d <- psych20_data()
  f <- lavaan::sem("y ~ x", data = d, estimator = "MLR", missing = "fiml")
  r <- sieve_R2_intervals(f)
  ss <- lavaan::standardizedSolution(f)
  b <- ss[ss$op == "~", ]
  expect_equal(r$R2, unname(lavaan::lavInspect(f, "r2")["y"]), tolerance = 1e-12)
  expect_equal(r$R2, b$est.std^2, tolerance = 1e-12)
  # lavaan numerically differentiates two equivalent standardizations.
  expect_lt(abs(r$se - 2 * abs(b$est.std) * b$se), 1e-8)
  # The limits are formed on the logit scale (Browne, 1982) and lie inside (0, 1).
  half <- stats::qnorm(.975) * r$se / (r$R2 * (1 - r$R2))
  expect_equal(r$lower, stats::plogis(stats::qlogis(r$R2) - half), tolerance = 1e-12)
  expect_equal(r$upper, stats::plogis(stats::qlogis(r$R2) + half), tolerance = 1e-12)
  expect_true(r$lower > 0 && r$upper < 1 && r$lower < r$R2 && r$R2 < r$upper)
})

# The columns sieve_R2_intervals() returns in every state.
r2_schema <- c(
  "group", "model_level", "outcome", "R2", "se", "lower", "upper", "conf_level",
  "method", "reason", "outcome_scale"
)

test_that("R-squared intervals give one row for each outcome in each group, from the group's own variances", {
  hs <- lavaan::HolzingerSwineford1939
  m <- "visual =~ x1+x2+x3; textual =~ x4+x5+x6; speed =~ x7+x8+x9; textual ~ visual; speed ~ visual + textual"
  fit <- lavaan::sem(m, data = hs, group = "school", std.lv = TRUE)
  r <- sieve_R2_intervals(fit)
  # A grouped model's R-squared list is matched to each outcome by name.
  expect_identical(names(r), r2_schema)
  expect_identical(nrow(r), 4L)
  expect_identical(r$group, rep(c("Pasteur", "Grant-White"), each = 2L))
  expect_true(all(is.na(r$model_level)))
  expect_identical(r$outcome, rep(c("textual", "speed"), 2L))
  engine <- lavaan::lavInspect(fit, "r2", drop.list.single.group = FALSE)
  expect_equal(r$R2, unname(c(engine[[1]][c("textual", "speed")], engine[[2]][c("textual", "speed")])), tolerance = 1e-12)
  expect_identical(r$reason, rep("", 4L))
  expect_identical(r$outcome_scale, rep("Latent variable", 4L))
  expect_identical(r$conf_level, rep(.95, 4L))
  # The standard error is that of each group's standardized residual variance.
  ss <- lavaan::standardizedSolution(fit)
  resid <- ss[ss$op == "~~" & ss$lhs == ss$rhs & ss$lhs %in% c("textual", "speed"), ]
  expect_equal(r$se, resid$se[match(paste(r$outcome, rep(1:2, each = 2L)), paste(resid$lhs, resid$group))], tolerance = 1e-12)
  # Logit-scale limits: for Pasteur's speed (R2 = .132) a symmetric lower
  # limit would be -.0068, below any attainable R-squared.
  z <- stats::qnorm(.975)
  expect_equal(r$lower, stats::plogis(stats::qlogis(r$R2) - z * r$se / (r$R2 * (1 - r$R2))), tolerance = 1e-12)
  expect_equal(r$upper, stats::plogis(stats::qlogis(r$R2) + z * r$se / (r$R2 * (1 - r$R2))), tolerance = 1e-12)
  expect_true(all(r$lower > 0 & r$upper < 1))
  expect_lt(r$R2[2] - z * r$se[2], 0)
  expect_match(r$method, "logit scale (Browne, 1982)", fixed = TRUE)
  # Equal unstandardized paths across groups still give each group its own
  # R-squared (marker identification, so the factor variances differ).
  equal <- lavaan::sem(m, data = hs, group = "school", group.equal = "regressions")
  e <- sieve_R2_intervals(equal)
  pe <- lavaan::parameterEstimates(equal)
  expect_equal(pe$est[pe$op == "~" & pe$group == 1], pe$est[pe$op == "~" & pe$group == 2], tolerance = 1e-12)
  engine <- lavaan::lavInspect(equal, "r2", drop.list.single.group = FALSE)
  expect_equal(e$R2, unname(c(engine[[1]][c("textual", "speed")], engine[[2]][c("textual", "speed")])), tolerance = 1e-12)
  expect_gt(abs(e$R2[1] - e$R2[3]), 1e-3)
  expect_identical(sieve_R2_intervals(equal, conf_level = .90)$conf_level, rep(.90, 4L))
})

test_that("R-squared intervals give one row for each level of a two-level model", {
  m <- "level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1 + x2
  level: 2
    fb =~ y1 + y2 + y3
    fb ~ w1"
  fit <- lavaan::sem(m, data = lavaan::Demo.twolevel, cluster = "cluster")
  r <- sieve_R2_intervals(fit)
  # The within and cluster R-squared lists are matched to each outcome.
  expect_identical(names(r), r2_schema)
  expect_identical(r$model_level, c("within", "cluster"))
  expect_true(all(is.na(r$group)))
  expect_identical(r$outcome, c("fw", "fb"))
  engine <- lavaan::lavInspect(fit, "r2", drop.list.single.group = FALSE)
  expect_equal(r$R2, unname(c(engine$within["fw"], engine$cluster["fb"])), tolerance = 1e-12)
  # For the between-level R-squared (.018) a symmetric interval would be
  # [-.022, .058].
  expect_lt(r$R2[2], .05)
  expect_true(all(r$lower > 0 & r$upper < 1))
  # A level without a regression says so in its own row.
  within_only <- lavaan::sem("level: 1
    fw =~ y1 + y2 + y3
    fw ~ x1 + x2
  level: 2
    fb =~ y1 + y2 + y3", data = lavaan::Demo.twolevel, cluster = "cluster")
  w <- sieve_R2_intervals(within_only)
  expect_identical(names(w), r2_schema)
  expect_identical(w$model_level, c("within", "cluster"))
  expect_identical(w$outcome, c("fw", NA))
  expect_true(is.na(w$R2[2]) && is.na(w$lower[2]))
  expect_match(w$reason[2], "no variable on another in level cluster", fixed = TRUE)
})

test_that("R-squared intervals keep one schema and say why when there is no interval", {
  hs <- lavaan::HolzingerSwineford1939
  # No structural outcome: one row with the reason, not NULL.
  cfa <- lavaan::cfa("visual =~ x1+x2+x3; textual =~ x4+x5+x6", data = hs, std.lv = TRUE)
  none <- sieve_R2_intervals(cfa)
  expect_identical(names(none), r2_schema)
  expect_identical(nrow(none), 1L)
  expect_true(is.na(none$outcome) && is.na(none$R2) && is.na(none$lower))
  expect_match(none$reason, "no `~` line", fixed = TRUE)
  expect_match(none$reason, "communality", fixed = TRUE)
  # An inadmissible fit: the same columns, with the fit's own status.
  items <- paste0("x", 1:3)
  S <- matrix(c(1, .8, .8, .8, 1, .3, .8, .3, 1), 3L, dimnames = list(items, items))
  bad <- suppressWarnings(lavaan::cfa("f =~ x1+x2+x3", sample.cov = S, sample.nobs = 500L, std.lv = TRUE))
  expect_identical(sieve_fit_status(bad)$code, "inadmissible")
  refused <- sieve_R2_intervals(bad)
  expect_identical(names(refused), r2_schema)
  expect_identical(nrow(refused), 1L)
  expect_match(refused$reason, "R-squared intervals need an admissible solution.", fixed = TRUE)
  expect_match(refused$reason, sieve_fit_status(bad)$message, fixed = TRUE)
  # An R-squared the model fixes has no sampling interval.
  fixed <- sieve_R2_intervals(lavaan::sem("visual =~ x1+x2+x3; textual =~ x4+x5+x6; textual ~ 0*visual",
    data = hs, std.lv = TRUE
  ))
  expect_identical(names(fixed), r2_schema)
  expect_identical(fixed$R2, 0)
  expect_true(is.na(fixed$lower) && is.na(fixed$upper) && is.na(fixed$method))
  expect_identical(fixed$reason, "R-squared is fixed at 0 by the model, so it has no sampling interval.")
})

test_that("the SEM tab shows the R-squared table with its interval columns", {
  d <- psych20_data()
  st <- psych20_state(d)
  seen <- new.env(parent = emptyenv())
  real_datatable <- sieve_dt
  testthat::local_mocked_bindings(sieve_dt = function(data, ...) {
    if (is.data.frame(data) && "R2" %in% names(data)) {
      seen$r2 <- data
      seen$colnames <- list(...)$colnames
    }
    real_datatable(data, ...)
  }, .package = "SIEVER")
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(outcomes = "y", predictors = "A", fit_go = 1)
    expect_true(sieve_has_solution(path_run()))
    invisible(output$r2_tbl)
    # A single-group, single-level model shows no group or level column.
    expect_identical(names(seen$r2), setdiff(r2_schema, c("group", "model_level")))
    expect_identical(seen$colnames, c(
      "Outcome", "R-squared", "SE", "CI lower", "CI upper", "Confidence level", "Method", "Reason", "Outcome scale"
    ))
    expect_identical(path_run()$estimates$R2_intervals$outcome, "y")
    expect_true(seen$r2$lower > 0 && seen$r2$upper < 1)
  })
})

test_that("the SEM tab's R-squared table gives the reason when there is no R-squared to show", {
  d <- psych20_data()
  st <- psych20_state(d)
  refusal <- sieve_R2_intervals(lavaan::cfa("A =~ a1+a2+a3", data = d, std.lv = TRUE))
  seen <- new.env(parent = emptyenv())
  real_datatable <- sieve_dt
  # The table is served by DataTables' server side, so its rows are read
  # from the data frame handed to it.
  testthat::local_mocked_bindings(
    sieve_R2_intervals = function(fit, ...) refusal,
    sieve_dt = function(data, ...) {
      if (is.data.frame(data) && identical(names(data), "Note")) seen$note <- data$Note
      real_datatable(data, ...)
    },
    .package = "SIEVER"
  )
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(outcomes = "y", predictors = "A", fit_go = 1)
    expect_true(sieve_has_solution(path_run()))
    invisible(output$r2_tbl)
    expect_identical(seen$note, refusal$reason)
    expect_match(seen$note, "it has no `~` line", fixed = TRUE)
  })
})

# A two-group mediation model: one label for each
# path, so each path is constrained equal across the schools.
shared_mediation <- "visual =~ x1 + x2 + x3
  textual =~ x4 + x5 + x6
  speed =~ x7 + x8 + x9
  textual ~ a * visual
  speed ~ b * textual + visual
  ab := a * b"

test_that("a defined effect whose labels are shared across groups is evaluated in each group", {
  fit <- suppressWarnings(lavaan::sem(shared_mediation, data = lavaan::HolzingerSwineford1939,
    std.lv = TRUE, group = "school"
  ))
  r <- sieve_indirect_mc(fit, B = 2000L, seed = 1)
  expect_null(r$error)
  # One labeled row per group, each from that group's own parameters.
  expect_identical(nrow(r$table), 2L)
  expect_identical(names(r$table), c("effect", "group", "est.std", "ci.lower", "ci.upper"))
  expect_identical(r$table$effect, c("ab", "ab"))
  expect_identical(r$table$group, c("Pasteur", "Grant-White"))
  expect_identical(rownames(r$table), c("ab.g1", "ab.g2"))
  ss <- suppressWarnings(lavaan::standardizedSolution(fit))
  per_group <- vapply(1:2, function(g) {
    ss$est.std[ss$label == "a" & ss$group == g] * ss$est.std[ss$label == "b" & ss$group == g]
  }, numeric(1))
  expect_equal(unname(r$table$est.std), per_group, tolerance = 1e-12)
  expect_gt(abs(diff(per_group)), 1e-3)
  expect_true(all(r$table$ci.lower < r$table$est.std & r$table$est.std < r$table$ci.upper))
  expect_identical(colnames(r$samples), c("a.g1", "b.g1", "a.g2", "b.g2", "ab.g1", "ab.g2"))
  expect_equal(unname(r$samples[, "ab.g2"]), unname(r$samples[, "a.g2"] * r$samples[, "b.g2"]))
  # lavaan's ambiguity warning reaches the result, and the method says how
  # each group was evaluated.
  expect_true(any(grepl("ambiguous", r$warnings, fixed = TRUE)))
  expect_match(r$method, "evaluated in each group from that group's standardized parameters", fixed = TRUE)
})

test_that("a label naming more than one parameter otherwise is refused with the parameters it names", {
  hs <- lavaan::HolzingerSwineford1939
  # One label on two paths of one group is refused, not read as the first
  # path's value.
  within <- lavaan::sem("visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9
    textual ~ a * visual
    speed ~ a * textual + visual
    aa := a * a", data = hs, std.lv = TRUE)
  r <- sieve_indirect_mc(within, B = 2000L, seed = 1)
  expect_null(r$table)
  expect_match(r$error, "The defined effect aa uses the label a, which names 2 parameters (textual ~ visual, std.all", fixed = TRUE)
  expect_match(r$error, "speed ~ textual, std.all", fixed = TRUE)
  expect_match(r$error, "not unique (lavaan reports the first occurrence only)", fixed = TRUE)
  expect_match(r$error, "c(a1, a2) * visual", fixed = TRUE)
  expect_true(any(grepl("ambiguous", r$warnings, fixed = TRUE)))
  # A cross-group label combined with a label of one group has no value in every group.
  mixed <- suppressWarnings(lavaan::sem("visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9
    textual ~ a * visual
    speed ~ c(b1, b2) * textual + visual
    ab := a * b1", data = hs, std.lv = TRUE, group = "school"))
  r <- sieve_indirect_mc(mixed, B = 2000L, seed = 1)
  expect_null(r$table)
  expect_match(r$error, "The defined effect ab combines a, constrained equal across the groups", fixed = TRUE)
  expect_match(r$error, "with b1, which names a parameter of one group only (speed ~ textual in group Pasteur", fixed = TRUE)
})

test_that("group-specific labels keep their effects and draws", {
  fit <- lavaan::sem("visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9
    textual ~ c(a1, a2) * visual
    speed ~ c(b1, b2) * textual + visual
    ab1 := a1 * b1
    ab2 := a2 * b2", data = lavaan::HolzingerSwineford1939, std.lv = TRUE, group = "school")
  r <- sieve_indirect_mc(fit, B = 2000L, seed = 1)
  expect_null(r$error)
  expect_identical(r$warnings, character(0))
  expect_identical(r$table$effect, c("ab1", "ab2"))
  # Evaluated once, as written: the group column is NA.
  expect_true(all(is.na(r$table$group)))
  # Reference values under the same seed, stored here.
  expect_equal(unname(unlist(r$table[c("est.std", "ci.lower", "ci.upper")])),
    c(0.114013995582448, 0.0406240190118135, -0.00505429170249057, -0.104466011659519, 0.242403046205859, 0.175307908117137),
    tolerance = 1e-12
  )
  ss <- lavaan::standardizedSolution(fit)
  expect_equal(unname(r$table$est.std), ss$est.std[ss$op == ":="], tolerance = 1e-12)
})

test_that("declared categorical outcomes and missing-covariate choices reproduce direct SEM", {
  d <- psych20_data()
  d$binary <- as.numeric(d$y > 0)
  m <- psych20_meta(d)
  r <- sieve_run_structural(d[, 1:6], d, m, "binary", predictors = "A", outcome_types = c(binary = "binary"))
  expect_true(sieve_has_solution(r))
  expect_identical(r$spec$ordered, "binary")
  args <- sieve_spec_args(r$spec)
  args$data <- d
  oracle <- do.call(lavaan::sem, args)
  expect_equal(lavaan::coef(r$fit), lavaan::coef(oracle), tolerance = 1e-10)
  bad <- sieve_run_structural(d[, 1:6], d, m, "binary", predictors = "A", outcome_types = c(binary = "binary"), estimator = "mlr")
  expect_match(bad$error, "categorical outcome")
  d$x[1:30] <- NA_real_
  for (strategy in c("engine_default", "random", "fiml_x")) {
    r <- sieve_run_structural(d[, 1:6], d, m, "A", antecedents = "x", exogenous_missing = strategy)
    expect_true(sieve_has_solution(r))
    args <- sieve_spec_args(r$spec)
    args$data <- d
    oracle <- suppressWarnings(do.call(lavaan::sem, args))
    expect_equal(lavaan::coef(r$fit), lavaan::coef(oracle), tolerance = 1e-10)
    expect_equal(sum(lavaan::lavInspect(r$fit, "nobs")), if (strategy == "engine_default") 270 else 300)
    env <- new.env(parent = globalenv())
    env$items <- d[, 1:6]
    env$d <- d
    suppressWarnings(eval(parse(text = sieve_sem_code(r$spec)), env))
    expect_equal(lavaan::coef(env$sem_fit), lavaan::coef(r$fit), tolerance = 1e-10)
  }
})

test_that("Monte Carlo defined effects keep the fitted covariance and actual standardized target", {
  d <- psych20_data()
  f <- lavaan::sem("a1 ~ a*x\ny ~ b*a1+c*x\nind := a*b", data = d, estimator = "MLR", missing = "fiml")
  r <- sieve_indirect_mc(f, B = 2000, seed = 113)
  expect_null(r$error)
  # Reference values computed by semTools::monteCarloCI(nRep = 2000,
  # standardized = TRUE) under the same seed and stored here: the same draws
  # give the same limits.
  expect_equal(unname(unlist(r$table[c("est.std", "ci.lower", "ci.upper")])),
    c(0.17943976549126917, 0.081630226568832107, 0.28392714946680386),
    tolerance = 1e-10
  )
  expect_identical(rownames(r$table), "ind")
  expect_identical(r$rng$seed, 113L)
  expect_identical(r$B, 2000L)
})

test_that("operational scoring permits constant and all-missing later cohorts", {
  d <- data.frame(a = c(2, 2, NA), b = c(3, 3, NA))
  expect_equal(sieve_score_policy(d, names(d))$scores, c(2.5, 2.5, NA))
  expect_equal(sieve_score_policy(d[1, , drop = FALSE], names(d))$scores, 2.5)
  expect_true(is.na(sieve_score_policy(d[3, , drop = FALSE], names(d))$scores))
  expect_error(sieve_score_policy_sensitivity(d, names(d), counts = c(1, 1.5)), "positive integers")
})

test_that("sampling declarations and actual cluster values invalidate held inference", {
  st <- psych20_state()
  raw <- shiny::isolate(st$raw)
  raw$cluster <- rep(1:30, each = 10)
  st$raw <- raw
  roles <- sieve_declared_roles(raw, names(shiny::isolate(st$items)))
  roles$role[roles$variable %in% c("x", "y")] <- "criterion"
  roles$role[roles$variable == "cluster"] <- "group"
  st$roles <- roles
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(fit_go = 1)
    expect_identical(demand$status()$state, "current")
    iid <- cfa_run()$fit
    state$sampling_design <- list(unit = "clustered", cluster = "cluster")
    session$flushReact()
    expect_false(identical(demand$status()$state, "current"))
    session$setInputs(fit_go = 2)
    expect_identical(demand$status()$state, "current")
    expect_match(lavaan::lavInspect(cfa_run()$fit, "options")$se, "cluster")
    old <- cfa_run()$fit
    raw <- shiny::isolate(state$raw)
    raw$cluster <- rev(raw$cluster)
    raw$cluster[1:2] <- c(
      2,
      1
    )
    state$raw <- raw
    session$flushReact()
    expect_false(identical(demand$status()$state, "current"))
    expect_null(cfa_run()$fit)
    expect_true(length(shiny::isolate(state$analyses)) >= 2L)
  })
  expect_match(sieve_sampling_problem(st), "independent rows")
})

test_that("model-linked composites refuse ordinary row bootstrap for clustered fits", {
  d <- psych20_data()
  d$cl <- rep(1:30, each = 10)
  f <- lavaan::cfa("A =~ a1+a2+a3\nB =~ b1+b2+b3", data = d, cluster = "cl", estimator = "MLR", std.lv = TRUE)
  r <- sieve_model_composite(f, B = 100, seed = 3)
  expect_match(r$interval_reason, "clustered")
  expect_true(all(is.na(r$table$lower)))
  expect_null(r$draws)
})

test_that("score guidance treats lavaan's normalized ml missing option as FIML", {
  st <- psych20_state()
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(score_method = "Bartlett", scores_go = 1)
    expect_identical(lavaan::lavInspect(score_fit(), "options")$missing, "ml")
    note <- paste(as.character(unlist(output$scores_note)), collapse = " ")
    expect_match(note, "full-information maximum likelihood", fixed = TRUE)
    expect_false(grepl("not a FIML fit", note, fixed = TRUE))
  })
})

test_that("clustered CFA and SEM replay exact eligible rows and cluster columns", {
  d <- psych20_data()
  d$cl <- rep(1:30, each = 10)
  d$cl[c(3, 17)] <- NA_integer_
  s <- psych20_state(d)
  s$sampling_design <- list(unit = "clustered", cluster = "cl")
  prepared <- sieve_sampling_apply(sieve_cfa_spec(psych20_meta(d)), s, d[, 1:6])
  r <- sieve_run_model(prepared$spec, prepared$data)
  env <- new.env(parent = globalenv())
  env$items <- d[, 1:6]
  env$d <- d
  invisible(utils::capture.output(eval(parse(text = sieve_cfa_code(r$spec)), env)))
  expect_equal(lavaan::coef(env$cfa_fit), lavaan::coef(r$fit), tolerance = 1e-10)
  expect_equal(sum(lavaan::lavInspect(env$cfa_fit, "nobs")), 298)
  r <- sieve_run_structural(d[, 1:6], d, psych20_meta(d), "y", predictors = "A", sampling_state = s)
  eval(parse(text = sieve_sem_code(r$spec)), env)
  expect_equal(lavaan::coef(env$sem_fit), lavaan::coef(r$fit), tolerance = 1e-10)
  expect_equal(sum(lavaan::lavInspect(env$sem_fit, "nobs")), 298)
})

test_that("outcome-scale labels follow the fitted latent and observed model identities", {
  d <- psych20_data()
  continuous <- lavaan::sem("y ~ x", data = d, estimator = "MLR", missing = "fiml")
  before <- lavaan::standardizedSolution(continuous)
  label <- sieve_outcome_scale(continuous, "y")
  expect_identical(label$kind, "observed_continuous")
  expect_identical(label$outcome_scale, "Observed continuous outcome")
  expect_true(is.na(label$link))
  interval <- sieve_R2_intervals(continuous)
  expect_identical(interval$outcome_scale, label$outcome_scale)
  expect_equal(interval$R2, unname(lavaan::lavInspect(continuous, "r2")["y"]), tolerance = 1e-12)
  expect_identical(lavaan::standardizedSolution(continuous), before)
  latent <- lavaan::sem("A =~ a1+a2+a3\nB =~ b1+b2+b3\nB ~ A", data = d, estimator = "MLR", missing = "fiml", std.lv = TRUE)
  expect_identical(sieve_outcome_scale(latent, "B")$kind, "latent")
  expect_identical(sieve_R2_intervals(latent)$outcome_scale, "Latent variable")
  expect_error(sieve_outcome_scale(latent, "not_modeled"), "from the fitted model")
  # Numerical category codes alone do not make an outcome categorical.
  d$binary <- as.numeric(d$y > 0)
  treated_continuous <- lavaan::sem("binary ~ x", data = d, estimator = "MLR")
  expect_identical(sieve_outcome_scale(treated_continuous, "binary")$kind, "observed_continuous")
})

test_that("binary and ordinal path and R-squared scales retain direct engine estimates", {
  d <- psych20_data()
  d$binary <- as.numeric(d$y > 0)
  d$ordinal <- as.integer(cut(d$y, c(-Inf, -.5, 0, .5, Inf)))
  for (nm in c("binary", "ordinal")) {
    type <- if (nm == "binary") "binary" else "ordered"
    r <- sieve_run_structural(d[, 1:6], d, psych20_meta(d), nm, predictors = "A", outcome_types = stats::setNames(type, nm))
    expect_true(sieve_has_solution(r))
    args <- sieve_spec_args(r$spec)
    args$data <- d
    oracle <- do.call(lavaan::sem, args)
    before <- lavaan::coef(r$fit)
    label <- sieve_outcome_scale(r$fit, nm)
    expect_identical(label$kind, "underlying_categorical")
    expect_identical(label$link, "probit")
    expect_identical(label$outcome_scale, "Underlying categorical response (probit)")
    expect_equal(lavaan::coef(r$fit), lavaan::coef(oracle), tolerance = 1e-12)
    expect_equal(lavaan::standardizedSolution(r$fit), lavaan::standardizedSolution(oracle), tolerance = 1e-12)
    interval <- sieve_R2_intervals(r$fit)
    expect_identical(interval$outcome_scale, label$outcome_scale)
    expect_equal(interval$R2, unname(lavaan::lavInspect(oracle, "r2")[nm]), tolerance = 1e-12)
    expect_identical(lavaan::coef(r$fit), before)
  }
  ui <- as.character(mod_sem_ui("s"))
  expect_match(ui, "not category-probability changes", fixed = TRUE)
  expect_match(ui, "observed category codes", fixed = TRUE)
})

test_that("SEM renders the fitted outcome scale in both paths and R-squared tables", {
  d <- psych20_data()
  d$binary <- as.numeric(d$y > 0)
  st <- psych20_state(d)
  roles <- sieve_roles_table(d, d[, 1:6], NULL)
  roles$role[roles$variable == "binary"] <- "criterion"
  st$roles <- roles
  seen <- new.env(parent = emptyenv())
  seen$paths <- NULL
  seen$r2 <- NULL
  real_datatable <- sieve_dt
  testthat::local_mocked_bindings(sieve_dt = function(data, ...) {
    if (is.data.frame(data) && all(c("Outcome", "Predictor", "outcome_scale") %in% names(data))) seen$paths <- data
    if (is.data.frame(data) && all(c("outcome", "R2", "outcome_scale") %in% names(data))) seen$r2 <- data
    real_datatable(data, ...)
  }, .package = "SIEVER")
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_sem_server, args = list(state = st), {
    do.call(session$setInputs, stats::setNames(list("binary"), paste0("outcome_type_", sieve_input_key("binary"))))
    session$setInputs(outcomes = "binary", predictors = "A", fit_go = 1)
    expect_true(sieve_has_solution(path_run()))
    text <- function(x) paste(as.character(unlist(x)), collapse = " ")
    expect_match(text(output$paths_tbl), "Outcome scale", fixed = TRUE)
    expect_match(text(output$r2_tbl), "Outcome scale", fixed = TRUE)
    expect_identical(seen$paths$outcome_scale, "Underlying categorical response (probit)")
    expect_identical(seen$r2$outcome_scale, "Underlying categorical response (probit)")
    expect_equal(seen$paths$beta,
      lavaan::standardizedSolution(path_run()$fit)$est.std[lavaan::standardizedSolution(path_run()$fit)$op == "~"],
      tolerance = 1e-12
    )
    csv <- tempfile(fileext = ".csv")
    utils::write.csv(seen$r2, csv, row.names = FALSE)
    exported <- utils::read.csv(csv, stringsAsFactors = FALSE)
    expect_identical(exported$outcome_scale, seen$r2$outcome_scale)
    expect_equal(exported$R2, seen$r2$R2, tolerance = 1e-12)
    expect_identical(path_run()$estimates$R2_intervals$outcome_scale, "Underlying categorical response (probit)")
    log <- paste(state$codelog, collapse = "\n")
    expect_match(log, "sieve_outcome_scale", fixed = TRUE)
  })
})


test_that("an unused lavaan link placeholder cannot relabel its fitted WLS response", {
  d <- psych20_data()
  d$y <- as.integer(cut(d$y, c(-Inf, -.5, 0, .5, Inf)))
  default <- lavaan::sem("y ~ x", data = d, ordered = "y", estimator = "WLSMV")
  expect_warning(ignored <- lavaan::sem("y ~ x", data = d, ordered = "y", estimator = "WLSMV", link = "logit"),
    'link will be set to "probit"',
    fixed = TRUE
  )
  expect_identical(lavaan::lavInspect(ignored, "options")$link, "logit")
  expect_equal(lavaan::coef(ignored), lavaan::coef(default), tolerance = 1e-12)
  expect_identical(sieve_outcome_scale(ignored, "y")$link, "probit")
})

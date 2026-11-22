irt_evidence_fixture <- function() {
  withr::with_seed(1, {
    theta <- stats::rnorm(600)
    data <- as.data.frame(lapply(c(.65, .8, .9, .7), function(a) {
      cut(a * theta + stats::rnorm(length(theta)), breaks = c(-Inf, -.5, .6, Inf), labels = FALSE)
    }))
    names(data) <- paste0("item", 1:4)
    list(data = data, grm = DMAR::irt_grm(data, metric = "normal_ogive"))
  })
}

test_that("IRT delta locations retain the loading-threshold covariance term", {
  V <- matrix(c(.0025, -.0012, -.0012, .01), 2L)
  lambda <- .6
  tau <- .4
  got <- sieve_irt_delta_parameters(lambda, tau, "item1", 1L, V, 1L, 2L)
  # Independent analytic expression for var(tau/lambda), including covariance.
  expected <- tau^2 / lambda^4 * V[1, 1] + V[2, 2] / lambda^2 - 2 * tau / lambda^3 * V[1, 2]
  expect_equal(got$table$b_se^2, expected, tolerance = 1e-13)
  expect_false(isTRUE(all.equal(got$table$b_se^2, tau^2 / lambda^4 * V[1, 1] + V[2, 2] / lambda^2)))
  transform <- function(x) c(x[1] / sqrt(1 - x[1]^2), x[2] / x[1])
  x <- c(lambda, tau)
  h <- 1e-6
  finite <- vapply(seq_along(x), function(i) {
    up <- down <- x
    up[i] <- up[i] + h
    down[i] <- down[i] - h
    (transform(up) - transform(down)) / (2 * h)
  }, numeric(2))
  expect_equal(unname(got$jacobian), finite, tolerance = 1e-8)
  expect_equal(unname(got$covariance), finite %*% V %*% t(finite), tolerance = 1e-8)
  reflected <- sieve_irt_delta_parameters(-lambda, tau, "item1", 1L,
    diag(c(-1, 1)) %*% V %*% diag(c(-1, 1)), 1L, 2L,
    sign = -1
  )
  expect_equal(reflected$table, got$table, tolerance = 1e-13)
})

test_that("weak and boundary loadings cannot acquire plausible finite location intervals", {
  V <- diag(c(.05^2, .1^2))
  weak <- sieve_irt_delta_parameters(.03, .4, "weak", 1L, V, 1L, 2L)
  expect_match(weak$table$status, "includes zero")
  expect_true(is.na(weak$table$b_lower))
  improper <- sieve_irt_delta_parameters(1, .4, "improper", 1L, V, 1L, 2L)
  expect_true(is.na(improper$table$a))
  expect_true(is.na(improper$table$b_upper))
  boundary <- sieve_irt_delta_parameters(.98, .4, "near boundary", 1L, V, 1L, 2L)
  expect_match(boundary$table$status, "variance boundary")
  expect_true(is.na(boundary$table$a_lower))
})

test_that("IRT parameter intervals agree with the stored fitted covariance and retain point estimates", {
  fx <- irt_evidence_fixture()
  grm <- fx$grm
  before <- grm
  result <- sieve_irt_parameter_evidence(grm)
  expect_identical(result$status, "completed")
  expect_equal(result$table$a, grm$a, tolerance = 1e-12)
  expect_equal(result$table$b, grm$b, tolerance = 1e-12)
  fit <- attr(grm, "fit")
  V <- lavaan::lavInspect(fit, "vcov.std.all")
  i <- match("theta=~item1", colnames(V))
  j <- match("item1|t1", colnames(V))
  l <- grm$lambda[1L]
  t <- grm$tau[1L]
  expected <- t^2 / l^4 * V[i, i] + V[j, j] / l^2 - 2 * t / l^3 * V[i, j]
  expect_equal(result$table$b_se[1L]^2, expected, tolerance = 1e-12)
  expect_identical(grm, before)
  no_fit <- grm
  attr(no_fit, "fit") <- NULL
  expect_identical(sieve_irt_parameter_evidence(no_fit)$status, "unavailable")
})

test_that("joint parameter information draws are reproducible, counted, and preserve the caller RNG", {
  fx <- irt_evidence_fixture()
  set.seed(8)
  before <- .Random.seed
  a <- sieve_irt_information_bands(fx$grm, theta = c(-1, 0, 1), B = 50L, seed = 9)
  expect_identical(.Random.seed, before)
  b <- sieve_irt_information_bands(fx$grm, theta = c(-1, 0, 1), B = 50L, seed = 9)
  expect_identical(a$table, b$table)
  expect_identical(a$attempted, a$used + a$failed)
  expect_identical(a$status, "completed")
  direct <- DMAR::irt_information(grm = fx$grm, trait_value = c(-1, 0, 1))
  expect_equal(a$table$information, direct$test_information, tolerance = 1e-14)
  expect_equal(a$table$conditional_se, direct$se, tolerance = 1e-14)
  expect_true(all(a$table$information_lower <= a$table$information_upper))
  expect_match(a$method, "not a bootstrap", fixed = TRUE)
})

test_that("IRT residual correlations reproduce the supported lavaan residual definition", {
  fx <- irt_evidence_fixture()
  got <- sieve_irt_residual_pairs(fx$grm)
  expected <- lavaan::lavResiduals(attr(fx$grm, "fit"), type = "cor.bentler", zstat = FALSE)$cov
  for (i in seq_len(nrow(got))) expect_equal(got$residual_correlation[i],
    expected[
      got$item1[i],
      got$item2[i]
    ],
    tolerance = 1e-14
  )
  expect_match(attr(got, "method"), "not Yen Q3", fixed = TRUE)
})

test_that("bivariate expected cells obey independence and reproduce exact analytic counts", {
  zero <- sieve_irt_rectangle(-Inf, 0, -Inf, 0, 0)
  expect_equal(zero$probability, .25, tolerance = 1e-9)
  positive <- sieve_irt_rectangle(-Inf, 0, -Inf, 0, .5)
  # Closed form for a centered bivariate standard normal quadrant.
  expect_equal(positive$probability, .25 + asin(.5) / (2 * pi), tolerance = 1e-9)
  fx <- irt_evidence_fixture()
  table <- sieve_irt_bivariate_table(fx$grm, fx$data, "item1", "item2")
  expect_equal(sum(table$expected), nrow(fx$data), tolerance = 1e-7)
  expect_identical(sum(table$observed), as.numeric(nrow(fx$data)))
  direct <- table(fx$data$item1, fx$data$item2)
  expect_identical(table$observed, as.numeric(direct))
  missing <- fx$data
  missing$item4[1:5] <- NA
  reduced <- sieve_irt_bivariate_table(fx$grm, missing, "item1", "item2")
  expect_identical(unique(reduced$n), nrow(missing) - 5L)
  expect_error(sieve_irt_bivariate_table(fx$grm, fx$data, "item1", "item1"), "distinct")
})

test_that("normal ogive category probabilities and empirical traces have honest separate axes", {
  fx <- irt_evidence_fixture()
  theta <- c(-2, 0, 2)
  probabilities <- sieve_irt_category_probabilities(fx$grm, "item1", theta)
  p <- fx$grm[fx$grm$item == "item1", ]
  a <- p$a[1L]
  b <- p$b
  manual <- cbind(
    1 - stats::pnorm(a * (theta - b[1L])),
    stats::pnorm(a * (theta - b[1L])) - stats::pnorm(a * (theta - b[2L])), stats::pnorm(a * (theta - b[2L]))
  )
  expect_equal(unname(probabilities), manual, tolerance = 1e-14)
  expect_equal(rowSums(probabilities), rep(1, length(theta)), tolerance = 1e-14)
  expect_true(all(probabilities >= 0 & probabilities <= 1))
  empirical <- sieve_irt_empirical_traces(fx$grm, fx$data, "item1", bins = 4L)
  expect_true(all(empirical$count <= empirical$n))
  expect_equal(
    as.numeric(tapply(
      empirical$proportion,
      empirical$group,
      sum
    )),
    rep(
      1,
      length(unique(empirical$group))
    ),
    tolerance = 1e-14
  )
  expect_true(all(empirical$lower <= empirical$proportion & empirical$upper >= empirical$proportion))
  # Its grouping variable must not contain the focal item's response.
  changed <- fx$data
  changed$item1 <- rev(changed$item1)
  again <- sieve_irt_empirical_traces(fx$grm, changed, "item1", bins = 4L)
  expect_identical(
    again[
      ,
      c(
        "group",
        "rest_mean",
        "rest_min",
        "rest_max",
        "n"
      )
    ],
    empirical[
      ,
      c(
        "group",
        "rest_mean",
        "rest_min",
        "rest_max",
        "n"
      )
    ]
  )
})

test_that("IRT evidence panels retain exact records and reject a stale parent without refitting", {
  fx <- irt_evidence_fixture()
  s <- sieve_new_state()
  s$raw <- s$items <- fx$data
  s$meta <- sieve_build_meta(fx$data, names(fx$data), stats::setNames(rep("Trait", ncol(fx$data)), names(fx$data)))
  m <- shiny::isolate(s$meta)
  m$type <- "ordered"
  s$meta <- m
  s$roles <- sieve_declared_roles(fx$data, names(fx$data))
  s$confirmed <- TRUE
  s$estimator <- "WLSMV"
  shiny::isolate(sieve_new_dataset(s))
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  original <- DMAR::irt_grm
  testthat::local_mocked_bindings(irt_grm = function(...) {
    calls$n <- calls$n + 1L
    original(...)
  }, .package = "DMAR")
  shiny::testServer(mod_irt_server, args = list(state = s), {
    session$setInputs(
      scope = "__all__",
      metric = "normal_ogive",
      estimator = "WLSMV",
      theta_min = -2,
      theta_max = 2,
      theta_n = 11,
      fit_go = 1
    )
    session$setInputs(evidence_conf = .95, parameter_keep = 1)
    expect_identical(calls$n, 1L)
    expect_true(any(vapply(state$analyses, function(x) identical(x$kind, "IRT parameter uncertainty"), logical(1))))
    session$setInputs(pair_item1 = "item1", pair_item2 = "item2", pair_go = 1)
    expect_match(output$pair_note$html, "item1 versus item2", fixed = TRUE)
    session$setInputs(trace_item = "item1", trace_bins = 3L, trace_go = 1)
    expect_match(output$trace_note$html, "not a latent score", fixed = TRUE)
    expect_identical(calls$n, 1L)
    before <- length(state$analyses)
    d <- state$items
    d$item1 <- rev(d$item1)
    state$items <- d
    state$raw <- d
    session$setInputs(pair_go = 2)
    expect_match(output$pair_note$html, paste(
      "This fitted model is from an earlier request.",
      "Update the graded response model before calculating a new diagnostic."
    ), fixed = TRUE)
    expect_identical(length(state$analyses), before)
    expect_identical(calls$n, 1L)
    t <- sieve_new_state()
    sieve_project_restore(t, sieve_project_snapshot(state))
    expect_identical(
      shiny::isolate(t$analyses[[length(t$analyses)]]$estimates),
      state$analyses[[length(state$analyses)]]$estimates
    )
  })
})

test_that("information bands enforce the confidence-level draw minimum and omitted-tail caution", {
  fx <- irt_evidence_fixture()
  expect_error(sieve_irt_information_bands(fx$grm, B = 198L, conf_level = .99), "at least 199")
  original <- DMAR::irt_information
  counter <- 0L
  testthat::local_mocked_bindings(irt_information = function(...) {
    counter <<- counter + 1L
    if (counter %% 2L == 0L) stop("A deliberately unavailable information draw")
    original(...)
  }, .package = "DMAR")
  out <- sieve_irt_information_bands(fx$grm, theta = c(-1, 0, 1), B = 100L, seed = 901)
  expect_identical(out$status, "completed")
  expect_identical(out$used, 50L)
  expect_identical(out$failed, 50L)
  expect_match(out$caution, "unusable draw share reaches the interval tail area")
})

# A diagnostic computed on request is compared with its own controls as
# well as with the parent fit,
# so another pair, item, bin count or level never leaves it as current.
test_that("an IRT diagnostic is from before once its own controls change, and current again when they return", {
  fx <- irt_evidence_fixture()
  s <- sieve_new_state()
  s$raw <- s$items <- fx$data
  s$meta <- sieve_build_meta(fx$data, names(fx$data), stats::setNames(rep("Trait", ncol(fx$data)), names(fx$data)))
  m <- shiny::isolate(s$meta)
  m$type <- "ordered"
  s$meta <- m
  s$roles <- sieve_declared_roles(fx$data, names(fx$data))
  s$confirmed <- TRUE
  s$estimator <- "WLSMV"
  shiny::isolate(sieve_new_dataset(s))
  shiny::testServer(mod_irt_server, args = list(state = s), {
    session$setInputs(scope = "__all__", metric = "normal_ogive", estimator = "WLSMV",
      theta_min = -2, theta_max = 2, theta_n = 11, fit_go = 1, evidence_conf = .95)
    session$setInputs(pair_item1 = "item1", pair_item2 = "item2", pair_go = 1)
    first <- output$pair_note$html
    expect_match(first, "item1 versus item2", fixed = TRUE)
    session$setInputs(pair_item2 = "item3")
    expect_match(output$pair_note$html, "computed with other settings than those now chosen", fixed = TRUE)
    session$setInputs(pair_item2 = "item2")
    expect_identical(output$pair_note$html, first)
    session$setInputs(trace_item = "item1", trace_bins = 3L, trace_go = 1)
    expect_match(output$trace_note$html, "not a latent score", fixed = TRUE)
    session$setInputs(trace_bins = 4L)
    expect_match(output$trace_note$html, "computed with other settings than those now chosen", fixed = TRUE)
    session$setInputs(trace_bins = 3L, evidence_conf = .90)
    expect_match(output$trace_note$html, "computed with other settings than those now chosen", fixed = TRUE)
  })
})

# ---- The discrimination metric --------------------------------------------------
# A DMAR::irt_grm(metric = "logistic") result is read in its own metric: read
# as if its a were normal ogive it would give category probabilities of
# neither model. The parameter table says which metric its a is in.

irt_metric_fixture <- function() {
  fx <- irt_evidence_fixture()
  fx$logistic <- withr::with_seed(1, DMAR::irt_grm(fx$data, metric = "logistic"))
  fx
}

test_that("category probabilities read the metric DMAR records, and are the same for both metrics", {
  fx <- irt_metric_fixture()
  expect_identical(attr(fx$logistic, "metric"), "logistic")
  theta <- seq(-3, 3, by = .25)
  for (item in paste0("item", 1:4)) {
    normal <- sieve_irt_category_probabilities(fx$grm, item, theta)
    logistic <- sieve_irt_category_probabilities(fx$logistic, item, theta)
    expect_equal(logistic, normal, tolerance = 1e-12)
    expect_lt(max(abs(logistic - normal)), 1e-12)
  }
  # The normal ogive model, written out with the logistic a divided by 1.702.
  p <- fx$logistic[fx$logistic$item == "item1", ]
  a <- p$a[1L] / 1.702
  manual <- cbind(
    1 - stats::pnorm(a * (theta - p$b[1L])),
    stats::pnorm(a * (theta - p$b[1L])) - stats::pnorm(a * (theta - p$b[2L])),
    stats::pnorm(a * (theta - p$b[2L]))
  )
  expect_equal(unname(sieve_irt_category_probabilities(fx$logistic, "item1", theta)), manual, tolerance = 1e-14)
  # A hand-built table without the attribute is read as normal ogive.
  plain <- as.data.frame(fx$grm)[, c("item", "category", "a", "b")]
  attr(plain, "metric") <- NULL
  expect_equal(sieve_irt_category_probabilities(plain, "item1", theta),
    sieve_irt_category_probabilities(fx$grm, "item1", theta), tolerance = 0)
  unknown <- fx$grm
  attr(unknown, "metric") <- "probit"
  expect_error(sieve_irt_category_probabilities(unknown, "item1", theta),
    "attr(grm, \"metric\") is \"probit\"; sieve_irt_category_probabilities() reads the two discrimination metrics",
    fixed = TRUE)
  attr(unknown, "metric") <- c("logistic", "normal_ogive")
  expect_error(sieve_irt_category_probabilities(unknown, "item1", theta), "reads the two discrimination metrics",
    fixed = TRUE)
})

test_that("parameter evidence names its metric and always gives the logistic a beside the normal ogive a", {
  fx <- irt_metric_fixture()
  normal <- sieve_irt_parameter_evidence(fx$grm)
  logistic <- sieve_irt_parameter_evidence(fx$logistic)
  shape <- c("item", "category", "a", "a_se", "a_lower", "a_upper", "a_logistic", "a_logistic_lower",
    "a_logistic_upper", "b", "b_se", "b_lower", "b_upper", "status")
  for (e in list(normal, logistic)) {
    expect_identical(names(e$table), shape)
    expect_identical(e$metric, "normal_ogive")
    expect_match(e$method, "(a in the normal ogive metric)", fixed = TRUE)
    expect_equal(e$table$a_logistic, 1.702 * e$table$a, tolerance = 1e-15)
    expect_equal(e$table$a_logistic_lower, 1.702 * e$table$a_lower, tolerance = 1e-15)
    expect_equal(e$table$a_logistic_upper, 1.702 * e$table$a_upper, tolerance = 1e-15)
  }
  # One fitted model, one table, whichever metric DMAR reported.
  expect_equal(logistic$table, normal$table, tolerance = 1e-12)
  expect_equal(normal$table$a, fx$grm$a, tolerance = 1e-12)
  expect_equal(logistic$table$a_logistic, fx$logistic$a, tolerance = 1e-10)
  expect_match(logistic$reason, "the supplied DMAR::irt_grm() result reports a in the logistic metric, 1.702 times larger",
    fixed = TRUE)
  expect_false(grepl("logistic metric, 1.702", normal$reason, fixed = TRUE))
})

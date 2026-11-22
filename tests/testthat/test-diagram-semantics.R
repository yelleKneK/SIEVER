# The path diagrams: a direct effect is not drawn through its mediator, a
# composite model is not drawn as if it were valid, and a caption does not
# say the estimates are shown when the standardized values are undefined.

# Straight arrows (loadings and regressions) whose line between the two
# node centers enters the box of a node that is not one of their ends.
# Computed here from the node table alone, independently of the renderer.
edges_through_nodes <- function(spec) {
  n <- spec$nodes[spec$nodes$visible, , drop = FALSE]
  e <- spec$edges[spec$edges$visible & spec$edges$type %in% c("loading", "regression"), , drop = FALSE]
  bad <- character(0)
  along <- seq(0, 1, length.out = 401)
  for (r in seq_len(nrow(e))) {
    a <- n[n$id == e$from[r], ]
    b <- n[n$id == e$to[r], ]
    px <- a$x + along * (b$x - a$x)
    py <- a$y + along * (b$y - a$y)
    for (j in which(!n$id %in% c(e$from[r], e$to[r]))) {
      if (any(abs(px - n$x[j]) < n$w[j] / 2 & abs(py - n$y[j]) < n$h[j] / 2)) {
        bad <- c(bad, sprintf("%s -> %s through %s", e$from[r], e$to[r], n$id[j]))
      }
    }
  }
  bad
}

test_that("a direct effect is not drawn through the mediator", {
  skip_if_not_installed("lavaan")
  model <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed =~ x7 + x8 + x9
    textual ~ visual
    speed ~ visual + textual"
  fit <- lavaan::sem(model, lavaan::HolzingerSwineford1939)
  for (layout in c("left_right", "top_down")) {
    spec <- sieve_diagram_spec(fit, layout = layout)
    expect_identical(edges_through_nodes(spec), character(0), info = layout)
    # The three structural variables are not on one line: the mediator
    # stands off the line from the predictor to the outcome.
    at <- function(id) unlist(spec$nodes[spec$nodes$id == id, c("x", "y")])
    v <- at("visual")
    t <- at("textual")
    s <- at("speed")
    off_line <- abs((s[["x"]] - v[["x"]]) * (t[["y"]] - v[["y"]]) -
      (s[["y"]] - v[["y"]]) * (t[["x"]] - v[["x"]])) /
      sqrt(sum((s - v)^2))
    mediator <- spec$nodes[spec$nodes$id == "textual", ]
    expect_gt(off_line,
      max(
        mediator$w,
        mediator$h
      ) / 2,
      label = paste(
        "distance of the mediator from the direct line,",
        layout
      )
    )
    # Each regression still carries its own coefficient, equal to lavaan's.
    std <- lavaan::standardizedSolution(fit)
    reg <- spec$edges[spec$edges$type == "regression", ]
    for (r in seq_len(nrow(reg))) {
      want <- std$est.std[std$op == "~" & std$lhs == reg$to[r] & std$rhs == reg$from[r]]
      expect_equal(reg$value[r], want, tolerance = 1e-12, info = paste(reg$from[r], reg$to[r]))
    }
    # The two arrows that arrive at the outcome come from different directions.
    arrive <- reg[reg$to == "speed", ]
    angle <- vapply(arrive$from, function(id) {
      d <- at(id) - s
      atan2(d[["y"]], d[["x"]])
    }, numeric(1))
    expect_gt(abs(diff(angle)), 0.1)
  }
  # Display options do not move the nodes, so the clear paths hold whatever is shown.
  full <- sieve_diagram_spec(fit)
  bare <- sieve_diagram_update(full, residuals = FALSE, variances = FALSE)
  keep <- match(bare$nodes$id, full$nodes$id)
  expect_identical(bare$nodes[, c("x", "y")], full$nodes[keep, c("x", "y")], ignore_attr = TRUE)
})

test_that("a mediator among several predictors, and a longer chain, stay clear of the arrows that pass them", {
  skip_if_not_installed("lavaan")
  d <- lavaan::HolzingerSwineford1939
  # Three exogenous predictors, one mediator, one observed outcome.
  d$y <- as.numeric(scale(d$x9))
  d$a <- d$ageyr
  three <- lavaan::sem("
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    textual ~ visual + a + grade
    y ~ visual + a + grade + textual", d)
  for (layout in c("left_right", "top_down")) {
    expect_identical(edges_through_nodes(sieve_diagram_spec(three, layout = layout)), character(0),
      info = paste("three predictors,", layout)
    )
  }
  # A chain of four with every later variable regressed on every earlier one.
  chain <- lavaan::sem("
    x4 ~ x1
    x7 ~ x1 + x4
    x9 ~ x1 + x4 + x7", d)
  for (layout in c("left_right", "top_down")) {
    expect_identical(edges_through_nodes(sieve_diagram_spec(chain, layout = layout)), character(0),
      info = paste("chain,", layout)
    )
  }
})

test_that("a model with a composite is refused, not drawn without the weights that define it", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::sem("
    comp <~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    textual ~ comp", lavaan::HolzingerSwineford1939)
  # lavaan fit it: the three weights exist in the model.
  expect_identical(sum(lavaan::parTable(fit)$op == "<~"), 3L)
  spec <- sieve_diagram_spec(fit)
  expect_identical(spec$status$code, "failed")
  expect_match(spec$status$message, "composites")
  expect_identical(nrow(spec$nodes), 0L)
  expect_identical(nrow(spec$edges), 0L)
  file <- tempfile(fileext = ".pdf")
  expect_error(sieve_export_diagram(spec, file), class = "sieve_diagram_error")
  expect_false(file.exists(file))
})

test_that("an improper solution whose standardized values are undefined says so", {
  skip_if_not_installed("lavaan")
  # A covariance matrix that forces a negative factor variance: lavaan then
  # has no standardized value for that factor's loadings or variance.
  S <- matrix(0, 6, 6, dimnames = list(
    c("a1", "a2", "a3", "b1", "b2", "b3"),
    c("a1", "a2", "a3", "b1", "b2", "b3")
  ))
  S[1:3, 1:3] <- 0.5
  S[4:6, 4:6] <- -0.3
  diag(S) <- 1
  run <- sieve_capture(lavaan::cfa("A =~ a1 + a2 + a3\nB =~ b1 + b2 + b3",
    sample.cov = S, sample.nobs = 300
  ))
  fit <- run$value
  pe <- lavaan::parameterEstimates(fit)
  expect_lt(pe$est[pe$op == "~~" & pe$lhs == "B" & pe$rhs == "B"], 0)
  std <- lavaan::standardizedSolution(fit)
  undefined <- sum(is.na(std$est.std[std$lhs == "B" & std$op %in% c("=~", "~~") &
    (std$op == "=~" | std$rhs == "B")]))
  expect_gt(undefined, 0L)

  spec <- sieve_diagram_spec(fit)
  expect_identical(spec$status$code, "improper")
  caption <- spec$meta$caption
  expect_match(caption, "negative variance estimate for B")
  expect_match(caption, "undefined")
  expect_false(grepl("shown as they are", caption, fixed = TRUE))
  # The unstandardized view has every estimate, with its sign, and may say so.
  raw <- sieve_diagram_spec(fit, standardized = "none")
  var_b <- raw$edges[raw$edges$type == "variance" & raw$edges$from == "B", ]
  expect_lt(var_b$value, 0)
  expect_true(var_b$improper)
  expect_match(raw$meta$caption, "shown as they are")

  # Where the standardized value is undefined, the edge shows the
  # unstandardized estimate with its sign and says which metric it is in;
  # no drawn edge reads NA.
  e <- spec$edges[spec$edges$visible, ]
  expect_false(any(e$label_text == "NA"))
  pe <- lavaan::parameterEstimates(fit)
  b_var <- e[e$id == "B~~B", ]
  expect_true(is.na(b_var$value))
  expect_identical(
    b_var$label_text,
    paste(formatC(pe$est[pe$lhs == "B" & pe$op == "~~" & pe$rhs == "B"],
      format = "f", digits = 2
    ), "unstd.")
  )
  expect_match(b_var$label_text, "^-")
  b_load <- e[e$type == "loading" & e$from == "B", ]
  expect_true(all(is.na(b_load$value)))
  expect_true(all(grepl(" unstd\\.$", b_load$label_text)))
  # The marker loading of B was fixed at 1 and is written as that constant.
  expect_identical(b_load$label_text[b_load$fixed], "1 unstd.")
  expect_identical(
    b_load$label_text[!b_load$fixed],
    paste(formatC(b_load$est[!b_load$fixed], format = "f", digits = 2), "unstd.")
  )
  # Edges whose standardized value exists keep it: no unstd. mark there.
  a_load <- e[e$type == "loading" & e$from == "A", ]
  expect_false(any(grepl("unstd", a_load$label_text, fixed = TRUE)))
  expect_equal(a_load$value, std$est.std[std$op == "=~" & std$lhs == "A"], tolerance = 1e-12)
  expect_match(caption, "4 standardized values are undefined", fixed = TRUE)
  expect_match(caption, "marked unstd.", fixed = TRUE)
  # A fully standardized residual variance above 1 is out of range and is
  # flagged, like a negative variance.
  res_b <- spec$edges[spec$edges$id %in% c("b1~~b1", "b2~~b2", "b3~~b3"), ]
  expect_true(all(res_b$value > 1))
  expect_true(all(res_b$improper))
  expect_true(all(c("b1~~b1", "b2~~b2", "b3~~b3", "B~~B") %in% spec$status$details$improper$id))
  expect_true(all(spec$parameters$improper[spec$parameters$id %in% c("b1~~b1", "b2~~b2", "b3~~b3")]))
  # The unstandardized view flags the same parameters: the flag is a
  # property of the solution, not of the display.
  expect_identical(raw$edges$improper, spec$edges$improper)
  # A proper solution has no such flag.
  expect_false(any(sieve_diagram_spec(lavaan::cfa("A =~ a1 + a2 + a3",
    sample.cov = S[1:3, 1:3],
    sample.nobs = 300
  ))$edges$improper))
})

# ---- Caption semantics -----------------------------------------------------

rf_hs <- function() lavaan::HolzingerSwineford1939

rf_ordered_hs <- function(k = 3L) {
  d <- rf_hs()
  for (v in paste0("x", 1:6)) {
    d[[v]] <- as.integer(cut(d[[v]], stats::quantile(d[[v]], 0:k / k), include.lowest = TRUE))
  }
  d
}

test_that("the ordered-variable sentence is written from the residual variances of the group shown", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6", rf_ordered_hs(),
    ordered = TRUE, group = "school", std.lv = TRUE, parameterization = "theta",
    group.equal = c("thresholds", "loadings")
  )
  pt <- lavaan::parTable(fit)
  own <- pt$op == "~~" & pt$lhs == pt$rhs & pt$lhs %in% paste0("x", 1:6)
  # lavaan: residual variances fixed at 1 in group 1, free in group 2.
  expect_true(all(pt$free[own & pt$group == 1L] == 0L))
  expect_true(all(pt$free[own & pt$group == 2L] > 0L))
  g1 <- sieve_diagram_spec(fit, group = 1L, standardized = "none")
  g2 <- sieve_diagram_spec(fit, group = 2L, standardized = "none")
  expect_match(g1$meta$caption_parts[["categorical"]],
    "Ordered variables (x1, x2, x3, x4, x5, x6): coefficients refer to latent response variables (theta parameterization); their residual variances are fixed at 1.",
    fixed = TRUE
  )
  expect_match(g2$meta$caption_parts[["categorical"]],
    "their residual variances are estimated in this group.",
    fixed = TRUE
  )
  expect_false(grepl("fixed at 1", g2$meta$caption_parts[["categorical"]], fixed = TRUE))
  expect_false(grepl("Ordered indicators", g2$meta$caption, fixed = TRUE))
  # The edges agree with the sentence.
  e2 <- g2$edges[g2$edges$type == "variance" & g2$edges$from %in% paste0("e.x", 1:6), ]
  expect_true(all(e2$free > 0L) && !any(e2$fixed))
  pe <- lavaan::parameterEstimates(fit)
  expect_equal(e2$est, pe$est[pe$op == "~~" & pe$lhs == pe$rhs & pe$lhs %in% paste0("x", 1:6) &
    pe$group == 2L], tolerance = 1e-12)
  # Delta: implied, in both groups; single group theta: fixed at 1.
  delta <- lavaan::cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6", rf_ordered_hs(),
    ordered = TRUE, group = "school", std.lv = TRUE
  )
  expect_match(sieve_diagram_spec(delta, group = 2L)$meta$caption_parts[["categorical"]],
    "implied by the model, not estimated",
    fixed = TRUE
  )
  one <- lavaan::cfa("visual =~ x1 + x2 + x3", rf_ordered_hs(),
    ordered = TRUE, std.lv = TRUE,
    parameterization = "theta"
  )
  expect_match(sieve_diagram_spec(one)$meta$caption_parts[["categorical"]],
    "their residual variances are fixed at 1.",
    fixed = TRUE
  )
  # An ordered outcome is an ordered variable, not an indicator.
  d <- rf_hs()
  d$y <- as.integer(cut(d$x9, stats::quantile(d$x9, 0:3 / 3), include.lowest = TRUE))
  out <- lavaan::sem("textual =~ x4 + x5 + x6\ny ~ textual", d, ordered = "y")
  expect_match(
    sieve_diagram_spec(out)$meta$caption_parts[["categorical"]],
    "^Ordered variables \\(y\\)"
  )
})

test_that("the estimator is named by its lavaan shortcut, FIML is stated, and the model kind is right", {
  skip_if_not_installed("lavaan")
  two <- "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6"
  for (est in c("ML", "MLM", "MLMV", "MLMVS", "MLF", "MLR", "GLS", "WLS")) {
    fit <- lavaan::cfa(two, rf_hs(), estimator = est)
    expect_identical(sieve_diagram_spec(fit)$meta$estimator, est, info = est)
  }
  for (est in c("WLSMV", "WLSM", "WLSMVS", "ULSMV", "ULSM", "ULSMVS", "DWLS", "ULS")) {
    fit <- lavaan::cfa(two, rf_ordered_hs(), ordered = TRUE, estimator = est)
    expect_identical(sieve_diagram_spec(fit)$meta$estimator, est, info = est)
  }
  fiml <- sieve_diagram_spec(lavaan::cfa(two, rf_hs(), missing = "fiml"))
  expect_identical(fiml$meta$estimator, "ML")
  expect_match(fiml$meta$caption_parts[["model"]], "(ML, FIML, n = 301)", fixed = TRUE)
  plain <- sieve_diagram_spec(lavaan::cfa(two, rf_hs()))
  expect_match(plain$meta$caption_parts[["model"]], "(ML, n = 301)", fixed = TRUE)
  # An exploratory block is not a confirmatory model, and a growth model is
  # named as one.
  esem <- lavaan::sem(
    paste0(
      "efa('b1')*f1 =~ x1 + x2 + x3 + x4 + x5 + x6\n",
      "efa('b1')*f2 =~ x1 + x2 + x3 + x4 + x5 + x6"
    ), rf_hs(),
    rotation = "oblimin"
  )
  s <- sieve_diagram_spec(esem)
  expect_identical(s$meta$model_kind, "esem")
  expect_match(s$meta$caption_parts[["model"]], "^Exploratory structural equation model \\(rotated\\)")
  growth <- lavaan::growth(
    "i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4\ns =~ 0*t1 + 1*t2 + 2*t3 + 3*t4",
    lavaan::Demo.growth
  )
  g <- sieve_diagram_spec(growth)
  expect_identical(g$meta$model_kind, "growth")
  expect_match(g$meta$caption_parts[["model"]], "^Latent growth model")
})

test_that("a cross-loading fixed at zero does not change how the scaling is read", {
  skip_if_not_installed("lavaan")
  unit <- lavaan::cfa("visual =~ x1 + x2 + x3 + 0*x4\ntextual =~ x4 + x5 + x6", rf_hs(),
    std.lv = TRUE
  )
  pt <- lavaan::parTable(unit)
  expect_true(all(pt$free[pt$op == "~~" & pt$lhs == pt$rhs & pt$lhs %in% c("visual", "textual")] == 0L))
  s <- sieve_diagram_spec(unit)
  expect_identical(s$meta$identification_detail$scaling, c("unit_variance", "unit_variance"))
  expect_identical(s$meta$identification, "unit_variance")
  expect_match(s$meta$caption, "Each latent scale was set by fixing its variance to 1.", fixed = TRUE)
  marker <- lavaan::cfa("visual =~ x1 + x2 + x3 + 0*x4\ntextual =~ x4 + x5 + x6", rf_hs())
  m <- sieve_diagram_spec(marker)
  expect_identical(m$meta$identification, "marker")
  # The zero loading itself is still a fixed parameter, hidden as fixed at zero.
  z <- s$edges[s$edges$id == "visual=~x4", ]
  expect_true(z$fixed && !z$visible && identical(z$hidden_reason, "fixed_zero"))
  expect_match(s$meta$caption, "Fixed at zero and not drawn: visual =~ x4.", fixed = TRUE)
  # Where the scaling is something else (here a loading and the variance
  # both fixed), the caption points to the table.
  other <- sieve_capture(lavaan::cfa("visual =~ 1*x1 + x2 + x3\nvisual ~~ 1*visual", rf_hs()))$value
  expect_match(sieve_diagram_spec(other)$meta$caption,
    "see the parameter table for how each latent scale was set",
    fixed = TRUE
  )
})

test_that("intercepts fixed at zero are named when intercepts are requested", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6", rf_hs(),
    std.lv = TRUE,
    meanstructure = TRUE
  )
  s <- sieve_diagram_spec(fit, intercepts = TRUE)
  hidden <- s$edges[s$edges$id %in% c("visual~1", "textual~1"), ]
  expect_true(all(!hidden$visible & hidden$hidden_reason == "fixed_zero"))
  expect_setequal(s$meta$fixed_zero, c("visual~1", "textual~1"))
  expect_match(s$meta$caption, "Fixed at zero and not drawn: visual ~ 1, textual ~ 1.", fixed = TRUE)
  # Not when the mean structure is left to the table as a whole.
  plain <- sieve_diagram_spec(fit)
  expect_length(plain$meta$fixed_zero, 0L)
  expect_match(plain$meta$caption, "Intercepts and thresholds are in the parameter table", fixed = TRUE)
  # Requested on a model without a mean structure: the caption says so.
  none <- sieve_diagram_spec(lavaan::cfa("visual =~ x1 + x2 + x3", rf_hs(), std.lv = TRUE),
    intercepts = TRUE
  )
  expect_identical(sum(none$edges$type %in% c("intercept", "threshold")), 0L)
  expect_match(none$meta$caption, "Intercepts were requested, but the model has no mean structure.",
    fixed = TRUE
  )
})

test_that("constraints that are not simple equalities, and bounds, are disclosed", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::cfa("visual =~ l1*x1 + l2*x2 + l3*x3\ntextual =~ x4 + x5 + x6\nl1 == 2*l2\nl3 > 0.1",
    rf_hs(),
    std.lv = TRUE
  )
  pt <- lavaan::parTable(fit)
  expect_true(any(pt$op == "==" & pt$lhs == "l1" & pt$rhs == "2*l2"))
  # lavaan turns the simple inequality into a bound on the parameter.
  expect_identical(pt$lower[pt$label == "l3"], 0.1)
  expect_false(any(pt$op == ">"))
  s <- sieve_diagram_spec(fit)
  expect_identical(
    s$meta$constraints[, c("lhs", "op", "rhs", "kind")],
    data.frame(
      lhs = "l1", op = "==", rhs = "2*l2", kind = "other",
      stringsAsFactors = FALSE
    )
  )
  expect_match(s$meta$caption, "Other constraints of the model: l1 == 2*l2.", fixed = TRUE)
  expect_identical(s$meta$bounded$parameter, "visual =~ x3")
  expect_identical(s$meta$bounded$lower, 0.1)
  expect_match(s$meta$caption, "Bounded parameters: visual =~ x3 (lower 0.1).", fixed = TRUE)
  # The loadings follow the constraint, and lavaan's values are shown.
  pe <- lavaan::parameterEstimates(fit)
  expect_equal(pe$est[pe$label == "l1"], 2 * pe$est[pe$label == "l2"], tolerance = 1e-6)
  l <- s$edges[s$edges$type == "loading" & s$edges$from == "visual", ]
  expect_equal(l$est, pe$est[pe$op == "=~" & pe$lhs == "visual"], tolerance = 1e-12)
  bounded <- sieve_diagram_spec(lavaan::cfa("visual =~ x1 + x2 + x3", rf_hs(), bounds = "standard"))
  expect_match(bounded$meta$caption, "Parameters were estimated within bounds (lavaan bounds = \"standard\").",
    fixed = TRUE
  )
  plain <- sieve_diagram_spec(lavaan::cfa("visual =~ x1 + x2 + x3", rf_hs()))
  expect_false(grepl("bounds", plain$meta$caption, fixed = TRUE))
  expect_false(grepl("Other constraints", plain$meta$caption, fixed = TRUE))
  # A simple equality written as a constraint row is shown as a shared label,
  # not as an other constraint.
  simple <- sieve_diagram_spec(lavaan::cfa("visual =~ a*x1 + b*x2 + x3\na == b", rf_hs(), std.lv = TRUE))
  expect_false(grepl("Other constraints", simple$meta$caption, fixed = TRUE))
  expect_match(simple$meta$caption, "constrained equal (a)", fixed = TRUE)
})

test_that("a multiple-group improper solution names the group of each negative variance", {
  skip_if_not_installed("lavaan")
  n <- 150L
  # Group a: a proper one-factor structure. Group b: a correlation matrix
  # that forces a negative residual variance (r12 * r13 / r23 > 1).
  da <- sieve_with_seed(20261012, {
    f <- stats::rnorm(n)
    data.frame(
      y1 = 0.7 * f + stats::rnorm(n, sd = 0.6), y2 = 0.7 * f + stats::rnorm(n, sd = 0.6),
      y3 = 0.7 * f + stats::rnorm(n, sd = 0.6), g = "a", stringsAsFactors = FALSE
    )
  })$value
  Sb <- matrix(c(1, 0.8, 0.8, 0.8, 1, 0.5, 0.8, 0.5, 1), 3)
  L <- chol(Sb)
  Z <- sieve_with_seed(20261013, matrix(stats::rnorm(3 * n), n))$value
  Z <- scale(Z)
  Z <- Z %*% solve(chol(stats::cov(Z))) %*% L
  db <- data.frame(y1 = Z[, 1], y2 = Z[, 2], y3 = Z[, 3], g = "b", stringsAsFactors = FALSE)
  fit <- sieve_capture(lavaan::cfa("f =~ y1 + y2 + y3", rbind(da, db), group = "g", std.lv = TRUE))$value
  pe <- lavaan::parameterEstimates(fit)
  neg <- pe[pe$op == "~~" & pe$lhs == pe$rhs & pe$est < 0, ]
  expect_identical(nrow(neg), 1L)
  expect_identical(c(neg$lhs, neg$group), c("y1", "2"))
  a <- sieve_diagram_spec(fit, group = 1L, standardized = "none")
  expect_identical(a$status$code, "improper")
  expect_match(a$status$message, "negative variance estimate for y1 in group b.", fixed = TRUE)
  expect_identical(a$status$details$negative_variances$group_label, "b")
  # Nothing drawn for group a is flagged, and the caption says where the flag is.
  expect_false(any(a$edges$improper))
  expect_match(a$meta$caption, "negative variance estimate for y1 in group b, not shown here.", fixed = TRUE)
  b <- sieve_diagram_spec(fit, group = "b", standardized = "none")
  expect_true(b$edges$improper[b$edges$id == "y1~~y1"])
  expect_lt(b$edges$value[b$edges$id == "y1~~y1"], 0)
  expect_match(b$meta$caption, "negative variance estimate for y1 in group b.", fixed = TRUE)
  expect_false(grepl("not shown here", b$meta$caption, fixed = TRUE))
  # A single-group model keeps the plain sentence.
  one <- sieve_diagram_spec(sieve_capture(lavaan::cfa("f =~ y1 + y2 + y3", db[, 1:3], std.lv = TRUE))$value)
  expect_match(one$meta$caption, "negative variance estimate for y1. ", fixed = TRUE)
  expect_false(grepl("in group", one$meta$caption, fixed = TRUE))
})

test_that("with residual terms hidden, residual and disturbance covariances leave the figure and are counted", {
  skip_if_not_installed("lavaan")
  fit <- lavaan::sem(paste("visual =~ x1 + x2 + x3", "textual =~ x4 + x5 + x6", "speed =~ x7 + x8 + x9",
    "textual ~ visual", "speed ~ visual", "textual ~~ speed", "x1 ~~ x4",
    sep = "\n"
  ), rf_hs(), std.lv = TRUE)
  s <- sieve_diagram_spec(fit, residuals = FALSE)
  dis <- s$edges[s$edges$id == "textual~~speed", ]
  res <- s$edges[s$edges$id == "x1~~x4", ]
  expect_identical(c(dis$cov_class, res$cov_class), c("disturbance", "residual"))
  expect_false(dis$visible || res$visible)
  expect_identical(c(dis$hidden_reason, res$hidden_reason), c("residuals", "residuals"))
  # The value kept is lavaan's residual covariance, not the implied correlation
  # of the two variables, which is what a reader of an arc between them would take it for.
  ss <- lavaan::standardizedSolution(fit)
  expect_equal(dis$value, ss$est.std[ss$lhs == "textual" & ss$op == "~~" & ss$rhs == "speed"],
    tolerance = 1e-12
  )
  expect_gt(abs(dis$value - lavaan::lavInspect(fit, "cor.lv")["textual", "speed"]), 0.1)
  n_terms <- sum(s$nodes$kind == "residual")
  expect_identical(n_terms, 11L)
  expect_identical(unname(s$meta$hidden$counts["residuals"]), 2L * n_terms + 2L)
  expect_match(s$meta$caption,
    "In the model but not drawn: residual terms and their variances; 2 residual covariances between them.",
    fixed = TRUE
  )
  # No other class of edge is touched, and the nodes do not move.
  full <- sieve_diagram_spec(fit)
  expect_true(all(s$edges$visible[s$edges$type %in% c("loading", "regression")]))
  expect_true(s$edges$visible[s$edges$id == "visual~~visual"])
  expect_identical(s$nodes[, c("id", "x", "y")], full$nodes[, c("id", "x", "y")])
  expect_identical(sieve_diagram_update(s, residuals = TRUE), full)
})

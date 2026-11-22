# The path diagram specification: the model-derived,
# renderer-independent description that every renderer, the Shiny modules,
# the exports, and plain R use share. Each expectation is checked against
# the fitted lavaan parameter table or against a graph written out by hand
# here, never against the function's own intermediate values.
#
# The data of the twelve test cases are the bundled Holzinger and
# Swineford fixtures, except the case of long labels (simulated on a local
# stream seeded with 113) and the Heywood case of the invalid input case (a
# stated covariance matrix, so no sampling is involved). The model syntax of each
# case is written out in dg_fit() below, and each specification records the
# lavaan and SIEVER versions that produced it (checked in the last test).

dg_cache <- new.env(parent = emptyenv())

dg_v <- c("t1_visual_perception", "t2_cubes", "t4_lozenges")
dg_t <- c("t6_paragraph_comprehension", "t7_sentence", "t9_word_meaning")
dg_s <- c("t10_addition", "t12_counting_groups_of_dots", "t13_straight_and_curved_capitals")
dg_verbal5 <- c(
  "t5_general_information", "t6_paragraph_comprehension", "t7_sentence",
  "t8_word_classification", "t9_word_meaning"
)

dg_syntax <- function(name) {
  two <- paste0(
    "Visual =~ ", paste(dg_v, collapse = " + "), "\n",
    "Textual =~ ", paste(dg_t, collapse = " + ")
  )
  three <- paste0(two, "\nSpeed =~ ", paste(dg_s, collapse = " + "))
  switch(name,
    one = paste0("Verbal =~ ", paste(dg_verbal5, collapse = " + ")),
    two = two,
    three = three,
    higher = paste0(three, "\ng =~ Visual + Textual + Speed"),
    rescov = paste0(three, "\nt10_addition ~~ t12_counting_groups_of_dots"),
    sem = paste(
      "Spatial =~ t1_visual_perception + t2_cubes + t3_paper_form_board + t4_lozenges",
      paste0("Verbal =~ ", paste(dg_verbal5, collapse = " + ")),
      "Speed =~ t10_addition + t11_code + t12_counting_groups_of_dots + t13_straight_and_curved_capitals",
      "Speed ~ Spatial + Verbal",
      "t24_woody_mccall ~ Spatial + Verbal",
      "Speed ~~ t24_woody_mccall",
      sep = "\n"
    ),
    chain = paste(
      "Spatial =~ t1_visual_perception + t2_cubes + t3_paper_form_board + t4_lozenges",
      paste0("Verbal =~ ", paste(dg_verbal5, collapse = " + ")),
      "Verbal ~ a*Spatial",
      "t24_woody_mccall ~ b*Verbal + c*Spatial",
      "indirect := a*b",
      sep = "\n"
    ),
    sim = "trait =~ q1 + q2 + q3 + q4 + q5",
    heywood = "f =~ y1 + y2 + y3"
  )
}

dg_sim_data <- function() {
  sieve_with_seed(113, {
    n <- 400
    f <- stats::rnorm(n)
    data.frame(
      q1 = 0.80 * f + stats::rnorm(n, sd = 0.60),
      q2 = 0.70 * f + stats::rnorm(n, sd = 0.70),
      q3 = 0.02 * f + stats::rnorm(n),
      q4 = -0.60 * f + stats::rnorm(n, sd = 0.80),
      q5 = 0.75 * f + stats::rnorm(n, sd = 0.66)
    )
  })$value
}

dg_ordered_data <- function(binary = FALSE) {
  d <- sieve_hs9_fixture()$data[, c(dg_v, dg_t)]
  d[] <- lapply(d, function(x) {
    if (binary) {
      as.integer(x > stats::median(x))
    } else {
      as.integer(cut(x, stats::quantile(x, 0:4 / 4), include.lowest = TRUE))
    }
  })
  d
}

# Each fit is built once and reused; a specification must never change it.
dg_fit <- function(name) {
  if (!is.null(dg_cache[[name]])) {
    return(dg_cache[[name]])
  }
  d9 <- sieve_hs9_fixture()$data
  d24 <- sieve_hs_fixture()$data
  fit <- switch(name,
    one = lavaan::cfa(dg_syntax("one"), d24, std.lv = TRUE),
    two_unit = lavaan::cfa(dg_syntax("two"), d9, std.lv = TRUE),
    two_marker = lavaan::cfa(dg_syntax("two"), d9, std.lv = FALSE),
    two_orth = lavaan::cfa(dg_syntax("two"), d9, std.lv = TRUE, orthogonal = TRUE),
    two_nose = lavaan::cfa(dg_syntax("two"), d9, std.lv = TRUE, se = "none"),
    three = lavaan::cfa(dg_syntax("three"), d9,
      std.lv = TRUE, estimator = "MLR",
      missing = "fiml"
    ),
    five = {
      fx <- sieve_hs_fixture()
      model <- paste(
        sprintf(
          "%s =~ %s", names(fx$theory_map),
          vapply(fx$theory_map, paste, "", collapse = " + ")
        ),
        collapse = "\n"
      )
      lavaan::cfa(model, d24, std.lv = TRUE, estimator = "MLR", missing = "fiml")
    },
    higher = lavaan::cfa(dg_syntax("higher"), d9, std.lv = TRUE),
    sem = lavaan::sem(dg_syntax("sem"), d24, std.lv = TRUE),
    chain = lavaan::sem(dg_syntax("chain"), d24, std.lv = TRUE),
    rescov = lavaan::cfa(dg_syntax("rescov"), d9, std.lv = TRUE),
    ordered = lavaan::cfa(dg_syntax("two"), dg_ordered_data(),
      ordered = TRUE,
      estimator = "WLSMV", std.lv = TRUE
    ),
    ordered_theta = lavaan::cfa(dg_syntax("two"), dg_ordered_data(),
      ordered = TRUE,
      estimator = "WLSMV", std.lv = TRUE,
      parameterization = "theta"
    ),
    binary = lavaan::cfa(dg_syntax("two"), dg_ordered_data(binary = TRUE),
      ordered = TRUE,
      estimator = "WLSMV", std.lv = TRUE
    ),
    sim = lavaan::cfa(dg_syntax("sim"), dg_sim_data(), std.lv = TRUE),
    heywood = {
      # r12 * r13 / r23 = 1.28: the first loading squared exceeds the
      # variance, so the first residual variance must be negative.
      S <- matrix(c(1, 0.8, 0.8, 0.8, 1, 0.5, 0.8, 0.5, 1), 3,
        dimnames = list(c("y1", "y2", "y3"), c("y1", "y2", "y3"))
      )
      sieve_capture(lavaan::cfa(dg_syntax("heywood"),
        sample.cov = S, sample.nobs = 200,
        std.lv = TRUE
      ))$value
    },
    not_converged = sieve_capture(lavaan::cfa(dg_syntax("three"), d9,
      std.lv = TRUE,
      control = list(iter.max = 2L)
    ))$value,
    groups = lavaan::cfa(dg_syntax("two"), d9, group = "school", group.equal = "loadings")
  )
  dg_cache[[name]] <- fit
  fit
}

dg_key <- function(d) paste(d$lhs, d$op, d$rhs)

# Every edge that encodes a lavaan parameter carries that parameter's
# estimate, and the chosen standardized value, to 1e-12.
expect_edges_match_lavaan <- function(spec, fit, type = "std.all", group = NULL, level = 0.95) {
  pe <- as.data.frame(lavaan::parameterEstimates(fit, level = level))
  pt <- as.data.frame(lavaan::parTable(fit))
  if (!is.null(group)) {
    pe <- pe[pe$group == group, ]
    pt <- pt[pt$group == group, ]
  }
  e <- spec$edges[!is.na(spec$edges$op), ]
  i <- match(dg_key(e), dg_key(pe))
  expect_false(anyNA(i))
  expect_lt(max(abs(e$est - pe$est[i])), 1e-12)
  free <- pt$free[match(dg_key(e), dg_key(pt))] > 0L
  expect_identical(e$free > 0L, free)
  expect_lt(max(abs(e$se[free] - pe$se[i][free])), 1e-12)
  expect_lt(max(abs(e$ci_lower[free] - pe$ci.lower[i][free])), 1e-12)
  expect_lt(max(abs(e$ci_upper[free] - pe$ci.upper[i][free])), 1e-12)
  # A parameter that was not estimated has no standard error to report.
  expect_true(all(is.na(e$se[!free])))
  if (identical(type, "none")) {
    expect_true(all(is.na(e$std)))
    expect_identical(e$value, e$est)
  } else {
    ss <- as.data.frame(lavaan::standardizedSolution(fit, type = type, level = level))
    if (!is.null(group)) {
      ss <- ss[ss$group == group, ]
    }
    j <- match(dg_key(e), dg_key(ss))
    expect_false(anyNA(j))
    expect_lt(max(abs(e$std - ss$est.std[j])), 1e-12)
    expect_identical(e$value, e$std)
    known <- !is.na(e$std_se)
    expect_lt(max(abs(e$std_se[known] - ss$se[j][known])), 1e-12)
    expect_lt(max(abs(e$std_ci_lower[known] - ss$ci.lower[j][known])), 1e-12)
  }
  invisible(TRUE)
}

dg_edges <- function(spec, type, visible = NULL) {
  e <- spec$edges[spec$edges$type == type, , drop = FALSE]
  if (!is.null(visible)) {
    e <- e[e$visible == visible, , drop = FALSE]
  }
  e
}

dg_node <- function(spec, id) spec$nodes[match(id, spec$nodes$id), , drop = FALSE]

# The highest (or furthest) point of a covariance arc, from the documented
# curve convention.
dg_apex <- function(spec, id) {
  e <- spec$edges[match(id, spec$edges$id), ]
  a <- dg_node(spec, e$from)
  b <- dg_node(spec, e$to)
  d <- c(b$x - a$x, b$y - a$y)
  normal <- c(-d[2], d[1]) / sqrt(sum(d^2))
  c((a$x + b$x) / 2, (a$y + b$y) / 2) + e$curve * normal
}

dg_overlaps <- function(spec) {
  n <- spec$nodes
  count <- 0L
  for (i in seq_len(nrow(n) - 1L)) {
    for (j in (i + 1L):nrow(n)) {
      if (abs(n$x[i] - n$x[j]) < (n$w[i] + n$w[j]) / 2 - 1e-9 &&
        abs(n$y[i] - n$y[j]) < (n$h[i] + n$h[j]) / 2 - 1e-9) {
        count <- count + 1L
      }
    }
  }
  count
}

test_that("one factor: every item and loading once, unique terms, no invented edge", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("one")
  spec <- sieve_diagram_spec(fit)
  expect_s3_class(spec, "sieve_diagram")
  expect_identical(spec$status$code, "ok")
  expect_true(sieve_diagram_drawable(spec))

  # The expected graph, written out by hand.
  expect_setequal(spec$nodes$id, c("Verbal", dg_verbal5, paste0("e.", dg_verbal5)))
  expect_identical(dg_node(spec, "Verbal")$kind, "latent")
  expect_identical(dg_node(spec, dg_verbal5)$kind, rep("observed", 5))
  expect_identical(dg_node(spec, paste0("e.", dg_verbal5))$kind, rep("residual", 5))
  expect_identical(dg_node(spec, paste0("e.", dg_verbal5))$role, rep("unique", 5))
  load <- dg_edges(spec, "loading")
  expect_identical(load$from, rep("Verbal", 5))
  expect_identical(load$to, dg_verbal5)
  expect_identical(anyDuplicated(load$to), 0L)
  paths <- dg_edges(spec, "residual_path")
  expect_identical(paths$from, paste0("e.", dg_verbal5))
  expect_identical(paths$to, dg_verbal5)
  expect_true(all(paths$fixed) && all(paths$est == 1) && all(is.na(paths$op)))
  vars <- dg_edges(spec, "variance")
  expect_setequal(vars$from, c(paste0("e.", dg_verbal5), "Verbal"))
  expect_identical(vars$from, vars$to)
  # A one factor model has no relationship between factors to draw.
  expect_identical(nrow(dg_edges(spec, "covariance")), 0L)
  expect_identical(nrow(dg_edges(spec, "regression")), 0L)
  expect_identical(nrow(spec$edges), 5L + 5L + 6L)
  expect_true(all(spec$edges$visible))

  expect_edges_match_lavaan(spec, fit, "std.all")
  # Unit variance identification, read from the parameter table.
  fv <- spec$edges[spec$edges$id == "Verbal~~Verbal", ]
  expect_true(fv$fixed)
  expect_identical(fv$free, 0L)
  expect_identical(fv$label_text, "1")
  expect_true(all(!load$fixed) && all(load$free > 0L))
  expect_identical(spec$meta$identification, "unit_variance")
  expect_identical(spec$meta$model, dg_syntax("one"))
  expect_identical(spec$meta$layout, "top_down")
  expect_length(sieve_diagram_validate(spec, error = FALSE), 0L)
})

test_that("two correlated factors: one covariance, the fitted one, above the factors", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("two_unit")
  spec <- sieve_diagram_spec(fit)
  cov <- dg_edges(spec, "covariance")
  expect_identical(nrow(cov), 1L)
  expect_identical(cov$cov_class, "exogenous")
  expect_setequal(c(cov$from, cov$to), c("Visual", "Textual"))
  ss <- lavaan::standardizedSolution(fit, type = "std.all")
  r <- ss$est.std[ss$lhs == "Visual" & ss$op == "~~" & ss$rhs == "Textual"]
  expect_lt(abs(cov$value - r), 1e-12)
  expect_identical(cov$label_text, formatC(r, format = "f", digits = 2))
  expect_edges_match_lavaan(spec, fit, "std.all")

  # Layout: factors on one row, each above its own indicators, the unique
  # terms directly beneath the indicators, the arc above the factors.
  f <- dg_node(spec, c("Visual", "Textual"))
  expect_equal(f$y[1], f$y[2])
  expect_lt(f$x[1], f$x[2])
  for (set in list(list("Visual", dg_v), list("Textual", dg_t))) {
    items <- dg_node(spec, set[[2]])
    unique_terms <- dg_node(spec, paste0("e.", set[[2]]))
    head <- dg_node(spec, set[[1]])
    expect_true(all(items$y < head$y))
    expect_equal(mean(range(items$x)), head$x)
    expect_true(all(diff(items$x) > 0))
    expect_equal(diff(items$x)[1], diff(items$x)[2])
    expect_equal(unique_terms$x, items$x)
    expect_true(all(unique_terms$y < items$y - items$h / 2))
  }
  gap_within <- diff(dg_node(spec, dg_v)$x)[1]
  gap_between <- dg_node(spec, dg_t[1])$x - dg_node(spec, dg_v[3])$x
  expect_gt(gap_between, gap_within)
  apex <- dg_apex(spec, cov$id)
  expect_gt(apex[2], f$y[1] + f$h[1] / 2)
  # The factor variance loops sit on top, the unique term loops underneath.
  expect_identical(dg_edges(spec, "variance")$angle[dg_edges(spec, "variance")$from == "Visual"], 90)
  expect_true(all(dg_edges(spec, "variance")$angle[grepl("^e\\.", dg_edges(spec, "variance")$from)] == 270))
  expect_identical(dg_overlaps(spec), 0L)
})

test_that("marker and unit variance: fixed flags from the parameter table, not the value", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("two_marker")
  pt <- as.data.frame(lavaan::parTable(fit))
  markers <- c(paste0("Visual=~", dg_v[1]), paste0("Textual=~", dg_t[1]))

  raw <- sieve_diagram_spec(fit, standardized = "none")
  expect_edges_match_lavaan(raw, fit, "none")
  load <- dg_edges(raw, "loading")
  expect_identical(load$id[load$fixed], markers)
  expect_identical(load$est[load$fixed], c(1, 1))
  expect_identical(load$label_text[load$fixed], c("1", "1"))
  expect_true(all(load$free[!load$fixed] > 0L))
  expect_identical(
    load$label_text[!load$fixed],
    formatC(load$est[!load$fixed], format = "f", digits = 2)
  )
  fvar <- raw$edges[raw$edges$id %in% c("Visual~~Visual", "Textual~~Textual"), ]
  expect_false(any(fvar$fixed))
  expect_true(all(fvar$free > 0L))
  expect_true(all(fvar$value != 1))
  expect_identical(raw$meta$identification, "marker")
  expect_identical(raw$meta$identification_detail$scaling, c("marker", "marker"))
  expect_match(raw$meta$caption, "Values are unstandardized", fixed = TRUE)
  expect_match(raw$meta$caption, "fixing one loading to 1", fixed = TRUE)
  expect_identical(
    raw$edges$fixed[!is.na(raw$edges$op)],
    pt$free[match(dg_key(raw$edges[!is.na(raw$edges$op), ]), dg_key(pt))] == 0L
  )

  std <- sieve_diagram_spec(fit, standardized = "std.all")
  expect_edges_match_lavaan(std, fit, "std.all")
  sload <- dg_edges(std, "loading")
  # A marker loading is still flagged fixed, but its standardized value is
  # not 1 and is not printed as the constant 1.
  expect_identical(sload$id[sload$fixed], markers)
  expect_true(all(abs(sload$value[sload$fixed] - 1) > 0.05))
  expect_false(any(sload$label_text[sload$fixed] == "1"))
  expect_identical(sload$label_text, formatC(sload$value, format = "f", digits = 2))
  # A standardized variance of 1 does not make the variance fixed.
  svar <- std$edges[std$edges$id %in% c("Visual~~Visual", "Textual~~Textual"), ]
  expect_lt(max(abs(svar$value - 1)), 1e-12)
  expect_false(any(svar$fixed))
  expect_identical(svar$label_text, c("1.00", "1.00"))
  expect_identical(std$meta$standardized, "std.all")
  expect_match(std$meta$caption, "fully standardized (std.all)", fixed = TRUE)
  expect_identical(std$meta$identification, "marker")

  lv <- sieve_diagram_spec(fit, standardized = "std.lv")
  expect_edges_match_lavaan(lv, fit, "std.lv")
  expect_match(lv$meta$caption, "(std.lv)", fixed = TRUE)

  # The same model identified by unit variances: every loading free, the
  # factor variances fixed, and the same standardized solution.
  unit <- sieve_diagram_spec(dg_fit("two_unit"))
  uload <- dg_edges(unit, "loading")
  expect_false(any(uload$fixed))
  uvar <- unit$edges[unit$edges$id %in% c("Visual~~Visual", "Textual~~Textual"), ]
  expect_true(all(uvar$fixed))
  expect_identical(uvar$label_text, c("1", "1"))
  expect_identical(unit$meta$identification, "unit_variance")
  expect_equal(uload$value, sload$value, tolerance = 1e-5)
})

test_that("orthogonal factors: no estimated covariance; the fixed zero is disclosed", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("two_orth")
  spec <- sieve_diagram_spec(fit)
  cov <- dg_edges(spec, "covariance")
  expect_identical(nrow(cov), 1L)
  expect_true(cov$fixed)
  expect_identical(cov$free, 0L)
  expect_identical(cov$est, 0)
  expect_false(cov$visible)
  expect_identical(cov$hidden_reason, "fixed_zero")
  expect_identical(nrow(dg_edges(spec, "covariance", visible = TRUE)), 0L)
  expect_identical(spec$meta$fixed_zero, "Visual~~Textual")
  expect_match(spec$meta$caption, "Fixed at zero and not drawn: Visual ~~ Textual.", fixed = TRUE)
  expect_identical(unname(spec$meta$hidden$counts["fixed_zero"]), 1L)

  shown <- sieve_diagram_update(spec, fixed_zero = TRUE)
  cov <- dg_edges(shown, "covariance")
  expect_true(cov$visible)
  expect_true(cov$fixed)
  expect_identical(cov$label_text, "0")
  expect_true(is.finite(cov$curve) && cov$curve != 0)
  expect_match(shown$meta$caption, "Fixed at zero and drawn as fixed: Visual ~~ Textual.", fixed = TRUE)
  # The covariance toggle then governs it like any other factor covariance.
  off <- sieve_diagram_update(shown, covariances = FALSE)
  expect_identical(dg_edges(off, "covariance")$hidden_reason, "covariances")

  # An estimate that happens to be near zero is not a fixed zero.
  sim <- sieve_diagram_spec(dg_fit("sim"))
  near <- sim$edges[sim$edges$id == "trait=~q3", ]
  expect_lt(abs(near$value), 0.15)
  expect_false(near$fixed)
  expect_true(near$visible)
  expect_length(sim$meta$fixed_zero, 0L)
})

test_that("three correlated factors: model order, grouped indicators, staggered arcs", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("three")
  spec <- sieve_diagram_spec(fit)
  expect_edges_match_lavaan(spec, fit, "std.all")
  expect_identical(spec$nodes$id[spec$nodes$kind == "latent"], c("Visual", "Textual", "Speed"))
  expect_identical(spec$nodes$id[spec$nodes$kind == "observed"], c(dg_v, dg_t, dg_s))
  load <- dg_edges(spec, "loading")
  expect_identical(load$to, c(dg_v, dg_t, dg_s))
  expect_identical(load$from, rep(c("Visual", "Textual", "Speed"), each = 3))
  cov <- dg_edges(spec, "covariance")
  expect_setequal(cov$id, c("Visual~~Textual", "Visual~~Speed", "Textual~~Speed"))
  expect_true(all(cov$cov_class == "exogenous"))
  # Not alphabetical: Visual, Textual, Speed from left to right.
  f <- dg_node(spec, c("Visual", "Textual", "Speed"))
  expect_true(all(diff(f$x) > 0))
  expect_true(all(diff(dg_node(spec, c(dg_v, dg_t, dg_s))$x) > 0))
  # Every arc bows above the factor row, and the one that spans the middle
  # factor clears the two short ones.
  top <- f$y[1] + f$h[1] / 2
  apex <- vapply(
    c("Visual~~Textual", "Textual~~Speed", "Visual~~Speed"),
    function(id) dg_apex(spec, id)[2], numeric(1)
  )
  expect_true(all(apex > top))
  expect_gt(apex[["Visual~~Speed"]], max(apex[c("Visual~~Textual", "Textual~~Speed")]) + 0.3)
  expect_identical(spec$meta$estimator, "MLR")
  expect_identical(spec$meta$nobs, 301L)
  expect_identical(dg_overlaps(spec), 0L)
  # The mean structure is in the parameter table and stated, not drawn.
  expect_identical(nrow(dg_edges(spec, "intercept", visible = TRUE)), 0L)
  expect_gt(nrow(dg_edges(spec, "intercept")), 0L)
  expect_match(spec$meta$caption, "Intercepts and thresholds are in the parameter table", fixed = TRUE)
})

test_that("five factors and 24 indicators: everything kept, and the size follows the content", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("five")
  spec <- sieve_diagram_spec(fit)
  expect_identical(
    as.integer(table(spec$nodes$kind)[c("latent", "observed", "residual")]),
    c(5L, 24L, 24L)
  )
  shown <- spec$edges[spec$edges$visible, ]
  expect_identical(
    as.integer(table(shown$type)[c("covariance", "loading", "residual_path", "variance")]),
    c(10L, 24L, 24L, 29L)
  )
  expect_identical(anyDuplicated(dg_edges(spec, "loading")$to), 0L)
  expect_edges_match_lavaan(spec, fit, "std.all")
  expect_identical(dg_overlaps(spec), 0L)
  # No label is cut: the bundled names are well over seven characters.
  expect_identical(spec$nodes$label[spec$nodes$kind == "observed"], sieve_hs_fixture()$item_cols)
  expect_gt(max(nchar(spec$nodes$label)), 25L)

  small <- sieve_diagram_spec(dg_fit("two_unit"))
  expect_gt(spec$size$width, 2 * small$size$width)
  expect_false(is.null(spec$size$note))
  expect_null(small$size$note)
  short <- sieve_diagram_update(spec, label_mode = "short")
  expect_lt(short$size$width, 0.6 * spec$size$width)
  expect_identical(short$nodes$label[short$nodes$kind == "observed"], paste0("V", 1:24))
  expect_identical(short$label_key$label, sieve_hs_fixture()$item_cols)
  expect_identical(short$label_key$short, paste0("V", 1:24))
  side <- sieve_diagram_update(short, layout = "left_right")
  expect_gt(side$size$height, side$size$width)
  expect_lt(side$size$width, 11)
  expect_identical(dg_overlaps(side), 0L)
  # On its side: factors to the left of their indicators, unique terms
  # further right, and the covariance arcs on the far left.
  expect_true(all(dg_node(side, "Spatial")$x < dg_node(side, sieve_hs_fixture()$theory_map$Spatial)$x))
  expect_true(all(side$nodes$x[side$nodes$kind == "residual"] >
    max(side$nodes$x[side$nodes$kind == "observed"])))
  expect_lt(dg_apex(side, "Spatial~~Reasoning")[1], dg_node(side, "Spatial")$x - 1)
  # Ten arcs, staggered: no two labels share a position.
  apex <- t(vapply(dg_edges(side, "covariance")$id, function(id) dg_apex(side, id), numeric(2)))
  expect_gt(min(stats::dist(apex)), 0.25)
})

test_that("higher-order CFA: levels, directed higher-order loadings, first-order disturbances", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("higher")
  spec <- sieve_diagram_spec(fit)
  expect_edges_match_lavaan(spec, fit, "std.all")
  first <- c("Visual", "Textual", "Speed")
  expect_identical(dg_node(spec, "g")$level, 2L)
  expect_identical(dg_node(spec, first)$level, rep(1L, 3))
  expect_identical(dg_node(spec, c(dg_v, dg_t, dg_s))$level, rep(0L, 9))
  expect_identical(dg_node(spec, "g")$role, "higher_order")
  expect_identical(dg_node(spec, first)$role, rep("factor", 3))
  expect_true(dg_node(spec, "g")$exogenous)
  expect_false(any(dg_node(spec, first)$exogenous))
  expect_identical(dg_node(spec, first)$parent, rep("g", 3))
  # Higher-order loadings run from g down to the first-order factors.
  load <- dg_edges(spec, "loading")
  high <- load[load$from == "g", ]
  expect_identical(high$to, first)
  expect_false(any(load$to == "g"))
  # First-order factors carry disturbances, not variances of their own.
  vars <- dg_edges(spec, "variance")
  expect_false(any(vars$from %in% first))
  expect_true(all(paste0("e.", first) %in% vars$from))
  expect_true("g" %in% vars$from)
  expect_identical(dg_node(spec, paste0("e.", first))$role, rep("disturbance", 3))
  expect_identical(dg_node(spec, paste0("e.", dg_v))$role, rep("unique", 3))
  paths <- dg_edges(spec, "residual_path")
  expect_true(all(paste0("e.", first, "->", first) %in% paths$id))
  # The disturbance variance is the lavaan residual variance of the factor.
  ss <- lavaan::standardizedSolution(fit)
  d_visual <- vars$value[vars$from == "e.Visual"]
  expect_lt(abs(d_visual - ss$est.std[ss$lhs == "Visual" & ss$op == "~~" & ss$rhs == "Visual"]), 1e-12)
  expect_lt(d_visual, 1)
  expect_identical(nrow(dg_edges(spec, "covariance")), 0L)
  expect_identical(spec$meta$model_kind, "higher_order")
  expect_match(spec$meta$caption, "Higher-order confirmatory factor model", fixed = TRUE)
  expect_match(spec$meta$caption, "its disturbance variance, if endogenous", fixed = TRUE)
  # Three levels from top to bottom, then the unique terms.
  expect_gt(dg_node(spec, "g")$y, max(dg_node(spec, first)$y))
  expect_gt(min(dg_node(spec, first)$y), max(dg_node(spec, c(dg_v, dg_t, dg_s))$y))
  expect_equal(dg_node(spec, "g")$x, mean(range(dg_node(spec, first)$x)))
  expect_equal(dg_node(spec, paste0("e.", first))$y, dg_node(spec, first)$y)
  expect_identical(dg_overlaps(spec), 0L)
  expect_identical(dg_overlaps(sieve_diagram_update(spec, layout = "left_right")), 0L)
})

test_that("structural model: loadings, regressions, and each covariance correctly typed", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("sem")
  spec <- sieve_diagram_spec(fit)
  expect_edges_match_lavaan(spec, fit, "std.all")
  expect_identical(spec$meta$model_kind, "sem")
  expect_identical(spec$meta$layout, "left_right")

  # Regressions run from predictor to outcome and are not loadings.
  reg <- dg_edges(spec, "regression")
  expect_identical(
    paste(reg$from, "->", reg$to),
    c(
      "Spatial -> Speed", "Verbal -> Speed",
      "Spatial -> t24_woody_mccall", "Verbal -> t24_woody_mccall"
    )
  )
  load <- dg_edges(spec, "loading")
  expect_identical(nrow(load), 13L)
  expect_true(all(load$from %in% c("Spatial", "Verbal", "Speed")))
  expect_false(any(load$to == "t24_woody_mccall"))
  # The observed outcome is a rectangle with a disturbance of its own.
  y <- dg_node(spec, "t24_woody_mccall")
  expect_identical(y$kind, "observed")
  expect_identical(y$role, "outcome")
  expect_false(y$exogenous)
  expect_identical(dg_node(spec, c("e.Speed", "e.t24_woody_mccall"))$role, rep("disturbance", 2))
  expect_identical(
    dg_node(spec, c("Spatial", "Verbal", "Speed"))$structural,
    c("predictor", "predictor", "outcome")
  )
  expect_identical(dg_node(spec, c("Spatial", "Verbal"))$exogenous, c(TRUE, TRUE))
  vars <- dg_edges(spec, "variance")
  expect_true(all(c("Spatial", "Verbal", "e.Speed", "e.t24_woody_mccall") %in% vars$from))
  expect_false(any(c("Speed", "t24_woody_mccall") %in% vars$from))

  cov <- dg_edges(spec, "covariance")
  expect_identical(nrow(cov), 2L)
  exo <- cov[cov$id == "Spatial~~Verbal", ]
  dis <- cov[cov$id == "Speed~~t24_woody_mccall", ]
  expect_identical(exo$cov_class, "exogenous")
  expect_identical(c(exo$from, exo$to), c("Spatial", "Verbal"))
  expect_identical(dis$cov_class, "disturbance")
  expect_identical(c(dis$from, dis$to), c("e.Speed", "e.t24_woody_mccall"))
  # The disturbance covariance is not the correlation of the two variables.
  implied <- lavaan::lavInspect(fit, "cor.all")["Speed", "t24_woody_mccall"]
  expect_gt(abs(dis$value - implied), 0.05)
  expect_match(spec$meta$caption, "residual covariances, not correlations of the variables", fixed = TRUE)

  # Each toggle hides its own class and nothing else.
  no_exo <- sieve_diagram_update(spec, covariances = FALSE)
  expect_identical(
    dg_edges(no_exo, "covariance")$visible[match(c(exo$id, dis$id), cov$id)],
    c(FALSE, TRUE)
  )
  no_res <- sieve_diagram_update(spec, residual_covariances = FALSE)
  expect_identical(
    dg_edges(no_res, "covariance")$visible[match(c(exo$id, dis$id), cov$id)],
    c(TRUE, FALSE)
  )
  expect_identical(
    dg_edges(no_res, "covariance")$hidden_reason[match(dis$id, cov$id)],
    "residual_covariances"
  )
  # With the residual terms hidden the disturbance covariance leaves the
  # figure with them (drawn between the variables it would look like their
  # correlation, which it is not); it keeps its row, its value, and its
  # class, and the caption counts it.
  bare <- sieve_diagram_update(spec, residuals = FALSE)
  dis2 <- bare$edges[bare$edges$id == dis$id, ]
  expect_identical(dis2$cov_class, "disturbance")
  expect_false(dis2$visible)
  expect_identical(dis2$hidden_reason, "residuals")
  expect_identical(dis2$value, dis$value)
  expect_true(bare$edges$visible[bare$edges$id == exo$id])
  expect_match(bare$meta$caption,
    "In the model but not drawn: residual terms and their variances; 1 residual covariance between them.",
    fixed = TRUE
  )
  expect_false(grepl("drawn between the variables", bare$meta$caption, fixed = TRUE))
  expect_false(any(bare$nodes$visible[bare$nodes$kind == "residual"]))
  # The specific toggle wins when both apply, so the reason is the one the
  # reader can act on.
  both <- sieve_diagram_update(spec, residuals = FALSE, residual_covariances = FALSE)
  expect_identical(both$edges$hidden_reason[both$edges$id == dis$id], "residual_covariances")

  # Left to right by structural depth, measurement blocks kept together.
  expect_identical(
    dg_node(spec, c("Spatial", "Verbal", "Speed", "t24_woody_mccall"))$depth,
    c(0L, 0L, 1L, 1L)
  )
  expect_equal(dg_node(spec, "Spatial")$x, dg_node(spec, "Verbal")$x)
  expect_equal(dg_node(spec, "Speed")$x, dg_node(spec, "t24_woody_mccall")$x)
  expect_lt(dg_node(spec, "Spatial")$x, dg_node(spec, "Speed")$x - 2)
  expect_gt(dg_node(spec, "Spatial")$y, dg_node(spec, "Verbal")$y)
  for (factor in c("Spatial", "Verbal", "Speed")) {
    items <- spec$nodes[spec$nodes$parent %in% factor & spec$nodes$kind == "observed", ]
    expect_identical(length(unique(round(items$y, 8))), 1L)
    expect_identical(unique(items$block), factor)
  }
  # Upper row indicators above, lower row indicators below.
  expect_gt(dg_node(spec, "t1_visual_perception")$y, dg_node(spec, "Spatial")$y)
  expect_lt(dg_node(spec, "t5_general_information")$y, dg_node(spec, "Verbal")$y)
  # Disturbances on the outer (right) side of the outcomes.
  expect_gt(dg_node(spec, "e.Speed")$x, dg_node(spec, "Speed")$x)
  expect_gt(dg_node(spec, "e.t24_woody_mccall")$x, dg_node(spec, "t24_woody_mccall")$x)
  # The exogenous covariance bows away from the regressions.
  expect_lt(dg_apex(spec, "Spatial~~Verbal")[1], dg_node(spec, "Spatial")$x)
  expect_identical(dg_overlaps(spec), 0L)
  expect_identical(dg_overlaps(sieve_diagram_update(spec, layout = "top_down")), 0L)

  # A mediator sits in the middle column.
  chain <- sieve_diagram_spec(dg_fit("chain"))
  expect_identical(dg_node(chain, c("Spatial", "Verbal", "t24_woody_mccall"))$depth, 0:2)
  expect_identical(dg_node(chain, "Verbal")$structural, "mediator")
  expect_true(all(diff(dg_node(chain, c("Spatial", "Verbal", "t24_woody_mccall"))$x) > 2))
  # Defined parameters are reported in the table, not drawn.
  expect_true("indirect:=a*b" %in% chain$parameters$id)
  expect_false("indirect:=a*b" %in% chain$edges$id)
  expect_true(is.finite(chain$parameters$se[chain$parameters$id == "indirect:=a*b"]))
})

test_that("a residual covariance joins the two unique terms and has its own toggle", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("rescov")
  spec <- sieve_diagram_spec(fit)
  expect_edges_match_lavaan(spec, fit, "std.all")
  id <- "t10_addition~~t12_counting_groups_of_dots"
  rc <- spec$edges[spec$edges$id == id, ]
  expect_identical(rc$type, "covariance")
  expect_identical(rc$cov_class, "residual")
  expect_identical(c(rc$from, rc$to), c("e.t10_addition", "e.t12_counting_groups_of_dots"))
  expect_true(rc$visible)
  cov <- dg_edges(spec, "covariance")
  expect_identical(sum(cov$cov_class == "exogenous"), 3L)
  expect_identical(sum(cov$cov_class == "residual"), 1L)
  # It bows below the unique terms, away from the items, and the loops of
  # the two unique terms turn aside to make room for it.
  expect_lt(dg_apex(spec, id)[2], dg_node(spec, "e.t10_addition")$y)
  vars <- dg_edges(spec, "variance")
  expect_identical(vars$angle[vars$from == "e.t10_addition"], 225)
  expect_identical(vars$angle[vars$from == "e.t12_counting_groups_of_dots"], 315)

  # The factor covariance toggle neither hides nor adds it.
  off <- sieve_diagram_update(spec, covariances = FALSE)
  expect_true(off$edges$visible[off$edges$id == id])
  expect_identical(sum(dg_edges(off, "covariance", visible = TRUE)$cov_class == "exogenous"), 0L)
  expect_identical(nrow(dg_edges(off, "covariance")), 4L)
  only <- sieve_diagram_update(spec, residual_covariances = FALSE)
  expect_false(only$edges$visible[only$edges$id == id])
  expect_identical(nrow(dg_edges(only, "covariance", visible = TRUE)), 3L)
  # A model without the parameter has no such edge.
  expect_false(id %in% sieve_diagram_spec(dg_fit("three"))$edges$id)
})

test_that("ordered and binary indicators: parameterization, residuals, and thresholds disclosed", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("ordered")
  spec <- sieve_diagram_spec(fit)
  expect_identical(spec$status$code, "ok")
  expect_edges_match_lavaan(spec, fit, "std.all")
  items <- c(dg_v, dg_t)
  expect_true(all(dg_node(spec, items)$ordered))
  expect_false(any(dg_node(spec, c("Visual", "Textual"))$ordered))
  expect_identical(spec$meta$ordered, items)
  expect_identical(spec$meta$estimator, "WLSMV")
  expect_identical(spec$meta$lavaan_options$parameterization, "delta")
  # Under delta the unique variances are implied: not free, not fixed.
  uv <- dg_edges(spec, "variance")
  uv <- uv[uv$from %in% paste0("e.", items), ]
  expect_true(all(uv$derived))
  expect_false(any(uv$fixed))
  expect_true(all(uv$free == 0L))
  expect_true(all(is.na(uv$se)))
  lam <- dg_edges(spec, "loading")$est
  expect_equal(uv$est, 1 - lam^2, tolerance = 1e-8)
  expect_identical(uv$label_text, formatC(uv$value, format = "f", digits = 2))
  # The factor variances are genuinely fixed.
  fv <- spec$edges[spec$edges$id %in% c("Visual~~Visual", "Textual~~Textual"), ]
  expect_true(all(fv$fixed) && !any(fv$derived))
  # Thresholds: in the table, not drawn unless asked for.
  pe <- lavaan::parameterEstimates(fit)
  thr <- dg_edges(spec, "threshold")
  expect_identical(nrow(thr), sum(pe$op == "|"))
  expect_identical(nrow(thr), 18L)
  expect_false(any(thr$visible))
  expect_true(all(thr$hidden_reason == "intercepts"))
  expect_identical(sum(spec$parameters$type == "threshold"), 18L)
  expect_identical(sum(spec$parameters$type == "scale"), 6L)
  expect_true(all(is.na(spec$parameters$edge[spec$parameters$type == "scale"])))
  with_thr <- sieve_diagram_update(spec, intercepts = TRUE)
  expect_true(all(dg_edges(with_thr, "threshold")$visible))
  expect_identical(dg_edges(with_thr, "threshold")$from, dg_edges(with_thr, "threshold")$to)
  expect_match(spec$meta$caption, "coefficients refer to latent response variables", fixed = TRUE)
  expect_match(spec$meta$caption, "delta parameterization", fixed = TRUE)
  expect_match(spec$meta$caption, "implied by the model, not estimated", fixed = TRUE)
  expect_match(spec$meta$caption, "thresholds are in the parameter table", fixed = TRUE)

  theta <- sieve_diagram_spec(dg_fit("ordered_theta"), standardized = "none")
  tv <- dg_edges(theta, "variance")
  tv <- tv[tv$from %in% paste0("e.", items), ]
  expect_true(all(tv$fixed) && !any(tv$derived))
  expect_identical(tv$est, rep(1, 6))
  expect_identical(tv$label_text, rep("1", 6))
  expect_match(theta$meta$caption, "theta parameterization", fixed = TRUE)
  expect_match(theta$meta$caption, "residual variances are fixed at 1", fixed = TRUE)
  expect_true(all(theta$parameters$derived[theta$parameters$type == "scale"]))

  bin <- sieve_diagram_spec(dg_fit("binary"))
  expect_edges_match_lavaan(bin, dg_fit("binary"), "std.all")
  expect_identical(nrow(dg_edges(bin, "threshold")), 6L)
  expect_true(all(dg_node(bin, items)$ordered))
  expect_true(all(dg_edges(bin, "variance")$derived[dg_edges(bin, "variance")$from %in%
    paste0("e.", items)]))
  expect_match(bin$meta$caption, "latent response variables (delta parameterization)", fixed = TRUE)
})

test_that("long labels, a negative and a near-zero loading, and unknown uncertainty", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("sim")
  long <- c(
    q1 = "I feel that the work I do in this organization is recognized by my supervisor",
    q2 = "My colleagues and I share information openly when a problem arises",
    q3 = "The cafeteria offers a reasonable variety of lunch options",
    q4 = "I often think about leaving this organization for another employer",
    q5 = "Decisions that affect my work are explained to me in a timely way"
  )
  expect_true(all(nchar(long) >= 40 & nchar(long) <= 80))
  spec <- sieve_diagram_spec(fit, labels = long, name_map = c(trait = "Organizational engagement"))
  expect_edges_match_lavaan(spec, fit, "std.all")
  items <- dg_node(spec, names(long))
  # Never truncated, and wrapped only at word boundaries.
  expect_identical(items$label, unname(long))
  expect_identical(items$label_full, unname(long))
  expect_identical(gsub("\n", " ", items$label_wrapped, fixed = TRUE), unname(long))
  # The automatic width: wider than 16 characters for wordings this long, so
  # that no label needs more than four lines, and no line is a leftover word.
  expect_null(spec$meta$options$wrap)
  expect_gt(spec$meta$wrap, 16L)
  auto_lines <- strsplit(items$label_wrapped, "\n", fixed = TRUE)
  expect_lte(max(nchar(unlist(auto_lines))), spec$meta$wrap)
  expect_lte(max(lengths(auto_lines)), 4L)
  expect_gt(min(nchar(unlist(auto_lines))), 5L)
  expect_identical(sieve_diagram_spec(dg_fit("two_unit"))$meta$wrap, 16L)
  # A width that is asked for is kept exactly.
  strict <- sieve_diagram_update(spec, wrap = 16L)
  expect_identical(strict$meta$wrap, 16L)
  strict_lines <- strsplit(dg_node(strict, names(long))$label_wrapped, "\n", fixed = TRUE)
  expect_lte(max(nchar(unlist(strict_lines))), 16L)
  expect_gt(max(lengths(strict_lines)), max(lengths(auto_lines)))
  expect_identical(sieve_diagram_update(strict, wrap = NULL), spec)
  expect_identical(dg_node(spec, "trait")$label, "Organizational engagement")
  expect_identical(dg_node(spec, "trait")$id, "trait")
  # Nodes are sized to their text and still do not collide.
  lines <- strsplit(items$label_wrapped, "\n", fixed = TRUE)
  expect_true(all(items$h >= lengths(lines) * 10 * 1.2 / 72))
  expect_true(all(items$w >= vapply(lines, function(l) max(sieve_diagram_text_width(l)), 0)))
  expect_identical(dg_overlaps(spec), 0L)
  wide <- sieve_diagram_update(spec, wrap = 40L)
  expect_lt(
    max(lengths(strsplit(dg_node(wide, names(long))$label_wrapped, "\n", fixed = TRUE))),
    max(lengths(lines))
  )
  expect_gt(dg_node(wide, "q1")$w, items$w[1])
  expect_identical(dg_node(sieve_diagram_update(spec, wrap = Inf), "q1")$label_wrapped, long[["q1"]])
  # A word longer than the wrap width stays whole.
  expect_identical(sieve_diagram_wrap("supercalifragilistic_word", 8), "supercalifragilistic_\nword")
  expect_identical(sieve_diagram_wrap("one two three", 7), "one two\nthree")

  # Signs are kept, in the value and in the label.
  pe <- lavaan::parameterEstimates(fit)
  neg <- spec$edges[spec$edges$id == "trait=~q4", ]
  expect_lt(neg$est, 0)
  expect_identical(neg$est, pe$est[pe$op == "=~" & pe$rhs == "q4"])
  expect_lt(neg$value, 0)
  expect_match(neg$label_text, "^-0\\.[0-9]{2}$")
  expect_false(neg$improper)
  near <- spec$edges[spec$edges$id == "trait=~q3", ]
  expect_identical(near$label_text, formatC(near$value, format = "f", digits = 2))
  # The threshold filter hides the near-zero loading only, on its absolute
  # value, so the negative loading stays.
  cut <- sieve_diagram_update(spec, min_loading = 0.30)
  expect_identical(cut$edges$id[!cut$edges$visible], "trait=~q3")
  expect_identical(cut$edges$hidden_reason[cut$edges$id == "trait=~q3"], "min_loading")
  expect_true(cut$edges$visible[cut$edges$id == "trait=~q4"])
  expect_match(cut$meta$caption, "1 loading with absolute value below 0.3 (trait =~ q3)", fixed = TRUE)
  expect_true(dg_node(cut, "q3")$visible)

  # Standard errors that were never computed are unknown, not zero.
  nose <- sieve_diagram_spec(dg_fit("two_nose"))
  expect_identical(nose$status$code, "ok")
  expect_match(nose$status$message, "Standard errors were not requested", fixed = TRUE)
  lavaan_rows <- nose$edges[!is.na(nose$edges$op), ]
  expect_true(all(is.na(lavaan_rows$se)) && all(is.na(lavaan_rows$ci_lower)) &&
    all(is.na(lavaan_rows$std_se)) && all(is.na(lavaan_rows$std_ci_upper)))
  expect_false(anyNA(lavaan_rows$est))
  expect_false(anyNA(lavaan_rows$value))
  expect_match(nose$meta$caption, "uncertainty is unknown", fixed = TRUE)
  expect_equal(lavaan_rows$est, lavaan::parameterEstimates(dg_fit("two_nose"))$est)
  # A value that is not known is labeled as not known.
  blank <- nose$edges[1:2, ]
  blank$value[1] <- NA_real_
  expect_identical(sieve_diagram_labels(blank, 2L)[1], "NA")
})

test_that("invalid input: explicit status, no numbers for a non-solution, Heywood shown as it is", {
  skip_if_not_installed("lavaan")
  none <- sieve_diagram_spec(NULL, analysis_id = "A7")
  expect_s3_class(none, "sieve_diagram")
  expect_identical(none$status$code, "failed")
  expect_identical(none$status$message, "No fitted model was supplied.")
  expect_identical(c(nrow(none$nodes), nrow(none$edges), nrow(none$parameters)), c(0L, 0L, 0L))
  expect_false(sieve_diagram_drawable(none))
  expect_match(none$meta$caption, "^No path diagram: No fitted model was supplied\\. Analysis A7\\.$")
  expect_true(is.na(none$size$width))
  expect_length(sieve_diagram_validate(none, error = FALSE), 0L)
  expect_identical(names(none$nodes), names(sieve_diagram_spec(dg_fit("one"))$nodes))
  expect_identical(names(none$edges), names(sieve_diagram_spec(dg_fit("one"))$edges))

  nc <- sieve_diagram_spec(dg_fit("not_converged"))
  expect_identical(nc$status$code, "not_converged")
  expect_match(nc$status$message, "did not converge")
  expect_identical(c(nrow(nc$nodes), nrow(nc$edges), nrow(nc$parameters)), c(0L, 0L, 0L))
  expect_false(sieve_diagram_drawable(nc))
  expect_false(nc$status$details$fit_status$converged)
  expect_output(print(nc), "Status: not_converged")
  # A display change cannot turn a failure into a diagram.
  nc2 <- sieve_diagram_update(nc, covariances = FALSE, analysis_id = "A9")
  expect_identical(nc2$status$code, "not_converged")
  expect_identical(nrow(nc2$edges), 0L)
  expect_false(nc2$meta$options$covariances)

  # Not identified: negative degrees of freedom.
  under <- sieve_capture(lavaan::cfa("f =~ t1_visual_perception + t2_cubes",
    data = sieve_hs9_fixture()$data, std.lv = TRUE
  ))
  ni <- sieve_diagram_spec(under$value)
  expect_identical(ni$status$code, "failed")
  expect_identical(ni$status$details$fit_status$code, "not_identified")
  expect_match(ni$status$message, "not identified")
  expect_identical(nrow(ni$edges), 0L)

  # A failed analysis record keeps its reason.
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  bad <- sieve_run_cfa(sieve_cfa_spec(meta), fx$data[, fx$item_cols[1:5]])
  failed <- sieve_diagram_spec(bad)
  expect_identical(failed$status$code, "failed")
  expect_match(failed$status$message, "do not contain the modeled variable")
  good <- sieve_diagram_spec(sieve_run_cfa(sieve_cfa_spec(meta), fx$data))
  expect_identical(good$status$code, "ok")
  expect_identical(good$meta$model_source, "sieve_spec")
  expect_identical(good$meta$model, sieve_cfa_plan(meta)$model)

  # Heywood: drawn with the negative variance and flagged, never repaired.
  fit <- dg_fit("heywood")
  pe <- lavaan::parameterEstimates(fit)
  truth <- pe$est[pe$lhs == "y1" & pe$op == "~~" & pe$rhs == "y1"]
  expect_lt(truth, 0)
  hey <- sieve_diagram_spec(fit, standardized = "none")
  expect_identical(hey$status$code, "improper")
  expect_true(sieve_diagram_drawable(hey))
  expect_match(hey$status$message, "negative variance estimate for y1")
  bad_var <- hey$edges[hey$edges$id == "y1~~y1", ]
  expect_identical(bad_var$value, truth)
  expect_true(bad_var$visible)
  expect_true(bad_var$improper)
  expect_match(bad_var$label_text, "^-")
  expect_identical(bad_var$label_text, formatC(truth, format = "f", digits = 2))
  expect_identical(hey$edges$id[hey$edges$improper], c("f=~y1", "y1~~y1"))
  expect_identical(hey$status$details$improper$id, c("f=~y1", "y1~~y1"))
  expect_false(hey$meta$admissible)
  expect_true(hey$meta$converged)
  expect_match(hey$meta$caption, "improper", fixed = TRUE)
  std <- sieve_diagram_update(hey, standardized = "std.all")
  expect_gt(std$edges$value[std$edges$id == "f=~y1"], 1)
  expect_lt(std$edges$value[std$edges$id == "y1~~y1"], 0)
  expect_edges_match_lavaan(std, fit, "std.all")
  expect_false(any(sieve_diagram_spec(dg_fit("one"))$edges$improper))

  expect_error(sieve_diagram_spec(mtcars), "must be a fitted lavaan model")
  expect_error(sieve_diagram_spec("model"), "must be a fitted lavaan model")
})

test_that("display toggles change what is drawn and nothing else", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("sem")
  coef_before <- lavaan::coef(fit)
  base <- sieve_diagram_spec(fit)
  position <- c("id", "x", "y", "w", "h")
  numbers <- c("id", "est", "se", "ci_lower", "ci_upper")
  changes <- list(
    list(residuals = FALSE), list(variances = FALSE), list(covariances = FALSE),
    list(residual_covariances = FALSE), list(intercepts = TRUE), list(fixed_zero = TRUE),
    list(min_loading = 0.7), list(standardized = "none"), list(standardized = "std.lv"),
    list(digits = 3L), list(analysis_id = "A4"),
    list(residuals = FALSE, covariances = FALSE, min_loading = 0.5, standardized = "none")
  )
  for (change in changes) {
    info <- paste(names(change), collapse = ", ")
    new <- do.call(sieve_diagram_update, c(list(base), change))
    expect_identical(new$nodes[, position], base$nodes[, position], info = info)
    expect_identical(new$edges[, numbers], base$edges[, numbers], info = info)
    expect_identical(new$parameters, base$parameters, info = info)
    expect_identical(new$edges$id, base$edges$id, info = info)
    expect_identical(new$edges$type, base$edges$type, info = info)
    expect_identical(new$edges$fixed, base$edges$fixed, info = info)
    # The same options applied to the fitted model give the same object, so
    # the app, a script, and an export cannot disagree.
    fresh <- do.call(sieve_diagram_spec, c(list(fit), change))
    expect_identical(new, fresh, info = info)
    expect_length(sieve_diagram_validate(new, error = FALSE), 0L)
  }
  expect_identical(lavaan::coef(fit), coef_before)

  # Each toggle hides exactly its own class of edges.
  hidden <- function(...) {
    s <- sieve_diagram_update(base, ...)
    s$edges[!s$edges$visible, ]
  }
  h <- hidden(residuals = FALSE)
  # The residual terms, their paths and variances, and the one covariance
  # between residuals (the disturbance covariance of the two outcomes).
  expect_setequal(unique(h$type), c("residual_path", "variance", "covariance"))
  terms <- h[h$type != "covariance", ]
  expect_true(all(grepl("^e\\.", terms$from)))
  expect_true(all(h$hidden_reason == "residuals"))
  expect_identical(nrow(terms), 2L * sum(base$nodes$kind == "residual"))
  expect_identical(h$id[h$type == "covariance"], "Speed~~t24_woody_mccall")
  h <- hidden(variances = FALSE)
  expect_identical(unique(h$type), "variance")
  expect_identical(nrow(h), nrow(dg_edges(base, "variance")))
  # The residual terms themselves stay when only the loops are hidden.
  expect_true(all(sieve_diagram_update(base, variances = FALSE)$nodes$visible))
  expect_identical(hidden(covariances = FALSE)$id, "Spatial~~Verbal")
  expect_identical(hidden(residual_covariances = FALSE)$id, "Speed~~t24_woody_mccall")
  expect_identical(nrow(hidden()), 0L)
  # digits changes the label only.
  d3 <- sieve_diagram_update(base, digits = 3L)
  expect_identical(d3$edges$value, base$edges$value)
  e <- base$edges$type == "loading"
  expect_identical(d3$edges$label_text[e], formatC(base$edges$value[e], format = "f", digits = 3))
  expect_identical(base$edges$label_text[e], formatC(base$edges$value[e], format = "f", digits = 2))
  # Stored values are at full precision, not rounded to the label.
  expect_true(all(base$edges$value[e] != round(base$edges$value[e], 4)))
  expect_true(all(base$edges$est[e] != round(base$edges$est[e], 4)))
  expect_identical(base$edges$est[e], lavaan::parameterEstimates(fit)$est[1:13])
})

test_that("min_loading filters loadings only", {
  skip_if_not_installed("lavaan")
  base <- sieve_diagram_spec(dg_fit("sem"))
  all_off <- sieve_diagram_update(base, min_loading = 5)
  gone <- all_off$edges[!all_off$edges$visible, ]
  expect_identical(unique(gone$type), "loading")
  expect_identical(nrow(gone), 13L)
  expect_true(all(gone$hidden_reason == "min_loading"))
  # Regressions, variances, and covariances smaller than the cutoff remain.
  kept <- all_off$edges[all_off$edges$visible, ]
  expect_true(all(c("regression", "variance", "covariance", "residual_path") %in% kept$type))
  expect_true(any(abs(kept$value[kept$type == "regression"]) < 0.3))
  expect_identical(nrow(dg_edges(all_off, "regression", visible = TRUE)), 4L)
  expect_true(all(all_off$nodes$visible))
  expect_identical(unname(all_off$meta$hidden$counts["min_loading"]), 13L)
  expect_setequal(all_off$meta$hidden$edges$id, gone$id)
  expect_match(all_off$meta$caption, "13 loadings with absolute value below 5", fixed = TRUE)
  # The cutoff is applied to the value on display.
  raw <- sieve_diagram_update(base, standardized = "none", min_loading = 0.9)
  shown <- raw$edges[raw$edges$type == "loading", ]
  expect_identical(shown$visible, abs(shown$est) >= 0.9)
  std <- sieve_diagram_update(base, min_loading = 0.7)
  shown <- std$edges[std$edges$type == "loading", ]
  expect_identical(shown$visible, abs(shown$std) >= 0.7)
  expect_true(any(shown$visible) && any(!shown$visible))
})

test_that("updating a specification never touches lavaan", {
  skip_if_not_installed("lavaan")
  skip_if(utils::packageVersion("testthat") < "3.2.0", "needs testthat mocking of another package")
  base <- sieve_diagram_spec(dg_fit("rescov"))
  refuse <- function(...) stop("lavaan was called")
  testthat::local_mocked_bindings(
    parTable = refuse, parameterEstimates = refuse, standardizedSolution = refuse,
    lavInspect = refuse, lavNames = refuse, fitMeasures = refuse, cfa = refuse,
    sem = refuse, lavaan = refuse,
    .package = "lavaan"
  )
  # The mock is in force: building a specification now fails.
  expect_error(sieve_diagram_build(dg_fit("rescov"), 1L, 0.95), "lavaan was called")
  new <- sieve_diagram_update(
    base,
    standardized = "none", residuals = FALSE, variances = FALSE, covariances = FALSE,
    residual_covariances = FALSE, intercepts = TRUE, fixed_zero = TRUE, min_loading = 0.4,
    label_mode = "short", wrap = 10L, digits = 3L, layout = "left_right",
    labels = c(t2_cubes = "Cubes"), name_map = c(Visual = "Visual ability"),
    positions = data.frame(id = "Speed", x = 1, y = 2), analysis_id = "A1"
  )
  expect_identical(new$status$code, "ok")
  expect_identical(new$meta$layout, "left_right")
  expect_identical(new$edges$est, base$edges$est)
  expect_true(all(is.na(new$edges$std)))
  expect_identical(invisible(sieve_diagram_code(new))[1], "diagram <- SIEVER::sieve_diagram_spec(")
  expect_output(print(new), "SIEVER path diagram specification")
})

test_that("every argument is validated, and unknown arguments are rejected", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("two_unit")
  base <- sieve_diagram_spec(fit)
  expect_error(sieve_diagram_spec(fit, colour = "red"), "unused argument")
  expect_error(sieve_diagram_update(base, colour = "red"), "Unknown diagram option\\(s\\): colour")
  expect_error(sieve_diagram_update(base, TRUE), "must be named")
  expect_error(sieve_diagram_update(base, digits = 2L, digits = 3L), "more than once")
  expect_error(sieve_diagram_update(base, group = 2L), "cannot be changed without the fitted model")
  expect_error(sieve_diagram_update(base, conf_level = 0.9), "cannot be changed without the fitted model")
  expect_error(sieve_diagram_update(list(), covariances = FALSE), "must be a diagram specification")
  expect_identical(sieve_diagram_update(base), base)
  bad <- list(
    list(standardized = "std.nox"), list(label_mode = "tiny"), list(layout = "circle"),
    list(residuals = NA), list(variances = "yes"), list(covariances = c(TRUE, FALSE)),
    list(residual_covariances = 1), list(intercepts = NULL), list(fixed_zero = NA),
    list(min_loading = -0.1), list(min_loading = "0.3"), list(min_loading = NA_real_),
    list(wrap = 2L), list(wrap = 10.5), list(digits = 0L), list(digits = 9L),
    list(digits = 2.5), list(group = 0L), list(group = c(1L, 2L)), list(group = NA),
    list(conf_level = 1), list(conf_level = 0), list(conf_level = "0.95"),
    list(name_map = c("unnamed")), list(labels = c(a = "x", a = "y")), list(labels = 1:3),
    list(positions = data.frame(id = "Visual", x = 0)),
    list(positions = data.frame(id = c("Visual", "Visual"), x = 0, y = 0)),
    list(positions = data.frame(id = "Visual", x = NA_real_, y = 0)),
    list(positions = data.frame(id = "nowhere", x = 0, y = 0)),
    list(analysis_id = c("A1", "A2")), list(analysis_id = NA)
  )
  for (arg in bad) {
    info <- paste(names(arg), paste(format(arg[[1]]), collapse = " "))
    expect_error(do.call(sieve_diagram_spec, c(list(fit), arg)), info = info)
  }
  expect_error(sieve_diagram_spec(fit, group = 2L), "the model has 1 group")
  expect_error(sieve_diagram_spec(fit, group = "Pasteur"), "is not a group of this model")
  expect_error(sieve_diagram_code(base, fit_name = ""), "single name")
  expect_error(sieve_diagram_code(list()), "must be a diagram specification")
})

test_that("labels, name maps, short labels, positions, and the confidence level take effect", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("two_unit")
  base <- sieve_diagram_spec(fit)
  expect_identical(base$nodes$label[base$nodes$kind != "residual"], c("Visual", "Textual", dg_v, dg_t))
  fx <- sieve_hs9_fixture()
  lab <- sieve_diagram_spec(fit, labels = fx$labels, name_map = c(Visual = "Visual ability", unused = "x"))
  expect_identical(dg_node(lab, dg_v)$label, unname(fx$labels[dg_v]))
  expect_identical(dg_node(lab, "Visual")$label, "Visual ability")
  expect_identical(dg_node(lab, "Textual")$label, "Textual")
  expect_identical(lab$nodes$id, base$nodes$id)
  expect_identical(lab$edges$id, base$edges$id)
  # labels takes precedence over the name map for the same variable.
  both <- sieve_diagram_spec(fit, labels = c(Visual = "From labels"), name_map = c(Visual = "From map"))
  expect_identical(dg_node(both, "Visual")$label, "From labels")
  short <- sieve_diagram_spec(fit, labels = fx$labels, label_mode = "short")
  expect_identical(dg_node(short, c(dg_v, dg_t))$label, paste0("V", 1:6))
  expect_identical(dg_node(short, "Visual")$label, "Visual")
  expect_identical(
    short$label_key,
    data.frame(
      id = c(dg_v, dg_t), short = paste0("V", 1:6),
      label = unname(fx$labels[c(dg_v, dg_t)]), stringsAsFactors = FALSE
    )
  )
  expect_lt(short$size$width, lab$size$width)
  # Names that are already short are their own short identifiers.
  expect_identical(
    sieve_diagram_spec(dg_fit("sim"), label_mode = "short")$label_key$short,
    paste0("q", 1:5)
  )

  # A name map attached to the fit by the structural builder is used.
  wordy <- stats::setNames(fx$theory_map, c("Visual ability", "Textual ability", "Speed of processing"))
  meta <- sieve_build_meta(fx$data, fx$item_cols, wordy, labels = fx$labels)
  path <- sieve_run_structural(fx$data[, fx$item_cols], fx$data, meta, outcomes = "Speed of processing", predictors = c("Visual ability", "Textual ability"))$fit
  expect_false(is.null(attr(path, "name_map")))
  named <- sieve_diagram_spec(path)
  expect_identical(
    named$nodes$id[named$nodes$kind == "latent"],
    c("Visual_ability", "Textual_ability", "Speed_of_processing")
  )
  expect_identical(
    named$nodes$label[named$nodes$kind == "latent"],
    c("Visual ability", "Textual ability", "Speed of processing")
  )
  expect_identical(named$meta$name_map[["Speed_of_processing"]], "Speed of processing")
  expect_null(named$meta$options$name_map)
  # An explicit map replaces it, and clearing the explicit map restores it.
  own <- sieve_diagram_update(named, name_map = c(Visual_ability = "Seeing"))
  expect_identical(
    dg_node(own, c("Visual_ability", "Textual_ability"))$label,
    c("Seeing", "Textual_ability")
  )
  expect_identical(sieve_diagram_update(own, name_map = NULL), named)
  expect_identical(named$meta$layout, "left_right")
  expect_identical(dg_edges(named, "regression")$to, rep("Speed_of_processing", 2))

  # positions: listed nodes go exactly where they are put, a residual term
  # follows its variable, and the rest stay.
  moved <- sieve_diagram_spec(fit, positions = data.frame(
    id = c("Visual", dg_v[1]), x = c(-9, -8), y = c(3, -4)
  ))
  expect_identical(c(dg_node(moved, "Visual")$x, dg_node(moved, "Visual")$y), c(-9, 3))
  expect_identical(c(dg_node(moved, dg_v[1])$x, dg_node(moved, dg_v[1])$y), c(-8, -4))
  offset <- dg_node(base, paste0("e.", dg_v[1]))[, c("x", "y")] - dg_node(base, dg_v[1])[, c("x", "y")]
  expect_equal(
    unlist(dg_node(moved, paste0("e.", dg_v[1]))[, c("x", "y")] - c(-8, -4)),
    unlist(offset)
  )
  others <- setdiff(base$nodes$id, c("Visual", dg_v[1], paste0("e.", dg_v[1])))
  expect_identical(dg_node(moved, others)[, c("x", "y")], dg_node(base, others)[, c("x", "y")])
  expect_identical(sieve_diagram_update(moved, positions = NULL), base)
  expect_error(
    sieve_diagram_update(base, positions = data.frame(id = "nowhere", x = 0, y = 0)),
    "not in the diagram"
  )

  # conf_level reaches the stored intervals.
  ninety <- sieve_diagram_spec(fit, conf_level = 0.90)
  expect_edges_match_lavaan(ninety, fit, "std.all", level = 0.90)
  free <- base$edges$free > 0L
  expect_true(all(ninety$edges$ci_upper[free] < base$edges$ci_upper[free]))
  expect_identical(ninety$meta$conf_level, 0.90)
  expect_match(ninety$meta$caption, "90% confidence intervals", fixed = TRUE)
  # analysis_id is recorded.
  expect_match(sieve_diagram_spec(fit, analysis_id = "A12")$meta$caption, "Analysis A12\\.$")
  expect_identical(sieve_diagram_spec(fit, analysis_id = 12)$meta$analysis_id, "12")
  expect_false(grepl("Analysis", base$meta$caption, fixed = TRUE))
  # layout: the measurement model turned on its side.
  side <- sieve_diagram_spec(fit, layout = "left_right")
  expect_identical(side$meta$layout, "left_right")
  expect_equal(dg_node(side, "Visual")$x, dg_node(side, "Textual")$x)
  expect_gt(dg_node(side, "Visual")$y, dg_node(side, "Textual")$y)
  expect_true(all(dg_node(side, dg_v)$x > dg_node(side, "Visual")$x))
  expect_identical(
    side$edges[, c("id", "est", "value", "label_text")],
    base$edges[, c("id", "est", "value", "label_text")]
  )
})

test_that("multiple groups: one group per diagram, named, with equality constraints kept", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("groups")
  g1 <- sieve_diagram_spec(fit, standardized = "none")
  g2 <- sieve_diagram_spec(fit, group = "Grant-White", standardized = "none")
  # A group can be named by its label or by its position.
  by_position <- sieve_diagram_spec(fit, group = 2L, standardized = "none")
  expect_identical(
    by_position[c("nodes", "edges", "parameters")],
    g2[c("nodes", "edges", "parameters")]
  )
  expect_identical(c(by_position$meta$group, g2$meta$group), c(2L, 2L))
  expect_edges_match_lavaan(g1, fit, "none", group = 1L)
  expect_edges_match_lavaan(g2, fit, "none", group = 2L)
  expect_edges_match_lavaan(sieve_diagram_spec(fit, group = 2L), fit, "std.all", group = 2L)
  expect_identical(c(g1$meta$group_label, g2$meta$group_label), c("Pasteur", "Grant-White"))
  expect_identical(c(g1$meta$nobs, g2$meta$nobs), c(156L, 145L))
  expect_identical(g1$meta$group_variable, "school")
  expect_match(g2$meta$caption, "Group shown: Grant-White (2 of 2).", fixed = TRUE)
  expect_true(all(g2$parameters$group %in% 2L))
  # The loadings constrained equal across groups carry one label in both.
  free_load <- c(paste0("Visual=~", dg_v[2:3]), paste0("Textual=~", dg_t[2:3]))
  c1 <- g1$edges$constraint[match(free_load, g1$edges$id)]
  c2 <- g2$edges$constraint[match(free_load, g2$edges$id)]
  expect_false(anyNA(c1))
  expect_identical(c1, c2)
  expect_identical(anyDuplicated(c1), 0L)
  expect_equal(g1$edges$est[match(free_load, g1$edges$id)],
    g2$edges$est[match(free_load, g2$edges$id)],
    tolerance = 1e-12
  )
  expect_true(all(is.na(g1$edges$constraint[!(g1$edges$id %in% free_load)])))
  expect_match(g1$meta$caption, "constrained equal", fixed = TRUE)
  # The groups differ where the model lets them.
  expect_false(isTRUE(all.equal(
    g1$edges$est[g1$edges$id == "Visual~~Textual"],
    g2$edges$est[g2$edges$id == "Visual~~Textual"]
  )))
  expect_identical(g1$edges$id, g2$edges$id)
  expect_identical(g1$nodes[, c("id", "x", "y")], g2$nodes[, c("id", "x", "y")])
  # A user label shared by two parameters is their constraint label.
  tau <- lavaan::cfa(paste0("f =~ a*", dg_t[1], " + a*", dg_t[2], " + ", dg_t[3]),
    data = as.data.frame(scale(sieve_hs9_fixture()$data[, dg_t])), std.lv = TRUE
  )
  expect_identical(dg_edges(sieve_diagram_spec(tau), "loading")$constraint, c("a", "a", NA))
})

test_that("cross-loadings, covariates, and path models give valid specifications", {
  skip_if_not_installed("lavaan")
  HS <- lavaan::HolzingerSwineford1939
  # An indicator that loads on two factors is one node with two loadings,
  # placed with the first factor that lists it.
  cross <- lavaan::cfa("f1 =~ x1 + x2 + x3 + x4\nf2 =~ x4 + x5 + x6", HS, std.lv = TRUE)
  spec <- sieve_diagram_spec(cross)
  expect_identical(sum(spec$nodes$id == "x4"), 1L)
  expect_identical(dg_edges(spec, "loading")$from[dg_edges(spec, "loading")$to == "x4"], c("f1", "f2"))
  expect_identical(dg_node(spec, "x4")$parent, "f1")
  expect_identical(sum(spec$nodes$kind == "residual"), 6L)
  expect_edges_match_lavaan(spec, cross, "std.all")
  # A standardized loading is flagged out of range only for a variable with
  # a single predictor, so a cross-loading is judged by its residual variance.
  expect_false(any(spec$edges$improper))

  # Observed covariates: their variances and covariance are sample moments
  # (neither estimated nor fixed by the analyst), and their covariance is
  # an exogenous covariance.
  mimic <- lavaan::sem("f =~ x1 + x2 + x3\nf ~ ageyr + grade\nx3 ~ ageyr", HS)
  spec <- sieve_diagram_spec(mimic, standardized = "none")
  expect_identical(spec$status$code, "ok")
  expect_identical(dg_node(spec, c("ageyr", "grade"))$role, rep("predictor", 2))
  expect_identical(dg_node(spec, c("ageyr", "grade"))$kind, rep("observed", 2))
  expect_identical(dg_node(spec, "x3")$role, "indicator")
  expect_identical(dg_node(spec, "e.f")$role, "disturbance")
  expect_identical(dg_node(spec, "e.x3")$role, "unique")
  cov <- dg_edges(spec, "covariance")
  expect_identical(cov$id, "ageyr~~grade")
  expect_identical(cov$cov_class, "exogenous")
  expect_true(cov$derived && !cov$fixed && cov$free == 0L)
  expect_identical(cov$label_text, formatC(cov$est, format = "f", digits = 2))
  reg <- dg_edges(spec, "regression")
  expect_identical(paste(reg$from, "->", reg$to), c("ageyr -> f", "grade -> f", "ageyr -> x3"))
  expect_lt(dg_node(spec, "ageyr")$x, dg_node(spec, "f")$x)
  expect_identical(dg_overlaps(spec), 0L)

  # A path model has no latent variables, and a covariance between two
  # outcomes is a covariance of their disturbances.
  path <- lavaan::sem("x1 ~ x2 + x3\nx4 ~ x1 + x2\nx1 ~~ x4", HS[, paste0("x", 1:4)])
  spec <- sieve_diagram_spec(path)
  expect_identical(spec$meta$model_kind, "path")
  expect_true(is.na(spec$meta$identification))
  expect_identical(sum(spec$nodes$kind == "latent"), 0L)
  cov <- dg_edges(spec, "covariance")
  expect_identical(cov$cov_class[match(c("x1~~x4", "x2~~x3"), cov$id)], c("disturbance", "exogenous"))
  expect_identical(
    unlist(cov[cov$id == "x1~~x4", c("from", "to")], use.names = FALSE),
    c("e.x1", "e.x4")
  )
  expect_identical(dg_node(spec, paste0("x", 1:4))$depth, c(1L, 0L, 0L, 2L))
  expect_match(spec$meta$caption, "^Path model: 4 observed variables")
  expect_edges_match_lavaan(spec, path, "std.all")
  expect_identical(dg_overlaps(spec), 0L)
})

test_that("the reproduction call regenerates the specification exactly", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("three")
  plain <- sieve_diagram_spec(fit)
  expect_identical(
    sieve_diagram_code(plain),
    "diagram <- SIEVER::sieve_diagram_spec(fit, standardized = \"std.all\")"
  )
  spec <- sieve_diagram_spec(
    fit,
    standardized = "none", min_loading = 0.3, labels = sieve_hs9_fixture()$labels,
    name_map = c(Visual = "Visual ability"), residual_covariances = FALSE, wrap = 20L,
    digits = 3L, layout = "left_right", conf_level = 0.9, analysis_id = "A3",
    positions = data.frame(id = "Visual", x = -1 / 3, y = 5.1)
  )
  code <- sieve_diagram_code(spec, fit_name = "cfa_fit", spec_name = "figure_spec")
  expect_match(code[1], "^figure_spec <- SIEVER::sieve_diagram_spec\\($")
  expect_true(any(grepl("min_loading = 0.3", code, fixed = TRUE)))
  expect_false(any(grepl("covariances = TRUE", code, fixed = TRUE)))
  env <- new.env(parent = globalenv())
  assign("cfa_fit", fit, envir = env)
  code <- sub("SIEVER::sieve_diagram_spec", "sieve_diagram_spec", code, fixed = TRUE)
  assign("sieve_diagram_spec", sieve_diagram_spec, envir = env)
  eval(parse(text = paste(code, collapse = "\n")), envir = env)
  expect_identical(env$figure_spec, spec)
})

test_that("a specification draws nothing, uses no random numbers, and leaves no file behind", {
  skip_if_not_installed("lavaan")
  fit <- dg_fit("three")
  withr::local_preserve_seed()
  set.seed(113)
  before <- get(".Random.seed", envir = globalenv())
  devices <- grDevices::dev.list()
  files <- list.files(getwd(), all.files = TRUE)
  spec <- sieve_diagram_spec(fit)
  spec <- sieve_diagram_update(spec, covariances = FALSE, layout = "left_right")
  invisible(sieve_diagram_code(spec))
  utils::capture.output(print(spec))
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_identical(grDevices::dev.list(), devices)
  expect_identical(list.files(getwd(), all.files = TRUE), files)
  expect_false(file.exists("Rplots.pdf"))
  # With no stream in the session, none is created.
  rm(".Random.seed", envir = globalenv())
  invisible(sieve_diagram_spec(fit))
  expect_false(exists(".Random.seed", envir = globalenv()))
})

test_that("printing reports status, counts, size, and caption", {
  skip_if_not_installed("lavaan")
  spec <- sieve_diagram_spec(dg_fit("two_unit"))
  out <- utils::capture.output(shown <- print(spec))
  expect_identical(shown, spec)
  expect_match(out[1], "SIEVER path diagram specification", fixed = TRUE)
  expect_true(any(grepl("Status: ok.", out, fixed = TRUE)))
  expect_true(any(grepl("Nodes: 14 (2 latent, 6 observed, 6 residual)", out, fixed = TRUE)))
  expect_true(any(grepl("Edges: 21 drawn of 21", out, fixed = TRUE)))
  expect_true(any(grepl("Layout: top_down", out, fixed = TRUE)))
  expect_true(any(grepl("Caption: Confirmatory factor model: 2 latent and 6 observed variables", out,
    fixed = TRUE
  )))
})

test_that("the validator catches a specification that renderers could not trust", {
  skip_if_not_installed("lavaan")
  spec <- sieve_diagram_spec(dg_fit("two_unit"))
  expect_true(sieve_diagram_validate(spec))
  expect_problem <- function(change, pattern) {
    problems <- sieve_diagram_validate(change(spec), error = FALSE)
    expect_true(any(grepl(pattern, problems, fixed = TRUE)), info = pattern)
  }
  expect_problem(function(s) {
    s$edges$to[1] <- "nowhere"
    s
  }, "does not exist")
  expect_problem(function(s) {
    s$nodes$id[2] <- s$nodes$id[1]
    s
  }, "node ids are not unique")
  expect_problem(function(s) {
    s$edges$visible[1] <- FALSE
    s
  }, "hidden without a reason")
  expect_problem(function(s) {
    s$nodes$x[3] <- NA
    s
  }, "no position")
  expect_problem(function(s) {
    s$edges$type[1] <- "arrow"
    s
  }, "unknown type")
  expect_problem(
    function(s) {
      s$edges$est[s$edges$type == "residual_path"][1] <- 0.5
      s
    },
    "not a structural path fixed at 1"
  )
  expect_problem(
    function(s) {
      s$nodes$visible[s$nodes$kind == "residual"] <- FALSE
      s
    },
    "joins a node that is not drawn"
  )
  expect_problem(function(s) {
    s$edges$cov_class[1] <- "exogenous"
    s
  }, "misplaced")
  expect_problem(function(s) {
    s$status$code <- "failed"
    s
  }, "cannot be drawn carries nodes")
  expect_problem(function(s) {
    s$edges$curve <- NULL
    s
  }, "lacks column")
  expect_error(sieve_diagram_validate(list()), "not an object of class sieve_diagram")
  expect_error(sieve_diagram_validate({
    s <- spec
    s$status$code <- "fine"
    s
  }), "status code")
})

test_that("the specification records its provenance", {
  skip_if_not_installed("lavaan")
  # A model written in the call is recorded verbatim.
  literal <- lavaan::cfa("trait =~ q1 + q2 + q3 + q4 + q5", dg_sim_data(), std.lv = TRUE)
  spec <- sieve_diagram_spec(literal, analysis_id = "C11")
  expect_identical(spec$meta$versions$lavaan, as.character(utils::packageVersion("lavaan")))
  expect_identical(spec$meta$versions$SIEVER, sieve_version())
  expect_identical(spec$meta$model, dg_syntax("sim"))
  expect_identical(spec$meta$model_source, "call")
  expect_identical(spec$meta$analysis_id, "C11")
  expect_identical(spec$meta$nobs, 400L)
  expect_identical(spec$meta$ngroups, 1L)
  expect_true(spec$meta$converged && spec$meta$admissible)
  expect_identical(spec$meta$options$standardized, "std.all")
  expect_identical(names(spec$meta$options), names(formals(sieve_diagram_spec))[-1])
  # The simulated data are reproducible from the recorded seed.
  expect_identical(dg_sim_data(), dg_sim_data())
  # A model passed as an object is rebuilt from the parameter table.
  rebuilt <- sieve_diagram_spec(dg_fit("five"))
  expect_identical(rebuilt$meta$model_source, "parameter_table")
  expect_match(rebuilt$meta$model, "^Spatial =~ t1_visual_perception \\+ t2_cubes")
  expect_identical(length(strsplit(rebuilt$meta$model, "\n", fixed = TRUE)[[1]]), 5L)
})

# The cases the path diagram is tested on (cases C01 to C12).
#
# One definition serves the renderer tests (test-diagram-render.R). Each case
# records what its provenance needs: the model syntax, the data, the seed
# when data are simulated, and the display options.
#
# Data. C01 to C10 and the non-converged fit of C12 use the bundled
# Holzinger and Swineford fixtures, which involve no random numbers. C11 is
# simulated on a local random number stream seeded with 113. The Heywood
# case of C12 is a stated covariance matrix, so no sampling is involved.

diagram_case_cache <- new.env(parent = emptyenv())

diagram_case_syntax <- function(name) {
  v <- c("t1_visual_perception", "t2_cubes", "t4_lozenges")
  t <- c("t6_paragraph_comprehension", "t7_sentence", "t9_word_meaning")
  s <- c("t10_addition", "t12_counting_groups_of_dots", "t13_straight_and_curved_capitals")
  verbal5 <- c(
    "t5_general_information", "t6_paragraph_comprehension", "t7_sentence",
    "t8_word_classification", "t9_word_meaning"
  )
  line <- function(factor, items) paste0(factor, " =~ ", paste(items, collapse = " + "))
  two <- paste(line("Visual", v), line("Textual", t), sep = "\n")
  three <- paste(two, line("Speed", s), sep = "\n")
  switch(name,
    one = line("Verbal", verbal5),
    two = two,
    three = three,
    five = {
      map <- sieve_hs_fixture()$theory_map
      paste(vapply(names(map), function(f) line(f, map[[f]]), character(1)), collapse = "\n")
    },
    higher = paste(three, "g =~ Visual + Textual + Speed", sep = "\n"),
    rescov = paste(three, "t10_addition ~~ t12_counting_groups_of_dots", sep = "\n"),
    sem = paste(
      line("Spatial", c(
        "t1_visual_perception", "t2_cubes", "t3_paper_form_board",
        "t4_lozenges"
      )),
      line("Verbal", verbal5),
      line("Speed", c(
        "t10_addition", "t11_code", "t12_counting_groups_of_dots",
        "t13_straight_and_curved_capitals"
      )),
      "Speed ~ Spatial + Verbal",
      "t24_woody_mccall ~ Spatial + Verbal",
      "Speed ~~ t24_woody_mccall",
      sep = "\n"
    ),
    mediation = paste(three, "Textual ~ Visual", "Speed ~ Visual + Textual", sep = "\n"),
    sim = "trait =~ q1 + q2 + q3 + q4 + q5",
    heywood = "f =~ y1 + y2 + y3"
  )
}

# Five items with one near-zero and one negative loading (seed 113).
diagram_case_sim_data <- function() {
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

# The first six primary tests cut at their quartiles (four ordered
# categories) or at their medians (binary).
diagram_case_ordered_data <- function(binary = FALSE) {
  d <- sieve_hs9_fixture()$data[, sieve_hs9_fixture()$item_cols[1:6]]
  d[] <- lapply(d, function(x) {
    if (binary) {
      as.integer(x > stats::median(x))
    } else {
      as.integer(cut(x, stats::quantile(x, 0:4 / 4), include.lowest = TRUE))
    }
  })
  d
}

# Each fit is made once. A captured fit keeps the warnings lavaan raised.
diagram_case_fit <- function(name) {
  if (!is.null(diagram_case_cache[[name]])) {
    return(diagram_case_cache[[name]])
  }
  d9 <- sieve_hs9_fixture()$data
  d24 <- sieve_hs_fixture()$data
  fit <- switch(name,
    one = lavaan::cfa(diagram_case_syntax("one"), d24, std.lv = TRUE),
    two_unit = lavaan::cfa(diagram_case_syntax("two"), d9, std.lv = TRUE),
    two_marker = lavaan::cfa(diagram_case_syntax("two"), d9, std.lv = FALSE),
    two_orthogonal = lavaan::cfa(diagram_case_syntax("two"), d9,
      std.lv = TRUE,
      orthogonal = TRUE
    ),
    three = lavaan::cfa(diagram_case_syntax("three"), d9,
      std.lv = TRUE, estimator = "MLR",
      missing = "fiml"
    ),
    five = lavaan::cfa(diagram_case_syntax("five"), d24,
      std.lv = TRUE, estimator = "MLR",
      missing = "fiml"
    ),
    higher = lavaan::cfa(diagram_case_syntax("higher"), d9, std.lv = TRUE),
    sem = lavaan::sem(diagram_case_syntax("sem"), d24, std.lv = TRUE),
    mediation = lavaan::sem(diagram_case_syntax("mediation"), d9, std.lv = TRUE),
    rescov = lavaan::cfa(diagram_case_syntax("rescov"), d9, std.lv = TRUE),
    ordered = lavaan::cfa(diagram_case_syntax("two"), diagram_case_ordered_data(),
      ordered = TRUE, estimator = "WLSMV", std.lv = TRUE
    ),
    binary = lavaan::cfa(diagram_case_syntax("two"), diagram_case_ordered_data(binary = TRUE),
      ordered = TRUE, estimator = "WLSMV", std.lv = TRUE
    ),
    sim = lavaan::cfa(diagram_case_syntax("sim"), diagram_case_sim_data(), std.lv = TRUE),
    sim_no_se = lavaan::cfa(diagram_case_syntax("sim"), diagram_case_sim_data(),
      std.lv = TRUE,
      se = "none"
    ),
    heywood = {
      # r12 * r13 / r23 = 1.28: the first loading squared exceeds the
      # variance, so the first residual variance must be negative.
      S <- matrix(c(1, 0.8, 0.8, 0.8, 1, 0.5, 0.8, 0.5, 1), 3,
        dimnames = list(c("y1", "y2", "y3"), c("y1", "y2", "y3"))
      )
      sieve_capture(lavaan::cfa(diagram_case_syntax("heywood"),
        sample.cov = S,
        sample.nobs = 200, std.lv = TRUE
      ))$value
    },
    not_converged = sieve_capture(lavaan::cfa(diagram_case_syntax("three"), d9,
      std.lv = TRUE,
      control = list(iter.max = 2L)
    ))$value,
    none = NULL
  )
  if (!is.null(fit)) {
    diagram_case_cache[[name]] <- fit
  }
  fit
}

# Where the data of each fit come from, for the provenance record.
diagram_case_data <- function(fit) {
  hs9 <- "sieve_hs9_fixture(): Holzinger and Swineford, primary 9 tests, n = 301"
  hs24 <- "sieve_hs_fixture(): Holzinger and Swineford, 24 tests, n = 301"
  switch(fit,
    one = ,
    five = ,
    sem = hs24,
    two_unit = ,
    two_marker = ,
    two_orthogonal = ,
    three = ,
    higher = ,
    rescov = ,
    mediation = hs9,
    ordered = paste0(hs9, "; first six tests cut at their quartiles (4 ordered categories)"),
    binary = paste0(hs9, "; first six tests cut at their medians (binary)"),
    sim = ,
    sim_no_se = "simulated, n = 400, one factor with loadings 0.80, 0.70, 0.02, -0.60, 0.75",
    heywood = "stated correlation matrix (r12 = r13 = 0.8, r23 = 0.5), n = 200",
    not_converged = paste0(hs9, "; optimizer stopped after 2 iterations"),
    none = "none"
  )
}

diagram_case_long_labels <- function() {
  c(
    q1 = "I feel that the work I do in this organization is recognized by my supervisor",
    q2 = "My colleagues and I share information openly when a problem arises",
    q3 = "The cafeteria offers a reasonable variety of lunch options",
    q4 = "I often think about leaving this organization for another employer",
    q5 = "Decisions that affect my work are explained to me in a timely way"
  )
}

# One record per figure: `name` is the file stem, `fit` the key of the
# fitted model, `spec` the arguments of sieve_diagram_spec() beyond the
# fit, `draw` the arguments of sieve_draw_diagram() beyond the
# specification, and `drawable` whether a figure is expected at all.
diagram_cases <- function() {
  hs <- sieve_hs_fixture()$labels
  case <- function(name, description, fit, syntax, seed = NA_integer_, spec = list(),
                   draw = list(), drawable = TRUE) {
    list(
      name = name, case = substr(name, 1L, 3L), description = description, fit = fit,
      model = if (is.na(syntax)) NA_character_ else diagram_case_syntax(syntax),
      data = diagram_case_data(fit), seed = seed, spec = spec, draw = draw,
      drawable = drawable
    )
  }
  cases <- list(
    case("C01_one_factor",
      "One factor, five indicators, unit-variance identification; std.all.",
      "one", "one",
      spec = list(labels = hs)
    ),
    case("C02_two_factors_unit_variance",
      "Two correlated factors, unit-variance identification; std.all.",
      "two_unit", "two",
      spec = list(labels = hs)
    ),
    case("C03_two_factors_marker_unstandardized",
      "The same model with marker identification, unstandardized: loadings fixed at 1, free factor variances.",
      "two_marker", "two",
      spec = list(labels = hs, standardized = "none")
    ),
    case("C03_two_factors_marker_std_all",
      "Marker identification shown fully standardized: the marker loadings are not 1 and stay marked as fixed.",
      "two_marker", "two",
      spec = list(labels = hs, standardized = "std.all")
    ),
    case("C04_orthogonal_factors",
      "Two orthogonal factors: no covariance edge; the fixed zero is named in the caption.",
      "two_orthogonal", "two",
      spec = list(labels = hs)
    ),
    case("C04_orthogonal_factors_fixed_zero_drawn",
      "Two orthogonal factors with the fixed-zero covariance drawn on request, in the fixed style.",
      "two_orthogonal", "two",
      spec = list(labels = hs, fixed_zero = TRUE)
    ),
    case("C05_three_factors",
      "Three correlated factors, nine indicators (MLR, FIML); std.all.",
      "three", "three",
      spec = list(labels = hs)
    ),
    case("C05_three_factors_monochrome",
      "The three-factor model in black and white.",
      "three", "three",
      spec = list(labels = hs), draw = list(monochrome = TRUE)
    ),
    case("C06_five_factors_24_indicators",
      "Five factors, 24 indicators, full item labels, top-down layout: the size follows the content.",
      "five", "five",
      spec = list(labels = hs)
    ),
    case("C06_five_factors_24_indicators_short_labels",
      "Five factors, 24 indicators, short item labels (the key is in the specification).",
      "five", "five",
      spec = list(labels = hs, label_mode = "short")
    ),
    case("C06_five_factors_24_indicators_left_right",
      "Five factors, 24 indicators, full item labels, left-to-right layout.",
      "five", "five",
      spec = list(labels = hs, layout = "left_right", wrap = 40L)
    ),
    case("C06_five_factors_24_indicators_short_left_right",
      "Five factors, 24 indicators, short item labels, left-to-right layout: the most compact complete view.",
      "five", "five",
      spec = list(labels = hs, layout = "left_right", label_mode = "short")
    ),
    case("C07_higher_order",
      "Second-order factor measured by three first-order factors, with first-order disturbances.",
      "higher", "higher",
      spec = list(labels = hs)
    ),
    case("C08_structural_left_right",
      "Two latent predictors, a latent outcome and an observed outcome: exogenous covariance and disturbance covariance; left-to-right layout.",
      "sem", "sem",
      spec = list(labels = hs, layout = "left_right")
    ),
    case("C08_structural_top_down",
      "The structural model in the top-down layout, with short item labels.",
      "sem", "sem",
      spec = list(labels = hs, layout = "top_down", label_mode = "short")
    ),
    case("C08_mediation_left_right",
      "A predictor, a mediator and an outcome, all latent: the direct effect has its own arrow, clear of the mediator; left-to-right layout.",
      "mediation", "mediation",
      spec = list(labels = hs, layout = "left_right")
    ),
    case("C08_mediation_top_down",
      "The mediation model in the top-down layout.",
      "mediation", "mediation",
      spec = list(labels = hs, layout = "top_down")
    ),
    case("C09_residual_covariance",
      "Three factors with one residual covariance between two indicators, joined at their unique terms.",
      "rescov", "rescov",
      spec = list(labels = hs)
    ),
    case("C09_residual_covariance_factor_covariances_hidden",
      "The factor-covariance toggle off: the residual covariance stays.",
      "rescov", "rescov",
      spec = list(labels = hs, covariances = FALSE)
    ),
    case("C10_ordered_wlsmv_delta",
      "Ordered indicators with four categories (WLSMV, delta parameterization): residual variances implied, thresholds in the table.",
      "ordered", "two",
      spec = list(labels = hs)
    ),
    case("C10_ordered_wlsmv_delta_thresholds",
      "The ordered model with thresholds shown on request.",
      "ordered", "two",
      spec = list(labels = hs, intercepts = TRUE)
    ),
    case("C10_binary_wlsmv_delta",
      "Binary indicators (WLSMV, delta parameterization).",
      "binary", "two",
      spec = list(labels = hs)
    ),
    case("C11_long_labels_negative_loading",
      "Item labels of 57 to 77 characters (wrapped automatically into at most four lines), a near-zero and a negative loading (simulated).",
      "sim", "sim",
      seed = 113L,
      spec = list(
        labels = diagram_case_long_labels(),
        name_map = c(trait = "Organizational engagement")
      )
    ),
    case("C11_long_labels_left_right",
      "The same long labels in the left-to-right layout, one line each.",
      "sim", "sim",
      seed = 113L,
      spec = list(
        labels = diagram_case_long_labels(), layout = "left_right", wrap = Inf,
        name_map = c(trait = "Organizational engagement")
      )
    ),
    case("C11_no_standard_errors",
      "The same model fit without standard errors: the caption says uncertainty is unknown.",
      "sim_no_se", "sim",
      seed = 113L,
      spec = list(
        labels = diagram_case_long_labels(), wrap = 26L,
        name_map = c(trait = "Organizational engagement")
      )
    ),
    case("C12_heywood",
      "Improper solution (negative residual variance): drawn as estimated, and flagged.",
      "heywood", "heywood",
      spec = list(standardized = "none")
    ),
    case("C12_heywood_std_all",
      "The improper solution fully standardized: a standardized loading above 1 and a negative standardized variance, both flagged.",
      "heywood", "heywood",
      spec = list(standardized = "std.all")
    ),
    case("C12_no_model",
      "No fitted model: a structured failure, no figure.",
      "none", NA,
      drawable = FALSE
    ),
    case("C12_not_converged",
      "A fit that did not converge: a structured failure, no figure.",
      "not_converged", "three",
      drawable = FALSE
    )
  )
  stats::setNames(cases, vapply(cases, function(x) x$name, character(1)))
}

diagram_case_spec <- function(case) {
  do.call(sieve_diagram_spec, c(list(diagram_case_fit(case$fit)), case$spec))
}

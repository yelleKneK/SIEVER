# Model identity and fit status. Three properties are pinned here:
#
# * a latent variable can never take the name of an observed variable, and a
#   fitted model whose variables are not the ones specified is refused;
# * every fit wrapper reports a status (failed, not converged, not
#   identified, inadmissible, ok), and identification is judged for the
#   model as a whole rather than by a count of indicators;
# * the item diagnostics work for an ordinary single-construct scale.
#
# Expected values come from sources that do not pass through the function
# under test: a count of moments and parameters, stats::factanal, psych::fa
# called directly, lavaan called directly on hand-written syntax, and
# correlations computed by hand.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

skip_if_not_installed("DMAR")
skip_if_not_installed("lavaan")
skip_if_not_installed("psych")

# A ledger for a plain data frame: one construct, every item included.
identity_meta <- function(x, construct = "F", type = "continuous", facet = "") {
  data.frame(
    item = names(x), label = names(x),
    construct = rep(construct, length.out = ncol(x)),
    facet = rep(facet, length.out = ncol(x)),
    type = rep(type, length.out = ncol(x)),
    status = "active", reverse = FALSE, stringsAsFactors = FALSE
  )
}

# One-factor data: six indicators, n = 500, loadings .5 .6 .7 .8 .65 .75.
one_factor_n500 <- function() {
  withr::with_seed(20260929, {
    f <- stats::rnorm(500)
    x <- as.data.frame(sapply(
      c(.5, .6, .7, .8, .65, .75),
      function(l) l * f + sqrt(1 - l * l) * stats::rnorm(500)
    ))
  })
  names(x) <- paste0("x", 1:6)
  x
}

hs_parts <- function() {
  fx <- sieve_hs9_fixture()
  list(
    fx = fx, items = fx$data[, fx$item_cols, drop = FALSE],
    meta = sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  )
}

identity_state <- function(meta = NULL, raw = NULL) {
  hs <- hs_parts()
  if (!is.null(raw)) hs$fx$data <- raw
  meta <- meta %||% hs$meta
  siever_test_study(hs$fx, theory = FALSE, configure = function(s) {
    s$meta <- meta
    roles <- sieve_declared_roles(s$raw, names(s$items))
    roles$role[roles$role == "auxiliary"] <- "criterion"
    s$roles <- roles
  })
}

identity_text <- function(x) {
  if (is.null(x)) "" else paste(as.character(unlist(x)), collapse = " ")
}

# ---- The name space ---------------------------------------------------------

test_that("allocated names are syntactic, distinct, and clear of every reserved name", {
  labels <- c(
    "A-B", "A_B", "A B", "9lives", "\u5fc3\u7406", "\u00e9", "\u80fd\u529b",
    "general", "NA", "function", "if", "TRUE", "c", "start", "t1", "x1",
    "Spatial-empirical", "_lead", ".dot", "St\u00e4rke", "", "  "
  )
  taken <- c("x1", "x2", "A_B_1", "F_9lives")
  got <- SIEVER:::sieve_allocate_names(labels, taken = taken)
  expect_identical(names(got), labels)
  # An independent statement of "usable": an R syntactic name made only of
  # ASCII letters, digits, underscores and periods, starting with a letter.
  expect_true(all(grepl("^[A-Za-z][A-Za-z0-9_.]*$", got)))
  expect_identical(make.names(unname(got)), unname(got))
  expect_false(any(duplicated(got)))
  expect_length(intersect(got, taken), 0L)
  expect_length(intersect(got, SIEVER:::sieve_reserved_words()), 0L)
  expect_false(any(grepl("^t[0-9]+$", got)))
  # Ordinary labels keep a readable stem.
  expect_identical(unname(got[c("A-B", "Spatial-empirical")]), c("A_B", "Spatial_empirical"))
  expect_identical(
    unname(SIEVER:::sieve_allocate_names(c("Visual", "Textual", "Speed"))),
    c("Visual", "Textual", "Speed")
  )
  expect_identical(
    unname(unlist(sieve_allocate_names(c("A-B", "A_B", "A B")))),
    c("A_B", "A_B_1", "A_B_2")
  )
  # The same labels always get the same names.
  expect_identical(SIEVER:::sieve_allocate_names(labels, taken = taken), got)
  # lavaan itself accepts every allocated name as a latent variable and
  # reads the model as intended (three indicators, one latent).
  for (nm in unname(got)) {
    pt <- lavaan::lavaanify(sprintf("%s =~ ind_a + ind_b + ind_c", nm))
    expect_identical(lavaan::lavNames(pt, "lv"), nm)
    expect_identical(lavaan::lavNames(pt, "ov"), c("ind_a", "ind_b", "ind_c"))
  }
})

test_that("the name plan separates constructs, facets, the general factor and observed variables", {
  x <- as.data.frame(matrix(0, 2, 8, dimnames = list(NULL, c(paste0("v", 1:6), "general_factor", "Speed"))))
  meta <- identity_meta(
    x,
    construct = c("v1", "v1", "v1", "Visual.a", "Visual.a", "Visual", "Visual", "general"),
    facet = c("", "", "", "", "", "a", "a", "")
  )
  plan <- SIEVER:::sieve_name_plan(meta, reserved = c("outcome", "general"))
  all_names <- c(plan$construct, plan$facet$name, plan$general)
  expect_false(any(duplicated(all_names)))
  expect_length(intersect(all_names, c(meta$item, "outcome", "general")), 0L)
  # A construct named like an item, like another construct's facet name
  # (Visual + a), and "general" each get a name of their own.
  expect_false(plan$construct[["v1"]] == "v1")
  expect_false(plan$construct[["Visual.a"]] == plan$facet$name[plan$facet$construct == "Visual"])
  expect_false(plan$construct[["general"]] == "general")
  # "general" and "general_factor" are observed here, so the general factor
  # moves on to the next free name.
  expect_false(plan$general %in% c("general", "general_factor"))
  expect_identical(plan$facet$label, "Visual: a")
  # Excluding an item does not rename a factor.
  parked <- meta
  parked$status[1:3] <- "parked"
  expect_identical(
    SIEVER:::sieve_name_plan(parked, reserved = c("outcome", "general"))$construct,
    plan$construct
  )
})

test_that("a construct named like an item keeps every indicator", {
  x <- one_factor_n500()
  m <- identity_meta(x)

  # The naive syntax, with the construct's label as the factor's name, given
  # to lavaan directly, is a five indicator model: x1 is taken as the
  # factor's name and leaves the observed variables (df 4, chi-square
  # 1.004). lavaan warns about the collision, and a wrapper that discarded
  # the warning would hide it.
  before <- SIEVER:::sieve_capture(lavaan::cfa(
    "x1 =~ x1 + x2 + x3 + x4 + x5 + x6",
    data = x, estimator = "MLR",
    std.lv = TRUE, missing = "fiml"
  ))
  old <- before$value
  expect_true(any(grepl("collide with observed variable names", gsub("\\s+", " ", before$warnings),
    fixed = TRUE
  )))
  expect_identical(lavaan::lavNames(old, "ov"), paste0("x", 2:6))
  expect_equal(unname(lavaan::fitMeasures(old, "df")), 4)
  expect_equal(unname(lavaan::fitMeasures(old, "chisq")), 1.004, tolerance = 5e-4)
  # That object does not pass the status check.
  expect_identical(SIEVER:::sieve_fit_status(old)$code, "not_identified")
  expect_true(SIEVER:::sieve_fit_status(old)$information_singular)

  # SIEVER's builder, on the same ledger with the construct labeled x1.
  collide <- m
  collide$construct <- "x1"
  syntax <- sieve_cfa_plan(collide)$model
  expect_false(grepl("^x1 =~", syntax))
  expect_setequal(SIEVER:::sieve_model_observed(syntax), names(x))
  fit <- sieve_fit_cfa(x, collide)
  expect_s4_class(fit, "lavaan")
  expect_identical(lavaan::lavNames(fit, "ov"), names(x))
  expect_identical(unname(attr(fit, "name_map")), "x1")
  expect_identical(SIEVER:::sieve_show_names(lavaan::lavNames(fit, "lv"), fit), "x1")

  # Degrees of freedom by counting: 6 means and 21 variances and covariances,
  # less 6 loadings, 6 residual variances and 6 intercepts.
  p <- 6
  expect_equal(unname(lavaan::fitMeasures(fit, "df")), (p + p * (p + 1) / 2) - 3 * p)
  expect_equal(unname(lavaan::fitMeasures(fit, "df")), 9)
  # Chi-square from an implementation that shares nothing with lavaan: n times
  # the maximum likelihood discrepancy of the one-factor model, from
  # stats::factanal (8.277).
  fa <- stats::factanal(x, factors = 1)
  expect_equal(unname(lavaan::fitMeasures(fit, "chisq")),
    nrow(x) * fa$criteria[["objective"]],
    tolerance = 1e-5
  )
  expect_equal(unname(lavaan::fitMeasures(fit, "chisq")), 8.277, tolerance = 5e-4)
  # And the label makes no difference to the model: the same fit as with "F".
  plain <- sieve_fit_cfa(x, m)
  expect_equal(unname(lavaan::fitMeasures(fit, c("chisq", "df", "chisq.scaled"))),
    unname(lavaan::fitMeasures(plain, c("chisq", "df", "chisq.scaled"))),
    tolerance = 1e-9
  )
  expect_equal(unname(lavaan::coef(fit)), unname(lavaan::coef(plain)), tolerance = 1e-9)

  # A second data set from an independent seed.
  v <- withr::with_seed(114, {
    f <- stats::rnorm(600)
    as.data.frame(sapply(seq(.50, .75, .05), function(l) l * f + sqrt(1 - l * l) * stats::rnorm(600)))
  })
  names(v) <- paste0("v", 1:6)
  mv <- identity_meta(v, construct = "v1")
  fv <- sieve_fit_cfa(v, mv)
  expect_identical(lavaan::lavNames(fv, "ov"), names(v))
  expect_equal(unname(lavaan::fitMeasures(fv, "df")), 9)
  expect_equal(unname(lavaan::fitMeasures(fv, "chisq")),
    nrow(v) * stats::factanal(v, factors = 1)$criteria[["objective"]],
    tolerance = 1e-5
  )
})

test_that("a model whose variables are not the ones specified is refused, not returned", {
  x <- one_factor_n500()
  m <- identity_meta(x)

  # Expert syntax that names a latent variable like a data column.
  spec <- SIEVER:::sieve_cfa_spec(m, syntax = "x1 =~ x1 + x2 + x3 + x4 + x5 + x6")
  run <- SIEVER:::sieve_run_cfa(spec, x)
  expect_null(run$fit)
  expect_identical(run$status$code, "failed")
  expect_match(run$error, "latent variable 'x1'", fixed = TRUE)
  expect_match(run$error, "cannot share a name", fixed = TRUE)
  expect_null(sieve_fit_cfa(x, m, syntax = "x1 =~ x1 + x2 + x3 + x4 + x5 + x6"))

  # A specification that intends six indicators but whose syntax holds five:
  # the fitted model is compared with the intended set after fitting.
  intended <- SIEVER:::sieve_cfa_spec(m)
  expect_identical(intended$observed, names(x))
  short <- intended
  short$model <- "F =~ x2 + x3 + x4 + x5 + x6"
  r2 <- SIEVER:::sieve_run_model(short, x)
  expect_null(r2$fit)
  expect_identical(r2$status$code, "failed")
  expect_match(r2$error, "intended observed variable not in the fitted model: x1", fixed = TRUE)
  # A latent variable that was not intended is named too.
  extra <- intended
  extra$model <- "F =~ x1 + x2 + x3\nG =~ x4 + x5 + x6"
  r3 <- SIEVER:::sieve_run_model(extra, x)
  expect_null(r3$fit)
  expect_match(r3$error, "latent variable in the fitted model that was not intended: G", fixed = TRUE)
  # The intended model passes the same check.
  ok <- SIEVER:::sieve_run_model(intended, x)
  expect_identical(ok$status$code, "ok")
  expect_null(SIEVER:::sieve_membership_problem(ok$fit, intended))

  # A specification resolved without the data's column names is still
  # refused at fit time when a latent name meets a column.
  aux <- cbind(x, Scale = 1)
  named <- identity_meta(x, construct = "Scale")
  bare <- SIEVER:::sieve_cfa_spec(named)
  expect_identical(bare$latent, "Scale")
  expect_identical(SIEVER:::sieve_run_cfa(bare, aux)$status$code, "failed")
  # Resolved with them, the latent variable is renamed and the model fits.
  aware <- SIEVER:::sieve_cfa_spec(named, reserved = names(aux))
  expect_false("Scale" %in% aware$latent)
  expect_identical(SIEVER:::sieve_run_cfa(aware, aux)$status$code, "ok")
  expect_identical(unname(aware$name_map), "Scale")
})

test_that("labels with punctuation, leading digits, no ASCII, or reserved words all fit the intended model", {
  hs <- hs_parts()
  reference <- sieve_fit_cfa(hs$items, hs$meta)
  ref_fit <- unname(lavaan::fitMeasures(reference, c("chisq", "df", "npar")))
  relabel <- function(new) {
    m <- hs$meta
    old <- c("Visual", "Textual", "Speed")
    for (i in seq_along(old)) m$construct[hs$meta$construct == old[i]] <- new[i]
    m
  }
  cases <- list(
    punctuation = c("A-B", "A_B", "A B"),
    digits = c("1st factor", "2nd factor", "3"),
    non_ascii = c("\u5fc3\u7406", "\u80fd\u529b", "\u00e9"),
    reserved = c("NA", "function", "general"),
    items = c("t1_visual_perception", "t2_cubes", "t13_straight_and_curved_capitals"),
    operators = c("=~", "~~", "a + b")
  )
  for (nm in names(cases)) {
    m <- relabel(cases[[nm]])
    spec <- SIEVER:::sieve_cfa_spec(m, reserved = names(hs$fx$data))
    run <- SIEVER:::sieve_run_cfa(spec, hs$items)
    expect_identical(run$status$code, "ok", info = nm)
    # Every item is in, three latent variables, none sharing an observed name.
    expect_setequal(lavaan::lavNames(run$fit, "ov"), hs$fx$item_cols)
    expect_length(lavaan::lavNames(run$fit, "lv"), 3L)
    expect_length(intersect(lavaan::lavNames(run$fit, "lv"), names(hs$fx$data)), 0L)
    # The model does not depend on what the constructs are called.
    expect_equal(unname(lavaan::fitMeasures(run$fit, c("chisq", "df", "npar"))), ref_fit,
      tolerance = 1e-8, info = nm
    )
    # The researcher's labels come back through the name map.
    expect_identical(SIEVER:::sieve_show_names(spec$latent, run$fit), cases[[nm]], info = nm)
  }
  # A blank label, empty or only white space, names no construct
  # (sieve_construct_assigned()): its items are not modeled and are listed with
  # the reason, rather than "" and " " fit as two factors. The one named
  # construct is fit alone, as the same model of its items under any other
  # labels.
  m <- relabel(c("", " ", "Speed"))
  spec <- SIEVER:::sieve_cfa_spec(m, reserved = names(hs$fx$data))
  blank_items <- hs$meta$item[hs$meta$construct %in% c("Visual", "Textual")]
  speed_items <- hs$meta$item[hs$meta$construct == "Speed"]
  expect_identical(spec$not_modeled$reason, "no construct is assigned")
  expect_identical(spec$not_modeled$level, "item")
  expect_identical(spec$not_modeled$n_items, length(blank_items))
  expect_setequal(strsplit(spec$not_modeled$items, ", ", fixed = TRUE)[[1]], blank_items)
  expect_setequal(spec$observed, speed_items)
  run <- SIEVER:::sieve_run_cfa(spec, hs$items)
  expect_identical(run$status$code, "ok")
  expect_setequal(lavaan::lavNames(run$fit, "ov"), speed_items)
  expect_length(lavaan::lavNames(run$fit, "lv"), 1L)
  expect_length(intersect(lavaan::lavNames(run$fit, "lv"), names(hs$fx$data)), 0L)
  alone <- SIEVER:::sieve_run_cfa(
    SIEVER:::sieve_cfa_spec(hs$meta[hs$meta$construct == "Speed", , drop = FALSE], reserved = names(hs$fx$data)),
    hs$items
  )
  expect_equal(unname(lavaan::fitMeasures(run$fit, c("chisq", "df", "npar", "logl"))),
    unname(lavaan::fitMeasures(alone$fit, c("chisq", "df", "npar", "logl"))),
    tolerance = 1e-8
  )
  expect_identical(SIEVER:::sieve_show_names(spec$latent, run$fit), "Speed")
})

test_that("a construct named like an outcome, and outcome columns that need new names, are kept apart", {
  hs <- hs_parts()
  reference <- SIEVER:::sieve_run_structural(hs$items, hs$fx$data, hs$meta, "age_years",
    all_constructs = TRUE
  )
  expect_identical(reference$status$code, "ok")
  ref_paths <- lavaan::standardizedSolution(reference$fit)
  ref_paths <- ref_paths[ref_paths$op == "~", ]

  # A construct labeled exactly like the criterion column.
  m <- hs$meta
  m$construct[m$construct == "Visual"] <- "age_years"
  run <- SIEVER:::sieve_run_structural(hs$items, hs$fx$data, m, "age_years", all_constructs = TRUE)
  expect_identical(run$status$code, "ok")
  expect_setequal(lavaan::lavNames(run$fit, "ov"), c(hs$fx$item_cols, "age_years"))
  expect_false("age_years" %in% lavaan::lavNames(run$fit, "lv"))
  paths <- lavaan::standardizedSolution(run$fit)
  paths <- paths[paths$op == "~", ]
  expect_true(all(paths$lhs == "age_years"))
  expect_equal(paths$est.std, ref_paths$est.std, tolerance = 1e-8)
  expect_identical(SIEVER:::sieve_show_names(paths$rhs, run$fit), c("age_years", "Textual", "Speed"))
  # The fit itself carries the name maps and its status.
  legacy <- sieve_run_structural(hs$items, hs$fx$data, m, "age_years", all_constructs = TRUE)$fit
  expect_identical(unname(attr(legacy, "out_map")), "age_years")
  expect_identical(unname(attr(legacy, "lat_map")), c("age_years", "Textual", "Speed"))
  expect_identical(SIEVER:::sieve_status_of(legacy)$code, "ok")

  # Outcome columns whose names are not usable, or become an item's name once
  # made syntactic, get names of their own; the items stay in the model.
  raw <- hs$fx$data
  raw[["t2 cubes"]] <- raw$age_years
  raw[["t2_cubes "]] <- raw$grade
  raw[["9th"]] <- raw$id
  out <- c("t2 cubes", "t2_cubes ", "9th")
  r2 <- SIEVER:::sieve_run_structural(hs$items, raw, hs$meta, out, all_constructs = TRUE)
  expect_false(is.null(r2$fit), info = r2$error)
  expect_identical(r2$status$code, "ok")
  engine <- names(r2$spec$out_map)
  expect_identical(unname(r2$spec$out_map), out)
  expect_false(any(duplicated(engine)))
  expect_length(intersect(engine, c(hs$fx$item_cols, r2$spec$latent)), 0L)
  expect_setequal(lavaan::lavNames(r2$fit, "ov"), c(hs$fx$item_cols, engine))
  # The same three criteria under plain column names give the same model.
  plain <- hs$fx$data
  plain$crit_a <- plain$age_years
  plain$crit_b <- plain$grade
  plain$crit_c <- plain$id
  r_plain <- SIEVER:::sieve_run_structural(hs$items, plain, hs$meta, c("crit_a", "crit_b", "crit_c"),
    all_constructs = TRUE
  )
  expect_identical(names(r_plain$spec$out_map), c("crit_a", "crit_b", "crit_c"))
  p2 <- lavaan::standardizedSolution(r2$fit)
  p_plain <- lavaan::standardizedSolution(r_plain$fit)
  expect_equal(p2$est.std[p2$op == "~"], p_plain$est.std[p_plain$op == "~"], tolerance = 1e-8)
  expect_equal(unname(lavaan::fitMeasures(r2$fit, c("chisq", "df"))),
    unname(lavaan::fitMeasures(r_plain$fit, c("chisq", "df"))),
    tolerance = 1e-8
  )

  # An outcome that is one of the model's own indicators is refused by name.
  r3 <- SIEVER:::sieve_run_structural(hs$items, hs$fx$data, hs$meta, "t2_cubes", all_constructs = TRUE)
  expect_identical(r3$status$code, "failed")
  expect_match(r3$error, "'t2_cubes' is also an indicator", fixed = TRUE)
  expect_null(sieve_run_structural(hs$items, hs$fx$data, hs$meta, "t2_cubes", all_constructs = TRUE)$fit)
})

test_that("a construct named like another construct's facet stays a separate factor in the higher-order model", {
  n <- 400
  x <- withr::with_seed(31, {
    g <- stats::rnorm(n)
    fa <- .7 * g + stats::rnorm(n, sd = .7)
    fb <- .7 * g + stats::rnorm(n, sd = .7)
    h <- stats::rnorm(n)
    as.data.frame(cbind(
      sapply(1:3, function(i) .7 * fa + stats::rnorm(n, sd = .7)),
      sapply(1:3, function(i) .7 * fb + stats::rnorm(n, sd = .7)),
      sapply(1:3, function(i) .7 * h + stats::rnorm(n, sd = .7))
    ))
  })
  names(x) <- paste0("y", 1:9)
  facet <- c("a", "a", "a", "b", "b", "b", "", "", "")
  plain <- identity_meta(x, construct = rep(c("Visual", "Other"), c(6, 3)), facet = facet)
  # The second construct is labeled as the first construct's facet a would
  # be named: "Visual" + "a".
  clash <- identity_meta(x, construct = rep(c("Visual", "Visual.a"), c(6, 3)), facet = facet)

  plan <- SIEVER:::sieve_cfa_plan(clash, "higher_order")
  lhs <- sub(" =~.*$", "", strsplit(plan$model, "\n", fixed = TRUE)[[1]])
  expect_length(lhs, 4L)
  expect_false(any(duplicated(lhs))) # never two lines beginning "Visual_a =~"
  expect_setequal(plan$observed, names(x))
  expect_setequal(unname(plan$name_map), c("Visual: a", "Visual: b", "Visual", "Visual.a"))

  fit_clash <- sieve_fit_cfa(x, clash, syntax = NULL) # first-order: three items per construct
  run_clash <- SIEVER:::sieve_run_cfa(SIEVER:::sieve_cfa_spec(clash, "higher_order"), x)
  run_plain <- SIEVER:::sieve_run_cfa(SIEVER:::sieve_cfa_spec(plain, "higher_order"), x)
  expect_false(is.null(run_clash$fit), info = run_clash$error)
  expect_setequal(lavaan::lavNames(run_clash$fit, "ov"), names(x))
  expect_length(lavaan::lavNames(run_clash$fit, "lv"), 4L)
  expect_identical(run_clash$status$code, run_plain$status$code)
  expect_equal(unname(lavaan::fitMeasures(run_clash$fit, c("chisq", "df", "npar"))),
    unname(lavaan::fitMeasures(run_plain$fit, c("chisq", "df", "npar"))),
    tolerance = 1e-8
  )
  expect_s4_class(fit_clash, "lavaan")
})

test_that("a construct or an item named 'general' cannot be confused with the bifactor general factor", {
  hs <- hs_parts()
  reference <- sieve_fit_bifactor(hs$items, hs$meta)

  m <- hs$meta
  m$construct[m$construct == "Visual"] <- "general"
  out <- sieve_fit_bifactor(hs$items, m)
  expect_s4_class(out$fit, "lavaan")
  lines <- strsplit(out$syntax, "\n", fixed = TRUE)[[1]]
  expect_match(lines[1], "^general =~ ")
  expect_identical(sum(grepl("^general =~ ", lines)), 1L)
  # All nine items on the general factor, three on each group factor.
  expect_setequal(lavaan::lavNames(out$fit, "ov"), hs$fx$item_cols)
  pt <- lavaan::parTable(out$fit)
  expect_identical(sum(pt$op == "=~" & pt$lhs == "general"), 9L)
  expect_identical(as.integer(table(pt$lhs[pt$op == "=~" & pt$lhs != "general"])), c(3L, 3L, 3L))
  # The tables tell the two apart, and the model is the reference model.
  expect_identical(out$indices$factor, c("general", "general (group factor)", "Textual", "Speed"))
  expect_setequal(unique(out$loadings$group), c("general (group factor)", "Textual", "Speed"))
  expect_equal(out$indices$ECV, reference$indices$ECV, tolerance = 1e-8)
  expect_equal(unname(lavaan::fitMeasures(out$fit, c("chisq", "df"))),
    unname(lavaan::fitMeasures(reference$fit, c("chisq", "df"))),
    tolerance = 1e-8
  )

  # An item called "general": the general factor takes another name, the
  # tables still show "general", and the indices are those of the reference.
  items <- hs$items
  names(items)[1] <- "general"
  m2 <- hs$meta
  m2$item[1] <- "general"
  out2 <- sieve_fit_bifactor(items, m2)
  expect_s4_class(out2$fit, "lavaan")
  expect_false(grepl("^general =~", out2$syntax))
  expect_true("general" %in% lavaan::lavNames(out2$fit, "ov"))
  expect_identical(out2$indices$factor, c("general", "Visual", "Textual", "Speed"))
  expect_equal(out2$indices$omega_H, reference$indices$omega_H, tolerance = 1e-8)
  expect_equal(out2$loadings$general, reference$loadings$general, tolerance = 1e-8)

  # In the CFA a construct labeled "general" is an ordinary factor under
  # another name.
  spec <- SIEVER:::sieve_cfa_spec(m)
  expect_false("general" %in% spec$latent)
  expect_true("general" %in% spec$name_map)
})

test_that("coefficient labels and defined effects avoid items named like them", {
  hs <- hs_parts()
  reference <- SIEVER:::sieve_run_structural(hs$items, hs$fx$data, hs$meta,
    outcomes = c("Visual", "Speed"),
    predictors = c("Textual", "Visual"), effects = TRUE
  )
  expect_identical(reference$status$code, "ok")
  ref_defs <- lavaan::parameterEstimates(reference$fit)
  ref_defs <- ref_defs[ref_defs$op == ":=", ]
  expect_identical(nrow(ref_defs), 2L)
  # The indirect effect is the product of its two paths, and the total is the
  # direct path plus the indirect effect, from the estimates themselves.
  pe <- lavaan::parameterEstimates(reference$fit)
  path <- function(y, x) pe$est[pe$op == "~" & pe$lhs == y & pe$rhs == x]
  expect_equal(ref_defs$est[1], path("Visual", "Textual") * path("Speed", "Visual"), tolerance = 1e-10)
  expect_equal(ref_defs$est[2], path("Speed", "Textual") + ref_defs$est[1], tolerance = 1e-10)

  # Items named like the labels the builder would otherwise use.
  items <- hs$items
  names(items)[1:3] <- c("b1", "ind1", "tot1")
  raw <- hs$fx$data
  names(raw)[match(hs$fx$item_cols[1:3], names(raw))] <- c("b1", "ind1", "tot1")
  m <- hs$meta
  m$item[1:3] <- c("b1", "ind1", "tot1")
  run <- SIEVER:::sieve_run_structural(items, raw, m,
    outcomes = c("Visual", "Speed"),
    predictors = c("Textual", "Visual"), effects = TRUE
  )
  expect_identical(run$status$code, "ok")
  expect_setequal(lavaan::lavNames(run$fit, "ov"), names(items))
  defs <- lavaan::parameterEstimates(run$fit)
  defs <- defs[defs$op == ":=", ]
  expect_length(intersect(defs$lhs, names(items)), 0L)
  expect_identical(names(run$spec$eff_map), defs$lhs)
  expect_equal(defs$est, ref_defs$est, tolerance = 1e-8)
  expect_equal(defs$se, ref_defs$se, tolerance = 1e-6)
  expect_identical(unname(run$spec$eff_map), unname(reference$spec$eff_map))
})

# ---- Status for every fit wrapper -------------------------------------------

test_that("identification is judged for the whole model, not by a count of indicators", {
  hs <- hs_parts()
  d <- hs$items

  # One factor, two indicators: 2 means and 3 variances and covariances, 6
  # free parameters. Not identified, and it has no fit indices to report.
  two <- hs$meta
  two$status[!(two$item %in% hs$fx$item_cols[1:2])] <- "parked"
  fit_two <- sieve_fit_cfa(d, two)
  st <- SIEVER:::sieve_status_of(fit_two)
  expect_identical(st$code, "not_identified")
  expect_equal(st$df, (2 + 3) - 6)
  fm <- sieve_fit_measures(fit_two)
  expect_identical(attr(fm, "status"), "not_identified")
  expect_equal(unname(fm["df"]), -1)
  expect_true(all(is.na(fm[c("chisq", "cfi", "tli", "rmsea", "rmsea_lo", "rmsea_hi", "srmr", "aic", "bic")])))
  # What lavaan itself hands back for this model is an RMSEA of zero (with a
  # warning that the robust version could not be computed).
  raw_rmsea <- SIEVER:::sieve_capture(lavaan::fitMeasures(fit_two, "rmsea"))
  expect_equal(unname(raw_rmsea$value), 0)
  expect_true(length(raw_rmsea$warnings) > 0L)
  expect_null(sieve_factor_scores(fit_two))

  # The same two indicators inside a correlated-factors model: identified
  # through the factor covariance, and accepted. No three-item rule.
  inside <- hs$meta
  inside$status[inside$item == hs$fx$item_cols[3]] <- "parked"
  fit_in <- sieve_fit_cfa(d, inside)
  s_in <- SIEVER:::sieve_status_of(fit_in)
  expect_identical(s_in$code, "ok")
  # 8 means + 36 moments = 44; 8 loadings, 8 residuals, 8 intercepts, 3 covariances.
  expect_equal(s_in$df, (8 + 8 * 9 / 2) - (3 * 8 + 3))
  expect_true(is.finite(sieve_fit_measures(fit_in)[["rmsea"]]))
  expect_s3_class(sieve_factor_scores(fit_in), "data.frame")

  # Remove that covariance (orthogonal factors) and the two indicator factor
  # is no longer identified although the degrees of freedom are positive.
  fit_orth <- sieve_fit_cfa(d, inside, orthogonal = TRUE)
  s_orth <- SIEVER:::sieve_status_of(fit_orth)
  expect_identical(s_orth$code, "not_identified")
  expect_gt(s_orth$df, 0)
  expect_true(!s_orth$se_available || isTRUE(s_orth$information_singular))
  expect_true(is.na(sieve_fit_measures(fit_orth)[["cfi"]]))
  # The same conclusion from the algebra of the model, without lavaan's
  # information matrix. With no covariance to another factor, the two
  # loadings enter the implied covariance matrix only through their product
  # (the covariance of the two indicators) and through each indicator's own
  # variance, where the residual variance can absorb any change. Sigma is
  # rebuilt by hand at lavaan's estimates and at another point on that curve:
  # the two are the same matrix, so the data cannot tell the points apart.
  shift <- function(est, by = 1.1) {
    pair <- which(est$lambda[, 1] != 0)
    lambda <- est$lambda
    theta <- est$theta
    lambda[pair[1], 1] <- lambda[pair[1], 1] * by
    lambda[pair[2], 1] <- lambda[pair[2], 1] / by
    for (j in pair) theta[j, j] <- est$theta[j, j] + est$lambda[j, 1]^2 - lambda[j, 1]^2
    list(pair = pair, lambda = lambda, theta = theta)
  }
  implied <- function(lambda, psi, theta) lambda %*% psi %*% t(lambda) + theta
  est <- lavaan::lavInspect(fit_orth, "est")
  other <- shift(est)
  expect_length(other$pair, 2L)
  expect_gt(max(abs(other$lambda - est$lambda)), 0.1)
  expect_equal(implied(other$lambda, est$psi, other$theta),
    implied(est$lambda, est$psi, est$theta),
    tolerance = 1e-12
  )
  # In the correlated model the same move changes the covariances with the
  # other factors' indicators, so the data do tell the points apart.
  est_in <- lavaan::lavInspect(fit_in, "est")
  other_in <- shift(est_in)
  expect_gt(max(abs(implied(other_in$lambda, est_in$psi, other_in$theta) -
    implied(est_in$lambda, est_in$psi, est_in$theta))), 1e-2)

  # Two indicators with an equality constraint, supplied by an expert: a
  # legitimately identified model, and it passes.
  z <- as.data.frame(scale(d[, hs$fx$item_cols[4:5]]))
  tau <- sieve_fit_cfa(z, hs$meta, syntax = sprintf("f =~ a*%s + a*%s", names(z)[1], names(z)[2]))
  s_tau <- SIEVER:::sieve_status_of(tau)
  expect_identical(s_tau$code, "ok")
  expect_true(s_tau$just_identified)

  # Many indicators do not make a model identified: two orthogonal factors
  # measured by the same nine items have a rotation the data cannot fix.
  # The Bifactor tab refuses this ledger before fitting, so the model is
  # written out and fit here.
  one <- hs$meta
  one$construct <- "Only"
  refused <- sieve_fit_bifactor(d, one)
  expect_null(refused$fit)
  expect_match(refused$error, "two uncorrelated factors on the same items are not identified", fixed = TRUE)
  syntax <- paste(
    sprintf("general =~ %s", paste(hs$fx$item_cols, collapse = " + ")),
    sprintf("Only =~ %s", paste(hs$fx$item_cols, collapse = " + ")),
    sep = "\n"
  )
  bi <- list(fit = sieve_fit_cfa(d, one, syntax = syntax, orthogonal = TRUE))
  bi$status <- SIEVER:::sieve_status_of(bi$fit)
  expect_identical(bi$status$code, "not_identified")
  expect_gt(bi$status$df, 0)
  expect_true(bi$status$information_singular)
  expect_true(bi$status$se_available) # lavaan printed standard errors all the same
  # The verdict does not depend on having lavaan's warning in hand: the
  # status read from the bare fitted object is the same.
  bare <- bi$fit
  attr(bare, "sieve_status") <- NULL
  attr(bare, "sieve_warnings") <- NULL
  expect_identical(SIEVER:::sieve_status_of(bare)$code, "not_identified")
  # The same conclusion from the algebra: with two uncorrelated unit variance
  # factors on the same items, rotating the pair of loading columns leaves
  # the implied covariance matrix unchanged, so the loadings lavaan printed
  # are one of infinitely many equally good sets.
  est <- lavaan::lavInspect(bi$fit, "est")
  angle <- 0.7
  rot <- matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)), 2, 2)
  rotated <- est$lambda %*% rot
  expect_gt(max(abs(rotated - est$lambda)), 0.1)
  expect_equal(tcrossprod(rotated) + est$theta, tcrossprod(est$lambda) + est$theta, tolerance = 1e-12)
  expect_null(refused$loadings)
  expect_null(refused$indices)
})

test_that("a singular robust covariance matrix is a caution, not a verdict on identification", {
  hs <- hs_parts()
  # grade takes two values (7 and 8). Treated as a continuous criterion with
  # robust standard errors and full information maximum likelihood, the
  # casewise scores for its mean and its variance are linearly dependent, so
  # the sandwich covariance matrix is singular. The model is identified.
  expect_identical(sort(unique(stats::na.omit(hs$fx$data$grade))), c(7L, 8L))
  # The subject is the robust (sandwich) covariance matrix, so MLR is
  # chosen (Follow the item types gives ML with FIML).
  run <- SIEVER:::sieve_run_structural(hs$items, hs$fx$data, hs$meta, c("age_years", "grade"),
    all_constructs = TRUE, estimator = "mlr"
  )
  expect_identical(run$status$code, "ok")
  expect_false(run$status$information_singular)
  expect_true(run$status$vcov_singular)
  expect_match(run$status$message, "although the model is identified", fixed = TRUE)
  expect_match(run$status$message, "Joint parameter tests require a nonsingular covariance", fixed = TRUE)
  # The model based (not robust) covariance matrix of the same model is
  # positive definite, which is what identification means here.
  d <- cbind(hs$items, hs$fx$data[, c("age_years", "grade")])
  ml <- SIEVER:::sieve_capture(lavaan::sem(run$spec$model, data = d, estimator = "ML", std.lv = TRUE))$value
  ev <- eigen(stats::cov2cor(lavaan::lavInspect(ml, "vcov")), symmetric = TRUE, only.values = TRUE)$values
  expect_gt(min(ev), 1e-6)
  expect_false(SIEVER:::sieve_vcov_singular(ml))
  # An ordinary model has neither symptom.
  ok <- SIEVER:::sieve_status_of(sieve_fit_cfa(hs$items, hs$meta))
  expect_false(ok$information_singular)
  expect_false(ok$vcov_singular)
  expect_gt(ok$information_min_eigen, 1e-4)
  # Equality constraints do not count as singularity: the invariance rungs.
  dg <- hs$items
  dg$school <- hs$fx$data$school
  res <- sieve_fit_invariance(dg, "school", hs$meta)
  for (lv in names(res$status)) {
    expect_false(res$status[[lv]]$information_singular, info = lv)
    expect_false(res$status[[lv]]$vcov_singular, info = lv)
  }
})

test_that("failure, non-convergence and improper solutions are told apart by every wrapper", {
  hs <- hs_parts()
  d <- hs$items

  # Failed, with the engine's reason: an indicator with no variance.
  flat <- d
  flat[[1]] <- 0
  utils::capture.output(run <- SIEVER:::sieve_run_cfa(SIEVER:::sieve_cfa_spec(hs$meta), flat))
  expect_null(run$fit)
  expect_identical(run$status$code, "failed")
  # Refused before the engine runs, naming the item and the reason (a column
  # with one value throughout); lavaan's own "no variance" is the fallback.
  expect_match(run$status$message, "same value|no variance")
  expect_match(run$status$message, "t1_visual_perception", fixed = TRUE)
  utils::capture.output(legacy <- sieve_fit_cfa(flat, hs$meta))
  expect_null(legacy)
  utils::capture.output(st_run <- SIEVER:::sieve_run_structural(flat, hs$fx$data, hs$meta, "age_years",
    all_constructs = TRUE
  ))
  expect_identical(st_run$status$code, "failed")
  expect_match(st_run$status$message, "same value|no variance")
  expect_match(st_run$status$message, "t1_visual_perception", fixed = TRUE)
  utils::capture.output(bf <- sieve_fit_bifactor(flat, hs$meta))
  expect_null(bf$fit)
  expect_identical(bf$status$code, "failed")
  expect_match(bf$error, "same value|no variance")
  expect_match(bf$error, "t1_visual_perception", fixed = TRUE)

  # Not converged: nine indicators and eight cases. The wrapper still hands
  # back lavaan's object, and the status says it is not a solution.
  few <- d[1:8, ]
  fit_few <- sieve_fit_cfa(few, hs$meta)
  expect_s4_class(fit_few, "lavaan")
  expect_false(lavaan::lavInspect(fit_few, "converged"))
  expect_identical(SIEVER:::sieve_status_of(fit_few)$code, "not_converged")
  expect_true(length(attr(fit_few, "sieve_warnings")) > 0L)
  expect_null(sieve_fit_measures(fit_few))
  expect_null(sieve_factor_scores(fit_few))
  bf_few <- sieve_fit_bifactor(few, hs$meta)
  expect_identical(bf_few$status$code, "not_converged")
  expect_null(bf_few$indices)
  expect_null(bf_few$loadings)

  # Inadmissible: the nine test bifactor solution has a negative residual
  # variance. It converged and is identified, so its estimates and indices
  # are returned, with the status and lavaan's warning attached.
  bf_hs <- sieve_fit_bifactor(d, hs$meta)
  expect_identical(bf_hs$status$code, "inadmissible")
  pe <- lavaan::parameterEstimates(bf_hs$fit)
  negative <- pe$lhs[pe$op == "~~" & pe$lhs == pe$rhs & pe$est < 0]
  expect_true(length(negative) > 0L)
  expect_setequal(bf_hs$status$negative_variances, negative)
  expect_true(any(grepl("negative", bf_hs$warnings)))
  expect_true(isTRUE(bf_hs$improper))
  expect_false(is.null(bf_hs$indices))
  expect_true(is.finite(sieve_fit_measures(bf_hs$fit)[["cfi"]]))

  # When lavaan cannot compute standard errors and also reports very unequal
  # observed variances, the message passes that diagnosis on, because unequal
  # scales can cause the failure on their own. (Plain maximum likelihood on
  # complete cases, where lavaan declines to invert the information matrix
  # of the orthogonal two indicator factor.)
  no_se <- SIEVER:::sieve_capture(lavaan::cfa(
    sprintf(
      "V =~ %s + %s\nT =~ %s + %s + %s", hs$fx$item_cols[1], hs$fx$item_cols[2],
      hs$fx$item_cols[4], hs$fx$item_cols[5], hs$fx$item_cols[6]
    ),
    data = d, std.lv = TRUE, orthogonal = TRUE
  ))
  plain_status <- SIEVER:::sieve_fit_status(no_se$value, warnings = no_se$warnings)
  expect_identical(plain_status$code, "not_identified")
  expect_false(plain_status$se_available)
  expect_false(grepl("similar scales", plain_status$message, fixed = TRUE))
  hinted <- SIEVER:::sieve_fit_status(
    no_se$value,
    warnings = c(
      no_se$warnings,
      "lavaan->lav_data_full():  \n   some observed variances are (at least) a factor 1000 times larger than others"
    )
  )
  expect_match(hinted$message, "similar scales", fixed = TRUE)

  # Ok: the three factor model and its structural extensions.
  expect_identical(SIEVER:::sieve_status_of(sieve_fit_cfa(d, hs$meta))$code, "ok")
  path <- sieve_run_structural(d, hs$fx$data, hs$meta, outcomes = "Visual", predictors = c("Textual", "Speed"))$fit
  expect_identical(SIEVER:::sieve_status_of(path)$code, "ok")
  expect_identical(attr(path, "sieve_spec")$fun, "sem")
  expect_identical(unname(attr(path, "name_map")), c("Textual", "Speed", "Visual"))
  med <- sieve_run_structural(d, hs$fx$data, hs$meta, outcomes = c("Visual", "Speed"), predictors = c("Textual", "Visual"), effects = TRUE)$fit
  expect_identical(SIEVER:::sieve_status_of(med)$code, "ok")
  mf <- SIEVER:::sieve_run_method_factor(d, hs$meta)
  expect_true(mf$status$code %in% c("ok", "inadmissible", "not_identified", "not_converged"))
  expect_false(is.null(mf$fit))
  expect_identical(SIEVER:::sieve_show_names(attr(mf$fit, "method_factor"), mf$fit), "Method factor")
  expect_identical(sum(lavaan::parTable(mf$fit)$op == "=~" &
    lavaan::parTable(mf$fit)$lhs == attr(mf$fit, "method_factor")), 9L)
})

test_that("the estimator follows the indicators in the structural model, and its conditions are kept", {
  hs <- hs_parts()
  d <- hs$items
  d[] <- lapply(d, function(v) as.integer(cut(v, stats::quantile(v, 0:4 / 4), include.lowest = TRUE)))
  m <- hs$meta
  m$type <- "ordered"
  raw <- cbind(hs$fx$data[, hs$fx$id_vars], d)
  run <- SIEVER:::sieve_run_structural(d, raw, m, "age_years", all_constructs = TRUE)
  expect_false(is.null(run$fit), info = run$error)
  expect_identical(run$spec$estimator, "WLSMV")
  expect_identical(run$spec$ordered, hs$fx$item_cols)
  expect_identical(run$spec$missing, "listwise")
  opt <- lavaan::lavInspect(run$fit, "options")
  expect_identical(opt$missing, "listwise")
  expect_setequal(lavaan::lavNames(run$fit, "ov.ord"), hs$fx$item_cols)
  # A continuous model that uses only continuous indicators is not switched
  # to the categorical estimator by an ordered item outside it.
  mixed <- hs$meta
  mixed$type[mixed$construct == "Speed"] <- "ordered"
  path <- SIEVER:::sieve_run_structural(hs$items, hs$fx$data, mixed,
    outcomes = "Visual",
    predictors = "Textual"
  )
  # Follow the item types gives the continuous modeled items ML with FIML
  # (DMAR's default).
  expect_identical(path$spec$estimator, "ML")
  expect_identical(path$spec$missing, "fiml")
  expect_null(path$spec$ordered)
  expect_identical(path$status$code, "ok")
  # lavaan's warning about very different variances is kept, not discarded.
  crit <- SIEVER:::sieve_run_structural(hs$items, hs$fx$data, hs$meta, "age_years", all_constructs = TRUE)
  expect_true(any(grepl("variances", crit$warnings)))
  expect_identical(attr(crit$fit, "sieve_warnings"), crit$warnings)
})

test_that("what a model leaves out is recorded with the reason, and a selection is never trimmed quietly", {
  hs <- hs_parts()
  m <- hs$meta
  m$construct[9] <- "Alone"

  # By default the single item construct is not in the syntax.
  plain <- sieve_cfa_plan(m)$model
  expect_identical(length(strsplit(plain, "\n", fixed = TRUE)[[1]]), 3L)
  expect_false(grepl(hs$fx$item_cols[9], plain, fixed = TRUE))
  # It is on the record.
  spec <- SIEVER:::sieve_cfa_spec(m)
  expect_identical(nrow(spec$not_modeled), 1L)
  expect_identical(spec$not_modeled$construct, "Alone")
  expect_identical(spec$not_modeled$items, hs$fx$item_cols[9])
  expect_identical(spec$not_modeled$n_items, 1L)
  expect_match(SIEVER:::sieve_not_modeled_text(spec),
    "Construct 'Alone' is not in the model: the construct has 1 included item",
    fixed = TRUE
  )
  expect_identical(nrow(SIEVER:::sieve_cfa_spec(hs$meta)$not_modeled), 0L)
  # min_items = 1 writes the factor; whether that model is identified is then
  # a matter for the fit status, not for the builder.
  expect_identical(length(strsplit(sieve_cfa_plan(m, min_items = 1L)$model, "\n", fixed = TRUE)[[1]]), 4L)

  # An included item with no construct at all is on the record too.
  loose <- hs$meta
  loose$construct[1] <- NA
  lspec <- SIEVER:::sieve_cfa_spec(loose)
  expect_false(hs$fx$item_cols[1] %in% lspec$observed)
  expect_identical(lspec$not_modeled$items, hs$fx$item_cols[1])
  expect_match(SIEVER:::sieve_not_modeled_text(lspec), "An item is not in the model: no construct is assigned",
    fixed = TRUE
  )

  # Nothing to model at all: the reason is in the failure.
  none <- hs$meta
  none$construct <- paste0("C", seq_len(nrow(none)))
  empty <- SIEVER:::sieve_run_cfa(SIEVER:::sieve_cfa_spec(none), hs$items)
  expect_identical(empty$status$code, "failed")
  expect_match(empty$error, "Construct 'C1' is not in the model", fixed = TRUE)

  # Higher-order: a facet with too few items and items with no facet. The
  # construct below has facets a (2 items), b (2 items) and c (1 item), and
  # two items with no facet; a second construct has two items.
  x <- as.data.frame(matrix(0, 2, 9, dimnames = list(NULL, paste0("q", 1:9))))
  big <- identity_meta(x,
    construct = rep(c("Wide", "Small"), c(7, 2)),
    facet = c("a", "a", "b", "b", "c", "", "", "", "")
  )
  ho <- SIEVER:::sieve_cfa_plan(big, "higher_order")
  expect_identical(ho$observed, c("q1", "q2", "q3", "q4", "q8", "q9"))
  expect_identical(ho$not_modeled$level, c("facet", "item"))
  expect_identical(ho$not_modeled$facet, c("c", NA))
  expect_identical(ho$not_modeled$items, c("q5", "q6, q7"))
  expect_identical(ho$not_modeled$n_items, c(1L, 2L))
  expect_identical(
    strsplit(ho$not_modeled$items[2], ", ", fixed = TRUE)[[1]],
    sieve_unfacetted_items(big)
  )
  expect_identical(length(ho$latent), 4L) # two facets, Wide, Small
  expect_identical(sieve_cfa_plan(big, structure = "higher_order")$model, ho$model)
  # The first-order model of the same ledger keeps all nine items.
  expect_identical(SIEVER:::sieve_cfa_plan(big, "first_order")$observed, paste0("q", 1:9))

  # A structural model: every construct in the criterion model, less the one
  # that cannot be a factor, which is listed.
  crit <- SIEVER:::sieve_run_structural(hs$items, hs$fx$data, m, "age_years", all_constructs = TRUE)
  expect_identical(crit$status$code, "ok")
  expect_identical(crit$spec$not_modeled$construct, "Alone")
  expect_false("Alone" %in% crit$spec$name_map)
  # A construct the researcher selected is not dropped: the model is refused.
  sel <- SIEVER:::sieve_run_structural(hs$items, hs$fx$data, m,
    outcomes = "Visual",
    predictors = c("Textual", "Alone")
  )
  expect_null(sel$fit)
  expect_identical(sel$status$code, "failed")
  expect_match(sel$error, "'Alone' cannot enter the structural model", fixed = TRUE)
  expect_null(sieve_run_structural(hs$items, hs$fx$data, m, outcomes = "Visual", predictors = c("Textual", "Alone"))$fit)
  # No mediator: nothing to estimate, said in words.
  nomed <- SIEVER:::sieve_run_structural(hs$items, hs$fx$data, hs$meta,
    outcomes = "Visual",
    predictors = c("Textual", "Speed"), effects = TRUE
  )
  expect_identical(nomed$status$code, "failed")
  expect_length(nomed$spec$mediators, 0L)
  expect_match(nomed$error, "no indirect effect to estimate", fixed = TRUE)
})

test_that("exploratory solutions carry a status and the factor bound is a degrees of freedom count", {
  hs <- hs_parts()
  d <- hs$items
  # The bound against the count it stands for, for 2 to 30 variables.
  for (p in 2:30) {
    k <- seq_len(p - 1L)
    dof <- (p * (p + 1) / 2) - (p * k + p - k * (k - 1) / 2) # moments less free parameters
    expect_equal(dof, ((p - k)^2 - (p + k)) / 2)
    expect_identical(as.integer(SIEVER:::sieve_efa_max_factors(p)),
      as.integer(max(c(0L, k[dof >= 0]))),
      info = p
    )
  }
  expect_identical(as.integer(SIEVER:::sieve_efa_max_factors(9)), 5L)

  by_k <- SIEVER:::sieve_efa_fit_by_k(d, "MLR", NULL, rotation = "oblimin", max_k = 8L)
  expect_identical(by_k$k, 1:5) # not 1:8: k = 6, 7, 8 have negative df
  expect_true(all(by_k$status %in% c("ok", "inadmissible", "not_converged", "not_identified", "failed")))
  usable <- by_k$status %in% c("ok", "inadmissible")
  expect_true(all(is.finite(by_k$CFI[usable])))
  expect_true(all(is.na(by_k$CFI[!usable])))
  expect_length(attr(by_k, "warnings"), 5L)
  # The three factor row against lavaan called directly.
  ref <- lavaan::efa(data = d, nfactors = 3, rotation = "oblimin", estimator = "MLR")
  ref <- if (inherits(ref, "efaList")) ref[[1]] else ref
  expect_equal(by_k$BIC[3], unname(lavaan::fitMeasures(ref, "bic")), tolerance = 1e-8)
  expect_equal(by_k$converged[3], lavaan::lavInspect(ref, "converged"))

  # psych::fa returns an object for six factors on nine items; its degrees of
  # freedom are negative, and the status says the solution is not identified.
  over <- SIEVER:::sieve_capture(psych::fa(d, nfactors = 6, fm = "ml", rotate = "oblimin"))
  expect_false(is.null(over$value))
  st <- SIEVER:::sieve_efa_status(over$value, over$warnings, over$error)
  expect_identical(st$code, "not_identified")
  expect_equal(st$df, ((9 - 6)^2 - (9 + 6)) / 2)
  good <- SIEVER:::sieve_capture(psych::fa(d, nfactors = 3, fm = "ml", rotate = "oblimin"))
  expect_identical(SIEVER:::sieve_efa_status(good$value, good$warnings)$code, "ok")
  expect_equal(SIEVER:::sieve_efa_status(good$value)$df, ((9 - 3)^2 - (9 + 3)) / 2)
  expect_identical(SIEVER:::sieve_efa_status(NULL, error = "psych said no")$code, "failed")
  expect_identical(SIEVER:::sieve_efa_status(NULL, error = "psych said no")$message, "psych said no")
  heywood <- good$value
  heywood$communality[2] <- 1.02
  hst <- SIEVER:::sieve_efa_status(heywood)
  expect_identical(hst$code, "inadmissible")
  expect_identical(hst$heywood, names(heywood$communality)[2])
})

test_that("the invariance ladder reports a status for every rung and keeps latent names clear of the group column", {
  hs <- hs_parts()
  d <- hs$items
  d$school <- hs$fx$data$school
  res <- sieve_fit_invariance(d, "school", hs$meta)
  expect_null(res$error)
  # The ladder is fit from the same resolved specification as the CFA.
  expect_s3_class(res$spec, "sieve_spec")
  expect_identical(res$spec$model, res$syntax)
  expect_identical(res$spec$observed, hs$fx$item_cols)
  expect_identical(res$spec$estimator, "ML")
  expect_identical(res$missing, "fiml")
  expect_identical(names(res$status), as.character(res$mi$level))
  codes <- vapply(res$status, function(s) s$code, character(1))
  expect_true(all(codes %in% c("ok", "inadmissible", "not_converged", "not_identified")))
  for (lv in names(res$status)) {
    expect_identical(res$status[[lv]]$converged,
      lavaan::lavInspect(attr(res$mi, "fits")[[lv]], "converged"),
      info = lv
    )
  }
  expect_type(res$warnings, "character")

  # A construct labeled like the grouping column.
  m <- hs$meta
  m$construct[m$construct == "Visual"] <- "school"
  res2 <- sieve_fit_invariance(d, "school", m)
  expect_null(res2$error)
  expect_false(grepl("^school =~", res2$syntax))
  fit1 <- attr(res2$mi, "fits")[[1]]
  expect_setequal(lavaan::lavNames(fit1, "ov"), hs$fx$item_cols)
  expect_equal(res2$mi$chi_square, res$mi$chi_square, tolerance = 1e-8)
  expect_true("school" %in% res2$spec$name_map)
  expect_false("school" %in% res2$spec$latent)

  # An ordered item that is not in the model (alone in its construct) does
  # not switch the ladder to the categorical estimator.
  m3 <- hs$meta
  m3$construct[6] <- "Alone"
  m3$type[6] <- "ordered"
  res3 <- sieve_fit_invariance(d, "school", m3)
  expect_null(res3$error)
  expect_identical(res3$spec$estimator, "ML")
  expect_null(res3$spec$ordered)
  expect_identical(res3$spec$not_modeled$construct, "Alone")
  expect_false(hs$fx$item_cols[6] %in% lavaan::lavNames(attr(res3$mi, "fits")[[1]], "ov"))
  # A ladder DMAR cannot complete comes back as an error with the resolved
  # specification attached, not as a table.
  m4 <- hs$meta
  m4$construct[9] <- "Alone"
  res4 <- sieve_fit_invariance(d, "school", m4)
  if (!is.null(res4$error)) {
    expect_null(res4$mi)
    expect_s3_class(res4$spec, "sieve_spec")
  } else {
    expect_identical(names(res4$status), as.character(res4$mi$level))
  }
})

test_that("the status alert says what was established, and stays silent for a clean model", {
  hs <- hs_parts()
  ok <- SIEVER:::sieve_run_cfa(SIEVER:::sieve_cfa_spec(hs$meta), hs$items)
  expect_null(SIEVER:::sieve_status_alert(ok))
  expect_null(SIEVER:::sieve_status_alert(ok$fit))
  m <- hs$meta
  m$construct[9] <- "Alone"
  left <- SIEVER:::sieve_run_cfa(SIEVER:::sieve_cfa_spec(m), hs$items)
  expect_match(identity_text(SIEVER:::sieve_status_alert(left)), "Construct 'Alone' is not in the model",
    fixed = TRUE
  )
  failed <- SIEVER:::sieve_run_cfa(SIEVER:::sieve_cfa_spec(hs$meta), hs$items[, 1:5])
  txt <- identity_text(SIEVER:::sieve_status_alert(failed, what = "structural model"))
  expect_match(txt, "The structural model could not be fit.", fixed = TRUE)
  expect_match(txt, "do not contain the modeled variable", fixed = TRUE)
  two <- hs$meta
  two$status[!(two$item %in% hs$fx$item_cols[1:2])] <- "parked"
  not_id <- SIEVER:::sieve_run_cfa(SIEVER:::sieve_cfa_spec(two), hs$items)
  txt <- identity_text(SIEVER:::sieve_status_alert(not_id))
  expect_match(txt, "is not identified", fixed = TRUE)
  expect_match(txt, "not interpretable", fixed = TRUE)
  expect_match(txt, "warning", fixed = TRUE) # lavaan's own warning is shown
  bf <- sieve_fit_bifactor(hs$items, hs$meta)
  txt <- identity_text(SIEVER:::sieve_status_alert(bf$status,
    what = "bifactor model",
    warnings = bf$warnings, spec = bf$spec
  ))
  expect_match(txt, "Improper solution", fixed = TRUE)
  expect_match(txt, "negative variance estimate", fixed = TRUE)
  just <- hs$meta
  just$status[just$construct != "Visual"] <- "parked"
  jrun <- SIEVER:::sieve_run_cfa(SIEVER:::sieve_cfa_spec(just), hs$items)
  expect_true(jrun$status$just_identified)
  expect_match(identity_text(SIEVER:::sieve_status_alert(jrun)), "just identified", fixed = TRUE)
})

test_that("the modules show the status of the model they report", {
  skip_if_not_installed("shiny")
  hs <- hs_parts()

  # SEM tab: a predictor with a single item is refused with the reason, and a
  # clean model shows no alert.
  m <- hs$meta
  m$construct[9] <- "Alone"
  st <- identity_state(meta = m)
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(
      outcomes = "Visual", predictors = c("Textual", "Alone"),
      layout = "tree2", min_path = 0
    )
    session$setInputs(fit_go = 1)
    expect_null(fit())
    expect_identical(path_run()$status$code, "failed")
    expect_match(identity_text(output$gate), "cannot enter the structural model", fixed = TRUE)
    session$setInputs(predictors = "Textual")
    session$setInputs(fit_go = 2)
    expect_s4_class(fit(), "lavaan")
    expect_identical(path_run()$status$code, "ok")
    # The gate carries the on-request line (fit at ..., current) and no alert.
    expect_match(identity_text(output$gate), "nothing analytic has changed since", fixed = TRUE)
    expect_false(grepl("alert-danger", identity_text(output$gate), fixed = TRUE))
    # The fitted structural model is entered in the analysis registry with
    # its specification and status.
    rec <- shiny::isolate(state$analyses)[[path_run()$analysis_id]]
    expect_identical(rec$kind, "sem")
    expect_identical(rec$status$code, "ok")
    expect_identical(rec$spec$key, path_run()$spec$key)
    expect_match(identity_text(output$effects_note), "No mediator is present", fixed = TRUE)
  })

  # SEM tab: a structural model on eight cases does not converge, and says so.
  few <- hs$fx$data[1:8, ]
  st <- identity_state(raw = few)
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(
      outcomes = "Visual", predictors = c("Textual", "Speed"),
      layout = "tree2", min_path = 0, estimator = "ml"
    )
    session$setInputs(fit_go = 1)
    expect_true(path_run()$status$code %in% c("not_converged", "not_identified", "failed"))
    expect_match(identity_text(output$gate), "did not converge|is not identified|could not be fit")
    # No estimate is put on the screen from a model with no solution.
    expect_null(fit())
    expect_error(output$paths_tbl)
  })

  # CFA tab: the single item construct is named as left out, and a construct
  # labeled like an item is shown under its label.
  st <- identity_state(meta = m)
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    session$setInputs(fit_go = 1)
    expect_true(sieve_has_solution(cfa_run()))
    expect_match(identity_text(output$gate), "Construct 'Alone' is not in the model", fixed = TRUE)
  })
  named <- hs$meta
  named$construct[named$construct == "Visual"] <- "t2_cubes"
  named$construct[named$construct == "Textual"] <- "school" # an auxiliary column
  st <- identity_state(meta = named)
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    session$setInputs(fit_go = 1)
    run <- cfa_run()
    expect_identical(run$status$code, "ok")
    expect_setequal(lavaan::lavNames(run$fit, "ov"), hs$fx$item_cols)
    expect_length(intersect(run$spec$latent, names(hs$fx$data)), 0L)
    expect_identical(unname(run$spec$name_map), c("t2_cubes", "school", "Speed"))
    expect_no_error(output$est_tbl)
    expect_no_error(output$phi_tbl)
  })

  # Bifactor tab: one construct only is not identified, so the tab refuses
  # it before fitting; the panel says why and shows no indices.
  one <- hs$meta
  one$construct <- "Only"
  st <- identity_state(meta = one)
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    session$setInputs(fit = 1)
    expect_null(res())
    expect_match(identity_text(output$status), "two uncorrelated factors on the same items are not identified", fixed = TRUE)
  })
  st <- identity_state()
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    session$setInputs(fit = 1)
    expect_identical(res()$status$code, "inadmissible")
    expect_match(identity_text(output$status), "Improper solution", fixed = TRUE)
    kinds <- vapply(shiny::isolate(state$analyses), function(a) a$kind, character(1))
    expect_true("bifactor" %in% kinds)
  })

  # Items tab: the overall fit line states the status instead of printing
  # indices for a model that is not identified.
  two <- hs$meta
  two$status[!(two$item %in% hs$fx$item_cols[1:2])] <- "parked"
  st <- identity_state(meta = two)
  shiny::testServer(mod_winnow_server, args = list(state = st), {
    session$setInputs(
      coef = "omega", factor_cov = "correlated",
      load_low = .4, salient = .3, communality_low = .3,
      item_total_low = .3, redundancy_high = .8
    )
    session$elapse(400)
    txt <- identity_text(output$overall_fit)
    expect_match(txt, "no fit indices", fixed = TRUE)
    expect_false(grepl("RMSEA = 0.000", txt, fixed = TRUE))
  })
})

# ---- One-construct item diagnostics -----------------------------------------

# The checks shared by the three measurement levels. `x` holds one construct.
expect_one_factor_diagnostics <- function(x, type) {
  m <- identity_meta(x, type = type)
  d <- sieve_item_diagnostics(x, m)
  expect_false(is.null(d$items))
  it <- d$items
  expect_identical(it$item, names(x))
  expect_identical(d$n_obs, sum(stats::complete.cases(x)))
  xc <- x[stats::complete.cases(x), , drop = FALSE]

  # Loadings and communalities from DMAR::fa_exploratory called directly on
  # the same correlation matrix (ML, one factor, unrotated), the Items tab's
  # engine, and from psych::fa, within the two optimizers' tolerance.
  ref <- sieve_efa_dmar_solution(DMAR::fa_exploratory(S = stats::cor(xc), N = nrow(xc), nfactors = 1L,
    estimator = "ML", rotation = "none", conf_level = 0.95))
  l_ref <- as.numeric(ref$loadings[, 1])
  l_ref <- l_ref * sign(sum(l_ref)) * sign(sum(it$loading_target))
  expect_equal(it$loading_target, l_ref, tolerance = 1e-10)
  expect_equal(it$communality, unname(ref$communality), tolerance = 1e-10)
  ps <- psych::fa(stats::cor(xc), nfactors = 1L, n.obs = nrow(xc), fm = "ml")
  expect_lt(max(abs(abs(it$loading_target) - abs(as.numeric(unclass(ps$loadings)[, 1])))), 1e-5)
  expect_lt(max(abs(it$communality - unname(ps$communality))), 1e-5)
  # And from stats::factanal, a separate maximum likelihood implementation
  # (its sign is arbitrary; the optimizers agree to about four decimals).
  fa <- stats::factanal(xc, factors = 1)
  l_fa <- as.numeric(fa$loadings[, 1])
  l_fa <- l_fa * sign(sum(l_fa)) * sign(sum(it$loading_target))
  expect_equal(it$loading_target, l_fa, tolerance = 2e-3)
  expect_equal(it$communality, 1 - unname(fa$uniquenesses), tolerance = 2e-3)
  # One factor: the communality is the squared loading.
  expect_equal(it$communality, it$loading_target^2, tolerance = 1e-10)

  # Corrected item-total correlation by hand: the item against the mean of
  # the other items, pairwise.
  by_hand <- vapply(seq_along(x), function(j) {
    stats::cor(x[[j]], rowMeans(x[, -j, drop = FALSE], na.rm = TRUE), use = "pairwise.complete.obs")
  }, numeric(1))
  expect_equal(it$item_total, by_hand, tolerance = 1e-12)

  # No second factor: no off target loading, no cross loading, no suggestion.
  expect_true(all(is.na(it$loading_off_max)))
  expect_true(all(is.na(it$off_construct)))
  expect_true(all(is.na(it$suggest_construct)))
  expect_identical(it$cross_loading, rep(FALSE, ncol(x)))
  for (flag in c(
    "flag_loading", "flag_communality", "flag_item_total", "flag_omega_drop",
    "flag_redundancy"
  )) {
    expect_type(it[[flag]], "logical")
    expect_false(anyNA(it[[flag]]))
  }
  th <- sieve_default_thresholds()
  expect_identical(it$flag_loading, it$loading_target < th$load_low)
  expect_identical(it$flag_item_total, by_hand < th$item_total_low)
  expect_identical(
    it$removal_signals,
    as.integer(it$flag_loading + it$flag_communality + it$flag_item_total +
      it$flag_omega_drop + it$flag_redundancy)
  )
  # Largest correlation with another item, from the correlation matrix.
  r <- abs(stats::cor(xc))
  diag(r) <- NA
  expect_equal(it$redundancy_max, unname(apply(r, 1, max, na.rm = TRUE)), tolerance = 1e-12)

  # One construct row; its omega against the factanal solution on the
  # covariance scale: (sum of loadings)^2 over that plus the uniquenesses.
  expect_identical(nrow(d$constructs), 1L)
  expect_identical(d$constructs$n_active, ncol(x))
  # The Items tab reports robust omega: for continuous items the squared sum of
  # the loadings over the variance of the sum from the sample covariance matrix
  # (divisor n); for ordered or binary items categorical robust omega on the
  # polychoric correlations.
  if (identical(type, "continuous")) {
    n <- nrow(xc)
    s <- vapply(xc, stats::sd, numeric(1)) * sqrt((n - 1) / n)
    lam <- abs(as.numeric(fa$loadings[, 1])) * s
    S <- stats::cov(xc) * (n - 1) / n
    expect_equal(d$constructs$omega, sum(lam)^2 / sum(S), tolerance = 2e-3)
  } else {
    ref <- suppressMessages(DMAR::reliability_omega_categorical(xc, ci_method = "none"))
    expect_equal(d$constructs$omega, ref$value[ref$term == "estimate"], tolerance = 1e-8)
  }
  invisible(d)
}

test_that("one-construct diagnostics work on continuous items and match independent values", {
  x <- one_factor_n500()
  d <- expect_one_factor_diagnostics(x, "continuous")
  # The data were generated with these loadings; the estimates are
  # near them (a sanity check on the scale of the numbers, n = 500).
  expect_equal(d$items$loading_target, c(.5, .6, .7, .8, .65, .75), tolerance = 0.12)
  expect_length(d$notes, 0L)
  # A second data set from an independent seed.
  v <- withr::with_seed(114, {
    f <- stats::rnorm(600)
    as.data.frame(sapply(seq(.50, .75, .05), function(l) l * f + sqrt(1 - l * l) * stats::rnorm(600)))
  })
  names(v) <- paste0("v", 1:6)
  expect_one_factor_diagnostics(v, "continuous")
  # Incomplete data: listwise for the factor analysis, pairwise for the
  # item-total correlation.
  holes <- x
  holes[c(3, 70, 200), 1] <- NA
  holes[c(9, 70), 4] <- NA
  dh <- expect_one_factor_diagnostics(holes, "continuous")
  expect_identical(dh$n_obs, 496L)
})

test_that("one-construct diagnostics work on binary items", {
  x <- one_factor_n500()
  # Dichotomized at 1, with a median split as a second case.
  rare <- as.data.frame(lapply(x, function(z) as.numeric(z > 1)))
  expect_one_factor_diagnostics(rare, "binary")
  half <- as.data.frame(lapply(x, function(z) as.numeric(z > stats::median(z))))
  expect_one_factor_diagnostics(half, "binary")
})

test_that("one-construct diagnostics work on ordered items", {
  x <- one_factor_n500()
  ordered4 <- as.data.frame(lapply(x, function(z) as.numeric(cut(z, c(-Inf, -.7, 0, .7, Inf)))))
  expect_one_factor_diagnostics(ordered4, "ordered")
  likert5 <- as.data.frame(lapply(x, function(z) as.numeric(cut(z, c(-Inf, -1.2, -.4, .4, 1.2, Inf)))))
  expect_one_factor_diagnostics(likert5, "ordered")
})

test_that("with two or more constructs the off target loading is still an estimate", {
  hs <- hs_parts()
  d <- sieve_item_diagnostics(hs$items, hs$meta)
  it <- d$items
  expect_false(anyNA(it$loading_off_max))
  expect_false(anyNA(it$off_construct))
  # Against psych::fa called directly (the Items tab's engine is DMAR's maximum
  # likelihood on the same matrix, which psych::fa reproduces): for each item
  # the target loading is the largest in absolute value on these data, and the
  # off target value is the larger of the other two.
  xc <- hs$items[stats::complete.cases(hs$items), ]
  ref <- unclass(psych::fa(stats::cor(xc),
    nfactors = 3L, n.obs = nrow(xc), fm = "ml",
    rotate = "oblimin"
  )$loadings)
  sorted <- t(apply(abs(ref), 1, sort, decreasing = TRUE))
  # (The two optimizers' oblique rotations agree to about five decimals.)
  expect_equal(abs(it$loading_target), unname(sorted[, 1]), tolerance = 1e-4)
  expect_equal(it$loading_off_max, unname(sorted[, 2]), tolerance = 1e-4)
  expect_type(it$cross_loading, "logical")
})

test_that("the Items tab renders for a one-construct scale", {
  skip_if_not_installed("shiny")
  x <- one_factor_n500()
  meta <- sieve_build_meta(x, names(x))
  meta$construct <- "Ability"
  s <- sieve_new_state()
  s$dataset_name <- "one factor"
  s$raw <- x
  s$items <- x
  s$id_vars <- character(0)
  s$meta <- meta
  s$confirmed <- TRUE
  s$palette <- "nd"
  s$thresholds <- sieve_default_thresholds()
  shiny::testServer(mod_winnow_server, args = list(state = s), {
    session$setInputs(
      coef = "omega", factor_cov = "correlated",
      load_low = .4, salient = .3, communality_low = .3,
      item_total_low = .3, redundancy_high = .8
    )
    session$elapse(400)
    shown <- display()
    expect_identical(nrow(shown), 6L)
    expect_false(anyNA(shown$loading))
    expect_true(all(shown$Reassign == ""))
    expect_no_error(output$pool)
    expect_no_error(output$overall_fit)
  })
})

# ---- wording ----------------------------------------------------------------

test_that("no interface or documentation text says a fixed factor variance standardizes the loadings", {
  root <- testthat::test_path("..", "..")
  skip_if_not(dir.exists(file.path(root, "R")))
  files <- file.path(root, "R", c(
    "fct_models.R", "fct_spec.R", "fct_names.R", "fct_diagnostics.R",
    "mod_sem.R", "mod_cfa.R", "mod_bifactor.R"
  ))
  src <- unlist(lapply(files, readLines, warn = FALSE))
  expect_false(any(grepl("free and standardized", src, fixed = TRUE)))
  expect_false(any(grepl("independent and standardized", src, fixed = TRUE)))
  # The CFA tab's own description says what the scaling does. The sentence
  # is written from the resolved specification when the tab is shown, so it
  # is read from the running module, for both identification choices.
  fx <- sieve_hs9_fixture()
  state <- sieve_new_state()
  shiny::isolate({
    state$raw <- fx$data
    state$items <- fx$data[, fx$item_cols]
    state$id_vars <- fx$id_vars
    state$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
    state$confirmed <- TRUE
  })
  shiny::testServer(mod_cfa_server, args = list(state = state), {
    session$setInputs(structure = "first", ident = "varone")
    note <- identity_text(as.character(output$model_note$html))
    expect_match(note, "it does not standardize the observed items", fixed = TRUE)
    expect_match(note, "maximum likelihood (ML) with full information maximum likelihood (FIML)", fixed = TRUE)
    expect_match(note, "DMAR's default", fixed = TRUE)
    session$setInputs(ident = "marker")
    marker <- identity_text(as.character(output$model_note$html))
    expect_match(marker, "fixing the loading of the first item to one", fixed = TRUE)
    expect_false(grepl("standardized", sub("standardize the observed", "", marker), fixed = TRUE) &&
      grepl("loadings are free and standardized", marker, fixed = TRUE))
  })
})

# ---- Names, labels, and the status of further fits ---------------------------

test_that("an outcome that is both a construct label and a data column is never guessed at", {
  # Nine items on three correlated factors, and an observed criterion y. One
  # construct is labeled y as well, so "y" could be the latent variable or
  # the column: two different models.
  n <- 600L
  d <- withr::with_seed(4411L, {
    eta <- matrix(stats::rnorm(n * 3L), n, 3L) %*% chol(matrix(c(1, .4, .4, .4, 1, .4, .4, .4, 1), 3L))
    lam <- c(.7, .6, .8, .65, .75, .7, .6, .7, .8)
    x <- sapply(1:9, function(j) lam[j] * eta[, (j - 1L) %/% 3L + 1L] + sqrt(1 - lam[j]^2) * stats::rnorm(n))
    out <- as.data.frame(x)
    names(out) <- paste0("x", 1:9)
    out$y <- as.numeric(0.5 * eta[, 1] + 0.3 * eta[, 2] + stats::rnorm(n))
    out
  })
  items <- d[, paste0("x", 1:9)]
  meta <- identity_meta(items, construct = rep(c("y", "B", "C"), each = 3L))

  # The two readings, each written by hand with names that cannot collide.
  measurement <- "SA =~ x1 + x2 + x3\nSB =~ x4 + x5 + x6\nSC =~ x7 + x8 + x9"
  observed_y <- lavaan::sem(paste(measurement, "y ~ SA + SB + SC", sep = "\n"), d,
    estimator = "ML", std.lv = TRUE, missing = "ml"
  )
  latent_y <- lavaan::sem(paste(measurement, "SA ~ SB + SC", sep = "\n"), d,
    estimator = "ML", std.lv = TRUE, missing = "ml"
  )
  # Degrees of freedom by counting, for the observed outcome: ten observed
  # variables give 55 variances and covariances; the model has 9 loadings,
  # 9 residual variances, 3 factor covariances, 3 regressions, and the
  # residual variance of y (means are saturated).
  expect_equal(unname(lavaan::fitMeasures(observed_y, "df")), 55 - (9 + 9 + 3 + 3 + 1))

  # By name alone the model is refused, and the name is given.
  by_name <- SIEVER:::sieve_run_structural(items, d, meta, "y", c("y", "B", "C"))
  expect_identical(by_name$status$code, "failed")
  expect_null(by_name$fit)
  expect_match(by_name$error, "'y' is both a construct label and a column of the data", fixed = TRUE)
  expect_null(sieve_run_structural(items, d, meta, "y", c("y", "B", "C"))$fit)

  # Said to be the column: the observed outcome on all three constructs.
  ext <- SIEVER:::sieve_run_structural(items, d, meta, "y", c("y", "B", "C"),
    outcome_kind = "external"
  )
  expect_identical(ext$status$code, "ok")
  expect_setequal(lavaan::lavNames(ext$fit, "ov"), c(paste0("x", 1:9), "y"))
  expect_false("y" %in% lavaan::lavNames(ext$fit, "lv"))
  expect_equal(unname(lavaan::fitMeasures(ext$fit, c("chisq", "df"))),
    unname(lavaan::fitMeasures(observed_y, c("chisq", "df"))),
    tolerance = 1e-6
  )
  got <- lavaan::standardizedSolution(ext$fit)
  ref <- lavaan::standardizedSolution(observed_y)
  expect_identical(got$lhs[got$op == "~"], rep("y", 3L))
  expect_equal(got$est.std[got$op == "~"], ref$est.std[ref$op == "~"], tolerance = 1e-6)
  # The two are told apart where they are shown.
  expect_identical(
    SIEVER:::sieve_show_names(c("y", ext$spec$latent[1L]), ext$fit),
    c("y (data column)", "y")
  )

  # Said to be the construct: the latent outcome on the other two.
  con <- SIEVER:::sieve_run_structural(items, d, meta, "y", c("y", "B", "C"),
    outcome_kind = "construct"
  )
  expect_identical(con$status$code, "ok")
  expect_setequal(lavaan::lavNames(con$fit, "ov"), paste0("x", 1:9))
  expect_equal(unname(lavaan::fitMeasures(con$fit, c("chisq", "df"))),
    unname(lavaan::fitMeasures(latent_y, c("chisq", "df"))),
    tolerance = 1e-6
  )

  # Both at once: two regressions, and the indirect effects through the
  # construct y onto the column y are defined from the right coefficients.
  both <- SIEVER:::sieve_run_structural(items, d, meta, c("y", "y"), c("y", "B", "C"),
    outcome_kind = c("construct", "external"), effects = TRUE
  )
  expect_identical(both$status$code, "ok")
  by_hand <- lavaan::sem(
    paste(measurement, "SA ~ a1*SB + a2*SC", "y ~ b*SA + c1*SB + c2*SC",
      "i1 := a1*b", "i2 := a2*b",
      sep = "\n"
    ),
    d,
    estimator = "ML", std.lv = TRUE, missing = "ml"
  )
  eff <- lavaan::parameterEstimates(both$fit)
  ref_eff <- lavaan::parameterEstimates(by_hand)
  expect_equal(eff$est[eff$op == ":=" & grepl("^ind", eff$lhs)],
    ref_eff$est[ref_eff$op == ":="],
    tolerance = 1e-6
  )
  expect_true(all(grepl("y \\(data column\\)", both$spec$eff_map)))
  # A kind that names no construct is refused.
  none <- SIEVER:::sieve_run_structural(items, d, meta, "nothing", c("B", "C"),
    outcome_kind = "construct"
  )
  expect_identical(none$status$code, "failed")
  expect_match(none$error, "no construct with included items has that label", fixed = TRUE)
  # A construct labeled like an item is still the construct: an item is not
  # offered as an external outcome.
  item_named <- meta
  item_named$construct[item_named$construct == "y"] <- "x9"
  ok <- SIEVER:::sieve_run_structural(items, d, item_named, "x9", c("B", "C"))
  expect_identical(ok$status$code, "ok")
  expect_setequal(lavaan::lavNames(ok$fit, "ov"), paste0("x", 1:9))

  # The selector gives the two entries values of their own.
  choices <- SIEVER:::sieve_outcome_choices(c("B", "C", "y"), "y")
  expect_false(any(duplicated(choices$value)))
  expect_identical(choices$value[choices$name == "y"], c("y (construct)", "y (data column)"))
  expect_identical(choices$value[choices$name == "B"], "B")
  picked <- SIEVER:::sieve_resolve_outcomes(c("y (data column)", "B", "y"), choices)
  expect_identical(picked$name, c("y", "B", "y"))
  expect_identical(picked$kind, c("external", "construct", NA_character_))

  # The SEM tab, driven as a user would.
  st <- sieve_new_state()
  st$dataset_name <- "ambiguous"
  st$raw <- d
  st$items <- items
  st$id_vars <- "y"
  st$meta <- meta
  st$confirmed <- TRUE
  st$palette <- "nd"
  st$thresholds <- sieve_default_thresholds()
  # The outcome selector offers the columns with the Criterion role, which
  # the researcher sets on the Ingest tab; y is given that role here.
  roles <- SIEVER:::sieve_roles_table(d, items)
  roles$role[roles$variable == "y"] <- "criterion"
  st$roles <- roles
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(
      outcomes = "y (data column)", predictors = c("y", "B", "C"),
      layout = "tree2", min_path = 0
    )
    session$setInputs(fit_go = 1)
    run <- path_run()
    expect_identical(run$status$code, "ok")
    expect_true("y" %in% lavaan::lavNames(run$fit, "ov"))
    expect_equal(unname(lavaan::fitMeasures(run$fit, "df")), 30)
    session$setInputs(outcomes = "y (construct)")
    session$setInputs(fit_go = 2)
    expect_false("y" %in% lavaan::lavNames(path_run()$fit, "ov"))
    # A bare "y" is not one of the selector's values; it is refused, not read
    # as the construct.
    session$setInputs(outcomes = "y")
    session$setInputs(fit_go = 3)
    expect_identical(path_run()$status$code, "failed")
    expect_null(fit())
    expect_match(identity_text(output$gate), "both a construct label and a column of the data",
      fixed = TRUE
    )
  })
})

test_that("the Bollen-Stine refit carries a status and reports nothing without a solution", {
  # A model that fits (the one-factor data above), with the construct
  # labeled like its first item: every indicator stays in the refit.
  x <- one_factor_n500()
  spec <- sieve_cfa_spec(identity_meta(x, "x1"), reserved = names(x))
  withr::local_seed(2)
  before <- .Random.seed
  res <- SIEVER:::sieve_bollen_stine(spec, x, B = 100L, seed = 41L)
  expect_identical(.Random.seed, before)
  expect_null(res$error)
  expect_identical(res$status$code, "ok")
  expect_identical(res$rng$seed, 41L)
  expect_identical(res$n, 500L)
  # The same bootstrap run with lavaan directly, on hand-written syntax.
  # SIEVER passes its seed to DMAR::sem_exact_fit_bootstrap(), which draws
  # the resamples lavaan draws under the same seed given as lavaan's own
  # iseed (lavaan called without iseed draws one from the caller's stream
  # instead).
  direct <- lavaan::cfa(
    "SAFE =~ x1 + x2 + x3 + x4 + x5 + x6", x,
    estimator = "ML", std.lv = TRUE,
    test = "bollen.stine", bootstrap = 100L, iseed = 41L
  )
  expect_equal(res$p_bs, lavaan::lavInspect(direct, "test")$bollen.stine$pvalue, tolerance = 1e-12)
  expect_equal(res$chisq, unname(lavaan::fitMeasures(direct, "chisq")), tolerance = 1e-8)
  expect_equal(res$df, 27 - 18)
  expect_true(res$p_bs > 0 && res$p_bs < 1)

  # Not identified (three orthogonal factors, one with two indicators): the
  # status says so and no statistic or p-value is returned.
  hs <- hs_parts()
  orth <- sieve_cfa_spec(hs$meta[1:8, ], orthogonal = TRUE)
  none <- SIEVER:::sieve_bollen_stine(orth, hs$items, B = 100L, seed = 1L)
  expect_null(none$error)
  expect_identical(none$status$code, "not_identified")
  expect_true(all(is.na(unlist(none[c("chisq", "df", "p_ml", "p_bs")]))))
  # Ordered items and too few cases are refused with the reason.
  cat_meta <- hs$meta
  cat_meta$type <- "ordered"
  expect_match(SIEVER:::sieve_bollen_stine(sieve_cfa_spec(cat_meta), hs$items)$error,
    "categorical (WLSMV) estimator",
    fixed = TRUE
  )
  expect_match(SIEVER:::sieve_bollen_stine(sieve_cfa_spec(hs$meta), hs$items[1:8, ])$error,
    "too few to bootstrap",
    fixed = TRUE
  )

  # The panel: a refit with no solution shows its status and no table.
  two <- x[, 1:2]
  st <- sieve_new_state()
  st$dataset_name <- "two"
  st$raw <- two
  st$items <- two
  st$id_vars <- character(0)
  st$meta <- identity_meta(two)
  st$confirmed <- TRUE
  st$palette <- "nd"
  st$thresholds <- sieve_default_thresholds()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE,
      bs_B = 100, bs_seed = 11
    )
    session$setInputs(fit_go = 1)
    expect_identical(cfa_run()$status$code, "not_identified")
    session$setInputs(bs_run = 1)
    r <- bs_result()
    expect_null(r$error)
    expect_false(r$solved)
    expect_identical(r$status$code, "not_identified")
    note <- identity_text(output$bs_note)
    expect_match(note, "bootstrap refit is not identified", fixed = TRUE)
    expect_false(grepl("Read the two p-values together", note, fixed = TRUE))
    expect_error(output$bs_tbl)
  })
})

test_that("the CFA tab shows no estimate table for a model with no solution", {
  hs <- hs_parts()
  st <- identity_state(raw = hs$fx$data[1:8, ])
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE,
      # The subject is MLR's rank refusal, so MLR is chosen (Follow the
      # item types gives ML with FIML).
      estimator = "mlr"
    )
    # Under MLR, 27 free parameters for 8 respondents are refused before
    # the fit: robust standard errors would be rank deficient, and the fit
    # would not converge. Nothing is fit and no estimate is shown.
    session$setInputs(fit_go = 1)
    expect_null(cfa_run()$fit)
    expect_false(sieve_has_solution(cfa_run()$status))
    # One statement about the model, not two that contradict each other, and
    # nothing leaks to the console while the tab renders.
    gate <- expect_no_warning(identity_text(output$gate))
    expect_match(gate, "rank deficient", fixed = TRUE)
    expect_false(grepl("Improper solution", gate, fixed = TRUE))
    expect_false(grepl("even though the model converged", gate, fixed = TRUE))
    for (tbl in c("est_tbl", "phi_tbl", "mi_tbl", "fit_tbl")) {
      expect_no_warning(expect_error(output[[tbl]]))
    }
  })
  # An improper solution is still a solution: it is flagged, and shown.
  heywood <- NULL
  for (seed in 1:60) {
    d <- withr::with_seed(seed, {
      e <- matrix(stats::rnorm(100), 50, 2) %*% chol(matrix(c(1, .5, .5, 1), 2))
      data.frame(
        a1 = .97 * e[, 1] + stats::rnorm(50, sd = .25),
        a2 = .45 * e[, 1] + stats::rnorm(50, sd = .9),
        a3 = .45 * e[, 1] + stats::rnorm(50, sd = .9),
        b1 = .7 * e[, 2] + stats::rnorm(50, sd = .7),
        b2 = .7 * e[, 2] + stats::rnorm(50, sd = .7),
        b3 = .7 * e[, 2] + stats::rnorm(50, sd = .7)
      )
    })
    run <- sieve_run_cfa(sieve_cfa_spec(identity_meta(d, rep(c("A", "B"), each = 3L))), d)
    if (identical(run$status$code, "inadmissible")) {
      heywood <- d
      break
    }
  }
  skip_if(is.null(heywood), "no improper solution among the sixty samples")
  st <- sieve_new_state()
  st$dataset_name <- "heywood"
  st$raw <- heywood
  st$items <- heywood
  st$id_vars <- character(0)
  st$meta <- identity_meta(heywood, rep(c("A", "B"), each = 3L))
  st$confirmed <- TRUE
  st$palette <- "nd"
  st$thresholds <- sieve_default_thresholds()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    session$setInputs(fit_go = 1)
    expect_identical(cfa_run()$status$code, "inadmissible")
    expect_match(identity_text(output$gate), "Improper solution", fixed = TRUE)
    expect_no_error(output$est_tbl)
  })
})

test_that("tables and files show the researcher's labels, not the allocated names", {
  hs <- hs_parts()
  # Two labels that cannot be names (no ASCII letter) and one that is an
  # item's name: the latent variables become F1, F2, and F_t2_cubes.
  named <- hs$meta
  labels <- c("心理", "能力", "t2_cubes")
  named$construct[hs$meta$construct == "Visual"] <- labels[1L]
  named$construct[hs$meta$construct == "Textual"] <- labels[2L]
  named$construct[hs$meta$construct == "Speed"] <- labels[3L]
  fit <- sieve_fit_cfa(hs$items, named)
  expect_identical(names(attr(fit, "name_map")), c("F1", "F2", "F_t2_cubes"))
  # Factor scores: model names by default, labels on request, same values.
  plain <- sieve_factor_scores(fit)
  shown <- sieve_factor_scores(fit, labels = TRUE)
  expect_identical(names(plain), c("F1", "F2", "F_t2_cubes"))
  expect_identical(names(shown), labels)
  expect_equal(unname(as.matrix(shown)), unname(as.matrix(plain)))

  st <- identity_state(meta = named)
  shiny::testServer(mod_reliab_server, args = list(state = st), {
    session$setInputs(score_method = "Bartlett")
    session$setInputs(scores_go = 1)
    expect_identical(names(fscores()), labels)
  })
  # Modification indices: the same indices as lavaan's, with the labels.
  mi <- SIEVER:::sieve_modification_indices(fit)
  direct <- lavaan::modindices(fit, sort = TRUE, minimum_value = 3.84)
  # The rows lavaan computes keep lavaan's values and order. The residual
  # covariances lavaan leaves without an index on this raw score scale are
  # recovered by the score test (test-cfa-modindices.R) and rank among them,
  # under the labels like every other row.
  from_lavaan <- !(mi$table$Path %in% mi$recovered)
  expect_true(all(mi$recovered %in% mi$table$Path))
  expect_equal(mi$table$MI[from_lavaan], utils::head(direct$mi, sum(from_lavaan)), tolerance = 1e-12)
  expect_gt(nrow(mi$table), 0L)
  expect_true(any(direct$lhs %in% c("F1", "F2", "F_t2_cubes")))
  expect_false(any(grepl("^(F1|F2|F_t2_cubes) ", mi$table$Path)))
  expect_true(all(sub(" (=~|~~) .*$", "", mi$table$Path) %in% c(labels, hs$fx$item_cols)))
  expect_identical(mi$table$Type[from_lavaan], ifelse(
    utils::head(direct$op, sum(from_lavaan)) == "=~", "cross loading", "correlated residual"
  ))
  expect_match(SIEVER:::sieve_modification_indices(fit, minimum = 1e6)$note, "No modification indices",
    fixed = TRUE
  )
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE
    )
    session$setInputs(fit_go = 1)
    expect_no_error(output$mi_tbl)
  })
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    session$setInputs(inv_level = "metric")
    session$setInputs(inv_diag = 1)
    dg <- inv_diag()
    expect_null(dg$error)
    # The label is shown; the engine's name is kept for the refit.
    expect_true(all(dg$table$lhs %in% c("F1", "F2", "F_t2_cubes")))
    expect_false(any(grepl("^F1 |^F2 |^F_t2_cubes ", dg$table$parameter)))
    expect_true(all(sub(" =~.*$", "", dg$table$parameter) %in% labels))
    expect_type(dg$warnings, "character")
    session$setInputs(inv_diag_tbl_rows_selected = 1)
    session$setInputs(inv_refit = 1)
    p_res <- inv_partial()
    expect_null(p_res$error)
    expect_match(p_res$freed, "^F", perl = TRUE)
    expect_true(sub(" =~.*$", "", p_res$freed_label) %in% labels)
    out <- identity_text(output$inv_partial_out)
    expect_match(out, sprintf("freeing %s", p_res$freed_label), fixed = TRUE)
  })
})

test_that("the aligned factor is the construct under its own latent name, clear of an item named f", {
  # DMAR's default factor name is f. Five items on one factor in two groups,
  # the first item named f: the alignment must keep all five, and the factor
  # is the construct under the latent name the ledger's name plan gives it
  # (its label, Trait, which collides with nothing).
  n <- 400L
  d <- withr::with_seed(31L, {
    one <- function(mu) {
      f <- stats::rnorm(n, mu)
      as.data.frame(sapply(c(.8, .7, .75, .65, .7), function(l) l * f + stats::rnorm(n, sd = .6)))
    }
    rbind(one(0), one(0.35))
  })
  names(d) <- c("f", paste0("i", 2:5))
  d$grp <- rep(c("a", "b"), each = n)
  st <- sieve_new_state()
  st$dataset_name <- "align"
  st$raw <- d
  st$items <- d[, 1:5]
  st$id_vars <- "grp"
  st$meta <- identity_meta(d[, 1:5], "Trait")
  st$confirmed <- TRUE
  st$palette <- "nd"
  st$thresholds <- sieve_default_thresholds()
  # The same data with the first item named i1, aligned by DMAR directly
  # with its own default model.
  plain <- d
  names(plain)[1L] <- "i1"
  ref <- DMAR::measurement_alignment(plain,
    items = paste0("i", 1:5), group = "grp",
    alignment = "fixed", estimator = "ML", n_starts = 5,
    seed = 7
  )
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(
      group = "grp", align_construct = "Trait", align_std = FALSE,
      align_starts = 5, align_seed = 7, align_ident = "fixed"
    )
    session$setInputs(align_go = 1)
    a <- align_res()
    expect_null(a$error)
    expect_identical(a$model, "Trait =~ f + i2 + i3 + i4 + i5")
    expect_identical(a$factor_name, "Trait")
    expect_identical(unname(a$name_map), "Trait")
    expect_identical(a$items, c("f", paste0("i", 2:5)))
    expect_equal(a$table$factor_mean, ref$factor_mean, tolerance = 1e-8)
    expect_equal(a$table$factor_variance, ref$factor_variance, tolerance = 1e-8)
    expect_setequal(a$r2$item, c("f", paste0("i", 2:5)))
    expect_type(a$warnings, "character")
    expect_match(paste(shiny::isolate(state$codelog), collapse = "\n"),
      "model = \"Trait =~ f + i2 + i3 + i4 + i5\"",
      fixed = TRUE
    )
  })
  # The construct itself labeled f: the plan allocates a name clear of the
  # item (F_f), as it does for every other model built from the ledger.
  st$meta <- identity_meta(d[, 1:5], "f")
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(
      group = "grp", align_construct = "f", align_std = FALSE,
      align_starts = 2, align_seed = 7, align_ident = "fixed"
    )
    session$setInputs(align_go = 1)
    a <- align_res()
    expect_null(a$error)
    expect_identical(a$model, "F_f =~ f + i2 + i3 + i4 + i5")
    expect_identical(
      a$factor_name,
      sieve_latent_name(
        sieve_name_plan(
          identity_meta(
            d[
              ,
              1:5
            ],
            "f"
          ),
          reserved = "grp"
        ),
        "f"
      )
    )
    expect_identical(unname(a$name_map), "f")
    expect_identical(a$items, c("f", paste0("i", 2:5)))
    expect_equal(a$table$factor_mean, ref$factor_mean, tolerance = 1e-6)
  })
})

# ---- Values that are not numbers are refused by name, never coerced --------

test_that("a column that cannot be read as numbers is refused by name, never coerced to missing", {
  x <- one_factor_n500()
  spec <- sieve_cfa_spec(identity_meta(x), reserved = names(x))
  clean <- sieve_run_model(spec, x)
  expect_identical(clean$status$code, "ok")

  # Thirty entries of x1 are text. The model is not fit on the 470 cases that
  # remain: it is refused, and the refusal names the column, the count, and
  # one of the entries.
  text <- x
  text$x1 <- as.character(text$x1)
  text$x1[1:30] <- "n/a"
  run <- sieve_run_model(spec, text)
  expect_identical(run$status$code, "failed")
  expect_null(run$fit)
  expect_match(run$error, "Item 'x1' holds 30 values that are not numbers", fixed = TRUE)
  expect_match(run$error, "\"n/a\"", fixed = TRUE)
  expect_match(run$error, "Items tab", fixed = TRUE)
  expect_identical(sieve_run_cfa(spec, text)$status$code, "failed")
  expect_null(sieve_fit_cfa(text, identity_meta(x)))
  # A factor column with text levels is text; a factor of numbers is read.
  lev <- x
  lev$x2 <- factor(ifelse(lev$x2 > 0, "agree", "disagree"))
  expect_match(sieve_run_model(spec, lev)$error, "Item 'x2' holds 500 values", fixed = TRUE)
  # Numbers stored as text are numbers: the same fit as the numeric columns.
  as_text <- x
  as_text$x1 <- format(as_text$x1, digits = 17)
  again <- sieve_run_model(spec, as_text)
  expect_identical(again$status$code, "ok")
  expect_equal(unname(lavaan::fitMeasures(again$fit, "chisq")),
    unname(lavaan::fitMeasures(clean$fit, "chisq")),
    tolerance = 1e-10
  )
  # Blank entries are missing values, not problems.
  blank <- x
  blank$x1 <- as.character(blank$x1)
  blank$x1[1:5] <- ""
  expect_identical(sieve_run_model(spec, blank)$status$code, "ok")
  expect_identical(sieve_numeric_columns(blank, "x1")$problems$kind, character(0))
  expect_identical(sum(is.na(sieve_numeric_columns(blank, "x1")$data$x1)), 5L)

  # A column with one value throughout, or with no observed values, is
  # refused by name before the engine sees it, and nothing is printed.
  flat <- x
  flat$x3 <- 2
  printed <- utils::capture.output(const <- sieve_run_model(spec, flat))
  expect_length(printed, 0L)
  expect_identical(const$status$code, "failed")
  expect_match(const$error, "Item 'x3' takes the same value (2) for every case", fixed = TRUE)
  none <- x
  none$x4 <- NA_real_
  expect_match(sieve_run_model(spec, none)$error, "Item 'x4' has no observed values", fixed = TRUE)
  read <- sieve_numeric_columns(flat, names(flat))
  expect_identical(read$problems$column, "x3")
  expect_identical(read$problems$kind, "no_variance")
  expect_true(all(vapply(read$data, is.double, logical(1))))
})

test_that("what the engine prints while stopping is captured and the variable it flags is named", {
  # An ordered item with two categories, where every case in the second
  # category is incomplete on another item: among the complete cases the
  # engine fits (listwise deletion under WLSMV) the item has one level. The
  # whole column passes the reading, so the engine itself stops, with its
  # own message naming the item; nothing reaches the console.
  n <- 240L
  d <- withr::with_seed(7L, {
    eta <- stats::rnorm(n)
    out <- as.data.frame(lapply(1:4, function(j) {
      as.integer(cut(.7 * eta + sqrt(.51) * stats::rnorm(n), c(-Inf, -.8, 0, .8, Inf)))
    }))
    names(out) <- paste0("y", 1:4)
    out
  })
  d$y3 <- ifelse(d$y3 >= 3L, 2L, 1L)
  d$y1[d$y3 == 2L] <- NA_integer_
  expect_identical(sieve_numeric_columns(d, names(d))$problems$kind, character(0))
  spec <- sieve_cfa_spec(identity_meta(d, type = "ordered"), reserved = names(d))
  expect_identical(spec$estimator, "WLSMV")
  printed <- utils::capture.output(run <- sieve_run_model(spec, d))
  expect_length(printed, 0L)
  expect_identical(run$status$code, "failed")
  expect_match(run$error, "some categories of variable `y3' are empty", fixed = TRUE)
  expect_identical(run$console, character(0))

  # A column with one value throughout is refused before the engine sees it.
  # Were it to reach the engine (the reading is stood down here to make it),
  # lavaan would print its variable table to the console and stop with a
  # message that names no variable: the print is captured, kept, and the
  # variable it marks is added to the message.
  x <- one_factor_n500()
  x$x3 <- 2
  cspec <- sieve_cfa_spec(identity_meta(x), reserved = names(x))
  expect_match(sieve_run_model(cspec, x)$error, "Item 'x3' takes the same value", fixed = TRUE)
  testthat::local_mocked_bindings(sieve_numeric_columns = function(data, columns, what = "Item") {
    list(
      data = data[, columns, drop = FALSE], problems = data.frame(column = character(0)),
      text = character(0)
    )
  })
  printed <- utils::capture.output(reached <- sieve_run_model(cspec, x))
  expect_length(printed, 0L)
  expect_identical(reached$status$code, "failed")
  expect_match(reached$error, "the engine flagged 'x3'", fixed = TRUE)
  expect_true(any(grepl("^3\\*\\*\\*\\s+x3", reached$console)))
  expect_gt(length(reached$console), 3L)
  # The message builder on its own.
  expect_identical(SIEVER:::sieve_engine_failure("lavaan ERROR: some reason."), "lavaan ERROR: some reason.")
  expect_identical(
    SIEVER:::sieve_engine_failure("a reason.", c("1 x1 ...", "2*** x2 ...", "3*** x3 ...")),
    "a reason (the engine flagged 'x2', 'x3')."
  )
})

test_that("the modules refuse an item that cannot be read as numbers and say which", {
  skip_if_not_installed("shiny")
  hs <- hs_parts()
  raw <- hs$fx$data
  bad <- hs$fx$item_cols[1L]
  raw[[bad]] <- as.character(raw[[bad]])
  raw[[bad]][1:30] <- "n/a"
  st <- identity_state(raw = raw)
  wants <- function(x) {
    expect_match(x, sprintf("Item '%s' holds 30 values that are not numbers", bad), fixed = TRUE)
  }
  shiny::testServer(mod_finalize_server, args = list(state = st), {
    session$setInputs(holdout = 50, seed = 3)
    session$setInputs(split = 1)
    r <- cv_result()
    expect_match(r$error, "The split cannot be fit", fixed = TRUE)
    wants(r$error)
    expect_match(identity_text(output$cv_msg), "cannot be fit", fixed = TRUE)
    expect_error(output$cv_tbl, class = "shiny.silent.error")
    session$setInputs(group = "school")
    session$setInputs(run_inv = 1)
    r <- inv_result()
    expect_match(r$error, "cannot be fit", fixed = TRUE)
    wants(r$error)
    expect_error(output$inv_tbl, class = "shiny.silent.error")
    session$setInputs(
      group = "school", align_construct = "Visual", align_std = FALSE,
      align_starts = 2, align_seed = 1, align_ident = "fixed"
    )
    session$setInputs(align_go = 1)
    a <- align_res()
    expect_match(a$error, "'Visual' cannot be aligned", fixed = TRUE)
    wants(a$error)
    nr <- norms_read()
    expect_false("Visual" %in% nr$table$construct)
    expect_true(all(c("Textual", "Speed") %in% nr$table$construct))
    expect_match(nr$refused, "'Visual' is not scored", fixed = TRUE)
    wants(nr$refused)
    expect_match(identity_text(output$norms_note), "not scored", fixed = TRUE)
  })
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE,
      bs_B = 100, bs_seed = 11
    )
    session$setInputs(fit_go = 1)
    expect_identical(cfa_run()$status$code, "failed")
    wants(cfa_run()$error)
    expect_match(identity_text(output$gate), "could not be fit", fixed = TRUE)
    wants(identity_text(output$gate))
    session$setInputs(bs_run = 1)
    b <- bs_result()
    wants(b$error)
    expect_error(output$bs_tbl, class = "shiny.silent.error")
  })
  shiny::testServer(mod_efa_server, args = list(state = st), {
    session$setInputs(n_factors = 3, rotation = "oblimin")
    session$setInputs(efa_go = 1L)
    run <- efa_run()
    expect_null(run$fit)
    wants(run$refused)
    # output$status_line is the EFA page's one status line, with the caveat
    # on the displayed solution.
    expect_match(identity_text(output$status_line), "The EFA was not fit", fixed = TRUE)
    wants(identity_text(output$status_line))
    expect_error(output$pattern_tbl, class = "shiny.silent.error")
  })
})

test_that("the Bollen-Stine result is not shown after the dataset, the data, or the ledger change", {
  skip_if_not_installed("shiny")
  x <- one_factor_n500()
  st <- sieve_new_state()
  st$dataset_name <- "one"
  st$raw <- x
  st$items <- x
  st$id_vars <- character(0)
  st$meta <- identity_meta(x)
  st$confirmed <- TRUE
  st$palette <- "nd"
  st$thresholds <- sieve_default_thresholds()
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(
      structure = "first", ident = "varone", layout = "tree2",
      what = "std", min_path = 0, resid = TRUE, covs = TRUE,
      bs_B = 100, bs_seed = 5
    )
    # The run is a background process: the mock clock advances while real
    # time passes, and a run still going at the limit fails here and ends
    # the checks.
    bs_finished <- function() {
      siever_wait_until(function() !isTRUE(shiny::isolate(bs_running())),
        "The Bollen-Stine run", step = function() {
          Sys.sleep(0.05)
          session$elapse(250)
        })
    }
    session$setInputs(bs_run = 1)
    if (bs_finished()) {
      r <- bs_result()
      expect_null(r$error)
      expect_true(r$solved)
      expect_no_error(output$bs_tbl)
      # A write to the state from here does not run the outputs' observers by
      # itself, as an input would; the flush stands in for the one the app does.
      sieve_new_dataset(state)
      session$flushReact()
      r <- bs_result()
      expect_true(isTRUE(r$stale))
      expect_match(r$error, "a different dataset has been loaded since", fixed = TRUE)
      expect_match(r$error, "Press Run Bollen-Stine bootstrap", fixed = TRUE)
      expect_match(identity_text(output$bs_note), "computed from different inputs", fixed = TRUE)
      expect_error(output$bs_tbl, class = "shiny.silent.error")
      session$setInputs(bs_run = 2)
      if (bs_finished()) {
        expect_null(bs_result()$error)
        m <- shiny::isolate(state$meta)
        m$status[1L] <- "parked"
        state$meta <- m
        session$flushReact()
        expect_match(bs_result()$error, "the item ledger has changed since", fixed = TRUE)
        expect_error(output$bs_tbl, class = "shiny.silent.error")
      }
    }
  })
})

test_that("the SEM outcome selector offers the criterion columns and never an identifier", {
  skip_if_not_installed("shiny")
  hs <- hs_parts()
  d <- hs$fx$data
  n <- nrow(d)
  d$pcode <- withr::with_seed(2L, sample(100000:999999, n))
  d$gpa <- withr::with_seed(3L, stats::rnorm(n, 3, .4))
  d$tx <- rep_len(c(0L, 1L), n)
  st <- identity_state(raw = d)
  st$id_vars <- setdiff(names(d), hs$fx$item_cols)
  roles <- sieve_roles_table(d, d[, hs$fx$item_cols, drop = FALSE], NULL)
  roles$role[roles$variable == "pcode"] <- "identifier"
  roles$role[roles$variable == "gpa"] <- "criterion"
  roles$role[roles$variable == "tx"] <- "criterion" # binary: offered, with an explicit type required
  st$roles <- roles
  shiny::isolate(sieve_confirm_types(st))
  shiny::testServer(mod_sem_server, args = list(state = st), {
    expect_identical(aux_numeric(), c("gpa", "tx"))
    ch <- outcome_choices()
    expect_identical(ch$name[ch$kind == "external"], c("gpa", "tx"))
    expect_false(any(c("id", "pcode", "age_years", "school") %in% ch$name))
    # The role is followed: a criterion that becomes a background variable
    # leaves the selector, and age_years enters when it becomes a criterion.
    r2 <- roles
    r2$role[r2$variable == "gpa"] <- "auxiliary"
    r2$role[r2$variable == "age_years"] <- "criterion"
    state$roles <- r2
    sieve_confirm_types(state)
    session$flushReact()
    expect_identical(aux_numeric(), c("age_years", "tx"))
    # A binary criterion is offered without guessing its measurement level.
    # Fitting is refused until the researcher explicitly declares the type.
    session$setInputs(
      outcomes = "tx", predictors = c("Visual", "Textual"),
      layout = "tree2", min_path = 0
    )
    expect_identical(sem_choices()$outcome_types, c(tx = "unconfirmed"))
    session$setInputs(fit_go = 1)
    expect_identical(path_run()$status$code, "failed")
    expect_match(path_run()$error, "Declare each external outcome", fixed = TRUE)
    expect_null(fit())
    do.call(session$setInputs, stats::setNames(
      list("binary"),
      paste0("outcome_type_", SIEVER:::sieve_input_key("tx"))
    ))
    session$setInputs(fit_go = 2)
    binary_run <- path_run()
    expect_true(SIEVER:::sieve_has_solution(binary_run))
    expect_identical(binary_run$spec$ordered, "tx")
    expect_identical(binary_run$spec$estimator, "WLSMV")
    direct_args <- SIEVER:::sieve_spec_args(binary_run$spec)
    direct_args$data <- d
    direct_binary <- do.call(lavaan::sem, direct_args)
    expect_equal(lavaan::coef(binary_run$fit), lavaan::coef(direct_binary),
      tolerance = 1e-10
    )
    # The continuous criterion still fits under its own name.
    state$roles <- roles
    sieve_confirm_types(state)
    session$flushReact()
    session$setInputs(
      outcomes = "gpa", predictors = c("Visual", "Textual"),
      layout = "tree2", min_path = 0
    )
    session$setInputs(fit_go = 3)
    run <- path_run()
    expect_identical(run$status$code, "ok")
    expect_true("gpa" %in% lavaan::lavNames(run$fit, "ov"))
  })
  # An outcome column that cannot be read as numbers is named as a column,
  # with the action that fits an outcome.
  text <- d
  text$gpa <- as.character(text$gpa)
  text$gpa[1:4] <- "withdrawn"
  run <- SIEVER:::sieve_run_structural(text[, hs$fx$item_cols], text, hs$meta, "gpa",
    c("Visual", "Textual"),
    outcome_kind = "external"
  )
  expect_identical(run$status$code, "failed")
  expect_match(run$error, "Outcome column 'gpa' holds 4 values that are not numbers", fixed = TRUE)
  expect_match(run$error, "choose another outcome", fixed = TRUE)
  expect_false(grepl("Items tab", run$error, fixed = TRUE))
})

# ---- The SEM tab's recorded R runs top to bottom ----------------------------

test_that("the SEM tab's recorded R defines sem_fit before it is read and replays in a clean process", {
  skip_if_not_installed("shiny")
  skip_on_cran()
  hs <- hs_parts()
  d <- hs$fx$data
  roles <- sieve_roles_table(d, d[, hs$fx$item_cols, drop = FALSE], NULL)
  roles$role[roles$variable == "age_years"] <- "criterion"
  st <- identity_state()
  st$roles <- roles
  got <- new.env(parent = emptyenv())
  shiny::testServer(mod_sem_server, args = list(state = st), {
    # A latent outcome, an observed outcome, and a mediator (Textual is a
    # predictor and an outcome), so every step the tab records is in the log.
    session$setInputs(
      outcomes = c("Textual", "age_years"), predictors = c("Visual", "Textual"),
      layout = "tree2", min_path = 0
    )
    session$setInputs(fit_go = 1)
    expect_identical(path_run()$status$code, "ok")
    expect_no_error(output$paths_tbl)
    expect_no_error(output$r2_tbl)
    expect_identical(med_run()$status$code, "ok")
    expect_no_error(output$effects_tbl)
    ss <- lavaan::standardizedSolution(fit())
    got$paths <- ss[ss$op == "~", c("lhs", "rhs", "est.std")]
    got$r2 <- lavaan::lavInspect(fit(), "r2")
    se <- lavaan::standardizedSolution(med_fit())
    got$effects <- se[se$op == ":=", c("lhs", "est.std")]
    got$log <- shiny::isolate(state$codelog)
  })
  app_paths <- got$paths
  app_r2 <- got$r2
  app_effects <- got$effects
  log <- got$log
  # The first recorded step fits the model and defines sem_fit; no step
  # reads sem_fit or sem_fit_effects before the step that defines it.
  steps <- split(log, cumsum(startsWith(log, "# [")))
  expect_match(log[1L], "^# \\[1\\] Fit the structural model")
  expect_true(sieve_repro_reads(steps[[1L]])$creates("sem_fit"))
  defined <- character(0)
  for (s in steps) {
    r <- sieve_repro_reads(s)
    for (nm in c("sem_fit", "sem_fit_effects")) {
      if (r$reads(nm) && !r$creates(nm)) {
        expect_true(nm %in% defined, info = s[1L])
      }
      if (r$creates(nm)) defined <- c(defined, nm)
    }
  }
  expect_true(any(grepl("sem_std[sem_std$op == \"~\", ]", log, fixed = TRUE)))
  expect_false(any(grepl("standardizedSolution(fit)", log, fixed = TRUE)))
  lines <- unlist(strsplit(log, "\n", fixed = TRUE))
  first <- which(lines == "sem_data <- items")[1L]
  last <- grep("^sem_fit <- lavaan::sem", lines)[1L] - 1L
  expect_true(is.finite(first) && is.finite(last) && first < last)
  preparation <- parse(text = lines[seq.int(first, last)])
  # Replay the complete preparation block, including SPSS missing values and
  # the common numeric conversion, on factors with a nonnumeric level order.
  values <- c(10, 30, NA_real_, 20)
  for (input in list(values, as.character(values),
    factor(as.character(values), levels = c("30", "10", "20")),
    haven::labelled_spss(c(10, 30, 999, 20), na_values = 999))) {
    replay_input <- new.env(parent = baseenv())
    replay_input$d <- d[seq_along(input), , drop = FALSE]
    replay_input$d$age_years <- input
    replay_input$items <- replay_input$d
    replay_input$items$item <- seq_along(input)
    eval(preparation, replay_input)
    expect_identical(replay_input$sem_data$age_years, values)
    expect_identical(replay_input$sem_data$item, seq_along(input))
  }

  # The whole log, with the preamble the Reproducible Code view adds, runs
  # top to bottom in a new R process on the data as loaded, and gives the
  # numbers the tab shows.
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "data.csv")
  utils::write.csv(d, csv, row.names = FALSE)
  out_paths <- file.path(dir, "paths.csv")
  out_r2 <- file.path(dir, "r2.csv")
  out_effects <- file.path(dir, "effects.csv")
  script <- file.path(dir, "replay.R")
  writeLines(c(
    sprintf("d <- utils::read.csv(%s, stringsAsFactors = FALSE)", deparse(csv)),
    sieve_replay_script(st),
    "sem_std <- lavaan::standardizedSolution(sem_fit)",
    sprintf(
      "utils::write.csv(sem_std[sem_std$op == \"~\", c(\"lhs\", \"rhs\", \"est.std\")], %s, row.names = FALSE)",
      deparse(out_paths)
    ),
    sprintf(
      "utils::write.csv(data.frame(name = names(lavaan::lavInspect(sem_fit, \"r2\")), r2 = as.numeric(lavaan::lavInspect(sem_fit, \"r2\"))), %s, row.names = FALSE)",
      deparse(out_r2)
    ),
    "sem_std_effects <- lavaan::standardizedSolution(sem_fit)",
    sprintf(
      "utils::write.csv(sem_std_effects[sem_std_effects$op == \":=\", c(\"lhs\", \"est.std\")], %s, row.names = FALSE)",
      deparse(out_effects)
    )
  ), script)
  # The source under test, installed first on the path (helper-interface.R):
  # the log calls SIEVER::sieve_outcome_scale() and SIEVER::sieve_R2_intervals().
  out <- withr::with_envvar(
    c(R_LIBS = paste(compat_source_libs(), collapse = .Platform$path.sep)),
    suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(script)),
      stdout = TRUE, stderr = TRUE
    ))
  )
  status <- attr(out, "status")
  expect_true(is.null(status) || identical(status, 0L), info = paste(out, collapse = "\n"))
  expect_true(file.exists(out_paths), info = paste(out, collapse = "\n"))
  replay <- utils::read.csv(out_paths, stringsAsFactors = FALSE)
  expect_identical(nrow(replay), nrow(app_paths))
  expect_equal(replay$est.std, app_paths$est.std, tolerance = 1e-8)
  expect_identical(replay$lhs, app_paths$lhs)
  r2 <- utils::read.csv(out_r2, stringsAsFactors = FALSE)
  expect_equal(r2$r2[match(names(app_r2), r2$name)], as.numeric(app_r2), tolerance = 1e-8)
  effects <- utils::read.csv(out_effects, stringsAsFactors = FALSE)
  expect_identical(effects$lhs, app_effects$lhs)
  expect_equal(effects$est.std, app_effects$est.std, tolerance = 1e-8)
})

# ---- Gates name what stops the model and the one action that fixes it --------

test_that("the CFA and SEM gates name the construct or item and the action that fixes it", {
  skip_if_not_installed("shiny")
  hs <- hs_parts()
  cfa_gate <- function(meta) {
    st <- identity_state(meta = meta)
    got <- new.env(parent = emptyenv())
    shiny::testServer(mod_cfa_server, args = list(state = st), {
      session$setInputs(
        structure = "first", ident = "varone", layout = "tree2",
        what = "std", min_path = 0, resid = TRUE, covs = TRUE
      )
      session$setInputs(fit_go = 1)
      got$text <- identity_text(output$gate)
    })
    got$text
  }
  # Every item excluded.
  m <- hs$meta
  m$status <- "parked"
  g <- cfa_gate(m)
  expect_match(g, "There is no measurement model to fit", fixed = TRUE)
  expect_match(g, "Every item is excluded. On the Items tab, include the items", fixed = TRUE)
  # Three constructs with one included item each: each is named with its
  # count, and the one action is on the Items tab.
  m <- hs$meta
  m$status <- ifelse(m$item %in% c("t1_visual_perception", "t6_paragraph_comprehension", "t10_addition"),
    "active", "parked"
  )
  g <- cfa_gate(m)
  expect_match(g, "No construct has two or more included items. On the Items tab, include", fixed = TRUE)
  for (cn in c("Visual", "Textual", "Speed")) {
    expect_match(g, sprintf("Construct '%s' is not in the model: the construct has 1 included item", cn),
      fixed = TRUE
    )
  }
  # Included items with no construct.
  m <- hs$meta
  m$construct <- NA_character_
  g <- cfa_gate(m)
  expect_match(g, "No included item is assigned to a construct. On the Items tab, assign", fixed = TRUE)
  # A construct with one item selected as a predictor on the SEM tab: the
  # construct, its count, and the action.
  m <- hs$meta
  m$construct[9] <- "Alone"
  st <- identity_state(meta = m)
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(
      outcomes = "Visual", predictors = c("Textual", "Alone"),
      layout = "tree2", min_path = 0
    )
    session$setInputs(fit_go = 1)
    g <- identity_text(output$gate)
    expect_match(g, "'Alone' cannot enter the structural model", fixed = TRUE)
    expect_match(g, "'Alone' has 1 included item, and a factor needs at least 2", fixed = TRUE)
    expect_match(g, "On the Items tab, include at least 2 items of 'Alone'", fixed = TRUE)
  })
  # The hint for the engine's own messages: the item and the action, or
  # nothing when the message is not a known one.
  hint <- SIEVER:::sieve_failure_hint(
    "lavaan->lav_samp_step1():  \n   some categories of variable `y1' are empty in group 2; frequencies are \n   [36, 39, 75, 0] "
  )
  expect_match(hint, "Item 'y1' has a response category that no case in group 2 uses", fixed = TRUE)
  expect_match(hint, "Collapse that category with a neighboring one before loading the data", fixed = TRUE)
  expect_match(SIEVER:::sieve_failure_hint("the sample covariance matrix is not positive-definite"),
    "exclude one item of such a pair on the Items tab",
    fixed = TRUE
  )
  expect_null(SIEVER:::sieve_failure_hint("something else went wrong"))
  expect_null(SIEVER:::sieve_failure_hint(NULL))
  # The status alert carries the hint for a failed model.
  alert <- identity_text(sieve_status_alert(
    list(code = "failed", message = "some categories of variable `y1' are empty in group 2; frequencies are [1, 0]"),
    what = "ladder"
  ))
  expect_match(alert, "The ladder could not be fit.", fixed = TRUE)
  expect_match(alert, "Collapse that category", fixed = TRUE)
})

# The 24 tests as a higher-order model (one general factor over the five
# domains) are not a raw-scale artifact of lavaan's information matrix but a
# boundary. The Reasoning factor is explained entirely by the general factor
# (R-square .9997 under ML, with a second-order loading of 58 and a standard
# error of 6212; .999998 under MLR); its disturbance variance is fixed at 1,
# so the variance checks cannot see it. A status that read ok under ML and
# not identified under MLR would differ for one and the same solution; both
# name it as a boundary case.
test_that("a factor explained entirely by the factor above it is an improper boundary case under ML and MLR alike", {
  fx <- sieve_hs_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  meta$facet <- meta$construct
  meta$construct <- "General"
  X <- fx$data[, fx$item_cols]
  spec <- sieve_cfa_spec(meta, structure = "higher_order", reserved = names(fx$data), estimator = "mlr")
  for (est in c("MLR", "ML")) {
    fit <- suppressWarnings(lavaan::cfa(spec$model, data = X, std.lv = TRUE, estimator = est,
      missing = if (est == "MLR") "fiml" else "listwise"))
    st <- sieve_fit_status(fit)
    expect_identical(st$code, "inadmissible", info = est)
    expect_identical(as.character(st$boundary_factors), "General_Reasoning", info = est)
    expect_gt(attr(st$boundary_factors, "rsquare"), .999)
    expect_match(st$message, "General_Reasoning is explained entirely by General (R-square 0.99", fixed = TRUE)
    expect_false(st$admissible)
  }
  # The correlated-factors model of the same tests has no such factor.
  first <- sieve_cfa_spec(sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels),
    reserved = names(fx$data), estimator = "mlr")
  fit1 <- lavaan::cfa(first$model, data = X, std.lv = TRUE, estimator = "MLR", missing = "fiml")
  expect_identical(sieve_fit_status(fit1)$code, "ok")
  expect_length(sieve_boundary_factors(fit1), 0L)
})

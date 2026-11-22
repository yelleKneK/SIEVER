# Every exported function that takes a confidence level checks it with one
# shared check and refuses an invalid level with one message that names the
# argument and what was received. Every function takes its confidence level
# as `conf_level`, never `level`, and returns a `conf_level` column.

refusal <- function(arg, bad) {
  sprintf(
    "`%s` must be one number strictly between 0 and 1; received %s.", arg,
    paste(deparse(bad, width.cutoff = 200L, nlines = 2L), collapse = " ")
  )
}

bad_levels <- list(95, 1.5, 0, 1, -0.5, NA_real_, NA, Inf, c(.9, .95), "0.95", TRUE, NULL)

test_that("one shared check refuses every invalid level, naming the argument and the value", {
  for (bad in bad_levels) {
    expect_error(sieve_check_conf_level(bad), refusal("conf_level", bad), fixed = TRUE)
    expect_error(sieve_check_conf_level(bad, "level"), refusal("level", bad), fixed = TRUE)
  }
  # A percentage-style level says how to give it.
  expect_error(sieve_check_conf_level(95),
    "received 95. A level is a proportion (0.95 for 95%), not a percentage.",
    fixed = TRUE
  )
  expect_identical(sieve_check_conf_level(0.9), 0.9)
  expect_identical(sieve_check_conf_level(1L / 2L), 0.5)
})

test_that("every exported function with a confidence level refuses an invalid one before anything else", {
  # Each call is cut short by the check, so the other arguments are never
  # read: no NaN limits, no wording of a function's own, no arithmetic or
  # cor.test error, and no level read as given.
  calls <- list(
    sieve_invariance_interpret = function(x) sieve_invariance_interpret(NULL, conf_level = x),
    sieve_longitudinal_contrasts = function(x) sieve_longitudinal_contrasts(NULL, conf_level = x),
    sieve_norm_transform_limits = function(x) sieve_norm_transform_limits(NULL, conf_level = x),
    sieve_factor_score_record = function(x) sieve_factor_score_record(NULL, conf_level = x),
    sieve_R2_intervals = function(x) sieve_R2_intervals(NULL, conf_level = x),
    sieve_indirect_mc = function(x) sieve_indirect_mc(NULL, conf_level = x),
    sieve_model_composite = function(x) sieve_model_composite(NULL, conf_level = x),
    sieve_plan_congeneric = function(x) sieve_plan_congeneric(NULL, NULL, conf_level = x),
    sieve_mi_plan = function(x) sieve_mi_plan(.5, conf_level = x),
    sieve_mi_model = function(x) sieve_mi_model("f =~ a + b + c", NULL, NULL, conf_level = x),
    sieve_irt_parameter_evidence = function(x) sieve_irt_parameter_evidence(NULL, conf_level = x),
    sieve_irt_information_bands = function(x) sieve_irt_information_bands(NULL, conf_level = x),
    sieve_irt_empirical_traces = function(x) sieve_irt_empirical_traces(NULL, NULL, "a", conf_level = x),
    sieve_reliability_spec = function(x) sieve_reliability_spec(c("a", "b", "c"), rep("continuous", 3), conf_level = x),
    sieve_bifactor_bootstrap = function(x) sieve_bifactor_bootstrap(NULL, NULL, conf_level = x),
    sieve_bollen_stine = function(x) sieve_bollen_stine(NULL, NULL, conf_level = x),
    sieve_diagram_spec = function(x) sieve_diagram_spec(NULL, conf_level = x),
    sieve_efa_fit_by_k = function(x) sieve_efa_fit_by_k(NULL, "ml", conf_level = x),
    sieve_compare_structures = function(x) sieve_compare_structures(NULL, NULL, conf_level = x),
    sieve_fit_compare = function(x) sieve_fit_compare(list(), conf_level = x),
    sieve_validity_estimate = function(x) sieve_validity_estimate(1:5, 1:5, conf_level = x),
    sieve_ordinal_fit_impact = function(x) sieve_ordinal_fit_impact(NULL, NULL, "A", "B", anchors = "a", conf_level = x)
  )
  # Every export with a confidence level argument is listed here.
  exports <- getNamespaceExports("SIEVER")
  with_level <- Filter(function(e) {
    f <- get(e, envir = asNamespace("SIEVER"))
    is.function(f) && "conf_level" %in% names(formals(f))
  }, exports)
  expect_setequal(names(calls), with_level)
  for (fun in names(calls)) {
    for (bad in list(95, 0, NA_real_, c(.9, .95), "0.95")) {
      expect_error(calls[[fun]](bad), refusal("conf_level", bad), fixed = TRUE, label = fun)
    }
  }
  # No exported function takes a confidence level named `level`; `level`
  # is the invariance rung of the longitudinal family (named by
  # sieve_longitudinal_spec(), and passed on to it by
  # sieve_run_longitudinal()).
  named_level <- Filter(function(e) {
    f <- get(e, envir = asNamespace("SIEVER"))
    is.function(f) && "level" %in% names(formals(f))
  }, exports)
  expect_identical(named_level, "sieve_longitudinal_spec")
})

test_that("sieve_invariance_interpret() forms its contrasts at the level given, in a conf_level column", {
  hs <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6", hs,
    group = "school", group.equal = c("loadings", "intercepts"), std.lv = TRUE
  )
  at95 <- sieve_invariance_interpret(fit)$contrasts
  at90 <- sieve_invariance_interpret(fit, conf_level = .90)$contrasts
  expect_false("level" %in% names(at95))
  expect_identical(unique(at95$conf_level), 0.95)
  expect_identical(unique(at90$conf_level), 0.90)
  expect_true(all(at95$licensed))
  expect_true(all(is.finite(at90$lower) & is.finite(at90$upper)))
  # Narrower at 0.90, by the ratio of the normal quantiles, about the same
  # estimate and standard error.
  expect_equal(at90$estimate, at95$estimate, tolerance = 0)
  expect_equal((at90$upper - at90$lower) / (at95$upper - at95$lower),
    rep(stats::qnorm(.95) / stats::qnorm(.975), nrow(at95)),
    tolerance = 1e-12
  )
  # A level of 95 is refused, not turned into NaN limits on rows marked
  # licensed, and the argument is conf_level, not level.
  expect_error(sieve_invariance_interpret(fit, conf_level = 95), "received 95", fixed = TRUE)
  expect_error(sieve_invariance_interpret(fit, level = .90), "unused argument")
})

test_that("sieve_longitudinal_contrasts() and sieve_norm_transform_limits() take conf_level and return it as a column", {
  map <- data.frame(
    construct = "Democracy", wave = rep(c("1960", "1965"), each = 4L),
    item = rep(c("press", "opposition", "elections", "legislature"), 2L),
    variable = paste0("y", 1:8), wave_order = rep(1:2, each = 4L)
  )
  scalar <- sieve_run_longitudinal(lavaan::PoliticalDemocracy, map,
    level = "scalar",
    justification = "The same four indicators were rated at both waves"
  )
  at95 <- sieve_longitudinal_contrasts(scalar)
  at80 <- sieve_longitudinal_contrasts(scalar, conf_level = .80)
  expect_false("level" %in% names(at95))
  expect_identical(at80$conf_level, 0.80)
  z <- stats::qnorm(.90)
  expect_equal(c(at80$lower, at80$upper), c(at95$estimate - z * at95$se, at95$estimate + z * at95$se), tolerance = 1e-12)
  # The invariance rung passed as the confidence level is refused by name:
  # the confidence level is conf_level, and the level argument of
  # sieve_run_longitudinal() names the rung.
  expect_error(sieve_longitudinal_contrasts(scalar, conf_level = "scalar"), refusal("conf_level", "scalar"), fixed = TRUE)
  items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:3)]
  meta <- data.frame(
    item = names(items), label = names(items), construct = "Visual",
    facet = NA_character_, type = "continuous", status = "active",
    reverse = FALSE, stringsAsFactors = FALSE
  )
  reference <- sieve_norm_freeze(items, meta, name = "Calibration", population = "Grade 7 and 8 students")
  limits <- sieve_norm_transform_limits(reference, B = 199L, seed = 1, conf_level = .90)
  expect_false("level" %in% names(limits))
  expect_true(all(limits$conf_level == 0.90))
})

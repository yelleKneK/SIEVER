# The help of the advanced pages, ranked by consequence: the ordinal impact,
# the invariance contrasts and anchor sensitivity, the norm transformation
# limits, the longitudinal contrasts, the congeneric planner and the
# information bands each state the estimand, the method, the refusals,
# every returned column and their references, and no listed page keeps a
# description that repeats its title. The code the help describes is
# checked too: sieve_anchor_sensitivity() refuses a `missing` value it does
# not know (read as complete cases, "FIML" would discard every incomplete
# row without notice) and returns its row flow, and the planner's
# sensitivity rows say what population omega each N plans for.

ah_man <- function() {
  root <- file.path(testthat::test_path(), "..", "..", "man")
  if (dir.exists(root)) root else NULL
}

ah_text <- function(x) {
  flat <- function(e) if (is.list(e)) paste(vapply(e, flat, ""), collapse = "") else paste(as.character(e), collapse = "")
  gsub("[[:space:]]+", " ", trimws(flat(x)))
}

ah_rd <- function(page) {
  rd <- tools::parse_Rd(file.path(ah_man(), paste0(page, ".Rd")))
  tags <- vapply(rd, attr, "", "Rd_tag")
  list(rd = rd, tags = tags, section = function(tag) {
    at <- which(tags == tag)
    if (length(at)) ah_text(rd[[at[1]]]) else NA_character_
  })
}

test_that("the ranked advanced pages state the estimand, the refusals, every column and their references", {
  skip_if(is.null(ah_man()), "the Rd sources are not beside the tests")
  ranked <- c(
    "sieve_ordinal_fit_impact", "sieve_invariance_interpret", "sieve_anchor_sensitivity",
    "sieve_norm_transform_limits", "sieve_longitudinal_contrasts", "sieve_plan_congeneric",
    "sieve_irt_information_bands"
  )
  for (page in ranked) {
    x <- ah_rd(page)
    expect_false(identical(x$section("\\title"), x$section("\\description")), info = page)
    for (tag in c("\\details", "\\references", "\\seealso")) {
      expect_true(tag %in% x$tags, info = paste(page, tag))
    }
    value <- x$rd[[which(x$tags == "\\value")]]
    expect_true(any(grepl("\\describe", unlist(lapply(value, attr, "Rd_tag")), fixed = TRUE)), info = page)
    expect_match(x$section("\\details"), "Refusal|refused|unavailable|no interval|withheld", info = page)
  }
  # What each page must say, in its own words. The ordinal impact has
  # pointwise intervals from draws of the item parameters, and its page
  # states their method and their bias near 0.
  must <- list(
    sieve_ordinal_fit_impact = c("focal minus reference", "units of the category scores", "601-point grid",
      "pointwise percentiles (type 7) of the usable draws", "biased upward near 0",
      "at least two verified anchors"),
    sieve_invariance_interpret = c("first group of the fit", "Wald", "at least two verified anchors", "reason"),
    sieve_anchor_sensitivity = c("released", "complete cases even when", "row_flow", "FIML"),
    sieve_norm_transform_limits = c("50 + 10 * (x - m) / s", "type 7", "90 percent", "not the measurement error"),
    sieve_longitudinal_contrasts = c("first wave's latent standard deviation", "Wald", "configural or metric"),
    sieve_plan_congeneric = c("delta method", "not the interval the Reliability page reports", "population_omega", "three times"),
    sieve_irt_information_bands = c("item parameters only", "1 / sqrt(information)", "not the posterior standard deviation", "discarded")
  )
  for (page in names(must)) {
    x <- ah_rd(page)
    body <- paste(x$section("\\description"), x$section("\\details"), x$section("\\value"))
    for (phrase in must[[page]]) expect_match(body, phrase, fixed = TRUE, info = paste(page, phrase))
  }
})

test_that("no listed help page repeats its title as its description", {
  skip_if(is.null(ah_man()), "the Rd sources are not beside the tests")
  listed <- c(
    "print.siever_analysis", "sieve_anchor_sensitivity", "sieve_construct_norms", "sieve_facet_item_rest",
    "sieve_invariance_interpret", "sieve_irt_bivariate_table", "sieve_irt_category_probabilities",
    "sieve_irt_empirical_traces", "sieve_irt_information_bands", "sieve_irt_parameter_evidence",
    "sieve_irt_residual_pairs", "sieve_latent_bound", "sieve_longitudinal_compare", "sieve_longitudinal_contrasts",
    "sieve_longitudinal_spec", "sieve_norm_apply", "sieve_norm_freeze", "sieve_norm_transform_limits",
    "sieve_ordinal_fit_impact", "sieve_outcome_scale", "sieve_plan_congeneric", "sieve_reliability_construct",
    "sieve_replay_verify", "sieve_residual_pairs", "sieve_run_longitudinal", "sieve_score_policy_sensitivity",
    "sieve_scoring_code", "sieve_validity_evidence"
  )
  present <- listed[file.exists(file.path(ah_man(), paste0(listed, ".Rd")))]
  expect_gt(length(present), 20L)
  for (page in present) {
    x <- ah_rd(page)
    expect_false(identical(x$section("\\title"), x$section("\\description")), info = page)
  }
})

test_that("sieve_anchor_sensitivity() refuses a missing-data rule it does not know, before fitting anything", {
  meta <- data.frame(item = c("x4", "x5", "x6"), label = c("x4", "x5", "x6"), construct = "Textual",
    facet = NA_character_, type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE)
  hs <- lavaan::HolzingerSwineford1939[, c("x4", "x5", "x6", "school")]
  sets <- list(First = c("x4", "x5"), Last = c("x5", "x6"))
  for (bad in list("FIML", "ml", NA_character_, c("fiml", "listwise"))) {
    expect_error(
      sieve_anchor_sensitivity(hs, "school", meta, anchor_sets = sets, missing = bad, rationale = "Stated in advance"),
      "`missing` must be \"fiml\", \"listwise\" or \"complete\"", fixed = TRUE
    )
  }
})

test_that("sieve_anchor_sensitivity() returns the rows it used and why the others were not", {
  meta <- data.frame(item = c("x4", "x5", "x6"), label = c("x4", "x5", "x6"), construct = "Textual",
    facet = NA_character_, type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE)
  hs <- lavaan::HolzingerSwineford1939[, c("x4", "x5", "x6", "school")]
  hs$x4[1:10] <- NA
  sets <- list(First = c("x4", "x5"), Last = c("x5", "x6"))
  fiml <- suppressWarnings(sieve_anchor_sensitivity(hs, "school", meta, anchor_sets = sets,
    missing = "fiml", rationale = "Two anchor pairs stated in advance", discovery = FALSE))
  expect_identical(fiml$row_flow$input, 301L)
  expect_identical(fiml$row_flow$eligible, 301L)
  expect_identical(fiml$row_flow$partial_rows_retained, 10L)
  expect_match(fiml$row_flow$rule, "FIML", fixed = TRUE)
  complete <- suppressWarnings(sieve_anchor_sensitivity(hs, "school", meta, anchor_sets = sets,
    missing = "listwise", rationale = "Two anchor pairs stated in advance", discovery = FALSE))
  expect_identical(complete$row_flow$eligible, 291L)
  expect_identical(complete$row_flow$incomplete_items_removed, 10L)
  expect_match(complete$row_flow$rule, "Complete cases", fixed = TRUE)
})

test_that("each row of the congeneric plan names the population omega it plans for", {
  lambda <- c(.8, .7, .6, .7)
  psi <- c(.36, .51, .64, .51)
  plan <- sieve_plan_congeneric(lambda = lambda, psi = psi, width = .1, seed = 1)
  expect_identical(plan$sensitivity$loading_multiplier, c(.8, 1, 1.2))
  want <- vapply(c(.8, 1, 1.2), function(m) sum(lambda * m)^2 / (sum(lambda * m)^2 + sum(psi)), numeric(1))
  expect_equal(plan$sensitivity$population_omega, want, tolerance = 1e-12)
  # A higher omega needs fewer cases for the same width.
  expect_true(all(diff(plan$sensitivity$N) < 0))
})

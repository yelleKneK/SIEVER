# The Items tab's reliability and flags: a construct fit by FIML names the
# saturated FIML matrix as its denominator; a construct holding an item
# keyed opposite to the others withholds the item-rest and "rises if
# dropped" signals of all its items; the Expert Review Brief reads the
# grid's flags at the page's thresholds; and a planned-missing design gives
# each construct its reliability headless, with a note naming the FIML
# basis the factor analysis needs.

skip_if_not_installed("lavaan")

rf_hs <- function() lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]

rf_meta <- function(items, constructs) {
  data.frame(item = items, label = items, construct = constructs, facet = NA_character_,
    type = "continuous", status = "active", reverse = FALSE, stringsAsFactors = FALSE)
}

test_that("a construct fit by FIML names the saturated FIML matrix as its denominator", {
  d <- rf_hs()
  withr::with_seed(2, d$x7[sample(nrow(d), 80)] <- NA)
  meta <- rf_meta(names(d), rep(c("Visual", "Textual", "Speed"), each = 3L))
  diag <- sieve_item_diagnostics(d, meta, rng = sieve_draw_seed(1))
  cons <- diag$constructs
  expect_identical(cons["Speed", "missing_treatment"], "fiml")
  expect_gt(cons["Speed", "n_reliability"], cons["Speed", "n_complete"])
  expect_identical(cons["Speed", "omega_matrix"], "the saturated FIML covariance matrix of the items")
  # Complete constructs: the covariances of the item scores, which are the
  # FIML estimates when every item is observed.
  expect_identical(cons["Visual", "omega_matrix"], "Pearson covariances of the item scores")
  expect_identical(
    sieve_item_omega_matrices_text(cons),
    "Pearson covariances of the item scores for Visual and Textual; the saturated FIML covariance matrix of the items for Speed"
  )
  expect_identical(sieve_item_omega_matrices_text(cons[c("Visual", "Textual"), ]), "Pearson covariances of the item scores")
  expect_identical(
    sieve_item_omega_matrix_text(FALSE, engine_missing = "fiml", aux = "ageyr"),
    "the saturated FIML covariance matrix of the items (with ageyr as auxiliary)"
  )
  expect_identical(sieve_item_omega_matrix_text(TRUE, engine_missing = "listwise"), "unrestricted polychoric correlations")
})

test_that("in a construct holding an opposite-signed item no item's item-rest or rises-if-dropped signal counts", {
  d <- rf_hs()
  d$x6 <- -d$x6
  meta <- rf_meta(names(d), rep(c("VT", "Speed"), c(6L, 3L)))
  diag <- sieve_item_diagnostics(d, meta, rng = sieve_draw_seed(1))
  it <- diag$items
  vt <- it$construct == "VT"
  expect_identical(it$item[it$flag_keying], "x6")
  # Each item of VT reads a total in which x6 subtracts; the correctly
  # keyed items trip the item-rest or rises-if-dropped cutoffs because of it.
  expect_true(any((it$flag_item_total | it$flag_omega_drop)[vt & it$item != "x6"]))
  expect_identical(it$keying_withheld, vt)
  expect_identical(
    it$removal_signals[vt],
    as.integer((it$flag_loading + it$flag_communality + it$flag_redundancy)[vt])
  )
  expect_identical(
    it$removal_signals[!vt],
    as.integer((it$flag_loading + it$flag_communality + it$flag_item_total + it$flag_omega_drop +
      it$flag_redundancy)[!vt])
  )
  # The grid's flags (and the brief's) say the same at the page's thresholds.
  live <- sieve_item_live_flags(it, sieve_default_thresholds(), meta = meta)
  expect_false(any(live$itc[vt] | live$relup[vt]))
  expect_identical(live$withheld, vt)
  expect_identical(live$keying, it$flag_keying)
  expect_match(diag$constructs["VT", "keying_note"],
    "the item-rest correlations and the changes if dropped of all the items of VT read that total", fixed = TRUE)
})

test_that("the review brief judges salience by the size of the loading, at the page's thresholds", {
  d <- rf_hs()
  d$x6 <- -d$x6
  meta <- rf_meta(names(d), rep(c("Visual", "Textual", "Speed"), each = 3L))
  diag <- sieve_item_diagnostics(d, meta, rng = sieve_draw_seed(1))
  x6 <- diag$items[diag$items$item == "x6", ]
  expect_lt(x6$loading_target, -0.40)
  lines <- strsplit(sieve_construct_brief(meta, "Textual", diag = diag$items), "\n", fixed = TRUE)[[1]]
  x6_line <- grep("(x6)", lines, fixed = TRUE, value = TRUE)
  expect_match(x6_line, "opposite in sign to the other items: check its keying", fixed = TRUE)
  expect_false(grepl("weak loading", x6_line, fixed = TRUE))
  # Every item-rest correlation of Textual reads a total in which x6
  # subtracts, so none is tagged low.
  expect_false(any(grepl("low item-total", lines, fixed = TRUE)))
  # The page's thresholds, not fixed numbers: at a low-loading cutoff of
  # .95 every Visual item is weak, as the grid says.
  th <- sieve_default_thresholds()
  th$load_low <- 0.95
  visual <- strsplit(sieve_construct_brief(meta, "Visual", diag = diag$items, thresholds = th), "\n", fixed = TRUE)[[1]]
  expect_identical(sum(grepl("weak loading", visual, fixed = TRUE)), 3L)
  every <- strsplit(sieve_instrument_brief(meta, diag = diag$items, thresholds = th), "\n", fixed = TRUE)[[1]]
  expect_identical(sum(grepl("weak loading", every, fixed = TRUE)), sum(abs(diag$items$loading_target) < 0.95))
  desc <- sieve_construct_brief(meta, "Textual", mode = "description", diag = diag$items)
  expect_match(desc, "Items flagged by the live diagnostics for review: x6.", fixed = TRUE)
})

test_that("a planned-missing design gives each construct its reliability headless and names the FIML basis", {
  P <- rf_hs()
  form <- withr::with_seed(1, sample(1:3, nrow(P), replace = TRUE))
  P[form == 1, c("x4", "x5", "x6")] <- NA
  P[form == 2, c("x7", "x8", "x9")] <- NA
  P[form == 3, c("x1", "x2", "x3")] <- NA
  meta <- rf_meta(names(P), rep(c("Visual", "Textual", "Speed"), each = 3L))
  r <- sieve_item_diagnostics(P, meta, rng = sieve_draw_seed(1))
  expect_true(r$reliability_only)
  expect_identical(r$n_obs, 0L)
  expect_identical(r$status$code, "not_fit")
  expect_true(all(is.finite(r$constructs$omega)))
  expect_true(all(is.finite(r$constructs$alpha)))
  expect_true(all(r$constructs$missing_treatment == "fiml"))
  # Each form observes two constructs completely: each construct's own rows
  # are its complete rows, though no row is complete on all nine items.
  expect_identical(r$constructs$n_reliability, r$constructs$n_complete)
  expect_true(all(r$constructs$n_reliability > 150L))
  expect_true(all(is.na(r$items$loading_target)))
  expect_true(all(is.na(r$items$redundancy_max)))
  expect_match(paste(r$notes, collapse = " "), "sieve_retention_cor(data, missing = \"fiml\")", fixed = TRUE)
  expect_match(paste(r$notes, collapse = " "), "Each construct's reliability reads its own rows and is given",
    fixed = TRUE)
  # Each construct's omega is DMAR's FIML robust omega of its own rows.
  for (cn in c("Visual", "Textual", "Speed")) {
    its <- meta$item[meta$construct == cn]
    x <- P[rowSums(!is.na(P[, its])) > 0L, its]
    om <- DMAR::reliability_omega(data = x, missing = "fiml", ci_method = "none", denominator = "observed")
    expect_equal(r$constructs[cn, "omega"], om$value[om$term == "estimate"], tolerance = 1e-10)
  }
  # The recorded R records no factor analysis that could not run.
  code <- sieve_item_diagnostics_code(r, NULL, meta$construct, meta$item)
  expect_false(any(grepl("fa_exploratory(", code, fixed = TRUE)))
  expect_true(any(grepl("was not fit", code, fixed = TRUE)))
  # With the saturated FIML matrix as the basis the factor analysis is
  # attempted (DMAR judges its solution), and the reliability is the same.
  b <- sieve_retention_cor(P, missing = "fiml")
  fitted <- sieve_item_diagnostics(P, meta, basis = b, rng = sieve_draw_seed(1))
  expect_false(isTRUE(fitted$reliability_only))
  expect_false(is.null(fitted$efa_call))
  expect_false(identical(fitted$status$code, "not_fit"))
  expect_equal(fitted$constructs$omega, r$constructs$omega, tolerance = 1e-12)
})

# The Reliability tab: the interval of model-implied omega is labeled by
# its standard error, "MLR (sandwich) SE, logit-scale limits", so "robust"
# means one thing in a table; the help states what DMAR's study of those
# limits found and that it did not cover SIEVER's default interval; and the
# compact table shows model-implied omega with its interval, so every
# construct of continuous items has an interval in view before any bootstrap
# is run.

skip_if_not_installed("lavaan")

rd_results <- function() {
  d <- lavaan::HolzingerSwineford1939
  blocks <- list(Visual = paste0("x", 1:3), Textual = paste0("x", 4:6), Speed = paste0("x", 7:9))
  lapply(names(blocks), function(cn) {
    spec <- sieve_reliability_spec(blocks[[cn]], rep("continuous", 3), construct = cn, run_bootstrap = FALSE)
    sieve_reliability_construct(d, spec, alpha = FALSE)
  })
}

test_that("the model-implied interval is named by its standard error, not as robust", {
  expect_identical(sieve_reliability_interval("fiml", "mlr_logistic")$label,
    "MLR (sandwich) SE, logit-scale limits (delta method)")
  expect_identical(sieve_reliability_interval("listwise", "mlr")$label, "MLR (sandwich) SE (delta method)")
  expect_identical(sieve_reliability_H_label("mlr_logistic"),
    "MLR (sandwich) SE, logit-scale limits (delta method, DMAR)")
  menu <- names(sieve_reliability_methods())
  expect_false(any(grepl("robust ML", menu, ignore.case = TRUE)))
  expect_match(menu[1], "MLR (sandwich) SE with logit-scale limits for model-implied", fixed = TRUE)
  res <- rd_results()
  tab <- sieve_reliability_table(res)
  expect_identical(unique(tab$model_interval), "MLR (sandwich) SE, logit-scale limits (delta method)")
  # In the table and the method note "robust" is said only of robust omega.
  text <- paste(vapply(res, sieve_reliability_paragraph, character(1)), collapse = " ")
  expect_false(grepl("Robust ML", text, fixed = TRUE))
  expect_false(any(grepl("Robust ML", unlist(tab[vapply(tab, is.character, logical(1))]), fixed = TRUE)))
})

test_that("the help says what DMAR's study of the logit-scale limits found and what it did not cover", {
  study <- sieve_reliability_limits_study_text()
  expect_match(study, "5,000 replications in each of 40 cells", fixed = TRUE)
  expect_match(study, "covered between .939 and .956 in every one of the 48", fixed = TRUE)
  expect_match(study, "covered between .919 and .951", fixed = TRUE)
  expect_match(study, ".936 at N = 200 and .942 at N = 500", fixed = TRUE)
  expect_match(study, "did not include SIEVER's default interval, the percentile bootstrap of Robust omega", fixed = TRUE)
  expect_false(grepl("—", study, fixed = TRUE))
  # The normal-data range is DMAR's own statement of its study.
  rd <- tools::Rd_db("DMAR")[["reliability_omega.Rd"]]
  dmar_help <- gsub("\\s+", " ", paste(utils::capture.output(tools::Rd2txt(rd, options = list(underline_titles = FALSE))), collapse = " "))
  expect_match(dmar_help, "the logit limits covered from .939 to .956 in every cell", fixed = TRUE)
  # The Reliability tab's help on the interval methods carries it.
  ui <- gsub("\\s+", " ", as.character(mod_reliab_ui("reliab")))
  expect_match(ui, "covered between .939 and .956", fixed = TRUE)
  expect_false(grepl("chosen on a Monte Carlo study in which these limits covered closer", ui, fixed = TRUE))
})

test_that("the compact table shows model-implied omega with its interval in every row of continuous constructs before any bootstrap", {
  res <- rd_results()
  tab <- sieve_reliability_table(res)
  expect_true(all(tab$uncertainty == "pending"))
  shown <- sieve_reliability_display(tab, res)
  expect_true(all(c("model_omega", "model_ci") %in% names(shown$data)))
  expect_true(all(grepl("^\\[0\\.[0-9]{3}, 0\\.[0-9]{3}\\]$", shown$data$model_ci)))
  expect_equal(shown$data$model_omega, tab$model_omega)
  expect_true("Omega (model-implied denominator)" %in% shown$colnames)
  expect_identical(length(shown$colnames), ncol(shown$data))
  # Show all columns adds the method once, not a second copy of the estimate.
  all_cols <- sieve_reliability_display(tab, res, show_all = TRUE)
  expect_identical(sum(names(all_cols$data) == "model_omega"), 1L)
  expect_true("model_interval" %in% names(all_cols$data))
  expect_identical(length(all_cols$colnames), ncol(all_cols$data))
})

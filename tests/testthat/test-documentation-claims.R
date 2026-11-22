# test-documentation-claims.R
#
# What NEWS and the articles say SIEVER does, held to what it does: NEWS
# holds one heading, the release's, and no sections; the default
# interval levels and the disclosure label are as the pages give them; the
# user guide and the validation article report Harman's single-factor screen
# without reading it against .50, as the Validity page does; and the user
# guide gives the WLSMV memory guard as the code applies it (an estimated
# peak against sieve.wlsmv_limit), not as one weight matrix against the
# machine's memory.
#
# Author: Ken Kelley

doc_flat <- function(text) gsub("[[:space:]]+", " ", paste(text, collapse = " "))

doc_read <- function(root, ...) readLines(file.path(root, ...), warn = FALSE, encoding = "UTF-8")

test_that("NEWS holds one heading, the release's, and no sections", {
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  news <- doc_read(root, "NEWS.md")
  version <- as.character(utils::packageVersion("SIEVER"))
  expect_identical(news[1L], paste("# SIEVER", version))
  expect_length(grep("^# ", news), 1L)
  expect_length(grep("^##", news), 0L)
})

test_that("intervals default to 90 percent for the RMSEA and the unbiased SRMR, and 95 percent otherwise", {
  withr::local_options(sieve.rmsea_conf_level = NULL, sieve.conf_level = NULL)
  expect_identical(sieve_default_rmsea_conf_level(), 0.90)
  expect_identical(sieve_default_conf_level(), 0.95)
  expect_identical(formals(sieve_fit_usrmr)$conf_level, quote(sieve_default_rmsea_conf_level()))
})

test_that("the disclosure label reads the same on every page that shows it", {
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  label <- "Method and data behind this result"
  for (file in c("fct_page_layout.R", "mod_cfa.R", "mod_reliab.R")) {
    src <- doc_flat(doc_read(root, "R", file))
    expect_match(src, sprintf("tags$summary(\"%s\")", label), fixed = TRUE, info = file)
  }
})

test_that("the user guide and the validation article report Harman's screen without a cutoff, as the Validity page does", {
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  guide <- doc_flat(doc_read(root, "vignettes", "user-guide.Rmd"))
  validation <- doc_flat(doc_read(root, "vignettes", "validation.Rmd"))
  for (text in list(guide, validation)) {
    expect_false(grepl("read against the .50", text, fixed = TRUE))
  }
  expect_match(guide, paste(
    "the share of the total item variance that one factor accounts for. Podsakoff, MacKenzie, Lee and",
    "Podsakoff (2003) describe the convention of taking a share above .50 as a sign of method variance;",
    "SIEVER applies no cutoff to the value."
  ), fixed = TRUE)
  expect_match(validation, paste(
    "names it Harman's single-factor screen in its table row, note, and button, and reports the proportion",
    "without a cutoff; the note names the .50 convention of Podsakoff et al. (2003) once and says that",
    "SIEVER applies no cutoff to it."
  ), fixed = TRUE)
  # The page's note is the one the articles describe.
  module <- doc_flat(doc_read(root, "R", "mod_validity.R"))
  expect_match(module, "SIEVER applies no cutoff to the value.", fixed = TRUE)
})

test_that("the user guide gives the WLSMV memory guard as the code applies it: an estimated peak against sieve.wlsmv_limit", {
  # The guard: about twenty working copies of the weight matrix, the
  # casewise scores, which grow with n, and 0.4 GB, against the option
  # sieve.wlsmv_limit or, unset, sieve.memory_limit (three quarters of
  # physical memory).
  s <- sieve_wlsmv_statistics(10L, 5L)
  expect_equal(sieve_wlsmv_peak_bytes(10L, 5L) - 4e8, 20 * 8 * s^2)
  expect_gt(sieve_wlsmv_peak_bytes(10L, 5L, n = 1000), sieve_wlsmv_peak_bytes(10L, 5L, n = 100))
  withr::with_options(list(sieve.wlsmv_limit = NULL, sieve.memory_limit = 5e9), {
    expect_identical(sieve_wlsmv_limit(), 5e9)
  })
  withr::with_options(list(sieve.wlsmv_limit = 3e9), expect_identical(sieve_wlsmv_limit(), 3e9))
  root <- compat_source_root()
  skip_if(is.null(root), "the source tree is not beside the tests")
  guide <- doc_flat(doc_read(root, "vignettes", "user-guide.Rmd"))
  expect_false(grepl("weight matrix exceeds the machine's memory", guide, fixed = TRUE))
  expect_false(grepl("exceeds the machine,", guide, fixed = TRUE))
  expect_match(guide, paste(
    "whose estimated peak memory (about twenty working copies of its weight matrix, plus casewise scores",
    "that grow with the number of respondents) exceeds `sieve.wlsmv_limit`, by default three quarters of",
    "physical memory, is refused before it starts"
  ), fixed = TRUE)
  guarded <- gregexpr("a WLSMV fit whose estimated peak memory exceeds `sieve.wlsmv_limit` (see Options You Can Set) is refused before it starts",
    guide, fixed = TRUE)[[1L]]
  expect_length(guarded, 2L)
  expect_true(all(guarded > 0L))
  expect_match(guide, "### Options You Can Set", fixed = TRUE)
  expect_match(guide, "or whose estimated memory need exceeds `sieve.memory_limit` (three quarters of physical memory by default)",
    fixed = TRUE)
})

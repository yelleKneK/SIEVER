# sieve_analyze() returns a DMAR refusal from irt, htmt, ave, and content as
# a failed analysis with DMAR's reason, as it returns the exploratory
# refusal of an item lavaan cannot fit, rather than stopping the script
# with DMAR's error.

test_that("a DMAR refusal from irt, htmt, ave, or content is a failed analysis with DMAR's reason", {
  items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  fit <- lavaan::cfa("V =~ x1 + x2 + x3", items, std.lv = TRUE)
  cases <- list(
    irt = list(args = list(data = items[, 1:4]), engine = "irt_grm", text = "contains non-integer values"),
    htmt = list(
      args = list(data = items, blocks = list(V = paste0("x", 1:3))), engine = "htmt",
      text = "'blocks' must be a named list of two or more constructs."
    ),
    ave = list(
      args = list(fit = fit, ci_method = "percentile", B = 10L, seed = 1), engine = "average_variance_extracted",
      text = "'B' must be a single whole number of at least 400."
    ),
    content = list(args = list(ratings = matrix(c(1, 2, 5, 7), 2L)), engine = "content_validity_index", text = "4 point relevance scale")
  )
  for (analysis in names(cases)) {
    case <- cases[[analysis]]
    # The engine's own refusal, signaled directly.
    direct <- tryCatch(do.call(getExportedValue("DMAR", case$engine), case$args), error = function(e) e)
    expect_s3_class(direct, "dmar_refusal")
    # Through the dispatcher: a failed analysis, not an error.
    a <- expect_no_error(do.call(sieve_analyze, c(list(analysis), case$args)))
    expect_s3_class(a, "siever_analysis")
    expect_null(a$result)
    expect_identical(a$status$code, "failed")
    expect_identical(a$status$message, trimws(gsub("\\s+", " ", conditionMessage(direct))))
    expect_match(a$status$message, case$text, fixed = TRUE)
    expect_false(grepl("\n", a$status$message, fixed = TRUE))
    expect_identical(a$diagnostics$refusal$class, class(direct))
    expect_identical(a$diagnostics$refusal$message, conditionMessage(direct))
    expect_identical(a$diagnostics$components$component, paste0("DMAR::", case$engine, "()"))
    expect_identical(a$diagnostics$components$code, "failed")
    # The generics say why there is nothing to read.
    printed <- utils::capture.output(print(a))
    expect_identical(printed[2L], "Status: failed ")
    expect_error(as.data.frame(a), "This analysis has no estimates.", fixed = TRUE)
    expect_error(stats::confint(a), "This analysis has no estimates.", fixed = TRUE)
  }
})

test_that("an error that is not a DMAR refusal is still signaled", {
  # A caller's argument the engine does not take is a programming error,
  # not a refusal of the data, and stops with R's unused argument error.
  expect_error(sieve_analyze("content", ratings = matrix(1:4, 2L), not_an_argument = 1), "unused argument")
})

test_that("an accepted DMAR request is unchanged by the refusal handler", {
  ratings <- withr::with_seed(1, matrix(sample(1:4, 40, TRUE), 10, 4))
  a <- sieve_analyze("content", ratings = ratings)
  expect_identical(a$status$code, "ok")
  expect_null(a$diagnostics$refusal)
  expect_identical(a$result, DMAR::content_validity_index(ratings = ratings))
})

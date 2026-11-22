test_that("ordered MI sensitivity runs the pilot, plans M and pools actual fits", {
  skip_on_cran()
  skip_if_not_installed("mice")
  # Not skip_if_not_installed(), which loads lavaan.mi's namespace while
  # lavaan is not attached (see test-lavaan-mi-namespace.R).
  skip_if_not(sieve_installed("lavaan.mi"), "lavaan.mi is not installed")
  raw <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  x <- as.data.frame(lapply(raw, function(v) as.integer(cut(v,
    breaks = unique(stats::quantile(v, probs = seq(0, 1, .2))), include.lowest = TRUE))))
  x[1:60, 1] <- NA_integer_
  withr::local_seed(81L)
  before <- .Random.seed
  search_before <- search()
  actual <- sieve_mi_model("Textual =~ x4 + x5 + x6", x, ordered = names(x), seed = 13L)
  expect_identical(.Random.seed, before)
  # lavaan and lavaan.mi are attached for the call only and not left
  # attached.
  expect_identical(search(), search_before)
  expect_identical(actual$plan$pilot, 20L)
  expect_identical(actual$imputations, actual$plan$imputations)
  expect_gte(actual$imputations, 20L)
  expect_equal(actual$used + actual$omitted, actual$imputations)
  expect_gt(actual$used, 0L)
  free <- is.finite(actual$estimates$se) & actual$estimates$se > 0
  expect_true(any(free))
  expect_true(all(is.finite(actual$estimates$ci.lower[free])))
  expect_true(all(is.finite(actual$estimates$ci.upper[free])))
  expect_true(all(actual$plan$table$upper_limit >= actual$plan$table$fmi))
})

test_that("an infinite response is refused before imputation, with its column and rows, and NaN is read as missing", {
  # sieve_as_number() keeps Inf, so a check that read the columns with it
  # alone would make an Inf in an ordered indicator its highest category
  # ("levels of x5: 1 2 3 4 5 Inf") and a scripted sieve_mi_model() would
  # impute and fit it, where every other route that fits a model refuses it
  # with the item and the rows named.
  test_frame <- environment()
  local_mocked_bindings(sieve_installed = mi_suggests_installed(), .env = test_frame)
  raw <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
  x <- as.data.frame(lapply(raw, function(v) as.integer(cut(v, 5))))
  x[1:10, 1] <- NA_integer_
  model <- "Textual =~ x4 + x5 + x6"
  bad <- x
  bad$x5[20] <- Inf
  expect_error(sieve_mi_check(model, bad, ordered = names(x)),
    "Item 'x5' holds 1 infinite value (Inf in row 20), so the model cannot be fit", fixed = TRUE)
  # The exported function refuses it before it attaches or imputes anything.
  search_before <- search()
  expect_error(sieve_mi_model(model, bad, ordered = names(x), seed = 1L), "Inf in row 20", fixed = TRUE)
  expect_identical(search(), search_before)
  # An auxiliary column is refused by name, with its own action.
  aux <- x
  aux$age <- lavaan::HolzingerSwineford1939$ageyr
  aux$age[c(7L, 9L)] <- c(-Inf, Inf)
  expect_error(sieve_mi_check(model, aux, ordered = names(x), aux = "age"), paste0(
    "Auxiliary column 'age' holds 2 infinite values (-Inf in row 7; Inf in row 9), so the model cannot be fit: ",
    "a response is a finite number or missing (NA)."), fixed = TRUE)
  expect_error(sieve_mi_check(model, aux, ordered = names(x), aux = "age"),
    "or remove it from the auxiliary variables.", fixed = TRUE)
  # NaN is a missing value: it is not a category, and an auxiliary keeps NA.
  # A factor auxiliary still enters by its codes.
  nan <- x
  nan$x5[30] <- NaN
  nan$age <- lavaan::HolzingerSwineford1939$ageyr
  nan$age[5] <- NaN
  nan$school <- lavaan::HolzingerSwineford1939$school
  prepared <- sieve_mi_check(model, nan, ordered = names(x), aux = c("age", "school"))
  expect_true(is.na(prepared$X$x5[30]))
  expect_identical(levels(prepared$X$x5), as.character(sort(unique(x$x5))))
  expect_true(is.na(prepared$X$age[5]) && !is.nan(prepared$X$age[5]))
  expect_false(any(is.nan(prepared$X$age)))
  expect_identical(prepared$X$school, as.numeric(as.integer(nan$school)))
})

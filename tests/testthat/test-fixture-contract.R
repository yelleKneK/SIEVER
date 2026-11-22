# The dependency contract behind the bundled examples. DMAR ships the
# Holzinger and Swineford (1939) data under the single documented name
# `holzinger_swineford`, with no short alias. These tests
# pin what SIEVER relies on, so a change upstream is reported here rather
# than as ninety unrelated failures.

test_that("the supported DMAR version supplies the data under its documented name", {
  expect_true(utils::packageVersion("DMAR") >= "1.1.0")
  expect_true("holzinger_swineford" %in% getNamespaceExports("DMAR") ||
    is.data.frame(getExportedValue("DMAR", "holzinger_swineford")))
  d <- sieve_hs_data()
  expect_s3_class(d, "data.frame")
  expect_identical(nrow(d), 301L)
  expect_true(all(c("id", "sex", "grade", "age_years", "school") %in% names(d)))
  tests24 <- names(sieve_hs_test_labels())[1:24]
  expect_true(all(tests24 %in% names(d)))
  expect_false(anyNA(d[tests24]))
})

test_that("school and sex carry the documented coding and counts", {
  d <- sieve_hs_data()
  # Holzinger and Swineford (1939): 156 Pasteur and 145 Grant-White pupils.
  expect_identical(
    as.integer(table(as.character(d$school))[c("Pasteur", "Grant-White")]),
    c(156L, 145L)
  )
  # Row order is by school, Pasteur first, which is the order lavaan assigns
  # to the groups of a multiple group model and that the invariance panels
  # display.
  expect_identical(unique(as.character(d$school)), c("Pasteur", "Grant-White"))
  expect_setequal(unique(as.character(d$sex)), c("Female", "Male"))
  expect_identical(
    as.integer(table(as.character(d$sex))[c("Female", "Male")]),
    c(155L, 146L)
  )
})

test_that("the first pupils have the published raw scores", {
  d <- sieve_hs_data()
  # Raw scores for ids 1 to 3 on the primary nine tests, read from the
  # corrected Holzinger and Swineford data (identical in MBESS::HS and
  # psychTools::holzinger.raw). They are exact integers, so identity is the
  # right comparison.
  nine <- sieve_hs9_fixture()$item_cols
  expect_identical(d$id[1:3], 1:3)
  expect_identical(unname(unlist(d[1, nine])), c(20L, 31L, 3L, 7L, 23L, 9L, 78L, 115L, 229L))
  expect_identical(unname(unlist(d[2, nine])), c(32L, 21L, 17L, 5L, 12L, 9L, 87L, 125L, 285L))
  expect_identical(unname(unlist(d[3, nine])), c(27L, 21L, 15L, 3L, 7L, 3L, 75L, 78L, 159L))
})

test_that("the primary nine are lavaan's x1 to x9 up to the documented divisors", {
  skip_if_not_installed("lavaan")
  d <- sieve_hs_data()
  L <- lavaan::HolzingerSwineford1939
  expect_identical(d$id, L$id)
  nine <- sieve_hs9_fixture()$item_cols
  # lavaan's help file: each x is the raw test score divided by a constant.
  divisor <- c(6, 4, 8, 3, 4, 7, 23, 20, 36)
  # lavaan stores the quotients to eight significant digits. Every quotient
  # is below 10, so the largest possible discrepancy is the rounding
  # half-width, 5e-8; the DMAR columns are the exact integers.
  for (j in seq_along(nine)) {
    gap <- max(abs(d[[nine[j]]] / divisor[j] - L[[paste0("x", j)]]))
    expect_lte(gap, 5e-8, label = sprintf("largest gap for %s", nine[j]))
    expect_equal(signif(d[[nine[j]]] / divisor[j], 8), L[[paste0("x", j)]],
      tolerance = 1e-12, info = nine[j]
    )
  }
  # Same pupils in the same schools, so a group analysis addresses the same
  # comparison.
  expect_identical(as.character(d$school), as.character(L$school))
})

test_that("both bundled examples are built from the validated data", {
  d <- sieve_hs_data()
  f9 <- sieve_hs9_fixture()
  f24 <- sieve_hs_fixture()
  expect_identical(f9$id_vars, c("id", "sex", "grade", "age_years", "school"))
  expect_identical(f24$id_vars, f9$id_vars)
  expect_length(f9$item_cols, 9L)
  expect_length(f24$item_cols, 24L)
  expect_identical(f24$item_cols, names(sieve_hs_test_labels())[1:24])
  expect_identical(f9$data[, f9$item_cols], d[, f9$item_cols])
  expect_identical(f24$data[, f24$item_cols], d[, f24$item_cols])
  expect_identical(unname(lengths(f9$theory_map)), c(3L, 3L, 3L))
  expect_identical(unname(unlist(f9$theory_map)), f9$item_cols)
  expect_identical(sort(unname(unlist(f24$theory_map))), sort(f24$item_cols))
  expect_identical(
    names(f24$theory_map),
    c("Spatial", "Verbal", "Speed", "Memory", "Reasoning")
  )
})

test_that("a changed upstream data object is reported, not passed along", {
  d <- sieve_hs_data()
  expect_identical(sieve_hs_validate(d), d)
  expect_error(
    sieve_hs_validate(NULL, version = "9.9.9"),
    "`holzinger_swineford` was not found"
  )
  expect_error(sieve_hs_validate(NULL, version = "9.9.9"), "version 9.9.9")
  expect_error(
    sieve_hs_validate(d[, setdiff(names(d), "t2_cubes")]),
    "missing column\\(s\\) t2_cubes"
  )
  expect_error(sieve_hs_validate(d[1:300, ]), "300 rows were found")
  chr <- d
  chr$t1_visual_perception <- as.character(chr$t1_visual_perception)
  expect_error(sieve_hs_validate(chr), "non-numeric test column\\(s\\) t1_visual_perception")
  na <- d
  na$t24_woody_mccall[5] <- NA
  expect_error(sieve_hs_validate(na), "missing values in the 24 complete tests")
  sch <- d
  sch$school <- factor(ifelse(sch$school == "Pasteur", "A", "B"))
  expect_error(sieve_hs_validate(sch), "does not name exactly the Pasteur and Grant-White")
})

test_that("no source file names HS_Data", {
  root <- testthat::test_path("..", "..")
  skip_if_not(dir.exists(file.path(root, "R")))
  files <- c(
    list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
    list.files(file.path(root, "vignettes"), pattern = "[.]Rmd$", full.names = TRUE)
  )
  hits <- vapply(
    files, function(f) any(grepl("HS_Data", readLines(f, warn = FALSE), fixed = TRUE)),
    logical(1)
  )
  expect_identical(unname(files[hits]), character(0))
})

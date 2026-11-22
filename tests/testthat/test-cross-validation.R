cv_fixture <- function() {
  data <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
  meta <- sieve_build_meta(data, names(data), list(
    Visual = paste0("x", 1:3), Textual = paste0("x", 4:6), Speed = paste0("x", 7:9)
  ))
  # Eligible-row references must survive gaps and incomplete parked items.
  data$x1[c(2L, 17L, 283L)] <- NA_real_
  data$parked <- NA_real_
  meta <- rbind(meta, transform(meta[1L, , drop = FALSE], item = "parked", status = "parked"))
  list(data = data, meta = meta)
}

test_that("the public CV split preserves rows and matches an independent covariance discrepancy", {
  x <- cv_fixture()
  set.seed(619)
  before <- .Random.seed
  result <- sieve_cross_validate(x$data, x$meta, holdout = .4, seed = 113, estimator = "mlr")
  expect_identical(.Random.seed, before)
  expect_null(result$error)
  eligible <- which(stats::complete.cases(x$data[, paste0("x", 1:9)]))
  expect_identical(result$eligible_rows, eligible)
  expect_length(intersect(result$calibration_rows, result$validation_rows), 0L)
  expect_identical(sort(c(result$calibration_rows, result$validation_rows)), eligible)
  expect_identical(result$n_val, length(result$validation_rows))
  expect_identical(result$n_cal, length(result$calibration_rows))
  expected <- withr::with_seed(113, eligible[sort(sample.int(length(eligible), round(.4 * length(eligible))))])
  expect_identical(result$validation_rows, expected)
  model <- paste("Visual =~ x1+x2+x3", "Textual =~ x4+x5+x6", "Speed =~ x7+x8+x9", sep = "\n")
  cal <- lavaan::cfa(model,
    data = x$data[result$calibration_rows, ],
    std.lv = TRUE, estimator = "MLR", missing = "fiml"
  )
  val <- lavaan::cfa(model,
    data = x$data[result$validation_rows, ],
    std.lv = TRUE, estimator = "MLR", missing = "fiml"
  )
  sigma <- lavaan::lavInspect(cal, "implied")$cov
  # The maximum likelihood covariance (divisor n), the scale of the loose
  # discrepancy.
  S <- stats::cov(x$data[result$validation_rows, rownames(sigma)])
  S <- S * (nrow(x$data[result$validation_rows, ]) - 1) / nrow(x$data[result$validation_rows, ])
  # An independent eigenvalue implementation of log|Sigma| + tr(S Sigma^-1)
  # - log|S| - p, without calling the service's determinant/trace helper.
  whiten <- chol(sigma)
  root_inverse <- backsolve(whiten, diag(ncol(sigma)))
  relative <- t(root_inverse) %*% S %*% root_inverse
  eigenvalues <- eigen(relative, symmetric = TRUE, only.values = TRUE)$values
  tight <- sum(eigenvalues - log(eigenvalues) - 1)
  loose <- 2 * unname(lavaan::fitMeasures(val, "fmin"))
  expect_equal(result$xval$Discrepancy, c(tight, loose, tight - loose), tolerance = 1e-10)
  expect_true(all(is.finite(result$xval$Discrepancy)))
  if (!is.na(result$family)) {
    suffix <- if (result$family == "standard") "" else paste0(".", result$family)
    indices <- paste0(c("cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"), suffix)
    for (i in seq_len(2L)) {
      fit <- if (i == 1L) cal else val
      # (The RMSEA interval is at the RMSEA level, 90%,
      # and the ECVI interval at the table's 95%; lavaan is asked for the
      # RMSEA level.)
      expect_identical(result$conf_level, 0.95)
      expect_identical(result$rmsea_conf_level, 0.90)
      expect_equal(unname(as.numeric(result$table[i, c("CFI", "RMSEA", "RMSEA.lo", "RMSEA.hi")])),
        unname(lavaan::fitMeasures(fit, indices, fm.args = list(rmsea.ci.level = result$rmsea_conf_level))),
        tolerance = 1e-10
      )
    }
  }
  dispatched <- sieve_analyze("cross_validation",
    data = x$data, meta = x$meta,
    holdout = .4, seed = 113, estimator = "mlr"
  )
  expect_equal(dispatched$result, result, tolerance = 1e-12)
  expect_identical(dispatched$service, "SIEVER::sieve_cross_validate")
  # The table as the service returned it, with the envelope's schema
  # version as an attribute (the result envelope).
  xval <- as.data.frame(dispatched, table = "xval")
  expect_identical(attr(xval, "schema_version"), 1L)
  attr(xval, "schema_version") <- NULL
  expect_equal(xval, result$xval)
})

test_that("CV keeps the local seed contract and refuses dependent designs before drawing", {
  x <- cv_fixture()
  set.seed(771)
  actual_seed <- sample.int(.Machine$integer.max, 1L)
  expected_stream <- .Random.seed
  set.seed(771)
  result <- sieve_cross_validate(x$data, x$meta, seed = NULL, estimator = "mlr")
  expect_identical(.Random.seed, expected_stream)
  expect_identical(result$rng$seed, actual_seed)
  expect_false(result$rng$supplied)
  expect_identical(result$rng$kind, RNGkind())
  repeated <- sieve_cross_validate(x$data, x$meta, seed = actual_seed, estimator = "mlr")
  expect_identical(repeated$validation_rows, result$validation_rows)
  expect_equal(repeated$table, result$table, tolerance = 1e-12)
  before <- .Random.seed
  for (unit in c("clustered", "repeated")) {
    refused <- sieve_cross_validate(x$data, x$meta, design = list(unit = unit))
    expect_match(refused$error, "assumes independent rows", fixed = TRUE)
    expect_null(refused$rng)
  }
  expect_identical(.Random.seed, before)
  few <- sieve_cross_validate(x$data[1:19, ], x$meta)
  expect_match(few$error, "17 complete cases", fixed = TRUE)
  expect_identical(few$eligible_rows, setdiff(1:19, c(2L, 17L)))
  expect_identical(.Random.seed, before)
})

test_that("ordered CV retains categorical targets without a covariance discrepancy", {
  x <- cv_fixture()
  x$data <- as.data.frame(lapply(x$data, function(v) {
    if (all(is.na(v))) rep(NA_integer_, length(v)) else as.integer(cut(v, breaks = 4L))
  }))
  x$meta$type <- "ordered"
  result <- sieve_cross_validate(x$data, x$meta, holdout = .4, seed = 113)
  expect_null(result$error)
  expect_true(result$ordered)
  expect_true(is.na(result$xval$Discrepancy[1L]))
  expect_true(is.na(result$xval$Discrepancy[3L]))
  expect_equal(result$n_val, round(.4 * length(result$eligible_rows)))
  # (The label names the matrix behind the denominator: the unrestricted
  # polychoric correlations.)
  expect_true(all(grepl("polychoric", result$reliability_targets, fixed = TRUE)))
})

test_that("the GUI and complete session replay use the public CV service", {
  withr::local_seed(577, .rng_kind = "L'Ecuyer-CMRG")
  x <- cv_fixture()
  state <- sieve_new_state()
  state$raw <- x$data
  state$items <- x$data
  state$meta <- x$meta
  state$roles <- sieve_declared_roles(x$data, x$meta$item)
  state$confirmed <- TRUE
  state$estimator <- "MLR"
  shiny::isolate(sieve_new_dataset(state))
  direct <- sieve_cross_validate(x$data, x$meta, holdout = .4, seed = 113, estimator = "mlr")
  shiny::testServer(mod_finalize_server, args = list(state = state), {
    session$setInputs(holdout = 40, seed = 113, estimator = "mlr", split = 1)
    app <- cv_shown()
    expect_null(app$error)
    expect_equal(app[names(direct)], direct, tolerance = 1e-12)
    script <- sieve_replay_script(state)
    expect_match(script, "cross_validation <- SIEVER::sieve_cross_validate", fixed = TRUE)
    expect_match(script, "do.call(RNGkind, as.list(", fixed = TRUE)
    env <- new.env(parent = baseenv())
    # Replay must restore the recorded RNG kind, not inherit this new one.
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    expect_no_error(suppressWarnings(capture.output(eval(parse(text = script), env))))
    expect_equal(env$cross_validation, direct, tolerance = 1e-12)
    expect_identical(env$cross_validation$rng$kind[[1L]], "L'Ecuyer-CMRG")
    expect_match(paste(as.character(unlist(output$cv_msg)), collapse = " "), "internal assessment", fixed = TRUE)
  })
})

# ---- Arguments checked as given ---------------------------------------------------
# validation_rows is checked before any unique(as.integer()), and an
# invalid holdout is refused, not made .5: otherwise a logical mask would
# become a one-row validation sample, respondent identifiers read as
# positions would silently select 98 of the 145 Grant-White rows with both
# halves "ok", and holdout = 30 (meant as 30 percent) would run a 50/50
# split labeled ok.

cv_two <- function() {
  data <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
  meta <- sieve_build_meta(data, names(data), list(Visual = paste0("x", 1:3), Textual = paste0("x", 4:6)))
  list(data = data, meta = meta, hs = lavaan::HolzingerSwineford1939)
}

test_that("CV refuses validation rows that are not row positions of data, naming the values, before the stream is touched", {
  x <- cv_two()
  set.seed(12)
  before <- .Random.seed
  refused <- function(rows) sieve_cross_validate(x$data, x$meta, validation_rows = rows)$error
  expect_match(refused(x$hs$school == "Pasteur"), paste(
    "`validation_rows` must be row numbers, whole numbers from 1 to 301, the rows of `data`; a logical vector",
    "of length 301 was given. For the rows a logical vector marks, pass which() of it"
  ), fixed = TRUE)
  # Respondent identifiers (Grant-White ids 201 to 351) passed as positions.
  ids <- refused(x$hs$id[x$hs$school == "Grant-White"])
  expect_match(ids, "holds 47 values above 301, the number of rows of `data` (302, 303, 304, 305, 306, and 42 more)",
    fixed = TRUE)
  expect_match(ids, "Row positions are not respondent identifiers: to reserve rows by identifier, pass match(ids, id_column)",
    fixed = TRUE)
  expect_match(refused(factor(x$hs$id[1:40])), "a factor was given", fixed = TRUE)
  expect_match(refused(as.character(1:40)), "text was given", fixed = TRUE)
  expect_match(refused(seq(1.9, 150.9)), "holds 150 values that are not whole numbers", fixed = TRUE)
  expect_match(refused(c(1:20, 500:900)), "holds 401 values above 301", fixed = TRUE)
  expect_match(refused(c(1:40, NA)), "1 missing or infinite value (at element 41: NA)", fixed = TRUE)
  expect_match(refused(c(0, 1:40)), "1 value below 1 (0)", fixed = TRUE)
  expect_match(refused(c(1:40, 10)), "repeats row number 10", fixed = TRUE)
  expect_identical(.Random.seed, before)
  # The same positions as doubles are the same rows as integers.
  expect_equal(sieve_cross_validate(x$data, x$meta, validation_rows = as.numeric(which(x$hs$school == "Pasteur"))),
    sieve_cross_validate(x$data, x$meta, validation_rows = which(x$hs$school == "Pasteur")), tolerance = 0)
})

test_that("CV refuses an invalid holdout and a half too small to fit, before the split is drawn, and records the holdout", {
  x <- cv_two()
  set.seed(13)
  before <- .Random.seed
  for (bad in list(30, 2, 0, 1, -.2, NA, c(.2, .3), "0.3")) {
    out <- sieve_cross_validate(x$data, x$meta, holdout = bad, seed = 1)
    expect_identical(out$error, sprintf(paste(
      "`holdout` must be one number strictly between 0 and 1, the proportion of the complete rows assigned",
      "to validation (0.5 by default); %s was given, so no split was drawn."
    ), sieve_given_text(bad)))
    expect_null(out$n_val)
  }
  # An empty half (not n_cal = 0 with status "partial") and a one-row half.
  empty <- sieve_cross_validate(x$data, x$meta, holdout = .999, seed = 1)
  expect_identical(empty$error, paste(
    "A validation proportion of 0.999 assigns 301 of the 301 complete rows to validation and 0 to calibration,",
    "so no split was drawn: each sample needs at least 7 complete rows, one more than the 6 active items,",
    "because with fewer its sample covariance matrix is singular and the model can be neither fit nor",
    "evaluated there."
  ))
  expect_match(sieve_cross_validate(x$data, x$meta, holdout = .004, seed = 1)$error,
    "assigns 1 of the 301 complete rows to validation and 300 to calibration", fixed = TRUE)
  three <- sieve_cross_validate(x$data, x$meta, validation_rows = 1:3)$error
  expect_match(three, "Of the 3 given validation rows, 3 are complete on the active items and 298 other complete rows remain for calibration, so no split was fit: each sample needs at least 7 complete rows",
    fixed = TRUE)
  expect_match(sieve_cross_validate(x$data, x$meta, validation_rows = 1:300)$error, "and 1 other complete rows remain",
    fixed = TRUE)
  # 1.9 is refused, not run as seed 1.
  expect_identical(sieve_cross_validate(x$data, x$meta, holdout = .3, seed = 1.9)$error, paste(
    "`seed` must be NULL or one whole number from -2147483647 to 2147483647; 1.9 was given.",
    "No split was drawn."
  ))
  expect_identical(.Random.seed, before)
  # The dispatcher records the refusal as a failed request.
  dispatched <- sieve_analyze("cross_validation", data = x$data, meta = x$meta, holdout = 30, seed = 1)
  expect_identical(dispatched$status$code, "failed")
  expect_match(dispatched$status$message, "30 was given, so no split was drawn", fixed = TRUE)
  # A drawn split records its holdout; given rows record NA and how many of
  # them were incomplete on the active items.
  drawn <- sieve_cross_validate(x$data, x$meta, holdout = .3, seed = 1)
  expect_identical(drawn$holdout, .3)
  expect_identical(drawn$n_val, 90L)
  expect_identical(drawn$n_reserved_incomplete, NA_integer_)
  holes <- x$data
  holes$x1[c(5L, 10L, 200L)] <- NA
  given <- sieve_cross_validate(holes, x$meta, validation_rows = 1:100)
  expect_identical(given$holdout, NA_real_)
  expect_identical(given$n_reserved_incomplete, 2L)
  expect_identical(given$n_val, 98L)
  expect_identical(given$validation_rows, setdiff(1:100, c(5L, 10L)))
})

test_that("the CV page shows the split's refusal of an invalid seed entry", {
  withr::local_seed(577)
  x <- cv_fixture()
  state <- sieve_new_state()
  state$raw <- x$data
  state$items <- x$data
  state$meta <- x$meta
  state$roles <- sieve_declared_roles(x$data, x$meta$item)
  state$confirmed <- TRUE
  state$estimator <- "MLR"
  shiny::isolate(sieve_new_dataset(state))
  shiny::testServer(mod_finalize_server, args = list(state = state), {
    session$setInputs(holdout = 40, seed = 12.7, estimator = "mlr", split = 1)
    expect_identical(cv_shown()$error, paste(
      "The random seed must be a whole number from -2147483647 to 2147483647; 12.7 was entered.",
      "Clear the box to have a seed drawn and recorded. No split was drawn."
    ))
    session$setInputs(seed = 113, split = 2)
    expect_null(cv_shown()$error)
    expect_identical(cv_shown()$rng$seed, 113L)
    expect_identical(cv_shown()$holdout, .4)
  })
})

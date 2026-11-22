# The shared contracts the analysis code is built on: captured conditions,
# the local random number
# stream, content-derived identities, the resolved CFA specification, and
# the fit status.

fresh_rscript <- function(code) {
  # A genuinely fresh R process: no .Random.seed exists until something
  # draws. The working source is loaded the way the suite loads it.
  root <- normalizePath(testthat::test_path("..", ".."))
  installed <- !file.exists(file.path(root, "DESCRIPTION"))
  load <- if (installed) "suppressMessages(library(SIEVER))" else
    sprintf("suppressMessages(pkgload::load_all(%s, quiet = TRUE, helpers = FALSE))", deparse(root))
  script <- tempfile(fileext = ".R")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(
    sprintf(".libPaths(%s)", paste(deparse(.libPaths()), collapse = "")),
    load, code
  ), script)
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(script)),
    stdout = TRUE, stderr = TRUE
  ))
  out
}

test_that("sieve_capture keeps warnings, messages, and the reason for a failure", {
  r <- SIEVER:::sieve_capture({
    warning("first")
    message("note one")
    warning("first")
    warning("second")
    42
  })
  expect_true(r$ok)
  expect_identical(r$value, 42)
  expect_identical(r$warnings, c("first", "second"))
  expect_identical(r$messages, "note one")
  expect_null(r$error)
  expect_null(r$error_class)

  f <- SIEVER:::sieve_capture({
    warning("before the failure")
    stop("the reason")
  })
  expect_false(f$ok)
  expect_null(f$value)
  expect_identical(f$error, "the reason")
  expect_identical(f$error_class, c("simpleError", "error", "condition"))
  expect_identical(f$warnings, "before the failure")

  # A classed condition keeps its classes, so a caller can recognize a
  # refusal by class rather than by its words.
  classed <- SIEVER:::sieve_capture(stop(structure(
    list(message = "refused, in whatever words", call = NULL, code = "example"),
    class = c("example_refusal", "dmar_refusal", "error", "condition"))))
  expect_false(classed$ok)
  expect_identical(classed$error, "refused, in whatever words")
  expect_identical(classed$error_class, c("example_refusal", "dmar_refusal", "error", "condition"))
})

test_that("a supplied seed gives a replayable result and restores the caller's stream", {
  set.seed(20261012)
  before <- get(".Random.seed", envir = globalenv())
  a <- SIEVER:::sieve_with_seed(113, stats::rnorm(5))
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  b <- SIEVER:::sieve_with_seed(113, stats::rnorm(5))
  expect_identical(a$value, b$value)
  expect_identical(a$rng$seed, 113L)
  expect_true(a$rng$supplied)
  expect_identical(a$rng$kind, RNGkind())
  # The recorded line replays the run in a plain script.
  code <- SIEVER:::sieve_seed_code(a$rng)
  expect_identical(code, "set.seed(113)")
  replay <- withr::with_seed(113, stats::rnorm(5))
  expect_identical(a$value, replay)
  # A failure inside the code still restores the stream.
  expect_error(SIEVER:::sieve_with_seed(7, stop("boom")), "boom")
  expect_identical(get(".Random.seed", envir = globalenv()), before)
})

test_that("an unseeded run differs between calls, advances the stream by one draw, and can be replayed", {
  set.seed(1)
  a <- SIEVER:::sieve_with_seed(NULL, stats::rnorm(5))
  after_a <- get(".Random.seed", envir = globalenv())
  # Exactly one draw was taken from the caller's stream to choose the seed.
  set.seed(1)
  expected_seed <- sample.int(.Machine$integer.max, 1L)
  expect_identical(a$rng$seed, expected_seed)
  expect_identical(get(".Random.seed", envir = globalenv()), after_a)
  expect_false(a$rng$supplied)
  b <- SIEVER:::sieve_with_seed(NULL, stats::rnorm(5))
  expect_false(identical(a$value, b$value))
  expect_false(identical(a$rng$seed, b$rng$seed))
  # The recorded seed replays the unseeded run exactly.
  expect_identical(SIEVER:::sieve_with_seed(a$rng$seed, stats::rnorm(5))$value, a$value)
  expect_match(SIEVER:::sieve_seed_code(a$rng), sprintf("^set\\.seed\\(%d\\)  # no seed was supplied", a$rng$seed))
  # Invalid seeds are refused, not truncated or dropped (the cases are in
  # test-input-validation.R).
  expect_error(SIEVER:::sieve_draw_seed(NA), "one whole number", class = "sieve_seed_invalid")
  expect_error(SIEVER:::sieve_draw_seed(c(1, 2)), "one whole number", class = "sieve_seed_invalid")
  expect_null(SIEVER:::sieve_seed_input(NA))
  expect_null(SIEVER:::sieve_seed_input(NULL))
  expect_error(SIEVER:::sieve_seed_input("abc"), class = "sieve_seed_invalid")
  expect_error(SIEVER:::sieve_seed_input(12.9), class = "sieve_seed_invalid")
  expect_identical(SIEVER:::sieve_seed_input(12), 12L)
})

test_that("a seeded run in a fresh session leaves no random number stream behind", {
  skip_on_cran()
  out <- fresh_rscript(c(
    'cat("before:", exists(".Random.seed", envir = globalenv()), "\\n")',
    'x <- SIEVER:::sieve_with_seed(113, stats::rnorm(3))',
    'cat("after:", exists(".Random.seed", envir = globalenv()), "\\n")',
    'cat("value:", format(x$value, digits = 17), "\\n")',
    'set.seed(113); cat("replay:", format(stats::rnorm(3), digits = 17), "\\n")'
  ))
  expect_true(any(grepl("^before: FALSE", out)), info = paste(out, collapse = "\n"))
  expect_true(any(grepl("^after: FALSE", out)), info = paste(out, collapse = "\n"))
  value <- sub("^value: ", "", grep("^value:", out, value = TRUE))
  replay <- sub("^replay: ", "", grep("^replay:", out, value = TRUE))
  expect_identical(value, replay)
})

test_that("no module restores the seed by hand or uses superassignment", {
  root <- testthat::test_path("..", "..")
  skip_if_not(dir.exists(file.path(root, "R")))
  files <- list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE)
  src <- unlist(lapply(files, function(f) {
    l <- readLines(f, warn = FALSE)
    l[!grepl("^\\s*#", l)]
  }))
  expect_false(any(grepl(".Random.seed", src, fixed = TRUE)))
  expect_false(any(grepl("<<-", src, fixed = TRUE)))
  expect_false(any(grepl("set.seed(", src, fixed = TRUE) & !grepl("\"", src, fixed = TRUE)))
})

test_that("data and ledger keys identify values, not storage", {
  d <- data.frame(a = c(1L, 2L, 3L), b = c(0.1, 0.2, NA))
  k <- SIEVER:::sieve_data_key(d)
  expect_match(k, "^[0-9a-f]{32}$")
  expect_identical(SIEVER:::sieve_data_key(d), k)
  d_dbl <- d
  d_dbl$a <- as.double(d_dbl$a)
  expect_identical(SIEVER:::sieve_data_key(d_dbl), k)
  rownames(d_dbl) <- c("x", "y", "z")
  expect_identical(SIEVER:::sieve_data_key(d_dbl), k)
  # A round trip through a full precision file keeps the identity.
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  utils::write.csv(d, f, row.names = FALSE)
  expect_identical(SIEVER:::sieve_data_key(utils::read.csv(f)), k)
  changed <- d
  changed$b[1] <- 0.1 + 1e-12
  expect_false(identical(SIEVER:::sieve_data_key(changed), k))
  expect_false(identical(SIEVER:::sieve_data_key(d[, 2:1]), k))
  expect_false(identical(SIEVER:::sieve_data_key(d[1:2, ]), k))
  expect_true(is.na(SIEVER:::sieve_data_key(NULL)))

  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  mk <- SIEVER:::sieve_meta_key(meta)
  relabeled <- meta
  relabeled$label[1] <- "A different display label"
  expect_identical(SIEVER:::sieve_meta_key(relabeled), mk)
  for (col in c("construct", "type", "status")) {
    m2 <- meta
    m2[[col]][1] <- "something else"
    expect_false(identical(SIEVER:::sieve_meta_key(m2), mk), info = col)
  }
  m3 <- meta
  m3$reverse[1] <- !isTRUE(m3$reverse[1])
  expect_false(identical(SIEVER:::sieve_meta_key(m3), mk))
})

test_that("a revision matches only the inputs it was taken from", {
  fx <- sieve_hs9_fixture()
  state <- shiny::reactiveValues(
    dataset_id = NULL, dataset_seq = 0L,
    items = fx$data[, fx$item_cols],
    meta = sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  )
  shiny::isolate({
    expect_identical(SIEVER:::sieve_new_dataset(state), "D1")
    r1 <- SIEVER:::sieve_revision(state)
    expect_true(SIEVER:::sieve_same_revision(r1, SIEVER:::sieve_revision(state)))
    # Reloading the same file is a new dataset: an undo record or a search
    # candidate from the first load must not apply to the second.
    expect_identical(SIEVER:::sieve_new_dataset(state), "D2")
    r2 <- SIEVER:::sieve_revision(state)
    expect_false(SIEVER:::sieve_same_revision(r1, r2))
    expect_true(SIEVER:::sieve_same_revision(r1, r2, what = c("data", "meta")))
    state$items[[1]][1] <- state$items[[1]][1] + 1
    r3 <- SIEVER:::sieve_revision(state)
    expect_false(SIEVER:::sieve_same_revision(r2, r3, what = "data"))
    expect_true(SIEVER:::sieve_same_revision(r2, r3, what = c("dataset", "meta")))
    state$meta$status[2] <- "parked"
    expect_false(SIEVER:::sieve_same_revision(r3, SIEVER:::sieve_revision(state), what = "meta"))
    # A revision with a missing part never matches, even against itself.
    blank <- list(dataset_id = NA_character_, data_key = NA_character_, meta_key = NA_character_)
    expect_false(SIEVER:::sieve_same_revision(blank, blank))
  })
})

test_that("the CFA specification resolves the engine choices the fit and the script share", {
  skip_if_not_installed("lavaan")
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  spec <- SIEVER:::sieve_cfa_spec(meta)
  expect_s3_class(spec, "sieve_spec")
  # ML with FIML: DMAR's default, the study's for continuous items.
  expect_identical(spec$estimator, "ML")
  expect_identical(spec$missing, "fiml")
  expect_null(spec$ordered)
  expect_identical(spec$missing, "fiml")
  expect_true(spec$std_lv)
  expect_identical(spec$observed, fx$item_cols)
  expect_identical(spec$latent, c("Visual", "Textual", "Speed"))

  run <- SIEVER:::sieve_run_cfa(spec, fx$data)
  expect_s3_class(run, "sieve_result")
  expect_identical(run$status$code, "ok")
  opt <- lavaan::lavInspect(run$fit, "options")
  expect_identical(toupper(opt$estimator), "ML")
  expect_true(any(c("ml", "fiml", "direct") %in% opt$missing))
  expect_identical(attr(run$fit, "sieve_spec")$key, spec$key)

  # The recorded script reproduces the fit exactly in a clean environment.
  env <- new.env(parent = globalenv())
  assign("items", fx$data[, fx$item_cols], envir = env)
  code <- SIEVER:::sieve_cfa_code(spec)
  code <- code[!grepl("lavaan::summary", code, fixed = TRUE)]
  eval(parse(text = paste(code, collapse = "\n")), envir = env)
  expect_equal(lavaan::coef(env$cfa_fit), lavaan::coef(run$fit), tolerance = 1e-10)
  expect_equal(unname(lavaan::fitMeasures(env$cfa_fit, c("chisq.scaled", "df", "cfi.robust"))),
    unname(lavaan::fitMeasures(run$fit, c("chisq.scaled", "df", "cfi.robust"))),
    tolerance = 1e-10
  )
})

test_that("a categorical CFA is fit and recorded with its ordered variables and listwise deletion", {
  skip_if_not_installed("lavaan")
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols]
  # Four ordered categories per test, cut at the quartiles; a few values set
  # missing so the missing data treatment matters.
  d[] <- lapply(d, function(v) as.integer(cut(v, stats::quantile(v, 0:4 / 4), include.lowest = TRUE)))
  d[c(3, 40, 170), 1] <- NA
  meta <- sieve_build_meta(d, fx$item_cols, fx$theory_map, labels = fx$labels)
  meta$type <- "ordered"
  spec <- SIEVER:::sieve_cfa_spec(meta)
  expect_identical(spec$estimator, "WLSMV")
  expect_identical(spec$ordered, fx$item_cols)
  expect_identical(spec$missing, "listwise")

  run <- SIEVER:::sieve_run_cfa(spec, d)
  expect_false(is.null(run$fit))
  opt <- lavaan::lavInspect(run$fit, "options")
  expect_identical(opt$missing, "listwise")
  expect_identical(lavaan::lavInspect(run$fit, "nobs"), 298L)

  code <- SIEVER:::sieve_cfa_code(spec)
  expect_true(any(grepl("ordered = c(\"t1_visual_perception\"", code, fixed = TRUE)))
  expect_true(any(grepl("missing = \"listwise\"", code, fixed = TRUE)))
  expect_false(any(grepl("fiml", code, fixed = TRUE)))
  env <- new.env(parent = globalenv())
  assign("items", d, envir = env)
  code <- code[!grepl("lavaan::summary", code, fixed = TRUE)]
  expect_no_error(eval(parse(text = paste(code, collapse = "\n")), envir = env))
  expect_equal(lavaan::coef(env$cfa_fit), lavaan::coef(run$fit), tolerance = 1e-10)
  expect_identical(lavaan::lavInspect(env$cfa_fit, "nobs"), 298L)
})

test_that("the estimator follows the indicators that are in the model, not every active item", {
  skip_if_not_installed("lavaan")
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  # An ordered item alone in its own construct is not in the model (one
  # indicator), so it must not switch the continuous model to WLSMV.
  meta$type[9] <- "ordered"
  meta$construct[9] <- "Alone"
  spec <- SIEVER:::sieve_cfa_spec(meta)
  expect_false(fx$item_cols[9] %in% spec$observed)
  expect_identical(spec$estimator, "ML")
  expect_null(spec$ordered)
  expect_identical(spec$missing, "fiml")
})

test_that("a fitted object is not a certificate: the status names what lavaan established", {
  skip_if_not_installed("lavaan")
  fx <- sieve_hs9_fixture()
  d <- fx$data[, fx$item_cols]

  # One factor measured by two indicators with both loadings free: more
  # parameters than moments.
  under <- SIEVER:::sieve_capture(lavaan::cfa(
    "f =~ t1_visual_perception + t2_cubes",
    data = d, std.lv = TRUE
  ))
  st <- SIEVER:::sieve_fit_status(under$value, warnings = under$warnings)
  expect_identical(st$code, "not_identified")
  expect_lt(st$df, 0)

  # Three indicators: just identified. Usable, and said to be untestable.
  just <- lavaan::cfa("f =~ t1_visual_perception + t2_cubes + t4_lozenges", data = d, std.lv = TRUE)
  sj <- SIEVER:::sieve_fit_status(just)
  expect_identical(sj$code, "ok")
  expect_true(sj$just_identified)
  expect_match(sj$message, "just identified")

  # A legitimately identified two indicator model (equal loadings) passes:
  # the rule is identification, not a count of items.
  # The two tests are put on a common scale first: with equal loadings the
  # common variance is the covariance, which on the raw scales exceeds the
  # smaller of the two variances and so forces a negative residual variance.
  z <- as.data.frame(scale(d[, c("t6_paragraph_comprehension", "t7_sentence")]))
  tau <- lavaan::cfa("f =~ a*t6_paragraph_comprehension + a*t7_sentence", data = z, std.lv = TRUE)
  expect_identical(SIEVER:::sieve_fit_status(tau)$code, "ok")
  raw_tau <- SIEVER:::sieve_capture(lavaan::cfa(
    "f =~ a*t6_paragraph_comprehension + a*t7_sentence",
    data = d, std.lv = TRUE
  ))
  expect_identical(
    SIEVER:::sieve_fit_status(raw_tau$value, warnings = raw_tau$warnings)$code,
    "inadmissible"
  )

  # Non-convergence.
  nc <- SIEVER:::sieve_capture(lavaan::cfa(
    sieve_cfa_plan(sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels))$model,
    data = d, std.lv = TRUE, control = list(iter.max = 2L)
  ))
  sn <- SIEVER:::sieve_fit_status(nc$value, warnings = nc$warnings)
  expect_identical(sn$code, "not_converged")
  expect_false(sn$converged)

  # A negative residual variance (a Heywood case) from a population that
  # has one in the sample: identified and converged, but inadmissible.
  set.seed(113)
  n <- 60
  f <- stats::rnorm(n)
  h <- data.frame(
    y1 = 0.99 * f + stats::rnorm(n, sd = 0.05),
    y2 = 0.4 * f + stats::rnorm(n, sd = 0.9),
    y3 = 0.4 * f + stats::rnorm(n, sd = 0.9)
  )
  hey <- SIEVER:::sieve_capture(lavaan::cfa("f =~ y1 + y2 + y3", data = h, std.lv = TRUE))
  sh <- SIEVER:::sieve_fit_status(hey$value, warnings = hey$warnings)
  pe <- lavaan::parameterEstimates(hey$value)
  has_negative <- any(pe$est[pe$op == "~~" & pe$lhs == pe$rhs] < 0)
  expect_identical(sh$code, if (has_negative) "inadmissible" else "ok")
  expect_identical(length(sh$negative_variances) > 0L, has_negative)

  # No fit at all keeps its reason.
  none <- SIEVER:::sieve_fit_status(NULL, error = "lavaan said no")
  expect_identical(none$code, "failed")
  expect_identical(none$message, "lavaan said no")
})

test_that("a failed CFA run keeps the reason instead of returning a bare NULL", {
  skip_if_not_installed("lavaan")
  fx <- sieve_hs9_fixture()
  meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  spec <- SIEVER:::sieve_cfa_spec(meta)
  run <- SIEVER:::sieve_run_cfa(spec, fx$data[, fx$item_cols[1:5]])
  expect_null(run$fit)
  expect_identical(run$status$code, "failed")
  expect_match(run$error, "do not contain the modeled variable")
  empty <- SIEVER:::sieve_cfa_spec(meta[0, ])
  expect_identical(SIEVER:::sieve_run_cfa(empty, fx$data)$status$code, "failed")
  # The exported wrapper returns NULL to a caller that needs only the fit.
  expect_null(sieve_fit_cfa(fx$data[, fx$item_cols[1:5]], meta))
})

test_that("the version shown is the version of the code that is running", {
  v <- SIEVER:::sieve_version()
  root <- testthat::test_path("..", "..")
  if (file.exists(file.path(root, "DESCRIPTION"))) {
    expect_identical(v, unname(read.dcf(file.path(root, "DESCRIPTION"), "Version")[1, 1]))
  } else {
    expect_identical(v, as.character(utils::packageVersion("SIEVER")))
  }
  expect_match(SIEVER:::sieve_version_label(), "(development source|installed package)\\)$")
  expect_true(startsWith(SIEVER:::sieve_version_label(), v))
})

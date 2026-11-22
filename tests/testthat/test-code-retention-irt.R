# Disclosures read saved computations rather than evaluating the numerical
# reactives whose results they describe.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

code_coverage_state <- function(ordered = FALSE, confirmed = TRUE) {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = confirmed, labels = TRUE,
    theory = FALSE, name = NULL, configure = function(s) {
  if (ordered) shiny::isolate({
    s$items <- as.data.frame(lapply(s$items, function(v) as.integer(cut(v, breaks = 5))))
    m <- s$meta
    m$type <- "ordered"
    s$meta <- m
  })
  })
  s
}

code_coverage_html <- function(x) paste(as.character(x$html), collapse = "\n")

test_that("retention code disclosures remain empty until a calculation runs", {
  st <- code_coverage_state(confirmed = FALSE)
  calls <- 0L
  local_mocked_bindings(sieve_state_basis = function(...) {
    calls <<- calls + 1L
    stop("Reading code must not request a correlation matrix.")
  })
  shiny::testServer(mod_retention_server, args = list(state = st), {
    before <- shiny::isolate(state$codelog)
    for (id in c("scree_code", "pa_code", "vss_code", "fit_code")) {
      html <- code_coverage_html(output[[id]])
      expect_match(html, "Run this analysis", fixed = TRUE)
      expect_match(html, "<details", fixed = TRUE)
      expect_false(grepl("<details[^>]*\\bopen", html))
      expect_false(grepl("lavaan::efa", html, fixed = TRUE))
    }
    expect_identical(shiny::isolate(state$codelog), before)
    expect_identical(calls, 0L)
  })
})

test_that("retention scree code freezes its input preparation and does not rerun", {
  st <- code_coverage_state()
  calls <- 0L
  original <- sieve_scree_values
  local_mocked_bindings(sieve_scree_values = function(...) {
    calls <<- calls + 1L
    original(...)
  })
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "pearson", seed = 71, max_k = 3)
    expect_true(scree_vals()$ok)
    saved <- shiny::isolate(code_records())$scree
    expect_match(paste(saved$code, collapse = "\n"), "R_items <- stats::cor", fixed = TRUE)
    expect_match(paste(saved$code, collapse = "\n"), "stats::complete.cases", fixed = TRUE)
    expect_match(paste(saved$code, collapse = "\n"),
      sprintf("retention_items <- c(%s", encodeString(names(shiny::isolate(state$items))[1L], quote = '"')),
      fixed = TRUE)
    sieve_log(state, "Unrelated later action", "unrelated_later_action <- TRUE")
    before <- shiny::isolate(state$codelog)
    counter_before <- calls
    rng_before <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
    html <- code_coverage_html(output$scree_code)
    expect_match(html, "Scree eigenvalues", fixed = TRUE)
    expect_false(grepl("unrelated_later_action", html, fixed = TRUE))
    expect_identical(shiny::isolate(code_records())$scree, saved)
    expect_identical(shiny::isolate(state$codelog), before)
    expect_identical(calls, counter_before)
    expect_identical(get0(".Random.seed", envir = globalenv(), inherits = FALSE), rng_before)
    session$setInputs(cor_type = "polychoric")
    expect_match(code_coverage_html(output$scree_code), "earlier result", fixed = TRUE)
    expect_identical(calls, counter_before)
    state$dataset_id <- "another_loaded_dataset"
    session$flushReact()
    expect_match(code_coverage_html(output$scree_code), "Run this analysis", fixed = TRUE)
  })
})

test_that("IRT disclosures never fit an unrun model or information grid", {
  st <- code_coverage_state(ordered = TRUE)
  calls <- 0L
  local_mocked_bindings(irt_grm = function(...) {
    calls <<- calls + 1L
    stop("Code viewing must not fit the model.")
  }, .package = "DMAR")
  shiny::testServer(mod_irt_server, args = list(state = st), {
    session$setInputs(scope = "Textual", estimator = "WLSMV")
    before <- shiny::isolate(state$codelog)
    for (id in c("model_code", "fit_code", "test_info_code", "item_info_code", "coverage_code")) {
      html <- code_coverage_html(output[[id]])
      expect_match(html, "Run this analysis", fixed = TRUE)
      expect_false(grepl("DMAR::irt_grm", html, fixed = TRUE))
    }
    expect_null(shiny::isolate(information_code()))
    expect_identical(shiny::isolate(state$codelog), before)
    expect_identical(calls, 0L)
  })
})

test_that("IRT disclosures keep the fitted parent and information grid without a refit", {
  skip_if_not_installed("DMAR")
  skip_if_not_installed("lavaan")
  st <- code_coverage_state(ordered = TRUE)
  calls <- 0L
  original <- DMAR::irt_grm
  local_mocked_bindings(irt_grm = function(...) {
    calls <<- calls + 1L
    original(...)
  }, .package = "DMAR")
  shiny::testServer(mod_irt_server, args = list(state = st), {
    session$setInputs(scope = "Textual", estimator = "WLSMV", theta_min = -3, theta_max = 3, theta_n = 21)
    session$setInputs(fit_go = 1L)
    g <- grm_fit()
    expect_null(g$error)
    expect_true(is.list(g$code_record))
    expect_match(paste(g$code_record$code, collapse = "\n"),
      sprintf("irt_items <- c(%s)", paste(encodeString(g$items, quote = '"'), collapse = ", ")), fixed = TRUE)
    expect_null(info_res()$error)
    saved_info <- shiny::isolate(information_code())
    before <- shiny::isolate(state$codelog)
    counter_before <- calls
    rng_before <- get0(".Random.seed", envir = globalenv(), inherits = FALSE)
    model_html <- code_coverage_html(output$model_code)
    info_html <- code_coverage_html(output$test_info_code)
    expect_match(model_html, "DMAR::irt_grm", fixed = TRUE)
    expect_match(info_html, "DMAR::irt_information", fixed = TRUE)
    expect_match(info_html, "length.out = 21", fixed = TRUE)
    expect_identical(shiny::isolate(information_code()), saved_info)
    expect_identical(shiny::isolate(state$codelog), before)
    expect_identical(calls, counter_before)
    expect_identical(get0(".Random.seed", envir = globalenv(), inherits = FALSE), rng_before)
    session$setInputs(theta_max = 4)
    # The existing information outputs update this cheap derived quantity
    # when the trait grid changes. The disclosure must follow that actual
    # computation without fitting the model or creating another log entry.
    updated <- shiny::isolate(information_code())
    expect_identical(max(updated$theta), 4)
    updated_log <- shiny::isolate(state$codelog)
    expect_match(code_coverage_html(output$test_info_code), "theta -3 to 4", fixed = TRUE)
    expect_identical(calls, counter_before)
    expect_identical(shiny::isolate(information_code()), updated)
    expect_identical(shiny::isolate(state$codelog), updated_log)
    session$setInputs(scope = "Speed")
    expect_match(code_coverage_html(output$test_info_code), "Run this analysis", fixed = TRUE)
    expect_match(code_coverage_html(output$model_code), "Run this analysis", fixed = TRUE)
    expect_identical(calls, counter_before)
  })
})

test_that("each completed retention section supplies replayable saved preparation", {
  # Low-B record plumbing only; production reference counts are tested separately.
  siever_local_dmar_mechanics()
  skip_if_not_installed("psych")
  skip_if_not_installed("lavaan")
  st <- code_coverage_state()
  withr::local_options(sieve.cores = 1L)
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  shiny::testServer(mod_retention_server, args = list(state = st), {
    session$setInputs(cor_type = "pearson", seed = 71L, n_iter = 10L,
      max_k = 2L, estimator = "ml", workers = 1L, criterion = "p95", pa_reference = "simulated")
    scr <- scree_vals()
    par <- pa_sel()
    vr <- vss_run()
    fr <- fit_run()
    expect_true(scr$ok)
    expect_true(par$ok)
    expect_true(vr$ok)
    expect_true(fr$ok)
    records <- shiny::isolate(code_records())
    expect_setequal(names(records), c("scree", "pa", "vss", "fit"))
    for (section in names(records)) {
      saved <- records[[section]]
      expect_no_error(parse(text = saved$code))
      expect_match(paste(saved$code, collapse = "\n"), "active_items <- items_data", fixed = TRUE, info = section)
      expect_true(length(saved$input_key) == 1L, info = section)
    }
    # The saved fit-by-k preparation follows the model's actual rows and
    # missing-data policy, rather than borrowing the correlations' sample.
    expect_match(paste(records$fit$code, collapse = "\n"), 'estimator = "ML"', fixed = TRUE)
    env <- new.env(parent = globalenv())
    env$items_data <- shiny::isolate(state$items)
    withr::with_preserve_seed(eval(parse(text = records$scree$code), envir = env))
    expect_equal(env$scree_unreduced, scr$table$unreduced, tolerance = 1e-12)
    withr::with_preserve_seed(eval(parse(text = records$fit$code), envir = env))
    expect_equal(as.numeric(env$fit_by_k$rmsea), as.numeric(fr$table$rmsea), tolerance = 1e-10)
  })
})

# Cached results must identify their own fitted parent in the recorded script.
cached_parent_state <- function(ordered = FALSE) {
  fx <- sieve_hs9_fixture()
  s <- siever_test_study(fx, confirmed = TRUE, labels = TRUE,
    theory = FALSE, name = NULL, configure = function(s) {
  s$roles <- sieve_roles_table(fx$data, fx$data[, fx$item_cols, drop = FALSE], NULL)
  if (ordered) shiny::isolate({
    s$items <- as.data.frame(lapply(s$items, function(x) as.integer(cut(x, breaks = 5L))))
    m <- s$meta
    m$type <- "ordered"
    s$meta <- m
  })
  })
  s
}

cached_parent_child <- function(state, expression) {
  directory <- withr::local_tempdir()
  paths <- sieve_session_write(state, directory)
  output <- file.path(directory, "result.rds")
  runner <- file.path(directory, "child.R")
  writeLines(c(
    sprintf("setwd(%s)", deparse(directory)),
    sprintf("sys.source(%s, envir = globalenv())", deparse(paths[["script"]])),
    sprintf("saveRDS(%s, %s)", expression, deparse(output))
  ), runner)
  log <- file.path(directory, "child.log")
  status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(runner)),
    stdout = log, stderr = log,
    # The source under test, installed first on the path (helper-interface.R):
    # the script checks the SIEVER version and calls SIEVER:: functions.
    env = paste0("R_LIBS=", paste(compat_source_libs(), collapse = .Platform$path.sep))
  )
  expect_identical(status, 0L, info = paste(readLines(log, warn = FALSE), collapse = "\n"))
  if (!identical(status, 0L)) return(NULL)
  readRDS(output)
}

test_that("cached nonfirst IRT information and evidence rebuild their selected model", {
  skip_on_cran()
  skip_if_not_installed("DMAR")
  skip_if_not_installed("lavaan")
  st <- cached_parent_state(TRUE)
  expected <- new.env(parent = emptyenv())
  shiny::testServer(mod_irt_server, args = list(state = st), {
    session$setInputs(
      scope = "Textual", estimator = "WLSMV", metric = "normal_ogive",
      theta_min = -3, theta_max = 3, theta_n = 21L, info_threshold = 10
    )
    session$setInputs(fit_go = 1L)
    first <- grm_fit()
    expect_null(first$error)
    invisible(info_res())
    session$setInputs(scope = "Speed", fit_go = 2L)
    expect_null(grm_fit()$error)
    invisible(info_res())
    session$setInputs(scope = "Textual")
    expect_equal(grm_fit()$grm$a, first$grm$a, tolerance = 1e-12)
    expected$info <- info_res()$info$test_information
    expected$parameters <- sieve_irt_parameter_evidence(first$grm)$table
    session$setInputs(parameter_keep = 1L)
    expected$items <- first$items
  })
  actual <- cached_parent_child(
    st,
    "list(items = irt_items, information = info$test_information, parameters = irt_parameter_intervals$table)"
  )
  expect_identical(actual$items, expected$items)
  expect_equal(actual$information, expected$info, tolerance = 1e-12)
  expect_equal(actual$parameters, expected$parameters, tolerance = 1e-12)
})

test_that("cached structural paths, R-squared, and effects rebuild their selected models", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  st <- cached_parent_state()
  r <- shiny::isolate(st$roles)
  r$role[r$variable == "age_years"] <- "criterion"
  st$roles <- r
  shiny::isolate(sieve_confirm_types(st))
  expected <- new.env(parent = emptyenv())
  shiny::testServer(mod_sem_server, args = list(state = st), {
    session$setInputs(
      outcomes = c("Textual", "age_years"), predictors = c("Visual", "Textual"),
      layout = "tree2", min_path = 0
    )
    session$setInputs(fit_go = 1L)
    first <- path_run()
    expect_identical(first$status$code, "ok")
    invisible(output$paths_tbl)
    invisible(output$r2_tbl)
    invisible(output$effects_tbl)
    session$setInputs(outcomes = c("Speed", "age_years"), predictors = c("Visual", "Speed"), fit_go = 2L)
    expect_identical(path_run()$status$code, "ok")
    invisible(output$paths_tbl)
    invisible(output$r2_tbl)
    invisible(output$effects_tbl)
    session$setInputs(outcomes = c("Textual", "age_years"), predictors = c("Visual", "Textual"))
    expect_identical(path_run()$spec$key, first$spec$key)
    invisible(output$paths_tbl)
    invisible(output$r2_tbl)
    invisible(output$effects_tbl)
    expected$paths <- lavaan::standardizedSolution(fit())
    expected$r2 <- sieve_R2_intervals(fit())
    expected$effects <- lavaan::standardizedSolution(med_fit())
  })
  actual <- cached_parent_child(
    st,
    "list(paths = sem_std, R2 = sem_R2, effects = sem_std_effects)"
  )
  expect_equal(actual$paths, expected$paths, tolerance = 1e-12)
  expect_equal(actual$R2, expected$r2, tolerance = 1e-12)
  expect_equal(actual$effects, expected$effects, tolerance = 1e-12)
})

test_that("Bollen-Stine replay needs no earlier CFA fit or implicit complete-case data", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  st <- cached_parent_state()
  expected <- new.env(parent = emptyenv())
  shiny::testServer(mod_cfa_server, args = list(state = st), {
    session$setInputs(structure = "correlated", ident = "std", bs_B = 100L, bs_seed = 20261020L)
    session$setInputs(bs_run = 1L)
    # The run is a background process: the mock clock advances while real
    # time passes, and a run still going at the limit fails here and leaves
    # nothing to replay.
    expected$finished <- siever_wait_until(function() !isTRUE(shiny::isolate(bs_running())),
      "The Bollen-Stine run", step = function() {
        Sys.sleep(0.05)
        session$elapse(250)
      })
    if (expected$finished) {
      b <- bs_result()
      expect_null(b$error)
      expected$result <- as.numeric(unlist(b[c("chisq", "df", "p_ml", "p_bs", "n")]))
    }
  })
  if (expected$finished) {
    # The recorded R is the refit and one DMAR::sem_exact_fit_bootstrap() call,
    # whose table the replay reads.
    actual <- cached_parent_child(st, paste0(
      'unname(stats::setNames(bollen_stine$value, bollen_stine$term)',
      '[c("chi_square", "df", "p_chi_square", "p_bootstrap", "N")])'
    ))
    expect_equal(actual, expected$result, tolerance = 1e-12)
  }
})


test_that("bifactor primary replay binds its own estimator and missing-data treatment", {
  skip_on_cran()
  skip_if_not_installed("lavaan")
  skip_if_not_installed("DMAR")
  st <- cached_parent_state()
  expected <- new.env(parent = emptyenv())
  shiny::testServer(mod_bifactor_server, args = list(state = st), {
    session$setInputs(estimator_choice = "mlr")
    session$setInputs(fit = 1L)
    r <- demand$result()
    expect_s4_class(r$fit, "lavaan")
    expected$coefficients <- lavaan::coef(r$fit)
  })
  actual <- cached_parent_child(st, "lavaan::coef(bifactor_fit)")
  expect_equal(actual, expected$coefficients, tolerance = 1e-12)
})

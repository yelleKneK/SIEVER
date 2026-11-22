# Factor names identify columns within one EFA; navigation is not a fit request.

# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

efa_order_state <- function() {
  siever_test_study(name = NULL, configure = function(s) s$theory <- list(k = 3L))
}

efa_order_text <- function(x) paste(as.character(unlist(x)), collapse = " ")

test_that("page cues inspect EFA identities without running an analysis", {
  state <- efa_order_state()
  calls <- new.env(parent = emptyenv())
  calls$fa <- 0L
  calls$basis <- 0L
  calls$worker <- 0L
  local_mocked_bindings(fa_exploratory = function(...) {
    calls$fa <- calls$fa + 1L
    stop("The page cue must not fit EFA")
  }, .package = "DMAR")
  local_mocked_bindings(
    sieve_retention_cor = function(...) {
      calls$basis <- calls$basis + 1L
      stop("The page cue must not estimate correlations")
    },
    sieve_efa_dmar_start = function(...) {
      calls$worker <- calls$worker + 1L
      stop("The page cue must not start a worker")
    }
  )
  shiny::testServer(function(input, output, session) {
    mod_efa_server("efa", state)
  }, {
    session$setInputs(nav = "ingest", `efa-n_factors` = 3L)
    guide <- sieve_page_status(state, "efa")
    records <- shiny::isolate(state$page_results$efa)
    expect_length(records, 2L)
    expect_true(all(vapply(records, function(x) identical(x$state, "not_run"), logical(1))))
    expect_length(guide$results, 0L)
    expect_identical(calls$fa, 0L)
    expect_identical(calls$basis, 0L)
    expect_identical(calls$worker, 0L)
    expect_length(shiny::isolate(state$codelog), 0L)
    expect_false(any(c("fit", "value", "items") %in% names(records[[1L]])))
  })
})

test_that("EFA navigation reuses exact fit and code while changed inputs wait for Run", {
  state <- efa_order_state()
  calls <- new.env(parent = emptyenv())
  calls$fa <- 0L
  engine <- DMAR::fa_exploratory
  local_mocked_bindings(fa_exploratory = function(...) {
    calls$fa <- calls$fa + 1L
    engine(...)
  }, .package = "DMAR")
  shiny::testServer(function(input, output, session) {
    mod_efa_server("efa", state)
  }, {
    session$setInputs(nav = "efa", `efa-n_factors` = 3L, `efa-fm` = "ml",
      `efa-rotation` = "oblimin", `efa-cor_type` = "auto")
    first <- output[["efa-efa_code"]]
    before <- shiny::isolate(state$codelog)
    expect_identical(calls$fa, 1L)
    expect_match(efa_order_text(first), "DMAR::fa_exploratory", fixed = TRUE)
    for (page in c("cfa", "repro", "ingest", "efa")) {
      session$setInputs(nav = page)
      expect_identical(output[["efa-efa_code"]], first)
      expect_identical(calls$fa, 1L)
    }
    expect_identical(shiny::isolate(state$codelog), before)
    expect_identical(shiny::isolate(state$page_results$efa[["efa-primary"]]$state), "current")

    # The correlations of the matrix route do not enter a solution fit to
    # the item responses.
    session$setInputs(`efa-cor_type` = "pearson")
    expect_identical(output[["efa-efa_code"]], first)
    expect_identical(calls$fa, 1L)
    expect_identical(shiny::isolate(state$codelog), before)

    # The estimator is the solution's own (DMAR fits the item responses
    # with it), so a changed estimator is a changed request that waits.
    session$setInputs(`efa-estimator` = "ml")
    expect_identical(shiny::isolate(state$page_results$efa[["efa-primary"]]$state), "stale")
    expect_identical(calls$fa, 1L)
    session$setInputs(`efa-efa_go` = 1L)
    expect_match(efa_order_text(output[["efa-efa_code"]]), "3 factors; ML on the complete cases", fixed = TRUE)
    expect_identical(calls$fa, 2L)

    shiny::isolate({
      values <- state$items
      values[1L, 1L] <- values[1L, 1L] + .1
      state$items <- values
    })
    sieve_confirm_types(state)
    session$flushReact()
    expect_identical(shiny::isolate(state$page_results$efa[["efa-primary"]]$state), "stale")
    expect_match(efa_order_text(output[["efa-efa_code"]]), "Run this analysis", fixed = TRUE)
    expect_identical(calls$fa, 2L)
    session$setInputs(nav = "repro")
    session$setInputs(nav = "efa")
    output[["efa-efa_code"]]
    expect_identical(calls$fa, 2L)
    session$setInputs(`efa-efa_go` = 2L)
    expect_match(efa_order_text(output[["efa-efa_code"]]), "DMAR::fa_exploratory", fixed = TRUE)
    expect_identical(calls$fa, 3L)
    expect_identical(shiny::isolate(state$page_results$efa[["efa-primary"]]$state), "current")

    session$setInputs(`efa-n_factors` = 2L)
    expect_match(efa_order_text(output[["efa-efa_code"]]), "Run this analysis", fixed = TRUE)
    expect_identical(calls$fa, 3L)
    session$setInputs(`efa-efa_go` = 3L)
    expect_match(efa_order_text(output[["efa-efa_code"]]), "2 factors", fixed = TRUE)
    expect_identical(calls$fa, 4L)
  })
})

test_that("factor-order explanations and executable readout preserve all named matrices", {
  state <- efa_order_state()
  shiny::testServer(mod_efa_server, args = list(state = state), {
    session$setInputs(efa_go = 1L, n_factors = 3L, fm = "ml", rotation = "oblimin")
    run <- efa_run()
    pattern <- unclass(run$fit$loadings)
    order <- colnames(pattern)
    expect_match(efa_order_text(output$factor_order_pattern), paste(order, collapse = ", "), fixed = TRUE)
    expect_match(efa_order_text(output$factor_order_structure), paste(order, collapse = ", "), fixed = TRUE)
    expect_match(efa_order_text(output$factor_order_phi), paste(order, collapse = ", "), fixed = TRUE)
    expect_match(efa_order_text(output$factor_order_phi), "both rows and columns of Phi", fixed = TRUE)
    expect_match(efa_order_text(output$factor_order_phi), "Sorting items changes rows", fixed = TRUE)
    # The readout reads DMAR's table, the recorded efa_fit.
    code <- factor_readout_code()
    env <- new.env(parent = baseenv())
    env$efa_fit <- run$fit$dmar
    read <- eval(parse(text = code), env)
    expect_identical(read$factor_column_order, order)
    expect_identical(read$pattern, pattern)
    expect_equal(read$structure, pattern %*% run$fit$Phi, tolerance = 1e-12, ignore_attr = TRUE)
    expect_identical(read$Phi, run$fit$Phi)
    expect_identical(colnames(read$structure), order)

    # Readout uses the fitted labels themselves, including a nonsequential
    # column order, rather than assuming Factor 1, Factor 2, Factor 3.
    relabel <- c(f1 = "f3", f2 = "f1", f3 = "f2")
    modified <- run$fit$dmar
    class(modified) <- "data.frame"
    for (column in c("lhs", "rhs")) {
      at <- modified[[column]] %in% names(relabel)
      modified[[column]][at] <- unname(relabel[modified[[column]][at]])
    }
    env$efa_fit <- modified
    reordered <- eval(parse(text = code), env)
    expect_identical(reordered$factor_column_order, unname(relabel[order]))
    expect_identical(colnames(reordered$pattern), unname(relabel[order]))
    expect_identical(unname(reordered$pattern), unname(pattern))
    expect_identical(rownames(reordered$Phi), unname(relabel[order]))
    expect_equal(reordered$structure, reordered$pattern %*% reordered$Phi,
      tolerance = 1e-12, ignore_attr = TRUE)

    # Orthogonal output (no factor correlation rows) has an identity Phi
    # with the same real factor labels.
    std_factor_cov <- modified$op == "~~" & modified$lhs %in% relabel & modified$rhs %in% relabel & modified$lhs != modified$rhs
    env$efa_fit <- modified[!std_factor_cov, , drop = FALSE]
    orthogonal <- eval(parse(text = code), env)
    expect_identical(rownames(orthogonal$Phi), unname(relabel[order]))
    expect_equal(unname(orthogonal$Phi), diag(3L))
    expect_identical(orthogonal$structure, orthogonal$pattern)
    log_before <- shiny::isolate(state$codelog)
    output$factor_order_pattern
    output$factor_order_structure
    output$factor_order_phi
    expect_identical(shiny::isolate(state$codelog), log_before)
  })
})

test_that("the number of factors cue reads the kept comparison and distinguishes changed requests", {
  state <- efa_order_state()
  local_mocked_bindings(
    sieve_efa_dmar_start = function(...) stop("A status cue must not start a worker"),
    sieve_retention_cor = function(...) stop("A status cue must not estimate a matrix")
  )
  shiny::testServer(mod_efa_server, args = list(state = state), {
    session$setInputs(n_factors = 3L, estimator = "mlr", count_to = 4L, count_criterion = "BIC")
    expect_identical(shiny::isolate(state$page_results$efa[[session$ns("factor_count")]]$state), "not_run")
    key <- count_key()
    count_kept$key <- key
    count_kept$last_key <- key
    count_kept$value <- list(status = "ran", key = key)
    count_tick(shiny::isolate(count_tick()) + 1L)
    session$flushReact()
    expect_identical(shiny::isolate(state$page_results$efa[[session$ns("factor_count")]]$state), "current")
    # A current comparison puts nothing about it in the page's status line.
    expect_false(grepl("number of factors comparison", efa_order_text(output$status_line), fixed = TRUE))
    session$setInputs(estimator = "ml")
    expect_identical(shiny::isolate(state$page_results$efa[[session$ns("factor_count")]]$state), "stale")
    expect_match(efa_order_text(output$status_line), "number of factors comparison was computed", fixed = TRUE)
    expect_match(efa_order_text(output$status_line), "Update (", fixed = TRUE)
    expect_null(count_job$process)
  })
})

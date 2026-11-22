test_that("a long network search waits for explicit approval of the current scope", {
  skip_if_not_installed("psychonetrics")
  test_frame <- environment()
  dialogs <- character()
  calls <- 0L
  columns <- character()
  local_mocked_bindings(
    sieve_network_wait = function(...) 901,
    sieve_network_estimate = function(data, ...) {
      calls <<- calls + 1L
      columns <<- names(data)
      list(error = "Counted network fixture", warnings = character())
    }, .env = test_frame)
  local_mocked_bindings(showModal = function(ui, ...) {
    dialogs <<- c(dialogs, as.character(ui))
  }, .package = "shiny", .env = test_frame)
  state <- siever_test_study()
  shiny::testServer(mod_network_server,
    args = list(state = state, start = sieve_network_inline_start), {
    session$setInputs(estimator = "study", scope = "all", ggm_go = 1L)
    expect_identical(calls, 0L)
    expect_true(model_plan("ggm")()$confirm_long)
    expect_match(tail(dialogs, 1L), sieve_duration_label(901), fixed = TRUE)
    html <- tail(dialogs, 1L)
    expect_lt(regexpr("Cancel", html)[1L], regexpr("Start This Search", html)[1L])
    # An approval for the previous pool cannot start the newly selected one.
    session$setInputs(scope = "construct:Visual")
    session$setInputs(ggm_go_confirm = 1L)
    expect_identical(calls, 0L)
    expect_length(active_items(), 3L)
    session$setInputs(ggm_go = 2L)
    expect_identical(calls, 0L)
    session$setInputs(ggm_go_confirm = 2L)
    expect_identical(calls, 1L)
    expect_setequal(columns, active_items())
    expect_match(shown("ggm")$error, "Counted network fixture", fixed = TRUE)
  })
})

test_that("ordered network cross-table cautions appear before any search", {
  skip_if_not_installed("psychonetrics")
  state <- siever_test_study(configure = function(s) {
    s$items <- as.data.frame(lapply(s$items, function(x) as.numeric(x > stats::median(x))))
    s$items[[2L]] <- s$items[[1L]]
    s$raw[names(s$items)] <- s$items
    m <- s$meta
    m$type <- "binary"
    s$meta <- m
  })
  shiny::testServer(mod_network_server,
    args = list(state = state, start = sieve_network_inline_start), {
    session$setInputs(estimator = "study")
    note <- model_plan("ggm")()$dwls_note
    expect_match(note, "empty cells", fixed = TRUE)
    expect_match(note, "ML on standardized item codes", fixed = TRUE)
    expect_match(as.character(output$ggm_line$html), "empty cells", fixed = TRUE)
    expect_null(shown("ggm"))
  })
})

test_that("the optional Network package notice names the package and installation action", {
  test_frame <- environment()
  have <- sieve_have
  local_mocked_bindings(sieve_have = function(package) {
    if (identical(package, "psychonetrics")) FALSE else have(package)
  }, .env = test_frame)
  state <- siever_test_study()
  shiny::testServer(mod_network_server,
    args = list(state = state, start = sieve_network_inline_start), {
    session$flushReact()
    note <- as.character(output$gate$html)
    expect_match(note, "psychonetrics", fixed = TRUE)
    expect_match(note, "install.packages", fixed = TRUE)
    expect_null(shown("ggm"))
  })
})

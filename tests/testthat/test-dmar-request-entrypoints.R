test_that("FMI requests keep their counts and refuse before touching the engine or RNG", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_mocked_bindings(fraction_missing_information = function(...) {
    calls$n <- calls$n + 1L
    stop("The FMI engine must not run for an invalid request")
  }, .package = "DMAR")
  state <- sieve_new_state()
  model <- list(fit = list(token = "unneeded for preflight"))
  parent <- shiny::reactive(model)
  current <- shiny::reactive(TRUE)
  withr::local_seed(781)
  server <- function(input, output, session) {
    sieve_missing_info_server(input, output, session, state, parent, current)
  }
  shiny::testServer(server, {
      before <- .Random.seed
      session$setInputs(missing_B = 399L, missing_level = .95,
        missing_seed = NA_real_, missing_ci = 1L)
      expect_identical(input$missing_B, 399L)
      expect_match(output$missing_info_note$html, "at least 400", fixed = TRUE)
      expect_identical(calls$n, 0L)
      expect_identical(.Random.seed, before)
      session$setInputs(missing_B = 400L, missing_level = .99, missing_ci = 2L)
      expect_identical(input$missing_B, 400L)
      expect_match(output$missing_info_note$html, "at least 2000", fixed = TRUE)
      expect_identical(calls$n, 0L)
      session$setInputs(missing_B = 400.5, missing_level = .95, missing_ci = 3L)
      expect_identical(input$missing_B, 400.5)
      expect_match(output$missing_info_note$html, "whole number", fixed = TRUE)
      expect_identical(calls$n, 0L)
    })
})

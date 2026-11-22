# Empty the store of measured rates.
sieve_rate_reset <- function() {
  rm(list = ls(envir = SIEVER:::sieve_rates, all.names = TRUE), envir = SIEVER:::sieve_rates)
  invisible(NULL)
}

# SIEVER's own recorder of measured rates (setup-rates.R replaces it for
# the test run).
siever_rate_note_measured <- get("sieve_rate_note", envir = asNamespace("SIEVER"))

# Records measured rates as SIEVER does for the rest of the calling test,
# from an empty store that is emptied again when the test ends. Call it in
# the test_that() body, not in a shiny::testServer() block, whose frame
# never ends.
local_measured_rates <- function(envir = parent.frame()) {
  sieve_rate_reset()
  withr::defer(sieve_rate_reset(), envir = envir)
  testthat::local_mocked_bindings(sieve_rate_note = siever_rate_note_measured,
    .package = "SIEVER", .env = envir)
  invisible(NULL)
}

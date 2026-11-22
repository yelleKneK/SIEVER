# The CFA tab runs DMAR's Bollen-Stine bootstrap in a background process
# (sieve_bollen_stine_start()) and looks at it on a timer, so inside
# shiny::testServer() a test advances the mock clock while real time
# passes until the run has ended (or the deadline is reached).
bs_wait_done <- function(session, running, timeout = 180) {
  deadline <- Sys.time() + timeout
  while (isTRUE(shiny::isolate(running())) && Sys.time() < deadline) {
    Sys.sleep(0.05)
    session$elapse(250)
  }
  testthat::expect_false(isTRUE(shiny::isolate(running())))
  invisible(NULL)
}

# Checks of wall-clock time (an estimate of a run's seconds against the
# seconds measured, or a step that must finish within a stated time) hold on
# an idle machine, and a busy one makes any of them fail without a fault in
# SIEVER. They run when the environment variable SIEVER_TIMING_TESTS is
# true; set it on an idle machine to run them.
siever_timing_checks <- function() {
  isTRUE(as.logical(Sys.getenv("SIEVER_TIMING_TESTS")))
}

# Skips the rest of a test, from the first check of wall-clock time on.
skip_unless_timing <- function() {
  testthat::skip_if_not(siever_timing_checks(),
    "wall-clock checks run when SIEVER_TIMING_TESTS is true, on an idle machine")
}

# Waits until `done()` is TRUE, calling `step()` between looks (by default a
# pause of 0.05 seconds; a test of a Shiny module passes the step that lets
# the session's observers run). The limit allows for a busy machine. A
# process still running at the limit fails here, saying what was running
# and for how long, rather than in the expectations on its results.
# Returns TRUE when done, FALSE (after the failure) when not. With
# stop_on_timeout = TRUE a timeout ends the test with that one message, for
# a caller whose every later expectation reads the finished result.
siever_wait_until <- function(done, what = "The background process", seconds = 600,
                              step = function() Sys.sleep(0.05), stop_on_timeout = FALSE) {
  until <- proc.time()[["elapsed"]] + seconds
  repeat {
    if (isTRUE(done())) return(invisible(TRUE))
    if (proc.time()[["elapsed"]] >= until) break
    step()
  }
  message <- sprintf("%s was still running after %d seconds.", what, as.integer(seconds))
  if (isTRUE(stop_on_timeout)) stop(message, call. = FALSE)
  testthat::fail(message)
  invisible(FALSE)
}

# Drain the actual subprocess queue in tests that inspect the completed
# comparison table. Production uses Shiny's timer, never this waiting loop.
efa_finish_comparisons <- function(jobs, tick, session, timeout = 30) {
  deadline <- Sys.time() + timeout
  while ((!is.null(jobs$process) || length(jobs$pending)) && Sys.time() < deadline) {
    sieve_efa_jobs_poll(jobs)
    Sys.sleep(0.02)
  }
  if (!is.null(jobs$process) || length(jobs$pending)) stop("EFA comparison worker did not finish within the test deadline.")
  tick(shiny::isolate(tick()) + 1L)
  session$flushReact()
  invisible(NULL)
}

# A stand-in for the EFA page's DMAR background process
# (sieve_efa_dmar_start()): it runs the DMAR call in this process with the
# production runner (sieve_efa_dmar_run()) when its result is read, and
# counts the calls in `log`. `alive` keeps it "running" until the test says
# otherwise, for progress, Cancel and stale-request checks.
efa_dmar_fake_start <- function(log = new.env(parent = emptyenv()), alive = FALSE) {
  log$calls <- list()
  log$killed <- 0L
  function(call, rng_kind, runner = "sieve_efa_dmar_run") {
    log$calls <- c(log$calls, list(call))
    log$runners <- c(log$runners, runner)
    p <- new.env(parent = emptyenv())
    p$alive <- alive
    p$is_alive <- function() p$alive
    p$get_pid <- function() NA_integer_
    p$kill <- function() {
      p$alive <- FALSE
      log$killed <- log$killed + 1L
      invisible(TRUE)
    }
    # The runner the real worker would call (the Dimensionality page's
    # parallel analysis runs several DMAR calls through sieve_pa_dmar_run).
    p$get_result <- function() utils::getFromNamespace(runner, "SIEVER")(call, rng_kind = rng_kind)
    log$process <- p
    p
  }
}

# Let the module's poller collect a finished DMAR job and redraw.
efa_dmar_settle <- function(session, rounds = 4L) {
  for (i in seq_len(rounds)) {
    session$elapse(300)
    session$flushReact()
  }
  invisible(NULL)
}

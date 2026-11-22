# Real background processes: started, canceled, and collected when their
# session or parent ends. Each test skips on CRAN; the fast queue tests in
# test-efa-background.R run everywhere.
# This file starts from the standard timing rates: rates measured by earlier
# files (slower on a busy machine) would move the EFA page's fits into a
# background process and leave their results pending when a test reads them.
sieve_rate_reset()

# Waits a short time for a process that is being stopped (canceled, or
# collected when its session or parent ends). The children here end by
# themselves after 10 or 30 seconds, so a longer wait could pass without the
# stop. A wait for a process to finish its work uses siever_wait_until(),
# whose limit allows for a busy machine.
life_wait <- function(test, seconds = 8) {
  deadline <- Sys.time() + seconds
  while (!isTRUE(test()) && Sys.time() < deadline) Sys.sleep(.05)
  isTRUE(test())
}
life_pid_alive <- function(pid) siever_test_pid_alive(pid)
life_child <- function(k, delay = .15, fail = FALSE) {
  p <- callr::r_bg(function(k, delay, fail) {
    Sys.sleep(delay)
    if (fail) stop('Controlled lifecycle worker error')
    data.frame(k = k, status = 'ok', message = 'Controlled completed worker')
  }, args = list(k = k, delay = delay, fail = fail), supervise = TRUE, stdout = '|', stderr = '|', user_profile = FALSE)
  p
}

test_that('installed EFA workers finish and repeated cancellation collects owned processes', {
  skip_on_cran()
  # A production worker, not only an injected successful process.
  d <- lavaan::HolzingerSwineford1939[, paste0('x', 1:9)]
  key <- list(estimator = 'MLR', ordered = NULL, rotation = 'none', missing = 'fiml', ks = 2L)
  p <- sieve_efa_worker_start(d, key, 2L)
  withr::defer({
    if (p$is_alive()) p$kill()
    p$wait(2000)
  })
  siever_wait_until(function() !p$is_alive(), "The EFA worker")
  expect_identical(p$get_result()$k, 2L)
  expect_false(life_pid_alive(p$get_pid()))
  for (i in 1:3) {
    jobs <- sieve_efa_jobs(function(data, key, k) life_child(k, delay = 10))
    sieve_efa_jobs_submit(jobs, list(data = i, ks = 1L), data.frame(x = 1:10), 1L)
    child <- jobs$process
    withr::defer({
      if (child$is_alive()) child$kill()
      child$wait(2000)
    })
    pid <- child$get_pid()
    sieve_efa_jobs_cancel(jobs)
    expect_true(life_wait(function() !child$is_alive()))
    expect_false(life_pid_alive(pid))
    expect_null(jobs$process)
    expect_length(jobs$pending, 0L)
    expect_length(jobs$cache, 0L)
  }
})

test_that('real worker errors and superseded requests cannot supply current results', {
  skip_on_cran()
  jobs <- sieve_efa_jobs(function(data, key, k) life_child(k, fail = identical(key$data, 'error')))
  sieve_efa_jobs_submit(jobs, list(data = 'error', ks = 1L), data.frame(x = 1:10), 1L)
  error_child <- jobs$process
  withr::defer({
    if (error_child$is_alive()) error_child$kill()
    error_child$wait(2000)
  })
  siever_wait_until(function() {
    sieve_efa_jobs_poll(jobs)
    is.null(jobs$process)
  }, "The failing worker")
  expect_identical(jobs$failed, 1L)
  expect_match(jobs$errors, 'Controlled lifecycle worker error', fixed = TRUE)
  oldkey <- list(data = 'old', ks = 2L)
  sieve_efa_jobs_submit(jobs, oldkey, data.frame(x = 1:10), 2L)
  old <- jobs$process
  withr::defer({
    if (old$is_alive()) old$kill()
    old$wait(2000)
  })
  newkey <- list(data = 'new', ks = 3L)
  sieve_efa_jobs_submit(jobs, newkey, data.frame(x = 2:11), 3L)
  current <- jobs$process
  withr::defer({
    if (current$is_alive()) current$kill()
    current$wait(2000)
  })
  siever_wait_until(function() {
    sieve_efa_jobs_poll(jobs)
    is.null(jobs$process)
  }, "The current worker")
  expect_identical(jobs$key, newkey)
  expect_identical(jobs$completed, 3L)
  expect_null(sieve_efa_job_cache(jobs, newkey, 2L))
  expect_false(life_pid_alive(old$get_pid()))
})

test_that('closing a Shiny session terminates its real comparison child', {
  skip_on_cran()
  seen <- new.env(parent = emptyenv())
  seen$child <- NULL
  # The EFA page's number of factors comparison (DMAR::factor_count) runs
  # in a real child; closing the session stops it.
  local_mocked_bindings(sieve_efa_dmar_start = function(call, rng_kind) {
    seen$child <- life_child(1L, delay = 10)
    seen$child
  })
  fx <- sieve_hs9_fixture()
  s <- sieve_new_state()
  s$raw <- fx$data
  s$items <- fx$data[, fx$item_cols, drop = FALSE]
  s$meta <- sieve_build_meta(fx$data, fx$item_cols, fx$theory_map, labels = fx$labels)
  s$confirmed <- TRUE
  withr::defer(if (!is.null(seen$child)) {
    if (seen$child$is_alive()) seen$child$kill()
    seen$child$wait(2000)
  })
  shiny::testServer(mod_efa_server, args = list(state = s), {
    session$setInputs(n_factors = 3, count_to = 3, count_criterion = 'BIC', efa_go = 1L, count_go = 1L)
    count_run()
    expect_false(is.null(seen$child))
    expect_true(sieve_efa_dmar_active(count_job))
    session$close()
  })
  expect_true(life_wait(function() !seen$child$is_alive()))
  expect_false(life_pid_alive(seen$child$get_pid()))
})

test_that('a supervised child is collected after its owning R parent exits', {
  skip_on_cran()
  marker <- tempfile(fileext = '.pid')
  on.exit(unlink(marker), add = TRUE)
  owner <- callr::r_bg(function(marker) {
    child <- callr::r_bg(function() {
      Sys.sleep(30)
    }, supervise = TRUE, user_profile = FALSE)
    writeLines(as.character(child$get_pid()), marker)
    # Normal parent shutdown exercises callr's own supervisor/handle cleanup.
    invisible(NULL)
  }, args = list(marker = marker), supervise = TRUE, stdout = '|', stderr = '|', user_profile = FALSE)
  withr::defer({
    if (owner$is_alive()) owner$kill()
    owner$wait(2000)
  })
  siever_wait_until(function() file.exists(marker), "The owning R process, before writing its child's PID,")
  pid <- as.integer(readLines(marker))
  marked <- proc.time()[["elapsed"]]
  siever_wait_until(function() !owner$is_alive(), "The owning R process")
  expect_identical(owner$get_exit_status(), 0L)
  # The child ends by itself 30 seconds after it starts, so on a machine
  # slow enough that the owner took 20 seconds or more to exit, the child's
  # end would not show that the owner's exit collected it.
  exited_after <- proc.time()[["elapsed"]] - marked
  if (exited_after >= 20) {
    skip(sprintf("the owning R process took %.0f seconds to exit, too close to its child's own end", exited_after))
  }
  expect_true(life_wait(function() !life_pid_alive(pid)))
  stderr <- paste(owner$read_all_error(), collapse = '\n')
  expect_false(grepl('unable to terminate some child processes|Error while shutting down parallel', stderr))
})

test_that('forced termination of a parent collects its supervised comparison child', {
  skip_on_cran()
  marker <- tempfile(fileext = '.pid')
  stderr_file <- tempfile(fileext = '.log')
  on.exit(unlink(c(marker, stderr_file)), add = TRUE)
  owner <- callr::r_bg(function(marker) {
    child <- callr::r_bg(function() {
      Sys.sleep(30)
    }, supervise = TRUE, user_profile = FALSE)
    writeLines(as.character(child$get_pid()), marker)
    Sys.sleep(30)
  }, args = list(marker = marker), supervise = TRUE, stdout = '|', stderr = stderr_file, user_profile = FALSE)
  withr::defer({
    if (owner$is_alive()) owner$kill()
    owner$wait(2000)
  })
  siever_wait_until(function() file.exists(marker) && file.info(marker)$size > 0,
    "The owning R process, before writing its child's PID,")
  pid <- as.integer(readLines(marker))
  expect_true(life_pid_alive(pid))
  cat('Owned parent PID', owner$get_pid(), 'supervised child PID', pid, 'before forced termination.\n')
  owner$kill()
  owner$wait(2000)
  expect_false(owner$is_alive())
  expect_true(life_wait(function() !life_pid_alive(pid)))
  stderr <- paste(readLines(stderr_file, warn = FALSE), collapse = '\n')
  expect_false(grepl('unable to terminate some child processes|Error while shutting down parallel', stderr))
})

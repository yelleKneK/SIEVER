# fct_dmar_boot.R
# DMAR's counted bootstraps, run for the Reliability and Bifactor tabs.
#
# Every DMAR function that runs its counted bootstrap engine
# (reliability_omega(), reliability_omega_categorical(), reliability_alpha(),
# ci_bifactor_indices(), and others) counts every replication's fate, keeps
# its estimate, and takes a progress callback, `progress(done, total)`,
# called after each replication (or each block of replications on several
# processes); returning FALSE cancels the call with an error of class
# dmar_bootstrap_cancelled, and an error inside the callback comes back with
# class dmar_progress_error. SIEVER's own counted engines
# (fct_reliability_boot.R, fct_bifactor_boot.R) serve one design only,
# declared clusters, which DMAR's engine does not resample (it resamples
# independent rows).
#
# A bootstrap that runs in the Shiny process blocks the interface: a Stop
# click cannot be processed while R is busy. So the DMAR calls of a run are
# made in one background R process (callr), the same kind of process the EFA
# page uses for its fits (fct_efa_jobs.R). The callback writes how far the
# run has gone to a small file in the job's folder, which the page polls,
# and returns FALSE once the page has written a stop file there, so DMAR
# cancels at its next check: after the replication in progress on one
# process, or after the block of replications in progress on several
# (about B/50 replications, rounded up to a multiple of `ncpus`; DMAR calls
# the callback only between blocks then). If DMAR has not stopped within a
# grace period, the page ends the process. Each finished call is written to
# the folder as soon as it returns, so a stop keeps the calls completed
# before it.
#
# Forking. With `ncpus > 1` DMAR spreads the replications over forked
# workers (parallel::mclapply; a socket cluster on Windows). Whether forking
# is safe is a property of the process that forks, so it is decided there.
# The pages ask for SIEVER's workers (`sieve_dmar_boot_cores()`), and the
# background process applies the check of `sieve_dmar_fork_safe()` to
# itself before each call: it is a fresh R that has loaded DMAR and its
# dependencies and has started no child through processx, so processx has
# not replaced the SIGCHLD handler that parallel installs at its first fork
# (the cause of R's "cannot wait for child" message at shutdown), and it
# forks. The Shiny process itself never forks; it
# loads processx when it starts its first background process (callr), so
# the check made there would answer for the wrong process. The functions
# that run DMAR in the calling process (sieve_reliability_omega(),
# sieve_bifactor_bootstrap()) apply the check to that process
# (`sieve_dmar_boot_ncpus()`): where processx has been loaded and the
# documented switch PROCESSX_NOTIFY_OLD_SIGCHLD is not set, the call runs
# on one process instead. DMAR draws every resample before any replication
# runs, so a given seed returns the same limits, counts and replicates
# whatever `ncpus` is (checked in the tests); the processes DMAR used are
# read from its result and recorded in the call.

#' Whether this R process may fork DMAR's bootstrap workers
#'
#' Forking (parallel::mclapply, which DMAR uses with `ncpus > 1` outside
#' Windows) is safe unless processx is loaded without the switch
#' `PROCESSX_NOTIFY_OLD_SIGCHLD`, in which case processx's SIGCHLD handler
#' may replace parallel's and the forked workers are not reaped. A socket
#' cluster (Windows) does not fork.
#'
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_dmar_fork_safe <- function() {
  if (identical(.Platform$OS.type, "windows")) {
    return(TRUE)
  }
  !("processx" %in% loadedNamespaces()) || nzchar(Sys.getenv("PROCESSX_NOTIFY_OLD_SIGCHLD"))
}

#' How many processes DMAR's bootstrap uses in this process
#'
#' The workers SIEVER's parallel work uses (`sieve_cores()`: the cores this
#' computer reports less one, at most eight, or the option `sieve.cores` /
#' `SIEVER_CORES`), or one where forking this process would not be safe
#' (see `sieve_dmar_fork_safe()`). For DMAR calls made in this process; the
#' pages' calls run in a background process, which asks for
#' `sieve_dmar_boot_cores()` and applies the check to itself.
#'
#' @param cores Processes wanted.
#' @return A single integer, at least 1.
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_ncpus <- function(cores = sieve_cores()) {
  cores <- sieve_dmar_boot_cores(cores)
  if (cores < 2L || !sieve_dmar_fork_safe()) {
    return(1L)
  }
  cores
}

#' How many processes the pages ask DMAR's bootstrap to use
#'
#' The workers SIEVER's parallel work uses (`sieve_cores()`), for a DMAR
#' call run in the background process (`sieve_dmar_boot_start()`). Whether
#' forking is safe is decided in that process, which forks, not in this one:
#' this process loads processx when it starts its first background process,
#' so a check made here would put every run after the first on one process
#' (94.5 s in place of 23.4 s for the same run). The background process falls back to one
#' process only if its own check fails, and the processes DMAR used are read
#' from its result.
#'
#' @param cores Processes wanted.
#' @return A single integer, at least 1.
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_cores <- function(cores = sieve_cores()) {
  cores <- suppressWarnings(as.integer(cores))
  if (length(cores) != 1L || !is.finite(cores) || cores < 2L) {
    return(1L)
  }
  cores
}

#' The processes DMAR used, in words
#'
#' @param ncpus The processes DMAR used (its `bootstrap` attribute's
#'   `ncpus`), one value per call; missing values are ignored.
#' @return `"1 process"`, `"8 processes"`, `"1 and 8 processes"`, or `NULL`
#'   when none is known.
#' @author Ken Kelley
#' @noRd
sieve_dmar_process_text <- function(ncpus) {
  ncpus <- sort(unique(suppressWarnings(as.integer(unlist(ncpus, use.names = FALSE)))))
  ncpus <- ncpus[is.finite(ncpus)]
  if (!length(ncpus)) {
    return(NULL)
  }
  sprintf("%s process%s", paste(ncpus, collapse = " and "), if (identical(ncpus, 1L)) "" else "es")
}

#' Run one DMAR bootstrap call and keep what happened
#'
#' Runs `DMAR::<call$fun>(<call$args>)` with an optional progress callback,
#' keeping the result, the warnings and messages DMAR gave, and, when the
#' call stopped, its condition with its classes and fields (`reason`,
#' `done`, `total`, ...), so the page can tell a cancelled run
#' (`dmar_bootstrap_cancelled`), a failing callback (`dmar_progress_error`)
#' and one of DMAR's refusals (`dmar_refusal` and its subclasses) apart by
#' class. The function uses base R only and reaches DMAR by name, so it runs
#' unchanged in the background process, which does not load SIEVER.
#'
#' @param call A list with `fun` (the name of a DMAR export) and `args` (its
#'   arguments, the data or fit among them).
#' @param progress `NULL` or a function `progress(done, total)`.
#' @param fun The function to call, or `NULL` for the DMAR export named by
#'   `call$fun` (in this process the Reliability tab passes
#'   `sieve_reliability_engine(call$fun)`, the indirection its tests use).
#' @return A list with `ok`, `value` (the DMAR result, or `NULL`),
#'   `condition` (the error condition without its call, or `NULL`),
#'   `error` (its message), `error_class`, `warnings`, `messages`, and
#'   `seconds`.
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_run <- function(call, progress = NULL, fun = NULL) {
  seen <- new.env(parent = emptyenv())
  seen$warnings <- character(0)
  seen$messages <- character(0)
  args <- call$args
  if (!is.null(progress)) args$progress <- progress
  if (is.null(fun)) fun <- getExportedValue("DMAR", call$fun)
  started <- proc.time()[["elapsed"]]
  value <- tryCatch(
    withCallingHandlers(
      do.call(fun, args),
      warning = function(w) {
        seen$warnings <- c(seen$warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        seen$messages <- c(seen$messages, sub("\n$", "", conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) e
  )
  seconds <- proc.time()[["elapsed"]] - started
  failed <- inherits(value, "error")
  condition <- NULL
  if (failed) {
    condition <- value
    condition$call <- NULL
    if (inherits(condition$parent, "condition")) condition$parent$call <- NULL
  }
  list(
    ok = !failed, value = if (failed) NULL else value, condition = condition,
    error = if (failed) conditionMessage(condition) else NULL,
    error_class = if (failed) class(condition) else NULL,
    warnings = unique(seen$warnings), messages = unique(seen$messages),
    seconds = seconds
  )
}

#' What a DMAR bootstrap call's outcome was
#'
#' Read by condition class, never by the words of the message.
#'
#' @param run A result of `sieve_dmar_boot_run()`.
#' @return One of `"ok"`, `"cancelled"` (the progress callback returned
#'   `FALSE`; class `dmar_bootstrap_cancelled`), `"progress_error"` (the
#'   callback failed; class `dmar_progress_error`), `"refused"` (one of
#'   DMAR's refusals; class `dmar_refusal`), or `"error"`.
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_outcome <- function(run) {
  if (isTRUE(run$ok)) {
    return("ok")
  }
  cls <- run$error_class %||% class(run$condition)
  if ("dmar_bootstrap_cancelled" %in% cls) {
    "cancelled"
  } else if ("dmar_progress_error" %in% cls) {
    "progress_error"
  } else if ("dmar_refusal" %in% cls) {
    "refused"
  } else {
    "error"
  }
}

#' Why a DMAR bootstrap call returned no interval, in the interface's words
#'
#' @param run A result of `sieve_dmar_boot_run()` that did not succeed.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_failure_text <- function(run) {
  cond <- run$condition
  switch(sieve_dmar_boot_outcome(run),
    cancelled = sprintf(paste0(
      "The bootstrap was canceled after %d of %d replications. DMAR forms no interval from a ",
      "canceled run (its limits and counts are those of all B replications), so none is shown; ",
      "run it again to completion."
    ), as.integer(cond$done %||% NA_integer_), as.integer(cond$total %||% NA_integer_)),
    progress_error = paste(
      "The bootstrap was stopped because SIEVER's progress display failed:",
      conditionMessage(cond$parent %||% cond)
    ),
    refused = paste0(
      "DMAR refused the bootstrap",
      if (length(cond$reason) == 1L && !is.na(cond$reason)) sprintf(" (%s)", cond$reason) else "",
      ": ", gsub("\\s+", " ", run$error)
    ),
    paste("DMAR's bootstrap stopped with an error:", gsub("\\s+", " ", run$error))
  )
}

#' Start DMAR bootstrap calls in a background R process
#'
#' One background R process (callr) runs the calls in order. It loads the
#' DMAR the app has loaded (the same path, so the background result is the
#' one this process would compute), not SIEVER. For each call the progress
#' callback writes `call done total` to the file `progress` in the job's
#' folder, at most about five times a second and at the last replication,
#' and returns `FALSE` (so DMAR cancels) once the file `stop` exists there.
#' Before each call the process applies `sieve_dmar_fork_safe()` to itself
#' and, if it may not fork, asks DMAR for one process in place of the
#' call's `ncpus`. Each finished call is written to `call<i>.rds` as soon as
#' it returns, with the `ncpus` it was run with, whether forking was safe,
#' and whether processx was loaded in that process.
#'
#' @param calls A list of calls, each a list with `fun` and `args` (see
#'   `sieve_dmar_boot_run()`); `args$ncpus` is the number of processes
#'   asked for (`sieve_dmar_boot_cores()`).
#' @return A job (see `sieve_dmar_boot_handle()`).
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_start <- function(calls) {
  dir <- tempfile("siever_boot_")
  dir.create(dir)
  paths <- sieve_worker_paths()["DMAR"]
  bootstrap <- sieve_worker_bootstrap
  environment(bootstrap) <- baseenv()
  runner <- sieve_dmar_boot_run
  environment(runner) <- baseenv()
  fork_safe <- sieve_dmar_fork_safe
  environment(fork_safe) <- baseenv()
  child <- function(calls, paths, bootstrap, runner, fork_safe, dir) {
    bootstrap(paths)
    stop_file <- file.path(dir, "stop")
    progress_file <- file.path(dir, "progress")
    write_atomic <- function(lines, file) {
      part <- paste0(file, ".part")
      writeLines(lines, part)
      file.rename(part, file)
    }
    tick <- new.env(parent = emptyenv())
    for (i in seq_along(calls)) {
      if (file.exists(stop_file)) break
      tick$last <- -Inf
      progress <- function(done, total) {
        now <- proc.time()[["elapsed"]]
        if (now - tick$last >= 0.2 || done >= total) {
          write_atomic(sprintf("%d %d %d", i, as.integer(done), as.integer(total)), progress_file)
          tick$last <- now
        }
        !file.exists(stop_file)
      }
      # Forking is decided here, in the process that forks.
      call <- calls[[i]]
      safe <- isTRUE(fork_safe())
      if (!safe && !is.null(call$args$ncpus)) call$args$ncpus <- 1L
      run <- runner(call, progress = progress)
      run$ncpus <- call$args$ncpus
      run$fork_safe <- safe
      run$processx_loaded <- "processx" %in% loadedNamespaces()
      file <- file.path(dir, sprintf("call%04d.rds", i))
      saveRDS(run, paste0(file, ".part"))
      file.rename(paste0(file, ".part"), file)
      if ("dmar_bootstrap_cancelled" %in% run$error_class) break
    }
    invisible(TRUE)
  }
  environment(child) <- baseenv()
  process <- tryCatch(
    callr::r_bg(child,
      args = list(calls = calls, paths = paths, bootstrap = bootstrap, runner = runner, fork_safe = fork_safe, dir = dir),
      libpath = .libPaths(), supervise = TRUE, user_profile = FALSE,
      stdout = file.path(dir, "stdout.txt"), stderr = file.path(dir, "stderr.txt")
    ),
    error = function(e) {
      unlink(dir, recursive = TRUE)
      stop(e)
    }
  )
  sieve_dmar_boot_handle(process, dir, length(calls))
}

#' A handle on one background DMAR bootstrap process
#'
#' @param process The callr process.
#' @param dir The job's folder.
#' @param n The number of calls.
#' @return An environment with `process`, `dir`, `n`, `results` (the calls
#'   finished so far, by position), `progress` (`call`, `done`, `total`),
#'   `stop_requested` (the time Stop was pressed, or `NA`), and the
#'   functions `poll()` (read the progress and any finished calls; returns
#'   `TRUE` while the process runs), `stop()` (ask DMAR to cancel at its
#'   next check: the next replication, or the end of the block in progress
#'   on several processes), `kill()` (end the process and remove the folder),
#'   `finished()`, and `failure()` (why the process ended without its
#'   results, or `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_handle <- function(process, dir, n) {
  job <- new.env(parent = emptyenv())
  job$process <- process
  job$dir <- dir
  job$n <- as.integer(n)
  job$results <- vector("list", n)
  job$progress <- list(call = 1L, done = 0L, total = NA_integer_)
  job$stop_requested <- NA_real_
  job$killed <- FALSE
  read_results <- function() {
    for (i in seq_len(job$n)) {
      if (!is.null(job$results[[i]])) next
      file <- file.path(dir, sprintf("call%04d.rds", i))
      if (file.exists(file)) {
        got <- tryCatch(readRDS(file), error = function(e) NULL)
        if (!is.null(got)) job$results[[i]] <- got
      }
    }
  }
  job$poll <- function() {
    alive <- tryCatch(process$is_alive(), error = function(e) FALSE)
    file <- file.path(dir, "progress")
    if (file.exists(file)) {
      line <- tryCatch(readLines(file, warn = FALSE), error = function(e) character(0))
      parts <- suppressWarnings(as.integer(strsplit(line[1] %||% "", " ", fixed = TRUE)[[1]]))
      if (length(parts) == 3L && all(is.finite(parts))) {
        job$progress <- list(call = parts[1], done = parts[2], total = parts[3])
      }
    }
    read_results()
    alive
  }
  job$stop <- function() {
    if (is.na(job$stop_requested)) {
      job$stop_requested <- proc.time()[["elapsed"]]
      if (dir.exists(dir)) file.create(file.path(dir, "stop"))
    }
    invisible(TRUE)
  }
  job$kill <- function() {
    job$killed <- TRUE
    tryCatch(process$kill(), error = function(e) FALSE)
    unlink(dir, recursive = TRUE)
    invisible(TRUE)
  }
  job$finished <- function() {
    !isTRUE(tryCatch(process$is_alive(), error = function(e) FALSE))
  }
  job$failure <- function() {
    if (!job$finished() || isTRUE(job$killed)) {
      return(NULL)
    }
    read_results()
    if (!is.na(job$stop_requested)) {
      return(NULL)
    }
    if (all(!vapply(job$results, is.null, logical(1)))) {
      return(NULL)
    }
    err <- tryCatch({
      process$get_result()
      NULL
    }, error = function(e) conditionMessage(e))
    log <- file.path(dir, "stderr.txt")
    tail <- if (file.exists(log)) utils::tail(readLines(log, warn = FALSE), 3L) else character(0)
    paste(c("The background R process ended before returning every bootstrap.", err, tail), collapse = " ")
  }
  job$release <- function() {
    unlink(dir, recursive = TRUE)
    invisible(TRUE)
  }
  job
}

#' The seconds Stop waits for DMAR to cancel before ending the process
#'
#' DMAR cancels at its next check after its callback returns `FALSE`: after
#' the replication in progress on one process, after the block of
#' replications in progress on several. A replication that runs long (a
#' bifactor solution running away), or a long block, can delay that, so
#' after this many seconds the background process is ended.
#'
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_grace_seconds <- function() {
  5
}

#' The wait of DMAR's bootstrap in the background process
#'
#' The serial time of the replications divided over DMAR's processes, plus
#' the cost of forking them for each of DMAR's blocks (about fifty a call)
#' and this construct's share of starting the background process.
#'
#' @param serial The time of the fits in one process, in seconds.
#' @param ncpus DMAR's processes.
#' @param alongside The constructs (calls) that share the process.
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_wait <- function(serial, ncpus = 1L, alongside = 1L) {
  ncpus <- max(1L, as.integer(ncpus))
  alongside <- max(1L, suppressWarnings(as.integer(alongside)), na.rm = TRUE)
  blocks <- if (ncpus > 1L) 50 * sieve_rate("dmar_boot_block") else 0
  sieve_rate("dmar_boot_start") / alongside + serial / ncpus + blocks
}

#' The range of DMAR's wait on several processes
#'
#' With the progress hook DMAR runs the replications in blocks (about B/50,
#' rounded up to a multiple of `ncpus`) and waits for a block's slowest
#' replication before it reports progress and starts the next block. When
#' the replications take about the same time the wait is the serial time
#' divided over the processes (`sieve_dmar_boot_wait()`); when a few of them
#' are much slower (a bifactor resample whose refit fails or runs away), each
#' block waits for its slow one and most of the gain is lost. Measured on
#' a bifactor model with 5 of 400 replications failing: 60.3 s on one process, 52.1 s on eight with the
#' hook. The wait therefore lies between the even split and the time on one
#' process (plus the forks of the blocks).
#'
#' @param serial The time of the fits in one process, in seconds.
#' @param ncpus DMAR's processes.
#' @param alongside The constructs (calls) that share the process.
#' @return A numeric vector with `low` and `high`, in seconds (equal on one
#'   process).
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_wait_range <- function(serial, ncpus = 1L, alongside = 1L) {
  ncpus <- max(1L, as.integer(ncpus))
  low <- sieve_dmar_boot_wait(serial, ncpus, alongside)
  high <- if (ncpus > 1L) {
    sieve_dmar_boot_wait(serial, 1L, alongside) + 50 * sieve_rate("dmar_boot_block")
  } else {
    low
  }
  c(low = low, high = max(low, high))
}

#' A wait range in words
#'
#' @param range A result of `sieve_dmar_boot_wait_range()`.
#' @return `sieve_duration_label()` of the wait when the two ends read the
#'   same, otherwise "between ... and ...".
#' @author Ken Kelley
#' @noRd
sieve_dmar_wait_range_label <- function(range) {
  low <- sieve_duration_label(range[["low"]])
  high <- sieve_duration_label(range[["high"]])
  if (identical(low, high)) low else sprintf("between %s and %s", low, high)
}

#' A percentile limit by boot's rule
#'
#' The rule `boot::boot.ci(type = "perc")` applies (its internal
#' `norm.inter()`), which DMAR's percentile limits come from: with `R`
#' values, the limit at level `alpha` is the order statistic at position
#' `(R + 1) alpha` when that is a whole number, and otherwise an
#' interpolation between its neighbors on the normal quantile scale; a
#' position outside the sample takes the extreme value. Infinite values at
#' either end (the bounds of `sieve_dmar_percentile_range()`) give an
#' infinite limit where the position reaches them.
#'
#' @param sorted Values in increasing order (possibly with `-Inf` first or
#'   `Inf` last).
#' @param alpha The level, in (0, 1).
#' @return A single number.
#' @author Ken Kelley
#' @noRd
sieve_boot_quantile <- function(sorted, alpha) {
  R <- length(sorted)
  if (!R) {
    return(NA_real_)
  }
  rk <- (R + 1) * alpha
  k <- trunc(rk)
  if (k <= 0) {
    return(sorted[1L])
  }
  if (k >= R) {
    return(sorted[R])
  }
  if (k == rk) {
    return(sorted[k])
  }
  lo <- sorted[k]
  hi <- sorted[k + 1L]
  if (!is.finite(lo)) {
    return(lo)
  }
  if (!is.finite(hi)) {
    return(hi)
  }
  z <- stats::qnorm(alpha)
  z_lo <- stats::qnorm(k / (R + 1))
  z_hi <- stats::qnorm((k + 1) / (R + 1))
  lo + (z - z_lo) / (z_hi - z_lo) * (hi - lo)
}

#' How far DMAR's percentile limits could move had every replication counted
#'
#' The limits are quantiles of the replications used, so they condition on
#' the fit having returned a value. Had each replication set aside produced
#' one, the limits would be quantiles of all the replications attempted;
#' the two extreme cases (all of them below every value used, or all above)
#' bound where each limit could then lie, by the rule DMAR's limits follow
#' (`sieve_boot_quantile()`), cut to the range the coefficient can take.
#' With no replication set aside each bound is the limit itself.
#'
#' @param x The values used.
#' @param attempted The replications attempted.
#' @param conf_level Confidence level.
#' @param range The smallest and largest value the coefficient can take.
#' @return A list with `lower_min`, `lower_max`, `upper_min`, `upper_max`.
#' @author Ken Kelley
#' @noRd
sieve_dmar_percentile_range <- function(x, attempted, conf_level = 0.95, range = c(-Inf, Inf)) {
  x <- sort(x[is.finite(x)])
  aside <- max(0L, as.integer(attempted) - length(x))
  a <- (1 - conf_level) / 2
  clip <- function(v) min(max(v, range[1]), range[2])
  below <- c(rep(-Inf, aside), x)
  above <- c(x, rep(Inf, aside))
  list(
    lower_min = clip(sieve_boot_quantile(below, a)), lower_max = clip(sieve_boot_quantile(above, a)),
    upper_min = clip(sieve_boot_quantile(below, 1 - a)), upper_max = clip(sieve_boot_quantile(above, 1 - a))
  )
}

#' The fewest usable replications a percentile limit needs, by boot's rule
#'
#' boot places a limit at level `alpha` at position `(R + 1) alpha`; below
#' position one it can only repeat the smallest value (boot warns that
#' extreme order statistics were used), so a `C` level interval needs at
#' least `2 / (1 - C) - 1` values (39 at 95 percent). This is a requirement
#' of boot's arithmetic; `sieve_bootstrap_min_draws()` applies the same
#' count to SIEVER's own type 7 limits as a common floor.
#'
#' @param conf_level Confidence level.
#' @return A single integer.
#' @author Ken Kelley
#' @noRd
sieve_dmar_min_used <- function(conf_level = 0.95) {
  sieve_bootstrap_min_draws(conf_level)
}

#' The counted fates of DMAR's replications, in SIEVER's vocabulary
#'
#' Reads the `bootstrap` attribute of a DMAR bootstrap result (its `status`
#' factor and replicate values `t`, one per replication, and its counts)
#' into the counts, per-replication frame and reasons the Reliability and
#' Bifactor tabs show. A replication's outcome is `"used"` (a proper
#' solution whose value enters the interval), `"inadmissible"` (an improper
#' solution: kept when it still yields a value under `improper = "keep"`,
#' set aside when it yields none), `"failed"` (the refit raised an error,
#' among them DMAR's `dmar_no_maximum` refusals, which name the item whose
#' error variance runs off below zero), `"not_converged"` (set aside without
#' an error: the fit did not converge or returned no finite value), or
#' `"undetermined"` (categorical omega: a polychoric correlation was
#' undetermined in the resample). A proper replication whose value lies
#' outside `range` is counted as used but not admissible.
#'
#' @param bs The `bootstrap` attribute.
#' @param used_flags Whether each replication's value entered the
#'   interval (`NULL` reads it from `t` and `status`).
#' @param values The value of each replication that decides admissibility
#'   (`NULL` uses `bs$t`).
#' @param range The range of an admissible value.
#' @param words How each set-aside status is described.
#' @return A list with `counts`, `draws` (one row per replication: `draw`,
#'   `status`, `outcome`, `returned`, `converged`, `admissible`, `used`,
#'   `estimate`, `message`), and `reasons` (`outcome`, `message`, `draws`).
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_counts <- function(bs, used_flags = NULL, values = NULL, range = c(0, 1), words = NULL) {
  status <- as.character(bs$status)
  B <- length(status)
  t <- values %||% (if (is.matrix(bs$t)) rep(NA_real_, B) else as.numeric(bs$t))
  keep <- identical(bs$improper_rule, "keep")
  used <- used_flags %||% (is.finite(t) & (status == "proper" | (status == "improper" & keep)))
  inside <- is.finite(t) & t >= range[1] & t <= range[2]
  outcome <- ifelse(status == "proper", ifelse(used & !inside, "inadmissible", "used"),
    ifelse(status == "improper", "inadmissible",
      ifelse(status == "error", "failed",
        ifelse(status == "undetermined", "undetermined", "not_converged")
      )
    )
  )
  outcome[status == "proper" & !used] <- "not_converged"
  errors <- bs$error_messages %||% integer(0)
  error_text <- if (length(errors) == 1L) {
    names(errors)
  } else if (length(errors) > 1L) {
    "the refit raised an error (the messages and their counts are listed with the reasons)"
  } else {
    "the refit raised an error"
  }
  defaults <- c(
    failed = error_text,
    not_converged = "the fit did not converge or returned no finite value",
    inadmissible = "an improper solution that yields no value (a negative error variance)",
    undetermined = "a polychoric correlation was undetermined in the resample"
  )
  words <- c(words, defaults[setdiff(names(defaults), names(words))])
  message <- ifelse(used & outcome == "used", "", words[outcome])
  message[used & outcome == "inadmissible"] <- ifelse(status[used & outcome == "inadmissible"] == "improper",
    "an improper solution (a negative error variance) whose value was kept",
    "the value lies outside the range of the coefficient"
  )
  no_solution <- as.integer(bs$no_solution %||% 0L)
  frame <- data.frame(
    draw = seq_len(B), status = status, outcome = outcome,
    returned = status != "error", converged = status %in% c("proper", "improper"),
    admissible = status == "proper" & inside, used = used, estimate = t, message = unname(message),
    stringsAsFactors = FALSE
  )
  set_aside <- frame[!frame$used, , drop = FALSE]
  reasons <- if (nrow(set_aside)) {
    tally <- as.data.frame(table(outcome = set_aside$outcome, message = set_aside$message), stringsAsFactors = FALSE)
    tally <- tally[tally$Freq > 0L, , drop = FALSE]
    names(tally)[3] <- "draws"
    if (length(errors) > 1L) {
      tally <- tally[tally$outcome != "failed", , drop = FALSE]
      tally <- rbind(tally, data.frame(outcome = "failed", message = names(errors), draws = as.integer(errors),
        stringsAsFactors = FALSE))
    }
    if (no_solution > 0L) {
      hit <- tally$outcome == "inadmissible"
      heywood <- sum(tally$draws[hit]) - no_solution
      tally <- tally[!hit, , drop = FALSE]
      tally <- rbind(tally, data.frame(
        outcome = "inadmissible",
        message = c(
          "three items whose correlations admit no single-factor solution",
          if (heywood > 0L) words[["inadmissible"]]
        ),
        draws = c(no_solution, if (heywood > 0L) heywood), stringsAsFactors = FALSE
      ))
    }
    tally <- tally[order(-tally$draws, tally$outcome, tally$message), , drop = FALSE]
    rownames(tally) <- NULL
    tally
  } else {
    data.frame(outcome = character(0), message = character(0), draws = integer(0), stringsAsFactors = FALSE)
  }
  levels <- c("used", "failed", "not_converged", "inadmissible", "undetermined")
  counts <- list(
    requested = as.integer(bs$attempted), attempted = as.integer(bs$attempted),
    returned = sum(frame$returned), converged = sum(frame$converged),
    admissible = sum(frame$admissible), used = sum(frame$used),
    improper_kept = as.integer(bs$improper_kept %||% 0L),
    improper_set_aside = as.integer(bs$improper %||% 0L),
    no_solution = no_solution,
    too_few_cases = as.integer(bs$too_few_cases %||% 0L),
    error_messages = errors,
    outcome = table(factor(outcome, levels = levels))
  )
  list(counts = counts, draws = frame, reasons = reasons)
}

#' DMAR's acceptance rule, in the interface's words
#'
#' @param kind `"reliability"` (the reliability family: DMAR's default
#'   `improper = "keep"`, the half rule) or `"bifactor"`
#'   (`ci_bifactor_indices()` with `improper = "keep"`).
#' @param conf_level Confidence level, for the fewest usable replications.
#' @param half For the bifactor indices, whether DMAR applied its half rule
#'   (the `require_half` element of its `bootstrap` attribute). DMAR 1.1.0's
#'   `ci_bifactor_indices()` reports `TRUE` and withholds its intervals when
#'   fewer than half of the replications return values, although its help
#'   page says `FALSE`; the sentence follows what the result reports.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_dmar_boot_rule <- function(kind = c("reliability", "bifactor"), conf_level = 0.95, half = TRUE) {
  kind <- match.arg(kind)
  tail <- format(100 * (1 - conf_level) / 2)
  if (identical(kind, "reliability")) {
    paste0(
      "DMAR's counted bootstrap (improper = \"keep\", DMAR's default for the reliability family). ",
      "A replication is used when its refit returns a finite estimate. An improper solution ",
      "(a negative error variance) that still yields a coefficient is kept and counted; one ",
      "that yields none (its error variances sum below zero, or three items whose correlations ",
      "admit no single-factor solution) is set aside and counted, as are refits that raise an ",
      "error or do not converge. Half rule: no interval is formed when fewer than half of the B ",
      "replications return a value (an estimability safeguard, not a coverage guarantee). A ",
      sprintf("caution is given when the replications set aside and the improper ones kept reach %s%% of B, ", tail),
      "the tail area beyond a limit. Limits are the percentiles of the values used, by boot's ",
      "rule (boot::boot.ci), or their BCa adjustment."
    )
  } else {
    paste0(
      "DMAR's counted bootstrap (ci_bifactor_indices(), improper = \"keep\", passed by SIEVER; ",
      "DMAR's own default for the bifactor indices is \"drop\"). Each replication refits the model ",
      "with the point estimate's options; it is used when the refit converges and every displayed ",
      "index is a proportion (H defined, omega not above one). An improper solution (a ",
      "standardized loading beyond one or a negative residual variance) whose indices are ",
      "proportions is kept and counted; one whose indices are not is set aside and counted, as ",
      "are refits that raise an error (DMAR names the item whose error variance runs off below ",
      "zero when the likelihood has no maximum) or do not converge. The same replications are ",
      "used for every index. ",
      if (isTRUE(half)) {
        "Half rule: DMAR forms no interval when fewer than half of the B replications return values. "
      } else {
        "DMAR forms an interval from two or more used replications (no half rule). "
      },
      sprintf("A caution is given when the replications set aside and the improper ones kept reach %s%% of B; ", tail),
      sprintf("SIEVER shows a %s%% interval only from at least %d used replications, the fewest from which boot's percentile rule reaches a limit inside the sample. ",
        format(100 * conf_level), sieve_dmar_min_used(conf_level)),
      "PUC is fixed by the model and has no interval."
    )
  }
}

#' One DMAR call as recorded R
#'
#' @param object The name the result is assigned to.
#' @param fun The DMAR function.
#' @param first The first argument, as R code (the data or the fit).
#' @param args The other arguments, as values.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_dmar_call_code <- function(object, fun, first, args) {
  written <- vapply(names(args), function(nm) {
    value <- args[[nm]]
    sprintf("%s = %s", nm, if (is.character(value) && length(value) > 1L) {
      sprintf("c(%s)", paste(vapply(value, sieve_r_literal, character(1)), collapse = ", "))
    } else {
      sieve_r_literal(value)
    })
  }, character(1))
  c(
    sprintf("%s <- DMAR::%s(%s,", object, fun, first),
    paste0("  ", paste(written, collapse = ", "), ")")
  )
}

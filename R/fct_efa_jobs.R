# EFA fit-by-k comparisons run in supervised R subprocesses (workers), up to
# the number of workers at once. Only plain, immutable inputs cross the
# process boundary; no reactive object is exported. The one-row tables the
# workers return are kept per Shiny session in one store that the EFA and
# Dimensionality tabs share, so a solution either tab has fit is not fit
# again for the other.

#' Validate the exact requested number of common factors
#' @param k Requested count, without rounding or coercion.
#' @param p Number of included indicators.
#' @return `NULL` when supported, otherwise an explanation.
#' @author Ken Kelley
#' @noRd
sieve_efa_k_problem <- function(k, p) {
  if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k < 1 || k != floor(k)) {
    return("Choose a whole number of factors of at least 1. No other factor count was substituted.")
  }
  top <- sieve_efa_max_factors(p)
  if (k > top) {
    return(sprintf(paste0(
      "The requested k = %d is not supported by %d included items: the exploratory ",
      "model has negative degrees of freedom. At most %d factors can be fit. ",
      "Choose a supported count; no other factor count was substituted."
    ), k, p, top))
  }
  NULL
}

#' Launch exploratory fits outside Shiny's R process
#'
#' A worker fits one number of factors, or several in the order given (its
#' share of a request split over the workers, see `sieve_efa_chunks()`),
#' sharing what does not depend on k (see `sieve_efa_solutions()`). Each
#' finished solution is written at once, as its one-row table, to the
#' worker's own temporary folder, so the progress display shows each k as
#' it finishes rather than when the worker ends.
#'
#' @param data Immutable included item responses.
#' @param key Resolved scientific settings.
#' @param k One requested count, or several, in the order to fit them.
#' @return A worker handle (see `sieve_efa_worker_handle()`), polled
#'   without waiting. Its result is the one-row table of `k` (one count) or
#'   a list of them (several).
#' @author Ken Kelley
#' @noRd
sieve_efa_worker_start <- function(data, key, k) {

  paths <- sieve_worker_paths()
  bootstrap <- sieve_worker_bootstrap
  environment(bootstrap) <- baseenv()
  dir <- tempfile("siever_efa_")
  dir.create(dir)
  process <- tryCatch(
    callr::r_bg(
      function(data, key, ks, paths, bootstrap, dir) {
        bootstrap(paths)
        solve <- utils::getFromNamespace("sieve_efa_solutions", "SIEVER")
        table <- utils::getFromNamespace("sieve_efa_table", "SIEVER")
        report <- function(solution, lavaan_missing) {
          file <- file.path(dir, sprintf("k%d.rds", solution$k))
          part <- paste0(file, ".part")
          saveRDS(table(list(solution), lavaan_missing), part)
          file.rename(part, file)
        }
        fits <- withr::with_options(list(mc.cores = 1L), solve(
          data, key$estimator, key$ordered,
          rotation = key$rotation, ks = ks, missing = key$missing, on_solution = report
        ))
        tables <- lapply(fits, function(s) table(list(s), attr(fits, "lavaan_missing")))
        if (length(tables) == 1L) tables[[1L]] else tables
      },
      args = list(data = data, key = key, ks = as.integer(k), paths = paths, bootstrap = bootstrap, dir = dir),
      libpath = .libPaths(), supervise = TRUE, user_profile = FALSE,
      # Files, not pipes: nothing reads a pipe while the worker runs, and a
      # worker that writes more than the pipe holds (about 8 KB here; lavaan
      # prints its whole variable table before it stops on an item without
      # variance) would wait on it for ever. The error stream's last lines
      # are read when the worker ends without a result.
      stdout = file.path(dir, "stdout.txt"), stderr = file.path(dir, "stderr.txt")
    ),
    error = function(e) {
      unlink(dir, recursive = TRUE)
      stop(e)
    }
  )
  sieve_efa_worker_handle(process, dir)
}

#' A handle on one background exploratory worker
#'
#' Wraps the callr process with what the queue reads: whether it is alive,
#' its process identifier, its result, and the solutions it has finished
#' since the last look (`read_progress()`). Killing the worker or reading
#' its result removes its temporary folder. When the worker ended without a
#' result, the error `get_result()` raises carries the last lines the worker
#' wrote to its error stream (`worker_output`, see
#' `sieve_efa_worker_output()`), which `sieve_efa_worker_cause()` states.
#'
#' @param process A callr background process.
#' @param dir The worker's progress folder, which holds its `stdout.txt`
#'   and `stderr.txt`.
#' @return An environment with the functions `is_alive()`, `get_pid()`,
#'   `wait()`, `kill()`, `get_result()` and `read_progress()`.
#' @author Ken Kelley
#' @noRd
sieve_efa_worker_handle <- function(process, dir) {
  handle <- new.env(parent = emptyenv())
  handle$process <- process
  handle$dir <- dir
  handle$is_alive <- function() process$is_alive()
  handle$get_pid <- function() process$get_pid()
  handle$wait <- function(timeout = -1) {
    process$wait(timeout)
    invisible(handle)
  }
  handle$kill <- function() {
    killed <- tryCatch(process$kill(), error = function(e) FALSE)
    unlink(dir, recursive = TRUE)
    invisible(killed)
  }
  handle$read_progress <- function() {
    files <- list.files(dir, pattern = "^k[0-9]+[.]rds$", full.names = TRUE)
    tables <- lapply(files, function(f) tryCatch(readRDS(f), error = function(e) NULL))
    unlink(files)
    Filter(Negate(is.null), tables)
  }
  handle$get_result <- function() {
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    tryCatch(process$get_result(), error = function(e) {
      # Read before the folder is removed.
      e$worker_output <- sieve_efa_worker_output(dir)
      stop(e)
    })
  }
  handle
}

#' The last lines a background worker wrote to its error stream
#'
#' Engine refusals, warnings and messages come back inside the worker's
#' result; what reaches the error stream is what the worker could not
#' return: a crash's traceback, a start-up failure, or an error from
#' outside the captured engine calls. Those lines say why a worker ended
#' without a result.
#'
#' @param dir The worker's folder.
#' @param n The number of lines.
#' @param width The longest line kept, in characters; a longer line is cut
#'   and marked.
#' @return The last `n` lines that are not blank, trimmed, or
#'   `character(0)`.
#' @author Ken Kelley
#' @noRd
sieve_efa_worker_output <- function(dir, n = 5L, width = 300L) {
  file <- file.path(dir, "stderr.txt")
  if (!file.exists(file)) return(character(0))
  lines <- tryCatch(readLines(file, warn = FALSE), error = function(e) character(0))
  lines <- trimws(lines)
  lines <- utils::tail(lines[nzchar(lines)], n)
  long <- nchar(lines) > width
  lines[long] <- paste0(substring(lines[long], 1L, width), " [cut]")
  lines
}

#' The fit-by-k rows one Shiny session keeps
#'
#' A plain store of one-row tables (`cache`, at most 64, oldest dropped
#' first) and of the solutions being fit right now (`inflight`), shared by
#' the EFA and Dimensionality tabs of a session (see
#' `sieve_efa_session_shared()`), so that a solution with the same
#' scientific identity (`sieve_efa_job_key()`: the data, the included items
#' and their types, the estimator, the ordered items, the missing data rule,
#' the rotation and k) is fit once whichever tab asks first.
#'
#' @return An environment with `cache` and `inflight`.
#' @author Ken Kelley
#' @noRd
sieve_efa_shared <- function() {
  out <- new.env(parent = emptyenv())
  out$cache <- list()
  out$inflight <- list()
  out
}

#' The fit-by-k store of the current Shiny session
#' @param session A Shiny session, or `NULL` for a store of one's own.
#' @return The session's store (see `sieve_efa_shared()`), created on first
#'   use and kept in `session$userData`.
#' @author Ken Kelley
#' @noRd
sieve_efa_session_shared <- function(session = shiny::getDefaultReactiveDomain()) {
  user <- if (is.null(session)) NULL else tryCatch(session$userData, error = function(e) NULL)
  if (!is.environment(user)) {
    return(sieve_efa_shared())
  }
  held <- get0("sieve_efa_shared", envir = user, inherits = FALSE)
  if (is.null(held)) {
    held <- sieve_efa_shared()
    assign("sieve_efa_shared", held, envir = user)
  }
  held
}

# Distinguishes the queues of one session (the EFA and Dimensionality tabs)
# in the store's record of solutions being fit.
sieve_efa_queue_ids <- new.env(parent = emptyenv())

#' Per-session EFA cache and cancellable comparison queue
#'
#' Requested numbers of factors run in background workers, up to
#' `workers` at once (fewer when the memory estimate of one worker says so,
#' see `sieve_efa_worker_cap()`). The selected count (the request's
#' `selected_k`) is fit first and alone; the other counts are then split
#' over the workers by `sieve_efa_chunks()`; each worker fits its counts in
#' turn, sharing what does not depend on k, and the progress display reads
#' each count as it finishes.
#'
#' @param start Background process factory (injectable for verification),
#'   called with the data, the request and the counts of one worker.
#' @param workers The largest number of workers, or a function returning
#'   it; `sieve_cores()` by default.
#' @param shared The store of fitted rows (see `sieve_efa_shared()`).
#' @return An environment with the request, the cached rows (`cache`, the
#'   shared store's), and the running workers (`slots`; `process` reads the
#'   first worker).
#' @author Ken Kelley
#' @noRd
sieve_efa_jobs <- function(start = sieve_efa_worker_start, workers = function() sieve_cores(),
                           shared = sieve_efa_shared()) {
  out <- new.env(parent = emptyenv())
  out$start <- start
  out$workers <- workers
  out$shared <- shared
  makeActiveBinding("cache", function(value) {
    if (missing(value)) shared$cache else assign("cache", value, envir = shared)
  }, out)
  # The first running worker, for code that reads a single worker.
  makeActiveBinding("process", function(value) {
    if (missing(value)) {
      if (length(out$slots)) out$slots[[1L]]$process else NULL
    } else {
      invisible(NULL)
    }
  }, out)
  sieve_efa_queue_ids$n <- (get0("n", envir = sieve_efa_queue_ids, inherits = FALSE) %||% 0L) + 1L
  out$owner <- sieve_efa_queue_ids$n
  out$timings <- list()
  out$key <- NULL
  out$last_key <- NULL
  out$started_at <- NULL
  out$generation <- 0L
  out$requested <- integer(0)
  out$pending <- integer(0)
  out$completed <- integer(0)
  out$failed <- integer(0)
  out$errors <- character(0)
  out$slots <- list()
  out$cancelled <- FALSE
  out$data <- NULL
  out
}

#' Identity of one EFA fit, independent of the requested comparison range
#'
#' The fields that decide the solution, in a fixed order (so that the EFA
#' and Dimensionality tabs, which build their requests separately, name the
#' same solution the same way), and the count. Display labels, the range,
#' the selected count and the case count (already part of the data) are not
#' part of it.
#'
#' @param key Scientific specification.
#' @param k Exact count.
#' @return The single-fit cache key.
#' @author Ken Kelley
#' @noRd
sieve_efa_job_key <- function(key, k) {
  key$ks <- NULL
  key$selected_k <- NULL
  key$basis <- NULL
  key$label <- NULL
  key$n <- NULL # Already inherent in the immutable response-data identity.
  key <- key[sort(names(key))]
  # A whole count names one solution whether it arrives as 8 or 8L.
  key$k <- if (is.numeric(k) && length(k) == 1L && is.finite(k) && k == floor(k)) as.integer(k) else k
  key
}

#' Read or store an EFA fit at full precision
#' @param jobs Per-session jobs.
#' @param key Scientific specification.
#' @param k Exact count.
#' @param value A one-row fit table; `NULL` reads it.
#' @return The cached table or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_efa_job_cache <- function(jobs, key, k, value = NULL) {
  identity <- sieve_efa_job_key(key, k)
  cache <- jobs$cache
  at <- which(vapply(cache, function(x) identical(x$key, identity), logical(1)))
  if (is.null(value)) return(if (length(at)) cache[[at[1L]]]$value else NULL)
  if (length(at)) cache <- cache[-at]
  cache <- c(cache, list(list(key = identity, value = value)))
  # Bound memory, including the immutable response keys retained by the cache.
  if (length(cache) > 64L) cache <- utils::tail(cache, 64L)
  jobs$cache <- cache
  value
}

#' Who is fitting a solution right now
#' @param shared The session's store.
#' @param identity A single-fit key (`sieve_efa_job_key()`).
#' @return The owning queue's number, or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_efa_inflight_owner <- function(shared, identity) {
  for (x in shared$inflight) if (identical(x$key, identity)) return(x$owner)
  NULL
}

#' Record that a queue fits, or no longer fits, a solution
#' @param shared The session's store.
#' @param identity A single-fit key.
#' @param owner The queue's number.
#' @param on `TRUE` to record, `FALSE` to remove the queue's record.
#' @return Invisible `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_efa_inflight_mark <- function(shared, identity, owner, on = TRUE) {
  held <- Filter(function(x) !(identical(x$key, identity) && identical(x$owner, owner)), shared$inflight)
  if (isTRUE(on)) held <- c(held, list(list(key = identity, owner = owner)))
  shared$inflight <- held
  invisible(NULL)
}

#' The counts the running workers have not finished
#' @param jobs Per-session jobs.
#' @return An integer vector, in the workers' order.
#' @author Ken Kelley
#' @noRd
sieve_efa_jobs_running <- function(jobs) {
  as.integer(unlist(lapply(jobs$slots, function(s) setdiff(s$ks, s$done))))
}

#' Whether a queue has work running or waiting
#' @param jobs Per-session jobs.
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_efa_jobs_active <- function(jobs) {
  length(jobs$slots) > 0L || length(jobs$pending) > 0L
}

#' The number of workers a queue may run now
#' @param jobs Per-session jobs.
#' @return A whole number, at least 1: the queue's `workers`, limited by
#'   `sieve_efa_worker_cap()` for its current request.
#' @author Ken Kelley
#' @noRd
sieve_efa_jobs_workers <- function(jobs) {
  w <- if (is.function(jobs$workers)) jobs$workers() else jobs$workers
  w <- max(1L, as.integer(sieve_finite_or(w, 1L)))
  key <- jobs$key
  cap <- if (is.null(key$p) || is.null(key$estimator)) w else sieve_efa_worker_cap(key$p, key$estimator, n = key$n %||% NA_real_)
  max(1L, min(w, cap))
}

#' Progress of a running request, for the status line
#'
#' The counts running, the number of workers running, the seconds since the
#' first of them started, and the estimated time remaining: for each worker
#' the estimate of its whole share (`sieve_efa_plan_wait()` on one worker)
#' less the time it has run, the longest of them, plus the estimate of the
#' counts still queued on the workers allowed.
#'
#' @param jobs Per-session jobs.
#' @param now The current time.
#' @return A list with `running`, `workers`, `elapsed`, `remaining` and
#'   `wait` (a sentence).
#' @author Ken Kelley
#' @noRd
sieve_efa_jobs_progress <- function(jobs, now = Sys.time()) {
  key <- jobs$key
  starts <- lapply(jobs$slots, function(s) s$started_at)
  first <- if (length(starts)) min(do.call(c, starts)) else jobs$started_at
  elapsed <- if (is.null(first)) 0 else max(0, as.numeric(difftime(now, first, units = "secs")))
  # A worker whose counts are all in is only exiting.
  busy <- Filter(function(s) length(setdiff(s$ks, s$done)) > 0L, jobs$slots)
  left <- vapply(busy, function(s) {
    estimated <- sieve_efa_plan_wait(key$p, key$estimator, key$n, s$ks, 1L)
    estimated - as.numeric(difftime(now, s$started_at, units = "secs"))
  }, numeric(1))
  queued <- if (length(jobs$pending)) {
    sieve_efa_plan_wait(key$p, key$estimator, key$n, jobs$pending, sieve_efa_jobs_workers(jobs))
  } else {
    0
  }
  remaining <- max(c(0, left)) + queued
  wait <- if (!length(busy) && length(jobs$pending)) {
    "Waiting for the same models, which the session's other tab is fitting now."
  } else if (length(left) && all(left < 0) && !length(jobs$pending)) {
    "The running fits have exceeded their estimate; they are still running."
  } else {
    paste("Estimated time remaining:", sieve_efa_wait_label(remaining))
  }
  list(
    running = sieve_efa_jobs_running(jobs), workers = length(busy),
    elapsed = elapsed, remaining = remaining, wait = wait
  )
}

#' Stop one worker and release its solutions
#' @param jobs Per-session jobs.
#' @param slot A running worker record.
#' @return Invisible `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_efa_slot_stop <- function(jobs, slot) {
  try(slot$process$kill(), silent = TRUE)
  for (k in slot$ks) sieve_efa_inflight_mark(jobs$shared, sieve_efa_job_key(slot$key, k), jobs$owner, on = FALSE)
  invisible(NULL)
}

#' Cancel EFA work, including every running subprocess
#' @param jobs Per-session jobs.
#' @return Invisible `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_efa_jobs_cancel <- function(jobs) {
  if (!is.null(jobs$key)) jobs$last_key <- jobs$key
  jobs$generation <- jobs$generation + 1L
  for (slot in jobs$slots) sieve_efa_slot_stop(jobs, slot)
  jobs$slots <- list()
  jobs$pending <- integer(0)
  jobs$cancelled <- TRUE
  invisible(NULL)
}

#' Submit a request: the selected count first, then the additional counts
#'
#' Counts already kept (by this tab or the other) are complete at once. A
#' running worker whose solutions have the same scientific identity and are
#' still asked for keeps running under the new request (a different range
#' does not change a fit in flight); every other worker is stopped.
#'
#' @param jobs Per-session jobs.
#' @param key Full request identity.
#' @param data Immutable included responses.
#' @param ks Requested counts, the selected one first.
#' @return Invisible `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_efa_jobs_submit <- function(jobs, key, data, ks) {
  if (identical(jobs$key, key)) return(invisible(NULL))
  old_key <- jobs$key
  same <- function(a, b) identical(sieve_efa_job_key(a, 0L), sieve_efa_job_key(b, 0L))
  keep <- if (isTRUE(jobs$cancelled)) {
    list()
  } else {
    Filter(function(s) same(s$key, key) && any(setdiff(s$ks, s$done) %in% ks), jobs$slots)
  }
  if (length(keep)) {
    for (slot in jobs$slots) {
      if (!any(vapply(keep, identical, logical(1), slot))) sieve_efa_slot_stop(jobs, slot)
    }
    # A different comparison range does not change the fits in flight.
    # Rehome those verified requests under the new queue identity; the
    # generation check remains active for genuinely changed inputs.
    jobs$last_key <- old_key
    jobs$generation <- jobs$generation + 1L
    for (slot in keep) {
      slot$key <- key
      slot$generation <- jobs$generation
    }
    jobs$slots <- keep
  } else {
    sieve_efa_jobs_cancel(jobs)
    jobs$started_at <- Sys.time()
  }
  jobs$key <- key
  jobs$data <- data
  jobs$cancelled <- FALSE
  jobs$requested <- as.integer(ks)
  cached <- lapply(ks, function(k) sieve_efa_job_cache(jobs, key, k))
  available <- !vapply(cached, is.null, logical(1))
  failed <- vapply(cached, function(row) {
    !is.null(row) && any(row$status %in% c("failed", "not_converged", "not_identified"))
  }, logical(1))
  jobs$completed <- ks[available & !failed]
  jobs$failed <- ks[failed]
  jobs$errors <- vapply(which(failed), function(i) {
    sprintf("k = %d: %s", ks[i], paste(cached[[i]]$message, collapse = " "))
  }, character(1))
  jobs$pending <- setdiff(ks[!available], sieve_efa_jobs_running(jobs))
  sieve_efa_jobs_poll(jobs)
  invisible(NULL)
}

#' Read the cause of a background failure without its transport wrapper
#' @param error A worker condition.
#' @return A concise plain-text cause, followed by the last lines the
#'   worker wrote to its error stream when the condition carries them
#'   (`worker_output`, see `sieve_efa_worker_handle()`).
#' @author Ken Kelley
#' @noRd
sieve_efa_worker_cause <- function(error) {
  cause <- error
  while (inherits(cause$parent, "condition")) cause <- cause$parent
  message <- conditionMessage(cause)
  message <- gsub("^[[:space:]!\u2022]+", "", message)
  message <- gsub("[\r\n]+[[:space:]!\u2022]*", " ", message)
  if (grepl("crashed or was killed|non-zero status", message, ignore.case = TRUE)) {
    message <- paste(sub("[.]?$", ".", message), "The worker may have been stopped or run out of memory.")
  }
  said <- error$worker_output
  if (length(said)) {
    message <- paste0(
      sub("[.]?$", ".", message), " The last lines it wrote to its error stream: ",
      paste(sprintf("\"%s\"", said), collapse = "; "), "."
    )
  }
  message
}

#' Explain why saved EFA fit indices are historical
#' @param previous,current Scientific request keys.
#' @return One concise sentence naming changed scientific inputs.
#' @author Ken Kelley
#' @noRd
sieve_efa_changed_reason <- function(previous, current) {
  if (is.null(previous)) return(NULL)
  fields <- c(
    data = "item values or included observations", p = "the included item pool",
    estimator = "the estimator", ordered = "the ordered-indicator declaration",
    missing = "the missing-data rule", types = "the declared item types"
  )
  changed <- fields[!vapply(names(fields), function(nm) identical(previous[[nm]], current[[nm]]), logical(1))]
  if (!length(changed)) return(NULL)
  description <- if (length(changed) == 1L) changed else paste(
    paste(head(changed, -1L), collapse = ", "), tail(changed, 1L),
    sep = " and "
  )
  paste0(
    "Earlier fit indices are historical because ", description,
    " changed. Reverting to the same scientific inputs reuses cached rows."
  )
}

#' Record one finished solution of a worker
#' @param jobs Per-session jobs.
#' @param slot The worker's record.
#' @param table The solution's one-row table.
#' @param current Whether the worker still serves the current request.
#' @return Whether anything visible changed.
#' @author Ken Kelley
#' @noRd
sieve_efa_slot_accept <- function(jobs, slot, table, current) {
  if (!is.data.frame(table) || !nrow(table)) return(FALSE)
  k <- as.integer(table$k[1L])
  if (!k %in% slot$ks || k %in% slot$done) return(FALSE)
  slot$done <- c(slot$done, k)
  sieve_efa_inflight_mark(jobs$shared, sieve_efa_job_key(slot$key, k), jobs$owner, on = FALSE)
  # A canceled or superseded job never supplies a current result.
  if (!current) return(TRUE)
  sieve_efa_job_cache(jobs, slot$key, k, table)
  # The worker's first solution includes its start-up and calibrates the
  # next estimate.
  real_worker <- is.function(slot$process$get_pid)
  if (real_worker && identical(k, slot$ks[1L]) && all(table$status %in% c("ok", "inadmissible"))) {
    elapsed <- as.numeric(difftime(Sys.time(), slot$started_at, units = "secs"))
    timing <- sieve_efa_time_note(slot$key, k, elapsed)
    if (!is.null(timing)) jobs$timings <- utils::tail(c(jobs$timings, list(timing)), 32L)
  }
  if (k %in% jobs$requested) {
    if (any(table$status %in% c("failed", "not_converged", "not_identified"))) {
      jobs$failed <- c(jobs$failed, k)
      jobs$errors <- c(jobs$errors, sprintf("k = %d: %s", k, paste(table$message, collapse = " ")))
    } else {
      jobs$completed <- c(jobs$completed, k)
    }
  }
  TRUE
}

#' Poll the workers, take up rows the other tab finished, and start workers
#'
#' Reads each worker's finished solutions, collects workers that have
#' ended (a worker that ended without a solution fails its counts with the
#' cause), marks complete the waiting counts the session's other queue has
#' kept meanwhile, and starts workers for the waiting counts no queue is
#' fitting, as many as `sieve_efa_jobs_workers()` allows, each with its
#' share of them (`sieve_efa_chunks()`).
#'
#' @param jobs Per-session jobs.
#' @return Whether the queue's visible state changed.
#' @author Ken Kelley
#' @noRd
sieve_efa_jobs_poll <- function(jobs) {
  changed <- FALSE
  keep <- list()
  for (slot in jobs$slots) {
    current <- identical(slot$generation, jobs$generation) && identical(slot$key, jobs$key) &&
      !jobs$cancelled
    if (is.function(slot$process$read_progress)) {
      for (table in slot$process$read_progress()) {
        changed <- sieve_efa_slot_accept(jobs, slot, table, current) || changed
      }
    }
    if (slot$process$is_alive()) {
      keep <- c(keep, list(slot))
      next
    }
    if (is.function(slot$process$read_progress)) {
      for (table in slot$process$read_progress()) {
        changed <- sieve_efa_slot_accept(jobs, slot, table, current) || changed
      }
    }
    result <- tryCatch(list(value = slot$process$get_result(), error = NULL),
      error = function(e) list(value = NULL, error = sieve_efa_worker_cause(e))
    )
    tables <- if (is.data.frame(result$value)) list(result$value) else result$value
    for (table in tables) changed <- sieve_efa_slot_accept(jobs, slot, table, current) || changed
    undone <- setdiff(slot$ks, slot$done)
    for (k in undone) {
      sieve_efa_inflight_mark(jobs$shared, sieve_efa_job_key(slot$key, k), jobs$owner, on = FALSE)
    }
    if (current) {
      lost <- intersect(undone, jobs$requested)
      jobs$failed <- c(jobs$failed, lost)
      jobs$errors <- c(jobs$errors, sprintf(
        "k = %d: %s", lost,
        result$error %||% "the background worker ended without a result."
      ))
    }
    changed <- TRUE
  }
  jobs$slots <- keep
  if (!jobs$cancelled && !is.null(jobs$key) && length(jobs$pending)) {
    # Rows the session's other queue kept meanwhile.
    for (k in jobs$pending) {
      row <- sieve_efa_job_cache(jobs, jobs$key, k)
      if (is.null(row)) next
      jobs$pending <- setdiff(jobs$pending, k)
      if (any(row$status %in% c("failed", "not_converged", "not_identified"))) {
        jobs$failed <- c(jobs$failed, k)
        jobs$errors <- c(jobs$errors, sprintf("k = %d: %s", k, paste(row$message, collapse = " ")))
      } else {
        jobs$completed <- c(jobs$completed, k)
      }
      changed <- TRUE
    }
  }
  if (!jobs$cancelled && length(jobs$pending)) {
    # A worker whose counts are all in is only exiting: its core is free.
    busy <- sum(vapply(jobs$slots, function(s) length(setdiff(s$ks, s$done)) > 0L, logical(1)))
    free <- sieve_efa_jobs_workers(jobs) - busy
    if (free > 0L) {
      # A count the other tab is fitting right now waits for its row.
      startable <- Filter(function(k) {
        is.null(sieve_efa_inflight_owner(jobs$shared, sieve_efa_job_key(jobs$key, k)))
      }, jobs$pending)
      # The selected model is fit first and alone, so that it keeps the
      # speed it has alone; the other counts wait for its row and are then
      # split over the workers. Measured on 68 items under MLR on a busy
      # machine, five workers started with it
      # (at equal or lower operating-system priority) delayed the selected
      # model from about 65 to about 125 seconds.
      selected <- jobs$key$selected_k
      plan <- if (length(selected) == 1L && is.numeric(selected) &&
        selected %in% sieve_efa_jobs_running(jobs)) {
        list()
      } else if (length(selected) == 1L && is.numeric(selected) && selected %in% unlist(startable)) {
        list(as.integer(selected))
      } else {
        sieve_efa_chunks(as.integer(startable), free)
      }
      for (chunk in plan) {
        jobs$pending <- setdiff(jobs$pending, chunk)
        started <- tryCatch(list(process = jobs$start(jobs$data, jobs$key, chunk), error = NULL),
          error = function(e) list(process = NULL, error = sieve_efa_worker_cause(e))
        )
        if (is.null(started$error)) {
          slot <- new.env(parent = emptyenv())
          slot$process <- started$process
          slot$ks <- as.integer(chunk)
          slot$done <- integer(0)
          slot$key <- jobs$key
          slot$generation <- jobs$generation
          slot$started_at <- Sys.time()
          for (k in chunk) sieve_efa_inflight_mark(jobs$shared, sieve_efa_job_key(jobs$key, k), jobs$owner)
          jobs$slots <- c(jobs$slots, list(slot))
        } else {
          jobs$failed <- c(jobs$failed, chunk)
          jobs$errors <- c(jobs$errors, sprintf(
            "k = %d: background execution unavailable: %s", chunk, started$error
          ))
        }
        changed <- TRUE
      }
    }
  }
  changed
}

#' A solution that has no fit because its worker failed
#' @param k The count.
#' @param message Why there is no fit.
#' @return A solution (see `sieve_efa_solutions()`) with the status
#'   `"failed"`, as the engine's own failure would give.
#' @author Ken Kelley
#' @noRd
sieve_efa_failed_solution <- function(k, message) {
  status <- sieve_fit_status(NULL, error = message)
  row <- data.frame(
    k = as.integer(k), converged = FALSE, status = status$code,
    message = status$message, CFI = NA_real_, TLI = NA_real_, RMSEA = NA_real_,
    RMSEA.lo = NA_real_, RMSEA.hi = NA_real_, SRMR = NA_real_, BIC = NA_real_,
    chisq = NA_real_, df = NA_real_, pvalue = NA_real_, nobs = NA_real_,
    stringsAsFactors = FALSE
  )
  list(k = as.integer(k), row = row, warnings = character(0), measures = sieve_fit_families(NULL))
}

#' The fit-by-k table of a set of k, from the session's kept solutions
#'
#' The Dimensionality tab's model fit by k reads every solution the
#' session keeps (fit by either tab, see `sieve_efa_shared()`), fits the
#' others in this process in one call that shares what does not depend on k
#' (`sieve_efa_solutions()`), keeps them for the EFA tab, and combines the
#' set exactly as [sieve_efa_fit_by_k()] would (`sieve_efa_table()`).
#'
#' @param jobs A queue whose store is the session's.
#' @param key The request (see `sieve_efa_job_key()`).
#' @param data The included item responses.
#' @param spec The estimator specification (see `sieve_lavaan_spec()`).
#' @param ks The identified counts, in increasing order.
#' @param fit Whether to fit the counts the store lacks; with `FALSE` a
#'   missing count is a failed row with `failures[[k]]` as its reason.
#' @param failures Reasons by count (as character names) for `fit = FALSE`.
#' @return See [sieve_efa_fit_by_k()].
#' @author Ken Kelley
#' @noRd
sieve_efa_byk_kept <- function(jobs, key, data, spec, ks, fit = TRUE, failures = list()) {
  ks <- as.integer(ks)
  lavaan_missing <- sieve_efa_missing_arg(spec$missing, data)
  tables <- lapply(ks, function(k) sieve_efa_job_cache(jobs, key, k))
  todo <- ks[vapply(tables, is.null, logical(1))]
  if (length(todo) && isTRUE(fit)) {
    fresh <- sieve_efa_solutions(data, spec$estimator, spec$ordered,
      rotation = "none",
      ks = todo, missing = spec$missing
    )
    for (s in fresh) {
      one <- sieve_efa_table(list(s), attr(fresh, "lavaan_missing"))
      sieve_efa_job_cache(jobs, key, s$k, one)
      tables[[match(s$k, ks)]] <- one
    }
  }
  solutions <- Map(function(k, table) {
    if (is.null(table)) {
      sieve_efa_failed_solution(k, failures[[as.character(k)]] %||% "The background worker ended without a result.")
    } else {
      sieve_efa_solution_of(table)
    }
  }, ks, tables)
  sieve_efa_table(structure(unname(solutions), lavaan_missing = lavaan_missing))
}

#' Combine completed EFA rows in one common fit-index family
#' @param rows One-row tables retaining all fit families.
#' @return A comparison table; no model is refit.
#' @author Ken Kelley
#' @noRd
sieve_efa_rows <- function(rows) {
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  measures <- unlist(lapply(rows, function(x) attr(x, "measures")), recursive = FALSE)
  common <- sieve_efa_common_family(measures)
  out[, c("CFI", "TLI", "RMSEA", "RMSEA.lo", "RMSEA.hi")] <- NA_real_
  if (!is.na(common$family)) {
    for (i in which(common$solved)) {
      values <- measures[[i]]$families[[common$family]]$values
      out[i, c("CFI", "TLI", "RMSEA", "RMSEA.lo", "RMSEA.hi")] <-
        unname(values[c("cfi", "tli", "rmsea", "rmsea_lo", "rmsea_hi")])
    }
  }
  attr(out, "family") <- common$family
  attr(out, "family_reason") <- common$reason
  attr(out, "measures") <- measures
  attr(out, "warnings") <- unlist(lapply(rows, function(x) attr(x, "warnings")), recursive = FALSE)
  out[order(out$k), , drop = FALSE]
}

#' Keep the fits in flight under a changed request, without starting new ones
#'
#' When the EFA tab's request changes in its range or selected count only
#' (the same data, estimator, ordered items and missing data rule, see
#' `sieve_efa_job_key()`), a worker fitting a count still asked for keeps
#' running under the new request and a worker whose counts are no longer
#' asked for is stopped; nothing new starts. The queue then holds the
#' counts kept or being fit; the others are left for
#' `sieve_efa_jobs_extend()`, which the visible page calls under the run
#' policy. The first half of
#' `sieve_efa_jobs_submit()`, without its second.
#'
#' @param jobs Per-session jobs (see `sieve_efa_jobs()`).
#' @param key The new request.
#' @param ks The counts the new request asks for, the selected one first.
#' @return `TRUE` when the queue was rehomed; `FALSE` when nothing was
#'   queued, the request's scientific identity changed, or the queue was
#'   canceled (the caller cancels and starts afresh).
#' @author Ken Kelley
#' @noRd
sieve_efa_jobs_rehome <- function(jobs, key, ks) {
  if (is.null(jobs$key) || is.null(key) || identical(jobs$key, key) || isTRUE(jobs$cancelled)) {
    return(FALSE)
  }
  if (!identical(sieve_efa_job_key(jobs$key, 0L), sieve_efa_job_key(key, 0L))) {
    return(FALSE)
  }
  ks <- as.integer(ks)
  keep <- Filter(function(s) any(setdiff(s$ks, s$done) %in% ks), jobs$slots)
  for (slot in jobs$slots) {
    if (!any(vapply(keep, identical, logical(1), slot))) sieve_efa_slot_stop(jobs, slot)
  }
  jobs$last_key <- jobs$key
  jobs$generation <- jobs$generation + 1L
  for (slot in keep) {
    slot$key <- key
    slot$generation <- jobs$generation
  }
  jobs$slots <- keep
  jobs$key <- key
  running <- sieve_efa_jobs_running(jobs)
  cached <- lapply(ks, function(k) sieve_efa_job_cache(jobs, key, k))
  available <- !vapply(cached, is.null, logical(1))
  failed <- vapply(cached, function(row) {
    !is.null(row) && any(row$status %in% c("failed", "not_converged", "not_identified"))
  }, logical(1))
  jobs$requested <- ks[available | ks %in% running]
  jobs$completed <- ks[available & !failed]
  jobs$failed <- ks[failed]
  jobs$errors <- vapply(which(failed), function(i) {
    sprintf("k = %d: %s", ks[i], paste(cached[[i]]$message, collapse = " "))
  }, character(1))
  jobs$pending <- integer(0)
  TRUE
}

#' Add counts to the request the queue holds, and start them
#'
#' The second half of `sieve_efa_jobs_submit()`: the counts not yet asked
#' for are queued under the request the queue holds (a row either tab has
#' kept is complete at once) and the workers are started
#' (`sieve_efa_jobs_poll()`).
#'
#' @param jobs Per-session jobs.
#' @param ks The counts the request asks for, the selected one first.
#' @return `TRUE` when a count was added, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_efa_jobs_extend <- function(jobs, ks) {
  key <- jobs$key
  ks <- setdiff(as.integer(ks), jobs$requested)
  if (is.null(key) || !length(ks)) {
    return(invisible(FALSE))
  }
  jobs$cancelled <- FALSE
  cached <- lapply(ks, function(k) sieve_efa_job_cache(jobs, key, k))
  available <- !vapply(cached, is.null, logical(1))
  failed <- vapply(cached, function(row) {
    !is.null(row) && any(row$status %in% c("failed", "not_converged", "not_identified"))
  }, logical(1))
  jobs$requested <- c(jobs$requested, ks)
  jobs$completed <- c(jobs$completed, ks[available & !failed])
  jobs$failed <- c(jobs$failed, ks[failed])
  jobs$errors <- c(jobs$errors, vapply(which(failed), function(i) {
    sprintf("k = %d: %s", ks[i], paste(cached[[i]]$message, collapse = " "))
  }, character(1)))
  jobs$pending <- c(jobs$pending, setdiff(ks[!available], sieve_efa_jobs_running(jobs)))
  if (is.null(jobs$started_at) || !length(jobs$slots)) jobs$started_at <- Sys.time()
  sieve_efa_jobs_poll(jobs)
  invisible(TRUE)
}

# ---- DMAR exploratory calls in a background worker ---------------------------
# The EFA page's selected solution (DMAR::fa_exploratory(), when its
# estimate is above two seconds), its number of factors comparison
# (DMAR::factor_count()), and the Dimensionality page's parallel analysis
# (DMAR::parallel_analysis(), when its estimate is above two seconds) run in
# one supervised R subprocess each, so the session stays usable; the status line shows the elapsed time against the
# estimate, Cancel stops the process, and a result is accepted only for
# the request it was started for (a changed request cancels it).

#' Start one DMAR exploratory call in a background R process
#'
#' @param call A call from `sieve_efa_dmar_call()` or
#'   `sieve_efa_count_call()`; only plain values cross the process
#'   boundary.
#' @param rng_kind The [RNGkind()] settings of the process that drew the
#'   seed.
#' @param runner The SIEVER function the worker runs the call with:
#'   `"sieve_efa_dmar_run"` for one DMAR call, `"sieve_pa_dmar_run"` for the
#'   calls of a parallel analysis (the Dimensionality page).
#' @return A handle (see `sieve_efa_worker_handle()`), polled without
#'   waiting; its result is the runner's record.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_start <- function(call, rng_kind = RNGkind(), runner = "sieve_efa_dmar_run") {
  paths <- sieve_worker_paths()
  bootstrap <- sieve_worker_bootstrap
  environment(bootstrap) <- baseenv()
  dir <- tempfile("siever_dmar_")
  dir.create(dir)
  process <- tryCatch(
    callr::r_bg(
      function(call, rng_kind, paths, bootstrap, runner) {
        bootstrap(paths)
        run <- utils::getFromNamespace(runner, "SIEVER")
        withr::with_options(list(mc.cores = 1L), run(call, rng_kind = rng_kind))
      },
      args = list(call = call, rng_kind = rng_kind, paths = paths, bootstrap = bootstrap, runner = runner),
      libpath = .libPaths(), supervise = TRUE, user_profile = FALSE,
      # Files, not pipes, as for the exploratory workers
      # (sieve_efa_worker_start()).
      stdout = file.path(dir, "stdout.txt"), stderr = file.path(dir, "stderr.txt")
    ),
    error = function(e) {
      unlink(dir, recursive = TRUE)
      stop(e)
    }
  )
  sieve_efa_worker_handle(process, dir)
}

#' A background DMAR job: one request at a time
#'
#' @param start The process factory (injectable for verification), called
#'   with the call and the RNG kind.
#' @return An environment with `start`, `key` (the request running or last
#'   run), `process`, `started_at`, `seconds` (the estimate), `record` (the
#'   finished record and its `key`), `stopped` (the key of a canceled
#'   request), and `generation`.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_job <- function(start = function(call, rng_kind) sieve_efa_dmar_start(call, rng_kind)) {
  out <- new.env(parent = emptyenv())
  out$start <- start
  out$key <- NULL
  out$process <- NULL
  out$started_at <- NULL
  out$seconds <- NA_real_
  out$record <- NULL
  out$record_key <- NULL
  out$stopped <- NULL
  out$error <- NULL
  out$generation <- 0L
  out
}

#' Whether a DMAR job is running
#' @param job A job (`sieve_efa_dmar_job()`).
#' @param key Optional: running for this request.
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_active <- function(job, key = NULL) {
  !is.null(job$process) && (is.null(key) || identical(job$key, key))
}

#' Start a DMAR job for a request
#'
#' A job running for the same request keeps running; one running for
#' another request is stopped first.
#'
#' @param job A job.
#' @param key The request's identity.
#' @param call The DMAR call.
#' @param seconds The estimate, for the progress line.
#' @param rng_kind The RNG kind of the seed.
#' @return `TRUE` when a process was started or is running for `key`,
#'   `FALSE` when it could not start (`job$error` says why), invisibly.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_submit <- function(job, key, call, seconds = NA_real_, rng_kind = RNGkind()) {
  if (sieve_efa_dmar_active(job, key)) return(invisible(TRUE))
  sieve_efa_dmar_cancel(job, remember = FALSE)
  job$generation <- job$generation + 1L
  job$key <- key
  job$stopped <- NULL
  job$error <- NULL
  job$seconds <- seconds
  job$started_at <- Sys.time()
  started <- tryCatch(list(process = job$start(call, rng_kind), error = NULL),
    error = function(e) list(process = NULL, error = sieve_efa_worker_cause(e))
  )
  if (!is.null(started$error)) {
    job$error <- paste("Background execution unavailable:", started$error)
    job$record <- list(
      value = NULL, ok = FALSE, error = job$error, error_class = c("simpleError", "error", "condition"),
      reason = NULL, warnings = character(0), messages = character(0), seconds = NA_real_, fun = call$fun
    )
    job$record_key <- key
    return(invisible(FALSE))
  }
  job$process <- started$process
  invisible(TRUE)
}

#' Collect a finished DMAR job
#'
#' @param job A job.
#' @return `TRUE` when the job finished at this poll (its record is in
#'   `job$record`, for `job$record_key`), otherwise `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_poll <- function(job) {
  if (is.null(job$process) || isTRUE(job$process$is_alive())) return(FALSE)
  process <- job$process
  job$process <- NULL
  record <- tryCatch(process$get_result(),
    error = function(e) list(
      value = NULL, ok = FALSE, error = sieve_efa_worker_cause(e),
      error_class = c("simpleError", "error", "condition"), reason = NULL,
      warnings = character(0), messages = character(0), seconds = NA_real_, fun = NA_character_
    )
  )
  if (!is.list(record) || is.null(record$ok)) {
    record <- list(
      value = NULL, ok = FALSE, error = "The background worker ended without a result.",
      error_class = c("simpleError", "error", "condition"), reason = NULL,
      warnings = character(0), messages = character(0), seconds = NA_real_, fun = NA_character_
    )
  }
  job$record <- record
  job$record_key <- job$key
  TRUE
}

#' Stop a DMAR job
#' @param job A job.
#' @param remember `TRUE` to record the request as stopped by the user (it
#'   does not start again until asked for).
#' @return Invisible `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_cancel <- function(job, remember = TRUE) {
  if (!is.null(job$process)) {
    try(job$process$kill(), silent = TRUE)
    if (isTRUE(remember)) job$stopped <- job$key
  }
  job$process <- NULL
  job$generation <- job$generation + 1L
  invisible(NULL)
}

#' Progress of a running DMAR job, for the status line
#' @param job A job.
#' @param now The current time.
#' @return A list with `elapsed`, `seconds` (the estimate) and `sentence`.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_progress <- function(job, now = Sys.time()) {
  elapsed <- if (is.null(job$started_at)) 0 else max(0, as.numeric(difftime(now, job$started_at, units = "secs")))
  estimate <- job$seconds
  sentence <- if (!is.finite(estimate)) {
    sprintf("Elapsed: %.0f seconds; no estimate is available.", elapsed)
  } else if (elapsed > 1.5 * estimate) {
    sprintf("Elapsed: %.0f seconds, past the estimate (%s); still running.", elapsed, sieve_efa_wait_label(estimate))
  } else {
    sprintf("Elapsed: %.0f seconds of an estimated %s.", elapsed, sieve_efa_wait_label(estimate))
  }
  list(elapsed = elapsed, seconds = estimate, sentence = sentence)
}

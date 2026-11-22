# fct_reliability_boot.R
# SIEVER's whole-cluster counted reliability bootstrap, the part that runs
# the replications: one replication as a plain task that runs the same in
# this R process and in a socket worker, the replications spread over
# workers in batches, and the wait stated before a run. The plan, the
# acceptance rule, the summary, and the recorded R are in fct_reliability.R.
#
# Every reliability bootstrap of independent rows is DMAR's counted
# bootstrap (fct_dmar_boot.R), which counts every replication's fate and
# takes a progress and cancellation hook; this engine serves the one design
# DMAR's engine does not resample, declared clusters (whole clusters drawn
# with replacement, see sieve_bootstrap_partition()).
#
# Each replication draws its rows on a stream seeded with its own seed, one
# per replication, drawn from the seed of the run (see
# `sieve_reliability_boot_plan()`). A replication's rows therefore depend on
# the run's seed and on its number only, never on the worker that computes it
# or on how many workers there are, so a run on several workers returns the
# same replications, value for value, as the same run in one process.

#' The serial wait above which the counted bootstrap uses workers
#'
#' Starting the socket workers takes a second or two (each loads SIEVER,
#' DMAR and lavaan), so a bootstrap estimated to take less than this many
#' seconds in one process runs in that process.
#'
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_parallel_seconds <- function() {
  10
}

#' The seconds it takes to start the bootstrap's socket workers
#'
#' The start measured in this R process, or 2 seconds before any start has
#' been measured (the measured start of a pool of two to eight workers that
#' load the installed SIEVER).
#'
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_start_seconds <- function() {
  measured <- sieve_rate("reliability_boot_start")
  if (is.finite(measured)) measured else 2
}

#' How many workers a counted reliability bootstrap uses
#'
#' All the workers SIEVER's parallel loops use (`sieve_cores()`: the cores
#' this computer reports less one, at most eight, or the option
#' `sieve.cores` / `SIEVER_CORES`), when the bootstrap would take longer
#' than `sieve_reliability_boot_parallel_seconds()` in one process; one
#' otherwise.
#'
#' @param seconds Estimated seconds of the replications in one process.
#' @param cores Workers available.
#' @return A single integer, at least 1.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_workers <- function(seconds, cores = sieve_cores()) {
  cores <- suppressWarnings(as.integer(cores))
  if (length(cores) != 1L || !is.finite(cores) || cores < 2L ||
    length(seconds) != 1L || !is.finite(seconds) ||
    seconds <= sieve_reliability_boot_parallel_seconds()) {
    return(1L)
  }
  cores
}

#' Estimate how long a reliability bootstrap will take
#'
#' For DMAR's counted bootstrap (independent rows), the time of one fit
#' (the point estimate's call) times the number of fits, the jackknife of
#' BCa included (see `sieve_bootstrap_wait()`), spread over DMAR's processes
#' in the background process (`sieve_dmar_boot_cores()`, the processes the
#' page asks for there), plus the time to start that process (see
#' `sieve_dmar_boot_wait()`), shared with the other constructs of the run
#' (`alongside`). For SIEVER's whole-cluster bootstrap, the time of one
#' replication (measured on the full data when the point estimate was
#' computed, see `sieve_reliability_point()`) times the number of
#' replications, divided over the workers the run will use, plus the time
#' to start them (see `sieve_reliability_boot_workers()`).
#'
#' @param record A `sieve_reliability` record of the construct's point
#'   estimate (its `seconds`, `n$used`, `spec$cluster`, and
#'   `uncertainty$seconds_per_draw`).
#' @param B Number of bootstrap replications.
#' @param method `"percentile"` or `"dmar_bca"`.
#' @param cores Workers available.
#' @param alongside The number of constructs of the run (this one
#'   included), which share the start of the background process.
#' @return The estimated wait, in seconds.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_wait <- function(record, B, method = "percentile", cores = sieve_cores(),
                                        alongside = 1L) {
  n <- record$n$used %||% NA_integer_
  if (is.null(record$spec$cluster)) {
    serial <- sieve_bootstrap_wait(record$seconds, B, n, method)
    return(sieve_dmar_boot_wait(serial, sieve_dmar_boot_cores(cores), alongside))
  }
  per <- record$uncertainty$seconds_per_draw %||% record$seconds
  serial <- per * B
  workers <- sieve_reliability_boot_workers(serial, cores)
  if (workers < 2L) {
    return(serial)
  }
  sieve_reliability_boot_start_seconds() + serial / workers
}

#' The locale a socket worker should take from this process
#'
#' Messages an engine raises in a worker (an error a replication or a
#' construct records, for example) quote names according to the character
#' set and the fancy quotes setting, and are translated according to the
#' message locale. A worker started with `--vanilla` may not have this
#' process's settings, so they are passed with each task and set in the
#' worker, so the recorded messages read the same wherever they arose.
#'
#' @return A list with `categories` (locale categories and their values)
#'   and `quotes` (the `useFancyQuotes` option).
#' @author Ken Kelley
#' @noRd
sieve_worker_locale <- function() {
  categories <- c("LC_CTYPE", "LC_COLLATE", if (.Platform$OS.type == "unix") "LC_MESSAGES")
  values <- vapply(categories, function(ct) Sys.getlocale(ct), character(1))
  list(categories = as.list(values[nzchar(values)]), quotes = getOption("useFancyQuotes", TRUE))
}

#' How the replications of a counted bootstrap estimate the coefficient
#'
#' `"raw"` repeats on each resample the engine call the point estimate
#' made (see `sieve_reliability_call()`). `"ml_fit"` is used for robust
#' omega (the observed denominator) of complete rows: each resample's
#' coefficient from DMAR's one-factor fit by maximum likelihood (see
#' `sieve_reliability_omega_ml_fit()`). DMAR's `reliability_omega()` takes
#' the loadings of robust omega from a robust ML (MLR) fit of the same
#' model to the same rows; MLR's point estimates are ML's, and it adds a
#' scaled test statistic and the asymptotic covariance matrix of the
#' sample moments that the coefficient does not use, whose cost grows with
#' the square of the number of item pairs. As measured, a replication of 3
#' to 14 items took 1.1 to 1.8 times as long by the engine call as by the
#' ML fit, of 19 items 3 times, of 35 items 24 times, and of 45 to 73 items
#' 4 to 67 seconds against 0.07 to 0.14 seconds. On every replication of
#' the 45 constructs and facets compared (of 3 to 73 items, from the
#' bundled examples and from two private pools of 68 and 303 items) the two
#' gave the same number, to the last bit. Under full information maximum
#' likelihood, for the model-implied denominator, and for categorical omega
#' the engine call is repeated.
#'
#' DMAR's summary-statistics path (`reliability_omega(S = , N = )`) is not
#' used: it fits the model to the covariance matrix rather than to the
#' rows, so the optimizer stops at a slightly different point, and its
#' estimates differed from the raw-data path's by up to 9e-10 on the facets
#' of the 303-item pool and 1e-7 on three items (the saturated model).
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @return `"ml_fit"` or `"raw"`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_path <- function(spec) {
  if (identical(spec$engine, "reliability_omega") &&
    identical(spec$denominator, "observed") &&
    identical(spec$missing, "listwise")) {
    "ml_fit"
  } else {
    "raw"
  }
}

#' How far a replication's estimate may be from the engine call's
#'
#' @return The largest absolute difference accepted, 1e-10.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_tolerance <- function() {
  1e-10
}

#' Robust omega of complete rows from DMAR's one-factor ML fit
#'
#' What DMAR's `reliability_omega(denominator = "observed")` computes for
#' complete rows (the fit is asked for listwise deletion by name: DMAR's
#' `cfa_1()` defaults to FIML on complete data too, whose optimizer
#' stops up to 8e-8 away from the listwise ML fit of `reliability_omega()`
#' as measured; with `missing = "listwise"` the two agree exactly),
#' (sum of the loadings)^2 / 1'S1, with the loadings of DMAR's one-factor
#' model (`cfa_1()`, the factor variance fixed to 1) fitted by maximum
#' likelihood to the rows, and S their covariance matrix with divisor N.
#' The engine call fits the same model to the same rows by robust ML, whose
#' point estimates are these. As in the engine, an
#' improper solution is announced by lavaan's warning about negative
#' variances (DMAR's own Heywood warning is muffled, as the engine muffles
#' it), and a fit that fails or does not converge stops with the engine's
#' message.
#'
#' @param data A data frame of complete rows of the item columns.
#' @return A single number.
#' @author Ken Kelley
#' @noRd
sieve_reliability_omega_ml_fit <- function(data) {
  fit <- tryCatch(
    withCallingHandlers(
      DMAR::cfa_1(data = data, estimator = "ML", se = "none", output = "fit", missing = "listwise"),
      dmar_heywood_warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) NULL
  )
  if (is.null(fit) || !isTRUE(lavaan::lavInspect(fit, "converged"))) {
    stop("The single-factor CFA model did not converge.", call. = FALSE)
  }
  lambda <- lavaan::lavInspect(fit, "est")$lambda
  theta <- lavaan::lavInspect(fit, "est")$theta
  if (any(diag(theta) < 0) || !isTRUE(suppressWarnings(lavaan::lavInspect(fit, "post.check")))) {
    # Let the public engine distinguish a defined improper coefficient from
    # a negative composite error variance or a model with no solution.
    result <- DMAR::reliability_omega(data = data, denominator = "observed", ci_method = "none")
    return(sieve_reliability_value(result, "estimate"))
  }
  s <- stats::cov(data)
  n <- nrow(data)
  sum(lambda)^2 / (sum(s) * (n - 1) / n)
}

#' Choose and time the replication path on the full data
#'
#' The path of `sieve_reliability_boot_path()`, confirmed on the full rows
#' before it is used: the ML fit must reproduce the engine's estimate to
#' `sieve_reliability_boot_tolerance()`, or the replications repeat the
#' engine call. The time of that one fit is the time of a replication.
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @param X The construct's item data (numeric columns).
#' @param estimate The engine's estimate on the full rows.
#' @param seconds The time the engine call took.
#' @return A list with `path`, `reason` (why the engine call is repeated
#'   when the ML fit was not confirmed, or `NULL`), and `seconds` (the time
#'   of one replication).
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_path_check <- function(spec, X, estimate, seconds) {
  path <- sieve_reliability_boot_path(spec)
  if (!identical(path, "ml_fit")) {
    return(list(path = "raw", reason = NULL, seconds = seconds))
  }
  rows <- X[stats::complete.cases(X), , drop = FALSE]
  rownames(rows) <- NULL
  started <- proc.time()[["elapsed"]]
  check <- withr::with_preserve_seed(sieve_capture(sieve_reliability_omega_ml_fit(rows)))
  took <- proc.time()[["elapsed"]] - started
  gap <- if (check$ok) abs(check$value - estimate) else NA_real_
  if (!isTRUE(gap <= sieve_reliability_boot_tolerance())) {
    return(list(
      path = "raw",
      reason = if (check$ok) {
        sprintf(paste0(
          "The one-factor ML fit gave %s against the engine's %s on the full rows, so each ",
          "replication repeats the engine call."
        ), format(check$value, digits = 15), format(estimate, digits = 15))
      } else {
        paste("The one-factor ML fit failed on the full rows, so each replication repeats the engine call:", check$error)
      },
      seconds = seconds
    ))
  }
  list(path = "ml_fit", reason = NULL, seconds = took)
}

#' One replication of the counted reliability bootstrap, as plain data
#'
#' Everything a replication needs, without a function or an environment, so
#' it can be sent to a socket worker: the pool of rows, the seed of each
#' replication, the engine call that estimates the coefficient, and the path
#' the estimate takes (see `sieve_reliability_boot_estimate()`).
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @param pool The rows resampled (a data frame of the item columns).
#' @param draw_seeds One seed per replication.
#' @param sampling Aligned independent sampling-unit partition.
#' @param path `"raw"` or `"ml_fit"` (see `sieve_reliability_boot_path()`).
#' @return A list with `pool`, `seeds`, `fun`, `args`, and `path`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_task <- function(spec, pool, draw_seeds, path = "raw", sampling = sieve_bootstrap_partition(nrow(pool))) {
  call <- sieve_reliability_call(spec, ci_method = "none")
  list(pool = pool, seeds = draw_seeds, fun = call$fun, args = call$args, path = path, sampling = sampling)
}

#' Estimate the coefficient of one resample
#'
#' @param task A task from `sieve_reliability_boot_task()`.
#' @param data The resampled rows.
#' @return The engine's result table (`"raw"`), or the estimate
#'   (`"ml_fit"`).
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_estimate <- function(task, data) {
  if (identical(task$path, "ml_fit")) {
    return(sieve_reliability_omega_ml_fit(data))
  }
  do.call(sieve_reliability_engine(task$fun), c(list(data = data), task$args))
}

#' Fit one replication of the counted reliability bootstrap
#'
#' Replication `b` resamples the pool with replacement on a stream seeded
#' with its own seed (so its rows depend on the run's seed and on `b`
#' only), estimates the coefficient under the caller's stream held aside,
#' and applies the acceptance rule of `sieve_reliability_boot_rule()`. The
#' outcome is one of `"used"`, `"failed"` (the engine raised an error),
#' `"not_converged"` (the engine reported no convergence or no estimate),
#' and `"inadmissible"` (an improper solution was flagged, or the estimate
#' lies outside `[0, 1]`). The same function runs in this process and in a
#' socket worker.
#'
#' @param task A task from `sieve_reliability_boot_task()`.
#' @param b The replication number.
#' @return A list with `draw`, `seed`, `outcome`, `returned`, `converged`,
#'   `admissible`, `used`, `estimate`, and `message`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_replicate <- function(task, b) {
  n <- nrow(task$pool)
  rows <- sieve_with_seed(task$seeds[b], sieve_bootstrap_rows(task$sampling %||% sieve_bootstrap_partition(n)))$value
  resample <- task$pool[rows, , drop = FALSE]
  rownames(resample) <- NULL
  run <- withr::with_preserve_seed(sieve_capture(sieve_reliability_boot_estimate(task, resample)))
  read <- list(
    draw = as.integer(b), seed = task$seeds[b], outcome = "failed",
    returned = FALSE, converged = FALSE, admissible = FALSE, used = FALSE,
    estimate = NA_real_, message = ""
  )
  if (!run$ok) {
    read$message <- gsub("\\s+", " ", run$error)
    read$outcome <- if (grepl("converge", run$error, ignore.case = TRUE)) "not_converged" else "failed"
    return(read)
  }
  read$returned <- TRUE
  est <- if (is.data.frame(run$value)) {
    sieve_reliability_value(run$value, "estimate")
  } else {
    suppressWarnings(as.numeric(run$value)[1])
  }
  if (!is.finite(est)) {
    read$outcome <- "not_converged"
    read$message <- "The engine returned no estimate for this replication."
    return(read)
  }
  read$converged <- TRUE
  read$estimate <- est
  improper <- sieve_reliability_improper(run$warnings)
  if (length(improper) || est < 0 || est > 1) {
    read$outcome <- "inadmissible"
    read$message <- if (length(improper)) {
      gsub("\\s+", " ", improper[1])
    } else {
      sprintf("The estimate (%s) lies outside [0, 1].", format(est))
    }
    read$used <- TRUE
    return(read)
  }
  read$admissible <- TRUE
  read$used <- TRUE
  read$outcome <- "used"
  read
}

#' The workers of one counted bootstrap
#'
#' A small mutable record the plan carries: the number of workers wanted,
#' the pool once started, and why the workers could not be used, if they
#' could not. The pool is started by the first batch that needs it and
#' stopped when the last replication is in, when the run is summarized
#' (also after a stop), or, failing both, when the record is garbage
#' collected or R exits.
#'
#' @param size The number of workers wanted.
#' @return An environment with `size`, `pool`, and `failed`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_crew <- function(size = 1L) {
  crew <- new.env(parent = emptyenv())
  crew$size <- as.integer(size)
  crew$pool <- NULL
  crew$failed <- NULL
  reg.finalizer(crew, function(e) {
    if (!is.null(e$pool)) sieve_socket_stop(e$pool)
  }, onexit = TRUE)
  crew
}

#' Whether a plan's replications are spread over workers
#'
#' @param plan A plan from `sieve_reliability_boot_plan()`.
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_parallel <- function(plan) {
  crew <- plan$crew
  is.environment(crew) && crew$size >= 2L && is.null(crew$failed)
}

#' The socket workers of a plan, started when first needed
#'
#' @param plan A plan from `sieve_reliability_boot_plan()`.
#' @return The pool (see `sieve_socket_pool()`), or `NULL` when the plan
#'   runs in one process or the workers could not start (the reason is kept
#'   in `plan$crew$failed`, and the replications run in this process).
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_pool <- function(plan) {
  if (!sieve_reliability_boot_parallel(plan)) {
    return(NULL)
  }
  crew <- plan$crew
  if (is.null(crew$pool)) {
    started <- proc.time()[["elapsed"]]
    pool <- tryCatch(sieve_socket_pool(crew$size), error = function(e) e)
    if (inherits(pool, "error")) {
      crew$failed <- conditionMessage(pool)
      return(NULL)
    }
    crew$pool <- pool
    sieve_rate_note("reliability_boot_start", proc.time()[["elapsed"]] - started, 1)
  }
  crew$pool
}

#' Stop the socket workers of a plan
#'
#' @param plan A plan from `sieve_reliability_boot_plan()`.
#' @return `NULL`, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_release <- function(plan) {
  crew <- plan$crew
  if (is.environment(crew) && !is.null(crew$pool)) {
    sieve_socket_stop(crew$pool)
    crew$pool <- NULL
  }
  invisible(NULL)
}

#' The replications to compute next
#'
#' In one process, one replication at a time, so a caller that services a
#' user interface between replications keeps doing so. With workers, about
#' half a second of replications per worker, so each batch is worth sending.
#'
#' @param plan A plan from `sieve_reliability_boot_plan()`.
#' @param done The number of replications already computed.
#' @return The replication numbers of the next batch (none when all are
#'   done).
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_next <- function(plan, done) {
  left <- plan$B - as.integer(done)
  if (left <= 0L) {
    return(integer(0))
  }
  size <- 1L
  if (sieve_reliability_boot_parallel(plan)) {
    per <- plan$seconds_per_draw
    each <- if (length(per) == 1L && is.finite(per) && per > 0) {
      max(1L, as.integer(floor(0.5 / per)))
    } else {
      1L
    }
    size <- plan$crew$size * each
  }
  as.integer(done) + seq_len(min(left, size))
}

#' Fit a batch of replications of the counted reliability bootstrap
#'
#' Computes replications `b` (see `sieve_reliability_boot_replicate()`),
#' spread over the plan's workers in contiguous blocks when the plan uses
#' workers, in this process otherwise. The result does not depend on where
#' a replication was computed. If the workers cannot start or fail, the
#' batch and the rest of the run are computed in this process, with the same
#' result. The workers are stopped once the last replication is in.
#'
#' @param plan A plan from `sieve_reliability_boot_plan()`.
#' @param b Replication numbers, from 1 to `plan$B`.
#' @return A list of replications, in the order of `b`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_draws <- function(plan, b) {
  b <- as.integer(b)
  if (!length(b)) {
    return(list())
  }
  out <- NULL
  pool <- if (length(b) > 1L) sieve_reliability_boot_pool(plan)
  if (!is.null(pool)) {
    blocks <- unname(split(b, sort(rep_len(seq_len(min(length(b), pool$workers)), length(b)))))
    run <- function(bs, task, locale) {
      for (ct in names(locale$categories)) try(Sys.setlocale(ct, locale$categories[[ct]]), silent = TRUE)
      options(useFancyQuotes = locale$quotes)
      replicate <- utils::getFromNamespace("sieve_reliability_boot_replicate", "SIEVER")
      lapply(bs, function(i) replicate(task, i))
    }
    # Only the task's plain data travels; the function carries no package
    # environment (see sieve_socket_map()).
    environment(run) <- baseenv()
    got <- tryCatch(
      parallel::clusterApply(pool$cluster, blocks, run, task = plan$task, locale = sieve_worker_locale()),
      error = function(e) e
    )
    if (inherits(got, "error")) {
      plan$crew$failed <- conditionMessage(got)
      sieve_reliability_boot_release(plan)
    } else {
      out <- unlist(got, recursive = FALSE)
    }
  }
  if (is.null(out)) {
    out <- lapply(b, function(i) sieve_reliability_boot_replicate(plan$task, i))
  }
  if (max(b) >= plan$B) {
    sieve_reliability_boot_release(plan)
  }
  out
}

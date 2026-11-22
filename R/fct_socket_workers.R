# Socket workers avoid sharing the parent process's native child handlers.

#' Start a bounded pool using this exact installed package or source checkout
#' @param cores Positive number of workers.
#' @return A pool with socket cluster, worker identities, and actual size.
#' @author Ken Kelley
#' @noRd
sieve_socket_pool <- function(cores) {
  cores <- as.integer(sieve_finite_or(cores, 1L))
  if (length(cores) != 1L || cores < 1L) stop("Choose at least one socket worker.", call. = FALSE)
  if (!requireNamespace("parallel",
    quietly = TRUE
  )) stop("Socket workers require the parallel package bundled with R.",
    call. = FALSE
  )
  paths <- sieve_worker_paths()
  bootstrap <- sieve_worker_bootstrap
  environment(bootstrap) <- baseenv()
  cl <- withr::with_preserve_seed(parallel::makePSOCKcluster(cores,
    rscript_args = "--vanilla", setup_timeout = 30
  ))
  complete <- FALSE
  on.exit(if (!complete) try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  initialize <- function(paths, bootstrap, libraries, rng_kind) {
    .libPaths(libraries)
    bootstrap(paths)
    suppressWarnings(do.call(RNGkind, as.list(rng_kind)))
    loaded <- getNamespaceInfo(asNamespace("SIEVER"), "path")
    if (!identical(
      normalizePath(loaded),
      normalizePath(paths[["SIEVER"]])
    )) stop("The socket worker loaded a different SIEVER package.")
    list(pid = Sys.getpid(), path = loaded, version = as.character(getNamespaceVersion("SIEVER")),
      dmar_path = getNamespaceInfo(asNamespace("DMAR"), "path"),
      dmar_version = as.character(getNamespaceVersion("DMAR")), rng_kind = RNGkind())
  }
  # Serializing a namespace closure would load whichever installed package
  # the worker first found, before its exact source context was established.
  environment(initialize) <- baseenv()
  identity <- parallel::clusterCall(cl, initialize, paths, bootstrap, .libPaths(), RNGkind())
  complete <- TRUE
  list(cluster = cl, workers = length(cl), identity = identity, backend = "PSOCK")
}

#' Stop all socket workers owned by a computation
#' @param pool Pool, or NULL when the serial path was used.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_socket_stop <- function(pool) {
  if (!is.null(pool$cluster)) try(parallel::stopCluster(pool$cluster), silent = TRUE)
  invisible(NULL)
}

#' Dispatch plain tasks to named package services without capturing app state
#' @param tasks Ordered task values.
#' @param fun Name of an internal SIEVER service.
#' @param args Explicit shared plain-data arguments.
#' @param task_arg Name of the task argument; NULL passes it positionally.
#' @param cores Maximum workers, limited by the number of tasks.
#' @param pool Optional reusable pool owned and stopped by the caller.
#' @return Ordered values; unexpected task failures are classed error messages.
#' @author Ken Kelley
#' @noRd
sieve_socket_map <- function(tasks, fun, args = list(), task_arg = NULL, cores = 1L, pool = NULL) {
  if (!length(tasks)) return(list())
  if (length(fun) != 1L || !is.character(fun) || !exists(fun,
    envir = asNamespace("SIEVER"),
    inherits = FALSE
  )) stop("Name a SIEVER worker service.",
    call. = FALSE
  )
  apply_one <- function(task, service, args, task_arg) {
    call <- list(task)
    if (!is.null(task_arg)) names(call) <- task_arg
    tryCatch(do.call(utils::getFromNamespace(service, "SIEVER"), c(call, args)),
      error = function(e) structure(conditionMessage(e), class = c("siever_worker_error", "character"))
    )
  }
  environment(apply_one) <- baseenv()
  cores <- max(1L, min(length(tasks), as.integer(sieve_finite_or(cores, 1L))))
  if (is.null(pool) && cores == 1L) return(lapply(tasks, apply_one, service = fun, args = args, task_arg = task_arg))
  if (is.null(pool)) {
    pool <- tryCatch(sieve_socket_pool(cores), error = function(e) list(error = conditionMessage(e)))
    if (!is.null(pool$error)) return(rep(
      list(structure(
        paste(
          "Socket workers could not start:",
          pool$error
        ),
        class = c(
          "siever_worker_error",
          "character"
        )
      )),
      length(tasks)
    ))
    on.exit(sieve_socket_stop(pool), add = TRUE)
  }
  tryCatch(parallel::parLapply(pool$cluster, tasks, apply_one, service = fun, args = args, task_arg = task_arg),
    error = function(e) rep(
      list(structure(
        paste(
          "Socket worker results could not be retrieved:",
          conditionMessage(e)
        ),
        class = c(
          "siever_worker_error",
          "character"
        )
      )),
      length(tasks)
    )
  )
}

#' Exact Package Locations and Installations Inherited by Scientific Workers
#' @param packages Loaded packages, in dependency order.
#' @return A named list of the namespace paths, each carrying the `stamp` of
#'   the installation this session loaded (see `sieve_loaded_stamp()`).
#' @author Ken Kelley
#' @noRd
sieve_worker_paths <- function(packages = c("DMAR", "SIEVER")) {
  stats::setNames(lapply(packages, function(package) {
    structure(getNamespaceInfo(asNamespace(package), "path"), stamp = sieve_loaded_stamp(package))
  }), packages)
}

#' Load the Same Engines and Application in a Fresh Worker
#'
#' The function uses base R only, so it runs unchanged in a worker that has
#' loaded nothing. An installed package is identified by its path and by the
#' stamp of its installation: the `Built` field, which carries the time of the
#' installation, and the checksums of the lazy-load databases that hold its
#' code and internal data. An installation rewrites them, so a package
#' reinstalled at the same path after the session loaded it has a different
#' stamp. The session would otherwise keep the old code in memory while each
#' new worker loaded the new code, and one analysis would be computed on two
#' builds. Such a worker stops here.
#' @param paths Named package paths, in dependency order, from
#'   `sieve_worker_paths()`.
#' @param stamp_only Return the stamps of the installations at `paths`
#'   without loading anything.
#' @return Loaded paths invisibly, or the stamps.
#' @author Ken Kelley
#' @noRd
sieve_worker_bootstrap <- function(paths, stamp_only = FALSE) {
  installed <- function(path) file.exists(file.path(path, "Meta", "package.rds"))
  stamp <- function(package, path) {
    if (!installed(path)) return(NA_character_)
    built <- tryCatch(unname(read.dcf(file.path(path, "DESCRIPTION"), fields = "Built")[1L, 1L]),
      error = function(e) NA_character_)
    code <- file.path(path, "R", paste0(c(package, "sysdata"), ".rdb"))
    code <- code[file.exists(code)]
    paste(c(built, unname(tools::md5sum(code))), collapse = " | ")
  }
  if (isTRUE(stamp_only)) {
    return(vapply(names(paths), function(package) stamp(package, as.character(paths[[package]])), character(1)))
  }
  for (package in names(paths)) {
    expected <- attr(paths[[package]], "stamp", exact = TRUE)
    path <- as.character(paths[[package]])
    if (installed(path)) {
      if (length(expected) == 1L && !is.na(expected) && !identical(stamp(package, path), expected)) {
        stop("The ", package, " installed at ", path, " is not the installation this session loaded: ",
          "the package was reinstalled or replaced while the session was running. ",
          "Save the project, restart R, and run the analysis again, so that the session and its ",
          "background computations use one build.", call. = FALSE)
      }
      loadNamespace(package, lib.loc = dirname(path))
    } else {
      if (!requireNamespace("pkgload", quietly = TRUE)) {
        stop("Source workers require optional pkgload; install the packages to run without it.")
      }
      pkgload::load_all(path, quiet = TRUE, export_all = FALSE, helpers = FALSE)
    }
    loaded <- getNamespaceInfo(asNamespace(package), "path")
    if (!identical(normalizePath(loaded), normalizePath(path))) {
      stop("The worker loaded a different ", package, " package.")
    }
  }
  invisible(paths)
}

# The app also runs supervised callr/processx workers. These bounded socket
# paths must neither depend on parent fork handlers nor change numeric targets.
# Waits for stopped socket workers to exit. A busy machine can take a while
# to end an R process; a worker still running at the limit fails here.
socket_workers_gone <- function(pids, seconds = 600) {
  alive <- function() vapply(pids, siever_test_pid_alive, logical(1))
  siever_wait_until(function() !any(alive()), "A stopped socket worker", seconds = seconds)
}

socket_fixture <- function() lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]

test_that("socket workers use the exact namespace and restore all parent settings", {
  withr::local_seed(113)
  before_rng <- .Random.seed
  before_options <- options()
  before_env <- Sys.getenv()
  before_libs <- .libPaths()
  p <- sieve_socket_pool(2L)
  withr::defer(sieve_socket_stop(p))
  expect_identical(p$workers, 2L)
  expect_identical(p$backend, "PSOCK")
  expect_true(all(vapply(
    p$identity,
    function(x) identical(
      normalizePath(x$path),
      normalizePath(getNamespaceInfo(
        asNamespace("SIEVER"),
        "path"
      ))
    ),
    logical(1)
  )))
  expect_true(all(vapply(
    p$identity,
    function(x) identical(
      x$version,
      as.character(getNamespaceVersion("SIEVER"))
    ),
    logical(1)
  )))
  expect_true(all(vapply(p$identity, function(x) {
    identical(normalizePath(x$dmar_path), normalizePath(getNamespaceInfo(asNamespace("DMAR"), "path"))) &&
      identical(x$dmar_version, as.character(getNamespaceVersion("DMAR")))
  }, logical(1))))
  values <- sieve_socket_map(
    c(
      "auto",
      "invalid"
    ),
    "sieve_estimator_resolve",
    list(types = "continuous"),
    task_arg = "choice",
    pool = p
  )
  expect_identical(values[[1]]$estimator, "ML")
  expect_s3_class(values[[2]], "siever_worker_error")
  expect_identical(before_rng, .Random.seed)
  expect_identical(before_options, options())
  expect_identical(before_env, Sys.getenv())
  expect_identical(before_libs, .libPaths())
  pids <- vapply(p$identity, `[[`, integer(1), "pid")
  sieve_socket_stop(p)
  expect_true(socket_workers_gone(pids))
})

# The parallel analysis does not run on socket workers
# (DMAR::parallel_analysis() runs its reference datasets in one process, in a
# background R process when long). The pool's guarantees matter to its other
# users (the reliability bootstrap, the Items diagnostics), so the checks
# below read them through a seeded SIEVER service: sieve_holdout_draw() draws
# with sample.int() under the seed it is given.
socket_draw_args <- function() list(index = seq_len(301L), proportion = 0.3)

test_that("each seeded task has identical values and failures on serial and socket paths", {
  p <- sieve_socket_pool(2L)
  withr::defer(sieve_socket_stop(p))
  seeds <- c(113L, 114L, NA_integer_)
  one <- sieve_socket_map(seeds, "sieve_holdout_draw", socket_draw_args(), task_arg = "seed", cores = 1L)
  two <- sieve_socket_map(seeds, "sieve_holdout_draw", socket_draw_args(), task_arg = "seed", pool = p)
  expect_identical(lapply(one[1:2], `[[`, "rows"), lapply(two[1:2], `[[`, "rows"))
  expect_identical(lapply(one[1:2], function(x) x$rng$seed), lapply(two[1:2], function(x) x$rng$seed))
  expect_s3_class(two[[3]], "siever_worker_error")
  expect_identical(as.character(one[[3]]), as.character(two[[3]]))
})

test_that("parallel analysis starts no socket worker: DMAR's calls run in one process", {
  X <- socket_fixture()
  b <- sieve_retention_cor(X)
  seen <- new.env(parent = emptyenv())
  seen$starts <- 0L
  testthat::local_mocked_bindings(
    sieve_socket_pool = function(cores) {
      seen$starts <- seen$starts + 1L
      stop("no socket pool expected", call. = FALSE)
    },
    .package = "SIEVER"
  )
  r <- sieve_parallel_analysis(X, b, n_iter = 200L, seed = 113L)
  expect_true(r$ok)
  expect_identical(r$cores, 1L)
  expect_identical(seen$starts, 0L)
})

test_that("worker initialization failures stop every newly created socket process", {
  seen <- new.env(parent = emptyenv())
  seen$pids <- integer(0)
  actual <- parallel::clusterCall
  testthat::local_mocked_bindings(
    clusterCall = function(
      cl,
      fun,
      ...
    ) {
      seen$pids <- unlist(actual(
        cl,
        Sys.getpid
      ))
      stop("Deliberate initialization failure")
    },
    .package = "parallel"
  )
  expect_error(sieve_socket_pool(2L), "Deliberate initialization failure")
  expect_length(seen$pids, 2L)
  expect_true(socket_workers_gone(seen$pids))
})

test_that("dropped-item diagnostics retain exact values and reuse cached blocks without workers", {
  X <- socket_fixture()
  m <- sieve_build_meta(X, names(X), list(A = names(X)[1:4], B = names(X)[5:6]))
  a <- withr::with_seed(113, sieve_item_diagnostics(X, m, cores = 1L))
  seen <- new.env(parent = emptyenv())
  seen$starts <- 0L
  seen$pids <- integer(0)
  actual <- sieve_socket_pool
  testthat::local_mocked_bindings(
    sieve_socket_pool = function(cores) {
      p <- actual(cores)
      seen$starts <- seen$starts + 1L
      seen$pids <- c(
        seen$pids,
        vapply(
          p$identity,
          `[[`,
          integer(1),
          "pid"
        )
      )
      p
    },
    .package = "SIEVER"
  )
  keep <- new.env(parent = emptyenv())
  b <- withr::with_seed(113, sieve_item_diagnostics(X, m, cores = 2L, keep = keep))
  # The time the exploratory fit took is a measurement, not a result.
  untimed <- function(x) { x$efa_seconds <- NULL; x }
  expect_equal(untimed(a), untimed(b), tolerance = 1e-12)
  expect_identical(seen$starts, 1L)
  expect_true(socket_workers_gone(seen$pids))
  c <- withr::with_seed(113, sieve_item_diagnostics(X, m, cores = 2L, keep = keep))
  expect_identical(seen$starts, 1L)
  expect_identical(c$recomputed, character(0))
  expect_identical(b$items, c$items)
})

test_that("socket failures are explicit unavailable records rather than successful diagnostics", {
  X <- socket_fixture()
  testthat::local_mocked_bindings(sieve_socket_pool = function(...) stop("No socket capacity"), .package = "SIEVER")
  r <- sieve_socket_map(c(113L, 114L), "sieve_holdout_draw", socket_draw_args(), task_arg = "seed", cores = 2L)
  expect_true(all(vapply(r, function(x) inherits(x, "siever_worker_error") && grepl("No socket capacity", x), logical(1))))
  m <- sieve_build_meta(X, names(X), list(A = names(X)[1:4], B = names(X)[5:6]))
  keep <- new.env(parent = emptyenv())
  d <- sieve_item_diagnostics(X, m, cores = 2L, keep = keep)
  expect_true(all(is.na(d$items$omega_drop_delta[1:4])))
  expect_match(paste(d$notes, collapse = " "), "Drop-item reliability.*unavailable.*No socket capacity")
  expect_false(exists("construct:A", envir = keep, inherits = FALSE))
})


test_that("socket tasks preserve nondefault generator and normal/sample RNG kinds", {
  withr::local_seed(113, .rng_kind = "L'Ecuyer-CMRG")
  before <- list(seed = .Random.seed, kind = RNGkind())
  one <- sieve_socket_map(c(113L, 114L), "sieve_holdout_draw", socket_draw_args(), task_arg = "seed", cores = 1L)
  p <- sieve_socket_pool(2L)
  withr::defer(sieve_socket_stop(p))
  two <- sieve_socket_map(c(113L, 114L), "sieve_holdout_draw", socket_draw_args(), task_arg = "seed", pool = p)
  expect_identical(lapply(one, `[[`, "rows"), lapply(two, `[[`, "rows"))
  expect_true(all(vapply(p$identity, function(x) identical(x$rng_kind, before$kind), logical(1))))
  expect_identical(list(seed = .Random.seed, kind = RNGkind()), before)
})


test_that("alternate normal and sampling algorithms retain serial/socket values", {
  suppressWarnings(withr::local_seed(113,
    .rng_kind = "L'Ecuyer-CMRG",
    .rng_normal_kind = "Kinderman-Ramage", .rng_sample_kind = "Rounding"
  ))
  before <- list(seed = .Random.seed, kind = RNGkind())
  one <- suppressWarnings(sieve_socket_map(c(113L, 114L), "sieve_holdout_draw", socket_draw_args(), task_arg = "seed", cores = 1L))
  p <- sieve_socket_pool(2L)
  withr::defer(sieve_socket_stop(p))
  two <- sieve_socket_map(c(113L, 114L), "sieve_holdout_draw", socket_draw_args(), task_arg = "seed", pool = p)
  expect_identical(lapply(one, `[[`, "rows"), lapply(two, `[[`, "rows"))
  expect_true(all(vapply(p$identity, function(x) identical(x$rng_kind, before$kind), logical(1))))
  expect_identical(list(seed = .Random.seed, kind = RNGkind()), before)
})

test_that("socket setup and explicit-seed tasks do not create an absent caller random state", {
  withr::local_seed(113)
  rm(".Random.seed", envir = .GlobalEnv)
  before_kind <- RNGkind()
  p <- sieve_socket_pool(2L)
  withr::defer(sieve_socket_stop(p))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  r <- sieve_socket_map(c(113L, 114L), "sieve_holdout_draw", socket_draw_args(), task_arg = "seed", pool = p)
  expect_true(all(vapply(r, function(x) length(x$rows) == 90L, logical(1))))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_identical(RNGkind(), before_kind)
  sieve_socket_stop(p)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("process probes observe the live parent without sending it a signal", {
  pid <- Sys.getpid()
  expect_true(siever_test_pid_alive(pid))
  expect_identical(Sys.getpid(), pid)
})

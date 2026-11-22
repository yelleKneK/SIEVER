# A stand-in for the MI panel's background process (sieve_mi_start()). It
# keeps the arguments and random-number settings it was given in `log` and
# returns a job that has finished, whose result is `answer(args)` read as
# the background process returns it (sieve_capture()), with `engines` as
# the identities of the mice and lavaan.mi that process loaded
# (sieve_mi_worker()), or, with `alive = TRUE`, a job still running at
# `progress` until the page kills it (`log$killed` counts the kills).
mi_fake_start <- function(answer = mi_fake_answer, alive = FALSE,
                          progress = list(stage = "starting", done = 0L, total = NA_integer_),
                          log = new.env(parent = emptyenv()), engines = NULL) {
  log$killed <- 0L
  log$starts <- 0L
  function(args, rng_kind = NULL) {
    log$starts <- log$starts + 1L
    log$args <- args
    log$rng_kind <- rng_kind
    job <- new.env(parent = emptyenv())
    job$progress <- progress
    job$alive <- alive
    job$poll <- function() job$alive
    job$result <- function() {
      run <- sieve_capture(answer(args))
      if (isTRUE(run$ok) && !is.null(engines)) run$engine_identity <- engines
      run
    }
    job$kill <- function() {
      job$alive <- FALSE
      log$killed <- log$killed + 1L
      invisible(TRUE)
    }
    log$job <- job
    job
  }
}

# The tables a finished MI run returns, small and fixed.
mi_fake_answer <- function(args) {
  list(
    estimates = data.frame(lhs = "F", op = "=~", rhs = "i1", est = .6, se = .1,
      ci.lower = .4, ci.upper = .8),
    plan = list(table = data.frame(fmi = .1, lower_limit = .05, upper_limit = .2, imputations = 20L)),
    imputations = 20L, used = 20L, omitted = 0L, improper = 0L, imputation_events = NULL,
    pooled_variance = args$pooled_variance)
}

# sieve_installed() as on a machine where mice and lavaan.mi are installed
# (`present = TRUE`) or are not (`present = FALSE`), whatever this machine
# has; every other package as it is here. The MI panel checks for both
# (sieve_mi_check()) before it starts the background process, which the
# panel tests replace with mi_fake_start(), so with this mock the panel's
# tests need neither package (CRAN's check without Suggests, for one).
mi_suggests_installed <- function(present = TRUE) {
  function(pkg) {
    if (pkg %in% c("mice", "lavaan.mi")) return(present)
    nzchar(system.file(package = pkg))
  }
}

# The identities a background MI process records for the mice and
# lavaan.mi it loaded (sieve_mi_worker()), as records no installation has,
# so a test can tell them from this process's own.
mi_fake_engines <- function() {
  list(
    mice = list(package = "mice", status = "loaded", version = "0.0.0",
      id = "mice as the background process loaded it"),
    lavaan.mi = list(package = "lavaan.mi", status = "loaded", version = "0.0.0",
      id = "lavaan.mi as the background process loaded it"))
}

# A confirmed study whose Textual items are ordered (five categories) and
# whose first Textual item misses its first 25 responses, so the MI panel's
# own check (sieve_mi_check()) accepts the request.
mi_ordered_study <- function() {
  items <- sieve_hs9_fixture()$theory_map$Textual
  siever_test_study(configure = function(s) {
    d <- s$items
    for (nm in items) d[[nm]] <- as.integer(cut(d[[nm]],
      breaks = unique(stats::quantile(d[[nm]], 0:5 / 5)), include.lowest = TRUE))
    d[[items[1L]]][1:25] <- NA_real_
    s$items <- d
    s$raw[items] <- d[items]
    s$meta$type[s$meta$item %in% items] <- "ordered"
  })
}

# A fresh R process that has loaded the DMAR and SIEVER this test session
# loaded (an installed SIEVER, or this source tree through pkgload), runs
# `fun(...)` there and returns its value. `fun` is sent without its
# enclosure (which would carry this session's SIEVER namespace into the
# process before the bootstrap loads the right one), so it takes what it
# needs through `args`.
mi_fresh_siever <- function(fun, args = list()) {
  environment(fun) <- globalenv()
  bootstrap <- sieve_worker_bootstrap
  environment(bootstrap) <- baseenv()
  child <- function(fun, args, paths, bootstrap) {
    bootstrap(paths)
    do.call(fun, args)
  }
  environment(child) <- baseenv()
  callr::r(child, args = list(fun = fun, args = args, paths = sieve_worker_paths(),
    bootstrap = bootstrap), libpath = .libPaths(), user_profile = FALSE)
}

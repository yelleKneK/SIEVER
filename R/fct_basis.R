# fct_basis.R
# One correlation basis per item pool. The Screening heatmap, the
# Dimensionality criteria, the EFA and the Items diagnostics all work from
# the inter-item correlations of the included items. Computing them again
# for each tab is costly: on 68 ordered items a polychoric matrix takes
# about twenty seconds, so four tabs would cost four matrices.
# sieve_state_basis() computes the type-appropriate matrix once, through
# sieve_retention_cor() (which keeps what was requested, what was used, the
# engine's conditions and the seconds taken), and keeps it in the shared
# state under the identity of what it was computed from, one entry per
# request of the current pool (a tab that asks for Pearson correlations and
# a tab on the automatic basis each keep their own). The second tab gets the
# kept matrix; an edit that does not change the matrix (a label, a facet, a
# construct) gets it too; a new pool drops the entries of the old one; a
# dataset change empties the cache. Whether a tab computes the matrix by
# itself or waits for a button follows the run policy
# (sieve_basis_demand()): Pearson correlations
# cost nothing and are always computed; a polychoric, tetrachoric or mixed
# matrix is computed by itself when its estimate at the standard rates is
# two seconds or less, and otherwise waits for Compute the correlations.
#
# The same file holds the plain arithmetic behind the waits the interface
# states before a long computation starts: a rate per unit of work, taken
# from the computations already timed in this process and, before any, from
# the measured defaults of sieve_rate_defaults().

#' The included items as the analyses read them
#'
#' The columns of the item data that the ledger marks as included, read as
#' numbers, with the rows that are complete on every one of them. This is
#' the item pool every correlation-based analysis works from, built in one
#' place so that the Screening, Dimensionality, EFA and Items tabs read the
#' same rows and the same columns in the same order.
#'
#' @param state The shared state store. Read reactively when called inside
#'   a reactive context, so a caller is evaluated again when the item data
#'   or the ledger change.
#' @return `NULL` when no data are loaded; otherwise a list with `X` (a data
#'   frame of the included items, numeric, complete cases only), `types`
#'   (their confirmed measurement levels, named by item), `items` (their
#'   names, in ledger order), `n` (complete cases), `n_rows` (rows of the
#'   item data), and `key` (the content key of `X`, see `sieve_data_key()`).
#' @author Ken Kelley
#' @noRd
sieve_state_items <- function(state) {
  items_df <- state$items
  meta <- state$meta
  if (is.null(items_df) || is.null(meta)) {
    return(NULL)
  }
  active <- meta[meta$status == "active", , drop = FALSE]
  items <- intersect(as.character(active$item), names(items_df))
  types <- as.character(active$type)[match(items, active$item)]
  types[is.na(types)] <- "continuous"
  X <- as.data.frame(items_df)[, items, drop = FALSE]
  # Every reader of the item data coerces this way, so a labelled column
  # (an SPSS file read by haven) reads as its codes here as it does on the
  # other tabs.
  X[] <- lapply(X, sieve_as_number)
  raw <- X
  X <- X[stats::complete.cases(X), , drop = FALSE]
  list(
    X = X, raw = raw, types = stats::setNames(types, items), items = items,
    n = nrow(X), n_rows = nrow(items_df), key = sieve_data_key(raw)
  )
}

#' The number of workers SIEVER's own parallel loops use
#'
#' A worker is a separate socket-connected R process. The bounded PSOCK
#' pool loads the same SIEVER installation and uses a separate seed for
#' each task, so numerical results do not depend on the worker count.
#' Startup and memory costs can outweigh parallel gains for small pools.
#' The default is the available cores less one, at most `max`. Windows
#' defaults to one to avoid starting unsolicited socket connections;
#' explicit requests can use several workers on every supported platform.
#' The option `sieve.cores`, or `SIEVER_CORES` when the option is unset,
#' controls the count, and an explicit setting is honored up to 64
#' whatever `max` is. Tests normally use two workers on shared machines.
#' When neither is set and `R CMD check` limits cores (the environment
#' variable `_R_CHECK_LIMIT_CORES_` set to anything but `"false"`, as
#' `--as-cran` sets it), the default is at most two, CRAN's limit, so the
#' headless services that default to `sieve_cores()` (the reliability
#' bootstraps of `sieve_reliability_construct()`) never start more.
#' The Dimensionality tab permits up to `sieve_cores(max = 64)` workers.
#'
#' @param max The largest number of workers to use.
#' @return A single integer, at least 1.
#' @author Ken Kelley
#' @noRd
sieve_cores <- function(max = 8L) {
  opt <- suppressWarnings(as.integer(getOption("sieve.cores")))
  if (!length(opt) || !is.finite(opt)) {
    opt <- suppressWarnings(as.integer(Sys.getenv("SIEVER_CORES", "")))
  }
  # An explicit setting is the researcher's: it is honored up to 64, the
  # Workers input's maximum, whatever `max` caps the default at.
  if (length(opt) == 1L && is.finite(opt) && opt >= 1L) {
    return(as.integer(min(opt, 64L)))
  }
  if (identical(.Platform$OS.type, "windows")) {
    return(1L)
  }
  n <- tryCatch(parallel::detectCores(), error = function(e) NA_integer_)
  if (!length(n) || !is.finite(n)) {
    n <- 1L
  }
  limit <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  if (nzchar(limit) && !identical(limit, "false")) max <- min(max, 2L)
  as.integer(max(1L, min(max, n - 1L)))
}

# The seconds per unit of work measured in this process, by kind of
# computation (see sieve_rate_note()).
sieve_rates <- new.env(parent = emptyenv())

#' Default seconds per unit of work
#'
#' Measured with one worker: a polychoric matrix of 68 seven-point items by
#' 500 respondents took 21 seconds (2,278 item pairs); the one-factor WLSMV
#' model of those items took 20.7 seconds; one `lavaan::efa()` solution
#' under WLSMV took 62 seconds per number of factors; the 24-test bundled
#' example (276 pairs) took 0.37 seconds under MLR as a CFA and 0.6 seconds
#' per solution as an EFA. (The Items tab's diagnostics have their own
#' estimate, `sieve_diagnostics_wait()`.) One `lavaan::efa()` solution
#' under MLR took 0.4 seconds on the 24 tests and 27 to 28 seconds
#' on the 68 items, which the MLR units of `sieve_fit_units()` (the
#' square of the pairs) reproduce from the one rate. They are starting
#' values for the first estimate of a wait; every computation timed
#' afterwards replaces them.
#'
#' @return A named numeric vector of seconds per unit (item pairs).
#' @author Ken Kelley
#' @noRd
sieve_rate_defaults <- function() {
  c(
    pearson = 0, polychoric = 21 / 2278, tetrachoric = 21 / 2278, mixed = 21 / 2278,
    cfa_WLSMV = 20.7 / 2278, cfa_MLR = 0.37 / 276, cfa_ML = 0.37 / 276,
    efa_WLSMV = 62 / 2278, efa_MLR = 0.4 / 276, efa_ML = 2.1 / 2145,
    # Harman's screen (DMAR::common_method_single_factor, a one-factor
    # stats::factanal of the item correlation matrix): measured at 0.01
    # seconds at 68 items, 0.08 at 150 and 0.68 at 300
    # (n = 500), about the cube of the item count (see
    # `sieve_harman_wait()`). The reliability planner's Monte Carlo
    # (DMAR::ss_aipe_reliability with assurance): 0.9 to 2.0 seconds at
    # 4 to 12 items, a fixed cost (see `sieve_planner_wait()`).
    harman = 0.682 / 300^3, planner = 1.5,
    # The background process that runs DMAR's bootstraps (fct_dmar_boot.R):
    # starting it and loading DMAR and lavaan, measured at about 1.5
    # seconds; and, with several processes, the cost of forking
    # them for each of DMAR's blocks of replications (about fifty blocks a
    # call), about 0.01 seconds a block.
    dmar_boot_start = 1.5, dmar_boot_block = 0.01,
    # DMAR::parallel_analysis(), seconds per unit of one reference dataset
    # (see `sieve_pa_units()`), as measured.
    pa_dmar_pearson = 0.001, pa_dmar_polychoric = 0.03,
    # The ordered-item MI sensitivity analysis (sieve_mi_run()), seconds
    # per completed-data WLSMV fit per item pair, the imputation included:
    # 30.9 seconds for 20 pilot and 70 final fits of nine ordered items
    # (36 pairs), N = 301.
    mi_fit = 30.9 / (90 * 36)
  )
}

#' Record the time a computation took
#'
#' The rate of a kind of computation is its total measured seconds over its
#' total units of work in this process, so a large computation weighs more
#' than a small one. A computation of no units, or without a finite time,
#' is not recorded.
#'
#' @param kind A name from `sieve_rate_defaults()`.
#' @param seconds The elapsed seconds.
#' @param units The units of work (item pairs, or items).
#' @return The rate now in force for `kind`, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_rate_note <- function(kind, seconds, units) {
  if (length(seconds) != 1L || !is.finite(seconds) || seconds < 0 ||
    length(units) != 1L || !is.finite(units) || units <= 0) {
    return(invisible(sieve_rate(kind)))
  }
  held <- if (exists(kind, envir = sieve_rates, inherits = FALSE)) {
    get(kind, envir = sieve_rates)
  } else {
    c(seconds = 0, units = 0)
  }
  held <- c(seconds = unname(held[["seconds"]]) + seconds, units = unname(held[["units"]]) + units)
  assign(kind, held, envir = sieve_rates)
  invisible(sieve_rate(kind))
}

#' The seconds per unit of work in force for a kind of computation
#'
#' @param kind A name from `sieve_rate_defaults()`.
#' @return The measured rate when one was recorded in this process,
#'   otherwise the default; `NA` for an unknown kind.
#' @author Ken Kelley
#' @noRd
sieve_rate <- function(kind) {
  if (exists(kind, envir = sieve_rates, inherits = FALSE)) {
    held <- get(kind, envir = sieve_rates)
    return(unname(held[["seconds"]] / held[["units"]]))
  }
  defaults <- sieve_rate_defaults()
  if (kind %in% names(defaults)) unname(defaults[[kind]]) else NA_real_
}
#' An estimate at the standard rates
#'
#' Evaluates an estimate as if no time had been measured in this process,
#' so it depends only on the size of the problem. The run policy decides
#' with it (`sieve_runs_by_itself()`), so whether a first result runs by
#' itself does not change from one visit to the next as the session's
#' measured rates move; the wait the page states keeps the measured rates.
#'
#' @param expr An expression computing seconds.
#' @return The value of `expr` at the default rates; the measured rates
#'   are restored afterwards.
#' @author Ken Kelley
#' @noRd
sieve_at_default_rates <- function(expr) {
  names_held <- ls(envir = sieve_rates, all.names = TRUE)
  held <- mget(names_held, envir = sieve_rates)
  rm(list = names_held, envir = sieve_rates)
  on.exit({
    rm(list = ls(envir = sieve_rates, all.names = TRUE), envir = sieve_rates)
    list2env(held, envir = sieve_rates)
  }, add = TRUE)
  expr
}

#' The number of item pairs among `p` items
#'
#' @param p The number of items.
#' @return `p (p - 1) / 2`.
#' @author Ken Kelley
#' @noRd
sieve_pairs <- function(p) {
  p <- as.numeric(p)
  p * (p - 1) / 2
}

#' The units of work of a model fit
#'
#' What a measured time is divided by, and an estimate multiplied by. Under
#' ML and WLSMV the time of a fit grows about with the number of item pairs.
#' Under MLR the robust standard errors and scaled test statistic need the
#' asymptotic covariance matrix of the sample moments, whose size is the
#' square of the number of pairs, and the time grows about with that square:
#' measured on a pool of 68 seven-point items (n = 500), a one-factor CFA
#' of the 68 items took 25.5 seconds under MLR and 0.1 under ML, against
#' 0.7 seconds for 34 items under MLR, and one `lavaan::efa()` solution of
#' 68 items took 27 to 28 seconds under MLR and 0.9 under ML. The MLR
#' units are scaled to equal the pairs at 24 items (276 pairs), where the
#' default rates were measured, so that those rates keep their meaning;
#' with them the estimate for the 68-item CFA is 25.2 seconds.
#'
#' @param p The number of items (a vector, one count per model, is allowed).
#' @param estimator `"WLSMV"`, `"MLR"`, or `"ML"`.
#' @return The units of work, as long as `p`.
#' @author Ken Kelley
#' @noRd
sieve_fit_units <- function(p, estimator = "MLR") {
  pairs <- sieve_pairs(p)
  if (identical(estimator, "MLR")) pairs * pairs / 276 else pairs
}

#' Estimated seconds to compute a correlation matrix
#'
#' Polychoric, tetrachoric and mixed correlations are estimated pair by
#' pair, so the time grows with the number of pairs; Pearson correlations
#' take no time worth stating.
#'
#' @param p The number of items.
#' @param basis A basis identifier (see `sieve_retention_request()`).
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_basis_wait <- function(p, basis) {
  rate <- sieve_rate(basis)
  if (!is.finite(rate)) {
    return(NA_real_)
  }
  rate * sieve_pairs(p)
}

#' Estimated seconds of Harman's single-factor screen
#'
#' DMAR's screen fits one factor to the p by p item correlation matrix with
#' `stats::factanal()`, whose cost grows about with the cube of p (the
#' default rate, see `sieve_rate_defaults()`), plus a hundredth of a second
#' for the correlations. The Validity tab records the measured time after
#' each run (`sieve_rate_note("harman", ...)`), so later estimates follow
#' this machine.
#'
#' @param p The number of included items.
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_harman_wait <- function(p) {
  p <- as.numeric(p)
  if (length(p) != 1L || !is.finite(p)) {
    return(NA_real_)
  }
  0.01 + sieve_rate("harman") * p^3
}

#' Estimated seconds of the reliability planner's Monte Carlo
#'
#' DMAR::ss_aipe_reliability with an assurance level simulates the sampling
#' distribution of the interval width at the candidate sample sizes
#' (Kelley & Pornprasertmanit, 2016); its time is a fixed cost that does
#' not grow with the planning inputs in any useful way (0.9 to 2.0 seconds
#' at 4 to 12 items, as measured), so the estimate is the rate
#' itself, replaced by the time of each run made in this process
#' (`sieve_rate_note("planner", seconds, 1)`).
#'
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_planner_wait <- function() {
  sieve_rate("planner")
}

#' Estimated seconds to fit a measurement model
#'
#' The cost of a WLSMV fit is dominated by the asymptotic covariance matrix
#' of the thresholds and polychoric correlations, which grows with the
#' number of item pairs; an ML fit grows with the pairs too, through the
#' implied covariance matrix, at a much smaller rate; an MLR fit grows with
#' the square of the pairs (see `sieve_fit_units()`). An exploratory
#' solution is fit once per number of factors.
#'
#' @param p The number of items in the model.
#' @param estimator `"WLSMV"`, `"MLR"`, or `"ML"`.
#' @param what `"cfa"` for a confirmatory model, `"efa"` for exploratory
#'   solutions.
#' @param k The number of solutions (exploratory) to fit.
#' @param n Eligible observations for an exploratory fit. With `n` and
#'   `factors` both NULL the estimate rests on the item count and `k`
#'   alone.
#' @param factors Actual requested factor counts, rather than their count.
#' @param floor `TRUE` to give each first estimate (no rate measured yet)
#'   the half second of fixed fit overhead; `FALSE` leaves the overhead to
#'   the caller, which adds it once for several fits of one computation
#'   (`sieve_demand_wait()`).
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_fit_wait <- function(p, estimator = "MLR", what = c("cfa", "efa"), k = 1L,
                           n = NULL, factors = NULL, floor = TRUE) {
  what <- match.arg(what)
  if (identical(what, "efa") && (!is.null(n) || !is.null(factors))) {
    if (is.null(factors)) factors <- rep(1, max(1L, as.integer(k)))
    if (is.null(n)) n <- 500L
    baseline <- sieve_efa_wait_baseline(p, estimator, n, factors)
    observed <- sieve_rate(paste0("efa_elapsed_", estimator))
    correction <- if (is.finite(observed) && observed > 0) observed else 1
    return(sum(baseline) * correction)
  }
  kind <- sprintf(
    "%s_%s",
    what,
    if (identical(
      estimator,
      "WLSMV"
    )) "WLSMV" else if (identical(
      estimator,
      "ML"
    )) "ML" else "MLR"
  )
  rate <- sieve_rate(kind)
  if (!is.finite(rate)) {
    return(NA_real_)
  }
  estimate <- rate * sieve_fit_units(p, sub("^[a-z]+_", "", kind)) * max(1, as.numeric(k))
  if (!exists(kind, envir = sieve_rates, inherits = FALSE)) {
    # A first estimate includes fixed fit overhead and a conservative allowance
    # for larger pools. Observed rates replace this allowance after a run.
    estimate <- estimate * ifelse(p >= 20, 2, 1)
    if (isTRUE(floor)) estimate <- pmax(0.5, estimate)
  }
  estimate
}

#' Estimated seconds for the Items tab's per item diagnostics
#'
#' The diagnostics fit one exploratory factor analysis of the pool and, for
#' each construct of `m` items computed (not kept), robust omega of its
#' items, robust omega without each item in turn and one fit for the
#' residual correlations: `m + 2` one-factor fits with four or more items,
#' one with three. Each fit costs a fixed part and a part that grows with
#' the item pairs. Measured on one core: the diagnostics of a pool of 68
#' seven-point items (n = 500) as one construct took 15.5
#' seconds (70 fits, 9.5 on two workers), as six facets 3.1 seconds
#' (80 fits), the 24 bundled tests 0.9 seconds and the nine 0.1 seconds,
#' about 0.034 seconds plus 0.000084 per pair for each robust omega
#' (observed denominator) of continuous items; categorical robust omega
#' (ordered items, polychoric correlations and WLSMV for each fit) took
#' 0.47, 0.81 and 1.35 seconds at 4, 8 and 12 items, about 0.3 seconds
#' plus 0.016 per pair. The exploratory factor analysis is
#' `DMAR::fa_exploratory()` with point estimates only, estimated by
#' `sieve_efa_point_wait()` with its own measured ratio: on 68 items it
#' took about 0.7 to 1.1 seconds at one factor and 1.8 to 3.7 at six (with
#' its standard errors, about 1.2 seconds at one factor and 9 at six). It
#' is not part of the wait when the EFA page's solution is read in its
#' place (`efa = FALSE`).
#' Socket workers share the per item fits when the estimate on one is two
#' seconds or more (each started worker costs about a second). After each
#' run the ratio of the measured to the estimated time of the reliability
#' part is recorded (`sieve_rate_note("diagnostics_ratio", ...)`), and
#' later estimates of it are scaled by it.
#'
#' @param m The item counts of the constructs to compute (one per construct;
#'   a single count is one construct).
#' @param ordered Whether each construct is read as ordered (recycled).
#' @param p The number of items in the factor analysis.
#' @param workers The workers the per item fits may use.
#' @param k The number of factors of the factor analysis.
#' @param efa `FALSE` when the factor analysis is not fit (the EFA page's
#'   solution is read in its place).
#' @param refused `TRUE` when the factor analysis is expected to be refused
#'   (`sieve_efa_boundary_ks()`), which can take far longer.
#' @param fiml Whether each construct's reliability is fit by full
#'   information maximum likelihood (recycled): its fits then cost about
#'   0.08 seconds plus 4.8e-6 seconds per row and item pair (measured with
#'   DMAR 1.1.0: 0.10 seconds for 6 items and 500 rows, 0.29
#'   for 12 items and 500 rows, 1.03 for 12 items and 3,000 rows, 10 percent
#'   of each item missing), in place of the complete-data rate.
#' @param n The rows each construct's reliability reads (recycled).
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_diagnostics_wait <- function(m, ordered = FALSE, p = sum(m), workers = 1L, k = 1L, efa = TRUE, refused = FALSE,
                                   fiml = FALSE, n = 0) {
  m <- as.numeric(m)
  efa_part <- if (isTRUE(efa)) {
    sieve_efa_point_wait(p, max(1, sieve_finite_or(as.numeric(k), 1)), "matrix", refused = isTRUE(refused))
  } else {
    0
  }
  ratio <- sieve_rate("diagnostics_ratio")
  scale <- if (is.finite(ratio) && ratio > 0) ratio else 1
  if (!length(m)) {
    return(efa_part + 0.05 * scale)
  }
  ordered <- rep_len(as.logical(ordered), length(m))
  fits <- ifelse(m >= 4, m + 2, ifelse(m >= 3, 1, 0))
  fiml <- rep_len(as.logical(fiml), length(m)) %in% TRUE
  n <- rep_len(if (length(n)) as.numeric(n) else 0, length(m))
  n[!is.finite(n)] <- 0
  per_fit <- ifelse(ordered, 0.3 + 0.016 * sieve_pairs(m),
    ifelse(fiml, 0.08 + 4.8e-6 * n * sieve_pairs(m), 0.034 + 8.4e-5 * sieve_pairs(m)))
  blocks <- sum(fits * per_fit)
  workers <- max(1L, as.integer(sieve_finite_or(workers, 1L)))
  if (workers > 1L && blocks >= 2) {
    blocks <- blocks / workers * 1.2 + workers
  }
  efa_part + (0.05 + blocks) * scale
}

#' The constants of the point-estimate EFA wait
#'
#' The point-only maximum likelihood fit costs a fixed part, a part per
#' factor, and a part that grows with the items: each iteration of the
#' fit evaluates the moments, `p (p + 1) / 2`, against the free
#' parameters, about `p (k + 1)`, so that term is in seconds per million
#' units of their product (`moments_npar`). Measured with DMAR 1.1.0
#' (lavaan 0.7-3, the matrix route through `sieve_efa_point_call()`,
#' n = 600, data with three factors at 9 items and six at 24 to 68, the
#' shorter of two runs): 0.09 to 0.16 seconds at 9 items and 1 to 3
#' factors, 0.15 to 0.44 at 24 items and 1 to 7, 0.14 to 0.55 at 36,
#' 0.23 to 1.13 at 48, and 0.68 to 4.2 at 68 items and 1 to 7 factors; a
#' separate run of the same call at 68 items took 1.09, 0.83, 1.32, 1.26,
#' 1.91, 1.82, 2.54 and 4.44 seconds at 1 to 8 factors, 15.2 in all, where
#' this estimate gives 14.9. The fixed and per factor constants are those
#' measured at 9 and 24 items (0.15 to 0.23 seconds at 9 items and 3
#' factors, 0.38 to 0.43 at 24 and 5), and `moments_npar` minimizes the
#' squared log ratio of measured to estimated time over the proper fits
#' (ratios 0.74 to 1.68 at 9 to 68 items). Eight factors where the data
#' have six took 4.4 to 7.8 seconds at 48 and 68 items, 1.5 to 6 times the
#' estimate: a solution with more factors than the data hold converges
#' slowly, which the measured ratio of the session
#' (`sieve_rate_note("efa_point", ...)`) then takes up. Without a term for
#' the items the wait would put eight factors on 68 items at 1.6 seconds,
#' and the run rule would start Very Simple Structure of 68 items (15
#' seconds) in the application's own process unasked.
#'
#' @return A named numeric vector: `fixed` seconds, seconds `per_factor`,
#'   seconds per million units of `moments * npar` (`moments_npar`), the
#'   data route's extra seconds (`data`), and the refused solution's fixed
#'   seconds and multiple of a proper fit (`refusal_fixed`,
#'   `refusal_multiple`; `sieve_efa_point_wait(refused = TRUE)`).
#' @author Ken Kelley
#' @noRd
sieve_efa_point_rate <- function() {
  c(fixed = 0.06, per_factor = 0.03, moments_npar = 1.9, data = 0.02,
    refusal_fixed = 3, refusal_multiple = 10)
}

#' The identity of a correlation basis
#'
#' What determines the matrix, and nothing else: the dataset, the values of
#' the included items on the complete cases (which carries the included
#' item names and their keying), their confirmed measurement levels, and
#' the correlations requested. The ledger key of `sieve_revision()` is not
#' part of it on purpose: that key moves when a construct or a facet is
#' edited, and the matrix does not.
#'
#' @param state The shared state store.
#' @param pool The result of `sieve_state_items()`.
#' @param resolved The basis the request resolves to.
#' @return A list.
#' @author Ken Kelley
#' @noRd
sieve_basis_key <- function(state, pool, resolved) {
  roles <- sieve_roles_table(state$raw, state$items, state$roles)
  aux <- setdiff(roles$variable[roles$role %in% "auxiliary"], pool$items)
  list(
    dataset = shiny::isolate(state$dataset_id) %||% NA_character_,
    data = pool$key,
    treatment = sieve_estimator_effective("study", state),
    types = unname(pool$types),
    requested = resolved, auxiliary = aux,
    auxiliary_values = if (is.data.frame(state$raw) && length(aux))
      sieve_data_key(state$raw[, intersect(aux, names(state$raw)), drop = FALSE]) else NA_character_
  )
}

#' The correlation request the study's estimator setting implies
#'
#' The Ingest tab's Estimator control sets how the items are treated in
#' every model: following the item types (`"auto"`), as continuous (`"mlr"`,
#' `"ml_fiml"`, `"ml"`), or as ordered (`"wlsmv"`). The correlation basis the Screening,
#' Dimensionality, EFA and Items tabs share follows the same treatment, so
#' the matrix an exploratory analysis reads agrees with the estimator the
#' models use.
#'
#' @param state The shared state store (read reactively).
#' @return `"auto"`, `"pearson"`, or `"poly"` (see `sieve_retention_request()`).
#' @author Ken Kelley
#' @noRd
sieve_study_basis_request <- function(state) {
  switch(sieve_estimator_effective("study", state),
    mlr = ,
    ml_fiml = ,
    ml = "pearson",
    wlsmv = "poly",
    "auto"
  )
}

#' The correlations a request resolves to
#'
#' `"auto"` (or `"study"`) follows the study's estimator setting (the item
#' types, or the treatment the Ingest tab's Estimator control chose:
#' continuous for MLR and ML, ordered for WLSMV); a resolved identifier is
#' kept as given; `"pearson"` or `"poly"` is resolved against the confirmed
#' types (`sieve_retention_request()`).
#'
#' @param state The shared state store.
#' @param pool The result of `sieve_state_items()`.
#' @param requested See `sieve_state_basis()`.
#' @return `"pearson"`, `"polychoric"`, `"tetrachoric"`, or `"mixed"`.
#' @author Ken Kelley
#' @noRd
sieve_basis_resolve <- function(state, pool, requested = NULL) {
  choice <- requested %||% "auto"
  if (choice %in% c("auto", "study")) {
    choice <- sieve_study_basis_request(state)
  }
  if (choice %in% names(sieve_retention_basis_labels())) {
    choice
  } else {
    sieve_retention_request(choice, unname(pool$types))
  }
}

#' The shared correlation basis, under the run policy
#'
#' What the Screening, Dimensionality, EFA and Items tabs read in place of
#' `sieve_state_basis()`: the kept matrix when there is one, the matrix
#' computed now when the researcher asked for it (`asked`) or when it may
#' be computed by itself, and otherwise a record saying that it waits, with
#' the estimate and the button to press. Pearson correlations take no time
#' worth stating and are computed whenever they are read, a reopened
#' project included; a polychoric, tetrachoric or mixed matrix is computed
#' by itself when its estimate at the standard rates is two seconds or less
#' and this machine's estimate at most six (`sieve_runs_by_itself()`), and
#' not when the project was reopened from a file
#' (`sieve_restored_project()`): on 68 ordered items it takes about twenty
#' seconds, too long to start only because one of the four tabs was shown.
#' A tab whose own result covers the
#' matrix (the EFA tab's selected solution, the Items tab's diagnostics,
#' whose estimates include it) passes `asked = TRUE` once its own gate or
#' button has decided.
#'
#' @param state The shared state store.
#' @param requested See `sieve_state_basis()`.
#' @param asked `TRUE` when the researcher asked for the matrix (a result
#'   whose request covers it, or a page Update), or the `key` of a waiting
#'   record the button was pressed for (the module keeps it in a reactive
#'   value, so an edit that changes the pool asks again).
#' @return `NULL` when no data are loaded or no item is included; the list
#'   `sieve_state_basis()` returns when the matrix is kept, computed now, or
#'   may be computed by itself; otherwise a waiting record: `ok = FALSE`,
#'   `status = "not run"`, `waiting = TRUE`, `requested` and `used` (the
#'   correlations the request resolves to), `p`, `n`, `n_rows`, `items`,
#'   `key` (its identity, see `sieve_basis_key()`), `wait` and
#'   `standard_wait` (seconds at this machine's and at the standard rates),
#'   `label`, `restored`, `action` (the button's label, "Compute the
#'   correlations (about X)"), and `reason` (one sentence for the page).
#' @author Ken Kelley
#' @noRd
sieve_basis_demand <- function(state, requested = NULL, asked = FALSE) {
  pool <- sieve_state_items(state)
  if (is.null(pool) || !length(pool$items)) {
    return(NULL)
  }
  resolved <- sieve_basis_resolve(state, pool, requested)
  key <- sieve_basis_key(state, pool, resolved)
  held <- shiny::isolate(state$cache)$basis[[resolved]]
  kept <- !is.null(held) && identical(held$key, key)
  if (kept || isTRUE(asked) || identical(asked, key) || identical(resolved, "pearson")) {
    return(sieve_state_basis(state, requested))
  }
  p <- length(pool$items)
  wait <- sieve_basis_wait(p, resolved)
  standard <- sieve_at_default_rates(sieve_basis_wait(p, resolved))
  restored <- sieve_restored_project(state)
  if (!restored && sieve_runs_by_itself(wait, standard = standard)) {
    return(sieve_state_basis(state, requested))
  }
  label <- sieve_duration_label(wait)
  action <- sprintf("Compute the correlations (%s)", label)
  list(
    ok = FALSE, status = "not run", waiting = TRUE, cached = FALSE,
    requested = resolved, used = resolved, p = p, n = pool$n, n_rows = pool$n_rows,
    items = pool$items, key = key, wait = wait, standard_wait = standard, label = label,
    restored = restored, action = action,
    reason = sprintf(
      paste0(
        "The %s correlations of the %d included items have not been computed%s. Every ",
        "result on this page that reads the inter-item correlations waits for them: %s."
      ),
      sieve_retention_basis_label(resolved), p,
      if (restored) {
        " (this project was reopened: its recorded results are in the analysis registry and the saved-results report, and nothing is computed until asked)"
      } else {
        sprintf(" (estimated at %s, above the two seconds a first result may take by itself)", label)
      },
      action
    )
  )
}

#' The shared correlation basis of the included items
#'
#' The type-appropriate inter-item correlation matrix of the included
#' items: Pearson when every item is continuous, polychoric when every item
#' is ordered (or ordered and binary), tetrachoric when every item is
#' binary, and mixed when the confirmed types mix, with the confirmed sets
#' passed explicitly (see `sieve_retention_request()`). It is computed once,
#' through `sieve_retention_cor()`, and kept in `state$cache$basis` (a plain
#' list, so a saved project stays plain data) under the identity of its
#' inputs (`sieve_basis_key()`), one entry per resolved request, so that a
#' tab that asks for Pearson correlations and a tab on the automatic basis
#' do not compute each other's matrix again on every visit. A call with the
#' same identity returns the kept matrix without computing; a changed
#' identity computes again; a new item pool keeps only its own entries;
#' `sieve_new_dataset()` empties the cache. The tabs read the matrix
#' through `sieve_basis_demand()`, which applies the run policy before
#' calling this.
#'
#' When the matrix is computed inside a Shiny session the computation runs
#' under a progress message that names the correlations, the item count
#' and the estimated wait (from `sieve_basis_wait()`: the rate measured in
#' this process, or the default the first time). Outside a session it runs
#' silently. The computation itself is on one worker whatever the session's
#' `mc.cores` option, so that what psych reports is kept (see
#' `sieve_retention_cor()`). The first computation of a basis writes the R
#' that reproduces it to the reproducible code; later tabs that use the
#' matrix refer to `R_items` from that step.
#'
#' @param state The shared state store.
#' @param requested `NULL` or `"auto"` for the correlations the confirmed
#'   types imply, `"pearson"`, or `"poly"` (polychoric for every item,
#'   tetrachoric when all are binary); a resolved identifier (`"pearson"`,
#'   `"polychoric"`, `"tetrachoric"`, `"mixed"`) is accepted as well.
#' @return `NULL` when no data are loaded or no item is included; otherwise
#'   the list from `sieve_retention_cor()` with `key` (its identity),
#'   `cached` (`TRUE` when this call returned the kept matrix), and
#'   `n_rows` (rows of the item data) added.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny withProgress
sieve_state_basis <- function(state, requested = NULL) {
  pool <- sieve_state_items(state)
  if (is.null(pool) || !length(pool$items)) {
    return(NULL)
  }
  resolved <- sieve_basis_resolve(state, pool, requested)
  key <- sieve_basis_key(state, pool, resolved)
  # The cache is read in isolation: a reactive that computed the basis
  # must not depend on the field it writes. One entry per request.
  held <- shiny::isolate(state$cache)$basis[[resolved]]
  if (!is.null(held) && identical(held$key, key)) {
    out <- held$value
    out$cached <- TRUE
    return(out)
  }
  p <- length(pool$items)
  available <- identical(resolved, "pearson") &&
    !identical(sieve_estimator_effective("study", state), "ml")
  compute <- function() {
    prepared <- if (available) sieve_auxiliary_apply(list(missing = "fiml", ordered = NULL,
      observed = pool$items), state, pool$raw) else list(data = pool$X)
    if (!is.null(prepared$reason)) return(list(requested = resolved, used = resolved,
      ok = FALSE, reason = prepared$reason, p = p, n = pool$n, items = pool$items, seconds = NA_real_))
    result <- sieve_retention_cor(prepared$data, unname(pool$types), resolved,
      missing = if (available) "fiml" else "listwise", aux = prepared$spec$engine_args$aux)
    result$warnings <- c(result$warnings, prepared$warnings)
    result
  }
  label <- sieve_retention_basis_label(resolved)
  wait <- sieve_basis_wait(p, resolved)
  # Pearson correlations are immediate; the others say what they cost.
  b <- if (!is.null(shiny::getDefaultReactiveDomain()) && !identical(resolved, "pearson")) {
    withProgress(
      message = sprintf(
        "Computing the %s correlations of %d items (%s)", label, p,
        sieve_duration_label(wait)
      ),
      detail = "Every tab that uses the inter-item correlations shares this matrix; it is computed once per item pool.",
      value = 1,
      compute()
    )
  } else {
    compute()
  }
  if (isTRUE(b$ok) && is.finite(b$seconds)) {
    sieve_rate_note(b$used, b$seconds, sieve_pairs(p))
  }
  b$key <- key
  b$cached <- FALSE
  b$n_rows <- pool$n_rows
  sieve_log(
    state,
    sprintf(
      paste0(
        "Correlation basis of the included items (requested %s, used %s), shared by the ",
        "Screening, Dimensionality, EFA and Items tabs; %s, reference n = %d of %d."
      ),
      sieve_retention_basis_label(b$requested), sieve_retention_basis_label(b$used),
      if (identical(b$missing, "fiml")) "saturated FIML covariance estimated by EM, complete-case reference N" else "complete cases",
      b$n, pool$n_rows
    ),
    sieve_retention_cor_code(b),
    passive = TRUE
  )
  cache <- shiny::isolate(state$cache) %||% list()
  # Only the bases of the current pool are kept: a request made on another
  # pool is dropped, so the cache holds at most one matrix per request.
  pool_id <- key[c("dataset", "data", "types")]
  bases <- Filter(
    function(e) identical(e$key[c("dataset", "data", "types")], pool_id),
    cache$basis %||% list()
  )
  bases[[resolved]] <- list(key = key, value = b)
  cache$basis <- bases
  state$cache <- cache
  b
}

#' Timing units for an exploratory fit, with its actual sample and factors
#' @param p Number of indicators.
#' @param estimator Resolved estimator.
#' @param n Eligible model observations.
#' @param factors Actual numbers of factors.
#' @return Units relative to 500 observations and one factor.
#' @author Ken Kelley
#' @noRd
sieve_efa_time_units <- function(p, estimator, n, factors) {
  if (length(n) != 1L || !is.finite(n) || n < 1 ||
    !length(factors) || any(!is.finite(factors) | factors < 1)) return(rep(NA_real_, length(factors)))
  # Timings at p = 66, n = 500 and 1000, and k = 1 and 5 motivate these
  # smooth starting ratios. They are heuristics, not a general runtime law.
  sample_ratio <- if (identical(estimator, "ML")) .9 + .1 * n / 500 else .85 + .15 * n / 500
  sieve_fit_units(p, estimator) * sample_ratio * (1 + .07 * (factors - 1))
}

#' First-run exploratory timings before this session supplies observations
#' @param p,estimator,n,factors See sieve_efa_time_units().
#' @return Seconds per requested fit, including a worker-start allowance.
#' @author Ken Kelley
#' @noRd
sieve_efa_wait_baseline <- function(p, estimator, n, factors) {
  # Measured: MLR at p = 66, n = 500, k = 1 took 33.1 seconds; ML about
  # 2.1 seconds. The ordered-item timing of sieve_rate_defaults() supplies
  # WLSMV.
  rate <- switch(estimator,
    ML = 2.1 / sieve_fit_units(66, "ML"),
    WLSMV = 62 / sieve_fit_units(68, "WLSMV"),
    33.1 / sieve_fit_units(66, "MLR")
  )
  .7 + rate * sieve_efa_time_units(p, estimator, n, factors)
}

#' Learn a session correction from an accepted real exploratory worker
#' @param key Scientific worker request with p, n, and estimator.
#' @param k Actual factor count.
#' @param seconds Elapsed worker time including startup.
#' @return A timing record, or NULL for an unavailable measurement.
#' @author Ken Kelley
#' @noRd
sieve_efa_time_note <- function(key, k, seconds) {
  if (length(key$p) != 1L || length(key$n) != 1L || length(seconds) != 1L ||
    !is.finite(seconds) || seconds <= 0) return(NULL)
  baseline <- sieve_efa_wait_baseline(key$p, key$estimator, key$n, k)
  if (length(baseline) != 1L || !is.finite(baseline) || baseline <= 0) return(NULL)
  # Ratios retain fixed startup in both observed and expected times. This
  # avoids attributing all small-model worker startup to per-item work.
  sieve_rate_note(paste0("efa_elapsed_", key$estimator), seconds, baseline)
  list(
    p = key$p, n = key$n, factors = k, estimator = key$estimator,
    observed_seconds = seconds, baseline_seconds = baseline, ratio = seconds / baseline
  )
}

#' How requested numbers of factors are split over the workers
#'
#' The counts are taken in the order given (the selected count first) and
#' dealt out in consecutive runs, as evenly as the number of free workers
#' allows: eight counts on six workers give two workers two counts each and
#' four workers one each. A worker fits its counts in turn and shares what
#' does not depend on k (see `sieve_efa_solutions()`), so a second count on
#' a worker costs less than the first. The background queue starts its
#' workers with this split and the wait estimate assumes the same one.
#'
#' @param ks The counts, in the order to fit them.
#' @param workers The number of free workers.
#' @return A list of integer vectors, at most `workers` of them.
#' @author Ken Kelley
#' @noRd
sieve_efa_chunks <- function(ks, workers) {
  ks <- as.integer(ks)
  free <- max(1L, as.integer(sieve_finite_or(workers, 1L)))
  out <- list()
  while (length(ks) && free > 0L) {
    size <- ceiling(length(ks) / free)
    out <- c(out, list(ks[seq_len(size)]))
    ks <- ks[-seq_len(size)]
    free <- free - 1L
  }
  out
}

#' The time of a later solution on a worker, as a share of a first one
#'
#' A worker that fits several numbers of factors computes the data, the
#' sample statistics, the saturated model and the baseline model once (see
#' `sieve_efa_solutions()`). Measured on 68 seven-point items, n = 500
#' (one core): a second MLR solution took 25.7 seconds against 47.5 for the
#' same solution alone (0.54), a second WLSMV solution 17.7 against 40.5
#' (0.44), and on the 24 bundled tests a second ML solution 0.18 against
#' 0.21 (0.86).
#'
#' @param estimator `"MLR"`, `"WLSMV"`, or `"ML"`.
#' @return A share between 0 and 1.
#' @author Ken Kelley
#' @noRd
sieve_efa_later_share <- function(estimator) {
  switch(estimator %||% "MLR",
    MLR = 0.55,
    WLSMV = 0.45,
    0.85
  )
}

#' Estimated seconds of a set of exploratory solutions on the workers
#'
#' The requested counts are split over the workers as the background queue
#' splits them (`sieve_efa_chunks()`); each worker's time is its first
#' solution's estimate (`sieve_fit_wait()`, which includes the start of the
#' worker) plus `sieve_efa_later_share()` of each later one's; the workers
#' run at the same time, so the wait is the longest of them.
#'
#' @param p,estimator,n See `sieve_fit_wait()`.
#' @param ks The requested counts, in the order they run.
#' @param workers The number of workers.
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_efa_plan_wait <- function(p, estimator, n, ks, workers = 1L) {
  ks <- as.integer(ks)
  if (!length(ks)) {
    return(0)
  }
  share <- sieve_efa_later_share(estimator)
  times <- vapply(sieve_efa_chunks(ks, workers), function(chunk) {
    each <- vapply(chunk, function(k) {
      sieve_fit_wait(p, estimator, "efa", k = 1L, n = n, factors = k)
    }, numeric(1))
    each[1L] + share * sum(each[-1L])
  }, numeric(1))
  max(times)
}

#' Estimated seconds of the EFA tab's request, the selected model first
#'
#' The background queue fits the selected count first and alone, so that it
#' keeps the speed it has alone, and then splits the other counts over the
#' workers (see `sieve_efa_jobs_poll()`); the wait is the selected model's
#' estimate plus that of the others on the workers
#' (`sieve_efa_plan_wait()`). Without a selected count among `ks` it is the
#' plan of all of them.
#'
#' @param p,estimator,n See `sieve_fit_wait()`.
#' @param ks The requested counts.
#' @param workers The number of workers.
#' @param selected The selected count, or `NULL`.
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_efa_request_wait <- function(p, estimator, n, ks, workers = 1L, selected = NULL) {
  ks <- unique(as.integer(ks))
  if (length(selected) != 1L || !is.finite(selected) || !as.integer(selected) %in% ks) {
    return(sieve_efa_plan_wait(p, estimator, n, ks, workers))
  }
  selected <- as.integer(selected)
  sieve_fit_wait(p, estimator, "efa", k = 1L, n = n, factors = selected) +
    sieve_efa_plan_wait(p, estimator, n, setdiff(ks, selected), workers)
}

#' The workers of an exploratory plan, in words
#'
#' @param workers The number of workers the plan uses.
#' @param n_models The number of solutions requested.
#' @return A phrase to follow the estimated wait (empty for one solution).
#' @author Ken Kelley
#' @noRd
sieve_efa_workers_words <- function(workers, n_models) {
  workers <- max(1L, as.integer(sieve_finite_or(workers, 1L)))
  if (n_models <= 1L) {
    return("")
  }
  if (workers == 1L) {
    return(" on one background worker, which fits the models in turn")
  }
  sprintf(" on %d background workers at once (separate R processes)", min(workers, n_models))
}

#' Estimated peak memory of one exploratory worker
#'
#' Measured as the peak resident memory of one R process fitting one
#' lavaan::efa solution (n = 500), on the first p items of a pool of 303
#' items and on a pool of 68 seven-point items. Under MLR the robust test
#' statistic and its baseline model hold several matrices with as many rows and
#' columns as there are sample moments, p* = p (p + 3) / 2, and the memory
#' grows with the square of p*: 0.57 GB at p = 50, 2.2 GB at p = 80, 6.3
#' GB at p = 110, and 1.3 GB for the 68 seven-point items; about 0.6 GB
#' plus 147 bytes for each of the p*^2 elements, which at p = 303 is over
#' 300 GB, beyond any laptop. Under ML it grows with p^3: 0.27 GB at
#' p = 100, 0.47 GB at 150, 0.86 GB at 200 and 2.4 GB at 303, about 0.22 GB
#' plus 80 bytes times p^3. Under WLSMV it follows the weight matrix of the
#' thresholds and polychoric correlations (`sieve_wlsmv_weight_bytes()`):
#' 1.4 GB for the 68 seven-point items, whose weight matrix is 58
#' MB, about 0.3 GB plus 20 times the weight matrix, and at large samples
#' the casewise scores add a term in n (`sieve_wlsmv_peak_bytes()`, the
#' estimate the WLSMV guard uses: 1.17 GB at 40 four-category items and
#' n = 20,000, where the weight matrix is 6.5 MB).
#'
#' @param p The number of items.
#' @param estimator `"MLR"`, `"WLSMV"`, or `"ML"`.
#' @param categories For WLSMV, the number of categories of each item (one
#'   value is repeated); seven by default.
#' @param n For WLSMV, the number of respondents, or `NA` when it is not
#'   known (the casewise scores are then left out).
#' @return Bytes.
#' @author Ken Kelley
#' @noRd
sieve_efa_worker_bytes <- function(p, estimator, categories = 7, n = NA_real_) {
  p <- as.numeric(p)
  switch(estimator %||% "MLR",
    WLSMV = sieve_wlsmv_peak_bytes(as.integer(p), categories, n),
    ML = 2.2e8 + 80 * p^3,
    6e8 + 147 * (p * (p + 3) / 2)^2
  )
}

#' The memory the concurrent exploratory workers may plan to hold
#'
#' Eight gigabytes, or the option `sieve.efa_memory` (bytes) when it is
#' set. It limits how many workers run at once, never whether a solution
#' is fit.
#'
#' @return Bytes.
#' @author Ken Kelley
#' @noRd
sieve_efa_memory_budget <- function() {
  opt <- suppressWarnings(as.numeric(getOption("sieve.efa_memory")))
  if (length(opt) == 1L && is.finite(opt) && opt > 0) opt else 8e9
}

#' The most exploratory workers that fit in the memory budget
#'
#' @param p,estimator,n See `sieve_efa_worker_bytes()`.
#' @return A whole number, at least 1 (one worker always runs).
#' @author Ken Kelley
#' @noRd
sieve_efa_worker_cap <- function(p, estimator, n = NA_real_) {
  bytes <- sieve_efa_worker_bytes(p, estimator, n = n)
  if (!is.finite(bytes) || bytes <= 0) {
    return(1L)
  }
  max(1L, as.integer(floor(sieve_efa_memory_budget() / bytes)))
}

#' Show the uncertainty of an exploratory wait estimate
#' @param seconds Central estimated seconds.
#' @return A concise approximate range; no calibration guarantee.
#' @author Ken Kelley
#' @noRd
sieve_efa_wait_label <- function(seconds) {
  if (!is.finite(seconds) || seconds < 2) return(sieve_duration_label(seconds))
  lower <- sub("^about ", "", sieve_duration_label(.7 * seconds))
  upper <- sub("^about ", "", sieve_duration_label(1.5 * seconds))
  paste("roughly", lower, "to", upper)
}

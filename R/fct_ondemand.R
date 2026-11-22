# fct_ondemand.R
# Model fits on request. In a study of 500 respondents by 68 ordered items,
# a CFA, Reliability, Validity, SEM or IRT tab that fit its model at
# Confirm, after every ledger edit, or as soon as the tab was shown would
# keep the researcher waiting with nothing on the page saying what was
# running or how long it would take. The rule: nothing that fits a model
# runs at Load, at Confirm, or on an edit. A first result may run by itself
# on the first view of its page under one gate, written once in
# sieve_runs_by_itself() and sieve_restored_project(): an estimate of two
# seconds or less at the standard rates, a measured estimate of at most six,
# the first result of its kind for the dataset, the page visible, and not a
# reopened project. Every other fit has a Run button whose label states the
# estimated time, runs under a progress message naming the computation,
# announces when it is done or has failed, and is kept with the revision it
# was computed from.
# When the included items, their types, constructs, facets or keying change,
# the tab says in one line what changed and that the result below is from
# before; a label or wording edit changes nothing; pressing Run with nothing
# changed reuses the stored result and says so.
#
# sieve_demand_wire() is the one implementation of that rule, shared by the
# six tabs. A module gives it the identity its result depends on (key), the
# estimate and the guard (plan), and the computation (compute); it gets back
# the status, the result, the button and the status line.

#' The longest estimated wait of a result that runs by itself
#'
#' Two seconds. See `sieve_run_policy_text()`.
#'
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_auto_seconds <- function() 2

#' Refit Requested Results After an Inclusion Change
#' @param state Shared study state.
#' @param update Function updating previously computed results only.
#' @return A list with the observer and a cancel function for an explicit user stop.
#' @author Ken Kelley
#' @noRd
sieve_inclusion_wire <- function(state, update) {
  memory <- new.env(parent = emptyenv())
  memory$previous <- NULL
  memory$pending <- FALSE
  current <- function() list(dataset = state$dataset_id, items = state$meta$item, status = state$meta$status)
  observer <- shiny::observe({
    now <- current()
    before <- memory$previous
    memory$previous <- now
    same <- !is.null(before) && identical(before$dataset, now$dataset) && identical(before$items, now$items)
    if (!same) memory$pending <- FALSE
    if (same && !identical(before$status, now$status)) memory$pending <- TRUE
    if (memory$pending && isTRUE(sieve_confirmation_status(state)$confirmed)) {
      accepted <- shiny::isolate(update())
      if (identical(accepted, FALSE)) shiny::invalidateLater(250) else memory$pending <- FALSE
    }
  })
  list(observer = observer, cancel = function() {
    memory$pending <- FALSE
    memory$previous <- shiny::isolate(current())
    invisible(NULL)
  })
}

#' How far this machine's measured time may exceed the limit
#'
#' A result whose standard estimate is within `sieve_auto_seconds()` still
#' waits when the rates measured on this machine put it above this multiple
#' of the limit (six seconds), so a slow machine is not surprised by a long
#' wait while ordinary variation in measured times changes nothing.
#'
#' @return A multiple.
#' @author Ken Kelley
#' @noRd
sieve_auto_slack <- function() 3

#' Does a result run by itself? The run policy, written once
#'
#' A first result runs by itself only when its page is visible and its
#' estimated wait is `sieve_auto_seconds()` or less. Everything longer runs
#' on request with its estimate stated; an unknown estimate counts as on
#' request; a result made stale by an edit is marked and is updated by the
#' page's action, not by itself. The one exception is the Items tab's per
#' item diagnostics, which stay live while their estimate is within the same
#' two seconds (`live = TRUE`), because they are the tab's working surface
#' for one exclusion after another. Every page that fits a model or runs a
#' long computation reads its decision here, and reads
#' `sieve_restored_project()` beside it (a reopened project waits
#' everywhere): CFA and Reliability (`sieve_demand_wire()`), the EFA tab's
#' selected solution and its fit indices, the Items tab's diagnostics and
#' overall model, the Dimensionality tab's parallel analysis and VSS and
#' model fit by k, the shared correlation basis (`sieve_basis_demand()`),
#' the Validity tab's Harman screen and the Reliability tab's planner (its
#' Monte Carlo). `first` is decided per dataset: data loaded after another
#' dataset get a first result of their own.
#'
#' @param seconds The estimated wait.
#' @param first `TRUE` when no result of this kind has been computed yet
#'   for the dataset in this session (a first result).
#' @param visible `TRUE` when the result's page is the one shown. Inside a
#'   render, Shiny evaluates nothing for a hidden page, so a reactive read
#'   only by the page's outputs is visible whenever it is evaluated.
#' @param live `TRUE` for a result that follows edits by itself while it
#'   is quick (the Items diagnostics): `first` is then not required.
#' @param standard The estimate at the standard rates
#'   (`sieve_at_default_rates()`), which decides; `seconds`, the estimate
#'   at this machine's measured rates, then only stops a result it puts
#'   above `sieve_auto_slack()` times the limit. Without it `seconds`
#'   decides.
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_runs_by_itself <- function(seconds, first = TRUE, visible = TRUE, live = FALSE, standard = NULL) {
  policy <- getOption("sieve.auto_run", "default")
  if (identical(policy, FALSE) || identical(policy, "never")) return(FALSE)
  if (identical(policy, TRUE) || identical(policy, "always")) {
    return(isTRUE(visible) && (isTRUE(live) || isTRUE(first)))
  }
  known <- function(x) length(x) == 1L && is.finite(x)
  quick <- if (is.null(standard)) {
    known(seconds) && seconds <= sieve_auto_seconds()
  } else {
    known(standard) && standard <= sieve_auto_seconds() &&
      !(known(seconds) && seconds > sieve_auto_slack() * sieve_auto_seconds())
  }
  quick && isTRUE(visible) && (isTRUE(live) || isTRUE(first))
}

#' Were the loaded data restored from a saved project?
#'
#' A reopened project waits on every page: its recorded results are in the
#' analysis registry and the saved-results report, and nothing is refit
#' until a button asks for it. Every automatic path reads this one check
#' beside `sieve_runs_by_itself()`: the shared controller
#' (`sieve_demand_wire()`), the EFA tab's selected solution and its fit
#' indices, the Items tab's diagnostics and overall model, the
#' Dimensionality tab's parallel analysis and VSS and model fit by k, the
#' shared correlation basis (`sieve_basis_demand()`), the Harman screen and
#' the reliability planner's Monte Carlo. Data loaded after the project (a
#' new dataset identity, see `sieve_new_dataset()`) are not restored.
#'
#' @param state The shared state store (read reactively inside a reactive
#'   context, so a caller is evaluated again when a project is opened or
#'   other data are loaded).
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_restored_project <- function(state) {
  project <- state$project
  !is.null(project) && identical(project$dataset_id, state$dataset_id)
}

#' The one sentence a page states while a reopened project waits
#'
#' @param button How the page names its button (`"Fit the model"`).
#' @return One sentence.
#' @author Ken Kelley
#' @noRd
sieve_restored_sentence <- function(button = "its button") {
  sprintf(
    paste0(
      "This project was reopened: its recorded results are in the analysis registry and ",
      "the saved-results report, and nothing is refit until you press %s."
    ),
    button
  )
}

#' The results that may run by themselves, written once
#'
#' One row per result whose controller reads `sieve_runs_by_itself()`:
#' `file` is the source file of that controller (the CFA and the
#' Reliability table share `sieve_demand_wire()`), and `long` is the result
#' in the words of the run policy, which `sieve_run_policy_text()` reads
#' for the page guide and the About dialog; a test keeps the files here and
#' the files that call the rule in step. The page guide's paragraph is the
#' one statement of the policy; the Ingest status chip does not repeat it.
#'
#' @return A data frame with character columns `file` and `long`.
#' @author Ken Kelley
#' @noRd
sieve_run_policy_results <- function() {
  data.frame(
    file = c(
      "fct_ondemand.R", "fct_ondemand.R", "mod_efa.R", "mod_winnow.R",
      "mod_retention.R", "fct_basis.R", "fct_fit.R", "mod_validity.R"
    ),
    long = c(
      "the CFA",
      "the Reliability table",
      "the EFA tab's selected solution and its fit indices",
      "the Items tab's item diagnostics and overall model",
      "the Dimensionality tab's parallel analysis and VSS, and model fit by k",
      "the shared correlation matrix of the included items (Pearson correlations take no time and are always computed)",
      "the unbiased SRMR with its interval beside the SRMR on the CFA and SEM tabs",
      "Harman's single-factor screen on the Validity tab"
    ),
    stringsAsFactors = FALSE
  )
}

#' The results that may run by themselves, as one list in words
#'
#' The results of one controller joined by "and", the controllers by
#' semicolons, in the words of the run policy.
#'
#' @return One string.
#' @author Ken Kelley
#' @noRd
sieve_run_policy_list <- function() {
  x <- sieve_run_policy_results()
  join <- function(parts, sep, last) {
    n <- length(parts)
    if (n < 2L) return(paste(parts, collapse = ""))
    paste0(paste(parts[-n], collapse = sep), last, parts[n])
  }
  clauses <- vapply(split(x$long, factor(x$file, levels = unique(x$file))),
    function(parts) join(parts, ", ", " and "), character(1)
  )
  join(unname(clauses), "; ", "; and ")
}

#' The run policy in words
#'
#' The paragraph the About dialog and the page guide state, so the rule the
#' code follows (`sieve_runs_by_itself()`, `sieve_restored_project()`) is
#' said once, with every result that may run by itself, every exception,
#' and the dataset and reopened-project rules.
#'
#' @return One paragraph.
#' @author Ken Kelley
#' @noRd
sieve_run_policy_text <- function() {
  paste(
    "A first result runs by itself only under one rule: its page is shown, its estimated time",
    "is two seconds or less, the times measured on this machine do not put it above six",
    "seconds, it is the first result of its kind for the loaded data, and the project was not",
    "reopened from a file. That estimate depends only on the size of the problem, so the same",
    "page behaves the same way on every visit. The results that may run by themselves this",
    sprintf("way are %s.", sieve_run_policy_list()),
    "Everything else (the Validity, SEM, Bifactor, Network, IRT, and CV and Invariance models, the",
    "Optimizer, the reliability planner's Monte Carlo, and every bootstrap) waits for its",
    "button, which states the estimate for this machine, whatever that estimate is; a result",
    "whose time cannot be estimated waits as well. A page that waits still shows, without a",
    "click, a fit this session already made under the identical identity on another page",
    "(the same model, data, rows, estimator and settings), saying which page fit it and when:",
    "nothing is estimated for it. Data loaded after other data get a first result of their own; a result of the",
    "earlier data is kept in the analysis registry and the saved-results report, not shown.",
    "A reopened project waits on every page: its recorded results are in the registry and the",
    "report, and nothing is refit until you press the page's button. After an edit that",
    "changes what a result was computed from, the result is marked as from before and is",
    "updated only when you press the page's Update. Two exceptions to the first-result rule:",
    "the Items tab's item diagnostics follow each inclusion, exclusion or reassignment by",
    "themselves while their estimate is within two seconds (and otherwise wait for Update),",
    "and on the EFA tab choosing the number of factors or a range of k is itself the request",
    "for the fit indices, which then start in background R processes from the shown page",
    "once the status line states their scope and estimate and the memory guard allows them."
  )
}

#' Evaluate an expression and keep what it signaled, letting Shiny's silent
#' errors through
#'
#' `sieve_capture()` converts every error into a recorded failure. Inside a
#' Shiny render or reactive that is not wanted for the one error that is
#' not a failure: the silent error [shiny::req()] and [shiny::validate()]
#' raise to say that an input is not ready, which must reach Shiny so the
#' output shows nothing. This wrapper captures everything else and lets
#' that one through, so a render can show a real error in place of the
#' `tryCatch(..., error = function(e) NULL)` that would swallow it.
#'
#' @param expr An expression to evaluate.
#' @return The list `sieve_capture()` returns (`value`, `error`,
#'   `warnings`, `messages`, `ok`).
#' @author Ken Kelley
#' @noRd
sieve_attempt <- function(expr) {
  seen <- new.env(parent = emptyenv())
  seen$silent <- NULL
  run <- sieve_capture(
    tryCatch(expr, shiny.silent.error = function(e) {
      seen$silent <- e
      NULL
    })
  )
  if (!is.null(seen$silent)) {
    stop(seen$silent)
  }
  run
}

#' Evaluate an expression and say whether it was ready at all
#'
#' For a render that shows a result when there is one, a message saying
#' what to press when there is none yet, and the error when the result
#' could not be computed. A silent error from [shiny::req()] or an
#' [shiny::eventReactive()] whose event has not happened is "not ready"
#' (`ready = FALSE`, no error); every other error is kept, as in
#' `sieve_capture()`.
#'
#' @param expr An expression to evaluate.
#' @return The list `sieve_capture()` returns, with `ready` added.
#' @author Ken Kelley
#' @noRd
sieve_outcome <- function(expr) {
  seen <- new.env(parent = emptyenv())
  seen$silent <- FALSE
  run <- sieve_capture(
    tryCatch(expr, shiny.silent.error = function(e) {
      seen$silent <- TRUE
      NULL
    })
  )
  run$ready <- !seen$silent
  run
}

#' Show a notification when there is a session to show it in
#'
#' The one path every pop-up notification of the app takes (a source test
#' fails on a direct `sieve_notify()` anywhere else in `R/`), so the
#' notification policy is applied in one place. A message confirms a change
#' the researcher just made or a run that just finished and lasts at most
#' `sieve_notify_message_seconds()` seconds; a warning or an error always
#' stays until it is dismissed, whatever duration the caller gives, as the
#' user guide states. With `id`, a notification replaces the one shown under
#' the same `id` instead of stacking beside it, so one action keeps one
#' notification. A notification that stays until it is dismissed (every
#' warning and error, and a message given `duration = NULL`) and is given no
#' `id` takes one from its type and text, so the same warning or error
#' raised again replaces the one on screen instead of stacking a copy that
#' must be dismissed as well (a refusal repeated, such as Exclude pressed
#' with nothing picked, shows one copy, not one per press). A message with a
#' duration is given no `id`, since it leaves on its own. The notification
#' panel is announced to screen readers and its close control is reachable
#' and operable from the keyboard (`inst/app/www/sieve.js`,
#' `sieveNotificationAccess()`); what needs attention beyond the moment
#' belongs in the page's status line instead.
#'
#' @param message The text, or a tag.
#' @param type `"message"`, `"warning"`, or `"error"`.
#' @param duration For a message: seconds, at most
#'   `sieve_notify_message_seconds()`, or `NULL` for a message that stays
#'   until dismissed; when omitted, `sieve_notify_message_seconds()`. A
#'   warning or an error stays until dismissed, and a duration given for one
#'   is not applied.
#' @param id An identifier, or `NULL`. A notification shown with the `id` of
#'   one still on screen replaces it; a warning or an error (or a message
#'   that stays until dismissed) given no `id` takes one from its type and
#'   text, so the same one repeated replaces itself instead of stacking.
#' @return `NULL`, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_notify <- function(message, type = "message", duration, id = NULL) {
  type <- match.arg(type, c("message", "warning", "error", "default"))
  if (!type %in% c("message", "default")) {
    duration <- NULL
  } else if (missing(duration)) {
    duration <- sieve_notify_message_seconds()
  } else if (!is.null(duration)) {
    duration <- min(duration, sieve_notify_message_seconds())
  }
  session <- shiny::getDefaultReactiveDomain()
  if (!is.null(session)) {
    if (is.null(id) && is.null(duration)) {
      id <- paste0("sieve-note-", sieve_hash(list(type, as.character(message))))
    }
    shiny::showNotification(message, type = type, duration = duration, id = id, session = session)
  }
  invisible(NULL)
}

#' How long a message notification stays
#'
#' A message only confirms what just happened; five seconds is enough to
#' read one sentence, and anything longer belongs on the page.
#'
#' @return `5`.
#' @author Ken Kelley
#' @noRd
sieve_notify_message_seconds <- function() 5

#' Seconds in words, for a notification
#'
#' @param seconds A duration.
#' @return `"20.7 seconds"`, `"3.2 minutes"`, or `"an unknown time"`.
#' @author Ken Kelley
#' @noRd
sieve_seconds_text <- function(seconds) {
  if (length(seconds) != 1L || !is.finite(seconds)) {
    return("an unknown time")
  }
  if (seconds < 120) {
    return(sprintf("%.1f seconds", seconds))
  }
  sprintf("%.1f minutes", seconds / 60)
}

#' The lavaan estimators that need the asymptotic covariance matrix of the
#' thresholds and polychoric correlations
#'
#' @return A character vector.
#' @author Ken Kelley
#' @noRd
sieve_categorical_estimators <- function() {
  c("WLSMV", "WLSM", "WLS", "DWLS", "ULSMV", "ULSM", "ULS")
}

#' The rate class of an estimator, for the wait estimates
#'
#' ML with FIML (the default for continuous items) is in MLR's class: its
#' time on a page is that of the corrected fit indices of a FIML fit (Zhang
#' & Savalei, 2023), which lavaan forms from the information matrices of the
#' unrestricted model in every `lavaan::fitMeasures()` call and DMAR's check
#' of the unrestricted model forms once more, and which grow, as MLR's, with
#' the square of the number of sample moments. The fit itself is quick since
#' SIEVER asks for the standard test alone (`sieve_spec_default_test()`:
#' 0.12 seconds at 40 items), but the CFA page's path (`sieve_run_cfa()` and
#' `sieve_fit_measures()`; four correlated factors, n = 500, complete data)
#' took 0.81, 2.3 and 6.2 seconds at 24, 32 and 40 items, where the first
#' estimate of MLR's class (`sieve_fit_wait()`) is 0.74, 2.4 and 5.9
#' seconds; MLR took 1.6, 5.2 and 15.2, and ML on the complete cases 0.05 to
#' 0.09 (see `sieve_ml_bytes()`). lavaan's default tests, which add Browne's
#' residual based test, would take 1.3, 5.5 and 19.6 seconds. The ML class
#' is ML on the complete cases.
#'
#' @param estimator A lavaan estimator name.
#' @param missing The missing data treatment (`"fiml"` or `"listwise"`),
#'   or `NULL` when unknown (read as the complete cases).
#' @return `"WLSMV"` for every categorical estimator, `"ML"` for plain
#'   maximum likelihood on the complete cases, otherwise `"MLR"`.
#' @author Ken Kelley
#' @noRd
sieve_demand_class <- function(estimator, missing = NULL) {
  if (is.null(estimator) || !length(estimator)) {
    return("MLR")
  }
  if (estimator[1L] %in% sieve_categorical_estimators()) {
    "WLSMV"
  } else if (identical(estimator[1L], "ML") && !identical(missing, "fiml")) {
    "ML"
  } else {
    "MLR"
  }
}

#' How much longer than one CFA a kind of computation takes, before it has
#' been timed in this process
#'
#' The first estimate of a wait comes from the CFA rate per item pair (see
#' `sieve_rate_defaults()`, or the CFA fits already timed in this process)
#' scaled by what the kind of computation costs relative to one
#' correlated-factors CFA of the same items, timed in the same process (the
#' CFA's time covers its fit measures and standardized solution, read with
#' the fit). Measured on a study of 500 respondents by 68 ordered items
#' (WLSMV), in three runs on one computer, against the CFA of those runs
#' (91 to 110 seconds): the reliability analysis of the construct
#' (categorical omega, H and alpha, each a one-factor model of its items)
#' took 1.1 to 1.5 times the CFA; the graded response model 1.45 times; the
#' bifactor model with the omega of the raw total score 1.1 to 1.5 times
#' together; the measurement model behind the factor scores 0.3 to 0.4 times
#' (no fit measures are read); the method-factor model, compared with a
#' current substantive model, 0.2 to 0.3 times. Every computation timed
#' afterwards replaces the scaled estimate with its own rate (see
#' `sieve_demand_note()`).
#'
#' @param kind A kind of computation (`"cfa"`, `"sem"`, `"bifactor"`,
#'   `"method_factor"`, `"irt"`, `"reliability"`, `"scores"`,
#'   `"total_omega"`; the network models have `sieve_network_wait()`).
#' @return A single positive number.
#' @author Ken Kelley
#' @noRd
sieve_demand_multiplier <- function(kind) {
  m <- c(
    cfa = 1, sem = 1, bifactor = 0.8, method_factor = 0.3, irt = 1.45,
    reliability = 1.3, scores = 0.35, total_omega = 0.6
    # The network models have their own wait, from their measured times
    # (sieve_network_wait(), fct_network.R): their time grows with the item
    # pairs, and a multiplier of the CFA would state about 65 seconds for a
    # search of 68 items that runs longer than an hour.
  )
  if (kind %in% names(m)) unname(m[[kind]]) else 1
}

#' Estimated seconds for a kind of model fit
#'
#' The rate measured for this kind of computation and estimator in this
#' process, or, before any, the CFA rate scaled by
#' `sieve_demand_multiplier()`, times the number of item pairs (the cost of
#' a WLSMV fit is dominated by the asymptotic covariance matrix of the
#' thresholds and polychoric correlations, which grows with the pairs; an
#' ML fit grows with the pairs through the implied covariance matrix, at a
#' much smaller rate).
#'
#' @param kind See `sieve_demand_multiplier()`.
#' @param p The number of items, or a vector of item counts (one per
#'   construct, for a computation made construct by construct).
#' @param estimator A lavaan estimator name.
#' @param k The number of such fits.
#' @param missing The missing data treatment (see `sieve_demand_class()`).
#' @return Seconds, or `NA` when no rate is known.
#' @author Ken Kelley
#' @noRd
sieve_demand_wait <- function(kind, p, estimator = "MLR", k = 1L, missing = NULL) {
  class <- sieve_demand_class(estimator, missing)
  # A CFA is estimated by sieve_fit_wait() itself (which reads the rate the
  # CFA fits of this process measured); every other kind by its own measured
  # rate, or, before any, by the CFA estimate scaled by the kind.
  own <- if (identical(kind, "cfa")) NA_real_ else sieve_rate(sprintf("%s_%s", kind, class))
  seconds <- if (is.finite(own)) {
    own * sum(sieve_fit_units(p, class))
  } else {
    # One computation of several small fits (reliability construct by
    # construct) pays the fixed overhead once, not once per construct: five
    # three- to six-item constructs, estimated at 3.25 seconds one by one,
    # took 0.8.
    each <- sieve_fit_wait(p, class, "cfa", floor = FALSE)
    measured <- exists(sprintf("cfa_%s", class), envir = sieve_rates, inherits = FALSE)
    total <- if (measured) sum(each) else max(0.5, sum(each))
    total * sieve_demand_multiplier(kind)
  }
  if (!is.finite(seconds)) {
    return(NA_real_)
  }
  seconds * max(1, as.numeric(k))
}

#' Record the time a kind of model fit took
#'
#' @param kind See `sieve_demand_multiplier()`.
#' @param estimator A lavaan estimator name.
#' @param seconds The elapsed seconds.
#' @param p The number of items, or one count per construct.
#' @param missing The missing data treatment (see `sieve_demand_class()`).
#' @return The rate now in force, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_demand_note <- function(kind, estimator, seconds, p, missing = NULL) {
  class <- sieve_demand_class(estimator, missing)
  sieve_rate_note(
    sprintf("%s_%s", kind, class), seconds,
    sum(sieve_fit_units(p, class))
  )
}

#' The identity of a result computed on request
#'
#' The dataset, the content key of the item data, the analytic ledger key
#' (see `sieve_meta_key()`: items, constructs, facets, types, inclusion and
#' keying, not labels), original respondent-row identities, and the choices
#' the model was computed under. Read
#' inside a reactive, so the caller is evaluated again when any of them
#' changes; a label or wording edit changes none of them.
#'
#' Currency on the effective inputs: a caller that names the `items` its
#' model reads gets a key over those columns of the data and those rows of
#' the ledger only, so an edit to an item outside the model (the keying of
#' an excluded item, the values of a column the model does not read) leaves
#' the result current, while every change to a modeled item (its inclusion,
#' type, keying, construct, or values) still marks it stale. `facets =
#' FALSE` leaves the facet column out of the ledger key, for a model that
#' does not read facets (the first-order CFA; `sieve_cfa_plan()` writes
#' facets into the higher-order and the direct facet models only). The
#' dataset, the respondent rows and the choices are always part of the key.
#'
#' @param state The shared state store.
#' @param choices A list of the model choices (structure, identification,
#'   scope, settings, selected outcomes, and so on).
#' @param items The names of the items the model reads, or `NULL` for every
#'   column of the item data and every row of the ledger.
#' @param facets `FALSE` for a model that does not read facet assignments.
#' @return A list with `dataset`, `data`, `meta`, `rows` and `choices`.
#' @author Ken Kelley
#' @noRd
sieve_demand_key <- function(state, choices = list(), items = NULL, facets = TRUE) {
  data <- state$items
  meta <- state$meta
  if (!is.null(items)) {
    items <- as.character(items)
    if (!is.null(data)) {
      data <- as.data.frame(data)[, intersect(names(data), items), drop = FALSE]
    }
    if (!is.null(meta)) {
      meta <- meta[meta$item %in% items, , drop = FALSE]
    }
  }
  if (!isTRUE(facets) && !is.null(meta) && "facet" %in% names(meta)) {
    meta$facet <- NULL
  }
  list(
    dataset = state$dataset_id %||% NA_character_,
    data = sieve_data_key(data),
    meta = sieve_meta_key(meta),
    rows = sieve_hash(state$case_index %||% if (!is.null(state$items)) seq_len(nrow(state$items)) else NULL),
    choices = choices
  )
}

#' The respondents who left or joined the analysis, in one phrase
#'
#' For the line that says why a kept result is from before: who was removed
#' from the analysis (Screening tab) or restored to it since the result was
#' computed, by their rows of the data as loaded, with the n then and now.
#'
#' @param then,now The rows of the data as loaded in the analysis then and
#'   now (see `sieve_case_index()`); `then` may be `NULL` when it was not
#'   recorded.
#' @param most How many rows to name in each part.
#' @return One phrase without a final period.
#' @author Ken Kelley
#' @noRd
sieve_rows_sentence <- function(then, now, most = 5L) {
  if (is.null(then)) {
    return(sprintf("the respondents in the analysis changed (n = %d now)", length(now)))
  }
  named <- function(rows, what) {
    n <- length(rows)
    if (!n) {
      return(NULL)
    }
    shown <- paste(utils::head(sort(rows), most), collapse = ", ")
    if (n > most) shown <- sprintf("%s and %d more", shown, n - most)
    sprintf(
      "%d respondent%s %s (row%s %s of the data as loaded)",
      n, if (n == 1L) "" else "s", what, if (n == 1L) "" else "s", shown
    )
  }
  parts <- c(
    named(setdiff(then, now), "removed from the analysis"),
    named(setdiff(now, then), "restored to the analysis")
  )
  if (!length(parts)) {
    parts <- "the respondents in the analysis changed"
  }
  sprintf("%s; n = %d then, %d now", paste(parts, collapse = "; "), length(then), length(now))
}

#' Seconds since a background computation started
#'
#' The time so far that the status line of a computation running outside
#' this process states (`sieve_demand_wire()`), read from the clock each
#' time the line's progress text is redrawn.
#'
#' @param started The `POSIXct` time the computation started.
#' @return The elapsed seconds, never negative.
#' @author Ken Kelley
#' @noRd
sieve_demand_elapsed <- function(started) {
  max(0, as.numeric(difftime(Sys.time(), started, units = "secs")))
}

#' Wire a model fit to its Run button
#'
#' The one implementation of the on-request rule (see the file header). The
#' module supplies:
#'
#' * `key`: a reactive returning `sieve_demand_key()` for the current
#'   inputs and choices (it may `req()` what the fit needs, such as the
#'   confirmation of the item types);
#' * `plan`: a reactive returning a list with `seconds` (the estimated
#'   wait), `label` (the wait in words), `progress` (the computation named
#'   for the progress bar and the notifications, such as `"CFA: WLSMV, 68
#'   items"`), `detail` (a line for the progress bar), `sentence` (the
#'   estimate as the status line states it before the run), `p` (the item
#'   count, or one per construct, for the rate measured afterwards),
#'   `estimator`, `missing` (the missing data treatment, which puts ML with
#'   FIML in MLR's rate and memory class, see `sieve_demand_class()`; `NULL`
#'   reads as the complete cases), and `refused` (`NULL`, or the message
#'   that refuses the run before it starts, see `sieve_wlsmv_guard()`);
#' * `compute`: a function of the key and the plan that makes the
#'   computation and returns its result (a list; an element `error` that
#'   is a string means the engine produced no solution).
#'
#' One result is kept per set of choices, each with the revision it was
#' computed from and the analytic ledger of that moment. The status for the
#' current choices is `not_run`, `current`, `stale` (with the sentence
#' from `sieve_stale_sentence()`), or `failed` (the computation stopped
#' with an error). Pressing the button while the result is current reuses
#' it and says so; otherwise the computation runs under a progress message
#' naming it and its estimate, a notification says what ran and how long it
#' took, and a failure is announced by a notification that stays and by the
#' status line.
#'
#' With `background`, the computation runs outside this R process, so the
#' session stays usable and the computation can be canceled: pressing the
#' button starts it and returns at once, the status is `running` (with what
#' it was before: a result from before stays on the page, marked as such)
#' and the line says what is running, since when, its estimate and the time
#' so far, with a Cancel button; when the computation ends its result is
#' completed in this process and kept exactly as a computation made here
#' would be. One computation runs per wire: a request with other inputs
#' replaces the one running, and the session's end stops it. A handle that
#' is no longer running when it is returned (a computation made in this
#' process because none can run outside it) is completed at once.
#'
#' The line of a running computation (and the line of one still running for
#' the inputs before a change) is drawn once per run, with its Cancel button
#' in an element of its own. Only the time so far is redrawn each second: a
#' text output of the wire, `<id>_progress`, inside the line, in a span with
#' role `timer` (whose live region is off, so a screen reader does not read
#' the line again every second). Redrawing the whole line each second would
#' replace the Cancel button each time: keyboard focus on it would fall to
#' the page body, and a press whose release came after a redraw would land
#' on a button no longer in the page. The page that places `line()` must not
#' itself redraw it on a timer, and places it once, since the output id is
#' the wire's.
#'
#' @param input,session The module's input and session.
#' @param state The shared state store.
#' @param id The input id of the Run button.
#' @param what A short name of the computation, for the record (for
#'   example `"CFA"`).
#' @param key,plan,compute See above.
#' @param capability Sampling-design capability; independent by default.
#' @param kind The kind of computation for the measured rate (see
#'   `sieve_demand_note()`), or `NULL` to measure nothing.
#' @param noun How the status line names the result (`"The model"`).
#' @param verb What is done to it: `"fit"` or `"computed"`.
#' @param action The button's label for a first run; `refit` for a run
#'   after a change.
#' @param stale_lead The first sentence of the line after a change.
#' @param background `NULL` to compute in this process (`compute`), or a
#'   list of `start`, a function of the key and the plan that starts the
#'   computation outside this process and returns a handle (an environment
#'   with `is_alive()`, `kill()` and `get_result()`); `finish`, a function
#'   of the key, the plan, the handle's result, the handle and `current`
#'   (whether the data, the ledger and the rows in the analysis are still
#'   those it started with) that completes
#'   the result in this process (its records and code) and returns the
#'   value kept; and `in_process`, a function returning `TRUE` when `start`
#'   computes in this process (no worker can be started), so that the
#'   progress message is shown.
#' @param fitted `NULL`, or a function of a kept value that returns the
#'   variable names of the items the result was fitted to, or a list with
#'   `items`, `respondents` (the rows the fit read) and `included` (how
#'   many items the study includes, when the model may use fewer of them);
#'   the CFA page passes the modeled items of its specification. It is
#'   evaluated each time the status is read, so a page may count the
#'   included items as the ledger stands. Without it the status says
#'   nothing about the scope. The status carries the
#'   answer as `fitted$items`, `fitted$respondents` and `fitted$included`,
#'   and `trigger` says what started the kept run: `"request"`,
#'   `"automatic"` (a first result), `"inclusion"` (a refit after an item
#'   was excluded or included) or `"update"`.
#' @return A list with `status` (a reactive, see above), `result` (a
#'   reactive returning the held value, also when it is stale, or `NULL`),
#'   `button(label)`, `line()` (the status line with the button where one
#'   belongs; it takes no timer, so a page redraws it only when the status
#'   changes), `run()` (what the button does), `cancel()` (stops a
#'   computation running in the background), `job` (the one running, a
#'   reactive value) and `store` (the kept entries, a reactive value).
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny reactive reactiveVal observeEvent withProgress actionButton
#' @importFrom shiny div strong tagList
sieve_demand_wire <- function(input, session, state, id, what, key, plan, compute,
                              kind = NULL, noun = "The model", verb = "fit",
                              action = "Fit the model", refit = "Refit the model",
                              stale_lead = "The data or the item pool changed since this model was fit: refit it. ",
                              capability = "independent", peek = NULL, background = NULL,
                              fitted = NULL) {
  original_key <- key
  key <- reactive({
    k <- original_key()
    roles <- sieve_roles_table(state$raw, state$items, state$roles)
    aux <- roles$variable[roles$role %in% "auxiliary"]
    if (length(aux)) k$auxiliary <- list(names = aux,
      values = sieve_data_key(state$raw[, intersect(aux, names(state$raw)), drop = FALSE]))
    state$sampling_design
    design <- sieve_sampling_spec(state)
    if (!identical(design$unit, "independent")) {
      raw <- state$raw
      column <- design$cluster
      design$values_key <- if (length(column) == 1L && column %in% names(raw)) sieve_hash(raw[[column]]) else NULL
      k$choices$.sampling_design <- design
    }
    k
  })
  store <- reactiveVal(list())
  # The computation running outside this process (`background`): its
  # handle with the key, plan and inputs it started with; at most one.
  job <- reactiveVal(NULL)
  cancel_id <- paste0(id, "_cancel")
  # The time so far of the computation running in the background, the one
  # part of its line redrawn each second (see `progress_text()`).
  progress_id <- paste0(id, "_progress")
  # A computation the researcher canceled, kept until the next one starts:
  # the line then says Stopped, with what was canceled and when, rather
  # than the wording of a result that merely waits.
  stopped <- reactiveVal(NULL)
  # What a kept result was fitted to, as the module reads it from the
  # value (`fitted`): its items, and the respondents it used when the
  # module says so. A wire without `fitted` says nothing, since a model
  # need not read every included item.
  fitted_of <- function(held) {
    if (is.null(held) || !is.function(fitted)) {
      return(NULL)
    }
    read <- tryCatch(fitted(held$value), error = function(e) NULL)
    if (is.null(read)) {
      return(NULL)
    }
    if (!is.list(read)) read <- list(items = read)
    items <- as.character(read$items %||% character(0))
    if (!length(items)) {
      return(NULL)
    }
    list(items = items, respondents = read$respondents, included = read$included)
  }
  # The datasets a result of this wire ran for (automatic or on request),
  # so that "first" is decided per dataset: data loaded after another
  # dataset get a first result of their own.
  datasets <- new.env(parent = emptyenv())
  datasets$seen <- character(0)
  datasets$inclusion <- NULL
  dataset_name_of <- function(k) k$dataset %||% "(unnamed data)"
  cap <- function(x) paste0(toupper(substring(x, 1L, 1L)), substring(x, 2L))
  held_for <- function(k) store()[[sieve_hash(k$choices)]]
  # The plan, or NULL when it is not ready (its req() stopped it), or a list
  # holding only `error` when it stopped with an error: the line shows that
  # error rather than hiding it behind "not fit yet" with no estimate.
  safe_plan <- function() {
    read <- sieve_outcome(plan())
    if (!read$ready) {
      return(NULL)
    }
    if (!read$ok) {
      return(list(error = read$error))
    }
    p <- read$value
    problem <- sieve_sampling_problem(state, capability = capability)
    if (!is.null(problem)) p$refused <- problem
    # An MLR (or ML) fit whose memory need exceeds the machine is refused
    # before it starts, for every tab that fits on request through this
    # wire (sieve_mlr_guard(); p is one count per separately fit model).
    if (is.null(p$refused) && length(p$p) && length(p$estimator) == 1L) {
      p$refused <- sieve_mlr_guard(p$p, p$estimator,
        n = if (is.null(state$items)) NA_real_ else nrow(state$items), npar = p$npar,
        missing = p$missing
      )
    }
    p
  }

  status_of <- function(k) {
    held <- held_for(k)
    if (is.null(held)) {
      return(list(
        state = "not_run", value = NULL, error = NULL, seconds = NA_real_,
        when = NULL, sentence = NULL, key = k, progress = NULL
      ))
    }
    # A result of other data does not stand for these data: it is
    # withheld from the page (status not run, no value), and the line says where
    # it is kept. The automatic observer then runs a first result for these data
    # under the gate.
    if (!identical(held$key$dataset, k$dataset)) {
      return(list(
        state = "not_run", value = NULL, error = NULL, seconds = NA_real_,
        when = NULL, key = k, progress = NULL, withheld = TRUE,
        sentence = sprintf(
          paste0(
            "The result of the data loaded before (%s: %s at %s) is kept in the analysis ",
            "registry and the saved-results report, not shown for these data."
          ),
          held$dataset_name %||% dataset_name_of(held$key), held$progress %||% what, held$when
        )
      ))
    }
    current <- identical(held$key, k)
    sentence <- NULL
    if (!current) {
      # A change of the respondents in the analysis (removed on the
      # Screening tab, or restored) is named as such, with who and the n
      # then and now, rather than as "the item values changed".
      rows_changed <- !identical(held$key$rows, k$rows) && identical(held$key$dataset, k$dataset)
      respondents <- rows_changed && !identical(held$key$data, k$data)
      sentence <- sieve_stale_sentence(
        held$ledger, sieve_ledger_analytic(state$meta),
        dataset_then = held$key$dataset, dataset_now = k$dataset,
        data_then = if (respondents) NULL else held$key$data,
        data_now = if (respondents) NULL else k$data
      )
      if (respondents) {
        sentence <- paste(c(
          sentence,
          sieve_rows_sentence(held$case_rows, sieve_case_index(state))
        ), collapse = "; ")
      }
      sentence <- sentence %||% if (rows_changed) "the original respondent row identities changed" else "the inputs changed"
    }
    # `progress` names the computation the held result came from (its
    # estimator and item count at the time), so a line about a result from
    # before describes that result and not the plan the inputs make now.
    list(
      state = if (!current) "stale" else if (!is.null(held$error)) "failed" else "current",
      value = held$value, error = held$error, seconds = held$seconds, when = held$when,
      sentence = sentence, key = k, refused = held$refused, progress = held$progress,
      trigger = held$trigger, fitted = fitted_of(held)
    )
  }
  status <- reactive({
    k <- key()
    out <- status_of(k)
    # A computation running outside this process for these inputs: the
    # status is running, with what it was before; a result from before
    # stays (marked as such), and nothing else is shown meanwhile.
    j <- if (is.null(background)) NULL else job()
    running <- if (!is.null(j)) {
      list(
        started = j$started_at, when = j$when, progress = j$plan$progress %||% what,
        label = j$plan$label, seconds = j$plan$seconds
      )
    }
    if (!is.null(j) && identical(j$key, k) && !identical(out$state, "current")) {
      out$before <- out$state
      out$state <- "running"
      if (!identical(out$before, "stale")) out$value <- NULL
      out$job <- running
    } else if (!is.null(j)) {
      # Still running for the inputs before a change: said on the line, with
      # Cancel, until it ends and is kept as from before.
      out$elsewhere <- running
    }
    # A cancel belongs to the inputs it was made for: other data, or other
    # choices, never read it.
    halted <- stopped()
    out$stopped <- if (identical(out$state, "running") || is.null(halted) ||
      !identical(halted$key$dataset, k$dataset) || !identical(halted$key$choices, k$choices)) NULL else halted
    out
  })

  # Publish cache metadata only: this observer must never evaluate the plan
  # or start a model while its page is hidden.
  observe({
    read <- sieve_outcome(status())
    if (!read$ready) {
      held <- store()
      if (!length(held)) return()
      previous <- tail(held, 1L)[[1L]]
      shown <- list(state = "blocked", key = previous$key, when = previous$when,
        sentence = "Confirm the study and complete the required inputs before using this result.")
    } else if (!read$ok) {
      shown <- list(state = "failed", sentence = read$error)
    } else shown <- read$value
    sieve_page_publish(state, sub("-.*$", "", session$ns("")), session$ns(id), what,
      shown, action = if (shown$state %in% c("stale", "failed")) refit else action, own = TRUE)
  })

  result <- reactive(status()$value)

  # progress = FALSE when a page's Update runs several results under its
  # own progress message, so one bar is shown, not one per result.
  # Only the visible primary CFA and Reliability results may run by
  # themselves; every other result is computed on request whatever its
  # estimate.
  auto_wire <- (identical(kind, "cfa") && identical(session$ns(id), "cfa-fit_go")) ||
    (identical(kind, "reliability") && identical(session$ns(id), "reliab-run_go"))
  confirmation <- reactiveVal(NULL)
  run <- function(automatic = FALSE, progress = TRUE, approved = FALSE, trigger = NULL) {
    # A result is bound to the dataset it was computed from, so data that
    # arrived without an identity (a state assembled by hand) are given one
    # first, as sieve_bind_result() does; the key is read after that.
    sieve_ensure_dataset(state)
    k <- shiny::isolate(key())
    st <- shiny::isolate(status())
    p <- shiny::isolate(safe_plan())
    if (identical(st$state, "current")) {
      sieve_notify(sprintf(
        "Nothing analytic has changed since %s ran at %s (%s), so the result shown is that run.",
        st$progress %||% p$progress %||% what, st$when, sieve_seconds_text(st$seconds)
      ))
      return(invisible(st))
    }
    if (!is.null(p$refused)) {
      sieve_notify(p$refused, type = "error", duration = NULL)
      sieve_log(
        state,
        sprintf("%s was not fit: %s", what, p$refused),
        c(sprintf("# %s", strwrap(p$refused, width = 76)))
      )
      return(invisible(st))
    }
    if (is.null(p) || !is.null(p$error)) {
      sieve_notify(p$error %||% "The analysis inputs are not ready. Complete the required settings before running.",
        type = "error", duration = NULL)
      return(invisible(st))
    }
    if (isTRUE(p$confirm_long) && !isTRUE(approved)) {
      confirmation(k)
      shiny::showModal(shiny::modalDialog(
        title = "Confirm a Long Search",
        shiny::p(sprintf("This search is estimated to take %s. Choose a smaller construct or facet pool to shorten it.", sieve_duration_label(p$seconds))),
        easyClose = TRUE,
        footer = shiny::tagList(shiny::modalButton("Cancel"),
          shiny::actionButton(session$ns(paste0(id, "_confirm")), "Start This Search"))
      ))
      return(invisible(st))
    }
    trigger <- trigger %||% if (automatic) "automatic" else "request"
    stopped(NULL)
    ledger <- sieve_ledger_analytic(shiny::isolate(state$meta))
    case_rows <- shiny::isolate(sieve_case_index(state))
    dataset_name <- shiny::isolate(state$dataset_name)
    label <- if (is.null(p) || is.null(p$label)) "" else sprintf(" (%s)", p$label)
    in_progress <- function(expr) {
      if (!isTRUE(progress)) {
        return(expr)
      }
      withProgress(
        message = sprintf("%s%s", p$progress %||% what, label),
        detail = p$detail %||% "",
        value = 1,
        expr
      )
    }
    started <- proc.time()[["elapsed"]]
    if (!is.null(background)) {
      j <- shiny::isolate(job())
      if (!is.null(j)) {
        if (identical(j$key, k)) {
          sieve_notify(sprintf(
            "%s is already running (started at %s); its line on the page has Cancel.",
            j$plan$progress %||% what, j$when
          ))
          return(invisible(st))
        }
        # One computation per result: a request with other inputs replaces
        # the one running.
        try(j$handle$kill(), silent = TRUE)
        job(NULL)
      }
      begin <- function() tryCatch(background$start(k, p), error = function(e) e)
      local_only <- is.function(background$in_process) && isTRUE(background$in_process())
      handle <- if (local_only) in_progress(begin()) else begin()
      if (inherits(handle, "error")) {
        keep(k, p, ledger, case_rows, list(value = NULL, error = conditionMessage(handle), warnings = character(0)),
          proc.time()[["elapsed"]] - started, automatic, dataset_name, trigger)
        return(invisible(shiny::isolate(status())))
      }
      now <- Sys.time()
      j <- list(
        handle = handle, key = k, plan = p, ledger = ledger, case_rows = case_rows,
        started = started, started_at = now, when = format(now, "%H:%M"),
        automatic = automatic, dataset_name = dataset_name, trigger = trigger
      )
      if (isTRUE(tryCatch(handle$is_alive(), error = function(e) FALSE))) job(j) else collect(j)
      return(invisible(shiny::isolate(status())))
    }
    outcome <- in_progress(sieve_capture(compute(k, p)))
    keep(k, p, ledger, case_rows, outcome, proc.time()[["elapsed"]] - started, automatic, dataset_name, trigger)
    invisible(shiny::isolate(status()))
  }

  # Keep a computation's outcome for its key, as the line and the page read
  # it, and say what happened.
  keep <- function(k, p, ledger, case_rows, outcome, seconds, automatic, dataset_name, trigger = "request") {
    value <- outcome$value
    engine_error <- if (is.list(value) && is.character(value[["error"]])) value[["error"]] else NULL
    if (is.null(engine_error) && is.list(value) && is.list(value$status) && length(value$status$code) == 1L &&
      !sieve_has_solution(value$status)) engine_error <- value$status$message %||% "The engine returned no fitted solution."
    entry <- list(
      key = k, ledger = ledger, case_rows = case_rows,
      value = value, error = outcome$error %||% engine_error, seconds = seconds,
      when = format(Sys.time(), "%H:%M"), warnings = outcome$warnings,
      progress = p$progress %||% what,
      dataset_name = dataset_name, trigger = trigger
    )
    held <- shiny::isolate(store())
    held[[sieve_hash(k$choices)]] <- entry
    store(held)
    datasets$seen <- unique(c(datasets$seen, dataset_name_of(k)))
    if (!is.null(outcome$error)) {
      sieve_notify(
        sprintf(
          "%s stopped with an error after %s: %s", p$progress %||% what,
          sieve_seconds_text(seconds), outcome$error
        ),
        type = "error", duration = NULL
      )
    } else if (!is.null(engine_error)) {
      sieve_notify(
        sprintf(
          "%s produced no solution after %s: %s", p$progress %||% what,
          sieve_seconds_text(seconds), engine_error
        ),
        type = "error", duration = NULL
      )
    } else {
      # A fit reused from the session's store (fct_fit_store.R) returns in
      # milliseconds and says nothing about this machine's rate, so it does
      # not enter the measured rate of its kind.
      if (!is.null(kind) && !is.null(p) && is.null(p$error) && is.finite(seconds) &&
        !(is.list(value) && isTRUE(value$reused))) {
        sieve_demand_note(kind, p$estimator, seconds, p$p, missing = p$missing)
      }
      if (!automatic) sieve_notify(sprintf("%s finished in %s.", p$progress %||% what, sieve_seconds_text(seconds)))
    }
    invisible(NULL)
  }

  # The computation that ran outside this process, once its process has
  # ended: its result is completed here (`background$finish`) and kept for
  # the key it started with, a result from inputs since changed as from
  # before.
  collect <- function(j) {
    job(NULL)
    seconds <- proc.time()[["elapsed"]] - j$started
    got <- tryCatch(list(value = j$handle$get_result(), error = NULL),
      error = function(e) list(value = NULL, error = sieve_efa_worker_cause(e))
    )
    outcome <- if (!is.null(got$error)) {
      list(value = NULL, error = got$error, warnings = character(0))
    } else {
      # Current: the data, the ledger and the rows are those it started
      # with (a setting changed meanwhile names another result).
      now <- tryCatch(shiny::isolate(key()), error = function(e) NULL)
      inputs_of <- function(x) if (is.list(x)) x[setdiff(names(x), "choices")] else x
      current <- !is.null(now) && identical(inputs_of(now), inputs_of(j$key))
      sieve_capture(background$finish(j$key, j$plan, got$value, j$handle, current = current))
    }
    keep(j$key, j$plan, j$ledger, j$case_rows, outcome, seconds, j$automatic, j$dataset_name, j$trigger %||% "request")
    invisible(NULL)
  }

  # Stop the computation running outside this process; nothing is kept.
  cancel <- function() {
    j <- shiny::isolate(job())
    if (is.null(j)) {
      return(invisible(FALSE))
    }
    try(j$handle$kill(), silent = TRUE)
    job(NULL)
    stopped(list(
      when = format(Sys.time(), "%H:%M"), progress = j$plan$progress %||% what,
      seconds = proc.time()[["elapsed"]] - j$started, trigger = j$trigger, key = j$key
    ))
    sieve_notify(sprintf(
      "%s was canceled after %s. Nothing was kept or recorded.",
      j$plan$progress %||% what, sieve_seconds_text(proc.time()[["elapsed"]] - j$started)
    ))
    invisible(TRUE)
  }

  # An inclusion or exclusion changes both the model and its complete-case
  # set. Refit a result already requested for this dataset; this does not
  # initiate an analysis on load, confirmation, or a threshold edit.
  observe({
    req(isTRUE(sieve_confirmation_status(state)$confirmed))
    k <- key()
    held <- held_for(k)
    if (is.null(held) || !identical(held$key$dataset, k$dataset)) return()
    if (identical(held$key, k)) {
      datasets$inclusion <- NULL
      return()
    }
    if (!is.null(job())) return()
    now <- sieve_ledger_analytic(state$meta)
    before <- held$ledger
    if (is.null(now) || is.null(before) || !identical(now$item, before$item)) return()
    changed <- !identical(now$status, before$status)
    if (!changed) return()
    token <- sieve_hash(list(dataset = k$dataset, items = now$item, status = now$status,
      choices = k$choices))
    if (identical(datasets$inclusion, token)) return()
    p <- safe_plan()
    if (is.null(p) || !is.null(p$refused) || !is.null(p$error)) return()
    datasets$inclusion <- token
    isolate(run(automatic = TRUE, trigger = "inclusion"))
  })

  observeEvent(input[[paste0(id, "_confirm")]], {
    pending <- confirmation()
    confirmation(NULL)
    shiny::removeModal()
    if (!is.null(pending) && identical(pending, key())) run(approved = TRUE)
  })
  observeEvent(input[[id]], run())

  if (!is.null(background)) {
    observeEvent(input[[cancel_id]], cancel())
    # The running computation is collected when its process ends: polled
    # twice a second while one runs, and nothing is polled otherwise.
    observe({
      j <- job()
      if (is.null(j)) {
        return()
      }
      if (isTRUE(tryCatch(j$handle$is_alive(), error = function(e) FALSE))) {
        shiny::invalidateLater(500, session)
        return()
      }
      shiny::isolate(collect(j))
    })
    session$onSessionEnded(function() {
      j <- shiny::isolate(job())
      if (!is.null(j)) try(j$handle$kill(), silent = TRUE)
    })
    # The time so far of the line of a running computation, redrawn each
    # second as text while one runs; the line itself, with its Cancel
    # button, is drawn once per run (line()). Nothing is redrawn otherwise
    # (sieve_progress_outputs(), fct_progress_line.R).
    sieve_progress_outputs(session$output, progress_id, function() progress_text(), session)
  }

  # Other workflows also fit a CFA and can use the same local button ID.
  # Automatic fitting belongs only to these two visible primary controls.
  if (auto_wire) {
    observe({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      # The page on screen, read through the workspaces (fct_workspaces.R):
      # the sub-tab shown in the workspace shown.
      active_tab <- sieve_active_page(session$rootScope()$input)
      expected_tab <- if (identical(kind, "cfa")) "cfa" else "reliab"
      req(identical(active_tab, expected_tab))
      st <- status()
      p <- safe_plan()
      # The run policy (sieve_runs_by_itself(), sieve_restored_project()):
      # a first result for these data, on the visible page, estimated at
      # two seconds or less, and not a reopened project.
      first <- identical(st$state, "not_run") && !dataset_name_of(st$key) %in% datasets$seen &&
        !sieve_restored_project(state)
      if (first && is.null(p$refused) && is.null(p$error) &&
        sieve_runs_by_itself(p$seconds, first = TRUE, visible = TRUE, standard = p$standard_seconds)) {
        isolate(run(automatic = TRUE))
      }
    })
  }

  # A result computed on request adopts, without a click, a fit the session
  # already holds under the identical identity (`peek`, which answers whether
  # the store has it; fct_fit_store.R): reading a stored result is immediate,
  # nothing is estimated, and the page's line says which page fit it and when.
  # Only on the shown page, and only while nothing was computed here yet.
  if (is.function(peek)) {
    page_id <- sub("-.*$", "", session$ns(""))
    observe({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      req(identical(sieve_active_page(session$rootScope()$input), page_id))
      st <- status()
      if (!identical(st$state, "not_run")) return()
      p <- safe_plan()
      if (is.null(p) || !is.null(p$refused) || !is.null(p$error)) return()
      k <- isolate(key())
      held <- tryCatch(isTRUE(peek(k, p)), error = function(e) FALSE)
      if (held) isolate(run(automatic = TRUE))
    })
  }

  # A second copy of the button placed elsewhere on the page takes its own
  # input id (`input_id`), which the page wires to run(): two buttons with
  # one id leave the second unbound in the browser (the Validity tab's
  # criterion note places a second Run button).
  button <- function(label = action, input_id = id) {
    p <- safe_plan()
    text <- if (is.null(p) || is.null(p$label)) label else sprintf("%s (%s)", label, p$label)
    actionButton(session$ns(input_id), text, class = "btn-primary")
  }

  # The sentence of a current result, for a tab that places it inside an
  # existing paragraph (the CFA tab's model note) rather than on a line of
# its own above the tabs, which would take height from every sub-tab.
  current_text <- function() {
    st <- status()
    if (!identical(st$state, "current")) {
      return(NULL)
    }
    # A fit reused from another page (fct_fit_store.R) names that page and
    # the time the identical model was fit there, not the moment of reuse.
    reuse <- if (is.list(st$value)) sieve_reuse_text(st$value) else NULL
    # What it was fitted to (fitted_of()), and whether it ran by itself
    # after an item was excluded or included (a run served from the fit
    # store reused the identical earlier fit and says so).
    fitted_text <- if (is.list(st$fitted) && length(st$fitted$items)) {
      paste0(" ", cap(sieve_fitted_to_text(st$fitted$items, st$fitted$respondents, verb = verb, included = st$fitted$included)), ".")
    } else {
      ""
    }
    after <- if (!identical(st$trigger, "inclusion")) {
      ""
    } else if (isTRUE(st$value$reused)) {
      " It was updated by itself after your item change, reusing the identical earlier fit."
    } else {
      sprintf(" It was %s by itself after your item change.", if (identical(verb, "fit")) "refit" else "recomputed")
    }
    if (!is.null(reuse)) {
      return(paste0(sprintf(
        "%s (the fit took %s then), on the current data and item pool: nothing analytic has changed since.",
        reuse, sieve_seconds_text(st$value$seconds)
      ), fitted_text, after))
    }
    paste0(sprintf(
      "%s at %s in %s, on the current data and item pool: nothing analytic has changed since.",
      st$progress %||% cap(what), st$when, sieve_seconds_text(st$seconds)
    ), fitted_text, after)
  }

  # The time so far of a computation running in the background, for the
  # wire's text output `<id>_progress`: redrawn each second while one runs
  # (for the inputs now, with a sentence once it has run past its
  # estimate, or for the inputs before a change), and NULL otherwise, when
  # the output is empty and nothing is polled.
  progress_text <- function() {
    st <- status()
    jb <- if (identical(st$state, "running")) st$job else st$elsewhere
    if (is.null(jb)) {
      return(NULL)
    }
    elapsed <- sieve_demand_elapsed(jb$started)
    over <- identical(st$state, "running") && is.numeric(jb$seconds) && length(jb$seconds) == 1L &&
      is.finite(jb$seconds) && elapsed > jb$seconds
    paste0(
      sprintf("%s so far.", sieve_seconds_text(elapsed)),
      if (over) " It has run past its estimate and is still running."
    )
  }
  # Where that text stands in the line: a span with role timer, whose live
  # region is off, so the line is not read again every second.
  ticking <- function() sieve_progress_text(session$ns(progress_id))

  line <- function() {
    st <- status()
    p <- safe_plan()
    badge <- function(s) sieve_lifecycle_badge(s, verb)
    # A refusal after a change leaves a result from before on the page: it
    # says Not refit, so the reader knows the tables are not the new pool's.
    refusal <- if (!is.null(p$refused)) {
      if (identical(st$state, "stale")) {
        div(class = "alert alert-danger small", strong("Not refit: "), sub("^Not fit: ", "", p$refused))
      } else {
        div(class = "alert alert-danger small", strong("Not fit. "), p$refused)
      }
    }
    # A computation canceled by the researcher, until the next one starts
    # (the badge before it says Stopped).
    stopped_text <- if (!is.null(st$stopped)) {
      strong(sprintf(
        "%s was canceled at %s after %s; nothing was kept. ",
        st$stopped$progress, st$stopped$when, sieve_seconds_text(st$stopped$seconds)
      ))
    }
    # The estimate could not be made: shown, not hidden behind the line.
    unplanned <- if (!is.null(p$error)) {
      div(
        class = "alert alert-danger small",
        strong("The estimated time could not be computed: "), p$error
      )
    }
    # On a reopened project the automatic results say in one sentence why they
    # wait; a result of other data says where it is kept.
    restored <- auto_wire && identical(st$state, "not_run") && sieve_restored_project(state)
    # A computation still running for the inputs before a change. The line
    # is drawn once; only its time so far is redrawn (progress_text()), so
    # Cancel stays the same element while it runs.
    elsewhere <- if (!is.null(st$elsewhere)) {
      ew <- st$elsewhere
      div(
        class = "alert alert-secondary small py-2", role = "status",
        sprintf(
          "%s for the inputs before the change is still running in a separate R process, started at %s; ",
          ew$progress, ew$when
        ),
        ticking(),
        " Its result will be kept as from before. ",
        actionButton(session$ns(cancel_id), "Cancel", class = "btn-outline-danger btn-sm ms-1")
      )
    }
    main <- switch(st$state,
      not_run = tagList(
        refusal,
        unplanned,
        div(
          class = "alert alert-info",
          badge(if (is.null(st$stopped)) "not_run" else "stopped"), stopped_text,
          strong(sprintf("%s is not %s yet. ", noun, verb)),
          if (isTRUE(st$withheld)) paste0(st$sentence, " "),
          if (restored) paste0(sieve_restored_sentence(action), " "),
          if (!is.null(p$sentence)) {
            paste0(p$sentence, if (auto_wire && !restored) ", so it waits for you. " else "; it is computed when you ask for it. ")
          },
          # When this machine's estimate is quick but the estimate at the
          # standard rates is not, say which one decided.
          if (is.numeric(p$standard_seconds) && is.finite(p$standard_seconds) &&
            p$standard_seconds > sieve_auto_seconds() && is.finite(p$seconds %||% NA_real_) &&
            p$seconds <= sieve_auto_seconds()) {
            sprintf(
              "(A first result runs by itself when its estimate at the standard rates, %s here, is two seconds or less.) ",
              sieve_duration_label(p$standard_seconds)
            )
          },
          sprintf("Nothing here is %s until you press the button. ", verb),
          div(class = "mt-2", button(action))
        )
      ),
      current = div(class = "small text-muted mb-2", badge("current"), current_text()),
      running = {
        # Drawn once per run, Cancel included; only the time so far is
        # redrawn each second (progress_text()).
        jb <- st$job
        div(
          class = "alert alert-info", role = "status",
          badge(if (identical(st$before, "stale")) "refitting" else "running"),
          strong(sprintf("%s is being %s in a separate R process. ", noun, verb)),
          sprintf(
            "%s, started at %s and estimated at %s; ", jb$progress, jb$when,
            jb$label %||% "an unknown time"
          ),
          ticking(),
          " This page and the others stay usable meanwhile, and the result appears here when it is done. ",
          if (identical(st$before, "stale")) {
            sprintf("The result below is from before (%s at %s). ", st$progress %||% what, st$when)
          },
          div(class = "mt-2", actionButton(session$ns(cancel_id), "Cancel", class = "btn-outline-danger btn-sm"))
        )
      },
      stale = tagList(
        refusal,
        unplanned,
        div(
          class = "alert alert-warning",
          badge(if (is.null(st$stopped)) "stale" else "stopped"), stopped_text,
          strong(stale_lead),
          sprintf(
            "What changed: %s. The result below is from before (%s at %s).",
            st$sentence, st$progress %||% what, st$when
          ),
          div(class = "mt-2", button(refit))
        )
      ),
      failed = tagList(
        refusal,
        unplanned,
        div(
          class = "alert alert-danger",
          badge("failed"),
          strong(sprintf("%s stopped with an error at %s: ", st$progress %||% cap(what), st$when)),
          st$error,
          sprintf(" Nothing is shown for it. Correct the cause, then press %s.", action),
          div(class = "mt-2", button(action))
        )
      )
    )
    if (is.null(elsewhere)) main else tagList(elsewhere, main)
  }

  list(
    status = status, result = result, run = run, button = button, line = line,
    current_text = current_text, store = store, cancel = cancel, job = job
  )
}

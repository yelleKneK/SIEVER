# Shared services.


#' The EFA tab's kept solution, shared with the Items tab
#'
#' A plain environment kept in `state$cache` (session only: `state$cache`
#' is never saved in a project, and `sieve_new_dataset()` empties it with
#' the data), written by the EFA tab when it keeps a solution
#' (`sieve_efa_store_keep()`) and read by the Items tab's diagnostics,
#' which reuse the solution and its seed instead of refitting when the
#' items, cases, correlation basis, number of factors, extraction and
#' rotation are identical (`sieve_efa_store_match()`). The environment
#' carries a reactive value `signal`, bumped at every write, so a display
#' that names the solution can follow it.
#'
#' @param state The shared state store.
#' @param create `FALSE` to read without creating the store.
#' @return The environment, or `NULL` when there is none and `create` is
#'   `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_efa_store <- function(state, create = TRUE) {
  cache <- shiny::isolate(state$cache) %||% list()
  store <- cache$efa
  if (is.environment(store) || !isTRUE(create)) {
    return(store)
  }
  store <- new.env(parent = emptyenv())
  # Bound to no session (as the state's own reactive values are when a
  # state is built outside one), so a state read by several sessions in
  # turn, as the tests do, can still read it.
  store$signal <- shiny::withReactiveDomain(NULL, shiny::reactiveVal(0L))
  cache$efa <- store
  state$cache <- cache
  store
}

#' Keep the EFA tab's solution in the shared store
#'
#' @param state The shared state store.
#' @param key The request the solution was fit for (`primary_key()`).
#' @param value The kept record (`fit`, `basis`, `fm`, `rotation`, `rng`,
#'   `warnings`, and for the EFA page's DMAR solution `engine = "DMAR"` and
#'   its `route`).
#' @return The store, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_efa_store_keep <- function(state, key, value) {
  store <- sieve_efa_store(state)
  store$key <- list(
    dataset_id = key$dataset_id, data = key$data, items = key$items, types = key$types,
    rows = key$rows, k = as.integer(key$k), fm = key$fm %||% value$fm, rotation = key$rotation %||% value$rotation,
    basis_used = value$basis$used, n = value$basis$n %||% value$n,
    engine = value$engine %||% "psych", route = value$route$label,
    form = value$route$form, estimator = value$route$estimator, missing = value$route$missing,
    n_used = value$n
  )
  store$fit <- value$fit
  store$status <- value$status
  store$call <- value$call
  store$rng <- value$rng
  store$warnings <- value$warnings %||% character(0)
  store$when <- format(Sys.time(), "%H:%M")
  store$signal(shiny::isolate(store$signal()) + 1L)
  invisible(store)
}

#' Is the EFA tab's kept solution the model the Items tab would fit?
#'
#' The Items tab's exploratory model is `DMAR::fa_exploratory()` with
#' maximum likelihood and direct oblimin rotation (no rotation at one
#' factor), k the number of constructs (or of the facets of the one
#' construct), on the shared correlation matrix of the included items and
#' their complete cases. The EFA tab's solution is `DMAR::fa_exploratory()`
#' too, so it is the same model, and is read in place of the Items tab's
#' own fit when it was fit for the same dataset, items, item values,
#' confirmed types and respondents, at the same k, with direct oblimin
#' (any rotation at one factor), by maximum likelihood (ML or MLR: the
#' same estimates, MLR adding robust standard errors) on the item
#' responses, every row it analyzed complete (on the
#' complete cases, or under FIML with no value missing, where FIML's
#' estimates are those of the complete cases), and the Items tab reads the
#' Pearson correlations of those same complete cases. Maximum likelihood is
#' invariant to the scale of the items, so the standardized solution of the
#' item responses is the solution of their correlation matrix. Every other
#' route is another model, and the difference is named: WLSMV fits the
#' polychoric correlations with diagonal weights, minimum residual is
#' unweighted least squares, and FIML with values missing (or the
#' saturated FIML matrix the Items tab then reads) estimates the
#' correlations otherwise.
#'
#' @param store The shared store (`sieve_efa_store()`), or `NULL`.
#' @param want The Items tab's identity: `dataset_id`, `data`, `items`,
#'   `types`, `rows`, `k`, `basis_used`, `basis_missing` (`"fiml"` for the
#'   saturated FIML matrix), `n` (its complete cases).
#' @return A list with `usable` (the solution is the same model),
#'   `same_pool` (the same dataset, items, values, types and respondents),
#'   `differences` (how the two models differ, for the Items tab's model
#'   line, when the pool is the same), `solution` (`fit`, `status`, `rng`,
#'   `warnings`, `k`, `call`, `route`, for `sieve_item_diagnostics()`) and
#'   `when`.
#' @author Ken Kelley
#' @noRd
sieve_efa_store_match <- function(store, want) {
  none <- list(usable = FALSE, same_pool = FALSE, differences = character(0), solution = NULL, when = NULL)
  if (is.null(store) || is.null(store$key) || is.null(store$fit)) {
    return(none)
  }
  key <- store$key
  same_pool <- identical(key$dataset_id, want$dataset_id) && identical(key$data, want$data) &&
    identical(as.character(key$items), as.character(want$items)) &&
    identical(unname(key$types), unname(want$types)) && identical(key$rows, want$rows)
  if (!same_pool) {
    return(none)
  }
  k_want <- as.integer(want$k)
  dmar <- identical(key$engine, "DMAR")
  likelihood <- dmar && identical(key$form, "data") && isTRUE(key$estimator %in% c("ML", "MLR"))
  route_words <- key$route %||% "on the item responses"
  differences <- c(
    if (!dmar) "a solution not fit by DMAR there, DMAR::fa_exploratory here",
    if (dmar && !likelihood) sprintf(
      "%s there, maximum likelihood on the %s correlations of the complete cases here", route_words,
      sieve_retention_basis_label(want$basis_used %||% "pearson")
    ),
    if (likelihood && !identical(want$basis_used, "pearson")) sprintf(
      "maximum likelihood on the item responses there, on the %s correlations here",
      sieve_retention_basis_label(want$basis_used)
    ),
    if (likelihood && identical(want$basis_used, "pearson") &&
      (identical(want$basis_missing, "fiml") || !identical(as.integer(key$n_used), as.integer(want$n)))) {
      sprintf(
        "%s on %s respondents there, the %s here",
        route_words, if (is.null(key$n_used)) "its" else format(as.integer(key$n_used)),
        if (identical(want$basis_missing, "fiml")) {
          "saturated FIML correlations (complete-case N a benchmark)"
        } else {
          sprintf("Pearson correlations of the %d complete cases", as.integer(want$n))
        }
      )
    },
    if (!identical(as.integer(key$k), k_want)) sprintf("k = %d there, %d here", key$k, k_want),
    if (k_want > 1L && !identical(key$rotation, "oblimin")) {
      sprintf("%s rotation there, direct oblimin here", sieve_efa_rotation_words(key$rotation %||% "another"))
    }
  )
  list(
    usable = !length(differences), same_pool = TRUE, differences = differences,
    solution = list(
      fit = store$fit, status = store$status, rng = store$rng, warnings = store$warnings, k = key$k,
      call = store$call, route = route_words
    ),
    when = store$when
  )
}

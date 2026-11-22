# Exact-identity reuse of fitted measurement models within a session.
# Several pages fit the same correlated-factors CFA to the same data: the
# CFA page, the Items page's overall model, the Validity page's measurement
# model, and the measurement model behind the Reliability page's factor
# scores. Fit separately, the same model would be estimated up to four
# times per session. The store below keeps each fitted model under the
# exact identity of what the engine was given (the dataset, the values of
# the modeled columns in model order, the respondent rows, the model syntax,
# the ordered indicators, the estimator, the missing data treatment, the
# identification, the sampling design, every other engine option, and the
# engine versions), and a second caller with the identical identity gets
# the identical fitted object back, marked as reused, with the page and the
# time of the fit it reuses. Nothing is equated by approximation: a
# different estimator, missing data rule, identification, syntax, row set
# or column value is a different identity and is fit again. The store lives
# in the session (state$fit_store), is emptied whenever data are loaded,
# and is never saved in a project: a reopened project waits on every page
# and refits only when asked.

#' A Session Store of Fitted Models, by Exact Identity
#'
#' Creates the store that `sieve_run_cfa()` (through [sieve_fit_cfa()])
#' reads and writes: a plain environment holding, for each distinct model
#' fit, the fitted result under the content identity of everything the
#' engine was given (see [sieve_fit_identity()]). A second request with the
#' identical identity receives the identical fitted object, so a session
#' that needs the same correlated-factors model on several pages
#' estimates it once. Two fits are never equated by approximation: any
#' difference in the data values of the modeled columns, the rows, the
#' syntax, the estimator, the missing data treatment, the identification,
#' the sampling design or the engine versions is a different identity.
#'
#' The store is a reference object: every holder of it sees the same
#' entries. SIEVER keeps one per session, empties it whenever data are
#' loaded (`sieve_new_dataset()`), and never writes it into a saved project.
#'
#' @param dataset An identifier of the data the store serves, kept as part
#'   of every identity (`NULL` for none).
#' @return An environment with `dataset`, `versions` (the engine versions
#'   at creation, see `sieve_engine_versions()`), and `entries` (a named
#'   list of fitted results, by identity).
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' store <- sieve_fit_store()
#' first <- sieve_fit_cfa(items, meta, store = store)
#' length(store$entries)
#' # The identical model on the identical data: the identical object back,
#' # without a second estimation.
#' again <- sieve_fit_cfa(items, meta, store = store)
#' identical(first, again)
#' # Another estimator is another identity.
#' other <- sieve_fit_cfa(items, meta, store = store, estimator = "ml")
#' length(store$entries)
#' @export
sieve_fit_store <- function(dataset = NULL) {
  store <- new.env(parent = emptyenv())
  sieve_fit_store_clear(store, dataset)
  store
}

#' Empty a fit store and bind it to a dataset
#'
#' @param store A store from [sieve_fit_store()].
#' @param dataset The identifier of the data the store now serves.
#' @return The store, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_fit_store_clear <- function(store, dataset = NULL) {
  store$dataset <- if (is.null(dataset) || !length(dataset)) NA_character_ else as.character(dataset)[1L]
  store$versions <- sieve_engine_versions()
  store$engine_identity <- sieve_engine_identity(c("lavaan", "DMAR"))
  store$entries <- list()
  invisible(store)
}

#' The versions of the engines a fit depends on
#'
#' Part of every fit identity: a fit made under one lavaan (or DMAR) is not
#' the fit the current version would make.
#'
#' @return A named character vector (`lavaan`, `DMAR`), `NA` for a package
#'   that is not installed.
#' @author Ken Kelley
#' @noRd
sieve_engine_versions <- function() {
  version_of <- function(pkg) {
    if (sieve_have(pkg)) as.character(utils::packageVersion(pkg)) else NA_character_
  }
  c(lavaan = version_of("lavaan"), DMAR = version_of("DMAR"))
}

#' The Exact Identity of a Model Fit
#'
#' A content hash of everything the engine is given for one fit: the
#' dataset identifier, the values of the modeled columns in model order
#' (and of the design columns, when the specification carries a cluster or
#' sampling weights), the identities of the respondent rows, the model
#' syntax, the ordered indicators, the estimator, the missing data
#' treatment, the identification (`std_lv`, `orthogonal`), every further
#' engine argument the specification passes, the sampling design, and the
#' engine versions. Two fits with the same identity are the same
#' estimation to the last bit, so one can stand for the other; any
#' difference in these inputs is a different fit, never equated.
#'
#' @param spec A specification from [sieve_cfa_spec()] (or any
#'   `sieve_spec`).
#' @param data The data frame the model is fit to.
#' @param rows The identities of the rows of `data` (the original row
#'   numbers of the data as loaded), or `NULL` when they are not tracked.
#' @param dataset An identifier of the dataset, or `NULL`.
#' @param versions The engine versions (see `sieve_engine_versions()`).
#' @param engine_identity Immutable identities of the executing engines.
#' @return A hash string.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' spec <- sieve_cfa_spec(meta, reserved = names(items))
#' id <- sieve_fit_identity(spec, items)
#' identical(id, sieve_fit_identity(spec, items))
#' # A column the model does not read leaves the identity unchanged; a
#' # value of a modeled column, another estimator, or another row set
#' # changes it.
#' with_extra <- cbind(items, age = lavaan::HolzingerSwineford1939$ageyr)
#' identical(id, sieve_fit_identity(spec, with_extra))
#' changed <- items
#' changed$x1[1] <- changed$x1[1] + 1
#' identical(id, sieve_fit_identity(spec, changed))
#' other <- sieve_cfa_spec(meta, reserved = names(items), estimator = "ml")
#' identical(id, sieve_fit_identity(other, items))
#' identical(id, sieve_fit_identity(spec, items[-1, ], rows = 2:301))
#' @export
sieve_fit_identity <- function(spec, data, rows = NULL, dataset = NULL,
                               versions = sieve_engine_versions(),
                               engine_identity = sieve_engine_identity(c("lavaan", "DMAR"))) {
  d <- as.data.frame(data, stringsAsFactors = FALSE)
  columns <- intersect(as.character(spec$observed %||% character(0)), names(d))
  design <- unique(as.character(c(spec$engine_args$cluster, spec$engine_args$sampling.weights, spec$engine_args$aux)))
  design <- intersect(design, names(d))
  sieve_hash(c(
    list(
      dataset = if (is.null(dataset) || !length(dataset)) NA_character_ else as.character(dataset)[1L],
      data = sieve_data_key(d[, columns, drop = FALSE]),
      design_data = if (length(design)) sieve_data_key(d[, design, drop = FALSE]) else NA_character_,
      rows = sieve_hash(rows)
    ),
    sieve_fit_spec_identity(spec),
    list(
      versions = versions,
      engine_identity = sieve_engine_keys(engine_identity)
    )
  ))
}

#' The part of a fit's identity its specification gives
#'
#' Everything [sieve_fit_identity()] takes from the specification: the
#' function, the model syntax, the ordered indicators, the estimator, the
#' missing data treatment, the identification, every further engine
#' argument and the sampling design. Two specifications with the same
#' fields, fit to the same data and rows under the same engines, are the
#' same estimation; the methods draft reads it to recognize a CFA record of
#' its own fit (`sieve_draft_usrmr_held()`).
#'
#' @param spec A `sieve_spec`.
#' @return A named list.
#' @author Ken Kelley
#' @noRd
sieve_fit_spec_identity <- function(spec) {
  sampling <- spec$sampling_design %||% list()
  list(
    fun = spec$fun %||% "cfa",
    model = spec$model,
    ordered = spec$ordered,
    estimator = spec$estimator,
    missing = spec$missing,
    std_lv = isTRUE(spec$std_lv),
    orthogonal = isTRUE(spec$orthogonal),
    engine_args = spec$engine_args %||% list(),
    sampling = list(
      unit = sampling$unit %||% "independent",
      cluster = sampling$cluster,
      values_key = sampling$values_key
    )
  )
}

#' Read a fitted result from the store by its identity
#'
#' @param store A store from [sieve_fit_store()], or `NULL`.
#' @param id An identity from [sieve_fit_identity()].
#' @return The stored result, or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_fit_store_get <- function(store, id) {
  if (is.null(store) || !is.environment(store) || is.null(id)) {
    return(NULL)
  }
  store$entries[[id]]
}

#' Does the store hold the identical fit a specification would make?
#'
#' The lookup `sieve_run_cfa()` makes before fitting, without fitting: the
#' identity of what the engine would be given (`sieve_fit_identity()`),
#' looked up in the store. A page that computes on request reads it to
#' adopt a fit another page already made, with the reuse sentence, instead
#' of waiting for a click that would return the same object: reading a
#' stored result is immediate.
#'
#' @param store A store from [sieve_fit_store()], or `NULL`.
#' @param spec A specification from [sieve_cfa_spec()].
#' @param data The data frame the fit would be given.
#' @param rows The row identities (see `sieve_run_cfa()`).
#' @return `TRUE` when a result with a fit is held under that identity.
#' @author Ken Kelley
#' @noRd
sieve_fit_store_has <- function(store, spec, data, rows = NULL) {
  if (is.null(store) || !is.environment(store) || is.null(spec) || length(spec$problems) ||
    !nzchar(spec$model %||% "")) {
    return(FALSE)
  }
  id <- tryCatch(
    sieve_fit_identity(spec, data,
      rows = rows, dataset = store$dataset,
      versions = store$versions %||% sieve_engine_versions()
    ),
    error = function(e) NULL
  )
  held <- sieve_fit_store_get(store, id)
  !is.null(held) && !is.null(held$fit)
}

#' Write a fitted result into the store under its identity
#'
#' @param store A store from [sieve_fit_store()].
#' @param id An identity from [sieve_fit_identity()].
#' @param run The result (a `sieve_result` carrying a fit).
#' @return The store, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_fit_store_put <- function(store, id, run) {
  entries <- store$entries
  entries[[id]] <- run
  store$entries <- entries
  invisible(store)
}

#' Keep with a stored fit what a page read from it
#'
#' The fit measures and the standardized solution are read once by the
#' page that fits a model (lavaan fits the baseline models for the
#' measures, which costs engine calls); a page that reuses the fit should
#' not pay them again. This records them with the stored entry when it does
#' not hold them yet, and records the names the page's script gave the fit
#' and the model, so a later page's script can refer to them instead of
#' repeating the call. The first names recorded stay: the script defines
#' the object once.
#'
#' @param store A store from [sieve_fit_store()], or `NULL`.
#' @param run A result returned by `sieve_run_cfa()` with the store, after
#'   the page added `measures` or `standardized`.
#' @param fit_name,model_name The object names the page's recorded script
#'   uses for the fit and the model syntax, or `NULL`.
#' @return The stored entry, invisibly, or `NULL` when nothing is stored
#'   under the result's identity.
#' @author Ken Kelley
#' @noRd
sieve_fit_store_keep <- function(store, run, fit_name = NULL, model_name = NULL) {
  held <- sieve_fit_store_get(store, run$fit_id)
  if (is.null(held)) {
    return(invisible(NULL))
  }
  for (field in c("measures", "standardized")) {
    if (is.null(held[[field]]) && !is.null(run[[field]])) held[[field]] <- run[[field]]
  }
  if (is.null(held$fit_name) && !is.null(fit_name)) {
    held$fit_name <- fit_name
    held$model_name <- model_name
  }
  sieve_fit_store_put(store, run$fit_id, held)
  invisible(held)
}

#' The sentence that says a fit was reused
#'
#' @param run A result returned by `sieve_run_cfa()` with a store.
#' @return `"Reused the fit from <page> at HH:MM"`, or `NULL` for a result
#'   that was fit.
#' @author Ken Kelley
#' @noRd
sieve_reuse_text <- function(run) {
  if (!isTRUE(run$reused)) {
    return(NULL)
  }
  from <- run$reused_from
  from <- if (is.null(from) || length(from) != 1L || is.na(from) || !nzchar(from)) "another page" else from
  sprintf("Reused the fit from %s at %s", from, run$reused_when %||% "an earlier time")
}

#' Why a reused fit was not fit again, under the status line
#'
#' The status line already says that the fit was reused, from which page and
#' when (`sieve_demand_wire()`'s `line()`, through `sieve_reuse_text()`).
#' This one does not repeat that; it is a labeled disclosure, closed, that
#' gives the reason the earlier fit applies: the identical lavaan model on
#' the identical data.
#'
#' @param run A result returned by `sieve_run_cfa()` with a store, or
#'   `NULL`.
#' @param status The result's demand status (see `sieve_demand_wire()`), or
#'   `NULL`; the disclosure is shown for a current result only.
#' @return A `<details>` tag, or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_reuse_line <- function(run, status = NULL) {
  text <- if (is.null(run)) NULL else sieve_reuse_text(run)
  if (is.null(text) || (!is.null(status) && !identical(status$state, "current"))) {
    return(NULL)
  }
  shiny::tags$details(
    class = "sieve-provenance small text-muted mb-2", `data-sieve-remember` = "reuse",
    shiny::tags$summary("Why nothing was fit again"),
    sprintf(
      paste0(
        "The fit is that of %s: the identical lavaan model on the identical data (the same syntax, ",
        "estimator, missing data treatment, identification and respondent rows), so nothing was fit ",
        "again; the fit took %s then."
      ),
      sub("^Reused the fit from ", "", text), sieve_seconds_text(run$seconds)
    )
  )
}

#' The recorded script of a reused fit
#'
#' One recorded step: a comment naming the page and the time of the fit
#' reused and, when that page's script named the fit, the assignments that
#' give this page's names to the same objects; otherwise the call itself,
#' repeated so the script stands alone.
#'
#' @param run A result returned by `sieve_run_cfa()` with a store.
#' @param fit_name,model_name,data_name The object names this page's script
#'   uses (see `sieve_cfa_code()`).
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_reuse_code <- function(run, fit_name = "cfa_fit", model_name = "cfa_model", data_name = "items") {
  note <- sprintf(
    "# %s: the identical lavaan model on the identical data; nothing was fit again.",
    sieve_reuse_text(run) %||% "Reused a fit"
  )
  from_fit <- run$fit_name
  if (is.null(from_fit) || !nzchar(from_fit)) {
    return(c(
      note, "# The call is repeated here so that the script stands alone.",
      sieve_cfa_code(run$spec, fit_name = fit_name, model_name = model_name, data_name = data_name)
    ))
  }
  if (identical(from_fit, fit_name)) {
    return(note)
  }
  from_model <- run$model_name %||% sub("_fit$", "_model", from_fit)
  c(
    note,
    sprintf("%s <- %s", model_name, from_model),
    sprintf("%s <- %s", fit_name, from_fit)
  )
}

#' Resolve the workflow allowed by the construct assignment basis
#'
#' The selector records how assignments were established. It does not certify
#' that a theory was specified before exposure to data. Screening and an
#' explicitly exploratory EFA remain available under either basis.
#' @param context A shared state or saved project, or a basis string.
#' @return A list giving the basis and permitted workflow actions.
#' @author Ken Kelley
#' @noRd
sieve_workflow_policy <- function(context = "theoretical") {
  basis <- if (is.character(context)) context else context$construct_basis
  basis <- basis %||% "theoretical"
  if (length(basis) != 1L || is.na(basis) || !basis %in% c("theoretical", "empirical")) {
    stop("Construct basis must be theoretical or empirical.", call. = FALSE)
  }
  list(
    basis = basis, optimization = identical(basis, "empirical"),
    screening = TRUE, exploratory_efa = TRUE,
    message = if (identical(basis, "theoretical")) {
      paste(
        "Theoretical assignments are retained. Use content evidence, CFA, reliability,",
        "and validity predictions; screening and exploratory EFA may challenge the model.",
        "Optimization is unavailable. This basis alone does not establish prespecification."
      )
    } else {
      paste(
        "Empirical assignments and searched item sets are provisional.",
        "Dimensionality, exploratory EFA, diagnostics, and optional optimization inform",
        "researcher judgment; an optimizer proposes candidates and establishes no substantive truth."
      )
    }
  )
}

#' Enforce the project workflow before searching or applying optimization
#' @param context Shared state, saved project, or basis string.
#' @return Invisibly the resolved policy; errors when optimization is forbidden.
#' @author Ken Kelley
#' @noRd
sieve_require_optimization <- function(context) {
  policy <- shiny::isolate(sieve_workflow_policy(context))
  if (!isTRUE(policy$optimization)) {
    stop("Optimization is unavailable under the theoretical construct basis. Deliberately change the construct basis on Ingest before searching or applying candidates.",
      call. = FALSE
    )
  }
  invisible(policy)
}

#' Identify the current workflow epoch
#' @param state The shared state or saved project.
#' @return Basis and full transition history, for invalidating earlier candidates.
#' @author Ken Kelley
#' @noRd
sieve_workflow_token <- function(state) {
  list(
    basis = sieve_workflow_policy(state)$basis,
    history = state$workflow_history %||% state$other$workflow_history %||% list()
  )
}

#' Deliberately change how construct assignments were established
#'
#' No assignment, response, fit, or decision is changed. History is append-only.
#' Returning to empirical does not revive candidates from an earlier workflow.
#' @param state Shared application state.
#' @param basis Theoretical or empirical.
#' @return Invisibly whether the basis changed.
#' @author Ken Kelley
#' @noRd
sieve_set_construct_basis <- function(state, basis) {
  next_policy <- sieve_workflow_policy(basis)
  old <- shiny::isolate(sieve_workflow_policy(state)$basis)
  if (identical(old, next_policy$basis)) return(invisible(FALSE))
  history <- shiny::isolate(state$workflow_history) %||% list()
  history[[length(history) + 1L]] <- list(
    from = old, to = basis, time = format(Sys.time(), tz = "UTC", usetz = TRUE),
    note = "Researcher changed the assignment basis; prior specifications and exposure remain recorded."
  )
  state$construct_basis <- basis
  state$workflow_history <- history
  sieve_log(
    state, sprintf("Change construct basis from %s to %s", old, basis),
    c(
      sprintf("construct_basis <- %s", deparse(basis)),
      "# Assignment basis does not establish prespecification or independent validation.",
      "# Prior assignments, analyses, decisions, and exposure history are retained."
    )
  )
  invisible(TRUE)
}

#' Keep navigation consistent with the shared workflow policy
#' @param state Shared application state.
#' @param session The top-level Shiny session.
#' @return A Shiny observer.
#' @author Ken Kelley
#' @noRd
sieve_workflow_wire <- function(state, session) {
  shiny::observe({
    policy <- sieve_workflow_policy(state)
    # The Optimizer's link is a sub-tab of the Structure workspace
    # (sieve_page_navset(), fct_workspaces.R).
    navset <- sieve_page_navset("optimize")
    if (policy$optimization) bslib::nav_show(navset, target = "optimize", session = session)
    else bslib::nav_hide(navset, target = "optimize", session = session)
    session$sendCustomMessage("sieve-workflow-policy", list(optimization = policy$optimization))
  })
}

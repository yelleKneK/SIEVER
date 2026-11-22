# fct_item_decision.R
# Excluding or including items from the pages that show item results, and
# one vocabulary for where a result stands after such a change.
#
# The dialog that excludes and includes items (mod_drop.R) is reached from the
# navigation bar and from the pages that show item results, so an item can be
# removed and the results updated where they are read: the CFA Estimates table
# and the EFA Pattern and Structure tables select rows, and one button beside
# each opens that same dialog with the selected items filled in. The dialog's
# own Exclude and Include actions remain the only writers of the item status
# (sieve_set_item_status()), so the reason, the decision history, the coverage
# advisory, the preview and the reproducible code are the same whichever page
# the decision is made from.
#
# After the decision every page says in the same words where its result
# stands (sieve_lifecycle_badge()): Current, Refitting, Running, Update
# needed, Stopped, Failed, Not fit yet (Not computed yet for a result that
# is computed rather than fit) or Waiting on inputs, beside the sentence
# that says what the result was fitted to.

# A counter for the request nonce, so two requests for the same items in
# one session are both heard (reactiveValues ignores an identical value).
.sieve_item_decision <- new.env(parent = emptyenv())
.sieve_item_decision$n <- 0L

#' A Value That Differs From Every Earlier One in This Session
#' @return A string.
#' @author Ken Kelley
#' @noRd
sieve_item_decision_nonce <- function() {
  .sieve_item_decision$n <- .sieve_item_decision$n + 1L
  paste(format(Sys.time(), "%Y%m%d%H%M%OS3"), .sieve_item_decision$n, sep = "-")
}

#' Ask the Exclude / Include Dialog to Open With Items Filled In
#'
#' Writes the request to the session-only state field
#' `item_decision_request`, which the dialog's module observes
#' (mod_drop.R). Nothing in the ledger changes until the researcher presses
#' Exclude or Include there. A request with no items opens the dialog
#' unfilled.
#'
#' @param state The shared state store.
#' @param items Variable names of the items to fill in (may be empty).
#'   Items not in the ledger are dropped.
#' @param status `"parked"` to fill the exclusion picker, `"active"` the
#'   inclusion picker.
#' @param origin Where the request came from, as the dialog names it
#'   ("the CFA Estimates table").
#' @return The request, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_item_decision_request <- function(state, items, status = c("parked", "active"),
                                        origin = "a results table") {
  status <- match.arg(status)
  meta <- shiny::isolate(state$meta)
  items <- unique(as.character(items))
  items <- items[!is.na(items) & nzchar(items)]
  if (!is.null(meta)) items <- intersect(items, meta$item)
  request <- list(items = items, status = status, origin = origin, nonce = sieve_item_decision_nonce())
  state$item_decision_request <- request
  invisible(request)
}

#' The Sentence Beside an Item Table's Exclude or Include Button
#' @return One string.
#' @author Ken Kelley
#' @noRd
sieve_item_decision_sentence <- function() {
  paste(
    "Select rows, then press the button to exclude those items (the same dialog includes excluded items again).",
    "An exclusion applies to this whole study, on every page, and deletes nothing: the item can be included",
    "again in that dialog, on the Items page, or with Exclude / Include items in the navigation bar."
  )
}

#' The Button and the Sentence Beside an Item Table
#' @param ns The module namespace function.
#' @param id The button's input id within the namespace.
#' @return A tag.
#' @author Ken Kelley
#' @noRd
sieve_item_decision_controls <- function(ns, id) {
  shiny::div(
    class = "d-flex flex-wrap align-items-center gap-2 mb-2 small sieve-item-decision",
    shiny::actionButton(ns(id), "Exclude or include items", class = "btn-outline-danger btn-sm",
      title = "Open the Exclude / Include dialog with the selected rows filled in"),
    shiny::span(class = "text-muted", sieve_item_decision_sentence())
  )
}

#' Wire an Item Table's Button to the Shared Dialog
#'
#' @param input The module's input.
#' @param state The shared state store.
#' @param id The button's input id within the namespace.
#' @param items A function returning the variable names of the selected
#'   rows (see `sieve_selected_items()`).
#' @param origin Where the request comes from, as the dialog names it.
#' @return `NULL`, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_item_decision_wire <- function(input, state, id, items, origin) {
  shiny::observeEvent(input[[id]], {
    picked <- tryCatch(items(), error = function(e) character(0))
    sieve_item_decision_request(state, picked, "parked", origin)
  }, ignoreInit = TRUE)
  invisible(NULL)
}

#' The Words for Where a Result Stands
#'
#' @param verb `"fit"` for a model, `"computed"` for another result.
#' @return A named character vector, by state.
#' @author Ken Kelley
#' @noRd
sieve_lifecycle_states <- function(verb = "fit") {
  c(
    not_run = sprintf("Not %s yet", verb), current = "Current", running = "Running",
    refitting = "Refitting", stale = "Update needed", settings = "Update needed",
    stopped = "Stopped", failed = "Failed", blocked = "Waiting on inputs"
  )
}

#' A Result's State at the Start of Its Status Line
#'
#' The same words on every page: Current, Refitting, Running, Update
#' needed, Stopped, Failed, Not fit yet (or Not computed yet), Waiting on
#' inputs. A refit refused after an item change keeps Update needed, and
#' the line beside it says Not refit with the reason.
#'
#' @param state One of the names of `sieve_lifecycle_states()`.
#' @param verb `"fit"` or `"computed"`.
#' @return A span.
#' @author Ken Kelley
#' @noRd
sieve_lifecycle_badge <- function(state, verb = "fit") {
  states <- sieve_lifecycle_states(verb)
  if (length(state) != 1L || is.na(state) || !state %in% names(states)) state <- "blocked"
  shiny::span(class = paste("sieve-lifecycle", paste0("sieve-lifecycle-", state)), states[[state]])
}

#' The Lifecycle State of an On-Demand Result, From Its Status
#'
#' Reads the status `sieve_demand_wire()` returns: a running computation
#' that replaces a result from before is Refitting, a canceled one is
#' Stopped until the next run, and the other states are its own.
#'
#' @param st The status list.
#' @return A state name for `sieve_lifecycle_badge()`.
#' @author Ken Kelley
#' @noRd
sieve_demand_lifecycle <- function(st) {
  if (!is.list(st) || is.null(st$state)) return("blocked")
  if (identical(st$state, "running")) return(if (identical(st$before, "stale")) "refitting" else "running")
  if (!is.null(st$stopped) && st$state %in% c("stale", "not_run")) return("stopped")
  st$state
}

#' The Items and Respondents a Result Was Fitted To
#'
#' @param items The variable names of the items the result was fitted to.
#' @param respondents The number of respondents the result used (the rows
#'   the fit read, as the module reports them), or `NULL` to leave them
#'   unsaid.
#' @param most How many items to name before counting the rest.
#' @param included How many items the study includes, or `NULL`. When more
#'   are included than the result used (a CFA leaves out an included item
#'   with no construct, a construct left with one included item, or a facet
#'   too small or not selected), the phrase counts the used items against
#'   them: "5 of the 6 included items (...)".
#' @return One phrase without a final period, "the 9 included items (x1,
#'   x2, x3, x4, x5 and 4 more) and 301 respondents".
#' @author Ken Kelley
#' @noRd
sieve_fitted_scope_text <- function(items, respondents = NULL, most = 5L, included = NULL) {
  items <- as.character(items %||% character(0))
  n <- length(items)
  included <- suppressWarnings(as.integer(included))
  partial <- n > 0L && length(included) == 1L && !is.na(included) && included > n
  item_part <- if (!n) {
    "the included items"
  } else {
    shown <- paste(utils::head(items, most), collapse = ", ")
    if (n > most) shown <- sprintf("%s and %d more", shown, n - most)
    if (partial) {
      sprintf("%d of the %d included items (%s)", n, included, shown)
    } else {
      sprintf("the %d included item%s (%s)", n, if (n == 1L) "" else "s", shown)
    }
  }
  resp_part <- if (length(respondents) == 1L && is.finite(respondents) && respondents > 0) {
    sprintf(" and %d respondent%s", as.integer(respondents), if (respondents == 1L) "" else "s")
  } else ""
  paste0(item_part, resp_part)
}

#' The Same Scope, Led by the Verb of the Result
#'
#' @inheritParams sieve_fitted_scope_text
#' @param verb `"fit"` ("fitted to ...") or `"computed"` ("computed from ...").
#' @return One phrase without a final period.
#' @author Ken Kelley
#' @noRd
sieve_fitted_to_text <- function(items, respondents = NULL, most = 5L, verb = "fit", included = NULL) {
  paste0(if (identical(verb, "fit")) "fitted to " else "computed from ", sieve_fitted_scope_text(items, respondents, most, included))
}

#' A Clock Time, From a Recorded UTC Stamp
#' @param x A string of the form "YYYY-MM-DD HH:MM:SS UTC", or `NULL`.
#' @return "HH:MM" in the session's time zone, or `NULL` when it cannot be read.
#' @author Ken Kelley
#' @noRd
sieve_clock_text <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(NULL)
  at <- suppressWarnings(as.POSIXct(sub(" UTC$", "", x), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  if (is.na(at)) return(NULL)
  # The record is kept in UTC; the page states it as the session's clock reads.
  format(at, "%H:%M", tz = "")
}

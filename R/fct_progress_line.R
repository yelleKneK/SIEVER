# fct_progress_line.R
# The line of a computation running in a background R process: drawn once
# per run, with its Cancel button, while only its progress text is redrawn.
#
# Each page that runs a computation outside this process (the EFA page's
# selected EFA and factor count comparison, the Items page's exploratory
# factor analysis, the Dimensionality page's parallel analysis, VSS and
# model fit by k, and the on-demand wire of fct_ondemand.R) says, while it
# runs, what is running, the time so far, and a Cancel button. The line is
# drawn once per run by its page, with the Cancel button in it, and the
# part that changes is a text output of its own (sieve_progress_text() in
# the line, sieve_progress_outputs() on the server), redrawn each second
# while the computation runs. A line drawn by a renderUI() that redrew
# itself each second (invalidateLater()) or at each completed part of the
# run would replace the Cancel button at every redraw: keyboard focus on it
# would fall to the page body, and a press whose release came after a
# redraw would land on a button no longer in the page. The MI panels
# (sieve_mi_progress_ui()) and the on-demand status lines follow the same
# design.

#' The progress text of a running computation's line
#'
#' Placed in a line that its page draws once per run (with the Cancel
#' button), this is the one part of the line that is redrawn while the
#' computation runs (`sieve_progress_outputs()`). It is an inline text
#' output in a span with role `timer`, whose live region is off: the line
#' itself is usually a live region (role `status`), and without this a
#' screen reader would read the whole line again every second.
#'
#' @param id The text output's id, namespaced (`session$ns(...)`).
#' @return A tag.
#' @author Ken Kelley
#' @noRd
sieve_progress_text <- function(id) {
  shiny::tagAppendAttributes(
    shiny::textOutput(id, inline = TRUE),
    role = "timer", `aria-live` = "off"
  )
}

#' Register the progress text outputs of a running computation
#'
#' Each output gives the sentence `text()` returns, and is redrawn every
#' `interval` milliseconds while that sentence is not `NULL`; when it is
#' `NULL` (nothing runs) the output is empty and nothing is polled. `text`
#' is read reactively, so it should read what starts and ends the run (a
#' tick or a running flag), which wakes the output for the next run. A
#' computation shown in more than one line (a page's status line and its
#' panel) has one output per line, each with the same sentence, since an
#' output id is placed once in a page.
#'
#' @param output The module's output object.
#' @param ids The outputs' ids (not namespaced).
#' @param text A function of no arguments: the progress sentence while the
#'   computation runs, otherwise `NULL`.
#' @param session The Shiny session.
#' @param interval Milliseconds between redraws while it runs.
#' @return Invisible `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_progress_outputs <- function(output, ids, text, session = shiny::getDefaultReactiveDomain(),
                                   interval = 1000) {
  for (id in ids) {
    output[[id]] <- shiny::renderText({
      value <- text()
      if (is.null(value)) {
        return("")
      }
      shiny::invalidateLater(interval, session)
      value
    })
  }
  invisible(NULL)
}

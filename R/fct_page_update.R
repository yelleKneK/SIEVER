# fct_page_update.R
# One Update per page.
#
# A result that an edit made stale (an item excluded, included or reverse
# scored, a respondent removed or restored, a setting of the page changed)
# is kept on the page and marked as from before. Rather than a Refit or Run
# button for each such result, so that a page with three results would show
# three of them, each in its own panel, the page's status line says, in one
# sentence, what is from before and what changed, and carries one button,
# Update, labeled with the combined estimate. Update recomputes, one
# after another and under one progress message, every result of the page
# that was computed before and is now stale, exactly as its own button
# would, and announces when it is done. It never starts a result that was
# never requested, and it leaves to their own buttons the results whose
# computation is a resampling run (a bootstrap, a Monte Carlo interval) or
# that depend on a choice the researcher makes in a table the Update
# redraws; the line names those, with the button to press.
#
# sieve_update_wire() is the one implementation; a module gives it the list
# of its results (sieve_update_entry()), and places the line it returns in
# its status line.

#' Service the Session Between Batches of a Requested Computation
#' @param running Reactive function indicating whether work remains.
#' @param session Shiny session.
#' @param step Function performing one module-specific batch.
#' @param interval Milliseconds between batches.
#' @return A Shiny observer.
#' @author Ken Kelley
#' @noRd
sieve_chunked_wire <- function(running, session, step, interval = 20L) {
  shiny::observe({
    shiny::req(isTRUE(running()))
    shiny::invalidateLater(interval, session)
    shiny::isolate(step())
  })
}

#' Bind Page Controls to Saved Settings Without Overwriting an Open Project
#' @param state Shared study state.
#' @param field The state field storing the settings.
#' @param settings Function reading the current controls, or NULL until ready.
#' @param restore Function updating the page's controls from saved settings.
#' @param normalize Function resolving saved selections against available choices.
#' @param clear_empty Whether an empty saved field clears a pending restoration.
#' @param require_common Whether pending and current settings must share a field.
#' @param check_attributes Whether attribute differences prevent restoration.
#' @return The pending restoration reactive.
#' @author Ken Kelley
#' @noRd
sieve_bind_settings <- function(state, field, settings, restore, normalize = identity,
                                clear_empty = TRUE, require_common = FALSE,
                                check_attributes = FALSE) {
  pending <- shiny::reactiveVal(NULL)
  memory <- new.env(parent = emptyenv())
  memory$written <- NULL
  shiny::observeEvent(state[[field]], {
    saved <- state[[field]]
    if (!is.list(saved) || !length(saved)) {
      if (clear_empty) pending(NULL)
      return()
    }
    if (identical(saved, memory$written)) return()
    saved <- normalize(saved)
    pending(saved)
    restore(saved)
  }, ignoreNULL = TRUE, priority = 100)
  shiny::observe({
    saved <- settings()
    if (is.null(saved)) return()
    target <- pending()
    if (!is.null(target)) {
      fields <- intersect(names(target), names(saved))
      matched <- (!require_common || length(fields) > 0L) && all(vapply(fields,
        function(field) isTRUE(all.equal(saved[[field]], target[[field]],
          check.attributes = check_attributes)), logical(1)))
      if (!matched) return()
      pending(NULL)
    }
    memory$written <- saved
    if (!identical(shiny::isolate(state[[field]]), saved)) state[[field]] <- saved
  })
  list(pending = pending)
}

#' One result of a page, as the page's Update reads it
#'
#' @param title A short name of the result ("Correlated-factors model").
#' @param state `"not_run"`, `"current"`, `"stale"`, `"failed"`, or
#'   `"blocked"`.
#' @param sentence What changed since the result was computed (for a stale
#'   result), or why it failed.
#' @param when When the result was computed (`"14:02"`), or `NULL`.
#' @param seconds The estimated time of computing it again, or `NA`.
#' @param run A function of no arguments that computes the result again
#'   exactly as the result's own button does.
#' @param update `FALSE` for a result the page's Update leaves to its own
#'   button (a bootstrap, a Monte Carlo interval, a result that depends on a
#'   selection the Update redraws).
#' @param button The label of the result's own button, named in the line
#'   when the Update leaves it to that button.
#' @param why Why the Update leaves it to its own button.
#' @param shown `TRUE` when the page keeps showing the stale result (marked
#'   as from before); `FALSE` when it withholds it until it is computed
#'   again.
#' @param background Whether the run starts work completed by a later session pass.
#' @param retry Whether a failed requested result joins this Update.
#' @return A list.
#' @author Ken Kelley
#' @noRd
sieve_update_entry <- function(title, state, sentence = NULL, when = NULL, seconds = NA_real_,
                               run = NULL, update = TRUE, button = NULL, why = NULL, shown = TRUE,
                               background = FALSE, retry = FALSE) {
  list(
    title = title, state = state %||% "not_run", sentence = sentence, when = when,
    seconds = if (length(seconds) == 1L && is.numeric(seconds)) seconds else NA_real_,
    run = run, update = isTRUE(update) && is.function(run), button = button, why = why,
    shown = !isFALSE(shown), background = isTRUE(background), retry = isTRUE(retry)
  )
}

#' The entry of a result computed on request through sieve_demand_wire()
#'
#' @param demand The list `sieve_demand_wire()` returns.
#' @param title A short name of the result.
#' @param plan The reactive giving the result's plan (its `seconds` are the
#'   estimate of computing it again), or `NULL`.
#' @return A `sieve_update_entry()`, read reactively, or `NULL` while the
#'   result's inputs are not ready (its key stopped on a `req()`).
#' @author Ken Kelley
#' @noRd
sieve_update_demand_entry <- function(demand, title, plan = NULL) {
  read <- sieve_outcome(demand$status())
  if (!read$ready || !read$ok) {
    return(NULL)
  }
  st <- read$value
  if (identical(st$state, "not_run") && !isTRUE(st$withheld) && length(demand$store())) {
    st$state <- "settings"
    st$sentence <- "the page settings changed"
  }
  seconds <- NA_real_
  if (identical(st$state, "stale") && !is.null(plan)) {
    estimate <- sieve_outcome(plan())
    if (estimate$ready && estimate$ok && is.list(estimate$value)) {
      seconds <- estimate$value$seconds %||% NA_real_
    }
  }
  sieve_update_entry(
    title = title, state = st$state, sentence = st$sentence,
    when = if (!is.null(st$when)) sprintf("%s, at %s", st$progress %||% title, st$when),
    # Under the Update's own progress message (one bar per page).
    seconds = seconds, shown = !isTRUE(st$withheld),
    run = function() demand$run(automatic = TRUE, progress = FALSE, trigger = "update")
  )
}

#' A list of names as prose
#'
#' @param x A character vector.
#' @return "a", "a and b", or "a, b, and c".
#' @author Ken Kelley
#' @noRd
sieve_update_list_text <- function(x) {
  x <- unique(as.character(x))
  sieve_tab_list_text(x)
}

#' A result's name inside a sentence
#'
#' The titles start with a capital ("Correlated-factors model"); inside a
#' sentence the first letter is lowered, except for a name that starts
#' with an acronym (HTMT, CFA). With `first = TRUE` the first name of the
#' list keeps its capital, for a sentence that starts with it.
#'
#' @param x A character vector of titles.
#' @param first Keep the first title's capital.
#' @return A character vector.
#' @author Ken Kelley
#' @noRd
sieve_update_lower <- function(x, first = FALSE) {
  x <- sub("^the +", "", as.character(x), ignore.case = TRUE)
  low <- function(t) {
    if (grepl("^[A-Z][A-Z]", t)) t else paste0(tolower(substring(t, 1L, 1L)), substring(t, 2L))
  }
  out <- vapply(as.character(x), low, character(1), USE.NAMES = FALSE)
  if (isTRUE(first) && length(out)) out[1L] <- as.character(x)[1L]
  out
}

#' Wire one Update to the results of a page
#'
#' @param input,session The module's input and session.
#' @param entries A function, read reactively, returning a named list of
#'   `sieve_update_entry()` lists, one per result of the page, in the order
#'   the Update computes them (a result another one reads comes first).
#' @param id The input id of the Update button.
#' @param sequence Optional function preserving a page's ordered background job sequence.
#' @param completion Optional formatter of done, failed, started titles and elapsed
#'   seconds, returning arguments for `sieve_notify()`.
#' @return A list with `pending` (a reactive: the entries the Update
#'   computes), `held` (a reactive: the stale entries left to their own
#'   buttons), `seconds` (a reactive: the combined estimate), `label()`,
#'   `button()`, `line()` (the status line when a result the Update
#'   computes is from before, or `NULL`), `note()` (one small line naming
#'   the results from before that keep their own buttons, when the Update
#'   has nothing to compute), and `run()` (what the button does; it returns
#'   the titles it brought up to date).
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny reactive observeEvent actionButton div strong isolate withProgress incProgress
sieve_update_wire <- function(input, session, entries, id = "update_go", sequence = NULL, completion = NULL) {
  read <- function() {
    got <- sieve_outcome(entries())
    if (!got$ready || !got$ok || !is.list(got$value)) {
      return(list())
    }
    Filter(Negate(is.null), got$value)
  }
  stale_of <- function(all, update) {
    Filter(function(e) (e$state %in% c("stale", "settings") ||
      (identical(e$state, "failed") && isTRUE(e$retry))) &&
      identical(isTRUE(e$update), update), all)
  }
  pending <- reactive(stale_of(read(), TRUE))
  held <- reactive(stale_of(read(), FALSE))
  # The combined estimate: the sum of the component estimates that are
  # known. A component whose duration is unknown (no rate measured for it
  # yet, a plan that could not be made) is left out of the sum and named,
  # so the label never passes a partial sum off as the whole.
  seconds <- reactive({
    s <- vapply(pending(), function(e) as.numeric(e$seconds %||% NA_real_), numeric(1))
    if (!length(s) || !any(is.finite(s))) NA_real_ else sum(s[is.finite(s)])
  })
  unknown <- reactive({
    todo <- pending()
    s <- vapply(todo, function(e) as.numeric(e$seconds %||% NA_real_), numeric(1))
    vapply(todo[!is.finite(s)], function(e) e$title, character(1), USE.NAMES = FALSE)
  })
  duration <- function() sieve_update_duration_text(seconds(), length(unknown()), length(pending()))
  label <- function() sprintf("Update (%s)", duration())
  button <- function() {
    actionButton(session$ns(id), label(), class = "btn-primary btn-sm ms-1")
  }

  # The status line, when a result the Update computes is from before.
  line <- function() {
    todo <- pending()
    kept <- held()
    if (!length(todo)) {
      return(NULL)
    }
    everything <- c(todo, kept)
    sentences <- vapply(everything, function(e) {
      if (length(e$sentence) == 1L && !is.na(e$sentence) && nzchar(e$sentence)) e$sentence else ""
    }, character(1))
    said <- unique(sentences[nzchar(sentences)])
    changed <- if (!length(said)) {
      NULL
    } else if (length(said) == 1L) {
      sprintf("What changed: %s. ", said)
    } else {
      sprintf("What changed: %s. ", paste(sprintf(
        "%s (%s)", sentences[nzchar(sentences)],
        sieve_update_lower(vapply(everything, function(e) e$title, character(1))[nzchar(sentences)])
      ), collapse = "; "))
    }
    titles <- function(x) {
      sieve_update_list_text(sieve_update_lower(vapply(x, function(e) e$title, character(1))))
    }
    # "is" for one result named in the singular, "are" otherwise.
    many <- function(x) length(x) > 1L || grepl("[^siu]s$", x[[1L]]$title)
    subject <- paste0("The ", titles(everything))
    not_known <- unknown()
    # Its own class, sieve-update-line: sieve-status-line is the class of
    # sieve_status_line(), whose rule in custom.css would draw this warning
    # on white with a grey border.
    div(
      class = "alert alert-warning py-2 mb-2 sieve-update-line",
      role = "status",
      sieve_lifecycle_badge("stale"),
      strong(sprintf("%s %s from before. ", subject, if (many(everything)) "are" else "is")),
      changed,
      if (length(not_known)) {
        sprintf(
          "The duration of the %s is unknown and is not in the estimate. ",
          sieve_update_list_text(sieve_update_lower(not_known))
        )
      },
      if (length(on_page <- Filter(function(e) isTRUE(e$shown), todo))) {
        sprintf(
          "Shown below as computed (%s). ",
          paste(vapply(on_page, function(e) e$when %||% e$title, character(1)), collapse = "; ")
        )
      },
      if (length(withheld <- Filter(function(e) !isTRUE(e$shown), todo))) {
        sprintf("Not shown until updated: the %s. ", titles(withheld))
      },
      if (length(kept)) {
        sprintf(
          "The %s %s computed again with %s own button%s (%s), not by Update. ",
          titles(kept), if (many(kept)) "are" else "is", if (many(kept)) "their" else "its",
          if (length(kept) > 1L) "s" else "",
          paste(unique(vapply(kept, function(e) {
            paste0(e$button %||% "its button", if (!is.null(e$why)) paste0(", ", e$why) else "")
          }, character(1))), collapse = "; ")
        )
      },
      if (length(todo)) button()
    )
  }

  # When nothing the Update computes is from before, but a result that
  # keeps its own button is (a bootstrap run on a model since refit): one
  # small line under the page's own line, naming it and its button.
  note <- function() {
    if (length(pending())) {
      return(NULL)
    }
    kept <- held()
    if (!length(kept)) {
      return(NULL)
    }
    many <- length(kept) > 1L || grepl("[^siu]s$", kept[[1L]]$title)
    div(
      class = "small text-muted mb-2 sieve-status-note",
      sprintf(
        "The %s %s from before; %s computed again with %s own button%s (%s).",
        sieve_update_list_text(sieve_update_lower(vapply(kept, function(e) e$title, character(1)))),
        if (many) "are" else "is", if (many) "they are" else "it is", if (many) "their" else "its",
        if (length(kept) > 1L) "s" else "",
        paste(unique(vapply(kept, function(e) {
          paste0(e$button %||% "its button", if (!is.null(e$why)) paste0(", ", e$why) else "")
        }, character(1))), collapse = "; ")
      )
    )
  }

  run <- function() {
    todo <- isolate(pending())
    if (is.function(sequence)) return(shiny::isolate(sequence(todo)))
    if (!length(todo)) {
      sieve_notify("Nothing on this page is from before, so nothing was computed.")
      return(invisible(character(0)))
    }
    started <- proc.time()[["elapsed"]]
    done <- character(0)
    begun <- character(0)
    not_done <- character(0)
    titles <- vapply(todo, function(e) e$title, character(1))
    withProgress(
      message = sprintf("Updating %s (%s)", sieve_update_list_text(sieve_update_lower(titles)), isolate(duration())),
      value = 0,
      for (key in names(todo)) {
        # An earlier result of this Update may have brought this one up to
        # date (two results that share a fit), so it is read again first.
        now <- isolate(read())[[key]]
        if (is.null(now) || !now$state %in% c("stale", "settings", "failed") || !is.function(now$run)) {
          incProgress(1 / length(todo))
          next
        }
        shiny::setProgress(detail = now$title)
        outcome <- tryCatch(
          {
            now$run()
            NULL
          },
          shiny.silent.error = function(e) "its inputs are not ready",
          error = function(e) conditionMessage(e)
        )
        after <- isolate(read())[[key]]
        if (is.null(outcome) && identical(after$state, "current")) {
          done <- c(done, now$title)
        } else if (is.null(outcome) && (identical(after$state, "running") || isTRUE(now$background))) {
          # Started in a separate R process (the Network page's searches):
          # its line shows the time so far and Cancel.
          begun <- c(begun, now$title)
        } else {
          not_done <- c(not_done, if (is.null(outcome)) now$title else sprintf("%s (%s)", now$title, outcome))
        }
        incProgress(1 / length(todo))
      }
    )
    elapsed <- proc.time()[["elapsed"]] - started
    if (is.function(completion)) {
      notice <- completion(done, not_done, begun, elapsed)
      if (!is.null(notice)) do.call(sieve_notify, notice)
      return(invisible(done))
    }
    if (length(done)) {
      sieve_notify(sprintf("Updated the %s in %s.", sieve_update_list_text(sieve_update_lower(done)), sieve_seconds_text(elapsed)))
    }
    if (length(begun)) {
      sieve_notify(sprintf(
        "Started the %s in a separate R process; the page stays usable, and each line shows the time so far and Cancel.",
        sieve_update_list_text(sieve_update_lower(begun))
      ))
    }
    if (length(not_done)) {
      sieve_notify(
        sprintf(
          "Not brought up to date: %s. The message on the page says why.",
          paste(not_done, collapse = "; ")
        ),
        type = "warning", duration = NULL
      )
    }
    invisible(done)
  }

  observeEvent(input[[id]], run())

  list(
    pending = pending, held = held, seconds = seconds, label = label, button = button,
    line = line, note = note, run = run
  )
}

#' The duration of an Update whose component estimates are not all known
#'
#' The text in the Update button's label and in its progress message. With
#' every component known it is the rounded estimate (`sieve_duration_label()`,
#' "about 4 seconds"); with none known, "unknown time"; with some unknown,
#' the known part as a floor and the count of the unknown components, so
#' the label never passes a partial sum off as the whole. The floor is the rounded label without its "about"; when
#' the known part is under two seconds, the label says so instead of a
#' floor it cannot state.
#'
#' @param seconds The sum of the known component estimates (`NA` when none
#'   is known).
#' @param n_unknown How many pending components have no estimate.
#' @param n_pending How many components are pending.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_update_duration_text <- function(seconds, n_unknown = 0L, n_pending = n_unknown) {
  n_unknown <- as.integer(n_unknown)
  if (n_unknown <= 0L) {
    return(sieve_duration_label(seconds))
  }
  if (n_unknown >= n_pending || !is.finite(seconds)) {
    return("unknown time")
  }
  which <- if (n_unknown == 1L) "one component" else sprintf("%d components", n_unknown)
  known <- sieve_duration_label(seconds)
  if (seconds < 2) {
    return(sprintf("%s plus %s of unknown duration", known, which))
  }
  sprintf("at least %s; %s of unknown duration", sub("^about ", "", known), which)
}

#' The choice a selector keeps when its choices are offered again
#'
#' A page's selectors are offered their choices again whenever what they
#' list is read again (an item excluded or reverse scored, a ladder fit
#' again). Offered without a selection, a selector falls back to its first
#' choice, or to none, and the result the page's Update computes would then
#' follow a choice the researcher did not make (another grouping variable,
#' another rung), or have no outcomes to fit at all. The choice in force is
#' kept while it is still offered.
#'
#' @param current The selector's value now.
#' @param offered The choices offered (a vector, or a list of groups).
#' @param default The choice when the current one is not offered.
#' @param several `TRUE` for a selector of several values: every current
#'   value still offered is kept (none when none is).
#' @return The value to select.
#' @author Ken Kelley
#' @noRd
sieve_keep_choice <- function(current, offered, default = NULL, several = FALSE) {
  current <- as.character(current %||% character(0))
  kept <- intersect(current, as.character(unlist(offered, use.names = FALSE)))
  if (isTRUE(several)) {
    return(kept)
  }
  if (length(kept)) kept[1L] else default
}

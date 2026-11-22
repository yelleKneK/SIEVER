# Shared services.


#' Withdraw the confirmation when the pool's estimator decision changed
#'
#' An inclusion or an exclusion is in effect at once and leaves the
#' confirmation of the study in force, unless it changes the estimator the
#' included items' types call for (an ordered or binary item included into a
#' continuous pool, or the last one excluded): that is the decision the
#' confirmation is for, so it is withdrawn, as a type change withdraws it.
#' The estimators before and after the change are both resolved under the
#' study's present setting, not read from a stored label, so only a change
#' of the included items' types can withdraw it. Every writer of the item
#' status calls this (the Ingest grid and the Items tab through
#' `sieve_set_item_status()`, the constructs to analyze, the Optimizer's
#' Apply and Undo).
#'
#' @param state The shared state, with the ledger after the change.
#' @param before The ledger before the change.
#' @return Invisibly, `TRUE` when the confirmation was withdrawn.
#' @author Ken Kelley
#' @noRd
sieve_recheck_confirmation <- function(state, before) {
  shiny::isolate({
    after <- state$meta
    if (!isTRUE(state$confirmed) || is.null(before) || is.null(after)) {
      return(invisible(FALSE))
    }
    choice <- sieve_estimator_effective("study", state)
    n <- NROW(state$items)
    implied <- function(meta) {
      sieve_estimator_resolve(meta$type[meta$status == "active"], n = n, choice = choice)$label
    }
    if (identical(implied(before), implied(after))) {
      return(invisible(FALSE))
    }
    state$confirmed <- FALSE
    invisible(TRUE)
  })
}

#' Set the inclusion status of items in the ledger
#'
#' The one change behind every way of dropping or restoring an item: the
#' ledger status becomes `"parked"` (excluded from every analysis, kept in
#' the data) or `"active"` (included). The reason given for a drop is kept
#' in the ledger column `exclude_reason` (created when absent) and cleared
#' on a restore. The step is written to the reproducible code with the
#' reason as a comment. Items that are not in the ledger, or already have
#' the status asked for, are left alone and reported.
#'
#' The change is reported with its content coverage (the included items
#' of every construct and facet before and after, from
#' `sieve_item_coverage()` and `sieve_coverage_change()`) and the names of
#' the constructs and facets it leaves with no included item, so every
#' caller can show the advisory beside the decision. Nothing is prevented:
#' the researcher decides, and the advisory informs.
#'
#' @param state The shared state store.
#' @param items Character vector of item (variable) names.
#' @param status `"parked"` to drop, `"active"` to restore.
#' @param reason The researcher's reason, in their words (may be empty).
#' @param label Optional contextual title for the single recorded mutation.
#' @return Invisibly, a list with `changed` (the items whose status changed),
#'   `unchanged` (the items left alone), `coverage` (the coverage change
#'   table, or `NULL` without a ledger) and `emptied` (a list with
#'   `constructs`, the names of constructs left with no included item, and
#'   `facets`, a data frame of `construct` and `facet` for the facets).
#' @author Ken Kelley
#' @noRd
sieve_set_item_status <- function(state, items, status = c("parked", "active"), reason = "", label = NULL,
                                    reasons = NULL) {
  if (missing(status)) status <- "parked"
  items <- as.character(items)
  if (!length(items)) status <- "parked"
  if (!length(status) || !length(status) %in% c(1L, length(items)) || anyNA(status) ||
      any(!status %in% c("parked", "active"))) stop("Supply one inclusion status, or one per item.", call. = FALSE)
  requested <- stats::setNames(rep_len(status, length(items)), items)
  if (!is.null(reasons) && length(reasons) != length(items)) {
    stop("Supply one exclusion reason per requested item.", call. = FALSE)
  }
  requested_reasons <- if (!is.null(reasons)) stats::setNames(as.character(reasons), items) else NULL
  if (anyDuplicated(items)) {
    conflict <- vapply(split(unname(requested), items), function(x) length(unique(x)) > 1L, logical(1))
    if (any(conflict)) stop("An item has conflicting requested inclusion states.", call. = FALSE)
  }
  status <- unname(requested[!duplicated(items)])
  replay_before <- sieve_replay_inputs(state)
  m <- shiny::isolate(state$meta)
  items <- unique(as.character(items))
  before <- sieve_item_coverage(m)
  unchanged_report <- function(unchanged) {
    coverage <- if (is.null(m)) NULL else sieve_coverage_change(before, before)
    invisible(list(
      changed = character(0), unchanged = unchanged,
      coverage = coverage, emptied = sieve_coverage_emptied(coverage)
    ))
  }
  if (is.null(m) || !length(items)) {
    return(unchanged_report(items))
  }
  idx <- match(items, m$item)
  known <- !is.na(idx)
  changed <- items[known][m$status[idx[known]] != status[known]]
  unchanged <- setdiff(items, changed)
  if (!length(changed)) {
    return(unchanged_report(unchanged))
  }
  reason <- trimws(paste(reason %||% "", collapse = " "))
  rows <- match(changed, m$item)
  if (!"exclude_reason" %in% names(m)) {
    m$exclude_reason <- NA_character_
  }
  m_before <- m
  changed_status <- unname(requested[changed])
  changed_reasons <- if (is.null(requested_reasons)) rep(reason, length(changed)) else
    unname(requested_reasons[changed])
  m$status[rows] <- changed_status
  m$exclude_reason[rows] <- ifelse(changed_status == "parked" & !is.na(changed_reasons) &
    nzchar(changed_reasons), changed_reasons, NA_character_)
  sieve_write_ledger(state, m)
  sieve_recheck_confirmation(state, m_before)
  coverage <- sieve_coverage_change(before, sieve_item_coverage(m))
  state$item_actions <- c(shiny::isolate(state$item_actions) %||% list(), list(sieve_audit_stamp(list(
    items = changed, status = if (length(unique(changed_status)) == 1L) changed_status[1] else changed_status, reason = reason,
    previous_reasons = replay_before$meta$exclude_reason[match(changed, replay_before$meta$item)],
    exclusion_reasons = m$exclude_reason[rows],
    revision_before = replay_before$revision, revision = sieve_revision(state),
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ), state)))
  changes <- split(changed, changed_status)
  code <- unlist(lapply(names(changes), function(value) {
    quoted <- paste(encodeString(changes[[value]], quote = '"'), collapse = ", ")
    sprintf('meta$status[meta$item %%in%% c(%s)] <- "%s"', quoted, value)
  }), use.names = FALSE)
  code <- c(code, 'if (!"exclude_reason" %in% names(meta)) meta$exclude_reason <- NA_character_',
    sprintf("meta$exclude_reason[match(%s, meta$item)] <- %s",
    sieve_r_literal(changed), sieve_r_literal(m$exclude_reason[rows])))
  sieve_log(state,
    label %||% if (length(unique(changed_status)) == 1L) sprintf("%s %d item%s %s further consideration: %s%s",
      if (changed_status[1] == "parked") "Drop" else "Restore", length(changed), if (length(changed) == 1L) "" else "s",
      if (changed_status[1] == "parked") "from" else "to", paste(changed, collapse = ", "),
      if (nzchar(reason)) sprintf(" (reason: %s)", reason) else "") else
      sprintf("Update inclusion for %d items: %s", length(changed), paste(changed, collapse = ", ")),
    c(if (nzchar(reason)) sieve_comment_lines("Reason:", reason), code),
    before = replay_before, kind = "mutation")
  invisible(list(
    changed = changed, unchanged = unchanged,
    coverage = coverage, emptied = sieve_coverage_emptied(coverage)
  ))
}

#' Content Coverage of an Item Pool: Included Items per Construct and Facet
#'
#' Counts, for every construct and for every facet nested in a construct,
#' the items of an item ledger and how many of them are included
#' (`status == "active"`). Psychometric item selection can drop items but
#' cannot identify the content that is then missing, and narrowing an item
#' pool raises internal consistency at the expense of breadth (Clark &
#' Watson, 2019); the Standards ask that the content of each construct
#' remain represented (AERA, APA, & NCME, 2014). The table is the baseline
#' against which a change of inclusion is read (`sieve_coverage_change()`).
#'
#' @param meta An item ledger: a data frame with `construct`, `status`
#'   (`"active"` for an included item) and, optionally, `facet` (`NA` or
#'   `""` for an item without one). A ledger without `construct` or
#'   `status`, or without rows, gives an empty table.
#' @return A data frame with one row per construct and one per facet:
#'   `level` (`"construct"` or `"facet"`), `construct`, `facet` (`NA` on a
#'   construct row), `included` and `total` (integer counts).
#' @references
#' American Educational Research Association, American Psychological
#' Association, & National Council on Measurement in Education. (2014).
#' *Standards for educational and psychological testing*. AERA.
#'
#' Clark, L. A., & Watson, D. (2019). Constructing validity: New
#' developments in creating objective measuring instruments.
#' *Psychological Assessment, 31*(12), 1412--1427.
#' @author Ken Kelley
#' @examples
#' meta <- data.frame(
#'   item = paste0("x", 1:6),
#'   construct = rep(c("Visual", "Textual"), each = 3L),
#'   facet = c("Spatial", "Spatial", "Closure", NA, NA, NA),
#'   status = c("active", "parked", "active", "active", "active", "active"),
#'   stringsAsFactors = FALSE
#' )
#' sieve_item_coverage(meta)
#' @export
sieve_item_coverage <- function(meta) {
  empty <- data.frame(
    level = character(0), construct = character(0), facet = character(0),
    included = integer(0), total = integer(0), stringsAsFactors = FALSE
  )
  if (is.null(meta) || !is.data.frame(meta) || !nrow(meta) ||
    !all(c("construct", "status") %in% names(meta))) {
    return(empty)
  }
  construct <- as.character(meta$construct)
  construct[!sieve_construct_assigned(construct)] <- "Unassigned"
  included <- !is.na(meta$status) & meta$status == "active"
  facet <- if ("facet" %in% names(meta)) as.character(meta$facet) else rep(NA_character_, nrow(meta))
  facet[!is.na(facet) & !nzchar(facet)] <- NA_character_
  cons <- unique(construct)
  rows_construct <- data.frame(
    level = rep("construct", length(cons)), construct = cons, facet = rep(NA_character_, length(cons)),
    included = vapply(cons, function(cn) sum(included & construct == cn), integer(1), USE.NAMES = FALSE),
    total = vapply(cons, function(cn) sum(construct == cn), integer(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  with_facet <- !is.na(facet)
  pairs <- unique(data.frame(construct = construct[with_facet], facet = facet[with_facet], stringsAsFactors = FALSE))
  rows_facet <- if (nrow(pairs)) {
    count <- function(only_included) {
      vapply(seq_len(nrow(pairs)), function(i) {
        in_facet <- with_facet & construct == pairs$construct[i] & facet == pairs$facet[i]
        sum(if (only_included) in_facet & included else in_facet)
      }, integer(1))
    }
    data.frame(
      level = rep("facet", nrow(pairs)), construct = pairs$construct, facet = pairs$facet,
      included = count(TRUE), total = count(FALSE), stringsAsFactors = FALSE
    )
  } else {
    empty
  }
  out <- rbind(rows_construct, rows_facet)
  row.names(out) <- NULL
  out
}

#' The Change in Content Coverage Between Two States of an Item Pool
#'
#' Sets the included count of every construct and facet before a change of
#' inclusion against the count after it, and names the constructs and
#' facets the change leaves with no included item (`emptied`) and those
#' below three included items (`thin`). A construct or facet absent from
#' one table (a facet named after the fact) counts as zero there.
#'
#' @param before,after Coverage tables from `sieve_item_coverage()`.
#' @return A data frame with `level`, `construct`, `facet`, `before`,
#'   `after` (included counts), `total` (items in the ledger after),
#'   `emptied` (included before, none after) and `thin` (fewer than three
#'   included after).
#' @author Ken Kelley
#' @examples
#' meta <- data.frame(
#'   item = paste0("x", 1:6),
#'   construct = rep(c("Visual", "Textual"), each = 3L),
#'   facet = c("Spatial", "Spatial", "Closure", NA, NA, NA),
#'   status = "active", stringsAsFactors = FALSE
#' )
#' before <- sieve_item_coverage(meta)
#' meta$status[3] <- "parked"
#' change <- sieve_coverage_change(before, sieve_item_coverage(meta))
#' change[change$emptied, ]
#' @export
sieve_coverage_change <- function(before, after) {
  key <- function(x) paste(x$level, x$construct, ifelse(is.na(x$facet), "", x$facet), sep = "\r")
  before_key <- key(before)
  after_key <- key(after)
  keys <- unique(c(before_key, after_key))
  at_before <- match(keys, before_key)
  at_after <- match(keys, after_key)
  source <- ifelse(is.na(at_after), "before", "after")
  pick <- function(column) {
    ifelse(source == "after", after[[column]][at_after], before[[column]][at_before])
  }
  count <- function(table, at) ifelse(is.na(at), 0L, table$included[at])
  out <- data.frame(
    level = pick("level"), construct = pick("construct"), facet = pick("facet"),
    before = as.integer(count(before, at_before)), after = as.integer(count(after, at_after)),
    total = as.integer(pick("total")), stringsAsFactors = FALSE
  )
  out$emptied <- out$before > 0L & out$after == 0L
  # Below three included items for a construct, below two for a facet (a
  # facet is identified through the rest of its construct).
  out$thin <- out$after < ifelse(out$level == "facet", 2L, 3L)
  row.names(out) <- NULL
  out
}

#' The constructs and facets a coverage change leaves with no included item
#' @param change A table from `sieve_coverage_change()`, or `NULL`.
#' @return A list with `constructs` (names) and `facets` (a data frame of
#'   `construct` and `facet`).
#' @author Ken Kelley
#' @noRd
sieve_coverage_emptied <- function(change) {
  none <- list(constructs = character(0), facets = data.frame(construct = character(0), facet = character(0), stringsAsFactors = FALSE))
  if (is.null(change) || !nrow(change)) return(none)
  gone <- change[change$emptied, , drop = FALSE]
  list(
    constructs = gone$construct[gone$level == "construct"],
    facets = data.frame(
      construct = gone$construct[gone$level == "facet"], facet = gone$facet[gone$level == "facet"],
      stringsAsFactors = FALSE
    )
  )
}

#' One sentence per construct or facet a change leaves with no included item
#' @param change A table from `sieve_coverage_change()`.
#' @return Character sentences (none when nothing was emptied).
#' @author Ken Kelley
#' @noRd
sieve_coverage_sentences <- function(change) {
  if (is.null(change) || !nrow(change)) return(character(0))
  gone <- change[change$emptied, , drop = FALSE]
  if (!nrow(gone)) return(character(0))
  vapply(seq_len(nrow(gone)), function(i) {
    what <- if (identical(gone$level[i], "facet")) {
      sprintf("facet %s of construct %s", gone$facet[i], gone$construct[i])
    } else {
      sprintf("construct %s", gone$construct[i])
    }
    sprintf(
      "This leaves %s with no included items (%d included before, 0 now); its content is no longer represented.",
      what, gone$before[i]
    )
  }, character(1))
}

#' The constructs and facets below three included items, in one sentence
#'
#' Names every construct with fewer than three included items and every
#' facet with fewer than two (a facet is identified through the rest of its
#' construct, so two items is often by design), those with none included
#' among them (a construct or facet whose last item was excluded stays
#' named, as its content is then unrepresented).
#'
#' @param coverage A table from `sieve_item_coverage()`.
#' @return One sentence, or `NULL` when every construct and facet has three
#'   or more included items.
#' @author Ken Kelley
#' @noRd
sieve_coverage_thin_text <- function(coverage) {
  if (is.null(coverage) || !nrow(coverage)) return(NULL)
  thin <- coverage[coverage$included < ifelse(coverage$level == "facet", 2L, 3L), , drop = FALSE]
  if (!nrow(thin)) return(NULL)
  parts <- vapply(seq_len(nrow(thin)), function(i) {
    name <- if (identical(thin$level[i], "facet")) {
      sprintf("facet %s of %s", thin$facet[i], thin$construct[i])
    } else {
      thin$construct[i]
    }
    sprintf(
      "%s (%d of %d included%s)", name, thin$included[i], thin$total[i],
      if (thin$included[i] == 0L) ", its content is not represented" else ""
    )
  }, character(1))
  sprintf(
    "Below three included items%s: %s.",
    if (any(thin$level == "facet")) " (two for a facet)" else "", paste(parts, collapse = "; ")
  )
}

#' A link to the Content Validity page from a coverage advisory
#'
#' Sent as a namespaced event (several advisories of one module share it
#' without a DOM id each); `sieve_coverage_navigation()` wires it.
#'
#' @param ns The module namespace function, or `NULL` for plain text.
#' @param label The link text.
#' @return A link tag, or the label as text.
#' @author Ken Kelley
#' @noRd
sieve_coverage_link <- function(ns = NULL, label = "Content Validity page") {
  if (!is.function(ns)) return(label)
  shiny::tags$a(href = "#", onclick = sprintf(
    'Shiny.setInputValue(%s, Date.now(), {priority: "event"}); return false;',
    encodeString(ns("coverage_open_content"), quote = '"')
  ), label)
}

#' Wire the coverage advisory's link to the Content Validity page
#' @param input,session Module input and session.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_coverage_navigation <- function(input, session) {
  shiny::observeEvent(input$coverage_open_content, {
    sieve_go(session, "content")
  }, ignoreInit = TRUE)
  invisible(NULL)
}

#' The coverage advisory beside an inclusion decision
#'
#' Shown when a change of inclusion leaves a construct or a facet with no
#' included item: which, with the included counts before and after, why
#' it matters (Clark & Watson, 2019; AERA, APA, & NCME, 2014), and a link
#' to the Content Validity page. Nothing is prevented and no reason is
#' demanded; the exclusion stands as the researcher made it.
#'
#' @param out The result of `sieve_set_item_status()`.
#' @param ns The module namespace function, or `NULL`.
#' @return A tag, or `NULL` when no construct or facet was emptied.
#' @author Ken Kelley
#' @noRd
sieve_coverage_advisory <- function(out, ns = NULL) {
  sentences <- sieve_coverage_sentences(out$coverage)
  if (!length(sentences)) return(NULL)
  shiny::div(
    class = "alert alert-warning py-2 small mt-2 mb-2 sieve-coverage-advisory", role = "status",
    shiny::tags$b("Content coverage. "),
    paste(sentences, collapse = " "), " ",
    paste0(
      "Psychometric item selection can drop items but cannot identify the content that is then missing, ",
      "and narrowing a pool raises internal consistency at the expense of breadth (Clark & Watson, 2019); ",
      "the Standards ask that the content of each construct remain represented (AERA, APA, & NCME, 2014). ",
      "The exclusion stands as you made it. Review what the pool still covers on the "
    ),
    sieve_coverage_link(ns), "."
  )
}

#' Show the coverage advisory as a notification
#'
#' For the callers without a status line of their own (the Ingest grid's
#' Use cell, the Content Validity page's exclusion, the Optimizer's Apply):
#' the advisory `sieve_coverage_advisory()` builds, as a notification that
#' stays until dismissed. Nothing when no construct or facet was emptied,
#' or outside a Shiny session.
#'
#' @param out The result of `sieve_set_item_status()`.
#' @return `out`, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_coverage_notify <- function(out) {
  session <- shiny::getDefaultReactiveDomain()
  advisory <- if (is.list(out)) sieve_coverage_advisory(out) else NULL
  if (!is.null(session) && !is.null(advisory)) {
    sieve_notify(advisory, type = "warning", duration = NULL)
  }
  invisible(out)
}

#' The choices of the item pickers in the drop dialog
#'
#' Each item labeled by its item name, its variable and its construct, so a
#' typed name, variable or construct finds it.
#'
#' @param meta The item ledger.
#' @param status The status of the items offered.
#' @return A named character vector (labels as names, variables as values).
#' @author Ken Kelley
#' @noRd
sieve_drop_choices <- function(meta, status = "active") {
  if (is.null(meta) || !nrow(meta)) {
    return(character(0))
  }
  m <- meta[meta$status %in% status, , drop = FALSE]
  if (!nrow(m)) {
    return(character(0))
  }
  label <- as.character(m$label)
  label[is.na(label) | !nzchar(label)] <- m$item[is.na(label) | !nzchar(label)]
  cons <- as.character(m$construct)
  where <- ifelse(is.na(cons) | !nzchar(cons), "", sprintf(", %s", cons))
  text <- ifelse(label == m$item, sprintf("%s%s", m$item, where),
    sprintf("%s (%s%s)", label, m$item, where)
  )
  stats::setNames(as.character(m$item), text)
}

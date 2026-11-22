# Metadata edit.

#' Compare Two Item Ledgers Once for All Change Descriptions
#' @param then,now Earlier and current item ledgers.
#' @return Aligned ledgers, common, joined and departed items, and changed fields.
#' @author Ken Kelley
#' @noRd
sieve_ledger_diff <- function(then, now) {
  common <- intersect(then$item, now$item)
  a <- then[match(common, then$item), , drop = FALSE]
  b <- now[match(common, now$item), , drop = FALSE]
  fields <- union(names(a), names(b))
  changed <- lapply(fields, function(field) {
    if (!field %in% names(a) || !field %in% names(b)) return(rep(FALSE, length(common)))
    x <- as.character(a[[field]])
    y <- as.character(b[[field]])
    !((x == y) %in% TRUE | (is.na(x) & is.na(y)))
  })
  names(changed) <- fields
  list(a = a, b = b, common = common, joined = setdiff(now$item, then$item),
    left = setdiff(then$item, now$item), changed = changed)
}

#' Preview a simultaneous item metadata change
#' @param meta Current item ledger.
#' @param ids Stable item identifiers.
#' @param field Metadata column.
#' @param value One value or one per selected item.
#' @return Checked before/after patch.
#' @author Ken Kelley
#' @noRd
sieve_metadata_patch <- function(meta, ids, field, value) {
  allowed <- c("construct", "facet", "type", "label", "wording", "note")
  if (!field %in% allowed || !field %in% names(meta)) stop("Choose an editable metadata field.", call. = FALSE)
  ids <- unique(as.character(ids))
  if (!length(ids) || anyNA(match(ids, meta$item))) stop("Select existing item identifiers.", call. = FALSE)
  if (!length(value) %in% c(1L, length(ids))) stop("Supply one value or one value per selected item.", call. = FALSE)
  value <- rep_len(as.character(value), length(ids))
  value <- trimws(value)
  if (field == "construct") value[is.na(value) | !nzchar(value)] <- "Unassigned"
  if (field == "facet") value[is.na(value) | !nzchar(value)] <- NA_character_
  if (field == "type" && anyNA(match(
    value,
    c(
      "continuous",
      "ordered",
      "binary"
    )
  ))) stop("Unknown item type.",
    call. = FALSE
  )
  idx <- match(ids, meta$item)
  after <- meta
  after[[field]][idx] <- value
  assigned <- !is.na(after$facet) & nzchar(after$facet)
  no_parent <- is.na(after$construct) | after$construct %in% c("", "Unassigned")
  if (any(assigned & no_parent)) stop("Every facet needs a named parent construct. Clear the facet before removing its construct.",
    call. = FALSE
  )
  data.frame(
    item = ids, field = field, before = as.character(meta[[field]][idx]), after = value,
    stringsAsFactors = FALSE
  )
}

#' Apply one checked metadata batch and retain its reversal
#' @param state Shared state.
#' @param patch A metadata patch.
#' @param note Researcher-visible action description.
#' @return Edit record invisibly.
#' @author Ken Kelley
#' @noRd
sieve_apply_metadata_patch <- function(state, patch, note = "Edit item metadata") {
  replay_before <- sieve_replay_inputs(state)
  m <- shiny::isolate(state$meta)
  # A type set here is the researcher's choice (type_source "chosen");
  # where each type came from before is kept with the record, so an undo
  # gives it back.
  typed <- unique(patch$item[patch$field == "type"])
  source_before <- if (length(typed)) {
    stats::setNames(sieve_type_source(m)[match(typed, m$item)], typed)
  }
  for (field in unique(patch$field)) {
    p <- patch[patch$field == field, , drop = FALSE]
    checked <- sieve_metadata_patch(m, p$item, field, p$after)
    if (!identical(
      checked$before,
      p$before
    )) stop("The metadata changed after this preview. Prepare it again.",
      call. = FALSE
    )
    m[[field]][match(p$item, m$item)] <- p$after
  }
  if (all((is.na(patch$before) & is.na(patch$after)) | (!is.na(patch$before) & !is.na(patch$after) & patch$before == patch$after))) return(invisible(NULL))
  record <- list(
    patch = patch,
    note = note,
    time = format(Sys.time(),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    undone = FALSE,
    type_source_before = source_before
  )
  if (length(typed)) {
    m <- sieve_mark_type_source(m, typed, "chosen")
  }
  record <- sieve_audit_stamp(record, state)
  sieve_write_ledger(state, m)
  if (any(patch$field %in% c("type", "construct"))) state$confirmed <- FALSE
  state$metadata_edits <- c(shiny::isolate(state$metadata_edits) %||% list(), list(record))
  sieve_log(state, sprintf("%s (%d items)", note, length(unique(patch$item))),
    sieve_metadata_patch_code(patch),
    before = replay_before, kind = "mutation"
  )
  invisible(record)
}

#' Undo the latest unreversed metadata batch without overwriting unrelated edits
#' @param state Shared state.
#' @return Reversal record invisibly.
#' @author Ken Kelley
#' @noRd
sieve_undo_metadata_edit <- function(state) {
  replay_before <- sieve_replay_inputs(state)
  history <- shiny::isolate(state$metadata_edits) %||% list()
  available <- which(!vapply(history, function(x) isTRUE(x$undone), logical(1)))
  if (!length(available)) stop("There is no metadata edit to undo.", call. = FALSE)
  k <- tail(available, 1L)
  p <- history[[k]]$patch
  m <- shiny::isolate(state$meta)
  for (field in unique(p$field)) {
    z <- p[p$field == field, , drop = FALSE]
    now <- as.character(m[[field]][match(z$item, m$item)])
    if (!identical(
      now,
      z$after
    )) stop("An affected field changed again. Review that later edit before undoing this one.",
      call. = FALSE
    )
    m[[field]][match(z$item, m$item)] <- z$before
  }
  # Where each retyped item's type came from returns with it.
  was <- history[[k]]$type_source_before
  if (length(was)) {
    for (src in unique(was)) m <- sieve_mark_type_source(m, names(was)[was == src], src)
  }
  sieve_write_ledger(state, m)
  if (any(p$field %in% c("type", "construct"))) state$confirmed <- FALSE
  history[[k]]$undone <- TRUE
  history[[k]]$reversed_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  state$metadata_edits <- history
  inverse <- p
  inverse$before <- p$after
  inverse$after <- p$before
  sieve_log(state, paste("Undo metadata edit:", history[[k]]$note), sieve_metadata_patch_code(inverse),
    before = replay_before, kind = "mutation"
  )
  invisible(history[[k]])
}

#' Exact R for a stable-identifier metadata patch
#' @param patch Checked before/after patch.
#' @return Executable R lines.
#' @author Ken Kelley
#' @noRd
sieve_metadata_patch_code <- function(patch) {
  c(
    "# Simultaneous metadata edit by stable item identifier.",
    paste0("metadata_patch <- ", paste(utils::capture.output(dput(patch)), collapse = "\n")),
    "for (.field in unique(metadata_patch$field)) {",
    "  .patch <- metadata_patch[metadata_patch$field == .field, , drop = FALSE]",
    "  meta[[.field]][match(.patch$item, meta$item)] <- .patch$after",
    "}"
  )
}

#' Controls for reviewed batch assignment and group naming
#' @param ns Module namespace.
#' @return UI tags.
#' @author Ken Kelley
#' @noRd
sieve_metadata_controls_ui <- function(ns) {
  shiny::tags$details(
    shiny::tags$summary("Batch assignments, group names, and undo"),
    shiny::p(
      class = "small",
      "Select rows for a batch assignment. Rename changes every item in one group; Merge is an explicit separate action. Frozen historical definitions and claims retain their original wording."
    ),
    shiny::selectInput(ns("meta_field"), "Metadata field", c("Construct" = "construct", "Facet" = "facet")),
    shiny::textInput(ns("meta_value"), "Assignment or new group name"),
    shiny::actionButton(ns("meta_batch"), "Preview assignment to selected items"),
    shiny::textInput(ns("meta_parent"), "Parent construct (required for a facet group)"),
    shiny::textInput(ns("meta_old"), "Existing group name"),
    shiny::actionButton(ns("meta_rename"), "Preview rename group"),
    shiny::actionButton(ns("meta_merge"), "Preview merge into existing group"),
    shiny::actionButton(ns("meta_undo"), "Undo last metadata edit"),
    shiny::uiOutput(ns("meta_edit_status"))
  )
}

#' Wire the shared metadata editor to stable selected identifiers
#' @param input,output,session Module objects.
#' @param state Shared state.
#' @param selected Function returning stable selected item identifiers.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_metadata_controls_server <- function(input, output, session, state, selected) {
  pending <- shiny::reactiveVal(NULL)
  message <- shiny::reactiveVal(NULL)
  preview <- function(mode) {
    tryCatch(
      {
        m <- shiny::isolate(state$meta)
        field <- input$meta_field %||% "construct"
        value <- input$meta_value %||% ""
        ids <- selected()
        if (mode != "batch") {
          old <- trimws(input$meta_old %||% "")
          parent <- trimws(input$meta_parent %||% "")
          if (!nzchar(old) || !nzchar(trimws(value))) stop("Give the existing and destination group names.")
          scope <- if (field == "facet") {
            if (!nzchar(parent)) stop("Name the facet's parent construct.")
            !is.na(m$construct) & m$construct == parent
          } else rep(TRUE, nrow(m))
          ids <- m$item[scope & !is.na(m[[field]]) & m[[field]] == old]
          exists <- any(scope & !is.na(m[[field]]) & m[[field]] == trimws(value))
          if (mode == "rename" && exists && !identical(
            old,
            trimws(value)
          )) stop("That group already exists. Choose Merge explicitly.")
          if (mode == "merge" && !exists) stop("Merge requires an existing destination group.")
        }
        patch <- sieve_metadata_patch(m, ids, field, value)
        pending(list(
          patch = patch,
          note = paste(
            switch(mode,
              batch = "Assign selected items",
              rename = "Rename group",
              merge = "Merge groups"
            ),
            field
          )
        ))
        shiny::showModal(shiny::modalDialog(
          title = "Review metadata changes", easyClose = TRUE,
          shiny::p(sprintf("%d items. Values and inclusion status are unchanged.", nrow(patch))),
          sieve_plain_table_ui(patch),
          shiny::actionButton(session$ns("meta_apply"),
            "Apply reviewed changes",
            class = "btn-primary"
          ),
          footer = shiny::modalButton("Cancel")
        ))
      },
      error = function(e) message(conditionMessage(e))
    )
  }
  shiny::observeEvent(input$meta_batch, preview("batch"))
  shiny::observeEvent(input$meta_rename, preview("rename"))
  shiny::observeEvent(input$meta_merge, preview("merge"))
  shiny::observeEvent(input$meta_apply, {
    p <- pending()
    shiny::req(p)
    tryCatch(
      {
        sieve_apply_metadata_patch(
          state,
          p$patch,
          p$note
        )
        pending(NULL)
        shiny::removeModal()
        message("Reviewed metadata edit retained. Reconfirm the study if requested.")
      },
      error = function(e) message(conditionMessage(e))
    )
  })
  shiny::observeEvent(
    input$meta_undo,
    tryCatch(
      {
        r <- sieve_undo_metadata_edit(state)
        message(paste(
          "Undid",
          r$note
        ))
      },
      error = function(e) message(conditionMessage(e))
    )
  )
  output$meta_edit_status <- shiny::renderUI(shiny::p(role = "status", class = "small", message()))
  invisible(NULL)
}

#' Write a Checked Item Ledger
#' @param state Shared application state.
#' @param meta The new ledger.
#' @return The ledger invisibly.
#' @author Ken Kelley
#' @noRd
sieve_write_ledger <- function(state, meta) {
  if (!is.null(meta) && (!is.data.frame(meta) || !"item" %in% names(meta) || anyNA(meta$item) || anyDuplicated(meta$item))) {
    stop("An item ledger must have one uniquely named row per item.", call. = FALSE)
  }
  state$meta <- meta
  invisible(meta)
}

#' Set Item Notes Through the Shared Ledger Writer
#' @param state Shared state.
#' @param items Items to annotate.
#' @param notes Notes in the same order.
#' @return The updated ledger invisibly.
#' @author Ken Kelley
#' @noRd
sieve_set_note <- function(state, items, notes) {
  m <- shiny::isolate(state$meta)
  at <- match(items, m$item)
  if (anyNA(at) || length(notes) != length(items)) stop("Match each note to a known item.", call. = FALSE)
  m$note[at] <- as.character(notes)
  sieve_write_ledger(state, m)
}

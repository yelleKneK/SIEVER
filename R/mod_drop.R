# mod_drop.R
# Excluding and including items from any tab. The navbar carries an "Exclude /
# Include items" link that opens one dialog: pick the items to exclude (by item
# name, variable or construct), say why, and exclude them; or pick excluded
# items and include them again. Excluding sets the ledger status to excluded
# ("parked"), the same change the Ingest grid's Use cell and the Items tab's
# Included/Excluded cell make, so every tab follows and nothing is deleted. The
# reason is kept in the ledger column `exclude_reason` and in the reproducible
# code. The CFA Estimates table and the EFA Pattern and Structure tables open
# this same dialog with their selected rows filled in
# (sieve_item_decision_request(), fct_item_decision.R).

























#' The navbar link that opens the drop dialog
#'
#' The link alone; the application UI places it in the navbar beside the
#' About link, in one item, so the navbar keeps two rows at 1,280 px.
#'
#' @param id Module id.
#' @return A [shiny::actionLink()].
#' @author Ken Kelley
#' @noRd
mod_drop_link_ui <- function(id) {
  ns <- NS(id)
  # A short label without an icon (the full name is the link's title and
  # the dialog's).
  shiny::actionLink(ns("open"),
    label = "Exclude / Include items", class = "nav-link",
    title = "Exclude or include items, from any tab"
  )
}

#' Exclude and include items from any tab: server
#'
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer observeEvent showModal modalDialog modalButton renderUI
#' @importFrom shiny selectizeInput textAreaInput actionButton showNotification req
#' @importFrom DT renderDT datatable DTOutput
mod_drop_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # The coverage advisory of the last exclusion made here, beside the
    # Exclude button; cleared by an inclusion.
    coverage_note <- reactiveVal(NULL)
    output$coverage <- renderUI(coverage_note())
    sieve_coverage_navigation(input, session)

    # A page's request to open this dialog with items filled in (the CFA
    # Estimates table, the EFA Pattern and Structure tables, through
    # sieve_item_decision_request()): the same dialog, the picker of the
    # requested side filled, and nothing changed until Exclude or Include
    # is pressed. The navbar link opens it unfilled.
    prefill <- reactiveVal(NULL)
    open_dialog <- function() {
      showModal(modalDialog(
        title = "Exclude / Include items",
        easyClose = TRUE,
        size = "l",
        uiOutput(ns("body")),
        footer = modalButton("Close")
      ))
    }
    observeEvent(input$open, {
      prefill(NULL)
      open_dialog()
    })
    observeEvent(state$item_decision_request, {
      request <- state$item_decision_request
      req(is.list(request))
      prefill(request)
      open_dialog()
    }, ignoreNULL = TRUE)
    # Once the decision asked for from a page is made, the request is spent
    # and the dialog closes, so the page's status line is what is seen next;
    # it stays open when the decision left a construct or a facet with no
    # included item, so the coverage advisory beside the button is read.
    # A dialog closed without deciding keeps the request until the next
    # one replaces it (Shiny gives no event for that close); both ways of
    # opening reset the filled items, a new dataset empties the field, and
    # the observer ignores the emptied field, so nothing is replayed.
    settle_request <- function(close = TRUE) {
      if (is.null(prefill())) {
        return(invisible(FALSE))
      }
      prefill(NULL)
      state$item_decision_request <- NULL
      if (close) shiny::removeModal()
      invisible(TRUE)
    }

    output$body <- renderUI({
      m <- state$meta
      if (is.null(m)) {
        return(p(
          class = "text-muted",
          "No data are loaded yet. Load a file or a bundled example on the Ingest tab first."
        ))
      }
      n_in <- sum(m$status == "active")
      dropped <- sieve_drop_choices(m, "parked")
      pre <- prefill()
      # The items a page asked to fill in, on the side they belong to.
      pre_items <- function(status) {
        if (!is.list(pre) || !identical(pre$status, status)) {
          return(NULL)
        }
        intersect(pre$items, m$item[m$status == if (status == "parked") "active" else "parked"])
      }
      tagList(
        p(
          class = "small",
          "An excluded item is left out of every analysis (Screening, Dimensionality, EFA, ",
          "Items, CFA, Reliability and the rest) and stays in the data: nothing is deleted, and ",
          "it can be included again here, in the Ingest grid's Use cell, or in the Items tab's ",
          "Included/Excluded cell. The reason you give is kept with the item and written to ",
          "the reproducible code. An analysis already requested for this study is refit by ",
          "itself after the change, and each page's status line says Refitting, Current, ",
          "Update needed, Stopped, or why it was not refit."
        ),
        if (is.list(pre)) p(
          class = "small sieve-item-decision-origin",
          strong(paste0("Opened from ", pre$origin, ". ")),
          if (length(filled <- pre_items(pre$status))) {
            sprintf(
              "%d selected item%s %s filled in below; add or remove items before deciding.",
              length(filled), if (length(filled) == 1L) "" else "s",
              if (length(filled) == 1L) "is" else "are"
            )
          } else {
            "No row was selected there, so pick the items below."
          }
        ),
        p(strong(sprintf("%d of %d items are included.", n_in, nrow(m)))),
        selectizeInput(
          ns("drop_pick"), "Items to exclude",
          choices = sieve_drop_choices(m, "active"),
          selected = pre_items("parked"),
          multiple = TRUE, width = "100%",
          options = list(
            placeholder = "Type an item name, a variable or a construct",
            plugins = list("remove_button")
          )
        ),
        textAreaInput(ns("drop_reason"), "Reason for excluding them (kept with the items)",
          rows = 2, width = "100%",
          placeholder = "For example: low corrected item-total correlation; wording duplicates item 12."
        ),
        uiOutput(ns("drop_preview")),
        actionButton(ns("drop_go"), "Exclude the selected items", class = "btn-primary"),
        # The content coverage advisory, beside the decision: shown when an
        # exclusion leaves a construct or a facet with no included item.
        uiOutput(ns("coverage")),
        tags$hr(),
        tags$h6(sprintf("Excluded items (%d)", length(dropped))),
        if (length(dropped)) {
          tagList(
            DTOutput(ns("dropped_tbl")),
            selectizeInput(
              ns("restore_pick"), "Items to include again",
              choices = dropped, multiple = TRUE,
              selected = pre_items("active"),
              width = "100%",
              options = list(
                placeholder = "Type an item name or a variable",
                plugins = list("remove_button")
              )
            ),
            textAreaInput(ns("restore_reason"), "Reason for including them again (kept in the decision history)",
              rows = 2, width = "100%"
            ),
            uiOutput(ns("restore_preview")),
            actionButton(ns("restore_go"), "Include the selected items", class = "btn-outline-primary")
          )
        } else {
          p(class = "text-muted small", "No item is excluded.")
        },
        tags$details(tags$summary("Item inclusion decision history"), DTOutput(ns("item_history")))
      )
    })

    output$drop_preview <- renderUI({
      state$meta
      state$items
      sieve_item_change_preview_ui(sieve_item_change_preview(state, input$drop_pick, "parked"))
    })
    output$restore_preview <- renderUI({
      state$meta
      state$items
      sieve_item_change_preview_ui(sieve_item_change_preview(state, input$restore_pick, "active"))
    })

    output$dropped_tbl <- renderDT({
      m <- state$meta
      req(m)
      d <- m[m$status != "active", , drop = FALSE]
      req(nrow(d) > 0)
      reason <- if ("exclude_reason" %in% names(d)) as.character(d$exclude_reason) else rep(NA_character_, nrow(d))
      reason[is.na(reason) | !nzchar(reason)] <- "(no reason given)"
      df <- data.frame(
        Variable = d$item, ItemName = d$label, Construct = d$construct,
        Reason = reason, stringsAsFactors = FALSE
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        colnames = c("Variable Name", "Item Name", "Construct", "Reason"),
        options = list(dom = "tp", pageLength = 8, scrollX = TRUE)
      )
    })

    output$item_history <- renderDT({
      history <- state$item_actions %||% list()
      rows <- lapply(history, function(record) {
        data.frame(
          Item = record$items, Status = ifelse(rep_len(record$status, length(record$items)) == "active", "Included", "Excluded"),
          Reason = if (nzchar(record$reason %||% "")) record$reason else "No reason given",
          Time = record$time, stringsAsFactors = FALSE
        )
      })
      data <- if (length(rows)) do.call(rbind, rows) else
        data.frame(Item = character(0), Status = character(0), Reason = character(0), Time = character(0))
      sieve_dt(data,
        rownames = FALSE, escape = TRUE, selection = "none",
        options = list(dom = "tp", pageLength = 8, scrollX = TRUE)
      )
    })

    observeEvent(input$drop_go, {
      picked <- input$drop_pick
      if (!length(picked)) {
        sieve_notify("Pick at least one item to exclude.", type = "warning")
        return(invisible(NULL))
      }
      m <- state$meta
      left <- sum(m$status == "active" & !m$item %in% picked)
      if (left < 2L) {
        sieve_notify(
          sprintf(
            "Excluding these would leave %d included item%s; at least two must stay included.",
            left, if (left == 1L) "" else "s"
          ),
          type = "error"
        )
        return(invisible(NULL))
      }
      out <- sieve_set_item_status(state, picked, "parked", input$drop_reason)
      shiny::updateTextAreaInput(session, "drop_reason", value = "")
      coverage_note(sieve_coverage_advisory(out, ns = ns))
      emptied <- sieve_coverage_sentences(out$coverage)
      sieve_notify(
        sprintf(
          "Excluded %d item%s: %s. Every tab now works without %s.%s",
          length(out$changed), if (length(out$changed) == 1L) "" else "s",
          paste(out$changed, collapse = ", "), if (length(out$changed) == 1L) "it" else "them",
          if (length(emptied)) paste0(" ", paste(emptied, collapse = " ")) else ""
        ),
        type = if (length(emptied)) "warning" else "message"
      )
      settle_request(close = !length(emptied))
    })

    observeEvent(input$restore_go, {
      picked <- input$restore_pick
      if (!length(picked)) {
        sieve_notify("Pick at least one item to include again.", type = "warning")
        return(invisible(NULL))
      }
      out <- sieve_set_item_status(state, picked, "active", input$restore_reason %||% "")
      shiny::updateTextAreaInput(session, "restore_reason", value = "")
      coverage_note(NULL)
      sieve_notify(
        sprintf(
          "Included %d item%s again: %s.", length(out$changed), if (length(out$changed) == 1L) "" else "s",
          paste(out$changed, collapse = ", ")
        ),
        type = "message", duration = 6
      )
      settle_request()
    })
  })
}

# Project controls are available from every tab. Browser downloads have no
# reliable completion event, so preparing a file never means it was saved.





#' Project toolbar
#' @param id Module identifier.
#' @return UI tags.
#' @author Ken Kelley
#' @noRd
mod_project_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "sieve-project-toolbar border-bottom px-3 py-2 bg-light",
    shiny::div(
      class = "d-flex flex-wrap align-items-center gap-2",
      shiny::div(
        class = "sieve-project-name d-flex align-items-center gap-2",
        shiny::tags$label(`for` = ns("name"), class = "small mb-0 text-nowrap", "Project name"),
        shiny::textInput(ns("name"), NULL, "Untitled project", width = "200px")
      ),
      shiny::downloadButton(ns("save"), "Save project", class = "btn-primary btn-sm"),
      shiny::actionButton(ns("save_as"), "Save project as", class = "btn-outline-primary btn-sm"),
      shiny::actionButton(ns("open"), "Open project", class = "btn-outline-primary btn-sm"),
      shiny::actionButton(ns("assistant"), "Scientific task assistant", class = "btn-outline-secondary btn-sm"),
      shiny::uiOutput(ns("status"), inline = TRUE),
      # In the toolbar's one row and closed by default: open below the
      # toolbar, it would put a paragraph above every page's first result.
      shiny::tags$details(
        class = "sieve-project-saving small",
        shiny::tags$summary("Saving in a browser"),
        shiny::p(
          class = "small mb-1",
          "Save prepares a download under the project name. Save project as offers a new name for a separate copy. Your browser chooses its destination and may rename an existing file. SIEVER cannot verify completion or overwrite an arbitrary local file. After keeping the download, acknowledge it below; a canceled download leaves the project marked as changed."
        )
      )
    ),
    shiny::uiOutput(ns("prepared_copy"))
  )
}

#' Project toolbar server
#' @param id Module identifier.
#' @param state Shared state.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
mod_project_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    sieve_assistant_wire(input, output, session, state)
    pending <- shiny::reactiveVal(NULL)
    # Depend on each field to update the indicator, but hash in isolation.
    fingerprint <- shiny::reactive({
      invisible(shiny::reactiveValuesToList(state))
      sieve_project_fingerprint(state)
    })
    shiny::observeEvent(input$name,
      {
        if (nzchar(trimws(input$name))) state$project_name <- trimws(input$name)
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(state$project_name,
      {
        shiny::updateTextInput(session, "name", value = state$project_name)
      },
      ignoreInit = FALSE
    )
    output$status <- shiny::renderUI({
      if (is.null(state$meta)) return(shiny::span(class = "small mb-3", "No project loaded"))
      status <- if (identical(
        (state$project_control %||% list())$acknowledged,
        fingerprint()
      )) "Saved copy acknowledged" else "Changed / download not confirmed"
      shiny::span(class = "small mb-3", role = "status", status)
    })
    output$prepared_copy <- shiny::renderUI({
      x <- (state$project_control %||% list())$prepared
      if (is.null(x)) return(NULL)
      shiny::div(
        class = "small", role = "status",
        shiny::span(paste("Prepared:", x$name, paste0("(", x$time, "). "))),
        shiny::actionButton(session$ns("acknowledge"),
          "I have kept this download",
          class = "btn-sm btn-outline-secondary"
        ),
        shiny::span(" Browser completion cannot be verified. Acknowledging this copy does not acknowledge later changes.")
      )
    })
    write_download <- function(file, name) {
      shiny::req(state$meta)
      sieve_prepare_project_copy(state, file, name)
    }
    output$save <- shiny::downloadHandler(
      filename = function() sieve_project_filename(state$project_name),
      content = function(file) write_download(file, shiny::isolate(state$project_name))
    )
    shiny::observeEvent(input$save_as, {
      shiny::showModal(shiny::modalDialog(
        title = "Save project as", easyClose = TRUE,
        shiny::textInput(session$ns("copy_name"), "Name for the separate copy", paste0(state$project_name, " copy")),
        shiny::downloadButton(session$ns("copy"), "Download named copy"),
        shiny::p(class = "small", "Keep the earlier download to preserve both versions."),
        footer = shiny::modalButton("Close")
      ))
    })
    output$copy <- shiny::downloadHandler(
      filename = function() sieve_project_filename(input$copy_name),
      content = function(file) {
        name <- trimws(input$copy_name %||% "")
        shiny::validate(
          shiny::need(nzchar(name), "Enter a name for the copy."),
          shiny::need(
            !identical(sieve_project_filename(name), sieve_project_filename(state$project_name)),
            "Use a different name to preserve the earlier copy."
          )
        )
        write_download(file, name)
      }
    )
    shiny::observeEvent(input$acknowledge, {
      sieve_acknowledge_project_copy(state)
    })
    project_open <- sieve_project_open_wire(input, output, session, state)
    pending <- project_open$pending
    invisible(NULL)
  })
}

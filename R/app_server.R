#' The application server logic
#'
#' Creates the single shared state store, wires the feature modules to it,
#' and handles the About dialog and the soft confirmation gate.
#'
#' @param input,output,session Internal Shiny parameters.
#' @return Invisibly `NULL`; called for its side effects.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny observeEvent showModal modalDialog modalButton showNotification
#' @importFrom shiny tags p strong em
app_server <- function(input, output, session) {
  state <- sieve_new_state()
  mod_project_server("project", state)
  sieve_workflow_wire(state, session)
  # The page on screen (fct_workspaces.R): the selected page of the selected
  # workspace.
  current_page <- shiny::reactive(sieve_active_page(input))
  # A page's navigation value given in place of a workspace (a bookmark or
  # a script) opens that page in its workspace.
  observeEvent(input$nav, {
    legacy <- input$nav
    if (length(legacy) == 1L && legacy %in% names(sieve_pages())) sieve_go(session, legacy)
  })
  output$page_workflow <- shiny::renderUI({
    sieve_page_status_ui(sieve_page_status(state, current_page()), state)
  })
  # The study goal of the Study workspace suggests a route and nothing
  # else: it computes nothing, changes nothing, and is kept for the session
  # only (it is not part of the project record, so a saved project is the
  # same whatever goal is chosen).
  study_goal <- shiny::reactiveVal("none")
  observeEvent(input$study_goal, {
    goal <- input$study_goal
    if (goal %in% unname(sieve_study_goals()$choices)) study_goal(goal)
  }, ignoreInit = TRUE)
  goal_pages <- c(develop = "retention", evaluate = "cfa", score = "scoring")
  goal_labels <- c(develop = "Open Dimensionality", evaluate = "Open CFA", score = "Open Scoring and Norms")
  output$study_route <- shiny::renderUI({
    goal <- study_goal()
    shiny::tagList(
      tags$span(class = "small text-muted", unname(sieve_study_goals()$routes[goal])),
      if (goal %in% names(goal_pages)) shiny::actionButton("study_route_go", goal_labels[[goal]],
        class = "btn-outline-primary btn-sm")
    )
  })
  observeEvent(input$study_route_go, {
    page <- goal_pages[study_goal()]
    if (length(page) == 1L && !is.na(page)) sieve_go(session, unname(page))
  })
  observeEvent(input$open_scoring, sieve_go(session, "scoring"))

  for (page in sieve_pages()) {
    if (!is.null(page$server)) get(page$server)(page$module, state)
  }
  mod_drop_server("drop", state)

  # No warranty and bug report affordance.
  observeEvent(input$show_about, {
    showModal(modalDialog(
      title = "About SIEVER",
      easyClose = TRUE,
      size = "l",
      tags$div(
        style = "display:flex;align-items:center;gap:1rem;",
        tags$img(
          src = "www/hex_SIEVER.svg", alt = "SIEVER package hex logo",
          width = "96", height = "101", style = "flex:none;object-fit:contain;"
        ),
        tags$div(
          p(strong("SIEVER"), sprintf(" version %s.", sieve_version_label())),
          p(
            "Scale Item Evaluation and Validation Environment in R (SIEVER), an environment for ",
            "theory-informed and evidence-supported psychometric decision making."
          )
        )
      ),
      # The versions this session is running on, and the citation.
      sieve_about_table(),
      p("Nothing is sent anywhere by default. SIEVER runs on your machine. Optional external review is available only in Scientific task assistant after you inspect and authorize its exact payload."),
      tags$hr(),
      sieve_workspaces_note(class = "small"),
      p(
        class = "small",
        strong("How to tell what SIEVER is doing. "),
        "A spinner over a panel, or the thin bar moving along the top of the page, ",
        "means it is computing. Nothing shown means it is waiting for you: a setting, ",
        "a Run, Fit or Update button. The guide above each page says what is needed and ",
        "whether an earlier result is current. An unchanged result is reused when ",
        "you return. Parallel ",
        "analysis and supported bootstraps can be stopped and keep completed work. ",
        "Red text identifies a failed calculation and explains what to correct."
      ),
      tags$details(open = NA,
        class = "small mb-2",
        tags$summary("When analyses run"),
        p(sieve_run_policy_text())
      ),
      # The core path and the optional tabs.
      tags$details(open = NA,
        class = "small mb-2",
        tags$summary("Choosing analyses"),
        sieve_tab_paths_note(class = "small")
      ),
      p(
        class = "small text-muted",
        strong("Research and test software, provided \"as is\", without warranty of any kind, "),
        "express or implied (including merchantability, fitness for a particular ",
        "purpose, or non-infringement). Use is at your own risk; results should be ",
        "independently verified and are not a substitute for professional judgment. ",
        "Not intended for high stakes decisions without independent validation. ",
        "Under active development."
      ),
      p(
        class = "small",
        strong("Report a bug: "),
        tags$a(
          href = "mailto:kkelley@nd.edu?subject=SIEVER%20bug%20report",
          "kkelley@nd.edu"
        )
      ),
      footer = modalButton("Close")
    ))
  })

  invisible(NULL)
}

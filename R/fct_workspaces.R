# fct_workspaces.R
# The workspaces group the pages by the question each workspace answers.
# A page's navigation value given in place of a workspace (a bookmark or a
# script) opens that page.

#' The workspaces and the pages each holds
#'
#' @return A list, one element per workspace, each with `label`, `question`
#'   (the question the workspace answers, shown at its top) and `pages` (the
#'   navigation values of its pages, in order).
#' @author Ken Kelley
#' @noRd
sieve_workspaces <- function() {
  registry <- sieve_pages()
  in_workspace <- function(id) {
    selected <- Filter(function(page) identical(page$workspace, id) && !identical(page$label, "Scoring and Norms"), registry)
    names(selected)[order(vapply(selected, function(page) page$workspace_order, numeric(1)))]
  }
  list(
    study = list(
      label = "Study",
      question = "What are the items and the data, and are they ready to analyze?",
      pages = in_workspace("study")
    ),
    structure = list(
      label = "Structure",
      question = "How many dimensions are there, and which items measure them?",
      pages = in_workspace("structure")
    ),
    scores = list(
      label = "Scores",
      question = "How is each score formed, and how precise is it?",
      pages = in_workspace("scores")
    ),
    evidence = list(
      label = "Evidence",
      question = "What do the scores relate to, and do they mean the same thing across groups and samples?",
      pages = in_workspace("evidence")
    ),
    report = list(
      label = "Report",
      question = "What was done, and how can it be reproduced?",
      pages = in_workspace("report")
    )
  )
}

#' Every page value, workspace by workspace
#'
#' @return A character vector of navigation values.
#' @author Ken Kelley
#' @noRd
sieve_workspace_pages <- function() {
  unlist(lapply(sieve_workspaces(), function(w) w$pages), use.names = FALSE)
}

#' The workspace that holds a page
#'
#' @param page A page's navigation value (for example `"cfa"`).
#' @return The workspace id, or `NA` for an unknown page.
#' @author Ken Kelley
#' @noRd
sieve_page_workspace <- function(page) {
  if (identical(page, "scoring")) page <- "reliab"
  ws <- sieve_workspaces()
  hit <- names(ws)[vapply(ws, function(w) page %in% w$pages, logical(1))]
  if (length(hit)) hit[1] else NA_character_
}

#' The navset that holds a page's link
#'
#' A page is a sub-tab of its workspace's navset (`"ws_<id>"`).
#' Used wherever a page's link is shown or hidden (`bslib::nav_show()`,
#' `bslib::nav_hide()`).
#'
#' @param page A page's navigation value.
#' @return The navset id.
#' @author Ken Kelley
#' @noRd
sieve_page_navset <- function(page) {
  if (identical(page, "scoring")) return("reliab-panels")
  ws <- sieve_page_workspace(page)
  if (is.na(ws)) "nav" else paste0("ws_", ws)
}

#' Open a page from anywhere
#'
#' Selects the workspace and its page, including the nested Scoring and
#' Norms panel when requested.
#'
#' @param session Any Shiny session (its root scope is used).
#' @param page A page's navigation value.
#' @return `NULL`, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_go <- function(session, page) {
  root <- tryCatch(session$rootScope(), error = function(e) session)
  scoring <- identical(page, "scoring")
  if (scoring) page <- "reliab"
  if (scoring) bslib::nav_select("reliab-panels", "scoring", session = root)
  ws <- sieve_page_workspace(page)
  if (is.na(ws)) {
    return(invisible(NULL))
  }
  bslib::nav_select("nav", ws, session = root)
  bslib::nav_select(paste0("ws_", ws), page, session = root)
  invisible(NULL)
}

#' The page on screen
#'
#' Returns the selected page of the selected workspace. A page value in
#' place of a workspace (from a bookmark or a script) is that page. Reading
#' it inside a reactive context takes a dependency on the inputs it reads,
#' so an observer that acts on the visible page runs again when that page
#' changes.
#'
#' @param input The root session's input (or any list with the same names).
#' @return The navigation value of the page shown.
#' @author Ken Kelley
#' @noRd
sieve_active_page <- function(input) {
  shown <- function(page) {
    if (identical(page, "reliab") && identical(input[["reliab-panels"]], "scoring")) "scoring" else page
  }
  nav <- input$nav
  if (identical(nav, "scoring")) return("scoring")
  ws <- nav %||% "study"
  ws_list <- sieve_workspaces()
  if (ws %in% sieve_workspace_pages()) {
    return(shown(ws))
  }
  if (!ws %in% names(ws_list)) {
    return("ingest")
  }
  shown(input[[paste0("ws_", ws)]] %||% ws_list[[ws]]$pages[1])
}

#' Where a page is found, in words
#'
#' Names the workspace and the page so a message tells the researcher
#' where to look.
#'
#' @param page A page's navigation value.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_page_path_text <- function(page) {
  if (identical(page, "scoring")) return(paste(sieve_page_path_text("reliab"), "\u2192 Scoring and Norms"))
  catalog <- sieve_page_catalog()
  label <- if (page %in% names(catalog)) catalog[[page]]$label else page
  ws <- sieve_page_workspace(page)
  if (is.na(ws)) return(label)
  paste(sieve_workspaces()[[ws]]$label, "\u2192", label)
}

#' The study goals and the route each suggests
#'
#' The researcher's answer to "what is this study for?" changes only the
#' route the Study workspace suggests; it computes nothing and changes no
#' data, model or setting, and it does not establish that a structure was
#' specified in advance. The goal is kept for the session only: it is not
#' part of the project record, so a saved project is the same whatever goal
#' is chosen.
#'
#' @return A list with `choices` (named character vector) and `routes`
#'   (one sentence per goal).
#' @author Ken Kelley
#' @noRd
sieve_study_goals <- function() {
  list(
    choices = c(
      "Not chosen yet" = "none",
      "Develop or revise an item pool" = "develop",
      "Evaluate a specified measurement model" = "evaluate",
      "Score an established instrument" = "score"
    ),
    routes = c(
      none = paste(
        "Choose a goal to see a suggested route; every page stays open whatever you choose."
      ),
      develop = paste(
        "Suggested route: Study (data, content, screening; reserve a holdout now if you will confirm",
        "the structure on data not used to select items), then Structure (Dimensionality, EFA, Items,",
        "then CFA on the holdout), then Scores and Evidence."
      ),
      evaluate = paste(
        "Suggested route: Study (data and screening), then Structure: CFA directly with the model you",
        "specified (Dimensionality and EFA are not required), then Scores and Evidence."
      ),
      score = paste(
        "Suggested route: Study (data, keying and screening), then Scores (Reliability, then Scoring",
        "and Norms); a CFA of the published structure in Structure supports the scores."
      )
    )
  )
}

#' The top of a workspace: its question, and on Study the goal choice
#'
#' @param id The workspace id.
#' @return A tag.
#' @author Ken Kelley
#' @noRd
sieve_workspace_header <- function(id) {
  ws <- sieve_workspaces()[[id]]
  shiny::div(
    class = "sieve-workspace-head d-flex flex-wrap align-items-center gap-3 px-3 pt-2 pb-1",
    shiny::tags$span(class = "sieve-workspace-question", ws$question),
    if (identical(id, "study")) {
      shiny::div(
        class = "d-flex align-items-center gap-2 sieve-study-goal",
        shiny::selectInput("study_goal", NULL,
          choices = sieve_study_goals()$choices,
          selected = "none", width = "300px"
        ),
        shiny::uiOutput("study_route", inline = TRUE)
      )
    },
    if (identical(id, "scores")) shiny::actionButton("open_scoring", "Open Scoring and Norms",
      class = "btn-outline-primary btn-sm")
  )
}

#' The workspace panels of the navbar
#'
#' One `nav_panel` per workspace, each holding its question (and on Study
#' the goal choice) above a `bslib::navset_underline` of its pages, every
#' page under its own value and label (`sieve_page_catalog()`), the
#' optional pages in the lighter style (`sieve_nav_label()`).
#'
#' @param pages A named list of the page bodies, by navigation value.
#' @return A list of `nav_panel`s, one per workspace.
#' @author Ken Kelley
#' @noRd
sieve_workspace_panels <- function(pages) {
  ws <- sieve_workspaces()
  catalog <- sieve_page_catalog()
  lapply(names(ws), function(id) {
    w <- ws[[id]]
    tabs <- lapply(w$pages, function(pg) {
      bslib::nav_panel(sieve_nav_label(catalog[[pg]]$label), value = pg, pages[[pg]])
    })
    bslib::nav_panel(
      w$label,
      value = id,
      sieve_workspace_header(id),
      do.call(bslib::navset_underline, c(list(id = paste0("ws_", id)), tabs))
    )
  })
}

#' The About Dialog's Description of the Page Groups
#'
#' @param class The paragraph's classes.
#' @return A [shiny::p()] tag.
#' @author Ken Kelley
#' @noRd
sieve_workspaces_note <- function(class = "small") {
  ws <- sieve_workspaces()
  groups <- vapply(names(ws), function(id) {
    sprintf("%s (%s)", ws[[id]]$label, sieve_tab_list_text(
      vapply(ws[[id]]$pages, function(pg) sieve_page_catalog()[[pg]]$label, character(1))
    ))
  }, character(1))
  shiny::p(
    class = class,
    shiny::strong("The arrangement of the pages. "),
    paste0(
      "The pages are grouped into five workspaces, each answering one question: ",
      paste(groups, collapse = "; "), ". ",
      "Scoring and Norms is a Reliability sub-tab, also available directly from the Scores heading."
    )
  )
}

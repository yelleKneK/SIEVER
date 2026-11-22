# Launcher for the header test: the SIEVER page shell with the navbar-bg Sass
# variable withheld from the theme.
#
# sieve_theme() hands the header color to bslib as navbar-bg, and the
# stylesheet states the header outright as well, so the header keeps its
# color, and its white links stay legible, whether or not a Bootstrap 5
# theme applies that variable. This app is how a test shows the
# stylesheet is sufficient: the theme here says nothing about the header, so
# whatever color the header has comes from inst/app/www/custom.css alone.
#
# SIEVER is loaded exactly as apps/siever/app.R loads it (the source tree this
# file sits in when there is one, otherwise the installed package).
#
# Author: Ken Kelley

sieve_launcher_source_root <- function(start = getwd()) {
  root <- normalizePath(file.path(start, "..", "..", "..", ".."), mustWork = FALSE)
  desc <- file.path(root, "DESCRIPTION")
  if (!file.exists(desc) || !dir.exists(file.path(root, "R"))) {
    return(NULL)
  }
  fields <- readLines(desc, warn = FALSE)
  if (!any(grepl("^Package:[[:space:]]*SIEVER[[:space:]]*$", fields))) {
    return(NULL)
  }
  root
}

root <- sieve_launcher_source_root()
if (!is.null(root)) {
  pkgload::load_all(root, quiet = TRUE, helpers = FALSE, export_all = FALSE)
} else if (requireNamespace("SIEVER", quietly = TRUE)) {
  library(SIEVER)
} else {
  stop("Cannot find the SIEVER package: it is neither installed nor above ", getwd())
}

ui <- function(request) {
  shiny::tagList(
    SIEVER:::golem_add_external_resources(),
    bslib::page_navbar(
      title = shiny::tags$span(
        shiny::tags$strong("SIEVER"),
        shiny::tags$small(
          class = "text-light ms-2 opacity-75",
          "Scale Item Evaluation and Validation Environment"
        )
      ),
      theme = SIEVER:::sieve_theme(navbar_bg = NULL),
      id = "nav",
      bslib::nav_panel("Ingest", value = "ingest", shiny::p("First panel.")),
      bslib::nav_panel("Screening", value = "screen", shiny::p("Second panel.")),
      bslib::nav_spacer(),
      bslib::nav_item(shiny::tags$a(href = "#", class = "nav-link", "About and Disclaimer"))
    )
  )
}

source(file.path("..", "protocol.R"), local = TRUE)
siever_browser_protocol(shiny::shinyApp(ui, function(input, output, session) NULL))

#' The application user interface
#'
#' Builds the SIEVER navigation shell: a Bootstrap 5 navbar (themed by
#' `sieve_theme()`) whose tabs are five workspaces, Study, Structure,
#' Scores, Evidence and Report, each holding its pages as sub-tabs in the
#' order of `sieve_workspaces()` (fct_workspaces.R). The core path
#' (`sieve_tab_paths()`) is Ingest to CV and Invariance; the optional pages
#' are labeled in a lighter style wherever they sit (`sieve_nav_label()`)
#' and each opens with a sentence saying it is optional. Each page keeps its
#' module and navigation value.
#'
#' @param request Internal Shiny parameter for bookmarking state.
#' @return A [shiny::tagList()] holding the full page.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny tagList tags
#' @importFrom bslib page_navbar nav_panel nav_spacer nav_item
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    do.call(page_navbar, c(
      list(
        title = tags$span(
          tags$img(
            src = "www/hex_SIEVER.svg", alt = "", class = "sieve-brand-hex",
            width = "32", height = "34"
          ),
          tags$strong("SIEVER"),
          tags$small(
            class = "text-light ms-2 opacity-75",
            "Scale Item Evaluation and Validation Environment"
          )
        ),
        theme = sieve_theme(),
        id = "nav",
        fillable = TRUE,
        header = tagList(
          # Visible work: a spinner over a panel that is recalculating and a
          # pulse bar along the top of the page while the session is busy;
          # nothing shown means the application is waiting for the researcher.
          # The About dialog says what the indicators mean.
          shiny::useBusyIndicators(spinners = TRUE, pulse = TRUE, fade = TRUE),
          shiny::busyIndicatorOptions(
            spinner_type = "bars3",
            spinner_color = sieve_palette_colors("nd", 1L),
            spinner_delay = "150ms",
            pulse_background = sprintf(
              "linear-gradient(90deg, %s, %s)",
              sieve_palette_colors("nd", 2L)[1], sieve_palette_colors("nd", 2L)[2]
            ),
            pulse_height = "4px",
            fade_opacity = 0.6
          ),
          # NULL (nothing shown) unless an optional package is missing.
          sieve_missing_packages_notice(),
          mod_project_ui("project"),
          shiny::uiOutput("page_workflow")
        )
      ),
      # Pages grouped by their research question (fct_workspaces.R).
      sieve_workspace_panels(sieve_page_bodies()),
      list(
        nav_spacer(),
        # Drop or restore items from any tab (mod_drop.R), and the About
        # dialog: one item, so the navbar keeps two rows at 1,280 px.
        nav_item(
          tags$div(
            class = "d-flex flex-wrap align-items-center",
            mod_drop_link_ui("drop"),
            tags$a(
              href = "#",
              onclick = "Shiny.setInputValue('show_about', Math.random()); return false;",
              class = "nav-link",
              "About and Disclaimer"
            )
          )
        )
      )
    ))
  )
}

#' The page bodies, by navigation value
#'
#' Every page's module interface, named by the page's value, for the
#' workspaces to place (`sieve_workspace_panels()`). The Scoring and Norms
#' page has no body here: its card, `mod_finalize_scoring_ui()`, is a
#' sub-tab of Reliability, under the module id `"finalize"`, so its server
#' is the one `mod_finalize_server()` runs for CV and Invariance.
#'
#' @return A named list of tags.
#' @author Ken Kelley
#' @noRd
sieve_page_bodies <- function() {
  lapply(sieve_pages()[setdiff(names(sieve_pages()), "scoring")], function(page) get(page$ui)(page$module))
}

#' Add external resources to the application
#'
#' Registers the package's `inst/app/www` directory and the favicon so
#' bundled static assets are served by Shiny.
#'
#' @return A [shiny::tags] head fragment.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny tags
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(
    favicon(ico = "hex_SIEVER", rel = "icon", ext = "png"),
    bundle_resources(path = app_sys("app/www"), app_title = "SIEVER")
  )
}

# mod_efa.R
# Exploratory factor analysis with SPSS-parity output. The engine is DMAR:
# the selected solution is DMAR::fa_exploratory(), which fits the common-factor model
# through lavaan with rotation-aware standard errors (direct oblimin as the
# default oblique rotation per the house style), and the number of factors
# panel is DMAR::factor_count(). The page keeps its own layout: the
# pattern matrix (standardized regression coefficients of items
# on the correlated factors), the structure matrix (item-factor
# correlations), the factor correlation matrix Phi, communalities and
# complexity, and a variance-accounted table, filled with DMAR's values and
# with DMAR's intervals beside them, and each factor's determinacy and H. A
# loading-suppression cutoff and optional dominant-factor sorting
# reproduce the familiar SPSS layout. A solution estimated above two
# seconds, and every factor count comparison, runs in a background R
# process (fct_efa_jobs.R) with its elapsed time, Cancel, and a result
# accepted only for the request it was started for.

#' EFA module UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput numericInput selectInput checkboxInput helpText radioButtons
#' @importFrom shiny conditionalPanel div
#' @importFrom bslib layout_sidebar sidebar card card_header navset_card_tab nav_panel
#' @importFrom DT DTOutput
mod_efa_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    # The main area is a document that scrolls: in a fillable column the
    # explanation below the tables would take its height from the table
    # card, which would then show four rows of the pattern matrix at 1440
    # by 900.
    fillable = FALSE,
    sidebar = sidebar(
      open = "desktop",
      width = 300,
      title = "Estimation and rotation",
      sieve_settings("Estimation",
        ids = ns(c("fm", "estimator", "cor_type")),
        # The method first, then what each method reads: the estimator of
        # the item responses, or the correlation matrix of the matrix route.
        selectInput(ns("fm"), "Method", choices = sieve_efa_method_choices(), selected = "ml"),
        helpText(paste0(
          "DMAR::fa_exploratory fits the common-factor model through lavaan, with confidence ",
          "intervals for the rotated solution; every method is a common factor method, none is ",
          "principal components."
        )),
        uiOutput(ns("method_note")),
        sieve_estimator_input(ns("estimator"), "Estimator"),
        helpText(paste0(
          "For the first method: ML or MLR on the item responses (with FIML or the complete ",
          "cases, as the study's setting says), or WLSMV when the items are ordered."
        )),
        uiOutput(ns("estimator_source")),
        selectInput(ns("cor_type"), "Correlations (minres on a matrix)", choices = sieve_basis_choices(), selected = "auto"),
        helpText(paste0(
          "The matrix minres is fit to under the third method (polychoric for ordered items); ",
          "a supplied matrix carries no standard errors, so its loadings have no intervals."
        )),
        uiOutput(ns("cor_source"))
      ),
      tags$hr(),
      numericInput(ns("n_factors"), "Number of factors", value = 3L, min = 1L, step = 1L),
      uiOutput(ns("decision_note")),
      selectInput(ns("rotation"), "Rotation", choices = sieve_efa_rotation_choices(), selected = "oblimin"),
      helpText(paste0(
        "DMAR's rotation criteria, each from 30 random starts with rotation-aware standard errors. ",
        "Promax and equamax are not offered, since DMAR::fa_exploratory() does not offer them; a saved study that chose ",
        "one opens with direct oblimin (promax) or varimax (equamax). Target rotates toward each construct's items, as set on Ingest."
      )),
      uiOutput(ns("rotation_note")),
      helpText("Compare numbers of factors in the Number of factors panel."),
      numericInput(ns("suppress"), "Hide loadings below",
        value = 0.30,
        min = 0, max = 0.6, step = 0.05
      ),
      checkboxInput(ns("sort_loadings"), "Sort items by dominant factor (simple structure)", value = TRUE),
      checkboxInput(ns("show_ci"), "Show the 95% confidence intervals beside the coefficients", value = TRUE),
      helpText("These display settings change the tables without refitting the model.")
    ),
    # Results first: one status line with the page's one action, the one-line
    # method statement and the fit line, the display filters and the download
    # in one compact row, then the loading tables. What the analysis is and
    # its code follow the tables. One block, so the main area's spacing is not
    # repeated for each line that is empty.
    div(
      class = "sieve-efa-top",
      tags$style(HTML(paste0(
        ".sieve-efa-display .form-group { display: flex; align-items: center; gap: 0.4rem; margin-bottom: 0; }",
        ".sieve-efa-display .form-group label { margin-bottom: 0; white-space: nowrap; }",
        ".sieve-efa-display .form-group > div { min-width: 11rem; }"
      ))),
      uiOutput(ns("status_line")),
      uiOutput(ns("efa_method")),
      uiOutput(ns("efa_fit_line")),
      div(
        class = "d-flex flex-wrap align-items-center gap-3 mb-1 small sieve-efa-display",
        shiny::selectInput(ns("display_construct"), "Show construct (display only)",
          choices = c("All constructs" = ""), width = "auto"
        ),
        shiny::selectInput(ns("display_facet"), "Show facet", choices = c("All facets" = ""), width = "auto"),
        shiny::downloadButton(ns("dl_numeric"), "Download numeric EFA tables (.tar.gz)", class = "btn-sm")
      ),
      uiOutput(ns("display_pool_note"))
    ),
    navset_card_tab(
      nav_panel(
        "Pattern matrix",
        card(
          card_header("Pattern matrix (standardized regression coefficients of items on the correlated factors)"),
          sieve_item_decision_controls(ns, "pattern_decide"),
          DTOutput(ns("pattern_tbl")),
          uiOutput(ns("pattern_ci_note")),
          uiOutput(ns("factor_order_pattern")),
          sieve_more_below(
            "Read the pattern coefficients for each factor's contribution after accounting for the other factors.",
            p(
              "The ", strong("pattern matrix"), " holds each item's standardized regression weight on a ",
              "factor, holding the other (correlated) factors constant: the unique contribution of that ",
              "factor to the item. These are the coefficients you read for simple structure. With an ",
              "orthogonal rotation the pattern matrix, the structure matrix, and the factor loading ",
              "matrix all coincide. Beside each coefficient is its Wald interval from DMAR, which ",
              "describes the selected rotated solution at the selected number of factors; it does not ",
              "include the uncertainty of choosing that number or that rotation."
            ),
            label = "Pattern coefficients and factor loadings"
          )
        )
      ),
      nav_panel(
        "Structure matrix",
        card(
          card_header("Structure matrix (item-factor correlations)"),
          sieve_item_decision_controls(ns, "structure_decide"),
          DTOutput(ns("structure_tbl")),
          uiOutput(ns("factor_order_structure")),
          sieve_more_below(
            "Structure coefficients are the correlations between items and factors.",
            p(
              "The ", strong("structure matrix"), " holds the simple (zero-order) correlation between each ",
              "item and each factor, not partialling the other factors. Because the factors are ",
              "correlated under an oblique rotation, the structure matrix equals the pattern matrix ",
              "multiplied by Phi, which is how it is formed here from DMAR's standardized estimates; ",
              "DMAR reports no interval for it. Individual structure coefficients can be larger or ",
              "smaller than the corresponding pattern coefficients, depending on the signs and ",
              "correlations. The ", strong("factor loading matrix"),
              " is the general term for these item-on-factor values; it equals the pattern matrix for an ",
              "oblique solution and equals both pattern and structure for an orthogonal one."
            ),
            label = "How the structure and pattern matrices differ"
          )
        )
      ),
      nav_panel(
        "Factor plot",
        card(
          fill = FALSE, full_screen = TRUE,
          card_header("Factor plot of the selected EFA solution"),
          bslib::card_body(fillable = FALSE,
            p(class = "text-muted small",
              "Each point shows an item's fitted coefficients. Hover to identify items; expand this card for a larger view."
            ),
            tags$details(id = ns("factor_plot_controls"), class = "mb-2",
              tags$summary("Change factors, coefficients and labels"),
              div(class = "d-flex flex-wrap gap-3",
                selectInput(ns("plot_matrix"), "Coefficients",
                  choices = c("Pattern (regression coefficients)" = "pattern",
                    "Structure (item-factor correlations)" = "structure"), selected = "pattern"),
                uiOutput(ns("plot_axes")),
                checkboxInput(ns("plot_labels"), "Label item identifiers", value = TRUE)
              ),
              p(class = "small text-muted", "These display choices reuse the fitted solution without running another EFA.")
            ),
            uiOutput(ns("factor_plot_note")),
            sieve_plot_save_ui(ns("factor_plot")),
            shiny::plotOutput(ns("factor_plot"), height = "auto",
              hover = shiny::hoverOpts(ns("factor_plot_hover"), delay = 100, delayType = "debounce")),
            uiOutput(ns("factor_plot_hover_note")),
            uiOutput(ns("factor_plot_code"))
          )
        )
      ),
      nav_panel(
        "Factor correlations (Phi)",
        card(
          card_header("Factor correlation matrix Phi"),
          DTOutput(ns("phi_tbl")),
          uiOutput(ns("phi_ci_head")),
          DTOutput(ns("phi_ci_tbl")),
          uiOutput(ns("factor_order_phi"))
        )
      ),
      nav_panel(
        "Communalities and Simple Structure",
        card(
          card_header("Communalities, uniqueness, and complexity"),
          DTOutput(ns("comm_tbl")),
          uiOutput(ns("comm_note"))
        ),
        card(
          card_header("Simple structure (Thurstone): how cleanly items map to factors"),
          DTOutput(ns("ss_summary")),
          DTOutput(ns("ss_items")),
          uiOutput(ns("ss_note")),
          uiOutput(ns("ss_code"))
        )
      ),
      nav_panel(
        "Variance accounted",
        card(
          card_header("Variance accounted for per factor"),
          DTOutput(ns("vacc_tbl")),
          uiOutput(ns("vacc_note"))
        )
      ),
      nav_panel(
        "Determinacy and H",
        card(
          card_header("Factor determinacy, the minimum correlation of equally consistent scores, and coefficient H"),
          DTOutput(ns("quality_tbl")),
          uiOutput(ns("quality_note"))
        )
      ),
      nav_panel(
        "Number of factors",
        card(
          card_header("Number of factors: fit, information criteria, reduced eigenvalues with the Dimensionality page's parallel analysis, and MAP (DMAR::factor_count)"),
          div(
            class = "d-flex flex-wrap align-items-end gap-3 mb-2 small",
            numericInput(ns("count_to"), "Largest number of factors", value = 8L, min = 1L, step = 1L, width = "12rem"),
            selectInput(ns("count_criterion"), "Information criterion of the weights",
              choices = c("BIC" = "BIC", "Sample-size adjusted BIC (SABIC)" = "SABIC", "HBIC" = "HBIC", "AIC" = "AIC"),
              selected = "BIC", width = "16rem"
            )
          ),
          uiOutput(ns("count_note")),
          DTOutput(ns("count_tbl")),
          uiOutput(ns("count_lines")),
          uiOutput(ns("count_code"))
        )
      )
    ),
    # Below the result: what the analysis is (its first paragraph visible,
    # the rest under a closed label) and the code that produced it.
    div(
      class = "mt-2",
      uiOutput(ns("efa_overview")),
      uiOutput(ns("efa_code"))
    )
  )
}

#' EFA module server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive req renderUI div observeEvent updateNumericInput
#' @importFrom shiny reactiveVal withProgress actionButton strong updateRadioButtons isolate observe
#' @importFrom stats complete.cases
#' @importFrom DT renderDT datatable formatRound formatStyle styleEqual
mod_efa_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    sieve_code_navigation(input, session)
    # Seed the factor count from the stated theory once it is available.
    observeEvent(state$theory$k,
      {
        k <- state$theory$k
        if (is.null(isolate(state$efa_settings)$n_factors) && !is.null(k) && is.finite(k) && k >= 1L) {
          updateNumericInput(session, "n_factors", value = as.integer(k))
        }
      },
      once = TRUE,
      ignoreNULL = TRUE
    )

    # A recorded retention decision proposes the number of factors here:
    # when a new decision is recorded for these data on
    # the Dimensionality tab, the control is set to its count once, as a
    # proposal the researcher can change; nothing is fit until Run selected
    # EFA is pressed.
    decision_seen <- reactiveVal(NULL)
    retention_decision <- reactive({
      state$decisions
      sieve_current_decision(state, "factor_retention", state$dataset_id %||% NA_character_)
    })
    observe({
      d <- retention_decision()
      if (is.null(d) || identical(d$id, isolate(decision_seen()))) return()
      decision_seen(d$id)
      k <- suppressWarnings(as.integer(d$choice))
      if (length(k) == 1L && is.finite(k) && k >= 1L) {
        updateNumericInput(session, "n_factors", value = k)
      }
    })
    output$decision_note <- renderUI({
      d <- retention_decision()
      if (is.null(d)) return(NULL)
      k <- suppressWarnings(as.integer(d$choice))
      shiny::p(
        class = "small text-muted",
        sprintf(
          paste0(
            "Proposed by the decision recorded on the Dimensionality tab (%s: %d factor%s). ",
            "Change the number here to fit another; the decision record is not changed."
          ),
          d$id, k, if (identical(k, 1L)) "" else "s"
        )
      )
    })

    # The confirmed types of the included items, in ledger order.
    item_types <- reactive({
      req(state$meta)
      m <- state$meta[state$meta$status == "active", , drop = FALSE]
      stats::setNames(as.character(m$type), m$item)
    })

    # The Correlations control names what the study's setting resolves to,
    # and the lines under the controls say what is in force and where it
    # was set.
    observe({
      label <- sieve_study_basis_label(state, unname(item_types()))
      shiny::updateSelectInput(session, "cor_type",
        choices = sieve_basis_choices(label),
        selected = efa_binding$pending()$cor_type %||% isolate(input$cor_type) %||% "auto"
      )
    })
    output$cor_source <- renderUI({
      req(state$meta)
      div(
        class = "small text-muted mb-2",
        sieve_basis_source_text(input$cor_type %||% "auto", state, unname(item_types())),
        if (!identical(method(), "minres_matrix")) " Not used by the method in force."
      )
    })
    estimator_choice <- sieve_estimator_wire(input, output, state,
      output_id = "estimator_source", detail = function() {
      req(state$meta)
      if (identical(method(), "ml")) sprintf(" For these items: %s.", route()$label) else " Not used by the method in force."
    })

    # The estimator the item responses are fit with: the panel's choice,
    # or the study's.
    fit_spec <- reactive({
      req(state$meta)
      sieve_lavaan_spec(state$meta, estimator = sieve_estimator_effective(input$estimator %||% "study", state))
    })
    # Saved settings that have no DMAR route (principal axis, weighted and
    # generalized least squares extraction; promax and equamax rotation,
    # which open as direct oblimin and varimax,
    # sieve_efa_retired_rotation()) are named once, here.
    retired <- reactiveVal(list())
    method <- reactive({
      fm <- input$fm %||% "ml"
      if (fm %in% sieve_efa_method_choices()) fm else "ml"
    })
    rotation <- reactive({
      rot <- input$rotation %||% "oblimin"
      if (rot %in% sieve_efa_rotation_choices()) rot else "oblimin"
    })
    # DMAR's estimator, missing data method and ordered items for the
    # method in force.
    route <- reactive({
      sieve_efa_dmar_route(method(), fit_spec())
    })
    output$method_note <- renderUI({
      notes <- c(
        retired()$fm,
        if (identical(method(), "minres") && any(item_types() %in% c("ordered", "binary"))) paste(
          "Minres on the item responses reads them as continuous (Pearson correlations of the category",
          "codes), which attenuates the loadings of ordered items; minres on the polychoric matrix or WLSMV",
          "reads them as ordered."
        )
      )
      if (!length(notes)) return(NULL)
      div(class = "small text-muted mb-2", role = "status", paste(notes, collapse = " "))
    })
    output$rotation_note <- renderUI({
      notes <- c(
        retired()$rotation,
        if (identical(rotation(), "target")) {
          tg <- tryCatch(sieve_efa_target(state$meta, colnames(active_x())), error = function(e) NULL)
          if (is.null(tg)) {
            "Target rotation needs constructs: assign the included items to constructs on Ingest."
          } else {
            sprintf(
              "Target: %d factor%s, one per construct (%s); each item targeted at zero off its own construct. The number of factors must be %d.",
              length(tg), if (length(tg) == 1L) "" else "s", paste(names(tg), collapse = ", "), length(tg)
            )
          }
        }
      )
      if (!length(notes)) return(NULL)
      div(class = "small text-muted mb-2", role = "status", paste(notes, collapse = " "))
    })

    # Keep the requested count intact. Unsupported requests are explained, never capped.
    k_asked <- reactive({
      input$n_factors %||% 3L
    })

    # The number of factors comparison runs from 1 to the largest count
    # whose model keeps a degree of freedom, at most 8, by default; the
    # control is set again when the number of included items changes.
    observeEvent(ncol(active_x()), {
      p <- ncol(active_x())
      saved_to <- isolate(state$efa_settings)$count_to
      updateNumericInput(session, "count_to", value = saved_to %||% sieve_efa_count_default(p),
        max = max(1L, sieve_efa_max_factors(p)))
    })

    # The active item matrix, read as numbers once. A column that cannot be
    # read (text entries, no observed values, one value throughout) is named
    # in the "problems" attribute (see sieve_numeric_columns()) and refused by
    # the analyses below; it is never coerced to missing values quietly.
    active_x <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      meta <- state$meta
      active_items <- meta$item[meta$status == "active"]
      read <- sieve_numeric_columns(state$items, intersect(active_items, names(state$items)))
      d <- read$data
      attr(d, "problems") <- read$text
      d
    })
    # The cases DMAR analyzes: under FIML every row with an observed item,
    # otherwise the complete cases.
    cases_of <- function(d, rt) {
      if (identical(rt$form, "data") && identical(rt$missing, "fiml")) {
        sum(rowSums(!is.na(d)) > 0L)
      } else {
        sum(stats::complete.cases(d))
      }
    }

    # Opening a snapshot restores specifications and evidence, not an implicit
    # request to fit. Authorization is bound to that load and its exact settings.
    primary_authorized <- reactiveVal(NULL)
    # The request whose solution was updated by itself after an item was
    # excluded or included (sieve_inclusion_wire()): the method line says so
    # while that solution is the one on screen; a press of Run or Update,
    # or a solution for other inputs, leaves it unsaid.
    refit_after_item <- reactiveVal(NULL)
    primary_kept <- new.env(parent = emptyenv())
    primary_kept$key <- NULL
    primary_kept$value <- NULL
    primary_kept$attempt_key <- NULL
    primary_kept$attempt_status <- NULL
    primary_kept$attempt_value <- NULL
    primary_signal <- reactiveVal(0L)
    keep_primary <- function(value, key) {
      primary_kept$attempt_key <- key
      primary_kept$attempt_status <- value$status
      primary_kept$attempt_value <- value
      if (!is.null(value$fit)) {
        primary_kept$key <- key
        primary_kept$value <- value
        # Shared with the Items tab, which names the difference between
        # this model and its own (sieve_efa_store()).
        sieve_efa_store_keep(state, key, value)
      }
      primary_signal(isolate(primary_signal()) + 1L)
      value
    }
    primary_rng <- new.env(parent = emptyenv()) # one seed per request (sieve_keep_each)
    # The selected solution's background process (above two seconds), and
    # the tick that wakes the page when it finishes.
    primary_job <- sieve_efa_dmar_job()
    primary_tick <- reactiveVal(0L)
    # The identity of the request: what DMAR reads (the included items,
    # their values and types, the respondents; the correlations asked for
    # under the matrix route; the constructs under a target rotation) and
    # the settings. A construct or facet edit is otherwise not part of it,
    # so it neither withholds the solution nor draws a new seed.
    primary_key <- reactive({
      pool <- sieve_pool_revision(state)
      rt <- route()
      rot <- rotation()
      matrix_route <- identical(rt$form, "matrix")
      list(
        dataset_id = pool$dataset, data = pool$data, items = pool$items, types = pool$types,
        reverse = pool$reverse, rows = pool$rows, k = k_asked(),
        fm = method(), rotation = rot,
        estimator = rt$estimator, missing = rt$missing, ordered = rt$ordered,
        cor_type = if (matrix_route) input$cor_type %||% "auto",
        study_basis = if (matrix_route) sieve_study_basis_request(state),
        target = if (identical(rot, "target")) sieve_efa_target(state$meta, pool$items)
      )
    })
    # The estimated wait of the selected solution: the shared correlation
    # matrix when the matrix route needs it and it is not kept yet, and
    # DMAR::fa_exploratory itself (sieve_efa_dmar_wait()).
    primary_wait <- function() {
      d <- active_x()
      p <- ncol(d)
      rt <- route()
      k <- suppressWarnings(as.numeric(k_asked()))
      if (!length(k) || !is.finite(k)) k <- 1
      basis_wait <- 0
      if (identical(rt$form, "matrix")) {
        choice <- input$cor_type %||% "auto"
        if (choice %in% c("auto", "study")) choice <- sieve_study_basis_request(state)
        resolved <- if (choice %in% names(sieve_retention_basis_labels())) choice else {
          sieve_retention_request(choice, unname(item_types()))
        }
        held <- isolate(state$cache)$basis[[resolved]]
        pool <- sieve_state_items(state)
        basis_wait <- if (!is.null(held) && identical(held$key$data, pool$key)) 0 else sieve_basis_wait(p, resolved)
      }
      basis_wait + primary_fit_wait()
    }
    # The DMAR fit alone (the matrix route's correlations are computed in
    # the session, under their own progress message, before it): what
    # decides whether the fit runs here or in a background process.
    primary_fit_wait <- function() {
      d <- active_x()
      rt <- route()
      k <- suppressWarnings(as.numeric(k_asked()))
      if (!length(k) || !is.finite(k)) k <- 1
      sieve_efa_dmar_wait(ncol(d), rt$estimator, cases_of(d, rt), k, rt$form,
        patterns = sieve_efa_patterns(d, rt))
    }
    primary_seconds <- reactive(primary_wait())
    # The run policy decides at the standard rates (sieve_at_default_rates()).
    primary_standard <- reactive(sieve_at_default_rates(primary_wait()))
    primary_allowed <- reactive({
      restored <- sieve_restored_project(state)
      visible <- identical(sieve_active_page(session$rootScope()$input), "efa")
      key <- primary_key()
      current <- identical(primary_kept$key, key) && !is.null(primary_kept$value$fit)
      first_for_dataset <- is.null(primary_kept$key) ||
        !identical(primary_kept$key$dataset_id, key$dataset_id)
      # The run policy (sieve_runs_by_itself()): the first view of a new
      # dataset fits by itself when the estimate is two seconds or less.
      # Later input changes require the explicit action; a tab visit is not
      # a new request. A request the user stopped waits for its button.
      current || identical(primary_authorized(), key) ||
        (!restored && !identical(primary_job$stopped, key) && sieve_runs_by_itself(
          tryCatch(primary_seconds(), error = function(e) NA_real_),
          first = first_for_dataset, visible = visible,
          standard = tryCatch(primary_standard(), error = function(e) NA_real_)
        ))
    })
    observeEvent(input$efa_go, {
      refit_after_item(NULL)
      # A press asks again: a failed attempt or a stopped run of the same
      # request is tried afresh.
      primary_kept$attempt_value <- NULL
      primary_job$stopped <- NULL
      primary_authorized(primary_key())
      primary_tick(isolate(primary_tick()) + 1L)
    })

    # Auxiliary variables declared for FIML on Ingest, which
    # DMAR::fa_exploratory() and DMAR::factor_count() do not take: on the
    # FIML route of the item responses they are left out, and the method
    # line, the count panel and the recorded R say so
    # (sieve_efa_declared_aux()). The matrix route's saturated
    # FIML matrix includes them. With every item observed in every row they
    # would add no information to any fit (sieve_auxiliary_in_force()), so
    # nothing is said about them.
    aux_ignored <- function(rt, d) {
      if (!identical(rt$form, "data") || !identical(rt$missing, "fiml")) return(character(0))
      if (!sieve_auxiliary_in_force(colnames(d), d)) return(character(0))
      sieve_efa_declared_aux(state, colnames(d))
    }

    # The words of the data a request reads, for the method line and the
    # recorded R.
    data_words <- function(rt, b, n) {
      if (identical(rt$form, "matrix")) {
        sprintf("%s correlations, %s", sieve_retention_basis_label(b$used), sieve_basis_n_text(b))
      } else if (identical(rt$missing, "fiml")) {
        sprintf("FIML on %d respondents (every row with an observed item)", n)
      } else {
        sprintf("%d complete cases", n)
      }
    }

    # Accept a finished DMAR call for its request: the solution, its
    # status (a refusal read by its class, with DMAR's reason), the
    # recorded R and the code view.
    accept_primary <- function(record, request, job) {
      status <- sieve_efa_dmar_status(record)
      solution <- if (identical(status$code, "ok")) sieve_efa_dmar_solution(record$value) else NULL
      rt <- job$route
      d <- job$data
      b <- job$basis
      n_used <- if (!is.null(solution)) as.integer(solution$method$N) else job$n
      code_view <- NULL
      if (!is.null(solution)) {
        if (is.finite(record$seconds %||% NA_real_)) {
          sieve_efa_dmar_time_note(rt$estimator, ncol(d), job$n, job$k, rt$form, record$seconds,
            patterns = sieve_efa_patterns(d, rt))
        }
        rot <- job$rotation
        aux_line <- sieve_efa_aux_sentence(job$aux, "fa_exploratory")
        sieve_log(
          state,
          sprintf(
            paste0(
              "EFA: %d factor(s) by DMAR::fa_exploratory, %s, %s rotation, on %d active items (%s); ",
              "%s intervals%s; rotation starts from seed %d.%s"
            ),
            job$k, rt$label, sieve_efa_rotation_words(rot), ncol(d), data_words(rt, b, n_used),
            sieve_ci_percent(job$call$args$conf_level),
            if (sieve_efa_has_rmsea(rt)) sprintf(" (the RMSEA's at %s)", sieve_ci_percent(sieve_efa_call_rmsea_level(job$call))) else "",
            job$rng$seed, if (length(aux_line)) paste0(" ", aux_line) else ""
          ),
          c(
            if (identical(rt$form, "matrix")) c(
              sprintf("# Correlations: %s", sieve_basis_source_text(job$cor_choice, state, unname(item_types()))),
              sieve_retention_cor_code(b)
            ),
            "# Exploratory factor analysis by DMAR::fa_exploratory(): the common-factor model",
            "# (not principal components), fit through lavaan, with Wald intervals for the",
            "# rotated solution from rotation-aware standard errors.",
            sprintf("# Estimation: %s.", rt$label),
            if (length(aux_line)) paste("#", strwrap(aux_line, width = 88)),
            sprintf("# Rotation: %s, from 30 random starts.", sieve_efa_rotation_words(rot)),
            if (rot %in% c("oblimin", "geomin", "quartimin", "target") && job$k > 1L) {
              "# Oblique: the factors are allowed to correlate (Thurstone's argument for real constructs)."
            },
            sprintf("# Rotation starts: seed = %d%s; DMAR sets it locally and restores the session's stream.",
              job$rng$seed, if (isTRUE(job$rng$supplied)) "" else " (no seed was supplied; SIEVER drew this one so the run can be replayed)"),
            "# The generator is fixed to the one the app used, since DMAR's seed seeds the generator in force.",
            sieve_efa_dmar_code(job$call, rng_kind = job$rng$kind),
            "efa_fit                               # the solution with its intervals, as DMAR prints it",
            "attr(efa_fit, \"variance_accounted\")   # variance each factor accounts for",
            "attr(efa_fit, \"factor_quality\")       # determinacy, minimum score correlation and H",
            sprintf(
              "attr(efa_fit, \"fit_measures\")         # the fit, from the unrotated solution; the RMSEA interval at %s (%s)",
              if (!is.null(job$call$args$rmsea_conf_level)) "rmsea_conf_level" else "conf_level",
              sieve_ci_percent(sieve_efa_call_rmsea_level(job$call))
            ),
            utils::head(factor_readout_code(), -3L)
          )
        )
        code_view <- sieve_code_snapshot(state, context = c(
          sprintf("Displayed loadings and Phi: DMAR::fa_exploratory (DMAR %s); %d factors; %s; %s rotation.",
            as.character(utils::packageVersion("DMAR")), job$k, rt$label, sieve_efa_rotation_words(rot)),
          sprintf("Analyzed: %d items; %s.", ncol(d), data_words(rt, b, n_used)),
          "The fitted pool includes every active item, across constructs. Display filters do not refit the model.",
          paste("Fitted items, in input order:", paste(solution$items, collapse = ", ")),
          "The pattern matrix contains standardized item-on-factor coefficients; the structure matrix is pattern times Phi.",
          paste("Factor columns, in this fit's actual order:", paste(solution$factors, collapse = ", ")),
          "Factor names identify columns within this fit. Another program can permute factors or reverse their signs; match whole loading columns and use that same mapping on both axes of Phi.",
          sprintf(
            "The fit line reads DMAR's fit_measures attribute, computed from the unrotated solution of the same model; its RMSEA interval is at the call's rmsea_conf_level (%s), the RMSEA level; the coefficients' intervals are at %s.",
            sieve_ci_percent(sieve_efa_call_rmsea_level(job$call)), sieve_ci_percent(job$call$args$conf_level)
          ),
          if (length(aux_line)) aux_line,
          sprintf("lavaan version: %s.", as.character(utils::packageVersion("lavaan")))
        ))
      }
      keep_primary(list(
        fit = solution, status = status, warnings = record$warnings %||% character(0),
        basis = b, n = n_used, fm = job$fm, route = rt, cor_choice = job$cor_choice, rng = job$rng,
        rotation = job$rotation, call = job$call, code_view = code_view, engine = "DMAR",
        refused = if (is.null(solution)) status$message, refusal = status$refusal, reason = status$reason,
        error = record$error, seconds = record$seconds, requested_k = job$k, aux_ignored = job$aux
      ), request)
    }

    # Fit the common-factor model with DMAR::fa_exploratory. A solution
    # estimated at two seconds or less is fit here, under a progress
    # message; a longer one runs in a background R process
    # (sieve_efa_dmar_start()), and this reactive reads it when it is done.
    # The result is kept as a record: the solution, its status, and the
    # conditions DMAR raised.
    efa_run <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      request <- primary_key()
      primary_tick()
      if (identical(primary_kept$key, request) && !is.null(primary_kept$value$fit)) {
        return(primary_kept$value)
      }
      # A refusal is DMAR's answer for these inputs; it is not asked again
      # until the inputs change or Run selected EFA is pressed.
      if (identical(primary_kept$attempt_key, request) && !is.null(primary_kept$attempt_value)) {
        return(primary_kept$attempt_value)
      }
      if (!primary_allowed()) {
        return(list(
          fit = NULL, pending = TRUE, warnings = character(0),
          stopped = identical(primary_job$stopped, request),
          status = list(code = "not_run", message = "The selected EFA awaits an explicit request.")
        ))
      }
      d <- active_x()
      if (length(attr(d, "problems"))) {
        why <- paste(attr(d, "problems"), collapse = " ")
        return(keep_primary(list(
          fit = NULL, status = list(code = "failed", message = why),
          warnings = character(0), refused = why
        ), request))
      }
      k <- k_asked()
      why <- sieve_efa_k_problem(k, ncol(d))
      if (!is.null(why)) {
        return(keep_primary(list(
          fit = NULL, status = list(code = "failed", message = why),
          warnings = character(0), refused = why, requested_k = k
        ), request))
      }
      k <- as.integer(k)
      rot <- rotation()
      rt <- route()
      fm <- method()
      cor_choice <- input$cor_type %||% "auto"
      target <- NULL
      if (identical(rot, "target")) {
        target <- sieve_efa_target(state$meta, colnames(d))
        if (is.null(target)) {
          why <- "Target rotation needs constructs: no included item is assigned to a construct on Ingest. Assign them, or choose another rotation."
          return(keep_primary(list(
            fit = NULL, status = list(code = "failed", message = why),
            warnings = character(0), refused = why, requested_k = k
          ), request))
        }
      }
      # The matrix route fits minres to the shared correlation basis of the
      # included items (sieve_state_basis): the study's setting ("auto",
      # which follows the Ingest tab: Pearson for continuous items,
      # polychoric for ordered, tetrachoric for binary, mixed when the
      # types mix), or the matrix chosen on this tab, computed once per
      # item pool on the complete cases (or, for the saturated FIML matrix,
      # from every case) and shared with the other tabs that ask for it.
      b <- NULL
      if (identical(rt$form, "matrix")) {
        # Asked for: the solution's own gate (primary_allowed(), whose
        # estimate includes the matrix) or its button has decided
        # (sieve_basis_demand()).
        b <- sieve_basis_demand(state, cor_choice, asked = TRUE)
        req(b)
        if (!isTRUE(b$ok)) {
          why <- sprintf("No correlation matrix could be computed for the included items. %s", b$reason)
          return(keep_primary(list(
            fit = NULL, status = list(code = "failed", message = why),
            warnings = character(0), refused = why, basis = b
          ), request))
        }
      }
      # A fit the machine or the sample cannot carry is refused before
      # anything starts, with the reason and the ways forward.
      guard <- sieve_efa_dmar_guard(rt, d, k)
      if (!is.null(guard)) {
        return(keep_primary(list(
          fit = NULL, status = list(code = "failed", message = guard),
          warnings = character(0), refused = guard, requested_k = k
        ), request))
      }
      # DMAR's rotations start from 30 random positions, so the seed decides
      # which of the equivalent solutions (and which column order) is
      # reported. One seed is kept per request (sieve_keep_each()): a
      # request seen before (respondents restored after a removal, a
      # rotation chosen again) gives back the solution fit for it then.
      rng <- sieve_keep_each(primary_rng, "rng", request, function() sieve_draw_seed())
      call <- sieve_efa_dmar_call(rt, k, rot, data = d, basis = b, target = target, seed = rng$seed)
      job <- list(
        route = rt, data = d, basis = b, n = if (is.null(b)) cases_of(d, rt) else as.integer(b$n),
        k = k, rotation = rot, fm = fm, cor_choice = cor_choice, rng = rng, call = call,
        aux = aux_ignored(rt, d)
      )
      # A background run of this request has finished.
      if (identical(primary_job$record_key, request) && !is.null(primary_job$record)) {
        record <- primary_job$record
        primary_job$record <- NULL
        primary_job$record_key <- NULL
        return(accept_primary(record, request, job))
      }
      if (sieve_efa_dmar_active(primary_job, request)) {
        return(list(
          fit = NULL, running = TRUE, warnings = character(0),
          status = list(code = "running", message = "The selected EFA is running in a background R process.")
        ))
      }
      seconds <- tryCatch(primary_fit_wait(), error = function(e) NA_real_)
      if (is.finite(seconds) && seconds <= sieve_auto_seconds()) {
        record <- withProgress(
          message = sprintf("EFA: DMAR::fa_exploratory, %d factor%s (%s)", k, if (k == 1L) "" else "s",
            sieve_duration_label(seconds)),
          value = 0.2,
          sieve_efa_dmar_run(call, rng_kind = rng$kind)
        )
        return(accept_primary(record, request, job))
      }
      sieve_efa_dmar_submit(primary_job, request, call, seconds = seconds, rng_kind = rng$kind)
      if (identical(primary_job$record_key, request) && !is.null(primary_job$record)) {
        # The process could not start: say why, as a failure of this request.
        record <- primary_job$record
        primary_job$record <- NULL
        primary_job$record_key <- NULL
        return(accept_primary(record, request, job))
      }
      list(
        fit = NULL, running = TRUE, warnings = character(0),
        status = list(code = "running", message = "The selected EFA is running in a background R process.")
      )
    })

    # This status reads identities and saved metadata only. The page cue can
    # inspect it while EFA is hidden without starting a fit or a correlation.
    primary_status <- reactive({
      primary_signal()
      primary_tick()
      key <- primary_key()
      when <- primary_kept$value$code_view$recorded_at
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) return(list(state = "not_run", key = key,
        sentence = "Confirm item preparation on Ingest before using EFA results."))
      if (sieve_efa_dmar_active(primary_job, key)) {
        return(list(state = "running", key = key,
          sentence = "The selected EFA is running in a background R process."))
      }
      if (identical(primary_kept$attempt_key, key) &&
        !is.null(primary_kept$attempt_status) && !sieve_has_solution(primary_kept$attempt_status)) {
        return(list(state = "failed", key = key, when = when,
          sentence = primary_kept$attempt_status$message %||% "The selected EFA has no solution."))
      }
      if (identical(primary_kept$key, key) && !is.null(primary_kept$value$fit)) {
        return(list(state = "current", key = key, when = when,
          sentence = "The loadings and factor correlations match the current EFA inputs."))
      }
      # From before: a solution fit for these data under other inputs. A
      # solution of another dataset is not a stale form of this one.
      if (!is.null(primary_kept$value$fit) && identical(primary_kept$key$dataset_id, key$dataset_id)) {
        return(list(state = "stale", key = key, when = when,
          sentence = "The data, item pool or EFA settings changed. Update, in the status line, refits the selected EFA."))
      }
      list(state = "not_run", key = key,
        sentence = "Run selected EFA, in the status line, to obtain loadings and factor correlations for these inputs.")
    })
    observe({
      status <- primary_status()
      sieve_page_publish(state, "efa", session$ns("primary"), "EFA loadings and factor correlations",
        status, action = switch(status$state, current = , running = NULL, stale = "Update", "Run selected EFA"), own = TRUE)
    })

    efa <- reactive({
      efa_run()$fit
    })

    # What the displayed solution is not: refused by the data or by DMAR
    # (a refusal read by its class, with DMAR's reason), or not fit. NULL
    # for a proper solution and while the selected EFA waits for its
    # request or runs.
    result_caveat <- function(run) {
      if (is.null(run) || isTRUE(run$pending) || isTRUE(run$running)) {
        return(NULL)
      }
      if (!is.null(run$refused)) {
        # The data, the target, or DMAR's check of the solution stopped the
        # analysis: the reason is named, and nothing is shown as a result.
        full <- run$error %||% ""
        return(div(
          class = "alert alert-danger",
          strong("The EFA was not fit. "), run$refused,
          if (grepl("\n", full, fixed = TRUE)) {
            tags$details(class = "mt-1",
              tags$summary("DMAR's full message"),
              tags$pre(class = "small mb-0", style = "white-space: pre-wrap;", full)
            )
          }
        ))
      }
      if (is.null(run$fit)) {
        return(div(
          class = "alert alert-danger",
          "The EFA could not be fit. Try fewer factors or a different rotation.",
          if (!is.null(run$status$message)) tagList(" DMAR reported: ", em(run$status$message))
        ))
      }
      # DMAR refuses an improper solution, so a returned one is proper;
      # what lavaan warned about along the way is shown.
      if (length(run$warnings)) {
        return(div(
          class = "alert alert-warning small",
          strong("Warnings raised while fitting: "), paste(unique(run$warnings), collapse = " ")
        ))
      }
      NULL
    }

    # The page's one status line, in this order: what blocks the page; a
    # computation running in the background (with its elapsed time against the
    # estimate and Cancel); results from before an edit, with the one Update;
    # the selected EFA not fit yet, with Run selected EFA and its wait; a
    # stopped or failed comparison. A caveat on the displayed solution
    # (refused, not fit) follows on its own line.
    update_plan <- reactive({
      ps <- primary_status()
      cs <- tryCatch(count_status(), error = function(e) NULL)
      count <- identical(cs$state, "stale")
      # The comparison does not wait for the selected EFA, but Update fits
      # the selected EFA too when it is from before.
      primary <- identical(ps$state, "stale")
      seconds <- 0
      if (primary) seconds <- seconds + tryCatch(primary_seconds(), error = function(e) NA_real_)
      if (count) seconds <- seconds + tryCatch(count_plan()$seconds, error = function(e) NA_real_)
      list(
        primary = primary, fit = count, any = primary || count, seconds = seconds,
        primary_state = ps$state, fit_sentence = cs$sentence
      )
    })
    update_label <- function(seconds) sprintf("Update (%s)", sieve_duration_label(seconds))
    output$status_line <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(sieve_gate(state))
      }
      primary_tick()
      count_tick()
      run <- tryCatch(efa_run(), error = function(e) NULL)
      caveat <- result_caveat(run)
      line <- function(tone, ...) div(class = paste0("alert alert-", tone, " small py-2 mb-2"), role = "status", ...)
      up <- tryCatch(update_plan(), error = function(e) NULL)
      running <- list()
      # A running line is drawn once per run, with its Cancel button; only
      # its elapsed time is redrawn each second (fct_progress_line.R), so
      # Cancel keeps keyboard focus and takes every click.
      if (sieve_efa_dmar_active(primary_job)) {
        running <- c(running, list(line(
          "info", sieve_lifecycle_badge(if (!is.null(primary_kept$value$fit)) "refitting" else "running", "fit"),
          sprintf(
            "The selected EFA (DMAR::fa_exploratory, %s, k = %s) is running in a background R process. ",
            route()$label, as.character(primary_job$key$k %||% k_asked())
          ),
          sieve_progress_text(session$ns("efa_progress")), " ",
          actionButton(session$ns("efa_cancel"), "Cancel the EFA", class = "btn-sm")
        )))
      }
      if (sieve_efa_dmar_active(count_job)) {
        running <- c(running, list(line(
          "info", sieve_lifecycle_badge("running", "computed"),
          sprintf(
            "The number of factors comparison (DMAR::factor_count, k = 1 to %d) is running in a background R process. ",
            as.integer(count_job$key$max_factors %||% NA_integer_)
          ),
          sieve_progress_text(session$ns("count_progress_line")), " ",
          actionButton(session$ns("count_cancel_global"), "Cancel the comparison", class = "btn-sm")
        )))
      }
      if (length(running)) {
        status <- tagList(running,
          # The loadings from before an edit are updated while the
          # comparison of the new request runs: the same line, the one Update.
          if (isTRUE(up$primary) && identical(up$primary_state, "stale") && !sieve_efa_dmar_active(primary_job)) {
            line("warning", sieve_lifecycle_badge("stale"),
              strong("The loadings are from before an edit and are withheld until they are updated. "),
              actionButton(session$ns("update_go"), update_label(up$seconds), class = "btn-primary btn-sm")
            )
          }
        )
      } else if (isTRUE(up$any)) {
        loadings_stale <- identical(up$primary_state, "stale")
        what <- if (loadings_stale && up$fit) {
          "The data, the item pool or the EFA settings changed since the loadings and the factor count comparison were computed; both are withheld until they are updated. "
        } else if (loadings_stale) {
          "The data, the item pool or the EFA settings changed since the loadings were computed; they are withheld until they are updated. "
        } else {
          paste0(up$fit_sentence %||% "The factor count comparison's inputs changed since it was computed.", " ")
        }
        status <- line(
          "warning", sieve_lifecycle_badge("stale"), strong(what),
          actionButton(session$ns("update_go"), update_label(up$seconds), class = "btn-primary btn-sm")
        )
      } else if (isTRUE(run$pending)) {
        status <- line(
          "info", sieve_lifecycle_badge(if (isTRUE(run$stopped)) "stopped" else "not_run", "fit"),
          strong(if (isTRUE(run$stopped)) {
            "The selected EFA was canceled before it finished. "
          } else {
            "The selected EFA has not been fit for these data and settings. "
          }),
          if (sieve_restored_project(state)) {
            paste0(sieve_restored_sentence("Run selected EFA"), " ")
          } else if (!isTRUE(run$stopped)) {
            "A first result estimated above two seconds waits for this button. "
          },
          actionButton(session$ns("efa_go"),
            sprintf("Run selected EFA (%s)", sieve_efa_wait_label(tryCatch(primary_seconds(), error = function(e) NA_real_))),
            class = "btn-primary btn-sm"
          )
        )
      } else if (identical(count_job$stopped, tryCatch(count_key(), error = function(e) NULL))) {
        status <- line(
          "info", sieve_lifecycle_badge("stopped"), "The number of factors comparison was canceled. ",
          actionButton(session$ns("count_go_global"), "Run the comparison again", class = "btn-sm")
        )
      } else {
        status <- NULL
      }
      if (is.null(status) && is.null(caveat)) {
        return(NULL)
      }
      tagList(status, caveat)
    })
    # The elapsed time in the selected EFA's running line, redrawn each
    # second while it runs; primary_tick() wakes it for the next run.
    sieve_progress_outputs(output, "efa_progress", function() {
      primary_tick()
      if (sieve_efa_dmar_active(primary_job)) sieve_efa_dmar_progress(primary_job)$sentence
    }, session)
    observeEvent(input$efa_cancel, {
      # A queued refit after an item change does not start it again.
      inclusion_wire$cancel()
      sieve_efa_dmar_cancel(primary_job)
      primary_authorized(NULL)
      primary_tick(isolate(primary_tick()) + 1L)
    })

    # The one Update: every result on the page that was computed before
    # and is now from before, in turn, under one progress message; never a
    # result that was not requested (a comparison not computed yet keeps
    # its own button). A background computation announces itself when it
    # finishes.
    update_watch <- new.env(parent = emptyenv())
    update_watch$primary <- FALSE
    update_watch$fit <- FALSE
    update_current <- function() {
      if (sieve_efa_dmar_active(primary_job) || sieve_efa_dmar_active(count_job)) return(FALSE)
      up <- update_plan()
      if (!isTRUE(up$any)) {
        return()
      }
      what <- c(
        if (up$primary) "the loadings and factor correlations (DMAR::fa_exploratory)",
        if (up$fit) "the number of factors comparison (DMAR::factor_count)"
      )
      withProgress(
        message = sprintf("Updating %s (%s)", paste(what, collapse = ", then "), sieve_duration_label(up$seconds)),
        value = 0,
        {
          if (up$primary) {
            shiny::setProgress(0.1, detail = "The selected EFA.")
            primary_kept$attempt_value <- NULL
            primary_authorized(primary_key())
            tryCatch(efa_run(), error = function(e) NULL)
            # A run started here in the background wakes the poll observer,
            # which collects it, and the line's progress text, which shows
            # its time; neither reads anything else this path changes.
            if (sieve_efa_dmar_active(primary_job)) primary_tick(isolate(primary_tick()) + 1L)
          }
          if (up$fit) {
            shiny::setProgress(0.6, detail = "The number of factors comparison, in a background R process.")
            request_count()
            tryCatch(count_run(), error = function(e) NULL)
          }
          shiny::setProgress(1)
        }
      )
      background <- sieve_efa_dmar_active(primary_job) || sieve_efa_dmar_active(count_job)
      if (background) {
        update_watch$primary <- sieve_efa_dmar_active(primary_job)
        update_watch$fit <- sieve_efa_dmar_active(count_job)
        sieve_notify(paste0(
          if (up$primary && !update_watch$primary) "Updated the loadings and factor correlations. " else "",
          "The rest is running in the background; a message follows when it finishes."
        ))
      } else {
        sieve_notify(sprintf("Updated %s.", paste(what, collapse = " and ")))
      }
    }
    page_update <- sieve_update_wire(input, session, function() {
      up <- update_plan()
      list(page = sieve_update_entry("EFA results",
        state = if (isTRUE(up$any)) "stale" else "current", seconds = up$seconds,
        run = update_current, background = TRUE, shown = FALSE))
    }, sequence = function(entries) {
      refit_after_item(NULL)
      update_current()
    })
    inclusion_wire <- sieve_inclusion_wire(state, function() {
      # Only an update that had something to refit counts as a refit after
      # the item change, and only for the request it fit: with nothing stale
      # (no solution yet), or for other data later, a first run is not
      # labeled as following it.
      updating <- isTRUE(tryCatch(update_plan()$any, error = function(e) FALSE))
      out <- update_current()
      if (updating && !identical(out, FALSE)) refit_after_item(tryCatch(primary_key(), error = function(e) NULL))
      out
    })

    # The one-line method statement of the displayed solution, read from
    # the run that produced it.
    output$efa_method <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      run <- tryCatch(efa_run(), error = function(e) NULL)
      req(!is.null(run$fit))
      f <- run$fit
      rt <- run$route
      k <- length(f$factors)
      source <- if (identical(rt$form, "matrix")) {
        b <- run$basis
        sprintf("; %s correlations (%s), %s",
          sieve_retention_basis_label(b$used),
          if (isTRUE(b$changed)) {
            sprintf("%s requested and not computable", sieve_retention_basis_label(b$requested))
          } else if ((run$cor_choice %||% "auto") %in% c("auto", "study")) "study setting" else "chosen on this tab",
          sieve_basis_n_text(b))
      } else {
        sprintf("; %s", if (identical(rt$missing, "fiml")) {
          sprintf("FIML, N = %d", as.integer(f$method$N))
        } else {
          sprintf("complete cases, N = %d", as.integer(f$method$N))
        })
      }
      ps <- primary_status()
      p(
        class = "small mb-1",
        # Where the solution stands, in the same word as every other page.
        sieve_lifecycle_badge(ps$state, "fit"),
        strong("Model: "),
        sprintf(
          "DMAR::fa_exploratory, %s, %s rotation, k = %d; %d items%s; %s intervals%s.",
          rt$label,
          if (k == 1L) "no" else switch(run$rotation %||% "oblimin", oblimin = "direct oblimin", none = "no", run$rotation),
          k, length(f$items), source, sieve_ci_percent(f$conf_level %||% sieve_default_conf_level()),
          if (sieve_efa_has_rmsea(rt)) sprintf(" (the RMSEA's at %s)", sieve_ci_percent(sieve_efa_fit_level(f))) else ""
        ),
        if (identical(ps$state, "current")) sprintf(
          " Fit at %s; the tables below are from this fit.%s",
          sieve_clock_text(run$code_view$recorded_at) %||% "an unrecorded time",
          if (!is.null(refit_after_item()) && identical(refit_after_item(), primary_kept$key)) {
            " It was refit by itself after your item change."
          } else {
            ""
          }
        ),
        sieve_holdout_n_tag(state),
        if (length(run$aux_ignored)) {
          # One line, its explanation one click away, so the method block
          # stays short above the tables.
          tags$details(class = "d-block mt-1",
            tags$summary(strong("Auxiliary variables: "), sprintf("%s not used by this fit.",
              paste(run$aux_ignored, collapse = ", "))),
            sieve_efa_aux_sentence(run$aux_ignored, "fa_exploratory"))
        }
      )
    })

    # The fit line: DMAR's fit_measures attribute of the selected solution,
    # computed from the unrotated solution of the same model (the chi
    # square and the standard indices do not depend on the rotation, and
    # lavaan's FIML-corrected indices of a rotated object do, so every
    # rotation reports the unrotated fit).
    output$efa_fit_line <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      run <- tryCatch(efa_run(), error = function(e) NULL)
      req(!is.null(run$fit))
      sentence <- fit_sentence(run$fit)
      if (!nzchar(sentence)) {
        return(NULL)
      }
      p(class = "small mb-2", strong("Fit: "), HTML(sentence))
    })
    fit_sentence <- function(f) {
      fm <- f$fit_measures
      if (is.null(fm)) {
        return(paste(
          "DMAR withheld the fit measures (the unrestricted FIML information they need is unavailable);",
          "the parameter results are qualified, as DMAR's note says."
        ))
      }
      v <- function(t) sieve_efa_fit_value(fm, t)
      # DMAR forms the RMSEA interval at the call's conf_level, the level of
      # the coefficients' intervals (the page's level).
      level <- sieve_ci_percent(sieve_efa_fit_level(f))
      version <- sieve_efa_fit_version(fm)
      tail_text <- if (length(version)) {
        sprintf(" <span class=\"text-muted\">%s</span>", htmltools::htmlEscape(paste(version, collapse = " ")))
      } else {
        ""
      }
      if (identical(f$method$estimator, "ULS")) {
        return(paste0(sprintf(
          paste0(
            "the unrotated solution of the same model (every rotation shares it). ULS discrepancy statistic ",
            "(N - 1 times the minimum) = %s on %s df, with no chi-square reference distribution, so no p-value, ",
            "RMSEA, CFI or TLI; NFI = %s, SRMR = %s."
          ),
          fmt_fit(v("chi_square")), format(v("df")), fmt_fit(v("nfi")), fmt_fit(v("srmr"))
        ), tail_text))
      }
      p_text <- if (is.finite(v("p_chi_square"))) format.pval(v("p_chi_square"), digits = 3, eps = 1e-4) else "not available"
      paste0(sprintf(
        paste0(
          "the unrotated solution of the same model (every rotation shares it). Chi-square = %s on %s df, ",
          "p = %s; RMSEA = %s, %s CI [%s, %s]; CFI = %s; TLI = %s; SRMR = %s."
        ),
        fmt_fit(v("chi_square")), format(v("df")), p_text, fmt_fit(v("rmsea")), level,
        fmt_fit(v("rmsea_ci_lower")), fmt_fit(v("rmsea_ci_upper")), fmt_fit(v("cfi")), fmt_fit(v("tli")),
        fmt_fit(v("srmr"))
      ), tail_text)
    }

    # What the analysis is, below the tables: the first paragraph visible,
    # the rest under a closed label.
    output$efa_overview <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      d <- tryCatch(active_x(), error = function(e) NULL)
      req(!is.null(d), ncol(d) >= 2L)
      k <- k_asked()
      if (!primary_allowed() || !is.null(sieve_efa_k_problem(k, ncol(d)))) return(NULL)
      rt <- route()
      rot_lbl <- sieve_efa_rotation_words(rotation())
      run <- tryCatch(efa_run(), error = function(e) NULL)
      b <- run$basis
      data_txt <- if (identical(rt$form, "matrix")) {
        if (!is.null(b) && isTRUE(b$ok)) {
          sprintf(
            paste0(
              " Minres is fit to the <b>%s</b> correlation matrix of the included items (%s), computed once ",
              "and shared with the other tabs that ask for it%s, and supplied to DMAR with its n; a supplied ",
              "matrix carries no covariance matrix of the correlations, so DMAR reports the estimates without ",
              "standard errors or intervals, and says so."
            ),
            sieve_retention_basis_label(b$used),
            htmltools::htmlEscape(sub("[.]$", "", sieve_basis_source_text(input$cor_type %||% "auto", state, unname(item_types())))),
            if (isTRUE(b$changed)) sprintf(
              " (%s correlations were requested and could not be computed)",
              sieve_retention_basis_label(b$requested)
            ) else ""
          )
        } else {
          ""
        }
      } else if (identical(rt$estimator, "WLSMV")) {
        paste0(
          " The ordered items are fit as they stand by <b>WLSMV</b> (their polychoric correlations and ",
          "thresholds), on the complete cases; the loadings, determinacy and H describe the latent ",
          "responses behind the categories."
        )
      } else if (identical(rt$estimator, "ULS")) {
        paste0(
          " Minres (unweighted least squares on the correlation matrix, Harman &amp; Jones, 1966) is fit ",
          "to the complete item responses, with sandwich standard errors from the raw data; its statistic has ",
          "no chi-square reference distribution, so the fit line gives the discrepancy, the NFI and the SRMR."
        )
      } else if (identical(rt$missing, "fiml")) {
        paste0(
          " The model is fit to the raw item responses by <b>full information maximum likelihood</b>: ",
          "every respondent with an observed item contributes, under missingness at random given the ",
          "included items. It is not the two-step route (EM moments, then a fit with the complete-case n), ",
          "and its estimates and fit differ from that route's by design.",
          if (length(run$aux_ignored)) paste0(" ", htmltools::htmlEscape(sieve_efa_aux_sentence(run$aux_ignored, "fa_exploratory"))) else ""
        )
      } else {
        " The model is fit to the complete cases of the raw item responses."
      }
      est_txt <- if (identical(rt$form, "data") && !identical(rt$estimator, "ULS")) {
        sprintf(
          " The estimator is <b>%s</b> (%s).",
          htmltools::htmlEscape(rt$label),
          htmltools::htmlEscape(sub("[.]$", "", sieve_estimator_source_text(input$estimator %||% "study", state)))
        )
      } else {
        ""
      }
      rmsea_level <- if (!is.null(run$fit)) sieve_efa_fit_level(run$fit) else sieve_default_rmsea_conf_level()
      div(
        class = "small border-top pt-2 mb-2",
        p(class = "mb-1", paste0(
          "The tables come from DMAR::fa_exploratory, with DMAR's 95% Wald intervals beside the ",
          "coefficients; the fit line is the fit of the same model",
          if (sieve_efa_has_rmsea(rt)) paste0(
            ", with the RMSEA's interval at ", sieve_ci_percent(rmsea_level),
            ", the RMSEA level (DMAR's and lavaan's: the limits of a 90% interval settle the tests of close ",
            "and not close fit at .05; MacCallum, Browne & Sugawara, 1996); the coefficients' intervals are at 95%."
          ) else "."
        )),
        tags$details(open = NA,
          tags$summary("More on how to read this analysis"),
          p(class = "mb-1", HTML(sprintf(
            paste0(
              "<b>What this analysis is.</b> An <b>exploratory factor analysis</b> by DMAR::fa_exploratory ",
              "(the common-factor model, not principal components, fit through lavaan), extracting ",
              "<b>%d factor%s</b> with a <b>%s</b> rotation (from 30 random starts), on %d active items. ",
              "The <b>pattern matrix</b> holds the standardized regression weights of items on ",
              "the factors (each factor's unique contribution); the <b>structure matrix</b> holds the ",
              "item-factor correlations; <b>Phi</b> is the factor correlation matrix; <b>h2</b> is the ",
              "communality; and each factor's <b>determinacy</b> and <b>H</b> say how well the items ",
              "determine it. The intervals describe the selected rotated solution at the selected ",
              "number of factors; they do not include the uncertainty of choosing that number or rotation."
            ),
            as.integer(k), if (identical(as.integer(k), 1L)) "" else "s", rot_lbl, ncol(d)
          ))),
          p(class = "mb-1", HTML(paste0(
            trimws(paste0(data_txt, est_txt)),
            " Set the method, the estimator, the correlations of the matrix route, the ",
            "number of factors and the rotation at the left."
          )))
        )
      )
    })

    factor_readout_code <- function() {
      c(
        "# The tables, read from DMAR's table (its standardized rows):",
        "efa_std <- as.data.frame(efa_fit)",
        "efa_std <- efa_std[efa_std$metric == \"standardized\", ]",
        "efa_lam <- efa_std[efa_std$op == \"=~\", ]",
        "factor_order <- unique(efa_lam$lhs)  # actual column labels, in the order used for this fit",
        "efa_items <- unique(efa_lam$rhs)",
        "efa_cell <- cbind(match(efa_lam$rhs, efa_items), match(efa_lam$lhs, factor_order))",
        "pattern <- matrix(NA_real_, length(efa_items), length(factor_order), dimnames = list(efa_items, factor_order))",
        "pattern_lower <- pattern_upper <- pattern",
        "pattern[efa_cell] <- efa_lam$estimate",
        "pattern_lower[efa_cell] <- efa_lam$ci_lower",
        "pattern_upper[efa_cell] <- efa_lam$ci_upper",
        "Phi <- diag(length(factor_order))",
        "dimnames(Phi) <- list(factor_order, factor_order)",
        "efa_fc <- efa_std[efa_std$op == \"~~\" & efa_std$lhs %in% factor_order & efa_std$rhs %in% factor_order & efa_std$lhs != efa_std$rhs, ]",
        "Phi[cbind(match(efa_fc$lhs, factor_order), match(efa_fc$rhs, factor_order))] <- efa_fc$estimate",
        "Phi[cbind(match(efa_fc$rhs, factor_order), match(efa_fc$lhs, factor_order))] <- efa_fc$estimate",
        "structure_matrix <- pattern %*% Phi  # item-factor correlations",
        "communalities <- efa_std[efa_std$op == \"communality\", c(\"lhs\", \"estimate\", \"ci_lower\", \"ci_upper\")]",
        "complexity <- efa_std[efa_std$op == \"complexity\", c(\"lhs\", \"estimate\")]",
        "efa_fc[, c(\"lhs\", \"rhs\", \"estimate\", \"ci_lower\", \"ci_upper\")]  # factor correlations with intervals",
        "list(factor_column_order = factor_order, pattern = pattern, structure = structure_matrix, Phi = Phi)",
        "# Across programs, match whole loading columns (including possible sign changes).",
        "# Apply the same mapping to both rows and columns of Phi; factor numbers alone do not identify a factor."
      )
    }
    output$efa_code <- renderUI({
      run <- tryCatch(efa_run(), error = function(e) NULL)
      sieve_code_view(run$code_view,
        status = if (is.null(run$fit)) "not_run" else "current",
        title = "Show code: loadings, factor correlations and EFA tables",
        ns = session$ns,
        readout = factor_readout_code()
      )
    })
    factor_order_note <- reactive({
      f <- efa()
      req(f)
      order <- colnames(unclass(f$loadings))
      div(class = "small text-muted mb-2",
        p(class = "mb-1", strong("Factor column order in this fit: "), paste(order, collapse = ", "), "."),
        tags$details(open = NA,
          tags$summary("Comparing factor labels across programs"),
          p(class = "mb-1",
            "These engine labels identify columns within this fit. Match whole loading columns across programs, allowing for ",
            "different order or signs, then apply the same mapping to both rows and columns of Phi. ",
            "Sorting items changes rows; factor column labels remain the same. Show code prints the actual order."
          )
        )
      )
    })
    output$factor_order_pattern <- renderUI(factor_order_note())
    output$factor_order_structure <- renderUI(factor_order_note())
    output$factor_order_phi <- renderUI(factor_order_note())

    output$display_pool_note <- renderUI({
      construct <- input$display_construct %||% ""
      facet <- input$display_facet %||% ""
      if (!nzchar(construct) && !nzchar(facet)) return(NULL)
      run <- tryCatch(efa_run(), error = function(e) NULL)
      p <- if (is.null(run$fit)) NULL else nrow(unclass(run$fit$loadings))
      # A notice about the display: one line under the filters that cause
      # it, not a banner.
      div(class = "small text-muted mb-1", role = "status",
        sprintf("Showing %s%s rows from the full fitted item pool%s. This is a display filter; the model has not been fit to this subset alone.",
          if (nzchar(construct)) paste0("construct ", construct) else "all construct",
          if (nzchar(facet)) paste0(", facet ", facet) else "",
          if (is.null(p)) "" else sprintf(" (%d items)", p))
      )
    })

    # Pattern matrix as a plain numeric matrix.
    pattern_mat <- reactive({
      f <- efa()
      req(f)
      unclass(f$loadings)
    })

    # Plot coordinates read the same current fit as the loading tables. Display
    # controls never enter primary_key(), so neither plotting nor saving fits.
    plot_factors <- reactive({
      columns <- colnames(pattern_mat())
      x <- input$plot_x %||% columns[1L]
      if (!x %in% columns) x <- columns[1L]
      if (length(columns) == 1L) return(x)
      remaining <- setdiff(columns, x)
      y <- input$plot_y %||% remaining[1L]
      if (!y %in% remaining) y <- remaining[1L]
      c(x, y)
    })
    output$plot_axes <- renderUI({
      columns <- colnames(pattern_mat())
      axes <- plot_factors()
      div(class = "d-flex flex-wrap gap-3",
        selectInput(session$ns("plot_x"), "Horizontal factor", columns, selected = axes[1L]),
        if (length(columns) > 1L) selectInput(session$ns("plot_y"), "Vertical factor",
          setdiff(columns, axes[1L]), selected = axes[2L])
      )
    })
    factor_plot_items <- reactive({
      ids <- rownames(pattern_mat())
      meta <- state$meta[match(ids, state$meta$item), , drop = FALSE]
      keep <- rep(TRUE, length(ids))
      if (nzchar(input$display_construct %||% "")) {
        keep <- keep & !is.na(meta$construct) & meta$construct == input$display_construct
      }
      if (nzchar(input$display_facet %||% "")) {
        keep <- keep & !is.na(meta$facet) & meta$facet == input$display_facet
      }
      ids[keep]
    })
    factor_plot_data <- reactive({
      f <- efa()
      shiny::validate(shiny::need(!is.null(f), "The factor plot waits for the selected EFA: press Run selected EFA or Update in the status line above."))
      data <- sieve_efa_plot_coordinates(f, factors = plot_factors(),
        matrix = input$plot_matrix %||% "pattern", items = factor_plot_items())
      shiny::validate(shiny::need(nrow(data) > 0L, "No fitted items match this display filter."))
      data
    })
    output$factor_plot_note <- renderUI({
      data <- factor_plot_data()
      run <- efa_run()
      axes <- attr(data, "factors")
      div(class = "small text-muted mb-2",
        p(class = "mb-1", sprintf("DMAR::fa_exploratory, %s; %s rotation; %s coefficients. Showing %d of %d fitted items.",
          run$route$label, run$rotation, attr(data, "matrix"),
          nrow(data), nrow(unclass(run$fit$loadings)))),
        if (length(axes) == 2L) p(class = "mb-1",
          sprintf("Factor correlation (%s, %s): %.3f.", axes[1L], axes[2L], attr(data, "factor_correlation"))
        ) else p(class = "mb-1", "One factor was fitted; its coefficients are shown by item."),
        tags$details(open = NA, class = "mb-1",
          tags$summary("Interpreting the factor plot"),
          if (length(axes) == 2L) p(class = "mb-1",
            "Cartesian axes show coefficient coordinates; they do not imply that the factors are uncorrelated. ",
            "This pair is a projection of the full solution; inspect the other factors for additional loadings."
          ),
          p(class = "mb-1",
            "Pattern coefficients are standardized regression coefficients; structure coefficients are item-factor correlations ",
            "(pattern multiplied by Phi). Oblique pattern coefficients can exceed one. ",
            "The loading-table suppression cutoff does not remove plot points. ",
            "Definitions: the help page of DMAR::fa_exploratory()."
          )
        )
      )
    })
    factor_plot_figure <- function() {
      data <- factor_plot_data()
      sieve_efa_factor_plot(efa(), factors = attr(data, "factors"), matrix = attr(data, "matrix"),
        items = data$item, labels = input$plot_labels %||% TRUE)
      invisible(NULL)
    }
    output$factor_plot <- shiny::renderPlot(factor_plot_figure(), res = 96, alt = "Loadings of each item on each factor of the exploratory solution. The values are in the pattern and structure matrices on this page.", height = function() {
      data <- factor_plot_data()
      if (length(attr(data, "factors")) == 1L) max(600L, 120L + 22L * nrow(data)) else 600L
    })
    # Saving is a display operation; avoid a duplicate analysis step or input
    # snapshot. Its executable readout is available beside the plot itself.
    sieve_plot_save_server("factor_plot", output, session, state = NULL, draw = factor_plot_figure,
      file_stem = "siever_efa_factor_plot", what = "the selected EFA coefficient plot", default = c(900, 600))
    output$factor_plot_hover_note <- renderUI({
      data <- factor_plot_data()
      near <- shiny::nearPoints(data, input$factor_plot_hover, xvar = "x", yvar = "y",
        maxpoints = 3L, threshold = 12, addDist = FALSE)
      if (!nrow(near)) return(p(class = "small text-muted", "Hover over a point for its item name and coefficients."))
      tagList(lapply(seq_len(nrow(near)), function(i) {
        label <- state$meta$label[match(near$item[i], state$meta$item)]
        if (is.na(label) || !nzchar(label)) label <- near$item[i]
        axes <- attr(data, "factors")
        p(class = "small mb-1", strong(near$item[i]), ": ", label, ". ",
          sprintf("%s = %.6f", axes[1L], near$x[i]),
          if (length(axes) == 2L) sprintf("; %s = %.6f", axes[2L], near$y[i]))
      }))
    })
    output$factor_plot_code <- renderUI({
      data <- factor_plot_data()
      literal <- function(x) paste(deparse(x, width.cutoff = 80L), collapse = "\n")
      sieve_code_view(efa_run()$code_view, status = "current", title = "Show code: factor plot",
        ns = session$ns, readout = c(
          "# Uses the same fitted efa_fit (DMAR::fa_exploratory) as the loading tables; no additional fit.",
          sprintf("SIEVER::sieve_efa_factor_plot(efa_fit, factors = %s, matrix = %s,",
            literal(attr(data, "factors")), literal(attr(data, "matrix"))),
          sprintf("  items = %s, labels = %s)", literal(data$item),
            if (isTRUE(input$plot_labels %||% TRUE)) "TRUE" else "FALSE")
        ))
    })

    # Order items by their dominant (largest absolute) loading factor, then by
    # the size of that loading. Mirrors the SPSS sorted-loadings layout.
    sort_order <- function(mat) {
      dom <- apply(abs(mat), 1L, which.max)
      peak <- mat[cbind(seq_len(nrow(mat)), dom)]
      order(dom, -abs(peak))
    }

    # Apply the suppression cutoff: blank cells below the cutoff (as ""), which
    # forces a character matrix for display.
    suppress_blank <- function(mat, cutoff) {
      out <- formatC(mat, format = "f", digits = 3)
      out[abs(mat) < cutoff] <- ""
      dim(out) <- dim(mat)
      dimnames(out) <- dimnames(mat)
      out
    }
    show_ci <- reactive(isTRUE(input$show_ci %||% TRUE))
    ci_level <- function(f) sprintf("%.0f%%", 100 * (f$conf_level %||% 0.95))

    # The rows of the Pattern and Structure tables are items, as displayed
    # (after the construct and facet filters); the selected ones go to the
    # shared Exclude / Include dialog (fct_item_decision.R, mod_drop.R),
    # whose Exclude and Include remain the only writers of the item status.
    shown_items <- new.env(parent = emptyenv())
    shown_items$pattern <- character(0)
    shown_items$structure <- character(0)
    sieve_item_decision_wire(input, state, "pattern_decide",
      items = function() sieve_selected_items(NULL, input$pattern_tbl_rows_selected, shown_items$pattern),
      origin = "the EFA Pattern matrix"
    )
    sieve_item_decision_wire(input, state, "structure_decide",
      items = function() sieve_selected_items(NULL, input$structure_tbl_rows_selected, shown_items$structure),
      origin = "the EFA Structure matrix"
    )
    # A coefficient table in SIEVER's layout: one column per factor and,
    # when intervals are shown and DMAR gave them, the interval of each
    # coefficient in the column after it (blank, as the coefficient is,
    # below the cutoff).
    loading_table <- function(mat, which = "pattern", lower = NULL, upper = NULL, level = "95%") {
      cutoff <- input$suppress %||% 0.30
      if (is.na(cutoff)) cutoff <- 0
      if (isTRUE(input$sort_loadings)) {
        at <- sort_order(mat)
        mat <- mat[at, , drop = FALSE]
        if (!is.null(lower)) lower <- lower[rownames(mat), colnames(mat), drop = FALSE]
        if (!is.null(upper)) upper <- upper[rownames(mat), colnames(mat), drop = FALSE]
      }
      blanked <- suppress_blank(mat, cutoff)
      # Highlight the salient loadings (those above the cutoff) so the simple
      # structure stands out; small values are already blanked.
      nz <- nzchar(blanked)
      blanked[nz] <- sprintf('<span style="color:#0c2340;font-weight:600">%s</span>', blanked[nz])
      with_ci <- show_ci() && !is.null(lower) && !is.null(upper) && any(is.finite(lower))
      cells <- list()
      headers <- character(0)
      for (j in seq_len(ncol(mat))) {
        cells[[length(cells) + 1L]] <- blanked[, j]
        headers <- c(headers, colnames(mat)[j])
        if (with_ci) {
          ci <- sieve_efa_interval_cell(lower[, j], upper[, j])
          ci[abs(mat[, j]) < cutoff] <- ""
          cells[[length(cells) + 1L]] <- sprintf('<span class="text-muted">%s</span>', ci)
          headers <- c(headers, sprintf("%s %s CI", colnames(mat)[j], level))
        }
      }
      item_name <- state$meta$label[match(rownames(mat), state$meta$item)]
      item_name[is.na(item_name)] <- rownames(mat)[is.na(item_name)]
      keep <- rep(TRUE, nrow(mat))
      mm <- state$meta[match(rownames(mat), state$meta$item), , drop = FALSE]
      if (nzchar(input$display_construct %||% "")) keep <- keep & !is.na(mm$construct) & mm$construct == input$display_construct
      if (nzchar(input$display_facet %||% "")) keep <- keep & !is.na(mm$facet) & mm$facet == input$display_facet
      df <- data.frame(
        Item = rownames(mat), ItemName = item_name,
        stats::setNames(cells, make.unique(headers)),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      df <- df[keep, , drop = FALSE]
      shown_items[[which]] <- as.character(df$Item)
      sieve_dt(df,
        rownames = FALSE, selection = "multiple", escape = c(1L, 2L),
        colnames = c("Variable Name", "Item Name", headers),
        options = list(dom = "fltip", pageLength = 25, lengthMenu = c(10, 25, 50, 100), scrollX = TRUE)
      )
    }

    observeEvent(state$meta,
      {
        m <- state$meta
        groups <- sort(unique(m$construct[sieve_construct_assigned(m$construct)]))
        shiny::updateSelectInput(session,
          "display_construct",
          choices = c("All constructs" = "", stats::setNames(groups, groups)),
          selected = intersect(isolate(input$display_construct) %||% "", c("", groups))
        )
      },
      ignoreInit = FALSE
    )
    observeEvent(list(state$meta, input$display_construct),
      {
        m <- state$meta
        if (nzchar(input$display_construct %||% "")) m <- m[!is.na(m$construct) & m$construct == input$display_construct, ,
          drop = FALSE
        ]
        groups <- sort(unique(m$facet[!is.na(m$facet) & nzchar(m$facet)]))
        shiny::updateSelectInput(session,
          "display_facet",
          choices = c("All facets" = "", stats::setNames(groups, groups)),
          selected = intersect(isolate(input$display_facet) %||% "", c("", groups))
        )
      },
      ignoreInit = FALSE
    )
    output$dl_numeric <- shiny::downloadHandler(
      filename = function() "siever_efa_numeric_tables.tar.gz",
      content = function(file) {
        r <- efa_run()
        req(r$fit)
        tables <- sieve_efa_numeric_tables(r$fit, isolate(state$meta))
        count <- tryCatch(count_run(), error = function(e) NULL)
        result <- list(
          tables = tables, fit = tables$fit_measures,
          factor_count = if (identical(count$status, "ran")) count$table,
          manifest = list(
            revision = sieve_revision(state), engine = sprintf("DMAR %s, fa_exploratory()", as.character(utils::packageVersion("DMAR"))),
            rmsea_conf_level = sieve_efa_fit_level(r$fit),
            method = r$route$label, estimator = r$route$estimator, missing = r$route$missing %||% NA_character_,
            rotation = r$rotation, factors = length(r$fit$factors), seed = r$rng$seed,
            conf_level = r$fit$conf_level, correlation_basis = r$basis$used %||% NA_character_,
            analytic_n = r$n %||% NA_integer_,
            suppression = "No numeric values suppressed or rounded; display filters do not change this export.",
            status = r$status[c("code", "message")]
          )
        )
        dir <- tempfile("siever_efa_tables_")
        dir.create(dir)
        on.exit(unlink(dir, recursive = TRUE), add = TRUE)
        for (nm in names(tables)) {
          if (!is.null(tables[[nm]])) {
            utils::write.csv(tables[[nm]], file.path(dir, paste0(nm, ".csv")), row.names = is.matrix(tables[[nm]]))
          }
        }
        if (!is.null(result$factor_count)) {
          fc <- result$factor_count
          class(fc) <- "data.frame"
          utils::write.csv(fc, file.path(dir, "factor_count.csv"), row.names = FALSE)
        }
        saveRDS(result, file.path(dir, "efa_results.rds"), version = 3L)
        dput(result$manifest, file.path(dir, "manifest.R"))
        file <- normalizePath(file, mustWork = FALSE)
        withr::with_dir(dir, utils::tar(file, list.files(dir), compression = "gzip", tar = "internal"))
      }
    )
    output$pattern_tbl <- renderDT({
      f <- efa()
      req(f)
      loading_table(pattern_mat(), "pattern", f$loadings_lower, f$loadings_upper, ci_level(f))
    })
    output$pattern_ci_note <- renderUI({
      f <- efa()
      req(f)
      if (!any(is.finite(f$loadings_lower))) {
        return(p(class = "small text-muted mb-1",
          "No intervals: minres on a supplied correlation matrix has no standard errors (the matrix carries no covariance matrix of the correlations). Minres on the item responses gives them."))
      }
      if (!show_ci()) return(NULL)
      p(class = "small text-muted mb-1", sprintf(
        "Beside each coefficient: its %s Wald interval (DMAR, from lavaan's rotation-aware standard errors), blank with the coefficient below the cutoff.",
        ci_level(f)))
    })

    output$structure_tbl <- renderDT({
      f <- efa()
      req(f, f$Structure)
      loading_table(unclass(f$Structure), "structure")
    })

    output$phi_tbl <- renderDT({
      f <- efa()
      req(f)
      phi <- f$Phi
      df <- data.frame(
        Factor = rownames(phi), phi,
        check.names = FALSE, stringsAsFactors = FALSE
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t", pageLength = 25)
      ) |>
        formatRound(colnames(phi), 3)
    })
    phi_pairs <- reactive({
      f <- efa()
      req(f)
      k <- ncol(f$Phi)
      if (k < 2L) return(NULL)
      at <- which(upper.tri(f$Phi), arr.ind = TRUE)
      data.frame(
        A = colnames(f$Phi)[at[, 1L]], B = colnames(f$Phi)[at[, 2L]],
        r = f$Phi[at], lower = f$Phi_lower[at], upper = f$Phi_upper[at],
        stringsAsFactors = FALSE
      )
    })
    output$phi_ci_head <- renderUI({
      f <- efa()
      req(f)
      if (ncol(f$Phi) < 2L) return(NULL)
      if (!isTRUE(f$oblique)) {
        return(p(class = "small text-muted mt-2",
          "The rotation keeps the factors uncorrelated: every factor correlation is fixed at zero, with no interval."))
      }
      p(class = "small mt-2 mb-1", strong(sprintf("Factor correlations with their %s intervals", ci_level(f))))
    })
    output$phi_ci_tbl <- renderDT({
      f <- efa()
      req(f, isTRUE(f$oblique))
      df <- phi_pairs()
      req(df)
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        colnames = c("Factor", "With factor", "Correlation", sprintf("%s CI lower", ci_level(f)), sprintf("%s CI upper", ci_level(f))),
        options = list(dom = "t", pageLength = 50)
      ) |>
        formatRound(c("r", "lower", "upper"), 3)
    })

    output$comm_tbl <- renderDT({
      f <- efa()
      req(f)
      h2 <- f$communality
      item_name <- state$meta$label[match(names(h2), state$meta$item)]
      item_name[is.na(item_name)] <- names(h2)[is.na(item_name)]
      df <- data.frame(
        Item = names(h2), ItemName = item_name,
        Communality = as.numeric(h2),
        Communality_lower = as.numeric(f$communality_lower),
        Communality_upper = as.numeric(f$communality_upper),
        Uniqueness = 1 - as.numeric(h2),
        Uniqueness_lower = 1 - as.numeric(f$communality_upper),
        Uniqueness_upper = 1 - as.numeric(f$communality_lower),
        Complexity = as.numeric(f$complexity),
        stringsAsFactors = FALSE
      )
      level <- ci_level(f)
      labels <- c("Variable Name", "Item Name", "Communality (h2)", sprintf("h2 %s CI lower", level), sprintf("h2 %s CI upper", level),
        "Uniqueness (1 - h2)", sprintf("Uniqueness %s CI lower", level), sprintf("Uniqueness %s CI upper", level), "Complexity")
      keep <- if (show_ci() && any(is.finite(df$Communality_lower))) seq_along(df) else c(1L, 2L, 3L, 6L, 9L)
      sieve_dt(df[, keep, drop = FALSE],
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25),
        colnames = labels[keep]
      ) |>
        formatRound(intersect(names(df)[keep], c("Communality", "Communality_lower", "Communality_upper",
          "Uniqueness", "Uniqueness_lower", "Uniqueness_upper", "Complexity")), 3)
    })
    output$comm_note <- renderUI({
      req(efa())
      p(class = "small text-muted",
        "A communality is one minus the item's standardized residual variance, so its interval reverses the limits of that variance (DMAR); ",
        "limits are not clipped to [0, 1]. Complexity is Hofmann's (1978): 1 for an item on one factor, the number of factors for an item loading equally on all."
      )
    })

    # DMAR's variance accounted for (SIEVER's layout, DMAR's values): each
    # factor's diagonal of Phi Lambda' Lambda, which adds to the total
    # communality.
    output$vacc_note <- renderUI({
      f <- efa()
      req(f)
      if (!isTRUE(f$oblique)) {
        return(p(class = "text-muted small",
          "Uncorrelated factors: each factor's variance accounted for is the column sum of its squared loadings, and the factors add to the total communality."))
      }
      p(
        class = "text-muted small",
        "Correlated factors: each factor's variance accounted for is its diagonal element of Phi Lambda' Lambda, the column sum of ",
        "pattern times structure coefficients (what psych labels SS loadings under an oblique rotation); the factors add to the total ",
        "communality, and the proportions are of the items' total variance (their number) and of the common variance. They are ",
        "not SPSS's rotation sums of squared loadings, which are the column sums of the squared structure coefficients and, with ",
        "correlated factors, do not add to a total. No cumulative row is shown, since the order of the factors is arbitrary."
      )
    })
    output$vacc_tbl <- renderDT({
      f <- efa()
      req(f)
      df <- sieve_efa_vaccounted_table(f)
      req(df)
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t", pageLength = 25)
      ) |>
        formatRound(setdiff(names(df), "Statistic"), 3)
    })

    # Each factor's determinacy with DMAR's guardrail beside it (the lowest
    # correlation two equally consistent sets of factor scores can have,
    # 2 rho^2 - 1) and coefficient H, with their intervals.
    output$quality_tbl <- renderDT({
      f <- efa()
      req(f)
      q <- sieve_efa_quality_table(f)
      req(q)
      level <- ci_level(f)
      out <- data.frame(
        Factor = q$Factor,
        Determinacy = sieve_efa_ci_cell(q$Determinacy, q$Determinacy_lower, q$Determinacy_upper),
        Squared = formatC(q$Squared, format = "f", digits = 3),
        Min_r = sieve_efa_ci_cell(q$Min_score_r, q$Min_score_r_lower, q$Min_score_r_upper),
        H = sieve_efa_ci_cell(q$H, q$H_lower, q$H_upper),
        stringsAsFactors = FALSE
      )
      sieve_dt(out,
        rownames = FALSE, selection = "none",
        colnames = c("Factor", sprintf("Determinacy [%s CI]", level), "Squared determinacy (maximal reliability)",
          sprintf("Minimum correlation of equally consistent scores, 2 rho^2 - 1 [%s CI]", level), sprintf("H [%s CI]", level)),
        options = list(dom = "t", pageLength = 25, ordering = FALSE)
      )
    })
    output$quality_note <- renderUI({
      f <- efa()
      req(f)
      div(class = "small text-muted",
        p(class = "mb-1",
          "The ", strong("determinacy"), " of a factor is the correlation of its regression factor score with the factor ",
          "(Grice, 2001); its square is the largest reliability any weighted sum of the items reaches for that factor. ",
          "Beside it, ", strong("2 rho^2 - 1"), " is the lowest correlation two sets of factor scores equally consistent with ",
          "the model can have (Guttman, 1955): zero when the squared determinacy is .50, negative below. ",
          strong("H"), " is Hancock and Mueller's (2001) reliability of the optimally weighted composite from the factor's ",
          "own loadings, with each item's variance from the other factors counted as error (Rodriguez, Reise & Haviland, 2016); ",
          "with one factor it equals the squared determinacy. Limits are formed on the logit scale and mapped back (DMAR). ",
          "No threshold is applied: the values are reported, not judged."
        ),
        if (identical(f$method$estimator, "WLSMV")) p(class = "mb-1",
          "For ordered items both describe the latent responses behind the categories, not composites of the observed scores."),
        if (!any(is.finite(f$factor_quality$determinacy_ci_lower))) p(class = "mb-1",
          "DMAR returned no factor-quality limits for this solution. Read the estimator and analysis notes above for its uncertainty status.")
      )
    })

    # ---- Simple structure (Thurstone) -------------------------------------
    # Quantify how cleanly the rotated solution maps items to factors: the
    # explicit goal of rotation since Thurstone (1947). The summary metrics
    # (Hofmann's item complexity, 1 = loads on a single factor; the proportion
    # of near-zero loadings, Thurstone's hyperplane; the count of pure vs
    # complex items at the chosen salience cutoff) come from
    # DMAR::simple_structure, the canonical implementation; per-item salient
    # counts are computed here for the item-level table it does not carry.
    ss_metrics <- reactive({
      f <- efa()
      req(f)
      P <- unclass(f$loadings)
      cutoff <- input$suppress %||% 0.30
      if (is.na(cutoff)) cutoff <- 0.30
      ss <- tryCatch(
        DMAR::simple_structure(P, salient = cutoff, hyperplane = 0.10),
        error = function(e) NULL
      )
      req(ss)
      term_val <- function(nm) {
        v <- ss$value[ss$term == nm]
        if (length(v) == 1L && is.finite(v)) v else NA_real_
      }
      cmplx <- attr(ss, "complexity")
      salient <- rowSums(abs(P) >= cutoff)
      sieve_log(
        state,
        sprintf("EFA: simple structure metrics at salience cutoff %.2f (DMAR::simple_structure).", cutoff),
        c(
          "loadings <- pattern  # the standardized pattern matrix read from efa_fit above",
          sprintf("DMAR::simple_structure(loadings, salient = %.2f, hyperplane = 0.10)", cutoff)
        )
      )
      code_view <- sieve_code_snapshot(state, context = c(
        sprintf("Simple structure of the displayed DMAR solution; salience cutoff = %.2f; hyperplane cutoff = .10.", cutoff),
        "The loading model is documented in Show code above. This step reads its unrounded pattern matrix."
      ))
      list(
        code_view = code_view,
        P = P, cmplx = as.numeric(cmplx), salient = salient,
        k = ncol(P), nitem = nrow(P), cutoff = cutoff,
        mean_cmplx = term_val("mean_complexity"),
        hyperplane = term_val("hyperplane_proportion"),
        pure = as.integer(term_val("n_pure")),
        complex = as.integer(term_val("n_complex")),
        none = sum(salient == 0L)
      )
    })

    output$ss_code <- renderUI({
      result <- tryCatch(ss_metrics(), error = function(e) NULL)
      sieve_code_view(result$code_view, status = if (is.null(result)) "not_run" else "current", ns = session$ns)
    })

    output$ss_note <- renderUI({
      req(efa())
      div(
        class = "small text-muted",
        HTML(paste0(
          "<b>Simple structure</b> is the target of factor rotation (Thurstone, 1947): a solution in ",
          "which each item loads on as few factors as possible and each factor has many near-zero ",
          "loadings, so the factors are interpretable and the assignment of items to factors is ",
          "unambiguous. Thurstone's case for an <b>oblique</b> rotation (here direct oblimin) is ",
          "exactly that real constructs are correlated, and forcing orthogonality distorts simple ",
          "structure. Three quantities below: <b>item complexity</b> (Hofmann's index; 1.0 means the ",
          "item loads on a single factor, larger means it spreads across factors), the <b>hyperplane ",
          "proportion</b> (the share of all loadings that are essentially zero, |loading| &lt; .10; ",
          "higher is cleaner), and the counts of <b>pure</b> versus <b>complex</b> items at the salience ",
          "cutoff set at the left. The metrics are reported without a verdict: no threshold on them ",
          "is a rule for a clean solution. Very Simple Structure (Revelle &amp; Rocklin, 1979) on the ",
          "Dimensionality tab is a complementary, fit-based operationalization of the same idea."
        ))
      )
    })

    output$ss_summary <- renderDT({
      s <- ss_metrics()
      req(s)
      df <- data.frame(
        Metric = c(
          "Mean item complexity (Hofmann)",
          "Items loading on exactly one factor (pure)",
          "Items loading on two or more factors (complex)",
          "Items with no salient loading",
          "Hyperplane proportion (|loading| < .10)",
          "Salience cutoff used"
        ),
        Value = c(
          sprintf("%.2f  (1.0 = each item on one factor)", s$mean_cmplx),
          sprintf("%d of %d", s$pure, s$nitem),
          sprintf("%d of %d", s$complex, s$nitem),
          sprintf("%d of %d", s$none, s$nitem),
          sprintf("%.1f%%", 100 * s$hyperplane),
          sprintf("%.2f", s$cutoff)
        ),
        stringsAsFactors = FALSE
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        colnames = c("", ""), options = list(dom = "t", ordering = FALSE)
      )
    })

    output$ss_items <- renderDT({
      s <- ss_metrics()
      req(s)
      items <- rownames(s$P)
      nm <- state$meta$label[match(items, state$meta$item)]
      nm[is.na(nm)] <- items[is.na(nm)]
      verdict <- ifelse(s$salient == 0L, "none salient",
        ifelse(s$salient == 1L, "pure", "complex")
      )
      df <- data.frame(
        Item = items, ItemName = nm,
        Complexity = s$cmplx,
        Salient.factors = s$salient,
        Status = verdict,
        stringsAsFactors = FALSE
      )
      df <- df[order(-df$Salient.factors, -df$Complexity), ]
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25),
        colnames = c(
          "Variable Name", "Item Name", "Complexity",
          "Salient factors", "Simple structure"
        )
      ) |>
        formatRound("Complexity", 2)
    })

    # Restore external project settings, including a second Open in the same
    # session. Ignore the writes made by the controls themselves. A saved
    # extraction or rotation with no DMAR route is named, and the default
    # in its place waits for Run selected EFA as any restored setting does.
    efa_binding <- sieve_bind_settings(state, "efa_settings",
      settings = function() {
      settings <- list(
        n_factors = input$n_factors, cor_type = input$cor_type,
        fm = input$fm, rotation = input$rotation, estimator = input$estimator,
        fit_scope = "k",
        suppress = input$suppress, sort_loadings = input$sort_loadings,
        show_ci = input$show_ci, count_to = input$count_to, count_criterion = input$count_criterion
      )
      if (is.null(settings$n_factors)) return(NULL)
      settings <- Filter(Negate(is.null), settings)
      settings
      },
      restore = function(saved) {
        for (nm in intersect(names(saved), c("n_factors", "suppress", "count_to"))) {
          updateNumericInput(session, nm, value = saved[[nm]])
        }
        for (nm in intersect(names(saved), c("cor_type", "fm", "rotation", "estimator", "count_criterion"))) {
          shiny::updateSelectInput(session, nm, selected = saved[[nm]])
        }
        for (nm in intersect(names(saved), c("sort_loadings", "show_ci"))) {
          if (!is.null(saved[[nm]])) shiny::updateCheckboxInput(session, nm, value = saved[[nm]])
        }
      }, normalize = function(saved) {
        saved$fit_scope <- "k"
        saved[c("fit_from", "fit_to")] <- NULL
        notes <- list()
        old_fm <- saved$fm
        if (length(old_fm) == 1L && old_fm %in% names(sieve_efa_retired_methods())) {
          notes$fm <- sprintf(
            "The saved project chose %s extraction, which DMAR does not offer; the method is the estimator below until you choose another.",
            sieve_efa_retired_methods()[[old_fm]]
          )
          saved$fm <- "ml"
        }
        # Promax and equamax are not offered, since DMAR does not offer
        # them. A saved rotation the page does not offer opens as direct
        # oblimin when it is oblique (promax among them) and as varimax when
        # it is orthogonal (equamax among them), and the page says so.
        retired_rot <- sieve_efa_retired_rotation(saved$rotation)
        if (!is.null(retired_rot)) {
          notes$rotation <- retired_rot$sentence
          saved$rotation <- retired_rot$rotation
        }
        retired(notes)
        saved
      }, clear_empty = FALSE, require_common = TRUE, check_attributes = TRUE)

    # ---- Number of factors: DMAR::factor_count -------------------------------
    # The comparison of one factor, two, and so on, under the selected
    # solution's method and estimator: each count's fit (from its unrotated
    # solution) and the difference test of the previous count against it, the
    # information criteria with their weights, the reduced eigenvalues (beside
    # the Dimensionality page's parallel analysis reference when it is of the
    # same matrix: one parallel analysis in the application, see
    # sieve_efa_count_pa()), and Velicer's MAP. DMAR applies no cutoff and
    # chooses nothing; the table and the lines under it say what each
    # criterion shows. It always runs in a background R process (several
    # unrotated fits take seconds even for nine items), on its button or the
    # page's Update.
    count_job <- sieve_efa_dmar_job()
    count_tick <- reactiveVal(0L)
    count_kept <- new.env(parent = emptyenv())
    count_kept$key <- NULL
    count_kept$value <- NULL
    count_kept$last_key <- NULL
    count_requested <- reactiveVal(NULL)
    # Bumped when a comparison is kept, so the status (which reads the kept
    # record, not count_run()) follows it.
    count_signal <- reactiveVal(0L)
    # The largest count asked for: the control's value (before the control
    # reports one, the default; a cleared control is refused, never filled in).
    count_max <- reactive({
      v <- input$count_to
      if (is.null(v)) sieve_efa_count_default(ncol(active_x())) else v
    })
    count_problem <- reactive({
      d <- active_x()
      p <- ncol(d)
      if (p < 3L) return("The comparison needs at least three included items.")
      v <- count_max()
      top <- sieve_efa_max_factors(p)
      if (!is.numeric(v) || length(v) != 1L || !is.finite(v) || v < 1 || v != floor(v) || v > top) {
        return(sprintf(
          "Choose a whole number of factors from 1 to %d for the largest count (%d included items identify at most %d); no other count was substituted.",
          top, p, top
        ))
      }
      NULL
    })
    count_key <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      pool <- sieve_pool_revision(state)
      rt <- route()
      matrix_route <- identical(rt$form, "matrix")
      list(
        dataset_id = pool$dataset, data = pool$data, items = pool$items, types = pool$types,
        reverse = pool$reverse, rows = pool$rows,
        fm = method(), estimator = rt$estimator, missing = rt$missing, ordered = rt$ordered,
        cor_type = if (matrix_route) input$cor_type %||% "auto",
        study_basis = if (matrix_route) sieve_study_basis_request(state),
        max_factors = count_max(), criterion = input$count_criterion %||% "BIC", B = 0L
      )
    })
    count_plan <- reactive({
      d <- active_x()
      rt <- route()
      p <- ncol(d)
      max_k <- suppressWarnings(as.integer(count_max()))
      if (!length(max_k) || !is.finite(max_k)) max_k <- sieve_efa_count_default(p)
      basis_wait <- if (identical(rt$form, "matrix")) {
        choice <- input$cor_type %||% "auto"
        if (choice %in% c("auto", "study")) choice <- sieve_study_basis_request(state)
        resolved <- if (choice %in% names(sieve_retention_basis_labels())) choice else {
          sieve_retention_request(choice, unname(item_types()))
        }
        sieve_basis_wait(p, resolved)
      } else {
        0
      }
      seconds <- basis_wait + sieve_efa_count_wait(p, rt$estimator, cases_of(d, rt), max_k, rt$form, B = 0L,
        patterns = sieve_efa_patterns(d, rt))
      list(seconds = seconds, label = sieve_efa_wait_label(seconds), max_factors = max_k, p = p, route = rt)
    })
    request_count <- function() {
      key <- count_key()
      count_requested(key)
      count_job$stopped <- NULL
      count_tick(isolate(count_tick()) + 1L)
    }
    accept_count <- function(record, key, job) {
      value <- if (isTRUE(record$ok) && !is.null(record$value)) {
        table <- record$value
        rt <- job$route
        sieve_log(
          state,
          sprintf(
            "EFA: number of factors comparison by DMAR::factor_count, k = 1 to %d, %s; the parallel analysis reference is the Dimensionality page's (B = 0 here).",
            job$max_factors, rt$label
          ),
          c(
            if (identical(rt$form, "matrix")) sieve_retention_cor_code(job$basis),
            "# The exploratory model at one factor, two, and so on (each unrotated, since the",
            "# fit of a count does not depend on the rotation), with the difference tests, the",
            "# information criteria and their weights, the reduced eigenvalues, and Velicer's MAP.",
            "# No cutoff is applied and no count chosen. B = 0: the parallel analysis reference of",
            "# these eigenvalues is DMAR::parallel_analysis() on the Dimensionality page (its code is",
            "# there); factor_count() would run the same normal reference a second time.",
            sprintf("# conf_level = %s sets the RMSEA intervals, the comparison's only intervals (the RMSEA level, DMAR's default; the page's other intervals are at 95%%).",
              format(job$call$args$conf_level)),
            if (length(job$aux)) paste("#", strwrap(sieve_efa_aux_sentence(job$aux, "factor_count"), width = 88)),
            sieve_efa_dmar_code(job$call, result = "factor_counts"),
            "factor_counts                           # the table with the lines beneath it, as DMAR prints it",
            "attr(factor_counts, \"map\")               # the average squared partial correlation for every m"
          )
        )
        code_view <- sieve_code_snapshot(state, context = c(
          sprintf("Number of factors: DMAR::factor_count (DMAR %s), k = 1 to %d; %s.",
            as.character(utils::packageVersion("DMAR")), job$max_factors, rt$label),
          "Each count is fitted unrotated by DMAR::fa_exploratory; its fit is the fit of every rotation of it."
        ))
        list(status = "ran", table = table, code_view = code_view, route = rt, rng = job$rng,
          basis_used = job$basis$used, aux_ignored = job$aux,
          warnings = record$warnings, when = sieve_clock_text(code_view$recorded_at), key = key)
      } else {
        list(status = "failed", error = sieve_efa_dmar_refusal_text(record), error_class = record$error_class,
          reason = record$reason, key = key)
      }
      count_kept$key <- key
      count_kept$value <- value
      count_kept$last_key <- key
      count_signal(isolate(count_signal()) + 1L)
      value
    }
    count_run <- reactive({
      count_tick()
      key <- count_key()
      if (identical(count_kept$key, key)) return(count_kept$value)
      why <- count_problem()
      if (!is.null(why)) return(list(status = "refused", error = why, key = key))
      plan <- count_plan()
      # A comparison the machine or the sample cannot carry is refused
      # before anything starts, with the reason and the ways forward.
      guard <- sieve_efa_dmar_guard(plan$route, active_x(), plan$max_factors)
      if (!is.null(guard)) return(list(status = "refused", key = key, error = guard))
      if (identical(count_job$record_key, key) && !is.null(count_job$record)) {
        record <- count_job$record
        count_job$record <- NULL
        count_job$record_key <- NULL
        return(accept_count(record, key, count_job$meta))
      }
      if (sieve_efa_dmar_active(count_job, key)) {
        return(list(status = "running", key = key, plan = plan))
      }
      if (!identical(count_requested(), key) || identical(count_job$stopped, key)) {
        before <- count_kept$last_key
        stale <- !is.null(before) && identical(before$dataset_id, key$dataset_id)
        return(list(status = "not run", key = key, plan = plan, stale = stale,
          stopped = identical(count_job$stopped, key)))
      }
      d <- active_x()
      rt <- plan$route
      b <- NULL
      if (identical(rt$form, "matrix")) {
        b <- sieve_basis_demand(state, input$cor_type %||% "auto", asked = TRUE)
        req(b)
        if (!isTRUE(b$ok)) {
          return(list(status = "refused", key = key,
            error = sprintf("No correlation matrix could be computed for the included items. %s", b$reason)))
        }
      }
      # Nothing in the comparison is random with B = 0, so no seed is drawn.
      rng <- NULL
      call <- sieve_efa_count_call(rt, plan$max_factors, data = d, basis = b,
        criterion = key$criterion, B = key$B, seed = NULL)
      count_job$meta <- list(route = rt, basis = b, rng = rng, call = call, max_factors = plan$max_factors,
        aux = aux_ignored(rt, d))
      sieve_efa_dmar_submit(count_job, key, call, seconds = plan$seconds, rng_kind = rng$kind)
      if (identical(count_job$record_key, key) && !is.null(count_job$record)) {
        record <- count_job$record
        count_job$record <- NULL
        count_job$record_key <- NULL
        return(accept_count(record, key, count_job$meta))
      }
      list(status = "running", key = key, plan = plan)
    })
    count_status <- reactive({
      count_tick()
      count_signal()
      read <- sieve_outcome(count_key())
      if (!read$ready) return(list(state = "not_run", sentence = "Confirm item preparation before comparing numbers of factors."))
      if (!read$ok) return(list(state = "failed", sentence = read$error))
      key <- read$value
      if (sieve_efa_dmar_active(count_job, key)) {
        return(list(state = "running", key = key, sentence = "The number of factors comparison is running in a background R process."))
      }
      if (identical(count_kept$key, key)) {
        failed <- !identical(count_kept$value$status, "ran")
        return(list(state = if (failed) "failed" else "current", key = key,
          sentence = if (failed) count_kept$value$error else "The number of factors comparison matches the current data and settings."))
      }
      before <- count_kept$last_key
      if (!is.null(before) && identical(before$dataset_id, key$dataset_id)) {
        return(list(state = "stale", key = key, sentence = paste(
          "The data, the item pool or the settings changed since the number of factors comparison was computed;",
          "it is withheld until it is computed again."
        )))
      }
      list(state = "not_run", key = key, sentence = "The number of factors comparison has not been computed.")
    })
    observe({
      status <- count_status()
      sieve_page_publish(state, "efa", session$ns("factor_count"), "Number of factors comparison (DMAR::factor_count)", status,
        action = switch(status$state, current = , running = NULL, stale = "Update", "Compare numbers of factors"),
        own = TRUE)
    })
    observeEvent(input$count_go, request_count())
    observeEvent(input$count_go_global, request_count())
    cancel_count <- function() {
      inclusion_wire$cancel()
      sieve_efa_dmar_cancel(count_job)
      count_tick(isolate(count_tick()) + 1L)
    }
    observeEvent(input$count_cancel, cancel_count())
    observeEvent(input$count_cancel_global, cancel_count())
    # The elapsed time of a running comparison, in its panel's line and in
    # the page's status line, redrawn each second while it runs.
    sieve_progress_outputs(output, c("count_progress", "count_progress_line"), function() {
      count_tick()
      if (sieve_efa_dmar_active(count_job)) sieve_efa_dmar_progress(count_job)$sentence
    }, session)

    output$count_note <- renderUI({
      r <- tryCatch(count_run(), error = function(e) NULL)
      if (is.null(r)) return(NULL)
      if (identical(r$status, "refused") || identical(r$status, "failed")) {
        return(div(class = "alert alert-danger small", strong("The comparison was not computed. "), r$error))
      }
      if (identical(r$status, "running")) {
        # Drawn once per run with its Cancel button; only the elapsed time
        # is redrawn (count_progress below).
        return(div(class = "alert alert-info small",
          sprintf("Running in a background R process: k = 1 to %d under %s. ", r$plan$max_factors, r$plan$route$label),
          sieve_progress_text(session$ns("count_progress")), " ",
          actionButton(session$ns("count_cancel"), "Cancel the comparison", class = "btn-sm")
        ))
      }
      if (identical(r$status, "not run")) {
        plan <- r$plan
        return(div(class = "alert alert-info small",
          strong(if (isTRUE(r$stopped)) "Canceled. " else if (isTRUE(r$stale)) "The earlier comparison is from before an edit. " else "Not computed yet. "),
          sprintf(
            paste0(
              "Fitting the exploratory model at k = 1 to %d under %s on %d items, with the reduced eigenvalues ",
              "and Velicer's MAP, is estimated to take %s, in a background R process. ",
              "Nothing is chosen for you: the table sets the criteria side by side."
            ),
            plan$max_factors, plan$route$label, plan$p, plan$label
          ),
          div(class = "mt-2", actionButton(session$ns("count_go"),
            sprintf("Compare numbers of factors (%s)", plan$label), class = "btn-primary btn-sm"))
        ))
      }
      t <- r$table
      m <- attr(t, "method", exact = TRUE)
      div(class = "text-muted small mb-1",
        sprintf(
          paste0(
            "Each row is the exploratory model with that many factors, fit unrotated by DMAR under %s (N = %s); ",
            "the fit is the same for every rotation. Computed at %s."
          ),
          r$route$label, format(m$N), r$when %||% "an unrecorded time"
        )
      )
    })
    # The Dimensionality page's parallel analysis, when it is of the same
    # matrix (one parallel analysis in the application).
    count_pa <- reactive({
      r <- count_run()
      req(identical(r$status, "ran"))
      sieve_efa_count_pa(r$table, state$cache$pa, sieve_pool_revision(state),
        basis_used = if (identical(r$route$form, "matrix")) r$basis_used)
    })
    count_display <- reactive({
      r <- count_run()
      req(identical(r$status, "ran"))
      t <- r$table
      class(t) <- "data.frame"
      pa <- count_pa()
      t$reference_quantile <- if (isTRUE(pa$ok)) pa$reference else NA_real_
      t$exceeds_reference <- if (isTRUE(pa$ok)) pa$exceeds else NA
      columns <- c(
        factors = "Factors (k)", solution = "Solution", chi_square = "Chi-square", df = "df", p_value = "p",
        rmsea = "RMSEA", rmsea_ci_lower = "RMSEA 95% CI lower", rmsea_ci_upper = "RMSEA 95% CI upper",
        cfi = "CFI", tli = "TLI", srmr = "SRMR", aic = "AIC", bic = "BIC", sabic = "SABIC",
        criterion_delta = sprintf("%s difference from the lowest", attr(r$table, "method")$criterion),
        criterion_weight = sprintf("%s weight", attr(r$table, "method")$criterion),
        p_delta = "p of the difference test against k - 1",
        eigenvalue = "Reduced eigenvalue",
        reference_quantile = sprintf("Parallel analysis reference (Dimensionality page, %s)",
          if (identical(count_pa()$statistic, "mean")) "mean" else "95th percentile"),
        exceeds_reference = "Exceeds its reference", map = "MAP (average squared partial)"
      )
      keep <- names(columns)[vapply(names(columns), function(nm) {
        nm %in% c("factors", "solution") || (nm %in% names(t) && any(!is.na(t[[nm]])))
      }, logical(1))]
      level <- attr(r$table, "conf_level") %||% sieve_default_rmsea_conf_level()
      labels <- columns[keep]
      labels <- sub("95%", sieve_ci_percent(level), labels, fixed = TRUE)
      list(data = t[, keep, drop = FALSE], labels = unname(labels), table = r$table)
    })
    output$count_tbl <- renderDT({
      x <- tryCatch(count_display(), error = function(e) NULL)
      if (is.null(x)) {
        return(sieve_dt(data.frame(Note = "Press Compare numbers of factors above to fit the counts."),
          rownames = FALSE, selection = "none", options = list(dom = "t")))
      }
      df <- x$data
      if ("exceeds_reference" %in% names(df)) df$exceeds_reference <- ifelse(is.na(df$exceeds_reference), "", ifelse(df$exceeds_reference, "yes", "no"))
      dt <- sieve_dt(df, rownames = FALSE, selection = "none",
        colnames = x$labels, options = list(dom = "t", pageLength = 50, scrollX = TRUE, ordering = FALSE))
      three <- intersect(names(df), c("rmsea", "rmsea_ci_lower", "rmsea_ci_upper", "cfi", "tli", "srmr",
        "criterion_weight", "eigenvalue", "reference_quantile", "map", "p_value", "p_delta"))
      one <- intersect(names(df), c("chi_square", "aic", "bic", "sabic", "criterion_delta"))
      if (length(three)) dt <- formatRound(dt, three, 3)
      if (length(one)) dt <- formatRound(dt, one, 2)
      marks <- unique(c(state$theory$k, suppressWarnings(as.integer(k_asked()))))
      marks <- marks[is.finite(marks)]
      if (length(marks)) {
        dt <- formatStyle(dt, "factors", target = "row",
          fontWeight = styleEqual(marks, rep("bold", length(marks))),
          backgroundColor = styleEqual(marks, rep("#fdf3d6", length(marks))))
      }
      dt
    })
    output$count_lines <- renderUI({
      x <- tryCatch(count_display(), error = function(e) NULL)
      req(x)
      t <- x$table
      class(t) <- "data.frame"
      m <- attr(x$table, "method", exact = TRUE)
      proper <- t$solution == "proper"
      lowest <- function(column) {
        v <- t[[column]]
        ok <- proper & is.finite(v)
        if (!any(ok)) return(NULL)
        sprintf("%s at k = %d", toupper(column), t$factors[ok][which.min(v[ok])])
      }
      ics <- Filter(Negate(is.null), lapply(c("aic", "bic", "sabic"), lowest))
      best <- if (any(is.finite(t$criterion_weight))) {
        i <- which.max(t$criterion_weight)
        sprintf("By %s, k = %d has the largest weight among the proper counts (%.3f, conditional on the counts compared).",
          m$criterion, t$factors[i], t$criterion_weight[i])
      }
      shared <- tryCatch(count_pa(), error = function(e) NULL)
      basis_label <- sieve_retention_basis_label(shared$shared$basis_used %||% "polychoric")
      pa <- if (isTRUE(shared$ok)) {
        k_pa <- shared$suggested
        sprintf(
          paste0(
            "Parallel analysis (the Dimensionality page's DMAR::parallel_analysis, the same reduction of the same ",
            "matrix, seed %d): the first %s reduced eigenvalue%s exceed%s the %s (Horn, 1965); the reference is a ",
            "null threshold, not an interval. It is not computed a second time here."
          ),
          as.integer(shared$seed), fmt_k(k_pa), if (identical(as.integer(k_pa), 1L)) "" else "s",
          if (identical(as.integer(k_pa), 1L)) "s" else "", shared$label
        )
      } else {
        paste0(
          "Parallel analysis: the reference for these reduced eigenvalues is the Dimensionality page's ",
          "(DMAR::parallel_analysis, the reference DMAR::factor_count() would compute a second time), ",
          switch(shared$why %||% "none",
            stale = "and the run there is of an earlier item pool; update it there to read it here.",
            other = sprintf(
              "and the run there is of another correlation matrix (the %s); set the Dimensionality page's correlations to this route's to read it here.",
              shared$shared$matrix_words %||% "page's matrix"
            ),
            estimator = sprintf(
              paste0(
                "and its %s correlations are DMAR's two-step estimate from the item responses (lavaan::lavCor), ",
                "where minimum residual on a correlation matrix here reads the page's %s matrix (psych's estimate); ",
                "the two differ by about 1e-5 (here %s in the reduced eigenvalues), so these are not the same ",
                "reduction and the reference is not read. No setting of the Dimensionality page changes this; ",
                "the Method \"The estimator below\" (WLSMV on the ordered item responses) reduces DMAR's matrix and reads it."
              ),
              basis_label, basis_label,
              if (is.finite(shared$gap %||% NA_real_)) format(signif(shared$gap, 2)) else "a small amount"
            ),
            "and it has not run for these data; run it there to read it here."
          )
        )
      }
      map <- if (is.finite(m$map_minimum %||% NA_real_)) {
        sprintf("Velicer's MAP is smallest after %d component%s (a rule about components, reported beside the factor models, not in place of them).",
          m$map_minimum, if (m$map_minimum == 1L) "" else "s")
      }
      reasons <- attr(x$table, "reasons", exact = TRUE)
      improper <- which(!proper)
      none <- if (length(improper)) {
        sprintf("No proper solution: %s. Such a count is informative (the unrestricted model drives a residual variance to or below zero), not a stopping rule.",
          paste(sprintf("k = %d (%s)", t$factors[improper], ifelse(is.na(reasons[improper]), t$solution[improper], reasons[improper])), collapse = "; "))
      }
      aux_note <- sieve_efa_aux_sentence(tryCatch(count_run()$aux_ignored, error = function(e) NULL), "factor_count")
      div(class = "small mt-2",
        if (length(aux_note)) p(class = "mb-1", strong("Auxiliary variables: "), aux_note),
        if (length(ics)) p(class = "mb-1", strong("Lowest information criteria among the proper counts: "), paste0(paste(ics, collapse = "; "), ".")),
        if (!is.null(best)) p(class = "mb-1", best),
        if (!is.null(pa)) p(class = "mb-1", pa),
        if (!is.null(map)) p(class = "mb-1", map),
        if (!is.null(none)) p(class = "mb-1", none),
        if (length(m$notes)) p(class = "mb-1 text-muted", paste(m$notes, collapse = " ")),
        p(class = "mb-1 text-muted", attr(x$table, "analysis_note", exact = TRUE)),
        if (!identical(m$estimator, "ULS")) p(class = "mb-1 text-muted", paste(
          "Hu and Bentler (1999), from simulations of maximum likelihood estimates, proposed values close to .95",
          "for CFI and TLI, close to .06 for RMSEA, and close to .08 for SRMR; none of them is a rule that makes a",
          "model acceptable or not, and the models and conditions they simulated limit how far they generalize",
          "(Marsh, Hau & Wen, 2004). Reference values are shown for orientation. Interpret the selected estimator,",
          "the degrees of freedom, the residuals, and the RMSEA interval together (Hu & Bentler, 1999;",
          "Marsh, Hau & Wen, 2004; Kenny, Kaniskan & McCoach, 2015)."
        )),
        p(class = "mb-1 text-muted", paste(
          "The selected and the theorized counts are in bold. Read the criteria together with theory and the",
          "interpretability of the rotated solutions; the RMSEA intervals are at",
          paste0(sieve_ci_percent(m$conf_level %||% attr(x$table, "conf_level", exact = TRUE) %||% sieve_default_rmsea_conf_level()), ","),
          "the level of every RMSEA interval SIEVER shows (90% by default, DMAR's and lavaan's, whose limits",
          "settle the tests of close and not close fit at .05; MacCallum, Browne & Sugawara, 1996); the page's",
          "other intervals are at 95%."
        ))
      )
    })
    output$count_code <- renderUI({
      r <- tryCatch(count_run(), error = function(e) NULL)
      sieve_code_view(if (identical(r$status, "ran")) r$code_view,
        status = if (identical(r$status, "ran")) "current" else "not_run",
        title = "Show code: number of factors comparison", ns = session$ns
      )
    })

    # The background processes: polled while they run; a finished one wakes
    # the page, which accepts its result only for the request it was
    # started for. A changed request stops a process fitting the old one.
    observe({
      primary_tick()
      count_tick()
      busy <- FALSE
      if (sieve_efa_dmar_active(primary_job)) {
        key <- tryCatch(primary_key(), error = function(e) NULL)
        if (!identical(key, primary_job$key)) {
          sieve_efa_dmar_cancel(primary_job, remember = FALSE)
          primary_tick(isolate(primary_tick()) + 1L)
        } else if (sieve_efa_dmar_poll(primary_job)) {
          primary_tick(isolate(primary_tick()) + 1L)
          # Read the finished result now, so the page's cue is right even
          # while the page is hidden.
          isolate(tryCatch(efa_run(), error = function(e) NULL))
        } else {
          busy <- TRUE
        }
      }
      if (sieve_efa_dmar_active(count_job)) {
        key <- tryCatch(count_key(), error = function(e) NULL)
        if (!identical(key, count_job$key)) {
          sieve_efa_dmar_cancel(count_job, remember = FALSE)
          count_tick(isolate(count_tick()) + 1L)
        } else if (sieve_efa_dmar_poll(count_job)) {
          count_tick(isolate(count_tick()) + 1L)
          isolate(tryCatch(count_run(), error = function(e) NULL))
        } else {
          busy <- TRUE
        }
      }
      if (busy) {
        shiny::invalidateLater(250, session)
      } else if (isTRUE(update_watch$primary) || isTRUE(update_watch$fit)) {
        # The Update that started them announces when they are done.
        update_watch$primary <- FALSE
        update_watch$fit <- FALSE
        sieve_notify("The EFA page's background computations finished; see the status line for each result.")
      }
    })
    session$onSessionEnded(function() {
      sieve_efa_dmar_cancel(primary_job, remember = FALSE)
      sieve_efa_dmar_cancel(count_job, remember = FALSE)
    })
  })
}

# Format a fit index for a sentence: three decimals, or "not available".
fmt_fit <- function(v) {
  if (length(v) != 1L || !is.finite(v)) "not available" else sprintf("%.3f", v)
}

#' An extraction method in words
#'
#' The EFA page's methods (`sieve_efa_method_choices()`), and the psych
#' `fm` codes a saved project or the Items tab's model may name.
#'
#' @param fm A method code.
#' @return The method's name.
#' @author Ken Kelley
#' @noRd
sieve_efa_extraction_label <- function(fm) {
  out <- unname(c(
    ml = "maximum likelihood", minres = "minimum residual (minres, ULS)",
    minres_matrix = "minimum residual (minres, ULS) on a correlation matrix",
    pa = "principal axis factoring", wls = "weighted least squares",
    gls = "generalized least squares"
  )[fm])
  ifelse(is.na(out), fm, out)
}

#' The extraction methods the EFA tab offers
#'
#' An alias of `sieve_efa_method_choices()`. Every one is a common factor
#' method; principal components are not offered.
#'
#' @return A named character vector: labels as names, codes as values.
#' @author Ken Kelley
#' @noRd
sieve_efa_extraction_choices <- function() {
  sieve_efa_method_choices()
}

#' The R that replays exploratory fit indices by number of factors
#'
#' One lavaan::efa solution per number of factors asked for, with the
#' estimator, the ordered items and the missing data treatment of the run
#' (the Dimensionality page's model fit by k), and the fit indices of the family shown.
#'
#' @param key The key of the run: `ks`, `estimator`, `ordered`, `missing`,
#'   `rotation`, and `label`.
#' @param family The family of CFI, TLI and RMSEA the tab shows (`"robust"`,
#'   `"scaled"`, `"standard"`, or `NA`).
#' @param data_name The name of the included item data in the script.
#' @param conf_level The level of the RMSEA intervals (the table's,
#'   `sieve_efa_fit_by_k()`'s `conf_level`), passed to lavaan as
#'   `rmsea.ci.level`.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_efa_fit_code <- function(key, family = NA_character_, data_name = "active_items",
                               conf_level = sieve_default_rmsea_conf_level()) {
  ks <- as.integer(key$ks)
  ks_code <- if (length(ks) == 1L) {
    sprintf("%d", ks)
  } else if (identical(ks, seq.int(ks[1], ks[length(ks)]))) {
    sprintf("%d:%d", ks[1], ks[length(ks)])
  } else {
    sprintf("c(%s)", paste(ks, collapse = ", "))
  }
  suffix <- switch(family %||% NA_character_,
    robust = ".robust",
    scaled = ".scaled",
    ""
  )
  if (is.na(suffix)) suffix <- ""
  idx <- c(paste0(c("cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"), suffix), sieve_srmr_measure(), "bic")
  ordered <- if (length(key$ordered)) {
    sprintf(", ordered = c(%s)", paste(encodeString(key$ordered, quote = "\""), collapse = ", "))
  } else {
    ""
  }
  missing <- switch(key$lavaan_missing %||% "default",
    ml = ", missing = \"ml\"",
    listwise = ", missing = \"listwise\"",
    ""
  )
  c(
    sprintf(
      "# Fit indices of the separate exploratory model%s (lavaan::efa), %s; CFI, TLI and RMSEA in the %s family.",
      if (length(ks) == 1L) "" else "s", key$label %||% key$estimator,
      if (is.na(family %||% NA_character_)) "(no common)" else family
    ),
    sprintf("X <- %s", data_name),
    sprintf("efa_fit_indices <- sapply(stats::setNames(%s, paste(\"k =\", %s)), function(k) {", ks_code, ks_code),
    sprintf(
      "  f <- lavaan::efa(data = X, nfactors = k, rotation = \"%s\", estimator = \"%s\"%s%s)",
      key$rotation, key$estimator, ordered, missing
    ),
    "  if (inherits(f, \"efaList\")) f <- f[[1]]",
    if (is.na(family %||% NA_character_)) {
      "  c(cfi = NA_real_, tli = NA_real_, rmsea = NA_real_, rmsea.ci.lower = NA_real_, rmsea.ci.upper = NA_real_, lavaan::fitMeasures(f, c(\"srmr_bentler_nomean\", \"bic\")))"
    } else {
      c(sprintf("  m <- lavaan::fitMeasures(f, c(%s), fm.args = list(rmsea.ci.level = %s))",
        paste(sprintf("\"%s\"", idx), collapse = ", "), sieve_num_lit(conf_level)),
        sieve_zero_df_mask_code(), "  m")
    },
    "})",
    "efa_fit_indices"
  )
}

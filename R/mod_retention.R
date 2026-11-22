# mod_retention.R
# Factor retention and dimensionality. The tab presents the evidence on the
# number of factors criterion by criterion and records the researcher's
# decision; it does not choose a number. Every criterion is shown with what
# it suggests, the correlations it was actually computed from (next to the
# ones requested), whether it ran, and the engine's reason when it did not.
# Disagreement among the criteria is reported as a finding. A tally of the
# criteria and the plus or minus one neighborhood are shown as the
# heuristics they are. The theorized number of factors (state$theory$k) is
# overlaid as a reference line and never overrides the evidence.
#
# What is estimated is factor analytic (maximum likelihood; direct oblimin
# where a rotation is needed). Every criterion uses common factors. The solid gray
# line at 1 on the scree plot is for orientation only.
#
# The computations live in fct_retention.R as plain functions; this module
# displays their results, writes the R that replays them, and holds the
# control that records the decision (fct_decisions.R).

#' Factor retention module UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput plotOutput numericInput helpText p tags HTML tagList
#' @importFrom shiny selectInput textAreaInput actionButton downloadButton
#' @importFrom bslib layout_sidebar sidebar card card_header card_body navset_card_tab nav_panel layout_columns
#' @importFrom DT DTOutput
mod_retention_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    # The main area is a document that scrolls, not a fillable column: in
    # a fillable column at 1440 by 900 the evidence and decision card is
    # squeezed to 32 pixels of its 432 (scrolled inside), and the panels
    # take the rest.
    fillable = FALSE,
    sidebar = sidebar(
      open = "desktop",
      width = 300,
      title = "Retention settings",
      selectInput(
        ns("cor_type"), "Correlations",
        choices = sieve_basis_choices(),
        selected = "auto"
      ),
      helpText("The correlation matrix parallel analysis, VSS and the scree plot read."),
      uiOutput(ns("cor_source")),
      sieve_settings("Parallel Analysis Settings",
        ids = ns(c("criterion", "pa_reference", "n_iter")),
      selectInput(
        ns("criterion"), "Parallel analysis reference value",
        choices = c(
          "Mean of the reference eigenvalues" = "mean",
          "95th percentile (usually more conservative)" = "p95"
        ),
        selected = "p95"
      ),
      selectInput(
        ns("pa_reference"), "Parallel analysis reference data",
        choices = c(
          "Normal data, DMAR's default (categorized for ordered items)" = "simulated",
          "Auto (resampled responses if any item is ordered or binary)" = "auto",
          "Resampled item responses" = "resampled"
        ),
        # DMAR::parallel_analysis()'s defaults: reference = "normal", B =
        # 1000, percentile = 0.95.
        selected = "simulated"
      ),
      numericInput(ns("n_iter"), "Parallel analysis reference datasets", value = 1000, min = 200, max = 10000, step = 10),
      helpText("DMAR requires at least 200 requested datasets for its 95th-percentile reference limits. Counts are used as entered; invalid requests are explained before a run.")
      ),
      numericInput(ns("max_k"), "Maximum factors to probe", value = 8, min = 2, max = 20, step = 1),
      numericInput(ns("workers"), "Workers (model fit by k)",
        value = sieve_cores(), min = 1L,
        max = sieve_cores(max = 64L), step = 1L
      ),
      helpText(sieve_workers_help()),
      numericInput(ns("seed"), "Seed (optional)", value = NULL, step = 1),
      helpText(
        "The settings above are starting points, not rules. The study's setting follows ",
        "the Ingest tab: the confirmed item types (Pearson correlations for continuous ",
        "items, polychoric or tetrachoric for ordered or binary items, mixed correlations ",
        "when the pool holds both kinds), or the treatment its Estimator control chose ",
        "(Pearson for items treated as continuous, polychoric for items treated as ",
        "ordered). Pearson or polychoric chosen here applies to this tab only. If the ",
        "requested correlations cannot be computed, Pearson correlations are used ",
        "instead and every affected result says so, with the reason. The reference ",
        "value and the reference data are read from one set of random draws, so ",
        "changing either is instant. The parallel analysis reference and the rotation ",
        "starts behind Very Simple Structure are random: they run from the seed you ",
        "enter, or from one drawn for the run and shown with the results, and the ",
        "reproducible code replays them exactly. Parallel analysis is ",
        "DMAR::parallel_analysis(), one call for each kind of reference data; DMAR ",
        "reports the Monte Carlo limits of the reference percentile, so more reference ",
        "datasets give a more precise reference. A first run for the data runs ",
        "automatically only when its estimated wait is two seconds or less; otherwise, and ",
        "after an edit that changes the included items, the Scree panel and the evidence ",
        "table show the estimate and a Run parallel analysis button. A run estimated above ",
        "two seconds goes to a background R process, so the page stays usable; the status ",
        "line shows its elapsed time against the estimate, and Cancel stops it (a canceled ",
        "run keeps nothing). With continuous items (Pearson correlations) a parallel ",
        "analysis takes seconds; with ordered items the polychoric correlations of every ",
        "reference dataset are estimated, about 0.03 to 0.2 seconds each on 6 to 24 items."
      )
    ),
    # Results first: what blocks the page or the one status line with its
    # Update, the one-line method statement, then the evidence: the summary
    # beside the decision it informs (the number of factors is the one thing
    # the tab is for, so the control sits beside the summary, large enough to
    # find), and the criteria table. The plots and the fit by k follow; the
    # explanations are below them.
    div(
      class = "sieve-retention-top",
      uiOutput(ns("gate")),
      uiOutput(ns("status_line")),
      uiOutput(ns("method_line"))
    ),
    tags$style(HTML(paste0(
      ".sieve-decide-k input.form-control { font-size: 1.75rem; font-weight: 600; ",
      "height: auto; padding: 0.25rem 0.75rem; }",
      ".sieve-decide-k label { font-weight: 600; }"
    ))),
    card(
      class = "sieve-decision-card mb-2",
      card_header("The evidence on the number of factors, and your decision"),
      card_body(
        fillable = FALSE,
        layout_columns(
          col_widths = c(6, 6),
          div(
            tags$h6("What the evidence shows"),
            uiOutput(ns("decide_evidence"))
          ),
          div(
            uiOutput(ns("optional_note")),
            div(
              class = "sieve-decide-k",
              numericInput(ns("decide_k"), "Number of factors retained",
                value = NULL, min = 0, step = 1,
                width = "220px"
              )
            ),
            textAreaInput(ns("decide_why"), "Rationale (optional)",
              value = "", rows = 2,
              width = "100%",
              placeholder = "Why this number, in your own words (optional)."
            ),
            div(class = "mt-1", actionButton(ns("decide_go"), "Record decision", class = "btn-primary")),
            uiOutput(ns("decide_bounds")),
            uiOutput(ns("decide_status"))
          )
        ),
        tags$h6(class = "mt-2", "Evidence by criterion"),
        uiOutput(ns("context_line")),
        uiOutput(ns("summary_tbl")),
        uiOutput(ns("summary_reco")),
        uiOutput(ns("summary_note"))
      )
    ),
    navset_card_tab(
      # The bodies below are not fillable: the notes, the plot, and the
      # table lie in ordinary document flow and the card scrolls when they
      # do not fit. In a fillable body they compete for one flex column and
      # a long note squeezes the table out of sight.
      nav_panel(
        "Scree and Parallel Analysis",
        card(
          card_header("Eigenvalues of the correlation matrix, with the Kaiser criterion at 1 as a reference"),
          card_body(
            fillable = FALSE,
            uiOutput(ns("scree_control")),
            sieve_plot_save_ui(ns("scree_plot")),
            plotOutput(ns("scree_plot"), height = "440px"),
            uiOutput(ns("scree_plot_note")),
            uiOutput(ns("scree_code"))
          )
        ),
        card(
          card_header("Factor-analytic eigenvalues with Horn's parallel analysis"),
          card_body(
            fillable = FALSE,
            uiOutput(ns("pa_control")),
            uiOutput(ns("pa_window")),
            sieve_plot_save_ui(ns("scree")),
            plotOutput(ns("scree"), height = "460px"),
            uiOutput(ns("scree_note")),
            uiOutput(ns("pa_code"))
          )
        )
      ),

      nav_panel(
        "VSS and Decisions",
        card(
          card_header("Very Simple Structure"),
          card_body(
            fillable = FALSE,
            DTOutput(ns("mapvss_tbl")),
            uiOutput(ns("mapvss_note")),
            uiOutput(ns("vss_code"))
          )
        ),
        card(
          card_header("Your decision records"),
          card_body(
            fillable = FALSE,
            p(
              class = "small",
              "The number of factors is your decision, entered at the top of the tab. ",
              "A rationale is optional: what you weighed, where the criteria disagreed and ",
              "how you resolved it, and what the theorized structure and the ",
              "interpretability of the competing solutions contributed. Each record keeps ",
              "the choice, the rationale (or that none was given), what the evidence table ",
              "showed at the time, and the data and item pool in force, and is written to ",
              "the reproducible code. Recording a decision changes no setting elsewhere. A ",
              "later decision does not erase an earlier one: it is added as a new record ",
              "that names the one it replaces."
            ),
            DTOutput(ns("decide_tbl")),
            downloadButton(ns("decisions_csv"), "Download all decision records (.csv)",
              class = "btn-sm mt-2"
            )
          )
        )
      ),
      nav_panel(
        "Model fit by k",
        card(
          card_header("Exploratory factor model fit across the number of factors (lavaan::efa)"),
          card_body(
            fillable = FALSE,
            uiOutput(ns("fit_control")),
            sieve_plot_save_ui(ns("fit_plot")),
            plotOutput(ns("fit_plot"), height = "380px"),
            DTOutput(ns("fit_tbl")),
            uiOutput(ns("fit_note")),
            # The panel's own setting, closed: the estimator in force is
            # named in the note above and in the plot's subtitle.
            tags$details(
              class = "mt-2",
              tags$summary("Settings: the estimator for the model fit by k"),
              div(
                style = "max-width: 640px;",
                sieve_estimator_input(ns("estimator"), "Estimator for the model fit by k"),
                helpText(paste0(
                  "The estimator lavaan::efa fits the exploratory model with at each k: the study's ",
                  "setting (Ingest tab; ML with FIML for continuous items unless set otherwise there), ",
                  "ML with FIML, MLR with FIML (for markedly nonnormal items) or ML on the complete cases ",
                  "(items treated as continuous), or WLSMV on polychoric correlations (items treated as ordered)."
                )),
                helpText(paste0(
                  "A first sequence for the data estimated at two seconds or less runs by itself in this ",
                  "R process. A longer one, or one after an edit, waits for Run the model fit by k (or the ",
                  "page's Update) and then runs in background R ",
                  "processes, split over the Workers set at the left (fewer when one model needs a lot ",
                  "of memory), while the page stays usable; each worker shares the sample statistics ",
                  "and the baseline model across its numbers of factors, and Cancel stops every worker. ",
                  "A solution the EFA tab has fit for the same data, items, estimator and missing data ",
                  "rule is read, not fit again, and the EFA tab reads the solutions fit here."
                )),
                uiOutput(ns("estimator_source"))
              )
            ),
            uiOutput(ns("fit_code"))
          )
        )
      )
    ),
    # Below the results: how to read the criteria (the first paragraph
    # visible, the rest under a closed label).
    div(
      class = "mt-2 small border-top pt-2",
      uiOutput(ns("overview"))
    )
  )
}

#' Factor retention module server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive req renderUI renderPlot div tags HTML tagList p observe
#' @importFrom shiny observeEvent reactiveVal validate need downloadHandler isolate
#' @importFrom DT renderDT datatable
#' @importFrom stats cor complete.cases
mod_retention_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    # Each disclosure reads only a saved record. It never evaluates an
    # analysis reactive, draws a seed, or starts a worker to obtain code.
    sieve_code_navigation(input, session)
    code_records <- reactiveVal(list())
    code_settings <- function(section) {
      common <- list(cor_type = input$cor_type %||% "auto",
                     study_estimator = state$estimator_choice %||% "auto")
      switch(section,
        scree = c(common, list(nfactors = 1L)),
        pa = c(common, list(
          n_iter = sieve_finite_or(input$n_iter, 1000L), seed = input$seed,
          criterion = input$criterion %||% "p95", reference = input$pa_reference %||% "simulated"
        )),
        vss = c(common, list(max_k = sieve_finite_or(input$max_k, 8L), seed = input$seed)),
        fit = fit_key()
      )
    }
    remember_code <- function(section, context, correlation_basis = NULL, model_data = FALSE) {
      record <- sieve_code_snapshot(state, context = context)
      its <- names(shiny::isolate(x_data()))
      preparation <- c(
        sprintf("retention_items <- c(%s)", paste(encodeString(its, quote = '"'), collapse = ", ")),
        "active_items <- items_data[, retention_items, drop = FALSE]",
        "active_items[] <- lapply(active_items, function(v) suppressWarnings(as.numeric(unclass(v))))",
        if (isTRUE(model_data))
          "active_items <- active_items[rowSums(!is.na(active_items)) > 0L, , drop = FALSE]",
        if (!is.null(correlation_basis)) sieve_retention_cor_code(correlation_basis)
      )
      record$code <- c(preparation, record$code)
      record$revision <- sieve_revision(state)
      # What these criteria read (sieve_pool_revision()): a construct or
      # facet edit is not part of it.
      record$page_revision <- shiny::isolate(sieve_pool_revision(state))
      record$settings <- shiny::isolate(code_settings(section))
      held <- shiny::isolate(code_records())
      held[[section]] <- record
      code_records(held)
      invisible(record)
    }
    code_output <- function(section) renderUI({
      record <- code_records()[[section]]
      # Explicit dependencies keep the status current; sieve_revision itself
      # reads in isolation so computing a result does not depend on its log.
      state$dataset_id
      state$items
      state$meta
      current <- sieve_revision(state)
      if (!is.null(record) && !identical(record$revision$dataset_id, current$dataset_id)) record <- NULL
      status <- if (is.null(record)) "not_run" else if (
        !identical(record$page_revision, sieve_pool_revision(state)) ||
          !identical(record$settings, code_settings(section))
      ) "stale" else "current"
      sieve_code_view(record, status = status, ns = session$ns)
    })
    output$scree_code <- code_output("scree")
    output$pa_code <- code_output("pa")
    output$vss_code <- code_output("vss")
    output$fit_code <- code_output("fit")
    observe({
      records <- code_records()
      state$sampling_design
      state$case_index
      state$raw
      titles <- c(scree = "Scree and eigenvalues", pa = "Parallel analysis", vss = "VSS",
        fit = "Exploratory model fit by factor count")
      for (section in names(records)) {
        record <- records[[section]]
        same <- identical(record$page_revision, sieve_pool_revision(state)) &&
          identical(record$settings, code_settings(section))
        # The scree plot follows the shared matrix by itself; the criteria
        # from before an edit wait for the page's one Update.
        waits <- section %in% c("pa", "vss", "fit")
        sieve_page_publish(state, "retention", session$ns(paste0("result_", section)), titles[[section]],
          list(state = if (same) "current" else "stale",
            sentence = if (!same) paste(
              "The included items, their values, the respondents or the settings changed after this result was computed.",
              if (waits) "Update, in the status line, computes it again."
            ),
            key = record$settings, when = record$recorded_at),
          if (waits) "Update" else "Review scree plot", own = TRUE)
      }
    })

    # Active items only, coerced numeric, listwise complete: the pool every
    # correlation-based tab reads (sieve_state_items). Returns NULL when
    # there are too few items or cases to support a factor analysis.
    x_data <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      pool <- sieve_state_items(state)
      if (is.null(pool) || length(pool$items) < 3) {
        return(NULL)
      }
      available <- identical(sieve_basis_resolve(state, pool, input$cor_type %||% "auto"), "pearson") &&
        !identical(sieve_estimator_effective("study", state), "ml")
      X <- if (available) pool$raw else pool$X
      if (nrow(X) < 10 || ncol(X) < 3) {
        return(NULL)
      }
      X
    })

    # The theorized number of factors, or NULL when working from uploaded data.
    theory_k <- reactive({
      k <- state$theory$k
      if (is.null(k) || !is.finite(k)) NULL else as.integer(k)
    })

    # The decision in force for these data, read reactively (a new record
    # or another dataset changes it), and the number it carries.
    current_decision <- reactive({
      state$decisions
      sieve_current_decision(state, "factor_retention", state$dataset_id %||% NA_character_)
    })
    decision_k <- reactive({
      d <- current_decision()
      if (is.null(d)) NULL else as.integer(d$choice)
    })
    # The one line every panel repeats, so the choice in force is never far
    # from the evidence it was made on.
    decision_line <- function() {
      d <- current_decision()
      if (is.null(d)) {
        return(div(
          class = "small mt-2",
          tags$b("No decision recorded yet. "),
          "Enter the number of factors at the top of the tab when you are ready; nothing elsewhere waits for it."
        ))
      }
      k <- as.integer(d$choice)
      div(
        class = "small mt-2",
        tags$b(sprintf("Decision in force: %d factor%s (%s). ", k, if (k == 1L) "" else "s", d$id)),
        if (identical(d$rationale, sieve_no_rationale())) {
          "No rationale was given."
        } else {
          paste0("Rationale: ", d$rationale)
        }
      )
    }

    # The confirmed measurement level of each included item, by item name.
    item_types <- reactive({
      req(state$meta)
      m <- state$meta[state$meta$status == "active", , drop = FALSE]
      stats::setNames(as.character(m$type), m$item)
    })

    # The correlations the settings ask for, before anything is computed:
    # the study's setting (the Ingest tab's Estimator control, or the item
    # types) for "auto", or the choice made here.
    basis_request <- reactive({
      choice <- input$cor_type %||% "auto"
      if (identical(choice, "auto")) {
        choice <- sieve_study_basis_request(state)
      }
      sieve_retention_request(choice, unname(item_types()))
    })

    # The first choice names what the study's setting resolves to, and the
    # line under the control says what is in force and where it was set.
    observe({
      types <- unname(item_types())
      label <- sieve_study_basis_label(state, types)
      shiny::updateSelectInput(session, "cor_type",
        choices = sieve_basis_choices(label),
        selected = isolate(input$cor_type) %||% "auto"
      )
    })
    output$cor_source <- renderUI({
      req(state$meta)
      div(
        class = "small text-muted mb-2",
        sieve_basis_source_text(input$cor_type %||% "auto", state, unname(item_types()))
      )
    })

    # The same request under psych's short names, which the tests read.
    cor_kind <- reactive({
      switch(basis_request(),
        pearson = "pearson",
        polychoric = "poly",
        tetrachoric = "tet",
        mixed = "mixed"
      )
    })

    # Is any included item ordered or binary? This decides the reference
    # data parallel analysis reads when the reference data are left on Auto.
    # "Auto" follows the correlations used, not the item types: polychoric
    # correlations of items typed continuous (the study's WLSMV setting, or
    # the Polychoric choice here) take the resampled reference, which is
    # analyzed with the same correlations.
    categorical <- reactive({
      b <- tryCatch(basis(), error = function(e) NULL)
      if (!is.null(b) && isTRUE(b$ok) && length(b$used) == 1L) {
        !identical(b$used, "pearson")
      } else {
        any(item_types() %in% c("ordered", "binary"))
      }
    })

    # The identity of what the criteria are computed from: the complete
    # case values of the included items and their confirmed measurement
    # levels. The reactives above read the whole ledger, so they are
    # evaluated again when anything in it is assigned, an item's display
    # label included. The analyses below are therefore kept under this
    # identity (sieve_keep): an assignment that leaves it unchanged reruns
    # nothing, draws no new seed, and writes nothing to the script.
    kept <- new.env(parent = emptyenv())
    pool_key <- reactive({
      X <- x_data()
      req(X)
      sieve_hash(list(dataset = state$dataset_id, data = sieve_data_key(X),
        rows = sieve_hash(state$case_index %||% seq_len(nrow(state$items))),
        types = unname(item_types()[names(X)])))
    })

    # The correlation matrix, computed once for every criterion that uses
    # one, and shared with the other tabs that work from the inter-item
    # correlations (sieve_state_basis keeps it in the state under the
    # identity of its inputs, and writes the R that computes it when it
    # is computed). When the requested correlations cannot be computed the
    # result carries the correlations actually used and the engine's reason.
    # Under the run policy (sieve_basis_demand())
    # a polychoric, tetrachoric or mixed matrix estimated above two seconds,
    # or one of a reopened project, waits for the page's Update (labeled
    # Compute the correlations), once per item pool; meanwhile the record
    # says so, and every criterion that reads the matrix waits with it.
    basis_asked <- reactiveVal(NULL)
    basis <- reactive({
      X <- x_data()
      req(X)
      b <- sieve_basis_demand(state, input$cor_type %||% "auto", asked = basis_asked())
      req(b)
      b
    })
    # The waiting record while the matrix waits for its button, or NULL.
    basis_waiting <- reactive({
      b <- basis()
      if (isTRUE(b$waiting)) b else NULL
    })
    # What a panel says while the correlations wait: the one button is in
    # the status line (Update, labeled Compute the correlations).
    basis_waiting_ui <- function(b) {
      div(
        class = "alert alert-info mb-2", role = "status",
        tags$b("The correlations have not been computed. "), b$reason,
        " The button is in the status line at the top of the page."
      )
    }

    # One recorded seed for the random parts of this analysis: the supplied
    # one, or one drawn from the session's stream. It is drawn for each new
    # identity of the item data, the confirmed item types, the respondents
    # and the seed input, and not when a display control or a label
    # changes. Each identity keeps its seed (sieve_keep_each()): data that
    # return to an identity seen before (respondents restored after a
    # removal, an item returned to its scoring) give back the reference
    # datasets, and so the values, computed for it then.
    run_rng <- reactive({
      req(x_data())
      seed_in <- sieve_seed_input(input$seed)
      sieve_keep_each(kept, "rng", list(pool_key(), seed_in), function() sieve_draw_seed(seed_in))
    })

    # The parallel analysis: DMAR::parallel_analysis(), one call per kind of
    # reference data (sieve_pa_route()), from the one recorded seed. It
    # depends on the data, the correlations, the number of reference
    # datasets, and the seed, and NOT on the reference value or the
    # reference data: both statistics and both kinds of reference data are
    # read from the draws DMAR keeps, so those controls are instant and the
    # choices comparable, read from the same draws.
    #
    # Whether it runs by itself is decided from its estimated wait
    # (sieve_pa_plan): a first run for the data estimated at two seconds or
    # less runs here, under a progress message. Otherwise the tab shows the
    # estimate with a Run parallel analysis button. A run on request
    # estimated above two seconds goes to a background R process (the EFA
    # page's mechanism, sieve_efa_dmar_start() with the parallel analysis
    # runner), since DMAR's function has no progress hook: the status line
    # shows its elapsed time against the estimate and Cancel, a changed
    # request stops it, and a result is accepted only for the request it
    # was started for.
    # The workers of the model fit by k: the Workers control, a whole number
    # from 1 to sieve_cores(max = 64); its default is sieve_cores() (all
    # cores but one, at most 8). The result does not depend on it.
    workers_now <- reactive({
      w <- suppressWarnings(as.integer(sieve_finite_or(input$workers, sieve_cores())))
      max(1L, min(w, sieve_cores(max = 64L)))
    })
    pa_key <- reactive({
      list(
        pool_key(), basis_request(), sieve_finite_or(input$n_iter, 1000L),
        run_rng()$seed
      )
    })
    pa_plan <- reactive({
      b <- basis()
      sieve_pa_plan(b, sieve_finite_or(input$n_iter, 1000L))
    })
    pa_running <- reactiveVal(FALSE)
    pa_error <- reactiveVal(NULL)
    pa_tick <- reactiveVal(0L)
    # The background run (plain state; see sieve_efa_dmar_job()).
    pa_bg <- sieve_efa_dmar_job(start = function(call, rng_kind) {
      sieve_efa_dmar_start(call, rng_kind, runner = "sieve_pa_dmar_run")
    })
    pa_store <- reactiveVal(NULL) # the last run made on request, with its key

    # The text that announces a run, automatic or on request.
    pa_detail <- function(b, plan) {
      sprintf(
        paste0(
          "DMAR::parallel_analysis(): %d reference datasets%s on the %s correlations, ",
          "from one recorded seed; estimated wait %s."
        ),
        plan$n_iter,
        if (length(plan$families) > 1L) " for each of the two kinds of reference data" else "",
        sieve_retention_basis_label(b$used), plan$label
      )
    }

    pa_run <- reactive({
      X <- x_data()
      req(X)
      b <- basis()
      rng <- run_rng()
      key <- pa_key()
      plan <- pa_plan()
      pa_tick()
      held <- pa_store()
      if (!is.null(held) && identical(held$key, key)) return(held$value)
      cached <- kept$pa
      if (!is.null(cached) && identical(cached$key, key)) return(cached$value)
      # The correlations wait for their button: so does everything that
      # reads them (sieve_basis_demand()).
      if (isTRUE(b$waiting)) {
        out <- sieve_pa_not_run(b, plan$n_iter, rng, plan)
        out$basis_waiting <- TRUE
        out$reason <- paste("Parallel analysis waits for the correlations.", b$reason)
        return(out)
      }
      # The run policy: only a first parallel analysis for the dataset runs
      # by itself (and only at two seconds or less), and not on a reopened
      # project (sieve_restored_project()); after an edit the earlier
      # result is from before and the page's Update (or the panel's button)
      # updates it. Another dataset's result does not count as an earlier
      # result for this one, so a newly loaded dataset gets its first run.
      dataset <- state$dataset_id %||% "(unnamed data)"
      first <- !dataset %in% kept$pa_done
      restored <- sieve_restored_project(state)
      if (isTRUE(plan$automatic) && first && !restored) {
        kept$pa_done <- unique(c(kept$pa_done, dataset))
        return(sieve_keep(kept, "pa", key, function() {
          withProgress(
            message = sprintf("Parallel analysis: %d reference datasets", plan$n_iter),
            detail = pa_detail(b, plan),
            value = 0.5,
            sieve_parallel_analysis(X, b, n_iter = plan$n_iter, rng = rng)
          )
        }))
      }
      out <- sieve_pa_not_run(b, plan$n_iter, rng, plan)
      if (identical(pa_bg$stopped, key)) {
        out$stopped <- TRUE
        out$reason <- paste(
          "Canceled: the run was stopped before it finished, and nothing from it is kept.", out$reason
        )
      } else if (!first) {
        out$stale <- TRUE
        out$reason <- paste(
          "The included items, their values or the respondents changed since parallel analysis ran,",
          "so its earlier result is from before and is not shown.", out$reason
        )
      } else if (restored) {
        out$restored <- TRUE
        out$reason <- paste(sieve_restored_sentence("Run parallel analysis"), out$reason)
      }
      out
    })

    # A finished run, kept for its request.
    accept_pa <- function(res, key, dataset) {
      pa_store(list(key = key, value = res))
      kept$pa_done <- unique(c(kept$pa_done, dataset))
      invisible(res)
    }

    # The run behind the button: in the session when it is estimated at two
    # seconds or less (a background process takes about that long to
    # start), otherwise in a background R process.
    start_pa <- function() {
      if (isTRUE(pa_running())) {
        return(invisible(NULL))
      }
      X <- x_data()
      req(X)
      b <- basis()
      req(isTRUE(b$ok))
      plan <- pa_plan()
      pa_error(NULL)
      if (b$n < 10L) {
        pa_error("Parallel analysis needs ten complete cases for its reference N. The EM scree plot remains available.")
        return(invisible(NULL))
      }
      rng <- run_rng()
      key <- pa_key()
      dataset <- state$dataset_id %||% "(unnamed data)"
      pa_bg$stopped <- NULL
      # Asked for on these data: a run stopped by a changed request (an
      # exclusion while it runs) is then from before, and the page's Update
      # runs it for the new request.
      kept$pa_done <- unique(c(kept$pa_done, dataset))
      if (!isTRUE(plan$background)) {
        res <- withProgress(
          message = sprintf("Parallel analysis: %d reference datasets", plan$n_iter),
          detail = pa_detail(b, plan),
          value = 0.5,
          sieve_parallel_analysis(X, b, n_iter = plan$n_iter, rng = rng)
        )
        accept_pa(res, key, dataset)
        return(invisible(NULL))
      }
      call <- sieve_pa_dmar_calls(X, b, plan$n_iter, rng$seed)
      pa_bg$meta <- list(basis = b, call = call, rng = rng, n_iter = plan$n_iter, dataset = dataset,
        started = proc.time()[["elapsed"]])
      if (!isTRUE(sieve_efa_dmar_submit(pa_bg, key, call, seconds = plan$seconds, rng_kind = rng$kind))) {
        pa_bg$record <- NULL
        pa_bg$record_key <- NULL
        message <- paste("The parallel analysis could not start:", sub("^Background execution unavailable: ", "", pa_bg$error))
        pa_error(message)
        sieve_notify(message, type = "error", duration = NULL)
        return(invisible(NULL))
      }
      pa_running(TRUE)
      pa_tick(isolate(pa_tick()) + 1L)
      invisible(NULL)
    }
    observeEvent(input$pa_go, start_pa())
    observeEvent(input$pa_go_table, start_pa())
    cancel_pa <- function() {
      if (sieve_efa_dmar_active(pa_bg)) {
        sieve_efa_dmar_cancel(pa_bg)
      }
      if (isTRUE(isolate(pa_running()))) {
        pa_running(FALSE)
        pa_tick(isolate(pa_tick()) + 1L)
      }
      invisible(NULL)
    }
    observeEvent(input$pa_stop, {
      inclusion_wire$cancel()
      cancel_pa()
      # An Update in progress goes on to its next result (the model fit by
      # k), and does not announce the canceled run as updated.
      update_queue$what <- setdiff(update_queue$what, "parallel analysis")
      update_continue()
    })

    session$onSessionEnded(function() sieve_efa_dmar_cancel(pa_bg, remember = FALSE))
    # The elapsed time of a running parallel analysis, in the Scree panel's
    # line and in the page's status line, redrawn each second while it runs.
    sieve_progress_outputs(output, c("pa_progress", "pa_progress_line"), function() {
      if (isTRUE(pa_running())) sieve_efa_dmar_progress(pa_bg)$sentence
    }, session)

    # The finished background run: its record read into the page's result
    # for the request it was started for. A worker that ended without a
    # result is an error the panel shows with its button; a refusal by
    # DMAR is the result, shown with its reason.
    finish_pa <- function() {
      record <- pa_bg$record
      key <- pa_bg$record_key
      meta <- pa_bg$meta
      pa_bg$record <- NULL
      pa_bg$record_key <- NULL
      pa_running(FALSE)
      if (is.list(record$value)) {
        res <- sieve_pa_dmar_result(meta$basis, meta$call, record, meta$rng, meta$n_iter,
          seconds = proc.time()[["elapsed"]] - meta$started)
        accept_pa(res, key, meta$dataset)
      } else {
        message <- paste("The parallel analysis stopped with an error:", record$error %||% "the background process ended without a result.")
        pa_error(message)
        sieve_notify(message, type = "error", duration = NULL)
      }
      pa_tick(isolate(pa_tick()) + 1L)
      # An Update in progress goes on to its next result.
      update_continue()
    }

    # The background run, polled while it runs. A changed request stops it.
    observe({
      pa_tick()
      if (!sieve_efa_dmar_active(pa_bg)) return(invisible(NULL))
      key <- tryCatch(pa_key(), error = function(e) NULL)
      if (!identical(key, pa_bg$key)) {
        sieve_efa_dmar_cancel(pa_bg, remember = FALSE)
        pa_running(FALSE)
        pa_tick(isolate(pa_tick()) + 1L)
        update_continue()
      } else if (sieve_efa_dmar_poll(pa_bg)) {
        finish_pa()
      } else {
        shiny::invalidateLater(250, session)
      }
    })

    # What the Scree panel says above the plot: the estimate and the button
    # while the run waits to be asked for, the elapsed time and Cancel while
    # it runs, and what ran afterwards.
    output$pa_control <- renderUI({
      req(x_data())
      b <- basis()
      if (!is.null(pa_error())) {
        return(div(
          class = "alert alert-danger", role = "alert", pa_error(),
          p("Correct the cause, then press Run parallel analysis to try again."),
          actionButton(session$ns("pa_go"), "Run parallel analysis")
        ))
      }
      if (isTRUE(pa_running())) {
        # Drawn once per run with its Cancel button; only the elapsed time
        # is redrawn each second (pa_progress, fct_progress_line.R).
        return(div(
          class = "d-flex align-items-center gap-2 mb-2",
          tags$span(
            class = "small",
            "Parallel analysis (DMAR::parallel_analysis) is running in a background R process. ",
            sieve_progress_text(session$ns("pa_progress"))
          ),
          actionButton(session$ns("pa_stop"), "Cancel the parallel analysis", class = "btn-outline-secondary btn-sm")
        ))
      }
      run <- pa_run()
      plan <- pa_plan()
      if (isTRUE(run$basis_waiting)) {
        return(basis_waiting_ui(b))
      }
      if (identical(run$status, "not run")) {
        slow_basis <- isTRUE(b$ok) && isTRUE(plan$route$ordered)
        return(div(
          class = "alert alert-info mb-2",
          tags$b(run$reason),
          div(
            class = "small mt-1",
            sprintf(
              paste0(
                "The time per reference dataset is estimated from %s. DMAR analyzes the reference ",
                "datasets one after another in one R process; a run estimated above two seconds goes ",
                "to a background R process, so the page stays usable, and Cancel stops it (a canceled ",
                "run keeps nothing). "
              ),
              plan$source
            ),
            if (slow_basis) {
              paste0(
                "With continuous items (Pearson correlations) a parallel analysis takes seconds; ",
                "the wait is longer here because the polychoric correlations of every reference ",
                "dataset are estimated. "
              )
            } else {
              "With continuous items (Pearson correlations) a parallel analysis usually takes seconds. "
            },
            "The other criteria are shown already; the tally leaves parallel analysis out until it has run."
          ),
          div(class = "mt-2", actionButton(session$ns("pa_go"), "Run parallel analysis", class = "btn-primary"))
        ))
      }
      if (!isTRUE(run$ok)) {
        return(NULL)
      }
      div(
        class = "small text-muted mb-2",
        sprintf(
          "Parallel analysis ran %s: DMAR::parallel_analysis(), %d reference datasets%s, in %s.",
          if (isTRUE(plan$automatic) && is.null(pa_store())) "automatically (a first run, estimated at two seconds or less)" else "on request",
          run$n_iter_requested,
          if (length(run$route$families) > 1L) " for each of the two kinds of reference data" else "",
          if (is.finite(run$seconds)) sprintf("%.1f seconds", run$seconds) else "an unrecorded time"
        )
      )
    })

    # The observed series of that run (NULL when it failed).
    pa_raw <- reactive(pa_run()$engine)

    # The two display controls. Their defaults are the ones the inputs show.
    pa_statistic <- reactive(input$criterion %||% "p95")

    # The page's context line (sieve_context_line()): the rows and items the
    # evidence comes from, what it estimates, and how its uncertainty is
    # expressed, above the evidence table.
    output$context_line <- renderUI({
      b <- tryCatch(basis(), error = function(e) NULL)
      req(b, isTRUE(b$ok))
      B <- sieve_finite_or(input$n_iter, 1000L)
      seed <- tryCatch(run_rng()$seed, error = function(e) NA_integer_)
      sieve_context_line(
        sample = sprintf("%d items, %s, %s correlations", as.integer(b$p), sieve_basis_n_text(b),
          sieve_retention_basis_label(b$used)),
        estimand = paste(
          "the number of common factors, read criterion by criterion (parallel analysis and Very Simple",
          "Structure), with the fit of each k beside them (the model fit by k gives no count); SIEVER",
          "recommends no number"
        ),
        uncertainty = sprintf(paste0(
          "parallel analysis compares each observed reduced eigenvalue with the %s of %d reference ",
          "datasets%s; VSS is descriptive; the fit by k gives each RMSEA with its interval at the ",
          "RMSEA level (90%% by default)"),
          if (identical(pa_statistic(), "mean")) "mean" else "95th percentile", B,
          if (length(seed) == 1L && is.finite(seed)) sprintf(" (seed %d)", as.integer(seed)) else "")
      )
    })
    pa_family <- reactive(input$pa_reference %||% "simulated")

    # One parallel analysis in the application, not two: DMAR::factor_count()
    # on the EFA page would run DMAR::parallel_analysis() with the normal
    # reference again (the same draws under the same seed), so the EFA
    # page's number of factors panel runs without it and shows this one
    # beside its reduced eigenvalues when they are the eigenvalues of the
    # same matrix. Kept in the state's cache, which a saved project does not
    # hold; the selection in force is what is shared.
    pa_share <- function(run, sel) {
      cache <- shiny::isolate(state$cache) %||% list()
      cache$pa <- if (isTRUE(run$ok) && isTRUE(sel$ok)) {
        list(
          revision = shiny::isolate(sieve_pool_revision(state)),
          empirical = run$empirical, values = sel$values, label = sel$label,
          statistic = sel$statistic, family = sel$family_used, suggested = sel$suggested,
          n_used = sel$n_used, seed = run$rng$seed, basis_used = run$basis_used,
          matrix_words = sieve_pa_matrix_words(run)
        )
      }
      state$cache <- cache
      invisible(NULL)
    }

    # The reference series selected by the two display controls, with the
    # count it gives. Logged here, so the recorded R is written when the
    # analysis has actually run and carries the seed it ran from.
    pa_sel <- reactive({
      run <- pa_run()
      statistic <- pa_statistic()
      family <- pa_family()
      mixed_pool <- categorical()
      if (identical(run$status, "not run")) {
        # Nothing ran, so nothing is selected or recorded.
        return(sieve_pa_select(run, statistic, family, mixed_pool))
      }
      sieve_keep(kept, "pa_sel", list(pa_key(), run$status, run$n_iter, statistic, family, mixed_pool), function() {
        sel <- sieve_pa_select(run, statistic, family, mixed_pool)
        sieve_log(
          state,
          if (isTRUE(run$ok)) {
            sprintf(
              paste0(
                "Dimensionality: factor-analytic parallel analysis (Horn, 1965) by DMAR::parallel_analysis on the %s, ",
                "%d reference datasets%s, seed %d%s; reference: %s."
              ),
              sieve_pa_matrix_words(run), run$n_iter_requested,
              if (length(run$route$families) > 1L) " for each of the two kinds of reference data" else "",
              run$rng$seed,
              if (isTRUE(run$rng$supplied)) "" else " drawn for this run",
              if (isTRUE(sel$ok)) sel$label else "not available"
            )
          } else {
            "Dimensionality: factor-analytic parallel analysis did not run."
          },
          sieve_pa_code(run, sel)
        )
        if (isTRUE(run$ok)) remember_code("pa", c(
          sprintf("Parallel analysis: DMAR::parallel_analysis (DMAR %s) on the %s; %d reference datasets per kind of reference data; seed %d.",
                  as.character(utils::packageVersion("DMAR")), sieve_pa_matrix_words(run), run$n_iter_requested, run$rng$seed),
          if (isTRUE(sel$ok)) sel$label else "No selected reference is available."
        ), correlation_basis = basis())
        pa_share(run, sel)
        sel
      })
    })

    # The engine's object with the selected reference attached: what the
    # scree plot draws, so the gold line and the reported count cannot
    # disagree.
    pa <- reactive({
      out <- pa_raw()
      if (is.null(out)) {
        return(NULL)
      }
      sel <- pa_sel()
      if (!isTRUE(sel$ok)) {
        out$sieve_reference <- NA_character_
        return(out)
      }
      out$nfact <- sel$suggested
      out$sieve_ref_values <- sel$values
      out$sieve_reference <- sel$code
      out$sieve_ref_label <- sel$label
      out
    })

    max_k_now <- reactive(sieve_finite_or(input$max_k, 8L))

    # Very Simple Structure at complexity 1 and 2, under
    # the run rule (sieve_runs_by_itself()): a first result for the data
    # runs by itself when its estimate (sieve_vss_wait()) is two seconds or
    # less, as it is for a small pool at the default eight factors (not for
    # 68 items, about 15 seconds); after an edit (the
    # items, their values, the respondents, the correlations, the largest
    # number of factors or the seed) the earlier result is from before and
    # waits for the page's Update, and a longer first run waits for Run VSS
    # and VSS, rather than running again after every edit whatever its
    # wait (20 seconds at 20 factors on 68 items).
    vss_key <- reactive(list(pool_key(), basis_request(), max_k_now(), run_rng()$seed))
    vss_store <- reactiveVal(NULL)
    vss_seconds <- reactive({
      b <- basis()
      sieve_vss_wait(sieve_retention_max_k(max_k_now(), ncol(x_data())), ncol(x_data()),
        R = if (isTRUE(b$ok) && is.matrix(b$R)) b$R)
    })
    # A run estimated above two seconds (sieve_auto_seconds()) goes to a
    # background R process (sieve_efa_dmar_start() with the VSS runner):
    # DMAR::fa_exploratory() at each k gives point estimates only, whose
    # time grows with the items: about 2 to 4 seconds for the 24 bundled
    # tests at eight factors, about 15 for 68 items. The
    # status line and the panel show the elapsed time against the estimate
    # with Cancel; a changed request stops it; a canceled run keeps nothing.
    vss_bg <- sieve_efa_dmar_job(start = function(call, rng_kind) {
      sieve_efa_dmar_start(call, rng_kind, runner = "sieve_vss_dmar_run")
    })
    vss_running <- reactiveVal(FALSE)
    vss_error <- reactiveVal(NULL)
    vss_tick <- reactiveVal(0L)
    # The result read, logged and remembered: from a run in the session, or
    # from a background record (`record`).
    compute_vss <- function(record = NULL, meta = NULL) {
      b <- meta$basis %||% basis()
      rng <- meta$rng %||% run_rng()
      max_k <- meta$max_k %||% max_k_now()
      v <- if (is.null(record)) {
        sieve_retention_vss(b, max_k = max_k, rng = rng)
      } else {
        shell <- sieve_vss_shell(b, max_k, rng)
        shell$seconds <- record$seconds
        sieve_vss_read(shell, record)
      }
      if (isTRUE(v$ok) && is.finite(v$seconds %||% NA_real_)) {
        ks <- sieve_vss_ks(v$max_k, v$p)
        sieve_efa_point_time_note(v$p, ks, "matrix", v$seconds, refused = as.character(ks) %in% names(v$refused))
      }
      sieve_log(
        state,
        if (isTRUE(v$ok)) {
          sprintf(
            paste0(
              "Dimensionality: Very Simple Structure for 1 to %d ",
              "factors (DMAR::fa_exploratory, maximum likelihood, direct oblimin) on the %s correlation matrix, seed %d%s."
            ),
            v$max_k, sieve_retention_basis_label(v$basis_used), v$rng$seed,
            if (isTRUE(v$rng$supplied)) "" else " drawn for this run"
          )
        } else {
          "Dimensionality: Very Simple Structure did not run."
        },
        sieve_vss_code(v)
      )
      if (isTRUE(v$ok)) remember_code("vss", c(
        sprintf("VSS: %s correlations; maximum %d factors; seed %d.",
                sieve_retention_basis_label(v$basis_used), v$max_k, v$rng$seed),
        "DMAR::fa_exploratory: maximum likelihood; direct oblimin rotation (none at one factor) for VSS."
      ), correlation_basis = b)
      kept$vss_done <- unique(c(kept$vss_done, state$dataset_id %||% "(unnamed data)"))
      v
    }
    vss_run <- reactive({
      req(x_data())
      key <- vss_key()
      vss_tick()
      held <- vss_store()
      if (!is.null(held) && identical(held$key, key)) return(held$value)
      cached <- kept$vss
      if (!is.null(cached) && identical(cached$key, key)) return(cached$value)
      dataset <- state$dataset_id %||% "(unnamed data)"
      first <- !dataset %in% kept$vss_done
      seconds <- vss_seconds()
      b <- basis()
      if (isTRUE(b$waiting)) {
        out <- sieve_vss_not_run(b, run_rng(), max_k_now(), seconds)
        out$basis_waiting <- TRUE
        out$reason <- paste("Very Simple Structure wait for the correlations.", b$reason)
        return(out)
      }
      if (isTRUE(vss_running()) && sieve_efa_dmar_active(vss_bg, key)) {
        out <- sieve_vss_not_run(b, run_rng(), max_k_now(), seconds)
        out$running <- TRUE
        out$reason <- "Very Simple Structure (DMAR::fa_exploratory at each k) is running in a background R process."
        return(out)
      }
      restored <- sieve_restored_project(state)
      if (first && !restored && sieve_runs_by_itself(seconds, standard = seconds)) {
        return(sieve_keep(kept, "vss", key, compute_vss))
      }
      out <- sieve_vss_not_run(b, run_rng(), max_k_now(), seconds, stale = !first)
      if (identical(vss_bg$stopped, key)) {
        out$stopped <- TRUE
        out$reason <- paste(
          "Canceled: Very Simple Structure was stopped before it finished, and nothing from it is kept.", out$reason
        )
      }
      if (first && restored) {
        out$restored <- TRUE
        out$reason <- paste(sieve_restored_sentence("Run VSS"), out$reason)
      }
      out
    })
    run_vss_now <- function(progress = TRUE) {
      key <- vss_key()
      held <- vss_store()
      if (!is.null(held) && identical(held$key, key)) return(invisible(NULL))
      if (!is.null(kept$vss) && identical(kept$vss$key, key)) return(invisible(NULL))
      if (isTRUE(vss_running()) && sieve_efa_dmar_active(vss_bg, key)) return(invisible(NULL))
      seconds <- vss_seconds()
      vss_bg$stopped <- NULL
      vss_error(NULL)
      b <- basis()
      if (isTRUE(b$ok) && is.finite(seconds) && seconds > sieve_auto_seconds()) {
        rng <- run_rng()
        K <- sieve_retention_max_k(max_k_now(), b$p)
        vss_bg$meta <- list(basis = b, rng = rng, max_k = max_k_now(), key = key)
        if (isTRUE(sieve_efa_dmar_submit(vss_bg, key, sieve_vss_call(b, K, rng), seconds = seconds, rng_kind = rng$kind))) {
          vss_running(TRUE)
          vss_tick(isolate(vss_tick()) + 1L)
          return(invisible(NULL))
        }
        # The worker could not start: run here, and say why.
        vss_error(paste(
          "Very Simple Structure could not start in a background R process",
          sprintf("(%s), so it ran in this session.", sub("^Background execution unavailable: ", "", vss_bg$error))
        ))
        vss_bg$record <- NULL
        vss_bg$record_key <- NULL
      }
      value <- if (isTRUE(progress)) {
        withProgress(
          message = sprintf("Very Simple Structure (%s)", sieve_duration_label(seconds)),
          value = 1,
          compute_vss()
        )
      } else {
        compute_vss()
      }
      vss_store(list(key = key, value = value))
      invisible(NULL)
    }
    observeEvent(input$vss_go, run_vss_now())
    observeEvent(input$vss_go_table, run_vss_now())
    cancel_vss <- function() {
      if (sieve_efa_dmar_active(vss_bg)) sieve_efa_dmar_cancel(vss_bg)
      if (isTRUE(isolate(vss_running()))) {
        vss_running(FALSE)
        vss_tick(isolate(vss_tick()) + 1L)
      }
      invisible(NULL)
    }
    stop_vss <- function() {
      cancel_vss()
      # An Update in progress goes on to its next result, and does not
      # announce the canceled run as updated.
      update_queue$what <- setdiff(update_queue$what, "VSS")
      update_continue()
    }
    # The Cancel of the page's status line and that of the VSS panel: two
    # buttons, both on the page while VSS runs, so two ids.
    observeEvent(input$vss_stop, stop_vss())
    observeEvent(input$vss_stop_panel, stop_vss())
    session$onSessionEnded(function() sieve_efa_dmar_cancel(vss_bg, remember = FALSE))
    # The elapsed time of a running VSS, in the page's status line and in
    # the VSS panel, redrawn each second while it runs.
    sieve_progress_outputs(output, c("vss_progress_line", "vss_progress"), function() {
      if (isTRUE(vss_running())) sieve_efa_dmar_progress(vss_bg)$sentence
    }, session)
    # The background run, polled while it runs: a changed request stops it;
    # a finished one is read for the request it was started for.
    observe({
      vss_tick()
      if (!sieve_efa_dmar_active(vss_bg)) return(invisible(NULL))
      key <- tryCatch(vss_key(), error = function(e) NULL)
      if (!identical(key, vss_bg$key)) {
        sieve_efa_dmar_cancel(vss_bg, remember = FALSE)
        vss_running(FALSE)
        vss_tick(isolate(vss_tick()) + 1L)
        update_continue()
      } else if (sieve_efa_dmar_poll(vss_bg)) {
        record <- vss_bg$record
        meta <- vss_bg$meta
        vss_bg$record <- NULL
        vss_bg$record_key <- NULL
        vss_running(FALSE)
        if (is.list(record) && isTRUE(record$ok)) {
          value <- compute_vss(record, meta)
          vss_store(list(key = meta$key, value = value))
        } else {
          message <- paste("Very Simple Structure stopped with an error:",
            record$error %||% "the background process ended without a result.")
          vss_error(message)
          sieve_notify(message, type = "error", duration = NULL)
        }
        vss_tick(isolate(vss_tick()) + 1L)
        update_continue()
      } else {
        shiny::invalidateLater(250, session)
      }
    })
    vss_res <- reactive(vss_run()$engine)

    # The exploratory factor model at k = 1 to max_k, each a lavaan::efa fit
    # of the included items under the estimator in force (the panel's
    # Estimator control, or the study's setting on the Ingest tab): the test
    # statistic, RMSEA with its 90 percent interval (the RMSEA level), CFI, TLI, SRMR and BIC,
    # with CFI, TLI and RMSEA in one family for every k, so the gain from
    # each added factor can be read. Unrotated, because lavaan's robust
    # indices of a rotated efa object fit by FIML depend on the rotation.
    # Under WLSMV on a large pool each solution takes a minute,
    # so the sequence runs by itself only when its estimated wait is about
    # ten seconds or less, and otherwise from the Run the model fit by k
    # button, with the estimate stated first.
    fit_spec <- reactive({
      req(state$meta)
      sieve_lavaan_spec(state$meta, estimator = sieve_estimator_effective(input$estimator %||% "study", state))
    })
    # Every row of the included items: lavaan applies the estimator's
    # missing data treatment (FIML under ML with FIML and MLR, the complete
    # cases under ML on the complete cases and WLSMV).
    fit_data <- reactive({
      X <- x_data()
      req(X)
      d <- as.data.frame(state$items)[, names(X), drop = FALSE]
      d[] <- lapply(d, sieve_as_number)
      d[rowSums(!is.na(d)) > 0L, , drop = FALSE]
    })
    fit_key <- reactive({
      spec <- fit_spec()
      list(
        pool_key(), sieve_data_key(fit_data()), spec$estimator, spec$ordered, spec$missing,
        max_k_now()
      )
    })
    # The same request as the EFA tab's fit indices names it (see
    # sieve_efa_job_key()): a solution either tab has fit for these data,
    # items, estimator and missing data rule is kept in the session's store
    # and read, not fit again.
    fit_request <- reactive({
      d <- fit_data()
      spec <- fit_spec()
      revision <- sieve_page_revision(state)
      missing_arg <- sieve_efa_missing_arg(spec$missing, d) %||% "default"
      list(
        dataset_id = revision$dataset, rows = revision$rows, sampling = revision$sampling,
        data = sieve_data_key(d), estimator = spec$estimator, ordered = spec$ordered,
        missing = spec$missing, lavaan_missing = missing_arg,
        n = if (identical(missing_arg, "ml")) nrow(d) else sum(stats::complete.cases(d)),
        label = spec$label, types = item_types(), rotation = "none",
        ks = fit_plan()$ks, p = ncol(d)
      )
    })
    fit_plan <- reactive({
      p <- ncol(x_data())
      K <- sieve_retention_max_k(max_k_now(), p)
      ks <- seq_len(K)
      ok <- sieve_efa_df(p, ks) >= 0
      spec <- fit_spec()
      d <- fit_data()
      n <- if (identical(sieve_efa_missing_arg(spec$missing, d), "ml")) nrow(d) else sum(stats::complete.cases(d))
      # Two estimates. Run here, in this R process, the solutions are fit in
      # turn and share what does not depend on k: that estimate decides
      # whether the sequence runs by itself (about ten seconds or less). Run
      # on request, they are split over the workers (sieve_efa_chunks) in
      # background R processes, and that is the wait stated.
      inline <- sieve_efa_plan_wait(p, spec$estimator, n, ks[ok], 1L)
      # The estimate the run policy reads: the sequence in this process at
      # the standard rates (sieve_at_default_rates()), which the not-run line
      # states beside the workers' wait of the button.
      standard <- sieve_at_default_rates(sieve_efa_plan_wait(p, spec$estimator, n, ks[ok], 1L))
      screen <- sieve_numeric_columns(d, names(d))$text
      unusable <- if (length(screen)) {
        paste(c("The model fit by k was not started.", screen), collapse = " ")
      }
      workers <- max(1L, min(workers_now(), sieve_efa_worker_cap(p, spec$estimator, n = n), max(1L, sum(ok))))
      seconds <- sieve_efa_plan_wait(p, spec$estimator, n, ks[ok], workers)
      list(
        ks = ks[ok], not_identified = ks[!ok], p = p, estimator = spec$estimator, n = n,
        seconds = seconds, label = sieve_duration_label(seconds), workers = workers,
        inline_seconds = inline, standard_seconds = standard,
        automatic = sieve_runs_by_itself(inline, standard = standard),
        # An item lavaan cannot fit (no observed values, or one value
        # throughout) is refused by name before any worker starts: lavaan
        # would print its whole variable table and stop, at each k. An MLR
        # (or ML) sequence whose memory need exceeds the machine is refused
        # as well (sieve_mlr_guard()).
        refused = unusable %||% sieve_mlr_guard(p, spec$estimator, n = n,
          npar = sieve_free_parameters(p, min(c(ks[ok], 1L)), exploratory = TRUE,
            means = identical(sieve_efa_missing_arg(spec$missing, d), "ml"))
        )
      )
    })
    fit_store <- reactiveVal(NULL)
    # The background queue of a requested sequence: its workers, the
    # request in flight, and a tick for the displays that read its progress.
    fit_jobs <- sieve_efa_jobs(
      workers = function() isolate(workers_now()),
      shared = sieve_efa_session_shared(session)
    )
    fit_pending <- reactiveVal(NULL)
    fit_tick <- reactiveVal(0L)
    record_fit <- function(f, spec, d) {
      sieve_log(
        state,
        if (isTRUE(f$ok)) {
          sprintf(
            "Dimensionality: model fit by k, lavaan::efa for 1 to %d factors under %s (%s family of CFI, TLI and RMSEA).",
            f$max_k, f$label, f$family %||% "no common"
          )
        } else {
          "Dimensionality: the model fit by k did not run."
        },
        sieve_fit_code(f)
      )
      if (isTRUE(f$ok)) remember_code("fit", c(
        sprintf("Model fit by k: lavaan::efa; %s; unrotated; %s fit-index family.",
                f$label, f$family %||% "no common"),
        sprintf("Missing data: %s. Eligible item rows: %d.", spec$missing, nrow(d))
      ), model_data = TRUE)
      kept$fit_done <- unique(c(kept$fit_done, shiny::isolate(state$dataset_id) %||% "(unnamed data)"))
      f
    }
    run_fit <- function(key, plan) {
      b <- basis()
      spec <- fit_spec()
      d <- fit_data()
      max_k <- max_k_now()
      request <- fit_request()
      started <- proc.time()[["elapsed"]]
      f <- withProgress(
        message = sprintf(
          "Model fit by k: lavaan::efa for k = %s (%s, %d items, %s)",
          if (length(plan$ks) > 1L) sprintf("%d to %d", min(plan$ks), max(plan$ks)) else plan$ks,
          spec$label, plan$p, sieve_duration_label(plan$inline_seconds)
        ),
        detail = "One exploratory solution per number of factors; the fit indices of every solution are read in one family.",
        value = 1,
        sieve_retention_fit(b,
          max_k = max_k, data = d, spec = spec,
          byk = if (length(plan$ks)) sieve_efa_byk_kept(fit_jobs, request, d, spec, plan$ks)
        )
      )
      # The time is kept with the result and shown; it is not recorded as
      # a rate per item pair. On a small pool a lavaan solution's cost is
      # mostly fixed (the robust indices under FIML take about a second
      # whatever the size), so scaling it by item pairs would misstate the
      # wait of a larger pool; the EFA and CFA tabs record their own rates.
      f$seconds <- proc.time()[["elapsed"]] - started
      record_fit(f, spec, d)
    }
    fit_run <- reactive({
      req(x_data())
      fit_tick()
      key <- fit_key()
      plan <- fit_plan()
      held <- fit_store()
      if (!is.null(held) && identical(held$key, key)) return(held$value)
      cached <- kept$fit
      if (!is.null(cached) && identical(cached$key, key)) return(cached$value)
      spec <- fit_spec()
      waiting <- sieve_fit_not_run(spec, plan, sieve_spec_basis(spec, unname(item_types()[names(x_data())])))
      if (!is.null(plan$refused)) {
        waiting$reason <- plan$refused
        waiting$refused <- TRUE
        return(waiting)
      }
      b <- basis()
      if (isTRUE(b$waiting)) {
        waiting$basis_waiting <- TRUE
        waiting$reason <- paste("The model fit by k waits for the correlations.", b$reason)
        return(waiting)
      }
      # The run policy: a first sequence for the dataset, at two seconds or
      # less, runs by itself, and not on a reopened project
      # (sieve_restored_project()); after an edit the page's Update (or
      # the panel's button) updates it. Another dataset's sequence does not
      # count as an earlier sequence for this one, so a newly loaded dataset
      # gets its first run.
      dataset <- state$dataset_id %||% "(unnamed data)"
      first <- !dataset %in% kept$fit_done
      restored <- sieve_restored_project(state)
      if (isTRUE(plan$automatic) && first && !restored) {
        return(sieve_keep(kept, "fit", key, function() run_fit(key, plan)))
      }
      if (!first) {
        waiting$stale <- TRUE
        waiting$reason <- paste(
          "The included items, their values, the respondents or the estimator changed since the model fit by k ran,",
          "so its earlier result is from before and is not shown.", waiting$reason
        )
      } else if (restored) {
        waiting$restored <- TRUE
        waiting$reason <- paste(sieve_restored_sentence("Run the model fit by k"), waiting$reason)
      }
      pending <- fit_pending()
      if (!is.null(pending) && identical(pending$key, key)) {
        # Running in the background: still not in the tally, and said so.
        waiting$running <- TRUE
        waiting$reason <- sprintf(
          paste0(
            "The model fit by k is running: %d exploratory model%s (k = %s) on %d items with ",
            "lavaan::efa under %s, on %d background worker%s."
          ),
          length(plan$ks), if (length(plan$ks) == 1L) "" else "s",
          if (length(plan$ks) > 1L) sprintf("%d to %d", min(plan$ks), max(plan$ks)) else as.character(plan$ks),
          plan$p, spec$label, plan$workers, if (plan$workers == 1L) "" else "s"
        )
      }
      waiting
    })
    # The sequence asked for: every solution kept by either tab is read; the
    # others run in background R processes, split over the workers, while
    # the page stays usable; Cancel stops them.
    start_fit <- function() {
      key <- fit_key()
      plan <- fit_plan()
      if (!is.null(plan$refused)) {
        sieve_notify(plan$refused, type = "error", duration = NULL)
        return(invisible(NULL))
      }
      request <- fit_request()
      d <- fit_data()
      spec <- fit_spec()
      missing <- plan$ks[vapply(plan$ks, function(k) is.null(sieve_efa_job_cache(fit_jobs, request, k)), logical(1))]
      if (!length(missing)) {
        fit_store(list(key = key, value = run_fit(key, plan)))
        return(invisible(NULL))
      }
      fit_pending(list(
        key = key, request = request, plan = plan, data = d, spec = spec,
        basis = basis(), max_k = max_k_now(), started = proc.time()[["elapsed"]]
      ))
      fit_jobs$key <- NULL
      sieve_efa_jobs_submit(fit_jobs, request, d, plan$ks)
      fit_tick(isolate(fit_tick()) + 1L)
      invisible(NULL)
    }
    observeEvent(input$fit_go, start_fit())
    cancel_fit <- function() {
      inclusion_wire$cancel()
      sieve_efa_jobs_cancel(fit_jobs)
      fit_pending(NULL)
      fit_tick(isolate(fit_tick()) + 1L)
      update_queue$active <- FALSE
      update_queue$steps <- character(0)
    }
    observeEvent(input$fit_cancel, cancel_fit())
    session$onSessionEnded(function() sieve_efa_jobs_cancel(fit_jobs))
    # Whether the sequence for the current inputs is running (fit_run()'s
    # running state), read from what starts and ends a run alone: the lines
    # that show the run are drawn once per run, not at each count that
    # finishes (fit_tick()), and their progress text says how far it is.
    fit_running <- reactive({
      pending <- fit_pending()
      !is.null(pending) && identical(pending$key, tryCatch(fit_key(), error = function(e) NULL))
    })
    fit_progress_counts <- function() {
      progress <- sieve_efa_jobs_progress(fit_jobs)
      list(
        progress = progress, done = length(fit_jobs$completed) + length(fit_jobs$failed),
        total = length(fit_pending()$plan$ks),
        running = if (length(progress$running)) paste(progress$running, collapse = ", ") else "none",
        workers = sprintf("%d worker%s", progress$workers, if (progress$workers == 1L) "" else "s")
      )
    }
    # The progress of a running sequence, redrawn each second (and as each
    # count finishes) while it runs: in full on the Model fit by k panel,
    # and the counts in the page's status line.
    sieve_progress_outputs(output, "fit_progress", function() {
      fit_tick()
      if (!isTRUE(fit_running())) return(NULL)
      x <- fit_progress_counts()
      sprintf(
        "%d of %d complete; k = %s running on %s; %d queued. Elapsed: %.0f seconds. %s",
        x$done, x$total, x$running, x$workers, length(fit_jobs$pending), x$progress$elapsed, x$progress$wait
      )
    }, session)
    sieve_progress_outputs(output, "fit_progress_line", function() {
      fit_tick()
      if (!isTRUE(fit_running())) return(NULL)
      x <- fit_progress_counts()
      sprintf("%d of %d complete; k = %s running on %s.", x$done, x$total, x$running, x$workers)
    }, session)
    # Poll the background sequence; when every count is in, combine the set
    # exactly as one call would, record it, and keep it.
    observe({
      pending <- fit_pending()
      if (is.null(pending)) return()
      current <- tryCatch(fit_key(), error = function(e) NULL)
      if (!identical(current, pending$key)) {
        # The inputs changed: the run no longer describes them, nor does an
        # Update in progress.
        sieve_efa_jobs_cancel(fit_jobs)
        fit_pending(NULL)
        update_queue$active <- FALSE
        update_queue$steps <- character(0)
        return()
      }
      if (sieve_efa_jobs_poll(fit_jobs)) fit_tick(isolate(fit_tick()) + 1L)
      if (sieve_efa_jobs_active(fit_jobs)) {
        shiny::invalidateLater(150, session)
        return()
      }
      failures <- as.list(stats::setNames(sub("^k = [0-9]+: ", "", fit_jobs$errors), as.character(fit_jobs$failed)))
      f <- sieve_retention_fit(pending$basis,
        max_k = pending$max_k, data = pending$data, spec = pending$spec,
        byk = sieve_efa_byk_kept(fit_jobs, pending$request, pending$data, pending$spec,
          pending$plan$ks,
          fit = FALSE, failures = failures
        )
      )
      f$seconds <- proc.time()[["elapsed"]] - pending$started
      f$workers <- pending$plan$workers
      isolate(record_fit(f, pending$spec, pending$data))
      fit_store(list(key = pending$key, value = f))
      fit_pending(NULL)
      update_continue()
    })

    estimator_choice <- sieve_estimator_wire(input, output, state,
      output_id = "estimator_source", detail = function() {
      spec <- fit_spec()
      resolved <- sieve_estimator_resolve(unname(item_types()), n = nrow(fit_data()),
        choice = sieve_estimator_effective(input$estimator %||% "study", state))
      tagList(sprintf(" For these items: %s. ", spec$label), resolved$reason)
    })

    # What the panel says above the plot: the estimate and the button while
    # the sequence waits to be asked for, and how it ran afterwards.
    output$fit_control <- renderUI({
      req(x_data())
      # While the sequence runs: drawn once per run with its Cancel button;
      # only its progress is redrawn (fit_progress), so this output reads
      # fit_running(), not fit_run(), which changes as each count finishes.
      if (isTRUE(fit_running())) {
        f <- isolate(fit_run())
        return(div(
          class = "alert alert-info mb-2", role = "status",
          tags$b(f$reason),
          div(class = "small mt-1", sieve_progress_text(session$ns("fit_progress"))),
          div(class = "mt-2", actionButton(session$ns("fit_cancel"), "Cancel the model fit by k", class = "btn-sm"))
        ))
      }
      f <- fit_run()
      plan <- fit_plan()
      if (identical(f$status, "not run") && isTRUE(f$refused)) {
        return(div(class = "alert alert-danger mb-2", f$reason))
      }
      if (isTRUE(f$basis_waiting)) {
        return(basis_waiting_ui(basis()))
      }
      if (identical(f$status, "not run")) {
        return(div(
          class = "alert alert-info mb-2",
          tags$b(f$reason),
          div(class = "mt-2", actionButton(session$ns("fit_go"), "Run the model fit by k", class = "btn-primary"))
        ))
      }
      if (!isTRUE(f$ok)) {
        return(NULL)
      }
      div(
        class = "small text-muted mb-2",
        sprintf(
          "The model fit by k ran %s: %d solution%s in %s.",
          if (isTRUE(plan$automatic) && is.null(fit_store())) {
            "automatically (a first run, estimated at two seconds or less)"
          } else if (is.finite(f$workers %||% NA_real_)) {
            sprintf("on request, on %d background worker%s", f$workers, if (f$workers == 1L) "" else "s")
          } else {
            "on request"
          },
          nrow(f$table), if (nrow(f$table) == 1L) "" else "s",
          if (is.finite(f$seconds %||% NA_real_)) sprintf("%.1f seconds", f$seconds) else "an unrecorded time"
        )
      )
    })
    fit_res <- reactive({
      tab <- fit_run()$table
      if (is.null(tab) || (all(is.na(tab$rmsea)) && all(is.na(tab$cfi)))) NULL else tab
    })

    # The evidence, one row per criterion.
    criteria <- reactive({
      req(x_data())
      req(is.null(basis_waiting()))
      pa_sel()
      sieve_retention_criteria(
        basis(), pa_run(), vss_run(),
        statistic = pa_statistic(),
        reference = pa_family(),
        categorical = categorical()
      )
    })

    # The suggested counts by name (theory is the reference, not a criterion).
    suggestions <- reactive({
      cr <- criteria()
      k <- stats::setNames(as.list(cr$suggested), cr$id)
      list(theory = theory_k(), pa = k$pa, vss1 = k$vss1, vss2 = k$vss2)
    })

    # Range, disagreement, the heuristic tally, and the heuristic neighborhood.
    evidence_summary <- reactive(sieve_retention_summary(criteria(), theory_k()))

    # Under a theoretical construct basis (the Ingest option) the structure
    # is specified, so this tab is not a step on the way to the CFA tab, and
    # it says so before anything else: a confirmatory study can start with
    # the CFA tab.
    output$optional_note <- renderUI({
      basis <- state$construct_basis %||% "theoretical"
      if (!identical(basis, "theoretical")) {
        return(NULL)
      }
      # Beside the decision it concerns: the sentence visible, the reason
      # under it (results first).
      div(
        class = "small text-muted sieve-optional-tab mb-2",
        tags$details(
          tags$summary(tags$strong("This tab is optional for a confirmatory study.")),
          "The construct basis is theoretical (set on the Ingest tab), so the CFA tab is ready ",
          "to fit the structure you specified; nothing on the CFA tab waits for a decision ",
          "made here. The evidence here reads the number of factors from the data, which is ",
          "worth seeing even when the structure is specified, and is the way in when the ",
          "basis is empirical."
        )
      )
    })

    output$gate <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(sieve_gate(state))
      }
      if (is.null(x_data())) {
        return(div(
          class = "alert alert-danger",
          "Need at least three included items and ten usable respondents to assess dimensionality. Ordered-item correlations and complete-case ML require ten complete cases."
        ))
      }

      NULL
    })

    # ---- The page's one status line and its Update --------------------------
    # What waits on this page: the criteria computed before an edit and now
    # from before (parallel analysis, VSS, the model fit by k),
    # which the one Update computes again, in turn; and the criteria never
    # requested for these data (a first run estimated above two seconds),
    # which keep their own buttons and waits in the table and the panels.
    update_plan <- reactive({
      req(x_data())
      pa <- tryCatch(pa_run(), error = function(e) NULL)
      vss <- tryCatch(vss_run(), error = function(e) NULL)
      fit <- tryCatch(fit_run(), error = function(e) NULL)
      waits <- function(r) identical(r$status, "not run") && !isTRUE(r$refused) && !isTRUE(r$running)
      out <- list(
        pa = waits(pa) && isTRUE(pa$stale), vss = waits(vss) && isTRUE(vss$stale),
        fit = waits(fit) && isTRUE(fit$stale),
        new_pa = waits(pa) && !isTRUE(pa$stale), new_vss = waits(vss) && !isTRUE(vss$stale),
        new_fit = waits(fit) && !isTRUE(fit$stale)
      )
      wait <- function(on, seconds) if (isTRUE(on)) tryCatch(seconds(), error = function(e) NA_real_) else 0
      out$seconds <- wait(out$vss, vss_seconds) + wait(out$pa, function() pa_plan()$seconds) +
        wait(out$fit, function() fit_plan()$seconds)
      out$names <- c(
        if (out$pa) "parallel analysis", if (out$vss) "VSS", if (out$fit) "the model fit by k"
      )
      out$new <- c(
        if (out$new_pa) sprintf("parallel analysis (%s)", pa_plan()$label),
        if (out$new_vss) sprintf("VSS (%s)", sieve_duration_label(vss_seconds())),
        if (out$new_fit) {
          # The estimate that decided (in this process, at the standard rates)
          # beside the wait of the button (on the workers), when they differ.
          fp <- fit_plan()
          standard <- sieve_duration_label(fp$standard_seconds)
          if (identical(standard, fp$label)) {
            sprintf("the model fit by k (%s)", fp$label)
          } else {
            sprintf(
              "the model fit by k (%s in this process at the standard rates, which decides; %s on %d worker%s from its button)",
              standard, fp$label, fp$workers, if (fp$workers == 1L) "" else "s"
            )
          }
        }
      )
      out$any <- out$pa || out$vss || out$fit
      out
    })
    join_words <- function(x) {
      if (length(x) <= 1L) return(x)
      paste(paste(x[-length(x)], collapse = ", "), x[length(x)], sep = " and ")
    }
    output$status_line <- renderUI({
      # What blocks the page is said by the gate above, alone.
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) return(NULL)
      X <- tryCatch(x_data(), error = function(e) NULL)
      if (is.null(X)) return(NULL)
      line <- function(tone, ...) div(class = paste0("alert alert-", tone, " small py-2 mb-2"), role = "status", ...)
      queued <- if (isTRUE(update_queue$active) && length(update_queue$steps)) {
        sprintf(" Then, in turn: %s.", join_words(c(vss = "VSS", pa = "parallel analysis", fit = "the model fit by k")[update_queue$steps]))
      } else {
        ""
      }
      # A running line is drawn once per run with its Cancel button; only
      # its progress is redrawn each second (fct_progress_line.R), so
      # Cancel keeps keyboard focus and takes every click.
      if (isTRUE(vss_running())) {
        return(line(
          "info", sieve_lifecycle_badge("running", "computed"),
          "Very Simple Structure (DMAR::fa_exploratory at each k) is running in a background R process. ",
          sieve_progress_text(session$ns("vss_progress_line")),
          queued, " ",
          actionButton(session$ns("vss_stop"), "Cancel VSS", class = "btn-outline-secondary btn-sm")
        ))
      }
      if (isTRUE(pa_running())) {
        return(line(
          "info", sieve_lifecycle_badge("running", "computed"),
          "Parallel analysis (DMAR::parallel_analysis) is running in a background R process. ",
          sieve_progress_text(session$ns("pa_progress_line")),
          queued, " ",
          actionButton(session$ns("update_stop"), "Cancel the update", class = "btn-outline-secondary btn-sm")
        ))
      }
      # The shared correlation matrix waits for its button (the run policy,
      # sieve_basis_demand()): the page's one action computes it first, and
      # the criteria then follow the policy as on a first view.
      bw <- tryCatch(basis_waiting(), error = function(e) NULL)
      if (!is.null(bw)) {
        return(line(
          "info", sieve_lifecycle_badge("not_run", "computed"),
          strong("The correlations have not been computed. "), bw$reason, " ",
          actionButton(session$ns("update_go"), bw$action, class = "btn-primary btn-sm")
        ))
      }
      # fit_running(), not fit_run(): the line is not redrawn as each count
      # finishes; its counts are the progress text (fit_progress_line).
      if (isTRUE(tryCatch(fit_running(), error = function(e) FALSE))) {
        return(line(
          "info", sieve_lifecycle_badge("running", "computed"),
          "The model fit by k is running in the background: ",
          sieve_progress_text(session$ns("fit_progress_line")), " ",
          actionButton(session$ns("update_cancel"), "Cancel the model fit by k", class = "btn-sm")
        ))
      }
      up <- tryCatch(update_plan(), error = function(e) NULL)
      if (is.null(up)) return(NULL)
      if (isTRUE(up$any)) {
        return(line(
          "warning", sieve_lifecycle_badge("stale"),
          strong(sprintf(
            "The included items, their values, the respondents or the settings changed since %s %s computed; %s withheld until updated. ",
            join_words(up$names), if (length(up$names) == 1L) "was" else "were",
            if (length(up$names) == 1L) "it is" else "they are"
          )),
          actionButton(session$ns("update_go"), sprintf("Update (%s)", sieve_duration_label(up$seconds)),
            class = "btn-primary btn-sm"
          )
        ))
      }
      if (length(up$new)) {
        restored <- sieve_restored_project(state)
        # The criteria have their buttons in the evidence table; the model
        # fit by k, which is not a criterion, has its button on its panel.
        in_table <- c(if (up$new_pa) "parallel analysis", if (up$new_vss) "VSS")
        where <- c(
          if (length(in_table)) {
            sprintf(
              "%s in the evidence table below",
              if (!up$new_fit) {
                if (length(in_table) == 1L) "its button is" else "their buttons are"
              } else {
                sprintf("the button%s of %s %s", if (length(in_table) == 1L) "" else "s",
                  join_words(in_table), if (length(in_table) == 1L) "is" else "are")
              }
            )
          },
          if (up$new_fit) {
            sprintf("%s on the Model fit by k panel",
              if (length(in_table)) "the button of the model fit by k is" else "its button is")
          }
        )
        where <- paste(where, collapse = ", and ")
        if (restored) where <- paste0(toupper(substring(where, 1L, 1L)), substring(where, 2L))
        return(line(
          "info", sieve_lifecycle_badge("not_run", "computed"),
          if (restored) strong(paste0(sieve_restored_sentence(
            if (length(up$new) == 1L) "its button" else "their buttons"
          ), " ")),
          sprintf(
            "Not run yet for these data: %s. %s%s.",
            join_words(up$new),
            if (restored) "" else "A first result runs by itself only when its estimate in this process at the standard rates is two seconds or less; ",
            where
          )
        ))
      }
      NULL
    })

    # The one Update: the stale criteria, in turn. VSS run here
    # under one progress message; parallel analysis then runs as its job
    # (with Stop), and the model fit by k after it (in the background, with
    # Cancel). A message says when everything is done.
    update_queue <- new.env(parent = emptyenv())
    update_queue$active <- FALSE
    update_queue$steps <- character(0)
    update_queue$what <- character(0)
    # A result that finishes hands on to the next one on the next flush,
    # outside the finishing job's own error capture.
    update_signal <- reactiveVal(0L)
    update_continue <- function() {
      if (isTRUE(update_queue$active)) update_signal(isolate(update_signal()) + 1L)
      invisible(NULL)
    }
    observeEvent(update_signal(), update_next(), ignoreInit = TRUE)
    update_next <- function() {
      if (!isTRUE(update_queue$active)) return(invisible(NULL))
      while (length(update_queue$steps)) {
        step <- update_queue$steps[1L]
        update_queue$steps <- update_queue$steps[-1L]
        if (identical(step, "vss")) {
          tryCatch(run_vss_now(), error = function(e) NULL)
          if (isTRUE(isolate(vss_running()))) return(invisible(NULL))
        } else if (identical(step, "pa")) {
          tryCatch(start_pa(), error = function(e) NULL)
          if (isTRUE(isolate(pa_running()))) return(invisible(NULL))
        } else if (identical(step, "fit")) {
          tryCatch(start_fit(), error = function(e) NULL)
          if (!is.null(isolate(fit_pending()))) return(invisible(NULL))
        }
      }
      update_queue$active <- FALSE
      if (length(update_queue$what)) {
        sieve_notify(sprintf("Updated %s: the Dimensionality page is current.", join_words(update_queue$what)))
      }
      invisible(NULL)
    }
    update_current <- function() {
      # The correlations first, when they wait: computed now under their
      # own progress message; the criteria then follow the run policy as
      # on a first view (a first result at two seconds or less by itself,
      # the others with their buttons), and what is stale is updated below.
      bw <- basis_waiting()
      if (!is.null(bw)) {
        basis_asked(bw$key)
        if (isTRUE(basis()$waiting)) return(FALSE)
      }
      up <- update_plan()
      if (isTRUE(update_queue$active) || isTRUE(isolate(pa_running())) || isTRUE(isolate(vss_running())) ||
        !is.null(isolate(fit_pending()))) return(FALSE)
      if (!isTRUE(up$any)) return(TRUE)
      update_queue$what <- up$names
      # VSS first (in this session under its own progress message at two
      # seconds or less, otherwise in a background R process with Cancel),
      # then parallel analysis and the model fit by k, in turn.
      update_queue$steps <- c(if (up$vss) "vss", if (up$pa) "pa", if (up$fit) "fit")
      update_queue$active <- TRUE
      update_next()
    }
    page_update <- sieve_update_wire(input, session, function() {
      up <- update_plan()
      list(page = sieve_update_entry("Dimensionality results",
        state = if (isTRUE(up$any)) "stale" else "current", seconds = up$seconds,
        run = update_current, background = TRUE, shown = FALSE))
    }, sequence = function(entries) update_current())
    inclusion_wire <- sieve_inclusion_wire(state, update_current)
    observeEvent(input$update_stop, {
      inclusion_wire$cancel()
      update_queue$active <- FALSE
      update_queue$steps <- character(0)
      cancel_pa()
      cancel_vss()
    })
    observeEvent(input$update_cancel, cancel_fit())

    # The one-line method statement: the correlations, the items and the
    # cases the criteria are read from, what computes each, and the seed.
    output$method_line <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), x_data())
      b <- basis()
      req(isTRUE(b$ok))
      choice <- input$cor_type %||% "auto"
      source <- if (isTRUE(b$changed)) {
        sprintf("%s requested and not computable; see below", sieve_retention_basis_label(b$requested))
      } else if (identical(choice, "auto")) {
        "study setting"
      } else {
        "chosen on this tab"
      }
      spec <- tryCatch(fit_spec(), error = function(e) NULL)
      # The seed of the random parts is stated under the criteria table.
      p(
        class = "small mb-2",
        strong("Model: "),
        sprintf(
          paste0(
            "%s correlations (%s), %d items, %s; DMAR::parallel_analysis for parallel analysis; ",
            "DMAR::fa_exploratory (ML) for VSS; lavaan::efa (%s) for the fit by k."
          ),
          sieve_retention_basis_label(b$used), source, b$p, sieve_basis_n_text(b),
          spec$label %||% "the estimator in force"
        ),
        sieve_holdout_n_tag(state)
      )
    })

    # A statement of the correlations requested and used, shown next to every
    # result computed from them. A change is an alert, never a footnote.
    basis_note <- function(b) {
      if (isTRUE(b$waiting)) {
        return(basis_waiting_ui(b))
      }
      if (!isTRUE(b$ok)) {
        return(div(
          class = "alert alert-danger",
          tags$b("No correlation matrix could be computed. "), b$reason
        ))
      }
      # Everything the engine said while it computed the matrix, warnings
      # and messages alike: a replaced empty cell is reported as a message.
      said <- c(b$warnings, b$messages)
      extra <- if (length(said)) {
        div(
          class = "small text-muted",
          "The engine also reported: ", paste(said, collapse = " ")
        )
      }
      if (isTRUE(b$changed)) {
        return(div(
          class = "alert alert-warning",
          tags$b(sprintf(
            "Requested: %s correlations. Used: %s correlations. ",
            sieve_retention_basis_label(b$requested),
            sieve_retention_basis_label(b$used)
          )),
          # One string: separate pieces are set on separate lines of the page
          # source, which puts a space before a period that starts a piece.
          paste0(
            "The requested correlations could not be computed, so this is a different ",
            "analysis from the one requested. ", sub("[.]?$", ".", b$reason), " ",
            if (!is.null(b$facts)) paste0(b$facts, " ") else "",
            "Pearson correlations of ordered or binary items are attenuated when ",
            "categories are few or the response distributions are skewed, which can ",
            "change the counts below."
          ),
          extra
        ))
      }
      div(
        class = "small text-muted",
        sprintf("Correlations: %s, as requested.", sieve_retention_basis_label(b$used)),
        extra
      )
    }

    seed_text <- function(rng) {
      sprintf(
        "seed %d (%s)", rng$seed,
        if (isTRUE(rng$supplied)) "supplied" else "drawn for this run; enter it under Seed to repeat the run"
      )
    }

    # ---- Overview -----------------------------------------------------------
    output$overview <- renderUI({
      b <- basis()
      rng <- run_rng()
      n_all <- nrow(state$items)
      ctype <- switch(b$used %||% "",
        pearson = "Pearson product-moment correlations",
        polychoric = "polychoric correlations (ordered or binary items; Garrido, Abad and Ponsoda, 2013)",
        tetrachoric = "tetrachoric correlations (binary items)",
        mixed = paste0(
          "mixed correlations from the confirmed item types (Pearson among continuous ",
          "items, polychoric or tetrachoric among ordered or binary items, polyserial ",
          "or biserial between the two kinds)"
        ),
        "no correlation matrix"
      )
      tagList(
        tags$h6("How many factors? Reading dimensionality before EFA"),
        p(
          "Deciding ", tags$b("how many factors to retain"), " is the first inferential ",
          "step in scale construction and is settled ", tags$em("before"), " the EFA is ",
          "interpreted, because the rotated solution, the pattern of loadings, and every ",
          "downstream reliability and validity claim are conditional on that count. ",
          "Over-extraction splinters a coherent construct into minor, unreplicable factors; ",
          "under-extraction collapses distinct constructs and inflates cross-loadings."
        ),
        tags$details(open = NA,
          tags$summary("More on how to read the criteria: what each assumes, the Kaiser rule, and what was computed"),
          p(
            tags$b("No single rule is definitive, and no vote among rules is either."),
            " The criteria on this tab fail in different ways. Read each for what it assumes, ",
            "note where they agree and where they do not, and decide in light of the ",
            "theorized structure and the interpretability of the competing solutions. ",
            "SIEVER reports the evidence criterion by criterion and records your decision ",
            "with your rationale; it does not choose the number of factors."
          ),
          tags$ul(
            tags$li(
              tags$b("Parallel analysis (Horn, 1965)"), " counts a factor only while its ",
              "empirical eigenvalue exceeds the eigenvalue of the same rank from data with ",
              "no common factors. SIEVER runs it on ", tags$b("factor-analytic"),
              " eigenvalues (the correlation matrix reduced by the communalities of a fixed ",
              "one-factor maximum likelihood solution), not on principal components, with ",
              "DMAR::parallel_analysis(). The reference value is the 95th percentile of the ",
              "reference eigenvalues (Glorfeld, 1995), which is usually the more conservative, ",
              "or their mean (Horn, 1965); DMAR states the Monte Carlo limits of the ",
              "percentile, which say how precisely the reference datasets fix it. The ",
              "reference data are either independent normal responses (for ordered items, ",
              "categorized at each item's observed category proportions and analyzed with ",
              "polychoric correlations) or the observed responses resampled item by item, ",
              "analyzed with the same correlations as the items; for ordered or binary items ",
              "the reference data should resemble the real items in their response ",
              "distributions (Garrido, Abad and Ponsoda, 2013). Which eigenvalues parallel ",
              "analysis should use is not settled: Timmerman and Lorenzo-Seva (2011) found ",
              "a common factor version most accurate, while Garrido and colleagues and Lim ",
              "and Jahng (2019) found the traditional version on the unreduced correlation ",
              "matrix more accurate than the factor-analytic variants they studied, and ",
              "all report that parallel analysis can miss weak or highly correlated ",
              "factors. That is a reason to read it next to the other criteria."
            ),
            tags$li(
              tags$b("Very Simple Structure (Revelle and Rocklin, 1979)"), " evaluates how well ",
              "a solution of each rank reproduces the correlations when each item is allowed ",
              "only its largest one (complexity 1) or two (complexity 2) loadings. The two ",
              "complexities are one family of evidence, not two independent criteria."
            ),
            tags$li(
              tags$b("The model fit by k"), " tracks RMSEA (with its ",
              sieve_ci_percent(sieve_default_rmsea_conf_level()), " interval, the level of every RMSEA ",
              "interval SIEVER shows; the page's other intervals are at ",
              sieve_ci_percent(sieve_default_conf_level()), "), CFI, and ",
              "TLI of the exploratory factor model as factors are added, each solution fit by ",
              "lavaan::efa under the estimator in force (the Model fit by k panel's Estimator ",
              "control, which starts from the study's setting on the Ingest tab). It is read beside ",
              "the criteria and is not one of them: it gives no count. What it shows is how much ",
              "each added factor improves fit, with the RMSEA read through its interval. No value ",
              "of an index is applied as a cutoff. The values Hu and Bentler (1999) proposed came ",
              "from simulations of confirmatory models of continuous data, the conditions they ",
              "simulated limit how far they generalize (Marsh, Hau and Wen, 2004), and none of ",
              "them is a property of a good exploratory solution. Xia (2021) evaluated sequential ",
              "rules of this kind against parallel analysis and MAP when a parsimonious model only ",
              "approximates the population; Finch (2020) studied the change in fit between ",
              "successive solutions."
            )
          ),
          p(
            tags$b("The Kaiser eigenvalue-greater-than-one rule is not a criterion here."), " It applies to ",
            "the eigenvalues of the unreduced correlation matrix, over-factors, and has no ",
            "inferential basis. The Scree plot panel draws those eigenvalues with a solid gray line ",
            "at 1 marked as a reference, not a rule."
          ),
          p(
            "Whatever number a criterion suggests is an ", tags$b("estimate"), " with unknown ",
            "error. Lim and Jahng (2019) advise treating the numbers within one of the ",
            "parallel analysis estimate as viable candidates and consulting the ",
            "interpretability of the competing solutions. SIEVER shows that neighborhood as ",
            "a heuristic range of solutions worth fitting; it is not a confidence interval. ",
            "Timmerman and Lorenzo-Seva (2011) propose parallel analysis based on minimum ",
            "rank factor analysis for ordered polytomous items; it is not computed here."
          ),
          basis_note(b),
          p(
            class = "text-muted small",
            sprintf("What was computed: %s; %s (loaded rows: %d). ", ctype, sieve_basis_n_text(b), n_all),
            "Parallel analysis (DMAR::parallel_analysis, which forms this matrix from the item ",
            "responses where it can and is given it otherwise) and Very Simple Structure work ",
            "from this matrix with maximum likelihood extraction, and Very Simple Structure uses ",
            "direct oblimin rotation. The ",
            "model fit by k is lavaan::efa on the item data under its own estimator, unrotated ",
            "because lavaan's robust CFI, TLI and RMSEA of a rotated model fit by FIML depend on the rotation. The random parts (the parallel analysis reference and the rotation ",
            sprintf("starts) run from %s.", seed_text(rng))
          )
        )
      )
    })

    # ---- Scree plot (the eigenvalues of the basis, no simulation) -----------
    # The pool the display controls belong to: the study and its included
    # items, held in a reactiveVal so that the controls are drawn again (on
    # every rank) only when it changes, never on a refit or an edit that
    # leaves it as it was. Its own number of items, not only the count: a
    # new study with as many items is a new pool.
    scree_pool <- reactiveVal(NULL)
    # The reduced series always uses the one-factor ML communalities,
    # the same reduction used for parallel analysis.
    observe({
      X <- tryCatch(x_data(), error = function(e) NULL)
      id <- if (is.null(X)) NULL else list(dataset = state$dataset_id, items = names(X))
      if (!identical(id, isolate(scree_pool()))) scree_pool(id)
    }, priority = 100)

    output$scree_control <- renderUI({
      id <- scree_pool()
      req(id)
      n_items <- length(id$items)
      req(n_items >= 2L)
      tagList(
        sliderInput(session$ns("scree_ranks"), "Eigenvalue ranks shown",
          min = 1L, max = n_items, value = c(1L, n_items), step = 1L, ticks = FALSE, width = "100%"
        ),
        tags$details(
          open = NA, class = "small text-muted mb-2",
          tags$summary("What the Rank Slider Shows"),
          p("The slider changes only what is drawn: nothing is refitted, and the counts on this tab do not change. The plot opens on all ranks. Narrow it to read the leading eigenvalues, for example the first 6 of 50."),
          p("The reduced series uses the communalities of a one-factor maximum likelihood model on the diagonal of the correlation matrix (Fabrigar, Wegener, MacCallum and Strahan, 1999). This fixed reduction matches the parallel analysis. The unreduced series uses ones on the diagonal. Read the reduced series for its elbow; a reduced matrix can have negative eigenvalues.")
        )
      )
    })

    # Drawn from the shared matrix alone, so it is on screen as soon as the
    # matrix is, whether or not the parallel analysis has run.
    # The key holds the k actually fitted, so a value held to a smaller
    # pool's bound and the same value typed are one computation. Only the
    # last k is kept: going back to an earlier k fits and records it again,
    # so that the session script always ends on the series on screen.
    scree_vals <- reactive({
      req(x_data())
      b <- basis()
      k <- 1L
      # Not kept, and nothing recorded, while the matrix waits for its
      # button: the plot is drawn as soon as the matrix is.
      if (isTRUE(b$waiting)) {
        return(sieve_scree_values(b, k))
      }
      sieve_keep(kept, "scree", list(pool_key(), basis_request(), sieve_hash(b$R), k), function() {
        v <- sieve_scree_values(b, k)
        sieve_log(
          state,
          if (isTRUE(v$ok)) {
            sprintf(
              "Dimensionality: scree plot, the eigenvalues of the %s correlation matrix (unreduced, and reduced by %s communalities); %d above 1 (Kaiser criterion, a reference).",
              sieve_retention_basis_label(v$basis_used), sieve_scree_factor_words(v$nfactors), v$kaiser
            )
          } else {
            "Dimensionality: the scree plot could not be drawn."
          },
          sieve_scree_code(v)
        )
        if (isTRUE(v$ok)) remember_code("scree", c(
          sprintf("Scree eigenvalues: %s correlations; %s.",
                  sieve_retention_basis_label(v$basis_used), sieve_basis_n_text(b)),
          sprintf(
            "Unreduced eigenvalues and a reduction by %s maximum likelihood communalities.",
            sieve_scree_factor_words(v$nfactors)
          )
        ), correlation_basis = b)
        v
      })
    })

    scree_figure <- function() {
      v <- scree_vals()
      validate(need(isTRUE(v$ok), paste("No scree plot:", v$reason)))
      sieve_scree_plot(v, state$palette %||% "nd",
        theory_k = theory_k(), decision_k = decision_k(), shown = input$scree_ranks
      )
    }
    output$scree_plot <- renderPlot(scree_figure(), res = 96,
      alt = "Scree plot: the eigenvalues of the correlation matrix in decreasing order, by factor number."
    )
    sieve_plot_save_server("scree_plot", output, session, state,
      draw = scree_figure,
      file_stem = "siever_scree_plot",
      what = "the scree plot (Dimensionality tab)",
      # A narrowed window is part of what the file shows: the recorded R
      # draws it too.
      note = function() {
        v <- scree_vals()
        n_all <- nrow(v$table %||% data.frame())
        sieve_rank_window_code(
          sieve_rank_window(n_all, input$scree_ranks), n_all,
          "scree_unreduced", "scree_reduced", "Eigenvalue rank", "Eigenvalue"
        )
      }
    )

    output$scree_plot_note <- renderUI({
      req(x_data())
      v <- scree_vals()
      b <- basis()
      if (!isTRUE(v$ok)) {
        return(tagList(
          basis_note(b),
          div(class = "alert alert-warning", tags$b("The scree plot could not be drawn. "), v$reason),
          decision_line()
        ))
      }
      n_red <- sum(is.finite(v$table$reduced))
      improper <- sieve_scree_improper_text(v)
      tagList(
        basis_note(b),
        if (!is.null(improper)) div(class = "alert alert-warning", improper),
        p(
          class = "small",
          sprintf(
            paste0(
              "Two series from the %s correlation matrix of the %d included items (%s). ",
              "Circles, solid line: the eigenvalues of the matrix as it stands (unreduced). ",
              "Triangles, dashed line: the factor-analytic eigenvalues of the matrix reduced by the ",
              "communalities of a %s maximum likelihood solution, %s. Solid gray horizontal line: ",
              "the Kaiser criterion at eigenvalue 1."
            ),
            sieve_retention_basis_label(v$basis_used), v$p, sieve_basis_n_text(b),
            sieve_scree_factor_words(v$nfactors),
            if (identical(as.integer(v$nfactors), 1L)) {
              "the series parallel analysis compares on the next panel"
            } else {
              "as set above (parallel analysis, on the next panel, compares the one-factor reduction)"
            }
          ),
          if (n_red == 0L) " The reduced series could not be computed and is not drawn." else "",
          {
            shown <- sieve_rank_window(nrow(v$table), input$scree_ranks)
            hidden <- sieve_rank_window_lines(shown, theory_k(), decision_k())$outside
            paste0(
              if (is.null(sieve_rank_window_text(shown, nrow(v$table)))) "" else " ",
              sieve_rank_window_text(shown, nrow(v$table)) %||% "",
              if (is.null(hidden)) "" else paste0(" ", hidden)
            )
          }
        ),
        p(
          class = "small",
          tags$b(sprintf(
            "Kaiser criterion (eigenvalue 1): %d eigenvalue%s of the unreduced matrix exceed%s 1. ",
            v$kaiser, if (v$kaiser == 1L) "" else "s", if (v$kaiser == 1L) "s" else ""
          )),
          "The line at 1 is a reference, not a rule. The criterion concerns the eigenvalues of the ",
          "unreduced correlation matrix, tends to retain too many factors, and has no inferential ",
          "basis (Zwick and Velicer, 1986); it is not among the criteria tallied on this tab. Read ",
          "the elbow of the curve for orientation and the criteria on the other panels for evidence."
        ),
        if (length(c(v$warnings, v$messages))) {
          div(
            class = "small text-muted",
            "The engine also reported: ",
            paste(
              c(
                v$warnings,
                v$messages
              ),
              collapse = " "
            )
          )
        },
        decision_line()
      )
    })

    # ---- Scree plot with parallel analysis overlay --------------------------
    pa_figure <- function() {
      X <- x_data()
      req(X)
      run <- pa_run()
      # Nothing is drawn in place of a failed analysis: component
      # eigenvalues are not factor-analytic eigenvalues. A run that waits
      # for its button says so in its own words.
      validate(need(
        isTRUE(run$ok),
        if (identical(run$status, "not run")) {
          run$reason
        } else {
          paste("Parallel analysis did not run.", run$reason)
        }
      ))
      p <- pa()
      sel <- pa_sel()
      # The very series the count was read from, carried on the object, so
      # the legend's line and the reported count cannot disagree.
      sieve_pa_plot(
        as.numeric(p$fa.values),
        reference = if (isTRUE(sel$ok)) as.numeric(sel$values) else NULL,
        reference_label = if (isTRUE(sel$ok)) sel$label else "reference not available",
        basis_used = run$basis_used, palette = state$palette %||% "nd",
        theory_k = theory_k(), decision_k = decision_k(), shown = input$pa_ranks
      )
    }
    # The same display window as the scree plot's, for this figure: it
    # changes what is drawn and nothing else (the parallel analysis is not
    # run again, and its count does not change).
    output$pa_window <- renderUI({
      id <- scree_pool()
      req(id)
      n_items <- length(id$items)
      req(n_items >= 2L)
      div(
        style = "max-width: 32rem;",
        sliderInput(session$ns("pa_ranks"), "Eigenvalue ranks shown",
          min = 1L, max = n_items, value = c(1L, n_items), step = 1L, ticks = FALSE, width = "100%"
        ),
        p(
          class = "small text-muted mb-2",
          "Narrows the figure to a range of ranks to read the leading eigenvalues against the ",
          "reference. Only the drawing changes; the analysis and its count do not."
        )
      )
    })
    output$scree <- renderPlot(pa_figure(), res = 96,
      alt = "Parallel analysis: the observed eigenvalues beside those of random data of the same size, by factor number."
    )
    sieve_plot_save_server("scree", output, session, state,
      draw = pa_figure,
      file_stem = "siever_parallel_analysis",
      what = "the parallel analysis figure (Dimensionality tab)",
      note = function() {
        n_all <- length(pa_run()$empirical)
        sieve_rank_window_code(
          sieve_rank_window(n_all, input$pa_ranks), n_all,
          "observed", "reference", "Factor", "Factor-analytic eigenvalue (reduced correlation matrix)"
        )
      }
    )

    # Where the observed series of the parallel analysis is not computed
    # from the page's own matrix (DMAR's polychoric correlations on the
    # ordered route), how far it is from the scree plot's reduced series.
    pa_scree_gap <- function(run) {
      v <- tryCatch(scree_vals(), error = function(e) NULL)
      if (is.null(v) || !isTRUE(v$ok) || !length(run$empirical)) return(NULL)
      m <- min(length(run$empirical), nrow(v$table))
      gap <- suppressWarnings(max(abs(v$table$reduced[seq_len(m)] - run$empirical[seq_len(m)]), na.rm = TRUE))
      if (!is.finite(gap) || gap < 1e-8) return(NULL)
      sprintf(
        paste0(
          " The scree plot's reduced series is computed from the page's %s matrix and this one from ",
          "DMAR's own (%s); here they differ by at most %.1e."
        ),
        sieve_retention_basis_label(run$basis_used),
        if (isTRUE(run$route$ordered)) "lavaan's polychoric estimates" else "its own estimate of the matrix", gap
      )
    }

    output$scree_note <- renderUI({
      req(x_data())
      run <- pa_run()
      b <- basis()
      if (identical(run$status, "not run")) {
        return(tagList(basis_note(b), decision_line()))
      }
      if (!isTRUE(run$ok)) {
        return(tagList(
          basis_note(b),
          div(
            class = "alert alert-warning",
            tags$b("Parallel analysis did not run, so it suggests nothing. "), run$reason
          ),
          decision_line()
        ))
      }
      sel <- pa_sel()
      if (!isTRUE(sel$ok) || is.na(sel$suggested)) {
        return(tagList(
          basis_note(b),
          div(
            class = "alert alert-warning",
            tags$b("Parallel analysis ran but no count could be read from it. "),
            sel$reason %||% "An eigenvalue comparison was undefined."
          )
        ))
      }
      k <- as.integer(sel$suggested)
      # The same draws read the other ways: how much the count depends on
      # the reference value and the reference data.
      others <- expand.grid(
        statistic = c("p95", "mean"), family = c("simulated", "resampled"),
        stringsAsFactors = FALSE
      )
      alt <- vapply(seq_len(nrow(others)), function(i) {
        s <- sieve_pa_select(run, others$statistic[i], others$family[i])
        if (!isTRUE(s$ok) || isTRUE(s$family_changed)) {
          return(NA_character_)
        }
        sprintf(
          "%s, %s: %s",
          if (others$statistic[i] == "p95") "95th percentile" else "mean",
          if (others$family[i] == "simulated") "simulated normal data" else "resampled responses",
          fmt_k(s$suggested)
        )
      }, character(1))
      stop_txt <- if (k < length(run$empirical)) {
        sprintf(
          " The count stops at factor %d, whose eigenvalue is %.3f against a reference of %.3f.",
          k + 1L, run$empirical[k + 1L], sel$values[k + 1L]
        )
      } else {
        ""
      }
      tagList(
        basis_note(b),
        if (isTRUE(sel$family_changed)) {
          div(
            class = "alert alert-warning",
            tags$b("The requested reference data are not available. "), sel$reason,
            sprintf(
              " The reference shown is from %s.",
              if (identical(sel$family_used, "simulated")) {
                "simulated normal data analyzed with Pearson correlations"
              } else {
                "the items' own responses, resampled independently"
              }
            )
          )
        },
        if (!is.null(sel$caution)) {
          div(class = "alert alert-warning", tags$b("Mismatched reference. "), sel$caution)
        },
        div(
          class = "small text-muted mt-2",
          sprintf(
            paste0(
              "Read against the %s, parallel analysis retains %d factor%s: that many empirical ",
              "factor-analytic eigenvalues exceed the reference in order.%s Computed by ",
              "DMAR::parallel_analysis() on the %s (%d reference datasets%s, %s%s)."
            ),
            sel$label, k, if (k == 1L) "" else "s", stop_txt, sieve_pa_matrix_words(run),
            run$n_iter_requested,
            if (length(run$route$families) > 1L) " for each kind of reference data" else "",
            seed_text(run$rng),
            if (is.finite(run$seconds)) sprintf("; the run took %.1f seconds", run$seconds) else ""
          ),
          if (identical(sel$statistic, "p95") && length(sel$mc_lower) > k) {
            sprintf(
              paste0(
                " The reference percentile is itself an estimate from the draws: at factor %d its Monte Carlo ",
                "limits are [%s, %s] (DMAR's %s%% order-statistic limits; an infinite limit means %d draws are ",
                "too few to bound it). These limits describe the reference, not the sampling error of the ",
                "observed eigenvalue or of the count."
              ),
              k + 1L, fmt_lim(sel$mc_lower[k + 1L]), fmt_lim(sel$mc_upper[k + 1L]), "95", sel$n_used
            )
          },
          if (sieve_finite_or(run$failed, 0) > 0) {
            sprintf(" %d reference fit%s could not be completed and %s left out, as DMAR counts them.",
              as.integer(run$failed), if (run$failed == 1L) "" else "s", if (run$failed == 1L) "is" else "are")
          },
          if (length(run$boundary)) {
            sprintf(" The observed one-factor reduction reaches the lower uniqueness bound for %s.", sieve_name_some(run$boundary))
          },
          pa_scree_gap(run)
        ),
        {
          # DMAR's own record of the reference it read: its counts and its
          # note (the conditional coverage of the Monte Carlo limits, the
          # boundary reference fits, a benchmark N). DMAR tallies the
          # conditions of the reference fits; which dataset raised which is
          # not reported.
          tbl <- run$dmar[[sel$dmar_reference]]
          counts <- attr(tbl, "counts", exact = TRUE)
          said <- names(counts$warning_messages)
          div(
            class = "small text-muted",
            if (sieve_finite_or(counts$warned, 0) > 0) {
              sprintf("%d reference dataset%s issued warnings (%s). ", as.integer(counts$warned),
                if (identical(as.integer(counts$warned), 1L)) "" else "s",
                paste(utils::head(said, 3L), collapse = "; "))
            },
            if (!is.null(tbl)) paste("DMAR's note:", attr(tbl, "analysis_note", exact = TRUE))
          )
        },
        div(
          class = "small text-muted",
          "The same draws read the other ways (reference value, reference data: count): ",
          paste(alt[!is.na(alt)], collapse = "; "), ". ",
          "The count is an estimate, not a fixed value: Lim and Jahng (2019) advise treating ",
          "the numbers within one of it as viable candidates and consulting the ",
          "interpretability of the competing solutions."
        ),
        {
          # What the figure's window leaves out, said under it as well as in
          # its subtitle, as the scree plot's note does.
          n_all <- length(run$empirical)
          shown <- sieve_rank_window(n_all, input$pa_ranks)
          said <- c(
            sieve_rank_window_text(shown, n_all),
            sieve_rank_window_lines(shown, theory_k(), decision_k())$outside
          )
          if (length(said)) div(class = "small text-muted", paste(said, collapse = " "))
        },
        decision_line()
      )
    })

    # ---- VSS --------------------------------------------------------
    output$mapvss_note <- renderUI({
      req(x_data())
      v <- vss_run()
      b <- basis()
      if (isTRUE(v$basis_waiting)) {
        return(tagList(basis_waiting_ui(b), decision_line()))
      }
      if (isTRUE(v$running)) {
        # Drawn once per run with its Cancel button (its own id: the
        # status line's Cancel VSS is on the page too); only the elapsed
        # time is redrawn each second (vss_progress).
        return(tagList(
          div(
            class = "d-flex align-items-center gap-2 mb-2",
            tags$span(
              class = "small",
              "Very Simple Structure (DMAR::fa_exploratory at each k) is running in a background R process. ",
              sieve_progress_text(session$ns("vss_progress"))
            ),
            actionButton(session$ns("vss_stop_panel"), "Cancel VSS", class = "btn-outline-secondary btn-sm")
          ),
          basis_note(b),
          decision_line()
        ))
      }
      error_note <- if (!is.null(vss_error())) div(class = "alert alert-warning small", role = "alert", vss_error())
      if (identical(v$status, "not run")) {
        return(tagList(
          error_note,
          div(
            class = "alert alert-info",
            tags$b(v$reason),
            if (!isTRUE(v$stale) || isTRUE(v$stopped)) {
              div(class = "mt-2", actionButton(session$ns("vss_go"),
                sprintf("Run VSS (%s)", sieve_duration_label(v$seconds)),
                class = "btn-primary"
              ))
            }
          ),
          basis_note(b),
          decision_line()
        ))
      }
      if (!isTRUE(v$ok)) {
        return(tagList(
          error_note,
          basis_note(b),
          div(
            class = "alert alert-warning",
            tags$b("Very Simple Structure did not run, so they suggest nothing. "),
            v$reason
          )
        ))
      }
      s <- suggestions()
      tagList(
        error_note,
        basis_note(b),
        p(
          class = "small",
          tags$b("VSS"), " (Revelle and Rocklin, 1979) peaks at ", tags$b(fmt_k(s$vss1)),
          " factor(s) at complexity 1 and ", tags$b(fmt_k(s$vss2)), " at complexity 2. ",
          "Complexity 1 keeps only each item's single largest loading (pure simple structure); ",
          "complexity 2 allows the two largest (one cross-loading). The two complexities are ",
          "one family of evidence. A later complexity 2 peak is what one would see if items ",
          "load on more than one factor."
        ),
        p(
          class = "small text-muted",
          sprintf(
            paste0(
              "Produced from DMAR::fa_exploratory ML factor loadings and their factor correlations on the %s correlation matrix (%s): maximum likelihood ",
              "with direct oblimin rotation from 30 random starts and the recorded %s. ",
              "Higher VSS fit points to a number; it does not decide it. The fit includes diagonal cells."
            ),
            sieve_retention_basis_label(v$basis_used), sieve_basis_n_text(b), seed_text(v$rng)
          )
        ),
        p(
          class = "small text-muted",
          paste0(
            "A VSS cell is left empty where there is no fit to read: complexity 2 needs at ",
            "least two factors",
            if (any(!v$identified)) {
              sprintf(
                paste0(
                  ", for k = %s an exploratory model has negative degrees of freedom ",
                  "with %d items, so its loadings are not unique and it is not fit"
                ),
                paste(v$k[!v$identified], collapse = ", "), v$p
              )
            } else {
              ""
            },
            if (length(v$refused)) {
              sprintf(
                paste0(
                  ", and DMAR refused the solution at %s (DMAR refuses an improper solution, one that ",
                  "did not converge, and a likelihood without a maximum, where psych's fa() holds a ",
                  "uniqueness at its bound of 0.005 and reports loadings)"
                ),
                paste(sprintf("k = %s (%s)", names(v$refused), v$refused), collapse = " and ")
              )
            } else {
              ""
            },
            ". VSS is read over the remaining k, and only when at least two of them can be ",
            "compared."
          )
        ),
        decision_line()
      )
    })

    output$mapvss_tbl <- renderDT(
      {
        v <- vss_run()
        req(isTRUE(v$ok))
        # Shown where there is a fit to read (see the note above the table).
        shown <- function(fit, complexity) {
          fit[!v$identified | v$k < complexity] <- NA_real_
          fit
        }
        df <- data.frame(
          `Factors extracted` = v$k,
          `VSS complexity 1` = round_or_na(shown(v$vss1, 1L)),
          `VSS complexity 2` = round_or_na(shown(v$vss2, 2L)),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        sieve_dt(
          df,
          rownames = FALSE, selection = "none",
          options = list(dom = "t", pageLength = nrow(df), ordering = FALSE)
        )
      },
      server = FALSE
    )

    # ---- Model fit by k -----------------------------------------------------
    output$fit_note <- renderUI({
      req(x_data())
      f <- fit_run()
      ft <- fit_res()
      if (identical(f$status, "not run")) {
        return(decision_line())
      }
      if (is.null(ft)) {
        return(tagList(
          div(
            class = "alert alert-warning",
            tags$b("The fit sequence could not be computed. "),
            f$reason %||% "No fit index was returned for any number of factors."
          ),
          decision_line()
        ))
      }
      categorical <- length(f$ordered) > 0L
      no_df <- ft$k[ft$status == "ran" & is.finite(ft$df) & ft$df <= 0]
      tagList(
        p(
          class = "small text-muted",
          sprintf(
            paste0(
              "lavaan::efa at each k under %s, on the %d included items (n = %s%s; ",
              "unrotated, because lavaan's robust indices of a rotated model fit by FIML depend on the rotation). "
            ),
            f$label, ncol(x_data()), if (is.finite(f$n %||% NA)) format(f$n) else "not available",
            paste0(", ", sieve_efa_missing_words(f$lavaan_missing, f$missing))
          ),
          if (!is.na(f$family %||% NA_character_)) {
            paste(f$family_reason, sieve_family_text(f$family, categorical,
              normal_theory = identical(f$estimator, "ML")), "")
          } else if (!is.null(f$family_reason)) {
            paste(f$family_reason, "")
          },
          paste0(
            "The indices are shown for every k, the RMSEA with its interval; no value of an index is applied ",
            "as a cutoff, and no number of factors is read from them. ",
            sprintf("Read the %s RMSEA interval with the point estimate, and watch for diminishing ",
              sieve_ci_percent(sieve_valid_conf_level(f$conf_level, sieve_default_rmsea_conf_level()))),
            "returns: once added factors stop materially improving fit, the extra factor is ",
            "likely minor or a method artifact (Finch, 2020; Xia, 2021).",
            if (length(no_df)) {
              sprintf(
                " At k = %s the model has no positive degrees of freedom, so its indices do not test fit.",
                paste(no_df, collapse = ", ")
              )
            } else {
              ""
            },
            if (length(f$not_identified)) {
              sprintf(
                paste0(
                  " Not fit: k = %s, where the model has more parameters than the ",
                  "correlations can identify (negative degrees of freedom)."
                ),
                paste(f$not_identified, collapse = ", ")
              )
            } else {
              ""
            }
          )
        ),
        decision_line()
      )
    })

    fit_figure <- function() {
      ft <- fit_res()
      req(ft)
      cols <- sieve_palette_colors(state$palette %||% "nd", 5L)
      navy <- cols[1]
      gold <- cols[2]
      green <- cols[3]
      long <- rbind(
        data.frame(k = ft$k, value = ft$rmsea, index = "RMSEA"),
        data.frame(k = ft$k, value = ft$cfi, index = "CFI"),
        data.frame(k = ft$k, value = ft$tli, index = "TLI")
      )
      long <- long[is.finite(long$value), , drop = FALSE]
      req(nrow(long) > 0)
      pal <- c(RMSEA = navy, CFI = gold, TLI = green)
      # Each RMSEA with its interval (at the RMSEA level); no reference line
      # is drawn, because no value of an index is applied as a cutoff.
      bars <- data.frame(k = ft$k, lo = ft$rmsea_lo, hi = ft$rmsea_hi, index = "RMSEA")
      bars <- bars[is.finite(bars$lo) & is.finite(bars$hi), , drop = FALSE]
      level <- sieve_ci_percent(sieve_valid_conf_level(fit_run()$conf_level, sieve_default_rmsea_conf_level()))
      gg <- ggplot2::ggplot(
        long, ggplot2::aes(
          x = .data$k, y = .data$value,
          color = .data$index, group = .data$index
        )
      )
      if (nrow(bars)) {
        gg <- gg + ggplot2::geom_errorbar(
          data = bars,
          ggplot2::aes(x = .data$k, ymin = .data$lo, ymax = .data$hi, color = .data$index),
          inherit.aes = FALSE, width = 0.15, linewidth = 0.5
        )
      }
      gg <- gg +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(size = 2.4)
      k <- theory_k()
      if (!is.null(k)) {
        gg <- gg + ggplot2::geom_vline(
          xintercept = k, linetype = "dashed",
          color = "#5b6770", linewidth = 0.5
        )
      }
      dk <- decision_k()
      if (!is.null(dk)) {
        gg <- gg + ggplot2::geom_vline(
          xintercept = dk, linetype = "dotdash",
          color = "#8a3b12", linewidth = 0.6
        )
      }
      gg +
        ggplot2::scale_color_manual(values = pal, name = NULL) +
        ggplot2::scale_x_continuous(breaks = ft$k) +
        ggplot2::labs(
          x = "Number of factors (k)", y = "Fit index",
          subtitle = paste0(
            sprintf(
              "lavaan::efa under %s%s. ", fit_run()$label %||% "the estimator in force",
              if (is.na(fit_run()$family %||% NA_character_)) "" else sprintf(", %s family", fit_run()$family)
            ),
            if (nrow(bars)) sprintf("Bars: the %s RMSEA interval. ", level) else "",
            "Dashed vertical: theory k.",
            if (!is.null(dk)) sprintf(" Dot-dash vertical: your decision, k = %d.", dk) else ""
          )
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "top",
          plot.subtitle = ggplot2::element_text(color = "#5b6770", size = 10)
        )
    }
    output$fit_plot <- renderPlot(fit_figure(), res = 96,
      alt = "Fit indices of the exploratory solutions by number of factors, with the RMSEA interval. The values are in the fit table on this page."
    )
    sieve_plot_save_server("fit_plot", output, session, state,
      draw = fit_figure,
      file_stem = "siever_fit_by_factors",
      what = "the fit by number of factors (Dimensionality tab)"
    )

    output$fit_tbl <- renderDT(
      {
        ft <- fit_res()
        req(ft)
        rmsea_ci <- ifelse(
          is.finite(ft$rmsea_lo) & is.finite(ft$rmsea_hi),
          sprintf("[%.3f, %.3f]", ft$rmsea_lo, ft$rmsea_hi),
          ""
        )
        # What a row needs said beside its numbers: why a solution was not
        # fit, the engine's note on an inadmissible one, or degrees of
        # freedom that leave nothing to test. No row is graded. An improper
        # solution without positive degrees of freedom is said to be both.
        df_note <- ifelse(!is.finite(ft$df), "degrees of freedom not available",
          ifelse(ft$df <= 0, "no positive degrees of freedom: the indices do not test fit", "")
        )
        note <- ifelse(
          ft$status != "ran", paste("not fit:", ft$reason),
          ifelse(is.na(ft$reason), df_note,
            ifelse(nzchar(df_note), paste0(sub("[.]\\s*$", "", ft$reason), "; ", df_note), ft$reason)
          )
        )
        f <- fit_run()
        tag <- if (is.na(f$family %||% NA_character_)) "" else sprintf(" (%s)", f$family)
        df <- data.frame(
          Factors = ft$k,
          `Chi-square` = round_or_na(ft$chisq),
          df = round_or_na(ft$df),
          p = round_or_na(ft$pval),
          RMSEA = round_or_na(ft$rmsea),
          rmsea_ci = rmsea_ci,
          CFI = round_or_na(ft$cfi),
          TLI = round_or_na(ft$tli),
          SRMR = round_or_na(ft$srmr),
          BIC = round_or_na(ft$bic),
          Note = note,
          `Engine warnings` = ifelse(is.na(ft$warnings), "", ft$warnings),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        names(df)[names(df) == "Chi-square"] <- if (identical(
          f$test_family,
          "scaled"
        )) "Chi-square (scaled)" else "Chi-square"
        names(df)[names(df) == "rmsea_ci"] <- sprintf("RMSEA %s", sieve_rmsea_ci_words(f$conf_level %||% NA_real_))
        for (index in c("RMSEA", sprintf("RMSEA %s", sieve_rmsea_ci_words(f$conf_level %||% NA_real_)), "CFI", "TLI")) {
          names(df)[names(df) == index] <- paste0(index, tag)
        }
        # The two text columns hold sentences and get the room.
        widths <- c("6%", "8%", "5%", "6%", "6%", "11%", "6%", "6%", "6%", "7%", "18%", "15%")
        sieve_dt(
          df,
          rownames = FALSE, selection = "none",
          options = list(
            dom = "t", pageLength = nrow(df), ordering = FALSE, autoWidth = FALSE,
            columnDefs = lapply(seq_along(widths), function(i) {
              list(width = widths[i], targets = i - 1L)
            })
          )
        )
      },
      server = FALSE
    )

    # ---- Evidence by criterion ----------------------------------------------
    output$summary_reco <- renderUI({
      req(x_data())
      b <- basis()
      if (isTRUE(b$waiting)) {
        return(tagList(basis_waiting_ui(b), decision_line()))
      }
      cr <- criteria()
      st <- sieve_retention_statements(cr, evidence_summary())
      tagList(
        basis_note(b),
        # The heading comes with the statements: "Agreement" needs at least
        # two criterion families with one and the same count.
        div(
          class = paste0("alert alert-", st$tone),
          tags$b(paste0(st$heading, " ")),
          st$range,
          if (!is.null(st$unavailable)) tagList(tags$br(), st$unavailable)
        ),
        if (!is.null(st$identified)) div(class = "alert alert-warning", st$identified),
        if (!is.null(st$tally)) {
          div(class = "small", tags$p(st$tally), tags$p(st$neighborhood))
        },
        if (!is.null(st$theory)) div(class = "small", tags$p(st$theory)),
        div(
          class = "small",
          tags$b("SIEVER does not choose the number of factors. "),
          "Record your decision at the top of the tab; a rationale is optional."
        ),
        decision_line()
      )
    })

    # An HTML table, not a data table widget: the cells hold sentences (what
    # produced each count and what it assumes), which must wrap in full.
    output$summary_tbl <- renderUI({
      cr <- criteria()
      s <- suggestions()
      ran <- c(
        ran = "yes", "no count" = "yes, but it gives no count", failed = "no",
        "not run" = "not yet"
      )
      head <- c(
        "Criterion", "Suggested factors, or why there is no count", "Correlations requested",
        "Correlations used", "Did it run?", "What produced it, and what it assumes"
      )
      rows <- lapply(seq_len(nrow(cr)), function(i) {
        has_count <- is.finite(cr$suggested[i])
        tags$tr(
          tags$td(cr$criterion[i]),
          # The count, or in its place the reason there is none; a note on
          # a count that exists (for example, more than an exploratory
          # model identifies) stays with what produced it.
          tags$td(
            if (has_count) {
              tags$b(fmt_k(cr$suggested[i]))
            } else if (is.finite(cr$at_least[i] %||% NA_integer_)) {
              # A minimum at the edge of the probed range: a lower bound.
              tagList(
                tags$b(sprintf("at least %d", cr$at_least[i])),
                tags$div(class = "small text-muted", cr$reason[i])
              )
            } else if (identical(cr$status[i], "not run") && identical(cr$id[i], "vss2")) {
              # One run gives both complexities; its button is on the first row.
              tags$span(class = "text-muted", "Not run yet; see VSS complexity 1 above.")
            } else if (identical(cr$status[i], "not run") && cr$id[i] %in% c("pa", "vss1")) {
              tagList(
                tags$span(class = "text-muted", cr$reason[i]),
                tags$div(
                  class = "mt-1",
                  if (identical(cr$id[i], "vss1")) {
                    actionButton(session$ns("vss_go_table"), "Run VSS", class = "btn-primary btn-sm")
                  } else {
                    actionButton(session$ns("pa_go_table"), "Run parallel analysis",
                      class = "btn-primary btn-sm"
                    )
                  }
                )
              )
            } else {
              tags$span(
                class = "text-muted",
                if (is.na(cr$reason[i])) "No count." else cr$reason[i]
              )
            }
          ),
          tags$td(sieve_retention_basis_label(cr$basis_requested[i])),
          tags$td(
            class = if (isTRUE(cr$basis_changed[i])) "table-warning",
            if (isTRUE(cr$basis_changed[i])) {
              sprintf(
                "%s (requested: %s)", sieve_retention_basis_label(cr$basis_used[i]),
                sieve_retention_basis_label(cr$basis_requested[i])
              )
            } else {
              sieve_retention_basis_label(cr$basis_used[i])
            }
          ),
          tags$td(unname(ran[cr$status[i]])),
          tags$td(
            tags$div(cr$produced_by[i]),
            tags$div(class = "text-muted", cr$assumptions[i]),
            if (has_count && !is.na(cr$reason[i])) tags$div(tags$b("Note: "), cr$reason[i])
          )
        )
      })
      div(
        class = "table-responsive",
        tags$table(
          class = "table table-sm small align-top",
          tags$thead(tags$tr(lapply(head, tags$th))),
          tags$tbody(
            tags$tr(
              tags$td("Theorized number of factors (reference, not a criterion)"),
              tags$td(tags$b(fmt_k(s$theory))), tags$td(""), tags$td(""), tags$td(""),
              tags$td("Your researcher-specified structure.")
            ),
            rows
          )
        )
      )
    })

    output$summary_note <- renderUI({
      p(
        class = "text-muted small mt-2",
        paste0(
          "Each row is one criterion read on its own. The theorized number is your specified ",
          "structure and serves as a reference. The random parts ran from ",
          seed_text(run_rng()), ". Where the criteria disagree, fit the candidate solutions ",
          "and weigh their interpretability, their loadings and factor correlations, and ",
          "their fit together; no index and no count of indices settles the number."
        )
      )
    })

    # ---- The researcher's decision ------------------------------------------
    decide_msg <- reactiveVal(NULL)

    # What the pool allows, said before the number is entered: the whole
    # numbers the record accepts, and the largest number an exploratory
    # model of these items identifies. The number is not capped at that
    # maximum, because a restricted (confirmatory) model with more factors
    # can be identified; a larger number is recorded as entered, with a note.
    output$decide_bounds <- renderUI({
      X <- x_data()
      req(X)
      p <- ncol(X)
      max_id <- sieve_max_identified_k(p)
      updateNumericInput(session, "decide_k", max = p - 1L)
      div(
        class = "small text-muted mb-1",
        sprintf("The included pool has %d items, so a whole number from 0 to %d can be recorded.", p, p - 1L),
        tags$details(
          tags$summary(sprintf(
            "An exploratory factor model of these items is identified for at most %d factor%s.",
            max_id, if (max_id == 1L) "" else "s"
          )),
          paste0(
            "The EFA tab fits exploratory models; a larger number is recorded as entered and ",
            "noted as such, since a confirmatory model with restrictions may still be identified."
          )
        )
      )
    })

    observeEvent(input$decide_go, {
      X <- x_data()
      if (is.null(X)) {
        decide_msg(list(ok = FALSE, text = "There is no item pool to decide about yet."))
        return()
      }
      k <- input$decide_k
      # The rationale is optional: a blank one is recorded as "no rationale
      # given" (sieve_record_decision), never refused.
      why <- input$decide_why %||% ""
      whole <- length(k) == 1L && is.finite(k) && k >= 0 && abs(k - round(k)) < 1e-8
      if (!whole || k > ncol(X) - 1L) {
        decide_msg(list(ok = FALSE, text = sprintf(
          "Enter the number of factors retained: a whole number from 0 to %d.", ncol(X) - 1L
        )))
        return()
      }
      cr <- tryCatch(criteria(), error = function(e) NULL)
      # A record is appended only while every earlier record is what was
      # written (sieve_record_decision verifies their keys); a refusal is
      # shown here and ends the session's work no more than any other
      # message does.
      rec <- tryCatch(
        sieve_record_decision(
          state,
          topic = "factor_retention",
          question = sprintf("How many factors are retained for the %d included items?", ncol(X)),
          choice = as.integer(round(k)),
          rationale = why,
          evidence = if (is.null(cr)) {
            "No retention criterion had been computed when the decision was recorded."
          } else {
            sieve_retention_evidence_lines(cr, theory_k())
          }
        ),
        error = function(e) e
      )
      if (inherits(rec, "error")) {
        decide_msg(list(ok = FALSE, text = conditionMessage(rec)))
        return()
      }
      # The number is the researcher's and is recorded as entered. When it is
      # more than an exploratory model can carry for this pool, that is said,
      # because the next tab fits exploratory models; a restricted
      # (confirmatory) model with that many factors may still be identified.
      max_id <- sieve_max_identified_k(ncol(X))
      decide_msg(list(ok = TRUE, text = paste0(
        sprintf(
          "Recorded as %s: %d factor%s retained%s%s. Nothing else was changed.",
          rec$id, rec$choice, if (rec$choice == 1L) "" else "s",
          if (is.na(rec$supersedes)) "" else sprintf(" (replaces %s, which is kept in the record)", rec$supersedes),
          if (identical(rec$rationale, sieve_no_rationale())) ", no rationale given" else ""
        ),
        if (rec$choice > max_id) {
          sprintf(
            paste0(
              " Note that an exploratory model with %d factors has negative degrees of freedom ",
              "for %d items (at most %d can be fit as an exploratory model); a confirmatory ",
              "model with restrictions may still be identified."
            ),
            rec$choice, ncol(X), max_id
          )
        } else {
          ""
        }
      )))
    })

    output$decide_status <- renderUI({
      msg <- decide_msg()
      current <- sieve_current_decision(state, "factor_retention", state$dataset_id %||% NA_character_)
      # A decision belongs to the inputs it was made on. sieve_revision()
      # reads in isolation, so the dependency on the data and the ledger is
      # taken here.
      state$items
      state$meta
      stale <- !is.null(current) &&
        !sieve_same_revision(current$revision, sieve_revision(state), what = c("data", "meta"))
      # The record lists what was looked at. When the evidence on screen is
      # no longer that (another seed, another setting), the tab says so; the
      # record itself is not touched.
      now <- if (!is.null(current) && !stale) {
        cr <- tryCatch(criteria(), error = function(e) NULL)
        if (!is.null(cr)) sieve_retention_evidence_lines(cr, theory_k())
      }
      moved <- !is.null(now) && !identical(now, current$evidence)
      # A record is appended and never edited. The key written to the
      # reproducible code when it was recorded makes a later edit visible.
      check <- sieve_decisions_check(state)
      altered <- check$id[check$intact %in% FALSE]
      tagList(
        if (!is.null(msg)) {
          div(
            class = if (isTRUE(msg$ok)) "alert alert-success mt-2" else "alert alert-danger mt-2",
            msg$text
          )
        },
        if (is.null(current)) {
          div(class = "small text-muted mt-2", "No factor retention decision has been recorded for these data.")
        } else {
          div(
            class = "small mt-2",
            tags$b(sprintf(
              "Decision in force (%s): %s factor(s) retained. ",
              current$id, format(current$choice)
            )),
            if (identical(current$rationale, sieve_no_rationale())) {
              "No rationale was given."
            } else {
              paste0("Rationale: ", current$rationale)
            }
          )
        },
        if (stale) {
          div(
            class = "alert alert-warning mt-2",
            "The data or the included items have changed since this decision was recorded, ",
            "so it was made on different inputs. It stays in the record as written; record ",
            "a new decision if it no longer holds."
          )
        },
        if (moved) {
          div(
            class = "alert alert-warning mt-2",
            "The evidence shown above is not the evidence this decision lists: the criteria ",
            "have since been computed with another seed or other settings. The record keeps ",
            "what was looked at when the decision was made; record a new decision if the ",
            "evidence now on screen changes it."
          )
        },
        if (length(altered)) {
          div(
            class = "alert alert-danger mt-2",
            sprintf(
              paste0(
                "Record %s no longer matches the key written to the reproducible code when it ",
                "was recorded, so it was changed afterwards. Decision records are not edited; ",
                "the reproducible code holds the record as it was made."
              ),
              paste(altered, collapse = ", ")
            )
          )
        }
      )
    })

    # The evidence in brief, beside the control: the heading and the range
    # of the counts, then one line per criterion, then the theorized number
    # and the decision in force. The full table and its reasons are on the
    # Evidence and decision panel.
    output$decide_evidence <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed) || is.null(x_data())) {
        return(div(
          class = "small text-muted",
          "The evidence is read once the item types are confirmed and the item pool holds at least three items."
        ))
      }
      read <- sieve_outcome({
        cr <- criteria()
        list(cr = cr, st = sieve_retention_statements(cr, evidence_summary()))
      })
      if (!read$ready) {
        return(div(class = "small text-muted", "The evidence is being read."))
      }
      if (!read$ok) {
        return(div(
          class = "small text-danger",
          "The evidence could not be read: ", read$error
        ))
      }
      cr <- read$value$cr
      st <- read$value$st
      counts <- vapply(seq_len(nrow(cr)), function(i) {
        sprintf(
          "%s: %s", sieve_retention_names(cr$id[i]),
          if (is.finite(cr$suggested[i])) {
            as.character(cr$suggested[i])
          } else if (is.finite(cr$at_least[i] %||% NA_integer_)) {
            sprintf("at least %d", cr$at_least[i])
          } else if (identical(cr$status[i], "not run")) {
            "not run yet"
          } else {
            "no count"
          }
        )
      }, character(1))
      tk <- theory_k()
      tagList(
        div(class = "small", tags$b(paste0(st$heading, " ")), st$range),
        tags$ul(class = "small mb-1", lapply(counts, tags$li)),
        if (!is.null(tk)) {
          div(
            class = "small text-muted",
            sprintf(
              "Theorized number of factors (a reference, not a criterion): %d.",
              tk
            )
          )
        },
        decision_line()
      )
    })

    output$decide_tbl <- renderDT(
      {
        tab <- sieve_decisions_table(state)
        tab <- tab[tab$topic == "factor_retention", , drop = FALSE]
        req(nrow(tab) > 0)
        df <- data.frame(
          Record = tab$id, `Factors retained` = tab$choice, Rationale = tab$rationale,
          Status = ifelse(tab$status == "current", "current",
            sprintf("replaced by %s", tab$superseded_by)
          ),
          Dataset = ifelse(is.na(tab$dataset_id), "", paste("dataset", tab$dataset_id)),
          `Recorded (UTC)` = tab$recorded_at,
          check.names = FALSE, stringsAsFactors = FALSE
        )
        sieve_dt(
          df,
          rownames = FALSE, selection = "none",
          options = list(dom = "t", pageLength = nrow(df), ordering = FALSE)
        )
      },
      server = FALSE
    )

    output$decisions_csv <- downloadHandler(
      filename = function() "siever_decisions.csv",
      content = function(file) {
        utils::write.csv(isolate(sieve_decisions_table(state)), file, row.names = FALSE)
      }
    )
  })
}

# Format a possibly-NA factor count for display.
# Format a Monte Carlo limit: three decimals, or the infinite limit in words.
fmt_lim <- function(x) {
  if (length(x) != 1L || is.na(x)) return("not available")
  if (!is.finite(x)) return(if (x > 0) "infinity" else "minus infinity")
  sprintf("%.3f", x)
}

fmt_k <- function(k) {
  if (is.null(k) || length(k) != 1L || is.na(k) || !is.finite(k)) "not available" else as.character(as.integer(k))
}

# Round a numeric vector for a DT column, leaving NA where undefined.
round_or_na <- function(v) {
  if (is.null(v)) return(NA_real_)
  out <- suppressWarnings(as.numeric(v))
  round(out, 4)
}

# Coerce a single value to numeric, returning NA when missing or non-finite.
#' The estimated wait of Very Simple Structure
#'
#' `DMAR::fa_exploratory()` fits a maximum likelihood solution, rotated by
#' direct oblimin, for every identified number of factors from 1 to
#' `max_k` (`sieve_vss_ks()`), each estimated by `sieve_efa_point_wait()`
#' (a solution expected to be refused at a refusal's cost). The solutions
#' are point estimates only, whose time grows with the items: with DMAR
#' 1.1.0, eight factors on 68 items take about 15 seconds in all
#' (estimated at 14.9), and one to seven factors on 24 items about 2
#' (estimated at 2.2 for one to eight). A first result runs by itself at two
#' seconds or less (`sieve_runs_by_itself()`); a longer one waits for Run
#' VSS and then runs in a background R process.
#'
#' @param max_k The largest number of factors probed (after
#'   `sieve_retention_max_k()`).
#' @param p The number of items.
#' @param R Optional: the correlation matrix VSS reads. A bounded maximum
#'   likelihood screen of it (`sieve_efa_boundary_ks()`) marks the ranks
#'   DMAR is expected to refuse, which can take far longer than a proper
#'   fit (`sieve_efa_point_wait(refused = TRUE)`).
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_vss_wait <- function(max_k, p, R = NULL) {
  max_k <- suppressWarnings(as.numeric(max_k))
  p <- suppressWarnings(as.numeric(p))
  if (length(max_k) != 1L || !is.finite(max_k) || length(p) != 1L || !is.finite(p)) {
    return(NA_real_)
  }
  ks <- sieve_vss_ks(max_k, p)
  if (!length(ks)) return(0)
  refused <- if (is.matrix(R) && ncol(R) == p) sieve_efa_boundary_ks(R, ks) else FALSE
  sum(sieve_efa_point_wait(p, ks, "matrix", refused = refused))
}

#' Very Simple Structure that have not run
#'
#' The record `sieve_retention_vss()` returns, for a run that waits: a first
#' run estimated above two seconds (its button states the wait), or a result
#' from before an edit (the page's Update computes it again).
#'
#' @param basis The result of `sieve_retention_cor()`.
#' @param rng The seed record the run would use.
#' @param max_k The largest number of factors it would probe.
#' @param seconds Its estimated wait.
#' @param stale `TRUE` when an earlier result for these data is from before.
#' @return A list shaped as `sieve_retention_vss()`'s, with
#'   `status = "not run"`, `stale`, and `seconds`.
#' @author Ken Kelley
#' @noRd
sieve_vss_not_run <- function(basis, rng, max_k, seconds, stale = FALSE) {
  K <- sieve_retention_max_k(max_k, basis$p)
  reason <- if (isTRUE(stale)) {
    sprintf(
      paste0(
        "Very Simple Structure are from before: the included items, their values, the ",
        "respondents or the settings changed since they ran. Update, in the status line, ",
        "computes them again (%s)."
      ),
      sieve_duration_label(seconds)
    )
  } else {
    ks <- sieve_vss_ks(K, basis$p)
    sprintf(
      paste0(
        "Very Simple Structure have not run yet. They would fit %d maximum likelihood ",
        "solutions by DMAR::fa_exploratory (k = %s) with direct oblimin rotation; the estimated wait is %s, so they ",
        "wait for Run VSS%s."
      ),
      length(ks), if (length(ks) > 1L) sprintf("1 to %d", max(ks)) else paste(ks, collapse = ", "),
      sieve_duration_label(seconds),
      if (is.finite(seconds) && seconds > sieve_auto_seconds()) ", and then run in a background R process" else ""
    )
  }
  list(
    ok = FALSE, status = "not run", reason = reason, stale = isTRUE(stale), seconds = seconds,
    warnings = character(0), messages = character(0),
    basis_requested = basis$requested, basis_used = basis$used,
    max_k = K, k = integer(0), vss1 = NULL, vss2 = NULL, identified = logical(0),
    max_identified = sieve_max_identified_k(basis$p), p = basis$p,
    rng = rng, engine = NULL
  )
}

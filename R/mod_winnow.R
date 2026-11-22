# mod_winnow.R
# The Items tab: the item organizer, the heart of SIEVER. Items are shown by
# construct with their wording on click; the user includes or excludes items
# (always reversible) and reassigns them between constructs, with an immediate
# refit of the live per item diagnostics.
#
# The live diagnostics are EXPLORATORY: an exploratory factor analysis
# (DMAR::fa_exploratory, maximum likelihood, oblimin rotation) extracting as many factors as there are
# constructs (or facets, for one construct split into facets), fit to the currently included items. Loadings, communalities,
# and cross loadings come from that EFA; per construct reliability comes from a
# single factor model on each construct's items. This is the judgment driven,
# data structure free view; the confirmatory model (CFA) lives on the CFA and
# SEM tab. Screening thresholds only color the flags, so moving a slider is
# instant and never refits the model; nor does a label, a wording, or a facet
# edit, since the diagnostics are kept under the identity of what they are
# computed from (included items, their responses and types, constructs).

#' Winnowing engine UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput actionButton helpText selectInput sliderInput radioButtons
#' @importFrom shiny hr p strong tags div tagList icon checkboxInput downloadButton
#' @importFrom bslib layout_sidebar sidebar card card_header accordion
#' @importFrom bslib accordion_panel
#' @importFrom DT DTOutput
mod_winnow_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      open = "desktop",
      width = 360,
      title = "Selected item",
      uiOutput(ns("detail")),
      helpText(
        "Edit in the table cells: type a ", strong("Construct"), " or ", strong("Facet"),
        " (pick an existing name or type a new one), and set ", strong("Included / Excluded"), ". ",
        "Excluding an item takes it out of the active pool and out of every fit, reliability, ",
        "and diagnostic; it is never deleted, so it can be included again at any time. ",
        "Click a row to read its full wording above."
      ),
      hr(),
      radioButtons(
        ns("coef"), "Reliability coefficient",
        choices = c(
          "Coefficient Omega, \u03c9, robust (observed denominator; recommended)" = "omega",
          "Coefficient Alpha, \u03b1 (legacy)" = "alpha"
        ),
        selected = "omega"
      ),
      radioButtons(
        ns("factor_cov"), "Overall model factors",
        choices = c("Correlated (default)" = "correlated", "Orthogonal (uncorrelated)" = "orthogonal"),
        selected = "correlated"
      ),
      sieve_estimator_panel(ns, "Overall model estimator"),
      accordion(
        open = "Diagnostic thresholds",
        accordion_panel(
          "Diagnostic thresholds",
          helpText(
            "Screening conventions, not decision rules: they only color the flags, ",
            "so changing one is instant and does not refit the model."
          ),
          sliderInput(ns("load_low"), "Low loading below", 0.20, 0.60, 0.40, 0.01),
          helpText(class = "text-muted small mt-0", "Flag an item whose loading on its own construct is weak."),
          sliderInput(ns("salient"), "Salient cross loading at", 0.20, 0.50, 0.30, 0.01),
          helpText(class = "text-muted small mt-0", "How large an off-target loading counts as a cross loading."),
          sliderInput(ns("communality_low"), "Low communality below", 0.10, 0.50, 0.30, 0.01),
          helpText(class = "text-muted small mt-0", "Flag an item with little shared variance (low h2)."),
          sliderInput(ns("item_total_low"), "Low construct item-rest r below", 0.10, 0.50, 0.30, 0.01),
          helpText(class = "text-muted small mt-0", "Flag an item weakly correlated with the rest of its construct."),
          sliderInput(ns("redundancy_high"), "Redundant inter-item r above", 0.60, 0.95, 0.80, 0.01),
          helpText(
            class = "text-muted small mt-0",
            "Flag a near-duplicate of another item (possible local dependence)."
          )
        )
      ),
      helpText(
        "A soft warning appears when a construct drops below three included indicators."
      )
    ),
    card(
      card_header("Item pool by construct, with live diagnostics"),
      bslib::card_body(
        class = "sieve-item-workspace",
        # Results first: the item grid is the page's result.
        # Above it, always visible: the one-line statement of the model
        # behind the loadings and h2, the one
        # status line (errors in the diagnostics or in an edit, an improper
        # solution, and the page's one Update when something waits), the
        # overall model in one line, the per construct reliability that
        # gives "if dropped" its baseline, and the grid's row of controls.
        # Below it: how the diagnostics are computed (the first paragraph
        # visible, the rest closed), the column guide, and the tools.
        uiOutput(ns("model_line")),
        # The code behind the loadings, h2, cross loadings and construct
        # reliability, under the model line.
        uiOutput(ns("diag_code")),
        uiOutput(ns("status_line")),
        uiOutput(ns("overall_fit")),
        uiOutput(ns("model_code")),
        uiOutput(ns("rel_summary")),
        div(
          class = "d-flex align-items-end gap-2 my-1 flex-wrap",
          radioButtons(ns("information_view"), "Item information",
            choices = c("Basic" = "basic", "Advanced" = "advanced"),
            selected = "basic", inline = TRUE
          ),
          actionButton(ns("exclude_selected"), "Exclude selected", class = "btn-outline-danger"),
          actionButton(ns("include_selected"), "Include selected", class = "btn-outline-primary"),
          tags$button(
            type = "button", class = "btn btn-outline-secondary sieve-reset-widths",
            `data-sieve-table` = "items-pool", "Reset column widths"
          )
        ),
        DTOutput(ns("pool")),
        uiOutput(ns("model_note")),
        tags$details(open = NA,
          tags$summary("More on how these diagnostics are computed"),
          uiOutput(ns("model_more")),
          uiOutput(ns("fit_note")),
          uiOutput(ns("rel_note")),
          uiOutput(ns("overall_note"))
        ),
        accordion(
          id = ns("column_guide"),
          open = FALSE,
          accordion_panel(
            "Column Guide (Expand for Definitions)",
            tags$ul(
              class = "small mb-0",
              tags$li(
                tags$b("loading"),
                ": the item's standardized loading on its own construct's factor, from the exploratory factor analysis described above. Interpret its magnitude with the model, uncertainty, and content; a larger loading alone does not establish better coverage."
              ),
              tags$li(
                tags$b("h2"),
                ": communality, the proportion of the item's variance explained by the factors (also from that EFA)."
              ),
              tags$li(
                tags$b("Construct item-rest r and Facet item-rest r"), ": corrected item-rest correlations. ",
                tags$em("Corrected"), " means the item itself is left out: it is the correlation of the item with the ",
                tags$b("mean"),
                " of the OTHER active items in its construct or its facet within that construct. The mean uses available responses; Pearson correlation uses finite item-mean pairs. Focus a correlation cell or select a row for analytic n and reasons an estimate is undefined."
              ),
              tags$li(
                tags$b("Robust \u03c9 / \u03b1 if dropped"),
                ": the change in the construct's reliability (the coefficient chosen at left: robust omega, observed denominator, or alpha) if this item were excluded; a positive value means reliability would rise without it. The construct's current reliability is shown above the table, with the largest residual correlation of its one-factor model: a rise that keeps a large residual pair signals redundancy, not better measurement."
              ),
              tags$li(
                tags$b("max |r|"),
                ": the item's largest absolute correlation with another item in the same construct, in the shared correlation matrix the EFA reads (its kind is named in the header: Pearson, polychoric or tetrachoric); a high value flags a near duplicate or local dependence."
              ),
              tags$li(
                tags$b("Diagnostic flags"),
                ": which screening cutoffs the item trips (load, comm, itc, rel up, dup), and keying: a salient loading opposite in sign to the other items of its factor. Low loading is judged by the size of the loading, so a strongly reverse keyed item is flagged for its keying, not as low loading. Every item-rest r and change if dropped of its construct reads a total in which that item subtracts: its own negative item-rest r and the rise in reliability without it, and the lowered item-rest r and rises of the correctly keyed items beside it, so itc and rel up are not flagged for any item of that construct until the keying is resolved. If it is reverse keyed, reverse score it on the Ingest tab (Reverse? Yes) rather than excluding it; the note above the grid names it."
              ),
              tags$li(
                tags$b("Cross loading"),
                ": from the same EFA, the item loads almost as strongly on another construct's factor as on its own (a salient off-target loading within 0.10 of the target). ",
                "It is an exploratory pattern to inspect with content: the arrow names the other factor, or ",
                tags$em("complex"),
                " if it spreads across several factors."
              )
            )
          )
        ),
        tags$details(open = NA,
          tags$summary("How to Use the Grid"),
          helpText("Hover or use keyboard focus for full column definitions and cell text. Select a row for its full name, wording, identifiers, and analytic sample. Excluded items remain in this table and can be included again. Column widths are remembered in this browser; Reset column widths clears that preference."),
          p(
            id = "items-pool-keyboard-help", class = "small text-muted",
            "Grid keyboard: arrow keys inspect cells; Enter or F2 edits; Escape returns to the cell; Tab leaves the grid. ",
            "Arrow down from the last row continues to the next page. ",
            "Arrow up from the first row reaches column definitions and resizing."
          )
        ),
        sieve_metadata_controls_ui(ns),
        # Gather one construct's included items and their wordings for a second
        # opinion: read them as a written description, or copy and paste into an
        # external LLM for construct refinement. Nothing is sent anywhere by the
        # app; this only formats text the user already has.
        tags$details(
          tags$summary("Prepare an Expert Review Brief"),
          div(
            class = "d-flex align-items-end gap-2 my-2 flex-wrap",
            div(
              style = "min-width:180px;",
              selectInput(ns("review_construct"), "Review a construct's items",
                choices = character(0), width = "100%"
              )
            ),
            div(
              style = "min-width:150px;",
              selectInput(ns("review_facet"), "Facet (optional)",
                choices = c("All facets" = ""), width = "100%"
              )
            ),
            radioButtons(ns("review_mode"), "Output",
              choices = c("Structured expert brief" = "prompt", "Written description" = "description"),
              selected = "prompt", inline = TRUE
            ),
            div(
              shiny::checkboxInput(ns("review_diag"), "Include diagnostics", value = TRUE),
              shiny::checkboxInput(ns("review_all"), "All constructs", value = FALSE)
            ),
            actionButton(ns("review_copy"),
              label = tagList(shiny::icon("clipboard"), "Expert review brief"),
              class = "btn-outline-primary"
            )
          )
        )
      )
    )
  )
}

#' Winnowing engine server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer observe observeEvent req reactive renderUI
#' @importFrom shiny updateSelectizeInput updateSelectInput debounce isolate em strong div tags br HTML
#' @importFrom shiny showModal modalDialog modalButton tagList icon downloadHandler
#' @importFrom shiny withProgress reactiveVal actionButton
#' @importFrom DT renderDT datatable formatStyle formatRound styleEqual styleInterval
mod_winnow_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    # The overall model's Estimator control: the study's setting (Ingest
    # tab) unless chosen otherwise here (sieve_estimator_wire()).
    estimator_choice <- sieve_estimator_wire(input, output, state)
    ns <- session$ns

    # Threshold sliders feed the shared state. They only affect flag coloring
    # (applied at display time), never the model fit.
    observe({
      req(state$meta)
      state$thresholds <- list(
        load_low = input$load_low %||% 0.40,
        salient = input$salient %||% 0.30,
        communality_low = input$communality_low %||% 0.30,
        item_total_low = input$item_total_low %||% 0.30,
        redundancy_high = input$redundancy_high %||% 0.80
      )
    })

    # The expensive part: the EFA and the reliability statistics. They are
    # kept under the analytic identity of the item pool (sieve_keep): the
    # included items with their responses, their confirmed types, and the
    # constructs they are assigned to (what enters the exploratory model and
    # the per construct reliability), and for a one-construct study the
    # facets, which are then the factors. A
    # label, a wording, a facet of a study with several constructs, or a
    # threshold is not part of that identity, so an edit to one recomputes
    # nothing: the reactive is evaluated again (it reads the ledger) and
    # returns the kept result. The per construct reliability blocks are
    # kept one level down as well (rel_kept), so that moving or removing
    # one item recomputes the constructs it touched and no other.
    #
    # Nothing here is debounced: a debounced reactive is evaluated by its
    # own observer whether or not the tab is shown, and before the item
    # types are confirmed, so a debounced reactive here would fit a model at
    # Load. These reactives run when an output on the tab reads them,
    # and only once the types are confirmed. An error inside them is
    # captured (sieve_capture) and shown in the tab's status line rather
    # than lost.
    kept <- new.env(parent = emptyenv())
    kept$diag_asked <- character(0) # the datasets the diagnostics were computed for
    rel_kept <- new.env(parent = emptyenv())
    cores <- sieve_cores()
    diag_key <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      pool <- sieve_state_items(state)
      req(pool, length(pool$items) > 0L)
      m <- state$meta[state$meta$status == "active", , drop = FALSE]
      list(
        data = sieve_data_key(as.data.frame(state$items)[, pool$items, drop = FALSE]),
        items = pool$items,
        types = unname(pool$types),
        constructs = as.character(m$construct)[match(pool$items, m$item)],
        facets = if (length(unique(m$construct)) == 1L && "facet" %in% names(m)) {
          as.character(m$facet)[match(pool$items, m$item)]
        },
        dataset = state$dataset_id,
        rows = sieve_hash(state$case_index %||% seq_len(nrow(state$items))),
        # The correlations the study's estimator setting asks for (the
        # Ingest tab): the shared basis the EFA reads follows it, so a
        # change of it is a new identity.
        basis = sieve_study_basis_request(state),
        ordered = sieve_lavaan_spec(state$meta, estimator = sieve_estimator_effective("study", state))$ordered %||% character(0),
        # What each construct's reliability reads besides its items: the
        # study's missing data treatment and, under FIML, the declared
        # auxiliaries (sieve_items_reliability_inputs()), as on the
        # Reliability tab.
        reliability = sieve_items_reliability_inputs(state)$key
      )
    })
    # The run policy (sieve_runs_by_itself(live = TRUE)): the diagnostics
    # follow each inclusion, exclusion or reassignment by themselves while
    # what is to be computed (the factor analysis and the constructs whose
    # block is not kept) is estimated at two seconds or less; otherwise
    # they wait for Update, which states the estimate, and the earlier
    # values are withheld rather than shown as current.
    # The shared basis of the pool when it is kept (it is not computed for
    # a plan), to read whether the EFA page's solution is this model.
    held_basis <- function(key) {
      resolved <- sieve_retention_request(key$basis, unname(key$types))
      held <- isolate(state$cache)$basis[[resolved]]
      if (is.null(held) || !identical(held$key$data, sieve_state_items(state)$key)) NULL else held$value
    }
    diag_plan_at <- function(key) {
      b <- held_basis(key)
      reuse <- if (!is.null(b)) sieve_efa_store_match(sieve_efa_store(state, create = FALSE), diag_want(key, b))
      rel_in <- sieve_items_reliability_inputs(state)
      plan <- sieve_diagnostics_plan(rel_in$data, state$meta, keep = rel_kept, ordered = key$ordered, workers = cores,
        efa = !isTRUE(reuse$usable), R = if (isTRUE(b$ok)) b$R, missing = rel_in$missing, aux = rel_in$aux)
      if (!is.null(plan)) {
        # The shared correlation matrix is part of the wait when it is not
        # kept yet (polychoric correlations take seconds per hundred pairs).
        if (is.null(b)) {
          resolved <- sieve_retention_request(key$basis, unname(key$types))
          wait <- sieve_basis_wait(length(key$items), resolved)
          if (is.finite(wait)) plan$seconds <- plan$seconds + wait
        }
        plan$label <- sieve_duration_label(plan$seconds)
      }
      plan
    }
    diag_plan <- reactive({
      key <- diag_key()
      # The EFA page's solution, when it keeps one, may be this model.
      store <- sieve_efa_store(state, create = FALSE)
      if (!is.null(store$signal)) store$signal()
      plan <- diag_plan_at(key)
      # The run policy decides at the standard rates (sieve_at_default_rates()).
      if (!is.null(plan)) plan$standard_seconds <- sieve_at_default_rates(diag_plan_at(key))$seconds
      plan
    })
    diag_request <- reactiveVal(NULL)
    diag_done <- reactiveVal(0L)
    # `progress = FALSE` when the page's Update runs it under its own
    # progress message (one message for everything Update recomputes).
    # The model the diagnostics fit, as the EFA tab's shared store names
    # it (sieve_efa_store_match()): k is one factor per construct, or per
    # facet of the one construct.
    diag_want <- function(key, b) {
      grouping <- sieve_structure_groups(isolate(state$meta))
      list(
        dataset_id = key$dataset, data = key$data, items = key$items, types = key$types, rows = key$rows,
        k = if (identical(grouping$level, "facet")) length(grouping$groups) else length(unique(key$constructs)),
        basis_used = if (isTRUE(b$ok)) b$used else NA_character_, basis_missing = b$missing,
        n = b$n %||% NA_integer_
      )
    }
    # The exploratory factor analysis behind the loadings, in a background
    # R process when it alone is estimated above two seconds
    # (sieve_diagnostics_plan()'s `efa_background`; DMAR::fa_exploratory()'s
    # point estimates alone take about 1.8 to 3.7 seconds for 68 items at six
    # factors): the status line shows the elapsed time
    # against the estimate with Cancel, a changed pool stops it, and its
    # record is read for the request it was started for, in place of
    # fitting in the session (sieve_item_diagnostics(efa_record = )). The
    # reliability blocks follow in the session.
    efa_bg <- sieve_efa_dmar_job()
    efa_bg_tick <- reactiveVal(0L)
    efa_bg_error <- reactiveVal(NULL)
    kept$efa_bg <- NULL
    start_efa_bg <- function(key, b, rng, plan) {
      if (sieve_efa_dmar_active(efa_bg, key)) return(TRUE)
      setup <- sieve_item_efa_setup(state$items, state$meta, b)
      if (is.null(setup) || !is.null(setup$problem)) return(FALSE)
      call <- sieve_efa_point_call(setup$k, R = setup$R, n = setup$n_fit, seed = rng$seed)
      efa_bg$meta <- list(p = length(setup$items), k = setup$k)
      efa_bg_error(NULL)
      if (!isTRUE(sieve_efa_dmar_submit(efa_bg, key, call, seconds = plan$efa_seconds, rng_kind = rng$kind))) {
        efa_bg$record <- NULL
        efa_bg$record_key <- NULL
        efa_bg_error(list(key = key, message = paste(
          "The exploratory factor analysis could not start in a background R process",
          sprintf("(%s), so it was fit in this session.", sub("^Background execution unavailable: ", "", efa_bg$error))
        )))
        return(FALSE)
      }
      efa_bg_tick(isolate(efa_bg_tick()) + 1L)
      TRUE
    }
    compute_diag <- function(key, plan, progress = TRUE) {
      held <- kept$diag
      if (!is.null(held) && identical(held$key, key)) return(held$value)
      # Asked for: the diagnostics' own gate (whose estimate includes the
      # matrix) or Update has decided (sieve_basis_demand()).
      b <- sieve_basis_demand(state, asked = TRUE)
      # The EFA tab's kept solution is read instead of refitting when it
      # is the same model (same items, cases, correlations, k, estimates
      # and rotation), with its seed: both tabs fit DMAR::fa_exploratory().
      # Otherwise the oblique rotation starts from random positions: one
      # seed per identity of the item pool (sieve_keep_each()), so the
      # same pool gives the same loadings, also after respondents are
      # restored, and the session's own random stream is left as it was.
      reuse <- sieve_efa_store_match(sieve_efa_store(state, create = FALSE), diag_want(key, b))
      rng <- if (isTRUE(reuse$usable)) {
        reuse$solution$rng
      } else {
        sieve_keep_each(kept, "diag_rng", key, function() sieve_draw_seed())
      }
      background <- if (!isTRUE(reuse$usable) && identical(kept$efa_bg$key, key)) kept$efa_bg$record
      if (!isTRUE(reuse$usable) && is.null(background) && isTRUE(plan$efa_background) &&
        !identical(isolate(efa_bg_error())$key, key) && isTRUE(start_efa_bg(key, b, rng, plan))) {
        return(list(
          value = NULL, error = NULL, warnings = character(0), status = "running", plan = plan, key = key,
          stopped = identical(efa_bg$stopped, key)
        ))
      }
      sieve_keep(kept, "diag", key, function() {
        on.exit(isolate(diag_done(diag_done() + 1L)), add = TRUE)
        kept$diag_asked <- unique(c(kept$diag_asked, key$dataset %||% "(unnamed data)"))
        p <- length(key$items)
        started <- proc.time()[["elapsed"]]
        rel_in <- sieve_items_reliability_inputs(state)
        compute <- function() {
          sieve_capture(sieve_item_diagnostics(
            rel_in$data, state$meta,
            basis = b, keep = rel_kept,
            # Reliability reads the items as the study's estimator treats
            # them: ordered under WLSMV (categorical robust omega).
            ordered = key$ordered,
            # Each construct's own rows, under the Reliability tab's
            # missing data treatment and auxiliaries.
            missing = rel_in$missing, aux = rel_in$aux,
            # Socket workers share the per item fits when they take two
            # seconds or more on one (sieve_diagnostics_plan()).
            cores = plan$workers %||% 1L,
            rng = rng,
            solution = if (isTRUE(reuse$usable)) reuse$solution,
            efa_record = background
          ))
        }
        run <- if (isTRUE(progress)) {
          withProgress(
            message = sprintf("Items: diagnostics of %d included items (%s)", p, plan$label %||% "an unknown time"),
            detail = paste(
              "An exploratory factor analysis (DMAR::fa_exploratory) on the shared correlation matrix, then robust",
              "omega of each construct and without each item in turn (the coefficient if the item were dropped)."
            ),
            value = 1,
            compute()
          )
        } else {
          compute()
        }
        seconds <- proc.time()[["elapsed"]] - started
        if (run$ok && isTRUE(run$value$solution_reused)) run$value$efa_when <- reuse$when
        if (run$ok && !is.null(run$value)) {
          # The measured times against their estimates, so later estimates
          # of this session follow this machine: the factor analysis when it
          # was fit here (sieve_efa_point_wait()), and the reliability part
          # when a construct's block was computed (sieve_diagnostics_wait()).
          efa_seconds <- run$value$efa_seconds %||% NA_real_
          if (is.finite(efa_seconds) && is.null(background)) {
            sieve_efa_point_time_note(p, plan$k %||% run$value$k, "matrix", efa_seconds,
              refused = !identical(run$value$status$code, "ok"))
          }
          if (length(run$value$recomputed) && is.finite(plan$seconds)) {
            rest <- plan$seconds - (plan$efa_seconds %||% 0)
            spent <- seconds - (if (is.finite(efa_seconds) && is.null(background)) efa_seconds else 0)
            if (rest > 0) sieve_rate_note("diagnostics_ratio", max(0, spent), rest)
          }
        }
        if (run$ok && !is.null(run$value) && !is.null(rel_in$note)) run$value$notes <- c(run$value$notes, rel_in$note)
        if (run$ok && !is.null(run$value$items)) {
          sieve_log(state,
            sprintf(
              "Items: exploratory diagnostics of %d included items (DMAR::fa_exploratory, k = %d, %s correlations, n = %d) and robust omega by construct.",
              p, run$value$k %||% length(unique(key$constructs)), sieve_retention_basis_label(run$value$basis_used), run$value$n_obs
            ),
            sieve_item_diagnostics_code(run$value, b, key$constructs, key$items,
              solution_call = if (isTRUE(run$value$solution_reused)) reuse$solution$call)
          )
        }
        list(value = run$value, error = run$error, warnings = run$warnings, seconds = seconds, key = key,
          background = !is.null(background))
      })
    }
    diag_run <- reactive({
      key <- diag_key()
      efa_bg_tick()
      held <- kept$diag
      if (!is.null(held) && identical(held$key, key)) return(held$value)
      plan <- diag_plan()
      if (is.null(plan)) {
        return(list(value = NULL, error = NULL, warnings = character(0), seconds = 0))
      }
      # A reopened project waits for Update once (sieve_restored_project());
      # after that first request the diagnostics
      # follow edits by themselves again while quick.
      restored <- sieve_restored_project(state) && !(key$dataset %||% "(unnamed data)") %in% kept$diag_asked
      stopped <- identical(efa_bg$stopped, key)
      if ((identical(diag_request(), key) && !stopped) ||
        (!restored && sieve_runs_by_itself(plan$seconds, live = TRUE, standard = plan$standard_seconds))) {
        return(compute_diag(key, plan))
      }
      list(
        value = NULL, error = NULL, warnings = character(0), status = "not run", plan = plan,
        stale = !is.null(held), key = key, restored = restored, stopped = stopped
      )
    })
    # The background factor analysis, polled while it runs. A changed pool
    # stops it; a finished one is kept for its request, and the diagnostics
    # then follow (diag_run() reads it).
    observe({
      efa_bg_tick()
      if (!sieve_efa_dmar_active(efa_bg)) return(invisible(NULL))
      key <- tryCatch(diag_key(), error = function(e) NULL)
      if (!identical(key, efa_bg$key)) {
        sieve_efa_dmar_cancel(efa_bg, remember = FALSE)
        efa_bg_tick(isolate(efa_bg_tick()) + 1L)
      } else if (sieve_efa_dmar_poll(efa_bg)) {
        record <- efa_bg$record
        if (is.finite(record$seconds %||% NA_real_) && !is.null(efa_bg$meta)) {
          sieve_efa_point_time_note(efa_bg$meta$p, efa_bg$meta$k, "matrix", record$seconds,
            refused = !isTRUE(record$ok))
        }
        kept$efa_bg <- list(key = efa_bg$record_key, record = record)
        efa_bg$record <- NULL
        efa_bg$record_key <- NULL
        efa_bg_tick(isolate(efa_bg_tick()) + 1L)
      } else {
        shiny::invalidateLater(250, session)
      }
    })
    cancel_efa_bg <- function() {
      if (sieve_efa_dmar_active(efa_bg)) {
        sieve_efa_dmar_cancel(efa_bg)
        efa_bg_tick(isolate(efa_bg_tick()) + 1L)
      }
      invisible(NULL)
    }
    observeEvent(input$efa_cancel, cancel_efa_bg())
    session$onSessionEnded(function() sieve_efa_dmar_cancel(efa_bg, remember = FALSE))
    # The elapsed time in the status line's running line, redrawn each
    # second while the factor analysis runs.
    sieve_progress_outputs(output, "efa_progress", function() {
      efa_bg_tick()
      if (sieve_efa_dmar_active(efa_bg)) sieve_efa_dmar_progress(efa_bg)$sentence
    }, session)
    diag <- reactive(diag_run()$value)

    # Facet membership affects only these inexpensive observed-score correlations.
    # It is intentionally absent from the EFA/construct-reliability cache key.
    facet_kept <- new.env(parent = emptyenv())
    facet_diag <- reactive({
      req(state$meta, state$items)
      sieve_facet_item_rest(state$items, state$meta, keep = facet_kept)
    })
    selected_ids <- reactiveVal(character(0))
    observeEvent(input$pool_item_ids,
      {
        selected_ids(intersect(as.character(input$pool_item_ids), state$meta$item))
      },
      ignoreNULL = FALSE
    )
    observeEvent(input$information_view, {
      view <- input$information_view
      if (length(view) && view %in% c("basic", "advanced")) {
        presentation <- isolate(state$presentation) %||% list()
        if (!identical(presentation$items_view, view)) {
          presentation$items_view <- view
          state$presentation <- presentation
        }
      }
    })
    observe({
      view <- state$presentation$items_view %||% "basic"
      shiny::updateRadioButtons(session, "information_view", selected = view)
    })

    coef <- reactive({
      if (identical(input$coef, "alpha")) "alpha" else "omega"
    })

    # The overall measurement model (each construct a factor) for headline
    # fit: correlated factors by default, optionally orthogonal. Under the run
    # policy (sieve_runs_by_itself()) a first fit for the dataset runs by
    # itself when the page is shown and its estimate is two seconds or less;
    # anything longer, or a model made stale by an edit, waits for the page's
    # Update button, which states the estimate. A fit stays while the pool it
    # was fit on is the pool on screen; after the pool changes the panel says
    # so.
    fit_kept <- new.env(parent = emptyenv())
    fit_kept$datasets <- character(0)
    fit_store <- reactiveVal(NULL)
    fit_completion <- reactiveVal(NULL)
    fit_key <- reactive({
      key <- diag_key()
      # The declared sampling design is part of the model's identity: the
      # model is fit under it as the CFA tab fits it.
      state$sampling_design
      design <- sieve_sampling_spec(state)
      if (!identical(design$unit, "independent") && length(design$cluster) == 1L &&
        design$cluster %in% names(state$raw)) {
        design$values_key <- sieve_hash(state$raw[[design$cluster]])
      }
      list(
        data = key$data, items = key$items, types = key$types, constructs = key$constructs,
        dataset = key$dataset, rows = key$rows,
        orthogonal = identical(input$factor_cov, "orthogonal"),
        estimator = estimator_choice(), sampling = design
      )
    })
    fit_plan <- reactive({
      key <- fit_key()
      spec <- sieve_lavaan_spec(state$meta, estimator = key$estimator)
      # The rate class: ML with FIML is costed as MLR (sieve_demand_class()).
      rate_class <- sieve_demand_class(spec$estimator, spec$missing)
      seconds <- sieve_fit_wait(length(key$items), rate_class, "cfa")
      standard_seconds <- sieve_at_default_rates(sieve_fit_wait(length(key$items), rate_class, "cfa"))
      k <- length(unique(key$constructs))
      list(
        estimator = spec$estimator, rate_class = rate_class, p = length(key$items), seconds = seconds,
        # The estimator in words, with its treatment of incomplete cases
        # ("ML with FIML", "MLR with FIML"), for the lines that state it.
        est_label = spec$label,
        label = sieve_duration_label(seconds),
        # Quick enough to run by itself as a first result (whether it is a
        # first result is decided in fit_run()).
        automatic = sieve_runs_by_itself(seconds, standard = standard_seconds),
        standard_seconds = standard_seconds,
        # Refused before anything starts: a categorical model whose weight
        # matrix, or an MLR model whose matrix of the moments, would not fit
        # in memory, or MLR with as many free parameters as respondents.
        refused = sieve_wlsmv_guard(key$items, state$items,
          ordered = spec$ordered %||% character(0), estimator = spec$estimator
        ) %||% sieve_mlr_guard(length(key$items), spec$estimator, n = nrow(state$items),
          npar = sieve_free_parameters(length(key$items), k, means = identical(spec$missing, "fiml")),
          missing = spec$missing
        )
      )
    })
    run_fit <- function(key, plan, progress = TRUE) {
      started <- proc.time()[["elapsed"]]
      # Whether the fit came from the session's store (fct_fit_store.R):
      # a reused fit enters no measured rate and the panel names its page.
      reuse_seen <- new.env(parent = emptyenv())
      fit_it <- function() {
        sieve_capture({
          spec <- sieve_cfa_spec(state$meta,
            orthogonal = identical(input$factor_cov, "orthogonal"),
            reserved = union(names(state$items), names(state$raw)),
            estimator = key$estimator %||% "auto"
          )
          if (!nzchar(spec$model)) {
            NULL
          } else {
            # The declared sampling design, applied as on the CFA tab: a
            # clustered design gives cluster-robust MLR, a design this
            # model cannot carry is refused with its reason.
            prepared <- sieve_sampling_apply(spec, state, state$items)
            if (!is.null(prepared$reason)) stop(prepared$reason, call. = FALSE)
            for (w in prepared$warnings) warning(w, call. = FALSE)
            run <- sieve_run_cfa(prepared$spec, prepared$data,
              store = shiny::isolate(state$fit_store), page = "Items",
              rows = shiny::isolate(sieve_case_index(state))[prepared$row_index]
            )
            # A model the design leaves without inference (too few
            # clusters for the free parameters) is said, not left blank.
            if (is.null(run$fit) && length(run$status$message)) stop(run$status$message, call. = FALSE)
            reuse_seen$run <- run[intersect(c("reused", "reused_from", "reused_when", "seconds"), names(run))]
            run$fit
          }
        })
      }
      run <- if (isTRUE(progress)) {
        withProgress(
          message = sprintf(
            "Items: overall measurement model (%s, %d items, %s)",
            plan$est_label %||% plan$estimator, plan$p, plan$label
          ),
          detail = sprintf(
            "The %s-factor model implied by the construct assignments, fit with lavaan.",
            if (identical(input$factor_cov, "orthogonal")) "orthogonal" else "correlated"
          ),
          value = 1,
          fit_it()
        )
      } else {
        fit_it()
      }
      seconds <- proc.time()[["elapsed"]] - started
      reused <- isTRUE(reuse_seen$run$reused)
      if (!reused) {
        rate_class <- plan$rate_class %||% sieve_demand_class(plan$estimator)
        sieve_rate_note(paste0("cfa_", rate_class), seconds, sieve_fit_units(plan$p, rate_class))
      }
      fit_kept$datasets <- unique(c(fit_kept$datasets, key$dataset %||% "(unnamed data)"))
      result <- list(
        status = "ran", fit = run$value, error = run$error, warnings = run$warnings,
        key = key, seconds = seconds, estimator = plan$estimator, est_label = plan$est_label, p = plan$p,
        revision = sieve_revision(state),
        reused = reused, reused_from = reuse_seen$run$reused_from,
        reused_when = reuse_seen$run$reused_when, fit_seconds = reuse_seen$run$seconds
      )
      fit_completion(list(key = key, revision = result$revision, error = result$error,
        when = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")))
      result
    }
    fit_run <- reactive({
      key <- fit_key()
      plan <- fit_plan()
      held <- fit_store()
      if (!is.null(held) && identical(held$key, key)) return(held$value)
      cached <- fit_kept$fit
      if (!is.null(cached) && identical(cached$key, key)) return(cached$value)
      previous <- if (!is.null(held)) held$key else if (!is.null(cached)) cached$key
      first <- !(key$dataset %||% "(unnamed data)") %in% fit_kept$datasets
      # Not on a reopened project (sieve_restored_project()): its recorded
      # results are in the registry.
      restored <- first && sieve_restored_project(state)
      if (is.null(plan$refused) && !restored &&
        sieve_runs_by_itself(plan$seconds, first = first, standard = plan$standard_seconds)) {
        return(sieve_keep(fit_kept, "fit", key, function() run_fit(key, plan)))
      }
      # Stale means the item pool changed under a fit; another model choice
      # (orthogonal factors) is a model not fit yet, not a stale one.
      pool <- c("data", "items", "types", "constructs", "dataset", "rows")
      list(
        status = "not run", plan = plan, key = key,
        stale = !is.null(previous) && !identical(previous[pool], key[pool]),
        restored = restored
      )
    })
    # The page's one action: Update computes what
    # waits on this page, in turn, under one progress message, and says
    # when it is done: the diagnostics (when they wait: estimated above two
    # seconds, or from before an edit) and the overall model (not fit yet
    # or from before). Fit the overall model (fit_go, no button of its own)
    # is the same action for the model alone.
    run_model_now <- function(progress = TRUE) {
      key <- fit_key()
      plan <- fit_plan()
      if (!is.null(plan$refused)) {
        sieve_notify(plan$refused, type = "error", duration = NULL)
        return(invisible(NULL))
      }
      held <- fit_store()
      if (!is.null(held) && identical(held$key, key)) return(invisible(NULL))
      if (!is.null(fit_kept$fit) && identical(fit_kept$fit$key, key)) return(invisible(NULL))
      result <- run_fit(key, plan, progress = progress)
      fit_store(list(key = key, value = result))
      invisible(result)
    }
    observeEvent(input$fit_go, run_model_now())
    update_page <- function() {
      up <- update_plan()
      if (is.null(up) || (!isTRUE(up$diag) && !isTRUE(up$model))) {
        sieve_notify("Nothing on this page waits for Update, so nothing was recomputed.")
        return(invisible(NULL))
      }
      steps <- c(if (isTRUE(up$diag)) "the item diagnostics", if (isTRUE(up$model)) "the overall model")
      done <- character(0)
      withProgress(
        message = sprintf("Updating the Items page (%s)", sieve_duration_label(up$seconds)),
        detail = sprintf("%s.", paste(steps, collapse = ", then ")),
        value = 0,
        {
          if (isTRUE(up$diag)) {
            sieve_progress_set(0, sprintf("1 of %d: the item diagnostics", length(steps)))
            key <- diag_key()
            # A press asks again: a canceled background run is started afresh.
            efa_bg$stopped <- NULL
            diag_request(key)
            ran <- compute_diag(key, diag_plan(), progress = FALSE)
            done <- c(done, if (identical(ran$status, "running")) {
              paste(
                "the item diagnostics (their exploratory factor analysis runs in a background R process,",
                "and they follow when it finishes; see the status line)"
              )
            } else if (is.null(ran$error)) {
              sprintf("the item diagnostics (%s)", sieve_seconds_text(ran$seconds))
            } else {
              sprintf("the item diagnostics (stopped with an error: %s)", ran$error)
            })
          }
          if (isTRUE(up$model)) {
            sieve_progress_set((length(steps) - 1) / length(steps),
              sprintf("%d of %d: the overall model", length(steps), length(steps)))
            fitted <- run_model_now(progress = FALSE)
            done <- c(done, if (is.null(fitted)) {
              "the overall model (not fit; see the status line)"
            } else if (!is.null(fitted$error)) {
              sprintf("the overall model (stopped with an error: %s)", fitted$error)
            } else {
              sprintf("the overall model (%s)", sieve_seconds_text(fitted$seconds))
            })
          }
          sieve_progress_set(1)
        }
      )
      sieve_notify(sprintf("Update finished: %s.", paste(done, collapse = "; ")))
      invisible(done)
    }
    observeEvent(input$update_go, update_page())
    overall_fit <- reactive(fit_run()$fit)
    observe({
      key <- fit_key()
      plan <- fit_plan()
      latest <- fit_completion()
      manual <- fit_store()
      automatic <- fit_kept$fit
      record <- if (!is.null(manual) && identical(manual$key, key)) manual$value else if (
        !is.null(automatic) && identical(automatic$key, key)) automatic$value else latest
      phase <- if (is.null(record)) "not_run" else if (!identical(record$key, key)) "stale" else if (!is.null(record$error)) "failed" else "current"
      sieve_page_publish(state, "winnow", session$ns("overall_fit"), "Items measurement model",
        list(state = phase, sentence = if (phase == "stale") "The item pool or model settings changed." else if (!is.null(plan$refused)) plan$refused else record$error,
          key = record$key, when = record$when), "Update", own = TRUE)
    })
    # The diagnostics' own status for the page guide, read from what is
    # kept (it never evaluates the diagnostics, so a hidden page computes
    # nothing).
    observe({
      key <- tryCatch(diag_key(), error = function(e) NULL)
      diag_done()
      if (is.null(key)) return()
      held <- kept$diag
      phase <- if (is.null(held)) {
        "not_run"
      } else if (!identical(held$key, key)) {
        "stale"
      } else if (!is.null(held$value$error)) {
        "failed"
      } else {
        "current"
      }
      sieve_page_publish(state, "winnow", session$ns("diagnostics"), "Item diagnostics",
        list(state = phase, key = key,
          sentence = if (identical(phase, "stale")) "The item pool changed since the diagnostics were computed." else held$value$error),
        "Update", own = TRUE)
    })

    # What went wrong in an edit, if anything: shown in the status line.
    edit_problem <- reactiveVal(NULL)

    # The display table. Flags are computed here from the stats and the live
    # thresholds (cheap), so threshold changes never refit.
    display <- reactive({
      req(state$meta)
      m <- state$meta
      m <- m[order(match(m$construct, unique(m$construct)), seq_len(nrow(m))), , drop = FALSE]
      d <- tryCatch(diag(), error = function(e) NULL)
      di <- if (!is.null(d)) d$items else NULL
      th <- state$thresholds %||% sieve_default_thresholds()
      use_alpha <- identical(coef(), "alpha")
      num <- function(col) {
        if (is.null(di)) rep(NA_real_, nrow(m)) else di[[col]][match(m$item, di$item)]
      }
      chr <- function(col) {
        if (is.null(di)) rep(NA_character_, nrow(m)) else di[[col]][match(m$item, di$item)]
      }
      loading <- num("loading_target")
      h2 <- num("communality")
      itc <- num("item_total")
      itc_n <- num("item_total_n")
      itc_reason <- chr("item_total_reason")
      fd <- facet_diag()
      fi <- match(m$item, fd$item)
      facet_itc <- fd$facet_item_total[fi]
      facet_n <- fd$facet_item_total_n[fi]
      facet_reason <- fd$facet_item_total_reason[fi]
      reldrop <- if (use_alpha) num("alpha_drop_delta") else num("omega_drop_delta")
      redun <- num("redundancy_max")
      offmax <- num("loading_off_max")
      offcon <- chr("off_construct")

      # The flags at the live thresholds (sieve_item_live_flags(), which the
      # expert review brief reads too): salience by the size of the loading,
      # so a strongly reverse keyed item is not low loading; a salient target
      # loading opposite in sign to the other items of its factor is the
      # keying flag; and in a construct holding such an item the item-rest
      # and "rel up" signals of every item are withheld, since each reads
      # a total in which that item subtracts.
      lf <- if (is.null(di)) NULL else sieve_item_live_flags(di, th, meta = m, use_alpha = use_alpha)[match(m$item, di$item), , drop = FALSE]
      cross <- if (is.null(lf)) rep(FALSE, nrow(m)) else lf$cross %in% TRUE

      flags <- vapply(seq_len(nrow(m)), function(i) {
        if (is.null(lf)) return("")
        paste(c(
          if (isTRUE(lf$keying[i])) "keying",
          if (isTRUE(lf$load[i])) "load",
          if (isTRUE(lf$comm[i])) "comm",
          if (isTRUE(lf$itc[i])) "itc",
          if (isTRUE(lf$relup[i])) "rel up",
          if (isTRUE(lf$dup[i])) "dup"
        ), collapse = ", ")
      }, character(1))
      reassign <- vapply(seq_len(nrow(m)), function(i) {
        if (!isTRUE(cross[i])) return("")
        if (is.na(offcon[i])) "complex" else paste0("-> ", offcon[i])
      }, character(1))

      eid <- ns("cell_edit")
      cons_opts <- unique(c(m$construct, "Unassigned"))
      facet_opts <- sort(unique(m$facet[!is.na(m$facet) & nzchar(m$facet)]))
      construct_cell <- vapply(seq_len(nrow(m)), function(i) {
        sieve_cell_combo(eid, m$item[i], "construct", m$construct[i], cons_opts)
      }, character(1))
      facet_cell <- vapply(seq_len(nrow(m)), function(i) {
        fv <- if (is.na(m$facet[i])) "" else m$facet[i]
        sieve_cell_combo(eid, m$item[i], "facet", fv, facet_opts)
      }, character(1))
      status_cell <- vapply(seq_len(nrow(m)), function(i) {
        cur <- if (m$status[i] == "active") "Included" else "Excluded"
        sieve_cell_select(eid, m$item[i], "status", cur, c("Included", "Excluded"))
      }, character(1))
      wording_vec <- if ("wording" %in% names(m)) m$wording else rep(NA_character_, nrow(m))
      wording_cell <- vapply(seq_len(nrow(m)), function(i) sieve_wording_cell(wording_vec[i]), character(1))
      data.frame(
        Item = m$item,
        Label = m$label,
        Wording = wording_cell,
        Construct = construct_cell,
        Facet = facet_cell,
        Status = status_cell,
        loading = loading,
        communality = h2,
        item_total = vapply(seq_len(nrow(m)), function(i) {
          sieve_item_rest_cell(itc[i], itc_n[i], if (m$status[i] != "active") "Item is excluded." else itc_reason[i])
        }, character(1)),
        facet_item_total = vapply(seq_len(nrow(m)), function(i) {
          sieve_item_rest_cell(facet_itc[i], facet_n[i], facet_reason[i])
        }, character(1)),
        rel_drop = reldrop,
        redundancy = redun,
        Flags = flags,
        Reassign = reassign,
        stringsAsFactors = FALSE
      )
    })

    output$pool <- renderDT(
      {
        df <- display()
        # Columns holding HTML (in-cell widgets or the read-only wording span): not
        # escaped, and not sortable (no misleading sort arrow). Targets are derived
        # by name so the layout survives column insertions.
        html_names <- c("Wording", "Construct", "Facet", "Status", "item_total", "facet_item_total")
        html_cols <- which(names(df) %in% html_names)
        no_sort <- which(names(df) %in% html_names) - 1L
        rel_label <- if (identical(coef(), "alpha")) "\u03b1 if dropped" else "Robust \u03c9 if dropped"
        # The basis of the largest correlation, named.
        d_now <- tryCatch(diag(), error = function(e) NULL)
        max_r_label <- sprintf(
          "max |r| (%s)",
          if (is.null(d_now$basis_used) || is.na(d_now$basis_used)) "shared matrix" else sieve_retention_basis_label(d_now$basis_used)
        )
        sieve_dt(
          df,
          rownames = FALSE,
          selection = list(mode = "multiple", selected = match(intersect(isolate(selected_ids()), df$Item), df$Item)),
          escape = setdiff(seq_along(df), html_cols),
          options = list(
            pageLength = 30, dom = "ftp", order = list(), scrollX = TRUE, autoWidth = FALSE, stateSave = TRUE,
            stateSaveCallback = sieve_dt_state_save(), stateLoadCallback = sieve_dt_state_load(),
            sieve = list(
              id = "items-pool", keys = names(df), rowKey = "Item",
              widths = list(
                Item = 125, Label = 170, Wording = 250, Construct = 145, Facet = 140, Status = 105,
                loading = 90, communality = 90, item_total = 145, facet_item_total = 145, rel_drop = 115,
                redundancy = 90, Flags = 130, Reassign = 130
              ),
              selectionInput = ns("pool_item_ids"), definitions = sieve_item_column_definitions(),
              advanced = identical(input$information_view %||% "basic", "advanced")
            ),
            columnDefs = list(
              list(width = "125px", targets = which(names(df) == "Item") - 1L),
              list(width = "105px", targets = which(names(df) == "Status") - 1L),
              list(
                visible = identical(input$information_view %||% "basic", "advanced"),
                targets = which(names(df) %in% c("redundancy")) - 1L
              ),
              list(width = "140px", targets = which(names(df) == "Label") - 1L), # Item Name
              list(width = "230px", targets = which(names(df) == "Wording") - 1L), # Wording
              list(width = "140px", targets = which(names(df) %in% c("Construct", "Facet")) - 1L),
              # The numeric diagnostics need room for three decimals; without an
              # explicit width the fixed layout starves them into ellipses and
              # the workspace reads as broken. scrollX carries the overflow.
              list(
                width = "76px",
                targets = which(names(df) %in%
                  c(
                    "loading", "communality", "item_total", "facet_item_total",
                    "rel_drop", "redundancy"
                  )) - 1L
              ),
              list(width = "110px", targets = which(names(df) %in% c("Flags", "Reassign")) - 1L),
              list(orderable = FALSE, targets = no_sort)
            )
          ),
          colnames = c(
            "Variable Name", "Item Name", "Wording", "Construct", "Facet", "Included?",
            "loading", "h2", "Construct item-rest r", "Facet item-rest r", rel_label,
            max_r_label, "Diagnostic flags", "Cross loading"
          )
        ) |>
          formatRound(c("loading", "communality", "rel_drop", "redundancy"), 3) |>
          formatStyle("Flags", color = "#b00", fontWeight = "bold") |>
          formatStyle("Reassign", color = "#1c4f8f", fontStyle = "italic")
      },
      server = FALSE
    )

    # Map selected display rows back to item names.
    sel_items <- reactive({
      req(state$meta)
      sieve_selected_items(input$pool_item_ids, input$pool_rows_selected, display()$Item)
    })
    sieve_metadata_controls_server(input, output, session, state, function() isolate(sel_items()))
    # The content coverage advisory of the last inclusion decision made on
    # this page: shown above the grid, beside the Exclude and Include buttons,
    # when an exclusion leaves a construct or a facet with no included item;
    # an inclusion that fills it clears it.
    coverage_note <- reactiveVal(NULL)
    sieve_coverage_navigation(input, session)
    note_coverage <- function(out) {
      advisory <- sieve_coverage_advisory(out, ns = ns)
      if (!is.null(advisory) || length(out$changed)) coverage_note(advisory)
      invisible(out)
    }
    change_selection_status <- function(status) {
      picked <- isolate(sel_items())
      if (!length(picked)) {
        edit_problem("Select one or more item rows first. Nothing was changed.")
        return(invisible(NULL))
      }
      selected_ids(picked)
      out <- note_coverage(sieve_set_item_status(state, picked, status))
      edit_problem(NULL)
      invisible(out)
    }
    observeEvent(input$exclude_selected, change_selection_status("parked"))
    observeEvent(input$include_selected, change_selection_status("active"))

    output$detail <- renderUI({
      it <- sel_items()
      if (!length(it)) {
        return(em("No item selected. Click a row to read its full wording."))
      }
      m <- state$meta[match(it, state$meta$item), ]
      d <- tryCatch(diag(), error = function(e) NULL)
      fd <- facet_diag()
      tagList(lapply(seq_len(nrow(m)), function(i) {
        dr_idx <- if (!is.null(d) && !is.null(d$items)) match(m$item[i], d$items$item) else NA_integer_
        dr <- if (m$status[i] == "active" && !is.na(dr_idx)) d$items[dr_idx, , drop = FALSE] else NULL
        diag_line <- if (!is.null(dr) && nrow(dr)) {
          sprintf(
            "loading %.2f, h2 %.2f, r(item-total) %.2f",
            dr$loading_target, dr$communality, dr$item_total
          )
        } else {
          NULL
        }
        fr <- fd[match(m$item[i], fd$item), , drop = FALSE]
        rest_line <- function(value, n, reason, scope) {
          sprintf(
            "%s item-rest r = %s; analytic n = %s%s", scope,
            if (length(value) && is.finite(value)) sprintf("%.3f", value) else "undefined",
            if (length(n) && !is.na(n)) n else "unavailable",
            if (length(reason) && !is.na(reason) && nzchar(reason)) paste0(". ", reason) else ""
          )
        }
        status_lbl <- if (m$status[i] == "active") "included" else "excluded"
        wording <- if ("wording" %in% names(m) && !is.na(m$wording[i]) && nzchar(m$wording[i])) m$wording[i] else NULL
        div(
          class = "border rounded p-2 mb-2",
          strong(m$label[i]),
          tags$span(class = "badge bg-primary ms-1", m$construct[i]),
          p(class = "small text-muted mt-1 mb-0", m$item[i]),
          if (!is.null(wording)) p(class = "small fst-italic mt-1 mb-0", paste0("\u201c", wording, "\u201d")),
          tags$small(
            class = "text-muted",
            sprintf("%s, %d levels, %d missing, %s", m$type[i], m$n_levels[i], m$n_missing[i], status_lbl)
          ),
          if (!is.null(diag_line)) tags$small(class = "d-block text-secondary", diag_line),
          tags$small(class = "d-block", rest_line(
            if (!is.null(dr)) dr$item_total else NA_real_,
            if (!is.null(dr)) dr$item_total_n else NA_integer_,
            if (!is.null(dr)) dr$item_total_reason else "Item is excluded or diagnostics are unavailable.",
            "Construct"
          )),
          tags$small(class = "d-block", rest_line(
            fr$facet_item_total, fr$facet_item_total_n,
            fr$facet_item_total_reason, "Facet"
          ))
        )
      }))
    })

    # Keep the "review a construct" selector in sync with the active constructs.
    observe({
      req(state$meta)
      cons <- unique(state$meta$construct[state$meta$status == "active"])
      updateSelectInput(session, "review_construct",
        choices = cons,
        selected = if (length(cons)) cons[1] else NULL
      )
    })
    # Facet choices follow the chosen construct (only its facets).
    observe({
      req(state$meta, input$review_construct)
      m <- state$meta[state$meta$status == "active" & state$meta$construct == input$review_construct, ]
      facs <- sort(unique(m$facet[!is.na(m$facet) & nzchar(m$facet)]))
      updateSelectInput(session, "review_facet",
        choices = c("All facets" = "", stats::setNames(facs, facs))
      )
    })

    # Build the brief from the current options (construct or whole instrument,
    # facet, prompt vs description, with or without diagnostics).
    review_text <- reactive({
      req(state$meta)
      d <- diag()
      di <- if (!is.null(d)) d$items else NULL
      mode <- input$review_mode %||% "prompt"
      if (isTRUE(input$review_all)) {
        sieve_instrument_brief(state$meta,
          mode = mode,
          diag = if (isTRUE(input$review_diag)) di else NULL,
          thresholds = state$thresholds %||% sieve_default_thresholds()
        )
      } else {
        req(input$review_construct)
        facet <- if (!is.null(input$review_facet) && nzchar(input$review_facet)) input$review_facet else NULL
        sieve_construct_brief(state$meta, input$review_construct,
          facet = facet,
          mode = mode,
          diag = if (isTRUE(input$review_diag)) di else NULL,
          thresholds = state$thresholds %||% sieve_default_thresholds()
        )
      }
    })

    # Show the brief in a modal with copy-to-clipboard and download. The text
    # never leaves the machine through the app; the user copies/saves it and may
    # paste it elsewhere (e.g. into an LLM) at their discretion.
    observeEvent(input$review_copy, {
      brief <- review_text()
      ta_id <- ns("brief_text")
      scope <- if (isTRUE(input$review_all)) "the whole instrument" else sprintf(
        "\u201c%s\u201d",
        input$review_construct
      )
      showModal(modalDialog(
        title = sprintf("Items for review: %s", scope),
        tags$p(
          class = "text-muted small",
          "Read this as a written description of the items currently selected, ",
          "or copy/download it and paste it into a large language model for a ",
          "second opinion. Nothing is sent anywhere by the app."
        ),
        tags$textarea(
          id = ta_id, class = "form-control",
          rows = 16, style = "font-family:monospace;font-size:12px;", brief
        ),
        footer = tagList(
          tags$button(
            type = "button", class = "btn btn-primary",
            onclick = sprintf(
              "var t=document.getElementById('%s'); t.select(); if(navigator.clipboard){navigator.clipboard.writeText(t.value);} else {document.execCommand('copy');}",
              ta_id
            ),
            tagList(shiny::icon("clipboard"), "Copy to clipboard")
          ),
          downloadButton(ns("dl_brief"), "Download (.txt)", class = "btn-outline-secondary"),
          modalButton("Close")
        ),
        size = "l", easyClose = TRUE
      ))
      sieve_log(
        state,
        sprintf(
          "Assemble items + wordings (%s, %s) for external review",
          if (isTRUE(input$review_all)) "all constructs" else input$review_construct,
          input$review_mode %||% "prompt"
        ),
        if (isTRUE(input$review_all)) "# cat(sieve_instrument_brief(meta))"
        else sprintf('# cat(sieve_construct_brief(meta, "%s"))', input$review_construct)
      )
    })

    output$dl_brief <- downloadHandler(
      filename = function() {
        base <- if (isTRUE(input$review_all)) "instrument" else gsub(
          "[^A-Za-z0-9]+",
          "_",
          input$review_construct %||% "construct"
        )
        sprintf("siever_review_%s.txt", base)
      },
      content = function(file) writeLines(review_text(), file)
    )

    # In-cell edits: reassign the construct/facet or include/exclude an item.
    # An error here would otherwise vanish inside the observer; it is
    # captured and shown in the status line.
    observeEvent(input$cell_edit, {
      info <- input$cell_edit
      req(state$meta, info$item, info$field, !is.null(info$value))
      outcome <- sieve_capture(apply_edit(info))
      edit_problem(if (outcome$ok) NULL else sprintf(
        "The edit of %s stopped with an error (check its row in the table): %s", info$item, outcome$error
      ))
    })
    apply_edit <- function(info) {
      idx <- which(state$meta$item == info$item)
      if (!length(idx)) {
        return(invisible())
      }
      if (info$field == "construct") {
        cn <- trimws(info$value)
        sieve_apply_metadata_patch(
          state,
          sieve_metadata_patch(
            isolate(state$meta),
            info$item,
            "construct",
            cn
          ),
          "Reassign construct"
        )
        sieve_log(
          state, sprintf("Reassign %s to construct %s", info$item, state$meta$construct[idx]),
          sprintf('# %s now loads on "%s"', info$item, state$meta$construct[idx])
        )
      } else if (info$field == "facet") {
        fc <- trimws(info$value)
        sieve_apply_metadata_patch(
          state,
          sieve_metadata_patch(
            isolate(state$meta),
            info$item,
            "facet",
            fc
          ),
          "Assign facet"
        )
        lbl <- if (nzchar(fc)) sprintf('facet "%s"', fc) else "no facet"
        sieve_log(
          state, sprintf("Assign %s to %s", lbl, info$item),
          sprintf('# %s -> %s', info$item, lbl)
        )
      } else if (info$field == "status") {
        excl <- identical(info$value, "Excluded")
        note_coverage(sieve_set_item_status(state, info$item, if (excl) "parked" else "active"))
      }
      invisible()
    }

    # How the diagnostics are computed, below the grid (results first): the
    # first paragraph visible, the rest in "More on how these diagnostics are
    # computed" (model_more, fit_note, rel_note and overall_note), closed by
    # default.
    output$model_note <- renderUI({
      div(
        class = "small mt-2 mb-1",
        HTML(paste0(
          "<b>How these diagnostics are computed.</b> They come from an ",
          "<b>exploratory factor analysis</b> by DMAR::fa_exploratory(): maximum likelihood with an ",
          "oblique <b>direct oblimin (delta = 0)</b> rotation from 30 random starts on a recorded seed, ",
          "extracting as many factors as you ",
          "have constructs (for a study of one construct split into facets, as many as it has facets), fit to the inter-item correlations of the currently <b>included</b> ",
          "items (Pearson for continuous items, polychoric or tetrachoric for ordered or binary ",
          "items; the one matrix the Dimensionality and EFA tabs share). The loadings and ",
          "communalities (h2) come from that EFA, the cross-loading flag from its off-target ",
          "loadings, and each construct's reliability (robust omega, observed denominator; ",
          "categorical robust omega on the polychoric correlations for ordered items) from a ",
          "single-factor model on its own items. DMAR refuses an improper solution (a residual ",
          "variance below zero) rather than holding it at a bound, so after an exclusion that ",
          "leaves one the loading columns are empty and the status line says why. When the EFA ",
          "tab's solution is the same model (maximum likelihood on the same complete cases, the ",
          "same number of factors, direct oblimin), its loadings are read here instead of fitting again."
        ))
      )
    })
    output$model_more <- renderUI({
      p(
        class = "small mt-2",
        paste0(
          "This view is exploratory and data driven: it does not assume your construct ",
          "assignments are correct, follows each inclusion, exclusion or reassignment by itself ",
          "while its estimated time is two seconds or less (a longer one waits for Update above the ",
          "grid), and is distinct from the confirmatory model on the CFA tab. Excluding an item ",
          "refits all of this on the remaining pool."
        )
      )
    })

    # Per-construct reliability, shown above the table so the "if dropped"
    # deltas have a baseline.
    output$rel_summary <- renderUI({
      req(state$meta)
      # A construct or a facet with fewer than three included items is
      # named here, beside the reliability it affects, rather than in a
      # banner of its own above the grid (results first); one whose last
      # item was excluded stays named, with none included, as its content
      # is then unrepresented (sieve_item_coverage()).
      # The coverage advisory of the last decision made here comes first.
      thin <- sieve_coverage_thin_text(sieve_item_coverage(state$meta))
      thin_note <- if (length(thin)) {
        tags$span(class = "small text-warning ms-1", thin)
      }
      advisory <- coverage_note()
      d <- diag()
      if (is.null(d) || is.null(d$constructs)) {
        return(tagList(advisory, if (!is.null(thin_note)) div(class = "mb-1", thin_note)))
      }
      cons <- d$constructs
      use_alpha <- identical(coef(), "alpha")
      label <- if (use_alpha) "Coefficient alpha (legacy)" else "Robust omega (observed denominator)"
      vals <- if (use_alpha) cons$alpha else cons$omega
      # The rows each construct's reliability reads: its own, by FIML for
      # continuous items, complete on its items otherwise.
      rows_text <- vapply(seq_len(nrow(cons)), function(i) {
        n <- cons$n_reliability[i] %||% NA_integer_
        if (!length(n) || !is.finite(n)) return("")
        if (identical(cons$missing_treatment[i], "fiml")) {
          sprintf("; n = %d by FIML", as.integer(n))
        } else {
          sprintf("; n = %d complete on its items", as.integer(n))
        }
      }, character(1))
      chips <- lapply(seq_len(nrow(cons)), function(i) {
        v <- if (is.na(vals[i])) "n/a" else sprintf("%.2f", vals[i])
        resid <- if (!use_alpha && length(cons$residual_max) && is.finite(cons$residual_max[i])) {
          sprintf(
            "; largest residual r = %.2f (%s)",
            cons$residual_max[i], htmltools::htmlEscape(cons$residual_pair[i])
          )
        } else {
          ""
        }
        tags$span(
          class = "badge bg-light text-dark border me-1 mb-1 text-wrap text-start",
          HTML(sprintf(
            "%s &nbsp;%s = %s%s &nbsp;<span class='text-muted'>(%d items%s)</span>",
            htmltools::htmlEscape(cons$construct[i]), label, v, resid, cons$n_active[i], rows_text[i]
          ))
        )
      })
      # What the engine noted about a construct's coefficient (for ordered
      # items, the coding note), once per construct.
      said <- cons[[if (use_alpha) "alpha_note" else "omega_note"]]
      engine_notes <- if (length(said)) lapply(which(!is.na(said)), function(i) {
        div(class = "small text-muted mb-1", tags$b(paste0(cons$construct[i], ": ")), said[i])
      })
      # Keying, read live at the salience threshold (sieve_item_keying()):
      # an item loading opposite in sign to the others of its factor, what
      # it does to the values here, and the action.
      th <- state$thresholds %||% sieve_default_thresholds()
      keying <- if (!is.null(d$items)) sieve_item_keying(d$items, salient = th$salient %||% 0.30, meta = state$meta)$notes
      keying_notes <- if (length(keying)) lapply(names(keying)[!is.na(keying)], function(cn) {
        div(class = "small text-warning mb-1", tags$b(paste0(cn, " (keying): ")), keying[[cn]])
      })
      engine_notes <- c(keying_notes, engine_notes)
      tagList(
        advisory,
        div(
          class = "mb-1",
          tags$span(class = "small text-muted me-1", "Current reliability by construct:"),
          chips,
          thin_note
        ),
        engine_notes
      )
    })
    # What the reliability above the grid is, below it (in "More on how
    # these diagnostics are computed").
    output$rel_note <- renderUI({
      d <- diag()
      if (is.null(d) || is.null(d$constructs) || identical(coef(), "alpha")) {
        return(NULL)
      }
      div(
        class = "small text-muted mt-2",
        "Robust omega: the squared sum of the one-factor loadings over the variance of the sum ",
        sprintf("from the unrestricted matrix (here %s), a point estimate (DMAR; ", sieve_item_omega_matrices_text(d$constructs)),
        "Kelley & Pornprasertmanit, 2016); the interval is on the Reliability tab. ",
        "Robust to misspecification of the total variance only. Beside it, the largest residual ",
        "correlation of the one-factor model and its item pair: a rising omega while a large residual ",
        "pair is retained signals redundancy, not better measurement. A small largest residual does ",
        "not certify unidimensionality: when shared variance sits on the dominant items, the single ",
        "factor absorbs it and the misfit appears among the other items."
      )
    })

    # Overall model fit (RMSEA with CI, CFI, AIC) for the correlated- (or
    # orthogonal-) factors measurement model across the included constructs.
    output$overall_fit <- renderUI({
      run <- tryCatch(fit_run(), error = function(e) NULL)
      if (is.null(run)) {
        return(NULL)
      }
      kind <- if (identical(input$factor_cov, "orthogonal")) "orthogonal" else "correlated"
      # One line (results first): what waits is said once, in
      # the status line above it, with the page's Update; this line names
      # the model and its estimate without a banner of its own.
      if (identical(run$status, "not run")) {
        plan <- run$plan
        if (!is.null(plan$refused)) {
          return(div(class = "small text-danger mb-1", tags$b("Overall measurement model: "), plan$refused))
        }
        return(div(
          class = "small text-muted mb-1",
          if (isTRUE(run$stale)) {
            tags$b("Overall measurement model: from before, the item pool changed; nothing is shown for it until it is fit again. ")
          } else {
            tags$b("Overall measurement model: not fit yet. ")
          },
          sprintf(
            paste0(
              "The %s-factor model of the %d included items under %s is estimated to take %s, ",
              "so it waits for Update above (a first model of two seconds or less is fit by itself). ",
              "The per item diagnostics do not depend on it."
            ),
            kind, plan$p, plan$est_label %||% plan$estimator, plan$label
          )
        ))
      }
      if (!is.null(run$error)) {
        return(div(
          class = "small text-danger mb-2",
          sprintf(
            "Overall measurement model (%s factors): the fit stopped with an error. %s",
            kind,
            run$error
          )
        ))
      }
      f <- run$fit
      if (is.null(f)) {
        return(div(
          class = "small text-muted mb-2",
          sprintf(
            "Overall measurement model (%s factors): no model could be written (a construct needs at least two included items).",
            kind
          )
        ))
      }
      design <- run$key$sampling %||% list(unit = "independent")
      fitted_on <- sprintf(
        " %s the current pool of %d items (%s, %.1f s); %s.",
        # A fit reused from another page (fct_fit_store.R) names that page.
        if (isTRUE(run$reused)) {
          sprintf("%s, the identical model on", sieve_reuse_text(run) %||% "Reused the fit for")
        } else {
          "Fit on"
        },
        run$p, run$est_label %||% run$estimator, if (isTRUE(run$reused)) run$fit_seconds %||% run$seconds else run$seconds,
        if (identical(design$unit, "clustered")) {
          sprintf("cluster-robust standard errors and test over the clusters in %s", design$cluster)
        } else {
          "rows treated as independent (no clustered or repeated sampling design is declared)"
        }
      )
      # A fitted object is not a certificate: no fit index is shown for a
      # model that did not converge or is not identified, and an improper
      # solution is named as one.
      st <- sieve_status_of(f)
      if (!sieve_has_solution(st)) {
        return(div(
          class = "small text-danger mb-2",
          sprintf("Overall measurement model (%s factors): no fit indices. %s", kind, st$message)
        ))
      }
      m <- sieve_fit_measures(f, conf_level = sieve_default_rmsea_conf_level())
      if (is.null(m)) {
        return(NULL)
      }
      div(
        class = "small text-muted mb-1",
        HTML(sprintf(
          paste0(
            "Overall measurement model (%s factors): %s = %.3f, %s [%.3f, %.3f], ",
            "%s = %.3f, AIC = %s (for comparing models; see below the grid).%s"
          ),
          kind, sieve_fit_label(m, "rmsea"), m["rmsea"], sieve_rmsea_ci_words(sieve_fit_ci_level(m)),
          m["rmsea_lo"], m["rmsea_hi"],
          sieve_fit_label(m, "cfi"), m["cfi"],
          if (is.finite(m["aic"])) formatC(m["aic"], format = "f", digits = 0, big.mark = ",") else "n/a",
          fitted_on
        )),
        if (identical(st$code, "inadmissible")) {
          tags$span(class = "text-danger", " ", st$message)
        }
      )
    })

    # What AIC is for, below the grid (in "More on how these diagnostics are
    # computed"), beside the overall model's line above it.
    output$overall_note <- renderUI({
      p(
        class = "small text-muted mt-2 mb-0",
        paste0(
          "The overall measurement model's AIC is for comparing models (lower is better), ",
          "only for models of the same observed variables and compatible likelihoods; an AIC change ",
          "after excluding items is not a comparable fit improvement. The model is the ",
          "confirmatory model implied by the construct assignments (each construct a factor, ",
          "correlated unless Orthogonal is chosen), fit with lavaan under the estimator shown; ",
          "the per item diagnostics do not depend on it."
        )
      )
    })

    # The model behind the loadings and h2, in one visible line: the
    # engine, extraction, rotation, number of factors, correlations and n.
    output$model_line <- renderUI({
      run <- tryCatch(diag_run(), error = function(e) NULL)
      d <- run$value
      if (is.null(d) || is.null(d$items)) {
        return(NULL)
      }
      k <- d$k %||% length(unique(d$items$construct))
      k_what <- if (identical(d$factor_level, "facet")) {
        "the number of facets of the one construct"
      } else {
        "the number of constructs"
      }
      # The EFA tab's kept solution: read here when it is the same model,
      # named with how the two models differ when it is not; followed as the
      # EFA tab keeps a solution (its signal).
      store <- sieve_efa_store(state, create = FALSE)
      if (!is.null(store$signal)) store$signal()
      match <- if (!is.null(run$key)) {
        sieve_efa_store_match(store, list(
          dataset_id = run$key$dataset, data = run$key$data, items = run$key$items,
          types = run$key$types, rows = run$key$rows, k = k, basis_used = d$basis_used,
          basis_missing = d$basis_missing,
          n = d$n_obs
        ))
      }
      seed_text <- if (!is.null(d$rng$seed) && k > 1L) sprintf(", rotation starts from seed %d", d$rng$seed) else ""
      source <- if (isTRUE(d$solution_reused)) {
        sprintf(
          paste(
            "the EFA tab's solution (fit at %s by DMAR::fa_exploratory, %s, whose estimates are those",
            "of maximum likelihood on these correlations%s)"
          ),
          d$efa_when %||% "an unrecorded time", match$solution$route %||% "maximum likelihood on the item responses",
          seed_text
        )
      } else {
        sprintf("DMAR::fa_exploratory, fit here%s", if (isTRUE(run$background)) " in a background R process" else "")
      }
      differs <- if (!isTRUE(d$solution_reused) && isTRUE(match$same_pool) && length(match$differences)) {
        sprintf(
          " The EFA tab's solution (fit at %s) is a different model (%s), so the diagnostics fit their own.",
          match$when %||% "an unrecorded time", paste(match$differences, collapse = "; ")
        )
      } else {
        ""
      }
      div(
        class = "small mb-1",
        tags$b("Model: "),
        sprintf(
          paste0(
            "loadings, h2 and cross loadings from %s, maximum likelihood, %s, ",
            "k = %d (%s), on the %s correlations of the %d included items, ",
            "%s%s; each construct's robust omega (observed denominator) and alpha from a one-factor model of its own items ",
            "on its own rows (%s), with the n beside each construct above.%s"
          ),
          source,
          if (k > 1L) "direct oblimin rotation (delta = 0)" else "one factor (no rotation)", k, k_what,
          sieve_retention_basis_label(d$basis_used %||% "pearson"), nrow(d$items),
          sieve_items_efa_rows_text(d),
          if (isTRUE(d$solution_reused)) "" else seed_text,
          sieve_items_reliability_rows_text(d),
          differs
        ),
        sieve_holdout_n_tag(state)
      )
    })

    # ---- Show code at the point of use ------------------------------------
    # The code behind the result on screen, read from the recorded step and
    # never recomputed. The diagnostics' record is taken when a computation
    # finishes (the step compute_diag() writes, kept with kept$diag); the
    # overall model's when a fit finishes, which is when that fit is
    # written to the reproducible code (fit_completion()). Each panel
    # compares the record's identity with the pool on screen: current,
    # from before (with what changed), not run, or failed. Opening a
    # panel, copying or downloading evaluates nothing.
    sieve_code_navigation(input, session, state)
    diag_record <- reactiveVal(NULL)
    # An observer, not observeEvent(ignoreInit = TRUE): the grid's outputs
    # are defined above this point and can compute the diagnostics in the
    # flush that first runs this observer, so the count it first sees can
    # already be one; each count is recorded once.
    observe({
      done <- diag_done()
      earlier <- isolate(diag_record())
      if (done < 1L || identical(earlier$done, done)) return()
      held <- kept$diag
      if (is.null(held)) return()
      value <- held$value
      record <- if (!is.null(value$value$items)) {
        d <- value$value
        k <- d$k %||% length(unique(held$key$constructs))
        sieve_code_record(state, "Items: exploratory diagnostics", context = c(
          sprintf("Included items: %s.", paste(held$key$items, collapse = ", ")),
          sprintf(
            "Exploratory factor analysis: DMAR::fa_exploratory, maximum likelihood, %s, k = %d (%s), on the %s correlations of %s.",
            if (k > 1L) "direct oblimin rotation (delta = 0)" else "one factor",
            k, if (identical(d$factor_level, "facet")) "the facets of the one construct" else "the constructs",
            sieve_retention_basis_label(d$basis_used %||% "pearson"), sieve_items_efa_rows_text(d)
          ),
          sprintf(
            "Robust omega (observed denominator) and alpha of each construct from a one-factor model of its own items on its own rows (%s); the flags are colored at display time from the thresholds, which refit nothing.",
            sieve_items_reliability_rows_text(d)
          )
        ))
      } else {
        earlier$record
      }
      diag_record(list(key = held$key, record = record, error = value$error, done = done))
    })
    output$diag_code <- renderUI({
      key <- tryCatch(diag_key(), error = function(e) NULL)
      rec <- diag_record()
      status <- if (is.null(rec)) {
        list(state = "not_run")
      } else if (!is.null(rec$error)) {
        list(state = "failed", sentence = rec$error)
      } else if (!identical(rec$key, key)) {
        st <- sieve_code_status(rec$record, state)
        if (identical(st$state, "current")) {
          st <- list(state = "stale", sentence = "What changed: the correlation basis or the estimator setting of the study.")
        }
        st
      } else {
        list(state = "current")
      }
      sieve_code_view(rec$record, status, ns = session$ns,
        title = "Show code: loadings, h2, cross loadings and construct reliability",
        readout = c(
          "items_loadings          # each item's loading on its own construct's factor, and the off-target loadings",
          "items_h2                # h2",
          "items_reliability       # robust omega, the change if each item were dropped, the largest residual"
        )
      )
    })
    model_record <- reactiveVal(NULL)
    observe({
      done <- fit_completion()
      earlier <- isolate(model_record())
      if (is.null(done) || identical(earlier$done, done)) return()
      held <- fit_store()
      automatic <- fit_kept$fit
      result <- if (!is.null(held) && identical(held$key, done$key)) {
        held$value
      } else if (!is.null(automatic) && identical(automatic$key, done$key)) {
        automatic$value
      }
      if (is.null(result)) return()
      record <- isolate(model_record())$record
      if (!is.null(result$fit)) {
        # The specification the fit used (run_fit()), rebuilt from the same
        # ledger and settings, with the sampling design applied as there.
        spec <- sieve_capture({
          built <- sieve_cfa_spec(isolate(state$meta),
            orthogonal = isTRUE(done$key$orthogonal),
            reserved = union(names(isolate(state$items)), names(isolate(state$raw))),
            estimator = done$key$estimator %||% "auto"
          )
          sieve_sampling_apply(built, state, isolate(state$items))$spec
        })
        if (spec$ok && !is.null(spec$value)) {
          k <- length(unique(done$key$constructs))
          sieve_log(
            state,
            sprintf(
              "Items: overall measurement model, the %d-factor CFA implied by the construct assignments (%s factors, %s, %d items).",
              k, if (isTRUE(done$key$orthogonal)) "orthogonal" else "correlated",
              spec$value$estimator, result$p %||% length(done$key$items)
            ),
            sieve_cfa_code(spec$value, fit_name = "items_overall_fit", model_name = "items_overall_model")
          )
          record <- sieve_code_record(state, "Items: overall measurement model", context = c(
            sprintf("Modeled item columns: %s.", paste(spec$value$observed, collapse = ", ")),
            sprintf(
              "Estimator: %s; missing data: %s; factors %s; std.lv = TRUE.",
              spec$value$estimator, spec$value$missing,
              if (isTRUE(done$key$orthogonal)) "orthogonal" else "correlated"
            ),
            sprintf(
              "The line above reads RMSEA with its %s interval, CFI, SRMR and AIC from this fit; the per item diagnostics do not depend on it.",
              sieve_ci_percent(sieve_default_rmsea_conf_level())
            )
          ))
        }
      }
      model_record(list(key = done$key, record = record, error = result$error, when = done$when, done = done))
    })
    output$model_code <- renderUI({
      key <- tryCatch(fit_key(), error = function(e) NULL)
      rec <- model_record()
      status <- if (is.null(rec)) {
        list(state = "not_run")
      } else if (!is.null(rec$error)) {
        list(state = "failed", sentence = rec$error)
      } else if (!identical(rec$key, key)) {
        st <- sieve_code_status(rec$record, state)
        if (identical(st$state, "current")) {
          st <- list(state = "stale", sentence = "What changed: the model settings (the factor covariances, the estimator or the sampling design).")
        }
        st
      } else {
        list(state = "current")
      }
      sieve_code_view(rec$record, status, ns = session$ns,
        title = "Show code: overall measurement model",
        readout = c(
          sprintf(
            'lavaan::fitMeasures(items_overall_fit, fm.args = list(rmsea.ci.level = %s))',
            sieve_num_lit(sieve_default_rmsea_conf_level())
          ),
          sprintf(
            "# The line above the grid labels the standard, scaled or robust family read from these measures, with the RMSEA interval at %s.",
            sieve_ci_percent(sieve_default_rmsea_conf_level())
          ),
          sprintf("DMAR::sem_fit_measures(items_overall_fit, conf_level = %s)  # the SRMR shown is its srmr row: of the covariance residuals alone and of the model's own variables (lavaan's srmr_bentler_nomean when the fit has no auxiliary variable; lavaan's srmr also averages in the mean residuals)", sieve_num_lit(sieve_default_rmsea_conf_level()))
        )
      )
    })

    # What the page's Update button would compute, and its estimate.
    update_plan <- reactive({
      d <- tryCatch(diag_run(), error = function(e) NULL)
      f <- tryCatch(fit_run(), error = function(e) NULL)
      wait_diag <- identical(d$status, "not run")
      wait_model <- identical(f$status, "not run") && is.null(f$plan$refused)
      seconds <- sum(c(if (wait_diag) d$plan$seconds, if (wait_model) f$plan$seconds))
      list(
        diag = wait_diag, model = wait_model, seconds = seconds,
        what = paste(c(if (wait_diag) "the item diagnostics", if (wait_model) "the overall model"), collapse = " and "),
        diag_stale = wait_diag && isTRUE(d$stale),
        model_stale = wait_model && isTRUE(f$stale),
        # A reopened project waits for Update.
        restored = (wait_diag && isTRUE(d$restored)) || (wait_model && isTRUE(f$restored))
      )
    })

    # The one sentence that says what waits for Update: the
    # diagnostics and the overall model together, from before an edit or
    # not computed yet.
    update_sentence <- function(up) {
      d_stale <- isTRUE(up$diag) && isTRUE(up$diag_stale)
      d_first <- isTRUE(up$diag) && !isTRUE(up$diag_stale)
      m_stale <- isTRUE(up$model) && isTRUE(up$model_stale)
      m_first <- isTRUE(up$model) && !isTRUE(up$model_stale)
      if (d_stale && m_stale) {
        "The item pool changed since the item diagnostics and the overall model were computed; they are withheld until they are updated. "
      } else if (d_stale) {
        paste0(
          "The item pool changed since the item diagnostics were computed; they are withheld until they are updated",
          if (m_first) ", and the overall model is not fit yet. " else ". "
        )
      } else if (d_first) {
        paste0(
          "The item diagnostics are not computed yet",
          if (m_stale) ", and the overall model is from before (the item pool changed). " else if (m_first) ", nor is the overall model. " else ". "
        )
      } else if (m_stale) {
        "The item pool changed since the overall model was fit; it is withheld until it is updated. "
      } else {
        "The overall model is not fit yet. "
      }
    }

    # The status line, always visible: an error in the diagnostics or in an
    # edit, a solution that is not admissible, what the engine said, and
    # the page's Update button when something waits for it.
    output$status_line <- renderUI({
      problem <- edit_problem()
      run <- tryCatch(diag_run(), error = function(e) NULL)
      d <- run$value
      up <- tryCatch(update_plan(), error = function(e) NULL)
      running <- identical(run$status, "running") && sieve_efa_dmar_active(efa_bg)
      tagList(
        # Drawn once per run, with its Cancel button; only the elapsed time
        # is redrawn each second (efa_progress, fct_progress_line.R).
        if (running) {
          div(
            class = "alert alert-info py-2 mb-2 small d-flex align-items-center gap-2", role = "status",
            tags$span(
              sprintf(
                paste(
                  "The exploratory factor analysis behind the loadings, h2 and cross loadings",
                  "(DMAR::fa_exploratory, maximum likelihood, k = %d, %d items) is running in a background",
                  "R process; the reliability columns follow when it finishes. "
                ),
                efa_bg$meta$k %||% NA_integer_, efa_bg$meta$p %||% NA_integer_
              ),
              sieve_progress_text(ns("efa_progress"))
            ),
            actionButton(ns("efa_cancel"), "Cancel the EFA", class = "btn-outline-secondary btn-sm")
          )
        },
        if (isTRUE(run$stopped) && !running) {
          div(class = "small text-muted mb-1", paste(
            "Canceled: the exploratory factor analysis was stopped before it finished, and nothing from it is kept.",
            "Update runs it again."
          ))
        },
        if (!is.null(efa_bg_error()) && identical(efa_bg_error()$key, run$key %||% tryCatch(diag_key(), error = function(e) NULL))) {
          div(class = "small text-warning", efa_bg_error()$message)
        },
        if (isTRUE(up$diag) || isTRUE(up$model)) {
          div(
            class = if (isTRUE(up$diag_stale) || isTRUE(up$model_stale)) "alert alert-warning py-2 mb-2 small" else "alert alert-info py-2 mb-2 small",
            tags$b(update_sentence(up)),
            if (isTRUE(up$restored)) paste0(sieve_restored_sentence("Update"), " "),
            sprintf(
              "Update computes %s%s, estimated at %s. %s",
              up$what, if (isTRUE(up$diag) && isTRUE(up$model)) " in turn" else "", sieve_duration_label(up$seconds),
              paste(
                "The item diagnostics follow each edit by themselves while their estimate is two seconds or less;",
                "anything longer, and an overall model fit before the edit, waits for this button."
              )
            ),
            div(class = "mt-1", actionButton(ns("update_go"),
              sprintf("Update (%s)", sieve_duration_label(up$seconds)),
              class = "btn-primary btn-sm"
            ))
          )
        },
        if (!is.null(problem)) div(class = "small text-danger", role = "alert", problem),
        if (!is.null(run) && !is.null(run$error)) {
          div(
            class = "small text-danger", role = "alert",
            sprintf("The item diagnostics stopped with an error and nothing is shown for them: %s", run$error)
          )
        },
        if (!is.null(run) && is.null(run$error) && is.null(d) && !identical(run$status, "not run")) {
          div(class = "small text-muted", "Need at least three included items in the pool to fit diagnostics.")
        },
        # The exploratory solution's own status: DMAR refuses an improper
        # solution (a residual variance below zero), one that did not
        # converge, and a likelihood without a maximum, by class, with its
        # reason; the refusal is named, and the loading columns are empty.
        if (!is.null(d$status) && !identical(d$status$code, "ok")) {
          div(class = "small text-danger", role = "alert", paste(
            "The exploratory solution behind the loadings, h2 and cross loadings was not fit:",
            d$status$message
          ))
        },
        if (length(d$notes)) div(class = "small text-warning", paste(d$notes, collapse = " ")),
        if (length(run$warnings)) {
          div(class = "small text-warning", paste("The engine reported:", paste(run$warnings, collapse = " ")))
        }
      )
    })

    # Fit context: the correlations, the listwise n, and what the
    # reliability values here are for.
    output$fit_note <- renderUI({
      run <- tryCatch(diag_run(), error = function(e) NULL)
      d <- run$value
      if (is.null(d)) {
        return(NULL)
      }
      tagList(
        div(
          class = "small text-muted",
          HTML(sprintf(
            paste0(
              "The EFA is fit to the %s correlation matrix of the included items, %s ",
              "(the matrix the Dimensionality and EFA tabs share); the corrected ",
              "item-total uses all available cases (pairwise). Each construct's reliability uses its own rows ",
              "(%s), as the Reliability tab does, so it does not change when items of another construct are ",
              "included or excluded. The reliability values here are ",
              "point estimates to help you decide which items to keep (no interval and no bootstrap ",
              "at each step); for robust omega with a confidence interval, see the Reliability tab."
            ),
            sieve_retention_basis_label(d$basis_used %||% "pearson"), sieve_items_efa_rows_text(d),
            sieve_items_reliability_rows_text(d)
          ))
        )
      )
    })
  })
}

#' The rows the Items tab's factor analysis reads, in words
#'
#' Under the saturated FIML matrix (`basis_missing = "fiml"`) the EFA reads
#' every row's available responses, and the complete cases are only a
#' reference; otherwise it reads the rows complete on every included item.
#'
#' @param d A result of `sieve_item_diagnostics()`.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_items_efa_rows_text <- function(d) {
  if (identical(d$basis_missing, "fiml")) {
    sprintf("the saturated FIML (EM) correlations of every row (complete-case reference n = %d)", as.integer(d$n_obs))
  } else {
    sprintf("n = %d complete cases", as.integer(d$n_obs))
  }
}

#' The rows the Items tab's reliability reads, in words
#'
#' @param d A result of `sieve_item_diagnostics()`.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_items_reliability_rows_text <- function(d) {
  aux <- d$reliability_aux
  if (identical(d$reliability_missing %||% "fiml", "fiml")) {
    paste0(
      "continuous items by full information maximum likelihood, every row with an observed item of the construct",
      if (length(aux)) sprintf(" or an auxiliary (%s)", paste(aux, collapse = ", ")) else "",
      "; ordered items on the rows complete on the construct's items, as polychoric correlations need"
    )
  } else {
    "the rows complete on the construct's own items, the study's complete-case treatment"
  }
}

#' What the Items tab's reliability reads from the study
#'
#' The Reliability tab's missing data treatment, which follows the study's
#' estimator (FIML under ML or MLR with FIML, complete cases under ML on
#' the complete cases; `sieve_lavaan_spec()`), and, under FIML, the
#' study's declared continuous auxiliaries, attached as the Reliability tab
#' attaches them (`sieve_auxiliary_apply()`), so the Items tab's omega,
#' alpha and omega if dropped are the Reliability tab's point estimates on
#' each construct's own rows.
#'
#' @param state The shared state.
#' @return A list with `data` (the item data, with the auxiliary columns
#'   under FIML), `missing` (`"fiml"` or `"listwise"`), `aux` (the
#'   auxiliary names, or `NULL`), `note` (why declared auxiliaries are not
#'   used, or `NULL`), and `key` (the identity of all of it).
#' @author Ken Kelley
#' @noRd
sieve_items_reliability_inputs <- function(state) {
  items <- state$items
  missing <- tryCatch(
    sieve_lavaan_spec(state$meta, estimator = sieve_estimator_effective("study", state))$missing,
    error = function(e) "fiml"
  )
  missing <- if (identical(missing, "listwise")) "listwise" else "fiml"
  out <- list(data = items, missing = missing, aux = NULL, note = NULL)
  # Read here so the identity follows a change of the declared roles.
  roles <- state$roles
  if (identical(missing, "fiml") && !is.null(state$raw) && !is.null(items)) {
    prepared <- tryCatch({
      spec <- sieve_cfa_spec(state$meta, reserved = names(items), estimator = "ml_fiml")
      sieve_auxiliary_apply(spec, state, items)
    }, error = function(e) list(reason = conditionMessage(e)))
    if (!is.null(prepared$reason)) {
      out$note <- paste("The declared auxiliaries are not used in the reliability values here:", prepared$reason)
    } else if (!is.null(prepared$data)) {
      aux <- setdiff(names(prepared$data), names(items))
      if (length(aux)) {
        out$data <- prepared$data
        out$aux <- aux
      }
    }
  }
  out$key <- list(
    missing = missing, aux = out$aux,
    aux_data = if (length(out$aux)) sieve_data_key(out$data[, out$aux, drop = FALSE]),
    note = out$note, roles = sieve_hash(roles)
  )
  out
}

# `%||%` (null-coalescing) lives in utils_state.R so it is defined once for the
# whole package.

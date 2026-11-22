# mod_sem.R
# Structural equation modeling: the structural model that sits on top of the
# confirmatory measurement model. Where the CFA tab estimates how items measure
# the latent constructs, this tab estimates relationships AMONG the latent
# constructs (and onto external observed outcomes). Because the regressions are
# among latent variables, the structural coefficients are disattenuated: they
# are not biased by the measurement error that contaminates regressions of sum
# scores or factor scores, which is why latent regressions are preferred.

#' SEM module UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput plotOutput selectInput radioButtons sliderInput downloadButton p strong
#' @importFrom bslib layout_sidebar sidebar card card_header navset_card_tab nav_panel layout_columns value_box
#' @importFrom DT DTOutput
mod_sem_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      title = "Structural model",
      p(
        class = "text-muted small",
        "Choose outcomes and the predictors to regress them on. ",
        "Outcomes may be other constructs (latent on latent) or external ",
        "numeric variables (the columns given the Criterion role on the Ingest ",
        "tab); predictors are constructs (latent) and antecedents (the columns given ",
        "the Antecedent role: variables that exist before the target construct, entered ",
        "as observed exogenous predictors). Relationships among the constructs are ",
        "estimated at the latent level, free of measurement-error attenuation; an ",
        "antecedent enters as measured, so its path carries its own measurement error."
      ),
      selectInput(ns("outcomes"), "Outcome(s)", choices = NULL, multiple = TRUE),
      selectInput(ns("predictors"), "Predictor(s): constructs and antecedents", choices = NULL, multiple = TRUE),
      # Shown when two or more constructs are both predictor and outcome:
      # their causal order makes the model recursive (serial mediation).
      uiOutput(ns("mediator_order_ui")),
      sieve_estimator_panel(ns),
      checkboxInput(ns("two_stage"), "Robust Two-Stage ML (Sensitivity Analysis)", FALSE),
      # The study's sampling design is declared on the Ingest tab, step 4,
      # for every page; this page states it and reads it.
      uiOutput(ns("sampling_note")),
      uiOutput(ns("outcome_types_ui")),
      selectInput(
        ns("exogenous_missing"), "Missing observed covariates",
        c(
          "Engine default (fixed covariates; missing covariates can delete rows)" = "engine_default",
          "Model covariates as random (joint continuous FIML model)" = "random",
          "FIML-X (retain missing fixed covariates)" = "fiml_x"
        )
      )
    ),
    sieve_optional_tab_note("structural relations among the constructs and onto outcomes, estimated at the latent level"),
    # Results first: the one status line, the one-line method statement
    # with the fit, then the result; how to read each panel is below it.
    uiOutput(ns("gate")),
    uiOutput(ns("method")),
    uiOutput(ns("two_stage_test")),
    navset_card_tab(
      nav_panel(
        "Path diagram",
        card(
          card_header("Structural model: regressions among latent constructs and onto outcomes"),
          sieve_diagram_panel(ns, kind = "sem", layout = "left_right"),
          sieve_more_below(
            tagList(
              "A single-headed arrow from a construct to one of its items is a ",
              strong("measurement loading"), ". A single-headed arrow from one construct to ",
              "another, or onto an outcome, is a ", strong("structural regression"),
              " (drawn heavier). The controls in this card change what is drawn and never refit the model."
            ),
            p(
              "A curved double-headed arrow between two predictors that no ",
              "other variable in the model predicts is their ", strong("covariance"),
              " (their correlation in a standardized display). A curved double-headed arrow ",
              "between the disturbance terms of two outcomes is a ",
              strong("residual (disturbance) covariance"),
              ": what the two outcomes share beyond their predictors, not the correlation of ",
              "the outcomes themselves. The direction of an arrow shows the model that was ",
              "specified; it does not by itself establish a causal direction."
            ),
            label = "More on reading the diagram: covariances and the direction of an arrow"
          )
        )
      ),
      nav_panel(
        "Structural paths",
        sieve_missing_info_ui(ns),
        sieve_mi_ui(ns),
        card(
          card_header("Standardized structural coefficients (with confidence intervals)"),
          DTOutput(ns("paths_tbl")),
          uiOutput(ns("sem_code"))
        ),
        card(
          card_header("Variance explained (R-squared) per outcome"),
          DTOutput(ns("r2_tbl")), DTOutput(ns("sem_row_flow"))
        ),
        layout_columns(
          col_widths = c(4, 4, 4),
          value_box(sprintf("RMSEA (%s CI)", sieve_ci_percent(sieve_default_rmsea_conf_level())),
            uiOutput(ns("vb_rmsea")), theme = "primary"),
          value_box("CFI", uiOutput(ns("vb_cfi")), theme = "secondary"),
          value_box("AIC", uiOutput(ns("vb_aic")), theme = "success")
        ),
        uiOutput(ns("fit_family")),
        uiOutput(ns("usrmr_action")),
        p(
          class = "small text-muted",
          "Each row names the fitted outcome scale. For binary or ordered outcomes, WLSMV uses an underlying response with a probit link. Its standardized paths and R-squared describe that response; they are not category-probability changes or variance explained in the observed category codes. Latent-outcome R-squared describes modeled latent-variable variance."
        )
      ),
      nav_panel(
        "Indirect and total effects",
        card(
          card_header("Mediation: indirect (X -> M -> Y) and total effects"),
          uiOutput(ns("effects_note")),
          DTOutput(ns("effects_tbl")),
          uiOutput(ns("effects_code")),
          sieve_boot_inputs(ns, B = 20000, B_id = "indirect_B", seed_id = "indirect_seed", min_B = 100, max_B = NULL, step_B = 1000, label = "Joint parameter Monte Carlo draws", width = NULL, seed_label = "Seed (optional)", seed_width = NULL, seed_min = 1),

          actionButton(ns("indirect_mc_go"), "Compute Monte Carlo Effect Intervals"),
          uiOutput(ns("indirect_mc_note")), DTOutput(ns("indirect_mc_table")),
          p(
            class = "text-muted small mt-2",
            "When a construct is selected as both a predictor and an outcome it acts as a ",
            strong("mediator"), ". The model is then fit with labeled paths (the labels change no estimate) and the ",
            "standardized ", strong("indirect"), " effects (the product of the X -> M and M -> Y ",
            "paths) and ", strong("total"), " effects (direct plus indirect) are reported with ",
            "confidence intervals. With two or more mediators, their order (set in the sidebar) ",
            "makes the model serial: an indirect effect is then the product of the paths along ",
            "each chain X -> M1 -> M2 -> Y, and a total effect is the direct path plus every chain, ",
            "the element of (I - B) inverse minus I for that pair. The indirect effect is usually ",
            "the scientifically interesting quantity in a mediation hypothesis."
          )
        )
      )
    )
  )
}

#' SEM module server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive req observe renderUI renderPlot div HTML tags
#' @importFrom shiny updateSelectInput downloadHandler
#' @importFrom DT renderDT datatable formatRound
mod_sem_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    # The tab's Estimator control: the study's setting (Ingest tab) unless
    # chosen otherwise here (sieve_estimator_wire()).
    estimator_choice <- sieve_estimator_wire(input, output, state)

    # Constructs (>= 2 active items) and external numeric variables available.
    constructs <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$meta)
      m <- state$meta[state$meta$status == "active", , drop = FALSE]
      tab <- table(m$construct[sieve_construct_assigned(m$construct)])
      sort(names(tab)[tab >= 2L])
    })
    # The external outcomes offered are the columns with the criterion role
    # (set on the Ingest tab) whose values read as numbers (see
    # sieve_numeric_columns(): a column stored as text that holds numerals
    # is read, one with entries that are not numbers is refused by name with
    # the reason) with more than two distinct values; identifiers, grouping
    # and background variables, and items are never offered, whatever their
    # values.
    aux_read <- reactive({
      req(state$raw)
      state$roles
      state$items
      cand <- intersect(sieve_role_vars(state, "criterion"), names(state$raw))
      if (!length(cand)) {
        return(list(offered = character(0), refused = character(0)))
      }
      read <- sieve_numeric_columns(state$raw, cand, what = "Criterion")
      refused <- read$text
      names(refused) <- read$problems$column
      few <- vapply(cand, function(v) {
        x <- read$data[[v]]
        !(v %in% read$problems$column) &&
          (sum(!is.na(x)) < 3L || length(unique(x[!is.na(x)])) < 2L)
      }, logical(1))
      if (any(few)) {
        more <- sprintf(
          "Criterion '%s' has fewer than two distinct values, so it is not offered as an observed outcome.",
          cand[few]
        )
        names(more) <- cand[few]
        refused <- c(refused, more)
      }
      list(offered = setdiff(cand, names(refused)), refused = refused)
    })
    aux_numeric <- reactive(aux_read()$offered)
    aux_refused <- reactive(aux_read()$refused)

    # The antecedents offered as observed predictors: the columns with the
    # antecedent role (never proposed by the application; the researcher
    # gives it on the Ingest tab) whose values read as numbers with at least
    # three distinct values. Refused ones are named with the reason.
    ante_read <- reactive({
      req(state$raw)
      state$roles
      state$items
      cand <- intersect(sieve_role_vars(state, "antecedent"), names(state$raw))
      if (!length(cand)) {
        return(list(offered = character(0), refused = character(0)))
      }
      read <- sieve_numeric_columns(state$raw, cand, what = "Antecedent column")
      refused <- read$text
      names(refused) <- read$problems$column
      few <- vapply(cand, function(v) {
        x <- read$data[[v]]
        !(v %in% read$problems$column) &&
          (sum(!is.na(x)) < 3L || length(unique(x[!is.na(x)])) <= 2L)
      }, logical(1))
      if (any(few)) {
        more <- sprintf(
          "Antecedent '%s' has fewer than three distinct values, so it is not offered as an observed predictor.",
          cand[few]
        )
        names(more) <- cand[few]
        refused <- c(refused, more)
      }
      list(offered = setdiff(cand, names(refused)), refused = refused)
    })
    # An antecedent is selected under a value of its own ("antecedent:<column>"),
    # so a construct and a column that share a name stay two things. No
    # antecedent gives no value: without recycle0 = TRUE, paste0() of a
    # zero-length vector returns the prefix alone, which would put an option
    # labeled NA (value "antecedent:") under Antecedents in every study
    # without one.
    ante_value <- function(x) paste0("antecedent:", x, recycle0 = TRUE)
    picked_predictors <- reactive({
      chosen <- unique(as.character(input$predictors))
      is_ante <- startsWith(chosen, "antecedent:")
      list(constructs = chosen[!is_ante], antecedents = sub("^antecedent:", "", chosen[is_ante]))
    })

    # Every outcome entry has one meaning. A label that is both a construct
    # and an external column is offered twice, as "y (construct)" and
    # "y (data column)", with a value of its own each, so the server knows
    # which model was asked for (a latent outcome or an observed one).
    outcome_choices <- reactive({
      ext <- tryCatch(aux_numeric(), error = function(e) character(0))
      sieve_outcome_choices(constructs(), ext)
    })
    picked_outcomes <- reactive({
      sieve_resolve_outcomes(input$outcomes, outcome_choices())
    })

    observe({
      cons <- constructs()
      tab <- outcome_choices()
      entries <- function(kind) {
        rows <- tab$kind == kind
        stats::setNames(as.list(tab$value[rows]), tab$label[rows])
      }
      # The outcomes and predictors in force are kept while they are
      # offered: an edit elsewhere (any ledger change, a label included)
      # re-reads the constructs, and offered without a selection the
      # selectors would be emptied, so the model on screen could not be
      # updated.
      updateSelectInput(session, "outcomes",
        choices = list(
          Constructs = entries("construct"),
          `External variables` = entries("external")
        ),
        selected = sieve_keep_choice(isolate(input$outcomes), tab$value, several = TRUE)
      )
      ante <- tryCatch(ante_read()$offered, error = function(e) character(0))
      updateSelectInput(session, "predictors",
        choices = list(
          Constructs = as.list(cons),
          Antecedents = stats::setNames(as.list(ante_value(ante)), ante)
        ),
        selected = sieve_keep_choice(isolate(input$predictors), c(cons, ante_value(ante)), several = TRUE)
      )
    })

    # The structural model is fit on request (fct_ondemand.R), as a result
    # record: the fitted object when there is one, its status (failed, not
    # converged, not identified, inadmissible, or ok), and the conditions
    # lavaan raised. The selected outcomes and predictors are the choice the
    # result is kept under; when a construct is both a predictor and an
    # outcome, the Run fits the model with labeled paths, so the indirect
    # and total effects are defined parameters of that one fit.
    output$sampling_note <- renderUI({
      state$sampling_design
      helpText(
        strong("Sampling design of the study: "), sieve_sampling_text(sieve_sampling_spec(state)),
        " (set on the Ingest tab, step 4, for every page). Clustered or repeated rows need the cluster ",
        "or respondent column there; the supported continuous path uses cluster-robust inference, which ",
        "does not by itself fit a longitudinal measurement model."
      )
    })
    sem_sync <- new.env(parent = emptyenv())
    sem_sync$written <- NULL
    sem_restore <- reactiveVal(NULL)
    observeEvent(state$sem_settings,
      {
        saved <- state$sem_settings %||% list()
        if (!length(saved) || identical(saved, sem_sync$written)) return()
        sem_restore(saved)
        updateSelectInput(session, "exogenous_missing", selected = saved$exogenous_missing %||% "engine_default")
        for (nm in names(saved$outcome_types)) updateSelectInput(session,
          paste0(
            "outcome_type_",
            sieve_input_key(nm)
          ),
          selected = saved$outcome_types[[nm]]
        )
      },
      ignoreNULL = TRUE
    )
    observe({
      picked <- picked_outcomes()
      ext <- picked$name[which(picked$kind == "external")]
      held <- isolate(state$sem_settings) %||% list()
      target <- sem_restore()
      if (!is.null(target)) {
        if (!identical(input$exogenous_missing, target$exogenous_missing %||% "engine_default")) return()
        pending <- intersect(ext, names(target$outcome_types))
        if (!all(vapply(
          pending,
          function(nm) identical(
            input[[paste0(
              "outcome_type_",
              sieve_input_key(nm)
            )]],
            target$outcome_types[[nm]]
          ),
          logical(1)
        ))) return()
        sem_restore(NULL)
      }
      types <- as.list(held$outcome_types %||% list())
      for (nm in ext) {
        value <- input[[paste0("outcome_type_", sieve_input_key(nm))]]
        if (!is.null(value)) types[[nm]] <- value
      }
      next_settings <- list(
        outcome_types = types,
        exogenous_missing = input$exogenous_missing %||% held$exogenous_missing %||% "engine_default"
      )
      if (!identical(held, next_settings)) {
        sem_sync$written <- next_settings
        state$sem_settings <- next_settings
      }
    })
    output$outcome_types_ui <- renderUI({
      picked <- picked_outcomes()
      ext <- picked$name[which(picked$kind == "external")]
      tagList(lapply(ext, function(nm) selectInput(session$ns(paste0("outcome_type_", sieve_input_key(nm))),
        paste(
          "Measurement level of",
          nm
        ),
        choices = c(
          "Choose explicitly for a binary outcome" = "unconfirmed",
          "Continuous" = "continuous",
          "Ordered categorical" = "ordered",
          "Binary" = "binary",
          "Nominal/count (unsupported)" = "nominal"
        ),
        selected = isolate(as.list(state$sem_settings$outcome_types %||% list())[[nm]]) %||% if (length(unique(stats::na.omit(state$raw[[nm]]))) == 2L) "unconfirmed" else "continuous"
      )))
    })
    # The mediators: constructs chosen as both predictor and outcome. With
    # two or more, the researcher sets their order (serial mediation); the
    # control keeps the order already set for the mediators still chosen.
    mediators_now <- reactive({
      picked <- picked_outcomes()
      intersect(picked_predictors()$constructs, picked$name[which(picked$kind == "construct")])
    })
    output$mediator_order_ui <- renderUI({
      med <- mediators_now()
      if (length(med) < 2L) {
        return(NULL)
      }
      held <- intersect(isolate(input$mediator_order) %||% character(0), med)
      tagList(
        selectInput(session$ns("mediator_order"),
          "Order of the mediators (serial mediation): pick the first, then the next",
          choices = med, selected = held, multiple = TRUE
        ),
        helpText(
          "Each mediator is regressed on the predictors and on the mediators before it, and ",
          "predicts the mediators after it and the outcomes; the indirect effects include every ",
          "chain through them. Without an order each mediator would be regressed on the others, ",
          "a feedback loop that is not identified, so the model is not fit until the order is set."
        )
      )
    })
    mediator_order <- reactive({
      med <- mediators_now()
      if (length(med) < 2L) {
        return(NULL)
      }
      given <- as.character(input$mediator_order %||% character(0))
      given[given %in% med]
    })
    output$two_stage_test <- renderUI({
      run <- path_run()
      req(identical(run$spec$missing, "robust.two.stage"))
      sieve_two_stage_residual_ui(run$two_stage_residual)
    })
    sem_choices <- reactive({
      picked <- picked_outcomes()
      # In the order selected: the order enters the model syntax and the
      # names of its covariance edges.
      preds <- picked_predictors()
      list(
        outcomes = picked$name, kinds = picked$kind,
        predictors = preds$constructs, antecedents = preds$antecedents,
        mediator_order = mediator_order(),
        estimator = estimator_choice(), two_stage = isTRUE(input$two_stage),
        outcome_types = stats::setNames(vapply(picked$name[which(picked$kind == "external")], function(nm) {
          input[[paste0(
            "outcome_type_",
            sieve_input_key(nm)
          )]] %||% as.list(state$sem_settings$outcome_types %||% list())[[nm]] %||% if (length(unique(stats::na.omit(state$raw[[nm]]))) == 2L) "unconfirmed" else "continuous"
        }, character(1)), picked$name[which(picked$kind == "external")]),
        exogenous_missing = input$exogenous_missing %||% "engine_default", sampling = sieve_sampling_spec(state)
      )
    })
    sem_key <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta, state$raw)
      req(length(input$outcomes) > 0, length(input$predictors) > 0)
      choices <- sem_choices()
      key <- sieve_demand_key(state, choices)
      external <- union(choices$outcomes[choices$kinds == "external"], choices$antecedents)
      external <- intersect(external, names(state$raw))
      # Observed outcomes and antecedents are inputs, not model choices.
      # Changing their values makes the same requested model stale; unrelated
      # raw columns and display labels leave its stored result current.
      key$external_data <- if (length(external)) {
        sieve_data_key(state$raw[, external, drop = FALSE])
      } else NULL
      key
    })
    sem_plan <- reactive({
      req(state$meta, length(input$outcomes) > 0, length(input$predictors) > 0)
      choices <- sem_choices()
      spec <- sieve_lavaan_spec(state$meta, estimator = choices$estimator)
      active <- state$meta[state$meta$status == "active" & sieve_construct_assigned(state$meta$construct), , drop = FALSE]
      involved <- union(choices$predictors, choices$outcomes[choices$kinds == "construct"])
      its <- active$item[active$construct %in% involved]
      external <- choices$outcomes[choices$kinds == "external"]
      antecedents <- choices$antecedents
      p <- length(its) + length(external) + length(antecedents)
      mediated <- length(intersect(choices$predictors, choices$outcomes[choices$kinds == "construct"])) > 0L
      # One fit, with labeled paths when there is a mediator: the labels and
      # the defined effects do not change the estimates.
      seconds <- sieve_demand_wait("sem", p, spec$estimator, k = 1L, missing = spec$missing)
      list(
        estimator = spec$estimator, missing = spec$missing, p = p, seconds = seconds,
        label = sieve_duration_label(seconds),
        progress = sprintf(
          "Structural model: %s, %d items%s%s", spec$estimator, length(its),
          if (length(external)) sprintf(
            ", %d external outcome%s", length(external),
            if (length(external) == 1L) "" else "s"
          ) else "",
          if (length(antecedents)) sprintf(
            ", %d antecedent%s", length(antecedents),
            if (length(antecedents) == 1L) "" else "s"
          ) else ""
        ),
        detail = if (mediated) {
          "Fitting the structural model in lavaan, with labeled paths that define the indirect and total effects."
        } else {
          "Fitting the structural model in lavaan."
        },
        sentence = sprintf(
          "Fitting the structural model of %d items%s%s under %s%s is estimated to take %s",
          length(its),
          if (length(external)) sprintf(
            " and %d external outcome%s", length(external),
            if (length(external) == 1L) "" else "s"
          ) else "",
          if (length(antecedents)) sprintf(
            " and %d antecedent%s", length(antecedents),
            if (length(antecedents) == 1L) "" else "s"
          ) else "",
          spec$estimator,
          if (mediated) " (with labeled paths for the indirect and total effects)" else "",
          sieve_duration_label(seconds)
        ),
        # Two or more mediators without an order: refused before anything is
        # fit, with the reason.
        refused = sieve_mediator_order_problem(mediators_now(), choices$mediator_order) %||%
          sieve_wlsmv_guard(c(its, external, antecedents), state$raw,
            ordered = intersect(its, spec$ordered %||% character(0)),
            estimator = spec$estimator
          )
      )
    })
    compute_sem <- function(key, plan) {
      items <- shiny::isolate(state$items)
      raw <- shiny::isolate(state$raw)
      meta <- shiny::isolate(state$meta)
      choices <- key$choices
      # With a mediator the model is fit once, with labeled paths and the
      # indirect and total effects as defined parameters: labels that are
      # all distinct constrain nothing and a defined parameter is not
      # estimated, so the fit is the unlabeled model's (every estimate and
      # the discrepancy identical). Both panels read that one fit.
      mediated <- length(intersect(choices$predictors, choices$outcomes[choices$kinds == "construct"])) > 0L
      fit_structural <- function(labeled) {
        sieve_run_structural(items, raw, meta, choices$outcomes, choices$predictors,
          effects = labeled, outcome_kind = choices$kinds, antecedents = choices$antecedents,
          estimator = choices$estimator %||% "auto", outcome_types = choices$outcome_types,
          exogenous_missing = choices$exogenous_missing %||% "engine_default",
          sampling_state = state, mediator_order = choices$mediator_order,
          two_stage = isTRUE(choices$two_stage)
        )
      }
      run <- fit_structural(mediated)
      effects <- NULL
      if (mediated && is.null(run$fit) && length(run$spec$problems)) {
        # The effects could not be defined (a mediator with no outcome it
        # predicts, for one): the model is fit without them, and the reason
        # is kept for the effects panel.
        effects <- run
        run <- fit_structural(FALSE)
      }
      if (!is.null(run$fit)) {
        # Read once with the fit (see mod_cfa.R): the baseline models behind
        # the measures and the delta method behind the standardized solution
        # cost seconds on a large model.
        run$measures <- sieve_fit_measures(run$fit, conf_level = sieve_default_rmsea_conf_level(), residual = TRUE)
        # The analysis record holds the measures the page shows, with the
        # unbiased SRMR or the reason it was not computed, as the CFA page's
        # record does: sieve_run_structural() reads them without it, and the
        # saved-results report and its references read the record.
        run$estimates$fit <- run$measures
        run$standardized <- sieve_capture(lavaan::standardizedSolution(run$fit))
        note <- sprintf(
          "Fit the structural model: %s on %s",
          paste(choices$outcomes, collapse = ", "),
          paste(
            c(
              choices$predictors,
              if (length(choices$antecedents)) sprintf("%s (antecedent)", choices$antecedents)
            ),
            collapse = ", "
          )
        )
        run$estimates$R2_intervals <- sieve_R2_intervals(run$fit)
        run$analysis_id <- sieve_register_analysis(state, run, note = note)
        # The first step this tab records fits the model and defines sem_fit,
        # which every later step of the tab reads, so the recorded script
        # runs top to bottom.
        labeled <- length(attr(run$fit, "eff_map")) > 0L
        sieve_log(
          state, sprintf(
            "%s (lavaan::sem, %s, missing = \"%s\")%s.", note,
            run$spec$estimator, run$spec$missing,
            if (labeled) {
              "; the paths are labeled, so the indirect and total effects are defined parameters of this one fit"
            } else ""
          ),
          c(
            if (length(run$aux_notes)) paste("#", run$aux_notes),
            sieve_sem_code(run$spec),
            # The residual statistic computed beside a robust two-stage fit
            # (see sieve_two_stage_residual()).
            if (identical(run$spec$missing, "robust.two.stage")) run$two_stage_residual$code,
            # The unbiased SRMR beside the SRMR of the method line (see
            # sieve_fit_usrmr()).
            sieve_usrmr_code(attr(run$measures, "families")$usrmr, "sem_fit")
          )
        )
        # The record a disclosure beside the tables reads: the step just
        # written, with its settings.
        run$code_view <- sieve_code_snapshot(state, context = c(
          sprintf("Outcomes: %s; predictors: %s%s.",
            paste(choices$outcomes, collapse = ", "), paste(choices$predictors, collapse = ", "),
            if (length(choices$antecedents)) sprintf("; antecedents: %s", paste(choices$antecedents, collapse = ", ")) else ""),
          sprintf("Estimator: %s; missing data: %s; std.lv = TRUE.", run$spec$estimator, run$spec$missing),
          run$aux_notes,
          if (labeled) "The paths are labeled, so the indirect and total effects are defined parameters of this one fit.",
          "Standardized coefficients and their intervals come from lavaan's standardizedSolution (std.all); they are displayed to three decimal places."
        ))
      }
      # The effects panel reads the same run (with or without a solution).
      if (mediated && is.null(effects)) {
        effects <- run
      }
      list(path = run, effects = effects, error = run$error)
    }
    demand <- sieve_demand_wire(
      input, session, state,
      id = "fit_go", what = "Structural model",
      key = sem_key, plan = sem_plan, compute = compute_sem, kind = "sem", capability = "cluster",
      noun = "The structural model", action = "Fit the structural model",
      refit = "Update"
    )
    path_run <- reactive({
      r <- demand$result()
      req(r)
      r$path
    })

    # Estimates are shown only for a model that has a solution: one that did
    # not converge or is not identified has none, and the gate says why.
    fit <- reactive({
      run <- path_run()
      if (sieve_has_solution(run)) run$fit else NULL
    })

    output$gate <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(sieve_gate(state))
      }
      refused <- aux_refused()
      refused_ante <- tryCatch(ante_read()$refused, error = function(e) character(0))
      refused_note <- tagList(
        if (length(refused)) {
          div(
            class = "alert alert-warning small",
            strong("Not offered as an outcome: "), paste(refused, collapse = " ")
          )
        },
        if (length(refused_ante)) {
          div(
            class = "alert alert-warning small",
            strong("Not offered as a predictor: "), paste(refused_ante, collapse = " ")
          )
        }
      )
      if (!length(input$outcomes) || !length(input$predictors)) {
        return(tagList(
          refused_note,
          div(
            class = "alert alert-info",
            "Select at least one outcome and one predictor (a construct, or an antecedent) in the sidebar to fit the structural model."
          )
        ))
      }
      # The one line that says whether the model is not fit yet, current,
      # or stopped with an error, with its button; or, when it is from
      # before a change, what changed, with the page's one Update.
      # Then what was established about the fitted model (a fitted object
      # is not a certificate).
      r <- demand$result()
      tagList(
        refused_note,
        page_update$line() %||% tagList(demand$line(), page_update$note()),
        if (!is.null(r)) sieve_status_alert(r$path, what = "structural model")
      )
    })

    # The one-line method statement of the fitted model, with its fit.
    output$method <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed) || !length(input$outcomes) || !length(input$predictors)) {
        return(NULL)
      }
      r <- demand$result()
      if (is.null(r) || !sieve_has_solution(r$path)) {
        return(NULL)
      }
      sieve_method_line(r$path$fit, "Structural model",
        measures = tryCatch(fm(), error = function(e) r$path$measures), notes = r$path$aux_notes)
    })

    # The diagram specification is built once per fitted model, with the
    # item labels of the ledger and the construct and outcome names the fit
    # carries. The display controls are applied to it by
    # sieve_diagram_wire(), which never reads the fitted model.
    diagram_base <- reactive({
      f <- fit()
      # The readable names are passed as an option, so the logged call
      # regenerates the same labels from a refitted model.
      names_fit <- attr(f, "name_map")
      names_fit <- names_fit[names(names_fit) != names_fit]
      spec <- sieve_diagram_spec(f,
        labels = sieve_diagram_item_labels(state$meta),
        name_map = if (length(names_fit)) names_fit else NULL
      )
      if (is.null(f) || !sieve_diagram_drawable(spec)) {
        return(spec)
      }
      # The diagram carries the identifier of the fit it shows: the analysis
      # recorded when that model was fit (compute_sem), with the revision and
      # the respondents it was fit on. It is never recorded again from here:
      # this reactive is evaluated again after any change of the inputs,
      # while the fit on screen may be from before the change (a respondent
      # removed, an item reverse scored), and a record made now would carry
      # the current revision and inputs for an earlier fit (the CFA diagram
      # reads its run's identifier in the same way).
      sieve_diagram_update(spec, analysis_id = path_run()$analysis_id)
    })
    # The logged diagram call acts on sem_fit, which the step that fit the
    # model defined (path_run); the export step does not repeat the fit.
    diagram <- sieve_diagram_wire(
      input, output, session, state, diagram_base,
      layout = "left_right", fit_name = "sem_fit", file_stem = "siever_structural_diagram",
      what = "structural model"
    )

    # The unbiased SRMR of a model too large to compute it with the fit is
    # offered on a button under the boxes (see sieve_usrmr_wire()).
    usrmr_merge <- sieve_usrmr_wire(input, output, session, state,
      fit = function() fit(), key = function() path_run()$analysis_id,
      measures = function() fm(), fit_name = "sem_fit", page = "SEM"
    )
    fm <- reactive({
      f <- fit()
      req(f)
      usrmr_merge(sieve_fit_measures_at(path_run()$measures, f, sieve_default_rmsea_conf_level(), residual = TRUE))
    })
    # Each box names the family its value is in (robust, scaled, or standard).
    family_tag <- function(m) {
      family <- sieve_fit_family(m, "index")
      if (is.na(family)) "" else sprintf("<br><span style='font-size:0.55em'>%s</span>", family)
    }
    output$vb_rmsea <- renderUI({
      m <- fm()
      req(m)
      # Degrade a non-finite RMSEA (df = 0) and its CI to "n/a" rather than "NA".
      if (!is.finite(m["rmsea"])) {
        return(HTML("n/a<br><span style='font-size:0.7em'>df = 0 (just identified)</span>"))
      }
      ci <- if (is.finite(m["rmsea_lo"]) && is.finite(m["rmsea_hi"])) {
        sprintf("[%.3f, %.3f]", m["rmsea_lo"], m["rmsea_hi"])
      } else "CI n/a"
      HTML(sprintf("%.3f<br><span style='font-size:0.7em'>%s</span>%s", m["rmsea"], ci, family_tag(m)))
    })
    output$vb_cfi <- renderUI({
      m <- fm()
      req(m)
      HTML(if (is.finite(m["cfi"])) sprintf("%.3f%s", m["cfi"], family_tag(m)) else "n/a")
    })
    # Which family the two boxes are in, and why.
    output$fit_family <- renderUI({
      m <- fm()
      req(m)
      note <- sieve_fit_note(m)
      if (!length(note)) {
        return(NULL)
      }
      div(class = "small text-muted mb-2", lapply(note, function(x) tags$div(x)))
    })
    output$vb_aic <- renderUI({
      m <- fm()
      req(m)
      HTML(if (is.finite(m["aic"])) formatC(m["aic"], format = "f", digits = 0, big.mark = ",") else "n/a")
    })

    # A table that could not be read from the fit says so, in the table.
    unreadable <- function(read, what) {
      sieve_dt(
        data.frame(Note = sprintf("The %s could not be read from the fitted model: %s", what, read$error)),
        rownames = FALSE, selection = "none", options = list(dom = "t")
      )
    }

    output$paths_tbl <- renderDT({
      f <- fit()
      req(f)
      read <- path_run()$standardized %||% sieve_capture(lavaan::standardizedSolution(f))
      if (!read$ok) {
        return(unreadable(read, "standardized solution"))
      }
      ss <- read$value
      paths <- ss[ss$op == "~", , drop = FALSE]
      req(nrow(paths) > 0)
      nm <- attr(f, "name_map")
      relabel <- function(x) ifelse(x %in% names(nm), unname(nm[x]), x)
      df <- data.frame(
        Outcome = relabel(paths$lhs), Predictor = relabel(paths$rhs),
        outcome_scale = sieve_outcome_scale(f, paths$lhs)$outcome_scale,
        beta = paths$est.std, CI.lower = paths$ci.lower, CI.upper = paths$ci.upper,
        p = paths$pvalue, stringsAsFactors = FALSE
      )
      # Logging from inside a render is safe here only because sieve_log()
      # isolate()s its read of state$codelog (so it creates no reactive
      # dependency) and skips a step identical to the previous one (so a tab
      # revisit or a formatRound redraw does not append duplicates). The
      # specification is repeated because a cached model can be revisited
      # after another structural model has been fit.
      sieve_log(
        state,
        "SEM: standardized structural regressions of the outcomes on the predictor constructs, from sem_fit.",
        c(
          sieve_sem_code(path_run()$spec),
          "sem_std <- lavaan::standardizedSolution(sem_fit)",
          "sem_std[sem_std$op == \"~\", ]  # outcome ~ predictor: est.std with its interval and p",
          "SIEVER::sieve_outcome_scale(sem_fit, unique(sem_std$lhs[sem_std$op == \"~\"]))"
        )
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25),
        colnames = c("Outcome", "Predictor", "Outcome scale", "beta", "CI lower", "CI upper", "p")
      ) |>
        formatRound(c("beta", "CI.lower", "CI.upper", "p"), 3)
    })

    output$r2_tbl <- renderDT({
      f <- fit()
      req(f)
      read <- sieve_capture(lavaan::lavInspect(f, "r2"))
      if (!read$ok) {
        return(unreadable(read, "variance explained"))
      }
      tab <- path_run()$estimates$R2_intervals %||% sieve_R2_intervals(f)
      # A table with no R-squared to show is the reason, never a blank.
      if (is.null(tab) || !nrow(tab) || all(is.na(tab$R2))) {
        said <- unique(tab$reason[nzchar(tab$reason %||% "")])
        return(sieve_dt(
          data.frame(Note = if (length(said)) said else "sieve_R2_intervals() returned no row for this model."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      named <- !is.na(tab$outcome)
      tab$outcome[named] <- sieve_show_names(tab$outcome[named], f)
      sieve_log(
        state, "SEM: R-squared of each structural outcome with its logit-scale Wald interval",
        c(sieve_sem_code(path_run()$spec), "sem_R2 <- SIEVER::sieve_R2_intervals(sem_fit)")
      )
      labels <- c(
        group = "Group", model_level = "Level of the model", outcome = "Outcome", R2 = "R-squared",
        se = "SE", lower = "CI lower", upper = "CI upper", conf_level = "Confidence level",
        method = "Method", reason = "Reason", outcome_scale = "Outcome scale"
      )
      # The group and level columns are shown when the model has groups or
      # levels; every other column is shown always.
      shown <- names(tab)[!(names(tab) %in% c("group", "model_level") & vapply(tab, function(x) all(is.na(x)), logical(1)))]
      sieve_dt(tab[shown],
        rownames = FALSE,
        selection = "none",
        colnames = unname(labels[shown]),
        options = list(
          dom = "t",
          scrollX = TRUE
        )
      ) |>
        formatRound(c("R2", "se", "lower", "upper"), 3) |>
        formatRound("conf_level", 2)
    })

    # Mediation: the model with labeled paths, which is the one fit of the
    # Run when a mediator is present (a construct chosen as both predictor
    # and outcome).
    med_run <- reactive({
      r <- demand$result()
      req(r)
      r$effects
    })

    med_fit <- reactive({
      run <- med_run()
      if (!is.null(run) && sieve_has_solution(run)) run$fit else NULL
    })

    output$effects_note <- renderUI({
      r <- demand$result()
      if (is.null(r)) {
        return(div(
          class = "alert alert-info",
          "The structural model is not fit yet: press Fit the structural model above the tabs. ",
          "The indirect and total effects are fit with it when a construct is selected as ",
          strong("both"), " a predictor and an outcome (so X -> M -> Y)."
        ))
      }
      run <- r$effects
      if (is.null(run) || !length(run$spec$mediators)) {
        return(div(
          class = "alert alert-info",
          "No mediator is present. Select a construct as ", strong("both"),
          " a predictor and an outcome (so X -> M -> Y) to estimate indirect and total effects."
        ))
      }
      sieve_status_alert(run, what = "model with indirect effects")
    })

    output$effects_tbl <- renderDT({
      f <- med_fit()
      req(f)
      read <- sieve_capture(lavaan::standardizedSolution(f))
      if (!read$ok) {
        return(unreadable(read, "standardized solution of the model with labeled paths"))
      }
      ss <- read$value
      defs <- ss[ss$op == ":=", , drop = FALSE]
      req(nrow(defs) > 0)
      sieve_log(
        state,
        "SEM: standardized indirect and total effects, the defined parameters of sem_fit.",
        c(
          sieve_sem_code(med_run()$spec),
          "sem_std_effects <- lavaan::standardizedSolution(sem_fit)",
          "sem_std_effects[sem_std_effects$op == \":=\", ]  # the defined effects with their intervals"
        )
      )
      emap <- attr(f, "eff_map")
      df <- data.frame(
        Effect = ifelse(defs$lhs %in% names(emap), unname(emap[defs$lhs]), defs$lhs),
        beta = defs$est.std, CI.lower = defs$ci.lower, CI.upper = defs$ci.upper,
        p = defs$pvalue, stringsAsFactors = FALSE
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25),
        colnames = c("Effect", "std. estimate", "CI lower", "CI upper", "p")
      ) |>
        formatRound(c("beta", "CI.lower", "CI.upper", "p"), 3)
    })
    sieve_missing_info_server(input, output, session, state, path_run,
      function() identical(demand$status()$state, "current"))
    sieve_mi_server(input, output, session, state, path_run,
      function() identical(demand$status()$state, "current"))
    sem_inference <- sieve_sem_evidence_server(
      input, output, session, state, path_run, med_run,
      function() identical(demand$status()$state, "current")
    )

    # ---- One Update for the page --------------------------------------------
    # The structural model is the tab's one result fit on request; the
    # Monte Carlo intervals of the effects keep their own button.
    mc_entry <- function() {
      read <- sieve_outcome(sem_inference())
      if (!read$ready || !read$ok || is.null(read$value) || !is.null(read$value$result$error)) {
        return(NULL)
      }
      state$dataset_id
      state$items
      state$meta
      if (is.null(sieve_stale_reason(read$value$revision, sieve_revision(state))) &&
        identical(demand$status()$state, "current")) {
        return(NULL)
      }
      sieve_update_entry("Monte Carlo effect intervals", "stale",
        update = FALSE, button = "Compute Monte Carlo Effect Intervals", why = "a Monte Carlo interval"
      )
    }
    page_update <- sieve_update_wire(input, session, function() {
      list(
        structural = sieve_update_demand_entry(demand, "Structural model", sem_plan),
        intervals = mc_entry()
      )
    })

    # ---- Show code at the point of use --------------------------------------
    # The record kept with the held run (compute_sem()), under the demand
    # status: current, from before (with what changed), not fit or failed.
    # Reading a panel evaluates nothing.
    sieve_code_navigation(input, session, state)
    output$sem_code <- renderUI({
      st <- demand$status()
      # The unbiased SRMR's line follows what the page holds for the fit on
      # screen (sieve_usrmr_code(); see the CFA page).
      usrmr <- attr(tryCatch(fm(), error = function(e) st$value$path$measures), "families")$usrmr
      sieve_code_view(st$value$path$code_view, st, ns = session$ns,
        title = "Show code: structural model and standardized paths",
        readout = c(
          "sem_std <- lavaan::standardizedSolution(sem_fit)",
          'sem_std[sem_std$op == "~", ]  # outcome ~ predictor: est.std with its interval and p',
          'lavaan::lavInspect(sem_fit, "r2")',
          sprintf(
            "lavaan::fitMeasures(sem_fit, fm.args = list(rmsea.ci.level = %s))  # the fit boxes: the RMSEA interval at %s",
            sieve_num_lit(sieve_default_rmsea_conf_level()), sieve_ci_percent(sieve_default_rmsea_conf_level())
          ),
          sprintf("DMAR::sem_fit_measures(sem_fit, conf_level = %s)  # the SRMR shown is its srmr row: of the covariance residuals alone and of the model's own variables (lavaan's srmr_bentler_nomean when the fit has no auxiliary variable; lavaan's srmr also averages in the mean residuals)", sieve_num_lit(sieve_default_rmsea_conf_level())),
          sieve_usrmr_code(usrmr, "sem_fit")
        ))
    })
    output$effects_code <- renderUI({
      st <- demand$status()
      run <- st$value$effects
      sieve_code_view(run$code_view %||% st$value$path$code_view, st, ns = session$ns,
        title = "Show code: indirect and total effects",
        readout = c(
          "sem_std_effects <- lavaan::standardizedSolution(sem_fit)",
          'sem_std_effects[sem_std_effects$op == ":=", ]  # the defined effects with their intervals'
        ))
    })
  })
}

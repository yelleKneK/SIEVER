# mod_validity.R
# Validity evidence (draft). Convergent and discriminant evidence framed
# around the multitrait-multimethod logic: average variance extracted (AVE)
# beside each construct's largest correlation with another, the heterotrait-monotrait
# ratio (HTMT) as an optional adjunct, and common method variance screening
# (Harman's single-factor screen, the Lindell and Whitney marker adjustment,
# and the unmeasured latent method factor). The AVE, HTMT, Harman, and
# marker computations all come from DMAR (average_variance_extracted, htmt,
# common_method_single_factor, common_method_marker), so the app and the
# package report identical numbers. This tab is a working draft and is
# intentionally lighter than the core CFA/EFA/Reliability tabs.

#' Validity evidence module UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput checkboxInput selectInput radioButtons hr helpText
#' @importFrom shiny numericInput actionButton div tags
#' @importFrom bslib layout_sidebar sidebar card card_header card_body navset_card_tab nav_panel
#' @importFrom DT DTOutput
mod_validity_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      title = "Validity evidence",
      div(
        class = "text-muted small",
        "Convergent and discriminant evidence on the active correlated-factors",
        " model, criterion related validity against external variables, and",
        " common method variance. Draft."
      ),
      sieve_estimator_panel(ns, "Estimator for the models on this tab"),
      sieve_conf_level_input(ns("conf_level"),
              "Confidence level (two-sided intervals for the criterion and latent correlations; one-sided upper bound for HTMT)",
              value = 0.95,
              min = 0.5, max = 0.999, step = 0.01
            ),
      hr(),
      selectInput(ns("criteria"), "Criterion variables (external)", choices = NULL, multiple = TRUE),
      radioButtons(
        ns("score_method"), "Construct score, for every construct",
        choices = c(
          "Mean of the answered items" = "mean",
          "Bartlett factor score" = "bartlett",
          "Regression factor score" = "regression",
          # Shown, never applied: the constructs do not share one of the
          # three scores.
          "As specified per construct" = "mixed"
        ),
        selected = "mean"
      ),
      uiOutput(ns("score_method_state")),
      helpText(
        "Each construct has one operational score specification (Reliability tab, Operational ",
        "scores: the score, and the minimum answered count or proportion a respondent needs to ",
        "receive it). This control sets the score of every construct at once; the criterion ",
        "note names what is in force, construct by construct, with the eligible n. The ",
        "specification governs the criterion correlations and the marker adjustment here; HTMT ",
        "reads the items, not the scores. ",
        "Criterion variables are the numeric columns whose role is criterion or auxiliary ",
        "on the Ingest tab's Variable roles panel; identifiers and groups are not offered. ",
        "A mean score is a simple composite of the answered items; a factor score is computed ",
        "from the fitted measurement model and is unavailable until that model is fit (nothing ",
        "is substituted for it). Neither removes measurement error: the ",
        "correlation of an estimated score with a criterion is, as a rule, pulled toward ",
        "zero relative to the latent relation, and factor scores do not avoid that. The ",
        "Structural model tab estimates the relation at the latent level. On the ",
        "Criterion validity tab, state what your theory predicts for each construct ",
        "and criterion; a pair you leave without a prediction is reported and not scored."
      ),
      hr(),
      selectInput(ns("marker"), "CMV marker variable", choices = c("(none)" = "")),
      helpText(
        "A variable that theory says is unrelated to the constructs. The Lindell and Whitney ",
        "(2001) adjustment assumes that its correlation with them reflects only a shared method ",
        "and partials that from the inter-construct correlations; the adjusted values, read with ",
        "their sensitivity to larger marker correlations, show how much the correlations depend on ",
        "that assumption, and they neither establish nor rule out method variance."
      )
    ),
    # Results first: the one status line (with the page's one action, Update,
    # when a result is from before), the one-line method statement of the
    # measurement model, then the tables; what construct validity is, below
    # them.
    uiOutput(ns("gate")),
    uiOutput(ns("method")),
    navset_card_tab(
      nav_panel(
        "Construct correlations and AVE",
        card(
          card_header("Inter-construct latent correlations (with CIs)"),
          DTOutput(ns("phi_tbl"))
        ),
        card(
          card_header("Average Variance Extracted and the Largest Correlation With Another Construct"),
          DTOutput(ns("ave_tbl")),
          uiOutput(ns("ave_code")),
          tags$hr(),
          div(class = "d-flex align-items-center gap-2 flex-wrap",
            sieve_boot_inputs(ns, B = 1000L, B_id = "ave_B", seed_id = "ave_seed", min_B = 400L),
            sieve_conf_level_input(ns("ave_level"), "AVE confidence level", value = .95),
            actionButton(ns("ave_boot"), "Compute AVE intervals", class = "btn-primary")),
          uiOutput(ns("ave_interval_plan")),
          uiOutput(ns("ave_interval_note")),
          DTOutput(ns("ave_interval_tbl")),
          uiOutput(ns("ave_interval_code"))
        ),
        sieve_bound_ui(ns)
      ),
      nav_panel(
        "Criterion validity",
        card(
          card_header("Construct to criterion correlations (with CIs)"),
          # Not fillable: the note, the prediction controls, the table, and
          # the counts lie in document flow and the card scrolls.
          card_body(
            fillable = FALSE,
            uiOutput(ns("crit_note")),
            uiOutput(ns("expectations")),
            uiOutput(ns("stated_list")),
            DTOutput(ns("crit_tbl")),
            uiOutput(ns("crit_code")),
            uiOutput(ns("crit_tally")),
            uiOutput(ns("crit_kept")),
            uiOutput(ns("crit_more"))
          )
        )
      ),
      nav_panel(
        "Structural model",
        card(
          card_header("Latent regression of criteria on the constructs (SEM)"),
          uiOutput(ns("sem_note")),
          DTOutput(ns("sem_tbl"))
        ),
        card(
          card_header("Variance explained and model fit"),
          DTOutput(ns("sem_r2"))
        )
      ),
      nav_panel(
        "HTMT (optional)",
        card(
          card_header("Heterotrait-monotrait ratio of correlations"),
          actionButton(ns("htmt_on"), "Compute HTMT", class = "btn-primary mb-2"),
          uiOutput(ns("htmt_note")),
          DTOutput(ns("htmt_tbl")),
          uiOutput(ns("htmt_code")),
          tags$hr(),
          card_header("Bootstrap confidence intervals (Henseler, Ringle & Sarstedt, 2015)"),
          # The request pattern of the Reliability tab's bootstraps: the
          # method, the level, the resampling unit, the
          # resamples and the seed, an estimate from a measured rate before
          # the run, the counts attempted and used after it, and the reuse
          # of a completed request that matches.
          div(
            class = "d-flex align-items-center gap-2 mb-2 flex-wrap",
            sieve_boot_inputs(ns, B = 1000, B_id = "htmt_B", seed_id = "htmt_seed", min_B = 200, max_B = NULL, step_B = 100, label = "Bootstrap resamples", width = "180px", seed_label = "Seed (optional)", seed_width = "140px"),
            actionButton(ns("htmt_boot"), "Compute HTMT CIs", class = "btn-primary")
          ),
          uiOutput(ns("htmt_boot_plan")),
          uiOutput(ns("htmt_ci_note")),
          DTOutput(ns("htmt_ci_tbl")),
          uiOutput(ns("htmt_ci_code")),
          tags$small(
            class = "text-muted d-block mt-2",
            "The table gives each pair's HTMT and its one-sided upper confidence bound. ",
            sieve_htmt_convention_text(), " An upper bound well below 1 is evidence that two ",
            "constructs separate and does not by itself establish discriminant validity, and a ",
            "point estimate alone is weaker evidence than its bound."
          )
        )
      ),
      nav_panel(
        "Measurement direction",
        card(
          card_header("Reflective or Formative? The Assumption Behind Every Model Here"),
          p(
            class = "small mb-2",
            "Three questions about theory, for each construct, examine the reflective ",
            "assumption; what it is and why it matters is below them."
          ),
          uiOutput(ns("direction_ui")),
          uiOutput(ns("direction_verdict")),
          p(
            class = "small text-muted mt-2",
            "Every model in this environment treats indicators as ", strong("reflective"),
            ": the construct causes the item responses, so under the model the items are ",
            "exchangeable indicators of one common cause, should correlate, and the ",
            "construct is defined independently of any one of them. That exchangeability ",
            "is about the cause, not the content: dropping items can narrow the content ",
            "the scale represents, and choosing items for their intercorrelation alone ",
            "raises internal consistency at the expense of validity, the attenuation ",
            "paradox (Loevinger, 1954; Clark & Watson, 2019), so an item is dropped on ",
            "its evidence with the scale's content coverage in view. Some constructs are ",
            "built the other way: ",
            strong("formative"), " (causal or composite) indicators ", em("define"),
            " the construct (socioeconomic status from education, income, and ",
            "occupation is the standard example): causality runs from items to ",
            "construct, indicators need not correlate, and dropping one changes the ",
            "construct's meaning (Bollen & Lennox, 1991; Bollen & Bauldry, 2011). ",
            "Fitting a reflective CFA to formative indicators is a specification ",
            "error no fit index reliably flags. Answer the three questions for each ",
            "construct; a formative-leaning answer raises a warning that the factor ",
            "analytic results for that construct should not be interpreted as ",
            "measurement of a common cause. SIEVER does not estimate formative ",
            "structures; the gate exists so the assumption is examined rather than ",
            "silently made."
          )
        )
      ),
      nav_panel(
        "Common method variance",
        card(
          card_header("Harman's Single-Factor Screen"),
          DTOutput(ns("harman_tbl")),
          uiOutput(ns("harman_note"))
        ),
        card(
          card_header("Marker-Variable Adjustment of Observed Construct-Score Correlations (Lindell & Whitney, 2001)"),
          uiOutput(ns("marker_note")),
          DTOutput(ns("marker_tbl")),
          # The adjustment over larger values of r_M, which the researcher
          # can change (blank: r_M + .05 and r_M + .10).
          div(
            class = "d-flex align-items-center gap-2 mt-3 flex-wrap",
            numericInput(ns("marker_r2"), "Second r_M to read the adjustment at",
              value = NA, min = -0.99, max = 0.99, step = 0.01, width = "230px"
            ),
            numericInput(ns("marker_r3"), "Third r_M",
              value = NA, min = -0.99, max = 0.99, step = 0.01, width = "150px"
            )
          ),
          uiOutput(ns("marker_sens_note")),
          DTOutput(ns("marker_sens_tbl")),
          uiOutput(ns("marker_code"))
        ),
        card(
          card_header("Unmeasured latent method factor"),
          uiOutput(ns("ulmf_note")),
          DTOutput(ns("ulmf_tbl")),
          div(
            class = "text-muted small mt-2",
            "A single method factor, orthogonal to the substantive constructs, is added loading on every item. The proportion of item variance it captures and the change in fit index how much shared method variance is present; a small result under this particular model does not establish negligible method bias. Consider the identification assumptions, alternative method structures, uncertainty, and evidence from other methods."
          )
        )
      )
    ),
    p(
      class = "text-muted small mt-2",
      strong("Construct validity"), " asks whether the scale measures the construct it claims to ",
      "(Cronbach & Meehl, 1955): the construct should relate to what theory says it should and ",
      "not to what it should not. ", strong("Convergent"), " evidence is strong relations with ",
      "kindred measures; ", strong("discriminant"), " evidence is weak relations with distinct ones ",
      "(Campbell & Fiske, 1959). Above: inter-construct correlations and AVE beside each ",
      "construct's largest correlation with another, the HTMT ratio, criterion relations read against your own ",
      "stated predictions, a latent structural model, and common-method-variance diagnostics. ",
      "The conventional values that appear here are evidence to weigh; none of them declares a ",
      "scale valid."
    )
  )
}

#' Validity evidence module server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive req renderUI HTML div tags
#' @importFrom shiny eventReactive withProgress incProgress
#' @importFrom DT renderDT datatable formatRound formatStyle styleInterval styleEqual
mod_validity_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    # The tab's Estimator control: the study's setting (Ingest tab) unless
    # chosen otherwise here (sieve_estimator_wire()).
    estimator_choice <- sieve_estimator_wire(input, output, state)

    # The page's confidence level, read once: the level of the two-sided
    # Fisher z intervals of the criterion correlations, of the two-sided
    # intervals of the latent correlations, and of the one-sided HTMT upper
    # bound, and the level every header, note, tally and recorded call on
    # the page states. An empty input is the default level (0.95); a value
    # outside (0, 1) is refused with the reason, never replaced. The RMSEA
    # interval keeps its own level (sieve_default_rmsea_conf_level()) and
    # the AVE intervals theirs (the AVE confidence level input).
    page_level <- reactive({
      level <- sieve_finite_or(input$conf_level, sieve_default_conf_level())
      if (!(level > 0 && level < 1)) {
        stop(sprintf(
          paste0(
            "The page's confidence level is %s; it must be a number strictly between 0 and 1 ",
            "(0.95 for 95%% intervals), so no interval on this page is computed until it is corrected."
          ),
          format(level, digits = 15)
        ), call. = FALSE)
      }
      level
    })

    # The active item ledger and the construct -> item grouping.
    active_meta <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$meta)
      m <- state$meta
      m[m$status == "active", , drop = FALSE]
    })

    # The active item matrix, coerced to numeric.
    active_x <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items)
      am <- active_meta()
      items <- intersect(am$item, names(state$items))
      d <- state$items[, items, drop = FALSE]
      d[] <- lapply(d, function(v) suppressWarnings(as.numeric(v)))
      d
    })

    # A named list, construct label -> active item names with data.
    construct_items <- reactive({
      am <- active_meta()
      x <- active_x()
      cons <- unique(am$construct[sieve_construct_assigned(am$construct)])
      out <- lapply(cons, function(cn) intersect(am$item[am$construct == cn], names(x)))
      names(out) <- cons
      out[vapply(out, length, integer(1L)) > 0L]
    })

    # The correlated-factors CFA, fit once for AVE, the factor correlations
    # and the Bartlett scores, kept as a result record so its status can be
    # stated. It is fit on request only (fct_ondemand.R): the Fit the
    # measurement model button in the main panel states the estimated time,
    # the result is kept with the revision it was computed from, and the
    # gate says in one line when the data or the item pool changed since.
    measurement_plan <- function(what, kind) {
      req(state$meta)
      spec <- sieve_lavaan_spec(state$meta, estimator = estimator_choice())
      active <- state$meta[state$meta$status == "active" & sieve_construct_assigned(state$meta$construct), , drop = FALSE]
      p <- nrow(active)
      seconds <- sieve_demand_wait(kind, p, spec$estimator, missing = spec$missing)
      list(
        estimator = spec$estimator, missing = spec$missing, p = p, seconds = seconds,
        label = sieve_duration_label(seconds),
        progress = sprintf("%s: %s, %d items", what, spec$estimator, p),
        detail = "Fitting the model in lavaan.",
        sentence = sprintf(
          "Fitting the %s of %d items under %s is estimated to take %s",
          tolower(what), p, spec$estimator, sieve_duration_label(seconds)
        ),
        refused = sieve_wlsmv_guard(active$item, state$items,
          ordered = spec$ordered %||% character(0),
          estimator = spec$estimator
        )
      )
    }
    cfa_key <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      # Currency on the effective inputs: the included items,
      # without the facet column (this first-order model reads no facet).
      sieve_demand_key(state, list(estimator = estimator_choice()),
        items = state$meta$item[state$meta$status == "active"], facets = FALSE
      )
    })
    cfa_plan <- reactive(measurement_plan("Correlated-factors model", "cfa"))
    fit_measurement <- function(items, meta, estimator = "auto") {
      # The session's fit store: the identical model on the identical data,
      # fit on the CFA page (or elsewhere), is reused, not fit again.
      store <- shiny::isolate(state$fit_store)
      prepared <- sieve_sampling_apply(sieve_cfa_spec(meta,
        reserved = union(names(items), names(shiny::isolate(state$raw))), estimator = estimator), state, items)
      if (!is.null(prepared$reason)) stop(prepared$reason, call. = FALSE)
      run <- sieve_run_cfa(prepared$spec, prepared$data,
        store = store, page = "Validity", rows = shiny::isolate(sieve_case_index(state))[prepared$row_index])
      run$warnings <- unique(c(run$warnings, prepared$warnings))
      # Declared auxiliary variables left out because every analyzed item is
      # observed (sieve_auxiliary_apply()); the method line and the recorded
      # R say so.
      run$aux_notes <- prepared$notes
      run$row_flow <- prepared$row_flow
      run$input_rows <- prepared$row_index
      if (!is.null(run$fit)) {
        # Read once with the fit (see mod_cfa.R); a fit reused from another
        # page brings what that page read, and what is read here is kept
        # with the stored fit for the next page. No script name is recorded
        # here: this page writes the fit's call into the script when its
        # tables render, not at the fit.
        run$measures <- sieve_fit_measures_at(run$measures, run$fit, sieve_default_rmsea_conf_level())
        run$standardized <- run$standardized %||% sieve_capture(lavaan::standardizedSolution(run$fit))
        sieve_fit_store_keep(store, run)
        if (isTRUE(run$reused)) {
          reused <- sieve_reuse_text(run)
          sieve_log(
            state,
            sprintf("Validity: %s%s for the correlated-factors measurement model",
              tolower(substring(reused, 1L, 1L)), substring(reused, 2L)),
            sieve_reuse_code(run, fit_name = "validity_fit", model_name = "validity_model")
          )
        }
      }
      run
    }
    compute_cfa <- function(key, plan) {
      fit_measurement(shiny::isolate(state$items), shiny::isolate(state$meta),
        estimator = key$choices$estimator %||% "auto"
      )
    }
    demand <- sieve_demand_wire(
      input, session, state,
      id = "fit_go", what = "Correlated-factors model",
      key = cfa_key, plan = cfa_plan, compute = compute_cfa, kind = "cfa",
      noun = "The correlated-factors model", action = "Fit the measurement model",
      refit = "Update",
      # The identical fit made on another page is adopted without a click
      # (fct_fit_store.R).
      peek = function(key, plan) {
        items <- shiny::isolate(state$items)
        prepared <- sieve_sampling_apply(sieve_cfa_spec(shiny::isolate(state$meta),
          reserved = union(names(items), names(shiny::isolate(state$raw))),
          estimator = key$choices$estimator %||% "auto"), state, items)
        is.null(prepared$reason) && sieve_fit_store_has(shiny::isolate(state$fit_store),
          prepared$spec, prepared$data, rows = shiny::isolate(sieve_case_index(state))[prepared$row_index])
      }
    )
    cfa_run <- reactive({
      run <- demand$result()
      req(run)
      run
    })
    # The recorded R of the measurement model the page's results read, with
    # the sentence that says a declared auxiliary is not used, when it is not.
    validity_fit_code <- function() {
      run <- cfa_run()
      c(
        if (length(run$aux_notes)) paste("#", run$aux_notes),
        sieve_cfa_code(run$spec, fit_name = "validity_fit", model_name = "validity_model",
          conf_level = sieve_default_rmsea_conf_level())
      )
    }

    # Only a model with a solution feeds the tables: one that did not converge
    # or is not identified has no correlations, AVE, or scores to report.
    fit <- reactive({
      run <- cfa_run()
      if (sieve_has_solution(run)) run$fit else NULL
    })

    output$gate <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(sieve_gate(state))
      }
      # The page's one status line: when a result of this page is from before
      # an edit, the sentence that says which and what changed, with the one
      # Update that recomputes every one of them; otherwise the measurement
      # model's line (not fit yet, with its button and estimate; stopped with
      # an error; or current). Then what was established about the fitted
      # model (a fitted object is not a certificate).
      run <- demand$result()
      tagList(
        page_update$line() %||% tagList(demand$line(), page_update$note()),
        sieve_reuse_line(run, demand$status()),
        if (!is.null(run)) sieve_status_alert(run, what = "correlated-factors model")
      )
    })

    # The one-line method statement of the measurement model the tables
    # below come from (the latent correlations, AVE and the Bartlett
    # scores of this tab).
    output$method <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(NULL)
      }
      run <- demand$result()
      if (is.null(run) || !sieve_has_solution(run)) {
        return(NULL)
      }
      # The RMSEA interval at the RMSEA level (90% by default), not at the
      # page's confidence level, which sets its other intervals; measures
      # read at another level are read again.
      sieve_method_line(run$fit, "Correlated-factors CFA",
        measures = sieve_fit_measures_at(run$measures, run$fit, sieve_default_rmsea_conf_level()),
        notes = run$aux_notes)
    })

    # ---- Tab 1: construct correlations and AVE ----------------------------

    # The standardized solution of the fitted model, or why it could not be
    # read.
    std_solution <- reactive({
      f <- fit()
      req(f)
      cfa_run()$standardized %||% sieve_capture(lavaan::standardizedSolution(f))
    })
    # The standardized solution with its intervals at the page's level, for
    # the latent correlation table (the kept solution is at lavaan's 0.95).
    phi_solution <- reactive({
      f <- fit()
      req(f)
      level <- page_level()
      sieve_capture(lavaan::standardizedSolution(f, level = level))
    })
    not_fit_note <- function(what) {
      sieve_dt(
        data.frame(Note = sprintf(paste0(
          "The %s come from the correlated-factors model, which is not fit yet or has no ",
          "solution: press Fit the measurement model (above the tabs) and read its status."
        ), what)),
        rownames = FALSE, selection = "none", options = list(dom = "t")
      )
    }

    output$phi_tbl <- renderDT({
      checked <- sieve_capture(page_level())
      if (!checked$ok) {
        return(sieve_dt(
          data.frame(Note = checked$error),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      level <- checked$value
      read <- sieve_outcome(phi_solution())
      if (!read$ready) {
        return(not_fit_note("inter-construct correlations"))
      }
      solution <- if (read$ok) read$value else read
      if (!solution$ok) {
        return(sieve_dt(
          data.frame(Note = paste("The standardized solution could not be read:", solution$error)),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      ss <- solution$value
      latents <- lavaan::lavNames(fit(), "lv")
      phi <- ss[ss$op == "~~" & ss$lhs != ss$rhs & ss$lhs %in% latents & ss$rhs %in% latents, , drop = FALSE]
      if (!nrow(phi)) {
        return(sieve_dt(
          data.frame(Note = "Only one construct is active; no inter-construct correlations to report."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      df <- data.frame(
        Construct.1 = sieve_show_names(phi$lhs, fit()), Construct.2 = sieve_show_names(phi$rhs, fit()),
        r = phi$est.std, CI.lower = phi$ci.lower, CI.upper = phi$ci.upper,
        p = phi$pvalue, stringsAsFactors = FALSE
      )
      sieve_log(
        state,
        sprintf(
          "Validity: inter-construct latent correlations from the standardized CFA solution, with two-sided %s confidence intervals.",
          sieve_ci_percent(level)
        ),
        c(
          validity_fit_code(),
          sprintf("validity_ss <- lavaan::standardizedSolution(validity_fit, level = %s)", sieve_num_lit(level)),
          'validity_latents <- lavaan::lavNames(validity_fit, "lv")',
          'validity_ss[validity_ss$op == "~~" & validity_ss$lhs != validity_ss$rhs & validity_ss$lhs %in% validity_latents & validity_ss$rhs %in% validity_latents, ]'
        )
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 15),
        colnames = c("Construct 1", "Construct 2", "r", paste(sieve_ci_percent(level), c("CI lower", "CI upper")), "p")
      ) |>
        formatRound(c("r", "CI.lower", "CI.upper", "p"), 3)
    })

    # AVE per construct from DMAR::average_variance_extracted (the canonical
    # implementation), with sqrt(AVE) and the largest |r| with another
    # construct, and which construct that is, shown side by side: no
    # comparison column and no shading (SIEVER applies no cutoff in output).
    ave_df <- reactive({
      read <- std_solution()
      if (!read$ok) {
        stop(sprintf("The standardized solution could not be read: %s", read$error), call. = FALSE)
      }
      ss <- read$value
      f <- fit()
      req(f)
      computed <- sieve_capture(DMAR::average_variance_extracted(fit = f))
      if (!computed$ok) {
        stop(sprintf("DMAR::average_variance_extracted stopped: %s", computed$error), call. = FALSE)
      }
      av <- computed$value
      req(!is.null(av), nrow(av) > 0L)
      cons <- av$factor
      ave <- stats::setNames(av$ave, cons)
      sqrt_ave <- sqrt(ave)

      # Maximum absolute inter-construct correlation per construct.
      latents <- lavaan::lavNames(fit(), "lv")
      phi <- ss[ss$op == "~~" & ss$lhs != ss$rhs & ss$lhs %in% latents & ss$rhs %in% latents, , drop = FALSE]
      # The largest absolute correlation of each construct with another,
      # and the construct it is with, so its interval can be found in the
      # correlation table above.
      largest <- lapply(cons, function(cn) {
        rows <- phi[(phi$lhs == cn | phi$rhs == cn) & is.finite(phi$est.std), , drop = FALSE]
        if (!nrow(rows)) {
          return(list(r = NA_real_, with = NA_character_))
        }
        i <- which.max(abs(rows$est.std))
        list(r = abs(rows$est.std[i]), with = if (rows$lhs[i] == cn) rows$rhs[i] else rows$lhs[i])
      })
      max_r <- vapply(largest, function(x) x$r, numeric(1L))
      with <- vapply(largest, function(x) x$with, character(1L))
      data.frame(
        Construct = sieve_show_names(cons, f),
        AVE = unname(ave),
        sqrt.AVE = unname(sqrt_ave),
        Max.r.with.other = unname(max_r),
        With = ifelse(is.na(with), "n/a", sieve_show_names(with, f)),
        stringsAsFactors = FALSE
      )
    })

    output$ave_tbl <- renderDT({
      read <- sieve_outcome(ave_df())
      if (!read$ready) {
        return(not_fit_note("average variance extracted and each construct's largest correlation with another"))
      }
      if (!read$ok) {
        return(sieve_dt(
          data.frame(Note = paste("AVE could not be computed:", read$error)),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      df <- read$value
      sieve_log(
        state,
        "Validity: AVE per construct (mean squared standardized loading), sqrt(AVE), and each construct's largest |r| with another construct.",
        c(
          validity_fit_code(),
          "ave <- DMAR::average_variance_extracted(fit = validity_fit)",
          "# sqrt(ave) beside each construct's largest |phi| with another construct (Fornell & Larcker, 1981); no cutoff applied"
        )
      )
      dt <- sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 15),
        # The convention, named once with its source (SIEVER applies no
        # cutoff in output): the values are shown side by side, unshaded.
        caption = htmltools::tags$caption(
          class = "small text-muted", style = "caption-side: bottom; text-align: left;",
          paste0(
            "Fornell and Larcker (1981) compare the square root of each construct's AVE with its ",
            "largest absolute correlation with another construct; SIEVER applies no cutoff, and the ",
            "interval of each correlation is in the table above and that of each AVE is computed below."
          )
        ),
        colnames = c(
          "Construct",
          "AVE",
          "sqrt(AVE)",
          "Max |r| with other",
          "With"
        )
      ) |>
        formatRound(c("AVE", "sqrt.AVE", "Max.r.with.other"), 3)
      dt
    })

    # Optional intervals belong to this exact fitted measurement model and
    # request. Changing the data, model, estimator, design or settings makes
    # them stale; ordinary AVE remains available without resampling.
    ave_request <- reactive(list(model = cfa_key(), B = sieve_finite_or(input$ave_B, 1000L),
      level = sieve_finite_or(input$ave_level, .95), seed = sieve_seed_input(input$ave_seed),
      sampling = sieve_sampling_spec(state), engine = sieve_engine_identity()))
    output$ave_interval_plan <- renderUI({
      choices <- ave_request()
      request <- sieve_dmar_boot_request(choices$B, conf_level = choices$level)
      p(class = "small text-muted", if (!request$ok) request$message else sprintf(
        "Optional %s%% percentile intervals refit the current measurement model on %d independent-row resamples. At this level at least %d replications must be requested. This runs only on request and can take several minutes; no intervals are recomputed when settings change.",
        format(100 * choices$level), choices$B, request$required))
    })
    observeEvent(input$ave_level, {
      shiny::updateNumericInput(session, "ave_B", min = sieve_dmar_boot_required("percentile",
        sieve_finite_or(input$ave_level, .95)))
    }, ignoreInit = FALSE)
    ave_interval_store <- reactiveVal(list())
    ave_intervals <- eventReactive(input$ave_boot, {
      choices <- ave_request()
      stamp <- sieve_hash(choices)
      if (!identical(demand$status()$state, "current")) {
        return(list(error = "Fit the current measurement model before requesting AVE intervals.", stamp = stamp))
      }
      f <- fit()
      kept <- ave_interval_store()[[stamp]]
      if (!is.null(kept)) return(kept)
      request <- sieve_dmar_boot_request(choices$B, conf_level = choices$level)
      if (!request$ok) return(list(error = request$message, stamp = sieve_hash(choices)))
      problem <- sieve_sampling_problem(state, capability = "independent")
      if (!is.null(problem)) return(list(error = problem, stamp = sieve_hash(choices)))
      rng <- sieve_draw_seed(choices$seed)
      result <- withProgress(message = "Computing optional AVE intervals", value = 0,
        detail = "The fitted-model bootstrap reports its result when the requested resamples finish.",
        sieve_ave_intervals(f, B = choices$B, seed = rng$seed, conf_level = choices$level,
          sampling_unit = choices$sampling$unit))
      spec <- list(kind = "ave_intervals", measurement = cfa_run()$spec, B = choices$B,
        conf_level = choices$level, seed = rng$seed, sampling = choices$sampling)
      spec$key <- sieve_hash(spec)
      result$seed <- rng$seed
      result$stamp <- sieve_hash(choices)
      result$revision <- sieve_revision(state)
      if (!is.null(result$table)) {
        store <- ave_interval_store(); store[[stamp]] <- result; ave_interval_store(store)
      }
      sieve_register_analysis(state, list(spec = spec, revision = result$revision,
        estimates = list(table = result$table, counts = result$counts, conf_level = choices$level,
          fit_signature = result$fit_signature, seed = rng$seed),
        status = result$status, warnings = result$warnings), note = "AVE percentile intervals")
      sieve_log(state, sprintf("Validity: AVE percentile intervals, B = %d, seed = %d, confidence level %s.",
        choices$B, rng$seed, format(choices$level)), c(
        validity_fit_code(),
        sprintf("ave_intervals <- DMAR::average_variance_extracted(fit = validity_fit, ci_method = \"percentile\", B = %d, seed = %d, conf_level = %s)",
          choices$B, rng$seed, sieve_num_lit(choices$level))))
      result
    })
    ave_intervals_now <- reactive({
      read <- sieve_outcome(ave_intervals())
      if (!read$ready) return(NULL)
      if (!read$ok) return(list(error = read$error))
      result <- read$value
      if (!identical(result$stamp, sieve_hash(ave_request()))) {
        return(list(stale = "The data, measurement model, estimator, design or AVE interval settings changed. Compute AVE intervals again for the current request."))
      }
      result
    })
    ave_interval_status <- reactive({
      result <- ave_intervals_now()
      if (is.null(result)) return(NULL)
      list(state = if (!is.null(result$stale)) "stale" else
        if (!is.null(result$error) || result$status$code %in% c("failed", "unavailable")) "failed" else "current",
        sentence = result$stale %||% result$error %||% result$status$message)
    })
    observe({
      status <- ave_interval_status()
      if (is.null(status)) return()
      sieve_page_publish(state, "validity", session$ns("ave_intervals"), "AVE percentile intervals",
        status, "Compute AVE intervals")
    })
    output$ave_interval_note <- renderUI({
      result <- ave_intervals_now()
      if (is.null(result)) return(NULL)
      p(class = "small text-muted", result$stale %||% result$error %||% result$status$message,
        if (length(result$warnings)) paste(result$warnings, collapse = " "))
    })
    output$ave_interval_tbl <- renderDT({
      result <- ave_intervals_now()
      req(result, is.null(result$stale), is.null(result$error), result$table)
      t <- result$table
      sieve_dt(data.frame(Construct = sieve_show_names(t$factor, fit()), AVE = t$ave,
        Lower = t$ci_lower, Upper = t$ci_upper), rownames = FALSE, selection = "none",
        options = list(dom = "t")) |> formatRound(c("AVE", "Lower", "Upper"), 3)
    })
    output$ave_interval_code <- renderUI({
      result <- ave_intervals_now()
      if (is.null(result)) return(NULL)
      sieve_code_disclosure(state, "Validity: AVE percentile intervals", ns = session$ns,
        status = ave_interval_status(), title = "Show code: AVE intervals", readout = "ave_intervals")
    })

    # ---- Tab 2: HTMT (optional) -------------------------------------------

    output$htmt_note <- renderUI({
      if (!isTRUE(as.logical(input$htmt_on))) {
        return(div(
          class = "alert alert-info",
          "HTMT is off by default. Press Compute HTMT on this panel to compute the heterotrait-monotrait ratio of each construct pair. ",
          sieve_htmt_convention_text()
        ))
      }
      div(
        class = "text-muted small mb-2",
        "HTMT is computed from the item correlations (DMAR::htmt on each construct's items, pairwise ",
        "complete), not from construct scores: the operational score specification does not enter ",
        "it. ", sieve_htmt_convention_text()
      )
    })

    # HTMT per construct pair from DMAR::htmt (the canonical implementation;
    # Henseler, Ringle & Sarstedt, 2015), which also supplies the one-sided
    # bootstrap upper bound used for the discriminant test. The time of this
    # one evaluation is the measured rate the bootstrap's estimate rests on
    # (B evaluations, one per resample).
    htmt_seconds <- reactiveVal(NA_real_)
    htmt_key <- reactive(sieve_hash(list(data = active_x(), blocks = construct_items(),
      sampling = sieve_sampling_spec(state))))
    htmt_run <- eventReactive(input$htmt_on, {
      req(isTRUE(as.logical(input$htmt_on)))
      grp <- construct_items()
      req(length(grp) >= 2L)
      started <- proc.time()[["elapsed"]]
      computed <- sieve_capture(DMAR::htmt(active_x(), blocks = grp))
      htmt_seconds(proc.time()[["elapsed"]] - started)
      if (!computed$ok) {
        stop(sprintf("DMAR::htmt stopped: %s", computed$error), call. = FALSE)
      }
      list(table = computed$value, key = htmt_key())
    })
    htmt_pairs <- reactive({
      req(isTRUE(as.logical(input$htmt_on)))
      held <- htmt_run()
      if (!identical(held$key, htmt_key())) stop(
        "HTMT no longer matches the included items or data. Press Compute HTMT again.", call. = FALSE)
      held$table
    })

    # The bootstrap upper bound, computed by DMAR, shown beside each pair's
    # HTMT and read against no value (SIEVER applies no cutoff in output);
    # an upper bound well below 1 is evidence that the pair
    # separates.
    # Validate the entered request against the one-sided engine rule; its
    # count is never changed by the application.
    htmt_boot_choices <- reactive({
      asked <- sieve_finite_or(input$htmt_B, 1000L)
      list(
        B = asked,
        asked = asked,
        seed = sieve_seed_input(input$htmt_seed),
        level = page_level()
      )
    })
    observeEvent(input$conf_level, {
      # A level outside (0, 1) leaves the minimum as it was; the plan below
      # the HTMT controls says why no bootstrap can be requested.
      level <- tryCatch(page_level(), error = function(e) NULL)
      req(level)
      shiny::updateNumericInput(session, "htmt_B", min = sieve_dmar_boot_required("percentile",
        level, "one"))
    }, ignoreInit = FALSE)
    level_text <- function(level) sprintf("%s%%", sub("\\.?0+$", "", sprintf("%.4f", 100 * level)))
    # A completed request is kept under the identity of everything it rests
    # on (the data revision, the item blocks, B, the seed, the level, and
    # the sampling design), so a request that matches is reused, not rerun.
    htmt_store <- reactiveVal(list())
    htmt_request_key <- function(grp, choices) {
      sieve_hash(list(
        revision = sieve_revision(state), blocks = grp, B = choices$B,
        seed = choices$seed, level = choices$level, sampling = sieve_sampling_spec(state)
      ))
    }
    # The method, the level, the resampling unit, and the estimate, stated
    # before the run (the measured time of one HTMT evaluation times B).
    output$htmt_boot_plan <- renderUI({
      req(isTRUE(as.logical(input$htmt_on)))
      choices <- htmt_boot_choices()
      request <- sieve_dmar_boot_request(choices$B, conf_level = choices$level, sidedness = "one")
      problem <- sieve_sampling_problem(state, capability = "independent")
      measured <- sieve_outcome(htmt_pairs())
      per <- htmt_seconds()
      div(
        class = "small text-muted mb-2",
        sprintf(
          "Method: one-sided percentile bootstrap upper bound (DMAR::htmt) at the %s level, resampling independent rows%s. ",
          level_text(choices$level),
          if (!is.null(problem)) " (refused under the declared clustered or repeated design)" else ""
        ),
        if (is.finite(per)) {
          sprintf(
            paste0(
              "With B = %d the bootstrap is estimated to take %s (the measured time of one HTMT ",
              "evaluation on these items, %s, times the resamples); it runs as one call that cannot ",
              "be stopped part way. "
            ),
            choices$B, sieve_duration_label(per * choices$B), sieve_seconds_text(per)
          )
        } else {
          "The estimate is made from the measured time of the HTMT table's computation once it is shown. "
        },
        if (!request$ok) request$message else sprintf("At this level DMAR requires at least %d requested resamples. ", request$required),
        paste0(
          "The number of resamples sets the Monte Carlo precision of the bound, not what it ",
          "estimates; Hesterberg (2015) suggests 10,000 for routine use, and a request is never ",
          "lowered to shorten a wait. A request that matches a completed one (the same data, items, ",
          "resamples, seed, and level) is reused rather than run again."
        )
      )
    })
    htmt_ci <- eventReactive(input$htmt_boot, {
      problem <- sieve_sampling_problem(state, capability = "independent")
      if (!is.null(problem)) return(list(error = problem))
      grp <- construct_items()
      if (length(grp) < 2L) {
        return(list(error = "Need at least two constructs with two or more items each."))
      }
      # The rows the HTMT table uses: every row, with the correlations formed
      # from pairwise complete observations (DMAR::htmt), so the bootstrap's
      # HTMT is the table's and each resample treats missing values the same
      # way.
      x <- active_x()
      informative <- rowSums(!is.na(as.matrix(x))) > 0L
      x <- x[informative, , drop = FALSE]
      if (nrow(x) < 30L) {
        return(list(error = sprintf("Only %d rows with an observed item; too few to bootstrap reliably.", nrow(x))))
      }
      choices <- htmt_boot_choices()
      request <- sieve_dmar_boot_request(choices$B, conf_level = choices$level, sidedness = "one")
      if (!request$ok) return(list(error = request$message))
      level <- choices$level
      if (!(level > 0 && level < 1)) {
        return(list(error = "The confidence level must lie strictly between 0 and 1."))
      }
      sampling <- sieve_sampling_spec(state)
      B <- choices$B
      # The bootstrap belongs to the data and the ledger it was computed
      # from: the revision is kept with it, and the panels below show it
      # only while that is still what is loaded (sieve_stale_reason). Data
      # without an identity are given one first, as every kept result does.
      sieve_ensure_dataset(state)
      revision <- sieve_revision(state)
      key <- htmt_request_key(grp, choices)
      held <- htmt_store()[[key]]
      if (!is.null(held) && is.null(held$error)) {
        sieve_notify(sprintf(
          "The HTMT bootstrap for this request (the same data, items, %d resamples, seed %d, and %s level) finished at %s; the result shown is that run.",
          held$B, held$seed, level_text(held$level), held$when
        ))
        # The request's own entry (the number entered may differ while the
        # resamples, seed and level that identify it do not).
        held$choices <- choices
        held$reused <- TRUE
        return(held)
      }
      # The bootstrap always runs from a known seed, which DMAR sets locally
      # (restoring the session's stream): the supplied one, or one drawn from
      # the session stream when the input is empty, so the bounds can be
      # replayed from the recorded call.
      rng <- sieve_draw_seed(choices$seed)
      seed <- rng$seed
      per <- isolate(htmt_seconds())
      estimate <- if (is.finite(per)) per * B else NA_real_
      started <- proc.time()[["elapsed"]]
      computed <- withProgress(
        message = sprintf(
          "Bootstrapping HTMT (DMAR): B = %d%s", B,
          if (is.finite(estimate)) sprintf(", estimated %s", sieve_duration_label(estimate)) else ""
        ),
        detail = "One call to DMAR::htmt, which reports nothing until it returns.", value = 1,
        sieve_capture(DMAR::htmt(x, blocks = grp, B = B, conf_level = level, seed = seed))
      )
      seconds <- proc.time()[["elapsed"]] - started
      if (!computed$ok) {
        return(list(error = sprintf("HTMT could not be computed: %s", computed$error)))
      }
      h <- computed$value
      sieve_dmar_require_columns(h, c("construct_1", "construct_2", "htmt", "ci_upper"), "HTMT")
      counts <- sieve_dmar_boot_result(h, "HTMT")
      dropped <- counts$dropped
      used <- counts$used
      method <- sprintf(
        "one-sided percentile bootstrap upper bound (DMAR::htmt) at the %s level, resampling independent rows",
        level_text(level)
      )
      sieve_log(
        state,
        sprintf(
          "Validity: HTMT bootstrap upper bounds (%s), B = %d attempted and %d used, seed = %d%s, %s; no cutoff is applied to the bounds.",
          method, B, used, seed, if (rng$supplied) "" else " drawn for this run",
          sieve_seconds_text(seconds)
        ),
        c(
          "# Every row with an observed item; correlations from pairwise complete observations.",
          "htmt_rows <- active_items[rowSums(!is.na(active_items)) > 0, , drop = FALSE]",
          sprintf(
            "htmt_bounds <- DMAR::htmt(htmt_rows, blocks = construct_items, B = %d, conf_level = %s, seed = %d)",
            B, sieve_num_lit(level), seed
          ),
          sprintf("# each pair's HTMT with its one-sided %s upper bound (column ci_upper)", level_text(level))
        )
      )
      result <- list(
        table = h, B = B, seed = seed, seed_supplied = rng$supplied,
        revision = revision, choices = choices, sampling = sampling,
        dropped = dropped, used = used, attempted = B, rows = nrow(x), level = level,
        method = method, seconds = seconds, when = format(Sys.time(), "%H:%M"),
        key = key, reused = FALSE,
        warnings = computed$warnings, bootstrap_status = counts$status, B_required = counts$required
      )
      # The request and its counts, recorded in the analysis registry as the
      # Reliability tab records its bootstraps.
      spec <- list(
        kind = "htmt_bootstrap", blocks = grp, B = B, seed = seed, conf_level = level,
        rows = nrow(x), sampling = sampling
      )
      spec$key <- sieve_hash(spec)
      sieve_register_analysis(
        state,
        list(
          spec = spec, revision = revision,
          estimates = list(
            table = as.data.frame(h), method = method,
            counts = counts,
            conf_level = level, seed = seed, seed_supplied = rng$supplied, seconds = seconds
          ),
          status = list(code = if (all(is.finite(h$ci_upper))) "ok" else "partial",
            message = sprintf("%d of %d resamples used; engine status: %s.", used, B, counts$status)),
          warnings = result$warnings
        ),
        note = "HTMT bootstrap upper bounds"
      )
      store <- htmt_store()
      store[[key]] <- result
      htmt_store(store)
      result
    })

    # The bootstrap as it stands for the data now loaded: the kept result,
    # or the reason it no longer applies. The revision is read in isolation
    # by sieve_revision(), so the dependency on the dataset, the item data
    # and the ledger is taken here.
    htmt_ci_now <- reactive({
      read <- sieve_outcome(htmt_ci())
      r <- if (!read$ready) {
        NULL
      } else if (!read$ok) {
        list(error = sprintf("The HTMT bootstrap stopped with an error: %s", read$error))
      } else {
        read$value
      }
      state$dataset_id
      state$items
      state$meta
      state$sampling_design
      if (is.null(r) || !is.null(r$error)) {
        return(r)
      }
      why <- sieve_stale_reason(r$revision, sieve_revision(state))
      if (is.null(why) && !identical(r$sampling, sieve_sampling_spec(state))) {
        why <- "the sampling design changed; the earlier bootstrap used the recorded design"
      }
      # A page level that cannot be read is a changed request (its reason is
      # in the plan above the button), not an error of this reading.
      if (is.null(why) && !identical(r$choices, tryCatch(htmt_boot_choices(), error = function(e) NULL))) {
        why <- "the requested bootstrap replications or seed, or the confidence level, changed"
      }
      if (!is.null(why)) {
        r$stale <- why
      }
      r
    })

    observe({
      # The event has no value until requested. This reads its cached result;
      # only the explicit bootstrap button invalidates the computation.
      read <- sieve_outcome(htmt_ci())
      if (!read$ready || !read$ok || is.null(read$value)) return()
      r <- htmt_ci_now()
      phase <- if (!is.null(r$stale)) "stale" else if (!is.null(r$error)) "failed" else "current"
      sieve_page_publish(state, "validity", session$ns("htmt_ci"), "HTMT bootstrap interval",
        list(state = phase, sentence = r$stale %||% r$error, key = r$choices), "Compute HTMT CIs")
    })

    output$htmt_tbl <- renderDT({
      if (!isTRUE(as.logical(input$htmt_on))) {
        return(NULL)
      }
      read <- sieve_outcome(htmt_pairs())
      if (read$ready && !read$ok) {
        return(sieve_dt(
          data.frame(Note = read$error),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      h <- read$value
      if (is.null(h) || !nrow(h)) {
        return(sieve_dt(
          data.frame(Note = "HTMT requires at least two constructs that each have two or more active items."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      df <- data.frame(
        Pair = sprintf("%s - %s", h$construct_1, h$construct_2),
        HTMT = h$htmt, stringsAsFactors = FALSE
      )
      sieve_log(
        state,
        "Validity: HTMT per construct pair (DMAR::htmt); no cutoff is applied to the values.",
        "DMAR::htmt(active_items, blocks = construct_items)"
      )
      # The values without shading: no cutoff is applied (SIEVER applies no
      # cutoff in output); the convention is named
      # with its source in the note above the table.
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 15),
        colnames = c("Construct pair", "HTMT")
      ) |>
        formatRound("HTMT", 3)
    })

    output$htmt_ci_note <- renderUI({
      r <- htmt_ci_now()
      if (is.null(r)) {
        return(div(
          class = "text-muted small",
          "Press Compute HTMT CIs for a DMAR bootstrap upper bound per construct pair."
        ))
      }
      if (!is.null(r$error)) {
        return(div(class = "alert alert-danger", r$error))
      }
      if (!is.null(r$stale)) {
        return(div(
          class = "alert alert-warning",
          sprintf(
            paste0(
              "The HTMT bootstrap computed earlier (B = %d, seed = %d) is not shown: %s. ",
              "Press Compute HTMT CIs to compute it for the data now loaded."
            ),
            r$B, r$seed, r$stale
          )
        ))
      }
      div(
        class = "text-muted small",
        sprintf(
          paste0(
            "Method: %s; the %d rows with an observed item are resampled, and each correlation is formed ",
            "from the pairs observed in the resample, as the HTMT table forms it. B = %d resamples ",
            "attempted%s, %d of the %d resamples were used%s; seed = %d%s; %s. No cutoff is applied to ",
            "the bounds. The number of resamples sets the Monte Carlo precision of ",
            "the bound, not what it estimates (Hesterberg, 2015, suggests 10,000 for routine use)."
          ),
          r$method %||% "one-sided percentile bootstrap upper bound (DMAR::htmt)",
          r$rows %||% NA_integer_,
          r$B,
          if (!is.null(r$choices$asked) && !identical(r$choices$asked, r$B)) {
            sprintf(" (%s were requested)", format(r$choices$asked))
          } else {
            ""
          },
          r$used %||% r$B, r$B,
          if (isTRUE((r$dropped %||% 0L) > 0L)) {
            sprintf(
              paste0(
                "; %d left HTMT undefined for some pair (an average within-construct correlation that ",
                "was not positive) and were dropped, so the bound describes the resamples in which every ",
                "pair was defined"
              ),
              r$dropped
            )
          } else {
            ""
          },
          r$seed,
          if (isTRUE(r$seed_supplied)) "" else " (drawn for this run and recorded)",
          if (isTRUE(r$reused)) {
            sprintf("completed earlier at %s and reused for this matching request", r$when)
          } else {
            sprintf("it took %s", sieve_seconds_text(r$seconds %||% NA_real_))
          }
        )
      )
    })

    output$htmt_ci_tbl <- renderDT({
      r <- htmt_ci_now()
      req(r, is.null(r$error), is.null(r$stale), r$table)
      h <- r$table
      df <- sieve_htmt_bounds_table(h)
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 15),
        colnames = c("Construct pair", "HTMT", sprintf("One-sided %s upper bound", level_text(r$level)))
      ) |>
        formatRound(c("HTMT", "Upper"), 3)
    })

    # ---- Measurement direction (reflective vs formative) -------------------
    # Three questions per construct, after Bollen & Lennox (1991) and the
    # three Cs of Bollen & Bauldry (2011). Any formative-leaning answer turns
    # the construct's verdict into a warning; the answers are judgments the
    # researcher makes about theory, so nothing here reads the data.
    output$direction_ui <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      cons <- names(construct_items())
      req(length(cons) > 0)
      qs <- function(cn) {
        key <- gsub("[^A-Za-z0-9]", "_", cn)
        tags$fieldset(
          class = "border rounded p-2 mb-2",
          tags$legend(class = "small fw-bold w-auto px-1 mb-0", cn),
          div(
            class = "d-flex gap-4 flex-wrap",
            radioButtons(
              session$ns(paste0("dir_cause_", key)),
              "Causality runs from...",
              choices = c(
                "Construct to items (reflective)" = "reflective",
                "Items to construct (formative)" = "formative",
                "Unsure" = "unsure"
              ),
              selected = "reflective", inline = FALSE, width = "260px"
            ),
            radioButtons(
              session$ns(paste0("dir_interch_", key)),
              "Items are interchangeable samples of one domain?",
              choices = c("Yes" = "yes", "No" = "no", "Unsure" = "unsure"),
              selected = "yes", inline = FALSE, width = "260px"
            ),
            radioButtons(
              session$ns(paste0("dir_cor_", key)),
              "Theory expects the items to correlate?",
              choices = c("Yes" = "yes", "No" = "no", "Unsure" = "unsure"),
              selected = "yes", inline = FALSE, width = "260px"
            )
          )
        )
      }
      tagList(lapply(cons, qs))
    })

    output$direction_verdict <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      cons <- names(construct_items())
      req(length(cons) > 0)
      verdicts <- lapply(cons, function(cn) {
        key <- gsub("[^A-Za-z0-9]", "_", cn)
        cause <- input[[paste0("dir_cause_", key)]] %||% "reflective"
        interch <- input[[paste0("dir_interch_", key)]] %||% "yes"
        corr <- input[[paste0("dir_cor_", key)]] %||% "yes"
        formative <- identical(cause, "formative") || identical(interch, "no") ||
          identical(corr, "no")
        unsure <- !formative && (identical(cause, "unsure") ||
          identical(interch, "unsure") || identical(corr, "unsure"))
        list(construct = cn, formative = formative, unsure = unsure)
      })
      bad <- vapply(verdicts, function(v) v$formative, logical(1))
      unk <- vapply(verdicts, function(v) v$unsure, logical(1))
      sieve_log(
        state,
        sprintf(
          "Measurement direction review (Bollen & Lennox, 1991): %s.",
          if (any(bad)) paste("formative-leaning:", paste(cons[bad], collapse = ", "))
          else if (any(unk)) "some answers unsure" else "all constructs judged reflective"
        ),
        "# a theory judgment, recorded so the reflective assumption is explicit"
      )
      blocks <- list()
      if (any(bad)) {
        blocks <- c(blocks, list(div(
          class = "alert alert-warning",
          strong("Formative-leaning: "), paste(cons[bad], collapse = ", "), ". ",
          "For these constructs a reflective CFA misstates the measurement structure: ",
          "loadings, omega, and AVE presume a common cause the theory does not assert. ",
          "Consider a formative or composite specification outside this app (a MIMIC ",
          "or composite model; Bollen & Bauldry, 2011), or rethink the indicator set."
        )))
      }
      if (any(unk)) {
        blocks <- c(blocks, list(div(
          class = "alert alert-info",
          strong("Unsure: "), paste(cons[unk], collapse = ", "), ". ",
          "Settle the direction before leaning on the factor analytic results; the ",
          "three questions are about theory, not data."
        )))
      }
      if (!any(bad) && !any(unk)) {
        blocks <- list(div(
          class = "alert alert-success",
          "All constructs are judged reflective, which is the assumption every model ",
          "in this environment makes. The judgment is now recorded in the ",
          "reproducible code log."
        ))
      }
      do.call(tagList, blocks)
    })

    # ---- Tab 3: common method variance ------------------------------------

    # Harman's screen, under the run policy (sieve_runs_by_itself(),
    # sieve_restored_project()): a first result for the dataset is computed by
    # itself when its estimate (sieve_harman_wait(), a one-factor factanal of
    # the p by p matrix) is two seconds or less and the project was not
    # reopened; otherwise, and after an edit, it waits for Compute Harman's
    # screen. A computed screen is kept for its key, so showing the panel
    # again does not recompute it.
    harman_kept <- new.env(parent = emptyenv())
    harman_kept$datasets <- character(0)
    harman_request <- reactiveVal(NULL)
    observeEvent(input$harman_go, harman_request(harman_key()))
    harman_key <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      x <- active_x()
      req(ncol(x) >= 3L)
      list(
        dataset = state$dataset_id, data = sieve_data_key(x), items = names(x),
        rows = sieve_hash(state$case_index %||% seq_len(nrow(x)))
      )
    })
    harman_plan <- reactive({
      p <- length(harman_key()$items)
      seconds <- sieve_harman_wait(p)
      list(
        p = p, seconds = seconds, label = sieve_duration_label(seconds),
        standard_seconds = sieve_at_default_rates(sieve_harman_wait(p))
      )
    })
    compute_harman <- function(key, plan) {
      x <- active_x()
      started <- proc.time()[["elapsed"]]
      computed <- sieve_capture(DMAR::common_method_single_factor(data = x))
      seconds <- proc.time()[["elapsed"]] - started
      harman_kept$datasets <- unique(c(harman_kept$datasets, key$dataset %||% "(unnamed data)"))
      if (!computed$ok) {
        stop(sprintf("DMAR::common_method_single_factor stopped: %s", computed$error), call. = FALSE)
      }
      sieve_rate_note("harman", seconds, plan$p^3)
      h <- computed$value
      prop <- h$value[h$term == "variance_explained"]
      prop <- if (length(prop) == 1L) prop else NA_real_
      sieve_log(
        state,
        paste(
          "Validity: Harman's single-factor screen (DMAR::common_method_single_factor: a one-factor",
          "maximum likelihood fit of the item correlation matrix, the sum of the squared loadings over",
          "the number of items), on all active items."
        ),
        "DMAR::common_method_single_factor(data = active_items)"
      )
      list(
        key = key, when = format(Sys.time(), "%H:%M"), seconds = seconds, prop = prop,
        table = data.frame(
          # The value alone: no row reads it against the .50 convention
          # (SIEVER applies no cutoff in output); the note
          # under the table names the convention once.
          Statistic = c("Active items", "Proportion variance, factor 1"),
          Value = c(
            as.character(ncol(x)),
            if (is.finite(prop)) sprintf("%.3f", prop) else "n/a"
          ),
          stringsAsFactors = FALSE
        )
      )
    }
    harman_run <- reactive({
      key <- harman_key()
      held <- harman_kept$held
      if (!is.null(held) && identical(held$key, key)) {
        return(held)
      }
      plan <- harman_plan()
      first <- !(key$dataset %||% "(unnamed data)") %in% harman_kept$datasets
      restored <- sieve_restored_project(state)
      if (identical(harman_request(), key) ||
        (first && !restored && sieve_runs_by_itself(plan$seconds, standard = plan$standard_seconds))) {
        harman_kept$held <- compute_harman(key, plan)
        return(harman_kept$held)
      }
      list(
        status = "not run", key = key, plan = plan, stale = !is.null(held),
        restored = first && restored,
        reason = if (!is.null(held)) {
          sprintf(
            "The included items, their values or the respondents changed since Harman's screen was computed (at %s); its earlier value is from before and is not shown.",
            held$when
          )
        } else if (first && restored) {
          sieve_restored_sentence("Compute Harman's screen")
        } else {
          sprintf("Harman's screen has not been computed yet (estimated at %s).", plan$label)
        }
      )
    })
    # The table: the screen when it is computed, else one row saying so.
    harman_df <- reactive({
      r <- harman_run()
      if (identical(r$status, "not run")) {
        return(data.frame(Statistic = "Harman's single-factor screen", Value = r$reason, stringsAsFactors = FALSE))
      }
      r$table
    })

    output$harman_note <- renderUI({
      r <- tryCatch(harman_run(), error = function(e) NULL)
      control <- if (!is.null(r) && identical(r$status, "not run")) {
        div(
          class = "alert alert-info small",
          strong(r$reason), " ",
          div(class = "mt-2", actionButton(session$ns("harman_go"),
            sprintf("Compute Harman's screen (%s)", r$plan$label), class = "btn-primary btn-sm"
          ))
        )
      }
      tagList(
        control,
        div(
          class = "text-muted small",
          tags$p(
            "Harman's single-factor screen, as DMAR computes it (DMAR::common_method_single_factor):",
            " one common factor is fit by maximum likelihood (stats::factanal) to the Pearson",
            " correlation matrix of the active items (pairwise complete cases), and the statistic",
            " is the sum of the squared loadings over the number of items, the share of the total",
            " item variance that one factor accounts for. Podsakoff, MacKenzie, Lee and Podsakoff",
            " (2003) describe the convention of taking one factor that accounts for more than half",
            " of the variance as a sign of method variance; SIEVER applies no cutoff to the value.",
            " The screen is a weak diagnostic that cannot by itself",
            " rule method variance in or out (a single substantive factor also passes it, and",
            " method variance spread over several factors escapes it); it is reported only as a",
            " starting point beside the marker and method-factor models below."
          )
        )
      )
    })

    output$harman_tbl <- renderDT({
      read <- sieve_outcome(harman_df())
      if (!read$ready) {
        return(sieve_dt(
          data.frame(Note = "Harman's screen needs DMAR and at least three active items."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      if (!read$ok) {
        return(sieve_dt(
          data.frame(Note = paste("Harman's screen could not be computed:", read$error)),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      sieve_dt(read$value,
        rownames = FALSE, selection = "none",
        options = list(dom = "t")
      )
    })

    # An orthogonal method factor loading on every active item, added to the
    # substantive measurement model. A small result under this specification
    # does not establish negligible method bias. The two
    # models (with and without the method factor) are fit on request from
    # the Fit the method-factor model button; the substantive model is the
    # one the Fit the measurement model button fits when that fit is
    # current, and is fit here otherwise.
    ulmf_key <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      sieve_demand_key(state, list(estimator = estimator_choice()))
    })
    ulmf_plan <- reactive({
      p <- measurement_plan("Method-factor model", "method_factor")
      current <- identical(demand$status()$state, "current")
      if (!current) {
        both <- measurement_plan("Correlated-factors model", "cfa")
        p$seconds <- p$seconds + both$seconds
        p$label <- sieve_duration_label(p$seconds)
        p$sentence <- sprintf(
          paste0(
            "Fitting the method-factor model of %d items under %s and ",
            "the correlated-factors model it is compared with is estimated to take %s"
          ),
          p$p, p$estimator, p$label
        )
      }
      p$detail <- "Fitting the measurement model with an orthogonal method factor in lavaan, for the comparison."
      p
    })
    compute_ulmf <- function(key, plan) {
      items <- shiny::isolate(state$items)
      meta <- shiny::isolate(state$meta)
      held <- shiny::isolate(demand$status())
      substantive <- if (identical(held$state, "current")) {
        held$value
      } else {
        fit_measurement(items, meta, estimator = key$choices$estimator %||% "auto")
      }
      method <- sieve_run_method_factor(items, meta, estimator = key$choices$estimator %||% "auto")
      # The comparison reads the fit measures of both models, for which
      # lavaan fits the baseline models; made once, here, under the
      # progress bar, so the panel does not pay it again after a label edit.
      comparison <- if (sieve_has_solution(method) && sieve_has_solution(substantive)) {
        sieve_capture(sieve_method_factor_table(substantive$fit, method$fit))
      }
      list(
        method = method, substantive = substantive, comparison = comparison,
        error = method$error
      )
    }
    demand_ulmf <- sieve_demand_wire(
      input, session, state,
      id = "ulmf_go", what = "Method-factor model",
      key = ulmf_key, plan = ulmf_plan, compute = compute_ulmf, kind = "method_factor",
      noun = "The method-factor model", action = "Fit the method-factor model",
      refit = "Update"
    )
    ulmf_run <- reactive({
      r <- demand_ulmf$result()
      req(r)
      r$method
    })

    ulmf_fit <- reactive({
      run <- ulmf_run()
      if (sieve_has_solution(run)) run$fit else NULL
    })

    # A result's own line in its panel: not fit yet, current, or failed, with
    # its button; from before, a pointer to the page's one Update in the
    # status line, which fits it again.
    panel_line <- function(d, noun) {
      st <- d$status()
      if (identical(st$state, "stale")) {
        return(div(
          class = "small text-muted mb-2",
          sprintf(
            "%s below is from before (%s at %s); Update, in the line at the top of the page, fits it again.",
            noun, st$progress %||% noun, st$when %||% "an earlier time"
          )
        ))
      }
      d$line()
    }

    output$ulmf_note <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(NULL)
      }
      r <- demand_ulmf$result()
      # What the method factor is, and how to read it, is below the table.
      tagList(
        panel_line(demand_ulmf, "The method-factor model"),
        if (!is.null(r)) sieve_status_alert(r$method, what = "method-factor model")
      )
    })

    output$ulmf_tbl <- renderDT({
      r <- demand_ulmf$result()
      mf <- if (!is.null(r) && sieve_has_solution(r$method)) r$method$fit else NULL
      sub <- if (!is.null(r) && sieve_has_solution(r$substantive)) r$substantive$fit else NULL
      if (is.null(r)) {
        return(sieve_dt(
          data.frame(Note = "The method-factor model is not fit yet: press Fit the method-factor model above."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      if (is.null(mf) || is.null(sub)) {
        return(sieve_dt(
          data.frame(Note = paste(
            "The comparison needs both models to have a solution.",
            if (is.null(mf)) paste("Method-factor model:", r$method$status$message),
            if (is.null(sub)) paste("Correlated-factors model:", r$substantive$status$message)
          )),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      # The comparison is made in a plain function: the two models are set
      # side by side within one family of fit indices (the first of robust,
      # scaled, standard that both report), so each change in CFI or RMSEA is
      # a difference of two values of the same statistic. It returns no table
      # when either model has no solution or no family is common to both.
      made <- r$comparison %||% sieve_capture(sieve_method_factor_table(sub, mf))
      if (!made$ok) {
        return(sieve_dt(
          data.frame(Note = paste("The comparison could not be made:", made$error)),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      compared <- made$value
      df <- compared$table
      if (is.null(df)) {
        return(sieve_dt(
          data.frame(Note = paste(
            "The method-factor comparison could not be computed for this model.",
            if (is.na(compared$reason)) "" else compared$reason
          )),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      sieve_log(
        state,
        "Validity (CMV): unmeasured latent method factor (orthogonal, on all items) vs the substantive model.",
        c(
          "# Method =~ all items; Method ~~ 0*each construct",
          "# compare CFI/RMSEA and inspect the method loadings"
        )
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t")
      ) |>
        formatRound("Value", 3)
    })

    # ---- External criterion variables and construct scores ----------------

    # The columns offered as criteria or as the marker: the variables whose
    # role (Ingest tab, Variable roles panel) is criterion or auxiliary and
    # whose values read as numbers (see sieve_numeric_columns(): a column
    # stored as text that holds numerals is read, one with entries that are
    # not numbers is refused by name with the reason) with at least two
    # distinct values, so that a correlation with them is defined. An
    # identifier is never offered (it numbers the rows), and a group is a
    # grouping variable for the multiple group analyses, not a criterion.
    # The roles table is read here, not through sieve_role_vars(), so that
    # a role changed on the Ingest tab reaches the selectors at once.
    aux_read <- reactive({
      req(state$raw)
      roles <- sieve_roles_table(state$raw, state$items, state$roles)
      req(roles)
      vars <- roles$variable[roles$role %in% c("criterion", "auxiliary")]
      vars <- intersect(vars, names(state$raw))
      if (!length(vars)) {
        return(list(offered = character(0), refused = character(0)))
      }
      read <- sieve_numeric_columns(state$raw, vars, what = "Criterion")
      refused <- read$text
      names(refused) <- read$problems$column
      few <- vapply(vars, function(v) {
        x <- read$data[[v]]
        !(v %in% read$problems$column) && length(unique(x[!is.na(x)])) < 2L
      }, logical(1))
      if (any(few)) {
        more <- sprintf(
          "Criterion '%s' has fewer than two distinct values, so no correlation with it is defined.",
          vars[few]
        )
        names(more) <- vars[few]
        refused <- c(refused, more)
      }
      list(offered = setdiff(vars, names(refused)), refused = refused, data = read$data)
    })
    aux_numeric <- reactive(aux_read()$offered)
    aux_refused <- reactive(aux_read()$refused)

    observe({
      vars <- aux_numeric()
      # The criteria and the marker in force are kept while they are
      # offered: an edit elsewhere (a reverse scoring re-reads the columns)
      # does not clear them, so the structural model on screen is still the
      # one its Update fits again.
      updateSelectInput(session, "criteria", choices = vars,
        selected = sieve_keep_choice(isolate(input$criteria), vars, several = TRUE)
      )
      updateSelectInput(session, "marker", choices = c("(none)" = "", vars),
        selected = sieve_keep_choice(isolate(input$marker), vars, "")
      )
    })

    # ---- The operational score of each construct ---------------------------
    # One score specification per construct (its items, the score, and the
    # minimum answered rule; sieve_score_specification()) governs the
    # criterion correlations and the marker adjustment. The sidebar control
    # sets the score of every construct at once, and follows the
    # specification when it is set elsewhere (the Reliability tab's card, a
    # reopened project).
    observeEvent(input$score_method, ignoreInit = TRUE, {
      method <- input$score_method
      req(method %in% names(sieve_score_methods()))
      spec <- sieve_score_specification(state)
      changed <- character(0)
      for (s in spec) {
        if (!identical(s$method, method)) {
          sieve_set_score_specification(state, s$construct, method = method)
          changed <- c(changed, s$construct)
        }
      }
      if (length(changed)) {
        sieve_log(
          state,
          sprintf(
            "Validity: the operational score of %s set to the %s (the minimum answered rule of each is kept).",
            paste(changed, collapse = ", "), unname(sieve_score_methods()[method])
          ),
          sprintf(
            "scoring_policies[[%s]]$method <- %s",
            encodeString(changed, quote = '"'), encodeString(method, quote = '"')
          )
        )
      }
    })
    # The control shows what is in force: the one score every construct
    # shares, or "As specified per construct" when they differ or share a
    # score the control does not offer (the prorated sum), with the scores
    # named under it; it does not keep its last value under a mixed
    # specification.
    observe({
      state$scoring_policies
      state$meta
      spec <- sieve_score_specification(state)
      methods <- unique(vapply(spec, `[[`, character(1), "method"))
      shown <- if (length(methods) == 1L && methods %in% c("mean", "bartlett", "regression")) methods else "mixed"
      if (length(spec) && !identical(isolate(input$score_method), shown)) {
        updateRadioButtons(session, "score_method", selected = shown)
      }
    })
    output$score_method_state <- renderUI({
      state$scoring_policies
      state$meta
      spec <- sieve_score_specification(state)
      methods <- unique(vapply(spec, `[[`, character(1), "method"))
      if (!length(spec) || (length(methods) == 1L && methods %in% c("mean", "bartlett", "regression"))) {
        return(NULL)
      }
      p(
        class = "small text-muted",
        "In force, construct by construct: ",
        sieve_score_policy_sentence(spec, isolate(state$scoring_policies)),
        " Choosing a score above sets it for every construct."
      )
    })
    score_spec <- reactive({
      am <- active_meta()
      x <- active_x()
      state$scoring_policies
      sieve_score_spec_build(am, isolate(state$scoring_policies) %||% list(), available = names(x))
    })

    # One score per construct per row of state$raw (rows align with
    # state$items), as specified: a composite through
    # sieve_composite_score(), or a factor score read from the fitted
    # measurement model with the construct's minimum answered rule applied
    # to it. A factor score that cannot be formed (the model is not fit, is
    # from before a change, or the scoring engine failed) leaves the scores
    # unavailable, with the reason and the way forward; no mean score is
    # substituted in any branch.
    construct_scores <- reactive({
      spec <- score_spec()
      x <- active_x()
      req(length(spec) > 0)
      latent <- Filter(function(s) isTRUE(s$latent), spec)
      f <- NULL
      scored_by <- list()
      if (length(latent)) {
        what <- paste(unique(vapply(latent, `[[`, character(1), "label")), collapse = " and ")
        named <- paste(names(latent), collapse = ", ")
        st <- demand$status()
        if (identical(st$state, "stale")) {
          return(list(
            scores = NULL, spec = spec, unavailable = "stale",
            method = sprintf("unavailable: the measurement model behind the %s of %s is from before", what, named),
            error = sprintf(
              paste0(
                "The measurement model behind the %s of %s is from before the last change (%s): press Update, ",
                "in the line at the top of the page, to fit it again before the scores are correlated with the ",
                "current criterion data."
              ),
              what, named, st$sentence %||% "the inputs changed"
            )
          ))
        }
        run <- demand$result()
        f <- if (!is.null(run) && sieve_has_solution(run)) run$fit else NULL
        if (is.null(f)) {
          return(list(
            scores = NULL, spec = spec, unavailable = "not_fit",
            method = sprintf("unavailable: the measurement model behind the %s of %s is not fit", what, named),
            error = sprintf(
              "The %s of %s %s read from the correlated-factors measurement model, which is not fit%s.",
              what, named, if (length(latent) == 1L) "is" else "are",
              if (!is.null(run)) " (it stopped without a solution; its status is in the line at the top of the page)" else ""
            )
          ))
        }
        for (m in unique(vapply(latent, `[[`, character(1), "method"))) {
          engine <- if (identical(m, "bartlett")) "Bartlett" else "regression"
          scored <- sieve_capture(sieve_factor_scores(f, method = engine))
          if (!scored$ok) {
            return(list(
              scores = NULL, spec = spec, unavailable = "engine",
              method = sprintf("unavailable: the scoring engine stopped for the %s", what),
              error = sprintf("The scoring engine stopped for the %s of %s: %s", what, named, scored$error)
            ))
          }
          fs <- scored$value
          if (is.null(fs) || nrow(fs) != nrow(x)) {
            return(list(
              scores = NULL, spec = spec, unavailable = "engine",
              method = sprintf("unavailable: the %s could not be formed", what),
              error = if (is.null(fs)) {
                sprintf("The scoring engine returned no %s of %s from the fitted model.", what, named)
              } else {
                sprintf(
                  "The %s of %s do not align with the rows of the data (%d scores for %d rows).",
                  what, named, nrow(fs), nrow(x)
                )
              }
            ))
          }
          # The score columns carry the model's latent names; match them to
          # the constructs through the fit's name map.
          names(fs) <- sieve_show_names(names(fs), f)
          scored_by[[m]] <- list(scores = fs, label = sieve_score_method(f, engine)$label)
        }
      }
      scores <- list()
      eligible <- list()
      labels <- character(0)
      for (s in spec) {
        cn <- s$construct
        if (!isTRUE(s$latent)) {
          r <- sieve_composite_score(x, s$items, s$weights, s$method, s$min_count, s$min_prop)
          scores[[cn]] <- r$scores
          eligible[[cn]] <- sum(r$eligible)
          labels[cn] <- s$label
          next
        }
        fs <- scored_by[[s$method]]$scores
        if (!cn %in% names(fs)) {
          return(list(
            scores = NULL, spec = spec, unavailable = "engine",
            method = sprintf("unavailable: no %s for %s", s$label, cn),
            error = sprintf(
              "The fitted measurement model has no factor for %s, so its %s cannot be read from it (a construct needs two or more included items to be modeled).",
              cn, s$label
            )
          ))
        }
        v <- fs[[cn]]
        answered <- rowSums(!is.na(x[, s$items, drop = FALSE]))
        v[answered < s$required] <- NA_real_
        scores[[cn]] <- v
        eligible[[cn]] <- sum(is.finite(v))
        labels[cn] <- scored_by[[s$method]]$label
      }
      df <- as.data.frame(scores, check.names = FALSE, optional = TRUE)
      names(df) <- names(scores)
      # The one-line label of the scores in force: one score for every
      # construct, or each construct's own.
      method <- if (length(unique(labels)) == 1L) {
        unique(labels)
      } else {
        sprintf("as specified per construct (%s)", paste(sprintf("%s: %s", names(labels), labels), collapse = "; "))
      }
      list(scores = df, method = method, spec = spec, eligible = eligible, labels = labels, fit = f)
    })

    # The explicit choice when a factor score is unavailable: the constructs
    # specified as factor scores are set to the mean of their answered items
    # (their minimum answered rule is kept), and the change is recorded.
    observeEvent(input$scores_use_mean, {
      spec <- sieve_score_specification(state)
      changed <- character(0)
      for (s in spec) {
        if (isTRUE(s$latent)) {
          sieve_set_score_specification(state, s$construct, method = "mean")
          changed <- c(changed, s$construct)
        }
      }
      req(length(changed) > 0)
      sieve_log(
        state,
        sprintf(
          "Validity: mean scores chosen for %s in place of the factor scores specified (the measurement model was not available).",
          paste(changed, collapse = ", ")
        ),
        sprintf("scoring_policies[[%s]]$method <- \"mean\"", encodeString(changed, quote = '"'))
      )
      sieve_notify(sprintf(
        "%s now scored as the mean of the answered items; the minimum answered rule of each is kept.",
        paste(changed, collapse = ", ")
      ))
    })

    # The specification in force, one line per construct with its eligible n.
    scores_in_force <- function(sc) {
      lines <- vapply(sc$spec, function(s) {
        n <- sc$eligible[[s$construct]]
        sprintf(
          "%s: %s, at least %d of %d items answered; n eligible = %d",
          s$construct, sc$labels[[s$construct]] %||% s$label, s$required, length(s$items),
          if (is.null(n)) NA_integer_ else as.integer(n)
        )
      }, character(1))
      tags$ul(class = "mb-1", lapply(lines, tags$li))
    }

    # ---- Tab: criterion validity ------------------------------------------

    output$crit_note <- renderUI({
      # The note follows the sampling design as the table does (crit_df()).
      state$sampling_design
      vars <- aux_numeric()
      refused <- aux_refused()
      refused_note <- if (length(refused)) {
        div(
          class = "alert alert-warning small",
          strong("Not offered as a criterion: "), paste(refused, collapse = " ")
        )
      }
      if (!length(vars)) {
        return(tagList(
          refused_note,
          div(
            class = "alert alert-info",
            paste0(
              "No variable is available as a criterion: a criterion is a column whose values ",
              "are numbers (stored as numbers, or as numerals in text) with the role criterion ",
              "or auxiliary on the Ingest tab's Variable roles panel (identifiers and groups ",
              "are not offered). Give an external variable that role, or use the bundled ",
              "example's age in years."
            )
          )
        ))
      }
      if (!length(input$criteria)) {
        return(tagList(
          refused_note,
          div(
            class = "alert alert-info",
            "Select one or more criterion variables in the sidebar to estimate criterion related validity."
          )
        ))
      }
      read <- sieve_outcome(construct_scores())
      sc <- if (read$ready && read$ok) read$value else NULL
      if (!is.null(sc) && !is.null(sc$error)) {
        # The scores are unavailable: the reason, the action that makes them
        # available (the measurement model's own button, or the page's
        # Update), and the explicit choice of mean scores instead. Nothing
        # is correlated until one of them is taken.
        return(tagList(
          refused_note,
          div(
            class = "alert alert-warning small",
            strong("The construct scores are unavailable. "), sc$error,
            " Nothing is correlated until the model is fit or mean scores are chosen; no mean score is substituted on its own.",
            div(
              class = "mt-2 d-flex gap-2 flex-wrap align-items-center",
              if (identical(sc$unavailable, "not_fit")) demand$button("Fit the measurement model", input_id = "scores_fit_go"),
              actionButton(session$ns("scores_use_mean"), "Use mean scores instead", class = "btn-outline-secondary")
            )
          )
        ))
      }
      # The one-line method statement, the specification in force with the
      # eligible n of each construct, and the one instruction the controls
      # need; how to read the table is below it.
      div(
        class = "text-muted small mb-2",
        refused_note,
        if (read$ready && !read$ok) {
          div(class = "alert alert-danger small", "The construct scores could not be formed: ", read$error)
        },
        # The note reads the design check the table reads, so it never
        # promises an interval the table withholds.
        if (is.null(sieve_sampling_problem(state, capability = "independent"))) {
          sprintf(paste0(
            "Each row is the Pearson correlation between a construct score (%s) and a criterion, ",
            "with its two-sided %s Fisher z confidence interval (the page's confidence level), from ",
            "the cases that have an eligible score and the criterion (n in the table). "
          ), sc$method %||% "mean of the answered items", sieve_ci_percent(page_level()))
        } else {
          sprintf(paste0(
            "Each row is the descriptive Pearson correlation between a construct score (%s) and a ",
            "criterion, from the cases that have an eligible score and the criterion (n in the table). ",
            "The declared %s sampling unit makes the rows dependent, and the Fisher z interval and ",
            "its p value assume independent rows, so neither is given. "
          ), sc$method %||% "mean of the answered items", sieve_sampling_spec(state)$unit)
        },
        if (!is.null(sc)) {
          tagList(
            "Scores in force (Reliability tab, Operational scores), with the respondents eligible under each minimum answered rule:",
            scores_in_force(sc)
          )
        },
        strong("State your predictions, then read the estimates against them"),
        " (how, and why, below the table)."
      )
    })

    # How to read the criterion table, below it: the first paragraph
    # visible, the rest in a labeled disclosure closed by default.
    output$crit_more <- renderUI({
      req(length(input$criteria) > 0, length(aux_numeric()) > 0)
      sieve_more_below(
        tagList(
          "These are correlations of observed scores: measurement error in the score and in the ",
          "criterion pulls them toward zero as a rule, which makes a directional interval more likely to ",
          "include zero, and a discriminant interval more likely to fall inside a bound, than the ",
          "latent relation warrants. The Structural model tab estimates the relation at the ",
          "latent level."
        ),
        tags$p(
          strong("State your predictions, then read the estimates against them. "),
          "A correlation is evidence about validity only in relation to what theory said it ",
          "should be (Cronbach & Meehl, 1955; Campbell & Fiske, 1959), so a prediction chosen ",
          "after seeing the estimate is not a test of it. For each pair choose one of three: ",
          em("no prediction"), " (the estimate is reported and not scored; leaving a pair blank ",
          "is not a prediction that it is small), a ", em("direction"),
          " (positive or negative, optionally with a smallest magnitude you would expect), or ",
          em("small"), " (a discriminant prediction, with your own bound on the absolute ",
          "correlation). The table then says where each interval lies in relation to your ",
          "prediction. It does not say that a prediction is supported: an interval is evidence, ",
          "and how much it bears on validity depends on how risky the prediction was. The values ",
          ".10, .30, and .50 are Cohen's (1988) conventional small, medium, and large ",
          "correlations; the .30 shown as a starting bound is such a convention, not a rule, and ",
          "should be replaced by the largest correlation your theory would regard as negligible."
        ),
        tags$p(
          "Your predictions are kept for the session by construct and criterion name. They stay ",
          "as you stated them when you change the selected criteria or the kind of score, they ",
          "return when a criterion is selected again, and each change is written to the ",
          "reproducible code."
        ),
        label = "More on reading the criterion table: predictions, and how they are kept"
      )
    })

    # Input identifiers for one construct and criterion pair. They are built
    # from the names, not from positions, so a stated prediction stays with
    # its pair when the selected criteria change.
    exp_id <- function(what, criterion, construct) {
      paste("exp", what, sieve_input_key(criterion), sieve_input_key(construct), sep = "__")
    }

    # The constructs that have a score, in the order of the table.
    pair_constructs <- reactive(names(construct_scores()$scores))

    # The stated predictions are kept in the shared state (state$predictions,
    # one row per construct and criterion), not in the controls. The
    # controls below are rebuilt whenever the selected criteria, the kind of
    # score, or the constructs change, and a rebuilt control holds only what
    # it is given: built with fixed starting values, it would reset every
    # prediction on the page to "No prediction" in the browser each time a
    # criterion is added. Each control is therefore drawn from the store, and
    # the store is written from the controls.
    #
    # This observer is the only writer. It reads the controls of the pairs
    # on the page; a control the page has not reported yet is NULL and
    # writes nothing, while a magnitude the researcher cleared arrives as NA
    # and is stored as missing.
    observe(
      {
        crits <- input$criteria
        req(length(crits) > 0)
        cons <- pair_constructs()
        held <- isolate(state$predictions)
        new <- held
        for (cr in crits) {
          for (cn in cons) {
            choice <- input[[exp_id("kind", cr, cn)]]
            low <- input[[exp_id("min", cr, cn)]]
            bound <- input[[exp_id("bound", cr, cn)]]
            # Anything but one of the four choices is not a statement.
            if (length(choice) != 1L || !choice %in% sieve_prediction_choices()) {
              choice <- NULL
            }
            if (is.null(choice) && is.null(low) && is.null(bound)) {
              next
            }
            new <- sieve_set_prediction(new, cn, cr, choice = choice, min_abs = low, bound = bound)
          }
        }
        if (!identical(new, held)) {
          state$predictions <- new
        }
      },
      priority = 10
    )

    # One row of controls per construct and criterion pair: the kind of
    # prediction, and the magnitude that belongs to that kind. A pair starts
    # with no prediction, and a pair with a stored prediction is drawn with
    # it. The store is read in isolation: writing to it (which every change
    # of a control does) must not rebuild the controls under the researcher's
    # hands.
    output$expectations <- renderUI({
      crits <- input$criteria
      req(length(crits) > 0)
      cons <- pair_constructs()
      held <- isolate(state$predictions)
      rows <- lapply(crits, function(cr) {
        tags$fieldset(
          class = "border rounded p-2 mb-2",
          tags$legend(
            class = "small fw-bold w-auto px-1 mb-0",
            sprintf("Predictions for %s", cr)
          ),
          lapply(cons, function(cn) {
            kind_id <- exp_id("kind", cr, cn)
            now <- sieve_get_prediction(held, cn, cr)
            div(
              class = "d-flex align-items-end gap-3 flex-wrap",
              selectInput(
                session$ns(kind_id), sprintf("%s with %s", cn, cr),
                choices = c(
                  "No prediction" = "unspecified",
                  "Positive relation predicted" = "positive",
                  "Negative relation predicted" = "negative",
                  "Small relation predicted (discriminant)" = "discriminant"
                ),
                selected = now$choice, width = "300px"
              ),
              conditionalPanel(
                condition = sprintf(
                  "input['%s'] == 'positive' || input['%s'] == 'negative'",
                  session$ns(kind_id), session$ns(kind_id)
                ),
                numericInput(
                  session$ns(exp_id("min", cr, cn)),
                  "Smallest |r| you would expect (optional)",
                  value = now$min_abs, min = 0, max = 1, step = 0.05, width = "300px"
                )
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'discriminant'", session$ns(kind_id)),
                numericInput(
                  session$ns(exp_id("bound", cr, cn)),
                  "Your bound on |r| (.30 is a convention, not a rule)",
                  value = now$bound, min = 0.01, max = 1, step = 0.05, width = "300px"
                )
              )
            )
          })
        )
      })
      tagList(rows)
    })

    # The researcher's stated expectations for the pairs in the table, read
    # from the store. The three kinds are kept apart: a pair with no
    # prediction is unspecified and is never treated as a discriminant
    # prediction.
    expectations <- reactive({
      crits <- input$criteria
      req(length(crits) > 0)
      sieve_predictions_expectations(state$predictions, pair_constructs(), crits)
    })

    # Stated predictions for pairs the table does not show now (a criterion
    # that is not selected, a construct that is not in the current model).
    predictions_elsewhere <- reactive({
      cons <- tryCatch(pair_constructs(), error = function(e) character(0))
      sieve_predictions_elsewhere(state$predictions, cons, input$criteria %||% character(0))
    })

    # Each construct score against each criterion, at full precision, read
    # against the stated expectation (sieve_validity_evidence).
    crit_df <- reactive({
      crits <- input$criteria
      req(length(crits) > 0)
      scored <- construct_scores()
      if (!is.null(scored$error)) stop(scored$error, call. = FALSE)
      sc <- scored$scores
      cons <- names(sc)
      # The declared sampling design decides whether an interval applies:
      # under clustered or repeated observations the correlations are
      # descriptive.
      state$sampling_design
      design <- sieve_sampling_problem(state, capability = "independent")
      level <- page_level()
      # Each criterion as the gate that offers it read it (aux_read(), through
      # sieve_numeric_columns(): a factor by its labels, never by its level
      # codes; numerals stored as text as those numbers), so the column
      # offered and the column correlated are one reading.
      aux <- aux_read()
      rows <- list()
      for (cr in crits) {
        # Skip a criterion that is no longer offered (a stale selectInput
        # value after a data reload or a role change, or a column the gate
        # now refuses, which the note names with the reason) rather than
        # correlating a reading the gate did not accept.
        if (!cr %in% aux$offered) next
        y <- aux$data[[cr]]
        for (cn in cons) {
          est <- sieve_validity_estimate(sc[[cn]], y, conf_level = level, design = design)
          rows[[length(rows) + 1L]] <- data.frame(
            construct = cn, criterion = cr,
            r = est$r, lower = est$lower, upper = est$upper, n = est$n, p = est$p,
            reason = est$reason, stringsAsFactors = FALSE
          )
        }
      }
      req(length(rows) > 0)
      sieve_validity_evidence(do.call(rbind, rows), expectations())
    })

    # What the criterion table shows: the stated prediction in words, the
    # estimate with its interval, and where the interval lies in relation
    # to the prediction. Still at full precision; the renderer rounds.
    crit_shown <- reactive({
      ev <- crit_df()
      data.frame(
        Construct = ev$construct, Criterion = ev$criterion,
        Prediction = unname(mapply(
          sieve_validity_prediction_text, ev$kind, ev$sign,
          ev$min_abs, ev$bound
        )),
        r = ev$r, CI.lower = ev$lower, CI.upper = ev$upper, n = ev$n, p = ev$p,
        Reading = ev$statement, stringsAsFactors = FALSE
      )
    })

    output$crit_tbl <- renderDT({
      read <- sieve_outcome(crit_df())
      if (read$ready && !read$ok) {
        return(sieve_dt(
          data.frame(Note = paste("The criterion correlations could not be computed:", read$error)),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      ev <- read$value
      if (is.null(ev) || !nrow(ev)) {
        return(sieve_dt(
          data.frame(Note = "Select criterion variables to compute criterion related validity."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      scored <- construct_scores()
      method <- scored$method
      # The level crit_df() computed the intervals at, for the headers and
      # the recorded calls.
      level <- page_level()
      stated <- ev[ev$kind != "unspecified", , drop = FALSE]
      n_open <- sum(ev$kind == "unspecified")
      design <- sieve_sampling_problem(state, capability = "independent")
      # The scores as specified, in runnable R: a factor score reads the
      # fitted correlated-factors model, which is written first.
      any_latent <- any(vapply(scored$spec, function(s) isTRUE(s$latent), logical(1)))
      cfa_spec <- if (any_latent) cfa_run()$spec
      score_code <- c(
        if (any_latent) validity_fit_code(),
        sieve_score_specification_code(scored$spec,
          data_name = "items", fit_name = "validity_fit",
          name_map = cfa_spec$name_map
        )
      )
      sieve_log(
        state,
        sprintf(
          paste0(
            "Validity: construct to criterion correlations (%s; Pearson, %s), ",
            "read against the researcher's stated predictions (%d stated, %d pair%s without one)."
          ),
          method,
          if (is.null(design)) {
            sprintf("two-sided %s Fisher z interval", sieve_ci_percent(level))
          } else {
            sprintf("descriptive, no interval under the declared %s sampling unit", sieve_sampling_spec(state)$unit)
          },
          nrow(stated), n_open, if (n_open == 1L) "" else "s"
        ),
        sieve_validity_code(unique(ev$criterion), method, stated, predictions_elsewhere(),
          design = design, score_code = score_code, conf_level = level
        )
      )
      shown <- crit_shown()
      # The reading is a sentence, so its column is given the room; every
      # pair is shown on one page.
      widths <- c("11%", "11%", "13%", "6%", "9%", "9%", "5%", "6%", "30%")
      sieve_dt(shown,
        rownames = FALSE, selection = "none",
        options = list(
          dom = "t", pageLength = nrow(shown), ordering = FALSE, autoWidth = FALSE,
          columnDefs = lapply(seq_along(widths), function(i) {
            list(width = widths[i], targets = i - 1L)
          })
        ),
        colnames = c(
          "Construct", "Criterion", "Your prediction", "r",
          paste(sieve_ci_percent(level), c("CI lower", "CI upper")), "n", "p",
          "The interval in relation to your prediction"
        )
      ) |>
        formatRound(c("r", "CI.lower", "CI.upper", "p"), 3)
    })

    # A count of where the intervals lie, for the stated predictions only.
    output$crit_tally <- renderUI({
      read <- sieve_outcome(crit_df())
      if (read$ready && !read$ok) {
        return(div(
          class = "alert alert-danger small",
          "The criterion correlations could not be computed: ", read$error
        ))
      }
      ev <- read$value
      req(!is.null(ev), nrow(ev) > 0)
      # The counts, each followed by the pairs it counts, named by construct
      # and criterion with the prediction stated for them.
      lines <- sieve_validity_tally_text(sieve_validity_tally(ev), conf_level = page_level(), evidence = ev)
      div(
        class = "small mt-2",
        lapply(lines, tags$p),
        tags$p(
          class = "text-muted",
          "These counts describe the table. They are not a score for the scale: validity is an ",
          "argument from all of the evidence (Cronbach & Meehl, 1955), and the weight of an ",
          "interval depends on how specific and how risky the prediction was."
        )
      )
    })

    # Every stated prediction as a list the researcher can read at once,
    # with a control to remove each one. A pair on the page is edited in
    # its controls above; a pair whose criterion is not selected is edited
    # by selecting the criterion.
    output$stated_list <- renderUI({
      stated <- sieve_predictions_expectations(state$predictions)
      if (!nrow(stated)) {
        return(div(
          class = "small text-muted mt-2",
          "No prediction has been stated yet. A pair without a prediction is reported and not scored."
        ))
      }
      cons <- tryCatch(pair_constructs(), error = function(e) character(0))
      crits <- input$criteria %||% character(0)
      js <- function(x) gsub("'", "\\\\'", gsub("\\\\", "\\\\\\\\", as.character(x)))
      rows <- lapply(seq_len(nrow(stated)), function(i) {
        on_page <- stated$construct[i] %in% cons && stated$criterion[i] %in% crits
        tags$li(
          sprintf(
            "%s with %s: %s.", stated$construct[i], stated$criterion[i],
            sieve_validity_prediction_text(
              stated$kind[i], stated$sign[i],
              stated$min_abs[i], stated$bound[i]
            )
          ),
          if (!on_page) " Not in the table now (select its criterion in the sidebar to edit it).",
          tags$button(
            type = "button", class = "btn btn-link btn-sm py-0 align-baseline",
            onclick = sprintf(
              "Shiny.setInputValue('%s', {construct: '%s', criterion: '%s'}, {priority: 'event'})",
              session$ns("remove_prediction"), js(stated$construct[i]), js(stated$criterion[i])
            ),
            "Remove"
          )
        )
      })
      div(
        class = "small mt-2",
        tags$b(sprintf("Stated predictions (%d). ", nrow(stated))),
        "Edit a prediction in its controls above; Remove takes it back to no prediction.",
        tags$ul(class = "mb-1", rows)
      )
    })

    # Removing a stated prediction: the pair leaves the store, and the
    # control for it on the page, when there is one, is set to no
    # prediction so that the page and the store agree.
    observeEvent(input$remove_prediction, {
      info <- input$remove_prediction
      req(is.character(info$construct), is.character(info$criterion))
      state$predictions <- sieve_drop_prediction(state$predictions, info$construct, info$criterion)
      updateSelectInput(session, exp_id("kind", info$criterion, info$construct), selected = "unspecified")
    })

    # Stated predictions that the table does not show are named, so that a
    # prediction is never out of sight without a word: it is kept, it is
    # applied to nothing on screen, and it returns with its pair.
    output$crit_kept <- renderUI({
      off <- predictions_elsewhere()
      req(nrow(off) > 0)
      said <- sprintf(
        "%s with %s (%s)", off$construct, off$criterion,
        unname(mapply(
          sieve_validity_prediction_text, off$kind, off$sign,
          off$min_abs, off$bound
        ))
      )
      div(
        class = "small text-muted mt-2",
        sprintf(
          paste0(
            "Also stated, for %s not in this table: %s. %s kept as stated and ",
            "applied when the pair is shown again (select its criterion in the sidebar)."
          ),
          if (nrow(off) == 1L) "a pair" else "pairs", paste(said, collapse = "; "),
          if (nrow(off) == 1L) "It is" else "They are"
        )
      )
    })

    # ---- Structural model (latent regression) -----------------------------
    # One SEM of every construct with each selected criterion regressed on
    # all of them, fit on request from the Fit the structural model button;
    # the selected criteria are the choice the result is kept under.

    sem_key <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta, state$raw)
      req(length(input$criteria) > 0)
      criteria <- unique(as.character(input$criteria))
      key <- sieve_demand_key(state, list(
        criteria = criteria,
        estimator = estimator_choice()
      ))
      external <- intersect(criteria, names(state$raw))
      # Keep selected criterion values with the inputs, outside the choice
      # slot, so recoding a criterion marks the earlier SEM stale.
      key$external_data <- if (length(external)) {
        sieve_data_key(state$raw[, external, drop = FALSE])
      } else NULL
      key
    })
    sem_plan <- reactive({
      req(state$meta)
      crit <- unique(as.character(input$criteria))
      spec <- sieve_lavaan_spec(state$meta, estimator = estimator_choice())
      active <- state$meta[state$meta$status == "active" & sieve_construct_assigned(state$meta$construct), , drop = FALSE]
      p <- nrow(active) + length(crit)
      seconds <- sieve_demand_wait("sem", p, spec$estimator, missing = spec$missing)
      list(
        estimator = spec$estimator, missing = spec$missing, p = p, seconds = seconds,
        label = sieve_duration_label(seconds),
        progress = sprintf(
          "Structural model: %s, %d items, %d criteri%s", spec$estimator, nrow(active),
          length(crit), if (length(crit) == 1L) "on" else "a"
        ),
        detail = "Fitting the measurement model with the criteria regressed on the constructs in lavaan.",
        sentence = sprintf(
          paste0(
            "Fitting the structural model of %d items and %d criteri%s under %s ",
            "is estimated to take %s"
          ),
          nrow(active), length(crit), if (length(crit) == 1L) "on" else "a",
          spec$estimator, sieve_duration_label(seconds)
        ),
        refused = sieve_wlsmv_guard(c(active$item, crit), state$raw,
          ordered = spec$ordered %||% character(0),
          estimator = spec$estimator
        )
      )
    })
    compute_sem <- function(key, plan) {
      items <- shiny::isolate(state$items)
      raw <- shiny::isolate(state$raw)
      meta <- shiny::isolate(state$meta)
      run <- sieve_run_structural(items, raw, meta, key$choices$criteria,
        all_constructs = TRUE,
        estimator = key$choices$estimator %||% "auto"
      )
      if (!is.null(run$fit)) {
        run$measures <- sieve_fit_measures(run$fit, conf_level = sieve_default_rmsea_conf_level())
        run$standardized <- sieve_capture(lavaan::standardizedSolution(run$fit))
      }
      run
    }
    demand_sem <- sieve_demand_wire(
      input, session, state,
      id = "sem_go", what = "Structural model",
      key = sem_key, plan = sem_plan, compute = compute_sem, kind = "sem",
      noun = "The structural model", action = "Fit the structural model",
      refit = "Update"
    )
    sem_run <- reactive({
      run <- demand_sem$result()
      req(run)
      run
    })

    sem_fit <- reactive({
      run <- sem_run()
      if (sieve_has_solution(run)) run$fit else NULL
    })

    output$sem_note <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(NULL)
      }
      if (!length(input$criteria)) {
        return(div(
          class = "alert alert-info",
          "Select one or more criterion variables in the sidebar; they will be regressed on the latent constructs in a single SEM."
        ))
      }
      run <- demand_sem$result()
      tagList(
        panel_line(demand_sem, "The structural model"),
        if (!is.null(run)) sieve_status_alert(run, what = "structural model"),
        if (!is.null(run) && sieve_has_solution(run)) {
          div(
            class = "text-muted small",
            "Standardized path coefficients from one SEM: the measurement model plus each criterion regressed on every construct. Estimating at the latent level avoids the attenuation that biases sum or factor-score regressions."
          )
        }
      )
    })

    output$sem_tbl <- renderDT({
      f <- sem_fit()
      req(f)
      read <- sem_run()$standardized %||% sieve_capture(lavaan::standardizedSolution(f))
      if (!read$ok) {
        return(sieve_dt(
          data.frame(Note = paste("The standardized solution could not be read:", read$error)),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      ss <- read$value
      paths <- ss[ss$op == "~", , drop = FALSE]
      req(nrow(paths) > 0)
      latmap <- attr(f, "lat_map")
      outmap <- attr(f, "out_map")
      df <- data.frame(
        Criterion = unname(ifelse(paths$lhs %in% names(outmap), outmap[paths$lhs], paths$lhs)),
        Construct = unname(ifelse(paths$rhs %in% names(latmap), latmap[paths$rhs], paths$rhs)),
        beta = paths$est.std, CI.lower = paths$ci.lower, CI.upper = paths$ci.upper,
        p = paths$pvalue, stringsAsFactors = FALSE
      )
      sieve_log(
        state,
        "Validity: structural model regressing criteria on the latent constructs (standardized paths).",
        c(
          "# measurement model + outcome ~ constructs, fit with lavaan::sem()",
          "lavaan::standardizedSolution(fit)[op == \"~\", ]"
        )
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25),
        colnames = c("Criterion", "Construct", "beta", "CI lower", "CI upper", "p")
      ) |>
        formatRound(c("beta", "CI.lower", "CI.upper", "p"), 3)
    })

    output$sem_r2 <- renderDT({
      f <- sem_fit()
      req(f)
      read <- sieve_capture(lavaan::lavInspect(f, "r2"))
      if (!read$ok) {
        return(sieve_dt(
          data.frame(Note = paste("The variance explained could not be read from the model:", read$error)),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      r2 <- read$value
      outmap <- attr(f, "out_map")
      req(!is.null(r2), !is.null(outmap))
      keep <- names(r2) %in% names(outmap)
      req(any(keep))
      fm <- sieve_fit_measures_at(sem_run()$measures, f, sieve_default_rmsea_conf_level())
      cap <- if (!is.null(fm) && is.finite(fm["cfi"])) {
        sprintf(
          "Model fit: %s %.3f, %s %.3f, %s [%.3f, %.3f], SRMR %.3f",
          sieve_fit_label(fm, "cfi"), fm["cfi"], sieve_fit_label(fm, "rmsea"),
          fm["rmsea"], sieve_rmsea_ci_words(sieve_fit_ci_level(fm)), fm["rmsea_lo"], fm["rmsea_hi"], fm["srmr"]
        )
      } else {
        ""
      }
      df <- data.frame(
        Criterion = unname(outmap[names(r2)[keep]]),
        R2 = as.numeric(r2[keep]), stringsAsFactors = FALSE
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t"), caption = cap,
        colnames = c("Criterion", "R-squared")
      ) |>
        formatRound("R2", 3)
    })

    # ---- Marker-variable adjustment (Lindell & Whitney, 2001) --------------
    # The Pearson matrix of the construct scores in force is adjusted by
    # r_M, the smallest positive marker to construct-score correlation,
    # under the constant method effect assumption; the adjustment is read
    # over two larger values as well (sieve_marker_sensitivity()), with
    # Lindell and Whitney's t test on n - 3 degrees of freedom.

    output$marker_note <- renderUI({
      mk <- input$marker %||% ""
      if (!nzchar(mk)) {
        return(div(
          class = "alert alert-info",
          paste0(
            "Select a marker variable in the sidebar: a variable that theory says is unrelated to the ",
            "constructs. Its correlation with the construct scores, r_M, is taken as shared method ",
            "variance and partialled from every construct-score correlation: r_adjusted = ",
            "(r - r_M) / (1 - r_M) (Lindell & Whitney, 2001)."
          )
        ))
      }
      read <- sieve_outcome(construct_scores())
      what <- if (read$ready && read$ok && is.null(read$value$error)) {
        read$value$method
      } else {
        "the operational construct scores in force"
      }
      div(
        class = "text-muted small",
        tags$p(sprintf(
          paste0(
            "Marker: %s. What is adjusted: the Pearson correlation matrix of the operational construct ",
            "scores in force (%s), formed from the pairwise complete eligible scores. r_M is the ",
            "smallest positive correlation between the marker and those construct scores; it is one ",
            "conservative choice, not the only one, so the adjustment is also read at larger values ",
            "below (the sensitivity). The adjustment r_adjusted = (r - r_M) / (1 - r_M) (Lindell & ",
            "Whitney, 2001, Equation 4; computed by DMAR::common_method_marker) assumes a constant ",
            "method effect: the same r_M is removed from every correlation, whichever constructs it ",
            "joins. Each adjusted correlation is tested with Lindell and Whitney's t statistic on ",
            "n - 3 degrees of freedom (their Equation 5), with n the smallest pairwise count among ",
            "the construct scores. A correlation that remains different from zero after the ",
            "adjustment is consistent with a relationship that is not solely a method artifact under ",
            "that assumption; it is not evidence that no method variance is present, and a negative ",
            "correlation is pushed away from zero by the adjustment rather than toward it."
          ),
          mk, what
        ))
      )
    })

    marker_df <- reactive({
      mk <- input$marker %||% ""
      # nzchar guards the empty default; the names() check guards a stale marker
      # selection that no longer matches a column after a data reload.
      req(nzchar(mk), mk %in% names(state$raw))
      scored <- construct_scores()
      # The scores in force, or the reason they are unavailable; no
      # substitute score is adjusted in their place.
      if (!is.null(scored$error)) stop(scored$error, call. = FALSE)
      sc <- scored$scores
      cons <- names(sc)
      req(length(cons) >= 2L)
      # The marker as the gate that offers it read it (aux_read(): a factor
      # by its labels, never by its level codes), as the criteria are.
      aux <- aux_read()
      req(mk %in% aux$offered)
      mv <- aux$data[[mk]]
      # r_M: the smallest positive marker-construct correlation (Lindell and
      # Whitney, 2001). If the marker shows no positive correlation with any
      # construct score, no adjustment is defined and the table reports that.
      r_marker <- vapply(cons, function(cn) {
        ok <- is.finite(sc[[cn]]) & is.finite(mv)
        if (sum(ok) >= 4L) stats::cor(sc[[cn]][ok], mv[ok]) else NA_real_
      }, numeric(1L))
      pos <- r_marker[is.finite(r_marker) & r_marker > 0]
      if (!length(pos)) {
        return(list(rM = NA_real_, tbl = NULL, marker_r = r_marker, method = scored$method))
      }
      rM <- min(pos)
      M <- as.matrix(sc)
      R <- stats::cor(M, use = "pairwise.complete.obs")
      # The sample size of the t test: the smallest pairwise complete count.
      counts <- crossprod(is.finite(M) * 1)
      n_min <- as.integer(min(counts[upper.tri(counts)]))
      # The two larger values: the researcher's, or r_M + .05 and r_M + .10.
      more <- c(sieve_finite_or(input$marker_r2, NA_real_), sieve_finite_or(input$marker_r3, NA_real_))
      defaults <- pmin(rM + c(.05, .10), .99)
      more[is.na(more)] <- defaults[is.na(more)]
      more <- more[more > -1 & more < 1]
      values <- unique(c(rM, more))
      sens <- sieve_capture(sieve_marker_sensitivity(R, values, n = if (is.finite(n_min) && n_min > 3) n_min else NULL))
      if (!sens$ok) {
        return(list(
          rM = rM, tbl = NULL, marker_r = r_marker, method = scored$method,
          error = sprintf("The marker adjustment stopped: %s", sens$error)
        ))
      }
      s <- sens$value
      main <- s[s$marker_r == rM, , drop = FALSE]
      tbl <- data.frame(
        Construct.1 = main$variable_1, Construct.2 = main$variable_2,
        r.unadjusted = main$r, r.adjusted = main$r_adjusted, t = main$t, p = main$p,
        stringsAsFactors = FALSE
      )
      list(
        rM = rM, tbl = tbl, sensitivity = s, values = values, n = n_min,
        marker_r = r_marker, method = scored$method
      )
    })

    output$marker_tbl <- renderDT({
      read <- sieve_outcome(marker_df())
      if (!read$ready || (read$ok && is.null(read$value))) {
        return(sieve_dt(
          data.frame(Note = "Select a marker variable and ensure at least two constructs have scores."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      if (!read$ok) {
        return(sieve_dt(
          data.frame(Note = paste("The marker adjustment could not be computed:", read$error)),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      res <- read$value
      if (!is.null(res$error)) {
        return(sieve_dt(
          data.frame(Note = res$error),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      if (is.null(res$tbl)) {
        return(sieve_dt(
          data.frame(Note = paste0(
            "The marker has no positive correlation with any construct score, so the Lindell and ",
            "Whitney adjustment is undefined here without a positive marker correlation: there is no ",
            "r_M to partial out. That does not show that method variance is absent; the marker may ",
            "be a poor proxy for the method."
          )),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      sieve_log(
        state,
        sprintf(
          "Validity: marker-variable adjustment of the construct-score correlations (%s; Lindell & Whitney, 2001; DMAR::common_method_marker through SIEVER::sieve_marker_sensitivity); r_M = %.3f, read also at %s; t test on n - 3 = %d df.",
          res$method, res$rM, paste(sprintf("%.3f", res$values[-1]), collapse = " and "), res$n - 3L
        ),
        c(
          "R <- cor(construct_scores, use = \"pairwise.complete.obs\")",
          "# r_M = smallest positive marker to construct-score correlation; the adjustment is read at larger values as well",
          sprintf(
            "marker_sensitivity <- SIEVER::sieve_marker_sensitivity(R, marker_r = c(%s), n = %d)",
            paste(sieve_num_lit(res$values), collapse = ", "), res$n
          )
        )
      )
      sieve_dt(res$tbl,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25),
        caption = sprintf(
          "Marker correlation r_M = %.3f (the smallest positive marker to construct-score correlation); t on n - 3 = %d degrees of freedom",
          res$rM, res$n - 3L
        ),
        colnames = c("Construct 1", "Construct 2", "r (unadjusted)", "r (adjusted)", "t", "p")
      ) |>
        formatRound(c("r.unadjusted", "r.adjusted", "t", "p"), 3)
    })

    output$marker_sens_note <- renderUI({
      read <- sieve_outcome(marker_df())
      req(read$ready, read$ok, !is.null(read$value$tbl))
      res <- read$value
      shown <- res$marker_r[is.finite(res$marker_r)]
      div(
        class = "small text-muted mt-2",
        sprintf(
          paste0(
            "Sensitivity: the adjustment at r_M = %.3f (the smallest positive marker correlation) and at %s, ",
            "which you can change above (blank: r_M + .05 and r_M + .10). The marker's correlations with ",
            "the construct scores are %s. A relationship that holds at every value you consider ",
            "plausible is the stronger reading; one that holds only at the smallest r_M rests on that choice."
          ),
          res$rM, paste(sprintf("%.3f", res$values[-1]), collapse = " and "),
          paste(sprintf("%s %.3f", names(shown), shown), collapse = ", ")
        )
      )
    })

    output$marker_sens_tbl <- renderDT({
      read <- sieve_outcome(marker_df())
      req(read$ready, read$ok, !is.null(read$value$sensitivity))
      s <- read$value$sensitivity
      sieve_dt(s,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25),
        colnames = c("r_M", "Construct 1", "Construct 2", "r (unadjusted)", "r (adjusted)", "t", "df", "p")
      ) |>
        formatRound(c("marker_r", "r", "r_adjusted", "t", "p"), 3)
    })
    # The criterion note's copy of the measurement model's button, under
    # its own id, runs the same fit as the button in the page's line.
    observeEvent(input$scores_fit_go, demand$run())
    boundary_result <- sieve_bound_server(
      input, output, session, state, cfa_run,
      function() identical(demand$status()$state, "current")
    )

    # ---- One Update for the page --------------------------------------------
    # The three models this tab fits on request, in the order Update fits
    # them: the measurement model first, because the method-factor
    # comparison reuses it when it is current (so its estimate here is the
    # method-factor model's alone). The HTMT bootstrap interval and the
    # latent boundary comparison keep their own buttons: one is a
    # bootstrap, the other a comparison against a boundary the researcher
    # states. Each is named in the line when an edit made it historical.
    ulmf_alone_plan <- reactive(measurement_plan("Method-factor model", "method_factor"))
    held_since <- function(revision) {
      state$dataset_id
      state$items
      state$meta
      sieve_stale_reason(revision, sieve_revision(state))
    }
    htmt_entry <- function() {
      read <- sieve_outcome(htmt_ci())
      if (!read$ready || !read$ok || is.null(read$value) || !is.null(read$value$error)) {
        return(NULL)
      }
      if (is.null(held_since(read$value$revision))) {
        return(NULL)
      }
      sieve_update_entry("HTMT bootstrap interval", "stale",
        update = FALSE, button = "Compute HTMT CIs", why = "a bootstrap"
      )
    }
    boundary_entry <- function() {
      read <- sieve_outcome(boundary_result())
      if (!read$ready || !read$ok || is.null(read$value) || !is.null(read$value$value$error)) {
        return(NULL)
      }
      if (is.null(held_since(read$value$revision)) && identical(demand$status()$state, "current")) {
        return(NULL)
      }
      sieve_update_entry("Latent boundary comparison", "stale",
        update = FALSE, button = "Compare With My Boundary",
        why = "after the measurement model is current"
      )
    }
    page_update <- sieve_update_wire(input, session, function() {
      list(
        measurement = sieve_update_demand_entry(demand, "Correlated-factors model", cfa_plan),
        structural = sieve_update_demand_entry(demand_sem, "Structural model", sem_plan),
        method_factor = sieve_update_demand_entry(demand_ulmf, "Method-factor model", ulmf_alone_plan),
        htmt = htmt_entry(),
        boundary = boundary_entry()
      )
    })

    # ---- Show code at the point of use -------------------------------------
    # Each table above writes its step to the reproducible code when it is
    # drawn; the disclosure under it reads that step back
    # (sieve_code_disclosure()), under the status of what the table
    # depends on: the measurement model's demand status for AVE, the
    # criterion correlations and the marker adjustment; the bootstrap's own
    # currency for its upper bounds; the input revision for the HTMT point
    # estimates. Reading a panel evaluates nothing.
    sieve_code_navigation(input, session, state)
    output$ave_code <- renderUI({
      sieve_code_disclosure(state, "Validity: AVE per construct", status = demand$status(), ns = session$ns,
        title = "Show code: AVE and each construct's largest correlation with another",
        readout = c("ave", "# sqrt(ave) is shown beside each construct's largest |r| with another construct, read from validity_ss; no cutoff is applied."))
    })
    output$crit_code <- renderUI({
      sieve_code_disclosure(state, "Validity: construct to criterion correlations", status = demand$status(),
        ns = session$ns, title = "Show code: construct to criterion correlations")
    })
    output$htmt_code <- renderUI({
      if (!isTRUE(as.logical(input$htmt_on))) {
        return(NULL)
      }
      sieve_code_disclosure(state, "Validity: HTMT per construct pair", ns = session$ns,
        title = "Show code: HTMT per construct pair")
    })
    output$htmt_ci_code <- renderUI({
      r <- tryCatch(htmt_ci_now(), error = function(e) NULL)
      status <- if (is.null(r)) {
        list(state = "not_run")
      } else if (!is.null(r$stale)) {
        list(state = "stale", sentence = sprintf("What changed: %s.", r$stale))
      } else if (!is.null(r$error)) {
        list(state = "failed", sentence = r$error)
      } else {
        list(state = "current")
      }
      sieve_code_disclosure(state, "Validity: HTMT bootstrap upper bounds", status = status, ns = session$ns,
        title = "Show code: HTMT bootstrap upper bounds",
        readout = sprintf("htmt_bounds  # each pair's HTMT with its one-sided %s percentile upper bound",
          level_text(r$level %||% tryCatch(page_level(), error = function(e) sieve_default_conf_level()))))
    })
    output$marker_code <- renderUI({
      sieve_code_disclosure(state, "Validity: CMV marker adjustment", status = demand$status(), ns = session$ns,
        title = "Show code: marker variable adjustment")
    })
  })
}

#' Adapt DMAR's HTMT Bound Schema for Display
#'
#' Each pair's HTMT and its one-sided upper bound, with no reading against a
#' cutoff: SIEVER applies no cutoff in output, so the table has no column
#' saying whether a bound lies below a cutoff.
#'
#' @param h The DMAR table, including its one-sided `ci_upper` column.
#' @return A display table with `Pair`, `HTMT` and `Upper`, retaining full
#'   numeric precision.
#' @author Ken Kelley
#' @noRd
sieve_htmt_bounds_table <- function(h) {
  sieve_dmar_require_columns(h, c("construct_1", "construct_2", "htmt", "ci_upper"), "HTMT bounds")
  data.frame(Pair = sprintf("%s - %s", h$construct_1, h$construct_2),
    HTMT = h$htmt, Upper = h$ci_upper, stringsAsFactors = FALSE)
}

#' The HTMT convention, named with its source, as context
#'
#' One sentence, the same wherever the Validity page shows HTMT: the values
#' Henseler, Ringle and Sarstedt (2015) proposed, and that SIEVER applies
#' no cutoff.
#'
#' @return One string.
#' @author Ken Kelley
#' @noRd
sieve_htmt_convention_text <- function() {
  paste(
    "Henseler, Ringle and Sarstedt (2015) proposed 0.85, and 0.90 for conceptually similar constructs,",
    "as conventions for HTMT; SIEVER applies no cutoff and shows each value as estimated."
  )
}

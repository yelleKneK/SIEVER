# mod_bifactor.R
# Optional bifactor measurement model. A bifactor model places one general
# factor that loads every active item alongside a set of orthogonal group
# (specific) factors, one per construct with at least three active items, or
# one per facet when the study has one construct split into facets (the
# groups of Compare structures on the CFA tab; sieve_bifactor_plan()).
# Because the general and group factors are independent (orthogonal = TRUE,
# std.lv = TRUE), the standardized loadings partition each item's reliable
# variance into a general part and a group part, which is exactly what the
# bifactor indices (omegaH, omegaHS, ECV, PUC, H) summarize per Reise (2012)
# and Rodriguez, Reise & Haviland (2016). This model is methodologically
# heavier than the correlated-factors CFA, so it is off by default and only
# fits on an explicit button press.

#' Bifactor model module UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput verbatimTextOutput actionButton numericInput div tags p strong
#' @importFrom bslib layout_sidebar sidebar card card_header card_body
#' @importFrom bslib navset_card_tab nav_panel
#' @importFrom DT DTOutput
mod_bifactor_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      title = "Bifactor model",
      p(
        "A bifactor model adds one ", strong("general factor"),
        " that loads every active item, plus ", strong("orthogonal group factors"),
        " (one per construct with at least three active items, or one per facet when",
        " the study has one construct split into facets; at least three group factors,",
        " the rule Compare structures on the CFA tab applies as well). The general and",
        " group factors are independent, so each item's reliable variance splits",
        " into a general part and a specific part."
      ),
      p(
        "Use it to gauge how strongly a single general dimension accounts for",
        " the items (ECV, omega hierarchical) and whether the subscales retain",
        " unique, replicable variance after the general factor is removed",
        " (omegaHS, H)."
      ),
      p(
        class = "small text-muted mb-0",
        "The model is fit from the Fit bifactor model button above the tabs, which",
        " states its estimated time; nothing is fit until it is pressed."
      ),
      sieve_estimator_panel(ns)
    ),
    sieve_optional_tab_note("the bifactor model and its indices (ECV, omegaH, omegaHS, PUC, H), the evidence on whether the items can be scored as one total"),
    # Results first: the one status line,
    # the one-line method statement with the fit, then the indices; what
    # each index is and how the set is read, below the table.
    uiOutput(ns("gate")),
    uiOutput(ns("status")),
    uiOutput(ns("method")),
    navset_card_tab(
      nav_panel(
        "Bifactor indices",
        uiOutput(ns("intro")),
        card(
          card_header("ECV, omega, omegaH, omegaHS, PUC, and H"),
          DTOutput(ns("idx_tbl")),
          uiOutput(ns("bf_code")),
          # An improper solution is announced in the status line above the
          # tabs; what it means for these indices is said directly under
          # them.
          uiOutput(ns("improper")),
          uiOutput(ns("essential")),
          sieve_more_below(
            tagList(
              "These indices answer one question (Reise, 2012; Rodriguez, Reise & Haviland, 2016): ",
              tags$b("is this scale unidimensional enough to score as a single total, even though it is multidimensional?")
            ),
            tags$ul(
              class = "mb-1",
              tags$li(
                tags$b("ECV"),
                " (explained common variance): the share of common variance due to the general factor (in a group factor's row, the share within that group's items; see below the table). It is read together with PUC, since the higher the PUC, the less the size of ECV matters for the bias a unidimensional model would carry (Reise, Scheines, Widaman, & Haviland, 2013; Bonifay, Reise, Scheines, & Meijer, 2015). Rodriguez, Reise, and Haviland (2016) read an ECV of .80 with a PUC of .77 as common variance that is ",
                # No whitespace after the bold phrase, so the comma follows it.
                tags$b("essentially unidimensional", .noWS = "after"),
                ", and SIEVER applies no cutoff."
              ),
              tags$li(
                tags$b("Omega (model-implied denominator)"),
                " (total): the reliability of the total score from all sources (general plus group factors), with the total-score variance the fitted bifactor model reproduces in the denominator (DMAR::bifactor_indices(), as in Rodriguez, Reise, & Haviland, 2016). It is computed from the standardized loadings, so it describes the total of the standardized items (with ordered or binary items, of their standardized latent responses). The omega of the raw total score under the table is a different coefficient: robust omega (Kelley & Pornprasertmanit, 2016) of a one-factor model of every item. Its numerator is the squared sum of that model's loadings, so group-factor variance the single factor does not absorb counts as error. It is computed on the items as scored, over the observed variance of their unweighted total. It differs from this omega through its numerator and its item metric, not only its denominator, and under a bifactor structure it usually falls between the omegaH and the omega of that same total. Beside it, the bifactor model's own Omega and omegaH with the observed denominator (the same standardized loadings over the observed correlations of the items, DMAR::bifactor_indices(denominator = \"observed\")) show how much of any gap the denominator alone accounts for. When any item is treated as ordered or binary, the line under the table is instead categorical omega (Green & Yang, 2009) of a one-factor model of every item, for the sum of the categories as scored, over the total variance built from the unrestricted polychoric correlations and the thresholds (the observed denominator), while this table describes the sum of the latent responses."
              ),
              tags$li(
                tags$b("omegaH"),
                " (hierarchical): the proportion of total-score variance attributable to the ",
                tags$b("general factor alone"),
                " (McDonald, 1999). It is read against omega: the gap omega minus omegaH is the variance the group factors add. Rodriguez et al. (2016) read an omegaH of .93 against an omega of .98 as a total score that is essentially univocal for the general factor, and SIEVER applies no cutoff."
              ),
              tags$li(
                tags$b("omegaHS"),
                " (hierarchical subscale): the reliable variance ",
                tags$b("unique"),
                " to a subscale after the general factor is partialled out. A low omegaHS means the subscale adds little beyond the general factor and may not warrant a separate score (Reise, 2012; Reise, Bonifay, & Haviland, 2013)."
              ),
              tags$li(
                tags$b("PUC"),
                " (percent of uncontaminated correlations): the share of item correlations that reflect only the general factor; read ECV alongside PUC (a high PUC makes a given ECV more tolerant of unidimensional scoring)."
              ),
              tags$li(
                tags$b("H"),
                " (construct replicability; Hancock & Mueller, 2001): how well a factor is defined by its indicators and would replicate. Hancock and Mueller (2001) proposed .70 as a convention for H, which Rodriguez et al. (2016) apply to the general and the group factors, and SIEVER applies no cutoff."
              )
            ),
            tags$p(
              class = "mb-1",
              tags$b("Reading the set:"),
              " a high ECV and PUC and an omegaH close to omega, with low subscale omegaHS, are evidence for ",
              tags$b("a total score only"),
              "; substantial omegaHS and H on well-defined subscales ",
              "is evidence for ",
              tags$b("reporting subscales too"),
              " (or instead). These are conventions with the citations above, not rules: ",
              "the researcher weighs them with the bootstrap intervals below, the fit of the model, and the use the scores will be put to. ",
              "An improper solution voids all of this."
            ),
            tags$p(
              class = "mb-0",
              tags$b("What a row holds. "),
              "In the general factor's row, ECV and omega are computed over every item: the general ",
              "factor's share of the common variance, and the reliability of the total score. In a ",
              "group factor's row they are computed over that group's items only: the general factor's ",
              "share of the group's common variance, beside it the group factor's own share (one minus ",
              "it), and subscale omega, the reliability of the subscale total from the general and the ",
              "group factor together; omegaHS is the part of the subscale total's variance the group ",
              "factor alone accounts for, and H is the group factor's coefficient H."
            ),
            label = "More on the indices: each one, reading the set, and what a row holds"
          ),
          tags$hr(),
          card_header("Bootstrap confidence intervals for the bifactor indices"),
          div(
            class = "d-flex align-items-center gap-2 mb-2 flex-wrap",
            sieve_conf_level_input(ns("conf_level"), "Confidence Level for Bifactor Indices"),
            sieve_boot_inputs(ns, B = 1000, B_id = "idx_B", seed_id = "idx_seed", min_B = 400, max_B = 10000, step_B = 100, label = "Bootstrap replications", width = "180px", seed_label = "Seed (optional)", seed_width = "140px"),

            actionButton(ns("idx_boot"), "Bootstrap index CIs", class = "btn-primary"),
            uiOutput(ns("idx_stop_ui"), inline = TRUE)
          ),
          uiOutput(ns("idx_plan_note")),
          uiOutput(ns("idx_ci_note")),
          DTOutput(ns("idx_ci_tbl"), fill = FALSE),
          uiOutput(ns("boot_code")),
          uiOutput(ns("idx_ci_range")),
          tags$small(
            class = "text-muted d-block mt-2",
            "Percentile bootstrap, aligned with the estimates above: DMAR's counted bootstrap ",
            "(DMAR::ci_bifactor_indices(), model-implied denominator) resamples the rows the fitted ",
            "model used (with their missing data patterns) and refits the same model with the same ",
            "lavaan options, run in a background R process with progress shown and a Cancel that ",
            "cancels at the next replication (on several processes, at the end of the block of ",
            "replications in progress; the process is ended if DMAR has not stopped five seconds ",
            "after the press). One rule decides which replications are used, and it ",
            "is the same for every index: the refit must converge and every index must be a ",
            "proportion (H defined, omega not above one); an improper solution (a standardized ",
            "loading above one or a negative variance) whose indices are proportions is kept and ",
            "counted (improper = \"keep\", SIEVER's rule; DMAR's own default here is \"drop\"), and ",
            "refits that fail are set aside and counted. The counts of replications attempted, ",
            "converged, admissible, and used are reported with the intervals. With a declared ",
            "cluster design SIEVER's own aligned bootstrap resamples whole clusters. PUC depends ",
            "only on which item belongs to which group factor, so resampling cannot move ",
            "it and it has no interval. The intervals are for the model as specified: they ",
            "do not carry the uncertainty of having chosen the items or their grouping ",
            "from these same data. Wide intervals say that an index (ECV, omegaH, omegaHS) ",
            "is not estimated precisely enough to carry a scoring decision on its own."
          )
        )
      ),
      nav_panel(
        "Loadings",
        card(
          card_header("Standardized loadings on the general and group factors"),
          DTOutput(ns("load_tbl"))
        )
      ),
      nav_panel(
        "Model",
        card(
          card_header("Headline fit (CFI, RMSEA with its CI, SRMR)"),
          DTOutput(ns("fit_tbl"))
        ),
        card(
          card_header("lavaan model syntax"),
          verbatimTextOutput(ns("syntax"))
        )
      )
    )
  )
}

#' Bifactor model module server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveVal req renderUI renderPrint
#' @importFrom shiny observeEvent eventReactive withProgress incProgress div HTML strong tags
#' @importFrom shiny observe isolate tagList helpText
#' @importFrom DT renderDT datatable formatRound
mod_bifactor_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    # The tab's Estimator control: the study's setting (Ingest tab) unless
    # chosen otherwise here (sieve_estimator_wire()).
    estimator_choice <- sieve_estimator_wire(input, output, state)

    # The most recent fit: a list with model syntax, the fitted lavaan
    # object, any error message, the omega of the raw total score on the
    # same rows, and the revision (dataset, item data, ledger) it was fitted
    # to. It is fit on request (fct_ondemand.R): the Fit bifactor model
    # button states the estimated time, the result is kept with the
    # revision it was computed from, and the status line says in one line
    # when the data or the item pool changed since; the result stays on
    # screen with that line, and the bootstrap refuses to run on it until
    # it is refit. NULL until the user presses Fit.
    bf_key <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      sieve_demand_key(state, list(estimator = estimator_choice()))
    })
    bf_plan <- reactive({
      req(state$meta)
      spec <- sieve_lavaan_spec(state$meta, estimator = estimator_choice())
      active <- state$meta[state$meta$status == "active" & sieve_construct_assigned(state$meta$construct), , drop = FALSE]
      p <- nrow(active)
      seconds <- sieve_demand_wait("bifactor", p, spec$estimator, missing = spec$missing) +
        sieve_demand_wait("total_omega", p, spec$estimator, missing = spec$missing)
      list(
        estimator = spec$estimator, missing = spec$missing, p = p, seconds = seconds,
        label = sieve_duration_label(seconds),
        progress = sprintf("Bifactor model: %s, %d items", spec$estimator, p),
        detail = "Fitting the general and group factors in lavaan, then the omega of the raw total score on the same rows.",
        sentence = sprintf(
          paste0(
            "Fitting the bifactor model of %d items under %s, and the one factor ",
            "model behind the omega of the raw total score, is estimated to take %s"
          ),
          p, spec$estimator, sieve_duration_label(seconds)
        ),
        # A ledger with no bifactor model (one construct without facets, or
        # too few group factors) is refused before anything is fit, with
        # the reason the plan gives.
        refused = {
          built <- sieve_bifactor_plan(state$meta, reserved = union(names(state$items), names(state$raw)))
          if (!nzchar(built$model) && !is.na(built$reason)) {
            built$reason
          } else {
            sieve_wlsmv_guard(active$item, state$items,
              ordered = spec$ordered %||% character(0),
              estimator = spec$estimator
            )
          }
        }
      )
    })
    compute_bifactor <- function(key, plan) {
      items <- shiny::isolate(state$items)
      meta <- shiny::isolate(state$meta)
      estimator <- key$choices$estimator %||% "auto"
      spec <- sieve_model_spec(sieve_bifactor_plan(meta, reserved = union(names(items), names(state$raw))),
        meta, kind = "bifactor", fun = "cfa", std_lv = TRUE, orthogonal = TRUE, estimator = estimator)
      prepared <- sieve_sampling_apply(spec, state, items)
      if (!is.null(prepared$reason)) stop(prepared$reason, call. = FALSE)
      r <- sieve_fit_bifactor(prepared$data, meta, estimator = estimator,
        aux = prepared$spec$engine_args$aux, cluster = prepared$spec$engine_args$cluster)
      r$bootstrap_row_index <- prepared$row_index
      r$input_rows <- prepared$row_index
      r$warnings <- c(r$warnings, prepared$warnings)
      # Declared auxiliary variables left out because every analyzed item is
      # observed (sieve_auxiliary_apply()); the method line and the recorded
      # R say so.
      r$aux_notes <- prepared$notes
      # The fit is bound to the dataset it was made on, so the dataset needs
      # its identity (assigned here when the data arrived without one).
      sieve_ensure_dataset(state)
      r$revision <- sieve_revision(state)
      if (is.null(r$error) && !is.null(r$fit)) {
        sieve_register_analysis(state, r, note = "Fit the bifactor model")
        sieve_log(
          state,
          "Fit bifactor model (one general factor plus orthogonal group factors)",
          c(
            if (length(r$aux_notes)) paste("#", r$aux_notes),
            sieve_cfa_code(r$spec, fit_name = "bifactor_fit", model_name = "bifactor_model",
              conf_level = sieve_default_rmsea_conf_level())
          )
        )
        # The record a disclosure under the indices reads: the step just
        # written, with its settings.
        r$code_view <- sieve_code_snapshot(state, context = c(
          sprintf("Modeled item columns: %s.", paste(r$spec$observed, collapse = ", ")),
          sprintf("Estimator: %s; missing data: %s; general and group factors orthogonal, variances fixed at 1.",
            r$spec$estimator, r$spec$missing),
          r$aux_notes,
          "ECV, omegaH, omegaHS, PUC and H are computed from the standardized loadings of this fit (Rodriguez, Reise & Haviland, 2016); they are displayed to three decimal places."
        ))
        # The RMSEA interval at the RMSEA level (90% by default, DMAR's and
        # lavaan's), not at the page's confidence level, which sets the
        # bifactor indices' intervals.
        r$measures <- sieve_fit_measures(r$fit, conf_level = sieve_default_rmsea_conf_level())
        # Omega of the unweighted total of the active items, on the rows the
        # bifactor model uses (see sieve_total_omega()): a one factor model
        # of every item, fit here under the same progress bar rather than
        # when the indices panel is shown.
        total <- sieve_capture(sieve_total_omega(prepared$data, meta, estimator = estimator,
          aux = prepared$spec$engine_args$aux))
        r$total_omega <- total$value
        if (!total$ok) {
          r$warnings <- c(r$warnings, paste("The omega of the raw total score could not be computed:", total$error))
        }
        # What the engine noted about the omega of the raw total score (for
        # ordered items, that its codes are not one rescaling of the ranks
        # categorical omega scores) is shown with the fit's other conditions.
        noted <- unique(gsub("\\s+", " ", c(total$messages, total$value$messages)))
        if (length(noted)) {
          r$warnings <- c(r$warnings, sprintf(
            "While estimating the omega of the raw total score the engine noted: %s.",
            paste(sprintf("\"%s\"", noted), collapse = "; ")
          ))
        }
      }
      r
    }
    demand <- sieve_demand_wire(
      input, session, state,
      id = "fit", what = "Bifactor model", capability = "cluster",
      key = bf_key, plan = bf_plan, compute = compute_bifactor, kind = "bifactor",
      noun = "The bifactor model", action = "Fit bifactor model",
      refit = "Update"
    )
    # NULL until a fit exists, also before the item types are confirmed.
    res <- reactive(sieve_outcome(demand$result())$value)

    # The fit on screen with what changed since it was made, or NULL when
    # there is none. A fit made before the data or the ledger changed stays
    # on screen, with the sentence that says what changed (`stale`); the
    # bootstrap refuses to run on it until it is refit.
    shown <- reactive({
      st <- demand$status()
      r <- st$value
      if (is.null(r)) {
        return(NULL)
      }
      if (identical(st$state, "stale")) {
        r$stale <- st$sentence
      }
      r
    })
    # The fit on screen, or NULL when there is none.
    current <- function() {
      shown()
    }

    # The pipeline gate. A fit that stopped with an error is stated once,
    # by the status line below, in one line.
    output$gate <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(sieve_gate(state))
      }
      NULL
    })

    # What was established about the fitted model, shown above every panel:
    # a fitted object is not a certificate. A model that did not converge or
    # is not identified has no loadings or indices to read; an improper
    # solution is announced here and explained on the indices panel. The
    # line above it says whether the model is not fit yet, current, or
    # stopped with an error, with its button; or, when it is from before a
    # change, what changed, with the page's one Update.
    output$status <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(NULL)
      }
      r <- shown()
      tagList(
        page_update$line() %||% tagList(demand$line(), page_update$note()),
        if (!is.null(r$indices_reason)) div(class = "alert alert-info", r$indices_reason),
        if (!is.null(r) && !is.null(r$fit)) {
          sieve_status_alert(r$status, what = "bifactor model", warnings = r$warnings, spec = r$spec)
        }
      )
    })

    # The one-line method statement of the fitted model, with its fit.
    output$method <- renderUI({
      r <- if (isTRUE(sieve_confirmation_status(state)$confirmed)) shown()
      if (is.null(r) || is.null(r$fit) || !sieve_has_solution(r$status)) {
        return(NULL)
      }
      sieve_method_line(r$fit, "Bifactor model",
        identification = "general and group factors orthogonal, variances fixed at 1",
        measures = sieve_fit_measures_at(r$measures, r$fit, sieve_default_rmsea_conf_level()),
        notes = r$aux_notes
      )
    })

    output$intro <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      if (!is.null(current())) {
        return(NULL)
      }
      card(
        card_header("The bifactor model is optional and off by default"),
        card_body(
          p(
            "A bifactor model estimates one general factor across all active",
            " items together with orthogonal group factors, one for each",
            " construct that has at least three active items (one for each such facet",
            " when the study has one construct split into facets), and it needs at least",
            " three group factors. It is the proper",
            " full-information specification (not a Schmid-Leiman approximation),",
            " fit by maximum likelihood through lavaan."
          ),
          p(
            "Fit it when you want to know whether the item set is essentially",
            " unidimensional for scoring purposes, how much common variance the",
            " general factor explains (ECV, omega hierarchical), and whether the",
            " subscales carry replicable specific variance (omegaHS, H), per",
            " Reise (2012) and Rodriguez, Reise & Haviland (2016)."
          ),
          p(strong("Press Fit bifactor model, above the tabs, to estimate it."))
        )
      )
    })

    output$syntax <- renderPrint({
      r <- current()
      req(r, r$syntax)
      cat(r$syntax)
    })

    output$fit_tbl <- renderDT({
      r <- current()
      req(r, r$fit)
      m <- sieve_fit_measures_at(r$measures, r$fit, sieve_default_rmsea_conf_level())
      req(m)
      # Each index is named with its family (robust, scaled, or standard),
      # and the RMSEA interval with its level (the RMSEA level).
      df <- sieve_fit_table(m, c(
        "cfi", "rmsea", "rmsea_lo", "rmsea_hi", "srmr", "chisq",
        "df", "pvalue"
      ))
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t", pageLength = 20)
      ) |>
        formatRound("Value", 3)
    })

    output$load_tbl <- renderDT({
      r <- current()
      req(r, r$loadings)
      # Show the display columns only (drop the internal uniqueness_raw), and
      # round the numeric loading columns (group_loading, not the group NAME).
      ld <- r$loadings[, c("item", "group", "general", "group_loading", "uniqueness")]
      sieve_dt(ld,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25),
        colnames = c(
          "Item", "Group factor", "General loading",
          "Group loading", "Uniqueness"
        )
      ) |>
        formatRound(c("general", "group_loading", "uniqueness"), 3)
    })

    output$improper <- renderUI({
      r <- current()
      if (is.null(r) || is.null(r$fit)) {
        return(NULL)
      }
      # Heywood signals, in priority order: the improper flag DMAR's
      # bifactor_indices computes (a standardized loading above one or a
      # negative residual variance), the same check on the local loadings
      # table when the indices are unavailable (reading uniqueness_raw, the
      # unclamped value; the displayed `uniqueness` is floored at zero, so
      # checking it would never detect this case), and lavaan's post.check.
      bad <- isTRUE(r$improper)
      ld <- r$loadings
      if (!isTRUE(bad) && is.na(r$improper %||% NA) && !is.null(ld)) {
        bad <- any(abs(ld$general) > 1, na.rm = TRUE) ||
          any(abs(ld$group_loading) > 1, na.rm = TRUE) ||
          any(ld$uniqueness_raw < 0, na.rm = TRUE)
      }
      # lavaan's post check is read from the status recorded at fit time (its
      # warning was captured there) rather than raised again on every render.
      ok <- !identical(r$status$code, "inadmissible")
      if (bad || isFALSE(ok)) {
        div(
          class = "alert alert-warning",
          tags$b("Improper solution (Heywood case). "),
          "Some standardized loadings exceed one or a residual variance is negative, ",
          "so the bifactor model is over-parameterized for these data (often too many ",
          "or too small group factors). The indices in the table are not trustworthy until the ",
          "model is respecified, for example by merging or dropping weak group factors."
        )
      } else {
        NULL
      }
    })

    # The indices as DMAR returns them (general factor first), with the
    # group rows' quantities named for what they are: in a group row ECV
    # and omega are computed over that group's items (the general factor's
    # share of the group's common variance, and subscale omega), so the
    # group factor's own share is shown beside them. The stored table is
    # unchanged and at full precision.
    output$idx_tbl <- renderDT({
      r <- current()
      req(r, r$indices)
      ix <- r$indices
      df <- data.frame(
        factor = ix$factor, ECV = ix$ECV,
        ECV_group = ifelse(seq_len(nrow(ix)) == 1L, NA_real_, 1 - ix$ECV),
        omega = ix$omega, omega_H = ix$omega_H, omega_HS = ix$omega_HS,
        PUC = ix$PUC, H = ix$H, stringsAsFactors = FALSE
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t", pageLength = 25),
        colnames = c(
          "Factor", "ECV of the general factor (all items, or the group's items)",
          "ECV of the group factor (its items)",
          "Omega (model-implied denominator; total, or subscale in a group row)",
          "omegaH", "omegaHS", "PUC", "H"
        )
      ) |>
        formatRound(c("ECV", "ECV_group", "omega", "omega_H", "omega_HS", "PUC", "H"), 3)
    })

    # The active items as numbers, every row kept. The total score omega and
    # the bootstrap both start from these rows and apply the missing data
    # treatment of the fitted model, so neither changes the analysis sample.
    active_data <- reactive({
      req(state$items, state$meta)
      items <- intersect(state$meta$item[state$meta$status == "active"], names(state$items))
      sieve_reliability_data(state$items, items)
    })

    # The complete rows of the active items (a description of the data; no
    # analysis on this tab is restricted to them unless its model is).
    active_complete <- reactive({
      X <- active_data()
      X[stats::complete.cases(X), , drop = FALSE]
    })

    # Omega of the unweighted total of the active items, on the rows the
    # bifactor model uses (see sieve_total_omega()), computed with the fit.
    total_omega <- reactive({
      r <- current()
      req(r)
      r$total_omega
    })

    # Total-composite omega (all active items as one scale), plus the reading
    # of the general factor's indices as evidence about essential
    # unidimensionality. Each convention is named once, with its source,
    # where its index is defined in the page's More on the indices
    # (Rodriguez, Reise & Haviland, 2016, and the work they cite), and the
    # reading here points there; SIEVER reports where the values stand and
    # applies no cutoff. The total-score
    # line is robust omega of a one-factor model of every item (categorical
    # omega when any item is treated as ordered or binary), on the items as
    # scored. Its observed denominator does not rest on the one-factor model,
    # but its numerator, the squared sum of that model's loadings, does, so
    # it is not the table's omega with another denominator: it differs
    # through its numerator and its item metric, and the note under it says
    # so. Beside it, the fitted bifactor model's own Omega and omegaH with
    # the observed denominator (DMAR::bifactor_indices(denominator =
    # "observed")) differ from the table's row by the denominator alone, so
    # the reader sees how little of the gap the denominator accounts for.
    # It is a point estimate (its interval is bootstrap based and is not
    # computed here), on the rows the bifactor model uses, so the two are not
    # compared across different samples.
    output$essential <- renderUI({
      r <- current()
      req(r, r$indices)
      ov <- r$indices[r$indices$factor == "general", , drop = FALSE]
      req(nrow(ov) == 1L)
      X <- active_data()
      total <- total_omega()
      om_txt <- sieve_total_omega_label(total)
      categorical <- identical(total$coefficient, "omega_categorical")
      fmt <- function(x) if (length(x) == 1L && is.finite(x)) sprintf("%.2f", x) else "not available"
      # The same standardized loadings over the observed (saturated)
      # correlations of the items, polychoric when the fit is categorical.
      general <- names(r$name_map)[r$name_map == "general"]
      observed <- sieve_capture(DMAR::bifactor_indices(r$fit,
        general = if (length(general) == 1L) general else NULL, denominator = "observed"))
      observed_row <- if (observed$ok && is.data.frame(observed$value)) {
        observed$value[observed$value$factor %in% general, , drop = FALSE]
      }
      observed_txt <- if (!is.null(observed_row) && nrow(observed_row) == 1L) {
        sprintf(
          paste0(
            "Omega (observed denominator, total) = %s, omegaH (general) = %s: the bifactor ",
            "model's standardized loadings over the observed %scorrelations of the items ",
            "(DMAR::bifactor_indices(denominator = \"observed\")), so they differ from the ",
            "table's row by the denominator alone. "
          ),
          fmt(observed_row$omega), fmt(observed_row$omega_H), if (categorical) "polychoric " else ""
        )
      } else {
        sprintf("The bifactor model's omega with the observed denominator is not available (%s). ",
          if (observed$ok) "DMAR returned no row for the general factor" else gsub("\\s+", " ", observed$error))
      }
      # The raw total score is an UNWEIGHTED sum of the items as they are
      # scored, and its omega has the one-factor model's numerator, so it can
      # sit well below the bifactor omega in the table above it (which counts
      # the group factors' variance and is computed from standardized
      # loadings) whatever the item standard deviations. That is correct
      # arithmetic about two different coefficients, but adjacent numbers
      # invite the wrong reading, so the note is always shown; when the item
      # standard deviations differ widely it adds that the widest-scaled item
      # dominates the total.
      sds <- vapply(X, function(v) stats::sd(v, na.rm = TRUE), numeric(1))
      sds <- sds[is.finite(sds) & sds > 0]
      sd_ratio <- if (length(sds) >= 2L) max(sds) / min(sds) else 1
      scale_note <- tags$div(
        class = "small mt-1",
        tags$b("Note on the two omegas. "),
        paste0(
          "The omega of the raw total score is not the table's Omega (model-implied denominator) ",
          "with another denominator. Its numerator is the squared sum of the loadings of a one-factor ",
          "model of every item, so group-factor variance the single factor does not absorb counts as ",
          "error, where the table's Omega counts the general and the group factors alike. ",
          if (categorical) {
            paste0(
              "It is categorical omega for the sum of the categories as scored, where the table's ",
              "Omega comes from standardized loadings and describes the sum of the latent responses. "
            )
          } else {
            paste0(
              "It is computed on the items as scored, where the table's Omega comes from standardized ",
              "loadings and describes the total of the standardized items. "
            )
          },
          sprintf("The item standard deviations differ by a factor of %.1f across this pool%s. ", sd_ratio,
            if (sd_ratio > 2) ", so the unweighted raw total is dominated by the widest-scaled item" else ""),
          "So the two differ through the numerator and the item metric, not only the denominator; ",
          "under a bifactor structure the omega of the raw total usually falls between the omegaH and ",
          "the omega of that same total. Both are correct about different composites. If a total score ",
          "will be reported, read the omega of the composite you will actually use; standardizing the ",
          "items first removes the difference in metric but not the one in the numerator."
        )
      )
      # The values first; how to read them against the conventions in use
      # is one click away.
      div(
        class = "alert alert-secondary py-2 mt-2 mb-2 small",
        strong("Omega of the raw total score under a one-factor model of every item: "), om_txt, tags$br(),
        strong("The bifactor model with the observed denominator: "), observed_txt, tags$br(),
        sprintf(
          "ECV = %s, PUC = %s, omegaH (general) = %s, Omega (model-implied denominator, total) = %s, H (general) = %s. ",
          fmt(ov$ECV), fmt(ov$PUC), fmt(ov$omega_H), fmt(ov$omega), fmt(ov$H)
        ),
        scale_note,
        tags$details(open = NA,
          class = "sieve-intro mb-0",
          tags$summary("How to read them (conventions, not a verdict)"),
          paste0(
          "No cutoff for essential unidimensionality is established. ECV is read together with ",
          "PUC, and omegaH against omega, as the share of total score variance the general ",
          "factor alone accounts for; the conventions in use for ECV with PUC, for omegaH ",
          "against omega and for H are each named once, with its source, where the index is ",
          "defined under More on the indices below. ",
          "Where this scale's values stand, together with the subscale omegaHS and H values ",
          "below, the bootstrap intervals, the fit of the model, and the use the scores will be ",
          "put to, is for the researcher to judge; SIEVER does not decide it."
          )
        )
      )
    })

    # ---- Bootstrap intervals for the indices -------------------------------
    # The calculations are in fct_bifactor_boot.R: the plan (the rows the
    # fitted model used, its specification, its indices) and DMAR's counted
    # bootstrap, DMAR::ci_bifactor_indices() on the fit on screen. The DMAR
    # call runs in a background R process (fct_dmar_boot.R), so the page
    # stays responsive: the page polls the progress DMAR's hook writes, and
    # Stop asks DMAR to cancel at its next check (after the replication in
    # progress, or after the block in progress on several processes; the
    # process is ended if DMAR runs past a grace period). With a declared cluster
    # design SIEVER's own aligned bootstrap resamples whole clusters, in
    # short batches in this process with the session serviced between them.
    # The interval level is 95 percent.
    idx_conf_level <- reactive(sieve_finite_or(input$conf_level, .95))
    boot_result <- reactiveVal(NULL) # the last bootstrap, or why there is none
    boot_running <- reactiveVal(FALSE)
    job <- new.env(parent = emptyenv()) # the run in progress (plain state)
    session$onSessionEnded(function() {
      if (!is.null(job$handle)) job$handle$kill()
    })
    idx_request <- reactive({
      state$sampling_design
      list(model = bf_key(), sampling = sieve_page_revision(state)$sampling,
        B = sieve_finite_or(input$idx_B, 1000), seed = sieve_seed_input(input$idx_seed),
        conf_level = idx_conf_level())
    })

    # The time of one draw, for the wait estimate shown before a run: the
    # time lavaan recorded for the fit on screen plus the time to read it.
    draw_seconds <- reactive({
      r <- current()
      req(r, r$fit)
      sieve_bifactor_draw_seconds(r)
    })

    output$idx_plan_note <- renderUI({
      r <- current()
      req(r, r$fit, !isTRUE(boot_running()))
      B <- sieve_finite_or(input$idx_B, 1000)
      clustered <- !is.null(r$spec$engine_args$cluster)
      request <- if (!clustered) sieve_dmar_boot_request(B, conf_level = idx_conf_level()) else NULL
      if (!is.null(request) && !request$ok) {
        return(helpText(class = "text-danger", request$message))
      }
      req(B >= 1, B == floor(B))
      secs <- tryCatch(draw_seconds(), error = function(e) NA_real_)
      mc <- sieve_bootstrap_mc_error(c(B, 400, 10000), idx_conf_level())
      # The processes the background run asks DMAR for; it decides there
      # whether it may fork (sieve_dmar_boot_cores()).
      ncpus <- sieve_dmar_boot_cores()
      serial <- sieve_bootstrap_wait(secs, B, NA, "percentile")
      range <- sieve_dmar_boot_wait_range(serial, ncpus)
      helpText(
        class = "small text-muted",
        if (clustered) {
          sprintf(
            paste0(
              "Each replication refits the bifactor model, so %d replications should take %s ",
              "on this computer (from the time of one fit). With the declared cluster design SIEVER ",
              "resamples whole clusters with its own bootstrap (DMAR's resamples independent rows). ",
              "Progress is shown as replications finish and the run can be stopped; those completed ",
              "by then are kept. "
            ),
            as.integer(B), sieve_duration_label(serial)
          )
        } else {
          paste0(
            if (ncpus > 1L) {
              sprintf(
                paste0(
                  "Each replication refits the bifactor model, so %d replications should take %s on ",
                  "this computer, from the time of one fit: the shorter if the replications spread ",
                  "evenly over %d R processes, the longer if a few resamples are slow (a refit that fails ",
                  "or a solution that runs away), since DMAR completes the replications in blocks and ",
                  "each block waits for its slowest. The progress bar estimates the time left from the ",
                  "rate so far. "
                ),
                as.integer(B), sieve_dmar_wait_range_label(range), ncpus
              )
            } else {
              sprintf(
                paste0(
                  "Each replication refits the bifactor model, so %d replications should take %s ",
                  "on this computer (from the time of one fit, in one R process; a resample whose ",
                  "solution runs away takes longer). "
                ),
                as.integer(B), sieve_duration_label(range[["low"]])
              )
            },
            paste0(
              "DMAR's counted bootstrap (DMAR::ci_bifactor_indices()) runs in a background R process, ",
              "so this page stays responsive; progress is shown as replications finish, and Stop ",
              "cancels the bootstrap at the next replication, or on several processes at the end of ",
              "the block of replications in progress (if DMAR has not stopped five seconds after the ",
              "press, the background process is ended). DMAR forms no interval from a canceled run, so ",
              "a canceled run shows none. "
            )
          )
        },
        sprintf(
          paste0(
            "The number of replications sets how reproducible the limits are, not what they ",
            "estimate: at B = %d each percentile limit carries a Monte Carlo standard error ",
            "of about %.2f bootstrap standard errors, against %.2f at B = 400 and %.2f at ",
            "B = 10,000. DMAR's minimum requested count depends on the confidence level; ",
            "meeting it does not guarantee usable draws or adequate Monte Carlo precision. ",
            "An empty seed is drawn for the run and recorded, so the run can be repeated ",
            "exactly."
          ),
          as.integer(B), mc[1], mc[2], mc[3]
        )
      )
    })

    output$idx_stop_ui <- renderUI({
      req(isTRUE(boot_running()))
      actionButton(session$ns("idx_stop"),
        if (identical(job$mode, "dmar")) "Cancel the bootstrap" else "Cancel after the current replication",
        class = "btn-outline-secondary"
      )
    })

    close_progress <- function() {
      if (!is.null(job$progress)) {
        job$progress$close()
        job$progress <- NULL
      }
      if (!is.null(job$handle)) {
        job$handle$release()
        job$handle <- NULL
      }
    }

    # Record a finished bootstrap and put it on screen.
    record_boot <- function(result) {
      result$revision <- job$revision
      result$request <- job$request
      cnt <- result$counts
      engine <- if (identical(result$engine, "dmar")) {
        sprintf("DMAR's counted bootstrap, DMAR::ci_bifactor_indices(), %d process%s",
          as.integer(result$ncpus %||% 1L), if (identical(as.integer(result$ncpus %||% 1L), 1L)) "" else "es")
      } else {
        "SIEVER's whole-cluster bootstrap"
      }
      sieve_log(
        state,
        sprintf(
          paste0(
            "Bifactor indices: nonparametric bootstrap interval (percentile, B = %d, seed = %d%s; %s). ",
            "In the session: %d replications attempted%s, %d converged, %d converged and ",
            "admissible, %d used."
          ),
          cnt$requested, result$uncertainty$rng$seed,
          if (isTRUE(result$uncertainty$rng$supplied)) "" else ", drawn for this run",
          engine,
          cnt$attempted,
          if (isTRUE(result$complete)) "" else sprintf(" of %d requested (stopped)", cnt$requested),
          cnt$converged, cnt$admissible, cnt$used
        ),
        result$code,
        inputs = job$replay_inputs
      )
      # The record a disclosure under the intervals reads: the step just
      # written, with the run's counts.
      result$code_view <- sieve_code_snapshot(state, context = c(
        sprintf("Percentile bootstrap, B = %d, seed = %d%s; %s.", cnt$requested, result$uncertainty$rng$seed,
          if (isTRUE(result$uncertainty$rng$supplied)) "" else " (drawn for this run)", engine),
        sprintf("Replications: %d attempted, %d converged, %d converged and admissible, %d used.",
          cnt$attempted, cnt$converged, cnt$admissible, cnt$used)
      ))
      boot_result(result)
    }

    # Close a run: summarize it, record the call, and put the result on
    # screen. A DMAR run stopped part way has no interval: DMAR forms its
    # limits and counts from all B replications.
    finish_boot <- function() {
      if (identical(job$mode, "dmar")) {
        run <- if (is.null(job$handle)) NULL else job$handle$results[[1]]
        close_progress()
        boot_running(FALSE)
        if (is.null(run) || identical(sieve_dmar_boot_outcome(run), "cancelled")) {
          done <- run$condition$done %||% job$done %||% 0L
          boot_result(list(notice = sprintf(paste0(
            "The bootstrap was canceled after %d of %d replications. DMAR forms no interval from a ",
            "canceled run (its limits and counts are those of all B replications), so there is ",
            "nothing to show. Press Bootstrap index CIs to run it again."
          ), as.integer(done), job$plan$B)))
          return(invisible(NULL))
        }
        result <- sieve_bifactor_boot_from_dmar(job$plan, run)
        if (!identical(sieve_dmar_boot_outcome(run), "ok")) {
          boot_result(list(error = result$uncertainty$reason, condition_class = run$error_class))
          return(invisible(NULL))
        }
        result$uncertainty$seconds <- proc.time()[["elapsed"]] - job$started
        record_boot(result)
        return(invisible(NULL))
      }
      if (job$done < 1L) {
        # Stopped before the first replication finished: nothing to report.
        close_progress()
        boot_running(FALSE)
        boot_result(list(notice = paste0(
          "The bootstrap was stopped before a replication finished, so there is ",
          "nothing to show. Press Bootstrap index CIs to run it."
        )))
        return(invisible(NULL))
      }
      done <- job$draws[seq_len(job$done)]
      result <- sieve_bifactor_boot_summary(
        job$plan, done,
        seconds = proc.time()[["elapsed"]] - job$started
      )
      close_progress()
      boot_running(FALSE)
      record_boot(result)
    }

    fail_boot <- function(error) {
      if (!is.null(job$handle)) {
        job$handle$kill()
        job$handle <- NULL
      }
      close_progress()
      boot_running(FALSE)
      boot_result(list(error = paste("The bootstrap stopped with an error:", error)))
    }

    observeEvent(input$idx_boot, {
      if (isTRUE(boot_running())) {
        return()
      }
      problem <- sieve_sampling_problem(state, capability = "cluster")
      if (!is.null(problem)) {
        boot_result(list(error = problem, refused = TRUE, sampling = sieve_sampling_spec(state)))
        return()
      }
      r <- sieve_outcome(shown())$value
      if (!is.null(r) && !is.null(r$stale)) {
        boot_result(list(error = sprintf(paste0(
          "The data or the item ledger changed after the model shown here was fitted (%s). ",
          "Fit the bifactor model again, then bootstrap."
        ), r$stale)))
        return()
      }
      if (is.null(r) || is.null(r$fit)) {
        boot_result(list(error = "Fit the bifactor model first."))
        return()
      }
      B <- sieve_finite_or(input$idx_B, 1000)
      if (B < 1 || B != floor(B)) {
        boot_result(list(error = "The number of bootstrap resamples must be a positive whole number."))
        return()
      }
      if (is.null(r$spec$engine_args$cluster)) {
        request <- sieve_dmar_boot_request(B, conf_level = idx_conf_level())
        if (!request$ok) {
          boot_result(list(error = request$message))
          return()
        }
      }
      estimator <- estimator_choice()
      aux <- r$spec$engine_args$aux
      plan <- sieve_bifactor_boot_plan(
        r$data %||% state$items, state$meta,
        B = B, seed = sieve_seed_input(input$idx_seed),
        conf_level = idx_conf_level(), original_rows = r$bootstrap_row_index,
        cluster = if (!is.null(r$spec$engine_args$cluster)) r$data[[r$spec$engine_args$cluster]],
        fit_fun = function(data, meta) sieve_fit_bifactor(data, meta, estimator = estimator, aux = aux)
      )
      revision <- sieve_revision(state)
      # The intervals belong to the estimates on screen. The plan refits the
      # model from the current data and ledger; if that is not the fit on
      # screen, the data or the ledger changed after it was fitted.
      shown <- sieve_bifactor_boot_read(r)
      if (!isTRUE(all.equal(plan$point$values, shown$values, tolerance = 1e-8))) {
        boot_result(list(error = paste0(
          "The data or the item ledger changed after the model shown here was fitted. ",
          "Fit the bifactor model again, then bootstrap."
        )))
        return()
      }
      if (!identical(plan$status$code, "ready")) {
        result <- sieve_bifactor_boot_summary(plan)
        result$revision <- revision
        result$request <- idx_request()
        boot_result(result)
        return()
      }
      job$plan <- plan
      job$mode <- plan$engine
      job$handle <- NULL
      job$draws <- vector("list", plan$B)
      job$done <- 0L
      job$stop <- FALSE
      job$primed <- FALSE
      job$revision <- revision
      job$request <- idx_request()
      job$replay_inputs <- sieve_replay_inputs(state)
      job$started <- proc.time()[["elapsed"]]
      job$ncpus <- sieve_dmar_boot_cores()
      wait <- if (identical(job$mode, "dmar")) {
        sieve_dmar_wait_range_label(sieve_dmar_boot_wait_range(
          sieve_bootstrap_wait(plan$seconds_per_fit, plan$B, NA, "percentile"), job$ncpus
        ))
      } else {
        sieve_duration_label(sieve_bootstrap_wait(plan$seconds_per_fit, plan$B, NA, "percentile"))
      }
      job$progress <- shiny::Progress$new(session, min = 0, max = plan$B)
      job$progress$set(
        value = 0, message = "Bootstrapping the bifactor indices",
        detail = sprintf("0 of %d replications; %s remaining", plan$B, wait)
      )
      boot_running(TRUE)
    })

    observeEvent(input$idx_stop, {
      if (isTRUE(boot_running())) {
        job$stop <- TRUE
        if (!is.null(job$handle)) job$handle$stop()
      }
    })
    observe({
      if (isTRUE(boot_running()) && !identical(job$request, tryCatch(idx_request(), error = function(e) NULL))) {
        job$stop <- TRUE
        if (!is.null(job$handle)) job$handle$stop()
      }
    })

    # DMAR's run: the first pass starts nothing, so the page shows the run
    # (the progress bar, the Stop button); the next starts the background
    # process; then each pass reads its progress until it ends.
    sieve_chunked_wire(function() isTRUE(boot_running()) && identical(job$mode, "dmar"), session, function() {
      if (!isTRUE(job$primed)) {
        job$primed <- TRUE
        return(invisible(NULL))
      }
      outcome <- sieve_capture({
        if (is.null(job$handle) && isTRUE(job$stop)) {
          # Stopped before anything started: nothing to run or record.
          finish_boot()
          return(invisible(NULL))
        }
        if (is.null(job$handle)) {
          job$handle <- sieve_dmar_boot_start(list(sieve_bifactor_boot_dmar_call(job$plan, ncpus = job$ncpus)))
        }
        handle <- job$handle
        alive <- handle$poll()
        job$done <- as.integer(handle$progress$done)
        elapsed <- proc.time()[["elapsed"]] - job$started
        job$progress$set(
          value = job$done,
          detail = sprintf(
            "%d of %d replications (DMAR, %d process%s)%s", job$done, job$plan$B, job$ncpus,
            if (job$ncpus == 1L) "" else "es",
            if (isTRUE(job$stop)) {
              "; stopping"
            } else if (job$done > 0L && job$done < job$plan$B) {
              sprintf("; %s remaining", sieve_duration_label(elapsed / job$done * (job$plan$B - job$done)))
            } else {
              ""
            }
          )
        )
        if (alive && !is.na(handle$stop_requested) &&
          proc.time()[["elapsed"]] - handle$stop_requested > sieve_dmar_boot_grace_seconds()) {
          handle$process$kill()
          alive <- FALSE
        }
        if (!alive) {
          handle$poll()
          failure <- handle$failure()
          if (!is.null(failure)) stop(failure, call. = FALSE)
          finish_boot()
        }
      })
      if (!outcome$ok) fail_boot(outcome$error)
    }, interval = 200L)

    # SIEVER's whole-cluster run: one batch of draws per pass, about half a
    # second of fitting (always at least one draw), then the session is
    # serviced before the next batch. The first pass fits nothing, so the
    # page shows the run (the progress bar, the Stop button) before the
    # first fit starts.
    sieve_chunked_wire(function() isTRUE(boot_running()) && identical(job$mode, "siever"), session, function() {
      if (!isTRUE(job$primed)) {
        job$primed <- TRUE
        return(invisible(NULL))
      }
      outcome <- sieve_capture({
        began <- proc.time()[["elapsed"]]
        while (!isTRUE(job$stop) && job$done < job$plan$B) {
          b <- job$done + 1L
          job$draws[[b]] <- sieve_bifactor_boot_draw(job$plan, b)
          job$done <- b
          if (proc.time()[["elapsed"]] - began > 0.5) {
            break
          }
        }
        elapsed <- proc.time()[["elapsed"]] - job$started
        not_used <- sum(!vapply(job$draws[seq_len(job$done)], function(d) d$used, logical(1)))
        job$progress$set(
          value = job$done,
          detail = sprintf(
            "%d of %d replications%s%s", job$done, job$plan$B,
            if (job$done < job$plan$B) {
              sprintf("; %s remaining", sieve_duration_label(
                elapsed / max(job$done, 1L) * (job$plan$B - job$done)
              ))
            } else {
              ""
            },
            if (not_used > 0L) sprintf("; %d not usable so far", not_used) else ""
          )
        )
        if (isTRUE(job$stop) || job$done >= job$plan$B) {
          finish_boot()
        }
      })
      if (!outcome$ok) fail_boot(outcome$error)
    })

    # The bootstrap on screen. It describes one analysis, so it is shown only
    # while the data and the ledger are the ones it was run on.
    idx_ci <- reactive({
      r <- boot_result()
      if (is.null(r) || !is.null(r$error) || !is.null(r$notice)) {
        return(r)
      }
      request <- tryCatch(idx_request(), error = function(e) NULL)
      if (!identical(r$request, request)) {
        return(list(error = paste0(
          "The data, respondent rows, estimator, sampling design, or bootstrap settings changed after these intervals were run. ",
          "Use a current bifactor model, then press Bootstrap index CIs for the current request."
        ), stale = TRUE))
      }
      r
    })
    observe({
      stored <- boot_result()
      running <- boot_running()
      if (is.null(stored) && !running) return()
      r <- idx_ci()
      phase <- if (running) "running" else if (isTRUE(r$stale)) "stale" else if (
        !is.null(r$error) || !is.null(r$notice)) "failed" else "current"
      sieve_page_publish(state, "bifactor", session$ns("index_intervals"), "Bifactor bootstrap intervals",
        list(state = phase, sentence = r$error %||% r$notice, key = stored$request %||% job$request),
        "Bootstrap index CIs")
    })

    output$idx_ci_note <- renderUI({
      if (isTRUE(boot_running())) {
        return(div(
          class = "text-muted small",
          "The bootstrap is running; the progress bar shows the replications completed."
        ))
      }
      r <- idx_ci()
      if (is.null(r)) {
        return(div(
          class = "text-muted small",
          "Press Bootstrap index CIs to estimate uncertainty for the indices."
        ))
      }
      if (!is.null(r$error)) {
        return(div(class = "alert alert-danger", r$error))
      }
      if (!is.null(r$notice)) {
        return(div(class = "text-muted small", r$notice))
      }
      if (identical(r$uncertainty$status, "unavailable")) {
        return(div(
          class = "alert alert-warning",
          tags$b("No bootstrap interval: "), r$uncertainty$reason
        ))
      }
      said <- sieve_bifactor_boot_notes(r)
      tagList(
        if (identical(r$uncertainty$status, "failed")) {
          div(
            class = "alert alert-warning",
            tags$b("No bootstrap interval: "), r$uncertainty$reason
          )
        },
        div(class = "small mb-1", said$method),
        div(
          class = "small mb-1", tags$b(said$counts),
          if (length(said$reasons)) paste0(" Not used: ", paste(said$reasons, collapse = "; "), ".")
        ),
        if (!is.null(said$stopped)) div(class = "small mb-1", said$stopped),
        if (!is.null(said$caution)) {
          div(
            class = if (isTRUE(r$caution)) "alert alert-warning small" else "small mb-1",
            tags$b(if (isTRUE(r$caution)) "Read the limits as conditional on a successful fit. " else NULL),
            said$caution,
            if (nrow(r$reasons)) {
              tags$ul(class = "mb-0", lapply(seq_len(min(5L, nrow(r$reasons))), function(i) {
                tags$li(sprintf(
                  "%d replication%s: %s", r$reasons$draws[i],
                  if (r$reasons$draws[i] == 1L) "" else "s", r$reasons$message[i]
                ))
              }))
            }
          )
        },
        div(class = "text-muted small mb-1", said$precision),
        div(class = "text-muted small mb-2", tags$b("Rule: "), said$rule)
      )
    })

    # Display names of the indices (the stored table keeps the engine's).
    # Given the factor of each row, ECV and omega in a group factor's row
    # are named for what they are there (see output$idx_tbl).
    idx_label <- function(x, factor = NULL) {
      shown <- c(
        ECV = "ECV", omega = "Omega (model-implied denominator)", omega_H = "omegaH", omega_HS = "omegaHS",
        PUC = "PUC", H = "H"
      )
      out <- unname(ifelse(x %in% names(shown), shown[x], x))
      if (!is.null(factor)) {
        group <- factor != "general"
        out[group & x == "ECV"] <- "ECV of the general factor within the group's items"
        out[group & x == "omega"] <- "Omega (model-implied denominator; subscale)"
      }
      out
    }

    output$idx_ci_tbl <- renderDT({
      r <- idx_ci()
      req(r, is.null(r$error), r$table)
      df <- r$table[, c("factor", "index", "estimate", "lower", "upper", "interval", "used")]
      df$index <- idx_label(df$index, df$factor)
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25),
        colnames = c(
          "Factor", "Index", "Estimate", "CI lower", "CI upper",
          "Interval", "Replications used"
        )
      ) |>
        formatRound(c("estimate", "lower", "upper"), 3)
    })

    # How far each limit could move had the unusable draws produced values:
    # shown only when some draws could not be used.
    output$idx_ci_range <- renderUI({
      r <- idx_ci()
      req(
        r, is.null(r$error), r$table, identical(r$uncertainty$status, "completed"),
        r$counts$attempted > r$counts$used
      )
      tagList(
        tags$p(
          class = "small mt-2 mb-1",
          tags$b("How far the limits could move. "),
          "Had every replication that could not be used produced a value, each limit ",
          "would be a quantile of all the replications attempted. Where those values ",
          "would have fallen is unknown; the two columns for each limit give it when all ",
          "of them fall below, and when all of them fall above, the ones that were used."
        ),
        DTOutput(session$ns("idx_ci_range_tbl"), fill = FALSE)
      )
    })

    output$idx_ci_range_tbl <- renderDT({
      r <- idx_ci()
      req(r, is.null(r$error), r$table)
      df <- r$table[
        r$table$status == "completed",
        c("factor", "index", "lower_min", "lower_max", "upper_min", "upper_max")
      ]
      df$index <- idx_label(df$index, df$factor)
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25),
        colnames = c(
          "Factor", "Index", "CI lower, at least", "CI lower, at most",
          "CI upper, at least", "CI upper, at most"
        )
      ) |>
        formatRound(c("lower_min", "lower_max", "upper_min", "upper_max"), 3)
    })

    # ---- One Update for the page ------------------------------------------
    # The bifactor model is the tab's one result fit on request; the
    # bootstrap intervals keep their own button, and are named in the line
    # when the model they were run on is from before.
    boot_entry <- function() {
      if (isTRUE(demand$status()$withheld)) return(NULL)
      r <- boot_result()
      if (is.null(r) || !is.null(r$error) || !is.null(r$notice) || isTRUE(boot_running())) {
        return(NULL)
      }
      if (identical(r$request$model, tryCatch(bf_key(), error = function(e) NULL)) &&
        identical(demand$status()$state, "current")) {
        return(NULL)
      }
      sieve_update_entry("Bifactor bootstrap intervals", "stale",
        update = FALSE, button = "Bootstrap index CIs", why = "a bootstrap"
      )
    }
    page_update <- sieve_update_wire(input, session, function() {
      list(
        bifactor = sieve_update_demand_entry(demand, "Bifactor model", bf_plan),
        intervals = boot_entry()
      )
    })

    # ---- Show code at the point of use ------------------------------------
    # The record kept with the held fit (compute_bifactor()) under the
    # demand status, and the record kept with the bootstrap (finish_boot())
    # under the intervals' own currency. Reading a panel evaluates nothing.
    sieve_code_navigation(input, session, state)
    output$bf_code <- renderUI({
      st <- demand$status()
      sieve_code_view(st$value$code_view, st, ns = session$ns,
        title = "Show code: bifactor model and indices",
        readout = c(
          'bifactor_std <- lavaan::standardizedSolution(bifactor_fit, type = "std.all")',
          'bifactor_std[bifactor_std$op == "=~", ]  # general and group loadings behind ECV, omegaH, omegaHS, PUC and H'
        ))
    })
    output$boot_code <- renderUI({
      stored <- boot_result()
      r <- idx_ci()
      status <- if (is.null(stored)) {
        list(state = "not_run")
      } else if (isTRUE(boot_running())) {
        list(state = "not_run")
      } else if (isTRUE(r$stale)) {
        list(state = "stale", sentence = r$error)
      } else if (!is.null(stored$error) || !is.null(stored$notice)) {
        list(state = "failed", sentence = stored$error %||% stored$notice)
      } else {
        list(state = "current")
      }
      sieve_code_view(stored$code_view, status, ns = session$ns,
        title = "Show code: bootstrap intervals for the indices")
    })
  })
}

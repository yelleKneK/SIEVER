# mod_cfa.R
# Confirmatory factor analysis and the model path diagram. Fits the
# correlated-factors measurement model implied by the active ledger and
# shows it three ways: headline fit (with the RMSEA confidence interval,
# per the AIPE emphasis), a full path diagram in the classroom notation
# (loadings, unique terms with their variances, and factor covariances; see
# fct_diagram_controls.R and fct_diagram.R), and the inter-factor correlations with
# confidence intervals.

#' CFA and SEM module UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput plotOutput radioButtons checkboxInput selectInput downloadButton
#' @importFrom shiny actionLink
#' @importFrom bslib layout_sidebar sidebar card card_header navset_card_tab nav_panel
#' @importFrom bslib layout_columns value_box
#' @importFrom DT DTOutput
mod_cfa_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      open = "desktop",
      width = 300,
      title = "Model and diagram",
      radioButtons(ns("scope"), "CFA scope",
        choices = c("Constructs" = "construct", "Facets (direct first-order CFA)" = "facet"),
        selected = "construct"
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'facet'", ns("scope")),
        checkboxInput(ns("all_facets"), "Model all assigned facets", value = TRUE),
        selectInput(ns("facets"), "Facets to model when the box is cleared (construct: facet)",
          choices = character(0),
          multiple = TRUE, width = "100%"
        ),
        helpText(
          "Selected facets are freely correlated first-order factors. Select one to inspect it alone.",
          " A single factor needs at least three indicators without additional constraints;",
          " no equal-loading constraint is added to identify a small facet."
        )
      ),
      radioButtons(
        ns("structure"), "Model structure",
        choices = c(
          "First-order (correlated factors)" = "first",
          "Higher-order (construct to facet to items)" = "higher"
        ),
        selected = "first"
      ),
      radioButtons(
        ns("ident"), "Identification",
        choices = c(
          "Factor variance = 1 (free loadings)" = "varone",
          "Marker indicator = 1" = "marker"
        ),
        selected = "varone"
      ),
      # The estimator is a choice:
      # the default follows the item types, and the model note above the
      # tabs states what was resolved and why (sieve_estimator_resolve()).
      sieve_estimator_panel(ns),
      checkboxInput(ns("two_stage"), "Robust Two-Stage ML (Sensitivity Analysis)", FALSE),
      helpText("Optional for incomplete continuous data. The fit uses the EM moments and corrected two-stage standard errors with a Satorra-Bentler test. ML with FIML (DMAR's default) remains the default for continuous items."),
      # Conditional guidance, not a blanket rule: each
      # estimator is named with the condition under which it is the one to
      # use, and the citation that supports it.
      sieve_more_below(
        "The default follows the study setting and item types. The method line states the estimator and missing-data treatment used.",
        p(
          "Which estimator, and when. ML with FIML (full information maximum likelihood) is the ",
          "default for continuous items (DMAR's default) and the default treatment of incomplete cases: ",
          "every respondent contributes the items they ",
          "answered, and the estimates are unbiased and more efficient than those from the complete ",
          "cases when the missingness depends on observed values, that is, missing at random ",
          "(Enders & Bandalos, 2001). Its standard errors and test statistic are normal theory. MLR ",
          "with FIML keeps the same estimates and adds Huber-White (sandwich) standard errors and a ",
          "scaled test statistic, and is the choice for markedly non-normal items (strongly skewed or ",
          "heavy-tailed): under non-normality the normal-theory chi square and RMSEA tend to be ",
          "inflated and the standard errors understated (Yuan & Bentler, 2000). SIEVER does not switch ",
          "to it on its own; the Estimator control above makes the choice. The sandwich ",
          "needs more respondents than free parameters, the condition the refusal above states when it ",
          "is not met. ML on the complete cases is the comparison to run when the missing-at-random ",
          "assumption is in doubt, so the two sets of estimates can be set side by side; it is not a ",
          "small-sample rule, and it discards every partly answered respondent. WLSMV estimates a ",
          "threshold per category boundary and the polychoric correlations of every pair, on the ",
          "complete cases, and its weight matrix grows with the square of their number: the model note ",
          "states its size, and a fit above the limit is refused before it starts. For ordered items the ",
          "choice between the two treatments follows Rhemtulla, Brosseau-Liard and Savalei (2012): with ",
          "five or more categories and approximately symmetric thresholds the continuous treatment ",
          "(ML, MLR) performs about as well as the categorical one; with fewer categories, or with ",
          "asymmetric thresholds, the categorical treatment (WLSMV) does better, and stays the better ",
          "choice up to seven categories under asymmetry. The continuous treatment of ordered items is ",
          "a different model, with attenuated loadings; the categorical treatment of an item with many ",
          "distinct values is not meant for it (set the types on the Ingest tab instead). The choice ",
          "applies to the model fit here and is recorded with it; a fit under another choice is kept and ",
          "shown again when that choice is selected again."
        ),
        label = "Which estimator, and when"
      ),
    ),
    # Results first: the one status line
    # (with the page's Update when a result is from before an edit), then the
    # one-line method statement, then the tabs, whose first view is the
    # fitted model. The model note's explanation is below the diagram.
    uiOutput(ns("gate")),
    uiOutput(ns("method_line")),
    # The tab opens on the fitted model (the path diagram); the
    # specification is a later sub-tab, so the first view is a result and
    # not a page of syntax and assumptions.
    navset_card_tab(
      nav_panel(
        "Path diagram",
        card(
          full_screen = TRUE,
          card_header("Measurement model with loadings, unique terms, and factor covariances"),
          uiOutput(ns("overview_fit")),
          sieve_diagram_panel(ns, kind = "cfa"),
          # How an item leaves this model: the decision is made beside the
          # item results. The pointer sits below the diagram, so that the
          # diagram is the first thing under the fit line.
          p(
            class = "small text-muted mt-2 mb-1 sieve-item-decision-pointer",
            "To leave an item out of this model and of every other analysis of the study, select it on the ",
            "Estimates sub-tab and press Exclude or include items; nothing is deleted, and the item can be included again."
          ),
          # The explanation, below the result: the model note visible, and
          # how to read the diagram in a disclosure open by default.
          tags$h6(class = "mt-3", "About this model"),
          uiOutput(ns("model_note")),
          tags$details(open = NA,
            class = "sieve-intro",
            tags$summary("How to read this diagram"),
            p(
              class = "text-muted small",
              "Each ellipse is a latent factor, each rectangle an item, and each small circle the ",
              "unique (error) term of the item above it. A single-headed arrow from a factor to ",
              "an item is a loading. A double-headed loop is a variance. A curved double-headed ",
              "arrow between two factors is their covariance (their correlation in a standardized ",
              "display); one is drawn only where the model has that parameter. A dashed line ",
              "marks a parameter that was fixed to set a scale, whatever value it shows. The ",
              "diagram appearance controls change what is drawn without refitting the model."
            ),
          )
        )
      ),
      nav_panel(
        "Fit",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box(sprintf("RMSEA (%s CI)", sieve_ci_percent(sieve_default_rmsea_conf_level())),
            uiOutput(ns("vb_rmsea")), theme = "primary"),
          value_box("CFI", uiOutput(ns("vb_cfi")), theme = "secondary"),
          value_box("TLI", uiOutput(ns("vb_tli")), theme = "secondary"),
          value_box("SRMR", uiOutput(ns("vb_srmr")), theme = "success")
        ),
        uiOutput(ns("fit_family")),
        uiOutput(ns("usrmr_action")),
        card(card_header("Model test and fit indices"), DTOutput(ns("fit_tbl")), uiOutput(ns("fit_code"))),
        card(
          card_header("Every family of the test and the indices, side by side"),
          p(
            class = "small",
            "lavaan reports the model test, CFI, TLI, and RMSEA in up to three families, ",
            "which are different statistics and not three roundings of one: ",
            strong("standard"), " (from the uncorrected test statistic), ",
            strong("scaled"), " (the scaled test statistic put into the usual formulas), and ",
            strong("robust"), " (corrected so that the index estimates the population value ",
            "its normal theory counterpart estimates). The boxes and the table above show one ",
            "family, named beside every value; all three are here so that the choice is yours ",
            "to review. An empty cell is a value lavaan does not report for this model, and ",
            "the reason is given below the table."
          ),
          uiOutput(ns("two_stage_test")),
          DTOutput(ns("fit_fam_tbl")),
          uiOutput(ns("fit_fam_why"))
        ),
        card(
          card_header("How to read these fit indices"),
          tags$ul(
            class = "small mb-0",
            tags$li(
              strong("Model chi square"), ": the test of exact fit. With realistic sample sizes ",
              "it rejects nearly every useful model, so it is reported but rarely decisive on its own."
            ),
            tags$li(
              strong("RMSEA"), " (root mean square error of approximation): misfit per degree of ",
              "freedom, read with its ",
              strong(sprintf("%s confidence interval", sieve_ci_percent(sieve_default_rmsea_conf_level()))),
              " in the AIPE spirit ",
              "(Browne & Cudeck, 1993; MacCallum, Browne & Sugawara, 1996). The interval is at ",
              sieve_ci_percent(sieve_default_rmsea_conf_level()), ", the level of every RMSEA interval SIEVER ",
              "shows (90% by default, DMAR's default for the RMSEA interval and lavaan's): the two limits of a ",
              "90% interval settle the one-sided tests of close and of not close fit at .05 ",
              "(MacCallum, Browne & Sugawara, 1996). The interval of the unbiased SRMR (below) is at the same ",
              "level, and the page's other intervals are at ",
              sieve_ci_percent(sieve_default_conf_level()), ". A wide interval ",
              "expresses uncertainty about the population value; its upper limit alone does not ",
              "establish substantial misfit. With few degrees of freedom the RMSEA is estimated ",
              "imprecisely and its interval is wide (Kenny, Kaniskan & McCoach, 2015)."
            ),
            tags$li(
              strong("CFI / TLI"), " (comparative and Tucker-Lewis indices): improvement over the ",
              "null model of uncorrelated items. TLI penalizes complexity more."
            ),
            tags$li(
              strong("SRMR"),
              ": the standardized root mean square residual of the covariances (the square root of the mean squared difference between an observed ",
              "and a model implied covariance, each divided by the two observed standard deviations), the most ",
              "direct answer to how far the model implied correlations miss the observed ones. It is the SRMR DMAR reports. ",
              "lavaan prints a second SRMR for a model with a mean structure, as every FIML fit has, which also averages in the mean residuals; ",
              "those are zero when the intercepts are free, so that number is smaller for the same misfit. ",
              "With auxiliary variables the SRMR is of the model's own variables, as DMAR forms it: lavaan averages an auxiliary variable's ",
              "variances and covariances in, with residuals of zero or nearly so, which makes its SRMR of the same model smaller. ",
              "The note under the boxes says when a fit has them. ",
              "The sample SRMR overestimates the population SRMR, since sampling error adds to every residual, and so it is shown with the ",
              strong("unbiased SRMR"), " of Maydeu-Olivares (2017), which removes that bias on the squared scale, and its ",
              strong(sprintf("%s confidence interval", sieve_ci_percent(sieve_default_rmsea_conf_level()))),
              ", the large sample normal reference interval of Maydeu-Olivares, Shi and Rosseel (2018), at the level of the RMSEA ",
              "interval (DMAR::sem_residual_fit()). Computed without the normality assumption, as it is under MLR, it covered close to ",
              "the nominal rate in most of the conditions Maydeu-Olivares, Shi and Rosseel (2018) simulated, models of 60 variables among ",
              "them; computed under normal theory, as it is under ML, it covered poorly when the items were not normal. Its limits are not ",
              "truncated at zero. DMAR computes it for one group of continuous items fit by maximum likelihood to complete data; for ",
              "any other fit (incomplete data under FIML, ordered items, several groups) the note under the boxes gives DMAR's reason. ",
              "Its time grows with the cube of the number of residual covariances, so for a large model it waits for its button."
            ),
            tags$li(
              strong("AIC / BIC"), ": for comparing competing models on the same data (lower is ",
              "better); they say nothing about absolute fit."
            ),
            tags$li(
              strong("No cutoffs"), ": Hu and Bentler (1999) proposed values for these indices from ",
              "simulations of maximum likelihood estimates of particular confirmatory models; the ",
              "models and conditions they simulated limit how far such values generalize (Marsh, ",
              "Hau & Wen, 2004), and no value of an index makes a model acceptable or not. SIEVER ",
              "applies none of them: each index is reported as it is, the RMSEA with its interval."
            ),
            tags$li(
              em("Interpret the selected estimator, the degrees of freedom, the residuals, and the "),
              em("RMSEA interval together (Hu & Bentler, 1999; Marsh, Hau & Wen, 2004; "),
              em("Kenny, Kaniskan & McCoach, 2015).")
            )
          )
        ),
        card(
          card_header("Bollen-Stine Bootstrap Test of Exact Fit"),
          p(
            class = "small",
            "The model chi square's p-value assumes multivariate normality. The ",
            strong("Bollen-Stine bootstrap"), " (Bollen & Stine, 1992) replaces that ",
            "assumption: the data are transformed so the fitted model holds exactly, ",
            "resampled, and the model is refit on each resample; the p-value is the ",
            "share of bootstrap chi squares at least as large as the observed one. It ",
            "is the standard check on the exact-fit test when items are non-normal and ",
            "the sample is not large, and it complements (rather than replaces) the ",
            "scaled statistic of MLR, the Estimator control's choice for markedly non-normal ",
            "items (the default, ML with FIML, reports the normal-theory statistic). It is defined for maximum likelihood on continuous ",
            "items and computed by DMAR's ", code("sem_exact_fit_bootstrap()"), ". With ",
            "complete data the model is refit by ML on the complete cases. With missing ",
            "responses it is refit by FIML on every respondent, the transformation is the ",
            "extension of Savalei and Yuan (2009) under which the saturated FIML estimates of ",
            "the transformed data are the model implied moments (one matrix for every missing ",
            "data pattern by default, or each pattern on its own), and each resample keeps the ",
            "missing values of the rows it draws, so no available response is deleted. Every ",
            "resample is counted: used in the p-value, improper (an inadmissible solution, kept ",
            "in the p-value unless you set such resamples aside), or failed (did not converge). ",
            "Declared auxiliary variables enter the FIML refit as saturated correlates (Graham, ",
            "2003). DMAR 1.1.0 then judges impropriety with lavaan's check of the item residuals ",
            "and the auxiliary variables together, which can flag resamples whose model ",
            "parameters are proper, so with auxiliary variables such resamples are always kept ",
            "and Set aside is not offered. ",
            "Each replication refits the model, so it runs on request and in the background. ",
            "DMAR does not report its progress while the bootstrap runs, so the progress message ",
            "shows the time elapsed against the estimate; Cancel ends the run, and a canceled run ",
            "reports nothing."
          ),
          div(
            class = "d-flex align-items-center gap-2 mb-2 flex-wrap",
            sieve_boot_inputs(ns, B = 1000, B_id = "bs_B", seed_id = "bs_seed", min_B = 100, max_B = 10000, step_B = 100, label = "Bootstrap replications", width = "180px", seed_label = "Seed (optional)", seed_width = "140px"),
            selectInput(ns("bs_transformation"), "Missing responses",
              choices = c(
                "One matrix for every pattern (Savalei & Yuan)" = "common",
                "Each pattern on its own (Savalei & Yuan)" = "pattern"
              ),
              selected = "common", width = "300px"
            ),
            selectInput(ns("bs_improper"), "Improper resamples",
              choices = c("Keep in the p-value" = "keep", "Set aside" = "drop"),
              selected = "keep", width = "190px"
            ),
            actionButton(ns("bs_run"), "Run Bollen-Stine bootstrap", class = "btn-primary"),
            uiOutput(ns("bs_stop_ui"), inline = TRUE)
          ),
          uiOutput(ns("bs_note")),
          DTOutput(ns("bs_tbl"))
        )
      ),
      nav_panel(
        "Compare structures",
        card(
          card_header("The same items under four structures: one factor, correlated groups, higher order, bifactor"),
          p(
            class = "small",
            strong("Why structures and not a number of factors."), " A confirmatory model with ",
            "k factors needs an assignment of items to factors; without one there is nothing ",
            "to fit. The confirmatory counterpart of a range of factors is therefore a set of ",
            "structures over the same first-order groups (the facets of the one construct, ",
            "or the constructs when there are several): ", strong("one factor"), " (the groups ",
            "not distinguished), the ", strong("correlated groups"), " (one factor per group, ",
            "free to correlate), the ", strong("higher-order model"), " (the group factors ",
            "loading on one second-order factor, which needs three or more groups), and the ",
            strong("bifactor model"), " (a general factor of every item plus one orthogonal ",
            "group factor per group of three or more items, with at least three such group ",
            "factors, the rule the Bifactor tab applies as well). The ",
            "exploratory range, one solution per number of factors with no assignment, is ",
            "the EFA tab's ", actionLink(ns("go_efa"), "Fit by number of factors"), " panel."
          ),
          p(
            class = "small text-muted",
            "The fit indices are read in one family for every structure (named in the table), ",
            "so a difference between two rows compares like with like. The chi square ",
            "difference test is lavaan's, reported for the nested pairs only: the one-factor ",
            "model is nested in each of the other three, and the higher-order model in the ",
            "correlated model and in the bifactor model (Yung, Thissen & McLeod, 1999); the ",
            "correlated model and the bifactor model are not nested, so no test is reported ",
            "for that pair. Three of the constraints lie on the boundary of the parameter ",
            "space (correlations fixed at one, disturbance variances fixed at zero, and group ",
            "loadings fixed at zero, which is the same as group factor variances fixed at zero), ",
            "where the difference test is approximate (Stoel, Garre, Dolan & van den Wittenboer, ",
            "2006). A test that involves an improper solution is shown with a note saying so, ",
            "since its chi square reference distribution is not assured. SIEVER ",
            "reports the fit of each structure and the tests; it draws no verdict. Every ",
            "structure is fit from the Fit the structures button, under the estimator the ",
            "confirmed item types imply, and each fit carries its status."
          ),
          uiOutput(ns("compare_line")),
          uiOutput(ns("compare_note")),
          sieve_plot_save_ui(ns("compare_plot")),
          plotOutput(ns("compare_plot"), height = "300px"),
          DTOutput(ns("compare_tbl")),
          tags$h6(class = "mt-3", "Chi square difference tests of the nested pairs"),
          DTOutput(ns("compare_tests"))
        )
      ),
      nav_panel(
        "Estimates",
        sieve_missing_info_ui(ns),
        sieve_mi_ui(ns),
        card(
          card_header("Standardized loadings with 95% confidence intervals"),
          p(
            class = "small text-muted",
            "The loadings are lavaan's fully standardized (std.all) loadings, read with ",
            "lavaan::standardizedSolution(). Each standard error is the delta method standard error ",
            "computed from the covariance matrix of the estimates under the selected estimator (the ",
            "robust one under MLR and WLSMV), and each interval is the 95% Wald interval, the ",
            "estimate plus or minus 1.96 standard errors. The residual variances are drawn on the ",
            "Path diagram and listed with every parameter below it."
          ),
          sieve_item_decision_controls(ns, "est_decide"),
          DTOutput(ns("est_tbl")), uiOutput(ns("est_code"))
        ),
        card(
          card_header("Factor correlations with 95% confidence intervals"),
          p(
            class = "small text-muted",
            "The correlations are lavaan's fully standardized (std.all) factor covariances, read ",
            "with lavaan::standardizedSolution(). Each standard error is the delta method standard ",
            "error computed from the covariance matrix of the estimates under the selected ",
            "estimator (the robust one under MLR and WLSMV), and each interval is the 95% Wald ",
            "interval, the estimate plus or minus 1.96 standard errors."
          ),
          DTOutput(ns("phi_tbl")), uiOutput(ns("phi_code"))
        ),
        card(
          card_header("Items, Assumptions, and Generated Syntax"),
          uiOutput(ns("specification"))
        )
      ,
        sieve_cfa_evidence_ui(ns, "reliability")
      ),


      nav_panel(
        "Local Fit",
        sieve_cfa_evidence_ui(ns, "residuals"),
        card(
          card_header("Largest modification indices (exploratory, data driven)"),
          div(
            class = "alert alert-warning small",
            strong("Exploratory and data driven."),
            " A modification index estimates how much the model chi square would drop if a ",
            "fixed parameter (a cross loading or a correlated residual) were freed. These ",
            "capitalize on chance, are not confirmatory, and should be acted on only with a ",
            "substantive rationale and ideally cross validation. EPC is the expected parameter ",
            "change if the path were freed, in the units of the items; Std. EPC is the same change ",
            "with every variable standardized. The index does not depend on the scale of the ",
            "items, but lavaan's modindices() leaves a candidate without an index when its ",
            "information falls under a fixed tolerance, which happens on raw score scales with ",
            "large variances. SIEVER then computes that candidate by the equivalent score test on ",
            "the same fit (lavaan::lavTestScore()) and says so here, so no pair is silently omitted."
          ),
          uiOutput(ns("mi_note")),
          DTOutput(ns("mi_tbl"))
        )
      ),


      nav_panel(
        "Plan the Next Study",
        card(
          card_header("Sample Size for a Target RMSEA Interval Width (AIPE)"),
          p(
            class = "small",
            "How many respondents would the ", strong("next"), " study need for the ",
            "RMSEA confidence interval to be no wider than a target? Accuracy in ",
            "parameter estimation (AIPE) planning for the RMSEA (Kelley & Lai, 2011; ",
            "the program of research is reviewed in Maxwell, Kelley, & Rausch, 2008) ",
            "inverts the noncentral chi square; the computation is DMAR's ",
            "ss_aipe_rmsea(). The planning RMSEA and the model degrees of freedom ",
            "prefill from the model fit on this tab, the RMSEA in the family the Fit ",
            "sub-tab shows (robust under MLR and WLSMV, named below the result); narrow ",
            "the target width to see the price of precision. The computation is normal ",
            "theory: the interval it plans is the maximum likelihood interval from the ",
            "noncentral chi square, which a robust or scaled interval reported under MLR ",
            "or WLSMV in the next study need not match in width. The returned sample size makes the interval ",
            "narrow in expectation; the omega planner on the Reliability tab also ",
            "offers assurance, and the two panels together plan the measurement and ",
            "the model sides of the same next study."
          ),
          div(
            class = "d-flex align-items-end gap-3 flex-wrap mb-2",
            numericInput(ns("plan_rmsea"), "Planning RMSEA",
              value = NULL,
              min = 0.001, max = 0.3, step = 0.005, width = "160px"
            ),
            numericInput(ns("plan_df"), "Model df",
              value = NULL,
              min = 1, max = 10000, step = 1, width = "140px"
            ),
            numericInput(ns("plan_rmsea_width"), "Target CI full width",
              value = 0.05,
              min = 0.005, max = 0.3, step = 0.005, width = "160px"
            )
          ),
          uiOutput(ns("plan_rmsea_result")),
          div(
            class = "d-flex align-items-center gap-2 flex-wrap",
            actionButton(ns("plan_rmsea_log"), "Add this plan to the Reproducible Code",
              class = "btn-outline-secondary btn-sm"
            ),
            span(
              class = "small text-muted",
              "The plan is written to the Reproducible Code only when this button is pressed, ",
              "with the inputs at full precision."
            )
          )
        )
      )
    )
  )
}

#' CFA and SEM module server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive req renderUI renderPlot HTML div tags downloadHandler
#' @importFrom shiny reactiveVal observeEvent withProgress actionButton validate need
#' @importFrom DT renderDT datatable formatRound
mod_cfa_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    sieve_code_navigation(input, session)
    # One resolved specification drives the fit, the status shown, the
    # analysis record, and the R written to the reproducible script, so the
    # recorded call is the call that ran (estimator, ordered variables,
    # missing data treatment, and scaling included).
    #
    # The model is fit on request only (fct_ondemand.R): the Fit the model
    # button states the estimated time, the fit runs under a progress
    # message naming the model, the result is kept with the revision and
    # the model choice it was computed under, and the gate says in one line
    # when the data or the item pool changed since. A label or wording edit
    # changes nothing; pressing the button with nothing changed reuses the
    # fit and says so.
    # The tab's Estimator control: the study's setting (Ingest tab) unless
    # chosen otherwise here (sieve_estimator_wire()).
    estimator_choice <- sieve_estimator_wire(input, output, state)
    observe({
      req(state$meta)
      groups <- sieve_facet_groups(state$meta)
      choices <- stats::setNames(names(groups), vapply(groups, function(x) x$label, character(1)))
      saved <- isolate(state$cfa_settings)
      old <- isolate(input$facets)
      target <- cfa_restore()
      selected <- if (!is.null(target$facets)) intersect(
        target$facets,
        names(groups)
      ) else if (!is.null(old)) intersect(
        old,
        names(groups)
      ) else {
        if (!is.null(saved$facets)) intersect(saved$facets, names(groups)) else names(groups)
      }
      updateSelectInput(session, "facets", choices = choices, selected = selected)
    })
    cfa_binding <- sieve_bind_settings(state, "cfa_settings",
      settings = function() {
      req(input$scope, input$structure, input$ident)
      saved <- list(
        scope = input$scope, structure = input$structure, ident = input$ident,
        facets = input$facets %||% character(0), all_facets = !isFALSE(input$all_facets)
      )
      saved
      },
      restore = function(saved) {
        if (!identical(
          isolate(input$scope),
          saved$scope
        )) updateRadioButtons(session,
          "scope",
          selected = saved$scope %||% "construct"
        )
        if (!identical(
          isolate(input$structure),
          saved$structure
        )) updateRadioButtons(session,
          "structure",
          selected = saved$structure %||% "first"
        )
        if (!identical(
          isolate(input$ident),
          saved$ident
        )) updateRadioButtons(session,
          "ident",
          selected = saved$ident %||% "varone"
        )
        if (!is.null(saved$facets) && !identical(
          isolate(input$facets),
          saved$facets
        )) updateSelectInput(session,
          "facets",
          selected = saved$facets
        )
        if (!is.null(saved$all_facets) && !identical(
          isolate(input$all_facets),
          saved$all_facets
        )) updateCheckboxInput(session,
          "all_facets",
          value = saved$all_facets
        )
      }, normalize = function(saved) {
        if (!is.null(saved$facets)) saved$facets <- intersect(saved$facets, names(sieve_facet_groups(state$meta)))
        saved
      })
    cfa_restore <- cfa_binding$pending
    # The mapping from the controls to the model is the one the validation
    # reservation reads from the saved settings (sieve_cfa_choices()).
    cfa_choices <- reactive(c(
      sieve_cfa_choices(list(
        scope = input$scope, structure = input$structure, ident = input$ident,
        facets = input$facets, all_facets = input$all_facets
      )),
      list(estimator = estimator_choice(), sampling = sieve_sampling_spec(state), two_stage = isTRUE(input$two_stage))
    ))
    cfa_key <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      # Currency on the effective inputs: the key covers the
      # items the specified model reads, and the facet column only for a
      # model that reads facets (higher-order, direct facet), so a facet
      # edit, or an edit to an item outside the model, leaves a first-order
      # fit current, while every change to a modeled item marks it stale.
      choices <- cfa_choices()
      sieve_demand_key(state, choices,
        items = build_spec()$observed,
        facets = choices$structure %in% c("higher_order", "facet_first_order")
      )
    })
    cfa_plan <- reactive({
      req(state$meta)
      spec <- build_spec()
      active <- state$meta[match(spec$observed, state$meta$item), , drop = FALSE]
      p <- length(spec$observed)
      seconds <- sieve_demand_wait("cfa", p, spec$estimator, missing = spec$missing)
      standard_seconds <- sieve_at_default_rates(sieve_demand_wait("cfa", p, spec$estimator, missing = spec$missing))
      higher <- identical(cfa_choices()$structure, "higher_order")
      facet <- identical(cfa_choices()$structure, "facet_first_order")
      marker <- identical(cfa_choices()$ident, "marker")
      model <- if (facet) "direct correlated-facet" else if (higher) "higher-order" else "correlated-factors"
      list(
        estimator = spec$estimator, missing = spec$missing, p = p, seconds = seconds, standard_seconds = standard_seconds,
        label = sieve_duration_label(seconds),
        progress = sprintf("CFA: %s, %d items", spec$estimator, p),
        detail = sprintf(
          "Fitting the %s model with %s identification in lavaan.",
          if (facet) "direct correlated-facet" else if (higher) "higher-order (construct to facet to items)" else "correlated-factors",
          if (marker) "marker indicator" else "factor variance = 1"
        ),
        sentence = sprintf(
          "Fitting the %s model of %d items under %s is estimated to take %s",
          model, p, spec$estimator, sieve_duration_label(seconds)
        ),
        refused = if (length(spec$problems)) paste(spec$problems,
          collapse = " "
        ) else sieve_wlsmv_guard(active$item,
          state$items,
          ordered = spec$ordered %||% character(0),
          estimator = spec$estimator
        )
      )
    })
    build_spec <- function() {
      higher <- identical(input$structure, "higher")
      marker <- identical(input$ident, "marker")
      spec <- sieve_cfa_spec(
        state$meta,
        structure = cfa_choices()$structure,
        facet_ids = cfa_choices()$facet_ids,
        std_lv = !marker,
        reserved = union(names(state$items), names(state$raw)),
        estimator = estimator_choice()
      )
      sieve_two_stage_spec(spec, isTRUE(input$two_stage))
    }
    fit_cfa <- function(key, plan) {
      higher <- identical(key$choices$structure, "higher_order")
      marker <- identical(key$choices$ident, "marker")
      spec <- build_spec()
      prepared <- sieve_sampling_apply(spec, state, state$items)
      spec <- prepared$spec
      if (!is.null(prepared$reason)) spec$problems <- c(spec$problems, prepared$reason)
      # The session's fit store: the identical model on the identical data,
      # fit on another page (Items, Validity, the scores behind
      # Reliability), is reused rather than fit again (fct_fit_store.R).
      store <- shiny::isolate(state$fit_store)
      run <- sieve_run_cfa(spec, prepared$data,
        store = store, page = "CFA", rows = shiny::isolate(sieve_case_index(state))[prepared$row_index]
      )
      run$warnings <- unique(c(run$warnings, prepared$warnings))
      # Declared auxiliary variables left out because every analyzed item
      # is observed (sieve_auxiliary_apply()); the method line says so.
      run$aux_notes <- prepared$notes
      run$row_flow <- prepared$row_flow
      run$input_rows <- prepared$row_index
      # The estimator words of this fit, resolved from the types it was fit
      # under, so the method line describes the result it shows after a
      # later edit.
      fit_meta <- shiny::isolate(state$meta)
      run$est <- sieve_estimator_resolve(fit_meta$type[match(spec$observed, fit_meta$item)],
        n = NROW(prepared$data),
        choice = spec$estimator_choice %||% shiny::isolate(estimator_choice())
      )
      if (identical(spec$missing, "robust.two.stage")) {
        run$est$estimator <- spec$estimator
        run$est$missing <- spec$missing
        run$est$label <- "Robust Two-Stage ML"
        run$est$reason <- "ML with robust two-stage standard errors and the Satorra-Bentler test; the Savalei-Bentler residual statistic is supplied separately."
      }
      if (!is.null(run$fit)) {
        note <- sprintf(
          "Fit the %s CFA (%s identification)",
          if (identical(
            spec$structure,
            "facet_first_order"
          )) "direct correlated-facet" else if (higher) "higher-order (construct -> facet -> items)" else "correlated-factors",
          if (marker) "marker indicator = 1" else "factor variance = 1"
        )
        # The fit measures and the standardized solution are read once,
        # here, under the progress bar: lavaan fits the baseline models for
        # the measures and takes the delta method through every parameter
        # for the standardized solution, which on a large WLSMV model costs
        # seconds each, and the panels must not pay that again after a
        # label edit. A fit reused from another page brings what that page
        # read; what is read here is kept with the stored fit for the next.
        run$measures <- sieve_fit_measures_at(run$measures, run$fit, sieve_default_rmsea_conf_level(), residual = TRUE)
        run$standardized <- run$standardized %||% sieve_capture(lavaan::standardizedSolution(run$fit))
        if (sieve_has_solution(run$status)) {
          # The registry's record of the estimates, from the measures just
          # read (the registry would otherwise read them a second time).
          run$estimates <- list(
            fit = run$measures,
            parameters = lavaan::parameterEstimates(run$fit, standardized = TRUE)
          )
        }
        run$analysis_id <- sieve_register_analysis(state, run, note = note)
        if (isTRUE(run$reused)) {
          reused <- sieve_reuse_text(run)
          sieve_log(
            state,
            sprintf("%s: %s%s", note, tolower(substring(reused, 1L, 1L)), substring(reused, 2L)),
            c(
              sieve_reuse_code(run, fit_name = "cfa_fit", model_name = "cfa_model"),
              sieve_usrmr_code(attr(run$measures, "families")$usrmr, "cfa_fit")
            )
          )
        } else {
          sieve_log(state, note, c(
            if (length(prepared$notes)) paste("#", prepared$notes),
            sieve_cfa_code(spec),
            # The residual statistic computed beside a robust two-stage fit
            # (see sieve_two_stage_residual()).
            if (identical(spec$missing, "robust.two.stage")) run$two_stage_residual$code,
            # The unbiased SRMR shown beside the SRMR (see sieve_fit_usrmr()).
            sieve_usrmr_code(attr(run$measures, "families")$usrmr, "cfa_fit")
          ))
        }
        sieve_fit_store_keep(store, run, fit_name = "cfa_fit", model_name = "cfa_model")
        run$code_view <- sieve_code_snapshot(state, context = c(
          sprintf("Model: %s; estimator: %s; missing data: %s; std.lv = %s.",
            spec$structure, spec$estimator, spec$missing, spec$std_lv),
          sprintf("Modeled item columns: %s.", paste(spec$observed, collapse = ", ")),
          prepared$notes,
          sprintf("Rows supplied to lavaan: %d; fitted rows: %s.", nrow(prepared$data),
            paste(lavaan::lavInspect(run$fit, "nobs"), collapse = ", ")),
          "Loadings and factor correlations use lavaan's fully standardized solution (std.all); they are displayed to three decimal places."
        ))
      }
      run
    }
    # The standardized solution of the fit on screen: the one read with the
    # fit, or read now for a fit that came without one.
    standardized <- function(run) {
      run$standardized %||% sieve_capture(lavaan::standardizedSolution(run$fit))
    }
    # A model from before an edit is updated by the tab's one Update
    # (update_go below): the page guide's record names that action.
    demand <- sieve_demand_wire(
      input, session, state,
      id = "fit_go", what = "CFA",
      key = cfa_key, plan = cfa_plan, compute = fit_cfa, kind = "cfa", capability = "cluster",
      refit = "Update",
      # The identical model on the identical data, already fit on another
      # page (the Items page's overall model, Validity's measurement model,
      # the model behind the Reliability page's factor scores), is adopted
      # without a click, as those pages adopt this page's fit
      # (fct_fit_store.R): the run policy decides whether this page fits a
      # model by itself, not whether it reads a fit the session already
      # holds. The lookup is fit_cfa()'s.
      peek = function(key, plan) {
        shiny::isolate({
          prepared <- sieve_sampling_apply(build_spec(), state, state$items)
          is.null(prepared$reason) && sieve_fit_store_has(state$fit_store,
            prepared$spec, prepared$data, rows = sieve_case_index(state)[prepared$row_index])
        })
      },
      # The modeled items and the rows the fit read (under FIML every row
      # with an answer, under listwise the complete cases), with the count
      # of included items, so the sentence says when the model used fewer
      # of them (an included item with no construct, a construct left with
      # one included item, a facet too small or not selected; the note above
      # the diagram names each). The count is the ledger's as it stands:
      # excluding or including an item outside the model leaves the fit
      # current, and the sentence follows the ledger.
      fitted = function(value) list(
        items = value$spec$observed,
        respondents = if (inherits(value$fit, "lavaan")) sum(lavaan::lavInspect(value$fit, "nobs")) else NULL,
        included = tryCatch(sum(state$meta$status == "active"), error = function(e) NULL)
      )
    )
    cfa_run <- reactive({
      st <- demand$status()
      if (!is.null(st$value)) {
        return(st$value)
      }
      # Not fit yet, or the computation stopped: the record carries the
      # specification (what the model would hold) and no fit, so every
      # consumer reads it as a model without a solution.
      spec <- build_spec()
      why <- if (identical(st$state, "failed")) {
        st$error
      } else {
        "The model is not fit yet. Press Fit the model (above the tabs)."
      }
      out <- list(
        spec = spec, fit = NULL,
        status = sieve_fit_status(NULL, error = why, code = "failed"),
        warnings = character(0), messages = character(0), error = NULL,
        console = character(0), not_run = TRUE
      )
      class(out) <- "sieve_result"
      out
    })

    # Read only the held run: revealing a disclosure never fits a model or
    # regenerates its code from controls that may now describe another model.
    output$two_stage_test <- renderUI({
      run <- cfa_run()
      req(identical(run$spec$missing, "robust.two.stage"))
      sieve_two_stage_residual_ui(run$two_stage_residual)
    })
    output$fit_code <- renderUI({
      st <- demand$status()
      # The unbiased SRMR's line follows what the page holds for the fit on
      # screen (sieve_usrmr_code()): the call that gave the value (on the
      # same model without its auxiliary variables when DMAR refuses them),
      # DMAR's reason when it refused, or the call to run when it waits for
      # its button.
      usrmr <- attr(tryCatch(fm(), error = function(e) st$value$measures), "families")$usrmr
      sieve_code_view(st$value$code_view, st, ns = session$ns,
        readout = c(
          sprintf("lavaan::fitMeasures(cfa_fit, fm.args = list(rmsea.ci.level = %s))",
            sieve_num_lit(sieve_default_rmsea_conf_level())),
          "# The fit table labels the standard, scaled or robust family selected from these measures;",
          sprintf("# the RMSEA interval is at %s, the level of every RMSEA interval SIEVER shows (90%% by default, DMAR's and lavaan's).",
            sieve_ci_percent(sieve_default_rmsea_conf_level())),
          sprintf("DMAR::sem_fit_measures(cfa_fit, conf_level = %s)  # the SRMR shown is its srmr row: of the covariance residuals alone and of the model's own variables (lavaan's srmr_bentler_nomean when the fit has no auxiliary variable; lavaan's srmr also averages in the mean residuals)", sieve_num_lit(sieve_default_rmsea_conf_level())),
          sieve_usrmr_code(usrmr, "cfa_fit")
        ))
    })
    output$phi_code <- renderUI({
      st <- demand$status()
      sieve_code_view(st$value$code_view, st, ns = session$ns,
        readout = c(
          'ss <- lavaan::standardizedSolution(cfa_fit, type = "std.all")',
          'latents <- unique(ss$lhs[ss$op == "=~"])',
          'ss[ss$op == "~~" & ss$lhs != ss$rhs &',
          '   ss$lhs %in% latents & ss$rhs %in% latents, ]'
        ))
    })
    output$est_code <- renderUI({
      st <- demand$status()
      sieve_code_view(st$value$code_view, st, ns = session$ns,
        readout = c(
          'ss <- lavaan::standardizedSolution(cfa_fit, type = "std.all")',
          'ss[ss$op == "=~", ]',
          '# est.std is the fully standardized loading; se and ci.lower/ci.upper describe that estimate.'
        ))
    })

    output$specification <- renderUI({
      run <- cfa_run()
      spec <- run$spec
      tagList(
        p(class = "small", sprintf(
          "Scope: %s. Estimator: %s. Missing data: %s. Identification: %s.",
          spec$structure, spec$estimator, spec$missing,
          if (spec$std_lv) "factor variances fixed at one; all loadings free" else "first loading fixed at one"
        )),
        if (identical(spec$sampling_design$unit, "clustered")) p(
          class = "small",
          sprintf(
            "Cluster-robust MLR: %d independent clusters (G); cluster column: %s. Rank and small-cluster checks apply to G, not to the number of respondent rows.",
            spec$sampling_design$clusters, spec$sampling_design$cluster
          )
        ),
        p(
          class = "small",
          "Modeled items: ",
          if (length(spec$observed)) paste(spec$observed,
            collapse = ", "
          ) else "none",
          "."
        ),
        if (nrow(spec$not_modeled)) tags$ul(class = "small", lapply(seq_len(nrow(spec$not_modeled)), function(i) {
          row <- spec$not_modeled[i, ]
          tags$li(paste0(row$items, ": ", row$reason, "."))
        })),
        if (length(spec$problems)) div(class = "alert alert-warning small", paste(spec$problems, collapse = " ")),
        tags$pre(if (nzchar(spec$model)) spec$model else "No model can be written for the current selection."),
        p(class = "small", "Status: ", run$status$message)
      )
    })

    fit <- reactive({
      cfa_run()$fit
    })

    # The estimate tables describe a solution. A model that did not converge
    # or is not identified has none, so they show nothing for it and the
    # alert above says why (the same convention as the SEM and validity tabs).
    solved_fit <- reactive({
      run <- cfa_run()
      if (sieve_has_solution(run)) run$fit else NULL
    })

    # The rows of the Estimates table are items; the selected ones go to the
    # shared Exclude / Include dialog (fct_item_decision.R, mod_drop.R),
    # whose Exclude and Include remain the only writers of the item status.
    est_shown <- new.env(parent = emptyenv())
    est_shown$items <- character(0)
    sieve_item_decision_wire(input, state, "est_decide",
      items = function() sieve_selected_items(NULL, input$est_tbl_rows_selected, est_shown$items),
      origin = "the CFA Estimates table"
    )

    output$gate <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(sieve_gate(state))
      }
      notes <- list()
      if (identical(cfa_choices()$structure, "higher_order")) {
        n_facetted <- sum(vapply(
          unique(state$meta$construct[state$meta$status == "active"]),
          function(cn) {
            f <- state$meta$facet[state$meta$status == "active" & state$meta$construct == cn]
            length(unique(f[!is.na(f) & nzchar(f)])) >= 2L
          }, logical(1)
        ))
        if (n_facetted == 0L) {
          notes <- c(notes, list(div(
            class = "alert alert-info",
            strong("No facets assigned."),
            " The higher-order model needs at least one construct split into two or more facets, ",
            "so it currently reduces to the first-order model. Assign facets to items in the ",
            "Facet column of the Ingest or Items tab to build construct to facet to items."
          )))
        }
        excl <- sieve_unfacetted_items(state$meta)
        if (length(excl)) {
          notes <- c(notes, list(div(
            class = "alert alert-warning small",
            sprintf(
              "Excluded from the higher-order model (in a facetted construct but with no facet of their own): %s.",
              paste(excl, collapse = ", ")
            )
          )))
        }
      }
      run <- cfa_run()
      st <- run$status
      # What the model leaves out, and why: a construct with a single included
      # item, or a facet with too few items (items with no facet are named in
      # the note above). A current fit is described by the live specification:
      # its modeled items are the fit's (the result's key covers them), while
      # an item outside the model may have been excluded or included since
      # the fit, which leaves the fit current; this note then follows the
      # ledger, as the method line's count of included items does.
      note_spec <- if (identical(demand$status()$state, "current")) {
        tryCatch(build_spec(), error = function(e) run$spec)
      } else {
        run$spec
      }
      nm <- note_spec$not_modeled
      unfacetted <- nm$level == "item" & !is.na(nm$construct) &
        identical(note_spec$structure, "higher_order")
      left_out <- sieve_not_modeled_text(list(not_modeled = nm[!unfacetted, , drop = FALSE]))
      if (length(left_out)) {
        notes <- c(notes, list(div(
          class = "alert alert-warning small",
          paste(left_out, collapse = " ")
        )))
      }
      # The one status line: the results from before an edit, with the
      # page's one Update; otherwise whether the model is not fit yet or
      # stopped with an error, with its Fit button. A current fit is stated
      # in the method line instead: a line of its own above the tabs takes
      # its height from every sub-tab, and the Parameter estimates table of
      # the bundled example then loses its last row to the card's scroll.
      status_line <- page_status_line()
      if (!is.null(status_line)) {
        notes <- c(list(status_line), notes)
      }
      if (isTRUE(run$not_run)) {
        return(do.call(tagList, notes))
      }
      if (is.null(run$fit)) {
        active <- state$meta[state$meta$status == "active", , drop = FALSE]
        notes <- c(notes, list(if (!nzchar(run$spec$model %||% "")) {
          # No factor could be written: either nothing is included, or no
          # construct has two included items (each one is named here with
          # its count). The one action is on the Items tab.
          div(
            class = "alert alert-danger",
            strong("There is no measurement model to fit. "),
            if (!nrow(active)) {
              "Every item is excluded. On the Items tab, include the items you want in the model."
            } else if (all(!sieve_construct_assigned(active$construct))) {
              "No included item is assigned to a construct. On the Items tab, assign the items to constructs."
            } else {
              if (identical(cfa_choices()$structure, "facet_first_order")) {
                "No selected facet has two or more included items. Select assigned facets above, or include at least two items per facet on the Items tab."
              } else paste(
                "No construct has two or more included items. On the Items tab, include",
                "at least two items of each construct you want in the model."
              )
            },
            # Each construct left out, with its count: the sentences the
            # warning above carries when some other construct is modeled.
            if (length(left_out)) paste0(" ", paste(left_out, collapse = " "))
          )
        } else {
          # The engine refused the model: its reason, with the item and the
          # action when the reason is a known one.
          div(
            class = "alert alert-danger",
            strong("The model could not be fit. "),
            if (!is.null(run$error)) tagList(em(run$error), " ") else NULL,
            sieve_failure_hint(run$error) %||%
              "Check that each construct (or facet) has at least two included items and that any second-order factor has at least two facets."
          )
        }))
      } else {
        # A fitted object is not a certificate. State what lavaan established
        # before any estimate or fit index below is read.
        if (st$code %in% c("not_converged", "not_identified")) {
          notes <- c(notes, list(div(
            class = "alert alert-danger",
            strong(if (identical(
              st$code,
              "not_converged"
            )) "The model did not converge. " else "The model is not identified. "),
            st$message,
            " The model has no solution, so no estimate, standard error, or fit index is shown for it."
          )))
        } else if (isTRUE(st$just_identified)) {
          notes <- c(notes, list(div(
            class = "alert alert-info small",
            strong("Just identified model. "),
            "The model has zero degrees of freedom, so there is no ordinary chi square test of fit. ",
            "Saturated-model conventions apply only when exact fit is established and supply ",
            "no evidence of approximate fit."
          )))
        }
        # Heywood / improper-solution check: a model can converge and report
        # clean global fit while harboring a negative residual variance or a
        # standardized loading above one. Surface it, because such a solution is
        # not interpretable even when CFI/RMSEA look good. It is a statement
        # about a solution, so it is made only for a model that has one: the
        # numbers a model that did not converge stopped at are not estimates,
        # and calling them an improper solution "even though the model
        # converged" would contradict the alert above.
        f <- run$fit
        solved <- sieve_has_solution(st)
        checked <- if (!solved) {
          list(ok = TRUE, value = FALSE, error = NULL)
        } else {
          read <- standardized(run)
          if (!read$ok) {
            read
          } else {
            sieve_capture({
              std <- read$value
              ld <- std[std$op == "=~", "est.std"]
              rv <- std[std$op == "~~" & std$lhs == std$rhs, "est.std"]
              any(abs(ld) > 1, na.rm = TRUE) || any(rv < 0, na.rm = TRUE)
            })
          }
        }
        heywood <- isTRUE(checked$value)
        if (!checked$ok) {
          notes <- c(notes, list(div(
            class = "alert alert-warning small",
            "The standardized solution could not be read to check for an improper solution: ",
            checked$error
          )))
        }
        if (solved && (identical(st$code, "inadmissible") || isTRUE(heywood))) {
          notes <- c(notes, list(div(
            class = "alert alert-warning",
            strong("Improper solution (Heywood case). "),
            "A residual variance is negative or a standardized loading exceeds one, ",
            "so the estimates are not fully interpretable even though the model converged. ",
            if (length(st$negative_variances)) sprintf(
              "Negative variance estimate: %s. ",
              paste(st$negative_variances,
                collapse = ", "
              )
            ) else NULL,
            "This often means a construct is over-extracted or weakly identified; consider ",
            "merging constructs or excluding indicators, or revisiting the dimensionality."
          )))
        }
        if (length(run$warnings)) {
          notes <- c(notes, list(tags$details(
            class = "alert alert-secondary small",
            tags$summary(sprintf(
              "lavaan raised %d warning%s while fitting this model",
              length(run$warnings), if (length(run$warnings) == 1L) "" else "s"
            )),
            tags$ul(lapply(run$warnings, tags$li))
          )))
        }
      }
      if (length(notes)) do.call(tagList, notes) else NULL
    })

    # The sentence above the tabs states the estimator the confirmed item
    # types (or the Estimator control) resolve to, with the reason, and what
    # the identification choice does, so the text cannot disagree with the
    # model that is fit.
    output$model_note <- renderUI({
      req(state$meta)
      # The estimator of the model that was fit, which follows the items in
      # the model; before there is one, what the included items imply.
      resolved <- NULL
      problem <- NULL
      if (isTRUE(sieve_confirmation_status(state)$confirmed)) {
        read <- sieve_attempt(cfa_run()$spec)
        resolved <- read$value
        problem <- read$error
      }
      active <- state$meta[state$meta$status == "active", , drop = FALSE]
      modeled <- if (!is.null(resolved)) intersect(resolved$observed, active$item) else active$item
      est <- sieve_estimator_resolve(active$type[match(modeled, active$item)],
        n = NROW(state$items),
        choice = (resolved$estimator_choice %||% estimator_choice())
      )
      # The weight matrix of a categorical fit, stated with its size, and the
      # fit's estimated peak memory, which the guard compares with its limit.
      weight <- if (identical(est$estimator, "WLSMV") && length(modeled)) {
        cats <- vapply(modeled, function(nm) {
          if (!est$categorical[match(nm, modeled)]) return(NA_real_)
          v <- sieve_as_number(state$items[[nm]])
          length(unique(v[!is.na(v)]))
        }, numeric(1))
        bytes <- sieve_wlsmv_weight_bytes(length(modeled), cats)
        present <- intersect(modeled, names(state$items))
        n <- sum(rowSums(!is.na(as.data.frame(state$items)[, present, drop = FALSE])) > 0L)
        peak <- sieve_wlsmv_peak_bytes(length(modeled), cats, n)
        sprintf(
          paste0(
            " The weight matrix of this fit holds the %s thresholds and correlations of %d items (about %s for one ",
            "copy); with lavaan's working copies of it and the casewise scores of %s respondents the fit needs an ",
            "estimated %s at its peak (the limit is %s)."
          ),
          format(sieve_wlsmv_statistics(length(modeled), cats), big.mark = ","),
          length(modeled), sieve_bytes_text(bytes), format(n, big.mark = ","),
          sieve_bytes_text(peak), sieve_bytes_text(sieve_wlsmv_limit())
        )
      }
      marker <- identical(input$ident, "marker")
      p(
        class = "text-muted small",
        if (!is.null(problem)) {
          tagList(strong("The specification could not be resolved: "), problem, " ")
        },
        strong("Confirmatory measurement model."), " This tab fits the CFA implied by your ",
        if (identical(cfa_choices()$structure, "facet_first_order")) {
          "facet assignments: each selected facet is a freely correlated first-order factor measured directly by its included items. No higher-order construct is inserted. "
        } else "construct assignments: each construct is a latent factor measured by its items, ",
        tagList("estimated by ", strong(est$label), ". ", est$reason, weight, " "),
        if (marker) {
          paste0(
            "Each latent scale is set by fixing the loading of the first item to one, ",
            "so the factor variances are estimated. "
          )
        } else {
          paste0(
            "Each latent scale is set by fixing the factor variance to one, so every ",
            "loading is free. This sets the scale of the latent variables; it does not ",
            "standardize the observed items, so the loadings are fully standardized only ",
            "in the standardized (std.all) solution. "
          )
        },
        "It tests how well the items measure the constructs. To estimate relationships ",
        strong("among"), " the constructs (structural regressions), use the ", strong("SEM"),
        " tab."
      )
    })

    # The one-line method statement above the tabs: the model and the
    # program, the estimator, what it is fit to, the cases used with n, and,
    # for a current fit, when it was fit.
    # It reads the model on screen (or, before one, the specification the
    # controls make) and never fits.
    output$method_line <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$meta)
      run <- cfa_run()
      read <- sieve_attempt(run$spec)
      spec <- read$value
      req(!is.null(spec))
      rows_now <- NROW(state$items)
      held <- !is.null(run$fit)
      # A fitted result is described by what it was computed from: its
      # estimator words, its rows and its n are the held run's, never paired with the rows in
      # the analysis now ("n = 301 of 300 rows" after one removal), and the
      # rows now are stated separately when they differ.
      est <- if (held && !is.null(run$est)) {
        run$est
      } else {
        active <- state$meta[state$meta$status == "active", , drop = FALSE]
        modeled <- intersect(spec$observed, active$item)
        sieve_estimator_resolve(active$type[match(modeled, active$item)],
          n = rows_now,
          choice = (spec$estimator_choice %||% estimator_choice())
        )
      }
      fit_n <- if (held) {
        sieve_capture(sum(lavaan::lavInspect(run$fit, "nobs")))$value
      }
      fit_rows <- if (held) {
        run$row_flow$input %||% sieve_capture(sum(lavaan::lavInspect(run$fit, "norig")))$value
      }
      st <- demand$status()
      current <- identical(st$state, "current")
      # The headline: where the result
      # stands, the model, the estimator, the cases and n, and when it was
      # fit. The provenance behind it (the missing-data diagnostics, the
      # uncertainty, and what the diagram and tables belong to) is in a
      # labeled disclosure under it, which keeps the state the researcher
      # leaves it in for the browser session.
      when <- if (current) {
        if (isTRUE(run$reused)) {
          sprintf(" %s (%s then), current.", sieve_reuse_text(run), sieve_seconds_text(run$seconds))
        } else {
          sprintf(" Fit at %s in %s, current.", st$when, sieve_seconds_text(st$seconds))
        }
      }
      provenance <- tagList(
        if (held) {
          diagnostics <- attr(run$fit, "sieve_missing_data", exact = TRUE)
          if (!is.null(diagnostics)) div(sieve_missing_line(diagnostics,
            used = fit_n, treatment = spec$missing))
        },
        # What the fit left out and why: the declared auxiliary variables,
        # when every analyzed value is observed (sieve_auxiliary_unused_note()).
        if (held && length(run$aux_notes)) div(paste(run$aux_notes, collapse = " ")),
        if (held) div(class = "sieve-inference-line", paste0("Uncertainty: ", sieve_inference_text(sieve_fit_inference(run$fit)), ".")),
        # What the diagram and the tables below belong to, and whether the
        # fit ran by itself after an item was excluded or included.
        if (current) {
          div(sprintf(
            "The diagram and tables below are from this fit of %s.%s",
            sieve_fitted_scope_text(st$fitted$items, st$fitted$respondents, included = st$fitted$included),
            if (!identical(st$trigger, "inclusion")) {
              ""
            } else if (isTRUE(run$reused)) {
              " It was updated by itself after your item change, reusing the identical earlier fit."
            } else {
              " It was refit by itself after your item change."
            }
          ))
        }
      )
      div(
        class = "small mb-2 sieve-method-line",
        # Where the result stands, in the same word as every other page.
        sieve_lifecycle_badge(sieve_demand_lifecycle(st), "fit"),
        strong("Method: "),
        sieve_cfa_method_text(spec, est,
          rows = if (held && length(fit_rows) == 1L) fit_rows else rows_now,
          used = fit_n, rows_now = rows_now
        ),
        sieve_holdout_n_tag(state),
        when,
        if (held || current) {
          tags$details(
            class = "sieve-provenance text-muted", `data-sieve-remember` = "cfa-method",
            tags$summary("Method and data behind this result"),
            provenance
          )
        }
      )
    })

    # The diagram specification is built once per fitted model, with the
    # item labels and construct names of the ledger at the time of the fit.
    # The display controls are applied to it by sieve_diagram_wire(), which
    # never reads the fitted model, so no control can refit it.
    diagram_base <- reactive({
      run <- cfa_run()
      sieve_diagram_spec(
        run,
        labels = sieve_diagram_item_labels(state$meta),
        name_map = if (length(run$spec$name_map)) run$spec$name_map else NULL,
        analysis_id = run$analysis_id
      )
    })
    diagram <- sieve_diagram_wire(
      input, output, session, state, diagram_base,
      layout = "top_down", fit_name = "cfa_fit", file_stem = "siever_path_diagram",
      what = "confirmatory factor model"
    )

    # The unbiased SRMR of a model too large to compute it with the fit is
    # offered on a button under the boxes (see sieve_usrmr_wire()).
    usrmr_merge <- sieve_usrmr_wire(input, output, session, state,
      fit = function() cfa_run()$fit, key = function() cfa_run()$analysis_id,
      measures = function() fm(), fit_name = "cfa_fit", page = "CFA"
    )
    fm <- reactive({
      run <- cfa_run()
      req(run$fit)
      usrmr_merge(sieve_fit_measures_at(run$measures, run$fit, sieve_default_rmsea_conf_level(), residual = TRUE))
    })

    # Format a fit index, degrading a non-finite value (for example RMSEA/CFI
    # when df = 0 in a just-identified two-indicator model) to "n/a" rather than
    # printing the literal "NA" inside a value box.
    fmt_num <- sieve_fmt_num
    fmt_ci <- function(est, lo, hi) sieve_fmt_ci(est, lo, hi,
      reason = if (identical(attr(fm(), "status"), "not_identified")) "model not identified" else "df = 0 (just identified)")
    # Every box names the family its value is in (robust, scaled, or
    # standard): the three are different statistics, so an unlabeled CFI or
    # RMSEA cannot be compared with one from another source.
    family_tag <- function(m) {
      family <- sieve_fit_family(m, "index")
      if (is.na(family)) "" else sprintf("<br><span style='font-size:0.55em'>%s</span>", family)
    }
    # The fit measures the tab last displayed: the planning inputs below are
    # prefilled from them by an observer, and an observer is not suspended
    # while the tab is hidden, so it reads what this output computed rather
    # than fm() itself (which would fit the model with the tab never shown,
    # at Load and after every ledger edit, whenever the fit is automatic).
    fm_shown <- reactiveVal(NULL)
    output$vb_rmsea <- renderUI({
      m <- fm()
      req(m)
      fm_shown(m)
      tagList(fmt_ci(m["rmsea"], m["rmsea_lo"], m["rmsea_hi"]), HTML(family_tag(m)))
    })
    output$vb_cfi <- renderUI({
      m <- fm()
      req(m)
      HTML(paste0(fmt_num(m["cfi"]), family_tag(m)))
    })
    output$vb_tli <- renderUI({
      m <- fm()
      req(m)
      HTML(paste0(fmt_num(m["tli"]), family_tag(m)))
    })
    output$vb_srmr <- renderUI({
      m <- fm()
      req(m)
      unbiased <- sieve_usrmr_words(m, num = function(x) fmt_num(x))
      HTML(paste0(
        fmt_num(m["srmr"]),
        if (!is.null(unbiased)) sprintf("<br><span style='font-size:0.55em'>%s</span>", unbiased) else ""
      ))
    })
    # The fit of the model on screen, in one line above the path diagram, so
    # the tab opens on the fitted model with its fit. It
    # reads the measures held with the fit and never fits; every index is
    # named with its family, as in the boxes on the Fit sub-tab.
    output$overview_fit <- renderUI({
      m <- fm()
      req(m)
      fm_shown(m)
      lab <- function(index) sieve_fit_label(m, index)
      p_text <- if (!is.finite(m["pvalue"])) {
        "n/a"
      } else if (m["pvalue"] < 0.001) "< .001" else sprintf("%.3f", m["pvalue"])
      parts <- c(
        sprintf(
          "%s = %s, df = %s, p %s", lab("chisq"), fmt_num(m["chisq"]),
          if (is.finite(m["df"])) sprintf("%.0f", m["df"]) else "n/a",
          if (startsWith(p_text, "<")) p_text else paste("=", p_text)
        ),
        sprintf("%s = %s", lab("cfi"), fmt_num(m["cfi"])),
        sprintf("%s = %s", lab("tli"), fmt_num(m["tli"])),
        sprintf(
          "%s = %s, %s %s", lab("rmsea"), fmt_num(m["rmsea"]),
          sieve_rmsea_ci_words(sieve_fit_ci_level(m)),
          if (is.finite(m["rmsea_lo"]) && is.finite(m["rmsea_hi"])) {
            sprintf("[%.3f, %.3f]", m["rmsea_lo"], m["rmsea_hi"])
          } else "n/a"
        ),
        sprintf("%s = %s", lab("srmr"), fmt_num(m["srmr"])),
        sieve_usrmr_words(m, num = function(x) fmt_num(x))
      )
      # One line: each index keeps its
      # family label; the pointer to the Fit sub-tab is a few words.
      p(
        class = "small mb-2 sieve-fit-line",
        strong("Fit of this model: "), paste(parts, collapse = "; "), ". ",
        tags$span(class = "text-muted", "Every family and how to read them: Fit sub-tab.")
      )
    })

    # Which family is shown, why, and which test statistic the chi square is.
    output$fit_family <- renderUI({
      m <- fm()
      req(m)
      note <- sieve_fit_note(m)
      if (!length(note)) {
        return(NULL)
      }
      div(class = "small text-muted mb-2", lapply(note, function(x) tags$div(x)))
    })

    output$fit_tbl <- renderDT({
      m <- fm()
      req(m)
      # Every row is labeled with the family its value is in.
      df <- sieve_fit_table(m)
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t", pageLength = 20)
      ) |>
        formatRound("Value", 3)
    })

    # All three families at full precision in the stored object; rounded
    # here, for display only.
    output$fit_fam_tbl <- renderDT({
      m <- fm()
      req(m)
      tab <- sieve_fit_family_table(m)
      req(tab)
      sieve_dt(tab,
        rownames = FALSE, selection = "none",
        options = list(dom = "t", pageLength = 20, ordering = FALSE),
        colnames = c("Measure", "Standard", "Scaled", "Robust")
      ) |>
        formatRound(c("standard", "scaled", "robust"), 3)
    })
    output$fit_fam_why <- renderUI({
      m <- fm()
      req(m)
      why <- attr(sieve_fit_family_table(m), "reasons")
      if (!length(why)) {
        return(NULL)
      }
      div(
        class = "small text-muted mt-2",
        strong("Not reported, and why: "), tags$ul(lapply(why, tags$li))
      )
    })

    # A table that could not be read from the fit says so, in the table.
    unreadable <- function(read, what) {
      sieve_dt(
        data.frame(Note = sprintf("The %s could not be read from the fitted model: %s", what, read$error)),
        rownames = FALSE, selection = "none", options = list(dom = "t")
      )
    }

    output$phi_tbl <- renderDT({
      f <- solved_fit()
      req(f)
      read <- standardized(cfa_run())
      if (!read$ok) {
        return(unreadable(read, "standardized solution"))
      }
      ss <- read$value
      # Inter-factor correlations are the off-diagonal covariances BETWEEN
      # latent factors only. Restrict to pairs whose names are both latent (the
      # lhs of the =~ measurement lines); without this we would also pick up
      # correlated item residuals and, in a higher-order model, disturbance
      # covariances, none of which are factor correlations.
      latents <- unique(ss$lhs[ss$op == "=~"])
      phi <- ss[ss$op == "~~" & ss$lhs != ss$rhs &
        ss$lhs %in% latents & ss$rhs %in% latents, , drop = FALSE]
      if (!nrow(phi)) {
        return(sieve_dt(
          data.frame(Note = "No inter-factor correlations (a single factor, or a higher-order model with no same-level factor pairs)."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      df <- data.frame(
        Factor.1 = sieve_show_names(phi$lhs, f), Factor.2 = sieve_show_names(phi$rhs, f),
        r = phi$est.std, CI.lower = phi$ci.lower, CI.upper = phi$ci.upper,
        p = phi$pvalue, stringsAsFactors = FALSE
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 15),
        colnames = c("Factor 1", "Factor 2", "r", "95% CI lower", "95% CI upper", "p")
      ) |>
        formatRound(c("r", "CI.lower", "CI.upper", "p"), 3)
    })

    output$est_tbl <- renderDT({
      f <- solved_fit()
      req(f)
      read <- standardized(cfa_run())
      if (!read$ok) {
        return(unreadable(read, "standardized solution"))
      }
      ss <- read$value
      load <- ss[ss$op == "=~", ]
      item_name <- state$meta$label[match(load$rhs, state$meta$item)]
      item_name[is.na(item_name)] <- load$rhs[is.na(item_name)]
      df <- data.frame(
        Factor = sieve_show_names(load$lhs, f), Item = sieve_show_names(load$rhs, f),
        ItemName = item_name,
        Std.loading = load$est.std, SE = load$se,
        CI.lower = load$ci.lower, CI.upper = load$ci.upper,
        stringsAsFactors = FALSE
      )
      est_shown$items <- as.character(load$rhs)
      sieve_dt(df,
        rownames = FALSE, selection = "multiple",
        options = list(dom = "tp", pageLength = 25),
        colnames = c("Factor", "Variable Name", "Item Name", "Std. loading", "SE", "95% CI lower", "95% CI upper")
      ) |>
        formatRound(c("Std.loading", "SE", "CI.lower", "CI.upper"), 3)
    })

    # The modification indices of a fit are computed once per fit (they take
    # lavaan a while on a large model) and kept until another model is fit,
    # keyed on the fit's own identity in the analysis registry, so a fit
    # shown as from before a change is not read again.
    mi_kept <- new.env(parent = emptyenv())
    mi_result <- reactive({
      f <- solved_fit()
      req(f)
      run <- cfa_run()
      # Built by a plain function: lavaan's conditions are kept, a failure is
      # shown with its reason, latent variables carry their labels, and a
      # candidate lavaan left without an index is computed by the score test
      # on the same fit or named (see sieve_modification_indices()).
      sieve_keep(
        mi_kept, "mi", list(id = run$analysis_id, spec = run$spec$key),
        function() sieve_modification_indices(f)
      )
    })
    # The rows lavaan left without an index, and what was done about them.
    output$mi_note <- renderUI({
      said <- sieve_mi_recovery_text(mi_result())
      if (is.null(said)) {
        return(NULL)
      }
      div(class = "small text-muted mb-2", lapply(said, p, class = "mb-1"))
    })
    output$mi_tbl <- renderDT({
      mi <- mi_result()
      if (is.null(mi$table)) {
        return(sieve_dt(
          data.frame(Note = mi$note),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      sieve_dt(mi$table,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25),
        colnames = c("Suggested path", "Kind", "MI", "EPC", "Std. EPC")
      ) |>
        formatRound(c("MI", "EPC", "Std.EPC"), 3)
    })

    # ---- Compare structures ------------------------------------------------
    # The four structures over the same first-order groups (see
    # sieve_structure_plans()), fit on request through the same on-demand
    # machinery as the model above: the button states the estimate for the
    # set, every fit goes through sieve_run_model(), the result is kept with
    # the revision it was computed from, and the line says what changed.
    compare_key <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      sieve_demand_key(state, list(what = "structures", estimator = estimator_choice()))
    })
    compare_built <- reactive({
      req(state$meta)
      sieve_structure_plans(state$meta, reserved = union(names(state$items), names(state$raw)))
    })
    compare_plan <- reactive({
      req(state$meta)
      built <- compare_built()
      spec <- sieve_lavaan_spec(state$meta, estimator = estimator_choice())
      available <- Filter(function(st) isTRUE(st$available), built$structures)
      n <- length(available)
      modeled <- built$groups$items[!is.na(built$groups$group)]
      p <- length(modeled)
      seconds <- sieve_demand_wait("cfa", p, spec$estimator, k = max(1L, n), missing = spec$missing)
      names_of <- vapply(available, function(st) st$label, character(1))
      list(
        estimator = spec$estimator, missing = spec$missing, p = rep(p, max(1L, n)), seconds = seconds,
        label = sieve_duration_label(seconds), n_models = n,
        progress = sprintf(
          "Compare structures: %s, %d items, %d model%s", spec$estimator, p, n,
          if (n == 1L) "" else "s"
        ),
        detail = sprintf(
          "Fitting %s in lavaan, then reading their fit in one family.",
          if (n) paste(tolower(names_of), collapse = ", ") else "no structure"
        ),
        sentence = if (n) {
          sprintf(
            "Fitting the %d structure%s (%s) of %d items under %s is estimated to take %s",
            n, if (n == 1L) "" else "s", paste(tolower(names_of), collapse = ", "), p,
            spec$estimator, sieve_duration_label(seconds)
          )
        } else {
          "No structure can be built from the ledger (the reasons are below)"
        },
        refused = if (p) {
          sieve_wlsmv_guard(modeled, state$items,
            ordered = intersect(spec$ordered %||% character(0), modeled),
            estimator = spec$estimator
          )
        }
      )
    })
    fit_structures <- function(key, plan) {
      res <- sieve_compare_structures(state$meta, state$items,
        reserved = names(state$raw),
        estimator = estimator_choice(), conf_level = sieve_default_rmsea_conf_level()
      )
      for (st in res$structures) {
        if (isTRUE(st$available) && !is.null(st$run$fit)) {
          sieve_register_analysis(state, st$run, note = sprintf("Compare structures: %s", st$label))
        }
      }
      fitted <- vapply(res$structures[res$fitted], function(st) st$label, character(1))
      sieve_log(
        state,
        sprintf(
          "Compare structures on the CFA tab (%s; groups by %s: %s): %s.",
          sieve_lavaan_spec(state$meta, estimator = estimator_choice())$estimator, res$groups$level,
          paste(res$groups$groups, collapse = ", "),
          if (length(fitted)) paste(fitted, collapse = ", ") else "no structure could be built"
        ),
        sieve_structures_code(res)
      )
      res
    }
    compare <- sieve_demand_wire(
      input, session, state,
      id = "compare_go", what = "Compare structures",
      key = compare_key, plan = compare_plan, compute = fit_structures, kind = "cfa",
      noun = "The comparison of structures", verb = "fit",
      action = "Fit the structures", refit = "Update",
      stale_lead = "The data or the item pool changed since the structures were fit: refit them. "
    )
    compare_result <- reactive(compare$status()$value)

    # ---- One Update for the page -------------------------------------------
    # The results of this tab that an edit can leave from before: the model
    # and the compared structures. One action updates the ones that were
    # computed before and are now stale, in turn, under one progress message
    # (each fit's own progress line is shown inside it), and says when it is
    # done. It starts nothing that was never requested: the structures are
    # updated only when they were fit before, and the Bollen-Stine bootstrap
    # and the model-linked composite reliability keep their own buttons.
    page_parts <- list(
      list(wire = demand, plan = cfa_plan, name = "the model"),
      list(wire = compare, plan = compare_plan, name = "the compared structures")
    )
    part_state <- function(part) {
      read <- sieve_outcome(part$wire$status())
      if (read$ready && read$ok) read$value$state else NA_character_
    }
    stale_parts <- reactive({
      Filter(function(part) identical(part_state(part), "stale"), page_parts)
    })
    update_seconds <- reactive({
      secs <- vapply(stale_parts(), function(part) {
        read <- sieve_outcome(part$plan())
        s <- if (read$ready && read$ok) read$value$seconds else NULL
        if (length(s) == 1L) as.numeric(s) else NA_real_
      }, numeric(1))
      sum(secs)
    })
    update_label <- function() sprintf("Update (%s)", sieve_duration_label(update_seconds()))

    # The status line: from before an edit (with the Update), or the model
    # not fit yet or stopped with an error (with its Fit button), or
    # nothing for a current model.
    page_status_line <- function() {
      parts <- stale_parts()
      if (!length(parts)) {
        model <- part_state(page_parts[[1]])
        if (identical(model, "current") || is.na(model)) {
          return(NULL)
        }
        return(demand$line())
      }
      first <- parts[[1]]$wire$status()
      held <- vapply(parts, function(part) {
        st <- part$wire$status()
        sprintf("%s at %s", st$progress %||% part$name, st$when %||% "an earlier time")
      }, character(1))
      model_stale <- identical(parts[[1]]$name, "the model")
      refused <- unlist(lapply(parts, function(part) {
        read <- sieve_outcome(part$plan())
        if (read$ready && read$ok) read$value$refused
      }))
      tagList(
        if (length(refused)) {
          div(class = "alert alert-danger small", strong("Not refit: "), sub("^Not fit: ", "", paste(unique(refused), collapse = " ")))
        },
        div(
          class = "alert alert-warning",
          sieve_lifecycle_badge("stale"),
          strong(if (model_stale) {
            "The data or the item pool changed since this model was fit. "
          } else {
            "The data or the item pool changed since the structures were fit. "
          }),
          sprintf(
            "What changed: %s. The result%s below %s from before (%s).",
            first$sentence, if (length(parts) > 1L) "s" else "",
            if (length(parts) > 1L) "are" else "is", paste(held, collapse = "; ")
          ),
          if (length(parts) > 1L) {
            sprintf(" Update refits %s in turn.", paste(vapply(parts, `[[`, character(1), "name"), collapse = " and "))
          },
          div(class = "mt-2", actionButton(session$ns("update_go"), update_label(), class = "btn-primary"))
        )
      )
    }

    page_update <- sieve_update_wire(input, session, function() {
      entries <- lapply(page_parts, function(part) sieve_update_demand_entry(part$wire, part$name, part$plan))
      stats::setNames(entries, c("model", "comparison"))
    }, completion = function(done, not_done, begun, elapsed) {
      if (!length(not_done) && !length(begun)) return(list(message = sprintf(
        "Update finished in %s: %s %s current.", sieve_seconds_text(elapsed),
        paste(done, collapse = " and "), if (length(done) == 1L) "is" else "are")))
      list(message = sprintf("Update finished in %s, but %s could not be updated; the line above says why.",
        sieve_seconds_text(elapsed), paste(c(not_done, begun), collapse = " and ")),
        type = "warning", duration = NULL)
    })
    run_update <- page_update$run


    # The comparison's own line. From before an edit, it says so and keeps
    # a button that refits the structures alone; the status line above the
    # tabs carries the page's Update.
    output$compare_line <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(NULL)
      }
      st <- compare$status()
      if (!identical(st$state, "stale")) {
        return(compare$line())
      }
      div(
        class = "alert alert-warning",
        strong("The data or the item pool changed since the structures were fit. "),
        sprintf(
          "What changed: %s. The result below is from before (%s at %s). Update above the tabs refits it with the model; this button refits the structures alone.",
          st$sentence, st$progress %||% "Compare structures", st$when
        ),
        div(class = "mt-2", compare$button("Refit the structures"))
      )
    })

    # What the comparison is built on, which structures the ledger does not
    # support and why, the family, and the pair that is not nested.
    output$compare_note <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      built <- compare_built()
      g <- built$groups
      noun <- if (identical(g$level, "facet")) "facet" else "construct"
      groups_text <- if (length(g$groups)) {
        sprintf(
          "The first-order groups are the %d %s%s of the included items: %s%s.",
          length(g$groups), noun, if (length(g$groups) == 1L) "" else "s",
          paste(sprintf("%s (%d item%s)", g$groups, g$sizes, ifelse(g$sizes == 1L, "", "s")), collapse = ", "),
          if (length(g$ungrouped)) {
            sprintf(
              "; %d item%s with no %s %s left out of the grouped structures (%s)",
              length(g$ungrouped), if (length(g$ungrouped) == 1L) "" else "s", noun,
              if (length(g$ungrouped) == 1L) "is" else "are", paste(g$ungrouped, collapse = ", ")
            )
          } else {
            ""
          }
        )
      } else {
        "No included item is assigned to a construct, so there are no groups to build structures on."
      }
      not_built <- Filter(function(st) !isTRUE(st$available), built$structures)
      res <- compare_result()
      tagList(
        div(class = "small text-muted", groups_text),
        if (length(not_built)) {
          div(
            class = "small text-muted",
            "Not fit: ",
            paste(vapply(not_built, function(st) sprintf("%s (%s)", st$label, st$reason), character(1)),
              collapse = "; "
            ), "."
          )
        },
        if (!is.null(res) && !is.null(res$compare)) {
          div(class = "small text-muted", res$compare$common$reason)
        },
        if (!is.null(res)) {
          nn <- res$tests[!res$tests$nested, , drop = FALSE]
          if (nrow(nn)) {
            div(
              class = "small text-muted",
              sprintf(
                "Not nested, so no difference test: %s.",
                paste(sprintf("%s and %s", nn$less_label, nn$more_label), collapse = "; ")
              )
            )
          }
        }
      )
    })

    compare_table <- reactive({
      res <- compare_result()
      req(res)
      sieve_structures_table(res)
    })

    compare_figure <- function() {
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      res <- compare_result()
      validate(need(!is.null(res), "The structures are not fit yet. Press Fit the structures above."))
      gg <- sieve_structures_plot(compare_table(), state$palette %||% "nd")
      validate(need(!is.null(gg), "No structure has fit indices to draw."))
      gg
    }
    output$compare_plot <- renderPlot(compare_figure(), res = 96,
      alt = "Fit indices of the compared structures, each with its interval where one exists. The values are in the comparison table on this page."
    )
    sieve_plot_save_server("compare_plot", output, session, state,
      draw = compare_figure,
      file_stem = "siever_cfa_structures",
      what = "the fit of the compared structures (CFA tab)"
    )

    output$compare_tbl <- renderDT(
      {
        res <- compare_result()
        if (is.null(res)) {
          return(sieve_dt(
            data.frame(Note = "Press Fit the structures above to fit the one-factor, correlated, higher-order and bifactor models and read their fit in one family."),
            rownames = FALSE, selection = "none", options = list(dom = "t")
          ))
        }
        tab <- compare_table()
        family <- attr(tab, "family")
        test_family <- attr(tab, "test_family")
        tag <- function(fam) if (is.null(fam) || is.na(fam)) "" else sprintf(" (%s)", fam)
        words <- c(
          ok = "ok", inadmissible = "improper solution", failed = "could not be fit",
          not_converged = "did not converge", not_identified = "not identified",
          "not fit" = "not fit"
        )
        df <- data.frame(
          Structure = tab$structure,
          Status = unname(words[tab$status]),
          npar = tab$npar,
          chisq = tab$chisq, df = tab$df, p = tab$pvalue,
          CFI = tab$cfi, TLI = tab$tli, RMSEA = tab$rmsea,
          rmsea_ci = ifelse(is.finite(tab$rmsea_lo) & is.finite(tab$rmsea_hi),
            sprintf("[%.3f, %.3f]", tab$rmsea_lo, tab$rmsea_hi), ""
          ),
          SRMR = tab$srmr,
          Note = ifelse(is.na(tab$note), "", tab$note),
          check.names = FALSE, stringsAsFactors = FALSE
        )
        sieve_dt(
          df,
          rownames = FALSE, selection = "none",
          options = list(dom = "t", pageLength = nrow(df), ordering = FALSE),
          colnames = c(
            "Structure", "Status", "Free parameters", paste0("Chi square", tag(test_family)),
            "df", "p", paste0("CFI", tag(family)), paste0("TLI", tag(family)),
            paste0("RMSEA", tag(family)), sprintf("RMSEA %s", sieve_rmsea_ci_words(attr(tab, "conf_level"))),
            "SRMR", "Note"
          )
        ) |>
          formatRound(c("chisq", "p", "CFI", "TLI", "RMSEA", "SRMR"), 3)
      },
      server = FALSE
    )

    output$compare_tests <- renderDT(
      {
        res <- compare_result()
        if (is.null(res)) {
          return(sieve_dt(
            data.frame(Note = "The difference tests of the nested pairs are read with the structures."),
            rownames = FALSE, selection = "none", options = list(dom = "t")
          ))
        }
        t <- res$tests
        df <- data.frame(
          Pair = sprintf("%s, then %s", t$less_label, t$more_label),
          Nested = ifelse(t$nested, "yes: the second is the first with constraints added", "no"),
          Constraint = t$constraint,
          statistic = t$statistic, df = t$df, p = t$p,
          Test = ifelse(is.na(t$label), "", t$label),
          Note = ifelse(t$available, ifelse(is.na(t$note), "", t$note), ifelse(is.na(t$reason), "", t$reason)),
          stringsAsFactors = FALSE
        )
        sieve_dt(
          df,
          rownames = FALSE, selection = "none",
          options = list(dom = "t", pageLength = nrow(df), ordering = FALSE, scrollX = TRUE),
          colnames = c(
            "Pair (less restricted, then more restricted)", "Nested?", "What the second model fixes",
            "Chi square difference", "df", "p", "Test", "Note"
          )
        ) |>
          formatRound(c("statistic", "p"), 3)
      },
      server = FALSE
    )

    # The link to the exploratory range: the EFA tab, whose Fit by number
    # of factors panel fits one solution per number of factors.
    observeEvent(input$go_efa, {
      root <- tryCatch(session$rootScope(), error = function(e) NULL)
      if (!is.null(root)) {
        sieve_go(root, "efa")
      }
    })

    # ---- Bollen-Stine bootstrap test of exact fit --------------------------
    # The model on screen is refit by ML (FIML when a value is missing, the
    # complete cases otherwise) and DMAR::sem_exact_fit_bootstrap() runs the
    # model based bootstrap of Bollen and Stine (1992), with Savalei and
    # Yuan's (2009) transformation for incomplete data (see
    # sieve_bollen_stine()). On request only: every replication is a full
    # refit. DMAR 1.1.0's bootstrap has no progress or cancellation hook, so
    # it runs in a background process (sieve_bollen_stine_start()), as the
    # EFA tab's fits by number of factors do: the page stays responsive, the
    # progress message shows the time elapsed against the estimate, and Stop
    # ends the process. DMAR returns its result only when every replication
    # is done, so a stopped run reports nothing. The refit itself (one fit)
    # runs in the session when the button is pressed, so a refusal or a refit
    # without a solution is reported at once.
    # The result is bound to the dataset, the item data, the ledger, and the
    # bootstrap request it was started from: after any of them changes, the
    # panel says so and shows no old table (see sieve_current_result()), and
    # a change while the bootstrap runs stops it.
    revision_now <- sieve_current_revision(state)
    bs_settings <- reactive(list(
      # DMAR::sem_exact_fit_bootstrap()'s default B (1000) is the page's
      # default; up to 10000 on request.
      B = max(100L, min(as.integer(sieve_finite_or(input$bs_B, 1000L)), 10000L)),
      seed = sieve_seed_input(input$bs_seed),
      transformation = if (identical(input$bs_transformation, "pattern")) "pattern" else "common",
      improper = if (identical(input$bs_improper, "drop")) "drop" else "keep"
    ))
    bs_job <- new.env(parent = emptyenv()) # the run in progress (plain state)
    bs_running <- reactiveVal(FALSE)
    bs_held <- reactiveVal(NULL)
    bs_publication <- reactiveVal(NULL)
    bs_refusal <- function(why) {
      list(result = list(
        error = why, refused = TRUE, status = sieve_fit_status(NULL, error = why),
        warnings = character(0)
      ))
    }
    # The checks and the refit, in the session: the same specification the
    # tab fits (the same model and latent names), on the rows the sampling
    # design keeps.
    bs_prepare <- function(settings) {
      problem <- sieve_sampling_problem(state, capability = "independent")
      if (!is.null(problem)) return(bs_refusal(problem))
      spec <- build_spec()
      prepared <- sieve_sampling_apply(spec, state, state$items)
      if (!is.null(prepared$reason)) return(bs_refusal(prepared$reason))
      plan <- sieve_bollen_stine_plan(prepared$spec, prepared$data,
        B = settings$B, seed = settings$seed,
        transformation = settings$transformation, improper = settings$improper
      )
      if (!is.null(plan$result)) {
        plan$result$warnings <- unique(c(plan$result$warnings, prepared$warnings))
      } else {
        plan$out$warnings <- unique(c(plan$out$warnings, prepared$warnings))
      }
      plan
    }
    # What the panel keeps of a result: the numbers at full precision, the
    # counts, how it was computed, and its seed.
    bs_shown <- function(res) {
      if (!is.null(res$error)) {
        return(list(
          error = res$error, refused = isTRUE(res$refused), stopped = isTRUE(res$stopped),
          error_class = res$error_class, status = res$status, warnings = res$warnings
        ))
      }
      keep <- c(
        "chisq", "df", "p_ml", "p_bs", "p_bs_lower", "p_bs_upper", "bootstrap_mean",
        "B", "B_used", "B_improper", "B_failed", "n", "missing", "transformation",
        "transformation_requested", "transformation_check", "improper", "conf_level", "aux",
        "method", "table", "status", "warnings"
      )
      out <- res[intersect(keep, names(res))]
      out$seed <- res$rng$seed
      out$seed_supplied <- res$rng$supplied
      out$solved <- sieve_has_solution(res$status) && isTRUE(is.finite(res$chisq))
      out
    }
    # Close a run: record it when DMAR ran, keep it with the request and the
    # inputs it was started from, and put it on the page.
    bs_finish <- function(res) {
      if (!is.null(bs_job$progress)) {
        bs_job$progress$close()
        bs_job$progress <- NULL
      }
      bs_job$handle <- NULL
      out <- bs_shown(res)
      out$revision <- bs_job$revision
      out$bs_request_key <- bs_job$request
      if (inherits(res$table, "dmar_tbl")) {
        rng <- res$rng
        sieve_log(
          state,
          sprintf(
            "Bollen-Stine bootstrap test of exact fit (B = %d, seed = %d%s; %s). %s",
            res$B, rng$seed, if (rng$supplied) "" else " drawn for this run",
            res$method, sieve_bollen_stine_counts(res)
          ),
          sieve_bollen_stine_code(res)
        )
      }
      bs_publication(list(
        request = bs_job$request, revision = out$revision,
        state = if (isTRUE(out$stopped)) "not_run" else if (!is.null(out$error) || !isTRUE(out$solved)) "failed" else "current",
        sentence = out$error, when = format(Sys.time(), "%H:%M")
      ))
      bs_held(out)
      bs_running(FALSE)
      invisible(out)
    }
    bs_stop_job <- function(why) {
      handle <- bs_job$handle
      if (!is.null(handle)) tryCatch(handle$kill(), error = function(e) NULL)
      bs_finish(list(error = why, stopped = TRUE, status = bs_job$plan$out$status, warnings = character(0)))
    }
    bs_elapsed_label <- function(seconds) {
      seconds <- max(0, floor(seconds))
      if (seconds < 60) sprintf("%d seconds", as.integer(seconds)) else sprintf("%d min %02d s", as.integer(seconds %/% 60), as.integer(seconds %% 60))
    }
    bs_progress_detail <- function(elapsed) {
      sprintf(
        paste0(
          "%s elapsed of an estimated %s. DMAR's bootstrap does not report its progress ",
          "while it runs, so the bar shows the time elapsed against the estimate."
        ),
        bs_elapsed_label(elapsed), sieve_duration_label(bs_job$wait)
      )
    }
    observeEvent(input$bs_run, {
      if (isTRUE(bs_running())) {
        return()
      }
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      settings <- bs_settings()
      bs_job$settings <- settings
      bs_job$request <- bs_request_key()
      bs_job$revision <- sieve_bind_result(list(), state)$revision
      bs_job$started <- proc.time()[["elapsed"]]
      waited <- sieve_outcome(bs_wait())
      bs_job$wait <- if (waited$ready && waited$ok) waited$value else NA_real_
      bs_job$plan <- NULL
      plan <- bs_prepare(settings)
      if (!is.null(plan$result)) {
        bs_finish(plan$result)
        return()
      }
      bs_job$plan <- plan
      handle <- tryCatch(sieve_bollen_stine_start(plan), error = function(e) e)
      if (inherits(handle, "error")) {
        bs_finish(list(
          error = paste(
            "The Bollen-Stine bootstrap could not be started in a background process:",
            conditionMessage(handle)
          ),
          status = plan$out$status, warnings = plan$out$warnings
        ))
        return()
      }
      bs_job$handle <- handle
      bs_job$progress <- shiny::Progress$new(session, min = 0, max = 1)
      bs_job$progress$set(
        value = 0,
        message = sprintf("Bollen-Stine bootstrap (B = %d, %s)", settings$B, sieve_duration_label(bs_job$wait)),
        detail = bs_progress_detail(0)
      )
      bs_running(TRUE)
    })
    # The worker is looked at four times a second: while it runs the
    # progress message gives the elapsed time; when it has ended its result
    # is read.
    sieve_chunked_wire(bs_running, session, function() {
      handle <- bs_job$handle
      if (is.null(handle)) {
        bs_running(FALSE)
        return(invisible(NULL))
      }
      elapsed <- proc.time()[["elapsed"]] - bs_job$started
      if (handle$is_alive()) {
        if (!is.null(bs_job$progress)) {
          bs_job$progress$set(
            value = min(elapsed / max(sieve_finite_or(bs_job$wait, 1), 1), 0.95),
            detail = bs_progress_detail(elapsed)
          )
        }
        return(invisible(NULL))
      }
      run <- tryCatch(handle$get_result(), error = function(e) list(
        ok = FALSE, value = NULL, error_class = class(e), warnings = character(0),
        error = paste("the background process ended without a result:", conditionMessage(e))
      ))
      res <- sieve_bollen_stine_read(bs_job$plan, run)
      if (inherits(res$table, "dmar_tbl")) {
        # The time it took sets the rate of the next estimate.
        sieve_rate_note("bollen_stine_ML", elapsed,
          sum(sieve_fit_units(rep(length(res$spec$observed), res$B), "ML")))
      }
      bs_finish(res)
    }, interval = 250L)
    output$bs_stop_ui <- renderUI({
      req(isTRUE(bs_running()))
      actionButton(session$ns("bs_stop"), "Cancel the bootstrap", class = "btn-outline-secondary")
    })
    observeEvent(input$bs_stop, {
      if (!isTRUE(bs_running())) {
        return()
      }
      bs_stop_job(sprintf(
        paste0(
          "Stopped: the Bollen-Stine bootstrap was canceled after %s. DMAR returns its result ",
          "only when every replication is done, so a canceled run reports nothing. Press Run ",
          "Bollen-Stine bootstrap to run it again."
        ),
        bs_elapsed_label(proc.time()[["elapsed"]] - bs_job$started)
      ))
    })
    # A change of the data, the model, or the bootstrap settings while the
    # bootstrap runs stops it: its result would not describe the request.
    observe({
      if (!isTRUE(bs_running())) {
        return()
      }
      request <- tryCatch(bs_request_key(), error = function(e) NULL)
      current <- revision_now()
      if (!identical(request, bs_job$request) || !sieve_same_revision(bs_job$revision, current)) {
        isolate(bs_stop_job(paste0(
          "Stopped: the data, the model or the bootstrap settings changed while the ",
          "Bollen-Stine bootstrap ran, so it was stopped. Press Run Bollen-Stine bootstrap ",
          "for the current request."
        )))
      }
    })
    session$onSessionEnded(function() {
      handle <- bs_job$handle
      if (!is.null(handle)) tryCatch(handle$kill(), error = function(e) NULL)
    })
    # The wait the button states: a result run on request says how long it
    # will take. Measured on the bundled examples at B = 250, one replicate
    # takes 0.044 seconds with 9 items and 0.047 with 24, 0.06 to 0.09 of the first estimate of one ML
    # CFA of the same items (which carries a half-second floor; see
    # sieve_fit_wait()), so a tenth of that estimate is the first estimate
    # per replicate; a run replaces it with the rate it measured (from the
    # press of the button to the result, the background process included).
    bs_wait <- reactive({
      req(state$meta)
      bs_publication() # a run measures the rate the estimate then uses
      p <- length(build_spec()$observed)
      B <- bs_settings()$B
      own <- sieve_rate("bollen_stine_ML")
      if (is.finite(own)) {
        own * sum(sieve_fit_units(rep(p, B), "ML"))
      } else {
        B * 0.1 * sieve_demand_wait("cfa", p, "ML")
      }
    })
    observe({
      read <- sieve_outcome(bs_wait())
      updateActionButton(session, "bs_run", label = if (read$ready && read$ok && is.finite(read$value)) {
        sprintf("Run Bollen-Stine bootstrap (%s)", sieve_duration_label(read$value))
      } else {
        "Run Bollen-Stine bootstrap"
      })
    })
    # What the refit will be, read from the data without fitting: whether a
    # value is missing (so the refit is FIML and the transformation matters)
    # and which auxiliary variables it carries.
    bs_scope <- reactive({
      prepared <- sieve_sampling_apply(build_spec(), state, state$items)
      columns <- intersect(
        unique(c(prepared$spec$observed, prepared$spec$engine_args$aux)),
        names(prepared$data)
      )
      incomplete <- length(columns) > 0L && anyNA(prepared$data[, columns, drop = FALSE])
      list(
        incomplete = incomplete,
        aux = if (incomplete) as.character(prepared$spec$engine_args$aux %||% character(0)) else character(0)
      )
    })
    # With auxiliary variables in the refit, improper resamples are always
    # kept (see sieve_bollen_stine()), so Set aside is not offered.
    observe({
      scope <- tryCatch(bs_scope(), error = function(e) NULL)
      aux <- length(scope$aux) > 0L
      current <- isolate(input$bs_improper) %||% "keep"
      updateSelectInput(session, "bs_improper",
        choices = if (aux) c("Keep in the p-value" = "keep") else c("Keep in the p-value" = "keep", "Set aside" = "drop"),
        selected = if (aux) "keep" else current
      )
    })
    # The transformation applies to incomplete data only: with complete data
    # a change of it changes nothing, so it is not part of the request.
    bs_request_key <- reactive({
      settings <- bs_settings()
      scope <- tryCatch(bs_scope(), error = function(e) list(incomplete = TRUE))
      if (!isTRUE(scope$incomplete)) settings$transformation <- NA_character_
      sieve_hash(list(
        spec = build_spec()$key, settings = settings,
        sampling = sieve_sampling_spec(state), rows = state$case_index
      ))
    })
    bs_result_run <- reactive({
      req(bs_held())
      bs_held()
    })
    observe({
      held <- bs_publication()
      running <- isTRUE(bs_running())
      if (is.null(held) && !running) return()
      if (running) {
        sieve_page_publish(state, sub("-.*$", "", session$ns("")), session$ns("bs_run"),
          "Bollen-Stine bootstrap", list(state = "running", key = list(request = bs_job$request,
            revision = bs_job$revision)),
          action = "Run Bollen-Stine bootstrap")
        return()
      }
      current <- revision_now()
      request <- sieve_outcome(bs_request_key())
      phase <- held$state
      sentence <- held$sentence
      if (!request$ready || !request$ok || !identical(held$request, request$value) ||
        !sieve_same_revision(held$revision, current)) {
        phase <- "stale"
        sentence <- "The data, model or bootstrap settings changed; run Bollen-Stine again for this request."
      }
      sieve_page_publish(state, sub("-.*$", "", session$ns("")), session$ns("bs_run"),
        "Bollen-Stine bootstrap", list(state = phase, sentence = sentence,
          key = list(request = held$request, revision = held$revision), when = held$when),
        action = "Run Bollen-Stine bootstrap")
    })
    bs_result <- reactive({
      out <- sieve_current_result(bs_result_run(), revision_now(),
        "Bollen-Stine bootstrap", "Run Bollen-Stine bootstrap")
      if (isTRUE(out$stale)) return(out)
      if (!identical(out$bs_request_key, bs_request_key())) return(list(
        error = "The model or bootstrap settings changed. Run Bollen-Stine bootstrap again for the current request.",
        stale = TRUE
      ))
      out
    })

    output$bs_note <- renderUI({
      if (isTRUE(bs_running())) {
        return(div(
          class = "text-muted small",
          sprintf(
            paste0(
              "The Bollen-Stine bootstrap is running in the background (B = %d; estimated %s). ",
              "DMAR's bootstrap does not report its progress while it runs, so the progress ",
              "message shows the time elapsed against the estimate. Cancel ends the run; a ",
              "canceled run reports nothing."
            ),
            bs_job$settings$B, sieve_duration_label(bs_job$wait)
          )
        ))
      }
      read <- sieve_outcome(bs_result())
      if (!read$ready || (read$ok && is.null(read$value))) {
        return(div(
          class = "text-muted small",
          "Press Run Bollen-Stine bootstrap to compute the bootstrap p for the exact-fit test."
        ))
      }
      if (!read$ok) {
        return(div(
          class = "alert alert-danger",
          strong("The Bollen-Stine bootstrap stopped with an error: "), read$error
        ))
      }
      r <- read$value
      if (!is.null(r$error)) {
        # A refusal (SIEVER's or DMAR's, with its reason), a stopped run, or
        # a result for other inputs.
        return(div(class = "alert alert-info", r$error))
      }
      # The refit's status comes first: no p-value describes a model that did
      # not converge or is not identified, and none is shown for one.
      alert <- sieve_status_alert(r$status, what = "bootstrap refit", warnings = r$warnings)
      if (!isTRUE(r$solved) || !inherits(r$table, "dmar_tbl")) {
        return(alert)
      }
      check <- r$transformation_check
      check_line <- if (length(check) && !identical(r$transformation, "bollen_stine")) {
        g <- check[[1L]]
        sprintf(
          paste0(
            " Transformation check (DMAR%s): %d missing data patterns; the saturated FIML ",
            "estimating equations of the transformed data hold to %s on the standardized ",
            "scale%s, and the means of every pattern equal the model implied means to %s."
          ),
          if (length(check) > 1L) ", first group" else "", as.integer(g$patterns),
          format(signif(g$score, 3)),
          if (isTRUE(g$iterations > 0L)) sprintf(" after %d Newton steps", as.integer(g$iterations)) else "",
          format(signif(g$mean_residual, 3))
        )
      } else {
        ""
      }
      tagList(
        alert,
        if (!isTRUE(is.finite(r$p_bs))) {
          div(
            class = "alert alert-warning",
            paste0(
              "DMAR reports no bootstrap p-value: fewer than half of the attempted resamples, ",
              "or fewer than two, were used, and a p-value from the resamples that happened to ",
              "converge would describe a selected distribution."
            )
          )
        },
        div(
          class = "text-muted small",
          sprintf(
            paste0(
              "%s. %s Seed = %d%s.%s The Monte Carlo interval is the uncertainty in the ",
              "bootstrap p from using %d resamples rather than infinitely many (Wilson, %s ",
              "percent); it is not an interval for anything about the model. Read the two ",
              "p-values together: a normal-theory rejection that the bootstrap does not echo ",
              "points at non-normality rather than misfit."
            ),
            r$method, sieve_bollen_stine_counts(r), r$seed,
            if (isTRUE(r$seed_supplied)) "" else " (drawn for this run)", check_line,
            r$B_used, format(100 * r$conf_level)
          )
        )
      )
    })

    output$bs_tbl <- renderDT({
      req(!isTRUE(bs_running()))
      read <- sieve_outcome(bs_result())
      req(read$ready, read$ok)
      r <- read$value
      req(r, is.null(r$error), isTRUE(r$solved))
      # Rounded for display only; the result keeps full precision.
      df <- sieve_bollen_stine_rows(r)
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t", pageLength = nrow(df))
      )
    })

    # ---- AIPE planning for the RMSEA interval ------------------------------
    # Prefill the planning RMSEA and df from the fitted model once per fit,
    # as the tab displays it (see fm_shown above).
    # The value prefilled and the family it was read in, so the result can
    # say which RMSEA it plans from (the planner is normal theory; the value
    # shown under MLR or WLSMV is the robust RMSEA).
    plan_prefill <- reactiveVal(NULL)
    observe({
      m <- fm_shown()
      req(m)
      if (is.finite(m["rmsea"])) {
        value <- round(max(m["rmsea"], 0.005), 3)
        plan_prefill(list(
          rmsea = value, estimate = unname(m["rmsea"]),
          family = sieve_fit_family(m, "index"), label = sieve_fit_label(m, "rmsea")
        ))
        updateNumericInput(session, "plan_rmsea",
          value = value
        )
      }
      if (is.finite(m["df"]) && m["df"] >= 1) {
        updateNumericInput(session, "plan_df", value = as.integer(m["df"]))
      }
    })

    # The plan for the inputs as they stand: the inputs, the DMAR call's
    # outcome, and the required N. The display reads it; the Reproducible
    # Code receives it only when the researcher asks (plan_rmsea_log), with
    # the inputs written as exact R literals, so the recorded call returns
    # the N the panel shows.
    plan_rmsea_now <- reactive({
      r0 <- sieve_finite_or(input$plan_rmsea, NA_real_)
      dfm <- sieve_finite_or(input$plan_df, NA_real_)
      w <- sieve_finite_or(input$plan_rmsea_width, 0.05)
      if (!is.finite(r0) || !is.finite(dfm)) {
        return(list(ready = FALSE, rmsea = r0, df = dfm, width = w))
      }
      # The interval planned is at the level the Fit sub-tab shows the
      # RMSEA interval (the RMSEA level, 0.90 by default, DMAR's default for
      # ss_aipe_rmsea()); the level is passed explicitly, so a level set
      # through sieve.rmsea_conf_level is the level planned.
      planned <- sieve_capture(
        DMAR::ss_aipe_rmsea(
          true_rmsea = r0, df = as.integer(dfm), width = w,
          conf_level = sieve_default_rmsea_conf_level()
        )
      )
      n_req <- if (planned$ok) {
        res <- planned$value
        res$value[res$term == "necessary_N"]
      } else NA_real_
      list(
        ready = TRUE, rmsea = r0, df = as.integer(dfm), width = w, planned = planned,
        conf_level = sieve_default_rmsea_conf_level(),
        n = if (length(n_req) == 1L) n_req else NA_real_
      )
    })
    observeEvent(input$plan_rmsea_log, {
      plan <- plan_rmsea_now()
      if (!isTRUE(plan$ready) || !isTRUE(plan$planned$ok) || !is.finite(plan$n)) {
        sieve_notify(
          "There is no plan to add: enter a planning RMSEA, the model df and a target width the planner accepts.",
          type = "warning"
        )
        return()
      }
      sieve_log(
        state,
        sprintf(
          paste(
            "Plan the next study: n for an RMSEA %s CI of full width %s (planning RMSEA = %s,",
            "df = %d; AIPE, normal theory; required N = %d)."
          ),
          sieve_ci_percent(plan$conf_level), sieve_num_text(plan$width), sieve_num_text(plan$rmsea),
          plan$df, as.integer(plan$n)
        ),
        sprintf(
          "DMAR::ss_aipe_rmsea(true_rmsea = %s, df = %d, width = %s, conf_level = %s)",
          sieve_num_lit(plan$rmsea), plan$df, sieve_num_lit(plan$width), sieve_num_lit(plan$conf_level)
        )
      )
      sieve_notify("The plan was added to the Reproducible Code.")
    })

    output$plan_rmsea_result <- renderUI({
      invisible(NULL)
      plan <- plan_rmsea_now()
      r0 <- plan$rmsea
      dfm <- plan$df
      w <- plan$width
      if (!isTRUE(plan$ready)) {
        return(div(
          class = "text-muted small",
          "Fit the model (or enter a planning RMSEA and df) to plan."
        ))
      }
      planned <- plan$planned
      if (!planned$ok) {
        return(div(
          class = "alert alert-danger",
          "The planner could not run for these inputs: ", planned$error
        ))
      }
      n_req <- plan$n
      req(length(n_req) == 1L, is.finite(n_req))
      # Which RMSEA the planning value is: the one prefilled from the fit
      # (named with its family), or one entered here.
      held <- plan_prefill()
      source <- if (!is.null(held) && isTRUE(all.equal(r0, held$rmsea))) {
        sprintf(
          "The planning RMSEA is the %s of the model fit on this tab (%.3f, rounded to %.3f for the planner)%s.",
          held$label, held$estimate, held$rmsea,
          if (identical(held$family, "robust")) {
            ", which estimates the population RMSEA the normal theory formula defines"
          } else ""
        )
      } else {
        "The planning RMSEA was entered here, not read from the fit."
      }
      div(
        class = "alert alert-success",
        HTML(sprintf(
          paste0(
            "<b>Required N: %d.</b> At a planning RMSEA of %.3f with %d degrees of ",
            "freedom, a sample of %d gives an expected %s confidence interval no ",
            "wider than %.3f (Kelley & Lai, 2011). The %s level is the level of the RMSEA ",
            "interval the Fit sub-tab reports, so the plan is for the interval this page shows."
          ),
          as.integer(n_req), r0, as.integer(dfm), as.integer(n_req),
          sieve_ci_percent(plan$conf_level), w, sieve_ci_percent(plan$conf_level)
        )),
        tags$div(
          class = "small mt-1",
          source, " ",
          paste0(
            "The computation is normal theory: it plans the width of the maximum likelihood ",
            "interval from the noncentral chi square. A robust or scaled interval reported under ",
            "MLR or WLSMV in the next study is a different interval, whose width this planner ",
            "does not compute."
          )
        )
      )
    })
    sieve_missing_info_server(input, output, session, state, cfa_run,
      function() identical(demand$status()$state, "current"))
    sieve_mi_server(input, output, session, state, cfa_run,
      function() identical(demand$status()$state, "current"))
    inference <- sieve_cfa_evidence_server(
      input, output, session, state, cfa_run,
      function() identical(demand$status()$state, "current")
    )
  })
}

#' The CFA tab's method statement, in one sentence
#'
#' What the result above every sub-tab comes from: the model and the program,
#' how the latent scales are set, the estimator and what it is fit to, and the
#' cases used with n. The estimator words are the resolved choice's own
#' (`est$label`, `est$basis`), so a new choice reads correctly without a
#' change here.
#'
#' @param spec A `sieve_cfa_spec()` specification (the model on screen, or
#'   the one the controls make before a fit).
#' @param est A `sieve_estimator_resolve()` result for the modeled items.
#' @param rows The rows in the analysis the model was fit to (for a fitted
#'   model, the rows when it was fit; before a fit, the rows now).
#' @param used The number of rows lavaan used (`lavInspect(fit, "nobs")`,
#'   summed over groups) for a fitted model, or `NULL` before a fit.
#' @param rows_now The rows in the analysis now, or `NULL`. For a fitted
#'   model whose `rows` differ from them, the sentence says so separately,
#'   so the n of a result
#'   from before is never paired with the rows of the current request.
#' @return One sentence.
#' @author Ken Kelley
#' @noRd
sieve_cfa_method_text <- function(spec, est, rows, used = NULL, rows_now = NULL) {
  p <- length(spec$observed)
  k <- length(spec$latent)
  model <- switch(spec$structure %||% "first_order",
    facet_first_order = "correlated-facet",
    higher_order = "higher-order",
    "correlated-factors"
  )
  ident <- if (isTRUE(spec$std_lv)) "std.lv = TRUE" else "marker indicators fixed at one"
  fitted_to <- if (identical(est$basis, "Pearson")) {
    "Pearson covariances"
  } else {
    sprintf("%s correlations and thresholds", est$basis)
  }
  cases <- if (identical(spec$missing, "fiml")) "FIML" else
    if (spec$missing %in% c("two.stage", "robust.two.stage")) "two-stage ML from available-data EM moments" else "complete cases"
  moved <- length(used) == 1L && is.finite(used) && length(rows_now) == 1L &&
    is.finite(rows_now) && !identical(as.integer(rows_now), as.integer(rows))
  n_text <- if (moved) {
    d <- as.integer(rows) - as.integer(rows_now)
    sprintf(
      "n = %d of %d rows when fit (%s); %d rows are in the analysis now, %d %s than then",
      as.integer(used), as.integer(rows), cases, as.integer(rows_now), abs(d),
      if (d > 0L) "fewer" else "more"
    )
  } else if (length(used) == 1L && is.finite(used)) {
    sprintf("n = %d of %d rows (%s)", as.integer(used), as.integer(rows), cases)
  } else {
    sprintf("%d rows supplied (%s)", as.integer(rows), cases)
  }
  # A FIML fit with declared auxiliary variables (entered only when an
  # analyzed item is missing) names them beside the items they accompany.
  aux <- as.character(spec$engine_args$aux %||% character(0))
  aux_text <- if (length(aux)) {
    sprintf(
      ", with %d auxiliary variable%s as %s (%s)", length(aux), if (length(aux) == 1L) "" else "s",
      if (length(aux) == 1L) "a saturated correlate" else "saturated correlates", paste(aux, collapse = ", ")
    )
  } else {
    ""
  }
  sprintf(
    "%s CFA, %d item%s on %d factor%s (lavaan::cfa(), %s)%s; %s, %s; %s.",
    model, p, if (p == 1L) "" else "s", k, if (k == 1L) "" else "s", ident, aux_text,
    est$label, fitted_to, n_text
  )
}

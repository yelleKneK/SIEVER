# mod_reliab.R
# Reliability module. Reports McDonald's omega for each construct with a
# confidence interval (Kelley & Pornprasertmanit, 2016), the house default
# reliability coefficient: a factor analytic estimate that does not assume
# tau equivalence. Coefficient alpha is offered only for comparison with
# older reports, never as the headline. The calculations live in
# fct_reliability.R as plain functions: each construct's request is resolved
# into a specification (the coefficient its item types call for, the missing
# data treatment and the interval method actually used, and the reason when
# a request cannot be honored), and this module only collects the choices
# and displays the results. Estimates come from DMAR::reliability_omega and,
# for constructs whose items are all ordered or binary,
# DMAR::reliability_omega_categorical (the Green & Yang, 2009, coefficient
# for the sum of the items' category ranks). DMAR's `denominator` argument distinguishes
# robust omega, whose denominator is the variance of the composite
# estimated from the data, from the textbook model implied omega. Robust
# omega is the default, and the intervals are DMAR's defaults: model implied
# omega (beside robust omega, labeled as assuming the one-factor model, with
# that model's RMSEA) takes the MLR (sandwich) SE with logit-scale limits,
# categorical omega the BCa bootstrap, and robust omega, which has no closed
# form interval, the percentile bootstrap DMAR names for it. Only the denominator of robust
# omega is free of the one-factor model; its numerator is that model's. A
# bootstrap runs only when the user asks for it, with an estimate of the
# wait shown beforehand; until then the row says it has not been run. The
# point estimate and its interval are shown
# both as a table and as a point and error bar plot, in keeping with the AIPE
# emphasis on precision over point values.

#' Reliability module UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput plotOutput selectInput numericInput checkboxInput conditionalPanel helpText
#' @importFrom bslib layout_sidebar sidebar navset_card_tab nav_panel card card_header accordion accordion_panel
#' @importFrom DT DTOutput
mod_reliab_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      open = "desktop",
      width = 300,
      id = ns("sidebar"),
      conditionalPanel("input.panels !== 'scoring'", ns = ns,
      tags$h4("Reliability"),
      # Estimation first: the item treatment (which coefficient and which
      # model) and the missing data treatment, then the omega variant and
      # the interval. The treatment follows the study's estimator setting on
      # the Ingest tab unless chosen otherwise here (sieve_reliability_treat()).
      selectInput(ns("scope"), "Reporting scope",
        choices = c(
          "Construct totals" = "construct", "Facets within constructs" = "facet",
          "Construct totals and facets" = "combined"
        ), selected = "construct"
      ),
      helpText("Facet coefficients describe each facet's own items. They are distinct from construct totals and conditional or bifactor subscale coefficients."),
      sieve_settings("Estimation and Intervals",
        ids = ns(c("treatment", "missing", "denominator", "ci_method", "B", "boot_seed", "small_sample_t")),
      selectInput(
        ns("treatment"), "Item treatment",
        choices = c("The study's setting (Ingest tab)" = "study", sieve_reliability_treatments()),
        selected = "study", width = "100%"
      ),
      uiOutput(ns("treatment_src")),
      radioButtons(
        ns("missing"), "Missing data",
        choices = c("Follow the study" = "study", "FIML (all available data)" = "fiml",
          "Listwise deletion (comparison)" = "listwise"),
        selected = "study"
      ),
      sieve_more_below(
        "FIML uses available responses for continuous items. The ordinary ordered or binary table uses complete cases. Categorical Omega Under MAR below offers a separate MI sensitivity analysis.",
        tags$p(
          "FIML fits the one factor model to the raw item data, so a respondent contributes exactly the information their observed items hold, and a respondent with no observed item or auxiliary contributes nothing. Under the model's assumptions it gives consistent estimates when the data are missing at random given the items in the model; listwise deletion generally needs the stronger assumption that they are missing completely at random. With FIML the ML and MLR (sandwich) standard errors come from the observed information matrix, and the bootstrap resamples the partially observed rows. FIML is not offered for ordered or binary items; a construct made of them uses complete cases, and the table says so."
        ),
        label = "Missing-data assumptions and computation"
      ),
      selectInput(
        ns("denominator"), "Variant of Coefficient Omega, \u03c9",
        choices = c(
          "Robust omega (observed denominator)" = "observed",
          "Omega (model-implied denominator)" = "model_implied"
        ),
        selected = "observed"
      ),
      sieve_more_below(
        "The variants use the same model-based numerator and different estimates of total score variance.",
        tags$p(
          sieve_reliability_variant_text()
        ),
        label = "The two omega denominators"
      ),
      checkboxInput(ns("show_difference"),
        "Show the denominator diagnostic: robust minus model-implied omega",
        value = FALSE
      ),
      sieve_more_below(
        "Adds a diagnostic of the difference between the observed and model-implied denominators.",
        tags$p(
          paste0(
            "Optional columns. Robust minus model-implied omega reflects the summed residual covariance ",
            "within the composite, 1'(S - \u03a3\u0302)1, the amount by which the observed denominator ",
            "exceeds the model-implied one: negative when the one-factor model leaves out positive ",
            "covariance among the items, zero when it reproduces the composite's variance. It describes ",
            "the misfit in the composite's variance; it is not a test. The summed residual covariance is ",
            "shown for continuous items, on the scale of the item scores."
          )
        ),
        label = "What the denominator diagnostic means"
      ),
      selectInput(
        ns("ci_method"), "Confidence interval method",
        choices = sieve_reliability_methods(),
        selected = "recommended"
      ),
      uiOutput(ns("method_note")),
      checkboxInput(ns("small_sample_t"), "Use FMI t Intervals for Model-Implied Omega", value = FALSE),
      helpText("Opt-in sensitivity analysis: DMAR uses df = max(3, N(1 - FMI) - 1), originally calibrated for continuous model-implied omega with ML or MLR (sandwich) SE Wald intervals on the scale of omega, so under it the default model-implied interval is the MLR (sandwich) SE with limits on that scale (DMAR's default under the rule) rather than the logit-scale one. Current likelihood-eligibility checks change which samples qualify; coverage under that selection has not been established. Auxiliary-variable fits use the rule by analogy because the calibration used no auxiliaries. The observed-denominator bootstrap, H and factor-score intervals keep their stated methods. Normal-reference intervals remain the default."),
      sieve_more_below(
        "The table names the interval method actually used. Bootstrap intervals run only when requested.",
        tags$p(
          "The first choice, DMAR's defaults, is resolved construct by construct: Omega (model-implied denominator) takes the MLR (sandwich) standard error with its limits formed on the logit scale, DMAR's default; categorical omega takes DMAR's BCa bootstrap; robust omega, which has no closed form interval, takes the percentile bootstrap DMAR names for it (Kelley & Pornprasertmanit, 2016), which runs on request. Every other method below stays available. ", sieve_reliability_limits_study_text(), " The MLR (sandwich) SE and ML intervals are Wald intervals from the delta method standard error of omega. ML is the normal theory version. The MLR (sandwich) standard error does not assume normal items: with complete cases the engine uses lavaan's robust.sem standard error (the Satorra and Bentler, 1994, correction), and with FIML the Huber-White sandwich built from the observed information (Yuan & Bentler, 2000). This is not the same thing as lavaan's MLR estimator on the CFA tab, which pairs Huber-White standard errors with a Yuan-Bentler type scaled test statistic, not the Satorra-Bentler test. Each comes in two forms from the same standard error: limits on the scale of omega, symmetric about the estimate and truncated to [0, 1], or, for the logit scale choices, limits formed on the logit scale, log(omega / (1 - omega)), whose standard error the delta method gives as SE / (omega (1 - omega)), and transformed back (Browne, 1982), so they lie inside (0, 1) and are asymmetric near one, as the sampling distribution of a high reliability is. ADF, logit scale, uses the asymptotic distribution-free standard error of a weighted least squares fit (Browne, 1984), which needs complete cases and a fairly large sample. Profile likelihood inverts the likelihood ratio test, respects [0, 1], and need not be symmetric; the engine offers it for complete cases only. The bootstrap intervals are nonparametric; they are the only intervals available for robust omega and for categorical omega. The percentile interval takes quantiles of the replications, BCa adjusts them for bias and acceleration, and the bootstrap standard error, logit scale, uses the standard deviation of the replications as a standard error on the logit scale (Kelley & Pornprasertmanit, 2016, Eq. 32). All three are DMAR's counted bootstrap (DMAR 1.1.0), run in a background R process so the page stays responsive, with progress shown and a Cancel that cancels at the next replication (on several processes, at the end of the block of replications in progress; the process is ended if DMAR has not stopped five seconds after the press): it resamples the rows the estimate used (with their missing data patterns under FIML), refits each replication with DMAR's point estimator, keeps replications with a finite estimate, including improper solutions that still yield a coefficient (improper = \"keep\"), sets aside and counts the rest, forms no interval when fewer than half of the replications return a value (its half rule), and reports how many replications were attempted, converged, were admissible, and were used. The actual method is reported if BCa falls back to percentile. With a declared cluster design SIEVER's own counted bootstrap resamples whole clusters instead."
        ),
        label = "Interval methods and assumptions"
      ),
      conditionalPanel(
        condition = sprintf("['recommended', 'percentile', 'dmar_percentile', 'dmar_bca', 'bootstrap_se_logistic'].includes(input['%s'])", ns("ci_method")),
        # DMAR's default number of replications; the wait is stated before a
        # run starts.
        sieve_boot_inputs(ns, B = 10000, B_id = "B", seed_id = "boot_seed", min_B = 100, max_B = 100000, step_B = 100, label = "Bootstrap replications", width = NULL, seed_label = "Bootstrap seed (optional)", seed_width = NULL),

        uiOutput(ns("boot_note")),
        uiOutput(ns("stop_boot_ui"))
      )
      ),
      sieve_conf_level_input(ns("conf_level"), "Confidence level", value = 0.95, min = 0.80, max = 0.99, step = 0.01),
      checkboxInput(ns("show_alpha"),
        "Also show Coefficient Alpha, \u03b1 (for comparison with older reports)",
        value = FALSE
      ),
      sieve_more_below(
        "Alpha is available for comparison with older reports; its interval method is reported separately.",
        tags$p(
          "Alpha uses DMAR's analytic covariance estimator on these same items and missing-data treatment. Its interval is identified separately. Under DMAR's defaults, and while the bootstrap selected for omega has not been run, it is DMAR's default for alpha: Bonett (2002) with complete cases, and the ML delta interval under FIML. An interval chosen for omega gives alpha its counterpart: an ADF delta interval for a listwise MLR (sandwich) SE request (ADF, logit scale, for its logit scale form), an ML delta interval under FIML (in the same form), and Bonett for a profile-likelihood request. Bootstrap alpha is DMAR's counted bootstrap of analytic alpha (percentile, BCa, or the bootstrap standard error on the logit scale), run in the same background process as omega's, and records its actual method and counts. These choices keep alpha's point estimand unchanged."
        ),
        label = "How alpha and its interval are computed"
      )
      ),
      conditionalPanel("input.panels === 'scoring'", ns = ns,
        tags$h4("Scoring and Norms"),
        p("Scoring policies and sample reference controls are beside their tables.")
      )
    ),
    # The page's one status line: what is not computed or from before, with
    # the page's one action (Compute reliability for a first table, Update
    # for everything an edit made stale).
    uiOutput(ns("gate")),
    navset_card_tab(
      id = ns("panels"),
      nav_panel(
        "Reliability table",
        card(
          # Results first: the method line and the table come
          # first; the bootstrap notice, the method of each row and the
          # explanation follow it. "Show all columns" is a display choice
          # beside the title, so it takes no height above the table.
          card_header(
            class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
            tags$span("Reliability by Construct or Facet: Omega and Alpha With Uncertainty"),
            tags$div(
              class = "sieve-table-columns small",
              style = "margin-bottom:-1rem;",
              checkboxInput(ns("show_all"), "Show all columns", value = FALSE, width = "auto")
            )
          ),
          uiOutput(ns("method_line")),
          DTOutput(ns("omega_tbl"), fill = FALSE),
          uiOutput(ns("boot_main")),
          uiOutput(ns("not_estimable")),
          uiOutput(ns("footnote")),
          uiOutput(ns("status_notes")),
          tags$details(open = NA,
            class = "mt-2 mb-2",
            tags$summary("More on how to read this table: why \u03c9, not \u03b1, the two denominators, H and reference values"),
            uiOutput(ns("footnote_more")),
            tags$div(
              class = "small",
              tags$p(
                tags$b("Coefficient Omega, \u03c9, is the reliability coefficient here."),
                " Omega is model-based: it is the proportion of total score variance attributable to the common factor in the congeneric (one-factor) measurement model, the squared sum of the loadings over the total variance of the composite (McDonald, 1999), which Robust omega (observed denominator) estimates from the data and Omega (model-implied denominator) takes from the model. Because it lets loadings differ across items, it is the right estimate for the realistic case in which items are unequally good indicators."
              ),
              tags$p(
                tags$b("Coefficient Alpha, \u03b1, is not a general lower bound."),
                " Alpha equals the reliability of the sum score only when the items are essentially tau-equivalent (equal loadings; the intercepts and the error variances may differ) and their errors are uncorrelated. When the errors are uncorrelated but the loadings differ, alpha falls below the reliability, which is the sense in which it is called a lower bound. When errors are correlated, or the item set is multidimensional, alpha can exceed the reliability, so the lower bound reading holds only under its assumptions. For these reasons alpha is reported here as a flagged column for comparison with older reports only, never as the basis for a retention or scoring decision (Kelley & Pornprasertmanit, 2016; Flora, 2020)."
              ),
              tags$p(
                tags$b("Two variants of \u03c9, differing in the denominator."),
                " Robust omega (observed denominator), the default here, is what Kelley and Pornprasertmanit (2016) introduced as hierarchical omega (MBESS::ci.reliability(type = \"hierarchical\")); it is not the bifactor omega hierarchical of Zinbarg et al. (2005). It is (\u03a3\u03bb)\u00b2 / 1'S1: the numerator is the squared sum of the loadings \u03bb of the one-factor model with the factor variance fixed to 1, and the denominator is the variance of the unit-weighted composite actually scored, from the unrestricted covariance matrix S of the items (ML divisor N; under FIML the saturated FIML estimate). Omega (model-implied denominator) is (\u03a3\u03bb)\u00b2 / [(\u03a3\u03bb)\u00b2 + \u03a3\u03c8], the variance the fitted model reproduces. The two denominators differ by exactly 1'(S - \u03a3\u0302)1, the sum of the residual covariances: the model-implied version treats covariance the model leaves out as nonexistent, while robust omega counts it as composite variance not due to the target factor. Under a correct model the two estimate the same parameter, and Omega (model-implied denominator) is the reliability of the composite only then. What is robust is the denominator, and only against misspecification of the total variance: robust here is not robustness to outliers (Zhang & Yuan, 2016, use \"robust omega\" in that sense) and not robust ML standard errors. What is not robust is the numerator, which comes from the one-factor model under both denominators, so a doublet or a method factor among a few same-signed items inflates omega either way: robust omega does not repair a misspecified model, and neither variant does (Flora, 2020). The intervals are DMAR's defaults (the first choice of the interval menu). Robust omega has no closed form interval, so its interval is a bootstrap, the percentile bootstrap DMAR names for it (Kelley & Pornprasertmanit, 2016), and categorical omega's is DMAR's BCa bootstrap: the point estimate is shown when the table is computed, and the bootstrap runs on request, with its wait stated before it starts. Beside robust omega the table shows Omega (model-implied denominator) with DMAR's default interval, the MLR (sandwich) standard error with its limits formed on the logit scale (Browne, 1982), which is immediate and assumes the one-factor model, and, with Show all columns, that model's RMSEA with its interval at the RMSEA level (90% by default, DMAR's and lavaan's), so the assumption can be judged. Constructs whose items are all ordered or binary use categorical omega, Green and Yang's (2009) coefficient for the sum of the items' category ranks (each item's observed categories scored 0, 1, ..., C - 1 in the order of its codes, as DMAR::reliability_omega_categorical() scores them), computed from the polychoric solution: with the observed denominator its total variance is built from the unrestricted polychoric correlations and the thresholds (Kelley & Pornprasertmanit, 2016), which is not the literal variance of the sum score, and with the model-implied denominator from the polychoric correlations the one-factor model implies (Green & Yang, 2009). The sum of the category ranks has the same reliability as the sum of the codes SIEVER scores only when every item's codes step by one shared amount (a shift of an item, or its reversal, changes nothing). When they do not (an unused category between two used ones, a missing-value code such as 9 left among the responses, or codes such as 1, 2, 5, 10), the two are different composites: the method note names each such item with its codes as administered and as scored, the table's Composite scored and Coded composite columns show it, and SIEVER gives the coded composite's reliability under the same fitted model as a point estimate (DMAR forms intervals for the rank composite only). A bootstrap replication that omits a sparse interior category re-ranks the categories it holds, so the method note also names such categories beside a bootstrap interval, with the share of replications expected to omit each."
              ),
              tags$p(
                tags$b("Read the interval, not just the point estimate."),
                " Following the AIPE principle, a coefficient is reported with a confidence interval wherever the selected method supports one (Kelley & Pornprasertmanit, 2016), and the table states, construct by construct, which interval was used. When no interval is shown the reason is given under the table: the method does not apply to that coefficient, a bootstrap has been selected but not run, or the engine could not form the interval. A point omega of .82 means little if the interval runs from .68 to .91; a narrow interval is the actual goal. Judge a construct on where its lower limit sits and on the width of the interval, and prefer adding well-targeted items or N over chasing a higher point value. To plan the sample size a target interval width requires before data collection, use DMAR::ss_aipe_reliability(), the AIPE planner for reliability coefficients."
              ),
              tags$p(
                tags$b("H (construct replicability / maximal reliability)"),
                " is the reliability of the optimally weighted composite of these items under the one-factor model, the best any weighted sum of them could do (Hancock & Mueller, 2001); under that model the reliability of the unit-weighted composite (model implied omega) does not exceed it, and the gap between them is what unit weighting gives up relative to the optimal weights of that model. H is attained by that model's own factor score: when the one-factor model holds with uncorrelated errors and its parameters are taken as known, the squared determinacy of its regression score equals H. It is not a promise about the factor scores on this tab, which come from the correlated-factors CFA of all constructs, use estimated weights, and for ordered items describe latent responses, so their reliability can be above or below H. A structural model with the latent variable (the SEM and Validity tabs) avoids forming a score at all; it does not recover H, it removes the scoring step. H and its interval are DMAR::reliability_H()'s at its defaults, from the one-factor model that matches the item types: the delta method with the full covariance matrix of the estimates (the loadings are estimated together, so they are correlated), with the MLR (sandwich) standard error for continuous items (the fit's own robust standard errors for ordered items), formed on the logit scale so the limits stay inside (0, 1). For ordered or binary items the model is the categorical one, so H refers to the latent response variables behind the items, whereas categorical omega refers to the sum of the items' category ranks: the two are then not on the same footing, and their gap is not a statement about weighting alone."
              ),
              tags$p(
                class = "text-muted mb-0",
                tags$b("Conventional reference values, not verdicts:"),
                " Nunnally and Bernstein (1994) suggested reliabilities of about .70 in the early stages of research, .80 for basic research, and .90 or more (.95 preferred) where important decisions are made about individuals. They are conventions about the use of scores, not properties of a scale and not thresholds a coefficient passes or fails; the lower limit and the width of the interval, and the use to which the scores are put, carry the evidence."
              )
            )
          ),
          uiOutput(ns("reliability_code")),
          accordion(
            open = FALSE,
            accordion_panel(
              "Data and method behind each row",
              DTOutput(ns("detail_tbl"), fill = FALSE)
            )
          )
        ),
        sieve_ordinal_omega_mi_ui(ns),
        card(
          card_header("Coefficient Omega, \u03c9: Point Estimate and Confidence Interval by Construct or Facet"),
          plotOutput(ns("omega_plot"), height = "480px")
        )
      ),

      nav_panel(
        "Plan the Next Study",
        card(
          card_header("Sample Size for a Target \u03c9 Interval Width (AIPE)"),
          sieve_more_below(
            "Enter the expected omega, the number of items and a target confidence-interval width to plan N for the next study.",
            p(
              "This planner estimates the sample size for a target width of the omega ",
              "confidence interval. Accuracy in parameter estimation (AIPE) asks how many ",
              "respondents are needed for the omega confidence interval to be no wider ",
              "than a target (Kelley & Pornprasertmanit, 2016; the program of research ",
              "is reviewed in Maxwell, Kelley, & Rausch, 2008). Supply the reliability ",
              "you expect the scale to have (the planning value; prefill it from a ",
              "construct estimated on this sample), the number of items, and the ",
              "target full width. The computation is DMAR's ss_aipe_reliability() under ",
              "a parallel model: the planning omega and k are converted to the common ",
              "inter-item correlation by the Spearman-Brown relation ",
              "(rho = omega / (k - omega (k - 1))), with standardized items. Under a ",
              "parallel model the observed and the model-implied denominators estimate ",
              "the same omega, so a planning value taken from either serves; the width ",
              "planned is that of DMAR's normal theory interval, not of the bootstrap ",
              "interval of robust omega. By ",
              "default the returned sample size makes the interval narrow in ",
              "expectation. Expected width does not control the probability that a ",
              "realized interval meets the target. Add assurance to target that ",
              "probability explicitly ",
              "(Kelley & Pornprasertmanit, 2016). Assurance is computed by Monte ",
              "Carlo, so it takes a moment and accepts an optional seed (one is drawn ",
              "and recorded when the input is empty); either way the exact call, ",
              "seed included, is written into the reproducible code log."
            ),
            label = "Planning assumptions, interval width and assurance"
          ),
          div(
            class = "d-flex align-items-end gap-3 flex-wrap mb-2",
            selectInput(ns("plan_prefill"), "Prefill from this sample",
              choices = c("Manual entry" = ""), width = "230px"
            ),
            numericInput(ns("plan_omega"), "Planning \u03c9",
              value = 0.80,
              min = 0.05, max = 0.99, step = 0.01, width = "150px"
            ),
            numericInput(ns("plan_k"), "Items on the scale",
              value = 6,
              min = 3, max = 50, step = 1, width = "150px"
            ),
            numericInput(ns("plan_width"), "Target CI full width",
              value = 0.10,
              min = 0.01, max = 0.5, step = 0.01, width = "160px"
            ),
            numericInput(ns("plan_fmi"), "Planning FMI for Omega",
              value = 0, min = 0, max = .99, step = .01, width = "180px"),
            selectInput(ns("plan_assurance"), "Assurance",
              choices = c(
                "Expected width only" = "",
                "80%" = "0.80", "85%" = "0.85",
                "90%" = "0.90", "95%" = "0.95"
              ),
              width = "170px"
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] != ''", ns("plan_assurance")),
              numericInput(ns("plan_seed"), "Seed (optional)",
                value = NULL,
                min = 1, step = 1, width = "140px"
              )
            )
          ),
          uiOutput(ns("plan_result")),
          actionButton(ns("plan_log"), "Record this plan in the reproducible code",
            class = "btn-outline-secondary btn-sm mb-2"
          ),
          helpText("Planning FMI is the fraction of information missing for omega, not the fraction of responses missing. Use a comparable prior study or a sensitivity value. Zero assumes complete information. DMAR inflates the required total N for this information loss."),
          uiOutput(ns("plan_logged")),
          sieve_congeneric_ui(ns)
        )
      ),
      nav_panel("Operational scores", sieve_operational_ui(ns)),
      nav_panel(
        "Factor scores (latent)",
        card(
          card_header("Model-based factor scores per construct"),
          selectInput(
            ns("score_method"), "Scoring method",
            choices = c(
              "Bartlett (unbiased for each person's score)" = "Bartlett",
              "Regression / EAP (shrunken, smaller error)" = "regression"
            ),
            selected = "Bartlett", width = "320px"
          ),
          uiOutput(ns("scores_note")),
          downloadButton(ns("dl_both_scores"), "Download Bartlett and Regression Scores With Intervals (.csv)",
            class = "btn-outline-primary btn-sm mb-2"),
          downloadButton(ns("dl_scores"), "Download all factor scores (.csv)",
            class = "btn-outline-primary btn-sm mb-2"
          ),
          DTOutput(ns("scores_tbl")), uiOutput(ns("scores_code")),
          actionButton(ns("score_uncertainty_go"), "Compute Score Uncertainty and Determinacy"),
          uiOutput(ns("score_uncertainty_note")), DTOutput(ns("score_uncertainty_table")),
          DTOutput(ns("score_determinacy_table")),
          downloadButton(ns("dl_score_uncertainty"), "Download score uncertainty (.csv)")
        )
      ),
      nav_panel("Scoring and Norms", value = "scoring", mod_finalize_scoring_ui("finalize"))
    )
  )
}

#' Reliability module server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive req renderUI renderPlot div HTML tags
#' @importFrom DT renderDT datatable formatRound
mod_reliab_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$panels, {
      # bslib (>= 0.8.0, in Imports) has toggle_sidebar() with `open`.
      bslib::toggle_sidebar("sidebar", open = !identical(input$panels, "scoring"), session = session)
    }, ignoreInit = TRUE)
    sieve_code_navigation(input, session)
    ordinal_mi <- sieve_ordinal_omega_mi_server(input, output, session, state)
    # Constructs that have at least three active items are estimable; those
    # with fewer are reported separately as not estimable for omega.
    # The item treatment in force: the types as the analysis reads them.
    # "study" (the default) reads the study's estimator setting on the Ingest
    # tab: the item types for "auto", every item continuous for MLR or ML,
    # every item ordered for WLSMV.
    treatment <- reactive({
      t <- input$treatment %||% "study"
      if (identical(t, "study")) {
        t <- switch(sieve_estimator_effective("study", state),
          mlr = ,
          ml_fiml = ,
          ml = "continuous",
          wlsmv = "ordered",
          "auto"
        )
      }
      if (t %in% sieve_reliability_treatments()) t else "auto"
    })
    output$treatment_src <- renderUI({
      t <- treatment()
      words <- c(
        auto = "the item types (categorical omega for a construct of ordered or binary items)",
        continuous = "every item as continuous (omega from the linear one factor model)",
        ordered = "every item as ordered (categorical omega from the polychoric model)"
      )
      p(
        class = "small text-muted mb-2",
        sprintf(
          "In force here: %s, %s.", unname(words[t]),
          if (identical(input$treatment %||% "study", "study")) "the study's setting on the Ingest tab"
          else "chosen on this tab"
        )
      )
    })
    # The estimator of the measurement model behind the factor scores,
    # matched to the item treatment in force.
    scores_estimator <- reactive({
      switch(treatment(),
        continuous = "mlr",
        ordered = "wlsmv",
        "auto"
      )
    })
    rel_binding <- sieve_bind_settings(state, "reliability_settings",
      settings = function() {
      req(input$scope, input$treatment, input$missing, input$denominator, input$ci_method)
      saved <- list(
        scope = input$scope, treatment = input$treatment, missing = input$missing,
        denominator = input$denominator, ci_method = input$ci_method,
        conf_level = input$conf_level, B = input$B, boot_seed = sieve_seed_input(input$boot_seed),
        show_alpha = input$show_alpha, small_sample_t = isTRUE(input$small_sample_t)
      )
      saved
      },
      restore = function(saved) {
        for (field in c("scope", "treatment", "denominator", "ci_method")) {
          if (!is.null(saved[[field]]) && !identical(isolate(input[[field]]), saved[[field]]))
            updateSelectInput(session, field, selected = saved[[field]])
        }
        if (!is.null(saved$missing) && !identical(isolate(input$missing), saved$missing))
          updateRadioButtons(session, "missing", selected = saved$missing)
        for (field in c("conf_level", "B", "boot_seed")) {
          if (!is.null(saved[[field]]) && !identical(isolate(input[[field]]), saved[[field]]))
            updateNumericInput(session, field, value = saved[[field]])
        }
        if (!is.null(saved$show_alpha) && !identical(isolate(input$show_alpha), saved$show_alpha))
          updateCheckboxInput(session, "show_alpha", value = saved$show_alpha)
        if (!is.null(saved$small_sample_t) && !identical(isTRUE(isolate(input$small_sample_t)), saved$small_sample_t))
          updateCheckboxInput(session, "small_sample_t", value = saved$small_sample_t)
      }, normalize = function(saved) {
        saved$small_sample_t <- isTRUE(saved$small_sample_t)
        saved
      })
    rel_restore <- rel_binding$pending
    missing_treatment <- reactive({
      requested <- input$missing %||% "study"
      if (!identical(requested, "study")) return(requested)
      resolved <- sieve_lavaan_spec(state$meta, estimator = sieve_estimator_effective("study", state))
      resolved$missing
    })
    group_noun <- reactive(switch(input$scope %||% "construct",
      facet = "facet",
      combined = "group",
      "construct"
    ))
    construct_items <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      ci <- sieve_reliability_items(state$meta, names(state$items), scope = input$scope %||% "construct")
      treat <- treatment()
      lapply(ci, function(x) {
        x$types <- sieve_reliability_treat(x$types, treat)
        x
      })
    })

    estimable <- reactive({
      ci <- construct_items()
      Filter(function(x) x$k >= 1L, ci)
    })

    too_small <- reactive({
      ci <- construct_items()
      Filter(function(x) x$k < 3L, ci)
    })

    # The interval menu holds the methods that apply to at least one
    # construct under the current choices (see sieve_reliability_menu()). A
    # selection that applies to no construct is moved to the first method
    # that does, the panel says so, and the analysis uses the method in
    # force, so an option that cannot work is never run as if it could.
    menu <- reactive({
      est <- estimable()
      req(length(est) > 0)
      classes <- vapply(est, function(x) sieve_reliability_class(x$types), character(1))
      sieve_reliability_menu(
        classes,
        missing = missing_treatment(),
        denominator = input$denominator %||% "observed",
        selected = input$ci_method %||% "recommended"
      )
    })

    # The control is rewritten only when its options change or the selection
    # has to move, not every time the user picks a method.
    sent <- new.env(parent = emptyenv())
    observe({
      m <- menu()
      if (m$changed || !identical(sent$choices, m$choices)) {
        updateSelectInput(session, "ci_method", choices = m$choices, selected = m$selected)
      }
      sent$choices <- m$choices
    })

    output$method_note <- renderUI({
      read <- sieve_attempt(menu())
      if (!read$ok) {
        return(div(
          class = "alert alert-danger small py-2",
          "The interval menu could not be resolved: ", read$error
        ))
      }
      m <- read$value
      req(m, !is.null(m$note))
      div(class = "alert alert-info small py-2", m$note)
    })

    # The choices in force, read once so every construct is resolved from
    # the same request.
    settings <- reactive({
      est <- estimable()
      req(length(est) > 0)
      conf <- sieve_finite_or(input$conf_level, 0.95)
      B <- input$B %||% 10000
      shiny::validate(
        shiny::need(
          conf > 0 && conf < 1,
          "The confidence level must lie strictly between 0 and 1."
        ),
        shiny::need(is.numeric(B) && length(B) == 1L && is.finite(B) && B >= 1 && B == floor(B) && B <= .Machine$integer.max,
          "The number of bootstrap replications must be a positive whole number.")
      )
      list(
        constructs = est, conf_level = conf, B = as.integer(B),
        missing = missing_treatment(),
        denominator = input$denominator %||% "observed",
        ci_method = menu()$selected,
        seed = sieve_seed_input(input$boot_seed),
        alpha = isTRUE(input$show_alpha), wald_df = if (isTRUE(input$small_sample_t)) "missing_information" else Inf
      )
    })

    reliability_data <- reactive({
      spec <- sieve_cfa_spec(state$meta, reserved = names(state$items), estimator = "ml_fiml")
      if (!identical(missing_treatment(), "fiml")) return(state$items)
      prepared <- sieve_auxiliary_apply(spec, state, state$items)
      if (!is.null(prepared$reason)) stop(prepared$reason, call. = FALSE)
      attr(prepared$data, "sieve_aux_warnings") <- prepared$warnings
      # Declared auxiliary variables left out because every analyzed item is
      # observed (sieve_auxiliary_apply()); the method line and the recorded
      # R say so.
      attr(prepared$data, "sieve_aux_notes") <- prepared$notes
      prepared$data
    })

    # One resolved specification per construct. `run_bootstrap = FALSE`
    # leaves a selected bootstrap pending: the point estimate is computed and
    # the resampling waits for the button.
    resolve_specs <- function(s, run_bootstrap) {
      # The declared auxiliary variables enter FIML only when an analyzed
      # item is missing, as in reliability_data() (sieve_auxiliary_apply()).
      aux <- if (identical(s$missing, "fiml")) {
        analyzed <- sieve_cfa_spec(state$meta, reserved = names(state$items), estimator = "ml_fiml")$observed
        if (sieve_auxiliary_in_force(analyzed, state$items)) {
          setdiff(sieve_roles_table(state$raw, state$items, state$roles)$variable[
            sieve_roles_table(state$raw, state$items, state$roles)$role == "auxiliary"], sieve_sampling_spec(state)$cluster)
        } else {
          character(0)
        }
      }
      lapply(s$constructs, function(x) {
        sieve_reliability_spec(
          items = x$items, types = x$types, construct = x$construct,
          missing = s$missing, ci_method = s$ci_method, denominator = s$denominator,
          conf_level = s$conf_level, B = s$B, seed = s$seed,
          wald_df = s$wald_df, run_bootstrap = run_bootstrap, group_id = x$id, scope = x$scope,
          parent_construct = x$parent_construct, facet = x$facet, cluster = sieve_bootstrap_cluster(state),
          aux = aux
        )
      })
    }

    # A completed bootstrap belongs to one request on one set of values. The
    # key binds it to both, so a bootstrap run before the data or the choices
    # changed is never shown against the new ones.
    boot_key <- function(spec, items, identity = boot_identity()) {
      absent <- setdiff(spec$aux, names(items))
      if (length(absent)) items[absent] <- state$raw[absent]
      paste(spec$key, sieve_data_key(items[, unique(c(spec$items, spec$aux)), drop = FALSE]), identity)
    }
    boot_identity <- function() sieve_hash(list(
      dataset = state$dataset_id, rows = state$case_index %||% seq_len(nrow(state$items)),
      meta = sieve_meta_key(state$meta)
    ))
    # The bootstrap request each construct would make under the choices in
    # force now: the key a completed bootstrap is looked up under. Built
    # from the current settings, not from the stored point estimates, so a
    # change of B or seed (which belong to the bootstrap, not to the
    # coefficient) reaches the lookup without recomputing anything.
    boot_requests <- function(s) {
      resolve_specs(s, run_bootstrap = TRUE)
    }

    # Everything that is immediate once asked for: the coefficient, a closed
    # form interval when one applies, H, and alpha, construct by construct.
    # Calculations are in fct_reliability.R. The computation fits two one
    # factor models per construct (omega and H) and runs on request only
    # (fct_ondemand.R): the Compute reliability button in the main panel
    # states the estimated time, the result is kept with the revision and
    # the choices it was computed under (the interval method, its level, the
    # missing data treatment and the omega variant; the bootstrap's own
    # settings and the alpha checkbox are not choices of this computation),
    # and the gate says in one line when the data or the item pool changed
    # since. Alpha is always computed (it costs nothing beside the models)
    # and the checkbox only shows or hides its column.
    rel_choices <- reactive({
      s <- settings()
      c(
        s[c(
          "conf_level",
          "missing",
          "denominator",
          "ci_method", "wald_df"
        )],
        list(
          treatment = treatment(),
          scope = input$scope %||% "construct"
        )
      )
    })
    rel_key <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      # Currency on the effective inputs: the items of the
      # constructs (or facet groups) in scope, and the facet column only
      # when facets are the groups, so a facet edit leaves a by-construct
      # table current and an edit to an item outside the scope leaves the
      # table current.
      choices <- rel_choices()
      sieve_demand_key(state, choices,
        items = as.character(unlist(lapply(estimable(), `[[`, "items"), use.names = FALSE)),
        facets = !identical(choices$scope, "construct")
      )
    })
    rel_plan <- reactive({
      est <- estimable()
      req(length(est) > 0)
      ks <- vapply(est, function(x) x$k, integer(1))
      classes <- vapply(est, function(x) sieve_reliability_class(x$types), character(1))
      estimator <- if (any(classes != "continuous")) "WLSMV" else "MLR"
      # The wait model counts two one-factor fits per group (omega and H);
      # beside robust omega a third fit gives model implied omega and its
      # interval.
      companion <- rep(identical(input$denominator %||% "observed", "observed"), length(classes))
      seconds <- sieve_demand_wait("reliability", ks, estimator) * (1 + 0.5 * mean(companion))
      standard_seconds <- sieve_at_default_rates(sieve_demand_wait("reliability", ks, estimator)) *
        (1 + 0.5 * mean(companion))
      refused <- NULL
      for (x in est) {
        guard <- sieve_wlsmv_guard(
          x$items, state$items,
          ordered = x$items[x$types %in% c("ordered", "binary")],
          estimator = "WLSMV"
        )
        if (!is.null(guard)) {
          refused <- sprintf("Construct %s: %s", x$construct, guard)
          break
        }
      }
      n_c <- length(est)
      list(
        estimator = estimator, p = ks, seconds = seconds, standard_seconds = standard_seconds,
        label = sieve_duration_label(seconds),
        progress = sprintf("Reliability: %d %s%s, %d items", n_c, group_noun(), if (n_c == 1L) "" else "s", sum(ks)),
        detail = sprintf(
          "Robust omega (observed denominator), Omega (model-implied denominator), H and alpha, %s by %s (DMAR and lavaan).",
          group_noun(),
          group_noun()
        ),
        sentence = sprintf(
          "Computing omega, H and alpha for %d %s%s (%d items, %s) is estimated to take %s",
          n_c,
          group_noun(),
          if (n_c == 1L) "" else "s",
          sum(ks),
          estimator,
          sieve_duration_label(seconds)
        ),
        refused = refused
      )
    })
    compute_reliability <- function(key, plan) {
      problem <- sieve_sampling_problem(state, capability = "cluster")
      if (!is.null(problem)) stop(problem, call. = FALSE)
      s <- shiny::isolate(settings())
      items <- shiny::isolate(reliability_data())
      specs <- resolve_specs(s, run_bootstrap = FALSE)
      ledger <- shiny::isolate(state$meta)
      results <- lapply(specs, function(sp) {
        result <- sieve_reliability_construct(items, sp, alpha = TRUE, meta = ledger)
        result$omega$warnings <- c(result$omega$warnings, attr(items, "sieve_aux_warnings"))
        # Kept beside the coefficient, not in it: a bootstrap replaces
        # result$omega (results()).
        result$aux_notes <- attr(items, "sieve_aux_notes")
        result
      })
      sieve_register_reliability(state, results, note = paste("Reliability:", group_noun(), "scope"))
      # H's interval is DMAR::reliability_H()'s, so the
      # recorded steps need no helper of SIEVER's.
      sieve_log(
        state,
        sprintf(
          paste0(
            "Reliability per construct: Robust omega (observed denominator) or Omega (model-implied ",
            "denominator), as each call names, with a %s%% confidence interval where the selected ",
            "method applies (DMAR's defaults unless another was chosen; a bootstrap interval runs on ",
            "request); Omega (model-implied denominator) with DMAR's default interval (MLR (sandwich) SE, ",
            "logit-scale limits) beside robust omega; for constructs of ordered or binary items the coefficient of the ",
            "sum of the items' category ranks (Green & Yang, 2009), its denominator from the unrestricted polychoric ",
            "correlations; H with DMAR::reliability_H()'s interval; and alpha for comparison."
          ),
          format(s$conf_level * 100)
        ),
        c(
          "library(DMAR)",
          if (length(attr(items, "sieve_aux_notes"))) paste("#", attr(items, "sieve_aux_notes")),
          unlist(lapply(results, function(r) r$omega$code), use.names = FALSE),
          unlist(lapply(results, function(r) r$model_implied$code), use.names = FALSE),
          unlist(lapply(results, function(r) r$H$code), use.names = FALSE),
          unlist(lapply(results, function(r) r$alpha$code), use.names = FALSE)
        )
      )
      code_record <- sieve_code_snapshot(state, context = c(
        sprintf("Reporting scope: %s; item treatment: %s; requested confidence level: %s.",
          key$choices$scope, key$choices$treatment, format(s$conf_level)),
        "The calls below name each construct's actual coefficient, missing-data treatment, interval method and item columns.",
        attr(items, "sieve_aux_notes"),
        vapply(results, function(r) sprintf("%s: modeled item columns %s.",
          r$spec$construct, paste(r$spec$items, collapse = ", ")), character(1))
      ))
      attr(results, "code_view") <- code_record
      results
    }
    demand <- sieve_demand_wire(
      input, session, state,
      id = "run_go", what = "Reliability", capability = "cluster",
      key = rel_key, plan = rel_plan, compute = compute_reliability, kind = "reliability",
      noun = "The reliability table", verb = "computed", action = "Compute reliability",
      # A table from before is updated by the page's one Update (the status
      # line), which the page guide names as the action.
      refit = "Update",
      stale_lead = "The data or the item pool changed since this table was computed: recompute it. "
    )
    immediate <- reactive({
      res <- demand$result()
      req(res)
      res
    })

    # Bootstrap intervals, computed only on request and kept by key.
    boot_store <- reactiveVal(list())

    # Validate the requested count before a background process is started.
    # Whole-cluster bootstraps keep SIEVER's own count policy.
    boot_requirement <- function(spec) {
      if (!is.null(spec$cluster) || identical(spec$resampling, "siever")) {
        return(list(ok = TRUE, B = spec$B, required = NA_integer_, message = NULL))
      }
      method <- switch(spec$ci_method,
        dmar_bca = "bca", bootstrap_se_logistic = "se", "percentile")
      sieve_dmar_boot_request(spec$B, method = method, conf_level = spec$conf_level)
    }

    # The pending bootstraps and an estimate of the wait: each replication
    # refits the model, so the time of one fit (the point estimate just
    # computed) times the number of fits is the wait for a construct, spread
    # over DMAR's processes in the background process, with alpha's own
    # bootstrap for comparison (one analytic estimate per replication). A
    # construct whose run was stopped part way is pending again.
    boot_plan <- reactive({
      res <- immediate()
      s <- settings()
      store <- boot_store()
      items <- state$items
      requests <- boot_requests(s)
      pending <- Filter(function(r) {
        sp <- requests[[sieve_reliability_id(r$spec)]]
        hit <- if (!is.null(sp)) store[[boot_key(sp, items)]]
        identical(r$omega$uncertainty$status, "pending") &&
          (is.null(hit) || isFALSE(hit$uncertainty$complete))
      }, res)
      methods <- vapply(pending, function(r) {
        requests[[sieve_reliability_id(r$spec)]]$ci_method %||% s$ci_method
      }, character(1))
      requirements <- lapply(pending, function(r) {
        sp <- requests[[sieve_reliability_id(r$spec)]]
        check <- boot_requirement(sp)
        if (!isTRUE(check$ok)) sprintf("%s: %s", sp$construct, check$message) else NULL
      })
      refusal <- unique(unlist(requirements, use.names = FALSE))
      refusal <- if (length(refusal)) paste(refusal, collapse = " ") else NULL
      # The processes the background run asks DMAR for; it decides there
      # whether it may fork (sieve_dmar_boot_cores()).
      ncpus <- sieve_dmar_boot_cores()
      seconds <- vapply(seq_along(pending), function(i) {
        r <- pending[[i]]
        omega <- sieve_reliability_boot_wait(r$omega, s$B, methods[[i]], alongside = length(pending))
        alpha <- r$alpha$seconds %||% NA_real_
        omega + if (is.null(r$omega$spec$cluster) && is.finite(alpha)) alpha * s$B / ncpus else 0
      }, numeric(1))
      jackknife <- vapply(seq_along(pending), function(i) {
        n <- pending[[i]]$omega$n$used
        if (identical(methods[[i]], "dmar_bca") && is.finite(n) && s$B < n) as.integer(n) else 0L
      }, integer(1))
      clustered <- any(vapply(pending, function(r) !is.null(r$omega$spec$cluster), logical(1)))
      list(
        constructs = names(pending), seconds = seconds, methods = unname(methods),
        jackknife = jackknife, clustered = clustered, ncpus = ncpus, refusal = refusal
      )
    })

    output$boot_note <- renderUI({
      read <- sieve_attempt(settings())
      if (!read$ok) {
        return(div(class = "alert alert-danger small", read$error))
      }
      s <- read$value
      req(s, s$ci_method %in% c("recommended", sieve_reliability_bootstrap_methods()))
      if (is.null(demand$result())) {
        return(helpText(
          class = "small text-muted",
          "Press Compute reliability (above the table) first; the bootstrap resamples the point estimates it computes."
        ))
      }
      helpText(class = "small text-muted", boot_wait_text(boot_plan(), s))
    })

    # The wait of the bootstrap still to run, and what the bootstrap
    # reports, stated before it starts (here and above the table).
    boot_wait_text <- function(plan, s) {
      if (!is.null(plan$refusal)) return(paste(
        plan$refusal, "Change the requested count before starting; no bootstrap has been dispatched."))
      mc <- sieve_bootstrap_mc_error(c(s$B, 200, 10000), s$conf_level)
      jack <- sum(plan$jackknife)
      wait <- if (length(plan$constructs)) {
        paste0(
          sprintf(
            paste0(
              "Each replication refits the model, so with %d replications and %d %s%s ",
              "the bootstrap should take %s on this computer (from the measured time of one ",
              "fit per %s, %s). "
            ),
            s$B, length(plan$constructs), group_noun(), if (length(plan$constructs) == 1L) "" else "s",
            sieve_duration_label(sum(plan$seconds)), group_noun(),
            if (isTRUE(plan$clustered)) {
              "shared over several R processes when the run is long"
            } else if (plan$ncpus > 1L) {
              sprintf("spread over %d R processes", plan$ncpus)
            } else {
              "in one R process"
            }
          ),
          if (jack > 0L) {
            sprintf(
              paste0(
                "That includes the jackknife behind the BCa acceleration constant: with fewer ",
                "replications than rows, BCa refits the model once per row left out, %d refits in ",
                "all here (with at least as many replications as rows the constant is read from ",
                "the replications at no extra cost). "
              ),
              jack
            )
          } else if (any(plan$methods == "dmar_bca")) {
            paste0(
              "BCa reads its acceleration constant from the replications here (there are at least ",
              "as many replications as rows), so no jackknife refits are added. "
            )
          },
          if (isTRUE(plan$clustered)) {
            paste0(
              "With the declared cluster design SIEVER resamples whole clusters with its own counted ",
              "bootstrap (DMAR's resamples independent rows). It shows progress as replications ",
              "finish and can be stopped between batches of replications (about half a second each): ",
              "completed groups keep their intervals, a group stopped part way keeps the replications ",
              "completed (labeled as stopped), and the counts of replications attempted, converged, ",
              "admissible, and used are reported with each interval. "
            )
          } else {
            paste0(
              "DMAR runs each group's bootstrap (omega, then alpha for comparison) in a background R ",
              "process, so this page stays responsive; the progress bar follows DMAR's count of ",
              "replications, and Cancel ends the bootstrap at its next replication (DMAR's progress ",
              "hook), or on several processes at the end of the block of replications in progress, ",
              "which DMAR completes together; if DMAR has not stopped five seconds after the press, ",
              "the background process is ended. Groups completed keep their intervals; the group in ",
              "progress gets none, since ",
              "DMAR forms its limits and counts from all B replications, and stays not run. DMAR counts ",
              "every replication's fate, and the counts of replications attempted, converged, ",
              "admissible, and used are reported with each interval. Its rule: a replication is used ",
              "when its refit returns a finite estimate; an improper solution (a negative error ",
              "variance) that still yields a coefficient is kept and counted, one that yields none is ",
              "set aside and counted, as are refits that fail or do not converge; and no interval is ",
              "formed when fewer than half of the B replications return a value (DMAR's half rule). "
            )
          }
        )
      } else {
        "The bootstrap for the current choices has been run. "
      }
      paste0(
        wait,
        "The approximation below is for planning and assumes every requested replication is usable. After the run, the result notes use the effective count. ",
        sprintf(
          paste0(
            "The number of replications sets how reproducible the limits are, not what they ",
            "estimate: at B = %d each percentile limit carries a Monte Carlo standard error ",
            "of about %.2f bootstrap standard errors, against %.2f at B = 200 and %.2f at ",
            "B = 10,000. BCa limits sit farther into the tails and need more replications ",
            "for the same stability. DMAR also requires a method- and confidence-level-dependent ",
            "minimum before a run can start; the count you request is kept unchanged. An empty seed is drawn for the run and recorded, so the run can be ",
            "repeated exactly."
          ),
          s$B, mc[1], mc[2], mc[3]
        )
      )
    }

    # Above the table, beside the rows it concerns: which intervals have
    # not been run, the wait, and the button (the sidebar holds the same
    # button with the number of replications and the seed).
    output$boot_main <- renderUI({
      read <- sieve_attempt(settings())
      req(read$ok)
      s <- read$value
      req(!is.null(demand$result()), identical(demand$status()$state, "current"))
      plan <- boot_plan()
      req(length(plan$constructs) > 0L, !isTRUE(boot_running()))
      if (!is.null(plan$refusal)) return(div(
        class = "alert alert-warning small py-2 mt-2", plan$refusal,
        " Change the requested count before starting; the point estimates remain available."))
      kinds <- unique(vapply(plan$methods, function(m) {
        sieve_reliability_interval("listwise", m)$label
      }, character(1)))
      div(
        class = "alert alert-info small py-2 mt-2",
        tags$p(
          class = "mb-1",
          tags$b("Interval: bootstrap not run yet "),
          sprintf(
            "for %d %s%s (%s, %d replications; the estimated wait is %s). ",
            length(plan$constructs), group_noun(), if (length(plan$constructs) == 1L) "" else "s",
            paste(kinds, collapse = " and "), s$B, sieve_duration_label(sum(plan$seconds))
          ),
          "The point estimates are shown now. The number of replications and the seed are set in the sidebar."
        ),
        actionButton(session$ns("run_boot_main"), "Run the bootstrap", class = "btn-primary btn-sm")
      )
    })

    # Two ways a bootstrap runs. DMAR's counted bootstrap (independent rows,
    # every case but a declared cluster design) runs in a background R
    # process (fct_dmar_boot.R): the page polls a small progress file it
    # writes, and Stop asks DMAR to cancel at its next check (its progress
    # hook: after the replication in progress, or after the block in
    # progress on several processes), ending the process if DMAR runs past
    # a grace period. SIEVER's whole-cluster bootstrap runs in short batches in this
    # process with the session serviced between them, so the progress bar
    # moves and the Stop button is heard.
    boot_running <- reactiveVal(FALSE)
    boot_last <- reactiveVal(NULL)
    bootstrap_request_key <- function() sieve_hash(list(
      identity = boot_identity(), data = sieve_data_key(reliability_data()),
      meta = sieve_meta_key(state$meta), settings = settings()[setdiff(names(settings()), "alpha")]
    ))
    boot_job <- new.env(parent = emptyenv()) # the run in progress (plain state)
    session$onSessionEnded(function() {
      if (!is.null(boot_job$handle)) boot_job$handle$kill()
    })

    output$stop_boot_ui <- renderUI({
      req(isTRUE(boot_running()))
      actionButton(
        session$ns("stop_boot"),
        if (identical(boot_job$mode, "siever")) "Cancel after the current batch" else "Cancel the bootstrap",
        class = "btn-outline-secondary btn-sm mb-3"
      )
    })

    # One start for the sidebar button and the button above the table.
    start_boot <- function() {
      problem <- sieve_sampling_problem(state, capability = "cluster")
      if (!is.null(problem)) {
        sieve_notify(problem, type = "error", duration = NULL)
        return()
      }
      if (isTRUE(boot_running())) {
        return()
      }
      s <- settings()
      items <- reliability_data()
      specs <- resolve_specs(s, run_bootstrap = TRUE)
      store <- boot_store()
      todo <- Filter(function(sp) {
        hit <- store[[boot_key(sp, items)]]
        identical(sp$uncertainty$status, "requested") &&
          sp$ci_method %in% sieve_reliability_bootstrap_methods() &&
          (is.null(hit) || isFALSE(hit$uncertainty$complete))
      }, specs)
      req(length(todo) > 0)
      checks <- lapply(todo, boot_requirement)
      bad <- which(!vapply(checks, function(x) isTRUE(x$ok), logical(1)))
      if (length(bad)) {
        sieve_notify(paste(vapply(bad, function(i) sprintf("%s: %s",
          todo[[i]]$construct, checks[[i]]$message), character(1)), collapse = " "),
          type = "error", duration = NULL)
        return()
      }
      if (!identical(demand$status()$state, "current")) {
        sieve_notify("Compute or update the reliability table (the button in the status line above it) before requesting bootstrap intervals.",
          type = "error", duration = NULL)
        return()
      }
      fitted <- immediate()
      wait <- vapply(names(todo), function(cn) {
        sieve_reliability_boot_wait(fitted[[cn]]$omega, s$B, todo[[cn]]$ci_method, alongside = length(todo))
      }, numeric(1))
      boot_job$todo <- todo
      boot_job$wait <- wait
      boot_job$settings <- s
      boot_job$items <- items
      boot_job$identity <- boot_identity()
      boot_job$request_key <- bootstrap_request_key()
      boot_job$base <- fitted
      boot_job$revision <- sieve_revision(state)
      boot_job$scientific_dataset_id <- shiny::isolate(state$scientific_dataset_id)
      boot_job$inputs <- sieve_replay_inputs(state)
      # A declared cluster design is resampled in whole clusters by SIEVER's
      # own counted bootstrap; every other run is DMAR's, in the background.
      boot_job$mode <- if (any(vapply(todo, function(sp) identical(sp$resampling, "siever"), logical(1)))) "siever" else "dmar"
      # One seed for the run: supplied, or drawn from the session stream and
      # recorded, so every construct's bootstrap can be replayed.
      boot_job$rng <- sieve_draw_seed(s$seed)
      boot_job$done <- list()
      # What DMAR's run fills in (dmar_prepare()); a run stopped before it
      # prepares must not read the previous run's.
      boot_job$owners <- NULL
      boot_job$points <- list()
      boot_job$alpha_plans <- list()
      boot_job$current <- 1L
      boot_job$plan <- NULL
      boot_job$draws <- list()
      boot_job$handle <- NULL
      boot_job$phase <- "prepare"
      boot_job$stopped <- list()
      boot_job$started <- proc.time()[["elapsed"]]
      boot_job$construct_started <- NA_real_
      boot_job$stop <- FALSE
      boot_job$primed <- FALSE
      # Progress counts replications (omega's, and alpha's for comparison in
      # DMAR's run); it is reset once DMAR's calls are known.
      boot_job$units <- rep(s$B, length(todo))
      boot_job$progress_max <- sum(boot_job$units)
      boot_job$progress <- shiny::Progress$new(session, min = 0, max = boot_job$progress_max)
      boot_job$progress$set(
        value = 0, message = sprintf("Bootstrap (%d replications per construct)", s$B),
        detail = sprintf(
          "%s, construct 1 of %d; %s remaining", todo[[1]]$construct,
          length(todo), sieve_duration_label(sum(wait))
        )
      )
      boot_running(TRUE)
    }
    observeEvent(input$run_boot, start_boot())
    observeEvent(input$run_boot_main, start_boot())

    observeEvent(input$stop_boot, {
      if (isTRUE(boot_running())) {
        boot_job$stop <- TRUE
        if (!is.null(boot_job$handle)) boot_job$handle$stop()
      }
    })

    # Close a run: keep the intervals of the constructs completed (with
    # SIEVER's whole-cluster bootstrap, also the replications completed of a
    # construct stopped part way, labeled as stopped) and record their
    # calls. Constructs not reached, and a construct whose DMAR bootstrap
    # was stopped, stay "not run yet".
    finish_boot <- function() {
      s <- boot_job$settings
      rng <- boot_job$rng
      if (!is.null(boot_job$plan) && length(boot_job$draws)) {
        cn <- sieve_reliability_id(boot_job$plan$spec)
        boot_job$done[[cn]] <- sieve_reliability_boot_result(
          boot_job$plan, boot_job$draws,
          seconds = proc.time()[["elapsed"]] - boot_job$construct_started
        )
      }
      boot_job$plan <- NULL
      boot_job$draws <- list()
      done <- lapply(boot_job$done, function(r) {
        if (is.null(r$alpha_result)) {
          r$alpha_result <- sieve_reliability_cluster_interval(sieve_reliability_alpha(boot_job$items, r$spec, rng = rng), r$spec)
        }
        r
      })
      if (!is.null(boot_job$progress)) {
        boot_job$progress$close()
        boot_job$progress <- NULL
      }
      if (!is.null(boot_job$handle)) {
        boot_job$handle$release()
        boot_job$handle <- NULL
      }
      boot_running(FALSE)
      stopped <- boot_job$stopped
      stopped_text <- if (length(stopped)) {
        paste(vapply(stopped, function(x) sprintf(
          "%s was canceled after %d of %d replications and has no interval (DMAR forms none from a canceled run); it stays not run.",
          x$construct, as.integer(x$done), as.integer(x$total)
        ), character(1)), collapse = " ")
      }
      if (!length(done)) {
        boot_last(list(key = boot_job$request_key, state = "failed", when = format(Sys.time(), "%H:%M"),
          sentence = paste(c("The bootstrap stopped before returning an interval.", stopped_text), collapse = " ")))
        return(invisible(NULL))
      }
      boot_last(list(key = boot_job$request_key, state = "current", when = format(Sys.time(), "%H:%M"),
        sentence = paste(c("The recorded bootstrap matches this request; inspect completion status and usable replication counts.",
          stopped_text), collapse = " ")))
      store <- boot_store()
      for (r in done) {
        store[[boot_key(r$spec, boot_job$items, boot_job$identity)]] <- r
      }
      boot_store(store)
      completed <- lapply(done, function(om) {
        base <- boot_job$base[[sieve_reliability_id(om$spec)]] %||% list()
        base$spec <- om$spec
        base$omega <- om
        base$alpha <- om$alpha_result
        base
      })
      sieve_register_reliability(state, completed,
        note = "Requested reliability bootstrap intervals",
        inputs = boot_job$inputs, revision = boot_job$revision,
        scientific_dataset_id = boot_job$scientific_dataset_id
      )
      counts <- vapply(done, function(r) {
        u <- r$uncertainty
        if (!is.null(u$counts)) {
          sprintf(
            "%s: %d attempted, %d converged, %d admissible, %d used", r$construct,
            u$counts$attempted, u$counts$converged, u$counts$admissible, u$counts$used
          )
        } else {
          sprintf(
            "%s: %d requested, %s attempted, %s used; %s", r$construct, u$B,
            format(u$B_attempted), format(u$B_used), u$reason %||% u$label
          )
        }
      }, character(1))
      engine <- if (identical(boot_job$mode, "siever")) {
        "SIEVER's whole-cluster counted bootstrap"
      } else {
        # The processes DMAR used, as its results report them.
        used <- sieve_dmar_process_text(lapply(done, function(r) {
          c(r$uncertainty$ncpus, r$alpha_result$uncertainty$ncpus)
        }))
        paste(c("DMAR's counted bootstrap", used), collapse = ", ")
      }
      sieve_log(
        state,
        sprintf(
          paste0(
            "Reliability: nonparametric bootstrap interval (%s, B = %d, seed = %d%s; %s) for ",
            "each construct's coefficient, as its recorded call names it (Robust omega, observed ",
            "denominator, unless the model-implied denominator was chosen). In the session: %s."
          ),
          paste(unique(vapply(done, function(r) r$spec$ci_method, character(1))), collapse = " and "),
          s$B, rng$seed,
          if (rng$supplied) "" else ", drawn for this run",
          engine,
          paste(counts, collapse = "; ")
        ),
        c(
          "library(DMAR)",
          unlist(lapply(done, function(r) c(r$code, r$alpha_result$code)), use.names = FALSE)
        ),
        inputs = boot_job$inputs
      )
      code_record <- sieve_code_snapshot(state, context = c(
        sprintf("Completed or stopped bootstrap request: method %s; requested B = %d; seed = %d; %s.",
          paste(unique(vapply(done, function(r) r$spec$ci_method, character(1))), collapse = " and "),
          s$B, rng$seed, engine),
        paste(counts, collapse = "; ")
      ))
      store <- boot_store()
      for (r in done) {
        r$code_view <- code_record
        store[[boot_key(r$spec, boot_job$items, boot_job$identity)]] <- r
      }
      boot_store(store)
    }

    # A run that stopped with an error: the background process is ended,
    # nothing is stored, and the reason is shown.
    fail_boot <- function(error) {
      if (!is.null(boot_job$plan)) sieve_reliability_boot_release(boot_job$plan)
      if (!is.null(boot_job$handle)) {
        boot_job$handle$kill()
        boot_job$handle <- NULL
      }
      if (!is.null(boot_job$progress)) {
        boot_job$progress$close()
        boot_job$progress <- NULL
      }
      boot_running(FALSE)
      boot_last(list(key = boot_job$request_key, state = "failed", when = format(Sys.time(), "%H:%M"),
        sentence = error))
      sieve_notify(
        paste("The bootstrap stopped with an error:", error),
        type = "error", duration = NULL
      )
    }

    # DMAR's run, first step: the point estimate of each construct (the
    # refusal of an improper or inadmissible estimate is decided here, as
    # sieve_reliability_omega() decides it), alpha's call, and the start of
    # the background process with every DMAR call of the run.
    dmar_prepare <- function() {
      todo <- boot_job$todo
      ncpus <- sieve_dmar_boot_cores()
      calls <- list()
      owners <- list()
      boot_job$points <- list()
      boot_job$alpha_plans <- list()
      for (sp in todo) {
        id <- sieve_reliability_id(sp)
        p <- sieve_reliability_point(boot_job$items, sp, rng = boot_job$rng, ci_method = "none")
        out <- p$out
        if (p$failed || !identical(out$uncertainty$status, "requested")) {
          boot_job$done[[id]] <- out
        } else if (!identical(out$status$code, "ok")) {
          out$uncertainty$status <- "unavailable"
          out$uncertainty$reason <- sieve_reliability_refusal(out$status$message)
          boot_job$done[[id]] <- out
        } else {
          boot_job$points[[id]] <- list(out = out, warnings = p$first$warnings)
          calls <- c(calls, list(sieve_reliability_boot_call(sp, boot_job$rng, p$X, ncpus = ncpus)))
          owners <- c(owners, list(list(id = id, part = "omega", construct = sp$construct, units = sp$B)))
        }
        ap <- sieve_reliability_alpha_plan(boot_job$items, sp, rng = boot_job$rng, ncpus = ncpus)
        boot_job$alpha_plans[[id]] <- ap
        if (!is.null(ap$call) && isTRUE(ap$bootstrap)) {
          calls <- c(calls, list(ap$call))
          owners <- c(owners, list(list(id = id, part = "alpha", construct = sp$construct, units = sp$B)))
        }
      }
      boot_job$owners <- owners
      boot_job$units <- vapply(owners, function(o) as.integer(o$units), integer(1))
      if (!length(calls)) {
        boot_job$phase <- "done"
        return(invisible(NULL))
      }
      boot_job$progress$set(value = 0, detail = sprintf(
        "Starting the background R process (%d DMAR call%s, %d process%s each)", length(calls),
        if (length(calls) == 1L) "" else "s", ncpus, if (ncpus == 1L) "" else "es"
      ))
      boot_job$job_started <- proc.time()[["elapsed"]]
      boot_job$handle <- sieve_dmar_boot_start(calls)
      boot_job$phase <- "running"
      invisible(NULL)
    }

    # DMAR's run, last step: each finished call read into its construct's
    # record; a construct stopped part way is kept out (it stays not run).
    dmar_collect <- function() {
      handle <- boot_job$handle
      results <- if (is.null(handle)) list() else handle$results
      owners <- boot_job$owners %||% list()
      alpha_runs <- list()
      for (k in seq_along(owners)) {
        o <- owners[[k]]
        run <- if (k <= length(results)) results[[k]]
        if (identical(o$part, "alpha")) {
          alpha_runs[[o$id]] <- run
          next
        }
        if (is.null(run)) {
          boot_job$stopped[[o$id]] <- list(construct = o$construct, done = 0L, total = o$units)
          next
        }
        if (identical(sieve_dmar_boot_outcome(run), "cancelled")) {
          boot_job$stopped[[o$id]] <- list(construct = o$construct,
            done = run$condition$done %||% NA_integer_, total = run$condition$total %||% o$units)
          next
        }
        point <- boot_job$points[[o$id]]
        boot_job$done[[o$id]] <- sieve_reliability_boot_complete(point$out, run, point_warnings = point$warnings)
      }
      for (id in names(boot_job$done)) {
        ap <- boot_job$alpha_plans[[id]]
        if (is.null(ap)) next
        alpha <- if (is.null(ap$call)) {
          ap$out
        } else if (!isTRUE(ap$bootstrap)) {
          sieve_reliability_alpha_finish(ap, sieve_dmar_boot_run(ap$call))
        } else {
          run <- alpha_runs[[id]] %||% list(
            ok = FALSE, error = "The bootstrap was stopped before alpha's replications began.",
            error_class = c("dmar_bootstrap_cancelled", "error", "condition"),
            condition = structure(list(message = "stopped", done = 0L, total = ap$spec$B),
              class = c("dmar_bootstrap_cancelled", "error", "condition"))
          )
          sieve_reliability_alpha_finish(ap, run)
        }
        boot_job$done[[id]]$alpha_result <- sieve_reliability_cluster_interval(alpha, boot_job$done[[id]]$spec)
      }
      invisible(NULL)
    }

    # One poll of DMAR's background run: progress, Stop, and the end.
    dmar_step <- function() {
      outcome <- sieve_capture({
        if (identical(boot_job$phase, "prepare")) {
          if (isTRUE(boot_job$stop)) {
            # Stopped before anything started: nothing to run or record.
            boot_job$phase <- "done"
          } else {
            dmar_prepare()
          }
        }
        handle <- boot_job$handle
        alive <- if (is.null(handle)) FALSE else handle$poll()
        if (!is.null(handle)) {
          pr <- handle$progress
          i <- min(max(1L, as.integer(pr$call)), length(boot_job$units))
          value <- sum(boot_job$units[seq_len(i - 1L)]) + min(as.integer(pr$done), boot_job$units[i])
          elapsed <- proc.time()[["elapsed"]] - boot_job$started
          total <- sum(boot_job$units)
          owner <- boot_job$owners[[i]]
          boot_job$progress$set(
            # DMAR's calls (omega and alpha) are known once the run starts;
            # the bar's scale was set from omega's replications.
            value = value / max(total, 1) * boot_job$progress_max,
            detail = sprintf(
              "%s, %s: %d of %d replications (DMAR call %d of %d)%s",
              owner$construct, if (identical(owner$part, "alpha")) "alpha for comparison" else "omega",
              as.integer(pr$done), as.integer(owner$units), i, length(boot_job$units),
              if (isTRUE(boot_job$stop)) {
                "; stopping"
              } else if (value > 0) {
                sprintf("; %s remaining", sieve_duration_label(elapsed / value * (total - value)))
              } else {
                ""
              }
            )
          )
          if (alive && !is.na(handle$stop_requested) &&
            proc.time()[["elapsed"]] - handle$stop_requested > sieve_dmar_boot_grace_seconds()) {
            # A replication ran past the grace period: end the process. The
            # calls finished before it are on disk and are kept.
            handle$poll()
            results <- handle$results
            handle$process$kill()
            handle$results <- results
            alive <- FALSE
          }
          if (!alive) {
            handle$poll()
            failure <- handle$failure()
            if (!is.null(failure)) stop(failure, call. = FALSE)
          }
        }
        if (!alive) {
          dmar_collect()
          finish_boot()
        }
      })
      if (!outcome$ok) fail_boot(outcome$error)
    }

    sieve_chunked_wire(function() isTRUE(boot_running()) && identical(boot_job$mode, "dmar"), session, function() {
      # The first pass starts nothing, so the page shows the run (the
      # progress bar, the Stop button) before any fit.
      if (!isTRUE(boot_job$primed)) {
        boot_job$primed <- TRUE
        return(invisible(NULL))
      }
      dmar_step()
    }, interval = 200L)

    sieve_chunked_wire(function() isTRUE(boot_running()) && identical(boot_job$mode, "siever"), session, function() {
      # The first pass fits nothing, so the page shows the run (the
      # progress bar, the Stop button) before the first fit starts.
      if (!isTRUE(boot_job$primed)) {
        boot_job$primed <- TRUE
        return(invisible(NULL))
      }
      outcome <- sieve_capture({
        todo <- boot_job$todo
        began <- proc.time()[["elapsed"]]
        repeat {
          i <- boot_job$current
          if (isTRUE(boot_job$stop) || i > length(todo)) {
            break
          }
          sp <- todo[[i]]
          if (is.null(boot_job$plan)) {
            boot_job$construct_started <- proc.time()[["elapsed"]]
            boot_job$plan <- sieve_reliability_boot_plan(boot_job$items, sp, rng = boot_job$rng)
            boot_job$draws <- list()
            if (!identical(boot_job$plan$status$code, "ready")) {
              # Refused with the reason (an improper estimate, for example).
              boot_job$done[[sieve_reliability_id(sp)]] <- sieve_reliability_boot_result(boot_job$plan)
              boot_job$plan <- NULL
              boot_job$current <- i + 1L
              next
            }
          }
          b <- sieve_reliability_boot_next(boot_job$plan, length(boot_job$draws))
          boot_job$draws[b] <- sieve_reliability_boot_draws(boot_job$plan, b)
          if (max(b) >= sp$B) {
            boot_job$done[[sieve_reliability_id(sp)]] <- sieve_reliability_boot_result(
              boot_job$plan, boot_job$draws,
              seconds = proc.time()[["elapsed"]] - boot_job$construct_started
            )
            boot_job$plan <- NULL
            boot_job$draws <- list()
            boot_job$current <- i + 1L
          }
          if (proc.time()[["elapsed"]] - began > 0.5) {
            break
          }
        }
        i <- boot_job$current
        if (i <= length(todo)) {
          finished_units <- sum(boot_job$units[seq_len(i - 1L)])
          partial <- length(boot_job$draws)
          value <- finished_units + partial
          elapsed <- proc.time()[["elapsed"]] - boot_job$started
          remaining <- if (value > 0) {
            elapsed / value * (sum(boot_job$units) - value)
          } else {
            sum(boot_job$wait[i:length(todo)])
          }
          not_used <- if (partial) {
            sum(!vapply(boot_job$draws, function(d) isTRUE(d$used), logical(1)))
          } else {
            0L
          }
          boot_job$progress$set(
            value = value,
            detail = sprintf(
              "%s, construct %d of %d: %d of %d replications; %s remaining%s", todo[[i]]$construct, i,
              length(todo), partial, todo[[i]]$B,
              sieve_duration_label(remaining),
              if (not_used > 0L) sprintf("; %d not usable so far", not_used) else ""
            )
          )
        }
        if (isTRUE(boot_job$stop) || boot_job$current > length(todo)) {
          finish_boot()
        }
      })
      if (!outcome$ok) fail_boot(outcome$error)
    })

    observe({
      held <- if (isTRUE(boot_running())) list(
        key = boot_job$request_key, state = "running", when = NULL,
        sentence = "Bootstrap intervals are running on their saved input values."
      ) else boot_last()
      if (is.null(held)) return()
      request <- sieve_outcome(bootstrap_request_key())
      if (!request$ready || !request$ok || !identical(held$key, request$value)) {
        held$state <- "stale"
        held$sentence <- "The data, respondents or bootstrap settings changed; these intervals belong to the earlier request."
      }
      sieve_page_publish(state, sub("-.*$", "", session$ns("")), session$ns("run_boot"),
        "Reliability bootstrap intervals", held, action = "Run bootstrap intervals")
    })

    # The results shown: the immediate results, with a completed bootstrap in
    # place of the pending interval when one exists for this exact request
    # (looked up under the request the current choices make).
    results <- reactive({
      res <- immediate()
      s <- settings()
      store <- boot_store()
      items <- state$items
      requests <- boot_requests(s)
      lapply(res, function(r) {
        sp <- requests[[sieve_reliability_id(r$spec)]]
        hit <- if (!is.null(sp)) store[[boot_key(sp, items)]]
        if (!is.null(hit)) {
          r$omega <- hit
          if (!is.null(hit$alpha_result)) r$alpha <- hit$alpha_result
        }
        r
      })
    })

    output$reliability_code <- renderUI({
      st <- demand$status()
      record <- attr(st$value, "code_view", exact = TRUE)
      # Reading existing bootstrap records costs no model fit. Resolve the
      # same saved request as the table so a stopped or changed request is
      # never described by a newly generated call.
      if (!is.null(st$value)) {
        shown <- results()
        bootstrap_records <- lapply(shown, function(r) r$omega$code_view)
        bootstrap_records <- Filter(Negate(is.null), bootstrap_records)
        if (length(bootstrap_records) && !is.null(record)) {
          distinct <- !duplicated(vapply(bootstrap_records, function(x) paste(x$code, collapse = "\n"), character(1)))
          for (saved in bootstrap_records[distinct]) {
            record$code <- c(record$code, "", saved$code)
            record$context <- c(record$context, saved$context)
          }
        }
      }
      sieve_code_view(record, st, ns = session$ns)
    })

    # The table behind every display, at full precision.
    reliab <- reactive({
      sieve_reliability_table(results())
    })

    output$gate <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(sieve_gate(state))
      }

      if (identical(input$panels, "scoring")) return(NULL)

      if (length(estimable()) == 0) {
        return(div(
          class = "alert alert-info",
          "No assigned group has an included item in this reporting scope. Assign constructs and facets on the Items tab."
        ))
      }
      # The one line that says whether the table is not computed yet,
      # current, from before a change (and what changed), or stopped with
      # an error, with the page's one action: Compute reliability for a
      # first table, Update for every result an edit left from before.
      page_line()
    })

    # The table the tab last displayed. The planning selector below is
    # prefilled from it by an observer, and an observer is not suspended
    # while the tab is hidden: reading reliab() there would make the tab fit
    # every construct's models at Load and after every ledger edit, with the
    # tab never shown (about three minutes on 68 ordered items, before the
    # Ingest chip could update). The observer reads what
    # this output computed, and Shiny suspends the output while the tab is
    # hidden.
    table_shown <- reactiveVal(NULL)
    # The table as shown (the compact table): the columns are selected and
    # formatted by sieve_reliability_display(), display only; reliab() keeps
    # every column at full precision for the stored results, the project and
    # the report.
    table_display <- reactive({
      df <- reliab()
      req(df)
      sieve_reliability_display(df, results(),
        show_all = isTRUE(input$show_all),
        show_alpha = isTRUE(input$show_alpha) && "alpha" %in% names(df),
        show_difference = isTRUE(input$show_difference)
      )
    })
    output$omega_tbl <- renderDT({
      df <- reliab()
      table_shown(df)
      req(df)
      shown <- table_display()
      # The columns that join two values (an interval, a residual and its
      # pair) are text: they are not offered for sorting.
      text_at <- which(names(shown$data) %in% shown$text) - 1L
      sieve_dt(shown$data,
        rownames = FALSE, selection = "none",
        colnames = shown$colnames,
        options = list(
          dom = "tp", pageLength = 25, scrollX = TRUE,
          columnDefs = list(list(orderable = FALSE, targets = text_at))
        )
      ) |>
        formatRound(shown$round, 3)
    })

    # The one-line method statement above the table: the coefficient, the
    # engine and the model behind it, the cases used and N, and the
    # interval, for the rows shown (each row's method is stated below the
    # table).
    output$method_line <- renderUI({
      read <- sieve_outcome(results())
      req(read$ready, read$ok, length(read$value) > 0L)
      # One line; the missing-data and sampling statements behind it are in a
      # labeled disclosure under it.
      missing_words <- sieve_reliability_missing_line(read$value)
      sampling_words <- sieve_reliability_sampling_line(read$value)
      behind <- Filter(function(x) !is.null(x) && length(x) && !identical(x, ""), list(missing_words, sampling_words))
      div(
        class = "small mb-2 sieve-method-line",
        tags$b("Method: "),
        sieve_reliability_method_line(read$value, group_noun()),
        sieve_holdout_n_tag(state),
        if (length(behind)) {
          tags$details(
            class = "sieve-provenance text-muted", `data-sieve-remember` = "reliab-method",
            tags$summary("Method and data behind this result"),
            lapply(behind, div)
          )
        }
      )
    })

    # The method note: one paragraph per construct (the coefficient, its
    # model, the missing data treatment and the rows used, the interval
    # method and level, the status of each fit, the counts of a bootstrap,
    # and anything the engines warned about). Nothing here is rounded in
    # storage; the paragraph formats for reading.
    output$status_notes <- renderUI({
      read <- sieve_attempt(results())
      if (!read$ok) {
        return(div(
          class = "alert alert-danger small",
          "The method notes could not be assembled: ", read$error
        ))
      }
      res <- read$value
      req(res, length(res) > 0)
      # A bootstrap not run yet is the normal state (intervals run on
      # request), so its note alone does not call for the warning style.
      attention <- vapply(res, function(r) {
        length(sieve_reliability_notes(r)) > as.integer(identical(r$omega$uncertainty$status, "pending"))
      }, logical(1))
      div(
        class = "small mt-2",
        tags$b(sprintf("Method, %s by %s:", group_noun(), group_noun())),
        lapply(names(res), function(cn) {
          tags$p(
            class = if (attention[[cn]]) "mb-1 alert alert-warning py-1 px-2" else "mb-1",
            tags$b(paste0(res[[cn]]$spec$construct, ": ")),
            sieve_reliability_paragraph(res[[cn]])
          )
        })
      )
    })

    output$detail_tbl <- renderDT({
      res <- results()
      req(res)
      rows <- lapply(res, function(r) {
        om <- r$omega
        data.frame(
          construct = om$construct,
          rows = om$n$rows,
          observed = om$n$observed,
          complete = om$n$complete,
          patterns = om$n$patterns,
          used = om$n$used,
          requested = if (identical(om$missing$requested, "fiml")) "FIML" else "complete cases",
          treatment = if (identical(om$missing$used, "fiml")) "FIML" else "complete cases",
          interval = sieve_reliability_interval_label(om),
          se_type = om$uncertainty$se_type %||% NA_character_,
          information = om$uncertainty$information %||% NA_character_,
          replications = om$uncertainty$B,
          attempted = om$uncertainty$B_attempted %||% NA_integer_,
          used = om$uncertainty$B_used %||% NA_integer_,
          seed = om$uncertainty$rng$seed %||% NA_integer_,
          h_model = r$H$model$model,
          h_status = r$H$status$code,
          h_refers_to = r$H$target,
          h_se = sprintf(
            "%s (%s information)", r$H$uncertainty$se_type,
            r$H$uncertainty$information
          ),
          resid_source = r$residuals$source %||% NA_character_,
          fit_family = r$H$fit_summary$family %||% NA_character_,
          cfi = r$H$fit_summary$cfi %||% NA_real_,
          srmr = r$H$fit_summary$srmr %||% NA_real_,
          stringsAsFactors = FALSE
        )
      })
      df <- do.call(rbind, rows)
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        colnames = c(
          "Construct", "Rows", "Rows with any item observed",
          "Complete rows", "Missing data patterns", "Rows used",
          "Missing data requested", "Missing data used",
          "Interval method of the estimate", "Standard error type",
          "Information matrix", "Bootstrap replications requested",
          "Bootstrap replications attempted",
          "Bootstrap replications used", "Bootstrap seed",
          "Model behind H", "Status of the model behind H",
          "H refers to", "H standard error", "Source of the residual correlations",
          "Family of the one-factor fit indices", "One-factor CFI", "One-factor SRMR"
        ),
        options = list(dom = "t", pageLength = 25, scrollX = TRUE)
      ) |>
        formatRound(c("cfi", "srmr"), 3)
    })

    output$footnote <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), length(estimable()) > 0)
      conf <- sieve_finite_or(input$conf_level, 0.95)
      read <- sieve_outcome(results())
      res <- if (read$ready && read$ok) read$value else list()
      ran <- Filter(function(r) {
        identical(r$omega$uncertainty$status, "completed") &&
          r$omega$spec$ci_method %in% sieve_reliability_bootstrap_methods()
      }, res)
      boot_txt <- if (length(ran)) {
        kinds <- vapply(ran, function(r) r$omega$spec$resampling, character(1))
        u <- ran[[1]]$omega$uncertainty
        seed_txt <- sprintf(
          "seed %d%s", u$rng$seed,
          if (isTRUE(u$rng$supplied)) "" else " (drawn for this run and recorded)"
        )
        paste0(
          if (any(kinds == "siever")) {
            sprintf(
              paste0(
                " Whole-cluster percentile bootstrap intervals with counted replications: %d replications ",
                "requested per group, %s; SIEVER resampled the declared clusters (DMAR's bootstrap resamples ",
                "independent rows) and refitted each replication with DMAR's point estimator, and the ",
                "replications attempted, converged, admissible, and used are stated for each group in the ",
                "method note."
              ),
              u$B, seed_txt
            )
          },
          if (any(kinds == "dmar")) {
            labels <- unique(vapply(ran[kinds == "dmar"], function(r) r$omega$uncertainty$label, character(1)))
            ncpus <- u$ncpus %||% 1L
            sprintf(
              paste0(
                " %s intervals: DMAR's counted bootstrap (DMAR %s), %d replications requested per group, %s, ",
                "%d process%s. DMAR resampled the rows each estimate used, refitted each replication, and ",
                "counted every replication's fate; the replications attempted, converged, admissible, and ",
                "used are stated for each group in the method note. Its rule: an improper solution that ",
                "still yields a coefficient is kept and counted (improper = \"keep\"), one that yields none ",
                "is set aside and counted, and no interval is formed when fewer than half of the B ",
                "replications return a value (the half rule)."
              ),
              paste(labels, collapse = " and "), sieve_reliability_versions()$DMAR, u$B, seed_txt,
              as.integer(ncpus), if (identical(as.integer(ncpus), 1L)) "" else "es"
            )
          }
        )
      }
      # Results first: the first paragraph, visible below the
      # table, reads the columns shown by default and states what the
      # bootstrap intervals shown are; the rest of the explanation is in
      # "More on how to read this table" (footnote_more), closed by default.
      tags$p(
        class = "text-muted small mt-2",
        sprintf(
          paste0(
            "Coefficient Omega, \u03c9, with a %s%% confidence interval (Kelley & Pornprasertmanit, 2016). ",
            "The Coefficient column names each estimate and its denominator: Robust omega (observed ",
            "denominator), the default, or Omega (model-implied denominator). The %s%% CI column gives the ",
            "limits, says the bootstrap has not been run yet (Run the bootstrap, below the table), or says ",
            "that none is shown; the Interval method column names the interval method used for that construct. ",
            "Beside each coefficient stand the largest residual correlation of the one-factor model and its ",
            "item pair (observed minus model-implied correlations; for ordered items, unrestricted ",
            "polychoric minus model-implied; from DMAR when it attaches them to its result, otherwise ",
            "computed by SIEVER from the same one-factor lavaan fit). Beside robust omega stands Omega ",
            "(model-implied denominator) with its interval, which for continuous items is immediate, so every ",
            "construct of continuous items has an interval in view before any bootstrap is run; it assumes the ",
            "one-factor model, whose RMSEA is under Show all columns, and robust omega's own interval is the ",
            "percentile bootstrap, run on request. For ordered items every interval DMAR gives is a bootstrap ",
            "that refits the model on each replication, so their model-implied omega is a point estimate and ",
            "their interval is robust omega's bootstrap. Show all ",
            "columns adds the model-implied interval method, the one-factor model's RMSEA, the missing data ",
            "treatment and H. ",
            "Coefficient Alpha, \u03b1, is shown only for comparison with older reports; it equals reliability only for ",
            "essentially tau-equivalent items with uncorrelated errors and is not a general lower bound."
          ),
          format(conf * 100), format(conf * 100)
        ),
        boot_txt
      )
    })

    # The rest of the explanation of the table, in "More on how to read
    # this table" below it (closed by default).
    output$footnote_more <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), length(estimable()) > 0)
      conf <- sieve_finite_or(input$conf_level, 0.95)
      tags$p(
        class = "text-muted small",
        sprintf(
          paste0(
            "For a construct whose items ",
            "are all ordered or binary the coefficient describes the sum of the items' category ranks (Green & Yang, ",
            "2009), and the label names the matrix that built its denominator: the unrestricted polychoric ",
            "correlations for the observed denominator, the polychoric correlations the one-factor model ",
            "implies for the model-implied one. With Show all columns the Missing data column names the ",
            "treatment actually used. Beside robust ",
            "omega, Omega (model-implied denominator) is shown with DMAR's default interval, the MLR (sandwich) ",
            "standard error with the limits formed on the logit scale: it is the reliability of the composite only if the one-factor model ",
            "holds, so that model's RMSEA, with its %s%% interval and the family of the index named, is ",
            "shown with Show all columns. ",
            "The numerator of omega is model based ",
            "under both denominators, so a doublet or a method factor among a few same-signed items inflates ",
            "it either way: a rising omega while a large residual pair is retained signals redundancy, not ",
            "better measurement. A small largest residual does not certify unidimensionality: when shared ",
            "variance sits on the dominant items, the single factor absorbs it and the misfit appears among ",
            "the other items. ",
            "H is the construct replicability (maximal reliability) of the one-factor model that matches ",
            "the item types, with DMAR::reliability_H()'s %s%% interval, the delta method with the full ",
            "covariance matrix of the estimates, formed on the logit scale; for ordered or binary items it ",
            "refers to the latent response variables. ",
            "Conventional reference values (Nunnally & Bernstein, 1994): about .70 early in research, .80 ",
            "for basic research, .90 or more for decisions about individuals; they describe uses of ",
            "scores, not verdicts, and the lower limit and the width of the interval carry the evidence."
          ),
          format(100 * sieve_default_rmsea_conf_level()), format(conf * 100)
        )
      )
    })

    output$not_estimable <- renderUI({
      ts <- too_small()
      req(length(ts) > 0)
      labs <- vapply(
        ts, function(x) sprintf(
          "%s (%d item%s)",
          x$construct, x$k,
          if (x$k == 1L) "" else "s"
        ),
        character(1)
      )
      div(
        class = "alert alert-secondary mt-2",
        tags$b("Omega and H need at least three included items: "),
        paste(labs, collapse = ", "),
        ". Alpha for a two-item group remains available in the table; no equality constraint is imposed for omega."
      )
    })

    output$omega_plot <- renderPlot(
      alt = "Each construct's reliability estimate with its confidence interval. The values are in the reliability table on this page.",
      {
        df <- reliab()
        req(df, nrow(df) > 0)
        df <- df[!is.na(df$omega), , drop = FALSE]
        req(nrow(df) > 0)
        nd <- sieve_palette_colors(state$palette %||% "nd", 2L)
        navy <- nd[1]
        gold <- nd[2]
        ord <- order(df$omega)
        df <- df[ord, , drop = FALSE]
        df$construct <- factor(df$construct, levels = unique(df$construct))

        ggplot2::ggplot(df, ggplot2::aes(x = .data$omega, y = .data$construct)) +
          ggplot2::geom_errorbar(
            data = df[is.finite(df$ci_lower) & is.finite(df$ci_upper), , drop = FALSE],
            ggplot2::aes(xmin = .data$ci_lower, xmax = .data$ci_upper),
            orientation = "y", width = 0.25, color = gold, linewidth = 1
          ) +
          ggplot2::geom_point(color = navy, size = 3.5) +
          ggplot2::scale_x_continuous(limits = c(0, 1)) +
          ggplot2::labs(
            x = expression(paste("Coefficient Omega, ", omega)),
            y = NULL,
            title = "Reliability by construct or facet with confidence interval",
            subtitle = paste(unique(df$coefficient), collapse = "; ")
          ) +
          ggplot2::theme_minimal(base_size = 14)
      },
      res = 96
    )

    # ---- AIPE planning for the next study ---------------------------------
    # The closed-form (no-assurance) planner is instant, so the result updates
    # as the inputs change; the assurance path is Monte Carlo (a couple of
    # seconds) and only runs when the user selects an assurance level, which
    # is an explicit choice rather than a keystroke. The planning omega and
    # item count are converted to the parallel-model inter-item correlation by
    # Spearman-Brown, with standardized items (psi_square = 1 - rho).

    # Offer this sample's per-construct omegas as planning values. The
    # choices update with the fitted table as the tab displays it (see
    # table_shown above); the selection itself is kept.
    observe({
      df <- table_shown()
      if (is.null(df) || !nrow(df)) return()
      chc <- c(
        "Manual entry" = "",
        stats::setNames(
          df$construct,
          sprintf(
            "%s (%s = %.2f, k = %d)",
            df$construct, df$coefficient, df$omega, df$k
          )
        )
      )
      sel <- shiny::isolate(input$plan_prefill %||% "")
      if (!sel %in% chc) sel <- ""
      updateSelectInput(session, "plan_prefill", choices = chc, selected = sel)
      # A planning value prefilled from a construct follows that
      # construct's estimate: after Update recomputes the table, the planner
      # reads the new estimate (one Update per page).
      if (nzchar(sel)) prefill_apply(sel, df)
    })

    observeEvent(input$plan_prefill, {
      cn <- input$plan_prefill
      req(nzchar(cn))
      df <- table_shown()
      req(df)
      prefill_apply(cn, df, force = TRUE)
    })
    # The construct, estimate and k the planner was last prefilled with; a
    # redraw of the table sends a value to the inputs only when one of them
    # changed, so a redraw that changes no estimate leaves an edited
    # planning value alone (choosing the construct again always prefills).
    prefill_last <- reactiveVal(NULL)
    prefill_apply <- function(cn, df, force = FALSE) {
      row <- df[df$construct == cn, , drop = FALSE]
      if (nrow(row) != 1L || !is.finite(row$omega)) {
        return(invisible(NULL))
      }
      now <- list(construct = cn, omega = row$omega, k = as.integer(row$k))
      if (!force && identical(shiny::isolate(prefill_last()), now)) {
        return(invisible(NULL))
      }
      prefill_last(now)
      # Input conveniences, not stored results: the planning value is the
      # user's to edit, so two decimals is the right granularity here.
      updateNumericInput(session, "plan_omega", value = round(row$omega, 2))
      updateNumericInput(session, "plan_k", value = as.integer(row$k))
      invisible(now)
    }

    # The plan for the inputs in force, computed once per change of an input
    # and kept: the result shown and the call recorded on request are this one
    # value, so the Monte Carlo seed of an assurance plan and every input are
    # the ones the displayed N came from. The assurance answer is Monte Carlo
    # and always waits for Plan with assurance, whose label states the
    # estimate (sieve_planner_wait()): a planner reads no data, so "first for
    # the dataset" has no referent, and one click with the estimate is clearer
    # than a rule nobody can predict. The closed form (expected width) is
    # immediate and stays live.
    plan_kept <- new.env(parent = emptyenv())
    plan_kept$ran <- FALSE
    plan_request <- reactiveVal(NULL)
    observeEvent(input$plan_go, plan_request(plan_inputs_key()))
    plan_calc <- reactive({
      invisible(NULL)
      om <- sieve_finite_or(input$plan_omega, 0.80)
      k <- max(3L, as.integer(sieve_finite_or(input$plan_k, 6L)))
      w <- sieve_finite_or(input$plan_width, 0.10)
      fmi <- sieve_finite_or(input$plan_fmi, 0)
      conf <- input$conf_level %||% 0.95
      assur <- suppressWarnings(as.numeric(input$plan_assurance %||% ""))
      assur <- if (length(assur) == 1L && is.finite(assur)) assur else NULL
      seed <- if (!is.null(assur)) sieve_seed_input(input$plan_seed) else NULL
      out <- list(om = om, k = k, w = w, fmi = fmi, conf = conf, assur = assur, rng = NULL, problem = NULL)
      if (fmi < 0 || fmi >= 1) {
        out$problem <- "Planning FMI must be at least zero and below one."
        return(out)
      }
      if (!(om > 0 && om < 1) || !(w > 0 && w < 1)) {
        out$problem <- "The planning omega and the target width must both lie in (0, 1)."
        return(out)
      }
      rho <- om / (k - om * (k - 1))
      out$rho <- rho
      if (!is.finite(rho) || rho <= 0 || rho >= 1) {
        out$problem <- "This omega is not attainable with a parallel model at this k; raise k or lower the planning omega."
        return(out)
      }
      plan_call <- function() {
        args <- list(
          model = "Parallel", type = "Normal Theory", width = w,
          n_items = k, true_item_covariance = rho, true_residual_var = 1 - rho,
          conf_level = conf, assurance = assur, fmi = fmi
        )
        do.call(DMAR::ss_aipe_reliability, args)
      }
      # The expected width answer is deterministic. The assurance answer is
      # Monte Carlo, so it runs on a local random number stream: the session's
      # stream is put back afterwards and the seed actually used is recorded
      # (drawn from the session stream when none is supplied).
      out$planned <- if (is.null(assur)) {
        sieve_capture(plan_call())
      } else {
        key <- plan_inputs_key()
        seconds <- sieve_planner_wait()
        out$seconds <- seconds
        out$label <- sieve_duration_label(seconds)
        if (!identical(plan_request(), key)) {
          out$waiting <- TRUE
          return(out)
        }
        started <- proc.time()[["elapsed"]]
        drawn <- sieve_with_seed(seed, withProgress(
          message = sprintf(
            "Planning with %d%% assurance (Monte Carlo, %s)",
            round(assur * 100), out$label
          ),
          value = 1,
          sieve_capture(plan_call())
        ))
        sieve_rate_note("planner", proc.time()[["elapsed"]] - started, 1)
        plan_kept$ran <- TRUE
        out$rng <- drawn$rng
        drawn$value
      }
      if (out$planned$ok) {
        res <- out$planned$value
        out$n_req <- res$value[res$term == "necessary_N"]
        out$assurance_summary <- sieve_reliability_assurance_summary(res)
      }
      out
    })

    # The planner's call as R, with every value at full precision.
    plan_code <- function(p) {
      c(
        if (!is.null(p$rng)) sieve_seed_code(p$rng, "the assurance planner is Monte Carlo") else NULL,
        sprintf("planning_omega <- %s", sieve_num_lit(p$om)),
        sprintf("planning_k <- %dL", as.integer(p$k)),
        "# Spearman-Brown: the common inter-item correlation of a parallel model with this omega and k.",
        "planning_rho <- planning_omega / (planning_k - planning_omega * (planning_k - 1))",
        sprintf(
          "aipe_plan <- DMAR::ss_aipe_reliability(model = \"Parallel\", type = \"Normal Theory\", width = %s,",
          sieve_num_lit(p$w)
        ),
        sprintf(
          "  n_items = planning_k, true_item_covariance = planning_rho, true_residual_var = 1 - planning_rho, conf_level = %s, assurance = %s%s)",
          sieve_num_lit(p$conf), if (is.null(p$assur)) "NULL" else sieve_num_lit(p$assur),
          paste0(", fmi = ", sieve_num_lit(p$fmi))
        ),
        "aipe_plan"
      )
    }

    output$plan_result <- renderUI({
      p <- plan_calc()
      if (!is.null(p$problem)) {
        return(div(class = "alert alert-info", p$problem))
      }
      if (isTRUE(p$waiting)) {
        return(div(
          class = "alert alert-info",
          strong("The assurance sample size is Monte Carlo based and waits for its button. "),
          sprintf(
            paste0(
              "For a %d%% assurance target, the required N is found by simulating the width of the %s%% ",
              "interval at candidate sample sizes (Kelley & Pornprasertmanit, 2016), estimated at %s; ",
              "the expected-width answer above it is immediate."
            ),
            round(p$assur * 100), format(p$conf * 100), p$label
          ),
          div(class = "mt-2", actionButton(session$ns("plan_go"),
            sprintf("Plan with assurance (%s)", p$label), class = "btn-primary btn-sm"
          ))
        ))
      }
      if (!p$planned$ok) {
        return(div(
          class = "alert alert-danger",
          "The planner could not run for these inputs: ", p$planned$error
        ))
      }
      n_req <- p$n_req
      req(length(n_req) == 1L, is.finite(n_req))
      om <- p$om
      k <- p$k
      w <- p$w
      conf <- p$conf
      assur <- p$assur
      rng <- p$rng
      rho <- p$rho
      msg <- if (is.null(assur)) {
        sprintf(
          paste0(
            "<b>Required N: %d.</b> With %d parallel items at a planning omega of %s ",
            "under the parallel model, where the observed and model-implied denominators ",
            "coincide (inter-item r = %.3f), a sample of %d gives an expected %s%% normal ",
            "theory confidence interval for omega no wider than %s (Kelley & Pornprasertmanit, 2016). ",
            "Expected width does not control the probability that a realized interval ",
            "meets the target; add assurance to target that probability."
          ),
          as.integer(n_req), k, format(om), rho, as.integer(n_req), format(conf * 100), format(w)
        )
      } else {
        sprintf(
          paste0(
            "<b>Required N: %d.</b> With %d parallel items at a planning omega of %s ",
            "under the parallel model, where the observed and model-implied denominators ",
            "coincide (inter-item r = %.3f), DMAR selected a sample of %d to target %s%% assurance ",
            "that the realized %s%% normal theory confidence interval for omega is no wider than %s ",
            "(Kelley & Pornprasertmanit, 2016). %sThe assurance sample size is Monte ",
            "Carlo based%s."
          ),
          as.integer(n_req), k, format(om), rho, as.integer(n_req), format(assur * 100),
          format(conf * 100), format(w), sieve_reliability_assurance_text(p$assurance_summary),
          sprintf(
            " (seed = %d%s)", rng$seed,
            if (rng$supplied) "" else ", drawn for this run and recorded"
          )
        )
      }
      div(class = "alert alert-success", HTML(msg))
    })

    # The plan enters the reproducible code only when asked for, once per
    # press, with the values the shown N came from.
    plan_logged <- reactiveVal(NULL)
    plan_inputs_key <- function() sieve_hash(list(
      input$plan_omega, input$plan_k, input$plan_width, input$conf_level,
      input$plan_assurance, input$plan_seed, input$plan_fmi
    ))
    observeEvent(input$plan_log, {
      p <- plan_calc()
      key <- plan_inputs_key()
      if (!is.null(p$problem) || !isTRUE(p$planned$ok) || !length(p$n_req)) {
        plan_logged(list(key = key, msg = "There is no plan to record for these inputs."))
        return()
      }
      sieve_log(
        state,
        sprintf(
          "Plan the next study: N = %d for a %s%% normal theory confidence interval of omega of full width %s (planning omega = %s under a parallel model, where the observed and model-implied denominators coincide; k = %d; %s; AIPE).",
          as.integer(p$n_req), format(p$conf * 100), format(p$w), format(p$om), p$k,
          if (is.null(p$assur)) "expected width" else paste0(
            sprintf("%s%% assurance target requested; ", format(p$assur * 100)),
            sieve_reliability_assurance_text(p$assurance_summary))
        ),
        plan_code(p)
      )
      plan_logged(list(
        key = key,
        msg = sprintf("Recorded: N = %d, in the Reproducible Code view.", as.integer(p$n_req))
      ))
    })
    output$plan_logged <- renderUI({
      done <- plan_logged()
      req(done, identical(done$key, plan_inputs_key()))
      p(class = "small text-muted", done$msg)
    })

    # ---- Latent factor scores ---------------------------------------------
    # Bartlett scores from the correlated-factors CFA. The model is fit with
    # FIML (continuous), so incomplete cases still contribute, and the scores
    # are latent estimates, less attenuated than sum or mean composites. The
    # model is fit on request (the Compute factor scores button); the
    # scoring method is a display choice applied to the kept fit.
    scores_key <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      # Currency on the effective inputs: the included items,
      # without the facet column (this first-order model reads no facet).
      sieve_demand_key(state, list(estimator = scores_estimator()),
        items = state$meta$item[state$meta$status == "active"], facets = FALSE
      )
    })
    scores_plan <- reactive({
      req(state$meta)
      spec <- sieve_lavaan_spec(state$meta, estimator = scores_estimator())
      active <- state$meta[state$meta$status == "active" & sieve_construct_assigned(state$meta$construct), , drop = FALSE]
      p <- nrow(active)
      seconds <- sieve_demand_wait("scores", p, spec$estimator, missing = spec$missing)
      list(
        estimator = spec$estimator, missing = spec$missing, p = p, seconds = seconds,
        label = sieve_duration_label(seconds),
        progress = sprintf("Factor scores: correlated-factors model, %s, %d items", spec$estimator, p),
        detail = "Fitting the measurement model in lavaan, then scoring every case from it.",
        sentence = sprintf(
          "Fitting the correlated-factors model of %d items under %s for the scores is estimated to take %s",
          p, spec$estimator, sieve_duration_label(seconds)
        ),
        refused = sieve_wlsmv_guard(active$item, state$items,
          ordered = spec$ordered %||% character(0),
          estimator = spec$estimator
        )
      )
    })
    compute_scores <- function(key, plan) {
      items <- shiny::isolate(state$items)
      meta <- shiny::isolate(state$meta)
      index <- shiny::isolate(sieve_case_index(state))
      # The session's fit store: the identical correlated-factors model on
      # the identical data, fit on the CFA page (or elsewhere), is reused
      # rather than fit again (fct_fit_store.R).
      store <- shiny::isolate(state$fit_store)
      prepared <- sieve_sampling_apply(sieve_cfa_spec(meta,
        reserved = union(names(items), names(shiny::isolate(state$raw))),
        estimator = key$choices$estimator %||% "auto"), state, items)
      if (!is.null(prepared$reason)) stop(prepared$reason, call. = FALSE)
      index <- index[prepared$row_index]
      run <- sieve_run_cfa(prepared$spec, prepared$data,
        store = store, page = "Reliability", rows = index)
      run$input_rows <- prepared$row_index
      # Declared auxiliary variables left out because every analyzed item is
      # observed (sieve_auxiliary_apply()); the scores' method says so.
      run$aux_notes <- prepared$notes
      run$score_data <- prepared$data
      run$case_index <- index
      if (!is.null(run$fit)) {
        if (isTRUE(run$reused)) {
          reused <- sieve_reuse_text(run)
          sieve_log(
            state,
            sprintf("The measurement model behind the factor scores: %s%s",
              tolower(substring(reused, 1L, 1L)), substring(reused, 2L)),
            sieve_reuse_code(run, fit_name = "score_fit", model_name = "score_model")
          )
        } else {
          sieve_log(state, "Fit the measurement model behind the factor scores", c(
            if (length(run$aux_notes)) paste("#", run$aux_notes),
            sieve_cfa_code(run$spec, fit_name = "score_fit", model_name = "score_model")
          ))
        }
        sieve_fit_store_keep(store, run, fit_name = "score_fit", model_name = "score_model")
        run$code_view <- sieve_code_snapshot(state, context = c(
          sprintf("Score measurement model: estimator %s; missing data %s; std.lv = %s.",
            run$spec$estimator, run$spec$missing, run$spec$std_lv),
          sprintf("Modeled item columns: %s.", paste(run$spec$observed, collapse = ", ")),
          run$aux_notes,
          "The scoring method below reads this saved fit; changing the scoring method does not refit it."
        ))
      }
      run
    }
    demand_scores <- sieve_demand_wire(
      input, session, state,
      id = "scores_go", what = "Factor scores",
      key = scores_key, plan = scores_plan, compute = compute_scores, kind = "scores",
      noun = "The measurement model behind the scores", action = "Compute factor scores",
      refit = "Recompute factor scores",
      # The identical measurement model made on another page is adopted
      # without a click (fct_fit_store.R).
      peek = function(key, plan) {
        items <- shiny::isolate(state$items)
        prepared <- sieve_sampling_apply(sieve_cfa_spec(shiny::isolate(state$meta),
          reserved = union(names(items), names(shiny::isolate(state$raw))),
          estimator = key$choices$estimator %||% "auto"), state, items)
        is.null(prepared$reason) && sieve_fit_store_has(shiny::isolate(state$fit_store),
          prepared$spec, prepared$data, rows = shiny::isolate(sieve_case_index(state))[prepared$row_index])
      }
    )
    output$scores_code <- renderUI({
      st <- demand_scores$status()
      method <- input$score_method %||% "Bartlett"
      resolved <- if (!is.null(st$value$fit)) sieve_score_method(st$value$fit, method)$method else method
      sieve_code_view(st$value$code_view, st, ns = session$ns,
        readout = c(
          "# DMAR 1.1.0 supplies both score types, per-case intervals, and missing-pattern determinacy.",
          "both_scores <- DMAR::factor_scores(score_fit)",
          # The exported record (sieve_factor_scores() is internal, so the
          # line shown calls SIEVER::sieve_factor_score_record()): the same
          # alignment and the same labeled columns.
          sprintf("score_result <- SIEVER::sieve_factor_score_record(score_fit, method = %s)$scores",
            sieve_r_literal(method)),
          sprintf("# Resolved engine scoring method: %s; row mapping and undefined ML estimates are handled by sieve_factor_score_record(), which scores an admissible model.", resolved)
        ))
    })
    score_fit <- reactive({
      run <- demand_scores$result()
      req(run, run$fit)
      run$fit
    })

    observe({
      est <- scores_estimator()
      m <- state$meta
      categorical <- identical(
        est,
        "wlsmv"
      ) || (identical(
        est,
        "auto"
      ) && any(m$type[m$status == "active"] %in% c(
        "ordered",
        "binary"
      )))
      chosen <- isolate(input$score_method %||% "Bartlett")
      choices <- if (categorical) c(
        "Categorical ML (response likelihood)" = "ML",
        "Empirical Bayes mode (EBM)" = "EBM"
      ) else
        c("Bartlett (linear model)" = "Bartlett", "Regression / EAP (linear model)" = "regression")
      selected <- if (categorical) {
        if (chosen %in% c(
          "Bartlett",
          "ML"
        )) "ML" else "EBM"
      } else {
        if (chosen %in% c(
          "Bartlett",
          "ML"
        )) "Bartlett" else "regression"
      }
      updateSelectInput(session, "score_method", choices = choices, selected = selected)
    })
    fscores <- reactive({
      fit <- score_fit()
      req(sieve_has_solution(fit))
      # Columns carry the construct labels, in the table and in the file.
      sieve_factor_scores(fit, method = input$score_method %||% "Bartlett", labels = TRUE)
    })

    output$scores_note <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(NULL)
      }
      line <- demand_scores$line()
      run <- demand_scores$result()
      model_alert <- if (!is.null(run) && !sieve_has_solution(run)) {
        sieve_status_alert(run, what = "correlated-factors model behind the scores")
      }
      meth <- input$score_method %||% "Bartlett"
      treatment <- if (!is.null(run$fit)) sieve_score_method(run$fit, meth) else NULL
      categorical <- !is.null(treatment) && isTRUE(treatment$categorical)
      meth_txt <- if (categorical) {
        if (identical(
          treatment$method,
          "ML"
        )) "Categorical ML factor scores maximize the response-pattern likelihood; finite estimates can be unavailable for extreme patterns. Linear Bartlett unbiasedness does not apply" else
          "Empirical Bayes modal factor scores (EBM) maximize the conditional posterior density. They are posterior modes, not posterior means or EAP scores"
      } else if (identical(if (is.null(treatment)) meth else treatment$method, "regression")) {
        "Regression scores (the posterior mean of the factor given the items; the EAP estimator for the normal model): shrunken toward the factor mean, so each person's score is biased toward the mean, with the smallest mean squared error of the two"
      } else {
        "Bartlett scores (the weighted least squares estimate of the factor given the items): unbiased for each person's factor value under the linear model with known parameters, with estimation error added to their variance"
      }
      missing_sentence <- if (is.null(treatment)) "Fit the measurement model to see its actual estimator and missing-data treatment." else if (treatment$missing %in% c(
        "ml",
        "fiml",
        "ml.x",
        "fiml.x"
      )) {
        "The model is fit with full-information maximum likelihood, so available item responses contribute (Estabrook & Neale, 2013)."
      } else sprintf(
        "The training model uses %s with missing = '%s'. This is not a FIML fit.",
        treatment$estimator,
        treatment$missing
      )
      # Unbiasedness of a person's score and consistency of a slope estimated
      # from scores are different properties. The algebra is in
      # tests/testthat/test-factor-scores.R. The method text is shown before
      # the model is fit, so the choice can be read first.
      read <- sieve_outcome(fscores())
      # The preview beside the operational score in force: the preview scores
      # every respondent the model scores, with no minimum answered rule, and
      # is the operational score only where the specification says so.
      state$meta
      policies <- state$scoring_policies %||% list()
      spec_now <- sieve_score_specification(state, available = names(state$items))
      preview_code <- if (identical(meth, "regression")) "regression" else "bartlett"
      same <- length(spec_now) && all(vapply(spec_now, function(s) identical(s$method, preview_code), logical(1)))
      operational_sentence <- if (!length(spec_now)) {
        ""
      } else if (same) {
        paste(
          " Every construct's operational score is this factor score; the preview applies no minimum",
          "answered rule, and the operational score applies the one in force (Operational scores)."
        )
      } else {
        paste0(
          " This preview applies no minimum answered rule and is not the operational score in force ",
          "(Operational scores): ", sieve_score_policy_sentence(spec_now, policies)
        )
      }
      n_rows <- if (read$ready && read$ok && !is.null(read$value)) nrow(read$value) else NA_integer_
      n_unscored <- if (read$ready && read$ok && !is.null(read$value)) sum(rowSums(is.finite(as.matrix(read$value))) < ncol(read$value)) else 0L
      tagList(
        if (n_unscored > 0L) div(
          class = "alert alert-info small",
          sprintf(
            "%d respondents have at least one unavailable score. Input rows are retained; the download gives a reason for each unavailable score.",
            n_unscored
          )
        ),
        line,
        sieve_reuse_line(run, demand_scores$status()),
        model_alert,
        if (read$ready && !read$ok) {
          div(class = "alert alert-danger", "The scores could not be computed: ", read$error)
        },
        sieve_more_below(
          paste0(meth_txt, ". ", missing_sentence,
            if (!is.null(run) && length(run$aux_notes)) paste0(" ", paste(run$aux_notes, collapse = " ")),
            " Both Bartlett and regression scores, with intervals and missing-pattern determinacy where defined, are available in the two-score download (DMAR 1.1.0). Regression intervals are posterior intervals; Bartlett intervals describe conditional score error. Both treat fitted parameters as known and omit their sampling uncertainty. Use regression scores when the score is a predictor and Bartlett scores when it is an outcome (Skrondal & Laake, 2001), from separate predictor and outcome measurement models. Prefer a latent SEM, or local SAM for small or moderate N (Rosseel & Loh, 2024), over a regression among scores. Bootstrap the full two-step procedure for its standard errors. One column per construct in this preview", if (is.finite(n_rows)) sprintf(", %d rows", n_rows) else "", ".",
            operational_sentence),
          HTML(sprintf(
            paste0(
              "%s. The following regression-on-scores guidance concerns linear continuous-indicator scoring, not categorical ML/EBM. Which score to use depends on what the scores are for. For describing or reporting a ",
              "person's standing, the two trade bias for error variance. For a later regression neither is ",
              "the right choice in general: used as a <b>predictor</b>, a Bartlett score carries its estimation ",
              "error into the predictor and attenuates the slope, whereas a regression score recovers it; used ",
              "as an <b>outcome</b>, the reverse holds (Skrondal &amp; Laake, 2001). With several correlated ",
              "factors, or with scores on both sides, these simple rules do not carry over, and in every case ",
              "the standard errors of a regression on estimated scores need correction (Devlieger, Mayer, &amp; ",
              "Rosseel, 2016). %s One column per construct%s. For a ",
              "question about relations among constructs, prefer a latent structural model (the SEM and Validity ",
              "tabs), which estimates the relation within the model and not from estimated scores; that removes ",
              "the scoring step, not the possibility that the measurement or structural model is misspecified."
            ),
            meth_txt, missing_sentence, if (is.finite(n_rows)) sprintf(", %d rows", n_rows) else ""
          )),
          label = "Factor-score assumptions and use in later analyses"
        )
      )
    })

    output$scores_tbl <- renderDT({
      read <- sieve_outcome(fscores())
      req(read$ready)
      if (!read$ok) {
        return(sieve_dt(data.frame(Note = paste("The scores could not be computed:", read$error)),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      fs <- read$value
      req(fs)
      preview <- utils::head(fs, 15L)
      index <- demand_scores$result()$case_index
      req(length(index) == nrow(fs))
      preview <- data.frame(
        row = seq_len(nrow(preview)),
        original_row = utils::head(
          index,
          nrow(preview)
        ),
        preview,
        check.names = FALSE
      )
      sieve_dt(preview,
        rownames = FALSE, selection = "none",
        options = list(dom = "t", scrollX = TRUE, pageLength = 15L),
        caption = "First 15 rows shown; download for all cases."
      ) |>
        formatRound(setdiff(names(preview), c("row", "original_row")), 3)
    })

    output$dl_both_scores <- downloadHandler(
      filename = function() "SIEVER_bartlett_and_regression_scores.csv",
      content = function(file) {
        if (!identical(demand_scores$status()$state, "current")) stop(
          "Factor scores no longer match the current request. Recompute factor scores before downloading.", call. = FALSE)
        run <- demand_scores$result()
        fit <- run$fit
        record <- sieve_factor_score_record(fit, data = run$score_data %||% state$items,
          conf_level = sieve_finite_or(input$conf_level, .95),
          original_rows = run$case_index)
        shiny::req(record$both_scores)
        table <- as.data.frame(record$both_scores)
        table$original_row <- record$original_rows[table$row]
        table$factor <- sieve_show_names(table$factor, fit)
        utils::write.csv(table, file, row.names = FALSE)
      }
    )
    output$dl_scores <- downloadHandler(
      filename = function() "siever_factor_scores.csv",
      content = function(file) {
        if (!identical(demand_scores$status()$state, "current")) stop(
          "Factor scores no longer match the current request. Recompute factor scores before downloading.", call. = FALSE)
        fs <- fscores()
        req(fs)
        index <- demand_scores$result()$case_index
        if (length(index) != nrow(fs)) stop("Score rows cannot be aligned to their fitted original rows. Recompute factor scores.", call. = FALSE)
        out <- data.frame(row = seq_len(nrow(fs)), original_row = index, fs, check.names = FALSE)
        record <- sieve_score_rows(
          score_fit(),
          sieve_score_method(
            score_fit(),
            input$score_method %||% "Bartlett"
          )$method
        )
        for (j in seq_len(ncol(fs))) out[[paste0(names(fs)[j], " reason")]] <- record$reasons[, j]
        utils::write.csv(out, file, row.names = FALSE)
      }
    )
    advanced_reliability <- sieve_reliability_evidence_server(
      input, output, session, state,
      function() demand_scores$result(), function() identical(demand_scores$status()$state, "current")
    )

    # ---- One Update per page -------------------------------------------------
    # The results on this page that an edit can leave from before are the
    # reliability table and the factor scores. Each is updated by the page's
    # one Update, in the status line, when it was computed before and is not
    # current now: from before (the data, the respondents or the item pool
    # changed), or not computed for the settings now in force although it
    # was computed for others. Update never starts a result that was never
    # requested: a first table follows the run rule and its Compute
    # reliability button, the factor scores keep their own button on their
    # tab, and a bootstrap interval keeps Run the bootstrap (below the
    # table), which states its wait. A failed table is retried with the
    # others. The AIPE planner's prefilled value follows the table it was
    # taken from (see prefill_apply()).
    words_and <- function(x) {
      if (length(x) <= 1L) paste(x, collapse = "") else paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
    }
    first_cap <- function(x) paste0(toupper(substring(x, 1L, 1L)), substring(x, 2L))
    update_parts <- function() {
      list(
        table = list(wire = demand, plan = rel_plan, name = "the reliability table"),
        scores = list(wire = demand_scores, plan = scores_plan, name = "the factor scores")
      )
    }
    phase_of <- function(x) {
      read <- sieve_outcome(x$wire$status())
      if (!read$ready || !read$ok) {
        return("blocked")
      }
      st <- read$value
      if (identical(st$state, "not_run") && !isTRUE(st$withheld) && length(x$wire$store())) "settings" else st$state
    }
    update_targets <- reactive({
      parts <- update_parts()
      phases <- vapply(parts, phase_of, character(1))
      due <- phases %in% c("stale", "settings")
      if (any(due)) due <- due | phases == "failed"
      out <- parts[due]
      for (nm in names(out)) out[[nm]]$phase <- phases[[nm]]
      out
    })
    plan_seconds <- function(x) {
      read <- sieve_outcome(x$plan())
      if (read$ready && read$ok && length(read$value$seconds) == 1L) read$value$seconds else NA_real_
    }
    update_plan <- reactive({
      targets <- update_targets()
      seconds <- sum(vapply(targets, plan_seconds, numeric(1)))
      list(
        targets = targets, seconds = seconds,
        what = words_and(vapply(targets, function(x) x$name, character(1)))
      )
    })

    # The status line when something waits for Update: what is from before
    # and what changed, in one sentence, then the one button.
    page_line <- function() {
      up <- update_plan()
      if (!length(up$targets)) {
        return(demand$line())
      }
      st <- lapply(up$targets, function(x) x$wire$status())
      stale <- vapply(up$targets, function(x) identical(x$phase, "stale"), logical(1))
      changed <- unique(unlist(lapply(st[stale], function(s) s$sentence), use.names = FALSE))
      refusals <- unlist(lapply(up$targets, function(x) {
        read <- sieve_outcome(x$plan())
        if (read$ready && read$ok) read$value$refused else if (read$ready) read$error
      }), use.names = FALSE)
      table <- st$table
      lead <- if (any(stale)) {
        sprintf(
          "The data or the item pool changed since %s %s computed (what changed: %s). ",
          words_and(vapply(up$targets[stale], function(x) x$name, character(1))),
          if (sum(stale) == 1L) "was" else "were",
          paste(changed, collapse = "; ")
        )
      }
      settings <- vapply(up$targets, function(x) identical(x$phase, "settings"), logical(1))
      settings_lead <- if (any(settings)) {
        sprintf(
          "The settings changed since %s %s computed, so %s not shown for the settings now in force. ",
          words_and(vapply(up$targets[settings], function(x) x$name, character(1))),
          if (sum(settings) == 1L) "was" else "were",
          if (sum(settings) == 1L) "it is" else "they are"
        )
      }
      failed <- vapply(up$targets, function(x) identical(x$phase, "failed"), logical(1))
      failed_lead <- if (any(failed)) {
        sprintf(
          "%s stopped with an error and is computed again. ",
          first_cap(words_and(vapply(up$targets[failed], function(x) x$name, character(1))))
        )
      }
      before <- if (!is.null(table) && identical(up$targets$table$phase, "stale")) {
        sprintf("The table below is from before (%s at %s). ", table$progress %||% "Reliability", table$when)
      }
      tagList(
        if (length(refusals)) div(class = "alert alert-danger small", strong("Not fit. "), paste(refusals, collapse = " ")),
        div(
          class = "alert alert-warning",
          strong(paste0(lead, settings_lead, failed_lead)),
          before,
          sprintf(
            "Update recomputes %s%s, estimated at %s; a bootstrap interval keeps its own button below the table.",
            up$what, if (length(up$targets) > 1L) " in turn" else "", sieve_duration_label(up$seconds)
          ),
          div(class = "mt-2", actionButton(session$ns("update_go"),
            sprintf("Update (%s)", sieve_duration_label(up$seconds)),
            class = "btn-primary"
          ))
        )
      )
    }

    # Update: every result that waits for it, in turn, under one progress
    # message, then one notification saying what was updated and how long
    # each took (a failure is announced by its own notification, which
    # stays, and by the status line).
    page_update <- sieve_update_wire(input, session, function() {
      targets <- names(update_targets())
      lapply(update_parts(), function(part) {
        entry <- sieve_update_demand_entry(part$wire, part$name, part$plan)
        if (!is.null(entry)) entry$retry <- part$name %in% vapply(update_parts()[targets], `[[`, "", "name")
        entry
      })
    })
    update_page <- page_update$run

  })
}

#' The Reliability table as it is shown
#'
#' A display helper (the compact table). It selects and
#' formats columns of the full table and changes no value: the full table
#' (`sieve_reliability_table()`) keeps every column at full precision for
#' the stored results, the project and the report. By default the table
#' shows the construct (or facet), k, the coefficient, its estimate, its
#' interval in one column, the interval method, N used, the largest
#' one-factor residual correlation with its item pair
#' (`sieve_reliability_residual_text()`), Omega (model-implied denominator)
#' with its interval wherever it is computed beside robust omega (so every
#' construct of continuous items has an interval in view before any
#' bootstrap is run: robust omega's is a bootstrap run on request, model
#' implied omega's is immediate; for ordered items every interval is a
#' bootstrap, so their model implied omega is a point estimate and its
#' interval cell says so), the
#' items loading opposite in sign to the others when any construct has one,
#' and, when a categorical
#' construct's codes do not step by one shared amount, the composite the
#' coefficient scores (the sum of category ranks) and the coded composite's
#' reliability. `show_all` adds the rest (the composite scored and the
#' coded composite always among them): the
#' scope columns of a facet analysis, the method of model implied omega's
#' interval, the one-factor RMSEA with its interval
#' (at the page's confidence level, named in the column), the missing data
#' treatment, and H with its interval. Alpha and
#' the denominator diagnostic appear when their own boxes are checked
#' (alpha's estimate and interval in the compact view; its interval method,
#' N and status added by `show_all`). An interval is written as
#' `[lower, upper]` at three decimals, the precision the browser shows
#' every estimate at.
#'
#' @param df The full table from `sieve_reliability_table()`.
#' @param results The results the table was built from, in the same order
#'   (for the residual text and the method of an interval not run yet).
#' @param show_all,show_alpha,show_difference Logical.
#' @return A list with `data` (the data frame shown), `colnames`, `round`
#'   (the numeric columns the browser shows at three decimals) and `text`
#'   (the columns that join two values, which are not sortable).
#' @author Ken Kelley
#' @noRd
sieve_reliability_display <- function(df, results, show_all = FALSE, show_alpha = FALSE,
                                      show_difference = FALSE) {
  results <- unname(results)
  level <- if (length(results)) format(results[[1]]$omega$conf_level * 100) else "95"
  # The level of the one-factor RMSEA intervals, read from the table (the
  # page's level, at which they were computed).
  rmsea_levels <- unique(df$rmsea_level[is.finite(df$rmsea_level %||% NA_real_)])
  rmsea_ci <- sieve_rmsea_ci_words(if (length(rmsea_levels) == 1L) rmsea_levels else NA_real_)
  interval_text <- function(lower, upper, otherwise) {
    ifelse(is.finite(lower) & is.finite(upper), sprintf("[%.3f, %.3f]", lower, upper), otherwise)
  }
  pending <- df$uncertainty == "pending"
  # A bootstrap not run yet: the interval column says "not run" (saying where
  # the button is would wrap every row of the table); the method column names
  # the bootstrap selected, and the button is below the table.
  method <- df$interval
  method[pending] <- vapply(results[pending], function(r) {
    om <- r$omega
    sprintf(
      "%s, %d replications",
      sieve_reliability_interval(om$spec$missing, om$uncertainty$requested)$label,
      as.integer(om$spec$B)
    )
  }, character(1))
  facets <- any(df$scope == "facet")
  # The columns and their names grow as options add them (an environment,
  # not superassignment).
  acc <- new.env(parent = emptyenv())
  acc$data <- data.frame(
    construct = df$construct,
    k = df$k,
    coefficient = df$coefficient,
    omega = df$omega,
    ci = interval_text(
      df$ci_lower, df$ci_upper,
      ifelse(pending, "not run", df$interval)
    ),
    interval = method,
    n_used = df$n_used,
    residual = vapply(results, function(r) sieve_reliability_residual_text(r$residuals), character(1)),
    stringsAsFactors = FALSE
  )
  acc$cn <- c(
    if (facets) "Construct or facet" else "Construct", "k items", "Coefficient", "Estimate",
    sprintf("%s%% CI", level), "Interval method", "N used",
    "Largest one-factor residual r (item pair)"
  )
  round_cols <- "omega"
  text_cols <- c("ci", "residual")
  add <- function(columns, names) {
    acc$data <- cbind(acc$data, as.data.frame(columns, stringsAsFactors = FALSE))
    acc$cn <- c(acc$cn, names)
  }
  # Omega (model-implied denominator) with its interval, in the compact
  # view too: robust omega's interval is a bootstrap run on request, so
  # without it the first view of the table would show point estimates of
  # omega with no interval in sight. Its immediate interval (DMAR's
  # default, the MLR (sandwich) SE with logit-scale limits) assumes the
  # one-factor model, whose RMSEA is under Show all columns.
  model_shown <- any(!is.na(df$model_interval) & !df$model_interval %in% c("not applicable", "not computed") &
    !grepl("^not available", df$model_interval))
  if (model_shown) {
    add(
      list(
        model_omega = df$model_omega,
        model_ci = interval_text(df$model_lower, df$model_upper, df$model_interval)
      ),
      c("Omega (model-implied denominator)", sprintf("Model-implied %s%% CI", level))
    )
    round_cols <- c(round_cols, "model_omega")
    text_cols <- c(text_cols, "model_ci")
  }
  # What each coefficient scores (sieve_reliability_coding()), shown when a
  # categorical construct's codes do not step by one shared amount, so the
  # sum of the category ranks and the sum of the codes differ, and with
  # Show all columns; the coding note itself is in the method note below.
  coded <- "coding_note" %in% names(df) && any(!is.na(df$coding_note))
  if ("score_target" %in% names(df) && (coded || isTRUE(show_all))) {
    add(
      list(score_target = df$score_target, coded_estimate = df$coded_estimate),
      c("Composite scored", "Coded composite (sum of the codes, point estimate)")
    )
    round_cols <- c(round_cols, "coded_estimate")
  }
  # Items loading opposite in sign to the others (sieve_reliability_keying()),
  # named when any construct has one; the method note says what it does to
  # omega and H and what to do.
  if ("keying_items" %in% names(df) && any(!is.na(df$keying_items))) {
    add(
      list(keying_items = ifelse(is.na(df$keying_items), "", df$keying_items)),
      "Loading opposite in sign (see the method note)"
    )
  }
  # The optional denominator diagnostic, shown when its box is checked.
  if (isTRUE(show_difference)) {
    add(
      list(omega_minus_model = df$omega_minus_model, sum_residual_cov = df$sum_residual_cov),
      c("Robust minus model-implied omega", "Summed residual covariance, 1'(S - \u03a3\u0302)1")
    )
    round_cols <- c(round_cols, "omega_minus_model", "sum_residual_cov")
  }
  if (isTRUE(show_all)) {
    # The method of model implied omega's interval (its estimate and
    # interval are in the compact view), with the fit of the one-factor
    # model it assumes.
    if (model_shown) {
      add(list(model_interval = df$model_interval), "Model-implied interval method")
    }
    add(
      list(
        rmsea_text = df$rmsea_text, missing = df$missing, H_label = df$H_label, H = df$H,
        H_ci = interval_text(df$H_lower, df$H_upper, "not available")
      ),
      c(sprintf("One-factor RMSEA [%s]", rmsea_ci), "Missing data", "H coefficient", "H", sprintf("H %s%% CI", level))
    )
    round_cols <- c(round_cols, "H")
    text_cols <- c(text_cols, "rmsea_text", "H_ci")
    if (facets) {
      acc$data <- cbind(
        data.frame(
          scope = df$scope, parent_construct = df$parent_construct, facet = df$facet,
          stringsAsFactors = FALSE
        ),
        acc$data
      )
      acc$cn <- c("Scope", "Parent construct", "Facet", acc$cn)
    }
  }
  if (isTRUE(show_alpha)) {
    add(
      list(alpha = df$alpha, alpha_ci = interval_text(df$alpha_lower, df$alpha_upper, df$alpha_interval)),
      c("\u03b1 (comparison)", sprintf("\u03b1 %s%% CI", level))
    )
    round_cols <- c(round_cols, "alpha")
    text_cols <- c(text_cols, "alpha_ci")
    if (isTRUE(show_all)) {
      add(
        list(alpha_interval = df$alpha_interval, alpha_n = df$alpha_n, alpha_status = df$alpha_status),
        c("\u03b1 interval method", "\u03b1 N used", "\u03b1 status")
      )
    }
  }
  data <- acc$data
  rownames(data) <- NULL
  list(data = data, colnames = acc$cn, round = round_cols, text = text_cols)
}

#' The method of the Reliability table in one line
#'
#' A display helper (results first): the coefficient, the
#' engine and the estimator of the one-factor model behind it, the cases
#' used and N, and the interval, for the rows of the table. Where the rows
#' differ, each variant is named; each row's full method is stated below
#' the table (`sieve_reliability_paragraph()`).
#'
#' @param results The results the table was built from.
#' @param noun `"construct"`, `"facet"` or `"group"`.
#' @return One sentence.
#' @author Ken Kelley
#' @noRd
sieve_reliability_method_line <- function(results, noun = "construct") {
  om <- lapply(unname(results), function(r) r$omega)
  pick <- function(f) unique(vapply(om, f, character(1)))
  coefficient <- pick(function(o) o$label)
  model <- pick(function(o) {
    if (identical(o$coefficient, "omega_categorical")) {
      sprintf("DMAR::%s, WLSMV on the polychoric correlations", o$spec$engine)
    } else {
      sprintf("DMAR::%s, ML", o$spec$engine)
    }
  })
  missing <- pick(function(o) if (identical(o$missing$used, "fiml")) "FIML" else "complete cases")
  n <- vapply(om, function(o) as.numeric(o$n$used %||% NA_real_), numeric(1))
  n <- n[is.finite(n)]
  n_text <- if (!length(n)) {
    "no usable rows"
  } else if (min(n) == max(n)) {
    sprintf("N = %d", as.integer(n[1]))
  } else {
    sprintf("N = %d to %d", as.integer(min(n)), as.integer(max(n)))
  }
  interval <- pick(function(o) {
    u <- o$uncertainty
    label <- sieve_reliability_interval(o$spec$missing, u$requested %||% "none")$label
    switch(u$status,
      completed = u$label,
      pending = sprintf("%s, not run yet", label),
      unavailable = "none for this coefficient",
      failed = sprintf("%s, failed", label),
      not_requested = "none requested",
      u$status
    )
  })
  # The declared auxiliary variables a FIML table left out
  # (sieve_auxiliary_unused_note()).
  notes <- unique(unlist(lapply(unname(results), function(r) r$aux_notes), use.names = FALSE))
  sprintf(
    paste0(
      "%s from each %s's one-factor model (%s; factor variance fixed to 1), %s, %s; ",
      "%s%% interval: %s.%s Each %s's method is stated below the table."
    ),
    paste(coefficient, collapse = "; "), noun, paste(model, collapse = "; "),
    paste(missing, collapse = " or "), n_text, format(om[[1]]$conf_level * 100),
    paste(interval, collapse = "; "), if (length(notes)) paste0(" ", paste(notes, collapse = " ")) else "", noun
  )
}

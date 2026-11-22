# mod_finalize.R
# Cross-validation, measurement invariance, and alignment. This is the gated
# finalize step: a late calibration/evaluation split is an internal
# assessment. Independent validation requires protection from prior exposure. The invariance ladder is fit by
# DMAR::measurement_invariance(), the canonical implementation: it fits the
# rungs of the ladder in force (five rather than four when the items are
# ordered, because thresholds carry the location information) and returns
# the fitted lavaan objects in its "fits" attribute, so the ladder table
# (fit indices in one family for every rung, their changes, and lavaan's
# scaled chi square difference tests; see sieve_fit_compare()), the score
# test diagnosis, the partial invariance refit, and the dMACS effect size
# all read those fits instead of refitting. DMAR::dmacs() supplies the
# effect size of noninvariance and DMAR::measurement_alignment() the
# approximate invariance solution that keeps factor means comparable when
# exact invariance fails across groups.

#' Finalize module UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput numericInput sliderInput actionButton selectInput
#' @importFrom shiny div p tags HTML strong em checkboxInput radioButtons
#' @importFrom bslib layout_sidebar sidebar card card_header navset_card_tab nav_panel
#' @importFrom DT DTOutput
mod_finalize_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      title = "Holdout / Cross-Validation",
      sieve_estimator_panel(ns, "Estimator for the models on this tab")
    ),
    # Results first: the one status line
    # (with the page's one Update when a result is from before), then the
    # panels, each with its result before its explanation; the purpose of
    # the page is below the panels.
    uiOutput(ns("gate")),
    navset_card_tab(
      nav_panel(
        "Holdout / Cross-Validation",
        # The evaluation on the rows reserved before development
        # (fct_holdout.R): a named, frozen version fit on the calibration
        # rows and evaluated on the reserved rows once.
        card(
          card_header("Evaluation on the Reserved Rows"),
          uiOutput(ns("holdout_msg")),
          div(
            class = "mb-2",
            actionButton(ns("holdout_go"), "Evaluate on the reserved rows", class = "btn-primary")
          ),
          DTOutput(ns("holdout_versions")),
          uiOutput(ns("holdout_tables")),
          sieve_more_below(
            tagList(
              "With a reservation in force (Screening page, Observations panel), pressing ",
              strong("Evaluate on the reserved rows"), " freezes the measurement model as it ",
              "stands (the included items with their constructs, the model syntax the CFA page ",
              "fits with its structure and identification, the estimator in force, and the scoring ",
              "specification), names the version, and states its model in the table, ",
              "fits it on the calibration rows, and evaluates it on the reserved rows once, with ",
              "the same fit, cross-validation discrepancy and reliability comparisons as the split ",
              "below. A version is evaluated once; pressing again with the same version shows that ",
              "evaluation and fits nothing."
            ),
            p(
              "The label says what the evaluation is. ", strong("Specified before evaluation"),
              ": the version predates the first evaluation of this reservation, so the reserved ",
              "rows played no part in fitting or selecting it, which is the condition under which a ",
              "validation sample estimates how the model would fare in a fresh sample (Browne, 2000; ",
              "Cudeck & Browne, 1983). ", strong("Revised during development"), ": a development ",
              "change created the version after an evaluation existed, so the reserved rows have ",
              "been looked at; the earlier evaluation becomes historical and cannot carry the ",
              "independent-validation label, and the new one is a second look. A reservation drawn ",
              "after results existed is marked as such on the Screening page and is a split of data ",
              "already examined. The Standards (AERA, APA, & NCME, 2014) ask that a validity claim ",
              "rest on evidence gathered for the interpretation proposed; fitting a CFA on the CFA ",
              "page does not make the model confirmatory, and a model is confirmed by data that did ",
              "not shape it. Without a reservation, the split below stays, with its disclosure."
            ),
            label = "More on the evaluation: the frozen version and its label"
          )
        ),
        card(
          card_header("Calibration vs Validation Fit"),
      # Whether a validation reservation is in force (fct_holdout.R): with
      # one, the evaluation on the reserved rows is on the panel; without
      # one, the late split below is internal assessment.
      uiOutput(ns("holdout_note")),
      p(
        "A calibration (training) / validation split. The measurement model is ",
        "evaluated in two disjoint subsets. A split made after examining the full data ",
        "is an internal assessment; it does not undo prior exposure or item selection."
      ),
      sliderInput(ns("holdout"), "Validation (holdout) percentage",
        min = 10, max = 90, value = 50, step = 5, post = "%"
      ),
      uiOutput(ns("split_sizes")),
      numericInput(ns("seed"), "Random seed (optional)", value = NULL, step = 1),
      actionButton(ns("split"), "Create split", class = "btn-primary w-100"),
      tags$hr(),
      tags$small(
        class = "text-muted",
        "The training percentage is 100 minus the validation percentage. ",
        "Supply a seed to choose the split; left empty, a seed is drawn ",
        "from the session's random number stream and recorded, so the ",
        "same split can be drawn again from the reproducible code."
      ),
          uiOutput(ns("cv_msg")),
          uiOutput(ns("cv_code")),
          DTOutput(ns("cv_tbl")),
          tags$small(
            class = "text-muted d-block mt-2",
            strong("ECVI"), " is the expected cross-validation index (Browne & Cudeck, ",
            "1989): the expected discrepancy of this solution in a fresh sample. Lower is ",
            "better and it can be compared across models even without a holdout. Its ",
            "confidence interval comes from the noncentral chi square (DMAR::ecvi); ",
            "it is blank when the model has zero degrees of freedom (df = 0)."
          )
        ),
        card(
          card_header("Cross-Validation: Training Solution on Validation Data"),
          uiOutput(ns("xval_msg")),
          DTOutput(ns("xval_tbl")),
          tags$small(
            class = "text-muted d-block mt-2",
            "The training (calibration) parameter estimates are applied unchanged to the ",
            "validation sample. The ", strong("tight"), " discrepancy uses the frozen training ",
            "parameters; the ", strong("loose"), " discrepancy re-estimates on validation. The ",
            "gap between them is shrinkage: how much the fit flatters itself by being estimated ",
            "on the same data. A small gap is evidence the solution generalizes."
          )
        ),
        card(
          card_header("Internal Reliability Comparison (Coefficient Omega, \u03c9, by Construct)"),
          uiOutput(ns("rel_msg")),
          DTOutput(ns("rel_tbl")),
          tags$small(
            class = "text-muted d-block mt-2",
            "Each construct's coefficient is the Reliability tab's target, estimated separately in the ",
            "training and validation samples and named in the Coefficient column: Robust omega (observed ",
            "denominator) by default, its denominator built from the unrestricted polychoric correlations ",
            "for ordered or binary items. Its intervals are bootstraps, which are not run for the split, ",
            "so they are point estimates here. Beside it, Omega (model-implied denominator) is given in ",
            "each half with its MLR (sandwich) SE (delta method) interval, and the difference between the halves ",
            "with a delta method interval for independent samples; the model-implied denominator is the ",
            "composite's variance only if the one-factor model holds. Beside the coefficient stands the ",
            "largest residual correlation of the one-factor model fitted in each half, with its item pair: ",
            "a large residual pair signals local dependence that raises omega without better measurement. ",
            "Similar values in the two halves ",
            "are consistent with a reliability that does not depend on which rows calibrated the model, ",
            "and the intervals show how far the halves could differ by sampling alone; both halves come ",
            "from the same data, so this is internal assessment, not independent replication."
          )
        )
      ),
      nav_panel(
        "Measurement Invariance",
        card(
          card_header("The Invariance Ladder Across a Grouping Variable"),
          div(
            class = "d-flex align-items-end gap-2 flex-wrap",
            selectInput(ns("group"), "Grouping variable", choices = character(0), width = "220px"),
            selectInput(ns("inv_missing"),
              "Continuous-item analysis sample",
              choices = c(
                "Follow the resolved estimator" = "auto",
                "FIML: retain partially observed cases" = "fiml",
                "Complete cases on every model item" = "complete"
              ),
              width = "320px"
            ),
            div(class = "mb-3", actionButton(ns("run_inv"), "Run invariance", class = "btn-primary"))
          ),
          uiOutput(ns("inv_head")),
          DTOutput(ns("inv_tbl")),
          uiOutput(ns("inv_msg")),
          uiOutput(ns("inv_code")),
          uiOutput(ns("inv_convention")),
          uiOutput(ns("inv_families")),
          sieve_more_below(
            tagList(
              "The ladder is fit by ", tags$code("DMAR::measurement_invariance()"),
              ", the canonical implementation, on the correlated-factors ",
              "measurement model that is the CFA tab's default: one latent variable per ",
              "construct with two or more active items, latent variances fixed ",
              "at 1. The ladder uses this first-order model whatever structure the CFA tab ",
              "is set to fit; the invariance of a higher-order or direct facet model is not ",
              "tested here. Each rung adds equality constraints across groups to the ",
              "rung before it. The table identifies the appropriate model-difference ",
              "test and its assumptions. With continuous items the rungs ",
              "are ", strong("configural"), " (same pattern, every parameter free ",
              "by group), ", strong("metric"), " (equal loadings, so relations ",
              "involving the factor are comparable), ", strong("scalar"),
              " (equal intercepts as well, required before comparing means), and ",
              strong("strict"), " (equal residual variances as well, required ",
              "before comparing observed score variances)."
            ),
            p(
              class = "small",
              strong("With ordered or binary items the ladder has five rungs, not four."),
              " A ", strong("thresholds"), " rung is fit between configural and ",
              "metric. For an ordered item the observed response is a coarsening ",
              "of an underlying continuous response at a set of thresholds, and ",
              "it is the thresholds, not an intercept, that locate the item on ",
              "the latent scale; constraining loadings while the thresholds stay ",
              "free across groups therefore does not deliver what metric ",
              "invariance is supposed to deliver, so the thresholds are ",
              "constrained first (Millsap & Yun-Tein, 2004; Wu & Estabrook, ",
              "2016). The table above reports whatever rungs were fit, so read ",
              "the row labels rather than counting on four."
            ),
            label = "Reading the invariance ladder: ordered and binary items"
          ),
          tags$hr(),
          selectInput(ns("inv_interpret_level"),
            "Inspect actual constraints and latent comparisons",
            choices = character()
          ),
          uiOutput(ns("inv_interpret_note")),
          DTOutput(ns("inv_latent_parameters")),
          DTOutput(ns("inv_anchors")),
          DTOutput(ns("inv_mean_contrasts"))
        ),
        card(
          card_header("Partial Invariance: Locate, Free, Refit"),
          div(
            class = "d-flex align-items-center gap-2 mb-2 flex-wrap",
            selectInput(ns("inv_level"), "Rung to diagnose",
              choices = c(
                "Metric (loadings)" = "metric",
                "Scalar (intercepts)" = "scalar",
                "Strict (residuals)" = "strict"
              ),
              selected = "scalar", width = "230px"
            ),
            actionButton(ns("inv_diag"), "Diagnose noninvariance", class = "btn-primary"),
            actionButton(ns("inv_refit"), "Refit freeing selected", class = "btn-outline-primary")
          ),
          uiOutput(ns("inv_diag_note")),
          DTOutput(ns("inv_diag_tbl")),
          uiOutput(ns("inv_partial_out")),
          uiOutput(ns("partial_code")),
          sieve_more_below(
            tagList(
              "When a rung fails, the constructive next step is ", strong("partial"),
              " invariance (Byrne, Shavelson & Muthen, 1989): find the few parameters ",
              "carrying the misfit, free them, and keep the rest constrained, ",
              "disclosing exactly what was freed. The diagnosis is a score test per ",
              "equality constraint (how much the chi square would drop if that one ",
              "constraint were released), the constrained-model analogue of a ",
              "modification index. Select the offending rows (usually one or two ",
              "intercepts) and refit. Mean comparisons require a defensible common ",
              "origin and scale; inspect actual constraints and anchors above. Freeing parameters found this ",
              "way is data driven, so it capitalizes on chance: disclose the freed ",
              "set, tie it to item content where possible, and confirm on fresh data."
            ),
            p(
              "The rung list is filled in from the ladder that was actually fit, ",
              "so with ordered items the thresholds rung appears here too. ",
              "Configural is not offered: it imposes no cross-group equality ",
              "constraints, so there is nothing to score test."
            ),
            label = "More on the diagnosis: the rungs offered"
          ),
          tags$hr(),
          p(
            class = "small",
            "Compare at least two defensible anchor sets. Use one line per set: name: item1, item2, item3. Each construct needs at least two retained anchors. Other loadings and locations are released. The choices and each exact model remain separate."
          ),
          textAreaInput(ns("inv_anchor_sets"), "Anchor sets (exact item column names)", rows = 4),
          textAreaInput(ns("inv_anchor_rationale"), "Substantive anchor rationale", rows = 2),
          checkboxInput(ns("inv_anchor_discovery"), "These choices used the current data", value = TRUE),
          actionButton(ns("inv_anchor_run"), "Compare and freeze anchor decisions", class = "btn-outline-primary"),
          uiOutput(ns("inv_anchor_note")),
          DTOutput(ns("inv_anchor_contrasts"))
        ),
        card(
          card_header("How Large Is the Noninvariance?"),
          div(class = "d-flex align-items-center gap-2 mb-2 flex-wrap",
            radioButtons(ns("dmacs_source"), "Fitted model to judge",
              choices = c(
                "Partial invariance refit" = "partial",
                "Ladder rung selected above" = "rung"
              ),
              selected = "partial", inline = TRUE
            ),
            selectInput(ns("dmacs_ref"), "Reference group",
              choices = character(0), width = "200px"
            ),
            selectInput(ns("dmacs_foc"), "Focal group",
              choices = character(0), width = "200px"
            )),
          uiOutput(ns("inv_impact_pending")),
          conditionalPanel(condition = sprintf("output['%s'] === 'continuous'", ns("inv_impact_kind")),
          actionButton(ns("dmacs_go"), "Compute dMACS", class = "btn-primary"),
          uiOutput(ns("dmacs_note")),
          DTOutput(ns("dmacs_tbl")),
          sieve_more_below(
            tagList(
              "A score test or a likelihood ratio test answers whether a ",
              "violation is ", em("detectable"), ", and in a large sample almost ",
              "any nonzero difference is. ", strong("dMACS"),
              " (Nye & Drasgow, 2011; computed by ", tags$code("DMAR::dmacs()"),
              ") answers how much it ", em("matters"), ". For each item it is the ",
              "root mean squared vertical distance between the reference group's ",
              "and the focal group's measurement equations, averaged over the ",
              "focal group's latent distribution and divided by the pooled item ",
              "standard deviation, so it reads on the same scale as a ",
              "standardized mean difference. Writing a = nu(reference) - ",
              "nu(focal) for the intercept difference and b = lambda(reference) ",
              "- lambda(focal) for the loading difference, the value is ",
              "sqrt(a^2 + 2ab*mu + b^2(sigma^2 + mu^2)) / SD(pooled), with mu ",
              "and sigma the focal group's latent mean and standard deviation. ",
              "It is nonnegative and carries no sign; the direction is read from ",
              "the components, which are reported beside it. No verbal size ",
              "label is attached: what counts as consequential depends on the ",
              "decision the scores inform, not on a table of cutoffs."
            ),
            p(
              strong("Which fitted model dMACS can be read from."),
              " dMACS compares the two groups' measurement equations, which is ",
              "meaningful only when their loadings ", em("and"), " intercepts are ",
              "expressed on a common metric. The configural model gives each ",
              "group its own latent scale, so its parameters are not comparable ",
              "at all, and a thresholds-only rung leaves the loadings free. The ",
              "metric rung is the case worth understanding, because it looks ",
              "eligible and is not: the loadings are equal there, but every ",
              "intercept is free and the group latent means are fixed at zero, ",
              "so a real difference in the latent ", em("mean"), " is absorbed ",
              "into the intercepts and would be reported as noninvariance that ",
              "does not exist. On data constructed to be perfectly invariant and ",
              "to differ only in latent mean, the metric rung returns a large ",
              "dMACS while the scalar rung correctly returns zero. The index is ",
              "therefore offered only from a model that constrains the ",
              "intercepts: the scalar or strict rung, or a partial invariance ",
              "refit of one. The partial refit is the informative case and the ",
              "setting the index was defined for (Nye & Drasgow, 2011): the ",
              "constrained items anchor the metric and estimate the focal ",
              "group's latent mean, while the parameters you freed carry the ",
              "noninvariance whose size is being reported. At a fully ",
              "constrained rung nothing is free to differ, so the values are ",
              "zero up to rounding, which is the correct answer rather than a ",
              "failure."
            ),
            p(
              strong("Not available with ordered or binary items."),
              " When the items are declared ordered, no rung of the ladder makes ",
              "this index readable and the panel refuses it rather than ",
              "reporting a number. dMACS is a function of the loadings and the ",
              "intercepts, and for an ordered item the intercept is not what ",
              "carries location: the thresholds are. The intercept of the ",
              "underlying continuous response is fixed at zero in the reference ",
              "group and freed in the remaining groups only so the model is ",
              "identified once the thresholds are held equal across groups. An ",
              "index reading the loadings and those intercepts would report that ",
              "identification rule as though it were noninvariance, and would ",
              "say nothing at all about the thresholds, which are the parameters ",
              "an ordered ladder actually constrains. Sizing noninvariance for ",
              "ordered items calls for a different quantity, built on the ",
              "expected item score function implied by an item's thresholds ",
              "together with its loading, not a reuse of ",
              tags$code("dmacs()"), ". The separate expected-score impact control below ",
              "gives that impact on a verified common metric, each value with a ",
              "pointwise interval from draws of the item parameters."
            ),
            label = "More on dMACS: the model it can be read from, and ordered items"
          ),
          tags$small(
            class = "text-muted d-block mt-2",
            "dMACS compares exactly two groups. When the grouping variable has ",
            "more than two, name the reference and the focal group above; the ",
            "result is that pair, and every other pair is a separate run. Rows ",
            "are sorted by dMACS, largest first, so the items carrying the ",
            "practically largest violation lead."
          )),
          conditionalPanel(condition = sprintf("output['%s'] === 'ordered'", ns("inv_impact_kind")),
          p(
            class = "small",
            "For ordered items, expected-score impact integrates differences in category probabilities over a declared common trait distribution. Values are focal minus reference in category-score units, not dMACS. A common metric and at least two verified anchors per factor are required. Each value has a 95% pointwise interval from 1,000 draws of the item parameters from their joint asymptotic normal distribution (the trait distribution, category scores, and anchors held fixed); absolute and RMS limits are biased upward near 0, because those values cannot be negative."
          ),
          numericInput(ns("ordinal_impact_mean"), "Common normal trait mean", value = 0),
          numericInput(ns("ordinal_impact_sd"), "Common normal trait SD", value = 1, min = .01),
          textAreaInput(ns("ordinal_impact_anchors"), "Common-metric and anchor justification", rows = 2),
          actionButton(ns("ordinal_impact_go"), "Compute ordinal expected-score impact", class = "btn-outline-primary"),
          uiOutput(ns("ordinal_impact_note")),
          DTOutput(ns("ordinal_impact_tbl")),
          p(
            class = "small",
            "Total impact describes the complete-response unit-weight sum within each factor. Signed effects can cancel; absolute and RMS total impact are computed after summing the item differences."
          ),
          DTOutput(ns("ordinal_impact_totals")))
        )
      ),
      nav_panel(
        "Alignment",
        card(
          card_header("Approximate Invariance by Alignment"),
          div(
            class = "d-flex align-items-end gap-2 mb-2 flex-wrap",
            selectInput(ns("align_construct"), "Construct to align",
              choices = character(0), width = "220px"
            ),
            sieve_settings("Alignment Settings",
              ids = ns(c("align_ident", "align_starts", "align_seed")),
            radioButtons(ns("align_ident"), "Identification",
              choices = c(
                "Fixed (first group is the reference)" = "fixed",
                "Free (no reference group)" = "free"
              ),
              selected = "fixed"
            ),
            numericInput(ns("align_starts"), "Starting values",
              value = 10,
              min = 1, max = 200, step = 1, width = "140px"
            ),
            numericInput(ns("align_seed"), "Random seed (optional)",
              value = NULL,
              step = 1, width = "180px"
            ),
            checkboxInput(ns("align_std"), "Standardize the items first",
              value = TRUE
            )
            ),
            actionButton(ns("align_go"), "Run alignment", class = "btn-primary")
          ),
          uiOutput(ns("align_note")),
          DTOutput(ns("align_tbl")),
          uiOutput(ns("align_code")),
          sieve_more_below(
            tagList(
              "The ladder on the Measurement Invariance panel asks whether ",
              "loadings and intercepts are ", em("exactly"),
              " equal across groups. Across ",
              "many groups that hypothesis is essentially never true: some item ",
              "behaves a little differently somewhere, the scalar rung is ",
              "rejected, comparison stalls at configural, and the comparison of ",
              "factor means the researcher wanted never happens. ",
              strong("Alignment"), " (Asparouhov & Muthen, 2014; computed by ",
              tags$code("DMAR::measurement_alignment()"), ") takes the configural ",
              "solution as given and asks a different question: of all the ",
              "group factor means and variances that fit the data identically ",
              "(the configural solution is not unique in this respect), which ",
              "set makes the measurement parameters as close to invariant as ",
              "possible? The answer is comparable factor means without the claim ",
              "that exact invariance holds, which is what ", strong("approximate"),
              " invariance means."
            ),
            p(
              strong("How the criterion decides."), " Alignment minimizes a ",
              "simplicity function that sums, over items and over pairs of ",
              "groups, the component loss f(x) = (x^2 + epsilon)^(1/4) applied to ",
              "the difference in a loading or an intercept, weighted by ",
              "sqrt(n1 * n2). The fourth root is the substance of the method, not ",
              "a technical detail: it is concave in the size of the difference, ",
              "so the marginal penalty falls as a difference grows. A squared ",
              "loss would spread a fixed amount of noninvariance evenly over all ",
              "the parameters; this one tolerates a few large differences and ",
              "punishes many small ones, which is exactly the pattern of ",
              "approximate invariance a researcher wants to find and report. The ",
              "constant epsilon (0.01 here) only rounds off the kink at zero so ",
              "the criterion is differentiable. The achieved value is comparable ",
              "across runs on the same items, groups, and epsilon, not across ",
              "data sets."
            ),
            p(
              strong("Two things to know before reading the numbers."), " First, ",
              "alignment is defined for two groups but has little to offer ",
              "there: with two groups the ordinary ladder is tractable and ",
              "interpretable and the criterion has a single group pair to work ",
              "with. It earns its keep when the number of groups makes exact ",
              "invariance implausible. Second, the criterion adds up raw ",
              "parameter differences, so items on very different measurement ",
              "scales do not contribute equally; standardizing the items first ",
              "(the default) puts them on a common metric. Alignment is ",
              "defined for a single factor, so one construct is aligned at a ",
              "time, and the configural model is fit with ML: the criterion uses ",
              "only its point estimates, which ML and robust ML share."
            ),
            p(
              "Under ", strong("fixed"), " identification the first group's ",
              "factor mean is held at 0 and its factor variance at 1, so every ",
              "reported mean is in that group's factor standard deviations and ",
              "every reported variance is a multiple of its factor variance. ",
              "Under ", strong("free"), " the group factor means average 0 and ",
              "the factor standard deviations have a geometric mean of 1, so no ",
              "group is the reference. The two rules give genuinely different ",
              "solutions, not a relabeling of one another. Alignment refits and ",
              "then optimizes, so it runs on the button, not as you change ",
              "inputs; leave the seed empty to have one drawn from the session's ",
              "stream and recorded with the result."
            ),
            label = "More on alignment: how the criterion decides, two things to know, and identification"
          )
        ),
        card(
          card_header("Solution Diagnostics"),
          uiOutput(ns("align_diag")),
          p(
            class = "small text-muted mt-2",
            "The simplicity function has local minima, so the optimizer is run ",
            "from several starting values and the best solution is kept. When ",
            "more than one distinct optimum is reached, the reported solution ",
            "is the best of several rather than the unique answer, and the ",
            "sensible response is to raise the number of starting values and ",
            "check that the factor means are stable. Starts that drift into ",
            "the degenerate branch of the criterion (factor variances running ",
            "off while the aligned loadings collapse toward zero) are ",
            "discarded rather than reported."
          )
        ),
        card(
          card_header("Per Item Invariance and Simplicity Contribution"),
          uiOutput(ns("align_r2_note")),
          DTOutput(ns("align_r2_tbl")),
          p(
            class = "small text-muted mt-2",
            "The per item R-squared of Asparouhov and Muthen (2014) asks how ",
            "much of the group to group variation in an item's configural ",
            "parameter is accounted for by the estimated factor means and ",
            "variances alone. A value of 1 on the loadings for every item is ",
            "metric invariance; 1 on both loadings and intercepts is scalar ",
            "invariance. Low values name the items whose parameters the factor ",
            "means and variances cannot explain, that is, the noninvariant ",
            "ones. The loss columns are the complementary view: they split the ",
            "achieved simplicity function into each item's contribution, so ",
            "the items carrying the noninvariance can be named directly. Rows ",
            "are sorted by total contribution, largest first."
          )
        )
      ),
      nav_panel("Longitudinal Invariance", mod_longitudinal_ui(ns("longitudinal")))
    ),
    sieve_more_below(
      tagList(
        "Two safeguards against over-fitting the scale to one sample, and the tools for ",
        "comparing groups. ", strong("Cross-validation"),
        " requires protected evaluation data and frozen choices. Splitting already inspected data ",
        "provides internal assessment, and cannot establish independent validation."
      ),
      p(
        strong("Measurement invariance"),
        " (Meredith, 1993) tests whether the model means the same thing across groups, in the ",
        "cumulative order configural, then ", strong("metric"), " (equal loadings: relations are ",
        "comparable), then ", strong("scalar"), " (equal intercepts: required before comparing ",
        "means), then ", strong("strict"), " (equal residuals); with ordered or binary items a ",
        strong("thresholds"), " rung comes between configural and metric, so the ladder has five ",
        "rungs (Wu & Estabrook, 2016). For ordinal means, inspect the actual location constraints and which group means are free; thresholds and loadings alone do not establish mean comparability under every identification. Read each step from the scaled chi square ",
        "difference test together with the changes in CFI and RMSEA. A fall in CFI of no ",
        "more than .01 and a rise in RMSEA of no more than .015 are widely used reference ",
        "values (Cheung & Rensvold, 2002; Chen, 2007), but they are conventions from ",
        "simulations with maximum likelihood and continuous indicators, not a test, and the ",
        "table states which family of fit indices its changes are in. If the scalar model is ",
        "not tenable, free the offending ",
        "intercepts for partial invariance (Byrne, Shavelson & Muthen, 1989) and read the ",
        strong("dMACS"), " effect size to judge whether the violation is large enough to matter ",
        "(Nye & Drasgow, 2011). When there are many groups, exact invariance essentially never ",
        "holds and the ladder stalls at configural; the ", strong("Alignment"), " panel estimates ",
        "the group factor means and variances that make the measurement parameters as nearly ",
        "invariant as possible instead (Asparouhov & Muthen, 2014)."
      ),
      label = "Cross-validation and invariance: purpose and interpretation"
    )
  )
}

#' The Scoring and Norms sub-tab mounted in Reliability
#'
#' The card's inputs and outputs belong to the module server
#' (`mod_finalize_server()`): the card is a sub-tab of the Reliability page
#' under the module id `"finalize"`, so the server that runs the CV and
#' Invariance page runs it as well, with one definition.
#'
#' @param id Module id (the same id as `mod_finalize_ui()`).
#' @return A card.
#' @author Ken Kelley
#' @noRd
mod_finalize_scoring_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Scoring rules, sample reference table, and frozen norm references"),
    uiOutput(ns("norms_note")),
    uiOutput(ns("scoring_eligibility")),
    DTOutput(ns("norms_tbl")),
    uiOutput(ns("norms_code")),
    p(
      class = "small text-muted mt-2",
      "Scores follow each construct's response policy from Reliability, including ",
      "its minimum answered count and weights. Available-response means can represent ",
      "different content for different people. The current-sample table is descriptive. ",
      "Freeze a named calibration reference to apply its original mean, SD, and empirical ",
      "percentile function unchanged to later cohorts. Describe the eligible population; ",
      "a sample reference alone does not establish population norms."
    ),
    p(
      class = "small",
      "Point quantiles use R quantile type 7. Pointwise percentile limits use distribution-free binomial order statistics, conservatively retaining ties. Applied percentile ranks use percent below plus half the percent equal in the frozen calibration sample. These ranks and interpolated type-7 quantiles are distinct conventions. Infinite limits show insufficient tail information. Ten scores is a display minimum, not a precision guarantee."
    ),
    DTOutput(ns("norms_uncertainty")),
    textInput(ns("norm_name"), "New reference name"),
    textAreaInput(ns("norm_population"), "Eligible population and calibration limitations", rows = 2),
    actionButton(ns("norm_freeze"), "Freeze current calibration reference", class = "btn-outline-primary"),
    selectInput(ns("norm_reference"), "Stored reference to apply", choices = character()),
    uiOutput(ns("norm_reference_note")),
    DTOutput(ns("norm_reference_scores")),
    # The calibration transformation bootstrap on the Reliability
    # tab's request pattern: the method, the
    # level, the resampling unit, B and the seed, an estimate from a
    # measured rate before the run, the counts after it, and the reuse
    # of a completed request that matches.
    div(
      class = "d-flex align-items-center gap-2 mt-3 flex-wrap",
      sieve_boot_inputs(ns, B = 999, B_id = "norm_B", seed_id = "norm_seed", min_B = 99, max_B = NULL, step_B = 100, label = "Bootstrap resamples", width = "170px", seed_label = "Seed (optional)", seed_width = "140px"),
      numericInput(ns("norm_level"), "Confidence level",
        value = 0.95, min = 0.5, max = 0.999, step = 0.01, width = "160px"
      ),

      actionButton(ns("norm_transform_ci"),
        "Estimate calibration transformation uncertainty",
        class = "btn-outline-secondary"
      )
    ),
    uiOutput(ns("norm_transform_plan")),
    uiOutput(ns("norm_transform_note")),
    DTOutput(ns("norm_transform_limits")),
    uiOutput(ns("norm_transform_code")),
    downloadButton(ns("dl_reference"), "Download frozen reference (.rds)", class = "btn-outline-primary btn-sm"),
    fileInput(ns("norm_import"), "Import a frozen SIEVER reference (.rds)", accept = ".rds"),
    downloadButton(ns("dl_reference_scores"),
      "Download fixed-reference scores (.csv)",
      class = "btn-outline-primary btn-sm"
    ),
    div(
      class = "mt-2",
      downloadButton(ns("dl_scoring"), "Download scoring syntax (.R)",
        class = "btn-outline-primary btn-sm me-2"
      ),
      downloadButton(ns("dl_norms"), "Download sample reference table (.csv)",
        class = "btn-outline-primary btn-sm"
      )
    ),
    uiOutput(ns("scoring_code"))
  )
}

#' Finalize module server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveVal observe observeEvent req
#' @importFrom shiny renderUI updateSelectInput isolate div HTML eventReactive
#' @importFrom shiny withProgress tagList strong em tags
#' @importFrom DT renderDT datatable formatRound
#' @importFrom utils head tail
mod_finalize_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    longitudinal <- mod_longitudinal_server("longitudinal", state)

    # The tab's Estimator control: the study's setting (Ingest tab) unless
    # chosen otherwise here (sieve_estimator_wire()).
    estimator_choice <- sieve_estimator_wire(input, output, state)

    # The page's one status line: the
    # pipeline gate; or, when results of this page are from before an
    # edit or a change of their settings, which and what changed, with the
    # one Update that computes each of them again as its own button would.
    output$gate <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(sieve_gate(state))
      }

      page_update$line() %||% page_update$note()
    })

    sieve_finalize_context_wire(environment())

    sieve_finalize_cv_wire(environment())

    sieve_finalize_invariance_wire(environment())

    sieve_finalize_partial_wire(environment())

    sieve_finalize_impact_wire(environment())

    sieve_finalize_alignment_wire(environment())

    sieve_finalize_norms_wire(environment())

    # ---- One Update for the page -------------------------------------------
    # Every result of this page is kept with the inputs and the settings it
    # was computed from and withheld once they change (see current()). The
    # page's one Update computes again, in this order, each one that was
    # computed before and is now from before, exactly as its own button
    # would: the split, the ladder, its diagnosis, the anchor comparison,
    # dMACS, the ordinal impact, the alignment, and the longitudinal rung.
    # The partial invariance refit frees the parameters selected in the
    # diagnosis table, which the Update redraws, so it keeps its own button
    # (and so do dMACS and the ordinal impact while they read it).
    section_titles <- c(
      cv = "Calibration/validation split", inv = "Invariance ladder", diagnosis = "Invariance diagnosis",
      partial = "Partial invariance refit", anchor = "Anchor sensitivity", dmacs = "dMACS table",
      ordinal = "Ordinal expected-score impact", alignment = "Alignment"
    )
    section_buttons <- c(
      partial = "Refit freeing selected", dmacs = "Compute dMACS",
      ordinal = "Compute ordinal expected-score impact"
    )
    section_shown <- list(
      cv = cv_shown, inv = inv_shown, diagnosis = inv_diag, partial = inv_partial,
      anchor = anchor_shown, dmacs = dmacs_res, ordinal = ordinal_impact_shown, alignment = align_res
    )
    section_runs <- list(
      cv = run_split, inv = run_inv_ladder, diagnosis = run_diagnosis, anchor = run_anchor,
      dmacs = run_dmacs, ordinal = run_ordinal, alignment = run_align
    )
    # Why a kept section is from before, in a phrase.
    section_why <- function(section, record) {
      why <- sieve_stale_reason(record$revision, revision_now())
      if (!is.null(why)) {
        return(why)
      }
      if (!is.null(record$.sampling) && !identical(record$.sampling, sieve_sampling_spec(state))) {
        return("the sampling design changed")
      }
      sprintf("the settings of the %s changed", sieve_update_lower(section_titles[[section]]))
    }
    # The results the Update leaves to their own buttons: the partial
    # invariance refit, and dMACS and the ordinal impact while they read it.
    section_held <- function(section) {
      reads_partial <- section %in% c("dmacs", "ordinal") &&
        identical(input$dmacs_source %||% "partial", "partial") && !is.null(status_records()$partial)
      identical(section, "partial") || reads_partial
    }
    section_entry <- function(section) {
      record <- status_records()[[section]]
      if (is.null(record)) {
        return(NULL)
      }
      read <- sieve_outcome(section_shown[[section]]())
      shown <- if (read$ready && read$ok) read$value else NULL
      if (is.null(shown)) {
        return(NULL)
      }
      if (!isTRUE(shown$stale)) {
        return(sieve_update_entry(section_titles[[section]], if (is.null(shown$error)) "current" else "failed"))
      }
      held <- section_held(section)
      sieve_update_entry(
        section_titles[[section]], "stale",
        sentence = if (!held) section_why(section, record),
        when = sprintf("%s, computed %s", section_titles[[section]], record$when %||% "earlier"),
        seconds = record$seconds %||% NA_real_,
        run = section_runs[[section]], update = !held, shown = FALSE,
        button = if (held) section_buttons[[section]],
        why = if (identical(section, "partial")) {
          "after you select again, in the diagnosis table, the parameters to free"
        } else if (held) {
          "after the partial invariance refit"
        }
      )
    }
    page_update <- sieve_update_wire(input, session, function() {
      sections <- c("cv", "inv", "diagnosis", "anchor", "dmacs", "ordinal", "alignment", "partial")
      out <- lapply(sections, section_entry)
      names(out) <- sections
      out$longitudinal <- longitudinal$entry()
      out
    })

    # After an item is excluded or included, the page's Update recomputes
    # what it holds from before; with nothing requested here it stays quiet
    # (the notice "nothing is from before" is for a press of Update, not for
    # a decision made on another page).
    sieve_inclusion_wire(state, function() {
      if (!length(shiny::isolate(page_update$pending()))) return(NULL)
      page_update$run()
    })

    # ---- Show code at the point of use -------------------------------------
    # Each section writes its step to the reproducible code when it runs;
    # the disclosure under its result reads that step back
    # (sieve_code_disclosure()) under the section's own currency (the
    # result bound to its revision, request and sampling design). The
    # scoring syntax is itself the code; the sample reference table and
    # the transformation uncertainty read their recorded steps. Reading a
    # panel evaluates nothing.
    sieve_code_navigation(input, session, state)
    section_status <- function(r) {
      if (is.null(r)) {
        list(state = "not_run")
      } else if (isTRUE(r$stale)) {
        list(state = "stale", sentence = r$error)
      } else if (!is.null(r$error)) {
        list(state = "failed", sentence = r$error)
      } else {
        list(state = "current")
      }
    }
    output$cv_code <- renderUI({
      r <- tryCatch(cv_shown(), error = function(e) NULL)
      sieve_code_disclosure(state, "Finalize: calibration/validation split", status = section_status(r),
        ns = session$ns, title = "Show code: calibration and validation split",
        readout = c("cross_validation$fit  # the measurement model in each half", "cross_validation$reliability"))
    })
    output$inv_code <- renderUI({
      r <- tryCatch(inv_shown(), error = function(e) NULL)
      sieve_code_disclosure(state, "Measurement invariance across", status = section_status(r),
        ns = session$ns, title = "Show code: the invariance ladder")
    })
    output$partial_code <- renderUI({
      r <- tryCatch(inv_partial(), error = function(e) NULL)
      sieve_code_disclosure(state, "^Partial (metric|scalar|strict) invariance across", regex = TRUE,
        status = section_status(r), ns = session$ns, title = "Show code: the partial invariance refit")
    })
    output$align_code <- renderUI({
      r <- tryCatch(align_res(), error = function(e) NULL)
      sieve_code_disclosure(state, "Alignment of ", status = section_status(r),
        ns = session$ns, title = "Show code: alignment")
    })
    output$norms_code <- renderUI({
      sieve_code_disclosure(state, "Scoring and the sample reference table", ns = session$ns,
        title = "Show code: scores and the sample reference table")
    })
    output$scoring_code <- renderUI({
      state$meta
      state$scoring_policies
      code <- tryCatch(scoring_syntax(), error = function(e) NULL)
      sieve_code_view(
        if (length(code)) list(
          code = code,
          context = "The scoring syntax drafted from the ledger and each construct's scoring policy, as the download above writes it; it reads the raw values of the data file."
        ),
        if (length(code)) "current" else "not_run",
        ns = session$ns, title = "Show code: the scoring syntax"
      )
    })
    output$norm_transform_code <- renderUI({
      ref <- tryCatch(norm_selected(), error = function(e) NULL)
      r <- norm_limits()
      status <- if (is.null(r)) {
        list(state = "not_run")
      } else if (is.null(ref) || !identical(r$reference, ref$name) || !identical(r$created, ref$created)) {
        list(state = "stale", sentence = "What changed: another frozen reference is selected.")
      } else {
        list(state = "current")
      }
      sieve_code_disclosure(state, "Calibration transformation uncertainty", status = status, ns = session$ns,
        title = "Show code: transformation uncertainty of the calibration",
        readout = "norm_transform_limits  # percentile limits of the calibration mean, SD and quantiles")
    })
  })
}

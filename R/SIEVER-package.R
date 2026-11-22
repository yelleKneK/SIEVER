#' Scale Item Evaluation and Validation Environment in R (SIEVER)
#'
#' An interactive environment for developing constructs and instruments,
#' evaluating score interpretations and intended uses, and preserving the
#' evidence and researcher decisions behind them. Theory-informed and
#' evidence-supported psychometric decision making guides the work.
#' We do not automate psychometric judgment. We make psychometric judgment
#' more informed, transparent, reproducible, and defensible.
#' Launch the environment with [run_app()].
#'
#' @details
#' # What a Session Looks Like
#'
#' Start with a scientific project: construct meaning, intended score use,
#' item content, and evidence that could challenge the interpretation. Each tab
#' explains its own methodology as you use it. The core path is Ingest,
#' Screening, Dimensionality, EFA, Items, CFA, Reliability, Validity, and
#' CV and Invariance. With a theoretical structure the CFA tab can be
#' opened directly after Confirm; nothing on it waits for the
#' Dimensionality, EFA or Items tabs. The optional tabs, each saying so
#' when opened, are Content Validity, Optimizer, Bifactor, Network, IRT,
#' SEM, and Reproducible Code.
#'
#' The core path, in order:
#'
#' 1. **Ingest.** Load data (SPSS `.sav` files carry item wording in as
#'    variable labels), review the role proposed for every column, assign
#'    items to constructs, and confirm each item's measurement level.
#'    A bundled example whose items are all continuous is confirmed when it
#'    loads. A file of your own is confirmed with **Confirm the study**,
#'    after you review the roles and types proposed for its columns;
#'    ordered and binary items change the estimator (polychoric or
#'    tetrachoric correlations with WLSMV rather than ML). A project can
#'    be saved, copied, and opened from the project bar on every main tab.
#'    Browser saves prepare downloads and require acknowledgment of a kept copy.
#' 2. **Screening.** Descriptives with skewness and kurtosis, missingness,
#'    the type-appropriate correlation matrix, corrected item-total
#'    correlations, and careless-responding screens (longstring,
#'    within-person variability, Mahalanobis distance).
#' 3. **Dimensionality.** Parallel analysis and VSS as evidence
#'    about the number of factors (the decision is yours; no number is
#'    recommended).
#' 4. **EFA.** Maximum likelihood EFA with direct oblimin rotation,
#'    pattern and structure matrices, and simple structure metrics. The exact
#'    selected factor count is fitted here; comparisons across factor counts live
#'    in Dimensionality's Model Fit by K panel.
#' 5. **Items.** The winnowing workspace: live per-item diagnostics
#'    (loadings, cross loadings, omega if dropped) with items excluded by
#'    a click and never destroyed. Basic and Advanced views retain adjacent
#'    corrected construct and nested-facet item-rest correlations, with analytic
#'    sample counts and reasons when a correlation is undefined.
#' 6. **CFA.** The confirmatory measurement model with the RMSEA interval
#'    and a path diagram, and the Bollen-Stine bootstrap for the exact-fit
#'    test. Direct selected-facet models are separate from the higher-order
#'    construct-to-facet model.
#' 7. **Reliability.** Robust omega (observed denominator) per construct,
#'    facet, or both, with Omega (model-implied denominator) beside it and a
#'    bootstrap interval on request (for ordered items the denominator is built
#'    from the unrestricted polychoric correlations), alpha with its compatible
#'    interval, coefficient H, factor
#'    scores, and an AIPE planner
#'    for the sample size the next study needs.
#' 8. **Validity.** AVE beside each construct's largest correlation with
#'    another (the comparison of Fornell and Larcker, 1981, with no cutoff),
#'    HTMT with its one-sided upper bound, criterion validity read against
#'    the predictions you state, common method variance, and a
#'    reflective-versus-formative gate on each construct.
#' 9. **CV and Invariance.** An internal calibration/evaluation split, measurement
#'    invariance with a partial-invariance workflow when a rung fails,
#'    alignment for many groups, and scoring syntax with a sample-bound
#'    norm table. A late split does not erase prior exposure or establish
#'    independent validation.
#'
#' The optional tabs, in the order they sit among the others. Nothing in
#' another tab depends on them, a study that does not need them skips them,
#' and each opens with a sentence saying so and what it adds. **Content
#' Validity** (after Screening): expert ratings of item relevance and
#' representativeness, computed from a ratings file without respondent
#' data. **Optimizer** (after Items): a bounded search over candidate item
#' sets under constraints you set, reported as candidates with their
#' statistics, not as a verdict. It is available only under an empirical
#' assignment basis; a theoretical basis preserves the specified map. **Bifactor** (after Reliability):
#' essential-unidimensionality indices (ECV and omegaH with their
#' bootstrap intervals, and PUC, which the model's structure fixes).
#' **Network** (after Bifactor, a trial): psychometric network models in psychonetrics, the partial correlation network of the
#' items, the residual network beside the CFA (local dependence the factors
#' leave behind) and the latent network of the constructs, each with its
#' standard errors and read against the factor models. After CV and
#' Invariance, past a divider in the
#' navbar: **IRT**, the item response theory reading of the same
#' categorical measurement model for ordered and binary items (item
#' parameters, item information and test information); **SEM**, structural
#' relations among the constructs and onto outcomes, with its own path
#' diagram. The project bar's
#' scientific task assistant offers
#' reviewable local plans and optional provider review, with exact payload
#' disclosure, an explicit model, a session-only key, and authorization; **Reproducible Code**,
#' the annotated session script (every action along the way has been
#' recording the equivalent R), a methods-and-results draft that refits
#' its own model from the current data and says so, and the exports of the
#' study. Selected saved model and reliability results can be reported
#' without refitting. Definitions retain prior versions, frozen claims retain
#' their item/score revision, and evaluations are appended without rewriting
#' predictions. SIEVER does not provide full multistudy exposure-controlled
#' validation.
#'
#' # Where the Numbers Come From
#'
#' Estimation and inference come from the DMAR package (reliability with
#' confidence intervals, bifactor indices, HTMT, AVE, the common method
#' variance builders, ECVI, the AIPE planners, measurement invariance and
#' alignment, the EFA page's exploratory factor analysis and the
#' Dimensionality page's parallel analysis) and from lavaan for the latent
#' variable models, with the Items page's live exploratory model, Very
#' Simple Structure and the polychoric, tetrachoric and mixed correlations
#' through psych. The
#' environment resolves specifications, transformations, and uncertainty
#' metadata around those engines. Each logged step keeps its exact input
#' revision; a saved step that holds none cannot recover inputs that were
#' never saved. Every
#' fitted model carries a status (converged and admissible, improper,
#' not identified, not converged, or failed) and nothing is shown from a
#' model without a solution.
#'
#' # The Exported Functions
#'
#' [run_app()] starts the environment. The path diagrams of the CFA and
#' SEM tabs are drawn by SIEVER itself with base R graphics, from one
#' model-derived specification, and the same functions are exported for
#' plain R use: [sieve_diagram_spec()] builds the specification from a
#' fitted lavaan model, [sieve_diagram_update()] changes what it shows
#' without refitting, [sieve_draw_diagram()] draws it on the current
#' device (as does `plot()` on the specification), [sieve_export_diagram()]
#' writes it as PDF, SVG, or PNG, and [sieve_diagram_code()] writes the R
#' that regenerates it. [sieve_specification_template()] and
#' [sieve_prepare_study()] support preparation of portable studies.
#' [sieve_analyze()] runs an analysis without opening the application, and
#' the table of the scripted workflows article
#' (`vignette("scripted-workflows", package = "SIEVER")`) names each of the
#' other exported functions beside the workflow it serves;
#' `help(package = "SIEVER")` lists them all with their help pages. Twelve are marked experimental in their
#' help (the item response evidence, the longitudinal invariance functions,
#' [sieve_anchor_sensitivity()] and [sieve_ordinal_fit_impact()]): their
#' arguments and the form of their results may change in a later release
#' without a period of deprecation.
#'
#' # The Map of the Five Workspaces
#'
#' \if{html}{\figure{siever-map-workspaces.svg}{options: style="width:100\%;max-width:1600px" alt="How SIEVER works in the five workspaces: how to install and open it and the options you can set, the workspaces with their pages, the shared state and services, the records, and the outputs."}}
#' The map shows how to install and open SIEVER and the settings you can
#' choose, the five workspaces (Study, Structure, Scores, Evidence and
#' Report) with their pages, the shared study record, the services every
#' page goes through, the records every run writes, and what comes out. It
#' is drawn in the HTML help, the user guide and Getting Started articles
#' show it, and the file is
#' `system.file("help", "figures", "siever-map-workspaces.svg", package = "SIEVER")`,
#' the same SVG file the articles draw.
#'
#' # Learning More
#'
#' The user guide (`vignette("user-guide", package = "SIEVER")`) is the
#' page by page reference: every page, control, default, run rule and
#' coefficient, in the order a study uses them, with the methodology
#' explained at the point of use. The getting-started vignette
#' (`vignette("getting-started", package = "SIEVER")`) walks a complete
#' session on the bundled Holzinger and Swineford example and shows the R
#' each step records. The item-wording vignette covers attaching item text
#' so the wording-quality checks and construct review have something to
#' read, and the validation vignette compares the validity statistics
#' with an independent implementation. The standards crosswalk
#' (`vignette("standards-crosswalk", package = "SIEVER")`) maps each output
#' to the chapters on validity, reliability/precision and fairness of the
#' *Standards for Educational and Psychological Testing* (2014) and lists the
#' evidence those chapters call for that SIEVER does not provide.
#'
#' @keywords internal
#' @author Ken Kelley
#'
#' @import shiny
#' @importFrom bslib page_navbar nav_panel nav_spacer nav_item layout_sidebar
#'   sidebar navset_card_tab layout_columns value_box card card_header
#'   accordion accordion_panel bs_theme font_collection
#' @importFrom DT DTOutput renderDT datatable formatStyle formatRound
#'   styleEqual styleInterval
#' @importFrom golem with_golem_options add_resource_path activate_js favicon
#'   bundle_resources
#' @importFrom lavaan lavPredict
"_PACKAGE"

# lavaan is called with lavaan:: throughout; the one import above makes R
# load it with SIEVER and enforce the Imports floor (>= 0.7-3), which R
# checks only for a namespace that is imported (under lavaan 0.7-2 the
# factor scores of categorical indicators are wrong).

# The .data pronoun of the ggplot2 code (ggplot2 is imported and used with
# `::`; rlang, which defines the pronoun, is not imported for it).
utils::globalVariables(".data")

# The seven sieve_finalize_*_wire() functions deliberately evalq their
# panel setup into mod_finalize_server()'s shared module frame. Their
# functions/reactives resolve these names in that frame, across panel
# boundaries, rather than in the package namespace. input/session come
# from moduleServer, state from its enclosing server call; estimator_choice
# and section_held are in mod_finalize_server; the remaining bindings are
# created by the context/CV/invariance/partial/impact/alignment/norms wires.
# test-finalize-wiring-contract.R checks that the complete module creates
# these bindings and that its ordinary callbacks close over that frame.
# Keep this exact list specific to the proven evalq contract: new misspelled
# names remain visible to the package usage checker.
.siever_finalize_wire_symbols <- c(
  "align_res", "anchor_shown", "bind", "current", "cv_shown", "dmacs_res",
  "eligible_groups", "estimator_choice", "input", "inv_diag", "inv_partial", "inv_shown",
  "norm_limits", "norm_selected", "ordinal_impact_choices", "ordinal_impact_shown",
  "revision_now", "run_align", "run_anchor", "run_diagnosis", "run_dmacs",
  "run_inv_ladder", "run_ordinal", "run_split", "scoring_syntax", "section_held",
  "session", "state", "status_records", "time_section"
)
utils::globalVariables(.siever_finalize_wire_symbols)

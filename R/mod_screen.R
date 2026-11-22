# mod_screen.R
# The Screening tab: screening and descriptives. Before any factor model is
# fit, this tab gives the analyst the univariate and bivariate lay of the
# land on the active item pool: per item descriptives (with skewness and
# kurtosis), the pattern of missingness, the type appropriate inter item
# correlation matrix, and corrected item total correlations within each
# construct. The correlation matrix respects the confirmed measurement level
# of the items: Pearson for continuous, polychoric for ordered categorical
# (or ordered and binary), tetrachoric for binary, and mixed when continuous
# and categorical items mix, consistent with the house estimation defaults.

#' Screening and descriptives UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput plotOutput helpText p strong em div sliderInput selectInput downloadButton
#' @importFrom shiny textInput textAreaInput actionButton checkboxInput radioButtons conditionalPanel
#' @importFrom bslib layout_sidebar sidebar card card_body card_header navset_card_tab nav_panel
#' @importFrom DT DTOutput
mod_screen_ui <- function(id) {
  ns <- NS(id)
  tagList(

    uiOutput(ns("gate")),
    # How many items are shown, and that the other variables are summarized
    # below and not plotted.
    uiOutput(ns("pool_note")),
    # Results first: each sub-tab opens
    # on its table or figure, with its explanation below it (the first
    # paragraph visible, the remaining explanation open by default), and the tab's opening
    # paragraph follows the sub-tabs.
    navset_card_tab(
      id = ns("screen_tabs"),
      nav_panel(
        "Descriptives",
        card(
          card_header("Per item descriptive statistics on the active pool"),
          DTOutput(ns("desc_tbl")),
          helpText(
            class = "mt-2",
            "Univariate summaries for each active item. ", strong("Skewness"),
            " and ", strong("(excess) kurtosis"), " gauge departures from ",
            "normality, which matters because the normal-theory test statistic and ",
            "standard errors of maximum likelihood assume it. ",
            strong("% at floor"), " and ", strong("% at ceiling"),
            " are the proportions of responses sitting at the observed minimum and ",
            "maximum; large values signal floor or ceiling effects that compress ",
            "variance and attenuate correlations. ", strong("Distinct"),
            " is the number of different values observed."
          ),
          tags$details(open = NA,
            class = "small text-muted mb-2",
            tags$summary("More on reading this table: reference values and flags"),
            helpText(
              "Curran, West and Finch ",
              "(1996) simulated moderate (skewness 2, excess kurtosis 7) and severe (3 ",
              "and 21) non-normality: the ML chi-square was already inflated under the ",
              "moderate condition, while the Satorra-Bentler scaled statistic stayed ",
              "close to its expected value. ML with FIML, the default for continuous items ",
              "(DMAR's default), reports the normal-theory statistic and standard errors; ",
              "where these values show marked non-normality, MLR with FIML (the Estimator ",
              "control on the Ingest tab, step 4) uses a scaled statistic and robust standard ",
              "errors, and SIEVER does not switch to it on its own. For an item with few ",
              "response options, treating it as ordered is the alternative. ",
              "Items are flagged when |skewness| ",
              "exceeds 2 or |excess kurtosis| exceeds 7 (Curran et al.'s moderate ",
              "condition, a reference value, not a rule), for near-zero variance (SD ",
              "effectively 0), or for high ",
              "missingness (over 20%). Read the table as a triage list, not a ",
              "verdict: a flagged item is a candidate for revision or reverse ",
              "scoring, not an automatic exclusion. For a pool of 20 items ",
              "or fewer the response distribution of each item is drawn below the ",
              "table; for a larger pool the table is the per item view."
            )
          ),
          uiOutput(ns("desc_plot_ui"))
        )
      ),
      nav_panel(
        "Missingness",
        card(
          card_header("Percent missing per included item"),
          uiOutput(ns("missing_rule_note")),
          p("Missing Data Patterns and Pair Coverage", class = "fw-bold"),
          p("Patterns show which items are missing together. Coverage gives the number and proportion observed for each item pair. These describe the included items before a model is fitted."),
          DTOutput(ns("missing_patterns")), DTOutput(ns("missing_coverage")),
          # Sized like the heatmap, in a plain block that scrolls (see the
          # heatmap panel for why the plot is not a fill item of the card).
          div(
            style = "overflow: auto; max-height: 70vh; width: 100%; border: 1px solid #dee2e6; border-radius: 4px;",
            uiOutput(ns("miss_plot_ui"))
          ),
          helpText(
            class = "mt-2",
            "Per item percent missing on the included items. The ", em("amount"),
            " of missing data is only part of the story; the ", em("mechanism"),
            " matters more. Data that are missing completely at random (MCAR) bias ",
            "nothing, missing at random (MAR) can be handled by a principled ",
            "estimator, and missing not at random (MNAR, e.g. an item people skip ",
            "because of its content) needs a sensitivity analysis."
          ),
          tags$details(open = NA,
            class = "small text-muted mb-2",
            tags$summary("More on reading this figure"),
            helpText(
              "Missing-data treatment ",
              "is specific to the analysis: eligible continuous ML models can use FIML ",
              "under a suitable MAR model, while ordered or explicitly complete-case ",
              "analyses can use fewer observations. Read the actual method and analytic n ",
              "beside each result. Use this ",
              "view to spot items with disproportionate missingness, which may ",
              "indicate an ambiguous, sensitive, or poorly placed item. Items are in ",
              "the order of the heatmap (construct, then facet, with the same ",
              "separators), one bar per item at the height of a heatmap tile; the box ",
              "scrolls when the pool is larger than the pane."
            )
          )
        )
      ),
      nav_panel(
        "Correlation heatmap",
        card(
          card_header("Type appropriate inter item correlation matrix"),
          uiOutput(ns("cor_note")),
          uiOutput(ns("cor_singular")),
          div(
            class = "d-flex flex-wrap align-items-end gap-3 mb-2",
            # The scope: all items, one construct, or one facet, so a large
            # pool is read one block at a time. A subset of the cached matrix,
            # nothing is recomputed.
            selectInput(ns("cor_scope"), "Show", choices = c("All items" = "all"), width = "280px"),
            sliderInput(ns("cor_highlight"), "Highlight |r| at or above (recolors only)",
              min = 0, max = 1, value = 0, step = 0.05, width = "300px"
            ),
            # The coefficients in the tiles: on by default for 16 items or
            # fewer, off above, and the user's choice from then on.
            div(
              class = "mb-3",
              checkboxInput(ns("cor_values"), "Show the correlations in the cells", value = FALSE),
              # Shown when the items have facets: the within-construct and
              # within-facet blocks boxed, in the heatmap and the table.
              uiOutput(ns("cor_boxes_ui")),
              radioButtons(ns("cor_view"), NULL,
                inline = TRUE,
                choices = c("Heatmap" = "heatmap", "Correlation table" = "table"),
                selected = "heatmap"
              )
            ),
            # The figure as shown (its scope and highlight), at 300 dots per
            # inch or as a vector, and the matrix with its diagonal.
            div(
              class = "d-flex gap-2 mb-3",
              downloadButton(ns("dl_png"), "PNG (300 dpi)", class = "btn-outline-primary btn-sm"),
              downloadButton(ns("dl_pdf"), "PDF (vector)", class = "btn-outline-primary btn-sm"),
              downloadButton(ns("dl_csv"), "CSV (matrix)", class = "btn-outline-primary btn-sm")
            )
          ),
          uiOutput(ns("cor_scope_note")),
          # The matrix as a table: the scope applied, item names as the row
          # and column labels, rounded for display only.
          conditionalPanel(
            condition = "input.cor_view == 'table'", ns = ns,
            helpText(
              "The matrix as shown above (the scope applied), one row and one column per item, ",
              "rounded to three decimals for display; the CSV download holds full precision. ",
              "Click a column header to sort by that item's correlations; the search box finds items by name."
            ),
            DTOutput(ns("cor_table"))
          ),
          # The figure is drawn at a size that keeps every tile at least 14 px
          # on screen, so the plot grows with the item count. This box is a
          # plain block with its own height, outside the card's fill layout
          # (a fill item would be flexed to the pane), and it scrolls in both
          # directions when the matrix is larger than the pane.
          conditionalPanel(
            condition = "input.cor_view != 'table'", ns = ns,
            div(
              class = "sieve-heatmap-box",
              style = "overflow: auto; max-height: 70vh; width: 100%; border: 1px solid #dee2e6; border-radius: 4px;",
              uiOutput(ns("cor_plot_ui"))
            )
          ),
          # One pair at a time: the correlation of two items, by item name,
          # with the kind of correlation and the n it was computed on.
          div(
            class = "d-flex flex-wrap align-items-end gap-3 mt-2 mb-2",
            selectInput(ns("pair_a"), "Item", choices = character(0), width = "300px"),
            selectInput(ns("pair_b"), "with item", choices = character(0), width = "300px"),
            div(class = "mb-3", style = "max-width: 520px;", uiOutput(ns("pair_r")))
          ),
          helpText(
            "The inter item association matrix, with the coefficient chosen to ",
            "match the items' measurement level: ", strong("Pearson"),
            " for continuous items, the ", strong("polychoric"),
            " matrix when any item is ordered, and the ", strong("tetrachoric"),
            " matrix when all items are binary. ",
            "Reading the map: items are ordered by construct, then facet, with a ",
            "heavy line between constructs and a light line between facets, so a ",
            "coherent facet shows as a dark block on the diagonal."
          ),
          tags$details(open = NA,
            class = "small text-muted mb-2",
            tags$summary("More on reading this matrix"),
            helpText(
              "The reason for the coefficient is attenuation: the ",
              "Pearson correlation between two coarse or ordinal items systematically ",
              "underestimates the underlying association, whereas the ",
              "polychoric/tetrachoric correlation estimates the relationship between ",
              "the latent continua assumed to generate the ordered responses ",
              "(Olsson, 1979). This is the same matrix the later WLSMV models are fit ",
              "to, so screening and estimation see the same data. It is computed once ",
              "for a given data set and item set, the first tab that needs it pays for ",
              "it (about 20 seconds for 70 ordered items), and a label edit, a ",
              "display control, or a visit to another tab recomputes nothing. ",
              "A very high off ",
              "diagonal value can mean that two items are near redundant or ",
              "locally dependent (essentially asking the same thing); the panel above ",
              "the figure names the three largest, and SIEVER applies no cutoff. A ",
              "uniformly low average correlation suggests a weak or poorly related ",
              "item set that may not support a coherent factor. The highlight control ",
              "grays every tile below the chosen |r| and recomputes nothing."
            )
          ),
          card_header("Key to the axis labels"),
          helpText(
            "The axes show each item's name from the ledger, cut to 24 characters (three periods mark a cut). ",
            "This key gives the variable, the full item name, the construct and facet ",
            "(the blocks between the separators), and the wording, in the order drawn."
          ),
          DTOutput(ns("cor_key"))
        )
      ),
      nav_panel(
        "Item-total",
        # The card keeps its own height (fill = FALSE), so the body of the
        # sub-tab scrolls and the table is in full view. As a fill item of
        # the sub-tab it would be a second scrolling box inside the body of
        # the sub-tab (494 px inside 568 px at 1600 by 1000 px), and the
        # table, 504 px with its wrapped item names, would run past it and
        # hide its last row.
        card(
          fill = FALSE,
          card_header("Corrected item total correlation within construct"),
          uiOutput(ns("itc_note")),
          DTOutput(ns("itc_tbl")),
          helpText(
            class = "mt-2",
            "Click a column header to sort. Click rows to select them, give a reason, and press ",
            strong("Exclude the selected items"), " to exclude them from every analysis: the ",
            "change the Ingest grid's Use cell makes, so the items stay in the data and can be ",
            "included again there or below. The reason is kept in the ledger and the session script."
          ),
          div(
            class = "d-flex flex-wrap align-items-end gap-2",
            textInput(ns("itc_reason"), "Reason for excluding", width = "460px"),
            actionButton(ns("itc_exclude"), "Exclude the selected items",
              class = "btn-outline-danger btn-sm mb-3"
            )
          ),

          uiOutput(ns("itc_result")),
          helpText(
            class = "mt-2",
            "The ", strong("corrected"), " item total correlation: the ",
            strong("Pearson correlation"), " of each item's observed scores with the ",
            strong("mean"), " of the ", em("other"),
            " included items in its own construct (an observed score as well, so no ",
            "model is fit here), with the item itself excluded from ",
            "the total."
          ),
          tags$details(open = NA,
            class = "small text-muted mb-2",
            tags$summary("More on reading this table"),
            helpText(
              "Excluding the item removes the part whole inflation that ",
              "would otherwise make every item look like it belongs. The total is the ",
              "mean (not the sum) of the peer items, computed pairwise, so a case ",
              "that is missing some items still contributes; this is consistent with ",
              "averaging items when forming composites under missingness. A corrected ",
              "item total correlation below about .30 marks an item that does not ",
              "cohere with its intended construct and is a prime candidate for ",
              "revision, reverse scoring, or exclusion (Clark & Watson, 1995)."
            )
          )
        )
      ),
      nav_panel(
        "Observations",
        # The validation reservation (fct_holdout.R): optional, drawn before
        # the structure and the items are examined; the reserved rows leave
        # the analysis through the respondent path below, marked so that
        # Restore does not return them, and every development result reads
        # the calibration rows only.
        card(
          card_header("Validation reservation (optional)"),
          uiOutput(ns("holdout_status")),
          div(
            class = "d-flex flex-wrap align-items-end gap-2",
            shiny::numericInput(ns("holdout_proportion"), "Proportion of the rows to reserve",
              value = 0.3, min = 0.05, max = 0.95, step = 0.05, width = "240px"
            ),
            shiny::numericInput(ns("holdout_seed"), "Random seed (optional)", value = NULL, step = 1, width = "180px"),
            selectInput(ns("holdout_stratify"), "Stratify by a grouping variable (optional)",
              choices = c("None" = ""), width = "280px"
            ),
            actionButton(ns("holdout_reserve"), "Reserve", class = "btn-outline-primary btn-sm mb-3"),
            actionButton(ns("holdout_release"), "Release the reservation", class = "btn-outline-danger btn-sm mb-3")
          ),
          uiOutput(ns("holdout_sizes")),
          uiOutput(ns("holdout_result")),
          sieve_more_below(
            tagList(
              "A reservation sets aside a proportion of the respondents, drawn by a seed, before the ",
              "structure and the items are examined. The reserved rows leave every development result ",
              "(the shared correlation matrix, Dimensionality, EFA, Items, CFA, Reliability), which then ",
              "read the calibration rows only and say so beside their n. The ordinary Restore does not ",
              "return a reserved row; ", strong("Release the reservation"), " does, after which development ",
              "sees the reserved rows and the reservation's protection of an evaluation is gone. On the CV ",
              "and Invariance page, a named, frozen version of the measurement model is fit on the ",
              "calibration rows and evaluated on the reserved rows once. Never required: without a ",
              "reservation the calibration/validation split there remains available as an internal assessment."
            ),
            p(
              "A validation sample estimates how a model fitted and selected on one sample would fare ",
              "on another only when it played no part in fitting or selecting the model (Browne, 2000; ",
              "Cudeck & Browne, 1983): a split drawn after the full data were examined is a split of data ",
              "already examined. A reservation therefore protects an evaluation only when it is made ",
              "before structure and items are examined. One drawn after development results exist (a ",
              "Dimensionality, EFA, Items, CFA or Reliability result, or any recorded model fit; this ",
              "page's own descriptives, correlations and respondent screens are screening, not ",
              "development, and the shared correlation matrix is computed again from the calibration ",
              "rows) is allowed and recorded as reserved after development began, with the results that ",
              "preceded it listed, so a reader can judge the exposure. The Standards (AERA, APA, & NCME, 2014) ask that a ",
              "validity claim rest on evidence gathered for the interpretation proposed; the sample that ",
              "shaped the item set cannot supply, on its own, evidence that the item set generalizes ",
              "beyond it. Fitting a CFA on the CFA page does not make the model confirmatory; a model is ",
              "confirmed by data that did not shape it."
            ),
            p(
              "Sizes: the reserved count is round(n times the proportion), with or without a grouping ",
              "variable. With one, the count is allocated across its levels by largest remainder (Hamilton's ",
              "proportional allocation): each level first reserves the whole part of its quota (its n times ",
              "the proportion), and the rows that remain go one each to the levels with the largest ",
              "fractional parts, the seed choosing among levels whose fractional parts tie, so the total is ",
              "exact and each level is within one row of its quota (a missing group value forms a level of ",
              "its own). A reservation is refused, before any row is drawn, when a half would ",
              "have fewer than 20 rows (the rule the calibration/validation split applies) or when the ",
              "smaller half cannot carry the current model under the study's estimator. Rows incomplete on ",
              "the included items are set aside at a fit, so a half near 20 rows may be too small when it ",
              "is fit. The seed, the proportion, the grouping variable and the rows drawn are kept with the ",
              "project and in the session script, which draws the same rows from the same seed."
            ),
            label = "More on the reservation: what it protects, and the sizes"
          )
        ),
        card(
          card_header("Respondents in the analysis, and those removed"),
          uiOutput(ns("case_grid_note")),
          div(
            class = "d-flex flex-wrap align-items-end gap-2",
            textInput(ns("case_reason"), "Reason for removing or restoring", width = "380px"),
            actionButton(ns("case_exclude"), "Remove the selected respondents", class = "btn-outline-danger btn-sm mb-3"),
            actionButton(ns("case_include"),
              "Restore the selected respondents",
              class = "btn-outline-primary btn-sm mb-3"
            )
          ),
          DTOutput(ns("case_grid")),
          uiOutput(ns("case_grid_result")),
          shiny::selectizeInput(ns("case_responses"),
            "Raw response columns to inspect",
            choices = NULL,
            multiple = TRUE
          ),
          shiny::downloadButton(ns("dl_case_ledger"), "Download the respondents in the analysis and those removed (.csv)"),
          helpText(
            class = "mt-2",
            "One row per respondent of the data as loaded, including those removed from the analysis. ",
            "Set a respondent's status cell to Removed or In the analysis, or select rows and press ",
            "Remove the selected respondents or Restore the selected respondents. Original row is stable ",
            "even when identifiers are missing or duplicated. A removal changes every analysis and is ",
            "reversible here or in the Careless responding panel's Removed respondents table; the ",
            "source file is never changed. A removal asks for its reason when the reason box is empty."
          )
        )
      ),
      nav_panel(
        "Careless responding",
        card(
          card_header("Careless / insufficient-effort responding screens"),
          DTOutput(ns("careless_summary")),
          shiny::tags$details(
            class = "mt-2", shiny::tags$summary("Define the response-pattern screens"),
            helpText("Straightlining is operationalized as an exactly repeated raw response across consecutive selected items, before reverse scoring. The selected items follow their column order in the data file; this is a proxy for presentation order. Choose items with comparable response options. Missing responses break a run unless you explicitly choose to skip them. The rule is a review convention, not proof of insufficient effort."),
            shiny::selectizeInput(ns("screen_items"), "Items used for the screens", choices = NULL, multiple = TRUE),
            div(
              class = "d-flex flex-wrap align-items-end gap-2",
              shiny::numericInput(ns("screen_min_run"),
                "Run cutoff (0 = half the selected items, rounded up)",
                value = 0,
                min = 0,
                step = 1
              ),
              shiny::numericInput(ns("screen_min_answered"),
                "Minimum answered for response-pattern flags",
                value = 3,
                min = 2,
                step = 1
              ),
              selectInput(ns("screen_missing"),
                "A missing response",
                choices = c(
                  "Breaks the run" = "break",
                  "Is skipped (can join runs across missing items)" = "skip"
                )
              ),
              shiny::numericInput(ns("screen_irv"), "Within-person SD cutoff", value = 0, min = 0, step = .01),
              shiny::numericInput(ns("screen_md_alpha"),
                "Mahalanobis tail cutoff",
                value = .001,
                min = .0000001,
                max = .5,
                step = .001
              )
            ),
            actionButton(ns("screen_apply"), "Apply screen definition", class = "btn-primary btn-sm"),
            uiOutput(ns("screen_rule_status"))
          ),
          card_header("Flagged respondents (worst first)"),
          DTOutput(ns("careless_tbl")),
          uiOutput(ns("careless_detail")),
          shiny::downloadButton(ns("dl_flagged_cases"), "Download flagged respondents (.csv)"),
          helpText(
            class = "mt-2",
            "Every respondent flagged by at least one screen, most screens first, ",
            "then by the Mahalanobis tail probability. ", strong("Original row"),
            " is the respondent's row in the data as loaded (it does not change when ",
            "other respondents are removed); ", strong("Identifier"),
            " is the value of the data's identifier column, when it has one. ",
            strong("Screens flagged (of 3)"), " counts the screens the respondent is ",
            "flagged by and ", strong("Flagged by"), " names them: a respondent can ",
            "be flagged by more than one screen (a straight-liner, for one, is ",
            "usually flagged by Longstring and often by the within-person SD as ",
            "well). Click rows to select respondents to remove."
          ),
          card_header("Remove respondents from the analysis"),
          helpText(
            "Select rows in the table above, or type respondents in the box (or ",
            "both), write the reason, and press ", strong("Remove from the analysis"),
            ". The respondents leave the data every tab reads; the ledger's item ",
            "summaries are computed again from the remaining rows, and the step is ",
            "recorded in the session script with the reason as a comment."
          ),
          uiOutput(ns("remove_hint")),
          div(
            class = "d-flex flex-wrap align-items-end gap-2",
            textAreaInput(ns("remove_ids"), "Respondents to remove (identifiers or row numbers)",
              rows = 2, width = "360px"
            ),
            textAreaInput(ns("remove_reason"), "Reason (kept with the removal and in the session script)",
              rows = 2, width = "420px"
            ),
            actionButton(ns("remove_go"), "Remove from the analysis", class = "btn-outline-danger btn-sm mb-3")
          ),
          # Who the typed entries name, before anything is removed.
          uiOutput(ns("remove_preview")),
          uiOutput(ns("remove_result")),
          card_header("Removed respondents"),
          uiOutput(ns("removed_note")),
          DTOutput(ns("removed_tbl")),
          div(
            class = "mt-2",
            actionButton(ns("restore_go"), "Restore the selected", class = "btn-outline-primary btn-sm")
          ),
          uiOutput(ns("restore_result")),
          card_header("About these screens"),
          helpText(
            "Survey data routinely contain respondents who answer without reading: ",
            "straight-liners, pattern responders, and random responders. Left in, ",
            "they attenuate correlations, distort factor structure, and can ",
            "manufacture spurious method factors, so screening for them precedes ",
            "any psychometric modeling (Meade & Craig, 2012). Three standard ",
            "row-level screens are computed on the included items; each is defined ",
            "below with the cutoff in force here. Flags identify candidates for ",
            "review, not automatic deletion: inspect each flagged respondent, decide, ",
            "and document the decision. A respondent can be removed from the ",
            "analysis on this panel, with the reason written down: the row leaves ",
            "every analysis on every tab (the data file is not changed), the removal ",
            "and its reason are recorded in the session script and kept in the ",
            "project file, and a removed respondent can be restored to the analysis ",
            "at any time."
          ),
          shiny::tags$details(open = NA,
            class = "small mb-2",
            shiny::tags$summary("How each screen is defined, with the cutoffs in force"),
            uiOutput(ns("careless_note"))
          )
        )
      )
    ),
    # The tab's opening paragraph, below the results: its first sentence in
    # view as the summary of a disclosure that holds the rest, one line in
    # all. Below the sub-tabs the card takes its height from them, and in
    # full (or as two lines) it would leave the Item-total table's last row
    # under the fold at 1600 by 1000 px.
    card(
      class = "mt-2",
      fill = FALSE,
      card_body(
        class = "py-2",
        tags$details(
          class = "small mb-0 sieve-below-more",
          tags$summary(
            strong("Preliminary item analysis."),
            " Screening is the first quantitative look at the items, run ",
            em("before"), " dimensionality assessment, EFA, or any factor model."
          ),
          p(
            class = "small mb-0 mt-1",
            "The purpose is to inspect each item's distribution, the pattern of ",
            "missing data, the redundancy structure among items, and how each item ",
            "relates to the rest of its intended scale. Poorly behaved items found ",
            "here (extreme non-normality, floor or ceiling effects, near-zero ",
            "variance, heavy missingness, redundancy, or weak coherence) are best ",
            "diagnosed now, because they distort the correlation or covariance ",
            "matrix that every later factor model is fit to. Screening is ",
            "diagnostic, not confirmatory: it informs which items to retain, revise, ",
            "or exclude before structure is estimated."
          )
        )
      )
    )
  )
}

#' Screening and descriptives server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive req renderUI renderPlot div em strong tags tagList HTML isolate observe updateSelectInput downloadHandler
#' @importFrom shiny observeEvent reactiveVal outputOptions updateTextInput updateTextAreaInput showNotification
#' @importFrom shiny updateCheckboxInput
#' @importFrom DT renderDT datatable formatRound
mod_screen_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # The roles table that describes the loaded data: which column is an
    # item, an identifier, a group, a criterion or an auxiliary variable.
    # Derived when the state carries none, so every consumer below sees one.
    roles_tbl <- reactive({
      req(state$items)
      sieve_roles_table(state$raw, state$items, state$roles)
    })

    # The included items, read through the roles: the ledger rows with
    # status "active" whose column is in the item pool with the role item.
    # Never an identifier, group, criterion or auxiliary column. Ordered by
    # construct, then facet, in order of first appearance in the ledger,
    # with the ledger order kept within a facet, so the blocks of the
    # correlation heatmap are contiguous.
    included_items <- reactive({
      req(state$meta)
      m <- state$meta[state$meta$status == "active", , drop = FALSE]
      pool <- names(state$items) %||% character(0)
      r <- roles_tbl()
      if (is.data.frame(r)) {
        pool <- intersect(pool, r$variable[r$role == "item"])
      }
      sieve_screen_item_order(m[m$item %in% pool, , drop = FALSE])
    })

    # The numeric data of the included items, in plot order. The coercion
    # never dispatches on the column's class: an item read from an SPSS file
    # is a haven_labelled column, and as.numeric() on one fails once vctrs
    # is loaded unless haven is loaded too.
    active_data <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items)
      it <- included_items()
      req(length(it) > 0)
      d <- state$items[, it, drop = FALSE]
      d[] <- lapply(d, sieve_screen_numeric)
      d
    })

    # The ledger facts of the included items, in plot order: the variable,
    # its label (the researcher's item name), its wording, construct, facet
    # and confirmed type.
    item_info <- reactive({
      it <- included_items()
      m <- state$meta
      i <- match(it, m$item)
      label <- as.character(m$label[i])
      blank <- is.na(label) | !nzchar(label)
      label[blank] <- it[blank]
      data.frame(
        item = it,
        label = label,
        wording = if ("wording" %in% names(m)) as.character(m$wording[i]) else rep(NA_character_, length(it)),
        construct = as.character(m$construct[i]),
        facet = as.character(m$facet[i]),
        type = as.character(m$type[i]),
        stringsAsFactors = FALSE
      )
    })

    # The variables that are not items: summarized in one table, plotted
    # nowhere on this tab. An empty table when every column is an item or
    # the state holds no data as read.
    other_vars <- reactive({
      r <- roles_tbl()
      raw <- state$raw
      empty <- data.frame(
        Variable = character(0), Role = character(0), Type = character(0),
        n = integer(0), Missing = integer(0), Distinct = integer(0),
        stringsAsFactors = FALSE
      )
      if (!is.data.frame(r) || is.null(raw)) {
        return(empty)
      }
      o <- r[r$role != "item", , drop = FALSE]
      if (!nrow(o)) {
        return(empty)
      }
      vars <- as.character(o$variable)
      data.frame(
        Variable = vars,
        Role = as.character(o$role),
        Type = vapply(vars, function(v) sieve_screen_storage(raw[[v]]), character(1)),
        n = as.integer(nrow(raw) - o$n_missing),
        Missing = as.integer(o$n_missing),
        Distinct = as.integer(o$n_distinct),
        stringsAsFactors = FALSE
      )
    })

    output$gate <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(sieve_gate(state))
      }
      NULL
    })

    output$pool_note <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$meta)
      it <- included_items()
      n_pool <- nrow(state$meta)
      n_in <- length(it)
      n_out <- n_pool - n_in
      o <- other_vars()
      n_other <- nrow(o)
      shown <- sprintf(
        "Showing %d included item%s of the %d in the item pool%s.",
        n_in, if (n_in == 1L) "" else "s", n_pool,
        if (n_out > 0L) sprintf(" (%d excluded)", n_out) else ""
      )
      others <- if (n_other) {
        counts <- table(o$Role)
        said <- paste(
          sprintf(
            "%d %s", as.integer(counts),
            sieve_screen_role_words(names(counts), as.integer(counts))
          ),
          collapse = ", "
        )
        sprintf(
          paste0(
            "The %d other variable%s in the data (%s) %s summarized under ",
            "Other variables and %s not plotted."
          ),
          n_other, if (n_other == 1L) "" else "s", said,
          if (n_other == 1L) "is" else "are", if (n_other == 1L) "is" else "are"
        )
      } else {
        "Every column of the data is an item."
      }
      n_resp <- NROW(state$raw)
      n_removed <- length(state$removed_cases %||% list())
      # Where a removed respondent is restored: the Observations panel (its
      # status cell, or Restore the selected respondents) and the Careless
      # responding panel's Removed respondents table, which lists every
      # removed respondent whichever panel removed it.
      respondents <- if (n_removed) {
        sprintf(
          paste(
            "%d respondents are in the analysis; %d %s removed from it (restore %s on the Observations",
            "panel, with the status cell or Restore the selected respondents, or in the Removed",
            "respondents table of the Careless responding panel)."
          ),
          n_resp, n_removed, if (n_removed == 1L) "is" else "are", if (n_removed == 1L) "it" else "them"
        )
      } else {
        sprintf("All %d respondents are in the analysis.", n_resp)
      }
      div(class = "alert alert-light border py-2 small mb-2", strong(shown), " ", others, " ", respondents)
    })

    output$antecedent_note <- renderUI({
      o <- other_vars()
      ante <- o$Variable[o$Role == "antecedent"]
      if (!length(ante)) {
        return(NULL)
      }
      div(
        class = "small mb-2",
        strong(sprintf(
          "Antecedent%s (variables that exist before the target construct): ",
          if (length(ante) == 1L) "" else "s"
        )),
        paste(ante, collapse = ", "),
        ". Offered as observed predictors on the SEM tab; not offered as criteria on the Validity tab."
      )
    })

    output$other_tbl <- renderDT({
      o <- other_vars()
      if (!nrow(o)) {
        return(sieve_dt(
          data.frame(Note = "Every column of the data is an item; there is nothing to summarize here."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      sieve_dt(
        o,
        rownames = FALSE, selection = "none",
        options = list(pageLength = 25, dom = "tp", scrollX = TRUE),
        colnames = c("Variable", "Role", "Stored as", "n", "Missing", "Distinct values")
      )
    })

    # ---- 1. Descriptives -----------------------------------------------------

    desc <- reactive({
      d <- active_data()
      n_total <- nrow(d)
      base_row <- function(x) {
        ok <- x[!is.na(x)]
        n <- length(ok)
        mn <- if (n) min(ok) else NA_real_
        mx <- if (n) max(ok) else NA_real_
        list(
          n = n,
          mean = if (n) mean(ok) else NA_real_,
          sd = if (n > 1) stats::sd(ok) else NA_real_,
          median = if (n) stats::median(ok) else NA_real_,
          min = mn,
          max = mx,
          # Floor and ceiling: proportion of observed responses at the observed
          # extremes. Strong floor or ceiling effects compress variance and
          # attenuate correlations, so they are central to item screening.
          pct_floor = if (n) 100 * sum(ok == mn) / n else NA_real_,
          pct_ceiling = if (n) 100 * sum(ok == mx) / n else NA_real_,
          skew = sieve_screen_skew(ok),
          kurt = sieve_screen_kurt(ok),
          pct_missing = if (n_total) 100 * (n_total - n) / n_total else NA_real_,
          distinct = length(unique(ok))
        )
      }
      rows <- lapply(d, base_row)
      item_name <- state$meta$label[match(names(d), state$meta$item)]
      item_name[is.na(item_name)] <- names(d)[is.na(item_name)]
      skew_v <- vapply(rows, function(r) r$skew, numeric(1))
      kurt_v <- vapply(rows, function(r) r$kurt, numeric(1))
      sd_v <- vapply(rows, function(r) r$sd, numeric(1))
      miss_v <- vapply(rows, function(r) r$pct_missing, numeric(1))
      # Triage flags. Skewness and kurtosis beyond Curran, West and Finch's
      # (1996) moderately nonnormal condition (2 and 7; their severe one was
      # 3 and 21), near-zero variance, and high missingness.
      flag <- vapply(seq_along(rows), function(i) {
        fl <- character(0)
        if (!is.na(skew_v[i]) && abs(skew_v[i]) > 2) fl <- c(fl, "skew")
        if (!is.na(kurt_v[i]) && abs(kurt_v[i]) > 7) fl <- c(fl, "kurtosis")
        if (!is.na(sd_v[i]) && sd_v[i] < 1e-8) fl <- c(fl, "no variance")
        if (!is.na(miss_v[i]) && miss_v[i] > 20) fl <- c(fl, "missing")
        paste(fl, collapse = ", ")
      }, character(1))
      data.frame(
        Item = names(d),
        ItemName = item_name,
        n = vapply(rows, function(r) r$n, integer(1)),
        mean = vapply(rows, function(r) r$mean, numeric(1)),
        sd = sd_v,
        median = vapply(rows, function(r) r$median, numeric(1)),
        min = vapply(rows, function(r) r$min, numeric(1)),
        max = vapply(rows, function(r) r$max, numeric(1)),
        pct_floor = vapply(rows, function(r) r$pct_floor, numeric(1)),
        pct_ceiling = vapply(rows, function(r) r$pct_ceiling, numeric(1)),
        skewness = skew_v,
        kurtosis = kurt_v,
        pct_missing = miss_v,
        distinct = vapply(rows, function(r) r$distinct, integer(1)),
        Flag = flag,
        stringsAsFactors = FALSE
      )
    })

    output$desc_tbl <- renderDT({
      df <- desc()
      req(nrow(df) > 0)
      sieve_dt(
        df,
        rownames = FALSE, selection = "none",
        options = list(pageLength = 30, dom = "tp", scrollX = TRUE),
        colnames = c(
          "Variable Name", "Item Name", "n", "Mean", "SD", "Median", "Min", "Max",
          "% at floor", "% at ceiling", "Skewness", "Kurtosis", "% missing", "Distinct", "Flag"
        )
      ) |>
        formatRound(
          c(
            "mean", "sd", "median", "min", "max", "pct_floor", "pct_ceiling",
            "skewness", "kurtosis", "pct_missing"
          ),
          3
        )
    })

    # Small multiples of the response distributions, for a pool of 20 items
    # or fewer; above that the table is the per item view and nothing is
    # drawn, because 68 or 300 small plots cannot be read.
    multiples <- reactive({
      d <- active_data()
      n <- ncol(d)
      if (n > 20L) {
        return(NULL)
      }
      info <- item_info()
      info <- info[match(names(d), info$item), , drop = FALSE]
      list(
        data = d, labels = sieve_screen_short_label(info$label, 30L), types = info$type,
        layout = sieve_screen_multiples_layout(n)
      )
    })

    output$desc_plot_ui <- renderUI({
      n <- ncol(active_data())
      m <- multiples()
      if (is.null(m)) {
        return(div(
          class = "small text-muted mt-2",
          sprintf(paste0(
            "With %d included items the per item plots are replaced by the table ",
            "above (they are drawn for 20 items or fewer); the Missingness and ",
            "Correlation heatmap panels cover every item."
          ), n)
        ))
      }
      tagList(
        div(
          class = "small text-muted mt-3 mb-1",
          sprintf(paste0(
            "Response distribution of each of the %d included items: counts per ",
            "response category for ordered and binary items, a histogram for ",
            "continuous items."
          ), n)
        ),
        plotOutput(session$ns("desc_plot"), fill = FALSE, height = paste0(m$layout$height, "px"))
      )
    })

    output$desc_plot <- renderPlot(
      alt = "Distribution of each included item, one small panel per item. The descriptive statistics are in the table on this page.",
      {
        m <- multiples()
        req(m)
        sieve_screen_draw_multiples(m$data, m$labels, m$types, state$palette %||% "nd")
      },
      height = function() {
        m <- multiples()
        if (is.null(m)) 400L else m$layout$height
      },
      res = 96
    )

    missing_diagnostics <- reactive({
      req(state$items, state$meta)
      items <- intersect(state$meta$item[state$meta$status == "active"], names(state$items))
      req(length(items))
      sieve_missing_diagnostics(state$items, items)
    })
    output$missing_patterns <- DT::renderDT({
      table <- missing_diagnostics()$patterns
      req(table)
      sieve_dt(as.data.frame(table), rownames = FALSE, options = list(pageLength = 10))
    })
    output$missing_coverage <- DT::renderDT({
      table <- missing_diagnostics()$coverage
      req(table)
      sieve_dt(as.data.frame(table), rownames = FALSE, options = list(pageLength = 10))
    })

    # ---- 2. Missingness ------------------------------------------------------

    miss <- reactive({
      d <- active_data()
      n_total <- nrow(d)
      pct <- vapply(d, function(x) {
        if (!n_total) return(NA_real_)
        100 * sum(is.na(x)) / n_total
      }, numeric(1))
      data.frame(item = names(d), pct_missing = unname(pct), stringsAsFactors = FALSE)
    })

    # Sized like the heatmap of every included item: one bar per item at the
    # height of a tile, so the two figures line up and a large pool scrolls
    # rather than being squeezed.
    miss_height <- reactive({
      sieve_screen_heatmap_size(length(included_items()))$height
    })

    output$miss_plot_ui <- renderUI({
      plotOutput(session$ns("miss_plot"),
        fill = FALSE, width = "100%",
        height = paste0(miss_height(), "px")
      )
    })

    output$miss_plot <- renderPlot(
      alt = "Missing responses of each item. The counts are in the missing data pattern and coverage tables on this page.",
      {
        df <- miss()
        req(nrow(df) > 0)
        info <- item_info()
        info <- info[match(df$item, info$item), , drop = FALSE]
        sieve_screen_draw_missing(
          df$pct_missing, sieve_screen_short_label(info$label),
          info[, c("construct", "facet"), drop = FALSE],
          state$palette %||% "nd"
        )
      },
      height = function() miss_height(),
      res = 96
    )

    # ---- 3. Correlation heatmap ---------------------------------------------

    # The shared correlation basis: the type appropriate matrix of the
    # included items (Pearson for continuous, polychoric for ordered,
    # tetrachoric for binary, mixed when the confirmed types mix), computed
    # once per data and item set and kept in the state, so the first tab
    # that needs it pays for it and every later read is free. The ledger and
    # the data are read here so that this reactive is evaluated again when
    # they change; a read that finds the matrix in the cache costs nothing.
    # Under the run policy (sieve_basis_demand())
    # a polychoric, tetrachoric or mixed matrix estimated above two seconds
    # waits for Compute the correlations (basis_go, in the matrix note),
    # once per item pool; the record it returns meanwhile says so.
    kept <- new.env(parent = emptyenv())
    basis_asked <- reactiveVal(NULL)
    observeEvent(input$basis_go, {
      b <- tryCatch(isolate(basis()), error = function(e) NULL)
      if (isTRUE(b$waiting)) basis_asked(b$key)
    })
    basis <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      it <- included_items()
      req(length(it) >= 2L)
      b <- sieve_basis_demand(state, asked = basis_asked())
      req(b)
      if (isTRUE(b$waiting)) {
        return(b)
      }
      # Recorded once per matrix: the cache returns the same matrix on every
      # later read (marked cached, so the object differs by that flag), so a
      # label edit or a display control records nothing.
      sig <- list(key = b$key, R = b$R, used = b$used)
      if (!identical(kept$logged, sig)) {
        sieve_log(
          state,
          sprintf(
            paste0(
              "Screening: correlations among the %d included items (requested %s, used %s); ",
              "complete cases, n = %d of %d."
            ),
            b$p, sieve_retention_basis_label(b$requested), sieve_retention_basis_label(b$used),
            b$n, nrow(isolate(state$items))
          ),
          sieve_retention_cor_code(b),
          passive = TRUE
        )
        kept$logged <- sig
      }
      b
    })

    # The matrix as shown: the basis, in plot order (construct, then facet),
    # with what was requested, what is shown, and the engine's reason when
    # they differ. The note names the matrix that is shown, never one that
    # failed.
    cor_mat <- reactive({
      b <- basis()
      it <- included_items()
      n_all <- nrow(state$items)
      if (!isTRUE(b$ok) || !is.matrix(b$R)) {
        return(list(
          R = NULL, kind = NA_character_, requested = b$requested,
          reason = b$reason %||% NA_character_, n = b$n, n_all = n_all, ok = FALSE,
          waiting = isTRUE(b$waiting), action = b$action
        ))
      }
      it <- it[it %in% rownames(b$R)]
      list(
        R = b$R[it, it, drop = FALSE], kind = b$used, requested = b$requested,
        reason = if (is.null(b$reason)) NA_character_ else b$reason,
        n = b$n, n_all = n_all, ok = TRUE
      )
    })

    output$cor_note <- renderUI({
      cm <- cor_mat()
      req(cm)
      # The matrix waits for its button (the run policy): the line says
      # what, how long, and what to press.
      if (isTRUE(cm$waiting)) {
        return(div(
          class = "alert alert-info py-2 small",
          tags$b("The correlations have not been computed. "), cm$reason, " ",
          div(class = "mt-2", actionButton(session$ns("basis_go"), cm$action, class = "btn-primary btn-sm"))
        ))
      }
      if (!isTRUE(cm$ok)) {
        return(div(
          class = "alert alert-warning py-2 small",
          tags$b("No correlation matrix could be computed. "), cm$reason
        ))
      }
      # The statistic is named as it is used: Pearson on the observed item
      # scores, or the polychoric or tetrachoric estimate of the latent
      # correlation behind the ordered responses, on the complete cases.
      shown <- sprintf(
        "Matrix shown: %s correlations among %d active items, %s Complete cases: n = %d of %d.",
        if (cm$kind == "pearson") "Pearson" else cm$kind, ncol(cm$R),
        if (cm$kind == "pearson") "on the observed item scores."
        else "estimated from the ordered responses (psych).",
        cm$n, cm$n_all
      )
      if (identical(cm$requested, cm$kind)) {
        return(div(class = "small text-muted", shown))
      }
      # A change of matrix is an alert, not a footnote.
      div(
        class = "alert alert-warning py-2 small",
        tags$b(sprintf(
          "Requested: %s correlations. Used: Pearson correlations. ",
          sieve_retention_basis_label(cm$requested)
        )),
        sub("[.]?$", ". ", trimws(cm$reason)),
        paste0(
          "The Pearson correlations of ordered or binary items are attenuated when ",
          "categories are few or the response distributions are skewed, so this map ",
          "can understate the associations the later WLSMV models estimate. "
        ),
        shown
      )
    })

    # Conditioning / multicollinearity callout: a near-singular matrix (a tiny
    # determinant or a near-zero / negative smallest eigenvalue) signals that
    # one item is nearly a linear combination of others, which destabilizes
    # factor extraction. We also list the three largest correlations between two
    # items, without a cutoff.
    output$cor_singular <- renderUI({
      cm <- cor_mat()
      req(cm, isTRUE(cm$ok), !is.null(cm$R))
      R <- cm$R
      ev <- tryCatch(eigen(R, symmetric = TRUE, only.values = TRUE)$values,
        error = function(e) NA_real_
      )
      det_R <- tryCatch(prod(ev), error = function(e) NA_real_)
      min_ev <- if (all(is.finite(ev))) min(ev) else NA_real_
      # The largest correlations between two items, by absolute value,
      # named without a cutoff (SIEVER applies no cutoff in output).
      idx <- which(upper.tri(R) & is.finite(R), arr.ind = TRUE)
      pairs <- if (nrow(idx)) {
        ord <- order(-abs(R[idx]))
        idx <- idx[ord, , drop = FALSE]
        utils::head(sprintf(
          "%s &harr; %s (r = %.2f)",
          rownames(R)[idx[, 1]], colnames(R)[idx[, 2]],
          R[idx]
        ), 3)
      } else character(0)
      near_singular <- isTRUE(is.finite(det_R) && det_R < 1e-5) ||
        isTRUE(is.finite(min_ev) && min_ev < 1e-6)
      # An item and its reversed copy, both in the file, read as the Ingest
      # tab's item grid reads them (Pearson correlations of the included
      # items, sieve_item_copies()), whatever matrix this page shows.
      copies <- sieve_item_copies(isolate(state$items), isolate(state$meta))
      copies <- copies[copies$r < 0, , drop = FALSE]
      if (!near_singular && !length(pairs) && !nrow(copies)) {
        return(NULL)
      }
      tagList(
        if (near_singular) div(
          class = "alert alert-warning py-2 small mb-2",
          strong("The correlation matrix is near singular "),
          sprintf("(determinant = %.2g, smallest eigenvalue = %.3f). ", det_R, min_ev),
          "One or more items are nearly redundant with the rest, which can make factor ",
          "extraction unstable or improper. Consider dropping or combining a near-duplicate item."
        ),
        if (length(pairs)) div(
          class = "small text-muted mb-2",
          strong("The largest correlations between two items: "),
          HTML(paste(pairs, collapse = "; "))
        ),
        if (nrow(copies)) div(
          class = "small text-warning-emphasis mb-2",
          strong("An item and its reversed copy? "),
          sprintf(
            "%s correlate near -1 (Pearson). Such a pair is almost certainly one item twice, the original and its reversed version both in the file, and only one belongs in the analysis: the one keyed with its construct. %s",
            paste(sprintf("%s and %s", copies$a, copies$b), collapse = "; "),
            if (any(!is.na(copies$keep))) {
              "The item grid on the Ingest tab names it and excludes the other in one click."
            } else {
              "The item grid on the Ingest tab names it once the construct is assigned."
            }
          )
        )
      )
    })

    # The scope of the figure: all items, one construct, or one facet. The
    # choices follow the ledger; a choice that no longer exists falls back
    # to all items.
    observe({
      info <- item_info()
      choices <- c("All items" = "all")
      cons <- unique(info$construct[!is.na(info$construct) & nzchar(info$construct)])
      if (length(cons) > 1L) {
        choices <- c(choices, stats::setNames(paste0("construct:", cons), paste("Construct:", cons)))
      }
      facs <- unique(info$facet[!is.na(info$facet) & nzchar(info$facet)])
      if (length(facs)) {
        choices <- c(choices, stats::setNames(paste0("facet:", facs), paste("Facet:", facs)))
      }
      current <- isolate(input$cor_scope)
      selected <- if (!is.null(current) && current %in% choices) current else "all"
      updateSelectInput(session, "cor_scope", choices = choices, selected = selected)
    })

    heat_scope <- reactive({
      s <- input$cor_scope
      if (is.null(s) || !nzchar(s) || identical(s, "all")) {
        return(list(kind = "all", name = NA_character_))
      }
      kind <- sub(":.*$", "", s)
      if (!kind %in% c("construct", "facet")) {
        return(list(kind = "all", name = NA_character_))
      }
      list(kind = kind, name = sub("^[^:]*:", "", s))
    })

    # The items the figure shows, in plot order.
    heat_items <- reactive({
      info <- item_info()
      sc <- heat_scope()
      it <- info$item
      if (identical(sc$kind, "construct")) {
        it <- info$item[!is.na(info$construct) & info$construct == sc$name]
      } else if (identical(sc$kind, "facet")) {
        it <- info$item[!is.na(info$facet) & info$facet == sc$name]
      }
      if (length(it)) it else info$item
    })

    output$cor_scope_note <- renderUI({
      sc <- heat_scope()
      n_shown <- length(heat_items())
      n_all <- length(included_items())
      text <- if (identical(sc$kind, "all") || n_shown == n_all) {
        sprintf("Showing all %d included items.", n_all)
      } else {
        sprintf(
          "Showing the %d item%s of %s %s (of %d included).", n_shown,
          if (n_shown == 1L) "" else "s", sc$kind, sc$name, n_all
        )
      }
      values <- if (isTRUE(heat_values())) {
        if (n_shown > 30L) {
          sprintf(
            paste(
              " The correlations are printed in the cells, which are drawn at %d px for them,",
              "so the figure of %d items is large and scrolls; the printed values are small",
              "next to the whole matrix. Choose one construct or facet with Show to read a",
              "block on one screen, or use the Correlation table."
            ),
            as.integer(round(heat_size()$cell)), n_shown
          )
        } else {
          " The correlations are printed in the cells, to two decimals."
        }
      } else {
        ""
      }
      div(class = "small text-muted mb-1", paste0(text, values))
    })

    # Whether the coefficients are printed in the tiles: the checkbox, which
    # is set on for a pool of 16 items or fewer and off above when a dataset
    # arrives (and is the user's choice from then on); before the browser
    # reports it, the same rule.
    observe({
      n <- length(included_items())
      key <- state$dataset_id %||% ""
      if (n > 0L && !identical(kept$values_for, key)) {
        kept$values_for <- key
        updateCheckboxInput(session, "cor_values", value = n <= 16L)
      }
    })
    heat_values <- reactive({
      v <- input$cor_values
      if (is.null(v)) length(included_items()) <= 16L else isTRUE(v)
    })

    # The size of the figure follows the number of items shown so that a
    # tile is never smaller than 14 px on screen, or 28 px when the
    # correlations are printed in it (see sieve_screen_heatmap_size); the
    # box around the plot scrolls when the figure is larger than the pane.
    heat_size <- reactive({
      sieve_screen_heatmap_size(length(heat_items()), values = heat_values())
    })

    output$cor_plot_ui <- renderUI({
      s <- heat_size()
      plotOutput(session$ns("cor_plot"),
        fill = FALSE,
        width = paste0(s$width, "px"), height = paste0(s$height, "px")
      )
    })

    # What the heatmap is drawn from: the matrix of the items shown, in plot
    # order, the labels and the groups from the ledger, and the highlight
    # value. The draw is the exported sieve_correlation_heatmap(), which the
    # recorded R calls on the same matrix, so a download and the screen show
    # one figure.
    # The blocks can be boxed when the items shown have facets: each
    # construct's correlations among its own items, and each facet's among its
    # own, in the heatmap and in the table.
    heat_has_facets <- reactive({
      info <- item_info()
      facet <- info$facet[match(heat_items(), info$item)]
      any(!is.na(facet) & nzchar(trimws(facet)))
    })
    output$cor_boxes_ui <- renderUI({
      if (!isTRUE(tryCatch(heat_has_facets(), error = function(e) FALSE))) {
        return(NULL)
      }
      checkboxInput(session$ns("cor_boxes"), "Box the within-construct and within-facet correlations",
        value = isTRUE(isolate(input$cor_boxes))
      )
    })

    heat_args <- reactive({
      cm <- cor_mat()
      req(isTRUE(cm$ok))
      shown <- heat_items()
      shown <- shown[shown %in% rownames(cm$R)]
      req(length(shown) >= 1L)
      info <- item_info()
      info <- info[match(shown, info$item), , drop = FALSE]
      list(
        R = cm$R[shown, shown, drop = FALSE],
        labels = stats::setNames(info$label, info$item),
        groups = info[, c("construct", "facet"), drop = FALSE],
        highlight = min(1, max(0, sieve_finite_or(input$cor_highlight, 0))),
        palette = state$palette %||% "nd",
        values = heat_values(),
        boxes = isTRUE(input$cor_boxes) && isTRUE(heat_has_facets()),
        scope = heat_scope()
      )
    })

    output$cor_plot <- renderPlot(
      alt = "Heatmap of the correlations among the included items. The correlations are in the correlation table on this page.",
      {
        a <- heat_args()
        sieve_correlation_heatmap(a$R,
          labels = a$labels, groups = a$groups,
          highlight = a$highlight, palette = a$palette, values = a$values, boxes = a$boxes
        )
      },
      width = function() heat_size()$width,
      height = function() heat_size()$height,
      res = 96
    )

    # The pair lookup: the choices are the included items by item name, in
    # plot order; a choice that no longer exists falls back to the first
    # (or second) item.
    observe({
      info <- item_info()
      choices <- stats::setNames(info$item, sprintf("%s (%s)", info$label, info$item))
      a <- isolate(input$pair_a)
      b <- isolate(input$pair_b)
      a <- if (!is.null(a) && a %in% info$item) a else info$item[1]
      b <- if (!is.null(b) && b %in% info$item) b else info$item[min(2L, length(info$item))]
      updateSelectInput(session, "pair_a", choices = choices, selected = a)
      updateSelectInput(session, "pair_b", choices = choices, selected = b)
    })

    pair <- reactive({
      cm <- cor_mat()
      a <- input$pair_a
      b <- input$pair_b
      req(isTRUE(cm$ok), length(a) == 1L, length(b) == 1L, nzchar(a), nzchar(b))
      sieve_screen_pair(cm, a, b, item_info())
    })

    output$pair_r <- renderUI({
      p <- tryCatch(pair(), error = function(e) NULL)
      if (is.null(p)) {
        return(div(class = "small text-muted", "Choose two items to see their correlation."))
      }
      div(class = "small", p$text)
    })

    # The matrix as shown (the scope applied) as a table: the item names in
    # the first column and as the column labels, the coefficients at full
    # precision (the table rounds them for display).
    cor_table_df <- reactive({
      a <- heat_args()
      info <- item_info()
      items <- rownames(a$R)
      lab <- info$label[match(items, info$item)]
      df <- data.frame(Item = lab, unname(a$R), check.names = FALSE, stringsAsFactors = FALSE)
      names(df) <- c("Item", items)
      rownames(df) <- NULL
      attr(df, "labels") <- lab
      df
    })

    output$cor_table <- renderDT({
      df <- cor_table_df()
      lab <- attr(df, "labels")
      a <- heat_args()
      options <- list(pageLength = 25, dom = "ftp", scrollX = TRUE, ordering = TRUE)
      # The same blocks as the heatmap's boxes, drawn on the rows as they
      # are displayed, so a sorted or paged table keeps them right.
      if (isTRUE(a$boxes)) options$drawCallback <- sieve_screen_table_boxes(a$groups)
      sieve_dt(
        df,
        rownames = FALSE, selection = "none",
        colnames = c("Item", sieve_screen_short_label(lab)),
        options = options
      ) |>
        formatRound(seq_along(lab) + 1L, 3)
    })

    output$cor_key <- renderDT({
      info <- item_info()
      req(nrow(info) > 0)
      df <- data.frame(
        Label = sieve_screen_short_label(info$label),
        Item = info$label,
        Variable = info$item,
        Construct = info$construct,
        Facet = info$facet,
        Wording = info$wording,
        stringsAsFactors = FALSE
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(pageLength = 10, dom = "ftp", scrollX = TRUE),
        colnames = c("Axis label", "Item name", "Variable", "Construct", "Facet", "Wording")
      )
    })
    # ---- Downloads: the figure as shown, and the matrix --------------------
    # Each file is written from heat_args(), the same arguments the screen
    # draws from, so a download is the figure on the screen: its scope, its
    # highlight, its labels and separators. The file name carries the
    # correlations used and, when the figure is scoped to one construct or
    # facet, that scope. The recorded R writes the same file from the
    # matrix the basis step records (R_items) and the ledger (meta).

    heat_file <- function(ext) {
      function() {
        a <- heat_args()
        cm <- cor_mat()
        sieve_screen_file_name(cm$kind, a$scope, ext)
      }
    }

    write_heat <- function(format, file) {
      a <- heat_args()
      s <- heat_size()
      cm <- cor_mat()
      name <- sieve_screen_file_name(cm$kind, a$scope, format)
      sieve_screen_open_device(format, file, s)
      on.exit(grDevices::dev.off(), add = TRUE)
      drawn <- sieve_correlation_heatmap(a$R,
        labels = a$labels, groups = a$groups,
        highlight = a$highlight, palette = a$palette, values = a$values, boxes = a$boxes
      )
      sieve_log(
        state,
        sprintf(
          "Screening: the correlation heatmap as shown (%d items) written as %s, %s.",
          nrow(a$R), toupper(format), name
        ),
        sieve_screen_heatmap_code(a, s, name, format)
      )
      invisible(drawn)
    }

    output$dl_png <- downloadHandler(
      filename = heat_file("png"),
      content = function(file) write_heat("png", file)
    )

    output$dl_pdf <- downloadHandler(
      filename = heat_file("pdf"),
      content = function(file) write_heat("pdf", file)
    )

    output$dl_csv <- downloadHandler(
      filename = heat_file("csv"),
      content = function(file) {
        a <- heat_args()
        cm <- cor_mat()
        name <- sieve_screen_file_name(cm$kind, a$scope, "csv")
        header <- sieve_screen_csv_header(cm, a)
        con <- file(file, open = "w", encoding = "UTF-8")
        on.exit(close(con), add = TRUE)
        writeLines(header, con)
        utils::write.csv(a$R, con)
        sieve_log(
          state,
          sprintf(
            "Screening: the correlation matrix as shown (%d items, diagonal included) written as CSV, %s.",
            nrow(a$R), name
          ),
          sieve_screen_csv_code(a, name, header)
        )
      }
    )
    # ---- 4. Item-total -------------------------------------------------------

    # The corrected item total correlation of every included item, computed
    # by sieve_screen_item_total() within construct (the items without a
    # construct are totaled together). A few correlations, no model, so the
    # table is computed whether or not its panel is open.
    itc <- reactive({
      d <- active_data()
      req(state$meta, ncol(d) >= 1)
      m <- state$meta
      it <- names(d)
      sieve_screen_item_total(
        d,
        construct = m$construct[match(it, m$item)],
        labels = m$label[match(it, m$item)],
        low = state$thresholds$item_total_low %||% 0.30
      )
    })

    output$itc_note <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed) || is.null(state$meta)) {
        return(div(
          class = "alert alert-info py-2 small",
          "Nothing is computed here until the data are loaded and the study is confirmed on the Ingest tab."
        ))
      }
      if (!length(included_items())) {
        return(div(
          class = "alert alert-warning py-2 small",
          "No item is included, so there is no item total correlation to compute. Include items on the Ingest tab."
        ))
      }
      df <- tryCatch(itc(), error = function(e) e)
      if (inherits(df, "shiny.silent.error")) {
        return(NULL)
      }
      if (inherits(df, "error")) {
        return(div(
          class = "alert alert-warning py-2 small",
          tags$b("The item total correlations could not be computed: "), conditionMessage(df)
        ))
      }
      div(class = "alert alert-light border py-2 small mb-2", sieve_screen_item_total_note(df))
    })

    output$itc_tbl <- renderDT(
      {
        df <- itc()
        req(nrow(df) > 0)
        shown <- data.frame(
          Item = df$item, ItemName = df$label, Construct = df$construct_shown,
          n_peers = df$n_peers, n = df$n, item_total = df$item_total,
          Flag = df$flag, Note = df$note, stringsAsFactors = FALSE
        )
        # The Note column says why a correlation is not defined; it is shown
        # when one is not.
        with_note <- any(nzchar(df$note))
        if (!with_note) {
          shown$Note <- NULL
        }
        sieve_dt(
          shown,
          rownames = FALSE, selection = list(mode = "multiple", target = "row"),
          options = list(
            pageLength = 30, dom = "ftp", scrollX = TRUE, stateSave = TRUE,
            stateSaveCallback = sieve_dt_state_save(), stateLoadCallback = sieve_dt_state_load(),
            sieve = list(
              id = "screen-item-rest", keys = names(shown),
              widths = list(
                Item = 230, ItemName = 250, Construct = 130,
                n_peers = 120, n = 65, item_total = 110, Flag = 90
              )
            )
          ),
          colnames = c(
            "Variable Name", "Item Name", "Construct", "Other items in construct",
            "n", "r(item-total)", "Flag", if (with_note) "Note"
          )
        ) |>
          formatRound("item_total", 3)
      },
      server = FALSE
    )
    # The table is ready when the panel is opened: it is computed while the
    # panel is hidden, and its rows travel with it (server = FALSE, a table
    # of one row per item), so it is drawn even while the session computes
    # something long, such as the polychoric matrix of the heatmap.
    outputOptions(output, "itc_tbl", suspendWhenHidden = FALSE)
    outputOptions(output, "itc_note", suspendWhenHidden = FALSE)

    itc_result <- reactiveVal(NULL)
    sieve_coverage_navigation(input, session)
    output$itc_result <- renderUI({
      r <- itc_result()
      if (is.null(r)) {
        return(NULL)
      }
      tagList(
        div(
          class = paste("alert py-2 small", if (identical(r$kind, "error")) "alert-warning" else "alert-success"),
          r$text
        ),
        # The content coverage advisory beside the decision, when the
        # exclusion leaves a construct or a facet
        # with no included item.
        r$advisory
      )
    })

    # Exclude the selected items: the ledger status every tab reads, through
    # sieve_exclude_items(), with the reason. At least two items stay
    # included, as on the Ingest tab.
    observeEvent(input$itc_exclude, {
      df <- isolate(itc())
      sel <- input$itc_tbl_rows_selected
      picked <- if (length(sel)) df$item[sel[sel >= 1L & sel <= nrow(df)]] else character(0)
      if (!length(picked)) {
        itc_result(list(
          kind = "error",
          text = "Select one or more rows of the table first (click a row to select it)."
        ))
        return(invisible(NULL))
      }
      m <- isolate(state$meta)
      left <- sum(m$status == "active" & !m$item %in% picked)
      if (left < 2L) {
        itc_result(list(kind = "error", text = sprintf(
          "Excluding %d item%s would leave %d included; at least two must stay included. Nothing was excluded.",
          length(picked), if (length(picked) == 1L) "" else "s", left
        )))
        return(invisible(NULL))
      }
      out <- sieve_exclude_items(state, picked, input$itc_reason %||% "")
      reason <- trimws(input$itc_reason %||% "")
      itc_result(list(kind = "ok", text = sprintf(
        "Excluded %d item%s from every analysis: %s%s. They stay in the data; include them again with Exclude / Include in the navigation bar.",
        length(out$changed), if (length(out$changed) == 1L) "" else "s", paste(out$changed, collapse = ", "),
        if (nzchar(reason)) sprintf(" (reason: %s)", reason) else " (no reason given)"
      ), advisory = sieve_coverage_advisory(out, ns = session$ns)))
      updateTextInput(session, "itc_reason", value = "")
    })

    observe({
      req(state$meta)
      shiny::updateSelectizeInput(session, "itc_restore_items", choices = sieve_drop_choices(state$meta, "parked"))
    })
    observeEvent(input$itc_restore, {
      picked <- input$itc_restore_items
      if (!length(picked)) {
        itc_result(list(kind = "error", text = "Choose the excluded items to include first."))
        return(invisible(NULL))
      }
      out <- sieve_set_item_status(state, picked, "active")
      itc_result(list(
        kind = "ok",
        text = sprintf(
          "Included %d items again: %s.",
          length(out$changed),
          paste(out$changed,
            collapse = ", "
          )
        )
      ))
    })

    # ---- 5. Careless / insufficient-effort responding ----------------------
    # Stable observation keys, independent of displayed order or imported-ID uniqueness.
    observation_view <- reactive({
      req(state$raw)
      r <- tryCatch(careless(), error = function(e) NULL)
      sieve_observation_review(state,
        flags = r$tbl %||% NULL,
        response_items = input$case_responses %||% character()
      )
    })
    observation_key <- reactive({
      req(state$raw)
      sieve_hash(list(
        dataset = state$dataset_id, data = sieve_data_key(state$raw),
        current = sieve_case_index(state),
        excluded = vapply(
          state$removed_cases %||% list(),
          function(r) as.integer(r$row),
          integer(1)
        )
      ))
    })
    case_grid_result <- reactiveVal(NULL)
    output$case_grid_result <- renderUI(case_grid_result())
    output$case_grid_note <- renderUI({
      d <- observation_view()
      from_flag <- opened()
      div(
        class = "small text-muted",
        sprintf(
          "%d respondents: %d in the analysis and %d removed. Raw response values are shown before reverse scoring. Current response-pattern flags describe the respondents in the analysis; a removed respondent keeps the flags and the definition recorded at the removal.",
          nrow(d), sum(d$status == "included"), sum(d$status == "excluded")
        ),
        if (length(from_flag)) sprintf(
          " Opened from the Careless responding panel for original row%s %s, with the screened items as the response columns%s.",
          if (length(from_flag) == 1L) "" else "s", paste(from_flag, collapse = ", "),
          if (length(from_flag) == 1L && !is.na(sieve_case_id_variable(state))) " and the table searched for the identifier" else ""
        )
      )
    })
    # A removal asked about in the dialog and then canceled redraws the
    # grid, so the status cell shows the respondent in the analysis again.
    grid_redraw <- reactiveVal(0L)
    output$case_grid <- renderDT({
      grid_redraw()
      d <- observation_view()
      if (!nrow(d)) return(sieve_dt(data.frame(Note = "Load the data on the Ingest tab first."), rownames = FALSE))
      key <- observation_key()
      raw_rows <- d$original_row
      # Numbers are rounded for display only (three decimals); the stored
      # values and the download keep full precision.
      d <- sieve_screen_display_round(d, digits = 3L)
      d[] <- lapply(d, function(x) htmltools::htmlEscape(ifelse(is.na(x), "", as.character(x))))
      d$status <- vapply(
        seq_len(nrow(d)), function(i) as.character(tags$select(
          `aria-label` = sprintf("In the analysis or removed: original row %d", raw_rows[i]),
          onchange = sprintf(
            "Shiny.setInputValue(%s,{row:%d,status:this.value,key:%s},{priority:'event'})",
            encodeString(ns("case_use"), quote = '"'), raw_rows[i], encodeString(key, quote = '"')
          ),
          tags$option(
            value = "included",
            selected = if (identical(
              observation_view()$status[i],
              "included"
            )) "selected" else NULL,
            "In the analysis"
          ),
          tags$option(
            value = "excluded",
            selected = if (identical(
              observation_view()$status[i],
              "excluded"
            )) "selected" else NULL,
            "Removed"
          )
        )),
        character(1)
      )
      d$Select <- vapply(
        raw_rows,
        function(row) as.character(tags$input(
          type = "checkbox",
          class = "sieve-observation-pick",
          `data-original-row` = row,
          `aria-label` = sprintf(
            "Select the respondent in original row %d to remove or restore",
            row
          )
        )),
        character(1)
      )
      callback <- DT::JS(sprintf(
        paste0(
          "var held={}, key=%s, node=$(table.table().node());",
          "function send(){var rows=Object.keys(held).filter(function(r){return held[r];}).map(Number);",
          "Shiny.setInputValue(%s,{rows:rows,key:key},{priority:'event'});}",
          "function paint(){node.find('input.sieve-observation-pick').each(function(){this.checked=!!held[this.getAttribute('data-original-row')];$(this).closest('tr').toggleClass('selected',this.checked);});}",
          "node.off('.sieverObservation').on('change.sieverObservation','input.sieve-observation-pick',function(e){e.stopPropagation();held[this.getAttribute('data-original-row')]=this.checked;paint();send();});",
          "node.on('click.sieverObservation','tbody td',function(e){if($(e.target).closest('input,select,button,a').length)return;var box=$(this).closest('tr').find('input.sieve-observation-pick')[0];if(box){box.checked=!box.checked;$(box).trigger('change');}});",
          "table.on('draw.sieverObservation',paint);paint();send();"
        ),
        encodeString(key, quote = '\"'), encodeString(ns("case_selected"), quote = '\"')
      ))
      shown_names <- c(
        original_row = "Original row", identifier = "Identifier", status = "Status",
        reason = "Reason", flagged_by = "Flagged by", answered = "Answered",
        longstring = "Longstring", irv = "Within-person SD", flags = "Screens flagged (of 3)",
        Select = "Select"
      )
      headers <- ifelse(names(d) %in% names(shown_names), shown_names[names(d)], names(d))
      sieve_dt(d,
        rownames = FALSE, escape = FALSE, callback = callback,
        selection = "none", colnames = unname(headers),
        options = list(
          dom = "fltip", pageLength = 15, scrollX = TRUE,
          columnDefs = list(list(orderable = FALSE, targets = c(which(names(d) == "status") - 1L, ncol(d) - 1L))),
          stateSave = TRUE, stateSaveCallback = sieve_dt_state_save(), stateLoadCallback = sieve_dt_state_load(),
          sieve = list(id = "screen-observations", keys = names(d))
        )
      )
    })
    change_observations <- function(rows, status, key, reason = input$case_reason %||% "") {
      if (!identical(key, isolate(observation_key()))) {
        case_grid_result(div(
          class = "alert alert-warning",
          "The respondents changed. Select the current rows again; nothing was changed."
        ))
        return(invisible(NULL))
      }
      r <- tryCatch(isolate(careless()), error = function(e) NULL)
      ans <- tryCatch(sieve_set_case_status(state, rows, status,
        reason = reason, screening = r
      ), error = function(e) list(refused = conditionMessage(e)))
      if (!is.null(ans$refused)) {
        case_grid_result(div(class = "alert alert-warning", sieve_screen_sentence(ans$refused)))
        return(invisible(NULL))
      }
      shiny::updateTextInput(session, "case_reason", value = "")
      changed <- ans$removed %||% ans$restored %||% integer()
      n <- length(changed)
      case_grid_result(div(
        class = "alert alert-info",
        sieve_screen_grid_change_text(n, status, NROW(isolate(state$raw)))
      ))
    }
    # A removal in the grid with the reason box empty asks for the reason
    # first:
    # Cancel puts the status cell back, and a reason left blank is recorded
    # as "no reason given". A restore needs no reason.
    pending_removal <- reactiveVal(NULL)
    remove_or_ask <- function(rows, key, from_cell) {
      if (nzchar(trimws(input$case_reason %||% ""))) {
        return(change_observations(rows, "excluded", key))
      }
      rows <- unique(as.integer(unlist(rows)))
      pending_removal(list(rows = rows, key = key, from_cell = from_cell))
      shiny::showModal(shiny::modalDialog(
        title = if (length(rows) == 1L) {
          sprintf("Remove the respondent in original row %d from the analysis", rows)
        } else {
          sprintf("Remove %d respondents from the analysis", length(rows))
        },
        textAreaInput(ns("case_reason_dialog"), "Reason (kept with the removal and in the session script)",
          rows = 2, width = "100%"
        ),
        p(class = "small text-muted mb-0", "A reason left blank is recorded as \"no reason given\"."),
        footer = tagList(
          actionButton(ns("case_reason_cancel"), "Cancel", class = "btn-outline-secondary"),
          actionButton(ns("case_reason_confirm"), "Remove from the analysis", class = "btn-danger")
        ),
        easyClose = FALSE, size = "m"
      ))
      invisible(NULL)
    }
    observeEvent(input$case_reason_confirm, {
      pend <- pending_removal()
      if (is.null(pend)) return(invisible(NULL))
      pending_removal(NULL)
      shiny::removeModal()
      reason <- trimws(input$case_reason_dialog %||% "")
      change_observations(pend$rows, "excluded", pend$key,
        reason = if (nzchar(reason)) reason else "no reason given"
      )
    })
    observeEvent(input$case_reason_cancel, {
      pend <- pending_removal()
      pending_removal(NULL)
      shiny::removeModal()
      if (isTRUE(pend$from_cell)) grid_redraw(isolate(grid_redraw()) + 1L)
      case_grid_result(div(class = "alert alert-light border small", "Nothing was removed."))
    })
    observeEvent(input$case_use, {
      x <- input$case_use
      if (!is.list(x) || length(x$status) != 1L || !x$status %in% c("included", "excluded")) return(invisible(NULL))
      if (identical(x$status, "excluded")) {
        remove_or_ask(x$row, x$key, from_cell = TRUE)
      } else {
        change_observations(x$row, x$status, x$key)
      }
    })
    observeEvent(input$case_exclude, {
      picked <- input$case_selected
      if (is.null(picked) || !length(picked$rows)) {
        case_grid_result(div(class = "alert alert-warning", "Select the rows of respondents first."))
        return(invisible(NULL))
      }
      remove_or_ask(unlist(picked$rows), picked$key, from_cell = FALSE)
    })
    observeEvent(input$case_include, {
      picked <- input$case_selected
      if (is.null(picked) || !length(picked$rows)) {
        case_grid_result(div(class = "alert alert-warning", "Select the rows of respondents first."))
        return(invisible(NULL))
      }
      change_observations(unlist(picked$rows), "included", picked$key)
    })
    output$dl_case_ledger <- downloadHandler(
      filename = function() "siever_observation_inclusion.csv",
      content = function(file) utils::write.csv(observation_view(), file, row.names = FALSE, na = "")
    )

    # Row-level screens over the active items in their original (upload)
    # column order, the closest available stand-in for presentation order:
    # longstring (Johnson, 2005), within-person variability (Dunn, Heggestad,
    # Shanock & Theilgard, 2018), and Mahalanobis distance with a chi square
    # tail probability (Meade & Craig, 2012). Each row carries its row of the
    # data as loaded and its identifier, so a respondent keeps one name when
    # others are removed; removal itself is sieve_remove_cases() (fct_cases.R).
    observeEvent(included_items(),
      {
        it <- intersect(names(state$raw), included_items())
        sp <- isolate(state$screening_spec) %||% list()
        selected <- if (is.null(sp$items)) it else intersect(it, sp$items)
        shiny::updateSelectizeInput(session, "screen_items", choices = it, selected = selected)
        shiny::updateSelectizeInput(session, "case_responses", choices = names(state$raw), selected = head(it, 3L))
      },
      ignoreNULL = FALSE
    )
    observeEvent(state$screening_spec,
      {
        sp <- state$screening_spec %||% list()
        shiny::updateNumericInput(session, "screen_min_run", value = sp$min_run %||% 0)
        shiny::updateNumericInput(session, "screen_min_answered", value = sp$min_answered %||% 3)
        shiny::updateSelectInput(session, "screen_missing", selected = sp$missing %||% "break")
        shiny::updateNumericInput(session, "screen_irv", value = sp$irv_tol %||% 0)
        shiny::updateNumericInput(session, "screen_md_alpha", value = sp$md_alpha %||% .001)
      },
      ignoreNULL = FALSE
    )
    screen_rule_status <- reactiveVal(NULL)
    output$screen_rule_status <- renderUI(screen_rule_status())
    observeEvent(input$screen_apply, {
      it <- intersect(intersect(names(state$raw), included_items()), input$screen_items %||% character())
      sp <- list(
        items = it,
        min_run = if (identical(
          as.numeric(input$screen_min_run),
          0
        )) NULL else input$screen_min_run,
        min_answered = input$screen_min_answered, missing = input$screen_missing,
        irv_tol = input$screen_irv, md_alpha = input$screen_md_alpha, values = "raw"
      )
      read <- tryCatch(sieve_response_screens(state$raw[, it, drop = FALSE],
        min_run = sp$min_run, min_answered = sp$min_answered, missing = sp$missing,
        irv_tol = sp$irv_tol, md_alpha = sp$md_alpha
      ), error = function(e) e)
      if (inherits(read, "error")) {
        screen_rule_status(div(class = "alert alert-warning", conditionMessage(read)))
        return(invisible(NULL))
      }
      state$screening_spec <- sp
      screen_rule_status(div(
        class = "alert alert-success",
        "Screen definition applied. A flag does not remove a respondent by itself."
      ))
    })
    careless <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      sp <- state$screening_spec %||% list()
      it <- intersect(names(state$raw), included_items())
      if (!is.null(sp$items)) it <- intersect(it, sp$items)
      req(length(it) >= 3L)
      r <- sieve_response_screens(state$raw[, it, drop = FALSE],
        min_run = sp$min_run,
        min_answered = sp$min_answered %||% 3L, missing = sp$missing %||% "break",
        irv_tol = sp$irv_tol %||% 0, md_alpha = sp$md_alpha %||% .001
      )
      ids <- sieve_case_ids(state)
      r$tbl$row <- ids$row
      r$tbl$id <- ids$id
      r$id_variable <- attr(ids, "variable")
      r$items <- it
      note <- sprintf(
        "Screening: careless-responding screens over %d active items, on raw responses; run >= %d, minimum answered %d, missing responses %s runs.",
        r$n_item,
        r$long_cut,
        r$min_answered,
        r$missing
      )
      code <- sprintf(
        "response_screens <- SIEVER::sieve_response_screens(d[, %s, drop = FALSE], min_run = %s, min_answered = %dL, missing = %s, irv_tol = %s, md_alpha = %s)",
        paste(utils::capture.output(dput(it)),
          collapse = " "
        ),
        sieve_r_literal(r$long_cut),
        r$min_answered,
        sieve_r_literal(r$missing),
        sieve_r_literal(r$irv_tol),
        sieve_r_literal(r$md_alpha)
      )
      if (!identical(kept$careless, list(note, code))) {
        sieve_log(state,
          note,
          c(
            code,
            "longstring <- response_screens$tbl$longstring",
            "irv <- response_screens$tbl$irv"
          ),
          passive = TRUE
        )
        kept$careless <- list(note, code)
      }
      r
    })
    output$missing_rule_note <- renderUI({
      req(state$raw, state$items)
      it <- included_items()
      d <- state$items[, it, drop = FALSE]
      n <- nrow(d)
      complete <- if (length(it)) sum(stats::complete.cases(d)) else 0L
      available <- sum(rowSums(!is.na(d)) > 0L)
      div(
        class = "small text-muted",
        sprintf(
          "Current pool: %d respondents in the analysis; %d complete on all %d included items; %d with at least one response. Removed respondents are outside every analysis. Each analysis reports its own estimator, missing-data rule, and analytic n.",
          n,
          complete,
          length(it),
          available
        )
      )
    })

    output$careless_note <- renderUI({
      r <- tryCatch(careless(), error = function(e) NULL)
      if (is.null(r)) {
        return(div(
          class = "alert alert-info",
          "The careless-responding screens need at least three included items."
        ))
      }
      sieve_screen_careless_definitions(r)
    })

    # The definitions are in an open explanation: rendered while it is closed,
    # so they are there when it is opened.
    outputOptions(output, "careless_note", suspendWhenHidden = FALSE)

    output$careless_summary <- renderDT({
      r <- tryCatch(careless(), error = function(e) NULL)
      if (is.null(r)) {
        return(sieve_dt(
          data.frame(Note = "The careless-responding screens need at least three included items."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      df <- data.frame(
        Screen = c(
          sprintf("Longstring >= %d of %d items (straight-lining)", r$long_cut, r$n_item),
          sprintf("Within-person SD <= %s (invariant responding)", r$irv_tol),
          sprintf("Mahalanobis p < %s, chi square on %d df (multivariate outlier)", r$md_alpha, r$n_item)
        ),
        Flagged = c(r$n_long, r$n_irv, r$n_md),
        stringsAsFactors = FALSE
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t", ordering = FALSE),
        colnames = c("Screen", "Respondents flagged")
      )
    })

    # The flagged respondents in the order shown, so a selected row of the
    # table names one respondent.
    careless_flagged <- reactive({
      r <- careless()
      df <- r$tbl[r$tbl$flags > 0, , drop = FALSE]
      df[order(-df$flags, df$p_mahalanobis), , drop = FALSE]
    })

    output$careless_tbl <- renderDT({
      r <- tryCatch(careless(), error = function(e) NULL)
      req(r)
      df <- careless_flagged()
      if (!nrow(df)) {
        return(sieve_dt(
          data.frame(Note = "No respondent is flagged by any of the three screens."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      with_id <- !is.na(r$id_variable)
      shown <- data.frame(
        row = df$row, id = df$id, longstring = df$longstring, irv = df$irv,
        mahalanobis = df$mahalanobis, p_mahalanobis = df$p_mahalanobis,
        flags = df$flags, flagged_by = df$flagged_by, stringsAsFactors = FALSE
      )
      if (!with_id) {
        shown$id <- NULL
      }
      sieve_dt(shown,
        rownames = FALSE, selection = list(mode = "multiple", target = "row"),
        options = list(
          dom = "ftp", pageLength = 15, scrollX = TRUE, stateSave = TRUE,
          stateSaveCallback = sieve_dt_state_save(), stateLoadCallback = sieve_dt_state_load(),
          sieve = list(id = "screen-flagged-cases", keys = names(shown))
        ),
        colnames = c(
          "Original row", if (with_id) sprintf("Identifier (%s)", r$id_variable),
          "Longstring", "Within-person SD", "Mahalanobis D2", "p (chi square)",
          "Screens flagged (of 3)", "Flagged by"
        )
      ) |>
        formatRound(c("irv", "mahalanobis"), 3) |>
        formatRound("p_mahalanobis", 4)
    })

    output$careless_detail <- renderUI({
      df <- careless_flagged()
      rows <- input$careless_tbl_rows_selected
      rows <- rows[rows >= 1L & rows <= nrow(df)]
      if (!length(rows)) return(p(
        class = "small text-muted",
        "Select a flagged respondent for the full stable identity and flags."
      ))
      var <- sieve_case_id_variable(state)
      tagList(lapply(rows, function(i) {
        p(
          class = "small border rounded p-2",
          strong(sprintf("Original row %d", df$row[i])),
          if (!is.na(var)) paste0(
            "; imported identifier (",
            var,
            "): ",
            if (is.na(df$id[i])) "(missing)" else df$id[i]
          ),
          paste0("; flagged by: ", df$flagged_by[i]),
          ". Original row identifies the case even when imported identifiers are duplicated or missing.",
          responses_line(df$row[i])
        )
      }),
      actionButton(ns("open_responses"), "Show in the Observations table",
        class = "btn-outline-secondary btn-sm mt-1"
      ))
    })
    # The respondent's own responses beside the flag: the screened items in
    # the order the screens read them, so a long run or an outlying pattern
    # can be read where the flag is; the button opens the Observations table
    # with the same columns and the respondent found.
    responses_line <- function(row) {
      r <- tryCatch(careless(), error = function(e) NULL)
      its <- r$items %||% character(0)
      at <- match(row, sieve_case_index(state))
      if (!length(its) || is.na(at)) {
        return(NULL)
      }
      values <- vapply(its, function(it) {
        v <- state$raw[[it]][[at]]
        if (is.null(v) || is.na(v)) "missing" else format(v, trim = TRUE)
      }, character(1))
      tags$span(
        class = "d-block mt-1",
        sprintf("Responses on the %d screened items, in the order screened: ", length(its)),
        tags$code(paste(sprintf("%s %s", its, values), collapse = "; "))
      )
    }
    observeEvent(input$open_responses, {
      df <- careless_flagged()
      rows <- input$careless_tbl_rows_selected
      rows <- rows[rows >= 1L & rows <= nrow(df)]
      req(length(rows))
      r <- careless()
      shiny::updateSelectizeInput(session, "case_responses", selected = r$items)
      bslib::nav_select("screen_tabs", "Observations", session = session)
      var <- sieve_case_id_variable(state)
      ids <- df$id[rows]
      # The grid's search finds the respondents by their identifier; rows
      # without one are named in the note.
      if (!is.na(var) && length(ids) == 1L && !is.na(ids)) {
        DT::updateSearch(DT::dataTableProxy("case_grid", session = session), keywords = list(global = ids))
      }
      opened(df$row[rows])
    })
    opened <- reactiveVal(NULL)
    output$dl_flagged_cases <- shiny::downloadHandler(
      filename = function() "siever_flagged_respondents.csv",
      content = function(file) {
        df <- careless_flagged()
        names(df)[names(df) == "row"] <- "original_row"
        df$id_variable <- sieve_case_id_variable(state)
        utils::write.csv(df, file, row.names = FALSE, na = "")
      }
    )

    # ---- Removing respondents, and restoring them ----------------------------

    output$remove_hint <- renderUI({
      req(state$raw)
      var <- sieve_case_id_variable(state)
      text <- if (is.na(var)) {
        paste(
          "These data have no identifier column (set one with the Ingest tab's Variable roles panel),",
          "so type rows of the data as loaded (the Row column), separated by commas or spaces."
        )
      } else {
        sprintf(paste(
          "Type values of the identifier column %s, separated by commas or spaces. To name a row",
          "of the data as loaded (the Row column) instead, type it with a leading # (#17); an entry",
          "that is not an identifier is never read as a row, so a mistyped identifier removes no one."
        ), var)
      }
      div(class = "small text-muted mb-1", text)
    })

    output$remove_preview <- renderUI({
      req(state$raw)
      typed <- input$remove_ids %||% ""
      sel <- input$careless_tbl_rows_selected
      if (!nzchar(trimws(typed)) && !length(sel)) {
        return(NULL)
      }
      ids_now <- sieve_case_ids(state)
      text <- sieve_case_parse_text(sieve_case_parse(typed, ids_now), attr(ids_now, "variable"))
      div(
        class = "small text-muted mb-1", role = "status",
        if (length(sel)) sprintf(
          "%d selected in the flagged respondents table%s",
          length(sel), if (is.null(text)) "." else "; typed: "
        ),
        text
      )
    })

    remove_result <- reactiveVal(NULL)
    restore_result <- reactiveVal(NULL)
    show_result <- function(r) {
      if (is.null(r)) {
        return(NULL)
      }
      div(
        class = paste("alert py-2 small mt-1", if (identical(r$kind, "error")) "alert-warning" else "alert-success"),
        r$text
      )
    }
    output$remove_result <- renderUI(show_result(remove_result()))
    output$restore_result <- renderUI(show_result(restore_result()))

    observeEvent(input$remove_go, {
      r <- tryCatch(isolate(careless()), error = function(e) NULL)
      flagged <- tryCatch(isolate(careless_flagged()), error = function(e) NULL)
      sel <- input$careless_tbl_rows_selected
      from_table <- if (length(sel) && is.data.frame(flagged) && nrow(flagged)) {
        flagged$row[sel[sel >= 1L & sel <= nrow(flagged)]]
      } else {
        integer(0)
      }
      ids_now <- isolate(sieve_case_ids(state))
      typed <- sieve_case_parse(input$remove_ids %||% "", ids_now)
      rows <- unique(c(from_table, typed$rows))
      unmatched <- if (length(typed$unmatched)) {
        paste0(
          sprintf(
            " Not found among the respondents in the analysis, so not removed: %s.",
            sieve_name_some(typed$unmatched)
          ),
          if (length(typed$hint)) paste0(" ", typed$hint) else ""
        )
      } else {
        ""
      }
      if (!length(rows)) {
        remove_result(list(kind = "error", text = paste0(
          "Select rows of the flagged respondents table or type respondents in the box first; nothing was removed.",
          unmatched
        )))
        return(invisible(NULL))
      }
      flagged_by <- if (!is.null(r)) {
        fb <- r$tbl$flagged_by[match(rows, r$tbl$row)]
        fb[is.na(fb) | !nzchar(fb)] <- "not flagged"
        stats::setNames(fb, rows)
      }
      out <- sieve_set_case_status(state, rows, "excluded", input$remove_reason %||% "", screening = r)
      if (!is.null(out$refused)) {
        remove_result(list(kind = "error", text = paste0(sieve_screen_sentence(out$refused), unmatched)))
        return(invisible(NULL))
      }
      reason <- trimws(input$remove_reason %||% "")
      remove_result(list(kind = "ok", text = paste0(
        sprintf(
          "Removed %d respondent%s from the analysis: %s%s. Every tab now works on the %d remaining respondents; restore any of them below.",
          length(out$removed), if (length(out$removed) == 1L) "" else "s",
          sieve_cases_who(
            out$removed, ids_now$id[match(out$removed, ids_now$row)],
            attr(ids_now, "variable")
          ),
          if (nzchar(reason)) sprintf(" (reason: %s)", reason) else " (no reason given)",
          NROW(isolate(state$raw))
        ),
        unmatched
      )))
      restore_result(NULL)
      updateTextAreaInput(session, "remove_ids", value = "")
      updateTextAreaInput(session, "remove_reason", value = "")
    })

    removed_shown <- reactive({
      sieve_removed_table(state)
    })

    output$removed_note <- renderUI({
      req(state$raw)
      k <- nrow(removed_shown())
      n <- NROW(state$raw)
      text <- if (!k) {
        sprintf("No respondent is removed: all %d respondents are in the analysis.", n)
      } else {
        sprintf(
          paste(
            "%d respondent%s removed from the analysis; %d remain. Select rows and press",
            "Restore the selected to put them back in their original rows."
          ),
          k, if (k == 1L) " is" else "s are", n
        )
      }
      div(class = "small text-muted mb-1", text)
    })

    output$removed_tbl <- renderDT({
      df <- removed_shown()
      if (!nrow(df)) {
        return(sieve_dt(data.frame(Note = "No respondent is removed from the analysis."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      var <- sieve_case_id_variable(state)
      shown <- data.frame(
        id = df$id, row = df$row,
        flagged_by = ifelse(is.na(df$flagged_by) | !nzchar(df$flagged_by),
          "not flagged",
          df$flagged_by
        ),
        reason = ifelse(is.na(df$reason), "(no reason given)", df$reason),
        time = df$time, stringsAsFactors = FALSE
      )
      if (is.na(var)) {
        shown$id <- NULL
      }
      sieve_dt(shown,
        rownames = FALSE, selection = list(mode = "multiple", target = "row"),
        options = list(dom = "tp", pageLength = 10, scrollX = TRUE),
        colnames = c(
          if (!is.na(var)) sprintf("Identifier (%s)", var), "Original row",
          "Flagged by (when removed)", "Reason", "Removed at"
        )
      )
    })

    observeEvent(input$restore_go, {
      df <- isolate(removed_shown())
      sel <- input$removed_tbl_rows_selected
      rows <- if (length(sel) && nrow(df)) df$row[sel[sel >= 1L & sel <= nrow(df)]] else integer(0)
      if (!length(rows)) {
        restore_result(list(
          kind = "error",
          text = "Select rows of the Removed respondents table first; nothing was restored."
        ))
        return(invisible(NULL))
      }
      ids <- df$id[match(rows, df$row)]
      out <- sieve_set_case_status(state, rows, "included", reason = "Restored from excluded observations")
      # A restore can be refused (a held value outside the bounds of a later
      # reversal); the reason is shown and nothing changes.
      if (!is.null(out$refused)) {
        restore_result(list(kind = "error", text = sieve_screen_sentence(out$refused)))
        return(invisible(NULL))
      }
      restore_result(list(kind = "ok", text = sprintf(
        "Restored %d respondent%s to the analysis in their original rows: %s. %d respondents are in the analysis.",
        length(out$restored), if (length(out$restored) == 1L) "" else "s",
        sieve_cases_who(out$restored, ids[match(out$restored, rows)], sieve_case_id_variable(state)),
        NROW(isolate(state$raw))
      )))
      remove_result(NULL)
    })

    # ---- Validation reservation (fct_holdout.R) ------------------------------
    holdout_now <- reactive({
      state$holdout
      state$dataset_id
      sieve_holdout(state)
    })
    holdout_result <- reactiveVal(NULL)
    output$holdout_result <- renderUI(holdout_result())
    # The grouping variables a reservation can be stratified by: the
    # columns with the group role (the choice in force is kept while it
    # is offered).
    observe({
      state$roles
      state$raw
      groups <- if (is.null(state$raw)) character(0) else intersect(sieve_role_vars(state, "group"), names(state$raw))
      choices <- c("None" = "", stats::setNames(groups, groups))
      updateSelectInput(session, "holdout_stratify",
        choices = choices,
        selected = sieve_keep_choice(isolate(input$holdout_stratify), choices, "")
      )
    })
    output$holdout_status <- renderUI({
      req(state$raw)
      rec <- holdout_now()
      if (is.null(rec)) {
        released <- Filter(function(r) identical(r$status, "released"), state$holdout %||% list())
        return(div(
          class = "small text-muted mb-2",
          sprintf(
            "No validation reservation is in force: all %d respondents in the analysis are read by every development result.",
            NROW(state$raw)
          ),
          if (length(released)) {
            last <- released[[length(released)]]
            sprintf(
              " Reservation %s (%d rows, seed %d) was released at %s; its record and evaluations are kept.",
              last$id, length(last$rows), last$rng$seed, last$released %||% "an earlier time"
            )
          }
        ))
      }
      div(
        class = "alert alert-info py-2 small mb-2",
        strong(sprintf("Reservation %s is in force. ", rec$id)),
        sprintf(
          "%d of %d respondents reserved for validation (proportion %s, seed %d%s%s, drawn %s). %s",
          rec$n_reserved, rec$n_reserved + rec$n_calibration, format(rec$proportion, digits = 7),
          rec$rng$seed, if (isTRUE(rec$rng$supplied)) "" else " drawn for this run",
          if (is.na(rec$stratify)) "" else sprintf(", stratified by %s", rec$stratify), rec$time,
          sieve_holdout_n_sentence(state)
        ),
        if (isTRUE(rec$after_development)) {
          p(
            class = "mb-0 mt-1",
            strong("Reserved after development began. "),
            sprintf(
              "%d recorded %s, %d requested analysis %s and %d page %s preceded it%s, so an evaluation on the reserved rows is a split of data already examined, not independent validation.",
              nrow(rec$preceded_by$analyses), if (nrow(rec$preceded_by$analyses) == 1L) "analysis" else "analyses",
              length(rec$preceded_by$steps), if (length(rec$preceded_by$steps) == 1L) "step" else "steps",
              length(rec$preceded_by$pages), if (length(rec$preceded_by$pages) == 1L) "result" else "results",
              if (length(c(rec$preceded_by$steps, rec$preceded_by$pages))) {
                sprintf(" (%s)", sieve_name_some(c(rec$preceded_by$steps, rec$preceded_by$pages), 4L))
              } else {
                ""
              }
            )
          )
        } else {
          p(class = "mb-0 mt-1", "Reserved before any result existed: the reserved rows played no part in fitting or selecting the model.")
        }
      )
    })
    output$holdout_sizes <- renderUI({
      req(state$raw, isTRUE(sieve_confirmation_status(state)$confirmed))
      if (!is.null(holdout_now())) return(NULL)
      state$items
      state$estimator_choice
      plan <- sieve_holdout_plan(state, input$holdout_proportion, input$holdout_stratify)
      if (!is.null(plan$refusal)) {
        return(div(class = "small text-danger", sieve_screen_sentence(plan$refusal)))
      }
      by_stratum <- if (nrow(plan$sizes) > 1L) {
        sizes <- plan$sizes
        label <- ifelse(sizes$missing, "missing value", sizes$stratum)
        counts <- ifelse(sizes$by_seed,
          sprintf("%d or %d", sizes$reserved, sizes$reserved + 1L),
          as.character(sizes$reserved)
        )
        placed <- attr(sizes, "by_seed_rows")
        paste0(
          sprintf(
            " By level of %s (largest remainder allocation, so the total is exact): %s.", plan$stratify,
            paste(sprintf("%s %s of %d", label, counts, sizes$n), collapse = "; ")
          ),
          if (placed > 0L) {
            sprintf(
              " The levels with two counts tie in their remainders, and the seed places %d more row%s among them.",
              placed, if (placed == 1L) "" else "s"
            )
          } else {
            ""
          },
          if (any(sizes$empty_half)) {
            sprintf(
              " One half holds no row of %s %s.",
              if (sum(sizes$empty_half) == 1L) "the level" else "the levels",
              sieve_name_some(label[sizes$empty_half])
            )
          } else {
            ""
          }
        )
      } else {
        ""
      }
      div(
        class = "small text-muted",
        sprintf(
          "Reserve would set aside %d of the %d respondents in the analysis and leave %d calibration rows.%s",
          plan$reserved, plan$n, plan$calibration, by_stratum
        )
      )
    })
    observeEvent(input$holdout_reserve, {
      out <- sieve_reserve_holdout(state, input$holdout_proportion,
        seed = sieve_seed_input(input$holdout_seed), stratify = input$holdout_stratify
      )
      if (!is.null(out$refused)) {
        holdout_result(div(class = "alert alert-danger", sieve_screen_sentence(out$refused)))
        return(invisible(NULL))
      }
      rec <- out$record
      holdout_result(div(
        class = "alert alert-success",
        sprintf(
          "Reserved %d respondents for validation (reservation %s, seed %d%s): %s. Every development result now reads the %d calibration rows.%s",
          length(out$reserved), rec$id, rec$rng$seed,
          if (isTRUE(rec$rng$supplied)) "" else " drawn and recorded",
          sieve_cases_who(out$reserved, rep(NA_character_, length(out$reserved)), NA_character_),
          rec$n_calibration,
          if (isTRUE(rec$after_development)) " Recorded as reserved after development began." else ""
        )
      ))
      remove_result(NULL)
      restore_result(NULL)
    })
    # Release cannot be undone (a new reservation is a new draw, and the
    # exposure of the reserved rows is recorded), so the button states its
    # count and a press asks before anything is released, so the
    # consequence is stated before the release and not after it.
    observe({
      rec <- sieve_holdout(state)
      updateActionButton(session, "holdout_release", label = if (is.null(rec)) {
        "Release the reservation"
      } else {
        sprintf("Release the %d reserved rows", length(sieve_holdout_held(state, rec$id)))
      })
    })
    observeEvent(input$holdout_release, {
      rec <- isolate(sieve_holdout(state))
      if (is.null(rec)) {
        out <- sieve_release_holdout(state)
        holdout_result(div(class = "alert alert-danger", sieve_screen_sentence(out$refused)))
        return(invisible(NULL))
      }
      n <- length(isolate(sieve_holdout_held(state, rec$id)))
      evaluated <- length(rec$evaluations %||% list())
      showModal(modalDialog(
        title = sprintf("Release reservation %s?", rec$id),
        p(sprintf(paste(
          "The %d reserved respondents return to the analysis in their original rows, and every",
          "development result then reads them. This cannot be undone: a new reservation is a new",
          "draw, and the reservation's record says the reserved rows were released, so an evaluation",
          "after this can no longer claim rows that played no part in development."
        ), n)),
        p(if (evaluated) {
          sprintf("Its %d evaluation%s and their records are kept.", evaluated, if (evaluated == 1L) "" else "s")
        } else {
          "No evaluation on the reserved rows has been made yet."
        }),
        footer = tagList(
          modalButton("Keep the reservation"),
          actionButton(session$ns("holdout_release_confirm"), sprintf("Release the %d rows", n), class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })
    observeEvent(input$holdout_release_confirm, {
      removeModal()
      out <- sieve_release_holdout(state)
      if (!is.null(out$refused)) {
        holdout_result(div(class = "alert alert-danger", sieve_screen_sentence(out$refused)))
        return(invisible(NULL))
      }
      holdout_result(div(
        class = "alert alert-warning",
        sprintf(
          "Released reservation %s: %d respondents returned to the analysis in their original rows. Development now sees the reserved rows, and the reservation's protection of an evaluation is gone; its record and evaluations are kept.",
          out$record$id, length(out$restored)
        )
      ))
      remove_result(NULL)
      restore_result(NULL)
    })
  })
}

# ---- Small internal helpers for the screening module -----------------------

#' Round the numbers of a grid for display
#'
#' A double column that holds a value that is not a whole number (the
#' within-person SD, for one) is written with `digits` decimals; whole
#' numbers and text are left as they are. For display only: the stored
#' values and the downloads keep full precision.
#'
#' @param d A data frame.
#' @param digits Decimals shown.
#' @return `d` with those columns as text.
#' @author Ken Kelley
#' @noRd
sieve_screen_display_round <- function(d, digits = 3L) {
  for (nm in names(d)) {
    x <- d[[nm]]
    if (is.double(x) && any(is.finite(x) & x != round(x))) {
      d[[nm]] <- ifelse(is.finite(x), formatC(x, format = "f", digits = digits), NA_character_)
    }
  }
  d
}

#' What a change in the Observations grid did, in words
#'
#' Respondents are removed and restored (the words of the respondent tools);
#' the sentence says where a removed respondent is restored.
#'
#' @param n How many respondents changed.
#' @param status `"excluded"` (removed) or `"included"` (restored).
#' @param remaining The respondents in the analysis now.
#' @return One or two sentences.
#' @author Ken Kelley
#' @noRd
sieve_screen_grid_change_text <- function(n, status, remaining) {
  if (!n) {
    return(sprintf(
      "Nothing changed: the selected respondents are already %s.",
      if (identical(status, "excluded")) "removed from the analysis" else "in the analysis"
    ))
  }
  who <- sprintf("%d respondent%s", n, if (n == 1L) "" else "s")
  if (identical(status, "excluded")) {
    sprintf(
      paste(
        "Removed %s from the analysis; every tab now works on the %d remaining. Restore %s here",
        "(the status cell, or Restore the selected respondents) or in the Removed respondents table",
        "of the Careless responding panel."
      ),
      who, as.integer(remaining), if (n == 1L) "it" else "them"
    )
  } else {
    sprintf(
      "Restored %s to the analysis in %s original row%s; %d respondents are in the analysis.",
      who, if (n == 1L) "its" else "their", if (n == 1L) "" else "s", as.integer(remaining)
    )
  }
}

#' A phrase as a sentence: a capital first letter and a final period
#'
#' @param x A character string.
#' @return The sentence.
#' @author Ken Kelley
#' @noRd
sieve_screen_sentence <- function(x) {
  x <- trimws(as.character(x))
  x <- paste0(toupper(substr(x, 1L, 1L)), substr(x, 2L, nchar(x)))
  if (grepl("[.]$", x)) x else paste0(x, ".")
}

#' The three careless-responding screens, defined with the numbers in force
#'
#' Classroom definitions at the point of use: what each screen measures,
#' what it is computed on, and the cutoff in force here with its basis.
#'
#' @param r The module's `careless()` result.
#' @return A tag list.
#' @author Ken Kelley
#' @noRd
sieve_screen_careless_definitions <- function(r) {
  shiny::div(
    class = "small mb-2",
    shiny::tags$dl(
      shiny::tags$dt("Straightlining review: longstring"),
      shiny::tags$dd(sprintf(
        "Longest run of exactly equal raw responses among %d selected items in the data file's column order, before reverse scoring. A missing response %s a run. Flag cutoff: at least %d consecutive equal answers, with at least %d answered items. These are researcher-controlled screening conventions; a long run can be legitimate and is not proof of careless responding. Items must share comparable response options, and file order must be a defensible proxy for presentation order (Johnson, 2005; Meade & Craig, 2012).",
        r$n_item,
        if (identical(
          r$missing,
          "skip"
        )) "is skipped and can join" else "breaks",
        r$long_cut,
        r$min_answered
      )),
      shiny::tags$dt("Within-person response variability"),
      shiny::tags$dd(sprintf(
        "SD of raw responses to the selected items. Flagged at SD <= %s with at least %d answers. A constant pattern can reflect either legitimate standing or insufficient effort. All-missing and insufficient-response cases do not receive a response-pattern flag.",
        r$irv_tol,
        r$min_answered
      )),
      shiny::tags$dt("Mahalanobis distance"),
      shiny::tags$dd(sprintf(
        "D2 = (x - xbar)' S^-1 (x - xbar), using the complete cases on these items. The chi-square tail on %d df is a descriptive approximation; the flag cutoff is p < %s. It is not an exact reference for arbitrary ordinal or nonnormal responses. Evaluated for %d of %d observations (%d complete). %s",
        r$n_item, r$md_alpha, r$n_md_eval, r$n_resp, r$n_complete, r$unavailable %||% ""
      ))
    ),
    shiny::p("A flag never removes a respondent by itself. Select respondents, inspect their raw responses and identifiers, and record the reason for any removal. The record of each removal keeps the definition in force.")
  )
}



#' How a column is stored, in plain words
#'
#' @param x A column of the data as read.
#' @return One of "number", "yes/no", "category", "date", or "text".
#' @author Ken Kelley
#' @noRd
sieve_screen_storage <- function(x) {
  if (is.factor(x)) {
    return("category")
  }
  if (inherits(x, c("Date", "POSIXt"))) {
    return("date")
  }
  x <- unclass(x)
  if (is.logical(x)) {
    return("yes/no")
  }
  if (is.numeric(x)) {
    return("number")
  }
  "text"
}

#' The plural of a role, for the note at the top of the tab
#'
#' @param role Role names from `sieve_role_levels()`.
#' @param n How many of each.
#' @return Character, one per role.
#' @author Ken Kelley
#' @noRd
sieve_screen_role_words <- function(role, n) {
  one <- c(
    identifier = "identifier", group = "grouping variable", criterion = "criterion",
    antecedent = "antecedent", auxiliary = "auxiliary variable", item = "item"
  )
  many <- c(
    identifier = "identifiers", group = "grouping variables", criterion = "criteria",
    antecedent = "antecedents", auxiliary = "auxiliary variables", item = "items"
  )
  out <- ifelse(n == 1L, one[role], many[role])
  out[is.na(out)] <- role[is.na(out)]
  unname(out)
}

#' Item names ordered by construct, then facet, then ledger position
#'
#' Constructs and facets are taken in order of first appearance in the
#' ledger, never alphabetically, so a ledger that is already grouped keeps
#' its order and one that is not is grouped without being reshuffled.
#'
#' @param m Ledger rows (a data frame with `item`, `construct`, `facet`).
#' @return The item names in plot order.
#' @author Ken Kelley
#' @noRd
sieve_screen_item_order <- function(m) {
  if (is.null(m) || !nrow(m)) {
    return(character(0))
  }
  con <- as.character(m$construct)
  con[is.na(con)] <- ""
  fac <- as.character(m$facet)
  fac[is.na(fac)] <- ""
  as.character(m$item)[order(match(con, unique(con)), match(fac, unique(fac)), seq_len(nrow(m)))]
}


# ---- The item total correlations ------------------------------------------

#' Corrected item total correlations within construct
#'
#' For each item, the Pearson correlation of its scores with the mean of
#' the other items of its construct (the corrected, or item rest, total),
#' the mean taken over the peer items each respondent answered, so a
#' respondent missing some items still contributes, and the correlation
#' taken over the respondents with both values (pairwise). Items with no
#' construct (`NA`, empty, or `"Unassigned"`) are totaled together. A
#' correlation that is not defined is `NA`, with the reason in `note`: the
#' item is the only one in its construct, it has no variance, the total of
#' the others has no variance, or fewer than three respondents have both
#' values.
#'
#' @param d A data frame of numeric item scores (one column per item).
#' @param construct The construct of each column.
#' @param labels The item name of each column (`NA` falls back to the
#'   variable).
#' @param low The flag cutoff (recolors nothing; marks `"low itc"`).
#' @return A data frame with one row per item: `item`, `label`,
#'   `construct` (as given), `construct_shown` (with `"(no construct)"` for
#'   the items without one), `unassigned`, `n_peers`, `n`, `item_total`
#'   (full precision), `flag`, and `note`; the attribute `n_resp` holds
#'   the number of respondents.
#' @author Ken Kelley
#' @noRd
sieve_screen_item_total <- function(d, construct, labels = NULL, low = 0.30) {
  it <- names(d)
  construct <- as.character(construct)
  unassigned <- !sieve_construct_assigned(construct)
  group <- ifelse(unassigned, "\r", construct)
  labels <- as.character(labels %||% it)
  labels[is.na(labels) | !nzchar(labels)] <- it[is.na(labels) | !nzchar(labels)]
  rows <- lapply(seq_along(it), function(i) {
    peers <- it[group == group[i] & it != it[i]]
    x <- d[[i]]
    r <- NA_real_
    n <- NA_integer_
    note <- ""
    if (!length(peers)) {
      note <- if (unassigned[i]) "the only included item without a construct" else "the only included item in its construct"
    } else {
      peer_mat <- d[, peers, drop = FALSE]
      rest <- rowMeans(peer_mat, na.rm = TRUE)
      rest[rowSums(!is.na(peer_mat)) == 0] <- NA_real_
      ok <- !is.na(x) & !is.na(rest)
      n <- sum(ok)
      if (n < 3L) {
        note <- "fewer than three respondents answered it and another item of the construct"
      } else if (stats::var(x[ok]) == 0) {
        note <- "no variance: every respondent gave the same answer"
      } else if (stats::var(rest[ok]) == 0) {
        note <- "the mean of the other items has no variance"
      } else {
        r <- stats::cor(x[ok], rest[ok])
      }
    }
    data.frame(
      item = it[i], label = labels[i], construct = construct[i],
      construct_shown = if (unassigned[i]) "(no construct)" else construct[i],
      unassigned = unassigned[i], n_peers = length(peers), n = n, item_total = r,
      flag = if (!is.na(r) && r < low) "low itc" else "", note = note,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(
      item = character(0), label = character(0), construct = character(0),
      construct_shown = character(0), unassigned = logical(0), n_peers = integer(0),
      n = integer(0), item_total = numeric(0), flag = character(0), note = character(0),
      stringsAsFactors = FALSE
    )
  }
  attr(out, "n_resp") <- nrow(d)
  out
}

#' The sentence above the item total table
#'
#' What was computed: on how many respondents and items, within which
#' constructs, how missing responses were handled, and which correlations
#' could not be computed and why.
#'
#' @param df The result of `sieve_screen_item_total()`.
#' @return A character string.
#' @author Ken Kelley
#' @noRd
sieve_screen_item_total_note <- function(df) {
  n_resp <- attr(df, "n_resp") %||% NA_integer_
  k <- nrow(df)
  cons <- unique(df$construct[!df$unassigned])
  n_un <- sum(df$unassigned)
  within <- if (!length(cons)) {
    "No construct is assigned, so each item is correlated with the mean of all other included items."
  } else {
    paste0(
      sprintf(
        "Totals are formed within %d construct%s (%s)", length(cons), if (length(cons) == 1L) "" else "s",
        paste(cons, collapse = ", ")
      ),
      if (n_un) {
        sprintf(
          paste0(
            "; the %d item%s with no construct %s totaled together, each correlated with the ",
            "mean of the other included items that have no construct."
          ),
          n_un, if (n_un == 1L) "" else "s", if (n_un == 1L) "is" else "are"
        )
      } else "."
    )
  }
  missing <- sprintf(paste0(
    "Computed on the %d respondents in the analysis: the total is the mean of the ",
    "peer items each respondent answered, and each correlation uses the respondents ",
    "with both values (n in the table)."
  ), n_resp)
  undefined <- sum(is.na(df$item_total))
  why <- if (undefined) {
    reasons <- table(df$note[is.na(df$item_total)])
    sprintf(
      " %d correlation%s could not be computed (%s); the Note column says why for each.",
      undefined, if (undefined == 1L) "" else "s",
      paste(sprintf("%s: %d", names(reasons), as.integer(reasons)), collapse = "; ")
    )
  } else {
    ""
  }
  sprintf("%d included item%s. %s %s%s", k, if (k == 1L) "" else "s", within, missing, why)
}

# ---- The heatmap -----------------------------------------------------------

#' The size of the correlation heatmap, from the item count
#'
#' The plotting region is square: `14 n` px at least (so a tile is never
#' smaller than 14 px on screen, above the 12 px a tile needs to be seen),
#' `40 n` px for a small pool, and 700 px between. With the correlations
#' printed in the tiles the floor is `28 n` px, so a two decimal value fits
#' its tile at a legible size. The margins hold the
#' axis labels (left and bottom), the color key (right) and a little air
#' (top). The same geometry in inches, at 96 dots per inch, sizes the
#' downloads, so a figure written at 300 dots per inch has the same layout
#' as the screen.
#'
#' @param n The number of items.
#' @param dpi The screen resolution the pixel sizes assume.
#' @param values Whether the correlations are printed in the tiles.
#' @return A list: `n`, `plot` (the side of the plotting region, px),
#'   `cell` (the side of a tile, px), `width` and `height` (px), `width_in`
#'   and `height_in` (inches), and `mai` (the margins in inches, bottom,
#'   left, top, right, as [graphics::par()] takes them).
#' @author Ken Kelley
#' @noRd
sieve_screen_heatmap_size <- function(n, dpi = 96, values = FALSE) {
  n <- max(1L, as.integer(n))
  plot <- max((if (isTRUE(values)) 28L else 14L) * n, min(700L, 40L * n))
  margins <- c(bottom = 190L, left = 190L, top = 30L, right = 90L)
  width <- plot + margins[["left"]] + margins[["right"]]
  height <- plot + margins[["bottom"]] + margins[["top"]]
  list(
    n = n, plot = plot, cell = plot / n, width = width, height = height,
    width_in = width / dpi, height_in = height / dpi, mai = unname(margins) / dpi
  )
}

#' Axis labels cut to a width, ending in three periods
#'
#' Three periods rather than the ellipsis character, which the PDF device's
#' default font encoding cannot write (it substitutes, with a warning).
#'
#' @param x Labels.
#' @param width The number of characters kept, the periods included.
#' @return Character, the same length as `x`; `NA` becomes `""`.
#' @author Ken Kelley
#' @noRd
sieve_screen_short_label <- function(x, width = 24L) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  ifelse(nchar(x) > width, paste0(substr(x, 1L, max(1L, width - 3L)), "..."), x)
}

#' Draw a Correlation Heatmap of an Item Pool
#'
#' Draws an inter item correlation matrix as a heatmap with base graphics on
#' the current device: one tile per pair, colored on a diverging scale from
#' -1 (red) through 0 (white) to 1 (the lead color of the palette), with the
#' items in the order of the matrix on both axes. This is the figure the
#' Screening tab shows and writes, and the R that SIEVER records for a
#' download calls this function on the same matrix.
#'
#' @details
#' Separators are drawn where the construct changes (a heavy line) and
#' where the facet changes (a light line), read from `groups`, so the block
#' structure of a pool ordered by construct and facet can be read from the
#' figure. With `highlight` above 0, every off diagonal tile whose absolute
#' correlation is below it is drawn gray and the others keep their color;
#' nothing is recomputed. The coefficients are printed in the tiles, to two
#' decimals, when `values` is `TRUE`, each in black or white, whichever has
#' the higher contrast with its tile (the relative luminance of WCAG 2). For
#' a large pool the printed values are small unless the device is large: at
#' `n * 28 / 96 + 2.92` by `n * 28 / 96 + 2.29` inches a tile is 28 pixels
#' at 96 dots per inch, which holds a two decimal value. The
#' margins are set by the function (about two inches at the left and the
#' bottom for the labels, and room for the color key at the right) and
#' restored on exit, so a tile is the plotting region divided by the number
#' of items: on a device `n * 14 / 96 + 2.92` inches wide and
#' `n * 14 / 96 + 2.29` inches high, a tile is 14 pixels at 96 dots per
#' inch, whatever the resolution the device writes at. A tile whose
#' correlation is `NA` is left blank.
#'
#' @param R A square numeric correlation matrix with the items as its
#'   dimnames.
#' @param labels Axis labels: a character vector in the order of `R`, a
#'   vector named by item, or `NULL` for the dimnames of `R`. A label
#'   longer than `label_width` characters is cut and ends in three periods.
#' @param groups `NULL`, or a data frame with one row per item in the order
#'   of `R` and the columns `construct` and `facet` (either may be absent).
#' @param highlight A number from 0 to 1. Off diagonal tiles with an
#'   absolute correlation below it are drawn gray; 0 (the default) grays
#'   nothing.
#' @param palette The embedded palette whose lead color is the positive end
#'   of the scale: `"nd"` (the default), `"okabe_ito"`, or `"viridis"`.
#' @param values Whether to print the coefficients in the tiles: `FALSE`
#'   (the default) or `TRUE`. `NULL` prints them for 16 items or fewer.
#' @param label_width The number of characters an axis label is cut to.
#' @param key Whether to draw the color key at the right.
#' @param boxes Whether to box the diagonal blocks: each construct's
#'   items with each other (a heavy dark box) and each facet's items with
#'   each other (a gold box inside it), so the within-construct and
#'   within-facet correlations stand out from those between blocks. A block
#'   is a run of adjacent items with the same construct (or the same
#'   construct and facet), read from `groups`; the separators are drawn as
#'   well, all of them light, so that the boxes stand out.
#' @return Invisibly, a list: `n` (the number of items), `items` (their
#'   names in the order drawn), `labels` (the axis labels as drawn),
#'   `highlight`, `faded` (the number of off diagonal tiles drawn gray),
#'   `separators` (a data frame with `after`, the number of items before the
#'   line, and `kind`, `"construct"` or `"facet"`), `values` (whether the
#'   coefficients were printed), `text` (`NULL`, or a matrix of the
#'   printed text with the attribute `color`, the text color of each tile),
#'   and `boxes` (a data frame with `kind`, `"construct"` or `"facet"`,
#'   `from` and `to`, the first and last item of each block boxed, and
#'   `name`; no rows unless `boxes` is `TRUE`).
#' @author Ken Kelley
#' @examples
#' R <- stats::cor(attitude)
#' groups <- data.frame(
#'   construct = rep(c("Climate", "Advancement"), c(4, 3)),
#'   facet = c(
#'     "Overall", "Overall", "Treatment", "Treatment",
#'     "Pay", "Pay", "Prospects"
#'   )
#' )
#' drawn <- sieve_correlation_heatmap(R, groups = groups, highlight = 0.5)
#' drawn$separators
#' # The coefficients in the tiles, on a device large enough to read them.
#' grDevices::pdf(tempfile(fileext = ".pdf"),
#'   width = 7 * 28 / 96 + 2.92,
#'   height = 7 * 28 / 96 + 2.29
#' )
#' drawn <- sieve_correlation_heatmap(R, groups = groups, values = TRUE)
#' grDevices::dev.off()
#' drawn$text
#' # The within-construct and within-facet blocks boxed.
#' drawn <- sieve_correlation_heatmap(R, groups = groups, boxes = TRUE)
#' drawn$boxes
#' @export
sieve_correlation_heatmap <- function(R, labels = NULL, groups = NULL, highlight = 0,
                                      palette = "nd", values = FALSE, label_width = 24L,
                                      key = TRUE, boxes = FALSE) {
  if (!is.matrix(R) || !is.numeric(R) || nrow(R) != ncol(R) || nrow(R) < 1L) {
    stop("R must be a square numeric correlation matrix.", call. = FALSE)
  }
  n <- nrow(R)
  items <- rownames(R) %||% colnames(R) %||% sprintf("item_%d", seq_len(n))
  if (is.null(labels)) {
    labels <- items
  } else if (!is.null(names(labels))) {
    got <- as.character(labels)[match(items, names(labels))]
    got[is.na(got)] <- items[is.na(got)]
    labels <- got
  }
  labels <- as.character(labels)
  if (length(labels) != n) {
    stop("labels must have one entry per item of R.", call. = FALSE)
  }
  highlight <- min(1, max(0, sieve_finite_or(highlight, 0)))
  if (is.null(values)) {
    values <- n <= 16L
  }
  short <- sieve_screen_short_label(labels, label_width)
  lead <- sieve_palette_colors(palette, 1L)[1]
  ramp <- grDevices::colorRampPalette(c("#b2182b", "#ffffff", lead))(101)
  gray <- "#e4e4e4"

  # One color index per tile: 1 to 101 along the scale, 102 for gray.
  r <- pmin(pmax(R, -1), 1)
  idx <- round((r + 1) * 50) + 1
  off <- row(R) != col(R)
  faded <- highlight > 0 & !is.na(R) & abs(R) < highlight & off
  idx[faded] <- 102L
  # Item i is the i-th row from the top and the i-th column from the left.
  z <- t(idx)[, rev(seq_len(n)), drop = FALSE]

  size <- sieve_screen_heatmap_size(n)
  op <- graphics::par(mai = size$mai)
  on.exit(graphics::par(op), add = TRUE)
  graphics::image(seq_len(n), seq_len(n), z,
    col = c(ramp, gray), zlim = c(0.5, 102.5),
    axes = FALSE, xlab = "", ylab = "", useRaster = FALSE
  )
  cex <- sieve_screen_heatmap_cex(n)
  graphics::axis(1,
    at = seq_len(n), labels = short, las = 2, cex.axis = cex, tick = FALSE,
    line = -0.4
  )
  graphics::axis(2,
    at = rev(seq_len(n)), labels = short, las = 1, cex.axis = cex, tick = FALSE,
    line = -0.4
  )

  # Separators: a light line where the facet changes, a heavy one where the
  # construct changes (which takes precedence at the same place). With the
  # boxes every separator is light, so the boxed blocks stand out.
  separators <- sieve_screen_separators(groups, n)
  for (k in seq_len(nrow(separators))) {
    heavy <- identical(separators$kind[k], "construct") && !isTRUE(boxes)
    pos <- separators$after[k] + 0.5
    graphics::abline(
      v = pos, h = n - pos + 1, col = if (heavy) "#1e1913" else "#8c8c8c",
      lwd = if (heavy) 2.2 else 1, xpd = FALSE
    )
  }

  # The diagonal blocks, boxed: the facets first, so a construct's box is
  # drawn over the edges it shares with its facets.
  blocks <- if (isTRUE(boxes)) sieve_screen_blocks(groups, n) else sieve_screen_blocks(NULL, n)
  for (k in c(which(blocks$kind == "facet"), which(blocks$kind == "construct"))) {
    heavy <- identical(blocks$kind[k], "construct")
    graphics::rect(
      blocks$from[k] - 0.5, n - blocks$to[k] + 0.5, blocks$to[k] + 0.5, n - blocks$from[k] + 1.5,
      border = if (heavy) "#1e1913" else "#d39f10", lwd = if (heavy) 3 else 2, xpd = FALSE
    )
  }

  text <- NULL
  if (isTRUE(values)) {
    txt <- sub("^(-?)0\\.", "\\1.", formatC(R, format = "f", digits = 2))
    txt[txt == "-.00"] <- ".00"
    txt[is.na(R)] <- ""
    # Black or white text, whichever contrasts more with the tile's color.
    fill <- c(ramp, gray)[idx]
    fill[is.na(fill)] <- "#ffffff"
    ink <- matrix(sieve_screen_text_color(fill), n, n)
    graphics::text(col(R), n - row(R) + 1, labels = txt, cex = cex * 0.85, col = ink)
    text <- txt
    attr(text, "color") <- ink
  }

  if (isTRUE(key)) {
    usr <- graphics::par("usr")
    span <- usr[2] - usr[1]
    x0 <- usr[2] + span * 0.025
    x1 <- x0 + span * 0.035
    ys <- seq(usr[3], usr[4], length.out = 102L)
    graphics::rect(x0, ys[-102L], x1, ys[-1L], col = ramp, border = NA, xpd = NA)
    graphics::rect(x0, usr[3], x1, usr[4], border = "#8c8c8c", xpd = NA)
    graphics::text(x1, c(usr[3], (usr[3] + usr[4]) / 2, usr[4]), c("-1", "0", "1"),
      pos = 4, cex = 0.8, xpd = NA
    )
    if (highlight > 0) {
      graphics::rect(x0, usr[3] - span * 0.07, x1, usr[3] - span * 0.04,
        col = gray,
        border = "#8c8c8c", xpd = NA
      )
      graphics::text(x1, usr[3] - span * 0.055, sprintf("|r| < %.2f", highlight),
        pos = 4,
        cex = 0.7, xpd = NA
      )
    }
  }

  invisible(list(
    n = n, items = items, labels = short, highlight = highlight,
    faded = sum(faded), separators = separators, values = isTRUE(values), text = text,
    boxes = blocks
  ))
}

#' The diagonal blocks of a correlation matrix ordered by construct and facet
#'
#' A block is a run of adjacent items with the same construct (a construct
#' block) or the same construct and facet (a facet block, so a facet name
#' used in two constructs makes two blocks). Items without a construct, or
#' without a facet, form no block of that kind.
#'
#' @param groups `NULL`, or a data frame with one row per item and the
#'   columns `construct` and `facet` (either may be absent).
#' @param n The number of items.
#' @return A data frame with `kind` (`"construct"` or `"facet"`), `from`,
#'   `to` (the first and last item of the block, by position) and `name`.
#' @author Ken Kelley
#' @noRd
sieve_screen_blocks <- function(groups, n) {
  out <- data.frame(kind = character(0), from = integer(0), to = integer(0), name = character(0),
    stringsAsFactors = FALSE)
  if (!is.data.frame(groups) || nrow(groups) != n || n < 1L) {
    return(out)
  }
  text_of <- function(x) {
    x <- if (is.null(x)) rep("", n) else as.character(x)
    x[is.na(x)] <- ""
    trimws(x)
  }
  construct <- text_of(groups$construct)
  facet <- text_of(groups$facet)
  runs <- function(key, kind, shown) {
    r <- rle(key)
    to <- cumsum(r$lengths)
    from <- to - r$lengths + 1L
    keep <- nzchar(r$values)
    if (!any(keep)) {
      return(NULL)
    }
    data.frame(kind = kind, from = as.integer(from[keep]), to = as.integer(to[keep]),
      name = shown[from[keep]], stringsAsFactors = FALSE)
  }
  facet_key <- ifelse(nzchar(facet), paste(construct, facet, sep = "\r"), "")
  rbind(out, runs(construct, "construct", construct), runs(facet_key, "facet", facet))
}

#' The text color that contrasts more with a fill color
#'
#' Black (`"#1e1913"`, the app's text color) or white, whichever has the
#' higher contrast ratio with the fill, from the relative luminance of
#' WCAG 2: `L = 0.2126 R + 0.7152 G + 0.0722 B` on the linearized channels,
#' and the ratio `(L1 + 0.05) / (L2 + 0.05)`.
#'
#' @param fill Colors, as [grDevices::col2rgb()] reads them.
#' @return A character vector of `"#1e1913"` and `"#ffffff"`.
#' @author Ken Kelley
#' @noRd
sieve_screen_text_color <- function(fill) {
  lum <- function(cols) {
    rgb <- grDevices::col2rgb(cols) / 255
    lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
    as.vector(c(0.2126, 0.7152, 0.0722) %*% lin)
  }
  l_fill <- lum(fill)
  l_dark <- lum("#1e1913")
  on_white <- (1 + 0.05) / (l_fill + 0.05)
  on_dark <- (l_fill + 0.05) / (l_dark + 0.05)
  ifelse(on_white > on_dark, "#ffffff", "#1e1913")
}

#' The correlation of one pair of items, in words
#'
#' @param cm The module's `cor_mat()` (the matrix of the included items,
#'   the kind of correlation, and the complete cases it was computed on).
#' @param a,b Two item (variable) names.
#' @param info The module's `item_info()` (for the item names).
#' @return A list with `r` (full precision, `NA` when the pair is not in the
#'   matrix), `kind`, `n`, and `text` (one sentence: the value to three
#'   decimals, the kind of correlation and the n).
#' @author Ken Kelley
#' @noRd
sieve_screen_pair <- function(cm, a, b, info) {
  name <- function(v) {
    lab <- info$label[match(v, info$item)]
    if (length(lab) && !is.na(lab)) lab else v
  }
  inside <- is.matrix(cm$R) && all(c(a, b) %in% rownames(cm$R))
  r <- if (inside) cm$R[a, b] else NA_real_
  pearson <- identical(cm$kind, "pearson")
  kind <- if (pearson) "Pearson correlation" else sprintf("%s correlation", cm$kind)
  how <- if (pearson) "computed on the observed item scores" else "estimated from the ordered responses (psych)"
  text <- if (!inside) {
    "One of the two items is not in the correlation matrix of the included items."
  } else if (identical(a, b)) {
    sprintf("r = 1: an item with itself (%s, %s).", kind, how)
  } else if (is.na(r)) {
    sprintf("The %s of %s with %s is not defined.", kind, name(a), name(b))
  } else {
    sprintf(
      "r = %s: the %s of %s with %s, %s; n = %d respondents who answered every included item (complete cases, of %d).",
      sub("^(-?)0\\.", "\\1.", sprintf("%.3f", r)), kind, name(a), name(b), how, as.integer(cm$n),
      as.integer(cm$n_all)
    )
  }
  list(r = r, kind = cm$kind, n = cm$n, text = text)
}

#' The character expansion of the heatmap's axis labels
#'
#' Scaled so a label's height is about four fifths of a tile, from the
#' plotting region of the open device, and kept between 0.45 and 1.
#'
#' @param n The number of items.
#' @param along Which side of the plotting region holds the `n` tiles:
#'   `"width"` (the heatmap) or `"height"` (the missingness bars).
#' @return A number for `cex`.
#' @author Ken Kelley
#' @noRd
sieve_screen_heatmap_cex <- function(n, along = c("width", "height")) {
  along <- match.arg(along)
  side <- graphics::par("pin")[if (identical(along, "width")) 1L else 2L]
  cell_in <- side / max(1L, n)
  ps <- graphics::par("ps")
  max(0.45, min(1, 0.8 * cell_in * 72 / ps))
}

#' Where the construct and facet separators of an item sequence fall
#'
#' @param groups `NULL`, or a data frame with one row per item and the
#'   columns `construct` and `facet` (either may be absent).
#' @param n The number of items.
#' @return A data frame with `after` (the number of items before the line)
#'   and `kind` (`"construct"` or `"facet"`; a construct line takes the
#'   place of a facet line at the same position), ordered by position.
#' @author Ken Kelley
#' @noRd
sieve_screen_separators <- function(groups, n) {
  separators <- data.frame(after = integer(0), kind = character(0), stringsAsFactors = FALSE)
  if (!is.data.frame(groups) || nrow(groups) != n || n < 2L) {
    return(separators)
  }
  for (kind in c("facet", "construct")) {
    if (kind %in% names(groups)) {
      g <- as.character(groups[[kind]])
      g[is.na(g)] <- ""
      change <- which(g[-1L] != g[-n])
      if (length(change)) {
        separators <- rbind(
          separators,
          data.frame(after = change, kind = kind, stringsAsFactors = FALSE)
        )
      }
    }
  }
  separators <- separators[!duplicated(separators$after, fromLast = TRUE), , drop = FALSE]
  separators <- separators[order(separators$after), , drop = FALSE]
  rownames(separators) <- NULL
  separators
}

#' Draw the percent missing of every item, one bar each, sized like the heatmap
#'
#' The first item is at the top, the bars are the height of a heatmap tile
#' when the device has the heatmap's height, the labels sit in the
#' heatmap's left margin, and the separators between constructs (heavy)
#' and facets (light) are the heatmap's.
#'
#' @param pct Percent missing per item.
#' @param labels One label per item.
#' @param groups `NULL`, or a data frame with `construct` and `facet` per item.
#' @param palette The embedded palette whose lead color fills the bars.
#' @return Invisibly, a list: `n`, `upper` (the axis limit), and
#'   `separators` (see `sieve_screen_separators()`).
#' @author Ken Kelley
#' @noRd
sieve_screen_draw_missing <- function(pct, labels, groups = NULL, palette = "nd") {
  n <- length(pct)
  col <- sieve_palette_colors(palette, 1L)[1]
  size <- sieve_screen_heatmap_size(n)
  op <- graphics::par(mai = c(0.6, size$mai[2], 0.3, 0.4))
  on.exit(graphics::par(op), add = TRUE)
  top <- suppressWarnings(max(pct, na.rm = TRUE))
  upper <- if (is.finite(top)) max(5, ceiling(top / 5) * 5) else 5
  y <- rev(seq_len(n))
  graphics::plot(NA,
    xlim = c(0, upper), ylim = c(0.5, n + 0.5), axes = FALSE,
    xlab = "Percent missing", ylab = "", yaxs = "i", xaxs = "i"
  )
  ticks <- pretty(c(0, upper))
  graphics::abline(v = ticks, col = "#eeeeee")
  shown <- !is.na(pct)
  graphics::rect(0, y[shown] - 0.4, pct[shown], y[shown] + 0.4, col = col, border = NA)
  cex <- sieve_screen_heatmap_cex(n, along = "height")
  graphics::axis(2, at = y, labels = labels, las = 1, tick = FALSE, cex.axis = cex, line = -0.4)
  graphics::axis(1, at = ticks)
  separators <- sieve_screen_separators(groups, n)
  for (k in seq_len(nrow(separators))) {
    heavy <- identical(separators$kind[k], "construct")
    graphics::abline(
      h = n - separators$after[k] + 0.5, col = if (heavy) "#1e1913" else "#8c8c8c",
      lwd = if (heavy) 2.2 else 1
    )
  }
  invisible(list(n = n, upper = upper, separators = separators))
}


# ---- The downloads ---------------------------------------------------------

#' The file name of a heatmap download
#'
#' `siever_correlations_<basis>.<ext>` for the whole pool, with the scope
#' appended (`_construct_<name>` or `_facet_<name>`, the name as a slug)
#' when the figure shows one block.
#'
#' @param kind The correlations used (`"pearson"`, `"polychoric"`, and so on).
#' @param scope A list with `kind` and `name` (see the module's `heat_scope`).
#' @param ext The extension.
#' @return A file name.
#' @author Ken Kelley
#' @noRd
sieve_screen_file_name <- function(kind, scope, ext) {
  suffix <- if (is.list(scope) && !identical(scope$kind, "all")) {
    sprintf("_%s_%s", scope$kind, sieve_screen_slug(scope$name))
  } else {
    ""
  }
  sprintf("siever_correlations_%s%s.%s", kind, suffix, ext)
}

#' A name as a file name slug: lower case, letters and digits, underscores between
#'
#' @param x A character string.
#' @return The slug, or `"unnamed"` when nothing remains.
#' @author Ken Kelley
#' @noRd
sieve_screen_slug <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!length(x) || is.na(x) || !nzchar(x)) "unnamed" else x
}

#' Open the device a heatmap download is written on
#'
#' PNG at 300 dots per inch or PDF, at the figure's size in inches (the
#' screen geometry, so the layout is the one on the screen). A device
#' function can fail with only a warning, and drawing would then go to
#' whatever device R opens by default; the number of devices is checked.
#'
#' @param format `"png"` or `"pdf"`.
#' @param file The file to write.
#' @param size The result of `sieve_screen_heatmap_size()`.
#' @return The device number, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_screen_open_device <- function(format, file, size) {
  opened <- sieve_open_device(format, file, size$width_in, size$height_in, title = "SIEVER correlation heatmap")
  invisible(opened$number)
}

#' A character vector as R source, a few values per line
#'
#' @param name The name to assign.
#' @param x The values.
#' @param per How many per line.
#' @return Lines of R.
#' @author Ken Kelley
#' @noRd
sieve_screen_chr_lines <- function(name, x, per = 4L) {
  lit <- sieve_chr_lit(x)
  if (length(lit) <= per) {
    return(sprintf("%s <- c(%s)", name, paste(lit, collapse = ", ")))
  }
  rows <- split(lit, ceiling(seq_along(lit) / per))
  body <- vapply(rows, function(r) paste0("  ", paste(r, collapse = ", ")), character(1))
  body[-length(body)] <- paste0(body[-length(body)], ",")
  c(sprintf("%s <- c(", name), body, ")")
}

#' The R that writes a heatmap download
#'
#' Reproduces the figure from `R_items`, the matrix the recorded basis step
#' computes on the included items, and `meta`, the ledger the recorded
#' script builds: the items shown in their order, their labels and groups,
#' the device at the figure's size, and the one drawing call.
#'
#' @param a The module's `heat_args()`.
#' @param size The result of `sieve_screen_heatmap_size()`.
#' @param name The file name.
#' @param format `"png"` or `"pdf"`.
#' @return Lines of R.
#' @author Ken Kelley
#' @noRd
sieve_screen_heatmap_code <- function(a, size, name, format) {
  items <- rownames(a$R)
  scope <- if (is.list(a$scope) && !identical(a$scope$kind, "all")) {
    sprintf(" (%s %s)", a$scope$kind, a$scope$name)
  } else {
    ""
  }
  highlight <- paste0(
    if (a$highlight > 0) sprintf(", |r| below %.2f grayed", a$highlight) else "",
    if (isTRUE(a$values)) ", the correlations printed in the cells" else "",
    if (isTRUE(a$boxes)) ", the within-construct and within-facet blocks boxed" else ""
  )
  # The size is written as the screen pixels over 96, exactly the numbers
  # the download uses, so the file the script writes has the same pixels.
  device <- switch(format,
    png = sprintf(
      "grDevices::png(\"%s\", width = %d / 96, height = %d / 96, units = \"in\", res = 300, bg = \"white\")",
      name, as.integer(size$width), as.integer(size$height)
    ),
    pdf = sprintf(
      "grDevices::pdf(\"%s\", width = %d / 96, height = %d / 96, bg = \"white\", useDingbats = FALSE)",
      name, as.integer(size$width), as.integer(size$height)
    )
  )
  c(
    sprintf(
      "# The correlation heatmap of the %d items shown%s, ordered by construct then facet%s,",
      length(items),
      scope,
      highlight
    ),
    sprintf(
      "# written as %s at the size the screen draws it (%.2f by %.2f inches%s).", toupper(format),
      size$width_in, size$height_in, if (identical(format, "png")) ", 300 dots per inch" else ""
    ),
    sieve_screen_chr_lines("heat_items", items),
    "heat_labels <- stats::setNames(meta$label, meta$item)[heat_items]",
    "heat_groups <- meta[match(heat_items, meta$item), c(\"construct\", \"facet\")]",
    device,
    sprintf(
      "SIEVER::sieve_correlation_heatmap(R_items[heat_items, heat_items], labels = heat_labels, groups = heat_groups, highlight = %s, palette = \"%s\", values = %s%s)",
      format(a$highlight), a$palette, if (isTRUE(a$values)) "TRUE" else "FALSE",
      if (isTRUE(a$boxes)) ", boxes = TRUE" else ""
    ),
    "grDevices::dev.off()"
  )
}

#' The correlation table's boxes, as a DataTables draw callback
#'
#' Marks, in the table, the blocks the heatmap boxes
#' (`sieve_screen_blocks()`): a cell of two items in one construct is
#' shaded and bordered at the construct block's edges (dark), a cell of two
#' items in one facet more strongly, at the facet block's edges (gold). The
#' edges are read from the rows as they are displayed, so a sorted or a
#' paged table keeps its blocks correct.
#'
#' @param groups The `construct` and `facet` of each item, in the order of
#'   the table's columns (and of its rows as they were given).
#' @return A [DT::JS()] function for the `drawCallback` option.
#' @author Ken Kelley
#' @noRd
sieve_screen_table_boxes <- function(groups) {
  text_of <- function(x) {
    x <- as.character(x %||% rep("", nrow(groups)))
    x[is.na(x)] <- ""
    trimws(x)
  }
  construct <- text_of(groups$construct)
  facet <- text_of(groups$facet)
  facet_key <- ifelse(nzchar(facet), paste(construct, facet, sep = "\r"), "")
  # Each block as a whole number (0 for none), so no name reaches the code.
  codes <- function(key) {
    id <- match(key, unique(key[nzchar(key)]))
    id[is.na(id)] <- 0L
    sprintf("[%s]", paste(as.integer(id), collapse = ","))
  }
  DT::JS(sprintf(paste0(
    "function(settings) {",
    "  var api = this.api(); var cons = %s; var facs = %s;",
    "  var rows = api.rows({page: 'current'}).indexes().toArray();",
    "  var nodes = api.rows({page: 'current'}).nodes().toArray();",
    "  for (var r = 0; r < nodes.length; r++) {",
    "    var i = rows[r], up = r > 0 ? rows[r - 1] : -1, down = r < rows.length - 1 ? rows[r + 1] : -1;",
    "    var cells = nodes[r].getElementsByTagName('td');",
    "    for (var c = 1; c < cells.length; c++) {",
    "      var j = c - 1, s = cells[c].style;",
    "      s.borderTop = s.borderBottom = s.borderLeft = s.borderRight = ''; s.backgroundColor = '';",
    "      var inC = cons[i] > 0 && cons[i] === cons[j], inF = facs[i] > 0 && facs[i] === facs[j];",
    "      if (inF) { s.backgroundColor = 'rgba(211, 159, 16, 0.16)'; } else if (inC) { s.backgroundColor = 'rgba(12, 35, 64, 0.06)'; }",
    "      var edge = function(key, line) {",
    "        if (up < 0 || key[up] !== key[j]) s.borderTop = line;",
    "        if (down < 0 || key[down] !== key[j]) s.borderBottom = line;",
    "        if (j === 0 || key[j - 1] !== key[i]) s.borderLeft = line;",
    "        if (j === key.length - 1 || key[j + 1] !== key[i]) s.borderRight = line;",
    "      };",
    "      if (inF) edge(facs, '2px solid #d39f10');",
    "      if (inC) edge(cons, '2px solid #1e1913');",
    "    }",
    "  }",
    "}"
  ), codes(construct), codes(facet_key)))
}

#' The header comment of a CSV download
#'
#' @param cm The module's `cor_mat()`.
#' @param a The module's `heat_args()`.
#' @return One line, starting with `#`.
#' @author Ken Kelley
#' @noRd
sieve_screen_csv_header <- function(cm, a) {
  scope <- if (is.list(a$scope) && !identical(a$scope$kind, "all")) {
    sprintf(", %s %s", a$scope$kind, a$scope$name)
  } else {
    ""
  }
  sprintf(
    paste0(
      "# SIEVER: %s correlations among %d included items%s (complete cases, n = %d of %d); ",
      "rows and columns in the order shown, diagonal included."
    ),
    sieve_retention_basis_label(cm$kind), nrow(a$R), scope, cm$n, cm$n_all
  )
}

#' The R that writes a CSV download
#'
#' @param a The module's `heat_args()`.
#' @param name The file name.
#' @param header The header comment.
#' @return Lines of R.
#' @author Ken Kelley
#' @noRd
sieve_screen_csv_code <- function(a, name, header) {
  c(
    "# The correlation matrix as shown, with its diagonal, written as CSV after a",
    "# header comment that names the correlations.",
    sieve_screen_chr_lines("heat_items", rownames(a$R)),
    sprintf("csv_file <- file(\"%s\", open = \"w\", encoding = \"UTF-8\")", name),
    sprintf("writeLines(%s, csv_file)", sieve_chr_lit(header)),
    "utils::write.csv(R_items[heat_items, heat_items], csv_file)",
    "close(csv_file)"
  )
}

# ---- Small multiples -------------------------------------------------------

#' The grid of the small multiples, from the item count
#'
#' @param n The number of items (at most 20).
#' @return A list: `rows`, `cols`, and `height` in px (190 px per row).
#' @author Ken Kelley
#' @noRd
sieve_screen_multiples_layout <- function(n) {
  n <- max(1L, as.integer(n))
  cols <- min(4L, n)
  rows <- as.integer(ceiling(n / cols))
  list(rows = rows, cols = cols, height = rows * 190L + 10L)
}

#' Draw the response distribution of every item, one small plot each
#'
#' Counts per response category for an ordered or binary item (or any item
#' with ten or fewer distinct values), a histogram otherwise, on the
#' current device, in the grid of `sieve_screen_multiples_layout()`.
#'
#' @param d A data frame of numeric item responses.
#' @param labels The title of each plot, one per column.
#' @param types The confirmed type of each column.
#' @param palette The embedded palette whose lead color fills the bars.
#' @return Invisibly, the names of the columns drawn.
#' @author Ken Kelley
#' @noRd
sieve_screen_draw_multiples <- function(d, labels, types, palette = "nd") {
  n <- ncol(d)
  lay <- sieve_screen_multiples_layout(n)
  col <- sieve_palette_colors(palette, 1L)[1]
  op <- graphics::par(
    mfrow = c(lay$rows, lay$cols), mar = c(2.6, 3.4, 2.2, 0.6),
    mgp = c(2.1, 0.6, 0), cex.axis = 0.85
  )
  on.exit(graphics::par(op), add = TRUE)
  for (i in seq_len(n)) {
    x <- d[[i]]
    x <- x[!is.na(x)]
    title <- labels[i]
    if (!length(x)) {
      graphics::plot.new()
      graphics::title(main = title, cex.main = 0.95)
      graphics::text(0.5, 0.5, "No non missing values", cex = 0.9)
      next
    }
    categorical <- isTRUE(types[i] %in% c("ordered", "binary")) || length(unique(x)) <= 10L
    if (categorical) {
      counts <- table(factor(x, levels = sort(unique(x))))
      graphics::barplot(counts,
        col = col, border = "white", main = title, cex.main = 0.95,
        ylab = "Count", las = 1
      )
    } else {
      graphics::hist(x,
        breaks = 20, col = col, border = "white", main = title, cex.main = 0.95,
        xlab = "", ylab = "Count", las = 1
      )
    }
  }
  invisible(names(d))
}

#' Skewness of a numeric vector, preferring DMAR then psych then base
#' @noRd
#' @author Ken Kelley
sieve_screen_skew <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3) return(NA_real_)
  {
    v <- tryCatch(DMAR::skewness(x), error = function(e) NULL)
    if (!is.null(v)) return(as.numeric(v))
  }
  {
    v <- tryCatch(psych::skew(x), error = function(e) NULL)
    if (!is.null(v)) return(as.numeric(v))
  }
  m <- mean(x)
  s <- stats::sd(x)
  if (is.na(s) || s == 0) return(NA_real_)
  mean(((x - m) / s)^3)
}

#' Excess kurtosis of a numeric vector, preferring DMAR then psych then base
#' @noRd
#' @author Ken Kelley
sieve_screen_kurt <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 4) return(NA_real_)
  {
    v <- tryCatch(DMAR::kurtosis(x), error = function(e) NULL)
    if (!is.null(v)) return(as.numeric(v))
  }
  {
    v <- tryCatch(psych::kurtosi(x), error = function(e) NULL)
    if (!is.null(v)) return(as.numeric(v))
  }
  m <- mean(x)
  s <- stats::sd(x)
  if (is.na(s) || s == 0) return(NA_real_)
  mean(((x - m) / s)^4) - 3
}

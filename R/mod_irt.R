# mod_irt.R
# Item response theory for ordered categorical items. Samejima's (1969)
# graded response model is fit as the single factor categorical factor
# analysis model it provably is (Takane and de Leeuw, 1987; Kamata and Bauer,
# 2008), then read as item discriminations and category boundary locations,
# and finally as the item and test information functions that follow from
# them. Every number comes from DMAR (irt_grm, irt_information,
# plot_irt_information); SIEVER adds no estimation of its own.
#
# Color. The item information figure is DMAR::plot_irt_information() with a
# SIEVER scale_color_manual() added on top, which replaces DMAR's Okabe-Ito
# discrete scale, so the DMAR geometry is reused and the app's palette
# governs the color. The test information figure is drawn here with ggplot2
# from the columns and attributes irt_information() returns, because DMAR
# sets its two line colors as fixed aesthetics (geom_line(color = ...))
# rather than through a scale, and a scale added afterward cannot override a
# fixed aesthetic. ggplot2 is imported, so neither figure has a base
# graphics fallback.

#' Item Response Theory Module UI
#'
#' The graded response model panel: item parameters in the factor analytic
#' and item response theory parameterizations side by side, the item and
#' test information functions, and a precision coverage readout.
#'
#' @param id Module id.
#' @return A [bslib::layout_sidebar()] page fragment.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput selectInput radioButtons numericInput helpText
#' @importFrom shiny plotOutput div p strong em tags hr
#' @importFrom bslib layout_sidebar sidebar card card_header card_body
#' @importFrom bslib navset_card_tab nav_panel accordion accordion_panel
#' @importFrom DT DTOutput
mod_irt_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 330,
      title = "Item response theory",
      div(
        class = "text-muted small",
        "Samejima's graded response model for ordered categorical items, with ",
        "the item and test information functions. The model is unidimensional ",
        "by construction, so it is fit within one construct at a time."
      ),
      selectInput(ns("scope"), "Items to model", choices = NULL),
      helpText(
        class = "small text-muted",
        "The graded response model here has one latent variable. Fitting it ",
        "across constructs would force items that belong to different ",
        "constructs onto a single dimension, so a construct is the default ",
        "scope; the combined option is available when the item set really is ",
        "intended as one scale."
      ),
      sieve_settings("Metric, Estimation and Information Grid",
        ids = ns(c("metric", "estimator", "theta_min", "theta_max", "theta_n")),
      radioButtons(
        ns("metric"), "Discrimination metric",
        choices = c(
          "Normal ogive (probit)" = "normal_ogive",
          "Logistic (a x 1.702)" = "logistic"
        ),
        selected = "normal_ogive"
      ),
      helpText(
        class = "small text-muted",
        "The conversion from the categorical factor analysis solution lands in ",
        "the normal ogive metric. Much item response theory software reports ",
        "logistic slopes, which are about 1.702 times larger for the same ",
        "items. The boundary locations b do not depend on the metric."
      ),
      selectInput(
        ns("estimator"), "Estimator",
        choices = c(
          "WLSMV (mean and variance adjusted)" = "WLSMV",
          "WLSM (mean adjusted)" = "WLSM",
          "DWLS (unadjusted test)" = "DWLS",
          "ULSMV (unweighted least squares)" = "ULSMV",
          "ULS (unweighted, unadjusted test)" = "ULS"
        ),
        selected = "WLSMV"
      ),
      helpText(
        class = "small text-muted",
        "WLSMV is SIEVER's default for ordered categorical items: ",
        "diagonally weighted least squares on the polychoric correlations ",
        "with a mean and variance adjusted test statistic."
      ),
      hr(),
      div(
        class = "d-flex gap-2 flex-wrap",
        numericInput(ns("theta_min"), "Theta from",
          value = -4, step = 0.5,
          width = "110px"
        ),
        numericInput(ns("theta_max"), "Theta to",
          value = 4, step = 0.5,
          width = "110px"
        ),
        numericInput(ns("theta_n"), "Grid points",
          value = 81, min = 11,
          max = 801, step = 10, width = "110px"
        )
      ),
      helpText(
        class = "small text-muted",
        "Theta is in standard normal units, so -4 to 4 covers essentially the ",
        "whole trait distribution. A finer grid locates the peak of the ",
        "information function more sharply."
      )
      ),
      numericInput(ns("info_threshold"), "Information threshold for coverage",
        value = 10, min = 0.1, max = 200, step = 1, width = "220px"
      ),
      helpText(
        class = "small text-muted",
        "The coverage band is the stretch of theta over which test ",
        "information reaches this value. Because SE(theta) = 1 / sqrt(I), a ",
        "threshold of 10 is the same statement as SE(theta) = 0.316. Set it ",
        "from the precision your study needs, not from convention."
      )
    ),
    sieve_optional_tab_note("the item response theory reading of the same categorical measurement model: item parameters, item information and test information"),
    # Results first: the one status line,
    # then, in the first panel, the one-line method statement with the fit
    # and the parameter table; how the factor analysis solution becomes
    # item response theory, and what the tab adds, below.
    uiOutput(ns("gate")),
    navset_card_tab(
      id = ns("panels"),
      nav_panel(
        "Item Parameters",
        card(
          card_header("Graded Response Model Item Parameters"),
          # A non-fillable body lays the note, the table and the explanation
          # out in ordinary document flow and scrolls when they do not fit.
          # In the default fillable body they compete for one flex column,
          # and a long explanation squeezes the table to zero height so the
          # htmlwidget never renders.
          card_body(
            fillable = FALSE,
            uiOutput(ns("model_note")),
            DTOutput(ns("param_tbl")),
            uiOutput(ns("model_code")),
            sieve_more_below(
              tags$p(
                tags$b("One model, two parameterizations."),
                " Samejima's (1969) graded response model and Muthen's (1984) ",
                "single factor categorical factor analysis model are the same ",
                "model written differently. Takane and de Leeuw (1987) proved ",
                "the equivalence and Kamata and Bauer (2008) give the algebra ",
                "item by item. Each observed response is a categorization of a ",
                "latent response variate at thresholds tau, and that variate ",
                "regresses on the latent trait with standardized loading ",
                "lambda. So the loading and the thresholds already contain the ",
                "item response theory parameters: ",
                tags$b("a = lambda / sqrt(1 - lambda^2)"), " and ",
                tags$b("b = tau / lambda"),
                ". Lambda and tau are shown beside a and b in the table so the ",
                "origin of every item response theory number is visible."
              ),
              tags$p(
                tags$b("What a and b mean."),
                " The boundary response function is P*(theta) = Phi[a (theta - ",
                "b)], the probability of responding above boundary k. So b is ",
                "the value of theta at which that probability reaches one half ",
                "(where on the trait the item's categories separate), and a is ",
                "the steepness of the separation. An item with m categories ",
                "has m - 1 boundaries, ordered b1 < b2 < ..., and one ",
                "discrimination shared across them. High a with boundaries ",
                "spread across the range is an item that carries information ",
                "widely; high a with tightly bunched boundaries is an item that ",
                "measures sharply in one narrow place."
              ),
              tags$p(
                tags$b("The constant 1.702."),
                " The conversion above gives a normal ogive slope, where the ",
                "boundary function is a normal cumulative distribution ",
                "function. Most item response theory software instead writes a ",
                "logistic boundary function, and the two agree to within 0.01 ",
                "in probability everywhere once the logistic argument is ",
                "stretched: |Phi(x) - Psi(1.702 x)| < 0.01 for every x, where ",
                "Psi is the standard logistic cumulative distribution function. ",
                "The value 1.702 is the constant that minimizes that maximum ",
                "discrepancy (Haley, 1952; Camilli, 1994). It multiplies the ",
                "slope and leaves the location alone, which is why b is ",
                "identical under the two metrics and a differs by a factor of ",
                "about 1.7. Both metrics are shown so a value read here can be ",
                "compared with a value reported elsewhere without arithmetic."
              ),
              tags$p(
                tags$b("Limited information estimation."),
                " lavaan estimates the thresholds and the polychoric ",
                "correlations and then fits the single factor model to those ",
                "correlations by (diagonally) weighted least squares. That ",
                "uses the univariate and bivariate margins of the response ",
                "table, whereas marginal maximum likelihood (the usual item ",
                "response theory route, as in mirt) uses the full response ",
                "pattern likelihood. The two are consistent for the same ",
                "population parameters and agree closely, but they are ",
                "different estimators, so numbers will not match to the last ",
                "decimal across software (Wirth & Edwards, 2007). The gain is ",
                "that ordinary factor analysis fit assessment comes along with ",
                "the item parameters, which is the fit table below."
              ),
              tags$p(
                class = "text-muted mb-0",
                tags$b("Read the fit before the parameters."),
                " The model here is unidimensional by construction. If it fits ",
                "poorly, the discriminations and locations are describing a ",
                "dimension the items do not share, and the information ",
                "function inherits that misspecification. A standardized ",
                "loading at or beyond one is an improper (Heywood) solution: ",
                "the implied discrimination is not finite and the item ",
                "parameters should not be interpreted until the model is ",
                "respecified."
              ),
              label = "More on the parameterization: what a and b mean, the constant 1.702, and limited information estimation"
            )
          )
        ),
        card(
          card_header("Fit of the Unidimensional Graded Response Model"),
          card_body(
            fillable = FALSE,
            uiOutput(ns("fit_note")),
            DTOutput(ns("fit_tbl")),
            uiOutput(ns("fit_code"))
          )
        ),
        sieve_irt_parameter_panel(ns)
      ),

      nav_panel(
        "Local Dependence and Response Traces",
        sieve_irt_dependence_panel(ns),
        sieve_irt_category_panel(ns)
      ),

      nav_panel(
        "Test Information",
        card(
          card_header("Test Information and the Standard Error of Theta"),
          card_body(
            fillable = FALSE,
            uiOutput(ns("test_info_note")),
            plotOutput(ns("test_info_plot"), height = "460px"),
            uiOutput(ns("test_info_code")),
            tags$div(
              class = "small mt-2",
              tags$p(
                tags$b("How to read the figure. "),
                "The solid curve is test information, the sum of the item ",
                "information functions. The dashed curve is the standard error of ",
                "the maximum likelihood estimate of theta, on the right hand ",
                "axis; the two are the same information twice, because ",
                tags$b("SE(theta) = 1 / sqrt(I(theta))"),
                ". Where information is high the standard error is small, and the ",
                "curves necessarily run in opposite directions. The dotted ",
                "vertical line marks where information peaks on the evaluation ",
                "grid, that is, where this item set estimates the trait most ",
                "precisely. A horizontal reference line and a shaded band mark ",
                "the coverage threshold set in the sidebar and the stretch of ",
                "theta that reaches it; when the threshold sits above everything ",
                "the curve attains, the line is omitted rather than allowed to ",
                "stretch the axis and flatten the curve, and the subtitle says so."
              ),
              tags$p(
                tags$b("Why this is not a reliability coefficient. "),
                "Omega and the classical standard error of measurement describe a ",
                "scale at one place on the continuum, implicitly the population ",
                "average. The information function drops that averaging. A scale ",
                "can have an excellent omega and still measure badly over the ",
                "range of the trait a particular study cares about: an item pool ",
                "whose boundaries all sit near the mean will look reliable in a ",
                "general sample and will fail to separate respondents in the upper ",
                "tail, which is exactly where a selection or screening ",
                "application needs the separation. This is the precision emphasis ",
                "of the accuracy in parameter estimation framework applied to the ",
                "person rather than to a parameter estimate: report where the ",
                "estimate is precise, not only that it is on average ",
                "(Kelley & Pornprasertmanit, 2016)."
              )
            )
          )
        )
      ),
      nav_panel(
        "Item Information",
        card(
          card_header("Item Information: Which Items Buy Precision, and Where"),
          card_body(
            fillable = FALSE,
            uiOutput(ns("item_info_note")),
            plotOutput(ns("item_info_plot"), height = "460px"),
            uiOutput(ns("item_info_code")),
            tags$div(
              class = "small mt-2",
              tags$p(
                tags$b("Information is additive across items. "),
                "Test information is the sum of these curves, with no covariance ",
                "term, because the graded response model assumes local ",
                "independence: conditional on theta, the item responses are ",
                "independent. That additivity is what makes the figure ",
                "actionable. Excluding an item subtracts its curve, so the cost of ",
                "excluding it can be read off directly and is different at ",
                "different places on the continuum. Two items with the same ",
                "discrimination contribute equally in total and unequally where ",
                "it matters, depending on where their category boundaries sit."
              ),
              tags$p(
                class = "text-muted mb-0",
                "Each curve peaks near the item's boundary locations and its ",
                "height there scales with the square of the discrimination, so a ",
                "steeper item is worth more but only over a narrower band. ",
                "Numerical peaks, and each item's share of information at the ",
                "test peak, are in the Precision Coverage panel."
              )
            )
          )
        )
      ),
      nav_panel(
        "Precision Coverage",
        card(
          card_header("Where the Scale Measures Well Enough"),
          card_body(
            fillable = FALSE,
            uiOutput(ns("coverage_note")),
            DTOutput(ns("coverage_tbl")),
            uiOutput(ns("coverage_code"))
          )
        ),
        card(
          card_header("Per Item Peak Information and Its Location"),
          card_body(
            fillable = FALSE,
            uiOutput(ns("peak_note")),
            DTOutput(ns("peak_tbl"))
          )
        )
      )
    ),
    p(
      class = "small text-muted mt-2",
      strong("What this tab adds. "),
      "A reliability coefficient is one number for a whole scale. The ",
      "information function is the same idea written as a function of ",
      "where the respondent sits on the latent continuum, so it answers a ",
      "question omega cannot: not ", em("how precise is this scale"),
      " but ", em("precise for whom"), ". Because information is additive ",
      "across items, the curve also decomposes into the items that produce ",
      "it, which is what makes it usable for deciding which items to keep ",
      "(Lord, 1980; Embretson & Reise, 2000)."
    )
  )
}

#' Item Response Theory Module Server
#'
#' @param id Module id.
#' @param state The shared application state.
#' @return Invisibly `NULL`; called for its side effects.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive req renderUI renderPlot observe
#' @importFrom shiny updateSelectInput div p strong tags tagList
#' @importFrom DT renderDT datatable formatRound
mod_irt_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    # ---- Scope: which items enter the unidimensional model ------------------

    active_meta <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$meta)
      m <- state$meta
      m[m$status == "active", , drop = FALSE]
    })

    construct_choices <- reactive({
      read <- sieve_outcome(active_meta())
      m <- if (read$ready && read$ok) read$value else NULL
      if (is.null(m) || !nrow(m)) {
        return(character(0))
      }
      unique(m$construct[sieve_construct_assigned(m$construct)])
    })

    observe({
      cons <- construct_choices()
      choices <- c(
        stats::setNames(cons, cons),
        stats::setNames("__all__", "All active items as one scale")
      )
      sel <- if ((input$scope %||% "") %in% choices) input$scope else choices[[1]]
      updateSelectInput(session, "scope", choices = choices, selected = sel)
    })

    # The selector may not have rendered yet (and does not exist at all under
    # shiny::testServer until it is set), so fall back to the first construct.
    scope_key <- reactive({
      cons <- construct_choices()
      s <- input$scope %||% ""
      if (nzchar(s) && (identical(s, "__all__") || s %in% cons)) {
        s
      } else if (length(cons)) {
        cons[1]
      } else {
        "__all__"
      }
    })

    scope_label <- reactive({
      k <- scope_key()
      if (identical(k, "__all__")) "all active items" else k
    })

    scope_items <- reactive({
      m <- active_meta()
      k <- scope_key()
      its <- if (identical(k, "__all__")) m$item else m$item[m$construct == k]
      intersect(its, names(state$items))
    })

    # The item values as a numeric data frame, read once and uncoerced
    # beyond that so nothing is silently rounded into a category it does not
    # belong to. A column that cannot be read as numbers is named in the
    # "problems" attribute (see sieve_numeric_columns()) and the gate below
    # refuses the model; it is never coerced to missing values quietly.
    scope_data <- reactive({
      its <- scope_items()
      req(length(its) > 0)
      read <- sieve_numeric_columns(state$items, its)
      d <- read$data
      attr(d, "problems") <- read$text
      d
    })

    # ---- The gate -----------------------------------------------------------
    # The graded response model is defined for ordered categorical responses.
    # Rather than let a continuous item reach lavaan and fail, say what the
    # model requires, what the user must change, and what would be wrong to
    # change. Returns a list: ok (logical) and ui (the alert, or NULL).

    irt_gate <- reactive({
      pipeline <- sieve_gate(state)
      if (!is.null(pipeline)) {
        return(list(ok = FALSE, ui = pipeline))
      }

      m <- active_meta()
      its <- scope_items()
      lbl <- scope_label()
      if (length(its) < 3L) {
        return(list(ok = FALSE, ui = div(
          class = "alert alert-warning",
          strong("At least three items are needed. "),
          sprintf(
            paste0(
              "The scope %s has %d included item%s. A single ",
              "latent variable is not identified by fewer than ",
              "three indicators. Include more items on the Items ",
              "tab, or choose a construct with three or more ",
              "included items in the sidebar."
            ),
            lbl, length(its), if (length(its) == 1L) "" else "s"
          )
        )))
      }
      types <- m$type[match(its, m$item)]
      not_ordered <- its[!types %in% c("ordered", "binary")]
      if (length(not_ordered)) {
        shown <- paste(utils::head(not_ordered, 12L), collapse = ", ")
        if (length(not_ordered) > 12L) {
          shown <- paste0(shown, ", and ", length(not_ordered) - 12L, " more")
        }
        # Read the two situations differently: every item continuous (what the
        # bundled example shows) and a mixed set (where naming the offenders is
        # the useful part).
        offending_types <- sort(unique(types[!types %in% c("ordered", "binary")]))
        count_txt <- if (length(not_ordered) == length(its)) {
          sprintf(
            paste0(
              "All %d included items in scope (%s) are confirmed as ",
              "%s, so there are no category boundaries to estimate ",
              "and nothing has been fit."
            ),
            length(its), lbl,
            paste(offending_types, collapse = " or ")
          )
        } else {
          sprintf(
            paste0(
              "Of the %d included items in scope (%s), %d %s not ",
              "confirmed as ordered or binary: %s. Nothing has been ",
              "fit; the model needs every item in one scale to be ",
              "categorical."
            ),
            length(its), lbl, length(not_ordered),
            if (length(not_ordered) == 1L) "is" else "are", shown
          )
        }
        return(list(ok = FALSE, kind = "not_ordered", ui = div(
          class = "alert alert-info",
          tags$p(
            strong("The graded response model requires ordered categorical responses. "),
            "Samejima's (1969) model describes the probability of responding at ",
            "or above each category boundary of an item, so it is defined only ",
            "for responses that fall into a small number of ordered categories: ",
            "a Likert item, a severity rating, a scored rubric level. ",
            count_txt
          ),
          tags$p(
            strong("If the responses really are ordered categories: "),
            "go to the ", strong("Ingest"), " tab, set ", strong("Type"),
            " to ", strong("Ordered"), " (or ", strong("Binary"),
            " for two categories) for these items in the grid, and press ",
            strong("Confirm the study"),
            ". That switch also moves the pipeline onto the polychoric ",
            "correlation matrix with the WLSMV estimator, which is the ",
            "substantive point rather than a formality: treating a coarse ",
            "ordinal item as continuous attenuates the loadings, and so ",
            "attenuates the discriminations and the information computed from ",
            "them. The distortion is material once an item has fewer than ",
            "about five categories, and worse when the categories are ",
            "asymmetrically used (Rhemtulla, Brosseau-Liard & Savalei, 2012)."
          ),
          tags$p(
            class = "mb-0",
            strong("If the responses are genuinely continuous: "),
            "do not relabel them as ordered to unlock this tab. A test score, ",
            "a reaction time, a physical measurement, or a composite of many ",
            "items has no category boundaries to estimate, and the model that ",
            "fits it is the continuous factor analysis model on the ",
            strong("CFA"), " tab, with Coefficient Omega, \u03c9, and its confidence ",
            "interval on the ", strong("Reliability"),
            " tab as the precision summary. The information function of this ",
            "parameterization is not defined for a continuous indicator."
          )
        )))
      }
      # The value checks irt_grm() enforces, surfaced here so the message is
      # actionable inside the app rather than a raw error from the estimator.
      d <- scope_data()
      if (length(attr(d, "problems"))) {
        return(list(ok = FALSE, ui = div(
          class = "alert alert-danger",
          strong("Some items cannot be read as numbers. "),
          paste(attr(d, "problems"), collapse = " ")
        )))
      }
      bad_int <- character(0)
      bad_k <- character(0)
      for (nm in names(d)) {
        x <- d[[nm]][!is.na(d[[nm]])]
        if (!length(x)) {
          bad_k <- c(bad_k, nm)
          next
        }
        if (!isTRUE(all.equal(x, round(x)))) {
          bad_int <- c(bad_int, nm)
          next
        }
        k <- length(unique(x))
        if (k < 2L || k > 20L) bad_k <- c(bad_k, nm)
      }
      if (length(bad_int)) {
        return(list(ok = FALSE, ui = div(
          class = "alert alert-warning",
          strong("Some items are confirmed ordered but hold non-integer values. "),
          sprintf(
            paste0(
              "The graded response model needs integer category ",
              "codes (for example 1, 2, 3, 4, 5). These items are ",
              "not integer valued: %s. Recode them to their ",
              "category numbers before loading, or set their type ",
              "back to continuous on the Ingest tab."
            ),
            paste(utils::head(bad_int, 12L), collapse = ", ")
          )
        )))
      }
      if (length(bad_k)) {
        return(list(ok = FALSE, ui = div(
          class = "alert alert-warning",
          strong("Some items do not have a usable number of categories. "),
          sprintf(
            paste0(
              "Every item needs at least 2 and at most 20 observed ",
              "categories; an item with no variability carries no ",
              "information about the latent variable, and an item ",
              "with more than 20 distinct values is continuous in ",
              "all but name. Check: %s."
            ),
            paste(utils::head(bad_k, 12L), collapse = ", ")
          )
        )))
      }
      list(ok = TRUE, ui = NULL)
    })

    # The gate's verdict, or why it could not be made: a silent error (the
    # pipeline gate's own req) shows the pipeline gate; any other error is
    # shown as such.
    gate_read <- reactive(sieve_outcome(irt_gate()))

    output$gate <- renderUI({
      read <- gate_read()
      if (!read$ready) {
        return(sieve_gate(state))
      }
      if (!read$ok) {
        return(div(
          class = "alert alert-danger",
          strong("The item scope could not be checked: "), read$error
        ))
      }
      g <- read$value
      if (!isTRUE(g$ok)) {
        return(g$ui)
      }
      # The one line that says whether the model is not fit yet, current,
      # or stopped with an error, with the Fit button where one belongs; or,
      # when it is from before a change, what changed, with the page's one
      # Update.
      page_update$line() %||% demand$line()
    })

    # ---- The graded response model (DMAR::irt_grm) --------------------------

    # The reported discrimination metric is a display choice, not a model
    # choice: a_logistic = 1.702 * a_normal_ogive exactly, item for item, and
    # the boundary locations do not move at all. So the model is fit once, in
    # the normal ogive metric the conversion from the categorical factor
    # analysis solution lands in, and the radio only selects which column the
    # table shows. Keeping input$metric out of the fit is what makes the
    # toggle instant instead of an eight second WLSMV refit.
    metric <- reactive(input$metric %||% "normal_ogive")

    # The model is fit on request (fct_ondemand.R): the scope and the
    # estimator are the choices the result is kept under, so a construct fit
    # earlier is shown again without a refit when it is selected again.
    irt_key <- reactive({
      read <- gate_read()
      req(read$ready, read$ok, isTRUE(read$value$ok))
      sieve_demand_key(state, list(scope = scope_key(), estimator = input$estimator %||% "WLSMV"))
    })
    irt_plan <- reactive({
      its <- scope_items()
      est <- input$estimator %||% "WLSMV"
      p <- length(its)
      seconds <- sieve_demand_wait("irt", p, est)
      list(
        estimator = est, p = p, seconds = seconds,
        label = sieve_duration_label(seconds),
        progress = sprintf("Graded response model: %s, %d items (%s)", est, p, scope_label()),
        detail = "Fitting the single factor categorical model in lavaan through DMAR::irt_grm().",
        sentence = sprintf(
          "Fitting the graded response model of the %d items in %s under %s is estimated to take %s",
          p, scope_label(), est, sieve_duration_label(seconds)
        ),
        refused = sieve_wlsmv_guard(its, state$items, ordered = its, estimator = est)
      )
    })
    compute_grm <- function(key, plan) {
      its <- shiny::isolate(scope_items())
      d <- shiny::isolate(scope_data())
      est <- key$choices$estimator
      # The RMSEA interval of the fit measures DMAR attaches is at the RMSEA
      # level (irt_grm()'s rmsea_conf_level).
      grm_args <- list(d, items = its, estimator = est, metric = "normal_ogive",
        rmsea_conf_level = sieve_default_rmsea_conf_level())
      run <- sieve_capture(do.call(DMAR::irt_grm, grm_args))
      warns <- run$warnings
      if (!run$ok) {
        return(list(error = run$error, estimator = est, items = its, warnings = warns))
      }
      out <- run$value
      # A returned fit is not a certificate: a model that did not converge
      # or is not identified has no solution for the item parameters or the
      # information function to describe, so nothing but its status is
      # shown.
      status <- sieve_fit_status(attr(out, "fit"), warnings = warns)
      if (!sieve_has_solution(status)) {
        return(list(
          error = status$message, status = status, estimator = est,
          items = its, warnings = warns
        ))
      }
      sieve_log(
        state,
        sprintf(
          paste0(
            "IRT: graded response model (Samejima, 1969) on %d ",
            "ordered items in %s, fit as the single factor ",
            "categorical factor analysis model with %s and read in ",
            "the normal ogive discrimination metric ",
            "(DMAR::irt_grm)."
          ),
          length(its), shiny::isolate(scope_label()), est
        ),
        c(
          sieve_irt_replay_code(its, est),
          "grm                          # a and b per item and boundary",
          'attr(grm, "a_logistic")      # the same slopes times 1.702',
          sprintf(
            'lavaan::fitMeasures(attr(grm, "fit"), fm.args = list(rmsea.ci.level = %s))  # CFI, TLI, RMSEA with its %s interval, SRMR (the srmr_bentler_nomean row)',
            sieve_num_lit(sieve_default_rmsea_conf_level()), sieve_ci_percent(sieve_default_rmsea_conf_level())
          ),
          sprintf(
            '# (attr(grm, "fit_measures") holds the same measures with the RMSEA interval at %s, its rmsea_conf_level.)',
            sieve_ci_percent(sieve_default_rmsea_conf_level())
          )
        )
      )
      code_record <- sieve_code_snapshot(state, context = c(
        sprintf("Graded response model: %s; %d ordered items in %s.",
                est, length(its), shiny::isolate(scope_label())),
        "Single-factor categorical CFA, fitted by DMAR::irt_grm; normal-ogive discrimination.",
        "The logistic discrimination is the same fitted slope multiplied by 1.702; the model is not refitted."
      ))
      # The fit measures are read once, here, under the progress bar: lavaan
      # fits the baseline models for them, which on a large WLSMV model costs
      # seconds, and the fit panels must not pay that again after a label
      # edit.
      list(
        grm = out, estimator = est, items = its, warnings = warns, status = status,
        code_record = code_record,
        measures = sieve_fit_measures(attr(out, "fit"), conf_level = sieve_default_rmsea_conf_level()),
        analysis_data = d[,
          its,
          drop = FALSE
        ],
        analysis_meta = shiny::isolate(state$meta),
        revision = sieve_revision(state)
      )
    }
    demand <- sieve_demand_wire(
      input, session, state,
      id = "fit_go", what = "Graded response model",
      key = irt_key, plan = irt_plan, compute = compute_grm, kind = "irt",
      noun = "The graded response model", action = "Fit the graded response model",
      refit = "Update"
    )

    grm_fit <- reactive({
      read <- gate_read()
      if (!read$ready || !read$ok || !isTRUE(read$value$ok)) {
        return(NULL)
      }
      demand$result()
    })

    sieve_code_navigation(input, session)
    model_code_output <- function() renderUI({
      # demand$status() reads stored results only; it cannot fit the model.
      read <- sieve_outcome(demand$status())
      status <- if (read$ready && read$ok) read$value else list(state = "not_run")
      sieve_code_view(status$value$code_record, status = status, ns = session$ns)
    })
    output$model_code <- model_code_output()
    output$fit_code <- model_code_output()
    information_code <- reactiveVal(NULL)
    info_code_output <- function() renderUI({
      record <- information_code()
      read <- sieve_outcome(demand$status())
      held <- if (read$ready && read$ok) read$value else list(state = "not_run")
      if (is.null(held$value)) record <- NULL
      current_parent <- if (!is.null(held$value)) sieve_hash(list(
        held$value$items, held$value$estimator, held$value$revision
      )) else NULL
      status <- if (is.null(record)) "not_run" else if (
        !identical(held$state, "current") || !identical(record$parent_key, current_parent) ||
          !identical(record$theta, theta_grid())
      ) "stale" else "current"
      sieve_code_view(record, status = status, ns = session$ns,
        readout = c("info$test_information", 'attr(info, "item_information")',
                    "1 / sqrt(info$test_information)"))
    })
    output$test_info_code <- info_code_output()
    output$item_info_code <- info_code_output()
    output$coverage_code <- info_code_output()

    # ---- Item and test information (DMAR::irt_information) ------------------

    theta_grid <- reactive({
      lo <- sieve_finite_or(input$theta_min, -4)
      hi <- sieve_finite_or(input$theta_max, 4)
      if (!(hi > lo)) {
        lo <- -4
        hi <- 4
      }
      n <- as.integer(sieve_finite_or(input$theta_n, 81))
      n <- max(11L, min(n, 801L))
      seq(lo, hi, length.out = n)
    })

    sieve_irt_evidence_wire(input, output, session, state, grm_fit, theta_grid,
      is_current = function() identical(demand$status()$state, "current"))

    # One Update for the page: the graded
    # response model is the tab's one result fit on request. The item and
    # test information follow it by themselves; the evidence panels
    # (parameter-uncertainty bands, category cells, traces) keep their own
    # buttons and say there when the model they read is from before.
    page_update <- sieve_update_wire(input, session, function() {
      list(grm = sieve_update_demand_entry(demand, "Graded response model", irt_plan))
    })

    # The information mathematics in irt_information() is written for normal
    # ogive discriminations, which is what grm_fit() holds whichever metric
    # the table is reporting. Information is therefore a property of the model
    # and not of how the slope is written down, and this reactive does not
    # depend on the metric radio at all.
    info_res <- reactive({
      g <- grm_fit()
      if (is.null(g) || !is.null(g$error)) {
        return(NULL)
      }
      if (!identical(demand$status()$state, "current")) return(NULL)
      gg <- g$grm
      params <- data.frame(
        item = as.character(gg$item),
        category = gg$category,
        a = gg$a,
        b = gg$b,
        stringsAsFactors = FALSE
      )
      th <- theta_grid()
      computed <- sieve_capture(DMAR::irt_information(grm = params, trait_value = th))
      if (!computed$ok) {
        return(list(error = computed$error))
      }
      out <- computed$value
      sieve_log(
        state,
        sprintf(
          paste0(
            "IRT: item and test information over theta from %.2f to ",
            "%.2f in %d grid points, with SE(theta) = ",
            "1/sqrt(I(theta)) (DMAR::irt_information)."
          ),
          min(th), max(th), length(th)
        ),
        c(
          sieve_irt_replay_code(g$items, g$estimator),
          sprintf(
            "theta <- seq(%s, %s, length.out = %d)",
            format(min(th)), format(max(th)), length(th)
          ),
          "info <- DMAR::irt_information(grm = grm, trait_value = theta)",
          'attr(info, "trait_value_max_information")   # where the scale measures best',
          'rowSums(attr(info, "item_information"))  # equals test_information'
        )
      )
      record <- sieve_code_snapshot(state, context = c(
        sprintf("Information: %s model, %d ordered items; theta %s to %s at %d grid points.",
                g$estimator, length(g$items), format(min(th)), format(max(th)), length(th)),
        "DMAR::irt_information; conditional standard error is 1 / sqrt(test information)."
      ))
      record$parent_key <- sieve_hash(list(g$items, g$estimator, g$revision))
      record$theta <- th
      information_code(record)
      list(info = out, params = params, theta = th, code_record = record)
    })

    # ---- Coverage and per item peaks ---------------------------------------

    coverage <- reactive({
      ir <- info_res()
      if (is.null(ir) || !is.null(ir$error)) {
        return(NULL)
      }
      info <- ir$info
      th <- info$trait_value
      ti <- info$test_information
      thr <- sieve_finite_or(input$info_threshold, 10)
      if (!(thr > 0)) thr <- 10
      covered <- is.finite(ti) & ti >= thr
      bands <- NULL
      if (any(covered)) {
        r <- rle(covered)
        ends <- cumsum(r$lengths)
        starts <- ends - r$lengths + 1L
        keep <- which(r$values)
        bands <- data.frame(
          lower = th[starts[keep]],
          upper = th[ends[keep]],
          stringsAsFactors = FALSE
        )
        bands$width <- bands$upper - bands$lower
        # Share of a standard normal trait distribution the band spans. Theta
        # is standardized by the identification of the model (the factor
        # variance is fixed to one), so the normal reference is the model's
        # own trait distribution, not an outside assumption.
        bands$normal_share <- stats::pnorm(bands$upper) - stats::pnorm(bands$lower)
      }
      peak_theta <- attr(info, "trait_value_max_information")
      peak_i <- max(ti, na.rm = TRUE)
      list(
        threshold = thr,
        se_at_threshold = 1 / sqrt(thr),
        bands = bands,
        peak_theta = peak_theta,
        peak_information = peak_i,
        se_at_peak = 1 / sqrt(peak_i),
        theta_range = range(th),
        grid_step = if (length(th) > 1L) th[2] - th[1] else NA_real_
      )
    })

    item_peaks <- reactive({
      ir <- info_res()
      if (is.null(ir) || !is.null(ir$error)) {
        return(NULL)
      }
      info <- ir$info
      M <- attr(info, "item_information")
      th <- info$trait_value
      ti <- info$test_information
      at_peak_row <- which.max(ti)
      contrib <- as.numeric(M[at_peak_row, ])
      total <- sum(contrib)
      a_by_item <- attr(info, "discrimination")
      items <- colnames(M)
      # The researcher's editable Item Name, falling back to the data column
      # name when no label was supplied.
      read <- sieve_outcome(active_meta())
      m <- if (read$ready && read$ok) read$value else NULL
      lab <- if (!is.null(m)) m$label[match(items, m$item)] else items
      lab[is.na(lab) | !nzchar(lab)] <- items[is.na(lab) | !nzchar(lab)]
      data.frame(
        item = items,
        label = lab,
        a = unname(a_by_item[items]),
        peak_information = apply(M, 2L, max),
        theta_at_peak = th[apply(M, 2L, which.max)],
        info_at_test_peak = contrib,
        share_at_test_peak = if (is.finite(total) && total > 0) contrib / total
        else rep(NA_real_, length(contrib)),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    })

    # ---- Notes that name the model behind every number ----------------------

    output$model_note <- renderUI({
      g <- grm_fit()
      if (is.null(g)) {
        return(NULL)
      }
      if (!is.null(g$error)) {
        if (!is.null(g$status)) {
          return(sieve_status_alert(g$status,
            what = "graded response model",
            warnings = g$warnings
          ))
        }
        return(div(
          class = "alert alert-danger",
          strong("The graded response model could not be fit. "),
          g$error
        ))
      }
      gg <- g$grm
      nk <- attr(gg, "n_categories")
      k_txt <- if (length(unique(nk)) == 1L) {
        sprintf("%d categories each", unique(nk))
      } else {
        sprintf("%d to %d categories", min(nk), max(nk))
      }
      met_txt <- if (identical(metric(), "logistic")) {
        "logistic (a multiplied by 1.702)"
      } else {
        "normal ogive (probit)"
      }
      warn_ui <- if (length(g$warnings)) {
        div(
          class = "alert alert-warning mt-2",
          strong("Estimator warning. "), paste(g$warnings, collapse = " ")
        )
      }
      # The one-line method statement of the table below, with the fit of
      # the model it comes from, so the fit is read before the parameters.
      fm <- g$measures %||% sieve_fit_measures(attr(gg, "fit"), conf_level = sieve_default_rmsea_conf_level())
      num <- function(x) if (length(x) == 1L && is.finite(x)) sprintf("%.3f", x) else "n/a"
      fit_txt <- if (!is.null(fm) && length(fm)) {
        sprintf(
          " Fit: %s %s, %s %s, %s [%s, %s], SRMR %s.",
          sieve_fit_label(fm, "cfi"), num(fm[["cfi"]]), sieve_fit_label(fm, "rmsea"),
          num(fm[["rmsea"]]), sieve_rmsea_ci_words(sieve_fit_ci_level(fm)),
          num(fm[["rmsea_lo"]]), num(fm[["rmsea_hi"]]), num(fm[["srmr"]])
        )
      } else {
        ""
      }
      # The shared uncertainty sentence of the method lines
      # (sieve_inference_text()).
      inference <- tryCatch(sieve_inference_text(sieve_fit_inference(attr(gg, "fit"))), error = function(e) NULL)
      tagList(
        div(
          class = "text-muted small mb-2",
          sprintf(
            paste0(
              "Model: one latent variable (%s) measured by the %d ",
              "items in scope (%s) as ordered indicators, fit by ",
              "lavaan with the %s estimator on the polychoric ",
              "correlations and read as Samejima's graded response ",
              "model (DMAR::irt_grm()); N = %d complete cases, %s; ",
              "discrimination in the %s metric, the complementary metric beside it.%s"
            ),
            unique(as.character(gg$factor))[1], length(g$items),
            scope_label(), attr(gg, "estimator"), attr(gg, "N"),
            k_txt, met_txt, fit_txt
          ),
          if (!is.null(inference)) tags$div(class = "sieve-inference-line", paste0("Uncertainty: ", inference, "."))
        ),
        warn_ui
      )
    })

    output$fit_note <- renderUI({
      g <- grm_fit()
      req(g, is.null(g$error))
      fm <- g$measures %||% sieve_fit_measures(attr(g$grm, "fit"), conf_level = sieve_default_rmsea_conf_level())
      zero_df <- !is.null(fm) && is.finite(fm[["df"]]) && fm[["df"]] == 0
      tagList(
        div(
          class = "text-muted small",
          "Fit of the single factor categorical model whose solution the item ",
          "parameters come from, taken from lavaan::fitMeasures() on the fitted ",
          "object DMAR::irt_grm() returns, with the RMSEA interval at ",
          sieve_ci_percent(sieve_default_rmsea_conf_level()),
          " (the level of every RMSEA interval SIEVER shows, 90% by default, DMAR's and lavaan's; ",
          "the other intervals are at ", sieve_ci_percent(sieve_default_conf_level()), "). ",
          # Robust and scaled indices are different statistics; the note says
          # which family the table shows and why.
          paste(sieve_fit_note(fm), collapse = " "), " Judge by ",
          "RMSEA with its interval, CFI, and SRMR together, and treat a poor ",
          "fit as a statement that the items do not share one dimension rather ",
          "than as a nuisance: the item parameters and the information function ",
          "both inherit that misspecification."
        ),
        if (zero_df) {
          div(
            class = "alert alert-warning mt-2 small mb-0",
            strong("This model has zero degrees of freedom (df = 0). "),
            "There is no ordinary test of the dimensional assumption. ",
            "Exact-fit conventions, when established, supply no evidence of ",
            "unidimensionality. Interpret parameters, uncertainty and information ",
            "in light of the model's convergence and admissibility. ",
            "To test the dimensional assumption, model a construct with four or ",
            "more items, or judge it from the multiple construct model on the ",
            "CFA tab."
          )
        }
      )
    })

    output$test_info_note <- renderUI({
      ir <- info_res()
      if (is.null(ir)) {
        return(NULL)
      }
      if (!is.null(ir$error)) {
        return(div(
          class = "alert alert-danger",
          strong("The information function could not be computed. "),
          ir$error
        ))
      }
      cv <- coverage()
      req(cv)
      g <- grm_fit()
      div(
        class = "text-muted small",
        sprintf(
          paste0(
            "Test information from the graded response model above ",
            "(%d items, %s estimator), evaluated at %d values of ",
            "theta from %.2f to %.2f (step %.3f) by ",
            "DMAR::irt_information(). Information peaks at theta = ",
            "%.2f, where I = %.2f and SE(theta) = %.3f. The figure is ",
            "drawn here with ggplot2 from the returned columns, in ",
            "the app palette, because DMAR's own test information ",
            "figure sets its two line colors as fixed aesthetics ",
            "rather than through a scale, and a fixed aesthetic ",
            "cannot be overridden by adding a ggplot2 scale."
          ),
          length(g$items), g$estimator, length(ir$theta),
          cv$theta_range[1], cv$theta_range[2], cv$grid_step,
          cv$peak_theta, cv$peak_information, cv$se_at_peak
        )
      )
    })

    output$item_info_note <- renderUI({
      ir <- info_res()
      req(ir, is.null(ir$error))
      g <- grm_fit()
      div(
        class = "text-muted small",
        sprintf(
          paste0(
            "One curve per item under the graded response model above, ",
            "from the item information matrix DMAR::irt_information() ",
            "returns (%d items, %s estimator, normal ogive ",
            "discriminations). The figure is ",
            "DMAR::plot_irt_information(info, what = \"item\") with ",
            "a SIEVER color scale added, which replaces DMAR's ",
            "Okabe-Ito scale so the app palette governs app color. ",
            "Curves are keyed by variable name to keep the legend ",
            "compact; the Item name for each is in the parameter ",
            "table and in the peak table under Precision Coverage."
          ),
          length(g$items), g$estimator
        )
      )
    })

    output$coverage_note <- renderUI({
      cv <- coverage()
      if (is.null(cv)) {
        return(NULL)
      }
      thr <- cv$threshold
      se_thr <- cv$se_at_threshold
      g <- grm_fit()
      head_txt <- sprintf(
        paste0(
          "Coverage of the latent trait by the graded response model above ",
          "(%d items, %s estimator). Threshold I(theta) >= %.3g. Because SE(theta) = 1 / ",
          "sqrt(I(theta)), that is the same statement as SE(theta) <= ",
          "%.3f, so at the edge of the band a 95%% interval for an ",
          "individual's theta spans about %.2f standard deviations of the ",
          "trait, and less inside the band. The default threshold of 10 is ",
          "the SE = 0.316 case: a defensible working reference, not a ",
          "rule, which is why the arithmetic is shown rather than the ",
          "number asserted. Set the threshold from the precision the ",
          "application needs, since a screening decision and an individual ",
          "diagnosis do not need the same interval."
        ),
        length(g$items), g$estimator, thr, se_thr, 2 * 1.959964 * se_thr
      )
      if (is.null(cv$bands)) {
        return(div(
          class = "alert alert-warning",
          strong("No part of the evaluated range reaches the threshold. "),
          head_txt,
          sprintf(
            paste0(
              " Test information peaks at theta = %.2f with I = ",
              "%.2f (SE = %.3f), which is below %.3g everywhere on ",
              "the grid. Either lower the threshold, or add items ",
              "with boundaries near the range you care about: ",
              "information is additive, so precision is bought by ",
              "adding discriminating items whose category ",
              "boundaries sit where the respondents are."
            ),
            cv$peak_theta, cv$peak_information, cv$se_at_peak, thr
          )
        ))
      }
      b <- cv$bands
      span_txt <- paste(sprintf("theta %.2f to %.2f", b$lower, b$upper),
        collapse = "; "
      )
      total_share <- sum(b$normal_share)
      div(
        class = "small",
        tags$p(head_txt),
        tags$p(
          class = "mb-0",
          strong("Covered band"),
          sprintf(
            " (%s): %s. ",
            if (nrow(b) == 1L) "one contiguous stretch"
            else sprintf("%d disjoint stretches", nrow(b)),
            span_txt
          ),
          sprintf(
            paste0(
              "That covers %.1f%% of a standard normal trait ",
              "distribution, so about %.0f in 100 respondents drawn ",
              "from such a distribution fall where this item set ",
              "measures to at least this precision. The endpoints ",
              "are grid values, so a finer grid locates them more ",
              "sharply. The complement is the actionable part: ",
              "respondents outside the band receive the widest ",
              "intervals, and because information is additive, an ",
              "item whose category boundaries sit out there buys ",
              "precision exactly where the scale currently has none ",
              "to spare."
            ),
            100 * total_share, 100 * total_share
          )
        )
      )
    })

    output$peak_note <- renderUI({
      cv <- coverage()
      req(cv)
      g <- grm_fit()
      div(
        class = "text-muted small",
        sprintf(
          paste0(
            "Each item's own information function under the graded ",
            "response model above (%d items, %s estimator), from the ",
            "item_information matrix of DMAR::irt_information(): ",
            "its highest value on the grid, the theta at which it ",
            "occurs, and what the item contributes at theta = %.2f, ",
            "where the test as a whole peaks. Because information ",
            "is additive, the last two columns are exactly what a ",
            "candidate item is worth at the place the scale ",
            "currently measures best. An item whose peak sits far ",
            "from where the respondents are can have a large ",
            "discrimination and still buy little."
          ),
          length(g$items), g$estimator, cv$peak_theta
        )
      )
    })

    # ---- Tables -------------------------------------------------------------

    # The displayed parameter table. Exposed as a reactive returning the data
    # frame and its column labels so the metric toggle can be verified with
    # shiny::testServer instead of by reading pixels, and so it is visible
    # that switching metrics rebuilds this table and nothing else.
    param_table <- reactive({
      g <- grm_fit()
      if (is.null(g) || !is.null(g$error)) {
        return(NULL)
      }
      gg <- g$grm
      met <- metric()
      logistic <- identical(met, "logistic")
      # Both columns come off the one fitted object. The fit is in the normal
      # ogive metric, so gg$a is the normal ogive slope and the logistic slope
      # is the a_logistic attribute irt_grm() carries alongside it, exactly
      # 1.702 times larger. Switching metrics swaps the two columns; no refit
      # is involved.
      a_no <- gg$a
      a_lo <- unname(attr(gg, "a_logistic")[as.character(gg$item)])
      a_shown <- if (logistic) a_lo else a_no
      a_other <- if (logistic) a_no else a_lo
      read <- sieve_outcome(active_meta())
      m <- if (read$ready && read$ok) read$value else NULL
      lab <- if (!is.null(m)) m$label[match(as.character(gg$item), m$item)] else NULL
      if (is.null(lab)) lab <- as.character(gg$item)
      lab[is.na(lab) | !nzchar(lab)] <- as.character(gg$item)[is.na(lab) | !nzchar(lab)]
      df <- data.frame(
        Item = lab,
        Variable = as.character(gg$item),
        Boundary = gg$category,
        lambda = gg$lambda,
        tau = gg$tau,
        a = a_shown,
        a_other = a_other,
        b = gg$b,
        stringsAsFactors = FALSE
      )
      a_lab <- if (logistic) {
        c("a (logistic)", "a (normal ogive)")
      } else {
        c("a (normal ogive)", "a (logistic)")
      }
      list(
        df = df, metric = met,
        colnames = c(
          "Item", "Variable", "Boundary k", "Lambda", "Tau",
          a_lab[1], a_lab[2], "b (location)"
        )
      )
    })

    output$param_tbl <- renderDT({
      g <- grm_fit()
      if (is.null(g)) {
        return(sieve_dt(
          data.frame(Note = "No graded response model has been fit: see the message above, and press Fit the graded response model when the items in scope qualify."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      # A model without a solution has no item parameters to show: the
      # status alert above is all the tab shows for it (the convention of
      # the CFA and SEM tabs).
      if (!is.null(g$status) && !sieve_has_solution(g$status)) {
        req(FALSE)
      }
      if (!is.null(g$error)) {
        return(sieve_dt(
          data.frame(Note = g$error),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      p <- param_table()
      req(p)
      sieve_dt(
        p$df,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25, scrollX = TRUE),
        colnames = p$colnames
      ) |>
        formatRound(c("lambda", "tau", "a", "a_other", "b"), 3)
    })

    output$fit_tbl <- renderDT({
      g <- grm_fit()
      req(g, is.null(g$error))
      fm <- g$measures %||% sieve_fit_measures(attr(g$grm, "fit"), conf_level = sieve_default_rmsea_conf_level())
      if (is.null(fm)) {
        return(sieve_dt(
          data.frame(Note = "Fit measures were not available for this model."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      # Each index is named with its family (robust, scaled, or standard).
      df <- sieve_fit_table(fm, c(
        "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea_lo",
        "rmsea_hi", "srmr", "npar"
      ))
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t")
      ) |>
        formatRound("Value", 3)
    })

    output$coverage_tbl <- renderDT({
      cv <- coverage()
      req(cv)
      if (is.null(cv$bands)) {
        return(sieve_dt(
          data.frame(
            Statistic = c(
              "Threshold on test information",
              "Equivalent SE(theta)",
              "Peak test information",
              "Theta at the peak",
              "SE(theta) at the peak"
            ),
            Value = c(
              cv$threshold, cv$se_at_threshold, cv$peak_information,
              cv$peak_theta, cv$se_at_peak
            ),
            stringsAsFactors = FALSE
          ),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ) |>
          formatRound("Value", 3))
      }
      b <- cv$bands
      df <- data.frame(
        Band = seq_len(nrow(b)),
        Lower = b$lower,
        Upper = b$upper,
        Width = b$width,
        Normal.share = b$normal_share,
        stringsAsFactors = FALSE
      )
      sieve_dt(
        df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t"),
        caption = sprintf(
          paste0(
            "Theta over which test information reaches ",
            "%.3g, equivalently SE(theta) at or below ",
            "%.3f. Peak information %.2f at theta = %.2f ",
            "(SE = %.3f)."
          ),
          cv$threshold, cv$se_at_threshold,
          cv$peak_information, cv$peak_theta, cv$se_at_peak
        ),
        colnames = c(
          "Band", "Theta lower", "Theta upper", "Width",
          "Share of a standard normal trait"
        )
      ) |>
        formatRound(c("Lower", "Upper", "Width"), 3) |>
        formatRound("Normal.share", 3)
    })

    output$peak_tbl <- renderDT({
      pk <- item_peaks()
      req(pk, nrow(pk) > 0)
      df <- pk[order(-pk$peak_information), , drop = FALSE]
      out <- data.frame(
        Item = df$label,
        Variable = df$item,
        a = df$a,
        Peak = df$peak_information,
        Theta.at.peak = df$theta_at_peak,
        At.test.peak = df$info_at_test_peak,
        Share = df$share_at_test_peak,
        stringsAsFactors = FALSE
      )
      sieve_dt(
        out,
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25, scrollX = TRUE),
        colnames = c(
          "Item", "Variable", "a (normal ogive)",
          "Peak item information", "Theta at the item peak",
          "Information at the test peak", "Share at the test peak"
        )
      ) |>
        formatRound(c("a", "Peak", "Theta.at.peak", "At.test.peak"), 3) |>
        formatRound("Share", 3)
    })

    # ---- Figures ------------------------------------------------------------
    # Both are exposed as reactives returning the plot object so the module can
    # be verified with shiny::testServer rather than by looking at pixels.

    test_info_plot <- reactive({
      ir <- info_res()
      req(ir, is.null(ir$error))
      cv <- coverage()
      req(cv)
      info <- ir$info
      cols <- sieve_palette_colors(state$palette %||% "nd", 3L)
      info_col <- cols[1]
      se_col <- cols[2]
      band_col <- cols[3]
      d <- data.frame(
        theta = info$trait_value,
        information = info$test_information,
        se = info$se,
        stringsAsFactors = FALSE
      )
      fin <- d[is.finite(d$se), , drop = FALSE]
      i_max <- max(d$information, na.rm = TRUE)
      se_lo <- if (nrow(fin)) min(fin$se) else NA_real_
      se_hi <- if (nrow(fin)) max(fin$se) else NA_real_
      se_range <- se_hi - se_lo
      draw_se <- nrow(fin) > 1L && is.finite(se_range) && se_range > 0 &&
        is.finite(i_max) && i_max > 0

      plt <- ggplot2::ggplot(d, ggplot2::aes(
        x = .data$theta,
        y = .data$information
      ))
      if (!is.null(cv$bands)) {
        plt <- plt + ggplot2::geom_rect(
          data = cv$bands,
          mapping = ggplot2::aes(
            xmin = .data$lower, xmax = .data$upper,
            ymin = -Inf, ymax = Inf
          ),
          inherit.aes = FALSE, fill = band_col, alpha = 0.12
        )
      }
      # The threshold reference line is drawn only when it is near the data. A
      # threshold far above the peak would stretch the vertical axis to reach
      # it and flatten the curve the panel exists to show, so in that case the
      # subtitle reports the threshold instead of the line.
      show_threshold <- is.finite(cv$threshold) && is.finite(i_max) &&
        cv$threshold <= 1.25 * i_max
      if (show_threshold) {
        plt <- plt + ggplot2::geom_hline(
          yintercept = cv$threshold, linetype = "dashed",
          color = "grey55", linewidth = 0.4
        )
      }
      plt <- plt +
        ggplot2::geom_vline(
          xintercept = cv$peak_theta, linetype = "dotted",
          color = "grey35", linewidth = 0.4
        )
      if (draw_se) {
        fin$se_rescaled <- (fin$se - se_lo) / se_range * i_max
        plt <- plt +
          ggplot2::geom_line(
            data = fin,
            mapping = ggplot2::aes(x = .data$theta, y = .data$se_rescaled),
            color = se_col, linetype = "dashed", linewidth = 0.7
          ) +
          ggplot2::scale_y_continuous(
            sec.axis = ggplot2::sec_axis(
              ~ . / i_max * se_range + se_lo,
              name = "SE of the estimated theta (dashed)"
            )
          )
      }
      plt +
        ggplot2::geom_line(color = info_col, linewidth = 0.95) +
        ggplot2::labs(
          x = "Latent trait score (theta), in standard normal units",
          y = "Test information, I(theta)",
          title = sprintf("Test information and precision, %s", scope_label()),
          subtitle = sprintf(
            paste0(
              "Peak I = %.2f at theta = %.2f, SE(theta) = %.3f. ",
              "Coverage threshold %.3g%s."
            ),
            cv$peak_information, cv$peak_theta, cv$se_at_peak, cv$threshold,
            if (show_threshold) {
              if (is.null(cv$bands)) "" else " (dashed line, shaded band)"
            } else " is above the top of this panel and is not drawn"
          )
        ) +
        ggplot2::theme_minimal(base_size = 13)
    })

    output$test_info_plot <- renderPlot(test_info_plot(), res = 96,
      alt = "Test information and its conditional standard error across the trait for the graded response model."
    )

    # DMAR's item figure, restyled with the app palette. Adding a second color
    # scale replaces the first (ggplot2 says so in a message), which is the
    # supported way to override the Okabe-Ito scale DMAR builds in.
    item_info_plot <- reactive({
      ir <- info_res()
      req(ir, is.null(ir$error))
      info <- ir$info
      items <- colnames(attr(info, "item_information"))
      cols <- sieve_palette_colors(state$palette %||% "nd", length(items))
      base <- DMAR::plot_irt_information(info, what = "item")
      suppressMessages(
        base +
          ggplot2::scale_color_manual(
            values = stats::setNames(cols, items), name = "Item"
          ) +
          ggplot2::labs(
            title = sprintf("Item information, %s", scope_label())
          )
      )
    })

    output$item_info_plot <- renderPlot(item_info_plot(), res = 96,
      alt = "Information of each item across the trait for the graded response model."
    )

    invisible(NULL)
  })
}

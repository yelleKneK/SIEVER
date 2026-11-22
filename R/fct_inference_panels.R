# Live controls for model-linked inference; optional engines remain optional.

#' Scientific table or actionable engine reason
#' @param x A data frame or list with table/error.
#' @return A DT widget.
#' @author Ken Kelley
#' @noRd
sieve_inference_table <- function(x) {
  if (is.null(x)) x <- data.frame(Note = "Choose the analysis and press its Compute button.")
  if (is.list(x) && !is.data.frame(x)) x <- x$table %||% data.frame(Note = x$error %||% x$reason %||% "The engine returned no table.")
  labels <- gsub("_", " ", names(x), fixed = TRUE)
  special <- c(
    "Pr(>Chisq)" = "p", "Chisq diff" = "Chi-square difference", "Df diff" = "df difference",
    "Df" = "df", "Chisq" = "Chi-square", "used" = "Usable draws", "R2" = "R-squared",
    "rank_estimate" = "Rank composite (categorical omega's target)"
  )
  replace <- match(names(x), names(special))
  labels[!is.na(replace)] <- unname(special[replace[!is.na(replace)]])
  widget <- sieve_dt(x,
    colnames = if (length(labels)) labels else NULL,
    rownames = FALSE, selection = "none",
    options = list(dom = "tp", scrollX = TRUE, pageLength = 15)
  )
  numeric_columns <- names(x)[vapply(x, is.numeric, logical(1))]
  numeric_columns <- setdiff(
    numeric_columns,
    c(
      "row",
      "original_row",
      "answered",
      "n",
      "used",
      "requested",
      "failed",
      "successful",
      "attempted",
      # The score specification table's counts.
      "k",
      "required",
      "eligible",
      "not_scored"
    )
  )
  if (length(numeric_columns)) widget <- DT::formatRound(widget, numeric_columns, 3)
  widget
}

#' CFA residual and composite evidence controls
#' @param ns Module namespace.
#' @param section Return residuals, model-linked reliability, or both.
#' @return UI fragment.
#' @author Ken Kelley
#' @noRd
sieve_cfa_evidence_ui <- function(ns, section = c("all", "residuals", "reliability")) {
  section <- match.arg(section)
  panels <- list(
    bslib::card(
      bslib::card_header("Observed Versus Implied Residuals"),
      shiny::p("Residuals describe the fitted model on its recorded data. A variance-scaled residual and a residual divided by its standard error are different quantities; the z column is the latter."),
      shiny::selectInput(
        ns("residual_type"),
        "Residual scaling",
        c(
          "Bentler variance scaling (SRMR)" = "cor.bentler",
          "Observed minus implied correlations" = "cor.bollen",
          "Raw covariance" = "raw"
        )
      ),
      shiny::selectInput(ns("residual_pair"), "Look up an item pair", choices = NULL),
      DT::DTOutput(ns("residual_lookup")), DT::DTOutput(ns("residual_pairs"))
    ),
    bslib::card(
      bslib::card_header("Reliability of the Selected Measurement Model"),
      shiny::p("Each construct's composite is a weighted sum of its modeled items, with unit weights unless you enter weights below; all modeled items together form one more composite. Its reliability, labeled Model-linked composite reliability (observed denominator), is the composite's true-score variance implied by the selected CFA as fitted (its common factors; residual variances and covariances count as error) divided by the composite's observed variance, estimated from the unrestricted covariance matrix of the items (the observed denominator, as in robust omega). Composites of ordered items use Green and Yang's (2009) coefficient for the weighted sum of the item codes as given in the data, each threshold step weighted by the increment between adjacent codes, with the model-implied standardized thresholds and the denominator built from the unrestricted polychoric correlations and the thresholds. When every item's codes step by one shared amount this is also the coefficient of the sum of consecutive category ranks (each item's categories scored 0, 1, ..., C - 1 in the order of its codes), the composite the Reliability tab's categorical omega describes; when they do not (an unused category between two used ones, codes such as 1, 2, 5, 10, or steps that differ between items), the two are different composites, and the rank coefficient is shown beside the coded one with a note naming the codes. The numerator comes from the selected multi-factor CFA, not from a separate one-factor model per construct, so these values are not the Robust omega (observed denominator) of each construct's or facet's own items, which the Reliability tab reports with DMAR::reliability_omega()."),
      shiny::textInput(
        ns("composite_weights"),
        "Optional custom weights in modeled item order (blank = unit weights)",
        ""
      ),
      sieve_boot_inputs(ns, B = 0, B_id = "composite_B", seed_id = "composite_seed", min_B = 0, max_B = NULL, step_B = 100, label = "Bootstrap replications (0 = point estimate only)", width = NULL, seed_label = "Seed (optional)", seed_width = NULL, seed_min = 1),

      shiny::actionButton(ns("composite_go"), "Compute Model-Linked Reliability"),
      shiny::uiOutput(ns("composite_note")), DT::DTOutput(ns("composite_table"))
    )
  )
  if (identical(section, "residuals")) return(panels[[1]])
  if (identical(section, "reliability")) return(panels[[2]])
  do.call(shiny::tagList, panels)
}

#' Bind residual and composite controls to a fitted CFA
#' @param input,output,session Shiny module objects.
#' @param state Shared state.
#' @param parent Reactive fitted result.
#' @param current Function saying whether the fitted inputs are current.
#' @return Reactive results.
#' @author Ken Kelley
#' @noRd
sieve_cfa_evidence_server <- function(input, output, session, state, parent, current) {
  # The controls a composite request was made with: kept with its result and
  # compared with the controls shown, as the score-uncertainty panel does
  # (score_now()), so an edited weight, B or seed never leaves an earlier
  # result as current.
  composite_request <- function() {
    list(
      weights = trimws(input$composite_weights %||% ""),
      B = input$composite_B %||% 0L,
      seed = sieve_seed_input(input$composite_seed)
    )
  }
  residuals <- shiny::reactive({
    run <- parent()
    shiny::req(run$fit, sieve_has_solution(run))
    sieve_residual_pairs(run$fit, input$residual_type %||% "cor.bentler")
  })
  shiny::observe({
    x <- sieve_outcome(residuals())
    if (!x$ready || !x$ok || is.null(x$value$table)) return()
    tab <- x$value$table
    # Served from the server and searched as it is typed: a pool of 68
    # items has 2,278 pairs, and a select of 1,000
    # options or more sent whole to the browser is slow (Shiny warns of it).
    shiny::updateSelectizeInput(session,
      "residual_pair",
      choices = stats::setNames(
        seq_len(nrow(tab)),
        paste(tab$item1,
          tab$item2,
          sep = " with "
        )
      ),
      selected = "1", server = TRUE
    )
  })
  shiny::observe({
    run <- sieve_outcome(parent())
    if (!run$ready || !run$ok || is.null(run$value$fit)) return()
    items <- sieve_model_composite_items(run$value$fit)
    shiny::updateTextInput(session, "composite_weights",
      label = paste0("Optional custom weights in item order: ", paste(items, collapse = ", "),
        " (blank = unit weights)"))
  })
  output$residual_pairs <- DT::renderDT(sieve_inference_table(residuals()))
  output$residual_lookup <- DT::renderDT({
    tab <- residuals()$table
    shiny::req(tab)
    i <- suppressWarnings(as.integer(input$residual_pair %||% 1L))
    sieve_inference_table(tab[seq_len(nrow(tab)) %in% i, , drop = FALSE])
  })
  composite <- shiny::eventReactive(input$composite_go, {
    shiny::req(isTRUE(current()))
    run <- parent()
    shiny::req(run$fit)
    text <- trimws(input$composite_weights %||% "")
    weights <- if (nzchar(text)) suppressWarnings(as.numeric(strsplit(text, "[,;[:space:]]+")[[1L]])) else NULL
    r <- tryCatch(sieve_model_composite(run$fit,
      weights,
      B = input$composite_B %||% 0L,
      seed = sieve_seed_input(input$composite_seed)
    ), error = function(e) list(error = conditionMessage(e)))
    if (is.null(r$error)) {
      choices <- list(weights = r$weights, B = input$composite_B %||% 0L, rng = r$rng)
      code <- c(
        sieve_cfa_code(run$spec, fit_name = "composite_fit"),
        if (!is.null(run$spec$engine_args$cluster)) c(
          paste0('attr(composite_fit, "sieve_spec") <- ', sieve_r_literal(run$spec)),
          'attr(composite_fit, "sieve_bootstrap_data") <- cfa_model_data'),
        sprintf(
          "composite_result <- SIEVER::sieve_model_composite(composite_fit, weights = %s, B = %d, seed = %s)",
          paste(utils::capture.output(dput(weights)), collapse = ""), input$composite_B %||% 0L,
          if (is.null(r$rng)) "NULL" else as.character(r$rng$seed)
        )
      )
      sieve_register_inference(
        state,
        "model_composite",
        run,
        choices,
        r[c(
          "table",
          "target",
          "method",
          "counts",
          "replications",
          "interval_reason",
          "warnings", "sampling", "draw_seeds"
        )],
        code
      )
    }
    list(result = r, key = run$spec$key, fit = run$fit, revision = sieve_revision(state), request = composite_request())
  })
  now <- shiny::reactive({
    x <- composite()
    shiny::req(x)
    if (!isTRUE(current()) || !identical(x$fit, parent()$fit) || !identical(
      x$key,
      parent()$spec$key
    ) || !sieve_same_revision(
      x$revision,
      sieve_revision(state)
    )) return(list(error = "The composite result is historical. Refit the current model and compute it again."))
    if (!identical(x$request, composite_request())) {
      return(list(error = paste(
        "The composite result was computed with other settings (the weights, the replications or",
        "the seed changed since); compute it again for the settings shown."
      )))
    }
    x$result
  })
  output$composite_table <- DT::renderDT(sieve_inference_table(now()))
  output$composite_note <- shiny::renderUI({
    r <- now()
    shiny::div(
      class = "small",
      shiny::p(paste(c(r$error, r$target, r$method, r$interval_reason, r$warnings), collapse = " ")),
      if (!is.null(r$replications)) {
        do.call(shiny::tagList, lapply(seq_len(nrow(r$replications)), function(i) {
          x <- r$replications[i, ]
          shiny::p(sprintf("%s: %d attempted; %d used, including %d improper retained; %d failed or undefined.",
            x$composite, x$attempted, x$used, x$improper_kept, x$failed),
            if (x$caution) "Caution: failed or improper draws can fill an interval tail. Review the solution and interval stability.")
        }))
      }
    )
  })
  list(residuals = residuals, composite = composite)
}

#' Latent boundary comparison controls
#' @param ns Namespace.
#' @return UI.
#' @author Ken Kelley
#' @noRd
sieve_bound_ui <- function(ns) {
  bslib::card(
    bslib::card_header("Your Latent Correlation Boundary"),
    shiny::p("State a signed correlation boundary from theory. This compares an equality-constrained CFA with the current model; it is not an automatic validity verdict. Correlation-one merged-factor tests require a separate boundary reference and are not substituted here."),
    shiny::selectInput(ns("latent_pair"), "Two factors", choices = NULL, multiple = TRUE),
    shiny::numericInput(ns("latent_bound"), "Signed correlation boundary", .9, min = -.99, max = .99, step = .05),
    shiny::actionButton(ns("latent_bound_go"), "Compare With My Boundary"),
    shiny::uiOutput(ns("latent_bound_note")), DT::DTOutput(ns("latent_bound_summary")),
    DT::DTOutput(ns("latent_bound_result"))
  )
}

#' Bind a latent boundary comparison to the current measurement model
#' @param input,output,session Module objects.
#' @param state Shared state.
#' @param parent Fitted result reactive.
#' @param current Currentness function.
#' @return Reactive comparison.
#' @author Ken Kelley
#' @noRd
sieve_bound_server <- function(input, output, session, state, parent, current) {
  # The pair and the bound a comparison was made with, compared with the
  # controls shown.
  bound_request <- function() {
    list(pair = sort(as.character(input$latent_pair %||% character(0))), bound = input$latent_bound %||% .9)
  }
  shiny::observe({
    run <- sieve_outcome(parent())
    if (!run$ready || !run$ok || is.null(run$value$fit)) return()
    lv <- lavaan::lavNames(run$value$fit, "lv")
    shiny::updateSelectInput(session, "latent_pair", choices = stats::setNames(lv, sieve_show_names(lv, run$value$fit)))
  })
  result <- shiny::eventReactive(input$latent_bound_go, {
    shiny::req(isTRUE(current()))
    run <- parent()
    shiny::req(run$fit)
    r <- tryCatch(
      sieve_latent_bound(
        run$fit,
        input$latent_pair,
        input$latent_bound %||% .9
      ),
      error = function(e) list(error = conditionMessage(e))
    )
    if (is.null(r$error)) {
      choices <- list(pair = r$pair, bound = r$bound, constraint = r$constraint)
      code <- c(
        sieve_cfa_code(run$spec, fit_name = "boundary_fit"),
        sprintf(
          "boundary_result <- SIEVER::sieve_latent_bound(boundary_fit, pair = %s, bound = %s)",
          paste(utils::capture.output(dput(r$pair)), collapse = ""), sieve_r_literal(r$bound)
        )
      )
      sieve_register_inference(
        state, "latent_boundary", run, choices,
        list(
          test = as.data.frame(r$test),
          estimate = r$estimate,
          interpretation = r$interpretation,
          warnings = r$warnings
        ),
        code
      )
    }
    list(value = r, revision = sieve_revision(state), parent_key = run$spec$key, request = bound_request())
  })
  boundary_now <- shiny::reactive({
    r <- result()
    if (!isTRUE(current()) || !sieve_same_revision(r$revision, sieve_revision(state)) ||
      !identical(r$parent_key, parent()$spec$key)) {
      return(list(error = "The boundary comparison is historical; refit and compare again."))
    }
    if (!identical(r$request, bound_request())) {
      return(list(error = paste(
        "The boundary comparison was computed for another pair or bound than the one shown;",
        "compare again for the pair and bound shown."
      )))
    }
    r$value
  })
  output$latent_bound_summary <- DT::renderDT(sieve_inference_table(boundary_now()$summary %||% boundary_now()))
  output$latent_bound_note <- shiny::renderUI({
    r <- boundary_now()
    shiny::p(
      class = "small",
      r$interpretation %||% r$error
    )
  })
  output$latent_bound_result <- DT::renderDT({
    r <- boundary_now()
    if (!is.null(r$error)) return(sieve_inference_table(r))
    table <- as.data.frame(r$test)
    table <- data.frame(model = ifelse(as.numeric(table$Df) == min(as.numeric(table$Df)),
      "Fitted CFA", "Correlation fixed at the stated bound"
    ), table, check.names = FALSE)
    sieve_inference_table(table)
  })
  result
}

#' Operational score specification controls
#'
#' One operational score per construct: the score and the minimum answered
#' rule, kept in `state$scoring_policies` through
#' `sieve_set_score_specification()`.
#'
#' @param ns Namespace.
#' @return UI.
#' @author Ken Kelley
#' @noRd
sieve_operational_ui <- function(ns) {
  bslib::card(
    bslib::card_header("Operational Score Specification"),
    shiny::p(paste0(
      "One operational score per construct: the score (the mean of the answered items by default, ",
      "the sum prorated from the answered items, or a Bartlett or regression factor score read from ",
      "the fitted measurement model) and the minimum answered count or proportion a respondent needs ",
      "to receive it. The specification is kept with the project and governs the criterion ",
      "correlations and the marker adjustment on the Validity tab, the sample reference table and ",
      "frozen references on CV and Invariance (composite scores only), the scoring syntax, and the ",
      "reproducible code. Requiring more answered items does not change a reliability coefficient's ",
      "estimand. The sensitivity table is descriptive: complete-score omega does not certify a ",
      "partial-response procedure, and SIEVER does not estimate how a partial-response rule changes ",
      "reliability in the population. A factor score uses the observed items of a respondent under ",
      "full information maximum likelihood without proration (Estabrook & Neale, 2013); the minimum ",
      "answered rule is applied to it as well."
    )),
    shiny::selectInput(ns("policy_construct"), "Construct", choices = NULL),
    shiny::selectInput(
      ns("policy_method"), "Score",
      choices = c(
        "Mean of the answered items" = "mean",
        "Prorated sum of the answered items" = "sum",
        "Bartlett factor score (from the fitted measurement model)" = "bartlett",
        "Regression factor score (from the fitted measurement model)" = "regression"
      )
    ),
    shiny::numericInput(ns("policy_count"), "Minimum answered items", 1, min = 1),
    shiny::numericInput(ns("policy_prop"), "Minimum answered proportion", 0, min = 0, max = 1, step = .1),
    shiny::actionButton(ns("policy_apply"), "Keep This Construct's Score Specification"),
    shiny::actionButton(ns("policy_sensitivity"), "Compare Response Policies"),
    shiny::uiOutput(ns("policy_note")),
    shiny::p(
      class = "small text-muted mt-3 mb-1",
      "The specification in force for every construct, with the number of respondents eligible for ",
      "a score under its minimum answered rule on the loaded data (for a factor score, the fitted ",
      "model may score fewer; a model fit on the complete cases scores only those)."
    ),
    DT::DTOutput(ns("policy_specification")),
    DT::DTOutput(ns("policy_table")), DT::DTOutput(ns("policy_cases"))
  )
}

#' Congeneric model planning controls
#' @param ns Namespace.
#' @return UI.
#' @author Ken Kelley
#' @noRd
sieve_congeneric_ui <- function(ns) {
  shiny::tags$details(
    shiny::tags$summary("Plan From Unequal Loadings and Uniquenesses"),
    shiny::p("This separate continuous congeneric plan calls the existing DMAR Factor Analytic planner (ML delta interval). Enter plausible population values rather than treating pilot estimates as known. The sensitivity grid multiplies loadings by .8, 1, and 1.2 and retains the requested assurance. Assurance planning uses Monte Carlo computation for three plans and can take several minutes. The shared Planning FMI for Omega setting applies here too. It describes anticipated information loss for omega, not the fraction of missing responses. Categorical and bootstrap targets require another validated design."),
    shiny::textInput(ns("plan_lambda"), "Population loadings", ".8, .7, .6, .75"),
    shiny::textInput(ns("plan_psi"), "Population uniquenesses", ".36, .51, .64, .4375"),
    shiny::actionButton(ns("plan_congeneric_go"), "Plan Congeneric Precision"),
    shiny::uiOutput(ns("plan_congeneric_note")),
    DT::DTOutput(ns("plan_congeneric_table")),
    DT::DTOutput(ns("plan_congeneric_sensitivity"))
  )
}

#' Wire operational policy, congeneric planning, and score uncertainty
#' @param input,output,session Module objects.
#' @param state Shared state.
#' @param parent Score model result function.
#' @param current Score model currentness function.
#' @return Reactive analysis results.
#' @author Ken Kelley
#' @noRd
sieve_reliability_evidence_server <- function(input, output, session, state, parent, current) {
  shiny::observe({
    m <- state$meta
    shiny::req(m)
    cons <- unique(m$construct[m$status == "active"])
    cons <- cons[sieve_construct_assigned(cons)]
    shiny::updateSelectInput(session, "policy_construct", choices = cons)
  })
  shiny::observeEvent(input$policy_construct, {
    p <- sieve_score_policy_read(shiny::isolate(state$scoring_policies)[[input$policy_construct]])
    shiny::updateSelectInput(session, "policy_method", selected = p$method)
    shiny::updateNumericInput(session, "policy_count", value = p$min_count)
    shiny::updateNumericInput(session, "policy_prop", value = p$min_prop)
  })
  policy_items <- function() {
    m <- shiny::isolate(state$meta)
    cn <- input$policy_construct
    m$item[m$status == "active" & m$construct %in% cn]
  }
  # The last Keep of this panel, with the policy it stored: shown only while
  # that policy is still the one in force, so a later change elsewhere (the
  # Validity tab's score control, a reopened project) never leaves it
  # standing beside a different specification.
  kept <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$policy_apply, {
    its <- policy_items()
    shiny::req(length(its))
    cn <- input$policy_construct
    method <- input$policy_method %||% "mean"
    if (!method %in% names(sieve_score_methods())) method <- "mean"
    held <- sieve_score_policy_read(shiny::isolate(state$scoring_policies)[[cn]], its)
    # The minimum answered rule is checked by scoring the construct under
    # it; a factor score keeps the rule too, so it is checked the same way.
    r <- tryCatch(sieve_composite_score(
      shiny::isolate(state$items), its,
      weights = held$weights,
      method = if (identical(method, "sum")) "sum" else "mean",
      min_count = input$policy_count %||% 1L, min_prop = input$policy_prop %||% 0
    ), error = function(e) list(error = conditionMessage(e)))
    if (!is.null(r$error)) {
      kept(r$error)
      return()
    }
    stored <- sieve_set_score_specification(state, cn,
      method = method, min_count = r$policy$min_count, min_prop = r$policy$min_prop
    )
    label <- unname(sieve_score_methods()[method])
    sieve_log(
      state, paste("Keep the operational score specification of", cn),
      c(
        sprintf(
          "# Construct: %s; operational score: %s; minimum answered count %d, proportion %s.",
          gsub("[\r\n]", " ", cn), label, stored$min_count, sieve_r_literal(stored$min_prop)
        ),
        sprintf(
          "scoring_policies[[%s]] <- %s",
          encodeString(cn, quote = '"'),
          paste(utils::capture.output(dput(stored)), collapse = "")
        ),
        if (sieve_score_is_latent(method)) {
          paste0(
            "# A factor score is read from the fitted correlated-factors model (Validity tab; scoring ",
            "syntax) with this minimum answered rule applied to it."
          )
        } else {
          sprintf(
            "score_policy <- SIEVER::sieve_composite_score(items, items = %s, method = %s, min_count = %dL, min_prop = %s)",
            paste(utils::capture.output(dput(its)), collapse = ""),
            encodeString(method, quote = '"'),
            stored$min_count, sieve_r_literal(stored$min_prop)
          )
        }
      )
    )
    kept(list(construct = cn, policy = stored, text = sprintf(
      "Specification kept for %s: %s; at least %d of %d answered items; %d respondents eligible now%s.",
      cn, label, r$policy$required, length(its), sum(r$eligible),
      if (sieve_score_is_latent(method)) " (the fitted model may score fewer)" else ""
    )))
  })
  # The note says what is in force, from the stored policies, and, while
  # the controls differ from the selected construct's stored
  # specification, what they would keep: no model is fit and nothing is
  # written until Keep.
  output$policy_note <- shiny::renderUI({
    state$meta
    policies <- state$scoring_policies %||% list()
    items <- state$items
    shiny::req(items)
    spec <- sieve_score_specification(state, available = names(items))
    last <- kept()
    last_text <- if (!is.null(last) && identical(policies[[last$construct]], last$policy)) last$text
    cn <- input$policy_construct
    its <- if (length(cn) == 1L && cn %in% names(spec)) spec[[cn]]$items else character(0)
    method <- input$policy_method %||% "mean"
    if (!method %in% names(sieve_score_methods())) method <- "mean"
    count <- input$policy_count %||% 1L
    prop <- input$policy_prop %||% 0
    held <- if (length(its)) spec[[cn]]
    pending <- length(its) && (!identical(method, held$method) ||
      !isTRUE(all.equal(as.numeric(count), as.numeric(held$min_count))) ||
      !isTRUE(all.equal(as.numeric(prop), as.numeric(held$min_prop))))
    preview <- if (pending) {
      r <- tryCatch(sieve_composite_score(items, its,
        weights = held$weights, method = if (identical(method, "sum")) "sum" else "mean",
        min_count = count, min_prop = prop
      ), error = function(e) list(error = conditionMessage(e)))
      if (!is.null(r$error)) {
        sprintf("Not yet kept for %s: %s", cn, r$error)
      } else {
        sprintf(
          "Not yet kept for %s: %s, at least %d of %d answered items; %d respondents would be eligible now%s. Keep applies it.",
          cn, unname(sieve_score_methods()[method]), r$policy$required, length(its), sum(r$eligible),
          if (sieve_score_is_latent(method)) " (the fitted model may score fewer)" else ""
        )
      }
    }
    # A stored policy that had to be repaired on reading (an unknown method
    # or an invalid minimum) is said here until a specification is kept.
    repaired <- sieve_score_policy_issues(policies)
    shiny::div(
      class = "small",
      if (!is.null(last_text)) shiny::p(class = "mb-1", last_text),
      shiny::p(class = "mb-1", shiny::strong("In force: "), sieve_score_policy_sentence(spec, policies)),
      if (length(repaired)) {
        shiny::p(
          class = "mb-1 text-danger", shiny::strong("Needs review: "),
          paste(sprintf("%s: %s", names(repaired), repaired), collapse = " "),
          " Choose the method and Keep to replace it."
        )
      },
      if (!is.null(preview)) shiny::p(class = "mb-1 text-warning-emphasis", preview)
    )
  })
  # The specification in force, with the eligible n of every construct.
  specification <- shiny::reactive({
    state$meta
    state$scoring_policies
    items <- state$items
    shiny::req(items)
    spec <- sieve_score_specification(state, available = names(items))
    shiny::req(length(spec) > 0)
    table <- sieve_score_specification_table(items, spec)
    # The stored method and the repair are shown when a policy needs review;
    # otherwise they say nothing the score column does not.
    if (all(is.na(table$review))) table[, setdiff(names(table), c("requested_method", "review")), drop = FALSE] else table
  })
  output$policy_specification <- DT::renderDT(sieve_inference_table(specification()))
  sensitivity <- shiny::eventReactive(input$policy_sensitivity, {
    its <- policy_items()
    shiny::req(length(its))
    r <- sieve_score_policy_sensitivity(shiny::isolate(state$items), its,
      counts = unique(c(
        1L,
        ceiling(length(its) / 2),
        length(its),
        input$policy_count %||% 1L
      )),
      meta = shiny::isolate(state$meta)
    )
    spec <- list(
      kind = "score_policy_sensitivity",
      observed = its,
      policies = r$policies,
      construct = input$policy_construct
    )
    spec$key <- sieve_hash(spec)
    sieve_register_analysis(
      state,
      list(
        spec = spec,
        estimates = r[c(
          "table",
          "policies",
          "interpretation"
        )],
        status = list(
          code = "ok",
          message = "Descriptive operational-score sensitivity."
        )
      )
    )
    sieve_log(
      state,
      "Compare operational response policies",
      sprintf(
        "SIEVER::sieve_score_policy_sensitivity(items, items = %s, counts = %s, meta = meta)",
        paste(utils::capture.output(dput(its)),
          collapse = ""
        ),
        paste(
          utils::capture.output(dput(vapply(
            r$policies,
            `[[`,
            integer(1),
            "min_count"
          ))),
          collapse = ""
        )
      )
    )
    list(result = r, revision = sieve_revision(state), construct = input$policy_construct)
  })
  policy_now <- shiny::reactive({
    r <- sensitivity()
    # sieve_revision() reads in isolation: the dependency on the dataset,
    # the item data (a respondent removed or restored, an item reverse
    # scored) and the ledger is taken here, so the comparison is marked
    # historical as soon as any of them changes.
    state$dataset_id
    state$items
    state$meta
    if (!sieve_same_revision(
      r$revision,
      sieve_revision(state)
    ) || !identical(
      r$construct,
      input$policy_construct
    )) return(list(error = "Response-policy results are historical; compare again for current inputs."))
    r$result
  })
  output$policy_table <- DT::renderDT(sieve_inference_table(policy_now()))
  output$policy_cases <- DT::renderDT(sieve_inference_table(policy_now()$cases))
  parse_numbers <- function(x) suppressWarnings(as.numeric(strsplit(trimws(x %||% ""), "[,;[:space:]]+")[[1L]]))
  congeneric_request <- shiny::reactive(list(lambda = input$plan_lambda, psi = input$plan_psi,
    assurance = input$plan_assurance, width = input$plan_width %||% .1,
    level = input$conf_level %||% .95, seed = sieve_seed_input(input$plan_seed),
    fmi = sieve_finite_or(input$plan_fmi, 0)))
  congeneric_run <- shiny::eventReactive(input$plan_congeneric_go, {
    request <- congeneric_request()
    lambda <- parse_numbers(input$plan_lambda)
    psi <- parse_numbers(input$plan_psi)
    assurance <- suppressWarnings(as.numeric(input$plan_assurance %||% ""))
    if (length(assurance) != 1L || !is.finite(assurance)) assurance <- NULL
    r <- tryCatch(sieve_plan_congeneric(
      lambda, psi, input$plan_width %||% .1, input$conf_level %||% .95,
      assurance, sieve_seed_input(input$plan_seed), fmi = request$fmi
    ), error = function(e) list(error = conditionMessage(e)))
    if (is.null(r$error)) {
      spec <- list(kind = "congeneric_aipe", planning = r$call, rng = r$rng)
      spec$key <- sieve_hash(spec)
      sieve_register_analysis(
        state,
        list(
          spec = spec,
          estimates = r[c(
            "result",
            "sensitivity",
            "target",
            "warnings", "sampling", "draw_seeds"
          )],
          status = list(
            code = "ok",
            message = "Computed through DMAR's declared model and interval."
          )
        )
      )
      sieve_log(state, "Congeneric AIPE planning and sensitivity", c(
        sprintf("set.seed(%d)", r$rng$seed),
        sprintf(
          "SIEVER::sieve_plan_congeneric(lambda = %s, psi = %s, width = %s, conf_level = %s, assurance = %s, seed = %d, fmi = %s)",
          paste(utils::capture.output(dput(lambda)),
            collapse = ""
          ),
          paste(utils::capture.output(dput(psi)),
            collapse = ""
          ),
          sieve_r_literal(r$call$width),
          sieve_r_literal(r$call$conf_level),
          if (is.null(assurance)) "NULL" else sieve_r_literal(assurance), r$rng$seed, sieve_r_literal(request$fmi)
        )
      ))
    }
    r$request <- request
    r
  })
  congeneric <- shiny::reactive({
    r <- congeneric_run()
    if (!identical(r$request, congeneric_request())) return(list(error = "The planning inputs changed. Press Plan Congeneric Precision for the current population values, FMI and interval settings; no plan was recomputed."))
    r
  })
  output$plan_congeneric_table <- DT::renderDT({
    r <- congeneric()
    sieve_inference_table(if (is.null(r$error)) r$result else r)
  })
  output$plan_congeneric_sensitivity <- DT::renderDT(sieve_inference_table(congeneric()$sensitivity))
  output$plan_congeneric_note <- shiny::renderUI({
    r <- congeneric()
    shiny::div(
      class = "small",
      paste(
        c(
          r$target,
          r$error,
          r$warnings
        ),
        collapse = " "
      )
    )
  })
  score_record <- shiny::eventReactive(input$score_uncertainty_go, {
    shiny::req(isTRUE(current()))
    problem <- sieve_sampling_problem(state, capability = "independent")
    if (!is.null(problem)) stop(problem, call. = FALSE)
    run <- parent()
    shiny::req(run$fit)
    level <- input$conf_level %||% .95
    r <- sieve_factor_score_record(run$fit, input$score_method %||% "Bartlett",
      run$score_data %||% shiny::isolate(state$items),
      conf_level = level, original_rows = run$case_index %||% shiny::isolate(sieve_case_index(state))
    )
    choices <- list(method = r$method, conf_level = level)
    code <- c(
      sieve_cfa_code(run$spec, fit_name = "score_model", model_name = "score_syntax"),
      sprintf(
        "score_uncertainty <- SIEVER::sieve_factor_score_record(score_model, method = %s, data = %s, conf_level = %s, original_rows = %s)",
        sieve_r_literal(r$method$method),
        if (length(run$spec$engine_args$aux)) "score_syntax_data" else "items", sieve_r_literal(level),
        paste(utils::capture.output(dput(r$original_rows)), collapse = "")
      )
    )
    sieve_register_inference(
      state, "factor_score_uncertainty", run, choices,
      r[c("method", "conf_level", "interval_method", "uncertainty", "reason", "warnings", "determinacy", "determinacy_table", "determinacy_reason", "determinacy_target")], code
    )
    list(
      result = r, revision = sieve_revision(state), key = run$spec$key,
      method = input$score_method %||% "Bartlett", conf_level = level
    )
  })
  score_now <- shiny::reactive({
    x <- score_record()
    if (!isTRUE(current()) || !sieve_same_revision(
      x$revision,
      sieve_revision(state)
    ) || !identical(
      x$key,
      parent()$spec$key
    ) || !identical(
      x$method,
      input$score_method %||% "Bartlett"
    ) ||
      !identical(
        x$conf_level,
        input$conf_level %||% .95
      )) return(list(reason = "Score uncertainty is historical; recompute for the current method and model."))
    x$result
  })
  output$score_uncertainty_note <- shiny::renderUI({
    r <- score_now()
    shiny::div(
      class = "small",
      paste(
        c(
          r$method$label,
          sprintf(
            "%g%% %s.",
            100 * (r$conf_level %||% .95),
            r$interval_method %||% "Score interval"
          ),
          sprintf(
            "%d respondents have at least one categorical ML score without a finite estimate for an extreme pattern.",
            r$extreme_patterns %||% 0L
          ),
          r$reason,
          r$determinacy_target,
          r$warnings
        ),
        collapse = " "
      )
    )
  })
  output$score_uncertainty_table <- DT::renderDT({
    r <- score_now()
    tab <- r$uncertainty
    sieve_inference_table(tab %||% list(reason = r$reason))
  })
  output$score_determinacy_table <- DT::renderDT({
    r <- score_now()
    tab <- r$determinacy_table
    if (is.data.frame(tab)) {
      names(tab)[names(tab) == "determinacy"] <- sprintf("%s determinacy", r$determinacy_method)
      return(sieve_inference_table(tab))
    }
    sieve_inference_table(list(reason = r$determinacy_reason %||% "Determinacy unavailable."))
  })
  output$dl_score_uncertainty <- shiny::downloadHandler(
    filename = function() "siever_score_uncertainty.csv",
    content = function(file) {
      x <- score_now()
      shiny::req(x$uncertainty)
      utils::write.csv(x$uncertainty, file, row.names = FALSE)
    }
  )
  list(policy = sensitivity, congeneric = congeneric, scores = score_record)
}

#' SEM inference panels bound to the same fitted model
#' @param input,output,session Module objects.
#' @param state Shared state.
#' @param parent,mediation Fitted result reactive functions.
#' @param current Currentness function.
#' @return Reactive Monte Carlo result.
#' @author Ken Kelley
#' @noRd
sieve_sem_evidence_server <- function(input, output, session, state, parent, mediation, current) {
  # The replications and seed of a Monte Carlo request, compared with the
  # controls shown.
  mc_request <- function() list(B = input$indirect_B %||% 20000L, seed = sieve_seed_input(input$indirect_seed))
  output$sem_row_flow <- DT::renderDT({
    run <- parent()
    sieve_inference_table(run$row_flow)
  })
  mc <- shiny::eventReactive(input$indirect_mc_go, {
    shiny::req(isTRUE(current()))
    run <- mediation()
    shiny::req(run$fit, sieve_has_solution(run))
    r <- sieve_indirect_mc(run$fit, input$indirect_B %||% 20000L, sieve_seed_input(input$indirect_seed))
    if (is.null(r$error)) {
      choices <- list(B = r$B, rng = r$rng, scale = "standardized")
      code <- c(
        sieve_sem_code(run$spec, fit_name = "inference_sem"),
        sprintf("indirect_mc <- SIEVER::sieve_indirect_mc(inference_sem, B = %d, seed = %d)", r$B, r$rng$seed)
      )
      sieve_register_inference(
        state,
        "indirect_mc",
        run,
        choices,
        r[c(
          "table",
          "method",
          "B",
          "counts",
          "rng",
          "warnings", "sampling", "draw_seeds"
        )],
        code
      )
    }
    list(result = r, revision = sieve_revision(state), key = run$spec$key, request = mc_request())
  })
  now <- shiny::reactive({
    r <- mc()
    if (!isTRUE(current()) || !sieve_same_revision(
      r$revision,
      sieve_revision(state)
    ) || !identical(
      r$key,
      mediation()$spec$key
    )) return(list(error = "These effect intervals are historical; refit and compute them again."))
    if (!identical(r$request, mc_request())) {
      return(list(error = paste(
        "These effect intervals were computed with other replications or another seed than those",
        "shown; compute them again for the settings shown."
      )))
    }
    r$result
  })
  output$indirect_mc_table <- DT::renderDT(sieve_inference_table(now()))
  output$indirect_mc_note <- shiny::renderUI({
    r <- now()
    shiny::div(
      class = "small",
      paste(
        c(
          r$error,
          r$method,
          r$warnings
        ),
        collapse = " "
      )
    )
  })
  mc
}

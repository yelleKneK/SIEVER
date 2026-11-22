# mod_network.R
# The Network page, a trial: psychometric network models of the
# included items in psychonetrics, read beside the factor models of the
# other pages. Three models, each on request through sieve_demand_wire()
# with its estimate on its button and never refit inside a display
# reactive: the item network (a Gaussian graphical model, the partial
# correlations among the items), the residual network (the ledger's
# correlated-factors CFA plus a partial correlation network among the
# residuals: the local dependence the factors leave behind), and the latent
# network (the same CFA with a partial correlation network among the
# constructs in place of their correlations). The calculations live in
# fct_network.R; this file places them, records them, and explains them.

#' Network page UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput plotOutput numericInput div tags p strong em
#' @importFrom bslib layout_sidebar sidebar card card_header card_body
#' @importFrom bslib navset_card_tab nav_panel
#' @importFrom DT DTOutput
mod_network_ui <- function(id) {
  ns <- NS(id)
  panel <- function(stem, header_edges, header_fit, extra = NULL) {
    tagList(
      uiOutput(ns(paste0(stem, "_line"))),
      uiOutput(ns(paste0(stem, "_method"))),
      card(
        card_header("The Network"),
        sieve_plot_save_ui(ns(paste0(stem, "_plot"))),
        plotOutput(ns(paste0(stem, "_plot")), height = "460px"),
        uiOutput(ns(paste0(stem, "_plot_note")))
      ),
      card(
        card_header(header_edges),
        # What the intervals cover, at the point of use.
        p(
          class = "small text-muted mb-1",
          "The intervals are Wald intervals for the model the search selected; they do not include the uncertainty of the selection itself."
        ),
        DTOutput(ns(paste0(stem, "_edges")), fill = FALSE),
        uiOutput(ns(paste0(stem, "_code")))
      ),
      extra,
      card(
        card_header(header_fit),
        DTOutput(ns(paste0(stem, "_fit")), fill = FALSE),
        uiOutput(ns(paste0(stem, "_fit_note")))
      )
    )
  }
  layout_sidebar(
    sidebar = sidebar(
      width = 320,
      title = "Network models",
      p(
        "Estimate partial correlations among items, among CFA residuals, or among constructs."
      ),
      p(
        class = "small text-muted",
        "Choose a model below and press its estimate button. The button gives an estimated wait.",
        " The estimator follows the study setting unless changed here."
      ),
      selectInput(ns("scope"), "Items to Analyze", choices = c("All Included Items" = "all")),
      p("A network of a subset is conditional on that subset only; its edges can differ from the full-pool network.", class = "small text-muted"),
      numericInput(ns("alpha"), "Significance level for keeping an edge (alpha)",
        value = 0.01, min = 0.0005, max = 0.5, step = 0.005
      ),
      sieve_more_below(
        "Alpha controls which edges the search keeps. Changing it requires a new estimate.",
        p(
          "The search estimates every edge, removes those not significant at alpha, then adds",
          " edges back by their modification index while each is significant at alpha. With a",
          " likelihood estimator, each addition must also improve BIC; under DWLS, the significance",
          " rule alone is used because BIC is undefined (Epskamp, 2020; .01 is the package default).",
          " With MLR, selection uses normal-theory ML tests; the selected model is then refit under",
          " MLR. Its Wald intervals describe the selected model and do not account for the search."
        ),
        label = "Edge selection and uncertainty"
      ),
      sieve_estimator_panel(ns)
    ),
    sieve_optional_tab_note(paste(
      "a trial of psychometric network models: the partial correlation network of the items,",
      "the residual network beside the CFA (local dependence), and the latent network of the constructs"
    )),
    uiOutput(ns("gate")),
    uiOutput(ns("status")),
    p(class = "small mb-1",
      "Edges are partial correlations. Read their intervals as well as the plot; an absent edge is a selection result."
    ),
    # The explanation, visible by default and collapsible: static text, so
    # it is in the interface itself, under the one visible line above it.
    tags$details(
      class = "sieve-intro mb-3", open = NA,
      tags$summary(strong("What a Network Model Shows, and What It Does Not")),
      sieve_network_about()
    ),
    navset_card_tab(
      id = ns("tabs"),
      nav_panel(
        "Item Network",
        value = "ggm",
        panel(
          "ggm",
          "Partial Correlations Among the Items, With Standard Errors and Intervals",
          "Fit of the Sparse Item Network",
          extra = tagList(
            card(
              card_header("Node Strength, Expected Influence, and Communities"),
              DTOutput(ns("ggm_centrality"), fill = FALSE),
              uiOutput(ns("ggm_communities"))
            )
          )
        )
      ),
      nav_panel(
        "Residual Network",
        value = "rnm",
        panel(
          "rnm",
          "Partial Correlations Among the Residuals, Beyond the Constructs",
          "Fit of the CFA With Its Residual Network",
          extra = tagList(
            card(
              card_header("The Measurement Model Behind the Residual Network"),
              DTOutput(ns("rnm_loadings"), fill = FALSE),
              DTOutput(ns("rnm_phi"), fill = FALSE),
              uiOutput(ns("rnm_communities"))
            )
          )
        )
      ),
      nav_panel(
        "Latent Network",
        value = "lnm",
        panel(
          "lnm",
          "Partial Correlations Among the Constructs",
          "Fit of the Latent Network Model",
          extra = tagList(
            card(
              card_header("The Construct Correlations the Latent Network Implies"),
              DTOutput(ns("lnm_implied"), fill = FALSE),
              uiOutput(ns("lnm_implied_note")),
              DTOutput(ns("lnm_loadings"), fill = FALSE)
            )
          )
        )
      )
    )
  )
}

#' Network page server
#' @param id Module id.
#' @param state The shared application state.
#' @param start How an estimate is started: `NULL` for a separate R
#'   process (`sieve_network_worker_start()`; callr is imported), or a
#'   function with their arguments that returns a handle, such as
#'   `sieve_network_inline_start()`, the in-process start the tests use and
#'   the fallback should callr ever be missing.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive req renderUI renderPlot observe isolate tagList
#' @importFrom DT renderDT datatable formatRound formatSignif
mod_network_server <- function(id, state, start = NULL) {
  moduleServer(id, function(input, output, session) {
    # The search runs in a separate R process, so a long one leaves the
    # session usable and can be canceled (on 68 items it runs longer than
    # an hour). callr is imported, so the in-process start is only a
    # fallback should it be missing.
    start <- start %||% sieve_network_worker_start
    in_process <- isTRUE(attr(start, "in_process"))
    estimator_choice <- sieve_estimator_wire(input, output, state)
    alpha <- reactive({
      a <- sieve_finite_or(input$alpha, 0.01)
      min(max(a, 0.0005), 0.5)
    })
    have_engine <- sieve_have("psychonetrics")

    # The included items present in the data, and the ledger's constructs
    # with at least two of them (the factors of the residual and latent
    # networks).
    scope_groups <- reactive({
      req(state$meta, state$items)
      m <- state$meta
      m <- m[m$status == "active" & m$item %in% names(state$items), , drop = FALSE]
      groups <- list(all = list(label = "All Included Items", items = m$item))
      for (cn in unique(m$construct[sieve_construct_assigned(m$construct)])) {
        id <- paste0("construct:", cn)
        groups[[id]] <- list(label = paste("Construct:", cn), items = m$item[m$construct %in% cn])
      }
      for (group in sieve_facet_groups(m)) groups[[paste0("facet:", group$id)]] <- list(label = paste("Facet:", group$label), items = group$items)
      groups
    })
    observe({
      groups <- scope_groups()
      chosen <- isolate(input$scope %||% "all")
      if (!chosen %in% names(groups)) chosen <- "all"
      updateSelectInput(session, "scope", choices = stats::setNames(names(groups),
        vapply(groups, `[[`, character(1), "label")), selected = chosen)
    })
    active_items <- reactive({
      groups <- scope_groups()
      groups[[input$scope %||% "all"]]$items %||% groups$all$items
    })
    lambda_plan <- reactive({
      req(state$meta)
      sieve_network_lambda(state$meta, active_items())
    })

    # ---- The three models on request (fct_ondemand.R) ----------------------
    kinds <- c(ggm = "network_ggm", rnm = "network_rnm", lnm = "network_lnm")
    titles <- c(ggm = "Item network", rnm = "Residual network", lnm = "Latent network")
    nouns <- c(ggm = "The item network", rnm = "The residual network", lnm = "The latent network")
    actions <- c(ggm = "Estimate the item network", rnm = "Estimate the residual network", lnm = "Estimate the latent network")

    model_key <- function(stem) {
      reactive({
        req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
        its <- active_items()
        if (stem != "ggm") {
          plan <- lambda_plan()
          its <- if (length(plan$items)) plan$items else its
        }
        sieve_demand_key(state,
          list(model = kinds[[stem]], estimator = estimator_choice(), alpha = alpha(), scope = input$scope %||% "all"),
          items = its, facets = FALSE
        )
      })
    }
    model_plan <- function(stem) {
      reactive({
        req(state$meta, state$items)
        its <- active_items()
        plan <- if (stem == "ggm") NULL else lambda_plan()
        if (!is.null(plan) && length(plan$items)) its <- plan$items
        scoped_meta <- state$meta
        scoped_meta$status[!scoped_meta$item %in% its] <- "parked"
        spec <- sieve_lavaan_spec(scoped_meta, estimator = estimator_choice())
        p <- length(its)
        engine <- sieve_network_engine(spec$estimator, spec$missing, intersect(spec$ordered %||% character(0), its),
          sieve_network_data(state$items, its))
        # The search's own wait (fct_network.R): its times measured at 9 to
        # 48 items, at this machine's speed, plus the start of the R
        # process it runs in.
        seconds <- sieve_network_wait(kinds[[stem]], p, engine, worker = !in_process)
        refused <- if (!have_engine) {
          paste(
            "Package psychonetrics is not installed. Install it from CRAN",
            "(install.packages(\"psychonetrics\")) and restart SIEVER to estimate network models."
          )
        } else if (p < 3L) {
          "A network needs at least three included items."
        } else if (!is.null(plan) && !is.na(plan$reason)) {
          plan$reason
        } else if (stem == "lnm" && length(plan$latents) < 2L) {
          paste(
            "A latent network needs at least two constructs with two or more included items each;",
            "with one construct there is nothing for the constructs' network to connect."
          )
        } else if (identical(engine$estimator, "DWLS")) {
          sieve_wlsmv_guard(its, state$items, ordered = engine$ordered, estimator = "WLSMV")
        }
        list(
          estimator = engine$lavaan_estimator, p = p, seconds = seconds, engine = engine,
          confirm_long = is.finite(seconds) && seconds > 15 * 60,
          dwls_note = if (identical(engine$estimator, "DWLS") &&
              sieve_network_empty_cells(state$items[, engine$ordered, drop = FALSE]))
            "Some ordered-item cross tables have empty cells. The polychoric estimate may fail. ML on standardized item codes is an alternative; choose a continuous-item estimator explicitly.",
          label = sieve_duration_label(seconds),
          # Said before the press: MLR asked for on
          # incomplete continuous data is estimated by FIML with normal
          # theory standard errors, because psychonetrics' robust correction
          # needs complete data.
          fiml_note = if (identical(engine$estimator, "FIML") && identical(toupper(spec$estimator %||% ""), "MLR")) {
            paste(
              "Incomplete continuous data are estimated by full information maximum likelihood with",
              "normal theory standard errors; psychonetrics applies its robust correction to complete data only."
            )
          },
          not_modeled = if (!is.null(plan)) plan$not_modeled,
          progress = sprintf("%s: %s, %d items", titles[[stem]], engine$estimator, p),
          npar = NULL,
          detail = switch(stem,
            ggm = "Estimating the saturated Gaussian graphical model in psychonetrics, pruning, then stepping up.",
            rnm = "Estimating the CFA with a residual network in psychonetrics, pruning the residual edges, then stepping up.",
            lnm = "Estimating the CFA with a latent network in psychonetrics, pruning the latent edges, then stepping up."
          ),
          sentence = sprintf(
            "Estimating the %s of %d items (%s, alpha = %s)%s is estimated to take %s",
            tolower(titles[[stem]]), p, engine$estimator, format(alpha()),
            if (in_process) "" else ", in a separate R process that can be canceled,",
            sieve_duration_label(seconds)
          ),
          refused = refused
        )
      })
    }
    # An estimate starts outside this process with the data, the ledger and
    # the settings, and keeps what it started from for its record.
    start_for <- function(stem) {
      function(key, plan) {
        sieve_ensure_dataset(state)
        launched <- proc.time()[["elapsed"]]
        items <- shiny::isolate(state$items)
        meta <- shiny::isolate(state$meta)
        scope <- shiny::isolate(active_items())
        meta$status[!meta$item %in% scope] <- "parked"
        # The included items cross the process boundary as plain numbers: a
        # column read from a .sav file (haven_labelled) needs haven to be
        # read, which the separate process does not load.
        included <- intersect(as.character(meta$item[meta$status == "active"]), names(items))
        handle <- start(sieve_network_data(items, included), meta,
          kind = kinds[[stem]], estimator = key$choices$estimator %||% "auto",
          alpha = key$choices$alpha %||% 0.01
        )
        handle$launched <- launched
        handle$context <- list(revision = sieve_revision(state), inputs = sieve_replay_inputs(state))
        handle
      }
    }
    # The estimate's records, made in this process once it is back: the
    # analysis registry with the inputs it started from, and, while the
    # data, items and rows are still those, its step of the session script
    # and its code; the measured time, for the next estimate's wait.
    finish_for <- function(stem) {
      function(key, plan, result, handle, current) {
        r <- result$value
        r$revision <- handle$context$revision
        if (is.null(r$error) && sieve_has_solution(r$status)) {
          sieve_network_wait_note(kinds[[stem]], plan$p, plan$engine, result$engine_seconds,
            start_seconds = if (in_process) NA_real_ else proc.time()[["elapsed"]] - handle$launched - result$engine_seconds
          )
          # The record of the analysis registry: the specification, the
          # stored results at full precision, and the methods used.
          r$estimates <- list(
            fit = r$fit$selected, fit_all = r$fit$all, edges = r$edges, network = r$omega,
            centrality = r$centrality, loadings = r$loadings, phi = r$phi,
            implied_cor = r$implied_cor, n = r$n, n_input = r$n_input, n_complete = r$n_complete,
            communities = if (!is.null(r$communities)) r$communities$membership
          )
          r$methods <- r$spec$methods
          # The record names the inputs the estimate started from, which an
          # edit made while it ran does not change.
          r$inputs <- handle$context$inputs
          r$analysis_id <- sieve_register_analysis(state, r, note = sprintf(
            "%s (psychonetrics, %s, alpha = %s)", titles[[stem]], r$spec$engine$estimator, format(r$spec$alpha)
          ))
          r$inputs <- NULL
          # The session script and Show code describe the data as they are
          # now, so a result whose data, items or rows changed while it ran
          # is kept as from before without a step of the script.
          if (!isTRUE(current)) {
            return(r)
          }
          sieve_log(
            state,
            sprintf(
              "%s: %s in psychonetrics; edges pruned at alpha = %s and stepped up%s",
              titles[[stem]], r$spec$engine$label, format(r$spec$alpha),
              if (identical(r$spec$engine$criterion, "bic")) " by BIC" else " by the significance rule"
            ),
            r$code
          )
          r$code_view <- sieve_code_snapshot(state, context = c(
            sprintf("Items: %s.", paste(r$spec$observed, collapse = ", ")),
            sprintf("Estimator: %s (%s); alpha = %s; Wald %d%% intervals from the standard errors.",
              r$spec$engine$estimator, r$spec$engine$label, format(r$spec$alpha), as.integer(100 * r$spec$level)),
            if (stem != "ggm") sprintf("Construct factors (%s): %s.",
              if (stem == "lnm") "Delta fixed at the identity; variances implied by the network" else "variances fixed at 1",
              paste(r$spec$name_map, collapse = ", ")),
            "Partial correlations, standard errors, fit measures, strength and expected influence are stored at full precision and displayed to three decimal places."
          ), packages = sieve_network_packages())
        }
        r
      }
    }
    demands <- lapply(stats::setNames(names(kinds), names(kinds)), function(stem) {
      sieve_demand_wire(
        input, session, state,
        id = paste0(stem, "_go"), what = titles[[stem]],
        key = model_key(stem), plan = model_plan(stem), compute = NULL,
        # The measured time is noted by finish_for(), against the search's
        # own reference times, not the CFA rate of the kind.
        kind = NULL, noun = nouns[[stem]], verb = "estimated",
        action = actions[[stem]], refit = "Update",
        stale_lead = "The data, the item pool or a setting changed since this network was estimated: estimate it again. ",
        background = list(start = start_for(stem), finish = finish_for(stem), in_process = function() in_process)
      )
    })
    plans <- lapply(stats::setNames(names(kinds), names(kinds)), model_plan)

    # The result on screen for a model, with what changed since when it is
    # from before, or NULL.
    shown <- function(stem) {
      st <- sieve_outcome(demands[[stem]]$status())
      if (!st$ready || !st$ok) {
        return(NULL)
      }
      r <- st$value$value
      if (is.null(r)) {
        return(NULL)
      }
      # A result from before stays on the page while its update runs.
      if (identical(st$value$state, "stale") || identical(st$value$before, "stale")) r$stale <- st$value$sentence
      r
    }
    solved <- function(stem) {
      r <- shown(stem)
      if (is.null(r) || is.null(r$omega) || !sieve_has_solution(r$status)) NULL else r
    }

    # ---- One Update for the page (fct_page_update.R) -----------------------
    page_update <- sieve_update_wire(input, session, function() {
      list(
        ggm = sieve_update_demand_entry(demands$ggm, titles[["ggm"]], plans$ggm),
        rnm = sieve_update_demand_entry(demands$rnm, titles[["rnm"]], plans$rnm),
        lnm = sieve_update_demand_entry(demands$lnm, titles[["lnm"]], plans$lnm)
      )
    })

    output$gate <- renderUI({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(sieve_gate(state))
      }
      if (!have_engine) {
        return(div(
          class = "alert alert-warning",
          strong("Package psychonetrics is not installed. "),
          "The network models are estimated in psychonetrics (Epskamp, 2020). Install it from CRAN with ",
          tags$code("install.packages(\"psychonetrics\")"),
          " and restart SIEVER. Nothing else in SIEVER depends on it."
        ))
      }
      NULL
    })
    output$status <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      tagList(page_update$line(), page_update$note())
    })

    # ---- Each model's line, method statement, figure, tables and code ------
    for (stem in names(kinds)) {
      local({
        s <- stem
        demand <- demands[[s]]
        output[[paste0(s, "_line")]] <- renderUI({
          req(isTRUE(sieve_confirmation_status(state)$confirmed))
          r <- shown(s)
          pl <- tryCatch(plans[[s]](), shiny.silent.error = function(e) NULL)
          # The items the residual and latent networks leave out, named
          # before the press (from the plan) and beside a result (from its
          # specification), as the CFA page names its own; and, before the
          # press, how incomplete data will be treated.
          left_out <- sieve_network_not_modeled_text(if (!is.null(r)) r$spec$not_modeled else pl$not_modeled)
          tagList(
            demand$line(),
            if (length(pl$dwls_note)) div(class = "alert alert-warning small", pl$dwls_note),
            if (length(left_out)) div(class = "alert alert-warning small", paste(left_out, collapse = " ")),
            if (is.null(r) && length(pl$fiml_note)) p(class = "small text-muted mb-1", pl$fiml_note),
            if (!is.null(r) && !is.null(r$error) && is.null(r$omega)) {
              NULL
            } else if (!is.null(r) && !sieve_has_solution(r$status)) {
              div(class = "alert alert-danger small", strong("No solution. "), r$status$message)
            } else if (!is.null(r) && length(r$warnings)) {
              div(class = "alert alert-warning small",
                strong("The engine raised a warning. "), paste(r$warnings, collapse = " "))
            }
          )
        })
        output[[paste0(s, "_method")]] <- renderUI({
          r <- if (isTRUE(sieve_confirmation_status(state)$confirmed)) solved(s)
          if (is.null(r)) {
            return(NULL)
          }
          div(class = "small text-muted mb-2 sieve-method-line", sieve_network_method_text(r))
        })
        draw <- function() {
          r <- solved(s)
          req(r)
          groups <- if (s == "lnm") NULL else r$groups
          labels <- if (s == "lnm") unname(r$spec$name_map[rownames(r$omega)]) else unname(r$labels[rownames(r$omega)])
          sieve_network_plot(r$omega, labels = labels, groups = groups,
            palette = shiny::isolate(state$palette) %||% "nd")
          NULL
        }
        output[[paste0(s, "_plot")]] <- renderPlot(alt = "Network model of the items, with an edge for each nonzero estimated partial correlation.", {
          draw()
        }, res = 96)
        sieve_plot_save_server(paste0(s, "_plot"), output, session, state,
          draw = draw, file_stem = paste0("network_", s),
          what = sprintf("the %s figure", tolower(titles[[s]])))
        output[[paste0(s, "_plot_note")]] <- renderUI({
          r <- solved(s)
          if (is.null(r)) {
            return(div(class = "small text-muted", sprintf(
              "The figure is drawn once %s is estimated.", tolower(nouns[[s]])
            )))
          }
          n_edges <- sum(r$edges$estimated)
          n_pairs <- nrow(r$edges)
          sieve_more_below(
            sprintf(paste(
              "%d of %d possible edges were kept. Blue edges are positive, red edges negative;",
              "width shows the size of the partial correlation."
            ), n_edges, n_pairs),
            tags$p(class = "mb-1", paste(
              "Node colors identify the ledger's constructs. The other edges were set to zero by the search.",
              "What the picture does not show: the placement of the nodes is a layout (qgraph's spring layout),",
              "so distance on the page carries no quantity, and two layouts of the same matrix can look different;",
              "the width of an edge is its point estimate, not its precision, so read the table with the",
              "standard errors and intervals before reading a thick edge as established; and a missing edge is a",
              "partial correlation the search set to zero at alpha, not a demonstrated independence."
            )),
            label = "More on reading the figure"
          )
        })
        output[[paste0(s, "_edges")]] <- renderDT({
          r <- solved(s)
          req(r, r$edges)
          e <- r$edges
          df <- data.frame(
            from = e$from_label, to = e$to_label, estimate = e$estimate, se = e$se,
            lower = e$lower, upper = e$upper, p = e$p,
            status = ifelse(e$estimated, "estimated", "set to zero by the search"),
            stringsAsFactors = FALSE
          )
          level <- as.integer(100 * (attr(e, "level") %||% 0.95))
          sieve_dt(df,
            rownames = FALSE, selection = "none",
            options = list(dom = "tp", pageLength = 15),
            colnames = c(
              "From", "To", "Partial correlation", "SE",
              sprintf("%d%% CI lower", level), sprintf("%d%% CI upper", level), "p", "Edge"
            )
          ) |>
            formatRound(c("estimate", "se", "lower", "upper"), 3) |>
            formatSignif("p", 2)
        })
        output[[paste0(s, "_fit")]] <- renderDT({
          r <- solved(s)
          req(r, r$fit)
          m <- sieve_network_selected(r$fit)
          df <- sieve_fit_table(m, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea_lo", "rmsea_hi", "srmr", "bic", "npar"))
          df <- df[!is.na(df$Value) | df$Statistic %in% c("BIC"), , drop = FALSE]
          sieve_dt(df,
            rownames = FALSE, selection = "none",
            options = list(dom = "t", pageLength = 20)
          ) |>
            formatRound("Value", 3)
        })
        output[[paste0(s, "_fit_note")]] <- renderUI({
          r <- solved(s)
          req(r, r$fit)
          m <- sieve_network_selected(r$fit)
          family <- attr(m, "family") %||% "standard"
          sieve_more_below(
            paste0(
              "Fit indices from psychonetrics for the selected sparse model (",
              switch(family,
                robust = "the robust CFI, TLI and RMSEA and the scaled chi square under MLR",
                scaled = "the scaled CFI, TLI, RMSEA and chi square under DWLS; psychonetrics gives no interval for the scaled RMSEA, so the interval shown belongs to the unscaled RMSEA",
                "the standard indices"
              ),
              "). The RMSEA interval is at ", sieve_ci_percent(sieve_fit_ci_level(m)),
              ", the level of every RMSEA interval SIEVER shows (90% by default, DMAR's and lavaan's, whose limits ",
              "settle the tests of close and not close fit at .05; the page's other intervals are at ",
              sieve_ci_percent(r$spec$level %||% 0.95), "): it is computed from psychonetrics' chi square, degrees of ",
              "freedom and N by DMAR::sem_fit_measures(), which at 90% gives psychonetrics' own limits."
            ),
            tags$p(class = "mb-1",
              if (identical(r$kind, "network_ggm")) {
                paste(
                  "A saturated network fits perfectly by construction (zero degrees of freedom), so the fit here is the",
                  "fit of the sparse network: how well the edges the search kept reproduce the covariances. BIC is",
                  "undefined under DWLS."
                )
              } else if (identical(r$kind, "network_rnm")) {
                paste(
                  "Compare with the CFA page. This model adds residual edges to that CFA; improved fit can reflect",
                  "the local dependence those edges model."
                )
              } else {
                paste(
                  "A saturated latent network has the same fit as the CFA because the two models are equivalent.",
                  "Removing an edge constrains two constructs to be conditionally independent given the others."
                )
              }
            ),
            label = "What model fit means for this network"
          )
        })
        output[[paste0(s, "_code")]] <- renderUI({
          st <- demand$status()
          # While an estimate runs, the code is that of what is on screen.
          if (identical(st$state, "running")) st$state <- st$before %||% "not_run"
          sieve_code_view(st$value$code_view, st, ns = session$ns,
            title = sprintf("Show code: %s", tolower(titles[[s]])),
            readout = switch(s,
              ggm = c(
                "net_ggm_pars[net_ggm_pars$matrix == \"omega\" & !net_ggm_pars$fixed, c(\"var1\", \"var2\", \"est\", \"se\", \"p\")]",
                "net_ggm_fit[net_ggm_fit$Measure %in% c(\"chisq\", \"df\", \"cfi\", \"tli\", \"rmsea\", \"srmr\", \"bic\"), ]  # the RMSEA interval at the RMSEA level is net_ggm_rmsea, above"
              ),
              rnm = c(
                "net_rnm_pars[net_rnm_pars$matrix == \"omega_epsilon\" & !net_rnm_pars$fixed, c(\"var1\", \"var2\", \"est\", \"se\", \"p\")]  # residual edges",
                "net_rnm_pars[net_rnm_pars$matrix %in% c(\"lambda\", \"sigma_zeta\") & !net_rnm_pars$fixed, ]  # loadings and Phi"
              ),
              lnm = c(
                "net_lnm_pars[net_lnm_pars$matrix == \"omega_zeta\", c(\"var1\", \"var2\", \"est\", \"se\", \"p\", \"fixed\")]",
                "net_lnm_phi  # the construct correlations the sparse latent network implies"
              )
            ))
        })
      })
    }

    # ---- The item network's centrality and communities ---------------------
    centrality_table <- function(r) {
      c_ <- r$centrality
      sieve_dt(
        data.frame(
          node = c_$label, strength = c_$strength, ei = c_$expected_influence, degree = c_$degree,
          stringsAsFactors = FALSE
        ),
        rownames = FALSE, selection = "none",
        options = list(dom = "t", pageLength = 50),
        colnames = c("Node", "Strength (sum of |edges|)", "Expected influence (signed sum)", "Edges")
      ) |>
        formatRound(c("strength", "ei"), 3)
    }
    communities_ui <- function(r, what) {
      cm <- r$communities
      if (is.null(cm)) {
        return(NULL)
      }
      tab <- as.data.frame.matrix(cm$table)
      tab <- cbind(Community = rownames(tab), tab, stringsAsFactors = FALSE)
      rownames(tab) <- NULL
      tagList(
        div(
          class = "small mt-3 mb-1",
          strong("Communities (walktrap, 4 steps) against the constructs. "),
          sprintf(
            paste(
              "%d communities among %d nodes%s; %d of the %d items with a construct sit in the community",
              "where most of their construct's items sit (modularity %s)."
            ),
            cm$n_communities, nrow(cm$membership),
            if (length(cm$isolated)) sprintf(" (%d with no edge: %s)", length(cm$isolated), paste(cm$isolated, collapse = ", ")) else "",
            cm$agreement, cm$n_items, formatC(cm$modularity, digits = 3, format = "f")
          )
        ),
        tags$table(
          class = "table table-sm small w-auto",
          tags$thead(tags$tr(lapply(names(tab), tags$th))),
          tags$tbody(lapply(seq_len(nrow(tab)), function(i) tags$tr(lapply(tab[i, ], function(v) tags$td(as.character(v))))))
        ),
        sieve_more_below(
          "Strength and expected influence summarize a node's edges. Neither measures how well an item represents its construct.",
          tags$p(class = "mb-1", paste(
            "Strength is the sum of the absolute partial correlations at a node (Opsahl, Agneessens & Skvoretz, 2010);",
            "expected influence is the same sum with signs (Robinaugh, Millner & McNally, 2016), so the two differ",
            "only where a node carries negative edges. Both describe the estimated network and inherit the sampling",
            "error of every edge (Epskamp, Borsboom & Fried, 2018); neither is a loading, and a node with high strength",
            "is a node with many or large partial correlations, which is not the same as a good indicator of a construct."
          )),
          tags$p(class = "mb-1", paste(
            "The communities come from a random walk on the absolute edge weights (Pons & Latapy, 2006, through igraph),",
            "the algorithm exploratory graph analysis applies to a psychometric network to count its dimensions",
            "(Golino & Epskamp, 2017). A community is a dense region of the estimated network, not a factor;",
            if (identical(what, "rnm")) {
              paste(
                "in a residual network it groups items whose residuals still depend on each other after their",
                "constructs are modeled, which is the local dependence the CFA page's residual correlations hint at."
              )
            } else {
              paste(
                "when the communities reproduce the constructs, the partial correlations and the factor model agree",
                "about which items go together, which is reassuring but is not a test of either model, because a",
                "one-factor model and a fully connected network imply the same covariances (van Bork et al., 2021)."
              )
            }
          )),
          label = "More on strength, expected influence and communities"
        )
      )
    }
    output$ggm_centrality <- renderDT({
      r <- solved("ggm")
      req(r, r$centrality)
      centrality_table(r)
    })
    output$ggm_communities <- renderUI({
      r <- solved("ggm")
      req(r)
      communities_ui(r, "ggm")
    })

    # ---- The residual network's measurement model --------------------------
    loadings_table <- function(r, caption = NULL) {
      ld <- r$loadings
      sieve_dt(
        data.frame(item = ld$label, factor = ld$factor, estimate = ld$estimate, se = ld$se,
          std = ld$std, se_std = ld$se_std, p = ld$p, stringsAsFactors = FALSE),
        rownames = FALSE, selection = "none",
        options = list(dom = "tp", pageLength = 25),
        colnames = c("Item", "Construct", "Loading (engine scale)", "SE", "Standardized loading", "SE (std)", "p"),
        caption = caption
      ) |>
        formatRound(c("estimate", "se", "std", "se_std"), 3) |>
        formatSignif("p", 2)
    }
    output$rnm_loadings <- renderDT({
      r <- solved("rnm")
      req(r, r$loadings)
      loadings_table(r, caption = paste(
        "Loadings with the construct variances fixed at 1 (the CFA page's identification) and the items standardized",
        "before estimation: the engine-scale loading is the completely standardized loading, comparable with the",
        "standardized loadings of the CFA page."
      ))
    })
    output$rnm_phi <- renderDT({
      r <- solved("rnm")
      req(r, r$phi)
      ph <- r$phi
      z <- stats::qnorm(0.975)
      sieve_dt(
        data.frame(from = ph$from, to = ph$to, estimate = ph$estimate, se = ph$se,
          lower = ph$estimate - z * ph$se, upper = ph$estimate + z * ph$se, stringsAsFactors = FALSE),
        rownames = FALSE, selection = "none",
        options = list(dom = "t", pageLength = 25),
        colnames = c("Construct", "Construct", "Correlation (Phi)", "SE", "95% CI lower", "95% CI upper"),
        caption = "The construct correlations of the CFA with the residual network in place; compare with Phi on the CFA page."
      ) |>
        formatRound(c("estimate", "se", "lower", "upper"), 3)
    })
    output$rnm_communities <- renderUI({
      r <- solved("rnm")
      req(r)
      tagList(
        div(class = "small text-muted mt-2", paste(
          "Residual edges are the dependence between two items that their constructs do not explain:",
          "shared wording, a shared method, a facet the ledger does not name, or an item that belongs",
          "elsewhere. Each is the partial correlation of two residuals given the other residuals and the factors."
        )),
        communities_ui(r, "rnm")
      )
    })

    # ---- The latent network's implied correlations -------------------------
    output$lnm_implied <- renderDT({
      r <- solved("lnm")
      req(r, r$implied_cor)
      S <- r$implied_cor
      labels <- unname(r$spec$name_map[rownames(S)])
      df <- as.data.frame(S)
      names(df) <- labels
      df <- cbind(Construct = labels, df, stringsAsFactors = FALSE)
      sieve_dt(df, rownames = FALSE, selection = "none", options = list(dom = "t", pageLength = 25)) |>
        formatRound(labels, 3)
    })
    output$lnm_implied_note <- renderUI({
      r <- solved("lnm")
      req(r)
      sieve_more_below(
        "These are the construct correlations implied by the selected latent network, rather than its partial-correlation edges.",
        tags$p(class = "mb-1", paste(
        "The correlation matrix of the constructs that the sparse latent network implies: the covariance matrix",
        "Delta (I - Omega)^-1 Delta, standardized (psychonetrics fixes Delta at the identity, so the construct",
        "variances are the diagonal of (I - Omega)^-1). When no latent edge was removed this is exactly the CFA's Phi;",
        "where an edge was removed, the model constrains the pair to be conditionally independent given the others.",
        "Their implied correlation is obtained from the full inverse matrix and need not be zero."
        )),
        label = "How implied correlations differ from network edges"
      )
    })
    output$lnm_loadings <- renderDT({
      r <- solved("lnm")
      req(r, r$loadings)
      loadings_table(r, caption = paste(
        "psychonetrics scales a latent network with its scaling matrix fixed at the identity, so the construct",
        "variances are not 1 and the engine-scale loadings are not on the CFA page's scale; compare the",
        "standardized loadings."
      ))
    })

    sieve_code_navigation(input, session, state)
    invisible(NULL)
  })
}

#' The one-line method statement of an estimated network model
#'
#' @param r A result of `sieve_network_estimate()` with a solution.
#' @return One string.
#' @author Ken Kelley
#' @noRd
sieve_network_method_text <- function(r) {
  m <- sieve_network_selected(r$fit)
  num <- function(x) if (length(x) == 1L && is.finite(x)) sprintf("%.3f", x) else "n/a"
  # The version that made the estimate, kept with it; the installed one
  # only for a result saved without its versions.
  version <- r$engine_versions$psychonetrics %||%
    tryCatch(as.character(utils::packageVersion("psychonetrics")), error = function(e) "?")
  # The rows the engine used, with the complete cases beside them under
  # FIML, where the two differ.
  n_text <- if (identical(r$spec$engine$missing, "fiml") && length(r$n_complete) == 1L) {
    sprintf("n = %d (FIML: every row with an observed item; %d complete cases)", as.integer(r$n), as.integer(r$n_complete))
  } else if (length(r$n_input) == 1L) {
    sprintf("n = %d complete cases of %d rows", as.integer(r$n), as.integer(r$n_input))
  } else {
    sprintf("n = %d", as.integer(r$n))
  }
  # The RMSEA interval named by its own family when it is not the RMSEA's
  # (under DWLS psychonetrics gives only the standard interval).
  ci_family <- attr(m, "rmsea_ci_family") %||% sieve_fit_family(m, "index")
  ci_words <- sieve_rmsea_ci_words(sieve_fit_ci_level(m))
  ci_text <- if (!identical(ci_family, sieve_fit_family(m, "index")) && !is.na(ci_family)) {
    sprintf(", %s of the %s RMSEA [%s, %s]", ci_words, ci_family, num(m[["rmsea_lo"]]), num(m[["rmsea_hi"]]))
  } else {
    sprintf(", %s [%s, %s]", ci_words, num(m[["rmsea_lo"]]), num(m[["rmsea_hi"]]))
  }
  what <- switch(r$kind,
    network_ggm = "Gaussian graphical model of the items",
    network_rnm = "Residual network model (correlated-factors CFA plus a residual network)",
    network_lnm = "Latent network model (CFA with a network among the constructs)"
  )
  n_edges <- sum(r$edges$estimated)
  sprintf(
    paste0(
      "%s in psychonetrics %s: %s; %d items%s; %s; the search kept %d of %d possible edges ",
      "(prune at alpha = %s, then step up%s). Fit: %s %s, %s %s%s, SRMR %s."
    ),
    what, version, r$spec$engine$label, length(r$spec$observed),
    if (length(r$spec$latents)) sprintf(", %d construct factors (%s)", length(r$spec$latents),
      if (identical(r$kind, "network_lnm")) "Delta fixed at the identity" else "variances fixed at 1") else "",
    n_text, n_edges, nrow(r$edges), format(r$spec$alpha),
    if (identical(r$spec$engine$criterion, "bic")) " by BIC" else " by the significance rule",
    sieve_fit_label(m, "cfi"), num(m[["cfi"]]), sieve_fit_label(m, "rmsea"), num(m[["rmsea"]]),
    ci_text, num(m[["srmr"]])
  )
}

#' The explanation at the top of the Network page
#'
#' Classroom prose on what a partial correlation network is, why a network
#' and a factor model can imply the same covariances, why the residual
#' network is the natural complement to a CFA, and what the figures do and
#' do not show.
#'
#' @return A [shiny::tagList()].
#' @author Ken Kelley
#' @noRd
sieve_network_about <- function() {
  tagList(
    tags$p(
      class = "mb-2",
      strong("What a partial correlation network is. "),
      paste(
        "Take the covariance matrix of the items, invert it, and rescale the inverse so that its diagonal is one:",
        "the off-diagonal entries, with their signs reversed, are the partial correlations, the correlation between",
        "each pair of items after every other item in the set is held constant (Lauritzen, 1996; Epskamp & Fried, 2018).",
        "Drawing the items as nodes and the nonzero partial correlations as edges gives a Gaussian graphical model.",
        "A zero means conditional independence: once the other items are known, the pair carries no further",
        "information about each other. A sparse network comes from a search that sets small edges to zero;",
        "the one used here prunes at a significance level and then steps edges back in while each is significant",
        "and, with a likelihood estimator, BIC improves. DWLS uses the significance rule alone (Epskamp, 2020).",
        "For an MLR result the search uses normal-theory ML tests, followed by one MLR fit of the selected model.",
        "The Wald intervals condition on that selection; they do not include uncertainty from the search.",
        "The edges are estimates with standard errors, and the table gives",
        "them; a figure shows the point estimates only."
      )
    ),
    tags$p(
      class = "mb-2",
      strong("Relations to factor models. "),
      paste(
        "A one-factor model with p indicators implies a covariance matrix whose inverse is dense: every pair",
        "of items has a nonzero partial correlation, because conditioning on the other items does not remove",
        "the factor. So a fully connected network and a one-factor model can imply exactly the same covariances,",
        "and no fit index tells them apart on one sample (Epskamp, Rhemtulla & Borsboom, 2017; van Bork et al., 2021;",
        "for binary items the same equivalence relates the Ising network to an item response model, Marsman et al., 2018).",
        "For equivalent models, fit alone cannot select an interpretation. A network is another parameterization",
        "of the covariance structure that shows which conditional dependencies are",
        "large, which items hold the network together, and whether the dense regions match the constructs."
      )
    ),
    tags$p(
      class = "mb-2",
      strong("Why the residual network is the natural complement to the CFA. "),
      paste(
        "A CFA says the items are conditionally independent given their factors. The residual network model",
        "keeps the factors and estimates a partial correlation network among the residuals instead of assuming",
        "it empty (Epskamp, Rhemtulla & Borsboom, 2017). Each residual edge is local dependence: two items that",
        "still move together after their constructs are accounted for, through shared wording, a shared method,",
        "a facet the ledger does not name, or an item that belongs elsewhere. The CFA page's modification",
        "indices point at the same thing one pair at a time; the residual network estimates the pairs jointly",
        "and reports them with their standard errors, so a residual edge is read as a partial correlation among",
        "residuals given all the others, not as a correlated error freed in isolation."
      )
    ),
    tags$p(
      class = "mb-2",
      strong("The latent network. "),
      paste(
        "The same measurement model with a partial correlation network among the constructs in place of their",
        "correlation matrix. Saturated, it is the CFA exactly, and its implied correlations are Phi. Where the",
        "search removes an edge, two constructs are conditionally independent given the others: the correlation",
        "the CFA reports between them is carried by their common relations to the rest."
      )
    ),
    tags$p(
      class = "mb-0",
      strong("What the figures do and do not show. "),
      paste(
        "Node placement is a layout, not a measurement: distance on the page carries no quantity, and two layouts",
        "of the same matrix can look different. Edge width is the point estimate; the standard errors and",
        "intervals are in the table under the figure. A thick edge can still have a wide interval.",
        "An absent edge is one the search set to zero at the chosen",
        "alpha, which is a decision, not a demonstrated independence. Strength and expected influence summarize",
        "the estimated edges at a node; they are not loadings, and a central node is not thereby a good",
        "indicator. Read every network beside the EFA, the CFA and the Items diagnostics of the same pool."
      )
    )
  )
}

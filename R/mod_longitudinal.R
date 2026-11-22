#' Longitudinal invariance UI with a visible identity map
#'
#' Results first: the fitted rung's status,
#' fit, latent contrasts and comparison sit at the top of the card when
#' there is one; the declaration that produces it (the map, the rung, the
#' residual structure and its justification, the check) follows, with the
#' map as read and the model syntax under it.
#'
#' @param id Module ID.
#' @return UI.
#' @author Ken Kelley
#' @noRd
mod_longitudinal_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Longitudinal Measurement Invariance"),
    shiny::uiOutput(ns("status")),
    DT::DTOutput(ns("fit_table")),
    DT::DTOutput(ns("means")),
    DT::DTOutput(ns("comparison")),
    shiny::p(class = "small mt-2", "Use wide data with one independent participant per row. Map the same construct and item identity across ordered waves. The mapping is a researcher declaration: a numerically valid map cannot establish that the wording/content really matches."),
    shiny::textAreaInput(ns("mapping"),
      "CSV mapping: construct,wave,item,variable,wave_order",
      value = "construct,wave,item,variable,wave_order",
      rows = 7,
      width = "100%"
    ),
    shiny::selectInput(
      ns("level"),
      "Rung to fit explicitly",
      c(
        "Configural" = "configural",
        "Thresholds (ordered only)" = "thresholds",
        "Metric" = "metric",
        "Scalar" = "scalar",
        "Strict" = "strict"
      )
    ),
    shiny::selectInput(
      ns("residuals"),
      "Repeated-item residual autocovariances",
      c(
        "Adjacent waves" = "adjacent",
        "None" = "none",
        "All waves" = "all"
      )
    ),
    shiny::textInput(ns("why"), "Why is this residual structure justified?", ""),
    shiny::checkboxInput(
      ns("confirm"),
      "I checked the item identities and wave order; each row is one independent participant",
      FALSE
    ),
    shiny::selectInput(ns("reference"), "Compare with an earlier fitted rung (optional)", choices = c("None" = "")),
    shiny::actionButton(ns("run"), "Fit This Longitudinal Rung"),
    shiny::uiOutput(ns("declared_order")), DT::DTOutput(ns("mapping_preview")),
    shiny::verbatimTextOutput(ns("syntax"))
  )
}

#' Longitudinal invariance module
#'
#' @param id Module ID.
#' @param state Shared state.
#' @return A list: `result` (the reactive value holding the last fitted
#'   record), `run()` (what the Fit button does, for the page's Update),
#'   and `entry()` (the record as the page's Update reads it; see
#'   `sieve_update_entry()`).
#' @author Ken Kelley
#' @noRd
mod_longitudinal_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal(NULL)
    output$declared_order <- shiny::renderUI({
      map <- tryCatch(utils::read.csv(text = input$mapping %||% "", stringsAsFactors = FALSE), error = function(e) NULL)
      if (is.null(map) || !all(c("wave", "wave_order") %in% names(map)) || !nrow(map)) {
        return(shiny::p(
          class = "small",
          "Declare wave_order for each wave. The earliest declared wave is the reference; CSV row order is ignored."
        ))
      }
      pairs <- unique(map[c("wave", "wave_order")])
      pairs$wave_order <- suppressWarnings(as.numeric(pairs$wave_order))
      if (any(!is.finite(pairs$wave_order)) || anyDuplicated(pairs$wave) || anyDuplicated(pairs$wave_order)) {
        return(shiny::p(class = "text-warning", "Each wave needs one distinct numeric wave_order value."))
      }
      waves <- as.character(pairs$wave[order(pairs$wave_order)])
      shiny::p(class = "small", sprintf(
        "Reference wave: %s. Adjacent pairs: %s.", waves[1],
        paste(paste(head(waves, -1L), tail(waves, -1L), sep = " - "), collapse = ", ")
      ))
    })
    history <- shiny::reactiveVal(list())
    choices <- shiny::reactive(list(
      mapping = input$mapping,
      level = input$level %||% "configural",
      residuals = input$residuals %||% "adjacent",
      why = input$why,
      estimator = sieve_estimator_effective(
        NULL,
        state
      ),
      sampling = sieve_sampling_spec(state)
    ))
    # Fit This Longitudinal Rung, as a function: its button and the page's
    # Update run it.
    run <- function() {
      started <- proc.time()[["elapsed"]]
      if (!isTRUE(shiny::isolate(sieve_confirmation_status(state)$confirmed))) {
        result(list(error = "Confirm the item pool and measurement types on Ingest before fitting longitudinal invariance."))
        return()
      }
      if (!isTRUE(input$confirm)) {
        result(list(error = "Check the map, wave order, and independent-participant declaration before fitting."))
        return()
      }
      design <- sieve_sampling_spec(state)
      if (identical(
        design$unit,
        "clustered"
      )) {
        result(list(error = "This wide longitudinal workflow does not additionally supply cluster-robust multilevel inference."))
        return()
      }
      sieve_ensure_dataset(state)
      opts <- choices()
      r <- tryCatch(
        {
          map <- utils::read.csv(text = input$mapping, stringsAsFactors = FALSE, check.names = FALSE)
          m <- shiny::isolate(state$meta)
          if (!all(map$variable %in% m$item[m$status == "active"])) stop("Map only included item variables; include an excluded item again before using it in this model.")
          ordered <- intersect(map$variable, m$item[m$type %in% c("ordered", "binary")])
          sieve_run_longitudinal(shiny::isolate(state$items), map,
            level = opts$level,
            residuals = opts$residuals, justification = opts$why, ordered = ordered, estimator = opts$estimator
          )
        },
        error = function(e) list(error = conditionMessage(e))
      )
      r$revision <- sieve_revision(state)
      r$choices <- opts
      if (!is.null(r$fit)) {
        prior <- shiny::isolate(history())[[input$reference %||% ""]]
        r$contrasts <- sieve_longitudinal_contrasts(r)
        if (!is.null(prior)) r$comparison <- tryCatch(
          {
            if (!sieve_same_revision(prior$revision, r$revision)) stop("The earlier rung used a different data revision.")
            sieve_longitudinal_compare(prior, r)
          },
          error = function(e) list(error = conditionMessage(e))
        )
        r$estimates <- list(
          fit = sieve_fit_measures(r$fit),
          parameters = lavaan::parameterEstimates(r$fit,
            standardized = TRUE
          ),
          contrasts = r$contrasts
        )
        r$analysis_id <- sieve_register_analysis(state, r, note = "Explicit longitudinal invariance rung")
        result_name <- paste0("longitudinal_result_", r$analysis_id)
        code <- c(
          sieve_cfa_code(r$spec, fit_name = paste0("longitudinal_", r$analysis_id)),
          sprintf(
            "%s <- list(fit = longitudinal_%s, spec = %s)",
            result_name,
            r$analysis_id,
            paste(utils::capture.output(dput(r$spec)),
              collapse = "\n"
            )
          ),
          sprintf("SIEVER::sieve_longitudinal_contrasts(%s)", result_name)
        )
        sieve_log(state, "Fit longitudinal invariance from the checked item/wave map", code)
        if (!is.null(prior) && is.null(r$comparison$error)) {
          sieve_register_inference(
            state, "longitudinal_comparison", r,
            list(reference_id = prior$analysis_id, reference_spec = prior$spec), r$comparison,
            sprintf("SIEVER::sieve_longitudinal_compare(longitudinal_result_%s, %s)", prior$analysis_id, result_name)
          )
        }
        h <- shiny::isolate(history())
        h[[r$analysis_id]] <- r
        history(h)
        shiny::updateSelectInput(session,
          "reference",
          choices = c(
            "None" = "",
            stats::setNames(
              names(h),
              vapply(
                h,
                function(x) paste(
                  x$analysis_id,
                  x$spec$longitudinal$level
                ),
                character(1)
              )
            )
          )
        )
      }
      r$seconds <- proc.time()[["elapsed"]] - started
      r$when <- format(Sys.time(), "%H:%M")
      result(r)
    }
    shiny::observeEvent(input$run, run())
    # Why the fitted rung no longer describes the inputs, or NULL.
    historical <- shiny::reactive({
      r <- result()
      shiny::req(r)
      # sieve_revision() reads in isolation, so the dependency on the
      # dataset, the item data (a respondent removed or restored, an item
      # reverse scored) and the ledger is taken here; without it the rung
      # would stay on screen as current after such a change.
      state$dataset_id
      state$items
      state$meta
      if (is.null(r$revision)) {
        return(NULL)
      }
      why <- sieve_stale_reason(r$revision, sieve_revision(state))
      if (!is.null(why)) {
        return(why)
      }
      if (!identical(r$choices, choices())) {
        return("the map, the rung, the residual structure, its justification, or the estimator changed")
      }
      NULL
    })
    current <- shiny::reactive({
      r <- result()
      shiny::req(r)
      if (!is.null(historical())) return(list(error = "The longitudinal result is historical; review the new inputs and fit the rung again."))
      r
    })
    output$status <- shiny::renderUI({
      r <- current()
      if (!is.null(r$error)) shiny::div(
        class = "alert alert-warning",
        r$error
      ) else sieve_status_alert(r)
    })
    output$mapping_preview <- DT::renderDT({
      r <- current()
      sieve_inference_table(r$spec$longitudinal$mapping)
    })
    output$syntax <- shiny::renderPrint({
      r <- current()
      cat(r$spec$model %||% "")
    })
    output$fit_table <- DT::renderDT({
      r <- current()
      shiny::req(r$fit)
      sieve_inference_table(sieve_fit_table(sieve_fit_measures(r$fit)))
    })
    output$means <- DT::renderDT({
      r <- current()
      sieve_inference_table(r$contrasts)
    })
    output$comparison <- DT::renderDT({
      r <- current()
      sieve_inference_table(r$comparison)
    })
    # The fitted rung as the page's Update reads it: a rung that was fit
    # and is now historical is computed again, from the map and choices
    # now on the page, when the declaration is still checked.
    entry <- function() {
      r <- result()
      if (is.null(r) || is.null(r$fit)) {
        return(NULL)
      }
      why <- historical()
      if (is.null(why)) {
        return(sieve_update_entry("Longitudinal rung", "current"))
      }
      sieve_update_entry(
        "Longitudinal rung", "stale",
        sentence = why, when = sprintf("Longitudinal %s rung, at %s", r$spec$longitudinal$level %||% "", r$when %||% "an earlier time"),
        seconds = r$seconds %||% NA_real_, run = run, shown = FALSE,
        update = isTRUE(input$confirm),
        button = "Fit This Longitudinal Rung", why = "after the map and the declaration are checked again"
      )
    }
    # The rung's status for the page guide, with the page's one Update when
    # it is from before and its declaration is still checked.
    shiny::observe({
      r <- result()
      if (is.null(r) || is.null(r$fit)) return()
      why <- historical()
      sieve_page_publish(state, "finalize", session$ns("rung"), "Longitudinal rung",
        list(state = if (is.null(why)) "current" else "stale", sentence = why, key = r$choices, when = r$when),
        if (!is.null(why) && isTRUE(input$confirm)) "Update" else "Fit This Longitudinal Rung", own = TRUE
      )
    })
    list(result = result, run = run, entry = entry)
  })
}

#' Wire the Norms Panel in the Finalize Module
#' @param envir The module server frame receiving its panel bindings.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_finalize_norms_wire <- function(envir) {
  evalq({
    # ---- Scoring syntax and norms ------------------------------------------
    # Mean scores per construct on the active items (reverse flips are already
    # applied to state$items by the reverse-coding action, so the scores here
    # match what the app analyzed). Percentiles and the T conversion are
    # computed on this sample and labeled as sample bound.
    # The table and, beside it, every construct that was not scored because
    # an item of it cannot be read as numbers (named with the reason), rather
    # than scored on the entries that happen to be numbers.
    norms_read <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      sieve_construct_norms(state$items, state$meta, state$scoring_policies)
    })

    norms <- reactive({
      norms_read()$table
    })

    output$scoring_eligibility <- renderUI({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      read <- sieve_scoring_eligibility(state$items, state$meta, state$scoring_policies)
      tags$details(open = NA, class = "sieve-scoring-eligibility small",
        tags$summary("Response Rules and Score Eligibility"),
        p("Required is the minimum number of answered items under the stored rule. One Response counts eligible scores based on one response; Partial Responses counts eligible scores with some items unanswered. Complete counts eligible scores with every item answered."),
        sieve_plain_table_ui(stats::setNames(read$rules, c("Construct", "Method", "Items", "Required", "Eligible", "One Response", "Partial Responses", "Complete Responses")[seq_along(read$rules)])),
        p("Available-response means can use different item content for different respondents. Reliability for the complete fixed composite does not describe every partial-response score. FIML estimation and this operational scoring rule are separate procedures. Follow the instrument's published scoring rules when available."),
        lapply(read$refused, function(x) p(x)),
        tags$details(
          tags$summary("Compare Alternative Minimum Answered Counts"),
          p("This descriptive comparison keeps the items, weights and scoring method fixed. It changes no stored rule and estimates no reliability or confidence interval. Current identifies the stored required count; there is no universal recommended response percentage."),
          sieve_plain_table_ui(read$sensitivity)))
    })

    output$norms_note <- renderUI({
      read <- tryCatch(norms_read(), error = function(e) NULL)
      df <- read$table
      said <- if (length(read$refused)) {
        div(class = "alert alert-danger", paste(read$refused, collapse = " "))
      }
      if (is.null(df)) {
        return(tagList(said, div(
          class = "alert alert-info",
          "The sample reference table needs at least one construct with two or more active items and ten scored respondents."
        )))
      }
      tagList(said, div(
        class = "text-muted small",
        sprintf(
          "Scores follow each construct\u2019s stored minimum-response and weighting policy; n is its eligible respondent count. T = 50 + 10 (score - mean) / SD uses this sample of %d rows. Percentile intervals assume independent observations; dependent-design intervals are withheld.",
          nrow(state$items)
        )
      ))
    })

    output$norms_tbl <- renderDT({
      df <- tryCatch(norms(), error = function(e) NULL)
      req(df)
      sieve_log(
        state,
        "Scoring and the sample reference table: mean construct scores with sample-bound percentiles and the T conversion (Bandalos, 2018); a description of this sample, not population norms.",
        c(
          "# Complete construct results use the same scored values and missing-response policy as the app.",
          paste0(
            "scoring_policies <- ",
            paste(utils::capture.output(dput(shiny::isolate(state$scoring_policies))),
              collapse = "\n"
            )
          ),
          "norms_result <- SIEVER::sieve_construct_norms(items, meta, scoring_policies)",
          "norms_result$table",
          "norms_result$T  # T scores are sample bound, with original row positions"
        )
      )
      sieve_dt(df,
        rownames = FALSE, selection = "none",
        options = list(dom = "t", scrollX = TRUE),
        colnames = c(
          "Construct", "k items", "n", "Mean", "SD",
          "P5", "P10", "P25", "P50", "P75", "P90", "P95"
        )
      ) |>
        formatRound(setdiff(names(df), c("construct", "k", "n")), 3)
    })

    output$norms_uncertainty <- renderDT({
      req(identical(sieve_sampling_spec(state)$unit, "independent"))
      d <- norms_read()$uncertainty
      req(d)
      sieve_dt(d, rownames = FALSE, selection = "none", options = list(dom = "tp", scrollX = TRUE)) |>
        formatRound(c("estimate", "lower", "upper"), 3)
    })
    norm_feedback <- reactiveVal(NULL)
    norm_limits <- reactiveVal(NULL)
    observe({
      refs <- state$norm_references %||% list()
      updateSelectInput(session, "norm_reference",
        choices = stats::setNames(names(refs), names(refs)),
        selected = isolate(input$norm_reference) %||% tail(names(refs), 1)
      )
    })
    observeEvent(input$norm_freeze, {
      r <- tryCatch(sieve_norm_freeze(state$items, state$meta, state$scoring_policies,
        name = input$norm_name %||% "", population = input$norm_population %||% "",
        rows = sieve_case_index(state), design = sieve_sampling_spec(state),
        calibration_scientific_dataset_id = state$scientific_dataset_id,
        cluster = if (identical(sieve_sampling_spec(state)$unit, "clustered")) sieve_bootstrap_cluster(state)
      ), error = function(e) e)
      if (inherits(r, "error")) {
        norm_feedback(conditionMessage(r))
        return()
      }
      refs <- state$norm_references %||% list()
      if (r$name %in% names(refs)) {
        norm_feedback("That reference already exists. Use a new name to preserve the original calibration.")
        return()
      }
      refs[[r$name]] <- r
      state$norm_references <- refs
      updateSelectInput(session, "norm_reference", choices = names(refs), selected = r$name)
      norm_feedback(paste("Frozen reference:", r$name))
      sieve_log(state, "Freeze norm reference", c(
        paste0(
          "norm_reference <- SIEVER::sieve_norm_freeze(items, meta, policies = ",
          paste(utils::capture.output(dput(state$scoring_policies)), collapse = "\n"), ", name = ",
          sieve_r_literal(r$name), ", population = ", sieve_r_literal(r$population),
          ", rows = ", paste(utils::capture.output(dput(r$calibration_rows)), collapse = "\n"),
          ", design = ", paste(utils::capture.output(dput(r$design)), collapse = "\n"),
          ", calibration_scientific_dataset_id = ", sieve_r_literal(r$calibration_scientific_dataset_id),
          ", cluster = ", sieve_r_literal(if (!is.null(r$bootstrap_scores)) sieve_bootstrap_cluster(state)), ")"
        )
      ))
    })
    observeEvent(input$norm_import, {
      # sieve_norm_read() checks that the file holds only plain data before
      # it reads any field, then the reference's structure, and says why
      # when it refuses (the place and the type or class, or the field).
      r <- tryCatch(sieve_norm_read(input$norm_import$datapath), error = function(e) e)
      if (inherits(r, "error")) {
        norm_feedback(conditionMessage(r))
        return()
      }
      refs <- state$norm_references %||% list()
      if (r$name %in% names(refs)) {
        norm_feedback("That reference name already exists; it was not replaced.")
        return()
      }
      # Validate its scoring contract against current data before retaining it.
      check <- tryCatch(sieve_norm_apply(r, state$items, state$meta, state$scoring_policies), error = function(e) e)
      if (inherits(check, "error")) {
        norm_feedback(conditionMessage(check))
        return()
      }
      refs[[r$name]] <- r
      state$norm_references <- refs
      updateSelectInput(session, "norm_reference", choices = names(refs), selected = r$name)
      norm_feedback(paste("Imported reference:", r$name))
    })
    norm_selected <- reactive({
      refs <- state$norm_references %||% list()
      refs[[input$norm_reference %||% ""]]
    })
    norm_application_key <- reactiveVal(NULL)
    norm_applied <- reactive({
      ref <- norm_selected()
      req(ref, state$items, state$meta)
      out <- sieve_norm_apply(ref, state$items, state$meta, state$scoring_policies)
      key <- sieve_hash(list(reference = ref, revision = sieve_revision(state), policies = state$scoring_policies))
      if (!identical(key, isolate(norm_application_key()))) {
        norm_application_key(key)
        sieve_log(state, "Apply frozen norm reference to the current cohort", c(
          paste0("norm_reference <- ", paste(utils::capture.output(dput(ref)), collapse = "\n")),
          paste0("norm_policies <- ", paste(utils::capture.output(dput(state$scoring_policies)), collapse = "\n")),
          "norm_applied <- SIEVER::sieve_norm_apply(norm_reference, items, meta, norm_policies)",
          paste0(
            "norm_original_rows <- ",
            paste(utils::capture.output(dput(sieve_case_index(state))),
              collapse = "\n"
            )
          ),
          "norm_applied$original_row <- norm_original_rows[norm_applied$row]"
        ))
      }
      out
    })
    output$norm_reference_note <- renderUI({
      ref <- norm_selected()
      problem <- tryCatch(
        {
          if (!is.null(ref)) norm_applied()
          NULL
        },
        error = function(e) conditionMessage(e)
      )
      tagList(
        if (!is.null(norm_feedback())) div(class = "alert alert-info", norm_feedback()),
        if (!is.null(ref)) p(class = "small", paste(ref$name, ref$population, ref$created, ref$limitation, sep = ". ")),
        # What Download frozen reference (.rds) writes, said before it is
        # downloaded: respondent-level data, and for a clustered reference
        # each score linked to its row and cluster.
        if (!is.null(ref)) p(class = "small text-muted", sieve_norm_reference_contents_text(ref)),
        if (!is.null(problem)) div(class = "alert alert-warning", problem)
      )
    })
    output$norm_reference_scores <- renderDT({
      d <- norm_applied()
      d$original_row <- sieve_case_index(state)[d$row]
      sieve_dt(d, rownames = FALSE, selection = "none", options = list(dom = "tp", scrollX = TRUE)) |>
        formatRound(c("score", "T", "percentile"), 3)
    })
    # The request in force: the resamples (an entry below the function's
    # minimum, 99 and at least 2 / (1 - level) - 1, is moved up to it; a
    # request is never lowered to shorten a wait), the seed, and the level.
    norm_request <- reactive({
      asked <- as.integer(sieve_finite_or(input$norm_B, 999L))
      level <- sieve_finite_or(input$norm_level, 0.95)
      minimum <- if (level > 0 && level < 1) sieve_norm_min_resamples(level) else 99L
      list(
        B = max(minimum, asked), asked = asked, minimum = minimum,
        seed = sieve_seed_input(input$norm_seed),
        level = level
      )
    })
    norm_level_text <- function(level) sprintf("%s%%", sub("\\.?0+$", "", sprintf("%.4f", 100 * level)))
    # A completed request is kept under the identity of everything it rests
    # on (the frozen reference, B, the seed, and the level).
    norm_request_key <- function(ref, request) {
      sieve_hash(list(reference = sieve_hash(ref), B = request$B, seed = request$seed, level = request$level))
    }
    # The measured rate: the time of one resample of the reference's
    # calibration scores, measured once per reference.
    norm_rate <- reactiveVal(list())
    norm_transform_rate <- function(ref) {
      key <- sieve_hash(ref)
      held <- isolate(norm_rate())
      if (!is.null(held[[key]])) {
        return(held[[key]])
      }
      measured <- sieve_norm_transform_wait(ref, B = 1L)$per_resample
      held[[key]] <- measured
      norm_rate(held)
      measured
    }
    output$norm_transform_plan <- renderUI({
      ref <- norm_selected()
      req(ref)
      request <- norm_request()
      unit <- ref$design$unit %||% "independent"
      refused <- !unit %in% c("independent", "clustered") || (identical(unit, "clustered") && is.null(ref$bootstrap_scores))
      unit_text <- if (identical(unit, "clustered") && !refused) {
        groups <- vapply(ref$bootstrap_scores, function(x) length(unique(x$cluster)), integer(1))
        sprintf("whole independent clusters of the frozen calibration sample (G = %s across constructs); resampled N can vary, and intervals can be unstable with few clusters", paste(unique(groups), collapse = ", "))
      } else "independent rows of the frozen calibration sample"
      per <- norm_transform_rate(ref)
      div(
        class = "small text-muted mt-2",
        sprintf(
          paste0(
            "Method: Percentile bootstrap of the calibration constants (the mean and SD of each ",
            "construct's calibration scores, and the T score at its 5th, 50th, and 95th percentiles) at the ",
            "%s level, the quantile method of sieve_norm_transform_limits (type 7 quantiles of the resampled ",
            "constants); resampling unit: %s%s. "
          ),
          norm_level_text(request$level), unit_text,
          if (refused) " (refused: this reference declares a dependent design, which needs a design-matched method)" else ""
        ),
        sprintf(
          paste0(
            "With B = %d the bootstrap is estimated to take %s (the measured time of one resample over the ",
            "%d construct%s, %s, times the resamples); it runs as one call. "
          ),
          request$B, sieve_duration_label(per * request$B), length(ref$scores),
          if (length(ref$scores) == 1L) "" else "s", sieve_seconds_text(per)
        ),
        if (request$asked < request$minimum) {
          sprintf(
            "%d was entered; the bootstrap needs at least %d resamples at this level, so %d will run. ",
            request$asked, request$minimum, request$minimum
          )
        },
        paste0(
          "The number of resamples sets the Monte Carlo precision of the limits, not what they estimate ",
          "(Hesterberg, 2015, suggests 10,000 for routine use); a request is never lowered to shorten a ",
          "wait, and a request that matches a completed one (the same reference, resamples, seed, and ",
          "level) is reused rather than run again. An empty seed is drawn for the run and recorded."
        )
      )
    })
    observeEvent(input$norm_transform_ci, {
      ref <- norm_selected()
      req(ref)
      request <- norm_request()
      if (!(request$level > 0 && request$level < 1)) {
        norm_feedback("The confidence level must lie strictly between 0 and 1.")
        return()
      }
      key <- norm_request_key(ref, request)
      records <- state$norm_transform_records %||% list()
      held <- records[[key]]
      if (!is.null(held) && !is.null(held$table)) {
        held$reused <- TRUE
        norm_limits(held)
        norm_feedback(sprintf(
          "The calibration transformation bootstrap for this request (%s; B = %d, seed = %d, %s level) was completed at %s; the result shown is that run.",
          ref$name, held$B, held$seed, norm_level_text(held$level), held$when
        ))
        return()
      }
      per <- norm_transform_rate(ref)
      estimate <- per * request$B
      started <- proc.time()[["elapsed"]]
      r <- withProgress(
        message = sprintf(
          "Calibration transformation bootstrap: B = %d, estimated %s", request$B,
          sieve_duration_label(estimate)
        ),
        detail = "One call to sieve_norm_transform_limits, which reports nothing until it returns.", value = 1,
        tryCatch(
          sieve_norm_transform_limits(ref, B = request$B, seed = request$seed, conf_level = request$level),
          error = function(e) e
        )
      )
      seconds <- proc.time()[["elapsed"]] - started
      if (inherits(r, "error")) {
        norm_feedback(conditionMessage(r))
        return()
      }
      record <- list(
        reference = ref$name, created = ref$created, reference_key = sieve_hash(ref), table = r,
        calibration_scientific_dataset_id = ref$calibration_scientific_dataset_id,
        B = request$B, asked = request$asked, minimum = request$minimum, seed = r$seed[1],
        seed_supplied = isTRUE(r$seed_supplied[1]),
        level = request$level, method = r$method[1],
        counts = list(attempted = request$B, successful = min(r$successful), failed = max(r$failed)),
        seconds = seconds, when = format(Sys.time(), "%H:%M"), key = key, reused = FALSE
      )
      records[[key]] <- sieve_audit_stamp(record, state)
      state$norm_transform_records <- records
      norm_limits(record)
      # The request and its counts, recorded in the analysis registry as the
      # Reliability tab records its bootstraps.
      spec <- list(
        kind = "norm_transform_limits", reference = ref$name, created = ref$created,
        reference_key = record$reference_key, B = request$B, seed = record$seed, level = request$level
      )
      spec$key <- sieve_hash(spec)
      sieve_register_analysis(
        state,
        list(
          spec = spec,
          estimates = list(
            table = r, method = record$method, counts = record$counts, conf_level = request$level,
            seed = record$seed, seed_supplied = record$seed_supplied, seconds = seconds
          ),
          status = list(code = "ok", message = sprintf(
            "%d resamples attempted; %d to %d successful per target.", request$B, min(r$successful), max(r$successful)
          ))
        ),
        note = "Calibration transformation uncertainty"
      )
      sieve_log(state, sprintf(
        "Calibration transformation uncertainty (%s; B = %d attempted, %d to %d successful per target, seed = %d%s, %s level; %s)",
        record$method, request$B, min(r$successful), max(r$successful), record$seed,
        if (record$seed_supplied) "" else " drawn for this run", norm_level_text(request$level),
        sieve_seconds_text(seconds)
      ), c(
        paste0("norm_reference <- ", paste(utils::capture.output(dput(ref)), collapse = "\n")),
        sprintf(
          "norm_transform_limits <- SIEVER::sieve_norm_transform_limits(norm_reference, B = %dL, seed = %d, conf_level = %s)",
          request$B, record$seed, sieve_num_lit(request$level)
        )
      ))
    })
    # The record shown for the selected reference: the last run, or the
    # kept record of the request now in force, or the latest kept for it.
    norm_record_shown <- reactive({
      ref <- norm_selected()
      req(ref)
      r <- norm_limits()
      same <- function(x) {
        !is.null(x) && identical(x$reference, ref$name) && identical(x$created, ref$created)
      }
      if (!same(r)) {
        records <- state$norm_transform_records %||% list()
        r <- records[[norm_request_key(ref, norm_request())]]
        if (!same(r)) {
          kept <- Filter(same, records)
          r <- if (length(kept)) kept[[length(kept)]] else NULL
        }
      }
      req(same(r))
      r
    })
    output$norm_transform_note <- renderUI({
      r <- norm_record_shown()
      counts <- r$counts
      div(
        class = "small text-muted mt-2",
        if (is.null(r$B)) {
          "This record does not state its resample count; it was made with 999 resamples, and its method, seed and level are in its table."
        } else {
          sprintf(
            paste0(
              "Shown: %s; B = %d resamples attempted%s, %d to %d successful per target (a resample ",
              "whose SD is zero gives no T score); seed = %d%s; %s level; %s."
            ),
            r$method, r$B,
            if (!is.null(r$asked) && !identical(r$asked, r$B)) {
              sprintf(" (%d was entered; the bootstrap needs at least %d, so %d were run)", r$asked, r$B, r$B)
            } else {
              ""
            },
            counts$successful %||% NA_integer_, max(r$table$successful), r$seed,
            if (isTRUE(r$seed_supplied)) "" else " (drawn for this run and recorded)",
            norm_level_text(r$level),
            if (isTRUE(r$reused)) {
              sprintf("completed earlier at %s and reused for this matching request", r$when)
            } else {
              sprintf("it took %s", sieve_seconds_text(r$seconds %||% NA_real_))
            }
          )
        }
      )
    })
    output$norm_transform_limits <- renderDT({
      r <- norm_record_shown()
      sieve_dt(r$table, rownames = FALSE, options = list(dom = "tp", scrollX = TRUE)) |>
        formatRound(intersect(c("fixed_score", "estimate", "lower", "upper"), names(r$table)), 3)
    })
    output$dl_reference <- downloadHandler(
      filename = function() "siever_frozen_norm_reference.rds",
      content = function(file) {
        r <- norm_selected()
        req(r)
        saveRDS(r, file)
      }
    )
    output$dl_reference_scores <- downloadHandler(
      filename = function() "siever_fixed_reference_scores.csv",
      content = function(file) {
        d <- norm_applied()
        d$original_row <- sieve_case_index(state)[d$row]
        utils::write.csv(d,
          file,
          row.names = FALSE
        )
      }
    )

    scoring_syntax <- reactive({
      req(state$meta)
      sieve_scoring_code(state$meta,
        values = "raw", policies = state$scoring_policies,
        raw = state$raw, processed = state$items
      )$code
    })

    output$dl_scoring <- downloadHandler(
      filename = function() "siever_scoring_syntax.R",
      content = function(file) writeLines(scoring_syntax(), file)
    )

    output$dl_norms <- downloadHandler(
      filename = function() "siever_sample_reference_table.csv",
      content = function(file) {
        df <- norms()
        req(df)
        # A sample reference table, not norms: the file says so on every row,
        # with the rows, the data set, the time, and the version behind it.
        df <- sieve_sample_reference_table(df,
          rows = nrow(state$items), dataset = state$dataset_name %||% NA_character_
        )
        utils::write.csv(df, file, row.names = FALSE)
      }
    )

    output$inv_partial_out <- renderUI({
      p_res <- tryCatch(inv_partial(), error = function(e) NULL)
      if (is.null(p_res)) {
        return(NULL)
      }
      if (!is.null(p_res$error)) {
        return(div(class = "alert alert-info", p_res$error))
      }
      # The values, the family they are in, and the engine's difference test
      # are reported. No verdict is drawn from them: the reference values are
      # conventions, stated below with the conditions they come from.
      num <- function(x) if (is.finite(x)) sprintf("%.3f", x) else "n/a"
      tag <- if (is.na(p_res$family)) "" else sprintf(" (%s)", p_res$family)
      tested <- p_res$compare$tests[[2L]]
      test_txt <- if (isTRUE(tested$available) && is.finite(p_res$lrt_p)) {
        sprintf(
          "%s: chi square difference %.3f on %d df, p %s", p_res$test,
          p_res$lrt_chisq, as.integer(p_res$lrt_df),
          if (p_res$lrt_p < .001) "< .001" else sprintf("= %.3f", p_res$lrt_p)
        )
      } else {
        why <- tested$reason
        sprintf(
          "chi square difference test not reported (%s)",
          if (is.null(why) || is.na(why)) "no reason recorded" else sub("[.]$", "", why)
        )
      }
      # Which model the partial model is read against, in words: the rung
      # before it with the same parameters free, which is the ladder's own
      # rung whenever none of them is constrained there.
      reference <- if (isTRUE(p_res$reference_partial)) {
        sprintf(paste0(
          "the %s model with the same parameters free (the partial model is ",
          "nested in that one, and not in the full %s rung, which still holds ",
          "them equal)"
        ), p_res$prev, p_res$prev)
      } else {
        sprintf(paste0(
          "the %s model (none of the freed parameters is constrained at that ",
          "rung, so it is the ladder's own)"
        ), p_res$prev)
      }
      shown <- p_res$freed_label %||% p_res$freed
      div(
        class = "alert alert-secondary",
        sieve_status_alert(p_res$status,
          warnings = character(0),
          what = sprintf("partial %s model", p_res$level)
        ),
        if (!is.null(p_res$prev_status) && !sieve_has_solution(p_res$prev_status)) {
          sieve_status_alert(p_res$prev_status,
            warnings = character(0),
            what = sprintf("%s model it is compared with", p_res$prev)
          )
        },
        HTML(sprintf(
          paste0(
            "<b>Partial %s invariance, freeing %s.</b> Refit through ",
            "DMAR::measurement_invariance() with those parameters named in ",
            "group_partial, then compared with %s: ",
            "CFI%s %s (change %s), RMSEA%s %s (change %s); %s."
          ),
          p_res$level, htmltools::htmlEscape(paste(shown, collapse = "; ")), reference,
          tag, num(p_res$cfi), num(p_res$dcfi), tag, num(p_res$rmsea), num(p_res$drmsea),
          htmltools::htmlEscape(test_txt)
        )),
        if (length(p_res$warnings)) {
          tags$details(
            class = "small mt-2",
            tags$summary(sprintf(
              "The engine raised %d warning%s while refitting and comparing the two models",
              length(p_res$warnings), if (length(p_res$warnings) == 1L) "" else "s"
            )),
            tags$ul(lapply(gsub("\\s+", " ", p_res$warnings), tags$li))
          )
        },
        div(class = "small mt-2", p_res$compare$common$reason),
        div(class = "small mt-1", sieve_invariance_convention(p_res$family)),
        div(
          class = "small mt-1",
          "Report the freed parameters alongside any group comparison, and ",
          "read the dMACS panel below to see how large the freed differences are."
        )
      )
    })

  }, envir = envir)
  invisible(NULL)
}

#' Wire the Cv Panel in the Finalize Module
#' @param envir The module server frame receiving its panel bindings.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_finalize_cv_wire <- function(envir) {
  evalq({
    # ---- Calibration / validation split -----------------------------------
    cv_result <- reactiveVal(NULL)
    keep_cv <- function(x) cv_result(bind(x, "cv"))
    cv_shown <- reactive(current(cv_result(), "split", "Create split", "cv"))

    # Create split, as a function: its button and the page's Update run it.
    run_split <- function() {
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      started <- proc.time()[["elapsed"]]
      p_hold <- (input$holdout %||% 50) / 100
      if (!is.finite(p_hold) || p_hold <= 0 || p_hold >= 1) p_hold <- .5
      design <- sieve_sampling_spec(state)
      # The split reliability follows the Reliability tab's omega variant
      # (robust omega unless that tab is set to model implied).
      denominator <- (state$reliability_settings %||% list())$denominator %||% "observed"
      if (!denominator %in% c("observed", "model_implied")) denominator <- "observed"
      result <- sieve_cross_validate(state$items, state$meta,
        holdout = p_hold,
        seed = sieve_seed_input(input$seed), estimator = estimator_choice(), design = design,
        denominator = denominator
      )
      keep_cv(result)
      if (!is.null(result$error)) return(invisible(NULL))
      rng <- result$rng
      sieve_log(
        state,
        sprintf(
          "Finalize: calibration/validation split (holdout = %.2f, seed = %d%s)",
          p_hold, rng$seed, if (rng$supplied) "" else " drawn for this run"
        ),
        c(
          sprintf(
            "do.call(RNGkind, as.list(%s))",
            paste(utils::capture.output(dput(rng$kind)), collapse = "")
          ),
          sprintf(
            paste0(
              "cross_validation <- SIEVER::sieve_cross_validate(items, meta, ",
              "holdout = %s, seed = %d, estimator = %s, design = %s, denominator = %s)"
            ),
            format(p_hold, digits = 17), rng$seed,
            sieve_r_literal(sieve_estimator_record_choice(estimator_choice(), state$meta)),
            paste(utils::capture.output(dput(design)), collapse = ""), sieve_r_literal(denominator)
          )
        )
      )
      time_section("cv", started)
    }
    observeEvent(input$split, run_split())

    # ---- Evaluation on the reserved rows (fct_holdout.R) ---------------------
    holdout_now <- reactive({
      state$holdout
      state$dataset_id
      sieve_holdout(state)
    })
    holdout_result <- reactiveVal(NULL)
    output$holdout_note <- renderUI({
      req(state$raw)
      rec <- holdout_now()
      if (is.null(rec)) {
        return(p(
          class = "small text-muted",
          "No validation reservation is in force (Screening page, Observations panel), so the ",
          "split below draws from the rows every development result has read."
        ))
      }
      p(
        class = "small text-muted",
        sprintf(
          "Reservation %s is in force: %d rows reserved for validation (seed %d, drawn %s). %s The split below draws from the calibration rows only; the evaluation on the reserved rows is on the Holdout / Cross-Validation panel.",
          rec$id, rec$n_reserved, rec$rng$seed, rec$time, sieve_holdout_n_sentence(state)
        )
      )
    })
    observeEvent(input$holdout_go, {
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      denominator <- (state$reliability_settings %||% list())$denominator %||% "observed"
      if (!denominator %in% c("observed", "model_implied")) denominator <- "observed"
      out <- sieve_holdout_evaluate(state, estimator_choice(),
        denominator = denominator, design = sieve_sampling_spec(state)
      )
      holdout_result(out)
      if (is.null(out$error)) {
        sieve_page_publish(state, "finalize", session$ns("result_holdout"),
          "Evaluation on the reserved rows",
          list(state = "current", when = out$time, key = out$version_hash),
          "Evaluate on the reserved rows",
          own = TRUE
        )
      }
    })
    output$holdout_msg <- renderUI({
      req(state$raw)
      rec <- holdout_now()
      out <- holdout_result()
      if (is.null(rec)) {
        return(div(
          class = "text-muted",
          "No validation reservation is in force. Reserve rows on the Screening page (Observations ",
          "panel) before the structure and the items are examined; without a reservation the split ",
          "below is an internal assessment."
        ))
      }
      head <- div(
        class = "text-muted",
        sprintf(
          "Reservation %s: %d rows reserved for validation (seed %d, drawn %s%s), %d calibration rows. %s",
          rec$id, rec$n_reserved, rec$rng$seed, rec$time,
          if (isTRUE(rec$after_development)) ", after development began" else ", before any result existed",
          rec$n_calibration,
          if (length(rec$evaluations)) {
            sprintf("%d evaluation%s recorded.", length(rec$evaluations), if (length(rec$evaluations) == 1L) "" else "s")
          } else {
            "Not evaluated yet: press Evaluate on the reserved rows to freeze the current version and evaluate it once."
          }
        )
      )
      if (is.null(out)) {
        return(head)
      }
      if (!is.null(out$error)) {
        return(tagList(head, div(class = "alert alert-danger", out$error)))
      }
      r <- out$result
      tagList(
        head,
        div(
          class = if (identical(out$label, "specified before evaluation") && !isTRUE(rec$after_development)) {
            "alert alert-success py-2"
          } else {
            "alert alert-warning py-2"
          },
          strong(sprintf("Version %s, %s. ", out$version, out$label)),
          sprintf(
            "Frozen %s (hash %s); evaluated %s on %d reserved rows (%d complete on the included items), fit on %d calibration rows.%s%s",
            out$version_time, substr(out$version_hash, 1L, 12L), out$time, length(out$rows), out$n_val, out$n_cal,
            if (isTRUE(out$repeated)) " This version was evaluated before; its evaluation is shown and nothing was refit." else "",
            if (identical(out$label, "revised during development")) {
              " The earlier evaluation is historical and cannot carry the independent-validation label."
            } else if (isTRUE(rec$after_development)) {
              " The reservation was drawn after development began, so this is a split of data already examined."
            } else {
              " The reserved rows played no part in fitting or selecting this version."
            }
          )
        ),
        if (!is.null(r$family_reason) && !is.na(r$family_reason)) div(class = "text-muted small", r$family_reason),
        lapply(names(r$status), function(nm) sieve_status_alert(r$status[[nm]], what = sprintf("%s model", nm)))
      )
    })
    output$holdout_versions <- renderDT({
      rec <- holdout_now()
      req(rec, length(rec$evaluations))
      sieve_dt(sieve_holdout_evaluation_table(rec),
        rownames = FALSE, selection = "none", options = list(dom = "t", scrollX = TRUE)
      )
    })
    output$holdout_tables <- renderUI({
      out <- holdout_result()
      req(out, is.null(out$error), out$result)
      r <- out$result
      tag <- if (is.null(r$family) || is.na(r$family)) "" else sprintf(" (%s)", r$family)
      fit <- sieve_dt(r$table,
        rownames = FALSE, selection = "none", options = list(dom = "t", scrollX = TRUE),
        colnames = c(
          "Sample", "n", paste0("CFI", tag), paste0("RMSEA", tag),
          sprintf("RMSEA %s lower", sieve_rmsea_ci_words(r$rmsea_conf_level %||% r$conf_level %||% NA_real_)),
          sprintf("RMSEA %s upper", sieve_rmsea_ci_words(r$rmsea_conf_level %||% r$conf_level %||% NA_real_)),
          "SRMR", "ECVI", "ECVI lo", "ECVI hi"
        )
      ) |>
        formatRound(c("CFI", "RMSEA", "RMSEA.lo", "RMSEA.hi", "SRMR", "ECVI", "ECVI.lo", "ECVI.hi"), 3)
      xval <- if (!isTRUE(r$ordered) && !is.null(r$xval)) {
        sieve_dt(r$xval,
          rownames = FALSE, selection = "none", options = list(dom = "t"),
          colnames = c("Quantity", "ML discrepancy")
        ) |>
          formatRound("Discrepancy", 4)
      }
      rel <- if (!is.null(r$rel)) {
        sieve_dt(r$rel,
          rownames = FALSE, selection = "none", options = list(dom = "t", scrollX = TRUE),
          colnames = c(
            "Construct", "Coefficient", "Calibration", "Calibration CI lower", "Calibration CI upper",
            "Reserved", "Reserved CI lower", "Reserved CI upper", "Difference (reserved - calibration)",
            "Difference CI lower", "Difference CI upper", "Interval",
            "Calibration: largest one-factor residual correlation", "Reserved: largest one-factor residual correlation"
          )
        ) |>
          formatRound(c(
            "Calibration", "Calibration.lower", "Calibration.upper", "Validation", "Validation.lower",
            "Validation.upper", "Difference", "Difference.lower", "Difference.upper"
          ), 3)
      }
      tagList(
        p(class = "small text-muted mb-1 mt-2", "Fit in the calibration rows and in the reserved rows (the Validation row), with the ECVI."),
        fit,
        if (!is.null(xval)) p(class = "small text-muted mb-1 mt-2", "The calibration parameters applied unchanged to the reserved rows (tight), the reserved rows re-estimated (loose), and the shrinkage."),
        xval,
        p(class = "small text-muted mb-1 mt-2", "Construct reliability in each sample, as on the Reliability page."),
        rel
      )
    })

    output$cv_msg <- renderUI({
      r <- cv_shown()
      if (is.null(r)) {
        return(div(
          class = "text-muted",
          "Set a holdout proportion and seed, then press Create split."
        ))
      }
      if (!is.null(r$error)) {
        return(div(class = "alert alert-danger", r$error))
      }
      tagList(
        div(
          class = "text-muted",
          sprintf(
            "Calibration n = %d; evaluation n = %d. Disjoint subsets of the current data; prior exposure is not erased. This is internal assessment, not independent validation.%s",
            r$n_cal, r$n_val,
            if (is.null(sieve_holdout_n_sentence(state))) "" else paste0(" Drawn from the calibration rows only: ", sieve_holdout_n_sentence(state))
          )
        ),
        if (!is.null(r$family_reason) && !is.na(r$family_reason)) {
          div(class = "text-muted small", r$family_reason)
        },
        # A fitted half is not a certificate: say when either model failed,
        # did not converge, is not identified, or is improper.
        lapply(names(r$status), function(nm) {
          sieve_status_alert(r$status[[nm]], what = sprintf("%s model", nm))
        }),
        # What DMAR said while forming the ECVI intervals is shown with them.
        if (length(r$ecvi_notes)) {
          tags$details(
            class = "small text-muted",
            tags$summary(sprintf(
              "DMAR raised %d note%s about the ECVI interval",
              length(r$ecvi_notes), if (length(r$ecvi_notes) == 1L) "" else "s"
            )),
            tags$ul(lapply(gsub("\\s+", " ", r$ecvi_notes), tags$li))
          )
        }
      )
    })

    output$cv_tbl <- renderDT({
      r <- cv_shown()
      req(r, is.null(r$error), r$table)
      tag <- if (is.null(r$family) || is.na(r$family)) "" else sprintf(" (%s)", r$family)
      sieve_dt(
        r$table,
        rownames = FALSE, selection = "none",
        options = list(dom = "t", scrollX = TRUE),
        colnames = c(
          "Sample", "n", paste0("CFI", tag), paste0("RMSEA", tag),
          sprintf("RMSEA %s lower", sieve_rmsea_ci_words(r$rmsea_conf_level %||% r$conf_level %||% NA_real_)),
          sprintf("RMSEA %s upper", sieve_rmsea_ci_words(r$rmsea_conf_level %||% r$conf_level %||% NA_real_)),
          "SRMR", "ECVI", "ECVI lo", "ECVI hi"
        )
      ) |>
        formatRound(c(
          "CFI", "RMSEA", "RMSEA.lo", "RMSEA.hi", "SRMR",
          "ECVI", "ECVI.lo", "ECVI.hi"
        ), 3)
    })

    # Live split sizes as the percentage slider moves (before any fit).
    output$split_sizes <- renderUI({
      req(state$items, state$meta)
      meta <- state$meta
      active_items <- intersect(meta$item[meta$status == "active"], names(state$items))
      d <- sieve_numeric_columns(state$items, active_items)$data
      n <- sum(stats::complete.cases(d))
      p_hold <- (input$holdout %||% 50) / 100
      n_val <- max(1L, round(n * p_hold))
      div(
        class = "small text-muted",
        sprintf(
          "Training n = %d, validation n = %d (of %d complete cases).%s",
          n - n_val, n_val, n,
          if (is.null(sieve_holdout_n_sentence(state))) "" else paste0(" ", sieve_holdout_n_sentence(state))
        )
      )
    })

    output$xval_msg <- renderUI({
      r <- cv_shown()
      if (is.null(r) || !is.null(r$error)) return(NULL)
      if (isTRUE(r$ordered)) {
        return(div(
          class = "alert alert-info py-1 small",
          "The cross-validation discrepancy is defined for continuous (ML/FIML) ",
          "models; with ordered/binary items it is not shown."
        ))
      }
      NULL
    })

    output$xval_tbl <- renderDT({
      r <- cv_shown()
      req(r, is.null(r$error), r$xval, !isTRUE(r$ordered))
      sieve_dt(r$xval,
        rownames = FALSE, selection = "none",
        options = list(dom = "t"),
        colnames = c("Quantity", "ML discrepancy")
      ) |>
        formatRound("Discrepancy", 4)
    })

    output$rel_msg <- renderUI({
      r <- cv_shown()
      if (is.null(r) || !is.null(r$error)) return(NULL)
      NULL
    })

    output$rel_tbl <- renderDT({
      r <- cv_shown()
      req(r, is.null(r$error), r$rel)
      # Each row on one line, the table scrolling sideways: wrapped, the
      # fourteen columns (two of them sentences) give rows up to 232 px tall
      # in a 1,164 px box at 1600 by 1000, so only two of the six rows
      # would be in view.
      sieve_dt(r$rel,
        class = "display nowrap",
        rownames = FALSE, selection = "none",
        options = list(dom = "t", scrollX = TRUE),
        colnames = c(
          "Construct", "Coefficient", "Training", "Training CI lower", "Training CI upper",
          "Validation", "Validation CI lower", "Validation CI upper", "Difference (validation - training)",
          "Difference CI lower", "Difference CI upper", "Interval",
          "Training: largest one-factor residual correlation", "Validation: largest one-factor residual correlation"
        )
      ) |>
        formatRound(c(
          "Calibration", "Calibration.lower", "Calibration.upper", "Validation", "Validation.lower",
          "Validation.upper", "Difference", "Difference.lower", "Difference.upper"
        ), 3)
    })

  }, envir = envir)
  invisible(NULL)
}

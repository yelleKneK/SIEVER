#' Wire the Invariance Panel in the Finalize Module
#' @param envir The module server frame receiving its panel bindings.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_finalize_invariance_wire <- function(envir) {
  evalq({
    # ---- Measurement invariance -------------------------------------------
    # The ladder is DMAR::measurement_invariance(), the canonical
    # implementation: it fits the rungs of the ladder in force (five rather
    # than four with ordered items) and hands back the fitted lavaan objects
    # in its "fits" attribute. Everything on this tab (the ladder table, the
    # score test diagnosis, the partial refit, dMACS) reads those fits, so no
    # model is fitted twice.
    inv_result <- reactiveVal(NULL)
    keep_inv <- function(x) inv_result(bind(x, "inv"))
    inv_shown <- reactive(current(inv_result(), "invariance ladder", "Run invariance", "inv"))

    # The analysis frame: the active items read as numbers plus the grouping
    # column, keeping rows complete on the items and on the group. A column
    # that cannot be read as numbers is reported beside the frame
    # (`problems`, see sieve_numeric_columns()), never coerced to missing
    # values quietly.
    inv_frame <- function(g) {
      meta <- state$meta
      sp <- sieve_cfa_spec(meta, reserved = names(state$raw), estimator = estimator_choice())
      cols <- intersect(sp$observed, names(state$items))
      rule <- input$inv_missing %||% "auto"
      if (identical(rule, "auto")) rule <- if (identical(sp$missing, "fiml")) "fiml" else "complete"
      frame <- sieve_invariance_frame(state$items, state$raw[[g]], cols,
        categorical = length(sp$ordered) > 0L, missing = rule
      )
      frame$data[[g]] <- frame$group
      frame
    }

    # Run invariance, as a function: its button and the page's Update run it.
    run_inv_ladder <- function() {
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      started <- proc.time()[["elapsed"]]
      design_problem <- sieve_sampling_problem(state)
      if (!is.null(design_problem)) {
        keep_inv(list(error = design_problem))
        return()
      }
      g <- input$group
      if (is.null(g) || !nzchar(g)) {
        keep_inv(list(error = sprintf(paste(
          "Choose a grouping variable (a column with the Group role and %s) in",
          "the selector above, then press Run invariance."
        ), sieve_group_levels_text())))
        return(invisible(NULL))
      }
      frame <- inv_frame(g)
      if (length(frame$problems)) {
        keep_inv(list(error = sprintf(
          "The invariance ladder across '%s' cannot be fit. %s",
          g, paste(frame$problems, collapse = " ")
        )))
        return(invisible(NULL))
      }
      d <- frame$data
      gsize <- table(d[[g]])
      gsize <- gsize[gsize > 0]
      res <- withProgress(
        message = "Fitting the invariance ladder (DMAR)", value = 1,
        sieve_fit_invariance(d, g, state$meta, estimator = estimator_choice(), missing_policy = frame$missing)
      )
      # A fit refused before it starts (the estimator's memory guard, see
      # sieve_mlr_guard()) is stated in the refusal's own words, alone.
      if (isTRUE(res$refused)) {
        keep_inv(list(error = res$error, refused = TRUE))
        return(invisible(NULL))
      }
      if (!is.null(res$error)) {
        msg <- sprintf(
          "The invariance ladder across '%s' could not be fit: %s",
          g, res$error
        )
        # A failure the engine's message is known for (a category no case in
        # one group uses, a correlation matrix that is not positive
        # definite) names the item and the one action that fixes it.
        hint <- sieve_failure_hint(res$error)
        if (!is.null(hint)) {
          msg <- paste(msg, hint)
        }
        # A model that never reached lavaan is a setup problem, not a group
        # size problem, so the group size advice is added only when the fit
        # itself failed.
        if (!isTRUE(res$precondition) && is.null(hint)) {
          msg <- paste(msg, sprintf(
            paste(
              "A multiple group model needs every group to support the whole",
              "measurement model, so the usual fixes are a grouping variable with",
              "fewer and larger groups, or excluding items until each group carries",
              "enough of them (each group here has: %s)."
            ),
            paste(sprintf("%s n = %d", names(gsize), as.integer(gsize)),
              collapse = "; "
            )
          ))
        }
        keep_inv(list(error = msg))
        return(invisible(NULL))
      }
      mi <- res$mi
      labs <- sieve_inv_level_labels(res$ordered)
      # The rungs are DMAR's fits. The table is read from those fits by
      # sieve_fit_compare(): CFI and RMSEA in one family for every rung,
      # changes within that family, and lavaan's difference tests, at full
      # precision. Rounding happens only in the renderer.
      cmp <- res$compare
      shown <- unname(labs[mi$level])
      tbl <- data.frame(
        Model = shown, level = as.character(mi$level),
        chi_square = cmp$table$chisq, df = cmp$table$df,
        p_chi_square = cmp$table$pvalue, cfi = cmp$table$cfi, rmsea = cmp$table$rmsea,
        # Each rung is compared with the rung before it, named here.
        compared_with = c("", shown[-length(shown)]),
        delta_chi_square = cmp$table$delta_chisq, delta_df = cmp$table$delta_df,
        p_value = cmp$table$delta_p, delta_cfi = cmp$table$delta_cfi,
        delta_rmsea = cmp$table$delta_rmsea, note = cmp$table$note,
        stringsAsFactors = FALSE
      )
      fits <- attr(mi, "fits")

      keep_inv(list(
        table = tbl, mi = mi, fits = fits, levels = as.character(mi$level),
        labels = labs, group = g, data = d, ordered = res$ordered,
        estimator = res$estimator, missing = res$missing,
        compare = cmp, family = cmp$family, test_family = cmp$test_family,
        test = cmp$test_label, fit_indices = cmp$family,
        notes = res$notes, n = nrow(d), group_sizes = gsize, row_flow = frame$row_flow, sample_rule = frame$rule,
        group_labels = sieve_group_labels(fits[[1L]]),
        status = res$status, warnings = res$warnings, code = res$code, data_code = res$data_code,
        name_map = res$spec$name_map
      ))

      updateSelectInput(session,
        "inv_interpret_level",
        choices = as.character(mi$level),
        selected = tail(
          as.character(mi$level),
          1
        )
      )
      sieve_log(
        state,
        sprintf(
          paste(
            "Measurement invariance across '%s': the %s ladder (%s),",
            "fit by DMAR::measurement_invariance()."
          ),
          g, if (res$ordered) "ordered item" else "continuous item",
          paste(mi$level, collapse = ", ")
        ),
        # The DMAR call that fit the rungs, then the R that computes the
        # table on screen from them.
        c(res$code, sieve_compare_code(cmp, fits = "fits_mi"))
      )
      time_section("inv", started)
    }
    observeEvent(input$run_inv, run_inv_ladder())

    inv_interpretation <- reactive({
      r <- inv_shown()
      req(r, is.null(r$error), !isTRUE(r$stale))
      fit <- r$fits[[input$inv_interpret_level %||% tail(r$levels, 1)]]
      req(fit)
      out <- sieve_invariance_interpret(fit)
      sieve_log(state, "Interpret measurement invariance identification and latent mean contrasts", c(
        r$code,
        paste0(
          "invariance_interpretation <- SIEVER::sieve_invariance_interpret(fits_mi[[",
          sieve_r_literal(input$inv_interpret_level %||% tail(r$levels, 1)), "]])"
        )
      ))
      out
    })
    output$inv_interpret_note <- renderUI({
      r <- tryCatch(inv_interpretation(), error = function(e) e)
      if (inherits(r, "error")) return(p(class = "small", conditionMessage(r)))
      p(class = "small", r$note)
    })
    output$inv_latent_parameters <- renderDT({
      d <- inv_interpretation()$parameters
      sieve_dt(d, rownames = FALSE, options = list(dom = "tp", scrollX = TRUE)) |> formatRound(c("est", "se"), 3)
    })
    output$inv_anchors <- renderDT({
      sieve_dt(inv_interpretation()$anchors,
        rownames = FALSE,
        options = list(
          dom = "tp",
          scrollX = TRUE
        )
      )
    })
    output$inv_mean_contrasts <- renderDT({
      sieve_dt(inv_interpretation()$contrasts,
        rownames = FALSE,
        options = list(
          dom = "tp",
          scrollX = TRUE
        )
      ) |> formatRound(
        c(
          "estimate",
          "se",
          "lower",
          "upper"
        ),
        3
      )
    })
    anchor_choices <- reactive(list(
      sets = input$inv_anchor_sets, rationale = input$inv_anchor_rationale,
      discovery = input$inv_anchor_discovery, estimator = estimator_choice()
    ))
    anchor_result <- reactiveVal(NULL)
    # Compare and freeze anchor decisions, as a function: its button and the
    # page's Update run it.
    run_anchor <- function() {
      r <- inv_shown()
      req(r, is.null(r$error), !isTRUE(r$stale))
      started <- proc.time()[["elapsed"]]
      lines <- trimws(strsplit(input$inv_anchor_sets %||% "", "\n", fixed = TRUE)[[1]])
      lines <- lines[nzchar(lines)]
      split <- strsplit(lines, ":", fixed = TRUE)
      out <- tryCatch(
        {
          if (any(lengths(split) != 2L)) stop("Use name: item1, item2 on each line.", call. = FALSE)
          sets <- lapply(split, function(x) trimws(strsplit(x[2], ",", fixed = TRUE)[[1]]))
          names(sets) <- vapply(split, function(x) trimws(x[1]), character(1))
          sieve_anchor_sensitivity(r$data, r$group, state$meta, sets,
            estimator = estimator_choice(), missing = r$missing,
            rationale = input$inv_anchor_rationale %||% "", discovery = isTRUE(input$inv_anchor_discovery)
          )
        },
        error = function(e) list(error = conditionMessage(e))
      )
      out$choices <- anchor_choices()
      anchor_result(bind(out, "anchor"))
      if (is.null(out$error)) {
        refs <- state$anchor_decisions %||% list()
        key <- sieve_hash(list(out$anchor_sets, out$rationale, sieve_revision(state)))
        refs[[key]] <- sieve_audit_stamp(out, state)
        state$anchor_decisions <- refs
        for (model in out$models) {
          spec <- list(
            kind = "anchor_sensitivity", key = model$model_id,
            model = model$syntax, group = r$group, anchors = model$anchors,
            group_partial = model$released, rationale = out$rationale,
            discovery = out$discovery, missing = r$missing
          )
          sieve_register_analysis(state, list(
            spec = spec,
            estimates = list(
              parameters = model$parameters,
              contrasts = model$interpretation$contrasts, anchors = model$interpretation$anchors
            ),
            status = sieve_fit_status(model$fit), warnings = model$warnings
          ),
          note = paste("Anchor set", model$name, out$exposure)
          )
        }
        sieve_log(state, "Freeze anchor sensitivity decisions", c(
          r$data_code,
          paste0("anchor_sets <- ", paste(utils::capture.output(dput(out$anchor_sets)), collapse = "\n")),
          paste0(
            "anchor_comparison <- SIEVER::sieve_anchor_sensitivity(inv_data, group = ", sieve_r_literal(r$group),
            ", meta = meta, anchor_sets = anchor_sets, estimator = ",
            sieve_r_literal(sieve_estimator_record_choice(estimator_choice(), state$meta)),
            ", missing = ", sieve_r_literal(r$missing), ", rationale = ", sieve_r_literal(out$rationale),
            ", discovery = ", sieve_r_literal(out$discovery), ")"
          )
        ))
      }
      time_section("anchor", started)
    }
    observeEvent(input$inv_anchor_run, run_anchor())
    anchor_shown <- reactive({
      r <- current(anchor_result(), "anchor sensitivity", "Compare and freeze anchor decisions", "anchor")
      if (!is.null(r) && is.null(r$error) && !identical(r$choices, anchor_choices())) return(list(
        stale = TRUE,
        error = "The anchor choices changed. Compare and freeze anchor decisions again; the earlier models remain saved separately."
      ))
      r
    })
    output$inv_anchor_note <- renderUI({
      r <- anchor_shown()
      if (is.null(r)) return(NULL)
      if (!is.null(r$error)) return(div(class = "alert alert-warning", r$error))
      p(class = "small", paste(r$rationale, r$exposure, sep = ". "))
    })
    output$inv_anchor_contrasts <- renderDT({
      r <- anchor_shown()
      req(r, is.null(r$error))
      sieve_dt(r$contrasts,
        rownames = FALSE,
        options = list(
          dom = "tp",
          scrollX = TRUE
        )
      ) |> formatRound(
        c(
          "estimate",
          "lower",
          "upper"
        ),
        3
      )
    })

    # Above the ladder table, the one line that says what was fit and on
    # which observations, the analysis sample's rule, and every rung that
    # failed, did not converge, is not identified, or is improper (a fitted
    # rung is not a certificate).
    output$inv_head <- renderUI({
      r <- inv_shown()
      if (is.null(r) || !is.null(r$error)) {
        return(NULL)
      }
      flagged <- Filter(function(st) !identical(st$code, "ok"), r$status %||% list())
      tagList(
        invariance_method(r),
        lapply(names(flagged), function(lv) {
          tagList(
            sieve_status_alert(flagged[[lv]],
              warnings = character(0),
              what = sprintf("%s model", lv)
            ),
            if (!identical(flagged[[lv]]$code, "inadmissible")) {
              div(
                class = "small text-danger mb-2",
                sprintf("Comparisons that involve the %s model are not interpretable.", lv)
              )
            }
          )
        })
      )
    })

    # Under the ladder table: what to press before there is one, why it
    # could not be fit, or how to read each row (the family of the
    # indices, the rung each row is compared with, the test), with the
    # warnings the engine raised.
    output$inv_msg <- renderUI({
      r <- inv_shown()
      if (is.null(r)) {
        if (length(eligible_groups()) == 0L) {
          return(div(
            class = "alert alert-info",
            sprintf(paste(
              "No grouping variable is available. Invariance needs a column",
              "with the Group role (set on the Ingest tab) and %s, one that",
              "splits the sample into groups each large enough to fit the",
              "model; give such a column the Group role, or load data with",
              "one (the bundled example's school variable is one), and",
              "select it above."
            ), sieve_group_levels_text())
          ))
        }
        return(div(
          class = "text-muted",
          "Choose a grouping variable and press Run invariance."
        ))
      }
      if (!is.null(r$error)) {
        return(div(class = "alert alert-danger", r$error))
      }
      test <- if (is.null(r$test) || is.na(r$test)) "no rung comparison was possible" else r$test
      tagList(
        # How to read each row: which family the indices are in, which rung
        # the row is compared with, and which test made the comparison. The
        # reference values for the changes are conventions, stated below.
        div(
          class = "text-muted small mt-2",
          sprintf(
            paste(
              "CFI and RMSEA are in the %s family (named in the column headers). Each rung is",
              "compared with the rung before it, named in the Compared with column, by %s;",
              "the change in CFI and in RMSEA is the difference between the two rungs' values",
              "in that family. The reference values stated under the table are conventions",
              "(Cheung & Rensvold, 2002; Chen, 2007), not verdicts: SIEVER reports the",
              "changes and the test, and the decision is the researcher's."
            ),
            if (is.na(r$family)) "no" else r$family, test
          )
        ),
        if (length(r$warnings)) {
          tags$details(
            class = "alert alert-secondary small",
            tags$summary(sprintf(
              "The engine raised %d warning%s while fitting and comparing the rungs",
              length(r$warnings), if (length(r$warnings) == 1L) "" else "s"
            )),
            tags$ul(lapply(gsub("\\s+", " ", r$warnings), tags$li))
          )
        }
      )
    })

    output$inv_tbl <- renderDT({
      r <- inv_shown()
      req(r, is.null(r$error), r$table)
      # The family of every index is in its column header. A rung whose
      # values are not reported says why in the Note column, which is shown
      # only when there is something to say.
      has_note <- any(nzchar(r$table$note))
      show <- r$table[, setdiff(names(r$table), c("level", if (!has_note) "note")),
        drop = FALSE
      ]
      # Short rung names keep the comparison readable at laptop widths.
      # The complete constraints remain available on pointer or keyboard focus;
      # the saved result retains the original full labels and precision.
      rung_cell <- function(labels) {
        vapply(labels, function(label) {
          sprintf(
            '<span class="sieve-explanation" tabindex="0" data-sieve-explanation="%s" aria-label="%s">%s</span>',
            htmltools::htmlEscape(label, attribute = TRUE),
            htmltools::htmlEscape(label, attribute = TRUE),
            htmltools::htmlEscape(sub(" \\(.*$", "", label))
          )
        }, character(1))
      }
      show$Model <- rung_cell(show$Model)
      show$compared_with <- rung_cell(show$compared_with)
      tag <- if (is.na(r$family)) "" else sprintf(" (%s)", r$family)
      test_tag <- sprintf(" (%s)", r$test_family)
      sieve_dt(
        show,
        rownames = FALSE, selection = "none",
        escape = which(!names(show) %in% c("Model", "compared_with")),
        options = list(dom = "t", scrollX = TRUE),
        colnames = c(
          "Model", paste0("chi square", test_tag), "df", "p (exact fit)",
          paste0("CFI", tag), paste0("RMSEA", tag), "Compared with",
          "delta chi square", "delta df", "delta chi square p",
          paste0("delta CFI", tag), paste0("delta RMSEA", tag), if (has_note) "Note"
        )
      ) |>
        formatRound(c(
          "chi_square", "p_chi_square", "cfi", "rmsea",
          "delta_chi_square", "p_value", "delta_cfi",
          "delta_rmsea"
        ), 3)
    })

    # Every displayed statistic says what produced it: the function, the
    # estimator, the identification, which version of the fit indices is
    # tabled, and which difference test the rung comparisons used.
    invariance_method <- function(r) {
      if (is.null(r) || !is.null(r$error)) {
        return(NULL)
      }
      test <- if (is.null(r$test) || is.na(r$test)) {
        "no rung comparison was possible"
      } else {
        r$test
      }
      div(
        class = "small text-muted mb-2",
        sprintf(
            "Nested models across '%s', from least to most restricted, on %d eligible observations (%s).",
            r$group, r$n,
            paste(sprintf(
              "%s n = %d", names(r$group_sizes),
              as.integer(r$group_sizes)
            ), collapse = "; ")
          ),
          if (!is.null(r$sample_rule)) paste(
            r$sample_rule,
            if (!is.null(r$row_flow)) paste(
              sprintf(
                "%s = %d",
                names(r$row_flow),
                as.integer(r$row_flow[1, ])
              ),
              collapse = "; "
            )
          ),
        " Every rung uses this same eligible pool. Missing group labels and observations with no model-item responses are excluded. Ordered-item models use complete cases. The sampling declaration must be independent for this workflow. ",
        sprintf(
          paste(
            "The rungs are fit by DMAR::measurement_invariance(): estimator %s,",
            "missing = \"%s\", latent variances fixed at 1 (std.lv = TRUE).",
            "The table is read from those fitted models. %s",
            "The chi square shown for each model is the %s statistic, and the",
            "rung to rung comparison is lavaan's %s, which is computed from",
            "the two models' standard statistics and scaling corrections, never",
            "by subtracting one scaled chi square from another."
          ),
          r$estimator, r$missing, r$compare$common$reason, r$test_family, test
        ),
        if (isTRUE(r$ordered)) {
          paste(
            " Ordered indicators were declared, so the thresholds rung is",
            "fit between configural and metric (Wu & Estabrook, 2016), and",
            "listwise deletion replaces full information maximum likelihood",
            "because the categorical estimator cannot do both."
          )
        },
        if (length(r$notes)) {
          div(
            class = "mt-1", strong("From DMAR: "),
            paste(r$notes, collapse = " ")
          )
        }
      )

    }

    # The reference values for the changes, stated as conventions with the
    # conditions they come from and the family the displayed changes are in.
    output$inv_convention <- renderUI({
      r <- inv_shown()
      if (is.null(r) || !is.null(r$error)) {
        return(tags$small(class = "text-muted d-block mt-2", sieve_invariance_convention()))
      }
      tags$small(
        class = "text-muted d-block mt-2",
        sieve_invariance_convention(r$family),
        " The chi square difference test grows more sensitive as the sample grows, so ",
        "read it beside the changes rather than instead of them."
      )
    })

    # Every family at every rung, with its own changes, so the family the
    # table uses is the reader's to review. Values are rounded here only.
    output$inv_families <- renderUI({
      r <- inv_shown()
      if (is.null(r) || !is.null(r$error) || is.null(r$compare)) {
        return(NULL)
      }
      num <- function(x) {
        ifelse(is.finite(x), sprintf("%.3f", ifelse(abs(x) < 5e-4, 0, x)), "n/a")
      }
      rows <- unlist(lapply(c("robust", "scaled", "standard"), function(fam) {
        tab <- r$compare$by_family[[fam]]
        lapply(seq_len(nrow(tab)), function(i) {
          partial <- !identical(tab$scope[i], "model")
          tags$tr(
            tags$td(if (i == 1L) strong(fam)),
            tags$td(unname(r$labels[tab$model[i]])),
            tags$td(num(tab$cfi[i])), tags$td(num(tab$delta_cfi[i])),
            tags$td(num(tab$rmsea[i])), tags$td(num(tab$delta_rmsea[i])),
            tags$td(if (partial) {
              "correlations only: leaves out thresholds and intercepts"
            } else if (!is.finite(tab$cfi[i]) || !is.finite(tab$rmsea[i])) {
              tab$reason[i]
            })
          )
        })
      }), recursive = FALSE)
      tags$details(
        class = "small mt-2",
        tags$summary(sprintf(
          "CFI and RMSEA in every family (the table above uses the %s family)",
          if (is.na(r$family)) "no" else r$family
        )),
        p(
          class = "text-muted mt-1 mb-1",
          "Each change is the value at a rung minus the value of the same family at the ",
          "rung before it. The families are different statistics, so compare within a ",
          "block and never across blocks. For ordered items lavaan's robust indices are ",
          "computed from the correlations alone and do not respond to constraints on ",
          "thresholds or intercepts; those rows are marked."
        ),
        tags$table(
          class = "table table-sm",
          tags$thead(tags$tr(
            tags$th("Family"), tags$th("Model"), tags$th("CFI"), tags$th("Change"),
            tags$th("RMSEA"), tags$th("Change"), tags$th("Note")
          )),
          tags$tbody(rows)
        )
      )
    })

  }, envir = envir)
  invisible(NULL)
}

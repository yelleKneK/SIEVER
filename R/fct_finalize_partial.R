#' Wire the Partial Panel in the Finalize Module
#' @param envir The module server frame receiving its panel bindings.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_finalize_partial_wire <- function(envir) {
  evalq({
    # ---- Partial invariance: score-test diagnosis and selective refit ------
    # The rungs offered for diagnosis are the ones DMAR actually fit, minus
    # configural, which imposes no cross-group equality constraints and so has
    # nothing to score test. With ordered items this list includes thresholds.
    observe({
      r <- inv_shown()
      if (is.null(r) || !is.null(r$error)) {
        return(NULL)
      }
      lv <- setdiff(r$levels, "configural")
      lv <- lv[vapply(lv, function(x) !is.null(r$fits[[x]]), logical(1))]
      if (!length(lv)) {
        return(NULL)
      }
      updateSelectInput(
        session, "inv_level",
        choices = stats::setNames(lv, unname(r$labels[lv])),
        selected = sieve_keep_choice(isolate(input$inv_level), lv, if ("scalar" %in% lv) "scalar" else lv[length(lv)])
      )
    })

    # Maps lavaan's per-constraint score tests back to named parameters via
    # the parameter table's plabels, so the display reads "item ~1, group 1
    # vs 2" rather than ".p12. == .p47.".
    diagnose_rung <- function() {
      r <- inv_shown()
      if (!is.null(r) && isTRUE(r$stale)) {
        return(list(error = r$error))
      }
      if (is.null(r) || !is.null(r$error)) {
        return(list(error = "Run the invariance ladder first, then diagnose the rung that failed."))
      }
      lv <- input$inv_level %||% "scalar"
      if (identical(lv, "configural")) {
        return(list(error = paste(
          "The configural model imposes no equality constraints across groups,",
          "so there is nothing to score test. Choose the rung that failed."
        )))
      }
      f <- r$fits[[lv]]
      if (is.null(f)) {
        return(list(error = sprintf(
          "The %s model was not among the rungs that were fit; rerun the ladder or choose another rung.",
          lv
        )))
      }
      # The score tests only rank the constraints for release. Whatever
      # lavaan says while computing them is kept and shown with the table,
      # and a failure carries lavaan's reason.
      scored <- sieve_capture(lavaan::lavTestScore(f, univariate = TRUE))
      sc <- scored$value
      if (!scored$ok || is.null(sc) || is.null(sc$uni) || !nrow(sc$uni)) {
        return(list(error = paste0(
          "No equality constraints could be score-tested for this model",
          if (is.null(scored$error)) "." else sprintf(
            " (lavaan: %s).",
            sub("[.]$", "", gsub("\\s+", " ", scored$error))
          )
        )))
      }
      pt <- lavaan::parTable(f)
      describe <- function(pl) {
        row <- pt[pt$plabel == pl, , drop = FALSE]
        if (!nrow(row)) return(c(
          par = NA_character_, type = NA_character_,
          grp = NA_character_, lhs = NA_character_,
          op = NA_character_, rhs = NA_character_
        ))
        r1 <- row[1, ]
        type <- switch(r1$op,
          "=~" = "loading",
          "~1" = "intercept",
          "~~" = "residual",
          "|" = "threshold",
          r1$op
        )
        c(
          par = sprintf("%s %s %s", r1$lhs, r1$op, r1$rhs), type = type,
          grp = as.character(r1$group), lhs = r1$lhs, op = r1$op, rhs = r1$rhs
        )
      }
      uni <- sc$uni
      left <- t(vapply(as.character(uni$lhs), describe, character(6)))
      right <- t(vapply(as.character(uni$rhs), describe, character(6)))
      tbl <- data.frame(
        parameter = left[, "par"],
        type = left[, "type"],
        groups = sprintf("%s vs %s", left[, "grp"], right[, "grp"]),
        X2 = uni$X2, df = uni$df, p = uni$p.value,
        lhs = left[, "lhs"], op = left[, "op"], rhs = left[, "rhs"],
        stringsAsFactors = FALSE
      )
      tbl <- tbl[!is.na(tbl$parameter), , drop = FALSE]
      # The table shows the researcher's label for each latent variable; the
      # engine's names (lhs, op, rhs) are kept for the refit.
      tbl$parameter <- trimws(sprintf(
        "%s %s %s", sieve_show_names(tbl$lhs, r$name_map),
        tbl$op, sieve_show_names(tbl$rhs, r$name_map)
      ))
      tbl <- tbl[order(-tbl$X2), , drop = FALSE]
      row.names(tbl) <- NULL
      sieve_log(
        state,
        sprintf("Partial invariance diagnosis: univariate score tests per equality constraint of the %s model.", lv),
        c(
          r$code, sprintf("sc <- lavaan::lavTestScore(fits_mi[[\"%s\"]], univariate = TRUE)", lv),
          "sc$uni  # X2 per released constraint; map plabels via lavaan::parTable()"
        )
      )
      list(
        table = tbl, level = lv, label = unname(r$labels[lv]),
        warnings = scored$warnings
      )
    }
    # Diagnose noninvariance, as a function: its button and the page's
    # Update run it, and the result is kept until the next run.
    diag_store <- reactiveVal(NULL)
    run_diagnosis <- function() {
      started <- proc.time()[["elapsed"]]
      diag_store(bind(diagnose_rung(), "diagnosis"))
      time_section("diagnosis", started)
    }
    observeEvent(input$inv_diag, run_diagnosis())
    inv_diag_run <- reactive({
      kept <- diag_store()
      req(kept)
      kept
    })
    inv_diag <- reactive(current(inv_diag_run(), "diagnosis", "Diagnose noninvariance", "diagnosis"))

    output$inv_diag_note <- renderUI({
      d <- tryCatch(inv_diag(), error = function(e) NULL)
      if (is.null(d)) {
        return(div(
          class = "text-muted small",
          "Run the ladder, pick the failing rung, then press Diagnose noninvariance."
        ))
      }
      if (!is.null(d$error)) {
        return(div(class = "alert alert-info", d$error))
      }
      tagList(
        div(
          class = "text-muted small",
          sprintf(
            paste(
              "Score test per equality constraint of the %s model, largest first,",
              "computed on the fit DMAR returned (no refitting). Click rows to select",
              "what to free, then press Refit freeing selected. A parameter freed",
              "here is left free at every rung of the refit, and the partial model is",
              "compared with the rung before it under the same freed parameters."
            ),
            d$level
          )
        ),
        if (length(d$warnings)) {
          tags$details(
            class = "alert alert-secondary small",
            tags$summary(sprintf(
              "lavaan raised %d warning%s while computing the score tests",
              length(d$warnings), if (length(d$warnings) == 1L) "" else "s"
            )),
            tags$ul(lapply(gsub("\\s+", " ", d$warnings), tags$li))
          )
        }
      )
    })

    output$inv_diag_tbl <- renderDT({
      d <- tryCatch(inv_diag(), error = function(e) NULL)
      req(d, is.null(d$error))
      show <- d$table[, c("parameter", "type", "groups", "X2", "df", "p"), drop = FALSE]
      sieve_dt(show,
        rownames = FALSE, selection = "multiple",
        options = list(dom = "tp", pageLength = 10),
        colnames = c(
          "Constrained parameter", "Kind", "Groups",
          "Score X2", "df", "p"
        )
      ) |>
        formatRound(c("X2", "p"), 3)
    })

    refit_partial <- function() {
      d <- tryCatch(inv_diag(), error = function(e) NULL)
      r <- inv_shown()
      if (!is.null(r) && isTRUE(r$stale)) {
        return(list(error = r$error))
      }
      if (!is.null(d) && isTRUE(d$stale)) {
        return(list(error = d$error))
      }
      if (is.null(d) || !is.null(d$error) || is.null(r) || !is.null(r$error)) {
        return(list(error = "Diagnose first, then select rows to free."))
      }
      sel <- input$inv_diag_tbl_rows_selected
      if (!length(sel)) {
        return(list(error = "Select at least one row of the diagnosis table to free."))
      }
      picked <- d$table[sel, , drop = FALSE]
      # An intercept constraint has an empty right hand side ("x ~1"), so the
      # assembled specification is trimmed before it reaches lavaan and the log.
      partial <- unique(trimws(sprintf(
        "%s %s %s", picked$lhs, picked$op,
        picked$rhs
      )))
      lv <- d$level
      pos <- match(lv, r$levels)
      if (is.na(pos) || pos < 2L) {
        return(list(error = "That rung has no less restricted rung to compare against."))
      }
      # The refit goes through the same DMAR entry point as the ladder, with
      # group_partial naming the freed parameters, so the partial model differs
      # from the rung only by those parameters and nothing else (same
      # estimator, same missing data handling, same identification). DMAR takes
      # a leading subset of the ladder, so only the rungs up to this one are
      # fit, each with the freed parameters.
      res <- withProgress(
        message = "Refitting with the selected parameters free (DMAR)", value = 1,
        sieve_fit_invariance(r$data, r$group, state$meta,
          group_partial = partial,
          levels = r$levels[seq_len(pos)],
          object = "mi_partial", compare = FALSE,
          estimator = estimator_choice(), missing_policy = r$missing
        )
      )
      if (isTRUE(res$refused)) {
        return(list(error = res$error, refused = TRUE))
      }
      if (!is.null(res$error)) {
        return(list(error = sprintf(
          "The partial invariance model could not be fit: %s",
          res$error
        )))
      }
      mip <- res$mi
      # The partial model is compared with the rung before it under the same
      # freed parameters (Byrne, Shavelson & Muthen, 1989), which is the model
      # it is nested in. That rung comes from this refit, not from the full
      # ladder: a loading freed at the scalar rung is still held equal in the
      # full metric model, so that model and the partial scalar model are not
      # nested. The pair is compared within one family of fit indices (the
      # ladder's own family when both models report it), and the difference
      # test is lavaan's, with its warnings kept.
      pc <- sieve_partial_compare(attr(mip, "fits"), lv,
        partial_status = res$status,
        ladder_fits = r$fits, prefer = r$family
      )
      if (!is.null(pc$error)) {
        return(list(error = pc$error))
      }
      f_par <- pc$fit
      prev_name <- pc$prev
      cmp <- pc$compare
      sieve_log(
        state,
        sprintf(
          "Partial %s invariance across '%s': freed %s (Byrne, Shavelson & Muthen, 1989).",
          lv, r$group, paste(partial, collapse = "; ")
        ),
        c(
          res$code,
          # The two models, their indices in the one family used for both,
          # and lavaan's difference test, as shown on screen.
          sprintf(
            "pair <- list(\"%s\" = fits_mi_partial[[\"%s\"]], \"%s\" = fits_mi_partial[[\"%s\"]])",
            pc$labels[1L], prev_name, pc$labels[2L], lv
          ),
          "# both rungs carry the freed parameters, so the second is nested in the first",
          sieve_compare_code(cmp, fits = "pair")
        )
      )
      list(
        freed = partial,
        freed_label = trimws(sprintf(
          "%s %s %s", sieve_show_names(picked$lhs, r$name_map),
          picked$op, sieve_show_names(picked$rhs, r$name_map)
        )),
        level = lv, prev = prev_name, fit = f_par,
        prev_fit = pc$reference, reference_partial = pc$reference_partial,
        label = unname(r$labels[lv]), prev_label = unname(r$labels[prev_name]),
        cfi = cmp$table$cfi[2L], rmsea = cmp$table$rmsea[2L],
        dcfi = cmp$table$delta_cfi[2L], drmsea = cmp$table$delta_rmsea[2L],
        lrt_chisq = cmp$table$delta_chisq[2L], lrt_df = cmp$table$delta_df[2L],
        lrt_p = cmp$table$delta_p[2L], test = cmp$test_label,
        family = cmp$family, compare = cmp, status = res$status[[lv]],
        prev_status = res$status[[prev_name]],
        warnings = unique(c(res$warnings, cmp$warnings)), code = res$code
      )
    }
    # Refit freeing selected: the selection is the researcher's, made in the
    # diagnosis table, so the page's Update leaves this result to its own
    # button (a redrawn diagnosis table does not keep a selection).
    partial_store <- reactiveVal(NULL)
    observeEvent(input$inv_refit, {
      started <- proc.time()[["elapsed"]]
      partial_store(bind(refit_partial(), "partial"))
      time_section("partial", started)
    })
    inv_partial_run <- reactive({
      kept <- partial_store()
      req(kept)
      kept
    })
    inv_partial <- reactive(current(
      inv_partial_run(), "partial invariance refit",
      "Refit freeing selected", "partial"
    ))

  }, envir = envir)
  invisible(NULL)
}

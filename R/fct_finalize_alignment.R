#' Wire the Alignment Panel in the Finalize Module
#' @param envir The module server frame receiving its panel bindings.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_finalize_alignment_wire <- function(envir) {
  evalq({
    # ---- Alignment: approximate invariance across groups -------------------
    # DMAR::measurement_alignment() is defined for a single factor, so one
    # construct is aligned at a time. It refits the configural model and then
    # optimizes the simplicity function from several starting values, so it
    # runs on an explicit button press rather than reactively.


    align_constructs <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$meta)
      m <- state$meta[state$meta$status == "active", , drop = FALSE]
      cons <- unique(m$construct[sieve_construct_assigned(m$construct)])
      # Three items is the identification minimum for a single factor.
      cons[vapply(cons, function(cn) sum(m$construct == cn) >= 3L, logical(1))]
    })

    observe({
      cn <- align_constructs()
      updateSelectInput(session, "align_construct",
        choices = cn,
        selected = sieve_keep_choice(isolate(input$align_construct), cn, if (length(cn)) cn[1] else NULL)
      )
    })

    run_alignment <- function() {
      problem <- sieve_sampling_problem(state)
      if (!is.null(problem)) return(list(error = problem))
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta, state$raw)

      g <- input$group %||% ""
      cn <- input$align_construct %||% ""
      if (!nzchar(g)) {
        return(list(error = sprintf(paste(
          "Choose a grouping variable. Only columns with the Group role and %s",
          "are offered on this tab."
        ), sieve_group_levels_text())))
      }
      if (!nzchar(cn)) {
        return(list(error = paste(
          "Choose a construct. Alignment is defined for a single factor, so",
          "one construct is aligned at a time, and only constructs with three",
          "or more active items are offered."
        )))
      }
      m <- state$meta[state$meta$status == "active", , drop = FALSE]
      its <- intersect(m$item[m$construct == cn], names(state$items))
      if (length(its) < 3L) {
        return(list(error = sprintf(
          paste(
            "'%s' has %d active items. A single factor needs three or more for",
            "identification, so this construct cannot be aligned."
          ),
          cn, length(its)
        )))
      }
      # The values are read as numbers once; a column that cannot be read is
      # refused with the reason rather than coerced to missing values.
      read <- sieve_numeric_columns(state$items, its)
      if (nrow(read$problems)) {
        return(list(error = sprintf(
          "'%s' cannot be aligned. %s", cn,
          paste(read$text, collapse = " ")
        )))
      }
      d <- read$data
      d[[g]] <- state$raw[[g]]
      d <- d[stats::complete.cases(d), , drop = FALSE]
      gsize <- table(d[[g]])
      gsize <- gsize[gsize > 0]
      if (length(gsize) < 2L) {
        return(list(error = sprintf(paste(
          "'%s' leaves fewer than two groups once rows incomplete on the %s",
          "items are dropped, so there is nothing to align across."
        ), g, cn)))
      }
      standardized <- isTRUE(input$align_std)
      if (standardized) {
        d[its] <- lapply(d[its], function(v) as.numeric(scale(v)))
      }
      n_starts <- max(1L, as.integer(sieve_finite_or(input$align_starts, 10L)))
      # The random starts always run from a known seed, which DMAR sets
      # locally (restoring the session's stream): the supplied one, or one
      # drawn from the session stream when the input is empty, so the same
      # solution can be reached again from the recorded call.
      rng <- sieve_draw_seed(sieve_seed_input(input$align_seed))
      seed <- rng$seed
      ident <- input$align_ident %||% "fixed"
      # The single factor is the construct, under the latent name the
      # ledger's name plan gives it everywhere else (its label when that is
      # usable, otherwise a name clear of every item, of the grouping column,
      # and of the engine's reserved words). DMAR's default name is "f": an
      # item of that name would be read as the factor and taken out of the
      # model.
      plan <- sieve_name_plan(state$meta, reserved = names(d))
      factor_name <- sieve_latent_name(plan, cn)
      align_model <- sprintf("%s =~ %s", factor_name, paste(its, collapse = " + "))
      aligned <- withProgress(
        message = "Aligning the measurement parameters (DMAR)", value = 1,
        sieve_capture(DMAR::measurement_alignment(
          data = d, items = its, group = g, model = align_model, alignment = ident,
          estimator = "ML", n_starts = n_starts, seed = seed
        ))
      )
      if (!aligned$ok) {
        return(list(
          error = sprintf("Alignment did not produce a usable solution: %s", aligned$error),
          warnings = aligned$warnings
        ))
      }
      out <- aligned$value
      groups <- data.frame(
        group = as.character(out$group), n = as.integer(out$n),
        factor_mean = out$factor_mean, factor_variance = out$factor_variance,
        stringsAsFactors = FALSE
      )
      il <- attr(out, "item_loss")
      r2 <- data.frame(
        item = rownames(il),
        label = unname(state$meta$label[match(rownames(il), state$meta$item)]),
        R2_loadings = unname(attr(out, "R2_loadings")[rownames(il)]),
        R2_intercepts = unname(attr(out, "R2_intercepts")[rownames(il)]),
        loss_loadings = unname(il[, "loadings"]),
        loss_intercepts = unname(il[, "intercepts"]),
        loss_total = unname(il[, "total"]),
        stringsAsFactors = FALSE
      )
      r2 <- r2[order(-r2$loss_total), , drop = FALSE]
      row.names(r2) <- NULL
      sieve_log(
        state,
        sprintf(
          paste(
            "Alignment of '%s' across '%s': %d groups, %s identification,",
            "%d starting values%s (Asparouhov & Muthen, 2014;",
            "DMAR::measurement_alignment)."
          ),
          cn, g, nrow(groups), ident, n_starts,
          sprintf(", seed = %d%s", seed, if (rng$supplied) "" else " drawn for this run")
        ),
        c(
          sprintf(
            "align_items <- c(%s)",
            paste(sieve_r_literal(its), collapse = ", ")
          ),
          sprintf("# align_data: those items plus '%s', complete on both.", g),
          paste0("align_data <- inv_data[, c(align_items, ", sieve_r_literal(g), "), drop = FALSE]"),
          "align_data <- align_data[stats::complete.cases(align_data), , drop = FALSE]",
          if (standardized) {
            "align_data[align_items] <- lapply(align_data[align_items], function(v) as.numeric(scale(v)))"
          } else {
            "# items aligned on their raw scales (no standardization requested)"
          },
          sprintf("al <- DMAR::measurement_alignment(align_data, items = align_items, group = %s,", sieve_r_literal(g)),
          sprintf("  model = %s,", sieve_r_literal(align_model)),
          sprintf(
            "  alignment = \"%s\", estimator = \"ML\", n_starts = %d, seed = %d)",
            ident, n_starts, seed
          ),
          "al                          # group factor means and variances",
          "attr(al, \"R2_loadings\"); attr(al, \"R2_intercepts\"); attr(al, \"item_loss\")"
        )
      )
      list(
        table = groups, r2 = r2, construct = cn, group = g, items = its,
        factor_name = factor_name, name_map = stats::setNames(cn, factor_name),
        standardized = standardized, alignment = ident, seed = seed,
        seed_supplied = rng$supplied,
        n_starts = attr(out, "n_starts"), n_optima = attr(out, "n_optima"),
        converged = isTRUE(attr(out, "converged")),
        simplicity = attr(out, "simplicity_function"),
        starts = attr(out, "simplicity_starts"),
        epsilon = attr(out, "epsilon"),
        R2_total = attr(out, "R2_total"),
        model = align_model, warnings = aligned$warnings
      )
    }
    # Run alignment, as a function: its button and the page's Update run it.
    align_store <- reactiveVal(NULL)
    run_align <- function() {
      started <- proc.time()[["elapsed"]]
      align_store(bind(run_alignment(), "alignment"))
      time_section("alignment", started)
    }
    observeEvent(input$align_go, run_align())
    align_run <- reactive({
      kept <- align_store()
      req(kept)
      kept
    })
    align_res <- reactive(current(align_run(), "alignment", "Run alignment", "alignment"))

    output$align_note <- renderUI({
      res <- tryCatch(align_res(), error = function(e) NULL)
      over <- sieve_groups_over_ceiling(state)
      ceiling_note <- if (length(over)) {
        sprintf(
          paste(
            "Grouping variables excluded by the ceiling of %d distinct values",
            "on this tab: %s. Alignment is meant for more groups than that,",
            "so if one of these is the grouping you want, the ceiling is",
            "what you are hitting."
          ),
          sieve_group_levels()[["max"]], paste(over, collapse = ", ")
        )
      } else {
        NULL
      }
      if (is.null(res)) {
        return(div(
          class = "text-muted small",
          "Choose a grouping variable and a construct, then press Run alignment. ",
          "Alignment is defined for two groups but is intended for many; with ",
          "two the ordinary ladder above is the better tool. ", ceiling_note
        ))
      }
      # What the engine said while fitting and aligning is shown, not dropped.
      said <- if (length(res$warnings)) {
        tags$details(
          class = "alert alert-secondary small",
          tags$summary(sprintf(
            "The engine raised %d warning%s during the alignment",
            length(res$warnings), if (length(res$warnings) == 1L) "" else "s"
          )),
          tags$ul(lapply(gsub("\\s+", " ", res$warnings), tags$li))
        )
      }
      if (!is.null(res$error)) {
        return(tagList(div(class = "alert alert-danger", res$error, " ", ceiling_note), said))
      }
      two_group <- if (nrow(res$table) == 2L) {
        paste(
          "With only two groups alignment has one group pair to work with,",
          "so it adds little to the ladder above; read it as a check on",
          "the ladder rather than as a substitute for it."
        )
      } else {
        NULL
      }
      tagList(said, div(
        class = "text-muted small",
        sprintf(
          paste(
            "Aligned '%s' (%d items) across '%s' (%d groups), %s",
            "identification, %d starting values%s. Items were %s. The",
            "configural model was fit with ML by",
            "DMAR::measurement_alignment()."
          ),
          res$construct, length(res$items), res$group, nrow(res$table),
          res$alignment, res$n_starts,
          sprintf(
            ", seed = %d%s", res$seed,
            if (isTRUE(res$seed_supplied)) "" else
              " (none was supplied, so this one was drawn and recorded)"
          ),
          if (res$standardized) "standardized first, so they contribute to the criterion on a common metric" else
            "left on their raw scales, so an item with a larger variance contributes more to the criterion"
        ),
        " ", two_group, " ", ceiling_note
      ))
    })

    output$align_tbl <- renderDT({
      res <- tryCatch(align_res(), error = function(e) NULL)
      req(res, is.null(res$error), res$table)
      sieve_dt(
        res$table,
        rownames = FALSE, selection = "none",
        options = list(dom = "t", scrollX = TRUE),
        colnames = c("Group", "n", "Factor mean", "Factor variance"),
        caption = sprintf(
          paste(
            "Estimated factor means and variances on the common",
            "metric set by %s identification."
          ),
          res$alignment
        )
      ) |>
        formatRound(c("factor_mean", "factor_variance"), 3)
    })

    output$align_diag <- renderUI({
      res <- tryCatch(align_res(), error = function(e) NULL)
      if (is.null(res) || !is.null(res$error)) {
        return(div(
          class = "text-muted small",
          "Run the alignment to see the achieved simplicity function, ",
          "whether the optimizer converged, and how many distinct optima ",
          "the starting values reached."
        ))
      }
      usable <- sum(is.finite(res$starts))
      multi <- res$n_optima > 1L
      alert <- if (!res$converged) {
        "alert alert-danger"
      } else if (multi) {
        "alert alert-warning"
      } else {
        "alert alert-success"
      }
      div(
        class = alert,
        HTML(sprintf(
          paste0(
            "<b>Simplicity function %.4f</b> at the retained solution, with ",
            "epsilon = %s. Optimizer converged: <b>%s</b>. Distinct optima ",
            "reached across %d starting values (%d usable): <b>%d</b>. ",
            "Overall R-squared invariance: loadings %.3f, intercepts %.3f."
          ),
          res$simplicity, format(res$epsilon),
          if (res$converged) "yes" else "no",
          res$n_starts, usable, res$n_optima,
          as.numeric(res$R2_total["loadings"]),
          as.numeric(res$R2_total["intercepts"])
        )),
        if (multi) {
          div(
            class = "mt-2",
            strong("More than one optimum. "),
            "The starting values reached ", res$n_optima, " distinct minima, ",
            "so the surface has local minima and the reported solution is the ",
            "best of several rather than the unique answer. Raise the number ",
            "of starting values and check that the factor means are stable ",
            "before reporting them."
          )
        },
        if (!res$converged) {
          div(
            class = "mt-2",
            strong("The optimizer did not report convergence. "),
            "Treat the factor means and variances as provisional: raise the ",
            "number of starting values, and check that the items are on a ",
            "common scale (standardizing them is the simple fix)."
          )
        },
        if (usable < res$n_starts) {
          div(
            class = "mt-2",
            sprintf(
              paste(
                "%d of %d starting values were discarded. A discarded",
                "start is one the optimizer could not complete or one",
                "that drifted into the degenerate branch of the",
                "criterion, where the factor variances run off and the",
                "aligned loadings collapse toward zero; that is not an",
                "estimate, so it is thrown away rather than reported."
              ),
              res$n_starts - usable, res$n_starts
            )
          )
        }
      )
    })

    output$align_r2_note <- renderUI({
      res <- tryCatch(align_res(), error = function(e) NULL)
      if (is.null(res) || !is.null(res$error)) {
        return(div(
          class = "text-muted small",
          "Run the alignment to see the per item invariance measures."
        ))
      }
      div(
        class = "text-muted small",
        sprintf(
          paste(
            "Per item R-squared and simplicity contributions for '%s'",
            "across '%s', from DMAR::measurement_alignment(). A value",
            "near 1 means the estimated factor means and variances",
            "account for that item's group to group variation, which is",
            "what approximate invariance looks like for the item; a low",
            "or negative value marks an item whose parameters they",
            "cannot explain."
          ),
          res$construct, res$group
        )
      )
    })

    output$align_r2_tbl <- renderDT({
      res <- tryCatch(align_res(), error = function(e) NULL)
      req(res, is.null(res$error), res$r2)
      sieve_dt(
        res$r2,
        rownames = FALSE, selection = "none",
        options = list(
          dom = "tp", pageLength = 25, scrollX = TRUE,
          ordering = FALSE
        ),
        colnames = c(
          "Item", "Item Name", "R-squared (loadings)",
          "R-squared (intercepts)", "Loss (loadings)",
          "Loss (intercepts)", "Loss (total)"
        )
      ) |>
        formatRound(c(
          "R2_loadings", "R2_intercepts", "loss_loadings",
          "loss_intercepts", "loss_total"
        ), 3)
    })

  }, envir = envir)
  invisible(NULL)
}

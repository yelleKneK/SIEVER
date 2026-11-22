#' Wire the Impact Panel in the Finalize Module
#' @param envir The module server frame receiving its panel bindings.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_finalize_impact_wire <- function(envir) {
  evalq({
    # ---- dMACS: the effect size of the noninvariance -----------------------
    # The reference and focal selectors are filled from the fitted model's own
    # group labels, so they always name groups the fit actually has.
    observe({
      r <- inv_shown()
      labs <- if (is.null(r) || !is.null(r$error)) character(0) else r$group_labels
      updateSelectInput(session, "dmacs_ref",
        choices = labs,
        selected = sieve_keep_choice(isolate(input$dmacs_ref), labs, if (length(labs)) labs[1] else NULL)
      )
      updateSelectInput(session, "dmacs_foc",
        choices = labs,
        selected = sieve_keep_choice(isolate(input$dmacs_foc), labs, if (length(labs) > 1L) labs[2] else NULL)
      )
    })

    impact_fit <- reactive({
      r <- tryCatch(inv_shown(), error = function(e) NULL)
      if (is.null(r) || !is.null(r$error) || isTRUE(r$stale)) return(NULL)
      part <- if (identical(input$dmacs_source %||% "partial", "partial"))
        tryCatch(inv_partial(), error = function(e) NULL) else NULL
      if (!is.null(part$fit) && is.null(part$error) && !isTRUE(part$stale)) return(part$fit)
      r$fits[[input$inv_level %||% "scalar"]]
    })
    output$inv_impact_kind <- shiny::renderText({
      fit <- impact_fit()
      if (is.null(fit)) "pending" else if (sieve_fit_is_categorical(fit)) "ordered" else "continuous"
    })
    shiny::outputOptions(output, "inv_impact_kind", suspendWhenHidden = FALSE)
    output$inv_impact_pending <- renderUI({
      if (is.null(impact_fit())) p(class = "small text-muted",
        "Run the invariance ladder and select a fitted model. Its actual treatment determines whether dMACS or ordinal expected-score impact is offered.")
    })
    compute_dmacs <- function() {
      r <- inv_shown()
      if (!is.null(r) && isTRUE(r$stale)) {
        return(list(error = r$error))
      }
      if (is.null(r) || !is.null(r$error)) {
        return(list(error = paste(
          "Run the invariance ladder first; dMACS is read off one of its",
          "fitted models."
        )))
      }
      part <- tryCatch(inv_partial(), error = function(e) NULL)
      if (!is.null(part) && isTRUE(part$stale)) {
        return(list(error = part$error))
      }
      want_partial <- identical(input$dmacs_source %||% "partial", "partial")
      have_partial <- !is.null(part) && is.null(part$error) && !is.null(part$fit)
      use_partial <- want_partial && have_partial
      lv <- if (use_partial) part$level else (input$inv_level %||% "scalar")
      lname <- unname(r$labels[lv])
      if (is.na(lname)) lname <- lv
      fit <- if (use_partial) part$fit else r$fits[[lv]]
      # A categorical fit is refused outright, ahead of the rung gate, because
      # no rung of an ordered ladder makes the index meaningful. dMACS is a
      # function of the loadings and the intercepts. For an ordered item the
      # intercept of the underlying continuous response is not a free
      # parameter carrying item location: it is fixed at zero in the reference
      # group and freed in the remaining groups only to identify the model
      # once the thresholds are held equal, and the thresholds are what locate
      # the item. So DMAR::dmacs(), which reads lambda and nu and never
      # inspects the thresholds, is not computing the quantity dMACS is
      # defined as. Measured on this app's coarsened ordered fixture at the
      # metric rung: every threshold equality constrained, every loading equal
      # to within 2.2e-16, the correct value therefore zero for every item,
      # and the index returning 0.98 for one of them.
      if (isTRUE(r$ordered) || sieve_fit_is_categorical(fit)) {
        return(list(error = paste0(
          "dMACS is not reported for this model, because the items were ",
          "declared ordered or binary and the fit therefore estimates ",
          "thresholds. The index (Nye & Drasgow, 2011), and ",
          "DMAR::dmacs() with it, is a function of the loadings and the ",
          "intercepts. For an ordered item that is not where the location ",
          "information sits. The observed response is a coarsening of an ",
          "underlying continuous response, the thresholds are what place the ",
          "item on the latent scale, and the intercept of that underlying ",
          "response is not free to carry location: it is fixed at zero in the ",
          "reference group and freed in the remaining groups only so the ",
          "model is identified once the thresholds are constrained equal. ",
          "Reading those identification constants as a difference in item ",
          "intercepts manufactures an effect size out of the parameterization: ",
          "on the coarsened example bundled with this app, every threshold is ",
          "held equal across groups and every loading agrees to within 2.2e-16, ",
          "so the correct value is zero for every item, and an index computed ",
          "from the loadings and those intercepts alone returns 0.98 for one ",
          "of them. Sizing noninvariance for ordered items calls for a ",
          "different quantity, built on the expected item score function ",
          "implied by an item's thresholds together with its loading: the ",
          "ordinal expected-score impact this tab offers for an ordered model. ",
          "The rung comparisons and the score tests above also read the ",
          "thresholds."
        )))
      }
      # dMACS needs the two groups' loadings and intercepts on a common
      # metric, which means the intercepts must be constrained somewhere.
      # Configural gives each group its own latent scale and a thresholds-only
      # rung leaves the loadings free, so neither qualifies. The metric rung
      # does not qualify either, and that is the subtle case: with the
      # loadings equal but every intercept free, the group latent means are
      # fixed at zero, so a genuine difference in the latent mean is absorbed
      # into the free intercepts and dMACS reads it as intercept
      # noninvariance. On data that are invariant by construction and differ
      # only in latent mean, the metric rung yields a large dMACS while the
      # scalar rung correctly yields zero. Only a model that constrains the
      # intercepts (scalar or strict), or a partial invariance refit of one,
      # estimates the focal latent mean freely and puts the intercepts on the
      # common metric the index assumes.
      if (!lv %in% c("scalar", "strict")) {
        return(list(error = paste0(
          "dMACS is not interpretable from the ", lname, " model, so it is ",
          "not reported here. The index compares the reference and focal ",
          "measurement equations, which requires the loadings and the ",
          "intercepts to be on a common metric. A configural model sets each ",
          "group's latent scale separately and a thresholds-only rung leaves ",
          "the loadings free, so their parameters are not comparable at all. ",
          "The metric rung is the case worth understanding: the loadings are ",
          "equal there, but every intercept is free and the group latent ",
          "means are fixed at zero, so a real difference in the latent mean ",
          "is absorbed into the intercepts and would be reported as ",
          "noninvariance that does not exist. Judge the scalar or strict ",
          "rung, or, better, free the offending parameters with the partial ",
          "invariance refit above and judge that model: a partial scalar ",
          "solution is the setting dMACS was defined for, since the ",
          "constrained items anchor the metric while the freed ones carry the ",
          "noninvariance being sized."
        )))
      }
      if (is.null(fit)) {
        return(list(error = sprintf(
          "The %s model was not fit, so there is nothing to judge.", lname
        )))
      }
      glabs <- sieve_group_labels(fit)
      if (length(glabs) < 2L) {
        return(list(error = paste(
          "The fitted model has fewer than two groups, so there are no two",
          "measurement equations to compare. Rerun the ladder on a grouping",
          "variable with at least two groups."
        )))
      }
      ref <- input$dmacs_ref %||% ""
      foc <- input$dmacs_foc %||% ""
      named <- nzchar(ref) && nzchar(foc) && ref %in% glabs && foc %in% glabs &&
        !identical(ref, foc)
      if (length(glabs) > 2L && !named) {
        return(list(error = sprintf(
          paste0(
            "'%s' has %d groups (%s). dMACS compares exactly two groups at a ",
            "time, so name a reference group and a focal group above, and make ",
            "them different; every other pair is a separate run."
          ),
          r$group, length(glabs), paste(glabs, collapse = ", ")
        )))
      }
      if (!named) {
        ref <- glabs[1]
        foc <- glabs[2]
      }
      out <- tryCatch(DMAR::dmacs(fit = fit, reference = ref, focal = foc),
        error = function(e) conditionMessage(e)
      )
      if (is.character(out)) {
        return(list(error = sprintf("dMACS could not be computed: %s", out)))
      }
      sieve_dmar_require_columns(out, c("item", "dmacs", "factor_loading_reference",
        "factor_loading_focal", "intercept_reference", "intercept_focal", "sd_pooled"), "dMACS")
      tbl <- data.frame(
        item = out$item,
        label = unname(state$meta$label[match(out$item, state$meta$item)]),
        dmacs = out$dmacs,
        lambda_reference = out$factor_loading_reference,
        lambda_focal = out$factor_loading_focal,
        nu_reference = out$intercept_reference,
        nu_focal = out$intercept_focal,
        sd_pooled = out$sd_pooled,
        stringsAsFactors = FALSE
      )
      # Largest violations first, so the practically important items lead.
      tbl <- tbl[order(-tbl$dmacs), , drop = FALSE]
      row.names(tbl) <- NULL
      source_note <- if (use_partial) {
        sprintf(
          "the partial %s invariance refit (freed: %s)", lv,
          paste(part$freed, collapse = "; ")
        )
      } else if (want_partial) {
        sprintf(
          "the %s rung of the ladder as fitted (no partial invariance refit has been run yet)",
          lv
        )
      } else {
        sprintf("the %s rung of the ladder as fitted", lv)
      }
      sieve_log(
        state,
        sprintf(
          paste(
            "dMACS effect size of noninvariance (Nye & Drasgow, 2011) from %s:",
            "reference '%s' vs focal '%s'."
          ),
          source_note, ref, foc
        ),
        c(
          if (use_partial) part$code else r$code,
          sprintf(
            "fit_dmacs <- %s",
            if (use_partial) sprintf("fits_mi_partial[[\"%s\"]]", lv)
            else sprintf("fits_mi[[\"%s\"]]", lv)
          ),
          sprintf(
            "dmacs_result <- DMAR::dmacs(fit_dmacs, reference = %s, focal = %s)",
            sieve_r_literal(ref), sieve_r_literal(foc)
          )
        )
      )
      list(
        table = tbl, reference = attr(out, "reference"),
        focal = attr(out, "focal"), mean_focal = attr(out, "mean_focal"),
        sd_focal = attr(out, "sd_focal"), level = lv, label = lname,
        source = source_note, partial = use_partial, n_groups = length(glabs),
        # A fully constrained rung leaves nothing free to differ, so its
        # values are zero up to rounding; the note says so rather than
        # letting a reader puzzle over a column of zeros.
        all_zero = all(tbl$dmacs < 1e-8)
      )
    }
    ordinal_impact_choices <- reactive(list(
      source = input$dmacs_source %||% "partial",
      reference = input$dmacs_ref, focal = input$dmacs_foc, rung = input$inv_level %||% "scalar",
      mean = input$ordinal_impact_mean %||% 0, sd = input$ordinal_impact_sd %||% 1,
      anchors = input$ordinal_impact_anchors %||% ""
    ))
    # Compute ordinal expected-score impact, as a function: its button and
    # the page's Update run it, and the result is kept until the next run.
    ordinal_store <- reactiveVal(NULL)
    ordinal_compute <- function() {
      r <- inv_shown()
      req(r, is.null(r$error), !isTRUE(r$stale))
      fit <- if (identical(input$dmacs_source %||% "partial", "partial")) {
        p <- inv_partial()
        req(p, is.null(p$error))
        p$fit
      } else r$fits[[input$inv_level %||% "scalar"]]
      req(fit)
      scores <- lapply(
        state$items[,
          lavaan::lavNames(
            fit,
            "ov"
          ),
          drop = FALSE
        ],
        function(x) sort(unique(stats::na.omit(sieve_screen_numeric(x))))
      )
      out <- tryCatch(
        sieve_ordinal_fit_impact(
          fit, scores,
          input$dmacs_ref, input$dmacs_foc, input$ordinal_impact_mean %||% 0,
          input$ordinal_impact_sd %||% 1, input$ordinal_impact_anchors %||% ""
        ),
        error = function(e) list(error = conditionMessage(e))
      )
      if (is.null(out$error)) {
        records <- state$ordinal_impact_records %||% list()
        records[[out$model_key]] <- sieve_audit_stamp(out, state)
        state$ordinal_impact_records <- records
        sieve_log(state, "Ordinal expected-score impact", c(
          if (identical(input$dmacs_source %||% "partial", "partial")) p$code else r$code,
          paste0(
            "ordinal_impact_fit <- ",
            if (identical(
              input$dmacs_source %||% "partial",
              "partial"
            )) paste0(
              "fits_mi_partial[[",
              sieve_r_literal(p$level),
              "]]"
            ) else paste0(
              "fits_mi[[",
              sieve_r_literal(input$inv_level %||% "scalar"),
              "]]"
            )
          ),
          paste0("ordinal_category_scores <- ", paste(utils::capture.output(dput(scores)), collapse = "\n")),
          paste0(
            "ordinal_impact <- SIEVER::sieve_ordinal_fit_impact(ordinal_impact_fit, ordinal_category_scores, reference = ",
            sieve_r_literal(input$dmacs_ref),
            ", focal = ",
            sieve_r_literal(input$dmacs_foc),
            ", mean = ",
            sieve_r_literal(input$ordinal_impact_mean %||% 0),
            ", sd = ",
            sieve_r_literal(input$ordinal_impact_sd %||% 1),
            ", anchors = ",
            sieve_r_literal(input$ordinal_impact_anchors %||% ""),
            # The parameter draws behind the intervals, with the seed they
            # used (drawn when none was supplied), so the record replays.
            if (is.numeric(out$interval$attempted) && is.numeric(out$interval$rng$seed)) {
              sprintf(", B = %dL, seed = %d", as.integer(out$interval$attempted), as.integer(out$interval$rng$seed))
            } else "",
            ")"
          )
        ))
      }
      out$choices <- ordinal_impact_choices()
      bind(out, "ordinal")
    }
    run_ordinal <- function() {
      started <- proc.time()[["elapsed"]]
      ordinal_store(ordinal_compute())
      time_section("ordinal", started)
    }
    observeEvent(input$ordinal_impact_go, run_ordinal())
    ordinal_impact <- reactive({
      kept <- ordinal_store()
      req(kept)
      kept
    })
    ordinal_impact_shown <- reactive({
      r <- current(ordinal_impact(), "ordinal expected-score impact", "Compute ordinal expected-score impact", "ordinal")
      if (!is.null(r) && is.null(r$error) && !identical(r$choices, ordinal_impact_choices())) return(list(
        stale = TRUE,
        error = "The groups, model source, trait distribution, or anchor justification changed. Compute ordinal expected-score impact again; the earlier record remains saved."
      ))
      r
    })
    output$ordinal_impact_note <- renderUI({
      r <- ordinal_impact_shown()
      req(r)
      if (!is.null(r$error)) div(class = "alert alert-warning", r$error) else p(class = "small", r$limitation)
    })
    output$ordinal_impact_tbl <- renderDT({
      r <- ordinal_impact_shown()
      req(r, is.null(r$error))
      sieve_dt(r$table,
        rownames = FALSE,
        options = list(
          dom = "tp",
          scrollX = TRUE
        )
      ) |> formatRound(
        intersect(c(
          "signed", "signed_lower", "signed_upper", "absolute", "absolute_lower",
          "absolute_upper", "rms", "rms_lower", "rms_upper"
        ), names(r$table)),
        4
      )
    })

    output$ordinal_impact_totals <- renderDT({
      r <- ordinal_impact_shown()
      req(r, is.null(r$error), r$totals)
      sieve_dt(r$totals,
        rownames = FALSE,
        options = list(
          dom = "tp",
          scrollX = TRUE
        )
      ) |> formatRound(
        intersect(c(
          "signed", "signed_lower", "signed_upper", "absolute", "absolute_lower",
          "absolute_upper", "rms", "rms_lower", "rms_upper"
        ), names(r$totals)),
        4
      )
    })

    # Compute dMACS, as a function: its button and the page's Update run it.
    dmacs_store <- reactiveVal(NULL)
    run_dmacs <- function() {
      started <- proc.time()[["elapsed"]]
      dmacs_store(bind(compute_dmacs(), "dmacs"))
      time_section("dmacs", started)
    }
    observeEvent(input$dmacs_go, run_dmacs())
    dmacs_run <- reactive({
      kept <- dmacs_store()
      req(kept)
      kept
    })
    dmacs_res <- reactive(current(dmacs_run(), "dMACS table", "Compute dMACS", "dmacs"))

    output$dmacs_note <- renderUI({
      res <- tryCatch(dmacs_res(), error = function(e) NULL)
      if (is.null(res)) {
        return(div(
          class = "text-muted small",
          "Run the ladder (and, when a rung fails, the partial ",
          "invariance refit above), then press Compute dMACS."
        ))
      }
      if (!is.null(res$error)) {
        return(div(class = "alert alert-info", res$error))
      }
      moments <- paste(sprintf(
        "%s: mean %.3f, SD %.3f", names(res$mean_focal),
        as.numeric(res$mean_focal),
        as.numeric(res$sd_focal)
      ), collapse = "; ")
      div(
        class = "text-muted small",
        sprintf(
          paste(
            "From %s, computed by DMAR::dmacs(): '%s' is the reference group",
            "and '%s' the focal group%s, and the expectation is taken over the",
            "focal group's latent distribution on the common metric (%s)."
          ),
          res$source, res$reference, res$focal,
          if (res$n_groups > 2L) {
            sprintf(
              " (one pair of the %d groups; every other pair is a separate run)",
              res$n_groups
            )
          } else {
            ""
          },
          moments
        ),
        if (isTRUE(res$all_zero)) {
          div(
            class = "mt-1",
            strong("Every value is zero to numerical precision. "),
            "That is what a fully constrained rung must give: with the ",
            "loadings and the intercepts held equal across groups, nothing ",
            "is free to differ, so the two measurement equations coincide by ",
            "construction. To size a violation, diagnose the rung above, ",
            "free the offending parameters, and read this index from the ",
            "partial invariance refit, where the parameters you freed can ",
            "move while the rest still anchor the metric."
          )
        }
      )
    })

    output$dmacs_tbl <- renderDT({
      res <- tryCatch(dmacs_res(), error = function(e) NULL)
      req(res, is.null(res$error), res$table)
      sieve_dt(
        res$table,
        rownames = FALSE, selection = "none",
        options = list(
          dom = "tp", pageLength = 25, scrollX = TRUE,
          ordering = FALSE
        ),
        colnames = c(
          "Item", "Item Name", "dMACS", "Loading (reference)",
          "Loading (focal)", "Intercept (reference)",
          "Intercept (focal)", "Pooled SD"
        )
      ) |>
        formatRound(c(
          "dmacs", "lambda_reference", "lambda_focal",
          "nu_reference", "nu_focal", "sd_pooled"
        ), 3)
    })

  }, envir = envir)
  invisible(NULL)
}

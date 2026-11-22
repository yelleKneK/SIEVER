#' Wire the Context Panel in the Finalize Module
#' @param envir The module server frame receiving its panel bindings.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_finalize_context_wire <- function(envir) {
  evalq({
    # ---- Results are bound to the inputs they were computed from ----------
    # Every result this tab keeps (the split, the ladder, its diagnosis, the
    # partial refit, dMACS, the alignment) is stamped with the revision of
    # the dataset, the item data, and the ledger it was computed from, and
    # is shown only while those are the current inputs. After another
    # dataset is loaded, an item is reverse scored, or the ledger changes,
    # each panel says what changed and what to press, instead of showing
    # the old table.
    revision_now <- sieve_current_revision(state)
    status_records <- reactiveVal(list())
    group_key <- function(name) {
      name <- name %||% ""
      list(name = name, values = if (nzchar(name) && name %in% names(state$raw))
        sieve_hash(state$raw[[name]]) else NULL)
    }
    requested <- function(section) {
      specification <- sieve_lavaan_spec(state$meta, estimator = estimator_choice())
      estimation <- specification[c("estimator", "ordered", "missing")]
      missing <- input$inv_missing %||% "auto"
      if (identical(missing, "auto")) {
        missing <- if (identical(specification$missing, "fiml")) "fiml" else "complete"
      }
      invariance <- list(estimation = estimation, group = group_key(input$group), missing = missing)
      diagnosis <- c(invariance, list(rung = input$inv_level %||% "scalar"))
      partial <- c(diagnosis, list(selected = sort(as.integer(input$inv_diag_tbl_rows_selected))))
      switch(section,
        cv = {
          holdout <- (input$holdout %||% 50) / 100
          if (!is.finite(holdout) || holdout <= 0 || holdout >= 1) holdout <- .5
          # An invalid seed entry is kept as entered, so the split's refusal
          # of it (sieve_cross_validate()) is bound and shown.
          list(estimation = estimation, holdout = holdout,
            seed = tryCatch(sieve_seed_input(input$seed), sieve_seed_invalid = function(e) input$seed))
        },
        inv = invariance,
        diagnosis = diagnosis,
        partial = partial,
        anchor = c(invariance, list(sets = input$inv_anchor_sets, rationale = input$inv_anchor_rationale,
          discovery = isTRUE(input$inv_anchor_discovery))),
        dmacs = c(invariance, list(rung = input$inv_level %||% "scalar",
          source = input$dmacs_source %||% "partial", reference = input$dmacs_ref,
          focal = input$dmacs_foc, partial = if (identical(input$dmacs_source %||% "partial", "partial")) partial)),
        ordinal = c(invariance, list(choices = ordinal_impact_choices(),
          partial = if (identical(input$dmacs_source %||% "partial", "partial")) partial)),
        alignment = list(estimation = estimation, group = group_key(input$group),
          construct = input$align_construct, standardized = isTRUE(input$align_std),
          starts = max(1L, as.integer(sieve_finite_or(input$align_starts, 10L))),
          seed = sieve_seed_input(input$align_seed), identification = input$align_ident %||% "fixed")
      )
    }
    bind <- function(result, section = NULL) {
      result$.sampling <- sieve_sampling_spec(state)
      if (!is.null(section)) result$.request <- requested(section)
      result <- sieve_bind_result(result, state)
      if (!is.null(section)) {
        records <- isolate(status_records())
        records[[section]] <- result[c("revision", ".sampling", ".request", "error")]
        records[[section]]$when <- format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
        status_records(records)
      }
      result
    }
    # The time a section's last computation took, kept with its record: the
    # page's Update states the combined estimate from these.
    time_section <- function(section, started) {
      records <- isolate(status_records())
      if (!is.null(records[[section]])) {
        records[[section]]$seconds <- proc.time()[["elapsed"]] - started
        status_records(records)
      }
      invisible(NULL)
    }
    current <- function(result, what, action, section = NULL) {
      state$sampling_design
      if (!is.null(result$.sampling) && !identical(
        result$.sampling,
        sieve_sampling_spec(state)
      )) return(list(
        stale = TRUE,
        error = paste(
          "The sampling design changed. Press",
          action,
          "for the current design."
        )
      ))
      shown <- sieve_current_result(result, revision_now(), what, action)
      if (!is.null(shown) && !isTRUE(shown$stale) && !is.null(section) &&
          !identical(result$.request, requested(section))) {
        return(list(stale = TRUE, error = sprintf(
          "The settings for this %s changed. Press %s to compute it for the current choices. No analysis runs when these settings change.",
          what, action)))
      }
      shown
    }
    observe({
      records <- status_records()
      titles <- c(cv = "Calibration/validation split", inv = "Invariance ladder",
        diagnosis = "Invariance diagnosis", partial = "Partial invariance", anchor = "Anchor sensitivity",
        dmacs = "dMACS", ordinal = "Ordinal expected-score impact", alignment = "Alignment")
      actions <- c(cv = "Create split", inv = "Run invariance", diagnosis = "Diagnose noninvariance",
        partial = "Refit freeing selected", anchor = "Compare and freeze anchor decisions",
        dmacs = "Compute dMACS", ordinal = "Compute ordinal expected-score impact", alignment = "Run alignment")
      for (section in names(records)) {
        record <- records[[section]]
        shown <- current(record, titles[[section]], actions[[section]], section)
        phase <- if (isTRUE(shown$stale)) "stale" else if (!is.null(shown$error)) "failed" else "current"
        # A stale result (computed from other inputs) is brought up to date
        # by the page's one Update, unless it keeps its own button.
        action <- if (identical(phase, "stale") && !section_held(section)) "Update" else actions[[section]]
        sieve_page_publish(state, "finalize", session$ns(paste0("result_", section)), titles[[section]],
          list(state = phase, sentence = shown$error, key = record$.request, when = record$when), action)
      }
    })

    # ---- Eligible grouping variables --------------------------------------
    # The selector reads the roles: a column is offered when it has the
    # group role (identifiers, criteria, and background variables never are,
    # whatever their values) and a usable number of distinct non-missing
    # values (see sieve_group_levels()).
    eligible_groups <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed))
      raw <- state$raw
      state$roles
      state$items
      if (is.null(raw)) return(character(0))
      cand <- intersect(sieve_role_vars(state, "group"), names(raw))
      cand[vapply(cand, function(v) sieve_group_usable_levels(raw[[v]]), logical(1))]
    })

    observe({
      g <- eligible_groups()
      # The grouping variable in force is kept while it is offered, so an
      # edit elsewhere, and the page's Update, do not move the ladder to
      # another grouping.
      updateSelectInput(session, "group",
        choices = g,
        selected = sieve_keep_choice(isolate(input$group), g, if (length(g)) g[1] else NULL)
      )
    })

  }, envir = envir)
  invisible(NULL)
}

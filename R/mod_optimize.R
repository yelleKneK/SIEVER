# mod_optimize.R
# The Item Optimizer tab. It answers a question scale developers ask
# constantly: which item sets could I reach by dropping items, and what would
# each one look like? The engine (fct_optimize.R) lists every item set the
# researcher's content constraints allow, counts them before building any,
# and describes each one with reliability, fit, and simple structure
# statistics computed on the data being searched. The search proposes
# candidates; it does not choose one. Nothing is changed until the researcher
# selects a row and applies it to the ledger. The panel uses its settings
# exactly as typed: a value it cannot use is refused with the reason.
#
# A search result is kept with the revision it was computed from (the dataset,
# the item data, and the ledger; see sieve_revision()). Apply changes the
# ledger only while the current inputs are still that revision, or the one
# this tab's own last Apply produced from it; anything else is refused with
# the reason, and loading another dataset discards the result altogether.
# After an Apply the message above the table names the items it excluded and
# offers Undo, which puts the ledger back exactly as it was before that
# Apply, for as long as the ledger is still as that Apply left it.

#' Item optimizer UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS uiOutput numericInput checkboxInput actionButton helpText
#' @importFrom shiny hr p strong tags div em selectInput radioButtons
#' @importFrom bslib layout_sidebar sidebar card card_header accordion accordion_panel
#' @importFrom DT DTOutput
mod_optimize_ui <- function(id) {
  ns <- NS(id)
  metrics <- sieve_optimizer_metrics()
  need <- sieve_optimizer_requirements()
  limits <- sieve_optimizer_panel_limits()
  order_choices <- sieve_order_choices()
  tagList(uiOutput(ns("policy")), div(class = "sieve-optimizer-controls", layout_sidebar(
    # Natural heights: the explanation and the table stack and the panel
    # scrolls, instead of two cards competing for one screen.
    fillable = FALSE,
    sidebar = sidebar(
      width = 360,
      title = "Search settings",
      helpText(
        "List every item set you could reach by excluding items, within the content ",
        "constraints you set here, and describe each one. The search proposes ",
        "candidates; you decide. Set the constraints, then run the search."
      ),
      tags$div(class = "fw-semibold small", "Content constraints (your decisions)"),
      numericInput(ns("max_drop"), "Maximum items to exclude per construct",
        value = 1, min = 0, max = 4, step = 1
      ),
      numericInput(ns("min_keep"), "Minimum items to keep per construct",
        value = 3, min = limits$min_keep, max = 10, step = 1
      ),
      numericInput(ns("min_facet"), "Minimum items to keep per facet (optional)",
        value = NULL, min = 1, max = 10, step = 1
      ),
      shiny::selectizeInput(ns("must_keep"), "Items that must be kept (optional)",
        choices = NULL, multiple = TRUE,
        options = list(placeholder = "No item is protected")
      ),
      radioButtons(
        ns("undersized"), "If a construct already has fewer items than the minimum",
        choices = c(
          "Hold it intact and say so" = "hold",
          "Stop and name it" = "refuse"
        ),
        selected = "hold"
      ),
      helpText(sprintf(paste0(
        "These are content decisions: which items the instrument cannot lose and how ",
        "many items each construct, and each facet if you use facets, must keep so its ",
        "content is still covered. The current item set is always the first row, and ",
        "with a maximum of 0 it is the only row. No construct is ever removed: one ",
        "that is already below the minimum keeps all of its items in every candidate. ",
        "Leave the facet minimum empty for none. A value that cannot be used (a negative ",
        "or fractional number, or a minimum per construct below %d) is refused with the ",
        "reason; it is never replaced by another value on your behalf."
      ), limits$min_keep)),
      helpText(sprintf(paste0(
        "A content minimum is not an estimation requirement. Omega comes from a ",
        "one-factor model, which needs at least %d items per construct. With a minimum ",
        "below %d, candidates that leave a construct with two items are still listed, ",
        "with omega marked unavailable and the reason given."
      ), need$omega, need$omega)),
      hr(),
      checkboxInput(ns("ss"), "Also describe simple structure (slower)", value = TRUE),
      numericInput(ns("cap"), "Evaluation cap (most candidates to examine)",
        value = 300, min = 1, max = limits$cap, step = 50
      ),
      uiOutput(ns("count")),
      actionButton(ns("run"), "Run search", class = "btn-primary w-100"),
      hr(),
      helpText(sprintf(paste0(
        "The number of candidates is counted from the constraints without building ",
        "them, and it grows fast. The count is exact, except that an astronomically ",
        "large one is stated approximately. A search above the cap is refused before ",
        "anything is built or fit; a search within the cap examines every candidate. ",
        "There is no partial search. The cap can be at most %s, because every candidate ",
        "is fit while the session waits."
      ), format(limits$cap, big.mark = ",")))
    ),
    sieve_optional_tab_note("a bounded search over the item sets reachable by excluding items within the content constraints you set, each listed with its statistics for you to compare"),
    # Results first: the one status line (with the page's one action), the
    # candidate table with the line that says what was searched, the notes on
    # the table under it, and what the search does and does not do below that.
    uiOutput(ns("gate")),
    card(
      card_header("Candidate item sets"),
      uiOutput(ns("run_head")),
      selectInput(ns("order_by"), "Order candidates by",
        choices = order_choices,
        selected = "search", width = "360px"
      ),
      DTOutput(ns("results"), fill = FALSE),
      uiOutput(ns("detail")),
      uiOutput(ns("search_code")),
      tags$hr(),
      div(
        class = "d-flex align-items-center gap-2",
        actionButton(ns("apply"), "Apply selected item-set to the ledger",
          class = "btn-success"
        ),
        tags$small(
          class = "text-muted",
          "Select one row above, then apply it: the items it leaves out become Excluded ",
          "across the whole pipeline (reversible on the Items tab). ",
          "The search never applies a candidate on its own."
        )
      ),
      uiOutput(ns("run_msg"))
    ),
    card(
      card_header("What this search does, and what it does not"),
      sieve_more_below(
        tagList(
          tags$b("It proposes; you decide."),
          " Every item set that satisfies your content constraints is listed with the same statistics. ",
          "The search does not pick one, and nothing is applied to your items until you select a row and press Apply. ",
          tags$b("The statistics are descriptive and selection dependent."),
          " Each one is computed on the same data used to choose among the candidates. The best of many ",
          "candidates is best partly by chance, so its reliability and fit are optimistic, more so with more ",
          "candidates and a smaller sample (MacCallum, Roznowski, & Necowitz, 1992). Read differences between ",
          "candidates as descriptions of this sample, not as tests."
        ),
        tags$ul(
          class = "small mb-2",
          tags$li(
            tags$b("A chosen item set needs independent data."),
            " Once you settle on an item set, treat it as fixed and estimate its reliability and fit in data that played no part in the choice: a holdout sample (the CV and Invariance tab makes a calibration and validation split) or a new sample (Cudeck & Browne, 1983). Shortening a scale also changes what it covers, so judge content first and statistics second (Smith, McCarthy, & Anderson, 2000)."
          ),
          tags$li(
            tags$b("Ordering is your choice."),
            " The table opens in search order, which favors no statistic. You may order by any one statistic; the current item set stays in the first row as the reference, and a candidate for which that statistic is unavailable is listed below the ranked ones, marked not ranked, with the reason. It is never hidden and never placed as if it were better or worse."
          )
        ),
        tags$p(class = "small fw-semibold mb-1", "What each column is, and the rows it was computed on"),
        tags$ul(
          class = "small mb-2",
          lapply(seq_len(nrow(metrics)), function(i) {
            tags$li(
              tags$b(paste0(metrics$label[i], ":")),
              sprintf(
                " %s. Computed on: %s. %s", metrics$target[i], metrics$sample[i],
                if (metrics$better[i] == "higher") {
                  "Higher is better."
                } else {
                  "Lower is better."
                }
              )
            )
          })
        ),
        tags$ul(
          class = "small mb-0",
          tags$li(
            tags$b("These are not always the Reliability and EFA tab values."),
            " Omega and the simple structure summaries here treat the item codes as continuous and use complete cases, whatever the confirmed item types are. For ordered or binary items, or with incomplete data, the Reliability tab computes Robust omega (observed denominator) differently (from the unrestricted polychoric correlations, or with a full information estimate) and gives its confidence interval on request, and the EFA tab analyzes polychoric or tetrachoric correlations. Use those tabs for the item set you choose."
          ),
          tags$li(
            tags$b("Why omega can be unavailable."),
            sprintf(
              " Robust omega (observed denominator) takes its numerator from a one-factor model of a construct's items (McDonald, 1999; Kelley & Pornprasertmanit, 2016) and its denominator from the variance of the construct's unit-weighted composite. That model needs at least %d items; with two, the loadings are not identified. When a candidate leaves any construct with fewer, min omega and mean omega are not reported for it, because a worst case cannot be stated while one construct's reliability is unknown. The omega of each construct that can be estimated is still shown.",
              need$omega
            )
          ),
          tags$li(
            tags$b("When omega is shown with an asterisk."),
            " The one-factor model of a construct's kept items can converge to an improper solution, with a negative residual variance estimate (a Heywood case), and the omega computed from it can even exceed 1. Such a value is not a usable reliability estimate, however high it looks. It is shown as estimated and marked, the status column names the construct, and the item set is not ranked on omega. An improper solution usually means that one factor does not describe those items or that the sample is small for the model (Chen, Bollen, Paxton, Curran, & Kirby, 2001)."
          ),
          tags$li(
            tags$b("Fit indices describe each candidate's own model."),
            " Every candidate is a different model of a different set of items, so CFI, RMSEA, and SRMR are not a test between candidates. A fit is not ranked when the solution is improper, not identified, or just identified, when a construct is left out of the model, or when a different estimator was used than for the current item set."
          ),
          tags$li(
            tags$b("AIC and BIC are not shown."),
            " An information criterion compares models of the same data. Candidates keep different items, so their likelihoods refer to different observed variables: the AIC of a smaller item set is usually lower only because fewer responses are being modeled, not because its model is better (Burnham & Anderson, 2002)."
          ),
          tags$li(
            tags$b("Simple structure."),
            " Both come from one exploratory factor analysis of each candidate's kept items by DMAR::fa_exploratory() (maximum likelihood on the complete rows, direct oblimin, as many factors as constructs). Item complexity is Hofmann's (1978) index, 1 for an item that loads on one factor; the hyperplane proportion is the share of near zero loadings (Thurstone, 1947). The oblique rotation starts from 30 random orientations, so the search runs it on a recorded seed and can be replayed. DMAR refuses an improper solution (a residual variance below zero) instead of holding it at a bound, and the candidate's simple structure is then unavailable, with DMAR's reason. DMAR is asked for the point estimates alone, about 0.3 seconds a candidate on the nine bundled tests and 0.7 on the 24."
          )
        ),
        label = "More on the search: each statistic, the rows it was computed on, and what to do with a chosen item set"
      )
    )
  )))
}

#' Item optimizer server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent req renderUI
#' @importFrom shiny withProgress incProgress isolate div strong showNotification
#' @importFrom shiny observe updateSelectizeInput tagList
#' @importFrom DT renderDT datatable formatStyle
mod_optimize_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    allow_optimization <- function() {
      allowed <- sieve_capture(sieve_require_optimization(state))
      if (!allowed$ok) sieve_notify(allowed$error, type = "warning")
      isTRUE(allowed$ok)
    }
    # Under the theoretical construct basis the search is unavailable and
    # the tab shows only this statement (the controls are hidden). Under
    # the empirical basis the statement of what the candidates are is read
    # with the notes under the table, so nothing stands above the result
    # but its status line.
    output$policy <- renderUI({
      policy <- sieve_workflow_policy(state)
      if (isTRUE(policy$optimization)) {
        return(NULL)
      }
      div(class = "alert alert-light border py-2", policy$message)
    })

    # The page's one status line: the pipeline gate; or the search not run
    # yet (what to press); or a search that could not be run (why); or a
    # search from before an edit or a change of the settings (what changed,
    # with the page's one Update).
    output$gate <- renderUI({
      g <- sieve_gate(state)
      if (!is.null(g)) {
        return(g)
      }

      req(sieve_workflow_policy(state)$optimization)
      r <- res()
      if (is.null(r)) {
        return(div(
          class = "alert alert-info py-2 mb-2",
          strong("No search has been run yet. "),
          "Set the constraints in the sidebar, then press Run search there. The candidates are described ",
          "on the included items; nothing is changed until you apply a row."
        ))
      }
      # A search that could not be run says why under the table, where the
      # candidates would be.
      page_update$line() %||% page_update$note()
    })

    # The must-keep choices follow the included items. A protected item that
    # is later excluded elsewhere leaves the list with it.
    observe({
      req(state$meta)
      pool <- state$meta[state$meta$status == "active", , drop = FALSE]
      label <- as.character(pool$label %||% pool$item)
      label[is.na(label) | !nzchar(label)] <- pool$item[is.na(label) | !nzchar(label)]
      shown <- ifelse(label == pool$item, pool$item, sprintf("%s (%s)", label, pool$item))
      updateSelectizeInput(
        session, "must_keep",
        choices = stats::setNames(pool$item, shown),
        selected = intersect(isolate(input$must_keep), pool$item)
      )
    })

    # The researcher's settings, read in one place for the live count and
    # for the search, exactly as typed: a value that cannot be used is
    # refused with the reason (sieve_optimizer_panel_settings()), never
    # replaced by another one, so the count shown is the count searched.
    content <- reactive({
      sieve_optimizer_panel_settings(
        max_drop = input$max_drop, min_keep = input$min_keep, min_facet = input$min_facet,
        must_keep = input$must_keep, undersized = input$undersized, cap = input$cap
      )
    })
    search_request <- reactive({
      constraints <- content()$constraints
      if (!is.null(constraints)) constraints$must_keep <- sort(constraints$must_keep)
      state$sampling_design
      list(constraints = constraints, simple_structure = isTRUE(input$ss),
        estimator = sieve_estimator_effective("study", state),
        sampling = sieve_page_revision(state)$sampling,
        rows = sieve_hash(state$case_index %||% if (!is.null(state$items)) seq_len(nrow(state$items)) else NULL),
        workflow = sieve_workflow_token(state))
    })

    # Live candidate count, as one paragraph: how many candidate item sets
    # the settings define, what is held and why, the cap, and what Run
    # search would do (with a rough time from the last search, if there
    # was one). The count is algebraic (no subset is built) and its cost
    # does not grow with the maximum to drop, so it is safe for any pool
    # and any typed value.
    output$count <- renderUI({
      req(sieve_workflow_policy(state)$optimization)
      req(state$meta, isTRUE(sieve_confirmation_status(state)$confirmed))
      cn <- content()
      if (is.null(cn$constraints)) {
        return(div(class = "alert alert-warning py-1 small", cn$error))
      }
      plan <- sieve_optimizer_plan(state$meta, cn$constraints)
      if (!is.null(plan$error)) {
        return(div(
          class = "alert alert-warning py-1 small",
          "These settings cannot be searched. ", plan$error
        ))
      }
      if (!is.null(cn$error)) {
        return(div(class = "alert alert-warning py-1 small", cn$error))
      }
      said <- sieve_count_paragraph(plan, cn,
        simple_structure = isTRUE(input$ss),
        last = res()
      )
      div(
        class = if (said$warn) "alert alert-warning py-1 small" else "alert alert-info py-1 small",
        said$text
      )
    })

    res <- reactiveVal(NULL)
    # The revision this tab's last Apply produced from the kept result (kept
    # apart from the result so applying a candidate does not redraw the table).
    last_applied <- reactiveVal(NULL)
    # What the last Apply did (the row, the items it excluded and brought
    # back, the ledger's statuses before it, and the revision it produced),
    # so the message above the table can name the items and offer Undo.
    applied <- reactiveVal(NULL)

    # A result belongs to the dataset it was computed from: loading another
    # dataset (even one with the same item names and values) discards it.
    observeEvent(state$dataset_id,
      {
        r <- res()
        if (!is.null(r) && !identical(r$revision$dataset_id, state$dataset_id)) {
          res(NULL)
          last_applied(NULL)
          applied(NULL)
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # Why the kept result does not apply to the current inputs, or NULL when
    # it does. The inputs are read here so the message follows every change.
    stale <- reactive({
      r <- res()
      if (is.null(r) || !is.null(r$error)) {
        return(NULL)
      }
      state$dataset_id
      state$items
      state$meta
      why <- sieve_result_stale(
        list(revision = r$revision, applied_revision = last_applied()),
        sieve_revision(state)
      )
      if (!is.null(why)) return(why)
      if (!identical(r$request, search_request())) {
        return("the content constraints, simple-structure request, estimator, respondent rows, sampling design, or construct workflow changed")
      }
      NULL
    })
    observe({
      r <- res()
      if (is.null(r)) return()
      why <- stale()
      phase <- if (!is.null(why)) "stale" else if (!is.null(r$error)) "failed" else "current"
      # Candidates from before are brought up to date by the page's one
      # Update.
      sieve_page_publish(state, "optimize", session$ns("candidate_search"), "Optimizer candidates",
        list(state = phase, sentence = why %||% r$error, key = r$request, when = r$recorded_at),
        if (identical(phase, "stale")) "Update" else "Run search")
    })

    # Run search, as a function: the button, and the page's Update when
    # the candidates on screen are from before, run the same search.
    run_search <- function() {
      if (!allow_optimization()) return(invisible(NULL))
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      sieve_ensure_dataset(state)
      workflow <- isolate(sieve_workflow_token(state))
      request <- search_request()
      ss <- isTRUE(input$ss)

      # The settings are used exactly as typed. A value the panel cannot use
      # is refused with the reason, and the engine counts the candidates
      # algebraically and, when the search cannot be run (infeasible
      # constraints, or more candidates than the cap), refuses with the
      # reason before building anything. The count here only sizes the
      # progress bar.
      cn <- content()
      total <- if (is.null(cn$constraints)) {
        0
      } else {
        sieve_optimizer_plan(state$meta, cn$constraints)$total
      }

      out <- withProgress(
        message = "Fitting candidate item-sets",
        value = 0,
        {
          step <- 1 / max(total, 1)
          sieve_optimizer_panel_search(
            cn, state$items, state$meta,
            simple_structure = ss,
            progress = function(i) incProgress(step), project = state
          )
        }
      )
      # Keep, with the result, what it was computed from: the revision of the
      # inputs and the included pool the candidates are subsets of.
      if (!sieve_workflow_policy(state)$optimization ||
        !identical(request, isolate(search_request()))) {
        sieve_notify("The workflow or analysis settings changed during the search. No candidates can be applied.", type = "warning")
        return(invisible(NULL))
      }
      out$workflow <- workflow
      out$request <- request
      out$recorded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
      out$revision <- sieve_revision(state)
      out$pool <- state$meta$item[state$meta$status == "active"]
      last_applied(NULL)
      applied(NULL)

      if (is.null(out$error)) {
        sieve_log(
          state,
          sprintf(
            "Optimizer: searched %d item-set%s, all examined (%s)",
            out$evaluated, if (out$evaluated == 1) "" else "s",
            sieve_constraints_text(out$constraints)
          ),
          c(
            "# Item set search: every item set that satisfies the content constraints was",
            "# described on these data (omega, CFI/RMSEA/SRMR, simple structure). The",
            "# statistics are descriptive; the search chose and applied nothing.",
            sprintf("# Constraints: %s.", sieve_constraints_text(out$constraints)),
            sprintf(
              "# Held intact in every item set: %s",
              sieve_held_text(out$constructs, out$facets)
            ),
            sprintf("# %d item sets examined, none skipped.", out$evaluated),
            if (!is.null(out$rng)) {
              sprintf("# Seed of the simple structure rotation: %d.", out$rng$seed)
            },
            # The exported call the search made, with its settings and the
            # seed it used, so the recorded step reproduces the candidates.
            sieve_optimizer_search_code(cn, simple_structure = ss, seed = out$rng$seed)
          )
        )
        # The record a disclosure under the candidates reads: the step just
        # written, with the search's settings.
        out$code_view <- sieve_code_snapshot(state, context = c(
          sprintf("Constraints: %s.", sieve_constraints_text(out$constraints)),
          sprintf("Held intact in every item set: %s.", sieve_held_text(out$constructs, out$facets)),
          sprintf("%d item sets examined; the statistics are descriptive, and the search applied nothing.", out$evaluated)
        ))
      }
      res(out)
      invisible(out)
    }
    observeEvent(input$run, run_search())

    # Above the table, the one line that says what was searched. The search not
    # run, or from before, is said by the status line above the card; everything
    # else about the table is under it, beside the Apply button it concerns.
    output$run_head <- renderUI({
      r <- res()
      if (is.null(r) || !is.null(r$error) || is.null(r$table)) {
        return(NULL)
      }
      n <- nrow(r$table)
      div(
        class = "small mb-1",
        sprintf(
          paste0(
            "Examined all %s item set%s that satisfy your constraints (%s); none was ",
            "skipped. Row 1 is your current item set, the reference."
          ),
          format(n, big.mark = ","), if (n == 1) "" else "s",
          sieve_constraints_text(r$constraints)
        )
      )
    })

    # Under the table and the Apply button: why a search was not run; that
    # candidates from before cannot be applied; what the last Apply did,
    # with Undo; and the notes on what the table holds (constructs held
    # intact, why omega or a fit is not ranked, the family of the fit
    # indices), with the statement of what the candidates are.
    output$run_msg <- renderUI({
      r <- res()
      if (is.null(r)) {
        return(NULL)
      }
      if (!is.null(r$error)) {
        return(div(class = "alert alert-danger mt-2", r$error))
      }
      tab <- r$table
      cn <- r$constraints
      need <- sieve_optimizer_requirements()
      n <- nrow(tab)
      held <- sieve_held_text(r$constructs, r$facets)
      # Why omega cannot be ordered, by cause: a construct with too few
      # items (expected with a low minimum), a one-factor model that gave no
      # estimate, or an estimate from an improper solution.
      no_omega <- sum(tab$reliability_code == "too_few_items")
      failed_omega <- sum(tab$reliability_code %in% c("no_estimate", "unavailable"))
      improper_omega <- sum(tab$reliability_code == "improper")
      no_fit <- sum(!tab$fit_ok)
      why <- stale()
      done <- applied()
      # Whether the ledger is still as the last Apply left it. Only then
      # can that Apply be undone from here.
      undoable <- !is.null(done) && is.null(sieve_stale_reason(done$revision, sieve_revision(state)))
      tagList(
        if (!is.null(why)) {
          div(
            class = "alert alert-warning py-2 small mt-2",
            strong("These candidates no longer match the current data. "),
            sprintf(paste(
              "They were computed from different inputs (%s), so they are shown",
              "for reference only and cannot be applied. Update, in the line above",
              "the table, searches the current item pool."
            ), why)
          )
        } else NULL,
        if (!is.null(done)) {
          div(
            class = if (undoable) "alert alert-success py-2 small mt-2" else "alert alert-secondary py-2 small mt-2",
            strong(sprintf("Applied item set %d. ", done$id)),
            sieve_applied_text(done),
            if (undoable) {
              tagList(
                " ",
                actionButton(session$ns("undo"), "Undo this Apply",
                  class = "btn-outline-secondary btn-sm py-0"
                ),
                " Undo puts the ledger back exactly as it was before this Apply."
              )
            } else {
              paste0(
                " The item ledger has changed since, so this Apply cannot be undone ",
                "from here; include or exclude items on the Items tab."
              )
            }
          )
        } else NULL,
        if (length(held)) {
          div(
            class = "alert alert-warning py-1 small mt-2 mb-1",
            paste(held, collapse = " "),
            if (length(held) == 1L) {
              " All of its items are in every candidate."
            } else {
              " All of their items are in every candidate."
            }
          )
        },
        if (no_omega) {
          div(
            class = "alert alert-info py-1 small mt-2 mb-1",
            sprintf(
              paste0(
                "Omega (min and mean) is unavailable for %d of %d item sets. This is expected, ",
                "not an error: omega comes from a one-factor model that needs at least %d items, ",
                "and those item sets leave a construct with fewer%s. They are listed with the ",
                "reason and are not ranked on omega. The table is not reordered to another ",
                "statistic on your behalf."
              ),
              no_omega, n, need$omega,
              if (min(cn$min_keep) < need$omega) {
                sprintf(" (your minimum per construct is %d)", min(cn$min_keep))
              } else {
                ""
              }
            )
          )
        },
        if (failed_omega) {
          div(
            class = "alert alert-warning py-1 small mt-2 mb-1",
            sprintf(paste0(
              "Omega could not be estimated for %d of %d item sets: the one-factor model of ",
              "a construct gave no estimate. They are listed with the reason and are not ",
              "ranked on omega."
            ), failed_omega, n)
          )
        },
        if (improper_omega) {
          div(
            class = "alert alert-warning py-1 small mt-2 mb-1",
            sprintf(
              paste0(
                "The omega of %d of %d item sets comes from an improper one-factor solution (a ",
                "negative residual variance estimate, a Heywood case) or lies outside 0 to 1. ",
                "Such a value is not a usable reliability estimate, however high it looks: it ",
                "is shown with an asterisk, it is not ranked, and the status column names the ",
                "construct. It usually means that one factor does not describe that ",
                "construct's kept items, or that the sample is small for the model."
              ),
              improper_omega, n
            )
          )
        },
        if (!isTRUE(r$simple_structure)) {
          div(
            class = "small text-muted mt-1",
            "Simple structure was not requested for this search, so complexity and the ",
            "hyperplane proportion were not computed and the table cannot be ordered by them."
          )
        },
        if (no_fit) {
          div(
            class = "small text-muted mt-1",
            sprintf(
              paste0(
                "The fit indices of %d of %d item sets are not ranked (improper, not identified, ",
                "or otherwise not comparable); the status column gives the reason for each."
              ),
              no_fit, n
            )
          )
        },
        # Which family of fit indices the CFI and RMSEA columns are in, and why.
        if (!is.null(r$fit_family_reason) && !is.na(r$fit_family_reason)) {
          div(class = "small text-muted mt-1", r$fit_family_reason)
        },
        div(
          class = "small text-muted mt-1 mb-2",
          "These statistics describe the data used to choose, so they are optimistic for ",
          "whichever item set looks best. Evaluate the item set you choose on independent data. ",
          sieve_workflow_policy(state)$message
        )
      )
    })

    # ---- One Update for the page --------------------------------------------
    # The candidates are the tab's one result. When they are from before
    # (the data, the item pool, or the search settings changed), Update
    # runs the search again with the settings now in the sidebar, as Run
    # search does; its estimate is scaled from the last search's rate.
    # ---- Show code at the point of use --------------------------------------
    # The record kept with the search (run_search()), current while the
    # candidates still apply to the ledger (the search's own revision, or
    # the one an Apply of this search produced), from before otherwise.
    # Reading the panel evaluates nothing.
    sieve_code_navigation(input, session, state)
    output$search_code <- renderUI({
      r <- res()
      status <- if (is.null(r)) {
        list(state = "not_run")
      } else if (!is.null(r$error)) {
        list(state = "failed", sentence = r$error)
      } else if (!is.null(why <- stale())) {
        list(state = "stale", sentence = sprintf("What changed: %s.", why))
      } else {
        list(state = "current")
      }
      sieve_code_view(r$code_view, status, ns = session$ns,
        title = "Show code: the item set search",
        readout = c(
          "# Each candidate is described by the statistics of its own fits on these data;",
          "# the table orders them as you choose and applies nothing."
        ))
    })

    page_update <- sieve_update_wire(input, session, function() {
      r <- res()
      if (is.null(r) || !is.null(r$error)) {
        return(list())
      }
      why <- stale()
      seconds <- NA_real_
      if (!is.null(why)) {
        cn <- content()
        plan <- if (!is.null(cn$constraints)) sieve_capture(sieve_optimizer_plan(state$meta, cn$constraints))$value
        if (!is.null(plan) && is.null(plan$error) && is.numeric(r$elapsed) && isTRUE(r$evaluated >= 1)) {
          seconds <- r$elapsed / r$evaluated * plan$total
        }
      }
      list(search = sieve_update_entry(
        "Optimizer candidates", if (is.null(why)) "current" else "stale",
        sentence = why, when = sprintf("searched %s", r$recorded_at %||% "earlier"),
        seconds = seconds, run = run_search
      ))
    })

    output$results <- renderDT(
      {
        req(sieve_workflow_policy(state)$optimization)
        r <- res()
        req(r, is.null(r$error), r$table)
        sieve_candidate_widget(r, by = sieve_order_by(r, input$order_by))
      },
      server = FALSE
    )

    # The ordering control offers the statistics the search computed, or,
    # before a search, the ones the checkbox asks for: with simple structure
    # off there is no complexity or hyperplane proportion to order by, so
    # those two choices are withdrawn (sieve_order_structure()).
    observe({
      offered <- sieve_order_choices(structure = sieve_order_structure(res(), input$ss))
      current <- isolate(input$order_by)
      shiny::updateSelectInput(
        session, "order_by",
        choices = offered,
        selected = if (length(current) == 1L && current %in% offered) current else "search"
      )
    })

    # The selected row in full: what it keeps, what each family of statistics
    # is and why any of it is unavailable, and what the engines signaled.
    output$detail <- renderUI({
      req(sieve_workflow_policy(state)$optimization)
      r <- res()
      req(r, is.null(r$error), r$table)
      row <- input$results_rows_selected
      if (is.null(row) || !length(row) || row > nrow(r$table)) {
        return(div(
          class = "text-muted small mt-2",
          "Select a row to see its items, statistics, and any reasons in full."
        ))
      }
      sieve_candidate_detail(r, row, by = sieve_order_by(r, input$order_by))
    })

    # Apply a chosen candidate: park the items it drops, activate the rest.
    # The candidate is applied only to the inputs it was computed from. A
    # result left over from another dataset, or from a ledger or item data
    # that have changed since the search, never changes the ledger.
    observeEvent(input$apply, {
      if (!allow_optimization()) return(invisible(NULL))
      r <- res()
      req(r, is.null(r$error), r$kept)
      if (!identical(r$workflow, isolate(sieve_workflow_token(state)))) {
        sieve_notify("The construct basis changed after this search. Run a new search before applying candidates.",
          type = "error"
        )
        return(invisible(NULL))
      }
      why <- stale()
      if (!is.null(why)) {
        sieve_notify(
          sprintf(paste(
            "Nothing was applied: these candidates were computed from different",
            "inputs (%s). Press Run search to search the current item pool,",
            "then apply a candidate from the new results."
          ), why),
          type = "error"
        )
        return(invisible(NULL))
      }
      row <- input$results_rows_selected
      if (is.null(row) || !length(row)) {
        sieve_notify("Select a candidate row first.", type = "warning")
        return(invisible(NULL))
      }
      # DT returns the selection index in the ORIGINAL data order (not the
      # client-side sorted order), and r$kept is aligned to r$table row order,
      # so the kept-item vector is simply r$kept[[row]].
      kept <- if (length(row) == 1L && row >= 1L && row <= length(r$kept)) r$kept[[row]] else NULL
      if (is.null(kept) || !length(kept) || !all(kept %in% state$meta$item)) {
        sieve_notify("Could not resolve the selected item-set.", type = "error")
        return(invisible(NULL))
      }
      # The candidates are subsets of the pool the search ran on, so the
      # choice is checked against that pool and the constraints the search
      # was run with (the same check the engine applied before returning
      # it), and then applied to that pool: its kept items are included and
      # the rest of that pool is excluded, whichever candidate was applied
      # before.
      pool_meta <- state$meta
      pool_meta$status <- ifelse(pool_meta$item %in% r$pool, "active", "parked")
      checked <- sieve_check_candidate(kept, pool_meta, r$constraints)
      if (!checked$ok) {
        sieve_notify(
          paste(c(
            "Nothing was applied: this item set does not satisfy the constraints",
            "the search was run with.", checked$violations
          ), collapse = " "),
          type = "error"
        )
        return(invisible(NULL))
      }
      active_items <- state$meta$item[state$meta$status == "active"]
      dropped <- setdiff(active_items, kept)
      returned <- setdiff(kept, active_items)
      before <- data.frame(
        item = state$meta$item, status = state$meta$status,
        exclude_reason = state$meta$exclude_reason %||% rep(NA_character_, nrow(state$meta)),
        stringsAsFactors = FALSE
      )
      sieve_set_item_status(state, c(dropped, returned),
        c(rep("parked", length(dropped)), rep("active", length(returned))),
        reason = "Optimizer item-set selection", label = "Optimizer: applied item-set")
      # The ledger this Apply produced is the one state, besides the search's
      # own, to which another candidate of the same search may be applied.
      last_applied(sieve_revision(state))
      applied(list(
        id = row, kept = kept, dropped = dropped, returned = returned,
        excluded = setdiff(r$pool, kept), before = before,
        revision = sieve_revision(state)
      ))

      sieve_notify(
        sprintf(
          "Applied item set %d: %d item%s kept, %d excluded%s. Undo is above the table.",
          row, length(kept), if (length(kept) == 1L) "" else "s",
          length(setdiff(r$pool, kept)),
          if (length(dropped)) sprintf(" (%s)", paste(dropped, collapse = ", ")) else ""
        ),
        type = "message"
      )
    })

    # Undo the last Apply: put every item's status back as it was before
    # that Apply, exactly, and only while the ledger is still as that Apply
    # left it. A ledger changed since (an item excluded elsewhere, another
    # dataset loaded) is never written over.
    observeEvent(input$undo, {
      if (!allow_optimization()) return(invisible(NULL))
      done <- applied()
      req(done)
      why <- sieve_stale_reason(done$revision, sieve_revision(state))
      if (!is.null(why)) {
        sieve_notify(
          sprintf(paste(
            "Nothing was undone: the item ledger is no longer as that Apply",
            "left it (%s). Include or exclude items on the Items tab."
          ), why),
          type = "error"
        )
        return(invisible(NULL))
      }
      sieve_set_item_status(state, state$meta$item,
        done$before$status[match(state$meta$item, done$before$item)],
        reasons = done$before$exclude_reason[match(state$meta$item, done$before$item)],
        reason = "Undo optimizer item-set selection", label = "Optimizer: undid the applied item-set")
      # The restored ledger is the one the search ran on, or the one an
      # earlier Apply of the same search produced: either way the result
      # still applies to it.
      last_applied(sieve_revision(state))
      applied(NULL)

      sieve_notify(
        sprintf("Undone: the item ledger is as it was before item set %d was applied.", done$id),
        type = "message"
      )
    })
  })
}

#' The limits the optimizer panel places on its own fields
#'
#' Kept in one place so the fields and the server agree. These are limits of
#' the panel, not of the search function:
#'
#' * `min_keep`: the smallest minimum per construct the panel accepts. It is
#'   the number of indicators a factor needs
#'   (`sieve_optimizer_requirements()`): a construct left with one item
#'   cannot define a factor, so no candidate's confirmatory model would
#'   describe the whole item set.
#' * `cap`: the largest evaluation cap. Every candidate is fit while the
#'   session waits, so the panel does not start a search larger than this
#'   however the field is filled in. This is the session's capacity limit;
#'   it says nothing about which item set is better.
#'
#' The upper ends of the other number fields are conveniences of the spin
#' buttons: a larger typed value is used as typed.
#'
#' @return A named list of integers.
#' @author Ken Kelley
#' @noRd
sieve_optimizer_panel_limits <- function() {
  list(min_keep = sieve_optimizer_requirements()$cfa_factor, cap = 3000L)
}

#' Read the optimizer panel's settings exactly as typed
#'
#' Validates the fields of the panel without changing any of them. A value
#' that cannot be used (negative, fractional, cleared, below the panel's
#' minimum per construct, above the panel's cap) produces the reason in
#' `error`; it is never replaced by a nearby usable value, because the
#' researcher would then be shown a search of settings that were not asked
#' for. An input that does not exist yet (`NULL`, before the page has drawn
#' the field) takes the field's initial value.
#'
#' @param max_drop,min_keep,min_facet,must_keep,undersized,cap The values of
#'   the panel's inputs.
#' @return A list with `error` (`NULL`, or the reason the settings cannot be
#'   searched), `constraints` (the validated content constraints from
#'   `sieve_optimizer_constraints()`, or `NULL` when they are invalid),
#'   `max_drop`, `min_keep`, and `min_facet` as typed (`min_facet` is `NULL`
#'   when the field is empty), `must_keep`, `undersized`, and `cap`.
#' @author Ken Kelley
#' @noRd
sieve_optimizer_panel_settings <- function(max_drop = NULL, min_keep = NULL, min_facet = NULL,
                                           must_keep = NULL, undersized = NULL, cap = NULL) {
  limits <- sieve_optimizer_panel_limits()
  max_drop <- max_drop %||% 1
  min_keep <- min_keep %||% 3
  cap <- cap %||% 300
  undersized <- undersized %||% "hold"
  # An empty facet minimum means none.
  min_facet <- if (is.finite(sieve_finite_or(min_facet, NA_real_))) min_facet else NULL
  one_number <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  typed <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  entered <- function(x) {
    if (one_number(x)) sprintf(" (you entered %s)", typed(x)) else " (the field is empty)"
  }
  out <- list(
    error = NULL, constraints = NULL, max_drop = max_drop, min_keep = min_keep,
    min_facet = min_facet, must_keep = must_keep, undersized = undersized, cap = cap
  )
  # The panel's own floor for the minimum per construct, stated with its
  # reason before the general checks.
  if (!one_number(min_keep) || min_keep != floor(min_keep) || min_keep < limits$min_keep) {
    out$error <- sprintf(
      paste0(
        "The minimum number of items to keep per construct must be a whole number, %d or more, ",
        "in this panel%s.%s"
      ), limits$min_keep, entered(min_keep),
      if (one_number(min_keep) && min_keep < limits$min_keep) {
        paste0(
          " A construct left with one item cannot define a factor, so the confirmatory ",
          "model of such a candidate would not describe the whole item set."
        )
      } else {
        ""
      }
    )
    return(out)
  }
  con <- sieve_capture(sieve_optimizer_constraints(
    max_drop = max_drop, min_keep = min_keep, min_facet = min_facet,
    must_keep = must_keep, undersized = undersized
  ))
  if (!con$ok) {
    out$error <- con$error
    return(out)
  }
  out$constraints <- con$value
  if (!one_number(cap) || cap < 1 || cap != floor(cap)) {
    out$error <- sprintf("The evaluation cap must be a whole number, one or more%s.", entered(cap))
  } else if (cap > limits$cap) {
    out$error <- sprintf(
      paste0(
        "The evaluation cap can be at most %s in this panel%s. Every candidate is fit while ",
        "the session waits, so a larger search is not started from here. Tighten the ",
        "constraints so that fewer item sets qualify."
      ),
      typed(limits$cap), entered(cap)
    )
  }
  out
}

#' Run the search the panel's settings describe, or say why not
#'
#' @param settings Settings from `sieve_optimizer_panel_settings()`.
#' @param items_df,meta,simple_structure,progress Passed to
#'   `sieve_optimize_items()`.
#' @param project Optional project context, whose workflow policy is enforced.
#' @return The result of `sieve_optimize_items()`, or, when the settings
#'   cannot be searched, a refusal of the same shape (`error`, `total` 0,
#'   `evaluated` 0) carrying the reason. Nothing is built or fit for a
#'   refusal.
#' @author Ken Kelley
#' @noRd
sieve_optimizer_panel_search <- function(settings, items_df, meta, simple_structure = TRUE,
                                         progress = NULL, project = NULL) {
  if (!is.null(project)) sieve_require_optimization(project)
  if (!is.null(settings$error)) {
    return(list(
      error = paste("The search was not run.", settings$error), total = 0,
      evaluated = 0L, complete = FALSE, constraints = settings$constraints
    ))
  }
  sieve_optimize_items(
    items_df, meta,
    max_drop = settings$max_drop, min_keep = settings$min_keep, cap = settings$cap,
    simple_structure = simple_structure, progress = progress,
    min_facet = settings$min_facet, must_keep = settings$must_keep,
    undersized = settings$undersized, project = project
  )
}

#' The R of an Optimizer search, as the page ran it
#'
#' The call of the exported [sieve_optimize_items()] with the panel's
#' settings and the seed of the simple structure rotation the search used,
#' on the session script's `items` and `meta`, and the table it returns.
#'
#' @param settings The panel's settings (`sieve_optimizer_panel_settings()`).
#' @param simple_structure Whether the simple structure statistics were
#'   computed.
#' @param seed The seed the search used (its `rng$seed`), or `NULL`.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_optimizer_search_code <- function(settings, simple_structure = TRUE, seed = NULL) {
  literal <- function(x) {
    if (is.null(x)) "NULL" else paste(utils::capture.output(dput(unname(x))), collapse = "")
  }
  c(
    "optimizer_search <- SIEVER::sieve_optimize_items(items, meta,",
    sprintf(
      "  max_drop = %s, min_keep = %s, cap = %s, simple_structure = %s,",
      literal(settings$max_drop), literal(settings$min_keep), literal(settings$cap),
      if (isTRUE(simple_structure)) "TRUE" else "FALSE"
    ),
    sprintf(
      "  min_facet = %s, must_keep = %s, undersized = %s, seed = %s)",
      literal(settings$min_facet), literal(settings$must_keep),
      literal(settings$undersized %||% "hold"),
      if (length(seed) == 1L && is.finite(seed)) sprintf("%dL", as.integer(seed)) else "NULL"
    ),
    "optimizer_search$table"
  )
}

#' The choices of the ordering control
#'
#' Search order, which favors no statistic, and then each statistic in
#' `sieve_optimizer_metrics()` with the direction it is ordered in.
#'
#' @param structure Whether the simple structure statistics are offered.
#'   They are withdrawn after a search that did not compute them.
#' @return A named character vector for [shiny::selectInput()].
#' @author Ken Kelley
#' @noRd
sieve_order_choices <- function(structure = TRUE) {
  metrics <- sieve_optimizer_metrics()
  if (!isTRUE(structure)) {
    metrics <- metrics[metrics$family != "structure", , drop = FALSE]
  }
  c(
    stats::setNames("search", "Search order (fewest items excluded first)"),
    stats::setNames(metrics$metric, sprintf(
      "%s (%s first)", metrics$label, ifelse(metrics$better == "higher", "highest", "lowest")
    ))
  )
}

#' The ordering in force for a search result
#'
#' The researcher's choice when the search computed that statistic, and
#' search order otherwise (no choice yet, or a statistic the search was not
#' asked to compute). Search order favors no statistic, so this never ranks
#' the table on something the researcher did not choose.
#'
#' @param r A search result from `sieve_optimize_items()`.
#' @param by The value of the ordering control.
#' @return `"search"` or a metric name.
#' @author Ken Kelley
#' @noRd
sieve_order_by <- function(r, by) {
  by <- by %||% "search"
  if (length(by) == 1L && by %in% sieve_order_choices(structure = isTRUE(r$simple_structure))) {
    by
  } else {
    "search"
  }
}

#' Whether the ordering control offers the simple structure statistics
#'
#' After a search, the statistics that search computed: a table without
#' complexity or hyperplane columns cannot be ordered by them, whatever the
#' checkbox now says. Before any search (or after a refused one), the
#' statistics the checkbox asks for, so the control never offers an order
#' the next search would not compute.
#'
#' @param r The kept search result, or `NULL`.
#' @param requested The value of the simple structure checkbox (`NULL`
#'   before the page has drawn it, which means its initial value, on).
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_order_structure <- function(r, requested = TRUE) {
  if (!is.null(r) && is.null(r$error)) {
    return(isTRUE(r$simple_structure))
  }
  isTRUE(requested %||% TRUE)
}

#' A number of seconds in rough words
#'
#' For the estimate of how long the next search would take, which is a rough
#' figure by nature (it is scaled from the last search's rate), so it is
#' said in round units. A display helper.
#'
#' @param seconds A non-negative number.
#' @return A length one character string such as `"about 40 seconds"`.
#' @author Ken Kelley
#' @noRd
sieve_rough_seconds <- function(seconds) {
  if (!is.finite(seconds) || seconds < 0) {
    return("an unknown time")
  }
  if (seconds < 1.5) {
    return("about a second")
  }
  if (seconds < 90) {
    return(sprintf("about %s seconds", formatC(seconds, format = "f", digits = 0)))
  }
  if (seconds < 5400) {
    minutes <- formatC(seconds / 60, format = "f", digits = 0)
    return(sprintf("about %s minute%s", minutes, if (minutes == "1") "" else "s"))
  }
  sprintf("about %s hours", formatC(seconds / 3600, format = "f", digits = 1))
}

#' The live count, as one paragraph
#'
#' What the researcher reads under the settings before pressing Run search:
#' how many candidate item sets the settings define, which constructs or
#' facets are held and why, where the count stands against the cap, what a
#' typed maximum amounts to when no construct can lose that many, and what
#' Run search would do, with a rough time scaled from the last search of
#' this session when there was one. One paragraph, so the panel is read top
#' to bottom instead of as a stack of alerts.
#'
#' @param plan A feasible plan from `sieve_optimizer_plan()`.
#' @param settings Usable settings from `sieve_optimizer_panel_settings()`.
#' @param simple_structure Whether the simple structure checkbox is on.
#' @param last The last search result of this session, or `NULL`.
#' @return A list with `text` (the paragraph) and `warn` (`TRUE` when Run
#'   search would be refused).
#' @author Ken Kelley
#' @noRd
sieve_count_paragraph <- function(plan, settings, simple_structure = TRUE, last = NULL) {
  total <- plan$total
  cap <- settings$cap
  need <- sieve_optimizer_requirements()
  over <- total > cap
  held <- sieve_held_text(plan$constructs, plan$facets)
  count <- sieve_count_text(total, plan$exact)
  said <- c(
    sprintf(
      paste0(
        "These settings define %s candidate item set%s, counting your current item ",
        "set, %s the cap of %s%s."
      ),
      count, if (total == 1) "" else "s", if (over) "above" else "within",
      sieve_count_text(cap),
      if (over) ", so Run search would be refused before anything is built or fit" else ""
    ),
    if (length(held)) {
      paste(
        paste(held, collapse = " "),
        if (length(held) == 1L) {
          "All of its items stay in every candidate."
        } else {
          "All of their items stay in every candidate."
        }
      )
    },
    if (settings$max_drop > plan$most_droppable) {
      if (plan$most_droppable == 0L) {
        paste0(
          "No construct can lose an item under these constraints, so your current item ",
          "set is the only candidate."
        )
      } else {
        sprintf(
          paste0(
            "No construct can lose more than %d item%s under these constraints, so a ",
            "maximum of %s allows the same exclusions as %d."
          ),
          plan$most_droppable, if (plan$most_droppable == 1L) "" else "s",
          sieve_count_text(settings$max_drop), plan$most_droppable
        )
      }
    },
    if (min(settings$min_keep) < need$omega && settings$max_drop > 0L) {
      sprintf(
        paste0(
          "With a minimum of %d, some candidates leave a construct with fewer than %d ",
          "items. Omega cannot be computed for those; they are listed with that reason."
        ),
        min(settings$min_keep), need$omega
      )
    },
    if (!is.null(settings$min_facet) && !nrow(plan$facets)) {
      "No included item has a facet, so the facet minimum has no effect."
    },
    if (!over) {
      sieve_run_forecast(total, count, simple_structure, last)
    }
  )
  list(text = paste(said, collapse = " "), warn = over)
}

#' What Run search would do, and roughly how long it would take
#'
#' @param total,count The number of candidates, and the same in words.
#' @param simple_structure Whether simple structure would be described.
#' @param last The last search result of this session, or `NULL`.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_run_forecast <- function(total, count, simple_structure = TRUE, last = NULL) {
  models <- paste0(
    "its correlated factors model and a one-factor model per construct for omega",
    if (isTRUE(simple_structure)) ", and one exploratory factor analysis for simple structure"
  )
  what <- if (total == 1) {
    sprintf("Run search describes your current item set alone (%s).", models)
  } else {
    sprintf(
      "Run search examines all %s while the session waits, fitting for each item set %s.",
      count, models
    )
  }
  timed <- !is.null(last) && is.null(last$error) && is.numeric(last$elapsed) &&
    is.finite(last$elapsed) && is.numeric(last$evaluated) && last$evaluated >= 1
  when <- if (timed) {
    then <- isTRUE(last$simple_structure)
    sprintf(
      paste0(
        "The last search examined %s item set%s in %s %s simple structure, so this ",
        "one would take %s%s."
      ),
      format(last$evaluated, big.mark = ","), if (last$evaluated == 1) "" else "s",
      sieve_rough_seconds(last$elapsed), if (then) "with" else "without",
      sieve_rough_seconds(last$elapsed / last$evaluated * total),
      if (identical(then, isTRUE(simple_structure))) {
        ""
      } else if (then) {
        " or less, since simple structure is now left out"
      } else {
        " or more, since simple structure is now included"
      }
    )
  } else {
    "There is no earlier search in this session to estimate the time from."
  }
  paste(what, when)
}

#' What the last Apply did, in words
#'
#' @param done The record the Apply observer keeps: `kept`, `dropped` (the
#'   items it excluded), and `returned` (the items it included again).
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_applied_text <- function(done) {
  n_dropped <- length(done$dropped)
  n_returned <- length(done$returned)
  n_kept <- length(done$kept)
  paste(c(
    if (n_dropped) {
      sprintf(
        "Excluded %d item%s: %s.", n_dropped, if (n_dropped == 1L) "" else "s",
        paste(done$dropped, collapse = ", ")
      )
    } else {
      "It excluded no item: every item that was included is in this item set."
    },
    if (n_returned) {
      sprintf(
        "Included again %d item%s that an earlier choice from this search had excluded: %s.",
        n_returned, if (n_returned == 1L) "" else "s", paste(done$returned, collapse = ", ")
      )
    },
    sprintf(
      paste0(
        "%d item%s now included. The exclusions hold across the whole pipeline and can ",
        "also be changed on the Items tab."
      ),
      n_kept, if (n_kept == 1L) " is" else "s are"
    )
  ), collapse = " ")
}

#' The frozen columns of the candidate table, if DT can freeze them
#'
#' The table is wider than any window, so its first columns (the order with
#' the value it is ordered by, the id, the number of items kept, and the
#' dropped items) are frozen with the DataTables FixedColumns extension and
#' stay in view while the statistics scroll. DT ships the extension as a
#' directory and refuses one whose directory is absent, so its presence is
#' checked (`sieve_dt_has_extension()`) and the table is drawn without it,
#' with a note, when it is missing.
#'
#' @param columns Number of leading drawn columns to freeze.
#' @return A list with `available` (logical), `extensions` (the
#'   `extensions` argument for [DT::datatable()]), `options` (DataTables
#'   options to append), and `note` (`NULL`, or the sentence that says the
#'   columns are not frozen and why).
#' @author Ken Kelley
#' @noRd
sieve_candidate_fixed_columns <- function(columns = 4L) {
  available <- sieve_dt_has_extension("FixedColumns")
  list(
    available = available,
    extensions = if (available) "FixedColumns" else list(),
    options = if (available) list(fixedColumns = list(leftColumns = columns)) else list(),
    note = if (available) {
      NULL
    } else {
      paste0(
        "This installation of the DT package does not include the DataTables FixedColumns ",
        "extension, so the first columns (order, id, items kept, excluded items) scroll ",
        "with the rest of the table instead of staying in view; the detail pane below ",
        "the table shows a selected item set in full. Some repackaged builds of DT ",
        "leave bundled extensions out. Installing DT from CRAN restores them: ",
        "install.packages(\"DT\")."
      )
    }
  )
}

#' The caption of the candidate table
#'
#' Held constructs and held facets are reported in the table itself, in the
#' same words as everywhere else, and so is a missing FixedColumns
#' extension.
#'
#' @param r A search result from `sieve_optimize_items()`.
#' @param fixed The result of `sieve_candidate_fixed_columns()`.
#' @return A caption tag, or `NULL` when there is nothing to say.
#' @author Ken Kelley
#' @noRd
sieve_candidate_caption <- function(r, fixed = sieve_candidate_fixed_columns()) {
  held <- sieve_held_text(r$constructs, r$facets)
  said <- c(
    if (length(held)) {
      paste(
        paste(held, collapse = " "),
        if (length(held) == 1L) {
          "All of its items are in every item set in this table."
        } else {
          "All of their items are in every item set in this table."
        }
      )
    },
    fixed$note
  )
  if (!length(said)) {
    return(NULL)
  }
  tags$caption(
    class = "small text-muted", style = "caption-side: top; text-align: left;",
    paste(said, collapse = " ")
  )
}

#' Where one candidate stands in the order the researcher chose, in words
#'
#' @param r A search result from `sieve_optimize_items()`.
#' @param row Row of the results table (search order).
#' @param by `"search"` or a statistic; see `sieve_order_candidates()`.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_candidate_rank_text <- function(r, row, by = "search") {
  metrics <- sieve_optimizer_metrics()
  tab <- r$table
  n <- nrow(tab)
  is_ref <- identical(tab$role[row], "baseline")
  ord <- sieve_order_candidates(tab, by)
  pos <- ord$position[row]
  if (!by %in% metrics$metric) {
    return(if (is_ref) {
      "The table is in search order; this is your current item set, shown first as the reference."
    } else {
      sprintf(
        paste0(
          "The table is in search order (fewest items excluded first), which ranks ",
          "nothing; this item set excludes %d item%s and is listed at position %d of %d."
        ),
        tab$n_dropped[row], if (tab$n_dropped[row] == 1L) "" else "s", pos, n
      )
    })
  }
  m <- metrics[match(by, metrics$metric), ]
  ok <- isTRUE(tab[[paste0(m$family, "_ok")]][row])
  value <- tab[[by]][row]
  shown <- if (is.finite(value)) {
    sprintf(
      "its %s is %s%s", m$label, formatC(value, format = "f", digits = 3),
      if (ok) "" else ", shown with an asterisk because it is computed but not usable for ordering"
    )
  } else {
    sprintf("no %s is available for it", m$label)
  }
  direction <- if (identical(m$better, "higher")) "highest first" else "lowest first"
  n_ranked <- sum(ord$group == "ranked")
  if (is_ref) {
    return(sprintf(
      paste0(
        "Ordered by %s (%s): this is your current item set, the reference, shown ",
        "first and not ranked among the candidates; %s."
      ),
      m$label, direction, shown
    ))
  }
  if (identical(ord$group[row], "ranked")) {
    return(sprintf(
      paste0(
        "Ordered by %s (%s): ranked %d of the %d item set%s that can be ranked ",
        "on it, at position %d of %d; %s."
      ),
      m$label, direction, ord$rank[row], n_ranked, if (n_ranked == 1L) "" else "s",
      pos, n, shown
    ))
  }
  note <- tab[[paste0(m$family, "_note")]][row]
  sprintf(
    paste0(
      "Ordered by %s (%s): not ranked, listed after the %d ranked item set%s at ",
      "position %d of %d, because %s; %s."
    ),
    m$label, direction, n_ranked, if (n_ranked == 1L) "" else "s", pos, n,
    if (nzchar(note)) note else "the statistic is not available for it", shown
  )
}

#' One candidate's eligibility, family by family, in sentences
#'
#' @param r A search result from `sieve_optimize_items()`.
#' @param row Row of the results table (search order).
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_candidate_status_text <- function(r, row) {
  tab <- r$table[row, , drop = FALSE]
  family <- function(ok, note, label) {
    if (isTRUE(ok)) {
      sprintf("%s: available and usable for ordering.", label)
    } else {
      sprintf("%s: %s.", label, note)
    }
  }
  paste(c(
    switch(tab$status,
      eligible = "Eligible: every requested statistic is available and usable for ordering.",
      "partly eligible" = "Partly eligible: some statistics are unavailable or not ranked.",
      "Not eligible: no statistic can be ranked."
    ),
    family(tab$reliability_ok, tab$reliability_note, "Reliability"),
    family(tab$fit_ok, tab$fit_note, "Fit"),
    if (isTRUE(r$simple_structure)) {
      family(tab$structure_ok, tab$structure_note, "Simple structure")
    } else {
      paste0(
        "Simple structure: not requested for this search, so it is neither shown nor ",
        "offered for ordering."
      )
    }
  ), collapse = " ")
}

#' The candidate table as shown, in the order the researcher chose
#'
#' A display helper: this is where statistics are rounded, and where a
#' statistic that does not exist is written as words instead of being left
#' blank. Rows stay in search order (so a selected row indexes the search
#' result's `kept` list); the first column carries the display position.
#'
#' The columns a reader needs first come first, so they are on screen
#' without scrolling sideways: the order (with the value of the statistic
#' the table is ordered by), the item set, what it drops, and its status
#' and reason. The statistics follow, family by family. A value that was
#' computed but is not usable for ordering (an improper solution, for a fit
#' index or for omega) carries an asterisk wherever it is shown.
#'
#' @param r A search result from `sieve_optimize_items()`.
#' @param by `"search"` or a statistic; see `sieve_order_candidates()`.
#' @return A data frame of character columns plus the integer `position`.
#' @author Ken Kelley
#' @noRd
sieve_candidate_display <- function(r, by = "search") {
  tab <- r$table
  ord <- sieve_order_candidates(tab, by)
  metrics <- sieve_optimizer_metrics()
  num <- function(x, ok, digits = 3, missing = "unavailable") {
    out <- ifelse(is.finite(x), formatC(x, format = "f", digits = digits), missing)
    ifelse(is.finite(x) & !ok, paste0(out, " *"), out)
  }
  count <- function(x) ifelse(is.finite(x), format(x, big.mark = ",", trim = TRUE), "none")
  cons <- colnames(r$omega)
  # The state of each construct's one-factor solution; a result that does
  # not record it is read as every solution proper, and nothing is marked.
  omega_state <- r$omega_state %||% matrix("ok", nrow(r$omega), ncol(r$omega))
  by_construct <- vapply(seq_len(nrow(tab)), function(i) {
    paste(sprintf("%s %s", cons, num(r$omega[i, ], omega_state[i, ] != "improper")),
      collapse = "; "
    )
  }, character(1))
  omega_n <- ifelse(
    !is.finite(tab$omega_n_min), "none",
    ifelse(tab$omega_n_min == tab$omega_n_max, count(tab$omega_n_min),
      paste(count(tab$omega_n_min), "to", count(tab$omega_n_max))
    )
  )
  not_requested <- grepl("not requested", tab$structure_note, fixed = TRUE)
  ss_missing <- ifelse(not_requested, "not requested", "unavailable")
  status <- ifelse(
    tab$status == "eligible",
    ifelse(not_requested, "all requested statistics available", "all statistics available"),
    paste0(ifelse(tab$status == "partly eligible", "some statistics unavailable or not ranked: ",
      "no statistic can be ranked: "
    ), tab$reason)
  )
  status <- ifelse(not_requested, paste0(status, "; simple structure not requested"), status)
  status <- ifelse(tab$role == "baseline", paste0("current item set (reference); ", status), status)
  # Beside the order, the value the table is ordered by, so it is read
  # without scrolling to its own column.
  label <- ord$label
  if (by %in% metrics$metric) {
    family_ok <- tab[[paste0(metrics$family[match(by, metrics$metric)], "_ok")]]
    label <- ifelse(is.finite(tab[[by]]), sprintf("%s (%s)", label, num(tab[[by]], family_ok)),
      label
    )
  }
  data.frame(
    position = ord$position,
    order = label,
    id = as.character(tab$id),
    n_items = as.character(tab$n_items),
    dropped = tab$dropped,
    status = status,
    per_construct = tab$per_construct,
    omega_min = num(tab$omega_min, tab$reliability_ok),
    omega_mean = num(tab$omega_mean, tab$reliability_ok),
    omega_by = by_construct,
    omega_n = omega_n,
    cfi = num(tab$cfi, tab$fit_ok),
    rmsea = num(tab$rmsea, tab$fit_ok),
    rmsea_ci = ifelse(is.finite(tab$rmsea_lo) & is.finite(tab$rmsea_hi),
      sprintf("[%.3f, %.3f]", tab$rmsea_lo, tab$rmsea_hi), "unavailable"
    ),
    srmr = num(tab$srmr, tab$fit_ok),
    fit_n = count(tab$fit_n),
    complexity = num(tab$complexity, tab$structure_ok, missing = ss_missing),
    hyperplane = num(tab$hyperplane, tab$structure_ok, missing = ss_missing),
    structure_n = count(tab$structure_n),
    stringsAsFactors = FALSE
  )
}

#' The two row header of the candidate table
#'
#' The upper row names, for each family of statistics, the model that
#' produced it and the rows it was computed on, so no column is read without
#' its target and sample. The header of the order column names the statistic
#' the table is ordered by.
#'
#' @param r A search result from `sieve_optimize_items()`.
#' @param by `"search"` or a statistic; see `sieve_order_candidates()`.
#' @return An HTML table skeleton for [DT::datatable()]'s `container`.
#' @author Ken Kelley
#' @noRd
sieve_candidate_header <- function(r, by = "search") {
  tab <- r$table
  metrics <- sieve_optimizer_metrics()
  fit_how <- if (identical(tab$fit_estimator[1], "WLSMV")) {
    "WLSMV, because ordered or binary items are included; complete rows"
  } else if (identical(tab$fit_estimator[1], "MLR")) {
    "robust maximum likelihood (MLR) with full information ML; every row with an observed kept item"
  } else {
    "maximum likelihood (ML) with full information ML, DMAR's default; every row with an observed kept item"
  }
  order_head <- if (by %in% metrics$metric) {
    sprintf("order (%s)", metrics$label[match(by, metrics$metric)])
  } else {
    "order"
  }
  # CFI and RMSEA are in one family of fit indices for every item set; the
  # headers name it, so the column never mixes two statistics unannounced.
  family <- if (is.null(r$fit_family) || is.na(r$fit_family)) "" else sprintf(" (%s)", r$fit_family)
  # The app's stylesheet keeps table headers on one line and truncates them;
  # these headers carry the target and the sample, so they wrap instead.
  wrap <- "white-space: normal; overflow: visible; text-overflow: clip; vertical-align: bottom;"
  lone <- function(x) tags$th(rowspan = 2, style = wrap, x)
  group <- function(span, ...) tags$th(colspan = span, class = "dt-center", style = wrap, ...)
  tags$table(
    class = "display",
    # The stylesheet's fixed layout sizes columns from the first header row,
    # which here holds the spanning group headers, so the columns under one
    # group would all get the same width whatever their content.
    style = "table-layout: auto;",
    tags$thead(
      tags$tr(
        lone("position"), lone(order_head), lone("id"), lone("items kept"),
        lone("excluded items"), lone("status and reason (* = shown, not ranked)"),
        lone("items per construct"),
        group(4, paste0(
          "Reliability: Coefficient Omega, \u03c9, as Robust omega (observed denominator), one-factor model of the item codes treated as ",
          "continuous; cases complete on each construct's kept items"
        )),
        group(5, paste0("Fit of each item set's own correlated factors model: ", fit_how)),
        group(3, paste0(
          "Simple structure: exploratory ML factor analysis, direct oblimin, ",
          "Pearson correlations; cases complete on all kept items"
        ))
      ),
      tags$tr(lapply(
        c(
          "min \u03c9", "mean \u03c9", "\u03c9 by construct", "n",
          paste0("CFI", family), paste0("RMSEA", family),
          paste0("RMSEA ", sieve_rmsea_ci_words(r$fit_conf_level %||% NA_real_), family), "SRMR", "n",
          "complexity", "hyperplane", "n"
        ),
        function(x) tags$th(style = wrap, x)
      ))
    )
  )
}

#' Column widths of the candidate table
#'
#' The table scrolls sideways, so each column gets the room its content
#' needs instead of an equal share of the panel. The first five columns
#' (order, id, items kept, dropped items, status and reason) are sized to
#' fit together in the panel at a 1,440 pixel window.
#'
#' @return A list of DataTables column definitions (zero-based targets, in
#'   the column order of `sieve_candidate_display()`).
#' @author Ken Kelley
#' @noRd
sieve_candidate_widths <- function() {
  px <- c(
    position = 0, order = 150, id = 40, n_items = 55, dropped = 200, status = 400,
    per_construct = 190, omega_min = 95, omega_mean = 95, omega_by = 330, omega_n = 90,
    cfi = 80, rmsea = 80, rmsea_ci = 130, srmr = 80, fit_n = 70,
    complexity = 105, hyperplane = 105, structure_n = 70
  )
  lapply(seq_along(px)[-1], function(i) {
    list(width = sprintf("%dpx", px[[i]]), targets = i - 1L)
  })
}

#' The candidate table widget
#'
#' The order is the researcher's explicit choice, applied through the hidden
#' position column, so the reference row stays first and an item set that is
#' not ranked can never sort above a ranked one. Header sorting is off for
#' the same reason: a click must not be able to sort a blank or an unusable
#' value to the top. The data stay in search order, so the row a user
#' selects still indexes the search result's `kept` list.
#'
#' The first drawn columns (the order with its value, the id, the number of
#' items kept, and the dropped items) are frozen when the installed DT
#' bundles the FixedColumns extension (`sieve_candidate_fixed_columns()`),
#' so a row keeps its identity while its statistics scroll into view. The
#' caption names what is held intact in every item set.
#'
#' @param r A search result from `sieve_optimize_items()`.
#' @param by `"search"` or a statistic; see `sieve_order_candidates()`.
#' @return A [DT::datatable()] widget.
#' @author Ken Kelley
#' @noRd
sieve_candidate_widget <- function(r, by = "search") {
  fixed <- sieve_candidate_fixed_columns()
  sieve_dt(
    sieve_candidate_display(r, by = by),
    rownames = FALSE, selection = "single",
    container = sieve_candidate_header(r, by = by),
    caption = sieve_candidate_caption(r, fixed),
    extensions = fixed$extensions,
    options = c(
      list(
        pageLength = 15, dom = "tp", scrollX = TRUE, autoWidth = TRUE,
        order = list(list(0, "asc")),
        columnDefs = c(
          list(
            list(visible = FALSE, targets = 0),
            list(orderable = FALSE, targets = "_all")
          ),
          sieve_candidate_widths()
        )
      ),
      fixed$options
    )
  ) |>
    formatStyle("order", fontWeight = "bold")
}

#' One candidate described in full
#'
#' In sentences: what it keeps and drops, where it stands in the order the
#' researcher chose and why it is or is not ranked there, which statistics
#' are available and why any is not, what is held intact in every item set,
#' then each family's values and what the engines signaled.
#'
#' @param r A search result from `sieve_optimize_items()`.
#' @param row Row of the results table (search order).
#' @param by `"search"` or a statistic; see `sieve_order_candidates()`.
#' @return A tag list for the detail pane under the table.
#' @author Ken Kelley
#' @noRd
sieve_candidate_detail <- function(r, row, by = "search") {
  tab <- r$table[row, , drop = FALSE]
  show <- sieve_candidate_display(r)[row, , drop = FALSE]
  kept <- r$kept[[row]]
  said <- r$warnings[[row]]
  held <- sieve_held_text(r$constructs, r$facets)
  line <- function(what, ...) tags$li(tags$b(paste0(what, ":")), " ", ...)
  note <- function(x) if (nzchar(x)) paste0(" Note: ", x, ".") else ""
  div(
    class = "small border rounded p-2 mt-2",
    tags$div(
      class = "fw-semibold",
      sprintf(
        "Item set %d%s", tab$id,
        if (tab$role == "baseline") " (your current item set, the reference)" else ""
      )
    ),
    tags$ul(
      class = "mb-0",
      line(sprintf("Keeps %d items", tab$n_items), paste(kept, collapse = ", ")),
      line("Excludes", tab$dropped),
      line("Order", sieve_candidate_rank_text(r, row, by)),
      line("Status", sieve_candidate_status_text(r, row)),
      if (length(held)) {
        line(
          "Held intact in every item set",
          paste(c(held, "These items are in this item set and in every other."),
            collapse = " "
          )
        )
      },
      line(
        "Coefficient Omega, \u03c9, by construct (continuous one-factor model, complete cases)",
        paste0(show$omega_by, "; cases: ", show$omega_n, ".", note(tab$reliability_note))
      ),
      line(
        sprintf(
          "Fit (%s, %s, n = %s; solution status: %s)", tab$fit_estimator,
          if (identical(tab$fit_missing, "fiml")) "full information ML" else "complete rows",
          show$fit_n, tab$fit_code
        ),
        paste0(sprintf(
          "CFI %s, RMSEA %s, %s %s, SRMR %s.", show$cfi, show$rmsea,
          sieve_rmsea_ci_words(r$fit_conf_level %||% NA_real_), show$rmsea_ci, show$srmr
        ), note(tab$fit_note))
      ),
      line(
        "Simple structure (exploratory ML, oblimin, Pearson, complete cases)",
        if (grepl("not requested", tab$structure_note, fixed = TRUE)) {
          "not requested for this search."
        } else {
          paste0(sprintf(
            "complexity %s, hyperplane %s, n = %s.", show$complexity,
            show$hyperplane, show$structure_n
          ), note(tab$structure_note))
        }
      ),
      if (length(said)) {
        line("Signaled while fitting", paste(gsub("\\s+", " ", said), collapse = " | "))
      }
    )
  )
}

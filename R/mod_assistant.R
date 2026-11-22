# Shared project assistant, available in the project bar on every tab.

#' Wire the scientific task dialog to project controls
#' @param input,output,session Module inputs, outputs, and session.
#' @param state Shared state.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_assistant_wire <- function(input, output, session, state) {
  plan <- shiny::reactiveVal(NULL)
  result <- shiny::reactiveVal(NULL)
  result_context <- shiny::reactiveVal(NULL)
  payload <- shiny::reactiveVal(NULL)
  response <- shiny::reactiveVal(NULL)
  approved_payload <- shiny::reactiveVal(NULL)
  kept_response <- shiny::reactiveVal(NULL)
  local_result_key <- function() {
    state$sampling_design
    list(inputs = sieve_page_revision(state),
      estimator = sieve_estimator_effective("study", state),
      goal = sieve_trim(input$assist_goal %||% ""), task = input$assist_task %||% "understand",
      basis = state$construct_basis, scoring = sieve_hash(state$scoring_policies))
  }
  local_result_status <- shiny::reactive({
    r <- result()
    original <- result_context()
    if (is.null(r) || is.null(original)) return(list(state = "not_run"))
    current <- identical(original$key, local_result_key())
    failed <- !is.null(r$status$code) && r$status$code %in% c("failed", "not_converged", "not_identified")
    list(state = if (!current) "stale" else if (failed) "failed" else "current",
      sentence = if (current && failed) "The recorded task used the current inputs but did not produce a usable fitted result. Review its status before trying again." else if (current)
        "This recorded local result matches the current inputs and task." else
        "Historical local result: the data, respondent rows, sampling design, estimator, goal, or task changed. Prepare and run a current plan only if you need updated results.",
      key = original$key, when = original$when)
  })
  external_context <- function() sieve_assistant_context(state, isTRUE(input$assist_wordings),
    analysis_ids = input$assist_analysis_ids, claim_ids = input$assist_claim_ids,
    decision_ids = input$assist_decision_ids
  )
  # Consent belongs to the disclosed immutable request. Preparing another
  # payload invalidates it on the server before the browser acknowledges
  # the checkbox reset.
  shiny::observeEvent(input$assist_consent,
    {
      p <- payload()
      approved_payload(if (isTRUE(input$assist_consent) && !is.null(p)) p$review_key else NULL)
    },
    ignoreNULL = FALSE
  )
  payload_is_current <- function(p) {
    current_plan <- plan()
    if (is.null(p) || is.null(current_plan)) return(FALSE)
    current <- external_context()
    identical(p$key, sieve_hash(current)) && identical(p$plan_key, current_plan$key) &&
      identical(p$goal, sieve_trim(input$assist_goal %||% "")) &&
      identical(input$assist_task %||% "understand", current_plan$task) &&
      identical(input$assist_provider, p$request$provider) && identical(input$assist_model, p$request$model) &&
      identical(as.integer(input$assist_max_chars %||% 50000L), p$request$max_chars)
  }
  shiny::observeEvent(input$assistant, {
    ns <- session$ns
    shiny::showModal(shiny::modalDialog(
      title = "Scientific task assistant", size = "l", easyClose = TRUE,
      shiny::p("Theory-informed and evidence-supported psychometric decision making. Local tasks use the same statistical services as the tabs; an external model supplies proposals only."),
      bslib::navset_tab(
        bslib::nav_panel(
          "Goal and plan",
          shiny::textAreaInput(ns("assist_goal"), "What are you trying to understand or decide?", width = "100%"),
          shiny::selectInput(
            ns("assist_task"),
            "Task",
            c(
              "Understand the project" = "understand",
              "Fit the construct CFA" = "cfa",
              "Estimate construct reliability" = "reliability",
              "Review content and response processes" = "content",
              "Formulate external predictions" = "predictions",
              "Record a judgment" = "decision",
              "Plan the next study" = "next_study"
            )
          ),
          shiny::actionButton(ns("assist_plan"), "Prepare plan", class = "btn-primary"),
          shiny::uiOutput(ns("assist_stages")),
          shiny::uiOutput(ns("assist_plan_summary")),
          shiny::tags$details(shiny::tags$summary("Technical plan"), shiny::verbatimTextOutput(ns("assist_plan_text"))),
          shiny::actionButton(ns("assist_execute"),
            "Run reviewed local task",
            class = "btn-outline-primary",
            disabled = TRUE
          ),
          shiny::uiOutput(ns("assist_result_summary")),
          shiny::tags$details(
            shiny::tags$summary("Exact local results"),
            shiny::verbatimTextOutput(ns("assist_result"))
          ),
          shiny::tags$details(
            shiny::tags$summary("Keep a researcher decision (local, no provider)"),
            shiny::p(
              class = "small",
              "Prepare a current plan above. Your decision is recorded with the goal and the exact saved analyses on the current data and item revision. Rationale is optional; recording does not change items or scoring."
            ),
            shiny::verbatimTextOutput(ns("assist_decision_evidence")),
            shiny::textAreaInput(ns("assist_local_choice"), "Your decision", width = "100%"),
            shiny::textAreaInput(ns("assist_local_rationale"), "Rationale (optional)", width = "100%"),
            shiny::textAreaInput(ns("assist_local_evidence"),
              "Additional evidence or limitations (optional)",
              width = "100%"
            ),
            shiny::actionButton(ns("assist_keep_decision"),
              "Keep researcher decision",
              class = "btn-outline-primary",
              disabled = TRUE
            )
          )
        ),
        bslib::nav_panel(
          "Construct and claim",
          shiny::p("These records are your statements. Saving a definition does not reassign items. New versions preserve earlier definitions and frozen claims."),
          shiny::textInput(ns("construct_name"), "Construct"),
          shiny::textAreaInput(ns("construct_definition"), "Definition, uncertainties, and boundaries", width = "100%"),
          shiny::textInput(ns("construct_population"), "Population (unsure is allowed)", "unspecified"),
          shiny::textInput(ns("construct_use"), "Intended score interpretation and use", "unspecified"),
          shiny::textInput(
            ns("construct_facets"),
            "Facet commitments and content that must be retained",
            "unspecified"
          ),
          shiny::actionButton(ns("construct_save"), "Keep definition version"),
          shiny::textAreaInput(ns("claim_prediction"), "Falsifiable claim and planned evaluation", width = "100%"),
          shiny::selectInput(
            ns("claim_exposure"),
            "Timing and exposure declaration",
            c(
              "Exploratory or after relevant exposure" = "exploratory_after_exposure",
              "Prospective declaration (software cannot verify prior human exposure)" = "prospective_declaration"
            )
          ),
          shiny::textAreaInput(ns("claim_score_rule"), "Operational score and missing-response rule",
            value = "available-item mean; all-missing yields NA", width = "100%"
          ),
          shiny::p(
            class = "small",
            "Edit this rule for a factor score or another operational score. The frozen claim retains saved analysis specifications and statuses matching this exact data and item revision; an empty model list means none was recorded."
          ),
          shiny::actionButton(ns("claim_freeze"), "Freeze claim with current item and score revision"),
          shiny::selectInput(ns("evaluate_claim_id"), "Frozen claim to evaluate", choices = character(0)),
          shiny::uiOutput(ns("claim_selected")),
          shiny::downloadButton(ns("claim_copy"), "Export frozen claim and SHA-256 (.tar.gz)"),
          shiny::selectInput(
            ns("evaluate_outcome"),
            "Evaluation outcome",
            c(
              "inconclusive",
              "supported within stated limits",
              "contradicted",
              "failed analysis",
              "not yet evaluated"
            )
          ),
          shiny::textAreaInput(ns("evaluate_evidence"), "Evidence, uncertainty, and limitations", width = "100%"),
          shiny::selectInput(
            ns("evaluate_role"),
            "Sample role",
            c(
              "internal assessment",
              "independently collected (researcher declaration)"
            )
          ),
          shiny::actionButton(ns("evaluate_keep"), "Append evaluation without changing frozen claim"),
          shiny::uiOutput(ns("construct_catalog")),
          shiny::tags$details(
            shiny::tags$summary("Exact construct and claim records"),
            shiny::verbatimTextOutput(ns("construct_records"))
          )
        ),
        bslib::nav_panel(
          "Optional external review",
          shiny::div(class = "alert alert-success", "Nothing is sent anywhere by default. The key stays in memory for this session and is never logged or written to disk."),
          shiny::p("Prepare and inspect the exact payload first. It contains the goal and selected task, item identifiers and assignments, sampling design, saved definitions, selected claims and decisions, and compact current analysis records. Respondent-level data are excluded. Dedicated item wording columns are included only if selected; saved narrative definitions, decisions, or analysis notes may quote item text, so inspect the complete payload. A provider request may incur charges and is subject to that provider's terms and data controls."),
          shiny::checkboxInput(ns("assist_wordings"), "Include private item wording in the disclosed payload", FALSE),
          shiny::div(
            class = "alert alert-warning",
            "You accept responsibility and liability for all data sent to a third party under your data agreements and ethics approval."
          ),
          shiny::tags$ul(class = "small",
            shiny::tags$li("The key lives in memory for this session only; it is never logged or written to disk."),
            shiny::tags$li("No external request can fire until the exact payload is inspected, its review key is authorized, and a session-only key is supplied."),
            shiny::tags$li("Local response bounds, reverse coding and wording checks run on this machine only and need no provider.")
          ),
          shiny::selectizeInput(ns("assist_analysis_ids"),
            "Include current analyses",
            choices = character(),
            multiple = TRUE
          ),
          shiny::selectizeInput(ns("assist_claim_ids"),
            "Include frozen claims",
            choices = character(),
            multiple = TRUE
          ),
          shiny::selectizeInput(ns("assist_decision_ids"), "Include decisions", choices = character(), multiple = TRUE),
          shiny::numericInput(ns("assist_max_chars"),
            "Payload ceiling (characters)",
            value = 50000L,
            min = 1000L,
            max = 500000L
          ),
          shiny::numericInput(ns("assist_timeout"), "Provider timeout (seconds)", value = 60L, min = 5L, max = 300L),
          shiny::selectInput(ns("assist_provider"), "Provider", c("OpenAI", "Anthropic")),
          shiny::textInput(ns("assist_model"), "Provider model identifier (your account must support it)"),
          shiny::passwordInput(ns("assist_key"), "API key (used for one request, then cleared)"),
          shiny::actionButton(ns("assist_prepare_external"), "Prepare exact external payload", disabled = TRUE),
          shiny::uiOutput(ns("assist_payload_summary")),
          shiny::downloadButton(ns("assist_payload_copy"), "Keep a local copy of this reviewed request (.txt)"),
          shiny::tags$details(
            shiny::tags$summary("Complete immutable request"),
            shiny::verbatimTextOutput(ns("assist_payload"))
          ),
          shiny::checkboxInput(
            ns("assist_consent"),
            "I authorize sending this exact disclosed payload to the selected provider, and I accept responsibility and liability for disclosing it under my data agreements and ethics approval",
            FALSE
          ),
          shiny::actionButton(ns("assist_send"),
            "Send disclosed review request",
            class = "btn-outline-primary",
            disabled = TRUE
          ),
          shiny::verbatimTextOutput(ns("assist_response")),
          shiny::selectInput(
            ns("assist_disposition"),
            "Record the proposal as",
            c(
              "Not adopted" = "not_adopted",
              "Rejected" = "rejected",
              "Researcher-edited decision" = "adopted"
            )
          ),
          shiny::textAreaInput(ns("assist_revision"),
            "Your reasoning or edited decision (optional unless adopting)",
            width = "100%"
          ),
          shiny::actionButton(ns("assist_record"), "Keep proposal and researcher response", disabled = TRUE),
          shiny::tags$details(
            shiny::tags$summary("Proposals and disclosures"),
            shiny::uiOutput(ns("assist_records"))
          )
        )
      ),
      footer = shiny::modalButton("Close")
    ))
  })
  notify <- function(expr) tryCatch(expr, error = function(e) {
    sieve_notify(conditionMessage(e), type = "error")
    NULL
  })
  shiny::observeEvent(input$assist_plan, {
    p <- notify(sieve_assistant_plan(state, input$assist_goal, input$assist_task))
    if (!is.null(p)) {
      plan(p)
      result(NULL)
      result_context(NULL)

    }
  })
  output$assist_plan_text <- shiny::renderText({
    p <- plan()
    if (is.null(p)) return("Describe the goal and prepare a plan. Nothing runs automatically.")
    paste(c(
      paste("Goal:", p$goal), p$actions, p$cost,
      if (length(p$specs)) utils::capture.output(utils::str(p$specs, max.level = 2L))
    ), collapse = "\n")
  })
  output$assist_plan_summary <- shiny::renderUI(sieve_assistant_plan_ui(plan()))
  plan_is_current <- function() {
    p <- plan()
    !is.null(p) && identical(p$goal, sieve_trim(input$assist_goal %||% "")) &&
      identical(p$task, input$assist_task %||% "understand") &&
      identical(p$context_key, sieve_hash(sieve_assistant_context(state)))
  }
  output$assist_result_summary <- shiny::renderUI({
    r <- result()
    if (is.null(r)) return(NULL)
    freshness <- local_result_status()
    original <- result_context()
    tables <- sieve_assistant_result_tables(r)
    shiny::tagList(
      shiny::h6(if (identical(freshness$state, "current")) "Current recorded local result" else if (
        identical(freshness$state, "stale")) "Historical local result" else "Recorded local task outcome"),
      shiny::p(class = if (identical(freshness$state, "current")) "small text-muted" else "alert alert-warning small",
        freshness$sentence),
      if (!is.null(original)) shiny::p(class = "small text-muted",
        paste("Recorded goal:", original$key$goal, "; estimator:", original$key$estimator,
          "; dataset:", original$key$inputs$dataset, "; recorded:", original$when)),
      shiny::p(
        class = "small",
        paste(
          "Task:",
          r$task,
          "; status:",
          r$status$code %||% "see coefficient-specific status"
        )
      ),
      lapply(names(tables), function(nm) shiny::tagList(shiny::strong(nm), sieve_plain_table_ui(tables[[nm]])))
    )
  })
  stage_state <- shiny::reactive({
    invisible(state$meta)
    invisible(state$items)
    invisible(state$analyses)
    invisible(state$decisions)
    invisible(state$dataset_id)
    invisible(state$construct_basis)
    invisible(state$scientific_dataset_id)
    invisible(state$construct_records)
    invisible(state$frozen_claims)
    invisible(state$validation_events)
    p <- plan()
    current <- plan_is_current()
    ext <- payload()
    ready <- payload_is_current(ext)
    stages <- data.frame(
      stage = c(
        "1. Describe the goal",
        "2. Prepare and review a current plan",
        "3. Run the reviewed local task or keep a decision",
        "4. Optional: prepare exact external review",
        "5. Optional: authorize and send that request"
      ),
      status = c(
        if (sieve_has_content(input$assist_goal)) "ready" else "enter a goal above",
        if (current) "current plan ready" else "prepare a plan after changing the goal, task, or project",
        if (current) "available" else "review a current plan first",
        if (current && sieve_has_content(input$assist_model)) "available" else "review a current plan and enter a provider model",
        if (ready && identical(
          approved_payload(),
          ext$review_key
        ) && sieve_has_content(input$assist_key)) "available" else "inspect the prepared payload, authorize its exact review key, and enter a session-only key"
      )
    )
    states <- list(
      assist_execute = current, assist_keep_decision = current,
      assist_prepare_external = current && sieve_has_content(input$assist_model),
      assist_send = ready && !is.null(ext) && identical(
        approved_payload(),
        ext$review_key
      ) && sieve_has_content(input$assist_key),
      assist_record = !is.null(response()) && !identical(
        kept_response(),
        sieve_hash(list(
          payload()$review_key,
          response()
        ))
      )
    )
    list(table = stages, actions = states)
  })
  output$assist_stages <- shiny::renderUI(sieve_plain_table_ui(stage_state()$table))
  shiny::observe({
    states <- stage_state()$actions
    for (nm in names(states)) session$sendCustomMessage(
      "sieve-action-state",
      list(
        id = session$ns(nm),
        disabled = !isTRUE(states[[nm]])
      )
    )
  })
  shiny::observe({
    invisible(state$meta)
    invisible(state$items)
    invisible(state$analyses)
    invisible(state$decisions)
    invisible(state$construct_records)
    invisible(state$frozen_claims)
    invisible(state$validation_events)
    p <- payload()
    if (!is.null(p) && !payload_is_current(p) && !is.null(approved_payload())) {
      approved_payload(NULL)
      shiny::updateCheckboxInput(session, "assist_consent", value = FALSE)
    }
  })
  shiny::observeEvent(input$assist_execute, {
    shiny::req(plan())
    if (!identical(sieve_trim(input$assist_goal %||% ""), plan()$goal) ||
      !identical(input$assist_task %||% "understand", plan()$task)) {
      sieve_notify("The goal or task changed. Prepare and review a new local plan.", type = "error")
      return(invisible(NULL))
    }
    out <- notify(shiny::withProgress(message = "Running the reviewed local task", value = 0, {
      out <- sieve_assistant_execute(state, plan())
      shiny::incProgress(1)
      out
    }))
    result(out)
    if (!is.null(out)) {
      result_context(list(key = shiny::isolate(local_result_key()),
        when = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")))
    } else {
      result_context(NULL)

    }
  })
  output$assist_result <- shiny::renderText({
    if (is.null(result())) return("")
    paste(c(local_result_status()$sentence,
      utils::capture.output(utils::str(result(), max.level = 4L))), collapse = "\n")
  })
  decision_analysis_ids <- function() {
    now <- sieve_revision(state)
    records <- shiny::isolate(state$analyses) %||% list()
    vapply(Filter(function(r) sieve_same_revision(r$revision, now), records), function(r) r$id, character(1))
  }
  output$assist_decision_evidence <- shiny::renderText({
    # Reading the registry here is cheap and never invokes an engine.
    records <- state$analyses
    # Bind the displayed evidence note to inclusion, scoring, and data edits.
    invisible(state$meta)
    invisible(state$items)
    invisible(state$dataset_id)
    ids <- decision_analysis_ids()
    if (length(ids)) paste("Saved analyses on the current data and item revision:", paste(ids, collapse = ", "))
    else "No saved analysis matches the current data and item revision. A decision may still record theory or other evidence."
  })
  shiny::observeEvent(input$assist_keep_decision, {
    p <- plan()
    current <- if (!is.null(p)) notify(sieve_assistant_plan(state, p$goal, p$task)) else NULL
    if (is.null(p) || is.null(current) || !identical(p$key, current$key) ||
      !identical(p$goal, sieve_trim(input$assist_goal %||% "")) ||
      !identical(p$task, input$assist_task %||% "understand")) {
      sieve_notify("The goal, evidence, or project changed. Prepare and review a current plan before keeping this decision.",
        type = "error"
      )
      return(invisible(NULL))
    }
    evidence <- c(
      unname(decision_analysis_ids()),
      if (sieve_has_content(input$assist_local_evidence)) sieve_trim(input$assist_local_evidence)
    )
    kept <- notify(sieve_record_decision(state,
      topic = paste0("assistant_judgment:", sieve_hash(p$goal)), question = p$goal,
      choice = input$assist_local_choice, rationale = input$assist_local_rationale, evidence = evidence
    ))
    if (!is.null(kept)) sieve_notify("Researcher decision retained with its evidence and revision. Items and scoring are unchanged.",
      type = "message"
    )
  })
  shiny::observeEvent(input$construct_save, {
    notify(sieve_record_construct(state, input$construct_name, input$construct_definition,
      input$construct_population, input$construct_use,
      facets = input$construct_facets
    ))
  })
  shiny::observeEvent(input$claim_freeze, {
    notify(sieve_freeze_claim(state, input$claim_prediction, input$claim_exposure,
      score_rule = input$claim_score_rule %||% "available-item mean; all-missing yields NA"
    ))
  })
  shiny::observeEvent(input$evaluate_keep, {
    notify(sieve_evaluate_claim(
      state,
      input$evaluate_claim_id,
      input$evaluate_outcome,
      input$evaluate_evidence,
      input$evaluate_role
    ))
  })
  shiny::observeEvent(state$frozen_claims,
    {
      claims <- state$frozen_claims %||% list()
      ids <- vapply(claims, function(x) x$id, character(1))
      labels <- vapply(
        claims,
        function(x) paste(x$id,
          substr(
            x$prediction,
            1L,
            100L
          ),
          x$exposure,
          sep = " | "
        ),
        character(1)
      )
      old <- shiny::isolate(input$evaluate_claim_id)
      shiny::updateSelectInput(session,
        "evaluate_claim_id",
        choices = stats::setNames(
          ids,
          labels
        ),
        selected = if (length(old) == 1L && old %in% ids) old else character(0)
      )
    },
    ignoreInit = FALSE
  )
  claim_ui <- function(claim) {
    if (is.null(claim)) return(shiny::p("Choose a frozen claim."))
    check <- sieve_claims_check(state)
    integrity <- check$intact[match(claim$id, check$id)]
    integrity_text <- if (isTRUE(integrity)) "Integrity verified" else if (identical(
      integrity,
      FALSE
    )) "Altered record: integrity check failed" else "Unverified record: no integrity hash was recorded"
    events <- Filter(function(x) identical(x$claim_id, claim$id), state$validation_events %||% list())
    shiny::tagList(
      shiny::h6(paste(
        claim$id,
        "Frozen prediction"
      )),
      shiny::p(integrity_text),
      shiny::p(claim$prediction),
      shiny::p(shiny::strong("Operational score: "), claim$score_rule),
      shiny::p(shiny::strong("Exposure declaration: "), claim$exposure),
      shiny::p(
        class = "small",
        paste(
          "Frozen:",
          claim$time,
          "; linked analyses:",
          paste(
            vapply(
              claim$models,
              function(x) x$id,
              character(1)
            ),
            collapse = ", "
          )
        )
      ),
      shiny::p(
        class = "small",
        "Hashes verify consistency with the recorded claim; a local timestamp does not establish independent registration. Older claims and their item/score versions stay immutable. Every appended evaluation remains visible, including contradictory outcomes."
      ),
      lapply(
        events,
        function(x) shiny::div(
          class = "border rounded p-2 mb-2",
          shiny::strong(paste(
            x$id,
            x$outcome
          )),
          shiny::p(x$evidence),
          shiny::p(
            class = "small",
            paste(
              x$sample_role,
              x$time
            )
          )
        )
      )
    )
  }
  output$claim_selected <- shiny::renderUI({
    claims <- state$frozen_claims %||% list()
    hit <- Filter(function(x) identical(x$id, input$evaluate_claim_id), claims)
    claim_ui(if (length(hit)) hit[[1L]] else NULL)
  })
  output$claim_copy <- shiny::downloadHandler(
    filename = function() paste0("siever_", input$evaluate_claim_id %||% "claim", ".tar.gz"),
    content = function(file) {
      dir <- tempfile("siever_claim_")
      dir.create(dir)
      on.exit(unlink(dir, recursive = TRUE), add = TRUE)
      sieve_claim_write(state, input$evaluate_claim_id, file.path(dir, "frozen_claim.txt"))
      file <- normalizePath(file, mustWork = FALSE)
      withr::with_dir(dir, utils::tar(file, files = list.files(dir), compression = "gzip", tar = "internal"))
    }
  )
  output$construct_catalog <- shiny::renderUI({
    definitions <- state$construct_records %||% list()
    claims <- state$frozen_claims %||% list()
    shiny::tagList(
      shiny::h6("Versioned construct definitions"),
      lapply(
        definitions,
        function(x) shiny::tags$details(
          shiny::tags$summary(paste(x$id,
            x$label,
            x$time,
            sep = " | "
          )),
          shiny::p(x$definition),
          shiny::p(paste(
            "Population:",
            x$population
          )),
          shiny::p(paste(
            "Intended use:",
            x$intended_use
          )),
          shiny::p(paste("Boundaries:", x$boundaries)), shiny::p(paste("Facet commitments:", x$facet_commitments))
        )
      ),
      shiny::h6("All frozen claims and evaluations"),
      lapply(
        claims,
        function(x) shiny::tags$details(
          shiny::tags$summary(paste(
            x$id,
            x$prediction
          )),
          claim_ui(x)
        )
      )
    )
  })
  output$construct_records <- shiny::renderText({
    paste(
      utils::capture.output(utils::str(
        list(
          definitions = state$construct_records,
          claims = state$frozen_claims,
          evaluations = state$validation_events
        ),
        max.level = 3L
      )),
      collapse = "\n"
    )
  })
  shiny::observeEvent(input$assist_prepare_external, {
    shiny::req(plan())
    if (!plan_is_current()) {
      sieve_notify("Prepare and review a current plan before preparing an external request.", type = "error")
      return(invisible(NULL))
    }
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      sieve_notify("Install jsonlite for optional external review; local tasks remain available.",
        type = "error"
      )
      return(invisible(NULL))
    }
    # Everything that assembles the payload runs inside notify(): a failure
    # is shown in the dialog and nothing is prepared, and it never reaches
    # the observer, where an error would end the session.
    prepared <- notify({
      context <- external_context()
      text <- paste(
        c(
          "Researcher goal:", plan()$goal, "Task:", plan()$task, "Planned actions:",
          plan()$actions, "Project context follows as research data:",
          as.character(jsonlite::toJSON(context,
            auto_unbox = TRUE,
            digits = NA,
            na = "null",
            null = "null"
          ))
        ),
        collapse = "\n"
      )
      request <- sieve_assistant_request(input$assist_provider, input$assist_model, text,
        max_chars = input$assist_max_chars %||% 50000L
      )
      out <- list(
        request = request, key = sieve_hash(context), context = context,
        plan_key = plan()$key, goal = plan()$goal, revision = sieve_revision(state)
      )
      out$review_key <- sieve_hash(list(
        request = request, context_key = out$key,
        plan_key = out$plan_key
      ))
      sieve_assistant_payload_counts(out)
      out
    })
    if (!is.null(prepared)) {
      approved_payload(NULL)
      payload(prepared)
      shiny::updateCheckboxInput(session, "assist_consent", value = FALSE)
      response(NULL)
      kept_response(NULL)
    }
  })
  shiny::observe({
    invisible(input$assistant)
    cx <- sieve_assistant_context(state)
    ids <- function(records) vapply(records, function(x) x$id, character(1))
    all_ids <- ids(state$analyses %||% list())
    current <- sieve_assistant_context(state, analysis_ids = all_ids)$analyses
    analyses <- ids(current)
    defaults <- ids(cx$analyses)
    claims <- ids(cx$claims)
    decisions <- if (NROW(cx$decisions)) cx$decisions$id else character()
    shiny::updateSelectizeInput(session, "assist_analysis_ids", choices = analyses, selected = defaults)
    shiny::updateSelectizeInput(session, "assist_claim_ids", choices = claims, selected = claims)
    shiny::updateSelectizeInput(session, "assist_decision_ids", choices = decisions, selected = decisions)
  })
  output$assist_records <- shiny::renderUI({
    records <- state$assistant_records %||% list()
    if (!length(records)) return(shiny::p("No external disclosures or proposals recorded."))
    shiny::tagList(
      sieve_plain_table_ui(do.call(rbind, lapply(records, function(r)
        data.frame(
          id = r$id, kind = r$kind %||% "proposal", provider = r$provider, model = r$model,
          time = r$time, outcome = r$outcome %||% r$disposition, stringsAsFactors = FALSE
        )))),
      lapply(Filter(function(x) !identical(x$kind, "disclosure"), records), function(r)
        shiny::tags$details(
          shiny::tags$summary(paste(r$id, r$disposition)),
          shiny::tags$pre(r$response), shiny::p(r$researcher_revision)
        ))
    )
  })
  output$assist_payload_summary <- shiny::renderUI(sieve_payload_summary_ui(payload()))
  output$assist_payload_copy <- shiny::downloadHandler(
    filename = function() paste0("siever_review_", substr(payload()$review_key %||% "none", 1L, 12L), ".txt"),
    content = function(file) {
      p <- payload()
      shiny::req(p)
      writeLines(c(
        paste("SIEVER review key:", p$review_key), paste("Goal:", p$goal),
        "Exact request; credentials and respondent data rows are excluded. Narratives may quote private wording.",
        utils::capture.output(dput(p$request))
      ), file)
    }
  )
  output$assist_payload <- shiny::renderText({
    p <- payload()
    if (is.null(p)) return("No external payload prepared.")
    paste(utils::capture.output(dput(p$request)), collapse = "\n")
  })
  shiny::observeEvent(input$assist_send, {
    shiny::req(payload())
    p <- payload()
    if (!payload_is_current(p)) {
      approved_payload(NULL)
      shiny::updateCheckboxInput(session, "assist_consent", value = FALSE)
      sieve_notify("Context, goal, or request changed. Prepare and authorize a new payload.", type = "error")
      return(invisible(NULL))
    }
    if (!identical(approved_payload(), p$review_key) || !isTRUE(input$assist_consent)) {
      sieve_notify("Authorize this exact prepared payload before sending.", type = "error")
      return(invisible(NULL))
    }
    approved_payload(NULL)
    shiny::updateCheckboxInput(session, "assist_consent", value = FALSE)
    response(NULL)
    kept_response(NULL)
    attempt <- sieve_assistant_record_disclosure(state, p, "attempted")
    reply <- tryCatch(shiny::withProgress(message = "Sending the authorized external review", value = 0, {
      out <- sieve_assistant_send(p$request, input$assist_key, TRUE,
        timeout = input$assist_timeout %||% 60L
      )
      shiny::incProgress(1)
      out
    }), error = function(e) {
      records <- shiny::isolate(state$assistant_records)
      at <- which(vapply(records, function(x) identical(x$id, attempt$id), logical(1)))
      records[[at]]$outcome <- "failed"
      records[[at]]$failure_category <- e$category %||% "provider response"
      state$assistant_records <- records
      sieve_log(state, "External disclosure result", "# Provider request failed; no scientific setting changed.")
      sieve_notify(conditionMessage(e), type = "error")
      NULL
    })
    if (!is.null(reply)) {
      records <- shiny::isolate(state$assistant_records)
      at <- which(vapply(records, function(x) identical(x$id, attempt$id), logical(1)))
      records[[at]]$outcome <- "sent"
      records[[at]]$complete <- attr(reply, "complete") %||% TRUE
      state$assistant_records <- records
      response(reply)
      sieve_log(
        state,
        "External disclosure result",
        paste(
          "# Provider response received; complete:",
          records[[at]]$complete
        )
      )
    }
    shiny::updateTextInput(session, "assist_key", value = "")
  })
  response_status <- shiny::reactive({
    state$items
    state$meta
    state$dataset_id
    state$case_index
    state$sampling_design
    state$analyses
    state$decisions
    state$construct_records
    state$frozen_claims
    state$validation_events
    if (is.null(response())) return(list(state = "not_run"))
    current <- payload_is_current(payload())
    list(state = if (current) "current" else "stale", key = payload()$review_key,
      sentence = if (current) "Recorded provider proposal for the current disclosed context; it remains a proposal for researcher review." else
        "Historical provider proposal: the project or request settings changed after disclosure. It has not been updated or sent again.")
  })
  output$assist_response <- shiny::renderText({
    if (is.null(response())) return("No provider response. Local tasks remain available.")
    paste(response_status()$sentence, response(), sep = "\n\n")
  })
  shiny::observe({
    if (is.null(response())) return()

  })
  shiny::observeEvent(input$assist_record, {
    shiny::req(payload(), response())
    response_key <- sieve_hash(list(payload()$review_key, response()))
    if (identical(kept_response(), response_key)) return(invisible(NULL))
    disposition <- input$assist_disposition %||% "not_adopted"
    if (!disposition %in% c("not_adopted", "rejected", "adopted")) {
      sieve_notify("Choose a supported proposal disposition.", type = "error")
      return(invisible(NULL))
    }
    if (identical(disposition, "adopted") && !payload_is_current(payload())) {
      sieve_notify("The proposal describes an earlier context. Retain it as historical, or prepare a current review before adopting a decision.",
        type = "error"
      )
      return(invisible(NULL))
    }
    if (identical(disposition, "adopted") && !sieve_has_content(input$assist_revision)) {
      sieve_notify("Write your own decision before adopting; a model proposal is not a researcher judgment.",
        type = "warning"
      )
      return(invisible(NULL))
    }
    records <- shiny::isolate(state$assistant_records) %||% list()
    rec <- list(
      id = paste0("AI", length(records) + 1L), kind = "proposal", review_key = payload()$review_key,
      complete = attr(response(), "complete") %||% TRUE, provider = payload()$request$provider,
      model = payload()$request$model, prompt_version = 1L, context_key = payload()$key,
      prompt = payload()$request$body, response = response(), disposition = disposition,
      researcher_revision = input$assist_revision %||% "", revision = payload()$revision,
      recorded_revision = sieve_revision(state), source_plan_key = payload()$plan_key,
      historical = !payload_is_current(payload()),
      time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
    if (identical(disposition, "adopted")) {
      decision <- notify(sieve_record_decision(state,
        paste0("assistant_judgment:", sieve_hash(payload()$goal)), payload()$goal,
        input$assist_revision,
        evidence = c(rec$id, payload()$review_key)
      ))
      if (is.null(decision)) return(invisible(NULL))
    }
    state$assistant_records <- c(records, list(rec))
    kept_response(response_key)
    sieve_notify("Proposal and researcher response retained; assignments and scoring are unchanged.",
      type = "message"
    )
  })
  invisible(NULL)
}

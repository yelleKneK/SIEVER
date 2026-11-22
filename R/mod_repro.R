# mod_repro.R
# The reproducible outputs view: a report written from the analyses recorded
# in this session (the primary path: it runs nothing; fct_saved_report.R), a
# manuscript methods-and-results draft assembled by a new analysis run on
# request (fct_report.R), and the annotated R accumulated by sieve_log()
# across the session. Together they are the paper trail of the session: the
# script discloses the computations, the report and the draft disclose the
# decisions in prose (Flake & Fried, 2020), and every reference list is the
# methods actually used (fct_references.R).
#
# sieve_replay_script() binds each recorded step to the data and item ledger
# captured for that step, and verifies each mutation before the next step.
#
# The tab also holds the exports of the study (the Export panel): the data
# file in either kind, the specification, the summary of the included items
# (covariance matrix, means, n), and the project file. Their contracts are
# in fct_export.R and fct_project.R; the Ingest tab is the way in, this tab
# the way out.

#' Reproducible Code Module UI
#' @param id Module id.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny NS downloadButton verbatimTextOutput actionButton p span uiOutput
#' @importFrom shiny radioButtons checkboxInput strong helpText tags tagList
#' @importFrom bslib card card_header navset_card_tab nav_panel
mod_repro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sieve_optional_tab_note("a report written from the analyses recorded in this session, a methods and results draft that runs a new analysis, the annotated session script, and the exports of the study (the data, the specification, the summary of the included items, the project file)"),
    # Results first: the one status line
    # (the methods draft from before, with the page's one Update), then
    # each panel's result before its explanation.
    uiOutput(ns("status")),
    navset_card_tab(
      nav_panel(
        "Report",
        # The primary path: a report written from the analyses recorded in this
        # session, which runs nothing. The draft that runs a new analysis is
        # the second card, under its own name.
        card(
          card_header(
            span("Report saved analyses"),
            shiny::uiOutput(ns("saved_download"), inline = TRUE, class = "float-end")
          ),
          p(
            class = "small text-muted mb-2",
            "Select the analyses to include, using the list or the table. The report uses their saved results, ",
            "states whether their data and item pool are current, and includes the methods and references. ",
            "No models are fitted."
          ),
          shiny::selectInput(ns("saved_ids"), "Analyses to report",
            choices = character(0), multiple = TRUE, width = "100%"
          ),
          DT::DTOutput(ns("analysis_catalog")),
          shiny::uiOutput(ns("report_selection_note")),
          shiny::tags$details(open = NA,
            shiny::tags$summary("Compare Selected Saved Analyses"),
            shiny::uiOutput(ns("saved_comparison"))),
          tags$details(
            class = "mb-2",
            tags$summary("Rename an analysis"),
            shiny::selectInput(ns("rename_id"), "Analysis to rename", choices = c("Choose an analysis" = "")),
            shiny::textInput(ns("analysis_label"), "Analysis label"),
            shiny::actionButton(ns("label_analysis"), "Keep analysis label", class = "btn-outline-primary btn-sm")
          ),
          shiny::uiOutput(ns("saved_readable")),
          sieve_more_below(
            "Current refers to the data and item pool. Check the saved settings against the analysis you intend to report.",
            tagList(
              "Each table row is one saved analysis. Current means its data and item pool still match; ",
              "it does not mean that its settings match every control now shown elsewhere. Earlier ",
              "analyses retain their original inputs and results. The selected analysis below lists ",
              "its specification, input and fitted sample sizes, estimator, missing-data treatment, ",
              "uncertainty method and recorded software versions. The report also includes these details ",
              "and an appendix of the exact specifications and unrounded results."
            ),
            p(
              "References follow the methods actually used, including the interval method and any completed ",
              "bootstrap. Duplicate references are removed, and software citations use the recorded versions. ",
              "If a bibliographic entry could not be verified, the method is named as not listed rather than ",
              "given an unverified citation. This keeps the report tied to what was done (Flake & Fried, 2020)."
            ),
            label = "Report details and references"
          )
        ),
        tags$details(
          tags$summary("Optional draft from a new analysis"),
          card(
            card_header(
              span("Run a new analysis and draft"),
              downloadButton(ns("dl_draft"), "Download draft (.md)", class = "btn-sm float-end")
            ),
            div(
              class = "d-flex align-items-center gap-2 mb-2",
              actionButton(ns("draft_go"), "Run a new analysis and draft", class = "btn-primary btn-sm")
            ),
            div(style = "max-height: 62vh; overflow-y: auto;", verbatimTextOutput(ns("draft"))),
            sieve_more_below(
              tagList(
                "This draft runs a new measurement analysis on the current included items. It does not ",
                "summarize the analyses selected above. Square brackets mark text for you to supply, ",
                "including the sample description, retention rationale and interpretation. Edit the draft before use."
              ),
              p(
                "The draft holds the sample, with the respondents removed ",
                "on the Screening tab and their reasons, the measurement design, the estimator ",
                "and how it treats incomplete cases, the CFA fit with the RMSEA interval and the ",
                "unbiased SRMR with its interval, ",
                "and per-construct reliability with its case treatment, n and interval. The ",
                "unbiased SRMR is computed whatever its estimated time, which grows with the cube ",
                "of the number of residual covariances, unless the CFA tab already computed it (below); ",
                "the button states that time when it exceeds two seconds. The ",
                "draft refits its own model: the correlated-factors CFA the ledger implies, ",
                "with the study's estimator, on the data as they stand when the button is pressed, ",
                "behind the same memory refusal as the CFA page. ",
                "Reliability is the coefficient the Reliability tab reports by default ",
                "(Robust omega (observed denominator), with Omega (model-implied denominator) beside it, ",
                "for continuous items; for ordered or binary items, the observed denominator built from ",
                "the unrestricted polychoric correlations), on the items as that tab's item ",
                "treatment takes them, with the study estimator's treatment of incomplete cases. ",
                "The draft does not read the results shown on the CFA tab or any other, with two ",
                "exceptions, each taken from a record of the same computation on the same data and ",
                "named where it appears: a bootstrap interval the Reliability tab ran for the same ",
                "request, and the unbiased SRMR, or DMAR's refusal of it, the CFA tab computed for ",
                "the identical fit (the same model, data, respondents and engines). It does not ",
                "reproduce an analysis run ",
                "earlier in this session with other settings or another item pool; it names ",
                "the data and ledger revision it used, and its references are the methods it ",
                "computed with. The status line above says when the draft is from before (the data, ",
                "the item pool, the study estimator, the sampling design, a Reliability setting it ",
                "follows, or a bootstrap completed since), and the page's Update assembles it again."
              ),
              label = "More on the draft: what it contains, the model it refits, and when it is from before"
            )
          )
        )
      ),
      nav_panel(
        "Session script",
        card(
          card_header(
            span("Annotated session script"),
            downloadButton(ns("dl"), "Download script and inputs (.tar.gz)", class = "btn-sm float-end")
          ),
          uiOutput(ns("code_line")),
          # The session's sharing choice (fct_code_view.R): a Download this
          # analysis bundle carries the recorded input view only when this
          # is ticked; the choice belongs to the session, not the project.
          shiny::checkboxInput(ns("share_input_view"),
            paste(
              "Include recorded input data in Download this analysis bundles.",
              "Check the study's sharing arrangements first. This choice applies only to this session."
            ),
            value = FALSE, width = "100%"
          ),
          # The script scrolls in its own box, so what it is and how to
          # share it stays in view under it.
          div(style = "max-height: 62vh; overflow-y: auto;", verbatimTextOutput(ns("code"))),
          sieve_more_below(
            "Unpack the script and inputs together, then run the script from that folder. The inputs contain research data.",
            p(
            "Each action records its equivalent R and a short explanation. Download the script ",
            "with its companion input file, unpack them together, and run the script from that ",
            "directory. Source data are stored once and checked by their content hash. Each ",
            "operation uses its recorded input revision; a replay stops if its item data differ ",
            "from the recorded output. Steps recorded without their inputs remain labeled ",
            "historical comments. The input file contains research data, including reversible ",
            "screening history; apply the study's sharing arrangements before distributing it. ",
            "The script names respondents removed on the Screening tab by their row of the data ",
            "as loaded, never by an identifier, and carries the reason typed for each removal as ",
            "a comment; read those reasons before sharing the script."
            ),
            label = "Replaying and sharing the script"
          )
        )
      ),
      nav_panel(
        "Export",
        card(
          card_header("Export the study"),
          sieve_more_below(
            "Download the data and specification as a pair. Use the sharing bundle for a restricted copy, or Save project for a private backup.",
            p(
            "The way out of a session. The data file and the specification are a pair: the ",
            "specification has one row per variable with its role (and, for a criterion, its ",
            "validity kind) and, for an item, its name, wording, type, construct, facet, included ",
            "or excluded status, keying direction, the response bounds a reversal is made within, and ",
            "whether the data file already holds the item reverse scored. The summary holds the ",
            "covariance matrix of the included items and, when asked for, their means, with the ",
            "number of complete cases, for an analysis that starts from moments. The project file ",
            "holds the whole session and opens through Open project in the project bar or Ingest."
            ),
            label = "What each export contains"
          ),
          radioButtons(
            ns("export_values"), "Values in the data file",
            choices = c(
              "Raw: the values as the file was read (no reverse scoring made here is applied)" = "raw",
              "Processed: the values the analyses use (an item you reverse scored is written reverse scored)" = "processed"
            ),
            selected = "raw"
          ),
          helpText(
            "Both downloads follow this choice and say which in their names; the label of a reverse ",
            "scored variable in a processed file says so, with the bounds."
          ),
          # Respondents removed on the Screening tab are not in either file.
          uiOutput(ns("export_rows_note")),
          downloadButton(ns("dl_data"), "Download data (.sav)", class = "btn-outline-primary btn-sm mb-2"),
          downloadButton(ns("dl_spec"), "Download specification (.csv)", class = "btn-outline-primary btn-sm mb-2"),
          downloadButton(ns("dl_bundle"),
            "Download matched study bundle (.tar.gz)",
            class = "btn-outline-primary btn-sm mb-2"
          ),
          helpText("The sharing bundle contains item columns only in data.csv, an analytic specification without private wording or notes, a processed complete-case covariance summary, a fresh sharing project, and an exact revision manifest. Removed respondents, identifier and auxiliary columns, narratives, analysis history, and external-review records are excluded from every file in this bundle. The separate Save project download is a complete private backup; inspect it before sharing."),
          tags$hr(),
          strong("Summary"),
          helpText(
            "The covariance matrix of the included items' processed values on the rows complete on ",
            "every included item, written as a comma separated file whose header comment names the ",
            "basis and the n; the specification above describes the items. When the analysis basis ",
            "of the included items is polychoric or tetrachoric (ordered or binary items), that ",
            "correlation matrix follows the covariance rows, one row per item named polychoric.<item> ",
            "(or tetrachoric, or mixed), and the file name says so; with Pearson correlations the ",
            "covariance matrix holds everything, so nothing is added."
          ),
          checkboxInput(ns("summary_means"), "Include the means (a row named mean after the matrix)", value = TRUE),
          downloadButton(ns("dl_summary"), "Download summary (.csv)", class = "btn-outline-primary btn-sm mb-2"),
          tags$hr(),
          strong("Project"),
          helpText(
            "A project file holds the whole session: the data as loaded, the item data, the item ",
            "ledger, the variable roles, the transformation records, the analysis registry and the ",
            "session script. Use the same project name and acknowledge the prepared copy in the project bar."
          )
        )
      )
    )
  )
}





#' Reproducible Code Module Server
#' @param id Module id.
#' @param state The shared application state.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny moduleServer reactive renderText downloadHandler eventReactive req validate need
mod_repro_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    report_ids <- shiny::reactiveVal(character(0))
    rename_target <- shiny::reactiveVal("")
    current_context <- reactive({
      lapply(c("analyses", "analysis_labels", "dataset_id", "case_index", "meta", "items",
        "raw", "roles", "construct_basis", "sampling_design", "scoring_policies", "holdout"),
        function(name) state[[name]])
      invisible(NULL)
    })
    catalog <- reactive({
      current_context()
      invisible(state$analyses)
      invisible(state$analysis_labels)
      invisible(state$dataset_id)
      invisible(state$case_index)
      invisible(state$meta)
      invisible(state$items)
      sieve_analysis_catalog(state)
    })
    shiny::observeEvent(catalog(),
      {
        x <- catalog()
        selected <- shiny::isolate(report_ids())
        labels <- stats::setNames(x$id, paste(x$id, x$label, x$revision, sep = " | "))
        shiny::updateSelectInput(session,
          "saved_ids",
          choices = labels,
          selected = intersect(selected, x$id)
        )
        target <- shiny::isolate(rename_target())
        shiny::updateSelectInput(session, "rename_id",
          choices = c("Choose an analysis" = "", labels),
          selected = if (target %in% x$id) target else ""
        )
      },
      ignoreInit = FALSE
    )
    shiny::observeEvent(input$saved_ids, {
      report_ids(unique(as.character(input$saved_ids %||% character(0))))
    }, ignoreNULL = FALSE)
    # Analysis IDs belong to a project. A restored project may also use
    # A1, so it must not inherit the previous project's report selection.
    shiny::observeEvent(state$dataset_id, {
      report_ids(character(0))
      rename_target("")
      shiny::updateSelectInput(session, "saved_ids", selected = character(0))
      shiny::updateSelectInput(session, "rename_id", selected = "")
      shiny::updateTextInput(session, "analysis_label", value = "")
    }, ignoreInit = TRUE, priority = 100)
    output$analysis_catalog <- DT::renderDT({
      x <- catalog()
      summary_columns <- c("id", "label", "scope", "status", "revision", "time")
      titles <- c(id = "ID", label = "Analysis", scope = "Scope", status = "Result", revision = "Inputs", time = "Run at (UTC)")
      headings <- names(x)
      headings[match(names(titles), names(x))] <- unname(titles)
      sieve_dt(x,
        rownames = FALSE,
        colnames = headings,
        selection = list(mode = "multiple", selected = which(x$id %in% shiny::isolate(report_ids()))),
        escape = TRUE,
        options = list(
          dom = "fltip", pageLength = 10, scrollX = TRUE, order = list(),
          columnDefs = list(list(visible = FALSE, targets = which(!names(x) %in% summary_columns) - 1L))
        )
      )
    })
    catalog_proxy <- DT::dataTableProxy("analysis_catalog", session = session)
    shiny::observeEvent(input$analysis_catalog_rows_selected, {
      x <- catalog()
      rows <- as.integer(input$analysis_catalog_rows_selected)
      rows <- rows[!is.na(rows) & rows >= 1L & rows <= nrow(x)]
      ids <- x$id[rows]
      current <- shiny::isolate(report_ids())
      # A proxy update can echo the same selection in catalog order.
      # Preserve the researcher's list order when the selected set agrees.
      if (setequal(ids, intersect(current, x$id))) return(invisible(NULL))
      ids <- c(intersect(current, ids), setdiff(ids, current))
      report_ids(ids)
      shiny::updateSelectInput(session, "saved_ids", selected = ids)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    shiny::observeEvent(report_ids(), {
      ids <- report_ids()
      DT::selectRows(catalog_proxy, which(catalog()$id %in% ids))
    }, ignoreNULL = FALSE)
    shiny::observeEvent(input$rename_id, {
      x <- catalog()
      at <- match(input$rename_id %||% "", x$id)
      rename_target(if (is.na(at)) "" else x$id[[at]])
      shiny::updateTextInput(session, "analysis_label", value = if (is.na(at)) "" else x$label[[at]])
    }, ignoreNULL = FALSE)
    shiny::observeEvent(input$label_analysis, {
      id <- rename_target()
      shiny::req(id %in% names(shiny::isolate(state$analyses)), nzchar(trimws(input$analysis_label %||% "")))
      labels <- as.list(shiny::isolate(state$analysis_labels) %||% list())
      labels[[id]] <- trimws(input$analysis_label)
      state$analysis_labels <- labels
    })
    output$report_selection_note <- shiny::renderUI({
      ids <- report_ids()
      if (!length(ids)) {
        return(shiny::p(class = "small text-muted", "Select at least one analysis to preview or download a report."))
      }
      shiny::p(class = "small text-muted", sprintf(
        "%d %s selected. To leave one out, select its table row again or remove it from the list.",
        length(ids), if (length(ids) == 1L) "analysis" else "analyses"
      ))
    })
    output$saved_download <- shiny::renderUI({
      if (!length(report_ids())) return(NULL)
      shiny::downloadButton(session$ns("dl_saved"), "Download report (.txt)", class = "btn-sm")
    })
    output$saved_comparison <- shiny::renderUI({
      current_context()
      ids <- report_ids()
      if (length(ids) < 2L) return(shiny::p(class = "small", "Select at least two saved analyses to compare their specifications and results."))
      sieve_analysis_comparison_ui(sieve_analysis_comparison(state, ids))
    })
    # The report on the page: each selected record as sieve_record_lines()
    # describes it (nothing is refit; an absent record is listed as absent),
    # its stored results, and the references of the methods the selected
    # records used.
    output$saved_readable <- shiny::renderUI({
      current_context()
      ids <- report_ids()
      records <- state$analyses %||% list()
      state$analysis_labels
      # Each record says whether it describes the data now in the analysis;
      # sieve_revision() reads in isolation, so the dependency is taken here.
      state$dataset_id
      state$case_index
      state$items
      state$meta
      now <- sieve_revision(state)
      present <- intersect(ids, names(records))
      blocks <- lapply(ids, function(id) {
        if (!id %in% names(records)) {
          absent <- sieve_record_absent_lines(id)
          return(shiny::tags$details(
            shiny::tags$summary(absent[1L]),
            shiny::p(class = "small", absent[2L])
          ))
        }
        r <- records[[id]]
        head <- sieve_record_lines(r, state, now)
        tables <- if (is.null(r$estimates)) list() else sieve_readable_result_tables(r$estimates)
        shiny::tags$details(
          shiny::tags$summary(head[1L]),
          lapply(head[-1L], function(line) shiny::p(class = "small mb-1", line)),
          if (!length(tables)) {
            shiny::p(class = "small", "Results: none stored (see the status above); nothing was refit.")
          },
          lapply(names(tables), function(nm) shiny::tagList(shiny::strong(nm), sieve_plain_table_ui(tables[[nm]])))
        )
      })
      references <- if (length(present)) {
        used <- sieve_records_methods(records[present])
        lines <- sieve_reference_lines(used$keys, versions = used$versions, markdown = FALSE)
        shiny::tags$details(
          shiny::tags$summary("References of the selected analyses"),
          lapply(lines[-1L][nzchar(lines[-1L])], function(line) shiny::p(class = "small mb-1", line))
        )
      }
      shiny::tagList(blocks, references)
    })
    output$dl_saved <- downloadHandler(
      filename = function() "siever_report_recorded_analyses.txt",
      content = function(file) {
        ids <- report_ids()
        shiny::validate(shiny::need(length(ids) > 0L, "Select at least one analysis to download a report."))
        writeLines(sieve_readable_saved_report(state, ids), file)
      }
    )
    # ---- Export: the data file, the specification, the summary, the project
    # The contract is in fct_export.R. Both the data file and the
    # specification follow one choice of which values the data file holds,
    # raw by default, and both say which in their names.
    export_kind <- function() {
      kind <- input$export_values
      if (is.null(kind) || !kind %in% sieve_export_kinds()) "raw" else kind
    }

    # Respondents removed from the analysis (Screening tab) are left out of
    # the data file and the summary; the panel says how many.
    output$export_rows_note <- renderUI({
      removed <- sieve_removed_table(state)
      n <- if (is.data.frame(removed)) nrow(removed) else 0L
      if (!n) {
        return(NULL)
      }
      p(
        class = "small text-muted",
        sprintf(
          paste0(
            "%d respondent%s removed from the analysis on the Screening tab %s not in the ",
            "data file or the summary; restore %s there to export every row as read. The data ",
            "file begins with original_row, each row's number in the data as loaded."
          ),
          n, if (n == 1L) "" else "s", if (n == 1L) "is" else "are", if (n == 1L) "it" else "them"
        )
      )
    })

    output$dl_spec <- downloadHandler(
      filename = function() sprintf("siever_specification_%s.csv", export_kind()),
      content = function(file) {
        req(state$meta, state$raw, state$items)
        sieve_spec_write(
          sieve_spec_table(state$raw, state$items, state$meta, state$roles, values = export_kind()),
          file
        )
      }
    )

    # The data with the Item Name attached as each variable's label, keeping
    # syntactic column names so the file stays valid for lavaan, psych, SPSS,
    # and Stata alike. The label of an item that is reverse scored in the
    # file says so.
    output$dl_data <- downloadHandler(
      filename = function() sprintf("siever_data_%s.sav", export_kind()),
      content = function(file) {
        validate(need(sieve_have("haven"), "haven is needed to write a labeled .sav file."))
        req(state$raw, state$meta, state$items)
        haven::write_sav(
          sieve_export_data(state$raw, state$items, state$meta,
            values = export_kind(),
            rows = state$case_index
          ),
          file
        )
      }
    )

    # The summary: always of the processed values of the included items,
    # and, when the analysis basis of the included items is polychoric,
    # tetrachoric or mixed (ordered or binary items), that matrix as well
    # (sieve_summary_table(); the basis is the shared one, computed once
    # per item pool and kept, so a tab that used it already paid for it).
    summary_basis <- function() {
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) {
        return(NULL)
      }
      sieve_state_basis(state)
    }
    summary_block <- function(basis) {
      if (is.list(basis) && isTRUE(basis$ok) && basis$used %in% c("polychoric", "tetrachoric", "mixed")) {
        basis$used
      } else {
        NULL
      }
    }
    summary_file <- function(block = summary_block(summary_basis())) {
      sprintf(
        "siever_summary_processed%s%s.csv",
        if (isTRUE(input$summary_means)) "_with_means" else "",
        if (is.null(block)) "" else paste0("_", block)
      )
    }
    output$dl_summary <- downloadHandler(
      filename = function() summary_file(),
      content = function(file) {
        req(state$meta, state$items)
        means <- isTRUE(input$summary_means)
        basis <- summary_basis()
        summary <- sieve_summary_table(state$items, state$meta, means = means, basis = basis)
        block <- if (is.null(summary$cor)) NULL else summary$cor_kind
        sieve_summary_write(summary, file, dataset = state$dataset_name)
        sieve_log(
          state,
          sprintf(
            "Export the summary of the %d included items (covariance matrix%s%s, n = %d complete cases)",
            length(summary$items), if (means) " and means" else "",
            if (is.null(block)) "" else sprintf(" and the %s correlation matrix", block), summary$n
          ),
          sieve_summary_code(means, summary_file(block), cor = block)
        )
      }
    )

    output$dl_bundle <- downloadHandler(
      filename = function() paste0(
        sub(
          "[.]siever[.]rds$",
          "",
          sieve_project_filename(state$project_name)
        ),
        "_",
        export_kind(),
        "_study.tar.gz"
      ),
      content = function(file) {
        req(state$meta)
        snapshot <- sieve_project_snapshot(state)
        sieve_export_study_bundle(snapshot, file, values = export_kind(), means = isTRUE(input$summary_means))
      }
    )
    output$save_project <- downloadHandler(
      filename = function() sieve_project_filename(state$project_name),
      content = function(file) {
        validate(need(!is.null(state$meta), "Load data first: there is no project to save yet."))
        sieve_prepare_project_copy(state, file)
      }
    )

    script <- reactive({
      invisible(state$codelog)
      invisible(state$replay_steps)
      sieve_replay_script(state)
    })
    output$code <- renderText({
      invisible(state$codelog)
      invisible(state$replay_steps)
      sieve_replay_script(state, data_file = "siever_inputs.rds")
    })
    # The one line above the script: what it holds.
    output$code_line <- renderUI({
      steps <- sum(startsWith(as.character(state$codelog %||% character(0)), "# ["))
      div(
        class = "small text-muted mb-2",
        sprintf(
          paste(
            "The R each action of this session ran, %d recorded step%s with a short explanation each,",
            "with the preamble that rebuilds the objects the steps read; it runs from its companion",
            "input file (siever_inputs.rds)."
          ),
          steps, if (steps == 1L) "" else "s"
        )
      )
    })
    output$dl <- downloadHandler(
      filename = function() "siever_session.tar.gz",
      content = function(file) {
        directory <- tempfile("siever_session_")
        dir.create(directory)
        on.exit(unlink(directory, recursive = TRUE), add = TRUE)
        sieve_session_write(state, directory)
        writeLines(c(
          "Unpack this archive into a private directory and set R's working directory there.",
          "Install SIEVER 0.9.0 or later and the compatible DMAR 1.1.0 or later build identified by the recorded calls.",
          "Run source('siever_session.R'). The script checks the companion input file's hash.",
          "The companion file contains research data, including respondents retained for reversible screening.",
          "The script names removed respondents by their row of the data as loaded and carries the reason typed for each removal as a comment.",
          "Apply the study's sharing arrangements before distributing this archive."
        ), file.path(directory, "README.txt"))
        old <- setwd(directory)
        on.exit(setwd(old), add = TRUE, after = FALSE)
        utils::tar(file,
          files = c("siever_session.R", "siever_inputs.rds", "README.txt"),
          compression = "gzip", tar = "internal"
        )
      }
    )

    # The methods draft refits the CFA and the omegas, so it is computed on an
    # explicit button press rather than reactively.
    # The draft's numbers describe everything it read at the press that
    # assembled it: the dataset, the item data and the ledger, and also the
    # study estimator, the sampling design, the Reliability settings it
    # follows and the bootstrap records it cited (sieve_draft_inputs()).
    # That snapshot is kept, and once any of it changes the draft is
    # withheld, the line names what changed, and the page's Update assembles
    # it again, so a draft of earlier inputs is never shown or downloaded as
    # current.
    draft_made <- reactiveVal(NULL)
    draft_seconds <- reactiveVal(NA_real_)
    draft_stale <- reactive({
      state$dataset_id
      state$items
      state$meta
      state$estimator_choice
      state$sampling_design
      state$raw
      state$reliability_settings
      state$analyses
      made <- draft_made()
      if (is.null(made)) {
        return(NULL)
      }
      sieve_draft_stale_reason(made, sieve_draft_inputs(state))
    })
    # Refresh draft, as a function: its button and the page's Update run
    # it, and the draft is kept until the next run.
    draft_store <- reactiveVal(NULL)
    draft_compute <- function() {
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      # Data that arrived without an identity (a state assembled by hand)
      # are given one first, as every kept result does (sieve_bind_result()).
      sieve_ensure_dataset(state)
      draft_made(sieve_draft_inputs(state))
      problem <- sieve_sampling_problem(state, "independent")
      if (!is.null(problem)) return(paste("Draft not computed.", problem,
        "Use the report of saved analyses for a model that supports the declared design.",
        sieve_assistant_disclosure_report(state),
        sep = "\n\n"
      ))
      # The reliability is the Reliability tab's default coefficient on its
      # item treatment, with a bootstrap interval only when that tab ran it
      # on these data; the
      # respondents removed on the Screening tab are counted with their
      # reasons.
      out <- sieve_methods_draft(state$items, state$meta,
        dataset_name = state$dataset_name %||% "the data",
        revision = sieve_revision(state),
        estimator = sieve_estimator_effective("study", state),
        sampling_design = sieve_sampling_spec(state),
        reliability = state$reliability_settings,
        removed = sieve_removed_table(state),
        analyses = state$analyses,
        scores = sieve_score_specification(state)
      )
      sieve_log(
        state,
        "Assemble the methods and results draft (computed from the included items, apart from two results taken from records, which the draft names).",
        c(
          "# See the Methods draft panel. The draft refits the correlated-factors CFA",
          "# with the study's estimator on the current data and recomputes the Reliability",
          "# tab's default reliability coefficient. Apart from two results, each taken from",
          "# a record of the same computation on the same data and named in the draft (a",
          "# bootstrap interval the Reliability tab ran for the same request, and the unbiased",
          "# SRMR, or DMAR's refusal of it, the CFA tab computed for the identical fit: the",
          "# same model, data, respondents and engines), it does not read the fits logged above."
        )
      )
      paste(out, sieve_assistant_disclosure_report(state), sep = "\n\n")
    }
    # What the draft's unbiased SRMR will cost, read without fitting: the
    # button states the estimate when the draft will compute it and it
    # exceeds the run policy's two seconds, as the CFA page's button does
    # (sieve_draft_usrmr_plan()).
    draft_plan <- reactive({
      req(isTRUE(sieve_confirmation_status(state)$confirmed), state$items, state$meta)
      sieve_draft_usrmr_plan(state$items, state$meta,
        estimator = sieve_estimator_effective("study", state),
        revision = sieve_revision(state), analyses = state$analyses
      )
    })
    observe({
      plan <- tryCatch(draft_plan(), error = function(e) NULL)
      updateActionButton(session, "draft_go", label = sieve_draft_button_label(plan))
    })
    run_draft <- function() {
      started <- proc.time()[["elapsed"]]
      plan <- tryCatch(shiny::isolate(draft_plan()), error = function(e) NULL)
      draft_store(shiny::withProgress(
        message = "Refitting the CFA and recomputing reliability for the draft",
        detail = if (isTRUE(plan$slow)) {
          sprintf("The unbiased SRMR alone is estimated to take %s.", sieve_duration_label(plan$seconds))
        },
        value = 0.5,
        draft_compute()
      ))
      draft_seconds(proc.time()[["elapsed"]] - started)
    }
    observeEvent(input$draft_go, run_draft())
    draft <- reactive({
      kept <- draft_store()
      req(kept)
      kept
    })

    # The session's sharing choice (sieve_sharing_set()): whether an
    # analysis download may carry the recorded input view.
    observeEvent(input$share_input_view, {
      sieve_sharing_set(state, input_view = isTRUE(input$share_input_view))
    }, ignoreInit = TRUE)

    # The page's one status line: shown when the methods draft is from
    # before (the session script is never stale: it is the record), with
    # the page's one Update, which assembles the draft again.
    page_update <- sieve_update_wire(input, session, function() {
      if (is.null(draft_store())) {
        return(list())
      }
      why <- draft_stale()
      list(draft = sieve_update_entry(
        "Methods draft", if (is.null(why)) "current" else "stale",
        sentence = why, seconds = draft_seconds(), run = run_draft, shown = FALSE
      ))
    })
    output$status <- renderUI(page_update$line() %||% page_update$note())
    # The draft's status for the page guide, with the page's one Update
    # when it is from before (it reads only what is kept).
    observe({
      if (is.null(draft_store())) return()
      why <- draft_stale()
      sieve_page_publish(state, "repro", session$ns("draft"), "Methods draft",
        list(state = if (is.null(why)) "current" else "stale", sentence = why, key = draft_made()$revision),
        if (is.null(why)) "Run a new analysis and draft" else "Update", own = TRUE
      )
    })
    output$draft <- renderText({
      d <- tryCatch(draft(), error = function(e) NULL)
      if (is.null(d)) {
        return(paste(
          "Press Run a new analysis and draft to refit the correlated-factors CFA, recompute",
          "reliability and assemble the methods and results draft from the current analysis;",
          "or write a report from the analyses already recorded, above, which runs nothing."
        ))
      }
      why <- draft_stale()
      if (!is.null(why)) {
        return(sprintf(paste(
          "The draft assembled earlier is not shown: %s, so its numbers no longer describe",
          "the analysis. Press Update, or Run a new analysis and draft, to assemble it from",
          "the current analysis."
        ), why))
      }
      d
    })
    output$dl_draft <- downloadHandler(
      filename = function() "siever_methods_draft.md",
      content = function(file) {
        d <- tryCatch(draft(), error = function(e) NULL)
        req(!is.null(d))
        why <- draft_stale()
        if (!is.null(why)) {
          stop(sprintf("The draft was assembled from earlier inputs (%s). Press Run a new analysis and draft first.", why),
            call. = FALSE
          )
        }
        writeLines(d, file)
      }
    )
  })
}

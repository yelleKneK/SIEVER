# fct_diagram_controls.R
# The path diagram panel shared by the CFA and SEM tabs. Both tabs build a
# diagram specification once per fitted model (sieve_diagram_spec()), apply
# the display controls to it without the fitted model
# (sieve_diagram_update()), draw it on screen (sieve_draw_diagram()), and
# write the same specification to PDF, SVG, or PNG
# (sieve_export_diagram()). Nothing here fits a model, so no display
# control can refit one.

#' Display controls of the path diagram
#'
#' The sidebar controls shared by the CFA and SEM tabs. Every control
#' changes only what is drawn; none of them reaches the fitted model.
#'
#' @param ns The namespace function of the calling module.
#' @param kind `"cfa"` or `"sem"`: the wording of the covariance controls.
#' @param layout The layout selected at the start.
#' @return A tag list.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny tagList tags selectInput checkboxInput sliderInput radioButtons
#' @importFrom shiny conditionalPanel numericInput
sieve_diagram_controls <- function(ns, kind = c("cfa", "sem"),
                                   layout = c("top_down", "left_right")) {
  kind <- match.arg(kind)
  layout <- match.arg(layout)
  tagList(
    tags$hr(),
    tags$h6("Path diagram"),
    selectInput(
      ns("what"), "Coefficients shown",
      choices = c(
        "Fully standardized (std.all)" = "std.all",
        "Latent variables standardized (std.lv)" = "std.lv",
        "Unstandardized" = "none"
      ),
      selected = "std.all"
    ),
    checkboxInput(
      ns("resid"),
      if (kind == "cfa") {
        "Show unique (error) terms"
      } else {
        "Show residual terms (unique terms and disturbances)"
      },
      value = TRUE
    ),
    checkboxInput(ns("vars"), "Show variances", value = TRUE),
    checkboxInput(
      ns("covs"),
      if (kind == "cfa") "Show factor covariances" else "Show covariances among predictors",
      value = TRUE
    ),
    checkboxInput(
      ns("rescovs"),
      if (kind == "cfa") "Show residual covariances" else "Show residual (disturbance) covariances",
      value = TRUE
    ),
    checkboxInput(ns("fixed_zero"),
      "Show parameters fixed at zero (such as the covariances of orthogonal factors)",
      value = FALSE
    ),
    checkboxInput(ns("intercepts"), "Show intercepts and thresholds", value = FALSE),
    sliderInput(ns("min_path"), "Hide loadings below |x| (loadings only)",
      min = 0, max = 0.9, value = 0, step = 0.05
    ),
    radioButtons(ns("labels"), "Item labels",
      choices = c("Full names" = "names", "Short IDs with a key" = "short"),
      selected = "names"
    ),
    radioButtons(ns("layout"), "Layout",
      choices = c("Top-down" = "top_down", "Left-right" = "left_right"),
      selected = layout
    ),
    checkboxInput(ns("mono"), "Black and white", value = FALSE),
    checkboxInput(ns("fit_width"), "Fit to panel, with nominal labels at least 8 pt (scroll when needed)",
      value = TRUE
    ),
    numericInput(ns("diagram_zoom"), "Screen zoom (%)", value = 100, min = 50, max = 300, step = 25),
    shiny::actionButton(ns("diagram_readable"), "Use readable full size"),
    shiny::actionButton(ns("diagram_short_ids"), "Use short IDs and key")
  )
}

#' The path diagram panel
#'
#' Status, plot, caption, downloads, label key, and the matching parameter
#' table, in that order.
#'
#' @param ns The namespace function of the calling module.
#' @param kind Measurement or structural diagram controls.
#' @param layout Initial diagram orientation.
#' @return A tag list.
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny uiOutput plotOutput div
#' @importFrom DT DTOutput
sieve_diagram_panel <- function(ns, kind = c("cfa", "sem"), layout = "top_down") {
  kind <- match.arg(kind)
  tagList(
    sieve_settings("Diagram Appearance", sieve_diagram_controls(ns, kind, layout)),
    uiOutput(ns("diagram_status")),
    div(
      style = "text-align: center; overflow-x: auto;",
      plotOutput(ns("diagram"), height = "auto")
    ),
    # The notices about the display (the label size, the size of a large
    # figure) in one line directly under the figure, so they read with the
    # diagram they describe.
    uiOutput(ns("diagram_readability")),
    uiOutput(ns("diagram_caption")),
    sieve_settings("File Settings", ids = ns(c("fig_auto", "fig_width", "fig_height", "dl_caption")),
    checkboxInput(ns("fig_auto"), "Size the file to its content", value = TRUE),
    conditionalPanel(
      condition = "!input.fig_auto", ns = ns,
      numericInput(ns("fig_width"), "File width (inches)",
        value = NULL, min = 2, max = 60,
        step = 0.5
      ),
      numericInput(ns("fig_height"), "File height (inches)",
        value = NULL, min = 2, max = 60,
        step = 0.5
      ),
      tags$p(
        class = "text-muted small",
        "Give one of the two and the other follows from the proportions of the figure."
      )
    ),
    checkboxInput(ns("dl_caption"), "Write the caption into the file", value = TRUE)
    ),
    uiOutput(ns("diagram_downloads")),
    uiOutput(ns("diagram_key")),
    tags$h6(class = "mt-3", "Fitted Model Parameters"),
    tags$p(
      class = "text-muted small",
      "One row per parameter of the fitted model, at the group shown: the estimate with its ",
      "standard error and confidence interval, and the standardized value with its interval. ",
      "Saturated auxiliary parameters omitted from the drawing are included here. ",
      "The diagram shows point estimates; their uncertainty is here. A parameter that was ",
      "fixed, or that is implied by the model, has no standard error."
    ),
    DTOutput(ns("diagram_params"))
  )
}

#' The notices about the path diagram's display, in one line
#'
#' The size of the node labels on screen and in the file, the advice when
#' the screen reduction takes them below eight points, and the size of a
#' figure that is more than 17 inches on its longer side
#' (`sieve_diagram_export_size()`), for the line under the figure: a
#' short line, and the full sentences for its tooltip.
#'
#' @param pt The node label size on screen, in points.
#' @param export_pt The node label size at the export dimensions, in points.
#' @param note The size note of the figure (`spec$size$note`, set when it is
#'   more than 17 inches on its longer side at its recommended size), or
#'   `NULL`.
#' @return A list with `short` (the line) and `full` (the sentences).
#' @author Ken Kelley
#' @noRd
sieve_diagram_notice_text <- function(pt, export_pt, note = NULL) {
  small <- is.finite(pt) && pt < 8
  large <- length(note) == 1L && nzchar(note)
  short <- paste0(
    sprintf("Labels about %.1f pt on screen, %.1f pt in the file", pt, export_pt),
    if (small) "; below 8 pt: use readable full size, zoom, or short IDs",
    if (large) "; over 17 inches at its recommended size",
    "."
  )
  full <- paste0(
    sprintf(
      paste(
        "Nominal node label size: about %.1f points on screen; about %.1f points at the selected export",
        "dimensions. Long labels may be reduced further within nodes."
      ),
      pt, export_pt
    ),
    if (small) {
      paste(
        " Fit-to-panel reduction is below the 8-point presentation threshold. Use readable full size,",
        "increase screen zoom, or use short IDs with the key. This threshold concerns readability, not",
        "model quality."
      )
    },
    if (large) paste0(" ", note)
  )
  list(short = short, full = full)
}
#' Item labels of the ledger, for the diagram
#'
#' @param meta The item ledger.
#' @return A named character vector (item to label) of the included items
#'   whose label differs from the variable name, or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_diagram_item_labels <- function(meta) {
  active <- meta[meta$status == "active", , drop = FALSE]
  labels <- stats::setNames(as.character(active$label), active$item)
  labels <- labels[!is.na(labels) & nzchar(labels) & labels != names(labels)]
  if (length(labels)) labels else NULL
}

#' The parameter table shown with a diagram
#'
#' The parameters of a specification as a display table. Values are kept at
#' full precision here; the table widget rounds them for display.
#'
#' @param spec A diagram specification.
#' @return A data frame with one row per parameter.
#' @author Ken Kelley
#' @noRd
sieve_diagram_parameter_table <- function(spec) {
  p <- spec$parameters
  scale <- if (identical(spec$meta$standardized, "std.lv")) "std_lv" else "std_all"
  status <- ifelse(p$derived, "implied", ifelse(p$fixed, "fixed", "estimated"))
  status <- ifelse(is.na(p$constraint), status, sprintf("%s, equal [%s]", status, p$constraint))
  data.frame(
    Parameter = paste(p$lhs, p$op, p$rhs),
    Type = p$type,
    Status = status,
    Estimate = p$est, SE = p$se, CI.lower = p$ci_lower, CI.upper = p$ci_upper,
    Standardized = p[[scale]],
    Std.CI.lower = p[[paste0(scale, "_ci_lower")]],
    Std.CI.upper = p[[paste0(scale, "_ci_upper")]],
    Flag = ifelse(p$improper, "out of range", ""),
    stringsAsFactors = FALSE
  )
}

#' The R that regenerates a displayed diagram and writes it to a file
#'
#' @param spec The displayed specification.
#' @param fit_name The name the fitted model has in the script.
#' @param file The file name written.
#' @param draw A named list of the drawing arguments that differ from the
#'   defaults of [sieve_export_diagram()].
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_diagram_export_code <- function(spec, fit_name, file, draw = list()) {
  value <- function(x) {
    if (is.character(x)) paste(deparse(x), collapse = "") else as.character(x)
  }
  extra <- if (length(draw)) {
    paste0(", ", paste(sprintf("%s = %s", names(draw), vapply(draw, value, character(1))),
      collapse = ", "
    ))
  } else {
    ""
  }
  c(
    "# The path diagram as shown: a specification built from the fitted model, then written.",
    sieve_diagram_code(spec, fit_name = fit_name),
    sprintf("SIEVER::sieve_export_diagram(diagram, %s%s)", value(file), extra)
  )
}

#' The text alternative of a drawn path diagram
#'
#' Says what the drawn model is, from the kind of model the specification
#' holds (`meta$model_kind`, see [sieve_diagram_spec()]) and whether it
#' draws regression paths, and which tables hold the estimates.
#'
#' @param spec A `sieve_diagram` specification, or `NULL` before a model is
#'   drawn.
#' @return One string.
#' @author Ken Kelley
#' @noRd
sieve_diagram_alt <- function(spec) {
  kind <- if (inherits(spec, "sieve_diagram")) spec$meta$model_kind %||% NA_character_ else NA_character_
  regression <- inherits(spec, "sieve_diagram") && is.data.frame(spec$edges) &&
    any(spec$edges$type == "regression")
  factors <- if (inherits(spec, "sieve_diagram") && is.data.frame(spec$nodes)) sum(spec$nodes$kind == "latent") else NA_integer_
  shown <- if (identical(kind, "cfa") && identical(factors, 1L)) {
    "the one-factor confirmatory factor model: the factor with its items and their loadings"
  } else if (identical(kind, "cfa")) {
    "the confirmatory factor model: each factor with its items and their loadings, and the correlations the model estimates among the factors"
  } else if (identical(kind, "higher_order")) {
    paste0(
      "the higher-order confirmatory factor model: each first-order factor with its items and their loadings, ",
      "and the loadings of the first-order factors on the higher-order factor"
    )
  } else if (identical(kind, "sem") || regression) {
    paste0(
      "the structural model: each factor with its items and their loadings, the regression paths among ",
      "the factors and the outcomes, and the covariances the model estimates among the predictors and ",
      "among the disturbances of the outcomes"
    )
  } else {
    "the fitted model: each latent variable with its indicators and their paths, and the covariances the model estimates"
  }
  sprintf("Path diagram of %s. The estimates are in the tables of the page that drew it.", shown)
}

#' Wire the path diagram panel of a module
#'
#' Called inside a module server. `base` builds the specification once per
#' fitted model; the reactive made here applies the display controls with
#' [sieve_diagram_update()], which reads only the specification, so moving
#' a control redraws and never refits. The screen and the three downloads
#' read that same reactive.
#'
#' @param input,output,session The module's own.
#' @param state The shared application state (palette and code log).
#' @param base A reactive returning the specification of the fitted model.
#' @param layout The layout used until the control has a value.
#' @param fit_name The name the fitted model has in the reproducible
#'   script.
#' @param file_stem The stem of the downloaded file names.
#' @param what A short description of the model, for the code log.
#' @param fit_code A function of the displayed specification returning the
#'   R lines that recreate the fitted model, or `NULL` when the module has
#'   already logged them.
#' @return A list of reactives: `spec` (the displayed specification),
#'   `problem` (`NULL`, or the condition that prevents drawing), and
#'   `draw_args` (the drawing arguments shared by screen and file).
#' @author Ken Kelley
#' @noRd
#' @importFrom shiny reactive req renderUI renderPlot downloadHandler downloadButton div tags
#' @importFrom shiny em strong
#' @importFrom DT renderDT datatable formatRound
sieve_diagram_wire <- function(input, output, session, state, base,
                               layout = "top_down", fit_name = "fit",
                               file_stem = "siever_path_diagram", what = "model",
                               fit_code = NULL) {
  flag <- function(x, default) if (is.null(x)) default else isTRUE(x)
  # A control that has no value yet, or holds a value the control does not
  # offer (for example from a restored session), counts as its default.
  choice <- function(x, choices, default) {
    if (is.character(x) && length(x) == 1L && x %in% choices) x else default
  }

  # The displayed specification: the display options applied to the
  # specification of the fitted model. No fitted model is read here.
  spec <- reactive({
    b <- base()
    req(inherits(b, "sieve_diagram"))
    sieve_diagram_update(
      b,
      standardized = choice(input$what, c("std.all", "std.lv", "none"), "std.all"),
      residuals = flag(input$resid, TRUE),
      variances = flag(input$vars, TRUE),
      covariances = flag(input$covs, TRUE),
      residual_covariances = flag(input$rescovs, TRUE),
      fixed_zero = flag(input$fixed_zero, FALSE),
      intercepts = flag(input$intercepts, FALSE),
      min_loading = max(0, sieve_finite_or(input$min_path, 0)),
      label_mode = choice(input$labels, c("names", "short"), "names"),
      layout = choice(input$layout, c("top_down", "left_right"), layout)
    )
  })

  # What stands in the way of a drawing, if anything: the status of the
  # model, or a specification that cannot be laid out.
  problem <- reactive({
    s <- spec()
    tryCatch(
      {
        sieve_diagram_check(s)
        sieve_diagram_paths(s)
        NULL
      },
      sieve_diagram_error = function(e) e
    )
  })

  draw_args <- reactive({
    list(palette = state$palette %||% "nd", monochrome = flag(input$mono, FALSE))
  })

  # The size of the diagram alone, in inches, at which its labels are 10
  # point: what the screen plot and the default file size follow.
  natural <- reactive({
    s <- spec()
    req(is.null(problem()))
    style <- sieve_diagram_style(s, show_caption = FALSE)
    sieve_diagram_export_size(s, style, NULL, NULL)
  })

  output$diagram_status <- renderUI({
    s <- spec()
    p <- problem()
    if (!is.null(p)) {
      warns <- s$status$details$warnings
      return(div(
        class = "alert alert-danger",
        strong("No path diagram. "),
        if (identical(p$reason, s$status$code)) s$status$message else conditionMessage(p),
        " A diagram is drawn only for a model that converged to a solution; nothing is ",
        "drawn or offered for download for this one.",
        if (length(warns)) tags$ul(class = "small mb-0 mt-2", lapply(warns, tags$li)) else NULL
      ))
    }
    notes <- list()
    if (identical(s$status$code, "improper")) {
      notes <- c(notes, list(div(
        class = "alert alert-warning small",
        strong("Improper solution. "), s$status$message,
        " The estimates are drawn as they are, with their signs, and each value that is ",
        "out of range is boxed and marked (!). This is not a proper solution."
      )))
    }
    # The size of a large figure is a notice about the display: it is in the
    # line under the figure (output$diagram_readability).
    if (length(notes)) do.call(tagList, notes) else NULL
  })

  # The plot is drawn at the size of its content, reduced only when the
  # panel is narrower, so the height follows the model, not a fixed box.
  screen <- function() {
    size <- natural()
    panel <- session$clientData[[paste0("output_", session$ns("diagram"), "_width")]]
    natural_width <- size[["width"]] * 96
    wide <- natural_width
    fitting <- flag(input$fit_width, TRUE)
    if (fitting && !is.null(panel) && is.finite(panel) && panel > 50) {
      wide <- min(wide, panel)
    }
    zoom <- sieve_finite_or(input$diagram_zoom, 100) / 100
    wide <- wide * max(.5, min(3, zoom))
    # Preserve the nominal eight-point floor after zoom; a narrow panel can scroll.
    if (fitting) wide <- max(0.8 * natural_width, wide)
    c(width = wide, height = wide * size[["height"]] / size[["width"]])
  }
  shiny::observeEvent(input$diagram_readable, {
    shiny::updateCheckboxInput(session, "fit_width", value = FALSE)
    shiny::updateNumericInput(session, "diagram_zoom", value = 100)
  })
  shiny::observeEvent(input$diagram_short_ids, shiny::updateRadioButtons(session, "labels", selected = "short"))
  output$diagram_readability <- renderUI({
    req(is.null(problem()))
    size <- natural()
    shown <- screen()
    pt <- sieve_diagram_effective_label(size[["width"]] * 96, shown[["width"]])
    style <- sieve_diagram_style(spec(), show_caption = flag(input$dl_caption, TRUE))
    auto <- flag(input$fig_auto, TRUE)
    good <- function(x) if (auto || is.null(x) || !is.finite(x) || x <= 0) NULL else x
    exported <- sieve_diagram_export_size(spec(), style, good(input$fig_width), good(input$fig_height))
    nominal <- sieve_diagram_export_size(spec(), style, NULL, NULL)
    export_pt <- sieve_diagram_effective_label(nominal[["width"]], exported[["width"]])
    said <- sieve_diagram_notice_text(pt, export_pt, note = spec()$size$note)
    div(
      class = paste("small mt-1", if (is.finite(pt) && pt < 8) "text-warning-emphasis" else "text-muted"),
      title = said$full,
      said$short
    )
  })
  # The text alternative says what the drawn model is: a confirmatory
  # factor model on the CFA page, a structural model with its regression
  # paths on the SEM page (sieve_diagram_alt()).
  output$diagram <- renderPlot(
    alt = function() sieve_diagram_alt(tryCatch(spec(), error = function(e) NULL)),
    {
      s <- spec()
      req(is.null(problem()))
      args <- draw_args()
      sieve_draw_diagram(s,
        palette = args$palette, monochrome = args$monochrome,
        show_caption = FALSE
      )
    },
    width = function() screen()[["width"]],
    height = function() screen()[["height"]],
    execOnResize = TRUE,
    res = 96
  )

  output$diagram_caption <- renderUI({
    s <- spec()
    req(is.null(problem()))
    tags$p(
      class = "small mt-2", sieve_diagram_figure_caption(s, key = FALSE),
      if (identical(s$meta$options$label_mode, "short")) {
        " The short IDs are explained in the key below."
      }
    )
  })

  output$diagram_key <- renderUI({
    s <- spec()
    req(is.null(problem()), identical(s$meta$options$label_mode, "short"))
    key <- s$label_key
    tagList(
      tags$h6(class = "mt-3", "Key to the short IDs"),
      tags$table(
        class = "table table-sm small w-auto",
        tags$thead(tags$tr(tags$th("ID"), tags$th("Variable"), tags$th("Label"))),
        tags$tbody(lapply(seq_len(nrow(key)), function(i) {
          tags$tr(tags$td(key$short[i]), tags$td(key$id[i]), tags$td(key$label[i]))
        }))
      )
    )
  })

  # Downloads are offered only for a specification that can be drawn.
  output$diagram_downloads <- renderUI({
    req(is.null(problem()))
    # Offered only when a device for it opens on this machine (probed once).
    svg_ok <- sieve_device_works("svglite") || sieve_device_works("svg")
    div(
      class = "d-flex gap-2 flex-wrap align-items-center mt-2",
      downloadButton(session$ns("dl_diagram"), "Download PDF", class = "btn-primary btn-sm"),
      if (svg_ok) {
        downloadButton(session$ns("dl_diagram_svg"), "SVG", class = "btn-outline-primary btn-sm")
      },
      downloadButton(session$ns("dl_diagram_png"), "PNG (300 dpi)",
        class = "btn-outline-primary btn-sm"
      ),
      tags$span(
        class = "text-muted small",
        "The file holds the diagram as shown, sized to its content.",
        if (!svg_ok) " SVG needs the svglite package." else NULL
      )
    )
  })

  # One writer for the three formats: the displayed specification, the
  # drawing arguments of the screen, and the size asked for.
  write_file <- function(file, format) {
    s <- spec()
    args <- draw_args()
    auto <- flag(input$fig_auto, TRUE)
    size <- function(x) {
      x <- sieve_finite_or(x, NA_real_)
      if (auto || is.na(x) || x <= 0) NULL else x
    }
    caption <- flag(input$dl_caption, TRUE)
    written <- sieve_export_diagram(
      s, file,
      format = format, width = size(input$fig_width),
      height = size(input$fig_height), palette = args$palette,
      monochrome = args$monochrome, show_caption = caption
    )
    name <- paste0(file_stem, ".", format)
    draw <- c(
      if (!identical(args$palette, "nd")) list(palette = args$palette),
      if (args$monochrome) list(monochrome = TRUE),
      if (!caption) list(show_caption = FALSE),
      if (!is.null(size(input$fig_width))) list(width = size(input$fig_width)),
      if (!is.null(size(input$fig_height))) list(height = size(input$fig_height))
    )
    sieve_log(
      state,
      sprintf(
        "Path diagram of the %s%s, written as %s.", what,
        if (is.null(s$meta$analysis_id)) "" else sprintf(" (analysis %s)", s$meta$analysis_id),
        toupper(format)
      ),
      c(
        if (is.function(fit_code)) fit_code(s),
        sieve_diagram_export_code(s, fit_name, name, draw)
      )
    )
    invisible(written)
  }
  output$dl_diagram <- downloadHandler(
    filename = function() paste0(file_stem, ".pdf"),
    content = function(file) write_file(file, "pdf")
  )
  output$dl_diagram_svg <- downloadHandler(
    filename = function() paste0(file_stem, ".svg"),
    content = function(file) write_file(file, "svg")
  )
  output$dl_diagram_png <- downloadHandler(
    filename = function() paste0(file_stem, ".png"),
    content = function(file) write_file(file, "png")
  )

  output$diagram_params <- renderDT({
    s <- spec()
    req(is.null(problem()))
    tab <- sieve_diagram_parameter_table(s)
    level <- format(100 * s$meta$conf_level)
    scale <- if (identical(s$meta$standardized, "std.lv")) "std.lv" else "std.all"
    sieve_dt(
      tab,
      rownames = FALSE, selection = "none",
      options = list(dom = "tp", pageLength = 25, scrollX = TRUE),
      colnames = c(
        "Parameter", "Type", "Status", "Estimate", "SE",
        sprintf("%s%% CI lower", level), sprintf("%s%% CI upper", level),
        sprintf("Standardized (%s)", scale), "Std. CI lower", "Std. CI upper", "Flag"
      )
    ) |>
      formatRound(c(
        "Estimate", "SE", "CI.lower", "CI.upper", "Standardized",
        "Std.CI.lower", "Std.CI.upper"
      ), 3)
  })

  list(spec = spec, problem = problem, draw_args = draw_args)
}

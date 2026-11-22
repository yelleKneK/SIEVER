# fct_diagram.R
# The path diagram specification. A fitted lavaan model is read once into a
# renderer-independent description of the diagram: which nodes exist, which
# relationships join them, what each number is and whether it was estimated,
# fixed, or implied, where each node sits, and what the caption must
# disclose. Every renderer, the Shiny modules, the exports, and plain R use
# read this one object, so they cannot disagree about the model. Nothing here
# draws, opens a graphics device, fits a model, or uses random numbers.
#
# Pipeline (each stage is a pure function of the stage before it):
#   sieve_diagram_build()   fit -> nodes, edges, parameters, model facts
#   sieve_diagram_relabel() display labels and node sizes
#   sieve_diagram_layout()  node coordinates
#   sieve_diagram_apply()   visibility, displayed values, routing, caption
# sieve_diagram_spec() runs all four; sieve_diagram_update() reruns only the
# later stages from the stored tables, so a display change never needs the
# fitted model.

#' Build the Specification of a Path Diagram From a Fitted Model
#'
#' Reads a fitted 'lavaan' model into a description of its path diagram that
#' does not depend on any plotting package: a table of nodes, a table of
#' relationships (edges), the matching parameter table, the layout, and a
#' caption. The same object is what the SIEVER app draws, what the exports
#' write, and what a script can regenerate with [sieve_diagram_code()].
#' Building the specification never refits the model, draws nothing, and
#' leaves the random number stream untouched.
#'
#' @details
#' **Notation.** Latent variables are nodes of kind `"latent"` (drawn as
#' circles or ellipses), observed variables are `"observed"` (rectangles),
#' and every endogenous variable that has a residual variance in the model
#' gets its own node of kind `"residual"`: a unique term for an indicator, a
#' disturbance for a latent or observed outcome. A residual node sends a
#' path fixed at 1 into its variable and carries the residual variance as a
#' variance edge on itself (a double-headed self-loop). Exogenous variables
#' carry their variance on themselves.
#'
#' **What a double-headed edge means.** An edge of type `"covariance"` exists
#' only for a covariance parameter of the model, and `cov_class` says which
#' kind: `"exogenous"` (between two exogenous variables), `"disturbance"`
#' (between the disturbances of two endogenous variables), or `"residual"`
#' (between the unique terms of two indicators). The last two are
#' covariances of residuals, are attached to the residual nodes, and are
#' never the marginal correlation of the variables themselves.
#'
#' **Identification is not standardization.** `fixed`, `free`, `derived` and
#' `constraint` come from the lavaan parameter table, never from the value.
#' Fixing a latent variance to 1 sets the scale of that latent variable
#' only. The fully standardized values come from
#' `lavaan::standardizedSolution(type = "std.all")`, are labeled as such in
#' the caption, and keep the `fixed` flag of the parameter they standardize:
#' a marker loading fixed at 1 need not be 1 after standardization, and a
#' standardized variance of 1 was not necessarily fixed.
#'
#' **Ordered indicators.** Coefficients then refer to the latent response
#' variables. Under the delta parameterization the residual variances are
#' implied by the model rather than estimated; they are flagged `derived`,
#' and the caption says so. Thresholds are not drawn unless
#' `intercepts = TRUE`; they are always in `$parameters`.
#'
#' **Status.** `$status$code` is `"ok"`; `"improper"` (converged, with a
#' negative variance, a fully standardized variance above 1, or another
#' standardized value out of range: drawn with the estimates as they are,
#' and flagged; where lavaan has no standardized value for a parameter the
#' edge shows the unstandardized estimate marked `unstd.`, and the caption
#' says so); `"not_converged"`; or `"failed"`
#' (no fitted model, a model that is not identified, or a model the diagram
#' does not support). The last two carry no nodes and no edges, so nothing
#' that looks like a result can be drawn or exported from them.
#'
#' Saturated auxiliary variables are omitted from the graph and named in
#' the caption; their estimates remain in the complete parameter table.
#' **Display filters** change only what is drawn. Every hidden edge keeps
#' its row with `visible = FALSE` and a
#' `hidden_reason`, and the caption names what was hidden.
#'
#' **Layout.** Coordinates are in inches at the recommended size
#' (`$size$width` by `$size$height`), at which node labels are 10 point and
#' edge labels 9 point. `"top_down"` puts higher-order factors on top,
#' factors beneath them, each factor's indicators in a row beneath it, and
#' the unique terms beneath the indicators, with room above the factors for
#' the covariance arcs. `"left_right"` runs the structural regressions from
#' left to right by the longest-path depth of each variable and keeps each
#' latent variable's indicators together beside it; for a model without
#' regressions it is the top-down layout turned on its side, which suits
#' many indicators with long names. `"auto"` chooses `"left_right"` when the
#' model has regressions and `"top_down"` otherwise. Factor and item order
#' follow the model, not the alphabet, and no display filter moves a node.
#'
#' The recommended size covers what is drawn: hiding a class of
#' relationships gives its room back, and every node stays where it was.
#'
#' The fitted model needs the 'lavaan' package.
#'
#' @param fit A fitted 'lavaan' model, or `NULL`. A model fit by SIEVER
#'   carries its resolved specification and name map as attributes, and
#'   they are used when present.
#' @param standardized Which values the edge labels show: `"std.all"`
#'   (fully standardized, the default), `"std.lv"` (standardized with
#'   respect to the latent variables only), or `"none"` (the unstandardized
#'   estimates).
#' @param group For a multiple-group model, the group to draw: its position
#'   or its label. One group is drawn per diagram.
#' @param name_map Optional named character vector giving readable names for
#'   model variables (model name to readable name), typically constructs and
#'   outcomes. When `NULL`, the `name_map` attribute of `fit` is used if
#'   there is one.
#' @param labels Optional named character vector of item labels (item name
#'   to label). An entry here takes precedence over `name_map`.
#' @param residuals `TRUE` draws the residual nodes (unique terms and
#'   disturbances) with their paths and variances. `FALSE` hides them and,
#'   with them, the covariances between residuals: drawn between the
#'   variables those would look like correlations of the variables, which
#'   they are not. The caption counts what is hidden.
#' @param variances `TRUE` draws the variance self-loops.
#' @param covariances `TRUE` draws the covariances between exogenous
#'   variables (the factor covariances of a measurement model).
#' @param residual_covariances `TRUE` draws the covariances between
#'   residuals (unique terms or disturbances).
#' @param intercepts `TRUE` also draws intercepts, latent means, and
#'   thresholds.
#' @param fixed_zero `TRUE` also draws parameters fixed at zero (for
#'   example the factor covariances of an orthogonal model), which are
#'   otherwise only counted and named in the caption.
#' @param min_loading Loadings whose displayed absolute value is below this
#'   are not drawn. It applies to loadings only, never to regressions,
#'   variances, or covariances.
#' @param label_mode `"names"` labels each node with its name or label;
#'   `"short"` labels observed variables with short identifiers and returns
#'   the key in `$label_key`.
#' @param wrap Labels longer than this many characters are wrapped at word
#'   boundaries. `NULL` (the default) chooses the width: 16 characters, or
#'   more when a label would otherwise need more than four lines, so that a
#'   long item wording gives a wider box rather than a tall narrow one. The
#'   width used is in `$meta$wrap`. Labels are never truncated.
#' @param digits Decimal places in the edge labels, from 1 to 8. Stored
#'   values are never rounded. At least one decimal is required so that an
#'   estimate near 1 is never printed like a parameter fixed at 1.
#' @param layout `"auto"`, `"top_down"`, or `"left_right"`; see Details.
#' @param positions Optional data frame with columns `id`, `x`, and `y`
#'   that replaces the computed position of the listed nodes. A residual
#'   node that is not listed moves with its variable. Positions that put
#'   one node on top of another are an error.
#' @param conf_level Confidence level of the stored intervals.
#' @param analysis_id Optional identifier of the analysis the diagram
#'   belongs to; it is recorded in the caption.
#'
#' @return An object of class `sieve_diagram`: a list with
#'   \describe{
#'   \item{`status`}{A list with `code`, `message`, and `details` (the
#'     fit status, the warnings that were raised, and the edges flagged as
#'     improper).}
#'   \item{`nodes`}{A data frame, one row per node. `id` is the model name
#'     (a residual node is `"e."` followed by the name of its variable);
#'     `label` is the display text, `label_wrapped` the same text with line
#'     breaks, `label_full` the full label whatever the label mode, and
#'     `short` the short identifier. `kind` is `"latent"`, `"observed"`, or
#'     `"residual"`; `role` is `"factor"`, `"higher_order"`, `"indicator"`,
#'     `"outcome"`, `"predictor"`, `"variable"`, `"unique"`, or
#'     `"disturbance"`; `structural` is the variable's place in the
#'     regressions (`"predictor"`, `"mediator"`, `"outcome"`, `"none"`).
#'     `level` is 0 for an indicator, 1 for a first-order factor or another
#'     observed variable, and higher for higher-order factors; `depth` is
#'     the structural depth; `block` is the factor an indicator belongs to;
#'     `parent` is that factor, or for a residual node its variable.
#'     `exogenous` and `ordered` are logical. `x`, `y` (center) and `w`,
#'     `h` (full width and height) are in inches; `out_x`, `out_y` point to
#'     the node's free side; `visible` says whether the node is drawn.}
#'   \item{`edges`}{A data frame, one row per relationship. `id`; `from`
#'     and `to` (node ids; a variance, intercept, or threshold has both on
#'     one node); `type` (`"loading"`, `"regression"`, `"residual_path"`,
#'     `"covariance"`, `"variance"`, `"intercept"`, `"threshold"`);
#'     `cov_class`; the lavaan parameter it encodes (`op`, `lhs`, `rhs`,
#'     and `par`, its row in `parameters`; all missing for a residual
#'     path); `est`, `se`, `ci_lower`, `ci_upper` (unstandardized); `std`,
#'     `std_se`, `std_ci_lower`, `std_ci_upper` (the chosen standardization,
#'     missing when `standardized = "none"`); `value` (the number the label
#'     shows, at full precision) and `label_text` (the rounded label; in a
#'     standardized display an edge whose standardized value is undefined
#'     shows its unstandardized estimate marked `unstd.`, and `value` is
#'     then missing);
#'     `free` (the lavaan index, 0 when not free), `fixed`, `derived`
#'     (implied by the model or set to a sample moment, neither estimated
#'     nor fixed by the analyst), `constraint` (the label of an equality
#'     constraint), `improper`; `visible` and `hidden_reason`; and the
#'     route: `curve` (the signed height of a covariance arc), `angle` (the
#'     direction, in degrees, in which a self-loop sits on its node), and
#'     `label_pos` (where along a straight edge its label goes).}
#'   \item{`parameters`}{The parameter table of the group shown, with the
#'     estimate, standard error, and interval of every parameter in the
#'     unstandardized, `std.all`, and `std.lv` metrics, all at full
#'     precision. A parameter that was not estimated has a missing
#'     standard error, not a standard error of zero.}
#'   \item{`label_key`}{A data frame mapping each observed variable to its
#'     short identifier and full label.}
#'   \item{`meta`}{Model facts (syntax, estimator, parameterization, group,
#'     identification, versions), the display options as resolved, the
#'     hidden edges, and the caption.}
#'   \item{`size`}{The recommended `width` and `height` in inches, the
#'     coordinate ranges `xlim` and `ylim`, and the type sizes they assume
#'     (`base_pt`, `edge_pt`).}
#'   }
#' @seealso [sieve_draw_diagram()] (or `plot()`) to draw a specification,
#'   [sieve_export_diagram()] to write it as PDF, SVG, or PNG,
#'   [sieve_diagram_update()] to change display options without the fitted
#'   model, and [sieve_diagram_code()] for the call that regenerates a
#'   specification.
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' model <- "visual =~ t1_visual_perception + t2_cubes + t4_lozenges
#'           textual =~ t6_paragraph_comprehension + t7_sentence + t9_word_meaning"
#' fit <- lavaan::cfa(model, data = hs, std.lv = TRUE)
#' diagram <- sieve_diagram_spec(
#'   fit,
#'   labels = c(t1_visual_perception = "Visual perception", t2_cubes = "Cubes")
#' )
#' diagram
#' # One row per relationship, with the model parameter it encodes.
#' diagram$edges[, c("from", "to", "type", "value", "label_text", "fixed")]
#' # The matching parameter table, at full precision, with intervals.
#' head(diagram$parameters[, c(
#'   "lhs", "op", "rhs", "est", "std_all",
#'   "std_all_ci_lower", "std_all_ci_upper"
#' )])
#' plot(diagram)
#' @md
#' @export
sieve_diagram_spec <- function(fit, standardized = c("std.all", "std.lv", "none"),
                               group = 1L, name_map = NULL, labels = NULL,
                               residuals = TRUE, variances = TRUE, covariances = TRUE,
                               residual_covariances = TRUE, intercepts = FALSE,
                               fixed_zero = FALSE, min_loading = 0,
                               label_mode = c("names", "short"), wrap = NULL,
                               digits = 2L, layout = c("auto", "top_down", "left_right"),
                               positions = NULL, conf_level = 0.95, analysis_id = NULL) {
  options <- sieve_diagram_options(list(
    standardized = match.arg(standardized), group = group, name_map = name_map,
    labels = labels, residuals = residuals, variances = variances,
    covariances = covariances, residual_covariances = residual_covariances,
    intercepts = intercepts, fixed_zero = fixed_zero, min_loading = min_loading,
    label_mode = match.arg(label_mode), wrap = wrap, digits = digits,
    layout = match.arg(layout), positions = positions, conf_level = conf_level,
    analysis_id = analysis_id
  ))
  source <- sieve_diagram_source(fit)
  status <- sieve_diagram_status(source)
  if (!status$code %in% c("ok", "improper")) {
    return(sieve_diagram_failed(options, status))
  }
  built <- sieve_diagram_build(source$fit, options$group, options$conf_level)
  if (!is.null(built$error)) {
    status$code <- "failed"
    status$message <- built$error
    return(sieve_diagram_failed(options, status))
  }
  status$details$warnings <- unique(c(status$details$warnings, built$warnings))
  flagged <- built$edges[built$edges$improper, c("id", "type"), drop = FALSE]
  status$details$improper <- flagged
  if (nrow(flagged) && identical(status$code, "ok")) {
    status$code <- "improper"
    status$message <- sprintf(
      "The solution is improper: %s is out of range.",
      paste(flagged$id, collapse = ", ")
    )
  }
  spec <- structure(list(
    status = status,
    nodes = built$nodes,
    edges = built$edges,
    parameters = built$parameters,
    label_key = NULL,
    meta = c(built$meta, list(
      converged = isTRUE(status$details$fit_status$converged),
      admissible = isTRUE(status$details$fit_status$admissible) && !nrow(flagged),
      name_map_fit = sieve_diagram_named(attr(source$fit, "name_map")),
      analysis_id = options$analysis_id,
      options = options
    )),
    size = NULL
  ), class = "sieve_diagram")
  spec <- sieve_diagram_relabel(spec)
  spec <- sieve_diagram_place(spec)
  spec <- sieve_diagram_apply(spec)
  sieve_diagram_validate(spec)
  spec
}

#' Change the Display Options of a Diagram Specification
#'
#' Applies new display options to an existing specification using only what
#' the specification already holds. The fitted model is not needed and is
#' not touched, so a control in the app can call this as often as it likes
#' without refitting anything. Options that only change what is drawn or how
#' a number is shown (`residuals`, `variances`, `covariances`,
#' `residual_covariances`, `intercepts`, `fixed_zero`, `min_loading`,
#' `standardized`, `digits`, `analysis_id`) leave every node exactly where
#' it was. Options that change the label text or the arrangement
#' (`name_map`, `labels`, `label_mode`, `wrap`, `layout`, `positions`)
#' recompute the layout.
#'
#' The group shown and the confidence level cannot be changed here, because
#' they need the fitted model; call [sieve_diagram_spec()] for those.
#'
#' @param spec A specification from [sieve_diagram_spec()].
#' @param ... Display options to change, named as in
#'   [sieve_diagram_spec()]. Unknown names are an error.
#' @return The updated `sieve_diagram`. Estimates, standard errors,
#'   intervals, and the parameter table are carried over unchanged.
#' @seealso [sieve_diagram_spec()]
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' model <- "visual =~ t1_visual_perception + t2_cubes + t4_lozenges
#'           textual =~ t6_paragraph_comprehension + t7_sentence + t9_word_meaning"
#' fit <- lavaan::cfa(model, data = hs, std.lv = TRUE)
#' diagram <- sieve_diagram_spec(fit)
#' plain <- sieve_diagram_update(diagram,
#'   covariances = FALSE,
#'   standardized = "none"
#' )
#' # The factor covariance is hidden, not removed, and no node has moved.
#' plain$edges[
#'   plain$edges$type == "covariance",
#'   c("id", "visible", "hidden_reason")
#' ]
#' identical(plain$nodes[, c("x", "y")], diagram$nodes[, c("x", "y")])
#' @md
#' @export
sieve_diagram_update <- function(spec, ...) {
  if (!inherits(spec, "sieve_diagram")) {
    stop("`spec` must be a diagram specification from sieve_diagram_spec().", call. = FALSE)
  }
  new <- list(...)
  if (!length(new)) {
    return(spec)
  }
  given <- names(new)
  if (is.null(given) || any(!nzchar(given))) {
    stop("Every option passed to sieve_diagram_update() must be named.", call. = FALSE)
  }
  if (anyDuplicated(given)) {
    stop(sprintf(
      "Option(s) given more than once: %s.",
      paste(unique(given[duplicated(given)]), collapse = ", ")
    ), call. = FALSE)
  }
  needs_fit <- intersect(given, c("fit", "group", "conf_level"))
  if (length(needs_fit)) {
    stop(sprintf(
      paste0(
        "%s cannot be changed without the fitted model. ",
        "Call sieve_diagram_spec() with the fitted model instead."
      ),
      paste(sprintf("`%s`", needs_fit), collapse = " and ")
    ), call. = FALSE)
  }
  old <- spec$meta$options
  unknown <- setdiff(given, names(old))
  if (length(unknown)) {
    stop(
      sprintf(
        "Unknown diagram option(s): %s. The options are: %s.",
        paste(unknown, collapse = ", "),
        paste(setdiff(names(old), c("group", "conf_level")), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  merged <- old
  for (name in given) {
    # A list element set to NULL is removed, so NULL is restored by position.
    merged[name] <- list(new[[name]])
  }
  options <- sieve_diagram_options(merged)
  changed <- given[!vapply(
    given, function(name) identical(options[[name]], old[[name]]),
    logical(1)
  )]
  spec$meta$options <- options
  spec$meta["analysis_id"] <- list(options$analysis_id)
  if (!spec$status$code %in% c("ok", "improper")) {
    return(spec)
  }
  if (any(changed %in% c("name_map", "labels", "label_mode", "wrap"))) {
    spec <- sieve_diagram_relabel(spec)
    spec <- sieve_diagram_place(spec)
  } else if (any(changed %in% c("layout", "positions"))) {
    spec <- sieve_diagram_place(spec)
  }
  spec <- sieve_diagram_apply(spec)
  sieve_diagram_validate(spec)
  spec
}

#' The R Call That Regenerates a Diagram Specification
#'
#' Writes the call to [sieve_diagram_spec()] that reproduces a
#' specification from its fitted model, for a script or the reproducible
#' code log. Only the options that differ from the defaults are written
#' (the coefficient scale is always written, because it decides what every
#' number means), and numbers are written at full precision.
#'
#' @param spec A specification from [sieve_diagram_spec()].
#' @param fit_name The name the fitted model has in the script.
#' @param spec_name The name the specification is assigned to.
#' @return A character vector of R code lines.
#' @seealso [sieve_diagram_spec()]
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' model <- "visual =~ t1_visual_perception + t2_cubes + t4_lozenges
#'           textual =~ t6_paragraph_comprehension + t7_sentence + t9_word_meaning"
#' fit <- lavaan::cfa(model, data = hs, std.lv = TRUE)
#' diagram <- sieve_diagram_spec(fit, standardized = "none", min_loading = 0.3)
#' cat(sieve_diagram_code(diagram), sep = "\n")
#' @md
#' @export
sieve_diagram_code <- function(spec, fit_name = "fit", spec_name = "diagram") {
  if (!inherits(spec, "sieve_diagram")) {
    stop("`spec` must be a diagram specification from sieve_diagram_spec().", call. = FALSE)
  }
  for (name in list(fit_name, spec_name)) {
    if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
      stop("`fit_name` and `spec_name` must each be a single name.", call. = FALSE)
    }
  }
  options <- spec$meta$options
  defaults <- sieve_diagram_defaults()
  write_value <- function(value) {
    if (is.null(value)) {
      return("NULL")
    }
    if (is.data.frame(value)) {
      cols <- vapply(names(value), function(col) {
        sprintf("%s = %s", col, write_value(value[[col]]))
      }, character(1))
      return(sprintf("data.frame(%s)", paste(cols, collapse = ", ")))
    }
    one <- if (is.character(value)) {
      vapply(value, function(v) paste(deparse(v), collapse = ""), character(1))
    } else if (is.integer(value)) {
      paste0(value, "L")
    } else if (is.double(value)) {
      # The shortest form that reads back as exactly the same number.
      vapply(value, function(v) {
        short <- format(v, digits = 15)
        if (identical(as.numeric(short), v)) short else format(v, digits = 17)
      }, character(1))
    } else {
      as.character(value)
    }
    if (!is.null(names(value))) {
      one <- sprintf("%s = %s", vapply(
        names(value), function(v) paste(deparse(v), collapse = ""),
        character(1)
      ), one)
    }
    if (length(one) == 1L && is.null(names(value))) one else sprintf("c(%s)", paste(one, collapse = ", "))
  }
  keep <- vapply(names(options), function(name) {
    identical(name, "standardized") || !identical(options[[name]], defaults[[name]])
  }, logical(1))
  args <- vapply(names(options)[keep], function(name) {
    sprintf("%s = %s", name, write_value(options[[name]]))
  }, character(1))
  args <- c(fit_name, unname(args))
  if (sum(nchar(args)) + nchar(spec_name) < 70L) {
    return(sprintf(
      "%s <- SIEVER::sieve_diagram_spec(%s)", spec_name,
      paste(args, collapse = ", ")
    ))
  }
  c(
    sprintf("%s <- SIEVER::sieve_diagram_spec(", spec_name),
    paste0("  ", args, c(rep(",", length(args) - 1L), "")),
    ")"
  )
}

#' Print a Diagram Specification
#'
#' Shows the status, the counts of nodes and relationships, the recommended
#' size, and the caption.
#'
#' @param x A specification from [sieve_diagram_spec()].
#' @param ... Not used.
#' @return `x`, invisibly.
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' fit <- lavaan::cfa("visual =~ t1_visual_perception + t2_cubes + t4_lozenges",
#'   data = hs, std.lv = TRUE
#' )
#' print(sieve_diagram_spec(fit))
#' @md
#' @export
print.sieve_diagram <- function(x, ...) {
  cat("SIEVER path diagram specification\n")
  cat(sprintf("Status: %s. %s\n", x$status$code, x$status$message))
  if (sieve_diagram_drawable(x)) {
    count <- function(v) {
      tab <- table(v)
      paste(sprintf("%d %s", as.integer(tab), names(tab)), collapse = ", ")
    }
    shown <- x$edges$visible
    cat(sprintf("Nodes: %d (%s)\n", nrow(x$nodes), count(x$nodes$kind)))
    cat(sprintf(
      "Edges: %d drawn of %d (%s)\n", sum(shown), nrow(x$edges),
      count(x$edges$type[shown])
    ))
    cat(sprintf(
      "Layout: %s, %s by %s inches\n", x$meta$layout,
      formatC(x$size$width, format = "f", digits = 1),
      formatC(x$size$height, format = "f", digits = 1)
    ))
    if (!is.null(x$size$note)) {
      cat(strwrap(paste("Note:", x$size$note), width = 78, exdent = 2), sep = "\n")
    }
  }
  cat(strwrap(paste("Caption:", x$meta$caption), width = 78, exdent = 2), sep = "\n")
  invisible(x)
}

#' Can a diagram specification be drawn?
#'
#' A specification is drawable when its model converged to a solution that
#' can be shown: status `"ok"` or `"improper"`. A renderer or an export must
#' refuse anything else.
#'
#' @param spec A specification from [sieve_diagram_spec()].
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_diagram_drawable <- function(spec) {
  inherits(spec, "sieve_diagram") && isTRUE(spec$status$code %in% c("ok", "improper"))
}

# ==== options ====

#' The default display options of a diagram specification
#'
#' @return A named list, in the argument order of [sieve_diagram_spec()].
#' @author Ken Kelley
#' @noRd
sieve_diagram_defaults <- function() {
  list(
    standardized = "std.all", group = 1L, name_map = NULL, labels = NULL,
    residuals = TRUE, variances = TRUE, covariances = TRUE,
    residual_covariances = TRUE, intercepts = FALSE, fixed_zero = FALSE,
    min_loading = 0, label_mode = "names", wrap = NULL, digits = 2L,
    layout = "auto", positions = NULL, conf_level = 0.95, analysis_id = NULL
  )
}

#' A named character vector, or `NULL`
#'
#' @param x A named character vector or list, or `NULL`.
#' @return A named character vector with empty and missing entries removed,
#'   or `NULL` when nothing usable remains.
#' @author Ken Kelley
#' @noRd
sieve_diagram_named <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  x <- unlist(x)
  if (!is.character(x) || is.null(names(x))) {
    return(NULL)
  }
  x <- x[!is.na(x) & nzchar(x) & !is.na(names(x)) & nzchar(names(x))]
  x <- x[!duplicated(names(x))]
  if (length(x)) x else NULL
}

#' Validate and normalize the display options
#'
#' The one place the options are checked, shared by [sieve_diagram_spec()]
#' and [sieve_diagram_update()], so both reject the same things.
#'
#' @param o A complete named list of options.
#' @return The list with each option in its stored form.
#' @author Ken Kelley
#' @noRd
sieve_diagram_options <- function(o) {
  fail <- function(...) stop(sprintf(...), call. = FALSE)
  choice <- function(name, choices) {
    v <- o[[name]]
    if (!is.character(v) || length(v) != 1L || is.na(v) || !v %in% choices) {
      fail("`%s` must be one of %s.", name, paste(sprintf("\"%s\"", choices), collapse = ", "))
    }
    v
  }
  flag <- function(name) {
    v <- o[[name]]
    if (!is.logical(v) || length(v) != 1L || is.na(v)) {
      fail("`%s` must be TRUE or FALSE.", name)
    }
    v
  }
  number <- function(name) {
    v <- o[[name]]
    if (!is.numeric(v) || length(v) != 1L || is.na(v)) {
      fail("`%s` must be a single number.", name)
    }
    v
  }
  names_vector <- function(name) {
    v <- o[[name]]
    if (is.null(v)) {
      return(NULL)
    }
    if (is.list(v)) {
      v <- unlist(v)
    }
    if (!is.character(v) || is.null(names(v)) || anyNA(v) || anyNA(names(v)) ||
      any(!nzchar(names(v))) || anyDuplicated(names(v))) {
      fail("`%s` must be a character vector with unique names (model name = label).", name)
    }
    v
  }
  out <- sieve_diagram_defaults()
  out$standardized <- choice("standardized", c("std.all", "std.lv", "none"))
  group <- o$group
  if (is.character(group)) {
    if (length(group) != 1L || is.na(group) || !nzchar(group)) {
      fail("`group` must be a single group label or a single position.")
    }
    out$group <- group
  } else {
    if (!is.numeric(group) || length(group) != 1L || is.na(group) ||
      group < 1 || group != floor(group)) {
      fail("`group` must be a single group label or a single position.")
    }
    out$group <- as.integer(group)
  }
  out["name_map"] <- list(names_vector("name_map"))
  out["labels"] <- list(names_vector("labels"))
  for (name in c(
    "residuals", "variances", "covariances", "residual_covariances",
    "intercepts", "fixed_zero"
  )) {
    out[[name]] <- flag(name)
  }
  min_loading <- number("min_loading")
  if (!is.finite(min_loading) || min_loading < 0) {
    fail("`min_loading` must be a single number that is zero or larger.")
  }
  out$min_loading <- as.numeric(min_loading)
  out$label_mode <- choice("label_mode", c("names", "short"))
  if (!is.null(o$wrap)) {
    wrap <- number("wrap")
    if (wrap < 4 || (is.finite(wrap) && wrap != floor(wrap))) {
      fail(paste0(
        "`wrap` must be a whole number of characters, 4 or more, Inf for no ",
        "wrapping, or NULL for the automatic width."
      ))
    }
    out$wrap <- if (is.finite(wrap)) as.integer(wrap) else Inf
  }
  digits <- number("digits")
  if (!is.finite(digits) || digits < 1 || digits > 8 || digits != floor(digits)) {
    fail("`digits` must be a whole number from 1 to 8.")
  }
  out$digits <- as.integer(digits)
  out$layout <- choice("layout", c("auto", "top_down", "left_right"))
  positions <- o$positions
  if (!is.null(positions)) {
    if (!is.data.frame(positions) || !all(c("id", "x", "y") %in% names(positions))) {
      fail("`positions` must be a data frame with columns id, x, and y.")
    }
    positions <- data.frame(
      id = as.character(positions$id), x = positions$x,
      y = positions$y, stringsAsFactors = FALSE
    )
    if (anyNA(positions$id) || anyDuplicated(positions$id)) {
      fail("`positions` must name each node once.")
    }
    if (!is.numeric(positions$x) || !is.numeric(positions$y) ||
      any(!is.finite(positions$x)) || any(!is.finite(positions$y))) {
      fail("`positions` must give a finite x and y for every listed node.")
    }
    positions$x <- as.numeric(positions$x)
    positions$y <- as.numeric(positions$y)
    out["positions"] <- list(if (nrow(positions)) positions else NULL)
  }
  out$conf_level <- sieve_check_conf_level(o[["conf_level"]], "conf_level")
  analysis_id <- o$analysis_id
  if (!is.null(analysis_id)) {
    if (length(analysis_id) != 1L || is.na(analysis_id) ||
      !(is.character(analysis_id) || is.numeric(analysis_id)) ||
      !nzchar(as.character(analysis_id))) {
      fail("`analysis_id` must be a single identifier or NULL.")
    }
    out$analysis_id <- as.character(analysis_id)
  }
  out
}

# ==== status ====

#' Resolve what was passed as the fitted model
#'
#' @param fit A fitted lavaan object, a `sieve_result` (see
#'   `sieve_run_cfa()`), or `NULL`.
#' @return A list with `fit` (a lavaan object or `NULL`), `warnings`,
#'   `error`, and `status` (a fit status when the result already has one).
#' @author Ken Kelley
#' @noRd
sieve_diagram_source <- function(fit) {
  if (is.null(fit)) {
    return(list(
      fit = NULL, warnings = character(0),
      error = "No fitted model was supplied.", status = NULL
    ))
  }
  if (inherits(fit, "sieve_result")) {
    return(list(
      fit = fit$fit, warnings = fit$warnings %||% character(0),
      error = fit$error %||% fit$status$message, status = fit$status
    ))
  }
  if (inherits(fit, "lavaan")) {
    return(list(fit = fit, warnings = character(0), error = NULL, status = NULL))
  }
  stop("`fit` must be a fitted lavaan model, a SIEVER analysis result, or NULL.",
    call. = FALSE
  )
}

#' The status of a diagram, from the status of its model
#'
#' Reads the shared fit status (`sieve_fit_status()`) and maps it to what a
#' diagram can do with it. A model that did not converge, could not be fit,
#' or is not identified gives a failure that carries no numbers; a converged
#' but inadmissible solution is drawable and flagged. One refinement is
#' made: a model fit with standard errors switched off (`se = "none"`) has
#' no standard errors by request, which is not evidence about
#' identification, so it stays drawable with its uncertainty unknown.
#'
#' @param source The list from `sieve_diagram_source()`.
#' @return A list with `code`, `message`, and `details`.
#' @author Ken Kelley
#' @noRd
sieve_diagram_status <- function(source) {
  details <- list(
    fit_status = NULL, warnings = source$warnings, improper = NULL,
    se_requested = NA
  )
  failed <- function(message, code = "failed") {
    list(code = code, message = message, details = details)
  }
  if (is.null(source$fit)) {
    details$fit_status <- source$status
    return(failed(source$error %||% "No fitted model was supplied."))
  }

  fit <- source$fit
  read <- sieve_capture(source$status %||% sieve_fit_status(fit, warnings = source$warnings))
  if (!read$ok) {
    return(failed(sprintf("The fitted model could not be read: %s", read$error)))
  }
  st <- read$value
  details$fit_status <- st
  details$warnings <- unique(c(source$warnings, st$warnings, read$warnings))
  se_option <- tryCatch(lavaan::lavInspect(fit, "options")$se, error = function(e) NA_character_)
  details$se_requested <- !identical(se_option, "none")
  nlevels <- tryCatch(lavaan::lavInspect(fit, "nlevels"), error = function(e) 1L)
  if (isTRUE(nlevels > 1L)) {
    return(failed("Path diagrams of multilevel models are not supported."))
  }
  # Every relationship in the fitted model has to have a place in the
  # figure. A composite (a variable formed from its indicators, lavaan's
  # "<~") has none in this notation, and drawing the rest of the model
  # without it would show a different model from the one that was fit.
  operators <- tryCatch(unique(lavaan::parTable(fit)$op), error = function(e) character(0))
  if ("<~" %in% operators) {
    return(failed(paste0(
      "Path diagrams of models with composites (variables defined by \"<~\") are not ",
      "supported: the figure would leave out the weights that define the composite."
    )))
  }
  unknown <- setdiff(operators, c("=~", "~", "~~", "~1", "|", "~*~", ":=", "==", "<", ">"))
  if (length(unknown)) {
    return(failed(sprintf(
      "Path diagrams of models with the operator%s %s are not supported.",
      if (length(unknown) == 1L) "" else "s", paste(sprintf("\"%s\"", unknown), collapse = ", ")
    )))
  }
  if (identical(st$code, "failed")) {
    return(failed(st$message))
  }
  if (identical(st$code, "not_converged")) {
    return(failed(st$message, code = "not_converged"))
  }
  no_se_by_request <- identical(st$code, "not_identified") && !details$se_requested &&
    isTRUE(st$converged) && is.finite(st$df) && st$df >= 0
  if (identical(st$code, "not_identified") && !no_se_by_request) {
    return(failed(st$message))
  }
  if (identical(st$code, "inadmissible") || (no_se_by_request && !isTRUE(st$admissible))) {
    negative <- sieve_diagram_negative_variances(fit)
    details$negative_variances <- negative
    message <- if (nrow(negative)) {
      sprintf(
        "The solution is improper: negative variance estimate for %s.",
        paste(sieve_diagram_name_in_group(negative), collapse = ", ")
      )
    } else if (length(st$negative_variances)) {
      sprintf(
        "The solution is improper: negative variance estimate for %s.",
        paste(st$negative_variances, collapse = ", ")
      )
    } else {
      "The solution is improper: a model covariance matrix is not positive definite."
    }
    return(list(code = "improper", message = message, details = details))
  }
  message <- if (no_se_by_request) {
    "Converged and admissible. Standard errors were not requested, so uncertainty is unknown."
  } else {
    st$message
  }
  list(code = "ok", message = message, details = details)
}

#' The negative variance estimates of a fitted model, with their groups
#'
#' Read from the parameter table, so that a multiple-group model says in
#' which group each negative variance occurs.
#'
#' @param fit A fitted lavaan object.
#' @return A data frame with `variable`, `group` (position), and
#'   `group_label` (the label, or `NA` for a single-group model), one row
#'   per negative variance estimate; no rows when there is none or the
#'   table cannot be read.
#' @author Ken Kelley
#' @noRd
sieve_diagram_negative_variances <- function(fit) {
  none <- data.frame(
    variable = character(0), group = integer(0), group_label = character(0),
    stringsAsFactors = FALSE
  )
  pt <- tryCatch(as.data.frame(lavaan::parTable(fit)), error = function(e) NULL)
  if (is.null(pt) || is.null(pt$est)) {
    return(none)
  }
  grp <- if (is.null(pt$group)) rep(1L, nrow(pt)) else as.integer(pt$group)
  rows <- which(pt$op == "~~" & pt$lhs == pt$rhs & is.finite(pt$est) & pt$est < 0)
  if (!length(rows)) {
    return(none)
  }
  labels <- tryCatch(lavaan::lavInspect(fit, "group.label"), error = function(e) character(0))
  single <- length(labels) < 2L
  data.frame(
    variable = pt$lhs[rows], group = grp[rows],
    group_label = if (single) rep(NA_character_, length(rows)) else labels[grp[rows]],
    stringsAsFactors = FALSE
  )
}

#' Name a negative variance by its variable and, in a multiple-group model,
#' its group
#'
#' @param negative The data frame from `sieve_diagram_negative_variances()`.
#' @param shown The position of the group drawn, or `NA` when the sentence
#'   is written before a group is chosen.
#' @return A character vector, one entry per row of `negative`: the variable
#'   alone for a single-group model; otherwise the variable with its group,
#'   and "not shown here" when that group is not the one drawn.
#' @author Ken Kelley
#' @noRd
sieve_diagram_name_in_group <- function(negative, shown = NA_integer_) {
  if (!nrow(negative) || all(is.na(negative$group_label))) {
    return(negative$variable)
  }
  elsewhere <- !is.na(shown) & negative$group != shown
  sprintf(
    "%s in group %s%s", negative$variable, negative$group_label,
    ifelse(elsewhere, ", not shown here", "")
  )
}

#' Empty node, edge, and parameter tables
#'
#' The column sets of a specification, with no rows. A failed specification
#' returns these, so code that reads a specification never has to guess
#' which columns exist.
#'
#' @return A list with `nodes`, `edges`, and `parameters`.
#' @author Ken Kelley
#' @noRd
sieve_diagram_empty <- function() {
  chr <- character(0)
  num <- numeric(0)
  int <- integer(0)
  lgl <- logical(0)
  list(
    nodes = data.frame(
      id = chr, label = chr, label_wrapped = chr, label_full = chr, short = chr,
      kind = chr, role = chr, structural = chr, level = int, depth = int,
      block = chr, parent = chr, exogenous = lgl, ordered = lgl,
      x = num, y = num, w = num, h = num, out_x = num, out_y = num,
      visible = lgl, stringsAsFactors = FALSE
    ),
    edges = data.frame(
      id = chr, from = chr, to = chr, type = chr, cov_class = chr,
      op = chr, lhs = chr, rhs = chr, par = int,
      est = num, se = num, ci_lower = num, ci_upper = num,
      std = num, std_se = num, std_ci_lower = num, std_ci_upper = num,
      value = num, label_text = chr, free = int, fixed = lgl, derived = lgl,
      constraint = chr, improper = lgl, visible = lgl, hidden_reason = chr,
      curve = num, angle = num, label_pos = num, stringsAsFactors = FALSE
    ),
    parameters = data.frame(
      id = chr, lhs = chr, op = chr, rhs = chr, group = int, type = chr,
      label = chr, plabel = chr, free = int, fixed = lgl, derived = lgl,
      exo = int, constraint = chr,
      est = num, se = num, ci_lower = num, ci_upper = num,
      std_all = num, std_all_se = num, std_all_ci_lower = num, std_all_ci_upper = num,
      std_lv = num, std_lv_se = num, std_lv_ci_lower = num, std_lv_ci_upper = num,
      improper = lgl, edge = chr, stringsAsFactors = FALSE
    )
  )
}

#' The package versions a specification records
#'
#' @return A list with `SIEVER`, `lavaan`, and `R`.
#' @author Ken Kelley
#' @noRd
sieve_diagram_versions <- function() {
  list(
    SIEVER = sieve_version(),
    lavaan = {
      as.character(utils::packageVersion("lavaan"))
    },
    R = paste(R.version$major, R.version$minor, sep = ".")
  )
}

#' A specification that carries a failure and nothing else
#'
#' @param options The validated options.
#' @param status The status list (code `"failed"` or `"not_converged"`).
#' @return A `sieve_diagram` with empty tables.
#' @author Ken Kelley
#' @noRd
sieve_diagram_failed <- function(options, status) {
  empty <- sieve_diagram_empty()
  structure(list(
    status = status,
    nodes = empty$nodes,
    edges = empty$edges,
    parameters = empty$parameters,
    label_key = data.frame(
      id = character(0), short = character(0), label = character(0),
      stringsAsFactors = FALSE
    ),
    meta = list(
      model = NA_character_, model_source = NA_character_, model_note = NULL,
      model_kind = NA_character_,
      estimator = NA_character_, lavaan_options = NULL, ordered = character(0),
      nobs = NA_integer_, ngroups = NA_integer_, group = NA_integer_,
      group_label = NA_character_, group_labels = character(0), group_variable = NA_character_,
      identification = NA_character_, identification_detail = NULL,
      standardized = options$standardized, converged = isTRUE(status$details$fit_status$converged),
      admissible = FALSE, versions = sieve_diagram_versions(),
      conf_level = options$conf_level, constraints = NULL, bounded = NULL, name_map = NULL,
      name_map_fit = NULL, analysis_id = options$analysis_id, options = options,
      layout = NA_character_,
      hidden = list(
        counts = integer(0),
        edges = data.frame(
          id = character(0), type = character(0),
          reason = character(0), stringsAsFactors = FALSE
        )
      ),
      fixed_zero = character(0),
      caption_parts = c(status = sprintf("No path diagram: %s", status$message)),
      caption = paste0(
        sprintf("No path diagram: %s", status$message),
        if (!is.null(options$analysis_id)) {
          sprintf(" Analysis %s.", options$analysis_id)
        } else {
          ""
        }
      )
    ),
    size = list(
      width = NA_real_, height = NA_real_, units = "in",
      xlim = c(NA_real_, NA_real_), ylim = c(NA_real_, NA_real_),
      base_pt = sieve_diagram_geometry()$base_pt,
      edge_pt = sieve_diagram_geometry()$edge_pt, note = NULL
    )
  ), class = "sieve_diagram")
}

# ==== stage 1: the model graph ====

#' Sets of parameters constrained to be equal
#'
#' Reads the equality constraints of a lavaan parameter table (shared
#' labels, and `==` rows that equate two parameters) and returns, for each
#' row, the label of the equality set it belongs to.
#'
#' @param pt A lavaan parameter table as a data frame.
#' @return A character vector as long as `pt`: the set label for a parameter
#'   that is constrained equal to another, `NA` otherwise.
#' @author Ken Kelley
#' @noRd
sieve_diagram_equalities <- function(pt) {
  out <- rep(NA_character_, nrow(pt))
  is_par <- nzchar(pt$plabel)
  if (!any(is_par)) {
    return(out)
  }
  set <- stats::setNames(pt$plabel[is_par], pt$plabel[is_par])
  join <- function(set, a, b) {
    ra <- set[[a]]
    rb <- set[[b]]
    if (!identical(ra, rb)) {
      set[set == rb] <- ra
    }
    set
  }
  lab <- pt$label[is_par]
  for (l in unique(lab[nzchar(lab)])) {
    members <- pt$plabel[is_par][lab == l]
    for (m in members[-1L]) {
      set <- join(set, members[1L], m)
    }
  }
  resolve <- function(token) {
    if (token %in% names(set)) {
      return(token)
    }
    hit <- pt$plabel[is_par][lab == token]
    if (length(hit)) hit[1L] else NA_character_
  }
  eq <- pt[pt$op == "==", c("lhs", "rhs"), drop = FALSE]
  for (i in seq_len(nrow(eq))) {
    a <- resolve(eq$lhs[i])
    b <- resolve(eq$rhs[i])
    if (!is.na(a) && !is.na(b)) {
      set <- join(set, a, b)
    }
  }
  size <- table(set)
  for (root in names(size)[size > 1L]) {
    members <- names(set)[set == root]
    rows <- match(members, pt$plabel)
    user <- pt$label[rows]
    user <- user[nzchar(user) & !grepl("^\\.p[0-9]+\\.$", user)]
    out[rows] <- if (length(user)) user[1L] else root
  }
  out
}

#' Model syntax of a fitted model, when it can be recovered
#'
#' @param fit A fitted lavaan object.
#' @param pt Its parameter table.
#' @return A list with `model` (a string, or `NA`) and `source`
#'   (`"sieve_spec"`, `"call"`, or `"parameter_table"`).
#' @author Ken Kelley
#' @noRd
sieve_diagram_syntax <- function(fit, pt) {
  spec <- attr(fit, "sieve_spec")
  if (!is.null(spec$model) && is.character(spec$model) && nzchar(spec$model)) {
    return(list(model = spec$model, source = "sieve_spec"))
  }
  call_model <- tryCatch(fit@call$model, error = function(e) NULL)
  if (is.character(call_model) && length(call_model) == 1L && nzchar(call_model)) {
    return(list(model = call_model, source = "call"))
  }
  # Rebuilt from the rows the user wrote, with fixed values and labels as
  # modifiers. It reproduces the structure, not the original formatting.
  user <- pt[pt$user == 1L & pt$op %in% c("=~", "~", "~~", "~1", ":=") &
    (pt$group %in% c(0L, 1L)), , drop = FALSE]
  if (!nrow(user)) {
    return(list(model = NA_character_, source = NA_character_))
  }
  term <- function(i) {
    rhs <- if (user$op[i] == "~1") "1" else user$rhs[i]
    if (user$op[i] == ":=") {
      return(rhs)
    }
    if (nzchar(user$label[i]) && !grepl("^\\.p[0-9]+\\.$", user$label[i])) {
      return(sprintf("%s*%s", user$label[i], rhs))
    }
    if (user$free[i] == 0L && is.finite(user$ustart[i])) {
      return(sprintf("%s*%s", format(user$ustart[i], digits = 15), rhs))
    }
    rhs
  }
  terms <- vapply(seq_len(nrow(user)), term, character(1))
  op <- ifelse(user$op == "~1", "~", user$op)
  key <- paste(user$lhs, op)
  lines <- vapply(unique(key), function(k) {
    rows <- which(key == k)
    sprintf("%s %s %s", user$lhs[rows[1L]], op[rows[1L]], paste(terms[rows], collapse = " + "))
  }, character(1))
  list(model = paste(lines, collapse = "\n"), source = "parameter_table")
}

#' The common name of a lavaan estimator configuration
#'
#' lavaan stores the shortcut the user asked for (`"MLR"`, `"WLSMV"`,
#' `"ULSM"`, ...) as a base estimator plus the standard error and test
#' settings it implies. This reads those settings back into the shortcut:
#' with `ML`, robust standard errors and the Yuan-Bentler test give `MLR`,
#' and the Satorra-Bentler, scaled and shifted, and mean and variance
#' adjusted tests give `MLM`, `MLMV`, and `MLMVS`; first-order information
#' gives `MLF`. The same three tests turn `DWLS` into `WLSM`, `WLSMV`, and
#' `WLSMVS`, and `ULS` into `ULSM`, `ULSMV`, and `ULSMVS`. Anything else is
#' the base estimator.
#'
#' @param options The list from `lavaan::lavInspect(fit, "options")`.
#' @return A length one character string such as `"MLR"` or `"WLSMV"`.
#' @author Ken Kelley
#' @noRd
sieve_diagram_estimator <- function(options) {
  est <- toupper(options$estimator %||% NA_character_)
  se <- options$se %||% ""
  test <- options$test %||% ""
  information <- options$information %||% ""
  if (is.na(est)) {
    return(est)
  }
  if (identical(est, "ML")) {
    if (identical(se, "robust.huber.white") && any(grepl("yuan.bentler", test, fixed = TRUE))) {
      return("MLR")
    }
    if (identical(se, "standard") && all(information == "first.order")) {
      return("MLF")
    }
  }
  if (est %in% c("ML", "DWLS", "ULS") && identical(se, "robust.sem")) {
    stem <- if (identical(est, "DWLS")) "WLS" else est
    if (any(test == "scaled.shifted")) {
      return(paste0(stem, "MV"))
    }
    if (any(test == "mean.var.adjusted")) {
      return(paste0(stem, "MVS"))
    }
    if (any(test == "satorra.bentler")) {
      return(paste0(stem, "M"))
    }
  }
  est
}

#' Read a fitted model into nodes, edges, and parameters
#'
#' The only stage that touches the fitted model. It reads the parameter
#' table, the estimates with their intervals, and the two standardized
#' solutions, and derives the graph from the parameter table alone: latent
#' variables are the left sides of `=~`, endogenous variables are indicators
#' and regression outcomes, and each relationship is one parameter row.
#'
#' @param fit A fitted lavaan object.
#' @param group The group to read (position or label).
#' @param conf_level Confidence level of the intervals.
#' @return A list with `nodes`, `edges`, `parameters`, `meta`, `warnings`,
#'   and `error` (`NULL` unless the model could not be read).
#' @author Ken Kelley
#' @noRd
sieve_diagram_build <- function(fit, group, conf_level) {
  pt <- as.data.frame(lavaan::parTable(fit), stringsAsFactors = FALSE)
  for (col in c("label", "plabel")) {
    if (is.null(pt[[col]])) {
      pt[[col]] <- ""
    }
    pt[[col]][is.na(pt[[col]])] <- ""
  }
  if (is.null(pt$group)) {
    pt$group <- 1L
  }
  if (is.null(pt$exo)) {
    pt$exo <- 0L
  }
  if (is.null(pt$user)) {
    pt$user <- 1L
  }
  ngroups <- lavaan::lavInspect(fit, "ngroups")
  group_labels <- if (ngroups > 1L) lavaan::lavInspect(fit, "group.label") else character(0)
  if (is.character(group)) {
    g <- match(group, group_labels)
    if (is.na(g)) {
      stop(sprintf(
        "`group` \"%s\" is not a group of this model%s.", group,
        if (length(group_labels)) {
          sprintf(" (the groups are: %s)", paste(group_labels, collapse = ", "))
        } else {
          " (it has a single group)"
        }
      ), call. = FALSE)
    }
  } else {
    g <- group
    if (g > ngroups) {
      stop(sprintf(
        "`group` is %d, but the model has %d group%s.", g, ngroups,
        if (ngroups == 1L) "" else "s"
      ), call. = FALSE)
    }
  }
  lav_options <- lavaan::lavInspect(fit, "options")
  read <- function(expr) {
    got <- sieve_capture(expr)
    list(
      value = if (got$ok) as.data.frame(got$value, stringsAsFactors = FALSE) else NULL,
      notes = c(got$warnings, got$error)
    )
  }
  full_parameters <- function(fun, ...) {
    args <- list(...)
    auxiliary_arg <- intersect(c("remove_aux", "remove.aux"), names(formals(fun)))
    if (length(auxiliary_arg)) args[[auxiliary_arg[1L]]] <- FALSE
    read(do.call(fun, args))
  }
  pe <- full_parameters(lavaan::parameterEstimates, fit,
    ci = TRUE, level = conf_level, standardized = FALSE)
  sa <- full_parameters(lavaan::standardizedSolution, fit, type = "std.all", level = conf_level)
  sl <- full_parameters(lavaan::standardizedSolution, fit, type = "std.lv", level = conf_level)
  notes <- unique(c(pe$notes, sa$notes, sl$notes))
  if (is.null(pe$value)) {
    return(list(error = sprintf(
      "The parameter estimates could not be read: %s",
      paste(pe$notes, collapse = " ")
    )))
  }
  key <- function(d) {
    grp <- if (is.null(d$group)) rep(1L, nrow(d)) else d$group
    grp[d$op == ":="] <- 0L
    paste(d$lhs, d$op, d$rhs, grp, sep = "\r")
  }
  pull <- function(d, col, rows_key) {
    if (is.null(d) || is.null(d[[col]])) {
      return(rep(NA_real_, length(rows_key)))
    }
    as.numeric(d[[col]][match(rows_key, key(d))])
  }

  keep <- pt$op %in% c("=~", "~", "~~", "~1", "|", "~*~", ":=") &
    (pt$group == g | pt$op == ":=")
  constraint_all <- sieve_diagram_equalities(pt)
  par <- pt[keep, , drop = FALSE]
  constraint <- constraint_all[keep]
  k <- key(par)
  n <- nrow(par)
  ordered <- lavaan::lavNames(fit, "ov.ord")
  parameterization <- lav_options$parameterization %||% "delta"
  is_def <- par$op == ":="
  is_var <- par$op == "~~" & par$lhs == par$rhs
  # Not free and not a constant the analyst fixed: implied by the model
  # (residual variance of an ordered variable under delta, scaling factor
  # under theta) or set to a sample moment (a fixed exogenous covariate).
  derived <- !is_def & par$free == 0L & (
    par$exo == 1L |
      (is_var & par$lhs %in% ordered & identical(parameterization, "delta")) |
      (par$op == "~*~" & identical(parameterization, "theta"))
  )
  fixed <- !is_def & par$free == 0L & !derived

  est <- pull(pe$value, "est", k)
  se <- pull(pe$value, "se", k)
  lo <- pull(pe$value, "ci.lower", k)
  hi <- pull(pe$value, "ci.upper", k)
  # lavaan reports a standard error of 0 for a parameter that was not
  # estimated. That is a convention, not an estimate of sampling
  # variability, so it is stored as missing.
  no_se <- !is_def & par$free == 0L
  se[no_se] <- NA_real_
  lo[no_se] <- NA_real_
  hi[no_se] <- NA_real_
  std <- function(d) {
    v <- pull(d, "est.std", k)
    s <- pull(d, "se", k)
    l <- pull(d, "ci.lower", k)
    u <- pull(d, "ci.upper", k)
    # A standardized value that is a constant (a variance standardized to
    # 1) has no sampling variability to report.
    const <- !is.na(s) & s == 0
    s[const] <- NA_real_
    l[const] <- NA_real_
    u[const] <- NA_real_
    list(est = v, se = s, lo = l, hi = u)
  }
  all_std <- std(sa$value)
  lv_std <- std(sl$value)

  substantive <- par$op %in% c("=~", "~")
  substantive_names <- unique(c(par$lhs[substantive], par$rhs[substantive]))
  omitted_aux <- setdiff(lav_options$aux, substantive_names)
  auxiliary_row <- par$lhs %in% omitted_aux | par$rhs %in% omitted_aux
  rows <- par[!is_def & !auxiliary_row, , drop = FALSE]
  load <- rows[rows$op == "=~", c("lhs", "rhs"), drop = FALSE]
  reg <- rows[rows$op == "~", c("lhs", "rhs"), drop = FALSE]
  latent <- unique(load$lhs)
  seen <- rows[rows$op %in% c("=~", "~", "~~"), c("lhs", "rhs"), drop = FALSE]
  variables <- unique(as.vector(t(as.matrix(seen))))
  variables <- unique(c(variables, rows$lhs[rows$op %in% c("~1", "|")]))
  observed <- setdiff(variables, latent)
  model_ids <- c(latent, observed)
  if (!length(model_ids)) {
    return(list(error = "The fitted model has no variables to draw."))
  }
  indicator <- unique(load$rhs)
  endogenous <- unique(c(indicator, reg$lhs))
  parent <- stats::setNames(load$lhs[match(model_ids, load$rhs)], model_ids)

  # Measurement height: 0 for an observed variable, 1 for a factor measured
  # by observed variables, one more for each level of factors above that.
  height <- stats::setNames(as.integer(model_ids %in% latent), model_ids)
  for (pass in seq_along(latent)) {
    moved <- FALSE
    for (f in latent) {
      h <- 1L + max(height[load$rhs[load$lhs == f]])
      if (h > height[[f]] && h <= length(latent)) {
        height[[f]] <- h
        moved <- TRUE
      }
    }
    if (!moved) {
      break
    }
  }
  root_of <- function(v) {
    for (step in seq_along(model_ids)) {
      p <- parent[[v]]
      if (is.na(p)) {
        break
      }
      v <- p
    }
    v
  }
  owner <- vapply(model_ids, root_of, character(1))
  roots <- model_ids[is.na(parent)]
  # Structural depth: the longest path of regressions that leads to a
  # variable, counted between the variables that head a measurement block.
  depth <- stats::setNames(rep(NA_integer_, length(model_ids)), model_ids)
  if (nrow(reg)) {
    from <- owner[reg$rhs]
    to <- owner[reg$lhs]
    use <- from != to
    from <- from[use]
    to <- to[use]
    d <- stats::setNames(rep(0L, length(roots)), roots)
    for (pass in seq_along(roots)) {
      moved <- FALSE
      for (i in seq_along(from)) {
        if (d[[to[i]]] < d[[from[i]]] + 1L && d[[from[i]]] + 1L < length(roots)) {
          d[[to[i]]] <- d[[from[i]]] + 1L
          moved <- TRUE
        }
      }
      if (!moved) {
        break
      }
    }
    depth[roots] <- d
  }
  in_lhs <- model_ids %in% reg$lhs
  in_rhs <- model_ids %in% reg$rhs
  structural <- ifelse(in_lhs & in_rhs, "mediator",
    ifelse(in_lhs, "outcome", ifelse(in_rhs, "predictor", "none"))
  )
  is_latent <- model_ids %in% latent
  has_latent_child <- vapply(
    model_ids, function(v) any(load$rhs[load$lhs == v] %in% latent),
    logical(1)
  )
  role <- ifelse(is_latent, ifelse(has_latent_child, "higher_order", "factor"),
    ifelse(model_ids %in% indicator, "indicator",
      ifelse(in_lhs, "outcome", ifelse(in_rhs, "predictor", "variable"))
    )
  )
  level <- ifelse(is_latent | !(model_ids %in% indicator), pmax(height, 1L), 0L)
  block <- ifelse(is_latent | is.na(parent), model_ids, parent)

  var_rows <- rows[rows$op == "~~" & rows$lhs == rows$rhs, , drop = FALSE]
  with_residual <- intersect(model_ids, intersect(endogenous, var_rows$lhs))
  residual_id <- stats::setNames(paste0("e.", with_residual), with_residual)
  while (any(residual_id %in% model_ids)) {
    clash <- residual_id %in% model_ids
    residual_id[clash] <- paste0("e.", residual_id[clash])
  }
  residual_role <- ifelse(with_residual %in% latent | !(with_residual %in% indicator),
    "disturbance", "unique"
  )
  empty <- sieve_diagram_empty()
  n_model <- length(model_ids)
  n_res <- length(with_residual)
  nodes <- empty$nodes[rep(NA_integer_, n_model + n_res), , drop = FALSE]
  rownames(nodes) <- NULL
  nodes$id <- c(model_ids, unname(residual_id))
  nodes$kind <- c(ifelse(is_latent, "latent", "observed"), rep("residual", n_res))
  nodes$role <- c(role, residual_role)
  nodes$structural <- c(structural, rep("none", n_res))
  nodes$level <- as.integer(c(level, level[match(with_residual, model_ids)]))
  nodes$depth <- as.integer(c(depth, rep(NA_integer_, n_res)))
  nodes$block <- c(block, block[match(with_residual, model_ids)])
  nodes$parent <- c(unname(parent), with_residual)
  nodes$exogenous <- c(!(model_ids %in% endogenous), rep(TRUE, n_res))
  nodes$ordered <- c(model_ids %in% ordered, with_residual %in% ordered)
  nodes$visible <- TRUE

  # One edge per drawable parameter row, then the residual paths.
  op <- par$op
  drawn <- op %in% c("=~", "~", "~~", "~1", "|") & !auxiliary_row
  type <- ifelse(op == "=~", "loading",
    ifelse(op == "~", "regression",
      ifelse(op == "~~", ifelse(is_var, "variance", "covariance"),
        ifelse(op == "~1", "intercept",
          ifelse(op == "|", "threshold",
            ifelse(op == "~*~", "scale", "defined")
          )
        )
      )
    )
  )
  node_of <- function(v) ifelse(v %in% with_residual, residual_id[v], v)
  from <- ifelse(op == "=~", par$lhs,
    ifelse(op == "~", par$rhs,
      ifelse(op == "~~", node_of(par$lhs), par$lhs)
    )
  )
  to <- ifelse(op == "=~", par$rhs,
    ifelse(op == "~", par$lhs,
      ifelse(op == "~~", node_of(par$rhs), par$lhs)
    )
  )
  is_cov <- type == "covariance"
  end_l <- par$lhs %in% endogenous
  end_r <- par$rhs %in% endogenous
  # A covariance between two exogenous variables is their covariance. Any
  # other covariance involves the residual of an endogenous variable: a
  # disturbance (a latent variable or an observed outcome) or, failing
  # that, the unique term of an indicator.
  disturbed <- function(v) v %in% endogenous & (v %in% latent | !(v %in% indicator))
  cov_class <- ifelse(!is_cov, NA_character_,
    ifelse(!end_l & !end_r, "exogenous",
      ifelse(disturbed(par$lhs) | disturbed(par$rhs),
        "disturbance", "residual"
      )
    )
  )
  edge_id <- paste0(par$lhs, par$op, par$rhs)

  # Improper values are a property of the solution, judged in the fully
  # standardized metric whatever the display shows. A standardized loading
  # beyond 1 is out of range only for a variable with a single predictor;
  # with correlated predictors it can legitimately exceed 1. A fully
  # standardized variance above 1 is out of range for any variable: the
  # variance of a standardized variable is 1, so a residual variance above
  # it means the part explained by the model is negative.
  incoming <- table(c(load$rhs, reg$lhs))
  single <- par$rhs %in% names(incoming)[incoming == 1L]
  beyond <- function(v) is.finite(v) & abs(v) > 1 + 1e-8
  improper <- (type == "variance" & is.finite(est) & est < 0) |
    (type == "variance" & is.finite(all_std$est) & all_std$est > 1 + 1e-8) |
    (type == "covariance" & beyond(all_std$est)) |
    (type == "loading" & single & beyond(all_std$est))

  parameters <- data.frame(
    id = edge_id, lhs = par$lhs, op = par$op, rhs = par$rhs,
    group = as.integer(par$group), type = type,
    label = par$label, plabel = par$plabel, free = as.integer(par$free),
    fixed = fixed, derived = derived, exo = as.integer(par$exo),
    constraint = constraint,
    est = est, se = se, ci_lower = lo, ci_upper = hi,
    std_all = all_std$est, std_all_se = all_std$se,
    std_all_ci_lower = all_std$lo, std_all_ci_upper = all_std$hi,
    std_lv = lv_std$est, std_lv_se = lv_std$se,
    std_lv_ci_lower = lv_std$lo, std_lv_ci_upper = lv_std$hi,
    improper = improper, edge = ifelse(drawn, edge_id, NA_character_),
    stringsAsFactors = FALSE
  )
  rownames(parameters) <- NULL

  n_edge <- sum(drawn)
  edges <- empty$edges[rep(NA_integer_, n_edge + n_res), , drop = FALSE]
  rownames(edges) <- NULL
  at <- seq_len(n_edge)
  edges$id[at] <- edge_id[drawn]
  edges$from[at] <- from[drawn]
  edges$to[at] <- to[drawn]
  edges$type[at] <- type[drawn]
  edges$cov_class[at] <- cov_class[drawn]
  edges$op[at] <- par$op[drawn]
  edges$lhs[at] <- par$lhs[drawn]
  edges$rhs[at] <- par$rhs[drawn]
  edges$par[at] <- which(drawn)
  edges$est[at] <- est[drawn]
  edges$se[at] <- se[drawn]
  edges$ci_lower[at] <- lo[drawn]
  edges$ci_upper[at] <- hi[drawn]
  edges$free[at] <- as.integer(par$free[drawn])
  edges$fixed[at] <- fixed[drawn]
  edges$derived[at] <- derived[drawn]
  edges$constraint[at] <- constraint[drawn]
  edges$improper[at] <- improper[drawn]
  if (n_res) {
    # The residual enters its variable with a coefficient of 1. That path
    # is part of the model's structure, not a row of the parameter table.
    rp <- n_edge + seq_len(n_res)
    edges$id[rp] <- paste0(residual_id, "->", with_residual)
    edges$from[rp] <- unname(residual_id)
    edges$to[rp] <- with_residual
    edges$type[rp] <- "residual_path"
    edges$est[rp] <- 1
    edges$free[rp] <- 0L
    edges$fixed[rp] <- TRUE
    edges$derived[rp] <- FALSE
    edges$improper[rp] <- FALSE
  }
  edges$visible <- TRUE

  # How each latent variable got its scale, read from the parameter table.
  # A loading fixed at zero (a cross-loading the model rules out) sets no
  # scale and is left out of the classification.
  scaling <- vapply(latent, function(f) {
    v <- which(par$op == "~~" & par$lhs == f & par$rhs == f)
    l <- which(par$op == "=~" & par$lhs == f)
    l <- l[!(fixed[l] & is.finite(est[l]) & est[l] == 0)]
    var_fixed_1 <- length(v) == 1L && fixed[v] && isTRUE(est[v] == 1)
    var_free <- length(v) == 1L && par$free[v] > 0L
    load_fixed <- any(fixed[l])
    marker <- sum(fixed[l]) == 1L && isTRUE(all(est[l][fixed[l]] == 1))
    if (var_fixed_1 && !load_fixed) {
      "unit_variance"
    } else if (marker && var_free) {
      "marker"
    } else {
      "other"
    }
  }, character(1))
  identification <- if (!length(scaling)) {
    NA_character_
  } else if (all(scaling == "unit_variance")) {
    "unit_variance"
  } else if (all(scaling == "marker")) {
    "marker"
  } else if (all(scaling %in% c("unit_variance", "marker"))) {
    "mixed"
  } else {
    "other"
  }
  syntax <- sieve_diagram_syntax(fit, pt)
  nobs <- lavaan::lavInspect(fit, "nobs")
  sieve_spec <- attr(fit, "sieve_spec")
  # An exploratory block (lavaan's efa() modifier) makes the measurement
  # part rotated rather than confirmatory; a growth model is fit by
  # lavaan::growth() and says so in its options.
  exploratory <- !is.null(pt$efa) && any(nzchar(pt$efa) & !is.na(pt$efa))
  model_kind <- if (identical(lav_options$model.type, "growth")) {
    "growth"
  } else if (exploratory) {
    "esem"
  } else if (!length(latent)) {
    "path"
  } else if (nrow(reg)) {
    "sem"
  } else if (any(role == "higher_order")) {
    "higher_order"
  } else {
    "cfa"
  }
  # Constraints beyond the equalities that the constraint labels show:
  # inequalities, and equalities that relate parameters by an expression.
  constraints <- pt[pt$op %in% c("==", "<", ">"), c("lhs", "op", "rhs"), drop = FALSE]
  rownames(constraints) <- NULL
  simple <- constraints$op == "==" &
    grepl("^[A-Za-z.][A-Za-z0-9_.]*$", constraints$lhs) &
    grepl("^[A-Za-z.][A-Za-z0-9_.]*$", constraints$rhs)
  constraints$kind <- ifelse(simple, "equality", "other")
  # Bounds the analyst wrote (lavaan turns a simple inequality such as
  # `l3 > 0.1` into a bound on that parameter). Automatic bounds (the
  # `bounds` option) apply to every parameter and are stated as such.
  bounded <- data.frame(
    parameter = character(0), lower = numeric(0), upper = numeric(0),
    stringsAsFactors = FALSE
  )
  automatic <- lav_options$bounds
  automatic <- !is.null(automatic) && !identical(automatic, "none") && !isFALSE(automatic)
  if (!automatic && (!is.null(pt$lower) || !is.null(pt$upper))) {
    lo <- if (is.null(pt$lower)) rep(-Inf, nrow(pt)) else as.numeric(pt$lower)
    hi <- if (is.null(pt$upper)) rep(Inf, nrow(pt)) else as.numeric(pt$upper)
    rows <- which((is.finite(lo) | is.finite(hi)) & pt$free > 0L &
      pt$op %in% c("=~", "~", "~~", "~1", "|") & pt$group %in% c(0L, g))
    bounded <- data.frame(
      parameter = paste(pt$lhs[rows], pt$op[rows], pt$rhs[rows]),
      lower = lo[rows], upper = hi[rows], stringsAsFactors = FALSE
    )
  }
  meta <- list(
    model = syntax$model,
    model_source = syntax$source,
    model_note = syntax$note,
    model_kind = model_kind,
    omitted_aux = omitted_aux,
    estimator = sieve_spec$estimator %||% sieve_diagram_estimator(lav_options),
    lavaan_options = list(
      estimator = lav_options$estimator, se = lav_options$se, test = lav_options$test,
      missing = lav_options$missing, parameterization = parameterization,
      std.lv = lav_options$std.lv, orthogonal = lav_options$orthogonal,
      fixed.x = lav_options$fixed.x, meanstructure = lav_options$meanstructure,
      bounds = lav_options$bounds, rotation = lav_options$rotation,
      model.type = lav_options$model.type
    ),
    ordered = intersect(model_ids, ordered),
    nobs = as.integer(nobs[g]),
    ngroups = as.integer(ngroups),
    group = as.integer(g),
    group_label = if (ngroups > 1L) group_labels[g] else NA_character_,
    group_labels = if (ngroups > 1L) group_labels else character(0),
    group_variable = if (ngroups > 1L) {
      tryCatch(lavaan::lavInspect(fit, "group"), error = function(e) NA_character_)
    } else {
      NA_character_
    },
    identification = identification,
    identification_detail = data.frame(
      latent = latent, scaling = unname(scaling),
      stringsAsFactors = FALSE
    ),
    versions = sieve_diagram_versions(),
    conf_level = conf_level,
    constraints = constraints,
    bounded = bounded
  )
  list(
    nodes = nodes, edges = edges, parameters = parameters, meta = meta,
    warnings = notes, error = NULL
  )
}

# ==== stage 2: labels and node sizes ====

#' Drawing constants of the diagram layout
#'
#' Lengths are in layout units, which are inches at the recommended size.
#' Renderers read the same constants, so a self-loop or a residual circle
#' has one size everywhere.
#'
#' @return A named list.
#' @author Ken Kelley
#' @noRd
sieve_diagram_geometry <- function() {
  list(
    base_pt = 10, edge_pt = 9, line_height = 1.2,
    pad_x = 0.10, pad_y = 0.08,
    observed_min = c(w = 0.60, h = 0.42),
    latent_min = c(w = 0.95, h = 0.70),
    residual_d = 0.44, residual_gap = 0.34,
    loop_length = 0.30, loop_width = 0.34, loop_ratio = 0.55, loop_width_max = 0.85,
    loop_sink = 0.10, label_room = 0.24, value_width = 0.42,
    # The variance loop of a residual term is round: the ellipse it is cut
    # from runs from `residual_loop_sink` inside the term to
    # `residual_loop_length` beyond it, and that span equals its width, so
    # both diameters are 0.34 (the narrowest loop a latent variable has).
    # A loop as long as it is wide beyond the term, with the sink added to
    # its length, would be taller than wide and read as a narrow oval whose
    # two arrowheads nearly meet.
    residual_loop_length = 0.20, residual_loop_width = 0.34, residual_loop_sink = 0.14,
    sibling_gap = c(horizontal = 0.20, vertical = 0.14),
    rescov_spacing = 0.90,
    group_gap = c(horizontal = 0.60, vertical = 0.40),
    loading_gap = c(horizontal = 1.00, vertical = 1.40),
    loading_gap_max = 2.60, loading_ratio = 0.22,
    structural_gap = c(left_right = 1.70, top_down = 1.30), cell_gap = 1.20,
    margin = 0.30,
    arc_min = 0.40, arc_base_max = 0.95, arc_ratio = 0.15, arc_step = 0.34,
    loop_clearance = 50, loop_margin = 12,
    label_pos = c(loading = 0.62, regression = 0.30)
  )
}

#' Approximate width of text, in inches
#'
#' Node sizes have to be known before any graphics device exists, so text
#' is measured with average advance widths of a sans serif face rather than
#' with a device. The estimate is deliberately a little generous.
#'
#' @param x Character vector (single lines).
#' @param pt Type size in points.
#' @return Numeric vector of widths in inches.
#' @author Ken Kelley
#' @noRd
sieve_diagram_text_width <- function(x, pt = 10) {
  narrow <- c(
    "i", "j", "l", "I", "|", ".", ",", ":", ";", "'", "!", " ", "t", "f", "r",
    "(", ")", "[", "]", "-", "/"
  )
  wide <- c("m", "w", "M", "W", "@", "%")
  vapply(x, function(s) {
    ch <- strsplit(s, "", fixed = TRUE)[[1]]
    if (!length(ch)) {
      return(0)
    }
    em <- rep(0.56, length(ch))
    em[grepl("^[A-Z]$", ch)] <- 0.69
    em[ch %in% narrow] <- 0.30
    em[ch %in% wide] <- 0.86
    sum(em) * pt / 72
  }, numeric(1), USE.NAMES = FALSE)
}

#' Wrap labels at word boundaries
#'
#' Breaks a label into lines of at most `width` characters where it can:
#' at a space (which is dropped at the break) or after an underscore,
#' hyphen, slash, or period (which is kept). A single word longer than
#' `width` stays whole. Nothing is ever cut off. The lines of one label are
#' made as even as the words allow: the label takes the number of lines it
#' needs at `width`, and is then broken at the narrowest width that still
#' gives that number, so no line holds a single leftover word.
#'
#' @param x Character vector of labels.
#' @param width Maximum line length in characters, or `Inf`.
#' @return Character vector with newlines at the breaks.
#' @author Ken Kelley
#' @noRd
sieve_diagram_wrap <- function(x, width) {
  fill <- function(tokens, width) {
    lines <- character(0)
    line <- ""
    for (token in tokens) {
      trial <- paste0(line, token)
      if (nchar(trimws(trial)) > width && nzchar(trimws(line))) {
        lines <- c(lines, trimws(line))
        line <- token
      } else {
        line <- trial
      }
    }
    lines <- c(lines, trimws(line))
    lines[nzchar(lines)]
  }
  one <- function(s) {
    if (is.na(s) || !is.finite(width) || nchar(s) <= width) {
      return(s)
    }
    tokens <- regmatches(s, gregexpr("[^ _./-]+[_./-]* *|[_./-]+ *| +", s, perl = TRUE))[[1]]
    lines <- fill(tokens, width)
    # The same number of lines at the narrowest width that still gives it,
    # so the lines are even and the last one is not a single short word.
    narrow <- min(width, ceiling(nchar(s) / length(lines)))
    while (narrow < width && length(fill(tokens, narrow)) > length(lines)) {
      narrow <- narrow + 1L
    }
    paste(fill(tokens, narrow), collapse = "\n")
  }
  vapply(x, one, character(1), USE.NAMES = FALSE)
}

#' Set the display labels and the node sizes
#'
#' Resolves each node's label from `labels`, then the name map, then the
#' model name; applies the label mode and the wrapping; and sizes each node
#' to its text. Nodes of one kind on one level share a size, so rows line
#' up and the spacing within a group is even.
#'
#' @param spec A specification holding nodes and options.
#' @return The specification with `label`, `label_wrapped`, `label_full`,
#'   `short`, `w`, and `h` filled, and `label_key` set.
#' @author Ken Kelley
#' @noRd
sieve_diagram_relabel <- function(spec) {
  g <- sieve_diagram_geometry()
  o <- spec$meta$options
  nodes <- spec$nodes
  name_map <- o$name_map %||% spec$meta$name_map_fit
  spec$meta["name_map"] <- list(name_map)
  is_res <- nodes$kind == "residual"
  full <- nodes$id
  for (map in list(name_map, o$labels)) {
    if (!is.null(map)) {
      hit <- unname(map[nodes$id])
      full[!is.na(hit)] <- hit[!is.na(hit)]
    }
  }
  full[is_res] <- ifelse(nodes$role[is_res] == "disturbance", "d", "e")
  obs <- nodes$kind == "observed"
  # Short identifiers are fixed by the model: the names themselves when
  # they are already short, otherwise V1, V2, ... in model order.
  short <- full
  short[obs] <- if (all(nchar(nodes$id[obs]) <= 6L)) {
    nodes$id[obs]
  } else {
    paste0("V", seq_len(sum(obs)))
  }
  nodes$label_full <- unname(full)
  nodes$short <- unname(short)
  nodes$label <- if (identical(o$label_mode, "short")) nodes$short else nodes$label_full
  wrap <- o$wrap
  if (is.null(wrap)) {
    # The automatic width: 16 characters, widened until no label needs more
    # than four lines (a long item wording reads better in a wider box).
    shown <- nodes$label[!is_res]
    lines_at <- function(width) {
      max(lengths(strsplit(sieve_diagram_wrap(shown, width), "\n", fixed = TRUE)))
    }
    wrap <- max(16L, as.integer(ceiling(max(nchar(shown)) / 4)))
    while (wrap < 40L && lines_at(wrap) > 4L) {
      wrap <- wrap + 1L
    }
  }
  spec$meta$wrap <- wrap
  nodes$label_wrapped <- sieve_diagram_wrap(nodes$label, wrap)

  lines <- strsplit(nodes$label_wrapped, "\n", fixed = TRUE)
  text_w <- vapply(
    lines, function(l) max(c(0, sieve_diagram_text_width(l, g$base_pt))),
    numeric(1)
  )
  text_h <- lengths(lines) * g$base_pt * g$line_height / 72
  w <- pmax(g$observed_min[["w"]], text_w + 2 * g$pad_x)
  h <- pmax(g$observed_min[["h"]], text_h + 2 * g$pad_y)
  lat <- nodes$kind == "latent"
  # The label rectangle is inscribed in the ellipse.
  w[lat] <- pmax(g$latent_min[["w"]], sqrt(2) * text_w[lat] + g$pad_x)
  h[lat] <- pmax(g$latent_min[["h"]], sqrt(2) * text_h[lat] + g$pad_y)
  w[is_res] <- g$residual_d
  h[is_res] <- g$residual_d
  size_group <- ifelse(is_res, "residual",
    ifelse(lat, paste("latent", nodes$level),
      ifelse(nodes$role == "indicator", "indicator", "observed")
    )
  )
  nodes$w <- as.numeric(stats::ave(w, size_group, FUN = max))
  nodes$h <- as.numeric(stats::ave(h, size_group, FUN = max))
  spec$nodes <- nodes
  spec$label_key <- data.frame(
    id = nodes$id[obs], short = nodes$short[obs], label = nodes$label_full[obs],
    stringsAsFactors = FALSE
  )
  spec
}

# ==== stage 3: layout ====

#' Lay out measurement blocks
#'
#' Places one or more measurement trees (a latent variable with its
#' indicators, recursively for higher-order factors) with their unique
#' terms and the disturbances of the lower-order factors. Siblings are
#' packed side by side along one axis, levels step along the other, and
#' `side` says where the indicators go relative to their factor.
#'
#' @param roots Ids of the variables that head the trees, in order.
#' @param side `"below"`, `"above"`, `"right"`, or `"left"`.
#' @param nodes The node table (with sizes).
#' @param edges The edge table.
#' @param gaps Optional named numeric vector of minimum distances between a
#'   level (the name) and the level beneath it, so that separate calls can
#'   share one spacing.
#' @return A data frame with `id`, `x`, `y`, `out_x`, `out_y`. With a
#'   single root the root is at the origin. The distances used between
#'   levels are returned as the attribute `gaps`.
#' @author Ken Kelley
#' @noRd
sieve_diagram_blocks <- function(roots, side, nodes, edges, gaps = NULL) {
  g <- sieve_diagram_geometry()
  idx <- stats::setNames(seq_len(nrow(nodes)), nodes$id)
  horizontal <- side %in% c("above", "below")
  along <- if (horizontal) "horizontal" else "vertical"
  breadth <- function(id) if (horizontal) nodes$w[idx[[id]]] else nodes$h[idx[[id]]]
  thick <- function(id) if (horizontal) nodes$h[idx[[id]]] else nodes$w[idx[[id]]]
  value_room <- if (horizontal) g$value_width else g$label_room
  is_res <- nodes$kind == "residual"
  residual_of <- stats::setNames(nodes$id[is_res], nodes$parent[is_res])
  loadings <- edges[edges$type == "loading", c("from", "to"), drop = FALSE]
  # Indicators whose unique terms are joined by a residual covariance: the
  # arc between the terms and their loops, which turn aside for it, need
  # more room than the plain sibling gap gives when the boxes are small.
  rescov <- edges$type == "covariance" & edges$cov_class %in% "residual"
  joined <- unique(c(edges$lhs[rescov], edges$rhs[rescov]))
  kids_of <- function(id) {
    k <- unique(loadings$to[loadings$from == id])
    k[nodes$parent[idx[k]] %in% id]
  }
  pack <- function(id, is_root) {
    kids <- kids_of(id)
    lo <- -breadth(id) / 2
    hi <- breadth(id) / 2
    if (!is_root && nodes$kind[idx[[id]]] == "latent" && id %in% names(residual_of)) {
      hi <- hi + g$residual_gap + g$residual_d + g$residual_loop_length + value_room
    }
    if (!length(kids)) {
      return(list(s = stats::setNames(0, id), lo = lo, hi = hi))
    }
    gap <- if (any(nodes$kind[idx[kids]] == "latent")) g$group_gap[[along]] else g$sibling_gap[[along]]
    cursor <- 0
    s <- numeric(0)
    for (i in seq_along(kids)) {
      sub <- pack(kids[i], FALSE)
      step <- gap
      if (i > 1L && any(kids[c(i - 1L, i)] %in% joined)) {
        step <- max(gap, g$rescov_spacing - (breadth(kids[i - 1L]) + breadth(kids[i])) / 2)
      }
      at <- if (i == 1L) -sub$lo else cursor + step - sub$lo
      s <- c(s, sub$s + at)
      cursor <- at + sub$hi
    }
    center <- (s[[kids[1L]]] + s[[kids[length(kids)]]]) / 2
    s <- s - center
    list(
      s = c(stats::setNames(0, id), s),
      lo = min(lo, -center), hi = max(hi, cursor - center)
    )
  }
  cursor <- 0
  s <- numeric(0)
  for (i in seq_along(roots)) {
    sub <- pack(roots[i], TRUE)
    at <- if (i == 1L) -sub$lo else cursor + g$group_gap[[along]] + 0.10 - sub$lo
    s <- c(s, sub$s + at)
    cursor <- at + sub$hi
  }
  s <- if (length(roots) == 1L) s - s[[roots]] else s - cursor / 2
  ids <- names(s)
  lev <- nodes$level[idx[ids]]
  levels <- sort(unique(lev), decreasing = TRUE)
  t_level <- stats::setNames(numeric(length(levels)), levels)
  used <- numeric(0)
  for (j in seq_along(levels)[-1L]) {
    upper <- ids[lev == levels[j - 1L]]
    lower <- ids[lev == levels[j]]
    span <- max(c(0, vapply(upper, function(id) {
      k <- kids_of(id)
      if (length(k)) diff(range(s[k])) else 0
    }, numeric(1))))
    gap <- min(g$loading_gap_max, max(g$loading_gap[[along]], g$loading_ratio * span))
    shared <- gaps[as.character(levels[j - 1L])]
    if (length(shared) && !is.na(shared)) {
      gap <- max(gap, shared)
    }
    used[as.character(levels[j - 1L])] <- gap
    t_level[j] <- t_level[j - 1L] + max(vapply(upper, thick, numeric(1))) / 2 + gap +
      max(vapply(lower, thick, numeric(1))) / 2
  }
  t <- stats::setNames(unname(t_level[as.character(lev)]), ids)
  # Residual terms of the variables inside the blocks: a unique term beyond
  # its indicator, a disturbance beside its lower-order factor.
  inner <- ids[!(ids %in% roots) & ids %in% names(residual_of)]
  out_s <- stats::setNames(numeric(length(ids)), ids)
  out_t <- stats::setNames(ifelse(ids %in% roots, -1, 1), ids)
  lower <- ids[!(ids %in% roots) & nodes$kind[idx[ids]] == "latent"]
  out_s[lower] <- 1
  out_t[lower] <- 0
  for (id in inner) {
    r <- residual_of[[id]]
    reach <- g$residual_gap + g$residual_d / 2
    if (nodes$kind[idx[[id]]] == "latent") {
      s[r] <- s[[id]] + breadth(id) / 2 + reach
      t[r] <- t[[id]]
      out_s[r] <- 1
      out_t[r] <- 0
    } else {
      s[r] <- s[[id]]
      t[r] <- t[[id]] + thick(id) / 2 + reach
      out_s[r] <- 0
      out_t[r] <- 1
    }
  }
  turn <- switch(side,
    below = list(x = s, y = -t, ox = out_s, oy = -out_t),
    above = list(x = s, y = t, ox = out_s, oy = out_t),
    right = list(x = t, y = -s, ox = out_t, oy = -out_s),
    left = list(x = -t, y = -s, ox = -out_t, oy = -out_s)
  )
  out <- data.frame(
    id = names(s), x = unname(turn$x), y = unname(turn$y),
    out_x = unname(turn$ox[names(s)]), out_y = unname(turn$oy[names(s)]),
    stringsAsFactors = FALSE
  )
  attr(out, "gaps") <- used
  out
}

#' Compute node coordinates
#'
#' A model without regressions is one row (or column) of measurement
#' blocks. A model with regressions is arranged in columns (or rows) by
#' structural depth, each variable carrying its measurement block on a side
#' away from the flow of the regressions and its disturbance on a free
#' side.
#'
#' @param nodes The node table (with sizes).
#' @param edges The edge table.
#' @param layout `"top_down"` or `"left_right"`.
#' @return The node table with `x`, `y`, `out_x`, and `out_y` filled.
#' @author Ken Kelley
#' @noRd
sieve_diagram_layout <- function(nodes, edges, layout) {
  g <- sieve_diagram_geometry()
  idx <- stats::setNames(seq_len(nrow(nodes)), nodes$id)
  is_res <- nodes$kind == "residual"
  residual_of <- stats::setNames(nodes$id[is_res], nodes$parent[is_res])
  roots <- nodes$id[!is_res & is.na(nodes$parent)]
  put <- function(nodes, placed, dx = 0, dy = 0) {
    at <- idx[placed$id]
    nodes$x[at] <- placed$x + dx
    nodes$y[at] <- placed$y + dy
    nodes$out_x[at] <- placed$out_x
    nodes$out_y[at] <- placed$out_y
    nodes
  }
  structural <- any(edges$type == "regression") && !all(is.na(nodes$depth[idx[roots]]))
  if (!structural) {
    side <- if (identical(layout, "top_down")) "below" else "right"
    return(put(nodes, sieve_diagram_blocks(roots, side, nodes, edges)))
  }

  flow_lr <- identical(layout, "left_right")
  concrete <- if (flow_lr) {
    c(cross_first = "above", cross_last = "below", flow_back = "left", flow_fwd = "right")
  } else {
    c(cross_first = "left", cross_last = "right", flow_back = "above", flow_fwd = "below")
  }
  across <- c(
    cross_first = "cross_last", cross_last = "cross_first",
    flow_back = "cross_last", flow_fwd = "cross_last"
  )
  unit <- list(above = c(0, 1), below = c(0, -1), left = c(-1, 0), right = c(1, 0))
  reach <- function(id, side) {
    if (side %in% c("above", "below")) nodes$h[idx[[id]]] / 2 else nodes$w[idx[[id]]] / 2
  }
  depth <- nodes$depth[idx[roots]]
  depth[is.na(depth)] <- 0L
  n_col <- max(depth) + 1L
  has_kids <- function(id) any(edges$type == "loading" & edges$from == id &
    nodes$parent[idx[edges$to]] %in% id)
  cells <- vector("list", length(roots))
  names(cells) <- roots
  block_side <- function(col, i, n_c) {
    if (n_c == 1L || i == 1L) {
      "cross_first"
    } else if (i == n_c) {
      "cross_last"
    } else if (col == 1L) {
      "flow_back"
    } else if (col == n_col) {
      "flow_fwd"
    } else {
      "cross_first"
    }
  }
  # Blocks that hang the same way share one distance between a factor and
  # its indicators, so their indicator rows line up.
  shared <- list(horizontal = numeric(0), vertical = numeric(0))
  for (col in seq_len(n_col)) {
    members <- roots[depth == col - 1L]
    for (i in seq_along(members)) {
      if (has_kids(members[i])) {
        side <- concrete[[block_side(col, i, length(members))]]
        way <- if (side %in% c("above", "below")) "horizontal" else "vertical"
        got <- attr(sieve_diagram_blocks(members[i], side, nodes, edges), "gaps")
        for (lev in names(got)) {
          shared[[way]][lev] <- max(shared[[way]][lev], got[[lev]], na.rm = TRUE)
        }
      }
    }
  }
  for (col in seq_len(n_col)) {
    members <- roots[depth == col - 1L]
    for (i in seq_along(members)) {
      id <- members[i]
      n_c <- length(members)
      blocked <- has_kids(id)
      b_side <- block_side(col, i, n_c)
      placed <- if (blocked) {
        way <- if (concrete[[b_side]] %in% c("above", "below")) "horizontal" else "vertical"
        sieve_diagram_blocks(id, concrete[[b_side]], nodes, edges, gaps = shared[[way]])
      } else {
        data.frame(id = id, x = 0, y = 0, out_x = 0, out_y = 0, stringsAsFactors = FALSE)
      }
      # The free side of the variable itself: back along the flow for a
      # source, forward for a sink, otherwise away from its indicators.
      free_side <- if (col == 1L && !(blocked && b_side == "flow_back")) {
        "flow_back"
      } else if (col == n_col && !(blocked && b_side == "flow_fwd")) {
        "flow_fwd"
      } else if (blocked) {
        across[[b_side]]
      } else if (i == n_c) {
        "cross_last"
      } else {
        "cross_first"
      }
      out <- unit[[concrete[[free_side]]]]
      placed$out_x[placed$id == id] <- out[1L]
      placed$out_y[placed$id == id] <- out[2L]
      if (id %in% names(residual_of)) {
        d_side <- if (col == n_col && !(blocked && b_side == "flow_fwd")) {
          "flow_fwd"
        } else if (blocked) {
          across[[b_side]]
        } else if (i == n_c) {
          "cross_last"
        } else {
          "cross_first"
        }
        way <- unit[[concrete[[d_side]]]]
        far <- reach(id, concrete[[d_side]]) + g$residual_gap + g$residual_d / 2
        placed <- rbind(placed, data.frame(
          id = residual_of[[id]], x = way[1L] * far, y = way[2L] * far,
          out_x = way[1L], out_y = way[2L], stringsAsFactors = FALSE
        ))
      }
      # The room the cell needs around its head variable, with space for
      # the self-loops and their values.
      at <- idx[placed$id]
      loop <- ifelse(nodes$kind[at] == "residual" | placed$id == id, 1, 0) *
        (sieve_diagram_loop_length(nodes, at) +
          ifelse(placed$out_x != 0, g$value_width, g$label_room))
      cells[[id]] <- list(
        placed = placed,
        left = -min(placed$x - nodes$w[at] / 2 + pmin(0, placed$out_x) * loop),
        right = max(placed$x + nodes$w[at] / 2 + pmax(0, placed$out_x) * loop),
        down = -min(placed$y - nodes$h[at] / 2 + pmin(0, placed$out_y) * loop),
        up = max(placed$y + nodes$h[at] / 2 + pmax(0, placed$out_y) * loop)
      )
    }
  }
  flow_gap <- g$structural_gap[[layout]]
  flow_at <- 0
  for (col in seq_len(n_col)) {
    members <- roots[depth == col - 1L]
    if (!length(members)) {
      next
    }
    part <- cells[members]
    back <- max(vapply(part, function(cl) if (flow_lr) cl$left else cl$up, numeric(1)))
    fwd <- max(vapply(part, function(cl) if (flow_lr) cl$right else cl$down, numeric(1)))
    flow_at <- if (col == 1L) 0 else flow_at + back
    cross <- numeric(length(members))
    for (i in seq_along(members)[-1L]) {
      before <- part[[i - 1L]]
      this <- part[[i]]
      cross[i] <- cross[i - 1L] + g$cell_gap +
        if (flow_lr) before$down + this$up else before$right + this$left
    }
    cross <- cross - (cross[1L] + cross[length(cross)]) / 2
    for (i in seq_along(members)) {
      nodes <- if (flow_lr) {
        put(nodes, part[[i]]$placed, dx = flow_at, dy = -cross[i])
      } else {
        put(nodes, part[[i]]$placed, dx = cross[i], dy = -flow_at)
      }
    }
    flow_at <- flow_at + fwd + flow_gap
  }
  sieve_diagram_clear_columns(nodes, edges, roots, depth, cells, flow_lr)
}

#' Keep a column clear of the regressions that pass it by
#'
#' A regression between variables two or more columns apart is a straight
#' arrow that crosses the columns in between. If a variable of such a
#' column stood on that line (as a mediator does when predictor, mediator,
#' and outcome are each alone in their column), the direct arrow would run
#' through it: the figure would read as a chain, and the direct coefficient
#' would appear to belong to another arrow. Each column that a longer
#' regression crosses is therefore moved, as a whole, across the flow by
#' the smallest distance that takes its variables, their indicators, and
#' their disturbance terms off every such line. The predictor to mediator
#' and mediator to outcome arrows then run at a slant and the direct arrow
#' runs free, which is the usual drawing of a mediation model.
#'
#' The move depends only on the model and the node sizes, so it is the same
#' whatever is shown or hidden.
#'
#' @param nodes The node table with coordinates.
#' @param edges The edge table.
#' @param roots Identifiers of the variables that head a cell.
#' @param depth Structural depth of each root (zero based).
#' @param cells The cells built by `sieve_diagram_layout()`.
#' @param flow_lr `TRUE` when the regressions run left to right.
#' @return The node table, with the columns that were in the way moved.
#' @author Ken Kelley
#' @noRd
sieve_diagram_clear_columns <- function(nodes, edges, roots, depth, cells, flow_lr) {
  g <- sieve_diagram_geometry()
  idx <- stats::setNames(seq_len(nrow(nodes)), nodes$id)
  depth_of <- stats::setNames(depth, roots)
  reg <- edges[edges$type == "regression" & edges$from %in% roots & edges$to %in% roots,
    c("from", "to"),
    drop = FALSE
  ]
  long <- reg[abs(depth_of[reg$to] - depth_of[reg$from]) >= 2, , drop = FALSE]
  if (!nrow(long)) {
    return(nodes)
  }
  clear <- g$cell_gap / 4
  flow <- function(id) if (flow_lr) nodes$x[idx[[id]]] else -nodes$y[idx[[id]]]
  cross <- function(id) if (flow_lr) nodes$y[idx[[id]]] else nodes$x[idx[[id]]]
  for (pass in seq_len(4L)) {
    moved <- FALSE
    for (col in sort(unique(depth))) {
      members <- roots[depth == col]
      over <- which(pmin(depth_of[long$from], depth_of[long$to]) < col &
        pmax(depth_of[long$from], depth_of[long$to]) > col)
      if (!length(members) || !length(over)) {
        next
      }
      part <- cells[members]
      # The stretch of the flow the column occupies, and where each cell
      # lies across it.
      at <- flow(members[1L])
      back <- max(vapply(part, function(cl) if (flow_lr) cl$left else cl$up, numeric(1)))
      fwd <- max(vapply(part, function(cl) if (flow_lr) cl$right else cl$down, numeric(1)))
      low <- vapply(seq_along(members), function(i) {
        cross(members[i]) - if (flow_lr) part[[i]]$down else part[[i]]$left
      }, numeric(1))
      high <- vapply(seq_along(members), function(i) {
        cross(members[i]) + if (flow_lr) part[[i]]$up else part[[i]]$right
      }, numeric(1))
      # The band each passing regression sweeps while it crosses the column.
      band <- vapply(over, function(r) {
        f0 <- flow(long$from[r])
        f1 <- flow(long$to[r])
        c0 <- cross(long$from[r])
        c1 <- cross(long$to[r])
        if (abs(f1 - f0) < 1e-9) {
          return(range(c0, c1))
        }
        range(c0 + (c(at - back, at + fwd) - f0) / (f1 - f0) * (c1 - c0))
      }, numeric(2))
      hits <- function(shift) {
        sum(outer(seq_len(ncol(band)), seq_along(members), function(b, m) {
          band[2L, b] > low[m] + shift - clear & band[1L, b] < high[m] + shift + clear
        }))
      }
      if (hits(0) == 0) {
        next
      }
      # Candidate moves: each cell taken just above, or just below, each band.
      slack <- 1e-6
      up <- as.vector(outer(band[2L, ], low, function(b, l) b - l + clear + slack))
      down <- as.vector(outer(band[1L, ], high, function(b, h) b - h - clear - slack))
      moves <- c(up, down)
      left <- vapply(moves, hits, numeric(1))
      best <- moves[order(left, abs(moves), -moves)][1L]
      inside <- idx[unlist(lapply(part, function(cl) cl$placed$id), use.names = FALSE)]
      if (flow_lr) {
        nodes$y[inside] <- nodes$y[inside] + best
      } else {
        nodes$x[inside] <- nodes$x[inside] + best
      }
      moved <- TRUE
    }
    if (!moved) {
      break
    }
  }
  nodes
}

#' Place the nodes: resolve the layout, compute it, apply the overrides
#'
#' @param spec A specification whose nodes have sizes.
#' @return The specification with node coordinates and `meta$layout` set.
#' @author Ken Kelley
#' @noRd
sieve_diagram_place <- function(spec) {
  o <- spec$meta$options
  layout <- o$layout
  if (identical(layout, "auto")) {
    layout <- if (any(spec$edges$type == "regression")) "left_right" else "top_down"
  }
  nodes <- sieve_diagram_layout(spec$nodes, spec$edges, layout)
  positions <- o$positions
  if (!is.null(positions)) {
    unknown <- setdiff(positions$id, nodes$id)
    if (length(unknown)) {
      stop(sprintf(
        "`positions` names node(s) that are not in the diagram: %s.",
        paste(unknown, collapse = ", ")
      ), call. = FALSE)
    }
    at <- match(positions$id, nodes$id)
    dx <- positions$x - nodes$x[at]
    dy <- positions$y - nodes$y[at]
    # A residual term that was not itself listed moves with its variable.
    follow <- which(nodes$kind == "residual" & !(nodes$id %in% positions$id) &
      nodes$parent %in% positions$id)
    lead <- match(nodes$parent[follow], positions$id)
    nodes$x[follow] <- nodes$x[follow] + dx[lead]
    nodes$y[follow] <- nodes$y[follow] + dy[lead]
    nodes$x[at] <- positions$x
    nodes$y[at] <- positions$y
    res <- which(nodes$kind == "residual")
    anchor <- match(nodes$parent[res], nodes$id)
    ox <- nodes$x[res] - nodes$x[anchor]
    oy <- nodes$y[res] - nodes$y[anchor]
    len <- sqrt(ox^2 + oy^2)
    ok <- len > 1e-8
    nodes$out_x[res[ok]] <- (ox / len)[ok]
    nodes$out_y[res[ok]] <- (oy / len)[ok]
    # A node placed on top of another would hide it, and the diagram would
    # still look complete. That is refused here rather than drawn.
    moved <- c(at, follow)
    for (i in moved) {
      over <- which(abs(nodes$x - nodes$x[i]) < (nodes$w + nodes$w[i]) / 2 - 1e-9 &
        abs(nodes$y - nodes$y[i]) < (nodes$h + nodes$h[i]) / 2 - 1e-9)
      over <- setdiff(over, i)
      if (length(over)) {
        stop(sprintf(
          "`positions` puts %s on top of %s; move them apart.", nodes$id[i],
          paste(nodes$id[over], collapse = ", ")
        ), call. = FALSE)
      }
    }
  }
  spec$nodes <- nodes
  spec$meta$layout <- layout
  spec
}

# ==== stage 4: display ====

#' Half the extent of each node in a given direction
#'
#' @param nodes The node table.
#' @param at Row indices.
#' @param ux,uy Direction (unit vector components, recycled).
#' @return Numeric vector of distances from center to boundary.
#' @author Ken Kelley
#' @noRd
sieve_diagram_reach <- function(nodes, at, ux, uy) {
  w <- nodes$w[at] / 2
  h <- nodes$h[at] / 2
  round_node <- nodes$kind[at] != "observed"
  ifelse(round_node,
    (w * h) / sqrt((h * ux)^2 + (w * uy)^2 + 1e-12),
    pmin(w / pmax(abs(ux), 1e-12), h / pmax(abs(uy), 1e-12))
  )
}

#' Route the curved edges and the self-loops
#'
#' Sets, for each covariance, the signed height of its arc (`curve`), and
#' for each variance, intercept, or threshold, the direction in which it
#' sits on its node (`angle`, in degrees counterclockwise from the positive
#' x axis). An arc bows toward the free side of its two nodes, rises enough
#' to clear what lies between them, and longer arcs clear shorter ones.
#' A variance loop takes the free side of its node unless a straight edge
#' leaves the node there; the loop of a residual term also turns away from
#' a covariance arc that leaves the term near it.
#' Routing looks at every relationship of the model that could be drawn,
#' so hiding one class of edges does not move the others.
#'
#' The curve convention, for every renderer: the arc is the quadratic
#' Bezier curve from the center of `from` to the center of `to` whose
#' control point is the midpoint of the two centers plus `2 * curve` times
#' the unit normal, the normal being the direction from `from` to `to`
#' turned a quarter turn counterclockwise. Its apex is therefore `curve`
#' from the chord. The curve is drawn between the two node boundaries.
#'
#' @param nodes The node table (positions and current visibility).
#' @param edges The edge table (current endpoints).
#' @param live Logical, one per edge: the relationships that could be drawn.
#' @return The edge table with `curve`, `angle`, and `label_pos` set.
#' @author Ken Kelley
#' @noRd
sieve_diagram_route <- function(nodes, edges, live) {
  g <- sieve_diagram_geometry()
  idx <- stats::setNames(seq_len(nrow(nodes)), nodes$id)
  edges$curve <- NA_real_
  edges$angle <- NA_real_
  edges$label_pos <- NA_real_
  straight <- edges$type %in% c("loading", "regression", "residual_path")
  edges$curve[straight] <- 0
  edges$label_pos[edges$type == "loading"] <- g$label_pos[["loading"]]
  edges$label_pos[edges$type == "regression"] <- g$label_pos[["regression"]]
  edges$label_pos[edges$type %in% c("residual_path", "covariance")] <- 0.5
  seen <- nodes$visible
  has_loop <- nodes$id %in% edges$from[edges$type == "variance" & live]
  center <- c(mean(nodes$x[nodes$kind != "residual"]), mean(nodes$y[nodes$kind != "residual"]))

  arcs <- which(edges$type == "covariance" & live)
  a <- idx[edges$from[arcs]]
  b <- idx[edges$to[arcs]]
  len <- sqrt((nodes$x[b] - nodes$x[a])^2 + (nodes$y[b] - nodes$y[a])^2)
  done <- list()
  for (j in order(len, seq_along(arcs))) {
    e <- arcs[j]
    pa <- c(nodes$x[a[j]], nodes$y[a[j]])
    pb <- c(nodes$x[b[j]], nodes$y[b[j]])
    chord <- len[j]
    if (chord < 1e-8) {
      edges$curve[e] <- g$arc_min
      next
    }
    u <- (pb - pa) / chord
    normal <- c(-u[2L], u[1L])
    lean <- sum((c(nodes$out_x[a[j]], nodes$out_y[a[j]]) +
      c(nodes$out_x[b[j]], nodes$out_y[b[j]])) * normal)
    away <- sum(((pa + pb) / 2 - center) * normal)
    sgn <- if (abs(lean) > 0.25) sign(lean) else if (abs(away) > 1e-6) sign(away) else 1
    bow <- normal * sgn
    height <- min(g$arc_base_max, max(g$arc_min, g$arc_ratio * chord))
    # An arc that ends on a rectangle has to clear the rectangle itself,
    # or all that shows of it is a stub between two neighboring boxes.
    own <- c(a[j], b[j])[nodes$kind[c(a[j], b[j])] == "observed"]
    if (length(own)) {
      height <- max(height, max(abs(bow[1L]) * nodes$w[own] / 2 +
        abs(bow[2L]) * nodes$h[own] / 2) + g$arc_min)
    }
    # Nodes the arc has to pass over.
    others <- setdiff(which(seen), c(a[j], b[j]))
    if (length(others)) {
      rx <- nodes$x[others] - pa[1L]
      ry <- nodes$y[others] - pa[2L]
      frac <- (rx * u[1L] + ry * u[2L]) / chord
      off <- rx * bow[1L] + ry * bow[2L]
      ext <- abs(bow[1L]) * nodes$w[others] / 2 + abs(bow[2L]) * nodes$h[others] / 2
      faces <- nodes$out_x[others] * bow[1L] + nodes$out_y[others] * bow[2L] > 0.5
      value_room <- if (abs(bow[1L]) > abs(bow[2L])) g$value_width else g$label_room
      room <- ext + 0.15 + ifelse(has_loop[others] & faces, g$loop_length + 1.5 * value_room, 0)
      inside <- frac > 0.08 & frac < 0.92 & off > -(ext + 0.10) & off - ext < height
      if (any(inside)) {
        f <- pmin(0.8, pmax(0.2, frac[inside]))
        height <- max(height, (off[inside] + room[inside]) / (4 * f * (1 - f)))
      }
    }
    # Arcs already routed on the same side: clear the ones this arc spans.
    # Arcs that share a node are kept apart where they meet it by the
    # renderer, which attaches the higher arc nearer the free side.
    for (prev in done) {
      if (sum(prev$bow * bow) < 0.9) {
        next
      }
      f0 <- sum((prev$pa - pa) * u) / chord
      f1 <- sum((prev$pb - pa) * u) / chord
      o0 <- abs(sum((prev$pa - pa) * bow))
      o1 <- abs(sum((prev$pb - pa) * bow))
      if (min(f0, f1) > -0.02 && max(f0, f1) < 1.02 && max(o0, o1) < 0.35) {
        f <- pmin(0.8, pmax(0.2, (f0 + f1) / 2))
        height <- max(height, (prev$height + g$arc_step) / (4 * f * (1 - f)))
      }
    }
    edges$curve[e] <- sgn * height
    done[[length(done) + 1L]] <- list(
      pa = pa, pb = pb, bow = bow, height = height,
      chord = chord, ends = c(a[j], b[j])
    )
  }

  # Directions taken at each node: by the straight edges, which a loop must
  # stay clear of, and by the arcs.
  hard <- vector("list", nrow(nodes))
  soft <- vector("list", nrow(nodes))
  prefer <- ifelse(abs(nodes$out_x) + abs(nodes$out_y) > 1e-8,
    (atan2(nodes$out_y, nodes$out_x) * 180 / pi) %% 360, 90
  )
  away <- vector("list", nrow(nodes))
  turned <- function(a, b) ((a - b + 180) %% 360) - 180
  for (e in which(live & edges$type %in% c(
    "loading", "regression", "residual_path",
    "covariance"
  ))) {
    i <- idx[[edges$from[e]]]
    k <- idx[[edges$to[e]]]
    if (i == k || !seen[i] || !seen[k]) {
      next
    }
    pi_ <- c(nodes$x[i], nodes$y[i])
    pk <- c(nodes$x[k], nodes$y[k])
    aim <- (pi_ + pk) / 2
    arc <- !is.na(edges$curve[e]) && edges$curve[e] != 0
    if (arc) {
      d <- pk - pi_
      aim <- aim + 2 * edges$curve[e] * c(-d[2L], d[1L]) / sqrt(sum(d^2))
    }
    at_i <- atan2(aim[2L] - pi_[2L], aim[1L] - pi_[1L]) * 180 / pi
    at_k <- atan2(aim[2L] - pk[2L], aim[1L] - pk[1L]) * 180 / pi
    if (!arc) {
      hard[[i]] <- c(hard[[i]], at_i)
      hard[[k]] <- c(hard[[k]], at_k)
      next
    }
    soft[[i]] <- c(soft[[i]], at_i)
    soft[[k]] <- c(soft[[k]], at_k)
    # The loop of a residual term turns aside for an arc that leaves near
    # it. Two terms joined by an arc turn together, each away from the arc,
    # so the pair stays symmetric.
    ends <- c(i, k)
    off <- c(turned(at_i, prefer[i]), turned(at_k, prefer[k]))
    res <- nodes$kind[ends] == "residual"
    if (any(res & abs(off) < g$loop_clearance + 10)) {
      for (m in which(res)) {
        away[[ends[m]]] <- c(away[[ends[m]]], if (off[m] >= 0) -1 else 1)
      }
    }
  }
  # The preferred direction if it is clear of the edges, otherwise the
  # nearest direction that is, otherwise the clearest one there is. A loop
  # is clear of an edge that leaves the node beyond the foot of the loop.
  pick <- function(taken, prefer, i = NULL) {
    choices <- (prefer + c(0, 45, -45, 90, -90, 135, -135, 180)) %% 360
    if (!length(taken)) {
      return(choices[1L])
    }
    gap <- vapply(choices, function(a) min(abs(((taken - a + 180) %% 360) - 180)), numeric(1))
    need <- rep(g$loop_clearance, length(choices))
    if (!is.null(i)) {
      need <- sieve_diagram_loop_foot(nodes, rep(i, length(choices)), choices) + g$loop_margin
    }
    ok <- which(gap >= need)
    if (length(ok)) choices[ok[1L]] else choices[which.max(gap - need)]
  }
  # A variance loop sits on the free side of its node. Only a straight edge
  # moves it: an arc that meets the node is attached beside the loop by the
  # renderer. The exception is the residual term above, whose loop turns
  # away from an arc when all its arcs leave on one side.
  taken <- hard
  for (e in which(edges$type == "variance")) {
    i <- idx[[edges$from[e]]]
    if (!is.na(edges$angle[e])) {
      next
    }
    want <- prefer[i]
    side <- unique(away[[i]])
    if (length(side) == 1L) {
      want <- (want + 45 * side) %% 360
    }
    angle <- pick(hard[[i]], want, i)
    edges$angle[edges$type == "variance" & edges$from == edges$from[e]] <- angle
    taken[[i]] <- c(taken[[i]], angle)
  }
  for (kind in c("intercept", "threshold")) {
    for (e in which(edges$type == kind)) {
      i <- idx[[edges$from[e]]]
      if (!is.na(edges$angle[e])) {
        next
      }
      angle <- pick(c(taken[[i]], soft[[i]]), prefer[i])
      edges$angle[edges$type == kind & edges$from == edges$from[e]] <- angle
      taken[[i]] <- c(taken[[i]], angle)
    }
  }
  edges
}

#' The width of a variance loop
#'
#' The loop on a latent variable is about half as wide as the variable, as
#' in conventional path diagrams; the loop on a residual term or an observed
#' variable has one fixed width. Nodes that share a size therefore share a
#' loop size.
#'
#' @param nodes The node table.
#' @param at Row indices.
#' @param angle The direction of each loop, in degrees.
#' @return Numeric vector of full widths, in layout units.
#' @author Ken Kelley
#' @noRd
sieve_diagram_loop_width <- function(nodes, at, angle) {
  g <- sieve_diagram_geometry()
  rad <- angle * pi / 180
  across <- sqrt((sin(rad) * nodes$w[at])^2 + (cos(rad) * nodes$h[at])^2)
  ifelse(nodes$kind[at] == "latent",
    pmin(g$loop_width_max, pmax(g$loop_width, g$loop_ratio * across)),
    ifelse(nodes$kind[at] == "residual", g$residual_loop_width, g$loop_width)
  )
}

#' How far a variance loop reaches beyond its node
#'
#' A latent variable's loop reaches `loop_length` beyond the node; a
#' residual term's loop reaches less far (`residual_loop_length`), and is
#' round: that length plus the part sunk into the term
#' (`residual_loop_sink`) equals its width. Observed variables without a
#' residual term keep the latent length.
#'
#' @param nodes The node table.
#' @param at Row indices into `nodes`.
#' @return A numeric vector of lengths in inches, one per `at`.
#' @author Ken Kelley
#' @noRd
sieve_diagram_loop_length <- function(nodes, at) {
  g <- sieve_diagram_geometry()
  ifelse(nodes$kind[at] == "residual", g$residual_loop_length, g$loop_length)
}

#' The path of a variance loop
#'
#' An ellipse that starts a little inside the node and reaches the loop
#' length beyond it; what lies inside the node is trimmed away.
#'
#' @param nodes The node table.
#' @param i A row index into `nodes`.
#' @param angle The direction of the loop, in degrees.
#' @return A two column matrix, or `NULL` when nothing of the loop lies
#'   outside the node.
#' @author Ken Kelley
#' @noRd
sieve_diagram_loop_path <- function(nodes, i, angle) {
  rad <- angle * pi / 180
  dir <- c(cos(rad), sin(rad))
  length <- sieve_diagram_loop_length(nodes, i)
  along <- (length + sieve_diagram_loop_sink(nodes, i)) / 2
  across <- sieve_diagram_loop_width(nodes, i, angle) / 2
  pa <- c(nodes$x[i], nodes$y[i])
  center <- pa + dir * (sieve_diagram_reach(nodes, i, dir[1L], dir[2L]) + length - along)
  turn <- pi + seq(0, 2 * pi, length.out = 181)
  node <- nodes[i, , drop = FALSE]
  sieve_diagram_trim(
    cbind(
      center[1L] + along * cos(turn) * dir[1L] - across * sin(turn) * dir[2L],
      center[2L] + along * cos(turn) * dir[2L] + across * sin(turn) * dir[1L]
    ), node, node
  )
}

#' How far round a variance loop reaches on its node
#'
#' The angle, seen from the center of the node, between the direction of
#' the loop and the edge of what the loop covers: the footprint other ends
#' that meet the node (the arcs of covariances, the edges that leave it)
#' are kept clear of. For a latent or observed variable it is estimated as
#' `atan2(width / 2, reach)`. For a residual term it is measured on the
#' loop as drawn (`sieve_diagram_loop_path()`): the round loop sunk into the
#' term has its feet farther round than that estimate (about 42 degrees
#' against 38), and an estimate that low would let an arc's arrowhead meet
#' the loop's. The larger of the two is used.
#'
#' @param nodes The node table.
#' @param at Row indices into `nodes`.
#' @param angle The direction of each loop, in degrees.
#' @return A numeric vector of angles in degrees, one per `at`.
#' @author Ken Kelley
#' @noRd
sieve_diagram_loop_foot <- function(nodes, at, angle) {
  at <- rep_len(at, length(angle))
  vapply(seq_along(at), function(j) {
    i <- at[j]
    rad <- angle[j] * pi / 180
    dir <- c(cos(rad), sin(rad))
    estimate <- atan2(
      sieve_diagram_loop_width(nodes, i, angle[j]) / 2,
      sieve_diagram_reach(nodes, i, dir[1L], dir[2L])
    ) * 180 / pi
    if (!identical(nodes$kind[i], "residual")) {
      return(estimate)
    }
    path <- sieve_diagram_loop_path(nodes, i, angle[j])
    if (is.null(path)) {
      return(estimate)
    }
    dx <- path[, 1L] - nodes$x[i]
    dy <- path[, 2L] - nodes$y[i]
    seen <- abs(atan2(dir[1L] * dy - dir[2L] * dx, dir[1L] * dx + dir[2L] * dy)) * 180 / pi
    max(estimate, max(seen))
  }, numeric(1))
}

#' How far a variance loop sinks into its node before it is trimmed
#'
#' @inheritParams sieve_diagram_loop_length
#' @return A numeric vector in inches, one per `at`.
#' @author Ken Kelley
#' @noRd
sieve_diagram_loop_sink <- function(nodes, at) {
  g <- sieve_diagram_geometry()
  ifelse(nodes$kind[at] == "residual", g$residual_loop_sink, g$loop_sink)
}

#' The canvas a diagram needs
#'
#' The bounding box of what is drawn (the visible nodes, self-loops, and
#' arcs, each with room for its value), with a margin. Hiding a class of
#' relationships therefore gives its room back, while every node stays
#' where it was.
#'
#' @param nodes The node table.
#' @param edges The routed edge table.
#' @return A list with `width`, `height`, `units`, `xlim`, `ylim`,
#'   `base_pt`, `edge_pt`, and `note`.
#' @author Ken Kelley
#' @noRd
sieve_diagram_extent <- function(nodes, edges) {
  g <- sieve_diagram_geometry()
  idx <- stats::setNames(seq_len(nrow(nodes)), nodes$id)
  shown <- nodes[nodes$visible, , drop = FALSE]
  xs <- c(shown$x - shown$w / 2, shown$x + shown$w / 2)
  ys <- c(shown$y - shown$h / 2, shown$y + shown$h / 2)
  # The room a value takes is the width of its text as written (a long
  # unstandardized value, a constraint key), never less than the width of
  # a short value.
  value_w <- pmax(
    g$value_width,
    sieve_diagram_text_width(sieve_diagram_edge_text(edges)$text, g$edge_pt) + 0.08
  )
  loops <- which(edges$type == "variance" & edges$visible & !is.na(edges$angle))
  if (length(loops)) {
    i <- idx[edges$from[loops]]
    rad <- edges$angle[loops] * pi / 180
    wide <- sieve_diagram_loop_width(nodes, i, edges$angle[loops])
    far <- sieve_diagram_reach(nodes, i, cos(rad), sin(rad)) + sieve_diagram_loop_length(nodes, i) + g$label_room
    px <- nodes$x[i] + far * cos(rad)
    py <- nodes$y[i] + far * sin(rad)
    half_x <- pmax(value_w[loops], abs(sin(rad)) * wide) / 2
    half_y <- pmax(g$loop_width, abs(cos(rad)) * wide) / 2
    xs <- c(xs, px - half_x, px + half_x)
    ys <- c(ys, py - half_y, py + half_y)
  }
  arcs <- which(edges$type == "covariance" & edges$visible & !is.na(edges$curve))
  if (length(arcs)) {
    a <- idx[edges$from[arcs]]
    b <- idx[edges$to[arcs]]
    dx <- nodes$x[b] - nodes$x[a]
    dy <- nodes$y[b] - nodes$y[a]
    len <- pmax(sqrt(dx^2 + dy^2), 1e-8)
    rise <- edges$curve[arcs] + sign(edges$curve[arcs]) * g$label_room
    px <- (nodes$x[a] + nodes$x[b]) / 2 - dy / len * rise
    py <- (nodes$y[a] + nodes$y[b]) / 2 + dx / len * rise
    xs <- c(xs, px - value_w[arcs] / 2, px + value_w[arcs] / 2)
    ys <- c(ys, py - g$label_room / 2, py + g$label_room / 2)
  }
  xlim <- range(xs) + c(-1, 1) * g$margin
  ylim <- range(ys) + c(-1, 1) * g$margin
  width <- diff(xlim)
  height <- diff(ylim)
  note <- if (max(width, height) > 17) {
    paste0(
      "At the recommended size this figure is more than 17 inches on its longer ",
      "side. Short labels (label_mode = \"short\"), a narrower wrap, or the other ",
      "layout make it more compact."
    )
  } else {
    NULL
  }
  list(
    width = width, height = height, units = "in", xlim = xlim, ylim = ylim,
    base_pt = g$base_pt, edge_pt = g$edge_pt, note = note
  )
}

#' Format the values shown on the edges
#'
#' A free estimate is written with `digits` decimals, sign kept. A
#' parameter that was fixed is written as the exact constant it was fixed
#' to (`1`, `0`, `0.5`) when the display is in the metric it was fixed in,
#' so a fixed 1 and an estimated 1.00 differ in the label as well as in the
#' `fixed` flag. A value that is not known is written `NA`, with one
#' exception: in a standardized display, a parameter whose standardized
#' value lavaan cannot form (the loadings and the variance of a latent
#' variable with a negative variance estimate have no standard deviation
#' to divide by) is written as its unstandardized estimate, with its sign,
#' followed by `unstd.`, so the edge still shows the estimate the model
#' holds and says which metric it is in.
#'
#' @param edges The edge table with `value`, `est`, and the flags.
#' @param digits Decimal places.
#' @param standardized `TRUE` when the display is a standardized metric.
#' @return A character vector of labels (empty for residual paths).
#' @author Ken Kelley
#' @noRd
sieve_diagram_labels <- function(edges, digits, standardized = FALSE) {
  write <- function(value) {
    out <- ifelse(is.na(value), "NA", formatC(value, format = "f", digits = digits))
    exact <- edges$fixed & !edges$derived & !is.na(value) & !is.na(edges$est) &
      abs(value - edges$est) <= 1e-12
    out[exact] <- vapply(
      edges$est[exact], function(v) format(v, digits = 15, trim = TRUE),
      character(1)
    )
    out
  }
  out <- write(edges$value)
  if (standardized) {
    undefined <- is.na(edges$value) & is.finite(edges$est) &
      edges$type %in% c("loading", "regression", "variance", "covariance")
    if (any(undefined)) {
      out[undefined] <- paste(write(edges$est)[undefined], "unstd.")
    }
  }
  out[edges$type == "residual_path"] <- ""
  out
}

#' Apply the display options to a specification
#'
#' Sets the displayed metric, decides which edges and nodes are drawn and
#' why the others are not, attaches residual covariances to the residual
#' terms (or, when those are hidden, to the variables), routes the curved
#' edges, formats the labels, and writes the caption and the size. It reads
#' only the specification.
#'
#' @param spec A specification with positioned nodes.
#' @return The specification, ready to draw.
#' @author Ken Kelley
#' @noRd
sieve_diagram_apply <- function(spec) {
  o <- spec$meta$options
  nodes <- spec$nodes
  edges <- spec$edges
  pars <- spec$parameters
  has_par <- !is.na(edges$par)
  rows <- edges$par[has_par]
  metric <- switch(o$standardized,
    std.all = "std_all",
    std.lv = "std_lv",
    none = NA
  )
  for (col in c("std", "std_se", "std_ci_lower", "std_ci_upper")) {
    edges[[col]] <- NA_real_
  }
  if (!is.na(metric)) {
    edges$std[has_par] <- pars[[metric]][rows]
    edges$std_se[has_par] <- pars[[paste0(metric, "_se")]][rows]
    edges$std_ci_lower[has_par] <- pars[[paste0(metric, "_ci_lower")]][rows]
    edges$std_ci_upper[has_par] <- pars[[paste0(metric, "_ci_upper")]][rows]
    # A residual path stays 1 in a standardized display: the standardized
    # residual variance is what the loop on the residual term then shows.
    edges$std[edges$type == "residual_path"] <- 1
  }
  edges$value <- if (is.na(metric)) edges$est else edges$std

  is_res_node <- stats::setNames(nodes$kind == "residual", nodes$id)
  on_residual <- edges$type == "variance" & is_res_node[edges$from]
  zero <- has_par & edges$fixed & !edges$derived & !is.na(edges$est) & edges$est == 0
  reason <- rep(NA_character_, nrow(edges))
  type <- edges$type
  reason[type == "loading" & !is.na(edges$value) & abs(edges$value) < o$min_loading] <-
    "min_loading"
  if (!o$variances) {
    reason[type == "variance"] <- "variances"
  }
  if (!o$residuals) {
    # A covariance of residuals joins the residual terms. Drawn between the
    # variables instead, it would look like a correlation of the variables,
    # which it is not; so it leaves the figure with the terms, and the
    # caption counts it.
    reason[on_residual | type == "residual_path" |
      (type == "covariance" & !(edges$cov_class %in% "exogenous"))] <- "residuals"
  }
  if (!o$covariances) {
    reason[type == "covariance" & edges$cov_class %in% "exogenous"] <- "covariances"
  }
  if (!o$residual_covariances) {
    reason[type == "covariance" & !(edges$cov_class %in% "exogenous")] <- "residual_covariances"
  }
  if (!o$fixed_zero) {
    reason[zero] <- "fixed_zero"
    # A residual term whose variance is fixed at zero is no term at all.
    gone <- edges$from[zero & on_residual]
    reason[type == "residual_path" & edges$from %in% gone] <- "fixed_zero"
  }
  if (!o$intercepts) {
    reason[type %in% c("intercept", "threshold")] <- "intercepts"
  }
  edges$hidden_reason <- reason
  edges$visible <- is.na(reason)
  live <- is.na(reason) | reason != "fixed_zero"

  res <- which(nodes$kind == "residual")
  dropped <- nodes$id[res] %in% edges$from[type == "residual_path" & reason %in% "fixed_zero"]
  nodes$visible <- TRUE
  nodes$visible[res] <- o$residuals & !dropped
  # A covariance of residuals joins the residual terms. When a term is not
  # drawn (hidden, or dropped because its variance is fixed at zero) the
  # edge is attached to the variable, so that routing stays defined; it is
  # not drawn then unless the term was dropped.
  residual_of <- stats::setNames(
    nodes$id[res][nodes$visible[res]],
    nodes$parent[res][nodes$visible[res]]
  )
  cov <- which(type == "covariance")
  end_node <- function(v) ifelse(v %in% names(residual_of), residual_of[v], v)
  edges$from[cov] <- unname(end_node(edges$lhs[cov]))
  edges$to[cov] <- unname(end_node(edges$rhs[cov]))

  edges <- sieve_diagram_route(nodes, edges, live)
  edges$label_text <- sieve_diagram_labels(edges, o$digits, standardized = !is.na(metric))
  spec$nodes <- nodes
  spec$edges <- edges
  spec$size <- sieve_diagram_extent(nodes, edges)
  hidden <- edges[!edges$visible, c("id", "type", "hidden_reason"), drop = FALSE]
  names(hidden)[3L] <- "reason"
  rownames(hidden) <- NULL
  counts <- table(hidden$reason)
  spec$meta$hidden <- list(
    counts = stats::setNames(as.integer(counts), names(counts)),
    edges = hidden
  )
  # Parameters fixed at zero that the caption names: the relationships, and
  # the intercepts and thresholds when those were asked for (otherwise the
  # mean structure is left to the parameter table as a whole).
  spec$meta$fixed_zero <- edges$id[zero & (
    type %in% c("loading", "regression", "covariance", "variance") |
      (o$intercepts & type %in% c("intercept", "threshold")))]
  spec$meta$standardized <- o$standardized
  parts <- sieve_diagram_caption(spec)
  spec$meta$caption_parts <- parts
  spec$meta$caption <- paste(parts, collapse = " ")
  spec
}

#' Write the caption of a diagram
#'
#' One sentence block that records what a reader needs to interpret the
#' figure without the app: the model and the group shown, the scale of the
#' numbers, how the latent variables were identified, how fixed parameters
#' are shown, which constraints the model carries, what is in the model but
#' not drawn, what the coefficients of ordered variables refer to, any flag
#' on the solution, and the analysis the figure belongs to. Every sentence
#' about a convention is written only when the figure shows that
#' convention: nothing is said about marks, labels, or residual paths that
#' are not drawn.
#'
#' @param spec A specification after the display options were applied.
#' @return A named character vector of sentences.
#' @author Ken Kelley
#' @noRd
sieve_diagram_caption <- function(spec) {
  m <- spec$meta
  o <- m$options
  nodes <- spec$nodes
  edges <- spec$edges
  several <- function(x, most = 6L) {
    if (length(x) > most) {
      x <- c(x[seq_len(most)], sprintf("and %d more", length(x) - most))
    }
    paste(x, collapse = ", ")
  }
  count <- function(n, one, many = paste0(one, "s")) sprintf("%d %s", n, if (n == 1L) one else many)
  spaced <- function(id) {
    at <- match(id, edges$id)
    op <- edges$op[at]
    ifelse(is.na(op), id,
      ifelse(op == "~1", paste(edges$lhs[at], "~ 1"),
        paste(edges$lhs[at], op, edges$rhs[at])
      )
    )
  }
  n_lat <- sum(nodes$kind == "latent")
  n_obs <- sum(nodes$kind == "observed")
  kind <- switch(m$model_kind,
    cfa = "Confirmatory factor model",
    higher_order = "Higher-order confirmatory factor model",
    sem = "Structural equation model",
    path = "Path model",
    esem = "Exploratory structural equation model (rotated)",
    growth = "Latent growth model"
  )
  fiml <- isTRUE(m$lavaan_options$missing %in% c("ml", "ml.x"))
  parts <- c(model = sprintf(
    "%s: %s%s (%s%s, n = %d).", kind,
    if (n_lat) sprintf("%d latent and ", n_lat) else "",
    count(n_obs, "observed variable"), m$estimator, if (fiml) ", FIML" else "", m$nobs
  ))
  if (isTRUE(m$ngroups > 1L)) {
    parts <- c(parts, group = sprintf(
      "Group shown: %s (%d of %d).",
      m$group_label, m$group, m$ngroups
    ))
  }
  parts <- c(parts, scale = switch(o$standardized,
    std.all = "Values are fully standardized (std.all).",
    std.lv = paste0(
      "Values are standardized for the latent variables only (std.lv); ",
      "observed variables keep their own scales."
    ),
    none = "Values are unstandardized."
  ))
  if (n_lat) {
    endogenous_latent <- any(nodes$kind == "latent" & !nodes$exogenous)
    parts <- c(parts, identification = switch(m$identification,
      unit_variance = sprintf(
        "Each latent scale was set by fixing its variance%s to 1.",
        if (endogenous_latent) " (its disturbance variance, if endogenous)" else ""
      ),
      marker = "Each latent scale was set by fixing one loading to 1.",
      mixed = paste0(
        "Latent scales were set by a variance fixed at 1 for some latent ",
        "variables and a loading fixed at 1 for others."
      ),
      other = paste0(
        "Latent scales were not set by one fixed variance or one fixed ",
        "loading each; see the parameter table for how each latent scale ",
        "was set."
      )
    ))
  }
  # Conventions are explained only where the figure uses them.
  shown <- edges$visible
  par_shown <- shown & !is.na(edges$par)
  fixed_shown <- par_shown & edges$fixed
  fixed_note <- character(0)
  if (any(fixed_shown)) {
    fixed_note <- "Fixed parameters are marked"
    # In a standardized display a fixed parameter shows its standardized
    # value, which need not be the constant it was fixed to.
    restated <- fixed_shown & !is.na(edges$value) & !is.na(edges$est) &
      abs(edges$value - edges$est) > 1e-12
    if (any(restated)) {
      fixed_note <- paste0(
        fixed_note, "; a marked standardized value belongs to a ",
        "parameter that was fixed in estimation"
      )
    }
  }
  if (any(shown & edges$type == "residual_path")) {
    fixed_note <- if (length(fixed_note)) {
      paste0(fixed_note, "; residual paths are fixed at 1 and unlabeled")
    } else {
      "Residual paths are fixed at 1 and unlabeled"
    }
  }
  if (length(fixed_note)) {
    parts <- c(parts, fixed = paste0(fixed_note, "."))
  }
  tied <- unique(stats::na.omit(edges$constraint[shown]))
  within <- tied[tied != "="]
  if (length(within)) {
    parts <- c(parts, constraints = sprintf(
      "Parameters with the same constraint label are constrained equal (%s).",
      several(within)
    ))
  }
  if ("=" %in% tied) {
    parts <- c(parts, constraints_groups = "Values marked [=] are constrained equal across groups.")
  }
  other <- m$constraints
  if (is.data.frame(other) && !is.null(other$kind) && any(other$kind == "other")) {
    other <- other[other$kind == "other", , drop = FALSE]
    parts <- c(parts, constraints_other = sprintf(
      "Other constraints of the model: %s.",
      paste(paste(other$lhs, other$op, other$rhs), collapse = "; ")
    ))
  }
  bounds <- m$lavaan_options$bounds
  if (!is.null(bounds) && !identical(bounds, "none") && !isFALSE(bounds)) {
    parts <- c(parts, bounds = sprintf(
      "Parameters were estimated within bounds (lavaan bounds = \"%s\").",
      as.character(bounds)[1L]
    ))
  } else if (is.data.frame(m$bounded) && nrow(m$bounded)) {
    b <- m$bounded
    limit <- vapply(seq_len(nrow(b)), function(i) {
      paste(
        c(
          if (is.finite(b$lower[i])) sprintf("lower %s", format(b$lower[i], digits = 15)),
          if (is.finite(b$upper[i])) sprintf("upper %s", format(b$upper[i], digits = 15))
        ),
        collapse = ", "
      )
    }, character(1))
    parts <- c(parts, bounds = sprintf(
      "Bounded parameters: %s.",
      several(sprintf("%s (%s)", b$parameter, limit))
    ))
  }
  if (any(shown & edges$type == "covariance" & edges$cov_class %in% c("residual", "disturbance")) &&
    any(nodes$kind == "residual" & nodes$visible)) {
    parts <- c(parts, residual_covariances = paste0(
      "Double-headed arrows between residual terms are residual covariances, ",
      "not correlations of the variables."
    ))
  }
  why <- edges$hidden_reason
  hidden <- character(0)
  if (!o$residuals) {
    hidden <- c(hidden, "residual terms and their variances")
    n_rc <- sum(why %in% "residuals" & edges$type == "covariance")
    if (n_rc) {
      hidden <- c(hidden, sprintf("%s between them", count(n_rc, "residual covariance")))
    }
  }
  if (any(why %in% "variances")) {
    hidden <- c(hidden, count(sum(why %in% "variances"), "variance"))
  }
  if (any(why %in% "covariances")) {
    hidden <- c(hidden, sprintf(
      "%s between exogenous variables",
      count(sum(why %in% "covariances"), "covariance")
    ))
  }
  if (any(why %in% "residual_covariances")) {
    hidden <- c(hidden, count(sum(why %in% "residual_covariances"), "residual covariance"))
  }
  if (any(why %in% "min_loading")) {
    hidden <- c(hidden, sprintf(
      "%s with absolute value below %s (%s)",
      count(sum(why %in% "min_loading"), "loading"),
      format(o$min_loading), several(spaced(edges$id[why %in% "min_loading"]))
    ))
  }
  if (length(hidden)) {
    parts <- c(parts, hidden = sprintf(
      "In the model but not drawn: %s.",
      paste(hidden, collapse = "; ")
    ))
  }
  zero <- m$fixed_zero
  if (length(zero)) {
    parts <- c(parts, fixed_zero = sprintf(
      "Fixed at zero%s: %s.",
      if (o$fixed_zero) " and drawn as fixed" else " and not drawn", several(spaced(zero))
    ))
  }
  has_means <- any(edges$type %in% c("intercept", "threshold"))
  if (has_means && !o$intercepts) {
    parts <- c(parts, intercepts = "Intercepts and thresholds are in the parameter table, not the figure.")
  } else if (!has_means && o$intercepts) {
    parts <- c(parts, intercepts = "Intercepts were requested, but the model has no mean structure.")
  }
  if (length(m$ordered)) {
    parts <- c(parts, categorical = sieve_diagram_ordered_sentence(spec))
  }
  if (identical(spec$status$code, "improper")) {
    negative <- spec$status$details$negative_variances
    message <- if (is.data.frame(negative) && nrow(negative)) {
      sprintf(
        "The solution is improper: negative variance estimate for %s.",
        several(sieve_diagram_name_in_group(negative, m$group))
      )
    } else {
      spec$status$message
    }
    # A standardized value does not exist where lavaan cannot form it (a
    # negative variance has no standard deviation). Such an edge shows the
    # unstandardized estimate marked unstd.; saying that the estimates are
    # shown as they are would then be untrue, so the caption says which
    # values are undefined and what the edge shows instead.
    drawn <- shown & edges$type %in% c("loading", "regression", "variance", "covariance")
    undefined <- !identical(o$standardized, "none") & drawn & is.na(edges$value) &
      is.finite(edges$est)
    unknown <- drawn & is.na(edges$value) & !undefined
    parts <- c(parts, status = paste(
      message,
      if (any(undefined)) {
        sprintf(
          paste0(
            "This is not a proper solution. %d standardized value%s undefined for it; %s ",
            "edge%s the unstandardized estimate instead, with its sign, marked unstd., ",
            "and the parameter table has every estimate."
          ),
          sum(undefined), if (sum(undefined) == 1L) " is" else "s are",
          if (sum(undefined) == 1L) "that" else "those",
          if (sum(undefined) == 1L) " shows" else "s show"
        )
      } else if (any(unknown)) {
        sprintf(
          "This is not a proper solution. %d drawn value%s not known and %s shown as NA.",
          sum(unknown), if (sum(unknown) == 1L) " is" else "s are",
          if (sum(unknown) == 1L) "is" else "are"
        )
      } else {
        "The estimates are shown as they are; this is not a proper solution."
      }
    ))
  }
  if (isFALSE(spec$status$details$se_requested)) {
    parts <- c(parts, uncertainty = "Standard errors were not computed, so uncertainty is unknown.")
  } else {
    parts <- c(parts, uncertainty = sprintf(
      "Standard errors and %s%% confidence intervals are in the parameter table.",
      format(100 * m$conf_level)
    ))
  }
  if (!is.null(m$analysis_id)) {
    parts <- c(parts, analysis = sprintf("Analysis %s.", m$analysis_id))
  }
  if (length(m$omitted_aux)) {
    parts <- c(parts, auxiliary = sprintf(
      "Saturated auxiliaries (%s) were included in estimation and omitted from the diagram; their parameters remain in the parameter table.",
      paste(m$omitted_aux, collapse = ", ")))
  }
  parts
}

#' The caption sentence about ordered variables
#'
#' Written from the edges of the group shown, not from the parameterization
#' alone: under the delta parameterization the residual variance of an
#' ordered variable is implied by the model; under theta it is fixed at 1,
#' except in a group where the model frees it (as a multiple-group model
#' with equal thresholds and loadings does in every group but the first).
#'
#' @param spec A specification after the display options were applied.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_diagram_ordered_sentence <- function(spec) {
  m <- spec$meta
  nodes <- spec$nodes
  edges <- spec$edges
  several <- function(x, most = 6L) {
    if (length(x) > most) {
      x <- c(x[seq_len(most)], sprintf("and %d more", length(x) - most))
    }
    paste(x, collapse = ", ")
  }
  ordered <- m$ordered
  at <- match(edges$from, nodes$id)
  own <- edges$type == "variance" & nodes$kind[at] == "residual" & nodes$parent[at] %in% ordered
  variable <- nodes$parent[at][own]
  state <- ifelse(edges$derived[own], "implied",
    ifelse(edges$free[own] > 0L, "estimated", "fixed")
  )
  where <- if (isTRUE(m$ngroups > 1L)) " in this group" else ""
  phrase <- c(
    fixed = "fixed at 1", estimated = paste0("estimated", where),
    implied = "implied by the model, not estimated"
  )
  kinds <- unique(state)
  residuals <- if (!length(kinds)) {
    "their residual variances are in the parameter table"
  } else if (length(kinds) == 1L) {
    sprintf("their residual variances are %s", phrase[[kinds]])
  } else {
    paste0("their residual variances are ", paste(vapply(kinds, function(k) {
      sprintf("%s for %s", phrase[[k]], several(variable[state == k]))
    }, character(1)), collapse = ", "))
  }
  sprintf(
    "Ordered variables (%s): coefficients refer to latent response variables (%s parameterization); %s.",
    several(ordered), m$lavaan_options$parameterization, residuals
  )
}

# ==== validation ====

#' Check that a diagram specification is internally consistent
#'
#' The contract every renderer relies on: the tables have their columns,
#' identifiers are unique, every edge joins nodes that exist, a hidden edge
#' says why, a drawable specification has a position for every node, and a
#' failed one has no nodes or edges at all.
#'
#' @param spec A specification.
#' @param error `TRUE` stops on the first set of problems; `FALSE` returns
#'   them.
#' @return With `error = TRUE`, `TRUE` invisibly when the specification is
#'   valid. With `error = FALSE`, a character vector of problems (empty
#'   when valid).
#' @author Ken Kelley
#' @noRd
sieve_diagram_validate <- function(spec, error = TRUE) {
  problems <- character(0)
  note <- function(problems, ok, what) if (isTRUE(ok)) problems else c(problems, what)
  if (!inherits(spec, "sieve_diagram")) {
    problems <- "not an object of class sieve_diagram"
  } else {
    empty <- sieve_diagram_empty()
    for (part in c("status", "nodes", "edges", "parameters", "label_key", "meta", "size")) {
      problems <- note(problems, !is.null(spec[[part]]), sprintf("element `%s` is missing", part))
    }
    for (tab in c("nodes", "edges", "parameters")) {
      miss <- setdiff(names(empty[[tab]]), names(spec[[tab]]))
      problems <- note(
        problems, !length(miss),
        sprintf("`%s` lacks column(s) %s", tab, paste(miss, collapse = ", "))
      )
    }
    code <- spec$status$code
    problems <- note(
      problems, isTRUE(code %in% c("ok", "improper", "not_converged", "failed")),
      "status code is not one of ok, improper, not_converged, failed"
    )
    problems <- note(
      problems, is.character(spec$meta$caption) && length(spec$meta$caption) == 1L,
      "the caption is not a single string"
    )
  }
  if (!length(problems)) {
    nodes <- spec$nodes
    edges <- spec$edges
    if (!sieve_diagram_drawable(spec)) {
      problems <- note(
        problems, nrow(nodes) == 0L && nrow(edges) == 0L,
        "a specification that cannot be drawn carries nodes or edges"
      )
    } else {
      problems <- note(problems, nrow(nodes) > 0L, "there are no nodes")
      problems <- note(problems, !anyDuplicated(nodes$id), "node ids are not unique")
      problems <- note(problems, !anyDuplicated(edges$id), "edge ids are not unique")
      problems <- note(
        problems, all(nodes$kind %in% c("latent", "observed", "residual")),
        "a node has an unknown kind"
      )
      problems <- note(
        problems,
        all(edges$type %in% c(
          "loading", "regression", "residual_path",
          "covariance", "variance", "intercept",
          "threshold"
        )),
        "an edge has an unknown type"
      )
      problems <- note(
        problems, all(edges$from %in% nodes$id) && all(edges$to %in% nodes$id),
        "an edge joins a node that does not exist"
      )
      problems <- note(
        problems, all(is.finite(nodes$x)) && all(is.finite(nodes$y)) &&
          all(is.finite(nodes$w)) && all(is.finite(nodes$h)),
        "a node has no position or size"
      )
      problems <- note(
        problems, !anyNA(nodes$label) && all(nzchar(nodes$label)),
        "a node has no label"
      )
      problems <- note(
        problems, identical(is.na(edges$hidden_reason), edges$visible),
        "an edge is hidden without a reason, or shown with one"
      )
      cov <- edges$type == "covariance"
      problems <- note(
        problems,
        all(edges$cov_class[cov] %in% c("exogenous", "disturbance", "residual")) &&
          all(is.na(edges$cov_class[!cov])),
        "covariance classes are missing or misplaced"
      )
      problems <- note(
        problems, all(edges$from[cov] != edges$to[cov]),
        "a covariance joins a node to itself"
      )
      loop <- edges$type %in% c("variance", "intercept", "threshold")
      problems <- note(
        problems, all(edges$from[loop] == edges$to[loop]),
        "a variance, intercept, or threshold is not attached to one node"
      )
      rp <- edges$type == "residual_path"
      problems <- note(
        problems, all(edges$fixed[rp]) && all(edges$est[rp] == 1) &&
          all(is.na(edges$par[rp])),
        "a residual path is not a structural path fixed at 1"
      )
      problems <- note(
        problems, !anyNA(edges$par[!rp]) &&
          all(edges$par[!rp] %in% seq_len(nrow(spec$parameters))),
        "an edge does not point to its parameter"
      )
      shown <- edges$visible
      ends_shown <- nodes$visible[match(edges$from, nodes$id)] &
        nodes$visible[match(edges$to, nodes$id)]
      problems <- note(
        problems, all(ends_shown[shown]),
        "a drawn edge joins a node that is not drawn"
      )
      problems <- note(
        problems, all(is.finite(edges$curve[shown & cov])) &&
          all(is.finite(edges$angle[shown & loop])),
        "a drawn arc or loop has no route"
      )
      problems <- note(
        problems, is.finite(spec$size$width) && is.finite(spec$size$height) &&
          spec$size$width > 0 && spec$size$height > 0,
        "the recommended size is not positive"
      )
    }
  }
  if (!error) {
    return(problems)
  }
  if (length(problems)) {
    stop(sprintf("Invalid diagram specification: %s.", paste(problems, collapse = "; ")),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

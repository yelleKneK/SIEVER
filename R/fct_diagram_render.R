# fct_diagram_render.R
# The renderer of the path diagram specification, written on base R graphics
# (graphics and grDevices only). It draws the classroom notation: an ellipse
# for a latent variable, a rectangle for an observed variable, a small
# circle for each residual term with a single-headed arrow into its
# variable, a double-headed self-loop for every variance, and one curved
# double-headed arrow for every covariance of the model.
#
# The renderer reads the specification and nothing else. It never sees the
# fitted model, so it cannot refit it or disagree with it; it uses no random
# numbers; it restores the graphics settings it changes; and the export
# closes only the device it opened.
#
# A drawing is made in three steps. sieve_diagram_paths() computes the path
# of every visible edge and stops, before any graphics setting is touched,
# if an edge cannot be drawn. sieve_diagram_scene() adds what needs the text
# metrics of the open device: the size of each label and where it goes.
# sieve_diagram_render() then draws, and returns a record of what it drew,
# which is what the tests compare with the specification.

#' Draw a Path Diagram
#'
#' Draws a diagram specification on the current graphics device in the
#' classroom notation: latent variables are ellipses, observed variables are
#' rectangles, and each residual term (a unique term or a disturbance) is a
#' small circle with a single-headed arrow into its variable. A variance is
#' a double-headed loop on its node, and a double-headed curved arrow is
#' drawn only for a covariance that is a parameter of the model.
#' `plot()` on a specification calls this function.
#'
#' @details
#' **What an arrow means.** A single-headed arrow from a latent variable to
#' one of its indicators is a loading; a single-headed arrow from a
#' predictor to an outcome is a regression (drawn heavier); the arrow from
#' a residual term into its variable is fixed at 1 and carries no label.
#' The direction of an arrow shows the model that was specified. It does
#' not by itself establish a causal direction. A curved double-headed
#' arrow between two exogenous variables is their covariance (their
#' correlation in a standardized display). A curved double-headed arrow
#' between two residual terms is a covariance of residuals, not the
#' correlation of the two variables.
#'
#' **How status is shown without color.** A solid line is an estimated
#' parameter. A dashed line is a parameter that was fixed in estimation; in
#' the metric in which it was fixed its value is printed as the constant
#' (`1`, not `1.00`). A dotted line with its value in parentheses is implied
#' by the model rather than estimated (the residual variance of an ordered
#' indicator under the delta parameterization, or a moment of a fixed
#' covariate). A label in square brackets after a value names an equality
#' constraint. A value that is out of range (a negative variance, or a
#' standardized value beyond 1) keeps its sign, is printed in bold, is
#' followed by `(!)`, and is boxed. The caption drawn under the figure
#' states the conventions that the figure uses.
#'
#' **Residual terms** are labeled with the Greek letter of their role:
#' delta for the unique term of an indicator of an exogenous latent
#' variable, epsilon for the unique term of an indicator of an endogenous
#' latent variable, and zeta for a disturbance. Terms that share a letter
#' are numbered in model order.
#'
#' **Where the values are written.** A loading is written beside its line,
#' at one height for the indicators of a factor; a variance beyond its
#' loop; a covariance above its arc. A value that would sit on another line
#' or node, or between two lines that cross, is moved along its line or
#' written on the line itself on a white ground, so that it cannot be read
#' as belonging to another line.
#'
#' **Intercepts and thresholds**, when the specification shows them, are
#' written in small type beyond the residual term of their variable: nu
#' for the intercept or mean of an observed variable, alpha for that of a
#' latent variable, and tau for a threshold. They need room beyond the
#' recommended size of the specification, and the figure grows to hold
#' them.
#'
#' **Size.** The specification is laid out in inches. The diagram is scaled
#' as a whole to fit the device, so on a device of the recommended size
#' (`spec$size`) node labels are 10 point and edge labels 9 point. A label
#' that would not fit its node with the fonts of the device is made
#' slightly smaller, never cut.
#'
#' Drawing never changes the specification, uses no random numbers, and
#' restores every graphics parameter it sets, the coordinate system
#' included. A specification that cannot be drawn (no fitted model, a model
#' that did not converge or is not identified) is an error of class
#' `sieve_diagram_error`, raised before anything is drawn; the condition
#' carries the `reason` (the status code) and the `status` of the
#' specification.
#'
#' @param spec A specification from [sieve_diagram_spec()].
#' @param ... Not used. It must be empty, and every argument after it must
#'   be named in full, so that a misspelled argument is an error instead of
#'   being silently ignored.
#' @param palette The construct colors, used only for the outline of a
#'   latent variable and of its indicators: the name of a SIEVER palette
#'   (`"nd"`, `"okabe_ito"`, or `"viridis"`), a character vector of colors,
#'   or `NULL` for none. Every node is white inside and the text is dark.
#' @param monochrome `TRUE` draws in black and white only. Every
#'   distinction in the figure is carried by shape, line type, line weight,
#'   or type style, so the monochrome figure loses nothing.
#' @param cex Multiplier for all text.
#' @param show_caption `TRUE` writes the caption of the specification, with
#'   the notation the figure uses, under the diagram. When the observed
#'   variables are shown by short identifiers, the caption ends with the
#'   key to them.
#' @param family The font family, as understood by the graphics device
#'   (`"sans"`, `"serif"`, or a family the device knows).
#' @return `spec`, invisibly.
#' @seealso [sieve_export_diagram()] to write a file.
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' model <- "visual =~ t1_visual_perception + t2_cubes + t4_lozenges
#'           textual =~ t6_paragraph_comprehension + t7_sentence + t9_word_meaning"
#' fit <- lavaan::cfa(model, data = hs, std.lv = TRUE)
#' diagram <- sieve_diagram_spec(fit)
#' sieve_draw_diagram(diagram)
#' # The same model without the factor covariance drawn, in black and white.
#' plot(sieve_diagram_update(diagram, covariances = FALSE), monochrome = TRUE)
#' @md
#' @export
sieve_draw_diagram <- function(spec, ..., palette = "nd", monochrome = FALSE, cex = 1,
                               show_caption = TRUE, family = "sans") {
  if (...length()) {
    stop("sieve_draw_diagram() takes no further arguments; check the spelling of each one.",
      call. = FALSE
    )
  }
  sieve_diagram_check(spec)
  style <- sieve_diagram_style(spec,
    palette = palette, monochrome = monochrome, cex = cex,
    show_caption = show_caption, family = family
  )
  sieve_diagram_render(spec, style)
  invisible(spec)
}

#' Plot a Path Diagram Specification
#'
#' The `plot()` method of a diagram specification draws it with
#' [sieve_draw_diagram()].
#'
#' @param x A specification from [sieve_diagram_spec()].
#' @param ... Passed to [sieve_draw_diagram()]: `palette`, `monochrome`,
#'   `cex`, `show_caption`, and `family`, each named in full.
#' @return `x`, invisibly.
#' @seealso [sieve_draw_diagram()], [sieve_export_diagram()]
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' fit <- lavaan::cfa("visual =~ t1_visual_perception + t2_cubes + t4_lozenges",
#'   data = hs, std.lv = TRUE
#' )
#' plot(sieve_diagram_spec(fit))
#' @md
#' @export
plot.sieve_diagram <- function(x, ...) {
  sieve_draw_diagram(x, ...)
}

#' Write a Path Diagram to a File
#'
#' Opens a graphics device of the requested format, draws the specification
#' with [sieve_draw_diagram()], and closes the device again. The figure is
#' written to a temporary file first and moved into place only when the
#' drawing succeeded, so a failed export never leaves a partial file and
#' never destroys an existing one. Only the device that this function
#' opened is closed, also after an error, and the device that was current
#' before stays current.
#'
#' @details
#' PDF and SVG are vector formats. PDF is written with [grDevices::pdf()]
#' in the standard Helvetica, Times, and Symbol fonts, which every viewer
#' has and which are therefore not embedded. SVG is written with the
#' 'svglite' package when it is installed (the text stays text and can be
#' edited) and otherwise with `grDevices::svg()`, which needs a build of R
#' with working cairo support; without either, the export fails with a
#' clear message and writes nothing. PNG is a raster format written with
#' [grDevices::png()] at `dpi` dots per inch. The background is white in
#' every format.
#'
#' By default the figure has the size recommended by the specification
#' (`spec$size`), at which node labels are 10 point, plus the room the
#' caption needs; a figure with a caption is at least 5 inches wide, so
#' the caption stays readable. When only one of `width` and `height` is
#' given, the other follows from the proportions of the figure.
#'
#' A failure is an error of class `sieve_diagram_error` whose `reason` says
#' where it arose: the status code of a specification that cannot be drawn
#' (`"failed"`, `"not_converged"`), `"invalid_spec"`, `"geometry"` (two
#' nodes overlap, so an edge has no path), `"file"` (the target cannot be
#' written), `"device"` (the graphics device could not be opened or wrote
#' nothing), or `"render"` (an error while drawing; the original condition
#' is attached as `parent`). In every case no file is written.
#'
#' A value that could not be placed clear of every line, node, and other
#' value is drawn at the least bad place, and the export warns with a
#' condition of class `sieve_diagram_warning` (reason `"placement"`) that
#' names the labels; the `cost` column of `drawn$edges` in the returned
#' list records the placement cost of each label, zero when it is clear.
#' The file is written.
#'
#' @param spec A specification from [sieve_diagram_spec()].
#' @param file Path of the file to write.
#' @param format `"pdf"`, `"svg"`, or `"png"`. When it is not given, the
#'   extension of `file` decides, and a file without one of these
#'   extensions is written as PDF.
#' @param width,height Size of the figure in inches. See Details for the
#'   default.
#' @param dpi Resolution of a PNG file, in dots per inch. Ignored for the
#'   vector formats.
#' @param ... Passed to [sieve_draw_diagram()]: `palette`, `monochrome`,
#'   `cex`, `show_caption`, and `family`, each named in full.
#' @return Invisibly, a list with the `file` written, its `format`,
#'   `width`, and `height` in inches, `dpi` (missing for a vector format),
#'   the `device` used, and the provenance of the figure: `analysis_id`,
#'   `status`, `model`, `model_kind`, `model_source`, `estimator`,
#'   `identification`, `nobs`, `standardized`, `group`, `versions` (SIEVER,
#'   lavaan, R), `options` (the display options of the specification),
#'   `code` (the call that regenerates the specification), `caption` (as
#'   drawn), and `drawn` (the nodes and edges that were drawn).
#' @seealso [sieve_draw_diagram()]
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' model <- "visual =~ t1_visual_perception + t2_cubes + t4_lozenges
#'           textual =~ t6_paragraph_comprehension + t7_sentence + t9_word_meaning"
#' fit <- lavaan::cfa(model, data = hs, std.lv = TRUE)
#' diagram <- sieve_diagram_spec(fit, analysis_id = "example")
#' file <- tempfile(fileext = ".pdf")
#' written <- sieve_export_diagram(diagram, file)
#' written[c("format", "width", "height", "analysis_id")]
#' unlink(file)
#' @md
#' @export
sieve_export_diagram <- function(spec, file, format = c("pdf", "svg", "png"), width = NULL,
                                 height = NULL, dpi = 300, ...) {
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    sieve_diagram_abort("`file` must be the path of the file to write.", "file", spec)
  }
  format <- if (missing(format)) {
    ext <- tolower(tools::file_ext(file))
    if (ext %in% c("pdf", "svg", "png")) ext else "pdf"
  } else {
    match.arg(format)
  }
  sieve_diagram_check(spec)
  style <- sieve_diagram_style(spec, ...)
  size <- sieve_diagram_export_size(spec, style, width, height)
  if (!is.numeric(dpi) || length(dpi) != 1L || !is.finite(dpi) || dpi <= 0) {
    sieve_diagram_abort("`dpi` must be a single positive number.", "device", spec)
  }
  if (identical(format, "png") && max(size) * dpi > 30000) {
    sieve_diagram_abort(sprintf(
      "A PNG of %.1f by %.1f inches at %s dpi is too large to write; lower `dpi` or the size.",
      size[["width"]], size[["height"]], format(dpi)
    ), "device", spec)
  }
  # The target is checked before any device is opened.
  if (!dir.exists(dirname(file))) {
    sieve_diagram_abort(sprintf("The folder %s does not exist.", dirname(file)), "file", spec)
  }
  if (file.access(dirname(file), 2L) != 0L) {
    sieve_diagram_abort(
      sprintf("The folder %s cannot be written to.", dirname(file)), "file",
      spec
    )
  }
  # The paths are computed first, so a diagram that cannot be drawn is
  # refused before a device exists.
  sieve_diagram_paths(spec)
  tmp <- tempfile("sieve_diagram_", fileext = paste0(".", format))
  before <- grDevices::dev.cur()
  opened <- NULL
  on.exit(
    {
      # Only the device opened here is closed, and the device that was
      # current before becomes current again.
      if (!is.null(opened) && opened %in% grDevices::dev.list()) {
        grDevices::dev.off(opened)
      }
      if (before > 1L && before %in% grDevices::dev.list() && grDevices::dev.cur() != before) {
        grDevices::dev.set(before)
      }
      unlink(tmp)
    },
    add = TRUE
  )
  device <- sieve_diagram_open(format, tmp, size, dpi, spec)
  opened <- device$number
  drawn <- tryCatch(
    sieve_diagram_render(spec, style),
    error = function(e) {
      # A refusal of the renderer keeps its own reason. Anything else went
      # wrong while drawing, and its cause is kept.
      if (inherits(e, "sieve_diagram_error")) {
        stop(e)
      }
      sieve_diagram_abort(sprintf(
        "The diagram could not be drawn as %s: %s", toupper(format),
        conditionMessage(e)
      ), "render", spec, parent = e)
    }
  )
  grDevices::dev.off(opened)
  opened <- NULL
  if (!file.exists(tmp) || !isTRUE(file.size(tmp) > 0)) {
    sieve_diagram_abort(sprintf("The %s device wrote nothing.", device$name), "device", spec)
  }
  # A value that could not be placed clear of every line, node, and other
  # value is drawn at the least bad place and reported, never passed
  # silently.
  crowded <- drawn$edges$id[drawn$edges$cost > 0]
  if (length(crowded)) {
    sieve_diagram_warn(sprintf(
      paste0(
        "%d value label%s could not be placed clear of every line, node, and label (%s). ",
        "Short labels, a wider wrap, or the other layout may give them room; the drawn ",
        "record lists the placement cost of each label."
      ),
      length(crowded), if (length(crowded) == 1L) "" else "s",
      paste(crowded, collapse = ", ")
    ), "placement")
  }
  copied <- sieve_capture(file.copy(tmp, file, overwrite = TRUE))
  if (!isTRUE(copied$value)) {
    sieve_diagram_abort(sprintf(
      "The diagram could not be written to %s%s", file,
      if (length(c(copied$warnings, copied$error))) {
        paste0(": ", paste(c(copied$warnings, copied$error), collapse = " "))
      } else {
        "."
      }
    ), "file", spec)
  }
  m <- spec$meta
  invisible(list(
    file = normalizePath(file, winslash = "/", mustWork = FALSE),
    format = format, width = size[["width"]], height = size[["height"]],
    dpi = if (identical(format, "png")) dpi else NA_real_, device = device$name,
    analysis_id = m$analysis_id, status = spec$status$code, model = m$model,
    model_kind = m$model_kind, model_source = m$model_source, estimator = m$estimator,
    identification = m$identification, nobs = m$nobs,
    standardized = m$standardized, group = m$group,
    versions = m$versions, options = m$options, code = sieve_diagram_code(spec),
    caption = if (style$show_caption) sieve_diagram_figure_caption(spec) else NULL,
    drawn = drawn
  ))
}

#' Open the graphics device of an export
#'
#' Opens the device and confirms that exactly one new device exists. A
#' device function can fail with only a warning (a build of R without
#' working cairo support does), and drawing would then go to whatever
#' device R opens by default; that is caught here. The warnings of a
#' failed attempt become part of the error; those of a successful one are
#' passed on.
#'
#' @param format `"pdf"`, `"svg"`, or `"png"`.
#' @param file The file the device writes.
#' @param size Named numeric vector with `width` and `height`, in inches.
#' @param dpi Resolution of a PNG.
#' @param spec The specification (for the error condition).
#' @return A list with the device `number` and the `name` of the device
#'   function used.
#' @author Ken Kelley
#' @noRd
sieve_diagram_open <- function(format, file, size, dpi, spec) {
  tryCatch(sieve_open_device(format, file, size[["width"]], size[["height"]], dpi,
    title = "SIEVER path diagram"),
    error = function(e) sieve_diagram_abort(conditionMessage(e), "device", spec))
}

# ==== conditions and checks ====

#' Signal a diagram error
#'
#' Every refusal and failure of the renderer and the export is a condition
#' of class `sieve_diagram_error`, so a caller can tell it from any other
#' error and show the reason.
#'
#' @param message The message.
#' @param reason A short code: the status code of a specification that
#'   cannot be drawn (`"failed"`, `"not_converged"`), or `"invalid_spec"`,
#'   `"geometry"`, `"device"`, `"render"`, or `"file"`.
#' @param spec The specification, when there is one; its status is carried.
#' @param parent The condition that caused this one, if any.
#' @return Does not return.
#' @author Ken Kelley
#' @noRd
sieve_diagram_abort <- function(message, reason, spec = NULL, parent = NULL) {
  stop(structure(
    class = c("sieve_diagram_error", "error", "condition"),
    list(
      message = message, call = NULL, reason = reason,
      status = if (inherits(spec, "sieve_diagram")) spec$status else NULL,
      parent = parent
    )
  ))
}

#' Signal a diagram warning
#'
#' A condition of class `sieve_diagram_warning` with a `reason`, so a
#' caller can tell it from any other warning: `"placement"` when a value
#' could not be placed clear of everything else, `"small_type"` when the
#' type at the exported size is smaller than the target.
#'
#' @param message The message.
#' @param reason A short code.
#' @return `NULL`, invisibly, after the warning was signaled.
#' @author Ken Kelley
#' @noRd
sieve_diagram_warn <- function(message, reason) {
  warning(structure(
    class = c("sieve_diagram_warning", "warning", "condition"),
    list(message = message, call = NULL, reason = reason)
  ))
  invisible(NULL)
}

#' The text written on each edge
#'
#' The value of the specification with the marks of the renderer: the
#' value of an implied parameter in parentheses, `(!)` after a value that
#' is out of range, and the equality constraint in square brackets. A
#' constraint label longer than six characters would crowd the figure, so
#' it is written as a short key (`c1`, `c2`, ...) and the key is explained
#' in the caption. The same function sizes the canvas and writes the
#' labels, so the room reserved for a value is the room it takes.
#'
#' @param edges Rows of the edge table of a specification.
#' @return A list with `text` (one string per row) and `key` (a data frame
#'   with `tag` and `constraint` for the constraint labels that were
#'   shortened, in order of first appearance).
#' @author Ken Kelley
#' @noRd
sieve_diagram_edge_text <- function(edges) {
  text <- edges$label_text
  text <- ifelse(edges$derived %in% TRUE & nzchar(text), paste0("(", text, ")"), text)
  text <- ifelse(edges$improper %in% TRUE & nzchar(text), paste(text, "(!)"), text)
  tag <- edges$constraint
  long <- !is.na(tag) & nchar(tag) > 6L
  key <- unique(tag[long])
  shown <- tag
  shown[long] <- paste0("c", match(tag[long], key))
  text <- ifelse(is.na(tag), text, paste0(text, " [", shown, "]"))
  list(
    text = text,
    key = data.frame(
      tag = if (length(key)) paste0("c", seq_along(key)) else character(0),
      constraint = key, stringsAsFactors = FALSE
    )
  )
}

#' Refuse a specification that cannot be drawn
#'
#' Called before any graphics function, so a refusal opens no device and
#' draws nothing.
#'
#' @param spec A candidate specification.
#' @return `TRUE`, invisibly, when the specification is valid and drawable.
#' @author Ken Kelley
#' @noRd
sieve_diagram_check <- function(spec) {
  if (!inherits(spec, "sieve_diagram")) {
    sieve_diagram_abort(
      "`spec` must be a diagram specification from sieve_diagram_spec().",
      "invalid_spec"
    )
  }
  if (!sieve_diagram_drawable(spec)) {
    sieve_diagram_abort(
      sprintf(
        "No path diagram can be drawn (status %s): %s",
        spec$status$code, spec$status$message
      ),
      as.character(spec$status$code), spec
    )
  }
  problems <- sieve_diagram_validate(spec, error = FALSE)
  if (length(problems)) {
    sieve_diagram_abort(sprintf(
      "The diagram specification is not valid: %s.",
      paste(problems, collapse = "; ")
    ), "invalid_spec", spec)
  }
  invisible(TRUE)
}

# ==== style ====

#' Drawing constants of the base graphics renderer
#'
#' Lengths are in layout units (inches at the recommended size), line
#' widths in the units of `lwd`, and type sizes in points. The layout
#' constants shared with the specification are in
#' `sieve_diagram_geometry()`.
#'
#' @return A named list.
#' @author Ken Kelley
#' @noRd
sieve_diagram_look <- function() {
  list(
    caption_pt = 9, caption_lead = 1.32, caption_min = 5, caption_max = 10,
    caption_share = 0.6, caption_most = 0.4, caption_foot = 0.16, note_lead = 1.3,
    note_gap = 0.10,
    label_gap = 0.035, label_pad = 0.022, on_line_pad = 0.045, note_pt = 8.5,
    greek_pt = 12.5, pair_gap = 0.16, arc_apart = 0.15, arc_clear = c(9, 13),
    # The least distance, along a node's outline, between the foot of its
    # variance loop and the end of an arc: half the width of a covariance
    # arrowhead (0.036) and of a variance arrowhead (0.030), and a little
    # air, so the two heads never touch. It applies to a residual term with
    # at most two arc ends, whose small circle makes `arc_clear[1]` degrees
    # too short a distance.
    loop_arc_gap = 0.08,
    label_clear = 0.03,
    ink = "#1a1a1a", line = "#2b2b2b", muted = "#444444", flag = "#b00020",
    lwd = c(
      loading = 1.15, regression = 2.1, residual_path = 0.95, covariance = 1.15,
      variance = 0.95, latent = 1.6, observed = 1.15, residual = 0.95
    ),
    head = list(
      loading = c(0.115, 0.075), regression = c(0.16, 0.11),
      residual_path = c(0.09, 0.062), covariance = c(0.11, 0.072),
      variance = c(0.085, 0.06)
    ),
    lty = c(free = "solid", fixed = "42", derived = "13")
  )
}

#' Resolve the style of one drawing
#'
#' Validates the style arguments shared by [sieve_draw_diagram()] and
#' [sieve_export_diagram()] and gives every node its outline and fill.
#' Color is an accent only: the outline of a latent variable and of its
#' indicators. Every node is white inside, and an observed variable that
#' belongs to no latent variable keeps the neutral line color. A light
#' palette color is darkened for the outline so that it stays visible on
#' white.
#'
#' @param spec A drawable specification.
#' @param palette,monochrome,cex,show_caption,family See
#'   [sieve_draw_diagram()].
#' @return A list with the validated arguments, `border` and `fill` (one
#'   color per row of `spec$nodes`), and the line colors.
#' @author Ken Kelley
#' @noRd
sieve_diagram_style <- function(spec, palette = "nd", monochrome = FALSE, cex = 1,
                                show_caption = TRUE, family = "sans") {
  flag <- function(x, name) {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) {
      stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
    }
    x
  }
  monochrome <- flag(monochrome, "monochrome")
  show_caption <- flag(show_caption, "show_caption")
  if (!is.numeric(cex) || length(cex) != 1L || !is.finite(cex) || cex <= 0) {
    stop("`cex` must be a single positive number.", call. = FALSE)
  }
  if (!is.character(family) || length(family) != 1L || is.na(family)) {
    stop("`family` must be a single font family name.", call. = FALSE)
  }
  k <- sieve_diagram_look()
  nodes <- spec$nodes
  # A construct is a latent variable with the variables it heads. An
  # observed variable that belongs to no construct gets no accent.
  heads <- nodes$id[nodes$kind == "latent"]
  colors <- NULL
  if (!monochrome && !is.null(palette)) {
    if (!is.character(palette) || !length(palette) || anyNA(palette)) {
      stop("`palette` must be a palette name, a vector of colors, or NULL.", call. = FALSE)
    }
    colors <- if (length(palette) == 1L && palette %in% names(sieve_palettes())) {
      sieve_palette_colors(palette, length(heads))
    } else {
      ok <- tryCatch(is.matrix(grDevices::col2rgb(palette)), error = function(e) FALSE)
      if (!ok) {
        stop(
          sprintf(
            "`palette` must be one of %s, a vector of colors, or NULL.",
            paste(sprintf("\"%s\"", names(sieve_palettes())), collapse = ", ")
          ),
          call. = FALSE
        )
      }
      rep_len(palette, length(heads))
    }
  }
  line <- if (monochrome) "#000000" else k$line
  border <- rep(line, nrow(nodes))
  fill <- rep("#ffffff", nrow(nodes))
  if (!is.null(colors) && length(heads)) {
    rgb <- grDevices::col2rgb(colors) / 255
    # A light color is darkened for the outline, so it stays visible on
    # white. Every node is white inside, as in the classroom figures.
    lum <- 0.2126 * rgb[1L, ] + 0.7152 * rgb[2L, ] + 0.0722 * rgb[3L, ]
    dark <- pmin(1, 0.42 / pmax(lum, 1e-6))
    outline <- grDevices::rgb(rgb[1L, ] * dark, rgb[2L, ] * dark, rgb[3L, ] * dark)
    at <- match(nodes$block, heads)
    colored <- nodes$kind != "residual" & !is.na(at)
    border[colored] <- outline[at[colored]]
  }
  border[nodes$kind == "residual"] <- if (monochrome) "#000000" else k$muted
  list(
    palette = palette, monochrome = monochrome, cex = cex, show_caption = show_caption,
    family = family, border = border, fill = fill, line = line,
    ink = if (monochrome) "#000000" else k$ink,
    flag = if (monochrome) "#000000" else k$flag
  )
}

# ==== caption ====

#' The notation a figure uses, as a sentence for its caption
#'
#' Names only the conventions that are not common to every path diagram
#' and that the figure actually shows, so the caption of a simple diagram
#' stays short: the letters of the residual terms, the line types that mark
#' fixed and implied values, and the marks on a value.
#'
#' @param spec A drawable specification.
#' @return A length one character string, or `NULL` when there is nothing
#'   to explain.
#' @author Ken Kelley
#' @noRd
sieve_diagram_notation <- function(spec) {
  edges <- spec$edges[spec$edges$visible, , drop = FALSE]
  par <- edges[!is.na(edges$par), , drop = FALSE]
  greek <- intersect(
    c("delta", "epsilon", "zeta"),
    sieve_diagram_greek(spec$nodes)[spec$nodes$visible]
  )
  noted <- spec$nodes$kind[match(edges$from[edges$type == "intercept"], spec$nodes$id)]
  role <- c(
    delta = "delta, a unique term", epsilon = "epsilon, a unique term",
    zeta = "zeta, a disturbance"
  )
  items <- c(
    if (length(greek)) {
      sprintf("small circle, residual term (%s)", paste(role[greek], collapse = "; "))
    },
    if (any(edges$type == "regression") && any(edges$type == "loading")) {
      "heavy arrow, regression; light arrow, loading"
    },
    if (any(par$fixed)) "dashed line, parameter fixed in estimation",
    if (any(par$derived)) {
      "dotted line with the value in parentheses, implied by the model and not estimated"
    },
    if (any(!is.na(par$constraint))) "label in square brackets, equality constraint",
    if (any(par$improper)) "boxed value in bold type marked (!), out of range",
    if (any(edges$type == "threshold")) {
      "tau, threshold of the indicator, written beyond its unique term"
    },
    if (any(noted == "observed")) "nu, intercept or mean of an observed variable",
    if (any(noted == "latent")) "alpha, intercept or mean of a latent variable"
  )
  if (length(items)) paste0("Notation: ", paste(items, collapse = "; "), ".") else NULL
}

#' The caption drawn under a figure
#'
#' The caption of the specification with the notation of this renderer
#' placed after the sentence about fixed parameters. When the observed
#' variables are shown by short identifiers, the key to them is added at
#' the end, so the figure can be read on its own.
#'
#' @param spec A drawable specification.
#' @param key `FALSE` leaves the key to the short identifiers out (the app
#'   shows it as a table instead).
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_diagram_figure_caption <- function(spec, key = TRUE) {
  parts <- spec$meta$caption_parts
  at <- match("fixed", names(parts), nomatch = length(parts))
  with_key <- key
  key <- spec$label_key[spec$label_key$short != spec$label_key$label, , drop = FALSE]
  # Constraint labels that the figure shows by a short key.
  tags <- sieve_diagram_edge_text(spec$edges[spec$edges$visible, , drop = FALSE])$key
  tag_key <- if (nrow(tags)) {
    sprintf(
      "Constraint labels in the figure: %s.",
      paste(sprintf("%s = %s", tags$tag, tags$constraint), collapse = "; ")
    )
  }
  rest <- parts[-seq_len(at)]
  after <- match("constraints", names(rest), nomatch = 0L)
  rest <- if (after > 0L) {
    c(rest[seq_len(after)], tag_key, rest[-seq_len(after)])
  } else {
    c(tag_key, rest)
  }
  paste(c(
    parts[seq_len(at)], sieve_diagram_notation(spec), rest,
    if (with_key && identical(spec$meta$options$label_mode, "short") && nrow(key)) {
      sprintf("Key: %s.", paste(sprintf("%s = %s", key$short, key$label), collapse = "; "))
    }
  ), collapse = " ")
}

#' The width a caption is set to
#'
#' A caption as wide as a wide figure has lines too long to read, and one
#' held to a fixed width leaves half of the band under a wide figure empty.
#' The measure therefore grows with the figure, more slowly than it.
#'
#' @param width The width of the figure, in layout units.
#' @return The measure, in layout units.
#' @author Ken Kelley
#' @noRd
sieve_diagram_caption_measure <- function(width) {
  k <- sieve_diagram_look()
  min(width - 2 * sieve_diagram_geometry()$margin, max(k$caption_max, k$caption_share * width))
}

#' Break a caption into lines
#'
#' Greedy line filling with the device-free text widths of the
#' specification, so the line breaks, and with them the height a figure
#' needs, are known before a device is opened and are the same in every
#' format.
#'
#' @param text The caption.
#' @param width The width of the figure, in layout units.
#' @param cex The text multiplier of the caption.
#' @return A character vector of lines.
#' @author Ken Kelley
#' @noRd
sieve_diagram_caption_lines <- function(text, width, cex = 1) {
  k <- sieve_diagram_look()
  avail <- sieve_diagram_caption_measure(width)
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  wide <- sieve_diagram_text_width(words, k$caption_pt * cex)
  space <- sieve_diagram_text_width(" ", k$caption_pt * cex)
  lines <- character(0)
  line <- NULL
  used <- 0
  for (i in seq_along(words)) {
    if (!is.null(line) && used + space + wide[i] > avail) {
      lines <- c(lines, line)
      line <- NULL
    }
    used <- if (is.null(line)) wide[i] else used + space + wide[i]
    line <- if (is.null(line)) words[i] else paste(line, words[i])
  }
  c(lines, line)
}

#' Height of a caption, in layout units
#'
#' @param n Number of lines.
#' @param cex The text multiplier of the caption.
#' @return A single number, zero when there is no caption.
#' @author Ken Kelley
#' @noRd
sieve_diagram_caption_height <- function(n, cex = 1) {
  k <- sieve_diagram_look()
  if (!n) 0 else n * k$caption_pt * cex * k$caption_lead / 72 + k$caption_foot
}

#' Set a caption under a figure of a given size
#'
#' The caption gives way before the diagram does: when it would take more
#' than two fifths of the figure, its type is reduced, in steps, to no less
#' than three fifths of its size. It is never cut.
#'
#' @param text The caption.
#' @param width,height Width and height of the diagram, in layout units.
#' @param cex The text multiplier of the drawing.
#' @return A list with the `lines`, their total `height`, and the `scale`
#'   applied to the caption type.
#' @author Ken Kelley
#' @noRd
sieve_diagram_caption_block <- function(text, width, height, cex = 1) {
  k <- sieve_diagram_look()
  for (scale in c(1, 0.9, 0.8, 0.7, 0.6)) {
    lines <- sieve_diagram_caption_lines(text, width, cex * scale)
    tall <- sieve_diagram_caption_height(length(lines), cex * scale)
    if (tall <= k$caption_most * (height + tall)) {
      break
    }
  }
  list(lines = lines, height = tall, scale = scale)
}

#' Text as the open device needs it
#'
#' The PDF and PostScript devices of R draw the ASCII hyphen as a minus
#' sign, which is right for a negative number and wrong inside a word
#' ("double-headed"). On those devices a hyphen between two letters is
#' sent as the hyphen of the device encoding instead. Other devices, and
#' everything that is not a character string, are left alone.
#'
#' @param x A character vector (or anything else, returned unchanged).
#' @return `x`, with word hyphens replaced on a PDF or PostScript device.
#' @author Ken Kelley
#' @noRd
sieve_diagram_device_text <- function(x) {
  if (!is.character(x) || !names(grDevices::dev.cur()) %in% c("pdf", "postscript")) {
    return(x)
  }
  gsub("(?<=[[:alpha:]])-(?=[[:alpha:]])", "\u00ad", x, perl = TRUE)
}

#' The canvas of a drawing
#'
#' The canvas of the specification covers the nodes, the loops, and the
#' arcs. Intercepts and thresholds, when they are shown, are written beyond
#' the residual terms and need more room; the canvas is extended to hold
#' them. Without them it is the canvas of the specification, unchanged.
#'
#' @param spec A drawable specification.
#' @param cex The text multiplier of the drawing.
#' @return A list with `xlim`, `ylim`, `width`, and `height`, in layout
#'   units.
#' @author Ken Kelley
#' @noRd
sieve_diagram_canvas <- function(spec, cex = 1) {
  xlim <- spec$size$xlim
  ylim <- spec$size$ylim
  notes <- sieve_diagram_notes(spec, cex)
  if (nrow(notes)) {
    pad <- sieve_diagram_geometry()$margin
    xlim <- range(xlim, notes$x - notes$w / 2 - pad, notes$x + notes$w / 2 + pad)
    ylim <- range(ylim, notes$y - notes$h / 2 - pad, notes$y + notes$h / 2 + pad)
  }
  list(xlim = xlim, ylim = ylim, width = diff(xlim), height = diff(ylim))
}

#' The size of an exported figure
#'
#' The natural size is the canvas, made at least wide enough for a
#' readable caption, plus the height of the caption.
#'
#' @param spec A drawable specification.
#' @param style The resolved style.
#' @param width,height Requested size in inches, or `NULL`.
#' @return A named numeric vector with `width` and `height`.
#' @author Ken Kelley
#' @noRd
sieve_diagram_export_size <- function(spec, style, width, height) {
  given <- function(x, name) {
    if (is.null(x)) {
      return(NULL)
    }
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
      sieve_diagram_abort(
        sprintf("`%s` must be a single positive number of inches.", name),
        "device", spec
      )
    }
    as.numeric(x)
  }
  width <- given(width, "width")
  height <- given(height, "height")
  canvas <- sieve_diagram_canvas(spec, style$cex)
  natural_w <- canvas$width
  natural_h <- canvas$height
  if (style$show_caption) {
    natural_w <- max(natural_w, sieve_diagram_look()$caption_min)
    natural_h <- natural_h + sieve_diagram_caption_block(
      sieve_diagram_figure_caption(spec), natural_w, natural_h, style$cex
    )$height
  }
  if (is.null(width) && is.null(height)) {
    width <- natural_w
    height <- natural_h
  } else if (is.null(height)) {
    height <- width * natural_h / natural_w
  } else if (is.null(width)) {
    width <- height * natural_w / natural_h
  }
  c(width = width, height = height)
}

#' Fit the diagram and its caption into the plot region
#'
#' @param canvas The canvas from `sieve_diagram_canvas()`.
#' @param pin Width and height of the plot region, in inches.
#' @param caption The caption text, or `NULL`.
#' @param cex The text multiplier of the drawing.
#' @return A list with `scale` (inches per layout unit), the user
#'   coordinate limits `xlim` and `ylim`, the caption `lines`, and
#'   `caption_scale` (the factor applied to the caption type).
#' @author Ken Kelley
#' @noRd
sieve_diagram_viewport <- function(canvas, pin, caption, cex) {
  fit <- function(extra) min(pin[1L] / canvas$width, pin[2L] / (canvas$height + extra))
  scale <- fit(0)
  block <- list(lines = character(0), height = 0, scale = 1)
  if (!is.null(caption)) {
    # The caption is set in type that scales with the figure, so its height
    # and the scale depend on each other; a few passes settle them.
    for (pass in 1:6) {
      block <- sieve_diagram_caption_block(caption, pin[1L] / scale, canvas$height, cex)
      settled <- abs(fit(block$height) - scale) < 1e-9
      scale <- fit(block$height)
      if (settled) {
        break
      }
    }
  }
  user_w <- pin[1L] / scale
  user_h <- pin[2L] / scale
  top <- canvas$ylim[2L] + (user_h - canvas$height - block$height) / 2
  list(
    scale = scale, xlim = mean(canvas$xlim) + c(-1, 1) * user_w / 2,
    ylim = c(top - user_h, top), lines = block$lines, caption_scale = block$scale
  )
}

# ==== geometry ====

#' Is a point inside a node?
#'
#' @param node One row of the node table.
#' @param x,y Coordinates (vectors).
#' @return A logical vector.
#' @author Ken Kelley
#' @noRd
sieve_diagram_inside <- function(node, x, y) {
  dx <- (x - node$x) / (node$w / 2)
  dy <- (y - node$y) / (node$h / 2)
  if (identical(node$kind, "observed")) abs(dx) < 1 & abs(dy) < 1 else dx^2 + dy^2 < 1
}

#' Trim a path to the part between two node boundaries
#'
#' The path runs from inside node `a` to inside node `b` (for a self-loop
#' both are the same node). The part inside `a` at the start and the part
#' inside `b` at the end are removed, and the new ends lie on the
#' boundaries.
#'
#' @param path A two column matrix of points.
#' @param a,b One row of the node table each.
#' @return The trimmed matrix, or `NULL` when nothing lies between the
#'   two nodes (they overlap).
#' @author Ken Kelley
#' @noRd
sieve_diagram_trim <- function(path, a, b) {
  n <- nrow(path)
  first <- match(TRUE, !sieve_diagram_inside(a, path[, 1L], path[, 2L]))
  last <- n + 1L - match(TRUE, rev(!sieve_diagram_inside(b, path[, 1L], path[, 2L])))
  if (is.na(first) || is.na(last) || first > last + 1L) {
    return(NULL)
  }
  cross <- function(node, p_in, p_out) {
    for (step in 1:40) {
      mid <- (p_in + p_out) / 2
      if (sieve_diagram_inside(node, mid[1L], mid[2L])) p_in <- mid else p_out <- mid
    }
    p_out
  }
  start <- if (first > 1L) cross(a, path[first - 1L, ], path[first, ]) else path[1L, ]
  end <- if (last < n) cross(b, path[last + 1L, ], path[last, ]) else path[n, ]
  out <- rbind(start, if (first <= last) path[first:last, , drop = FALSE], end,
    deparse.level = 0
  )
  # On a straight path both ends come from one segment: they must not have
  # passed each other.
  if (n == 2L && sum((end - start) * (path[2L, ] - path[1L, ])) <= 0) {
    return(NULL)
  }
  if (sieve_diagram_length(out) < 0.02) NULL else out
}

#' Length of a path
#'
#' @param path A two column matrix of points.
#' @return A single number.
#' @author Ken Kelley
#' @noRd
sieve_diagram_length <- function(path) {
  sum(sqrt(rowSums((path[-1L, , drop = FALSE] - path[-nrow(path), , drop = FALSE])^2)))
}

#' The part of a path between two distances along it
#'
#' @param path A two column matrix of points.
#' @param from,to Distances from the start of the path.
#' @return A two column matrix that starts and ends exactly at the two
#'   distances.
#' @author Ken Kelley
#' @noRd
sieve_diagram_cut <- function(path, from, to) {
  step <- sqrt(rowSums((path[-1L, , drop = FALSE] - path[-nrow(path), , drop = FALSE])^2))
  along <- c(0, cumsum(step))
  point <- function(d) {
    i <- min(max(findInterval(d, along, rightmost.closed = TRUE), 1L), length(step))
    f <- if (step[i] > 0) (d - along[i]) / step[i] else 0
    path[i, ] + f * (path[i + 1L, ] - path[i, ])
  }
  rbind(point(from), path[along > from & along < to, , drop = FALSE], point(to),
    deparse.level = 0
  )
}

#' A point on a path, with the direction of the path there
#'
#' @param path A two column matrix of points.
#' @param d A distance from the start of the path, clamped to its length.
#' @return A list with `at` (the point) and `u` (the unit tangent).
#' @author Ken Kelley
#' @noRd
sieve_diagram_along <- function(path, d) {
  step <- sqrt(rowSums((path[-1L, , drop = FALSE] - path[-nrow(path), , drop = FALSE])^2))
  along <- c(0, cumsum(step))
  d <- min(max(d, 0), along[length(along)])
  i <- min(max(findInterval(d, along, rightmost.closed = TRUE), 1L), length(step))
  f <- if (step[i] > 0) (d - along[i]) / step[i] else 0
  u <- (path[i + 1L, ] - path[i, ]) / max(step[i], 1e-12)
  list(at = path[i, ] + f * (path[i + 1L, ] - path[i, ]), u = u)
}

#' The quadratic Bezier curve the specification describes for an arc
#'
#' @param p0,p2 The two ends.
#' @param control The control point.
#' @param n Number of points.
#' @return A two column matrix.
#' @author Ken Kelley
#' @noRd
sieve_diagram_bezier <- function(p0, control, p2, n = 129L) {
  t <- seq(0, 1, length.out = n)
  cbind(
    (1 - t)^2 * p0[1L] + 2 * t * (1 - t) * control[1L] + t^2 * p2[1L],
    (1 - t)^2 * p0[2L] + 2 * t * (1 - t) * control[2L] + t^2 * p2[2L]
  )
}

#' A cubic Bezier curve
#'
#' @param p0,p3 The two ends.
#' @param c1,c2 The two control points.
#' @param n Number of points.
#' @return A two column matrix.
#' @author Ken Kelley
#' @noRd
sieve_diagram_cubic <- function(p0, c1, c2, p3, n = 129L) {
  t <- seq(0, 1, length.out = n)
  w0 <- (1 - t)^3
  w1 <- 3 * (1 - t)^2 * t
  w2 <- 3 * (1 - t) * t^2
  w3 <- t^3
  cbind(
    w0 * p0[1L] + w1 * c1[1L] + w2 * c2[1L] + w3 * p3[1L],
    w0 * p0[2L] + w1 * c1[2L] + w2 * c2[2L] + w3 * p3[2L]
  )
}

#' A curve between two points that leaves and arrives at a set angle
#'
#' A symmetric cubic Bezier curve from `pa` to `pb` whose apex is `height`
#' from the chord on the side of `normal` and which leaves each end at
#' `angle` degrees from the chord, so that where it meets a node its
#' arrowhead is clearly apart from a straight line arriving there. When
#' the chord is too short for that angle the quadratic curve of the same
#' height is used instead.
#'
#' @param pa,pb The two ends.
#' @param normal Unit normal of the chord, pointing to the side of the bow.
#' @param height The height of the apex above the chord.
#' @param angle The departure angle, in degrees.
#' @return A two column matrix.
#' @author Ken Kelley
#' @noRd
sieve_diagram_bend_curve <- function(pa, pb, normal, height, angle = 35) {
  chord <- sqrt(sum((pb - pa)^2))
  u <- (pb - pa) / max(chord, 1e-12)
  rad <- angle * pi / 180
  # For a symmetric cubic the apex is three quarters of the control
  # distance times the sine of the departure angle.
  d <- height / (0.75 * sin(rad))
  if (d * cos(rad) < chord / 2) {
    c1 <- pa + d * (cos(rad) * u + sin(rad) * normal)
    c2 <- pb + d * (-cos(rad) * u + sin(rad) * normal)
    sieve_diagram_cubic(pa, c1, c2, pb)
  } else {
    sieve_diagram_bezier(pa, (pa + pb) / 2 + 2 * height * normal, pb)
  }
}

#' The Greek letter of each residual term
#'
#' Delta for the unique term of an indicator whose factor is exogenous,
#' epsilon for the unique term of an indicator whose factor is endogenous,
#' zeta for a disturbance.
#'
#' @param nodes The node table.
#' @return A character vector as long as `nodes`: `"delta"`, `"epsilon"`,
#'   or `"zeta"` for a residual node, `NA` otherwise.
#' @author Ken Kelley
#' @noRd
sieve_diagram_greek <- function(nodes) {
  out <- rep(NA_character_, nrow(nodes))
  res <- nodes$kind == "residual"
  variable <- match(nodes$parent[res], nodes$id)
  factor_of <- match(nodes$parent[variable], nodes$id)
  out[res] <- ifelse(nodes$role[res] == "disturbance", "zeta",
    ifelse(!is.na(factor_of) & !nodes$exogenous[factor_of], "epsilon", "delta")
  )
  out
}


# ==== the scene ====

#' Where the intercepts and thresholds are written
#'
#' Intercepts, means, and thresholds are annotations in small type, shown
#' only on request. They are written beyond the residual term of their
#' variable (beyond its loop and the value of the loop), or beyond the
#' variable's own loop when it has no residual term, so they never sit
#' between neighboring nodes. Along a row of indicators each value has its
#' own line; beside a column of indicators the values share one line.
#'
#' @param spec A drawable specification.
#' @param cex The text multiplier of the drawing.
#' @param measure A function of the plotmath label and its plain text that
#'   returns the width of the label in layout units. `NULL` uses the
#'   device-free estimate of the specification, which is what the canvas
#'   is sized with.
#' @return A data frame with one row per visible intercept or threshold:
#'   `id` (the edge), `with` (the edge whose line it shares, or `NA`), the
#'   center `x`, `y` and the size `w`, `h` of its line, the plain `label`,
#'   and `display` (what is drawn).
#' @author Ken Kelley
#' @noRd
sieve_diagram_notes <- function(spec, cex = 1, measure = NULL) {
  g <- sieve_diagram_geometry()
  k <- sieve_diagram_look()
  nodes <- spec$nodes
  edges <- spec$edges
  idx <- stats::setNames(seq_len(nrow(nodes)), nodes$id)
  shown <- which(edges$visible & edges$type %in% c("intercept", "threshold"))
  blank <- rep(NA_real_, length(shown))
  out <- data.frame(
    id = edges$id[shown], with = rep(NA_character_, length(shown)),
    x = blank, y = blank, w = blank, h = blank,
    label = rep("", length(shown)), stringsAsFactors = FALSE
  )
  out$display <- vector("list", length(shown))
  if (!length(shown)) {
    return(out)
  }
  if (is.null(measure)) {
    measure <- function(display, plain) sieve_diagram_text_width(plain, k$note_pt * cex)
  }
  lead <- k$note_pt * cex * k$note_lead / 72
  res <- nodes$kind == "residual" & nodes$visible
  residual_of <- stats::setNames(nodes$id[res], nodes$parent[res])
  for (v in unique(edges$from[shown])) {
    at <- which(edges$from[shown] == v)
    rows <- shown[at]
    host <- if (v %in% names(residual_of)) residual_of[[v]] else v
    i <- idx[[host]]
    loop <- which(edges$type == "variance" & edges$from == host)
    dir <- if (nodes$kind[i] == "residual") {
      c(nodes$out_x[i], nodes$out_y[i])
    } else {
      rad <- (if (length(loop)) edges$angle[loop[1L]] else edges$angle[rows[1L]]) * pi / 180
      c(cos(rad), sin(rad))
    }
    # Along the nearer axis, so the lines stay level.
    dir <- if (abs(dir[2L]) >= abs(dir[1L]) - 1e-6) {
      c(0, if (dir[2L] > 0) 1 else -1)
    } else {
      c(sign(dir[1L]), 0)
    }
    # Clear of the loop and of the value written beyond it, with a gap
    # that sets the annotations apart from that value.
    start <- sieve_diagram_reach(nodes, i, dir[1L], dir[2L]) + if (!length(loop)) {
      k$label_gap
    } else if (dir[1L] == 0) {
      sieve_diagram_loop_length(nodes, i) + g$label_room + k$note_gap
    } else {
      sieve_diagram_loop_length(nodes, i) + g$label_room + g$value_width / 2 + k$note_gap
    }
    symbol <- ifelse(edges$type[rows] == "threshold", "tau",
      if (nodes$kind[idx[[v]]] == "latent") "alpha" else "nu"
    )
    value <- edges$label_text[rows]
    if (dir[1L] == 0) {
      number <- stats::ave(seq_along(rows), symbol, FUN = seq_along)
      several <- as.vector(table(symbol)[symbol]) > 1L
      slot <- if (dir[2L] > 0) rev(seq_along(rows)) else seq_along(rows)
      for (j in seq_along(rows)) {
        name <- if (several[j]) call("[", as.name(symbol[j]), number[j]) else as.name(symbol[j])
        display <- call("==", name, value[j])
        plain <- paste0(symbol[j], if (several[j]) number[j] else "", " = ", value[j])
        out$x[at[j]] <- nodes$x[i]
        out$y[at[j]] <- nodes$y[i] + dir[2L] * (start + (slot[j] - 0.5) * lead)
        out$w[at[j]] <- measure(display, plain)
        out$h[at[j]] <- lead
        out$label[at[j]] <- plain
        out$display[[at[j]]] <- display
      }
    } else {
      kinds <- unique(symbol)
      parts <- lapply(kinds, function(sy) {
        call("==", as.name(sy), paste(value[symbol == sy], collapse = ", "))
      })
      display <- if (length(parts) == 1L) parts[[1L]] else as.call(c(as.name("list"), parts))
      plain <- paste(vapply(kinds, function(sy) {
        paste0(sy, " = ", paste(value[symbol == sy], collapse = ", "))
      }, character(1)), collapse = ", ")
      width <- measure(display, plain)
      out$x[at] <- nodes$x[i] + dir[1L] * (start + width / 2)
      out$y[at] <- nodes$y[i]
      out$w[at] <- width
      out$h[at] <- lead
      out$label[at[1L]] <- plain
      out$display[[at[1L]]] <- display
      out$with[at[-1L]] <- edges$id[rows[1L]]
    }
  }
  out
}

#' The paths of the covariance arcs
#'
#' Each arc is the curve the specification routes: the quadratic Bezier
#' curve from center to center with the stated height, drawn between the
#' two node boundaries. Where an arc meets a node, its end is kept out of
#' the way of what else is there: clear of the node's own variance loop and
#' of the straight edges that leave the node, and apart from the ends of
#' the other arcs by about two arrowhead widths, so that every arrowhead is
#' seen. Of several arcs that leave a node on one side, the higher arc is
#' attached nearer the free side, so they do not cross where they meet the
#' node. An arc whose end was moved keeps its top exactly where the
#' specification put it, so the routing and the place of its value are
#' unchanged.
#'
#' The ends are worked out over every relationship that could be drawn
#' (everything not hidden as fixed at zero), so hiding one class of edges
#' does not move the ends of the others.
#'
#' @param nodes The node table of the specification.
#' @param edges The edge table of the specification (all rows).
#' @return A list as long as `edges`: the path of each covariance that
#'   could be drawn (a two column matrix, or `NULL` when its nodes
#'   overlap), `NULL` elsewhere.
#' @author Ken Kelley
#' @noRd
sieve_diagram_arcs <- function(nodes, edges) {
  g <- sieve_diagram_geometry()
  k <- sieve_diagram_look()
  idx <- stats::setNames(seq_len(nrow(nodes)), nodes$id)
  seen <- nodes$visible
  live <- is.na(edges$hidden_reason) | edges$hidden_reason != "fixed_zero"
  both <- seen[idx[edges$from]] & seen[idx[edges$to]]
  paths <- vector("list", nrow(edges))
  arcs <- which(edges$type == "covariance" & live & both & is.finite(edges$curve))
  if (!length(arcs)) {
    return(paths)
  }
  t <- seq(0, 1, length.out = 129)
  bezier <- function(p0, control, p2) {
    cbind(
      (1 - t)^2 * p0[1L] + 2 * t * (1 - t) * control[1L] + t^2 * p2[1L],
      (1 - t)^2 * p0[2L] + 2 * t * (1 - t) * control[2L] + t^2 * p2[2L]
    )
  }
  turned <- function(a, b) ((a - b + 180) %% 360) - 180
  top <- matrix(NA_real_, length(arcs), 2L)
  ends <- NULL
  for (j in seq_along(arcs)) {
    e <- edges[arcs[j], ]
    a <- nodes[idx[[e$from]], , drop = FALSE]
    b <- nodes[idx[[e$to]], , drop = FALSE]
    pa <- c(a$x, a$y)
    pb <- c(b$x, b$y)
    d <- pb - pa
    normal <- c(-d[2L], d[1L]) / max(sqrt(sum(d^2)), 1e-12)
    top[j, ] <- (pa + pb) / 2 + e$curve * normal
    path <- sieve_diagram_trim(bezier(pa, (pa + pb) / 2 + 2 * e$curve * normal, pb), a, b)
    if (is.null(path)) {
      next
    }
    paths[[arcs[j]]] <- path
    first <- path[1L, ] - pa
    last <- path[nrow(path), ] - pb
    bow <- sign(e$curve) * normal
    ends <- rbind(ends, data.frame(
      arc = j, end = 1:2, node = c(e$from, e$to),
      angle = c(atan2(first[2L], first[1L]), atan2(last[2L], last[1L])) * 180 / pi,
      bow = atan2(bow[2L], bow[1L]) * 180 / pi, height = abs(e$curve),
      stringsAsFactors = FALSE
    ))
  }
  if (is.null(ends)) {
    return(paths)
  }
  ends$angle <- ends$angle %% 360
  ends$new <- ends$angle
  straight <- which(edges$type %in% c("loading", "regression", "residual_path") & live & both)
  loops <- which(edges$type == "variance" & live & seen[idx[edges$from]] &
    is.finite(edges$angle))
  grid <- seq(0, 359.5, by = 0.5)
  for (node in unique(ends$node)) {
    at <- which(ends$node == node)
    i <- idx[[node]]
    here <- c(nodes$x[i], nodes$y[i])
    # What the ends of the arcs have to stay clear of at this node.
    blocked <- rep(FALSE, length(grid))
    for (e in loops[edges$from[loops] == node]) {
      clear <- k$arc_clear[1L]
      # The wider gap applies where there is room for it: a residual term
      # with three or more arc ends keeps the angular clearance, or the
      # arcs are pushed beside the neighbors' loops where their values do
      # not fit.
      if (identical(nodes$kind[i], "residual") && length(at) <= 2L) {
        rad <- edges$angle[e] * pi / 180
        reach <- sieve_diagram_reach(nodes, i, cos(rad), sin(rad))
        clear <- max(clear, k$loop_arc_gap / reach * 180 / pi)
      }
      half <- sieve_diagram_loop_foot(nodes, i, edges$angle[e]) + clear
      blocked <- blocked | abs(turned(grid, edges$angle[e])) < half
    }
    for (e in straight[edges$from[straight] == node | edges$to[straight] == node]) {
      other <- idx[[if (edges$from[e] == node) edges$to[e] else edges$from[e]]]
      way <- atan2(nodes$y[other] - here[2L], nodes$x[other] - here[1L]) * 180 / pi
      blocked <- blocked | abs(turned(grid, way)) < k$arc_clear[2L]
    }
    step <- k$arc_apart / mean(sieve_diagram_reach(
      nodes, rep(i, 8L), cos(0:7 * pi / 4),
      sin(0:7 * pi / 4)
    )) * 180 / pi
    if (!any(blocked)) {
      if (length(at) < 2L) {
        next
      }
      # Nothing else at this node: only keep the ends apart, starting after
      # the widest gap so the sequence does not wrap through a group.
      at <- at[order(ends$angle[at])]
      gap <- diff(c(ends$angle[at], ends$angle[at[1L]] + 360))
      at <- c(at[-seq_len(which.max(gap))], at[seq_len(which.max(gap))])
      angle <- ends$angle[at]
      angle <- angle + ifelse(angle < angle[1L], 360, 0)
      lift <- (seq_along(at) - 1) * min(step, 360 / length(at))
      ends$new[at] <- stats::isoreg(angle - lift)$yf + lift
      next
    }
    if (all(blocked)) {
      next
    }
    # The free stretches of the outline, each from `lo` over `wide` degrees.
    start <- which(!blocked & c(blocked[length(blocked)], blocked[-length(blocked)]))
    lo <- grid[start]
    wide <- vapply(start, function(s0) {
      run <- c(blocked[s0:length(blocked)], blocked[seq_len(s0 - 1L)])
      (match(TRUE, run) - 2L) * 0.5
    }, numeric(1))
    # Each end goes to the stretch it is in, or to the nearest one.
    home <- vapply(ends$angle[at], function(a) {
      inside <- (a - lo) %% 360
      far <- ifelse(inside <= wide, 0, pmin(abs(turned(a, lo)), abs(turned(a, lo + wide))))
      which.min(far)
    }, integer(1))
    for (h in unique(home)) {
      mine <- at[home == h]
      # Position along the stretch, 0 at its start; an end that lies
      # outside the stretch is beyond one of its two limits.
      u <- turned(ends$angle[mine], lo[h] + wide[h] / 2) + wide[h] / 2
      # Of the arcs on one side of the free direction, the higher one is
      # attached nearer to it.
      off <- turned(ends$angle[mine], ends$bow[mine])
      side <- paste(sign(round(off, 6)), round(ends$bow[mine] / 45))
      for (sd in unique(side[duplicated(side)])) {
        grp <- which(side == sd)
        u[grp[order(-ends$height[mine[grp]])]] <- u[grp[order(abs(off[grp]))]]
      }
      ord <- order(u)
      u <- pmin(pmax(u, 0), wide[h])
      m <- length(ord)
      st <- if (m > 1L) min(step, wide[h] / (m - 1L)) else 0
      lift <- (seq_len(m) - 1) * st
      placed <- if (m > 1L) stats::isoreg(u[ord] - lift)$yf + lift else u[ord]
      placed <- pmax(lift, pmin(placed, wide[h] - (m - seq_len(m)) * st))
      ends$new[mine[ord]] <- (lo[h] + placed) %% 360
    }
  }
  moved <- abs(turned(ends$new, ends$angle)) > 0.25
  for (j in unique(ends$arc[moved])) {
    point <- function(end) {
      row <- ends[ends$arc == j & ends$end == end, ]
      i <- idx[[row$node]]
      rad <- row$new * pi / 180
      c(nodes$x[i], nodes$y[i]) +
        sieve_diagram_reach(nodes, i, cos(rad), sin(rad)) * c(cos(rad), sin(rad))
    }
    p0 <- point(1L)
    p2 <- point(2L)
    paths[[arcs[j]]] <- bezier(p0, 2 * top[j, ] - (p0 + p2) / 2, p2)
  }
  paths
}

#' The path of every visible edge
#'
#' Pure geometry, computed before anything is drawn and before any
#' graphics setting is touched: a straight edge is trimmed to the node
#' boundaries, a covariance follows its arc, and a variance is an
#' elliptical loop that starts and ends on the boundary of its node. An edge whose
#' nodes overlap has no path; that is an error here, so an edge is never
#' silently left out and a refusal leaves the device as it was.
#'
#' A loading arrives at the middle of the side of its indicator that faces
#' the factor, so the arrowheads of a group line up and no line cuts the
#' corner of a neighboring box. Every other straight edge runs from center
#' to center.
#'
#' Two things a straight line must never do, because the figure would then
#' read as a different model: run through a node that is not one of its
#' ends (a direct effect drawn through its mediator reads as a chain), or
#' lie on the same line as another edge and overlap it (two arrowheads at
#' one point, one value apparently belonging to the other arrow). The
#' layout keeps nodes off such lines; this function checks every straight
#' edge nonetheless, and an edge that fails is re-routed as a curve that
#' clears what is in its way, bowing away from it. If no curve of up to
#' three inches clears it, the diagram is refused with reason
#' `"geometry"` rather than drawn wrong.
#'
#' @param spec A drawable specification.
#' @return A list with `path` (a list with one element per visible edge:
#'   a two column matrix, or `NULL` for an intercept or threshold, which
#'   has no line), `aim` (a two column matrix: the unit direction of each
#'   straight edge where it leaves its node), `axis` (for each loading
#'   `"v"` when the indicators of its factor form a row, so the loadings
#'   run mostly vertically, or `"h"` when they form a column; `"free"` for
#'   other edges), `paired` (`TRUE` for a regression whose opposite is also
#'   drawn; the two are set side by side), and `curved` (`TRUE` for a
#'   straight edge that had to be re-routed as a curve).
#' @author Ken Kelley
#' @noRd
sieve_diagram_paths <- function(spec) {
  g <- sieve_diagram_geometry()
  k <- sieve_diagram_look()
  nodes <- spec$nodes
  idx <- stats::setNames(seq_len(nrow(nodes)), nodes$id)
  edges <- spec$edges[spec$edges$visible, , drop = FALSE]
  loading <- edges$type == "loading"
  fan <- vapply(unique(edges$from[loading]), function(tail) {
    to <- idx[edges$to[loading & edges$from == tail]]
    spread <- c(diff(range(nodes$x[to])), diff(range(nodes$y[to])))
    if (max(spread) < 1e-6) {
      spread <- rev(abs(c(nodes$x[to[1L]], nodes$y[to[1L]]) -
        c(nodes$x[idx[[tail]]], nodes$y[idx[[tail]]])))
    }
    if (spread[1L] >= spread[2L]) "v" else "h"
  }, character(1))
  path <- sieve_diagram_arcs(nodes, spec$edges)[spec$edges$visible]
  aim <- matrix(NA_real_, nrow(edges), 2L)
  axis <- rep("free", nrow(edges))
  regression <- edges$type == "regression"
  paired <- regression & paste(edges$to, edges$from) %in%
    paste(edges$from, edges$to)[regression]
  for (r in seq_len(nrow(edges))) {
    e <- edges[r, ]
    a <- nodes[idx[[e$from]], , drop = FALSE]
    b <- nodes[idx[[e$to]], , drop = FALSE]
    pa <- c(a$x, a$y)
    pb <- c(b$x, b$y)
    if (e$type %in% c("loading", "regression", "residual_path")) {
      target <- pb
      if (e$type == "loading") {
        axis[r] <- fan[[e$from]]
      }
      if (e$type == "loading" && b$kind == "observed") {
        port <- if (axis[r] == "v") {
          c(pb[1L], pb[2L] + sign(pa[2L] - pb[2L]) * b$h / 2)
        } else {
          c(pb[1L] + sign(pa[1L] - pb[1L]) * b$w / 2, pb[2L])
        }
        way <- seq(0.02, 0.98, length.out = 25)
        if (!any(sieve_diagram_inside(
          b, pa[1L] + way * (port[1L] - pa[1L]),
          pa[2L] + way * (port[2L] - pa[2L])
        ))) {
          target <- port
        }
      }
      aim[r, ] <- (target - pa) / max(sqrt(sum((target - pa)^2)), 1e-12)
      start <- pa
      if (paired[r]) {
        # Each of two opposite regressions keeps to its own side, so the
        # pair reads as two arrows and never as one double-headed arrow.
        shift <- k$pair_gap / 2 * c(-aim[r, 2L], aim[r, 1L])
        start <- pa + shift
        target <- target + shift
      }
      path[r] <- list(sieve_diagram_trim(rbind(start, target, deparse.level = 0), a, b))
    } else if (e$type == "variance") {
      # An ellipse that starts a little inside the node and reaches the
      # loop length beyond it; what lies inside the node is trimmed away.
      path[r] <- list(sieve_diagram_loop_path(nodes, idx[[e$from]], e$angle))
    }
    if (!e$type %in% c("intercept", "threshold") && is.null(path[[r]])) {
      sieve_diagram_abort(sprintf(
        "The %s %s cannot be drawn because its nodes overlap (%s and %s); move them apart.",
        gsub("_", " ", e$type, fixed = TRUE), e$id, e$from, e$to
      ), "geometry", spec)
    }
  }

  # The hard check on the straight lines. A line whose interior enters a
  # node that is not one of its ends, or that lies on the line of another
  # edge and overlaps it, is re-routed as a curve; failing that, refused.
  curved <- rep(FALSE, nrow(edges))
  lines <- which(edges$type %in% c("loading", "regression", "residual_path"))
  shown <- which(nodes$visible)
  clear <- 0.04
  interior <- function(p) {
    if (nrow(p) == 2L) {
      t <- seq(0, 1, length.out = 201)
      p <- cbind(p[1L, 1L] + t * (p[2L, 1L] - p[1L, 1L]), p[1L, 2L] + t * (p[2L, 2L] - p[1L, 2L]))
    }
    p[-c(1L, nrow(p)), , drop = FALSE]
  }
  entered <- function(p, ends, grow = 0) {
    pts <- interior(p)
    hit <- character(0)
    for (i in setdiff(shown, idx[ends])) {
      node <- nodes[i, , drop = FALSE]
      node$w <- node$w + 2 * grow
      node$h <- node$h + 2 * grow
      if (any(sieve_diagram_inside(node, pts[, 1L], pts[, 2L]))) {
        hit <- c(hit, nodes$id[i])
      }
    }
    hit
  }
  overlap <- function(i, j) {
    pi_ <- path[[i]]
    pj <- path[[j]]
    if (is.null(pi_) || is.null(pj) || nrow(pi_) != 2L || nrow(pj) != 2L) {
      return(FALSE)
    }
    u <- (pi_[2L, ] - pi_[1L, ]) / max(sqrt(sum((pi_[2L, ] - pi_[1L, ])^2)), 1e-12)
    off <- function(q) abs((q[1L] - pi_[1L, 1L]) * u[2L] - (q[2L] - pi_[1L, 2L]) * u[1L])
    if (off(pj[1L, ]) > 0.01 || off(pj[2L, ]) > 0.01) {
      return(FALSE)
    }
    pr <- function(q) sum((q - pi_[1L, ]) * u)
    a <- sort(c(0, pr(pi_[2L, ])))
    b <- sort(c(pr(pj[1L, ]), pr(pj[2L, ])))
    min(a[2L], b[2L]) - max(a[1L], b[1L]) > 0.02
  }
  bend <- function(r, blocking) {
    e <- edges[r, ]
    a <- nodes[idx[[e$from]], , drop = FALSE]
    b <- nodes[idx[[e$to]], , drop = FALSE]
    pa <- c(a$x, a$y)
    pb <- c(b$x, b$y)
    d <- pb - pa
    u <- d / max(sqrt(sum(d^2)), 1e-12)
    normal <- c(-u[2L], u[1L])
    # Bow away from what is in the way; with nothing to one side, upward.
    side <- 1
    if (length(blocking)) {
      at <- idx[blocking]
      lean <- sum((c(mean(nodes$x[at]), mean(nodes$y[at])) - (pa + pb) / 2) * normal)
      if (abs(lean) > 1e-6) {
        side <- -sign(lean)
      }
    }
    for (height in c(0.5, 0.7, 0.9, 1.2, 1.5, 2, 2.5, 3)) {
      for (s in c(side, -side)) {
        p <- sieve_diagram_trim(sieve_diagram_bend_curve(pa, pb, s * normal, height), a, b)
        if (!is.null(p) && !length(entered(p, c(e$from, e$to), clear))) {
          return(p)
        }
      }
    }
    NULL
  }
  reroute <- function(r, blocking, why) {
    p <- bend(r, blocking)
    if (is.null(p)) {
      e <- edges[r, ]
      sieve_diagram_abort(sprintf(
        paste0(
          "The %s %s cannot be drawn as a straight line (%s), and no curve around it ",
          "was found; move the nodes apart (positions)."
        ),
        gsub("_", " ", e$type, fixed = TRUE), e$id, why
      ), "geometry", spec)
    }
    p
  }
  for (r in lines) {
    hit <- entered(path[[r]], c(edges$from[r], edges$to[r]))
    if (length(hit)) {
      p <- reroute(r, hit, sprintf("it would run through %s", paste(hit, collapse = ", ")))
      path[r] <- list(p)
      aim[r, ] <- (p[2L, ] - p[1L, ]) / max(sqrt(sum((p[2L, ] - p[1L, ])^2)), 1e-12)
      curved[r] <- TRUE
    }
  }
  for (i in lines) {
    for (j in lines[lines > i]) {
      if (overlap(i, j)) {
        longer <- if (sieve_diagram_length(path[[i]]) >= sieve_diagram_length(path[[j]])) i else j
        p <- reroute(
          longer, character(0),
          sprintf("it lies on the line of %s", edges$id[if (longer == i) j else i])
        )
        path[longer] <- list(p)
        aim[longer, ] <- (p[2L, ] - p[1L, ]) / max(sqrt(sum((p[2L, ] - p[1L, ])^2)), 1e-12)
        curved[longer] <- TRUE
      }
    }
  }
  list(path = path, aim = aim, axis = axis, paired = paired, curved = curved)
}

#' Which segments cross a box?
#'
#' @param segs A four column matrix of segments: x0, y0, x1, y1.
#' @param box The box: xmin, xmax, ymin, ymax.
#' @return A logical vector, one per segment.
#' @author Ken Kelley
#' @noRd
sieve_diagram_crosses <- function(segs, box) {
  n <- nrow(segs)
  if (!n) {
    return(logical(0))
  }
  dx <- segs[, 3L] - segs[, 1L]
  dy <- segs[, 4L] - segs[, 2L]
  enter <- rep(0, n)
  leave <- rep(1, n)
  out <- rep(FALSE, n)
  # Clip each segment against the four sides in turn (Liang and Barsky).
  for (side in list(
    list(-dx, segs[, 1L] - box[1L]), list(dx, box[2L] - segs[, 1L]),
    list(-dy, segs[, 2L] - box[3L]), list(dy, box[4L] - segs[, 2L])
  )) {
    p <- side[[1L]]
    q <- side[[2L]]
    flat <- abs(p) < 1e-12
    out <- out | (flat & q < 0)
    ratio <- ifelse(flat, 0, q / ifelse(flat, 1, p))
    enter <- ifelse(!flat & p < 0, pmax(enter, ratio), enter)
    leave <- ifelse(!flat & p > 0, pmin(leave, ratio), leave)
  }
  !out & enter <= leave
}

#' Distance from a point to each of a set of segments
#'
#' @param segs A four column matrix of segments: x0, y0, x1, y1.
#' @param at The point.
#' @return A numeric vector, one per segment.
#' @author Ken Kelley
#' @noRd
sieve_diagram_distance <- function(segs, at) {
  dx <- segs[, 3L] - segs[, 1L]
  dy <- segs[, 4L] - segs[, 2L]
  along <- ((at[1L] - segs[, 1L]) * dx + (at[2L] - segs[, 2L]) * dy) / pmax(dx^2 + dy^2, 1e-12)
  along <- pmin(1, pmax(0, along))
  sqrt((segs[, 1L] + along * dx - at[1L])^2 + (segs[, 2L] + along * dy - at[2L])^2)
}

#' Turn a specification into drawing primitives
#'
#' Adds to the paths what needs the text metrics of the open device: the
#' label of each node at the size at which it fits, and for each visible
#' edge its label and where the label goes. Nothing is drawn.
#'
#' Label placement. The value of a variance sits just beyond its loop. The
#' value of a covariance sits just outside the top of an arc that bows up
#' or down, and on the arc (on a white ground) when the arc bows sideways,
#' where a value beside it would float between neighboring arcs. A loading
#' label sits beside its line (toward the outside of the fan) at one height
#' for all the loadings of a factor whose indicators form a row, and on its
#' line at one distance when the indicators form a column. A regression
#' label sits beside its line near the predictor.
#'
#' Each label of a straight edge is then checked against everything else
#' that is drawn: the box of the label must not lie on a node, on another
#' label, or on the line of another edge, and a label beside its line must
#' be clearly nearer to its own line than to any line that is not a sibling
#' of it (two regressions that cross leave a wedge in which a value could
#' belong to either). A label that fails is moved to the first place that
#' passes: the other side of its line, another distance along it, or onto
#' the line itself on a white ground, where it cannot be attributed to
#' another line.
#'
#' @param spec A drawable specification.
#' @param style The resolved style.
#' @param unit The `cex` of one point of type on this device at this scale.
#' @param paths The paths from `sieve_diagram_paths()`.
#' @param canvas The canvas from `sieve_diagram_canvas()`; labels stay
#'   inside it.
#' @return A list with `nodes` (a data frame; `display` holds what is
#'   drawn as the label) and `edges` (a list of primitives, one per
#'   visible edge).
#' @author Ken Kelley
#' @noRd
sieve_diagram_scene <- function(spec, style, unit, paths, canvas) {
  g <- sieve_diagram_geometry()
  k <- sieve_diagram_look()
  all_nodes <- spec$nodes
  idx <- stats::setNames(seq_len(nrow(all_nodes)), all_nodes$id)
  measure <- function(label, cex, font = 1) {
    c(
      graphics::strwidth(label, "user", cex = cex, font = font),
      graphics::strheight(label, "user", cex = cex, font = font)
    )
  }

  nodes <- all_nodes
  nodes$border <- style$border
  nodes$fill <- style$fill
  nodes$greek <- sieve_diagram_greek(all_nodes)
  # Residual terms that share a letter are numbered in model order.
  nodes$index <- NA_integer_
  for (letter in unique(stats::na.omit(nodes$greek))) {
    same <- which(nodes$greek %in% letter)
    if (length(same) > 1L) {
      nodes$index[same] <- seq_along(same)
    }
  }
  nodes <- nodes[nodes$visible, , drop = FALSE]
  res <- nodes$kind == "residual"
  nodes$font <- 1L
  nodes$shape <- ifelse(nodes$kind == "observed", "rectangle",
    ifelse(res, "circle", "ellipse")
  )
  nodes$drawn_label <- ifelse(res, nodes$greek, nodes$label)
  display <- as.list(sieve_diagram_device_text(nodes$label_wrapped))
  display[res] <- lapply(which(res), function(i) {
    if (is.na(nodes$index[i])) {
      as.name(nodes$greek[i])
    } else {
      call("[", as.name(nodes$greek[i]), nodes$index[i])
    }
  })
  nodes$display <- I(display)
  # A label must fit its node with the fonts of this device. Nodes that
  # share a size share a type size.
  base <- g$base_pt * unit
  room <- rep(1, nrow(nodes))
  for (i in which(!res)) {
    text <- measure(nodes$label_wrapped[i], base, nodes$font[i])
    room[i] <- if (nodes$kind[i] == "latent") {
      0.95 / sqrt((text[1L] / nodes$w[i])^2 + (text[2L] / nodes$h[i])^2)
    } else {
      min((nodes$w[i] - 0.06) / text[1L], (nodes$h[i] - 0.05) / text[2L])
    }
  }
  group <- paste(nodes$kind, nodes$level, nodes$role == "indicator")
  nodes$cex <- base * as.numeric(stats::ave(pmin(1, room), group, FUN = min))
  greek_cex <- k$greek_pt * unit
  for (i in which(res)) {
    wide <- graphics::strwidth(nodes$display[[i]], "user", cex = greek_cex)
    nodes$cex[i] <- greek_cex * min(1, 0.80 * nodes$w[i] / max(wide, 1e-9))
  }

  edges <- spec$edges[spec$edges$visible, , drop = FALSE]
  edge_cex <- g$edge_pt * unit
  note_cex <- k$note_pt * unit
  notes <- sieve_diagram_notes(spec, style$cex, function(display, plain) {
    graphics::strwidth(display, "user", cex = note_cex)
  })
  prims <- vector("list", nrow(edges))
  texts <- sieve_diagram_edge_text(edges)$text
  for (r in seq_len(nrow(edges))) {
    e <- edges[r, ]
    text <- texts[r]
    flagged <- isTRUE(e$improper)
    font <- if (flagged) 2L else 1L
    half <- if (nzchar(text)) measure(text, edge_cex, font) / 2 + k$label_pad else c(0, 0)
    status <- if (is.na(e$par) || !(e$fixed || e$derived)) {
      "free"
    } else if (e$fixed) {
      "fixed"
    } else {
      "derived"
    }
    shape <- switch(e$type,
      loading = ,
      regression = ,
      residual_path = "line",
      covariance = "arc",
      variance = "loop",
      "note"
    )
    prims[[r]] <- list(
      id = e$id, type = e$type, from = e$from, to = e$to, label = text,
      label_text = e$label_text, display = sieve_diagram_device_text(text), font = font,
      cex = edge_cex, half = half, flagged = flagged, status = status,
      lty = k$lty[[status]], path = paths$path[[r]],
      heads = c(shape != "line" && shape != "note", shape != "note"),
      at = c(NA_real_, NA_real_), shape = shape, with = NA_character_, on_line = FALSE,
      curved = isTRUE(paths$curved[r]), cost = 0
    )
  }

  # What a label must stay clear of: every drawn path, as short segments
  # that remember their edge (arcs and loops at their full resolution, so
  # a label cannot slip between two sampled points of a curve); every
  # drawn node; every label already placed.
  segs <- do.call(rbind, lapply(seq_along(prims), function(r) {
    path <- prims[[r]]$path
    if (is.null(path)) {
      return(NULL)
    }
    cbind(path[-nrow(path), , drop = FALSE], path[-1L, , drop = FALSE], r)
  }))
  if (is.null(segs)) {
    segs <- matrix(numeric(0), 0L, 5L)
  }
  owner <- segs[, 5L]
  # Edges of one kind that leave one node are siblings: a regular fan.
  family <- paste(edges$type, edges$from)
  shape_of <- vapply(prims, function(p) p$shape, character(1))
  # The box of every label placed so far, one per edge.
  boxes <- vector("list", length(prims))
  box_of <- function(p) {
    c(p$at[1L] - p$half[1L], p$at[1L] + p$half[1L], p$at[2L] - p$half[2L], p$at[2L] + p$half[2L])
  }
  placed_boxes <- function() {
    have <- !vapply(boxes, is.null, logical(1))
    if (!any(have)) matrix(numeric(0), 0L, 4L) else do.call(rbind, boxes[have])
  }
  on_node <- function(box) {
    gx <- rep(c(box[1L], (box[1L] + box[2L]) / 2, box[2L]), 3L)
    gy <- rep(c(box[3L], (box[3L] + box[4L]) / 2, box[4L]), each = 3L)
    near <- which(abs(nodes$x - (box[1L] + box[2L]) / 2) < nodes$w / 2 + (box[2L] - box[1L]) / 2 &
      abs(nodes$y - (box[3L] + box[4L]) / 2) < nodes$h / 2 + (box[4L] - box[3L]) / 2)
    for (i in near) {
      if (nodes$kind[i] == "observed" || any(sieve_diagram_inside(nodes[i, ], gx, gy))) {
        return(TRUE)
      }
    }
    FALSE
  }
  # The cost of putting the label of edge `r` in a place: zero when it is
  # clear of everything. A label beside a line must also be clearly nearer
  # to its own line than to any line it could be taken for.
  cost <- function(r, at, half, beside, own) {
    box <- c(at[1L] - half[1L], at[1L] + half[1L], at[2L] - half[2L], at[2L] + half[2L])
    if (box[1L] < canvas$xlim[1L] || box[2L] > canvas$xlim[2L] ||
      box[3L] < canvas$ylim[1L] || box[4L] > canvas$ylim[2L]) {
      return(1000)
    }
    total <- 0
    if (on_node(box)) {
      total <- total + 100
    }
    placed <- placed_boxes()
    if (nrow(placed) && any(box[1L] < placed[, 2L] & placed[, 1L] < box[2L] &
      box[3L] < placed[, 4L] & placed[, 3L] < box[4L])) {
      total <- total + 100
    }
    kin <- owner != r & family[owner] == family[r]
    other <- owner != r & !kin
    grown <- box + c(-1, 1, -1, 1) * k$label_clear
    if (any(sieve_diagram_crosses(segs[kin, , drop = FALSE], box)) ||
      any(sieve_diagram_crosses(segs[other, , drop = FALSE], grown))) {
      total <- total + 50
    }
    foreign <- owner != r
    if (beside && any(foreign)) {
      # Measured from the edge of the box, against every other line, the
      # lines of the same fan included: a value between two loadings must
      # be clearly nearer to its own.
      gx <- rep(c(box[1L], at[1L], box[2L]), 3L)
      gy <- rep(c(box[3L], at[2L], box[4L]), each = 3L)
      nearest <- function(lines) {
        min(vapply(seq_along(gx), function(q) {
          min(sieve_diagram_distance(lines, c(gx[q], gy[q])))
        }, numeric(1)))
      }
      if (nearest(segs[foreign, , drop = FALSE]) < 1.8 * nearest(own) + 0.03) {
        total <- total + 10
      }
    }
    total
  }
  # The segments of one path, as `own` for the nearest-line test.
  own_segments <- function(path) {
    cbind(path[-nrow(path), , drop = FALSE], path[-1L, , drop = FALSE])
  }
  # Candidate places along a curved path, at fractions of its length:
  # `out` beside it on the given side, `inn` beside it on the other side,
  # and `on` on the path itself on a white ground.
  along <- function(path, fractions, half, wide, side = 1) {
    own <- own_segments(path)
    total <- sieve_diagram_length(path)
    beside_at <- function(f, s) {
      p <- sieve_diagram_along(path, f * total)
      normal <- c(-p$u[2L], p$u[1L])
      list(
        at = p$at + s * normal * (abs(normal[1L]) * half[1L] + abs(normal[2L]) * half[2L] +
          k$label_gap),
        half = half, beside = TRUE, own = own, on_line = FALSE
      )
    }
    on_at <- function(f) {
      p <- sieve_diagram_along(path, f * total)
      list(at = p$at, half = wide, beside = FALSE, own = own, on_line = TRUE)
    }
    list(
      out = lapply(fractions, beside_at, s = side),
      inn = lapply(fractions, beside_at, s = -side),
      on = lapply(fractions, on_at)
    )
  }
  settle <- function(r, places) {
    best <- NULL
    for (place in places) {
      place$cost <- cost(r, place$at, place$half, place$beside, place$own)
      if (is.null(best) || place$cost < best$cost) {
        best <- place
      }
      if (best$cost == 0) {
        break
      }
    }
    best
  }
  take <- function(p, best) {
    p$at <- best$at
    p$half <- best$half
    p$on_line <- isTRUE(best$on_line)
    p$cost <- best$cost
    p
  }

  # Annotations first: their places are fixed by the layout.
  for (r in which(shape_of == "note")) {
    p <- prims[[r]]
    note <- notes[match(p$id, notes$id), ]
    p$label <- note$label
    p$display <- note$display[[1L]]
    p$with <- note$with
    p$cex <- note_cex
    p$at <- c(note$x, note$y)
    p$half <- c(note$w, note$h) / 2
    p$status <- "note"
    prims[[r]] <- p
    if (nzchar(p$label) && all(is.finite(p$at))) {
      boxes[[r]] <- box_of(p)
    }
  }

  # Then the loops: the value of a variance sits just beyond its loop. When
  # that place is taken (two loops turned toward each other, an arc
  # passing) it moves beside the loop or a little further out.
  for (r in which(shape_of == "loop")) {
    p <- prims[[r]]
    e <- edges[r, ]
    i <- idx[[e$from]]
    pa <- c(all_nodes$x[i], all_nodes$y[i])
    rad <- e$angle * pi / 180
    dir <- c(cos(rad), sin(rad))
    normal <- c(-dir[2L], dir[1L])
    far <- sieve_diagram_reach(all_nodes, i, dir[1L], dir[2L]) + sieve_diagram_loop_length(all_nodes, i) + k$label_gap
    half <- p$half
    extent <- function(v) abs(v[1L]) * half[1L] + abs(v[2L]) * half[2L]
    own <- own_segments(p$path)
    # The loop points at its value, so the value is not held to the
    # nearest-line rule; it must only be clear of everything.
    place <- function(out, aside) {
      list(
        at = pa + dir * (far + out + extent(dir)) + normal * aside, half = half,
        beside = FALSE, own = own, on_line = FALSE
      )
    }
    aside <- extent(normal) + sieve_diagram_loop_width(all_nodes, i, e$angle) / 2 + k$label_gap
    places <- list(
      place(0, 0), place(0, aside), place(0, -aside), place(0.12, 0),
      place(0.12, aside), place(0.12, -aside), place(0.25, 0)
    )
    p <- take(p, settle(r, places))
    prims[[r]] <- p
    boxes[[r]] <- box_of(p)
  }

  # Then the arcs. The value of a covariance sits just outside the top of
  # an arc that bows up or down, and on the arc (on a white ground) when
  # the arc bows sideways, where a value beside it would float between
  # neighboring arcs. An arc too short to carry its value between its two
  # arrowheads gets the value beside it. Every candidate is checked against
  # the other arcs at full resolution.
  for (r in which(shape_of == "arc")) {
    p <- prims[[r]]
    e <- edges[r, ]
    a <- all_nodes[idx[[e$from]], , drop = FALSE]
    b <- all_nodes[idx[[e$to]], , drop = FALSE]
    d <- c(b$x - a$x, b$y - a$y)
    normal <- c(-d[2L], d[1L]) / max(sqrt(sum(d^2)), 1e-12)
    bow <- sign(e$curve) * normal
    half <- p$half
    wide <- half + (k$on_line_pad - k$label_pad)
    total <- sieve_diagram_length(p$path)
    on_ok <- total - 2 * k$head$covariance[1L] - 2 * wide[1L] >= 0.1
    got <- along(p$path, c(0.5, 0.35, 0.65), half, wide, side = if (e$curve < 0) -1 else 1)
    places <- if (abs(bow[1L]) > abs(bow[2L]) && on_ok) {
      c(got$on, got$out, got$inn)
    } else {
      c(got$out, if (on_ok) got$on, got$inn)
    }
    p <- take(p, settle(r, places))
    prims[[r]] <- p
    boxes[[r]] <- box_of(p)
  }

  # Two regressions that cross leave a wedge in which a value beside its
  # line could belong to either. The values of the regressions are then
  # written on their lines, all of them, so the figure stays consistent.
  reg <- which(vapply(prims, function(p) identical(p$type, "regression"), logical(1)))
  ends_of <- function(r) {
    c(prims[[r]]$path[1L, ], prims[[r]]$path[nrow(prims[[r]]$path), ])
  }
  side_of <- function(a, b, c) (b[1L] - a[1L]) * (c[2L] - a[2L]) - (b[2L] - a[2L]) * (c[1L] - a[1L])
  crossed <- FALSE
  for (i in seq_along(reg)) {
    for (j in seq_len(i - 1L)) {
      p <- ends_of(reg[i])
      q <- ends_of(reg[j])
      if (side_of(p[1:2], p[3:4], q[1:2]) * side_of(p[1:2], p[3:4], q[3:4]) < 0 &&
        side_of(q[1:2], q[3:4], p[1:2]) * side_of(q[1:2], q[3:4], p[3:4]) < 0) {
        crossed <- TRUE
      }
    }
  }

  # Then the straight edges, each with its preferred place first. A label
  # beside a line keeps to the side rule of its fan: when its place at the
  # common height is taken it moves along its own line, then onto the line
  # on a white ground, and only as a last resort to the other side.
  for (r in which(shape_of == "line")) {
    p <- prims[[r]]
    if (!nzchar(p$label)) {
      next
    }
    e <- edges[r, ]
    a <- all_nodes[idx[[e$from]], , drop = FALSE]
    pa <- c(a$x, a$y)
    u <- paths$aim[r, ]
    normal <- c(-u[2L], u[1L])
    tail <- p$path[1L, ]
    tip <- p$path[nrow(p$path), ]
    own <- matrix(c(tail, tip), 1L)
    half <- p$half
    wide <- half + (k$on_line_pad - k$label_pad)
    on_line <- function(f) {
      list(at = tail + f * (tip - tail), half = wide, beside = FALSE, own = own, on_line = TRUE)
    }
    places <- list()
    if (isTRUE(p$curved)) {
      # A line re-routed as a curve: along the curve, on the side it bows
      # to first, so the value sits in the bend and not under the chord.
      chord <- tip - tail
      bulge <- sieve_diagram_along(p$path, sieve_diagram_length(p$path) / 2)$at - (tail + tip) / 2
      got <- along(p$path, c(e$label_pos, 0.5, 0.3, 0.7), half, wide,
        side = if (chord[1L] * bulge[2L] - chord[2L] * bulge[1L] < 0) -1 else 1
      )
      places <- c(got$out, got$inn, got$on)
    } else if (paths$axis[r] == "v" && abs(u[2L]) > 0.35) {
      # Indicators in a row: beside the line, at one height per factor,
      # toward the outside of the fan.
      edge_y <- pa[2L] + sign(u[2L]) * a$h / 2
      out <- if (abs(u[1L]) < 0.01) 1 else sign(u[1L])
      beside <- function(f, side) {
        y <- edge_y + f * (tip[2L] - edge_y)
        x <- pa[1L] + (y - pa[2L]) * u[1L] / u[2L]
        list(
          at = c(x + side * (half[1L] + k$label_gap + half[2L] * abs(u[1L] / u[2L])), y),
          half = half, beside = TRUE, own = own, on_line = FALSE
        )
      }
      # At the common height first, beside the line and then on it; other
      # heights after that; the other side last.
      on_line_at <- function(f) {
        y <- edge_y + f * (tip[2L] - edge_y)
        list(
          at = c(pa[1L] + (y - pa[2L]) * u[1L] / u[2L], y), half = wide, beside = FALSE,
          own = own, on_line = TRUE
        )
      }
      heights <- c(0.5, 0.75, 0.4, 0.86)
      places <- c(
        list(beside(e$label_pos, out), on_line_at(e$label_pos)),
        lapply(heights, beside, side = out),
        lapply(c(0.5, 0.75), on_line_at),
        lapply(c(e$label_pos, heights), beside, side = -out)
      )
    } else if (paths$axis[r] == "h" && abs(u[1L]) > 0.35) {
      # Indicators in a column: on the line, at one distance per factor.
      edge_x <- pa[1L] + sign(u[1L]) * a$w / 2
      for (f in c(e$label_pos, 0.5, 0.75, 0.4, 0.86)) {
        x <- edge_x + f * (tip[1L] - edge_x)
        places[[length(places) + 1L]] <- list(
          at = c(x, pa[2L] + (x - pa[1L]) * u[2L] / u[1L]), half = wide, beside = FALSE,
          own = own, on_line = TRUE
        )
      }
    } else {
      # Beside the line, away from the other edges of this kind that leave
      # the same node; above the line when there is no other.
      out <- if (normal[2L] < -1e-9 || (abs(normal[2L]) <= 1e-9 && normal[1L] < 0)) -1 else 1
      sib <- which(edges$type == e$type & edges$from == e$from)
      if (paths$paired[r]) {
        # On the outer side of its own line of the pair.
        out <- 1
      } else if (length(sib) > 1L) {
        mean_dir <- colMeans(paths$aim[sib, , drop = FALSE])
        turn <- u[1L] * mean_dir[2L] - u[2L] * mean_dir[1L]
        if (abs(turn) > 1e-6) {
          out <- -sign(turn)
        }
      }
      beside <- function(f, side) {
        list(
          at = tail + f * (tip - tail) + side * normal *
            (abs(normal[1L]) * half[1L] + abs(normal[2L]) * half[2L] + k$label_gap),
          half = half, beside = TRUE, own = own, on_line = FALSE
        )
      }
      first <- e$label_pos
      if (crossed && e$type == "regression") {
        places <- lapply(c(first, 0.2, 0.42), on_line)
      }
      for (f in c(first, 0.2, 0.42)) {
        places <- c(places, list(beside(f, out), beside(f, -out)))
      }
      places <- c(places, lapply(c(first, 0.2, 0.42), on_line))
      for (f in c(0.55, 0.7)) {
        places <- c(places, list(beside(f, out), beside(f, -out), on_line(f)))
      }
    }
    p <- take(p, settle(r, places))
    prims[[r]] <- p
    boxes[[r]] <- box_of(p)
  }
  list(nodes = nodes, edges = prims)
}

# ==== drawing ====

#' Stroke a path and its arrowheads
#'
#' The line stops inside each arrowhead so that the point of the head, not
#' the end of the line, touches the node. A head points along the path as
#' it arrives, which keeps the heads of curved arrows and loops aligned
#' with their curves.
#'
#' @param path A two column matrix of points, ending on the node
#'   boundaries.
#' @param heads Two logicals: a head at the start, a head at the end.
#' @param head Length and width of a head, in layout units.
#' @param col,lwd,lty Line color, width, and type.
#' @return `NULL`, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_diagram_stroke <- function(path, heads, head, col, lwd, lty) {
  total <- sieve_diagram_length(path)
  len <- min(head[1L], total / 2.2)
  wid <- head[2L] * len / head[1L]
  inset <- 0.7 * len
  body <- sieve_diagram_cut(
    path, if (heads[1L]) inset else 0,
    total - if (heads[2L]) inset else 0
  )
  graphics::lines(body[, 1L], body[, 2L], col = col, lwd = lwd, lty = lty)
  for (end in which(heads)) {
    tip <- if (end == 1L) path[1L, ] else path[nrow(path), ]
    back <- if (end == 1L) {
      sieve_diagram_cut(path, len, total)[1L, ]
    } else {
      rest <- sieve_diagram_cut(path, 0, total - len)
      rest[nrow(rest), ]
    }
    u <- (tip - back) / max(sqrt(sum((tip - back)^2)), 1e-12)
    n <- c(-u[2L], u[1L])
    barb <- tip - len * u
    notch <- tip - inset * u
    graphics::polygon(c(tip[1L], barb[1L] + wid / 2 * n[1L], notch[1L], barb[1L] - wid / 2 * n[1L]),
      c(tip[2L], barb[2L] + wid / 2 * n[2L], notch[2L], barb[2L] - wid / 2 * n[2L]),
      col = col, border = NA
    )
  }
  invisible(NULL)
}

#' Draw a diagram specification and record what was drawn
#'
#' The work behind [sieve_draw_diagram()] and [sieve_export_diagram()]:
#' it sets the graphics parameters it needs (and restores them), scales the
#' layout to the device, builds the scene, and draws it in an order that
#' keeps labels on top: arcs, straight edges, loops, nodes with their
#' labels, edge labels, caption.
#'
#' @param spec A specification that passed `sieve_diagram_check()`.
#' @param style The resolved style.
#' @return A list with `nodes` and `edges` (data frames, one row per node
#'   and edge drawn, with the label, the line type, the number of
#'   arrowheads, the ends of the path, and the position of the label),
#'   `caption` (the lines drawn), `scale` (device inches per layout unit),
#'   and `xlim` and `ylim` (the user coordinates of the plot region).
#' @author Ken Kelley
#' @noRd
sieve_diagram_render <- function(spec, style) {
  g <- sieve_diagram_geometry()
  k <- sieve_diagram_look()
  caption <- if (style$show_caption) sieve_diagram_figure_caption(spec) else NULL
  canvas <- sieve_diagram_canvas(spec, style$cex)
  paths <- sieve_diagram_paths(spec)
  # Everything this function sets is put back, the coordinate system of
  # the plot included, so the device is left as it was found.
  keep <- graphics::par(c("usr", "xaxp", "yaxp"))
  old <- graphics::par(
    mar = c(0, 0, 0, 0), xpd = NA, cex = 1, family = style$family,
    lheight = 1.05, lend = "butt", ljoin = "mitre"
  )
  on.exit(
    {
      graphics::par(old)
      graphics::par(keep)
    },
    add = TRUE
  )
  graphics::plot.new()
  view <- sieve_diagram_viewport(canvas, graphics::par("pin"), caption, style$cex)
  graphics::plot.window(view$xlim, view$ylim, xaxs = "i", yaxs = "i")
  unit <- view$scale * style$cex / graphics::par("ps")
  scene <- sieve_diagram_scene(spec, style, unit, paths, canvas)
  nodes <- scene$nodes
  lwd <- function(what) max(0.4, k$lwd[[what]] * view$scale)
  pick <- function(field, type) vapply(scene$edges, function(p) p[[field]], type)
  edge_id <- pick("id", "")
  shape_of <- pick("shape", "")

  drawn <- logical(length(scene$edges))
  for (r in c(which(shape_of == "arc"), which(shape_of == "line"), which(shape_of == "loop"))) {
    p <- scene$edges[[r]]
    sieve_diagram_stroke(p$path, p$heads, k$head[[p$type]], style$line, lwd(p$type), p$lty)
    drawn[r] <- TRUE
  }
  turn <- seq(0, 2 * pi, length.out = 97)
  node_drawn <- logical(nrow(nodes))
  for (i in seq_len(nrow(nodes))) {
    if (nodes$shape[i] == "rectangle") {
      graphics::rect(nodes$x[i] - nodes$w[i] / 2, nodes$y[i] - nodes$h[i] / 2,
        nodes$x[i] + nodes$w[i] / 2, nodes$y[i] + nodes$h[i] / 2,
        col = nodes$fill[i], border = nodes$border[i], lwd = lwd(nodes$kind[i])
      )
    } else {
      graphics::polygon(nodes$x[i] + nodes$w[i] / 2 * cos(turn),
        nodes$y[i] + nodes$h[i] / 2 * sin(turn),
        col = nodes$fill[i], border = nodes$border[i], lwd = lwd(nodes$kind[i])
      )
    }
    graphics::text(nodes$x[i], nodes$y[i], nodes$display[[i]],
      cex = nodes$cex[i],
      font = nodes$font[i], col = style$ink
    )
    node_drawn[i] <- TRUE
  }
  for (r in which(nzchar(pick("label", "")))) {
    p <- scene$edges[[r]]
    graphics::rect(p$at[1L] - p$half[1L], p$at[2L] - p$half[2L],
      p$at[1L] + p$half[1L], p$at[2L] + p$half[2L],
      col = "#ffffff",
      border = if (p$flagged) style$flag else NA, lwd = lwd("latent")
    )
    graphics::text(p$at[1L], p$at[2L], p$display, cex = p$cex, font = p$font, col = style$ink)
    if (p$shape == "note") {
      # An annotation line may hold several values of one variable.
      drawn[r] <- TRUE
      drawn[pick("with", "") %in% p$id] <- TRUE
    }
  }
  lines <- view$lines
  if (length(lines)) {
    cap_cex <- k$caption_pt * unit * view$caption_scale
    measure <- sieve_diagram_caption_measure(diff(view$xlim))
    shown <- sieve_diagram_device_text(lines)
    widest <- max(graphics::strwidth(shown, "user", cex = cap_cex))
    if (widest > measure) {
      cap_cex <- cap_cex * measure / widest
      widest <- measure
    }
    lead <- k$caption_pt * style$cex * view$caption_scale * k$caption_lead / 72
    # The block is centered under the figure; its lines are set flush left.
    graphics::text(mean(view$xlim) - widest / 2,
      canvas$ylim[1L] - (seq_along(lines) - 1) * lead, shown,
      adj = c(0, 1), cex = cap_cex, col = style$ink
    )
  }

  # The drawing must be the visible part of the specification: every
  # visible node and edge once, and nothing else.
  wanted <- spec$edges$id[spec$edges$visible]
  if (!identical(edge_id, wanted) || !all(drawn) || !all(node_drawn) ||
    !identical(nodes$id, spec$nodes$id[spec$nodes$visible])) {
    sieve_diagram_abort(sprintf(
      "The drawing does not match the specification (%d of %d edges and %d of %d nodes drawn).",
      sum(drawn), length(wanted), sum(node_drawn), sum(spec$nodes$visible)
    ), "render", spec)
  }
  ends <- function(p, which_end, coord) {
    if (is.null(p$path)) NA_real_ else p$path[if (which_end == 1L) 1L else nrow(p$path), coord]
  }
  list(
    nodes = data.frame(
      id = nodes$id, kind = nodes$kind, shape = nodes$shape, x = nodes$x, y = nodes$y,
      w = nodes$w, h = nodes$h, label = nodes$drawn_label, index = nodes$index,
      pt = nodes$cex / unit, drawn = node_drawn, stringsAsFactors = FALSE
    ),
    edges = data.frame(
      id = edge_id, type = pick("type", ""), from = pick("from", ""), to = pick("to", ""),
      shape = shape_of, heads = vapply(scene$edges, function(p) sum(p$heads), 0),
      line = pick("status", ""), label = pick("label", ""),
      label_text = pick("label_text", ""), flagged = pick("flagged", NA),
      on_line = pick("on_line", NA), curved = pick("curved", NA), cost = pick("cost", 0),
      x0 = vapply(scene$edges, ends, 0, 1L, 1L), y0 = vapply(scene$edges, ends, 0, 1L, 2L),
      x1 = vapply(scene$edges, ends, 0, 2L, 1L), y1 = vapply(scene$edges, ends, 0, 2L, 2L),
      label_x = vapply(scene$edges, function(p) p$at[1L], 0),
      label_y = vapply(scene$edges, function(p) p$at[2L], 0),
      label_w = vapply(scene$edges, function(p) 2 * p$half[1L], 0),
      label_h = vapply(scene$edges, function(p) 2 * p$half[2L], 0),
      drawn = drawn, stringsAsFactors = FALSE
    ),
    caption = lines, scale = view$scale, xlim = view$xlim, ylim = view$ylim,
    canvas = canvas
  )
}

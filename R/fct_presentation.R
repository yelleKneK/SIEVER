#' Display a statistic with its analytic sample and undefined-result reason
#' @param value Full-precision scalar estimate.
#' @param n Number of finite paired observations.
#' @param reason Explanation when an estimate is undefined.
#' @return Escaped, focusable HTML for a table cell.
#' @author Ken Kelley
#' @noRd
sieve_item_rest_cell <- function(value, n, reason = "") {
  reason <- if (is.na(reason)) "" else as.character(reason)
  n <- if (length(n) && !is.na(n)) as.character(n) else "unavailable"
  label <- if (length(value) && is.finite(value)) sprintf("%.3f", value) else "Undefined"
  explanation <- paste0(
    "Pearson item-rest correlation with the available-item mean of the other active items; analytic n = ",
    n,
    if (nzchar(reason)) paste0(". ", reason) else "."
  )
  sprintf(
    '<span class="sieve-explanation" tabindex="0" title="%s" aria-label="%s">%s</span>',
    htmltools::htmlEscape(explanation, attribute = TRUE),
    htmltools::htmlEscape(paste(label, explanation), attribute = TRUE), label
  )
}

#' Item information column definitions, shared with the client accessibility layer
#' @return A named list indexed by semantic column identifier.
#' @author Ken Kelley
#' @noRd
sieve_item_column_definitions <- function() {
  list(
    Item = "Stable variable identifier. The editable item name and wording do not change this identifier.",
    Label = "The researcher's display name. Select this row for the full name and wording.",
    Wording = "Full item wording is available on hover, keyboard focus, and in the selected-item detail pane.",
    Construct = "Researcher-specified construct assignment. Edit in place.",
    Facet = "Facet within this item's parent construct. Equal facet labels in different constructs are distinct.",
    Status = "Included items enter analyses. Excluded items and their raw columns remain available to inspect and restore.",
    item_total = "Pearson correlation of this scored item with the available-item mean of the other active items in its construct. Only finite item-mean pairs enter; focus a cell for analytic n and conditions.",
    facet_item_total = "Pearson correlation of this scored item with the available-item mean of the other active items in its nested facet. The item itself is excluded. Focus a cell for analytic n and undefined-result reasons.",
    loading = "Target loading from the exploratory factor model described above.",
    communality = "Item variance represented by the exploratory factors; an estimate, not a content judgment.",
    rel_drop = "Change in the selected construct reliability coefficient if this item is excluded. A positive change is a diagnostic, not an instruction to remove content.",
    redundancy = "Largest absolute correlation with another included item in the same construct, in the shared correlation matrix (Pearson for continuous items, polychoric or tetrachoric for ordered or binary items; the header names the one in use); high correlation alone does not establish redundancy.",
    Flags = "Heuristic diagnostic thresholds to inspect with content and uncertainty. They do not make exclusion decisions.",
    Reassign = "Exploratory cross-loading pattern to inspect; it does not establish the item's substantive meaning."
  )
}

#' Resolve selected item identifiers without using visual row positions
#' @param ids Stable identifiers reported by the client.
#' @param rows DT original-data row indices, read when `ids` is `NULL`.
#' @param items Item identifiers in the table's original data order.
#' @return Known, unique selected item identifiers.
#' @author Ken Kelley
#' @noRd
sieve_selected_items <- function(ids, rows = NULL, items) {
  if (!is.null(ids)) return(intersect(unique(as.character(ids)), items))
  rows <- suppressWarnings(as.integer(rows))
  rows <- rows[!is.na(rows) & rows >= 1L & rows <= length(items)]
  unique(items[rows])
}

#' Stable per-table state callbacks for recreated DT widgets
#' @return A JavaScript callback; sort, filter, page, and scroll stay in this session.
#' @author Ken Kelley
#' @noRd
sieve_dt_state_save <- function() {
  DT::JS("function(settings,data){if(window.sieveTableStateSave)window.sieveTableStateSave(settings,data);}")
}

#' @rdname sieve_dt_state_save
#' @author Ken Kelley
#' @noRd
sieve_dt_state_load <- function() {
  DT::JS("function(settings){return window.sieveTableStateLoad?window.sieveTableStateLoad(settings):null;}")
}

#' Build an Interactive Table
#'
#' The one entry to `DT::datatable()` for the application's tables. It adds no
#' column definition of its own. A table with a column no sort can order (a
#' sparkline, an in-cell control, two values joined in one cell) turns that
#' column's ordering off in its own `columnDefs`, and a column of numbers or
#' text in markup, such as the emphasized loadings of the exploratory tables,
#' stays sortable.
#'
#' It sends no definition that makes every column with a cell beginning
#' with an HTML tag unsortable: such a definition would take the sort from
#' the loadings. A table's own `columnDefs` give their targets as numbers,
#' not as `which()` of a logical named by column: shiny and htmlwidgets
#' write a named vector as a JSON object, which DataTables does not read as
#' column targets (it reads a number, a string or an array), and jsonlite
#' prints a message about the named vector each time such a table is drawn.
#' @param data Table data.
#' @param ... Arguments passed to `DT::datatable()`.
#' @return A DT widget.
#' @author Ken Kelley
#' @noRd
sieve_dt <- function(data, ...) {
  do.call(DT::datatable, c(list(data = data), list(...)))
}

#' Format a Display Number Without Changing Stored Values
#' @param x Numeric values.
#' @param digits Decimal places.
#' @return Display strings.
#' @author Ken Kelley
#' @noRd
sieve_fmt_num <- function(x, digits = 3L) {
  ifelse(is.finite(x), sprintf(paste0("%.", digits, "f"), x), "n/a")
}

#' Format an Estimate and Its Interval in a Value Box
#' @param est,lo,hi Estimate and confidence limits.
#' @param reason Reason an estimate is unavailable.
#' @return HTML display text.
#' @author Ken Kelley
#' @noRd
sieve_fmt_ci <- function(est, lo, hi, reason = "df = 0 (just identified)") {
  if (!is.finite(est)) return(htmltools::HTML(sprintf("n/a<br><span style='font-size:0.7em'>%s</span>", htmltools::htmlEscape(reason))))
  ci <- if (is.finite(lo) && is.finite(hi)) sprintf("[%.3f, %.3f]", lo, hi) else "CI n/a"
  htmltools::HTML(sprintf("%.3f<br><span style='font-size:0.7em'>%s</span>", est, ci))
}

#' Shared Confidence Level Control
#' @param id Input ID.
#' @param label Visible label.
#' @param value,min,max,step,width Existing input settings.
#' @return A Shiny numeric input.
#' @author Ken Kelley
#' @noRd
sieve_conf_level_input <- function(id, label = "Confidence Level", value = .95,
                                   min = .8, max = .99, step = .01, width = NULL) {
  shiny::numericInput(id, label, value = value, min = min, max = max, step = step, width = width)
}

#' Open a Figure Device With a Shared Failure Check
#' @param format File format.
#' @param file Output file.
#' @param width,height Size in inches.
#' @param dpi PNG resolution.
#' @param title PDF title.
#' @return Open device number and actual device name.
#' @author Ken Kelley
#' @noRd
sieve_open_device <- function(format, file, width, height, dpi = 300, title = "SIEVER figure") {
  name <- if (identical(format, "svg") && sieve_have("svglite")) "svglite" else format
  had <- grDevices::dev.list()
  tried <- sieve_capture(switch(name,
    pdf = grDevices::pdf(file, width = width, height = height, bg = "white", useDingbats = FALSE, title = title),
    png = grDevices::png(file, width = width, height = height, units = "in", res = dpi, bg = "white"),
    svg = grDevices::svg(file, width = width, height = height, bg = "white"),
    svglite = svglite::svglite(file, width = width, height = height, bg = "white"),
    stop("The format must be png, pdf, or svg.", call. = FALSE)))
  opened <- setdiff(grDevices::dev.list(), had)
  if (!tried$ok || length(opened) != 1L) {
    for (device in opened) grDevices::dev.off(device)
    why <- c(tried$error, tried$warnings)
    stop(sprintf("The %s graphics device could not be opened%s%s", name,
      if (length(why)) paste0(": ", paste(why, collapse = " ")) else ".",
      if (identical(name, "svg")) " Installing svglite provides an SVG device that does not need cairo." else ""), call. = FALSE)
  }
  for (message_text in tried$warnings) warning(message_text, call. = FALSE)
  list(number = opened, name = name)
}

#' A closed settings disclosure with a live summary of its inputs
#' @param title Short settings title.
#' @param ... Settings controls and their explanations.
#' @param ids Fully namespaced input ids whose values appear in the summary.
#' @return A details tag, closed by default.
#' @author Ken Kelley
#' @noRd
sieve_settings <- function(title, ..., ids = character(0)) {
  shiny::tags$details(class = "sieve-settings mb-2",
    `data-sieve-settings` = paste(ids, collapse = " "),
    shiny::tags$summary(title, shiny::tags$span(class = "sieve-setting-values small")),
    ...
  )
}

#' Shared Replication Count and Optional Seed Controls
#' @param ns Module namespace.
#' @param B Starting replication count.
#' @param B_id,seed_id The input ids, within the namespace, of the count and
#'   the seed.
#' @param min_B,max_B,step_B Bounds and step for the replication count.
#' @param label,seed_label Control labels.
#' @param width,seed_width Control widths.
#' @param seed_min Optional seed input lower bound.
#' @return A tag list.
#' @author Ken Kelley
#' @noRd
sieve_boot_inputs <- function(ns, B = 1000L, B_id = "B", seed_id = "seed",
                               min_B = 100L, max_B = NULL, step_B = 100L,
                               label = "Bootstrap Replications", seed_label = "Seed (Optional)",
                               width = "180px", seed_width = "140px", seed_min = NULL) {
  shiny::tagList(
    shiny::numericInput(ns(B_id), label, value = B, min = min_B, max = max_B %||% NA_real_, step = step_B, width = width),
    shiny::numericInput(ns(seed_id), seed_label, value = NULL, min = seed_min %||% NA_real_, step = 1, width = seed_width)
  )
}

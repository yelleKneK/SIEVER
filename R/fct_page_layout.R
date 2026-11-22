# fct_page_layout.R
# Results first. On every page, from the top of the main area: the page guide
# line, at most one status line, a one-line method statement for the primary
# result, the result, and below it the explanation, whose first paragraph
# stays visible and whose rest is in a labeled disclosure open by default. The
# method statement is read from the fitted object (sieve_method_line()), so it
# cannot disagree with the model; sieve_more_below() lays out the explanation
# under a result. The method statement is one headline; the fit, the
# missing-data diagnostics and the uncertainty are in a labeled disclosure
# under it, apart from the explanation, which stays open.

#' The one-line method statement above a fitted model's result
#'
#' What produced the numbers below it, read from the fitted object itself
#' so it cannot disagree with the model: the model, the engine, the
#' estimator and its treatment of incomplete cases, what was analyzed
#' (covariances, or polychoric correlations for ordered items), the
#' identification, the counts of items and factors, and the cases used out
#' of the rows of the data.
#'
#' @param fit A fitted lavaan object, or `NULL`.
#' @param what The model in words ("Correlated-factors CFA").
#' @param identification How the latent scale is set, in words, or `NULL`.
#' @param extra Further words after the counts (for example the observed
#'   outcomes of a structural model), or `NULL`.
#' @param measures A result of `sieve_fit_measures()`, to add CFI, RMSEA
#'   with its interval (labeled with the level it was computed at), and
#'   SRMR (with the unbiased SRMR and its interval when the measures hold
#'   them), each named with its family; `NULL` for none.
#' @param notes Sentences said after the statement, such as what the page
#'   left out of the fit (`sieve_auxiliary_unused_note()`); `NULL` for none.
#' @return A [shiny::div()], or `NULL` when `fit` is not a lavaan object.
#' @author Ken Kelley
#' @noRd
sieve_method_line <- function(fit, what, identification = "latent variances fixed at 1",
                              extra = NULL, measures = NULL, notes = NULL) {
  parts <- sieve_method_parts(fit, what, identification = identification, extra = extra, measures = measures,
    notes = notes)
  if (is.null(parts)) {
    return(NULL)
  }
  shiny::div(class = "small text-muted mb-2 sieve-method-line",
    shiny::span(class = "sieve-method-headline", parts$headline),
    shiny::tags$details(
      class = "sieve-provenance", `data-sieve-remember` = "method-line",
      shiny::tags$summary("Method and data behind this result"),
      shiny::tags$div(parts$text),
      shiny::tags$div(class = "sieve-inference-line", paste0("Uncertainty: ", sieve_inference_text(sieve_fit_inference(fit)), "."))
    )
  )
}

#' The words of the one-line method statement
#'
#' @inheritParams sieve_method_line
#' @return One string, or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_method_text <- function(fit, what, identification = "latent variances fixed at 1",
                              extra = NULL, measures = NULL, notes = NULL) {
  sieve_method_parts(fit, what, identification = identification, extra = extra, measures = measures,
    notes = notes)$text
}

#' The method statement as a headline and its full text
#'
#' The headline names the model and the engine, the estimator and its
#' treatment of incomplete cases, the counts of observed variables and
#' factors, and n of the rows: what a reader needs to know which numbers
#' are below, in one line. The full text adds what was analyzed, the
#' identification, any further words, the fit, the missing-data
#' diagnostics, and any notes.
#'
#' @inheritParams sieve_method_line
#' @return A list with `headline` and `text` (strings), or `NULL` when
#'   `fit` is not a lavaan object.
#' @author Ken Kelley
#' @noRd
sieve_method_parts <- function(fit, what, identification = "latent variances fixed at 1",
                               extra = NULL, measures = NULL, notes = NULL) {
  if (!inherits(fit, "lavaan")) {
    return(NULL)
  }
  read <- function(what_) tryCatch(lavaan::lavInspect(fit, what_), error = function(e) NULL)
  options <- read("options")
  estimator <- options$estimator %||% "ML"
  test <- as.character(options$test %||% "")
  if (identical(estimator, "ML") && any(test %in% c("yuan.bentler.mplus", "yuan.bentler")) &&
    identical(as.character(options$se %||% ""), "robust.huber.white")) {
    estimator <- "MLR"
  }
  if (identical(estimator, "DWLS") && any(test %in% c("scaled.shifted", "mean.var.adjusted"))) {
    estimator <- "WLSMV"
  }
  missing <- as.character(options$missing %||% "listwise")
  cases <- switch(missing,
    ml = ,
    ml.x = "with FIML",
    two.stage = ,
    robust.two.stage = "from available-data EM moments with two-stage correction",
    listwise = "on the complete cases",
    pairwise = "on the pairs present",
    sprintf("(missing = %s)", missing)
  )
  ordered <- tryCatch(lavaan::lavNames(fit, "ov.ord"), error = function(e) character(0))
  observed <- tryCatch(lavaan::lavNames(fit, "ov"), error = function(e) character(0))
  # lavaan lists an auxiliary variable among the observed variables; it is
  # counted on its own, as the saturated correlate it is.
  aux <- intersect(as.character(options$aux %||% character(0)), observed)
  observed <- setdiff(observed, aux)
  latent <- tryCatch(lavaan::lavNames(fit, "lv"), error = function(e) character(0))
  analyzed <- if (!length(ordered)) {
    "covariances"
  } else if (length(ordered) == length(observed)) {
    "polychoric correlations"
  } else {
    "polychoric and Pearson correlations"
  }
  n_used <- sum(as.numeric(read("nobs") %||% NA_real_))
  n_rows <- sum(as.numeric(read("norig") %||% NA_real_))
  groups <- as.integer(read("ngroups") %||% 1L)
  counts <- sprintf(
    "%d observed variable%s%s, %d factor%s%s", length(observed), if (length(observed) == 1L) "" else "s",
    if (length(aux)) sprintf(" and %d auxiliary variable%s as %s (%s)", length(aux),
      if (length(aux) == 1L) "" else "s", if (length(aux) == 1L) "a saturated correlate" else "saturated correlates",
      paste(aux, collapse = ", ")) else "",
    length(latent), if (length(latent) == 1L) "" else "s",
    if (groups > 1L) sprintf(", %d groups", groups) else ""
  )
  fit_words <- if (!is.null(measures) && length(measures)) {
    m <- measures
    num <- function(x) if (length(x) == 1L && is.finite(x)) sprintf("%.3f", x) else "n/a"
    unbiased <- sieve_usrmr_words(m, num = num)
    sprintf(
      " Fit: %s %s, %s %s, %s [%s, %s], SRMR %s%s.",
      sieve_fit_label(m, "cfi"), num(m[["cfi"]]), sieve_fit_label(m, "rmsea"), num(m[["rmsea"]]),
      sieve_rmsea_ci_words(sieve_fit_ci_level(m)), num(m[["rmsea_lo"]]), num(m[["rmsea_hi"]]), num(m[["srmr"]]),
      if (is.null(unbiased)) "" else paste0(" (", unbiased, ")")
    )
  }
  n_words <- paste0(
    "n = ", if (is.finite(n_used)) format(n_used, big.mark = ",") else "n/a",
    if (is.finite(n_rows) && !identical(n_rows, n_used)) sprintf(" of %s rows", format(n_rows, big.mark = ",")) else ""
  )
  list(
    headline = paste0(what, " in lavaan: ", estimator, " ", cases, "; ", counts, "; ", n_words, "."),
    text = paste0(
      what, " in lavaan: ", estimator, " ", cases, " on the ", analyzed,
      if (!is.null(identification)) paste0(", ", identification), "; ",
      counts, if (!is.null(extra)) paste0(", ", extra), "; ", n_words,
      ".", fit_words,
      if (!is.null(attr(fit, "sieve_missing_data", exact = TRUE))) paste0(" ",
        sieve_missing_line(attr(fit, "sieve_missing_data", exact = TRUE), n_used, cases)),
      # What the page left out of the fit and why: the declared auxiliary
      # variables, when every analyzed value is observed
      # (sieve_auxiliary_unused_note()).
      if (length(notes)) paste0(" ", paste(notes, collapse = " "))
    )
  )
}

#' The Explanation Below a Result, Open by Default
#'
#' Explanations are visible by default. Below a result the first paragraph
#' stays visible and the rest is in a labeled disclosure, open until the
#' researcher closes it.
#'
#' @param first The first paragraph (a tag or text).
#' @param ... The rest.
#' @param label The label of the disclosure.
#' @return A [shiny::tagList()].
#' @author Ken Kelley
#' @noRd
sieve_more_below <- function(first, ..., label = "More on how to read this") {
  rest <- list(...)
  rest <- Filter(Negate(is.null), rest)
  shiny::tagList(
    shiny::div(class = "small text-muted mt-2 sieve-below-first", first),
    if (length(rest)) {
      shiny::tags$details(
        class = "sieve-intro small text-muted sieve-below-more",
        open = NA,
        shiny::tags$summary(label),
        rest
      )
    }
  )
}

#' The context line of a page's first result: sample, estimand, uncertainty
#'
#' One line, the same on every page that has no fitted-model method line
#' (`sieve_method_line()`), naming the rows and items the result below it
#' comes from, what it estimates, and how its uncertainty is expressed. Each
#' part is labeled, and a part given as `NULL` is left out.
#'
#' @param sample The rows and items, in words.
#' @param estimand What the result estimates, in words.
#' @param uncertainty How its uncertainty is expressed, in words.
#' @return A [shiny::div()] of class `sieve-context-line`.
#' @author Ken Kelley
#' @noRd
sieve_context_line <- function(sample = NULL, estimand = NULL, uncertainty = NULL) {
  part <- function(label, text) {
    if (is.null(text) || !length(text) || !nzchar(text[1L])) return(NULL)
    shiny::tags$span(class = "me-2", shiny::tags$b(paste0(label, ": ")), sub("([^.])$", "\\1.", text[1L]))
  }
  shiny::div(class = "small text-muted mb-2 sieve-context-line",
    part("Sample", sample), part("Estimand", estimand), part("Uncertainty", uncertainty))
}

#' A page's status line: one headline, then any notes
#'
#' The status line of the results-first contract: a sentence in normal text
#' with a colored left border, announced to screen readers
#' (`role = "status"`), and under it the notes that are present, one per item.
#' It stays short: the run policy and the tab paths are in the page guide and
#' the Ingest sidebar, not in the status line. The headline is held to
#' `sieve_status_line_budget()` characters by a test.
#'
#' @param headline The headline: what the page holds and the next step.
#' @param notes A character vector of further notes, each shown as an item
#'   of a list under the headline; empty strings and `NULL` are dropped.
#' @param tone `"success"`, `"warning"`, `"danger"` or `"neutral"`, which
#'   colors the left border.
#' @param id An element id, or `NULL`.
#' @return A [shiny::div()] of class `sieve-status-line`.
#' @author Ken Kelley
#' @noRd
sieve_status_line <- function(headline, notes = NULL, tone = c("neutral", "success", "warning", "danger"), id = NULL) {
  tone <- match.arg(tone)
  notes <- as.character(unlist(notes))
  notes <- notes[!is.na(notes) & nzchar(trimws(notes))]
  shiny::div(
    id = id, class = paste0("sieve-status-line sieve-status-", tone), role = "status",
    shiny::span(class = "sieve-status-headline", headline),
    if (length(notes)) shiny::tags$ul(class = "sieve-status-notes mb-0", lapply(notes, shiny::tags$li))
  )
}

#' The longest headline a status line may have
#'
#' @return The number of characters, 300: room for the counts, the
#'   estimator and the next step, and not for paragraphs, which belong in
#'   the notes under the headline.
#' @author Ken Kelley
#' @noRd
sieve_status_line_budget <- function() 300L

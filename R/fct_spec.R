# fct_spec.R
# The resolved analysis specification and the analysis record. An analysis
# is described once, by a specification that holds every choice the engine
# needs (model, estimator, ordered variables, missing data treatment,
# scaling). The fit, the R recorded for replay, the status shown to the
# user, and the report all read that one object, so they cannot disagree
# about what was done.

#' The observed variables a lavaan model syntax names
#'
#' @param syntax lavaan model syntax.
#' @return A character vector of observed variable names, in model order,
#'   or `character(0)` when the syntax is empty or cannot be parsed.
#' @author Ken Kelley
#' @noRd
sieve_model_observed <- function(syntax) {
  if (!nzchar(syntax)) {
    return(character(0))
  }
  tryCatch(
    lavaan::lavNames(lavaan::lavaanify(syntax, auto = FALSE, warn = FALSE), "ov"),
    error = function(e) character(0)
  )
}

#' The latent variables a lavaan model syntax defines
#'
#' @param syntax lavaan model syntax.
#' @return A character vector of latent variable names, in model order.
#' @author Ken Kelley
#' @noRd
sieve_model_latent <- function(syntax) {
  if (!nzchar(syntax)) {
    return(character(0))
  }
  tryCatch(
    lavaan::lavNames(lavaan::lavaanify(syntax, auto = FALSE, warn = FALSE), "lv"),
    error = function(e) character(0)
  )
}

#' Resolve the Specification of a Confirmatory Factor Analysis
#'
#' Turns the ledger and the user's model choices into the complete
#' specification of one lavaan CFA. The estimator and the missing data
#' treatment follow the measurement level of the indicators that are
#' actually in the model: maximum likelihood (ML) with full information
#' maximum likelihood when every modeled indicator is continuous (DMAR's
#' default), and WLSMV on polychoric (or tetrachoric) correlations with
#' listwise deletion when any modeled indicator is ordered or binary (lavaan
#' does not offer full information maximum likelihood for that estimator).
#' Robust maximum likelihood (MLR) with full information maximum likelihood,
#' the choice for markedly non-normal continuous items, is
#' `estimator = "mlr"`.
#'
#' The specification states what the model is meant to contain: `observed`
#' is the intended set of indicators and `latent` the latent variables,
#' both taken from the builder rather than read back from the syntax, so the
#' fit can be checked against them (see [sieve_run_model()]). `name_map`
#' gives the researcher's label for each latent name, and `not_modeled`
#' lists every construct, facet, or item the model leaves out, with the
#' reason.
#'
#' @param meta The item ledger.
#' @param structure `"first_order"` (correlated constructs),
#'   `"higher_order"` (construct to facet to items), or
#'   `"facet_first_order"` (direct, correlated facet factors).
#' @param facet_ids Nested facet identities for direct facet CFA; NULL selects all.
#' @param syntax Optional lavaan model syntax that overrides the syntax built
#'   from the ledger (for an expert or a derived model). Its observed and
#'   latent variables are then read from the syntax itself.
#' @param std_lv `TRUE` fixes each latent variance to one, so every loading
#'   is free; `FALSE` uses a marker indicator, fixing the first loading of
#'   each factor to one and freeing its variance. Either choice sets the
#'   scale of the latent variables; neither standardizes the indicators.
#' @param orthogonal `TRUE` fixes the factor covariances to zero.
#' @param reserved Further observed variable names the latent names must
#'   avoid: the columns of the data the model will be fit to. Every ledger
#'   item is always avoided.
#' @param min_items Minimum included items for a factor to be written (see
#'   `sieve_cfa_plan()`).
#' @param estimator One of `"auto"`, which follows the item types (ML
#'   with FIML when every item is continuous, WLSMV when any is ordered or
#'   binary), `"mlr"` (MLR with FIML), `"ml_fiml"` (ML with FIML), `"ml"`
#'   (ML on the complete cases) or `"wlsmv"` (WLSMV, every item treated as
#'   ordered); `"auto"` is the default, and the others are the researcher's choice
#'   on the CFA tab.
#' @return A list of class `sieve_spec` with `kind`, `structure`, `fun`,
#'   `model`, `observed`, `latent`, `estimator`, `ordered`, `missing`,
#'   `std_lv`, `orthogonal`, `name_map`, `not_modeled`, `problems`,
#'   `key` (a content hash of the specification), and `estimator_choice`.
#'   A construct cell that is blank, missing, or `Unassigned` means the
#'   item has no construct; such items are left out of every construct
#'   model, reliability analysis, score and norm, and are listed in
#'   `not_modeled` with the reason "no construct is assigned".
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' spec <- sieve_cfa_spec(meta, reserved = names(items))
#' cat(spec$model)
#' spec$estimator
#' spec$missing
#' @export
sieve_cfa_spec <- function(meta, structure = c("first_order", "higher_order", "facet_first_order"),
                           syntax = NULL, std_lv = TRUE, orthogonal = FALSE,
                           reserved = NULL, min_items = 2L, estimator = "auto", facet_ids = NULL) {
  structure <- match.arg(structure)
  plan <- sieve_cfa_plan(meta, structure, min_items = min_items, reserved = reserved, facet_ids = facet_ids)
  if (!is.null(syntax)) {
    latent <- sieve_model_latent(syntax)
    plan <- list(
      structure = structure, model = syntax,
      observed = sieve_model_observed(syntax), latent = latent,
      name_map = stats::setNames(sieve_show_names(latent, plan$name_map), latent),
      not_modeled = sieve_not_modeled()
    )
  }
  sieve_model_spec(plan, meta,
    kind = "cfa", fun = "cfa", std_lv = std_lv,
    orthogonal = orthogonal, estimator = estimator
  )
}

#' The model the CFA page's controls specify
#'
#' The one mapping from the CFA page's controls, or their saved copy
#' `state$cfa_settings`, to the model they specify. The CFA page reads it,
#' and so does the validation reservation's frozen version
#' (`sieve_holdout_spec()`), so the version evaluated on the reserved rows
#' is the model the page fits, not the default first-order model whatever
#' the page chose.
#'
#' @param settings A list with `scope` (`"construct"` or `"facet"`),
#'   `structure` (`"first"` or `"higher"`), `ident` (`"varone"` or
#'   `"marker"`), `facets` and `all_facets`, as `state$cfa_settings` holds
#'   them; `NULL` gives the page's defaults.
#' @return A list with `structure` (`"first_order"`, `"higher_order"` or
#'   `"facet_first_order"`), `facet_ids` (the facets of a direct facet
#'   model restricted to some, otherwise `NULL`) and `ident` (`"varone"` or
#'   `"marker"`).
#' @author Ken Kelley
#' @noRd
sieve_cfa_choices <- function(settings = NULL) {
  s <- if (is.list(settings)) settings else list()
  list(
    structure = if (identical(s$scope, "facet")) "facet_first_order" else {
      if (identical(s$structure, "higher")) "higher_order" else "first_order"
    },
    facet_ids = if (identical(s$scope, "facet") && isFALSE(s$all_facets)) s$facets %||% character(0) else NULL,
    ident = if (identical(s$ident, "marker")) "marker" else "varone"
  )
}

#' The model a set of CFA choices specifies, in words
#'
#' @param choices A result of `sieve_cfa_choices()`.
#' @return One phrase, such as `"correlated-factors model, factor variances
#'   fixed at 1"`.
#' @author Ken Kelley
#' @noRd
sieve_cfa_choices_text <- function(choices) {
  model <- switch(choices$structure %||% "first_order",
    higher_order = "higher-order model",
    facet_first_order = if (length(choices$facet_ids)) {
      sprintf("direct correlated-facet model of %d chosen facet%s", length(choices$facet_ids),
        if (length(choices$facet_ids) == 1L) "" else "s")
    } else "direct correlated-facet model",
    "correlated-factors model"
  )
  paste0(model, ", ", if (identical(choices$ident, "marker")) "marker indicators fixed at 1" else "factor variances fixed at 1")
}

#' The estimator choices a model fit offers
#'
#' The control on the CFA tab (full information maximum likelihood when the
#' sample is not small, and the estimator a choice). `"auto"` follows the
#' confirmed item types: ML with FIML when every item is continuous, which
#' is DMAR's default, with the other estimators offered at the user's
#' request, and WLSMV when any item is ordered or binary. The four others
#' name an estimator and the treatment of the items it implies, whatever
#' the types say. `"mlr"` (MLR with FIML) is the choice for markedly
#' non-normal continuous items: the same point estimates, with Huber-White
#' standard errors and a scaled test statistic; SIEVER points to it and
#' never switches to it on its own. `"ml_fiml"` (ML with FIML) is what
#' `"auto"` resolves to for continuous items, offered by name so a study
#' can keep it whatever the types say, and the way forward the refusal of
#' MLR's rank deficient sandwich names (`sieve_mlr_guard()`).
#'
#' @return A named character vector: labels as names, choice codes as
#'   values.
#' @author Ken Kelley
#' @noRd
sieve_estimator_choices <- function() {
  c(
    "Follow the item types: ML with FIML (DMAR's default) when every item is continuous, WLSMV when any is ordered or binary" = "auto",
    "MLR with FIML, every item treated as continuous (robust standard errors and scaled test, for markedly non-normal items)" = "mlr",
    "ML with FIML, every item treated as continuous" = "ml_fiml",
    "ML on the complete cases, every item treated as continuous" = "ml",
    "WLSMV on polychoric correlations, every item treated as ordered" = "wlsmv"
  )
}

#' The choices of a tab's Estimator control
#'
#' The study's estimator is chosen once, on the Ingest tab (step 4), and
#' every tab that fits a model offers the same control with one more
#' choice first: `"study"`, which follows the study's setting. A tab set to
#' another choice fits its own models under that choice and says so.
#'
#' @param study Whether to offer `"study"` first (every tab but the Ingest
#'   tab, which is where the study's setting is made).
#' @return A named character vector: short labels as names, choice codes
#'   as values.
#' @author Ken Kelley
#' @noRd
sieve_estimator_select_choices <- function(study = TRUE) {
  own <- c(
    "Follow the item types (ML with FIML if every item is continuous, WLSMV if any is ordered)" = "auto",
    "MLR with FIML (items treated as continuous; for markedly non-normal items)" = "mlr",
    "ML with FIML (items treated as continuous)" = "ml_fiml",
    "ML on the complete cases (items treated as continuous)" = "ml",
    "WLSMV on polychoric correlations (items treated as ordered)" = "wlsmv"
  )
  if (isTRUE(study)) c("The study's estimator (set on the Ingest tab)" = "study", own) else own
}

#' The estimator choice in force on a tab
#'
#' Maps a tab's Estimator control to one of `sieve_estimator_choices()`:
#' `"study"` (or nothing) reads the study's setting, `state$estimator_choice`
#' (made on the Ingest tab), and any other choice is the tab's own. Read
#' reactively, so a reactive that calls this is evaluated again when the
#' study's setting changes.
#'
#' @param choice The value of the tab's control, or `NULL`.
#' @param state The shared state store (or a list with `estimator_choice`).
#' @return One of `"auto"`, `"mlr"`, `"ml_fiml"`, `"ml"`, `"wlsmv"`.
#' @author Ken Kelley
#' @noRd
sieve_estimator_effective <- function(choice, state = NULL) {
  valid <- unname(sieve_estimator_choices())
  own <- length(choice) == 1L && !is.na(choice) && choice %in% valid
  if (own) {
    return(choice)
  }
  study <- if (is.null(state)) NULL else state$estimator_choice
  if (length(study) == 1L && !is.na(study) && study %in% valid) study else "auto"
}

#' The estimator choice a recorded SIEVER call names
#'
#' Recorded R reproduces what ran, with every argument explicit. `"auto"`
#' (Follow the item types) is resolved by the rule of the SIEVER that runs
#' the script, and for continuous items that rule is a default (ML with
#' FIML, DMAR's) that a version of SIEVER may set differently. A SIEVER call
#' recorded for items that are all continuous therefore names `"ml_fiml"`,
#' which gives the same specification as `"auto"` and does not depend on
#' that default. `"auto"` with an ordered or binary item is kept: it
#' resolves to WLSMV on those items with the others continuous, which no
#' named choice expresses.
#'
#' @param choice The estimator choice in force (one of
#'   `sieve_estimator_choices()`).
#' @param meta The item ledger whose included items the call models.
#' @return One of `sieve_estimator_choices()`.
#' @author Ken Kelley
#' @noRd
sieve_estimator_record_choice <- function(choice, meta) {
  choice <- sieve_estimator_effective(choice)
  if (!identical(choice, "auto") || !is.data.frame(meta) || !all(c("type", "status") %in% names(meta))) {
    return(choice)
  }
  resolved <- sieve_estimator_resolve(as.character(meta$type[meta$status == "active"]), choice = "auto")
  if (identical(resolved$estimator, "ML") && identical(resolved$missing, "fiml")) "ml_fiml" else "auto"
}

#' A tab's Estimator control
#'
#' The one control every model tab uses (see
#' `sieve_estimator_select_choices()`), so the choice reads the same way
#' everywhere.
#'
#' @param id The input id (namespaced by the caller).
#' @param label The label shown above the control.
#' @param selected The initial choice.
#' @param study Whether to offer the study's setting first.
#' @return A [shiny::selectInput()].
#' @author Ken Kelley
#' @noRd
sieve_estimator_input <- function(id, label = "Estimator", selected = "study", study = TRUE) {
  shiny::selectInput(id, label,
    choices = sieve_estimator_select_choices(study = study),
    selected = selected, width = "100%"
  )
}

#' A model tab's Estimator control with its sentence
#'
#' The control (see `sieve_estimator_input()`) and, under it, the output
#' `estimator_src` that `sieve_estimator_wire()` fills with the estimator in
#' force and where it was set.
#'
#' @param ns The module's namespace function.
#' @param label The label of the control.
#' @return A [shiny::tagList()].
#' @author Ken Kelley
#' @noRd
sieve_estimator_panel <- function(ns, label = "Estimator") {
  shiny::tags$details(
    class = "sieve-settings mb-2",
    shiny::tags$summary(shiny::uiOutput(ns("estimator_src"), inline = TRUE)),
    sieve_estimator_input(ns("estimator"), label)
  )
}

#' The server side of a model tab's Estimator control
#'
#' Fills `output$estimator_src` and returns the choice in force as a
#' reactive (see `sieve_estimator_effective()`), which the tab passes to
#' every specification it builds and to the key its on-request fits are
#' kept under.
#'
#' @param input,output The module's input and output.
#' @param state The shared state store.
#' @param detail Optional function returning the page-specific method detail.
#' @param output_id Output receiving the statement.
#' @return A reactive returning one of `"auto"`, `"mlr"`, `"ml_fiml"`, `"ml"`,
#'   `"wlsmv"`.
#' @author Ken Kelley
#' @noRd
sieve_estimator_wire <- function(input, output, state, detail = NULL, output_id = "estimator_src") {
  output[[output_id]] <- shiny::renderUI({
    own <- input$estimator %||% "study"
    override <- own != "study" &&
      !identical(sieve_estimator_effective(own, state), sieve_estimator_effective("study", state))
    shiny::span(class = "small", `data-sieve-estimator-override` = if (override) own else "",
      sieve_estimator_source_text(input$estimator, state),
      if (is.function(detail)) detail())
  })
  shiny::reactive(sieve_estimator_effective(input$estimator, state))
}

#' Where the estimator in force on a tab comes from, in words
#'
#' @param choice The value of the tab's control.
#' @param state The shared state store.
#' @return One sentence naming the estimator in force and where it was set.
#' @author Ken Kelley
#' @noRd
sieve_estimator_source_text <- function(choice, state = NULL) {
  effective <- sieve_estimator_effective(choice, state)
  words <- c(
    auto = "the estimator the item types imply", mlr = "MLR with FIML (items as continuous)",
    ml_fiml = "ML with FIML (items as continuous)",
    ml = "ML on the complete cases (items as continuous)",
    wlsmv = "WLSMV on polychoric correlations (items as ordered)"
  )
  own <- length(choice) == 1L && !is.na(choice) && choice %in% unname(sieve_estimator_choices())
  sprintf(
    "In force here: %s, %s.", unname(words[effective]),
    if (own) "chosen on this tab" else "the study's setting on the Ingest tab"
  )
}

#' Resolve the estimator, the missing data treatment and the item treatment
#'
#' The one rule behind every model SIEVER fits and every sentence that
#' states the estimator (the Ingest tab, the CFA tab, the recorded R). With
#' `choice = "auto"` the resolution follows the confirmed measurement
#' levels: maximum likelihood (ML) with full information maximum
#' likelihood (FIML) on Pearson covariances when every item is continuous,
#' which is DMAR's default, and WLSMV (diagonally
#' weighted least squares with a mean and variance adjusted test
#' statistic) on polychoric or tetrachoric correlations, on the complete
#' cases, when any item is ordered or binary (lavaan has no FIML for that
#' estimator). The four other choices override the types: `"mlr"`,
#' `"ml_fiml"` and `"ml"` treat every item as continuous (MLR with FIML for
#' `"mlr"`, whose Huber-White standard errors and scaled test statistic
#' suit markedly non-normal items; ML with FIML for `"ml_fiml"`, the
#' resolution of `"auto"` for continuous items; ML on the complete cases
#' for `"ml"`), and `"wlsmv"` treats every item as ordered. The reason
#' states the rule applied, names the alternative and the sample size; it
#' points to MLR with FIML for markedly non-normal items, a choice the
#' researcher makes, never an automatic switch. Sample size never changes
#' the treatment of missing responses.
#'
#' @param types The measurement level of each variable in the model
#'   (`"continuous"`, `"ordered"`, `"binary"`); `NA` for a variable that is
#'   not a ledger item (an external outcome), which is never treated as
#'   ordered.
#' @param n The number of rows of the data, or `NA`.
#' @param choice One of `sieve_estimator_choices()`.
#' @return A list with `choice`, `estimator` (`"MLR"`, `"ML"` or
#'   `"WLSMV"`), `missing` (`"fiml"` for `"MLR"`, for `"ml_fiml"` and for
#'   `"auto"` with every item continuous, otherwise `"listwise"`),
#'   `categorical` (a
#'   logical as long as `types`: which variables are treated as ordered),
#'   `basis` (the correlations the models work from, in words), `label`
#'   (the estimator in words, such as `"MLR with FIML"` or `"ML with
#'   FIML"`), `reason` (one or
#'   two sentences), `small_n` (`TRUE` when `n` is 100 or fewer), and
#'   `n_categorical` (how many variables are typed ordered or binary).
#' @author Ken Kelley
#' @noRd
sieve_estimator_resolve <- function(types, n = NA_integer_, choice = "auto") {
  choice <- match.arg(choice, unname(sieve_estimator_choices()))
  types <- as.character(types)
  known <- !is.na(types) & types %in% c("continuous", "ordered", "binary")
  typed_categorical <- known & types %in% c("ordered", "binary")
  n_cat <- sum(typed_categorical)
  n_items <- sum(known)
  n <- suppressWarnings(as.integer(n))
  n_known <- length(n) == 1L && is.finite(n)
  small_n <- n_known && n <= 100L
  if (!n_items) return(list(choice = choice, estimator = "ML", missing = "listwise",
    categorical = rep(FALSE, length(types)), basis = "Pearson", label = "ML",
    reason = "No items classified yet.", small_n = small_n, n_categorical = 0L))
  n_text <- if (n_known) sprintf(" (n = %s)", format(n, big.mark = ",")) else ""
  binary_all <- n_cat > 0L && all(types[known] == "binary")
  categorical <- switch(choice,
    auto = typed_categorical,
    wlsmv = known,
    mlr = ,
    ml_fiml = ,
    ml = rep(FALSE, length(types))
  )
  categorical[is.na(categorical)] <- FALSE
  any_cat <- any(categorical)
  estimator <- switch(choice,
    auto = if (any_cat) "WLSMV" else "ML",
    mlr = "MLR",
    ml_fiml = ,
    ml = "ML",
    wlsmv = "WLSMV"
  )
  # FIML under ML for continuous items (the default, DMAR's), under the
  # chosen ML with FIML and under MLR; complete cases under the chosen ML on
  # the complete cases and under WLSMV.
  fiml <- identical(estimator, "MLR") || identical(choice, "ml_fiml") ||
    (identical(choice, "auto") && identical(estimator, "ML"))
  missing <- if (fiml) "fiml" else "listwise"
  basis <- if (!any_cat) {
    "Pearson"
  } else if (all(categorical[known]) && binary_all) {
    "tetrachoric"
  } else if (all(categorical[known])) {
    "polychoric"
  } else {
    "mixed (polychoric and Pearson)"
  }
  label <- switch(estimator,
    MLR = "MLR with FIML",
    ML = if (identical(missing, "fiml")) "ML with FIML" else "ML (complete cases)",
    WLSMV = "WLSMV (DWLS)"
  )
  small_text <- NULL
  attenuation <- paste(
    "which treats the items as continuous (Pearson covariances of the category codes) and",
    "attenuates the loadings, the more so the fewer the categories (Rhemtulla, Brosseau-Liard",
    "& Savalei, 2012)"
  )
  # For a chosen continuous treatment: the ordered items it covers, if any.
  covered <- if (n_cat) {
    sprintf(
      paste(
        ", the %d item%s typed ordered or binary included, whose loadings are then attenuated,",
        "the more so the fewer the categories (Rhemtulla, Brosseau-Liard & Savalei, 2012)"
      ),
      n_cat, if (n_cat == 1L) "" else "s"
    )
  } else {
    ""
  }
  reason <- switch(choice,
    auto = if (!any_cat) {
      paste(c(sprintf(paste(
        "Every included item is continuous (five or more scale points, or values that are not",
        "integers), so maximum likelihood (ML) with full information maximum likelihood (FIML) for",
        "incomplete cases, on Pearson covariances%s: DMAR's default. Its standard errors and test",
        "statistic (and the RMSEA and CFI built on it) are normal theory. For markedly non-normal",
        "items (strongly skewed or heavy-tailed), choose MLR with FIML with the Estimator control (the",
        "Ingest tab for the study, or a model tab for its own fits): the same estimates, with",
        "Huber-White standard errors and a scaled test statistic that do not assume normality."
      ), n_text), small_text), collapse = " ")
    } else {
      sprintf(
        paste(
          "%d of the %d included items %s ordered or binary, so WLSMV (diagonally weighted least",
          "squares with a mean and variance adjusted test statistic) on %s correlations, on the",
          "complete cases%s; lavaan has no FIML for this estimator. The Estimator control (the",
          "Ingest tab for the study, or a model tab for its own fits) offers ML or MLR with FIML instead, %s."
        ),
        n_cat, n_items, if (n_cat == 1L) "is" else "are", basis, n_text, attenuation
      )
    },
    mlr = paste(c(sprintf(
      paste(
        "MLR with FIML was chosen with the Estimator control: every item is treated as continuous (Pearson",
        "covariances)%s, and an incomplete case contributes what it holds%s. The estimates are ML's; the",
        "standard errors are Huber-White and the test statistic is scaled, so the chi square, the RMSEA",
        "and the CFI reported are the scaled and robust ones, which suit markedly non-normal items."
      ),
      covered, n_text
    ), small_text), collapse = " "),
    ml_fiml = paste(c(sprintf(
      paste(
        "ML with FIML was chosen with the Estimator control: every item is treated as continuous (Pearson",
        "covariances)%s, and an incomplete case contributes what it holds%s; it is what Follow the item",
        "types gives continuous items (DMAR's default). The trade-off against MLR with FIML: the standard",
        "errors and the test statistic (and the RMSEA and CFI built on it) are normal theory, without",
        "MLR's robust corrections, so with skewed or heavy-tailed items the chi square and the RMSEA tend",
        "to be inflated and the standard errors understated; the point estimates are MLR's."
      ),
      covered, n_text
    ), small_text), collapse = " "),
    ml = sprintf(
      paste(
        "ML on the complete cases was chosen with the Estimator control: every item is treated as continuous",
        "(Pearson covariances)%s, and an incomplete case is dropped (listwise deletion)%s."
      ),
      covered, n_text
    ),
    wlsmv = sprintf(
      paste(
        "WLSMV was chosen with the Estimator control: every item is treated as ordered, with a threshold per",
        "category boundary and %s correlations, on the complete cases%s%s."
      ),
      basis, n_text,
      if (n_items > n_cat) {
        sprintf(
          "; %d item%s typed continuous on the Ingest tab enter%s as ordered here",
          n_items - n_cat, if (n_items - n_cat == 1L) "" else "s", if (n_items - n_cat == 1L) "s" else ""
        )
      } else {
        ""
      }
    )
  )
  list(
    choice = choice, estimator = estimator, missing = missing, categorical = categorical,
    basis = basis, label = label, reason = reason, small_n = small_n, n_categorical = n_cat
  )
}

#' Resolve a model plan into a specification
#'
#' The one place the engine choices are resolved for a lavaan model built
#' from the ledger (a CFA, a bifactor model, a structural model): the
#' estimator, the ordered variables, and the missing data treatment follow
#' the measurement level of the indicators in the plan, or the estimator
#' the researcher chose (see `sieve_estimator_resolve()`).
#'
#' @param plan A model plan: a list with `model`, `observed`, `latent`,
#'   `name_map`, and `not_modeled`, and optionally `structure`, `problems`,
#'   `out_map`, `pred_map`, `eff_map`, and `mediators` (see
#'   `sieve_cfa_plan()`, `sieve_structural_plan()`, and
#'   `sieve_bifactor_plan()`).
#' @param meta The item ledger (for the measurement level of the items).
#' @param kind A short name for the kind of analysis.
#' @param fun The lavaan function that fits it: `"cfa"` or `"sem"`.
#' @param std_lv,orthogonal See [sieve_cfa_spec()].
#' @param estimator One of `sieve_estimator_choices()`; `"auto"` follows
#'   the item types.
#' @return A list of class `sieve_spec`, with `estimator_choice` (the
#'   choice the estimator was resolved from; not part of `key`, so a
#'   chosen ML with FIML and the ML with FIML the types imply are the same
#'   specification).
#' @author Ken Kelley
#' @noRd
sieve_model_spec <- function(plan, meta, kind = "cfa", fun = c("cfa", "sem"),
                             std_lv = TRUE, orthogonal = FALSE, estimator = "auto") {
  fun <- match.arg(fun)
  active <- meta[meta$status == "active", , drop = FALSE]
  # A variable that is not a ledger item (an external outcome, an observed
  # predictor) has no measurement level here and is never treated as ordered.
  types <- as.character(active$type)[match(plan$observed, active$item)]
  external <- plan$external_types %||% character(0)
  at <- match(plan$observed, names(external))
  types[!is.na(at)] <- external[at[!is.na(at)]]
  resolved <- sieve_estimator_resolve(types, choice = estimator)
  ordered <- plan$observed[resolved$categorical]
  # Declared external outcome levels are authoritative, including when an
  # item-treatment override was selected for the measurement indicators.
  if (length(external)) ordered <- union(
    setdiff(
      ordered,
      names(external)
    ),
    names(external)[external %in% c(
      "ordered",
      "binary"
    )]
  )
  spec <- list(
    kind = kind,
    structure = plan$structure %||% kind,
    fun = fun,
    model = plan$model,
    observed = plan$observed,
    latent = plan$latent,
    estimator = resolved$estimator,
    ordered = if (length(ordered)) ordered else NULL,
    missing = resolved$missing,
    std_lv = isTRUE(std_lv),
    orthogonal = isTRUE(orthogonal),
    name_map = plan$name_map %||% character(0),
    not_modeled = plan$not_modeled %||% sieve_not_modeled(),
    problems = plan$problems %||% character(0),
    out_map = plan$out_map %||% character(0),
    pred_map = plan$pred_map %||% character(0),
    eff_map = plan$eff_map %||% character(0),
    mediators = plan$mediators %||% character(0)
  )
  if (length(external)) {
    spec$external_types <- external
    if (any(external %in% c("ordered", "binary")) && resolved$estimator %in% c("ML", "MLR")) {
      spec$problems <- c(
        spec$problems,
        "A declared categorical outcome requires an ordered-outcome estimator. Choose Follow the item types or WLSMV."
      )
    }
  }
  spec$key <- sieve_hash(spec)
  spec$estimator_choice <- resolved$choice
  structure(spec, class = "sieve_spec")
}

#' The lavaan arguments a specification resolves to
#'
#' @param spec A specification from [sieve_cfa_spec()].
#' @return A named list of arguments for [lavaan::cfa()], without `data`.
#' @author Ken Kelley
#' @noRd
sieve_spec_args <- function(spec) {
  args <- list(
    model = spec$model, estimator = spec$estimator, ordered = spec$ordered,
    std.lv = spec$std_lv, orthogonal = spec$orthogonal, missing = spec$missing
  )
  extra <- spec$engine_args %||% list()
  for (nm in names(extra)) args[[nm]] <- extra[[nm]]
  if (length(extra$aux)) args$check.post <- FALSE
  args$test <- args$test %||% sieve_spec_default_test(spec)
  args
}

#' The model test a specification asks lavaan for when it names none
#'
#' `"standard"` for ML with full information maximum likelihood (the
#' default for continuous items), and `NULL` (lavaan's default) otherwise.
#' lavaan adds Browne's residual based normal theory statistic
#' (`"browne.residual.nt.model"`) to every ML fit by default. SIEVER never
#' reports it (`sieve_lavaan_corrected_tests()` sets it aside), yet under
#' FIML it is computed when the model is fit and again for the baseline
#' model inside every `lavaan::fitMeasures()` and `DMAR::sem_fit_measures()`
#' call. Measured with lavaan 0.7-3 and DMAR 1.1.0 (40 continuous
#' items, n = 2,000, complete data): the fit took 2.83 seconds with
#' lavaan's default tests and 0.15 with `test = "standard"`, and
#' `DMAR::sem_fit_measures()` 8.6 and 3.1 seconds; at 80 items the fit took
#' 170 seconds and 0.94. Every estimate, standard error, test statistic and
#' fit index SIEVER reports is the same either way. MLR keeps lavaan's
#' default, because an explicit test list there changes its scaled
#' statistic (lavaan then sets the observed information to h1), and a test
#' the researcher sets (`engine_args$test`) is kept.
#'
#' @param spec A model specification (see `sieve_model_spec()`).
#' @return `"standard"` or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_spec_default_test <- function(spec) {
  if (!is.null(spec$engine_args$test)) {
    return(NULL)
  }
  ml <- identical(toupper(as.character(spec$estimator %||% "")[1L]), "ML")
  fiml <- isTRUE(as.character(spec$missing %||% "")[1L] %in% c("ml", "ml.x", "fiml"))
  if (ml && fiml && !length(spec$ordered)) "standard" else NULL
}

#' The comment the recorded R puts beside `test = "standard"`
#'
#' @param spec A model specification.
#' @return One comment line, or `NULL` when the specification asks for
#'   lavaan's default tests (see `sieve_spec_default_test()`).
#' @author Ken Kelley
#' @noRd
sieve_default_test_comment <- function(spec) {
  if (is.null(sieve_spec_default_test(spec))) {
    return(NULL)
  }
  paste0(
    "# test = \"standard\": the normal-theory model test SIEVER reports. lavaan (0.7-3 and later) would also ",
    "compute Browne's residual-based test, which SIEVER does not report and which under FIML takes most of the fit's time; ",
    "the estimates, standard errors and fit indices are the same either way."
  )
}

#' Extend a model specification with supported engine choices
#' @param spec A model specification.
#' @param args Named engine arguments for covariate or sampling design.
#' @return The specification with its revised content key.
#' @author Ken Kelley
#' @noRd
sieve_spec_engine_args <- function(spec, args = list()) {
  allowed <- c(
    "fixed.x",
    "conditional.x",
    "cluster",
    "sampling.weights",
    "sampling.weights.normalization",
    "se",
    "test",
    "bootstrap",
    "parameterization", "aux"
  )
  if (!is.list(args) || (length(args) && (is.null(names(args)) || any(!nzchar(names(args))) || anyDuplicated(names(args)) || any(!names(args) %in% allowed)))) stop("Unsupported or unnamed engine arguments.",
    call. = FALSE
  )
  for (nm in intersect(names(args), c("fixed.x", "conditional.x"))) {
    if (!is.logical(args[[nm]]) || length(args[[nm]]) != 1L || is.na(args[[nm]])) stop(
      paste(
        nm,
        "must be TRUE or FALSE."
      ),
      call. = FALSE
    )
  }
  for (nm in intersect(names(args), c("cluster", "sampling.weights"))) {
    if (!is.character(args[[nm]]) || length(args[[nm]]) != 1L || is.na(args[[nm]]) || !nzchar(args[[nm]])) stop(
      paste(
        nm,
        "must name one data column."
      ),
      call. = FALSE
    )
  }
  if (!is.null(args$parameterization) && !identical(
    args$parameterization,
    "theta"
  ) && !identical(
    args$parameterization,
    "delta"
  )) stop("parameterization must be theta or delta.",
    call. = FALSE
  )
  spec$engine_args <- utils::modifyList(spec$engine_args %||% list(), args)
  spec$key <- NULL
  spec$key <- sieve_hash(spec)
  spec
}

#' Exact engine extension arguments as R source
#' @param spec A model specification.
#' @return Source argument strings.
#' @author Ken Kelley
#' @noRd
sieve_engine_arg_code <- function(spec) {
  x <- spec$engine_args %||% list()
  vapply(
    names(x),
    function(nm) paste0(
      nm,
      " = ",
      paste(utils::capture.output(dput(x[[nm]])),
        collapse = ""
      )
    ),
    character(1)
  )
}

#' The R that reproduces a specified CFA
#'
#' Written from the same specification the fit used, so the recorded script
#' carries the actual estimator, ordered variables, missing data treatment,
#' scaling, and model.
#'
#' The summary it ends with prints the RMSEA interval at `conf_level`
#' (lavaan's `rmsea.ci.level`), the level of the interval the page shows:
#' `sieve_default_rmsea_conf_level()`, .90 by default, DMAR's default for
#' the RMSEA interval and lavaan's own.
#'
#' @param spec A specification from [sieve_cfa_spec()].
#' @param fit_name,model_name,data_name Object names used in the script.
#' @param conf_level The level of the RMSEA interval the summary prints.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_cfa_code <- function(spec, fit_name = "cfa_fit", model_name = "cfa_model",
                           data_name = "items", conf_level = sieve_default_rmsea_conf_level()) {
  chr <- function(x) sprintf("c(%s)", paste(sprintf("\"%s\"", x), collapse = ", "))
  args <- c(
    sprintf("estimator = \"%s\"", spec$estimator),
    if (!is.null(spec$ordered)) sprintf("ordered = %s", chr(spec$ordered)),
    sprintf("std.lv = %s", if (spec$std_lv) "TRUE" else "FALSE"),
    if (isTRUE(spec$orthogonal)) "orthogonal = TRUE",
    sprintf("missing = \"%s\"", spec$missing),
    if (!is.null(sieve_spec_default_test(spec))) "test = \"standard\"",
    sieve_engine_arg_code(spec),
    if (length(spec$engine_args$aux)) "check.post = FALSE"
  )
  why <- c(
    switch(spec$estimator_choice %||% "auto",
      mlr = "# Estimator chosen with the Estimator control: MLR with full information ML, every indicator treated as continuous.",
      ml_fiml = paste0(
        "# Estimator chosen with the Estimator control: ML with full information ML, every indicator treated as ",
        "continuous; normal-theory standard errors and test statistic."
      ),
      ml = "# Estimator chosen with the Estimator control: ML on the complete cases, every indicator treated as continuous.",
      wlsmv = "# Estimator chosen with the Estimator control: WLSMV on polychoric correlations, every indicator treated as ordered, complete cases.",
      if (!is.null(spec$ordered)) {
        "# Ordered or binary indicators: WLSMV on polychoric correlations, complete cases."
      } else if (identical(spec$estimator, "MLR")) {
        "# Continuous indicators: robust ML (MLR) with full information ML for incomplete cases."
      } else {
        paste0(
          "# Continuous indicators: ML with full information ML for incomplete cases (DMAR's default); ",
          "for markedly non-normal items choose MLR with FIML (estimator = \"MLR\")."
        )
      }
    ),
    if (spec$std_lv) {
      "# std.lv = TRUE fixes each latent variance to 1 (sets the latent scale; all loadings free)."
    } else {
      "# std.lv = FALSE: the first loading of each factor is fixed to 1 (marker indicator)."
    },
    sieve_default_test_comment(spec)
  )
  design_columns <- c(spec$engine_args$cluster, spec$engine_args$sampling.weights, spec$engine_args$aux)
  call_data <- paste0(model_name, "_data")
  preparation <- c(
    sprintf("%s <- %s", call_data, data_name),
    vapply(
      design_columns,
      function(nm) sprintf(
        "%s[[%s]] <- d[[%s]]",
        call_data,
        encodeString(nm,
          quote = '"'
        ),
        encodeString(nm,
          quote = '"'
        )
      ),
      character(1)
    ),
    sieve_numeric_code(call_data, unique(c(spec$observed, spec$engine_args$aux)), factor_labels = TRUE),
    if (length(spec$engine_args$cluster)) sprintf(
      "%s <- %s[!is.na(%s[[%s]]), , drop = FALSE]",
      call_data,
      call_data,
      call_data,
      encodeString(spec$engine_args$cluster,
        quote = '"'
      )
    )
  )
  c(
    "# Confirmatory measurement model. '=~' reads 'is measured by'.",
    sprintf("%s <- %s", model_name, sieve_r_literal(spec$model)),
    why, preparation,
    sprintf("%s <- lavaan::cfa(%s, data = %s,", fit_name, model_name, call_data),
    paste0("  ", paste(args, collapse = ", "), ")"),
    sprintf(
      "lavaan::summary(%s, fit.measures = list(fit.measures = TRUE, rmsea.ci.level = %s), standardized = TRUE)",
      fit_name, sieve_num_lit(conf_level)
    )
  )
}

#' The R that reproduces a specified structural model
#'
#' Written from the specification the fit used (model syntax, estimator,
#' ordered indicators, missing data treatment, and the data column behind
#' each external outcome), so the recorded script fits the same model, and
#' the step that fits it defines the object every later step of the tab
#' reads (`sem_fit`). The script reads `items` (the item columns, rebuilt
#' by the preamble of the recorded session) and `d` (the data as loaded).
#'
#' @param spec A specification from `sieve_model_spec()` with `fun =
#'   "sem"`, or any list with `model`, `estimator`, `ordered`, `missing`,
#'   `out_map` (engine column -> data column of each external outcome) and
#'   `pred_map` (the same for each antecedent).
#' @param fit_name,model_name,data_name Object names used in the script.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_sem_code <- function(spec, fit_name = "sem_fit", model_name = "sem_model",
                           data_name = "sem_data") {
  out_map <- spec$out_map %||% character(0)
  pred_map <- spec$pred_map %||% character(0)
  c(
    "# Structural model. '=~' reads 'is measured by'; '~' reads 'is regressed on'.",
    sprintf("%s <- %s", model_name, sieve_r_literal(spec$model)),
    "# The item columns, plus each external outcome and each antecedent (an observed",
    "# predictor) under its name in the model.",
    sprintf("%s <- items", data_name),
    if (length(out_map)) {
      vapply(seq_along(out_map), function(i) {
        source <- encodeString(unname(out_map[i]), quote = '"')
        sprintf(
          "%s[[%s]] <- d[[%s]]",
          data_name, encodeString(names(out_map)[i], quote = '"'), source
        )
      }, character(1))
    },
    if (length(pred_map)) {
      vapply(seq_along(pred_map), function(i) {
        source <- encodeString(unname(pred_map[i]), quote = '"')
        sprintf(
          "%s[[%s]] <- d[[%s]]",
          data_name, encodeString(names(pred_map)[i], quote = '"'), source
        )
      }, character(1))
    },
    vapply(
      unique(c(spec$engine_args$cluster, spec$engine_args$sampling.weights,
        spec$engine_args$aux)), function(nm)
        sprintf(
          "%s[[%s]] <- d[[%s]]",
          data_name,
          encodeString(nm,
            quote = '"'
          ),
          encodeString(nm,
            quote = '"'
          )
        ),
      character(1)
    ),
    sieve_numeric_code(data_name, unique(c(spec$observed, spec$engine_args$aux)), factor_labels = TRUE),
    if (length(spec$engine_args$cluster)) sprintf(
      "%s <- %s[!is.na(%s[[%s]]), , drop = FALSE]",
      data_name,
      data_name,
      data_name,
      encodeString(spec$engine_args$cluster,
        quote = '"'
      )
    ),
    sieve_default_test_comment(spec),
    sprintf(
      "%s <- lavaan::sem(%s, data = %s, estimator = \"%s\",",
      fit_name, model_name, data_name, spec$estimator
    ),
    sprintf(
      "  %sstd.lv = TRUE, missing = \"%s\"%s)",
      if (length(spec$ordered)) {
        sprintf("ordered = c(%s), ", paste(sprintf("\"%s\"", spec$ordered), collapse = ", "))
      } else {
        ""
      },
      spec$missing,
      if (length(c(sieve_spec_default_test(spec), sieve_engine_arg_code(spec)))) paste0(
        ", ",
        paste(c(
          if (!is.null(sieve_spec_default_test(spec))) "test = \"standard\"",
          sieve_engine_arg_code(spec)
        ), collapse = ", ")
      ) else ""
    )
  )
}

#' Read the modeled columns as numbers, and say what could not be read
#'
#' The engine fits numbers. A column that holds text (a factor, or a
#' character column with entries such as "n/a" or "7 (approx.)") cannot be
#' coerced quietly: every entry that is not a number would become a missing
#' value, the fit would run on the cases that remain, and the status would
#' say ok. The coercion is made here, once, and every value that was present
#' and could not be read as a number is counted and named, so the caller
#' refuses the model with the reason. Blank entries are missing values, not
#' problems. A column with no observed values, or with one value throughout,
#' is reported as well: the engine stops on such a column and prints its
#' whole variable table to the console instead of naming it.
#'
#' An infinite value (`Inf` or `-Inf`, which a text file holds as `Inf`,
#' `inf`, `Infinity`, or a number beyond double precision such as `1e999`)
#' is reported with its column and its rows (kind `"not_finite"`): it is not
#' a missing-data code but the trace of a division by zero, a logarithm of
#' zero or an overflow in the program that wrote the file, and treating it
#' as missing would change what each respondent answered without saying so.
#' `NaN` (read from `NaN` or `nan` in a text file) is a missing value, as
#' [is.na()], [stats::cor()] and lavaan treat it, and is returned as `NA`.
#'
#' @param data A data frame.
#' @param columns The columns to read, in the order wanted.
#' @param what What each column is, for the message: `"Item"` (the action
#'   is to exclude it on the Items tab), `"Outcome column"` (the action is
#'   to choose another outcome), `"Antecedent column"` (another
#'   predictor) or `"Auxiliary column"` (remove it from the auxiliary
#'   variables). One value, or one per column.
#' @param purpose What the values are for, in the message: `"model"` (the
#'   default; "so the model cannot be fit") or `"score"` ("so its score
#'   cannot be formed").
#' @return A list with `data` (the columns as doubles, `NaN` as `NA`),
#'   `problems` (a data frame with `column`, `kind` (`"not_numeric"`,
#'   `"not_finite"`, `"no_values"`, or `"no_variance"`), `n` (the number of
#'   values concerned), and `example` (one offending entry; for
#'   `"not_finite"` the values and their rows, such as `"Inf in row 3"`; or
#'   the constant value)), and `text` (one sentence per problem, naming the
#'   column and what to do; empty when there is none).
#' @author Ken Kelley
#' @noRd
sieve_numeric_columns <- function(data, columns, what = "Item", purpose = c("model", "score")) {
  purpose <- match.arg(purpose)
  consequence <- if (identical(purpose, "score")) "so its score cannot be formed" else "so the model cannot be fit"
  d <- data[, columns, drop = FALSE]
  what <- rep_len(as.character(what), length(columns))
  names(what) <- columns
  rows <- list()
  for (nm in columns) {
    v <- d[[nm]]
    if (is.factor(v)) {
      v <- as.character(v)
    }
    if (is.character(v)) {
      v[!is.na(v) & !nzchar(trimws(v))] <- NA_character_
      num <- suppressWarnings(as.numeric(v))
      lost <- which(!is.na(v) & is.na(num))
      if (length(lost)) {
        rows[[length(rows) + 1L]] <- data.frame(
          column = nm, kind = "not_numeric", n = length(lost), example = v[lost[1L]],
          stringsAsFactors = FALSE
        )
      }
    } else {
      num <- sieve_as_number(v)
    }
    num[is.nan(num)] <- NA_real_
    infinite <- which(is.infinite(num))
    present <- num[!is.na(num) & is.finite(num)]
    if (length(infinite)) {
      shown <- utils::head(infinite, 3L)
      rows[[length(rows) + 1L]] <- data.frame(
        column = nm, kind = "not_finite", n = length(infinite),
        example = paste0(
          paste(sprintf("%s in row %d", ifelse(num[shown] > 0, "Inf", "-Inf"), shown), collapse = "; "),
          if (length(infinite) > length(shown)) sprintf("; and %d more", length(infinite) - length(shown)) else ""
        ),
        stringsAsFactors = FALSE
      )
    } else if (length(present) < 2L) {
      rows[[length(rows) + 1L]] <- data.frame(
        column = nm, kind = "no_values", n = length(present), example = NA_character_,
        stringsAsFactors = FALSE
      )
    } else if (all(present == present[1L])) {
      rows[[length(rows) + 1L]] <- data.frame(
        column = nm, kind = "no_variance", n = length(present),
        example = format(present[1L], digits = 15), stringsAsFactors = FALSE
      )
    }
    d[[nm]] <- num
  }
  problems <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(
      column = character(0), kind = character(0), n = integer(0),
      example = character(0), stringsAsFactors = FALSE
    )
  }
  text <- vapply(seq_len(nrow(problems)), function(i) {
    kind <- what[[problems$column[i]]]
    leave_out <- if (identical(kind, "Item")) {
      "exclude the item on the Items tab"
    } else if (identical(kind, "Antecedent column")) {
      "choose another predictor"
    } else if (identical(kind, "Auxiliary column")) {
      "remove it from the auxiliary variables"
    } else {
      "choose another outcome"
    }
    switch(problems$kind[i],
      not_numeric = sprintf(
        paste0(
          "%s '%s' holds %d value%s that %s not numbers (for example \"%s\"), %s. ",
          "Correct those entries or set them to missing before loading the data, ",
          "or %s."
        ),
        kind, problems$column[i], problems$n[i], if (problems$n[i] == 1L) "" else "s",
        if (problems$n[i] == 1L) "is" else "are", problems$example[i], consequence, leave_out
      ),
      not_finite = sprintf(
        paste0(
          "%s '%s' holds %d infinite value%s (%s), %s: a response is a finite number or ",
          "missing (NA). An infinite value usually comes from a division by zero, a logarithm of ",
          "zero, or an overflow in the program that wrote the file (a text file holds it as Inf, ",
          "Infinity, or a number beyond double precision such as 1e999). Correct those entries or ",
          "set them to missing before loading the data, or %s."
        ),
        kind, problems$column[i], problems$n[i], if (problems$n[i] == 1L) "" else "s",
        problems$example[i], consequence, leave_out
      ),
      no_values = sprintf(
        paste0(
          "%s '%s' has %s observed value%s, %s. %s."
        ),
        kind, problems$column[i], if (problems$n[i] == 0L) "no" else "one",
        if (problems$n[i] == 1L) "" else "s", consequence,
        paste0(toupper(substring(leave_out, 1L, 1L)), substring(leave_out, 2L))
      ),
      no_variance = sprintf(
        paste0(
          "%s '%s' takes the same value (%s) for every case, so it carries no information ",
          "and %s. %s, or check that its values were read correctly."
        ),
        kind, problems$column[i], problems$example[i],
        if (identical(purpose, "score")) "its score cannot be formed" else "the model cannot be fit",
        paste0(toupper(substring(leave_out, 1L, 1L)), substring(leave_out, 2L))
      )
    )
  }, character(1))
  list(data = d, problems = problems, text = text)
}

#' The number of sample statistics a categorical fit estimates
#'
#' A WLSMV (or DWLS, WLSM, ULSMV) fit estimates the thresholds of the
#' ordered items and the polychoric correlations of every pair. For `p`
#' ordered items with `k` categories each, that is `p (k - 1)` thresholds
#' plus `p (p - 1) / 2` correlations; a continuous item in a mixed model
#' contributes its mean and its variance in place of thresholds. The count
#' is lavaan's own: it equals the dimension of `lavInspect(fit, "gamma")`.
#'
#' @param n_items The number of items in the model.
#' @param n_categories The number of response categories, one value for
#'   every item or one per item; `NA` for a continuous item.
#' @return The number of statistics, a double.
#' @author Ken Kelley
#' @noRd
sieve_wlsmv_statistics <- function(n_items, n_categories) {
  n_items <- as.integer(n_items)
  if (length(n_items) != 1L || is.na(n_items) || n_items < 0L) {
    stop("`n_items` must be a single non-negative integer.", call. = FALSE)
  }
  k <- rep_len(as.numeric(n_categories), n_items)
  per_item <- ifelse(is.na(k), 2, k - 1)
  sum(per_item) + n_items * (n_items - 1) / 2
}

#' The size of the weight matrix a categorical fit needs
#'
#' The asymptotic covariance matrix of the sample statistics of a WLSMV
#' (or DWLS, WLSM, ULSMV) fit is square in their number (see
#' `sieve_wlsmv_statistics()`): with `s` statistics it holds `s^2` doubles
#' of 8 bytes. 303 five-category items give 46,965 statistics and about
#' 17.6 GB, 26 such items 429 statistics and 1.5 MB. This is the size of
#' one copy; the memory a fit needs at its peak is several times larger
#' (see `sieve_wlsmv_peak_bytes()`).
#'
#' @inheritParams sieve_wlsmv_statistics
#' @return The number of bytes, a double.
#' @author Ken Kelley
#' @noRd
sieve_wlsmv_weight_bytes <- function(n_items, n_categories) {
  8 * sieve_wlsmv_statistics(n_items, n_categories)^2
}

#' The memory a categorical fit needs at its peak
#'
#' A lavaan WLSMV fit holds, at its peak, about twenty working copies of the
#' `s` by `s` weight matrix (`sieve_wlsmv_weight_bytes()`; lavaan 0.7-3's
#' `muthen1984()` forms the matrix and several products of it, among them
#' its inverse and the sandwich `B^-1 A B^-1'`) and, at large samples,
#' several `n` by `s` matrices of casewise scores (the scores of the
#' thresholds, slopes, variances and correlations it binds into one
#' matrix), on top of about 0.4 GB for R, the packages and the fit's own
#' state. The estimate is the sum,
#' `4e8 + 20 * 8 s^2 + 6 * 8 n s` bytes, which errs high, as a guard
#' should. Measured as the peak resident memory of one R
#' process running `sieve_analyze("cfa")` on four-category items (two
#' factors, lavaan 0.7-3, R 4.6.2, `/usr/bin/time -l`): 0.59, 0.64 and
#' 1.17 GB for 40 items (900 statistics) at n = 1,000, 5,000 and 20,000,
#' and 2.29 and 2.57 GB for 80 items (3,400 statistics) at n = 2,000 and
#' 8,000. The estimate is 0.57, 0.75, 1.39, 2.58 and 3.56 GB: between 2
#' percent below and 38 percent above each measurement. One copy of the
#' weight matrix alone, the size a guard against a fixed 2 GB would
#' compare, is 6.5 MB at 40 items and 92 MB at 80, a twenty-fifth of the
#' peak or less.
#'
#' @inheritParams sieve_wlsmv_statistics
#' @param n The number of respondents; `NA` leaves the casewise scores out
#'   (for a plan made before the data are known).
#' @return Bytes, a double.
#' @author Ken Kelley
#' @noRd
sieve_wlsmv_peak_bytes <- function(n_items, n_categories, n = NA_real_) {
  s <- sieve_wlsmv_statistics(n_items, n_categories)
  n <- suppressWarnings(as.numeric(n)[1L])
  casewise <- if (is.finite(n) && n > 0) 6 * 8 * n * s else 0
  4e8 + 20 * 8 * s^2 + casewise
}

#' The memory a categorical fit may plan to hold
#'
#' The option `sieve.wlsmv_limit` (bytes) when it is set; otherwise
#' `sieve_memory_limit()`, the limit every maximum likelihood fit is held to
#' (three quarters of the machine's physical memory, or the option
#' `sieve.memory_limit`). It bounds the estimated peak of the fit
#' (`sieve_wlsmv_peak_bytes()`), not one copy of its weight matrix.
#'
#' @return A number of bytes.
#' @author Ken Kelley
#' @noRd
sieve_wlsmv_limit <- function() {
  opt <- suppressWarnings(as.numeric(getOption("sieve.wlsmv_limit")))
  if (length(opt) == 1L && is.finite(opt) && opt > 0) opt else sieve_memory_limit()
}

#' Where a memory limit comes from, in words
#'
#' @param limit The limit, in bytes.
#' @param option The option that sets it here (`"sieve.wlsmv_limit"` or
#'   `"sieve.memory_limit"`).
#' @return A phrase such as "the 24.0 GB SIEVER allows on this machine
#'   (three quarters of its 32.0 GB; option sieve.memory_limit)".
#' @author Ken Kelley
#' @noRd
sieve_memory_allowed_text <- function(limit, option = "sieve.memory_limit") {
  set <- function(nm) {
    v <- suppressWarnings(as.numeric(getOption(nm)))
    length(v) == 1L && is.finite(v) && v > 0
  }
  physical <- sieve_physical_memory()
  if (set(option) || set("sieve.memory_limit") || !is.finite(physical) ||
    !isTRUE(all.equal(limit, 0.75 * physical))) {
    named <- if (set(option)) option else "sieve.memory_limit"
    return(sprintf("the %s SIEVER allows (option %s)", sieve_bytes_text(limit), named))
  }
  sprintf(
    "the %s SIEVER allows on this machine (three quarters of its %s; option sieve.memory_limit)",
    sieve_bytes_text(limit), sieve_bytes_text(physical)
  )
}

#' Refuse a categorical fit that would not fit in memory
#'
#' Before a WLSMV, DWLS, WLSM, ULSMV or ULS fit, its peak memory is
#' estimated from the item count, the observed categories and the number of
#' respondents (`sieve_wlsmv_peak_bytes()`) and compared with
#' `sieve_wlsmv_limit()`. Above it the fit is refused with the estimate,
#' the counts that drive it, and the two ways forward: one construct at a
#' time, or the continuous treatment under maximum likelihood (ML with
#' FIML, MLR with FIML or ML on the complete cases), which is a different
#' model and is said to be.
#'
#' @param items The names of the observed variables in the model.
#' @param data A data frame holding them.
#' @param ordered The names of the ordered (or binary) ones.
#' @param estimator The estimator the fit would use.
#' @param limit The largest peak allowed, in bytes.
#' @return `NULL` when the fit may proceed, otherwise the refusal, one
#'   string.
#' @author Ken Kelley
#' @noRd
sieve_wlsmv_guard <- function(items, data, ordered = items, estimator = "WLSMV",
                              limit = sieve_wlsmv_limit()) {
  ordered <- intersect(ordered, items)
  if (!length(ordered) || !estimator %in% sieve_categorical_estimators()) {
    return(NULL)
  }
  categories <- vapply(items, function(nm) {
    if (!nm %in% ordered || !nm %in% names(data)) {
      return(NA_real_)
    }
    v <- sieve_as_number(data[[nm]])
    length(unique(v[!is.na(v)]))
  }, numeric(1))
  # The respondents with a response to any of the items: lavaan's complete
  # cases are at most that many, so the estimate errs high.
  present <- intersect(items, names(data))
  n <- if (length(present)) {
    sum(rowSums(!is.na(as.data.frame(data)[, present, drop = FALSE])) > 0L)
  } else {
    NROW(data)
  }
  peak <- sieve_wlsmv_peak_bytes(length(items), categories, n)
  if (peak <= limit) {
    return(NULL)
  }
  thresholds <- sum(categories[!is.na(categories)] - 1)
  pairs <- length(items) * (length(items) - 1) / 2
  stats <- sieve_wlsmv_statistics(length(items), categories)
  big <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  paste0(
    sprintf(
      paste0(
        "Not fit: the weight matrix of a %s fit of these %d items (%d of them ordered or binary) ",
        "has a row and a column for each of its %s sample statistics (%s thresholds and %s correlations%s), ",
        "about %s, and lavaan keeps about twenty working copies of it beside several %s by %s matrices of ",
        "casewise scores (%s each), so on these %s respondents the fit would need an estimated %s at its peak, ",
        "more than %s. The need grows with the square of the number of sample statistics and with the number ",
        "of respondents. "
      ),
      estimator, length(items), length(ordered), big(stats), big(thresholds), big(pairs),
      if (any(is.na(categories))) ", with the means and variances of the continuous items" else "",
      sieve_bytes_text(sieve_wlsmv_weight_bytes(length(items), categories)),
      big(n), big(stats), sieve_bytes_text(8 * n * stats), big(n), sieve_bytes_text(peak),
      sieve_memory_allowed_text(limit, "sieve.wlsmv_limit")
    ),
    "Two ways forward. Analyze one construct at a time: on the Items tab exclude the items ",
    "of the other constructs (or prepare a project of one construct with ",
    "sieve_prepare_study()), and fit each construct's model in turn. Or treat the items as ",
    "continuous and fit them by maximum likelihood: choose ML with FIML (or MLR with FIML for markedly ",
    "non-normal items, or ML on the complete cases) in the ",
    "Estimator control of the CFA tab's sidebar, or set their type to continuous on the ",
    "Ingest tab and confirm again (Follow the item types then gives them ML with FIML). That is a different model, not the same one estimated ",
    "another way: Pearson covariances of the category codes replace the polychoric ",
    "correlations, so the loadings are attenuated, the more so the fewer the categories ",
    "(Rhemtulla, Brosseau-Liard & Savalei, 2012)."
  )
}

#' The memory a maximum likelihood fit of `p` items needs
#'
#' Under MLR the scaled test statistic needs the asymptotic covariance
#' matrix of the sample moments, whose rows and columns are the
#' `p* = p (p + 1) / 2 + p` means, variances and covariances (46,359 for
#' 303 items), and lavaan holds several matrices of that size while it
#' forms the statistic and its baseline model: the need grows with the
#' square of `p*`. Measured as the peak resident memory of one R process
#' fitting one `lavaan::efa()` solution (n = 500): 0.57 GB at 50 items,
#' 2.2 GB at 80, 6.3 GB at 110 and 1.3 GB for a private study of 68 items,
#' about 0.6 GB plus 147 bytes for each of the `p*^2` elements (see
#' `sieve_efa_worker_bytes()`), which at 303 items is over 300 GB. Under ML
#' on the complete cases there is no such matrix and the need grows with
#' `p^3` (0.27 GB at 100 items, 2.4 GB at 303), about 0.22 GB plus 80 bytes
#' times `p^3`. Under ML with FIML, the default for continuous items, the
#' fit measures of a FIML fit form `p*` by `p*` matrices too: the corrected CFI, TLI and RMSEA of
#' Zhang and Savalei (2023), which lavaan 0.7-3 computes in every
#' `lavaan::fitMeasures()` call of a FIML fit (`lav_fit_fiml_corrected()`,
#' from the information matrices of the unrestricted model) and which
#' DMAR's check of the unrestricted model (`sieve_h1_refusal()`, through
#' `DMAR::sem_fit_measures()`) computes once more. SIEVER's own ML fits with
#' FIML do not ask for Browne's residual based test
#' (`sieve_spec_default_test()`), which lavaan forms through
#' `lav_object_gamma()`; the corrected indices remain. Measured with
#' lavaan 0.7-3 and DMAR 1.1.0 (four correlated factors, n = 500, complete
#' data), the CFA page's path
#' (`sieve_run_cfa()` and `sieve_fit_measures()`) took 0.81, 2.3 and 6.2
#' seconds at 24, 32 and 40 items under ML with FIML, against MLR's 1.6,
#' 5.2 and 15.2 (and 1.3, 5.5 and 19.6 under ML with FIML with Browne's
#' test; 0.05 to 0.09 under ML on the complete cases), and 38 seconds at 56
#' items; one `lavaan::fitMeasures()` call at 40 items raised R's heap from
#' 82 MB to 265 MB, about thirty copies of the 5.9 MB `p*` by `p*` matrix,
#' none of it from `lav_object_gamma()`. So ML with FIML still grows with
#' the square of `p*` and is given MLR's need (`missing = "fiml"`). The
#' formula is the measurement; nothing is run to find it.
#'
#' @param p The number of items in the model.
#' @param estimator `"MLR"` or `"ML"`.
#' @param missing `"fiml"` for full information maximum likelihood, or
#'   `NULL` (or anything else) for the complete cases; read under ML only.
#' @return Bytes, a double.
#' @author Ken Kelley
#' @noRd
sieve_ml_bytes <- function(p, estimator = "MLR", missing = NULL) {
  p <- as.numeric(p)
  if (identical(estimator, "ML") && !identical(missing, "fiml")) {
    return(2.2e8 + 80 * p^3)
  }
  moments <- p * (p + 1) / 2 + p
  6e8 + 147 * moments^2
}

#' The physical memory of this machine
#'
#' Read once per process: `hw.memsize` on macOS, `MemTotal` in
#' `/proc/meminfo` on Linux; `NA` elsewhere or when it cannot be read.
#'
#' @return Bytes, or `NA`.
#' @author Ken Kelley
#' @noRd
sieve_physical_memory <- function() {
  if (exists("physical_memory", envir = sieve_rates, inherits = FALSE)) {
    return(get("physical_memory", envir = sieve_rates))
  }
  bytes <- NA_real_
  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    out <- tryCatch(suppressWarnings(system2("sysctl", c("-n", "hw.memsize"), stdout = TRUE, stderr = FALSE)),
      error = function(e) character(0)
    )
    bytes <- suppressWarnings(as.numeric(out[1]))
  } else if (file.exists("/proc/meminfo")) {
    line <- tryCatch(grep("^MemTotal:", readLines("/proc/meminfo", warn = FALSE), value = TRUE),
      error = function(e) character(0)
    )
    kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", line[1])))
    bytes <- kb * 1024
  }
  if (length(bytes) != 1L || !is.finite(bytes) || bytes <= 0) bytes <- NA_real_
  assign("physical_memory", bytes, envir = sieve_rates)
  bytes
}

#' The memory one maximum likelihood fit may plan to hold
#'
#' The option `sieve.memory_limit` (bytes) when it is set; otherwise three
#' quarters of the machine's physical memory (`sieve_physical_memory()`),
#' leaving room for the session and the data; 16 GB when the physical
#' memory cannot be read.
#'
#' @return Bytes.
#' @author Ken Kelley
#' @noRd
sieve_memory_limit <- function() {
  opt <- suppressWarnings(as.numeric(getOption("sieve.memory_limit")))
  if (length(opt) == 1L && is.finite(opt) && opt > 0) {
    return(opt)
  }
  physical <- sieve_physical_memory()
  if (is.finite(physical)) 0.75 * physical else 16e9
}

#' Bytes in words
#'
#' @param b Bytes.
#' @return `"316.2 GB"`, `"58.0 MB"`, or `"12 KB"`.
#' @author Ken Kelley
#' @noRd
sieve_bytes_text <- function(b) {
  if (!is.finite(b)) {
    return("an unknown amount")
  }
  if (b >= 1e9) sprintf("%.1f GB", b / 1e9) else if (b >= 1e6) sprintf("%.1f MB", b / 1e6) else sprintf("%.0f KB", b / 1e3)
}

#' The free parameters of a factor model of `p` items
#'
#' Loadings, residual variances, intercepts (under FIML, which estimates
#' the mean structure) and factor correlations: `p k - k (k - 1) / 2`
#' loadings for an exploratory model with `k` factors, `p` for a
#' confirmatory model whose items each load on one factor.
#'
#' @param p The number of items.
#' @param k The number of factors.
#' @param exploratory Whether every item loads on every factor.
#' @param means Whether the intercepts are estimated (FIML).
#' @return A number.
#' @author Ken Kelley
#' @noRd
sieve_free_parameters <- function(p, k = 1L, exploratory = FALSE, means = TRUE) {
  p <- as.numeric(p)
  k <- as.numeric(k)
  loadings <- if (isTRUE(exploratory)) p * k - k * (k - 1) / 2 else p
  correlations <- if (isTRUE(exploratory)) 0 else k * (k - 1) / 2
  loadings + p + (if (isTRUE(means)) p else 0) + correlations
}

#' Refuse a maximum likelihood fit the machine or the sample cannot carry
#'
#' The counterpart of `sieve_wlsmv_guard()` for MLR (and ML). Before a fit
#' starts, two things are checked, and either refuses it with its reason in
#' one sentence. Memory: the need is computed from the item count
#' (`sieve_ml_bytes()`; for MLR the asymptotic covariance matrix of the
#' sample moments that its scaled test statistic needs, and for ML with
#' FIML matrices of the same size, which lavaan forms for the corrected fit
#' indices of a FIML fit) and compared with
#' `sieve_memory_limit()`. Rank (MLR only): robust standard errors are a
#' sandwich whose middle term is a sum of n casewise score outer products,
#' of rank at most n, so with as many free parameters as respondents or
#' more they are rank deficient whatever the memory (measured on a private
#' study of 303 items, n = 500: lavaan warns that the covariance matrix of
#' the estimates is not positive definite). The ways forward follow, in order:
#' the constructs one at a time (a full fit is quick at 20 to 40 items);
#' after a memory refusal, ML on the complete cases, which forms no matrix
#' of the moments, with what
#' its normal-theory test statistic and standard errors and its listwise
#' deletion give up (after MLR's rank refusal, ML with FIML, which has no
#' sandwich); and, for MLR, robust (Huber-White) standard errors
#' with the normal-theory test only when the free parameters are well
#' below n (at most half) and the ML need fits, otherwise the reason they
#' are not offered. The estimator is never changed for the researcher.
#'
#' @param p The number of items in the largest single model (a vector of
#'   counts, one per separately fit model, is read by its maximum).
#' @param estimator The estimator the fit would use.
#' @param limit The largest memory need allowed, in bytes.
#' @param n The number of respondents, or `NA` to skip the rank check.
#' @param npar The number of free parameters of the model (see
#'   `sieve_free_parameters()`); by default that of a one-factor model of
#'   the items with intercepts.
#' @param missing The missing data treatment of the fit (`"fiml"` or
#'   `"listwise"`), or `NULL` when the fit forms no fit measures of a FIML
#'   fit (`lavaan::efa()`'s solutions by k); read under ML only.
#' @return `NULL` when the fit may proceed, otherwise the refusal, one
#'   string.
#' @author Ken Kelley
#' @noRd
sieve_mlr_guard <- function(p, estimator = "MLR", limit = sieve_memory_limit(), n = NA_real_,
                            npar = NULL, missing = NULL) {
  p <- suppressWarnings(max(as.numeric(p), na.rm = TRUE))
  if (!is.finite(p) || length(estimator) != 1L || !estimator %in% c("MLR", "ML")) {
    return(NULL)
  }
  npar <- as.numeric(npar %||% sieve_free_parameters(p))
  n <- suppressWarnings(as.numeric(n)[1])
  ml_fiml <- identical(estimator, "ML") && identical(missing, "fiml")
  bytes <- sieve_ml_bytes(p, estimator, missing = missing)
  memory <- bytes > limit
  rank <- identical(estimator, "MLR") && is.finite(n) && is.finite(npar) && npar >= n
  if (!memory && !rank) {
    return(NULL)
  }
  moments <- p * (p + 1) / 2 + p
  physical <- sieve_physical_memory()
  option_set <- is.finite(suppressWarnings(as.numeric(getOption("sieve.memory_limit", NA_real_)))[1])
  allowed <- if (isTRUE(option_set) || !is.finite(physical)) {
    sprintf("the %s SIEVER allows (option sieve.memory_limit)", sieve_bytes_text(limit))
  } else {
    sprintf("the %s SIEVER allows on this machine (three quarters of its %s)", sieve_bytes_text(limit), sieve_bytes_text(physical))
  }
  why_memory <- if (memory && identical(estimator, "MLR")) {
    sprintf(
      paste0(
        "MLR's scaled test statistic needs the asymptotic covariance matrix of the %s sample means, ",
        "variances and covariances of these %d items, and lavaan's working copies of it need an ",
        "estimated %s, more than %s."
      ),
      format(moments, big.mark = ","), as.integer(p), sieve_bytes_text(bytes), allowed
    )
  } else if (memory && ml_fiml) {
    sprintf(
      paste0(
        "under ML with FIML, the corrected fit indices lavaan reports for a FIML fit (Zhang & Savalei, 2023), ",
        "which lavaan::fitMeasures() and DMAR's check of the unrestricted model compute, need the information ",
        "matrices of the unrestricted model, with a row and a column for each of the %s sample means, variances ",
        "and covariances of these %d items, and their working copies need an estimated %s, more than %s."
      ),
      format(moments, big.mark = ","), as.integer(p), sieve_bytes_text(bytes), allowed
    )
  } else if (memory) {
    sprintf("An ML fit of these %d items needs an estimated %s, more than %s.", as.integer(p), sieve_bytes_text(bytes), allowed)
  }
  why_rank <- if (rank) {
    sprintf(
      paste0(
        "MLR's robust standard errors would be rank deficient: the model has %s free parameters and ",
        "there are %s respondents, and the middle term of the sandwich, a sum of %s casewise score ",
        "outer products, has rank at most %s."
      ),
      format(npar, big.mark = ","), format(n, big.mark = ","), format(n, big.mark = ","), format(n, big.mark = ",")
    )
  }
  head <- if (memory && rank) {
    paste("Not fit, for two reasons.", why_memory, why_rank)
  } else {
    paste("Not fit:", c(why_memory, why_rank)[1])
  }
  # ML on the complete cases forms no matrix of the moments; ML with FIML
  # does, for its fit measures, so it is not a way forward from a memory
  # refusal.
  ml_fits <- sieve_ml_bytes(p, "ML") <= limit
  robust <- if (identical(estimator, "MLR") && ml_fits && is.finite(n) && npar <= n / 2) {
    sprintf(
      paste0(
        " Third, ML with FIML and robust (Huber-White) standard errors with the normal-theory test ",
        "statistic, whose fit forms no matrix of the moments (%s free parameters against %s respondents); ",
        "lavaan's fitMeasures() of a FIML fit forms it, so read the test from the fit's summary."
      ),
      format(npar, big.mark = ","), format(n, big.mark = ",")
    )
  } else if (identical(estimator, "MLR")) {
    paste0(
      " Robust (Huber-White) standard errors with ML are not offered here: ",
      if (is.finite(n) && npar >= n) {
        sprintf("with %s free parameters for %s respondents they are rank deficient for the same reason.",
          format(npar, big.mark = ","), format(n, big.mark = ","))
      } else if (is.finite(n) && npar > n / 2) {
        sprintf("%s free parameters are too close to the %s respondents for a stable sandwich.",
          format(npar, big.mark = ","), format(n, big.mark = ","))
      } else if (!ml_fits) {
        "the ML fit itself does not fit in memory."
      } else {
        "the number of respondents is not known here."
      }
    )
  } else {
    ""
  }
  paste0(
    head, " ",
    "Ways forward, in this order. First, analyze the constructs one at a time: on the Items tab ",
    "exclude the items of the other constructs (or prepare a project of one construct with ",
    "sieve_prepare_study()); a full fit is quick at 20 to 40 items. ",
    if (memory && (identical(estimator, "MLR") || ml_fiml) && ml_fits) {
      paste0(
        "Second, ML on the complete cases (the Estimator control's choice \"ML on the complete cases\"), ",
        "which forms no matrix of the moments: an incomplete case is then dropped, and its test statistic, ",
        "RMSEA, CFI and standard errors are normal theory, which with skewed rating-scale items tends to ",
        "inflate the chi square and the RMSEA and to understate the standard errors."
      )
    } else if (!memory && identical(estimator, "MLR")) {
      paste0(
        "Second, ML with FIML (the Estimator control's choice \"ML with FIML\"), what Follow the item ",
        "types gives continuous items: its test statistic, RMSEA, CFI and standard errors are then normal ",
        "theory, which with skewed rating-scale items tends to inflate the chi square and the RMSEA and to ",
        "understate the standard errors."
      )
    } else {
      "Second, fewer items in the model."
    },
    robust,
    " SIEVER does not change the estimator for you: the Estimator control (the Ingest tab for the ",
    "study, or a model tab for its own fits) offers ML with FIML, MLR with FIML and ML on the complete cases",
    if (startsWith(robust, " Third")) {
      paste0(
        "; the third way is not one of its choices (in your own R, lavaan::cfa() with ",
        "estimator = \"ML\", missing = \"ml\" and se = \"robust.huber.white\")"
      )
    },
    ", and the choice you make is recorded."
  )
}

#' Refuse a specified MLR (or ML) fit before it starts
#'
#' The MLR refusal (`sieve_mlr_guard()`, the same check and the same
#' message) for the fits made from a model specification outside the
#' on-request wire of the model tabs: the halves of the calibration and
#' validation split, the rungs of the invariance ladder, and the
#' longitudinal rungs (the CV and Invariance tab). The item count is the
#' specification's observed variables; the free parameters are those of its
#' confirmatory model (`sieve_free_parameters()`: loadings, residual
#' variances, factor correlations, and the intercepts when the mean
#' structure is estimated), once per group for a multiple group model, so
#' the rank check compares the configural model of all groups with every
#' respondent.
#'
#' @param spec A specification from [sieve_cfa_spec()] or
#'   `sieve_model_spec()`.
#' @param n The number of respondents the fit would use, or `NA`.
#' @param groups The number of groups (1 for a single group model).
#' @param means Whether the mean structure is estimated: by default when
#'   the specification uses FIML.
#' @return `NULL` when the fit may proceed, otherwise the refusal, one
#'   string.
#' @author Ken Kelley
#' @noRd
sieve_spec_mlr_guard <- function(spec, n = NA_real_, groups = 1L, means = identical(spec$missing, "fiml")) {
  p <- length(spec$observed)
  if (!p) {
    return(NULL)
  }
  k <- max(1L, length(spec$latent))
  groups <- max(1L, suppressWarnings(as.integer(groups)), na.rm = TRUE)
  sieve_mlr_guard(p, spec$estimator,
    n = n,
    npar = groups * sieve_free_parameters(p, k, means = isTRUE(means)),
    missing = spec$missing
  )
}

#' The current revision of the inputs, as a reactive
#'
#' A module keeps results that were computed from the loaded dataset, the
#' item data, and the ledger. This reactive takes a dependency on all three
#' and returns the current `sieve_revision()`, so a display that reads it
#' is re-evaluated whenever one of them changes.
#'
#' @param state The shared state store.
#' @return A [shiny::reactive()] returning the current revision.
#' @author Ken Kelley
#' @noRd
sieve_current_revision <- function(state) {
  shiny::reactive({
    state$dataset_id
    state$items
    state$meta
    sieve_revision(state)
  })
}

#' Bind a module result to the inputs it was computed from
#'
#' The result (a list, with or without an `error`) is stamped with the
#' revision of the dataset, the item data, and the ledger at the moment it
#' was computed; `sieve_current_result()` reads the stamp.
#'
#' @param result A list.
#' @param state The shared state store.
#' @return `result` with a `revision` element.
#' @author Ken Kelley
#' @noRd
sieve_bind_result <- function(result, state) {
  if (is.null(result)) {
    return(NULL)
  }
  sieve_ensure_dataset(state)
  result$revision <- sieve_revision(state)
  result
}

#' A bound result while its inputs are current, or why it no longer applies
#'
#' A result computed from a dataset, item data, or ledger that have changed
#' since (another dataset loaded, an item reverse scored, an item included,
#' excluded, reassigned or retyped) describes a model that is no longer the
#' one on screen. It is replaced by a message that says what changed and
#' what to press, so an old table is never shown as if it were current.
#'
#' @param result A list stamped by `sieve_bind_result()`, or `NULL`.
#' @param now The current revision (see `sieve_current_revision()`).
#' @param what A short name for the result, used in the message.
#' @param action The control that computes it again.
#' @return `result` when it applies; otherwise a list with `error` (the
#'   message) and `stale = TRUE`.
#' @author Ken Kelley
#' @noRd
sieve_current_result <- function(result, now, what = "result", action = "the button above") {
  if (is.null(result)) {
    return(NULL)
  }
  why <- sieve_stale_reason(result$revision, now)
  if (is.null(why)) {
    return(result)
  }
  list(
    error = sprintf(paste(
      "The %s shown here was computed from different inputs: %s. Press %s to compute",
      "it for the current data."
    ), what, why, action),
    stale = TRUE
  )
}

#' Fit a Specified Model and Report What Happened
#'
#' Runs the lavaan model a specification describes and returns the outcome
#' as a record rather than a bare fit: the fitted object when there is one,
#' the conditions lavaan raised, and a status that distinguishes a usable
#' solution from one that failed, did not converge, is not identified, or
#' is inadmissible (see `sieve_fit_status()`).
#'
#' Two checks guard the identity of the model. Before fitting, no latent
#' variable of the specification may have the name of a column in `data`:
#' lavaan would read `x1 =~ x1 + x2 + x3` as a factor named x1 measured by
#' x2 and x3 only. After fitting, the observed and latent variables lavaan
#' actually modeled must be the ones the specification intended. A model
#' that fails either check is reported as failed, with the names concerned;
#' it is never returned as a fit.
#'
#' The data are read as numbers before the engine sees them (see
#' `sieve_numeric_columns()`): a column with values that are not numbers,
#' with no observed values, or with one value throughout is a failure that
#' names the column, never a quiet coercion to missing values. Anything the
#' engine prints to the console while fitting (its variable table, when it
#' stops on a variable) is captured and kept in `console`, and a variable it
#' flags there is named in the failure message.
#'
#' @param spec A specification from [sieve_cfa_spec()] or
#'   `sieve_model_spec()`.
#' @param data A data frame holding the observed variables under the names
#'   the specification uses.
#' @param test A lavaan test statistic to ask for by name (for example
#'   `"bollen.stine"`), or `NULL` for the engine's default for the
#'   estimator. Only a measurement model takes one.
#' @param bootstrap The number of bootstrap samples when `test` needs them.
#' @return A list of class `sieve_result` with `spec`, `fit` (a lavaan
#'   object or `NULL`), `status`, `warnings`, `messages`, `error`, and
#'   `console` (what the engine printed while fitting, usually nothing). The
#'   fit carries the attributes `sieve_spec`, `sieve_status`,
#'   `sieve_warnings`, and `name_map`.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' spec <- sieve_cfa_spec(meta, reserved = names(items))
#' run <- sieve_run_model(spec, items)
#' run$status$code
#' lavaan::fitMeasures(run$fit, c("cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"),
#'   fm.args = list(rmsea.ci.level = 0.9))
#' @export
sieve_run_model <- function(spec, data, test = NULL, bootstrap = NULL) {
  out <- list(
    spec = spec, fit = NULL, status = NULL,
    warnings = character(0), messages = character(0), error = NULL,
    console = character(0)
  )
  class(out) <- "sieve_result"
  fail <- function(code, why) {
    out$error <- why
    out$status <- sieve_fit_status(NULL, warnings = out$warnings, error = why, code = code)
    out
  }

  if (length(spec$problems)) {
    return(fail("failed", paste(spec$problems, collapse = " ")))
  }
  if (!nzchar(spec$model)) {
    return(fail("failed", paste(c(
      "The model is empty: no construct has enough included items.",
      sieve_not_modeled_text(spec)
    ), collapse = " ")))
  }
  clash <- intersect(spec$latent, names(data))
  if (length(clash)) {
    return(fail("failed", sprintf(
      paste(
        "The model names the latent variable%s %s, and the data have %s with the",
        "same name. A latent variable and an observed variable cannot share a name:",
        "the engine would take the observed variable out of the model. Rename the",
        "latent variable."
      ),
      if (length(clash) == 1L) "" else "s",
      paste(sprintf("'%s'", clash), collapse = ", "),
      if (length(clash) == 1L) "a column" else "columns"
    )))
  }
  missing_cols <- setdiff(spec$observed, names(data))
  if (length(missing_cols)) {
    return(fail("failed", sprintf(
      "The data do not contain the modeled variable(s): %s.",
      paste(missing_cols, collapse = ", ")
    )))
  }
  # The values the engine will fit, read as numbers once, here. A value that
  # cannot be read is a failure that names the column, not a missing value.
  # An external outcome column is named as one, with its own action.
  read <- sieve_numeric_columns(
    data, unique(c(spec$observed, spec$engine_args$aux)),
    what = ifelse(unique(c(spec$observed, spec$engine_args$aux)) %in% names(spec$out_map %||% character(0)), "Outcome column",
      ifelse(unique(c(spec$observed, spec$engine_args$aux)) %in% names(spec$pred_map %||% character(0)), "Antecedent column", "Item")
    )
  )
  if (nrow(read$problems)) {
    return(fail("failed", paste(read$text, collapse = " ")))
  }
  d <- read$data
  if (spec$missing %in% c("two.stage", "robust.two.stage") && length(spec$engine_args$aux)) {
    covariates <- tryCatch(lavaan::lavNames(lavaan::lavaanify(spec$model), "ov.x"), error = function(e) character())
    if (length(covariates) && !identical(spec$engine_args$fixed.x, FALSE)) {
      return(fail("failed", paste0(
        "SIEVER does not compute two-stage standard errors with auxiliaries and fixed observed covariates. ",
        "Choose the random-covariate model explicitly or turn off Robust two-stage ML to use FIML. No two-stage model was fitted."
      )))
    }
    indicators <- rowSums(!is.na(d[, spec$observed, drop = FALSE]))
    available <- rowSums(!is.na(d))
    if (any(indicators == 0L & available > 0L)) return(fail("failed", paste0(
      "SIEVER does not compute two-stage inference when some respondents are observed only on auxiliary variables. ",
      "Turn off Robust two-stage ML to use FIML with these auxiliaries; no two-stage model was fitted."
    )))
  }
  # A categorical fit whose weight matrix would not fit in memory is refused
  # here, with the size and the two ways forward, before the engine starts
  # on it (see sieve_wlsmv_guard()).
  refusal <- sieve_wlsmv_guard(spec$observed, d,
    ordered = spec$ordered %||% character(0),
    estimator = spec$estimator
  )
  if (!is.null(refusal)) {
    return(fail("failed", refusal))
  }
  args <- sieve_spec_args(spec)
  for (nm in c(args$cluster, args$sampling.weights)) {
    if (!nm %in% names(data)) return(fail("failed", paste("The design column is absent:", nm)))
    d[[nm]] <- data[[nm]]
  }
  if (!is.null(args$cluster)) {
    initialized <- sieve_capture(do.call(
      if (identical(spec$fun, "sem")) lavaan::sem else lavaan::cfa,
      c(args, list(data = d, do.fit = FALSE))
    ))
    if (!initialized$ok) return(fail("failed", initialized$error))
    cluster_check <- sieve_cluster_inference_check(initialized$value)
    if (!is.null(cluster_check$reason)) return(fail("failed", cluster_check$reason))
    if (!is.null(cluster_check$caution)) out$warnings <- c(out$warnings, cluster_check$caution)
  }
  fit_call <- function() {
    call <- c(args, list(data = d))
    if (!is.null(test)) {
      call$test <- test
      call$bootstrap <- if (is.null(bootstrap)) 1000L else as.integer(bootstrap)
    }
    do.call(if (identical(spec$fun, "sem") && is.null(test)) lavaan::sem else lavaan::cfa, call)
  }
  # What the engine prints to the console is not a condition, so it is
  # captured separately and kept; it never reaches the console.
  printed <- utils::capture.output(run <- sieve_capture(fit_call()), type = "output")
  out$console <- printed
  out$warnings <- unique(c(out$warnings, run$warnings))
  out$messages <- run$messages
  if (!run$ok) {
    return(fail("failed", sieve_engine_failure(run$error, printed)))
  }
  fit <- run$value
  # ML with FIML (the study's default, DMAR's) takes its standard errors
  # from the observed information; when that matrix cannot be inverted for
  # a converged solution, the model is fit again with the expected
  # information, which gives the same estimates. The page, the recorded R
  # and the warnings say so.
  retried <- sieve_expected_information_retry(fit, args, d,
    if (identical(spec$fun, "sem") && is.null(test)) lavaan::sem else lavaan::cfa)
  if (!is.null(retried)) {
    fit <- retried$fit
    args <- retried$args
    spec$engine_args <- utils::modifyList(spec$engine_args %||% list(), list(information = "expected"))
    if (!is.null(out$spec)) out$spec <- spec
    out$warnings <- unique(c(out$warnings, retried$note))
  }
  h1_refusal <- sieve_h1_refusal(fit)
  if (spec$missing %in% c("two.stage", "robust.two.stage") && !is.null(h1_refusal)) {
    return(fail("failed", paste(h1_refusal,
      "Two-stage estimation requires unrestricted first-stage moments. Use FIML for an eligible constrained model or review the missing-response patterns.")))
  }
  attr(fit, "sieve_h1_eligibility") <- list(reason = h1_refusal)
  changed <- sieve_membership_problem(fit, spec)
  if (!is.null(changed)) {
    return(fail("failed", changed))
  }
  out$status <- sieve_fit_status(fit, warnings = out$warnings)
  # lavaan's warning about unequal observed variances, said with its numbers
  # (after the status, which reads lavaan's words).
  out$warnings <- sieve_variance_ratio_note(out$warnings, fit, aux = spec$engine_args$aux)
  diagnostics <- sieve_missing_diagnostics(d, spec$observed, fit = fit)
  if (identical(spec$missing, "robust.two.stage")) {
    # The statistic describes this fit: the refit uses the respondents it
    # used and is compared with it (see sieve_two_stage_residual()).
    out$two_stage_residual <- sieve_two_stage_residual(spec, d, fit = fit)
  }
  out$missing_data <- diagnostics
  if (!is.null(args$cluster)) attr(fit, "sieve_bootstrap_data") <- d
  attr(fit, "sieve_missing_data") <- diagnostics
  attr(fit, "sieve_spec") <- spec
  attr(fit, "sieve_status") <- out$status
  attr(fit, "sieve_warnings") <- out$warnings
  attr(fit, "name_map") <- spec$name_map
  out$fit <- fit
  out
}

#' The engine's failure message, with the variables it flagged named
#'
#' When lavaan stops on a variable it prints its variable table to the
#' console with the rows concerned marked by three asterisks, and the error
#' itself names no variable. The marked rows are read from the captured
#' print and their names added to the message.
#'
#' @param error The engine's error message.
#' @param printed The lines the engine printed to the console.
#' @return One string.
#' @author Ken Kelley
#' @noRd
sieve_engine_failure <- function(error, printed = character(0)) {
  error <- gsub("\\s+", " ", trimws(error %||% "no reason was given"))
  marked <- regmatches(printed, regexec("^[0-9]+\\*\\*\\*[[:space:]]+([^[:space:]]+)", printed))
  flagged <- unique(vapply(
    marked, function(m) if (length(m) == 2L) m[2L] else NA_character_,
    character(1)
  ))
  flagged <- flagged[!is.na(flagged)]
  if (!length(flagged)) {
    return(error)
  }
  sprintf(
    "%s (the engine flagged %s).", sub("[.]$", "", error),
    paste(sprintf("'%s'", flagged), collapse = ", ")
  )
}

#' Fit a specified CFA and report what happened
#'
#' With a `store` (see [sieve_fit_store()]), the fit is looked up first by
#' the exact identity of what the engine would be given (see
#' [sieve_fit_identity()]): a result held under the identical identity is
#' returned as it was fit, marked `reused`, with the page and the time of
#' the fit it reuses and the seconds that fit took; otherwise the model is
#' fit, and a result with a fit is stored under its identity with the page
#' that made it. Without a store the model is fit by [sieve_run_model()],
#' with nothing looked up or stored.
#'
#' @param spec A specification from [sieve_cfa_spec()].
#' @param data A data frame holding the observed variables.
#' @param store A store from [sieve_fit_store()], or `NULL` to fit without
#'   one.
#' @param page A short name of the page (or caller) making the fit, kept
#'   with the stored result (`"CFA"`, `"Items"`, `"Validity"`,
#'   `"Reliability"`).
#' @param rows The identities of the rows of `data` (the original row
#'   numbers of the data as loaded; see `sieve_case_index()`), part of the
#'   identity; `NULL` when not tracked.
#' @return A `sieve_result` (see [sieve_run_model()]). With a store, it
#'   also carries `reused` (`TRUE` for a result taken from the store),
#'   `seconds` and `when` (the time the fit took and the clock time it was
#'   made, of the original fit for a reused one), `page` (the page that
#'   made the fit), `fit_id` (its identity), and, when reused,
#'   `reused_from` and `reused_when`; a reused result carries the
#'   `measures` and `standardized` the fitting page read from it, when it
#'   did (see `sieve_fit_store_keep()`). The caller's own specification is
#'   returned as `spec`; the fitted object is the identical one.
#' @author Ken Kelley
#' @noRd
sieve_run_cfa <- function(spec, data, store = NULL, page = NULL, rows = NULL) {
  if (is.null(store) || !is.environment(store) || length(spec$problems) || !nzchar(spec$model %||% "")) {
    return(sieve_run_model(spec, data))
  }
  id <- sieve_fit_identity(spec, data,
    rows = rows, dataset = store$dataset,
    versions = store$versions %||% sieve_engine_versions()
  )
  held <- sieve_fit_store_get(store, id)
  if (!is.null(held)) {
    held$spec <- spec
    held$reused <- TRUE
    held$reused_from <- held$page
    held$reused_when <- held$when
    return(held)
  }
  started <- proc.time()[["elapsed"]]
  run <- sieve_run_model(spec, data)
  run$seconds <- proc.time()[["elapsed"]] - started
  run$when <- format(Sys.time(), "%H:%M")
  run$page <- if (is.null(page) || !length(page)) NA_character_ else as.character(page)[1L]
  run$fit_id <- id
  run$reused <- FALSE
  if (!is.null(run$fit)) {
    sieve_fit_store_put(store, id, run)
  }
  run
}

#' Does a fitted model hold the variables its specification intended?
#'
#' Compares the observed and latent variables lavaan modeled with the
#' intended sets of the specification.
#'
#' @param fit A fitted lavaan object.
#' @param spec The specification it was fit from.
#' @return `NULL` when they agree, otherwise a message naming every
#'   variable that is missing from the fitted model or was not intended.
#' @author Ken Kelley
#' @noRd
sieve_membership_problem <- function(fit, spec) {
  got_ov <- tryCatch(setdiff(lavaan::lavNames(fit, "ov"), spec$engine_args$aux), error = function(e) NULL)
  got_lv <- tryCatch(lavaan::lavNames(fit, "lv"), error = function(e) NULL)
  if (is.null(got_ov) || is.null(got_lv)) {
    return("The variables of the fitted model could not be read, so the model cannot be confirmed to be the one specified.")
  }
  say <- function(names, one, several) {
    if (!length(names)) {
      return(NULL)
    }
    sprintf("%s: %s", if (length(names) == 1L) one else several, paste(names, collapse = ", "))
  }
  parts <- c(
    say(
      setdiff(spec$observed, got_ov),
      "intended observed variable not in the fitted model",
      "intended observed variables not in the fitted model"
    ),
    say(
      setdiff(got_ov, spec$observed),
      "observed variable in the fitted model that was not intended",
      "observed variables in the fitted model that were not intended"
    ),
    say(
      setdiff(spec$latent, got_lv),
      "intended latent variable not in the fitted model",
      "intended latent variables not in the fitted model"
    ),
    say(
      setdiff(got_lv, spec$latent),
      "latent variable in the fitted model that was not intended",
      "latent variables in the fitted model that were not intended"
    )
  )
  if (!length(parts)) {
    return(NULL)
  }
  paste0(
    "The fitted model is not the model that was specified (",
    paste(parts, collapse = "; "),
    "). The result was discarded."
  )
}

#' lavaan's warnings about the observed variances, with their numbers
#'
#' lavaan checks the scales of the observed variables before it fits and
#' gives two warnings, each pointing to `lavaan::varTable()`, which read as a
#' fault in the model. "Some observed variances are (at least) a factor 1000
#' times larger than others" is about their ratio: in the bundled example it
#' is the auxiliary variable, the age in years (variance 1.04), beside the
#' test scores (up to 1,319.83). "Some observed variances are larger than
#' 1000000" is about their size, with any ratio: items recorded as response
#' times in milliseconds, all on one scale, draw it alone. Each warning
#' lavaan gave is replaced by its own clause of one sentence, read from
#' `lavaan::varTable()`: for the ratio, the factor and the variables with the
#' largest and the smallest variance (naming an auxiliary variable as one);
#' for the size, how many variances exceed 1,000,000 and the largest. The
#' sentence then says whether the fit converged and what the warning means
#' for the result. Each clause keeps words `sieve_variance_warning_kinds()`
#' reads, so `sieve_fit_status()` reads it as it reads lavaan's warning.
#'
#' @param warnings The warnings of a fit.
#' @param fit The lavaan fit.
#' @param aux The fit's auxiliary variables, if any.
#' @return The warnings, with lavaan's variance warnings replaced by the
#'   sentence (at the place of the first).
#' @author Ken Kelley
#' @noRd
sieve_variance_ratio_note <- function(warnings, fit, aux = NULL) {
  warnings <- as.character(warnings %||% character(0))
  kinds <- sieve_variance_warning_kinds(warnings)
  hit <- kinds$ratio | kinds$size
  if (!any(hit) || !inherits(fit, "lavaan")) {
    return(warnings)
  }
  ratio <- any(kinds$ratio)
  size <- any(kinds$size)
  vt <- tryCatch(lavaan::varTable(fit), error = function(e) NULL)
  if (!is.data.frame(vt)) {
    return(warnings)
  }
  vt <- vt[vt$type %in% "numeric" & is.finite(vt$var) & vt$var > 0, , drop = FALSE]
  if (nrow(vt) < if (ratio) 2L else 1L) {
    return(warnings)
  }
  hi <- which.max(vt$var)
  lo <- which.min(vt$var)
  who <- function(i) {
    sprintf(
      "%s, %s%s", vt$name[i], formatC(vt$var[i], format = "f", digits = 2, big.mark = ","),
      if (vt$name[i] %in% aux) ", an auxiliary variable" else ""
    )
  }
  big <- sum(vt$var > 1e6)
  clauses <- c(
    if (ratio) {
      sprintf(
        "the observed variances are unequal by a factor of %s (largest %s; smallest %s)",
        formatC(vt$var[hi] / vt$var[lo], format = "f", digits = 0, big.mark = ","), who(hi), who(lo)
      )
    },
    if (size) {
      sprintf(
        "the observed variances are larger than 1,000,000 for %d of the %d variable%s%s", big, nrow(vt),
        if (nrow(vt) == 1L) "" else "s", if (ratio) "" else sprintf(" (largest %s)", who(hi))
      )
    }
  )
  converged <- isTRUE(tryCatch(lavaan::lavInspect(fit, "converged"), error = function(e) FALSE))
  text <- sprintf(
    paste0(
      "lavaan's numerical check: %s. ",
      "%sSIEVER checks the solution for admissibility itself, and dividing a variable by a constant%s changes no ",
      "standardized estimate."
    ),
    paste(clauses, collapse = ", and "), if (converged) "The fit converged. " else "",
    if (size) " (response times in milliseconds expressed in seconds, say)" else ""
  )
  first <- which(hit)[1L]
  keep <- !hit
  keep[first] <- TRUE
  warnings[first] <- text
  warnings[keep]
}

#' Which of lavaan's two variance warnings a set of warnings holds
#'
#' lavaan's warning about the ratio of the observed variances ("some
#' observed variances are (at least) a factor 1000 times larger than
#' others") and about their size ("some observed variances are larger than
#' 1000000"), read in lavaan's words or in the clauses of
#' `sieve_variance_ratio_note()` that replace them. Both say "observed
#' variances are", so that phrase alone cannot tell them apart.
#'
#' @param warnings Warning messages.
#' @return A list of two logical vectors along `warnings`: `ratio` and `size`.
#' @author Ken Kelley
#' @noRd
sieve_variance_warning_kinds <- function(warnings) {
  w <- gsub("\\s+", " ", as.character(warnings %||% character(0)))
  has <- function(x) grepl(x, w, fixed = TRUE)
  list(
    ratio = has("observed variances are (at least) a factor") | has("observed variances are unequal by a factor"),
    size = has("observed variances are larger than 1000000") | has("observed variances are larger than 1,000,000")
  )
}

#' The status a fitted model carries, or the status read from it now
#'
#' @param fit A fitted lavaan object (from any SIEVER wrapper or not), a
#'   `sieve_result`, or `NULL`.
#' @return A status list (see `sieve_fit_status()`).
#' @author Ken Kelley
#' @noRd
sieve_status_of <- function(fit) {
  if (inherits(fit, "sieve_result")) {
    return(fit$status)
  }
  if (is.null(fit)) {
    return(sieve_fit_status(NULL))
  }
  attr(fit, "sieve_status") %||%
    sieve_fit_status(fit, warnings = attr(fit, "sieve_warnings") %||% character(0))
}

#' Does a fitted model have a solution to report?
#'
#' Estimates, standard errors, fit indices, scores, and anything derived from
#' them describe a solution. A model that failed, did not converge, or is not
#' identified has none, so nothing is reported from it; an improper
#' (inadmissible) solution is still a solution, and is reported with its
#' status.
#'
#' @param x A fitted model, a `sieve_result`, or a status list.
#' @return `TRUE` when the status is `ok` or `inadmissible`.
#' @author Ken Kelley
#' @noRd
sieve_has_solution <- function(x) {
  status <- if (is.list(x) && !inherits(x, "sieve_result") && !is.null(x$code)) x else sieve_status_of(x)
  isTRUE(status$code %in% c("ok", "inadmissible"))
}

#' What a specification leaves out of its model, in words
#'
#' @param spec A specification (or anything with a `not_modeled` record).
#' @return A character vector, one sentence per construct, facet, or set of
#'   items left out (possibly empty).
#' @author Ken Kelley
#' @noRd
sieve_not_modeled_text <- function(spec) {
  nm <- spec$not_modeled
  if (is.null(nm) || !nrow(nm)) {
    return(character(0))
  }
  vapply(seq_len(nrow(nm)), function(i) {
    several <- nm$n_items[i] != 1L
    what <- switch(nm$level[i],
      construct = sprintf("Construct '%s'", nm$construct[i]),
      facet = sprintf("Facet '%s' of construct '%s'", nm$facet[i], nm$construct[i]),
      if (is.na(nm$construct[i])) {
        if (several) "Some items" else "An item"
      } else {
        sprintf("%s of construct '%s'", if (several) "Items" else "An item", nm$construct[i])
      }
    )
    sprintf(
      "%s %s not in the model: %s%s.", what,
      if (identical(nm$level[i], "item") && several) "are" else "is", nm$reason[i],
      if (nzchar(nm$items[i])) sprintf(" (%s)", nm$items[i]) else ""
    )
  }, character(1))
}

#' What to do about an engine failure, when the message is a known one
#'
#' lavaan's messages name the variable concerned but not the remedy. For
#' the failures a researcher can fix from inside the app, the item and the
#' one action that fixes it are stated: a category of an ordered item that
#' no case in one group uses (the thresholds of that group cannot be
#' estimated), and a correlation or covariance matrix of the items that is
#' not positive definite.
#'
#' @param error The engine's message.
#' @return One sentence, or `NULL` when the message is not a known one.
#' @author Ken Kelley
#' @noRd
sieve_failure_hint <- function(error) {
  if (is.null(error) || !length(error) || is.na(error[1L])) {
    return(NULL)
  }
  msg <- gsub("\\s+", " ", paste(error, collapse = " "))
  empty <- regmatches(msg, regexec(
    "some categories of variable [`'\"]([^`'\"]+)[`'\"] are empty in group ([^;]+);", msg
  ))[[1L]]
  if (length(empty) == 3L) {
    return(sprintf(paste(
      "Item '%s' has a response category that no case in group %s uses, so that group's",
      "thresholds cannot be estimated. Collapse that category with a neighboring one before",
      "loading the data (or choose a grouping variable whose groups all use every category",
      "of '%s')."
    ), empty[2L], trimws(empty[3L]), empty[2L]))
  }
  if (grepl("not positive[- ]definite|could not compute the polychoric", msg, ignore.case = TRUE)) {
    return(paste(
      "The correlation matrix of the items is not positive definite, which happens when two",
      "items are nearly identical or an item has almost no variance; exclude one item of",
      "such a pair on the Items tab."
    ))
  }
  NULL
}

#' The status of a model, as the interface states it
#'
#' One alert per thing the reader must know before reading an estimate:
#' that the model failed (with the engine's reason), did not converge, is
#' not identified, or is improper; that it is just identified; what it
#' leaves out; and the warnings the engine raised. Nothing is returned for
#' a model that is ok, leaves nothing out, and raised no warning.
#'
#' @param result A `sieve_result`, a fitted model carrying a status, or a
#'   status list.
#' @param what A short name for the model, used in the text.
#' @param warnings Warnings to show; taken from `result` when `NULL`.
#' @param spec The specification, for the record of what is left out; taken
#'   from `result` when `NULL`.
#' @return A [shiny::tagList()] of alerts, or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_status_alert <- function(result, what = "model", warnings = NULL, spec = NULL) {
  if (inherits(result, "sieve_result")) {
    status <- result$status
    warnings <- warnings %||% result$warnings
    spec <- spec %||% result$spec
  } else if (is.list(result) && !is.null(result$code)) {
    status <- result
  } else {
    status <- sieve_status_of(result)
    warnings <- warnings %||% attr(result, "sieve_warnings")
    spec <- spec %||% attr(result, "sieve_spec")
  }
  warnings <- warnings %||% status$warnings %||% character(0)
  cap <- function(x) paste0(toupper(substring(x, 1L, 1L)), substring(x, 2L))
  body <- switch(status$code,
    failed = shiny::div(
      class = "alert alert-danger",
      shiny::strong(sprintf("The %s could not be fit. ", what)), status$message,
      if (!is.null(sieve_failure_hint(status$message))) {
        paste0(" ", sieve_failure_hint(status$message))
      }
    ),
    not_converged = shiny::div(
      class = "alert alert-danger",
      shiny::strong(sprintf("The %s did not converge. ", what)), status$message,
      " The estimates, standard errors, and fit indices are not interpretable."
    ),
    not_identified = shiny::div(
      class = "alert alert-danger",
      shiny::strong(sprintf("The %s is not identified. ", what)), status$message,
      " The estimates, standard errors, and fit indices are not interpretable."
    ),
    inadmissible = shiny::div(
      class = "alert alert-warning",
      shiny::strong("Improper solution. "), status$message,
      " The model converged, but its estimates are not fully interpretable."
    ),
    if (isTRUE(status$just_identified)) {
      shiny::div(
        class = "alert alert-info small",
        shiny::strong(sprintf("%s: just identified. ", cap(what))),
        "The model has zero degrees of freedom, so there is no ordinary chi square test of fit. ",
        "Saturated-model fit conventions apply only when exact fit is established and supply ",
        "no evidence of approximate fit."
      )
    }
  )
  out <- list(body)
  left_out <- if (!is.null(spec)) sieve_not_modeled_text(spec) else character(0)
  if (length(left_out)) {
    out <- c(out, list(shiny::div(
      class = "alert alert-warning small",
      paste(left_out, collapse = " ")
    )))
  }
  if (length(warnings)) {
    out <- c(out, list(shiny::tags$details(
      class = "alert alert-secondary small",
      shiny::tags$summary(sprintf(
        "The engine raised %d warning%s while fitting this %s",
        length(warnings), if (length(warnings) == 1L) "" else "s", what
      )),
      shiny::tags$ul(lapply(gsub("\\s+", " ", warnings), shiny::tags$li))
    )))
  }
  out <- Filter(Negate(is.null), out)
  if (length(out)) do.call(shiny::tagList, out) else NULL
}

#' The status of a fitted latent variable model
#'
#' A fitted object is not a certificate. This reads from lavaan what a
#' result needs to state before it is interpreted:
#'
#' * `failed`: no fitted object (the reason is kept);
#' * `not_converged`: the optimizer did not converge;
#' * `not_identified`: negative degrees of freedom, an information matrix
#'   that could not be inverted (so standard errors do not exist), or an
#'   information matrix that is singular although lavaan returned standard
#'   errors from it (see `sieve_information_check()`);
#' * `inadmissible`: converged, but with a negative variance or a latent or
#'   residual covariance matrix that is not positive definite;
#' * `ok`: none of the above. This is not a claim that the model fits or
#'   that it is correctly specified.
#'
#' Zero degrees of freedom is not an error (a just identified model has
#' estimates and standard errors), but its test of fit carries no
#' information; `just_identified` records it so displays can say so.
#'
#' Identification is judged for the model as a whole, from what the engine
#' could and could not compute. No count of indicators is applied: a factor
#' with two indicators is accepted when the model that contains it is
#' identified (through a covariance with another factor, or an equality
#' constraint), and a model with any number of indicators is refused when
#' it is not (two orthogonal factors measured by the same items, for
#' example, have a rotation the data cannot determine).
#'
#' @param fit A fitted lavaan object, or `NULL`.
#' @param warnings Character vector of warnings raised while fitting.
#' @param error The failure message when `fit` is `NULL`.
#' @param code Status code to use when `fit` is `NULL`.
#' @return A list with `code`, `message`, `converged`, `df`,
#'   `just_identified`, `se_available`, `information_singular` and
#'   `information_min_eigen` (see `sieve_information_check()`),
#'   `vcov_singular` (see `sieve_vcov_singular()`; a caution, not a status),
#'   `admissible`, `negative_variances` (names of the variables concerned),
#'   and `warnings`.
#' @author Ken Kelley
#' @noRd
sieve_fit_status <- function(fit, warnings = character(0), error = NULL, code = "failed") {
  status <- list(
    code = code, message = error %||% "No fitted model.",
    converged = FALSE, df = NA_real_, just_identified = NA,
    se_available = FALSE, information_singular = NA, information_min_eigen = NA_real_,
    vcov_singular = NA, admissible = NA,
    negative_variances = character(0), boundary_variances = character(0), boundary_factors = character(0),
    warnings = warnings
  )
  if (is.null(fit)) {
    return(status)
  }
  inspect <- function(what, default) {
    tryCatch(lavaan::lavInspect(fit, what), error = function(e) default)
  }
  status$converged <- isTRUE(inspect("converged", FALSE))
  status$df <- tryCatch(unname(lavaan::fitMeasures(fit, "df")), error = function(e) NA_real_)
  if (!is.finite(status$df)) {
    # fitMeasures() declines when the model did not converge; the degrees of
    # freedom are still a property of the specification.
    pt <- tryCatch(lavaan::parTable(fit), error = function(e) NULL)
    nstat <- tryCatch(sum(lengths(lavaan::lavTech(fit, "wls.obs"))), error = function(e) NA_real_)
    if (!is.null(pt) && is.finite(nstat)) {
      status$df <- nstat - sum(pt$free > 0L & !duplicated(pt$free))
    }
  }
  status$just_identified <- is.finite(status$df) && status$df == 0
  pe <- tryCatch(lavaan::parameterEstimates(fit, ci = FALSE, standardized = FALSE),
    error = function(e) NULL
  )
  pt <- tryCatch(lavaan::parTable(fit), error = function(e) NULL)
  if (!is.null(pe) && !is.null(pt)) {
    free <- pt$free[match(
      paste(pe$lhs, pe$op, pe$rhs, pe$group %||% 1L),
      paste(pt$lhs, pt$op, pt$rhs, pt$group %||% 1L)
    )] > 0L
    free[is.na(free)] <- FALSE
    status$se_available <- "se" %in% names(pe) && any(free) && all(is.finite(pe$se[free]))
    var_rows <- pe$op == "~~" & pe$lhs == pe$rhs & is.finite(pe$est) & pe$est < 0
    status$negative_variances <- unique(pe$lhs[var_rows])
    # A free variance held at its lower bound (lavaan::efa() bounds every
    # variance at zero, bounds = "pos.var") is a Heywood case as surely as
    # a negative one: the estimate went as far as it was allowed and stopped
    # there. A variance fixed at zero is not free and is not counted.
    lower <- if ("lower" %in% names(pt)) {
      pt$lower[match(paste(pe$lhs, pe$op, pe$rhs, pe$group %||% 1L), paste(pt$lhs, pt$op, pt$rhs, pt$group %||% 1L))]
    } else {
      rep(NA_real_, nrow(pe))
    }
    bound <- ifelse(is.finite(lower), pmax(lower, 0), 0)
    at_bound <- pe$op == "~~" & pe$lhs == pe$rhs & free & is.finite(pe$est) & pe$est >= 0 & pe$est <= bound + 1e-8
    status$boundary_variances <- setdiff(unique(pe$lhs[at_bound]), status$negative_variances)
  }
  # A factor that the factors above it explain entirely: with its
  # disturbance variance fixed at 1 (factor variances at 1), the boundary
  # shows as a loading on the higher factor running off without bound, not
  # as a variance at zero, so the variance checks above cannot see it
  # (sieve_boundary_factors()).
  status$boundary_factors <- if (status$converged) sieve_boundary_factors(fit) else character(0)
  post_ok <- sieve_model_post_check(fit)
  status$admissible <- status$converged && post_ok && !length(status$negative_variances) &&
    !length(status$boundary_variances) && !length(status$boundary_factors)
  information <- sieve_information_check(fit)
  status$information_singular <- information$singular
  status$information_min_eigen <- information$min_eigen
  status$vcov_singular <- sieve_vcov_singular(fit)
  # Very unequal scales can by themselves stop the information matrix from
  # being inverted numerically; lavaan says when it sees them, and the reader
  # is told. (The singularity check above is made on a common scale, so this
  # does not apply to it.)
  # lavaan's two warnings are told apart: variances a factor of 1000 apart
  # call for similar scales, variances over 1,000,000 for smaller ones.
  kinds <- lapply(sieve_variance_warning_kinds(warnings), any)
  scale_hint <- if (kinds$ratio || kinds$size) {
    paste0(
      " lavaan also reports observed variances ",
      if (kinds$ratio && kinds$size) {
        "that differ by a factor of 1000 or more, some larger than 1,000,000"
      } else if (kinds$ratio) {
        "that differ by a factor of 1000 or more"
      } else {
        "larger than 1,000,000"
      },
      ", which can cause this on its own; ",
      if (kinds$ratio && kinds$size) {
        "putting the variables on similar, smaller scales"
      } else if (kinds$ratio) {
        "putting the variables on similar scales"
      } else {
        "dividing the variables by a constant"
      },
      " may resolve it."
    )
  } else {
    ""
  }
  if (!status$converged) {
    status$code <- "not_converged"
    status$message <- "The optimizer did not converge; the estimates are not a solution."
  } else if (is.finite(status$df) && status$df < 0) {
    status$code <- "not_identified"
    status$message <- sprintf(paste0(
      "The model has %d more free parameters than the data supply ",
      "(negative degrees of freedom), so it is not identified."
    ), -as.integer(status$df))
  } else if (!status$se_available) {
    status$code <- "not_identified"
    status$message <- paste0(
      "Standard errors could not be computed (the information matrix could not be ",
      "inverted), which indicates a model that is not identified or is empirically ",
      "underidentified in these data.", scale_hint
    )
  } else if (length(status$boundary_factors)) {
    # Named before the singularity it causes, so ML and MLR say the same
    # thing about the same solution (on the 24 tests as a higher-order
    # model, MLR's information matrix falls below the cutoff and ML's does
    # not, for one and the same boundary).
    status$code <- "inadmissible"
    who <- as.character(status$boundary_factors)
    above <- attr(status$boundary_factors, "above")
    one <- length(who) == 1L
    status$message <- sprintf(
      paste0(
        "The solution is improper (a boundary case): %s %s explained entirely by %s (R-square %s). ",
        "In these data %s cannot be distinguished from %s, and %s on %s %s off without bound, so the ",
        "structure is empirically underidentified. Fit the correlated-factors model, or a structure ",
        "without that path (Chen, Bollen, Paxton, Curran & Kirby, 2001)."
      ),
      paste(who, collapse = " and "), if (one) "is" else "are", paste(above, collapse = " and "),
      paste(sprintf("%.6f", attr(status$boundary_factors, "rsquare")), collapse = " and "),
      if (one) "it" else "they", paste(above, collapse = " and "), if (one) "its loading" else "their loadings",
      paste(above, collapse = " and "), if (one) "runs" else "run"
    )
  } else if (isTRUE(status$information_singular)) {
    status$code <- "not_identified"
    status$message <- paste0(
      "The information matrix is singular, so the data do not determine every parameter ",
      "of this model: it is not identified, or is empirically underidentified in these ",
      "data. The standard errors lavaan computed are not valid."
    )
  } else if (!status$admissible) {
    status$code <- "inadmissible"
    status$message <- if (length(status$negative_variances)) {
      sprintf(
        "The solution is improper: negative variance estimate for %s.",
        paste(status$negative_variances, collapse = ", ")
      )
    } else if (length(status$boundary_variances)) {
      sprintf(
        paste0(
          "The solution is improper (a Heywood case): the variance estimate of %s is at its ",
          "lower bound of zero, where lavaan held it."
        ),
        paste(status$boundary_variances, collapse = ", ")
      )
    } else {
      "The solution is improper: a model covariance matrix is not positive definite."
    }
  } else {
    status$code <- "ok"
    status$message <- if (status$just_identified) {
      "Converged and admissible. The model is just identified (zero degrees of freedom), so its fit cannot be tested."
    } else {
      "Converged and admissible."
    }
  }
  cluster_check <- sieve_cluster_inference_check(fit)
  status$clusters <- cluster_check$clusters
  status$free_parameter_directions <- cluster_check$parameters
  if (status$code %in% c("ok", "inadmissible") &&
    (!is.null(cluster_check$reason) ||
      (is.finite(cluster_check$clusters) && isTRUE(status$vcov_singular)))) {
    status$code <- "failed"
    status$message <- cluster_check$reason %||% paste(
      "The cluster sandwich covariance is rank deficient despite an identified measurement model.",
      "Cluster-robust standard errors and joint tests are unavailable; inspect the number and diversity of independent clusters."
    )
  } else if (status$code %in% c("ok", "inadmissible") && isTRUE(status$vcov_singular)) {
    status$message <- paste(
      status$message,
      "The covariance matrix of the estimates is singular although the model is identified.",
      "With robust standard errors this can follow from dependent casewise scores or too few independent observations.",
      "Joint parameter tests require a nonsingular covariance."
    )
  }
  if (!is.null(cluster_check$caution)) {
    status$warnings <- unique(c(status$warnings, cluster_check$caution))
    status$message <- paste(status$message, cluster_check$caution)
  }
  status
}

#' The status of an exploratory factor analysis from psych::fa
#'
#' The exploratory counterpart of `sieve_fit_status()`, with the same codes.
#' psych::fa returns an object in circumstances where the solution should
#' not be read as a result: with more factors than the variables can
#' identify (negative degrees of freedom), with a rotation that did not
#' converge, or with a Heywood case. psych's extraction holds every
#' uniqueness at or above 0.005 (the `lower = 0.005` bound of its
#' optimizer, for maximum likelihood, minres and the other iterative
#' methods), so a Heywood case usually appears as a uniqueness at that
#' bound (a communality of 0.995) rather than as a communality of one or
#' more; both are inadmissible.
#'
#' @param fa A psych::fa result, or `NULL`.
#' @param warnings Character vector of warnings raised while fitting.
#' @param error The failure message when `fa` is `NULL`.
#' @param bound psych's lower bound on the uniquenesses.
#' @return A list with `code` (`failed`, `not_converged`, `not_identified`,
#'   `inadmissible`, or `ok`), `message`, `df`, `just_identified`,
#'   `heywood` (the items concerned: a communality at or above one, or a
#'   uniqueness at the bound), `at_bound` (those at the bound), and
#'   `warnings`.
#' @author Ken Kelley
#' @noRd
sieve_efa_status <- function(fa, warnings = character(0), error = NULL, bound = 0.005) {
  status <- list(
    code = "failed", message = error %||% "No fitted model.",
    df = NA_real_, just_identified = NA, heywood = character(0), at_bound = character(0),
    warnings = warnings
  )
  if (is.null(fa)) {
    return(status)
  }
  status$df <- if (is.null(fa$dof)) NA_real_ else as.numeric(fa$dof)
  status$just_identified <- is.finite(status$df) && status$df == 0
  h2 <- fa$communality
  if (!is.null(h2)) {
    status$heywood <- names(h2)[is.finite(h2) & h2 >= 1]
  }
  u2 <- fa$uniquenesses
  if (!is.null(u2)) {
    # At the bound within the optimizer's tolerance (0.004999893 was seen).
    status$at_bound <- setdiff(names(u2)[is.finite(u2) & u2 <= bound + 1e-6], status$heywood)
    status$heywood <- c(status$heywood, status$at_bound)
  }
  not_converged <- any(grepl("convergence not obtained|did not converge|not converge",
    warnings,
    ignore.case = TRUE
  ))
  if (is.finite(status$df) && status$df < 0) {
    status$code <- "not_identified"
    status$message <- sprintf(
      paste0(
        "This many factors cannot be identified from this many items (the model has %d ",
        "more free parameters than the correlations supply). Extract fewer factors."
      ),
      -as.integer(status$df)
    )
  } else if (not_converged) {
    status$code <- "not_converged"
    status$message <- "The extraction or the rotation did not converge; the loadings are not a solution."
  } else if (length(status$heywood) ||
    any(grepl("Heywood", warnings, ignore.case = TRUE))) {
    status$code <- "inadmissible"
    above <- setdiff(status$heywood, status$at_bound)
    status$message <- if (length(status$heywood)) {
      paste(c(
        if (length(above)) sprintf(
          "The solution is improper (a Heywood case): the communality of %s is at or above one.",
          paste(above, collapse = ", ")
        ),
        if (length(status$at_bound)) sprintf(
          paste0(
            "The solution is improper (a Heywood case): the uniqueness of %s is at psych's lower ",
            "bound of %s (a communality of %s), where the extraction held it; the estimate would go lower."
          ),
          paste(status$at_bound, collapse = ", "), format(bound), format(1 - bound)
        )
      ), collapse = " ")
    } else {
      "The solution is improper (a Heywood case was detected)."
    }
  } else {
    status$code <- "ok"
    status$message <- if (status$just_identified) {
      "The solution has zero degrees of freedom, so its fit cannot be tested."
    } else {
      "The solution is admissible."
    }
  }
  status
}

#' The factors a fitted model explains entirely by other factors
#'
#' A factor that loads on a higher factor (a first-order factor of a
#' higher-order model) or is regressed on other factors, and whose R-square
#' on them is .999 or more: it is not distinguishable from the factors above
#' it in these data. Under the variance identification the disturbance
#' variance of such a factor is fixed at 1, so the boundary appears as a
#' loading or path running off without bound (and an information matrix
#' near singular in that direction) rather than as a variance at zero, the
#' boundary the variance checks of `sieve_fit_status()` read; it is an
#' improper solution all the same (Chen, Bollen, Paxton, Curran & Kirby,
#' 2001, on improper solutions and empirical underidentification). On the
#' 24 Holzinger and Swineford tests as a higher-order model the Reasoning
#' factor reaches R-square .9997 under ML (its loading 58, with a standard
#' error of 6212) and .999998 under MLR.
#'
#' @param fit A fitted lavaan object.
#' @param cutoff The R-square at or above which a factor is at the boundary.
#' @return A character vector of factor names (possibly empty) with the
#'   attributes `rsquare` (their R-squares) and `above` (the factors they
#'   load on or are regressed on).
#' @author Ken Kelley
#' @noRd
sieve_boundary_factors <- function(fit, cutoff = 0.999) {
  none <- character(0)
  pt <- tryCatch(lavaan::parTable(fit), error = function(e) NULL)
  lv <- tryCatch(lavaan::lavNames(fit, "lv"), error = function(e) character(0))
  if (is.null(pt) || !length(lv)) return(none)
  below <- pt$op == "=~" & pt$lhs %in% lv & pt$rhs %in% lv
  regressed <- pt$op == "~" & pt$lhs %in% lv & pt$rhs %in% lv
  endogenous <- unique(c(pt$rhs[below], pt$lhs[regressed]))
  if (!length(endogenous)) return(none)
  r2 <- tryCatch(lavaan::lavInspect(fit, "rsquare"), error = function(e) NULL)
  if (is.null(r2)) return(none)
  values <- if (is.list(r2)) unlist(unname(r2)) else r2
  hit <- names(values) %in% endogenous & is.finite(values) & values >= cutoff
  if (!any(hit)) return(none)
  out <- unique(names(values)[hit])
  attr(out, "rsquare") <- vapply(out, function(f) max(values[names(values) == f]), numeric(1))
  attr(out, "above") <- unique(c(pt$lhs[below & pt$rhs %in% out], pt$rhs[regressed & pt$lhs %in% out]))
  out
}

#' Is the information matrix of a fitted model singular?
#'
#' A model is locally identified when its expected information matrix is
#' nonsingular (Rothenberg, 1971), once the directions removed by equality
#' constraints are set aside. lavaan can return standard errors from a
#' singular information matrix (it falls back on a generalized inverse), so
#' the matrix is examined directly: it is restricted to the parameter space
#' the equality constraints leave, put on a common scale (unit diagonal, so
#' the units of the variables do not matter), and its smallest eigenvalue is
#' compared with zero. On that scale identified models have a smallest
#' eigenvalue well above 1e-8 and unidentified ones one near 1e-16; the
#' cutoff is 1e-10.
#'
#' The check is not made (`singular` is `NA`) for exploratory blocks, whose
#' rotation constraints are not in the parameter table, for models with
#' inequality or nonlinear constraints, and when lavaan cannot supply the
#' matrix.
#'
#' @param fit A fitted lavaan object.
#' @return A list with `singular` (`TRUE`, `FALSE`, or `NA`) and `min_eigen`
#'   (the smallest eigenvalue on the common scale, or `NA`).
#' @author Ken Kelley
#' @noRd
sieve_information_check <- function(fit) {
  out <- list(singular = NA, min_eigen = NA_real_)
  # The model is read first: for an exploratory block (or inequality
  # constraints) the check does not apply, and the information matrix,
  # seconds to form for a large model, is not computed only to be set aside.
  pt <- tryCatch(lavaan::parTable(fit), error = function(e) NULL)
  if (is.null(pt) || any(pt$op %in% c("<", ">")) ||
    ("efa" %in% names(pt) && any(nzchar(pt$efa)))) {
    return(out)
  }
  info <- sieve_capture(lavaan::lavInspect(fit, "information.expected"))$value
  if (is.null(info) || !is.matrix(info) || !nrow(info) || any(!is.finite(info))) {
    return(out)
  }
  npar <- nrow(info)
  basis <- diag(npar)
  equal <- pt[pt$op == "==", , drop = FALSE]
  if (nrow(equal)) {
    # Each simple equality (one parameter equals another) removes the
    # direction in which the two differ. Anything else is left unchecked.
    left <- pt$free[match(equal$lhs, pt$plabel)]
    right <- pt$free[match(equal$rhs, pt$plabel)]
    if (anyNA(left) || anyNA(right) || any(left < 1L) || any(right < 1L) ||
      any(left > npar) || any(right > npar)) {
      return(out)
    }
    jac <- matrix(0, nrow(equal), npar)
    jac[cbind(seq_len(nrow(equal)), left)] <- 1
    jac[cbind(seq_len(nrow(equal)), right)] <- jac[cbind(seq_len(nrow(equal)), right)] - 1
    dec <- svd(jac, nu = 0, nv = npar)
    rank <- sum(dec$d > 1e-10)
    if (rank >= npar) {
      return(out)
    }
    basis <- dec$v[, seq_len(npar)[-seq_len(rank)], drop = FALSE]
  }
  reduced <- crossprod(basis, info %*% basis)
  scale <- diag(reduced)
  if (any(scale <= 0)) {
    out$singular <- TRUE
    out$min_eigen <- 0
    return(out)
  }
  ev <- eigen(reduced / sqrt(scale %o% scale), symmetric = TRUE, only.values = TRUE)$values
  out$min_eigen <- min(ev)
  out$singular <- out$min_eigen < 1e-10
  out
}

#' Is the covariance matrix of the estimates singular?
#'
#' Separate from identification. A robust (sandwich) covariance matrix can
#' be singular for an identified model: when a variable treated as
#' continuous takes only two values, or when there are fewer cases than
#' free parameters, the casewise scores are linearly dependent. Individual
#' standard errors are still computed, but a test that involves several
#' parameters at once rests on a matrix that is not positive definite. The
#' matrix is examined in correlation form, and each equality constraint
#' accounts for one zero eigenvalue. Not applied to exploratory fits.
#'
#' @param fit A fitted lavaan object.
#' @return `TRUE`, `FALSE`, or `NA` when the matrix is unavailable or the
#'   check does not apply.
#' @author Ken Kelley
#' @noRd
sieve_vcov_singular <- function(fit) {
  # Exploratory blocks first: the check does not apply to them, so the
  # covariance matrix of the estimates is not formed only to be discarded.
  pt <- tryCatch(lavaan::parTable(fit), error = function(e) NULL)
  if (is.null(pt) || ("efa" %in% names(pt) && any(nzchar(pt$efa)))) {
    return(NA)
  }
  v <- sieve_capture(lavaan::lavInspect(fit, "vcov"))$value
  if (is.null(v) || !is.matrix(v) || !nrow(v) || any(!is.finite(v))) {
    return(NA)
  }
  variance <- diag(v)
  if (any(variance < 0)) {
    return(TRUE)
  }
  free <- variance > 0
  ev <- if (sum(free) > 1L) {
    eigen(stats::cov2cor(v[free, free, drop = FALSE]), symmetric = TRUE, only.values = TRUE)$values
  } else {
    rep(1, sum(free))
  }
  n_zero <- sum(abs(ev) < 1e-10) + sum(!free) - sum(pt$op == "==")
  any(ev < -1e-8) || n_zero > 0L
}

#' Record an analysis in the session's analysis registry
#'
#' Each distinct analysis (a specification applied to particular inputs) is
#' recorded once, with an identifier, the inputs it used, and its status.
#' Recording the same specification on the same inputs again returns the
#' existing identifier, so a reactive that recomputes does not create
#' duplicates.
#'
#' @param state The shared state store.
#' @param result A `sieve_result` (see `sieve_run_cfa()`).
#' @param note A short plain language description.
#' @return The analysis identifier (for example `"A3"`), invisibly.
#' @author Ken Kelley
#' @noRd
sieve_register_analysis <- function(state, result, note = NULL) {
  existing <- shiny::isolate(state$analyses) %||% list()
  revision <- result$revision %||% sieve_revision(state)
  captured <- result$inputs %||% sieve_replay_inputs(state)
  input_key <- sieve_hash(captured)
  engine_identity <- result$engine_identity %||% sieve_engine_identity()
  estimates <- result$estimates %||% if (!is.null(result$fit) && sieve_has_solution(result$status %||% result$fit)) {
    list(fit = sieve_fit_measures(result$fit),
      parameters = lavaan::parameterEstimates(result$fit, standardized = TRUE))
  } else NULL
  run_key <- sieve_hash(list(engines = sieve_engine_keys(engine_identity),
    status = result$status, estimates = estimates, inference = sieve_fit_inference(result$fit)))
  for (rec in rev(existing)) {
    if (!is.null(rec$estimates) && identical(rec$spec$key, result$spec$key) &&
      identical(rec$input_key, input_key) && identical(rec$run_key, run_key) &&
      sieve_same_revision(rec$revision, revision)) {
      return(invisible(rec$id))
    }
  }
  id <- sprintf("A%d", length(existing) + 1L)
  # Share the content-addressed replay store instead of embedding another
  # full response matrix in every saved analysis.
  input_reference <- if (is.list(captured) && all(c("raw", "items", "meta") %in% names(captured))) {
    key <- sieve_replay_store(state, captured)
    list(
      replay_key = key, n = nrow(captured$items),
      original_rows = captured$case_index %||% seq_len(nrow(captured$items)),
      data_hash = sieve_hash(captured$items), meta_hash = sieve_hash(captured$meta)
    )
  } else captured
  record <- list(
    id = id,
    kind = result$spec$kind,
    note = note,
    spec = result$spec,
    input_key = input_key,
    run_key = run_key, engine_identity = engine_identity,
    scientific_input_key = sieve_record_input_key(captured),
    revision = revision,
    scientific_dataset_id = result$scientific_dataset_id %||% shiny::isolate(state$scientific_dataset_id),
    inference = sieve_fit_inference(result$fit),
    fitted_respondents = sieve_fitted_respondents(result$fit, captured, result$input_rows),
    inputs = input_reference,
    sample_size = list(
      input = input_reference$n %||% NA_integer_,
      # A result from another engine supplies the rows it used (the
      # Network page: psychonetrics' own count, which under FIML is every
      # row with an observed item).
      fitted = if (inherits(result$fit, "lavaan")) {
        tryCatch(lavaan::lavInspect(result$fit, "nobs"), error = function(e) NA_integer_)
      } else if (length(result$n_fitted) == 1L && is.finite(result$n_fitted)) {
        as.integer(result$n_fitted)
      } else NA_integer_
    ),
    estimates = estimates,
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    status = result$status,
    warnings = result$warnings,
    versions = c(
      list(
        SIEVER = sieve_version(),
        DMAR = as.character(utils::packageVersion("DMAR")),
        lavaan = as.character(utils::packageVersion("lavaan")),
        psych = as.character(utils::packageVersion("psych")),
        R = paste(R.version$major, R.version$minor, sep = ".")
      ),
      # The versions of any further engine the result ran in, read when it
      # was computed (the Network page: psychonetrics, qgraph, igraph).
      if (is.list(result$engine_versions)) result$engine_versions
    )
  )
  # The methods the specification used, as keys of the reference registry
  # (sieve_spec_methods(); a caller may add keys in result$methods), so a
  # report cites the methods of the records it reads and nothing else.
  record$methods <- sieve_spec_methods(
    result$spec, record$estimates,
    fitted = !is.null(record$estimates), extra = result$methods
  )
  state$analyses <- c(existing, stats::setNames(list(record), id))
  invisible(id)
}

#' Fit again with the expected information when the observed cannot be inverted
#'
#' For a converged ML fit by full information maximum likelihood whose
#' standard errors could not be computed (lavaan's observed information
#' matrix, the default under FIML, could not be inverted), the same model
#' is fit with `information = "expected"`. The estimates are the same; the
#' standard errors come from the expected information, as for complete data
#' under ML. The higher-order model of the bundled 24
#' tests (complete data) converges under ML with FIML without standard
#' errors, and has them with the expected information or under MLR.
#'
#' @param fit The lavaan fit.
#' @param args The arguments it was fit with (without `data`).
#' @param data The data.
#' @param fun `lavaan::cfa` or `lavaan::sem`.
#' The standard test SIEVER asks for under ML with FIML
#' (`sieve_spec_default_test()`) does not stop the refit; any other test
#' (a bootstrap, a scaled statistic) or a chosen standard error does.
#'
#' @return `NULL` when nothing was done (another estimator, information
#'   already chosen, not converged, standard errors available, or the refit
#'   did not give them either), otherwise a list with `fit`, `args` and
#'   `note`.
#' @author Ken Kelley
#' @noRd
sieve_expected_information_retry <- function(fit, args, data, fun = lavaan::cfa) {
  if (!inherits(fit, "lavaan") || !identical(toupper(args$estimator %||% ""), "ML") ||
    !(args$missing %||% "") %in% c("ml", "ml.x", "fiml") || !is.null(args$information) ||
    !(is.null(args$test) || identical(args$test, "standard")) || !is.null(args$se)) {
    return(NULL)
  }
  converged <- isTRUE(tryCatch(lavaan::lavInspect(fit, "converged"), error = function(e) FALSE))
  if (!converged || isTRUE(sieve_fit_status(fit)$se_available)) return(NULL)
  call <- c(args, list(information = "expected", data = data))
  utils::capture.output(run <- sieve_capture(do.call(fun, call)), type = "output")
  if (!isTRUE(run$ok) || !isTRUE(sieve_fit_status(run$value)$se_available)) return(NULL)
  list(
    fit = run$value, args = c(args, list(information = "expected")),
    note = paste(
      "Standard errors: the observed information matrix (the default under full information ML)",
      "could not be inverted for this solution, so the model was fit again with the expected",
      "information (information = \"expected\"); the estimates are the same."
    )
  )
}

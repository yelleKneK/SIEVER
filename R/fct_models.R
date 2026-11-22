# fct_models.R
# Shared lavaan model construction from the item ledger. The winnowing
# engine keeps the authoritative construct -> item mapping in meta; these
# helpers turn the active part of that ledger into a lavaan measurement
# model and fit it with the estimator implied by the confirmed item types.
# Used by the CFA/SEM, Reliability, and Bifactor modules so the model is
# built one way everywhere.
#' The estimator and ordered-item specification for lavaan
#'
#' Maps the confirmed item types, or the estimator the researcher chose, to
#' a lavaan estimator through `sieve_estimator_resolve()`: ML with FIML for
#' all-continuous items (DMAR's default), WLSMV on a polychoric or
#' tetrachoric matrix when any active item is ordered or binary, or the
#' chosen one (MLR with FIML for markedly non-normal items among them).
#'
#' @param meta The item ledger.
#' @param estimator One of `sieve_estimator_choices()` (`"auto"` follows
#'   the item types).
#' @return A list with `estimator`, `ordered` (character vector of ordered
#'   item names, or `NULL`), `missing`, `choice` and `label`.
#' @author Ken Kelley
#' @noRd
sieve_lavaan_spec <- function(meta, estimator = "auto") {
  active <- meta[meta$status == "active", , drop = FALSE]
  resolved <- sieve_estimator_resolve(active$type, choice = estimator)
  ord <- active$item[resolved$categorical]
  list(
    estimator = resolved$estimator,
    ordered = if (length(ord)) ord else NULL,
    missing = resolved$missing,
    choice = resolved$choice,
    label = resolved$label
  )
}
#' The measurement model a ledger implies, with what it does and does not hold
#'
#' Builds the lavaan syntax of the correlated-factors model
#' (`"first_order"`) or of the construct to facet to items model
#' (`"higher_order"`) and returns, with it, what the syntax alone cannot
#' say: the indicators the model is meant to contain, the latent variables
#' it defines, the label behind each latent name, and every construct,
#' facet, or item that could not be modeled, with the reason. The fit is
#' later checked against `observed` and `latent`, so a model whose
#' membership changed on the way to the engine is refused rather than
#' reported (see [sieve_run_model()]).
#'
#' A factor is written when it has at least `min_items` included items. Two
#' is the default: a factor with two indicators is identified inside a model
#' that gives it a nonzero covariance with another factor, or under an
#' equality constraint, so the count is not raised to three here. Whether
#' the model as a whole is identified is established from the fit (see
#' `sieve_fit_status()`). A construct with fewer items is left out of the
#' model and listed in `not_modeled`.
#'
#' In the higher-order model a construct with two or more usable facets is a
#' second-order factor measured by its facets; a construct with fewer is
#' left first-order. Items with no facet inside a construct that is modeled
#' through its facets, and facets with too few items, are not modeled and
#' are listed.
#'
#' @param meta The item ledger.
#' @param structure `"first_order"`, `"higher_order"`, or `"facet_first_order"`.
#' @param min_items Minimum included items for a factor to be written.
#' @param reserved Further observed variable names the latent names must
#'   avoid (the columns of the data; every ledger item is always avoided).
#' @param facet_ids Nested facet identities to select for direct facet CFA.
#'   NULL selects every assigned facet; an empty vector selects none.
#' @return A list with `structure`, `model` (lavaan syntax, `""` when no
#'   factor qualifies), `observed` (the intended indicators, in model
#'   order), `latent`, `name_map` (latent name -> label), `not_modeled` (a
#'   data frame with `level`, `construct`, `facet`, `items`, `n_items`, and
#'   `reason`), and `names` (the `sieve_name_plan()` used).
#' @author Ken Kelley
#' @noRd
sieve_cfa_plan <- function(meta, structure = c("first_order", "higher_order", "facet_first_order"),
                           min_items = 2L, reserved = NULL, facet_ids = NULL) {
  structure <- match.arg(structure)
  plan_names <- sieve_name_plan(meta, reserved = reserved)
  active <- meta[meta$status == "active" & sieve_construct_assigned(meta$construct), , drop = FALSE]
  facet <- if ("facet" %in% names(active)) as.character(active$facet) else rep(NA_character_, nrow(active))
  cons <- unique(active$construct)
  lines <- character(0)
  observed <- character(0)
  latent <- character(0)
  name_map <- character(0)
  not_modeled <- sieve_not_modeled()
  unassigned <- meta$item[meta$status == "active" & !sieve_construct_assigned(meta$construct)]
  if (length(unassigned)) {
    not_modeled <- rbind(not_modeled, sieve_not_modeled(
      "item", NA_character_, NA_character_, unassigned, "no construct is assigned"
    ))
  }
  if (identical(structure, "facet_first_order")) {
    groups <- sieve_facet_groups(meta)
    selected <- if (is.null(facet_ids)) names(groups) else as.character(facet_ids)
    for (group in groups) {
      its <- group$items
      reason <- if (!group$id %in% selected) {
        "the facet is outside the selected model"
      } else if (length(its) < min_items) {
        sprintf(
          "the facet has %d included item%s; a factor needs at least %d",
          length(its), if (length(its) == 1L) "" else "s", as.integer(min_items)
        )
      } else NULL
      if (!is.null(reason)) {
        not_modeled <- rbind(not_modeled, sieve_not_modeled(
          "facet", group$construct, group$facet, its, reason
        ))
        next
      }
      row <- plan_names$facet$construct == group$construct & plan_names$facet$facet == group$facet
      fname <- plan_names$facet$name[row]
      lines <- c(lines, sprintf("%s =~ %s", fname, paste(its, collapse = " + ")))
      observed <- c(observed, its)
      latent <- c(latent, fname)
      name_map <- c(name_map, stats::setNames(group$label, fname))
    }
    unassigned <- active$item[is.na(facet) | !nzchar(facet)]
    if (length(unassigned)) {
      for (item in unassigned) {
        cn <- active$construct[match(item, active$item)]
        not_modeled <- rbind(not_modeled, sieve_not_modeled(
          "item", cn, NA_character_, item, "no facet is assigned for direct facet CFA"
        ))
      }
    }
    problems <- if (length(latent) == 1L && length(observed) == 2L) {
      paste(
        "A facet with two indicators by itself does not identify an unconstrained one-factor model.",
        "Add an indicator or select an appropriate correlated-facet model; no equal-loading constraint was added."
      )
    } else character(0)
    return(list(
      structure = structure, model = paste(lines, collapse = "\n"),
      observed = observed, latent = latent, name_map = name_map,
      not_modeled = not_modeled, names = plan_names, problems = problems,
      facet_ids = selected
    ))
  }
  for (cn in cons) {
    in_con <- active$construct == cn
    cit <- active$item[in_con]
    cfac <- facet[in_con]
    cname <- sieve_latent_name(plan_names, cn)
    if (identical(structure, "higher_order")) {
      has_facet <- !is.na(cfac) & nzchar(cfac)
      facs <- unique(cfac[has_facet])
      if (length(facs) >= 2L) {
        fac_lines <- character(0)
        valid <- character(0)
        valid_label <- character(0)
        fac_items <- character(0)
        fac_skipped <- sieve_not_modeled()
        for (fc in facs) {
          its <- cit[has_facet & cfac == fc]
          row <- plan_names$facet$construct == cn & plan_names$facet$facet == fc
          fname <- plan_names$facet$name[row]
          if (length(its) >= min_items) {
            fac_lines <- c(fac_lines, sprintf("%s =~ %s", fname, paste(its, collapse = " + ")))
            valid <- c(valid, fname)
            valid_label <- c(valid_label, plan_names$facet$label[row])
            fac_items <- c(fac_items, its)
          } else {
            fac_skipped <- rbind(fac_skipped, sieve_not_modeled(
              "facet", cn, fc, its,
              sprintf(
                "the facet has %d included item%s; a first-order factor needs at least %d",
                length(its), if (length(its) == 1L) "" else "s", as.integer(min_items)
              )
            ))
          }
        }
        if (length(valid) >= 2L) {
          lines <- c(
            lines, fac_lines,
            sprintf("%s =~ %s", cname, paste(valid, collapse = " + "))
          )
          observed <- c(observed, fac_items)
          latent <- c(latent, valid, cname)
          name_map <- c(name_map, stats::setNames(c(valid_label, cn), c(valid, cname)))
          not_modeled <- rbind(not_modeled, fac_skipped)
          if (any(!has_facet)) {
            not_modeled <- rbind(not_modeled, sieve_not_modeled(
              "item", cn, NA_character_, cit[!has_facet],
              "no facet is assigned, and this construct is modeled through its facets"
            ))
          }
          next
        }
      }
    }
    if (length(cit) >= min_items) {
      lines <- c(lines, sprintf("%s =~ %s", cname, paste(cit, collapse = " + ")))
      observed <- c(observed, cit)
      latent <- c(latent, cname)
      name_map <- c(name_map, stats::setNames(cn, cname))
    } else {
      not_modeled <- rbind(not_modeled, sieve_not_modeled(
        "construct", cn, NA_character_, cit,
        sprintf(
          "the construct has %d included item%s; a factor needs at least %d",
          length(cit), if (length(cit) == 1L) "" else "s", as.integer(min_items)
        )
      ))
    }
  }
  list(
    structure = structure,
    model = paste(lines, collapse = "\n"),
    observed = observed,
    latent = latent,
    name_map = name_map,
    not_modeled = not_modeled,
    names = plan_names
  )
}

#' One or more rows of the record of what a model leaves out
#'
#' @param level `"construct"`, `"facet"`, or `"item"`.
#' @param construct,facet The construct and facet concerned.
#' @param items The item names concerned.
#' @param reason A plain language reason.
#' @return A one row data frame, or the empty record when called with no
#'   arguments.
#' @author Ken Kelley
#' @noRd
sieve_not_modeled <- function(level = NULL, construct = NULL, facet = NA_character_,
                              items = character(0), reason = NULL) {
  if (is.null(level)) {
    return(data.frame(
      level = character(0), construct = character(0), facet = character(0),
      items = character(0), n_items = integer(0), reason = character(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    level = level, construct = construct, facet = facet,
    items = paste(items, collapse = ", "), n_items = length(items), reason = reason,
    stringsAsFactors = FALSE
  )
}
#' Active items inside a facetted construct that carry no facet
#'
#' These cannot be placed in a higher-order model (they have no first-order
#' factor to load on), so the higher-order fit reports them as excluded.
#'
#' @param meta The item ledger.
#' @return A character vector of item names (possibly empty).
#' @author Ken Kelley
#' @noRd
sieve_unfacetted_items <- function(meta) {
  active <- meta[meta$status == "active", , drop = FALSE]
  out <- character(0)
  for (cn in unique(active$construct)) {
    cit <- active[active$construct == cn, , drop = FALSE]
    has_facet <- !is.na(cit$facet) & nzchar(cit$facet)
    if (length(unique(cit$facet[has_facet])) >= 2L) {
      out <- c(out, cit$item[!has_facet])
    }
  }
  out
}

#' Fit the CFA Implied by the Ledger
#'
#' Builds the measurement model with [sieve_cfa_spec()] (the first-order
#' correlated-factors model by default, or any supplied `syntax` such as the
#' higher-order model) and
#' fits it with the type appropriate estimator. Continuous models use FIML
#' so incomplete cases contribute; ordered or binary models use WLSMV with
#' listwise deletion. Latent variances are fixed to one (`std.lv = TRUE`) by
#' default, which sets the scale of the latent variables and leaves every
#' loading free; it does not standardize the observed variables.
#'
#' @param items_df A data frame holding the item columns.
#' @param meta The item ledger.
#' @param syntax Optional lavaan model syntax. When `NULL`, the first-order
#'   correlated-factors model is built from `meta`.
#' @param std_lv Identification. `TRUE` (default) fixes each factor variance
#'   to one, so every loading (path) is freely estimated; `FALSE` uses the
#'   marker method, fixing the first indicator's loading to one and freeing
#'   the factor variance.
#' @param orthogonal If `TRUE`, the factors are fixed uncorrelated; `FALSE`
#'   (default) freely estimates the factor correlations.
#' @param estimator One of `"auto"`, which follows the item types (ML
#'   with FIML when every item is continuous, WLSMV when any is ordered or
#'   binary), `"mlr"` (MLR with FIML), `"ml_fiml"` (ML with FIML), `"ml"`
#'   (ML on the complete cases) or `"wlsmv"` (WLSMV, every item treated as
#'   ordered).
#' @param store A store from [sieve_fit_store()], or `NULL`. With a store,
#'   a model already fit under the identical identity (see
#'   [sieve_fit_identity()]) is returned without a second estimation, and
#'   a new fit is stored for later callers.
#' @param page A short name of the caller, kept with a stored fit.
#' @return A fitted [lavaan::lavaan] object carrying its resolved
#'   specification (`sieve_spec`), its status (`sieve_status`, see
#'   `sieve_status_of()`), the conditions lavaan raised (`sieve_warnings`),
#'   and its name map (`name_map`) as attributes; or `NULL` if lavaan is
#'   unavailable, the model is empty, the fit errors, or the fitted model
#'   does not hold the intended indicators. A returned fit is not a claim
#'   that the model converged or is identified: read its status. Callers
#'   that need the reason for a failure use [sieve_cfa_spec()] with
#'   `sieve_run_cfa()` instead.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' fit <- sieve_fit_cfa(items, meta)
#' attr(fit, "sieve_status")$code
#' # The RMSEA interval at 90%, the level of every RMSEA interval SIEVER
#' # shows (DMAR's default for the RMSEA interval and lavaan's).
#' # The SRMR of the covariance residuals, the one SIEVER and DMAR report.
#' lavaan::fitMeasures(fit, c("cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
#'   "srmr_bentler_nomean"), fm.args = list(rmsea.ci.level = 0.9))
#' # With a store, the identical model on the identical data is fit once.
#' store <- sieve_fit_store()
#' first <- sieve_fit_cfa(items, meta, store = store, page = "report")
#' identical(first, sieve_fit_cfa(items, meta, store = store, page = "report"))
#' @export
sieve_fit_cfa <- function(items_df, meta, syntax = NULL, std_lv = TRUE, orthogonal = FALSE,
                          estimator = "auto", store = NULL, page = NULL) {

  spec <- sieve_cfa_spec(meta,
    syntax = syntax, std_lv = std_lv, orthogonal = orthogonal,
    reserved = names(items_df), estimator = estimator
  )
  if (!nzchar(spec$model)) {
    return(NULL)
  }
  sieve_run_cfa(spec, items_df, store = store, page = page)$fit
}

#' The structural model a ledger and a selection imply
#'
#' Builds the measurement model for the constructs involved and the
#' structural regressions among them and onto external observed outcomes,
#' and returns with the syntax what the model is meant to contain (see
#' `sieve_cfa_plan()`). Every name in the model comes from one allocation:
#' the latent names avoid every ledger item and every name in `reserved`;
#' an external outcome keeps its column name when that name is usable and
#' free, and otherwise gets a new one (kept in `out_map`); the coefficient
#' labels and the names of the defined effects avoid all of them.
#'
#' An outcome is either a construct (a latent variable of the model) or an
#' external observed column, and the two are different models: with a
#' construct labeled `y` and a data column `y`, "y on the constructs" is a
#' regression among latent variables in one reading and a regression of an
#' observed criterion in the other. `outcome_kind` says which is meant. A
#' name that could be either and comes without a kind is not guessed at: the
#' model is refused and the name is given.
#'
#' @param meta The item ledger.
#' @param outcomes Outcome names: construct labels or external columns.
#' @param outcome_kind `NULL`, or a character vector as long as `outcomes`
#'   with `"construct"` or `"external"` for each (or `NA` to resolve that one
#'   by name). By name, a construct label is the construct and anything else
#'   an external column; a name that is a construct label and also a column
#'   in `reserved` other than a ledger item is ambiguous.
#' @param predictors Predictor construct labels. Ignored when
#'   `all_constructs` is `TRUE`.
#' @param antecedents Columns of the data with the role antecedent, used as
#'   observed exogenous predictors of every outcome beside the predictor
#'   constructs. Each keeps its column name when that name is usable and
#'   free, and otherwise gets a new one (kept in `pred_map`).
#' @param reserved Further observed variable names to avoid (the columns of
#'   the data).
#' @param effects `TRUE` labels every structural coefficient and defines
#'   each indirect effect (every chain X -> M -> Y, and with ordered
#'   mediators every chain through several of them) and the matching total
#'   effect.
#' @param mediator_order The constructs chosen as both predictor and
#'   outcome (the mediators), in their causal order: each is regressed on
#'   the predictors and on the mediators before it, and predicts the
#'   mediators after it and the outcomes (serial mediation). Needed when
#'   there are two or more mediators: without it each would be regressed on
#'   the others, a feedback loop the selection does not identify, and the
#'   model is refused with that reason. Ignored with one mediator.
#' @param all_constructs `TRUE` regresses every outcome, read as an external
#'   variable, on every construct in the measurement model.
#' @param min_items Minimum included items for a construct to be a latent
#'   variable.
#' @return A list with `structure`, `model`, `observed`, `latent`,
#'   `name_map` (engine name -> the label shown; an external outcome that
#'   shares its name with a construct in the model is shown with
#'   "(data column)" after it), `lat_map` (latent name -> construct),
#'   `out_map` (engine column -> source column, for external outcomes),
#'   `pred_map` (engine column -> source column, for antecedents),
#'   `eff_map` (defined effect -> description), `mediators`, `not_modeled`,
#'   `problems` (reasons the requested model cannot be written, possibly
#'   empty), and `names`.
#' @author Ken Kelley
#' @noRd
sieve_structural_plan <- function(meta, outcomes, predictors = NULL, reserved = NULL,
                                  effects = FALSE, all_constructs = FALSE, min_items = 2L,
                                  outcome_kind = NULL, antecedents = NULL,
                                  mediator_order = NULL) {
  outcomes <- as.character(outcomes)
  ante_pred <- unique(as.character(antecedents))
  ante_pred <- ante_pred[!is.na(ante_pred) & nzchar(ante_pred)]
  kind <- if (is.null(outcome_kind)) {
    rep(NA_character_, length(outcomes))
  } else {
    rep_len(as.character(outcome_kind), length(outcomes))
  }
  predictors <- unique(as.character(predictors))
  active <- meta[meta$status == "active" & sieve_construct_assigned(meta$construct), , drop = FALSE]
  cons <- unique(active$construct)
  # Each outcome is resolved to a construct or an external column once, here.
  ambiguous <- character(0)
  if (isTRUE(all_constructs)) {
    kind[] <- "external"
  } else {
    open <- is.na(kind) | !kind %in% c("construct", "external")
    # A ledger item is not offered as an external outcome (it is an indicator
    # or a parked item), so a construct labeled like an item is the construct.
    either <- open & outcomes %in% cons & outcomes %in% setdiff(reserved, meta$item)
    ambiguous <- unique(outcomes[either])
    kind[open] <- ifelse(outcomes[open] %in% cons, "construct", "external")
  }
  targets <- unique(data.frame(name = outcomes, kind = kind, stringsAsFactors = FALSE))
  outcomes <- targets$name
  out_cons <- targets$name[targets$kind == "construct"]
  ext_out <- targets$name[targets$kind == "external"]
  con_pred <- if (isTRUE(all_constructs)) cons else intersect(predictors, cons)
  plan_names <- sieve_name_plan(meta, reserved = c(reserved, ext_out, ante_pred))
  out <- list(
    structure = if (isTRUE(effects)) "mediation" else if (isTRUE(all_constructs)) "criterion" else "path",
    model = "", observed = character(0), latent = character(0),
    name_map = character(0), lat_map = character(0), out_map = character(0),
    pred_map = character(0),
    eff_map = character(0), mediators = intersect(con_pred, out_cons),
    not_modeled = sieve_not_modeled(), problems = character(0), names = plan_names
  )
  stop_with <- function(why) {
    out$problems <- c(out$problems, why)
    out
  }
  if (!length(outcomes)) {
    return(stop_with("No outcome is selected."))
  }
  if (length(ambiguous)) {
    return(stop_with(sprintf(
      paste(
        "The outcome%s %s %s both a construct label and a column of the data, and the two",
        "are different models (a latent outcome, or an observed one). Say which is meant:",
        "choose it under Constructs or under External variables."
      ),
      if (length(ambiguous) == 1L) "" else "s",
      paste(sprintf("'%s'", ambiguous), collapse = ", "),
      if (length(ambiguous) == 1L) "is" else "are"
    )))
  }
  not_a_construct <- setdiff(out_cons, cons)
  if (length(not_a_construct)) {
    return(stop_with(sprintf(
      "The outcome%s %s %s selected as a construct, and no construct with included items has that label.",
      if (length(not_a_construct) == 1L) "" else "s",
      paste(sprintf("'%s'", not_a_construct), collapse = ", "),
      if (length(not_a_construct) == 1L) "is" else "are"
    )))
  }
  if (!length(con_pred) && !length(ante_pred)) {
    return(stop_with("No predictor construct with included items, and no antecedent, is selected."))
  }
  if (length(intersect(ante_pred, c(out_cons, ext_out)))) {
    return(stop_with(sprintf(
      "The antecedent%s %s %s also selected as an outcome; a variable cannot predict itself.",
      if (length(intersect(ante_pred, c(out_cons, ext_out))) == 1L) "" else "s",
      paste(sprintf("'%s'", intersect(ante_pred, c(out_cons, ext_out))), collapse = ", "),
      if (length(intersect(ante_pred, c(out_cons, ext_out))) == 1L) "is" else "are"
    )))
  }
  # Two or more mediators: without an order each would be regressed on the
  # others (a feedback loop whose equations hold the same predictors, so it
  # is not identified); with one, each mediator predicts only the mediators
  # after it, and the model is recursive (serial mediation).
  loop <- sieve_mediator_order_problem(out$mediators, mediator_order)
  if (!is.null(loop)) {
    return(stop_with(loop))
  }
  med_order <- if (length(out$mediators) >= 2L) {
    as.character(mediator_order)[as.character(mediator_order) %in% out$mediators]
  } else {
    out$mediators
  }
  out$mediator_order <- med_order
  later_mediators <- function(m) {
    at <- match(m, med_order)
    if (is.na(at)) character(0) else med_order[seq_along(med_order) > at]
  }
  if (isTRUE(effects) && !length(out$mediators)) {
    return(stop_with(paste(
      "No construct is both a predictor and an outcome, so there is no indirect",
      "effect to estimate."
    )))
  }

  # Measurement part.
  used <- if (isTRUE(all_constructs)) cons else unique(c(con_pred, out_cons))
  items_of <- function(cn) active$item[active$construct == cn]
  enough <- vapply(used, function(cn) length(items_of(cn)) >= min_items, logical(1))
  for (cn in used[!enough]) {
    out$not_modeled <- rbind(out$not_modeled, sieve_not_modeled(
      "construct", cn, NA_character_, items_of(cn),
      sprintf(
        "the construct has %d included item%s; a factor needs at least %d",
        length(items_of(cn)), if (length(items_of(cn)) == 1L) "" else "s",
        as.integer(min_items)
      )
    ))
  }
  if (!isTRUE(all_constructs) && any(!enough)) {
    # A construct the researcher selected cannot be dropped quietly: the
    # remaining coefficients would be those of a different model.
    short <- used[!enough]
    counts <- vapply(short, function(cn) length(items_of(cn)), integer(1))
    return(stop_with(sprintf(
      paste(
        "The selected construct%s %s cannot enter the structural model as a latent",
        "variable: %s, and a factor needs at least %d. On the Items tab, include at",
        "least %d items of %s, or leave %s out of the selection."
      ),
      if (length(short) == 1L) "" else "s",
      paste(sprintf("'%s'", short), collapse = ", "),
      paste(sprintf(
        "'%s' has %d included item%s", short, counts,
        ifelse(counts == 1L, "", "s")
      ), collapse = ", "),
      as.integer(min_items), as.integer(min_items),
      if (length(short) == 1L) sprintf("'%s'", short) else "each",
      if (length(short) == 1L) "it" else "them"
    )))
  }
  modeled <- used[enough]
  if (!length(modeled)) {
    return(stop_with("No construct has enough included items to be a latent variable."))
  }
  con_pred <- intersect(con_pred, modeled)
  latent <- function(cn) sieve_latent_name(plan_names, cn)
  meas_lines <- vapply(modeled, function(cn) {
    sprintf("%s =~ %s", latent(cn), paste(items_of(cn), collapse = " + "))
  }, character(1))
  indicators <- unique(unlist(lapply(modeled, items_of)))

  # External outcomes: keep a usable, free column name; otherwise allocate.
  clash <- intersect(ext_out, indicators)
  if (length(clash)) {
    return(stop_with(sprintf(
      "The outcome%s %s %s also an indicator in the measurement model; an item cannot be both.",
      if (length(clash) == 1L) "" else "s", paste(sprintf("'%s'", clash), collapse = ", "),
      if (length(clash) == 1L) "is" else "are"
    )))
  }
  taken <- c(
    setdiff(plan_names$observed, c(ext_out, ante_pred)), plan_names$construct,
    plan_names$facet$name, plan_names$general
  )
  ext_name <- character(length(ext_out))
  for (i in seq_along(ext_out)) {
    o <- ext_out[i]
    ext_name[i] <- if (sieve_name_usable(o) && !(o %in% taken)) {
      o
    } else {
      unname(sieve_allocate_names(o, taken = taken, prefix = "Y"))
    }
    taken <- c(taken, ext_name[i])
  }
  # An external outcome that shares its name with a construct in the model
  # is told apart from it wherever the two are shown.
  ext_show <- ifelse(ext_out %in% modeled, sprintf("%s (data column)", ext_out), ext_out)

  # Antecedents: observed exogenous predictors, named like the external
  # outcomes; one that is an indicator of the measurement model is refused.
  clash <- intersect(ante_pred, indicators)
  if (length(clash)) {
    return(stop_with(sprintf(
      "The antecedent%s %s %s also an indicator in the measurement model; an item cannot be both.",
      if (length(clash) == 1L) "" else "s", paste(sprintf("'%s'", clash), collapse = ", "),
      if (length(clash) == 1L) "is" else "are"
    )))
  }
  ante_name <- character(length(ante_pred))
  for (i in seq_along(ante_pred)) {
    a <- ante_pred[i]
    ante_name[i] <- if (sieve_name_usable(a) && !(a %in% taken)) {
      a
    } else {
      unname(sieve_allocate_names(a, taken = taken, prefix = "X"))
    }
    taken <- c(taken, ante_name[i])
  }
  ante_show <- ifelse(ante_pred %in% modeled, sprintf("%s (data column)", ante_pred), ante_pred)
  # The engine name of a predictor: the latent name of a construct, the
  # column name of an antecedent.
  node_of <- function(p) {
    ifelse(p %in% con_pred, latent(p), ante_name[match(p, ante_pred)])
  }

  # Structural part. In the plain model every outcome is regressed on the
  # predictor constructs other than itself; with `effects` each coefficient
  # is labeled so the indirect and total effects can be defined from them.
  # Every variable is referred to by its engine name (`node`), which is
  # unique, so a construct and a column that share a label stay two things.
  struct_pairs <- list()
  for (i in seq_len(nrow(targets))) {
    o <- targets$name[i]
    latent_outcome <- identical(targets$kind[i], "construct")
    # A mediator is not regressed on the mediators after it in the order.
    preds <- c(
      if (latent_outcome) setdiff(con_pred, c(o, later_mediators(o))) else con_pred,
      ante_pred
    )
    if (length(preds)) {
      at <- match(o, ext_out)
      struct_pairs[[length(struct_pairs) + 1L]] <- list(
        outcome = o, kind = if (latent_outcome) "construct" else "external",
        node = if (latent_outcome) latent(o) else unname(ext_name[at]),
        show = if (latent_outcome) o else unname(ext_show[at]),
        predictors = preds
      )
    }
  }
  if (!length(struct_pairs)) {
    return(stop_with("No outcome is left to regress on a predictor other than itself."))
  }
  defs <- character(0)
  eff_map <- character(0)
  if (!isTRUE(effects)) {
    struct_lines <- vapply(struct_pairs, function(sp) {
      sprintf("%s ~ %s", sp$node, paste(node_of(sp$predictors), collapse = " + "))
    }, character(1))
  } else {
    n_coef <- sum(lengths(lapply(struct_pairs, `[[`, "predictors")))
    coef_label <- unname(sieve_allocate_names(sprintf("b%d", seq_len(n_coef)),
      taken = taken, prefix = "p"
    ))
    taken <- c(taken, coef_label)
    plab <- list()
    struct_lines <- character(0)
    li <- 0L
    for (sp in struct_pairs) {
      terms <- character(0)
      for (p in sp$predictors) {
        li <- li + 1L
        plab[[paste(node_of(p), sp$node, sep = "|")]] <- coef_label[li]
        terms <- c(terms, sprintf("%s*%s", coef_label[li], node_of(p)))
      }
      struct_lines <- c(
        struct_lines,
        sprintf("%s ~ %s", sp$node, paste(terms, collapse = " + "))
      )
    }
    # Indirect effects: every chain X -> M -> Y, and with ordered mediators
    # every chain X -> M1 -> M2 -> ... -> Y through mediators later in the
    # order, each the product of its paths; total = direct + indirects. In
    # a recursive model the sum over the chains from X to Y is the (Y, X)
    # element of (I - B)^-1 - I.
    indirect <- list()
    chains <- function(x, through, labels) {
      last <- through[length(through)]
      found <- list()
      for (sp in struct_pairs) {
        if (identical(sp$node, node_of(x)) || sp$node %in% latent(through)) next
        b <- plab[[paste(latent(last), sp$node, sep = "|")]] # M -> Y
        if (is.null(b)) next
        found[[length(found) + 1L]] <- list(
          x = x, m = through, y = sp$show, y_node = sp$node, paths = c(labels, b)
        )
        if (identical(sp$kind, "construct") && sp$outcome %in% out$mediators) {
          found <- c(found, chains(x, c(through, sp$outcome), c(labels, b)))
        }
      }
      found
    }
    for (m in out$mediators) {
      for (x in setdiff(c(con_pred, ante_pred), m)) {
        a <- plab[[paste(node_of(x), latent(m), sep = "|")]] # X -> M
        if (is.null(a)) next
        indirect <- c(indirect, chains(x, m, a))
      }
    }
    if (!length(indirect)) {
      return(stop_with(paste(
        "The selection has a mediator but no X -> M -> Y chain: an indirect effect",
        "needs a predictor of the mediator and an outcome the mediator predicts."
      )))
    }
    ind_name <- unname(sieve_allocate_names(sprintf("ind%d", seq_along(indirect)),
      taken = taken, prefix = "p"
    ))
    taken <- c(taken, ind_name)
    ind_by_pair <- list()
    pair_show <- list()
    for (i in seq_along(indirect)) {
      e <- indirect[[i]]
      defs <- c(defs, sprintf("%s := %s", ind_name[i], paste(e$paths, collapse = "*")))
      eff_map[ind_name[i]] <- sprintf(
        "%s -> %s -> %s (indirect)", e$x, paste(e$m, collapse = " -> "), e$y
      )
      key <- paste(node_of(e$x), e$y_node, sep = "|")
      ind_by_pair[[key]] <- c(ind_by_pair[[key]], ind_name[i])
      pair_show[[key]] <- c(e$x, e$y)
    }
    tot_name <- unname(sieve_allocate_names(sprintf("tot%d", seq_along(ind_by_pair)),
      taken = taken, prefix = "p"
    ))
    for (i in seq_along(ind_by_pair)) {
      key <- names(ind_by_pair)[i]
      xy <- pair_show[[key]]
      direct <- plab[[key]]
      defs <- c(defs, sprintf(
        "%s := %s", tot_name[i],
        paste(c(direct, ind_by_pair[[key]]), collapse = " + ")
      ))
      eff_map[tot_name[i]] <- sprintf(
        "%s -> %s (total%s)", xy[1], xy[2],
        if (is.null(direct)) ", indirect only" else " = direct + indirect"
      )
    }
  }

  out$model <- paste(c(meas_lines, struct_lines, defs), collapse = "\n")
  out$observed <- c(indicators, ext_name, ante_name)
  out$latent <- latent(modeled)
  out$lat_map <- stats::setNames(modeled, out$latent)
  out$out_map <- stats::setNames(ext_out, ext_name)
  out$pred_map <- stats::setNames(ante_pred, ante_name)
  out$name_map <- c(out$lat_map, stats::setNames(ext_show, ext_name), stats::setNames(ante_show, ante_name))
  out$eff_map <- eff_map
  out
}

#' Why a selection with several mediators cannot be fit as it stands
#'
#' A construct chosen as both a predictor and an outcome is a mediator, and
#' every latent outcome is regressed on the predictor constructs other than
#' itself. With two or more mediators each would therefore be regressed on
#' the others: a feedback loop (a nonrecursive model). Both equations of a
#' reciprocal pair would hold the same predictors, so no predictor is
#' excluded from one of them to identify it (the rank condition fails), and
#' lavaan reports the model as not identified. An order of the mediators
#' makes the model recursive (serial mediation), and is what the researcher
#' supplies.
#'
#' @param mediators The mediators, in the order the selection gives them.
#' @param mediator_order The order the researcher set, or `NULL`.
#' @return `NULL` when the selection can be fit (fewer than two mediators,
#'   or an order that names each of them once), and otherwise the reason,
#'   naming the mediators and the remedy.
#' @author Ken Kelley
#' @noRd
sieve_mediator_order_problem <- function(mediators, mediator_order = NULL) {
  mediators <- unique(as.character(mediators))
  if (length(mediators) < 2L) {
    return(NULL)
  }
  given <- as.character(mediator_order %||% character(0))
  given <- given[given %in% mediators]
  if (!anyDuplicated(given) && setequal(given, mediators)) {
    return(NULL)
  }
  named <- paste(sprintf("'%s'", mediators), collapse = if (length(mediators) == 2L) " and " else ", ")
  sprintf(
    paste(
      "The constructs %s are each chosen as both a predictor and an outcome. Without an order,",
      "each would be regressed on the other%s: a feedback loop (a nonrecursive model) whose",
      "equations hold the same predictors, so it is not identified, whatever the data. Set the",
      "order of the mediators (serial mediation: each mediator is regressed on the predictors and",
      "on the mediators before it, and predicts the mediators after it and the outcomes), or",
      "choose all but one of them as a predictor only or as an outcome only."
    ),
    named, if (length(mediators) == 2L) "" else "s"
  )
}

#' The outcome choices of the structural model, each with one meaning
#'
#' The outcome selector offers constructs and external columns. A label that
#' is both would be two entries with one value, and the server could not tell
#' which was chosen. Each entry therefore gets a value of its own: the name
#' itself when it occurs once, and the name followed by "(construct)" or
#' "(data column)" when it occurs in both lists.
#'
#' With nothing to offer (no construct with two or more included items and
#' no criterion column, as in a confirmed study whose items are all
#' Unassigned), the table has no rows. The labels are built by assignment
#' rather than with `ifelse()`, which returns a logical vector for an empty
#' test, so `make.unique()` always receives character: an error on an
#' empty offer, raised in the SEM page's observer, would end the session
#' once a study with no construct assigned is confirmed or opened.
#'
#' @param constructs Construct labels offered as outcomes.
#' @param external External column names offered as outcomes.
#' @return A data frame with one row per entry: `value` (unique), `label`
#'   (what the selector shows), `name` (the construct label or column name),
#'   and `kind` (`"construct"` or `"external"`); no rows when there is no
#'   entry.
#' @author Ken Kelley
#' @noRd
sieve_outcome_choices <- function(constructs, external) {
  constructs <- unique(as.character(constructs))
  external <- unique(as.character(external))
  name <- c(constructs, external)
  kind <- c(rep("construct", length(constructs)), rep("external", length(external)))
  shared <- name %in% intersect(constructs, external)
  label <- name
  label[shared] <- sprintf(
    "%s (%s)", name[shared],
    ifelse(kind[shared] == "construct", "construct", "data column")
  )
  data.frame(
    value = make.unique(label, sep = " "), label = label, name = name, kind = kind,
    stringsAsFactors = FALSE
  )
}

#' Resolve selected outcome values to names and kinds
#'
#' @param selected The values chosen in the outcome selector.
#' @param choices The table from `sieve_outcome_choices()`.
#' @return A list with `name` and `kind`, each as long as `selected`. A value
#'   that is not in the table keeps its text as the name and gets the kind
#'   `NA`, which `sieve_structural_plan()` resolves by name, refusing a name
#'   that could be either.
#' @author Ken Kelley
#' @noRd
sieve_resolve_outcomes <- function(selected, choices) {
  selected <- as.character(selected)
  at <- match(selected, choices$value)
  list(
    name = ifelse(is.na(at), selected, choices$name[at]),
    kind = ifelse(is.na(at), NA_character_, choices$kind[at])
  )
}

#' Fit a Structural Model and Report What Happened
#'
#' The one place SIEVER fits a structural model: the criterion and path
#' models of the SEM page, the criterion model of the Validity page, and,
#' with `effects = TRUE`, the labeled model with indirect and total effects.
#' The model is resolved to a specification and fit through
#' [sieve_run_model()], so the result states whether the model
#' failed, did not converge, is not identified, is inadmissible, or is ok,
#' and keeps the conditions lavaan raised. The estimator and the missing
#' data treatment follow the indicators that are in the model.
#'
#' Latent variances (residual variances, for a latent outcome) are fixed to
#' one, which sets the scale of the latent variables; standardized
#' coefficients are read from the standardized solution, not from this
#' scaling.
#'
#' @param items_df Item-column data frame.
#' @param raw The full data frame (for external outcome columns), with rows
#'   aligned to `items_df`.
#' @param meta The item ledger.
#' @param outcomes,predictors,effects,all_constructs,outcome_kind,antecedents
#'   See `sieve_structural_plan()`.
#' @param mediator_order With two or more mediators (constructs chosen as
#'   both predictor and outcome), their causal order: each is regressed on
#'   the predictors and on the mediators before it, and predicts the
#'   mediators after it and the outcomes (serial mediation). Without it such
#'   a selection is refused with the reason, since each mediator would be
#'   regressed on the others, a feedback loop that is not identified.
#'   Ignored with fewer than two mediators.
#' @param estimator One of `"auto"`, which follows the item types (ML
#'   with FIML when every item is continuous, WLSMV when any is ordered or
#'   binary), `"mlr"` (MLR with FIML), `"ml_fiml"` (ML with FIML), `"ml"`
#'   (ML on the complete cases) or `"wlsmv"` (WLSMV, every item treated as
#'   ordered); the tab passes its Estimator control's choice.
#' @param outcome_types Optional named declarations for external outcomes.
#' @param exogenous_missing Engine default, joint random covariates, or ML-X.
#' @param sampling_state Optional shared sampling declaration and aligned raw rows.
#' @param two_stage Optional robust two-stage sensitivity fit for continuous models.
#' @return A `sieve_result` (see [sieve_run_model()]). Its fit, when there
#'   is one, carries the attributes `name_map`, `lat_map`, `out_map`,
#'   `pred_map` and `eff_map`.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' sem <- sieve_run_structural(items, items, meta, outcomes = "Speed",
#'   predictors = c("Visual", "Textual"))
#' sem$status$code
#' sem$spec$model
#' @export
sieve_run_structural <- function(items_df, raw, meta, outcomes, predictors = NULL,
                                 effects = FALSE, all_constructs = FALSE,
                                 outcome_kind = NULL, antecedents = NULL, estimator = "auto",
                                 outcome_types = NULL,
                                 exogenous_missing = c(
                                   "engine_default",
                                   "random",
                                   "fiml_x"
                                 ),
                                 sampling_state = NULL, mediator_order = NULL, two_stage = FALSE) {
  exogenous_missing <- match.arg(exogenous_missing)
  plan <- sieve_structural_plan(
    meta, outcomes, predictors,
    reserved = union(names(items_df), names(raw)),
    effects = effects, all_constructs = all_constructs, outcome_kind = outcome_kind,
    antecedents = antecedents, mediator_order = mediator_order
  )
  if (length(outcome_types)) {
    if (is.null(names(outcome_types)) || anyDuplicated(names(outcome_types))) stop("`outcome_types` must be named by external outcome.",
      call. = FALSE
    )
    if (!is.character(outcome_types) || any(!nzchar(names(outcome_types))) || any(!names(outcome_types) %in% unname(plan$out_map))) stop("Declare types only for selected external outcomes.",
      call. = FALSE
    )
    for (nm in names(outcome_types)[outcome_types == "binary"]) {
      values <- unique(stats::na.omit(raw[[nm]]))
      if (length(values) != 2L) plan$problems <- c(
        plan$problems,
        paste(
          "Declared binary outcome",
          nm,
          "must have exactly two observed categories."
        )
      )
    }
    types <- outcome_types[unname(plan$out_map)]
    types[is.na(types)] <- "continuous"
    plan$external_types <- stats::setNames(as.character(types), names(plan$out_map))
    bad <- !types %in% c("continuous", "ordered", "binary")
    if (any(bad)) plan$problems <- c(
      plan$problems,
      "Declare each external outcome as continuous, ordered, or binary. Nominal and count outcome models are not supported by this engine."
    )
  }
  spec <- sieve_model_spec(plan, meta, kind = "sem", fun = "sem", std_lv = TRUE, estimator = estimator)
  if (!identical(exogenous_missing, "engine_default")) {
    if (length(spec$ordered)) spec$problems <- c(
      spec$problems,
      "FIML-X and random-covariate missingness strategies are offered here only for continuous ML-family models."
    )
    if (identical(
      exogenous_missing,
      "random"
    )) spec <- sieve_spec_engine_args(
      spec,
      list(
        fixed.x = FALSE,
        conditional.x = FALSE
      )
    )
    if (identical(exogenous_missing, "fiml_x")) {
      spec$missing <- "ml.x"
      spec <- sieve_spec_engine_args(spec, list(fixed.x = TRUE, conditional.x = FALSE))
    }
    spec$exogenous_missing <- exogenous_missing
    spec$key <- NULL
    spec$key <- sieve_hash(spec)
  }
  indicators <- setdiff(plan$observed, c(names(plan$out_map), names(plan$pred_map)))
  d <- items_df[, intersect(indicators, names(items_df)), drop = FALSE]
  for (nm in c(names(plan$out_map), names(plan$pred_map))) {
    src <- c(plan$out_map, plan$pred_map)[[nm]]
    if (src %in% names(raw)) {
      d[[nm]] <- raw[[src]]
    }
  }
  if (!is.null(sampling_state)) {
    prepared <- sieve_sampling_apply(spec, sampling_state, d)
    spec <- prepared$spec
    d <- prepared$data
    if (!is.null(prepared$reason)) spec$problems <- c(spec$problems, prepared$reason)
  }
  spec <- sieve_two_stage_spec(spec, two_stage)
  result <- sieve_run_model(spec, d)
  if (!is.null(sampling_state)) {
    result$input_rows <- prepared$row_index
    # Declared auxiliary variables left out because every analyzed value is
    # observed (sieve_auxiliary_apply()); the page's method line says so.
    result$aux_notes <- prepared$notes
  }
  result$row_flow <- sieve_sem_row_flow(d, spec, result$fit)
  if (!is.null(result$fit)) {
    result$estimates <- list(
      fit = sieve_fit_measures(result$fit),
      parameters = lavaan::parameterEstimates(result$fit,
        standardized = TRUE
      ),
      row_flow = result$row_flow
    )
    attr(result$fit, "lat_map") <- plan$lat_map
    attr(result$fit, "out_map") <- plan$out_map
    attr(result$fit, "pred_map") <- plan$pred_map
    attr(result$fit, "eff_map") <- plan$eff_map
  }
  result
}
#' Fit the Measurement Model With an Added Method Factor
#'
#' The correlated-factors measurement model plus one further latent
#' variable, uncorrelated with every construct, on which every included item
#' loads (an unmeasured latent method factor). The method factor's name is
#' allocated with the other latent names, so it cannot collide with a
#' construct labeled "Method" or with an item of that name, and it is
#' carried on the fit as the `method_factor` attribute.
#'
#' @details
#' Values must already reflect intended reverse scoring. This is an exploratory
#' sensitivity model, not a test that establishes or rules out common-method
#' bias. It assumes independent sampling units; no dependent-design adjustment
#' is supplied. Unsupported or unsuccessful fits retain the standard model
#' status and reason, and estimates are not rounded.
#' @param items_df Numeric item responses, one row per person.
#' @param meta Ledger defining active items, constructs, and item types.
#' @param estimator One of `"auto"`, which follows the item types (ML
#'   with FIML when every item is continuous, WLSMV when any is ordered or
#'   binary), `"mlr"` (MLR with FIML), `"ml_fiml"` (ML with FIML), `"ml"`
#'   (ML on the complete cases) or `"wlsmv"` (WLSMV, every item treated as
#'   ordered).
#' @return A `sieve_result` (see [sieve_run_model()]).
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' method <- sieve_run_method_factor(items, meta)
#' # The status says whether the added factor leaves an admissible solution.
#' method$status[c("code", "message")]
#' @export
sieve_run_method_factor <- function(items_df, meta, estimator = "auto") {
  plan <- sieve_cfa_plan(meta, "first_order", reserved = names(items_df))
  active <- meta$item[meta$status == "active"]
  its <- intersect(active, names(items_df))
  method <- unname(sieve_allocate_names(
    "Method",
    taken = c(
      plan$names$observed, plan$names$construct, plan$names$facet$name,
      plan$names$general
    ),
    prefix = "F"
  ))
  if (!nzchar(plan$model)) {
    plan$problems <- "No construct has enough included items for a measurement model, so there is nothing to add a method factor to."
  } else if (length(its) < 3L) {
    plan$problems <- "A method factor needs at least three included items."
  } else {
    plan$model <- paste(
      plan$model,
      sprintf("%s =~ %s", method, paste(its, collapse = " + ")),
      paste(sprintf("%s ~~ 0*%s", method, plan$latent), collapse = "\n"),
      sep = "\n"
    )
    plan$observed <- union(plan$observed, its)
    plan$name_map <- c(plan$name_map, stats::setNames("Method factor", method))
    plan$latent <- c(plan$latent, method)
  }
  plan$structure <- "method_factor"
  spec <- sieve_model_spec(plan, meta,
    kind = "method_factor", fun = "cfa", std_lv = TRUE,
    estimator = estimator
  )
  result <- sieve_run_model(spec, items_df)
  if (!is.null(result$fit)) {
    attr(result$fit, "method_factor") <- method
  }
  result
}

#' The Largest Modification Indices of a Fitted Model
#'
#' The rows the CFA tab lists as suggested paths: the modification indices
#' at or above a chi square with one degree of freedom at the .05 level,
#' largest first. Latent variables are shown under the researcher's labels,
#' not the names allocated for the engine. What lavaan says while computing
#' the indices is kept, and a failure is reported with its reason, never as
#' "none found". A candidate lavaan leaves without an index is computed by
#' the equivalent score test on the same fit, and one that cannot be
#' computed either way is named, never silently omitted.
#'
#' @details
#' Uses [lavaan::modindices()] without refitting the model. Modification
#' indices are exploratory diagnostics; selecting paths from them requires
#' substantive justification and subsequent evaluation. The fitted model's
#' assumptions carry through to this diagnostic. No values are rounded.
#'
#' `modindices()` divides the squared score of each candidate parameter by
#' its information given the model (the diagonal of a Schur complement) and
#' leaves the index `NA` where that information falls under a fixed
#' tolerance, `.Machine$double.eps^(1/3)` (about 6e-6), then removes those
#' rows by default. The tolerance is absolute and the information is in the
#' units of the parameter: for a residual covariance it scales with the
#' reciprocal of the product of the two items' variances, so on raw score
#' scales with large variances (the bundled speeded tests, with variances
#' from 410 to 1320) it falls under the tolerance although the index, a
#' score test, does not depend on the scale of the items (the same model on
#' standardized items lists the row). For every such candidate this function
#' asks [lavaan::lavTestScore()] for the univariate score test of freeing
#' that one parameter on the same fit, with its expected parameter change in
#' the parameter's own units and the fully standardized change; that routine
#' inverts the information in full and is not subject to the tolerance, and
#' it agrees with `modindices()` to at least five significant figures on the
#' rows both compute. Parameters fixed by identification (a latent variance
#' fixed at 1, a marker loading) are not candidates. The recovery runs for
#' single-group, single-level fits; elsewhere a candidate without an index
#' is reported in `dropped`.
#'
#' A multiple-group or multilevel model has an index for each candidate in
#' each group and at each level, and the same path can be large in one group
#' and small in another; the table then names the `Group` and the `Level`
#' of every row, and `recovered` and `dropped` add them to the path, for
#' example `"y1 ~~ y2 (level: cluster)"`.
#'
#' `Type` says what freeing the parameter would add, from its operator and
#' its variables: `"cross loading"` (`=~`), `"regression path"` (`~`),
#' `"correlated residual"` (`~~` between two observed variables),
#' `"latent covariance"` (`~~` between two latent variables, neither
#' regressed on another variable), `"latent residual covariance"` (`~~`
#' between two latent variables when either is an outcome of a regression,
#' so the covariance involves its disturbance), `"observed and latent
#' covariance"` (`~~` between an observed and a latent variable),
#' `"variance"` (`~~` of a variable with itself), `"intercept"` (`~1` of an
#' observed variable), and `"latent mean"` (`~1` of a latent variable).
#' @param fit A fitted lavaan object with a solution.
#' @param minimum The smallest index listed.
#' @param top The largest number of rows returned.
#' @return A list with `table` (a data frame with `Group` for a
#'   multiple-group model and `Level` for a multilevel model, then `Path`,
#'   `Type`, `MI`, `EPC`, and `Std.EPC` at full precision, or `NULL` when
#'   there is nothing to list), `note` (why there is no table, or `NULL`),
#'   `recovered` (the paths, with the labels, whose indices came from the
#'   score test because `modindices()` left them without one), `dropped`
#'   (the candidate paths that could not be computed either way), and
#'   `warnings`.
#' @author Ken Kelley
#' @examples
#' fit <- lavaan::cfa("visual =~ x1 + x2 + x3
#'                     textual =~ x4 + x5 + x6
#'                     speed =~ x7 + x8 + x9",
#'   data = lavaan::HolzingerSwineford1939, std.lv = TRUE)
#' sieve_modification_indices(fit, top = 5L)$table
#' # On a raw score scale with large variances lavaan leaves the residual
#' # covariances of the speeded tests without an index; the score test on
#' # the same fit recovers them, and $recovered names them.
#' raw <- lavaan::HolzingerSwineford1939
#' raw$x7 <- raw$x7 * 25
#' raw$x8 <- raw$x8 * 20
#' raw$x9 <- raw$x9 * 30
#' fit_raw <- lavaan::cfa("visual =~ x1 + x2 + x3
#'                         textual =~ x4 + x5 + x6
#'                         speed =~ x7 + x8 + x9", data = raw, std.lv = TRUE)
#' sieve_modification_indices(fit_raw, top = 5L)[c("table", "recovered")]
#' @export
sieve_modification_indices <- function(fit, minimum = 3.84, top = 25L) {
  scored <- sieve_capture(lavaan::modindices(fit, sort = FALSE, na_remove = FALSE))
  out <- list(
    table = NULL, note = NULL, recovered = character(0), dropped = character(0),
    warnings = scored$warnings
  )
  if (!scored$ok) {
    out$note <- sprintf(
      "lavaan could not compute modification indices for this model: %s",
      scored$error
    )
    return(out)
  }
  none <- sprintf(
    "No modification indices at or above %s (chi square, 1 df).",
    format(minimum)
  )
  mi <- scored$value
  if (is.null(mi) || !nrow(mi)) {
    out$note <- none
    return(out)
  }
  path <- function(rows) {
    sprintf(
      "%s %s %s", sieve_show_names(rows$lhs, fit), rows$op,
      sieve_show_names(rows$rhs, fit)
    )
  }
  # The group and level of each row, from lavaan's block (blocks run by
  # group, and within a group by level); NULL for a single group or level.
  n_groups <- lavaan::lavInspect(fit, "ngroups")
  n_levels <- lavaan::lavInspect(fit, "nlevels")
  where <- function(rows) {
    block <- rows[["block"]] %||% rep(1L, nrow(rows))
    list(
      group = if (n_groups > 1L) lavaan::lavInspect(fit, "group.label")[(block - 1L) %/% n_levels + 1L],
      level = if (n_levels > 1L) lavaan::lavInspect(fit, "level.label")[(block - 1L) %% n_levels + 1L]
    )
  }
  # A path in the recovered and dropped lists, with its group and level.
  named <- function(rows) {
    at <- where(rows)
    suffix <- paste0(
      if (!is.null(at$group)) paste0("group: ", at$group),
      if (!is.null(at$group) && !is.null(at$level)) ", ",
      if (!is.null(at$level)) paste0("level: ", at$level)
    )
    if (is.null(at$group) && is.null(at$level)) path(rows) else sprintf("%s (%s)", path(rows), suffix)
  }
  # The candidates lavaan left without an index (see Details): computed by
  # the score test on the same fit where that is possible, and named.
  missing <- is.na(mi$mi) & sieve_mi_candidate(mi, fit)
  if (any(missing)) {
    rows <- mi[missing, , drop = FALSE]
    got <- sieve_mi_recover(fit, rows)
    mi$mi[missing] <- got$mi
    mi$epc[missing] <- got$epc
    mi$sepc.all[missing] <- got$sepc.all
    out$warnings <- unique(c(out$warnings, got$warnings))
    out$recovered <- named(rows[!is.na(got$mi), , drop = FALSE])
    out$dropped <- named(rows[is.na(got$mi), , drop = FALSE])
  }
  mi <- mi[!is.na(mi$mi) & mi$mi >= minimum, , drop = FALSE]
  if (!nrow(mi)) {
    out$note <- none
    return(out)
  }
  mi <- utils::head(mi[order(mi$mi, decreasing = TRUE), , drop = FALSE], top)
  out$table <- data.frame(
    Path = path(mi),
    Type = sieve_mi_type(mi, fit),
    MI = mi$mi, EPC = mi$epc, Std.EPC = mi$sepc.all,
    stringsAsFactors = FALSE
  )
  at <- where(mi)
  if (!is.null(at$level)) out$table <- data.frame(Level = at$level, out$table, stringsAsFactors = FALSE)
  if (!is.null(at$group)) out$table <- data.frame(Group = at$group, out$table, stringsAsFactors = FALSE)
  out
}

#' What freeing each modification index row would add
#'
#' @param mi Rows of the table from [lavaan::modindices()] (`lhs`, `op`,
#'   `rhs`).
#' @param fit The fitted lavaan object.
#' @return A character vector along the rows of `mi` (see
#'   [sieve_modification_indices()] for the values).
#' @author Ken Kelley
#' @noRd
sieve_mi_type <- function(mi, fit) {
  latent <- lavaan::lavNames(fit, "lv")
  pt <- lavaan::parTable(fit)
  outcomes <- unique(pt$lhs[pt$op == "~"])
  lhs_latent <- mi$lhs %in% latent
  rhs_latent <- mi$rhs %in% latent
  covariance <- mi$op == "~~"
  both <- covariance & lhs_latent & rhs_latent
  type <- sprintf("parameter (%s)", mi$op)
  type[mi$op == "=~"] <- "cross loading"
  type[mi$op == "~"] <- "regression path"
  type[covariance & !lhs_latent & !rhs_latent] <- "correlated residual"
  type[covariance & xor(lhs_latent, rhs_latent)] <- "observed and latent covariance"
  type[both] <- ifelse(mi$lhs[both] %in% outcomes | mi$rhs[both] %in% outcomes,
    "latent residual covariance", "latent covariance"
  )
  type[covariance & mi$lhs == mi$rhs] <- "variance"
  type[mi$op == "~1"] <- ifelse(lhs_latent[mi$op == "~1"], "latent mean", "intercept")
  type
}

#' Which rows of a modification index table are candidate parameters
#'
#' A candidate is a cross loading or a covariance between two different
#' variables that the model fixes at zero. A parameter fixed at another
#' value is an identification constraint (a latent variance fixed at 1, a
#' marker loading) and is not one, whatever lavaan lists for it; nor is an
#' intercept.
#'
#' @param mi The table from [lavaan::modindices()] (`lhs`, `op`, `rhs`).
#' @param fit The fitted lavaan object.
#' @return A logical vector along the rows of `mi`.
#' @author Ken Kelley
#' @noRd
sieve_mi_candidate <- function(mi, fit) {
  ok <- mi$op %in% c("=~", "~~") & mi$lhs != mi$rhs
  pt <- lavaan::parTable(fit)
  fixed <- pt[pt$free == 0L & !is.na(pt$est) & pt$est != 0, , drop = FALSE]
  if (nrow(fixed)) {
    key <- function(lhs, op, rhs) paste(lhs, op, rhs)
    fixed_keys <- c(
      key(fixed$lhs, fixed$op, fixed$rhs),
      key(fixed$rhs, fixed$op, fixed$lhs)[fixed$op == "~~"]
    )
    ok <- ok & !(key(mi$lhs, mi$op, mi$rhs) %in% fixed_keys)
  }
  ok
}

#' Modification indices by the score test, for rows lavaan left without one
#'
#' For each row, the univariate score test of freeing that one parameter on
#' the fitted model ([lavaan::lavTestScore()] with `add`), with its expected
#' parameter change and the fully standardized change. The statistic is the
#' modification index (the same score test), computed from the full inverse
#' of the extended model's information rather than from a Schur complement
#' diagonal checked against a fixed tolerance, so it is defined where
#' `modindices()` returns `NA` for reasons of scale. The model is not refit:
#' lavaan evaluates the extended model at the estimates in hand. Only
#' single-group, single-level fits are handled; for any other fit every
#' value is `NA`.
#'
#' @param fit A fitted lavaan object.
#' @param rows A data frame with `lhs`, `op`, and `rhs`.
#' @return A list with `mi`, `epc`, and `sepc.all` (numeric, along the rows,
#'   `NA` where the test could not be computed) and `warnings`.
#' @author Ken Kelley
#' @noRd
sieve_mi_recover <- function(fit, rows) {
  n <- nrow(rows)
  out <- list(
    mi = rep(NA_real_, n), epc = rep(NA_real_, n), sepc.all = rep(NA_real_, n),
    warnings = character(0)
  )
  single <- isTRUE(lavaan::lavInspect(fit, "ngroups") == 1L) &&
    isTRUE(lavaan::lavInspect(fit, "nlevels") == 1L)
  if (!n || !single) {
    return(out)
  }
  for (i in seq_len(n)) {
    term <- sprintf("%s %s %s", rows$lhs[i], rows$op[i], rows$rhs[i])
    got <- sieve_capture(lavaan::lavTestScore(fit,
      add = term, univariate = TRUE, epc = TRUE, standardized = TRUE
    ))
    out$warnings <- unique(c(out$warnings, got$warnings))
    ts <- got$value
    if (!got$ok || is.null(ts$uni) || !nrow(ts$uni) || is.null(ts$epc)) {
      next
    }
    stat <- ts$uni$X2[1L]
    e <- ts$epc
    same <- e$op == rows$op[i] & e$lhs == rows$lhs[i] & e$rhs == rows$rhs[i]
    if (rows$op[i] == "~~") {
      same <- same | (e$op == "~~" & e$lhs == rows$rhs[i] & e$rhs == rows$lhs[i])
    }
    # The added parameter is constrained to zero in the extended model, so
    # lavaan lists it as not free; the row is the one that carries a change.
    hit <- which(same & !is.na(e$epc))
    if (!is.finite(stat) || !length(hit)) {
      next
    }
    out$mi[i] <- stat
    out$epc[i] <- e$epc[hit[1L]]
    out$sepc.all[i] <- e$sepc.all[hit[1L]]
  }
  out
}

#' What the modification index panel says about the rows lavaan left without an index
#'
#' @param mi The list from [sieve_modification_indices()].
#' @return A character vector of sentences, or `NULL` when lavaan computed
#'   every candidate.
#' @author Ken Kelley
#' @noRd
sieve_mi_recovery_text <- function(mi) {
  n_rec <- length(mi$recovered)
  n_drop <- length(mi$dropped)
  if (!n_rec && !n_drop) {
    return(NULL)
  }
  out <- character(0)
  if (n_rec) {
    out <- c(out, sprintf(paste0(
      "lavaan's modindices() left %d candidate path%s without an index (%s): the ",
      "information for each fell under lavaan's fixed tolerance, which happens on raw ",
      "score scales with large variances although the index itself does not depend on ",
      "the scale of the items. SIEVER computed %s by the equivalent score test on the ",
      "same fit, lavaan::lavTestScore(), with the EPC in the items' own units, and %s ",
      "ranked with the other candidates."
    ),
    n_rec, if (n_rec == 1L) "" else "s", paste(mi$recovered, collapse = "; "),
    if (n_rec == 1L) "it" else "them", if (n_rec == 1L) "it is" else "they are"
    ))
  }
  if (n_drop) {
    out <- c(out, sprintf(
      "No index could be computed for %d candidate path%s, so %s not listed: %s.",
      n_drop, if (n_drop == 1L) "" else "s", if (n_drop == 1L) "it is" else "they are",
      paste(mi$dropped, collapse = "; ")
    ))
  }
  out
}

#' The Bollen-Stine Bootstrap Test of Exact Fit for a Specified CFA
#'
#' Refits the measurement model on screen by maximum likelihood and gives
#' its chi square test of exact fit the reference distribution of the model
#' based bootstrap of Bollen and Stine (1992), computed by
#' [DMAR::sem_exact_fit_bootstrap()]: the data are transformed so that the
#' fitted model holds in them exactly, resampled, and the model is refit to
#' each resample; the bootstrap p-value is the share of the resampled
#' statistics at least as large as the observed one. With complete data the
#' refit is ML on the complete cases and the transformation is Bollen and
#' Stine's. When an item (or a declared auxiliary variable) has a missing
#' value the refit is FIML on every respondent with an observed value, the
#' transformation is the extension of Savalei and Yuan (2009) under which
#' the saturated FIML estimates of the transformed data are the model
#' implied moments (`transformation = "common"`, one matrix for every
#' missing data pattern, or `"pattern"`, each pattern on its own), and each
#' resample keeps the missing values of the rows it draws, so no available
#' response is deleted. The refit needs at least 30 respondents: complete
#' cases with complete data, respondents with an observed item otherwise.
#'
#' Declared auxiliary variables enter the FIML refit as saturated
#' correlates (Graham, 2003), so the bootstrap tests the statistic of the
#' model fit with them, and the transformation moves them with the items.
#' DMAR 1.1.0 judges a fit and each resample admissible with lavaan's
#' post-estimation check, which in that parameterization also tests the
#' covariance matrix of the item residuals and the auxiliary variables
#' together; that joint matrix can be indefinite when an auxiliary variable
#' relates to a factor, although the model's own parameters are proper
#' (SIEVER judges them apart from the auxiliary block; see
#' `sieve_model_post_check()`). So with auxiliary variables the resamples
#' DMAR counts as improper are only flagged by lavaan's check, they are
#' always kept in the p-value (`improper = "drop"` is refused, since
#' setting them aside would select the resamples by a check of the
#' auxiliary block), and a refit that is proper in its own parameters but
#' flagged by lavaan's check is refused with that reason before DMAR is
#' called.
#'
#' The refit goes through [sieve_run_model()], so the latent names are the
#' ones the specification allocated, the fitted model is checked against
#' the intended variables, and the result carries a status: a p-value is
#' reported only for a model that has a solution. DMAR counts every
#' resample: `B_used` (those in the p-value), `B_improper` (converged to an
#' inadmissible solution; counted in the p-value under `improper = "keep"`,
#' set aside under `"drop"`), and `B_failed` (did not converge or raised an
#' error). DMAR reports no bootstrap p-value when fewer than half of the
#' attempted resamples, or fewer than two, were used. A refit DMAR refuses
#' (ordered indicators, an estimator other than ML, two levels, sampling
#' weights, a two-stage fit, observed exogenous variables held fixed) is
#' returned with DMAR's reason in `error` and `refused = TRUE`; a refusal
#' of class `dmar_refusal` keeps its classes in `error_class`.
#'
#' The seed is resolved by SIEVER (a supplied one is used as given; with
#' `seed = NULL` one integer is drawn from the caller's stream and
#' recorded) and passed to DMAR, which sets it for the call only and puts
#' the caller's random number state back. With complete data DMAR draws the
#' same resamples as `lavaan::bootstrapLavaan(type = "bollen.stine")` given
#' the same seed as its `iseed`.
#'
#' @param spec The specification of the model (see [sieve_cfa_spec()]).
#' @param data A data frame holding the item columns.
#' @param B The number of bootstrap samples: 1000 by default,
#'   `DMAR::sem_exact_fit_bootstrap()`'s default.
#' @param seed A seed, or `NULL` to draw one from the caller's stream.
#' @param transformation For incomplete data, `"common"` (the default) or
#'   `"pattern"`; see [DMAR::sem_exact_fit_bootstrap()]. Ignored with
#'   complete data.
#' @param improper `"keep"` (the default) or `"drop"`: whether a resample
#'   whose refit converged to an inadmissible solution counts in the
#'   p-value.
#' @param conf_level The confidence level of the Wilson interval for the
#'   Monte Carlo error of the bootstrap p-value.
#' @return A list with `error` (why nothing was run or reported, or
#'   `NULL`), `refused` (`TRUE` when SIEVER or DMAR refused the request),
#'   `error_class`, `status` (the status of the refit, or `NULL` when
#'   nothing was fit), `warnings`, `chisq`, `df`, `p_ml` (the normal theory
#'   p-value), `p_bs` (the Bollen-Stine p-value), `p_bs_lower` and
#'   `p_bs_upper` (its Monte Carlo interval), `bootstrap_mean`, `B`,
#'   `B_used`, `B_improper`, `B_failed`, `n` (the respondents the refit
#'   used), `groups`, `missing` (`"fiml"` or `"listwise"`),
#'   `transformation` (the one DMAR applied: `"bollen_stine"`, `"common"`,
#'   or `"pattern"`), `transformation_requested`, `aux` (the auxiliary
#'   variables in the refit), `transformation_check`
#'   (DMAR's record of it per
#'   group), `improper`, `conf_level`, `method`, `table` (DMAR's table, at
#'   full precision), `rng` (see `sieve_draw_seed()`), and `spec` (the
#'   specification that was fit). The statistics are `NA` unless the refit
#'   has a solution and DMAR ran.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' spec <- sieve_cfa_spec(meta, reserved = names(items))
#' cat(spec$model, "\n")
#' # One hundred replications meet DMAR's minimum for a bootstrap p-value.
#' # They refit the model 100 times, which takes a second or two, so the
#' # bootstrap runs in an interactive session only.
#' if (interactive()) {
#'   bs <- sieve_bollen_stine(spec, items, B = 100L, seed = 1)
#'   bs[c("chisq", "df", "p_ml", "p_bs", "B", "B_used", "n")]
#' }
#' @export
sieve_bollen_stine <- function(spec, data, B = 1000L, seed = NULL,
                               transformation = c("common", "pattern"),
                               improper = c("keep", "drop"), conf_level = 0.95) {
  plan <- sieve_bollen_stine_plan(spec, data,
    B = B, seed = seed,
    transformation = transformation, improper = improper, conf_level = conf_level
  )
  if (!is.null(plan$result)) {
    return(plan$result)
  }
  sieve_bollen_stine_read(plan, sieve_bollen_stine_engine(plan$fit, plan$args))
}

#' Prepare a Bollen-Stine bootstrap: the checks, the refit, and DMAR's arguments
#'
#' Everything [sieve_bollen_stine()] does before DMAR's bootstrap runs, so
#' the CFA tab can run the bootstrap itself in a background process (see
#' `sieve_bollen_stine_start()`) and read it with
#' `sieve_bollen_stine_read()`. Nothing random happens here except the
#' resolution of the seed.
#'
#' @inheritParams sieve_bollen_stine
#' @return A list with `result` (the finished result when nothing is left
#'   to run: a refusal, or a refit without a solution; otherwise `NULL`),
#'   `out` (the result so far), `fit` (the refit), `args` (the arguments of
#'   `DMAR::sem_exact_fit_bootstrap()` other than the fit), and `kind` (the
#'   [RNGkind()] settings the seed belongs to).
#' @author Ken Kelley
#' @noRd
sieve_bollen_stine_plan <- function(spec, data, B = 1000L, seed = NULL,
                                    transformation = c("common", "pattern"),
                                    improper = c("keep", "drop"), conf_level = 0.95) {
  transformation <- match.arg(transformation)
  improper <- match.arg(improper)
  conf_level <- sieve_check_conf_level(conf_level)
  out <- list(
    error = NULL, refused = FALSE, error_class = NULL, status = NULL,
    warnings = character(0), chisq = NA_real_, df = NA_real_, p_ml = NA_real_,
    p_bs = NA_real_, p_bs_lower = NA_real_, p_bs_upper = NA_real_,
    bootstrap_mean = NA_real_, B = B, B_used = NA_integer_,
    B_improper = NA_integer_, B_failed = NA_integer_, n = NA_integer_,
    groups = NA_integer_, missing = NA_character_, transformation = NA_character_,
    transformation_requested = transformation, aux = character(0),
    transformation_check = NULL, improper = improper, conf_level = conf_level,
    method = NULL, table = NULL, rng = NULL, spec = NULL
  )
  done <- function(why = NULL, refused = TRUE) {
    out$error <- why
    out$refused <- refused && !is.null(why)
    list(result = out)
  }
  design <- spec$sampling_design$unit %||% "independent"
  args <- sieve_spec_args(spec)
  if (!identical(design, "independent") || !is.null(args$cluster)) {
    return(done("The Bollen-Stine implementation resamples independent rows. A clustered or repeated declaration needs a design-matched bootstrap; no row bootstrap was run."))
  }
  if (!is.null(args$sampling.weights)) {
    return(done("The Bollen-Stine bootstrap resamples equally weighted rows, and this model carries sampling weights; no bootstrap was run."))
  }
  if (length(spec$ordered)) {
    return(done(paste0(
      "The Bollen-Stine bootstrap is defined for maximum likelihood on continuous items; ",
      "this model uses the categorical (WLSMV) estimator."
    )))
  }
  if (!nzchar(spec$model)) {
    return(done("No estimable model syntax; check the construct assignments."))
  }
  request <- sieve_dmar_boot_request(B, method = "p", conf_level = conf_level)
  if (!request$ok) return(done(request$message))
  out$B <- as.integer(B)
  # The values are read as numbers once; a column that cannot be read is
  # refused with the reason, as in sieve_run_model().
  columns <- unique(c(spec$observed, spec$engine_args$aux))
  absent <- setdiff(columns, names(data))
  if (length(absent)) {
    return(done(paste("The Bollen-Stine input is missing:", paste(absent, collapse = ", "))))
  }
  read <- sieve_numeric_columns(data, columns)
  if (nrow(read$problems)) {
    out$status <- sieve_fit_status(NULL, error = paste(read$text, collapse = " "))
    return(done(paste(read$text, collapse = " ")))
  }
  d <- read$data
  incomplete <- anyNA(d)
  if (!incomplete && nrow(d) < 30L) {
    out$n <- nrow(d)
    return(done(sprintf("Only %d complete cases; too few to bootstrap.", nrow(d))))
  }
  if (incomplete) {
    # The same floor for a FIML refit: the respondents it can use.
    available <- sum(rowSums(!is.na(d[, spec$observed, drop = FALSE])) > 0L)
    if (available < 30L) {
      out$n <- available
      return(done(sprintf("Only %d respondents with an observed item; too few to bootstrap.", available)))
    }
  }
  # Auxiliary variables enter a FIML refit only (with complete data the
  # refit uses the complete cases and they play no part).
  aux <- if (incomplete) as.character(spec$engine_args$aux %||% character(0)) else character(0)
  out$aux <- aux
  if (length(aux) && identical(improper, "drop")) {
    return(done(sprintf(paste(
      "Set aside is not available with auxiliary variables (%s): DMAR 1.1.0 calls a resample",
      "improper by lavaan's check of the item residuals and the auxiliary variables together,",
      "which flags resamples whose model parameters are proper, so setting them aside would",
      "select the resamples by the auxiliary variables. Choose Keep in the p-value under",
      "Improper resamples."
    ), paste(aux, collapse = ", "))))
  }
  # The refit: ML, the standard test statistic (the one the bootstrap
  # resamples), FIML when a value is missing and the complete cases
  # otherwise. Its key is its own, so it is never mistaken for the fit on
  # screen.
  spec_bs <- unclass(spec)
  spec_bs$kind <- "bollen_stine"
  spec_bs$estimator <- "ML"
  spec_bs$estimator_choice <- if (incomplete) "ml_fiml" else "ml"
  spec_bs$missing <- if (incomplete) "fiml" else "listwise"
  spec_bs$engine_args$se <- NULL
  spec_bs$engine_args$test <- NULL
  spec_bs$engine_args$bootstrap <- NULL
  spec_bs$key <- NULL
  spec_bs$key <- sieve_hash(spec_bs)
  spec_bs <- structure(spec_bs, class = "sieve_spec")
  out$spec <- spec_bs
  out$missing <- spec_bs$missing
  fitted <- sieve_run_model(spec_bs, d)
  out$status <- fitted$status
  out$warnings <- fitted$warnings
  if (is.null(fitted$fit)) {
    out$error <- sprintf("The bootstrap refit could not be fit: %s", fitted$error %||% "no reason was given.")
    return(list(result = out))
  }
  if (!sieve_has_solution(fitted)) {
    # No test statistic describes a model that did not converge or is not
    # identified; the status says which.
    return(list(result = out))
  }
  flagged <- sieve_bollen_stine_aux_flag(fitted$fit, aux)
  if (!is.null(flagged)) {
    return(done(flagged))
  }
  out$n <- as.integer(sum(lavaan::lavInspect(fitted$fit, "nobs")))
  out$rng <- sieve_draw_seed(seed)
  list(
    result = NULL, out = out, fit = fitted$fit, kind = out$rng$kind,
    args = list(
      B = as.integer(B), transformation = transformation, improper = improper,
      conf_level = conf_level, seed = out$rng$seed, ncpus = 1L
    )
  )
}

#' Why DMAR would refuse a refit whose own parameters are proper
#'
#' With auxiliary variables lavaan's post-estimation check, which DMAR 1.1.0
#' applies to the fit before it bootstraps, also tests the covariance matrix
#' of the item residuals and the auxiliary variables together. When that
#' check fails although the model's own parameters are proper (SIEVER's
#' `sieve_model_post_check()`), DMAR would refuse the fit as having a
#' negative variance or a latent covariance matrix that is not positive
#' definite, neither of which is the case; this says what is.
#'
#' @param fit The bootstrap refit.
#' @param aux The auxiliary variables in it.
#' @return `NULL`, or the reason the bootstrap is not run.
#' @author Ken Kelley
#' @noRd
sieve_bollen_stine_aux_flag <- function(fit, aux) {
  if (!length(aux)) return(NULL)
  lavaan_ok <- isTRUE(tryCatch(suppressWarnings(lavaan::lavInspect(fit, "post.check")), error = function(e) FALSE))
  own_ok <- isTRUE(tryCatch(sieve_model_post_check(fit), error = function(e) FALSE))
  if (lavaan_ok || !own_ok) return(NULL)
  theta <- lavaan::lavInspect(fit, "theta")
  if (is.matrix(theta)) theta <- list(theta)
  smallest <- min(vapply(theta, function(x) {
    x <- unclass(x)
    s <- sqrt(pmax(diag(x), .Machine$double.eps))
    min(eigen(x / outer(s, s), symmetric = TRUE, only.values = TRUE)$values)
  }, numeric(1)))
  sprintf(paste(
    "The bootstrap was not run. With the auxiliary variable%s %s as saturated correlates,",
    "the covariance matrix of the item residuals and the auxiliary variables together is not",
    "positive definite (smallest eigenvalue of its correlation form %s), although the model's",
    "own parameters are proper: no variance is negative, and the residual covariance matrix of",
    "the items and the latent covariance matrix are positive definite. This happens when an",
    "auxiliary variable relates to a factor, whose covariance the saturated correlates carry",
    "through the residuals. DMAR 1.1.0 judges the fit with lavaan's check of the whole matrix",
    "and would refuse it as inadmissible. To run the bootstrap, remove the auxiliary role; the",
    "refit is then FIML on the model's variables alone, which assumes that the missingness does",
    "not depend on the auxiliary variables."
  ), if (length(aux) == 1L) "" else "s", paste(aux, collapse = ", "), format(signif(smallest, 3)))
}

#' The missing data patterns too small for the pattern-wise transformation
#'
#' Savalei and Yuan's pattern-wise transformation, as DMAR computes it,
#' needs every missing data pattern to have more respondents than observed
#' variables.
#'
#' @param fit The bootstrap refit.
#' @return The number of patterns (over groups) with no more respondents
#'   than observed variables.
#' @author Ken Kelley
#' @noRd
sieve_bollen_stine_small_patterns <- function(fit) {
  X <- lavaan::lavInspect(fit, "data")
  if (!is.list(X) || is.data.frame(X)) X <- list(X)
  sum(vapply(X, function(x) {
    seen <- !is.na(as.matrix(x))
    key <- apply(seen * 1L, 1L, paste, collapse = "")
    counts <- table(key)
    observed <- nchar(gsub("0", "", names(counts), fixed = TRUE))
    sum(as.integer(counts) <= observed)
  }, numeric(1)))
}

#' Run DMAR's Bollen-Stine bootstrap and keep what it signaled
#'
#' Calls `DMAR::sem_exact_fit_bootstrap()` with the prepared arguments,
#' keeping its warnings and, when it stops, its message, condition classes,
#' and `reason` element. It uses base R and DMAR only, so the same function
#' runs in the Shiny process and in a background worker that has loaded
#' DMAR alone (see `sieve_bollen_stine_start()`).
#'
#' @param fit The lavaan refit from `sieve_bollen_stine_plan()`.
#' @param args The other arguments of `DMAR::sem_exact_fit_bootstrap()`.
#' @param kind The [RNGkind()] settings to run under, or `NULL` for the
#'   current ones.
#' @return A list with `value` (DMAR's table, or `NULL`), `ok`, `error`,
#'   `error_class`, `reason`, and `warnings`.
#' @author Ken Kelley
#' @noRd
sieve_bollen_stine_engine <- function(fit, args, kind = NULL) {
  seen <- new.env(parent = emptyenv())
  seen$warnings <- character(0)
  if (length(kind) == 3L) {
    old <- RNGkind()
    on.exit(RNGkind(old[1L], old[2L], old[3L]), add = TRUE)
    RNGkind(kind[1L], kind[2L], kind[3L])
  }
  value <- tryCatch(
    withCallingHandlers(
      do.call(DMAR::sem_exact_fit_bootstrap, c(list(fit = fit), args)),
      warning = function(w) {
        seen$warnings <- c(seen$warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) invokeRestart("muffleMessage")
    ),
    error = function(e) {
      seen$error <- conditionMessage(e)
      seen$error_class <- class(e)
      seen$reason <- if (is.list(e) && !is.null(e[["reason"]])) as.character(e[["reason"]]) else NULL
      NULL
    }
  )
  list(
    value = value, ok = is.null(seen$error), error = seen$error,
    error_class = seen$error_class, reason = seen$reason,
    warnings = unique(seen$warnings)
  )
}

#' Start DMAR's Bollen-Stine bootstrap in a background process
#'
#' DMAR 1.1.0's `sem_exact_fit_bootstrap()` has no progress or cancellation
#' hook, so the CFA tab runs it in a supervised R subprocess, as the EFA
#' tab runs its fits by number of factors: the session stays responsive,
#' the elapsed time is shown, and Cancel ends the process. The worker loads
#' the same DMAR namespace the session uses (see `sieve_worker_paths()`)
#' and runs `sieve_bollen_stine_engine()`; only the refit and plain
#' arguments cross the process boundary.
#'
#' @param plan A plan from `sieve_bollen_stine_plan()` without a `result`.
#' @return A callr background process whose result is the list
#'   `sieve_bollen_stine_engine()` returns.
#' @author Ken Kelley
#' @noRd
sieve_bollen_stine_start <- function(plan) {
  paths <- sieve_worker_paths()["DMAR"]
  bootstrap <- sieve_worker_bootstrap
  environment(bootstrap) <- baseenv()
  engine <- sieve_bollen_stine_engine
  environment(engine) <- baseenv()
  callr::r_bg(
    function(fit, args, kind, paths, bootstrap, engine) {
      bootstrap(paths)
      requireNamespace("lavaan", quietly = TRUE)
      engine(fit, args, kind)
    },
    args = list(
      fit = plan$fit, args = plan$args, kind = plan$kind, paths = paths,
      bootstrap = bootstrap, engine = engine
    ),
    # Nothing reads the worker's console during a run, so its output is
    # discarded rather than piped (a full pipe would stall a long run); the
    # result, and any error, come back through callr's result file.
    libpath = .libPaths(), supervise = TRUE, user_profile = FALSE,
    stdout = NULL, stderr = NULL
  )
}

#' Read DMAR's Bollen-Stine bootstrap into SIEVER's result
#'
#' @param plan A plan from `sieve_bollen_stine_plan()`.
#' @param run What `sieve_bollen_stine_engine()` returned.
#' @return The result list described in [sieve_bollen_stine()].
#' @author Ken Kelley
#' @noRd
sieve_bollen_stine_read <- function(plan, run) {
  out <- plan$out
  out$warnings <- unique(c(out$warnings, run$warnings))
  if (!isTRUE(run$ok) || is.null(run$value)) {
    out$refused <- TRUE
    out$error_class <- run$error_class
    # One sentence whatever DMAR's condition: every refusal of
    # sem_exact_fit_bootstrap() is classed (dmar_refusal), the refit's and
    # the request's alike; the class is kept above.
    out$error <- sprintf("DMAR did not run the bootstrap: %s",
      sub("[.]?$", ".", run$error %||% "no reason was given"))
    # DMAR names its arguments; the page names its controls. When SIEVER
    # finds a pattern too small for the pattern-wise transformation (its own
    # count, not DMAR's words), it names the control to change.
    small <- if (identical(plan$args$transformation, "pattern") && inherits(plan$fit, "lavaan")) {
      tryCatch(sieve_bollen_stine_small_patterns(plan$fit), error = function(e) 0L)
    } else 0L
    if (small > 0L) {
      out$error <- paste(out$error, sprintf(paste(
        "Under Missing responses, choose One matrix for every pattern (Savalei & Yuan): the",
        "pattern-wise transformation needs every missing data pattern to have more respondents",
        "than observed variables, and %d pattern%s here %s not."
      ), small, if (small == 1L) "" else "s", if (small == 1L) "does" else "do"))
    }
    return(out)
  }
  tbl <- run$value
  sieve_dmar_require_terms(tbl, c("chi_square", "df", "p_chi_square", "p_bootstrap", "N", "n_groups"), "Bollen-Stine bootstrap")
  v <- stats::setNames(tbl$value, tbl$term)
  pick <- function(term) if (term %in% names(v)) unname(v[[term]]) else NA_real_
  out$chisq <- pick("chi_square")
  out$df <- pick("df")
  out$p_ml <- pick("p_chi_square")
  out$p_bs <- pick("p_bootstrap")
  out$p_bs_lower <- pick("p_bootstrap_ci_lower")
  out$p_bs_upper <- pick("p_bootstrap_ci_upper")
  out$bootstrap_mean <- pick("bootstrap_mean")
  out$B <- as.integer(pick("B"))
  out$B_used <- as.integer(pick("B_used"))
  out$B_improper <- as.integer(pick("B_improper"))
  out$B_failed <- as.integer(pick("B_failed"))
  out$n <- as.integer(pick("N"))
  out$groups <- as.integer(pick("n_groups"))
  record <- attr(tbl, "sem_fit")
  out$transformation <- paste(record$transformation %||% NA_character_, collapse = ", ")
  out$transformation_check <- attr(tbl, "transformation")
  out$table <- tbl
  out$method <- sieve_bollen_stine_method(out)
  out
}

#' The sentence that says how a Bollen-Stine bootstrap was computed
#'
#' @param r A result from [sieve_bollen_stine()].
#' @return One string.
#' @author Ken Kelley
#' @noRd
sieve_bollen_stine_method <- function(r) {
  aux <- if (length(r$aux)) {
    sprintf(
      ", the auxiliary variable%s %s entering as saturated correlates (Graham, 2003),",
      if (length(r$aux) == 1L) "" else "s", paste(r$aux, collapse = ", ")
    )
  } else ""
  how <- switch(r$transformation %||% "",
    bollen_stine = sprintf(
      "ML on the %d complete cases with Bollen and Stine's (1992) transformation", r$n
    ),
    common = sprintf(paste0(
      "FIML on the %d respondents%s with Savalei and Yuan's (2009) transformation for ",
      "incomplete data (one matrix for every missing data pattern, found with the items in ",
      "standard deviation units, so the result does not depend on their units); each ",
      "resample keeps the missing values of the rows it draws"
    ), r$n, aux),
    pattern = sprintf(paste0(
      "FIML on the %d respondents%s with Savalei and Yuan's (2009) transformation for ",
      "incomplete data (each missing data pattern on its own); each resample keeps the ",
      "missing values of the rows it draws"
    ), r$n, aux),
    sprintf("ML on the %d respondents", r$n)
  )
  paste0("DMAR::sem_exact_fit_bootstrap(): ", how)
}

#' The words for the resamples a Bollen-Stine bootstrap counted
#'
#' @param r A result from [sieve_bollen_stine()] with DMAR's counts.
#' @return One sentence.
#' @author Ken Kelley
#' @noRd
sieve_bollen_stine_counts <- function(r) {
  if (length(r$aux)) {
    # With auxiliary variables DMAR's improper count is lavaan's check of the
    # item residuals and the auxiliary variables together, not of the
    # model's own parameters (see sieve_bollen_stine()).
    return(sprintf(paste(
      "%d resamples attempted, %d used in the p-value, %d flagged by lavaan's admissibility",
      "check (kept; with the auxiliary variables the check also tests their covariances with",
      "the item residuals, so these are not necessarily improper in the model's own",
      "parameters), %d failed (did not converge or stopped with an error)."
    ), r$B, r$B_used, r$B_improper, r$B_failed))
  }
  sprintf(
    "%d resamples attempted, %d used in the p-value, %d improper (%s), %d failed (did not converge or stopped with an error).",
    r$B, r$B_used, r$B_improper,
    if (identical(r$improper, "drop")) "set aside" else "kept",
    r$B_failed
  )
}

#' The rows the CFA tab shows for a Bollen-Stine bootstrap
#'
#' The statistics, the Monte Carlo interval of the bootstrap p, the counts,
#' and the transformation, as text: numbers are rounded here for display
#' only, and the result keeps full precision.
#'
#' @param r A result from [sieve_bollen_stine()] (or the panel's copy of it).
#' @return A data frame with the character columns `Statistic` and `Value`.
#' @author Ken Kelley
#' @noRd
sieve_bollen_stine_rows <- function(r) {
  num <- function(x) if (length(x) == 1L && is.finite(x)) sprintf("%.4f", x) else "not reported"
  whole <- function(x) if (length(x) == 1L && is.finite(x)) format(x, big.mark = ",", scientific = FALSE, trim = TRUE) else "not reported"
  transformation <- switch(r$transformation %||% "",
    bollen_stine = "Bollen-Stine (complete data)",
    common = "Savalei-Yuan, one matrix for every pattern",
    pattern = "Savalei-Yuan, each pattern on its own",
    r$transformation %||% "not reported"
  )
  df <- data.frame(
    Statistic = c(
      "Model chi square (ML)", "df", "p (normal theory ML)",
      "p (Bollen-Stine bootstrap)",
      sprintf("Monte Carlo %s%% interval for the bootstrap p", format(100 * (r$conf_level %||% 0.95))),
      "Mean bootstrap chi square", "Resamples attempted", "Resamples used",
      if (length(r$aux)) {
        "Resamples flagged by lavaan's check, auxiliary variables included (kept)"
      } else {
        sprintf("Improper resamples (%s)", if (identical(r$improper, "drop")) "set aside" else "kept")
      },
      "Failed resamples", "Respondents", "Transformation"
    ),
    Value = c(
      num(r$chisq), whole(r$df), num(r$p_ml), num(r$p_bs),
      if (isTRUE(is.finite(r$p_bs_lower))) sprintf("[%s, %s]", num(r$p_bs_lower), num(r$p_bs_upper)) else "not reported",
      num(r$bootstrap_mean), whole(r$B), whole(r$B_used), whole(r$B_improper),
      whole(r$B_failed), whole(r$n), transformation
    ),
    stringsAsFactors = FALSE
  )
  df
}

#' The R that reproduces a Bollen-Stine bootstrap
#'
#' The refit, written from its specification (see `sieve_cfa_code()`), and
#' the one DMAR call with the seed and settings that ran.
#'
#' @param r A result from [sieve_bollen_stine()] that ran.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_bollen_stine_code <- function(r) {
  refit <- sieve_cfa_code(r$spec, fit_name = "bs_fit", model_name = "bs_model", data_name = "items")
  refit <- refit[!startsWith(refit, "lavaan::summary(")]
  c(
    "# The model on screen refit by ML for the Bollen-Stine bootstrap: FIML when a value is missing,",
    "# the complete cases otherwise; the bootstrap resamples its standard statistic.",
    refit,
    sprintf("RNGkind(%s)", paste(encodeString(r$rng$kind, quote = '"'), collapse = ", ")),
    sprintf(
      "bollen_stine <- DMAR::sem_exact_fit_bootstrap(bs_fit, B = %d, transformation = %s, improper = %s, conf_level = %s, seed = %d, ncpus = 1)",
      r$B, sieve_r_literal(r$transformation_requested %||% "common"),
      sieve_r_literal(r$improper), sieve_r_literal(r$conf_level), r$rng$seed
    ),
    "bollen_stine"
  )
}

#' The largest number of common factors an exploratory model can identify
#'
#' An exploratory factor model with `k` factors on `p` variables has
#' `((p - k)^2 - (p + k)) / 2` degrees of freedom. The model is estimable
#' only while that count is not negative (the Ledermann bound), which is a
#' stricter and more meaningful limit than `p - 1`.
#'
#' @param p The number of observed variables.
#' @return The largest `k` with nonnegative degrees of freedom (at least 0).
#' @author Ken Kelley
#' @noRd
sieve_efa_max_factors <- function(p) {
  k <- seq_len(max(0L, as.integer(p) - 1L))
  ok <- ((p - k)^2 - (p + k)) / 2 >= 0
  if (any(ok)) max(k[ok]) else 0L
}

#' Fit Exploratory Models at the Numbers of Factors Asked for, With Each One's Status
#'
#' Each solution is a separate lavaan::efa fit under the given estimator.
#' A row carries the status of its solution (see `sieve_fit_status()`): a
#' solution that failed, did not converge, or is not identified has no fit
#' indices, and the reason is kept in `message`. `converged` is what lavaan
#' reports for the solution, not merely whether an object came back.
#'
#' Which solutions are fit is the caller's: `ks`, the numbers of factors
#' asked for (the EFA tab's chosen k alone by default, or a range), or,
#' when `ks` is `NULL`, every k from 1 to `max_k`. A k above
#' `sieve_efa_max_factors()` (an exploratory model with negative degrees of
#' freedom, which has no unique solution) is not fit: it is kept as a row
#' with the status `"not_identified"` when it was asked for in `ks`, and
#' left out of the sequence 1 to `max_k`. A `ks` or `max_k` that is not a
#' whole number of at least 1 (a fraction, zero, a negative number, `NA`,
#' or text) is refused with an error that names it; no other number of
#' factors is substituted.
#'
#' An item lavaan cannot fit (no observed values, a single observed value,
#' or one value throughout, an ordered item with one category among them)
#' is refused before any model is fit, with an error that names it (see
#' `sieve_numeric_columns()`): lavaan would stop on it at every k after
#' printing its whole variable table, one line per item, instead of naming
#' it.
#'
#' The rows are read against one another (how the fit changes as factors
#' are added, and where adding a factor stops helping), so CFI, TLI, and
#' RMSEA are in ONE family for every row: the family
#' selected from the solved models with known positive degrees of freedom.
#' Other solved rows retain that family's available descriptive values. If no positive-df model exists,
#' an available family is descriptive only. There is no fallback row by row,
#' which would put a robust value beside a scaled one.
#' The model test (`chisq`, `df`, `pvalue`) is the one that goes with that
#' family: the scaled statistic when the fit has one (MLR, WLSMV), the
#' standard one otherwise.
#'
#' When several numbers of factors are fit, what does not depend on k (the
#' data, the sample statistics, and the saturated and baseline models) is
#' computed once and shared by the solutions; every number and every
#' warning is the one a separate fit of each k gives.
#'
#' @param data A data frame of the items (numeric).
#' @param estimator,ordered The estimator and the ordered variables (see
#'   `sieve_lavaan_spec()`).
#' @param rotation The rotation of the exploratory solutions, `"none"` by
#'   default. The model test, the standard and scaled indices, SRMR, AIC
#'   and BIC do not depend on it, but lavaan's robust CFI, TLI and RMSEA
#'   (with the RMSEA interval) of a rotated `efa` object fit by FIML
#'   (lavaan's `missing = "ml"`) do (lavaan 0.7-3; on the nine Holzinger and
#'   Swineford tests under MLR with FIML, x1 missing for the first fifteen
#'   cases, three factors, the robust RMSEA is 0.048 unrotated and 0.068
#'   under oblimin; with complete data, listwise deletion or WLSMV they do
#'   not move), so the fit indices are read
#'   from the unrotated solution, as DMAR's `fa_exploratory()` reads them;
#'   `"none"` also draws no random rotation starts. Pass a rotation only to
#'   reproduce lavaan's numbers for a rotated object.
#' @param max_k The largest number of factors to try when `ks` is `NULL`;
#'   capped at `sieve_efa_max_factors()`.
#' @param ks The numbers of factors to fit, or `NULL` for 1 to `max_k`.
#' @param missing `NULL` for lavaan's default, `"fiml"` (full information
#'   maximum likelihood, lavaan's `missing = "ml"`) or `"listwise"`, as
#'   `sieve_lavaan_spec()` gives it. Under `"fiml"` with no incomplete row
#'   the complete cases are every case and the fit is made on them (see
#'   `sieve_efa_missing_arg()`).
#' @param conf_level The confidence level of the RMSEA intervals
#'   (`RMSEA.lo`, `RMSEA.hi`): 0.90 by default
#'   (`sieve_default_rmsea_conf_level()`, DMAR's default for the RMSEA
#'   interval and lavaan's, whose limits decide the tests of close and not
#'   close fit at .05; lavaan's `fitMeasures()` is called with `fm.args =
#'   list(rmsea.ci.level = conf_level)`). Nothing else in the table depends
#'   on it.
#' @return A data frame with one row per k: `k`, `converged`, `status`,
#'   `message`, `CFI`, `TLI`, `RMSEA`, `RMSEA.lo`, `RMSEA.hi`, `SRMR`,
#'   `BIC`, `chisq`, `df`, `pvalue`, and `nobs` (the cases lavaan used), at
#'   full precision. The attributes are `warnings` (a list, the warnings
#'   raised for each k), `family` (the family of CFI, TLI, and RMSEA in
#'   every row, or `NA`), `family_reason` (the sentence that states the
#'   choice), `test_family` (the family of `chisq` and `pvalue`),
#'   `lavaan_missing` (the `missing` argument lavaan was given, or
#'   `"default"`), `measures` (the `sieve_fit_families()` object of
#'   each solution, with every family by name), and `conf_level` (the level
#'   of the RMSEA intervals).
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' fits <- sieve_efa_fit_by_k(items, estimator = "MLR", ks = 1:3, missing = "fiml")
#' fits[, c("k", "status", "CFI", "RMSEA", "RMSEA.lo", "RMSEA.hi", "SRMR")]
#' attr(fits, "family")
#' attr(fits, "conf_level")
#' @export
sieve_efa_fit_by_k <- function(data, estimator, ordered = NULL, rotation = "none",
                               max_k = 8L, ks = NULL, missing = NULL,
                               conf_level = sieve_default_rmsea_conf_level()) {
  conf_level <- sieve_check_conf_level(conf_level)
  top <- sieve_efa_max_factors(ncol(data))
  # The numbers of factors are taken as given: a value that is not a whole
  # number of at least 1 is refused by name, never rounded or dropped.
  whole <- function(x) is.numeric(x) & !is.na(x) & is.finite(x) & x >= 1 & x == floor(x)
  shown <- function(x) paste(format(x, trim = TRUE, digits = 15), collapse = ", ")
  if (is.null(ks)) {
    if (!is.numeric(max_k) || length(max_k) != 1L || !whole(max_k)) {
      stop(sprintf(paste0(
        "`max_k` must be a single whole number of factors of at least 1; it was %s, and no other ",
        "number of factors was substituted."
      ), if (is.null(max_k)) "NULL" else paste(deparse(max_k), collapse = "")), call. = FALSE)
    }
    ks <- seq_len(min(as.integer(max_k), top))
  } else {
    if (!is.numeric(ks) || !length(ks)) {
      stop(sprintf(paste0(
        "`ks` must hold whole numbers of factors of at least 1; it was %s, and no number of factors ",
        "was substituted."
      ), if (!length(ks)) "empty" else paste(deparse(ks), collapse = "")), call. = FALSE)
    }
    bad <- ks[!whole(ks)]
    if (length(bad)) {
      stop(sprintf(paste0(
        "`ks` must hold whole numbers of factors of at least 1, and %s %s not: a fraction, zero, a ",
        "negative number, NA or an infinite value is not a number of factors. No other number of ",
        "factors was substituted for %s."
      ), shown(bad), if (length(bad) == 1L) "is" else "are", if (length(bad) == 1L) "it" else "them"),
      call. = FALSE)
    }
    ks <- sort(unique(as.integer(ks)))
  }
  # An item lavaan cannot fit is named before anything is fit: lavaan
  # stops on it at every k after printing its whole variable table. An
  # ordered factor (which lavaan reads by its levels) is screened on the
  # codes of its observed values.
  screened <- as.data.frame(data)
  screened[] <- lapply(screened, function(v) {
    if (is.factor(v) || is.character(v)) match(as.character(v), unique(as.character(v[!is.na(v)]))) else v
  })
  unusable <- sieve_numeric_columns(screened, names(screened))$text
  if (length(unusable)) {
    unusable <- gsub("Exclude the item on the Items tab", "Leave the item out of `data`", unusable, fixed = TRUE)
    unusable <- gsub("exclude the item on the Items tab", "leave the item out of `data`", unusable, fixed = TRUE)
    stop(errorCondition(
      paste(c("No exploratory model was fit.", unusable), collapse = " "),
      class = "sieve_unusable_items", call = NULL
    ))
  }
  solutions <- sieve_efa_solutions(data, estimator, ordered,
    rotation = rotation, ks = ks,
    missing = missing, conf_level = conf_level
  )
  sieve_efa_table(solutions)
}

#' Fit the exploratory solutions, sharing what does not depend on k
#'
#' The work of [sieve_efa_fit_by_k()] before its rows are combined: one
#' lavaan::efa solution per number of factors in `ks`, in the order given,
#' each with its row, its warnings and its fit measures (see
#' `sieve_fit_families()`).
#'
#' Much of a lavaan fit does not depend on the number of factors: reading
#' the data, the sample statistics (under WLSMV the polychoric correlations,
#' thresholds and their asymptotic covariance matrix), the saturated (h1)
#' model, and the baseline (independence) model with its own scaled test
#' statistic. Measured on a study of 68 items, the baseline
#' model is about half of an MLR solution's time and the sample statistics
#' about half of a WLSMV solution's. When two or more solutions are fit
#' here, the data, sample statistics and h1 model are computed once, by a
#' lavaan::efa call with `do.fit = FALSE`, and handed to every solution
#' through lavaan's `slot_data`, `slot_sample_stats` and `sloth1`
#' arguments (the route lavaan itself takes when it refits a baseline
#' model); the baseline model of the first solution that raised no warning
#' of its own is given to the later ones. Every number is the one a separate
#' fit gives (identical, not only equal within a tolerance), and so is every
#' warning: the warnings of the shared computation are reported with each
#' solution, as a separate fit reports them. A solution that cannot be fit
#' this way is fit again on its own, so a failure is always the engine's
#' own message for that solution. The option `sieve.efa_share = FALSE`
#' turns the sharing off.
#'
#' @inheritParams sieve_efa_fit_by_k
#' @param ks The numbers of factors, in the order to fit them.
#' @param on_solution `NULL`, or a function of one solution and the
#'   `missing` argument lavaan was given, called as each solution is
#'   finished (a background worker writes it for the progress display).
#' @return A list, one element per k, each a list with `k`, `row`,
#'   `warnings` and `measures`; the attributes `lavaan_missing` and `p`.
#' @author Ken Kelley
#' @noRd
sieve_efa_solutions <- function(data, estimator, ordered = NULL, rotation = "none",
                                ks = integer(0), missing = NULL, on_solution = NULL,
                                conf_level = sieve_default_rmsea_conf_level()) {
  top <- sieve_efa_max_factors(ncol(data))
  ks <- as.integer(ks)
  lavaan_missing <- sieve_efa_missing_arg(missing, data)
  # A categorical solution whose weight matrix would not fit in memory is
  # refused before the engine starts, for every k, with the size and the
  # two ways forward (see sieve_wlsmv_guard()).
  refusal <- sieve_wlsmv_guard(names(data), data,
    ordered = ordered %||% character(0),
    estimator = estimator
  ) %||% sieve_mlr_guard(ncol(data), estimator, n = nrow(data),
    npar = sieve_free_parameters(ncol(data), min(c(ks, 1L)), exploratory = TRUE,
      means = identical(lavaan_missing, "ml"))
  )
  call_args <- function(k) {
    args <- list(
      data = data, nfactors = k, rotation = rotation, estimator = estimator,
      ordered = ordered
    )
    if (!is.null(lavaan_missing)) {
      args$missing <- lavaan_missing
    }
    args
  }
  fit_of <- function(obj) if (inherits(obj, "efaList")) obj[[1]] else obj
  # The parts of the fit every k shares, computed once (see the description).
  shared <- new.env(parent = emptyenv())
  shared$slots <- NULL
  shared$warnings <- character(0)
  shared$baseline <- NULL
  fittable <- ks[ks <= top]
  if (is.null(refusal) && length(unique(fittable)) >= 2L &&
    !identical(getOption("sieve.efa_share"), FALSE)) {
    stage <- sieve_capture(do.call(lavaan::efa, c(call_args(fittable[1L]), list(do.fit = FALSE))))
    held <- fit_of(stage$value)
    if (isTRUE(stage$ok) && inherits(held, "lavaan")) {
      shared$slots <- list(slot_data = held@Data, slot_sample_stats = held@SampleStats, sloth1 = held@h1)
      shared$warnings <- stage$warnings
    }
  }
  engine <- function(k) {
    if (is.null(shared$slots)) {
      return(sieve_capture(do.call(lavaan::efa, call_args(k))))
    }
    extra <- shared$slots
    if (!is.null(shared$baseline)) extra$baseline <- FALSE
    run <- sieve_capture(do.call(lavaan::efa, c(call_args(k), extra)))
    fit <- fit_of(run$value)
    if (!isTRUE(run$ok) || !inherits(fit, "lavaan")) {
      # The shared route never decides an outcome: the solution is fit on
      # its own, and its failure, if any, is the engine's own.
      return(sieve_capture(do.call(lavaan::efa, call_args(k))))
    }
    if (!is.null(shared$baseline)) {
      fit@baseline <- shared$baseline
    } else if (!length(run$warnings) && length(fit@baseline)) {
      # A baseline model fit without a warning of its own is the one every
      # later solution would fit again from the same sample statistics.
      shared$baseline <- fit@baseline
    }
    run$value <- fit
    run$warnings <- unique(c(shared$warnings, run$warnings))
    run
  }
  one <- function(k) {
    run <- if (k > top) {
      list(
        value = NULL, ok = FALSE, warnings = character(0), messages = character(0),
        error = NULL
      )
    } else if (!is.null(refusal)) {
      list(value = NULL, error = refusal, warnings = character(0), messages = character(0), ok = FALSE)
    } else {
      engine(k)
    }
    obj <- run$value
    fit <- if (inherits(obj, "efaList")) obj[[1]] else obj
    status <- if (k > top) {
      list(
        code = "not_identified", converged = FALSE,
        message = sprintf(paste0(
          "Not fit: an exploratory model with %d factors on %d items has negative degrees of ",
          "freedom, so its solution is not unique; at most %d factor%s can be fit as an ",
          "exploratory model."
        ), k, ncol(data), top, if (top == 1L) "" else "s")
      )
    } else {
      sieve_fit_status(fit, warnings = run$warnings, error = run$error)
    }
    row <- data.frame(
      k = k, converged = isTRUE(status$converged), status = status$code,
      message = status$message, CFI = NA_real_, TLI = NA_real_, RMSEA = NA_real_,
      RMSEA.lo = NA_real_, RMSEA.hi = NA_real_, SRMR = NA_real_, BIC = NA_real_,
      chisq = NA_real_, df = NA_real_, pvalue = NA_real_,
      nobs = if (is.null(fit)) NA_real_ else {
        tryCatch(as.numeric(sum(lavaan::lavInspect(fit, "nobs"))), error = function(e) NA_real_)
      },
      stringsAsFactors = FALSE
    )
    solution <- list(
      k = k, row = row, warnings = run$warnings,
      measures = if (k > top) {
        sieve_fit_families(NULL, conf_level = conf_level)
      } else {
        sieve_fit_families(fit, status = status, conf_level = conf_level)
      }
    )
    if (is.function(on_solution)) on_solution(solution, lavaan_missing)
    solution
  }
  out <- lapply(ks, one)
  attr(out, "lavaan_missing") <- lavaan_missing
  out
}

#' Combine exploratory solutions into the fit-by-k table
#'
#' The table [sieve_efa_fit_by_k()] returns, built from the solutions of
#' `sieve_efa_solutions()`: CFI, TLI and RMSEA in the one family
#' `sieve_efa_common_family()` selects from the positive-df comparison pool,
#' and the model test that goes with it. Other solved rows are descriptive.
#' One solution gives the one-row table of that k
#' alone, which is what the EFA tab's background workers return and cache.
#'
#' @param solutions A list of solutions (each with `k`, `row`, `warnings`
#'   and `measures`), with the attribute `lavaan_missing`.
#' @param lavaan_missing The `missing` argument lavaan was given; read from
#'   `solutions` by default.
#' @return See [sieve_efa_fit_by_k()].
#' @author Ken Kelley
#' @noRd
sieve_efa_table <- function(solutions, lavaan_missing = attr(solutions, "lavaan_missing")) {
  fits <- solutions
  ks <- vapply(fits, function(x) as.integer(x$k), integer(1))
  out <- do.call(rbind, lapply(fits, `[[`, "row"))
  if (is.null(out)) {
    out <- data.frame(
      k = integer(0), converged = logical(0), status = character(0),
      message = character(0), CFI = numeric(0), TLI = numeric(0), RMSEA = numeric(0),
      RMSEA.lo = numeric(0), RMSEA.hi = numeric(0), SRMR = numeric(0), BIC = numeric(0),
      chisq = numeric(0), df = numeric(0), pvalue = numeric(0), nobs = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  measures <- stats::setNames(lapply(fits, `[[`, "measures"), sprintf("k = %d", ks))
  common <- sieve_efa_common_family(measures)
  test_family <- NA_character_
  for (i in which(common$solved)) {
    out$SRMR[i] <- measures[[i]]$srmr$value
    out$BIC[i] <- measures[[i]]$bic$value
    # One family is shared by the positive-df candidates. Other solved rows
    # keep only that family's available descriptive values.
    if (!is.na(common$family)) {
      values <- measures[[i]]$families[[common$family]]$values
      out$CFI[i] <- values[["cfi"]]
      out$TLI[i] <- values[["tli"]]
      out$RMSEA[i] <- values[["rmsea"]]
      out$RMSEA.lo[i] <- values[["rmsea_lo"]]
      out$RMSEA.hi[i] <- values[["rmsea_hi"]]
    }
    # The model test of the family: the robust family has none of its own,
    # and goes with the scaled statistic.
    tf <- if (isTRUE(measures[[i]]$families$scaled$available)) "scaled" else "standard"
    test_values <- measures[[i]]$families[[tf]]$values
    out$chisq[i] <- test_values[["chisq"]]
    out$df[i] <- if (is.finite(test_values[["df"]])) test_values[["df"]] else measures[[i]]$df
    out$pvalue[i] <- test_values[["pvalue"]]
    test_family <- tf
  }
  attr(out, "warnings") <- lapply(fits, `[[`, "warnings")
  attr(out, "family") <- common$family
  attr(out, "family_reason") <- common$reason
  attr(out, "test_family") <- test_family
  attr(out, "lavaan_missing") <- lavaan_missing %||% "default"
  attr(out, "measures") <- measures
  levels <- unique(vapply(measures, sieve_fit_ci_level, numeric(1)))
  attr(out, "conf_level") <- if (length(levels) == 1L) levels else NA_real_
  out
}

#' The solution behind a one-row fit-by-k table
#'
#' Inverts `sieve_efa_table()` for one solution, so that kept one-row
#' tables (the EFA tab's cache, shared with the Dimensionality tab) can be
#' combined into the table of a set of k exactly as if the set had been fit
#' together: the common family of CFI, TLI and RMSEA is chosen again for the
#' set, from the fit measures each row keeps by family.
#'
#' @param table A one-row table from `sieve_efa_table()`.
#' @return A solution (see `sieve_efa_solutions()`).
#' @author Ken Kelley
#' @noRd
sieve_efa_solution_of <- function(table) {
  row <- table[1L, , drop = FALSE]
  row <- data.frame(lapply(row, function(v) v), stringsAsFactors = FALSE)
  filled <- c("CFI", "TLI", "RMSEA", "RMSEA.lo", "RMSEA.hi", "SRMR", "BIC", "chisq", "df", "pvalue")
  row[, filled] <- NA_real_
  list(
    k = as.integer(row$k), row = row,
    warnings = attr(table, "warnings")[[1L]],
    measures = attr(table, "measures")[[1L]]
  )
}

#' Headline fit indices from a lavaan fit, with the RMSEA confidence interval
#'
#' Returns the indices SIEVER foregrounds: the model test, RMSEA with
#' its confidence interval at `conf_level` (90% by default,
#' `sieve_default_rmsea_conf_level()`, the level of every RMSEA interval
#' SIEVER shows), CFI, TLI, and SRMR, with AIC and BIC. lavaan
#' reports the test and the indices built on it in up to three families
#' (standard, scaled, robust), which are different statistics. All three are
#' read and kept by name (see `sieve_fit_families()`); this function returns
#' ONE of them as a flat vector, selected by the rule of
#' `sieve_primary_family()` (robust when it is reported for the model and
#' describes the whole of it, then scaled, then standard) or named by the
#' caller, and says which through its attributes. A display must label the
#' family it shows: use `sieve_fit_label()` and `sieve_fit_note()`.
#'
#' A fit index describes a solution, so none is reported for a model that
#' has no solution to describe. When the model is not identified (see
#' `sieve_fit_status()`) the number of free parameters and the degrees of
#' freedom are returned and every index is `NA`: lavaan would otherwise
#' return, for example, an RMSEA of zero for a model with negative degrees
#' of freedom. The status code is carried in the `status` attribute. A
#' converged but inadmissible solution keeps its indices, because they are
#' the fit of the solution lavaan reached and the caller's status display
#' says that the solution is improper.
#'
#' @param fit A fitted lavaan object.
#' @param family `NULL` for the rule, or `"robust"`, `"scaled"`, or
#'   `"standard"` to return that family (for a comparison of models, where
#'   one family is used for all of them; see [sieve_fit_compare()]).
#' @param conf_level The confidence level of the RMSEA interval (see
#'   `sieve_fit_families()`).
#' @param residual `TRUE` to add the unbiased SRMR with its interval at
#'   `conf_level` (see `sieve_fit_families()`).
#' @return A named numeric vector at full precision (`npar`, `chisq`, `df`,
#'   `pvalue`, `cfi`, `tli`, `rmsea`, `rmsea_lo`, `rmsea_hi`, `srmr`, `aic`,
#'   `bic`) with the attributes `status`, `family` (the family of CFI, TLI,
#'   and RMSEA), `test_family` (the family of the chi square and its
#'   p-value), `passed_over`, and `families` (every family, by name, with
#'   the level of the RMSEA interval as its `ci_level`); or
#'   `NULL` when there is no fit or lavaan has no fit measures for it (a
#'   model that did not converge).
#' @author Ken Kelley
#' @noRd
sieve_fit_measures <- function(fit, family = NULL, conf_level = sieve_default_rmsea_conf_level(),
                               residual = FALSE) {
  if (is.null(fit)) {
    return(NULL)
  }
  families <- sieve_fit_families(fit, conf_level = conf_level, residual = residual)
  if (families$status %in% c("failed", "not_converged") || !is.null(families$engine_error)) {
    return(NULL)
  }
  sieve_fit_select(families, family)
}

#' Fit measures at the RMSEA level, reusing measures already read
#'
#' A fit is shared between pages (the session's fit store) with the
#' measures the page that fit it read. Those are reused when their RMSEA
#' interval is at the level asked for and read again at that level when it
#' is not, so a page never shows an interval at another level than the one
#' it states.
#'
#' @param measures Measures already read (a `sieve_fit_measures()`
#'   result), or `NULL`.
#' @param fit The fitted lavaan model the measures describe.
#' @param conf_level The level of the RMSEA interval
#'   (`sieve_default_rmsea_conf_level()` by default).
#' @param residual `TRUE` to hold the unbiased SRMR with its interval at
#'   the same level (`sieve_fit_usrmr()`), added to reused measures that do
#'   not hold it; `FALSE` (a page that does not show it) returns measures
#'   without it, also when measures reused from the page that computed it
#'   hold it (`sieve_fit_drop_usrmr()`), so such a page reads the same
#'   measures whichever page fit the model first.
#' @return A `sieve_fit_measures()` result at `conf_level`.
#' @author Ken Kelley
#' @noRd
sieve_fit_measures_at <- function(measures, fit, conf_level = sieve_default_rmsea_conf_level(),
                                  residual = FALSE) {
  held <- if (is.null(measures)) NA_real_ else sieve_fit_ci_level(measures)
  if (!is.null(measures) && isTRUE(all.equal(held, conf_level))) {
    return(if (isTRUE(residual)) sieve_fit_with_usrmr(measures, fit) else sieve_fit_drop_usrmr(measures))
  }
  sieve_fit_measures(fit, conf_level = conf_level, residual = residual)
}

#' Factor scores from a fitted CFA, aligned to the input rows
#'
#' Returns one column per latent factor. The model is fit with FIML, so
#' incomplete cases still contribute, and lavaan computes each case's score
#' from its available data; under missing data such full-information scores
#' are more reliable than sum scores (Estabrook & Neale, 2013). Two methods
#' are offered: Bartlett (the weighted least squares estimate of the factor
#' given the items; unbiased for each case's factor value, with a variance
#' larger than the factor's) and regression (the posterior mean, the EAP
#' estimator for the normal model; shrunken toward the mean, with the
#' smaller mean squared error). Neither is the better choice in general for
#' a later regression: as a predictor a Bartlett score attenuates the slope
#' and a regression score does not, and as an outcome the reverse holds
#' (Skrondal & Laake, 2001; Devlieger, Mayer, & Rosseel, 2016).
#'
#' @param fit A fitted lavaan object.
#' @param method `"Bartlett"` (default) or `"regression"`.
#' @param labels `TRUE` names each column with the researcher's label for
#'   the construct, for a table or a file a person reads. The model names
#'   (the default) can be positional (`F1`, `F2`) when a label is not usable
#'   as a name, and a column so named cannot be told from another without
#'   the name map.
#' @return A data frame of factor scores, one column per latent variable
#'   under its model name (see `sieve_show_names()` for the labels) or,
#'   with `labels = TRUE`, under its label (made unique if two labels are
#'   the same); or `NULL` when there is no fit or the model did not converge
#'   or is not identified.
#' @author Ken Kelley
#' @noRd
sieve_factor_scores <- function(fit, method = c("Bartlett", "regression", "ML", "EBM"), labels = FALSE) {
  if (is.null(fit)) {
    return(NULL)
  }
  method <- match.arg(method)
  method <- sieve_score_method(fit, method)$method
  # Scores are functions of the estimates, so a model with no solution (not
  # converged, or not identified) has no scores.
  if (!sieve_has_solution(fit)) {
    return(NULL)
  }
  run <- sieve_score_rows(fit, method, uncertainty = FALSE)
  if (!is.null(run$error)) return(NULL)
  fs <- as.data.frame(run$scores)
  if (isTRUE(labels)) {
    names(fs) <- make.unique(sieve_show_names(names(fs), fit), sep = " ")
  }
  fs
}

# ---- Structures to compare ------------------------------------------------
# A researcher may want to pick a range of factors so as to compare
# confirmatory models. A confirmatory model with k factors needs an
# assignment of items to factors, so the confirmatory counterpart of a range
# of factors is a set of structures over the same first-order groups: one
# factor, the correlated groups, the groups under a second-order factor,
# and the bifactor model. The functions below build the four from the
# ledger, say which of them the ledger supports, fit them through the
# status contract, read their fit in one family, and test the nested pairs.

#' The first-order groups a comparison of structures is built on
#'
#' The groups are the facets of the construct when exactly one construct is
#' included and it is split into two or more facets, and the constructs
#' otherwise. The Bifactor tab builds its group factors on the same groups
#' (`sieve_bifactor_plan()`), and so does the Items tab's exploratory
#' model (`sieve_item_diagnostics()`). Items of the one construct that
#' carry no facet cannot be placed in a grouped model and are named in
#' `ungrouped`.
#'
#' @param meta The item ledger.
#' @return A list with `level` (`"facet"` or `"construct"`), `scale` (the
#'   label of the one factor and of the second-order factor: the construct
#'   when there is one, `"General"` otherwise), `items` (the included items
#'   with a construct), `group` (the group of each, `NA` for an ungrouped
#'   item), `groups` (the distinct groups, in ledger order), `sizes` (the
#'   included items per group), and `ungrouped`.
#' @author Ken Kelley
#' @noRd
sieve_structure_groups <- function(meta) {
  active <- meta[meta$status == "active" & sieve_construct_assigned(meta$construct), , drop = FALSE]
  cons <- unique(as.character(active$construct))
  facet <- if ("facet" %in% names(active)) as.character(active$facet) else rep(NA_character_, nrow(active))
  has_facet <- !is.na(facet) & nzchar(facet)
  if (length(cons) == 1L && length(unique(facet[has_facet])) >= 2L) {
    group <- ifelse(has_facet, facet, NA_character_)
    level <- "facet"
    scale <- cons
  } else {
    group <- as.character(active$construct)
    level <- "construct"
    scale <- if (length(cons) == 1L) cons else "General"
  }
  groups <- unique(group[!is.na(group)])
  list(
    level = level, scale = scale, items = as.character(active$item), group = group,
    groups = groups,
    sizes = stats::setNames(vapply(groups, function(g) sum(group %in% g), integer(1)), groups),
    ungrouped = as.character(active$item)[is.na(group)]
  )
}

#' The four structures a ledger supports, as model plans
#'
#' Each structure is built by SIEVER's model builders from a ledger derived
#' for it: the one-factor model assigns every included item with a construct
#' to the scale (`sieve_cfa_plan()`, first order); the correlated model
#' assigns each grouped item
#' to its group (`sieve_cfa_plan()`, first order); the higher-order model
#' makes the groups the facets of the scale (`sieve_cfa_plan()`, higher
#' order); the bifactor model makes the groups the constructs of
#' `sieve_bifactor_plan()`, whose general factor loads every grouped item
#' and whose group factors need three items each. The correlated model needs
#' two or more groups and the higher-order model three or more: a
#' second-order factor over two first-order factors is not identified
#' without a constraint. The bifactor model takes its rule from
#' `sieve_bifactor_plan()`, the rule of the Bifactor tab: at least three
#' group factors of three or more items each (a bifactor model with two
#' group factors is at the same limit), so the two tabs fit and refuse the
#' same ledgers for the same reason.
#'
#' @param meta The item ledger.
#' @param reserved Further observed variable names the latent names must
#'   avoid (the columns of the data).
#' @return A list with `groups` (see `sieve_structure_groups()`) and
#'   `structures`, a list with one element per structure (`one_factor`,
#'   `correlated`, `higher_order`, `bifactor`), each a list with `id`,
#'   `label`, `available`, `reason` (why not, or `NA`), `plan` (`NULL` when
#'   not available), `orthogonal`, and `description`.
#' @author Ken Kelley
#' @noRd
sieve_structure_plans <- function(meta, reserved = NULL) {
  g <- sieve_structure_groups(meta)
  n_groups <- length(g$groups)
  grouped <- g$items[!is.na(g$group)]
  noun <- if (identical(g$level, "facet")) "facet" else "construct"
  plural <- function(n, word) sprintf("%d %s%s", n, word, if (n == 1L) "" else "s")
  # A ledger derived for a structure: the grouped items only, with the
  # construct and the facet columns set for it.
  derive <- function(construct, facet = NA_character_, items = grouped) {
    m <- meta
    m$status <- ifelse(m$item %in% items, "active", "parked")
    at <- match(m$item, g$items)
    m$construct <- ifelse(is.na(at), NA_character_, construct[pmax(at, 1L)])
    if (!"facet" %in% names(m)) {
      m$facet <- NA_character_
    }
    m$facet <- ifelse(is.na(at), NA_character_, facet[pmax(at, 1L)])
    m
  }
  n_items <- length(g$items)
  one <- rep(g$scale, n_items)
  structure_of <- function(id, label, description, orthogonal, needs, build) {
    out <- list(
      id = id, label = label, description = description, available = FALSE,
      reason = NA_character_, plan = NULL, orthogonal = orthogonal
    )
    if (!length(grouped) || length(grouped) < 3L) {
      out$reason <- sprintf(
        "the ledger includes %s with a %s; a structure needs at least three",
        plural(length(grouped), "item"), noun
      )
      return(out)
    }
    if (n_groups < needs) {
      out$reason <- sprintf(
        "the ledger has %s and this structure needs at least %d",
        plural(n_groups, noun), needs
      )
      return(out)
    }
    plan <- build()
    if (!nzchar(plan$model)) {
      # A plan that says why it wrote no model (the bifactor plan) gives its
      # own reason, as a clause.
      why <- plan$reason %||% NA_character_
      out$reason <- if (!is.na(why)) {
        why <- sub("[.]$", "", why)
        paste0(tolower(substr(why, 1L, 1L)), substring(why, 2L))
      } else {
        paste(c("no factor could be written for it", sieve_not_modeled_text(plan)),
          collapse = ": "
        )
      }
      return(out)
    }
    out$plan <- plan
    out$available <- TRUE
    out
  }
  group_labels <- function(plan) {
    # The groups are shown under their own names, not as "scale: group".
    map <- plan$name_map
    if (length(map)) {
      for (grp in g$groups) {
        hit <- which(map == sprintf("%s: %s", g$scale, grp))
        map[hit] <- grp
      }
    }
    plan$name_map <- map
    plan
  }
  structures <- list(
    one_factor = structure_of(
      "one_factor", "One factor",
      "every item with a construct loads on one factor, so the groups are not distinguished",
      orthogonal = FALSE, needs = 1L,
      build = function() sieve_cfa_plan(derive(one, items = g$items), "first_order", reserved = reserved)
    ),
    correlated = structure_of(
      "correlated", sprintf("Correlated %ss", noun),
      sprintf("one factor per %s, the factors free to correlate", noun),
      orthogonal = FALSE, needs = 2L,
      build = function() sieve_cfa_plan(derive(g$group), "first_order", reserved = reserved)
    ),
    higher_order = structure_of(
      "higher_order", "Higher order",
      sprintf("one factor per %s, each loading on a second-order factor that accounts for their correlations", noun),
      orthogonal = FALSE, needs = 3L,
      build = function() group_labels(sieve_cfa_plan(derive(one, facet = g$group), "higher_order",
        reserved = reserved
      ))
    ),
    bifactor = structure_of(
      "bifactor", "Bifactor",
      sprintf(
        "a general factor of every item and one orthogonal group factor per %s (a group factor needs three items; a smaller %s loads on the general factor only)",
        noun,
        noun
      ),
      orthogonal = TRUE, needs = 1L,
      build = function() sieve_bifactor_plan(derive(g$group), min_group = 3L, reserved = reserved)
    )
  )
  list(groups = g, structures = structures)
}

#' The pairs of structures that are nested, and the one that is not
#'
#' In each nested pair the second model is the first with constraints
#' added: the one-factor model fixes the correlations of the correlated
#' model at one, the disturbances of the higher-order model at zero, and
#' the group loadings of the bifactor model at zero; the higher-order model
#' constrains the correlations of the correlated model to a one-factor
#' structure (Rindskopf & Rose, 1988) and is the bifactor model with
#' proportional loadings (Yung, Thissen & McLeod, 1999). The correlated
#' model and the bifactor model are not nested in either direction. The
#' three constraints that fix a correlation at one or a variance at zero
#' lie on the boundary of the parameter space, where the chi square
#' difference test is approximate (Stoel, Garre, Dolan & van den
#' Wittenboer, 2006); the pair carries that note. Fixing the group loadings
#' of the bifactor model at zero is one of them: it is the same as fixing
#' the group factors' variances at zero, and at it the information about
#' those loadings is zero.
#'
#' @return A data frame with `less` and `more` (structure ids, the second
#'   nested in the first), `nested` (`TRUE` for the five nested pairs,
#'   `FALSE` for the pair that is not), `constraint` (what the second model
#'   fixes), and `boundary` (whether the constraint is on the boundary).
#' @author Ken Kelley
#' @noRd
sieve_structure_pairs <- function() {
  data.frame(
    less = c("correlated", "correlated", "bifactor", "bifactor", "higher_order", "correlated"),
    more = c("one_factor", "higher_order", "higher_order", "one_factor", "one_factor", "bifactor"),
    nested = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    constraint = c(
      "the correlations among the group factors fixed at one",
      "the correlations among the group factors reduced to a second-order factor",
      "the loadings on the general and the group factors held proportional within each group (Yung, Thissen & McLeod, 1999)",
      "the loadings on the group factors fixed at zero, which is the same as fixing the group factors' variances at zero",
      "the disturbances of the group factors fixed at zero",
      "not nested: neither model is the other with constraints added, so no difference test is defined for the pair"
    ),
    boundary = c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

#' Fit the Structures a Ledger Supports and Compare Them in One Family
#'
#' Each available structure (see `sieve_structure_plans()`) is resolved by
#' `sieve_model_spec()` with the estimator the item types imply and fit by
#' [sieve_run_model()], so every fit carries a status and the WLSMV guard
#' applies. The fit indices are read in one family for every model by
#' [sieve_fit_compare()] (without its consecutive tests, which assume a
#' ladder), and the chi square difference test of each nested pair (see
#' `sieve_structure_pairs()`) is lavaan's own through `sieve_nested_test()`,
#' made only when both models have a solution.
#'
#' @details
#' Compares the correlated-factor, one-factor, higher-order, and bifactor
#' structures the ledger can identify. Unsupported structures retain a reason.
#' Parameters, fit indices, and tests retain full precision. A boundary
#' constraint is named; an ordinary reference distribution is not a universal
#' boundary correction. The analysis assumes independent sampling units and
#' does not apply a clustered or repeated design.
#' @param meta Ledger with active item, construct, and measurement-type fields.
#' @param items Numeric item responses after intended reverse scoring.
#' @param reserved Further observed variable names the latent names must
#'   avoid; the columns of `items` are always avoided.
#' @param estimator One of `"auto"`, which follows the item types (ML
#'   with FIML when every item is continuous, WLSMV when any is ordered or
#'   binary), `"mlr"` (MLR with FIML), `"ml_fiml"` (ML with FIML), `"ml"`
#'   (ML on the complete cases) or `"wlsmv"` (WLSMV, every item treated as
#'   ordered); the CFA tab passes its Estimator control's choice.
#' @param conf_level The confidence level of the RMSEA intervals, 0.90 by
#'   default (`sieve_default_rmsea_conf_level()`, DMAR's default for the
#'   RMSEA interval; see [sieve_fit_compare()]).
#' @return A list of class `sieve_structures` with `groups`, `structures`
#'   (the plans, each with `spec` and `run`, the [sieve_run_model()]
#'   result, added when available), `fitted` (the ids of the structures
#'   with a fit), `compare` (the [sieve_fit_compare()] result over the
#'   fitted structures, or `NULL` when none was fit), `tests` (a data frame
#'   with one row per pair: `less`, `more`, `less_label`, `more_label`,
#'   `nested`, `constraint`, `boundary`, `available`, `statistic`, `df`,
#'   `p`, `label`, `note`, `reason`), `conf_level` (the level of the RMSEA
#'   intervals), and `seconds`.
#' @author Ken Kelley
#' @examples
#' # Two constructs of three tests each, so two structures are fit: one
#' # factor and correlated factors. With the three constructs of all nine
#' # tests the higher-order and bifactor structures are fit as well.
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' structures <- sieve_compare_structures(meta, items)
#' structures$fitted
#' structures$tests[, c("less", "more", "statistic", "df", "p")]
#' @export
sieve_compare_structures <- function(meta, items, reserved = NULL, estimator = "auto",
                                     conf_level = sieve_default_rmsea_conf_level()) {
  conf_level <- sieve_check_conf_level(conf_level)
  started <- proc.time()[["elapsed"]]
  reserved <- union(names(items), reserved)
  built <- sieve_structure_plans(meta, reserved = reserved)
  structures <- lapply(built$structures, function(st) {
    if (!isTRUE(st$available)) {
      return(st)
    }
    st$spec <- sieve_model_spec(st$plan, meta,
      kind = paste0("structure_", st$id), fun = "cfa",
      std_lv = TRUE, orthogonal = isTRUE(st$orthogonal),
      estimator = estimator
    )
    st$run <- sieve_run_model(st$spec, items)
    st
  })
  fitted <- names(structures)[vapply(structures, function(st) isTRUE(st$available), logical(1))]
  labels <- vapply(structures, function(st) st$label, character(1))
  compare <- NULL
  if (length(fitted)) {
    fits <- lapply(structures[fitted], function(st) st$run$fit)
    status <- lapply(structures[fitted], function(st) st$run$status)
    names(fits) <- unname(labels[fitted])
    compare <- sieve_fit_compare(fits, status = status, nested = FALSE, conf_level = conf_level)
  }
  solved <- vapply(structures, function(st) {
    isTRUE(st$available) && sieve_has_solution(st$run)
  }, logical(1))
  pairs <- sieve_structure_pairs()
  tests <- do.call(rbind, lapply(seq_len(nrow(pairs)), function(i) {
    less <- pairs$less[i]
    more <- pairs$more[i]
    row <- data.frame(
      less = less, more = more, less_label = unname(labels[less]), more_label = unname(labels[more]),
      nested = pairs$nested[i], constraint = pairs$constraint[i], boundary = pairs$boundary[i],
      available = FALSE, statistic = NA_real_, df = NA_real_, p = NA_real_,
      label = NA_character_, note = NA_character_, reason = NA_character_,
      stringsAsFactors = FALSE
    )
    if (!pairs$nested[i]) {
      row$reason <- "The two models are not nested, so no difference test is defined."
      return(row)
    }
    if (!solved[[less]] || !solved[[more]]) {
      missing <- c(if (!solved[[less]]) labels[less], if (!solved[[more]]) labels[more])
      why <- vapply(c(less, more)[!c(solved[[less]], solved[[more]])], function(id) {
        st <- structures[[id]]
        if (!isTRUE(st$available)) st$reason else st$run$status$message %||% st$run$error %||% "no solution"
      }, character(1))
      row$reason <- sprintf(
        "No test: %s %s no solution (%s).", paste(missing, collapse = " and "),
        if (length(missing) == 1L) "has" else "have", paste(why, collapse = "; ")
      )
      return(row)
    }
    got <- sieve_nested_test(structures[[less]]$run$fit, structures[[more]]$run$fit)
    row$available <- isTRUE(got$available)
    row$statistic <- got$statistic
    row$df <- as.numeric(got$df)
    row$p <- got$p
    row$label <- got$label
    # A test that involves an improper solution is shown with that said:
    # the statistic is computed, but a solution outside the parameter space
    # does not have the reference distribution the test assumes.
    improper <- c(less, more)[vapply(c(less, more), function(id) {
      identical(structures[[id]]$run$status$code, "inadmissible")
    }, logical(1))]
    row$note <- paste(c(
      if (!is.na(got$note)) got$note,
      if (pairs$boundary[i]) {
        paste0(
          if (identical(less, "bifactor") && identical(more, "one_factor")) {
            "Fixing the group loadings at zero is the same as fixing the group factors' variances at zero: "
          },
          if (identical(less, "bifactor") && identical(more, "one_factor")) "the" else "The",
          " constraint lies on the boundary of the parameter space, where the chi square ",
          "difference test is approximate (Stoel, Garre, Dolan & van den Wittenboer, 2006)."
        )
      },
      if (length(improper)) {
        sprintf(
          paste0(
            "The %s %s improper (a negative variance or a standardized loading above one), ",
            "so the chi square reference distribution of this test is not assured: read it with that caution."
          ),
          paste(unname(labels[improper]), collapse = " and "),
          if (length(improper) == 1L) "solution is" else "solutions are"
        )
      }
    ), collapse = " ")
    if (!nzchar(row$note)) {
      row$note <- NA_character_
    }
    row$reason <- got$reason
    row
  }))
  structure(list(
    groups = built$groups, structures = structures, fitted = fitted, compare = compare,
    tests = tests, conf_level = conf_level, seconds = proc.time()[["elapsed"]] - started
  ), class = "sieve_structures")
}

#' The Fit of the Compared Structures as One Table
#'
#' One row per structure, in the family [sieve_fit_compare()] chose for all
#' of them, at full precision. A structure that was not fit, or whose model
#' has no solution, has its reason in `note` and no values.
#'
#' @param result A [sieve_compare_structures()] result.
#' @return A data frame with `id`, `structure`, `status`, `npar`, `chisq`,
#'   `df`, `pvalue`, `cfi`, `tli`, `rmsea`, `rmsea_lo`, `rmsea_hi`, `srmr`,
#'   and `note`, with the attributes `family`, `test_family`, and
#'   `conf_level` (the level of the RMSEA interval).
#' @author Ken Kelley
#' @examples
#' # Two constructs of three tests each: one factor and correlated factors.
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' structures <- sieve_compare_structures(meta, items)
#' sieve_structures_table(structures)[, c(
#'   "structure", "status", "chisq", "df", "cfi", "rmsea", "rmsea_lo", "rmsea_hi"
#' )]
#' @export
sieve_structures_table <- function(result) {
  cmp <- result$compare
  family <- if (is.null(cmp)) NA_character_ else cmp$family
  test_family <- if (is.null(cmp)) NA_character_ else cmp$test_family
  rows <- lapply(result$structures, function(st) {
    out <- data.frame(
      id = st$id, structure = st$label, status = "not fit", npar = NA_real_,
      chisq = NA_real_, df = NA_real_, pvalue = NA_real_, cfi = NA_real_, tli = NA_real_,
      rmsea = NA_real_, rmsea_lo = NA_real_, rmsea_hi = NA_real_, srmr = NA_real_,
      note = NA_character_, stringsAsFactors = FALSE
    )
    if (!isTRUE(st$available)) {
      out$note <- sprintf("Not fit: %s.", st$reason)
      return(out)
    }
    m <- cmp$measures[[st$label]]
    out$status <- st$run$status$code
    out$npar <- m$npar
    out$df <- m$df
    if (!sieve_has_solution(st$run)) {
      out$note <- paste(c(sieve_status_alert_text(st$run$status), st$run$error), collapse = " ")
      return(out)
    }
    if (!is.na(test_family)) {
      v <- m$families[[test_family]]$values
      out$chisq <- v[["chisq"]]
      if (is.finite(v[["df"]])) out$df <- v[["df"]]
      out$pvalue <- v[["pvalue"]]
    }
    if (!is.na(family)) {
      v <- m$families[[family]]$values
      out$cfi <- v[["cfi"]]
      out$tli <- v[["tli"]]
      out$rmsea <- v[["rmsea"]]
      out$rmsea_lo <- v[["rmsea_lo"]]
      out$rmsea_hi <- v[["rmsea_hi"]]
    } else {
      out$note <- cmp$common$reason
    }
    out$srmr <- m$srmr$value
    if (identical(out$status, "inadmissible")) {
      out$note <- "Improper solution: a residual variance is negative or a standardized loading exceeds one; the indices are shown, the estimates are not fully interpretable."
    }
    out
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  attr(out, "family") <- family
  attr(out, "test_family") <- test_family
  attr(out, "conf_level") <- result$conf_level %||% cmp$conf_level %||% NA_real_
  out
}

#' One sentence on a status, for a table cell
#'
#' @param status A status from `sieve_fit_status()`.
#' @return A string.
#' @author Ken Kelley
#' @noRd
sieve_status_alert_text <- function(status) {
  switch(status$code %||% "failed",
    not_converged = "The model did not converge, so it has no solution and no fit index.",
    not_identified = "The model is not identified, so it has no solution and no fit index.",
    failed = paste0("The model could not be fit", if (is.null(status$message)) "." else paste0(": ", status$message)),
    ""
  )
}

#' The R that reproduces the comparison of structures
#'
#' One block per fitted structure (its model syntax and the lavaan call,
#' from its own specification, see `sieve_cfa_code()`), then the fit of
#' every fitted structure in the family the table shows, and lavaan's
#' difference test of each nested pair that has one. A structure that was
#' not fit is named in a comment with the reason.
#'
#' @param result A `sieve_compare_structures()` result.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_structures_code <- function(result) {
  cmp <- result$compare
  lines <- c(
    "# Structures to compare: the same first-order groups under one factor, as",
    "# correlated factors, under a second-order factor, and as a bifactor model.",
    sprintf("# Groups (%s): %s.", result$groups$level, paste(result$groups$groups, collapse = ", "))
  )
  for (st in result$structures) {
    if (!isTRUE(st$available)) {
      lines <- c(lines, sprintf("# %s: not fit (%s).", st$label, st$reason))
      next
    }
    lines <- c(
      lines, sprintf("# ---- %s", st$label),
      sieve_cfa_code(st$spec,
        fit_name = sprintf("fit_%s", st$id),
        model_name = sprintf("model_%s", st$id)
      )
    )
  }
  if (is.null(cmp) || is.na(cmp$family)) {
    return(lines)
  }
  columns <- sieve_fit_columns()
  shown <- c(columns[[cmp$test_family]][c("chisq", "df", "pvalue")],
    columns[[cmp$family]][c("cfi", "tli", "rmsea", "rmsea_lo", "rmsea_hi")],
    srmr = sieve_srmr_measure()
  )
  shown <- shown[!is.na(shown)]
  fitted <- result$fitted
  lines <- c(
    lines,
    sprintf(
      "fits_structures <- list(%s)",
      paste(sprintf("%s = fit_%s", fitted, fitted), collapse = ", ")
    ),
    sprintf(
      "# The table on screen: the model test in the %s family and CFI, TLI and RMSEA in the %s family, one family for every model.",
      cmp$test_family, cmp$family
    ),
    sprintf(
      "# The RMSEA interval at %s, the level of every RMSEA interval SIEVER shows (90%% by default, DMAR's and lavaan's).",
      sieve_ci_percent(cmp$conf_level %||% sieve_default_rmsea_conf_level())
    ),
    "# The SRMR is of the covariance residuals alone (srmr_bentler_nomean), the SRMR DMAR reports. For a fit with auxiliary",
    "# variables the table's SRMR is DMAR::sem_fit_measures()'s srmr row, which leaves them out of the average.",
    sprintf(
      "t(sapply(fits_structures, function(f) lavaan::fitMeasures(f, c(%s), fm.args = list(rmsea.ci.level = %s))))",
      paste(sprintf("\"%s\"", shown), collapse = ", "),
      sieve_num_lit(cmp$conf_level %||% sieve_default_rmsea_conf_level())
    )
  )
  tested <- result$tests[result$tests$available %in% TRUE, , drop = FALSE]
  if (nrow(tested)) {
    lines <- c(
      lines,
      "# lavaan's chi square difference test of each nested pair (the second model nested in the first)."
    )
    # Each call is preceded by its note (a boundary constraint, an improper
    # solution), so the recorded test carries the caution the table shows.
    lines <- c(lines, unlist(lapply(seq_len(nrow(tested)), function(i) {
      c(
        if (!is.na(tested$note[i])) paste("#", strwrap(tested$note[i], width = 76)),
        sprintf(
          "lavaan::lavTestLRT(fits_structures[[\"%s\"]], fits_structures[[\"%s\"]]%s)",
          tested$less[i], tested$more[i],
          if (grepl("satorra.bentler.2010",
            tested$label[i],
            fixed = TRUE
          )) ", method = \"satorra.bentler.2010\"" else ""
        )
      )
    }), use.names = FALSE))
  }
  not_nested <- result$tests[!result$tests$nested, , drop = FALSE]
  if (nrow(not_nested)) {
    lines <- c(lines, sprintf(
      "# %s and %s are not nested: no difference test.",
      not_nested$less_label, not_nested$more_label
    ))
  }
  lines
}

#' The compared structures as a figure
#'
#' RMSEA with its interval (at the table's `conf_level`) and CFI, one point
#' per structure, in the family the table shows. The structures are on the horizontal axis in
#' their order; a structure without a solution is left out and named in
#' the subtitle.
#'
#' @param table A `sieve_structures_table()` result.
#' @param palette The palette name (see `sieve_palette_colors()`).
#' @return A ggplot object, or `NULL` when no structure has indices.
#' @author Ken Kelley
#' @noRd
sieve_structures_plot <- function(table, palette = "nd") {
  family <- attr(table, "family")
  shown <- table[is.finite(table$cfi) | is.finite(table$rmsea), , drop = FALSE]
  if (!nrow(shown) || is.null(family) || is.na(family)) {
    return(NULL)
  }
  cols <- sieve_palette_colors(palette, 5L)
  level <- sieve_ci_percent(attr(table, "conf_level") %||% NA_real_)
  long <- rbind(
    data.frame(
      structure = shown$structure,
      index = sprintf("RMSEA (%s, with its %sinterval)", family, if (nzchar(level)) paste0(level, " ") else ""),
      value = shown$rmsea, lo = shown$rmsea_lo, hi = shown$rmsea_hi, stringsAsFactors = FALSE
    ),
    data.frame(
      structure = shown$structure, index = sprintf("CFI (%s)", family),
      value = shown$cfi, lo = NA_real_, hi = NA_real_, stringsAsFactors = FALSE
    )
  )
  long$structure <- factor(long$structure, levels = unique(table$structure))
  long$index <- factor(long$index, levels = unique(long$index))
  left_out <- table$structure[!table$structure %in% shown$structure]
  sub <- paste0(
    sprintf("One family of fit indices for every structure. The interval is the %sconfidence interval of the RMSEA.",
      if (nzchar(level)) paste0(level, " ") else ""),
    if (length(left_out)) sprintf(" Not shown (no solution or not fit): %s.", paste(left_out, collapse = ", ")) else ""
  )
  ggplot2::ggplot(long, ggplot2::aes(x = .data$structure, y = .data$value)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lo, ymax = .data$hi),
      width = 0.2,
      color = cols[1], na.rm = TRUE
    ) +
    ggplot2::geom_point(size = 3, color = cols[1], shape = 16) +
    ggplot2::facet_wrap(~ .data$index, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = paste(strwrap(sub, width = 110), collapse = "\n")) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_text(color = "#5b6770", size = 10)
    )
}

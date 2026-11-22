# fct_efa_dmar.R
# The EFA page's engine is DMAR: the selected solution is
# DMAR::fa_exploratory(), the number
# of factors panel is DMAR::factor_count(), and SIEVER keeps its own table
# layout (pattern, structure, communalities, factor correlations, variance
# accounted) filled with DMAR's values, with DMAR's intervals beside them.
# The helpers below resolve the page's settings to one DMAR call (minres
# to estimator = "ULS", ML to "ML", MLR to "MLR",
# ordered items to WLSMV on the raw ordered items, and minres on a supplied
# correlation matrix as the matrix route), write the R that replays it,
# run it while keeping every condition it raised (a refusal by its class,
# with its `reason`), and read DMAR's table into the matrices the page
# shows. Nothing here estimates anything DMAR does not.

#' The methods the EFA page offers
#'
#' Every one is a common factor method fitted by DMAR::fa_exploratory();
#' principal components are not offered. `"ml"` fits the item responses
#' with the estimator in force (ML or MLR, with FIML or the complete cases
#' as the study's setting says, or WLSMV when items are ordered);
#' `"minres"` is the minimum residual solution (unweighted least squares,
#' DMAR's `estimator = "ULS"`) on the complete item responses; and
#' `"minres_matrix"` is the minimum residual solution on a correlation
#' matrix computed first (the matrix route: polychoric, tetrachoric, mixed,
#' Pearson, or the saturated FIML matrix), supplied to DMAR as `S` with its
#' `N`.
#'
#' @return A named character vector: labels as names, codes as values.
#' @author Ken Kelley
#' @noRd
sieve_efa_method_choices <- function() {
  c(
    "The estimator below (ML, MLR, or WLSMV for ordered items)" = "ml",
    "Minimum residual (minres, ULS) on the item responses" = "minres",
    "Minimum residual (minres, ULS) on a correlation matrix" = "minres_matrix"
  )
}

#' The psych::fa() extraction codes a saved project may name
#'
#' Saved EFA settings may name psych::fa()'s `fm` codes. `"ml"` and
#' `"minres"` keep their meaning; principal axis, weighted and generalized
#' least squares have no DMAR route, and the page says so when a project
#' that chose one is opened.
#'
#' @return A named character vector: `fm` codes as names, labels as values.
#' @author Ken Kelley
#' @noRd
sieve_efa_retired_methods <- function() {
  c(pa = "principal axis factoring", wls = "weighted least squares", gls = "generalized least squares")
}

#' The rotations the EFA page offers
#'
#' DMAR's rotation criteria, each applied by lavaan's gradient projection
#' algorithm from 30 random starts in the form named (Browne, 2001), with
#' rotation-aware standard errors. Direct oblimin with delta = 0 is the
#' standard; `"target"` rotates to the partially specified target in which
#' each item is expected to load on its own construct only (the constructs
#' on the Ingest page; Browne, 1972b). Promax and equamax are not offered:
#' `DMAR::fa_exploratory()` does not offer them, and SIEVER fits every
#' exploratory solution with DMAR; see `sieve_efa_retired_rotation()`.
#'
#' @return A named character vector: labels as names, DMAR names as values.
#' @author Ken Kelley
#' @noRd
sieve_efa_rotation_choices <- function() {
  c(
    "Direct oblimin (oblique, delta = 0)" = "oblimin",
    "Geomin (oblique)" = "geomin",
    "Quartimin (oblique)" = "quartimin",
    "Target: each construct's items (oblique)" = "target",
    "Varimax (orthogonal)" = "varimax",
    "Quartimax (orthogonal)" = "quartimax",
    "None (unrotated)" = "none"
  )
}

#' The rotation in place of a saved one DMAR does not offer
#'
#' A saved study or saved settings may name a rotation SIEVER does not offer:
#' promax or equamax (not offered, since `DMAR::fa_exploratory()` does not
#' offer them), or another rotation of psych::fa(). An
#' oblique one opens as direct oblimin (delta = 0), the standard oblique
#' rotation, and an orthogonal one as varimax, the standard orthogonal
#' rotation, so the form of the solution (correlated or uncorrelated factors)
#' is kept; a rotation of unknown form opens as direct oblimin, the default.
#'
#' @param rotation The saved rotation name.
#' @return `NULL` when the rotation is offered; otherwise a list with
#'   `rotation` (the one in its place), `form` (`"oblique"` or
#'   `"orthogonal"`) and `sentence` (what the page says).
#' @author Ken Kelley
#' @noRd
sieve_efa_retired_rotation <- function(rotation) {
  if (length(rotation) != 1L || is.na(rotation) || rotation %in% sieve_efa_rotation_choices()) return(NULL)
  orthogonal <- c("equamax", "varimin", "bentlerT", "geominT", "bifactor", "target_orthogonal")
  form <- if (rotation %in% orthogonal) "orthogonal" else "oblique"
  replacement <- if (identical(form, "orthogonal")) "varimax" else "oblimin"
  not_yet <- rotation %in% c("promax", "Promax", "equamax")
  name <- if (rotation %in% c("promax", "Promax")) "promax" else rotation
  list(
    rotation = replacement, form = form,
    sentence = sprintf(
      "The saved settings chose %s rotation (%s), which %s; %s, the standard %s rotation, is in its place until you choose another.",
      name, form,
      if (not_yet) {
        "SIEVER does not offer, since DMAR::fa_exploratory() does not"
      } else {
        "DMAR::fa_exploratory() does not offer"
      },
      if (identical(replacement, "varimax")) "varimax" else "direct oblimin (delta = 0)", form
    )
  )
}

#' A rotation in words
#' @param rotation A DMAR rotation name.
#' @return A phrase.
#' @author Ken Kelley
#' @noRd
sieve_efa_rotation_words <- function(rotation) {
  words <- c(
    oblimin = "direct oblimin (oblique, delta = 0)", geomin = "geomin (oblique, epsilon = 0.001)",
    quartimin = "quartimin (oblique)", target = "target rotation to the constructs' items (oblique)",
    target_orthogonal = "target rotation (orthogonal)", varimax = "varimax (orthogonal, Kaiser row weights)",
    quartimax = "quartimax (orthogonal)", none = "no rotation", promax = "promax (oblique)",
    equamax = "equamax (orthogonal)", infomax = "infomax (oblique)", simplimax = "simplimax (oblique)",
    oblimax = "oblimax (oblique)", bentler = "Bentler's criterion (oblique)"
  )
  out <- unname(words[rotation])
  ifelse(is.na(out), rotation, out)
}

#' The DMAR route of the EFA page's settings
#'
#' Resolves the page's method and the estimator in force to DMAR's
#' estimator, missing data method and ordered items. Under `"ml"` the estimator is the one
#' `sieve_lavaan_spec()` resolves (ML with FIML for continuous items by
#' default, DMAR's default; MLR with FIML or ML on the complete cases by
#' choice; WLSMV for ordered items); WLSMV takes the complete cases, as SIEVER's WLSMV rule does
#' everywhere, and DMAR is told so explicitly. Minres has no likelihood, so
#' on the item responses it takes the complete cases (DMAR refuses FIML for
#' it rather than deleting cases unasked) and reads the items as continuous;
#' on a correlation matrix (`"minres_matrix"`) the matrix carries the item
#' treatment.
#'
#' @param fm The page's method (`sieve_efa_method_choices()`).
#' @param spec The estimator specification (`sieve_lavaan_spec()`).
#' @return A list with `form` (`"data"` or `"matrix"`), `estimator`,
#'   `missing` (`NA` for the matrix route), `ordered` (or `NULL`), and
#'   `label`.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_route <- function(fm, spec) {
  fm <- fm %||% "ml"
  if (identical(fm, "minres")) {
    return(list(
      form = "data", estimator = "ULS", missing = "listwise", ordered = NULL,
      label = "minimum residual (ULS) on the item responses, complete cases"
    ))
  }
  if (identical(fm, "minres_matrix")) {
    return(list(
      form = "matrix", estimator = "ULS", missing = NA_character_, ordered = NULL,
      label = "minimum residual (ULS) on the correlation matrix"
    ))
  }
  estimator <- spec$estimator %||% "ML"
  if (identical(estimator, "WLSMV")) {
    return(list(
      form = "data", estimator = "WLSMV", missing = "listwise", ordered = spec$ordered,
      label = "WLSMV on the ordered items (polychoric), complete cases"
    ))
  }
  missing <- if (identical(spec$missing, "fiml")) "fiml" else "listwise"
  list(
    form = "data", estimator = estimator, missing = missing, ordered = NULL,
    label = sprintf("%s %s", estimator, if (identical(missing, "fiml")) "with FIML" else "on the complete cases")
  )
}

#' The partially specified target of the constructs
#'
#' Each construct names a factor and the included items assigned to it, in
#' the list form DMAR::fa_exploratory() takes (`target =`): an item is
#' targeted at zero on every factor but its own and left unspecified on
#' its own; an included item with no construct is unspecified on every
#' factor.
#'
#' @param meta The item ledger.
#' @param items The included items, in the order analyzed.
#' @return A named list of item vectors, in the order the constructs first
#'   appear, or `NULL` when no included item has a construct.
#' @author Ken Kelley
#' @noRd
sieve_efa_target <- function(meta, items) {
  m <- meta[match(items, meta$item), , drop = FALSE]
  keyed <- sieve_construct_assigned(m$construct)
  if (!any(keyed)) {
    return(NULL)
  }
  groups <- unique(as.character(m$construct[keyed]))
  stats::setNames(lapply(groups, function(g) as.character(m$item[keyed & m$construct == g])), groups)
}

#' The DMAR::fa_exploratory() call of one request
#'
#' @param route The route (`sieve_efa_dmar_route()`).
#' @param k The number of factors.
#' @param rotation A DMAR rotation name.
#' @param data The included item responses (the data route).
#' @param basis The correlation basis (the matrix route): `R` and `n`.
#' @param target The target list (`sieve_efa_target()`) for
#'   `rotation = "target"`.
#' @param seed The seed of the rotation's random starts.
#' @param conf_level The confidence level of the coefficients' intervals:
#'   0.95, DMAR's default and the page's level.
#' @param rmsea_conf_level The confidence level of the RMSEA interval of the
#'   fit measures: 0.90 by default (`sieve_default_rmsea_conf_level()`),
#'   DMAR's `rmsea_conf_level` default. It is always passed (DMAR 1.1.0,
#'   the version SIEVER requires, takes it), and the solution records the
#'   level it was formed at (`sieve_efa_fit_level()`).
#' @return A list with `fun` (`"fa_exploratory"`) and `args`.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_call <- function(route, k, rotation = "oblimin", data = NULL, basis = NULL,
                                target = NULL, seed = NULL, conf_level = sieve_default_conf_level(),
                                rmsea_conf_level = sieve_default_rmsea_conf_level()) {
  args <- if (identical(route$form, "matrix")) {
    list(S = basis$R, N = as.integer(basis$n))
  } else {
    list(data = data)
  }
  args$nfactors <- as.integer(k)
  args$estimator <- route$estimator
  if (identical(route$form, "data")) args$missing <- route$missing
  if (length(route$ordered)) args$ordered <- as.character(route$ordered)
  args$rotation <- rotation
  if (rotation %in% c("target", "target_orthogonal")) args$target <- target
  args$conf_level <- conf_level
  args$rmsea_conf_level <- rmsea_conf_level
  args$seed <- seed
  list(fun = "fa_exploratory", args = args)
}

#' The level of the RMSEA interval a DMAR EFA call asks for
#'
#' @param call A call list from `sieve_efa_dmar_call()` or
#'   `sieve_efa_count_call()`.
#' @return The level: the call's `rmsea_conf_level` (an `fa_exploratory()`
#'   call) when it has one, else its `conf_level` (which sets the RMSEA
#'   interval of `factor_count()`).
#' @author Ken Kelley
#' @noRd
sieve_efa_call_rmsea_level <- function(call) {
  level <- call$args$rmsea_conf_level %||% call$args$conf_level
  sieve_valid_conf_level(level, sieve_default_rmsea_conf_level())
}

#' Does a route report an RMSEA?
#'
#' The likelihood and WLSMV routes on the item responses do; minres (ULS)
#' has no chi-square reference distribution, so DMAR reports no RMSEA for
#' it, on the item responses or on a matrix.
#'
#' @param route The route (`sieve_efa_dmar_route()`).
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_efa_has_rmsea <- function(route) {
  identical(route$form, "data") && !identical(route$estimator, "ULS")
}

#' The DMAR::factor_count() call of one request
#'
#' Fits the exploratory model at one factor, two, and so on to `max_factors`
#' under the same route as the selected solution, with the reduced
#' eigenvalues and Velicer's MAP; DMAR applies no cutoff and chooses
#' nothing. The page passes `B = 0`: the parallel analysis reference
#' DMAR's function would add is `DMAR::parallel_analysis()` with the normal
#' reference (the same draws under the same seed), which the Dimensionality
#' page already runs, so the application shows one parallel analysis, not
#' two (see `sieve_efa_count_pa()`).
#'
#' @inheritParams sieve_efa_dmar_call
#' @param max_factors The largest count.
#' @param B Parallel analysis reference datasets (0, the page's value,
#'   omits the reference).
#' @param criterion The information criterion of the comparison.
#' @param conf_level The level of the RMSEA intervals, the only intervals
#'   of the comparison (`DMAR::factor_count()`'s `conf_level`): 0.90 by
#'   default (`sieve_default_rmsea_conf_level()`), DMAR's default for the
#'   RMSEA interval; the page's other intervals are at 0.95.
#' @return A list with `fun` (`"factor_count"`) and `args`.
#' @author Ken Kelley
#' @noRd
sieve_efa_count_call <- function(route, max_factors, data = NULL, basis = NULL, B = 0L,
                                 criterion = "BIC", seed = NULL, conf_level = sieve_default_rmsea_conf_level()) {
  args <- if (identical(route$form, "matrix")) {
    list(S = basis$R, N = as.integer(basis$n))
  } else {
    list(data = data)
  }
  args$max_factors <- as.integer(max_factors)
  args$estimator <- route$estimator
  if (identical(route$form, "data")) args$missing <- route$missing
  if (length(route$ordered)) args$ordered <- as.character(route$ordered)
  args$criterion <- criterion
  args$conf_level <- conf_level
  args$B <- as.integer(B)
  args$percentile <- 0.95
  args$seed <- seed
  list(fun = "factor_count", args = args)
}

#' The R that replays a DMAR exploratory call
#'
#' The call as it ran, every argument written out, with the item data
#' (`data_name`) or the correlation matrix and its sample size
#' (`matrix_name`, `n_name`, defined by the correlation code before it) in
#' place of the values.
#'
#' DMAR sets a supplied seed for the generator in force, so with
#' `rng_kind` the call is wrapped in [withr::with_seed()] under the
#' generator the app used: the script then reproduces the run whatever
#' generator the session that runs it has chosen, and leaves that session's
#' generator and stream as they were.
#'
#' @param call A call from `sieve_efa_dmar_call()` or
#'   `sieve_efa_count_call()`.
#' @param result The name the script gives the result.
#' @param data_name,matrix_name,n_name The names the script uses.
#' @param rng_kind The three [RNGkind()] settings the run used, or `NULL`.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_code <- function(call, result = "efa_fit", data_name = "active_items",
                                matrix_name = "R_items", n_name = "n_complete", rng_kind = NULL) {
  a <- call$args
  literal <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = " ")
  parts <- c(
    if (!is.null(a$S)) c(sprintf("S = %s", matrix_name), sprintf("N = %s", n_name)) else sprintf("data = %s", data_name),
    if (!is.null(a$nfactors)) sprintf("nfactors = %d", a$nfactors),
    if (!is.null(a$max_factors)) sprintf("max_factors = %d", a$max_factors),
    sprintf("estimator = \"%s\"", a$estimator),
    if (!is.null(a$missing)) sprintf("missing = \"%s\"", a$missing),
    if (!is.null(a$ordered)) sprintf("ordered = %s", literal(a$ordered)),
    if (!is.null(a$rotation)) sprintf("rotation = \"%s\"", a$rotation),
    if (!is.null(a$target)) sprintf("target = %s", literal(a$target)),
    if (!is.null(a$criterion)) sprintf("criterion = \"%s\"", a$criterion),
    sprintf("conf_level = %s", format(a$conf_level)),
    if (!is.null(a$rmsea_conf_level)) sprintf("rmsea_conf_level = %s", format(a$rmsea_conf_level)),
    if (!is.null(a$B)) sprintf("B = %d", a$B),
    if (!is.null(a$percentile)) sprintf("percentile = %s", format(a$percentile)),
    if (is.null(a$seed)) "seed = NULL" else sprintf("seed = %d", as.integer(a$seed)),
    if (isTRUE(a$point_estimates_only)) "point_estimates_only = TRUE"
  )
  body <- c(
    sprintf("DMAR::%s(", call$fun),
    paste0("  ", parts, c(rep(",", length(parts) - 1L), "")),
    ")"
  )
  if (length(rng_kind) == 3L && !is.null(a$seed)) {
    return(c(
      sprintf("%s <- withr::with_seed(%d, .rng_kind = %s, .rng_normal_kind = %s, .rng_sample_kind = %s,",
        result, as.integer(a$seed), encodeString(rng_kind[1L], quote = "\""),
        encodeString(rng_kind[2L], quote = "\""), encodeString(rng_kind[3L], quote = "\"")),
      paste0("  ", body),
      ")"
    ))
  }
  c(paste0(result, " <- ", body[1L]), body[-1L])
}

#' Run one DMAR exploratory call and keep what it signaled
#'
#' The call runs as given (in this process, or in a background worker,
#' see `sieve_efa_dmar_start()`); every warning and message is kept, and an
#' error is kept with its condition classes and DMAR's `reason` element, so
#' the page reads a refusal by its class (`dmar_efa_no_solution`,
#' `dmar_no_maximum`, `dmar_target_invalid`, `dmar_target_unidentified`,
#' `dmar_items_invalid`, `dmar_h1_unavailable`, `dmar_refusal`), never by
#' its words.
#'
#' @param call A call from `sieve_efa_dmar_call()` or
#'   `sieve_efa_count_call()`.
#' @param rng_kind Optional: the three [RNGkind()] settings to run under
#'   (those of the process that drew the seed).
#' @return A list with `value`, `ok`, `error`, `error_class`, `reason`,
#'   `warnings`, `messages`, `seconds`, and `fun`.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_run <- function(call, rng_kind = NULL) {
  if (length(rng_kind) == 3L && !identical(RNGkind(), as.character(rng_kind))) {
    old <- RNGkind()
    on.exit(suppressWarnings(RNGkind(old[1L], old[2L], old[3L])), add = TRUE)
    suppressWarnings(RNGkind(rng_kind[1L], rng_kind[2L], rng_kind[3L]))
  }
  fun <- getExportedValue("DMAR", call$fun)
  seen <- new.env(parent = emptyenv())
  seen$warnings <- character(0)
  seen$messages <- character(0)
  started <- proc.time()[["elapsed"]]
  value <- tryCatch(
    withCallingHandlers(
      do.call(fun, call$args),
      warning = function(w) {
        seen$warnings <- c(seen$warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        seen$messages <- c(seen$messages, sub("\n$", "", conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    ),
    error = function(e) e
  )
  failed <- inherits(value, "error")
  out <- list(
    value = if (failed) NULL else value, ok = !failed,
    error = if (failed) conditionMessage(value) else NULL,
    error_class = if (failed) class(value) else NULL,
    reason = if (failed && is.character(value$reason) && length(value$reason) == 1L) value$reason else NULL,
    warnings = unique(seen$warnings), messages = unique(seen$messages),
    seconds = proc.time()[["elapsed"]] - started, fun = call$fun
  )
  out
}

#' A DMAR refusal or failure in words, read by its class
#'
#' @param record A record from `sieve_efa_dmar_run()`.
#' @return One or two sentences: what refused, the reason DMAR gave (its
#'   `reason` element), and the first paragraph of DMAR's message.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_refusal_text <- function(record) {
  cls <- record$error_class %||% character(0)
  message <- strsplit(record$error %||% "No result.", "\n", fixed = TRUE)[[1L]]
  message <- trimws(message[nzchar(trimws(message))][1L] %||% "")
  reason <- record$reason
  lead <- if ("dmar_no_maximum" %in% cls) {
    "DMAR refused the solution: the likelihood has no maximum (reason: no maximum)."
  } else if ("dmar_efa_no_solution" %in% cls) {
    sprintf("DMAR refused the solution (reason: %s).", reason %||% "no solution")
  } else if ("dmar_target_unidentified" %in% cls) {
    sprintf("DMAR refused the target: it does not identify the rotation (reason: %s).", reason %||% "unidentified")
  } else if ("dmar_target_invalid" %in% cls) {
    sprintf("DMAR refused the target rotation (reason: %s).", reason %||% "target")
  } else if ("dmar_items_invalid" %in% cls) {
    sprintf("DMAR refused the item names (reason: %s).", reason %||% "items")
  } else if ("dmar_h1_unavailable" %in% cls) {
    "DMAR refused the analysis: the unrestricted (H1) information its uncertainty needs is unavailable."
  } else if ("dmar_refusal" %in% cls) {
    sprintf("DMAR refused the analysis%s.", if (is.null(reason)) "" else sprintf(" (reason: %s)", reason))
  } else {
    "The analysis stopped with an error."
  }
  paste(lead, message)
}

#' The status of a DMAR exploratory solution
#'
#' The exploratory counterpart of `sieve_fit_status()`, with its codes.
#' DMAR::fa_exploratory() refuses an improper or unidentified solution
#' (class `dmar_efa_no_solution`, with `reason` `"not converged"`,
#' `"improper"`, `"standard errors unavailable"` or `"no maximum"`), so a
#' returned solution is proper: lavaan's post-estimation check passed and
#' the rotation-aware standard errors exist.
#'
#' @param record A record from `sieve_efa_dmar_run()`.
#' @return A list with `code` (`"ok"`, `"not_converged"` or `"failed"`),
#'   `message`, `refusal` (the most specific DMAR class, or `NULL`),
#'   `reason`, and `warnings`.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_status <- function(record) {
  if (isTRUE(record$ok) && !is.null(record$value)) {
    return(list(
      code = "ok", message = "The solution is proper: lavaan's post-estimation check passed, and DMAR refuses an improper one.",
      refusal = NULL, reason = NULL, warnings = record$warnings %||% character(0)
    ))
  }
  cls <- record$error_class %||% character(0)
  specific <- intersect(
    c("dmar_no_maximum", "dmar_efa_no_solution", "dmar_target_unidentified", "dmar_target_invalid",
      "dmar_items_invalid", "dmar_h1_unavailable", "dmar_refusal"),
    cls
  )
  list(
    code = if (identical(record$reason, "not converged")) "not_converged" else "failed",
    message = sieve_efa_dmar_refusal_text(record),
    refusal = if (length(specific)) specific[1L] else NULL,
    reason = record$reason, warnings = record$warnings %||% character(0)
  )
}

#' The matrices of a DMAR exploratory solution
#'
#' Reads DMAR's table, the standardized rows, into what the EFA page shows:
#' the pattern matrix with its limits and standard errors, the factor
#' correlations with their limits (the identity, with no limits, when the
#' rotation keeps the factors uncorrelated), the structure matrix (pattern
#' times Phi; DMAR reports no interval for it), the communalities with
#' their limits, the uniquenesses (one minus the communality, the limits
#' reversed), Hofmann's complexity, and DMAR's `variance_accounted`,
#' `factor_quality` (determinacy, the minimum correlation of equally
#' consistent factor scores, and H, with intervals), `fit_measures` (from
#' the unrotated fit), `method` and `analysis_note`. Full precision; the
#' DMAR table itself is kept as `dmar`. The fields `loadings`, `Structure`,
#' `Phi`, `communality`, `uniquenesses` and `complexity` carry the names a
#' psych::fa() solution uses for them, so the factor plot reads either.
#'
#' @param x A DMAR::fa_exploratory() result.
#' @return A list of class `sieve_efa_solution`.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_solution <- function(x) {
  if (!inherits(x, "dmar_fa_exploratory")) {
    stop("A DMAR::fa_exploratory() result is required.", call. = FALSE)
  }
  tab <- x
  class(tab) <- "data.frame"
  s <- tab[tab$metric == "standardized", , drop = FALSE]
  load <- s[s$op == "=~", , drop = FALSE]
  factors <- unique(as.character(load$lhs))
  items <- unique(as.character(load$rhs))
  cell <- cbind(match(load$rhs, items), match(load$lhs, factors))
  mat <- function(v) {
    m <- matrix(NA_real_, length(items), length(factors), dimnames = list(items, factors))
    m[cell] <- v
    m
  }
  Phi <- diag(1, length(factors))
  dimnames(Phi) <- list(factors, factors)
  Phi_lower <- Phi_upper <- Phi
  Phi_lower[] <- Phi_upper[] <- NA_real_
  diag(Phi_lower) <- diag(Phi_upper) <- 1
  off <- s[s$op == "~~" & s$lhs %in% factors & s$rhs %in% factors & s$lhs != s$rhs, , drop = FALSE]
  if (nrow(off)) {
    i <- cbind(match(off$lhs, factors), match(off$rhs, factors))
    j <- i[, 2:1, drop = FALSE]
    Phi[i] <- Phi[j] <- off$estimate
    Phi_lower[i] <- Phi_lower[j] <- off$ci_lower
    Phi_upper[i] <- Phi_upper[j] <- off$ci_upper
  }
  L <- mat(load$estimate)
  h2 <- s[s$op == "communality", , drop = FALSE]
  h2 <- h2[match(items, h2$lhs), , drop = FALSE]
  cx <- s[s$op == "complexity", , drop = FALSE]
  cx <- cx[match(items, cx$lhs), , drop = FALSE]
  method <- attr(x, "method", exact = TRUE)
  structure(list(
    items = items, factors = factors,
    loadings = L, loadings_lower = mat(load$ci_lower), loadings_upper = mat(load$ci_upper),
    loadings_se = mat(load$se),
    Phi = Phi, Phi_lower = Phi_lower, Phi_upper = Phi_upper,
    Structure = L %*% Phi,
    communality = stats::setNames(h2$estimate, items),
    communality_lower = stats::setNames(h2$ci_lower, items),
    communality_upper = stats::setNames(h2$ci_upper, items),
    uniquenesses = stats::setNames(1 - h2$estimate, items),
    complexity = stats::setNames(cx$estimate, items),
    variance_accounted = attr(x, "variance_accounted", exact = TRUE),
    factor_quality = attr(x, "factor_quality", exact = TRUE),
    fit_measures = attr(x, "fit_measures", exact = TRUE),
    method = method,
    oblique = length(factors) > 1L && !isTRUE(method$orthogonal) && !identical(method$rotation, "none"),
    conf_level = attr(x, "conf_level", exact = TRUE) %||% unique(tab$conf_level)[1L],
    rmsea_conf_level = attr(x, "rmsea_conf_level", exact = TRUE),
    analysis_note = attr(x, "analysis_note", exact = TRUE),
    dmar = x
  ), class = "sieve_efa_solution")
}

#' The level of an EFA solution's RMSEA interval
#'
#' DMAR forms the RMSEA interval of `fa_exploratory()`'s `fit_measures` at
#' the call's `rmsea_conf_level` (0.90 by default), recorded in the
#' solution's `rmsea_conf_level` attribute and as the `conf_level` of the
#' fit measures table. It is read from the solution's record, then from the
#' fit measures.
#'
#' @param f A solution from `sieve_efa_dmar_solution()`.
#' @return The level.
#' @author Ken Kelley
#' @noRd
sieve_efa_fit_level <- function(f) {
  level <- suppressWarnings(as.numeric(f$rmsea_conf_level %||%
    attr(f$fit_measures, "conf_level", exact = TRUE) %||%
    f$conf_level %||% sieve_default_rmsea_conf_level()))
  if (length(level) == 1L && is.finite(level)) level else sieve_default_rmsea_conf_level()
}

#' A value of DMAR's fit measures table
#' @param fm The `fit_measures` attribute.
#' @param term A term.
#' @return The value, or `NA`.
#' @author Ken Kelley
#' @noRd
sieve_efa_fit_value <- function(fm, term) {
  if (is.null(fm)) return(NA_real_)
  v <- fm$value[match(term, fm$term)]
  if (length(v) != 1L) NA_real_ else unname(v)
}

#' The version lines of DMAR's fit measures
#'
#' DMAR records which indices its fit measures are (the version, the test
#' statistic, the correction, the sample size convention) in the
#' `sem_fit` attribute's `footer`; the page shows them beside the values.
#'
#' @param fm The `fit_measures` attribute.
#' @return A character vector (possibly empty).
#' @author Ken Kelley
#' @noRd
sieve_efa_fit_version <- function(fm) {
  info <- attr(fm, "sem_fit", exact = TRUE)
  out <- info$footer %||% character(0)
  out[nzchar(out)]
}

#' Estimated seconds of one DMAR exploratory solution
#'
#' DMAR fits the unrotated model (for the fit measures) and the rotated one
#' with rotation-aware standard errors, then the standardized solution with
#' its delta method standard errors and the determinacy and H rows. Measured
#' with DMAR 1.1.0 and lavaan 0.7-3 against the units of one
#' lavaan::efa() solution (`sieve_efa_wait_baseline()` less its worker
#' start): 2.1 to 3.2 times under ML, 3.3 to 6 times under MLR with FIML
#' (the FIML corrected fit measures included), about 3.1 times under ULS on
#' raw data and 0.6 times under WLSMV, plus a quarter second; 0.14 to 0.26
#' seconds for minres on a supplied matrix of 9 items. Nine bundled tests at
#' three factors take 0.3 seconds under ML and 1.0 under MLR, the 24 tests
#' at five factors 1.1 (ML), 4.2 (MLR) and 5.5 (WLSMV on five categories).
#' The ratio of each run's measured time to this estimate is recorded
#' (`sieve_rate_note("efa_dmar_<estimator>", ...)`) and scales later
#' estimates in the session.
#'
#' @param p The number of items.
#' @param estimator `"ML"`, `"MLR"`, `"ULS"` or `"WLSMV"`.
#' @param n The cases analyzed.
#' @param k The number of factors (a vector gives one estimate each).
#' @param form `"data"` or `"matrix"`.
#' @param patterns The missing data patterns FIML reads
#'   (`sieve_efa_patterns()`: 1 for complete data on the FIML route, 0 off
#'   it). The FIML likelihood and its robust corrections are evaluated pattern
#'   by pattern, which the item count alone does not capture (measured on the
#'   EFA page's route): at 18 items and 1,000 cases, 10% of each item missing
#'   at random (410 patterns), the MLR solution took 31 seconds where the
#'   complete data took 1.6; at 18 items and 300 cases (165 patterns) 12.6
#'   against 1.2; at 9 items 1.7 to 2.2 against 0.7 (56 and 87 patterns). Each
#'   pattern beyond the first adds about 1.9e-4 p^2 seconds under MLR and
#'   1.3e-4 p^2 under ML (12.6 and 8.6 seconds at 18 items and 165 patterns),
#'   times the factor term of `sieve_efa_time_units()`. The FIML route also
#'   has a fixed part (the FIML corrected fit measures and the unrestricted
#'   model they need) that scaling from the 66 item reference does not
#'   capture: about 0.7 seconds under MLR (0.7 to 1.0 seconds on the nine
#'   tests where the scaling gives 0.3), counted under MLR on any route, and
#'   0.4 under ML with FIML (1.2 seconds at 18 items where the scaling gives
#'   0.7). With these terms the solution (whose RMSEA interval is DMAR's own,
#'   at the RMSEA level) took 0.73 to 1.14 times its estimate (six runs: the
#'   nine tests under ML and under MLR with FIML; 12 items, 300 cases,
#'   complete and with 88 patterns; 18 items, 300 cases, 173 patterns, 13.9
#'   seconds under MLR and 8.9 under ML).
#'
#'   ML with FIML, the default for continuous items, is estimated as MLR with
#'   FIML: DMAR's fit measures of an ML fit by FIML form the asymptotic
#'   covariance matrix of the moments (lavaan's FIML corrected indices;
#'   `sieve_ml_bytes()`), so its time grows as MLR's and not as ML's on the
#'   complete cases. Measured on 500 complete cases at three factors, 12, 24
#'   and 36 items took 0.94, 2.42 and 13.2 seconds under ML with FIML against
#'   0.88, 2.58 and 10.4 under MLR with FIML, where the ML estimate gives
#'   0.83, 1.42 and 2.41; the MLR estimate (1.11, 3.76 and 15.6) holds them at
#'   0.64 to 0.85. Its measured ratio is kept under its own name
#'   (`"efa_dmar_ML_fiml"`).
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_wait <- function(p, estimator, n, k, form = "data", patterns = 0) {
  p <- as.numeric(p)
  k <- as.numeric(k)
  if (!length(p) || !is.finite(p) || !length(k) || any(!is.finite(k))) return(NA_real_)
  rate_key <- sieve_efa_dmar_rate_key(estimator, form, patterns)
  if (identical(rate_key, "ML_fiml")) estimator <- "MLR"
  if (identical(form, "matrix")) {
    base <- 0.15 + 2e-4 * sieve_pairs(p) * k
  } else {
    units <- if (identical(estimator, "ULS")) "ML" else estimator
    core <- sieve_efa_wait_baseline(p, units, max(1, sieve_finite_or(n, 500)), k) - 0.7
    multiple <- switch(estimator, ML = 2.5, MLR = 4.5, ULS = 3.1, WLSMV = 0.7, 4.5)
    fiml <- sieve_finite_or(as.numeric(patterns), 0) >= 1
    base <- 0.25 + multiple * core +
      if (identical(estimator, "MLR")) 0.7 else if (fiml && identical(estimator, "ML")) 0.4 else 0
    extra <- max(0, sieve_finite_or(as.numeric(patterns), 0) - 1)
    if (extra > 0 && estimator %in% c("ML", "MLR")) {
      base <- base + extra * p^2 * (if (identical(estimator, "MLR")) 1.9e-4 else 1.3e-4) * (1 + 0.07 * (k - 1))
    }
  }
  ratio <- sieve_rate(paste0("efa_dmar_", rate_key))
  base * (if (is.finite(ratio) && ratio > 0) ratio else 1)
}

#' The name a DMAR exploratory solution's measured time is kept under
#'
#' @inheritParams sieve_efa_dmar_wait
#' @return The estimator, or `"ML_fiml"` for ML with FIML on the item
#'   responses (see `sieve_efa_dmar_wait()`).
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_rate_key <- function(estimator, form = "data", patterns = 0) {
  fiml <- !identical(form, "matrix") && sieve_finite_or(as.numeric(patterns), 0) >= 1
  if (identical(estimator, "ML") && fiml) "ML_fiml" else estimator
}

#' Record the measured time of a DMAR exploratory solution
#' @param estimator,p,n,k,form,patterns As for `sieve_efa_dmar_wait()`.
#' @param seconds The measured seconds of the DMAR calls.
#' @return Invisible `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_time_note <- function(estimator, p, n, k, form, seconds, patterns = 0) {
  expected <- sieve_at_default_rates(sieve_efa_dmar_wait(p, estimator, n, k, form, patterns = patterns))
  if (length(expected) == 1L && is.finite(expected) && expected > 0) {
    sieve_rate_note(paste0("efa_dmar_", sieve_efa_dmar_rate_key(estimator, form, patterns)), seconds, expected)
  }
  invisible(NULL)
}

#' Estimated seconds of a DMAR::factor_count() comparison
#'
#' One unrotated solution per count (about 1.3 times a rotated solution's
#' estimate each, measured on the nine bundled tests: 9.2 seconds for five
#' counts under MLR), the difference tests and information criteria, and,
#' when `B` is above zero, the parallel analysis reference (about one second
#' per thousand reference datasets at 24 items), plus the start of the
#' background worker.
#'
#' @inheritParams sieve_efa_dmar_wait
#' @param max_factors The largest count.
#' @param B Parallel analysis reference datasets (0 on the page).
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_efa_count_wait <- function(p, estimator, n, max_factors, form = "data", B = 0L, patterns = 0) {
  ks <- seq_len(max(1L, as.integer(sieve_finite_or(max_factors, 1L))))
  fits <- sum(vapply(ks, function(k) sieve_efa_dmar_wait(p, estimator, n, k, form, patterns = patterns), numeric(1)))
  2 + 1.3 * fits + 1e-3 * as.numeric(B) * max(1, as.numeric(p) / 24)^2
}

#' The factor count comparison's largest count by default
#'
#' The largest count whose model keeps a degree of freedom (DMAR's own
#' default), at most eight, the range the Dimensionality page uses.
#'
#' @param p The number of items.
#' @return A whole number (at least 1).
#' @author Ken Kelley
#' @noRd
sieve_efa_count_default <- function(p) {
  k <- seq_len(max(1L, as.integer(p) - 1L))
  ok <- ((p - k)^2 - (p + k)) >= 2
  top <- if (any(ok)) max(k[ok]) else 1L
  as.integer(min(8L, top))
}

#' Refuse a DMAR exploratory fit the machine or the sample cannot carry
#'
#' The guards the EFA page applies to the DMAR call before anything
#' starts: under WLSMV the weight matrix
#' of the thresholds and polychoric correlations (`sieve_wlsmv_guard()`);
#' under ML and MLR the memory of the fit (under ML with FIML, the
#' default, the matrix of the moments DMAR's fit measures of a FIML fit
#' form, as MLR's) and, for MLR, the rank of the
#' robust sandwich against the cases (`sieve_mlr_guard()`, with the free
#' parameters of the exploratory model at `k` factors); under minres on the
#' item responses the normal-theory covariance matrix of the sample
#' correlations its sandwich standard errors need, which is the size of
#' MLR's (memory only). The matrix route reads no raw data and is not
#' guarded. The estimator is never changed for the researcher.
#'
#' @param route The route (`sieve_efa_dmar_route()`).
#' @param data The included item responses.
#' @param k The largest number of factors fit.
#' @return `NULL`, or the refusal with its reason and the ways forward.
#' @author Ken Kelley
#' @noRd
sieve_efa_dmar_guard <- function(route, data, k = 1L) {
  if (!identical(route$form, "data") || is.null(data)) return(NULL)
  p <- ncol(data)
  switch(route$estimator,
    WLSMV = sieve_wlsmv_guard(names(data), data, ordered = route$ordered %||% character(0), estimator = "WLSMV"),
    ULS = sieve_mlr_guard(p, "MLR", n = NA_real_,
      npar = sieve_free_parameters(p, k, exploratory = TRUE, means = FALSE)),
    sieve_mlr_guard(p, route$estimator,
      n = if (identical(route$missing, "fiml")) sum(rowSums(!is.na(data)) > 0L) else sum(stats::complete.cases(data)),
      npar = sieve_free_parameters(p, k, exploratory = TRUE, means = identical(route$missing, "fiml")),
      # DMAR's fit measures of an ML fit with FIML form the matrix of the
      # moments MLR needs (sieve_ml_bytes()), so it is guarded as MLR's.
      missing = route$missing)
  )
}

#' The Dimensionality page's parallel analysis beside the factor count
#'
#' The application runs one parallel analysis: `DMAR::factor_count()`'s
#' reference is `DMAR::parallel_analysis()` with the normal reference on
#' the same correlations (identical draws under the same seed, measured on
#' DMAR 1.1.0), so the EFA page's comparison runs with `B = 0` and reads the
#' Dimensionality page's run beside its reduced eigenvalues. The run applies
#' when it is of the current item pool and its observed eigenvalues are the
#' comparison's reduced eigenvalues (to 1e-8): the two are then the same
#' reduction of the same correlation matrix.
#'
#' A comparison on the page's polychoric or tetrachoric matrix (minres on a
#' correlation matrix) and a parallel analysis of the same correlations are
#' not the same matrix: the Dimensionality page's run estimates them through
#' DMAR from the item responses (`lavaan::lavCor()`, the two-step
#' estimator), and the page's matrix is psych's estimate (about 1e-5 apart
#' in the reduced eigenvalues on an ordinal fixture of the bundled tests).
#' That case is `why = "estimator"`: no setting of the Dimensionality page
#' makes the run read here, and WLSMV on the item responses, whose
#' comparison reduces DMAR's polychoric matrix, does.
#'
#' @param table The `DMAR::factor_count()` table.
#' @param shared The Dimensionality page's shared record
#'   (`state$cache$pa`), or `NULL`.
#' @param revision The current pool revision (`sieve_pool_revision()`).
#' @param basis_used The correlations of the comparison's matrix route
#'   (`basis$used`), or `NULL` on the item responses.
#' @return A list with `ok`, `why` (`"none"`, `"stale"`, `"estimator"`, or
#'   `"other"` when it does not apply), and, when it applies, `reference`
#'   (the selected reference at the comparison's counts), `exceeds`,
#'   `label`, `suggested`, `seed`, `n_used`, and `matrix_words`.
#' @author Ken Kelley
#' @noRd
sieve_efa_count_pa <- function(table, shared, revision, basis_used = NULL) {
  if (is.null(shared)) return(list(ok = FALSE, why = "none"))
  if (!identical(shared$revision, revision)) return(list(ok = FALSE, why = "stale", shared = shared))
  eig <- as.numeric(table$eigenvalue)
  m <- length(eig)
  emp <- as.numeric(shared$empirical)
  same <- m > 0L && length(emp) >= m && all(is.finite(eig)) &&
    max(abs(emp[seq_len(m)] - eig)) < 1e-8
  if (!same) {
    ordinal <- length(basis_used) == 1L && basis_used %in% c("polychoric", "tetrachoric") &&
      identical(shared$basis_used, basis_used)
    return(list(ok = FALSE, why = if (ordinal) "estimator" else "other", shared = shared,
      gap = if (m > 0L && length(emp) >= m) max(abs(emp[seq_len(m)] - eig))))
  }
  reference <- as.numeric(shared$values)[seq_len(m)]
  list(
    ok = TRUE, why = NULL, reference = reference, exceeds = eig > reference,
    label = shared$label, suggested = shared$suggested, seed = shared$seed,
    n_used = shared$n_used, matrix_words = shared$matrix_words, statistic = shared$statistic
  )
}

#' The auxiliary variables declared for FIML that a DMAR route leaves out
#'
#' The study's roles (Ingest) may declare auxiliary variables for FIML,
#' the saturated correlates of Graham (2003): with values missing at random
#' given them, FIML without them assumes missingness at random given the
#' included items alone. `DMAR::fa_exploratory()` and
#' `DMAR::factor_count()` take no auxiliary variables, so on the FIML route
#' of the item responses the page says so; the shared saturated FIML matrix
#' (`sieve_state_basis()`, the matrix route) includes them. As in
#' `sieve_auxiliary_apply()`, a declared cluster identifier is not an
#' auxiliary variable.
#'
#' @param state The shared state.
#' @param items The included items.
#' @return The declared auxiliary variables (possibly none).
#' @author Ken Kelley
#' @noRd
sieve_efa_declared_aux <- function(state, items) {
  roles <- tryCatch(shiny::isolate(sieve_roles_table(state$raw, state$items, state$roles)), error = function(e) NULL)
  if (!is.data.frame(roles) || !nrow(roles)) return(character(0))
  aux <- setdiff(as.character(roles$variable[roles$role %in% "auxiliary"]), items)
  design <- tryCatch(sieve_sampling_spec(state), error = function(e) NULL)
  if (!is.null(design) && !identical(design$unit, "independent")) aux <- setdiff(aux, design$cluster)
  aux
}

#' The sentence that says the declared auxiliary variables are not used
#'
#' @param aux The declared auxiliary variables (`sieve_efa_declared_aux()`).
#' @param fun The DMAR function, for example `"fa_exploratory"`.
#' @return One or two sentences, or `NULL` when none is declared.
#' @author Ken Kelley
#' @noRd
sieve_efa_aux_sentence <- function(aux, fun = "fa_exploratory") {
  if (!length(aux)) return(NULL)
  sprintf(
    paste0(
      "The auxiliary variable%s declared for FIML (%s) %s not used: DMAR::%s() takes no auxiliary ",
      "variables, so this FIML fit assumes missingness at random given the included items alone. ",
      "Minimum residual on a correlation matrix uses %s when that matrix is the saturated FIML matrix ",
      "(the study's setting for incomplete continuous items), without standard errors or intervals."
    ),
    if (length(aux) == 1L) "" else "s", paste(aux, collapse = ", "), if (length(aux) == 1L) "is" else "are",
    fun, if (length(aux) == 1L) "it" else "them"
  )
}

#' The missing data patterns FIML reads
#'
#' @param data The included item responses.
#' @param route The route (`sieve_efa_dmar_route()`).
#' @return The number of distinct patterns of missing values among the rows
#'   with an observed item on the FIML route of the item responses (1 when
#'   none is missing); 0 off that route (complete cases, or a matrix).
#' @author Ken Kelley
#' @noRd
sieve_efa_patterns <- function(data, route) {
  if (!identical(route$form, "data") || !identical(route$missing, "fiml") || is.null(data)) return(0L)
  if (!anyNA(data)) return(1L)
  miss <- is.na(as.matrix(data))
  miss <- miss[rowSums(!miss) > 0L, , drop = FALSE]
  if (!nrow(miss)) return(1L)
  nrow(unique(miss))
}

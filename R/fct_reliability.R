# fct_reliability.R
# Reliability of a construct's composite, as plain functions the Reliability
# module (and anything else) calls. A reliability analysis is described once,
# by a resolved specification: the items and their measurement levels, the
# coefficient those levels call for, the missing data treatment and the
# interval method that will actually be used, and the reason whenever a
# request cannot be honored. The estimate, its interval, the status shown to
# the user, and the recorded R all read that one specification.
#
# Estimation is delegated: omega, categorical omega, alpha, and H with
# their intervals to DMAR (at DMAR's defaults unless the user chose
# otherwise), and the one factor
# model whose fit and residual correlations are shown beside them to
# lavaan. What SIEVER adds is the resolution of compatible choices per
# construct, the explicit status of the estimate and of its uncertainty,
# and the missing data provenance.

#' The item data of one construct, as numbers
#'
#' Each column is read by `sieve_as_number()` (the stored codes, user
#' missing values set to `NA`). An infinite value is refused with the
#' item and its rows named, in the words of `sieve_numeric_columns()`, so
#' that no reliability coefficient, item-rest correlation or one factor fit
#' meets it (DMAR's FIML routines stop on it with an internal R message, and
#' a listwise alpha returns `NaN`); `NaN` is a missing value and is
#' returned as `NA`.
#'
#' @param data A data frame holding the item columns.
#' @param items Item column names.
#' @return A data frame of the requested columns, each numeric.
#' @author Ken Kelley
#' @noRd
sieve_reliability_data <- function(data, items) {
  absent <- setdiff(items, names(data))
  if (length(absent)) {
    stop(sprintf(
      "The data do not contain the item(s): %s.",
      paste(absent, collapse = ", ")
    ), call. = FALSE)
  }
  X <- as.data.frame(data)[, items, drop = FALSE]
  X[] <- lapply(X, sieve_as_number)
  infinite <- vapply(X, function(v) any(is.infinite(v)), logical(1))
  if (any(infinite)) {
    read <- sieve_numeric_columns(X, names(X)[infinite])
    at <- read$problems$kind == "not_finite"
    stop(paste(read$text[at], collapse = " "), call. = FALSE)
  }
  X[] <- lapply(X, function(v) {
    v[is.nan(v)] <- NA_real_
    v
  })
  X
}

#' Missing data provenance of a set of items
#'
#' Counted directly from the data, without any model: how many rows there
#' are, how many carry at least one observed item (the rows that hold any
#' information), how many are complete, and how many distinct patterns of
#' observed and missing items occur among the rows that hold information.
#'
#' @param X A data frame of item columns.
#' @return A list with `rows`, `observed` (rows with at least one observed
#'   item), `empty` (rows with none), `complete`, and `patterns`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_counts <- function(X) {
  seen <- !is.na(as.matrix(X))
  per_row <- rowSums(seen)
  informative <- per_row > 0L
  pattern <- if (any(informative)) {
    do.call(paste0, as.data.frame(seen[informative, , drop = FALSE] + 0L))
  } else {
    character(0)
  }
  list(
    rows = nrow(X),
    observed = sum(informative),
    empty = sum(!informative),
    complete = sum(per_row == ncol(X)),
    patterns = length(unique(pattern))
  )
}

#' The constructs a reliability analysis covers
#'
#' @param meta The item ledger.
#' @param columns Names of the columns present in the item data.
#' @param scope Construct totals, nested facets, or both.
#' @return A named list, one element per construct that has an included
#'   item, each a list with `construct`, `items`, `types`, and `k`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_items <- function(meta, columns, scope = c("construct", "facet", "combined")) {
  scope <- match.arg(scope)
  active <- meta[meta$status == "active" & meta$item %in% columns &
    sieve_construct_assigned(meta$construct), , drop = FALSE]
  constructs <- unique(active$construct)
  out <- if (scope != "facet") lapply(constructs, function(cn) {
    rows <- active[active$construct == cn, , drop = FALSE]
    list(
      construct = cn, items = rows$item, types = as.character(rows$type), k = nrow(rows),
      id = cn, scope = "construct", parent_construct = cn, facet = NA_character_
    )
  }) else list()
  if (scope != "facet") names(out) <- constructs
  if (scope != "construct") {
    for (group in sieve_facet_groups(meta, columns)) {
      # The identity is separate from the displayed construct:facet label.
      # Prefixing also keeps a construct whose label resembles a facet key distinct.
      id <- paste0("facet:", group$id)
      while (id %in% names(out)) id <- paste0("facet:", id)
      out[[id]] <- list(
        construct = group$label, items = group$items, types = group$types,
        k = group$k, id = id, scope = "facet",
        parent_construct = group$construct, facet = group$facet
      )
    }
  }
  out
}

#' The stable group identity of a reliability specification
#' @param spec A reliability specification.
#' @return A character key: the group identity, or the construct for a
#'   record that names no group.
#' @author Ken Kelley
#' @noRd
sieve_reliability_id <- function(spec) spec$group_id %||% spec$construct

#' Register a compact reliability bundle without repeating estimation
#' @param state Shared state.
#' @param results Named group bundles containing spec, omega, alpha, and optionally H.
#' @param note Researcher's description of the analysis.
#' @param inputs,revision,scientific_dataset_id Optional original inputs and identities
#'   captured before an asynchronous bootstrap began.
#' @return Invisibly the analysis identifier.
#' @author Ken Kelley
#' @noRd
sieve_register_reliability <- function(state, results, note = "Reliability by construct or facet",
                                       inputs = NULL, revision = NULL, scientific_dataset_id = NULL) {
  if (!length(results)) return(invisible(NULL))
  specs <- lapply(results, function(r) r$omega$spec %||% r$spec)
  compact <- lapply(results, function(r) {
    om <- r$omega
    keep <- intersect(c(
      "construct", "k", "coefficient", "label", "estimate", "se", "lower", "upper",
      "conf_level", "status", "missing", "n", "warnings", "uncertainty", "versions"
    ), names(om))
    om <- om[keep]
    om$uncertainty$draws <- NULL # counts, reasons, actual method, and RNG are retained
    mi <- r$model_implied
    if (!is.null(mi)) mi <- mi[intersect(keep, names(mi))]
    list(omega = om, alpha = r$alpha, H = r$H, model_implied = mi)
  })
  spec <- list(
    kind = "reliability", groups = specs,
    observed = unique(unlist(lapply(specs, function(sp) sp$items), use.names = FALSE)),
    scopes = unique(vapply(specs, function(sp) sp$scope %||% "construct", character(1))),
    rng = lapply(results, function(r) r$omega$uncertainty$rng),
    interval_states = lapply(results, function(r) list(
      omega = r$omega$uncertainty$status, alpha = r$alpha$uncertainty$status,
      attempted = r$omega$uncertainty$B_attempted
    ))
  )
  spec$key <- sieve_hash(spec)
  good <- vapply(results, function(r) {
    identical(r$omega$status$code, "ok") &&
      (is.null(r$alpha) || identical(r$alpha$status$code, "ok")) &&
      r$omega$uncertainty$status %in% c("completed", "not_requested") &&
      (is.null(r$alpha) || r$alpha$uncertainty$status %in% c("completed", "not_requested"))
  }, logical(1))
  status <- list(
    code = if (all(good)) "ok" else "partial",
    message = sprintf(
      "%d group%s recorded. Estimate, interval, sample, and condition statuses are retained separately for every coefficient.",
      length(results), if (length(results) == 1L) "" else "s"
    )
  )
  run <- list(
    spec = spec, fit = NULL, status = status, estimates = compact,
    warnings = unique(unlist(
      lapply(
        results,
        function(r) c(
          r$omega$warnings,
          r$alpha$warnings
        )
      ),
      use.names = FALSE
    )),
    inputs = inputs, revision = revision, scientific_dataset_id = scientific_dataset_id
  )
  sieve_register_analysis(state, run, note = note)
}

#' The interval methods SIEVER offers for omega
#'
#' `"recommended"` is DMAR's defaults, resolved construct by construct by
#' `sieve_reliability_recommended()`. The Wald intervals of
#' model implied omega come in two forms built from one standard error:
#' limits on the scale of omega (`"mlr"`, `"ml"`), and limits formed on the
#' logit scale and transformed back (Browne, 1982), the `_logistic`
#' variants (`"mlr_logistic"`, DMAR's default, `"ml_logistic"`,
#' and `"adf_logistic"`, the asymptotic distribution-free standard error of
#' a weighted least squares fit, Browne, 1984, complete cases only).
#'
#' Three are bootstraps, all DMAR's counted bootstrap:
#' `"percentile"`, `"dmar_bca"` (the BCa interval), and
#' `"bootstrap_se_logistic"` (the standard deviation of the replications as
#' a standard error, carried to the logit scale by the delta method; Kelley
#' and Pornprasertmanit, 2016, Eq. 32). DMAR resamples the rows the point
#' estimate used, refits each replication with its point estimator, and
#' counts every replication's fate (used, improper and kept, improper and
#' set aside, failed, not converged), so the interval comes with its
#' counts, and a recorded DMAR call that replays it. `"dmar_percentile"`,
#' a name a saved project or script may hold, is
#' accepted and resolves to `"percentile"`, the same counted call, and only
#' `"percentile"` is on the menu. With a declared
#' cluster design SIEVER's own counted bootstrap resamples whole clusters
#' (`fct_reliability_boot.R`), which DMAR's engine does not.
#'
#' @return A named character vector: labels as names, method codes as values.
#' @author Ken Kelley
#' @noRd
sieve_reliability_methods <- function() {
  c(
    "DMAR's defaults: MLR (sandwich) SE with logit-scale limits for model-implied \u03c9; percentile bootstrap for robust \u03c9; BCa for categorical \u03c9" = "recommended",
    "MLR (sandwich) SE, logit-scale limits" = "mlr_logistic",
    "MLR (sandwich) SE, limits on the omega scale" = "mlr",
    "ML, logit scale" = "ml_logistic",
    "ML" = "ml",
    "ADF, logit scale (complete cases)" = "adf_logistic",
    "Profile likelihood" = "likelihood",
    "Bootstrap percentile (DMAR, counted replications)" = "percentile",
    "Bootstrap BCa (DMAR, counted replications)" = "dmar_bca",
    "Bootstrap standard error, logit scale (DMAR, counted replications)" = "bootstrap_se_logistic"
  )
}

#' The closed form interval methods, derived for model implied omega
#'
#' @return A character vector of method codes.
#' @author Ken Kelley
#' @noRd
sieve_reliability_closed_methods <- function() {
  c("mlr_logistic", "mlr", "ml_logistic", "ml", "adf_logistic", "likelihood")
}

#' What robust omega is, and what it is not, in the interface's words
#'
#' The explanation the Reliability tab shows beside the omega variant
#' control, on DMAR's robust omega: robust to misspecification of the total
#' variance only, not to outliers (the sense of Zhang & Yuan, 2016) and not
#' robust ML standard errors; the numerator is the one-factor model's under both
#' denominators (Flora, 2020); under a correct model the two estimate the
#' same parameter, and their denominators differ by exactly the summed
#' residual covariance.
#'
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_reliability_variant_text <- function() {
  paste0(
    "Robust omega (observed denominator) is the default: Kelley and Pornprasertmanit's (2016) ",
    "hierarchical omega (not the bifactor omega hierarchical of Zinbarg et al., 2005), ",
    "(\u03a3\u03bb)\u00b2 / 1'S1, with the loadings \u03bb of the one-factor model (factor variance 1). ",
    "Its denominator is the variance of the composite actually scored, 1'S1, from the unrestricted ",
    "covariance matrix S of the items (ML divisor N; under FIML the saturated FIML estimate), so ",
    "the denominator does not depend on the one-factor model. Omega (model-implied denominator) ",
    "divides by the variance that model reproduces, (\u03a3\u03bb)\u00b2 + \u03a3\u03c8. The two ",
    "denominators differ by exactly 1'(S - \u03a3\u0302)1, the sum of the residual covariances: the ",
    "model-implied version treats covariance the model leaves out as nonexistent, while robust omega ",
    "counts it as composite variance not due to the target factor. Under a correct model the two ",
    "estimate the same parameter. Robust means robust to misspecification of the total variance ",
    "only: it is not robustness to outliers (Zhang & Yuan, 2016, use \"robust omega\" in that sense) ",
    "and not robust ML standard errors. The numerator, the squared sum of the loadings, still comes ",
    "from that model, so robust omega does not repair a misspecified model (Flora, 2020): a doublet ",
    "or a method factor among a few same-signed items inflates omega under either denominator. ",
    "Robust omega has no closed form interval (the delta method and profile likelihood intervals are ",
    "derived for the model-implied ratio and ignore the sampling of the observed denominator), so its ",
    "interval is a bootstrap, which runs on request with its wait stated first; DMAR names the ",
    "percentile bootstrap for it, the interval Kelley and Pornprasertmanit (2016) found to perform ",
    "well. Omega (model-implied denominator) takes DMAR's default interval, the MLR (sandwich) standard ",
    "error with logit-scale limits, which is immediate."
  )
}

#' What DMAR's study of the logit-scale limits found, in words
#'
#' The evidence behind DMAR's default interval for model implied omega,
#' `ci_method = "mlr_logistic"`, as DMAR's study reports it
#' (`methodology/reliability_limits/RESULTS.md` and `summary_cells.csv` in
#' DMAR's source repository; the folder is not in DMAR's CRAN package; `?DMAR::reliability_omega`
#' summarizes it). The design and the decision rule were written before the
#' run: one-factor models of six items (loadings .8 to .5, and one with a
#' weak item at .2) and of four items near the bound (.95 to .85), and a
#' three-factor model of three-item factors; N of 50, 100, 200 and 500;
#' 5,000 replications in each of 40 cells (16 under listwise deletion and
#' 16 under FIML with normal data and the normal theory ML standard error,
#' 8 with factors and errors from a standardized chi-square on 4 df fitted
#' by MLR with the Huber-White standard error). Read from
#' `summary_cells.csv` for omega: logit-scale coverage .9391 to .9562 over
#' the 48 normal-data factor by N cells (Wald limits on the scale of omega
#' from the same standard error .9097 to .9524), and .9192 to .9508 over
#' the 16 nonnormal MLR factor by N cells, .9360 at N = 200 and .9418 at
#' N = 500 for the six-item factor with loadings .8 to .5, whose
#' Huber-White standard error was 4.4 and 3.0 percent below the empirical
#' standard deviation there. The study did not include SIEVER's default
#' interval, the percentile bootstrap of robust omega.
#'
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_reliability_limits_study_text <- function() {
  paste0(
    "DMAR chose the logit-scale limits on a Monte Carlo study whose design and decision rule were ",
    "written before it ran (DMAR's source repository, methodology/reliability_limits, which the CRAN ",
    "package does not include): one-factor models of four and six items and a three-factor model of ",
    "three-item factors, N of 50, 100, 200 and 500, and 5,000 replications in each of 40 cells. With ",
    "normal data and the normal theory ML standard error (complete cases and FIML), the logit-scale ",
    "limits for Omega (model-implied denominator) covered between .939 and .956 in every one of the 48 ",
    "factor by sample size cells, where limits on the scale of omega from the same standard error ",
    "covered between .910 and .952, their misses falling mostly below the lower limit. With nonnormal ",
    "data (factors and errors from a standardized chi-square on 4 df) fitted by MLR, the logit-scale ",
    "limits from the Huber-White standard error covered between .919 and .951 in its 16 factor by sample ",
    "size cells, mostly short of .95 because that standard error is too small at these sizes, and the ",
    "shortfall shrank with N (.936 at N = 200 and .942 at N = 500 for the six-item factor with loadings ",
    ".8 to .5). No cell used the robust.sem standard error this interval takes with complete cases, which ",
    "estimates the same asymptotic variance as the Huber-White one. That study did not include SIEVER's ",
    "default interval, the percentile bootstrap of Robust omega (observed denominator)."
  )
}

#' The bootstrap interval methods, by code
#'
#' @return A character vector of method codes.
#' @author Ken Kelley
#' @noRd
sieve_reliability_bootstrap_methods <- function() {
  c("percentile", "dmar_percentile", "dmar_bca", "bootstrap_se_logistic")
}

#' DMAR's default interval for a coefficient
#'
#' SIEVER's recommended interval for each coefficient follows DMAR's
#' default:
#'
#' * model implied omega takes `DMAR::reliability_omega()`'s default,
#'   `ci_method = "mlr_logistic"`: the MLR (sandwich)
#'   standard error with the limits formed on the logit scale and
#'   transformed back (Browne, 1982), chosen in DMAR on the Monte Carlo
#'   study `sieve_reliability_limits_study_text()` describes (40 cells of
#'   5,000 replications; logit-scale coverage .939 to .956 with normal
#'   data, .919 to .951 under MLR with nonnormal data), which did not
#'   include SIEVER's default interval, the percentile bootstrap of robust
#'   omega. Under the FMI t
#'   reference (`wald_df = "missing_information"`), calibrated for the
#'   limits on the scale of omega only, DMAR's default is `"mlr"`, and so
#'   is this one.
#' * Robust omega (the observed denominator) has no closed form interval:
#'   DMAR refuses `"mlr_logistic"` for it (the delta method ignores the
#'   sampling of the observed denominator), and its own default is the
#'   point estimate with a message naming `"percentile"` as the interval
#'   to request. SIEVER's default is that percentile bootstrap (Kelley and
#'   Pornprasertmanit, 2016), run on request.
#' * Categorical omega takes `DMAR::reliability_omega_categorical()`'s
#'   default, BCa.
#'
#' @param item_class `"continuous"`, `"categorical"`, or `"mixed"`.
#' @param denominator `"observed"` or `"model_implied"`.
#' @param wald_df The Wald reference of the request (`Inf` or
#'   `"missing_information"`).
#' @return A method code from `sieve_reliability_methods()`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_recommended <- function(item_class, denominator = "observed", wald_df = Inf) {
  if (identical(item_class, "categorical")) {
    "dmar_bca"
  } else if (identical(denominator, "observed")) {
    "percentile"
  } else if (identical(wald_df, "missing_information")) {
    "mlr"
  } else {
    "mlr_logistic"
  }
}

#' The resolved interval method of a request
#'
#' @param ci_method A method code, possibly `"recommended"`.
#' @param item_class,denominator,wald_df As for
#'   `sieve_reliability_recommended()`.
#' @return The method code a construct will use.
#' @author Ken Kelley
#' @noRd
sieve_reliability_resolve_method <- function(ci_method, item_class, denominator = "observed",
                                             wald_df = Inf) {
  if (identical(ci_method, "recommended")) {
    sieve_reliability_recommended(item_class, denominator, wald_df)
  } else {
    ci_method
  }
}

#' The engine's name for an interval method
#'
#' The bootstraps are DMAR's counted `"percentile"`, `"bca"`, and
#' `"bootstrap_se_logistic"`; the closed forms keep their names. (SIEVER's
#' own counted bootstrap, used with a declared cluster design, calls the
#' point estimator, `ci_method = "none"`, once per replication, and asks for
#' it by that name.)
#'
#' @param ci_method A method code from `sieve_reliability_methods()`.
#' @return The `ci_method` value the DMAR call takes.
#' @author Ken Kelley
#' @noRd
sieve_reliability_engine_ci <- function(ci_method) {
  switch(ci_method,
    percentile = "percentile",
    dmar_percentile = "percentile",
    dmar_bca = "bca",
    ci_method
  )
}

#' Which interval methods apply to one construct
#'
#' The compatibility table behind the resolved specification, DMAR's own
#' (`?DMAR::reliability_omega`, "Which intervals apply to which
#' denominator"). Categorical omega (Green and Yang, 2009) has only
#' bootstrap intervals in the engine. Omega with the observed denominator
#' (robust omega) has only bootstrap intervals, because the closed forms
#' are derived for the model implied ratio. With full information maximum
#' likelihood the engine offers the ML and robust ML delta method intervals
#' (both forms) and the bootstrap, not the profile likelihood and not the
#' ADF interval, which need complete cases.
#'
#' @param item_class `"continuous"`, `"categorical"`, or `"mixed"`.
#' @param missing The missing data treatment that will be used.
#' @param denominator `"model_implied"` or `"observed"`.
#' @return A character vector of method codes, in menu order.
#' @author Ken Kelley
#' @noRd
sieve_reliability_supported <- function(item_class, missing, denominator) {
  bootstrap <- sieve_reliability_bootstrap_methods()
  if (identical(item_class, "categorical") || identical(denominator, "observed")) {
    return(bootstrap)
  }
  if (identical(missing, "fiml")) {
    return(c("mlr_logistic", "mlr", "ml_logistic", "ml", bootstrap))
  }
  c(sieve_reliability_closed_methods(), bootstrap)
}

#' The class of a construct's items
#'
#' @param types Measurement levels of the items (`"continuous"`,
#'   `"ordered"`, `"binary"`).
#' @return `"continuous"` when no item is ordered or binary, `"categorical"`
#'   when every item is, and `"mixed"` otherwise.
#' @author Ken Kelley
#' @noRd
sieve_reliability_class <- function(types) {
  categorical <- types %in% c("ordered", "binary")
  if (all(categorical)) {
    "categorical"
  } else if (!any(categorical)) {
    "continuous"
  } else {
    "mixed"
  }
}

#' The item treatments the Reliability tab offers
#'
#' The coefficient follows the item types (categorical omega for a
#' construct of ordered or binary items, omega from the linear one factor
#' model otherwise); the treatment control lets the researcher take every
#' item as continuous or as ordered for the reliability analysis without
#' retyping the items (the estimator is a choice).
#'
#' @return A named character vector: labels as names, codes as values.
#' @author Ken Kelley
#' @noRd
sieve_reliability_treatments <- function() {
  c(
    "As typed on the Ingest tab (categorical \u03c9 for ordered or binary items)" = "auto",
    "Every item as continuous: \u03c9 from the linear one factor model (ML or MLR, FIML available)" = "continuous",
    "Every item as ordered: categorical \u03c9 from the polychoric model (complete cases)" = "ordered"
  )
}

#' The measurement levels a reliability analysis reads, under a treatment
#'
#' @param types The confirmed measurement levels of a construct's items.
#' @param treatment One of `sieve_reliability_treatments()`.
#' @return `types` as the analysis reads them: unchanged under `"auto"`,
#'   every item `"continuous"` or every item `"ordered"` otherwise (a binary
#'   item stays binary under the ordered treatment).
#' @author Ken Kelley
#' @noRd
sieve_reliability_treat <- function(types, treatment = "auto") {
  treatment <- match.arg(treatment, unname(sieve_reliability_treatments()))
  types <- as.character(types)
  switch(treatment,
    auto = types,
    continuous = rep("continuous", length(types)),
    ordered = ifelse(types == "binary", "binary", "ordered")
  )
}

#' The interval menu for a project
#'
#' The menu holds every method that applies to at least one construct under
#' the current choices, so an option that could not work for any construct
#' is not shown. When the selection in force is not on the menu (for
#' example a closed form method in a project whose constructs are all
#' ordered), the selection moves to the first method that applies and the
#' note says so.
#'
#' @param classes Item class of each construct (see
#'   `sieve_reliability_class()`).
#' @param missing The requested missing data treatment.
#' @param denominator The requested omega variant.
#' @param selected The method currently selected, or `NULL`.
#' @return A list with `choices` (a named character vector), `selected`,
#'   `changed` (whether the selection had to move), and `note` (plain
#'   language, or `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_reliability_menu <- function(classes, missing = "listwise",
                                   denominator = "observed", selected = NULL) {
  all_methods <- sieve_reliability_methods()
  # "dmar_percentile", a selection a saved project may hold, is the
  # percentile bootstrap: the same counted call.
  if (identical(selected, "dmar_percentile")) selected <- "percentile"
  applies <- unique(c("recommended", unlist(lapply(classes, function(cl) {
    used <- if (identical(cl, "categorical")) "listwise" else missing
    sieve_reliability_supported(cl, used, denominator)
  }))))
  choices <- all_methods[all_methods %in% applies]
  keep <- !is.null(selected) && length(selected) == 1L && selected %in% choices
  now <- if (keep) selected else unname(choices[1])
  changed <- !keep && !is.null(selected) && length(selected) == 1L
  all_categorical <- length(classes) > 0L && all(classes == "categorical")
  any_categorical <- any(classes == "categorical")
  any_continuous <- any(classes != "categorical")
  recommended <- if (identical(now, "recommended")) {
    paste0(
      "DMAR's default intervals are resolved construct by construct: ",
      paste(c(
        if (any_continuous && identical(denominator, "observed")) {
          paste0(
            "robust omega has no closed form interval, so it takes the bootstrap DMAR names for it, the ",
            "percentile bootstrap (Kelley & Pornprasertmanit, 2016), DMAR's counted bootstrap, which ",
            "resamples the rows, refits each replication, and reports how many replications were ",
            "attempted, converged, were admissible, and were used"
          )
        },
        if (any_continuous && !identical(denominator, "observed")) {
          paste0(
            "model implied omega takes DMAR's default, the MLR (sandwich) standard error with the limits formed on the logit ",
            "scale and transformed back (Browne, 1982), which keeps them inside (0, 1) and covered ",
            "closer to the nominal level in DMAR's simulation than limits on the scale of omega"
          )
        },
        if (any_categorical) {
          paste0(
            "categorical omega takes DMAR's default, the BCa bootstrap, DMAR's counted bootstrap, ",
            "whose acceleration constant needs a jackknife of one refit per row when the replications are ",
            "fewer than the rows"
          )
        }
      ), collapse = "; "),
      ". A bootstrap runs only on request, with its wait stated before it starts; until then the ",
      "table shows the point estimate and says the bootstrap has not been run. The other intervals ",
      "on the menu remain available."
    )
  }
  situation <- if (!is.null(recommended)) {
    recommended
  } else if (all_categorical) {
    paste0(
      "Every construct here is made of ordered or binary items, so the coefficient is ",
      "categorical omega (Green & Yang, 2009). It has no closed form interval, so the ",
      "interval is a bootstrap. DMAR's default for categorical omega is BCa (the first choice, ",
      "DMAR's defaults). Every bootstrap here is DMAR's counted bootstrap: ",
      "it resamples the rows, refits each replication with its point estimator, and reports how ",
      "many replications were attempted, converged, were admissible, and were used. The actual ",
      "method is reported, including a percentile fallback when DMAR cannot form BCa."
    )
  } else if (identical(denominator, "observed")) {
    paste0(
      "Robust omega has no closed form interval (the closed forms are derived for the ",
      "model implied denominator), so its interval is a bootstrap."
    )
  } else if (any_categorical) {
    paste0(
      "This project mixes constructs of continuous items and constructs of ordered or ",
      "binary items. A closed form interval applies only to the continuous constructs; ",
      "the table states, construct by construct, which interval was used and why any is ",
      "absent. A bootstrap interval applies to every construct."
    )
  } else if (identical(missing, "fiml")) {
    paste0(
      "The profile likelihood and ADF intervals need complete cases, and DMAR does not form them ",
      "under FIML, so they are not on the menu."
    )
  }
  moved <- if (changed) {
    label_of <- function(code) {
      hit <- names(all_methods)[match(code, all_methods)]
      if (is.na(hit)) code else hit
    }
    sprintf(paste0(
      "\"%s\" does not apply to any construct under the current choices, so the ",
      "interval method was set to \"%s\"."
    ), label_of(selected), label_of(now))
  }
  note <- if (length(c(moved, situation))) paste(c(moved, situation), collapse = " ")
  list(choices = choices, selected = now, changed = changed, note = note)
}

#' The label of a coefficient, naming its denominator
#'
#' The labels DMAR's definition of robust omega calls for: every value is
#' "Robust omega (observed
#' denominator)" or "Omega (model-implied denominator)", never an
#' unqualified omega. For ordered or binary items the label also says which
#' matrix built the denominator: the unrestricted polychoric correlations
#' (Kelley & Pornprasertmanit, 2016), or the polychoric correlations the
#' one-factor model implies (Green & Yang, 2009). H's label, set with its
#' model, says whether it describes observed item scores or latent
#' responses.
#'
#' @param coefficient `"omega"`, `"omega_robust"`, or
#'   `"omega_categorical"`.
#' @param mixed Whether ordered items enter a linear coefficient as their
#'   numeric codes.
#' @param denominator For categorical omega, `"observed"` (the default) or
#'   `"model_implied"`; ignored otherwise (the coefficient code carries it).
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_reliability_label <- function(coefficient, mixed = FALSE, denominator = "observed") {
  base <- switch(coefficient,
    omega_robust = "Robust omega (observed denominator",
    omega_categorical = if (identical(denominator, "model_implied")) {
      "Omega (model-implied denominator: polychoric correlations implied by the one-factor model"
    } else {
      "Robust omega (observed denominator: unrestricted polychoric correlations"
    },
    "Omega (model-implied denominator"
  )
  paste0(base, if (isTRUE(mixed)) "; item scores as continuous" else "", ")")
}

#' The denominator a specification uses, read safely
#'
#' A saved categorical specification may hold `NA` (or nothing) as its
#' denominator; it is read as `"observed"`, the denominator its engine
#' computes when none is named.
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @return `"observed"` or `"model_implied"`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_denominator <- function(spec) {
  d <- spec$denominator
  if (length(d) == 1L && !is.na(d) && d %in% c("observed", "model_implied")) d else "observed"
}

#' Resolve the Specification of One Construct's Reliability Analysis
#'
#' Turns the request (items, measurement levels, missing data treatment,
#' interval method, omega variant) into what will actually be computed, and
#' records the reason whenever the two differ. Nothing is substituted
#' silently:
#'
#' * a construct of continuous items gets omega from the one factor model
#'   (`"model_implied"` denominator) or robust omega (`"observed"`);
#' * a construct of ordered or binary items gets categorical omega (Green
#'   and Yang, 2009), the reliability of the sum of the items' category
#'   ranks. The engine
#'   fits those items by weighted least squares on polychoric correlations,
#'   for which full information maximum likelihood does not exist, so a
#'   request for it resolves to complete cases and says so;
#' * a construct that mixes the two has no categorical coefficient in the
#'   engine; omega from the linear one factor model of the item scores is
#'   computed and labeled as such;
#' * an interval method that does not apply to the construct resolves to no
#'   interval, with the reason (see `sieve_reliability_supported()`).
#'
#' @param items Item column names (three or more for omega).
#' @param types Measurement level of each item.
#' @param construct The construct's name (a label only).
#' @param missing Requested treatment: `"fiml"` (the default, DMAR's
#'   default for the reliability family: full information maximum
#'   likelihood, which keeps every row with an observed item) or
#'   `"listwise"` (complete cases). The treatment is passed to DMAR by
#'   name, so with no value missing `"fiml"` fits the complete data by FIML
#'   (observed information), where a DMAR call that leaves `missing` unset
#'   analyzes complete data as they stand; the estimate is the same.
#' @param ci_method Requested interval: `"recommended"` (the default:
#'   DMAR's defaults, resolved by `sieve_reliability_recommended()`:
#'   `"mlr_logistic"` for model implied omega, the percentile bootstrap for
#'   robust omega, which has no closed form interval, and the BCa bootstrap
#'   for categorical omega), the Wald intervals `"mlr_logistic"`, `"mlr"`,
#'   `"ml_logistic"`, `"ml"`, and `"adf_logistic"` (the `_logistic` forms
#'   build the limits on the logit scale), `"likelihood"`, the bootstraps
#'   `"percentile"`, `"dmar_bca"`, and `"bootstrap_se_logistic"` (DMAR's
#'   counted bootstraps; `"dmar_percentile"` is accepted and resolves to
#'   `"percentile"`), or `"none"`.
#' @param denominator Requested omega variant: `"observed"` (robust
#'   omega, the default, as it is DMAR's) or `"model_implied"`. When it is not supplied and
#'   `ci_method` is a closed form (`"mlr_logistic"`, `"mlr"`,
#'   `"ml_logistic"`, `"ml"`, `"adf_logistic"`, `"likelihood"`), the variant
#'   those intervals are derived for, `"model_implied"`, is used.
#' @param conf_level Confidence level, in (0, 1).
#' @param B Number of bootstrap replications: 10000 by default, DMAR's
#'   default for its reliability bootstraps. On the Reliability tab a
#'   bootstrap runs only on request, with its wait stated before it starts;
#'   in a script it runs when [sieve_reliability_construct()] is given a
#'   specification with `run_bootstrap = TRUE`, on the processes its
#'   `workers` argument names, reporting through its `progress` argument.
#' @param seed Optional bootstrap seed (a single number, or `NULL`).
#' @param run_bootstrap `FALSE` resolves a bootstrap request to "selected,
#'   not run yet" (status `"pending"`): the point estimate is computed and
#'   the expensive resampling waits for an explicit request.
#' @param group_id Stable group identity; distinct from its display label.
#' @param aux Optional continuous auxiliary column names for FIML.
#' @param cluster Optional independent cluster labels aligned to all supplied data rows.
#'   Intervals use the counted whole-cluster percentile bootstrap.
#' @param wald_df Normal-reference degrees of freedom (Inf), or the calibrated
#'   missing-information rule for continuous model-implied omega Wald intervals
#'   on the scale of omega (`"ml"`, `"mlr"`; DMAR calibrated it for those
#'   only, so under it DMAR's default interval, and this one, is `"mlr"`).
#' @param scope Whether this record describes a construct or a facet.
#' @param parent_construct,facet Parent construct and facet labels.
#' @return A list of class `sieve_reliability_spec` with `kind`,
#'   `construct`, `items`, `types`, `item_class`, `coefficient`
#'   (`"omega"`, `"omega_robust"`, or `"omega_categorical"`), `label`,
#'   `engine` (the DMAR function), `denominator` and `missing` (as used),
#'   `missing_reason`, `ci_method` (as used; `"none"` when no interval will
#'   be computed), `resampling` (`"dmar"` for DMAR's counted bootstrap of
#'   independent rows, `"siever"` for SIEVER's whole-cluster bootstrap under
#'   a declared cluster design, `NA` otherwise), `uncertainty` (a list
#'   with `status`, one of `"requested"`, `"pending"`, `"not_requested"`,
#'   `"unavailable"`, and `reason`), `interval` (a description of the
#'   method used), `h_se`, `requested` (the request as given),
#'   `conf_level`, `B`, `seed`, `notes`, and `key` (a content hash of the
#'   request, shared by the pending and the run form of the same request).
#' @author Ken Kelley
#' @examples
#' spec <- sieve_reliability_spec(c("x4", "x5", "x6"), rep("continuous", 3),
#'   construct = "Textual", missing = "fiml", ci_method = "mlr_logistic")
#' spec$coefficient
#' spec$interval
#' @export
sieve_reliability_spec <- function(items, types, construct = NA_character_,
                                   missing = c("fiml", "listwise"),
                                   ci_method = c(
                                     "recommended", "mlr_logistic", "mlr",
                                     "ml_logistic", "ml", "adf_logistic",
                                     "likelihood", "percentile",
                                     "dmar_percentile", "dmar_bca",
                                     "bootstrap_se_logistic", "none"
                                   ),
                                   denominator = c("observed", "model_implied"),
                                   conf_level = 0.95, B = 10000L, seed = NULL,
                                   run_bootstrap = TRUE, group_id = NULL,
                                   scope = "construct", parent_construct = construct, facet = NA_character_, aux = NULL, wald_df = Inf, cluster = NULL) {
  denominator_named <- !base::missing(denominator)
  missing <- match.arg(missing)
  ci_method <- match.arg(ci_method)
  denominator <- match.arg(denominator)
  # The default target is robust omega with DMAR's default intervals. A
  # request for a closed form interval that does not name the
  # variant asks for the variant those intervals are derived for, model
  # implied omega, and the label says so.
  if (!denominator_named && ci_method %in% sieve_reliability_closed_methods()) {
    denominator <- "model_implied"
  }
  items <- as.character(items)
  types <- as.character(types)
  if (!length(items) || length(types) != length(items)) {
    stop("`items` and `types` must have the same positive length.", call. = FALSE)
  }
  conf_level <- sieve_check_conf_level(conf_level)
  if (!is.numeric(B) || length(B) != 1L || !is.finite(B) || B < 1 || B != floor(B) || B > .Machine$integer.max) {
    stop("`B` must be a single positive whole number within R's integer range.", call. = FALSE)
  }
  B <- as.integer(B)
  seed <- sieve_seed_value(seed)
  # The engine's percentile bootstrap is counted, so the two percentile
  # codes are one request (see sieve_reliability_methods()).
  if (identical(ci_method, "dmar_percentile")) ci_method <- "percentile"
  request <- list(missing = missing, ci_method = ci_method, denominator = denominator)
  item_class <- sieve_reliability_class(types)
  # "recommended" is resolved here, construct by construct, so a project
  # that mixes continuous and ordered constructs gets each one's own
  # default interval (DMAR's) from one request.
  ci_method <- sieve_reliability_resolve_method(ci_method, item_class, denominator, wald_df)
  bootstrap <- ci_method %in% sieve_reliability_bootstrap_methods()
  notes <- character(0)
  if (identical(wald_df, "missing_information") && !identical(item_class, "categorical") &&
      identical(denominator, "model_implied") &&
      ci_method %in% c("mlr_logistic", "ml_logistic", "adf_logistic")) {
    # DMAR calibrated the FMI t reference for the ML and robust ML limits
    # on the scale of omega only, and refuses it for the logit forms; the
    # logit-scale interval chosen here keeps the normal reference.
    notes <- c(notes, paste0(
      "The FMI t reference applies to the ML and MLR (sandwich) SE intervals on the scale of omega only ",
      "(DMAR's calibration did not pass on the logit scale), so this logit-scale interval uses ",
      "the normal reference."
    ))
  }
  missing_used <- missing
  missing_reason <- NULL
  denominator_used <- denominator

  if (identical(item_class, "categorical")) {
    coefficient <- "omega_categorical"
    engine <- "reliability_omega_categorical"
    missing_used <- "listwise"
    label <- sieve_reliability_label("omega_categorical", denominator = denominator_used)
    if (identical(missing, "fiml")) {
      missing_reason <- paste0(
        "Full information maximum likelihood is not offered for ordered or binary ",
        "items (the engine fits them by weighted least squares on polychoric ",
        "correlations), so complete cases were used for this construct."
      )
    }
  } else {
    coefficient <- if (identical(denominator, "observed")) "omega_robust" else "omega"
    label <- sieve_reliability_label(coefficient)
    engine <- "reliability_omega"
    if (identical(item_class, "mixed")) {
      label <- sieve_reliability_label(coefficient, mixed = TRUE)
      notes <- c(notes, paste0(
        "This construct mixes continuous items with ordered or binary items. The engine ",
        "defines categorical omega only for a composite made entirely of ordered or ",
        "binary items, so omega from the linear one factor model of the item scores ",
        "was computed; the ordered items enter as their numeric codes."
      ))
    }
  }

  if (!is.null(cluster) && !identical(ci_method, "none")) {
    ci_method <- "percentile"
    bootstrap <- TRUE
    notes <- c(notes, "The declared cluster design uses a whole-cluster percentile bootstrap; independent-row analytic and BCa intervals are not used.")
  }
  supported <- sieve_reliability_supported(item_class, missing_used, denominator)
  uncertainty <- list(status = "requested", reason = NULL)
  ci_used <- ci_method
  if (identical(ci_method, "none")) {
    uncertainty <- list(status = "not_requested", reason = "No interval was requested.")
  } else if (!ci_method %in% supported) {
    ci_used <- "none"
    why <- if (identical(item_class, "categorical")) {
      paste0(
        "Categorical omega has no closed form interval in the engine; its interval ",
        "is bootstrap based. Choose a bootstrap interval method."
      )
    } else if (identical(denominator, "observed")) {
      paste0(
        "The closed form and profile likelihood intervals are derived for the model ",
        "implied denominator; the interval for robust omega is bootstrap based. ",
        "Choose a bootstrap interval method."
      )
    } else {
      paste0(
        "The profile likelihood and ADF intervals need complete cases, and the engine does not ",
        "form them under full information maximum likelihood. Choose ML or MLR (sandwich) SE (either ",
        "form), a bootstrap, or listwise deletion."
      )
    }
    uncertainty <- list(status = "unavailable", reason = why)
  } else if (bootstrap && !isTRUE(run_bootstrap)) {
    ci_used <- "none"
    uncertainty <- list(
      status = "pending",
      reason = sprintf(
        "bootstrap not run yet (%s, %d replications); it runs on request, with its wait stated before it starts.",
        sieve_reliability_interval(missing_used, ci_method)$label, B
      )
    )
  }

  spec <- list(
    kind = "reliability",
    construct = as.character(construct),
    group_id = group_id %||% as.character(construct), scope = scope,
    parent_construct = as.character(parent_construct), facet = as.character(facet),
    items = items,
    types = types,
    item_class = item_class,
    coefficient = coefficient,
    label = label,
    engine = engine,
    denominator = denominator_used,
    missing = missing_used,
    cluster = cluster,
    aux = if (identical(missing_used, "fiml")) setdiff(as.character(aux), items) else NULL,
    missing_reason = missing_reason,
    ci_method = ci_used,
    # The method this construct's interval uses once run: the resolved
    # request, kept when a bootstrap is pending (ci_method is then "none").
    ci_target = if (identical(uncertainty$status, "unavailable")) "none" else ci_method,
    # DMAR's counted bootstrap resamples independent rows; a declared
    # cluster design is resampled in whole clusters by SIEVER's own counted
    # bootstrap (fct_reliability_boot.R).
    resampling = if (!ci_used %in% sieve_reliability_bootstrap_methods()) {
      NA_character_
    } else if (!is.null(cluster)) {
      "siever"
    } else {
      "dmar"
    },
    uncertainty = uncertainty,
    interval = sieve_reliability_interval_of(missing_used, ci_used, cluster),
    h_se = if (ci_method %in% c("ml", "ml_logistic")) "normal_theory" else "robust",
    requested = request,
    conf_level = conf_level,
    B = B,
    seed = seed,
    notes = notes
  )
  spec$key <- sieve_hash(list(
    construct = spec$construct, group_id = spec$group_id, scope = scope,
    parent_construct = parent_construct, facet = facet,
    items = items, types = types, requested = request,
    conf_level = conf_level, B = B, seed = seed, aux = spec$aux, cluster = cluster
  ))
  if (!identical(wald_df, Inf)) {
    if (!identical(wald_df, "missing_information")) stop("Use Inf or \"missing_information\" for wald_df.", call. = FALSE)
    spec$wald_df <- wald_df
    spec$key <- sieve_hash(list(spec$key, wald_df = wald_df))
  }
  structure(spec, class = "sieve_reliability_spec")
}

#' What an interval method is, in the engine
#'
#' The standard error and information matrix behind each interval DMAR
#' forms for omega, established by comparison with independently
#' specified lavaan fits (see the package tests): with complete cases the
#' ML interval uses the expected information and the robust ML interval
#' uses lavaan's `robust.sem` standard error (the Satorra and Bentler,
#' 1994, correction); with full information maximum likelihood both use
#' the observed information, the robust one in a Huber-White sandwich (Yuan
#' and Bentler, 2000). The `_logistic` forms use the same standard error
#' and form the limits on the logit scale, carried there by the delta
#' method, SE / (omega (1 - omega)), and transformed back (Browne, 1982;
#' the confidence interval transformation principle, Kelley, 2007).
#'
#' @param missing The missing data treatment used.
#' @param ci_method The interval method used.
#' @return A list with `method`, `label`, `se_type`, `information`, and
#'   `transformation`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_interval <- function(missing, ci_method) {
  fiml <- identical(missing, "fiml")
  wald <- "none (symmetric about the estimate, limits truncated to [0, 1])"
  logit <- "logit (limits formed on the logit scale and transformed back, so they lie inside (0, 1))"
  robust_se <- if (fiml) {
    "Huber-White sandwich (Yuan-Bentler)"
  } else {
    "robust.sem sandwich (Satorra-Bentler)"
  }
  described <- switch(ci_method,
    ml = list(
      label = "ML (delta method)",
      se_type = "normal theory maximum likelihood",
      information = if (fiml) "observed" else "expected",
      transformation = wald
    ),
    ml_logistic = list(
      label = "ML, logit scale (delta method)",
      se_type = "normal theory maximum likelihood",
      information = if (fiml) "observed" else "expected",
      transformation = logit
    ),
    mlr = list(
      label = "MLR (sandwich) SE (delta method)",
      se_type = robust_se,
      information = if (fiml) "observed" else "expected",
      transformation = wald
    ),
    mlr_logistic = list(
      label = "MLR (sandwich) SE, logit-scale limits (delta method)",
      se_type = robust_se,
      information = if (fiml) "observed" else "expected",
      transformation = logit
    ),
    adf_logistic = list(
      label = "ADF, logit scale (delta method)",
      se_type = "asymptotic distribution-free (weighted least squares fit; Browne, 1984)",
      information = "not applicable (ADF weight matrix)",
      transformation = logit
    ),
    likelihood = list(
      label = "Profile likelihood",
      se_type = "none (likelihood ratio inversion)",
      information = "not applicable",
      transformation = "none (limits respect [0, 1])"
    ),
    percentile = ,
    dmar_percentile = list(
      label = "Bootstrap percentile",
      se_type = "standard deviation of the replications used",
      information = "not applicable (resampling in DMAR, counted replications)",
      transformation = "none (limits are percentiles of the replications used, by boot's rule)"
    ),
    dmar_bca = list(
      label = "Bootstrap BCa (DMAR)",
      se_type = "standard deviation of the replications used",
      information = "not applicable (resampling in DMAR, counted replications)",
      transformation = "none (limits are BCa-adjusted percentiles of the replications used)"
    ),
    bootstrap_se_logistic = list(
      label = "Bootstrap standard error, logit scale (DMAR)",
      se_type = "standard deviation of the replications used",
      information = "not applicable (resampling in DMAR, counted replications)",
      transformation = paste0(
        "logit (the bootstrap standard deviation carried to the logit scale by the delta method, ",
        "Kelley & Pornprasertmanit, 2016, Eq. 32; limits transformed back)"
      )
    ),
    cluster_percentile = list(
      label = "Whole-cluster bootstrap percentile",
      se_type = "standard deviation of the usable bootstrap estimates",
      information = "not applicable (whole-cluster resampling in SIEVER, counted replications)",
      transformation = "none (limits are quantiles of the usable replications)"
    ),
    list(
      label = "none", se_type = NA_character_, information = NA_character_,
      transformation = NA_character_
    )
  )
  c(list(method = ci_method), described)
}

#' The interval description of a resolved request
#'
#' As `sieve_reliability_interval()`, with SIEVER's whole-cluster
#' bootstrap described as such when a cluster design is declared (its
#' method code stays `"percentile"`).
#'
#' @param missing,ci_method As for `sieve_reliability_interval()`.
#' @param cluster The declared cluster labels, or `NULL`.
#' @return As `sieve_reliability_interval()`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_interval_of <- function(missing, ci_method, cluster = NULL) {
  out <- sieve_reliability_interval(missing, ci_method)
  if (!is.null(cluster) && ci_method %in% sieve_reliability_bootstrap_methods()) {
    whole <- sieve_reliability_interval(missing, "cluster_percentile")
    out[c("label", "se_type", "information", "transformation")] <- whole[c("label", "se_type", "information", "transformation")]
  }
  out
}

#' The DMAR call a specification resolves to
#'
#' One description of the call, used both to run it and to write the
#' recorded R, so the two cannot differ.
#'
#' A bootstrap call passes `B`, `seed`, `ncpus` (the processes DMAR
#' spreads the replications over; the limits do not depend on it for a
#' given seed, and it is recorded so the call is the one that ran) and, for
#' `reliability_omega()`, `improper = "keep"` (DMAR's default, written out:
#' an improper replication that still yields a coefficient is kept and
#' counted).
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @param ci_method The interval method for this call (a SIEVER code; it is
#'   translated to the engine's name by `sieve_reliability_engine_ci()`).
#' @param seed The bootstrap seed, when the engine runs the bootstrap.
#' @param ncpus For a bootstrap, the processes DMAR uses, or `NULL` to
#'   leave DMAR's default (one).
#' @return A list with `fun` (the DMAR function name) and `args` (its
#'   arguments, without `data`).
#' @author Ken Kelley
#' @noRd
sieve_reliability_call <- function(spec, ci_method = spec$ci_method, seed = NULL, ncpus = NULL) {
  engine_ci <- sieve_reliability_engine_ci(ci_method)
  args <- if (identical(spec$engine, "reliability_omega_categorical")) {
    # The denominator is passed by name, so the recorded call says which
    # one was computed.
    list(
      ci_method = engine_ci, denominator = sieve_reliability_denominator(spec),
      conf_level = spec$conf_level
    )
  } else {
    list(
      ci_method = engine_ci, denominator = spec$denominator,
      missing = spec$missing, conf_level = spec$conf_level
    )
  }
  if (length(spec$aux)) args$aux <- spec$aux
  if (!is.null(spec$wald_df) && identical(spec$engine, "reliability_omega") &&
      identical(spec$denominator, "model_implied") && engine_ci %in% c("ml", "mlr")) args$wald_df <- spec$wald_df
  if (engine_ci %in% c("percentile", "bca", "bootstrap_se_logistic")) {
    args$B <- spec$B
    args$seed <- seed
    if (!is.null(ncpus)) args$ncpus <- as.integer(ncpus)
    if (identical(spec$engine, "reliability_omega")) args$improper <- "keep"
  }
  list(fun = spec$engine, args = args)
}

#' The DMAR function that estimates a coefficient
#'
#' @param fun The name of an exported DMAR function.
#' @return The function.
#' @author Ken Kelley
#' @noRd
sieve_reliability_engine <- function(fun) {
  getExportedValue("DMAR", fun)
}

#' An R literal for a recorded argument value
#'
#' Numbers are written with the fewest digits that read back as the same
#' double, so the recorded call carries the value used, not a rounded one.
#'
#' @param x A character, logical, integer, or double value or vector.
#' @return A length one character string of R code.
#' @author Ken Kelley
#' @noRd
sieve_r_literal <- function(x) {
  if (length(x) != 1L || is.na(x)) {
    return(paste(utils::capture.output(dput(x)), collapse = ""))
  }
  if (is.character(x)) {
    return(encodeString(as.character(x), quote = '"'))
  }
  if (is.logical(x)) {
    return(if (isTRUE(x)) "TRUE" else "FALSE")
  }
  if (is.integer(x)) {
    return(sprintf("%d", x))
  }
  for (digits in 15:17) {
    text <- format(x, digits = digits, scientific = FALSE, trim = TRUE)
    if (identical(as.numeric(text), as.numeric(x))) {
      break
    }
  }
  text
}

#' The R that reproduces a construct's omega
#'
#' Written from the same specification (and the same resolved call) the
#' estimate used, so the recorded script carries the coefficient, the
#' missing data treatment, the interval method, the number of bootstrap
#' replications, and the seed that were actually used.
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @param rng The seed record from `sieve_draw_seed()`, for a bootstrap.
#' @param data_name The name of the item data frame in the script.
#' @param attempted For SIEVER's counted bootstrap, the number of
#'   replications run (fewer than `spec$B` when the run was stopped).
#' @param path For SIEVER's counted bootstrap, how each replication
#'   estimated the coefficient (see `sieve_reliability_boot_path()`).
#' @param ncpus For DMAR's bootstrap, the processes it used.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_reliability_code <- function(spec, rng = NULL, data_name = "items",
                                   attempted = spec$B, path = sieve_reliability_boot_path(spec),
                                   ncpus = NULL) {
  if (identical(spec$ci_method, "percentile") && identical(spec$resampling, "siever")) {
    return(sieve_reliability_boot_code(spec,
      rng = rng, attempted = attempted,
      data_name = data_name, path = path
    ))
  }
  what <- sprintf(
    "# %s: %s, %s%s.", spec$construct, spec$label,
    if (identical(spec$missing, "fiml")) {
      "full information maximum likelihood"
    } else {
      "complete cases"
    },
    if (identical(spec$ci_method, "none")) {
      ", point estimate"
    } else {
      sprintf(", %s interval", spec$interval$label)
    }
  )
  seed_note <- if (!is.null(rng) && !isTRUE(rng$supplied)) {
    "# No seed was supplied; SIEVER drew this one so the bootstrap can be replayed."
  }
  c(
    what,
    seed_note,
    sieve_reliability_call_code(spec, seed = rng$seed, data_name = data_name, ncpus = ncpus)
  )
}

#' The name a recorded reliability result is assigned to
#'
#' The recorded steps assign each result to a named object, so a script
#' run with [source()] keeps every value it computes rather than
#' discarding it.
#'
#' @param prefix What the object holds: `"omega"`, `"omega_model_implied"`,
#'   `"omega_boot"`, `"H"`, or `"alpha"`.
#' @param spec A specification from [sieve_reliability_spec()].
#' @return A syntactic R name, such as `omega_Visual`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_object <- function(prefix, spec) {
  id <- spec$group_id %||% spec$construct
  if (length(id) != 1L || is.na(id) || !nzchar(id)) id <- "group"
  make.names(paste(prefix, id, sep = "_"))
}

#' The DMAR call of a specification, as two lines of R
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @param ci_method The interval method for the call (a SIEVER code).
#' @param seed The bootstrap seed, when the engine runs the bootstrap.
#' @param data_name The name of the item data frame in the script.
#' @param object The name the result is assigned to, or `NULL`.
#' @param ncpus For DMAR's bootstrap, the processes it used.
#' @return A character vector of two R code lines.
#' @author Ken Kelley
#' @noRd
sieve_reliability_call_code <- function(spec, ci_method = spec$ci_method, seed = NULL,
                                        data_name = "items", object = NULL, ncpus = NULL) {
  object <- object %||% sieve_reliability_object(
    if (isTRUE(spec$companion)) "omega_model_implied" else "omega",
    spec
  )
  call <- sieve_reliability_call(spec, ci_method = ci_method, seed = seed, ncpus = ncpus)
  args <- vapply(names(call$args), function(nm) {
    sprintf("%s = %s", nm, sieve_r_literal(call$args[[nm]]))
  }, character(1))
  columns <- sieve_r_literal(unique(c(spec$items, spec$aux)))
  data_name <- sieve_reliability_data_code(spec, data_name)
  c(
    sprintf("%s <- DMAR::%s(data = %s[, %s, drop = FALSE],", object, call$fun, data_name, columns),
    paste0("  ", paste(args, collapse = ", "), ")")
  )
}

#' Prepare one construct's data and estimate its coefficient
#'
#' The part of a reliability analysis every interval method shares: the
#' item data as numbers, the missing data provenance, the seed record when
#' a bootstrap will run, the recorded R, and the engine call that gives the
#' point estimate (or, for a closed form interval, the estimate and its
#' interval in one call). See `sieve_reliability_omega()` for the record
#' this fills in.
#'
#' @param data A data frame holding the item columns.
#' @param spec A specification from [sieve_reliability_spec()].
#' @param rng Optional seed record (see `sieve_reliability_omega()`).
#' @param ci_method The interval method for the first engine call:
#'   `"none"` for the point estimate alone, or a closed form method so one
#'   call serves estimate and interval.
#' @return A list with `out` (the `sieve_reliability` record so far), `X`
#'   (the prepared data), `first` (the captured engine result), `engine` (a
#'   function `engine(ci_method, seed, data)` that runs the resolved call),
#'   `interval_error` (the engine's message when the closed form interval
#'   failed and the estimate was recomputed alone), and `failed` (`TRUE`
#'   when the coefficient could not be estimated; `out` then carries the
#'   reason).
#' @author Ken Kelley
#' @noRd
sieve_reliability_point <- function(data, spec, rng = NULL, ci_method = "none") {
  bootstrap <- spec$ci_method %in% sieve_reliability_bootstrap_methods()
  out <- list(
    spec = spec, construct = spec$construct, k = length(spec$items),
    coefficient = spec$coefficient, label = spec$label,
    estimate = NA_real_, se = NA_real_, lower = NA_real_, upper = NA_real_,
    conf_level = spec$conf_level,
    status = list(code = "failed", message = "Not computed."),
    uncertainty = c(
      list(status = spec$uncertainty$status, requested = spec$ci_target %||% spec$requested$ci_method),
      spec$interval,
      list(
        reason = spec$uncertainty$reason,
        B = if (bootstrap) spec$B else NA_integer_,
        B_attempted = NA_integer_, B_used = NA_integer_,
        mc_se = NA_real_, rng = NULL, warnings = character(0),
        seconds = NA_real_
      )
    ),
    missing = list(
      requested = spec$requested$missing, used = spec$missing,
      reason = spec$missing_reason
    ),
    n = list(
      rows = NA_integer_, observed = NA_integer_, empty = NA_integer_,
      complete = NA_integer_, patterns = NA_integer_, used = NA_integer_
    ),
    warnings = character(0), messages = character(0), error = NULL,
    seconds = NA_real_, code = character(0),
    versions = sieve_reliability_versions()
  )
  class(out) <- "sieve_reliability"
  result <- list(
    out = out, X = NULL, first = NULL, engine = NULL,
    interval_error = NULL, failed = TRUE
  )
  fail <- function(why) {
    if (grepl("composite", why, ignore.case = TRUE) && grepl("does not vary", why, fixed = TRUE)) {
      why <- sprintf("Composite from %s: %s", paste(spec$items, collapse = ", "), why)
    }
    out$error <- why
    out$status <- list(code = "failed", message = why)
    if (identical(out$uncertainty$status, "requested")) {
      out$uncertainty$status <- "failed"
      out$uncertainty$reason <- "The coefficient could not be estimated, so no interval was formed."
    }
    result$out <- out
    result
  }

  prepared <- sieve_capture(sieve_reliability_data(data, unique(c(spec$items, spec$aux))))
  if (!prepared$ok) {
    return(fail(prepared$error))
  }
  X <- prepared$value
  out$n[c("rows", "observed", "empty", "complete", "patterns")] <- sieve_reliability_counts(X[, spec$items, drop = FALSE])
  distinct <- vapply(X[, spec$items, drop = FALSE], function(x) length(unique(x[!is.na(x)])), integer(1L))
  empty_items <- spec$items[distinct == 0L]
  if (length(empty_items)) return(fail(sprintf(
    "These included reliability items have no observed responses: %s. Review or exclude them in Ingest.",
    paste(empty_items, collapse = ", "))))
  constant_items <- spec$items[distinct == 1L]
  if (length(constant_items)) return(fail(sprintf(
    "These included reliability items do not vary: %s. Review or exclude them in Ingest.",
    paste(constant_items, collapse = ", "))))
  if (!is.null(out$missing$reason)) {
    # Say what the substituted treatment cost in rows, so its consequence is
    # visible and not only its name.
    set_aside <- out$n$observed - out$n$complete
    out$missing$reason <- paste(out$missing$reason, if (set_aside > 0L) {
      sprintf(
        "%d of the %d rows with an observed item are incomplete and were set aside.",
        set_aside, out$n$observed
      )
    } else {
      "No row is incomplete on these items, so no respondent was set aside."
    })
  }
  if (length(spec$items) < 3L) {
    return(fail(if (length(spec$items) < 2L) {
      "Omega needs at least three included items in this unconstrained one-factor model. One item supplies no internal-consistency evidence."
    } else {
      "Omega needs three or more items: the unconstrained one-factor model is not identified with two."
    }))
  }
  if (bootstrap) {
    if (is.null(rng)) {
      rng <- sieve_draw_seed(spec$seed)
    }
    out$uncertainty$rng <- rng
  }
  out$code <- sieve_reliability_code(spec, rng = rng)

  engine <- function(ci_method, seed = NULL, data = X) {
    call <- sieve_reliability_call(spec, ci_method = ci_method, seed = seed)
    do.call(sieve_reliability_engine(call$fun), c(list(data = data), call$args))
  }
  # When the interval comes from resampling, the coefficient is estimated on
  # its own first, so what the engine says about the estimate (an improper
  # solution, for example) is kept apart from what it says while refitting
  # the resamples. A closed form interval comes from the same fit as the
  # estimate, so one call serves both.
  started <- proc.time()[["elapsed"]]
  first <- sieve_capture_quiet(engine(ci_method))
  interval_error <- NULL
  if (!identical(ci_method, "none") && !first$ok) {
    # The interval could not be formed. Keep the engine's reason and still
    # report the coefficient when it can be estimated on its own.
    interval_error <- first$error
    out$uncertainty$warnings <- first$warnings
    first <- sieve_capture_quiet(engine("none"))
  }
  out$seconds <- proc.time()[["elapsed"]] - started
  out$warnings <- first$warnings
  out$messages <- first$messages
  if (!first$ok) {
    return(fail(first$error))
  }
  out$estimate <- sieve_reliability_value(first$value, "estimate")
  out$n$used <- as.integer(sieve_reliability_value(first$value, "N"))
  # DMAR attaches its summary of the single-factor residual correlations
  # to the result when it forms one; SIEVER computes them itself otherwise
  # (see sieve_reliability_residuals()).
  out$engine_residual_cor <- attr(first$value, "residual_cor", exact = TRUE)
  if (!is.finite(out$estimate)) {
    return(fail("The engine returned no estimate."))
  }
  improper <- c(
    sieve_reliability_improper(first$warnings),
    sieve_reliability_outside(out$estimate, spec$coefficient, label = spec$label)
  )
  out$status <- if (length(improper)) {
    list(code = "inadmissible", message = paste(improper, collapse = " "))
  } else {
    list(code = "ok", message = "Estimated.")
  }
  if (identical(spec$ci_target, "percentile") && !is.null(spec$cluster)) {
    # SIEVER's whole-cluster counted bootstrap is requested (or pending):
    # how each replication will be estimated, and how long one takes, for
    # the wait stated before the run.
    path <- sieve_reliability_boot_path_check(spec, X, out$estimate, out$seconds)
    out$uncertainty$boot_path <- path$path
    out$uncertainty$boot_path_reason <- path$reason
    out$uncertainty$seconds_per_draw <- path$seconds
  }
  result$out <- out
  result$X <- X
  result$first <- first
  result$engine <- engine
  result$interval_error <- interval_error
  result$failed <- FALSE
  result
}

#' One term of an engine result
#'
#' @param result A DMAR reliability result (a data frame with `term` and
#'   `value`).
#' @param term The term wanted.
#' @return Its value, or `NA`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_value <- function(result, term) {
  hit <- result$value[result$term == term]
  if (length(hit)) as.numeric(hit[1]) else NA_real_
}

#' The engine's warnings that announce an improper solution
#'
#' @param warnings Warnings captured from an engine call.
#' @return The warnings concerned (possibly none).
#' @author Ken Kelley
#' @noRd
sieve_reliability_improper <- function(warnings) {
  warnings[grepl("Heywood|[Ii]mproper|variances are negative", warnings)]
}

#' Why a reliability estimate outside the unit interval is inadmissible
#'
#' A reliability coefficient is a proportion of variance, so an estimate
#' below 0 or above 1 is not a reliability. Omega with the observed
#' denominator (robust omega) and categorical omega divide variance implied
#' by the one factor model (the numerator) by the variance of the composite
#' estimated from the data (the denominator, from the sample covariances,
#' or from the polychoric correlations and the thresholds). When the model
#' fits poorly the numerator can exceed the denominator: the model implies
#' more common variance than the observed composite has. The estimate is
#' then inadmissible, and an interval from resamples that happen to fall in
#' the unit interval would not describe it.
#'
#' @param estimate The point estimate.
#' @param coefficient The coefficient code (`"omega"`, `"omega_robust"`,
#'   `"omega_categorical"`).
#' @param label The coefficient's label (see `sieve_reliability_label()`),
#'   or `NULL` for the label of `coefficient` with its default denominator.
#' @return A plain sentence, or `character(0)` when the estimate is in
#'   `[0, 1]` (or not finite; a missing estimate is a failure, not this).
#' @author Ken Kelley
#' @noRd
sieve_reliability_outside <- function(estimate, coefficient = "omega", label = NULL) {
  if (length(estimate) != 1L || !is.finite(estimate) || (estimate >= 0 && estimate <= 1)) {
    return(character(0))
  }
  what <- label %||% sieve_reliability_label(coefficient %||% "omega")
  if (estimate > 1) {
    sprintf(paste0(
      "The estimate, %s = %s, is above 1, so it is not a reliability and is inadmissible: ",
      "the one-factor model implies more common variance than the observed composite has, ",
      "which happens when that model does not describe these items. No interval is formed ",
      "for it; examine the one-factor model's fit and the item content."
    ), what, format(estimate, digits = 6))
  } else {
    sprintf(paste0(
      "The estimate, %s = %s, is below 0, so it is not a reliability and is inadmissible: ",
      "the loadings of the one-factor model largely cancel in the unit-weighted composite ",
      "(an item may need reverse scoring). No interval is formed for it."
    ), what, format(estimate, digits = 6))
  }
}

#' Estimate one construct's omega and report what happened
#'
#' Runs the DMAR call the specification resolves to on the raw item data,
#' so the missing data pattern stays with the analysis: under full
#' information maximum likelihood a row contributes exactly what its
#' observed items hold, and a row with no observed item contributes
#' nothing. The result is a record rather than a bare number. It separates
#' the status of the estimate from the status of its uncertainty:
#'
#' * `status$code`: `"ok"`, `"inadmissible"` (the engine reported a
#'   negative error variance; the coefficient is returned with that
#'   caution), or `"failed"` (with the engine's message);
#' * `uncertainty$status`: `"completed"`, `"failed"` (the interval was
#'   attempted and could not be formed; the reason is kept),
#'   `"unavailable"` (the requested method does not apply to this
#'   construct; the reason is kept), `"pending"` (a bootstrap is selected
#'   and has not been run), or `"not_requested"`.
#'
#' A bootstrap interval is DMAR's counted bootstrap, one DMAR call (see
#' `sieve_reliability_boot_call()` and `sieve_reliability_boot_complete()`),
#' run here in this process; the Reliability tab runs the same call in a
#' background process (fct_dmar_boot.R). Under a declared cluster design
#' SIEVER's whole-cluster counted bootstrap is run instead, through
#' `sieve_reliability_boot_plan()`, `sieve_reliability_boot_replicate()`,
#' and `sieve_reliability_boot_result()`. The closed forms are one engine
#' call.
#'
#' @param data A data frame holding the item columns.
#' @param spec A specification from [sieve_reliability_spec()].
#' @param rng Optional seed record from `sieve_draw_seed()`, so several
#'   constructs can share one recorded seed. When `NULL` and a bootstrap
#'   will run, the seed is resolved from `spec$seed` (drawn from the
#'   caller's stream when that is `NULL`).
#' @param progress For a bootstrap, an optional function called as it runs,
#'   `progress(done, total)` (DMAR's hook; SIEVER's whole-cluster bootstrap
#'   adds the replication's record as a third argument). Returning `FALSE`
#'   stops the run: DMAR then forms no interval (the record says it was
#'   stopped), and the whole-cluster bootstrap summarizes the replications
#'   completed.
#' @param workers For a bootstrap, the processes to spread the replications
#'   over: DMAR's `ncpus` (`NULL` for one), or the socket workers of the
#'   whole-cluster bootstrap (`NULL` to decide from the wait, see
#'   `sieve_reliability_boot_workers()`). The result does not depend on it.
#' @return A list of class `sieve_reliability` with `spec`, `construct`,
#'   `k`, `coefficient`, `label`, `estimate`, `se`, `lower`, `upper`,
#'   `conf_level` (all at full precision), `status`, `uncertainty` (with
#'   `status`, `requested`, `method`, `label`, `reason`, `se_type`,
#'   `information`, `transformation`, `B`, `B_attempted`, `B_used`,
#'   `mc_se`, `rng`, `warnings` (conditions raised while forming the
#'   interval, apart from those of the estimate), `seconds`, and, for the
#'   counted bootstrap, `counts`, `draws`, `reasons`, `unusable_share`,
#'   `caution_share`, `caution`, `complete`, `rule`, `rows`, and
#'   `limit_range`), `missing` (`requested`, `used`, `reason`), `n`
#'   (`rows`, `observed`, `empty`, `complete`, `patterns`, `used`),
#'   `warnings`, `messages`, `error`, `seconds`, `code`, and `versions`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_omega <- function(data, spec, rng = NULL, progress = NULL, workers = NULL) {
  bootstrap <- spec$ci_method %in% sieve_reliability_bootstrap_methods()
  if (bootstrap && identical(spec$resampling, "siever")) {
    plan <- sieve_reliability_boot_plan(data, spec, rng = rng, workers = workers)
    if (!identical(plan$status$code, "ready")) {
      return(plan$result)
    }
    on.exit(sieve_reliability_boot_release(plan), add = TRUE)
    started <- proc.time()[["elapsed"]]
    draws <- vector("list", plan$B)
    done <- 0L
    stopped <- FALSE
    # Batches of replications (one at a time in this process, several per
    # worker with workers); progress is still reported replication by
    # replication, and a stop keeps exactly the replications before it.
    while (done < plan$B && !stopped) {
      b <- sieve_reliability_boot_next(plan, done)
      got <- sieve_reliability_boot_draws(plan, b)
      for (j in seq_along(b)) {
        draws[[b[j]]] <- got[[j]]
        done <- b[j]
        if (is.function(progress) && identical(progress(b[j], plan$B, got[[j]]), FALSE)) {
          stopped <- TRUE
          break
        }
      }
    }
    return(sieve_reliability_boot_result(plan, draws[seq_len(done)],
      seconds = proc.time()[["elapsed"]] - started
    ))
  }
  p <- sieve_reliability_point(data, spec,
    rng = rng,
    ci_method = if (bootstrap) "none" else spec$ci_method
  )
  out <- p$out
  if (p$failed || !identical(out$uncertainty$status, "requested")) {
    return(out)
  }
  if (bootstrap && !identical(out$status$code, "ok")) {
    # The engine's bootstraps are refused for the reason SIEVER's counted
    # bootstrap is (sieve_reliability_boot_plan()): resamples of an improper
    # or inadmissible solution do not describe it.
    out$uncertainty$status <- "unavailable"
    out$uncertainty$reason <- sieve_reliability_refusal(out$status$message)
    return(out)
  }
  source <- p$first$value
  interval_error <- p$interval_error
  if (bootstrap) {
    request <- sieve_dmar_boot_request(spec$B,
      method = if (identical(spec$ci_method, "dmar_bca")) "bca" else
        if (identical(spec$ci_method, "bootstrap_se_logistic")) "se" else "percentile",
      conf_level = spec$conf_level)
    if (!request$ok) {
      out$uncertainty$status <- "unavailable"
      out$uncertainty$reason <- request$message
      return(out)
    }
    # DMAR's counted bootstrap: one call, with the seed of the run, its
    # replications on `workers` processes (one by default), and SIEVER's
    # progress function passed through as DMAR's hook.
    call <- sieve_reliability_boot_call(spec, out$uncertainty$rng, p$X,
      ncpus = sieve_dmar_boot_ncpus(workers %||% 1L)
    )
    hook <- if (is.function(progress)) function(done, total) progress(done, total)
    run <- sieve_dmar_boot_run(call, progress = hook, fun = sieve_reliability_engine(call$fun))
    return(sieve_reliability_boot_complete(out, run, point_warnings = p$first$warnings))
  }
  if (!is.null(interval_error)) {
    out$uncertainty$status <- "failed"
    out$uncertainty$reason <- interval_error
    # The recorded R says what the session saw: the call that was made, that
    # it did not form the interval, why, and the call that gave the estimate.
    out$code <- c(
      out$code,
      sprintf(
        "# In the session this call did not form the %s interval: %s",
        out$uncertainty$label, gsub("\\s+", " ", interval_error)
      ),
      "# The coefficient was then estimated without an interval:",
      sieve_reliability_call_code(spec, ci_method = "none")
    )
    return(out)
  }
  out <- sieve_reliability_engine_metadata(out, source)
  out$se <- sieve_reliability_value(source, "se")
  out$lower <- sieve_reliability_value(source, "ci_lower")
  out$upper <- sieve_reliability_value(source, "ci_upper")
  if (is.finite(out$lower) && is.finite(out$upper)) {
    out$uncertainty$status <- "completed"
  } else {
    out$uncertainty$status <- "failed"
    conditions <- c(out$uncertainty$warnings, p$first$warnings)
    out$uncertainty$reason <- if (length(conditions)) {
      paste(conditions, collapse = " ")
    } else {
      "The engine returned no interval limits."
    }
  }
  out
}

#' Preserve the engine's actual uncertainty contract
#' @param out A SIEVER reliability record.
#' @param source The returned DMAR table and its attributes.
#' @return The record with the actual method and effective resample counts.
#' @author Ken Kelley
#' @noRd
sieve_reliability_engine_metadata <- function(out, source) {
  sieve_dmar_require_terms(source, c("estimate", "se", "ci_lower", "ci_upper"), "Reliability")
  actual <- attr(source, "ci_method", exact = TRUE)
  counts <- attr(source, "bootstrap", exact = TRUE)
  out$engine_attributes <- attributes(source)[setdiff(names(attributes(source)), c("names", "row.names", "class"))]
  if (is.character(actual) && length(actual) == 1L && !is.na(actual)) {
    out$uncertainty$engine_method <- actual
    canonical <- sub("[ (].*$", "", actual)
    method <- switch(canonical,
      percentile = "percentile",
      bca = "dmar_bca",
      canonical
    )
    description <- sieve_reliability_interval(out$spec$missing, method)
    out$uncertainty$method <- actual
    out$uncertainty$label <- description$label
    if (!identical(actual, canonical)) out$uncertainty$method_reason <- actual
    out$uncertainty$transformation <- description$transformation
    out$uncertainty$se_type <- description$se_type
    out$uncertainty$information <- description$information
  }
  if (is.list(counts)) {
    out$uncertainty$engine_bootstrap <- counts
    out$uncertainty$B_attempted <- counts$attempted %||% NA_integer_
    out$uncertainty$B_used <- counts$returned %||% NA_integer_
  }
  df <- attr(source, "wald_df", exact = TRUE)
  if (length(df) == 1L && is.finite(df)) {
    out$uncertainty$wald_df <- df
    out$uncertainty$wald_df_table <- attr(source, "wald_df_table", exact = TRUE)
    out$uncertainty$label <- sprintf("%s, t reference (df = %.2f; simulation-calibrated missing-information rule; coverage under DMAR's eligibility diagnostic not established)", out$uncertainty$label, df)
  }
  out
}

#' The DMAR bootstrap call of one construct
#'
#' The call DMAR's counted bootstrap is run with, in this process or in the
#' Reliability tab's background process, and recorded: the specification's
#' DMAR function and arguments (see `sieve_reliability_call()`), the seed of
#' the run, the processes, and the prepared item data.
#'
#' @param spec A specification whose `ci_method` is a bootstrap.
#' @param rng The seed record of the run (see `sieve_draw_seed()`).
#' @param X The construct's prepared item data (see
#'   `sieve_reliability_data()`).
#' @param ncpus Processes DMAR spreads the replications over.
#' @return A list with `fun` and `args` (the data first), as
#'   `sieve_dmar_boot_run()` takes it.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_call <- function(spec, rng, X, ncpus = 1L) {
  request <- sieve_dmar_boot_request(spec$B,
    method = if (identical(spec$ci_method, "dmar_bca")) "bca" else
      if (identical(spec$ci_method, "bootstrap_se_logistic")) "se" else "percentile",
    conf_level = spec$conf_level)
  if (!request$ok) stop(request$message, call. = FALSE)
  call <- sieve_reliability_call(spec, ci_method = spec$ci_method, seed = rng$seed, ncpus = ncpus)
  cols <- intersect(unique(c(spec$items, spec$aux)), names(X))
  list(fun = call$fun, args = c(list(data = X[, cols, drop = FALSE]), call$args))
}

#' Complete a construct's record from DMAR's counted bootstrap
#'
#' Reads the DMAR call's result into the `sieve_reliability` record of the
#' point estimate: the limits and standard error at full precision, the
#' actual interval method (a BCa interval DMAR could not form is reported as
#' its percentile fallback), every replication's fate in SIEVER's count
#' vocabulary (see `sieve_dmar_boot_counts()`), DMAR's caution, its rule
#' (the half rule and the improper policy, see `sieve_dmar_boot_rule()`),
#' the Monte Carlo error of a percentile limit, how far each limit could
#' move had the replications set aside produced values, and the recorded
#' DMAR call with the processes it used. A percentile interval is shown
#' only from at least `sieve_bootstrap_min_draws()` used replications (39 at
#' 95 percent), as with SIEVER's own counted bootstrap: with fewer, boot's
#' limits are the extreme order statistics. When the call returned no
#' interval, the reason is kept: a stopped run (class
#' `dmar_bootstrap_cancelled`), one of DMAR's refusals (class
#' `dmar_refusal`), or another error, each read by class.
#'
#' @param out The `sieve_reliability` record of the point estimate, its
#'   uncertainty `"requested"` with the seed record of the run.
#' @param run A result of `sieve_dmar_boot_run()`.
#' @param point_warnings Warnings of the point estimate, not repeated as
#'   warnings of the interval.
#' @return The completed record (see `sieve_reliability_omega()`).
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_complete <- function(out, run, point_warnings = character(0)) {
  spec <- out$spec
  u <- out$uncertainty
  u$seconds <- run$seconds
  u$warnings <- setdiff(run$warnings, point_warnings)
  u$messages <- setdiff(run$messages %||% character(0), out$messages %||% character(0))
  u$rule <- sieve_dmar_boot_rule("reliability", spec$conf_level)
  u$rows <- if (identical(spec$missing, "fiml")) "observed" else "complete"
  outcome <- sieve_dmar_boot_outcome(run)
  if (!identical(outcome, "ok")) {
    u$status <- "failed"
    # An error of the analysis is kept in DMAR's words; a stopped run, a
    # failing progress display and DMAR's refusals (read by class) say so.
    u$reason <- if (identical(outcome, "error")) gsub("\\s+", " ", run$error) else sieve_dmar_boot_failure_text(run)
    u$condition_class <- run$error_class
    u$stopped <- identical(outcome, "cancelled")
    out$uncertainty <- u
    out$code <- c(
      out$code,
      sprintf(
        "# In the session this call did not form the %s interval: %s",
        u$label, gsub("\\s+", " ", u$reason)
      ),
      "# The coefficient was then estimated without an interval:",
      sieve_reliability_call_code(spec, ci_method = "none")
    )
    return(out)
  }
  source <- run$value
  out$uncertainty <- u
  out <- sieve_reliability_engine_metadata(out, source)
  u <- out$uncertainty
  # The bootstrap call reports the same estimate as the point call.
  estimate <- sieve_reliability_value(source, "estimate")
  if (!isTRUE(all.equal(estimate, out$estimate, tolerance = 1e-10))) {
    u$warnings <- c(u$warnings, sprintf(
      "The bootstrap call's estimate (%s) differs from the point estimate (%s).",
      format(estimate, digits = 15), format(out$estimate, digits = 15)
    ))
  }
  out$se <- sieve_reliability_value(source, "se")
  out$lower <- sieve_reliability_value(source, "ci_lower")
  out$upper <- sieve_reliability_value(source, "ci_upper")
  bs <- attr(source, "bootstrap", exact = TRUE)
  ncpus <- as.integer(bs$ncpus %||% 1L)
  u$ncpus <- ncpus
  if (is.list(bs) && length(bs$status)) {
    counted <- sieve_dmar_boot_counts(bs)
    u$counts <- counted$counts
    u$counts$requested <- spec$B
    u$draws <- counted$draws
    u$reasons <- counted$reasons
    u$B_attempted <- u$counts$attempted
    u$B_used <- u$counts$used
    u$unusable_share <- if (u$counts$attempted) (u$counts$attempted - u$counts$used) / u$counts$attempted else NA_real_
    u$caution_share <- sieve_bootstrap_caution_share(spec$conf_level)
    u$caution <- isTRUE(bs$caution)
    u$half_rule <- isTRUE(bs$require_half)
  }
  u$complete <- TRUE
  percentile <- isTRUE(grepl("^percentile($|[ (])", u$engine_method %||% ""))
  min_used <- sieve_bootstrap_min_draws(spec$conf_level)
  if (percentile && is.finite(out$lower) && is.finite(out$upper) && is.finite(u$B_used) && u$B_used < min_used) {
    # Too few replications for a percentile limit inside the sample: boot
    # places the limit below the first order statistic and returns the
    # extreme value (with a warning). SIEVER shows no interval then, as its
    # own counted bootstrap does; DMAR's limits are kept in the record.
    u$engine_limits <- c(lower = out$lower, upper = out$upper)
    out$lower <- NA_real_
    out$upper <- NA_real_
    u$status <- "failed"
    u$reason <- paste(
      if (identical(u$B_used, u$B_attempted)) {
        sprintf("Only %d replications were run.", u$B_used)
      } else {
        sprintf("Only %d of the %d replications attempted could be used.", u$B_used, u$B_attempted)
      },
      sprintf(paste0(
        "A %s percent percentile interval needs at least %d usable replications; with fewer, its ",
        "limits could only repeat the smallest and the largest value (boot's rule places them below ",
        "the first order statistic), so none is shown."
      ), format(spec$conf_level * 100), min_used)
    )
    out$uncertainty <- u
    out$code <- sieve_reliability_code(spec, rng = u$rng, ncpus = ncpus)
    return(out)
  }
  if (is.finite(out$lower) && is.finite(out$upper)) {
    u$status <- "completed"
    u$reason <- NULL
    if (percentile && is.finite(u$B_used) && u$B_used >= 2L) {
      u$mc_se <- sieve_bootstrap_mc_error(u$B_used, spec$conf_level) * out$se
      if (!is.null(u$draws)) {
        u$limit_range <- sieve_dmar_percentile_range(u$draws$estimate[u$draws$used], u$counts$attempted,
          spec$conf_level)
      }
    }
  } else {
    u$status <- "failed"
    used <- u$B_used %||% NA_integer_
    attempted <- u$B_attempted %||% NA_integer_
    u$reason <- paste(c(
      if (isTRUE(u$half_rule) && is.finite(used) && is.finite(attempted) && used < attempted / 2) {
        sprintf(paste0(
          "Only %d of the %d replications returned a value, fewer than half, so DMAR forms no ",
          "interval (its half rule): the replications that returned one are a minority chosen by ",
          "having returned it, and their spread does not describe the sampling distribution."
        ), used, attempted)
      } else {
        "DMAR returned no interval limits."
      },
      if (length(u$warnings)) paste("DMAR warned:", paste(u$warnings, collapse = " "))
    ), collapse = " ")
  }
  out$uncertainty <- u
  out$code <- sieve_reliability_code(spec, rng = u$rng, ncpus = ncpus)
  out
}

#' The acceptance rule of the counted reliability bootstrap, in words
#'
#' One rule, fixed before any resample is drawn, applied to every
#' replication and to the point estimate.
#'
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_rule <- function() {
  paste0(
    "A replication is used when the engine returns a finite converged estimate. ",
    "Improper replications are kept and counted separately, following DMAR's default; ",
    "the interval does not condition on admissibility."
  )
}

#' Why no bootstrap is run for an improper or inadmissible estimate
#'
#' @param message The status message of the point estimate.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_reliability_refusal <- function(message) {
  paste(
    message,
    "The bootstrap was not run: the rule every replication must pass applies to the",
    "estimate too, and an interval from admissible replications would not describe this",
    "solution."
  )
}

#' Plan SIEVER's counted bootstrap of one construct's coefficient
#'
#' Estimates the coefficient on the full data through
#' `sieve_reliability_point()`, fixes the pool of rows to resample (the
#' rows that estimate used: every row with an observed item under full
#' information maximum likelihood, the complete rows otherwise, checked
#' against the row count the engine reports), the acceptance rule, and one
#' seed per replication. Nothing is resampled here. The plan is
#' `"unavailable"`, with the reason in `result`, when the coefficient could
#' not be estimated, when the estimate itself fails the acceptance rule (an
#' interval from admissible replications would not describe an improper
#' solution), or when the pool is not the rows the estimate used.
#'
#' @param data A data frame holding the item columns.
#' @param spec A specification from [sieve_reliability_spec()] whose
#'   `ci_method` is `"percentile"`.
#' @param rng Optional seed record (see `sieve_reliability_omega()`).
#' @param workers The number of socket workers to spread the replications
#'   over, or `NULL` to decide from the wait (see
#'   `sieve_reliability_boot_workers()`).
#' @return A list of class `sieve_reliability_boot_plan` with `status`
#'   (`code`: `"ready"` or `"unavailable"`; `message`), `result` (the
#'   `sieve_reliability` record of the point estimate, with the refusal
#'   recorded when the plan is unavailable), `spec`, `pool`, `rows`
#'   (`"observed"` or `"complete"`), `n`, `B`, `conf_level`, `rng`,
#'   `draw_seeds`, `seconds_per_fit`, `seconds_per_draw`, `path` (see
#'   `sieve_reliability_boot_path()`), `task` (see
#'   `sieve_reliability_boot_task()`), `crew` (see
#'   `sieve_reliability_boot_crew()`), `rule`, `min_used`, `caution_share`,
#'   `engine`, and `key`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_plan <- function(data, spec, rng = NULL, workers = NULL) {
  if (!identical(spec$ci_method, "percentile")) {
    stop("The counted bootstrap plans only a specification whose ci_method is \"percentile\".",
      call. = FALSE
    )
  }
  p <- sieve_reliability_point(data, spec, rng = rng, ci_method = "none")
  plan <- list(
    kind = "reliability_bootstrap",
    status = list(code = "unavailable", message = "Not planned."),
    result = p$out, spec = spec, pool = NULL, rows = NA_character_, n = p$out$n,
    B = spec$B, conf_level = spec$conf_level, rng = p$out$uncertainty$rng,
    draw_seeds = integer(0), seconds_per_fit = p$out$seconds,
    seconds_per_draw = p$out$uncertainty$seconds_per_draw %||% p$out$seconds,
    path = p$out$uncertainty$boot_path %||% "raw", task = NULL,
    crew = sieve_reliability_boot_crew(1L),
    rule = sieve_reliability_boot_rule(),
    min_used = sieve_bootstrap_min_draws(spec$conf_level),
    caution_share = sieve_bootstrap_caution_share(spec$conf_level),
    engine = p$engine, key = spec$key
  )
  class(plan) <- "sieve_reliability_boot_plan"
  unavailable <- function(why) {
    plan$status <- list(code = "unavailable", message = why)
    plan$result$uncertainty$status <- "unavailable"
    plan$result$uncertainty$reason <- why
    plan$result$uncertainty$rule <- plan$rule
    plan$result$code <- c(sieve_reliability_call_code(spec, ci_method = "none"), paste0("# No bootstrap: ", why))
    plan
  }
  if (p$failed) {
    plan$status <- list(code = "unavailable", message = p$out$status$message)
    return(plan)
  }
  if (!identical(p$out$status$code, "ok")) {
    return(unavailable(sieve_reliability_refusal(p$out$status$message)))
  }
  X <- p$X
  seen <- rowSums(!is.na(as.matrix(X[, spec$items, drop = FALSE])))
  listwise <- !identical(spec$missing, "fiml")
  keep <- if (listwise) seen == length(spec$items) else rowSums(!is.na(as.matrix(X))) > 0L
  pool <- X[keep, , drop = FALSE]
  rownames(pool) <- NULL
  if (nrow(pool) != p$out$n$used) {
    return(unavailable(sprintf(paste0(
      "The rows the bootstrap would resample (%d) are not the rows the estimate used (%d), ",
      "so the two would not describe the same sample."
    ), nrow(pool), p$out$n$used)))
  }
  plan$pool <- pool
  if (!is.null(spec$cluster) && length(spec$cluster) != nrow(data)) return(unavailable("Cluster labels must match every supplied data row before item-specific case selection."))
  partition <- sieve_capture(sieve_bootstrap_partition(nrow(pool),
    if (!is.null(spec$cluster)) spec$cluster[keep], which(keep)))
  if (!partition$ok) return(unavailable(partition$error))
  plan$sampling <- partition$value
  plan$result$warnings <- c(plan$result$warnings, plan$sampling$caution)
  plan$rows <- if (listwise) "complete" else "observed"
  seeded <- sieve_with_seed(plan$rng$seed, sample.int(.Machine$integer.max, spec$B))
  plan$draw_seeds <- seeded$value
  plan$task <- sieve_reliability_boot_task(spec, pool, plan$draw_seeds, plan$path, plan$sampling)
  plan$crew <- sieve_reliability_boot_crew(
    workers %||% sieve_reliability_boot_workers(plan$seconds_per_draw * spec$B)
  )
  plan$status <- list(code = "ready", message = "Ready.")
  plan
}
#' Summarize the counted reliability bootstrap
#'
#' Forms the percentile interval from the replications that passed the
#' acceptance rule and reports what happened to all of them. The counts
#' nest through convergence; admissible and used are separate counts because
#' a defined improper coefficient is retained. The
#' interval is formed only from at least `sieve_bootstrap_min_draws()`
#' usable replications; with fewer the uncertainty status is `"failed"`
#' with the reason. The limits condition on the fit having succeeded, so
#' `caution` is raised when the unusable share reaches the tail area (see
#' `sieve_bootstrap_caution_share()`) and `limit_range` bounds each limit
#' over where the unusable replications could have fallen (see
#' `sieve_percentile_limits()`).
#'
#' @param plan A plan from `sieve_reliability_boot_plan()`.
#' @param draws A list of replications from `sieve_reliability_boot_replicate()`:
#'   the first so many of the plan.
#' @param seconds Time the replications took.
#' @return The `sieve_reliability` record of the plan, completed (see
#'   `sieve_reliability_omega()`).
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_result <- function(plan, draws = list(), seconds = NA_real_) {
  # The run is over (completed or stopped): its workers are no longer needed.
  sieve_reliability_boot_release(plan)
  out <- plan$result
  if (!identical(plan$status$code, "ready")) {
    return(out)
  }
  attempted <- length(draws)
  outcomes <- c("used", "failed", "not_converged", "inadmissible")
  flag <- function(name) vapply(draws, function(d) isTRUE(d[[name]]), logical(1))
  outcome <- vapply(draws, function(d) d$outcome, character(1))
  frame <- data.frame(
    draw = vapply(draws, function(d) d$draw, integer(1)),
    seed = vapply(draws, function(d) d$seed, integer(1)),
    outcome = outcome,
    returned = flag("returned"), converged = flag("converged"),
    admissible = flag("admissible"), used = flag("used"),
    estimate = vapply(draws, function(d) d$estimate, numeric(1)),
    message = vapply(draws, function(d) d$message, character(1)),
    stringsAsFactors = FALSE
  )
  used <- sum(frame$used)
  unusable <- frame[!frame$used, , drop = FALSE]
  reasons <- if (nrow(unusable)) {
    tally <- as.data.frame(table(outcome = unusable$outcome, message = unusable$message),
      stringsAsFactors = FALSE
    )
    tally <- tally[tally$Freq > 0L, , drop = FALSE]
    names(tally)[3] <- "draws"
    tally <- tally[order(-tally$draws, tally$outcome, tally$message), , drop = FALSE]
    rownames(tally) <- NULL
    tally
  } else {
    data.frame(
      outcome = character(0), message = character(0), draws = integer(0),
      stringsAsFactors = FALSE
    )
  }
  u <- out$uncertainty
  u$B_attempted <- attempted
  u$B_used <- used
  u$seconds <- seconds
  u$counts <- list(
    requested = plan$B, attempted = attempted, returned = sum(frame$returned),
    converged = sum(frame$converged), admissible = sum(frame$admissible), used = used,
    improper_kept = sum(frame$used & !frame$admissible),
    improper_set_aside = sum(!frame$used & frame$outcome == "inadmissible"),
    outcome = table(factor(outcome, levels = outcomes))
  )
  u$draws <- frame
  u$reasons <- reasons
  u$unusable_share <- if (attempted) (attempted - used) / attempted else NA_real_
  u$caution_share <- plan$caution_share
  # Compared as counts, so a share that equals the tail area exactly is not
  # lost to rounding error.
  affected <- attempted - used + u$counts$improper_kept
  u$caution <- affected > 0L && affected >= attempted * plan$caution_share - 1e-9
  u$complete <- attempted >= plan$B
  u$rule <- plan$rule
  u$rows <- plan$rows
  u$sampling <- plan$sampling
  if (!is.null(plan$spec$cluster)) u$label <- "Whole-cluster bootstrap percentile"
  u$limit_range <- NULL
  out$code <- sieve_reliability_code(plan$spec, rng = plan$rng, attempted = attempted, path = plan$path)
  if (used >= plan$min_used) {
    lim <- sieve_percentile_limits(frame$estimate[frame$used], attempted, plan$conf_level, range = c(-Inf, Inf))
    out$se <- lim$se
    out$lower <- lim$lower
    out$upper <- lim$upper
    u$status <- "completed"
    u$reason <- NULL
    u$mc_se <- sieve_bootstrap_mc_error(used, plan$conf_level) * lim$se
    u$limit_range <- lim[c("lower_min", "lower_max", "upper_min", "upper_max")]
  } else {
    u$status <- "failed"
    u$reason <- paste(
      if (attempted == 0L) {
        "No replication was completed."
      } else if (used == attempted) {
        sprintf("Only %d replications were completed.", attempted)
      } else {
        sprintf("Only %d of the %d replications attempted could be used.", used, attempted)
      },
      sprintf(
        paste0(
          "A %s percent percentile interval needs at least %d usable replications; with ",
          "fewer, even the most extreme replication cuts off, on average, more than the tail a ",
          "limit stands for (1 / (B + 1) of the bootstrap distribution, for B replications)."
        ),
        format(plan$conf_level * 100), plan$min_used
      )
    )
  }
  out$uncertainty <- u
  out
}

#' The R that reproduces SIEVER's counted bootstrap of a coefficient
#'
#' Plain DMAR, written from the specification the bootstrap ran from: the
#' pool of rows, the engine's point estimator, the acceptance rule, the
#' seed, the per replication seeds, and the minimum number of usable
#' replications an interval needs. Run in a clean session it draws the same
#' resamples and returns the same estimate, counts, and limits.
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @param rng The seed record of the run.
#' @param attempted The number of replications run (fewer than `spec$B`
#'   when the run was stopped).
#' @param data_name The name of the item data frame in the script.
#' @param path How each replication estimated the coefficient (see
#'   `sieve_reliability_boot_path()`).
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_reliability_boot_code <- function(spec, rng = NULL, attempted = spec$B,
                                        data_name = "items",
                                        path = sieve_reliability_boot_path(spec)) {
  call <- sieve_reliability_call(spec, ci_method = "none")
  args <- paste(vapply(names(call$args), function(nm) {
    sprintf("%s = %s", nm, sieve_r_literal(call$args[[nm]]))
  }, character(1)), collapse = ", ")
  estimate_lines <- if (identical(path, "ml_fit")) {
    c(
      "    # Robust omega of the resample as DMAR's reliability_omega() defines it, from the",
      "    # loadings of DMAR's one-factor model fitted by ML (the point estimates of the",
      "    # robust ML fit the engine call makes, without its scaled test statistic) over",
      "    # the variance of the composite (covariance matrix with divisor N).",
      "    fit <- DMAR::cfa_1(data = data, estimator = \"ML\", se = \"none\", output = \"fit\", missing = \"listwise\")",
      "    if (!isTRUE(lavaan::lavInspect(fit, \"converged\"))) stop(\"The single-factor CFA model did not converge.\")",
      "    theta <- lavaan::lavInspect(fit, \"est\")$theta",
      "    if (any(diag(theta) < 0) || !isTRUE(suppressWarnings(lavaan::lavInspect(fit, \"post.check\")))) {",
      "      r <- DMAR::reliability_omega(data = data, denominator = \"observed\", ci_method = \"none\")",
      "      r$value[r$term == \"estimate\"]",
      "    } else {",
      "    s <- stats::cov(data)",
      "    sum(lavaan::lavInspect(fit, \"est\")$lambda)^2 / (sum(s) * (nrow(data) - 1) / nrow(data))",
      "    }"
    )
  } else {
    c(
      sprintf("    r <- DMAR::%s(data = data, %s)", call$fun, args),
      "    r$value[r$term == \"estimate\"]"
    )
  }
  columns <- sieve_r_literal(unique(c(spec$items, spec$aux)))
  data_name <- sieve_reliability_data_code(spec, data_name)
  conf <- sieve_r_literal(spec$conf_level)
  min_used <- sieve_bootstrap_min_draws(spec$conf_level)
  stopped <- attempted < spec$B
  fiml <- identical(spec$missing, "fiml")
  c(
    sprintf(
      paste0(
        "# %s: %s, %s, bootstrap percentile interval with counted replications ",
        "(%d replications%s, %s%% level)."
      ),
      spec$construct, spec$label,
      if (fiml) "full information maximum likelihood" else "complete cases",
      attempted, if (stopped) sprintf(" (stopped; %d were requested)", spec$B) else "",
      format(spec$conf_level * 100)
    ),
    if (!is.null(rng) && !isTRUE(rng$supplied)) {
      "# No seed was supplied; SIEVER drew this one so the bootstrap can be replayed."
    },
    if (!is.null(spec$cluster)) paste0("omega_boot_cluster <- ", sieve_r_literal(spec$cluster)),
    sprintf("omega_boot_pool <- %s[, %s, drop = FALSE]", data_name, columns),
    sieve_numeric_code("omega_boot_pool"),
    if (!is.null(spec$cluster)) {
      c(if (fiml) "omega_boot_keep <- rowSums(!is.na(omega_boot_pool)) > 0" else "omega_boot_keep <- stats::complete.cases(omega_boot_pool)",
        "omega_boot_cluster <- omega_boot_cluster[omega_boot_keep]")
    },
    if (fiml) {
      "omega_boot_pool <- omega_boot_pool[rowSums(!is.na(omega_boot_pool)) > 0, , drop = FALSE]"
    } else {
      "omega_boot_pool <- omega_boot_pool[stats::complete.cases(omega_boot_pool), , drop = FALSE]"
    },
    sprintf("omega_boot_point <- DMAR::%s(data = omega_boot_pool, %s)", call$fun, args),
    "# One replication: the coefficient, or NULL when the replication is not used. A",
    "# replication is used when the engine returned a finite converged estimate;",
    "# improper replications are retained and counted separately.",
    "omega_boot_draw <- function(data) {",
    "  flags <- new.env()",
    "  flags$improper <- FALSE",
    "  est <- tryCatch(withCallingHandlers({",
    estimate_lines,
    "  }, warning = function(w) {",
    "    if (grepl(\"Heywood|[Ii]mproper|variances are negative\", conditionMessage(w))) {",
    "      flags$improper <- TRUE",
    "    }",
    "    invokeRestart(\"muffleWarning\")",
    "  }), error = function(e) NA_real_)",
    "  if (length(est) == 1L && is.finite(est)) structure(est, improper = flags$improper) else NULL",
    "}",
    "# One seed per replication, from the seed of the run, so any replication can be",
    "# refitted alone.",
    if (!is.null(rng)) sieve_seed_code(rng),
    sprintf(
      "omega_boot_seeds <- sample.int(.Machine$integer.max, %dL)%s", spec$B,
      if (stopped) sprintf("[seq_len(%dL)]", attempted) else ""
    ),
    "omega_boot_draws <- lapply(omega_boot_seeds, function(draw_seed) {",
    "  set.seed(draw_seed)",
    if (!is.null(spec$cluster)) c(
      "  blocks <- unname(split(seq_len(nrow(omega_boot_pool)), factor(match(omega_boot_cluster, unique(omega_boot_cluster)), levels = seq_along(unique(omega_boot_cluster)))))",
      "  rows <- unlist(blocks[sample.int(length(blocks), length(blocks), replace = TRUE)], use.names = FALSE)"
    ) else "  rows <- sample.int(nrow(omega_boot_pool), nrow(omega_boot_pool), replace = TRUE)",
    "  omega_boot_draw(omega_boot_pool[rows, , drop = FALSE])",
    "})",
    "omega_boot_used <- unlist(omega_boot_draws)",
    "omega_boot_improper_kept <- sum(vapply(omega_boot_draws, function(x) isTRUE(attr(x, \"improper\")), logical(1)))",
    sprintf("omega_boot_caution <- (length(omega_boot_draws) - length(omega_boot_used) + omega_boot_improper_kept > 0L) && length(omega_boot_draws) - length(omega_boot_used) + omega_boot_improper_kept >= length(omega_boot_draws) * (1 - %s) / 2 - 1e-9", conf),
    sprintf(paste0(
      "# Percentile limits from the replications used; a %s%% interval is formed only ",
      "from at least %d of them."
    ), format(spec$conf_level * 100), min_used),
    sprintf("omega_boot_limits <- if (length(omega_boot_used) >= %dL) {", min_used),
    sprintf(
      "  stats::quantile(omega_boot_used, probs = c((1 - %s) / 2, 1 - (1 - %s) / 2), names = FALSE)",
      conf, conf
    ),
    "} else {",
    "  c(NA_real_, NA_real_)",
    "}",
    sprintf("%s <- c(estimate = omega_boot_point$value[omega_boot_point$term == \"estimate\"],",
      sieve_reliability_object("omega_boot", spec)),
    "  lower = omega_boot_limits[1], upper = omega_boot_limits[2],",
    "  attempted = length(omega_boot_draws), used = length(omega_boot_used),",
    "  improper_kept = omega_boot_improper_kept, caution = omega_boot_caution)"
  )
}

#' Versions of the software behind a reliability result
#'
#' @return A named list of version strings.
#' @author Ken Kelley
#' @noRd
sieve_reliability_versions <- function() {
  version_of <- function(pkg) {
    if (sieve_have(pkg)) as.character(utils::packageVersion(pkg)) else NA_character_
  }
  list(
    SIEVER = sieve_version(),
    DMAR = version_of("DMAR"),
    lavaan = version_of("lavaan"),
    R = paste(R.version$major, R.version$minor, sep = ".")
  )
}

#' H and its delta method interval from standardized loadings
#'
#' The construct replicability (maximal reliability) coefficient of Hancock
#' and Mueller (2001),
#' \deqn{H = 1 / (1 + 1 / \sum_i \lambda_i^2 / (1 - \lambda_i^2)),}
#' for standardized loadings \eqn{\lambda_i} of a one factor model. Its
#' sampling variance by the delta method is \eqn{g' V g}, where \eqn{V} is
#' the joint covariance matrix of the estimated standardized loadings and
#' \deqn{g_i = \partial H / \partial \lambda_i =
#'   2 \lambda_i / [(1 - \lambda_i^2)^2 (1 + S)^2], \quad
#'   S = \sum_j \lambda_j^2 / (1 - \lambda_j^2).}
#' The estimated loadings of one model are correlated, so the off diagonal
#' elements of \eqn{V} matter: using the marginal standard errors alone
#' (a diagonal \eqn{V}) is a different, generally wrong, variance. The
#' interval is formed on the logit scale and transformed back, so its
#' limits stay inside (0, 1) and always contain the estimate.
#'
#' @param loadings Standardized loadings, each strictly between -1 and 1.
#' @param vcov Their joint covariance matrix, or `NULL` for the estimate
#'   and gradient only.
#' @param conf_level Confidence level.
#' @return A list with `estimate`, `gradient`, `variance`, `se`, `lower`,
#'   `upper`, `conf_level`, and `transformation` (`"logit"`).
#' @author Ken Kelley
#' @noRd
sieve_H_delta <- function(loadings, vcov = NULL, conf_level = 0.95) {
  loadings <- as.numeric(loadings)
  if (length(loadings) < 2L || anyNA(loadings)) {
    stop("`loadings` must hold two or more values, none missing.", call. = FALSE)
  }
  if (any(abs(loadings) >= 1)) {
    stop("Each standardized loading must lie strictly between -1 and 1.", call. = FALSE)
  }
  ratio <- loadings^2 / (1 - loadings^2)
  total <- sum(ratio)
  estimate <- total / (1 + total)
  gradient <- 2 * loadings / ((1 - loadings^2)^2 * (1 + total)^2)
  out <- list(
    estimate = estimate, gradient = gradient, variance = NA_real_,
    se = NA_real_, lower = NA_real_, upper = NA_real_,
    conf_level = conf_level, transformation = "logit"
  )
  if (!is.null(vcov) && !identical(dim(as.matrix(vcov)), c(length(loadings), length(loadings)))) {
    stop("`vcov` must be a square matrix with one row per loading.", call. = FALSE)
  }
  result <- DMAR::reliability_H(loadings = loadings, vcov_loadings = vcov,
    ci_method = if (is.null(vcov)) "none" else "delta_logistic", conf_level = conf_level)
  out$estimate <- sieve_reliability_value(result, "estimate")
  if (!is.null(vcov)) {
    out$se <- sieve_reliability_value(result, "se")
    out$variance <- out$se^2
    out$lower <- sieve_reliability_value(result, "ci_lower")
    out$upper <- sieve_reliability_value(result, "ci_upper")
  }
  out
}

#' The one factor model behind a construct's H
#'
#' The model follows the measurement level of the items: maximum likelihood
#' for continuous items (robust standard errors unless the normal theory
#' interval was requested for omega), and WLSMV on polychoric correlations
#' with complete cases when any item is ordered or binary, in which case
#' the standardized loadings of those items, and therefore H, refer to
#' their latent response variables. The latent variance is fixed to one
#' (`std.lv = TRUE`), so every loading is free and has a row in the
#' covariance matrix of the standardized estimates.
#'
#' The latent variable is named by the same allocation every other model
#' uses (`sieve_allocate_names()`): the construct's label when it is
#' usable as a name and no item carries it, otherwise a name derived from
#' it, so the recorded model reads as the construct and cannot be confused
#' with an item. The model is carried as a `sieve_spec` (see
#' `sieve_model_spec()`), so it is fitted through [sieve_run_model()] and
#' receives a status like every other fit.
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @return A list with `latent`, `label_shown` (the construct label the
#'   latent name stands for), `model`, `estimator`, `ordered`, `missing`,
#'   `missing_reason`, `target`, `label`, and `spec` (the `sieve_spec`).
#' @author Ken Kelley
#' @noRd
sieve_reliability_H_model <- function(spec) {
  ordered <- spec$items[spec$types %in% c("ordered", "binary")]
  shown <- if (length(spec$construct) == 1L && !is.na(spec$construct) &&
    nzchar(trimws(spec$construct))) spec$construct else "F"
  latent <- unname(sieve_allocate_names(shown, taken = c(spec$items, spec$aux), prefix = "F"))
  model <- sprintf("%s =~ %s", latent, paste(spec$items, collapse = " + "))
  plan <- list(
    structure = "one_factor", model = model, observed = spec$items, latent = latent,
    name_map = stats::setNames(shown, latent), not_modeled = sieve_not_modeled()
  )
  ledger <- data.frame(
    item = spec$items, type = spec$types, status = "active",
    stringsAsFactors = FALSE
  )
  mspec <- sieve_model_spec(plan, ledger, kind = "reliability_H", fun = "cfa", std_lv = TRUE)
  if (length(ordered)) {
    all_ordered <- length(ordered) == length(spec$items)
    reason <- if (identical(spec$requested$missing, "fiml")) {
      paste0(
        "Full information maximum likelihood is not offered for ordered or binary ",
        "items, so the model for H used complete cases."
      )
    }
    return(list(
      latent = latent, label_shown = shown, model = model,
      estimator = mspec$estimator, ordered = mspec$ordered,
      missing = mspec$missing, missing_reason = reason,
      target = if (all_ordered) {
        "latent response variables"
      } else {
        "latent response variables for the ordered items, observed scores for the others"
      },
      label = if (all_ordered) {
        "H (latent responses)"
      } else {
        "H (latent responses for the ordered items)"
      },
      spec = mspec
    ))
  }
  # Continuous items: the estimator and the missing data treatment follow
  # the omega request, so H and omega describe the same rows and the same
  # standard error type. The engine's robust interval for omega uses
  # lavaan's robust.sem standard error with complete cases (Satorra and
  # Bentler, 1994) and the Huber-White sandwich under FIML (see
  # sieve_reliability_interval()); lavaan's MLR estimator gives the
  # Huber-White sandwich in both cases, so with complete cases H is fitted
  # by ML with the robust.sem standard error (and the Satorra-Bentler
  # scaled test, for the one-factor RMSEA), and one request for robust ML
  # gives one standard error type.
  robust <- !identical(spec$h_se, "normal_theory")
  mspec$estimator <- if (robust && identical(spec$missing, "fiml")) "MLR" else "ML"
  mspec$missing <- spec$missing
  if (length(spec$aux)) mspec <- sieve_spec_engine_args(mspec, list(aux = spec$aux))
  if (robust && !identical(spec$missing, "fiml")) {
    mspec <- sieve_spec_engine_args(mspec, list(se = "robust.sem", test = "satorra.bentler"))
  }
  plain <- unclass(mspec)
  plain$key <- NULL
  mspec$key <- sieve_hash(plain)
  list(
    latent = latent, label_shown = shown, model = model,
    estimator = mspec$estimator, ordered = NULL,
    missing = mspec$missing, missing_reason = NULL,
    target = "observed item scores", label = "H (observed item scores)", spec = mspec
  )
}

#' The DMAR call that gives a construct's H and its interval
#'
#' H and its interval are `DMAR::reliability_H()`'s, at its defaults. For continuous items the call
#' takes the raw item scores (`data`, with any auxiliary columns), and DMAR
#' fits the single-factor model as `reliability_omega()` does: its default
#' interval, `"mlr_logistic"`, is the delta method with the full covariance
#' matrix of the parameter estimates and the robust ML standard error
#' (Satorra-Bentler with complete cases, Yuan-Bentler under FIML), with the
#' limits formed on the logit scale; `"ml_logistic"` (normal theory) when the
#' ML interval was requested for omega. For a model with ordered or binary
#' items, which `data` does not fit, the call takes the WLSMV fit
#' (`fit = h_fit`) and DMAR's default for a fit, `"delta_logistic"`, the
#' delta method with the fit's own (robust.sem) covariance matrix. The same
#' delta method interval formed from lavaan's covariance matrix of the
#' standardized loadings agrees with DMAR's: on the bundled nine tests
#' (complete, and with values removed at random, listwise and FIML) to
#' 3.5e-9 and in the ordered case to 5.3e-10, the difference being DMAR's
#' central difference gradient.
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @param m The model of `sieve_reliability_H_model()`.
#' @return A list with `fun` (`"reliability_H"`), `input` (`"data"` or
#'   `"fit"`), and `args` (the other arguments, every one named).
#' @author Ken Kelley
#' @noRd
sieve_reliability_H_call <- function(spec, m = sieve_reliability_H_model(spec)) {
  if (length(m$ordered)) {
    return(list(fun = "reliability_H", input = "fit", args = list(
      ci_method = "delta_logistic", conf_level = spec$conf_level
    )))
  }
  args <- list(
    ci_method = if (identical(spec$h_se, "normal_theory")) "ml_logistic" else "mlr_logistic",
    missing = m$missing
  )
  if (identical(m$missing, "fiml") && length(spec$aux)) args$aux <- spec$aux
  args$conf_level <- spec$conf_level
  list(fun = "reliability_H", input = "data", args = args)
}

#' The R that reproduces a construct's H and its interval
#'
#' The one-factor model behind H (whose fit and residual correlations the
#' table shows) and the DMAR call of `sieve_reliability_H_call()`, every
#' argument named.
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @param data_name The name of the item data frame in the script.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_reliability_H_code <- function(spec, data_name = "items") {
  m <- sieve_reliability_H_model(spec)
  chr <- function(x) sprintf("c(%s)", paste(sprintf("\"%s\"", x), collapse = ", "))
  rows <- if (identical(m$missing, "fiml")) {
    sprintf("h_rows <- rowSums(!is.na(%s[, %s, drop = FALSE])) > 0", "h_data", chr(unique(c(spec$items, spec$aux))))
  } else {
    sprintf("h_rows <- stats::complete.cases(%s[, %s, drop = FALSE])", "h_data", chr(spec$items))
  }
  args <- c(
    "std.lv = TRUE",
    sprintf("estimator = \"%s\"", m$estimator),
    if (length(m$ordered)) sprintf("ordered = %s", chr(m$ordered)),
    sprintf("missing = \"%s\"", m$missing),
    if (!is.null(sieve_spec_default_test(m$spec))) "test = \"standard\"",
    sieve_engine_arg_code(m$spec),
    if (length(spec$aux)) "check.post = FALSE"
  )
  call <- sieve_reliability_H_call(spec, m)
  dmar_args <- vapply(names(call$args), function(nm) {
    value <- call$args[[nm]]
    sprintf("%s = %s", nm, if (length(value) > 1L) chr(value) else sieve_r_literal(value))
  }, character(1))
  c(
    sprintf("# %s: %s, which describes the %s.", spec$construct, m$label, m$target),
    sprintf("h_data <- %s[, %s, drop = FALSE]", sieve_reliability_data_code(spec, data_name), chr(unique(c(spec$items, spec$aux)))),
    sieve_numeric_code("h_data"),
    rows,
    "# The one-factor model behind H; its fit and residual correlations are shown beside the coefficient.",
    sprintf(
      "h_fit <- lavaan::cfa(\"%s\", data = %s[h_rows, %s, drop = FALSE],",
      m$model, "h_data", chr(unique(c(spec$items, spec$aux)))
    ),
    paste0("  ", paste(args, collapse = ", "), ")"),
    sprintf(
      "%s <- DMAR::reliability_H(%s, %s)", sieve_reliability_object("H", spec),
      if (identical(call$input, "fit")) "fit = h_fit" else "data = h_data",
      paste(dmar_args, collapse = ", ")
    ),
    "# The one-factor model's RMSEA with its interval at the RMSEA level (90% by default, DMAR's and",
    "# lavaan's; the page's confidence level sets omega's and H's intervals), in the robust family",
    "# when lavaan reports it for the model, then scaled, then standard.",
    sprintf(
      "h_rmsea <- lavaan::fitMeasures(h_fit, fm.args = list(rmsea.ci.level = %s))",
      sieve_r_literal(sieve_default_rmsea_conf_level())
    ),
    "h_rmsea[grepl(\"^rmsea([.]ci[.](lower|upper))?([.](scaled|robust))?$\", names(h_rmsea))]",
    sieve_reliability_residual_code(spec, data_name = data_name)
  )
}

#' The recorded helper code for H's interval, which is empty
#'
#' H's interval is the DMAR call of `sieve_reliability_H_call()`, so the
#' recorded R defines no helper for it. The function returns no lines, so a
#' caller that prepends it still runs.
#'
#' @return `character(0)`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_H_helper_code <- function() {
  character(0)
}

#' Estimate one construct's H with DMAR's interval
#'
#' Fits the one factor model that matches the item types (see
#' `sieve_reliability_H_model()`) through [sieve_run_model()], so the fit
#' is guarded and given a status like every other model in SIEVER (its fit
#' and residual correlations are shown beside the coefficient), and, when
#' that solution is converged, identified, and admissible, takes H, its
#' standard error, and its interval from `DMAR::reliability_H()` at its
#' defaults (see `sieve_reliability_H_call()`): a fitted object is not
#' taken as a certificate, and an improper solution has no H. A refusal
#' of DMAR's (class `dmar_refusal`) is reported with its reason; the
#' standardized loadings and their joint covariance matrix from lavaan are
#' kept in the record.
#'
#' @param data A data frame holding the item columns.
#' @param spec A specification from [sieve_reliability_spec()].
#' @return A list of class `sieve_reliability_H` with `construct`, `k`,
#'   `label`, `target`, `estimate`, `se`, `variance`, `lower`, `upper`,
#'   `conf_level`, `loadings`, `vcov`, `status` (from
#'   `sieve_fit_status()`, as [sieve_run_model()] reports it),
#'   `uncertainty` (`status`, `method`, `ci_method` (DMAR's), `label`,
#'   `transformation`, `se_type`, `information`, `reason`,
#'   `condition_class`), `model` (see `sieve_reliability_H_model()`; its
#'   `spec` is the fitted specification), `missing` (`requested`, `used`,
#'   `reason`), `n` (`used`), `warnings`, `messages`, `error`, and `code`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_H <- function(data, spec) {
  m <- sieve_reliability_H_model(spec)
  call <- sieve_reliability_H_call(spec, m)
  out <- list(
    construct = spec$construct, k = length(spec$items),
    label = m$label, target = m$target,
    estimate = NA_real_, se = NA_real_, variance = NA_real_,
    lower = NA_real_, upper = NA_real_, conf_level = spec$conf_level,
    loadings = NULL, vcov = NULL,
    status = sieve_fit_status(NULL, error = "Not computed."),
    uncertainty = list(
      status = "failed",
      method = "DMAR::reliability_H(), delta method with the full covariance matrix of the estimates",
      ci_method = call$args$ci_method,
      label = sieve_reliability_H_label(call$args$ci_method),
      transformation = "logit", se_type = NA_character_,
      information = NA_character_, reason = NULL
    ),
    model = m,
    missing = list(
      requested = spec$requested$missing, used = m$missing,
      reason = m$missing_reason
    ),
    n = list(used = NA_integer_),
    fit_summary = sieve_reliability_fit_summary(NULL, conf_level = sieve_default_rmsea_conf_level()),
    warnings = character(0), messages = character(0), error = NULL,
    code = sieve_reliability_H_code(spec)
  )
  class(out) <- "sieve_reliability_H"
  fail <- function(why, status = NULL) {
    out$error <- why
    out$status <- status %||% sieve_fit_status(NULL, error = why)
    out$uncertainty$reason <- "H could not be estimated, so no interval was formed."
    out
  }

  if (length(spec$items) < 3L) {
    return(fail("H needs three or more items: the one factor model is not identified with two."))
  }
  prepared <- sieve_capture(sieve_reliability_data(data, unique(c(spec$items, spec$aux))))
  if (!prepared$ok) {
    return(fail(prepared$error))
  }
  X <- prepared$value
  seen <- rowSums(!is.na(as.matrix(X[, spec$items, drop = FALSE])))
  # Under FIML a row observed only on an auxiliary still informs the joint model.
  X <- X[if (identical(m$missing, "fiml")) rowSums(!is.na(as.matrix(X))) > 0L else
    seen == length(spec$items), , drop = FALSE]
  if (nrow(X) < 3L) {
    return(fail(sprintf("Only %d usable row(s) for this construct.", nrow(X))))
  }
  # The fit goes through the status contract: the latent name is checked
  # against the columns, the fitted variables against the intended ones,
  # the conditions are kept, and the status is read from the fitted object.
  run <- sieve_run_model(m$spec, X)
  out$warnings <- run$warnings
  out$messages <- run$messages
  if (is.null(run$fit)) {
    return(fail(run$error %||% run$status$message, run$status))
  }
  fit <- run$fit
  out$missing_diagnostics <- attr(fit, "sieve_missing_data", exact = TRUE)
  if (!is.null(out$missing_diagnostics)) {
    out$missing_diagnostics$counts <- sieve_reliability_counts(
      prepared$value[, spec$items, drop = FALSE])
  }
  status <- run$status
  out$status <- status
  out$n$used <- as.integer(sum(lavaan::lavInspect(fit, "nobs")))
  options <- lavaan::lavInspect(fit, "options")
  out$uncertainty$se_type <- as.character(options$se)[1]
  out$uncertainty$information <- as.character(options$information)[1]
  if (!status$code %in% c("ok", "inadmissible")) {
    return(fail(status$message, status))
  }
  # The fit of the one-factor model behind H, omega, and categorical omega,
  # so the reader can judge the assumption model implied omega rests on.
  out$fit_summary <- sieve_reliability_fit_summary(fit, conf_level = sieve_default_rmsea_conf_level())
  # When this is the coefficient's own one-factor model (every item
  # continuous, or every item ordered or binary), its residual correlations
  # are the ones shown beside omega, so no further model is fitted for them.
  if (!identical(spec$item_class, "mixed")) {
    resid <- sieve_capture(sieve_reliability_residual_summary(fit, spec$items, categorical = length(m$ordered) > 0L))
    if (resid$ok) out$residual_summary <- resid$value
  }
  lambda <- lavaan::lavInspect(fit, "std")$lambda
  loadings <- stats::setNames(as.numeric(lambda[spec$items, 1L]), spec$items)
  out$loadings <- loadings
  if (any(!is.finite(loadings)) || any(abs(loadings) >= 1) ||
    identical(status$code, "inadmissible")) {
    # H is a function of loadings strictly inside (-1, 1); an improper
    # solution has no H, whatever number the formula would return.
    status$code <- "inadmissible"
    status$admissible <- FALSE
    return(fail(paste0(
      "The one factor solution is improper (a standardized loading of one or more, ",
      "or a negative variance), so H is not defined for it."
    ), status))
  }
  rows <- paste0(m$latent, "=~", spec$items)
  joint <- sieve_capture(lavaan::lavInspect(fit, "vcov.std.all"))
  if (joint$ok && all(rows %in% rownames(joint$value))) {
    V <- joint$value[rows, rows, drop = FALSE]
    dimnames(V) <- list(spec$items, spec$items)
    out$vcov <- V
  }
  # H, its standard error, and its interval: DMAR::reliability_H() at its
  # defaults, on the rows and columns the recorded call names.
  input <- if (identical(call$input, "fit")) {
    list(fit = fit)
  } else {
    list(data = prepared$value[, unique(c(spec$items, spec$aux)), drop = FALSE])
  }
  engine <- sieve_dmar_boot_run(list(fun = call$fun, args = c(input, call$args)))
  out$warnings <- unique(c(out$warnings, engine$warnings))
  if (!isTRUE(engine$ok)) {
    out$uncertainty$condition_class <- engine$error_class
    why <- if (identical(sieve_dmar_boot_outcome(engine), "refused")) {
      cond <- engine$condition
      paste0(
        "DMAR refused H",
        if (length(cond$reason) == 1L && !is.na(cond$reason)) sprintf(" (%s)", cond$reason) else "",
        ": ", gsub("\\s+", " ", engine$error)
      )
    } else {
      paste("DMAR::reliability_H() stopped:", gsub("\\s+", " ", engine$error))
    }
    if (identical(sieve_dmar_boot_outcome(engine), "refused")) {
      # A refusal concerns the model or the data, so no H is reported.
      out$error <- why
      out$uncertainty$reason <- why
      return(out)
    }
    # Another error concerns the interval: H itself is the closed form of
    # the admissible loadings (DMAR's, with no interval).
    point <- sieve_capture(sieve_H_delta(loadings, conf_level = spec$conf_level))
    if (point$ok) out$estimate <- point$value$estimate
    out$uncertainty$reason <- why
    return(out)
  }
  value <- engine$value
  sieve_dmar_require_terms(value, c("estimate", "se", "ci_lower", "ci_upper"), "Reliability H")
  out$estimate <- sieve_reliability_value(value, "estimate")
  out$se <- sieve_reliability_value(value, "se")
  out$variance <- out$se^2
  out$lower <- sieve_reliability_value(value, "ci_lower")
  out$upper <- sieve_reliability_value(value, "ci_upper")
  actual <- attr(value, "ci_method", exact = TRUE)
  if (is.character(actual) && length(actual) == 1L && !is.na(actual)) {
    out$uncertainty$ci_method <- actual
    out$uncertainty$label <- sieve_reliability_H_label(actual)
  }
  if (is.finite(out$lower) && is.finite(out$upper)) {
    out$uncertainty$status <- "completed"
  } else {
    out$uncertainty$reason <- "DMAR returned no interval limits for H."
  }
  out
}

#' The interval label of H, by DMAR's method
#'
#' @param ci_method DMAR's `ci_method` for H.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_reliability_H_label <- function(ci_method) {
  switch(as.character(ci_method)[1],
    mlr_logistic = "MLR (sandwich) SE, logit-scale limits (delta method, DMAR)",
    ml_logistic = "ML, logit scale (delta method, DMAR)",
    delta_logistic = "Delta method, logit scale (the fit's robust standard errors, DMAR)",
    as.character(ci_method)[1]
  )
}

#' The fit of a construct's one-factor model, for the reliability table
#'
#' RMSEA with its interval (and CFI and SRMR) of the one-factor model
#' fitted for H, which is the model behind model implied omega: the same
#' items, rows, and estimator. Model implied omega is the reliability only
#' when that model holds, so its fit is shown beside it. The family of the
#' indices (standard, scaled, or robust) is chosen by the rule of
#' `sieve_fit_measures()` and named. With three items the usual model has
#' zero degrees of freedom, so there is no ordinary test of its fit. The
#' RMSEA interval is at `conf_level`, the RMSEA level
#' (`sieve_default_rmsea_conf_level()`, 90 percent by default, DMAR's and
#' lavaan's), not at the page's confidence level, which sets omega's and
#' H's intervals.
#'
#' @param fit A fitted lavaan model, or `NULL`.
#' @param conf_level The confidence level of the RMSEA interval.
#' @return A list with `rmsea`, `lower`, `upper`, `level`, `family`, `cfi`,
#'   `srmr`, `df`, and `reason` (why the RMSEA is not shown, or `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_reliability_fit_summary <- function(fit, conf_level = sieve_default_rmsea_conf_level()) {
  out <- list(
    rmsea = NA_real_, lower = NA_real_, upper = NA_real_, level = conf_level,
    family = NA_character_, cfi = NA_real_, srmr = NA_real_, df = NA_real_,
    reason = "The one-factor model was not fitted."
  )
  if (is.null(fit)) {
    return(out)
  }
  read <- sieve_capture(sieve_fit_measures(fit, conf_level = conf_level))
  if (!read$ok || is.null(read$value)) {
    out$reason <- read$error %||% "lavaan returned no fit measures for the one-factor model."
    return(out)
  }
  v <- read$value
  pick <- function(nm) if (nm %in% names(v)) unname(v[[nm]]) else NA_real_
  out$df <- pick("df")
  out$family <- attr(v, "family") %||% NA_character_
  out$level <- sieve_fit_ci_level(v)
  out$cfi <- pick("cfi")
  out$srmr <- pick("srmr")
  if (is.finite(out$df) && out$df == 0) {
    out$reason <- paste0(
      "The one-factor model has zero degrees of freedom, so there is no ordinary test of ",
      "its fit from these items alone."
    )
    return(out)
  }
  out$rmsea <- pick("rmsea")
  out$lower <- pick("rmsea_lo")
  out$upper <- pick("rmsea_hi")
  out$reason <- if (!is.finite(out$rmsea)) "lavaan returned no RMSEA for the one-factor model."
  out
}

#' The one-factor RMSEA as it is shown
#'
#' @param fs A list from `sieve_reliability_fit_summary()`.
#' @return A length one character string, such as
#'   `"0.071, 95% CI [0.040, 0.105] (robust)"` (the interval named with the
#'   level it was computed at), or the reason there is none.
#' @author Ken Kelley
#' @noRd
sieve_reliability_rmsea_text <- function(fs) {
  if (is.null(fs) || !is.finite(fs$rmsea %||% NA_real_)) {
    if (!is.null(fs$df) && is.finite(fs$df) && fs$df == 0) return("not interpreted (df = 0)")
    return("not available")
  }
  sprintf(
    "%.3f%s%s", fs$rmsea,
    if (is.finite(fs$lower) && is.finite(fs$upper)) {
      sprintf(", %s [%.3f, %.3f]", sieve_rmsea_ci_words(fs$level %||% NA_real_), fs$lower, fs$upper)
    } else "",
    if (!is.na(fs$family)) sprintf(" (%s)", fs$family) else ""
  )
}

#' The one-factor model behind a construct's residual correlations
#'
#' The model DMAR fits for the coefficient, with the same items, rows, and
#' estimator, fitted by SIEVER when neither DMAR nor the model behind H
#' supplies the residual correlations: maximum likelihood with the
#' coefficient's missing data treatment for continuous items (and for
#' ordered items entered as their numeric codes in a construct that mixes
#' them with continuous items), WLSMV on the complete rows for a construct
#' of ordered or binary items. The factor variance is fixed to one.
#'
#' @param data A data frame holding the item columns.
#' @param spec A specification from [sieve_reliability_spec()].
#' @return A fitted lavaan model.
#' @author Ken Kelley
#' @noRd
sieve_reliability_residual_fit <- function(data, spec) {
  X <- sieve_reliability_data(data, unique(c(spec$items, spec$aux)))
  sieve_one_factor(X, ordered = identical(spec$coefficient, "omega_categorical"),
    missing = spec$missing, aux = spec$aux, output = "fit")
}

#' The residual correlations of a fitted one-factor model
#'
#' The observed correlations (for ordered items the unrestricted polychoric
#' correlations) minus those the fitted one-factor model implies, lavaan's
#' `"cor.bollen"` residuals, which is what DMAR summarizes in its
#' `residual_cor` attribute, and the largest of them in absolute value with
#' its item pair. For continuous items the sum of the raw residual
#' covariances, 1'(S - Sigma_hat)1, is returned as well: the amount by which
#' the observed denominator of omega exceeds the model-implied one.
#'
#' @param fit A fitted one-factor lavaan model.
#' @param items The item names.
#' @param categorical Whether the items are modeled as ordered.
#' @return A list with `matrix`, `largest` (signed), `items` (the pair), and
#'   `sum_residual_cov` (`NA` for ordered items).
#' @author Ken Kelley
#' @noRd
sieve_reliability_residual_summary <- function(fit, items, categorical = FALSE) {
  as_matrix <- function(m) {
    m <- as.matrix(unclass(m))
    matrix(as.numeric(m), nrow(m), ncol(m), dimnames = dimnames(m))[items, items, drop = FALSE]
  }
  R <- as_matrix(lavaan::lavResiduals(fit, type = "cor.bollen", summary = FALSE, se = FALSE, zstat = FALSE)$cov)
  R <- (R + t(R)) / 2
  at <- which(upper.tri(R), arr.ind = TRUE)
  values <- R[at]
  k <- which.max(abs(replace(values, !is.finite(values), 0)))
  list(
    matrix = R, largest = values[k], items = items[at[k, ]],
    sum_residual_cov = if (categorical) {
      NA_real_
    } else {
      sum(as_matrix(lavaan::lavResiduals(fit, type = "raw", summary = FALSE, se = FALSE, zstat = FALSE)$cov))
    }
  )
}

#' The largest single-factor residual correlation beside a construct's omega
#'
#' Shown beside robust omega because its numerator is model based: a doublet
#' or a method factor among a few same-signed items raises their loadings,
#' and omega with them, under either denominator, and the residual
#' correlations are where that structure usually shows. A rising omega while a large residual pair is
#' retained signals redundancy, not better measurement; a small largest
#' residual does not certify unidimensionality, since shared variance on the
#' dominant items is absorbed by the single factor and the misfit appears
#' among the other items. DMAR's own summary (`attr(x, "residual_cor")`) is
#' used when DMAR attaches it to the result; otherwise SIEVER computes the
#' same quantity from the one-factor model of the same items, rows, and
#' estimator (the model behind H when it is that model, a fit of its own
#' otherwise). Three-item residual diagnostics are not interpreted here;
#' zero degrees of freedom do not provide an ordinary test of local dependence.
#'
#' @param data A data frame holding the item columns, or `NULL` when
#'   `summary` is supplied.
#' @param spec A specification from [sieve_reliability_spec()].
#' @param given DMAR's `residual_cor` attribute, or `NULL`.
#' @param summary A summary from `sieve_reliability_residual_summary()` of
#'   the same model, or `NULL` to fit the model here.
#' @return A list with `largest`, `items`, `matrix`, `sum_residual_cov`,
#'   `source`, `scale`, `saturated` (`TRUE` for three items, whose residual
#'   diagnostics are not interpreted; not an exact-fit certificate), and
#'   `reason` (why there is no value).
#' @author Ken Kelley
#' @noRd
sieve_reliability_residuals <- function(data, spec, given = NULL, summary = NULL) {
  categorical <- identical(spec$coefficient, "omega_categorical")
  out <- list(
    largest = NA_real_, items = c(NA_character_, NA_character_), matrix = NULL,
    sum_residual_cov = NA_real_, source = NA_character_,
    scale = if (categorical) {
      "unrestricted polychoric correlations minus those the one-factor model implies"
    } else {
      "observed correlations minus those the one-factor model implies"
    },
    saturated = length(spec$items) == 3L, reason = NULL
  )
  if (length(spec$items) < 3L) {
    out$reason <- "The one-factor model needs three or more items."
    return(out)
  }
  if (is.null(summary) && !is.null(data)) {
    run <- sieve_capture(sieve_reliability_summary_of_fit(data, spec, categorical))
    if (run$ok) summary <- run$value else out$reason <- run$error
  }
  if (is.list(given) && length(given$largest) == 1L && is.finite(given$largest)) {
    out$largest <- as.numeric(given$largest)
    out$items <- as.character(given$items)
    out$matrix <- given$matrix
    out$source <- "DMAR (its residual_cor attribute)"
  } else if (is.list(summary) && length(summary$largest) == 1L && is.finite(summary$largest)) {
    out$largest <- summary$largest
    out$items <- summary$items
    out$matrix <- summary$matrix
    out$source <- "SIEVER, from the one-factor lavaan fit (lavaan::lavResiduals(), type cor.bollen)"
  } else if (is.null(out$reason)) {
    out$reason <- "The residual correlations of the one-factor model could not be computed."
  }
  if (is.list(summary)) out$sum_residual_cov <- summary$sum_residual_cov %||% NA_real_
  out
}

#' Fit the one-factor model of a construct and summarize its residuals
#' @param data,spec As for `sieve_reliability_residuals()`.
#' @param categorical Whether the items are modeled as ordered.
#' @return A summary from `sieve_reliability_residual_summary()`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_summary_of_fit <- function(data, spec, categorical) {
  fit <- sieve_reliability_residual_fit(data, spec)
  if (!isTRUE(lavaan::lavInspect(fit, "converged"))) {
    stop("The one-factor model behind the residual correlations did not converge.", call. = FALSE)
  }
  sieve_reliability_residual_summary(fit, spec$items, categorical)
}

#' The largest residual correlation, as it is shown
#'
#' @param res A list from `sieve_reliability_residuals()`, or `NULL`.
#' @param digits Decimals shown.
#' @return A length one character string, such as `"0.214 (x3 with x4)"`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_residual_text <- function(res, digits = 3L) {
  if (is.null(res) || !is.finite(res$largest %||% NA_real_)) {
    return(sprintf("not available%s", if (!is.null(res$reason)) sprintf(" (%s)", sub("\\.$", "", res$reason)) else ""))
  }
  if (isTRUE(res$saturated)) {
    return("not interpreted (three-item residual diagnostics)")
  }
  sprintf(paste0("%.", digits, "f (%s with %s)"), res$largest, res$items[1], res$items[2])
}

#' The R that reproduces a construct's residual correlations
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @param data_name The name of the item data frame in the script.
#' @return A character vector of R code lines. When the model behind H is
#'   the coefficient's one-factor model the lines read `h_fit`, which
#'   `sieve_reliability_H_code()` defines just before them.
#' @author Ken Kelley
#' @noRd
sieve_reliability_residual_code <- function(spec, data_name = "items") {
  object <- sieve_reliability_object("residual_cor", spec)
  lead <- sprintf(
    "# %s: residual correlations of the one-factor model (%s); the largest in absolute value is shown beside the coefficient.",
    spec$construct,
    if (identical(spec$coefficient, "omega_categorical")) {
      "unrestricted polychoric minus model-implied"
    } else {
      "observed minus model-implied"
    }
  )
  if (!identical(spec$item_class, "mixed")) {
    return(c(lead, sprintf("%s <- lavaan::lavResiduals(h_fit, type = \"cor.bollen\", summary = FALSE, se = FALSE, zstat = FALSE)$cov", object)))
  }
  chr <- function(x) sprintf("c(%s)", paste(sprintf("\"%s\"", x), collapse = ", "))
  latent <- unname(sieve_allocate_names("F", taken = c(spec$items, spec$aux), prefix = "F"))
  fiml <- identical(spec$missing, "fiml")
  c(
    lead,
    "# The items enter as their numeric codes here, as in the coefficient; the model behind H treats the ordered ones as ordered.",
    sprintf("resid_data <- %s[, %s, drop = FALSE]", sieve_reliability_data_code(spec, data_name), chr(unique(c(spec$items, spec$aux)))),
    sieve_numeric_code("resid_data"),
    sprintf(
      "resid_rows <- %s",
      if (fiml) {
        sprintf("rowSums(!is.na(%s[, %s, drop = FALSE])) > 0", "resid_data", chr(if (fiml) unique(c(spec$items, spec$aux)) else spec$items))
      } else {
        sprintf("stats::complete.cases(%s[, %s, drop = FALSE])", "resid_data", chr(if (fiml) unique(c(spec$items, spec$aux)) else spec$items))
      }
    ),
    sprintf(
      "resid_fit <- lavaan::cfa(\"%s =~ %s\", data = %s[resid_rows, %s, drop = FALSE], std.lv = TRUE, estimator = \"ML\", missing = \"%s\", se = \"none\"%s)",
      latent, paste(spec$items, collapse = " + "), "resid_data", chr(unique(c(spec$items, spec$aux))), if (fiml) "ml" else "listwise",
      if (fiml && length(spec$aux)) paste0(", aux = ", sieve_r_literal(spec$aux), ", check.post = FALSE") else ""
    ),
    sprintf("%s <- lavaan::lavResiduals(resid_fit, type = \"cor.bollen\", summary = FALSE, se = FALSE, zstat = FALSE)$cov", object)
  )
}

#' Model implied omega beside robust omega
#'
#' SIEVER's default leads with robust omega for continuous items, whose
#' interval is a bootstrap run on request. Beside it the table carries model
#' implied omega with DMAR's default interval (MLR (sandwich) SE,
#' logit-scale limits), which is immediate and is the reliability of
#' the composite only when the one-factor model holds, so it is labeled
#' "Omega (model-implied denominator)" and shown with that model's RMSEA.
#' It is computed only beside robust omega: for
#' categorical omega there is no model implied variant, and when model
#' implied omega is itself the coefficient chosen it is already the row's
#' coefficient.
#'
#' @param data A data frame holding the item columns.
#' @param spec A specification from [sieve_reliability_spec()].
#' @return A result from `sieve_reliability_omega()`, or `NULL`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_model_implied <- function(data, spec) {
  # For ordered or binary items the companion is DMAR's model-implied
  # denominator (Green & Yang, 2009); it has no closed form interval, so it
  # is a point estimate.
  categorical <- identical(spec$coefficient, "omega_categorical") &&
    identical(sieve_reliability_denominator(spec), "observed")
  if (!identical(spec$coefficient, "omega_robust") && !categorical) {
    return(NULL)
  }
  # The companion's interval is DMAR's default for model implied omega
  # ("recommended": "mlr_logistic", or "mlr" under the FMI t reference).
  companion <- sieve_reliability_spec(
    items = spec$items, types = spec$types, construct = spec$construct,
    missing = spec$missing, ci_method = if (categorical) "none" else "recommended",
    denominator = "model_implied",
    conf_level = spec$conf_level, B = spec$B, seed = spec$seed,
    group_id = spec$group_id, scope = spec$scope %||% "construct",
    parent_construct = spec$parent_construct %||% spec$construct,
    facet = spec$facet %||% NA_character_, aux = spec$aux, wald_df = spec$wald_df %||% Inf
  )
  # Recorded under its own name, so it does not overwrite robust omega's.
  companion$companion <- TRUE
  out <- sieve_reliability_cluster_interval(sieve_reliability_omega(data, companion), spec)
  if (categorical && identical(out$uncertainty$status, "not_requested")) {
    # Every interval DMAR gives categorical omega is a bootstrap that refits
    # the WLSMV model on each replication, so the companion is a point
    # estimate: its interval is said to be unavailable, with the reason,
    # rather than "none requested", which would read as a choice the user
    # made; the construct's interval is robust omega's bootstrap.
    out$uncertainty$status <- "unavailable"
    out$uncertainty$label <- "none: ordered items have only bootstrap intervals"
    out$uncertainty$unavailable_text <- out$uncertainty$label
    out$uncertainty$reason <- paste0(
      "Omega (model-implied denominator) of ordered items (Green & Yang, 2009) is shown as a point estimate: ",
      "every interval DMAR gives categorical omega is a bootstrap that refits the WLSMV model on each ",
      "replication, so none is computed beside it, and this construct's interval is the bootstrap of robust ",
      "omega, run on request."
    )
  }
  out
}

#' Coefficient alpha for one construct, for comparison
#'
#' Alpha equals the reliability of the sum score only when the items are
#' essentially tau equivalent and their errors are uncorrelated. It is
#' computed here through DMAR's analytic covariance-based estimator on the
#' same items and missing-data treatment as omega, with a compatible
#' interval when requested. Two-item alpha remains available even when
#' unconstrained omega is not identified. With a bootstrap interval the
#' call is DMAR's counted bootstrap (`sieve_reliability_alpha_plan()`
#' writes it, `sieve_dmar_boot_run()` runs it, here or in the Reliability
#' tab's background process, and `sieve_reliability_alpha_finish()` reads
#' it).
#'
#' @param data A data frame holding the item columns.
#' @param spec A specification from [sieve_reliability_spec()].
#' @param rng Optional recorded bootstrap seed.
#' @param ncpus For a bootstrap interval, the processes DMAR uses.
#' @param progress Optional `progress(done, total)` for a bootstrap.
#' @return A list with `estimate`, `status` (`code` and `message`),
#'   `missing` (the treatment used), `n` (`used`), `warnings`, and `code`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_alpha <- function(data, spec, rng = NULL, ncpus = 1L, progress = NULL) {
  plan <- sieve_reliability_alpha_plan(data, spec, rng = rng, ncpus = ncpus)
  if (is.null(plan$call)) {
    return(plan$out)
  }
  sieve_reliability_alpha_finish(plan, sieve_dmar_boot_run(plan$call, progress = progress))
}

#' Prepare alpha's DMAR call for one construct
#'
#' @inheritParams sieve_reliability_alpha
#' @return A list with `out` (the record so far), `call` (the DMAR call,
#'   or `NULL` when alpha cannot be computed; `out` then says why), `X`,
#'   `method`, and `bootstrap`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_alpha_plan <- function(data, spec, rng = NULL, ncpus = 1L) {
  requested <- spec$requested$ci_method
  # Analytic alpha remains the covariance-based coefficient, including for
  # two-item facets. Robust model-based alpha would impose tau equivalence
  # and change that estimand. Its matching intervals come from DMAR.
  # Alpha has intervals of its own, so a request that has no interval for
  # the omega variant in force (a closed form for robust omega, the
  # default, for example) still gives alpha the interval that matches the
  # request.
  # The same holds while the omega's bootstrap is selected but not run
  # (status "pending", the default): alpha keeps its own
  # analytic interval rather than waiting for a bootstrap that omega needs
  # and alpha does not. That interval, and alpha's interval under DMAR's
  # defaults ("recommended"), is DMAR's default for the analytic estimator:
  # reliability_alpha()'s
  # ci_method = NULL resolves to Bonett (2002) with complete cases and to
  # the ML standard error of van Zyl, Neudecker, and Nel (2000) under FIML
  # (sieve_reliability_alpha_default()), written out by name in the call.
  # An interval the user chose for omega gives alpha its counterpart (ADF
  # for robust ML with complete cases, ML under FIML, each in the same form,
  # on the scale of alpha or of its logit). A completed bootstrap request
  # gives alpha DMAR's counted bootstrap.
  pending <- identical(spec$uncertainty$status, "pending")
  requested_alpha <- if (pending) {
    "recommended"
  } else if (identical(spec$uncertainty$status, "unavailable")) {
    spec$requested$ci_method
  } else if (identical(spec$requested$ci_method, "recommended") &&
             !spec$ci_method %in% sieve_reliability_bootstrap_methods()) {
    "recommended"
  } else {
    spec$ci_method
  }
  fiml <- identical(spec$missing, "fiml")
  method <- if (identical(requested_alpha, "none")) "none" else switch(requested_alpha,
    recommended = sieve_reliability_alpha_default(spec$missing),
    mlr = if (fiml) "ml" else "adf",
    mlr_logistic = if (fiml) "ml_logistic" else "adf_logistic",
    ml_logistic = "ml_logistic",
    adf_logistic = if (fiml) "ml_logistic" else "adf_logistic",
    likelihood = if (fiml) "ml" else "bonett",
    percentile = "percentile",
    dmar_percentile = "percentile",
    dmar_bca = "bca",
    requested_alpha
  )
  if (!is.null(spec$cluster)) method <- "none"
  out <- list(
    estimate = NA_real_, se = NA_real_, lower = NA_real_, upper = NA_real_,
    conf_level = spec$conf_level, status = list(code = "failed", message = "Not computed."),
    missing = spec$missing, n = list(used = NA_integer_), warnings = character(0),
    uncertainty = list(
      status = if (method == "none") "not_requested" else "requested",
      method = method,
      label = sieve_reliability_alpha_label(method),
      transformation = sieve_reliability_alpha_transformation(method),
      reason = if (pending) {
        "The bootstrap selected for omega has not been run; alpha's own analytic interval (DMAR's default for alpha) is shown."
      },
      B = if (method %in% c("percentile", "bca", "bootstrap_se_logistic")) spec$B else NA_integer_,
      B_attempted = NA_integer_, B_used = NA_integer_
    ), code = character(0), seconds = NA_real_
  )
  plan <- list(out = out, call = NULL, X = NULL, method = method, bootstrap = FALSE, spec = spec)
  if (length(spec$items) < 2L) {
    plan$out$status$message <- "Alpha needs at least two included items."
    plan$out$uncertainty$status <- "unavailable"
    plan$out$uncertainty$label <- "Unavailable: fewer than two items"
    plan$out$uncertainty$reason <- plan$out$status$message
    return(plan)
  }
  bootstrap <- method %in% c("percentile", "bca", "bootstrap_se_logistic")
  if (bootstrap && is.null(rng)) rng <- sieve_draw_seed(spec$seed)
  plan$out$uncertainty$rng <- rng
  args <- list(
    estimator = "analytic", missing = spec$missing, ci_method = method,
    conf_level = spec$conf_level
  )
  if (length(spec$aux)) args$aux <- spec$aux
  if (bootstrap) args <- c(args, list(B = spec$B, seed = rng$seed, ncpus = as.integer(ncpus)))
  plan$out$code <- sieve_reliability_alpha_code(spec, args)
  prepared <- sieve_capture(sieve_reliability_data(data, unique(c(spec$items, spec$aux))))
  if (!prepared$ok) {
    plan$out$status$message <- prepared$error
    plan$out$uncertainty$status <- "failed"
    plan$out$uncertainty$reason <- prepared$error
    return(plan)
  }
  plan$X <- prepared$value
  plan$bootstrap <- bootstrap
  plan$args <- args
  plan$call <- list(fun = "reliability_alpha", args = c(list(data = plan$X), args))
  plan
}

#' The interval label of an alpha method
#'
#' @param method DMAR's `ci_method` for alpha (possibly with a fallback
#'   reason in parentheses).
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_reliability_alpha_label <- function(method) {
  switch(sub("[ (].*$", "", method),
    adf = "ADF (delta method; analytic alpha)",
    adf_logistic = "ADF, logit scale (delta method; analytic alpha)",
    ml = "ML (delta method; analytic alpha)",
    ml_logistic = "ML, logit scale (delta method; analytic alpha)",
    bonett = "Bonett (analytic alpha)",
    percentile = "Bootstrap percentile (DMAR; analytic alpha)",
    bca = "Bootstrap BCa (DMAR; analytic alpha)",
    bootstrap_se_logistic = "Bootstrap standard error, logit scale (DMAR; analytic alpha)",
    none = "none requested",
    method
  )
}

#' The scale on which an alpha interval is formed
#'
#' Alpha's record names the scale of its interval, as omega's and H's do,
#' so that the interval can be formed again from the stored estimate and
#' standard error: ADF and ML limits are symmetric about the estimate and
#' clamped into the unit interval by DMAR, the `_logistic` forms are formed
#' on the logit scale, and Bonett's (2002) limits on the ln(1 - alpha)
#' scale, whose standard error DMAR reports carried back to the scale of
#' alpha by the delta method (SE / (1 - alpha)).
#'
#' @param method DMAR's `ci_method` for alpha (possibly with a fallback
#'   reason in parentheses).
#' @return A length one character string, `NA` when no interval is formed.
#' @author Ken Kelley
#' @noRd
sieve_reliability_alpha_transformation <- function(method) {
  code <- sub("[ (].*$", "", method %||% NA_character_)
  if (identical(code, "bonett")) {
    return(paste0(
      "log(1 - alpha) (limits formed on the ln(1 - alpha) scale, whose standard error is ",
      "the reported one divided by 1 - alpha, Bonett, 2002; transformed back and clamped to [0, 1])"
    ))
  }
  shared <- switch(code,
    adf = , ml = "ml",
    adf_logistic = , ml_logistic = "ml_logistic",
    bootstrap_se_logistic = "bootstrap_se_logistic",
    percentile = "percentile",
    bca = "dmar_bca",
    NA_character_
  )
  if (is.null(shared) || is.na(shared)) return(NA_character_)
  sieve_reliability_interval("listwise", shared)$transformation
}

#' DMAR's default interval for analytic alpha, by name
#'
#' What `DMAR::reliability_alpha(estimator = "analytic", ci_method = NULL)`
#' resolves to when `missing` is passed by name, as SIEVER passes it:
#' Bonett's (2002) interval with complete cases (`missing = "listwise"`),
#' and the ML standard error of van Zyl, Neudecker, and Nel (2000) under
#' full information maximum likelihood (`missing = "fiml"`, whether or not
#' a value is missing; checked against DMAR's own resolution, its
#' `ci_method` attribute, in the package tests). The
#' recorded call names it.
#'
#' @param missing The missing data treatment passed to DMAR.
#' @return `"bonett"` or `"ml"`.
#' @author Ken Kelley
#' @noRd
sieve_reliability_alpha_default <- function(missing) {
  if (identical(missing, "fiml")) "ml" else "bonett"
}

#' Alpha's DMAR call as recorded R
#'
#' @param spec A specification from [sieve_reliability_spec()].
#' @param args The call's arguments, without the data.
#' @return A length one character string of R code.
#' @author Ken Kelley
#' @noRd
sieve_reliability_alpha_code <- function(spec, args) {
  paste0(
    sieve_reliability_object("alpha", spec), " <- DMAR::reliability_alpha(data = ",
    sieve_reliability_data_code(spec), "[, ",
    paste(deparse(unique(c(spec$items, spec$aux)), width.cutoff = 500L), collapse = " "),
    ", drop = FALSE], ",
    paste(vapply(names(args), function(nm) {
      paste0(nm, " = ", paste(deparse(args[[nm]], width.cutoff = 500L), collapse = " "))
    }, character(1)), collapse = ", "),
    ")  # ", gsub("[\r\n]", " ", spec$construct), ": analytic alpha, for comparison"
  )
}

#' Read alpha's DMAR call into its record
#'
#' A bootstrap interval that DMAR could not form (or that was stopped)
#' keeps its reason, and alpha is then estimated without an interval.
#'
#' A negative alpha lies outside the range of a reliability coefficient,
#' 0 to 1, and DMAR clamps each limit into that range. When its upper
#' limit is above 0 (alpha just below zero) the
#' interval DMAR returned is kept, completed, with a reason that says the
#' lower limit of 0 is the bound of the range and the interval does not
#' contain the estimate; when both limits are clamped to 0 no interval is
#' reported (status `"unavailable"`), and the reason says so.
#'
#' @param plan A plan from `sieve_reliability_alpha_plan()`.
#' @param run A result of `sieve_dmar_boot_run()` of `plan$call`.
#' @return The alpha record.
#' @author Ken Kelley
#' @noRd
sieve_reliability_alpha_finish <- function(plan, run) {
  out <- plan$out
  out$warnings <- run$warnings
  out$seconds <- run$seconds
  if (!run$ok && plan$method != "none") {
    out$uncertainty$status <- "failed"
    out$uncertainty$reason <- if (identical(sieve_dmar_boot_outcome(run), "error")) run$error else sieve_dmar_boot_failure_text(run)
    out$uncertainty$condition_class <- run$error_class
    args <- plan$args
    args$ci_method <- "none"
    args[c("B", "seed", "ncpus")] <- NULL
    run <- sieve_dmar_boot_run(list(fun = "reliability_alpha", args = c(list(data = plan$X), args)))
  }
  if (!run$ok) {
    out$status$message <- run$error
    return(out)
  }
  sieve_dmar_require_terms(run$value, c("estimate", "se", "ci_lower", "ci_upper"), "Coefficient alpha")
  out$estimate <- sieve_reliability_value(run$value, "estimate")
  out$se <- sieve_reliability_value(run$value, "se")
  out$lower <- sieve_reliability_value(run$value, "ci_lower")
  out$upper <- sieve_reliability_value(run$value, "ci_upper")
  out$n$used <- as.integer(sieve_reliability_value(run$value, "N"))
  actual <- attr(run$value, "ci_method", exact = TRUE) %||% plan$method
  out$uncertainty$method <- actual
  out$uncertainty$label <- sieve_reliability_alpha_label(actual)
  out$uncertainty$transformation <- sieve_reliability_alpha_transformation(actual)
  counts <- attr(run$value, "bootstrap", exact = TRUE)
  if (is.list(counts)) {
    out$uncertainty$engine_bootstrap <- counts
    out$uncertainty$B_attempted <- counts$attempted
    out$uncertainty$B_used <- counts$returned
    out$uncertainty$ncpus <- counts$ncpus
  }
  negative <- is.finite(out$estimate) && out$estimate < 0 &&
    out$uncertainty$status %in% c("requested", "completed") &&
    is.finite(out$lower) && is.finite(out$upper)
  if (negative && out$upper <= 0) {
    # A negative alpha lies outside [0, 1], and DMAR clamps each limit into
    # [0, 1]. Far enough below zero both limits are clamped to 0, and the
    # interval [0, 0] says only that the data support no positive
    # reliability for the items as keyed: no interval is reported, and the
    # reason says so. DMAR's warning, kept in `warnings`, names the likely
    # cause (an item keyed in the opposite direction).
    out$uncertainty$status <- "unavailable"
    out$uncertainty$label <- "none (alpha is negative)"
    out$uncertainty$reason <- sprintf(
      paste0(
        "Alpha is negative (%s), outside the range [0, 1] of a reliability coefficient, so no interval is ",
        "reported: the engine clamps each limit into [0, 1], and here both limits are clamped to 0, an ",
        "interval that does not contain the estimate and says only that the data support no positive ",
        "reliability for the items as keyed."
      ),
      sprintf("%.3f", out$estimate)
    )
    out$lower <- NA_real_
    out$upper <- NA_real_
  } else if (negative && out$lower <= 0) {
    # Just below zero the upper limit is above 0 and informative: the
    # interval DMAR returned is kept, and the reason says from its own
    # limits what the clamped lower limit means.
    out$uncertainty$status <- "completed"
    out$uncertainty$reason <- sprintf(
      paste0(
        "Alpha is negative (%s), outside the range [0, 1] of a reliability coefficient. The engine clamps each ",
        "limit into [0, 1], so the interval %s does not contain the estimate: its lower limit, %s, is the ",
        "bound of that range rather than a limit computed from the data, and its upper limit, %s, is the upper ",
        "confidence limit of the reliability of the total as keyed."
      ),
      sprintf("%.3f", out$estimate), sprintf("[%.3f, %.3f]", out$lower, out$upper),
      sprintf("%.3f", out$lower), sprintf("%.3f", out$upper)
    )
  } else if (is.finite(out$lower) && is.finite(out$upper)) out$uncertainty$status <- "completed"
  else if (out$uncertainty$status == "requested") {
    out$uncertainty$status <- "failed"
    out$uncertainty$reason <- "The engine returned no alpha interval limits."
  }
  out$status <- if (is.finite(out$estimate)) list(code = "ok", message = "Estimated.") else {
    list(code = "failed", message = "The engine returned no estimate.")
  }
  out
}

#' One Construct's Reliability: Omega, H, and (Optionally) Alpha
#'
#' @description
#' Computes the reliability of one construct's composite as the Reliability
#' page does: omega with its interval, coefficient H with the one-factor
#' model's fit, and, on request, alpha and model-implied omega beside it,
#' each with its model, missing-data treatment and interval method recorded.
#' [sieve_analyze()] with `"reliability"` runs it for every construct.
#'
#' @param data A data frame holding the item columns.
#' @param spec A specification from [sieve_reliability_spec()].
#' @param rng Optional seed record shared across constructs (see
#'   `sieve_reliability_omega()`).
#' @param alpha Whether to compute coefficient alpha.
#' @param model_implied Whether to compute model implied omega with DMAR's
#'   default interval (MLR (sandwich) SE, logit-scale limits) beside robust
#'   omega (the default; see `sieve_reliability_model_implied()`).
#' @return A list with `spec`, `omega` (from `sieve_reliability_omega()`),
#'   `H` (from `sieve_reliability_H()`, with the one-factor model's fit in
#'   `fit_summary`), `alpha` (from `sieve_reliability_alpha()`, or `NULL`),
#'   `model_implied` (model implied omega beside robust omega, or `NULL`),
#'   `residuals` (the largest single-factor residual correlation and its
#'   item pair), and `coding` (what the coefficient scores: for
#'   categorical omega, the sum of category ranks, with a note naming the
#'   codes and the coded composite's reliability when the codes do not step
#'   by one shared amount), and `keying` (the items whose loading on the
#'   one-factor model behind H is opposite in sign to the others, each with
#'   its Wald interval at the page's level, or `NULL`: robust and model
#'   implied omega describe
#'   the total as scored, in which such an item subtracts from the common
#'   part, while H, whose optimal weights take each loading's sign, does
#'   not, and the method note says so and names the action).
#' @param meta Optional item ledger (`item`, `reverse`, `rev_lo`, `rev_hi`,
#'   `rev_bounds`), so the coding note of categorical omega names an item's
#'   codes as administered beside its codes as scored and its empty
#'   interior categories against its declared bounds, and the keying check
#'   tells an item already marked reverse keyed (a possible second
#'   reversal) from one that is not.
#' @param workers The processes the bootstraps use (omega's and, when its
#'   interval is a bootstrap, alpha's). `NULL`, the default, uses the
#'   option `sieve.cores` (or the environment variable `SIEVER_CORES`), at
#'   most eight, as the Reliability tab does; when neither is set, the
#'   machine's cores less one, at most eight, and one on Windows, or at most
#'   two under `R CMD check`'s `_R_CHECK_LIMIT_CORES_`. Under a declared cluster
#'   design `NULL` decides from the estimated wait (one process below ten
#'   seconds). The limits, counts and replications do not depend on the
#'   number of workers. For DMAR's bootstrap of independent rows, boot draws
#'   every resample's row indices from the recorded seed in this process
#'   before any replication runs, and the workers only refit the resamples
#'   they are handed, so the same seed gives the same draws for any number of
#'   workers; SIEVER's whole-cluster bootstrap seeds replication b from the
#'   b-th of B seeds drawn from the recorded seed, whichever worker runs it.
#'   Workers are forked (a socket cluster on Windows); in a session that has
#'   loaded processx without the environment variable
#'   `PROCESSX_NOTIFY_OLD_SIGCHLD` the bootstrap of independent rows runs in
#'   one process, and `omega$uncertainty$ncpus` records the number used.
#' @param progress `NULL`, or a function called as each bootstrap runs,
#'   `progress(done, total)`, with the replications attempted so far and B:
#'   omega's bootstrap first, then alpha's (DMAR's hook: after each
#'   replication on one process, after each block of about a fiftieth of B
#'   on several). Returning `FALSE` stops that bootstrap, which then forms
#'   no interval and says it was stopped (the whole-cluster bootstrap
#'   summarizes the replications completed).
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' spec <- sieve_reliability_spec(c("x4", "x5", "x6"), rep("continuous", 3),
#'   construct = "Textual", missing = "fiml", ci_method = "mlr")
#' textual <- sieve_reliability_construct(items, spec)
#' textual$omega[c("estimate", "lower", "upper")]
#' @export
sieve_reliability_construct <- function(data, spec, rng = NULL, alpha = TRUE,
                                        model_implied = TRUE, meta = NULL, workers = NULL, progress = NULL) {
  # The bootstraps' processes, as the pages choose them: sieve_cores() (the
  # option sieve.cores, or SIEVER_CORES) for DMAR's bootstrap of independent
  # rows and for alpha's; for the whole-cluster bootstrap NULL keeps its
  # rule of deciding from the estimated wait (sieve_reliability_boot_workers()).
  workers <- sieve_reliability_workers(workers)
  if (!is.null(progress) && !is.function(progress)) {
    stop("`progress` must be NULL or a function of (done, total).", call. = FALSE)
  }
  dmar_workers <- workers %||% sieve_cores()
  omega <- sieve_reliability_omega(data, spec, rng = rng,
    progress = if (is.function(progress)) function(done, total, ...) progress(done, total),
    workers = if (identical(spec$resampling, "siever")) workers else dmar_workers)
  H <- sieve_reliability_H(data, spec)
  if (!is.null(spec$cluster)) {
    partition <- sieve_capture(sieve_reliability_sampling(data, spec))
    if (partition$ok) omega$uncertainty$sampling <- partition$value else {
      omega$uncertainty$status <- "unavailable"
      omega$uncertainty$reason <- partition$error
      omega$code <- c(sieve_reliability_call_code(spec, ci_method = "none"), paste0("# No bootstrap: ", partition$error))
    }
  }
  list(
    spec = spec,
    omega = omega,
    H = sieve_reliability_cluster_interval(H, spec),
    alpha = if (isTRUE(alpha)) {
      sieve_reliability_cluster_interval(sieve_reliability_alpha(data, spec, rng = rng,
        ncpus = sieve_dmar_boot_ncpus(dmar_workers),
        progress = if (is.function(progress)) function(done, total) progress(done, total)), spec)
    },
    model_implied = if (isTRUE(model_implied)) sieve_reliability_model_implied(data, spec),
    residuals = sieve_reliability_residuals(data, spec,
      given = omega$engine_residual_cor, summary = H$residual_summary
    ),
    coding = sieve_reliability_coding(data, spec, meta = meta, omega = omega),
    keying = sieve_reliability_keying(H, spec, meta = meta)
  )
}

#' The workers argument of the headless reliability services, validated
#'
#' @param workers `NULL`, or a single whole number of at least one.
#' @return `NULL` or a single integer.
#' @author Ken Kelley
#' @noRd
sieve_reliability_workers <- function(workers) {
  if (is.null(workers)) return(NULL)
  if (!is.numeric(workers) || length(workers) != 1L || !is.finite(workers) || workers < 1 ||
    workers != floor(workers)) {
    stop("`workers` must be NULL or a single whole number of at least 1.", call. = FALSE)
  }
  as.integer(workers)
}

#' Items loading opposite in sign to the others of a construct
#'
#' The Reliability tab's keying check, read from the one-factor model behind
#' H: its standardized loadings and their sampling covariance
#' (`H$loadings`, `H$vcov`, lavaan's `vcov.std.all`). The factor's sign is
#' arbitrary (lavaan orients it by the first item), so an item is opposite
#' in sign to the orientation most of the items share
#' (`sieve_keying_orientation()`: the majority sign, the sign of the sum on
#' a tie), not merely below zero; with a reverse keyed item first, every
#' other loading is negative and that item is the one named. Each such
#' loading is given with its Wald interval at the page's level
#' (`spec$conf_level`), and no cutoff is applied: an item whose interval
#' excludes zero (or that has no interval) is named in the method note
#' (`sieve_reliability_keying_text()`), one whose interval covers zero is
#' kept in the record but not named, as its sign is not determined by these
#' data. Robust omega and model implied omega are the reliability of the
#' unit-weighted total as scored, in which such an item subtracts from the
#' common part; H is not affected, because its optimal weights take each
#' loading's sign. The estimate's status is left as it is: the coefficient
#' is correct for the total as scored.
#'
#' @param H A result of `sieve_reliability_H()`.
#' @param spec The construct's specification.
#' @param meta Optional item ledger with `reverse` (the item is marked
#'   reverse keyed and its values are reverse scored).
#' @return `NULL` when no orientation can be read (H has no admissible
#'   loadings), otherwise a list with `items` (a data frame: `item`,
#'   `loading`, `lower`, `upper`, `conf_level`, `marked_reverse`,
#'   `named`, one row per item opposite in sign, possibly none) and
#'   `majority` (the number of items sharing the orientation).
#' @author Ken Kelley
#' @noRd
sieve_reliability_keying <- function(H, spec, meta = NULL) {
  L <- H$loadings
  if (is.null(L) || length(L) < 2L || !all(is.finite(L)) || !is.finite(H$estimate %||% NA_real_)) {
    return(NULL)
  }
  orient <- sieve_keying_orientation(L)
  if (orient == 0) return(NULL)
  opposite <- names(L)[sign(L) == -orient]
  level <- spec$conf_level %||% sieve_default_conf_level()
  se <- if (is.matrix(H$vcov) && all(opposite %in% rownames(H$vcov))) {
    sqrt(pmax(diag(H$vcov)[opposite], 0))
  } else {
    rep(NA_real_, length(opposite))
  }
  z <- stats::qnorm(1 - (1 - level) / 2)
  lower <- unname(L[opposite] - z * se)
  upper <- unname(L[opposite] + z * se)
  covered <- is.finite(lower) & is.finite(upper) & lower <= 0 & upper >= 0
  reverse <- if (!is.null(meta) && "reverse" %in% names(meta)) {
    meta$reverse[match(opposite, meta$item)] %in% TRUE
  } else {
    rep(FALSE, length(opposite))
  }
  list(
    items = data.frame(
      item = opposite, loading = unname(L[opposite]), lower = lower, upper = upper,
      conf_level = rep(level, length(opposite)), marked_reverse = reverse, named = !covered,
      stringsAsFactors = FALSE
    ),
    majority = sum(sign(L) == orient)
  )
}

#' What the keying check says about a construct, in words
#'
#' For each item named by `sieve_reliability_keying()`: its loading and
#' interval, opposite in sign to the other items; that robust omega and
#' model implied omega describe the total as scored, in which the item
#' subtracts from the common part, while H, whose optimal weights take each
#' loading's sign, is unaffected, so the gap between H and omega reflects
#' keying, not weighting; and the action, reverse scoring on the Ingest
#' tab. For an item the ledger already marks as reverse keyed, it says that
#' the values may have been reversed twice.
#'
#' @param result A result from [sieve_reliability_construct()].
#' @return A character vector (empty when nothing is named).
#' @author Ken Kelley
#' @noRd
sieve_reliability_keying_text <- function(result) {
  k <- result$keying
  if (is.null(k) || !NROW(k$items) || !any(k$items$named)) return(character(0))
  it <- k$items[k$items$named, , drop = FALSE]
  num <- function(x) if (length(x) == 1L && is.finite(x)) sprintf("%.3f", x) else "not available"
  limits <- ifelse(is.finite(it$lower) & is.finite(it$upper),
    sprintf(" [%.3f, %.3f]", it$lower, it$upper), " (no interval)")
  level <- sieve_ci_percent(it$conf_level[1])
  construct <- result$spec$construct
  om <- result$omega
  mi <- result$model_implied
  H <- result$H
  fresh <- it[!it$marked_reverse, , drop = FALSE]
  fresh_limits <- limits[!it$marked_reverse]
  out <- character(0)
  if (nrow(fresh)) {
    several <- nrow(fresh) > 1L
    names_text <- sieve_and_text(fresh$item)
    loads <- if (several) {
      sprintf(
        "%s load opposite in sign to the other %d item%s on the one-factor model of %s (%s intervals)",
        sieve_and_text(sprintf("%s (%s%s)", fresh$item, vapply(fresh$loading, num, ""), fresh_limits)),
        k$majority, if (k$majority == 1L) "" else "s", construct, level
      )
    } else {
      sprintf(
        "%s loads %s%s (%s interval) on the one-factor model of %s, opposite in sign to the other %d item%s",
        fresh$item, num(fresh$loading), fresh_limits, level, construct,
        k$majority, if (k$majority == 1L) "" else "s"
      )
    }
    has_mi <- !is.null(mi) && is.finite(mi$estimate %||% NA_real_)
    out <- c(out, paste0(
      loads, ".",
      sprintf(
        " %s, %s,%s %s the reliability of the unit-weighted total as scored, in which %s %s from the common part.",
        om$label, num(om$estimate),
        if (has_mi) sprintf(" and %s, %s,", mi$label, num(mi$estimate)) else "",
        if (has_mi) "are" else "is",
        names_text, if (several) "subtract" else "subtracts"
      ),
      if (is.finite(H$estimate %||% NA_real_)) {
        sprintf(
          " %s, %s, is unaffected because its optimal weights take each loading's sign, so the gap between H and omega here reflects keying, not weighting.",
          H$label %||% "H", num(H$estimate)
        )
      } else {
        ""
      },
      sprintf(
        " If %s %s reverse keyed, set %s Reverse? cell%s to Yes on the Ingest tab and compute again.",
        names_text, if (several) "are" else "is", if (several) "their" else "its", if (several) "s" else ""
      )
    ))
  }
  twice <- it[it$marked_reverse, , drop = FALSE]
  if (nrow(twice)) {
    several <- nrow(twice) > 1L
    out <- c(out, sprintf(
      paste0(
        "%s %s marked reverse keyed and %s reverse scored, yet %s opposite in sign to the other %d item%s ",
        "of %s (%s, %s interval%s): if the file already held %s reverse scored, %s values have been reversed ",
        "twice; record %s on the Ingest tab as already reverse scored."
      ),
      sieve_and_text(twice$item), if (several) "are" else "is", if (several) "were" else "was",
      if (several) "load" else "loads", k$majority, if (k$majority == 1L) "" else "s", construct,
      paste(sprintf("%s %s%s", twice$item, vapply(twice$loading, num, ""), limits[it$marked_reverse]), collapse = "; "),
      level, if (several) "s" else "",
      if (several) "them" else "it", if (several) "their" else "its", if (several) "them" else "it"
    ))
  }
  out
}

#' A plain language label for an uncertainty status
#'
#' @param omega A result from `sieve_reliability_omega()`.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_reliability_interval_label <- function(omega) {
  u <- omega$uncertainty
  switch(u$status,
    completed = if (isFALSE(u$complete)) {
      sprintf("%s (stopped after %d of %d replications)", u$label, u$B_attempted, u$B)
    } else {
      u$label
    },
    pending = sprintf(
      "bootstrap not run yet (%s, %d replications)",
      sieve_reliability_interval(omega$spec$missing, u$requested)$label,
      as.integer(omega$spec$B)
    ),
    unavailable = u$unavailable_text %||% "not available for this coefficient",
    failed = "failed",
    not_requested = "none requested",
    u$status
  )
}

#' A list of words joined with commas and a final conjunction
#'
#' @param x A character vector.
#' @param conjunction `"and"` or `"or"`.
#' @return A length one character string (the serial comma with three or
#'   more).
#' @author Ken Kelley
#' @noRd
sieve_and_text <- function(x, conjunction = "and") {
  x <- as.character(x)
  n <- length(x)
  if (n <= 1L) return(paste(x, collapse = ""))
  if (n == 2L) return(paste(x, collapse = sprintf(" %s ", conjunction)))
  paste0(paste(x[-n], collapse = ", "), sprintf(", %s ", conjunction), x[n])
}

#' Whether category codes step by one shared amount
#'
#' The sum of the codes and the sum of consecutive category ranks (each
#' item's observed categories scored 0, 1, ..., C - 1 in the order of its
#' codes) are one composite up to a linear rescaling, and so have the same
#' reliability, exactly when every increment between adjacent observed
#' codes, on every item, equals one common step. A shift of any item, the
#' direction of its scoring (a reversed item's codes are sorted the same
#' way), and the number of categories of each item leave this unchanged;
#' an unused category between two used ones, a missing-value code such as
#' 9 beside 1, 2, 3, codes such as 1, 2, 5, 10, and a change of step on one
#' item break it. This is the rule of DMAR's coding message in
#' `DMAR::reliability_omega_categorical()`.
#'
#' @param steps A list of increments between adjacent sorted codes, one
#'   vector per item.
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_coding_shared <- function(steps) {
  d <- unlist(steps, use.names = FALSE)
  if (!length(d)) return(TRUE)
  all(abs(d - d[1L]) <= sqrt(.Machine$double.eps) * max(1, abs(d[1L])))
}

#' The items whose codes leave the most common step
#'
#' @param steps As for `sieve_coding_shared()`.
#' @return A logical vector, one element per item, `TRUE` for an item with
#'   an increment other than the most common one (the smallest of tied
#'   steps), with that step as the attribute `common`.
#' @author Ken Kelley
#' @noRd
sieve_coding_flagged <- function(steps) {
  d <- unlist(steps, use.names = FALSE)
  if (!length(d)) return(structure(rep(FALSE, length(steps)), common = NA_real_))
  counts <- table(signif(d, 12))
  common <- as.numeric(names(counts)[which.max(counts)])
  tol <- sqrt(.Machine$double.eps) * max(1, abs(common))
  structure(vapply(steps, function(s) any(abs(s - common) > tol), logical(1)), common = common)
}

#' Items that share a description, named together
#'
#' @param items Item names.
#' @param text One description per item (what follows the verb).
#' @param verb The participle, for example `"coded"` or `"scored"`.
#' @return One clause per distinct description, joined by semicolons.
#' @author Ken Kelley
#' @noRd
sieve_coding_groups <- function(items, text, verb = "coded") {
  if (!length(items)) return("")
  keys <- unique(text)
  paste(vapply(keys, function(k) {
    who <- items[text == k]
    if (length(who) == 1L) {
      sprintf("%s is %s %s", who, verb, k)
    } else {
      sprintf("%s are each %s %s", sieve_and_text(who), verb, k)
    }
  }, character(1)), collapse = "; ")
}

#' The categorical omega of the codes as scored, beside the rank coefficient
#'
#' `DMAR::reliability_omega_categorical()` makes each item an ordered
#' factor of the codes observed in the rows it analyzes and scores the
#' categories as consecutive ranks, 0, 1, ..., C - 1 in the order of the
#' codes (Green & Yang, 2009, Eq. 11; Kelley & Pornprasertmanit, 2016), so
#' its coefficient is the reliability of the sum of the ranks. SIEVER's
#' scores use the codes. This refits the same one-factor model of the
#' latent responses that DMAR fits (the same syntax, rows and estimator;
#' DMAR returns neither the loadings nor the thresholds) and assembles
#' Green and Yang's coefficient twice with `sieve_green_yang()`: with unit
#' step weights, which must reproduce DMAR's coefficient, and with the
#' increments between adjacent codes as the step weights, the coefficient
#' of the sum of the codes as scored, with the denominator the
#' specification names. The coded value is returned only when the rank
#' assembly reproduces DMAR's coefficient to 1e-6.
#'
#' @param X The complete rows of the items (numeric codes), as DMAR
#'   analyzes them.
#' @param denominator `"observed"` or `"model_implied"`.
#' @param steps The increments between adjacent sorted codes, one vector per
#'   item.
#' @return A list with `coded`, `rank`, and `error` (why there is no value,
#'   or `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_reliability_coded_estimate <- function(X, denominator, steps) {
  var_names <- paste0("y", seq_along(X))
  d <- as.data.frame(lapply(X, ordered))
  names(d) <- var_names
  model <- paste0(
    "f1 =~ NA*", var_names[1L], " + ",
    paste(paste0("a", seq_along(var_names), "*", var_names), collapse = " + "),
    "\nf1 ~~ 1*f1\n"
  )
  run <- sieve_capture(lavaan::cfa(model,
    data = d, se = "none", test = "none",
    ordered = var_names, missing = "listwise"
  ))
  if (!run$ok) return(list(coded = NA_real_, rank = NA_real_, error = run$error))
  fit <- run$value
  if (!isTRUE(lavaan::lavInspect(fit, "converged"))) {
    return(list(coded = NA_real_, rank = NA_real_, error = "the one-factor model did not converge"))
  }
  est <- lavaan::lavInspect(fit, "est")
  lambda <- est$lambda[var_names, , drop = FALSE]
  implied <- lavaan::lavInspect(fit, "implied")$cov[var_names, var_names, drop = FALSE]
  inv_sd <- 1 / sqrt(diag(implied))
  true_cor <- diag(inv_sd) %*% (lambda %*% est$psi %*% t(lambda)) %*% diag(inv_sd)
  total_cor <- if (identical(denominator, "model_implied")) {
    diag(inv_sd) %*% implied %*% diag(inv_sd)
  } else {
    stats::cov2cor(lavaan::lavInspect(fit, "sampstat")$cov[var_names, var_names, drop = FALSE])
  }
  th <- lavaan::lavInspect(fit, "th")
  th_item <- sub("[|].*$", "", names(th))
  thresholds <- lapply(var_names, function(v) as.numeric(th[th_item == v]))
  if (any(lengths(thresholds) != lengths(steps))) {
    return(list(coded = NA_real_, rank = NA_real_, error = "the fitted thresholds do not match the observed codes"))
  }
  weights <- rep(1, length(var_names))
  list(
    coded = sieve_green_yang(true_cor, total_cor, thresholds, weights, steps),
    rank = sieve_green_yang(true_cor, total_cor, thresholds, weights),
    error = NULL
  )
}

#' What a construct's coefficient scores, and the codes it scores
#'
#' The score target of a reliability coefficient and, for categorical
#' omega, whether it describes the scores SIEVER exports. Categorical omega
#' (DMAR) is the reliability of the sum of category ranks (each item's
#' observed categories scored 0, 1, ..., C - 1 in the order of its codes);
#' SIEVER's scores use the codes. The two composites have the same
#' reliability exactly when every item's codes step by one shared amount
#' (any shift of an item, and either direction, allowed; see
#' `sieve_coding_shared()`). When they do not, the coding note names each
#' item whose codes leave the common step, its codes as scored and, for a
#' reversed item, as administered (the reflection within the bounds the
#' ledger records), any empty interior category against the item's
#' declared bounds, how its categories count in the coefficient and in the
#' score, and SIEVER's point estimate of the coded composite's reliability
#' under the same fitted model (`sieve_reliability_coded_estimate()`): DMAR
#' forms intervals for the rank composite only. The observed codes are read
#' from the complete rows of the items, the rows DMAR analyzes.
#'
#' Bootstrap replications re-rank the categories each resample holds, so a
#' replication that omits a sparse interior category estimates the
#' reliability of a different composite. For an interval resampled from
#' the rows, the expected share of replications that omit a category held
#' by `n_c` of `N` rows is `(1 - n_c / N)^N`, about `exp(-n_c)`; for whole
#' clusters drawn with replacement it is `(1 - m_c / M)^M`, with `m_c` of
#' the `M` clusters holding the category. Interior categories whose share
#' is at least 1 percent are listed in `sparse` and, when the coefficient's
#' interval is a bootstrap, named in `sparse_note`.
#'
#' @param data A data frame holding the item columns, as scored.
#' @param spec A specification from [sieve_reliability_spec()].
#' @param meta Optional item ledger (`item`, `reverse`, `rev_lo`, `rev_hi`,
#'   `rev_bounds`): the declared bounds and the reversal of each item.
#' @param omega Optional result of `sieve_reliability_omega()` for the same
#'   construct: its estimate is the rank coefficient the coded value is set
#'   beside.
#' @param estimate Whether to compute the coded composite's reliability
#'   (one further fit of the one-factor model, only when the codes do not
#'   step by one shared amount).
#' @return A list with `score_target` (`"sum of category ranks"` for
#'   categorical omega, `"sum of item codes"` for omega of a construct
#'   mixing ordered and continuous items, `"sum of item scores"`
#'   otherwise), `categorical`, `n` (the complete rows), `items` (a data
#'   frame with `item`, `codes` (as scored), `administered` (a reversed
#'   item's codes as administered), `empty` (interior categories of the
#'   declared bounds no respondent chose), `outside` (codes outside the
#'   declared bounds), `flagged`), `shared`,
#'   `step` (the most common step), `note` (`NA` or the coding note),
#'   `coded_estimate` (the coded composite's reliability: the coefficient
#'   itself when the codes share one step, SIEVER's point estimate when
#'   they do not, `NA` for other coefficients), `rank_estimate` (SIEVER's
#'   rank assembly on the same fit), `estimate_reason`, `sparse` (a data
#'   frame with `item`, `code`, `administered`, `n`, `share`) and
#'   `sparse_note` (`NA` or the note).
#' @author Ken Kelley
#' @noRd
sieve_reliability_coding <- function(data, spec, meta = NULL, omega = NULL, estimate = TRUE) {
  items <- as.character(spec$items)
  categorical <- identical(spec$coefficient, "omega_categorical")
  item_class <- spec$item_class %||% "continuous"
  out <- list(
    score_target = if (categorical) {
      "sum of category ranks"
    } else if (identical(item_class, "mixed")) {
      "sum of item codes"
    } else {
      "sum of item scores"
    },
    categorical = categorical, n = NA_integer_, items = NULL, shared = NA, step = NA_real_,
    note = NA_character_, coded_estimate = NA_real_, rank_estimate = NA_real_,
    estimate_reason = NULL, sparse = NULL, sparse_note = NA_character_
  )
  if (!categorical) return(out)
  prepared <- sieve_capture(sieve_reliability_data(data, items))
  if (!prepared$ok) return(out)
  keep <- stats::complete.cases(prepared$value)
  X <- prepared$value[keep, , drop = FALSE]
  out$n <- nrow(X)
  if (nrow(X) < 2L) return(out)
  num <- function(x) paste(sieve_num_text(x), collapse = ", ")
  codes <- lapply(X, function(x) sort(unique(x)))
  steps <- lapply(codes, diff)
  out$shared <- sieve_coding_shared(steps)
  flagged <- sieve_coding_flagged(steps)
  out$step <- attr(flagged, "common")
  ledger <- if (is.data.frame(meta) && "item" %in% names(meta)) sieve_meta_keying(meta) else NULL
  whole <- function(x) all(is.finite(x)) && all(abs(x - round(x)) < 1e-8)
  # The item as administered: a reversed item is the reflection of the
  # administered values within the bounds its reversal recorded.
  info <- lapply(seq_along(items), function(j) {
    r <- if (is.null(ledger)) NULL else ledger[match(items[j], ledger$item), , drop = FALSE]
    has_row <- !is.null(r) && nrow(r) == 1L && !is.na(r$item)
    lo <- if (has_row) suppressWarnings(as.numeric(r$rev_lo)) else NA_real_
    hi <- if (has_row) suppressWarnings(as.numeric(r$rev_hi)) else NA_real_
    bounded <- is.finite(lo) && is.finite(hi) && lo < hi
    declared <- bounded && identical(as.character(r$rev_bounds), "declared")
    reversed <- has_row && isTRUE(as.logical(r$reverse))
    administered <- if (reversed && bounded) sort(lo + hi - codes[[j]]) else codes[[j]]
    # Interior categories of the declared scale that no respondent chose,
    # between the lowest and the highest observed category, and observed
    # codes the declared bounds do not include (a missing-value code).
    inside <- administered[administered >= lo & administered <= hi]
    empty <- if (declared && whole(c(lo, hi, administered)) && length(inside) >= 2L) {
      setdiff(seq(min(inside), max(inside)), inside)
    } else {
      numeric(0)
    }
    outside <- if (declared) administered[administered < lo | administered > hi] else numeric(0)
    list(lo = lo, hi = hi, bounded = bounded, declared = declared, reversed = reversed,
      administered = administered, empty = empty, outside = outside)
  })
  out$items <- data.frame(
    item = items,
    codes = vapply(codes, num, character(1)),
    administered = vapply(info, function(x) if (x$reversed && x$bounded) num(x$administered) else NA_character_, character(1)),
    empty = vapply(info, function(x) if (length(x$empty)) num(x$empty) else NA_character_, character(1)),
    outside = vapply(info, function(x) if (length(x$outside)) num(x$outside) else NA_character_, character(1)),
    flagged = if (out$shared) rep(FALSE, length(items)) else as.vector(flagged),
    stringsAsFactors = FALSE
  )
  rank_value <- omega$estimate %||% NA_real_
  if (isTRUE(out$shared)) {
    # One shared step: the sum of the codes is a rescaling of the sum of the
    # ranks, so the coefficient describes the scores as well.
    out$coded_estimate <- rank_value
  } else {
    shown <- which(flagged)
    if (!length(shown)) shown <- seq_along(items)
    describe <- vapply(shown, function(j) {
      x <- info[[j]]
      within <- c(
        if (x$reversed && x$bounded) {
          sprintf(
            "administered %s, scored %s after reversal within %s to %s",
            num(x$administered), num(x$lo + x$hi - x$administered), sieve_num_text(x$lo), sieve_num_text(x$hi)
          )
        } else if (x$reversed) {
          "recorded as reverse scored, with the bounds of its reversal not recorded"
        },
        if (length(x$empty)) {
          sprintf(
            "no respondent chose %s of the declared %s to %s",
            sieve_and_text(sieve_num_text(x$empty), "or"), sieve_num_text(x$lo), sieve_num_text(x$hi)
          )
        },
        if (length(x$outside)) {
          sprintf(
            "%s %s outside the declared %s to %s",
            sieve_and_text(sieve_num_text(x$outside)), if (length(x$outside) == 1L) "lies" else "lie",
            sieve_num_text(x$lo), sieve_num_text(x$hi)
          )
        }
      )
      sprintf(
        "%s%s, so %%s categories count %s in the coefficient and %s in the score",
        num(codes[[j]]), if (length(within)) sprintf(" (%s)", paste(within, collapse = "; ")) else "",
        num(seq_along(codes[[j]]) - 1L), num(codes[[j]] - min(codes[[j]]))
      )
    }, character(1))
    keys <- unique(describe)
    clauses <- vapply(keys, function(k) {
      who <- items[shown][describe == k]
      if (length(who) == 1L) {
        sprintf("%s is scored %s", who, sprintf(k, "its"))
      } else {
        sprintf("%s are each scored %s", sieve_and_text(who), sprintf(k, "their"))
      }
    }, character(1))
    others <- if (length(shown) < length(items) && is.finite(out$step)) {
      sprintf("; the other items step by %s", sieve_num_text(out$step))
    } else {
      ""
    }
    value <- if (isTRUE(estimate) && is.finite(rank_value)) {
      sieve_reliability_coded_estimate(X, spec$denominator %||% "observed", steps)
    } else {
      NULL
    }
    if (!is.null(value) && is.null(value$error) && abs(value$rank - rank_value) > 1e-6) {
      value$error <- sprintf(
        "SIEVER's assembly on the refitted model gave %s for the rank composite, not DMAR's %s",
        format(value$rank, digits = 10), format(rank_value, digits = 10)
      )
    }
    if (!is.null(value) && is.null(value$error)) {
      out$coded_estimate <- value$coded
      out$rank_estimate <- value$rank
    } else if (!is.null(value)) {
      out$estimate_reason <- value$error
    }
    dmar <- sieve_reliability_versions()$DMAR
    tail <- if (is.finite(out$coded_estimate)) {
      sprintf(
        paste0(
          " Under the same fitted model the coded composite's reliability is %.3f against %.3f (a point ",
          "estimate from SIEVER's Green and Yang assembly; DMAR %s forms intervals for the rank composite only)."
        ),
        out$coded_estimate, rank_value, dmar
      )
    } else if (!is.null(out$estimate_reason)) {
      sprintf(
        " SIEVER could not compute the coded composite's reliability under the same model: %s.",
        sub("[.]+$", "", gsub("\\s+", " ", out$estimate_reason))
      )
    } else if (!isTRUE(estimate)) {
      " The Reliability tab gives the coded composite's reliability under the same model beside the coefficient."
    } else {
      ""
    }
    out$note <- paste0(
      "Categorical omega is the reliability of the sum of category ranks; the scores SIEVER exports use ",
      "the codes, and for these items the two composites differ: ", paste(clauses, collapse = "; "), others,
      ".", tail
    )
  }
  # Sparse interior categories, which a bootstrap replication can omit.
  cluster <- spec$cluster
  if (!is.null(cluster) && length(cluster) == length(keep)) cluster <- cluster[keep] else cluster <- NULL
  rows <- lapply(seq_along(items), function(j) {
    cc <- codes[[j]]
    if (length(cc) < 3L) return(NULL)
    inner <- cc[-c(1L, length(cc))]
    n_c <- vapply(inner, function(v) sum(X[[j]] == v), numeric(1))
    share <- if (is.null(cluster)) {
      (1 - n_c / nrow(X))^nrow(X)
    } else {
      M <- length(unique(cluster))
      m_c <- vapply(inner, function(v) length(unique(cluster[X[[j]] == v])), numeric(1))
      (1 - m_c / M)^M
    }
    x <- info[[j]]
    data.frame(
      item = items[j], code = inner,
      administered = if (x$reversed && x$bounded) x$lo + x$hi - inner else NA_real_,
      n = as.integer(n_c), share = share, stringsAsFactors = FALSE
    )
  })
  sparse <- do.call(rbind, rows)
  if (!is.null(sparse)) sparse <- sparse[sparse$share >= 0.01, , drop = FALSE]
  if (!is.null(sparse) && nrow(sparse)) {
    rownames(sparse) <- NULL
    out$sparse <- sparse
    target <- spec$ci_target %||% spec$ci_method %||% "none"
    if (target %in% sieve_reliability_bootstrap_methods()) {
      pct <- function(p) if (p >= .005) sprintf("%.0f%%", 100 * p) else sprintf("%.1f%%", 100 * p)
      parts <- vapply(seq_len(nrow(sparse)), function(i) {
        s <- sparse[i, ]
        sprintf(
          "in %s, category %s%s has %d %s of %d (about %s of replications omit it, %s)",
          s$item, sieve_num_text(s$code),
          if (is.finite(s$administered)) sprintf(" as scored (%s as administered)", sieve_num_text(s$administered)) else "",
          s$n, if (s$n == 1L) "respondent" else "respondents", nrow(X), pct(s$share),
          if (is.null(cluster)) {
            sprintf("(1 - %d/%d)^%d", s$n, nrow(X), nrow(X))
          } else {
            "whole clusters drawn with replacement"
          }
        )
      }, character(1))
      out$sparse_note <- paste0(
        "Bootstrap replications can omit a sparse interior category: ", paste(parts, collapse = "; "),
        ". DMAR ", sieve_reliability_versions()$DMAR, " ranks the categories each replication holds, so in a ",
        "replication that omits one the categories above it move down a rank and its coefficient describes ",
        "a different composite; the interval then mixes the composites of those replications with this one."
      )
    }
  }
  out
}

#' The engine's messages about a reliability coefficient, as reported
#'
#' Every message the engine gave while estimating a construct's coefficient
#' and while forming its interval, with DMAR's message on the coding of the
#' categories left out when SIEVER's own coding note (see
#' `sieve_reliability_coding()`) says the same thing with the codes as
#' administered and the coded composite's reliability.
#'
#' @param result A result from [sieve_reliability_construct()].
#' @return A list with `estimate` and `interval` (character vectors).
#' @author Ken Kelley
#' @noRd
sieve_reliability_engine_messages <- function(result) {
  om <- result$omega
  coded <- !is.na(result$coding$note %||% NA_character_)
  keep <- function(x) {
    x <- unique(gsub("\\s+", " ", as.character(x %||% character(0))))
    if (coded) x <- x[!grepl("^Categorical omega scores the categories of each item as consecutive ranks", x)]
    x
  }
  estimate <- keep(om$messages)
  list(estimate = estimate, interval = setdiff(keep(om$uncertainty$messages), estimate))
}

#' The Reliability Table, at Full Precision
#'
#' One row per construct. Nothing is rounded here; rounding belongs to the
#' renderer.
#'
#' @param results A list of results from [sieve_reliability_construct()].
#' @return A data frame with `construct`, `k`, `coefficient` (the label of
#'   the coefficient computed), `omega`, `ci_lower`, `ci_upper`,
#'   `conf_level` (the level of the interval),
#'   `interval` (the interval method used, or why there is none),
#'   `uncertainty` (the status code), `missing` (the treatment used),
#'   `n_used`, `n_rows`, `n_observed`, `n_complete`, `n_patterns`, `H`,
#'   `H_lower`, `H_upper`, `H_label` (H with its target), `H_target`,
#'   `H_status` (the status code of the
#'   one factor model behind H), `boot_attempted` and `boot_used` (the
#'   replications of a bootstrap, `NA` when there was none or the engine
#'   does not report them), `model_omega`, `model_lower`, `model_upper`,
#'   `model_interval`, and `model_status` (model implied omega beside
#'   robust omega, `NA` where it does not apply), `rmsea`, `rmsea_lower`,
#'   `rmsea_upper`, `rmsea_level`, `rmsea_family`, and `rmsea_text` (the
#'   one-factor model's RMSEA and its interval), `resid_largest`,
#'   `resid_pair`, and `resid_source` (the largest single-factor residual
#'   correlation, its item pair, and where it came from),
#'   `omega_minus_model` (robust minus model-implied omega) and
#'   `sum_residual_cov` (the summed residual covariance 1'(S - Sigma_hat)1 of
#'   the one-factor model, continuous items), `score_target` (what the
#'   coefficient scores: `"sum of category ranks"` for categorical omega,
#'   `"sum of item codes"` for a construct mixing ordered and continuous
#'   items, `"sum of item scores"` otherwise), `coding_note` (`NA`, or for
#'   categorical omega whose codes do not step by one shared amount on
#'   every item the note naming the items, their codes as administered and
#'   as scored, empty interior categories, and the coded composite's
#'   reliability), `coded_estimate` (the reliability of the sum of the
#'   codes as scored under the same fitted model: the coefficient itself
#'   when the codes share one step, SIEVER's point estimate when they do
#'   not, `NA` for coefficients already computed on the item scores),
#'   `keying_items` (the items whose loading is opposite in sign to the
#'   others with an interval that excludes zero, or `NA`), and, when alpha was computed,
#'   `alpha` with `alpha_lower` and `alpha_upper` (for a negative alpha,
#'   DMAR's limits clamped into the range 0 to 1 when the upper one is
#'   above 0, and `NA` when both are clamped to 0), `alpha_interval`,
#'   `alpha_uncertainty`, `alpha_n` and `alpha_status`.
#' @author Ken Kelley
#' @examples
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
#' constructs <- list(Visual = paste0("x", 1:3), Textual = paste0("x", 4:6))
#' results <- lapply(names(constructs), function(name) {
#'   spec <- sieve_reliability_spec(constructs[[name]], rep("continuous", 3),
#'     construct = name, missing = "fiml", ci_method = "mlr")
#'   sieve_reliability_construct(items, spec)
#' })
#' sieve_reliability_table(results)[, c("construct", "k", "omega", "ci_lower", "ci_upper", "alpha")]
#' @export
sieve_reliability_table <- function(results) {
  rows <- lapply(results, function(r) {
    om <- r$omega
    row <- data.frame(
      construct = om$construct,
      group_id = sieve_reliability_id(r$spec), scope = r$spec$scope %||% "construct",
      parent_construct = r$spec$parent_construct %||% om$construct,
      facet = r$spec$facet %||% NA_character_,
      k = as.integer(om$k),
      coefficient = om$label,
      omega = om$estimate,
      ci_lower = om$lower,
      ci_upper = om$upper,
      conf_level = om$conf_level %||% NA_real_,
      interval = sieve_reliability_interval_label(om),
      uncertainty = om$uncertainty$status,
      missing = if (identical(om$missing$used, "fiml")) "FIML" else "complete cases",
      n_used = om$n$used,
      n_rows = om$n$rows,
      n_observed = om$n$observed,
      n_complete = om$n$complete,
      n_patterns = om$n$patterns,
      H = r$H$estimate,
      H_lower = r$H$lower,
      H_upper = r$H$upper,
      H_label = r$H$label %||% "H",
      H_target = r$H$target,
      H_status = r$H$status$code,
      boot_attempted = om$uncertainty$B_attempted %||% NA_integer_,
      boot_used = om$uncertainty$B_used %||% NA_integer_,
      stringsAsFactors = FALSE
    )
    # Model implied omega beside robust omega, and the fit of
    # the one-factor model it assumes.
    mi <- r$model_implied
    row$model_omega <- if (is.null(mi)) NA_real_ else mi$estimate
    row$model_lower <- if (is.null(mi)) NA_real_ else mi$lower
    row$model_upper <- if (is.null(mi)) NA_real_ else mi$upper
    row$model_interval <- if (is.null(mi)) {
      if (identical(om$coefficient, "omega_robust")) {
        "not computed"
      } else {
        "not applicable"
      }
    } else {
      sieve_reliability_interval_label(mi)
    }
    row$model_status <- if (is.null(mi)) NA_character_ else mi$status$code
    # The largest single-factor residual correlation and its item pair,
    # beside the coefficient.
    rc <- r$residuals
    row$resid_largest <- if (is.null(rc) || isTRUE(rc$saturated)) NA_real_ else rc$largest
    row$resid_pair <- if (is.null(rc) || !is.finite(rc$largest %||% NA_real_)) {
      NA_character_
    } else if (isTRUE(rc$saturated)) {
      "not interpreted: three-item residual diagnostics"
    } else {
      sprintf("%s with %s", rc$items[1], rc$items[2])
    }
    row$resid_source <- if (is.null(rc)) NA_character_ else rc$source
    # The denominator diagnostic (an optional column on the tab): robust
    # minus model-implied omega, and the summed residual covariance
    # 1'(S - Sigma_hat)1 by which the observed denominator exceeds the
    # model-implied one (continuous items; the two denominators differ by
    # exactly this amount).
    row$omega_minus_model <- if (!is.null(mi) && identical(om$spec$denominator, "observed") &&
      is.finite(om$estimate) && is.finite(mi$estimate)) {
      om$estimate - mi$estimate
    } else {
      NA_real_
    }
    row$sum_residual_cov <- if (is.null(rc)) NA_real_ else rc$sum_residual_cov %||% NA_real_
    # What the coefficient scores (sieve_reliability_coding()): the sum of
    # category ranks for categorical omega, with the coding note and the
    # coded composite's reliability when the codes do not step by one
    # shared amount.
    coding <- r$coding %||% sieve_reliability_coding(NULL, r$spec)
    row$score_target <- coding$score_target
    row$coding_note <- coding$note %||% NA_character_
    row$coded_estimate <- if (isTRUE(coding$shared)) om$estimate else coding$coded_estimate %||% NA_real_
    keyed <- r$keying$items
    row$keying_items <- if (NROW(keyed) && any(keyed$named)) paste(keyed$item[keyed$named], collapse = ", ") else NA_character_
    fs <- r$H$fit_summary %||%
      sieve_reliability_fit_summary(NULL, conf_level = sieve_default_rmsea_conf_level())
    row$rmsea <- fs$rmsea
    row$rmsea_lower <- fs$lower
    row$rmsea_upper <- fs$upper
    row$rmsea_level <- fs$level
    row$rmsea_family <- fs$family
    row$rmsea_text <- sieve_reliability_rmsea_text(fs)
    if (!is.null(r$alpha)) {
      row$alpha <- r$alpha$estimate
      row$alpha_lower <- r$alpha$lower %||% NA_real_
      row$alpha_upper <- r$alpha$upper %||% NA_real_
      row$alpha_interval <- r$alpha$uncertainty$label %||% "none requested"
      row$alpha_uncertainty <- r$alpha$uncertainty$status %||% "not_requested"
      row$alpha_n <- r$alpha$n$used
      row$alpha_status <- r$alpha$status$message
    }
    row
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' What a construct's row needs said about it
#'
#' Collects, for one construct, every statement the table cannot carry in a
#' cell: a missing data treatment that differs from the request, an
#' interval that is unavailable, pending, or failed (with the reason), an
#' improper solution, the conditions the engines raised, what the engine
#' noted while estimating the coefficient or forming its interval (its
#' messages, see `sieve_reliability_engine_messages()`), the coding note of
#' categorical omega whose codes do not step by one shared amount, and the
#' sparse interior categories a bootstrap of categorical omega can omit
#' (see `sieve_reliability_coding()`).
#'
#' @param result A result from [sieve_reliability_construct()].
#' @return A character vector of plain language statements (possibly
#'   empty).
#' @author Ken Kelley
#' @noRd
sieve_reliability_notes <- function(result) {
  om <- result$omega
  H <- result$H
  notes <- om$spec$notes
  if (!is.null(om$missing$reason)) {
    notes <- c(notes, om$missing$reason)
  }
  if (identical(om$status$code, "failed")) {
    notes <- c(notes, sprintf("The coefficient could not be estimated: %s", om$status$message))
  } else if (identical(om$status$code, "inadmissible")) {
    notes <- c(notes, om$status$message)
  }
  if (om$uncertainty$status %in% c("unavailable", "pending", "failed") &&
    !identical(om$status$code, "failed")) {
    lead <- switch(om$uncertainty$status,
      unavailable = "No interval for \u03c9:",
      pending = "Interval for \u03c9:",
      failed = "The interval for \u03c9 failed:"
    )
    # A refusal that repeats the estimate's own status says only the rest.
    reason <- om$uncertainty$reason %||% ""
    if (identical(om$status$code, "inadmissible") && startsWith(reason, om$status$message)) {
      reason <- trimws(substring(reason, nchar(om$status$message) + 1L))
    }
    notes <- c(notes, paste(lead, reason))
  }
  said <- function(conditions) {
    paste(sprintf("\"%s\"", gsub("\\s+", " ", conditions)), collapse = "; ")
  }
  if (length(om$warnings) && !identical(om$status$code, "inadmissible")) {
    notes <- c(notes, sprintf(
      "While estimating the coefficient the engine warned: %s.",
      said(om$warnings)
    ))
  }
  if (length(om$uncertainty$warnings) && !identical(om$uncertainty$status, "failed")) {
    notes <- c(notes, sprintf(
      "While forming the interval the engine warned: %s.",
      said(om$uncertainty$warnings)
    ))
  }
  # What the engine noted about the coefficient is said where it is
  # reported, and calls for attention like a warning.
  engine <- sieve_reliability_engine_messages(result)
  if (length(engine$estimate)) {
    notes <- c(notes, sprintf("The engine noted: %s.", said(engine$estimate)))
  }
  if (length(engine$interval)) {
    notes <- c(notes, sprintf("While forming the interval the engine noted: %s.", said(engine$interval)))
  }
  coding <- result$coding
  if (!is.na(coding$note %||% NA_character_)) notes <- c(notes, coding$note)
  if (!is.na(coding$sparse_note %||% NA_character_)) notes <- c(notes, coding$sparse_note)
  # An item loading opposite in sign to the others (sieve_reliability_keying()).
  notes <- c(notes, sieve_reliability_keying_text(result))
  if (!is.finite(H$estimate)) {
    notes <- c(notes, sprintf("H could not be estimated: %s", H$error %||% H$status$message))
  } else if (!identical(H$uncertainty$status, "completed")) {
    notes <- c(notes, paste("No interval for H:", H$uncertainty$reason))
  }
  if (!is.null(H$missing$reason) && is.null(om$missing$reason)) {
    notes <- c(notes, H$missing$reason)
  }
  if (!is.null(result$alpha) && !identical(result$alpha$status$code, "ok")) {
    notes <- c(notes, sprintf("Coefficient Alpha, \u03b1, could not be estimated: %s", result$alpha$status$message))
  } else if (!is.null(result$alpha) && length(result$alpha$warnings)) {
    notes <- c(notes, sprintf("While estimating alpha the engine warned: %s.", said(result$alpha$warnings)))
  }
  mi <- result$model_implied
  if (!is.null(mi) && !identical(mi$status$code, "ok")) {
    notes <- c(notes, sprintf(
      "%s, beside the robust omega: %s", mi$label %||% sieve_reliability_label("omega"),
      if (identical(mi$status$code, "failed")) {
        paste("could not be estimated:", mi$status$message)
      } else {
        mi$status$message
      }
    ))
  }
  unique(notes)
}

#' One construct's reliability analysis as a paragraph
#'
#' One paragraph per construct, for the method note under the table: the
#' coefficient with its interval, the interval method and level (or why
#' there is no interval), the model and the engine that produced it, the
#' rows used and the missing data treatment, the status of the solution,
#' the counts of a bootstrap, H with its model and the status of that fit,
#' alpha when computed, whatever the engines warned, and every message the
#' engine attached to the coefficient or its interval ("The engine
#' noted: ..."). For categorical omega it states the composite scored, the
#' sum of category ranks, and when that is the sum of the codes SIEVER
#' scores; when it is not, the coding note of `sieve_reliability_coding()`
#' replaces DMAR's message on the codes, and the paragraph says that alpha
#' describes the sum of the codes. Beside a bootstrap interval of
#' categorical omega it names the sparse interior categories a replication
#' can omit. Numbers are formatted here for reading; the stored results are
#' at full precision.
#'
#' @param result A result from [sieve_reliability_construct()].
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_reliability_paragraph <- function(result) {
  om <- result$omega
  H <- result$H
  u <- om$uncertainty
  num <- function(x) if (length(x) == 1L && is.finite(x)) sprintf("%.3f", x) else "not available"
  ci <- function(l, h) {
    if (length(l) == 1L && length(h) == 1L && is.finite(l) && is.finite(h)) {
      sprintf(" [%.3f, %.3f]", l, h)
    } else {
      ""
    }
  }
  said <- function(conditions) {
    paste(sprintf("\"%s\"", gsub("\\s+", " ", conditions)), collapse = "; ")
  }
  lower_first <- function(x) sub("^([A-Z])", "\\L\\1", sub("\\.$", "", x), perl = TRUE)
  level <- format(om$conf_level * 100)
  rows <- if (length(om$n$used) == 1L && is.finite(om$n$used)) {
    if (identical(om$missing$used, "fiml")) {
      sprintf(
        paste0(
          "%d rows used (full information maximum likelihood; ",
          "%d complete on the included items, %d item-response pattern%s; auxiliary-only rows may contribute when auxiliaries are declared)"
        ),
        om$n$used, om$n$complete, om$n$patterns, if (om$n$patterns == 1L) "" else "s"
      )
    } else if (is.finite(om$n$observed) && om$n$observed > om$n$used) {
      sprintf(
        "%d complete rows (%d rows with an observed item were set aside as incomplete)",
        om$n$used, om$n$observed - om$n$used
      )
    } else {
      sprintf("%d complete rows", om$n$used)
    }
  } else {
    "no usable rows"
  }
  model <- switch(om$coefficient,
    omega = paste0(
      "the one factor model of the item scores, with the total variance the model ",
      "reproduces (model-implied denominator)"
    ),
    omega_robust = paste0(
      "the one factor model of the item scores, with the total ",
      "variance taken from the data (observed denominator: the unrestricted covariance ",
      "matrix of the items)"
    ),
    omega_categorical = paste0(
      "the one factor model of the latent responses (polychoric ",
      "correlations, WLSMV), assembled by Green and Yang's (2009) formula for the sum of the ",
      "items' category ranks (each item's observed categories scored 0, 1, ..., C - 1 in the ",
      "order of its codes), with the denominator built from ",
      if (identical(om$spec$denominator, "model_implied")) {
        "the polychoric correlations the one-factor model implies (Green & Yang, 2009)"
      } else {
        "the unrestricted polychoric correlations (Kelley & Pornprasertmanit, 2016)"
      }
    ),
    "the one factor model"
  )
  requested_label <- sieve_reliability_interval(om$spec$missing, u$requested)$label
  interval <- switch(u$status,
    completed = sprintf("%s%% %s interval%s", level, u$label, if (isFALSE(u$complete)) {
      sprintf(" (stopped after %d of %d replications)", u$B_attempted, u$B)
    } else {
      ""
    }),
    pending = sprintf("interval: bootstrap not run yet (%s selected)", requested_label),
    unavailable = sprintf("no interval: %s", sub("\\.$", "", u$reason)),
    failed = sprintf(
      "the %s interval could not be formed: %s", requested_label,
      sub("\\.$", "", u$reason)
    ),
    not_requested = "no interval requested",
    u$status
  )
  solution <- switch(om$status$code,
    ok = "the solution converged and is admissible",
    inadmissible = paste("the solution is improper:", lower_first(om$status$message)),
    paste("not estimated:", om$status$message)
  )
  first <- if (is.finite(om$estimate)) {
    sprintf(
      "%s %s%s; %s. From %s, fitted by DMAR::%s on %s%s; %s.",
      om$label, num(om$estimate), ci(om$lower, om$upper), interval, model,
      om$spec$engine, rows,
      if (!is.null(om$missing$reason)) sprintf(" (%s)", om$missing$reason) else "",
      solution
    )
  } else {
    sprintf("%s could not be estimated: %s", om$label, om$status$message)
  }
  # The score target of categorical omega, stated in every paragraph.
  coding <- result$coding
  target <- if (identical(om$coefficient, "omega_categorical")) {
    paste0(
      "The sum of the category ranks has the same reliability as the sum of the codes SIEVER scores ",
      "only when every item's codes step by one shared amount.",
      if (isTRUE(coding$shared) && is.finite(coding$step %||% NA_real_)) {
        sprintf(
          " Here every item's codes step by %s, so the coefficient also describes those scores.",
          sieve_num_text(coding$step)
        )
      }
    )
  }
  boot <- if (!is.null(u$counts)) {
    cnt <- u$counts
    words <- c(
      failed = "no estimate returned", not_converged = "did not converge",
      inadmissible = "improper solution", undetermined = "a polychoric correlation undetermined"
    )
    by_outcome <- cnt$outcome[names(words)]
    by_outcome[is.na(by_outcome)] <- 0L
    names(by_outcome) <- names(words)
    by_outcome["inadmissible"] <- cnt$improper_set_aside %||% by_outcome["inadmissible"]
    not_used <- cnt$attempted - cnt$used
    reasons <- if (not_used > 0L) {
      sprintf(" (%s)", paste(sprintf(
        "%s: %d", words[by_outcome > 0L],
        as.integer(by_outcome[by_outcome > 0L])
      ), collapse = "; "))
    } else {
      ""
    }
    paste0(
      if (identical(om$spec$resampling, "dmar")) {
        sprintf(
          "DMAR's counted bootstrap (DMAR::%s, B = %d, %d process%s; improper solutions that still yield a coefficient kept, no interval from fewer than half of the replications). ",
          om$spec$engine, as.integer(u$B), as.integer(u$ncpus %||% 1L),
          if (identical(as.integer(u$ncpus %||% 1L), 1L)) "" else "es"
        )
      },
      sprintf(
        paste0(
          "Replications: %d attempted, %d returned an estimate, %d converged, %d admissible, ",
          "%d used%s%s; seed %d%s."
        ),
        cnt$attempted, cnt$returned, cnt$converged, cnt$admissible, cnt$used, reasons,
        if ((cnt$improper_kept %||% 0L) > 0L) sprintf("; %d improper coefficients retained", cnt$improper_kept) else "",
        u$rng$seed, if (isTRUE(u$rng$supplied)) "" else ", drawn for this run and recorded"
      ),
      if (is.finite(u$mc_se)) {
        sprintf(
          paste0(
            " With %d usable replications each percentile limit carries a Monte Carlo ",
            "standard error of about %.3f (%.2f bootstrap standard errors)."
          ),
          cnt$used, u$mc_se, sieve_bootstrap_mc_error(cnt$used, om$conf_level)
        )
      },
      if (isTRUE(u$caution)) {
        sprintf(
          paste0(
            " %d replications were set aside and %d improper coefficients were retained, ",
            "together at or above the interval's tail area (%s%% of %d). The limits depend ",
            "on how these solutions were treated; replications set aside are not a random subset."
          ),
          not_used, cnt$improper_kept %||% 0L, format(100 * u$caution_share, digits = 3), cnt$attempted
        )
      } else if (not_used > 0L) {
        sprintf(
          paste0(
            " %d of the %d replications could not be used, a share below the tail area of ",
            "the interval (%s%%); the limits describe the resamples in which the model could ",
            "be fitted."
          ),
          not_used, cnt$attempted, format(100 * u$caution_share, digits = 3)
        )
      }
    )
  } else if (identical(u$status, "completed") && identical(om$spec$resampling, "dmar")) {
    counts <- u$engine_bootstrap
    paste0(
      sprintf(
        "The engine ran %s: %d replications requested, seed %d%s.",
        u$label, u$B, u$rng$seed, if (isTRUE(u$rng$supplied)) "" else ", drawn and recorded"
      ),
      if (!is.null(u$method_reason)) paste0(" Actual engine method: ", u$method_reason, "."),
      if (is.list(counts)) {
        paste0(
          sprintf(
            " %d attempted, %d finite estimates used, %d failed.",
            counts$attempted, counts$returned, counts$failed
          ),
          " These counts do not certify convergence or admissibility separately.",
          if (length(counts$reasons)) paste0(
            " Reasons: ",
            paste(names(counts$reasons),
              counts$reasons,
              collapse = "; "
            ),
            "."
          ),
          if (is.finite(u$mc_se)) sprintf(
            " The percentile-limit Monte Carlo standard error is approximately %.3f from %d usable replications.",
            u$mc_se,
            counts$returned
          )
        )
      } else " The engine returned no replication counts."
    )
  }

  h <- if (is.finite(H$estimate)) {
    paste0(
      sprintf(
        "%s %s%s from the one factor model %s (%s, %s, %d rows): %s.",
        H$label, num(H$estimate), ci(H$lower, H$upper), H$model$model, H$model$estimator,
        if (identical(H$model$missing, "fiml")) {
          "full information maximum likelihood"
        } else {
          "complete cases"
        },
        H$n$used, lower_first(H$status$message)
      ),
      if (!identical(H$uncertainty$status, "completed")) {
        sprintf(" No interval for H: %s", H$uncertainty$reason)
      }
    )
  } else {
    sprintf("%s could not be estimated: %s", H$label, H$error %||% H$status$message)
  }
  alpha <- if (!is.null(result$alpha)) {
    if (identical(result$alpha$status$code, "ok")) {
      paste0(
        sprintf(
          "Coefficient Alpha, \u03b1 = %s%s on %d rows, for comparison only.",
          num(result$alpha$estimate),
          ci(
            result$alpha$lower %||% NA_real_,
            result$alpha$upper %||% NA_real_
          ),
          result$alpha$n$used
        ),
        " Interval: ", result$alpha$uncertainty$label %||% "none requested", "; ",
        result$alpha$uncertainty$status %||% "not_requested", ". ", result$alpha$uncertainty$reason %||% "",
        if (length(result$alpha$warnings)) {
          sprintf(" While estimating alpha the engine warned: %s.", said(result$alpha$warnings))
        } else {
          ""
        }
      )
    } else {
      sprintf("Coefficient Alpha, \u03b1, could not be estimated: %s", result$alpha$status$message)
    }
  }
  model_implied <- if (!is.null(result$model_implied)) {
    mi <- result$model_implied
    fs <- H$fit_summary
    paste0(
      if (is.finite(mi$estimate)) {
        sprintf(
          paste0(
            "Beside it, %s %s%s, %s, from the same one-factor model with the total variance ",
            "that model reproduces; it is the reliability of the composite only if that model holds."
          ),
          mi$label, num(mi$estimate), ci(mi$lower, mi$upper),
          if (identical(mi$uncertainty$status, "completed")) {
            sprintf("%s%% %s interval", level, mi$uncertainty$label)
          } else {
            sprintf("no interval (%s)", sub("\\.$", "", mi$uncertainty$reason %||% mi$uncertainty$status))
          }
        )
      } else {
        sprintf("%s could not be estimated: %s.", mi$label, sub("\\.$", "", mi$status$message))
      },
      if (is.finite(mi$estimate) && is.finite(om$estimate) && identical(om$spec$denominator, "observed")) {
        d <- result$residuals$sum_residual_cov %||% NA_real_
        sprintf(
          " Robust minus model-implied omega is %s%s.",
          sprintf("%.3f", om$estimate - mi$estimate),
          if (is.finite(d)) {
            sprintf(
              paste0(
                "; it reflects the summed residual covariance within the composite, 1'(S - \u03a3\u0302)1 = %.3f, ",
                "the amount by which the observed denominator exceeds the model-implied one"
              ),
              d
            )
          } else {
            ""
          }
        )
      },
      if (!is.null(fs)) {
        if (is.finite(fs$rmsea)) {
          sprintf(
            " The one-factor model's RMSEA is %.3f%s (%s family), the evidence on that assumption.",
            fs$rmsea,
            if (is.finite(fs$lower) && is.finite(fs$upper)) {
              sprintf(" with a %s interval [%.3f, %.3f]", sieve_ci_percent(fs$level), fs$lower, fs$upper)
            } else {
              ""
            },
            fs$family
          )
        } else if (!is.null(fs$reason)) {
          paste0(" ", fs$reason)
        }
      }
    )
  }
  # The largest single-factor residual correlation, and what it does and
  # does not show.
  residual <- if (!is.null(result$residuals)) {
    rc <- result$residuals
    if (!is.finite(rc$largest %||% NA_real_)) {
      sprintf(
        "The residual correlations of the one-factor model are not available: %s",
        rc$reason %||% "not computed."
      )
    } else if (isTRUE(rc$saturated)) {
      paste0(
        "Three-item residual diagnostics are not interpreted here. Zero degrees of freedom ",
        "do not provide an ordinary test of local dependence or establish that every residual is zero."
      )
    } else {
      sprintf(
        paste0(
          "Largest residual correlation of the one-factor model: %.3f, %s with %s (%s; from %s). ",
          "A rising omega while a large residual pair is retained signals redundancy, not better ",
          "measurement; a small largest residual does not certify unidimensionality, because shared ",
          "variance on the dominant items is absorbed by the single factor and the misfit appears ",
          "among the other items."
        ),
        rc$largest, rc$items[1], rc$items[2], rc$scale, rc$source
      )
    }
  }
  extras <- c(
    om$spec$notes,
    if (length(om$warnings) && !identical(om$status$code, "inadmissible")) {
      sprintf("While estimating the coefficient the engine warned: %s.", said(om$warnings))
    },
    if (length(u$warnings) && !identical(u$status, "failed")) {
      sprintf("While forming the interval the engine warned: %s.", said(u$warnings))
    },
    if (!is.null(H$missing$reason) && is.null(om$missing$reason)) H$missing$reason,
    # Every message the engine attached to the coefficient or its interval,
    # SIEVER's coding note in place of DMAR's message on the codes, and the
    # sparse categories a bootstrap can omit.
    {
      engine <- sieve_reliability_engine_messages(result)
      c(
        if (length(engine$estimate)) sprintf("The engine noted: %s.", said(engine$estimate)),
        if (length(engine$interval)) sprintf("While forming the interval the engine noted: %s.", said(engine$interval))
      )
    },
    if (!is.na(coding$note %||% NA_character_)) {
      c(
        coding$note,
        if (!is.null(result$alpha) && identical(result$alpha$status$code, "ok")) {
          paste0(
            "Alpha is computed on the item codes as scored, so beside this omega, which scores the ",
            "category ranks, it describes a different composite."
          )
        }
      )
    },
    if (!is.na(coding$sparse_note %||% NA_character_)) coding$sparse_note
  )
  keying <- sieve_reliability_keying_text(result)
  paste(c(first, keying, target, boot, model_implied, residual, h, alpha, unique(extras)), collapse = " ")
}

#' Omega of the unweighted total of every included item
#'
#' The reliability of the total score over all included items, treated as
#' one composite, for the Bifactor tab. It is estimated on the rows the
#' bifactor model uses, so the two are not compared across different
#' samples: full information maximum likelihood when every item is
#' continuous, complete rows when any item is ordered or binary (the
#' bifactor model is then fitted to complete rows). The coefficient follows
#' the item types as on the Reliability tab: robust omega (the observed
#' denominator, which does not lean on the one factor model the Bifactor
#' tab questions) for continuous items, categorical omega for ordered or
#' binary items. It is a point estimate; no interval is formed here.
#'
#' @param data A data frame holding the item columns.
#' @param meta The item ledger.
#' @param estimator One of `sieve_estimator_choices()`, the Bifactor tab's
#'   Estimator control: `"auto"` follows the item types; `"mlr"`,
#'   `"ml_fiml"` and `"ml"` treat every item as continuous (`"ml"` on
#'   complete rows); `"wlsmv"`
#'   treats every item as ordered, as the bifactor model is then fitted.
#' @param aux Continuous auxiliary columns used by the FIML fit.
#' @return A result from `sieve_reliability_omega()`, with `residuals`
#'   (see `sieve_reliability_residuals()`), or `NULL` when no included item
#'   is in the data.
#' @author Ken Kelley
#' @noRd
sieve_total_omega <- function(data, meta, estimator = "auto", aux = NULL) {
  active <- meta[meta$status == "active" & meta$item %in% names(data), , drop = FALSE]
  if (!nrow(active)) {
    return(NULL)
  }
  types <- as.character(active$type)
  resolved <- sieve_estimator_resolve(types, choice = estimator)
  categorical <- resolved$categorical
  # The treatment the bifactor model was fitted under: a binary item stays
  # binary when treated as categorical; any item treated as continuous is
  # continuous here.
  types <- ifelse(categorical, ifelse(types == "binary", "binary", "ordered"), "continuous")
  spec <- sieve_reliability_spec(
    items = active$item, types = types, construct = "Total score",
    missing = if (any(categorical) || identical(resolved$missing, "listwise")) "listwise" else "fiml",
    ci_method = "none",
    denominator = "observed", aux = aux
  )
  out <- sieve_reliability_omega(data, spec)
  # Beside the robust omega of the total, the largest residual correlation
  # of the one-factor model of every item: the structure the bifactor
  # model adds shows there.
  out$residuals <- sieve_reliability_residuals(data, spec, given = out$engine_residual_cor)
  out
}

#' The total score omega, as it is shown
#'
#' @param omega A result from `sieve_total_omega()`, or `NULL`.
#' @return A length one character string: the coefficient's label (which
#'   names its denominator), the estimate to two decimals, and the rows it
#'   rests on; or why there is no estimate.
#' @author Ken Kelley
#' @noRd
sieve_total_omega_label <- function(omega) {
  if (is.null(omega)) {
    return("not available")
  }
  what <- omega$label %||% sieve_reliability_label(omega$coefficient %||% "omega")
  if (!is.finite(omega$estimate)) {
    return(sprintf("%s not available (%s)", what, omega$status$message))
  }
  rows <- if (identical(omega$missing$used, "fiml")) {
    sprintf(
      "full information maximum likelihood on the %d rows with an observed item",
      omega$n$used
    )
  } else {
    sprintf("%d complete rows", omega$n$used)
  }
  residual <- if (!is.null(omega$residuals)) {
    sprintf(
      "; largest residual correlation of the one-factor model of every item: %s",
      sieve_reliability_residual_text(omega$residuals, digits = 2L)
    )
  } else {
    ""
  }
  sprintf(
    "%s = %.2f (point estimate; %s%s)%s", what, omega$estimate, rows, residual,
    if (length(sieve_reliability_outside(omega$estimate, omega$coefficient))) {
      if (omega$estimate > 1) {
        paste0(
          "; inadmissible: above 1, because the one-factor model implies more common ",
          "variance than the observed total score has"
        )
      } else {
        "; inadmissible: below 0"
      }
    } else if (identical(omega$status$code, "inadmissible")) {
      "; the one factor solution behind it is improper"
    } else {
      ""
    }
  )
}

#' Monte Carlo error of a bootstrap percentile limit
#'
#' The standard error, from resampling alone, of a percentile limit read
#' off `B` bootstrap estimates, in units of the bootstrap standard error.
#' It is the large sample standard error of a sample quantile,
#' \eqn{\sqrt{p (1 - p) / B} / \phi(z_p)} with \eqn{p = (1 - C) / 2}, under
#' an approximately normal bootstrap distribution. It quantifies what a
#' smaller `B` costs: the value is about 0.08 at `B = 1000`, 0.19 at
#' `B = 200`, and 0.03 at `B = 10000` for a 95 percent interval. The limits
#' of a BCa interval sit farther into the tails, so their Monte Carlo error
#' is larger than this.
#'
#' @param B Number of bootstrap replications.
#' @param conf_level Confidence level.
#' @return A number: the Monte Carlo standard error of one limit, as a
#'   multiple of the bootstrap standard error.
#' @author Ken Kelley
#' @noRd
sieve_bootstrap_mc_error <- function(B, conf_level = 0.95) {
  p <- (1 - conf_level) / 2
  sqrt(p * (1 - p) / B) / stats::dnorm(stats::qnorm(p))
}

#' Estimate how long a bootstrap will take
#'
#' Each replication refits the model, so the wait is the time of one fit
#' multiplied by the number of fits. The BCa interval adds one fit per
#' case when there are fewer replications than cases (the jackknife behind
#' its acceleration constant).
#'
#' @param seconds_per_fit Time of one fit for each construct, in seconds.
#' @param B Number of bootstrap replications.
#' @param n Number of rows resampled, for each construct.
#' @param method `"percentile"`, `"dmar_percentile"`, `"dmar_bca"`, or the
#'   engine's `"bca"`.
#' @return The estimated wait for each construct, in seconds.
#' @author Ken Kelley
#' @noRd
sieve_bootstrap_wait <- function(seconds_per_fit, B, n, method = "percentile") {
  extra <- if (method %in% c("bca", "dmar_bca")) ifelse(B < n, n, 0) else 0
  seconds_per_fit * (B + extra)
}

#' A duration in plain words
#'
#' @param seconds A duration in seconds.
#' @return A length one character string such as `"about 4 minutes"`.
#' @author Ken Kelley
#' @noRd
sieve_duration_label <- function(seconds) {
  if (!is.finite(seconds)) {
    return("an unknown time")
  }
  if (seconds < 2) return("under 2 seconds")
  if (seconds < 10) return(sprintf("about %d seconds", as.integer(ceiling(seconds))))
  if (seconds < 90) {
    return(sprintf("about %d seconds", as.integer(ceiling(seconds / 5) * 5)))
  }
  minutes <- seconds / 60
  if (minutes < 90) {
    return(sprintf("about %d minutes", as.integer(ceiling(minutes))))
  }
  sprintf("about %.1f hours", minutes / 60)
}

#' Data Expression Including the Declared FIML Auxiliaries
#' @param spec Resolved reliability specification.
#' @param data_name Item data expression in the session script.
#' @return R expression as text.
#' @author Ken Kelley
#' @noRd
sieve_reliability_data_code <- function(spec, data_name = "items") {
  if (!length(spec$aux)) return(data_name)
  sprintf(paste0("(function(x) { missing_aux <- setdiff(%s, names(x)); ",
    "if (length(missing_aux)) cbind(x, d[, missing_aux, drop = FALSE]) else x })(%s)"),
    sieve_r_literal(spec$aux), data_name)
}

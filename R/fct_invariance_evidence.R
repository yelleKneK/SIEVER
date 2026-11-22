#' Resolve the same eligible observation pool for every invariance rung
#' @param items Processed item values.
#' @param group Group labels, one per current row.
#' @param columns Active model item columns.
#' @param categorical Whether the fitted estimator treats items as ordered.
#' @param missing Selected FIML or complete-case rule.
#' @return Data, retained row positions, problems, and row flow.
#' @author Ken Kelley
#' @noRd
sieve_invariance_frame <- function(items, group, columns, categorical = FALSE,
                                   missing = c("fiml", "complete")) {
  missing <- match.arg(missing)
  if (length(group) != nrow(items)) stop("Group labels and items are not aligned.", call. = FALSE)
  read <- sieve_numeric_columns(items, columns)
  complete <- stats::complete.cases(read$data)
  any_response <- rowSums(!is.na(read$data)) > 0L
  known_group <- !is.na(group) & nzchar(trimws(as.character(group)))
  rule <- if (categorical || identical(missing, "complete")) "complete" else "fiml"
  keep <- known_group & if (identical(rule, "complete")) complete else any_response
  list(
    data = read$data[keep, , drop = FALSE], group = droplevels(factor(group[keep])),
    rows = which(keep), missing = if (identical(rule, "fiml")) "fiml" else "listwise",
    problems = read$text,
    row_flow = data.frame(
      input = nrow(items), missing_group = sum(!known_group),
      all_items_missing = sum(known_group & !any_response),
      incomplete_items_removed = if (identical(rule, "complete")) sum(known_group & any_response & !complete) else 0L,
      partial_rows_retained = sum(keep & !complete), eligible = sum(keep)
    ),
    rule = if (identical(
      rule,
      "fiml"
    )) "FIML; observed group and at least one observed model item" else "Complete cases on every model item and the grouping variable"
  )
}

#' Read Actual Latent Location/Scale Constraints and Eligible Mean Contrasts
#'
#' @description
#' Reads the parameter table of a fitted multiple-group lavaan model, finds
#' the items whose measurement parameters are held equal in every group
#' (the anchors), and reports the latent mean difference between each group
#' and the reference group, with a Wald interval, only for the factors whose
#' constraints put the groups on one latent metric. The constraints are
#' read from the fitted model, never from a label or the name of a rung.
#'
#' @details
#' **The reference group** is the first group of the fit,
#' `lavaan::lavInspect(fit, "group.label")[1]`: the first level of the
#' grouping variable as lavaan ordered it (for a factor, its first level;
#' otherwise the first value in the order lavaan met it). Every contrast is
#' that group's mean subtracted from another group's.
#'
#' **An anchor** is an item whose loading and location are equal in every
#' group, either by one shared equality label in every group or by being
#' fixed at the same value. The location is the intercept for a continuous
#' item; for an ordered item it is every threshold and the latent response
#' intercept, the scalar invariance of Wu and Estabrook (2016), under which
#' residual variances (theta parameterization) or scale factors (delta
#' parameterization) may still differ. Equal residuals, the strict
#' invariance of Meredith (1993), are reported (`strict_response_scale_equal`)
#' and not required.
#'
#' **A licensed contrast** (`licensed = TRUE`) needs the reference group's
#' factor mean fixed at 0, the focal group's free, and at least two verified
#' anchors on the factor, the minimum for a partially invariant model to set
#' a common origin and scale (Byrne, Shavelson, & Muthen, 1989). The
#' estimate is the focal mean minus the reference mean; its standard error
#' is `sqrt(a' V a)` from lavaan's covariance matrix of the free parameters
#' (`lavaan::lavInspect(fit, "vcov")`), and the interval is Wald,
#' `estimate +/- z * se` with `z = qnorm((1 + conf_level) / 2)`. With the
#' reference variance fixed at 1 the contrast is in reference-group factor
#' standard deviations; otherwise it is on the fitted reference metric
#' (`metric` says which). An unlicensed row is returned with `NA` values and
#' the reason in `reason`; it is not an error. Whether the anchors truly
#' function alike is a substantive claim the parameter table cannot settle.
#'
#' Missing data were handled by the fit itself. `conf_level` is a
#' proportion strictly between 0 and 1 (0.95, not 95).
#'
#' **Refusals** (an error): an object that is not a lavaan fit or has no
#' admissible solution, and a fit with one group.
#'
#' @param fit Multiple-group lavaan fit.
#' @param conf_level Confidence level of the Wald intervals of the latent
#'   mean contrasts: one number strictly between 0 and 1 (0.95 for a 95
#'   percent interval); anything else is refused before the fit is read.
#' @return A list:
#'   \describe{
#'     \item{`parameters`}{The latent means and variances of every group
#'       from the parameter table: `lhs`, `op`, `rhs`, `group`, `free`
#'       (0 when fixed), `label`, `est`, `se`, `group_label` and `fixed`.}
#'     \item{`anchors`}{One row per factor and item: `factor`, `item`,
#'       `loading_equal`, `location_scale_equal` (intercept, or thresholds
#'       and latent response intercept), `strict_response_scale_equal`
#'       (ordered items only, otherwise `NA`) and `anchor` (loading and
#'       location equal).}
#'     \item{`contrasts`}{One row per factor and non-reference group:
#'       `factor`, `reference`, `focal`, `verified_anchors`, `licensed`,
#'       `estimate` (focal minus reference latent mean), `se`, `lower`,
#'       `upper`, `conf_level`, `metric` and `reason`. On a licensed row `reason`
#'       is the caveat that anchor validity still needs substantive
#'       evidence; on an unlicensed row it says why there is no estimate.}
#'     \item{`categorical`}{Whether the fit treats the items as ordered.}
#'     \item{`note`}{The rule applied, in words.}
#'   }
#' @references
#' Byrne, B. M., Shavelson, R. J., & Muthen, B. (1989). Testing for the
#' equivalence of factor covariance and mean structures: The issue of
#' partial measurement invariance. *Psychological Bulletin, 105*(3),
#' 456--466. \doi{10.1037/0033-2909.105.3.456}
#'
#' Meredith, W. (1993). Measurement invariance, factor analysis and
#' factorial invariance. *Psychometrika, 58*(4), 525--543.
#' \doi{10.1007/BF02294825}
#'
#' Wu, H., & Estabrook, R. (2016). Identification of confirmatory factor
#' analysis models of different levels of invariance for ordered
#' categorical outcomes. *Psychometrika, 81*(4), 1014--1045.
#' \doi{10.1007/s11336-016-9506-0}
#' @seealso [sieve_anchor_sensitivity()] to compare named anchor sets;
#'   [sieve_ordinal_fit_impact()] for the expected-score impact of ordered
#'   items.
#' @author Ken Kelley
#' @examples
#' model <- "visual =~ x1 + x2 + x3
#'           textual =~ x4 + x5 + x6"
#' fit <- lavaan::cfa(model, data = lavaan::HolzingerSwineford1939, group = "school",
#'   group.equal = c("loadings", "intercepts"), std.lv = TRUE)
#' reading <- sieve_invariance_interpret(fit)
#' reading$contrasts
#' @export
sieve_invariance_interpret <- function(fit, conf_level = .95) {
  # Checked first: unchecked, a percent-style 95 gives NaN limits on rows
  # marked licensed, 0 a zero-width interval, 1 infinite limits, a negative
  # level an inverted interval, and two levels a duplicate of every row.
  conf_level <- sieve_check_conf_level(conf_level)
  status <- sieve_fit_status(fit)
  if (!inherits(fit, "lavaan") || !sieve_has_solution(fit)) {
    stop(paste("Select a converged, identified invariance fit.", status$message), call. = FALSE)
  }
  pt <- lavaan::parTable(fit)
  ng <- lavaan::lavInspect(fit, "ngroups")
  groups <- lavaan::lavInspect(fit, "group.label")
  if (ng < 2L) stop("This panel requires a multiple-group fit.", call. = FALSE)
  lv <- lavaan::lavNames(fit, "lv")
  categorical <- sieve_fit_is_categorical(fit)
  equal_rows <- function(lhs, op, rhs) {
    z <- pt[pt$lhs == lhs & pt$op == op & pt$rhs == rhs & pt$group > 0L, , drop = FALSE]
    if (nrow(z) != ng || anyDuplicated(z$group)) return(FALSE)
    identical(length(unique(z$label[nzchar(z$label)])), 1L) && all(nzchar(z$label)) ||
      all(z$free == 0L) && max(z$est) - min(z$est) < 1e-10
  }
  anchors <- lapply(lv, function(f) {
    its <- unique(pt$rhs[pt$lhs == f & pt$op == "=~"])
    do.call(rbind, lapply(its, function(it) {
      load <- equal_rows(f, "=~", it)
      location <- if (categorical && any(pt$lhs == it & pt$op == "|")) {
        th <- unique(pt$rhs[pt$lhs == it & pt$op == "|"])
        # Wu & Estabrook scalar invariance supplies a common factor metric
        # through threshold, loading, and intercept restrictions. Residual
        # variances (theta) or response scale factors (delta) may remain free;
        # equal residuals would impose strict invariance instead. Derived ~~
        # values under delta are not scale-factor constraints.
        all(vapply(th, function(t) equal_rows(it, "|", t), logical(1))) &&
          equal_rows(it, "~1", "")
      } else equal_rows(it, "~1", "")
      parameterization <- lavaan::lavInspect(fit, "options")$parameterization
      data.frame(
        factor = f, item = it, loading_equal = load, location_scale_equal = location,
        strict_response_scale_equal = if (categorical) {
          if (identical(parameterization, "delta")) equal_rows(it, "~*~", it) else equal_rows(it, "~~", it)
        } else NA,
        anchor = load && location, stringsAsFactors = FALSE
      )
    }))
  })
  anchors <- do.call(rbind, anchors)
  selected <- pt$lhs %in% lv & ((pt$op == "~1") | (pt$op == "~~" & pt$lhs == pt$rhs)) & pt$group > 0L
  parameters <- pt[selected, c("lhs", "op", "rhs", "group", "free", "label", "est", "se"), drop = FALSE]
  parameters$group_label <- groups[parameters$group]
  parameters$fixed <- parameters$free == 0L
  V <- lavaan::lavInspect(fit, "vcov")
  z <- stats::qnorm((1 + conf_level) / 2)
  contrasts <- do.call(rbind, lapply(lv, function(f) {
    means <- pt[pt$lhs == f & pt$op == "~1" & pt$group > 0L, , drop = FALSE]
    ref <- means[means$group == 1L, , drop = FALSE]
    n_anchor <- sum(anchors$factor == f & anchors$anchor)
    do.call(rbind, lapply(seq.int(2L, ng), function(g) {
      focal <- means[means$group == g, , drop = FALSE]
      licensed <- nrow(ref) == 1L && nrow(focal) == 1L && ref$free == 0L && abs(ref$est) < 1e-10 && focal$free > 0L && n_anchor >= 2L
      estimate <- lower <- upper <- se <- NA_real_
      if (licensed) {
        a <- numeric(nrow(V))
        a[focal$free] <- 1
        if (ref$free > 0L) a[ref$free] <- a[ref$free] - 1
        estimate <- focal$est - ref$est
        variance <- as.numeric(crossprod(a, V %*% a))
        if (is.finite(variance) && variance >= 0) {
          se <- sqrt(variance)
          lower <- estimate - z * se
          upper <- estimate + z * se
        }
      }
      data.frame(
        factor = f, reference = groups[1], focal = groups[g], verified_anchors = n_anchor,
        licensed = licensed, estimate = estimate, se = se, lower = lower, upper = upper, conf_level = conf_level,
        metric = if (all(pt$free[pt$lhs == f & pt$op == "~~" & pt$rhs == f & pt$group == 1L] == 0L) &&
          all(abs(pt$est[pt$lhs == f & pt$op == "~~" & pt$rhs == f & pt$group == 1L] - 1) < 1e-8)) {
          "latent mean difference in reference-group factor SD units (reference variance fixed at 1)"
        } else "latent mean difference on the fitted reference-group factor metric",
        reason = if (licensed) "Actual loading/location-scale constraints and a free focal mean support this contrast; anchor validity still requires substantive evidence." else "No interval: common origin/scale and a free mean were not verified from the actual parameter table (at least two verified anchors required).",
        stringsAsFactors = FALSE
      )
    }))
  }))
  list(
    parameters = parameters, anchors = anchors, contrasts = contrasts, categorical = categorical,
    note = paste(
      "A model label does not license a mean comparison. Actual loading/location constraints and at least two anchors are checked.",
      "For ordered items the criterion is Wu-Estabrook scalar invariance: equal thresholds, loadings, and latent-response intercepts; residual variances or delta scale factors may differ.",
      "Equal residual variances are strict invariance and are not required for a mean contrast. Substantive anchor validity and prespecification still require evidence."
    )
  )
}

#' Prespecified Anchor Sensitivity With Separate Exact Model Records
#'
#' @description
#' Experimental: the arguments and the form of the result may change in
#' a later release of SIEVER without a period of deprecation.
#' Fits the measurement invariance ladder once for each named set of anchor
#' items, releasing the other items' parameters across groups, and returns
#' the latent mean contrasts each anchor set implies. When the contrasts
#' agree across sets, the conclusion does not hinge on which items were
#' taken to function alike; when they differ, the anchor choice is part of
#' the result and must be reported with it.
#'
#' @details
#' **Released and retained.** In each set's model the anchors keep their
#' loadings and locations equal across groups; every other included item is
#' *released*: its loading and intercept (for an ordered item, its
#' thresholds and latent response intercept) are free in every group
#' (lavaan's `group.partial`, listed in each model's `released`). The ladder
#' is configural, metric and scalar, the cumulative order of Meredith (1993),
#' and for ordered items configural, thresholds, metric and scalar, the
#' order Wu and Estabrook (2016) recommend; the contrasts are read from its last
#' rung by [sieve_invariance_interpret()]: the focal minus reference latent
#' mean, with a Wald interval, licensed when the reference mean is fixed at
#' 0, the focal mean is free, and the factor has at least two verified
#' anchors (Byrne, Shavelson, & Muthen, 1989).
#'
#' **Missing data.** With `missing = "fiml"` a model of continuous items
#' uses full information maximum likelihood on every row with a known group
#' and at least one observed item. A model with any ordered item uses the
#' complete cases even when `missing = "fiml"` (WLSMV in SIEVER's ladder
#' uses the complete cases), and `"listwise"` or `"complete"` uses them for
#' every model. The rows used, and why the others were not, are in
#' `row_flow`.
#'
#' **Prespecification.** `rationale` records why the sets were chosen, and
#' `discovery = TRUE` says the sets were chosen after looking at these data,
#' so the comparison is exploratory and needs confirmation in independent
#' data (`exposure` says which).
#'
#' **Refusals** (an error naming the problem): fewer than two anchor sets,
#' or sets without distinct, non-empty names; an empty `rationale`; a
#' `missing` rule other than `"fiml"`, `"listwise"` or `"complete"` (any
#' other value, `"FIML"` among them, is refused rather than read as
#' complete cases); an item column that is not numeric; a set naming an
#' unknown or excluded item; a set with fewer than two anchors in some
#' construct; and an invariance model that could not be fit, named by its
#' set.
#'
#' @param data Common eligible frame, including group.
#' @param group Group column name.
#' @param meta Item ledger: one row per item with `item`, `construct`,
#'   `type` and `status` (`"active"` for an included item), as the Ingest
#'   page builds it.
#' @param anchor_sets Named list of researcher-defined item sets, at least
#'   two, each naming at least two anchors in every construct.
#' @param estimator One of `"auto"`, which follows the item types (ML
#'   when every item is continuous, WLSMV when any is ordered or binary),
#'   `"mlr"`, `"ml_fiml"`, `"ml"` or `"wlsmv"`, the choices of the
#'   Estimator control; the rows used follow `missing`.
#' @param missing `"fiml"` (the default), `"listwise"` or `"complete"`.
#' @param rationale Researcher justification.
#' @param discovery Whether choices used these data.
#' @return A list:
#'   \describe{
#'     \item{`models`}{One element per anchor set: `name`, `anchors`,
#'       `released` (the lavaan constraints freed), `model_id` (a hash of
#'       the specification, the anchors, the released parameters and the
#'       data), `fit` (the last rung), `parameters`
#'       (`lavaan::parameterEstimates()` with standardized values),
#'       `syntax`, `interpretation` (from [sieve_invariance_interpret()]),
#'       `code` and `warnings`.}
#'     \item{`contrasts`}{The contrasts of every set, the columns of
#'       [sieve_invariance_interpret()]'s `contrasts` with `anchor_set` and
#'       `model_id` added.}
#'     \item{`anchor_sets`, `rationale`, `discovery`}{As supplied.}
#'     \item{`exposure`}{Whether the sets were declared before the data
#'       or chosen with them.}
#'     \item{`row_flow`}{The rows supplied, those without a group, those
#'       with no observed item, the incomplete rows left out under complete
#'       cases, the incomplete rows kept under FIML, and the rows used, with
#'       `rule` saying which rule applied.}
#'     \item{`created`}{The time of the run, in UTC.}
#'   }
#' @references
#' Byrne, B. M., Shavelson, R. J., & Muthen, B. (1989). Testing for the
#' equivalence of factor covariance and mean structures: The issue of
#' partial measurement invariance. *Psychological Bulletin, 105*(3),
#' 456--466. \doi{10.1037/0033-2909.105.3.456}
#'
#' Meredith, W. (1993). Measurement invariance, factor analysis and
#' factorial invariance. *Psychometrika, 58*(4), 525--543.
#' \doi{10.1007/BF02294825}
#'
#' Wu, H., & Estabrook, R. (2016). Identification of confirmatory factor
#' analysis models of different levels of invariance for ordered
#' categorical outcomes. *Psychometrika, 81*(4), 1014--1045.
#' \doi{10.1007/s11336-016-9506-0}
#' @seealso [sieve_invariance_interpret()], [sieve_ordinal_fit_impact()]
#' @author Ken Kelley
#' @examples
#' hs <- lavaan::HolzingerSwineford1939
#' items <- c("x4", "x5", "x6")
#' meta <- data.frame(item = items, label = items, construct = "Textual",
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' anchor_sets <- list(First = c("x4", "x5"), Last = c("x5", "x6"))
#' # At least two anchor sets are named and one invariance ladder is fit for
#' # each, which takes a few seconds, so the comparison runs in an interactive
#' # session only.
#' if (interactive()) {
#'   anchors <- sieve_anchor_sensitivity(hs[, c(items, "school")], "school", meta,
#'     anchor_sets = anchor_sets,
#'     rationale = "Two anchor pairs stated before the data were examined", discovery = FALSE)
#'   anchors$contrasts[, c("anchor_set", "estimate", "lower", "upper", "licensed")]
#' }
#' @export
sieve_anchor_sensitivity <- function(
  data,
  group,
  meta,
  anchor_sets,
  estimator = "auto",
  missing = "fiml",
  rationale,
  discovery = TRUE
) {
  if (length(anchor_sets) < 2L || is.null(names(anchor_sets)) || any(!nzchar(names(anchor_sets))) || anyDuplicated(names(anchor_sets))) stop("Name at least two distinct, prespecified anchor sets.",
    call. = FALSE
  )
  if (length(rationale) != 1L || !nzchar(trimws(rationale))) stop("Record the substantive rationale for the anchor sets.",
    call. = FALSE
  )
  # Any value but the three is refused: read as complete cases without
  # notice, "FIML" would quietly discard every incomplete row.
  if (length(missing) != 1L || is.na(missing) || !missing %in% c("fiml", "listwise", "complete")) {
    stop(sprintf(
      "`missing` must be \"fiml\", \"listwise\" or \"complete\", not %s.",
      paste(deparse(missing), collapse = " ")
    ), call. = FALSE)
  }
  sp <- sieve_cfa_spec(meta, reserved = names(data), estimator = estimator)
  active <- meta[meta$status == "active" & meta$item %in% sp$observed, , drop = FALSE]
  frame <- sieve_invariance_frame(data, data[[group]], sp$observed,
    categorical = length(sp$ordered) > 0L, missing = if (missing == "fiml") "fiml" else "complete"
  )
  if (length(frame$problems)) stop(paste(frame$problems, collapse = " "), call. = FALSE)
  data <- frame$data
  data[[group]] <- frame$group
  runs <- lapply(names(anchor_sets), function(nm) {
    anchors <- unique(anchor_sets[[nm]])
    if (any(!anchors %in% active$item)) stop(
      sprintf(
        "Anchor set '%s' names an unknown or inactive item.",
        nm
      ),
      call. = FALSE
    )
    by <- split(active$item, active$construct)
    if (any(vapply(
      by,
      function(it) length(intersect(
        it,
        anchors
      )) < 2L,
      logical(1)
    ))) stop(
      sprintf(
        "Anchor set '%s' needs at least two anchors per construct.",
        nm
      ),
      call. = FALSE
    )
    released <- setdiff(active$item, anchors)
    partial <- unlist(lapply(released, function(it) {
      f <- names(sp$name_map)[match(as.character(active$construct[match(it, active$item)]), unname(sp$name_map))]
      if (length(f) != 1L || is.na(f)) stop("The construct-to-engine name map is incomplete.", call. = FALSE)
      c(paste(f, "=~", it), if (it %in% sp$ordered) {
        k <- length(unique(stats::na.omit(data[[it]])))
        c(paste(it, "|", paste0("t", seq_len(k - 1L))), paste(it, "~1"))
      } else paste(it, "~1"))
    }))
    r <- sieve_fit_invariance(data, group, meta,
      group_partial = partial,
      levels = if (length(sp$ordered)) c(
        "configural",
        "thresholds",
        "metric",
        "scalar"
      ) else c(
        "configural",
        "metric",
        "scalar"
      ),
      estimator = estimator, missing_policy = missing
    )
    if (!is.null(r$error)) stop(sprintf("Anchor set '%s': %s", nm, r$error), call. = FALSE)
    fits <- attr(r$mi, "fits")
    f <- fits[[length(fits)]]
    list(
      name = nm,
      anchors = anchors,
      released = partial,
      # The model, not the name of the choice that produced it: Auto and
      # ML with FIML give the same specification for continuous items (the
      # recorded call names the resolved choice), and must give one id.
      model_id = sieve_hash(list(
        sp = sp[setdiff(names(sp), c("estimator_choice", "key"))],
        anchors = anchors,
        partial = partial,
        data = data
      )),
      fit = f, parameters = lavaan::parameterEstimates(f, standardized = TRUE),
      syntax = r$syntax, interpretation = sieve_invariance_interpret(f), code = r$code, warnings = r$warnings
    )
  })
  names(runs) <- names(anchor_sets)
  contrasts <- do.call(
    rbind,
    lapply(
      runs,
      function(r) {
        z <- r$interpretation$contrasts
        z$anchor_set <- r$name
        z$model_id <- r$model_id
        z
      }
    )
  )
  list(
    models = runs, contrasts = contrasts, anchor_sets = anchor_sets, rationale = rationale,
    discovery = isTRUE(discovery),
    exposure = if (isTRUE(discovery)) "same-data discovery; confirm independently" else "researcher-declared prespecification; timing not independently verified",
    row_flow = cbind(frame$row_flow, rule = frame$rule, stringsAsFactors = FALSE),
    created = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

#' Generate an identified ordered partial-invariance ladder
#'
#' Each rung's lavaan syntax is written directly by
#' `sieve_measurement_syntax()`: Wu and Estabrook (2016) identification
#' under the theta parameterization, with the released parameters free in
#' every group and the latent response intercepts and residual variances of
#' the released items identified by the rules written into the syntax.
#'
#' @param data Grouped model data.
#' @param syntax Configural measurement model.
#' @param group Grouping column.
#' @param ordered Ordered indicator names.
#' @param levels Leading invariance rungs, or NULL for the full ladder.
#' @param estimator,missing Engine choices.
#' @param group_partial Researcher-declared released parameters.
#' @return A ladder table with fitted models as its fits attribute.
#' @author Ken Kelley
#' @noRd
sieve_ordered_partial_ladder <- function(data, syntax, group, ordered, levels = NULL,
                                         estimator = "WLSMV", missing = "listwise", group_partial) {
  all_binary <- all(vapply(
    data[,
      ordered,
      drop = FALSE
    ],
    function(x) length(unique(stats::na.omit(x))) == 2L,
    logical(1)
  ))
  ladder <- if (all_binary) c(
    "configural",
    "metric",
    "scalar"
  ) else c(
    "configural",
    "thresholds",
    "metric",
    "scalar",
    "strict"
  )
  if (is.null(levels)) levels <- ladder
  if (!identical(
    levels,
    ladder[seq_along(levels)]
  )) stop("Choose a leading subset of the ordered invariance ladder.",
    call. = FALSE
  )
  constraints <- list(
    configural = character(0), thresholds = "thresholds",
    metric = c("thresholds", "loadings"), scalar = c("thresholds", "loadings", "intercepts"),
    strict = c("thresholds", "loadings", "intercepts", "residuals")
  )
  # The configural structure, with its thresholds counted from the data, is
  # read from an unfitted template; every rung's syntax is written from it.
  template <- lavaan::cfa(syntax,
    data = data, group = group, ordered = ordered,
    parameterization = "theta", std.lv = TRUE, do.fit = FALSE
  )
  generated <- lapply(levels, function(rung) {
    sieve_measurement_syntax(template, group_equal = constraints[[rung]], group_partial = group_partial)
  })
  fits <- lapply(generated, function(generated_syntax) lavaan::cfa(generated_syntax,
    data = data, group = group, ordered = ordered, estimator = estimator,
    missing = missing, parameterization = "theta"
  ))
  names(fits) <- levels
  measures <- t(vapply(fits, function(f) as.numeric(lavaan::fitMeasures(
    f,
    c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust", "rmsea.robust")
  )), numeric(5)))
  out <- data.frame(
    level = levels, chi_square = measures[, 1], df = measures[, 2],
    p_chi_square = measures[, 3], cfi = measures[, 4], rmsea = measures[, 5],
    delta_chi_square = NA_real_, delta_df = NA_real_, p_value = NA_real_,
    delta_cfi = c(NA_real_, diff(measures[, 4])), delta_rmsea = c(NA_real_, diff(measures[, 5]))
  )
  if (length(fits) > 1L) for (j in 2:length(fits)) {
    test <- suppressWarnings(lavaan::lavTestLRT(fits[[j - 1L]], fits[[j]]))
    if (is.finite(test[["Df diff"]][2]) && test[["Df diff"]][2] > 0) {
      out$delta_chi_square[j] <- test[["Chisq diff"]][2]
      out$delta_df[j] <- test[["Df diff"]][2]
      out$p_value[j] <- test[["Pr(>Chisq)"]][2]
    }
  }
  attr(out, "fits") <- fits
  attr(out, "estimator") <- estimator
  attr(out, "ordered") <- TRUE
  attr(out, "generated_syntax") <- generated
  attr(out, "identification") <- "Wu and Estabrook (2016) identification, theta parameterization; released items' latent response intercepts and residual variances identified in the written syntax"
  out
}

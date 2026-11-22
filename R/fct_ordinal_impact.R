#' Ordinal expected-score impact on a declared common latent metric
#' @param reference,focal Named per-item lists with loading, intercept,
#'   thresholds, residual_sd, and scores in increasing response order.
#' @param theta Common latent grid.
#' @param mass Nonnegative normalized integration mass.
#' @param weights Named aggregation weights.
#' @param distribution Description of the common trait distribution.
#' @param anchors Substantive common-metric anchor declaration.
#' @return Expected-score curves and signed, absolute, and RMS impact in score units.
#' @author Ken Kelley
#' @noRd
sieve_ordinal_score_impact <- function(reference, focal, theta, mass, weights = NULL,
                                       distribution, anchors) {
  if (!length(reference) || !identical(
    names(reference),
    names(focal)
  ) || anyDuplicated(names(reference)) || is.null(names(reference))) stop("Both groups must name the same distinct items in the same order.",
    call. = FALSE
  )
  if (!length(theta) || any(!is.finite(theta)) || length(mass) != length(theta) || any(!is.finite(mass) | mass < 0) || sum(mass) <= 0) stop("Declare a finite common trait grid and nonnegative integration mass.",
    call. = FALSE
  )
  if (!nzchar(distribution) || !nzchar(anchors)) stop("Record the common trait distribution and anchor justification.",
    call. = FALSE
  )
  mass <- mass / sum(mass)
  if (is.null(weights)) weights <- stats::setNames(rep(1, length(reference)), names(reference))
  if (!is.null(names(weights))) weights <- weights[names(reference)]
  if (length(weights) != length(reference) || any(!is.finite(weights))) stop("Supply one finite aggregation weight per item.",
    call. = FALSE
  )
  expected <- function(p) {
    required <- c("loading", "intercept", "thresholds", "residual_sd", "scores")
    if (!all(required %in% names(p)) || length(p$loading) != 1L || length(p$intercept) != 1L || length(p$residual_sd) != 1L || p$residual_sd <= 0 ||
      any(!is.finite(unlist(p[required]))) || length(p$scores) != length(p$thresholds) + 1L || any(diff(p$thresholds) <= 0) || any(diff(p$scores) <= 0)) stop("Each item needs finite ordered thresholds/scores, one loading/intercept, and positive residual SD.",
      call. = FALSE
    )
    exceed <- vapply(
      p$thresholds,
      function(t) stats::pnorm((p$intercept + p$loading * theta - t) / p$residual_sd),
      numeric(length(theta))
    )
    as.numeric(p$scores[1] + exceed %*% diff(p$scores))
  }
  ref <- vapply(reference, expected, numeric(length(theta)))
  foc <- vapply(focal, expected, numeric(length(theta)))
  delta <- foc - ref
  summary <- function(d) c(signed = sum(mass * d), absolute = sum(mass * abs(d)), rms = sqrt(sum(mass * d^2)))
  item <- as.data.frame(t(apply(delta, 2L, summary)))
  item$item <- names(reference)
  item$weight <- as.numeric(weights)
  total <- as.data.frame(as.list(summary(as.vector(delta %*% weights))))
  total$target <- "weighted total score"
  list(
    items = item[, c("item", "weight", "signed", "absolute", "rms")], total = total,
    curves = data.frame(theta = theta, mass = mass, ref, foc, check.names = FALSE),
    reference = reference, focal = focal, weights = stats::setNames(as.numeric(weights), names(reference)),
    distribution = distribution, anchors = anchors, theta = theta, mass = mass,
    direction = "focal minus reference; original category-score units",
    limitation = "Point impact conditional on the fitted model, common metric, and declared trait distribution. No sampling interval is supplied. This is not dMACS."
  )
}

#' Ordinal Expected-Score Impact From a Fitted Ordered Invariance Model
#'
#' @description
#' Experimental: the arguments and the form of the result may change in
#' a later release of SIEVER without a period of deprecation.
#' Reads a fitted multiple-group model of ordered items and reports, for
#' each item and for the total score, how far the focal group's expected
#' item score departs from the reference group's at the same trait value,
#' averaged over a trait distribution you declare. It states the practical
#' size of measurement noninvariance on the response scale the scores are
#' reported on. The summaries are conditional on the fitted parameters,
#' and with `B` greater than 0 each has a pointwise interval from draws of
#' those parameters (see Details).
#'
#' @details
#' **Estimand.** For an item with ordered categories scored
#' `s_0 < s_1 < ... < s_C` (the `scores` you supply), the model-implied
#' expected score in group `g` at trait value `theta` is
#' `E_g(theta) = s_0 + sum_c (s_c - s_(c-1)) * pnorm((nu_g + lambda_g * theta - tau_gc) / sigma_g)`,
#' the normal-ogive graded response form (Samejima, 1969), where
#' `lambda_g`, `nu_g`, `tau_gc` and `sigma_g` are the group's fitted
#' loading, latent response intercept, thresholds and latent response
#' residual standard deviation. With `d(theta) = E_focal(theta) - E_reference(theta)`,
#' the three summaries are averages over the declared common trait
#' distribution `N(mean, sd)`: `signed` is the average of `d` (differences
#' of opposite sign over the trait range cancel), `absolute` the average of
#' `|d|` (they do not), and `rms` the square root of the average of `d^2`.
#' The average is a sum over a 601-point grid spanning `mean` plus or minus
#' six `sd`, weighted by the normal density normalized to sum to one.
#'
#' **Units and sign.** Every summary is in the units of the category
#' scores (with scores `1:4`, a signed impact of 0.20 is a fifth of a
#' response category), and the sign is focal minus reference. The totals
#' apply the same summaries to the sum of the item differences, every item
#' weighted 1 (the `weight` column). Both groups are integrated over the
#' same trait distribution, so the summaries describe the measurement
#' difference alone, not a difference between the groups' trait
#' distributions; `mean` and `sd` are on the reference group's factor
#' metric (in a model with the reference variance fixed at 1 and mean at 0,
#' `mean = 0, sd = 1` is the reference group's own distribution). It is not
#' a standardized effect size of noninvariance such as dMACS.
#'
#' **What the model must be.** An admissible fit of ordered items (`fit`
#' converged and identified) with at least two verified anchors on every
#' factor, read from the parameter table by [sieve_invariance_interpret()]:
#' items whose loadings, thresholds and latent response intercepts are
#' equal across the groups, the scalar invariance of Wu and Estabrook
#' (2016), which puts both groups on one latent metric. Items must load on
#' one factor each. Missing responses were handled by the fit itself
#' (SIEVER's ordered invariance models use the complete cases).
#'
#' **Intervals.** The intervals reflect the sampling uncertainty of the
#' estimated item parameters only. The free parameters are drawn `B` times
#' from their joint asymptotic normal distribution, N(`coef(fit)`,
#' `vcov(fit)`), with the directions an equality constraint fixes held at
#' the estimate; each draw sets every group's model matrices through lavaan
#' (residual variances included, under either parameterization), and the
#' impact is computed again. A draw with unordered thresholds or a
#' nonpositive residual variance is set aside and counted, never repaired.
#' The limits are the pointwise percentiles (type 7) of the usable draws,
#' issued only when at least `ceiling(2 / (1 - conf_level)) - 1` draws
#' are usable (39 at 0.95), with a caution when the share set aside reaches
#' the interval's tail area, (1 - `conf_level`) / 2. The trait
#' distribution, the category scores and the anchors are held fixed.
#' `absolute` and `rms` are bounded below by 0, so their percentile limits
#' are biased upward near 0: an item with no impact in the population still
#' has a lower limit above 0. This is the parameter imputation Chalmers,
#' Counsell, and Flora (2016) give for their signed and unsigned
#' differential test functioning statistics (sDTF and uDTF), which `signed`
#' and `absolute` are at the level of the item and of the total, with the
#' sign reversed (focal minus reference) and the declared normal trait
#' distribution as the weight.
#'
#' **Refusals** (an error naming the problem): a `conf_level` outside
#' (0, 1); a `B` that is neither 0 nor a whole number of at least
#' `ceiling(2 / (1 - conf_level)) - 1`; a fit that is not of ordered
#' items or has no admissible solution; a `mean` or `sd` that is not
#' finite, or `sd <= 0`; `reference` and `focal` that are not two distinct
#' group labels of the fit; a factor with fewer than two verified anchors;
#' an item that loads on more than one factor; an item whose `scores` are
#' missing or do not have one more value than the item has thresholds; and
#' an empty `anchors` statement.
#'
#' @param fit Fitted multiple-group lavaan model with ordered indicators.
#' @param scores Named item category scores in observed increasing order:
#'   a list with one numeric vector per item, named by item, each strictly
#'   increasing with one value per response category.
#' @param reference,focal Group labels, as in
#'   `lavaan::lavInspect(fit, "group.label")`; the impact is focal minus
#'   reference.
#' @param mean,sd Mean and SD of the declared common normal trait distribution.
#' @param anchors Researcher common-metric justification: one sentence
#'   saying why the anchors are taken to function equally, kept with the
#'   result.
#' @param B Number of parameter draws for the intervals: `0` for point
#'   impact alone, otherwise at least the number a percentile interval at
#'   `conf_level` needs (39 at 0.95; see Details).
#' @param seed `NULL` (a seed is drawn and recorded) or one whole number,
#'   for the parameter draws.
#' @param conf_level Confidence level of the pointwise intervals: one number
#'   strictly between 0 and 1.
#' @return A list:
#'   \describe{
#'     \item{`table`}{One row per item: `item`, `weight` (1), `signed`,
#'       `absolute`, `rms` (category-score units, focal minus reference),
#'       `factor`, and the limits `signed_lower`, `signed_upper`,
#'       `absolute_lower`, `absolute_upper`, `rms_lower` and `rms_upper`
#'       (`NA` when no interval was formed).}
#'     \item{`totals`}{One row per factor: `signed`, `absolute`, `rms` for
#'       the total of its items, `target` ("weighted total score"),
#'       `factor`, and the same limits.}
#'     \item{`constructs`}{One element per factor, holding the item and
#'       total summaries, the expected-score `curves` on the grid (`theta`,
#'       `mass`, then the reference group's curve of each item, then the
#'       focal group's), the parameters of each group (`reference`,
#'       `focal`), the `weights`, the declared `distribution`, the `anchors`
#'       statement, `theta`, `mass`, `direction` and `limitation`.}
#'     \item{`reference`, `focal`}{The group labels.}
#'     \item{`constraints`}{The anchors table of
#'       [sieve_invariance_interpret()]: which loadings and locations are
#'       equal across the groups.}
#'     \item{`interval`}{How the intervals were formed: `status`
#'       (`"completed"`, `"unavailable"` or `"not requested"`), `reason`,
#'       the `attempted`, `used` and `failed` draws, `failures` counted by
#'       reason, `caution`, `rng` (the seed record), `conf_level` and
#'       `method`.}
#'     \item{`model_key`}{A hash of the parameter table, the scores, the
#'       distribution and the anchors statement.}
#'     \item{`limitation`}{What the summary is conditional on, how its
#'       intervals were formed (or why there are none), and that it is not
#'       dMACS.}
#'   }
#' @references
#' Chalmers, R. P., Counsell, A., & Flora, D. B. (2016). It might not make
#' a big DIF: Improved differential test functioning statistics that
#' account for sampling variability. *Educational and Psychological
#' Measurement, 76*(1), 114--140. \doi{10.1177/0013164415584576}
#'
#' Samejima, F. (1969). Estimation of latent ability using a response
#' pattern of graded scores. *Psychometrika, 34*(S1), 1--97.
#' \doi{10.1007/BF03372160}
#'
#' Wu, H., & Estabrook, R. (2016). Identification of confirmatory factor
#' analysis models of different levels of invariance for ordered
#' categorical outcomes. *Psychometrika, 81*(4), 1014--1045.
#' \doi{10.1007/s11336-016-9506-0}
#' @seealso [sieve_invariance_interpret()] for the anchors and the latent
#'   mean contrasts; [sieve_anchor_sensitivity()] for fitting the anchored
#'   models.
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' verbal <- c("t5_general_information", "t6_paragraph_comprehension", "t7_sentence",
#'   "t8_word_classification", "t9_word_meaning")
#' # The five verbal tests cut at their quartiles into four ordered categories.
#' responses <- as.data.frame(lapply(hs[, verbal], function(x) {
#'   as.integer(cut(x, stats::quantile(x, 0:4 / 4), include.lowest = TRUE))
#' }))
#' responses$school <- hs$school
#' meta <- data.frame(item = verbal, label = verbal, construct = "Verbal",
#'   facet = NA_character_, type = "ordered", status = "active", reverse = FALSE,
#'   stringsAsFactors = FALSE)
#' table(responses$school)
#' # The impact is read from an ordered invariance model whose anchors give
#' # the two groups a common scale. Fitting the anchored models takes a few
#' # seconds, so the rest runs in an interactive session only.
#' if (interactive()) {
#'   anchored <- sieve_anchor_sensitivity(responses, "school", meta,
#'     anchor_sets = list(All = verbal, FirstFour = verbal[1:4]),
#'     rationale = "Anchor sets stated before the data were examined", discovery = FALSE)
#'   impact <- sieve_ordinal_fit_impact(anchored$models$All$fit,
#'     scores = stats::setNames(rep(list(1:4), 5L), verbal),
#'     reference = "Pasteur", focal = "Grant-White",
#'     anchors = "All five verbal tests are anchors", seed = 1)
#'   impact$table
#' }
#' @export
sieve_ordinal_fit_impact <- function(fit, scores, reference, focal, mean = 0, sd = 1, anchors,
                                     B = 1000L, seed = NULL, conf_level = .95) {
  conf_level <- sieve_check_conf_level(conf_level)
  minimum <- sieve_bootstrap_min_draws(conf_level)
  if (!is.numeric(B) || length(B) != 1L || !is.finite(B) || B != trunc(B) || B < 0 || (B > 0 && B < minimum)) {
    stop(sprintf(
      "`B` must be 0 (point impact alone) or a whole number of at least %d parameter draws for a %s interval; received %s.",
      minimum, sieve_ci_percent(conf_level), paste(deparse(B), collapse = " ")
    ), call. = FALSE)
  }
  B <- as.integer(B)
  if (!sieve_fit_is_categorical(fit) || !sieve_has_solution(fit)) stop("Use an identified, converged ordered invariance model.",
    call. = FALSE
  )
  if (!is.finite(mean) || !is.finite(sd) || sd <= 0) stop("Declare a finite common mean and positive SD.",
    call. = FALSE
  )
  labels <- lavaan::lavInspect(fit, "group.label")
  at <- match(c(reference, focal), labels)
  if (anyNA(at) || at[1] == at[2]) stop("Choose two distinct fitted groups.", call. = FALSE)
  reviewed <- sieve_invariance_interpret(fit)
  est <- lavaan::lavInspect(fit, "est")
  lv <- lavaan::lavNames(fit, "lv")
  grid <- seq(-6, 6, length.out = 601L) * sd + mean
  mass <- stats::dnorm(grid, mean, sd)
  distribution <- sprintf(
    "Common normal trait: mean=%s, SD=%s; 601-point grid over mean +/- 6 SD, normalized mass",
    mean, sd
  )
  # The structure (anchors, each factor's items, simple structure) is read
  # once, at the estimate; a parameter draw changes values, not structure.
  setup <- lapply(lv, function(f) {
    anchor_rows <- reviewed$anchors[reviewed$anchors$factor == f, ]
    if (sum(anchor_rows$anchor) < 2L) stop(
      sprintf(
        "A common origin/scale could not be verified for '%s'. Inspect actual constraints and anchors first.",
        f
      ),
      call. = FALSE
    )
    lambda <- est[[at[1]]]$lambda
    items <- rownames(lambda)[lambda[, f] != 0]
    if (!length(items) || any(rowSums(lambda[items, ,
      drop = FALSE
    ] != 0) > 1L)) stop("Expected-score impact requires simple-structure items: each item loads on one factor.",
      call. = FALSE
    )
    list(factor = f, items = items)
  })
  group_parameters <- function(e, f, items) stats::setNames(lapply(items, function(it) {
    tau <- e$tau[startsWith(rownames(e$tau), paste0(it, "|")), 1]
    vals <- scores[[it]]
    if (is.null(vals) || length(vals) != length(tau) + 1L) stop(
      sprintf(
        "Declare the exact ordered category scores for '%s'.",
        it
      ),
      call. = FALSE
    )
    residual <- e$theta[it, it]
    list(
      loading = unname(e$lambda[it, f]), intercept = unname(e$nu[it, 1]),
      thresholds = unname(tau), residual_sd = if (is.finite(residual) && residual > 0) sqrt(residual) else NA_real_,
      scores = vals
    )
  }), items)
  impact_at <- function(e) {
    out <- lapply(setup, function(s) {
      sieve_ordinal_score_impact(group_parameters(e[[at[1]]], s$factor, s$items),
        group_parameters(e[[at[2]]], s$factor, s$items), grid, mass,
        distribution = distribution, anchors = anchors
      )
    })
    names(out) <- lv
    out
  }
  out <- impact_at(est)
  table <- do.call(rbind, lapply(lv, function(f) {
    d <- out[[f]]$items
    d$factor <- f
    d
  }))
  totals <- do.call(rbind, lapply(lv, function(f) {
    d <- out[[f]]$total
    d$factor <- f
    d
  }))
  interval <- sieve_ordinal_impact_draws(fit, est, impact_at, out, B, seed, conf_level, minimum)
  stats <- c("signed", "absolute", "rms")
  for (s in stats) {
    table[[paste0(s, "_lower")]] <- interval$items[, paste0(s, "_lower")]
    table[[paste0(s, "_upper")]] <- interval$items[, paste0(s, "_upper")]
    totals[[paste0(s, "_lower")]] <- interval$totals[, paste0(s, "_lower")]
    totals[[paste0(s, "_upper")]] <- interval$totals[, paste0(s, "_upper")]
  }
  interval$items <- interval$totals <- NULL
  limitation <- paste(
    "Impact conditional on the fitted model, the common metric, the declared trait distribution, and the category scores.",
    switch(interval$status,
      completed = sprintf(
        "%s pointwise intervals from %d of %d parameter draws (parameter estimation uncertainty only; absolute and RMS limits are biased upward near 0).%s",
        sieve_ci_percent(conf_level), interval$used, interval$attempted,
        if (length(interval$caution)) paste0(" ", interval$caution) else ""
      ),
      paste("No interval:", interval$reason)
    ),
    "This is not dMACS."
  )
  # Each factor's impact object is the same impact as its rows of table and
  # totals, so it carries the same limits and limitation; the point-only
  # limitation of sieve_ordinal_score_impact() ("No sampling interval is
  # supplied") does not describe it.
  limit_columns <- paste0(rep(stats, each = 2L), c("_lower", "_upper"))
  for (f in lv) {
    for (column in limit_columns) {
      out[[f]]$items[[column]] <- table[[column]][table$factor == f]
      out[[f]]$total[[column]] <- totals[[column]][totals$factor == f]
    }
    out[[f]]$limitation <- limitation
  }
  list(
    table = table, totals = totals, constructs = out, reference = reference, focal = focal,
    constraints = reviewed$anchors, interval = interval,
    model_key = sieve_hash(list(lavaan::parTable(fit), scores, mean, sd, anchors, B, conf_level, interval$rng$seed)),
    limitation = limitation
  )
}

#' Parameter draw intervals for ordinal expected-score impact
#'
#' Draws the free parameters of `fit` from N(`coef(fit)`, `vcov(fit)`)
#' (the covariance's eigenvectors with negligible eigenvalues, the
#' directions an equality constraint fixes, held at the estimate), sets each
#' group's model matrices with [lavaan::lav_model_set_parameters()], and
#' computes the impact again; draws whose matrices give unordered
#' thresholds or a nonpositive residual variance are set aside and
#' counted.
#'
#' @param fit The fitted model.
#' @param est `lavaan::lavInspect(fit, "est")`, for the matrices' names.
#' @param impact_at A function of a list of group matrices returning each
#'   factor's impact.
#' @param point The impact at the estimate, `impact_at(est)`.
#' @param B,seed,conf_level The request.
#' @param minimum The fewest usable draws for an interval.
#' @return A list with `status`, `reason`, `attempted`, `used`, `failed`,
#'   `failures`, `caution`, `rng`, `conf_level`, `method`, and the limit
#'   matrices `items` and `totals` (`NA` when no interval was formed).
#' @author Ken Kelley
#' @noRd
sieve_ordinal_impact_draws <- function(fit, est, impact_at, point, B, seed, conf_level, minimum) {
  item_rows <- vapply(point, function(x) nrow(x$items), integer(1))
  n_items <- sum(item_rows)
  n_factors <- length(point)
  stats <- c("signed", "absolute", "rms")
  limit_names <- as.vector(t(outer(stats, c("_lower", "_upper"), paste0)))
  empty <- function(n) matrix(NA_real_, n, length(limit_names), dimnames = list(NULL, limit_names))
  method <- paste(
    "Pointwise percentiles (type 7) of the impact over draws of the free parameters from their joint",
    "asymptotic normal distribution N(coef(fit), vcov(fit)), conditional on admissible draws (ordered",
    "thresholds, positive residual variances); parameter estimation uncertainty only, with the trait",
    "distribution, category scores, and anchors fixed; not a bootstrap or a simultaneous interval",
    "(the parameter imputation of Chalmers, Counsell, & Flora, 2016)"
  )
  result <- function(status, reason, rng = NULL, used = NA_integer_, failures = table(character(0)),
                     caution = NULL, items = empty(n_items), totals = empty(n_factors)) {
    list(
      status = status, reason = reason, attempted = B, used = as.integer(used),
      failed = if (is.na(used)) NA_integer_ else B - as.integer(used), failures = failures,
      caution = caution, rng = rng, conf_level = conf_level, method = method,
      items = items, totals = totals
    )
  }
  if (B == 0L) return(result("not requested", "B = 0 requests point impact alone."))
  x_hat <- tryCatch(as.numeric(lavaan::coef(fit)), error = function(e) NULL)
  V <- tryCatch(lavaan::vcov(fit), error = function(e) NULL)
  model <- fit@Model
  if (is.null(x_hat) || is.null(V) || length(x_hat) != model@nx.free || !all(dim(V) == length(x_hat)) || any(!is.finite(V))) {
    return(result("unavailable", "The fit has no finite covariance matrix of its free parameters, so no parameter draws were made."))
  }
  eig <- eigen((V + t(V)) / 2, symmetric = TRUE)
  tolerance <- max(1, max(abs(eig$values))) * 1e-8
  if (min(eig$values) < -tolerance) {
    return(result("unavailable", "The covariance matrix of the free parameters is not positive semidefinite, so no parameter draws were made."))
  }
  root <- eig$vectors %*% diag(sqrt(pmax(eig$values, 0)), nrow = length(eig$values))
  generated <- sieve_with_seed(seed, sweep(matrix(stats::rnorm(B * length(x_hat)), nrow = B) %*% t(root), 2L, x_hat, "+"))
  draw_est <- function(x) sieve_lavaan_group_matrices(fit, x, est)
  flatten <- function(impact) {
    c(
      unlist(lapply(impact, function(x) as.matrix(x$items[, stats]))),
      unlist(lapply(impact, function(x) as.matrix(x$total[, stats])))
    )
  }
  used <- list()
  failures <- character(0)
  for (i in seq_len(B)) {
    run <- tryCatch(flatten(impact_at(draw_est(generated$value[i, ]))), error = function(e) e)
    if (inherits(run, "error")) {
      failures <- c(failures, if (grepl("finite ordered thresholds", conditionMessage(run), fixed = TRUE)) {
        "inadmissible draw (unordered thresholds or a nonpositive residual variance)"
      } else {
        "impact could not be computed"
      })
      next
    }
    if (any(!is.finite(run))) {
      failures <- c(failures, "impact not finite")
      next
    }
    used[[length(used) + 1L]] <- run
  }
  count <- length(used)
  if (count < minimum) {
    return(result("unavailable", sprintf(
      "Only %d of %d parameter draws were usable; at least %d are needed for a %s interval. No interval issued.",
      count, B, minimum, sieve_ci_percent(conf_level)
    ), rng = generated$rng, used = count, failures = table(failures)))
  }
  draws <- do.call(rbind, used)
  alpha <- (1 - conf_level) / 2
  limits <- apply(draws, 2L, stats::quantile, probs = c(alpha, 1 - alpha), names = FALSE, type = 7)
  # The columns of `draws` follow flatten(): each factor's item table by
  # column (every item's signed, then absolute, then rms), then each
  # factor's total (signed, absolute, rms).
  fill <- function(lower, upper) {
    out <- empty(nrow(lower))
    for (j in seq_along(stats)) {
      out[, paste0(stats[j], "_lower")] <- lower[, j]
      out[, paste0(stats[j], "_upper")] <- upper[, j]
    }
    out
  }
  start <- 0L
  pieces <- vector("list", n_factors)
  for (f in seq_len(n_factors)) {
    cols <- start + seq_len(item_rows[f] * length(stats))
    pieces[[f]] <- fill(
      matrix(limits[1L, cols], item_rows[f], length(stats)),
      matrix(limits[2L, cols], item_rows[f], length(stats))
    )
    start <- start + length(cols)
  }
  cols <- start + seq_len(n_factors * length(stats))
  totals <- fill(
    matrix(limits[1L, cols], n_factors, length(stats), byrow = TRUE),
    matrix(limits[2L, cols], n_factors, length(stats), byrow = TRUE)
  )
  caution <- if (sieve_bootstrap_caution_reached(B - count, B, conf_level)) {
    "The share of unusable draws reaches the interval's tail area, so the limits, conditional on admissible draws, may be sensitive to the draws set aside."
  }
  result("completed", "", rng = generated$rng, used = count, failures = table(failures), caution = caution,
    items = do.call(rbind, pieces), totals = totals
  )
}

#' The model matrices of each group of a lavaan fit at other parameter values
#'
#' Sets the free parameters of `fit` to `x` with
#' [lavaan::lav_model_set_parameters()], which also forms the matrices that
#' depend on them (the residual variances of ordered items under the delta
#' parameterization), and returns each group's loading, intercept,
#' threshold, and residual covariance matrices, named as
#' `lavaan::lavInspect(fit, "est")` names them.
#'
#' @param fit A multiple-group lavaan fit (one block per group).
#' @param x Values of the free parameters, in the order of `coef(fit)`.
#' @param est `lavaan::lavInspect(fit, "est")`.
#' @param needed The matrices to return.
#' @return A list, one element per group, of the named matrices.
#' @author Ken Kelley
#' @noRd
sieve_lavaan_group_matrices <- function(fit, x, est = lavaan::lavInspect(fit, "est"),
                                        needed = c("lambda", "nu", "tau", "theta")) {
  model <- fit@Model
  groups <- if (is.list(est) && !is.matrix(est[[1L]])) est else list(est)
  if (length(model@nmat) != length(groups)) {
    stop("The fit's model blocks do not correspond one to one to its groups.", call. = FALSE)
  }
  offsets <- c(0L, cumsum(model@nmat))
  m <- lavaan::lav_model_set_parameters(model, x = as.numeric(x))
  lapply(seq_along(groups), function(g) {
    mats <- m@GLIST[offsets[g] + seq_len(model@nmat[g])]
    out <- lapply(needed, function(nm) {
      v <- mats[[nm]]
      dimnames(v) <- dimnames(groups[[g]][[nm]])
      v
    })
    names(out) <- needed
    out
  })
}

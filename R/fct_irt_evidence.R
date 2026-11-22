# Rebuild the selected fitted parent in every dependent recorded step. A user
# can revisit a cached construct after fitting another one, so a generic `grm`
# left by an earlier step does not identify the model currently on screen.
sieve_irt_replay_code <- function(items, estimator) {
  ids <- paste(encodeString(as.character(items), quote = '"'), collapse = ", ")
  c(
    sprintf("irt_items <- c(%s)", ids),
    "irt_data <- items_data[, irt_items, drop = FALSE]",
    "irt_data[] <- lapply(irt_data, function(x) as.numeric(if (is.factor(x)) as.character(x) else x))",
    sprintf(
      "grm <- DMAR::irt_grm(irt_data, items = irt_items, estimator = %s, metric = \"normal_ogive\", rmsea_conf_level = %s)",
      encodeString(estimator, quote = '"'), sieve_num_lit(sieve_default_rmsea_conf_level())
    )
  )
}

# Evidence for the existing DMAR/lavaan limited-information graded response
# model. No alternative fitting engine or additional package is used here.

#' Delta transforms of standardized categorical model parameters
#' @param lambda Standardized loadings in the fitted factor's orientation.
#' @param tau Standardized thresholds, one per boundary.
#' @param item Item name for each boundary.
#' @param category Boundary index per item.
#' @param covariance Joint covariance of standardized loadings and thresholds.
#' @param loading_index,threshold_index Positions in that covariance.
#' @param sign Factor orientation multiplier, -1 or 1.
#' @param conf_level Confidence level.
#' @return Parameter intervals, Jacobian, and joint transformed covariance.
#' @author Ken Kelley
#' @noRd
sieve_irt_delta_parameters <- function(lambda, tau, item, category, covariance,
                                       loading_index, threshold_index, sign = 1,
                                       conf_level = .95) {
  if (length(conf_level) != 1L || !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) stop("Choose a confidence level between zero and one.",
    call. = FALSE
  )
  if (!sign %in% c(
    -1,
    1
  ) || length(lambda) != length(tau) || length(item) != length(tau) || length(category) != length(tau)) stop("Loading, threshold, and item identities must align.",
    call. = FALSE
  )
  covariance <- as.matrix(covariance)
  if (nrow(covariance) != ncol(covariance) || any(!is.finite(covariance)) || anyNA(c(
    loading_index,
    threshold_index
  ))) stop("A finite joint parameter covariance and matching indices are required.",
    call. = FALSE
  )
  if (any(c(
    loading_index,
    threshold_index
  ) < 1L | c(
    loading_index,
    threshold_index
  ) > ncol(covariance))) stop("Parameter covariance indices are outside the matrix.",
    call. = FALSE
  )
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  lambda_se <- sqrt(pmax(0, diag(covariance)[loading_index]))
  regular <- is.finite(lambda) & is.finite(tau) & abs(lambda) < 1 & abs(lambda) > sqrt(.Machine$double.eps)
  near_boundary <- !regular | abs(lambda) + z * lambda_se >= 1
  weak_location <- !regular | abs(lambda) <= z * lambda_se
  J_a <- J_b <- matrix(0, nrow = length(tau), ncol = ncol(covariance))
  for (i in which(regular)) {
    J_a[i, loading_index[i]] <- sign / (1 - lambda[i]^2)^(3 / 2)
    J_b[i, loading_index[i]] <- -sign * tau[i] / lambda[i]^2
    J_b[i, threshold_index[i]] <- sign / lambda[i]
  }
  variance <- function(J) rowSums((J %*% covariance) * J)
  va <- variance(J_a)
  vb <- variance(J_b)
  valid_a <- regular & !near_boundary & va >= -1e-12
  valid_b <- valid_a & !weak_location & vb >= -1e-12
  a <- ifelse(regular, sign * lambda / sqrt(pmax(0, 1 - lambda^2)), NA_real_)
  b <- ifelse(regular, sign * tau / lambda, NA_real_)
  a_se <- ifelse(valid_a, sqrt(pmax(0, va)), NA_real_)
  b_se <- ifelse(valid_b, sqrt(pmax(0, vb)), NA_real_)
  status <- ifelse(!regular, "unavailable: zero/improper loading", ifelse(near_boundary,
    "unstable: loading interval reaches a response-variance boundary", ifelse(weak_location,
      "unstable location: loading interval includes zero", "completed"
    )
  ))
  table <- data.frame(
    item = item, category = category, a = a, a_se = a_se,
    a_lower = a - z * a_se, a_upper = a + z * a_se,
    b = b, b_se = b_se, b_lower = b - z * b_se, b_upper = b + z * b_se,
    status = status, stringsAsFactors = FALSE
  )
  # One discrimination per item, and one location per boundary. Preserve all
  # cross-item and within-item covariance for downstream uncertainty work.
  unique_a <- !duplicated(item)
  J <- rbind(J_a[unique_a, , drop = FALSE], J_b)
  parameter <- c(paste0("a::", item[unique_a]), paste0("b::", item, "::", category))
  rownames(J) <- parameter
  transformed <- J %*% covariance %*% t(J)
  list(
    table = table, jacobian = J, covariance = transformed,
    estimates = stats::setNames(c(a[unique_a], b), parameter),
    method = "First-order delta Wald intervals from lavaan's full joint standardized-parameter covariance",
    conf_level = conf_level
  )
}

#' Uncertainty in the Existing DMAR Graded Response Parameters
#'
#' Experimental: the arguments and the form of the result may change in
#' a later release of SIEVER without a period of deprecation.
#'
#' Delta method Wald intervals for the discriminations a and the boundary
#' locations b of a graded response model fitted by [DMAR::irt_grm()],
#' from lavaan's joint covariance of the standardized loadings and
#' thresholds. The estimates and intervals are computed in the normal
#' ogive metric, whatever metric the supplied result reports a in; the
#' logistic metric (a multiplied by 1.702, the constant
#' [DMAR::irt_grm()] uses) is given beside it in every table, so the
#' table has one shape for either input. The locations b are the same in
#' both metrics.
#'
#' @param grm A [DMAR::irt_grm()] result, fitted with either `metric`.
#' @param conf_level Confidence level.
#' @return Parameter evidence or an explicit unavailable status. Evidence is
#'   a list with `table` (one row per item and boundary: `item`,
#'   `category`, `a`, `a_se`, `a_lower`, `a_upper` in the normal ogive
#'   metric, `a_logistic`, `a_logistic_lower`, `a_logistic_upper` (1.702
#'   times `a`, `a_lower`, `a_upper`), `b`, `b_se`, `b_lower`, `b_upper`,
#'   and `status`), `jacobian`, `covariance` and `estimates` (in the normal
#'   ogive metric), `method`, `conf_level`, `metric` (`"normal_ogive"`, the
#'   metric of `a`), `status`, and `reason`, which says when the supplied
#'   result reports a in the logistic metric.
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' verbal <- c("t5_general_information", "t6_paragraph_comprehension", "t7_sentence",
#'   "t8_word_classification", "t9_word_meaning")
#' # The five verbal tests, each cut at its quartiles into four ordered categories.
#' responses <- as.data.frame(lapply(hs[, verbal], function(x) {
#'   as.integer(cut(x, stats::quantile(x, 0:4 / 4), include.lowest = TRUE))
#' }))
#' grm <- DMAR::irt_grm(responses, metric = "normal_ogive")
#' evidence <- sieve_irt_parameter_evidence(grm)
#' head(evidence$table[, c("item", "category", "a", "a_lower", "a_upper", "b", "b_lower", "b_upper")])
#' @export
sieve_irt_parameter_evidence <- function(grm, conf_level = .95) {
  conf_level <- sieve_check_conf_level(conf_level)
  fit <- attr(grm, "fit")
  if (is.null(fit)) return(list(status = "unavailable", reason = "The stored result has no fitted categorical model."))
  tryCatch(
    {
      ss <- lavaan::standardizedSolution(fit, type = "std.all", remove_eq = TRUE, remove_ineq = TRUE)
      V <- lavaan::lavInspect(fit, "vcov.std.all")
      names_v <- colnames(V)
      if (is.null(names_v)) stop("The installed engine returned no covariance parameter labels.")
      loads <- ss[ss$op == "=~", , drop = FALSE]
      thresholds <- ss[ss$op == "|", , drop = FALSE]
      item <- as.character(grm$item)
      category <- as.integer(grm$category)
      li <- match(item, loads$rhs)
      ti <- match(paste(item, paste0("t", category)), paste(thresholds$lhs, thresholds$rhs))
      if (anyNA(c(li, ti))) stop("Stored IRT rows do not match the categorical model parameters.")
      load_names <- paste0(loads$lhs[li], "=~", loads$rhs[li])
      threshold_names <- paste0(thresholds$lhs[ti], "|", thresholds$rhs[ti])
      loading_index <- match(load_names, names_v)
      threshold_index <- match(threshold_names, names_v)
      orient <- if (isTRUE(attr(grm, "factor_sign_flipped"))) -1 else 1
      if (!isTRUE(all.equal(as.numeric(grm$lambda),
        orient * loads$est.std[li],
        tolerance = 1e-8
      ))) stop("Stored loading orientation does not match the fitted model.")
      out <- sieve_irt_delta_parameters(loads$est.std[li], thresholds$est.std[ti], item, category,
        V, loading_index, threshold_index,
        sign = orient, conf_level = conf_level
      )
      # The logistic metric beside the normal ogive one: a linear rescaling,
      # so its Wald limits are the rescaled limits.
      table <- out$table
      at <- match("a_upper", names(table))
      logistic <- data.frame(
        a_logistic = 1.702 * table$a, a_logistic_lower = 1.702 * table$a_lower,
        a_logistic_upper = 1.702 * table$a_upper
      )
      out$table <- cbind(table[seq_len(at)], logistic, table[-seq_len(at)])
      out$method <- paste0(out$method, " (a in the normal ogive metric)")
      out$metric <- "normal_ogive"
      out$status <- if (all(out$table$status == "completed")) "completed" else "completed with unstable parameters"
      out$reason <- "Unstable transformations retain point estimates when defined, with unavailable intervals and explicit reasons. Parameter estimation uncertainty differs from conditional SE(theta)."
      if (identical(attr(grm, "metric", exact = TRUE), "logistic")) {
        out$reason <- paste(
          out$reason,
          "a, a_lower and a_upper are in the normal ogive metric; the supplied DMAR::irt_grm() result reports a in the logistic metric, 1.702 times larger, which the a_logistic columns match."
        )
      }
      out
    },
    error = function(e) list(status = "unavailable", reason = conditionMessage(e))
  )
}

#' Pointwise Information Uncertainty From the Joint Transformed Parameter Covariance
#'
#' @description
#' Experimental: the arguments and the form of the result may change in
#' a later release of SIEVER without a period of deprecation.
#' Gives intervals for the test information function of a graded response
#' model at fixed trait values, and for the conditional standard error of
#' the trait estimate that follows from it. The intervals reflect the
#' uncertainty in the estimated item parameters only: how much the
#' information curve could differ with another calibration sample of the
#' same size, not how precisely any respondent is measured.
#'
#' @details
#' **Method.** The item discriminations `a` and thresholds `b` (normal
#' ogive metric) and their joint covariance matrix are taken from
#' [sieve_irt_parameter_evidence()], which transforms the fitted
#' categorical factor model's loadings and thresholds by the delta method.
#' `B` parameter vectors are drawn from the joint normal distribution with
#' that mean and covariance; a draw with a nonpositive `a` or with an
#' item's thresholds out of order is inadmissible and is discarded and
#' counted, never repaired. For each admissible draw the test information
#' at each `theta` is computed with [DMAR::irt_information()], and the
#' limits are the pointwise percentiles (type 7) of those values. The
#' band is pointwise, not simultaneous over `theta`, and it is not a
#' bootstrap. The point estimate is the information of `grm` itself
#' (Samejima, 1969).
#'
#' **Conditional standard error.** `conditional_se` is
#' `1 / sqrt(information)`, the asymptotic standard error of the maximum
#' likelihood trait estimate at `theta`, not the posterior standard
#' deviation of an EAP estimate; its limits are the reciprocal square roots
#' of the information limits (the upper information limit gives the lower
#' standard error limit).
#'
#' **When there is no interval.** The result has `status = "unavailable"`
#' and a `reason` when a parameter transformation is unstable (resolve it
#' first, see [sieve_irt_parameter_evidence()]), when the covariance matrix
#' is not positive semidefinite, or when fewer draws were admissible than
#' the minimum `B` (39 at 0.95). A `caution` is added
#' when the discarded share reaches the tail area `(1 - conf_level) / 2`,
#' since the limits are then conditional on many omitted draws. A `B` below
#' that minimum, or not a whole number, is refused with an error.
#'
#' @param grm A [DMAR::irt_grm()] result, fitted with either `metric`: the
#'   parameter draws are made in the normal ogive metric, and the point
#'   estimate comes from [DMAR::irt_information()], which reads the
#'   result's metric.
#' @param theta Trait grid.
#' @param B Number of joint normal parameter draws, a whole number of at
#'   least `ceiling(2 / (1 - conf_level)) - 1` (39 at 0.95, 19 at 0.90), the
#'   fewest draws for which both percentile limits lie inside the draws.
#' @param seed Optional local random seed; `NULL` draws one and records it
#'   in `rng`.
#' @param conf_level Confidence level.
#' @return When completed, a list:
#'   \describe{
#'     \item{`status`}{`"completed"`.}
#'     \item{`table`}{One row per `theta`: `theta`, `information`,
#'       `information_lower`, `information_upper`, `conditional_se`,
#'       `conditional_se_lower` and `conditional_se_upper`.}
#'     \item{`attempted`, `used`, `failed`}{The draws made, the admissible
#'       ones used, and those discarded.}
#'     \item{`failures`}{A table of the reasons draws were discarded.}
#'     \item{`caution`}{A caution about the discarded share, or `NULL`.}
#'     \item{`minimum_covariance_eigenvalue`}{The smallest eigenvalue of
#'       the parameter covariance matrix (negative values within numerical
#'       tolerance are set to zero for the draws).}
#'     \item{`rng`}{The seed used and whether it was supplied.}
#'     \item{`conf_level`, `method`}{The level and the method, in words.}
#'   }
#'   When unavailable, a list with `status = "unavailable"`, `reason`, and
#'   what is known (the parameter `evidence`, or the counts of draws and
#'   `rng`).
#' @references
#' Samejima, F. (1969). Estimation of latent ability using a response
#' pattern of graded scores. *Psychometrika, 34*(S1), 1--97.
#' \doi{10.1007/BF03372160}
#' @seealso [sieve_irt_parameter_evidence()], [DMAR::irt_information()]
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' verbal <- c("t5_general_information", "t6_paragraph_comprehension", "t7_sentence",
#'   "t8_word_classification", "t9_word_meaning")
#' responses <- as.data.frame(lapply(hs[, verbal], function(x) {
#'   as.integer(cut(x, stats::quantile(x, 0:4 / 4), include.lowest = TRUE))
#' }))
#' grm <- DMAR::irt_grm(responses, metric = "normal_ogive")
#' bands <- sieve_irt_information_bands(grm, theta = seq(-3, 3, by = 1), B = 200L, seed = 1)
#' bands$table
#' @export
sieve_irt_information_bands <- function(
  grm,
  theta = seq(-4,
    4,
    length.out = 81L
  ),
  B = 1000L,
  seed = NULL,
  conf_level = .95
) {
  conf_level <- sieve_check_conf_level(conf_level)
  evidence <- sieve_irt_parameter_evidence(grm, conf_level)
  if (is.null(evidence$table) || any(evidence$table$status != "completed")) return(list(
    status = "unavailable",
    reason = "Resolve unstable parameter transformations before requesting approximate joint-normal information bands.",
    evidence = evidence
  ))
  minimum <- sieve_bootstrap_min_draws(conf_level)
  if (length(B) != 1L || !is.finite(B) || B < minimum || B != as.integer(B)) {
    stop(sprintf("Use at least %d integer parameter draws for this interval level.", minimum), call. = FALSE)
  }
  V <- evidence$covariance
  eig <- eigen((V + t(V)) / 2, symmetric = TRUE)
  tolerance <- max(1, max(abs(eig$values))) * 1e-8
  if (min(eig$values) < -tolerance) return(list(
    status = "unavailable",
    reason = "The joint transformed parameter covariance is not positive semidefinite."
  ))
  # Tiny numerical eigenvalues are zeroed, with the magnitude recorded.
  root <- eig$vectors %*% diag(sqrt(pmax(eig$values, 0)), nrow = length(eig$values))
  mu <- evidence$estimates
  generated <- sieve_with_seed(seed, sweep(matrix(stats::rnorm(B * length(mu)), nrow = B) %*% t(root), 2L, mu, "+"))
  items <- unique(as.character(grm$item))
  na <- length(items)
  used <- list()
  failures <- character(0)
  for (i in seq_len(B)) {
    draw <- generated$value[i, ]
    a <- draw[seq_len(na)]
    b <- draw[na + seq_len(nrow(grm))]
    # Do not silently reorder thresholds, truncate slopes, or repair draws.
    proposed <- data.frame(item = as.character(grm$item), category = grm$category, a = a[match(grm$item, items)], b = b)
    ok <- all(is.finite(draw)) && all(a > 0) && all(vapply(
      split(
        seq_len(nrow(grm)),
        grm$item
      ),
      function(k) all(diff(b[k]) > 0),
      logical(1)
    ))
    if (!ok) {
      failures <- c(failures, "inadmissible normal draw")
      next
    }
    run <- tryCatch(DMAR::irt_information(grm = proposed, trait_value = theta), error = function(e) NULL)
    if (is.null(run) || any(!is.finite(run$test_information))) {
      failures <- c(
        failures,
        "information unavailable"
      )
      next
    }
    used[[length(used) + 1L]] <- run$test_information
  }
  count <- length(used)
  if (count < minimum) return(list(
    status = "unavailable",
    reason = sprintf(
      "Only %d admissible information draws; at least %d are needed for this interval level. No interval issued.",
      count,
      minimum
    ),
    attempted = as.integer(B), used = count, failed = B - count, rng = generated$rng
  ))
  caution <- if (sieve_bootstrap_caution_reached(B - count, B, conf_level)) {
    "The unusable draw share reaches the interval tail area. These conditional-on-admissibility limits may be sensitive to omitted draws."
  } else NULL
  draws <- do.call(rbind, used)
  alpha <- (1 - conf_level) / 2
  limits <- apply(draws, 2L, stats::quantile, probs = c(alpha, 1 - alpha), names = FALSE, type = 7)
  point <- DMAR::irt_information(grm = grm, trait_value = theta)
  list(
    status = "completed", table = data.frame(
      theta = theta, information = point$test_information,
      information_lower = limits[1L, ], information_upper = limits[2L, ],
      conditional_se = point$se,
      conditional_se_lower = 1 / sqrt(limits[2L, ]),
      conditional_se_upper = 1 / sqrt(limits[1L, ])
    ),
    attempted = as.integer(B), used = count, failed = B - count, failures = table(failures), caution = caution,
    minimum_covariance_eigenvalue = min(eig$values), rng = generated$rng, conf_level = conf_level,
    method = "Pointwise percentiles of a joint asymptotic normal parameter approximation, conditional on admissible draws; not a bootstrap or simultaneous band"
  )
}

#' Correlation Residuals for Local-Dependence Inspection
#'
#' @description
#' Experimental: the arguments and the form of the result may change in
#' a later release of SIEVER without a period of deprecation.
#' The residual correlation of every pair of items in the fitted categorical
#' factor model (observed minus model-implied correlation, lavaan's
#' `cor.bentler` scaling), largest in absolute value first. A large residual
#' points to local dependence between two items; it is not Yen's Q3, and no
#' test is attached.
#'
#' @param grm Stored categorical factor model result.
#' @return Item-pair residual correlations with their exact definition.
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' verbal <- c("t5_general_information", "t6_paragraph_comprehension", "t7_sentence",
#'   "t8_word_classification", "t9_word_meaning")
#' responses <- as.data.frame(lapply(hs[, verbal], function(x) {
#'   as.integer(cut(x, stats::quantile(x, 0:4 / 4), include.lowest = TRUE))
#' }))
#' grm <- DMAR::irt_grm(responses, metric = "normal_ogive")
#' sieve_irt_residual_pairs(grm)
#' @export
sieve_irt_residual_pairs <- function(grm) {
  fit <- attr(grm, "fit")
  if (is.null(fit)) stop("A stored categorical model is required.", call. = FALSE)
  residual <- lavaan::lavResiduals(fit, type = "cor.bentler", zstat = FALSE)
  mat <- residual$cov
  if (!is.matrix(mat)) stop("The installed engine did not return a residual correlation matrix.", call. = FALSE)
  at <- which(lower.tri(mat), arr.ind = TRUE)
  out <- data.frame(
    item1 = rownames(mat)[at[, 1L]], item2 = colnames(mat)[at[, 2L]],
    residual_correlation = mat[at], stringsAsFactors = FALSE
  )
  out <- out[order(-abs(out$residual_correlation)), , drop = FALSE]
  rownames(out) <- NULL
  attr(
    out,
    "method"
  ) <- "lavaan cor.bentler observed-minus-implied residual correlations, not Yen Q3 or a standardized LD test"
  out
}

#' A bivariate normal category-rectangle probability using base numerical integration
#' @param lo1,hi1,lo2,hi2 Category boundaries on standardized latent responses.
#' @param rho Model-implied latent-response correlation.
#' @return Probability and numerical integration error.
#' @author Ken Kelley
#' @noRd
sieve_irt_rectangle <- function(lo1, hi1, lo2, hi2, rho) {
  if (!is.finite(rho) || abs(rho) >= 1) stop("The model-implied response correlation must be inside (-1, 1).",
    call. = FALSE
  )
  result <- stats::integrate(
    function(x) stats::dnorm(x) *
      (stats::pnorm((hi2 - rho * x) / sqrt(1 - rho^2)) - stats::pnorm((lo2 - rho * x) / sqrt(1 - rho^2))),
    lower = lo1, upper = hi1, rel.tol = 1e-8, subdivisions = 200L, stop.on.error = TRUE
  )
  list(probability = result$value, absolute_error = result$abs.error)
}

#' Observed and Expected Bivariate Category Tables on the Fitted Analytic Sample
#'
#' @description
#' Experimental: the arguments and the form of the result may change in
#' a later release of SIEVER without a period of deprecation.
#' For two items of a fitted graded response model, crosses their observed
#' categories on the complete rows of the fitted items and sets beside each
#' cell the count the model expects, from the bivariate normal distribution
#' of the two latent responses, with the cell's Pearson residual. It is a
#' descriptive look at local dependence between the two items: no p value is
#' attached, because the cells of one table are not independent tests.
#'
#' @param grm Stored GRM result.
#' @param data Exact item data supplied to its fit.
#' @param item1,item2 Selected distinct item identifiers.
#' @return Descriptive bivariate cell residuals, without a calibrated p value.
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' verbal <- c("t5_general_information", "t6_paragraph_comprehension", "t7_sentence",
#'   "t8_word_classification", "t9_word_meaning")
#' responses <- as.data.frame(lapply(hs[, verbal], function(x) {
#'   as.integer(cut(x, stats::quantile(x, 0:4 / 4), include.lowest = TRUE))
#' }))
#' grm <- DMAR::irt_grm(responses, metric = "normal_ogive")
#' cells <- sieve_irt_bivariate_table(grm, responses, verbal[1], verbal[2])
#' head(cells[, c("category1", "category2", "observed", "expected", "pearson_cell_residual")])
#' @export
sieve_irt_bivariate_table <- function(grm, data, item1, item2) {
  items <- unique(as.character(grm$item))
  if (identical(
    item1,
    item2
  ) || !all(c(
    item1,
    item2
  ) %in% items)) stop("Choose two distinct fitted items.",
    call. = FALSE
  )
  data <- data[stats::complete.cases(data[, items, drop = FALSE]), items, drop = FALSE]
  if (!nrow(data)) stop("No complete rows remain in the fitted item set.", call. = FALSE)
  pars <- function(id) grm[as.character(grm$item) == id, , drop = FALSE]
  p1 <- pars(item1)
  p2 <- pars(item2)
  cuts1 <- c(-Inf, p1$tau, Inf)
  cuts2 <- c(-Inf, p2$tau, Inf)
  cats1 <- sort(unique(data[[item1]]))
  cats2 <- sort(unique(data[[item2]]))
  if (length(cats1) != length(cuts1) - 1L || length(cats2) != length(cuts2) - 1L) stop("The observed categories do not match the fitted thresholds.",
    call. = FALSE
  )
  rho <- p1$lambda[1L] * p2$lambda[1L]
  observed <- table(factor(data[[item1]], levels = cats1), factor(data[[item2]], levels = cats2))
  cells <- expand.grid(category1 = seq_along(cats1), category2 = seq_along(cats2))
  probabilities <- lapply(seq_len(nrow(cells)), function(i) {
    a <- cells$category1[i]
    b <- cells$category2[i]
    sieve_irt_rectangle(cuts1[a], cuts1[a + 1L], cuts2[b], cuts2[b + 1L], rho)
  })
  expected <- nrow(data) * vapply(probabilities, function(x) x$probability, numeric(1))
  data.frame(
    item1 = item1,
    item2 = item2,
    category1 = as.character(cats1[cells$category1]),
    category2 = as.character(cats2[cells$category2]),
    observed = as.numeric(observed), expected = expected, residual = as.numeric(observed) - expected,
    pearson_cell_residual = ifelse(expected > 0, (as.numeric(observed) - expected) / sqrt(expected), NA_real_),
    sparse_expected = expected < 5, n = nrow(data), rho = rho,
    integration_error = vapply(probabilities, function(x) x$absolute_error, numeric(1)), stringsAsFactors = FALSE
  )
}

#' Conditional Model Category Probabilities in the Normal Ogive Metric
#'
#' Experimental: the arguments and the form of the result may change in
#' a later release of SIEVER without a period of deprecation.
#'
#' The category probabilities of the normal ogive graded response model,
#' P(Y >= k | theta) = Phi(a_NO (theta - b_k)), differenced across the
#' boundaries. The discrimination is read in the metric the result's
#' `"metric"` attribute records, as [DMAR::irt_information()] reads it: a
#' logistic a is divided by 1.702 to give a_NO, and a table without the
#' attribute is taken as normal ogive. The probabilities are therefore the
#' same for one fitted model whichever `metric` [DMAR::irt_grm()] reported.
#' (A logistic a used as if it were normal ogive would give the curve of
#' neither model.)
#'
#' @param grm A [DMAR::irt_grm()] result, or a table with columns `item`,
#'   `category`, `a` and `b`. A `"metric"` attribute other than
#'   `"normal_ogive"` or `"logistic"` is refused.
#' @param item Selected item identifier.
#' @param theta Trait grid.
#' @return Numeric probability matrix with columns `category_1` to
#'   `category_m` and one row per value of `theta`, in its order.
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' verbal <- c("t5_general_information", "t6_paragraph_comprehension", "t7_sentence",
#'   "t8_word_classification", "t9_word_meaning")
#' responses <- as.data.frame(lapply(hs[, verbal], function(x) {
#'   as.integer(cut(x, stats::quantile(x, 0:4 / 4), include.lowest = TRUE))
#' }))
#' grm <- DMAR::irt_grm(responses, metric = "normal_ogive")
#' sieve_irt_category_probabilities(grm, verbal[1], theta = c(-2, 0, 2))
#' @export
sieve_irt_category_probabilities <- function(grm, item, theta) {
  metric <- attr(grm, "metric", exact = TRUE)
  if (!is.null(metric) && !(is.character(metric) && length(metric) == 1L &&
    metric %in% c("normal_ogive", "logistic"))) {
    stop(sprintf(
      paste(
        "attr(grm, \"metric\") is %s; sieve_irt_category_probabilities() reads the two discrimination",
        "metrics DMAR::irt_grm() records, \"normal_ogive\" and \"logistic\". Refit with",
        "DMAR::irt_grm(metric = \"normal_ogive\"), or remove the attribute from a table whose a is in",
        "the normal ogive metric."
      ),
      sieve_given_text(metric)
    ), call. = FALSE)
  }
  p <- grm[as.character(grm$item) == item, , drop = FALSE]
  p <- p[order(p$category), , drop = FALSE]
  if (!nrow(p) || any(!is.finite(c(p$a, p$b)))) stop("Finite fitted item parameters are required.", call. = FALSE)
  # The normal ogive discrimination, as DMAR::irt_information() reads it.
  a_normal_ogive <- if (identical(metric, "logistic")) p$a / 1.702 else p$a
  above <- vapply(seq_len(nrow(p)), function(i) stats::pnorm(a_normal_ogive[i] * (theta - p$b[i])), numeric(length(theta)))
  cumulative <- cbind(1, above, 0)
  probability <- cumulative[, -ncol(cumulative), drop = FALSE] - cumulative[, -1L, drop = FALSE]
  if (any(probability < -1e-10) || any(abs(rowSums(probability) - 1) > 1e-10)) stop("The stored boundaries do not define ordered category probabilities.",
    call. = FALSE
  )
  colnames(probability) <- paste0("category_", seq_len(ncol(probability)))
  probability
}

#' Empirical Response Traces From a Rest Score That Excludes the Focal Item
#'
#' @description
#' Experimental: the arguments and the form of the result may change in
#' a later release of SIEVER without a period of deprecation.
#' Groups the respondents by their rest score (the sum of the other fitted
#' items, so that the focal item does not predict itself) into about `bins`
#' groups of similar size, and gives the observed proportion in each
#' category of the focal item within each group, with an exact binomial
#' interval. Set beside the model's category curves, the traces show where
#' the model and the data part; the intervals are descriptive.
#'
#' @param grm Stored GRM result.
#' @param data Exact fitted item data.
#' @param item Focal item identifier.
#' @param bins Number of approximately equal-count rest-score groups.
#' @param conf_level Exact binomial confidence level for each descriptive proportion.
#' @return Category proportions and descriptive intervals by observed rest score.
#' @author Ken Kelley
#' @examples
#' hs <- DMAR::holzinger_swineford
#' verbal <- c("t5_general_information", "t6_paragraph_comprehension", "t7_sentence",
#'   "t8_word_classification", "t9_word_meaning")
#' responses <- as.data.frame(lapply(hs[, verbal], function(x) {
#'   as.integer(cut(x, stats::quantile(x, 0:4 / 4), include.lowest = TRUE))
#' }))
#' grm <- DMAR::irt_grm(responses, metric = "normal_ogive")
#' traces <- sieve_irt_empirical_traces(grm, responses, verbal[1], bins = 4L)
#' head(traces[, c("group", "rest_mean", "category", "proportion", "lower", "upper")])
#' @export
sieve_irt_empirical_traces <- function(grm, data, item, bins = 5L, conf_level = .95) {
  conf_level <- sieve_check_conf_level(conf_level)
  items <- unique(as.character(grm$item))
  other <- setdiff(items, item)
  if (!item %in% items || !length(other)) stop("Choose a fitted item with other items for its rest score.",
    call. = FALSE
  )
  if (length(bins) != 1L || !is.finite(bins) || bins < 2 || bins > 20 || bins != as.integer(bins)) stop("Use 2 to 20 rest-score groups.",
    call. = FALSE
  )
  data <- data[stats::complete.cases(data[, items, drop = FALSE]), items, drop = FALSE]
  # Numeric ordered codes retain the study's actual category spacing. This
  # is a descriptive score axis, not a latent theta estimate or fit test.
  rest <- rowMeans(as.data.frame(lapply(data[, other, drop = FALSE], as.numeric)))
  breaks <- unique(stats::quantile(rest, probs = seq(0, 1, length.out = bins + 1L), names = FALSE))
  if (length(breaks) < 3L) stop("The observed rest score has too few distinct values for grouped traces.",
    call. = FALSE
  )
  group <- cut(rest, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  categories <- sort(unique(data[[item]]))
  do.call(rbind, lapply(sort(unique(group)), function(g) {
    at <- which(group == g)
    n <- length(at)
    do.call(rbind, lapply(categories, function(category) {
      count <- sum(data[[item]][at] == category)
      ci <- stats::binom.test(count, n, conf.level = conf_level)$conf.int
      data.frame(
        group = g, rest_mean = mean(rest[at]), rest_min = min(rest[at]), rest_max = max(rest[at]),
        category = as.character(category), n = n, count = count, proportion = count / n,
        lower = ci[1L], upper = ci[2L], small_group = n < 20L, stringsAsFactors = FALSE
      )
    }))
  }))
}

#' Parameter uncertainty controls for the existing IRT model
#' @param ns Module namespace.
#' @return UI tags.
#' @author Ken Kelley
#' @noRd
sieve_irt_parameter_panel <- function(ns) {
  shiny::tagList(
    shiny::p("All discrimination intervals in this panel use the normal ogive metric. The primary parameter table can display a logistic conversion without changing these calculations. Intervals transform the full joint covariance of standardized categorical factor parameters. Loading-threshold covariance is retained. A near-zero loading or a loading interval reaching the response-variance boundary makes the corresponding transformation unstable; unavailable intervals remain explicit."),
    shiny::sliderInput(ns("evidence_conf"), "Confidence level", .8, .99, .95, .01),
    DT::DTOutput(ns("parameter_uncertainty")),
    shiny::actionButton(ns("parameter_keep"), "Keep these parameter intervals with the saved analyses"),
    shiny::h6("Information uncertainty from estimated item parameters"),
    shiny::p(
      class = "small",
      "These are pointwise parameter-uncertainty bands from joint asymptotic normal draws. They are distinct from conditional SE(theta), which treats item parameters as known. Draws with nonpositive slopes or unordered boundaries are counted as unusable. Percentiles condition on usable draws; this is neither a bootstrap nor a simultaneous band."
    ),
    shiny::numericInput(ns("information_draws"), "Joint parameter draws", 1000L, min = 39L, step = 100L),
    shiny::numericInput(ns("information_seed"), "Seed (optional)", NULL, min = 0),
    shiny::actionButton(ns("information_bands_go"), "Compute parameter-uncertainty bands"),
    shiny::uiOutput(ns("information_bands_note")),
    shiny::plotOutput(ns("information_bands_plot"), height = "320px"), DT::DTOutput(ns("information_bands_table"))
  )
}

#' Local dependence inspection controls
#' @param ns Module namespace.
#' @return UI tags.
#' @author Ken Kelley
#' @noRd
sieve_irt_dependence_panel <- function(ns) {
  shiny::tagList(
    shiny::p("Inspect observed-minus-implied latent-response correlation residuals before treating item information as additive. These are lavaan cor.bentler residual correlations, not Yen's Q3, standardized LD statistics, or calibrated significance tests."),
    DT::DTOutput(ns("local_residuals")),
    shiny::selectInput(ns("pair_item1"), "First fitted item", character()),
    shiny::selectInput(ns("pair_item2"), "Second fitted item", character()),
    shiny::actionButton(ns("pair_go"), "Inspect observed and expected category cells"),
    shiny::p(
      class = "small",
      "Expected cells integrate the fitted bivariate normal latent-response model. Pearson cell residuals are descriptive: parameter estimation and multiple cells preclude interpreting them as independent z tests. Small expected cells are flagged. All rows are the exact complete-case sample used by DMAR::irt_grm."
    ),
    shiny::uiOutput(ns("pair_note")), DT::DTOutput(ns("pair_cells"))
  )
}

#' Category curve and empirical rest-score trace controls
#' @param ns Module namespace.
#' @return UI tags.
#' @author Ken Kelley
#' @noRd
sieve_irt_category_panel <- function(ns) {
  shiny::tagList(
    shiny::p("Model category curves use latent theta. Empirical traces use the observed mean of the other fitted items, excluding the focal item; that rest-score axis is not theta. The two views describe different conditioning variables and are intentionally shown separately. These descriptive traces and marginal checks are not S-X2 item-fit tests."),
    shiny::selectInput(ns("trace_item"), "Focal fitted item", character()),
    shiny::numericInput(ns("trace_bins"), "Rest-score groups", 5L, min = 2L, max = 20L),
    shiny::actionButton(ns("trace_go"), "Inspect category response traces"),
    shiny::uiOutput(ns("trace_note")), shiny::h6("Model probabilities by latent theta"),
    shiny::plotOutput(ns("category_plot"), height = "300px"), DT::DTOutput(ns("category_probabilities")),
    shiny::h6("Observed proportions by item-excluded rest score"),
    shiny::plotOutput(ns("empirical_trace_plot"), height = "300px"), DT::DTOutput(ns("empirical_traces")),
    shiny::h6("Observed and model-implied marginal categories"), DT::DTOutput(ns("category_margins"))
  )
}

#' Wire IRT evidence to the stored fitted result without refitting
#' @param input,output,session Module objects.
#' @param state Shared state.
#' @param result Function returning the stored fitted IRT result and its exact analysis data.
#' @param theta Function returning the selected trait grid.
#' @param is_current Function checking the stored parent request without refitting.
#' @return NULL invisibly.
#' @author Ken Kelley
#' @noRd
sieve_irt_evidence_wire <- function(input, output, session, state, result, theta, is_current = function() TRUE) {
  level <- shiny::reactive(sieve_finite_or(input$evidence_conf, .95))
  # The controls each diagnostic is computed with (see shown_now()).
  bands_request <- function() {
    list(theta = theta(), B = input$information_draws %||% 1000L,
      seed = sieve_seed_input(input$information_seed), conf_level = level())
  }
  pair_request <- function() list(item1 = input$pair_item1, item2 = input$pair_item2)
  trace_request <- function() {
    list(item = input$trace_item, bins = input$trace_bins %||% 5L, theta = theta(), conf_level = level())
  }
  evidence <- shiny::reactive({
    g <- result()
    shiny::req(g$grm)
    sieve_irt_parameter_evidence(g$grm, level())
  })
  table_output <- function(expr) DT::renderDT({
    x <- expr()
    if (is.null(x)) return(NULL)
    sieve_dt(x,
      rownames = FALSE,
      selection = "none",
      escape = TRUE,
      options = list(
        dom = "fltip",
        pageLength = 15,
        scrollX = TRUE
      )
    )
  })
  output$parameter_uncertainty <- table_output(function() {
    e <- evidence()
    if (is.null(e$table)) data.frame(status = e$status, reason = e$reason) else e$table
  })
  shiny::observeEvent(result(),
    {
      g <- result()
      ids <- g$items %||% character()
      shiny::updateSelectInput(session, "pair_item1", choices = ids, selected = ids[1L])
      shiny::updateSelectInput(session, "pair_item2", choices = ids, selected = ids[min(2L, length(ids))])
      shiny::updateSelectInput(session, "trace_item", choices = ids, selected = ids[1L])
    },
    ignoreInit = FALSE
  )
  output$local_residuals <- table_output(function() {
    g <- result()
    shiny::req(g$grm)
    tryCatch(sieve_irt_residual_pairs(g$grm),
      error = function(e) data.frame(
        status = "unavailable",
        reason = conditionMessage(e)
      )
    )
  })
  current_result <- function() {
    if (!isTRUE(is_current())) stop("This fitted model is from an earlier request. Update the graded response model before calculating a new diagnostic.",
      call. = FALSE)
    g <- result()
    shiny::req(g$grm, g$analysis_data)
    if (!sieve_same_revision(
      g$revision,
      sieve_revision(state)
    )) stop("This model is from an earlier data/item revision. Refit it before calculating a new diagnostic.",
      call. = FALSE
    )
    g
  }
  register_evidence <- function(g, kind, choices, estimates, code) {
    spec <- c(list(
      kind = kind, estimator = g$estimator, observed = g$items,
      parent_revision = g$revision
    ), choices)
    spec$key <- sieve_hash(list(spec, lambda = g$grm$lambda, tau = g$grm$tau))
    status <- estimates$status %||% "completed"
    sieve_register_analysis(state, list(
      spec = spec, revision = g$revision,
      inputs = list(items = g$analysis_data, meta = g$analysis_meta), estimates = estimates,
      status = list(
        code = if (startsWith(status, "completed")) "ok" else "unavailable",
        message = estimates$reason %||% estimates$method %||% kind
      ), warnings = character()
    ), note = kind)
    sieve_log(state, kind, c(sieve_irt_replay_code(g$items, g$estimator), code))
  }
  shiny::observeEvent(input$parameter_keep, {
    tryCatch(
      {
        g <- current_result()
        e <- evidence()
        register_evidence(
          g, "IRT parameter uncertainty", list(conf_level = level()), e,
          sprintf("irt_parameter_intervals <- SIEVER::sieve_irt_parameter_evidence(grm, conf_level = %s)", level())
        )
      },
      error = function(e) sieve_notify(conditionMessage(e), type = "error")
    )
  })
  bands <- shiny::eventReactive(input$information_bands_go, {
    tryCatch(
      {
        g <- current_result()
        out <- sieve_irt_information_bands(g$grm,
          theta(),
          input$information_draws %||% 1000L,
          seed = sieve_seed_input(input$information_seed), conf_level = level()
        )
        out$revision <- g$revision
        out$request <- bands_request()
        register_evidence(
          g,
          "IRT information parameter-uncertainty bands",
          list(
            theta = theta(),
            B = input$information_draws %||% 1000L,
            seed = out$rng$seed,
            conf_level = level()
          ),
          out,
          c(
            sprintf(
              "irt_information_bands <- SIEVER::sieve_irt_information_bands(grm, theta = c(%s), B = %d, seed = %s, conf_level = %s)",
              paste(format(theta(), digits = 17L, trim = TRUE), collapse = ", "), input$information_draws %||% 1000L,
              out$rng$seed %||% "NULL", level()
            ),
            if (!is.null(out$attempted)) sprintf(
              "# Joint parameter draws: attempted %d, used %d, failed %d; pointwise, conditional on admissible draws.",
              out$attempted,
              out$used,
              out$failed
            )
          )
        )
        out
      },
      error = function(e) list(status = "unavailable", reason = conditionMessage(e))
    )
  })
  # A diagnostic computed on request (the information bands, a pair's
  # bivariate cells, an item's category traces) belongs to the fitted model
  # and the data revision it was computed from. It is shown only while that
  # model is the current one and the dataset, the respondents in the
  # analysis, the item values and the ledger are those it was computed on;
  # otherwise its panel says it is historical and what to press, as the
  # composite, boundary and Monte Carlo panels do. sieve_revision() reads in
  # isolation, so the dependency on the inputs is taken here.
  # Each diagnostic also keeps the controls it was computed with (the
  # theta grid, draws, seed, level, items, bins) and is compared with the
  # controls shown, as the score-uncertainty panel compares its method and
  # level.
  shown_now <- function(x, what, request = NULL) {
    state$dataset_id
    state$items
    state$meta
    if (is.null(x) || is.null(x$revision)) {
      return(x)
    }
    if (!isTRUE(is_current()) || !sieve_same_revision(x$revision, sieve_revision(state))) {
      why <- sprintf(paste(
        "The %s computed earlier is historical: the data, the respondents in the analysis,",
        "the item values or the item ledger changed after it was computed. Refit the graded",
        "response model, then compute it again."
      ), what)
      return(list(reason = why, error = why, stale = TRUE))
    }
    if (is.function(request) && !is.null(x$request) && !identical(x$request, request())) {
      why <- sprintf(paste(
        "The %s shown was computed with other settings than those now chosen;",
        "press its button again for the settings shown."
      ), what)
      return(list(reason = why, error = why, stale = TRUE))
    }
    x
  }
  bands_now <- shiny::reactive(shown_now(bands(), "information band", bands_request))
  output$information_bands_note <- shiny::renderUI({
    b <- bands_now()
    shiny::p(
      role = "status", class = "small", paste(b$status, b$reason %||% b$method, b$caution),
      if (!is.null(b$attempted)) sprintf(
        "Attempted %d, used %d, failed %d. Seed %s.",
        b$attempted,
        b$used,
        b$failed,
        b$rng$seed
      )
    )
  })
  output$information_bands_table <- table_output(function() bands_now()$table)
  output$information_bands_plot <- shiny::renderPlot(alt = "Test information of the graded response model across the trait, with its pointwise interval band. The values are in the table below the figure.", {
    b <- bands_now()
    shiny::req(b$table)
    d <- b$table
    cols <- sieve_palette_colors(state$palette %||% "nd", 2L)
    graphics::plot(d$theta,
      d$information,
      type = "n",
      ylim = range(
        c(
          d$information_lower,
          d$information_upper
        ),
        finite = TRUE
      ),
      xlab = "Latent theta",
      ylab = "Test information"
    )
    graphics::polygon(
      c(
        d$theta,
        rev(d$theta)
      ),
      c(
        d$information_lower,
        rev(d$information_upper)
      ),
      col = grDevices::adjustcolor(cols[1L],
        alpha.f = .2
      ),
      border = NA
    )
    graphics::lines(d$theta, d$information, col = cols[1L], lwd = 2)
  })
  pair <- shiny::eventReactive(input$pair_go, {
    tryCatch(
      {
        g <- current_result()
        cells <- sieve_irt_bivariate_table(
          g$grm,
          g$analysis_data,
          input$pair_item1,
          input$pair_item2
        )
        register_evidence(
          g,
          "IRT bivariate residual inspection",
          list(
            item1 = input$pair_item1,
            item2 = input$pair_item2
          ),
          list(
            table = cells,
            method = "Descriptive observed-versus-expected bivariate category cells, not a calibrated LD significance test"
          ),
          sprintf(
            "irt_bivariate_cells <- SIEVER::sieve_irt_bivariate_table(grm, items_data[, irt_items, drop = FALSE], %s, %s)",
            deparse(input$pair_item1), deparse(input$pair_item2)
          )
        )
        list(table = cells, revision = g$revision, request = pair_request())
      },
      error = function(e) list(error = conditionMessage(e))
    )
  })
  pair_now <- shiny::reactive(shown_now(pair(), "bivariate residual inspection", pair_request))
  output$pair_note <- shiny::renderUI({
    p <- pair_now()
    shiny::p(
      role = "status",
      class = "small",
      if (!is.null(p$error)) p$error else sprintf(
        "%s versus %s; fitted analytic n = %d. %d cells have expected count below five.",
        p$table$item1[1L], p$table$item2[1L], p$table$n[1L], sum(p$table$sparse_expected)
      )
    )
  })
  output$pair_cells <- table_output(function() pair_now()$table)
  traces <- shiny::eventReactive(input$trace_go, {
    tryCatch(
      {
        g <- current_result()
        item <- input$trace_item
        th <- theta()
        probability <- sieve_irt_category_probabilities(g$grm, item, th)
        empirical <- sieve_irt_empirical_traces(g$grm, g$analysis_data, item, input$trace_bins %||% 5L, level())
        data <- g$analysis_data[stats::complete.cases(g$analysis_data[, g$items, drop = FALSE]), , drop = FALSE]
        pars <- g$grm[as.character(g$grm$item) == item, ,
          drop = FALSE
        ]
        pars <- pars[order(pars$category), ,
          drop = FALSE
        ]
        categories <- sort(unique(data[[item]]))
        expected <- diff(stats::pnorm(c(-Inf, pars$tau, Inf)))
        count <- as.numeric(table(factor(data[[item]], levels = categories)))
        margins <- data.frame(
          category = as.character(categories),
          observed = count,
          model_expected = nrow(data) * expected,
          observed_proportion = count / nrow(data),
          model_probability = expected,
          n = nrow(data),
          sparse_expected = nrow(data) * expected < 5
        )
        saved <- list(
          item = item,
          theta = th,
          probabilities = probability,
          empirical = empirical,
          margins = margins,
          revision = g$revision,
          request = trace_request()
        )
        register_evidence(
          g,
          "IRT category response inspection",
          list(
            item = item,
            bins = input$trace_bins %||% 5L,
            theta = th,
            conf_level = level()
          ),
          saved,
          c(
            sprintf(
              "irt_category_probabilities <- SIEVER::sieve_irt_category_probabilities(grm, %s, theta = c(%s))",
              deparse(item),
              paste(
                format(th,
                  digits = 17L,
                  trim = TRUE
                ),
                collapse = ", "
              )
            ),
            sprintf(
              "irt_empirical_traces <- SIEVER::sieve_irt_empirical_traces(grm, items_data[, irt_items, drop = FALSE], %s, bins = %d, conf_level = %s)",
              deparse(item),
              input$trace_bins %||% 5L,
              level()
            )
          )
        )
        saved
      },
      error = function(e) list(error = conditionMessage(e))
    )
  })
  traces_now <- shiny::reactive(shown_now(traces(), "category response inspection", trace_request))
  output$trace_note <- shiny::renderUI({
    x <- traces_now()
    shiny::p(
      role = "status",
      class = "small",
      if (!is.null(x$error)) x$error else paste(
        "Focal item:",
        x$item,
        ". Exact binomial intervals describe each empirical proportion; groups below n=20 are flagged. The rest score preserves observed category spacing and is not a latent score."
      )
    )
  })
  output$category_probabilities <- table_output(function() {
    x <- traces_now()
    if (is.null(x$probabilities)) return(NULL)
    data.frame(
      theta = x$theta,
      x$probabilities
    )
  })
  output$empirical_traces <- table_output(function() traces_now()$empirical)
  output$category_margins <- table_output(function() traces_now()$margins)
  output$category_plot <- shiny::renderPlot(alt = "Model category probability curves of the chosen item across the trait. The probabilities are in the table below the figure.", {
    x <- traces_now()
    shiny::req(x$probabilities)
    graphics::matplot(x$theta,
      x$probabilities,
      type = "l",
      lty = seq_len(ncol(x$probabilities)),
      col = sieve_palette_colors(
        state$palette %||% "nd",
        ncol(x$probabilities)
      ),
      ylim = c(0, 1), xlab = "Latent theta", ylab = "Model category probability", main = x$item
    )
    graphics::legend("topright",
      colnames(x$probabilities),
      lty = seq_len(ncol(x$probabilities)),
      col = sieve_palette_colors(
        state$palette %||% "nd",
        ncol(x$probabilities)
      ),
      cex = .8
    )
  })
  output$empirical_trace_plot <- shiny::renderPlot(alt = "Observed proportion in each category of the chosen item by rest-score group, with exact binomial intervals. The proportions are in the table below the figure.", {
    x <- traces_now()
    shiny::req(x$empirical)
    d <- x$empirical
    categories <- unique(d$category)
    cols <- sieve_palette_colors(state$palette %||% "nd", length(categories))
    graphics::plot(range(d$rest_mean),
      c(
        0,
        1
      ),
      type = "n",
      xlab = "Observed item-excluded rest-score mean (not theta)",
      ylab = "Observed category proportion",
      main = x$item
    )
    for (i in seq_along(categories)) {
      z <- d[d$category == categories[i], ]
      graphics::lines(z$rest_mean,
        z$proportion,
        col = cols[i],
        lty = i,
        type = "b",
        pch = i
      )
      graphics::arrows(z$rest_mean,
        z$lower,
        z$rest_mean,
        z$upper,
        length = .03,
        angle = 90,
        code = 3,
        col = cols[i]
      )
    }
    graphics::legend("topright",
      paste(
        "category",
        categories
      ),
      col = cols,
      lty = seq_along(categories),
      pch = seq_along(categories),
      cex = .8
    )
  })
  invisible(NULL)
}

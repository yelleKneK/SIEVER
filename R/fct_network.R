# fct_network.R
# Psychometric network models, a trial. Three models of the
# included items, each estimated in psychonetrics (Epskamp, 2020) and read
# against the ledger's constructs: the Gaussian graphical model of the items
# (a partial correlation network; Epskamp & Fried, 2018), the residual
# network model (the ledger's correlated-factors CFA plus a partial
# correlation network among the residuals, which shows local dependence the
# factors leave behind; Epskamp, Rhemtulla & Borsboom, 2017), and the latent
# network model (the same measurement model with a partial correlation
# network among the constructs in place of their correlation matrix). Every
# function here is Shiny-free; mod_network.R places the results.
#
# The sparse solution follows one rule, written once in
# sieve_network_search(): the saturated model is estimated, the edges that
# are not significant at alpha are removed (prune), and edges are added back
# one at a time by the largest modification index while it is significant
# at alpha and, under a likelihood estimator, while the BIC improves
# (stepup). That is the model search psychonetrics documents (Epskamp,
# 2020), with its default alpha of .01. Under the least squares estimator
# for ordered items the information criteria are undefined, so the step-up
# stops on the significance rule alone.
#
# Everything a function returns is kept at full precision; the module's
# renderers round for display. psychonetrics is a Suggests package: every
# entry point checks for it and returns a plain error when it is absent.

#' Read a psychonetrics result without its printed table
#'
#' `psychonetrics::parameters()` and `psychonetrics::fit()` print their
#' table and return it invisibly; the page needs the value only.
#'
#' @param expr A call that prints and returns.
#' @return The value of `expr`.
#' @author Ken Kelley
#' @noRd
sieve_network_quiet <- function(expr) {
  value <- NULL
  utils::capture.output(value <- expr)
  value
}

#' The engine's estimator, from SIEVER's
#'
#' Maps the estimator SIEVER resolved for the ledger (`sieve_lavaan_spec()`:
#' ML with FIML for continuous items, DMAR's default, WLSMV on polychoric
#' correlations when any item is ordered or binary, or the researcher's own
#' choice, MLR with FIML among them) to the arguments psychonetrics takes.
#' ML with FIML is psychonetrics' FIML estimator, on complete data too, where
#' its likelihood is the ML likelihood: on the nine and the 24 complete
#' Holzinger and Swineford tests its sparse networks equal the ML engine's
#' (the same edges; partial correlations within 2e-5, standard errors within
#' 2e-6) in about the ML engine's time (24 tests, item network: 9.3 seconds
#' against 8.1), so its wait is read from the ML times then
#' (`sieve_network_wait_class()`). Robust maximum likelihood (`"MLR"`) needs
#' the raw rows kept in the model and complete data in the installed
#' version, so incomplete continuous data are estimated by full information
#' maximum likelihood with normal theory standard errors instead, and the
#' label says so. Under MLR the search itself runs under plain maximum
#' likelihood and the sparse model is then re-estimated once under MLR
#' (`search_estimator = "ML"`, `estimator = "MLR"`): psychonetrics
#' recomputes its robust information at every step of the search, which on
#' the 24 Holzinger and Swineford tests took 205 seconds for the item
#' network against 6 seconds under ML, while the one robust re-estimation
#' of the sparse model takes 7 seconds, keeps the same edges and point
#' estimates, and gives the robust standard errors and fit measures. The
#' pruning decisions are therefore made with normal
#' theory tests, and the reported solution is robust. WLSMV in lavaan's
#' terms is diagonally weighted least
#' squares (`"DWLS"`) with the mean and variance adjusted test in
#' psychonetrics, on the polychoric correlations of the ordered items, on
#' the complete cases.
#'
#' @param estimator A lavaan estimator name (`"MLR"`, `"ML"`, `"WLSMV"`).
#' @param missing `"fiml"` or `"listwise"`.
#' @param ordered The names of the items treated as ordered, or `NULL`.
#' @param data The item data the model will read (to see whether any value
#'   is missing).
#' Under the likelihood estimators the items are standardized before
#' estimation (`standardize = "z"`, each item by its sample mean and
#' standard deviation): partial correlations, fit measures and construct
#' correlations are invariant to the scale of the items, and the loadings
#' are then the completely standardized ones, but psychonetrics' optimizer
#' is not invariant. On the raw score scale of the bundled Holzinger and
#' Swineford tests (standard deviations from 3.5 to 36) its plain CFA
#' stopped at a chi square of 87.7 where lavaan reaches 85.3, and the
#' saturated latent network at 86.5; standardized, every one of them
#' reaches 85.3055. The polychoric input of the ordered items
#' needs no standardization and takes none.
#'
#' @return A list with `estimator` (the psychonetrics name of the reported
#'   solution), `search_estimator` (the estimator of the search, the same
#'   unless the solution is re-estimated), `missing`,
#'   `ordered` (a character vector, possibly empty), `storedata`,
#'   `standardize` (`TRUE` under the likelihood estimators), `criterion`
#'   (the step-up criterion: `"bic"` or `"none"`), `lavaan_estimator` (the
#'   rate class for the wait estimate), `family` (the fit index family to
#'   read: `"robust"`, `"scaled"` or `"standard"`) and `label` (the
#'   estimator in words).
#' @author Ken Kelley
#' @noRd
sieve_network_engine <- function(estimator = "ML", missing = "fiml", ordered = NULL, data = NULL) {
  estimator <- toupper(as.character(estimator %||% "ML"))[1L]
  ordered <- as.character(ordered %||% character(0))
  incomplete <- !is.null(data) && anyNA(data)
  if (estimator %in% sieve_categorical_estimators()) {
    return(list(
      estimator = "DWLS", search_estimator = "DWLS", missing = "listwise", ordered = ordered, storedata = FALSE,
      standardize = FALSE, criterion = "none", lavaan_estimator = "WLSMV", family = "scaled",
      label = paste0(
        "diagonally weighted least squares (WLSMV in lavaan's terms) on the polychoric ",
        "correlations of the ordered items, on the complete cases"
      )
    ))
  }
  if (identical(estimator, "MLR") && !incomplete) {
    return(list(
      estimator = "MLR", search_estimator = "ML", missing = "listwise", ordered = character(0), storedata = TRUE,
      standardize = TRUE, criterion = "bic", lavaan_estimator = "MLR", family = "robust",
      label = paste(
        "maximum likelihood on the covariances of the standardized items, complete data, for the search;",
        "the sparse model re-estimated once under robust maximum likelihood (MLR) for its standard errors and fit"
      )
    ))
  }
  if (identical(missing, "fiml") || identical(estimator, "MLR")) {
    return(list(
      estimator = "FIML", search_estimator = "FIML", missing = "fiml", ordered = character(0), storedata = FALSE,
      standardize = TRUE, criterion = "bic", lavaan_estimator = "ML", family = "standard",
      # FIML on complete data runs in about the ML engine's time (see above).
      complete = !incomplete,
      label = paste0(
        "full information maximum likelihood on the covariances of the standardized items, incomplete cases included",
        if (identical(estimator, "MLR")) {
          " (psychonetrics applies its robust correction to complete data only, so the standard errors are normal theory)"
        } else ""
      )
    ))
  }
  list(
    estimator = "ML", search_estimator = "ML", missing = "listwise", ordered = character(0), storedata = FALSE,
    standardize = TRUE, criterion = "bic", lavaan_estimator = "ML", family = "standard",
    label = "maximum likelihood on the covariances of the standardized items, complete cases"
  )
}

#' The measurement model of the ledger as a loading pattern
#'
#' The residual and latent network models take the ledger's constructs as a
#' confirmatory measurement model: one factor per construct, each item
#' loading on its construct only, the factor variances fixed at 1
#' (`identification = "variance"`, the identification SIEVER uses
#' everywhere). A construct needs at least `min_items` included items to be
#' a factor; the items of a smaller construct, and items without a
#' construct, are left out of these two models and named in `not_modeled`.
#' The latent names come from `sieve_name_plan()`, so a construct has the
#' same name here as in the CFA.
#'
#' @param meta The item ledger.
#' @param items The item columns available in the data.
#' @param min_items Minimum included items for a construct to be a factor.
#' @return A list with `lambda` (a 0/1 matrix, items by factors, with
#'   dimnames), `items`, `latents`, `labels` (construct labels in factor
#'   order), `name_map` (latent name to label), `construct` (the construct
#'   of each modeled item), `not_modeled` (a data frame with `item`,
#'   `construct` and `reason`), and `reason` (why no model can be written,
#'   or `NA`).
#' @author Ken Kelley
#' @noRd
sieve_network_lambda <- function(meta, items, min_items = 2L) {
  active <- meta[meta$status == "active" & meta$item %in% items, , drop = FALSE]
  has_construct <- sieve_construct_assigned(active$construct)
  not_modeled <- data.frame(
    item = as.character(active$item[!has_construct]),
    construct = rep(NA_character_, sum(!has_construct)),
    reason = rep("no construct is assigned", sum(!has_construct)),
    stringsAsFactors = FALSE
  )
  grouped <- active[has_construct, , drop = FALSE]
  cons <- unique(as.character(grouped$construct))
  sizes <- vapply(cons, function(cn) sum(grouped$construct == cn), integer(1))
  small <- cons[sizes < min_items]
  if (length(small)) {
    its <- as.character(grouped$item[grouped$construct %in% small])
    not_modeled <- rbind(not_modeled, data.frame(
      item = its, construct = as.character(grouped$construct[grouped$construct %in% small]),
      reason = sprintf("its construct has fewer than %d included items", as.integer(min_items)),
      stringsAsFactors = FALSE
    ))
  }
  keep <- cons[sizes >= min_items]
  used <- grouped[grouped$construct %in% keep, , drop = FALSE]
  out <- list(
    lambda = NULL, items = as.character(used$item), latents = character(0), labels = keep,
    name_map = character(0), construct = as.character(used$construct),
    not_modeled = not_modeled, reason = NA_character_
  )
  if (!length(keep)) {
    out$reason <- sprintf(
      "No construct has at least %d included items, so there is no measurement model to build the network on.",
      as.integer(min_items)
    )
    return(out)
  }
  plan_names <- sieve_name_plan(meta, reserved = items)
  latents <- sieve_latent_name(plan_names, keep)
  lambda <- matrix(0, nrow = nrow(used), ncol = length(keep),
    dimnames = list(as.character(used$item), latents))
  for (j in seq_along(keep)) {
    lambda[used$construct == keep[j], j] <- 1
  }
  out$lambda <- lambda
  out$latents <- latents
  out$name_map <- stats::setNames(keep, latents)
  out
}

#' The sparse network: prune, then step up
#'
#' The one rule behind every network this page shows (see the file header).
#' The saturated model is estimated first; the edges whose Wald test is not
#' significant at `alpha` are set to zero and the model is estimated again
#' (`psychonetrics::prune()`, one pass, no multiplicity adjustment, the
#' package default); then edges are added back one at a time, the largest
#' modification index first, while that index is significant at `alpha`
#' and, under a likelihood estimator, while the BIC improves
#' (`psychonetrics::stepup()`). The two steps together are the model search
#' Epskamp (2020) describes for psychonetrics, with its default alpha of
#' .01. Under DWLS the information criteria are undefined, so `criterion =
#' "none"` and the step-up stops on the significance rule alone.
#'
#' @param model An unrun psychonetrics model.
#' @param alpha The significance level for keeping an edge.
#' @param criterion `"bic"` or `"none"` (see `sieve_network_engine()`).
#' @param refit An estimator to re-estimate the sparse model under once the
#'   search is done (`"MLR"`, which needs the model built with
#'   `storedata = TRUE`), or `NULL` to report the search's own solution.
#' @return The estimated sparse model, with the attribute
#'   `sieve_search_optimizer`: the optimizer's verdict at each stage the
#'   engine returns (the saturated model, the pruned model, the model after
#'   the step-up, and the refit when there is one), as
#'   `sieve_network_optimizer_table()` reads it, for
#'   `sieve_network_status()` to judge.
#' @author Ken Kelley
#' @noRd
sieve_network_search <- function(model, alpha = 0.01, criterion = "bic", refit = NULL) {
  # Each stage's warnings are noted where they arise and still reach the
  # caller's capture; the stage a warning came from decides whether it
  # refuses the result or qualifies it.
  record <- new.env(parent = emptyenv())
  record$stages <- list()
  stage <- function(name, expr) {
    record$seen <- character(0)
    value <- withCallingHandlers(expr, warning = function(w) {
      record$seen <- c(record$seen, conditionMessage(w))
    })
    record$stages[[name]] <- list(model = value, warnings = record$seen)
    value
  }
  run <- stage("saturated", psychonetrics::runmodel(model))
  pruned <- stage("pruned", psychonetrics::prune(run, alpha = alpha, adjust = "none", recursive = FALSE))
  sparse <- stage("stepped_up", psychonetrics::stepup(pruned, alpha = alpha, criterion = criterion))
  out <- if (is.null(refit)) {
    sparse
  } else {
    stage("refit", psychonetrics::runmodel(psychonetrics::setestimator(sparse, refit)))
  }
  attr(out, "sieve_search_optimizer") <- sieve_network_optimizer_table(record$stages)
  out
}

#' The optimizer's verdict at each stage of a network search
#'
#' psychonetrics returns a model whether or not its optimizer converged: the
#' optimizer's completion code (0 is success) and message are kept in the
#' model's `optim` slot, and the engine warns "Model might not have converged
#' properly" when the gradient at the returned estimates is large. Finite
#' estimates and standard errors do not show that the optimizer converged.
#'
#' @param stages A named list, one element per stage, each a list with the
#'   estimated `model` and the `warnings` raised while it was estimated.
#' @return A data frame with `stage`, `code` (the completion code, `NA`
#'   when the engine recorded none), `message`, and `gradient_warning`
#'   (whether the engine's large-gradient warning was raised in that stage).
#' @author Ken Kelley
#' @noRd
sieve_network_optimizer_table <- function(stages) {
  rows <- lapply(names(stages), function(name) {
    s <- stages[[name]]
    opt <- tryCatch(s$model@optim, error = function(e) NULL)
    code <- if (is.list(opt) && length(opt$convergence)) {
      suppressWarnings(as.integer(opt$convergence[[1L]]))
    } else {
      NA_integer_
    }
    message <- if (is.list(opt) && length(opt$message)) as.character(opt$message[[1L]]) else ""
    data.frame(
      stage = name, code = code, message = if (is.na(message)) "" else message,
      gradient_warning = any(grepl("might not have converged", s$warnings, fixed = TRUE)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' The free edges of a network matrix, with standard errors and intervals
#'
#' Reads `psychonetrics::parameters()` for one symmetric network matrix
#' (`"omega"` for the item network, `"omega_epsilon"` for the residual
#' network, `"omega_zeta"` for the latent network) and returns every pair
#' below the diagonal: the partial correlation, its standard error and
#' p-value where the edge is estimated, and a Wald interval
#' (estimate plus or minus the normal quantile times the standard error),
#' the interval the engine's standard errors support (psychonetrics reports
#' no interval of its own; a bootstrap of the network would be the
#' alternative, Epskamp, Borsboom & Fried, 2018). An edge the search set to
#' zero is kept in the table as zero with no standard error, so the table
#' says which pairs the model holds at zero rather than hiding them.
#'
#' @param model An estimated psychonetrics model.
#' @param matrix The matrix name.
#' @param labels A named character vector, variable name to display label,
#'   or `NULL`.
#' @param level The confidence level of the Wald intervals.
#' @return A data frame with `from`, `to`, `from_label`, `to_label`,
#'   `estimate`, `se`, `p`, `lower`, `upper` and `estimated` (logical),
#'   ordered by the absolute estimate.
#' @author Ken Kelley
#' @noRd
sieve_network_edges <- function(model, matrix = "omega", labels = NULL, level = 0.95) {
  pars <- sieve_network_quiet(psychonetrics::parameters(model))
  rows <- pars[pars$matrix == matrix & pars$row != pars$col, , drop = FALSE]
  if (!nrow(rows)) {
    return(data.frame(
      from = character(0), to = character(0), from_label = character(0), to_label = character(0),
      estimate = numeric(0), se = numeric(0), p = numeric(0), lower = numeric(0), upper = numeric(0),
      estimated = logical(0), stringsAsFactors = FALSE
    ))
  }
  rows <- rows[rows$row > rows$col, , drop = FALSE]
  z <- stats::qnorm(1 - (1 - level) / 2)
  free <- !as.logical(rows$fixed)
  se <- ifelse(free, as.numeric(rows$se), NA_real_)
  est <- as.numeric(rows$est)
  show <- function(x) {
    x <- as.character(x)
    if (is.null(labels)) {
      return(x)
    }
    hit <- match(x, names(labels))
    ifelse(is.na(hit), x, unname(labels[hit]))
  }
  out <- data.frame(
    from = as.character(rows$var1), to = as.character(rows$var2),
    from_label = show(rows$var1), to_label = show(rows$var2),
    estimate = est, se = se, p = ifelse(free, as.numeric(rows$p), NA_real_),
    lower = est - z * se, upper = est + z * se, estimated = free,
    stringsAsFactors = FALSE
  )
  out <- out[order(-abs(out$estimate), out$from, out$to), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "level") <- level
  out
}

#' The fit measures of a network model, in SIEVER's names
#'
#' Reads `psychonetrics::fit()` and returns the complete set as a named
#' numeric vector (`all`, with the engine's names) and the selection the
#' pages show in SIEVER's names (`chisq`, `df`, `pvalue`, `cfi`, `tli`,
#' `rmsea`, `rmsea_lo`, `rmsea_hi`, `srmr`, `bic`, `npar`), read from the
#' family the estimator supports: the robust indices and the scaled test
#' under MLR, the scaled indices and test under DWLS (psychonetrics gives
#' no interval for the scaled RMSEA, so the interval shown is the standard
#' one, and the table says so), the standard ones otherwise.
#'
#' psychonetrics forms its RMSEA interval at 90 percent only. The interval
#' shown is at `conf_level`, the page's level, computed by
#' `sieve_network_rmsea_interval()` from
#' psychonetrics' own chi square, degrees of freedom and N with
#' `DMAR::sem_fit_measures()`; at 0.90 it reproduces psychonetrics'
#' interval. psychonetrics' 90 percent limits stay in `all`. The selection
#' carries the attributes `family`, `test_family`, `rmsea_ci_family` and
#' `ci_level` that `sieve_fit_table()` reads.
#'
#' @param model An estimated psychonetrics model.
#' @param family `"robust"`, `"scaled"` or `"standard"`.
#' @param conf_level The confidence level of the RMSEA interval.
#' @return A list with `all` and `selected`, both named numeric vectors.
#' @author Ken Kelley
#' @noRd
sieve_network_fit <- function(model, family = "standard", conf_level = sieve_default_rmsea_conf_level()) {
  conf_level <- sieve_check_conf_level(conf_level)
  tab <- sieve_network_quiet(psychonetrics::fit(model))
  all <- stats::setNames(as.numeric(tab$Value), as.character(tab$Measure))
  pick <- function(...) {
    for (nm in c(...)) {
      if (nm %in% names(all) && is.finite(all[[nm]])) {
        return(unname(all[[nm]]))
      }
    }
    NA_real_
  }
  index_family <- family
  test_family <- if (identical(family, "standard")) "standard" else "scaled"
  selected <- c(
    npar = pick("npar"),
    chisq = if (identical(test_family, "scaled")) pick("chisq.scaled", "chisq") else pick("chisq"),
    df = if (identical(test_family, "scaled")) pick("df.scaled", "df") else pick("df"),
    pvalue = if (identical(test_family, "scaled")) pick("pvalue.scaled", "pvalue") else pick("pvalue"),
    cfi = switch(family, robust = pick("cfi.robust", "cfi"), scaled = pick("cfi.scaled", "cfi"), pick("cfi")),
    tli = switch(family, robust = pick("tli.robust", "tli"), scaled = pick("tli.scaled", "tli"), pick("tli")),
    rmsea = switch(family, robust = pick("rmsea.robust", "rmsea"), scaled = pick("rmsea.scaled", "rmsea"), pick("rmsea")),
    rmsea_lo = switch(family, robust = pick("rmsea.ci.lower.robust", "rmsea.ci.lower"), pick("rmsea.ci.lower")),
    rmsea_hi = switch(family, robust = pick("rmsea.ci.upper.robust", "rmsea.ci.upper"), pick("rmsea.ci.upper")),
    srmr = pick("srmr"),
    bic = pick("bic")
  )
  # The families actually read: an index absent in its family falls back to
  # the standard one, and the attribute then says so.
  if (identical(family, "robust") && !("cfi.robust" %in% names(all) && is.finite(all[["cfi.robust"]]))) {
    index_family <- "standard"
  }
  if (identical(family, "scaled") && !("cfi.scaled" %in% names(all) && is.finite(all[["cfi.scaled"]]))) {
    index_family <- "standard"
  }
  if (identical(test_family, "scaled") && !("chisq.scaled" %in% names(all) && is.finite(all[["chisq.scaled"]]))) {
    test_family <- "standard"
  }
  attr(selected, "family") <- index_family
  attr(selected, "test_family") <- test_family
  # The family of the interval actually read: psychonetrics gives no
  # interval for the scaled RMSEA (DWLS), and a robust interval only when
  # it reports one, so the interval is labeled by what it is, which can
  # differ from the RMSEA beside it.
  ci_family <- if (identical(family, "robust") &&
    "rmsea.ci.lower.robust" %in% names(all) && is.finite(all[["rmsea.ci.lower.robust"]])) "robust" else "standard"
  # The interval at the RMSEA level (sieve_default_rmsea_conf_level(), 0.90
  # by default), from psychonetrics' statistics; psychonetrics' own limits
  # are at 90 percent only, which DMAR's route reproduces at that level.
  interval <- sieve_network_rmsea_interval(all, ci_family,
    N = sieve_network_nobs(model), groups = sieve_network_groups(model), conf_level = conf_level)
  if (identical(ci_family, "robust") && !isTRUE(interval$ok)) {
    # A robust interval DMAR's route cannot rebuild: the standard one, named so.
    ci_family <- "standard"
    interval <- sieve_network_rmsea_interval(all, ci_family,
      N = sieve_network_nobs(model), groups = sieve_network_groups(model), conf_level = conf_level)
  }
  selected[["rmsea_lo"]] <- interval$lower
  selected[["rmsea_hi"]] <- interval$upper
  attr(selected, "rmsea_ci_family") <- ci_family
  attr(selected, "ci_level") <- conf_level
  attr(selected, "rmsea_ci_reason") <- interval$reason
  list(all = all, selected = selected)
}

#' The recorded R of a network model's RMSEA interval at the RMSEA level
#'
#' psychonetrics forms the interval at 90 percent only; the page's interval
#' (`sieve_network_rmsea_interval()`) is `DMAR::sem_fit_measures()` on
#' psychonetrics' statistics at the RMSEA level (90 percent by default, at
#' which it gives psychonetrics' own limits), written out so the script
#' gives the limits the page shows at any level.
#'
#' @param name The object name stem.
#' @param family The fit family the page reads (`engine$family`).
#' @param conf_level The level of the RMSEA interval.
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_network_rmsea_code <- function(name, family = "standard", conf_level = sieve_default_rmsea_conf_level()) {
  level <- sieve_num_lit(conf_level)
  call <- function(chisq, df) {
    sprintf(paste0(
      "%s_rmsea <- DMAR::sem_fit_measures(chisq = %s, df = %s, N = sum(%s@sample@groups$nobs), ",
      "n_groups = nrow(%s@sample@groups), conf_level = %s, sample_size_convention = \"N\")"
    ), name, chisq, df, name, name, level)
  }
  head <- c(
    sprintf("# The RMSEA interval at %s, the level of every RMSEA interval SIEVER shows (psychonetrics' own is 90%%),",
      sieve_ci_percent(conf_level)),
    "# from psychonetrics' chi square, df and N by DMAR, with psychonetrics' conventions (T formed with N).",
    sprintf("%s_fv <- stats::setNames(%s_fit$Value, %s_fit$Measure)", name, name, name)
  )
  robust <- c(
    sprintf("%s_robust <- is.finite(%s_fv[\"rmsea.ci.lower.robust\"])  # robust interval (MLR, complete data)", name, name),
    sprintf("if (isTRUE(%s_robust)) {", name),
    "  # Brosseau-Liard, Savalei & Li (2012, Eq. 10): the scaled statistic, limits times sqrt(c df_s / df).",
    paste0("  ", call(sprintf("%s_fv[[\"chisq.scaled\"]]", name), sprintf("%s_fv[[\"df.scaled\"]]", name))),
    sprintf(paste0("  %s_c <- sqrt(%s_fv[[\"chisq.scaling.factor.sb\"]] * %s_fv[[\"df.scaled\"]] / ",
      "%s_fv[[\"df\"]])"), name, name, name, name),
    "} else {",
    paste0("  ", call(sprintf("%s_fv[[\"chisq\"]]", name), sprintf("%s_fv[[\"df\"]]", name))),
    sprintf("  %s_c <- 1", name),
    "}",
    sprintf("%s_c * %s_rmsea$value[%s_rmsea$term %%in%% c(\"rmsea_ci_lower\", \"rmsea_ci_upper\")]", name, name, name)
  )
  standard <- c(
    call(sprintf("%s_fv[[\"chisq\"]]", name), sprintf("%s_fv[[\"df\"]]", name)),
    sprintf("%s_rmsea[%s_rmsea$term %%in%% c(\"rmsea_ci_lower\", \"rmsea_ci_upper\"), ]", name, name)
  )
  c(head, if (identical(family, "robust")) robust else standard)
}

#' The fit measures a network page shows
#'
#' The selection of `sieve_network_fit()`. A saved result that carries no
#' level for its RMSEA interval has psychonetrics' own interval, at 90
#' percent, and is labeled so.
#'
#' @param fit The `fit` element of a network result.
#' @return The selected measures with their `ci_level`.
#' @author Ken Kelley
#' @noRd
sieve_network_selected <- function(fit) {
  m <- fit$selected
  if (!is.null(m) && is.na(sieve_fit_ci_level(m))) attr(m, "ci_level") <- 0.90
  m
}

#' The number of groups of a psychonetrics model
#'
#' The rows of the model's sample groups (`model@sample@groups`), the G of
#' psychonetrics' multiple group RMSEA.
#'
#' @param model An estimated psychonetrics model.
#' @return The number (1 for a single group).
#' @author Ken Kelley
#' @noRd
sieve_network_groups <- function(model) {
  g <- tryCatch(nrow(model@sample@groups), error = function(e) NA_integer_)
  if (length(g) == 1L && is.finite(g) && g >= 1) as.integer(g) else 1L
}

#' The RMSEA interval of a network model at a confidence level
#'
#' psychonetrics forms the RMSEA interval only at 90 percent. The interval
#' at another level is computed here, with `DMAR::sem_fit_measures()` from
#' the same statistics psychonetrics inverts and with its conventions (the
#' statistic formed with N, `sample_size_convention = "N"`; the multiple
#' group RMSEA, times the square root of the number of groups):
#'
#' * standard: the chi square and its degrees of freedom (Browne & Cudeck,
#'   1993; Steiger, 1998);
#' * robust (MLR on complete data): the scaled statistic on its degrees of
#'   freedom, the limits multiplied by the square root of c df_s / df, with
#'   c the Satorra-Bentler scaling constant psychonetrics reports
#'   (`chisq.scaling.factor.sb`), the interval of Brosseau-Liard, Savalei
#'   and Li (2012, Eq. 10) that psychonetrics and lavaan form.
#'
#' At 0.90 the limits equal psychonetrics' to the precision of its root
#' finder (`uniroot()` at its default tolerance); DMAR inverts the
#' noncentral chi square to full precision.
#'
#' @param all psychonetrics' fit measures, a named numeric vector.
#' @param ci_family `"standard"` or `"robust"`.
#' @param N The number of cases (`sieve_network_nobs()`, psychonetrics'
#'   `sampleSize`).
#' @param groups The number of groups.
#' @param conf_level The confidence level.
#' @return A list with `ok`, `lower`, `upper`, `chisq`, `df`, `multiplier`
#'   and `reason` (why there is no interval, or `NULL`).
#' @author Ken Kelley
#' @noRd
sieve_network_rmsea_interval <- function(all, ci_family = "standard", N, groups = 1L,
                                         conf_level = sieve_default_rmsea_conf_level()) {
  val <- function(nm) if (nm %in% names(all) && is.finite(all[[nm]])) unname(all[[nm]]) else NA_real_
  out <- list(ok = FALSE, lower = NA_real_, upper = NA_real_, chisq = NA_real_, df = NA_real_,
    multiplier = 1, reason = NULL)
  df <- val("df")
  if (identical(ci_family, "robust")) {
    chisq <- val("chisq.scaled")
    df_s <- val("df.scaled")
    c_hat <- val("chisq.scaling.factor.sb")
    if (!is.finite(c_hat)) c_hat <- val("chisq.scaling.factor")
    multiplier <- sqrt(c_hat * df_s / df)
  } else {
    chisq <- val("chisq")
    df_s <- df
    multiplier <- 1
  }
  out$chisq <- chisq
  out$df <- df_s
  out$multiplier <- multiplier
  if (!is.finite(chisq) || !is.finite(df_s) || !is.finite(multiplier) || !is.finite(N)) {
    out$reason <- "psychonetrics returned no statistic to form the RMSEA interval from."
    return(out)
  }
  if (df_s < 1) {
    out$reason <- "The model has no degrees of freedom, so the RMSEA has no interval."
    return(out)
  }
  run <- sieve_capture(DMAR::sem_fit_measures(chisq = chisq, df = df_s, N = N, n_groups = groups,
    conf_level = conf_level, sample_size_convention = "N"))
  if (!run$ok) {
    out$reason <- sprintf("DMAR did not form the interval (%s).", sub("[.]$", "", run$error %||% "no reason given"))
    return(out)
  }
  v <- stats::setNames(run$value$value, run$value$term)
  out$lower <- multiplier * unname(v[["rmsea_ci_lower"]])
  out$upper <- multiplier * unname(v[["rmsea_ci_upper"]])
  out$ok <- is.finite(out$lower) && is.finite(out$upper)
  if (!out$ok) out$reason <- "DMAR returned no RMSEA interval for these statistics."
  out
}

#' Node strength and expected influence
#'
#' Two summaries of a node's place in a partial correlation network, each
#' computed from the sparse network matrix: strength, the sum of the
#' absolute edge weights at the node (Opsahl, Agneessens & Skvoretz, 2010),
#' and one-step expected influence, the signed sum of the same weights
#' (Robinaugh, Millner & McNally, 2016), which differs from strength only
#' when a node carries negative edges. Both are descriptions of the
#' estimated network, not tests, and they inherit the sampling error of
#' every edge (Epskamp, Borsboom & Fried, 2018).
#'
#' @param omega A symmetric network matrix with dimnames.
#' @param labels A named character vector of display labels, or `NULL`.
#' @return A data frame with `node`, `label`, `strength`,
#'   `expected_influence` and `degree` (the number of estimated edges),
#'   ordered by strength.
#' @author Ken Kelley
#' @noRd
sieve_network_centrality <- function(omega, labels = NULL) {
  W <- as.matrix(omega)
  diag(W) <- 0
  nodes <- rownames(W) %||% paste0("V", seq_len(nrow(W)))
  show <- if (is.null(labels)) nodes else ifelse(is.na(match(nodes, names(labels))), nodes, unname(labels[match(nodes, names(labels))]))
  out <- data.frame(
    node = nodes, label = show,
    strength = unname(rowSums(abs(W))),
    expected_influence = unname(rowSums(W)),
    degree = unname(rowSums(W != 0)),
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$strength, out$node), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Communities of the network, read against the constructs
#'
#' Walktrap community detection (Pons & Latapy, 2006) on the absolute edge
#' weights, through igraph, with four steps, the setting exploratory graph
#' analysis uses on a psychometric network (Golino & Epskamp, 2017). The
#' communities are then read against the ledger's constructs: a cross
#' table, and the number of items whose community is the one most of their
#' construct's items fall in. A community is a description of the
#' estimated network; it is not a factor, and agreement with the
#' constructs is evidence about the item pool, not a test of the model.
#'
#' @param omega A symmetric network matrix with dimnames.
#' @param constructs The construct of each node (in the order of the
#'   matrix), `NA` for none.
#' @param labels A named character vector of display labels, or `NULL`.
#' @param steps The walktrap steps.
#' @return A list with `membership` (a data frame with `node`, `label`,
#'   `community`, `construct`), `table` (communities by constructs),
#'   `modularity`, `n_communities`, `agreement` (the count of items placed
#'   with the majority of their construct), `n_items`, `isolated` (nodes
#'   with no edge), and `steps`.
#' @author Ken Kelley
#' @noRd
sieve_network_communities <- function(omega, constructs = NULL, labels = NULL, steps = 4L) {
  W <- abs(as.matrix(omega))
  diag(W) <- 0
  nodes <- rownames(W) %||% paste0("V", seq_len(nrow(W)))
  dimnames(W) <- list(nodes, nodes)
  g <- igraph::graph_from_adjacency_matrix(W, mode = "undirected", weighted = TRUE, diag = FALSE)
  wt <- igraph::cluster_walktrap(g, steps = steps)
  member <- as.integer(igraph::membership(wt))
  show <- if (is.null(labels)) nodes else ifelse(is.na(match(nodes, names(labels))), nodes, unname(labels[match(nodes, names(labels))]))
  cons <- if (is.null(constructs)) rep(NA_character_, length(nodes)) else as.character(constructs)
  membership <- data.frame(
    node = nodes, label = show, community = member, construct = cons,
    stringsAsFactors = FALSE
  )
  tab <- table(community = member, construct = ifelse(is.na(cons), "(none)", cons))
  # Items placed with the majority of their construct: for each community
  # the construct most of its items belong to; an item agrees when its
  # construct is that one.
  agreement <- 0L
  if (any(!is.na(cons))) {
    for (cm in sort(unique(member))) {
      inside <- cons[member == cm]
      inside <- inside[!is.na(inside)]
      if (length(inside)) {
        agreement <- agreement + max(table(inside))
      }
    }
  }
  list(
    membership = membership, table = tab,
    modularity = tryCatch(igraph::modularity(wt), error = function(e) NA_real_),
    n_communities = length(unique(member)),
    agreement = as.integer(agreement), n_items = sum(!is.na(cons)),
    isolated = nodes[rowSums(W) == 0], steps = as.integer(steps)
  )
}

#' The correlation matrix a latent network implies
#'
#' A latent network model writes the construct covariance matrix as
#' `Delta (I - Omega)^-1 Delta`, with `Omega` the partial correlations among
#' the constructs and `Delta` a diagonal scaling (Epskamp, Rhemtulla &
#' Borsboom, 2017). psychonetrics identifies the latent network by fixing
#' `Delta` to the identity (its `identification = "variance"` for this
#' family), so the construct variances are the diagonal of
#' `(I - Omega)^-1`, not 1, and the loadings are on that scale. The
#' correlation matrix is the standardized form of the implied covariance
#' matrix, the quantity the CFA page reports as Phi. When the latent
#' network is saturated the two are the same matrix exactly (the two models
#' are one covariance structure); after the search removed an edge, the
#' implied correlation is what the sparse network reproduces, and the
#' correlation of two constructs whose edge was removed is carried by their
#' edges to the others.
#'
#' @param omega The partial correlation matrix among the constructs.
#' @param delta The diagonal scaling matrix (`getmatrix(model,
#'   "delta_zeta")`).
#' @return A correlation matrix with the dimnames of `omega`, with the
#'   attribute `covariance` (the implied covariance matrix, whose diagonal
#'   holds the construct variances).
#' @author Ken Kelley
#' @noRd
sieve_network_implied_cor <- function(omega, delta) {
  O <- as.matrix(omega)
  D <- as.matrix(delta)
  k <- nrow(O)
  S <- D %*% solve(diag(k) - O) %*% D
  dimnames(S) <- dimnames(O)
  R <- stats::cov2cor(S)
  attr(R, "covariance") <- S
  R
}

#' The status of an estimated network model
#'
#' psychonetrics returns a model whether or not the optimizer converged, and
#' warns when the information matrix is not positive definite (a model that
#' may not be identified). The optimizer's own verdict is read first: a
#' stage of the search whose optimizer
#' stopped with a nonzero completion code, or a single estimation (the MLR
#' refit, or a model without its search's record) after which the engine
#' warned of a large gradient, gives no solution, because the network
#' either is not at an optimum or was selected from estimates that were
#' not. The large-gradient warning raised inside the prune or the step-up
#' cannot be traced to the model the search kept (the engine estimates many
#' candidates there), so it qualifies the result instead of refusing it.
#' A solution then also needs every free parameter to have a finite
#' estimate and a finite standard error; the warnings raised while
#' estimating are kept with it.
#'
#' @param model An estimated psychonetrics model, or `NULL`; a model from
#'   `sieve_network_search()` carries its stages' verdicts.
#' @param warnings The warnings captured while estimating.
#' @param error The error message, when estimation stopped.
#' @return A list with `code` (`"ok"`, `"failed"` or `"not_converged"`),
#'   `message`, `converged`, `df`, `se_available`, `warnings` and
#'   `optimizer` (the stages' verdicts, see
#'   `sieve_network_optimizer_table()`), the shape `sieve_has_solution()`
#'   reads.
#' @author Ken Kelley
#' @noRd
sieve_network_status <- function(model, warnings = character(0), error = NULL) {
  out <- list(
    code = "failed", message = error %||% "No estimated model.", converged = FALSE,
    df = NA_real_, se_available = FALSE, warnings = warnings, optimizer = NULL
  )
  if (is.null(model) || !is.null(error)) {
    return(out)
  }
  stages <- attr(model, "sieve_search_optimizer", exact = TRUE)
  if (!is.data.frame(stages) || !nrow(stages)) {
    stages <- sieve_network_optimizer_table(list(model = list(model = model, warnings = warnings)))
  }
  out$optimizer <- stages
  last <- nrow(stages)
  stopped <- which(!is.na(stages$code) & stages$code != 0L)
  if (!length(stopped) && stages$stage[last] %in% c("refit", "model") && isTRUE(stages$gradient_warning[last])) {
    stopped <- last
  }
  if (length(stopped)) {
    i <- stopped[1L]
    where <- switch(stages$stage[i],
      saturated = "the saturated model of the search",
      pruned = "the pruned model of the search",
      stepped_up = "the model the step-up kept",
      refit = "the MLR refit of the model the search kept",
      "the model"
    )
    why <- if (!is.na(stages$code[i]) && stages$code[i] != 0L) {
      sprintf("code %d%s", stages$code[i], if (nzchar(stages$message[i])) paste0(": ", stages$message[i]) else "")
    } else {
      "psychonetrics warned that the gradient at its estimates is large"
    }
    out$code <- "not_converged"
    out$message <- paste0(
      "The optimizer did not converge for ", where, " (", why, "), so ",
      if (i == last) "its estimates are not a solution" else "the edges the search kept rest on an unfinished estimate",
      " and no network is reported. A network of fewer items, or another estimator, may estimate."
    )
    return(out)
  }
  pars <- tryCatch(sieve_network_quiet(psychonetrics::parameters(model)), error = function(e) NULL)
  if (is.null(pars)) {
    out$message <- "The estimated parameters could not be read from the model."
    return(out)
  }
  free <- !as.logical(pars$fixed)
  est_ok <- all(is.finite(as.numeric(pars$est[free])))
  se_ok <- all(is.finite(as.numeric(pars$se[free])))
  out$df <- tryCatch({
    f <- sieve_network_quiet(psychonetrics::fit(model))
    as.numeric(f$Value[f$Measure == "df"])[1L]
  }, error = function(e) NA_real_)
  out$converged <- est_ok
  out$se_available <- se_ok
  if (!est_ok) {
    out$code <- "not_converged"
    out$message <- "The optimizer returned no finite solution for every free parameter."
  } else if (!se_ok) {
    out$code <- "not_converged"
    out$message <- paste(
      "The estimates are finite but some standard errors are not: the information matrix",
      "is singular, so the model may not be identified."
    )
  } else {
    out$code <- "ok"
    out$message <- paste0(
      if (last > 1L) {
        "The optimizer converged for every model of the search, and the network has finite estimates and standard errors."
      } else {
        "The optimizer converged, and the model has finite estimates and standard errors."
      },
      if (any(stages$gradient_warning)) {
        paste(
          " psychonetrics warned that a model estimated during the search might not have converged (a large",
          "gradient at its estimates), so the selection of the edges carries that caution."
        )
      }
    )
  }
  out
}

#' The character of a pairwise edge for the recorded code
#' @param x A character vector of R names.
#' @return One string: `c("a", "b")`.
#' @author Ken Kelley
#' @noRd
sieve_network_c <- function(x) {
  sprintf("c(%s)", paste0("\"", x, "\"", collapse = ", "))
}

#' The recorded R of a network model
#'
#' The actual calls the page makes, written for the Reproducible Code view
#' and the Show code panel: the item columns read from `items` (the
#' recorded input view), the model constructor with its estimator, the
#' search (prune, then step up), and the readers of the result (the
#' parameters with their standard errors, the fit measures, the network
#' matrix, strength and expected influence, the walktrap communities, and
#' the qgraph figure).
#'
#' @param spec The specification the model was estimated under (see
#'   `sieve_network_ggm()`).
#' @param name The object name stem (`"net_ggm"`, `"net_rnm"`, `"net_lnm"`).
#' @return A character vector of R code lines.
#' @author Ken Kelley
#' @noRd
sieve_network_code <- function(spec, name = "net_ggm") {
  kind <- spec$kind
  engine <- spec$engine
  items <- spec$observed
  search_estimator <- engine$search_estimator %||% engine$estimator
  args <- c(
    sprintf("estimator = \"%s\"", search_estimator),
    if (length(engine$ordered)) sprintf("ordered = %s", sieve_network_c(engine$ordered)),
    if (identical(engine$missing, "listwise") && !identical(engine$estimator, "DWLS")) "missing = \"listwise\"",
    if (isTRUE(engine$storedata)) "storedata = TRUE",
    if (isTRUE(engine$standardize)) "standardize = \"z\""
  )
  lines <- c(
    "# The included items, as numbers, on the rows in the analysis (the recorded input view).",
    sprintf("%s_items <- items[, %s, drop = FALSE]", name, sieve_network_c(items)),
    sprintf("%s_items[] <- lapply(%s_items, function(v) suppressWarnings(as.numeric(v)))", name, name)
  )
  if (kind %in% c("network_rnm", "network_lnm")) {
    lambda <- spec$lambda
    lines <- c(
      lines,
      "# The measurement model of the ledger: one factor per construct, each item on its construct only.",
      sprintf("%s_latents <- %s", name, sieve_network_c(colnames(lambda))),
      sprintf(
        "%s_lambda <- matrix(0, nrow = %d, ncol = %d, dimnames = list(%s, %s_latents))",
        name, nrow(lambda), ncol(lambda), sieve_network_c(rownames(lambda)), name
      ),
      unlist(lapply(seq_len(ncol(lambda)), function(j) {
        sprintf("%s_lambda[%s, \"%s\"] <- 1", name, sieve_network_c(rownames(lambda)[lambda[, j] != 0]), colnames(lambda)[j])
      }))
    )
  }
  constructor <- switch(kind,
    network_ggm = sprintf("%s <- psychonetrics::ggm(%s_items, %s)", name, name, paste(args, collapse = ", ")),
    network_rnm = sprintf(
      "%s <- psychonetrics::rnm(%s_items, lambda = %s_lambda, latents = %s_latents, identification = \"variance\", %s)",
      name, name, name, name, paste(args, collapse = ", ")
    ),
    network_lnm = sprintf(
      "%s <- psychonetrics::lnm(%s_items, lambda = %s_lambda, latents = %s_latents, identification = \"variance\", %s)",
      name, name, name, name, paste(args, collapse = ", ")
    )
  )
  what <- switch(kind,
    network_ggm = "the Gaussian graphical model: every partial correlation among the items free",
    network_rnm = "the residual network model: the CFA (factor variances fixed at 1) plus a partial correlation network among the residuals",
    network_lnm = "the latent network model: the CFA with a partial correlation network among the constructs in place of their correlations"
  )
  matrix <- switch(kind, network_ggm = "omega", network_rnm = "omega_epsilon", network_lnm = "omega_zeta")
  lines <- c(
    lines,
    sprintf("# %s.", what),
    if (isTRUE(engine$standardize)) {
      "# standardize = \"z\": the items enter standardized, which leaves the partial correlations, the fit and the construct correlations unchanged and keeps the optimizer at the maximum on any item scale."
    },
    constructor,
    sprintf("%s <- psychonetrics::runmodel(%s)  # the saturated network", name, name),
    sprintf(
      "%s <- psychonetrics::prune(%s, alpha = %s, adjust = \"none\", recursive = FALSE)  # remove the edges not significant at alpha",
      name, name, format(spec$alpha)
    ),
    sprintf(
      "%s <- psychonetrics::stepup(%s, alpha = %s, criterion = \"%s\")  # add edges back by modification index%s",
      name, name, format(spec$alpha), engine$criterion,
      if (identical(engine$criterion, "bic")) " while the BIC improves" else " (information criteria are undefined under DWLS)"
    ),
    if (!identical(search_estimator, engine$estimator)) {
      sprintf(
        "%s <- psychonetrics::runmodel(psychonetrics::setestimator(%s, \"%s\"))  # the sparse model re-estimated once under %s: robust standard errors and fit, same edges",
        name, name, engine$estimator, engine$estimator
      )
    },
    sprintf("%s_pars <- psychonetrics::parameters(%s)  # prints the table and returns it: est, se and p of every parameter; the network is matrix \"%s\"", name, name, matrix),
    sprintf("%s_fit <- psychonetrics::fit(%s)  # chi square, CFI, TLI, RMSEA with psychonetrics' 90%% CI, SRMR, BIC", name, name),
    sieve_network_rmsea_code(name, family = engine$family, conf_level = sieve_default_rmsea_conf_level()),
    sprintf("%s_W <- psychonetrics::getmatrix(%s, \"%s\")  # the sparse partial correlation matrix", name, name, matrix)
  )
  if (identical(kind, "network_lnm")) {
    lines <- c(
      lines,
      sprintf("%s_delta <- psychonetrics::getmatrix(%s, \"delta_zeta\")", name, name),
      sprintf(
        "%s_sigma <- %s_delta %%*%% solve(diag(ncol(%s_W)) - %s_W) %%*%% %s_delta  # the implied construct covariances (delta is fixed at I)",
        name, name, name, name, name
      ),
      sprintf("%s_phi <- stats::cov2cor(%s_sigma)  # the implied construct correlations, the CFA page's Phi when the network is saturated", name, name),
      sprintf("%s_std <- psychonetrics::parameters(%s, standardized = TRUE)  # completely standardized loadings with delta method SEs", name, name)
    )
  } else {
    nodes <- if (identical(kind, "network_ggm")) sprintf("%s_items", name) else sprintf("%s_items[, rownames(%s_lambda)]", name, name)
    lines <- c(
      lines,
      sprintf("dimnames(%s_W) <- list(names(%s), names(%s))", name, nodes, nodes),
      sprintf("%s_strength <- rowSums(abs(%s_W))  # node strength (Opsahl, Agneessens & Skvoretz, 2010)", name, name),
      sprintf("%s_ei <- rowSums(%s_W)  # one-step expected influence (Robinaugh, Millner & McNally, 2016)", name, name),
      sprintf(
        "%s_graph <- igraph::graph_from_adjacency_matrix(abs(%s_W), mode = \"undirected\", weighted = TRUE, diag = FALSE)",
        name, name
      ),
      sprintf("%s_communities <- igraph::membership(igraph::cluster_walktrap(%s_graph, steps = 4))  # Pons & Latapy (2006)", name, name)
    )
  }
  # The figure, runnable as written: the construct of each node colors it
  # (the page takes the colors from the embedded palette; qgraph's defaults
  # serve here), the labels are the node names.
  grouped <- kind %in% c("network_ggm", "network_rnm") && any(!is.na(spec$node_constructs))
  lines <- c(
    lines,
    if (grouped) {
      sprintf(
        "%s_groups <- %s  # the construct of each node, in the order of the columns",
        name, sieve_network_c(ifelse(is.na(spec$node_constructs), "No construct", spec$node_constructs))
      )
    },
    sprintf(
      "qgraph::qgraph(%s_W, layout = \"spring\", theme = \"colorblind\", labels = %s%s)  # the figure: blue edges positive, red negative, width by size",
      name,
      if (identical(kind, "network_lnm")) sprintf("%s_latents", name) else sprintf("colnames(%s_W)", name),
      if (grouped) sprintf(", groups = %s_groups", name) else ""
    )
  )
  lines
}

#' The data a network model reads
#'
#' @param data The item data (the rows in the analysis).
#' @param items The item columns to read.
#' @return A data frame of numeric columns.
#' @author Ken Kelley
#' @noRd
sieve_network_data <- function(data, items) {
  sieve_reliability_data(data, items)
}

#' The display labels of a set of items
#'
#' @param meta The item ledger.
#' @param items Item names.
#' @return A named character vector, item name to label (the item name
#'   where the ledger has no label).
#' @author Ken Kelley
#' @noRd
sieve_network_labels <- function(meta, items) {
  at <- match(items, meta$item)
  label <- if ("label" %in% names(meta)) as.character(meta$label)[at] else rep(NA_character_, length(items))
  label <- ifelse(is.na(label) | !nzchar(label), items, label)
  stats::setNames(label, items)
}

#' A label short enough for a node
#'
#' @param x Labels.
#' @param n The most characters kept.
#' @return The labels, each cut to `n` characters with a trailing mark
#'   where it was cut.
#' @author Ken Kelley
#' @noRd
sieve_network_short <- function(x, n = 14L) {
  x <- as.character(x)
  long <- nchar(x) > n
  x[long] <- paste0(substr(x[long], 1L, n - 1L), "\u2026")
  x
}

#' Estimate one network model and assemble what the page shows
#'
#' The one estimation path of the three models. The constructor is called
#' once, the search runs (`sieve_network_search()`), and the edges, fit,
#' centrality and communities are read from the sparse model. Nothing is
#' rounded. A failure, or a missing psychonetrics, is returned as `error`
#' with a status that has no solution.
#'
#' @param data The item data.
#' @param meta The item ledger.
#' @param kind `"network_ggm"`, `"network_rnm"` or `"network_lnm"`.
#' @param estimator One of `sieve_estimator_choices()`.
#' @param alpha The significance level of the search.
#' @param level The confidence level of the Wald intervals.
#' @param steps The walktrap steps.
#' @return A list with `kind`, `spec` (the specification, with `kind`,
#'   `key`, `engine`, `observed`, `latents`, `name_map`, `lambda`, `alpha`,
#'   `level`, `estimator`, `missing`, `ordered`, `node_constructs` and
#'   `methods`), `model`
#'   (the estimated psychonetrics object), `omega` (the sparse network
#'   matrix with dimnames), `edges`, `fit` (see `sieve_network_fit()`),
#'   `centrality`, `communities` (item and residual networks), `loadings`
#'   (residual and latent networks: raw and completely standardized, each
#'   with its standard error), `phi` (residual network), `implied_cor`
#'   (latent network; see `sieve_network_implied_cor()`), `labels`,
#'   `groups` (the construct of each node), `status`,
#'   `warnings`, `error`, `code` and `n`.
#' @author Ken Kelley
#' @noRd
sieve_network_estimate <- function(data, meta, kind = "network_ggm", estimator = "auto",
                                   alpha = 0.01, level = 0.95, steps = 4L) {
  kind <- match.arg(kind, c("network_ggm", "network_rnm", "network_lnm"))
  out <- list(
    kind = kind, spec = NULL, model = NULL, omega = NULL, edges = NULL, fit = NULL,
    centrality = NULL, communities = NULL, loadings = NULL, phi = NULL, implied_cor = NULL,
    labels = NULL, groups = NULL, status = sieve_network_status(NULL), warnings = character(0),
    error = NULL, code = character(0), n = NA_integer_
  )
  if (!sieve_have("psychonetrics")) {
    out$error <- "Package psychonetrics is not installed."
    out$status <- sieve_network_status(NULL, error = out$error)
    return(out)
  }
  lav <- sieve_lavaan_spec(meta, estimator = estimator)
  active <- meta[meta$status == "active", , drop = FALSE]
  all_items <- intersect(as.character(active$item), names(data))
  if (length(all_items) < 3L) {
    out$error <- "A network needs at least three included items."
    out$status <- sieve_network_status(NULL, error = out$error)
    return(out)
  }
  plan <- if (identical(kind, "network_ggm")) NULL else sieve_network_lambda(meta, all_items)
  if (!is.null(plan) && !is.na(plan$reason)) {
    out$error <- plan$reason
    out$status <- sieve_network_status(NULL, error = out$error)
    return(out)
  }
  if (identical(kind, "network_lnm") && length(plan$latents) < 2L) {
    out$error <- paste(
      "A latent network needs at least two constructs with two or more included items each;",
      "with one construct there is nothing for the constructs' network to connect."
    )
    out$status <- sieve_network_status(NULL, error = out$error)
    return(out)
  }
  items <- if (is.null(plan)) all_items else plan$items
  X <- sieve_network_data(data, items)
  engine <- sieve_network_engine(lav$estimator, lav$missing, intersect(lav$ordered %||% character(0), items), X)
  labels <- sieve_network_labels(meta, items)
  groups <- as.character(active$construct[match(items, active$item)])
  groups[is.na(groups) | !nzchar(groups)] <- NA_character_
  spec <- list(
    kind = kind, engine = engine, observed = items, alpha = alpha, level = level, steps = as.integer(steps),
    estimator = engine$estimator, missing = engine$missing, ordered = engine$ordered,
    lavaan_estimator = engine$lavaan_estimator, choice = lav$choice,
    latents = plan$latents, name_map = plan$name_map, lambda = plan$lambda,
    # Named for what it is: the readers of the analysis registry take a
    # specification's `groups` for the group list of a reliability record.
    node_constructs = groups, not_modeled = plan$not_modeled
  )
  spec$key <- sieve_hash(list(
    kind = kind, items = items, engine = engine[c("estimator", "missing", "ordered")], alpha = alpha,
    lambda = if (!is.null(plan$lambda)) unname(plan$lambda) else NULL, latents = plan$latents
  ))
  spec$methods <- sieve_network_methods(kind, engine)
  out$spec <- spec
  out$labels <- labels
  out$groups <- groups
  out$code <- sieve_network_code(spec, name = switch(kind, network_ggm = "net_ggm", network_rnm = "net_rnm", "net_lnm"))
  # The rows of the input and its complete cases; once the model is
  # estimated, `n` becomes the rows the engine used, which under FIML are
  # every row with an observed item, not the complete cases.
  out$n_input <- nrow(X)
  out$n_complete <- sum(stats::complete.cases(X))
  out$n <- out$n_complete
  # The engines of this estimate, kept with it, so a reopened record cites
  # the versions that made it rather than the ones installed later.
  out$engine_versions <- sieve_network_versions()

  build <- function() {
    common <- list(X, estimator = engine$search_estimator %||% engine$estimator)
    if (length(engine$ordered)) common$ordered <- engine$ordered
    if (identical(engine$missing, "listwise") && !identical(engine$estimator, "DWLS")) common$missing <- "listwise"
    if (isTRUE(engine$storedata)) common$storedata <- TRUE
    if (isTRUE(engine$standardize)) common$standardize <- "z"
    if (identical(kind, "network_ggm")) {
      return(do.call(psychonetrics::ggm, common))
    }
    common$lambda <- plan$lambda
    common$latents <- plan$latents
    common$identification <- "variance"
    do.call(if (identical(kind, "network_rnm")) psychonetrics::rnm else psychonetrics::lnm, common)
  }
  refit <- if (!identical(engine$search_estimator %||% engine$estimator, engine$estimator)) engine$estimator
  run <- sieve_capture({
    model <- build()
    sieve_network_search(model, alpha = alpha, criterion = engine$criterion, refit = refit)
  })
  out$warnings <- unique(as.character(run$warnings))
  # The polychoric warning psychonetrics raises per empty cell is one fact
  # about the cross tables, said once.
  if (any(grepl("Zero frequency cell", out$warnings, fixed = TRUE))) {
    out$warnings <- c(
      "Some cross tables of the ordered items have empty cells, so the polychoric correlations behind the network may be unstable (psychonetrics).",
      out$warnings[!grepl("Zero frequency cell", out$warnings, fixed = TRUE)]
    )
  }
  if (!run$ok) {
    out$error <- sieve_network_error_text(run$error, engine, length(items))
    out$status <- sieve_network_status(NULL, warnings = out$warnings, error = out$error)
    return(out)
  }
  model <- run$value
  out$model <- model
  used <- sieve_network_nobs(model)
  if (!is.na(used)) out$n <- used
  out$n_fitted <- used
  out$status <- sieve_network_status(model, warnings = out$warnings)
  if (!sieve_has_solution(out$status)) {
    return(out)
  }
  matrix <- switch(kind, network_ggm = "omega", network_rnm = "omega_epsilon", network_lnm = "omega_zeta")
  nodes <- if (identical(kind, "network_lnm")) plan$latents else items
  W <- as.matrix(psychonetrics::getmatrix(model, matrix))
  dimnames(W) <- list(nodes, nodes)
  out$omega <- W
  node_labels <- if (identical(kind, "network_lnm")) plan$name_map else labels
  out$edges <- sieve_network_edges(model, matrix, labels = node_labels, level = level)
  # The edges' intervals are at the page's level; the RMSEA interval at the
  # RMSEA level (sieve_default_rmsea_conf_level()).
  out$fit <- sieve_network_fit(model, family = engine$family, conf_level = sieve_default_rmsea_conf_level())
  if (identical(kind, "network_lnm")) {
    delta <- as.matrix(psychonetrics::getmatrix(model, "delta_zeta"))
    out$implied_cor <- sieve_network_implied_cor(W, delta)
    out$centrality <- sieve_network_centrality(W, labels = plan$name_map)
  } else {
    out$centrality <- sieve_network_centrality(W, labels = labels)
    out$communities <- sieve_network_communities(W, constructs = groups, labels = labels, steps = steps)
  }
  if (kind %in% c("network_rnm", "network_lnm")) {
    # The completely standardized loadings beside the raw ones (psychonetrics
    # standardizes the lvm family by the delta method, as lavaan's std.all):
    # in the latent network the construct variances are not 1 (see
    # sieve_network_implied_cor()), so the raw loadings there are not on the
    # CFA page's scale and the standardized ones are what to compare.
    pars <- sieve_network_quiet(psychonetrics::parameters(model, standardized = TRUE))
    lam <- pars[pars$matrix == "lambda" & !as.logical(pars$fixed), , drop = FALSE]
    out$loadings <- data.frame(
      item = as.character(lam$var1), label = unname(labels[as.character(lam$var1)]),
      factor = unname(plan$name_map[as.character(lam$var2)]),
      estimate = as.numeric(lam$est), se = as.numeric(lam$se), p = as.numeric(lam$p),
      std = if ("std" %in% names(lam)) as.numeric(lam$std) else NA_real_,
      se_std = if ("se_std" %in% names(lam)) as.numeric(lam$se_std) else NA_real_,
      stringsAsFactors = FALSE
    )
    if (identical(kind, "network_rnm")) {
      phi <- pars[pars$matrix == "sigma_zeta" & pars$row > pars$col, , drop = FALSE]
      out$phi <- data.frame(
        from = unname(plan$name_map[as.character(phi$var1)]), to = unname(plan$name_map[as.character(phi$var2)]),
        estimate = as.numeric(phi$est), se = as.numeric(phi$se), p = as.numeric(phi$p),
        stringsAsFactors = FALSE
      )
    }
  }
  out
}

#' What to do when psychonetrics cannot estimate a network, in words
#'
#' On a pool of 68 ordered items with seven categories (500 respondents)
#' every network model stopped under DWLS with "system is
#' computationally singular" after about 45 seconds, while the
#' CFA page's WLSMV fit the same items through lavaan: psychonetrics
#' inverts matrices built from its own polychoric correlations, and with
#' many ordered items and empty cells in some of their cross tables one of
#' them is singular. The page passes the engine's words on with what can be
#' done; any other failure is passed on as it is.
#'
#' @param error The engine's message.
#' @param engine The engine specification (`sieve_network_engine()`).
#' @param p The number of items modeled.
#' @return One string.
#' @author Ken Kelley
#' @noRd
sieve_network_error_text <- function(error, engine, p) {
  error <- as.character(error %||% "")
  if (!identical(engine$estimator, "DWLS") || !grepl("singular", error, ignore.case = TRUE)) {
    return(error)
  }
  sprintf(
    paste(
      "psychonetrics could not estimate this network under DWLS from the polychoric correlations of these %d",
      "ordered items: a matrix it inverts is singular (%s). With many ordered items and empty cells in some",
      "of their cross tables this can happen in psychonetrics' polychoric estimates even where the CFA",
      "page's WLSMV, which uses lavaan's, fits the items. To estimate the network, treat the items as",
      "continuous with this page's Estimator control (ML or MLR, on the standardized items); with five or",
      "more categories and roughly symmetric thresholds the continuous treatment performs about as well as",
      "the categorical one (Rhemtulla, Brosseau-Liard & Savalei, 2012). The search re-estimates the model",
      "at every edge it adds, so its time grows quickly with the number of items; a network of fewer items",
      "is much faster to estimate."
    ),
    as.integer(p), sub("[.]$", "", error)
  )
}

# ---- The search in a separate R process, and its wait ----------------------

#' Start a network estimate in a separate R process
#'
#' The search (`sieve_network_search()`) re-estimates the model at every
#' edge it adds, so on a large pool it runs for many minutes: the residual
#' network of 68 seven-point items under ML did not finish within an hour.
#' It runs in a supervised R subprocess, as the EFA fit
#' indices do (`sieve_efa_worker_start()`), so the session stays usable and
#' the estimate can be canceled. Only the item responses, the ledger and the
#' settings cross the process boundary, and the subprocess calls the engine
#' function the page would call in its own process
#' (`sieve_network_estimate()`), from the installed package or, when the
#' session loaded SIEVER from source, from that source.
#'
#' @param data,meta,kind,estimator,alpha As for `sieve_network_estimate()`.
#' @return A handle (see `sieve_network_handle()`). Its result is a list of
#'   the estimate (`value`), the seconds the estimate took in the
#'   subprocess (`engine_seconds`) and the seconds the subprocess took to
#'   load SIEVER and the engines (`load_seconds`). The caller sends the item
#'   columns as plain numbers (`sieve_network_data()`): a haven_labelled
#'   column cannot be read where haven is not loaded.
#' @author Ken Kelley
#' @noRd
sieve_network_worker_start <- function(data, meta, kind, estimator = "auto", alpha = 0.01) {

  paths <- sieve_worker_paths()
  bootstrap <- sieve_worker_bootstrap
  environment(bootstrap) <- baseenv()
  process <- callr::r_bg(
    function(data, meta, kind, estimator, alpha, paths, bootstrap) {
      started <- proc.time()[["elapsed"]]
      bootstrap(paths)
      options(mc.cores = 1L)
      # The engines are loaded before the clock starts, so the time
      # returned is the estimate's, comparable with the reference times
      # (measured in a process that had them loaded).
      for (engine in c("psychonetrics", "qgraph", "igraph")) {
        if (requireNamespace(engine, quietly = TRUE)) loadNamespace(engine)
      }
      estimate <- utils::getFromNamespace("sieve_network_estimate", "SIEVER")
      loaded <- proc.time()[["elapsed"]]
      value <- estimate(data, meta, kind = kind, estimator = estimator, alpha = alpha)
      list(value = value, engine_seconds = proc.time()[["elapsed"]] - loaded, load_seconds = loaded - started)
    },
    args = list(
      data = data, meta = meta, kind = kind, estimator = estimator, alpha = alpha,
      paths = paths, bootstrap = bootstrap
    ),
    libpath = .libPaths(), supervise = TRUE, user_profile = FALSE,
    stdout = NULL, stderr = NULL
  )
  sieve_network_handle(process)
}

#' A handle on one network estimate running in a separate R process
#'
#' @param process A callr background process.
#' @return An environment with `is_alive()`, `get_pid()`, `kill()` and
#'   `get_result()`, the interface `sieve_demand_wire()` reads.
#' @author Ken Kelley
#' @noRd
sieve_network_handle <- function(process) {
  handle <- new.env(parent = emptyenv())
  handle$is_alive <- function() process$is_alive()
  handle$get_pid <- function() process$get_pid()
  handle$kill <- function() invisible(tryCatch(process$kill(), error = function(e) FALSE))
  handle$get_result <- function() process$get_result()
  handle
}

#' A network estimate made in this process, behind the same handle
#'
#' For a session without callr, and for the tests of the page's logic,
#' which count and replace the engine's calls in their own process. The
#' estimate is made before the handle is returned, so the page completes
#' it at once, under its progress message (the attribute `in_process`).
#'
#' @inheritParams sieve_network_worker_start
#' @return A handle whose process is not running and whose result is that
#'   of `sieve_network_worker_start()`, with `load_seconds` 0.
#' @author Ken Kelley
#' @noRd
sieve_network_inline_start <- structure(function(data, meta, kind, estimator = "auto", alpha = 0.01) {
  started <- proc.time()[["elapsed"]]
  value <- sieve_network_estimate(data, meta, kind = kind, estimator = estimator, alpha = alpha)
  result <- list(value = value, engine_seconds = proc.time()[["elapsed"]] - started, load_seconds = 0)
  handle <- new.env(parent = emptyenv())
  handle$is_alive <- function() FALSE
  handle$get_pid <- function() NA_integer_
  handle$kill <- function() invisible(FALSE)
  handle$get_result <- function() result
  handle
}, in_process = TRUE)

#' The wait class of a network engine
#'
#' The search's time depends on the engine's path, not on the lavaan
#' estimator it is reported as: DWLS on polychoric correlations; MLR, the
#' search under ML on complete data and one robust re-estimation; FIML on
#' incomplete data; ML, and FIML on complete data, which takes about the
#' ML engine's time (see `sieve_network_engine()`), so the default ML with
#' FIML on complete data is not stated at the incomplete data rate.
#'
#' @param engine A specification from `sieve_network_engine()`.
#' @return `"DWLS"`, `"MLR"`, `"FIML"` or `"ML"`.
#' @author Ken Kelley
#' @noRd
sieve_network_wait_class <- function(engine) {
  est <- toupper(as.character(engine$estimator %||% "ML"))[1L]
  if (identical(est, "FIML") && isTRUE(engine$complete)) {
    return("ML")
  }
  if (est %in% c("DWLS", "MLR", "FIML")) est else "ML"
}

#' The measured times of the network search
#'
#' Seconds for one estimate (`sieve_network_estimate()`: the model, the
#' search and, under MLR, the robust re-estimation, with the tables read
#' from it), measured in one R process, on the bundled Holzinger and
#' Swineford tests (9 and 24, n = 301) and on a pool of 68 seven-point
#' items cut to 12, 24, 36 and 48 items in facet order (n = 500, its six
#' facets as the constructs, the items treated as continuous for
#' ML, MLR and FIML, as ordered for DWLS; FIML with 5% of the responses
#' missing completely at random). Where two pools give a size, the longer
#' time is kept, so the estimate errs long.
#'
#' @return A data frame with `kind`, `class` (see
#'   `sieve_network_wait_class()`), `p` and `seconds`.
#' @author Ken Kelley
#' @noRd
sieve_network_timings <- function() {
  one <- function(kind, class, p, seconds) {
    data.frame(kind = kind, class = class, p = as.integer(p), seconds = seconds, stringsAsFactors = FALSE)
  }
  rbind(
    one("network_ggm", "DWLS", c(12, 24), c(1.599, 114.200)),
    one("network_ggm", "FIML", c(12, 24), c(3.736, 59.773)),
    one("network_ggm", "ML", c(9, 12, 24, 36, 48), c(0.241, 0.942, 10.299, 72.497, 629.956)),
    one("network_ggm", "MLR", c(9, 12, 24, 36), c(0.932, 2.253, 18.020, 125.952)),
    one("network_lnm", "DWLS", c(12, 24), c(1.540, 7.161)),
    one("network_lnm", "FIML", c(12, 24), c(1.115, 7.923)),
    one("network_lnm", "ML", c(9, 12, 24, 36, 48), c(0.473, 0.579, 2.582, 8.426, 20.727)),
    one("network_lnm", "MLR", c(9, 12, 24, 36), c(1.810, 2.457, 9.003, 28.872)),
    one("network_rnm", "DWLS", c(12, 24), c(3.087, 5.924)),
    one("network_rnm", "FIML", c(12, 24), c(4.497, 63.451)),
    one("network_rnm", "ML", c(9, 12, 24, 36, 48), c(0.874, 1.710, 14.569, 137.154, 910.342)),
    one("network_rnm", "MLR", c(9, 12, 24, 36), c(2.159, 4.441, 32.355, 220.057))
  )
}

#' The reference seconds of a network search on `p` items
#'
#' Read from the measured times (`sieve_network_timings()`) by linear
#' interpolation of the logarithm of the seconds on the logarithm of the
#' item pairs. Beyond the largest pool measured, the growth of the last
#' measured interval continues: the search re-estimates the model at every
#' edge it adds, and its growth steepened with the pool (with the item
#' pairs to about the power 1.5 from 12 to 24 items, 2.7 from 24 to 36, and
#' 3.25 for the residual network and 3.7 for the item network from 36 to
#' 48), so the last interval is the best guide to larger pools. Below the
#' smallest, the time of the smallest is the floor. A class measured on
#' smaller pools than ML (MLR, FIML and DWLS, measured to 36 or 24 items)
#' follows, beyond its largest, the growth of ML at its own ratio to ML
#' there: MLR is the ML search and one robust re-estimation, so its own
#' shallower interval would put it below ML on a large pool. A class not
#' measured reads the times of ML.
#'
#' @param kind `"network_ggm"`, `"network_rnm"` or `"network_lnm"`.
#' @param class See `sieve_network_wait_class()`.
#' @param p The number of items modeled.
#' @return Seconds at the measured machine's speed.
#' @author Ken Kelley
#' @noRd
sieve_network_reference_seconds <- function(kind, class, p) {
  if (!length(p) || !is.finite(p[[1L]]) || p[[1L]] < 2) {
    return(NA_real_)
  }
  p <- p[[1L]]
  tab <- sieve_network_timings()
  ml <- tab[tab$kind == kind & tab$class == "ML", , drop = FALSE]
  own <- tab[tab$kind == kind & tab$class == class, , drop = FALSE]
  if (nrow(ml) < 2L) {
    return(NA_real_)
  }
  along <- function(rows, at_p) {
    rows <- rows[order(rows$p), , drop = FALSE]
    x <- log(sieve_pairs(rows$p))
    y <- log(rows$seconds)
    at <- log(sieve_pairs(at_p))
    n <- length(x)
    log_seconds <- if (at <= x[[1L]]) {
      y[[1L]]
    } else if (at >= x[[n]]) {
      y[[n]] + (y[[n]] - y[[n - 1L]]) / (x[[n]] - x[[n - 1L]]) * (at - x[[n]])
    } else {
      stats::approx(x, y, xout = at)$y
    }
    exp(log_seconds)
  }
  if (identical(class, "ML") || nrow(own) < 2L) {
    return(along(ml, p))
  }
  top <- max(own$p)
  if (p <= top || top >= max(ml$p)) {
    return(along(own, p))
  }
  along(own, top) * along(ml, p) / along(ml, top)
}

#' Seconds to start the separate R process of a network estimate
#'
#' The time this process measured for the start of one (the elapsed time
#' of an estimate less the time its engine took), or, before any, the
#' reference time: 1.2 seconds with SIEVER installed and 1.8 seconds from
#' the source under pkgload (means of three).
#'
#' @return Seconds.
#' @author Ken Kelley
#' @noRd
sieve_network_worker_seconds <- function() {
  measured <- sieve_rate("network_worker")
  if (is.finite(measured)) {
    return(measured)
  }
  path <- tryCatch(getNamespaceInfo(asNamespace("SIEVER"), "path"), error = function(e) "")
  installed <- file.exists(file.path(path, "Meta", "package.rds"))
  if (installed) 1.2 else 1.8
}

#' Estimated seconds for a network estimate
#'
#' The reference seconds of the search on these items
#' (`sieve_network_reference_seconds()`), times this machine's speed
#' relative to the machine they were measured on (the ratio of the seconds
#' measured in this process to the reference seconds of the same estimates,
#' `sieve_rate()` of `"<kind>_<class>"`, 1 before any), plus the start of
#' the separate R process when the estimate runs in one. The search is not
#' timed as the CFA estimate scaled by a fixed multiplier: that grows with
#' the item pairs while the search grows far faster, so on 68 items it
#' would state about 65 seconds for a search that runs longer than an hour.
#'
#' @param kind `"network_ggm"`, `"network_rnm"` or `"network_lnm"`.
#' @param p The number of items modeled.
#' @param engine A specification from `sieve_network_engine()`.
#' @param worker Whether the estimate runs in a separate R process.
#' @return Seconds, or `NA` when no estimate can be made.
#' @author Ken Kelley
#' @noRd
sieve_network_wait <- function(kind, p, engine, worker = TRUE) {
  class <- sieve_network_wait_class(engine)
  reference <- sieve_network_reference_seconds(kind, class, p)
  if (!is.finite(reference)) {
    return(NA_real_)
  }
  speed <- sieve_rate(sprintf("%s_%s", kind, class))
  if (!is.finite(speed) || speed <= 0) speed <- 1
  reference * speed + if (isTRUE(worker)) sieve_network_worker_seconds() else 0
}

#' Record the time a network estimate took
#'
#' The engine's seconds against the reference seconds of the same estimate
#' (so the rate in force is this machine's speed relative to the measured
#' one, see `sieve_network_wait()`), and the start of its separate R
#' process (the elapsed time less the engine's), when it ran in one.
#'
#' @param kind,p,engine As for `sieve_network_wait()`.
#' @param engine_seconds The seconds the estimate took.
#' @param start_seconds The seconds the separate R process took to start
#'   and return, or `NA`.
#' @return The speed in force for the kind and class, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_network_wait_note <- function(kind, p, engine, engine_seconds, start_seconds = NA_real_) {
  class <- sieve_network_wait_class(engine)
  reference <- sieve_network_reference_seconds(kind, class, p)
  if (length(start_seconds) == 1L && is.finite(start_seconds) && start_seconds >= 0) {
    sieve_rate_note("network_worker", start_seconds, 1)
  }
  invisible(sieve_rate_note(sprintf("%s_%s", kind, class), engine_seconds, reference))
}

#' The rows a psychonetrics model was estimated from
#'
#' psychonetrics keeps the number of observations of each group on the
#' model's sample (`model@sample@groups$nobs`): the complete cases under ML
#' and DWLS, and under FIML every row with at least one observed item. The
#' `nobs` of its fit table is the number of sample statistics, not of
#' respondents, so it is not read.
#'
#' @param model A psychonetrics model after estimation.
#' @return An integer, or `NA` when the slot cannot be read.
#' @author Ken Kelley
#' @noRd
sieve_network_nobs <- function(model) {
  n <- tryCatch(model@sample@groups$nobs, error = function(e) NULL)
  n <- suppressWarnings(as.numeric(n))
  if (!length(n) || any(!is.finite(n))) NA_integer_ else as.integer(sum(n))
}

#' The versions of the network engines, as a record keeps them
#'
#' psychonetrics estimates the models, qgraph draws them and igraph finds
#' the walktrap communities; the versions are read when a model is
#' estimated and travel with its record and its code disclosure.
#'
#' @return A named list of version strings (`NA` for a package that is not
#'   installed).
#' @author Ken Kelley
#' @noRd
sieve_network_versions <- function() {
  pkgs <- sieve_network_packages()
  stats::setNames(lapply(pkgs, function(p) {
    if (sieve_have(p)) as.character(utils::packageVersion(p)) else NA_character_
  }), pkgs)
}

#' The packages a network model runs in
#'
#' @return A character vector.
#' @author Ken Kelley
#' @noRd
sieve_network_packages <- function() c("psychonetrics", "qgraph", "igraph")

#' The items a residual or latent network leaves out, in words
#'
#' The network's `not_modeled` record (`item`, `construct`, `reason`) in the
#' sentences the CFA page uses for its own (`sieve_not_modeled_text()`),
#' one per construct and reason, naming the items.
#'
#' @param nm The `not_modeled` data frame of `sieve_network_lambda()`.
#' @return A character vector, possibly empty.
#' @author Ken Kelley
#' @noRd
sieve_network_not_modeled_text <- function(nm) {
  if (is.null(nm) || !nrow(nm)) {
    return(character(0))
  }
  key <- paste(nm$construct, nm$reason, sep = "\r")
  parts <- split(seq_len(nrow(nm)), factor(key, levels = unique(key)))
  shaped <- do.call(rbind, lapply(parts, function(i) {
    data.frame(
      level = "item", construct = nm$construct[[i[[1L]]]], facet = NA_character_,
      n_items = length(i), reason = nm$reason[[i[[1L]]]], items = paste(nm$item[i], collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
  sieve_not_modeled_text(list(not_modeled = shaped))
}

#' The registry keys of the methods a network model used
#'
#' @param kind The model kind.
#' @param engine The engine specification (see `sieve_network_engine()`).
#' @return A character vector of keys of `sieve_method_references()`.
#' @author Ken Kelley
#' @noRd
sieve_network_methods <- function(kind, engine) {
  c(
    "network_ggm", "network_partial_correlation", "network_model_search", "network_accuracy",
    switch(kind,
      network_ggm = c("network_strength", "network_expected_influence", "network_communities", "network_ega"),
      network_rnm = c("cfa", "network_strength", "network_expected_influence", "network_communities", "network_ega"),
      network_lnm = c("cfa", "network_factor_equivalence")
    ),
    if (identical(engine$estimator, "DWLS")) "network_ising_irt",
    "psychonetrics", "qgraph", "igraph"
  )
}
#' Draw a network with qgraph in the embedded palette
#'
#' Nodes are colored by construct from SIEVER's embedded palette
#' (`sieve_palette_colors()`; no palette package is called), edges are blue
#' for a positive partial correlation and red for a negative one, with
#' width proportional to size (qgraph's colorblind theme; Epskamp, Cramer,
#' Waldorp, Schmittmann & Borsboom, 2012), and the layout is qgraph's
#' spring layout (Fruchterman and Reingold). A figure is a picture of the
#' estimated matrix: distance on the page carries no quantity, and the
#' width of an edge shows its point estimate, not its uncertainty.
#'
#' @param omega A symmetric network matrix with dimnames.
#' @param labels The node labels (as long as the matrix), or `NULL` for the
#'   dimnames.
#' @param groups The construct of each node, or `NULL`.
#' @param palette The embedded palette name.
#' @param title A title, or `NULL`.
#' @param ... Further arguments to [qgraph::qgraph()].
#' @return The qgraph object, invisibly; the figure is drawn on the current
#'   device.
#' @author Ken Kelley
#' @noRd
sieve_network_plot <- function(omega, labels = NULL, groups = NULL, palette = "nd", title = NULL, ...) {
  W <- as.matrix(omega)
  nodes <- rownames(W) %||% paste0("V", seq_len(nrow(W)))
  labels <- labels %||% nodes
  grouped <- !is.null(groups) && any(!is.na(groups))
  args <- list(
    W, layout = "spring", theme = "colorblind", labels = sieve_network_short(labels),
    label.cex = 1.1, label.scale = TRUE, vsize = max(5, min(11, 45 / sqrt(nrow(W)))),
    borders = TRUE, title = title, mar = c(3, 3, if (is.null(title)) 3 else 5, 3), ...
  )
  if (grouped) {
    g <- ifelse(is.na(groups), "No construct", as.character(groups))
    levels <- unique(g)
    cols <- sieve_palette_colors(palette, length(levels))
    args$groups <- factor(g, levels = levels)
    args$color <- cols
    args$legend <- length(levels) > 1L
    args$legend.cex <- 0.5
    args$GLratio <- 3
  } else {
    args$color <- sieve_palette_colors(palette, 1L)
    args$label.color <- "white"
  }
  invisible(do.call(qgraph::qgraph, args))
}

#' Whether Ordered Items Have an Empty Bivariate Cell
#' @param data The selected ordered items.
#' @return A logical scalar.
#' @author Ken Kelley
#' @noRd
sieve_network_empty_cells <- function(data) {
  if (ncol(data) < 2L) return(FALSE)
  data[] <- lapply(data, sieve_as_number)
  pairs <- utils::combn(seq_len(ncol(data)), 2L)
  any(vapply(seq_len(ncol(pairs)), function(j) {
    x <- data[[pairs[1L, j]]]
    y <- data[[pairs[2L, j]]]
    any(table(factor(x), factor(y)) == 0L)
  }, logical(1)))
}

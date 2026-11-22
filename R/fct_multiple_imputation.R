#' Plan the Number of Imputations From a Pilot Analysis
#'
#' The logit interval for FMI uses Harel's approximation reported by
#' von Hippel (2020), logit(FMI) +/- z sqrt(2/M). The upper limit enters
#' M = 1 + (1/2)(FMI/CV)^2. The largest requirement among the parameters
#' sets the final count, with the pilot count as a floor.
#' @param fmi Parameter-specific pilot fractions of missing information.
#' @param pilot Number of pilot imputations, at least 20 by default.
#' @param cv Target coefficient of variation of the standard errors.
#' @param conf_level Confidence level for the pilot FMI bounds.
#' @return Full-precision pilot intervals and the final integer count.
#' @references von Hippel, P. T. (2020). How many imputations do you need?
#' A two-stage calculation using a quadratic rule. Sociological Methods &
#' Research, 49, 699--718. doi:10.1177/0049124117747303.
#' @author Ken Kelley
#' @examples
#' plan <- sieve_mi_plan(c(.10, .35))
#' plan$imputations
#' plan$table
#' @export
sieve_mi_plan <- function(fmi, pilot = 20L, cv = .05, conf_level = .95) {
  if (length(pilot) != 1L || !is.finite(pilot) || pilot < 2 || pilot != floor(pilot)) {
    stop("Use a whole number of at least two pilot imputations.", call. = FALSE)
  }
  conf_level <- sieve_check_conf_level(conf_level)
  if (length(cv) != 1L || !is.finite(cv) || cv <= 0) {
    stop("Use a positive CV.", call. = FALSE)
  }
  fmi <- as.numeric(fmi)
  if (!length(fmi) || any(!is.finite(fmi)) || any(fmi < 0 | fmi > 1)) {
    stop("The pilot must supply finite FMI estimates between zero and one.", call. = FALSE)
  }
  half <- stats::qnorm(1 - (1 - conf_level) / 2) * sqrt(2 / pilot)
  lower <- stats::plogis(stats::qlogis(fmi) - half)
  upper <- stats::plogis(stats::qlogis(fmi) + half)
  required <- ceiling(1 + .5 * (upper / cv)^2)
  list(pilot = as.integer(pilot), cv = cv, conf_level = conf_level,
    table = data.frame(fmi = fmi, lower_limit = lower, upper_limit = upper,
      imputations = as.integer(pmax(pilot, required))),
    imputations = as.integer(max(pilot, required)))
}

#' Multiple Imputation Sensitivity Analysis for an Ordered Factor Model
#'
#' The same CFA or SEM is fitted to every completed data set by lavaan.mi,
#' with WLSMV for the ordered indicators. mice uses logistic regression for
#' binary items, proportional odds for ordered items, and predictive mean
#' matching for continuous variables. A pilot of 20 imputations determines
#' the final count using the upper 95 percent FMI bounds and CV = .05.
#' Incomplete rows are retained; this is a sensitivity analysis under MAR
#' and the specified imputation model, rather than the default continuous
#' FIML path. Inspect imputation diagnostics and substantive compatibility.
#' @param model CFA or SEM syntax.
#' @param data Data before complete-case filtering.
#' @param ordered Ordered indicator column names.
#' @param aux Optional imputation predictors, excluded from the fitted model.
#' @param fun Model function, CFA or SEM.
#' @param conf_level Confidence level for pooled parameter intervals.
#' @param seed Optional random seed, default NULL.
#' @param pilot Number of pilot imputations, default 20.
#' @param cv Target CV of standard errors, default .05.
#' @param std_lv Whether latent variances identify the factors.
#' @param engine_args Optional fixed.x and conditional.x model arguments.
#' @param pooled_variance How each parameter's pooled sampling variance is
#'   formed. The one convention offered is `"rubin_parameterwise"`, Rubin's
#'   (1987) rules applied parameter by parameter (see Details). It is an
#'   argument so that the recorded call names the convention it used.
#' @return A list. No values are rounded.
#'   * `fit`: the lavaan.mi object of the final imputations. Its own
#'     `summary()`, `coef()`, `vcov()`, `nobs()` and `anova()` methods
#'     dispatch through lavaan's generics, which are found only while lavaan
#'     is attached, and the function detaches lavaan and lavaan.mi again if
#'     it attached them (see the section below), so call
#'     `library(lavaan.mi)`, which attaches both, to use them.
#'   * `estimates`: the pooled estimates, one row per parameter, with
#'     standard errors, t reference degrees of freedom, FMI and intervals.
#'   * `plan`: the count plan of [sieve_mi_plan()], its table carrying the
#'     parameters whose pilot FMI set the count.
#'   * `pilot_estimates`: the pilot's pooled estimates.
#'   * `imputations`: the final number of imputations, M.
#'   * `used`, `omitted`: the fits pooled, and those omitted for
#'     nonconvergence or unavailable standard errors.
#'   * `improper`, `improper_imputations`: the number, and the positions,
#'     of pooled fits with an improper solution.
#'   * `imputation_events`, `pilot_events`: mice's logged events.
#'   * `rng`: the random-number provenance (seed, whether it was supplied,
#'     and the [RNGkind()] settings).
#'   * `pooled_variance`, `method`, `conf_level`: the pooling convention,
#'     the method in words, and the confidence level.
#'   * `engine_versions`: the mice and lavaan.mi versions used.
#' @details Requires optional packages mice and lavaan.mi. Nonconverged
#'   imputations and those without standard errors are omitted from pooling
#'   according to lavaan.mi's explicit default; their counts are returned.
#'
#'   An infinite value (`Inf` or `-Inf`) in a modeled or auxiliary column is
#'   refused before any imputation, with the column and the rows named, as
#'   on every route that fits a model: it is the trace of a division by zero
#'   or an overflow, not a response, and imputing it away or reading it as
#'   the highest category would change what a respondent answered. `NaN` is
#'   read as a missing value and imputed, as [is.na()] and mice treat it.
#'
#'   Every free parameter is pooled on its own by Rubin's (1987) rules. With
#'   M the number of imputations pooled, Q-bar the mean of the M
#'   completed-data estimates, W the mean of their squared standard errors
#'   (the within-imputation variance) and B the variance of the M estimates
#'   (the between-imputation variance), the pooled variance is
#'   T = W + (1 + 1/M)B and the standard error is sqrt(T). The interval is
#'   Q-bar +/- t sqrt(T), with t the quantile of a t distribution on
#'   df = (M - 1)(1 + W/((1 + 1/M)B))^2 degrees of freedom, and the fraction
#'   of missing information is FMI = (1 + 1/M)B/T. The pilot's FMI values,
#'   which set M, are computed the same way. These are the values
#'   `lavaan.mi::parameterEstimates.mi()` returns with `asymptotic = FALSE`
#'   and `scale.W = FALSE`. Its default, `scale.W = TRUE`, instead scales W
#'   by the average relative increase in variance over all free parameters,
#'   which assumes the same fraction of missing information for every
#'   parameter and belongs to multiparameter Wald tests (Li, Raghunathan, &
#'   Rubin, 1991); for a parameter with more missing information than the
#'   average it gives a standard error, and so an interval, that is too
#'   small, so it is not offered here.
#' @section What the Function Loads and Attaches:
#'   lavaan.mi's constructors check the search path and warn when
#'   lavaan.mi is not attached, and lavaan.mi registers its `summary()`, `coef()`, `vcov()`, `nobs()` and `anova()`
#'   methods where they dispatch only when lavaan is attached at the moment
#'   lavaan.mi's namespace loads; loaded otherwise, those methods do not
#'   dispatch for the rest of the R session, even after
#'   `library(lavaan.mi)`. So the function checks that mice and lavaan.mi
#'   are installed without loading them, and, once the request has passed
#'   its checks, attaches lavaan (when it is not attached) and then
#'   lavaan.mi (when it is not attached, which loads its namespace) for the
#'   duration of the call, and detaches whichever of the two it attached
#'   before it returns, whether it succeeds or fails. The search path is
#'   left as it was found; the namespaces of lavaan.mi and mice stay
#'   loaded, as after any use of a package. If lavaan.mi's namespace was
#'   loaded earlier in the session while lavaan was not attached, the
#'   function warns that the returned fit's own methods will not dispatch;
#'   the pooled estimates, intervals and count plan it returns are
#'   unaffected. SIEVER itself checks for lavaan.mi without loading it, and
#'   its app runs this analysis in a separate R process, so neither loads
#'   lavaan.mi in the user's session.
#' @references Rubin, D. B. (1987). Multiple imputation for nonresponse in
#' surveys. Wiley. doi:10.1002/9780470316696.
#'
#' Li, K. H., Raghunathan, T. E., & Rubin, D. B. (1991). Large-sample
#' significance levels from multiply imputed data using moment-based
#' statistics and an F reference distribution. Journal of the American
#' Statistical Association, 86(416), 1065--1073.
#' @author Ken Kelley
#' @examples
#' raw <- lavaan::HolzingerSwineford1939[, paste0("x", 4:6)]
#' d <- as.data.frame(lapply(raw, function(x) as.integer(cut(x, 5))))
#' d[1:10, 1] <- NA_integer_
#' colSums(is.na(d))
#' # The pilot is at least twenty imputations, each with a WLSMV fit, which
#' # takes a few seconds, so the analysis runs in an interactive session only.
#' if (interactive()) {
#'   analysis <- sieve_mi_model("Textual =~ x4 + x5 + x6", d,
#'     ordered = names(d), seed = 1)
#'   analysis$imputations
#'   analysis$estimates
#' }
#' @export
sieve_mi_model <- function(model, data, ordered, aux = NULL, fun = c("cfa", "sem"),
                             conf_level = .95, seed = NULL, pilot = 20L, cv = .05, std_lv = TRUE,
                             engine_args = list(), pooled_variance = "rubin_parameterwise") {
  fun <- match.arg(fun)
  conf_level <- sieve_check_conf_level(conf_level)
  sieve_mi_run(model, data, ordered, aux = aux, fun = fun, conf_level = conf_level,
    seed = seed, pilot = pilot, cv = cv, std_lv = std_lv, engine_args = engine_args,
    pooled_variance = pooled_variance)
}

#' Is a package installed, without loading it?
#'
#' [system.file()] finds an installed package in the library paths (or the
#' path of a namespace already loaded) and loads nothing. The launch notice
#' of missing optional packages and the MI request's own check use it, so
#' that asking whether a package is there never loads it: loading is the
#' job of the code that uses it. lavaan.mi is the case that matters (see
#' `sieve_mi_model()`): loaded while lavaan is not attached, its methods
#' never dispatch for the rest of the R session.
#' @param pkg A package name.
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_installed <- function(pkg) {
  nzchar(system.file(package = pkg))
}

#' Do lavaan.mi's methods dispatch in this R session?
#'
#' `methods::existsMethod()` without `where` reads the dispatch table of
#' the generic itself, so it tells whether `coef()` and `vcov()` called on a
#' lavaan.mi fit reach lavaan.mi's methods. They do not when lavaan.mi's
#' namespace was loaded while lavaan was not attached (checked in fresh R
#' processes in the tests; the result does not depend on where the check
#' is called from). Call it with both packages attached.
#' @return `TRUE` or `FALSE`.
#' @author Ken Kelley
#' @noRd
sieve_lavaan_mi_dispatches <- function() {
  all(vapply(c("coef", "vcov"), function(f) isTRUE(methods::existsMethod(f, "lavaan.mi")), logical(1)))
}

#' Load lavaan.mi's namespace so that its methods dispatch
#'
#' Loads the namespace, when it is not loaded, with lavaan attached for the
#' moment it loads (lavaan.mi registers its methods where they dispatch
#' only then; see `sieve_mi_model()`), and detaches lavaan again if it was
#' not attached before, so the search path is left as it was found.
#' @return `TRUE` when lavaan.mi's namespace is loaded, invisibly.
#' @author Ken Kelley
#' @noRd
sieve_load_lavaan_mi <- function() {
  if (isNamespaceLoaded("lavaan.mi")) return(invisible(TRUE))
  if (!sieve_installed("lavaan.mi")) return(invisible(FALSE))
  if (!"package:lavaan" %in% search()) {
    suppressPackageStartupMessages(attachNamespace("lavaan"))
    on.exit(detach("package:lavaan", character.only = TRUE), add = TRUE)
  }
  ok <- requireNamespace("lavaan.mi", quietly = TRUE)
  invisible(ok)
}

#' Check an ordered-item MI request and prepare its imputation data
#'
#' Every refusal `sieve_mi_model()` makes before any imputation, in its
#' order. It loads neither mice nor lavaan.mi (their installation is
#' checked with `sieve_installed()`), so the MI panel calls it before it
#' starts the background process, and a request that cannot run is refused
#' at once.
#'
#' The columns are read with `sieve_as_number()`, so a factor auxiliary
#' (a school label, say) enters by its codes. An infinite value in a
#' modeled or auxiliary column is refused with its column and rows named,
#' in the words of `sieve_numeric_columns()` (an auxiliary column is named
#' as one), as `sieve_reliability_data()` refuses it, rather than becoming
#' the highest category of an ordered indicator. `NaN` becomes `NA`.
#' @inheritParams sieve_mi_model
#' @return A list with `X` (the imputation data, the ordered indicators as
#'   ordered factors) and `observed` (the model's observed variables).
#' @author Ken Kelley
#' @noRd
sieve_mi_check <- function(model, data, ordered, aux = NULL, conf_level = .95, pilot = 20L,
                           cv = .05, engine_args = list(), pooled_variance = "rubin_parameterwise") {
  conf_level <- sieve_check_conf_level(conf_level)
  if (!identical(pooled_variance, "rubin_parameterwise")) {
    stop(paste0("pooled_variance must be \"rubin_parameterwise\": each parameter's pooled variance is ",
      "T = W + (1 + 1/M)B by Rubin's (1987) rules. The alternative in lavaan.mi, the within-imputation ",
      "variance scaled by the average relative increase in variance, assumes the same fraction of ",
      "missing information for every parameter and belongs to multiparameter tests (Li, Raghunathan, & ",
      "Rubin, 1991), so it is not offered for per-parameter intervals."), call. = FALSE)
  }
  sieve_mi_plan(.5, pilot = pilot, cv = cv, conf_level = conf_level)
  if (pilot < 20L) stop("Use at least 20 pilot imputations.", call. = FALSE)
  if (length(setdiff(names(engine_args), c("fixed.x", "conditional.x")))) {
    stop("MI model extensions support fixed.x and conditional.x only.", call. = FALSE)
  }
  for (package in c("mice", "lavaan.mi")) {
    if (!sieve_installed(package)) stop("Install ", package, " to request the ordered-item MI sensitivity analysis.", call. = FALSE)
  }
  if (!length(ordered) || any(!ordered %in% names(data))) {
    stop("Choose ordered indicators for the WLSMV sensitivity model.", call. = FALSE)
  }
  observed <- unique(lavaan::lavNames(lavaan::lavaanify(model), "ov"))
  columns <- unique(c(observed, aux))
  absent <- setdiff(columns, names(data))
  if (length(absent)) stop("The imputation data are missing: ", paste(absent, collapse = ", "), call. = FALSE)
  if (any(!ordered %in% observed)) stop("Ordered columns must be modeled indicators.", call. = FALSE)
  X <- data[, columns, drop = FALSE]
  X[] <- lapply(X, sieve_as_number)
  infinite <- vapply(X, function(v) any(is.infinite(v)), logical(1))
  if (any(infinite)) {
    read <- sieve_numeric_columns(X, names(X)[infinite],
      what = ifelse(names(X)[infinite] %in% observed, "Item", "Auxiliary column"))
    at <- read$problems$kind == "not_finite"
    stop(paste(read$text[at], collapse = " "), call. = FALSE)
  }
  X[] <- lapply(X, function(v) {
    v[is.nan(v)] <- NA_real_
    v
  })
  bad <- names(X)[vapply(X, function(x) length(unique(x[!is.na(x)])) < 2L, logical(1))]
  if (length(bad)) stop("Imputation needs variation and observed responses in: ", paste(bad, collapse = ", "), call. = FALSE)
  if (!anyNA(X[, observed, drop = FALSE])) stop("No modeled responses are missing; an MI sensitivity analysis is unnecessary.", call. = FALSE)
  for (name in ordered) X[[name]] <- base::ordered(X[[name]], levels = sort(unique(X[[name]][!is.na(X[[name]])])))
  list(X = X, observed = observed)
}

#' Run the ordered-item MI sensitivity analysis
#'
#' The computation of `sieve_mi_model()`, which calls it with `progress`
#' and `rng_kind` left `NULL`; the MI panel's background process
#' (`sieve_mi_worker()`) calls it with both. Neither changes a value: the
#' progress hook only counts, and the random-number settings are those of
#' the session that drew the seed.
#' @inheritParams sieve_mi_model
#' @param progress `NULL`, or a function `progress(stage, done, total)`,
#'   called as each stage starts (`"pilot_imputation"`,
#'   `"final_imputation"`, `"pooling"`) and after each completed-data fit
#'   that converges (`"pilot"`, `"final"`; lavaan.mi's `fun` hook, which
#'   lavaan calls only for a converged fit).
#' @param rng_kind `NULL`, or the three [RNGkind()] settings to run under,
#'   restored afterwards.
#' @return As `sieve_mi_model()`.
#' @author Ken Kelley
#' @noRd
sieve_mi_run <- function(model, data, ordered, aux = NULL, fun = "cfa", conf_level = .95,
                         seed = NULL, pilot = 20L, cv = .05, std_lv = TRUE,
                         engine_args = list(), pooled_variance = "rubin_parameterwise",
                         progress = NULL, rng_kind = NULL) {
  prepared <- sieve_mi_check(model, data, ordered, aux = aux, conf_level = conf_level,
    pilot = pilot, cv = cv, engine_args = engine_args, pooled_variance = pooled_variance)
  X <- prepared$X
  if (length(rng_kind) == 3L && !identical(RNGkind(), as.character(rng_kind))) {
    old <- RNGkind()
    on.exit(suppressWarnings(RNGkind(old[1L], old[2L], old[3L])), add = TRUE)
    suppressWarnings(RNGkind(rng_kind[1L], rng_kind[2L], rng_kind[3L]))
  }
  # lavaan.mi's constructors check the search path and warn when lavaan.mi
  # is not attached, and its methods dispatch only when lavaan is attached
  # as its namespace loads (see Details of sieve_mi_model()). So lavaan,
  # then lavaan.mi, are attached for this call when they are not attached,
  # and whichever of them was attached here is detached on the way out.
  attached <- character(0)
  on.exit(for (entry in rev(attached)) detach(entry, character.only = TRUE), add = TRUE)
  for (package in c("lavaan", "lavaan.mi")) {
    entry <- paste0("package:", package)
    if (!entry %in% search()) {
      suppressPackageStartupMessages(attachNamespace(package))
      attached <- c(attached, entry)
    }
  }
  if (!sieve_lavaan_mi_dispatches()) {
    warning(paste0("lavaan.mi was loaded in this R session before lavaan was attached, so lavaan.mi's ",
      "summary(), coef(), vcov(), nobs() and anova() methods do not dispatch for the returned fit: ",
      "coef() returns one column per imputation and summary() gives lavaanList averages without pooled ",
      "standard errors. The pooled estimates, intervals and count plan returned by sieve_mi_model() are ",
      "unaffected. To use the fit's own methods, restart R and call library(lavaan.mi) before loading ",
      "anything that uses it."), call. = FALSE)
  }
  note <- if (is.function(progress)) progress else function(stage, done, total) invisible(NULL)
  fit_sets <- function(completed, stage) {
    constructor <- if (identical(fun, "sem")) lavaan.mi::sem.mi else lavaan.mi::cfa.mi
    args <- list(model = model, data = completed, ordered = ordered,
      estimator = "WLSMV", std.lv = std_lv)
    if (is.function(progress)) {
      # lavaan.mi passes `fun` to lavaan::lavaanList(), which calls it after
      # each converged fit and keeps its NULL; nothing it returns is pooled.
      count <- new.env(parent = emptyenv())
      count$done <- 0L
      total <- length(completed)
      args$fun <- function(object) {
        count$done <- count$done + 1L
        progress(stage, count$done, total)
        NULL
      }
    }
    do.call(constructor, c(args, engine_args))
  }
  usable_fits <- function(fit) vapply(fit@convergence, function(x) {
    length(x) >= 2L && isTRUE(x[[1L]]) && isTRUE(x[[2L]])
  }, logical(1))
  # Rubin's (1987) rules parameter by parameter: scale.W = FALSE gives
  # T = W + (1 + 1/M)B for each parameter, not lavaan.mi's default of W
  # scaled by the average relative increase in variance (see Details).
  pool <- function(fit) lavaan.mi::parameterEstimates.mi(fit, fmi = TRUE,
    ci = TRUE, level = conf_level, asymptotic = FALSE, scale.W = FALSE,
    omit.imps = c("no.conv", "no.se"))
  run <- sieve_with_seed(seed, {
    impute <- function(m, stage) {
      note(stage, 0L, m)
      mice::mice(X, m = m, printFlag = FALSE, seed = NA)
    }
    pilot_data <- impute(as.integer(pilot), "pilot_imputation")
    pilot_fit <- fit_sets(mice::complete(pilot_data, "all"), "pilot")
    pilot_used <- sum(usable_fits(pilot_fit))
    if (pilot_used != pilot) stop(sprintf(
      "Only %d of %d pilot fits converged with standard errors. Review the imputation model and fit before planning the final count.",
      pilot_used, pilot), call. = FALSE)
    pilot_estimates <- pool(pilot_fit)
    usable <- is.finite(pilot_estimates$fmi) & pilot_estimates$se > 0
    if (!any(usable)) stop("The pilot supplied no usable FMI estimates. Review convergence and imputation diagnostics.", call. = FALSE)
    plan <- sieve_mi_plan(pilot_estimates$fmi[usable], pilot = pilot, cv = cv)
    plan$table <- cbind(pilot_estimates[usable, intersect(c("lhs", "op", "rhs", "group"), names(pilot_estimates)), drop = FALSE], plan$table)
    completed <- if (plan$imputations > pilot) impute(plan$imputations, "final_imputation") else pilot_data
    fit <- if (plan$imputations > pilot) fit_sets(mice::complete(completed, "all"), "final") else pilot_fit
    if (plan$imputations > pilot) note("pooling", plan$imputations, plan$imputations)
    estimates <- if (plan$imputations > pilot) pool(fit) else pilot_estimates
    used <- usable_fits(fit)
    improper <- vapply(fit@convergence, function(x) length(x) >= 4L &&
      (isTRUE(x[[3L]]) || isTRUE(x[[4L]])), logical(1))
    list(fit = fit, estimates = estimates, plan = plan, pilot_estimates = pilot_estimates,
      imputations = plan$imputations, used = sum(used), omitted = sum(!used),
      improper = sum(improper & used), improper_imputations = which(improper & used),
      imputation_events = completed$loggedEvents, pilot_events = pilot_data$loggedEvents)
  })
  c(run$value, list(rng = run$rng, pooled_variance = pooled_variance,
    method = paste0("mice imputation; WLSMV in lavaan.mi; each parameter pooled by Rubin's (1987) rules: ",
      "T = W + (1 + 1/M)B, a t reference with df = (M - 1)(1 + W/((1 + 1/M)B))^2, and FMI = (1 + 1/M)B/T"),
    conf_level = conf_level, engine_versions = stats::setNames(lapply(c("mice", "lavaan.mi"),
      function(package) as.character(getNamespaceVersion(package))), c("mice", "lavaan.mi"))))
}

#' The ordered-item MI analysis as the background process runs it
#'
#' Runs `sieve_mi_run()` with a progress hook that writes `stage done
#' total` to the file `progress` in the job's folder (at most about five
#' times a second, at each new stage and at a stage's last fit), keeping
#' every warning, message and error (`sieve_capture()`), the seconds the
#' analysis took and, after a run that succeeded, the identities of the
#' mice and lavaan.mi namespaces that computed it
#' (`sieve_engine_identity()`). Neither is loaded in the Shiny process, so
#' the replay record of the step takes their identities from here
#' (`sieve_log()`'s `engine_identity`), and a replay under another build of
#' either is flagged. The lavaan.mi fit is not returned: unserializing it in
#' the Shiny process would load lavaan.mi's namespace there (its stored
#' call holds a closure of that namespace) while lavaan is not attached,
#' which keeps lavaan.mi's methods from dispatching in the user's session
#' for the rest of it (see `sieve_mi_model()`); the panel shows the pooled
#' tables only, and the recorded call gives the fit to anyone who wants it.
#' @param args The arguments of `sieve_mi_model()`, `fun` resolved.
#' @param dir The job's folder.
#' @param rng_kind The [RNGkind()] settings of the session that drew the
#'   seed.
#' @return A list with `ok`, `value` (the result without `fit`), `error`,
#'   `error_class`, `warnings`, `messages`, `seconds` and, when `ok`,
#'   `engine_identity` (the records of mice and lavaan.mi).
#' @author Ken Kelley
#' @noRd
sieve_mi_worker <- function(args, dir, rng_kind = NULL) {
  file <- file.path(dir, "progress")
  last <- new.env(parent = emptyenv())
  last$time <- -Inf
  last$stage <- ""
  progress <- function(stage, done, total) {
    now <- proc.time()[["elapsed"]]
    if (!identical(stage, last$stage) || done >= total || now - last$time >= 0.2) {
      part <- paste0(file, ".part")
      writeLines(sprintf("%s %d %d", stage, as.integer(done), as.integer(total)), part)
      file.rename(part, file)
      last$time <- now
      last$stage <- stage
    }
    invisible(TRUE)
  }
  started <- proc.time()[["elapsed"]]
  run <- sieve_capture(do.call(sieve_mi_run, c(args, list(progress = progress, rng_kind = rng_kind))))
  run$seconds <- proc.time()[["elapsed"]] - started
  if (isTRUE(run$ok)) {
    run$value$fit <- NULL
    run$engine_identity <- sieve_engine_identity(c("mice", "lavaan.mi"))
  }
  run
}

#' Start the ordered-item MI analysis in a background R process
#'
#' One background R process (callr), which loads the DMAR and SIEVER this
#' session loaded (`sieve_worker_paths()`, checked by
#' `sieve_worker_bootstrap()`) and runs `sieve_mi_worker()`. Its output and
#' error streams go to files in the job's folder, never to pipes: nothing
#' reads a pipe while the process runs, and a process that filled one would
#' wait on it for ever (the EFA workers write to files for the same
#' reason). The error
#' stream's last lines say why a process ended without a result.
#' @param args The arguments of `sieve_mi_model()`, `fun` resolved.
#' @param rng_kind The [RNGkind()] settings of this session.
#' @return A job (see `sieve_mi_handle()`).
#' @author Ken Kelley
#' @noRd
sieve_mi_start <- function(args, rng_kind = RNGkind()) {
  dir <- tempfile("siever_mi_")
  dir.create(dir)
  paths <- sieve_worker_paths()
  bootstrap <- sieve_worker_bootstrap
  environment(bootstrap) <- baseenv()
  child <- function(args, rng_kind, paths, bootstrap, dir) {
    bootstrap(paths)
    worker <- utils::getFromNamespace("sieve_mi_worker", "SIEVER")
    worker(args, dir, rng_kind = rng_kind)
  }
  environment(child) <- baseenv()
  process <- tryCatch(
    callr::r_bg(child,
      args = list(args = args, rng_kind = rng_kind, paths = paths, bootstrap = bootstrap, dir = dir),
      libpath = .libPaths(), supervise = TRUE, user_profile = FALSE,
      stdout = file.path(dir, "stdout.txt"), stderr = file.path(dir, "stderr.txt")
    ),
    error = function(e) {
      unlink(dir, recursive = TRUE)
      stop(e)
    }
  )
  sieve_mi_handle(process, dir)
}

#' A handle on the background MI process
#'
#' @param process The callr process.
#' @param dir The job's folder.
#' @return An environment with `process`, `dir`, `progress` (`stage`,
#'   `done`, `total`; `stage` is `"starting"` until the process reports),
#'   and the functions `poll()` (read the progress; `TRUE` while the
#'   process runs), `result()` (the run of `sieve_mi_worker()`, or a
#'   failure that says why the process ended without one; removes the
#'   folder) and `kill()` (end the process and remove the folder).
#' @author Ken Kelley
#' @noRd
sieve_mi_handle <- function(process, dir) {
  job <- new.env(parent = emptyenv())
  job$process <- process
  job$dir <- dir
  job$progress <- list(stage = "starting", done = 0L, total = NA_integer_)
  job$poll <- function() {
    alive <- tryCatch(process$is_alive(), error = function(e) FALSE)
    file <- file.path(dir, "progress")
    if (file.exists(file)) {
      line <- tryCatch(readLines(file, warn = FALSE), error = function(e) character(0))
      parts <- strsplit(line[1L] %||% "", " ", fixed = TRUE)[[1L]]
      counts <- suppressWarnings(as.integer(parts[-1L]))
      if (length(parts) == 3L && all(is.finite(counts))) {
        job$progress <- list(stage = parts[1L], done = counts[1L], total = counts[2L])
      }
    }
    alive
  }
  job$result <- function() {
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    tryCatch(process$get_result(), error = function(e) {
      list(ok = FALSE, value = NULL, warnings = character(0), messages = character(0),
        error = paste(c("The background R process ended without a result:", conditionMessage(e),
          sieve_efa_worker_output(dir)), collapse = " "))
    })
  }
  job$kill <- function() {
    tryCatch(process$kill(), error = function(e) FALSE)
    unlink(dir, recursive = TRUE)
    invisible(TRUE)
  }
  job
}

#' The largest final imputation count the MI plan can set
#'
#' M = 1 + (1/2)(FMI/CV)^2 at the largest possible upper FMI bound, 1
#' (von Hippel, 2020): 201 at CV = .05.
#' @param cv Target CV of the standard errors.
#' @return An integer.
#' @author Ken Kelley
#' @noRd
sieve_mi_most_imputations <- function(cv = .05) {
  as.integer(ceiling(1 + .5 * (1 / cv)^2))
}

#' The wait of an MI request, before it starts
#'
#' Each completed data set gets one WLSMV fit: the pilot's, then M final
#' fits when M exceeds the pilot (otherwise the pilot's fits are the final
#' ones). A fit's time, imputation included, is the rate `mi_fit` per item
#' pair (`sieve_rate()`; measured in this session once an MI run has
#' finished).
#' @param p The model's observed variables.
#' @param pilot The pilot count.
#' @param cv Target CV.
#' @return A list of seconds: `per_fit`, `pilot`, `most` (the run at the
#'   largest M), and the integer `most_m`.
#' @author Ken Kelley
#' @noRd
sieve_mi_wait <- function(p, pilot = 20L, cv = .05) {
  per_fit <- sieve_rate("mi_fit") * sieve_pairs(max(2L, p))
  most_m <- sieve_mi_most_imputations(cv)
  list(per_fit = per_fit, pilot = pilot * per_fit, most = (pilot + most_m) * per_fit,
    most_m = most_m)
}

#' Where a running MI analysis is, in words
#' @param progress The job's `progress` (`stage`, `done`, `total`).
#' @param pilot The pilot count.
#' @return A length one character string.
#' @author Ken Kelley
#' @noRd
sieve_mi_progress_words <- function(progress, pilot = 20L) {
  done <- as.integer(progress$done %||% 0L)
  total <- as.integer(progress$total %||% NA_integer_)
  switch(progress$stage %||% "starting",
    pilot_imputation = sprintf("imputing the %d pilot data sets with mice", total),
    pilot = sprintf("pilot fit %d of %d", done, total),
    final_imputation = sprintf("the pilot set M = %d; imputing the %d final data sets with mice", total, total),
    final = sprintf("final fit %d of %d, after the %d pilot fits", done, total, pilot),
    pooling = "pooling the final fits by Rubin's rules",
    "starting a background R process and loading SIEVER, mice and lavaan.mi")
}

#' What a canceled MI run had done, in words
#' @inheritParams sieve_mi_progress_words
#' @return The page's note: `"Stopped: canceled after ...; nothing was
#'   computed or recorded."`
#' @author Ken Kelley
#' @noRd
sieve_mi_cancel_text <- function(progress, pilot = 20L) {
  done <- as.integer(progress$done %||% 0L)
  total <- as.integer(progress$total %||% NA_integer_)
  when <- switch(progress$stage %||% "starting",
    pilot_imputation = sprintf("while the %d pilot data sets were imputed, before any fit", total),
    pilot = sprintf("after %d of %d pilot fits", done, total),
    final_imputation = sprintf("after the %d pilot fits, while the M = %d final data sets were imputed", pilot, total),
    final = sprintf("after the %d pilot fits and %d of %d final fits", pilot, done, total),
    pooling = "after every fit, while the final fits were pooled",
    "before the background R process began")
  sprintf("Stopped: canceled %s; nothing was computed or recorded.", when)
}

#' The progress line of a background MI run, with its Cancel button
#'
#' The line is static UI, shown while the run goes on (a conditional
#' panel on the server's `running` flag); only the sentence inside it is
#' redrawn, as text, at each poll. A line drawn by a `renderUI()` twice a
#' second would replace the Cancel button each time: a keyboard user's
#' focus on it would fall to the page body within half a second, and a
#' mouse press whose release came after a redraw would land on a button no
#' longer in the page. The button is the same element for the whole run. A text output whose sentence is unchanged is
#' not rewritten, so the live region is not touched between changes.
#' @param ns Module namespace.
#' @param running Id of the server's text output, `"true"` while the run
#'   goes on and `"false"` otherwise (a text output, as for
#'   `inv_impact_kind`, so that `shiny::testServer()` reads it as well).
#' @param progress Id of the text output holding the progress sentence.
#' @param cancel The Cancel control, or a conditional panel holding it.
#' @return UI tags.
#' @author Ken Kelley
#' @noRd
sieve_mi_progress_ui <- function(ns, running, progress, cancel) {
  shiny::conditionalPanel(sprintf("output['%s'] === 'true'", ns(running)),
    shiny::div(class = "d-flex align-items-center gap-2 small my-2",
      shiny::span(role = "status", shiny::textOutput(ns(progress), inline = TRUE)),
      cancel))
}

#' Ordered-Item MI Sensitivity Panel
#' @param ns Module namespace.
#' @return UI tags.
#' @author Ken Kelley
#' @noRd
sieve_mi_ui <- function(ns) {
  bslib::card(bslib::card_header("Ordered-Item MI Sensitivity Analysis"),
    shiny::p(class = "small text-muted",
      "Fit the ordinary model first, then request this separate sensitivity analysis when ordered or binary indicators have missing responses. mice imputes under MAR and lavaan.mi fits WLSMV to each completed data set. Binary items use logistic regression; ordered items use proportional odds; continuous variables use predictive mean matching. Review substantive compatibility and imputation diagnostics. Continuous FIML remains the study default."),
    sieve_settings("MI Settings", ids = ns(c("mi_sensitivity_level", "mi_sensitivity_seed")),
      sieve_conf_level_input(ns("mi_sensitivity_level")),
      shiny::numericInput(ns("mi_sensitivity_seed"), "Seed (Optional)", value = NA, min = 0)),
    shiny::p(class = "small text-muted",
      "Twenty successful pilot fits determine M from the largest upper 95% FMI bound using M = 1 + (1/2)(FMI/.05)^2 (von Hippel, 2020). Each parameter is pooled on its own by Rubin's (1987) rules: with W the mean within-imputation variance (squared standard error) and B the between-imputation variance of its M completed-data estimates, the pooled variance is T = W + (1 + 1/M)B, its interval uses a t reference distribution with df = (M - 1)(1 + W/((1 + 1/M)B))^2, and its fraction of missing information is FMI = (1 + 1/M)B/T. Converged improper fits are retained and flagged; failed fits or unavailable standard errors are counted separately."),
    shiny::uiOutput(ns("mi_sensitivity_wait")),
    shiny::actionButton(ns("mi_sensitivity_go"), "Run MI Sensitivity Analysis", class = "btn-primary"),
    sieve_mi_progress_ui(ns, "mi_sensitivity_running", "mi_sensitivity_progress",
      shiny::actionButton(ns("mi_sensitivity_stop"), "Cancel the MI run", class = "btn-sm btn-outline-secondary")),
    shiny::uiOutput(ns("mi_sensitivity_note")), DT::DTOutput(ns("mi_sensitivity_table")),
    DT::DTOutput(ns("mi_sensitivity_plan")), DT::DTOutput(ns("mi_sensitivity_events")))
}

#' Run the Separate Ordered-Item Sensitivity Analysis
#'
#' The analysis runs in a background R process (`sieve_mi_start()`), polled
#' here twice a second: the page stays usable, the progress line counts the
#' imputations and fits, and Cancel ends the process. (In the Shiny
#' process, a run of M = 70 at nine items holds it for 31 seconds with a
#' progress bar that never moves, blocking every session of a shared
#' server.) Nothing is registered or logged until the run
#' finishes, and only when the ordinary model, the data and the item
#' ledger are those of the request; a change while it runs stops it.
#' @param input,output,session Module interfaces.
#' @param state Shared study state.
#' @param parent Reactive ordinary CFA or SEM result.
#' @param current Reactive currentness predicate.
#' @return The held result, the current display reader, and whether a run
#'   is in progress.
#' @author Ken Kelley
#' @noRd
sieve_mi_server <- function(input, output, session, state, parent, current) {
  held <- shiny::reactiveVal(NULL)
  inputs_key <- shiny::reactive(sieve_hash(list(raw = state$raw, items = state$items, roles = state$roles)))
  job <- new.env(parent = emptyenv())
  running <- shiny::reactiveVal(FALSE)
  tick <- shiny::reactiveVal(0L)
  # Whether the request a job runs for is no longer the current one: the
  # ordinary model, its fit, the data or the item ledger changed.
  stale <- function() {
    base <- job$base
    if (!isTRUE(current())) return(TRUE)
    # A model the page can no longer give (its reactive stops on req(), as
    # the SEM page's does when the model choices change) is a changed
    # model. The stop is caught here: left to the polling observer, it
    # would end every poll silently, so the run would be neither stopped
    # nor read and the panel would say it was running for as long as the
    # model was unavailable.
    run <- tryCatch(parent(), shiny.silent.error = function(e) NULL)
    if (is.null(run)) return(TRUE)
    !identical(base$spec_key, run$spec$key) ||
      !identical(base$inputs_key, inputs_key()) || !identical(base$parent_fit, run$fit) ||
      !sieve_same_revision(base$revision, sieve_revision(state))
  }
  finish <- function(read) {
    job$handle <- NULL
    running(FALSE)
    held(c(list(read = read), job$base))
  }
  # Registered and logged on completion, from the request as it was made,
  # with the identities of the mice and lavaan.mi that computed it in the
  # background process (`engines`; this process loads neither).
  record <- function(answer, engines = NULL) {
    request <- job$request
    spec <- request$spec
    mi_spec <- list(kind = "ordered_mi", model = spec$model, ordered = request$args$ordered,
      estimator = "WLSMV", missing = "multiple_imputation", fun = spec$fun,
      aux = request$args$aux, std_lv = spec$std_lv, conf_level = request$args$conf_level,
      seed = request$rng$seed, seed_supplied = request$rng$supplied,
      pilot = 20L, cv = .05, imputations = answer$imputations,
      engine_args = request$args$engine_args, pooled_variance = request$args$pooled_variance)
    mi_spec$key <- sieve_hash(mi_spec)
    sieve_register_analysis(state, list(spec = mi_spec,
      status = list(code = if (answer$improper > 0L) "inadmissible" else "ok",
        message = sprintf("%d fits pooled, %d omitted, %d improper retained.", answer$used, answer$omitted, answer$improper)),
      warnings = if (answer$omitted > 0L) "The planned precision target is not assured after omitted imputations.",
      engine_versions = answer$engine_versions,
      engine_identity = c(sieve_engine_identity(), engines),
      estimates = list(parameters = answer$estimates, count_plan = answer$plan,
        used = answer$used, omitted = answer$omitted, improper = answer$improper),
      n_fitted = nrow(request$args$data)),
      note = "Ordered-item multiple imputation sensitivity analysis")
    mapping <- request$mapping
    aux <- request$args$aux
    preparation <- c("mi_data <- items", vapply(names(mapping), function(name)
      sprintf("mi_data[[%s]] <- d[[%s]]", sieve_r_literal(name), sieve_r_literal(mapping[[name]])), character(1)),
      vapply(aux, function(name) sprintf("mi_data[[%s]] <- d[[%s]]",
        sieve_r_literal(name), sieve_r_literal(name)), character(1)))
    sieve_log(state, sprintf("Ordered-item MI sensitivity: M = %d, %d used, %d omitted",
      answer$imputations, answer$used, answer$omitted), c(preparation,
      sprintf("mi_sensitivity <- SIEVER::sieve_mi_model(%s, mi_data, ordered = %s, aux = %s, fun = %s, conf_level = %s, seed = %d, std_lv = %s, engine_args = %s, pooled_variance = %s)",
        sieve_r_literal(spec$model), sieve_r_literal(request$args$ordered), sieve_r_literal(aux),
        sieve_r_literal(spec$fun), sieve_r_literal(request$args$conf_level), request$rng$seed,
        sieve_r_literal(spec$std_lv), paste(utils::capture.output(dput(request$args$engine_args)), collapse = ""),
        sieve_r_literal(request$args$pooled_variance)),
      "mi_sensitivity$estimates", "mi_sensitivity$plan", "mi_sensitivity$imputation_events"),
      engine_identity = engines)
    invisible(TRUE)
  }
  shiny::observeEvent(input$mi_sensitivity_go, {
    if (isTRUE(running())) return(invisible(NULL))
    shiny::req(isTRUE(current()))
    run <- parent()
    spec <- run$spec
    shiny::req(spec$model)
    revision <- sieve_revision(state)
    level <- sieve_finite_or(input$mi_sensitivity_level, .95)
    requested_seed <- sieve_seed_input(input$mi_sensitivity_seed)
    rng <- sieve_draw_seed(requested_seed)
    base <- list(spec_key = spec$key, parent_fit = run$fit, inputs_key = inputs_key(),
      revision = revision, level = level, requested_seed = requested_seed)
    prep <- sieve_capture({
      problem <- sieve_sampling_problem(state, capability = "independent")
      if (!is.null(problem)) stop(problem, call. = FALSE)
      meta <- shiny::isolate(state$meta)
      ordered <- intersect(spec$observed, unique(c(spec$ordered,
        meta$item[meta$type %in% c("ordered", "binary")])))
      raw <- shiny::isolate(state$raw)
      roles <- shiny::isolate(sieve_roles_table(state$raw, state$items, state$roles))
      aux <- if (is.null(roles)) character() else intersect(roles$variable[roles$role == "auxiliary"], names(raw))
      aux <- setdiff(aux, c(spec$observed, spec$latent))
      data <- shiny::isolate(state$items)
      mapping <- c(spec$out_map, spec$pred_map)
      for (name in names(mapping)) data[[name]] <- raw[[mapping[[name]]]]
      for (name in aux) data[[name]] <- raw[[name]]
      extensions <- spec$engine_args[intersect(names(spec$engine_args), c("fixed.x", "conditional.x"))]
      # Rubin's (1987) rules parameter by parameter, named in the request,
      # its key and the recorded call (see sieve_mi_model()).
      args <- list(model = spec$model, data = data, ordered = ordered, aux = aux,
        fun = spec$fun, conf_level = level, seed = rng$seed, std_lv = spec$std_lv,
        engine_args = extensions, pooled_variance = "rubin_parameterwise")
      # Refused here, at once, rather than by the background process.
      sieve_mi_check(spec$model, data, ordered, aux = aux, conf_level = level,
        engine_args = extensions, pooled_variance = args$pooled_variance)
      list(args = args, mapping = mapping, handle = sieve_mi_start(args, rng_kind = rng$kind))
    })
    job$base <- base
    if (!isTRUE(prep$ok)) {
      held(c(list(read = prep), base))
      return(invisible(NULL))
    }
    job$handle <- prep$value$handle
    job$request <- list(spec = spec, args = prep$value$args, mapping = prep$value$mapping, rng = rng)
    job$started <- proc.time()[["elapsed"]]
    job$stage_started <- list()
    held(NULL)
    running(TRUE)
  })
  shiny::observeEvent(input$mi_sensitivity_stop, {
    if (!isTRUE(running()) || is.null(job$handle)) return(invisible(NULL))
    progress <- job$handle$progress
    job$handle$kill()
    finish(list(ok = FALSE, value = NULL, warnings = character(0), messages = character(0),
      error = sieve_mi_cancel_text(progress, job$request$args$pilot %||% 20L)))
  })
  shiny::observe({
    if (!isTRUE(running())) return(invisible(NULL))
    shiny::invalidateLater(500)
    handle <- job$handle
    alive <- handle$poll()
    stage <- handle$progress$stage
    if (is.null(job$stage_started[[stage]])) job$stage_started[[stage]] <- proc.time()[["elapsed"]]
    changed <- shiny::isolate(stale())
    if (alive && !changed) {
      shiny::isolate(tick(tick() + 1L))
      return(invisible(NULL))
    }
    if (changed) {
      handle$kill()
      finish(list(ok = FALSE, value = NULL, warnings = character(0), messages = character(0),
        error = paste("The ordinary model, the data or the item ledger changed while the MI sensitivity",
          "analysis ran, so it was stopped; nothing was recorded. Run it again for the current model.")))
      return(invisible(NULL))
    }
    read <- handle$result()
    if (isTRUE(read$ok)) {
      answer <- read$value
      answer$rng <- job$request$rng
      pilot <- job$request$args$pilot %||% 20L
      fits <- pilot + if (answer$imputations > pilot) answer$imputations else 0L
      sieve_rate_note("mi_fit", read$seconds, fits * sieve_pairs(length(job$request$spec$observed)))
      recorded <- shiny::isolate(sieve_capture(record(answer, read$engine_identity)))
      read <- list(ok = recorded$ok, value = if (recorded$ok) answer, error = recorded$error,
        warnings = unique(c(read$warnings, recorded$warnings)),
        messages = unique(c(read$messages, recorded$messages)))
    }
    finish(read)
  })
  session$onSessionEnded(function() {
    if (!is.null(job$handle)) try(job$handle$kill(), silent = TRUE)
  })
  output$mi_sensitivity_wait <- shiny::renderUI({
    # Redrawn after each run, whose time sets the rate measured in this
    # session.
    held()
    p <- length(parent()$spec$observed)
    shiny::req(p > 0L)
    wait <- sieve_mi_wait(p)
    shiny::p(class = "small text-muted", sprintf(paste(
      "Each completed data set is fitted once: %d pilot fits, then M final fits, with M set from the pilot",
      "(%d to %d at CV = .05; when M = %d the pilot's fits are the final ones). At the rate %s, a fit of",
      "these %d observed variables, imputation included, takes %s: %s for the pilot and %s for the whole",
      "run at M = %d. It runs in a background R process, so the page stays usable; the progress line",
      "counts the fits, and Cancel ends the run."),
      20L, 20L, wait$most_m, 20L,
      if (exists("mi_fit", envir = sieve_rates, inherits = FALSE)) "measured in this session" else "of SIEVER's reference run (nine items, N = 301)",
      p, sieve_duration_label(wait$per_fit), sieve_duration_label(wait$pilot),
      sieve_duration_label(wait$most), wait$most_m))
  })
  # The progress line and its Cancel button are static UI
  # (sieve_mi_progress_ui()), shown while this flag is "true"; only the
  # sentence is redrawn at each poll, so the button keeps focus and clicks.
  output$mi_sensitivity_running <- shiny::renderText(if (isTRUE(running())) "true" else "false")
  shiny::outputOptions(output, "mi_sensitivity_running", suspendWhenHidden = FALSE)
  output$mi_sensitivity_progress <- shiny::renderText({
    tick()
    if (!isTRUE(running()) || is.null(job$handle)) return(NULL)
    progress <- job$handle$progress
    now <- proc.time()[["elapsed"]]
    done <- as.integer(progress$done %||% 0L)
    total <- as.integer(progress$total %||% NA_integer_)
    since <- job$stage_started[[progress$stage]]
    left <- if (progress$stage %in% c("pilot", "final") && done > 0L && !is.na(total) && !is.null(since)) {
      sprintf(", %s left in this stage", sieve_duration_label((now - since) / done * max(total - done, 0L)))
    } else ""
    sprintf("Running in a background R process: %s; %s elapsed%s.",
      sieve_mi_progress_words(progress, job$request$args$pilot %||% 20L),
      sieve_duration_label(now - job$started), left)
  })
  shown <- shiny::reactive({
    x <- held()
    if (is.null(x)) return(list(reason = "No MI sensitivity analysis has been requested."))
    if (!isTRUE(current()) || !identical(x$spec_key, parent()$spec$key) ||
        !identical(x$inputs_key, inputs_key()) ||
        !identical(x$parent_fit, parent()$fit) ||
        !sieve_same_revision(x$revision, sieve_revision(state)) ||
        !identical(x$level, sieve_finite_or(input$mi_sensitivity_level, .95)) ||
        !identical(x$requested_seed, sieve_seed_input(input$mi_sensitivity_seed))) {
      return(list(reason = "The MI sensitivity analysis is historical. Update the ordinary model and run MI again."))
    }
    if (!x$read$ok) return(list(reason = x$read$error))
    r <- x$read$value
    list(value = r, reason = paste(sprintf("M = %d; %d fits pooled; %d omitted for nonconvergence or unavailable standard errors; %d improper fits retained.",
      r$imputations, r$used, r$omitted, r$improper),
      if (r$omitted) "The planned precision target is not assured after these omissions." else "",
      paste(x$read$warnings, collapse = " ")))
  })
  output$mi_sensitivity_note <- shiny::renderUI({
    if (isTRUE(running())) return(NULL)
    shiny::p(class = "small text-muted", shown()$reason)
  })
  output$mi_sensitivity_table <- DT::renderDT({
    shiny::req(shown()$value)
    sieve_dt(shown()$value$estimates, rownames = FALSE, options = list(scrollX = TRUE))
  })
  output$mi_sensitivity_plan <- DT::renderDT({
    shiny::req(shown()$value)
    sieve_dt(shown()$value$plan$table, rownames = FALSE)
  })
  output$mi_sensitivity_events <- DT::renderDT({
    events <- shown()$value$imputation_events
    shiny::req(events)
    sieve_dt(events, rownames = FALSE)
  })
  list(held = held, shown = shown, running = running)
}

#' The DMAR call of categorical omega under MAR, checked
#'
#' Checks the request (DMAR 1.1.0 and mice available, at least three
#' ordered or binary items, some item response missing) and returns the one
#' DMAR call that computes categorical omega with its bootstrap-then-impute
#' percentile interval, in the form `sieve_dmar_boot_run()` and the
#' background process take.
#'
#' @param data Aligned included items and optional auxiliary variables.
#' @param items Ordered or binary indicator names.
#' @param aux Names used only by the imputation model.
#' @param B,imputations Bootstrap count and imputations per replication
#'   (DMAR's defaults, 1000 and 20).
#' @param seed Optional random seed.
#' @param conf_level Confidence level.
#' @param denominator Observed or model-implied denominator.
#' @return A list with `fun` and `args`.
#' @author Ken Kelley
#' @noRd
sieve_ordinal_omega_mi_call <- function(data, items, aux = NULL, B = 1000L,
                                        imputations = 20L, seed = NULL,
                                        conf_level = .95, denominator = "observed") {
  request <- sieve_dmar_boot_request(B, conf_level = conf_level)
  if (!request$ok) stop(request$message, call. = FALSE)
  if (!sieve_have("mice")) stop("Install mice to use categorical omega under MAR.", call. = FALSE)
  items <- as.character(items)
  aux <- setdiff(as.character(aux), items)
  if (!length(aux)) aux <- NULL
  if (length(items) < 3L) stop("Select at least three ordered or binary items in one construct.", call. = FALSE)
  X <- sieve_reliability_data(data, c(items, aux))
  if (!anyNA(X[, items, drop = FALSE])) {
    stop("No included item responses are missing; use the ordinary categorical omega analysis.", call. = FALSE)
  }
  list(fun = "reliability_omega_categorical", args = list(data = X, missing = "mi", aux = aux,
    ci_method = "percentile", conf_level = conf_level, B = B, seed = seed,
    denominator = denominator, imputations = imputations))
}

#' Categorical Omega Under MAR, With Bootstrap Then Imputation
#'
#' Runs `sieve_ordinal_omega_mi_call()`'s DMAR call in this process. The
#' Reliability tab runs the same call in a background R process
#' (`sieve_ordinal_omega_mi_start()`).
#'
#' @inheritParams sieve_ordinal_omega_mi_call
#' @return The unchanged DMAR result, including imputation diagnostics.
#' @author Ken Kelley
#' @noRd
sieve_ordinal_omega_mi <- function(data, items, aux = NULL, B = 1000L,
                                  imputations = 20L, seed = NULL,
                                  conf_level = .95, denominator = "observed") {
  call <- sieve_ordinal_omega_mi_call(data, items, aux = aux, B = B, imputations = imputations,
    seed = seed, conf_level = conf_level, denominator = denominator)
  run <- sieve_dmar_boot_run(call)
  if (!isTRUE(run$ok)) stop(sieve_dmar_boot_failure_text(run), call. = FALSE)
  run$value
}

#' Start categorical omega under MAR in a background R process
#'
#' @param call The call from `sieve_ordinal_omega_mi_call()`.
#' @return A job (`sieve_dmar_boot_handle()`).
#' @author Ken Kelley
#' @noRd
sieve_ordinal_omega_mi_start <- function(call) {
  sieve_dmar_boot_start(list(call))
}

#' A Separate Categorical Reliability Sensitivity Panel
#' @param ns Module namespace.
#' @return UI tags.
#' @author Ken Kelley
#' @noRd
sieve_ordinal_omega_mi_ui <- function(ns) {
  bslib::card(bslib::card_header("Categorical Omega Under MAR"),
    shiny::p("For a construct whose included items are ordered or binary, compare the ordinary complete-case result with multiple imputation under MAR given the items and declared auxiliaries. Auxiliaries predict missing responses without entering the composite. This is a separate sensitivity analysis; review the imputation model and diagnostics."),
    shiny::p("DMAR uses predictive mean matching and a bootstrap-then-impute percentile interval (Schomaker & Heumann, 2018; Bartlett & Hughes, 2020). It plans the point estimate's imputation count from the upper 95% FMI bound and CV = .05 (von Hippel, 2020). Each bootstrap replication averages its own imputations. Undefined completed-data estimates are set aside and counted; an estimate or interval is refused when fewer than half return a value. The finite improper-draw rule for continuous omega does not define categorical omega for an improper categorical solution."),
    shiny::selectInput(ns("ordinal_mi_construct"), "Construct for MI Sensitivity", choices = character()),
    sieve_settings("Categorical MI Settings", ids = ns(c("ordinal_mi_B", "ordinal_mi_M", "ordinal_mi_seed", "ordinal_mi_level", "ordinal_mi_denominator")),
      shiny::numericInput(ns("ordinal_mi_B"), "Bootstrap Replications", value = 1000L, min = 400L),
      shiny::numericInput(ns("ordinal_mi_M"), "Imputations per Bootstrap Replication", value = 20L, min = 2L),
      shiny::numericInput(ns("ordinal_mi_seed"), "Seed (Optional)", value = NA, min = 0),
      sieve_conf_level_input(ns("ordinal_mi_level")),
      shiny::selectInput(ns("ordinal_mi_denominator"), "Omega Denominator", choices = c("Observed" = "observed", "Model Implied" = "model_implied"))),
    shiny::p(class = "small text-muted", "This request refits an imputation model and several categorical models per bootstrap replication, plus the point-estimate imputations: with DMAR's defaults (1000 replications, 20 imputations each) it can take hours. It runs only when requested, in a background R process, so the page stays usable; the progress line counts the replications and estimates the time left once they start, and Cancel ends the run."),
    shiny::uiOutput(ns("ordinal_mi_requirements")),
    shiny::actionButton(ns("ordinal_mi_go"), "Compute Categorical MI Omega and Interval", class = "btn-primary"),
    sieve_mi_progress_ui(ns, "ordinal_mi_running", "ordinal_mi_progress",
      shiny::conditionalPanel(sprintf("output['%s'] === 'true'", ns("ordinal_mi_cancelable")),
        shiny::actionButton(ns("ordinal_mi_stop"), "Cancel the imputation run", class = "btn-sm btn-outline-secondary"))),
    shiny::uiOutput(ns("ordinal_mi_note")),
    DT::DTOutput(ns("ordinal_mi_table")),
    shiny::verbatimTextOutput(ns("ordinal_mi_diagnostics")))
}

#' Wire Categorical Omega's Independent MI Request
#' @param input,output,session Module interfaces.
#' @param state Shared study state.
#' @return A reactive current result.
#' @author Ken Kelley
#' @noRd
sieve_ordinal_omega_mi_server <- function(input, output, session, state) {
  held <- shiny::reactiveVal(NULL)
  groups <- shiny::reactive({
    meta <- state$meta
    if (is.null(meta)) return(list())
    meta <- meta[meta$status == "active" & sieve_construct_assigned(meta$construct), , drop = FALSE]
    parts <- split(meta, meta$construct)
    lapply(Filter(function(m) nrow(m) >= 3L && all(m$type %in% c("ordered", "binary")), parts), function(m) m$item)
  })
  shiny::observeEvent(names(groups()), {
    choices <- names(groups())
    old <- shiny::isolate(input$ordinal_mi_construct)
    shiny::updateSelectInput(session, "ordinal_mi_construct", choices = choices,
      selected = if (length(old) == 1L && old %in% choices) old else head(choices, 1L))
  }, ignoreNULL = FALSE)
  settings <- shiny::reactive(list(
    construct = input$ordinal_mi_construct,
    B = sieve_finite_or(input$ordinal_mi_B, 1000L),
    imputations = as.integer(sieve_finite_or(input$ordinal_mi_M, 20L)),
    seed = sieve_seed_input(input$ordinal_mi_seed),
    conf_level = sieve_finite_or(input$ordinal_mi_level, .95),
    denominator = input$ordinal_mi_denominator %||% "observed"))
  output$ordinal_mi_requirements <- shiny::renderUI({
    request <- sieve_dmar_boot_request(settings()$B, conf_level = settings()$conf_level)
    shiny::p(class = "small text-muted", if (!request$ok) request$message else sprintf(
      "At this confidence level at least %d bootstrap replications must be requested; the entered count is retained.", request$required))
  })
  shiny::observeEvent(input$ordinal_mi_level, {
    shiny::updateNumericInput(session, "ordinal_mi_B", min = sieve_dmar_boot_required("percentile",
      sieve_finite_or(input$ordinal_mi_level, .95)))
  }, ignoreInit = FALSE)
  key <- shiny::reactive(sieve_hash(list(settings = settings(), raw = state$raw,
    items = state$items, meta = sieve_ledger_analytic(state$meta), roles = sieve_roles_table(state$raw, state$items, state$roles)[c("variable", "role")],
    sampling_design = state$sampling_design, confirmed = sieve_confirmation_status(state))))
  # The run: DMAR's call in a background R process
  # (sieve_ordinal_omega_mi_start()), polled here, with DMAR's progress
  # callback counting the replications and Stop asking DMAR to cancel; with
  # DMAR's defaults (20 imputations per replication) a run can take hours,
  # which the Shiny process must not wait out.
  job <- new.env(parent = emptyenv())
  running <- shiny::reactiveVal(FALSE)
  tick <- shiny::reactiveVal(0L)
  # The engine's warnings and messages are kept with the result and shown
  # in the note under it: DMAR's categorical omega says by message() what
  # it estimated and what it set aside (its coding note, an undetermined
  # item pair), so a message is part of what the coefficient means.
  finish_ok <- function(result, warnings, messages) {
    request <- job$request
    items <- job$items
    aux <- job$aux
    rng <- job$rng
    code <- c(sprintf("ordinal_mi_data <- items[, %s, drop = FALSE]", sieve_r_literal(items)),
      vapply(aux, function(name) sprintf("ordinal_mi_data[[%s]] <- d[[%s]]", sieve_r_literal(name), sieve_r_literal(name)), character(1)),
      sieve_numeric_code("ordinal_mi_data"),
      sprintf(paste0("ordinal_mi_omega <- DMAR::reliability_omega_categorical(ordinal_mi_data, ",
        "missing = \"mi\", aux = %s, ci_method = \"percentile\", conf_level = %s, B = %d, ",
        "imputations = %d, seed = %d, denominator = %s)"),
        sieve_r_literal(aux), sieve_r_literal(request$conf_level), request$B,
        request$imputations, rng$seed, sieve_r_literal(request$denominator)))
    sieve_log(state, paste("Categorical omega under MAR:", request$construct), code)
    spec <- c(list(kind = "categorical_omega_mi", items = items, aux = aux), request)
    spec$seed <- rng$seed
    spec$key <- sieve_hash(spec)
    limits <- c(sieve_reliability_value(result, "ci_lower"), sieve_reliability_value(result, "ci_upper"))
    mi <- attr(result, "imputation", exact = TRUE)
    partial <- any(!is.finite(limits)) || isTRUE(mi$imputations_returned < mi$imputations_point)
    sieve_register_analysis(state, list(spec = spec, status = list(code = if (partial) "partial" else "ok",
      message = if (partial) "Some requested limits or completed-data estimates are unavailable; read the imputation diagnostics."),
      estimates = result, n_fitted = sieve_reliability_value(result, "N"),
      engine_versions = c(sieve_reliability_versions(), list(mice = as.character(utils::packageVersion("mice"))))), note = "Categorical omega under MAR")
    held(list(key = job$stamp, request = request,
      run = list(ok = TRUE, value = list(result = result, rng = rng), warnings = as.character(warnings),
        messages = as.character(messages))))
  }
  finish_other <- function(message) {
    held(list(key = job$stamp, request = job$request, run = list(ok = FALSE, error = message, warnings = character(0))))
  }
  shiny::observeEvent(input$ordinal_mi_go, {
    if (isTRUE(running())) return(invisible(NULL))
    request <- settings()
    stamp <- key()
    previous <- held()
    if (!is.null(previous) && identical(previous$key, stamp) && isTRUE(previous$run$ok)) return(invisible(NULL))
    prep <- sieve_capture({
      if (!isTRUE(sieve_confirmation_status(state)$confirmed)) stop("Load data and confirm item types in Ingest before this analysis.", call. = FALSE)
      problem <- sieve_sampling_problem(state)
      if (!is.null(problem)) stop(problem, call. = FALSE)
      if (length(request$construct) != 1L || !request$construct %in% names(groups())) stop("Choose a construct of at least three included ordered or binary items.", call. = FALSE)
      items <- groups()[[request$construct]]
      if (length(items) < 3L) stop("Choose a construct of at least three included ordered or binary items.", call. = FALSE)
      raw <- shiny::isolate(state$raw)
      data <- shiny::isolate(state$items)
      roles <- shiny::isolate(sieve_roles_table(state$raw, state$items, state$roles))
      aux <- setdiff(roles$variable[roles$role == "auxiliary"], items)
      if (!length(aux)) aux <- NULL
      for (name in aux) data[[name]] <- raw[[name]]
      rng <- sieve_draw_seed(request$seed)
      call <- sieve_ordinal_omega_mi_call(data, items, aux = aux, B = request$B,
        imputations = request$imputations, seed = rng$seed,
        conf_level = request$conf_level, denominator = request$denominator)
      handle <- sieve_ordinal_omega_mi_start(call)
      list(handle = handle, items = items, aux = aux, rng = rng)
    })
    if (!isTRUE(prep$ok)) {
      held(list(key = stamp, request = request, run = prep))
      return(invisible(NULL))
    }
    job$handle <- prep$value$handle
    job$items <- prep$value$items
    job$aux <- prep$value$aux
    job$rng <- prep$value$rng
    job$request <- request
    job$stamp <- stamp
    job$started <- proc.time()[["elapsed"]]
    job$stop_at <- NA_real_
    held(NULL)
    running(TRUE)
  })
  shiny::observeEvent(input$ordinal_mi_stop, {
    if (isTRUE(running()) && !is.null(job$handle) && is.na(job$stop_at)) {
      job$handle$stop()
      job$stop_at <- proc.time()[["elapsed"]]
      tick(shiny::isolate(tick()) + 1L)
    }
  })
  shiny::observe({
    if (!isTRUE(running())) return(invisible(NULL))
    shiny::invalidateLater(500)
    handle <- job$handle
    alive <- handle$poll()
    if (alive && !is.na(job$stop_at) && proc.time()[["elapsed"]] - job$stop_at > sieve_dmar_boot_grace_seconds()) {
      handle$kill()
      alive <- FALSE
    }
    shiny::isolate(tick(tick() + 1L))
    if (alive) return(invisible(NULL))
    run <- handle$results[[1L]]
    failure <- handle$failure()
    handle$kill()
    job$handle <- NULL
    running(FALSE)
    stopped <- !is.na(job$stop_at)
    if (is.null(run)) {
      finish_other(if (stopped) {
        "Stopped: the run was canceled before DMAR returned; nothing was computed or recorded."
      } else {
        failure %||% "The background R process ended without a result; nothing was recorded."
      })
    } else if (identical(sieve_dmar_boot_outcome(run), "cancelled")) {
      finish_other(sprintf("Stopped: canceled after %s of %s bootstrap replications; nothing was computed or recorded.",
        format(run$condition$done %||% NA_integer_), format(run$condition$total %||% job$request$B)))
    } else if (!isTRUE(run$ok)) {
      finish_other(sieve_dmar_boot_failure_text(run))
    } else {
      finish_ok(run$value, run$warnings, run$messages)
    }
  })
  session$onSessionEnded(function() {
    if (!is.null(job$handle)) try(job$handle$kill(), silent = TRUE)
  })
  # As in sieve_mi_server(): the line and its Cancel button are static
  # (sieve_mi_progress_ui()) and only the sentence is redrawn. The button
  # is withdrawn once Cancel is pressed, while the line says the run is
  # being canceled.
  output$ordinal_mi_running <- shiny::renderText(if (isTRUE(running())) "true" else "false")
  shiny::outputOptions(output, "ordinal_mi_running", suspendWhenHidden = FALSE)
  output$ordinal_mi_cancelable <- shiny::renderText({
    tick()
    if (isTRUE(running()) && is.na(job$stop_at %||% NA_real_)) "true" else "false"
  })
  shiny::outputOptions(output, "ordinal_mi_cancelable", suspendWhenHidden = FALSE)
  output$ordinal_mi_progress <- shiny::renderText({
    tick()
    if (!isTRUE(running()) || is.null(job$handle)) return(NULL)
    pr <- job$handle$progress
    done <- suppressWarnings(as.integer(pr$done))
    total <- suppressWarnings(as.integer(pr$total))
    if (length(total) != 1L || is.na(total)) total <- job$request$B
    elapsed <- proc.time()[["elapsed"]] - job$started
    status <- if (!is.na(job$stop_at)) {
      "Canceling: DMAR stops at its next replication."
    } else if (length(done) == 1L && !is.na(done) && done > 0L) {
      sprintf("Running in a background R process: %d of %d bootstrap replications (each averages %d imputations); %s elapsed, about %s remaining.",
        done, total, job$request$imputations, sieve_duration_label(elapsed),
        sieve_duration_label(elapsed / done * max(total - done, 0L)))
    } else {
      sprintf("Running in a background R process: the point estimate's imputations, then %d bootstrap replications of %d imputations each; %s elapsed.",
        total, job$request$imputations, sieve_duration_label(elapsed))
    }
    status
  })
  current <- shiny::reactive({
    value <- held()
    if (is.null(value) || !identical(value$key, key()) || !isTRUE(value$run$ok)) return(NULL)
    value$run$value$result
  })
  output$ordinal_mi_note <- shiny::renderUI({
    value <- held()
    text <- if (isTRUE(running())) {
      NULL
    } else if (is.null(value)) {
      if (!length(groups())) "No construct has three included ordered or binary items. Declare the item types and constructs in Ingest." else "MI omega and its bootstrap-then-impute interval have not been requested."
    } else if (!identical(value$key, key())) {
      "The data, included items, roles or MI settings changed. Compute Categorical MI Omega and Interval again for the current request."
    } else if (!value$run$ok) value$run$error else {
      result <- value$run$value$result
      imputation <- attr(result, "imputation", exact = TRUE)
      paste(sprintf("Seed: %d. %d bootstrap replications; %d imputations per replication.",
        value$run$value$rng$seed, value$request$B, value$request$imputations),
        if (length(imputation$imputations_point)) sprintf("Point estimate: %d of %d imputations returned a value.", imputation$imputations_returned, imputation$imputations_point),
        paste(value$run$warnings, collapse = " "),
        if (length(value$run$messages)) sprintf("The engine noted: %s.",
          paste(sprintf("\"%s\"", gsub("\\s+", " ", value$run$messages)), collapse = "; ")))
    }
    if (is.null(text)) return(NULL)
    shiny::p(class = "small text-muted", text)
  })
  output$ordinal_mi_table <- DT::renderDT({
    result <- current()
    shiny::req(result)
    terms <- c(estimate = "estimate", se = "se", lower_limit = "ci_lower", upper_limit = "ci_upper",
      conf_level = "conf_level", N = "N", N_complete = "N_complete")
    sieve_dmar_require_terms(result, unname(terms), "Categorical MI omega")
    tab <- as.data.frame(stats::setNames(lapply(unname(terms), function(term) sieve_reliability_value(result, term)), names(terms)))
    tab$method <- attr(result, "ci_method", exact = TRUE)
    sieve_dt(tab, rownames = FALSE, options = list(scrollX = TRUE, dom = "t")) |>
      DT::formatRound(c("estimate", "se", "lower_limit", "upper_limit", "conf_level"), 3)
  })
  output$ordinal_mi_diagnostics <- shiny::renderText({
    result <- current()
    shiny::req(result)
    paste(utils::capture.output(print(result)), collapse = "\n")
  })
  current
}

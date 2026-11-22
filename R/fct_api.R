# Public headless entry points use the same services as the application.

#' Run an Analysis Without Opening the Application
#'
#' Dispatches to the same named analysis services used by the interface and
#' retains the request, engine versions, and returned values. It does not
#' select a model or substitute another method when a request is refused.
#'
#' @param analysis Analysis service. See Details.
#' @param ... Named arguments for the selected service.
#' @param design Sampling design, with `unit` equal to `"independent"`,
#'   `"clustered"`, or `"repeated"`. Dependent designs require a service
#'   that explicitly supports their inference.
#' @details
#' The service names and argument contracts are:
#' * `cfa`: the arguments of [sieve_fit_cfa()]; the model is fit through the
#'   structured service, so a fit that fails keeps its status and reason.
#' * `efa`: [sieve_efa_fit_by_k()].
#' * `reliability`: [sieve_reliability_construct()]; its `workers` and
#'   `progress` arguments, which do not change the result, are left out of
#'   `arguments_hash`.
#' * `retention`: [sieve_retention_evidence()].
#' * `screening`: [sieve_response_screens()].
#' * `sem`: [sieve_run_structural()].
#' * `bifactor`: [sieve_fit_bifactor()].
#' * `invariance`: [sieve_fit_invariance()].
#' * `cross_validation`: [sieve_cross_validate()].
#' * `longitudinal`: [sieve_run_longitudinal()].
#' * `factor_scores`: [sieve_factor_score_record()].
#' * `norms`: [sieve_construct_norms()].
#' * `irt`, `htmt`, `ave`, and `content`: respectively [DMAR::irt_grm()],
#'   [DMAR::htmt()], [DMAR::average_variance_extracted()], and
#'   [DMAR::content_validity_index()].
#'
#' This convenience dispatcher accepts independent sampling units. For supported
#' clustered CFA or SEM, use a resolved specification with a cluster field and
#' call [sieve_run_model()] directly. Dependent requests are refused by this
#' dispatcher. Calling an engine directly has that engine's assumptions.
#' No respondent data or credentials are transmitted by this function.
#' @section Interval Descriptor:
#' `intervals` holds every confidence interval the result holds, in one
#' data frame filled when the object is built, and is what [confint()],
#' [summary()], and [print()] read. It has one row per interval of every
#' table that holds limits (the tables `confint()` reads, listed in
#' [print.siever_analysis()]), in the columns:
#' * `table`: the interval table, as `confint(x, table = )` names it.
#' * `term`: the row, as `rownames(confint(x))` names it.
#' * `estimate`, `se`: the point estimate and its standard error (`NA`
#'   where the result keeps none).
#' * `lower`, `upper`: the stored limits, never rounded (`NA` for the
#'   absent limit of a one-sided bound).
#' * `conf_level`: the level the limits were formed at, read from where
#'   the result records it (a level column, DMAR's metadata, or the result
#'   element that holds it).
#' * `side`: `"two-sided"`, or `"upper"` or `"lower"` for a one-sided
#'   bound (HTMT's upper bound).
#' * `method`: the interval method, in words.
#' * `kind`: the type of interval that sets what `confint()` may do at
#'   another level: `"wald"` (formed again from `estimate` and `se`),
#'   `"bootstrap"`, `"lavaan_bootstrap"`, `"profile"`, `"order_statistic"`,
#'   `"exact_binomial"`, `"noncentral"`, `"wald_without_se"`,
#'   `"one_sided"`, or `"other"`.
#' * `scale`, `df`, `transformation`: for a Wald interval, the scale it was
#'   formed on (`"identity"`, `"unit"`, `"logit"`, `"log_complement"`), its
#'   t reference degrees of freedom (`Inf` for the normal), and the
#'   recorded transformation in words.
#' * `n_used`: the number of cases the estimate uses: the respondents in
#'   the parameter's group of a lavaan fit (every group for a defined
#'   parameter), the rows a coefficient, half, norm, or fit used, the
#'   experts who rated an item for content validity, and for HTMT the rows
#'   observed on at least two of the pair's items (its correlations are
#'   pairwise complete); `NA` for a respondent's factor score interval,
#'   which is conditional on that respondent's responses and rests on no
#'   sample, and where the result does not record it.
#' * `B_used`: the bootstrap replications used, for a bootstrap interval.
#' * `recomputed`: `FALSE`: the limits are those the service stored.
#'   `confint()` marks limits it forms at another level in its `source` and
#'   `recomputed` attributes.
#' * `reason`: why a row has no interval (a fixed parameter, a score
#'   without score error, an AVE or HTMT run without a bootstrap, with the
#'   call that forms one at the level the run asked for and the
#'   replications DMAR needs there), `NA` when it has one.
#' * `rerun`: how to obtain limits at another level, for a type
#'   `confint()` does not form again, with `<level>` standing for the level
#'   and `<min_B>` for the fewest bootstrap replications DMAR forms a limit
#'   from at that level: 10 / ((1 - level) / 2) for a two-sided percentile
#'   interval (200 at 0.90, 400 at 0.95), 10 / min(level, 1 - level) for a
#'   one-sided bound (200 at 0.95, 1000 at 0.99), and never fewer than 100. `confint()` fills both in
#'   when it refuses a level. For HTMT's stored bound, the refusal itself,
#'   which names the column that holds the bound.
#'
#' A table that holds no limits that can be read (a level the result does
#' not record, two places that disagree, a model without a solution) has
#' one row with an empty `term` and the `reason`; an analysis with no
#' table of limits (screening, a graded response model, a failed analysis)
#' has one row with an empty `table` and `term` and the reason. Building
#' the descriptor never stops an analysis.
#' @return A `siever_analysis` object containing `schema_version` (the
#'   version of this envelope, 1), `analysis`, `service`,
#'   `arguments_hash`, `design`, `versions`, immutable executing
#'   `engine_identity`, `result`, `status` (a list with
#'   `code` and `message`), `diagnostics`, and `intervals`, the interval
#'   descriptor (see Interval Descriptor). For `cfa`, the status is the
#'   fit's own, with the reason when the fit failed, and `diagnostics` holds
#'   the fit's error and warnings. For a result that carries its own status
#'   (`sem`, `bifactor`, `longitudinal`), the status is that one, and an
#'   `ave` read from a fit has the fit's status. For the other services it
#'   is read from what the service returned: `"failed"` when nothing usable
#'   was returned (a refusal with its `error`, no factor scores, no model
#'   with a solution, every coefficient, method, or construct failed);
#'   `"partial"` when some of what was requested is not available (a model
#'   without a solution beside others, a requested interval that failed or
#'   was unavailable, a
#'   correlation basis replaced by another, ML scores that do not exist for
#'   some respondents, a screen that could not be computed);
#'   `"inadmissible"` when every model has a solution and some are improper
#'   (a Heywood case), as for a `cfa`; and `"ok"` otherwise. The message
#'   gives every reason on one line, and `diagnostics$components` lists each
#'   coefficient, interval, model, level, half, method, or construct with its
#'   own `code` and `message`. The service's estimates and statuses retain
#'   their precision and structure. For `irt`, `htmt`, `ave`, and
#'   `content`, a refusal from DMAR (an error of class `dmar_refusal`, such
#'   as a continuous item given to the graded response model, a construct
#'   block of one column, or a bootstrap count below DMAR's minimum) is
#'   returned as a failed analysis, as an exploratory refusal is: no
#'   result, DMAR's reason as the status message, and the refusal's class,
#'   code, and message in `diagnostics$refusal`. An error that is not a
#'   refusal (an argument the engine does not take) is signaled.
#' @author Ken Kelley
#' @examples
#' x <- data.frame(a = c(1, 1, 2), b = c(1, 2, 2), c = c(1, 3, 2))
#' result <- sieve_analyze("screening", data = x, min_run = 3L)
#' summary(result)
#' @export
sieve_analyze <- function(analysis, ..., design = list(unit = "independent")) {
  services <- sieve_analysis_services()
  analysis <- match.arg(analysis, names(services))
  unit <- design$unit %||% "independent"
  if (!unit %in% c("independent", "clustered", "repeated")) {
    stop("Declare an independent, clustered, or repeated sampling unit.", call. = FALSE)
  }
  args <- list(...)
  if (identical(analysis, "cross_validation")) args$design <- design
  if (!identical(unit, "independent")) {
    # The direct fit wrappers take no design declaration. Require the resolved
    # specification for cluster inference rather than silently treating rows
    # as independent. The public model API supports it through sieve_run_model.
    stop("Use a design-aware model specification and sieve_run_model() for dependent-design inference; this convenience dispatcher supports independent units.",
      call. = FALSE
    )
  }
  package <- if (analysis %in% c("irt", "htmt", "ave", "content")) "DMAR" else "SIEVER"
  fun <- if (identical(package, "DMAR")) getExportedValue(package, services[[analysis]])
  else get(services[[analysis]], envir = asNamespace("SIEVER"), inherits = FALSE)
  diagnostics <- NULL
  if (identical(analysis, "cfa")) {
    # sieve_fit_cfa() returns NULL when the fit fails and keeps no reason;
    # the structured service it wraps keeps the status, the error, and the
    # engine's warnings, and returns the identical fit when it succeeds.
    run <- do.call(sieve_analyze_cfa, args)
    value <- run$fit
    status <- list(code = run$status$code %||% "failed", message = run$status$message %||% run$error %||% "")
    diagnostics <- list(error = run$error, warnings = run$warnings, problems = run$spec$problems)
  } else {
    # DMAR refuses a fit without a solution before returning an AVE table.
    # Keep the dispatcher's documented fit status instead of losing it to
    # that engine error; there are no estimates to extract from this fit.
    no_ave_solution <- identical(analysis, "ave") && inherits(args$fit, "lavaan") &&
      !sieve_has_solution(args$fit)
    # sieve_efa_fit_by_k() refuses an item lavaan cannot fit (no observed
    # values, or one value throughout) by name before any model is fit. The
    # dispatcher returns that refusal as a failed analysis, with the reason
    # for each number of factors requested, as it returns every other
    # service's failure.
    # A DMAR engine (irt, htmt, ave, content) refuses a request it cannot
    # answer with an error of class dmar_refusal that says why (an item that
    # is not ordered, a block of one column, a bootstrap count below DMAR's
    # minimum). The dispatcher returns that refusal as a failed analysis with
    # DMAR's reason, as it returns the exploratory refusal above, rather
    # than stopping the script. An error that is not a refusal (an argument
    # the engine does not take) is still signaled.
    # Each branch returns what it caught as part of its value, so nothing
    # is assigned outside the handler's own frame.
    outcome <- if (no_ave_solution) {
      list(value = NULL)
    } else if (identical(analysis, "efa")) {
      tryCatch(list(value = do.call(fun, args)),
        sieve_unusable_items = function(e) list(value = NULL, refused = conditionMessage(e)))
    } else if (identical(package, "DMAR")) {
      tryCatch(list(value = do.call(fun, args)),
        dmar_refusal = function(e) list(value = NULL, dmar_refusal = e))
    } else {
      list(value = do.call(fun, args))
    }
    value <- outcome$value
    refused <- outcome$refused
    dmar_refusal <- outcome$dmar_refusal
    # Each service states failure in its own way: read it from the service's
    # contract rather than calling every returned value a success.
    resolved <- if (!is.null(refused)) {
      ks <- if (!is.null(args$ks)) {
        sort(unique(as.integer(args$ks)))
      } else {
        seq_len(min(as.integer(args$max_k %||% 8L), sieve_efa_max_factors(ncol(args$data))))
      }
      list(code = "failed", message = refused, components = data.frame(
        component = paste("k =", ks), code = rep("failed", length(ks)),
        message = rep(refused, length(ks)), stringsAsFactors = FALSE
      ))
    } else if (!is.null(dmar_refusal)) {
      why <- trimws(gsub("\\s+", " ", conditionMessage(dmar_refusal)))
      list(code = "failed", message = why, components = data.frame(
        component = paste0("DMAR::", services[[analysis]], "()"), code = "failed",
        message = why, stringsAsFactors = FALSE
      ))
    } else {
      sieve_analysis_status(analysis, value, args)
    }
    status <- list(code = resolved$code, message = resolved$message)
    if (!is.null(resolved$components)) diagnostics <- list(components = resolved$components)
    if (!is.null(dmar_refusal)) {
      diagnostics$refusal <- list(
        class = class(dmar_refusal), code = dmar_refusal$code %||% class(dmar_refusal)[1L],
        message = conditionMessage(dmar_refusal)
      )
    }
  }
  # How a reliability bootstrap is run (its processes and its progress
  # reporter) does not change its result, so it is not part of the identity
  # of the request.
  hashed <- if (identical(analysis, "reliability")) args[setdiff(names(args), c("workers", "progress"))] else args
  object <- structure(list(
    schema_version = sieve_analysis_schema_version(),
    analysis = analysis, service = paste0(package, "::", services[[analysis]]),
    arguments_hash = sieve_hash(hashed), design = design,
    versions = lapply(c("SIEVER", "DMAR", "lavaan", "psych"), function(p) {
      list(package = p, version = as.character(utils::packageVersion(p)))
    }), engine_identity = sieve_engine_identity(),
    result = value, status = status, diagnostics = diagnostics
  ), class = "siever_analysis")
  # The interval descriptor of the result envelope: every interval the
  # result holds, read once here, so that confint(), summary() and print()
  # read one table rather than each rediscovering limits and levels in the
  # result.
  object$intervals <- sieve_analysis_intervals(object, args)
  object
}

#' The services of the headless dispatcher
#'
#' @return A named character vector, analysis name to the function that
#'   runs it (SIEVER's own, or DMAR's for `irt`, `htmt`, `ave`, and
#'   `content`).
#' @author Ken Kelley
#' @noRd
sieve_analysis_services <- function() {
  c(
    cfa = "sieve_fit_cfa", efa = "sieve_efa_fit_by_k",
    reliability = "sieve_reliability_construct", retention = "sieve_retention_evidence",
    screening = "sieve_response_screens", sem = "sieve_run_structural",
    bifactor = "sieve_fit_bifactor", invariance = "sieve_fit_invariance",
    cross_validation = "sieve_cross_validate",
    longitudinal = "sieve_run_longitudinal", factor_scores = "sieve_factor_score_record",
    norms = "sieve_construct_norms", irt = "irt_grm", htmt = "htmt",
    ave = "average_variance_extracted", content = "content_validity_index"
  )
}

#' The version of the `siever_analysis` envelope
#'
#' Version 1: the fields `schema_version`, `analysis`, `service`,
#' `arguments_hash`, `design`, `versions`, `engine_identity`, `result`,
#' `status`, `diagnostics`, and the interval descriptor `intervals`. A
#' change to a field's meaning, or the removal of a field, raises the
#' number.
#'
#' @return A single integer.
#' @author Ken Kelley
#' @noRd
sieve_analysis_schema_version <- function() 1L

#' The status of a headless analysis, read from its service's contract
#'
#' A service that returns a value has not necessarily succeeded: cross
#' validation and invariance return a list with an `error` when they refuse
#' the data; a reliability analysis keeps a status for each coefficient and
#' its interval; an exploratory fit, a retention analysis, the invariance
#' ladder, and the halves of a cross validation keep one for each model,
#' method, level, or half; a factor score record without an admissible
#' model has no scores; a norm table lists the constructs it refused; and
#' a screen says which screen it could not compute.
#'
#' A component has a solution when its code is `"ok"` or `"inadmissible"`
#' (an improper solution, reported with its status, as
#' `sieve_has_solution()` reads it). The status is `"failed"` when nothing
#' usable was returned; `"partial"` when some of what was requested is not
#' available (a model without a solution, an interval that failed, a
#' correlation basis replaced by another, scores that could not be
#' computed for some respondents); `"inadmissible"` when every model has a
#' solution and some are improper (a Heywood case at an over-extracted
#' number of factors, for one); and `"ok"` otherwise. A result with a status
#' of its own (a structural, bifactor, or longitudinal model) keeps it, and
#' an average variance extracted from a fit takes the fit's status. The
#' reasons are kept, one row per component.
#'
#' @param analysis The service name, as [sieve_analyze()] takes it.
#' @param value The service's result.
#' @param args The service's arguments (for a status read from an input).
#' @return A list with `code`, `message` (the reasons of the components
#'   that are not ok, or the service's own reason), and `components` (a data
#'   frame with `component`, `code`, and `message`, or `NULL` when the
#'   service has no components).
#' @author Ken Kelley
#' @noRd
sieve_analysis_status <- function(analysis, value, args = list()) {
  out <- function(code, message = "", components = NULL) {
    list(code = code, message = message, components = components)
  }
  # Engine text carries line breaks and indentation; one line, one space.
  clean <- function(x) trimws(gsub("\\s+", " ", x))
  text <- function(x, otherwise = "") {
    x <- clean(as.character(unlist(x %||% character(0))))
    x <- paste(x[!is.na(x) & nzchar(x)], collapse = " ")
    if (nzchar(x)) x else otherwise
  }
  part <- function(component, code, message) {
    data.frame(component = component, code = code, message = clean(ifelse(is.na(message), "", message)), stringsAsFactors = FALSE)
  }
  solved <- function(code) code %in% c("ok", "inadmissible")
  # Every component ok is ok. Every one with a solution, some improper, is
  # inadmissible. Some without a solution (or anything requested and not
  # available) is partial while something is usable, and failed otherwise.
  combine <- function(parts, usable, note = "") {
    if (is.null(parts) || !nrow(parts)) {
      return(out(if (usable) "ok" else "failed", if (usable) note else "The service returned no usable result."))
    }
    bad <- parts$code != "ok"
    if (!any(bad)) {
      return(out("ok", note, parts))
    }
    why <- paste(sprintf(
      "%s: %s.", parts$component[bad],
      sub("[.]$", "", ifelse(nzchar(parts$message[bad]), parts$message[bad], parts$code[bad]))
    ), collapse = " ")
    if (nzchar(note)) why <- paste(why, note)
    code <- if (!usable && !any(solved(parts$code))) {
      "failed"
    } else if (all(solved(parts$code))) {
      "inadmissible"
    } else {
      "partial"
    }
    out(code, why, parts)
  }
  # A fit's refusal is authoritative even when extraction never ran and
  # consequently produced no table.
  if (identical(analysis, "ave") && inherits(args$fit, "lavaan")) {
    st <- sieve_fit_status(args$fit)
    if (!identical(st$code, "ok")) {
      return(out(st$code, clean(paste("The fit the AVE is read from:", st$message %||% ""))))
    }
  }
  if (is.null(value)) {
    return(out("failed", "The service returned no result."))
  }
  listed <- is.list(value) && !is.data.frame(value)
  # A result with a status of its own (structural, bifactor, longitudinal).
  if (listed && is.list(value$status) && is.character(value$status$code) && length(value$status$code) == 1L) {
    return(out(value$status$code, clean(value$status$message %||% "")))
  }
  # A refusal before anything was estimated (cross validation, invariance).
  if (listed && nzchar(text(value$error))) {
    return(out("failed", text(value$error)))
  }
  finite <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  switch(analysis,
    reliability = {
      records <- list(
        Omega = value$omega, "Model-implied omega" = value$model_implied,
        H = value$H, Alpha = value$alpha
      )
      records <- Filter(function(x) is.list(x) && length(x), records)
      parts <- do.call(rbind, lapply(names(records), function(nm) {
        x <- records[[nm]]
        label <- x$label %||% nm
        code <- if (is.list(x$status) && is.character(x$status$code)) x$status$code else "ok"
        if (solved(code) && !finite(x$estimate)) code <- "failed"
        rows <- part(label, code, text(x$status$message %||% x$error))
        # A requested interval that could not be computed is part of what
        # was requested (an interval not requested, or still running, is not).
        u <- x$uncertainty
        if (finite(x$estimate) && is.list(u) && isTRUE(u$status %in% c("failed", "unavailable"))) {
          rows <- rbind(rows, part(paste(label, "interval"), u$status, text(u$reason %||% u$message, "The interval could not be computed.")))
        }
        rows
      }))
      combine(parts, any(vapply(records, function(x) finite(x$estimate), logical(1))))
    },
    cross_validation = ,
    invariance = {
      levels <- Filter(function(x) is.list(x) && is.character(x$code), if (is.list(value$status)) value$status else list())
      parts <- if (length(levels)) {
        do.call(rbind, lapply(names(levels), function(nm) part(nm, levels[[nm]]$code, text(levels[[nm]]$message))))
      }
      # A construct whose split coefficient is unavailable in a half.
      rel <- if (identical(analysis, "cross_validation")) value$rel else NULL
      if (is.data.frame(rel) && all(c("Construct", "Calibration", "Validation") %in% names(rel))) {
        for (cn in setdiff(unique(as.character(rel$Construct)), "Unassigned")) {
          r <- rel[rel$Construct == cn, , drop = FALSE]
          if (all(is.na(r$Calibration)) || all(is.na(r$Validation))) {
            why <- if ("Calibration.residual" %in% names(r)) text(r$Calibration.residual[1L]) else ""
            parts <- rbind(parts, part(sprintf("split reliability of %s", cn), "unavailable", text(why, "no coefficient in one half or both")))
          }
        }
      }
      usable <- if (identical(analysis, "invariance")) !is.null(value$mi) else !is.null(value$table)
      combine(parts, usable && (is.null(parts) || any(solved(parts$code))))
    },
    efa = {
      if (!is.data.frame(value) || !all(c("k", "status") %in% names(value))) {
        return(out("ok"))
      }
      if (!nrow(value)) {
        return(out("failed", "No number of factors was fitted: none was requested, or an exploratory model of these items identifies none."))
      }
      why <- if ("message" %in% names(value)) as.character(value$message) else rep("", nrow(value))
      parts <- part(paste("k =", value$k), as.character(value$status), why)
      combine(parts, any(solved(parts$code)))
    },
    retention = {
      if (is.list(value$basis) && isFALSE(value$basis$ok)) {
        return(out("failed", text(value$basis$reason, "The correlation basis could not be computed.")))
      }
      methods <- Filter(
        function(x) is.list(x) && is.logical(x$ok) && !identical(x$status, "not run"),
        value[intersect(c("pa", "vss", "fit"), names(value))]
      )
      parts <- if (length(methods)) {
        do.call(rbind, lapply(names(methods), function(nm) {
          x <- methods[[nm]]
          part(nm, if (isTRUE(x$ok)) "ok" else as.character(x$status %||% "failed"), text(x$reason))
        }))
      }
      # The basis requested and replaced by another is said.
      if (is.list(value$basis) && isTRUE(value$basis$changed)) {
        parts <- rbind(parts, part("correlation basis", "substituted", text(value$basis$reason, sprintf(
          "The %s correlations requested were replaced by %s correlations.", value$basis$requested %||% "requested", value$basis$used %||% "other"
        ))))
      }
      combine(parts, any(vapply(methods, function(x) isTRUE(x$ok), logical(1))))
    },
    factor_scores = {
      if (is.null(value$scores)) {
        return(out("failed", text(value$reason, "No scores were returned.")))
      }
      if (!any(is.finite(as.matrix(value$scores)))) {
        return(out("failed", "No respondent has a finite score."))
      }
      reasons <- c(
        if (finite(value$extreme_patterns) && value$extreme_patterns > 0) {
          sprintf(
            "%d %s no finite ML score (a uniform lowest or highest response pattern; the EBM posterior mode is finite).",
            as.integer(value$extreme_patterns), if (value$extreme_patterns == 1) "respondent has" else "respondents have"
          )
        },
        text(value$reason)
      )
      reasons <- reasons[nzchar(reasons)]
      if (length(reasons)) out("partial", paste(reasons, collapse = " ")) else out("ok")
    },
    norms = {
      # A policy repaired on reading is scored as repaired and said.
      review <- value$review
      note <- if (length(review)) {
        paste("Needs review:", paste(sprintf("%s: %s", names(review), clean(review)), collapse = " "))
      } else ""
      refused <- value$refused
      if (!length(refused)) {
        if (!length(value$T)) {
          return(out("failed", "No construct has included items to score."))
        }
        return(out("ok", note))
      }
      parts <- part(names(refused) %||% paste("construct", seq_along(refused)), "failed", as.character(refused))
      combine(parts, length(value$T) > 0L, note)
    },
    screening = {
      if (finite(value$n_resp) && value$n_resp == 0) {
        return(out("failed", "There are no respondents to screen."))
      }
      if (nzchar(text(value$unavailable))) out("partial", text(value$unavailable)) else out("ok")
    },
    ave = {
      if (!is.data.frame(value) || !"ave" %in% names(value) || !any(is.finite(value$ave))) {
        return(out("failed", "No finite factor AVE was returned."))
      }
      B <- attr(value, "B", exact = TRUE)
      requested <- identical(args$ci_method, "percentile") ||
        (is.numeric(B) && length(B) == 1L && is.finite(B) && B > 0)
      if (requested && (!all(c("ci_lower", "ci_upper") %in% names(value)) ||
          any(!is.finite(value$ci_lower) | !is.finite(value$ci_upper)))) {
        return(out("partial", paste(
          "Factor AVE point estimates are retained, but one or more requested percentile intervals were withheld.",
          "Read the recorded engine warnings and bootstrap counts before interpreting the available limits."
        )))
      }
      out("ok")
    },
    out("ok")
  )
}

#' The CFA of the public dispatcher, through the structured service
#'
#' Takes the arguments of [sieve_fit_cfa()] and returns the `sieve_result`
#' of `sieve_run_cfa()`, whose fit is the one `sieve_fit_cfa()` returns,
#' with the status and reason that wrapper drops; a ledger that defines no
#' model gives a failed result that says so.
#'
#' @inheritParams sieve_fit_cfa
#' @return A `sieve_result`-shaped list with `spec`, `fit`, `status`,
#'   `warnings`, and `error`.
#' @author Ken Kelley
#' @noRd
sieve_analyze_cfa <- function(items_df, meta, syntax = NULL, std_lv = TRUE, orthogonal = FALSE,
                              estimator = "auto", store = NULL, page = NULL) {
  spec <- sieve_cfa_spec(meta,
    syntax = syntax, std_lv = std_lv, orthogonal = orthogonal,
    reserved = names(items_df), estimator = estimator
  )
  if (!nzchar(spec$model %||% "")) {
    included <- meta$status %in% "active"
    why <- paste(c(if (any(included) && !any(included & sieve_construct_assigned(meta$construct))) paste(
      "No included item is assigned to a construct (each is blank or Unassigned), so there is no confirmatory",
      "model to fit. Assign items to constructs (the construct column of the specification, or the Construct",
      "cell on Ingest) before fitting."
    ) else "The item definitions specify no model to fit.", spec$problems), collapse = " ")
    return(list(
      spec = spec, fit = NULL, status = list(code = "failed", message = why),
      warnings = character(0), error = why
    ))
  }
  sieve_run_cfa(spec, items_df, store = store, page = page)
}

#' Inspect a Recorded Headless Analysis
#'
#' @description
#' The methods for a result of [sieve_analyze()]. `print()` says first
#' whether the analysis completed (and, when it did not, why), then shows
#' its tables, its design and the versions of the software it ran on;
#' `summary()` returns the same pieces in a list; `as.data.frame()` returns
#' one stored table; and `confint()` returns the stored intervals. Nothing
#' is refit: an interval at another level is computed only where its type
#' allows it, and refused with the reason otherwise.
#'
#' @param x,object A `siever_analysis` object.
#' @param ... Additional generic arguments.
#' @param table For `as.data.frame()`, the name or number of one table
#'   that `summary()` lists. For `confint()`, the table of limits: `NULL`
#'   (the default) reads the analysis's interval table (see Confidence
#'   Limits), a name reads another table with limits, and a number reads
#'   the table `summary()` lists at that position. A table without limits
#'   is refused, naming the tables that have them.
#' @param row.names,optional Arguments passed to [base::as.data.frame()].
#' @param parm Rows of the interval table, by name (as
#'   `rownames(confint(x))` gives them) or by position among the rows with
#'   limits. An unknown name is refused with the available names, and a
#'   row that has no limits (a fixed parameter, a score without score
#'   error) is refused with the reason.
#' @param level The confidence level. `NULL` (the default) returns the
#'   stored limits, each at the level it was stored at. Another level, one
#'   number strictly between 0 and 1, is computed only for a Wald interval
#'   and refused for every other type (see Confidence Limits).
#' @section Confidence Limits:
#' `confint()` follows one rule, by the type of interval.
#'
#' * With `level = NULL` it returns the stored limits of every interval, at
#'   the level each was stored at, whatever its method.
#' * Another level is computed only for a Wald interval, from the stored
#'   estimate and standard error, on the scale the interval was formed on,
#'   with the same reference distribution (normal, or t when the interval
#'   used one) and the same back-transformation: lavaan parameters fitted
#'   with standard or robust standard errors; a reliability coefficient
#'   whose interval is ML, MLR (sandwich) SE, or ADF on the coefficient's own
#'   scale (limits clamped to the unit interval, as DMAR clamps them), on
#'   the logit scale (the `_logistic` methods, the bootstrap standard error
#'   on the logit scale, and H), or on Bonett's (2002) ln(1 - alpha) scale;
#'   and a factor score's normal reference interval (the conditional Wald
#'   interval, or the normal-model posterior interval). Each such row is
#'   first formed again at its stored level, and a row whose stored limits
#'   are not reproduced (to a relative difference of 1e-10) is refused with
#'   the difference. The returned matrix records which limits were
#'   computed.
#' * Every other interval is refused at another level, with its stored
#'   level, its method, and the call that reruns the analysis at the new
#'   level: percentile and BCa bootstrap intervals (whose replications are
#'   not stored), profile likelihood intervals, the order-statistic limits
#'   of norms and of the parallel analysis reference, the exact binomial
#'   limits of content validity, the noncentral chi-square limits of RMSEA
#'   (rerun after `options(sieve.rmsea_conf_level = )`) and of ECVI, and
#'   limits stored without their standard errors (the split reliability of
#'   a cross validation). One-sided bounds, such as HTMT's upper bound, are
#'   not two-sided intervals and are refused at every level, naming the
#'   column that holds them.
#' * A request whose rows are of both kinds is refused as a whole, naming
#'   each row that cannot be formed at that level; `parm` selects the rows
#'   that can. A partly missing matrix is never returned.
#'
#' The interval table of each analysis (the default `table`):
#' * `cfa`, `sem`, `bifactor`, and `longitudinal`: `parameters`, the free
#'   and defined parameters of the stored lavaan fit, with the limits of
#'   lavaan's `parameterEstimates()` at its default level (0.95), named as
#'   lavaan's `coef()` names them (`Visual=~x1`, `x1~~x1`, `x1~1`, with
#'   `.g2` added for the second group) and defined parameters by their
#'   label. Fixed parameters have no interval and are omitted. A model
#'   without a solution is refused with its status.
#' * `reliability`: `reliability`, one row per coefficient with an
#'   interval (omega, model-implied omega, H, alpha), named
#'   "construct: coefficient".
#' * `factor_scores`: `uncertainty`, one row per respondent and factor,
#'   named by the original row and the factor (`row 12: Visual`).
#' * `norms`: `Results: uncertainty`, one row per construct and percentile
#'   (`Visual: p = 0.05`).
#' * `efa`: `Results`, the RMSEA interval of each number of factors.
#' * `retention`: `Results: fit: table` (RMSEA by number of factors), and
#'   the parallel analysis reference percentiles
#'   (`Results: pa: dmar: normal`, `Results: pa: dmar: resample`).
#' * `invariance`: `Results: compare: table`, the RMSEA interval of each
#'   level, and its companions by fit index family.
#' * `cross_validation`: `table` (RMSEA and ECVI of each half) and `rel`
#'   (the split reliability).
#' * `ave` and `content`: `Results`, by factor or by item.
#' * `htmt`, `screening`, and `irt` hold no two-sided limits and are
#'   refused with the reason.
#'
#' A stored level is read from a `conf_level` (or `level`) column, from a
#' DMAR table's metadata through [DMAR::dmar_metadata()] (its
#' `rmsea_conf_level` for RMSEA limits), or from the result element that
#' records it. A table that records no level, or two places that disagree,
#' is refused with the reason. `confint()` reads the limits, levels, and
#' methods from the interval descriptor `x$intervals`, which
#' [sieve_analyze()] fills when it builds the object.
#' @return `print` writes the status first (with the reason when an
#'   analysis failed), the tables, the interval tables (each with its rows
#'   that have limits, their side and level, or why it has none, read from
#'   the interval descriptor described in [sieve_analyze()]), the design,
#'   the versions, and the envelope's schema version, and returns the input
#'   invisibly; `summary` returns `schema_version`, the service, status,
#'   table information, `intervals` (one row per interval table: `table`,
#'   `rows` with limits, `conf_level`, `side`, `method`, and the `reason`
#'   when it has none) and diagnostics; `as.data.frame` returns one stored
#'   table, with the envelope's `schema_version` as an attribute, and
#'   refuses with the reason when the analysis has no estimates;
#'   `confint` returns a numeric matrix with columns `lower` and `upper`,
#'   one row per interval, with the attributes `conf_level`,
#'   `interval_method`, `source` (`"stored"`, or `"computed from the stored
#'   estimate and standard error"`), `recomputed` (`TRUE` for limits formed
#'   at another level), `transformation`, `analysis`, and `table`. An attribute is one value when every row shares it and one
#'   value per row, named by row, otherwise. Limits are never rounded.
#' @author Ken Kelley
#' @examples
#' # The first analysis of a session also records the identity of each
#' # engine, which takes a second or two, once.
#' items <- lavaan::HolzingerSwineford1939[, paste0("x", 1:9)]
#' meta <- data.frame(item = names(items), label = names(items),
#'   construct = rep(c("Visual", "Textual", "Speed"), each = 3L),
#'   facet = NA_character_, type = "continuous", status = "active",
#'   reverse = FALSE, stringsAsFactors = FALSE)
#' result <- sieve_analyze("cfa", items_df = items, meta = meta)
#' print(result)
#' head(as.data.frame(result))
#' head(confint(result))
#' # A Wald interval at another level, computed from the stored estimate
#' # and standard error, says so.
#' attr(confint(result, level = 0.90), "source")
#' @export
print.siever_analysis <- function(x, ...) {
  s <- summary(x)
  cat("SIEVER analysis:", x$analysis, paste0("(", x$service, ")"), "\n")
  st <- x$status
  if (!is.null(st)) {
    cat("Status:", st$code, "\n")
    if (nzchar(st$message %||% "")) cat(strwrap(st$message, indent = 2L, exdent = 2L), sep = "\n")
  }
  cat("Tables:", if (length(s$tables)) {
    paste(sprintf("%s (%d rows)", names(s$tables), s$tables), collapse = ", ")
  } else {
    "none"
  }, "\n")
  # The interval tables, read from the interval descriptor: the rows with
  # limits, their side and level, or why a table (or the analysis) has none.
  iv <- s$intervals
  said <- ifelse(is.na(iv$reason),
    sprintf(
      "%s (%d %s at %s)", iv$table, iv$rows, ifelse(is.na(iv$side), "", iv$side),
      ifelse(is.na(iv$conf_level), "", iv$conf_level)
    ),
    ifelse(is.na(iv$table), sprintf("none (%s)", iv$reason), sprintf("%s (none: %s)", iv$table, iv$reason))
  )
  cat(strwrap(paste("Intervals:", paste(said, collapse = "; ")), exdent = 2L), sep = "\n")
  if (length(x$diagnostics$warnings)) {
    cat("Engine warnings:", length(x$diagnostics$warnings), "(in summary()$diagnostics)\n")
  }
  cat("Design:", x$design$unit %||% "independent", "\n")
  cat("Versions:", paste(vapply(x$versions, function(v) paste(v$package, v$version), character(1)), collapse = ", "), "\n")
  cat("Schema version:", s$schema_version %||% "not recorded", "\n")
  invisible(x)
}

#' @rdname print.siever_analysis
#' @export
summary.siever_analysis <- function(object, ...) {
  tables <- sieve_analysis_tables(object)
  list(
    schema_version = object$schema_version,
    analysis = object$analysis, service = object$service, status = object$status,
    tables = stats::setNames(vapply(tables, nrow, integer(1)), names(tables)),
    intervals = sieve_intervals_overview(sieve_intervals_of(object)),
    diagnostics = object$diagnostics,
    design = object$design, versions = object$versions,
    engine_identity = object$engine_identity
  )
}

#' @rdname print.siever_analysis
#' @export
as.data.frame.siever_analysis <- function(x, row.names = NULL, optional = FALSE,
                                          table = 1L, ...) {
  if (inherits(x$result, "lavaan")) {
    out <- lavaan::parameterEstimates(x$result, ci = TRUE)
    attr(out, "schema_version") <- x$schema_version
    return(out)
  }
  tables <- sieve_analysis_tables(x)
  if (!length(tables)) {
    stop(paste("This analysis has no estimates.", x$status$message %||% ""), call. = FALSE)
  }
  if (length(table) != 1L || !length(tables) ||
    (is.character(table) && !table %in% names(tables)) ||
    (is.numeric(table) && (table < 1 || table > length(tables)))) {
    stop("Choose one table named by summary().", call. = FALSE)
  }
  out <- as.data.frame(tables[[table]], row.names = row.names, optional = optional, ...)
  attr(out, "schema_version") <- x$schema_version
  out
}

#' @rdname print.siever_analysis
#' @export
confint.siever_analysis <- function(object, parm, level = NULL, table = NULL, ...) {
  # One contract by interval type: the stored limits at their stored level
  # by default; another level only for a Wald interval, formed again from
  # the stored estimate and standard error; every other type refused with
  # the reason and the call that reruns it. The registry of interval
  # tables and the rule are in R/fct_confint.R.
  if (!is.null(level)) level <- sieve_check_conf_level(level, "level")
  # The limits are read from the interval descriptor the object carries.
  picked <- sieve_confint_table(object, sieve_intervals_of(object), table)
  rows <- sieve_confint_select(picked$rows, if (missing(parm)) NULL else parm)
  sieve_confint_limits(rows, level, analysis = object$analysis, table = picked$name)
}

#' Tables That Belong to an Analysis Rather Than Its Bookkeeping
#' @param object A public analysis result.
#' @return Named estimate tables.
#' @author Ken Kelley
#' @noRd
sieve_analysis_tables <- function(object) {
  value <- object$result
  # A service that returned nothing has no estimates to tabulate; its status
  # and diagnostics say why.
  if (is.null(value)) {
    return(list())
  }
  if (inherits(value, "lavaan")) {
    return(list(parameters = lavaan::parameterEstimates(value, ci = TRUE)))
  }
  if (identical(object$analysis, "factor_scores")) {
    out <- list(uncertainty = value$uncertainty, scores = value$scores)
    if (!is.null(value$determinacy)) {
      out$determinacy <- sieve_readable_result_tables(value$determinacy)[[1L]]
    }
    return(Filter(Negate(is.null), out))
  }
  if (identical(object$analysis, "reliability")) {
    return(list(reliability = sieve_reliability_table(list(result = value))))
  }
  if (identical(object$analysis, "cross_validation")) {
    return(Filter(Negate(is.null), value[c("table", "xval", "rel")]))
  }
  sieve_readable_result_tables(value)
}

#' Inspect a Saved Study Without Refitting It
#'
#' Reads decisions and analyses from a checked project snapshot. Reports use
#' stored results and disclosures; preparing one does not refit a model.
#' @param project A snapshot returned by [sieve_project_read()].
#' @param ids Analysis identifiers; `NULL` selects all stored analyses.
#' @return `sieve_project_analyses` and `sieve_project_decisions` return the
#'   respective named records; `sieve_project_report` returns report text.
#' @author Ken Kelley
#' @examples
#' d <- DMAR::holzinger_swineford[, c(
#'   "id", "school", "t1_visual_perception", "t2_cubes", "t4_lozenges"
#' )]
#' spec <- sieve_specification_template(d)
#' spec$construct[spec$role == "item"] <- "Visual"
#' path <- tempfile("holzinger_", fileext = ".siever.rds")
#' sieve_prepare_study(d, spec, file = path, name = "Holzinger and Swineford")
#' project <- sieve_project_read(path)
#' # A newly prepared study has no saved analyses or decisions yet.
#' length(sieve_project_analyses(project))
#' length(sieve_project_decisions(project))
#' cat(sieve_project_report(project))
#' @export
sieve_project_analyses <- function(project) {
  problems <- sieve_project_check(project)
  if (length(problems)) stop(paste(problems, collapse = "; "), call. = FALSE)
  project$analyses %||% list()
}

#' @rdname sieve_project_analyses
#' @export
sieve_project_decisions <- function(project) {
  sieve_project_analyses(project)
  project$other$decisions %||% project$decisions %||% list()
}

#' @rdname sieve_project_analyses
#' @export
sieve_project_report <- function(project, ids = NULL) {
  records <- sieve_project_analyses(project)
  state <- sieve_new_state()
  sieve_project_restore(state, project)
  ids <- ids %||% names(records)
  paste(sieve_saved_analyses_report(state, ids),
    sieve_assistant_disclosure_report(state),
    sep = "\n\n"
  )
}

#' Replay a Checked Project in an Explicit R Environment
#'
#' Writes the hash-verified session script and input sidecar, then sources the
#' script in the supplied environment. This is an explicit execution request;
#' reading a project or preparing a report never executes its recorded code.
#' Use only projects whose recorded expressions you have reviewed and trust.
#' A recorded step whose inputs were not saved is kept in the script as a
#' labeled record and is not claimed as replayed computation.
#' @param project A checked snapshot from [sieve_project_read()].
#' @param directory Destination for the reviewable script and input sidecar.
#' @param envir Environment in which the recorded expressions run.
#' @return Invisibly, the execution environment. The files remain available
#'   for inspection in `directory`.
#' @author Ken Kelley
#' @examples
#' d <- DMAR::holzinger_swineford[, c(
#'   "id", "school", "t1_visual_perception", "t2_cubes", "t4_lozenges"
#' )]
#' spec <- sieve_specification_template(d)
#' spec$construct[spec$role == "item"] <- "Visual"
#' path <- tempfile("holzinger_", fileext = ".siever.rds")
#' sieve_prepare_study(d, spec, file = path, name = "Holzinger and Swineford")
#' project <- sieve_project_read(path)
#' replayed <- sieve_replay(project, tempfile("siever_replay_"))
#' names(replayed$items)
#' replayed$meta[, c("item", "construct", "status")]
#' @export
sieve_replay <- function(project, directory, envir = new.env(parent = globalenv())) {
  paths <- sieve_session_write(project, directory)
  source(paths[["script"]], local = envir, chdir = TRUE)
  invisible(envir)
}
